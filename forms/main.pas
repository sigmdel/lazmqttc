unit main;

(*

  Uses mosquitto-p a Free Pascal conversions of the libmosquitto header file
  mosquitto.h and mqttclass.pas by Károly Balogh (chainq)
  @ https://github.com/chainq/mosquitto-p

  Two libraries need to be installed in Debian systems:
    libmosquitto1    (tested with version 1.6.9-1 in Mint 20.1)
    libmosquitto-dev (tested with version 1.6.9-1 in Mint 20.1)
  The first, libmosquitto1 will probably already be installed if
  mosquitto-clients has been installed.

  There is no need to install the mosquitto broker assuming access to
  an MQTT broker is available on the network.


  See project source because
  Two units, cthreads and ctypes must be loaded at the start of the
  program. See the uses clause in the project source:

    uses
      {$IFDEF UNIX}{$IFDEF UseCThreads}
      cthreads,
      {$ENDIF}{$ENDIF}
      ctypes, // needed by mosquitto
      ...

  The  UseCThreads was defined by adding -dUseCThreads
  Personalised Options of Compiler Options in Project Options

*)


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  PairSplitter, Grids, Buttons, ComCtrls, DividerBevel, brokerunit, mqttclass,
  topicgrids, fileinfo;

const
  SubscribedMemoSize = 2500; // maximum number of lines in subscribed memo

type

  { TMainForm }

  TMainForm = class(TForm)
    CheckBox1: TCheckBox;
    DividerBevel1: TDividerBevel;
    Label2: TLabel;
    SourceLabel: TLabel;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    RetainCheckBox: TCheckBox;
    QoSComboBox: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    EditBrokerButton: TButton;
    ConnectButton: TButton;
    PublishButton: TButton;
    QuitButton: TButton;
    HostLabel: TLabel;
    statusLabel: TLabel;
    Panel1: TPanel;
    PublishTopicEdit: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PayloadMemo: TMemo;
    MiddlePanel: TPanel;
    Splitter1: TSplitter;
    SubscribedMemo: TMemo;
    TopPanel: TPanel;
    procedure CheckBox1Change(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure EditBrokerButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SourceLabelClick(Sender: TObject);
    procedure PairSplitter1Resize(Sender: TObject);
    procedure PayloadMemoEditingDone(Sender: TObject);
    procedure PublishButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PublishTopicEditEditingDone(Sender: TObject);
    procedure QoSComboBoxChange(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure RetainCheckBoxChange(Sender: TObject);
  private
    FDisplayedState: TMQTTConnectionState;
    function ClientState: TMQTTConnectionState;
    procedure RefreshGui;
    function ConnectBroker(aBroker: TBroker): boolean;
    procedure UpdateConnectionState;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
  public
    TopicsGrid: TSubTopicsGrid;
    procedure TopicsGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure TopicsGridGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
    procedure TopicsGridSetUse(Sender: TObject; aCol, aRow: Integer; const aState: TCheckboxState);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLIntf, brokeredit, mosquitto;

resourcestring
  mssNoClient = 'No Mqtt Client';
  mssConnecting = 'Attempting to connect';
  mssConnected = 'Connected';
  mssReconnecting = 'Attempting to reconnect';
  mssDisconnected = 'Disconnected';

var
  statusStr: array[TMQTTConnectionState] of string;
  ShowTopics: boolean = true;

const
  falsetruestr: array[boolean] of string = ('false', 'true');

 procedure Delay(ms: DWORD);
(*
Source: GetMem on  Lazarus forum
https://forum.lazarus.freepascal.org/index.php/topic,31526.msg202088.html#msg202088
*)
var
  tc : DWORD;
begin
  tc := GetTickCount64;
  while (GetTickCount64 < tc + ms) and (not Application.Terminated) do
    Application.ProcessMessages;
end;

type
  TThisMQTTConnection = class(TMQTTConnection)
  private
    FThisMessage: string;
    procedure UpdateGUI;
    procedure MessageHandler(const payload: Pmosquitto_message);
  end;

{ TThisMQTTConnection }

procedure TThisMQTTConnection.UpdateGUI;
begin
   with MainForm.SubscribedMemo do begin
     Lines.BeginUpdate;
     while Lines.Count > SubscribedMemoSize do
        Lines.Delete(0);
     Lines.Add(FThisMessage);
     SelStart := Lines.Text.Length-1;
     SelLength := 1;
     Lines.EndUpdate;
  end;
end;

procedure TThisMQTTConnection.MessageHandler(const payload: Pmosquitto_message);
var
  msg: ansistring;
begin
   msg := '';
   with payload^ do begin
      { Note that MQTT messages can be binary, but for this test case we just
        assume they're printable text, as a test }
      SetLength(msg,payloadlen);
      Move(payload^,msg[1],payloadlen);
      if ShowTopics then
        FThisMessage := Format('[%s] - %s', [topic, msg])
      else
        FThisMessage := msg;
      Synchronize(@UpdateGui);
   end;
end;

var
  MqttClient: TThisMQTTConnection = nil;
  MqttConfig: TMQTTConfig;

function TMainForm.ClientState: TMQTTConnectionState;
begin
  if assigned(MqttClient) then
    result := MqttClient.State
  else
    result := mqttNone;
end;

procedure TMainForm.UpdateConnectionState;
var
  newState: TMQTTConnectionState;
  i: integer;
begin
  newState := ClientState;
  if FDisplayedState <> newState then begin
    statusLabel.Caption := statusStr[newState];
    FDisplayedState := newState;
    if (newState in [mqttConnecting, mqttConnected, mqttReconnecting]) then begin
      ConnectButton.Caption := 'Disconnect';
      ConnectButton.tag := 1;
    end
    else begin
      ConnectButton.Caption := 'Connect';
      ConnectButton.tag := 0;
    end;
    if newState = mqttConnected then begin
      // subscribe to all used topics
      for i := 0 to Broker.SubTopicsCount-1 do begin
        if Broker.SubTopics[i].Use then begin
          MqttClient.Subscribe(Broker.subTopics[i].Topic, Broker.subTopics[i].QoS); // and subsbcribe to wanted topics
        end;
      end;
    end;
  end;
end;

{ TMainForm }

procedure TMainForm.AppIdle(Sender: TObject; var Done: Boolean);
begin
  UpdateConnectionState;
  Done := true;
end;

function TMainForm.ConnectBroker(aBroker: TBroker): boolean;
begin
  FillChar(MqttConfig, sizeof(MqttConfig), 0);
  with MqttConfig do begin
      ssl := aBroker.SSL;
      ssl_cacertfile := aBroker.SSLCert;
      hostname := aBroker.host;
      port := aBroker.port;
      username := aBroker.user;
      password := aBroker.password;
      keepalives := 60;
      reconnect_delay := aBroker.ReconnectDelay;
      reconnect_backoff := aBroker.ReconnectBackoff;
  end;
  result := false;
  freeandnil(MqttClient);
  FDisplayedState := mqttnone;
  // with debug message use MOSQ_LOG_ALL);
  MqttClient := TThisMQTTConnection.Create('mqttClient', MqttConfig, MOSQ_LOG_NODEBUG);
  try
    MqttClient.OnMessage := @MqttClient.MessageHandler;
    MqttClient.Connect;
    PublishTopicEdit.Text := aBroker.PubTopic;
    TopicsGrid.Broker := aBroker;
    TopicsGrid.UpdateGridSize;
    TopicsGrid.Invalidate;
    result := true;
  except
    result := false;
  end;
end;

procedure TMainForm.ConnectButtonClick(Sender: TObject);
begin
  if ConnectButton.tag = 0 then
    ConnectBroker(Broker)
  else
    freeandnil(MqttClient);
(*
  if assigned(MqttClient) and (MqttClient.State in [mqttConnecting, mqttConnected, mqttReconnecting]) then begin
    ConnectButton.Caption := 'Disconnect';
    ConnectButton.tag := 1;
  end
  else begin
    ConnectButton.Caption := 'Connect';
    ConnectButton.tag := 0;
  end; *)
end;

procedure TMainForm.CheckBox1Change(Sender: TObject);
begin
  ShowTopics := CheckBox1.Checked;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Version : TProgramVersion;
begin
  if GetProgramVersion(Version) then with Version do
    label2.caption := Format('(version %d.%d.%d)', [Major,Minor,Revision])
  else
    label2.caption := '';

  statusLabel.caption := statusStr[mqttNone];
  TopicsGrid := TSubTopicsGrid.Create(self);
  with TopicsGrid do begin
    parent := PairSplitterSide1;
    Top := 34;
    Align := alClient;
    Broker := brokerunit.Broker;
    Options := Options + [goCellHints];
    ShowHint := true;
    BorderSpacing.Around := 6;
    Columns[1].Title.Caption := 'Subscribe Topics';
    OnSelectCell := @TopicsGridSelectCell;
    OnGetCellHint := @TopicsGridGetCellHint;
    OnSetCheckboxState := @TopicsGridSetUse;
    //TabOrder = 1
  end;
  RefreshGUI;
  FDisplayedState := mqttReconnecting; // force update
  Application.OnIdle := @AppIdle;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  freeandnil(MqttClient);
end;

procedure TMainForm.PublishTopicEditEditingDone(Sender: TObject);
begin
  if trim(PublishTopicEdit.Text) <> '' then
    Broker.PubTopic := PublishTopicEdit.Text;
end;

procedure TMainForm.QoSComboBoxChange(Sender: TObject);
begin
  Broker.PubQoS := QoSComboBox.ItemIndex;
end;

procedure TMainForm.EditBrokerButtonClick(Sender: TObject);
begin
  if TBrokerEditForm.EditBroker(Broker) then begin
    freeandnil(MqttClient);
    RefreshGUI;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

procedure TMainForm.PairSplitter1Resize(Sender: TObject);
begin
  TopicsGrid.Refresh;
end;

procedure TMainForm.PayloadMemoEditingDone(Sender: TObject);
begin
  Broker.PubPayload := PayloadMemo.Text;
end;

procedure TMainForm.PublishButtonClick(Sender: TObject);
var
  res: integer;
  waitfor: dword;
  msg: string;
begin
  if not assigned(MqttClient) then
    res := -1000
  else if MQttClient.State <> mqttConnected then
    res := -1001
  else begin
    res := MqttClient.Publish(PublishTopicEdit.Text, PayloadMemo.Text,
      QoSComboBox.ItemIndex, RetainCheckBox.Checked);
    if (res = 0) and (PayloadMemo.Text = '') then begin
      res := -1002;
      if RetainCheckBox.Checked then dec(res);
    end;
  end;

  PayloadMemo.Lines.Add('');
  waitfor := 3000;

  if res = -1000 then
    msg := 'The Mqtt Client has not been created'
  else if res = -1001 then
    msg := 'The Mqtt Client has not been created'
  else if res = -1002 then
    msg := 'Warning: Message set with no payload'
  else if res = -1003 then
    msg := 'OK - removed retain flag'
  else if res > 0 then
    msg := Format('MQTT Error: %d', [res])
  else begin
    msg := 'OK';
    waitfor := 1000;
  end;
  PayloadMemo.Lines.Add(msg);
  Delay(waitfor);
  PayloadMemo.Lines.Delete(PayloadMemo.Lines.Count-1);
  PayloadMemo.Lines.Delete(PayloadMemo.Lines.Count-1);
  RetainCheckBox.Checked := false;
end;

procedure TMainForm.QuitButtonClick(Sender: TObject);
begin
  freeandnil(MqttClient);
  // save broker definition ?
  close;
end;

procedure TMainForm.RetainCheckBoxChange(Sender: TObject);
begin
  RetainCheckBox.Caption := Format('(%s)', [falsetruestr[RetainCheckBox.Checked]]);
  Broker.PubRetain := RetainCheckBox.Checked;;
end;

procedure TMainForm.RefreshGui;
begin
  with Broker do begin
    hostLabel.caption := Format('%s:%d', [Host, Port]);
    PublishTopicEdit.Text := PubTopic;
    QoSComboBox.ItemIndex := PubQoS;
    RetainCheckBox.Checked := PubRetain;
    TopicsGrid.Broker := Broker;
    TopicsGrid.UpdateGridSize;
    TopicsGrid.Invalidate;
    if TopicsGrid.RowCount > 1 then
      TopicsGrid.Row := 1;
    SubscribedMemo.Clear;
  end;
end;

procedure TMainForm.SourceLabelClick(Sender: TObject);
begin
  OpenUrl('https://github.com/sigmdel/lazmqttc');  // report error ?
end;

procedure TMainForm.TopicsGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
begin
  if aCol = 2 then
    HintText := '0 - At most once'#10'1 - At least once'#10'2 - Exactly once'
end;

procedure TMainForm.TopicsGridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect := (aRow > 0) and
               (aRow <= Broker.SubTopicsCount)
               and (aCol = 0)
end;

procedure TMainForm.TopicsGridSetUse(Sender: TObject; aCol, aRow: Integer;
  const aState: TCheckboxState);
var
  doUse: boolean;
begin
  if (aCol=0) and (aRow > 0) and (aRow <= Broker.SubTopicsCount) then begin
    doUse := aState = cbChecked;
    with Broker.SubTopics[aRow-1] do begin
      Use := doUse;
      if doUse and (ClientState = mqttConnected) then
        MqttClient.Subscribe(topic, qos)
      else
        MqttClient.Unsubscribe(topic);
    end;
  end;
end;


initialization
  statusStr[mqttNone] := mssNoClient;
  statusStr[mqttConnecting] := mssConnecting;
  statusStr[mqttConnected] := mssConnected;
  statusStr[mqttReconnecting] := mssReconnecting;
  statusStr[mqttDisconnected] := mssDisconnected;
end.



