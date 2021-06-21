unit main;

(*

  Uses mosquitto-p a Free Pascal conversions of the libmosquitto header file
  mosquitto.h and mqttclass.pas by Károly Balogh (chainq)
  @ https://github.com/chainq/mosquitto-p

  Requirements: The Eclipse mosquitto libraries must be installed on the
  system. See the README file.

  There is no need to install the mosquitto broker assuming access to
  an MQTT broker is available on the network.

  See project source because the ctypes unit must be loaded at the start of
  the program. Furthermore, the cthreads unit must be loaded first in
  Linux systems. See the uses clause in the project source:

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
  Grids, Buttons, ComCtrls, DefaultTranslator, DividerBevel,  brokerunit, mqttclass,
  topicgrids, fileinfo;

const
  SubscribedMemoSize = 2500; // maximum number of lines in subscribed memo

type

  { TMainForm }

  TMainForm = class(TForm)
    BottomPanel: TPanel;
    CheckBox1: TCheckBox;
    autoClearCheckBox: TCheckBox;
    DividerBevel1: TDividerBevel;
    VersionLabel: TLabel;
    QuitButton: TButton;
    SourceLabel: TLabel;
    Splitter2: TSplitter;
    SubscribedMemo: TMemo;
    SubTopicsPanel: TPanel;
    RetainCheckBox: TCheckBox;
    QoSComboBox: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    EditBrokerButton: TButton;
    ConnectButton: TButton;
    PublishButton: TButton;
    HostLabel: TLabel;
    statusLabel: TLabel;
    PublishTopicEdit: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PayloadMemo: TMemo;
    MiddlePanel: TPanel;
    Splitter1: TSplitter;
    TopPanel: TPanel;
    procedure CheckBox1Change(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure EditBrokerButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SourceLabelClick(Sender: TObject);
    procedure PayloadMemoEditingDone(Sender: TObject);
    procedure PublishButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PublishTopicEditEditingDone(Sender: TObject);
    procedure QoSComboBoxChange(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure RetainCheckBoxChange(Sender: TObject);
    procedure SubTopicsPanelResize(Sender: TObject);
  private
    FDisplayedState: TMQTTConnectionState;
    procedure i18nFixup;
    function ClientState: TMQTTConnectionState;
    procedure RefreshGui;
    function ConnectBroker(aBroker: TBroker): boolean;
    procedure UpdateConnectionState;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
  public
    TopicsGrid: TSubTopicsGrid;
    procedure TopicsGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure TopicsGridSetUse(Sender: TObject; aCol, aRow: Integer; const aState: TCheckboxState);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLIntf, stringres, brokeredit, mosquitto, pwd, report;


// mosquitto library log level
const
  {$IFDEF MSWINDOWS}  // no log in Windows
  MOSQ_LOG = MOSQ_LOG_NONE;
  {$ELSE} // chose one from any log level defined in mosquitto.pas,  for all debug levels
  //MOSQ_LOG = MOSQ_LOG_ALL;
  MOSQ_LOG = MOSQ_LOG_NODEBUG;
  //MOSQ_LOG = MOSQ_LOG_NONE;
  {$ENDIF}

var
  ShowTopics: boolean = true;
  FClearMessageMemo: boolean = false;

type
  TThisMQTTConnection = class(TMQTTConnection)
  private
    FThisMessage: string;
    procedure UpdateGUI;
    procedure MessageHandler(const payload: Pmosquitto_message);
  end;


{ TThisMQTTConnection }


var
  MqttClient: TThisMQTTConnection = nil;
  MqttConfig: TMQTTConfig;


procedure TThisMQTTConnection.UpdateGUI;
begin
   with MainForm.SubscribedMemo do begin
     if FClearMessageMemo then begin
       Clear;
       FClearMessageMemo := false;
     end
     else begin
       Lines.BeginUpdate;
       try
         while Lines.Count > SubscribedMemoSize do
            Lines.Delete(0);
       finally
         Lines.EndUpdate;
       end;
     end;
     Lines.Add(FThisMessage);
     SelStart := Lines.Text.Length-1;
     SelLength := 1;
  end;
end;

procedure TThisMQTTConnection.MessageHandler(const payload: Pmosquitto_message);
var
  msg: ansistring;
begin
   msg := '';
   with payload^ do begin
      { Note that MQTT messages can be binary, but for this test case we just
        assume they're printable text, as a test
        Károly Balogh (chainq) }
      if (payloadlen > 0) then begin
        SetLength(msg,payloadlen);
        Move(payload^,msg[1],payloadlen);
      end;
      if ShowTopics then
        FThisMessage := Format(srxMsgFormat, [topic, msg])
      else
        FThisMessage := msg;
      Synchronize(@UpdateGui);
   end;
end;

{ TMainForm }

procedure TMainForm.AppIdle(Sender: TObject; var Done: Boolean);
begin
  UpdateConnectionState;
  Done := true;
end;

function TMainForm.ClientState: TMQTTConnectionState;
begin
  if assigned(MqttClient) then
    result := MqttClient.State
  else
    result := mqttNone;
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
  MqttClient := TThisMQTTConnection.Create('mqttClient', MqttConfig, MOSQ_LOG);
  try
    MqttClient.AutoReconnect := aBroker.AutoReconnect;
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
end;

procedure TMainForm.CheckBox1Change(Sender: TObject);
begin
  ShowTopics := CheckBox1.Checked;
end;

procedure TMainForm.EditBrokerButtonClick(Sender: TObject);
begin
  if TBrokerEditForm.EditBroker(Broker) then begin
    freeandnil(MqttClient);
    RefreshGUI;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Version : TProgramVersion;
begin
  i18nFixup;
  if GetProgramVersion(Version) then with Version do
    VersionLabel.caption := Format(sVersionFormat, [Major,Minor,Revision])
  else
    VersionLabel.caption := '';

  statusLabel.caption := statusStr[mqttNone];
  TopicsGrid := TSubTopicsGrid.Create(self);
  with TopicsGrid do begin
    parent := SubTopicsPanel;
    Top := 8;
    Align := alClient;
    Broker := brokerunit.Broker;
    Options := Options + [goCellHints];
    ShowHint := true;
    BorderSpacing.Around := 6;
    Columns[1].Title.Caption := sSubscribeColumnTitle;
    OnSelectCell := @TopicsGridSelectCell;
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

procedure TMainForm.FormShow(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  if DefaultKey = '' then begin
    ShowPublishResult(sNoEncryptionKey, false);
    halt;
  end;
end;

procedure TMainForm.i18nFixup;
begin
  QosComboBox.Items.Text := sQosHint;
  QosComboBox.ItemIndex := 0;
end;

procedure TMainForm.PublishTopicEditEditingDone(Sender: TObject);
begin
  if trim(PublishTopicEdit.Text) <> '' then
    Broker.PubTopic := PublishTopicEdit.Text;
end;

procedure TMainForm.PayloadMemoEditingDone(Sender: TObject);
begin
  Broker.PubPayload := PayloadMemo.Text;
end;

procedure TMainForm.PublishButtonClick(Sender: TObject);
var
  res: integer;
  topic, msg: string;
begin
  topic := trim(PublishTopicEdit.Text);
  if not assigned(MqttClient) then
    res := -1000
  else if MQttClient.State <> mqttConnected then
    res := -1001
  else if topic = '' then
    res := -1002
  else begin
    FClearMessageMemo := autoClearCheckBox.checked;
    res := MqttClient.Publish(topic, PayloadMemo.Text,
      QoSComboBox.ItemIndex, RetainCheckBox.Checked);
    if (res = 0) and (PayloadMemo.Text = '') then begin
      res := -1003;
      if RetainCheckBox.Checked then dec(res); // -1004
    end;
  end;

  if res = -1000 then
    msg := sNoClientError
  else if res = -1001 then
    msg := sNotConnectedError
  else if res = -1002 then
    msg := sNoTopicError
  else if res = -1003 then begin
    msg := sNoPayloadWarning;
    res := 0;
  end
  else if res = -1004 then begin
    msg := Format(SRetainRemovedFormat, [topic]);
    res := 0;
  end
  else if res > 0 then
    msg := Format(sMQTTErrorFormat, [res])
  else
    msg := sOk;
  if res <> 0 then
    FClearMessageMemo := false;
  ShowPublishResult(msg, res = 0);
  RetainCheckBox.Checked := false;
end;

procedure TMainForm.QoSComboBoxChange(Sender: TObject);
begin
  Broker.PubQoS := QoSComboBox.ItemIndex;
end;

procedure TMainForm.QuitButtonClick(Sender: TObject);
begin
  freeandnil(MqttClient);
  // save broker definition ?
  close;
end;

procedure TMainForm.RefreshGui;
begin
  with Broker do begin
    hostLabel.caption := Format('%s:%d', [Host, Port]);
    PublishTopicEdit.Text := PubTopic;
    PayloadMemo.Text := PubPayload;
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

procedure TMainForm.RetainCheckBoxChange(Sender: TObject);
begin
  RetainCheckBox.Caption := Format('(%s)', [falsetruestr[RetainCheckBox.Checked]]);
  Broker.PubRetain := RetainCheckBox.Checked;;
end;

procedure TMainForm.SourceLabelClick(Sender: TObject);
begin
  OpenUrl('https://github.com/sigmdel/lazmqttc');  // report error ?
end;

procedure TMainForm.SubTopicsPanelResize(Sender: TObject);
begin
  TopicsGrid.Invalidate; // needed in Mint 20 Mate at least
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
      if ClientState = mqttConnected then begin
        if doUse then
          MqttClient.Subscribe(topic, qos)
        else
          MqttClient.Unsubscribe(topic);
      end;
    end;
  end;
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
      ConnectButton.Caption := sDisconnect;
      ConnectButton.tag := 1;
    end
    else begin
      ConnectButton.Caption := sConnect;
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

end.



