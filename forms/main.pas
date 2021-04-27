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
  ComboEx, CheckLst, PairSplitter, brokerunit, mqttclass;

const
  SubscribedMemoSize = 2500; // maximum number of lines in subscribed memo

type

  { TMainForm }

  TMainForm = class(TForm)
    SubListBox: TCheckListBox;
    EditBrokerButton: TButton;
    ConnectButton: TButton;
    Label2: TLabel;
    PublishButton: TButton;
    QuitButton: TButton;
    HostLabel: TLabel;
    statusLabel: TLabel;
    Panel1: TPanel;
    PublishTopicEdit: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SubscribedMemo: TMemo;
    MessageMemo: TMemo;
    MiddlePanel: TPanel;
    BottomPanel: TPanel;
    Splitter1: TSplitter;
    TopPanel: TPanel;
    procedure ConnectButtonClick(Sender: TObject);
    procedure EditBrokerButtonClick(Sender: TObject);
    procedure PublishButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure SubListBoxClickCheck(Sender: TObject);
  private
    FDisplayedState: TMQTTConnectionState;
    procedure RefreshGui;
    function ConnectBroker(aBroker: TBroker): boolean;
    procedure UpdateConnectionState;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  brokeredit, mosquitto;

resourcestring
  mssNoClient = 'No Mqtt Client';
  mssConnecting = 'Attempting to connect';
  mssConnected = 'Connected';
  mssReconnecting = 'Attempting to reconnect';
  mssDisconnected = 'Disconnected';

var
  statusStr: array[TMQTTConnectionState] of string;

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
      FThisMessage := Format('Topic: [%s] - %s', [topic, msg]);
      Synchronize(@UpdateGui);
   end;
end;

var
  MqttClient: TThisMQTTConnection = nil;
  MqttConfig: TMQTTConfig;

procedure TMainForm.UpdateConnectionState;
var
  newState: TMQTTConnectionState;
begin
  if assigned(MqttClient) then
    newState := MqttClient.State
  else
    newState := mqttNone;
  if FDisplayedState <> newState then begin
    statusLabel.Caption := statusStr[newState];
    FDisplayedState := newState;
  end;
end;

{ TMainForm }

procedure TMainForm.AppIdle(Sender: TObject; var Done: Boolean);
begin
  UpdateConnectionState;
  Done := true;
end;

function TMainForm.ConnectBroker(aBroker: TBroker): boolean;
var
  i: integer;
  cnt: integer;
begin
  FillChar(MqttConfig, sizeof(MqttConfig), 0);
  with MqttConfig do begin
      //ssl: boolean;
      //ssl_cacertfile: ansistring;      not implemented
      hostname := aBroker.host;
      port := aBroker.port;
      username := aBroker.user;
      password := aBroker.password;
      keepalives := 60;               // not implemented
      // reconnect_delay: longint;
      // reconnect_backoff: boolean;     not implemented
  end;
  result := false;
  freeandnil(MqttClient);
  // with debug message use MOSQ_LOG_ALL);
  MqttClient := TThisMQTTConnection.Create('mqttClient', MqttConfig, MOSQ_LOG_NODEBUG);
  try
    MqttClient.OnMessage := @MqttClient.MessageHandler;
    MqttClient.Connect;
    // update topics on form
    // PublishTopicEdit.Text := '';
    cnt := 0;
    for i := 0 to aBroker.Count-1 do begin
      if aBroker.Subscribed[i] then begin
        MqttClient.Subscribe(aBroker.subTopics[i], 0); // and subsbcribe to wanted topics
        inc(cnt);
      end;
    end;
    // report number of subscribed messages ?
    Broker.AssignTo(SubListBox);
    PublishTopicEdit.Text := aBroker.PubTopic;
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
  if assigned(MqttClient) and (MqttClient.State in [mqttConnecting, mqttConnected, mqttReconnecting]) then begin
    ConnectButton.Caption := 'Disconnect';
    ConnectButton.tag := 1;
  end
  else begin
    ConnectButton.Caption := 'Connect';
    ConnectButton.tag := 0;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  statusLabel.caption := statusStr[mqttNone];
  RefreshGUI;
  Application.OnIdle := @AppIdle;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  freeandnil(MqttClient);
end;

procedure TMainForm.EditBrokerButtonClick(Sender: TObject);
begin
  if TBrokerEditForm.EditBroker(Broker) then begin
    freeandnil(MqttClient);
    RefreshGUI;
  end;
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
  else if MessageMemo.Text = '' then
    res := -1002
  else
    res := MqttClient.Publish(PublishTopicEdit.Text, MessageMemo.Text, 0, false);

  MessageMemo.Lines.Add('');
  waitfor := 3000;

  if res = -1000 then
    msg := 'The Mqtt Client has not been created'
  else if res = -1001 then
    msg := 'The Mqtt Client has not been created'
  else if res = -1002 then
    msg := 'Error: No payload'
  else if res > 0 then
    msg := Format('MQTT Error: %d', [res])
  else begin
    msg := 'OK';
    waitfor := 1000;
  end;
  MessageMemo.Lines.Add(msg);
  Delay(waitfor);
  MessageMemo.Lines.Delete(MessageMemo.Lines.Count-1);
  MessageMemo.Lines.Delete(MessageMemo.Lines.Count-1);
end;

procedure TMainForm.QuitButtonClick(Sender: TObject);
begin
  freeandnil(MqttClient);
  // save broker definition ?
  close;
end;

procedure TMainForm.SubListBoxClickCheck(Sender: TObject);
var
  itemIndex: integer;
begin
  itemIndex := SubListBox.ItemIndex;
  if (itemIndex < 0) or (itemIndex >= SubListBox.Items.Count) then exit;
  if Broker.Subscribed[itemIndex] <> SubListBox.Checked[itemIndex] then begin
    Broker.Subscribed[itemIndex] := SubListBox.Checked[itemIndex];
    if Broker.Subscribed[itemIndex] then
      MqttClient.Subscribe(Broker[itemIndex], 0)
    else
      MqttClient.Unsubscribe(Broker[itemIndex]);
  end;
end;

procedure TMainForm.RefreshGUI;
begin
  hostLabel.caption := Format('%s:%d', [Broker.Host, Broker.Port]);
  if assigned(MqttClient) and (MqttClient.State in [mqttConnecting, mqttConnected, mqttReconnecting]) then begin
    ConnectButton.Caption := 'Disconnect';
    ConnectButton.tag := 1;
  end
  else begin
    ConnectButton.Caption := 'Connect';
    ConnectButton.tag := 0;
    SubListBox.Items.Clear;
  end;
end;


initialization
  statusStr[mqttNone] := mssNoClient;
  statusStr[mqttConnecting] := mssConnecting;
  statusStr[mqttConnected] := mssConnected;
  statusStr[mqttReconnecting] := mssReconnecting;
  statusStr[mqttDisconnected] := mssDisconnected;
end.



