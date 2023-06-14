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
  Grids, Buttons, ComCtrls, DefaultTranslator, Menus, brokerunit,
  optionsunit, mqttclass, topicgrids, fileinfo;

type

  { TMainForm }

  TMainForm = class(TForm)
    BottomPanel: TPanel;
    OptionsButton: TButton;
    LogMemo: TMemo;
    LogMenuClearMenuItem: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MessagesMenuCopyMenuItem: TMenuItem;
    LogMenuCopySelectionMenuItem: TMenuItem;
    Separator3: TMenuItem;
    Separator2: TMenuItem;
    MessagesMenuCopySelectionMenuItem: TMenuItem;
    MessageMenuWordWrapMenuItem: TMenuItem;
    Separator1: TMenuItem;
    LogMenuCopyMenuItem: TMenuItem;
    MenuItem3: TMenuItem;
    MessagesMenuClearMenuItem: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    N1: TMenuItem;
    MessagesMemo: TMemo;
    PageControl1: TPageControl;
    LogMemoMenu: TPopupMenu;
    MessagesMemoMenu: TPopupMenu;
    ShowTopicsCheckBox: TCheckBox;
    autoClearCheckBox: TCheckBox;
    CopyPubCheckbox: TCheckBox;
    QuitButton: TButton;
    SourceLabel: TLabel;
    Splitter2: TSplitter;
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
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TopPanel: TPanel;
    procedure OptionsButtonClick(Sender: TObject);
    procedure LogMenuCopySelectionMenuItemClick(Sender: TObject);
    procedure LogMenuLevelClick(Sender: TObject);
    procedure MessagesMenuCopyMenuItemClick(Sender: TObject);
    procedure MessagesMenuCopySelectionMenuItemClick(Sender: TObject);
    procedure MessageMenuWordWrapMenuItemClick(Sender: TObject);
    procedure LogMenuClearMenuItemClick(Sender: TObject);
    procedure LogMenuCopyMenuItemClick(Sender: TObject);
    procedure MessagesMenuClearMenuItemClick(Sender: TObject);
    procedure LogMemoMenuPopup(Sender: TObject);
    procedure MessagesMemoMenuPopup(Sender: TObject);
    procedure ShowTopicsCheckBoxChange(Sender: TObject);
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
    Fpt: TPoint; // position of Log memo popup menu
    FLogLevel: longint; // log levels of MqttClient and the underlying mosquitto lib
    procedure i18nFixup;
    function ClientState: TMQTTConnectionState;
    procedure RefreshGui;
    function ConnectBroker(aBroker: TBroker): boolean;
    procedure UpdateConnectionState;
    procedure UpdateFormOptions;
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
  LCLIntf, stringres, startup, brokeredit, optionsedit, mosquitto, pwd, report, clipbrd;

CONST
  DEFAULT_LOG_LEVEL = MOSQ_LOG_NODEBUG; // defined in mqttclass

  { Global variables for MQTTClient that does not know about MainForm components }
var
  gvShowTopics: boolean = true;        // reflects ShowTopicsCheckbox.checked
  gvClearMessageMemo: boolean = false; // set to autoClearCheckBox.checked on publishing a message
  gvPubMessageTopic: string = '';      // if non empty, then published message to add to Messages
  gvPubMessagePayload: string = '';    // payload of shown published message

type
  TThisMQTTConnection = class(TMQTTConnection)
  private
    FThisMessage: string;
    procedure UpdateGUI;
    procedure MessageHandler(const payload: Pmosquitto_message);
    procedure ConnectionHandler(const rc: longint);
  end;


{ TThisMQTTConnection }


var
  MqttClient: TThisMQTTConnection = nil;
  MqttConfig: TMQTTConfig;

procedure TThisMQTTConnection.ConnectionHandler(const rc: longint);
begin
  Synchronize(@MainForm.UpdateConnectionState);
end;

procedure TThisMQTTConnection.UpdateGUI;
var
  outs: string;
begin
   with MainForm.MessagesMemo do begin
     if gvClearMessageMemo then begin
       Clear;
       gvClearMessageMemo := false;
     end
     else begin
       Lines.BeginUpdate;
       try
         while Lines.Count > Options.MessagesMaxLines do
            Lines.Delete(0);
       finally
         Lines.EndUpdate;
       end;
     end;
     if length(gvPubMessageTopic) > 0 then begin
       if gvShowTopics then
        outs := Format(sMsgFormatWithTopic, [Options.PubMsgHeader, gvPubMessageTopic, gvPubMessagePayload])
      else
        outs := Format(sMsgFormatNoTopic, [Options.PubMsgHeader, gvPubMessagePayload]);
       Lines.Add(outs);
       gvPubMessageTopic := '';
       gvPubMessagePayload := '';
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
      if gvShowTopics then
        FThisMessage := Format(sMsgFormatWithTopic, [Options.SubMsgHeader, topic, msg])
      else
        FThisMessage := Format(sMsgFormatNoTopic, [Options.SubMsgHeader, msg]);
      Synchronize(@UpdateGui);
   end;
end;

procedure mqttlog(const msg: ansistring);
begin
  MainForm.LogMemo.Lines.Add(msg);
  (* debug version)
  if assigned(MqttClient) then
    MainForm.LogMemo.Lines.Add(Format('%s (loglevel:%d, %d)', [msg, MainForm.FLogLevel, MqttClient.MOSQLogLevel]))
  else
    MainForm.LogMemo.Lines.Add(Format('%s (loglevel:%d)', [msg, MainForm.FLogLevel]));
  *)
end;

{ TMainForm }

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
  MqttClient := TThisMQTTConnection.Create('mqttClient', MqttConfig, FLogLevel);
  try
    MqttClient.AutoReconnect := aBroker.AutoReconnect;
    MqttClient.OnMessage := @MqttClient.MessageHandler;
    MqttClient.OnConnect := @MqttClient.ConnectionHandler;
    MqttClient.OnDisconnect := @MqttClient.ConnectionHandler;
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
  if ConnectButton.tag = 0 then begin
    ConnectBroker(Broker)
  end
  else
    freeandnil(MqttClient);
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
  Caption := changefileext(extractfilename(application.exename), '');
  if GetProgramVersion(Version) then with Version do
    caption := Format(sAppCaption, [Caption, Major,Minor,Revision]);

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
  FLogLevel := DEFAULT_LOG_LEVEL;
  RefreshGUI;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  freeandnil(MqttClient);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  if EncryptionKey = '' then begin
    ShowPublishResult(sNoEncryptionKey, false);
    halt;
  end;
  Options.LoadFromFile(optionsfile);
  UpdateFormOptions;
end;

procedure TMainForm.i18nFixup;
begin
  QosComboBox.Items.Text := sQosHint;
  QosComboBox.ItemIndex := 0;
end;

procedure TMainForm.LogMenuClearMenuItemClick(Sender: TObject);
begin
  LogMemo.Clear;
end;

procedure TMainForm.LogMenuCopyMenuItemClick(Sender: TObject);
begin
  ClipBoard.AsText := LogMemo.Lines.Text;
end;

procedure TMainForm.LogMenuCopySelectionMenuItemClick(Sender: TObject);
begin
  ClipBoard.AsText := LogMemo.SelText;
end;

procedure TMainForm.LogMenuLevelClick(Sender: TObject);
begin
  with (Sender as TMenuItem) do begin
     Checked := not Checked;
     if Checked then
       FLogLevel := FLogLevel or Tag
     else
       FLogLevel := FLogLevel and (not Tag);
  end;
  if assigned(MqttClient) then begin
    MqttClient.MQTTLogLevel := FLogLevel;
    MqttClient.MOSQLogLevel := FLogLevel;
  end;
  LogMemoMenu.popup(Fpt.X, Fpt.Y);
end;

procedure TMainForm.LogMemoMenuPopup(Sender: TObject);
begin
  Fpt := LogMemoMenu.PopupPoint;
  MenuItem5.Checked := (MenuItem5.tag and FLogLevel) > 0;
  MenuItem6.Checked := (MenuItem6.tag and FLogLevel) > 0;
  MenuItem7.Checked := (MenuItem7.tag and FLogLevel) > 0;
  MenuItem8.Checked := (MenuItem8.tag and FLogLevel) > 0;
  MenuItem9.Checked := (MenuItem9.tag and FLogLevel) > 0;
  MenuItem10.Checked := (MenuItem10.tag and FLogLevel) > 0;
  MenuItem11.Checked := (MenuItem11.tag and FLogLevel) > 0;
  MenuItem12.Checked := (MenuItem12.tag and FLogLevel) > 0;

  LogMenuCopySelectionMenuItem.Enabled := LogMemo.SelLength > 0;
  LogMenuClearMenuItem.Enabled := LogMemo.Lines.Count > 0;
  LogMenuCopyMenuItem.Enabled := LogMemo.Lines.Count > 0;
end;

procedure TMainForm.MessagesMenuCopySelectionMenuItemClick(Sender: TObject);
begin
  ClipBoard.AsText := MessagesMemo.SelText;
end;

procedure TMainForm.MessagesMenuClearMenuItemClick(Sender: TObject);
begin
  MessagesMemo.Clear;
end;

procedure TMainForm.MessagesMenuCopyMenuItemClick(Sender: TObject);
begin
  ClipBoard.AsText := MessagesMemo.Lines.Text;
end;

procedure TMainForm.MessageMenuWordWrapMenuItemClick(Sender: TObject);
begin
  MessagesMemo.WordWrap := MessageMenuWordWrapMenuItem.Checked;
end;

procedure TMainForm.MessagesMemoMenuPopup(Sender: TObject);
begin
   MessagesMenuCopySelectionMenuItem.Enabled := MessagesMemo.SelLength > 0;
   MessagesMenuClearMenuItem.Enabled := MessagesMemo.Lines.Count > 0;
   MessagesMenuCopyMenuItem.Enabled := MessagesMemo.Lines.Count > 0;
   MessageMenuWordWrapMenuItem.Checked := MessagesMemo.WordWrap;
end;

procedure TMainForm.OptionsButtonClick(Sender: TObject);
begin
  if TOptionsEditForm.EditOptions(Options) then begin
    Options.SaveToFile(optionsfile);
    UpdateFormOptions;
  end;
end;

procedure TMainForm.PayloadMemoEditingDone(Sender: TObject);
begin
  Broker.PubPayload := PayloadMemo.Text;
end;

procedure TMainForm.PublishButtonClick(Sender: TObject);
var
  res: integer;
  topic, msg: string;
  ticks: QWord;
  delay: QWord;
begin
  topic := trim(PublishTopicEdit.Text);
  res := 0; // assume connected and so on

  if topic = '' then
    res := -1002
  else begin
    if (not assigned(MqttClient) or (MQttClient.State <> mqttConnected))
      and Options.AutoConnectOnPublish then begin
      // attempt to automatically connect to the broker
      ConnectBroker(Broker);
      // wait until connected or the autoconnect delay has timed out
      ticks := gettickcount64;
      // ticks are milliseconds, Options.AutoConnectDelay is seconds
      delay := Options.AutoConnectDelay*1000;
      while assigned(MqttClient)
        and (MQttClient.State <> mqttConnected)
        and (gettickcount64 - ticks < delay) do begin
        sleep(2);
        application.ProcessMessages;
      end;
      // continue
    end;
    if not assigned(MqttClient) then
      res := -1000
    else if MQttClient.state <> mqttConnected then
      res := -1001;
  end;

  if res = 0 then begin
    // connected to the broker, publish message to it
    gvClearMessageMemo := autoClearCheckBox.checked;
    res := MqttClient.Publish(topic, PayloadMemo.Text,
      QoSComboBox.ItemIndex, RetainCheckBox.Checked);
    if (res = 0) then begin
      if CopyPubCheckBox.checked then begin
        gvPubMessageTopic := topic;
        gvPubMessagePayload := PayloadMemo.Text;
      end;
      if (PayloadMemo.Text = '') then begin
        res := -1003;
        if RetainCheckBox.Checked then dec(res); // -1004
      end;
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
    gvClearMessageMemo := false;
  ShowPublishResult(msg, res = 0);
  RetainCheckBox.Checked := false;
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
    MessagesMemo.Clear;
    LogMemo.Clear;
    PageControl1.ActivePage := TabSheet1;
    UpdateConnectionState;
  end;
end;

procedure TMainForm.RetainCheckBoxChange(Sender: TObject);
begin
  RetainCheckBox.Caption := Format('(%s)', [falsetruestr[RetainCheckBox.Checked]]);
  Broker.PubRetain := RetainCheckBox.Checked;;
end;

procedure TMainForm.ShowTopicsCheckBoxChange(Sender: TObject);
begin
  gvShowTopics := ShowTopicsCheckBox.Checked;
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
  /// debug: writeln(format('state changed to %d', [newState]));
  statusLabel.Caption := statusStr[newState];
  if (newState in [mqttConnecting, mqttConnected, mqttReconnecting]) then begin
    ConnectButton.Caption := sDisconnect;
    ConnectButton.tag := 1;
  end
  else begin
    ConnectButton.Caption := sConnect;
    ConnectButton.tag := 0;
  end;
  if newState = mqttConnected then begin
    /// debug: writeln('subscribing to all topics');
    // subscribe to all used topics
    for i := 0 to Broker.SubTopicsCount-1 do begin
      if Broker.SubTopics[i].Use then begin
        MqttClient.Subscribe(Broker.subTopics[i].Topic, Broker.subTopics[i].QoS); // and subsbcribe to wanted topics
      end;
    end;
  end;
end;

procedure TMainForm.UpdateFormOptions;
begin
  CopyPubCheckbox.Checked := Options.CopyPubMessages;
  autoClearCheckBox.Checked := Options.AutoClearOnPublish;
  ShowTopicsCheckbox.Checked := Options.ShowTopics;
  gvShowTopics := Options.ShowTopics;
end;

initialization
  mqtt_setlogfunc(@mqttlog);
end.



