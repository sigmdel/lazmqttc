unit brokeredit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, EditBtn, ActnList, ComCtrls, Grids, brokerunit, topicgrids;

type

  { TBrokerEditForm }

  TBrokerEditForm = class(TForm)
    AcceptButton: TButton;
    actAdd: TAction;
    actClear: TAction;
    actDel: TAction;
    actEdit: TAction;
    ActionList1: TActionList;
    CancelButton: TButton;
    AutoReconnectCheckBox: TCheckBox;
    Label10: TLabel;
    Label14: TLabel;
    PayloadMemo: TMemo;
    ReconnectBackoffCheckBox: TCheckBox;
    SSLCheckBox: TCheckBox;
    PubTopicEdit: TEdit;
    PubRetainCheckBox: TCheckBox;
    HostEdit: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    LoadButton: TButton;
    SSLCertMemo: TMemo;
    Notebook: TNotebook;
    BrokerPage: TPage;
    EncryptionPage: TPage;
    OpenDialog: TOpenDialog;
    PasswordEdit: TEditButton;
    PortEdit: TSpinEdit;
    KeepAlivesEdit: TSpinEdit;
    ReconnectDelayEdit: TSpinEdit;
    QoSComboBox: TComboBox;
    SaveDialog: TSaveDialog;
    SubscribePage: TPage;
    PublishPage: TPage;
    SecurityPage: TPage;
    Panel1: TPanel;
    SaveButton: TButton;
    tbAdd: TToolButton;
    tbClear: TToolButton;
    tbDelete: TToolButton;
    tbEdit: TToolButton;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    UserEdit: TEdit;
    procedure AcceptButtonClick(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure AutoReconnectCheckBoxChange(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure HostEditEditingDone(Sender: TObject);
    procedure KeepAlivesEditEditingDone(Sender: TObject);
    procedure PasswordEditButtonClick(Sender: TObject);
    procedure PasswordEditEditingDone(Sender: TObject);
    procedure PayloadMemoEditingDone(Sender: TObject);
    procedure ReconnectBackoffCheckBoxChange(Sender: TObject);
    procedure ReconnectDelayEditEditingDone(Sender: TObject);
    procedure PortEditEditingDone(Sender: TObject);
    procedure PubRetainCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure PubTopicEditEditingDone(Sender: TObject);
    procedure QoSComboBoxChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SSLCertMemoEditingDone(Sender: TObject);
    procedure SSLCheckBoxEditingDone(Sender: TObject);
    procedure UserEditEditingDone(Sender: TObject);
  private
    FBroker: TBroker;
    FSource: TBroker;
    FCheckModified: boolean;
    procedure Change;
    procedure i18nFixup;
    procedure UpdateView;
  public
    TopicsGrid: TSubTopicsGrid;
    class function EditBroker(aBroker: TBroker): boolean;
  end;

var
  configfile: string; // see initialization

implementation

{$R *.lfm}

uses
  stringres, editsubtopic, verify;

const
  CONFIGFILENAME = 'default.json';



{ TBrokerEditForm }

class function TBrokerEditForm.EditBroker(aBroker: TBroker): boolean;
begin
  with TBrokerEditForm.Create(application) do begin
    FBroker.Assign(aBroker);
    FSource := aBroker;
    UpdateView;
    ListBox1.ItemIndex := 0;
    result := ShowModal = mrOk;
  end;
end;

procedure TBrokerEditForm.AcceptButtonClick(Sender: TObject);
begin
  FCheckModified := false;
  if FBroker.isEqual(FSource) then begin
    ModalResult := mrNone;
    close;
  end
  else begin
    FSource.assign(FBroker);
    ModalResult := mrOk;
  end;
end;

procedure TBrokerEditForm.actAddExecute(Sender: TObject);
begin
  TopicsGrid.HideEditor;
  with TEditSubTopicForm.Create(self) do try
    TopicEdit.Text := '';
    QoSComboBox.ItemIndex := 0;
    UseCheckBox.Checked := True;
    if (ShowModal = mrOk) and (trim(TopicEdit.Text) <> '') then begin
      FBroker.AddSubTopic(Trim(TopicEdit.Text), QoSComboBox.ItemIndex, UseCheckBox.Checked);
      TopicsGrid.UpdateGridSize;
      Change;
    end;
  finally
    free;
  end;
end;

procedure TBrokerEditForm.actClearExecute(Sender: TObject);
begin
  if FBroker.SubTopicsCount > 0 then begin
    TopicsGrid.HideEditor;
    FBroker.ClearSubTopics;
    TopicsGrid.UpdateGridSize;
    Change;
  end;
end;

procedure TBrokerEditForm.actDelExecute(Sender: TObject);
var
  OldIndex : integer;
begin
  with TopicsGrid do begin
    OldIndex := row;
    if (row > 0)
      and (row <= FBroker.SubTopicsCount)
      and ConfirmAction(Format(cbsDeleteQuery, [FBroker.SubTopics[pred(row)].Topic])) then begin
        TopicsGrid.HideEditor;
        FBroker.DeleteSubTopic(pred(row));
        UpdateGridSize;
        if OldIndex >= RowCount then
          Row := RowCount-1
        else
          Row := OldIndex;
        Change;
    end;
  end;
end;

procedure TBrokerEditForm.actEditExecute(Sender: TObject);
var
  ndx: integer;
begin
  with TopicsGrid do begin
    if (row > 0) and (row <= FBroker.SubTopicsCount) then begin
      TopicsGrid.HideEditor;
      ndx := pred(row);
      with TEditSubTopicForm.Create(self) do try
        TopicEdit.Text := FBroker.SubTopics[ndx].topic;
        QoSComboBox.ItemIndex := FBroker.SubTopics[ndx].qos;
        UseCheckBox.Checked := FBroker.SubTopics[ndx].use;
        if ShowModal = mrOk then begin
          if trim(TopicEdit.Text) = '' then
            actDelExecute(sender)            /// verify before ?
          else begin
            FBroker.SubTopics[ndx].topic := trim(TopicEdit.Text);
            FBroker.SubTopics[ndx].qos   := QoSComboBox.ItemIndex;
            FBroker.SubTopics[ndx].use   := UseCheckBox.Checked;
          end;
        end;
      finally
        free;
      end;
    end;
  end;
end;

procedure TBrokerEditForm.AutoReconnectCheckBoxChange(Sender: TObject);
begin
  FBroker.AutoReconnect := AutoReconnectCheckBox.Checked;
  AutoReconnectCheckBox.Caption := Format('(%s)', [falsetruestr[FBroker.AutoReconnect]]);
end;

procedure TBrokerEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TBrokerEditForm.Change;
begin
  actDel.Enabled := (TopicsGrid.RowCount > 1) and (TopicsGrid.Row > 0);
  actEdit.Enabled := actDel.Enabled;
  actClear.Enabled := TopicsGrid.RowCount > 1;
end;

procedure TBrokerEditForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  aControl: TWinControl;
begin
  // Clicking on close [X] button while in an Edit control means that
  // an attempt to close the form will be made without calling the EditingDone
  // handler. Switching the active control before checking for changes
  // will result in EditingDone being called.
  aControl := ActiveControl;
  if assigned(aControl) then
    ListBox1.SetFocus;
  if FCheckModified and not FBroker.isEqual(FSource) then
    CanClose := ConfirmAction(cbsLoseChanges);
  if not CanClose and assigned(aControl) then
    aControl.SetFocus;
end;

procedure TBrokerEditForm.FormCreate(Sender: TObject);
begin
  i18nFixup;
  FCheckModified := true;
  FBroker := TBroker.create;
  TopicsGrid := TSubTopicsGrid.Create(self);
  with TopicsGrid do begin
     parent := SubscribePage;
     AnchorSideLeft.Control := ToolBar;
     AnchorSideTop.Control := ToolBar;
     AnchorSideTop.Side := asrBottom;
     AnchorSideRight.Control := ToolBar;
     AnchorSideRight.Side := asrBottom;
     AnchorSideBottom.Control := SubscribePage;
     AnchorSideBottom.Side := asrBottom;
     Left := 12;
     Height := 339;
     Top := 31;
     Width := 490;
     Anchors := [akTop, akLeft, akRight, akBottom];
     Broker := FBroker;
     //OnGetCellHint := @TopicGridGetCellHint;
     Options := Options + [goCellHints];
     ShowHint := true;
  end;
  OpenDialog.Filename := configfile;
  SaveDialog.Filename := configfile;
end;

procedure TBrokerEditForm.FormDestroy(Sender: TObject);
begin
  FBroker.free;
end;

procedure TBrokerEditForm.HostEditEditingDone(Sender: TObject);
begin
  FBroker.Host := trim(HostEdit.Text);
end;

procedure TBrokerEditForm.i18nFixup;
begin
  ListBox1.Items.Text := sPageNames;
  ListBox1.ItemIndex := 0;
  QosComboBox.Items.Text := sQosHint;
  QosComboBox.ItemIndex := 0;
end;

procedure TBrokerEditForm.KeepAlivesEditEditingDone(Sender: TObject);
begin
  FBroker.KeepAlives := KeepAlivesEdit.Value;
end;

procedure TBrokerEditForm.ListBox1Click(Sender: TObject);
begin
  with ListBox1 do begin
    if ItemIndex < 0 then
      ItemIndex := 0;
    Notebook.PageIndex := ItemIndex
  end;
end;

procedure TBrokerEditForm.LoadButtonClick(Sender: TObject);
begin
  PasswordEdit.passwordchar := '#';
  PasswordEdit.ImageIndex := 0;
  OpenDialog.Filename := configfile;
  if OpenDialog.Execute then begin
    FBroker.LoadFromFile(OpenDialog.FileName);
    UpdateView;
    configfile := OpenDialog.Filename;
  end;
end;

procedure TBrokerEditForm.PasswordEditButtonClick(Sender: TObject);
begin
  with PasswordEdit do begin
     if passwordchar = #0 then begin  // hide password
       passwordchar := '*';
       ImageIndex := 0;
     end
     else begin // show password
       passwordchar := #0;
       ImageIndex := 1;
     end;
   end;
end;

procedure TBrokerEditForm.PasswordEditEditingDone(Sender: TObject);
begin
  FBroker.Password := PasswordEdit.Text;
end;

procedure TBrokerEditForm.PayloadMemoEditingDone(Sender: TObject);
begin
  FBroker.PubPayload := PayloadMemo.Text;
end;

procedure TBrokerEditForm.PortEditEditingDone(Sender: TObject);
begin
  FBroker.Port := PortEdit.Value;
end;

procedure TBrokerEditForm.PubRetainCheckBoxChange(Sender: TObject);
begin
  FBroker.PubRetain := PubRetainCheckBox.Checked;
  PubRetainCheckBox.Caption := Format('(%s)', [falsetruestr[FBroker.PubRetain]]);
end;

procedure TBrokerEditForm.PubTopicEditEditingDone(Sender: TObject);
begin
  FBroker.PubTopic := trim(PubTopicEdit.Text);
end;

procedure TBrokerEditForm.QoSComboBoxChange(Sender: TObject);
begin
  if QoSComboBox.ItemIndex >= 0 then
    FBroker.PubQoS := QoSComboBox.ItemIndex;
end;

procedure TBrokerEditForm.ReconnectBackoffCheckBoxChange(Sender: TObject);
begin
  FBroker.ReconnectBackoff := ReconnectBackoffCheckBox.Checked;
  ReconnectBackoffCheckBox.Caption := Format('(%s)', [falsetruestr[FBroker.ReconnectBackoff]]);
end;

procedure TBrokerEditForm.ReconnectDelayEditEditingDone(Sender: TObject);
begin
  FBroker.ReconnectDelay := ReconnectDelayEdit.Value;
end;

procedure TBrokerEditForm.SaveButtonClick(Sender: TObject);
begin
  SaveDialog.Filename := configfile;
  if SaveDialog.Execute then begin
    FBroker.SaveToFile(SaveDialog.filename);
    configfile := SaveDialog.Filename;
  end;
end;

procedure TBrokerEditForm.SSLCertMemoEditingDone(Sender: TObject);
begin
  FBroker.SSLCert := SSLCertMemo.Text;
end;

procedure TBrokerEditForm.SSLCheckBoxEditingDone(Sender: TObject);
begin
  FBroker.SSL := SSLCheckBox.Checked;
end;

procedure TBrokerEditForm.UserEditEditingDone(Sender: TObject);
begin
  FBroker.User := UserEdit.Text;
end;

procedure TBrokerEditForm.UpdateView;
begin
  with FBroker do begin
    HostEdit.Text := Host;
    PortEdit.Value := Port;
    KeepAlivesEdit.Value := KeepAlives;
    ReconnectDelayEdit.Value := ReconnectDelay;
    ReconnectBackoffCheckBox.Checked := ReconnectBackoff;
    AutoReconnectCheckBox.Checked := AutoReconnect;
    UserEdit.Text := User;
    PasswordEdit.Text := Password;
    SSLCheckBox.Checked := SSL;
    SSLCertMemo.Text := SSLCert;
    PubTopicEdit.Text := PubTopic;
    PayloadMemo.Text := PubPayload;
    QoSComboBox.ItemIndex := PubQoS;
    PubRetainCheckBox.Checked := PubRetain;
    TopicsGrid.Broker := FBroker;
    TopicsGrid.UpdateGridSize;
    TopicsGrid.Invalidate;
    if TopicsGrid.RowCount > 1 then
      TopicsGrid.Row := 1;
    Change;
  end;
end;

function Vendor: string;
begin
  result := 'sigmdel';
end;

function  GetAppName: string;
begin
  result := changefileext(extractfilename(paramstr(0)), '');
end;

initialization
  OnGetVendorName := @Vendor;
  OnGetApplicationName := @GetAppName;
  configfile := GetAppConfigDir(false);
  ForceDirectories(configfile);   // create config directory, report error if false ?
  configfile := IncludeTrailingPathDelimiter(configfile) + CONFIGFILENAME;
end.


