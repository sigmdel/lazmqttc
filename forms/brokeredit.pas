unit brokeredit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  CheckLst, ComCtrls, ExtCtrls, ActnList, EditBtn, BrokerUnit;

type

  { TBrokerEditForm }

  TBrokerEditForm = class(TForm)
    actAdd: TAction;
    actDel: TAction;
    actEdit: TAction;
    actClear: TAction;
    ActionList1: TActionList;
    actMoveDown: TAction;
    actMoveUp: TAction;
    AcceptButton: TButton;
    Label6: TLabel;
    PasswordEdit: TEditButton;
    SaveButton: TButton;
    LoadButton: TButton;
    CancelButton: TButton;
    ImageList1: TImageList;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    Label7: TLabel;
    PortEdit: TSpinEdit;
    HostEdit: TEdit;
    SaveDialog: TSaveDialog;
    SubTopicsListBox: TCheckListBox;
    tbAdd: TToolButton;
    tbDelete: TToolButton;
    tbDown: TToolButton;
    tbEdit: TToolButton;
    tbUp: TToolButton;
    ToolBar: TToolBar;
    tbClear: TToolButton;
    ToolButton1: TToolButton;
    UserEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PubTopicEdit: TEdit;
    procedure AcceptButtonClick(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HostEditChange(Sender: TObject);
    procedure PasswordEditChange(Sender: TObject);
    procedure PortEditChange(Sender: TObject);
    procedure PubTopicEditChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PasswordEditButtonClick(Sender: TObject);
    procedure SubTopicsListBoxClickCheck(Sender: TObject);
    procedure UserEditChange(Sender: TObject);
  private
    FBroker: TBroker;
    FSource: TBroker;
    FCheckModified: boolean;
    procedure Change;
    procedure ClearFields;
    procedure UpdateView;
  public
    class function EditBroker(aBroker: TBroker): boolean;
  end;

implementation

{$R *.lfm}

resourcestring
  csbBrokerEditor = 'MQTT Broker Editor';
  cbsLoseChanges = 'Close without saving changes';
  cbsCheckListBoxEditor = 'Subscribe Topics Editor';
  cbsUp = 'Move the topic up';
  cbsDown = 'Move the topic down';
  cbsEdit = 'Edit the topic';
  cbsAdd = 'Add new topic';
  cbsDelete = 'Delete the topic';
  cbsDeleteQuery = 'Delete <%s>';
  cbsClear = 'Delete all topics';

const
  CONFIGFILENAME = 'mqttviewer.json';

var
  configfile: string; // see initialization

{ TBrokerEditForm }


procedure TBrokerEditForm.AcceptButtonClick(Sender: TObject);
begin
  FCheckModified := false;
  FSource.assign(FBroker);
  FBroker.SaveToFile('/home/michel/broker2.json');
  if FBroker.Modified then
    ModalResult := mrOk
  else begin
    ModalResult := mrNone;
    close;
  end;
end;

procedure TBrokerEditForm.actAddExecute(Sender: TObject);
var
  strItem: string;
begin
  strItem:='';
  if InputQuery(cbsCheckListBoxEditor, cbsAdd, strItem) and (strItem <> '') then begin
    SubTopicsListBox.Items.Add(strItem);
    SubTopicsListBox.Checked[SubTopicsListBox.Count-1] := true;
    FBroker.AddSubTopic(strItem, true);
    Change;
  end;
end;

procedure TBrokerEditForm.actClearExecute(Sender: TObject);
begin
  if (SubTopicsListBox.Items.Count > 0)
  and (MessageDlg(cbsCheckListBoxEditor, cbsClear,  mtConfirmation, mbYesNo, 0) = mrYes) then begin
    SubTopicsListBox.Items.clear;
    FBroker.SubTopicsClear;
    Change;
  end;
end;

procedure TBrokerEditForm.actDelExecute(Sender: TObject);
var
  OldIndex : integer;
begin
  if SubTopicsListBox.ItemIndex = -1 then exit;
  if MessageDlg(cbsCheckListBoxEditor, Format(cbsDeleteQuery, [SubTopicsListBox.Items[SubTopicsListBox.ItemIndex]]),
  mtConfirmation, mbYesNo, 0) = mrYes then begin
    //  save old index
    OldIndex := SubTopicsListBox.ItemIndex;
    FBroker.DeleteSubTopic(OldIndex);
    SubTopicsListBox.Items.Delete(SubTopicsListBox.ItemIndex);
    if (SubTopicsListBox.Count-1<OldIndex) then
      SubTopicsListBox.ItemIndex := SubTopicsListBox.Count-1
    else
      SubTopicsListBox.ItemIndex := OldIndex;
    Change;
  end;
end;

procedure TBrokerEditForm.actEditExecute(Sender: TObject);
var
  ItemIndex: integer;
  checkd: boolean;
begin
  ItemIndex :=  SubTopicsListBox.ItemIndex;
  if (SubTopicsListBox.Items.Count = 0) or (ItemIndex = -1) then
     Exit;
  checkd := SubTopicsListBox.Checked[ItemIndex];
  SubTopicsListBox.Items[ItemIndex] := InputBox(cbsCheckListBoxEditor,
     cbsEdit, SubTopicsListBox.Items[ItemIndex]);
  SubTopicsListBox.Checked[ItemIndex] := checkd;
  FBroker.SubTopics[ItemIndex] := SubTopicsListBox.Items[ItemIndex];
end;

procedure TBrokerEditForm.actMoveDownExecute(Sender: TObject);
var
  itemtmp: string;
  checkedtmp: boolean;
  currentIndex: integer;
begin
  currentIndex := SubTopicsListBox.ItemIndex;
  if (SubTopicsListBox.Items.Count<=1)or(currentIndex=SubTopicsListBox.Items.Count-1)or(currentIndex=-1) then exit;
  FBroker.ExchangeSubTopics(currentIndex, succ(currentIndex));
  // clean up below
  itemtmp := SubTopicsListBox.Items[SubTopicsListBox.ItemIndex+1];
  checkedtmp := SubTopicsListBox.Checked[SubTopicsListBox.ItemIndex+1];
  SubTopicsListBox.Items[SubTopicsListBox.ItemIndex+1] := SubTopicsListBox.Items[SubTopicsListBox.ItemIndex];
  SubTopicsListBox.Checked[SubTopicsListBox.ItemIndex+1] := SubTopicsListBox.Checked[SubTopicsListBox.ItemIndex];
  SubTopicsListBox.Items[SubTopicsListBox.ItemIndex] := itemtmp;
  SubTopicsListBox.Checked[SubTopicsListBox.ItemIndex] := checkedtmp;
  SubTopicsListBox.ItemIndex := SubTopicsListBox.ItemIndex+1;
  Change;
end;

procedure TBrokerEditForm.actMoveUpExecute(Sender: TObject);
var
  itemtmp: string;
  checkedtmp: boolean;
  currentIndex: integer;
begin
  currentIndex := SubTopicsListBox.ItemIndex;
  if (SubTopicsListBox.Items.Count<=1)or(currentIndex<1) then exit;
  // clean up below
  FBroker.ExchangeSubTopics(currentIndex, pred(currentIndex));
  itemtmp := SubTopicsListBox.Items[SubTopicsListBox.ItemIndex-1];
  checkedtmp := SubTopicsListBox.Checked[SubTopicsListBox.ItemIndex-1];
  SubTopicsListBox.Items[SubTopicsListBox.ItemIndex-1] := SubTopicsListBox.Items[SubTopicsListBox.ItemIndex];
  SubTopicsListBox.Checked[SubTopicsListBox.ItemIndex-1] := SubTopicsListBox.Checked[SubTopicsListBox.ItemIndex];
  SubTopicsListBox.Items[SubTopicsListBox.ItemIndex] := itemtmp;
  SubTopicsListBox.Checked[SubTopicsListBox.ItemIndex] := checkedtmp;
  SubTopicsListBox.ItemIndex := SubTopicsListBox.ItemIndex-1;
  Change;
end;

procedure TBrokerEditForm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  if FCheckModified and FBroker.Modified then
     CanClose := MessageDlg(csbBrokerEditor, cbsLoseChanges,  mtConfirmation, mbYesNo, 0) = mrYes;
end;

procedure TBrokerEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TBrokerEditForm.Change;
begin
  actClear.Enabled := SubTopicsListBox.Items.Count > 0;
  actDel.Enabled := SubTopicsListBox.ItemIndex <> -1;
  actEdit.Enabled := SubTopicsListBox.ItemIndex <> -1;
  actMoveUp.Enabled := (SubTopicsListBox.ItemIndex <> -1) and (SubTopicsListBox.ItemIndex > 0);
  actMoveDown.Enabled := (SubTopicsListBox.ItemIndex <> -1) and (SubTopicsListBox.ItemIndex < SubTopicsListBox.Count - 1);
end;

procedure TBrokerEditForm.ClearFields;
begin
  HostEdit.Text := '';
  PortEdit.Value := 1883;
  UserEdit.Text := '';
  PasswordEdit.Text := '';
  SubTopicsListBox.Items.Clear;
end;

class function TBrokerEditForm.EditBroker(aBroker: TBroker): boolean;
begin
  with TBrokerEditForm.Create(application) do begin
    ClearFields;
    FBroker.Assign(aBroker);
    FSource := aBroker;
    result := ShowModal = mrOk;
  end;
end;

procedure TBrokerEditForm.FormCreate(Sender: TObject);
begin
  FBroker := TBroker.Create;
end;

procedure TBrokerEditForm.FormDestroy(Sender: TObject);
begin
  FBroker.Free;
end;

procedure TBrokerEditForm.HostEditChange(Sender: TObject);
begin
  FBroker.Host := HostEdit.Text;
end;

procedure TBrokerEditForm.PasswordEditChange(Sender: TObject);
begin
  FBroker.Password := PasswordEdit.Text;
end;

procedure TBrokerEditForm.PortEditChange(Sender: TObject);
begin
  FBroker.Port := PortEdit.Value;
end;

procedure TBrokerEditForm.PubTopicEditChange(Sender: TObject);
begin
  FBroker.PubTopic := PubTopicEdit.Text;
end;

procedure TBrokerEditForm.FormShow(Sender: TObject);
begin
  OpenDialog.Filename := configfile;
  SaveDialog.Filename := configfile;
  actMoveUp.Hint := cbsUp;
  actMoveDown.Hint := cbsDown;
  actDel.Hint := cbsDelete;
  actAdd.Hint := cbsAdd;
  actEdit.Hint := cbsEdit;
  actClear.Hint := cbsClear;
  UpdateView;
end;

procedure TBrokerEditForm.UpdateView;
var
  i : integer;
begin
  with FBroker do begin
    HostEdit.Text := Host;
    PortEdit.Value := Port;
    UserEdit.Text := User;
    PasswordEdit.Text := Password;
    PubTopicEdit.Text := PubTopic;
    AssignTo(SubTopicsListBox);
    if Count > 0 then
      SubTopicsListBox.ItemIndex := 0;
    Change;
  end;
end;

procedure TBrokerEditForm.LoadButtonClick(Sender: TObject);
begin
  OpenDialog.Filename := configfile;
  if OpenDialog.Execute then begin
    FBroker.LoadFromFile(OpenDialog.FileName);
    UpdateView;
    configfile := OpenDialog.Filename;
    FBroker.Modified := true;
  end;
end;

procedure TBrokerEditForm.PasswordEditButtonClick(Sender: TObject);
begin
  with PasswordEdit do begin
    if passwordchar = #0 then begin  // hide password
      passwordchar := '*';
      ImageIndex := 6;
    end
    else begin // show password
      passwordchar := #0;
      ImageIndex := 7;
    end;
  end;
end;

procedure TBrokerEditForm.SaveButtonClick(Sender: TObject);
begin
  SaveDialog.Filename := configfile;
  if SaveDialog.Execute then begin
    FBroker.SaveToFile(SaveDialog.filename);
    configfile := SaveDialog.Filename;
  end;
end;

procedure TBrokerEditForm.SubTopicsListBoxClickCheck(Sender: TObject);
var
  itemIndex: integer;
begin
  itemIndex := SubTopicsListBox.ItemIndex;
  if (itemIndex < 0) or (itemIndex >= FBroker.Count) then exit;
  FBroker.Subscribed[itemIndex] := SubTopicsListBox.Checked[itemIndex];
end;

procedure TBrokerEditForm.UserEditChange(Sender: TObject);
begin
  FBroker.User := UserEdit.Text;
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

