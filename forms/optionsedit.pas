unit optionsedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, EditBtn, ActnList, ComCtrls, optionsunit;

type

  { TOptionsEditForm }

  TOptionsEditForm = class(TForm)
    AcceptButton: TButton;
    CancelButton: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LoadButton: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Panel1: TPanel;
    SaveButton: TButton;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    procedure AcceptButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    FOptions: TOptions;
    FSource: TOptions;
    FCheckModified: boolean;
    procedure i18nFixup;
    procedure UpdateView;
  public
    class function EditOptions(aOptions: TOptions): boolean;
  end;

var
  configfile: string; // see initialization

implementation

{$R *.lfm}

uses
  startup, stringres, editsubtopic, verify;

const
  CONFIGFILENAME = 'default.json';

{ TOptionsEditForm }

class function TOptionsEditForm.EditOptions(aOptions: TOptions): boolean;
begin
  with TOptionsEditForm.Create(application) do begin
    FOptions.Assign(aOptions);
    FSource := aOptions;
    UpdateView;
    result := ShowModal = mrOk;
  end;
end;

procedure TOptionsEditForm.AcceptButtonClick(Sender: TObject);
begin
  FCheckModified := false;
  if FOptions.isEqual(FSource) then begin
    ModalResult := mrNone;
    close;
  end
  else begin
    FSource.assign(FOptions);
    ModalResult := mrOk;
  end;
end;

(*
procedure TOptionsEditForm.AutoReconnectCheckBoxChange(Sender: TObject);
begin
  FBroker.AutoReconnect := AutoReconnectCheckBox.Checked;
  AutoReconnectCheckBox.Caption := Format('(%s)', [falsetruestr[FBroker.AutoReconnect]]);
end;
*)

procedure TOptionsEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TOptionsEditForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  aControl: TWinControl;
begin
  // Clicking on close [X] button while in an Edit control means that
  // an attempt to close the form will be made without calling the EditingDone
  // handler. Switching the active control before checking for changes
  // will result in EditingDone being called.
  (*
  aControl := ActiveControl;
  if assigned(aControl) then
    ListBox1.SetFocus;
  if FCheckModified and not FBroker.isEqual(FSource) then
    CanClose := ConfirmAction(cbsLoseChanges);
  if not CanClose and assigned(aControl) then
    aControl.SetFocus;
  *)
end;

procedure TOptionsEditForm.FormCreate(Sender: TObject);
begin
  i18nFixup;
  FCheckModified := true;
  FOptions := TOptions.create;
  OpenDialog.Filename := optionsfile;
  SaveDialog.Filename := optionsfile;
end;

procedure TOptionsEditForm.FormDestroy(Sender: TObject);
begin
  FOptions.free;
end;


procedure TOptionsEditForm.i18nFixup;
begin
  (*
  ListBox1.Items.Text := sPageNames;
  ListBox1.ItemIndex := 0;
  QosComboBox.Items.Text := sQosHint;
  QosComboBox.ItemIndex := 0;
  *)
end;


procedure TOptionsEditForm.LoadButtonClick(Sender: TObject);
begin
  OpenDialog.Filter := FileDialogFilter;
  OpenDialog.Filename := optionsfile;
  if OpenDialog.Execute then begin
    FOptions.LoadFromFile(OpenDialog.FileName);
    UpdateView;
    optionsfile := OpenDialog.Filename;
  end;
end;


procedure TOptionsEditForm.SaveButtonClick(Sender: TObject);
begin
  SaveDialog.Filename := configfile;
  SaveDialog.Filter := FileDialogFilter;
  if SaveDialog.Execute then begin
    FOptions.SaveToFile(SaveDialog.filename);
    optionsfile := SaveDialog.Filename;
  end;
end;


procedure TOptionsEditForm.UpdateView;
begin
  (*
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
  *)
end;

end.


