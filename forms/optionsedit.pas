unit optionsedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, ComCtrls, EditBtn, optionsunit;

type

  { TOptionsEditForm }

  TOptionsEditForm = class(TForm)
    AcceptButton: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    CancelButton: TButton;
    AutoConnectCheckBox: TCheckBox;
    AutoClearCheckBox: TCheckBox;
    DefaultBrokerFileNameEdit: TFileNameEdit;
    Label7: TLabel;
    ShowPublishedCheckBox: TCheckBox;
    ShowTopicsCheckBox: TCheckBox;
    PubHeaderEdit: TEdit;
    SubHeaderEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    ResetButton: TButton;
    AutoConnectDelayEdit: TSpinEdit;
    MaxLinesEdit: TSpinEdit;
    procedure AcceptButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure DefaultBrokerFileNameEditAcceptFileName(Sender: TObject;
      var Value: String);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
  private
    FOptions: TOptions;
    FSource: TOptions;
    procedure i18nFixup;
    procedure UpdateOptions;
    procedure UpdateView;
  public
    class function EditOptions(aOptions: TOptions): boolean;
  end;

implementation

{$R *.lfm}

uses
  startup, stringres, verify;

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
  UpdateOptions;
  if FOptions.isEqual(FSource) then begin
    ModalResult := mrNone;
    close;
  end
  else begin
    FSource.assign(FOptions);
    ModalResult := mrOk;
  end;
end;

procedure TOptionsEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TOptionsEditForm.DefaultBrokerFileNameEditAcceptFileName(
  Sender: TObject; var Value: String);
begin
  if extractfilepath(Value) = extractfilepath(configfile) then
    value := extractfilename(value);
end;

procedure TOptionsEditForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  aControl: TWinControl;
begin
  // Clicking on close [X] button while in an Edit control means that
  // an attempt to close the form will be made without calling the EditingDone
  // handler. Switching the active control before checking for changes
  // will result in EditingDone being called.

  aControl := ActiveControl;
  (*
  if assigned(aControl) then
    if aControl = SubHeaderEdit then
      PubHeaderEdit.SetFocus
    else
      SubHeaderEdit.SetFocus; *)
  UpdateOptions;
  if not FOptions.isEqual(FSource) then
    CanClose := ConfirmAction(cbsLoseChanges);
  if not CanClose and assigned(aControl) then
    aControl.SetFocus;
end;

procedure TOptionsEditForm.FormCreate(Sender: TObject);
begin
  i18nFixup;
  FOptions := TOptions.create;
  DefaultBrokerFileNameEdit.InitialDir := extractfilepath(configfile);
  DefaultBrokerFileNameEdit.Filter := sJSONFiles + '|*.json|' + sAllFiles + '|*.*';
end;

procedure TOptionsEditForm.FormDestroy(Sender: TObject);
begin
  FOptions.free;
end;

procedure TOptionsEditForm.i18nFixup;
begin
  (* Nothing to do *)
end;

procedure TOptionsEditForm.ResetButtonClick(Sender: TObject);
begin
  FOptions.Clear;
  UpdateView;
end;

procedure TOptionsEditForm.UpdateOptions;
begin
  with FOptions do begin
    MessagesMaxLines     := MaxLinesEdit.value;
    AutoclearOnPublish   := AutoClearCheckBox.checked;
    AutoconnectOnPublish := AutoConnectCheckBox.checked;
    AutoconnectDelay     := AutoConnectDelayEdit.value;
    PubMsgHeader         := PubHeaderEdit.Text;
    SubMsgHeader         := SubHeaderEdit.Text;
    CopyPubMessages      := ShowPublishedCheckBox.checked;
    ShowTopics           := ShowTopicsCheckBox.checked;
    DefaultBrokerFile    := DefaultBrokerFilenameEdit.Filename;
  end;
end;

procedure TOptionsEditForm.UpdateView;
begin
  with FOptions do begin
    MaxLinesEdit.value := MessagesMaxLines;
    AutoClearCheckBox.checked := AutoclearOnPublish;
    AutoConnectCheckBox.checked := AutoconnectOnPublish;
    AutoConnectDelayEdit.value := AutoconnectDelay;
    PubHeaderEdit.Text := PubMsgHeader;
    SubHeaderEdit.Text := SubMsgHeader;
    ShowPublishedCheckBox.checked := CopyPubMessages;
    ShowTopicsCheckBox.checked := ShowTopics;
    DefaultBrokerFilenameEdit.Filename := DefaultBrokerFile;
  end;
end;

end.


