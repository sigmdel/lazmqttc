unit editsubtopic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TEditSubTopicForm }

  TEditSubTopicForm = class(TForm)
    BottomPanel: TPanel;
    Button1: TButton;
    Button2: TButton;
    UseCheckBox: TCheckBox;
    Label3: TLabel;
    QoSComboBox: TComboBox;
    TopicEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure i18nFixup;
  public

  end;

//var
  //EditSubTopicForm: TEditSubTopicForm;

implementation

{$R *.lfm}

uses
  stringres;

{ TEditSubTopicForm }

procedure TEditSubTopicForm.FormShow(Sender: TObject);
begin
  ActiveControl := TopicEdit;
end;

procedure TEditSubTopicForm.FormCreate(Sender: TObject);
begin
  i18nFixup;
end;

procedure TEditSubTopicForm.i18nFixup;
begin
  QosComboBox.Items.Text := sQosHint;
  QosComboBox.ItemIndex := 0;
end;

end.

