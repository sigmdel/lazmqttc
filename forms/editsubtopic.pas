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
    procedure FormShow(Sender: TObject);
  private

  public

  end;

//var
  //EditSubTopicForm: TEditSubTopicForm;

implementation

{$R *.lfm}

{ TEditSubTopicForm }


procedure TEditSubTopicForm.FormShow(Sender: TObject);
begin
  ActiveControl := TopicEdit;
end;

end.

