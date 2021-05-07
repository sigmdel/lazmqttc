unit verify;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    NoButton: TButton;
    YesButton: TButton;
    Label1: TLabel;
    Panel1: TPanel;
  private
  public
  end;

  { Displays aMsg in an confirmation dialog type window centered on the currently
    active form until the user presses the Yes or No button. Returns true
    if the Yes button was pressed.}
function ConfirmAction(const aMsg: string): boolean;

implementation

{$R *.lfm}

function ConfirmAction(const aMsg: string): boolean;
begin
  with TForm2.create(Screen.ActiveForm) do try
    Label1.Caption := aMsg;
    result := ShowModal = mrYes;
  finally
    free;
  end;
end;

end.

