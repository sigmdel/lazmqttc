unit report;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
  public
  end;

  { Displays aMsg in an information dialog type window centered on the currently
    active form until a timer expires or the user presses a Close button.
    The time delay is 3 seconds when isOk is true, 5 seconds otherwise}
procedure ShowPublishResult(const aMsg: string; isOk: boolean);

implementation

{$R *.lfm}

uses
  stringres;

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   close;
end;

procedure ShowPublishResult(const aMsg: string; isOk: boolean);
begin
  with TForm1.create(Screen.ActiveForm) do try
    if isOk then
      Caption := sMessageSent
    else
      Caption := sMessageNotSent;
    Label1.Caption := aMsg;
    if not isOk then
      Timer1.Interval := 5000;
    Timer1.Enabled := true;
    ShowModal;
  finally
    free;
  end;
end;

end.

