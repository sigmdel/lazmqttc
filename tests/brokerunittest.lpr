program brokerunittest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, brokertest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

