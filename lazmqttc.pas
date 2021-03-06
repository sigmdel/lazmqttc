program lazmqttc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads} // defined in Project Options
  cthreads,
  {$ENDIF}{$ENDIF}
  ctypes, // needed by mosquitto
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, main, report, brokerunit, brokeredit, verify, optionsedit,
  stringres, pwd, optionsunit, startup;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='MQTT Client';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

