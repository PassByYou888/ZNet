program C40AppTemplet;

{$mode objfpc}{$H+}

uses
  jemalloc4p, { https://github.com/PassByYou888/jemalloc4p }
  {$IFNDEF MSWINDOWS}
  cthreads,
  {$ENDIF MSWINDOWS}
  Interfaces, // this includes the LCL widgetset
  Forms, c40apptempletfrm
  { you can add units after this };

{$R *.res}

begin
  InitC40AppParamFromSystemCmdLine;
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TC40AppTempletForm, C40AppTempletForm);
  Application.Run;
end.

