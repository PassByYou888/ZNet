program DTC40_UserDB_AdminTool;

{$mode objfpc}{$H+}

uses
  jemalloc4p, { https://github.com/PassByYou888/jemalloc4p }
  {$IFNDEF MSWINDOWS}
  cthreads,
  {$ENDIF MSWINDOWS}
  Interfaces, // this includes the LCL widgetset
  Forms, DTC40_UserDB_AdminTool_frm
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDTC40_UserDB_AdminToolForm, DTC40_UserDB_AdminToolForm);
  Application.Run;
end.

