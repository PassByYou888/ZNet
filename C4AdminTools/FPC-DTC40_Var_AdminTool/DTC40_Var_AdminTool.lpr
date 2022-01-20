program DTC40_Var_AdminTool;

{$mode objfpc}{$H+}

uses
  jemalloc4p, { https://github.com/PassByYou888/jemalloc4p }
  {$IFNDEF MSWINDOWS}
  cthreads,
  {$ENDIF MSWINDOWS}
  Interfaces, // this includes the LCL widgetset
  Forms, DTC40_Var_AdminToolFrm, dtc40_var_admintoolnewnmfrm
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDTC40_Var_AdminToolForm, DTC40_Var_AdminToolForm);
  Application.CreateForm(TDTC40_Var_AdminToolNewNMForm,
    DTC40_Var_AdminToolNewNMForm);
  Application.Run;
end.

