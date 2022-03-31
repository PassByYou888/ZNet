program DTC40_TEKeyValue_AdminTool;

{$mode objfpc}{$H+}

uses
  jemalloc4p, { https://github.com/PassByYou888/jemalloc4p }
  {$IFNDEF MSWINDOWS}
  cthreads,
  {$ENDIF MSWINDOWS}
  Interfaces, // this includes the LCL widgetset
  Forms, dtc40_tekeyvalue_admintoolfrm
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDTC40_TEKeyValue_AdminTool_Form, DTC40_TEKeyValue_AdminTool_Form);
  Application.Run;
end.

