program DTC40_Alias_AdminTool;

{$mode objfpc}{$H+}

uses
  jemalloc4p, { https://github.com/PassByYou888/jemalloc4p }
  {$IFNDEF MSWINDOWS}
  cthreads,
  {$ENDIF MSWINDOWS}
  Interfaces, // this includes the LCL widgetset
  Forms, dtc40_alias_admintoolfrm, newaliasfrm
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDTC40_Alias_AdminToolForm, DTC40_Alias_AdminToolForm);
  Application.CreateForm(TNewAliasForm, NewAliasForm);
  Application.Run;
end.

