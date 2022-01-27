program DTC40_Alias_AdminTool;

uses
  Vcl.Forms,
  DTC40_Alias_AdminTool_Frm in 'DTC40_Alias_AdminTool_Frm.pas' {DTC40_Alias_AdminToolForm},
  NewAliasFrm in 'NewAliasFrm.pas' {NewAliasForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDTC40_Alias_AdminToolForm, DTC40_Alias_AdminToolForm);
  Application.CreateForm(TNewAliasForm, NewAliasForm);
  Application.Run;
end.
