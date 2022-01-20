program DTC40_Var_AdminTool;

uses
  Vcl.Forms,
  DTC40_Var_AdminToolFrm in 'DTC40_Var_AdminToolFrm.pas' {DTC40_Var_AdminToolForm},
  DTC40_Var_AdminToolNewNMFrm in 'DTC40_Var_AdminToolNewNMFrm.pas' {DTC40_Var_AdminToolNewNMForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDTC40_Var_AdminToolForm, DTC40_Var_AdminToolForm);
  Application.CreateForm(TDTC40_Var_AdminToolNewNMForm, DTC40_Var_AdminToolNewNMForm);
  Application.Run;
end.
