program DTC40_UserDB_AdminTool;

{$R *.dres}

uses
  Vcl.Forms,
  DTC40_UserDB_AdminToolFrm in 'DTC40_UserDB_AdminToolFrm.pas' {DTC40_UserDB_AdminToolForm},
  DTC40_UserDB_AdminLargeScaleRegFrm in 'DTC40_UserDB_AdminLargeScaleRegFrm.pas' {DTC40_UserDB_AdminLargeScaleRegForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDTC40_UserDB_AdminToolForm, DTC40_UserDB_AdminToolForm);
  Application.CreateForm(TDTC40_UserDB_AdminLargeScaleRegForm, DTC40_UserDB_AdminLargeScaleRegForm);
  Application.Run;
end.
