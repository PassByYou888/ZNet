program DTC40_FS_AdminTool;

uses
  Vcl.Forms,
  DTC40_FS_AdminToolFrm in 'DTC40_FS_AdminToolFrm.pas' {DTC40_FS_AdminToolForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDTC40_FS_AdminToolForm, DTC40_FS_AdminToolForm);
  Application.Run;
end.
