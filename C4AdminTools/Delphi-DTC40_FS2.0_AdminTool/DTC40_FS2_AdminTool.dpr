program DTC40_FS2_AdminTool;

uses
  Vcl.Forms,
  DTC40_FS2_AdminToolFrm in 'DTC40_FS2_AdminToolFrm.pas' {DTC40_FS2_AdminToolForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDTC40_FS2_AdminToolForm, DTC40_FS2_AdminToolForm);
  Application.Run;
end.
