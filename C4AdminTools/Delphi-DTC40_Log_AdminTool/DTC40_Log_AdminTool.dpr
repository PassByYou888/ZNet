program DTC40_Log_AdminTool;

uses
  Vcl.Forms,
  DTC40_Log_AdminToolFrm in 'DTC40_Log_AdminToolFrm.pas' {DTC40_Log_AdminToolForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDTC40_Log_AdminToolForm, DTC40_Log_AdminToolForm);
  Application.Run;
end.
