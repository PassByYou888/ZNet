program ADRestoreServer;
uses
  Vcl.Forms,
  ADRestoreServFrm in 'ADRestoreServFrm.pas' {AuthDoubleServerForm};

{$R *.res}
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAuthDoubleServerForm, AuthDoubleServerForm);
  Application.Run;
end.
