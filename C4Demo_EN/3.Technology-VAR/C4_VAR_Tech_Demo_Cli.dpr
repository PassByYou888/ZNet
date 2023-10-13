program C4_VAR_Tech_Demo_Cli;

uses
  Vcl.Forms,
  C4_VAR_Tech_Demo_Cli_Frm in 'C4_VAR_Tech_Demo_Cli_Frm.pas' {C4_VAR_Tech_Demo_Cli_Form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TC4_VAR_Tech_Demo_Cli_Form, C4_VAR_Tech_Demo_Cli_Form);
  Application.Run;
end.
