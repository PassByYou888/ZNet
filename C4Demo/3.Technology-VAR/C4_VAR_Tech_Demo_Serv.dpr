program C4_VAR_Tech_Demo_Serv;

uses
  Vcl.Forms,
  C4_VAR_Tech_Demo_Serv_Frm in 'C4_VAR_Tech_Demo_Serv_Frm.pas' {C4_VAR_Tech_Demo_Serv_Form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TC4_VAR_Tech_Demo_Serv_Form, C4_VAR_Tech_Demo_Serv_Form);
  Application.Run;
end.
