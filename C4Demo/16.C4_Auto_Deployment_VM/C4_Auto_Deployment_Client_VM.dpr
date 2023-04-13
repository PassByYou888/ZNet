program C4_Auto_Deployment_Client_VM;

uses
  Vcl.Forms,
  C4_Auto_Deployment_User_LoginFrm in 'C4_Auto_Deployment_User_LoginFrm.pas' {user_login_Form},
  C4_Auto_Deployment_IMP_VM_Cli in 'C4_Auto_Deployment_IMP_VM_Cli.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tuser_login_Form, user_login_Form);
  Application.Run;
end.
