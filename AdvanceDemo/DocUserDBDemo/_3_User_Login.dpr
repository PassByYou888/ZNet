program _3_User_Login;

uses
  Vcl.Forms,
  _3_User_LoginFrm in '_3_User_LoginFrm.pas' {user_login_Form},
  MyCustomService in 'MyCustomService.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tuser_login_Form, user_login_Form);
  Application.Run;
end.
