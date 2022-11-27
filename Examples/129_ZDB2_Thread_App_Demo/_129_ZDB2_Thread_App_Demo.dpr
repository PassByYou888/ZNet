program _129_ZDB2_Thread_App_Demo;

uses
  jemalloc4p, { https://github.com/PassByYou888/jemalloc4p }
  Vcl.Forms,
  _129_ZDB2_Thread_App_DemoFrm in '_129_ZDB2_Thread_App_DemoFrm.pas' {_129_ZDB2_Thread_App_DemoForm};

{$R *.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(T_129_ZDB2_Thread_App_DemoForm, _129_ZDB2_Thread_App_DemoForm);
  Application.Run;

end.
