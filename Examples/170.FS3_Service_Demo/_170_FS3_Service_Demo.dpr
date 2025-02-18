program _170_FS3_Service_Demo;

uses
  Vcl.Forms,
  _170_FS3_Service_Demo_Frm in '_170_FS3_Service_Demo_Frm.pas' {_170_FS3_Service_Demo_Form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(T_170_FS3_Service_Demo_Form, _170_FS3_Service_Demo_Form);
  Application.Run;
end.
