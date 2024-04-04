program _160_C4_Console_Help_Tool_For_UI;

uses
  Vcl.Forms,
  _160_C4_Console_Help_Tool_For_UI_Frm in '_160_C4_Console_Help_Tool_For_UI_Frm.pas' {_160_C4_Console_Help_Tool_For_UI_Form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(T_160_C4_Console_Help_Tool_For_UI_Form, _160_C4_Console_Help_Tool_For_UI_Form);
  Application.Run;
end.
