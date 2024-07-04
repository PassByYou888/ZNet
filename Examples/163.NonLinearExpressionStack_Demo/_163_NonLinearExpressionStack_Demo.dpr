program _163_NonLinearExpressionStack_Demo;

uses
  Vcl.Forms,
  _163_NonLinearExpressionStack_Demo_Frm in '_163_NonLinearExpressionStack_Demo_Frm.pas' {_163_NonLinearExpressionStack_Demo_Form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(T_163_NonLinearExpressionStack_Demo_Form, _163_NonLinearExpressionStack_Demo_Form);
  Application.Run;
end.
