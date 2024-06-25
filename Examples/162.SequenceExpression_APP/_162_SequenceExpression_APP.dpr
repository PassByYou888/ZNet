program _162_SequenceExpression_APP;

uses
  Vcl.Forms,
  _162_SequenceExpression_APP_Frm in '_162_SequenceExpression_APP_Frm.pas' {_162_SequenceExpression_APP_Form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(T_162_SequenceExpression_APP_Form, _162_SequenceExpression_APP_Form);
  Application.Run;
end.
