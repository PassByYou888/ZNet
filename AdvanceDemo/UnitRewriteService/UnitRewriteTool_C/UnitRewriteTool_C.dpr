program UnitRewriteTool_C;

uses
  Vcl.Forms,
  UnitRewriteTool_C_Frm in 'UnitRewriteTool_C_Frm.pas' {UnitRewriteTool_C_Form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TUnitRewriteTool_C_Form, UnitRewriteTool_C_Form);
  Application.Run;
end.
