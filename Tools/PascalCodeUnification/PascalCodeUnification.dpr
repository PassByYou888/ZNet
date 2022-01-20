program PascalCodeUnification;

uses
  Vcl.Forms,
  PascalCodeUnificationFrm in 'PascalCodeUnificationFrm.pas' {PascalCodeUnificationForm},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.CreateForm(TPascalCodeUnificationForm, PascalCodeUnificationForm);
  Application.Run;
end.
