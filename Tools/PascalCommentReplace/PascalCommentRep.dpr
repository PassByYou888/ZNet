program PascalCommentRep;

uses
  Vcl.Forms,
  PascalCommentRepFrm in 'PascalCommentRepFrm.pas' {PascalCommentRepForm},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.CreateForm(TPascalCommentRepForm, PascalCommentRepForm);
  Application.Run;
end.
