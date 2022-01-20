program PascalRewriteModel;

{$R *.dres}

uses
  Vcl.Forms,
  PascalRewriteModelFrm in 'PascalRewriteModelFrm.pas' {PascalRewriteModelForm},
  NewUnitNameFrm in 'NewUnitNameFrm.pas' {NewUnitNameForm},
  NewSymbolDefFrm in 'NewSymbolDefFrm.pas' {NewSymbolDefForm},
  Vcl.Themes,
  Vcl.Styles,
  TextRepToolFrm in 'TextRepToolFrm.pas' {TextRepToolForm},
  PascalRewrite_TextEdtFrm in 'PascalRewrite_TextEdtFrm.pas' {PascalRewrite_TextEdtForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := False;
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.Title := 'Rename Pascal Unit tool';
  Application.CreateForm(TPascalRewriteModelForm, PascalRewriteModelForm);
  Application.CreateForm(TNewUnitNameForm, NewUnitNameForm);
  Application.CreateForm(TNewSymbolDefForm, NewSymbolDefForm);
  Application.CreateForm(TTextRepToolForm, TextRepToolForm);
  Application.CreateForm(TPascalRewrite_TextEdtForm, PascalRewrite_TextEdtForm);
  Application.Run;
end.
