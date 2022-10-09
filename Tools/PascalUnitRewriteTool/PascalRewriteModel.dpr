program PascalRewriteModel;

{$R *.dres}

uses
  jemalloc4p,
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Forms,
  System.SysUtils,
  PascalRewriteModelFrm in 'PascalRewriteModelFrm.pas' {PascalRewriteModelForm},
  NewUnitNameFrm in 'NewUnitNameFrm.pas' {NewUnitNameForm},
  NewSymbolDefFrm in 'NewSymbolDefFrm.pas' {NewSymbolDefForm},
  TextRepToolFrm in 'TextRepToolFrm.pas' {TextRepToolForm},
  PascalRewrite_TextEdtFrm in 'PascalRewrite_TextEdtFrm.pas' {PascalRewrite_TextEdtForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := False;
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.Title := 'Pascal Rewrite Model';
  Application.CreateForm(TPascalRewriteModelForm, PascalRewriteModelForm);
  Application.CreateForm(TNewUnitNameForm, NewUnitNameForm);
  Application.CreateForm(TNewSymbolDefForm, NewSymbolDefForm);
  Application.CreateForm(TTextRepToolForm, TextRepToolForm);
  Application.CreateForm(TPascalRewrite_TextEdtForm, PascalRewrite_TextEdtForm);
  if ParamCount = 1 then
      PascalRewriteModelForm.OpenProj_File(ParamStr(1));
  Application.Run;
end.
