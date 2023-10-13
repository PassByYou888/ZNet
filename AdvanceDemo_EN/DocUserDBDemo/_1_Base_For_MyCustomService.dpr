program _1_Base_For_MyCustomService;

uses
  Vcl.Forms,
  MyCustomService in 'MyCustomService.pas',
  C40AppTempletFrm in '..\..\C4AdminTools\Delphi-C4AppTemplet\C40AppTempletFrm.pas' {C40AppTempletForm};

{$R *.res}

begin
  InitC40AppParamFromSystemCmdLine;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TC40AppTempletForm, C40AppTempletForm);
  Application.Run;
end.
