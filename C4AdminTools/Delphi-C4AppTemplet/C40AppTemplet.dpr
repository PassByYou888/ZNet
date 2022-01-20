program C40AppTemplet;

uses
  Vcl.Forms,
  System.SysUtils,
  C40AppTempletFrm in 'C40AppTempletFrm.pas' {C40AppTempletForm};

{$R *.res}


begin
  InitC40AppParamFromSystemCmdLine;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TC40AppTempletForm, C40AppTempletForm);
  Application.Run;
end.
