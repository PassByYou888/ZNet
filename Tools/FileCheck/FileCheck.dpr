program FileCheck;

uses
  Vcl.Forms,
  FileCheckFrm in 'FileCheckFrm.pas' {FileCheckForm},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.CreateForm(TFileCheckForm, FileCheckForm);
  Application.Run;
end.
