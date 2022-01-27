program ConvFile2Pascal;

uses
  Vcl.Forms,
  ConvFileFrm in 'ConvFileFrm.pas' {ConvFileForm},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.Title := 'Convert File to Pascal Code';
  Application.CreateForm(TConvFileForm, ConvFileForm);
  Application.Run;
end.
