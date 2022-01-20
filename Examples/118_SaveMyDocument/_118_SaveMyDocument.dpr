program _118_SaveMyDocument;

uses
  Vcl.Forms,
  _118_SaveMyDocumentFrm in '_118_SaveMyDocumentFrm.pas' {SaveMyDocumentForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSaveMyDocumentForm, SaveMyDocumentForm);
  Application.Run;
end.
