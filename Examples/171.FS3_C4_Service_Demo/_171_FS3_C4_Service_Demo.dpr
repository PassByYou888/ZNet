program _171_FS3_C4_Service_Demo;

uses
  Vcl.Forms,
  _171_FS3_C4_Service_Demo_Frm in '_171_FS3_C4_Service_Demo_Frm.pas' {TFS3_C4_Service_Demo_Form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TTFS3_C4_Service_Demo_Form, TFS3_C4_Service_Demo_Form);
  Application.Run;
end.
