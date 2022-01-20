program _117_ZDB2_Json_DB;

uses
  Vcl.Forms,
  _117_ZDB2_Json_DB_Frm in '_117_ZDB2_Json_DB_Frm.pas' {TZDB2_Json_DB_Frm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TTZDB2_Json_DB_Frm, TZDB2_Json_DB_Frm);
  Application.Run;
end.
