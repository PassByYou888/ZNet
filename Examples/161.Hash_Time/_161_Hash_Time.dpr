program _161_Hash_Time;

uses
  FastMM5,
  Vcl.Forms,
  _161_Hash_Time_Frm in '_161_Hash_Time_Frm.pas' {_161_Hash_Time_Form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(T_161_Hash_Time_Form, _161_Hash_Time_Form);
  Application.Run;
end.
