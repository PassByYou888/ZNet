program XNATUserClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  XNATUserClientFrm in 'XNATUserClientFrm.pas' {UserClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TUserClientForm, UserClientForm);
  Application.Run;
end.
