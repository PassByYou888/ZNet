program UserClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  UserClientFrm in 'UserClientFrm.pas' {UserClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TUserClientForm, UserClientForm);
  Application.Run;
end.
