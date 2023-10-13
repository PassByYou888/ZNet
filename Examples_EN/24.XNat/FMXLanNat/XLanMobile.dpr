program XLanMobile;

uses
  System.StartUpCopy,
  FMX.Forms,
  MobileFrm in 'MobileFrm.pas' {MobileForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMobileForm, MobileForm);
  Application.Run;
end.
