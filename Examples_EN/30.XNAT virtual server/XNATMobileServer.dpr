program XNATMobileServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  XNATMobileDeviceFrm in 'XNATMobileDeviceFrm.pas' {XNATMobileDeviceForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TXNATMobileDeviceForm, XNATMobileDeviceForm);
  Application.Run;
end.
