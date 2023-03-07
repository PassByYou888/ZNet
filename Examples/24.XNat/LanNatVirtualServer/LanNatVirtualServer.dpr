program LanNatVirtualServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  VirtualServFrm in 'VirtualServFrm.pas' {VirtualServForm};

{$R *.res}


begin
  System.ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TVirtualServForm, VirtualServForm);
  Application.Run;
end.
