program _1_XNAT_Mapping_Service;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_XNAT,
  Z.Status,
  Z.Net.C4_Console_APP;

const
  Internet_XNAT_Service_Addr_ = '127.0.0.1';
  Internet_XNAT_Service_Port_ = 8397;

begin
  RegisterC40('MY_XNAT_1', TC40_XNAT_Service_Tool, TC40_XNAT_Client_Tool);

  Z.Net.C4.C40_QuietMode := False;

  {  Create an automated XNAT configuration service to provide intranet penetration support for SaaS networks  }
  {  When configuring services using C4 XNAT, do not use too many penetrations. 1-2 penetrations are sufficient. If more penetrations are needed, open several more configuration services  }
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_XNAT_Service_Addr_, Internet_XNAT_Service_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('MY_XNAT_1@XNAT_Host:127.0.0.1,XNAT_Port:9911');
      StartService;
    end;

  {  Main loop  }
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
