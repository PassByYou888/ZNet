program _2_XNAT_Mapping_Client_DP;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.MemoryStream,
  Z.Notify,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_XNAT,
  Z.Net.XNAT.Client, Z.Net.XNAT.MappingOnVirutalService, Z.Net.XNAT.Service, Z.Net.XNAT.Physics,
  Z.Net.C4_Console_APP;

const
  Internet_XNAT_Service_Addr_ = '127.0.0.1';
  Internet_XNAT_Service_Port_ = 8397;
  Internet_XNAT_Service_Port_DP_ = 8888;

begin
  RegisterC40('MY_XNAT_1', TC40_XNAT_Service_Tool, TC40_XNAT_Client_Tool);

  Z.Net.C4.C40_QuietMode := False;
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_XNAT_Service_Addr_, Internet_XNAT_Service_Port_, 'MY_XNAT_1', nil);

  {  Wait for the XNAT configuration service to be ready, and then use remote address mapping to become local TXNAT_MappingOnVirutalService  }
  {  TXNAT_MappingOnVirutalService is consistent with regular server usage and does not require XNAT to repeat linking  }
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('MY_XNAT_1', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    var
      XNAT_Cli: TC40_XNAT_Client_Tool;
    begin
      if length(States_) = 0 then
          exit;
      {  Obtaining TDTC40 from C4 network_XNAT_Client_Tool  }
      XNAT_Cli := TC40_XNAT_Client_Tool(States_[0].Client_);
      {  Add Remote Configuration  }
      XNAT_Cli.Add_XNAT_Mapping(True, Internet_XNAT_Service_Port_DP_, 'test', 5000);
      {  Open_XNAT_Tunnel will restart XNAT in the remote XNAT configuration service, and all XNAT systems that have established connections will be disconnected. When the XNAT service restarts, XNAT will automatically shake hands again  }
      {  When configuring services using C4 XNAT, do not use too many penetrations. 1-2 penetrations are sufficient. If more penetrations are needed, open several more configuration services  }
      XNAT_Cli.Open_XNAT_Tunnel;
      {  Create TXNAT_MappingOnVirutalService  }
      XNAT_Cli.Build_Physics_ServiceP('test', 1000,
        procedure(Sender: TC40_XNAT_Client_Tool; Service: TXNAT_MappingOnVirutalService)
        begin
          if Service = nil then
              exit;
          {  Using TXNAT_MappingOnVirutalService establishes penetration remotely and maps to the local  }
          with Z.Net.C4.TC40_PhysicsService.Create(Internet_XNAT_Service_Addr_, Internet_XNAT_Service_Port_DP_, Service) do
            begin
              BuildDependNetwork('DP');
              StartService;
            end;
          {  Connect to the dispatch terminal  }
          Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_XNAT_Service_Addr_, Internet_XNAT_Service_Port_DP_, 'DP', nil);
        end);
    end);

  {  Main loop  }
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;
end.
