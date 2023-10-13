program _1_DispatchSeed;

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
  Z.Net.C4_FS,
  Z.Net.C4_UserDB,
  Z.Net.C4_Var,
  Z.Status,
  Z.Net.C4_Console_APP;

const
  {  The public network address of the dispatching server port, which can be IPv4, IPv6 or DNS  }
  {  Public address, cannot be given to a type of 127.0.0.1  }
  Internet_DP_Addr_ = '127.0.0.1';
  {  Scheduling Server Port  }
  Internet_DP_Port_ = 8387;

begin
  {  Open Log Information  }
  Z.Net.C4.C40_QuietMode := False;

  {  Create scheduling service  }
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_DP_Addr_, Internet_DP_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('dp');
      StartService;
    end;
  {  Connect to the dispatch terminal  }
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'dp', nil);

  {  Main loop  }
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
