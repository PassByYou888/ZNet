program _1_UserDB_serv;

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
  Z.Net.C4_UserDB,
  Z.Status,
  Z.Net.C4_Console_APP;

const
  {  The public network address of the dispatching server port, which can be IPv4, IPv6 or DNS  }
  {  Public address, cannot be given to a type of 127.0.0.1  }
  Internet_DP_Addr_ = '127.0.0.1';
  {  Scheduling Server Port  }
  Internet_DP_Port_ = 8387;

function GetMyUserDB_Service: TC40_UserDB_Service;
var
  arry: TC40_Custom_Service_Array;
begin
  arry := C40_ServicePool.GetFromServiceTyp('userDB');
  if length(arry) > 0 then
      Result := TC40_UserDB_Service(arry[0] as TC40_UserDB_Service)
  else
      Result := nil;
end;

begin
  {  Open Log Information  }
  Z.Net.C4.C40_QuietMode := False;

  {  Create scheduling service and user identity database service  }
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_DP_Addr_, Internet_DP_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('DP|UserDB');
      StartService;
    end;

  {  Connect to the dispatch terminal  }
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'DP', nil);

  {  Main loop  }
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
