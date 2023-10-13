program _1_FS2_Service;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Windows,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_FS2,
  Z.Status,
  Z.Net.C4_Console_APP;

const
  {  The public network address of the dispatching server port, which can be IPv4, IPv6 or DNS  }
  {  Public address, cannot be given to a type of 127.0.0.1  }
  Internet_DP_Addr_ = '127.0.0.1';
  {  Scheduling Server Port  }
  Internet_DP_Port_ = 8387;

function GetMyUserDB_Service: TC40_FS2_Service;
var
  arry: TC40_Custom_Service_Array;
begin
  arry := C40_ServicePool.GetFromServiceTyp('FS2');
  if length(arry) > 0 then
      Result := arry[0] as TC40_FS2_Service
  else
      Result := nil;
end;

begin
  {  Open Log Information  }
  Z.Net.C4.C40_QuietMode := False;

  {  Create scheduling services and file system services  }
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_DP_Addr_, Internet_DP_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      {  FS@SafeCheckTime =5000 is the construction parameter for the fs server, and SafeCheckTime represents the time interval between security checks and IO data writing to the disk  }
      BuildDependNetwork('DP|FS2@SafeCheckTime=5000');
      StartService;
    end;

  {  Connect to the dispatch terminal  }
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'DP', nil);

  {  Main loop  }
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
