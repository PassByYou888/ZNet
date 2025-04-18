﻿program _2_RandNum_Client;

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
  Z.ListEngine,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_RandSeed,
  Z.Net.C4_Console_APP;

const
  {  The public network address of the dispatching server port, which can be IPv4, IPv6 or DNS  }
  {  Public address, cannot be given to a type of 127.0.0.1  }
  Internet_DP_Addr_ = '127.0.0.1';
  {  Scheduling Server Port  }
  Internet_DP_Port_ = 8387;

function GetRandSeed_Client: TC40_RandSeed_Client;
begin
  Result := TC40_RandSeed_Client(C40_ClientPool.FindConnectedServiceTyp('RandSeed'));
end;

begin
  Z.Net.C4.C40_QuietMode := False;
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'DP|RandSeed', nil);

  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('RandSeed', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    var
      i: Integer;
      L: TUInt32List;
    begin
      L := TUInt32List.Create;
      for i := 0 to 100 do
          GetRandSeed_Client.MakeSeed_P('my_group', 1000, 9999,
          procedure(sender: TC40_RandSeed_Client; Seed_: UInt32)
          begin
            L.Add(Seed_);
          end);

      GetRandSeed_Client.DTNoAuthClient.SendTunnel.IO_IDLE_TraceP(nil, procedure(data: TCore_Object)
        var
          i: Integer;
        begin
          for i := 0 to L.Count - 1 do
              GetRandSeed_Client.RemoveSeed('my_group', L[i]);
          L.Free;
        end);
    end);

  {  Main loop  }
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
