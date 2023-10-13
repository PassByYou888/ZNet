program _3_UserDB_Service;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Notify,
  Z.Status,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_FS,
  Z.Net.C4_UserDB,
  Z.Net.C4_Var,
  Z.Net.C4_Console_APP;

const
  {  The public network address of the dispatching server port, which can be IPv4, IPv6 or DNS  }
  {  Public address, cannot be given to a type of 127.0.0.1  }
  Internet_DP_Addr_ = '127.0.0.1';
  {  Scheduling Server Port  }
  Internet_DP_Port_ = 8387;

  {  Local server public network address  }
  Internet_LocalService_Addr_ = '127.0.0.1';
  Internet_LocalService_Port_ = 8385;

var
  FS: TC40_FS_Client = nil;

type
  {  C4 networks are diffuse, where one link crawls out many associated links and uses interfaces to listen  }
  TMonitorMySAAS = class(TCore_InterfacedObject, IC40_PhysicsTunnel_Event)
    procedure C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    procedure C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
  end;

procedure TMonitorMySAAS.C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
begin
  {  Create physical link  }
end;

procedure TMonitorMySAAS.C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
begin
  {  Physical link break  }
  if Sender.DependNetworkClientPool.IndexOf(FS) >= 0 then
      FS := nil;
end;

procedure TMonitorMySAAS.C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
  {  Create a peer-to-peer VM tunnel  }
end;

procedure TMonitorMySAAS.C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
  {  P2P VM tunnel handshake completed  }
  if Custom_Client_ is TC40_FS_Client then
    begin
      FS := Custom_Client_ as TC40_FS_Client;
      DoStatus('Found file support service:%s', [Custom_Client_.ClientInfo.ServiceTyp.Text]);
    end;
end;

begin
  {  Open Log Information  }
  Z.Net.C4.C40_QuietMode := False;

  {  Create DP and user database services  }
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_LocalService_Addr_, Internet_LocalService_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('dp|UserDB');
      StartService;
    end;

  {  Connect the dispatcher and file service  }
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'dp|FS', TMonitorMySAAS.Create);
  FS := nil;

  {  Main loop  }
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
