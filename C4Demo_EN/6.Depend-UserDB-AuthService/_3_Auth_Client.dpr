program _3_Auth_Client;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.Notify,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_Console_APP;

const
  {  The public network address of the dispatching server port, which can be IPv4, IPv6 or DNS  }
  {  Public address, cannot be given to a type of 127.0.0.1  }
  Internet_DP_Addr_ = '127.0.0.1';
  {  Scheduling Server Port  }
  Internet_DP_Port_ = 8387;

function GetVirtualAuth_Client: TC40_Base_VirtualAuth_Client;
begin
  Result := TC40_Base_VirtualAuth_Client(C40_ClientPool.ExistsConnectedServiceTyp('MyVA'));
end;

procedure SearchAndBuildVirtualAuth; forward;

procedure Do_QueryInfo(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
var
  arry: TC40_Info_Array;
begin
  {  SearchService will search for the target service and sort the payload information  }
  arry := L.SearchService('MyVA');

  if length(arry) > 0 then
    begin
      {  Virtual Auth's verification mechanism: After entering the network, no dual channel is created, but the verification mechanism is waiting to be executed. Once the verification is passed, a dual channel link is established and the automatic network is started  }
      {  When the automatic network is started, the disconnection and reconnection will automatically log in through authentication  }
      Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(arry[0], 'MyVA', nil);
    end
  else
    begin
      SysPost.PostExecuteC_NP(5.0, SearchAndBuildVirtualAuth);
    end;
end;

procedure SearchAndBuildVirtualAuth;
begin
  with Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_) do
      QueryInfoC(Do_QueryInfo); {  QueryInfo will return all address information from the cloud  }
end;

begin
  {  In one sentence, summarize the automatic authentication network. After passing the first authentication, start the automatic network  }

  RegisterC40('MyVA', TC40_Base_VirtualAuth_Service, TC40_Base_VirtualAuth_Client);
  Z.Net.C4.C40_QuietMode := False;

  {  Client Select VM  }
  SearchAndBuildVirtualAuth;

  {  WaitConnectedDone can simultaneously check if multiple dependent services are ready  }
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('MyVA', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    begin
      if not GetVirtualAuth_Client.LoginIsSuccessed then
        begin
          {  Registeruserandlogin is a switch, which is false by default. When it is turned on, the connect operation will automatically register new users. When the registration is successful, the authentication login will be turned on and the automatic network will be started  }
          {  When the registration fails, the system will automatically log in for the first time. If the login is successful, the automatic network will be started. If the login fails, the system will return and the automatic network will not be started  }
          {  Note: If using the verification mode to develop c4, human attendance is required to pass the verification before connecting to the server  }
          GetVirtualAuth_Client.Client.RegisterUserAndLogin := True;
          GetVirtualAuth_Client.Client.Connect_P('User_Test', '123456', procedure(const State: Boolean)
            begin
              if State then
                  DoStatus('Successfully registered or logged in')
              else
                  DoStatus('Registration or login failed');
            end);
        end;
    end);

  {  Main loop  }
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
