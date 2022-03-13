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

var
  exit_signal: Boolean;

procedure Do_Check_On_Exit;
var
  n: string;
  cH: TC40_Console_Help;
begin
  cH := TC40_Console_Help.Create;
  repeat
    TCompute.Sleep(100);
    Readln(n);
    cH.Run_HelpCmd(n);
  until cH.IsExit;
  disposeObject(cH);
  exit_signal := True;
end;

const
  // 调度服务器端口公网地址,可以是ipv4,ipv6,dns
  // 公共地址,不能给127.0.0.1这类
  Internet_DP_Addr_ = '127.0.0.1';
  // 调度服务器端口
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
  // SearchService会搜索目标服务，并对负载信息排序
  arry := L.SearchService('MyVA');

  if length(arry) > 0 then
    begin
      // VirtualAuth的验证机制：进入网络以后不创建双通道，而是等待执行验证机制，一旦通过验证，则建立双通道链接并启动自动网络
      // 当启动自动网络后，断线重连将会自动化通过身份验证登录
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
      QueryInfoC(Do_QueryInfo); // QueryInfo会返回云端的全部地址信息
end;

begin
  // 一句话总结自动化验证网络，通过首次身份验证后，开启自动化网络

  RegisterC40('MyVA', TC40_Base_VirtualAuth_Service, TC40_Base_VirtualAuth_Client);
  Z.Net.C4.C40_QuietMode := False;

  // 客户端选择VM
  SearchAndBuildVirtualAuth;

  // WaitConnectedDone可以同时检查多个依赖服务是否就绪
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('MyVA', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    begin
      if not GetVirtualAuth_Client.LoginIsSuccessed then
        begin
          // RegisterUserAndLogin是个开关，默认为false，打开以后，connect操作会自动化注册新用户，注册成功时会开启验证登录，并启动自动网络
          // 当注册失败时，系统会自动首次登录，如果登录成功，启动自动网络，登录失败，返回，不会启动自动网络
          // 注意：如果使用验证模式开发c4，接通服务器前，需要人为值守通过验证
          GetVirtualAuth_Client.Client.RegisterUserAndLogin := True;
          GetVirtualAuth_Client.Client.Connect_P('User_Test', '123456', procedure(const State: Boolean)
            begin
              if State then
                  DoStatus('注册或登录成功')
              else
                  DoStatus('注册或登录失败.');
            end);
        end;
    end);

  // 主循环
  StatusThreadID := False;
  exit_signal := False;
  TCompute.RunC_NP(@Do_Check_On_Exit);
  while not exit_signal do
      Z.Net.C4.C40Progress;

  Z.Net.C4.C40Clean;

end.
