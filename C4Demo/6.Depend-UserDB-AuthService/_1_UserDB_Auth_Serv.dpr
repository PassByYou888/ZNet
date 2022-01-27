program _1_UserDB_Auth_Serv;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Net,
  Z.Net.DoubleTunnelIO.VirtualAuth,
  Z.Status,
  Z.Net.PhysicsIO,
  Z.Json,
  Z.Net.C4, Z.Net.C4_UserDB;

const
  // 调度服务器端口公网地址,可以是ipv4,ipv6,dns
  // 公共地址,不能给127.0.0.1这类
  Internet_DP_Addr_ = '127.0.0.1';
  // 调度服务器端口
  Internet_DP_Port_ = 8387;

  // 本地服务器公网地址
  Internet_LocalService_Addr_ = '127.0.0.1';
  Internet_LocalService_Port_ = 8387;

begin
  // 打开Log信息
  Z.Net.C4.C40_QuietMode := False;

  with Z.Net.C4.TC40_PhysicsService.Create(Internet_LocalService_Addr_, Internet_LocalService_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('DP|UserDB');
      StartService;
    end;
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'DP', nil);

  // 主循环
  while True do
    begin
      Z.Net.C4.C40Progress;
      TCompute.Sleep(1);
    end;

end.
