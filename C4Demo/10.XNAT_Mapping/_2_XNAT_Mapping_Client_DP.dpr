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
  Z.Net.XNAT.Client, Z.Net.XNAT.MappingOnVirutalService, Z.Net.XNAT.Service, Z.Net.XNAT.Physics;

const
  Internet_XNAT_Service_Addr_ = '127.0.0.1';
  Internet_XNAT_Service_Port_ = 8397;
  Internet_XNAT_Service_Port_DP_ = 8888;

begin
  RegisterC40('MY_XNAT_1', TC40_XNAT_Service_Tool, TC40_XNAT_Client_Tool);

  Z.Net.C4.C40_QuietMode := False;
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_XNAT_Service_Addr_, Internet_XNAT_Service_Port_, 'MY_XNAT_1', nil);

  // 等XNAT配置服务就绪，然后使用远程地址映射成为本地 TXNAT_MappingOnVirutalService
  // TXNAT_MappingOnVirutalService与常规Server用法一致，不需要XNAT重复做链接
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('MY_XNAT_1', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    var
      XNAT_Cli: TC40_XNAT_Client_Tool;
    begin
      if length(States_) = 0 then
          exit;
      // 从C4网络获取 TDTC40_XNAT_Client_Tool
      XNAT_Cli := TC40_XNAT_Client_Tool(States_[0].Client_);
      // 添加远程配置
      XNAT_Cli.Add_XNAT_Mapping(True, Internet_XNAT_Service_Port_DP_, 'test', 5000);
      // Open_XNAT_Tunnel会在远程XNAT配置服务重启XNAT，已建立连接的XNAT系统会全部断线，当XNAT服务重启完成后，XNAT则会自动重新握手
      // 使用C4的XNAT配置服务时不要挂太多穿透，1-2个就够了，如果需要多穿，就多开几个配置服务
      XNAT_Cli.Open_XNAT_Tunnel;
      // 创建 TXNAT_MappingOnVirutalService
      XNAT_Cli.Build_Physics_ServiceP('test', 1000,
        procedure(Sender: TC40_XNAT_Client_Tool; Service: TXNAT_MappingOnVirutalService)
        begin
          if Service = nil then
              exit;
          // 使用TXNAT_MappingOnVirutalService在远程建立穿透，映射到本地
          with Z.Net.C4.TC40_PhysicsService.Create(Internet_XNAT_Service_Addr_, Internet_XNAT_Service_Port_DP_, Service) do
            begin
              BuildDependNetwork('DP');
              StartService;
            end;
          // 接通调度端
          Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_XNAT_Service_Addr_, Internet_XNAT_Service_Port_DP_, 'DP', nil);
        end);
    end);

  while True do
    begin
      Z.Net.C4.C40Progress;
      TCompute.Sleep(1);
    end;

end.
