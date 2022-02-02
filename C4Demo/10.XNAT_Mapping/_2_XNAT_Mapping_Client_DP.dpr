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

  // ��XNAT���÷��������Ȼ��ʹ��Զ�̵�ַӳ���Ϊ���� TXNAT_MappingOnVirutalService
  // TXNAT_MappingOnVirutalService�볣��Server�÷�һ�£�����ҪXNAT�ظ�������
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('MY_XNAT_1', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    var
      XNAT_Cli: TC40_XNAT_Client_Tool;
    begin
      if length(States_) = 0 then
          exit;
      // ��C4�����ȡ TDTC40_XNAT_Client_Tool
      XNAT_Cli := TC40_XNAT_Client_Tool(States_[0].Client_);
      // ���Զ������
      XNAT_Cli.Add_XNAT_Mapping(True, Internet_XNAT_Service_Port_DP_, 'test', 5000);
      // Open_XNAT_Tunnel����Զ��XNAT���÷�������XNAT���ѽ������ӵ�XNATϵͳ��ȫ�����ߣ���XNAT����������ɺ�XNAT����Զ���������
      // ʹ��C4��XNAT���÷���ʱ��Ҫ��̫�ഩ͸��1-2���͹��ˣ������Ҫ�ഩ���Ͷ࿪�������÷���
      XNAT_Cli.Open_XNAT_Tunnel;
      // ���� TXNAT_MappingOnVirutalService
      XNAT_Cli.Build_Physics_ServiceP('test', 1000,
        procedure(Sender: TC40_XNAT_Client_Tool; Service: TXNAT_MappingOnVirutalService)
        begin
          if Service = nil then
              exit;
          // ʹ��TXNAT_MappingOnVirutalService��Զ�̽�����͸��ӳ�䵽����
          with Z.Net.C4.TC40_PhysicsService.Create(Internet_XNAT_Service_Addr_, Internet_XNAT_Service_Port_DP_, Service) do
            begin
              BuildDependNetwork('DP');
              StartService;
            end;
          // ��ͨ���ȶ�
          Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_XNAT_Service_Addr_, Internet_XNAT_Service_Port_DP_, 'DP', nil);
        end);
    end);

  while True do
    begin
      Z.Net.C4.C40Progress;
      TCompute.Sleep(1);
    end;

end.
