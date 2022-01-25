program _1_XNAT_Mapping_Service;

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
  Z.Net.C4_XNAT;

const
  Internet_XNAT_Service_Addr_ = '127.0.0.1';
  Internet_XNAT_Service_Port_ = 8397;

begin
  RegisterC40('MY_XNAT_1', TC40_XNAT_Service_Tool, TC40_XNAT_Client_Tool);

  Z.Net.C4.C40_QuietMode := False;

  // �����Զ���XNAT���÷���ΪSaaS�����ṩ������͸֧��
  // ʹ��C4��XNAT���÷���ʱ��Ҫ��̫�ഩ͸��1-2���͹��ˣ������Ҫ�ഩ���Ͷ࿪�������÷���
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_XNAT_Service_Addr_, Internet_XNAT_Service_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('MY_XNAT_1@XNAT_Host:127.0.0.1,XNAT_Port:9911');
      StartService;
    end;

  while True do
    begin
      Z.Net.C4.C40Progress;
      TCompute.Sleep(1);
    end;

end.
