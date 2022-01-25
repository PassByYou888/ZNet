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
  Z.Net.C4_UserDB;

const
  // ���ȷ������˿ڹ�����ַ,������ipv4,ipv6,dns
  // ������ַ,���ܸ�127.0.0.1����
  Internet_DP_Addr_ = '127.0.0.1';
  // ���ȷ������˿�
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
  // ��Log��Ϣ
  Z.Net.C4.C40_QuietMode := False;

  // �������ȷ�����û�������ݿ����
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_DP_Addr_, Internet_DP_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('DP|UserDB');
      StartService;
    end;

  // ע��һ�����û���testUserΪ����ʶ��������ʶ��������������ָ��͵�¼������������Ŀ����Ҫ��ʶ����ֻ�ܵ�¼��֤
  GetMyUserDB_Service.RegUser('testUser', '123456');

  // ��ͨ���ȶ�
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'DP', nil);

  // ��ѭ��
  while True do
    begin
      Z.Net.C4.C40Progress;
      TCompute.Sleep(1);
    end;

end.
