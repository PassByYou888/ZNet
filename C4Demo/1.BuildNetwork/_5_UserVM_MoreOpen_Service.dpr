program _5_UserVM_MoreOpen_Service;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_FS,
  Z.Net.C4_UserDB,
  Z.Net.C4_Var,
  Z.Net.C4_Console_APP;

const
  // ���ȷ������˿ڹ�����ַ,������ipv4,ipv6,dns
  // ������ַ,���ܸ�127.0.0.1����
  Internet_DP_Addr_ = '127.0.0.1';
  // ���ȷ������˿�
  Internet_DP_Port_ = 8387;

  // ���ط�����������ַ
  Internet_LocalService_Addr_ = '127.0.0.1';
  Internet_LocalService_Port_ = 8384;

var
  FS: TC40_FS_Client = nil;
  UserDB: TC40_UserDB_Client = nil;
  Var_: TC40_Var_Client = nil;

type
  // C4��������ɢʽ��,һ�����ӻ���ȡ��������������,ʹ�ýӿ�������
  TMonitorMySAAS = class(TCore_InterfacedObject, IC40_PhysicsTunnel_Event)
    procedure C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    procedure C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
  end;

procedure TMonitorMySAAS.C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
begin
  // ������������
end;

procedure TMonitorMySAAS.C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
begin
  // ���������ж�
  if Sender.DependNetworkClientPool.IndexOf(FS) >= 0 then
      FS := nil;
  if Sender.DependNetworkClientPool.IndexOf(UserDB) >= 0 then
      UserDB := nil;
  if Sender.DependNetworkClientPool.IndexOf(Var_) >= 0 then
      Var_ := nil;
end;

procedure TMonitorMySAAS.C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
  // ����p2pVM���
end;

procedure TMonitorMySAAS.C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
  // p2pVM����������
  if Custom_Client_ is TC40_FS_Client then
    begin
      FS := Custom_Client_ as TC40_FS_Client;
      DoStatus('���ҵ��ļ�֧�ַ���: %s', [Custom_Client_.ClientInfo.ServiceTyp.Text]);
    end;
  if Custom_Client_ is TC40_UserDB_Client then
    begin
      UserDB := Custom_Client_ as TC40_UserDB_Client;
      DoStatus('���ҵ��û����ݿ�֧�ַ���: %s', [Custom_Client_.ClientInfo.ServiceTyp.Text]);
    end;
  if Custom_Client_ is TC40_Var_Client then
    begin
      Var_ := Custom_Client_ as TC40_Var_Client;
      DoStatus('���ҵ��������֧�ַ���: %s', [Custom_Client_.ClientInfo.ServiceTyp.Text]);
    end;
end;

begin
  // ��Log��Ϣ
  Z.Net.C4.C40_QuietMode := False;

  // ��������
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_LocalService_Addr_, Internet_LocalService_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('dp');
      StartService;
    end;

  // ��ͨ���ȶ�,�ļ�����,�û����ݿ����,�����������
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'dp|FS|UserDB|var', TMonitorMySAAS.Create);

  // ѭ�����ָ�����Ƿ�׼������,�������Ǵ���ĳЩ�¼�
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('dp|fs|UserDB|var', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    begin
      DoStatus('���е����������Ѿ�׼������....�������ɵ�ʲô��.');
    end);

  // ��ѭ��
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
