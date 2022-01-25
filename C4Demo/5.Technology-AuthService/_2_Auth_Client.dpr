program _2_Auth_Client;

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
  Z.Net.C4;

const
  // ���ȷ������˿ڹ�����ַ,������ipv4,ipv6,dns
  // ������ַ,���ܸ�127.0.0.1����
  Internet_DP_Addr_ = '127.0.0.1';
  // ���ȷ������˿�
  Internet_DP_Port_ = 8387;

function GetVirtualAuth_Client: TC40_Base_VirtualAuth_Client;
begin
  Result := TC40_Base_VirtualAuth_Client(C40_ClientPool.ExistsConnectedServiceTyp('MyVA'));
end;

begin
  // һ�仰�ܽ��Զ�����֤���磬ͨ���״������֤�󣬿����Զ�������

  RegisterC40('MyVA', TC40_Base_VirtualAuth_Service, TC40_Base_VirtualAuth_Client);
  Z.Net.C4.C40_QuietMode := False;

  // VirtualAuth����֤���ƣ����������Ժ󲻴���˫ͨ�������ǵȴ�ִ����֤���ƣ�һ��ͨ����֤������˫ͨ�����Ӳ������Զ�����
  // �������Զ�����󣬶������������Զ���ͨ�������֤��¼
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'MyVA', nil);

  // WaitConnectedDone����ͬʱ��������������Ƿ����
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('MyVA', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    begin
      if not GetVirtualAuth_Client.LoginIsSuccessed then
        begin
          // RegisterUserAndLogin�Ǹ����أ�Ĭ��Ϊfalse�����Ժ�connect�������Զ���ע�����û���ע��ɹ�ʱ�Ὺ����֤��¼���������Զ�����
          // ��ע��ʧ��ʱ��ϵͳ���Զ��״ε�¼�������¼�ɹ��������Զ����磬��¼ʧ�ܣ����أ����������Զ�����
          // ע�⣺���ʹ����֤ģʽ����c4����ͨ������ǰ����Ҫ��Ϊֵ��ͨ����֤
          GetVirtualAuth_Client.Client.RegisterUserAndLogin := True;
          GetVirtualAuth_Client.Client.Connect_P('User_Test', '123456', procedure(const State: Boolean)
            begin
              if State then
                  DoStatus('ע����¼�ɹ�')
              else
                  DoStatus('ע����¼ʧ��.');
            end);
        end;
    end);

  // ��ѭ��
  while True do
    begin
      Z.Net.C4.C40Progress;
      TCompute.Sleep(1);
    end;

end.
