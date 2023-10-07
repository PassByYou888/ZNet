program _144_CompleteBuffer_Server;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  FastMM5,
  System.SysUtils,
  Z.Core, Z.DFE, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status,
  Z.Parsing, Z.Expression, Z.OpCode, Z.MemoryStream,
  Z.Net, Z.Net.C4, Z.Net.C4_Console_APP;

type
  TDemo_Bridge_Server = class(TC40_Base_NoAuth_Service)
  public
    procedure cmd_cb_hello_world(Sender: TPeerIO; InData, OutData: TDFE);
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
  end;

  TDemo_Bridge_Client = class(TC40_Base_NoAuth_Client);

procedure TDemo_Bridge_Server.cmd_cb_hello_world(Sender: TPeerIO; InData, OutData: TDFE);
begin
  OutData.WriteString('bridge hello world');
end;

constructor TDemo_Bridge_Server.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited;
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_NoWait_Stream('cb_hello_world').OnExecute := cmd_cb_hello_world;
end;

type
  TDemo_Server = class(TC40_Base_NoAuth_Service)
  public
    deploy_bridge: TDemo_Bridge_Client; // �Զ�����ķ������ӿ�
  public
    procedure cmd_cb_directstream(Sender: TPeerIO; InData: TDFE);
    procedure cmd_cb_async_directstream(Sender: TPeerIO; InData: TDFE);
    procedure cmd_cb_stream(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_cb_thread_stream(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_cb_bridge_stream(Sender: TCommandCompleteBuffer_NoWait_Bridge; InData, OutData: TDFE);
    procedure cmd_cb_bridge_stream_HPC(Sender: TCommandCompleteBuffer_NoWait_Bridge; InData, OutData: TDFE);
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
  end;

procedure TDemo_Server.cmd_cb_directstream(Sender: TPeerIO; InData: TDFE);
begin
  DoStatus('cmd_cb_directstream ���յ������� %s', [umlMD5ToStr(InData.GetMD5(True)).Text]);
end;

procedure TDemo_Server.cmd_cb_async_directstream(Sender: TPeerIO; InData: TDFE);
begin
  DoStatus('cmd_cb_async_directstream ���յ������� %s', [umlMD5ToStr(InData.GetMD5(True)).Text]);
end;

procedure TDemo_Server.cmd_cb_stream(Sender: TPeerIO; InData, OutData: TDFE);
begin
  DoStatus('cmd_cb_stream ���յ������� %s', [umlMD5ToStr(InData.GetMD5(True)).Text]);
  OutData.Assign(InData);
  DoStatus('cmd_cb_stream ���ص����� %s', [umlMD5ToStr(OutData.GetMD5(True)).Text]);
end;

procedure TDemo_Server.cmd_cb_thread_stream(Sender: TPeerIO; InData, OutData: TDFE);
begin
  DoStatus('cmd_cb_thread_stream ���յ������� %s', [umlMD5ToStr(InData.GetMD5(True)).Text]);
  OutData.Assign(InData);
  DoStatus('cmd_cb_thread_stream ���ص����� %s', [umlMD5ToStr(OutData.GetMD5(True)).Text]);
end;

procedure TDemo_Server.cmd_cb_bridge_stream(Sender: TCommandCompleteBuffer_NoWait_Bridge; InData, OutData: TDFE);
var
  bridge_: TCompleteBuffer_Stream_Event_Bridge;
begin
  // ���ž����¼�ָ��,ʣ�µ������Զ�����
  bridge_ := TCompleteBuffer_Stream_Event_Bridge.Create(Sender);
  deploy_bridge.DTNoAuth.SendTunnel.SendCompleteBuffer_NoWait_StreamM('cb_hello_world', InData, bridge_.DoStreamEvent);
end;

procedure TDemo_Server.cmd_cb_bridge_stream_HPC(Sender: TCommandCompleteBuffer_NoWait_Bridge; InData, OutData: TDFE);
begin
  RunHPC_CompleteBuffer_StreamP(Sender, nil, nil, InData, OutData, procedure(thSender: THPC_CompleteBuffer_Stream; ThInData, ThOutData: TDFE)
    begin
      ThOutData.WriteString('HPC hello world.');
    end);
end;

constructor TDemo_Server.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited;
  // CompleteBuffer,���̹߳���ģʽ,������,InData�������߳̽���,���������������ϸ����ģ��
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_DirectStream('cb_directstream').OnExecute := cmd_cb_directstream;

  // CompleteBuffer,�̹߳���ģʽ,������,InData���߳��н���,ִ������Ϊ���ϸ����
  // InData���߳��н���
  // InData���߳��н���
  // InData���߳��н���
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_Asynchronous_DirectStream('cb_async_directstream').OnExecute := cmd_cb_async_directstream;

  // CompleteBuffer,���̹߳���ģʽ,������,InData�������߳̽���,OutData�������̱߳���,���������������ϸ����ģ��,��������Ϊ���ϸ����
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_NoWait_Stream('cb_stream').OnExecute := cmd_cb_stream;

  // CompleteBuffer,�̹߳���ģʽ,������,InData�����߳̽���,OutData�����̱߳���,�����¼����߳���ִ��,���߳�ִ����IO�����ʧ(����֮��),�ڴ������������,���ϸ�����
  // �߳���ִ��
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_NoWait_Stream_Thread('cb_thread_stream').OnExecute := cmd_cb_thread_stream;

  // CompleteBuffer,���̹߳���ģʽ,������,InData�������߳̽���,OutData�������̱߳���,���������������ϸ����ģ��,��������Ϊ���ϸ����
  // ��Ҫ�ر�ע��Ĺ��ܵ�,��Ҫ������˵����
  // ��RegisterCompleteBuffer_NoWait_Bridge_Stream�������¼�֧�ּ���+֧��HPC�����߳�
  // ��RegisterCompleteBuffer_NoWait_Bridge_Stream�������¼�֧�ּ���+֧��HPC�����߳�
  // ��RegisterCompleteBuffer_NoWait_Bridge_Stream�������¼�֧�ּ���+֧��HPC�����߳�
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_NoWait_Bridge_Stream('cb_bridge_stream').OnExecute := cmd_cb_bridge_stream;
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_NoWait_Bridge_Stream('cb_bridge_stream_HPC').OnExecute := cmd_cb_bridge_stream_HPC;

  // ��������������һ������������Ժ�,�ÿ�ܻ�������ķ��������,��ʱ��,�����Զ�������:�÷����������Ժ�,�Զ��������Ķ�������ʼ��
  // �Զ����������ǽ��Ѿ�������ͨѶ������赽Ŀ�����
  // ��Ϊc4�����пͻ��˶��ܶ�������,����ĸ�ֵһ���ɹ�,�ͻ��ǳ�����,��ʹ�Ͽ�,��һ��Ҳ��ָ�����
  // ֻ�е�����idc�����¼�ʱdeploy_bridge�Ż�������ʧ��,������޷������ڴ����
  // c4��idc�����¼�ֻ�е����������Ӵﵽһ�ܲŻᴥ��,ÿ�ܿ�һ�·�����״̬,��������������������
  // ���Զ�����д�ɷ���,������������ƥ��,���������ʱд������Ұ���
  // TC40_Auto_Deployment_Client����˵��:ִ��ʱ����������ֵĿ�����,��C4���������Ժ�,Ŀ������ᱻ�Զ���ֵ,���Ŀ�������ʱ��δ��ֵ����δ�����ɹ�
  TC40_Auto_Deployment_Client<TDemo_Bridge_Client>.Create('Demo_Bridge', deploy_bridge);
end;

type
  TDemo_Client = class(TC40_Base_NoAuth_Client)
  public
    procedure help_test(var OP_Param: TOpParam);
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
  end;

procedure TDemo_Client.help_test(var OP_Param: TOpParam);
begin
  TCompute.RunP_NP(procedure
    var
      m64: TMS64;
      d: TDFE;
      i: Integer;
    begin
      d := TDFE.Create;
      m64 := TMS64.Create;
      m64.Size := 10 * 1024 * 1024;
      d.WriteStream(m64);
      m64.Free;
      DoStatus('buff size: %s', [umlSizeToStr(d.ComputeEncodeSize).Text]);

      // ��Ҫ������˵����
      // ���ͨѶ����ģ���Ƿ��ϸ�����,ZNet�������Ż�ʱֻ��Ҫ��SendDirectStream�滻��SendCompleteBuffer_DirectStream,�����л����߳�
      // ���ͨѶ����ģ���Ƿ��ϸ�����,ZNet�������Ż�ʱֻ��Ҫ��SendDirectStream�滻��SendCompleteBuffer_DirectStream,�����л����߳�
      // ���ͨѶ����ģ���Ƿ��ϸ�����,ZNet�������Ż�ʱֻ��Ҫ��SendDirectStream�滻��SendCompleteBuffer_DirectStream,�����л����߳�
      DTNoAuth.SendTunnel.SendCompleteBuffer_DirectStream('cb_directstream', d);
      DTNoAuth.SendTunnel.SendCompleteBuffer_DirectStream('cb_async_directstream', d);

      // ��Ҫ������˵����
      // ���ͨѶ����ģ���Ƿ��ϸ�����,ZNet�������Ż�ʱֻ��Ҫ��SendStream�滻��SendCompleteBuffer_NoWait_Stream,�����л����߳�
      // ���ͨѶ����ģ���Ƿ��ϸ�����,ZNet�������Ż�ʱֻ��Ҫ��SendStream�滻��SendCompleteBuffer_NoWait_Stream,�����л����߳�
      // ���ͨѶ����ģ���Ƿ��ϸ�����,ZNet�������Ż�ʱֻ��Ҫ��SendStream�滻��SendCompleteBuffer_NoWait_Stream,�����л����߳�
      DTNoAuth.SendTunnel.SendCompleteBuffer_NoWait_StreamP('cb_stream', d, procedure(Sender: TPeerIO; Result_: TDFE)
        begin
          DoStatus('cb_stream �ͻ��� ���յ������� %s', [umlMD5ToStr(Result_.GetMD5(True)).Text]);
        end);
      DTNoAuth.SendTunnel.SendCompleteBuffer_NoWait_StreamP('cb_thread_stream', d, procedure(Sender: TPeerIO; Result_: TDFE)
        begin
          DoStatus('cb_thread_stream �ͻ��� ���յ������� %s', [umlMD5ToStr(Result_.GetMD5(True)).Text]);
        end);
      DTNoAuth.SendTunnel.SendCompleteBuffer_NoWait_StreamP('cb_bridge_stream', d, procedure(Sender: TPeerIO; Result_: TDFE)
        begin
          DoStatus('cb_bridge_stream �ͻ��� ���յ������� %s', [Result_.R.ReadString]);
        end);
      DTNoAuth.SendTunnel.SendCompleteBuffer_NoWait_StreamP('cb_bridge_stream_HPC', d, procedure(Sender: TPeerIO; Result_: TDFE)
        begin
          DoStatus('cb_bridge_stream �ͻ��� ���յ������� %s', [Result_.R.ReadString]);
        end);
      d.Free;
    end);
end;

constructor TDemo_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  Register_ConsoleCommand('test', 'test(),����CompleteBuffr').OnEvent_M := help_test; // ע��help����
end;

begin
  // ע��һ��
  RegisterC40('Demo_Bridge', TDemo_Bridge_Server, TDemo_Bridge_Client);
  RegisterC40('Demo_CB', TDemo_Server, TDemo_Client);

  // Ϊ�˷�����д�ű�,ʹ��C����ı����ʽ
  C40AppParsingTextStyle := TTextStyle.tsC;

  // ������C4�ű�,����������+�ͻ���,�����������
  // �����൱�ڿ���2��������+2���ͻ���,���ÿ�demo����,ֱ�Ӷ�һ��д
  // Demoʹ�÷���:�����ɹ��Ժ�,�������������Ѿ�ready ok, ��testm����
  if C40_Extract_CmdLine([
    'Service("0.0.0.0", "127.0.0.1", 9093, "Demo_Bridge")', // ��һ�б�ʾ����������,���������
    'Service("0.0.0.0", "127.0.0.1", 9099, "Demo_CB")', // ��һ�б�ʾ����������,���������
    'Client("127.0.0.1", 9099, "Demo_CB")', // ��һ�б�ʾ�����ͻ���,��ʹ��127.0.0.1�������
    'Client("127.0.0.1", 9093, "Demo_Bridge")' // ��һ�б�ʾ�����ͻ���,��ʹ��127.0.0.1�������
    ]) then
      C40_Execute_Main_Loop; // Consoleר��,��ѭ��,���������help����

  // �ͷ�C4
  Z.Net.C4.C40Clean;

end.
