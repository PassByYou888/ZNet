program _500_Multi_Long_Run_In_ZNet_Serv;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  FastMM5, // ���̺߳�̨��MM��,������
  SysUtils,
  Z.Core, Z.Status, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.DFE, Z.Expression, Z.OpCode,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth;

var
  serv_: TDT_P2PVM_NoAuth_Service;

  // ��������ص���Ǻ�̨������+�ӳٻ�����,��Ҳ����򵥵Ķ������л�����
  // ���������,��c4�ܹ��б�����ʹ��.
  // ���������ʽ���������,��������Ŀ����������ʹ�õļ���:long run��һ�ַ�ʽ���ʵļ���
procedure Do_Th_Large_Expression(ThSender: THPC_DirectStream; ThInData: TDFE);
var
  user_data_: UInt64; // Զ�̽ṹ
  i: Integer;
  OutData: TDFE;
begin
  user_data_ := ThInData.R.ReadPointer;
  OutData := TDFE.Create;
  OutData.WritePointer(user_data_); // ����ָ��
  while ThInData.R.NotEnd do
      OutData.WriteString(umlVarToStr(EvaluateExpressionValue(False, ThInData.R.ReadString)));

  // ͨ������ͨ��������ȥ,��S_ID��ʽ�����ǰ�ȫ��
  serv_.SendTunnel.SendDirectStreamCmd(ThSender.Send_Tunnel_ID, 'Done_Large_Expression', OutData);

  // ���鷽ʽ
  // TZNet_Server(ThSender.Send_Tunnel).SendDirectStreamCmd(ThSender.Send_Tunnel_ID, 'Done_Large_Expression', OutData);

  // �ڶ����������ͷ�ʽ,���ַ�ʽ����ִ������Ժ�,Զ��IO�պö��ߴӶ��쳣,��ʱ��,ʹ��TCompute���쳣������������Щ�쳣
  // ThSender.IO.Get_Send_Tunnel_IO.SendDirectStreamCmd('Done_Large_Expression', OutData);

  OutData.Free;
end;

procedure cmd_Large_Expression(Sender: TPeerIO; InData: TDFE);
begin
  // ����ת�����߳�
  RunHPC_DirectStreamC(Sender, nil, nil, InData, Do_Th_Large_Expression);
end;

procedure run_serv;
begin
  serv_ := TDT_P2PVM_NoAuth_Service.Create(TDTService_NoAuth);
  serv_.QuietMode := True; // ������
  serv_.RecvTunnel.RegisterDirectStream('Large_Expression').OnExecute_C := cmd_Large_Expression;

  serv_.StartService('0.0.0.0', '10999', '123456');
  while True do
    begin
      CheckThread();
      serv_.Progress;
      TCompute.Sleep(1);
    end;
end;

begin
  run_serv;

end.
