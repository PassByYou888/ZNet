program automatedP2PVM_HPC_Serv;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.DFE,
  Z.Parsing,
  Z.Expression,
  Z.OpCode;

// ������ʾ��hpc�������ش��ģ����
procedure cmd_runExp(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
begin
  // nop���������߳�����
  nop;
  // RunHPC_StreamP������ʱ��IO��ͣ����,�����������������߳�ִ��
  // RunHPC_StreamP��hpc��ר���ں��̳߳ص���,��ȫ����,����delphi/fpc�����̳߳�
  RunHPC_StreamP(Sender, nil, nil, InData, OutData, procedure(ThSender: THPC_Stream; ThInData, ThOutData: TDataFrameEngine)
    var
      num: Integer;
      exp: U_String;
      i: Integer;
      tk: TTimeTick;
      op: TOpCode;
    begin
      // ���´��붼���߳�����ִ��
      tk := GetTimeTick;
      num := ThInData.Reader.ReadInteger;
      exp := ThInData.Reader.ReadString;
      op := BuildAsOpCode(tsPascal, exp);
      for i := 0 to num - 1 do
          op.Execute();
      // ��ThOutData���涪����,����ʾҪ����.�����Ƿ���ִ��num�����ı��ʽʱ��
      ThOutData.WriteUInt64(GetTimeTick - tk);
    end); // ����һ��ִ����ɺ�,IO��ָ�����
end;

// AutomatedP2PVM��һ�ּ��򻯵�p2pVMӦ�÷�ʽ,���������ü��̴�������p2pVM���
// AutomatedP2PVMҲ���Ʒ�����3.0��ͨѶ�ػ�
procedure RunServ;
var
  phyServ: TPhysicsServer;
  vm_serv1, vm_serv2, vm_serv3, vm_serv4: TZNet_WithP2PVM_Server; // ��p2pVM������ͨѶ���,�����������׸�������˫ͨ��,���ݿ�,�ļ�����Ӧ��,��VM�����demo
begin
  vm_serv1 := TZNet_WithP2PVM_Server.Create;
  vm_serv2 := TZNet_WithP2PVM_Server.Create;
  vm_serv3 := TZNet_WithP2PVM_Server.Create;
  vm_serv4 := TZNet_WithP2PVM_Server.Create;

  phyServ := TPhysicsServer.Create;
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv1, '::', 99);    // ::��ͬ��0:0:0:0:0:0 ��p2pVM�е�����Ipv6,������ʵ�����ַ,�õ�ַ�����
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv2, '::', 199);   // ::��ͬ��0:0:0:0:0:0 ��p2pVM�е�����Ipv6,������ʵ�����ַ,�õ�ַ�����
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv3, '::', 299);   // ::��ͬ��0:0:0:0:0:0 ��p2pVM�е�����Ipv6,������ʵ�����ַ,�õ�ַ�����
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv4, '88::', 399); // 88::��ͬ��88:0:0:0:0:0 ��p2pVM�е�����Ipv6,������ʵ�����ַ,�õ�ַ�����
  phyServ.AutomatedP2PVMService := True;
  phyServ.AutomatedP2PVMAuthToken := '123456';
  vm_serv4.RegisterStream('runExp').OnExecute_C := cmd_runExp; // ע��ָ��
  phyServ.StartService('', 9799);

  while True do
    begin
      phyServ.Progress;
      DoStatus();
      CheckThreadSynchronize(10);
    end;
end;

begin
  RunServ;

end.
