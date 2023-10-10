program automatedP2PVM_HPC_Cli;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.DFE,
  Z.Net,
  Z.Net.PhysicsIO;

// AutomatedP2PVM��һ�ּ��򻯵�p2pVMӦ�÷�ʽ,���������ü��̴�������p2pVM���
// AutomatedP2PVMҲ���Ʒ�����3.0��ͨѶ�ػ�
procedure RunAutomatedP2PVM_Client;
var
  phyCli: TPhysicsClient;
  vm_cli1, vm_cli2, vm_cli3, vm_cli4: TZNet_WithP2PVM_Client; // ��p2pVM������ͨѶ���,�����������׸�������˫ͨ��,���ݿ�,�ļ�����Ӧ��,��VM�����demo
begin
  vm_cli1 := TZNet_WithP2PVM_Client.Create;
  vm_cli2 := TZNet_WithP2PVM_Client.Create;
  vm_cli3 := TZNet_WithP2PVM_Client.Create;
  vm_cli4 := TZNet_WithP2PVM_Client.Create;

  phyCli := TPhysicsClient.Create;
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli1, '::', 99);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli2, '::', 199);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli3, '::', 299);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli4, '88::', 399); // 88::��ͬ��88:0:0:0:0:0 ��p2pVM�е�����Ipv6,������ʵ�����ַ,�õ�ַ��Ҫ��p2pVM����������ַ�Ժ�
  phyCli.AutomatedP2PVMClient := True;
  phyCli.AutomatedP2PVMAuthToken := '123456';
  phyCli.Connect('127.0.0.1', 9799);

  phyCli.QuietMode := True;
  vm_cli1.QuietMode := True;
  vm_cli2.QuietMode := True;
  vm_cli3.QuietMode := True;
  vm_cli4.QuietMode := True;

  // ������ʾ��hpc�������ش��ģ����
  // ��hpc��demo�ͻ��˿��Զ࿪,������cpuԽ��,��������Խ��
  phyCli.OnAutomatedP2PVMClientConnectionDone_P := procedure(Sender: TZNet; P_IO: TPeerIO)
    var
      de, tmp: TDataFrameEngine;
    begin
      if phyCli.AutomatedP2PVMClientConnectionDone(phyCli.ClientIO) then
        begin
          phyCli.Print('��������������������');
          de := TDataFrameEngine.Create;
          de.WriteInteger(100 * 10000);
          de.WriteString('1+1=2');

          // ������ʽ���͵ȷ���
          tmp := TDFE.Create;
          vm_cli4.WaitSendStreamCmd('runExp', de, tmp, 5000);
          DoStatus('ִ��100��α��ʽ��ʱ %d ����', [tmp.Reader.ReadUInt64]);
          disposeObject(tmp);

          // �첽��ʽ���͵ȷ���
          vm_cli4.SendStreamCmdP('runExp', de, procedure(Sender: TPeerIO; ResultData: TDataFrameEngine)
            begin
              DoStatus('ִ��100��α��ʽ��ʱ %d ����', [ResultData.Reader.ReadUInt64]);
              // ѭ������
              TCompute.PostP1(procedure
                begin
                  phyCli.OnAutomatedP2PVMClientConnectionDone_P(phyCli, phyCli.ClientIO);
                end);
            end);
          disposeObject(de);
        end;
    end;

  while True do
    begin
      phyCli.Progress;
      DoStatus();
      CheckThreadSynchronize(10);
    end;

  phyCli.Disconnect;
  disposeObject(vm_cli1);
  disposeObject(vm_cli2);
  disposeObject(vm_cli3);
  disposeObject(phyCli);
end;

begin
  RunAutomatedP2PVM_Client();

end.
