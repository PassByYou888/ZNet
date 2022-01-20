program automatedP2PVM_Cli;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.Net,
  Z.Net.PhysicsIO;

// AutomatedP2PVM��һ�ּ��򻯵�p2pVMӦ�÷�ʽ,���������ü��̴�������p2pVM���
// AutomatedP2PVMҲ���Ʒ�����3.0��ͨѶ�ػ�
procedure RunAutomatedP2PVM_Client;
var
  phyCli: TPhysicsClient;
  vm_cli1, vm_cli2, vm_cli3: TZNet_WithP2PVM_Client; // ��p2pVM������ͨѶ���,�����������׸�������˫ͨ��,���ݿ�,�ļ�����Ӧ��,��VM�����demo
  tk: TTimeTick;
begin
  vm_cli1 := TZNet_WithP2PVM_Client.Create;
  vm_cli2 := TZNet_WithP2PVM_Client.Create;
  vm_cli3 := TZNet_WithP2PVM_Client.Create;

  phyCli := TPhysicsClient.Create;
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli1, '::', 99);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli2, '::', 199);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli3, '::', 299);
  phyCli.AutomatedP2PVMClient := True;
  phyCli.AutomatedP2PVMAuthToken := '123456';
  phyCli.Connect('127.0.0.1', 9799);

  tk := GetTimeTick;
  while GetTimeTick - tk < 5000 do
    begin
      phyCli.Progress;
      DoStatus();
      CheckThreadSynchronize(1);
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
