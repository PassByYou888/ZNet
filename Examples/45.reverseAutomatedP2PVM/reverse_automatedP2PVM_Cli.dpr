program reverse_automatedP2PVM_Cli;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.Net,
  Z.Net.PhysicsIO;

// reverseΪ����AutomatedP2PVMģ��,�� 31.reverseP2PVM �����demo
procedure RunAutomatedP2PVM_Client;
var
  phyCli: TPhysicsClient;
  tk: TTimeTick;
  vm_serv1, vm_serv2, vm_serv3: TZNet_WithP2PVM_Server; // ��p2pVM������ͨѶ���,�����������׸�������˫ͨ��,���ݿ�,�ļ�����Ӧ��,��VM�����demo
begin
  vm_serv1 := TZNet_WithP2PVM_Server.Create;
  vm_serv2 := TZNet_WithP2PVM_Server.Create;
  vm_serv3 := TZNet_WithP2PVM_Server.Create;

  phyCli := TPhysicsClient.Create;
  phyCli.AutomatedP2PVMServiceBind.AddService(vm_serv1, '::', 99);
  phyCli.AutomatedP2PVMServiceBind.AddService(vm_serv2, '::', 199);
  phyCli.AutomatedP2PVMServiceBind.AddService(vm_serv3, '::', 299);
  phyCli.AutomatedP2PVMService := True;
  phyCli.AutomatedP2PVMAuthToken := '123456';
  phyCli.Connect('127.0.0.1', 9799);

  tk := GetTimeTick;
  while GetTimeTick - tk < 5000 do
    begin
      phyCli.Progress;
      DoStatus();
      CheckThreadSynchronize(10);
    end;

  phyCli.Disconnect;
  disposeObject(vm_serv1);
  disposeObject(vm_serv2);
  disposeObject(vm_serv3);
  disposeObject(phyCli);
end;

begin
  RunAutomatedP2PVM_Client();

end.
