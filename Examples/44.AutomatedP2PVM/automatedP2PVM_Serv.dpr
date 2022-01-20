program automatedP2PVM_Serv;

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
procedure RunServ;
var
  phyServ: TPhysicsServer;
  vm_serv1, vm_serv2, vm_serv3: TZNet_WithP2PVM_Server; // ��p2pVM������ͨѶ���,�����������׸�������˫ͨ��,���ݿ�,�ļ�����Ӧ��,��VM�����demo
begin
  vm_serv1 := TZNet_WithP2PVM_Server.Create;
  vm_serv2 := TZNet_WithP2PVM_Server.Create;
  vm_serv3 := TZNet_WithP2PVM_Server.Create;

  phyServ := TPhysicsServer.Create;
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv1, '::', 99);
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv2, '::', 199);
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv3, '::', 299);
  phyServ.AutomatedP2PVMService := True;
  phyServ.AutomatedP2PVMAuthToken := '123456';

  phyServ.StartService('', 9799);

  while True do
    begin
      phyServ.Progress;
      DoStatus();
      CheckThreadSynchronize(1);
    end;
end;

begin
  RunServ;

end.
