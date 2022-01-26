program reverse_automatedP2PVM_Serv;

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
procedure RunServ;
var
  phyServ: TPhysicsServer;

  vm_cli1, vm_cli2, vm_cli3: TZNet_WithP2PVM_Client; // ��p2pVM������ͨѶ����,�����������׸�������˫ͨ��,���ݿ�,�ļ�����Ӧ��,��VM�����demo
begin
  vm_cli1 := TZNet_WithP2PVM_Client.Create;
  vm_cli2 := TZNet_WithP2PVM_Client.Create;
  vm_cli3 := TZNet_WithP2PVM_Client.Create;

  phyServ := TPhysicsServer.Create;
  phyServ.AutomatedP2PVMClientBind.AddClient(vm_cli1, '::', 99);
  phyServ.AutomatedP2PVMClientBind.AddClient(vm_cli2, '::', 199);
  phyServ.AutomatedP2PVMClientBind.AddClient(vm_cli3, '::', 299);
  phyServ.AutomatedP2PVMClient := True;
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