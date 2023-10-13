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

{  AutomatedP2PVM is a highly simplified P2P VM application paradigm that has the ability to drive the P2P VM framework with extremely short code  }
{  Automated P2P VM is also the communication foundation of ECS 3.0  }
procedure RunAutomatedP2PVM_Client;
var
  phyCli: TPhysicsClient;
  vm_cli1, vm_cli2, vm_cli3, vm_cli4: TZNet_WithP2PVM_Client; {  The communication tunnel driven by P2P VM can be used to host various dual channel, database, and file transfer applications, as shown in the relevant demos of VM  }
begin
  vm_cli1 := TZNet_WithP2PVM_Client.Create;
  vm_cli2 := TZNet_WithP2PVM_Client.Create;
  vm_cli3 := TZNet_WithP2PVM_Client.Create;
  vm_cli4 := TZNet_WithP2PVM_Client.Create;

  phyCli := TPhysicsClient.Create;
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli1, '::', 99);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli2, '::', 199);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli3, '::', 299);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli4, '88::', 399); {  88:: Equivalent to 88:0:0:0:0:0:0, it is a virtual Ipv6 in p2pVM and not a real physical address. This address needs to be matched with the listening address of the p2pVM service  }
  phyCli.AutomatedP2PVMClient := True;
  phyCli.AutomatedP2PVMAuthToken := '123456';
  phyCli.Connect('127.0.0.1', 9799);

  phyCli.QuietMode := True;
  vm_cli1.QuietMode := True;
  vm_cli2.QuietMode := True;
  vm_cli3.QuietMode := True;
  vm_cli4.QuietMode := True;

  {  This demonstrates how HPC can load large-scale computing  }
  {  The demo client of the HPC can be opened more, and the better the server CPU, the better the computing power  }
  phyCli.OnAutomatedP2PVMClientConnectionDone_P := procedure(Sender: TZNet; P_IO: TPeerIO)
    var
      de, tmp: TDataFrameEngine;
    begin
      if phyCli.AutomatedP2PVMClientConnectionDone(phyCli.ClientIO) then
        begin
          phyCli.Print('All virtual tunnels have been connected');
          de := TDataFrameEngine.Create;
          de.WriteInteger(100 * 10000);
          de.WriteString('1+1=2');

          {  Feedback such as blocking method sending  }
          tmp := TDFE.Create;
          vm_cli4.WaitSendStreamCmd('runExp', de, tmp, 5000);
          DoStatus('It took%d milliseconds to execute the expression 1 million times', [tmp.Reader.ReadUInt64]);
          disposeObject(tmp);

          {  Asynchronous sending and other feedback  }
          vm_cli4.SendStreamCmdP('runExp', de, procedure(Sender: TPeerIO; ResultData: TDataFrameEngine)
            begin
              DoStatus('It took%d milliseconds to execute the expression 1 million times', [ResultData.Reader.ReadUInt64]);
              {  Loop call  }
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
