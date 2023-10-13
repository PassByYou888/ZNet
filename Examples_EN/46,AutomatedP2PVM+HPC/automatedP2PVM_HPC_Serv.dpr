﻿program automatedP2PVM_HPC_Serv;

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

{  This demonstrates how HPC can load large-scale computing  }
procedure cmd_runExp(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
begin
  {  The nop function is in the main thread section  }
  nop;
  {  RunHPC_When StreamP is called, IO pauses feedback and immediately sends the request to the child thread for execution  }
  {  RunHPC_StreamP is scheduled by hpc's dedicated kernel thread pool, with secure scheduling, not delphi/fpc's built-in thread pool  }
  RunHPC_StreamP(Sender, nil, nil, InData, OutData, procedure(ThSender: THPC_Stream; ThInData, ThOutData: TDataFrameEngine)
    var
      num: Integer;
      exp: U_String;
      i: Integer;
      tk: TTimeTick;
      op: TOpCode;
    begin
      {  The following code is executed in the thread section  }
      tk := GetTimeTick;
      num := ThInData.Reader.ReadInteger;
      exp := ThInData.Reader.ReadString;
      op := BuildAsOpCode(tsPascal, exp);
      for i := 0 to num - 1 do
          op.Execute();
      {  Dropping data into ThOutData means that feedback is needed. Here is the expression time for the number of num executed for feedback  }
      ThOutData.WriteUInt64(GetTimeTick - tk);
    end); {  After completing this step, IO will resume feedback  }
end;

{  AutomatedP2PVM is a highly simplified P2P VM application paradigm that has the ability to drive the P2P VM framework with extremely short code  }
{  Automated P2P VM is also the communication foundation of ECS 3.0  }
procedure RunServ;
var
  phyServ: TPhysicsServer;
  vm_serv1, vm_serv2, vm_serv3, vm_serv4: TZNet_WithP2PVM_Server; {  The communication tunnel driven by P2P VM can be used to host various dual channel, database, and file transfer applications, as shown in the relevant demos of VM  }
begin
  vm_serv1 := TZNet_WithP2PVM_Server.Create;
  vm_serv2 := TZNet_WithP2PVM_Server.Create;
  vm_serv3 := TZNet_WithP2PVM_Server.Create;
  vm_serv4 := TZNet_WithP2PVM_Server.Create;

  phyServ := TPhysicsServer.Create;
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv1, '::', 99);    {  : is equivalent to 0:0:0:0:0:0:0, which is a virtual Ipv6 in P2P VM and not a real physical address. Any given address  }
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv2, '::', 199);   {  : is equivalent to 0:0:0:0:0:0:0, which is a virtual Ipv6 in P2P VM and not a real physical address. Any given address  }
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv3, '::', 299);   {  : is equivalent to 0:0:0:0:0:0:0, which is a virtual Ipv6 in P2P VM and not a real physical address. Any given address  }
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv4, '88::', 399); {  88:: equivalent to 88:0:0:0:0: 0 is a virtual IPv6 in p2pvm, not a real physical address. This address is given arbitrarily  }
  phyServ.AutomatedP2PVMService := True;
  phyServ.AutomatedP2PVMAuthToken := '123456';
  vm_serv4.RegisterStream('runExp').OnExecute_C := cmd_runExp; {  Registration instruction  }
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
