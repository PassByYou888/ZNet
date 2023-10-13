library zsLIB;

uses
  Z.Core,
  Z.PascalStrings,
  Z.Status,
  Z.Net.PhysicsIO,
  Z.Net.Client.CrossSocket,
  Z.Net,
  Z.Net.Test;

procedure DoStatus_BackCall(AText: SystemString; const ID: Integer);
begin
  Writeln(AText);
end;

procedure DLL_Init_Proc(); stdcall;
begin
  AddDoStatusHookC(nil, DoStatus_BackCall);
end;

procedure DLL_Exit_Proc(); stdcall;
begin
  DeleteDoStatusHook(nil);

  {  When we were in FreeLibrary, we often worked in finalization  }
  {  If threads or thread pools are used, the CheckSynchronize operation supported by the system often results in stuck operations  }
  {  The solution is to add an Exit API during FreeLibrary, allowing the DLL to kill some things for a smooth exit  }
end;

procedure DLL_ThreadSync_Proc(); stdcall;
begin
  CheckThreadSynchronize();
end;

procedure DLL_Demo_Proc(); stdcall;
var
  cli: TZNet_Client;
  Test: TCommunicationTestIntf;
begin
  cli := TPhysicsClient.Create;
  Test := TCommunicationTestIntf.Create;
  Test.RegCmd(cli);
  if cli.Connect('127.0.0.1', 8191) then
    begin
      Test.ExecuteTest(cli.ClientIO);
      Test.ExecuteAsyncTest(cli.ClientIO);
      Test.ExecuteAsyncTestWithBigStream(cli.ClientIO);
      cli.Wait(5000);
    end;
  DisposeObject(cli);
  DisposeObject(Test);
end;

procedure DLL_DemoAsyncThread_Proc(); stdcall;
begin
  TComputeThread.RunP(nil, nil, procedure(Sender: TComputeThread)
    begin
      DoStatus('Thread async on dll');
    end,
    procedure(Sender: TComputeThread)
    begin
      DoStatus('Thread sync on dll');
    end);
end;

exports
  DLL_Init_Proc,
  DLL_Exit_Proc,
  DLL_ThreadSync_Proc,
  DLL_Demo_Proc,
  DLL_DemoAsyncThread_Proc;

begin

end.
