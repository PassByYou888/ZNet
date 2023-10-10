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

  // 当我们在FreeLibrary时,很多时候都在finalization里面干
  // 如果使用了线程或线程池,需要系统所支持的CheckSynchronize操作往往会出现卡死
  // 解决办法办法就是在FreeLibrary时候多调一个Exit的api,让dll去干掉一些东西,方便顺利exit
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
