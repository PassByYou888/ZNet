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
  // ��������FreeLibraryʱ������ʹ����SyncEvent�ĵ��ã����Ῠס
  // ��debug׷��������ʹ��SyncEvent�ĵط�
  // Ѱ��ʹ��SyncEvent��ͬ���ĵط������ǿ�����finalization�¼�������׷����

  // crossSocket�Ľӿ��и�clientPool������finalization�ͷţ���ʹ��SyncEvent
  DisposeObject(ClientPool);
end;

procedure DLL_ThreadSync_Proc(); stdcall;
begin
  CheckThreadSynchronize();
end;

procedure DLL_Demo_Proc(); stdcall;
var
  cli: TZNet_Client;
  test: TCommunicationTestIntf;
begin
  cli := TPhysicsClient.Create;
  test := TCommunicationTestIntf.Create;
  test.RegCmd(cli);
  if cli.Connect('127.0.0.1', 8191) then
    begin
      test.ExecuteTest(cli.ClientIO);
      test.ExecuteAsyncTest(cli.ClientIO);
      test.ExecuteAsyncTestWithBigStream(cli.ClientIO);
      cli.Wait(5000);
    end;
  DisposeObject(cli);
  DisposeObject(test);
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
