(*
https://zpascal.net
https://github.com/PassByYou888/ZNet
https://github.com/PassByYou888/zRasterization
https://github.com/PassByYou888/ZSnappy
https://github.com/PassByYou888/Z-AI1.4
https://github.com/PassByYou888/ZAI_1.41
https://github.com/PassByYou888/InfiniteIoT
https://github.com/PassByYou888/zMonitor_3rd_Core
https://github.com/PassByYou888/tcmalloc4p
https://github.com/PassByYou888/jemalloc4p
https://github.com/PassByYou888/zCloud
https://github.com/PassByYou888/ZServer4D
https://github.com/PassByYou888/zShell
https://github.com/PassByYou888/ZDB2.0
https://github.com/PassByYou888/zGameWare
https://github.com/PassByYou888/CoreCipher
https://github.com/PassByYou888/zChinese
https://github.com/PassByYou888/zSound
https://github.com/PassByYou888/zExpression
https://github.com/PassByYou888/ZInstaller2.0
https://github.com/PassByYou888/zAI
https://github.com/PassByYou888/NetFileService
https://github.com/PassByYou888/zAnalysis
https://github.com/PassByYou888/PascalString
https://github.com/PassByYou888/zInstaller
https://github.com/PassByYou888/zTranslate
https://github.com/PassByYou888/zVision
https://github.com/PassByYou888/FFMPEG-Header
*)
{ ****************************************************************************** }
{ * Z.Net test framework                                                       * }
{ ****************************************************************************** }
unit Z.Net.Test;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses SysUtils, Z.Net, Z.DFE,
  Z.UnicodeMixedLib, Z.Core, Z.Status, Z.MemoryStream, Z.PascalStrings,
  Z.Cipher, Z.Notify;

type
  TCommunicationTestIntf = class(TCore_Object_Intermediate)
  private
    FPrepareSendConsole, FPrepareResultConsole: SystemString;
    FPrepareSendDataFrame, FPrepareResultDataFrame: TDFE;
    FLastReg: TZNet;
  public
    constructor Create;
    destructor Destroy; override;

    // client test command
    procedure Cmd_TestStream(Sender: TPeerIO; InData, OutData: TDFE);
    procedure Cmd_TestConsole(Sender: TPeerIO; InData: SystemString; var OutData: SystemString);
    procedure Cmd_TestDirectStream(Sender: TPeerIO; InData: TDFE);
    procedure Cmd_TestDirectConsole(Sender: TPeerIO; InData: SystemString);
    procedure Cmd_TestBigStream(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64);
    procedure Cmd_BigStreamPostInfo(Sender: TPeerIO; InData: SystemString);
    procedure Cmd_TestCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure Cmd_RemoteInfo(Sender: TPeerIO; InData: SystemString);
    procedure Delay_RunTestReponse(Sender: TN_Post_Execute);
    procedure Cmd_RunTestReponse(Sender: TPeerIO; InData: TDFE);

    // server test command result
    procedure CmdResult_TestConsole(Sender: TPeerIO; ResultData: SystemString);
    procedure CmdResult_TestStream(Sender: TPeerIO; ResultData: TDFE);

    procedure RegCmd(Intf: TZNet);
    procedure ExecuteTest(Intf: TPeerIO);
    procedure ExecuteAsyncTest(Intf: TPeerIO);
    procedure ExecuteAsyncTestWithBigStream(Intf: TPeerIO);
    procedure ExecuteTestReponse(Intf: TPeerIO);

    property LastReg: TZNet read FLastReg;
  end;

const
  C_TestStream: SystemString = '__@TestStream';
  C_TestConsole: SystemString = '__@TestConsole';
  C_TestDirectStream: SystemString = '__@TestDirectStream';
  C_TestDirectConsole: SystemString = '__@TestDirectConsole';
  C_TestBigStream: SystemString = '__@TestBigStream';
  C_BigStreamPostInfo: SystemString = '__@BigStreamPostInfo';
  C_TestCompleteBuffer: SystemString = '__@TestCompleteBuffer';
  C_RemoteInfo: SystemString = '__@RemoteInfo';
  C_RunTestReponse: SystemString = '__@RunTestReponse';
  C_Test_Complete_Stream: SystemString = '__@Test_Complete_Stream';
  C_Test_Complete_DirectStream: SystemString = '__@Test_Complete_DirectStream';
  C_Test_Complete_Async_DirectStream: SystemString = '__@Test_Complete_Async_DirectStream';

implementation

var
  TestStreamData: TMS64 = nil;
  TestStreamMD5: SystemString;
  TestBuff: PByte;
  TestBuffSize: NativeInt;
  TestBuffMD5: SystemString;

constructor TCommunicationTestIntf.Create;
var
  i: Integer;
begin
  inherited;
  FPrepareSendConsole := 'console test';
  FPrepareResultConsole := 'console result';
  FPrepareSendDataFrame := TDFE.Create;
  FPrepareResultDataFrame := TDFE.Create;
  for i := 1 to 10 do
    begin
      FPrepareSendDataFrame.WriteInteger(i);
      FPrepareResultDataFrame.WriteInteger(i);
    end;

  FLastReg := nil;
end;

destructor TCommunicationTestIntf.Destroy;
begin
  DisposeObject(FPrepareSendDataFrame);
  DisposeObject(FPrepareResultDataFrame);
  inherited Destroy;
end;

procedure TCommunicationTestIntf.Cmd_TestStream(Sender: TPeerIO; InData, OutData: TDFE);
begin
  if not InData.Compare(FPrepareSendDataFrame) then
      Sender.Print('TestStream in Data failed!');
  OutData.Assign(FPrepareResultDataFrame);
end;

procedure TCommunicationTestIntf.Cmd_TestConsole(Sender: TPeerIO; InData: SystemString; var OutData: SystemString);
begin
  if InData <> FPrepareSendConsole then
      Sender.Print('TestConsole in Data failed!');
  OutData := FPrepareResultConsole;
end;

procedure TCommunicationTestIntf.Cmd_TestDirectStream(Sender: TPeerIO; InData: TDFE);
begin
  if not InData.Compare(FPrepareSendDataFrame) then
      Sender.Print('TestDirectStream in Data failed!');
end;

procedure TCommunicationTestIntf.Cmd_TestDirectConsole(Sender: TPeerIO; InData: SystemString);
begin
  if InData <> FPrepareSendConsole then
      Sender.Print('TestDirectConsole in Data failed!');
end;

procedure TCommunicationTestIntf.Cmd_TestBigStream(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin
  if Sender.UserDefine.BigStreamBatchList.Count = 0 then
      Sender.UserDefine.BigStreamBatchList.NewPostData;

  Sender.UserDefine.BigStreamBatchList.Last^.Source.CopyFrom(InData, InData.Size);
end;

procedure TCommunicationTestIntf.Cmd_BigStreamPostInfo(Sender: TPeerIO; InData: SystemString);
begin
  if InData <> umlStreamMD5String(Sender.UserDefine.BigStreamBatchList.Last^.Source).Text then
      Sender.Print('TestBigStream failed!');
  Sender.UserDefine.BigStreamBatchList.Clear;
end;

procedure TCommunicationTestIntf.Cmd_TestCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
begin
  if umlMD5Char(InData, DataSize).Text <> TestBuffMD5 then
      Sender.Print('TestCompleteBuffer failed!');
end;

procedure TCommunicationTestIntf.Cmd_RemoteInfo(Sender: TPeerIO; InData: SystemString);
begin
  Sender.Print('remote:' + InData);
end;

procedure TCommunicationTestIntf.Delay_RunTestReponse(Sender: TN_Post_Execute);
begin
  ExecuteAsyncTestWithBigStream(TPeerIO(Sender.Data1));
end;

procedure TCommunicationTestIntf.Cmd_RunTestReponse(Sender: TPeerIO; InData: TDFE);
begin
  with Sender.OwnerFramework.PostProgress.PostExecuteM(False, 3, Delay_RunTestReponse) do
    begin
      Data1 := Sender;
      ready();
    end;
end;

procedure TCommunicationTestIntf.CmdResult_TestConsole(Sender: TPeerIO; ResultData: SystemString);
begin
  if ResultData <> FPrepareResultConsole then
      Sender.Print('TestResultConsole Data failed!');
end;

procedure TCommunicationTestIntf.CmdResult_TestStream(Sender: TPeerIO; ResultData: TDFE);
begin
  if not ResultData.Compare(FPrepareResultDataFrame) then
      Sender.Print('TestResultStream Data failed!');
end;

procedure TCommunicationTestIntf.RegCmd(Intf: TZNet);
begin
  Intf.RegisterStream(C_TestStream).OnExecute := Cmd_TestStream;
  Intf.RegisterConsole(C_TestConsole).OnExecute := Cmd_TestConsole;
  Intf.RegisterDirectStream(C_TestDirectStream).OnExecute := Cmd_TestDirectStream;
  Intf.RegisterDirectConsole(C_TestDirectConsole).OnExecute := Cmd_TestDirectConsole;
  Intf.RegisterBigStream(C_TestBigStream).OnExecute := Cmd_TestBigStream;
  Intf.RegisterDirectConsole(C_BigStreamPostInfo).OnExecute := Cmd_BigStreamPostInfo;
  Intf.RegisterCompleteBuffer(C_TestCompleteBuffer).OnExecute := Cmd_TestCompleteBuffer;
  Intf.RegisterDirectConsole(C_RemoteInfo).OnExecute := Cmd_RemoteInfo;
  Intf.RegisterDirectStream(C_RunTestReponse).OnExecute := Cmd_RunTestReponse;
  Intf.RegisterCompleteBuffer_DirectStream(C_Test_Complete_DirectStream).OnExecute := Cmd_TestDirectStream;
  Intf.RegisterCompleteBuffer_Asynchronous_DirectStream(C_Test_Complete_Async_DirectStream).OnExecute := Cmd_TestDirectStream;
  Intf.RegisterCompleteBuffer_NoWait_Stream(C_Test_Complete_Stream).OnExecute := Cmd_TestStream;

  FLastReg := Intf;
end;

procedure TCommunicationTestIntf.ExecuteTest(Intf: TPeerIO);
var
  tmpdf: TDFE;
begin
  Intf.SendConsoleCmdM(C_TestConsole, FPrepareSendConsole, CmdResult_TestConsole);
  Intf.SendStreamCmdM(C_TestStream, FPrepareSendDataFrame, CmdResult_TestStream);
  Intf.SendDirectConsoleCmd(C_TestDirectConsole, FPrepareSendConsole);
  Intf.SendDirectStreamCmd(C_TestDirectStream, FPrepareSendDataFrame);
  Intf.SendBigStream(C_TestBigStream, TestStreamData, False);
  Intf.SendDirectConsoleCmd(C_BigStreamPostInfo, umlStreamMD5String(TestStreamData).Text);
  Intf.SendCompleteBuffer(C_TestCompleteBuffer, TestBuff, TestBuffSize, False);
  Intf.SendCompleteBuffer(C_Test_Complete_DirectStream, FPrepareSendDataFrame);
  Intf.SendCompleteBuffer(C_Test_Complete_Async_DirectStream, FPrepareSendDataFrame);

  if Intf.OwnerFramework is TZNet_Client then
    begin
      if Intf.WaitSendConsoleCmd(C_TestConsole, FPrepareSendConsole, 0) <> FPrepareResultConsole then
          Intf.Print('wait Mode:TestResultConsole Data failed!');

      tmpdf := TDFE.Create;
      Intf.WaitSendStreamCmd(C_TestStream, FPrepareSendDataFrame, tmpdf, 0);
      if not tmpdf.Compare(FPrepareResultDataFrame) then
          Intf.Print('wait Mode:TestResultStream Data failed!');
      DisposeObject(tmpdf);
    end;

  Intf.SendDirectConsoleCmd(C_RemoteInfo, 'client id[' + IntToStr(Intf.ID) + '] test over!');
end;

procedure TCommunicationTestIntf.ExecuteAsyncTest(Intf: TPeerIO);
begin
  Intf.SendConsoleCmdM(C_TestConsole, FPrepareSendConsole, CmdResult_TestConsole);
  Intf.SendStreamCmdM(C_TestStream, FPrepareSendDataFrame, CmdResult_TestStream);
  Intf.SendDirectConsoleCmd(C_TestDirectConsole, FPrepareSendConsole);
  Intf.SendDirectStreamCmd(C_TestDirectStream, FPrepareSendDataFrame);
  Intf.SendBigStream(C_TestBigStream, TestStreamData, False);
  Intf.SendDirectConsoleCmd(C_BigStreamPostInfo, umlStreamMD5String(TestStreamData).Text);
  Intf.SendCompleteBuffer(C_TestCompleteBuffer, TestBuff, TestBuffSize, False);
  Intf.SendCompleteBuffer(C_Test_Complete_DirectStream, FPrepareSendDataFrame);
  Intf.SendCompleteBuffer(C_Test_Complete_Async_DirectStream, FPrepareSendDataFrame);

  Intf.SendDirectConsoleCmd(C_RemoteInfo, 'client id[' + IntToStr(Intf.ID) + '] test over!');
end;

procedure TCommunicationTestIntf.ExecuteAsyncTestWithBigStream(Intf: TPeerIO);
begin
  Intf.SendConsoleCmdM(C_TestConsole, FPrepareSendConsole, CmdResult_TestConsole);
  Intf.SendStreamCmdM(C_TestStream, FPrepareSendDataFrame, CmdResult_TestStream);
  Intf.SendDirectConsoleCmd(C_TestDirectConsole, FPrepareSendConsole);
  Intf.SendDirectStreamCmd(C_TestDirectStream, FPrepareSendDataFrame);
  Intf.SendBigStream(C_TestBigStream, TestStreamData, False);
  Intf.SendDirectConsoleCmd(C_BigStreamPostInfo, umlStreamMD5String(TestStreamData).Text);
  Intf.SendCompleteBuffer(C_TestCompleteBuffer, TestBuff, TestBuffSize, False);
  Intf.SendCompleteBuffer(C_Test_Complete_DirectStream, FPrepareSendDataFrame);
  Intf.SendCompleteBuffer(C_Test_Complete_Async_DirectStream, FPrepareSendDataFrame);

  Intf.SendBigStream(C_TestBigStream, TestStreamData, False);
  Intf.SendDirectConsoleCmd(C_BigStreamPostInfo, TestStreamMD5);
  Intf.SendCompleteBuffer(C_TestCompleteBuffer, TestBuff, TestBuffSize, False);

  Intf.SendDirectConsoleCmd(C_RemoteInfo, 'client id[' + IntToStr(Intf.ID) + '] test over!');
end;

procedure TCommunicationTestIntf.ExecuteTestReponse(Intf: TPeerIO);
begin
  Intf.SendDirectStreamCmd(C_RunTestReponse);
end;

procedure MakeRndBuff(v: Integer; p: Pointer; siz: NativeInt);
var
  Seed: Integer;
  i: Integer;
begin
  Seed := v;
  for i := 0 to (siz div 4) - 1 do
      PInteger(Pointer(nativeUInt(p) + (i * 4)))^ := TMISC.Ran03(Seed);
end;

initialization

TestStreamData := TMS64.Create;
TestStreamData.SetSize(Int64(1024));
MakeRndBuff(99999933, TestStreamData.Memory, TestStreamData.Size);
TestStreamMD5 := umlStreamMD5String(TestStreamData).Text;

TestBuffSize := 1024;
TestBuff := System.GetMemory(TestBuffSize);
MakeRndBuff(777777, TestBuff, TestBuffSize);
TestBuffMD5 := umlMD5String(TestBuff, TestBuffSize).Text;

finalization

DisposeObjectAndNil(TestStreamData);
FreeMem(TestBuff, TestBuffSize);

end.
 
