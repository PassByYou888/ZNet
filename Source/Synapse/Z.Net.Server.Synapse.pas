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
{ * FPC Synpase service Support, Max Connection:100                            * }
{ ****************************************************************************** }
(*
  Synapse Server的最大连接被限制到100
*)

unit Z.Net.Server.Synapse;

{$DEFINE FPC_DELPHI_MODE}
{$I ..\Z.Define.inc}

interface

uses SysUtils, Classes,
  Z.PascalStrings, Z.UPascalStrings, Z.Net, Z.Core, Z.MemoryStream, Z.DFE,
  Z.synsock, Z.blcksock;

type
  TZNet_Server_Synapse = class;
  TSynapseSockTh = class;

  // 带锁队列容器,装待发数据
  TSynapseClient_Send_Buffer_Queue = class(TCritical_Big_Object_List<TMem64>)
  public
    constructor Create;
  end;

  TSynapseServer_PeerIO = class(TPeerIO)
  protected
    SockTh: TSynapseSockTh;
    LastPeerIP: SystemString;
    SendBuffQueue: TSynapseClient_Send_Buffer_Queue;
    CurrentBuff: TMem64;
  public
    procedure CreateAfter; override;
    destructor Destroy; override;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure Write_IO_Buffer(const buff: PByte; const Size: nativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;
    function WriteBuffer_is_NULL: Boolean; override;
    procedure Progress; override;
  end;

  TSynapseListenTh = class(TCore_Thread)
  public
    Server: TZNet_Server_Synapse;
    LSock: TTCPBlockSocket;
    Activted: Boolean;
    Listen: Boolean;
    Sock_Th_Num: TAtomInt;
    procedure Do_CreateIO;
    procedure Execute; override;
  end;

  TSynapseSockTh = class(TCore_Thread)
  public
    Sock_Th_Num: TAtomInt;
    ClientSockID: TSocket;
    Activted: Boolean;
    IO: TSynapseServer_PeerIO;
    Sock: TTCPBlockSocket;
    CurrentSendBuff: TMem64;
    Recv_Buff: Pointer;
    Recv_Siz: Integer;
    procedure Do_PickBuff;
    procedure Do_CloseIO;
    procedure Execute; override;
  end;

  TZNet_Server_Synapse = class(TZNet_Server)
  protected
    FListenTh: TSynapseListenTh;
    procedure All_Disconnect(PeerClient: TPeerIO);
  public
    constructor Create; override;
    destructor Destroy; override;

    function StartService(Host: SystemString; Port: Word): Boolean; override;
    procedure StopService; override;

    procedure Progress; override;

    procedure CloseAll;

    function WaitSendConsoleCmd(p_io: TPeerIO;
      const Cmd, ConsoleData: SystemString; Timeout_: TTimeTick): SystemString; override;
    procedure WaitSendStreamCmd(p_io: TPeerIO;
      const Cmd: SystemString; StreamData, ResultData: TDFE; Timeout_: TTimeTick); override;
  end;

implementation

constructor TSynapseClient_Send_Buffer_Queue.Create;
begin
  inherited Create(True);
end;

procedure TSynapseServer_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  SockTh := nil;
  LastPeerIP := '';
  SendBuffQueue := TSynapseClient_Send_Buffer_Queue.Create;
  CurrentBuff := TMem64.CustomCreate(8192);
end;

destructor TSynapseServer_PeerIO.Destroy;
var
  i: Integer;
begin
  if SockTh <> nil then
    begin
      SockTh.IO := nil;
      SockTh.Activted := False;
    end;

  DisposeObject(SendBuffQueue);

  DisposeObject(CurrentBuff);
  inherited Destroy;
end;

function TSynapseServer_PeerIO.Connected: Boolean;
begin
  Result := (SockTh <> nil) and (SockTh.Activted);
end;

procedure TSynapseServer_PeerIO.Disconnect;
begin
  if SockTh <> nil then
    begin
      SockTh.IO := nil;
      SockTh.Activted := False;
    end;
  DisposeObject(Self);
end;

procedure TSynapseServer_PeerIO.Write_IO_Buffer(const buff: PByte; const Size: nativeInt);
begin
  if not Connected then
      Exit;

  if Size > 0 then
      CurrentBuff.write(Pointer(buff)^, Size);
end;

procedure TSynapseServer_PeerIO.WriteBufferOpen;
begin
  if not Connected then
      Exit;
  CurrentBuff.Clear;
end;

procedure TSynapseServer_PeerIO.WriteBufferFlush;
begin
  if not Connected then
      Exit;
  if CurrentBuff.Size > 0 then
    begin
      SendBuffQueue.Add(CurrentBuff);
      CurrentBuff := TMem64.CustomCreate(8192);
    end;
end;

procedure TSynapseServer_PeerIO.WriteBufferClose;
begin
  if not Connected then
      Exit;
  CurrentBuff.Clear;
end;

function TSynapseServer_PeerIO.GetPeerIP: SystemString;
begin
  if Connected then
    begin
      Result := SockTh.Sock.GetRemoteSinIP;
      LastPeerIP := Result;
    end
  else
      Result := LastPeerIP;
end;

function TSynapseServer_PeerIO.WriteBuffer_is_NULL: Boolean;
begin
  Result := (SendBuffQueue.Num <= 0) and (SockTh <> nil) and (SockTh.CurrentSendBuff = nil);
end;

procedure TSynapseServer_PeerIO.Progress;
begin
  inherited Progress;
  Self.Process_Send_Buffer;
end;

procedure TSynapseListenTh.Do_CreateIO;
var
  CurrentAcceptSockTh: TSynapseSockTh;
begin
  Sock_Th_Num.UnLock(Sock_Th_Num.LockP^ + 1);
  CurrentAcceptSockTh := TSynapseSockTh.Create(True);
  CurrentAcceptSockTh.Sock_Th_Num := Sock_Th_Num;
  CurrentAcceptSockTh.ClientSockID := LSock.Accept;
  CurrentAcceptSockTh.Activted := False;
  CurrentAcceptSockTh.IO := TSynapseServer_PeerIO.Create(Server, CurrentAcceptSockTh);
  CurrentAcceptSockTh.IO.SockTh := CurrentAcceptSockTh;
  CurrentAcceptSockTh.Suspended := False;

  while not CurrentAcceptSockTh.Activted do
      TCompute.Sleep(1);
end;

procedure TSynapseListenTh.Execute;
var
  ClientSock: TSocket;
begin
  FreeOnTerminate := True;
  LSock := TTCPBlockSocket.Create;
  LSock.CreateSocket;
  Activted := True;

  while Activted do
    if (Listen) and (Server.Count <= 100) then
      begin
        try
          if LSock.CanRead(15) then
              Do_CreateIO();
        except
            Listen := False;
        end;
      end;

  LSock.CloseSocket;
  DisposeObject(LSock);

  while Sock_Th_Num.V > 0 do
      Sleep(1);
  DisposeObjectAndNil(Sock_Th_Num);
  Server.FListenTh := nil;
end;

procedure TSynapseSockTh.Do_PickBuff;
begin
  if (IO.SendBuffQueue.Num > 0) and (IO <> nil) then
    begin
      CurrentSendBuff := IO.SendBuffQueue[0].Swap_To_New_Instance;
      IO.SendBuffQueue.Next;
    end;
end;

procedure TSynapseSockTh.Do_CloseIO;
begin
  if IO <> nil then
    begin
      IO.SockTh := nil;
      DisposeObject(IO);
    end;
  DisposeObject(Sock);
end;

procedure TSynapseSockTh.Execute;
const
  MemSiz: Integer = 64 * 1024;
begin
  FreeOnTerminate := True;
  Sock := TTCPBlockSocket.Create;
  Sock.Socket := ClientSockID;
  Sock.GetSins;
  Activted := True;

  Recv_Buff := System.GetMemory(MemSiz);
  while (Activted) and (IO <> nil) do
    begin
      try
        while Activted and (IO.SendBuffQueue.Num > 0) do
          begin
            CurrentSendBuff := nil;
            Do_PickBuff;

            if CurrentSendBuff <> nil then
              begin
                Sock.SendBuffer(CurrentSendBuff.Memory, CurrentSendBuff.Size);
                DisposeObject(CurrentSendBuff);
                CurrentSendBuff := nil;
              end;
          end;

        if Activted and Sock.CanRead(15) then
          begin
            Recv_Siz := Sock.RecvBuffer(Recv_Buff, MemSiz);

            if Sock.LastError <> 0 then
                break;

            if (Activted) and (Recv_Siz > 0) then
                IO.Write_Physics_Fragment(Recv_Buff, Recv_Siz);
          end;
      except
          Activted := False;
      end;
    end;
  System.FreeMemory(Recv_Buff);
  Do_CloseIO;
  Sock_Th_Num.UnLock(Sock_Th_Num.LockP^ - 1);
end;

procedure TZNet_Server_Synapse.All_Disconnect(PeerClient: TPeerIO);
begin
  PeerClient.Disconnect;
end;

constructor TZNet_Server_Synapse.Create;
begin
  inherited Create;
  EnabledAtomicLockAndMultiThread := False;

  FListenTh := TSynapseListenTh.Create(True);
  FListenTh.Server := Self;
  FListenTh.Activted := False;
  FListenTh.Suspended := False;
  FListenTh.Sock_Th_Num := TAtomInt.Create(0);
  while not FListenTh.Activted do
      Z.Core.Check_Soft_Thread_Synchronize(1, False);

  name := 'Synapse-Server';
end;

destructor TZNet_Server_Synapse.Destroy;
begin
  StopService;
  FListenTh.Activted := False;
  while FListenTh <> nil do
      Z.Core.Check_Soft_Thread_Synchronize(1, False);
  inherited Destroy;
end;

function TZNet_Server_Synapse.StartService(Host: SystemString; Port: Word): Boolean;
begin
  try
    FListenTh.LSock.SetLinger(True, 10000);
    FListenTh.LSock.EnableReuse(True); // fixed by.qq47324905
    FListenTh.LSock.Bind(Host, IntToStr(Port));
    FListenTh.LSock.Listen;
    FListenTh.Listen := True;
    Result := FListenTh.LSock.LastError = 0;
  except
    FListenTh.LSock.SetLinger(False, 0);
    Result := False;
    FListenTh.Listen := False;
  end;
end;

procedure TZNet_Server_Synapse.StopService;
begin
  CloseAll;
  FListenTh.LSock.SetLinger(False, 0);
  FListenTh.Listen := False;
end;

procedure TZNet_Server_Synapse.Progress;
begin
  inherited Progress;
end;

procedure TZNet_Server_Synapse.CloseAll;
begin
  ProgressPeerIOM(All_Disconnect);
end;

function TZNet_Server_Synapse.WaitSendConsoleCmd(p_io: TPeerIO;
  const Cmd, ConsoleData: SystemString; Timeout_: TTimeTick): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport');
end;

procedure TZNet_Server_Synapse.WaitSendStreamCmd(p_io: TPeerIO;
  const Cmd: SystemString; StreamData, ResultData: TDFE; Timeout_: TTimeTick);
begin
  RaiseInfo('WaitSend no Suppport');
end;

initialization

finalization

end.
 
