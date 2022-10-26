{ ****************************************************************************** }
{ * FPC Synpase service Support, Max Connection:100                            * }
{ ****************************************************************************** }
(*
  Synapse Server的最大连接被限制到100
  update history
*)

unit Z.Net.Server.Synapse;

{$I ..\Z.Define.inc}

interface

uses SysUtils, Classes,
  Z.PascalStrings, Z.UPascalStrings, Z.Net, Z.Core, Z.MemoryStream, Z.DFE,
  Z.synsock, Z.blcksock;

type
  TZNet_Server_Synapse = class;
  TSynapseSockTh = class;

  TSynapseServer_PeerIO = class(TPeerIO)
  protected
    SockTh: TSynapseSockTh;
    LastPeerIP: SystemString;
    SendBuffQueue: TCore_ListForObj;
    CurrentBuff: TMS64;
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
    procedure Sync_CreateIO;
    procedure Execute; override;
  end;

  TSynapseSockTh = class(TCore_Thread)
  public
    ClientSockID: TSocket;
    Activted: Boolean;
    IO: TSynapseServer_PeerIO;
    Sock: TTCPBlockSocket;
    CurrentSendBuff: TMS64;
    Recv_Buff: Pointer;
    Recv_Siz: Integer;
    procedure Sync_PickBuff;
    procedure Sync_CloseIO;
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

    procedure TriggerQueueData(v: PQueueData); override;
    procedure Progress; override;

    procedure CloseAll;

    function WaitSendConsoleCmd(p_io: TPeerIO;
      const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString; override;
    procedure WaitSendStreamCmd(p_io: TPeerIO;
      const Cmd: SystemString; StreamData, ResultData: TDFE; Timeout: TTimeTick); override;
  end;

implementation

procedure TSynapseServer_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  SockTh := nil;
  LastPeerIP := '';
  SendBuffQueue := TCore_ListForObj.Create;
  CurrentBuff := TMS64.Create;
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

  for i := 0 to SendBuffQueue.Count - 1 do
      DisposeObject(SendBuffQueue[i]);

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
      CurrentBuff := TMS64.Create;
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
  Result := (SendBuffQueue.Count = 0) and (SockTh <> nil) and (SockTh.CurrentSendBuff = nil);
end;

procedure TSynapseServer_PeerIO.Progress;
begin
  inherited Progress;
  Self.Process_Send_Buffer;
end;

procedure TSynapseListenTh.Sync_CreateIO;
var
  CurrentAcceptSockTh: TSynapseSockTh;
begin
  CurrentAcceptSockTh := TSynapseSockTh.Create(True);
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
          if LSock.CanRead(10) then
              TCompute.SyncM({$IFDEF FPC}@{$ENDIF FPC}Sync_CreateIO);
        except
            Listen := False;
        end;
      end;

  LSock.CloseSocket;
  DisposeObject(LSock);
  Server.FListenTh := nil;
end;

procedure TSynapseSockTh.Sync_PickBuff;
begin
  if (IO.SendBuffQueue.Count > 0) and (IO <> nil) then
    begin
      CurrentSendBuff := TMS64(IO.SendBuffQueue[0]);
      IO.SendBuffQueue.Delete(0);
    end;
end;

procedure TSynapseSockTh.Sync_CloseIO;
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
  memSiz: Integer = 1024 * 1024;
begin
  FreeOnTerminate := True;
  Sock := TTCPBlockSocket.Create;
  Sock.Socket := ClientSockID;
  Sock.GetSins;
  Activted := True;

  Recv_Buff := System.GetMemory(memSiz);
  while (Activted) and (IO <> nil) do
    begin
      try
        while Activted and (IO.SendBuffQueue.Count > 0) do
          begin
            CurrentSendBuff := nil;
            TCompute.SyncM({$IFDEF FPC}@{$ENDIF FPC}Sync_PickBuff);

            if CurrentSendBuff <> nil then
              begin
                Sock.SendBuffer(CurrentSendBuff.Memory, CurrentSendBuff.Size);
                DisposeObject(CurrentSendBuff);
                CurrentSendBuff := nil;
              end;
          end;

        if Activted and Sock.CanRead(100) then
          begin
            Recv_Siz := Sock.RecvBuffer(Recv_Buff, memSiz);

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
  TCompute.SyncM({$IFDEF FPC}@{$ENDIF FPC}Sync_CloseIO);
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
  while not FListenTh.Activted do
      Z.Core.CheckThreadSynchronize(1);
end;

destructor TZNet_Server_Synapse.Destroy;
begin
  StopService;
  FListenTh.Activted := False;
  while FListenTh <> nil do
      Z.Core.CheckThreadSynchronize(1);
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

procedure TZNet_Server_Synapse.TriggerQueueData(v: PQueueData);
var
  c: TPeerIO;
begin
  c := PeerIO[v^.IO_ID];
  if c <> nil then
    begin
      c.PostQueueData(v);
      c.Process_Send_Buffer();
    end
  else
      DisposeQueueData(v);
end;

procedure TZNet_Server_Synapse.Progress;
begin
  inherited Progress;
  CheckThread;
end;

procedure TZNet_Server_Synapse.CloseAll;
begin
  ProgressPeerIOM({$IFDEF FPC}@{$ENDIF FPC}All_Disconnect);
end;

function TZNet_Server_Synapse.WaitSendConsoleCmd(p_io: TPeerIO;
  const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport');
end;

procedure TZNet_Server_Synapse.WaitSendStreamCmd(p_io: TPeerIO;
  const Cmd: SystemString; StreamData, ResultData: TDFE; Timeout: TTimeTick);
begin
  RaiseInfo('WaitSend no Suppport');
end;

initialization

finalization

end.
