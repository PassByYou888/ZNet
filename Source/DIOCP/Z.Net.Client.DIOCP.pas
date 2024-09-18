(*
https://zpascal.net
https://github.com/PassByYou888/ZNet
https://github.com/PassByYou888/zRasterization
https://github.com/PassByYou888/ZSnappy
https://github.com/PassByYou888/Z-AI1.4
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
{ * DIOCP Support                                                              * }
{ ****************************************************************************** }
(*
  update history
*)
unit Z.Net.Client.DIOCP;

{$I ..\Z.Define.inc}

interface

uses SysUtils, Classes,
  Z.PascalStrings,
  Z.Net, Z.Core, Z.UnicodeMixedLib, Z.MemoryStream, Z.Notify,
  Z.diocp_tcp_client,
  Z.diocp_core_engine;

type
  TDIOCPClient_PeerIO = class;
  TZNet_Client_DIOCP = class;

  TIocpClientContextIntf_WithDCli = class(TIocpRemoteContext)
  private
    Link: TDIOCPClient_PeerIO;
    OwnerFramework: TZNet_Client_DIOCP;
  protected
    procedure OnConnected; override;
    procedure OnDisconnected; override;
    procedure OnConnectFail; override;
    procedure OnRecvBuffer(Buf: Pointer; Len: Cardinal; ErrCode: Word); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TDIOCPClient_PeerIO = class(TPeerIO)
  private
    Link: TIocpClientContextIntf_WithDCli;
    SendingStream: TMS64;
    CanTriggerDoDisconnect: Boolean;
  public
    procedure CreateAfter; override;
    destructor Destroy; override;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure Write_IO_Buffer(const buff: PByte; const Size: NativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;
    procedure Progress; override;
  end;

  TZNet_Client_DIOCP = class(TZNet_Client)
  private
    DIOCPClientPool: TDiocpTcpClient;
    DCIntf: TIocpClientContextIntf_WithDCli;

    FOnAsyncConnectNotify_C: TOnState_C;
    FOnAsyncConnectNotify_M: TOnState_M;
    FOnAsyncConnectNotify_P: TOnState_P;
  protected
    procedure DCDoConnected(Sender: TIocpClientContextIntf_WithDCli);
    procedure DCDoDisconnect(Sender: TIocpClientContextIntf_WithDCli);
    procedure DCDoConnectFailed(Sender: TIocpClientContextIntf_WithDCli);
    procedure DCDoRecvBuffer(Buf: Pointer; Len: Cardinal; ErrCode: Word);

    procedure DoConnected(Sender: TPeerIO); override;
    procedure DoDisconnect(Sender: TPeerIO); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure TriggerDoConnectFailed; override;
    procedure TriggerDoConnectFinished; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerIO; override;
    procedure Progress; override;

    procedure AsyncConnectC(addr: SystemString; Port: Word; const OnResult: TOnState_C); overload; override;
    procedure AsyncConnectM(addr: SystemString; Port: Word; const OnResult: TOnState_M); overload; override;
    procedure AsyncConnectP(addr: SystemString; Port: Word; const OnResult: TOnState_P); overload; override;
    function Connect(addr: SystemString; Port: Word): Boolean; overload; override;
    procedure Disconnect; override;
  end;

implementation

procedure TIocpClientContextIntf_WithDCli.OnConnected;
begin
  inherited OnConnected;
  OwnerFramework.DCDoConnected(Self);
end;

procedure TIocpClientContextIntf_WithDCli.OnDisconnected;
begin
  inherited OnDisconnected;
  OwnerFramework.DCDoDisconnect(Self);
end;

procedure TIocpClientContextIntf_WithDCli.OnConnectFail;
begin
  inherited OnConnectFail;
  OwnerFramework.DCDoConnectFailed(Self);
end;

procedure TIocpClientContextIntf_WithDCli.OnRecvBuffer(Buf: Pointer; Len: Cardinal; ErrCode: Word);
begin
  OwnerFramework.DCDoRecvBuffer(Buf, Len, ErrCode);
  inherited OnRecvBuffer(Buf, Len, ErrCode);
end;

constructor TIocpClientContextIntf_WithDCli.Create;
begin
  inherited Create;
  Link := nil;
end;

destructor TIocpClientContextIntf_WithDCli.Destroy;
begin
  if Link <> nil then
    begin
      Link.Link := nil;
      Link := nil;
    end;
  inherited Destroy;
end;

procedure TDIOCPClient_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  Link := nil;
  SendingStream := TMS64.Create;
  CanTriggerDoDisconnect := True;
end;

destructor TDIOCPClient_PeerIO.Destroy;
var
  cintf: TIocpClientContextIntf_WithDCli;
begin
  if Link <> nil then
    begin
      cintf := Link;
      Link := nil;
      cintf.Link := nil;
      if CanTriggerDoDisconnect then
          TZNet_Client_DIOCP(OwnerFramework).DoDisconnect(Self);
      cintf.Close();
    end;
  DisposeObject(SendingStream);
  inherited Destroy;
end;

function TDIOCPClient_PeerIO.Connected: Boolean;
begin
  Result := (Link <> nil) and (Link.Active);
end;

procedure TDIOCPClient_PeerIO.Disconnect;
var
  cintf: TIocpClientContextIntf_WithDCli;
begin
  if Link <> nil then
    begin
      cintf := Link;
      Link := nil;
      cintf.Link := nil;
      if CanTriggerDoDisconnect then
          TZNet_Client_DIOCP(OwnerFramework).DoDisconnect(Self);
      cintf.Close();
    end;
  DisposeObject(Self);
end;

procedure TDIOCPClient_PeerIO.Write_IO_Buffer(const buff: PByte; const Size: NativeInt);
begin
  if not Connected then
      Exit;

  if Size > 0 then
      SendingStream.WritePtr(buff, Size);
end;

procedure TDIOCPClient_PeerIO.WriteBufferOpen;
begin
  WriteBufferFlush;
end;

procedure TDIOCPClient_PeerIO.WriteBufferFlush;
begin
  if SendingStream.Size > 0 then
    begin
      Link.PostWSASendRequest(SendingStream.Memory, SendingStream.Size, True);
      SendingStream.Clear;
    end;
end;

procedure TDIOCPClient_PeerIO.WriteBufferClose;
begin
  WriteBufferFlush;
end;

function TDIOCPClient_PeerIO.GetPeerIP: SystemString;
begin
  if Connected then
      Result := Link.Host + ' ' + IntToStr(Link.Port)
  else
      Result := '';
end;

procedure TDIOCPClient_PeerIO.Progress;
begin
  inherited Progress;
  Process_Send_Buffer();
end;

procedure TZNet_Client_DIOCP.DCDoConnected(Sender: TIocpClientContextIntf_WithDCli);
begin
  if Sender.Link <> nil then
    begin
      Sender.Link.Print('connected addr: %s port: %d', [Sender.Host, Sender.Port]);
      DoConnected(Sender.Link);
    end;
end;

procedure TZNet_Client_DIOCP.DCDoDisconnect(Sender: TIocpClientContextIntf_WithDCli);
begin
  if Sender.Link <> nil then
    begin
      DisposeObject(Sender.Link);
      Sender.Link := TDIOCPClient_PeerIO.Create(Self, Sender);
      Sender.Link.Link := Sender;
      TriggerDoConnectFailed;
    end;
end;

procedure TZNet_Client_DIOCP.DCDoConnectFailed(Sender: TIocpClientContextIntf_WithDCli);
begin
  TriggerDoConnectFailed;
end;

procedure TZNet_Client_DIOCP.DCDoRecvBuffer(Buf: Pointer; Len: Cardinal; ErrCode: Word);
begin
  DCIntf.Link.Write_Physics_Fragment(Buf, Len);
end;

procedure TZNet_Client_DIOCP.DoConnected(Sender: TPeerIO);
begin
  inherited DoConnected(Sender);
end;

procedure TZNet_Client_DIOCP.DoDisconnect(Sender: TPeerIO);
begin
  inherited DoDisconnect(Sender);
end;

constructor TZNet_Client_DIOCP.Create;
begin
  inherited Create;
  EnabledAtomicLockAndMultiThread := False;

  DIOCPClientPool := TDiocpTcpClient.Create(nil);
  DIOCPClientPool.RegisterContextClass(TIocpClientContextIntf_WithDCli);
  DIOCPClientPool.NoDelayOption := True;
  DIOCPClientPool.DisableAutoConnect := True;
  DIOCPClientPool.TrigerDisconnectEventAfterNoneConnected := False;
  DIOCPClientPool.Open;

  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TDIOCPClient_PeerIO.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;

  name := 'DIOCP-Client';
end;

destructor TZNet_Client_DIOCP.Destroy;
begin
  DCIntf.Link.CanTriggerDoDisconnect := False;
  DisposeObject(DCIntf.Link);
  DIOCPClientPool.RemoveAllContext;
  DIOCPClientPool.Close;
  DisposeObject(DIOCPClientPool);
  Check_Soft_Thread_Synchronize(1, False);
  inherited Destroy;
end;

procedure TZNet_Client_DIOCP.TriggerDoConnectFailed;
begin
  inherited TriggerDoConnectFailed;

  try
    if Assigned(FOnAsyncConnectNotify_C) then
        FOnAsyncConnectNotify_C(False)
    else if Assigned(FOnAsyncConnectNotify_M) then
        FOnAsyncConnectNotify_M(False)
    else if Assigned(FOnAsyncConnectNotify_P) then
        FOnAsyncConnectNotify_P(False);
  except
  end;

  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;
end;

procedure TZNet_Client_DIOCP.TriggerDoConnectFinished;
begin
  inherited TriggerDoConnectFinished;

  try
    if Assigned(FOnAsyncConnectNotify_C) then
        FOnAsyncConnectNotify_C(True)
    else if Assigned(FOnAsyncConnectNotify_M) then
        FOnAsyncConnectNotify_M(True)
    else if Assigned(FOnAsyncConnectNotify_P) then
        FOnAsyncConnectNotify_P(True);
  except
  end;

  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;
end;

function TZNet_Client_DIOCP.Connected: Boolean;
begin
  Result := (ClientIO <> nil) and (ClientIO.Connected);
end;

function TZNet_Client_DIOCP.ClientIO: TPeerIO;
begin
  Result := DCIntf.Link;
end;

procedure TZNet_Client_DIOCP.Progress;
begin
  inherited Progress;
  try
      Check_Soft_Thread_Synchronize(1, False);
  except
  end;
end;

procedure TZNet_Client_DIOCP.AsyncConnectC(addr: SystemString; Port: Word; const OnResult: TOnState_C);
begin
  if (DCIntf.Link <> nil) then
      DCIntf.Link.CanTriggerDoDisconnect := Connected;
  DisposeObject(DCIntf.Link);
  DIOCPClientPool.RemoveAllContext;
  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TDIOCPClient_PeerIO.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  FOnAsyncConnectNotify_C := OnResult;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;

  DCIntf.Host := addr;
  DCIntf.Port := Port;
  DCIntf.ConnectASync;
end;

procedure TZNet_Client_DIOCP.AsyncConnectM(addr: SystemString; Port: Word; const OnResult: TOnState_M);
begin
  if (DCIntf.Link <> nil) then
      DCIntf.Link.CanTriggerDoDisconnect := Connected;
  DisposeObject(DCIntf.Link);
  DIOCPClientPool.RemoveAllContext;
  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TDIOCPClient_PeerIO.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := OnResult;
  FOnAsyncConnectNotify_P := nil;

  DCIntf.Host := addr;
  DCIntf.Port := Port;
  DCIntf.ConnectASync;
end;

procedure TZNet_Client_DIOCP.AsyncConnectP(addr: SystemString; Port: Word; const OnResult: TOnState_P);
begin
  if (DCIntf.Link <> nil) then
      DCIntf.Link.CanTriggerDoDisconnect := Connected;
  DisposeObject(DCIntf.Link);
  DIOCPClientPool.RemoveAllContext;
  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TDIOCPClient_PeerIO.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := OnResult;

  DCIntf.Host := addr;
  DCIntf.Port := Port;
  DCIntf.ConnectASync;
end;

function TZNet_Client_DIOCP.Connect(addr: SystemString; Port: Word): Boolean;
var
  t: TTimeTick;
begin
  if (DCIntf.Link <> nil) then
      DCIntf.Link.CanTriggerDoDisconnect := Connected;
  DisposeObject(DCIntf.Link);
  DIOCPClientPool.RemoveAllContext;
  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TDIOCPClient_PeerIO.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  Result := False;

  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;

  DCIntf.Host := addr;
  DCIntf.Port := Port;
  try
      DCIntf.Connect;
  except
    Result := False;
    Exit;
  end;

  t := GetTimeTick + 5000;

  while DCIntf.Active and (not DCIntf.Link.RemoteExecutedForConnectInit) and (GetTimeTick < t) do
      Progress;

  Result := True;
end;

procedure TZNet_Client_DIOCP.Disconnect;
begin
  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;

  if (DCIntf.Link <> nil) then
      DCIntf.Link.CanTriggerDoDisconnect := Connected;
  DisposeObject(DCIntf.Link);
  DIOCPClientPool.RemoveAllContext;
  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TDIOCPClient_PeerIO.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;
  Check_Soft_Thread_Synchronize(1, False);
end;

initialization

finalization

end.
 
