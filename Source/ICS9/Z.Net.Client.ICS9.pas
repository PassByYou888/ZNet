{ ****************************************************************************** }
{ * ics support                                                                * }
{ ****************************************************************************** }
unit Z.Net.Client.ICS9;

{$I ..\Z.Define.inc}

interface

uses Windows, SysUtils, Classes, Messages,
  Z.PascalStrings,
  Z.ICS9.OverbyteIcsWSocket,
  Z.Net.Server.ICS9CustomSocket,
  Z.Net, Z.Core, Z.Status;

type
  TZNet_Client_ICS9 = class;

  TICS9_Client_PeerIO = class(TPeerIO)
  public
    procedure CreateAfter; override;
    destructor Destroy; override;

    function Context: TZNet_Client_ICS9;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure Write_IO_Buffer(const buff: PByte; const Size: NativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;
  end;

  TClient_ICS9_Context_Intf = class(TCustom_ICS9)
  end;

  TZNet_Client_ICS9 = class(TZNet_Client)
  protected
    FDriver: TClient_ICS9_Context_Intf;
    FClient: TICS9_Client_PeerIO;

    FAsyncConnecting: Boolean;
    FOnAsyncConnectNotify_C: TOnState_C;
    FOnAsyncConnectNotify_M: TOnState_M;
    FOnAsyncConnectNotify_P: TOnState_P;

    procedure DataAvailable(Sender: TObject; ErrCode: Word);
    procedure SessionClosed(Sender: TObject; ErrCode: Word);
    procedure SessionConnectedAndCreateContext(Sender: TObject; ErrCode: Word);
    procedure AsyncConnect(addr: SystemString; Port: Word; OnResultCall: TOnState_C; OnResultMethod: TOnState_M; OnResultProc: TOnState_P); overload;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure TriggerDoConnectFailed; override;
    procedure TriggerDoConnectFinished; override;

    procedure AsyncConnectC(addr: SystemString; Port: Word; const OnResult: TOnState_C); overload; override;
    procedure AsyncConnectM(addr: SystemString; Port: Word; const OnResult: TOnState_M); overload; override;
    procedure AsyncConnectP(addr: SystemString; Port: Word; const OnResult: TOnState_P); overload; override;

    function Connect(Host, Port: SystemString; AWaitTimeOut: TTimeTick): Boolean; overload;
    function Connect(Host, Port: SystemString): Boolean; overload;
    function Connect(addr: SystemString; Port: Word): Boolean; overload; override;
    procedure Disconnect; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerIO; override;

    procedure Progress; override;
  end;

implementation

procedure TICS9_Client_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
end;

destructor TICS9_Client_PeerIO.Destroy;
begin
  inherited Destroy;
end;

function TICS9_Client_PeerIO.Context: TZNet_Client_ICS9;
begin
  Result := IOInterface as TZNet_Client_ICS9;
end;

function TICS9_Client_PeerIO.Connected: Boolean;
begin
  Result := Context.Connected;
end;

procedure TICS9_Client_PeerIO.Disconnect;
begin
  Context.Disconnect;
end;

procedure TICS9_Client_PeerIO.Write_IO_Buffer(const buff: PByte; const Size: NativeInt);
begin
  if Connected then
      Context.FDriver.Send(buff, Size);
end;

procedure TICS9_Client_PeerIO.WriteBufferOpen;
begin
end;

procedure TICS9_Client_PeerIO.WriteBufferFlush;
begin
  if Connected then
      Context.FDriver.TryToSend;
end;

procedure TICS9_Client_PeerIO.WriteBufferClose;
begin
end;

function TICS9_Client_PeerIO.GetPeerIP: SystemString;
begin
  Result := Context.FDriver.addr;
end;

procedure TZNet_Client_ICS9.DataAvailable(Sender: TObject; ErrCode: Word);
var
  BuffCount: Integer;
  buff: PByte;
begin
  // increment receive
  BuffCount := FDriver.RcvdCount;
  if BuffCount <= 0 then
      BuffCount := 255 * 255;
  buff := System.GetMemory(BuffCount);
  BuffCount := FDriver.Receive(buff, BuffCount);
  if BuffCount > 0 then
    begin
      FClient.Write_Physics_Fragment(buff, BuffCount);
    end;
  System.FreeMemory(buff);
end;

procedure TZNet_Client_ICS9.SessionClosed(Sender: TObject; ErrCode: Word);
begin
  if FAsyncConnecting then
      TriggerDoConnectFailed;
  DoDisconnect(FClient);
end;

procedure TZNet_Client_ICS9.SessionConnectedAndCreateContext(Sender: TObject; ErrCode: Word);
begin
  DoConnected(FClient);
end;

procedure TZNet_Client_ICS9.AsyncConnect(addr: SystemString; Port: Word; OnResultCall: TOnState_C; OnResultMethod: TOnState_M; OnResultProc: TOnState_P);
begin
  Disconnect;

  FDriver.OnSessionConnected := SessionConnectedAndCreateContext;
  FAsyncConnecting := True;
  FOnAsyncConnectNotify_C := OnResultCall;
  FOnAsyncConnectNotify_M := OnResultMethod;
  FOnAsyncConnectNotify_P := OnResultProc;

  FDriver.Proto := 'tcp';
  FDriver.Port := IntToStr(Port);
  FDriver.addr := addr;

  try
      FDriver.Connect;
  except
      TriggerDoConnectFailed;
  end;
end;

constructor TZNet_Client_ICS9.Create;
begin
  inherited Create;
  EnabledAtomicLockAndMultiThread := False;

  FDriver := TClient_ICS9_Context_Intf.Create(nil);
  FDriver.MultiThreaded := False;
  FDriver.KeepAliveOnOff := TSocketKeepAliveOnOff.wsKeepAliveOnCustom;
  FDriver.KeepAliveTime := 1 * 1000;
  FDriver.KeepAliveInterval := 1 * 1000;
  FDriver.OnDataAvailable := DataAvailable;
  FDriver.OnSessionClosed := SessionClosed;
  FClient := TICS9_Client_PeerIO.Create(Self, Self);

  FAsyncConnecting := False;
  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;

  name := 'ICS9-Client';
end;

destructor TZNet_Client_ICS9.Destroy;
begin
  Disconnect;
  DisposeObject(FDriver);
  DisposeObject(FClient);
  inherited Destroy;
end;

procedure TZNet_Client_ICS9.TriggerDoConnectFailed;
begin
  inherited TriggerDoConnectFailed;

  try
    if Assigned(FOnAsyncConnectNotify_C) then
        FOnAsyncConnectNotify_C(False);
    if Assigned(FOnAsyncConnectNotify_M) then
        FOnAsyncConnectNotify_M(False);
    if Assigned(FOnAsyncConnectNotify_P) then
        FOnAsyncConnectNotify_P(False);
  except
  end;

  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;

  FDriver.OnSessionConnected := nil;

  FAsyncConnecting := False;
end;

procedure TZNet_Client_ICS9.TriggerDoConnectFinished;
begin
  inherited TriggerDoConnectFinished;

  try
    if Assigned(FOnAsyncConnectNotify_C) then
        FOnAsyncConnectNotify_C(True);
    if Assigned(FOnAsyncConnectNotify_M) then
        FOnAsyncConnectNotify_M(True);
    if Assigned(FOnAsyncConnectNotify_P) then
        FOnAsyncConnectNotify_P(True);
  except
  end;

  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;

  FDriver.OnSessionConnected := nil;

  FAsyncConnecting := False;
end;

procedure TZNet_Client_ICS9.AsyncConnectC(addr: SystemString; Port: Word; const OnResult: TOnState_C);
begin
  AsyncConnect(addr, Port, OnResult, nil, nil);
end;

procedure TZNet_Client_ICS9.AsyncConnectM(addr: SystemString; Port: Word; const OnResult: TOnState_M);
begin
  AsyncConnect(addr, Port, nil, OnResult, nil);
end;

procedure TZNet_Client_ICS9.AsyncConnectP(addr: SystemString; Port: Word; const OnResult: TOnState_P);
begin
  AsyncConnect(addr, Port, nil, nil, OnResult);
end;

function TZNet_Client_ICS9.Connect(Host, Port: SystemString; AWaitTimeOut: TTimeTick): Boolean;
var
  AStopTime: TTimeTick;
begin
  Disconnect;

  FDriver.OnSessionConnected := nil;
  FAsyncConnecting := False;
  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;

  FDriver.Proto := 'tcp';
  FDriver.Port := Port;
  FDriver.addr := Host;

  try
      FDriver.Connect;
  except
    Result := False;
    exit;
  end;

  AStopTime := GetTimeTick + AWaitTimeOut;

  while not(FDriver.State in [wsConnected]) do
    begin
      Progress;

      if (GetTimeTick >= AStopTime) then
          Break;
      if FDriver.State in [wsClosed] then
          Break;

      TCore_Thread.Sleep(1);
    end;

  Result := FDriver.State in [wsConnected];

  if Result then
      DoConnected(FClient);

  while (not RemoteInited) and (FDriver.State in [wsConnected]) do
    begin
      Progress;

      if (GetTimeTick >= AStopTime) then
        begin
          FDriver.Close;
          Break;
        end;
      if FDriver.State in [wsClosed] then
          Break;

      TCore_Thread.Sleep(1);
    end;

  Result := (RemoteInited);

  if Result then
      FClient.Print('client connected %s:%s', [FDriver.addr, FDriver.Port]);
end;

function TZNet_Client_ICS9.Connect(Host, Port: SystemString): Boolean;
begin
  Result := Connect(Host, Port, 5000);
end;

function TZNet_Client_ICS9.Connect(addr: SystemString; Port: Word): Boolean;
begin
  Result := Connect(addr, IntToStr(Port), 5000);
end;

procedure TZNet_Client_ICS9.Disconnect;
begin
  FDriver.Close;
  DisposeObject(FClient);
  FClient := TICS9_Client_PeerIO.Create(Self, Self);
end;

function TZNet_Client_ICS9.Connected: Boolean;
begin
  Result := (FDriver.State in [wsConnected]);
end;

function TZNet_Client_ICS9.ClientIO: TPeerIO;
begin
  Result := FClient;
end;

procedure TZNet_Client_ICS9.Progress;
begin
  FClient.Process_Send_Buffer();

  inherited Progress;

  try
      FDriver.ProcessMessages;
  except
  end;
end;

initialization

finalization

end.
