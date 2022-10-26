{ ****************************************************************************** }
{ * CrossSocket support                                                        * }
{ ****************************************************************************** }
(*
  update history
*)
unit Z.Net.Client.CrossSocket;

{$I ..\Z.Define.inc}

interface

uses SysUtils, Classes,
  Z.Net.CrossSocket, Z.Net.SocketAPI, Z.Net.CrossSocket.Base,
  Z.PascalStrings,
  Z.Net.Server.CrossSocket,
  Z.Net, Z.Core, Z.UnicodeMixedLib, Z.MemoryStream,
  Z.Notify;

type
  TZNet_Client_CrossSocket = class;

  TCrossSocketClient_PeerIO = class(TCrossSocketServer_PeerIO)
  public
    OwnerClient: TZNet_Client_CrossSocket;
    procedure CreateAfter; override;
    destructor Destroy; override;
    procedure Disconnect; override;
  end;

  TZNet_Client_CrossSocket = class(TZNet_Client)
  private
    ClientIOIntf: TCrossSocketClient_PeerIO;

    FOnAsyncConnectNotify_C: TOnState_C;
    FOnAsyncConnectNotify_M: TOnState_M;
    FOnAsyncConnectNotify_P: TOnState_P;

    // fixed DCC < XE8
    procedure AsyncConnect__(addr: SystemString; Port: Word; OnResultCall: TOnState_C; OnResultMethod: TOnState_M; OnResultProc: TOnState_P);
  protected
    procedure DoConnected(Sender: TPeerIO); override;
    procedure DoDisconnect(Sender: TPeerIO); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure TriggerDoConnectFailed; override;
    procedure TriggerDoConnectFinished; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerIO; override;
    procedure TriggerQueueData(v: PQueueData); override;
    procedure Progress; override;

    procedure AsyncConnect(addr: SystemString; Port: Word); overload;
    procedure AsyncConnectC(addr: SystemString; Port: Word; const OnResult: TOnState_C); overload; override;
    procedure AsyncConnectM(addr: SystemString; Port: Word; const OnResult: TOnState_M); overload; override;
    procedure AsyncConnectP(addr: SystemString; Port: Word; const OnResult: TOnState_P); overload; override;

    function Connect(addr: SystemString; Port: Word): Boolean; overload; override;
    function Connect(Host: SystemString; Port: SystemString): Boolean; overload;
    procedure Disconnect; override;
  end;

  TGlobalCrossSocketClientPool = class
  private
    LastCompleted, LastResult: Boolean;
    LastConnection: ICrossConnection;
  public
    driver: TDriverEngine;
    AutoReconnect: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure CloseAllConnection;

    procedure DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
    procedure DoReceived(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
    procedure DoSendBuffResult(AConnection: ICrossConnection; ASuccess: Boolean);

    function BuildConnect(addr: SystemString; Port: Word; BuildIntf: TZNet_Client_CrossSocket): Boolean;
    procedure BuildAsyncConnect(addr: SystemString; Port: Word; BuildIntf: TZNet_Client_CrossSocket);
  end;

var
  ClientPool: TGlobalCrossSocketClientPool = nil;

implementation

procedure TCrossSocketClient_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  OwnerClient := nil;
end;

destructor TCrossSocketClient_PeerIO.Destroy;
begin
  if OwnerClient <> nil then
    begin
      OwnerClient.DoDisconnect(Self);
      OwnerClient.ClientIOIntf := nil;
    end;
  OwnerClient := nil;
  inherited Destroy;
end;

procedure TCrossSocketClient_PeerIO.Disconnect;
begin
  if OwnerClient <> nil then
    begin
      OwnerClient.DoDisconnect(Self);
      OwnerClient.ClientIOIntf := nil;
    end;
  OwnerClient := nil;
  inherited Disconnect;
end;

procedure TZNet_Client_CrossSocket.AsyncConnect__(addr: SystemString; Port: Word; OnResultCall: TOnState_C; OnResultMethod: TOnState_M; OnResultProc: TOnState_P);
begin
  FOnAsyncConnectNotify_C := OnResultCall;
  FOnAsyncConnectNotify_M := OnResultMethod;
  FOnAsyncConnectNotify_P := OnResultProc;

  ClientPool.BuildAsyncConnect(addr, Port, Self);
end;

procedure TZNet_Client_CrossSocket.DoConnected(Sender: TPeerIO);
begin
  inherited DoConnected(Sender);
end;

procedure TZNet_Client_CrossSocket.DoDisconnect(Sender: TPeerIO);
begin
  inherited DoDisconnect(Sender);
end;

constructor TZNet_Client_CrossSocket.Create;
begin
  inherited Create;
  EnabledAtomicLockAndMultiThread := False;
  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;
end;

destructor TZNet_Client_CrossSocket.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TZNet_Client_CrossSocket.TriggerDoConnectFailed;
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
end;

procedure TZNet_Client_CrossSocket.TriggerDoConnectFinished;
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
end;

function TZNet_Client_CrossSocket.Connected: Boolean;
begin
  Result := (ClientIO <> nil) and (ClientIO.Connected);
end;

function TZNet_Client_CrossSocket.ClientIO: TPeerIO;
begin
  Result := ClientIOIntf;
end;

procedure TZNet_Client_CrossSocket.TriggerQueueData(v: PQueueData);
begin
  if not Connected then
    begin
      DisposeQueueData(v);
      Exit;
    end;

  ClientIOIntf.PostQueueData(v);
  ClientIOIntf.Process_Send_Buffer();
end;

procedure TZNet_Client_CrossSocket.Progress;
begin
  inherited Progress;

  try
      Z.Core.CheckThreadSynchronize;
  except
  end;
end;

procedure TZNet_Client_CrossSocket.AsyncConnect(addr: SystemString; Port: Word);
begin
  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;

  ClientPool.BuildAsyncConnect(addr, Port, Self);
end;

procedure TZNet_Client_CrossSocket.AsyncConnectC(addr: SystemString; Port: Word; const OnResult: TOnState_C);
begin
  AsyncConnect__(addr, Port, OnResult, nil, nil);
end;

procedure TZNet_Client_CrossSocket.AsyncConnectM(addr: SystemString; Port: Word; const OnResult: TOnState_M);
begin
  AsyncConnect__(addr, Port, nil, OnResult, nil);
end;

procedure TZNet_Client_CrossSocket.AsyncConnectP(addr: SystemString; Port: Word; const OnResult: TOnState_P);
begin
  AsyncConnect__(addr, Port, nil, nil, OnResult);
end;

function TZNet_Client_CrossSocket.Connect(addr: SystemString; Port: Word): Boolean;
begin
  Result := ClientPool.BuildConnect(addr, Port, Self);
end;

function TZNet_Client_CrossSocket.Connect(Host: SystemString; Port: SystemString): Boolean;
begin
  Result := Connect(Host, umlStrToInt(Port, 0));
end;

procedure TZNet_Client_CrossSocket.Disconnect;
begin
  if Connected then
    begin
      ClientIO.Disconnect;
    end;

  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;
end;

constructor TGlobalCrossSocketClientPool.Create;
begin
  inherited Create;
  driver := TDriverEngine.Create(
{$IFDEF DEBUG}
    2
{$ELSE DEBUG}
    Z.Core.Get_Parallel_Granularity
{$ENDIF DEBUG}
    );
  driver.OnDisconnected := DoDisconnect;
  driver.OnReceived := DoReceived;

  AutoReconnect := False;

  ClientPool := Self;
end;

destructor TGlobalCrossSocketClientPool.Destroy;
begin
  try
      ICrossSocket(driver).DisconnectAll;
  except
  end;
  DisposeObject(driver);
  ClientPool := nil;
  inherited Destroy;
end;

procedure TGlobalCrossSocketClientPool.CloseAllConnection;
begin
  ICrossSocket(driver).CloseAllConnections;
end;

procedure TGlobalCrossSocketClientPool.DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
begin
  TCore_Thread.Synchronize(TCore_Thread.CurrentThread,
      procedure
    var
      p_io: TCrossSocketClient_PeerIO;
    begin
      if AConnection.UserObject is TCrossSocketClient_PeerIO then
        begin
          p_io := TCrossSocketClient_PeerIO(AConnection.UserObject);

          if p_io = nil then
              Exit;

          p_io.IOInterface := nil;

          if p_io.OwnerClient <> nil then
            begin
              try
                  DisposeObject(p_io);
              except
              end;
            end;

          AConnection.UserObject := nil;
        end;
    end);
end;

procedure TGlobalCrossSocketClientPool.DoReceived(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
var
  p_io: TCrossSocketClient_PeerIO;
begin
  if ALen <= 0 then
      Exit;
  if not(AConnection.UserObject is TCrossSocketClient_PeerIO) then
      Exit;

  p_io := TCrossSocketClient_PeerIO(AConnection.UserObject);

  if (p_io.IOInterface = nil) then
      Exit;

  p_io.Write_Physics_Fragment(aBuf, ALen);
end;

procedure TGlobalCrossSocketClientPool.DoSendBuffResult(AConnection: ICrossConnection; ASuccess: Boolean);
var
  p_io: TCrossSocketClient_PeerIO;
begin
  if not(AConnection.UserObject is TCrossSocketClient_PeerIO) then
      Exit;

  p_io := TCrossSocketClient_PeerIO(AConnection.UserObject);

  if p_io = nil then
      Exit;
  p_io.SendBuffResult(ASuccess);
end;

function TGlobalCrossSocketClientPool.BuildConnect(addr: SystemString; Port: Word; BuildIntf: TZNet_Client_CrossSocket): Boolean;
var
  dt: TTimeTick;
  p_io: TCrossSocketClient_PeerIO;
begin
  LastResult := False;
  LastCompleted := False;
  LastConnection := nil;

  if BuildIntf.ClientIOIntf <> nil then
      Z.Core.CheckThreadSynchronize(10);

  if BuildIntf.ClientIOIntf <> nil then
    begin
      try
        if BuildIntf.ClientIOIntf.Context <> nil then
            BuildIntf.ClientIOIntf.Context.Close;
      except
      end;
      while BuildIntf.ClientIOIntf <> nil do
        begin
          CheckThreadSynchronize(10);
          BuildIntf.Progress;
        end;
    end;

  ICrossSocket(driver).Connect(addr, Port,
    procedure(AConnection: ICrossConnection; ASuccess: Boolean)
    begin
      LastCompleted := True;
      LastResult := ASuccess;
      if LastResult then
          LastConnection := AConnection;
    end);

  TCore_Thread.Sleep(10);

  dt := GetTimeTick + 5000;
  while (not LastCompleted) and (GetTimeTick < dt) do
    begin
      BuildIntf.Progress;
      CheckThreadSynchronize(5);
    end;

  if LastResult then
    begin
      p_io := TCrossSocketClient_PeerIO.Create(BuildIntf, LastConnection.ConnectionIntf);
      p_io.OwnerClient := BuildIntf;
      LastConnection.UserObject := p_io;
      p_io.OwnerClient.ClientIOIntf := p_io;
      p_io.OnSendBackcall := DoSendBuffResult;
      BuildIntf.DoConnected(p_io);
    end;

  dt := GetTimeTick + 5000;
  while (LastCompleted) and (LastResult) and (not BuildIntf.RemoteInited) do
    begin
      BuildIntf.Progress;
      if GetTimeTick > dt then
        begin
          if LastConnection <> nil then
              LastConnection.Disconnect;
          Break;
        end;
    end;

  Result := BuildIntf.RemoteInited;

  if (not Result) and (AutoReconnect) then
      Result := BuildConnect(addr, Port, BuildIntf);
end;

procedure TGlobalCrossSocketClientPool.BuildAsyncConnect(addr: SystemString; Port: Word; BuildIntf: TZNet_Client_CrossSocket);
begin
  try
    if BuildIntf.ClientIOIntf <> nil then
        Z.Core.CheckThreadSynchronize(10);
    if BuildIntf.ClientIOIntf <> nil then
      begin
        try
          if BuildIntf.ClientIOIntf.Context <> nil then
              BuildIntf.ClientIOIntf.Context.Close;
        except
        end;

        while BuildIntf.ClientIOIntf <> nil do
          begin
            try
                BuildIntf.Progress;
            except
            end;
          end;
      end;
  except
    BuildIntf.TriggerDoConnectFailed;
    Exit;
  end;

  ICrossSocket(driver).Connect(addr, Port,
    procedure(AConnection: ICrossConnection; ASuccess: Boolean)
    begin
      if ASuccess then
        begin
          TCompute.SyncP(procedure
            var
              p_io: TCrossSocketClient_PeerIO;
            begin
              p_io := TCrossSocketClient_PeerIO.Create(BuildIntf, AConnection.ConnectionIntf);
              p_io.OwnerClient := BuildIntf;
              AConnection.UserObject := p_io;
              p_io.OwnerClient.ClientIOIntf := p_io;
              p_io.OnSendBackcall := DoSendBuffResult;
              BuildIntf.DoConnected(p_io);
            end);
        end
      else
        begin
          if AutoReconnect then
            begin
              BuildAsyncConnect(addr, Port, BuildIntf);
              Exit;
            end;
          BuildIntf.TriggerDoConnectFailed;
        end;
    end);
end;

initialization

TGlobalCrossSocketClientPool.Create;

finalization

DisposeObject(ClientPool);

end.
