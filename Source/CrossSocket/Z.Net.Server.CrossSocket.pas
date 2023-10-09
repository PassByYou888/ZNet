{ ****************************************************************************** }
{ * CrossSocket support                                                        * }
{ ****************************************************************************** }
(*
  CrossSocket Server的最大连接被限制到20000
  update history
*)

unit Z.Net.Server.CrossSocket;

{$I ..\Z.Define.inc}

interface

uses SysUtils, Classes,
  Z.Net.CrossSocket, Z.Net.SocketAPI, Z.Net.CrossSocket.Base, Z.Net.CrossServer,
  Z.PascalStrings, Z.UPascalStrings, Z.Status,
  Z.Net, Z.Core, Z.UnicodeMixedLib, Z.MemoryStream,
  Z.DFE;

type
  TCrossSocketServer_Mem_Order = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalOrderStruct<TMem64>;

  TCrossSocketServer_PeerIO = class(TPeerIO)
  public
    LastPeerIP: SystemString;
    Sending: Boolean;
    Internal_Send_Queue: TCrossSocketServer_Mem_Order;
    CurrentBuff: TMem64;
    LastSendingBuff: TMem64;
    OnSendBackcall: TProc<ICrossConnection, Boolean>;
    FSendCritical: TCritical;
    procedure CreateAfter; override;
    destructor Destroy; override;
    function Context: TCrossConnection;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure SendBuffResult(Success_: Boolean);
    procedure Write_IO_Buffer(const buff: PByte; const Size: NativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;
    function WriteBuffer_is_NULL: Boolean; override;
    function WriteBuffer_State(var WriteBuffer_Queue_Num, WriteBuffer_Size: Int64): Boolean; override;
    procedure Progress; override;
  end;

  TDriverEngine = TCrossSocket;

  TZNet_Server_CrossSocket = class(TZNet_Server)
  private
    FDriver: TDriverEngine;
    FStartedService: Boolean;
    FBindHost: SystemString;
    FBindPort: Word;
    FMaxConnection: Integer;
  protected
    procedure DoAccept(Sender: TObject; AListen: ICrossListen; var Accept: Boolean);
    procedure DoConnected(Sender: TObject; AConnection: ICrossConnection);
    procedure DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
    procedure DoReceived(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
    procedure DoSent(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
    procedure DoSendBuffResult(AConnection: ICrossConnection; Success_: Boolean);
  public
    constructor Create; override;
    constructor CreateTh(maxThPool: Word);
    destructor Destroy; override;

    function StartService(Host: SystemString; Port: Word): Boolean; override;
    procedure StopService; override;

    procedure Progress; override;

    function WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; TimeOut_: TTimeTick): SystemString; override;
    procedure WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDFE; TimeOut_: TTimeTick); override;

    property StartedService: Boolean read FStartedService;
    property driver: TDriverEngine read FDriver;
    property BindPort: Word read FBindPort;
    property BindHost: SystemString read FBindHost;
    property MaxConnection: Integer read FMaxConnection write FMaxConnection;
  end;

implementation

procedure TCrossSocketServer_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  LastPeerIP := '';
  Sending := False;
  Internal_Send_Queue := TCrossSocketServer_Mem_Order.Create;
  CurrentBuff := TMem64.Create;
  LastSendingBuff := nil;
  OnSendBackcall := nil;
  FSendCritical := TCritical.Create;
end;

destructor TCrossSocketServer_PeerIO.Destroy;
var
  c: TCrossConnection;
begin
  if IOInterface <> nil then
    begin
      c := Context;
      Context.UserObject := nil;
      IOInterface := nil;
      try
          c.Close;
      except
      end;
    end;

  while Internal_Send_Queue.Num > 0 do
    begin
      DisposeObject(Internal_Send_Queue.First^.Data);
      Internal_Send_Queue.Next;
    end;

  if LastSendingBuff <> nil then
      DisposeObjectAndNil(LastSendingBuff);

  DisposeObjectAndNil(CurrentBuff);
  DisposeObjectAndNil(Internal_Send_Queue);
  DisposeObjectAndNil(FSendCritical);

  inherited Destroy;
end;

function TCrossSocketServer_PeerIO.Context: TCrossConnection;
begin
  Result := IOInterface as TCrossConnection;
end;

function TCrossSocketServer_PeerIO.Connected: Boolean;
begin
  Result := (IOInterface <> nil) and (Context.ConnectStatus = TConnectStatus.csConnected);
end;

procedure TCrossSocketServer_PeerIO.Disconnect;
var
  c: TCrossConnection;
begin
  if IOInterface <> nil then
    begin
      c := Context;
      Context.UserObject := nil;
      IOInterface := nil;
      try
          c.Close;
      except
      end;
    end;
  DisposeObject(Self);
end;

procedure TCrossSocketServer_PeerIO.SendBuffResult(Success_: Boolean);
var
  c: TCrossConnection;
  Num: Integer;
begin
  try
    if LastSendingBuff <> nil then
      begin
        if FSendCritical <> nil then
            FSendCritical.Lock;
        DisposeObjectAndNil(LastSendingBuff);
        if FSendCritical <> nil then
            FSendCritical.UnLock;
      end;

    if (not Success_) then
      begin
        Sending := False;
        if IOInterface <> nil then
          begin
            c := Context;
            Context.UserObject := nil;
            IOInterface := nil;
          end;
        DelayFree();
        exit;
      end;

    if Connected then
      begin
        try
          UpdateLastCommunicationTime;
          FSendCritical.Lock;
          Num := Internal_Send_Queue.Num;
          FSendCritical.UnLock;

          if Num > 0 then
            begin
              FSendCritical.Lock;
              // 将发送队列拾取出来
              LastSendingBuff := Internal_Send_Queue.First^.Data;
              // 删除队列，下次回调时后置式释放
              Internal_Send_Queue.Next;

              if Context <> nil then
                begin
                  Context.SendBuf(LastSendingBuff.Memory, LastSendingBuff.Size, OnSendBackcall);
                  FSendCritical.UnLock;
                end
              else
                begin
                  FSendCritical.UnLock;
                  SendBuffResult(False);
                end;
            end
          else
            begin
              FSendCritical.Lock;
              Sending := False;
              FSendCritical.UnLock;
            end;
        except
          if IOInterface <> nil then
            begin
              c := Context;
              Context.UserObject := nil;
              IOInterface := nil;
            end;
          DelayClose();
        end;
      end
    else
      begin
        Sending := False;
      end;
  except
  end;
end;

procedure TCrossSocketServer_PeerIO.Write_IO_Buffer(const buff: PByte; const Size: NativeInt);
begin
  // 避免大量零碎数据消耗流量资源，碎片收集
  // 在flush中实现精确异步发送和校验
  if Size > 0 then
    begin
      FSendCritical.Lock;
      CurrentBuff.Position := CurrentBuff.Size;
      CurrentBuff.write(Pointer(buff)^, Size);
      FSendCritical.UnLock;
    end;
end;

procedure TCrossSocketServer_PeerIO.WriteBufferOpen;
begin
end;

procedure TCrossSocketServer_PeerIO.WriteBufferFlush;
begin
  if not Connected then
      exit;

  if CurrentBuff.Size = 0 then
      exit;

  FSendCritical.Lock;
  if Sending then
    begin
      Internal_Send_Queue.Push(CurrentBuff);
      CurrentBuff := TMem64.Create;
    end
  else
    begin
      if Internal_Send_Queue.Num = 0 then
          DisposeObjectAndNil(LastSendingBuff);

      Internal_Send_Queue.Push(CurrentBuff);
      CurrentBuff := TMem64.Create;
      LastSendingBuff := Internal_Send_Queue.First^.Data;
      Internal_Send_Queue.Next;
      Context.SendBuf(LastSendingBuff.Memory, LastSendingBuff.Size, OnSendBackcall);
    end;
  FSendCritical.UnLock;
end;

procedure TCrossSocketServer_PeerIO.WriteBufferClose;
begin
  WriteBufferFlush;
end;

function TCrossSocketServer_PeerIO.GetPeerIP: SystemString;
begin
  if Connected then
    begin
      Result := Context.PeerAddr;
      LastPeerIP := Result;
    end
  else
      Result := LastPeerIP;
end;

function TCrossSocketServer_PeerIO.WriteBuffer_is_NULL: Boolean;
begin
  FSendCritical.Lock;
  Result := (not Sending) and (Internal_Send_Queue.Num <= 0);
  FSendCritical.UnLock;
end;

function TCrossSocketServer_PeerIO.WriteBuffer_State(var WriteBuffer_Queue_Num, WriteBuffer_Size: Int64): Boolean;
var
  p: TCrossSocketServer_Mem_Order.POrderStruct;
begin
  FSendCritical.Lock;
  Result := Sending;
  WriteBuffer_Queue_Num := Internal_Send_Queue.Num;
  WriteBuffer_Size := CurrentBuff.Size;

  if Internal_Send_Queue.First <> nil then
    begin
      p := Internal_Send_Queue.First;
      repeat
        inc(WriteBuffer_Size, p^.Data.Size);
        p := p^.Next;
      until p <> nil;
    end;

  FSendCritical.UnLock;
end;

procedure TCrossSocketServer_PeerIO.Progress;
begin
  inherited Progress;
  Process_Send_Buffer();
end;

procedure TZNet_Server_CrossSocket.DoAccept(Sender: TObject; AListen: ICrossListen; var Accept: Boolean);
begin
  Accept := Count < FMaxConnection;
end;

procedure TZNet_Server_CrossSocket.DoConnected(Sender: TObject; AConnection: ICrossConnection);
begin
  TCompute.SyncP(procedure
    var
      p_io: TCrossSocketServer_PeerIO;
    begin
      p_io := TCrossSocketServer_PeerIO.Create(Self, AConnection.ConnectionIntf);
      AConnection.UserObject := p_io;
      p_io.OnSendBackcall := DoSendBuffResult;
    end);
end;

procedure TZNet_Server_CrossSocket.DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
begin
  if AConnection.UserObject is TCrossSocketServer_PeerIO then
    begin
      TCompute.SyncP(procedure
        var
          p_io: TCrossSocketServer_PeerIO;
        begin
          try
            p_io := TCrossSocketServer_PeerIO(AConnection.UserObject);
            if p_io <> nil then
              begin
                p_io.IOInterface := nil;
                AConnection.UserObject := nil;
                DisposeObject(p_io);
              end;
          except
          end;
        end);
    end;
end;

procedure TZNet_Server_CrossSocket.DoReceived(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
var
  p_io: TCrossSocketServer_PeerIO;
begin
  if ALen <= 0 then
      exit;

  try
    p_io := TCrossSocketServer_PeerIO(AConnection.UserObject);
    if (p_io = niL) or (p_io.IOInterface = nil) then
        exit;
    p_io.Write_Physics_Fragment(aBuf, ALen);
  except
  end;
end;

procedure TZNet_Server_CrossSocket.DoSent(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
begin
end;

procedure TZNet_Server_CrossSocket.DoSendBuffResult(AConnection: ICrossConnection; Success_: Boolean);
var
  p_io: TCrossSocketServer_PeerIO;
begin
  if AConnection.UserObject = nil then
      exit;

  p_io := TCrossSocketServer_PeerIO(AConnection.UserObject);
  if (p_io = niL) or (p_io.IOInterface = nil) then
      exit;

  p_io.SendBuffResult(Success_);
end;

constructor TZNet_Server_CrossSocket.Create;
begin
  CreateTh(
{$IFDEF DEBUG}
  2
{$ELSE DEBUG}
  Z.Core.Get_Parallel_Granularity
{$ENDIF DEBUG}
  );
end;

constructor TZNet_Server_CrossSocket.CreateTh(maxThPool: Word);
begin
  inherited Create;
  EnabledAtomicLockAndMultiThread := False;
  FDriver := TDriverEngine.Create(maxThPool);
  FDriver.OnAccept := DoAccept;
  FDriver.OnConnected := DoConnected;
  FDriver.OnDisconnected := DoDisconnect;
  FDriver.OnReceived := DoReceived;
  FDriver.OnSent := DoSent;
  FStartedService := False;
  FBindPort := 0;
  FBindHost := '';
  FMaxConnection := 20000;
  name:='Cross-Socket-Server';
end;

destructor TZNet_Server_CrossSocket.Destroy;
begin
  StopService;
  // CrossSocket使用了RTL的Synchronize机制,这是兼容UI的机制,在服务器领域这是非常蛋疼的东西
  // soft_synchronize_technology是Synchronize硬件仿真技术,用于DLL和静态库使用独立线程仿真RTL主线程,
  // 使用soft_synchronize_technology系技术,必须非常小心,同步队列一旦出问题都是传导型的问题
  // 如果开启了soft_synchronize_technology,这里必须同步一下
  // 同步的作用是清理IO同步事件,以免卡端口,导致PostQueuedCompletionStatus消息过去卡队列
  // 无论Used_Soft_Synchronize是否开启Check_Soft_Thread_Synchronize都会清理掉当前的UI同步队列.
  Check_Soft_Thread_Synchronize;
  try
      DisposeObject(FDriver);
  except
  end;
  inherited Destroy;
end;

function TZNet_Server_CrossSocket.StartService(Host: SystemString; Port: Word): Boolean;
var
  Completed, Successed: Boolean;
begin
  StopService;

  Completed := False;
  Successed := False;
  try
    ICrossSocket(FDriver).Listen(Host, Port,
      procedure(Listen: ICrossListen; Success_: Boolean)
      begin
        Completed := True;
        Successed := Success_;
      end);

    while not Completed do
        Check_Soft_Thread_Synchronize(5);

    FBindPort := Port;
    FBindHost := Host;
    Result := Successed;
    FStartedService := Result;
  except
      Result := False;
  end;
end;

procedure TZNet_Server_CrossSocket.StopService;
begin
  try
    try
        ICrossSocket(FDriver).CloseAll;
    except
    end;
    FStartedService := False;
  except
  end;
end;

procedure TZNet_Server_CrossSocket.Progress;
begin
  inherited Progress;
end;

function TZNet_Server_CrossSocket.WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; TimeOut_: TTimeTick): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport CrossSocket');
end;

procedure TZNet_Server_CrossSocket.WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDFE; TimeOut_: TTimeTick);
begin
  RaiseInfo('WaitSend no Suppport CrossSocket');
end;

initialization

finalization

end.
