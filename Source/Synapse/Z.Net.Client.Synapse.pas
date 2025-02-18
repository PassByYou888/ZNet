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
{ * FPC Synpase client Support                                                 * }
{ ****************************************************************************** }
(*
  synapse客户端是强行将阻塞模型模拟成异步模型
*)

unit Z.Net.Client.Synapse;

{$DEFINE FPC_DELPHI_MODE}
{$I ..\Z.Define.inc}

interface

uses SysUtils, Classes,
  Z.PascalStrings, Z.UPascalStrings, Z.Net, Z.Core, Z.UnicodeMixedLib, Z.MemoryStream, Z.Notify,
  Z.synsock, Z.blcksock;

type
  TZNet_Client_Synapse = class;

  // 带锁队列容器,装待发数据
  TSynapseClient_Send_Buffer_Queue = class(TCritical_Big_Object_List<TMem64>)
  public
    constructor Create;
  end;

  TSynapseClient_PeerIO = class(TPeerIO)
  protected
    LastPeerIP: SystemString;
    SendBuffQueue: TSynapseClient_Send_Buffer_Queue;
    CurrentBuff: TMem64;
  public
    procedure CreateAfter; override;
    destructor Destroy; override;

    function Context: TZNet_Client_Synapse;

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

  TZNet_Client_Synapse = class(TZNet_Client)
  private
    Sock: TTCPBlockSocket;
    InternalClient: TSynapseClient_PeerIO;
    SockConnected: Boolean;
    Synapse_Th_Activted, Synapse_Th_Runing: Boolean;
    // synapse的物理收发流程转移到线程运行,不再使用主线程跑收发,已在万兆环境用70路1080p测试通过,synapse表现优于ics,diocp,仅次于crosssocket
    // 不确定全部iot设备(包括wiki设备),可以支持在阻塞通讯下用双工模式同时收发,因此只开了一个线用阻塞模拟跑异步
    // synapse线程发送机制: 先检查发送缓冲,发现有数据按一个循环周期1M发送,阻塞发送带宽不够,会卡发送周期,在低带宽iot设备,自行调低即可
    // synapse线程检查接收缓冲机制:在发送周期完结后,进入接收周期,发现有数据,就启动持续接收,每个循环周期接收最大时长为0.5秒
    // 如果iot设备可以跑阻塞双工(大部分wiki设备是假双工),就自己改代码把
    // 思路是开两个线程,一个发送,一个接收,然后用状态机控制双线程,这样干可以在高流量环境提速20%
    // 2023-10-6 by.qq600585
    procedure Do_Synapse_Th();
    procedure Begin_Synapse_Th;
    procedure End_Synapse_Th;
  protected
    procedure DoConnected(Sender: TPeerIO); override;
    procedure DoDisconnect(Sender: TPeerIO); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerIO; override;
    procedure Progress; override;

    function Connect(addr: SystemString; Port: Word): Boolean; overload; override;
    function Connect(Host: SystemString; Port: SystemString): Boolean; overload;
    procedure Disconnect; override;
  end;

implementation

constructor TSynapseClient_Send_Buffer_Queue.Create;
begin
  inherited Create(True);
end;

procedure TSynapseClient_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  LastPeerIP := '';
  SendBuffQueue := TSynapseClient_Send_Buffer_Queue.Create;
  CurrentBuff := TMem64.CustomCreate(8192);
end;

destructor TSynapseClient_PeerIO.Destroy;
begin
  DisposeObject(SendBuffQueue);
  DisposeObject(CurrentBuff);
  inherited Destroy;
end;

function TSynapseClient_PeerIO.Context: TZNet_Client_Synapse;
begin
  Result := OwnerFramework as TZNet_Client_Synapse;
end;

function TSynapseClient_PeerIO.Connected: Boolean;
begin
  Result := Context.SockConnected;
end;

procedure TSynapseClient_PeerIO.Disconnect;
begin
  Context.Disconnect;
end;

procedure TSynapseClient_PeerIO.Write_IO_Buffer(const buff: PByte; const Size: nativeInt);
begin
  if not Connected then
      Exit;

  if Size > 0 then
      CurrentBuff.write(Pointer(buff)^, Size);
end;

procedure TSynapseClient_PeerIO.WriteBufferOpen;
begin
  if not Connected then
      Exit;
  CurrentBuff.Clear;
end;

procedure TSynapseClient_PeerIO.WriteBufferFlush;
begin
  if not Connected then
      Exit;
  if CurrentBuff.Size > 0 then
    begin
      SendBuffQueue.Add(CurrentBuff);
      CurrentBuff := TMem64.CustomCreate(8192);
    end;
end;

procedure TSynapseClient_PeerIO.WriteBufferClose;
begin
  if not Connected then
      Exit;
  CurrentBuff.Clear;
end;

function TSynapseClient_PeerIO.GetPeerIP: SystemString;
begin
  if Connected then
    begin
      Result := Context.Sock.GetRemoteSinIP;
      LastPeerIP := Result;
    end
  else
      Result := LastPeerIP;
end;

function TSynapseClient_PeerIO.WriteBuffer_is_NULL: Boolean;
begin
  Result := SendBuffQueue.Count = 0;
end;

procedure TSynapseClient_PeerIO.Progress;
begin
  inherited Progress;
  Process_Send_Buffer();
end;

procedure TZNet_Client_Synapse.Do_Synapse_Th;
const
  MemSiz: Integer = 64 * 1024;
var
  tmp: TMem64;
  buff: Pointer;
  siz: Integer;
  Total_Recv: Integer;
  ReadTimeout_: TTimeTick;
begin
  buff := System.GetMemory(MemSiz);
  try
    while Synapse_Th_Activted do
      begin
        try
          // 检查发送物理缓冲区,发现有数据,一直发
          siz := 0;
          while (InternalClient.SendBuffQueue.num > 0) and (siz < 1024 * 1024) do
            begin
              tmp := InternalClient.SendBuffQueue.First^.Data.Swap_To_New_Instance;
              InternalClient.SendBuffQueue.Next;
              try
                if tmp.Size > 0 then
                    Sock.SendBuffer(tmp.Memory, tmp.Size);
              except
                SockConnected := False;
                PostProgress.PostExecuteM_NP(0, Disconnect);
                DisposeObject(tmp);
                Exit;
              end;
              inc(siz, tmp.Size);
              DisposeObject(tmp);
            end;

          // 检查接收缓冲区,如果持续有数据,循环接收0.5秒
          ReadTimeout_ := GetTimeTick() + 500;
          Total_Recv := 0;
          repeat
            try
              if Sock.CanRead(1) then
                  siz := Sock.RecvBuffer(buff, MemSiz)
              else
                  siz := 0;
            except
              SockConnected := False;
              PostProgress.PostExecuteM_NP(0, Disconnect);
              Exit;
            end;

            if Sock.LastError <> 0 then
              begin
                SockConnected := False;
                PostProgress.PostExecuteM_NP(0, Disconnect);
                Exit;
              end;

            inc(Total_Recv, siz);

            if (siz > 0) then
                InternalClient.Write_Physics_Fragment(buff, siz);
          until (siz <= 0) or (GetTimeTick() > ReadTimeout_);

          // 如果本次收到数据彻底完结,并且待发缓冲区是空就让cpu空置1ms
          if (siz <= 0) and (InternalClient.SendBuffQueue.num <= 0) then
              TCompute.Sleep(1);
        except
          SockConnected := False;
          PostProgress.PostExecuteM_NP(0, Disconnect);
          Exit;
        end;
      end;
  finally
    System.FreeMemory(buff);
    Synapse_Th_Activted := False;
  end;
end;

procedure TZNet_Client_Synapse.Begin_Synapse_Th;
begin
  End_Synapse_Th;
  Synapse_Th_Activted := True;
  TCompute.RunM_NP(Do_Synapse_Th, @Synapse_Th_Runing, nil);
end;

procedure TZNet_Client_Synapse.End_Synapse_Th;
begin
  Synapse_Th_Activted := False;
  while Synapse_Th_Runing do
      TCompute.Sleep(1);
end;

procedure TZNet_Client_Synapse.DoConnected(Sender: TPeerIO);
begin
  inherited DoConnected(Sender);
end;

procedure TZNet_Client_Synapse.DoDisconnect(Sender: TPeerIO);
begin
  inherited DoDisconnect(Sender);
end;

constructor TZNet_Client_Synapse.Create;
begin
  inherited Create;
  EnabledAtomicLockAndMultiThread := False;

  Sock := TTCPBlockSocket.Create;
  Sock.Family := TSocketFamily.SF_IP4;
  Sock.CreateSocket;
  InternalClient := TSynapseClient_PeerIO.Create(Self, Sock);
  SockConnected := False;
  Synapse_Th_Activted := False;
  Synapse_Th_Runing := False;
  name := 'Synapse-Client';
end;

destructor TZNet_Client_Synapse.Destroy;
begin
  End_Synapse_Th;
  Sock.CloseSocket;
  DisposeObject(InternalClient);
  DisposeObject(Sock);
  inherited Destroy;
end;

function TZNet_Client_Synapse.Connected: Boolean;
begin
  Result := SockConnected;
end;

function TZNet_Client_Synapse.ClientIO: TPeerIO;
begin
  Result := InternalClient;
end;

procedure TZNet_Client_Synapse.Progress;
begin
  inherited Progress;
end;

function TZNet_Client_Synapse.Connect(addr: SystemString; Port: Word): Boolean;
var
  AStopTime: TTimeTick;
begin
  Result := False;
  try
    Sock.Connect(addr, IntToStr(Port));
    SockConnected := Sock.LastError = 0;
    if not SockConnected then
      begin
        Sock.CloseSocket;
        if Sock.Family = TSocketFamily.SF_IP4 then
            Sock.Family := TSocketFamily.SF_IP6
        else
            Sock.Family := TSocketFamily.SF_IP4;
        Sock.CreateSocket;
        Sock.Connect(addr, IntToStr(Port));
        SockConnected := Sock.LastError = 0;
        if not SockConnected then
            Exit;
      end;

    Begin_Synapse_Th;
    DoConnected(InternalClient);

    AStopTime := GetTimeTick + 5000;

    while (not RemoteInited) and Connected do
      begin
        Progress;
        if GetTimeTick > AStopTime then
          begin
            Disconnect;
            Exit;
          end;
      end;

    Result := RemoteInited;
  except
      Result := False;
  end;
end;

function TZNet_Client_Synapse.Connect(Host: SystemString; Port: SystemString): Boolean;
begin
  Result := Connect(Host, umlStrToInt(Port, 0));
end;

procedure TZNet_Client_Synapse.Disconnect;
begin
  End_Synapse_Th;
  if SockConnected then
      DoDisconnect(InternalClient);
  SockConnected := False;
  Sock.CloseSocket;
  Sock.CreateSocket;
  DisposeObject(InternalClient);
  InternalClient := TSynapseClient_PeerIO.Create(Self, Sock);
end;

initialization

finalization

end.
 
