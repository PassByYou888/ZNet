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
{ * ics support                                                                * }
{ ****************************************************************************** }
(*
  ICS Server的最大连接被限制到500
  update history
*)
unit Z.Net.Server.ICS;

{$I ..\Z.Define.inc}

interface

uses Windows, SysUtils, Classes, Messages,
  Z.OverbyteIcsWSocket,
  Z.PascalStrings, Z.UPascalStrings, Z.Core, Z.MemoryStream,
  Z.Net.Server.ICSCustomSocket,
  Z.Net, Z.Status, Z.DFE;

type
  TICSServer_PeerIO = class(TPeerIO)
  public
    Context: TCustomICS;
    SendBuffer: TMS64;

    procedure ClientDataAvailable(Sender: TObject; Error: Word);
    procedure ClientSessionClosed(Sender: TObject; Error: Word);

    procedure CreateAfter; override;
    destructor Destroy; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure Write_IO_Buffer(const buff: PByte; const Size: NativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;
    function WriteBuffer_is_NULL: Boolean; override;
    procedure Progress; override;
  end;

  TZNet_Server_ICS = class(TZNet_Server)
  private
    Driver: TCustomICS;
    procedure SessionAvailable(Sender: TObject; ErrCode: Word);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure StopService; override;
    function StartService(Host: SystemString; Port: Word): Boolean; override;

    procedure Progress; override;

    function WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; TimeOut_: TTimeTick): SystemString; override;
    procedure WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDFE; TimeOut_: TTimeTick); override;
  end;

implementation

procedure TICSServer_PeerIO.ClientDataAvailable(Sender: TObject; Error: Word);
var
  BuffCount: Integer;
  buff: PByte;
begin
  // increment receive
  BuffCount := Context.RcvdCount;
  if BuffCount <= 0 then
      BuffCount := 255 * 255;
  buff := System.GetMemory(BuffCount);
  BuffCount := Context.Receive(buff, BuffCount);
  if BuffCount > 0 then
    begin
      Write_Physics_Fragment(buff, BuffCount);
    end;
  System.FreeMemory(buff);
end;

procedure TICSServer_PeerIO.ClientSessionClosed(Sender: TObject; Error: Word);
begin
  DisposeObject(Self);
end;

procedure TICSServer_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  SendBuffer := TMS64.CustomCreate(8192);
  Context := TCustomICS.Create(nil);
  Context.OnDataAvailable := ClientDataAvailable;
  Context.OnSessionClosed := ClientSessionClosed;
end;

destructor TICSServer_PeerIO.Destroy;
begin
  DisposeObject(Context);
  DisposeObject(SendBuffer);
  inherited Destroy;
end;

function TICSServer_PeerIO.Connected: Boolean;
begin
  Result := (Context.State in [wsConnected]);
end;

procedure TICSServer_PeerIO.Disconnect;
begin
  Context.OnSessionClosed := nil;
  Context.OnSessionAvailable := nil;
  Context.Close;
  DisposeObject(Self);
end;

procedure TICSServer_PeerIO.Write_IO_Buffer(const buff: PByte; const Size: NativeInt);
begin
  SendBuffer.WritePtr(buff, Size);
end;

procedure TICSServer_PeerIO.WriteBufferOpen;
begin
  SendBuffer.Clear;
end;

procedure TICSServer_PeerIO.WriteBufferFlush;
begin
  Context.Send(SendBuffer.Memory, SendBuffer.Size);
  SendBuffer.Clear;
end;

procedure TICSServer_PeerIO.WriteBufferClose;
begin
  SendBuffer.Clear;
end;

function TICSServer_PeerIO.GetPeerIP: SystemString;
begin
  if Context <> nil then
      Result := Context.PeerAddr
  else
      Result := '';
end;

function TICSServer_PeerIO.WriteBuffer_is_NULL: Boolean;
begin
  Result := True;
end;

procedure TICSServer_PeerIO.Progress;
begin
  inherited Progress;
  Process_Send_Buffer();
end;

procedure TZNet_Server_ICS.SessionAvailable(Sender: TObject; ErrCode: Word);
var
  p_io: TICSServer_PeerIO;
begin
  if Count < 500 then
    begin
      p_io := TICSServer_PeerIO.Create(Self, nil);
      p_io.Context.HSocket := Driver.Accept;
      p_io.Context.KeepAliveOnOff := TSocketKeepAliveOnOff.wsKeepAliveOnCustom;
      p_io.Context.KeepAliveTime := 1 * 1000;
      p_io.Context.KeepAliveInterval := 1 * 1000;
    end;
end;

constructor TZNet_Server_ICS.Create;
begin
  inherited Create;
  EnabledAtomicLockAndMultiThread := False;

  Driver := TCustomICS.Create(nil);
  Driver.MultiThreaded := False;

  // client interface
  Driver.OnSessionAvailable := SessionAvailable;

  name := 'ICS-Server';
end;

destructor TZNet_Server_ICS.Destroy;
begin
  StopService;
  Check_Soft_Thread_Synchronize(1, False);
  try
      DisposeObject(Driver);
  except
  end;
  inherited Destroy;
end;

procedure TZNet_Server_ICS.StopService;
begin
  while Count > 0 do
    begin
      ProgressPeerIOP(procedure(cli: TPeerIO)
        begin
          cli.Disconnect;
        end);
      Progress;
      Check_Soft_Thread_Synchronize(1, False);
    end;

  try
    Check_Soft_Thread_Synchronize(1, False);
    Driver.Close;
  except
  end;
end;

function TZNet_Server_ICS.StartService(Host: SystemString; Port: Word): Boolean;
begin
  try
    // open listen
    Driver.Proto := 'tcp';
    Driver.Port := IntToStr(Port);
    Driver.addr := Host;

    Driver.Listen;
    Result := True;
  except
      Result := False;
  end;
end;

procedure TZNet_Server_ICS.Progress;
begin
  try
      Driver.ProcessMessages;
  except
  end;
  inherited Progress;
end;

function TZNet_Server_ICS.WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; TimeOut_: TTimeTick): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport ICSServer');
end;

procedure TZNet_Server_ICS.WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDFE; TimeOut_: TTimeTick);
begin
  RaiseInfo('WaitSend no Suppport ICSServer');
end;

initialization

finalization

end.
 
