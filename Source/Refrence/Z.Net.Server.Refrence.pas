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
{ * Developer refrence Support                                                 * }
{ ****************************************************************************** }
(*
  update history
*)

unit Z.Net.Server.Refrence;

{$DEFINE FPC_DELPHI_MODE}
{$I ..\Z.Define.inc}

interface

uses SysUtils, Classes,
  Z.PascalStrings,
  Z.Net, Z.Core, Z.UnicodeMixedLib, Z.MemoryStream, Z.DFE;

type
  TServer_PeerIO = class(TPeerIO)
  public
    procedure CreateAfter; override;
    destructor Destroy; override;

    { core interface: return connection state }
    function Connected: Boolean; override;

    { core interface: disconnect imp. }
    procedure Disconnect; override;

    { core interface: kernel triggers when sending data. }
    procedure Write_IO_Buffer(const buff: PByte; const Size: nativeInt); override;
    { core interface: kernel will do WriteBufferOpen before sending data. }
    procedure WriteBufferOpen; override;
    { core interface: kernel will do WriteBufferFlush after sending data. }
    procedure WriteBufferFlush; override;
    { core interface: kernel will do WriteBufferClose after sending a batch of data. }
    procedure WriteBufferClose; override;

    { core interface: get the IP information. }
    function GetPeerIP: SystemString; override;

    { select: If your data is in memory and wait been sent, it returns to False. }
    { select: if you do not consider high concurrency optimization, you can ignore the interface. }
    function WriteBuffer_is_NULL: Boolean; override;

    { select: Kernel main loop, you can do ignore the interface }
    procedure Progress; override;
  end;

  TCommunicationFramework_Server_Refrence = class(TZNet_Server)
  private
  protected
  public
    constructor Create; override;
    destructor Destroy; override;

    { core interface: The StartService can be Bluetooth, TCP, UDP, rs232 com, GPIO, anywhere Communication interace. }
    function StartService(Host: SystemString; Port: Word): Boolean; override;
    { core interface: StopService. }
    procedure StopService; override;

    { core interface: Kernel main loop, you can do ignore the interface }
    procedure Progress; override;

    { select: recommended no used blocking communication calls on the server, unstable!! }
    function WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; TimeOut_: TTimeTick): SystemString; override;
    { select: recommended no used blocking communication calls on the server, unstable!! }
    procedure WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDFE; TimeOut_: TTimeTick); override;
  end;

implementation

procedure TServer_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
end;

destructor TServer_PeerIO.Destroy;
begin
  inherited Destroy;
end;

function TServer_PeerIO.Connected: Boolean;
begin
  Result := True;
end;

procedure TServer_PeerIO.Disconnect;
begin
end;

procedure TServer_PeerIO.Write_IO_Buffer(const buff: PByte; const Size: nativeInt);
begin
  if not Connected then
      Exit;
end;

procedure TServer_PeerIO.WriteBufferOpen;
begin
end;

procedure TServer_PeerIO.WriteBufferFlush;
begin
end;

procedure TServer_PeerIO.WriteBufferClose;
begin
end;

function TServer_PeerIO.GetPeerIP: SystemString;
begin
  Result := '';
end;

function TServer_PeerIO.WriteBuffer_is_NULL: Boolean;
begin
  Result := True;
end;

procedure TServer_PeerIO.Progress;
begin
  inherited Progress;
  Process_Send_Buffer();
end;

constructor TCommunicationFramework_Server_Refrence.Create;
begin
  inherited Create;
end;

destructor TCommunicationFramework_Server_Refrence.Destroy;
begin
  StopService;
  inherited Destroy;
end;

function TCommunicationFramework_Server_Refrence.StartService(Host: SystemString; Port: Word): Boolean;
begin
  Result := False;
end;

procedure TCommunicationFramework_Server_Refrence.StopService;
begin
end;

procedure TCommunicationFramework_Server_Refrence.Progress;
begin
  inherited Progress;
end;

function TCommunicationFramework_Server_Refrence.WaitSendConsoleCmd(p_io: TPeerIO;
  const Cmd, ConsoleData: SystemString; TimeOut_: TTimeTick): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport');
end;

procedure TCommunicationFramework_Server_Refrence.WaitSendStreamCmd(p_io: TPeerIO;
  const Cmd: SystemString; StreamData, ResultData: TDFE; TimeOut_: TTimeTick);
begin
  RaiseInfo('WaitSend no Suppport');
end;

initialization

finalization

end.
 
