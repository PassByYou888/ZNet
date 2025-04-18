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
{ * Developer refrence Support                                                 * }
{ ****************************************************************************** }
(*
  update history
*)

unit Z.Net.Client.Refrence;

{$DEFINE FPC_DELPHI_MODE}
{$I ..\Z.Define.inc}

interface

uses SysUtils, Classes,
  Z.PascalStrings,
  Z.Net, Z.Core, Z.UnicodeMixedLib, Z.MemoryStream,
  Z.Notify;

type
  TClient_PeerIO = class(TPeerIO)
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

  TCommunicationFramework_Client_Refrence = class(TZNet_Client)
  public
    constructor Create; override;
    destructor Destroy; override;

    { selected ignore, TriggerDoConnectFailed provides callbacks for async connection failures }
    procedure TriggerDoConnectFailed; override;
    { selected ignore, TriggerDoConnectFinished provides callbacks for successful async connections }
    procedure TriggerDoConnectFinished; override;

    { select: Asynchronous connection, returns state by callback, and if the interface is ignored, the system uses blocking connections. }
    procedure AsyncConnectC(addr: SystemString; Port: Word; const OnResult: TOnState_C); override;
    { select: Asynchronous connection, returns state by callback, and if the interface is ignored, the system uses blocking connections. }
    procedure AsyncConnectM(addr: SystemString; Port: Word; const OnResult: TOnState_M); override;
    { select: Asynchronous connection, returns state by callback, and if the interface is ignored, the system uses blocking connections. }
    procedure AsyncConnectP(addr: SystemString; Port: Word; const OnResult: TOnState_P); override;
    { Core interface: Blocking connection, which must be made sure that the encryption protocol has been negotiated before the call returns to state, refer to CrossSocket or Indy's interface imp }
    function Connect(addr: SystemString; Port: Word): Boolean; override;

    { core interface: return connection state }
    function Connected: Boolean; override;
    { core interface: disconnect imp. }
    procedure Disconnect; override;
    { Core interface: returns the TPeerIO instance of the client. }
    function ClientIO: TPeerIO; override;
    { core interface: Kernel main loop }
    procedure Progress; override;
  end;

implementation


procedure TClient_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
end;

destructor TClient_PeerIO.Destroy;
begin
  inherited Destroy;
end;

function TClient_PeerIO.Connected: Boolean;
begin
  Result := True;
end;

procedure TClient_PeerIO.Disconnect;
begin
end;

procedure TClient_PeerIO.Write_IO_Buffer(const buff: PByte; const Size: nativeInt);
begin
  if not Connected then
      Exit;
end;

procedure TClient_PeerIO.WriteBufferOpen;
begin
end;

procedure TClient_PeerIO.WriteBufferFlush;
begin
end;

procedure TClient_PeerIO.WriteBufferClose;
begin
end;

function TClient_PeerIO.GetPeerIP: SystemString;
begin
  Result := '';
end;

function TClient_PeerIO.WriteBuffer_is_NULL: Boolean;
begin
  Result := True;
end;

procedure TClient_PeerIO.Progress;
begin
  inherited Progress;
  Process_Send_Buffer();
end;

constructor TCommunicationFramework_Client_Refrence.Create;
begin
  inherited Create;
end;

destructor TCommunicationFramework_Client_Refrence.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TCommunicationFramework_Client_Refrence.TriggerDoConnectFailed;
begin
  inherited TriggerDoConnectFailed;
end;

procedure TCommunicationFramework_Client_Refrence.TriggerDoConnectFinished;
begin
  inherited TriggerDoConnectFinished;
end;

procedure TCommunicationFramework_Client_Refrence.AsyncConnectC(addr: SystemString; Port: Word; const OnResult: TOnState_C);
begin
  inherited;
end;

procedure TCommunicationFramework_Client_Refrence.AsyncConnectM(addr: SystemString; Port: Word; const OnResult: TOnState_M);
begin
  inherited;
end;

procedure TCommunicationFramework_Client_Refrence.AsyncConnectP(addr: SystemString; Port: Word; const OnResult: TOnState_P);
begin
  inherited;
end;

function TCommunicationFramework_Client_Refrence.Connect(addr: SystemString; Port: Word): Boolean;
begin
  Result := False;
end;

function TCommunicationFramework_Client_Refrence.Connected: Boolean;
begin
  Result := (ClientIO <> nil) and (ClientIO.Connected);
end;

procedure TCommunicationFramework_Client_Refrence.Disconnect;
begin
  if Connected then
      ClientIO.Disconnect;
end;

function TCommunicationFramework_Client_Refrence.ClientIO: TPeerIO;
begin
  Result := nil;
end;

procedure TCommunicationFramework_Client_Refrence.Progress;
begin
  inherited Progress;
end;

initialization

finalization

end.
 
