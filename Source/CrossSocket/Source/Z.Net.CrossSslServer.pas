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
{******************************************************************************}
{                                                                              }
{       Delphi cross platform socket library                                   }
{                                                                              }
{       Copyright (c) 2017 WiNDDRiVER(soulawing@gmail.com)                     }
{                                                                              }
{       Homepage: https://github.com/winddriver/Delphi-Cross-Socket            }
{                                                                              }
{******************************************************************************}
unit Z.Net.CrossSslServer;

// winddriver�Ǹ�ϲ���ѳ���ģ��д���ļһ�Ⲣ����һ����ϰ�ߣ�cross�Ĵ���һ�������⣬�ǳ��Ѹ�
// �������ȫ�ֶ��������Чͳһ���Ż��������������еĿ⣬�����û��Լ��ڹ��̽��Ŀⶼ����һ��ȫ�ֶ���
{$I ..\..\Z.Define.inc}

interface

uses
  System.SysUtils,
  Z.Net.SocketAPI,
  Z.Net.CrossSocket.Base,
  Z.Net.CrossSslSocket;

type
  ICrossSslServer = interface(ICrossSslSocket)
  ['{DAEB2898-1EC4-4BCF-9BEB-078B582173AB}']
    function GetAddr: string;
    function GetPort: Word;
    function GetActive: Boolean;

    procedure SetAddr(const Value: string);
    procedure SetPort(const Value: Word);
    procedure SetActive(const Value: Boolean);

    procedure Start(const ACallback: TProc<Boolean> = nil);
    procedure Stop;

    property Addr: string read GetAddr write SetAddr;
    property Port: Word read GetPort write SetPort;

    property Active: Boolean read GetActive write SetActive;
  end;

  TCrossSslServer = class(TCrossSslSocket, ICrossSslServer)
  private
    FPort: Word;
    FAddr: string;
    FStarted: Integer;

    function GetAddr: string;
    function GetPort: Word;
    function GetActive: Boolean;

    procedure SetAddr(const Value: string);
    procedure SetPort(const Value: Word);
    procedure SetActive(const Value: Boolean);
  public
    constructor Create(AIoThreads: Integer); override;

    procedure Start(const ACallback: TProc<Boolean> = nil);
    procedure Stop;

    property Addr: string read GetAddr write SetAddr;
    property Port: Word read GetPort write SetPort;
    property Active: Boolean read GetActive write SetActive;
  end;

implementation

{ TCrossSslServer }

constructor TCrossSslServer.Create(AIoThreads: Integer);
begin
  inherited;

  InitSslCtx(SSLv23_server);
end;

function TCrossSslServer.GetActive: Boolean;
begin
  Result := (AtomicCmpExchange(FStarted, 0, 0) = 1);
end;

function TCrossSslServer.GetAddr: string;
begin
  Result := FAddr;
end;

function TCrossSslServer.GetPort: Word;
begin
  Result := FPort;
end;

procedure TCrossSslServer.SetActive(const Value: Boolean);
begin
  if Value then
    Start
  else
    Stop;
end;

procedure TCrossSslServer.SetAddr(const Value: string);
begin
  FAddr := Value;
end;

procedure TCrossSslServer.SetPort(const Value: Word);
begin
  FPort := Value;
end;

procedure TCrossSslServer.Start(const ACallback: TProc<Boolean>);
begin
  if (AtomicExchange(FStarted, 1) = 1) then
  begin
    if Assigned(ACallback) then
      ACallback(False);

    Exit;
  end;

  StartLoop;

  Listen(FAddr, FPort,
    procedure(AListen: ICrossListen; ASuccess: Boolean)
    begin
      if not ASuccess then
        AtomicExchange(FStarted, 0);

      // ����Ǽ���������˿�
      // ���ڼ����ɹ�֮��ʵ�ʵĶ˿�ȡ����
      if (FPort = 0) then
        FPort := AListen.LocalPort;

      if Assigned(ACallback) then
        ACallback(ASuccess);
    end);
end;

procedure TCrossSslServer.Stop;
begin
  CloseAll;
  StopLoop;
  AtomicExchange(FStarted, 0);
end;

end.
 