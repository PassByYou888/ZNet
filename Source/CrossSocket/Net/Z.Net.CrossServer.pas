{******************************************************************************}
{                                                                              }
{       Delphi cross platform socket library                                   }
{                                                                              }
{       Copyright (c) 2017 WiNDDRiVER(soulawing@gmail.com)                     }
{                                                                              }
{       Homepage: https://github.com/winddriver/Delphi-Cross-Socket            }
{                                                                              }
{******************************************************************************}
unit Z.Net.CrossServer;

// winddriver�Ǹ�ϲ���ѳ���ģ��д���ļһ�Ⲣ����һ����ϰ�ߣ�cross�Ĵ���һ�������⣬�ǳ��Ѹ�
// �������ȫ�ֶ��������Чͳһ���Ż��������������еĿ⣬�����û��Լ��ڹ��̽��Ŀⶼ����һ��ȫ�ֶ���
{$I ..\..\Z.Define.inc}

interface

uses
  System.SysUtils,
  Z.Net.SocketAPI,
  Z.Net.CrossSocket.Base,
  Z.Net.CrossSocket;

type
  ICrossServer = interface(ICrossSocket)
  ['{78865955-48EE-4354-9B03-389025839B67}']
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

  TCrossServer = class(TCrossSocket, ICrossServer)
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
    procedure Start(const ACallback: TProc<Boolean> = nil);
    procedure Stop;

    property Addr: string read GetAddr write SetAddr;
    property Port: Word read GetPort write SetPort;
    property Active: Boolean read GetActive write SetActive;
  end;

implementation

{ TCrossServer }

function TCrossServer.GetActive: Boolean;
begin
  Result := (AtomicCmpExchange(FStarted, 0, 0) = 1);
end;

function TCrossServer.GetAddr: string;
begin
  Result := FAddr;
end;

function TCrossServer.GetPort: Word;
begin
  Result := FPort;
end;

procedure TCrossServer.SetActive(const Value: Boolean);
begin
  if Value then
    Start
  else
    Stop;
end;

procedure TCrossServer.SetAddr(const Value: string);
begin
  FAddr := Value;
end;

procedure TCrossServer.SetPort(const Value: Word);
begin
  FPort := Value;
end;

procedure TCrossServer.Start(const ACallback: TProc<Boolean>);
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

procedure TCrossServer.Stop;
begin
  CloseAll;
  StopLoop;
  AtomicExchange(FStarted, 0);
end;

end.
