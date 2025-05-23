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
{ * ics support                                                                * }
{ ****************************************************************************** }
(*
  update history
*)
unit Z.Net.Server.ICS9CustomSocket;

{$I ..\Z.Define.inc}

interface

uses
  Messages, Windows,
  SysUtils, Classes,
  Z.ICS9.OverbyteIcsWSocket, Z.ICS9.OverbyteIcsWinsock;

type
  TCustom_ICS9 = class(TWSocket)
  public
  end;

function WSAInfo: string;

function WSAIPList: TStrings;

procedure ProcessICSMessages;

implementation

function WSAInfo: string;
var
  _D: TWSADATA;
  ipLst: TStrings;
begin
  _D := WinsockInfo;

  ipLst := LocalIPList(TSocketFamily.sfAny);

  Result := Format('Version:%D' + #13#10 + 'High Version:%D' + #13#10 + 'Description:%S' + #13#10 + 'System Status:%S' + #13#10 + 'Vendor Information:%S' + #13#10 +
    'Max Sockets:%D' + #13#10 + 'Max UDP:%D' + #13#10 + 'local host name:%s' + #13#10 + 'Local IP list:' + #13#10 + '%s', [
    _D.wVersion, _D.wHighVersion,
    StrPas(_D.szDescription),
    StrPas(_D.szSystemStatus),
    StrPas(_D.lpVendorInfo),
    _D.iMaxSockets,
    _D.iMaxUdpDg,
    LocalHostName,
    ipLst.Text]);

end;

function WSAIPList: TStrings;
begin
  Result := LocalIPList(TSocketFamily.sfAny);
end;

var
  ICSMessageProcessing: Boolean = False;

procedure ProcessICSMessages;
var
  Msg: TMsg;
begin
  if ICSMessageProcessing then
      Exit;

  ICSMessageProcessing := True;
  try
    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
      begin
        try
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        except
        end;
      end;
  except
  end;
  ICSMessageProcessing := False;
end;

end.
 
