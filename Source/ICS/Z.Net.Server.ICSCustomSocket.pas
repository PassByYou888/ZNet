{ ****************************************************************************** }
{ * ics support                                                                * }
{ ****************************************************************************** }
(*
  update history
*)
unit Z.Net.Server.ICSCustomSocket;

{$I ..\Z.Define.inc}

interface

uses
  Messages, Windows,
  SysUtils, Classes,
  Z.OverbyteIcsWSocket, Z.OverbyteIcsWinsock;

type
  TCustomICS = class(TWSocket)
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
