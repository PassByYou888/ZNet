{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  SSL Utility functions, TOcspHttpH.
Creation:     Nov 2023
Version:      V9.4
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2024 by Angus Robertson, Magenta Systems Ltd,
              Croydon, England. delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.


Overview
--------

TOcspHttp
---------
Component to support OCSP (Online Certificate Status Protocol) that replaces
CRL (Certificate Revocation Lists) as the way to confirm SSL/TLS certificates
are legitimate and not revoked for security reasons.  The component will check
an OCSP stapled response sent by a server during an SSL/TLS handshake or use
HTTP to contact the certificate issuer's OCSP server directly to get OCSP status.
OCSP responses are cached and optionally saved to a file for reloading later or
sharing with other applications, they generally have a seven day refresh life,
but ideally need to be checked more often. TOcspHttp can also download the issuer
or intermediate certificate if missing.

OCSP support is built into to the components TSslHttpRest, TIcsIpStrmLog,
TIcsMailQueue, TSslFtpClient, TSslHttpServer, TSslFtpServer, and
TSslWSocketServer.  The OverbyteIcsHttpsTst sample illustrates how it can
be used with other components like THttpCli in the onSslHandshakeDone event.

There is a web server test application OverbyteIcsSslMultiWebServ.dpr that
uses TIcsBlacklist to block potential hackers and abusers.

TIcsBuffLogStream
-----------------
TIcsBuffLogStream buffered log file stream is designed to efficiently
write busy log files ensuring they are safely and reguarly written to disk
in case of application crashes. The log file name is in date/time mask
format, typically for one log file per day, and is updated  before each
write.  The file is updated by being opened. written and closed to ensure
nothing remains in memory only, with repeated attempts to open the file
if another application has it open, and with an inactivity timeout so it
is written regularly, defaulting to every 30 seconds. The file code page
may be FileCPAnsi, FileCPUtf8 or FileCPUtf16 (unicode compilers only)
with a BOM written for unicode.  When writing to the log, CRLF is
optionally added to each line (default on).


Updates:
Nov 17, 2023 V9.1  Baseline.
                   Moved TOcspHttp here from OverbyteIcsSslHttpRest to avoid bringing that
                   unit into as many components.
Sep 6, 2024  V9.3  Using OverbyteIcsTypes for consolidated types and constants, allowing
                     other import units to be removed.
                   Moved TIcsBuffLogStream  here from BlackList, mainly used for servee logging.
Oct 11, 2024 V9.4  Updated Base64 encoding functions to IcsBase64 functions.



}

{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsSslUtils;
{$ENDIF}

{$I Include\Z.ICS9.OverbyteIcsDefs.inc}

{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$H+}             { Use long strings                    }
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

interface

{$IFDEF USE_SSL}

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.ShellAPI{$ELSE}ShellAPI{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Posix.Time,
    System.IOUtils,           { V8.69 }
    Z.ICS9.Ics.Posix.WinTypes,
    Z.ICS9.Ics.Posix.PXMessages,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Sysutils{$ELSE}Sysutils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.TypInfo{$ELSE}TypInfo{$ENDIF},
    Z.ICS9.OverbyteIcsSSLEAY, Z.ICS9.OverbyteIcsLIBEAY,
//    OverbyteIcsWinsock,
    Z.ICS9.OverbyteIcsTypes,
    Z.ICS9.OverbyteIcsUtils,
//    OverbyteIcsUrl,    { TRestParams }
{$IFDEF FMX}
    Z.ICS9.Ics.Fmx.OverbyteIcsWndControl,
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocket,
    Z.ICS9.Ics.Fmx.OverbyteIcsHttpProt,
//    Ics.Fmx.OverbyteIcsSslSessionCache,
//    Ics.Fmx.OverbyteIcsSslX509Utils,
//    Ics.Fmx.OverbyteIcsMsSslUtils,
//    Ics.Fmx.OverbyteIcsSslJose,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslBase,  { V9.1 TX509Base }
{$ELSE}
    Z.ICS9.OverbyteIcsWndControl,
//    OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsHttpProt,
//    OverbyteIcsSslSessionCache,
//    OverbyteIcsSslX509Utils,
//    OverbyteIcsMsSslUtils,
    Z.ICS9.OverbyteIcsSslBase,    { V9.1 TX509Base }
{$ENDIF FMX}
    Z.ICS9.OverbyteIcsMimeUtils,
    Z.ICS9.OverbyteIcsStreams,
//    OverbyteIcsLogger;         { for TLogOption }
    Z.ICS9.OverbyteIcsTicks64;

{ NOTE - TOcspHttp only builds with SSL, there is no non-SSL option }


type
{ event handlers }
  THttpRestProgEvent = procedure (Sender: TObject; LogOption: TLogOption; const Msg: string) of object;

type

{ V8.69 TOcspHttp supports Online Certificate Status Protocol to check revocation }
{ optionally caches OCSP responses received using HTTP or stapled }
    TOcspCache = record
        CertCName: String;
        CertStatus: Integer;
        RespDT: TDateTime;
        CacheDT: TDateTime;
        ReqLen: Integer;
        ReqRaw: AnsiString;
        RespLen: Integer;
        RespRaw: AnsiString;
    end;

TOcspHttp = Class(TIcsWndControl)
  private
    { Private declarations }
    FPostStream: TMemoryStream;
    FResponseStream: TMemoryStream;
    FCacheFlushTimer: TIcsTimer;
    FDebugLevel: THttpDebugLevel;
    FOnOcspProg: THttpRestProgEvent;
    FCertCName: String;
    FOcspCertIDRaw: AnsiString;
    FOcspReqRaw: AnsiString;
    FOcspRespRaw: AnsiString;
    FOcspRespDT: TDateTime;
    FOcspCacheDT: TDateTime;
    FOcspCache: array of TOcspCache;
    FCacheTot: Integer;
    FCacheFName: String;
    FCacheChanged: Boolean;
    FCacheStapled: Boolean;
    FCacheFileShared: Boolean;
    FCacheFileDT: TDateTime;
    FOcspStapleOnly: Boolean;
    FOcspMaxDays: Integer;
    FCacheRefrDays: Integer;
    FCacheFlushMins: Integer;
    FOcspLastResp: String;
    FOcspHttpProxy: String;
  protected
    { Protected declarations }
    procedure onHttpCommand (Sender: TObject; var S: String) ;
    procedure onHttpHeaderData (Sender : TObject);
    procedure onHttpSessionConnected (Sender : TObject);
    procedure onHttpSessionClosed(Sender: TObject);
    procedure onHttpORequestDone(Sender: TObject; RqType: THttpRequest; ErrCode: Word);
    procedure onHttpBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure doOcspLog(LogOption: TLogOption; const Msg: string);
    procedure CacheFlushOnTimer(Sender: TObject);
    procedure ClearCacheItem(Item: Integer);
    procedure DelReqFromCache;
    procedure NewCacheRecs(Tot: Integer);
    function  FindReqInCache: Integer;
    procedure SaveRespToCache;
    function  FindRespInCache: Boolean;
    function  GetCacheAsStr(Item: Integer): String;
    function  GetRespStatus: Integer;
    function  GetCertStatus: Integer;
    function  GetCertReason: Integer;
    procedure ProcResponse;
  public
    { Public declarations }
    HttpCliO: THttpCli;
    HttpCliI: THttpCli;
    OcspCert: TX509Base;
    OcspInters: TX509List;    // we use OpenSSL OCSP functions in this component
    constructor  Create (Aowner: TComponent); override;
    destructor   Destroy; override;
    procedure    ClearOcsp;
    procedure    ClearCache;
    function     ReadCacheFromFile: Boolean;
    function     SaveCacheToFile(CloseDown: Boolean = True): Boolean;
    function     OcspHttpRequest(const URL: String; Timeout: Integer = 0) : Boolean;    { timeout is seconds, zero none }
    function     IssuerHttpRequest(const URL: String; Timeout: Integer = 10): Boolean;
    function     CheckOcspRevoked(X509Store: PX509_STORE; Timeout: Integer = 0): Boolean;
    property     OcspReqRaw: AnsiString             read  FOcspReqRaw
                                                    write FOcspReqRaw;
    property     OcspRespRaw: AnsiString            read  FOcspRespRaw
                                                    write FOcspRespRaw;
    property     OcspRespDT: TDateTime              read  FOcspRespDT;
    property     OcspRespStatus: Integer            read  GetRespStatus;
    property     OcspCertStatus: Integer            read  GetCertStatus;
    property     OcspCertReason: Integer            read  GetCertReason;
    property     CacheTot: Integer                  read  FCacheTot;
    property     OcspLastResp: String               read  FOcspLastResp;
  published
    { Published declarations }
    property     OcspStapleOnly: Boolean            read  FOcspStapleOnly
                                                    write FOcspStapleOnly;
    property     CacheFName: String                 read  FCacheFName
                                                    write FCacheFName;
    property     CacheFileShared: Boolean           read  FCacheFileShared
                                                    write FCacheFileShared;
    property     OcspMaxDays: Integer               read  FOcspMaxDays
                                                    write FOcspMaxDays;
    property     CacheRefrDays: Integer             read  FCacheRefrDays
                                                    write FCacheRefrDays;
    property     CacheFlushMins: Integer            read  FCacheFlushMins
                                                    write FCacheFlushMins;
    property     CacheStapled: Boolean              read  FCacheStapled
                                                    write FCacheStapled;
    property     OcspHttpProxy: String              read  FOcspHttpProxy
                                                    write FOcspHttpProxy;
    property     DebugLevel: THttpDebugLevel        read  FDebugLevel
                                                    write FDebugLevel;
    property     OnOcspProg: THttpRestProgEvent     read  FOnOcspProg
                                                    write FOnOcspProg;
  end;

{$ENDIF USE_SSL}

{ V9.3 moved from OverbyteIcsBlacklist }

// buffered log file stream, designed to write log files, flushing regularly to disk
  TFileCodePage = (FileCPAnsi, FileCPUtf8, FileCPUtf16);
  TIcsBuffLogStream = class(TIcsWndControl)
  private
    FBuffer: TIcsStringBuild;
    FIdleSecs: Integer;
    FIdleTimer: TIcsTimer;
    FHeader: String;
    FBuffMax: Integer;
    FNameMask: String;
    FLogSize: integer;
    FFullName: string;
    FOpenAttempts: integer;
    FOpenDelayMs: LongWord;
    FFileCP: TFileCodePage;
  protected
    procedure OnTimer(Sender: TObject);
    procedure SetIdleSecs(IdleSecs: Integer);
    procedure SetNameMask(sNameMask: String);
    procedure SetFileCP(sFileCP: TFileCodePage);
  public
    constructor Create(Aowner: TComponent; const sNameMask, sHeader: String; sFileCP: TFileCodePage = FileCPAnsi); reintroduce;
    destructor Destroy; override;
    function FlushFile(OldFName: Boolean = False): Integer;
    function WriteLine(const Line: String; AddCRLF: Boolean = True): Integer;
    property BuffMax: Integer        read FBuffMax      write FBuffMax;
    property Header: String          read FHeader       write FHeader;
    property IdleSecs: Integer       read FIdleSecs     write SetIdleSecs;
    property NameMask: String        read FNameMask     write SetNameMask;
    property FileCP: TFileCodePage   read FFileCP       write SetFileCP;
    property OpenAttempts: Integer   read FOpenAttempts write FOpenAttempts;
    property OpenDelayMs: LongWord   read FOpenDelayMs  write FOpenDelayMs;
    property LogSize: integer        read FLogSize ;
    property FullName: string        read FFullName ;
  end;

resourcestring
  SFBuffOpenError = 'Cannot open buffer file %s';

implementation

{$IFDEF USE_SSL}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TOcspHttp }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TOcspHttp.Create (Aowner: TComponent);
begin
    inherited Create(AOwner);
    FPostStream := TMemoryStream.Create;
    FResponseStream := TMemoryStream.Create;
    HttpCliO := THttpCli.Create(self);
    HttpCliO.OnCommand := onHttpCommand;
    HttpCliO.OnHeaderData := onHttpHeaderData;
    HttpCliO.OnRequestDone := onHttpORequestDone;
    HttpCliO.onSessionConnected := onHttpSessionConnected;
    HttpCliO.onSessionClosed := onHttpSessionClosed;
    HttpCliO.onBgException := onHttpBgException;
    HttpCliO.CtrlSocket.ComponentOptions := [wsoNoReceiveLoop];
    HttpCliO.SocketFamily := sfAny;
    HttpCliI := THttpCli.Create(self);
    HttpCliI.OnCommand := onHttpCommand;
    HttpCliI.OnHeaderData := onHttpHeaderData;
    HttpCliI.onSessionConnected := onHttpSessionConnected;
    HttpCliI.onSessionClosed := onHttpSessionClosed;
    HttpCliI.onBgException := onHttpBgException;
    HttpCliI.CtrlSocket.ComponentOptions := [wsoNoReceiveLoop];
    HttpCliI.SocketFamily := sfAny;
    FCacheFlushTimer := TIcsTimer.Create(HttpCliO);
    FCacheFlushTimer.Enabled := False;
    FCacheFlushTimer.OnTimer := CacheFlushOnTimer;
    FDebugLevel := DebugNone;
    ClearCache;
    ClearOcsp;
    FOcspMaxDays := 7;
    FCacheRefrDays := 3;
    FCacheFlushMins := 2;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TOcspHttp.Destroy;
begin
    try    { V8.71 }
        FCacheFlushTimer.Enabled := False;
        if (FCacheFName <> '') and FCacheChanged and (FCacheTot > 0) then
            SaveCacheToFile(True);
        ClearCache;
        FreeAndNil(FCacheFlushTimer);
        FreeAndNil(FPostStream);
        FreeAndNil(FResponseStream);
        FreeAndNil(HttpCliO);
        FreeAndNil(HttpCliI);
    finally
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.ClearOcsp;
begin
    OcspCert := Nil;
    OcspInters := Nil;
    FOcspCertIDRaw := '';
    FOcspReqRaw := '';
    FOcspRespRaw := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.ClearCache;
begin
    SetLength(FOcspCache, 0);
    FCacheTot := 0;
    FCacheFileDT := -1 ;
    FCacheChanged := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.doOcspLog(LogOption: TLogOption; const Msg: string);
begin
    if Assigned(FOnOcspProg) then
        FOnOcspProg(Self, LogOption, Msg) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.onHttpBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
begin
    doOcspLog(loProtSpecInfo, (Sender as THttpCli).URL + ' Fatal Exception: ' + IcsGetExceptMess(E));
    CanClose := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.onHttpSessionConnected (Sender : TObject);
var
    S: String;
    Cli: THttpCli;
begin
    if FDebugLevel < DebugConn then Exit;
    Cli := Sender as THttpCli;
    if Cli.State = httpConnected then
        S := '= Connected OK to: '
    else
        S := '= Connection failed to: ';
    S := S + Cli.Hostname + ' (' + IcsFmtIpv6Addr(Cli.AddrResolvedStr) + ')';
    doOcspLog(loProtSpecInfo, S);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.onHttpSessionClosed(Sender: TObject);
var
    Cli: THttpCli;
    S: String;
begin
    if FDebugLevel < DebugConn then Exit;
    Cli := Sender as THttpCli;
    S := '= ' + Cli.URL + ' Session Closed';
    if Cli.RequestDoneError <> httperrNoError then
        S := S + ', Error: ' + Cli.RequestDoneErrorStr;
    S := S + ' - ' + Cli.ReasonPhrase;
    doOcspLog(loProtSpecInfo, S);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.onHttpCommand (Sender: TObject; var S: String) ;
begin
    if FDebugLevel < DebugHdr then Exit;
    doOcspLog(loProtSpecInfo, '> ' + S);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.onHttpHeaderData (Sender : TObject);
begin
    if FDebugLevel < DebugHdr then Exit;
    doOcspLog(loProtSpecInfo, '< ' + (Sender as THttpCli).LastResponse);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ get interemediate certificate for issuer of our certificate }
function TOcspHttp.IssuerHttpRequest(const URL: String; Timeout: Integer): Boolean;
var
    StatCode: Integer;
    RawCert: AnsiString;
    Errs: String;
    MyCert: TX509Base;
begin
    Result := False;
    OcspInters.Clear;
    if (URL = '') then Exit;

    try
        if HttpCliI.State <> httpReady then begin
            doOcspLog(loProtSpecInfo, 'Last Issuer Request Still Busy');
            Exit;
        end;
        HttpCliI.URL := URL;
        HttpCliI.ProxyURL := FOcspHttpProxy;
        HttpCliI.Connection := 'Close';   // no keep-alive
        HttpCliI.Timeout := Timeout;
        HttpCliI.Accept := MimeAppCert;
        FResponseStream.Clear;
        HttpCliI.RcvdStream := FResponseStream;
        HttpCliI.ResponseNoException := True;
        HttpCliI.Get;   // blocking sync request
        StatCode := HttpCliI.StatusCode;  // only for sync requests
        if (StatCode = 200) and (Pos(MimeAppCert, HttpCliI.ContentType) = 1) then begin
            FResponseStream.Position := 0;
            SetLength(RawCert, FResponseStream.Size);
            FResponseStream.ReadBuffer(RawCert[1], FResponseStream.Size);
         // if PEM format may load more than one, possibly
            if (IcsAnsiPosEx(PEM_STRING_HDR_BEGIN, RawCert) > 0) then
                OcspInters.LoadAllFromString(String(RawCert))
            else begin
                MyCert := TX509Base.Create(Self, Nil);
                MyCert.ReadFromAStr(RawCert, croNo, croNo, '', Errs);
                if MyCert.IsCertLoaded then
                    OcspInters.Add(MyCert.X509);
                MyCert.Free;
            end;
            if OcspInters.Count = 0 then begin
                if Errs = '' then Errs := 'Content size: ' + IntToStr(Length(RawCert));
                doOcspLog(loProtSpecInfo, 'Failed to Load Issuer Certificate from ' + URL + ' - ' + Errs);
            end
            else begin
                doOcspLog(loProtSpecInfo, 'Downloaded Issuer Certificate OK: ' + OcspInters[0].SubjectOneLine);
                Result := True;
            end;
        end
        else begin
            HttpCliI.Abort;
            doOcspLog(loProtSpecInfo, 'Download Issuer Certificate Request to ' + URL + ', failed - ' + HttpCliI.LastResponse);
        end;
        HttpCliI.CloseAsync;
    except
        on E:Exception do begin    { 400/500 no longer come here }
            doOcspLog(loProtSpecInfo, 'Download Issuer Certificate to ' + URL + ', failed: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.OcspHttpRequest(const URL: String; Timeout: Integer) : Boolean;
begin
    Result := False;
    FOcspRespRaw := '';
    FOcspRespDT := 0;
    if (URL = '') or (Length(FOcspReqRaw) < 20) then Exit;

    try
        if HttpCliO.State <> httpReady then begin
            doOcspLog(loProtSpecInfo, 'Last OCSP Request Still Busy');
            Exit;
        end;
        HttpCliO.URL := URL;
        HttpCliO.ProxyURL := FOcspHttpProxy;
        HttpCliO.Connection := 'Close';   // no keep-alive
        HttpCliO.Timeout := Timeout;
        HttpCliO.ContentTypePost := MimeOcspRequest;
        FResponseStream.Clear;
        HttpCliO.RcvdStream := FResponseStream;
        FPostStream.Clear;
        FPostStream.Write(FOcspReqRaw[1], Length(FOcspReqRaw));
        FPostStream.Position := 0;
        HttpCliO.SendStream := FPostStream;
        HttpCliO.ResponseNoException := True;
        if TimeOut = 0 then
            HttpCliO.PostASync   // non blocking async request
            // returns immediately,
        else
            HttpCliO.Post;       // blocking sync request
       // process response in onHttpORequestDone
       Result := True;
    except
        on E:Exception do begin    { 400/500 no longer come here }
            doOcspLog(loProtSpecInfo, 'OCSP Request to ' + URL + ', failed: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.GetRespStatus: Integer;
begin
    if NOT Assigned(OcspInters) then
        Result := -1
    else
        Result := OcspInters.OcspRespStatus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.GetCertStatus: Integer;
begin
    if NOT Assigned(OcspInters) then
        Result := -1
    else
        Result := OcspInters.OcspCertStatus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.GetCertReason: Integer;
begin
    if NOT Assigned(OcspInters) then
        Result := -1
    else
        Result := OcspInters.OcspCertReason;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.ProcResponse;
var
    RespInfo: String;
begin
    FOcspLastResp := 'No OCSP Status Available';
    if FOcspRespRaw = '' then Exit;
    if NOT Assigned(OcspInters) then Exit;
    if GetCertStatus >= V_OCSP_CERTSTATUS_GOOD then begin
        FOcspRespDT := OcspInters.OcspUpdateDT;
        doOcspLog(loProtSpecInfo, 'OCSP Status Response: ' + OcspInters.OcspRespStatusStr);

        if (FDebugLevel >= DebugSsl) then begin
            RespInfo := OcspInters.OcspCertStatusStr;
            if GetCertStatus = V_OCSP_CERTSTATUS_REVOKED then
                RespInfo := RespInfo + OcspInters.OcspCertReasonStr + ', on ' + DateTimeToStr(OcspInters.OcspRevokeDT);
            RespInfo := RespInfo + ', last updated '  + DateTimeToStr(OcspInters.OcspUpdateDT) +
                                              ', next update ' + DateTimeToStr(OcspInters.OcspNextUpdDT);
            doOcspLog(loProtSpecInfo, 'OCSP Certificate status: ' + RespInfo);
            if (FDebugLevel >= DebugBody) then                                    { V8.70 don't log raw OCSP stuff as often }
                doOcspLog(loProtSpecInfo, OcspInters.OCSPRespInfo(FOcspRespRaw) + IcsCRLF);
        end;

        if GetCertStatus = V_OCSP_CERTSTATUS_REVOKED then begin
           FOcspLastResp := 'Certificate revoked: ' + OcspInters.OcspCertReasonStr;
        end
        else begin
           if GetCertStatus = V_OCSP_CERTSTATUS_GOOD then
                FOcspLastResp := 'Certificate OCSP Status Valid, Not Revoked'
           else if GetCertStatus > V_OCSP_CERTSTATUS_GOOD then
                FOcspLastResp := 'Certificate OCSP Status Check Failed: ' + OcspInters.OcspCertStatusStr;
        end;
    end
    else begin
        FOcspLastResp := 'OCSP Status Failed: ' + OcspInters.OcspCertStatusStr;
        doOcspLog(loProtSpecInfo, FOcspLastResp);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.onHttpORequestDone(Sender: TObject; RqType: THttpRequest; ErrCode: Word);
var
    Cli: THttpCli;
    StatCode: Integer;
begin
    if RqType <> httpPOST then Exit;
    Cli := Sender as THttpCli;
    StatCode := Cli.StatusCode;
    if ErrCode <> 0 then begin
        doOcspLog(loProtSpecInfo, 'OCSP Request failed: ' + Cli.RequestDoneErrorStr + ' - ' + Cli.ReasonPhrase);
    end
    else begin
        if StatCode = 200 then begin
            FOcspRespDT := Cli.RespLastModDT;
            FResponseStream.Position := 0;
            SetLength(FOcspRespRaw, FResponseStream.Size);
            FResponseStream.ReadBuffer(FOcspRespRaw[1], FResponseStream.Size);
            if (FDebugLevel >= DebugSsl) then                                    { V8.70 don't log raw OCSP stuff as often }
               doOcspLog(loProtSpecInfo, 'OCSP Response OK from ' + Cli.URL + ', Size ' + IntToStr(FResponseStream.Size));
            SaveRespToCache;

         { note we don't verify response here since the issuer and root may not be available any longer for async
           operations and the handshake will already have been completed, it's checked next time from cache }
        end
        else begin
            doOcspLog(loProtSpecInfo, 'OCSP Request to ' + Cli.URL + ', failed - ' + Cli.ReasonPhrase);
        end;
    end;
    Cli.CloseAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.CheckOcspRevoked(X509Store: PX509_STORE; Timeout: Integer = 0): Boolean;
var
    I: Integer;
begin
    Result := False;
    FOcspLastResp := 'Checking OCSP Status';
    FOcspReqRaw := '';
    FOcspCertIDRaw := '';
    if NOT Assigned(OcspCert) then Exit;
    if NOT OcspCert.IsCertLoaded then Exit;
    if NOT Assigned(OcspInters) then Exit;
    if OcspInters.Count = 0 then Exit;

    try
    // look for intermediate in local bundle server sent us
        FCertCName := OcspCert.SubjectCName;
        I := OcspInters.IndexOfSubj(OcspCert.IssuerOneLine);

    // try and download intermediate - should rarely happen here
    // if it was missing, verify probably failed so we should repeat that somehow!!!
        if I < 0 then begin
            doOcspLog(loProtSpecInfo, 'Can not find Intermediate: ' + OcspCert.IssuerOneLine);    // TEMP
            doOcspLog(loProtSpecInfo, 'Intermediate bundle Tot=' + IntToStr(OcspInters.Count));   // TEMP

            doOcspLog(loProtSpecInfo, 'Downloading Missing Intermediate from: ' + OcspCert.UrlIssuer);
            IssuerHttpRequest(OcspCert.UrlIssuer, 10);
            I := OcspInters.IndexOfSubj(OcspCert.IssuerOneLine);
        end;
        if I < 0 then
            FOcspLastResp := 'Could Not Find Intermediate Certificate for OCSP'
        else begin
            FOcspCertIDRaw := OcspInters.BuildOCSPCertID(OcspCert);
            if FOcspCertIDRaw = '' then
                FOcspLastResp := 'Failed to Build OCSP Certificate ID';
        end;

     // got intermediate, test OCSP against stapled response sent during handshake, maybe
        if FOcspCertIDRaw <> '' then begin
            if (Length(FOcspRespRaw) > 50) then begin
                doOcspLog(loProtSpecInfo, 'Using OCSP Stapled Response from TLS Handshake');
                if (OcspInters.CheckOCSPResp(FOcspCertIDRaw, FOcspRespRaw, X509Store) >= 0) then begin
                    if (GetCertStatus <> V_OCSP_CERTSTATUS_GOOD) then
                        doOcspLog(loProtSpecInfo, 'OCSP Stapled Response Failed: ' + OcspInters.OcspCertStatusStr);
                    ProcResponse;
                    if GetCertStatus = V_OCSP_CERTSTATUS_REVOKED then
                        Result := True;
                    if FCacheStapled then begin
                        FOcspReqRaw := OcspInters.BuildOCSPReq(FOcspCertIDRaw);
                        SaveRespToCache;
                    end;
                end;
            end;

         // no staple response, see if cached response
            if GetCertStatus < V_OCSP_CERTSTATUS_GOOD then begin
                FOcspReqRaw := OcspInters.BuildOCSPReq(FOcspCertIDRaw);
                if (FOcspReqRaw = '') then
                    FOcspLastResp := 'Failed to Build OCSP Request'
                else begin
                    if FindRespInCache then begin
                        doOcspLog(loProtSpecInfo, 'Found Cached OCSP Status');
                        if (OcspInters.CheckOCSPResp(FOcspCertIDRaw, FOcspRespRaw, X509Store) >= 0) then begin
                            ProcResponse;
                            if GetCertStatus = V_OCSP_CERTSTATUS_REVOKED then
                                Result := True;
                        end;
                    end;

               // nothing cached or need to refresh it, make HTTP request to OCSP server and cache response for next time
               // if timeout none zero, block and wait for response, don't do this during SSL handshake or things break
                    if (GetCertStatus < V_OCSP_CERTSTATUS_GOOD) or ((Now - FOcspCacheDT) > FCacheRefrDays) then begin
                        if FOcspStapleOnly then
                           FOcspLastResp := 'OCSP Status Not Available'
                        else if Assigned(OcspCert) then begin
                            if OcspCert.UrlOcsp = '' then
                               FOcspLastResp := 'No OCSP Server URL Available'
                            else begin
                                FOcspLastResp := 'Awaiting OCSP Status';
                                doOcspLog(loProtSpecInfo, 'Getting OCSP Status from: ' + OcspCert.UrlOcsp);
                                if (FDebugLevel >= DebugBody) then                                    { V8.70 don't log raw OCSP stuff as often }
                                    doOcspLog(loProtSpecInfo, OcspInters.OCSPReqInfo(FOcspReqRaw) + IcsCRLF);
                                FOcspRespRaw := '';
                                if NOT OcspHttpRequest(OcspCert.UrlOcsp, Timeout) then begin
                                    FOcspLastResp := 'OCSP Request Failed to: ' + OcspCert.UrlOcsp;
                                end
                                else if (Timeout <> 0) then begin  // sync request check response
                                    if (OcspInters.CheckOCSPResp(FOcspCertIDRaw, FOcspRespRaw, X509Store) >= 0) then begin
                                        ProcResponse;
                                        if GetCertStatus = V_OCSP_CERTSTATUS_REVOKED then
                                            Result := True;
                                    end;
                                end;
                            end;
                        end
                        else
                            FOcspLastResp := 'OCSP Certificate Lost!';
                    end;
                end;
            end;
        end;
    except
        FOcspLastResp := 'OCSP Checking Failed, Exception';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.FindReqInCache: Integer;
var
    ReqLen: Integer;
begin
    Result := -1;
    if FCacheTot = 0 then Exit;
    ReqLen := Length(FOcspReqRaw);
    if (ReqLen = 0) then Exit;
    for Result := 0 to FCacheTot - 1 do begin
        if (ReqLen = FOcspCache[Result].ReqLen) and
              (OcspReqRaw = FOcspCache[Result].ReqRaw) then Exit;
    end;
    Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.ClearCacheItem(Item: Integer);
begin
    if Item < 0 then Exit;
    if Item >= Length(FOcspCache) then Exit;
    FOcspCache[Item].ReqRaw := '';
    FOcspCache[Item].ReqLen := 0;
    FOcspCache[Item].RespRaw := '';
    FOcspCache[Item].RespLen := 0;
    FOcspCache[Item].RespDT := 0;
    FOcspCache[Item].CertCName := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.DelReqFromCache;
begin
    ClearCacheItem(FindReqInCache);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.NewCacheRecs(Tot: Integer);
var
    J: Integer;
begin
    SetLength(FOcspCache, FCacheTot + Tot);
    for J := FCacheTot to Length(FOcspCache) - 1 do
        ClearCacheItem(J);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.SaveRespToCache;
var
    I, J: Integer;
begin
    if (FOcspReqRaw = '') or (FOcspRespRaw = '') then Exit;
    I := FindReqInCache;    // see if replacing old entry
    if I < 0 then begin
        if FCacheTot > 0 then begin  // look for empty entries
            for J := 0 to FCacheTot - 1 do begin
                if Length(FOcspCache[J].ReqRaw) = 0 then begin
                    I := J;
                    Break;
                end;
            end;
        end;
        if I < 0 then begin  // new entry in cache, may need to increase size
            I := FCacheTot;
            FCacheTot := FCacheTot + 1;
            if FCacheTot >= Length(FOcspCache) then
                NewCacheRecs(32);
        end;
    end;
    if FOcspRespDT = 0 then FOcspRespDT := Now;
    FOcspCache[I].CertCName := FCertCName;
    FOcspCache[I].CertStatus := GetCertStatus;
    FOcspCache[I].RespDT := FOcspRespDT;
    FOcspCache[I].CacheDT := Now;
    FOcspCache[I].ReqLen := Length(FOcspReqRaw);
    FOcspCache[I].ReqRaw := FOcspReqRaw;
    FOcspCache[I].RespLen := Length(FOcspRespRaw);
    FOcspCache[I].RespRaw := FOcspRespRaw;
    FCacheChanged := True;
    if NOT FCacheFlushTimer.Enabled then begin
        if FCacheFlushMins < 1 then
            FCacheFlushMins := 1;
        FCacheFlushTimer.Interval := LongWord(FCacheFlushMins) * TicksPerMinute;
        FCacheFlushTimer.Enabled := True;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.FindRespInCache: Boolean;
var
    I: Integer;
begin
    Result := False;
    FOcspRespRaw := '';
    if (FCacheTot = 0) and (FCacheFileDT >= 0) then  // skip if file not found already
        ReadCacheFromFile;   // initial load from file
    I := FindReqInCache;
    if I < 0 then begin
        if NOT ReadCacheFromFile then Exit;  // reload from file if newer
        I := FindReqInCache;
        if I < 0 then Exit;
    end;

  // has entry expired, clear it, unless revoked
    if FOcspMaxDays < 1 then FOcspMaxDays := 3;
    if ((Now - FOcspCache[I].RespDT) > FOcspMaxDays) and
            (FOcspCache[I].CertStatus <> V_OCSP_CERTSTATUS_REVOKED) then begin
        ClearCacheItem(I);
        Exit;
    end;

 // found cached response, ignore cert status
    FCertCName := FOcspCache[I].CertCName;
    FOcspCacheDT := FOcspCache[I].CacheDT;
    FOcspRespDT :=  FOcspCache[I].RespDT;
    FOcspRespRaw := FOcspCache[I].RespRaw;
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.GetCacheAsStr(Item: Integer): String;
begin
    Result := '';
    if Item < 0 then Exit;
    if Item >= Length(FOcspCache) then Exit;
    if Length(FOcspCache[Item].ReqRaw) = 0 then Exit;
    Result := FOcspCache[Item].CertCName + ',' +
        IntToStr(FOcspCache[Item].CertStatus) + ',' +
        RFC3339_DateToStr(FOcspCache[Item].CacheDT) + ',' +
        RFC3339_DateToStr(FOcspCache[Item].RespDT) + ',' +
        IntToStr(Length(FOcspCache[Item].ReqRaw)) + ',' +
        String(IcsBase64EncodeA(FOcspCache[Item].ReqRaw)) + ',' +      { V9.4 }
        IntToStr(Length(FOcspCache[Item].RespRaw)) + ',' +
        String(IcsBase64EncodeA(FOcspCache[Item].RespRaw));            { V9.4 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.SaveCacheToFile(CloseDown: Boolean = True): Boolean;
var
    I, Tot: Integer;
    FileUDT: TDateTime;
    StreamWriter: TIcsStreamWriter;
begin
    Result := False;
    try
        FCacheFlushTimer.Enabled := False;
        if FCacheFName = '' then Exit;
        if NOT FCacheChanged then Exit;
        FCacheChanged := False;
        FileUDT := IcsGetFileUAge(FCacheFName);

     // pending implement FCacheFileShared by only adding new records to file

        if FileUDT > 0 then
            IcsDeleteFile (FCacheFName, true) ;
        if FCacheTot = 0 then Exit;
        Tot := 0;
        StreamWriter := TIcsStreamWriter.Create(FCacheFName, fmCreate, 0, MAX_BUFSIZE);
        StreamWriter.LineBreakStyle := ilbsCRLF;
        for I:= 0 to FCacheTot - 1 do begin
            if Length(FOcspCache[I].ReqRaw) > 0 then begin
                StreamWriter.WriteLine(GetCacheAsStr(I));
                Tot := Tot + 1;
            end;
        end;
        StreamWriter.WriteLine('end');
        StreamWriter.Flush;
        StreamWriter.Destroy;
        FCacheFileDT := IcsGetFileUAge(FCacheFName);
        if NOT CloseDown then  // logging during close down can cause trouble
            doOcspLog(loProtSpecInfo, 'Saved OCSP Cache File OK: ' + FCacheFName + ' - ' + IntToStr(Tot) + ' records');
        Result := True;
    except
        on E:Exception do begin
            if NOT CloseDown then
                doOcspLog(loProtSpecInfo, 'Failed to Save OCSP Cache File: ' + FCacheFName + ' - ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.ReadCacheFromFile: Boolean;
var
    J, RawLen: Integer;
    FileUDT: TDateTime;
    StreamReader: TIcsStreamReader;
    RecCols: TStringList ;
    Line: String;
    RawField: AnsiString;
begin
    Result := False;
    FCacheFlushTimer.Enabled := False;
    if FCacheFName = '' then Exit;
    FileUDT := IcsGetFileUAge(FCacheFName);
    if FileUDT <= 0 then begin
        FCacheFileDT := FileUDT;
        doOcspLog(loProtSpecInfo, 'Can Not Find OCSP Cache File: ' + FCacheFName);
        Exit;
    end;

 // unchanged since last loaded or saved, give up
    if (FCacheFileDT = FileUDT) and (FCacheTot > 0) then begin
        Result := True;
        Exit;
    end;
    FCacheFileDT := FileUDT;

 // pending implement FCacheFileShared by updating existing records from file

    FCacheTot := 0;  // clear all cache records
    if Length(FOcspCache) > 0 then begin
        for J := 0 to Length(FOcspCache) - 1 do
           ClearCacheItem(J);
    end;
    RecCols := TStringList.Create ;
    try
        try
            StreamReader := TIcsStreamReader.Create(FCacheFName, fmShareDenyWrite, 0, MAX_BUFSIZE);
            StreamReader.MaxLineLength := 8000;
            StreamReader.Position := 0;
            while StreamReader.ReadLine(Line) do begin
               RecCols.CommaText := Line;
                if RecCols.Count >= 8 then begin
                    RawLen := atoi(RecCols[4]);
                    RawField := IcsBase64Decode(RecCols[5]);
                    if (RawLen = Length(RawField)) and (RawLen > 20) then begin
                        if ((FCacheTot + 1) >= Length(FOcspCache)) then
                            NewCacheRecs(32);
                        FOcspCache[FCacheTot].CertCName := RecCols[0];
                        FOcspCache[FCacheTot].CertStatus := atoi(RecCols[1]);
                        FOcspCache[FCacheTot].CacheDT := RFC3339_StrToDate(RecCols[2]);
                        FOcspCache[FCacheTot].RespDT := RFC3339_StrToDate(RecCols[3]);
                        FOcspCache[FCacheTot].ReqLen := RawLen;
                        FOcspCache[FCacheTot].ReqRaw := RawField;
                        FOcspCache[FCacheTot].RespLen := atoi(RecCols[6]);
                        FOcspCache[FCacheTot].RespRaw := IcsBase64Decode(RecCols[7]);
                        if (FOcspCache[FCacheTot].RespLen <> Length(FOcspCache[FCacheTot].RespRaw)) or
                          (((Now - FOcspCache[FCacheTot].RespDT) > FOcspMaxDays) and
                              (FOcspCache[FCacheTot].CertStatus <> V_OCSP_CERTSTATUS_REVOKED)) then begin
                            ClearCacheItem(FCacheTot);
                        end
                        else
                            FCacheTot := FCacheTot + 1;
                    end;
                end;
            end;
            StreamReader.Destroy;
            doOcspLog(loProtSpecInfo, 'Read OCSP Cache File OK: ' + FCacheFName + ' - ' + IntToStr(FCacheTot) + ' records');
            FCacheChanged := False;
            Result := True;
        except
            on E:Exception do begin
                doOcspLog(loProtSpecInfo, 'Failed to Read OCSP Cache File: ' + FCacheFName + ' - ' + E.Message);
            end;
        end;
    finally
        RecCols.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.CacheFlushOnTimer(Sender: TObject);
begin
    FCacheFlushTimer.Enabled := False;
    SaveCacheToFile(False);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$ENDIF USE_SSL}

{ V9.3 moved from OverbyteIcsBlacklist }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsBuffLogStream }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsBuffLogStream.Create(Aowner: TComponent; const sNameMask, sHeader: String; sFileCP: TFileCodePage = FileCPAnsi);
begin
    inherited Create(AOwner);
    FBuffMax := 1024*32;
    FBuffer := TIcsStringBuild.Create(FBuffMax);
    AllocateHWnd;
    FIdleTimer := TIcsTimer.Create (Self);
    FIdleTimer.Enabled := false;
    FIdleTimer.OnTimer := OnTimer;
    FIdleSecs := 30;
    FLogSize := -1;
    FOpenAttempts := 20;
    FOpenDelayMs := 50;
    FIdleTimer.Interval := Longword (FIdleSecs) * TicksPerSecond;
    SetNameMask(sNameMask);
    FHeader := sHeader;
    SetFileCP(sFileCP);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsBuffLogStream.Destroy;
begin
    try
        FlushFile(false);
    except
    end;
    if Assigned (FIdleTimer) then begin
        FIdleTimer.Enabled := false;
        FIdleTimer.Free;
    end ;
    if Assigned(FBuffer) then FBuffer.Free;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBuffLogStream.OnTimer(Sender: TObject) ;
begin
    FIdleTimer.Enabled := false;
    try
        FlushFile(false);
    except
        // ignore error, buffer will grow if needed
    end ;
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBuffLogStream.SetNameMask(sNameMask: String);
begin
    if sNameMask = '' then Exit;
    if Pos('"', sNameMask) = 0 then  // expecting name mask, if none create it
        FNameMask := '"' + sNameMask + '"'
    else
        FNameMask := sNameMask;

 // get initial full unmasked file name, repeated before every file flush
    FFullName := FormatDateTime(FNameMask, Now) ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBuffLogStream.SetFileCP(sFileCP: TFileCodePage);
begin
    FFileCP := sFileCP;
{$IFNDEF UNICODE}  { don't allow full Unicode on old compilers }
    if FFileCP = FileCPUtf16 then FFileCP := FileCPUtf8;
{$ENDIF}
    case FFileCP of
        FileCPAnsi: FBuffer.CharSize := 1;    { V8.67 now public }
        FileCPUtf8: FBuffer.CharSize := 1;
        FileCPUtf16: FBuffer.CharSize := 2;
    end;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsBuffLogStream.SetIdleSecs (IdleSecs: Integer);
begin
    FIdleSecs := IdleSecs;
    FIdleTimer.Interval := Longword (FIdleSecs) * TicksPerSecond;
    if FIdleTimer.Enabled then
    begin
        FIdleTimer.Enabled := false;
        FIdleTimer.Enabled := true;
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ opens log file, writes buffer, closes again }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsBuffLogStream.FlushFile(OldFName: Boolean = False): Integer;
var
    HdrLine: String;
    Utf8Line: RawByteString;
    LineLen: Integer;
    LogHandle, Attempts: Integer;
    TickCount: Int64;    { V8.71 }
    Bom : TBytes;
begin
    Result := 0 ;
    FIdleTimer.Enabled := false;
    if FBuffer.Len = 0 then exit;
    if FNameMask = '' then exit ;
    LogHandle := -1 ;

    try // finally

     // generally create new log file name with current time and date
     // but perhaps not at midnight and want stuff in yesterday's log
        if (NOT OldFName) or (FFullName = '') then
            FFullName := FormatDateTime (FNameMask, Now) ;

      // open log file, lots of attempts, probably, beware blocks application
        for Attempts := 1 to FOpenAttempts do begin
            if FileExists(FFullName) then begin
                LogHandle := FileOpen(FFullName, fmOpenReadWrite OR fmShareDenyWrite);
                if LogHandle >= 0 then begin
                    Result := FileSeek(LogHandle, 0, soFromEnd);  // end of file
                    if Result < 0 then exit;
                end;
            end;
            if (LogHandle < 0) and (NOT (FileExists(FFullName))) then begin
                ForceDirectories(ExtractFileDir(FFullName));
                LogHandle := FileCreate(FFullName);
                if LogHandle >= 0 then begin

                  // unicode files like a BOM at the beginning
                    SetLength(bom, 0);
                    if FFileCP = FileCPUtf8 then
                        Bom := GetBomFromCodePage(CP_UTF8)
                    else if FFileCP = FileCPUtf16 then
                        Bom := GetBomFromCodePage(CP_UTF16);
                    if Length(Bom) > 0 then begin
                        FileWrite (LogHandle, Bom[0], Length(Bom));
                    end;

                  // see if writing header to top of log file
                    if (Pos ('"', FHeader) > 0) and (Pos ('","', FHeader) = 0) then
                        HdrLine := FormatDateTime(FHeader, Now)  // warning max 255 characters may be formatted
                    else
                        HdrLine := FHeader;
                    LineLen := Length(HdrLine);
                    if LineLen > 0 then begin
                        HdrLine := HdrLine + IcsCRLF;
                        LineLen := LineLen + 2;
                        if FFileCP = FileCPAnsi then
                            Utf8Line := AnsiString(HdrLine)    // may lose Unicode chars
                        else if FFileCP = FileCPUtf8 then begin
                            Utf8Line := StringToUtf8(HdrLine);
                            LineLen := Length(Utf8Line);
                        end
                        else if FFileCP = FileCPUtf16 then
                            LineLen := LineLen * 2;
                        if FFileCP <> FileCPUtf16 then
                            FileWrite(LogHandle, PAnsiChar(Utf8Line)^, LineLen)
                        else
                            FileWrite(LogHandle, PChar(HdrLine)^, LineLen);
                    end;
                end ;
            end ;
            if LogHandle >= 0 then break ;  // finished OK

         // failed to open file, wait a few milliseconds and try again
            TickCount := IcsGetTickCount64;   { V8.71 }   { V8.65 }
            while (IcsElapsedMsecs64(TickCount) < FOpenDelayMs) do  // 50ms delay  { V8.65 }
                ProcessMessages;
        end ;

     // did not open file, fatal error raise exception
        if LogHandle < 0 then begin
            HdrLine := SysErrorMessage(GetLastError) + ' [' +
                                    IcsIntToCStr(GetLastError) + '] ' + fullname ;
            raise EFOpenError.CreateResFmt(@SFBuffOpenError, [HdrLine]);
            exit ;
        end ;

    // write buffered text, clear buffer
        Result := FileWrite(LogHandle, Pointer (FBuffer.Buffer)^, FBuffer.Len) ;
        FLogSize := FileSeek(LogHandle, 0, soFromEnd) ;  // end of file
        FBuffer.Clear;
    finally
        if LogHandle >= 0 then FileClose(LogHandle) ;
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// return exception, unless error flushing, might work next time
// write Line adding CRLF optionally
function TIcsBuffLogStream.WriteLine(const Line: String; AddCRLF: Boolean = True): Integer;
var
    Utf8Line: RawByteString;
    LineLen: Integer;
begin
    Result := 0;
    LineLen := Length(Line);
    if FFileCP = FileCPUtf8 then begin
        Utf8Line := StringToUtf8(Line);
        LineLen := Length(Utf8Line);
    end
    else if FFileCP = FileCPUtf16 then
        LineLen := LineLen * 2
    else begin      { V8.70 default is FileCPAnsi }
        Utf8Line := AnsiString(Line);   // may lose Unicode chars
        LineLen := Length(Utf8Line);
    end;

   // add CRLF
    if AddCRLF and (FFileCP <> FileCPUtf16) then begin
        Utf8Line := Utf8Line + IcsCRLF;
    end;

  // see if room in buffer for new line
    if ((FBuffer.Len + LineLen + 16) >= FBuffMax) then begin
        try
            Result := FlushFile(false);
        except
            // ignore error, buffer will grow if needed
        end ;
    end ;
    if FFileCP <> FileCPUtf16 then
        FBuffer.AppendBufA(Utf8Line)
    else begin
        FBuffer.AppendBufW(Line);                 { V8.67 use W }
        FBuffer.AppendBufW(WideString(IcsCRLF));  { V8.67 use W }
    end;

// buffer now full, flush it
    if (FBuffer.Len >= FBuffMax) then begin
        try
            Result := FlushFile(false);
        except
            // ignore error, buffer will grow if needed
        end ;
    end

// idle timeout flush, don't reset in case of low volume writes
    else begin
        if NOT FIdleTimer.Enabled then
            FIdleTimer.Enabled := true;
    end;
end ;

end.
