{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  TDnsQueryHttps and TIcsDomNameCacheHttps to lookup and cache
              DNS requests using HTTPS.
Creation:     Nov 2023
Version:      V9.1
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2023 by Angus Robertson, Magenta Systems Ltd,
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


TDnsQueryHttps
--------------
Component to support DOH, DNS over HTTPS lookups similarly to TDnsQuery.


TIcsDomNameCacheHttps
----------------------
Component descendening from TIcsDomainNameCache to cache DNS requests, adding
DNS over HTTPS support.


Updates:
Nov 16, 2023 V9.1  Baseline.
                   Split TDnsQueryHttps and TIcsDomNameCacheHttps from OverbyteIcsSslHttpRest to
                     avoid circular references and simplify unit.


}

{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsDnsHttps;
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
    Z.ICS9.OverbyteIcsSSLEAY, Z.ICS9.OverbyteIcsLIBEAY,
    Z.ICS9.OverbyteIcsTypes,
    Z.ICS9.OverbyteIcsUtils,
{$IFDEF FMX}
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocket,
    Z.ICS9.Ics.Fmx.OverbyteIcsHttpProt,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslHttpRest,
    Z.ICS9.Ics.Fmx.OverbyteIcsDnsQuery,
{$ELSE}
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsHttpProt,
    Z.ICS9.OverbyteIcsSslHttpRest,
    Z.ICS9.OverbyteIcsDnsQuery,
{$ENDIF}
    Z.ICS9.OverbyteIcsMimeUtils,
    Z.ICS9.OverbyteIcsTicks64,
    Z.ICS9.OverbyteIcsLogger;         { for TLogOption }

{ NOTE - these components only build with SSL, there is no non-SSL option }


type
  { V8.61 TDnsQueryHttps supports DOH - DNS over HTTPS }
  TDnsQueryHttps = Class(TDnsQuery)
  private
    { Private declarations }
    FDebugLevel: THttpDebugLevel;
    FDnsSrvUrl: string;
 //   FDnsSrvUrlList: TStrings;     { V8.71 not supported yet }
    FOnDnsProg: THttpRestProgEvent;
  protected
    { Protected declarations }
    procedure DnsRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
    procedure DnsRestRequestDone(Sender: TObject; RqType: THttpRequest; ErrCode: Word);
  public
    { Public declarations }
    HttpRest:  TSslHttpRest;
    constructor  Create (Aowner: TComponent); override;
    destructor   Destroy; override;
    function     DOHQueryAll(Host: String): Boolean;          { V8.64 }
    function     DOHQueryBothA(Host : String): Boolean;       { V8.71 does A then AAAA }
    function     DOHQueryAny(Host: String; QNumber: Integer;  { V8.64 }
                                    MultiRequests: Boolean = False) : Boolean;
  published
    { Published declarations }
    property DnsSrvUrl: string                      read  FDnsSrvUrl
                                                    write FDnsSrvUrl;
    property DebugLevel: THttpDebugLevel            read  FDebugLevel
                                                    write FDebugLevel;
    property OnDnsProg: THttpRestProgEvent          read  FOnDnsProg
                                                    write FOnDnsProg;
  end;

  { V8.71 Domain Name cache with DOH - DNS over HTTPS  }
  TIcsDomNameCacheHttps = class(TIcsDomainNameCache)
  private
    { Private declarations }
    FDnsQueryHttpss: array of TDnsQueryHttps;
    FDnsSrvUrlList: TStrings;
  protected
    { Protected declarations }
    function StartHttps(ItemNr: Integer): Boolean;
    function StartRequest(ItemNr: Integer): Boolean; override;
  public
    { Public declarations }
    constructor  Create (Aowner: TComponent); override;
    destructor   Destroy; override;
    published
  end;


{$ENDIF USE_SSL}

implementation

{$IFDEF USE_SSL}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TDnsQueryHttps V8.61 }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TDnsQueryHttps.Create (Aowner: TComponent);
begin
    inherited Create(AOwner);
    HttpRest := TSslHttpRest.Create(self);
    HttpRest.OnHttpRestProg := DnsRestProg;
    HttpRest.OnRestRequestDone := DnsRestRequestDone;
    FDnsSrvUrl := DnsPublicHttpsTable[0];
    FDebugLevel := DebugNone;
    FBothAReq := False;
    FMultiReqSeq := 0;
    FAnsTot := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TDnsQueryHttps.Destroy;
begin
    FreeAndNil(HttpRest);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQueryHttps.DnsRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
begin
    if Assigned(FOnDnsProg) then
        FOnDnsProg(Self, LogOption, Msg) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQueryHttps.DOHQueryAll(Host: String): Boolean;
begin
    FMultiReqSeq  := 1;
    FMultiHost := Host;
    FMultiReq := True;
    FBothAReq := False;
    FAnsTot := 0;
    Result := DOHQueryAny(FMultiHost, DnsAllReqTable[FMultiReqSeq], True);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.71 does A then AAAA }
function TDnsQueryHttps.DOHQueryBothA(Host : String) : Boolean;
begin
    FMultiReqSeq := 1;
    FMultiReq := True;
    FMultiHost := Host;
    FBothAReq := True;
    FAnsTot := 0;
    Result := DOHQueryAny(FMultiHost, DnsQueryA, True);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQueryHttps.DOHQueryAny(Host: String; QNumber: integer; MultiRequests: Boolean = False): Boolean;
var
    QueryBuf: AnsiString;
    QueryLen, StatCode: Integer;
begin
    Result := False;
    if Pos('https://', FDnsSrvUrl) <> 1 then begin
        DiagLog('Must Specify DNS over HTTPS Server URL');
        Exit;
    end;
    ResetCounters(QNumber);         { V8.71 }
    if NOT FMultiReq then
        FAnsTot := 0;  { V8.61 reset result records }
    if NOT MultiRequests then
        FAnsTot := 0;  { reset result records }
    HttpRest.RestParams.Clear;
    HttpRest.DebugLevel := FDebugLevel;
    HttpRest.Accept := MimeDnsMess;
    HttpRest.ContentTypePost := MimeDnsMess;
    HttpRest.NoCache := True;

// build binary wire format request per RFC8484, same as UDP requests RFC1035,
// but ID always 0, so we build and parse requests with TDnsQuery component
    SetLength(QueryBuf, 512);
    BuildRequestHeader(PDnsRequestHeader(@QueryBuf[1]),0,  DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
    QueryLen := BuildQuestionSection(@QueryBuf[SizeOf(TDnsRequestHeader) + 1], IcsTrim(Host), QNumber, DnsClassIN);  { V8.64 }
    QueryLen := QueryLen + SizeOf(TDnsRequestHeader);
    SetLength(QueryBuf, QueryLen);
    DiagLog('Contacting: ' + FDnsSrvUrl + ' for query: ' + FindDnsReqTypeName(QNumber));  { V8.71 }
    StatCode := HttpRest.RestRequest(httpPOST, FDnsSrvUrl, True, String(QueryBuf));  // async request
    Result := (StatCode = 0);  // raises exception on failure
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQueryHttps.DnsRestRequestDone(Sender: TObject; RqType: THttpRequest; ErrCode: Word);
var
    RespBuf: AnsiString;
begin
    if RqType <> httpPOST then Exit;    { V8.68 don't care }
    if ErrCode <> 0 then begin
        DiagLog('HTTPS Request failed, error #' + IntToStr(ErrCode) +
                          '. Status = ' + IntToStr(HttpRest.StatusCode) + ' - ' + HttpRest.ReasonPhrase);
        FMultiReqSeq := 0;
        FMultiReq := False;
        FBothAReq := False;
        TriggerRequestDone(ErrCode);
        Exit;
    end;
    if (HttpRest.StatusCode = 200) and (HttpRest.ContentType = MimeDnsMess) then begin
        RespBuf := HttpRest.ResponseOctet;
        DiagLog('HTTPS Request completed OK, Decoding Response');
        if DecodeWireResp(@RespBuf[1], Length(RespBuf)) then begin     { V8.71 was ContentLength }

         // if simulating ALL request or both A and AAAA, make next request in sequence
            if FMultiReqSeq > 0 then begin
                FMultiReqSeq := FMultiReqSeq + 1;
                if FBothAReq then begin     { V8.71 A and AAAA }
                    if (FMultiReqSeq = 2) then begin
                        DOHQueryAny(FMultiHost, DnsQueryAAAA, True);
                        Exit;
                    end;
                end
                else if FMultiReqSeq <= DnsAllReqTot then begin
                    DOHQueryAny(FMultiHost, DnsAllReqTable[FMultiReqSeq], True);
                    Exit;
                end;
                FMultiReqSeq := 0;
                FMultiReq := False;
                FBothAReq := False;
            end;
            TriggerRequestDone(0);  // all done
        end
        else
            TriggerRequestDone(99);
    end
    else
       TriggerRequestDone(HttpRest.StatusCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsDomNameCacheHttps V8.71 }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsDomNameCacheHttps.Create (Aowner: TComponent);
begin
    FDnsSrvUrlList := TStringList.Create;
    inherited Create(AOwner);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsDomNameCacheHttps.Destroy;
begin
    FreeAndNil(FDnsSrvUrlList);
    SetLength(FDnsQueryHttpss, 0);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomNameCacheHttps.StartHttps(ItemNr: Integer): Boolean;
var
    I, SocNr: Integer;
begin
    SocNr := 0;
    Result := False;

// create socket look control table, if needed
    if Length(FCntlLookups) < FMaxLookups then
        SetLength(FCntlLookups, FMaxLookups + 1);

    for I := 1 to FMaxLookups do begin            // base 1 so zero is illegal
        if (NOT FCntlLookups[I].Busy) then begin
            Socnr := I ;
            Break;
        end;
    end;

// all query components busy, add to lookup queue
    if SocNr <= 0 then begin
        FLookupQu.Add(ItemNr);
        LogEvent('DnsQueriesHttps all Busy, Request Queued for ' + FDNItems[ItemNr].Request);
        Exit;
    end;
    FDNItems[ItemNr].DNState := StateWaiting;
    FDNItems[ItemNr].TimeStamp := Now;
    FCntlLookups[SocNr].Busy := True;
    FCntlLookups[SocNr].ItemNr := ItemNr;
    FCntlLookups[SocNr].Request := FDNItems[ItemNr].Request;
    FCntlLookups[SocNr].StartTick := IcsGetTickCount64;
    try
        if Length(FDnsQueryHttpss) <= SocNr then begin
            SetLength(FDnsQueryHttpss, SocNr + 5);   // only create a max five spare components at a time
            for I := Socnr to Length(FDnsQueryHttpss) do
                FDnsQueryHttpss[I] := Nil;
        end;
        if NOT Assigned(FDnsQueryHttpss[SocNr]) then begin
            FDnsQueryHttpss[SocNr] := TDnsQueryHttps.Create(Self);
            FDnsQueryHttpss[SocNr].OnRequestDone := DnsQueryRequestDone;
            FDnsQueryHttpss[SocNr].OnLogEvent := DnsLogEvent;
            FDnsQueryHttpss[SocNr].DebugLevel := DebugConn;
            FDnsQueryHttpss[SocNr].Tag := SocNr;
            if FDnsSrvUrlList.Count > 0 then
                FDnsQueryHttpss[SocNr].FDnsSrvUrl := FDnsSrvUrlList[0]
            else
                FDnsQueryHttpss[SocNr].FDnsSrvUrl := DnsPublicHttpsTable[0];
        //    FDnsQueryHttpss[SocNr].DnsSrvUrlList.Assign(FDnsServerList);
        end;
        if FDNItems[ItemNr].DNReqType = ReqTypeDnsBack then
            FDnsQueryHttpss[SocNr].DOHQueryAny(FDNItems[ItemNr].Request, DnsQueryPTR)
        else begin
            if FDNItems[ItemNr].ReqFamily = sfIPv6 then
                FDnsQueryHttpss[SocNr].DOHQueryAny(FDNItems[ItemNr].Request, DnsQueryAAAA)
            else if FDNItems[ItemNr].ReqFamily = sfIPv4 then
                FDnsQueryHttpss[SocNr].DOHQueryAny(FDNItems[ItemNr].Request, DnsQueryA)
            else
                FDnsQueryHttpss[SocNr].DOHQueryBothA(FDNItems[ItemNr].Request);
        end;
        LogEvent('DnsQueryHttps-' + IntToStr(SocNr) + ' Started Look-up for ' + FDNItems[ItemNr].Request);
        Result := True;
    except
        LogEvent('Exception Starting DnsQueryHttps - ' + IcsGetExceptMess (ExceptObject));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomNameCacheHttps.StartRequest(ItemNr: Integer): Boolean;
begin
    Result := False;
    if (FDNItems[ItemNr].DNReqType <> ReqTypeNetBios) and (FDNMethod = MethodHttps) then begin
        Result := StartHttps(ItemNr);
    end
    else
        Inherited StartRequest(ItemNr);    // handle winsock and dnsquery
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$ENDIF USE_SSL}

end.
