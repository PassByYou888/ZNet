{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Author:       Angus Robertson, Magenta Systems Ltd
Description:  TIcsHttpMulti is a high level HTTP Delphi component that allows
              downloading of multiple files from an HTTP server using full URLs,
              or listed by parsing links from a web page, using a single
              function call.
Creation:     May 2001
Updated:      Aug 2023
Version:      V9.0
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


TIcsHttpMulti is a high level HTTP Delphi component that allows downloading of
multiple files from an HTTP server using full URLs, or listed by parsing
links from a web page, using a single function call.  The component handles
listing local and remote files, and comparing them to avoid unnecessary
transfers, selection using a file mask, unzipping of downloaded files.
A progress event provides various levels of information for logging or
display, depending upon application requirements, and allows downloads
to be cancelled.

TIcsHttpMulti descends from ICS THttpCli, and publishes all it's properties and events.
The component is undocumented, except for source code comments, but end user help is
available describing most of the functionality in Magenta Systems DUN Manager
application (from https://www.magsys.co.uk/dunman/), look under Scheduled Task
Properties, HTTP Download. DUN Manager also provides a compiled test application for
TIcsHttpMulti (but DM source code is not available).


Main functions:

Download - download a list of URLs, optionally parsing HTML for links




08 May 2001  - baseline
6 June 2001  - added ReplRO to replace read only files on download
              support # at end of URL for section
8 June 2001  - added Fr to TIcsFileRec elements so names are unique
14 June 2001 - remove section from URL
25 June 2001 - added LogLevelDelimFile and LogLevelDelimTot to return info
                 for each file suitable for further processing
19 July 2001 - using InetParseDate in magsubs4, ICS fix
10 Sept 2001 - allow for bad dates, corrected build URL relative to root
12 Oct 2001  - / at start of URL is in root so ignore directory
               keep parsed file if mask matches
26 Oct 2001  - skip blank or * lines
12 Dec 2001  - added TIcsTaskResult to distinguish results better
               clear LastResponse before http in case rubbish appears
22 Dec 2001  - added unzipping of downloads
08 Oct 2002  - no longer using trailing slashes on directories internally, still seems to work
09 Dec 2002  - always using UTC dates
4 Jan 2003   - file level info no longer logs URL scanning
7 March 2003 - added SSL/HTTPS support, needs non-free version of ICS component
7 Apr 2003   - never using UTC dates!!!
                short delay before HEAD otherwise URLs seem to be skipped
2 May 2003   - changed GetDirList with new params
17 Jun 2003  - added duration (ms) to LogLevelDelimFile events
24 Aug 2003  - ensure unzip directory exists, might help trap unzipping errors
               unzip errors in task log
11 Oct 2003  - added unzip message handler, VCLZIP 3 only
30 Oct 2003  - strip of section and argument when parsing URLs
31 Dec 2003  - using magsubs1 for common stuff instead of magsubs4
28 Aug 2004  - ICS ParseUrl has moved from httpprot to icsurl
               override WaitUntilReady in ftpcli.pas to avoid slow down problems
30 Sept 2004 - supporting NTLM authentication, messing with progress messages
13 Jan 2005  - removed sleep from WaitUntilReady
30 July 2005 - TIcsFindList moved to magclasses.pas
3 Nov 2005   - fixed FTP links for skipped
28 Nov 2005  - now only supporting ICS v5 SSL dated Nov 2005 or later
31 Dec 2005  - fix for divide by zero on fast download
               support for icslogger (not here)
18 Sep 2006  - unit is now MagentaHttp supporting ICS V6
6 Nov 2006   - new SSL session cache
03 Mar 2008  - added DelimActualSize which if non-zero is actual size relating to duration
               fix content field parsing for text failed if multiple arguments
7 Aug 2008   - updated for latest ICS V6 and V7, and for Delphi 2009 compatibility
18 Nov 2008  - no changes
7 Aug 2010   - 3.5 - fixed various string casts for D2009 and later
11 Aug 2011  - 3.7 - now registered in MagentaXferReg
20 Oct 2011  - 3.8 - log time and speed of each download
               slow down progress updates for better performance
               support 64-bit downloads
               added NoProgress property to skip LogLevelProg progress log events
               most file sizes now reported in Kbytes. Mbytes, Gbytes instead of bytes
24 Aug 2012 - 4.0 - updated to support ICS V8 with IPv6
11 Mar 2013 - 4.1 - default to allowing IPv4 or IPv6 host names
12 May 2015 - 4.2 - better SSL handshake reporting
              added SSL server certificate checking
23 Oct 2015 - 4.3 - better SSL certificate reporting
              fixed cert checking corrupting download file name
              failed certificate report error as last HTTP error
              Warning, self signed certificates are not trusted
              better failure reason logging
7 Dec 2016  - 4.5 - more friendly errors
              removed TX509Ex now using TX509Base
              using OpenSSL certificate verification host checking
              set SSL session caching correctly
              only check and report SSL certificates once per session
6 Mar 2017  - 4.6 - simplified SSL certificate reporting
              set SSL security level low, ideally should be configurable
18 Jun 2018 - 4.7 - support ResponseNoException
              fixed HTTP to HTTPS relocation failed unless HTTPS URL precessed
                first, always set SslContext
              Use built-in CA bundle if file missing.
              Added SslCliSecurity property to set security level to TSslCliSecurity
18 Mar 2019 - V8.60 - Adapted for main ICS packages and FMX support.
              TMagHttp to TIcsHttpMulti.
              Most Types have Ics added, so: TIcsTaskResult now TIcsTaskResult.
              No longer needs Forms.
              Added MaxRetries to repeat each URL, might help if multiple DNS
                addresses are returned.
17 Apr 2019 - V8.61 - Base component now has more response headers, don't need
                 to search for them.
Aug 7, 2019  V8.62 Builds without AUTO_X509_CERTS or USE_SSL
Dec 09, 2020 V8.65 - Pending, lots of Windows only APIs need Posix conversion
                    Renamed Ics.Posix.Messages.pas to Ics.Posix.PXMessages.pas.
04 Feb 2021 - V8.66 - Added ParseLevels property to follow links on parsed web
                 page to lower level pages, which are also parsed.  Ignores
                 parent links without a file extension, and external links,
                 but requires a specific file extension to located (ie .avi).
                 This allows the component to download files indexed using HTTP
                 from CCTV camera memory cards.
              Ensure keep alive works.
May 24, 2021 - V8.67 - Replaced Stream.Seek with Stream.Position.
Dec 01, 2021 - V8.68 - Fixed problem that meant large files failed download
                 with only an abort error if too large for TMemoryStream,
                 now downloaded to TFileStream with .part extension and renamed
                 up successful completion.
               If a partial file downloads, don't delete it if KeepPartDown=True.
               Diagnostic log session closed.
               Set Sync request timeout to 300 secs.
               Report RequestDoneError on failure.
               Correctly download files using chunked coding without known size,
                 note these show with size -1 since not known until complete.
               Log BgException.
Apr 14, 2022 - V8.69 - Previously the FtpSslRevocation property was only effective when
                 checking the windows certificate store, now it also works with bundle
                 files using the TOcspHttp component and OCSP stapling if available.
Oct 20, 2022 - V8.70 - Removed VclZip support, replaced by TZipFile from System.Zip unit for D10
                 and later, if you still need zipping for earlier compilers keep an old version
                 of this unit.
               Removed platform warning with FileSetAttr.
Mar 27, 2023 - V8.71 - Ensure inherited destroy called.
               Using Int64 ticks.
               Handle exception parsing strange web pages.
Aug 08, 2023 V9.0  Updated version to major release 9.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsHttpMulti;
{$ENDIF}

{$I Include\Z.ICS9.OverbyteIcsDefs.inc}

{$IFDEF COMPILER23_UP}      { TZipFile may work with XE2 and better, but not tested }
    {$DEFINE ZIPPING}       { V8.70 use System.Zip }
{$ENDIF}

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

{$IFDEF MSWINDOWS}
{$IFDEF USE_SSL}

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    Z.ICS9.OverbyteIcsWinCrypt,
{$ENDIF}
{$IFDEF POSIX}
    Posix.Time,
    Z.ICS9.Ics.Posix.WinTypes,
    Z.ICS9.Ics.Posix.PXMessages,
{$ENDIF}
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    {$Ifdef Rtl_Namespaces}System.Masks{$Else}Masks{$Endif},
    {$IFDEF RTL_NAMESPACES}System.TypInfo{$ELSE}TypInfo{$ENDIF},       { V8.68 }
{$IFDEF FMX}
    Z.ICS9.Ics.Fmx.OverbyteIcsWndControl,
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocket,
    Z.ICS9.Ics.Fmx.OverbyteIcsHttpProt,
    Z.ICS9.Ics.Fmx.OverbyteIcsBlacklist,
    Z.ICS9.Ics.Fmx.OverbyteIcsFileCopy,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslSessionCache,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslX509Utils,
    Z.ICS9.Ics.Fmx.OverbyteIcsMsSslUtils,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslHttpRest,  { V8.69 }
{$ELSE}
    Z.ICS9.OverbyteIcsWndControl,
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsHttpProt,
    Z.ICS9.OverbyteIcsBlacklist,
    Z.ICS9.OverbyteIcsFileCopy,
    Z.ICS9.OverbyteIcsSslSessionCache,
    Z.ICS9.OverbyteIcsSslX509Utils,
    Z.ICS9.OverbyteIcsMsSslUtils,
    Z.ICS9.OverbyteIcsSslHttpRest,  { V8.69 }
{$ENDIF FMX}
  Z.ICS9.OverByteIcsFtpSrvT,
  Z.ICS9.OverbyteIcsHtmlPars,
  Z.ICS9.OverbyteIcsUrl,
  Z.ICS9.OverbyteIcsTypes,
  Z.ICS9.OverbyteIcsUtils,
  Z.ICS9.OverbyteIcsLogger
  {$IFDEF Zipping}
    ,System.Zip            { V8.70 VclZip gone }
  {$ENDIF}
  , Z.ICS9.OverbyteIcsSSLEAY,
  Z.ICS9.OverbyteIcsLIBEAY,
  Z.ICS9.OverbyteIcsStreams,    { V8.68 }
  Z.ICS9.OverbyteIcsTicks64;    { V8.71 }

{ NOTE - these components only build with SSL, there is no non-SSL option }



const
    HttpMultiCopyRight : String = ' TIcsHttpMulti (c) 2023 V9.0 ';

type
    THttpSslVerifyMethod = (httpSslVerNone, httpSslVerBundle, httpSslVerWinStore) ;   // 20 Apr 2015

type
  TIcsHttpMulti = class(TSslHttpCli)
  private
    { Private declarations }
        fCancelFlag: boolean ;
        fProgFileSize: int64 ;
        fProgMessBase: string ;
        fIcsFileCopy: TIcsFileCopy ;
        fSslSessCache: boolean ;
        fSslContext: TSslContext ;
        fExternalSslSessionCache: TSslAvlSessionCache ;
        fHttpSslVerMethod: THttpSslVerifyMethod;  // 20 Apr 2015
        fSslRootFile: string ;  // 20 Apr 2015
        fSslRevocation: boolean;   // 20 Apr 2015
        fSslReportChain: boolean ;  // 20 Apr 2015
        fMsCertChainEngine: TMsCertChainEngine;   // 20 Apr 2015
        FSslCliSecurity: TSslCliSecurity;   // June 2018
        fMaxAttempts: integer ;    { V8.60  }
        fAttemptDelay: integer ;   { V8.60  }
        fParseLevels: Integer;     { V8.66 }
        fKeepPartDown: Boolean;    { V8.68 }
        FOcspHttp: TOcspHttp;                 { V8.69 }
  protected
    { Protected declarations }
        fUrlList: String ;
        fSrcMask: String ;
        fDownDir: String ;
        fKeepPath: boolean ;
        fKeepHost: boolean ;
        fParseHTML: Boolean ;
        fRepl: TIcsFileCopyRepl ;
        fReplRO: boolean ;
        fLogFiles: boolean ;
        fLogProt: boolean ;
        fLogLDir: boolean ;
        fLogRDir: boolean ;
        fCopyEvent: TBulkCopyEvent ;
        fTotProcFiles: integer ;
        fProcOKFiles: integer ;
        fProcFailFiles: integer ;
        fSkippedFiles: integer ;
        fReqResponse: string ;
        fNoProgress: boolean ;       // 10 Oct 2011
        fLastProgTick: Int64 ;       // 10 Oct 2011   { V8.71 }
        fProgressSecs: integer ;     // 10 Oct 2011

{$IFDEF Zipping}
        fZipDownDel: Boolean ;
        fZipped: Boolean ;
        fZipPath: TIcsZipPath ;
        fZipDir: String ;
    procedure ZipProgressEvent(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);   { V8.70 }
{$ENDIF}

    procedure doCopyEvent (LogLevel: TIcsCopyLogLevel ; Info: string) ;
    procedure onHttpDataEvent (Sender : TObject; Buffer : Pointer; Len : Integer);
    procedure onHttpCommand (Sender: TObject; var S: String) ;
    procedure onHttpHeaderData (Sender : TObject);
    procedure onHttpSessionConnected (Sender : TObject);
    procedure onHttpLocationChange(Sender : TObject);
    procedure onHttpStateChange(Sender : TObject);                    { V8.68 }
    procedure onHttpSessionClosed(Sender: TObject);                   { V8.68 }
    procedure onHttpBgException(Sender: TObject; E: Exception; var CanClose: Boolean);    { V8.68 }
    procedure onAuthStep (Sender : TObject);
    procedure OnHttpSslVerifyPeer(Sender: TObject; var Ok: Integer;
      Cert : TX509Base);
    procedure OnHttpSslCliNewSession(Sender: TObject; SslSession: Pointer;
                                    WasReused: Boolean; var IncRefCount : Boolean);
    procedure OnHttpSslCliGetSession(Sender: TObject; var SslSession: Pointer;
                                                            var FreeSession : Boolean);
    procedure OnHttpSslHandshakeDone(Sender: TObject; ErrCode: Word;
                                      PeerCert: TX509Base; var Disconnect: Boolean);
    procedure OnHttpSslCliCertRequest(Sender: TObject; var Cert: TX509Base);
    procedure onMagCopyEvent (LogLevel: TIcsCopyLogLevel ; Info: string ;
                                                  var Cancel: boolean) ;
    procedure DoRequestSync(Rq : THttpRequest); Override ;
    procedure IcsProgEvent (Sender: TObject; LogOption: TLogOption; const Msg : String) ;   { V8.69 }
  public
    { Public declarations }
        SrcFiles: TIcsFDirRecs  ;
        SrcFileList: TIcsFindList ;
        TotSrcFiles: integer ;
        TarFiles: TIcsFDirRecs  ;
        TarFileList: TIcsFindList ;
        LogRcvdCerts: boolean ; // 20 Apr 2015

        constructor Create (Aowner: TComponent) ; override ;
        destructor  Destroy ; override ;
        function Download (CheckFiles: boolean): TIcsTaskResult ;
        procedure Cancel ;

  published
    { Published declarations }
    property UrlList: String            read fUrlList    write fUrlList ;
    property SrcMask: String            read fSrcMask    write fSrcMask ;
    property DownDir: String            read fDownDir    write fDownDir ;
    property KeepPath: Boolean          read fKeepPath   write fKeepPath ;
    property KeepHost: Boolean          read fKeepHost   write fKeepHost ;
    property ParseHTML: Boolean         read fParseHTML  write fParseHTML ;
    property Repl: TIcsFileCopyRepl        read fRepl       write fRepl ;
    property ReplRO: boolean            read fReplRO     write fReplRO ;
    property LogFiles: boolean          read fLogFiles   write fLogFiles ;
    property LogProt: boolean           read fLogProt    write fLogProt ;
    property LogLDir: boolean           read fLogLDir    write fLogLDir ;
    property LogRDir: boolean           read fLogRDir    write fLogRDir ;
    property CopyEvent: TBulkCopyEvent  read fCopyEvent  write fCopyEvent ;
    property SslSessCache: boolean      read fSslSessCache write fSslSessCache ;
    property HttpSslVerMethod: THttpSslVerifyMethod read fHttpSslVerMethod write fHttpSslVerMethod ;  // 20 Apr 2015
    property SslRootFile: string        read fSslRootFile write fSslRootFile ;         // 20 Apr 2015
    property SslRevocation: boolean     read fSslRevocation write fSslRevocation ;     // 20 Apr 2015
    property SslReportChain: boolean    read fSslReportChain write fSslReportChain;   // 20 Apr 2015
    property SslCliSecurity: TSslCliSecurity read fSslCliSecurity  write fSslCliSecurity;   // June 2018
    property NoProgress: boolean        read fNoProgress     write fNoProgress ;    // 10 Oct 2011
    property ProgressSecs: integer      read fProgressSecs   write fProgressSecs ;  // 10 Oct 2011
    property MaxAttempts: integer       read fMaxAttempts    write fMaxAttempts ;   { V8.60  }
    property AttemptDelay: integer      read fAttemptDelay   write fAttemptDelay ;  { V8.60  }
    property ParseLevels: integer       read fParseLevels    write fParseLevels ;   { V8.66  }
    property KeepPartDown: Boolean      read fKeepPartDown   write fKeepPartDown ;  { V8.68 }
    property OcspHttp: TOcspHttp        read FOcspHttp       write FOcspHttp;               { V8.69 }
    property TotProcFiles: integer      read fTotProcFiles ;
    property ProcOKFiles: integer       read fProcOKFiles ;
    property ProcFailFiles: integer     read fProcFailFiles ;
    property SkippedFiles: integer      read fSkippedFiles ;
    property ReqResponse: string        read fReqResponse ;
  {$IFDEF Zipping}
    property ZipDownDel: Boolean       read fZipDownDel     write fZipDownDel ;
    property Zipped: Boolean           read fZipped         write fZipped ;
    property ZipPath: TIcsZipPath         read fZipPath        write fZipPath ;
    property ZipDir: String            read fZipDir         write fZipDir ;
  {$ENDIF}
  end;

    procedure ParseExURL (const url: string; var Proto, User, Pass,
                            Host, Port, Dirs, Fname, Section, Query: string) ;
    function BuildExURL (const Proto, User, Pass, Host, Port,
                                Dirs, Fname, Section, Query: string): string ;
    function RelativeName (Dirs, Rname: string): string ;

{$ENDIF USE_SSL}
{$ENDIF MSWINDOWS}

implementation

{$IFDEF MSWINDOWS}
{$IFDEF USE_SSL}

function ExtractUNIXName(const FileName: string): string;
var
    I: Integer;
begin
    I := LastDelimiter('/', FileName);
    Result := Copy(FileName, I + 1, MaxInt);
end;

constructor TIcsHttpMulti.Create(Aowner:TComponent);
begin
    inherited create(AOwner);
 // winsock bug fix for fast connections
    CtrlSocket.ComponentOptions := [wsoNoReceiveLoop] ;
    fIcsFileCopy := TIcsFileCopy.Create (self) ;
    fIcsFileCopy.CopyEvent := onMagCopyEvent ;
    OnDocData := onHttpDataEvent ;
    OnCommand := onHttpCommand ;
    OnHeaderData := onHttpHeaderData ;
    OnSessionConnected := onHttpSessionConnected ;
    OnLocationChange := onHttpLocationChange ;
    OnStateChange := onHttpStateChange;             { V8.68 }
    onSessionClosed := onHttpSessionClosed;         { V8.68 }
    OnBgException := onHttpBgException;             { V8.68 }
    fProgressSecs := 2 ;
    FSocketFamily := sfAny ;  // March 2013 allow IPv4 or IPv6
    fSslSessCache := true ;
    fExternalSslSessionCache := nil ;  // 9 Nov 2005
    OnSslVerifyPeer := OnHttpSslVerifyPeer ;
    OnSslCliGetSession := OnHttpSslCliGetSession ;
    OnSslCliNewSession := OnHttpSslCliNewSession ;
    OnSslHandshakeDone := OnHttpSslHandshakeDone ;
    OnSslCliCertRequest := OnHttpSslCliCertRequest ;
    fSslContext := TSslContext.Create (self) ;
    SslContext := fSslContext ;
    SslContext.SslVerifyPeer := false ;
    fHttpSslVerMethod := HttpSslVerNone ;  // 20 Apr 2015
    fSslRootFile := 'RootCaCertsBundle.pem' ; // 20 Apr 2015
    fSslCliSecurity := sslCliSecIgnore;  // June 2018
    SrcFileList := TIcsFindList.Create ;
    TarFileList := TIcsFindList.Create ;
    SetLength (SrcFiles, 0) ;
    SetLength (TarFiles, 0) ;
    TotSrcFiles := 0 ;
    fTotProcFiles := 0 ;
    fProcOKFiles := 0 ;
    fProcFailFiles := 0 ;
    fSkippedFiles := 0 ;
    fCancelFlag := false ;
    fMaxAttempts := 3 ;
    fAttemptDelay := 1 ;   // seconds
    LocationChangeMaxCount := 1;  // V8.66 really don't want relocations
    FOcspHttp := TOcspHttp.Create(Self);   { V8.69 }
    FOcspHttp.OnOcspProg := IcsProgEvent;   { V8.69 }
    FOcspHttp.CacheFName := 'ocshttpcache.recs';   { V8.69 }
end ;

destructor TIcsHttpMulti.Destroy;
begin
{ V8.71 Ensure inherited destroy called }
    try
        FreeAndNil (fIcsFileCopy) ;
        FreeAndNil (FOcspHttp);    { V8.69 }
        FreeAndNil (fIcsFileCopy) ;
        FreeAndNil (SrcFileList) ;
        FreeAndNil (TarFileList) ;
        FreeAndNil (FMsCertChainEngine) ;
        FreeAndNil (fExternalSslSessionCache) ;
        FreeAndNil (fSslContext) ;
    finally
        inherited Destroy;
    end;
end;

procedure TIcsHttpMulti.DoRequestSync(Rq : THttpRequest);
begin
    DoRequestAsync(Rq);

    if FMultiThreaded then begin
        while FState <> httpReady do begin
            FCtrlSocket.ProcessMessages;
        end;
    end
    else begin
        while FState <> httpReady do begin
            FCtrlSocket.ProcessMessages;
        end;
    end;

    if NOT FResponseNoException then begin  { V8.54 should error exceptions be skipped? May 2018 }
        if FStatusCode > 401 then
            raise EHttpException.Create(FReasonPhrase, FStatusCode);
    end;
end;

// borrowed from httpapp

function StatusString(StatusCode: Integer): string;
begin
    case StatusCode of
        100: Result := 'Continue';                                 {do not localize}
        101: Result := 'Switching Protocols';                      {do not localize}
        200: Result := 'OK';                                       {do not localize}
        201: Result := 'Created';                                  {do not localize}
        202: Result := 'Accepted';                                 {do not localize}
        203: Result := 'Non-Authoritative Information';            {do not localize}
        204: Result := 'No Content';                               {do not localize}
        205: Result := 'Reset Content';                            {do not localize}
        206: Result := 'Partial Content';                          {do not localize}
        300: Result := 'Multiple Choices';                         {do not localize}
        301: Result := 'Moved Permanently';                        {do not localize}
        302: Result := 'Moved Temporarily';                        {do not localize}
        303: Result := 'See Other';                                {do not localize}
        304: Result := 'Not Modified';                             {do not localize}
        305: Result := 'Use Proxy';                                {do not localize}
        400: Result := 'Bad Request';                              {do not localize}
        401: Result := 'Unauthorized';                             {do not localize}
        402: Result := 'Payment Required';                         {do not localize}
        403: Result := 'Forbidden';                                {do not localize}
        404: Result := 'Not Found';                                {do not localize}
        405: Result := 'Method Not Allowed';                       {do not localize}
        406: Result := 'None Acceptable';                          {do not localize}
        407: Result := 'Proxy Authentication Required';            {do not localize}
        408: Result := 'Request Timeout';                          {do not localize}
        409: Result := 'Conflict';                                 {do not localize}
        410: Result := 'Gone';                                     {do not localize}
        411: Result := 'Length Required';                          {do not localize}
        412: Result := 'Unless True';                              {do not localize}
        500: Result := 'Internal Server Error';                    {do not localize}
        501: Result := 'Not Implemented';                          {do not localize}
        502: Result := 'Bad Gateway';                              {do not localize}
        503: Result := 'Service Unavailable';                      {do not localize}
        504: Result := 'Gateway Timeout';                          {do not localize}
    else
        Result := '';
    end
end;

{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path[?query]         }

// break down URL into its constituents

procedure ParseExURL (const url: string; var Proto, User, Pass, Host, Port,
                                         Dirs, Fname, Section, Query: string) ;
var
    path: string ;
    nsep1, nsep2: integer ;
begin
    Z.ICS9.OverbyteIcsUrl.ParseURL (url, Proto, User, Pass, Host, Port, path) ;
    Dirs := '' ;
    Fname := '' ;
    Section := '' ;
    Query := '' ;
    if path = '' then exit ;
    nsep1 := Pos ('#', path) ;

// remove section from path and keep it
    if (nsep1 > 0) then
    begin
        if nsep1 < length (path) then
        begin
            Section := copy (path, succ (nsep1), 999) ;
            nsep2 := Pos ('?', Section) ;
            if (nsep2 > 0) then
            begin
                if nsep2 < length (Section) then Query := copy (Section, succ (nsep2), 999) ;
                Section := copy (Section, 1, pred (nsep2)) ;
            end ;
        end ;
        path := copy (path, 1, pred (nsep1)) ;
    end
// remove query from path and keep it
    else
    begin
        nsep2 := Pos ('?', path) ;
        if (nsep2 > 0) then
        begin
            if nsep2 < length (path) then Query := copy (path, succ (nsep2), 999) ;
            path := copy (path, 1, pred (nsep2)) ;
        end ;
    end ;

// remove file name from path and keep it and Dirs separately (no leading / on either)
    nsep1 := LastDelimiter ('/', path);
    if nsep1 > 1 then Dirs := copy (path, 2, pred (nsep1)) ;
    Fname := copy (path, succ (nsep1), 99) ;
end ;

// build URL from the many (optional) constituent parts

function BuildExURL (const Proto, User, Pass, Host, Port,
                                 Dirs, Fname, Section, Query: string): string ;
begin
    result := Proto + '://' ;
    if User <> '' then
    begin
        Result := Result + User ;
        if Pass <> '' then Result := Result + ':' + Pass ;
        Result := Result + '@' ;
    end ;
    Result := Result + Host ;
    if Port <> '' then Result := Result + ':' + Port ;
    Result := Result + '/' ;
    if Dirs <> '' then
    begin
        Result := Result + Dirs ;
        if Dirs [Length (Dirs)] <> '/' then Result := Result + '/' ;
    end ;
    if Pos('/', Fname) = 1 then        // V8.66 strip leading / from file
        Result := Result + Copy(Fname, 2, 999)
    else
        Result := Result + Fname ;
    if Section <> '' then Result := Result + '#' + Section ;
    if Query <> '' then Result := Result + '?' + Query ;
end ;

// builds a new relative combined Dirs/Fname from relative name
// /magsys/other/ and ../mbs/index.htm gives /magsys/mbs/index.htm
// /magsys/other/ and /mbs/index.htm gives /mbs/index.htm

function RelativeName (Dirs, Rname: String): string ;
var
    nsep: integer ;
begin
    result := Rname ;
    Dirs := trim (Dirs) ;
    if Dirs <> '' then
    begin
        if Dirs [Length (Dirs)] = '/' then
                      SetLength (Dirs, Pred (Length (Dirs))) ;  // remove last /
    end ;
    while (Pos ('../', Rname) = 1) do
    begin
        Rname := Copy (Rname, 4, 999) ;  // remove ../
        nsep := LastDelimiter ('/', Dirs) ;
        if nsep > 1 then
            Dirs := Copy (Dirs, 1, pred (nsep))
        else
            Dirs := '' ;
    end ;
    if Length (Rname) = 0 then exit ;
    if (Rname [1] = '/') then Dirs := '' ;    // assume root
    if Dirs <> '' then
        Result := Dirs + '/' + Rname
    else
        Result := Rname ;
end ;

procedure TIcsHttpMulti.onMagCopyEvent (LogLevel: TIcsCopyLogLevel ; Info: string ;
                                                  var Cancel: boolean) ;
begin
    doCopyEvent (LogLevel, Info) ;
    Cancel := fCancelFlag ;
end ;

procedure TIcsHttpMulti.doCopyEvent (LogLevel: TIcsCopyLogLevel ; Info: string) ;
begin
    if Assigned (CopyEvent) then
    begin
        CopyEvent (LogLevel, Info, fCancelFlag) ;
    end ;
end ;

procedure TIcsHttpMulti.IcsProgEvent (Sender: TObject; LogOption: TLogOption; const Msg : String) ;   { V8.69 }
begin
    doCopyEvent (LogLevelInfo, Msg) ;
end ;


procedure TIcsHttpMulti.onHttpBgException(Sender: TObject; E: Exception; var CanClose: Boolean);    { V8.68 }
begin
    doCopyEvent (LogLevelInfo, FURL + ' Fatal Exception: ' + IcsGetExceptMess(E)) ;
    CanClose := True;
end;

procedure TIcsHttpMulti.onHttpSessionConnected (Sender : TObject);
var
    S: String;
begin
    if FState = httpConnected then   { V8.60  }
        S := '= Connected OK to: '
    else
        S := '= Connection failed to: ';
    S := S + FHostname + ' (' + IcsFmtIpv6Addr(AddrResolvedStr) + ')';    { V8.60  }
    doCopyEvent (LogLevelDiag, S) ;
end ;

procedure TIcsHttpMulti.onHttpLocationChange(Sender : TObject);
begin
    doCopyEvent (LogLevelDiag, '= ' + FURL + ' Redirected to: ' + FLocation) ;
end ;

procedure TIcsHttpMulti.onHttpStateChange(Sender : TObject);     { V8.68 }
begin
//   doCopyEvent (LogLevelDiag, '= ' + FURL + ' State Changed to: ' + GetEnumName(TypeInfo(THttpState), Ord(FState)) +
//                                            ', FRequestDoneError=' + RequestDoneErrorStr + ', ReasonPhrase=' + ReasonPhrase);
end ;

procedure TIcsHttpMulti.onHttpSessionClosed(Sender: TObject);    { V8.68 }
begin
    doCopyEvent (LogLevelDiag, '= ' + FURL + ' Session Closed, Error: ' + RequestDoneErrorStr + ' - ' + ReasonPhrase);
end ;

procedure TIcsHttpMulti.onHttpCommand (Sender: TObject; var S: String) ;
begin
    doCopyEvent (LogLevelDiag, '> ' + S) ;
end;

procedure TIcsHttpMulti.onHttpHeaderData (Sender : TObject);
begin
    doCopyEvent (LogLevelDiag, '< ' + LastResponse) ;
end ;

procedure TIcsHttpMulti.onAuthStep (Sender : TObject);
begin
    doCopyEvent (LogLevelDiag, '= NTLM Authorisation Step ' + LastResponse) ;
end ;

procedure TIcsHttpMulti.onHttpDataEvent (Sender : TObject; Buffer : Pointer; Len : Integer);
var
    info: string ;
    newsize: int64 ;
begin
    if fProgFileSize < 0 then
        exit ;
    if fNoProgress then
        exit ; // 15 Feb 2011
    if fProgFileSize = 0 then
        exit ;
    newsize := RcvdStream.Size ;
    if (fProgressSecs > 0) and (newsize < fProgFileSize) and (NOT fCancelFlag) then // Oct 2011 slow down updates
    begin
        if (IcsElapsedSecs64 (fLastProgTick) < fProgressSecs) then    { V8.71 }
            exit ;
    end ;
    info := fProgMessBase + ', ' + IntToKByte (newsize) ;
    if fProgFileSize > 0 then
        info := info + ' of ' +  IntToKByte (fProgFileSize)  ;
    doCopyEvent (LogLevelProg, info) ;
    fLastProgTick := IcsGetTickCount64 ;              { V8.71 }
end ;

procedure TIcsHttpMulti.OnHttpSslVerifyPeer(Sender: TObject; var Ok: Integer; Cert : TX509Base);
var
    info: string ;
begin
    OK := 1; // don't check certificate until handshaking over
    if LogRcvdCerts then   // 20 Apr 2015
    begin
        info := 'Received Certificate, Depth ' + IntToStr (Cert.VerifyDepth) + #13#10 +
                 'Verify Result: ' + Cert.VerifyErrMsg + #13#10 +
                 Cert.CertInfo (true) + #13#10 ;  // Mar 2017 simplify
        doCopyEvent (LogLevelDiag, info);
    end;
end ;


procedure TIcsHttpMulti.OnHttpSslCliNewSession(Sender: TObject; SslSession: Pointer;
                                    WasReused: Boolean; var IncRefCount : Boolean) ;
var
    HttpCli: TSslHttpCli;
begin
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching.                                                              }
    if not fSslSessCache then Exit;  // March 2018 no logging if disabled
    doCopyEvent (LogLevelDiag, 'Starting SSL Session');
    if (not WasReused) then
    begin
        HttpCli := (Sender as TSslHttpCli);
        fExternalSslSessionCache.CacheCliSession(SslSession,
                           HttpCli.CtrlSocket.PeerAddr + HttpCli.CtrlSocket.PeerPort, IncRefCount);
        doCopyEvent (LogLevelDiag, 'Cache SSL Session: New');
    end
    else
        doCopyEvent (LogLevelDiag, 'Cache SSL Session: Reuse');
end;

procedure TIcsHttpMulti.OnHttpSslCliGetSession(Sender: TObject; var SslSession: Pointer;
                                                            var FreeSession : Boolean);
var
    HttpCli: TSslHttpCli;
begin
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching.                                                              }
    if not fSslSessCache then Exit;
    doCopyEvent (LogLevelDiag, 'Check for Old SSL Session');
    HttpCli := (Sender as TSslHttpCli);
    SslSession := fExternalSslSessionCache.GetCliSession(
                         HttpCli.CtrlSocket.PeerAddr + HttpCli.CtrlSocket.PeerPort, FreeSession);
    if Assigned (SslSession) then   // Dec 2016
        doCopyEvent (LogLevelDiag, 'Old SSL Session Found Cached')
    else
        doCopyEvent (LogLevelDiag, 'No Old SSL Session Cached');
end;

procedure TIcsHttpMulti.OnHttpSslHandshakeDone(Sender: TObject; ErrCode: Word;
                                    PeerCert: TX509Base; var Disconnect: Boolean);
var
    CertChain: TX509List;
    ChainVerifyResult: LongWord;
    info, VerifyInfo: String;
    Safe: Boolean;
    HttpCtl: TWSocket ;      // Dec 2016
begin
    HttpCtl := (Sender as TSslHttpCli).CtrlSocket ;  // Dec 2016

  // nothing much to do if SSL failed or event said disconnect
    if (ErrCode <> 0) or Disconnect then
    begin
        FReasonPhrase := HttpCtl.SslServerName + ' SSL Handshake Failed: ' + HttpCtl.SslHandshakeRespMsg;  // Oct 2015
        doCopyEvent (LogLevelInfo, FReasonPhrase) ;  // Dec 2014
        Disconnect := TRUE;
        exit;
    end  ;

    doCopyEvent (LogLevelInfo, HttpCtl.SslServerName + ' ' + HttpCtl.SslHandshakeRespMsg) ;     // Dec 2014
    if HttpCtl.SslSessionReused OR (fHttpSslVerMethod = HttpSslVerNone) then
    begin
        exit; // nothing to do, go ahead
    end ;

 // Property SslCertChain contains all certificates in current verify chain
    CertChain := HttpCtl.SslCertChain;

 // see if validating against Windows certificate store
    if fHttpSslVerMethod = HttpSslVerWinStore then
    begin
        // start engine
        if not Assigned (FMsCertChainEngine) then
            FMsCertChainEngine := TMsCertChainEngine.Create;

      // see if checking revoocation, CRL checks and OCSP checks in Vista+, very slow!!!!
        if fSslRevocation then
            FMsCertChainEngine.VerifyOptions := [mvoRevocationCheckChainExcludeRoot]
        else
            FMsCertChainEngine.VerifyOptions := [];

        // This option doesn't seem to work, at least when a DNS lookup fails
        FMsCertChainEngine.UrlRetrievalTimeoutMsec := 10000;

        { Pass the certificate and the chain certificates to the engine      }
        FMsCertChainEngine.VerifyCert (PeerCert, CertChain, ChainVerifyResult, True);

        Safe := (ChainVerifyResult = 0) or
                { We ignore the case if a revocation status is unknown.      }
                (ChainVerifyResult = CERT_TRUST_REVOCATION_STATUS_UNKNOWN) or
                (ChainVerifyResult = CERT_TRUST_IS_OFFLINE_REVOCATION) or
                (ChainVerifyResult = CERT_TRUST_REVOCATION_STATUS_UNKNOWN or
                                     CERT_TRUST_IS_OFFLINE_REVOCATION);

       { The MsChainVerifyErrorToStr function works on chain error codes     }
        VerifyInfo := MsChainVerifyErrorToStr (ChainVerifyResult); // Nov 2016

    // MSChain ignores host name, so see if it failed using OpenSSL
        if PeerCert.VerifyResult = X509_V_ERR_HOSTNAME_MISMATCH then begin  // Nov 2016
            Safe := False;
            VerifyInfo := PeerCert.FirstVerifyErrMsg;
        end;
    end
    else if fHttpSslVerMethod = HttpSslVerBundle then
    begin
        VerifyInfo := PeerCert.FirstVerifyErrMsg;   // Nov 2016
        Safe := (PeerCert.VerifyResult = X509_V_OK);   { check whether SSL chain verify result was OK }

      { V8.69 check OCSP to see if revoked, if we got a chain of certificates }
      { note this is a soft check, if we don't have a stapled OCSP response from the TLS handshake, we get it from an
        OCSP HTTP server and cache it but don't wait for the response. So next attempt comes from cache.  }
        if (Safe and FSslRevocation and PeerCert.IsCertLoaded and (CertChain.Count > 0)) then begin
            FOcspHttp.ClearOcsp;
            FOcspHttp.DebugLevel := DebugConn;
            FOcspHttp.OcspCert := PeerCert;
            FOcspHttp.OcspInters := CertChain;
            if (Length(HttpCtl.OcspStapleRaw) > 50) and
                 (HttpCtl.OcspStapleStatus = OCSP_RESPONSE_STATUS_SUCCESSFUL) then
                                        FOcspHttp.OcspRespRaw := HttpCtl.OcspStapleRaw;
            if FOcspHttp.CheckOcspRevoked(fSslContext.GetX509Store, 0) then
                Safe := False;
            VerifyInfo := FOcspHttp.OcspLastResp;
            FOcspHttp.OcspInters := Nil;
            doCopyEvent (LogLevelInfo, HttpCtl.SslServerName + ' ' + VerifyInfo)
        end;
    end
    else
    begin
        exit ;  // unknown method
    end ;

  // tell user verification failed
    if NOT Safe then
    begin
        info := 'SSL Chain Verification Failed: ' + VerifyInfo + ', Domain: ';
        if PeerCert.SubAltNameDNS = '' then
            info := info + IcsUnwrapNames (PeerCert.SubjectCName)
        else
            info := info + IcsUnwrapNames (PeerCert.SubAltNameDNS) ;    // Nov 2016
        info := info + ', Expected: ' + HttpCtl.SslServerName ;  // Nov 2016
        doCopyEvent (LogLevelInfo, info);
    end
    else
    begin
        doCopyEvent (LogLevelInfo, HttpCtl.SslServerName + ' SSL Chain Verification Succeeded') ;
    end;

// if certificate checking failed, see if the host is specifically listed as being allowed anyway
    if (NOT Safe) and (SslAcceptableHosts.IndexOf (HttpCtl.SslServerName) > -1) then  // 19 Oct 2015
    begin
        Safe := true ;
        doCopyEvent (LogLevelInfo, HttpCtl.SslServerName + ' SSL Succeeded with Acceptable Host Name') ;
    end ;

  // tell user about all the certificates we found
    if fSslReportChain and (CertChain.Count > 0) then
    begin
        info := HttpCtl.SslServerName + ' ' + IntToStr (CertChain.Count) +
                ' SSL Certificates in the verify chain:' + #13#10 +
                     CertChain.AllCertInfo (true, true) + #13#10 ; // Mar 2017 report all certs, backwards
        doCopyEvent (LogLevelInfo, info);
   end;

  // all failed
    if NOT Safe then
    begin
        Disconnect := TRUE;
        exit ;
    end;
end;

{$IFDEF Zipping}
procedure TIcsHttpMulti.ZipProgressEvent(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);   { V8.70 }
begin
{    if Assigned (fProgressEvent) then
    begin
        fCopyProg.LogLevel := LogLevelProg ;
        fCopyProg.Info := 'zipping...' ;                    // pending
        fProgressEvent (Self, fCopyProg, fCancelFlag) ;
    end;    }
end;
{$ENDIF}

procedure TIcsHttpMulti.OnHttpSslCliCertRequest(Sender: TObject; var Cert: TX509Base);
begin
    doCopyEvent (LogLevelDiag, 'Certificate Request Ignored') ;
end;

// HTTP download multiple local files
// returns false if error, with ReqResponse completed
{ Syntax of acceptable URL: protocol://[user[:password]@]server[:port]/path }

function TIcsHttpMulti.Download (CheckFiles: boolean): TIcsTaskResult ;
var
    fnametar, newtardir, dochref, myerror: string ;
    info, curURL, fnametemp: string ;
    hostname, rootfname: string ;
    I, J, K, L, donenr, statcode, attemptnr: integer ;
    newsize, totsize: int64 ;    // 10 Oct 2011
    initURLs, {attrs,} curLevel: integer ;
    {pageDT,} modDT: TDateTime ;
    starttick, fstarttick: Int64 ;      { V8.71 }
    duration: integer ;                 { V8.71 }
    NewList: TStringList ;
    SrcFileRec: PTIcsFDirRec ;
    DownloadStream: TStream ;
    CMask: TMask ;
    Proto, User, Pass, Host, Port, Dirs, Fname, Section, Query: String ;  // parsed URL
    HTMLParser: TIcsHTMLParser;
    HTMLObj: TObject ;
    HTMLTag: TIcsHTMLTag ;
    HTMLParam: TIcsHTMLParam ;
{$IFDEF Zipping}
    MyZipFile: TZipFile;         { V8.70 VclZip gone }
    MyZipName, MyDestDir, MyNewFullFile: String;   { V8.70 }
    MyRecreateDirs: Boolean;     { V8.70 }
    MyDateDT: TDateTime;         { V8.70 }
    MyDoneNr: Integer;
{$ENDIF}

    procedure sysDelayX (aMs: longword);
    var
        TickCount: Int64;      { V8.71 }
    begin
        TickCount := IcsGetTrgMSecs64(aMs);
        while NOT IcsTestTrgTick64 (TickCount) do
        begin
            ProcessMessages;
            if FTerminated then break ;
        end ;
    end;

begin
    fReqResponse := 'No Errors' ;
    fTotProcFiles := 0 ;
    fProcOKFiles := 0 ;
    fProcFailFiles := 0 ;
    fSkippedFiles := 0 ;
    fCancelFlag := false ;
    NewList := TStringList.Create ;
    Timeout := 300;  { V8.68 sync request timeout }
    ResponseNoException := True;  // June 2018
    CMask := TMask.Create (Trim (Lowercase (fSrcMask))) ;  // create match mask object
    result := TaskResOKNone ;
    fLastProgTick := Trigger64Immediate; // 10 Oct 2011 ensure progress displayed     { V8.71 }
    if fMaxAttempts = 0 then fMaxAttempts := 3 ;
    if fAttemptDelay = 0 then fAttemptDelay := 5 ;
{$IFDEF Zipping}
    MyZipFile := Nil;
    if fZipped then
    begin
        MyZipFile := TZipFile.Create;
        MyZipFile.OnProgress := ZipProgressEvent;
    end ;
{$ENDIF}
    try  // finally
        fDownDir := ExcludeTrailingBackslash (fDownDir) ;

    // build list of source files from HTTP URLs
        doCopyEvent (LogLevelInfo, 'Locating files to Download') ;
        NewList.SetText (Pchar (fUrlList)) ;
        if NewList.Count = 0 then
        begin
            result := TaskResFail ;
            fReqResponse := 'No HTTP URLs Specified' ;
            exit ;
        end ;

       // June 2018 do SslContext once, may be SSL relocate
        if not Assigned (fExternalSslSessionCache) then
        begin
            fExternalSslSessionCache := TSslAvlSessionCache.Create (self) ;
        end;
        FSslContext.SslOptions2 := FSslContext.SslOptions2 +
           [sslOpt2_NO_SESSION_RESUMPTION_ON_RENEGOTIATION, sslOpt2_NO_RENEGOTIATION]; // March 2018
        FSslContext.SslCliSecurity := fSslCliSecurity;  // June 2018
        if fSslSessCache then   // Dec 2016
        begin
            FSslContext.SslSessionCacheModes := [sslSESS_CACHE_CLIENT,
                sslSESS_CACHE_NO_INTERNAL_LOOKUP, sslSESS_CACHE_NO_INTERNAL_STORE] ;
        end;
        FSslContext.SslECDHMethod := sslECDHAuto ;  // 11 May 2015
        FSslContext.SslSessionCacheModes := [sslSESS_CACHE_CLIENT] ;  // Dec 2016

      // 20 Apr 2015 see if verifying server SSL certificate
        if (FHttpSslVerMethod > HttpSslVerNone) then
        begin
            FSslContext.SslVerifyPeer := true ;
            FSslContext.SslVerifyPeerModes := [SslVerifyMode_PEER] ;
            FSslContext.SslOcspStatus := true;     { V8.69 use OCSP stapling to get revoked status }
            rootfname := fSslRootFile ;    // Oct 2015, was fname and corrupted downloads
            if (Pos (':', rootfname) = 0) then rootfname := ExtractFileDir (ParamStr (0)) + '\' + rootfname ;
            if NOT FileExists (rootfname) then
            begin
                fSslContext.SslCALines.Text := sslRootCACertsBundle;  // June 2018 built-in
            end
            else
                fSslContext.SslCAFile := rootfname ;
        end ;
        try
            fSslContext.InitContext;
        except
            fReqResponse := 'Error Starting SSL - ' + IcsGetExceptMess (ExceptObject) ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
            result := TaskResFail ;
            exit ;
        end;

    // main loop parsing web pages and building list of files to download
        SetLength (SrcFiles, 1000) ;
        SrcFileList.Clear ;
        TotSrcFiles := 0  ;
        I := 0 ;
        initURLs := NewList.Count ;
        while (I < NewList.Count) do   // URLs may get added to this list during parsing
        begin
            ProcessMessages ;
            if fCancelFlag then exit ;
            curURL := trim (NewList [I]) ;
            inc (I) ;
            if curURL = '' then continue ;
            if curURL [1] = '*' then continue ;
            if curURL [1] = '#' then continue ;  // 11 Oct 2011
            UserName := '' ;
            Password := '' ;
            curLevel := 0;
            J := Pos('|', curURL);  { V8.66 see if processing sub-top level pages }
            if (J > 0) then
            begin
                curLevel := atoi(Copy(curURL, J + 1, 99));
                curURL := Copy(curURL, 1, J - 1);     // V8.66 strip level from URL
            end;

        // break down URL, see if rebuilding without logon
            ParseExURL (curURL, Proto, User, Pass, Host, Port, Dirs, Fname, Section, Query) ;
            if (Dirs = '') or (Section <> '') then
            begin
                curURL := BuildExURL (Proto, User, Pass, Host, Port, Dirs, Fname, '', Query) ;
            end ;
            ServerAuth := httpAuthNone;   // V8.66
            ParseExURL (curURL, Proto, User, Pass, Host, Port, Dirs, Fname, Section, Query) ;
            if User <> '' then
            begin
                UserName := User ;
                Password := Pass ;
                curURL := BuildExURL (Proto, '', '', Host, Port, Dirs, Fname, Section, Query) ;
                ServerAuth := httpAuthBasic;   // V8.66 avoid initial 401 response
            end ;
            hostname := Host ;   // get host name only less www and .coxxx
            if Pos ('.', hostname) > 1 then
            begin
                if Pos ('www.', hostname) = 1 then hostname := Copy (hostname, 5, 99) ;
                if Pos ('web.', hostname) = 1 then hostname := Copy (hostname, 5, 99) ;
                K := Pos ('.', hostname) ;
                if ((Length (hostname) - K) <= 6) then hostname := Copy (hostname, 1, Pred (K)) ;
            end ;
            if (Pos ('http', Proto) <> 1) then     // Sept 2001, allow blank files????
            begin
                if Length (curURL) <> 0 then doCopyEvent (LogLevelInfo, 'Invalid URL: ' + curURL) ;
                continue ;
            end ;
            doCopyEvent (LogLevelDiag, 'Get Headers for URL: ' + curURL) ;
            doCopyEvent (LogLevelProg, 'Get Headers for URL: ' + curURL) ;
            URL := curURL ;
            fProgMessBase := 'Downloading Headers' ;
            fProgFileSize := -1 ;  // don't show progress for headers only
            statcode := 999 ;
            RequestVer := '1.1';          // V8.66
            Connection := 'keep-alive';   // V8.66

         // V8.60  a few attempts, will use round robin DNS if more than one
            for attemptnr := 1 to fMaxAttempts do
            begin
                try
                    Head ;   // get sync header only
                    statcode := StatusCode ;
                    if statcode = 200 then break;
                    myerror := ReasonPhrase ;  // 19 Oct 2015
                except
                    myerror := IcsGetExceptMess (ExceptObject) ;    // 19 Oct 2015
                    statcode := StatusCode ;
                    if statcode = 200 then statcode := 999 ;
                end ;
                if statcode = 301 then        // V8.66 handle relocation
                begin
                    curURL := Location;
                    doCopyEvent (LogLevelDiag, 'Redirected to URL: ' + curURL) ;
                    URL := curURL ;
                    continue;
                end;
                if (attemptnr < fMaxAttempts) and (curLevel = 0) then   // only retry for top level
                begin
                    doCopyEvent (LogLevelInfo, 'Waiting for ' + IntToStr (fAttemptDelay) + ' secs, then Retrying') ;
                    sysDelayX (DWORD (fAttemptDelay) * 1000) ;
                end ;
            end;
            if statcode <> 200 then
            begin
                doCopyEvent (LogLevelInfo, 'Can Not Access URL: ' + curURL + ', ' +
                                                            myerror + ' (' + IntToStr (statcode) + ')') ;    // 19 Oct 2015
                doCopyEvent (LogLevelDelimFile, curURL + '|' + fnametar +
                         '|0|0|1|Can Not Access URL: ' + myerror + ' (' + IntToStr (statcode) + ')|0|0') ;    // 19 Oct 2015
                continue ;
            end ;
            if RcvdHeader.Count = 0 then continue ;
            modDT := RespLastModDT;   { V8.61 base component now has more response headers }
            if modDT = 0 then modDT := RespDateDT ;
            if modDT = 0 then modDT := Now ;

          // keeping this file, create file record
            info := LowerCase (ExtractUNIXName (Fname)) ;
            if fParseHTML and (fSrcMask <> '*.*') then   // allow blank names if all
            begin
                if NOT CMask.Matches (info) then info := '' ; // check against mask
            end ;
            if info <> '' then
            begin
                inc (TotSrcFiles) ;
                if Length (SrcFiles) <= TotSrcFiles then SetLength (SrcFiles, Length (SrcFiles) * 2) ;
                with SrcFiles [pred (TotSrcFiles)] do
                begin
                    FrFileName := Fname ;
                    FrSubDirs := '/' ;
                    if fKeepHost then FrSubDirs := FrSubDirs + hostname + '/' ;
                    if fKeepPath then FrSubDirs := FrSubDirs + Dirs ;
                    FrFullName := curURL ;
                    FrDirLevel := 0 ;
                    FrFileAttr := 0 ;
                    FrFileDT := modDT ;
                    FrFileUDT := modDT ;
                    FrFileBytes := ContentLength ;   { V8.68 might be -1 for chunked coding }
                    FrExtra := '' ;
                    FrFileCopy := FCStateNone ;
                    FrLinks := '' ;
                end ;
            end ;

        // see if need to get and parse an html file, DON'T PARSE FILES WE LINKED !!!!
            if fParseHTML and ((I <= initURLs) or (curLevel <= fParseLevels)) and    // V8.66 parse sub-zero level pages
                              (Pos ('text/html', ContentType) = 1) then  // 22 Feb 2008 allow for more stuff in content field
            begin
                doCopyEvent (LogLevelDiag, 'Get: ' + curURL) ;
                fProgMessBase := 'Downloading URL: ' + curURL ;
                fProgFileSize := ContentLength ;   // keep size for event handler  { V8.61 }
                fLastResponse := '' ;   // clear in case rubbish appears
                FreeAndNil(DownloadStream);   { V8.68 close stream }
                DownloadStream := TMemoryStream.Create ;    { V8.68 create memory stream }
                RcvdStream := DownloadStream ;
                onHttpDataEvent (Self, Nil, 0) ;
                myerror := 'None';  // V8.68
                ProcessMessages ;
                try
                    Get ;   // get sync header and body
                    statcode := StatusCode ;
                   {if statcode <> 200 then } myerror := ReasonPhrase ;   // 19 Oct 2015, V8.68 keep reason
                except
                    myerror := IcsGetExceptMess (ExceptObject) ;    // 19 Oct 2015
                    statcode := StatusCode ;
                    if statcode = 200 then statcode := 999 ;
                end ;
                if statcode <> 200 then
                begin
                    doCopyEvent (LogLevelInfo, 'Can Not Access URL: ' + curURL + ', ' + myerror +
                                                                               ' (' + IntToStr (statcode) + ')') ;   // 19 Oct 2015
                    doCopyEvent (LogLevelDelimFile, curURL + '|' + fnametar + '|0|0|1|Can Not Access URL: ' +
                                                        myerror + ' (' + IntToStr (statcode) + ')' + '|0|0') ;      // 19 Oct 2015
                    continue ;
                end ;
                DownloadStream.Position := 0; { V8.67 Seek (0, soFromBeginning) ; }
                if DownloadStream.Size = 0 then continue ;

            // parse file into tags
                doCopyEvent (LogLevelDiag, 'Parse Content for: ' + curURL + ', Level: ' + IntToStr(curLevel)) ;
                ProcessMessages ;
                HTMLParser := TIcsHTMLParser.Create;
                try
                    HTMLParser.Lines.LoadFromStream (DownloadStream) ;
                    try
                        HTMLParser.Execute;
                    except
                        doCopyEvent (LogLevelInfo, 'Failed to Parse URL: ' + curURL);  { V8.71 strange web pages }
                    end;
                    FreeAndNil(DownloadStream);   { V8.68 close stream }
                    if HTMLParser.Parsed.Count = 0 then continue ;
                    doCopyEvent (LogLevelDiag, 'Checking Content for: ' + curURL) ;
                    for J := 0 to Pred (HTMLParser.Parsed.Count) do
                    begin
                        HTMLObj := HTMLParser.Parsed [J] ;
                        if HTMLObj.ClassType = TIcsHTMLTag then
                        begin
                            HTMLTag := TIcsHTMLTag (HTMLObj) ;

                        // found an Anchor tag, look for link name
                            if HTMLTag.Name <> 'A' then continue ;
                            if HTMLTag.Params.Count = 0 then continue ;
                            for K := 0 to Pred (HTMLTag.Params.Count) do
                            begin
                                HTMLParam := HTMLTag.Params [K] ;
                                if HTMLParam.Key <> 'HREF' then continue ;
                                dochref := Trim (Lowercase (HTMLParam.Value)) ;
                                L := Pos ('#', dochref) ;   // 30 Oct 2003, strip off section and arguments
                                if L > 6 then dochref := Copy (dochref, 1, Pred (L)) ;
                                L := Pos ('?', dochref) ;
                                if L = 1 then Continue;          // V8.66 ignore complete query
                                if L > 6 then dochref := Copy (dochref, 1, Pred (L)) ;
                                if dochref = '' then continue ;
                                doCopyEvent (LogLevelDiag, 'Found Link: ' + dochref) ;
                                if Pos ('ftp://', dochref) > 0 then continue ;
                                if Pos ('mailto:', dochref) > 0 then continue ;

                            // check if link is file we want to download or another page we want to search
                                info := IcsLowerCase (ExtractUNIXName (dochref)) ;  // convert slashes to backslash
                                if ((info <> '') or (fSrcMask <> '*.*')) and       // allow blank names if all
                                                        (fParseLevels = 0) then    // V8.66 not if parsing sub levels
                                begin
                                    if (NOT CMask.Matches (info))  then continue ; // check download file extension mask
                                end ;
                                if Pos ('http', dochref) = 1 then begin
                                    if Pos(Host, dochref) = 0 then continue;  // V8.66 only links for our host
                                    info := HTMLParam.Value;
                                end
                                else
                                begin
                                    if (IcsPosEx('/..', dochref) > 0) then continue;       // V8.86 ignore parent path directories
                                    if (IcsPosEx('../', dochref) = 1) and
                                            (IcsPosEx('.', dochref, 4) = 0) then continue; // V8.86 ignore parent path directories
                                    if (IcsPosEx('.', info) > 1) then                      // V8.86 if we have a file extension, check it
                                    begin
                                        if (NOT CMask.Matches (info))  then      // V8.86 check download file extension mask
                                        begin
                                            if (curLevel >= fParseLevels) then continue;  // skip if not processing next level
                                            if (IcsPosEx('.htm', info) = 0) and
                                               (IcsPosEx('.asp', info) = 0) then continue // skip unless HTML pages for parsing
                                        end;
                                    end
                                    else
                                    begin
                                        if (curLevel >= fParseLevels) then continue;  // skip if not processing next level
                                    end;
                                    info := RelativeName (Dirs, HTMLParam.Value);
                                    info := BuildExURL (Proto, User, Pass, Host, Port, '', info, '', '') ;
                                end;

                           // save URL in list either to download or parse for more downloads
                                info := info + '|' + IntToStr(curLevel + 1);  // V8.66 page level
                                if NewList.IndexOf (info) = -1 then  // not if we have it already
                                begin
                                    doCopyEvent (LogLevelDiag, 'Saved URL: ' + info) ;  // TEMP !!!!
                                    NewList.Add (info) ;
                                end ;
                            end ;
                            ProcessMessages ;
                            if fCancelFlag then exit ;
                        end ;
                    end ;
                finally
                    HTMLParser.Free ;
                end ;
            end ;
        end ;
        FreeAndNil(DownloadStream);   { V8.68 close stream }
        SetLength (SrcFiles, TotSrcFiles) ;
        if TotSrcFiles = 0  then
        begin
            result := TaskResFail ;
            fReqResponse := 'No Source Files Found' ;
            exit ;
        end ;

    // build sorted list of source records, then remove duplicates
        SrcFileList.Capacity := TotSrcFiles ;
        for I := 0 to Pred (TotSrcFiles) do SrcFileList.Add (@SrcFiles [I]) ;
        SrcFileList.Sort (IcsCompareFNext) ;
        for I := 1 to Pred (SrcFileList.Count) do  // may get shorter!!
        begin
            if PTIcsFDirRec (SrcFileList [I]).FrFullName =
                                PTIcsFDirRec (SrcFileList [Pred (I)]).FrFullName then SrcFileList.Delete (I) ;
            if I >= Pred (SrcFileList.Count) then break ;
        end ;
        TotSrcFiles := SrcFileList.Count ;

    // show user the list we found
        if fLogRDir then doCopyEvent (LogLevelInfo, 'Source HTTP Files:' + IcsCRLF + IcsFmtFileDirList (SrcFileList, false)) ;
        ProcessMessages ;
        if fCancelFlag then exit ;

    // build list of target files, so we don't copy unnecessary stuff
        doCopyEvent (LogLevelFile, 'Target Directory: ' + fDownDir) ;
        if NOT IcsForceDirsEx (fDownDir) then
        begin
            result := TaskResFail ;
            fReqResponse := 'Can Not Create Target Directory' ;
            exit ;
        end ;
        fIcsFileCopy.GetDirList (fDownDir, '*.*', FCTypeAllDir, true, 0, 0, TarFiles, TarFileList) ;
        if fLogLDir then doCopyEvent (LogLevelInfo, 'Target Files on PC:' + IcsCRLF + IcsFmtFileDirList (TarFileList, false)) ;
        ProcessMessages ;
        if fCancelFlag then exit ;

    // compare source and target files, see what to copy
        fTotProcFiles := fIcsFileCopy.SelectCopyFileList (SrcFileList, TarFileList,
                                     '*.*' , FCTypeMaskDir, fRepl, -1, false, '', fSkippedFiles, false) ;
        if fTotProcFiles = 0  then
        begin
            result := TaskResOKNone ;
            if SkippedFiles <> 0 then
                fReqResponse := 'All Source Files Skipped Download'
            else
                fReqResponse := 'No Source Files Selected to Download' ;
            exit ;
        end ;

    // find size of stuff to copy
        info := IcsCRLF + 'HTTP URLs Selected for Downloading are: ' + IcsCRLF ;
        newsize := 0 ;
        for I := 0 to Pred (TotSrcFiles) do
        begin
            SrcFileRec := SrcFileList [I] ;
            with SrcFileRec^ do
            begin
                if FrFileCopy = FCStateSelect then
                begin
                    newsize := newsize + FrFileBytes ;
                    if CheckFiles then info := info + FrFullName + ', Size ' + IcsIntToCStr (FrFileBytes) + IcsCRLF ;
                end ;
            end ;
        end ;
        if CheckFiles then doCopyEvent (LogLevelInfo, info) ;
        doCopyEvent (LogLevelInfo, 'HTTP URLs Skipped ' + IntToStr (SkippedFiles)) ;
        doCopyEvent (LogLevelInfo, 'Selected Total Files ' + IntToStr (fTotProcFiles) +
                                                                 ', Total size ' + IntToKByte (newsize, true)) ;

    // stop now if only checking what will be downoaded
        if CheckFiles then
        begin
            result := TaskResOKNone ;
            exit ;
        end ;
        doCopyEvent (LogLevelInfo, 'Started HTTP Download') ;
        ProcessMessages ;
        if fCancelFlag then exit ;

    // start real HTTP downloading
        donenr := 0 ;
        totsize := 0 ;
        result := TaskResOKNone ;  // now want to get one file OK
        starttick := IcsGetTickCount64 ;       { V8.71 }
        for I := 0 to Pred (TotSrcFiles) do
        begin
            doCopyEvent (LogLevelProg, '') ;  // clear
            ProcessMessages ;
            if fCancelFlag then exit ;
            SrcFileRec := SrcFileList [I] ;
            with SrcFileRec^ do
            begin
                if FrFileCopy <> FCStateSelect then continue ;
                inc (donenr) ;
                curURL := FrFullName ;
                if Length (FrSubDirs) > 1 then
                begin
                    newtardir := IcsPathUnixToDos (fDownDir + FrSubDirs) ;
                    if NOT IcsForceDirsEx (newtardir) then
                    begin
                        doCopyEvent (LogLevelInfo, 'Can Not Create Directory: ' + newtardir) ;
                        inc (fProcFailFiles) ;
                        FrFileCopy := FCStateFailed ;
                        continue ;
                    end ;
                end
                else
                    newtardir := fDownDir ;
                fnametar := IncludeTrailingBackslash (newtardir) + FrFileName ;
                fProgMessBase := 'Downloading ' + IntToStr (donenr) + ' of ' + IntToStr (fTotProcFiles) ;
                doCopyEvent (LogLevelFile,  fProgMessBase + ' - ' + curURL +  ' to ' + fnametar +
                                                                         ', size ' + IntToKByte (FrFileBytes, true)) ;
                FrFileCopy := FCStateCopying ;
                fProgMessBase := fProgMessBase + IcsCRLF + 'URL: ' + curURL ;
                fProgFileSize := FrFileBytes ;   // keep size and data for event handler
                URL := curURL ;
                fLastResponse := '' ;   // clear in case rubbish appears

            { V8.68 create file stream for download (previously used memory only which limited size) }
                fnametemp := fnametar + '.part';
                DeleteFile (fnametemp) ;
                try
                    FreeAndNil(DownloadStream);
                    DownloadStream := TIcsBufferedFileStream.Create(fnametemp, fmCreate, MAX_BUFSIZE);
                except
                    doCopyEvent (LogLevelInfo, 'Failed to Open Download File: ' + fnametemp) ;
                    inc (fProcFailFiles) ;
                    FrFileCopy := FCStateFailed ;
                    continue ;
                end;

                RcvdStream := DownloadStream ;
                RequestVer := '1.1';          // V8.66
                Connection := 'keep-alive';   // V8.66
                onHttpDataEvent (Self, Nil, 0) ;
                myerror := 'None';  // V8.68
                fstarttick := IcsGetTickCount64 ;         { V8.71 }
                try
                    Get ;   // get sync header and body
                    statcode := StatusCode ;
                 {   if statcode <> 200 then } myerror := ReasonPhrase ;  // 19 Oct 2015, V8.68 keep reason
                except
                    myerror := IcsGetExceptMess (ExceptObject) ;          // 19 Oct 2015
                    statcode := StatusCode ;
                    if statcode = 200 then statcode := 999 ;
                end ;
                duration := IcsElapsedTicks64 (fstarttick) ;    { V8.71 }
                if (statcode = 200) then
                begin
                    inc (fProcOKFiles) ;
                    if duration = 0 then duration := 10 ;  // special case of copy OK but duration too short to measure
                    newsize := DownloadStream.Size ;
               //     newsize := DownloadStream.Position ;  { V8.68 size may be larger }
                    FreeAndNil(DownloadStream);   { V8.68 close file stream }
               //     DownloadStream.Position := 0; { V8.67 Seek (0, soFromBeginning) ; }
                    if (newsize = FrFileBytes) or (FrFileBytes <= 0) then   // V8.68 -1 for no size provided
                    begin
                        try
                   // replace old file, removing read only if necessary
                            IcsDeleteFile (fnametar, fReplRO) ;       { V8.70 simplified }
                            if (IcsRenameFile (fnametemp, fnametar, true, fReplRO) <> 0) then
                            begin
                               doCopyEvent (LogLevelInfo, 'Error Renaming File: ' + fnametemp + ' to ' + fnametar) ;
                                inc (fProcFailFiles) ;
                                FrFileCopy := FCStateFailed ;
                            end
                            else
                            begin
                          //     DownloadStream.SaveToFile (fnametar) ;  { V8.68 now filestream }
                                inc (totsize, newsize) ;
                                UpdateFileAge (fnametar, FrFileDT) ;  // not UTC date
                                FrFileCopy := FCStateOK ;
                                doCopyEvent (LogLevelFile, 'Download OK, size: ' + IntToKByte (newsize, true) +
                                  ', duration ' + IcsSecsToStr (duration div 1000) + ', average speed ' +
                                                                  IntToKByte (IcsCalcSpeed (duration, newsize)) + '/sec') ;
                                                                                 // 10 Oct 2011 added duration and speed
                                doCopyEvent (LogLevelDelimFile, curURL + '|' + fnametar + '|' + IntToStr (newsize) +
                                                             '|1|0|OK|' + IntToStr (duration) + '|' + IntToStr (newsize)) ;
                            end;
                        except
                            doCopyEvent (LogLevelInfo, 'Error Saving File: ' + fnametar + ', ' + IcsGetExceptMess (ExceptObject)) ;
                            inc (fProcFailFiles) ;
                            FrFileCopy := FCStateFailed ;
                        end ;

                  // see if unzipping it, V8.70 now using System.Zip instead of VclZip
{$IFDEF Zipping}
                        if fZipped and Assigned (MyZipFile) and (statcode = 200) and (AnsiLowercase (ExtractFileExt (fnametar)) = '.zip') then
                        begin
                            MyZipName := fnametar ;    // set the zip filename
                            try

                           // check it's not corrupted
                                if TZipFile.IsValid(MyZipName) then          { V8.70 VclZip gone }
                                begin
                                    MyZipFile.Open(MyZipName, zmRead) ;           // open it and read its information

                           // List filenames in zip file
                                    info := '' ;
                                    if MyZipFile.Comment <> '' then
                                        info := MyZipFile.Comment + IcsCRLF ;
                                    for J := 0 to Pred (MyZipFile.FileCount) do
                                    begin
                                        MyDateDT := 0 ;
                                        WinFileDateToDateTime(MyZipFile.FileInfo[J].ModifiedDateTime, MyDateDT);
                                        info := info + Format (sDirLine, [MyZipFile.Filename [J], IntToKByte (MyZipFile.FileInfo[J].UnCompressedSize, true), ' ',
                                             DateTimeToStr (MyDateDT), ' ']) + TZipCompressionToString(TZipCompression(MyZipFile.FileInfo[J].CompressionMethod)) + IcsCRLF ;
                                    end ;
                                    doCopyEvent (LogLevelInfo, 'Unzipping Files:' + IcsCRLF + info) ;

                                 // extract all files
                                    if (fZipDir = '') and (fZipPath >= PathSpecific) then
                                        fZipPath := PathNew ;
                                    MyDestDir := ExtractFileDir (fnametar) ;     // Set destination directory
                                    MyRecreateDirs := False ;
                                    if fZipPath in [PathOriginal, PathNewOrig, PathSpecOrig] then
                                        MyRecreateDirs := true ;
                                    if fZipPath in [PathNew, PathNewOrig] then
                                        MyDestDir := ExtractFileDir (fnametar) + '\' + IcsExtractNameOnly (fnametar) ;
                                    if fZipPath >= PathSpecific then
                                        MyDestDir := fZipDir ;
                                    if NOT IcsForceDirsEx (MyDestDir) then
                                    begin
                                        doCopyEvent (LogLevelFile, 'Failed to Create Unzip Dir: ' + MyDestDir) ;
                                        doCopyEvent (LogLevelDelimFile, fnametar + '|' + MyDestDir + '|0|0|1|Failed to Create Unzip Dir|0|0') ;
                                        continue ;
                                    end ;
                                    MyDoneNr := 0;
                                    for J := 0 to Pred (MyZipFile.FileCount) do
                                    begin
                                        try
                                            newsize := -1;
                                            MyNewFullFile := SlashesToBackSlashes(MyZipFile.Filename [J]) ;
                                            if NOT MyRecreateDirs then
                                                MyNewFullFile := ExtractFileName(MyNewFullFile);
                                            MyNewFullFile := IncludeTrailingBackslash (MyDestDir) + MyNewFullFile ;
                                            if MyNewFullFile[Length(MyNewFullFile)] = '\' then  // blank directory in zip, can not unzip it
                                            begin
                                                MyDoneNr := MyDoneNr + 1;
                                                if MyRecreateDirs then
                                                begin
                                                    if IcsForceDirsEx(MyNewFullFile) then
                                                        doCopyEvent (LogLevelFile, 'Extracted OK: ' + MyNewFullFile + ', Directory') ;
                                                end;
                                                continue;
                                            end;
                                            MyZipFile.Extract(J, MyDestDir, MyRecreateDirs);    // exception if it died
                                            newsize := IcsGetFileSize (MyNewFullFile) ;                                        except
                                            doCopyEvent (LogLevelFile, 'Zip Extract Failed ' + MyNewFullFile + ' - ' + IcsGetExceptMess (ExceptObject)) ;
                                        end ;
                                        if newsize >= 0 then
                                        begin
                                            MyDoneNr := MyDoneNr + 1;
                                            doCopyEvent (LogLevelFile, 'Extracted OK: ' + MyNewFullFile + ', size: ' + IntToKByte (newsize, true)) ;
                                            doCopyEvent (LogLevelDelimFile, 'Extracted|' + MyNewFullFile + '|' + IntToStr (newsize) + '|1|0|OK|0|0') ;
                                        end
                                        else
                                        begin
                                            doCopyEvent (LogLevelFile, 'Failed to Extract: ' + MyNewFullFile + ', size: ' + IntToKByte (newsize, true)) ;
                                            doCopyEvent (LogLevelDelimFile, 'Failed Extract|' + MyNewFullFile + '|' + IntToStr (newsize) + '|0|0|Failed to Unzip|0|0') ;
                                        end;
                                    end;
                                    if MyDoneNr = MyZipFile.FileCount then
                                    begin
                                        doCopyEvent (LogLevelFile, 'Unzipped OK: ' + fnametar) ;
                                        if fZipDownDel then
                                        begin
                                            doCopyEvent (LogLevelFile, 'Deleting: ' + fnametar) ;
                                            IcsDeleteFile (fnametar, true) ;
                                            doCopyEvent (LogLevelDelimFile, fnametar + '| |0|0|0|File Deleted After Unzipping|0|0') ;
                                        end ;
                                    end
                                    else
                                    begin
                                        doCopyEvent (LogLevelFile, 'Failed to Unzip: ' + fnametar) ;
                                        doCopyEvent (LogLevelDelimFile, fnametar + '|' + MyDestDir + '|0|0|1|Failed to Unzip File|0|0') ;
                                        continue ;
                                    end ;
                                end
                                else
                                begin
                                    doCopyEvent (LogLevelInfo, 'Zip File Corrupted:' + fnametar) ;
                                    doCopyEvent (LogLevelDelimFile, fnametar + '|' + MyDestDir + '|0|0|1|Zip File Corrupted|0|0') ;
                                    continue ;
                                end ;
                            except
                                doCopyEvent (LogLevelFile, 'Zip Decompression Failed ' + fnametar + ' - ' + IcsGetExceptMess (ExceptObject)) ;
                            end;
                            MyZipFile.Close;
                       end;
{$ENDIF}
                    end
                    else
                    begin
                        if (newsize = 0) or (NOT fKeepPartDown) then
                            DeleteFile (fnametemp) ;      { V8.68 kill partial file, unless told to leave it }
                        info := RequestDoneErrorStr + ' - ' + ReasonPhrase + ' (' + IntToStr (statcode) + ')' ;   { V8.68 added more error info }
                        if newsize >= 0 then
                            doCopyEvent (LogLevelFile, 'Request Failed: Partial File Downloaded, size: ' + IntToKByte (newsize, true))
                        else
                            doCopyEvent (LogLevelFile, 'Request Failed: No File Downloaded') ;
                        doCopyEvent (LogLevelDelimFile, curURL + '|' + fnametar + '|0|0|1|Download Failed: ' + info +
                                                                              '|' + IntToStr (duration) + '|' + IntToStr (newsize)) ;
                        inc (fProcFailFiles) ;
                        FrFileCopy := FCStateFailed ;
                    end ;
                end
                else
                begin
                    FreeAndNil(DownloadStream);   { V8.68 close file stream }
                    DeleteFile (fnametemp) ;      { V8.68 kill it }
                    info := RequestDoneErrorStr + ' - ' + ReasonPhrase + ' (' + IntToStr (statcode) + ')' ;   { V8.68 added more error info }
                    doCopyEvent (LogLevelInfo, 'Can Not Access URL: ' + curURL + ', Error: ' + info) ;
                    doCopyEvent (LogLevelDelimFile, curURL + '|' + fnametar + '|0|0|1|Download Failed: ' +  info +
                                                                               '|' + IntToStr (duration) + '|0') ;   // 19 Oct 2015
                    inc (fProcFailFiles) ;
                    FrFileCopy := FCStateFailed ;
                end ;
           end ;
        end ;
        FreeAndNil(DownloadStream);   { V8.68 close stream }
        result := TaskResOKNone ;
        if fProcFailFiles <> 0 then
            result := TaskResFail
        else
        begin
            if fProcOKFiles <> 0 then result := TaskResOKNew ;
        end ;
        duration := IcsElapsedTicks64 (starttick) ; // 10 Oct 2011   { V8.71 }
        doCopyEvent (LogLevelInfo, 'Finished, files downloaded OK: ' + IcsIntToCStr (fProcOKFiles) +
                            ', failed: ' + IntToStr (fProcFailFiles) + ', skipped: ' + IntToStr (fSkippedFiles)) ;
        doCopyEvent (LogLevelDelimTot, 'URLs|' + fDownDir + '|' + IntToStr (totsize) + '|' +
                                         IntToStr (fProcOKFiles) + '|' + IntToStr (fProcFailFiles) + '|Totals|') ;
        doCopyEvent (LogLevelInfo, 'Total size downloaded ' + IntToKByte (totsize, true) + ', duration ' +
                                     IcsSecsToStr (duration div 1000) + ', average speed ' +
                                                     IntToKByte (IcsCalcSpeed (duration, totsize)) + '/sec') ;
    finally
{$IFDEF Zipping}
      if fZipped and Assigned (MyZipFile) then
        MyZipFile.Free ;
{$ENDIF}
        if Assigned (fExternalSslSessionCache) then fExternalSslSessionCache.Flush;
        CMask.Free ;
        NewList.Free ;
        DownloadStream.Free ;
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled Downloading Files' ;
        end ;
   end ;
end;

procedure TIcsHttpMulti.Cancel ;
begin
    fCancelFlag := true ;
    fLastProgTick := Trigger64Immediate ; // force progress event     { V8.71 }
    Abort ;
end ;

{$ENDIF USE_SSL}
{$ENDIF MSWINDOWS}

end.
