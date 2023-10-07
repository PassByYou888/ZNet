{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsReg;
  {$DEFINE ICS_COMMON}
{$ENDIF}

{
Feb 15, 2012 Angus - added OverbyteIcsMimeUtils
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Jun 2012 - V8.00 - Angus added SysLog and SNMP components VCL only for now
Jul 2012   V8.02   Angus added TSslHttpAppSrv
Sep 2013   V8.03 - Angus added TSmtpSrv and TSslSmtpSrv
May 2017   V8.45 - Angus added TIcsProxy, TIcsHttpProxy
Apr 2018   V8.54 - Angus added TSslHttpRest, TSimpleWebSrv and TRestOAuth
May 2018   V8.54 - Angus added TSslX509Certs
Oct 2018   V8.58 - New components now installed for FMX and VCL
                   Added subversion to sIcsLongProductName for splash screen
Nov 2019   V8.59 - Version only
Mar 2019   V8.60 - Angus added TIcsMailQueue, TIcsIpStrmLog, TIcsWhoisCli,
                     TIcsTimeServer, TIcsTimeClient, TIcsBlacklist,
                     TIcsFileCopy, TIcsFtpMulti, TIcsHttpMulti.
                   For Delphi 2007 only, added TFtpClientW, TFtpServerW,
                     TIcsFileCopyW, TIcsFtpMultiW and TIcsHttpMultiW.
                   Added Forum and Wiki URLs to About Box.
Apr 2019  V8.61  - Added TDnsQueryHttps, TIcsSms
May 2019  V8.62  - Version only
Oct 2019  V8.63  - Version only
Nov 2019  V8.64  - Version only
Sep 2020  V8.65  - Added TIcsTwitter and TIcsRestEmail
Mar 2021  V8.66 -  Added TIcsInetAlive, OverbyteIcsSslThrdLock gone.
May 2021  V8.67 -  Version only
Oct 2021  V8.68 -  Version only
Mar 2022  V8.69 -  Added TOcspHttp, OverbyteIcsSslHttpOAuth.
Jun 2022  V8.70 -  Version only
Jul 2023  V8.71 -  Added TOAuthBrowser and TSslWebSocketCli
                   Added TIcsMonSocket and TIcsMonPcap
                   Added TIcsMQTTServer and TIcsMQTTClient
                   Added TIcsDomainNameCache and TIcsDomNameCacheHttps
                   Added TIcsNeighbDevices and TIcsIpChanges
Aug 08, 2023 V9.0  Updated version to major release 9.

}


{$I Include\Z.ICS9.OverbyteIcsDefs.inc}
{$IFDEF USE_SSL}
    {$I Include\Z.ICS9.OverbyteIcsSslDefs.inc}
{$ENDIF}
(*
{$IFDEF BCB}
  { So far no FMX support for C++ Builder, to be removed later }
  {$DEFINE VCL}
  {$IFDEF FMX}
    {$UNDEF FMX}
  {$ENDIF}
{$ENDIF}
*)
{$IFNDEF COMPILER16_UP}
  {$DEFINE VCL}
  {$IFDEF FMX}
    {$UNDEF FMX}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL}
  {$DEFINE VCL_OR_FMX}
{$ELSE}
  {$IFDEF FMX}
    {$DEFINE VCL_OR_FMX}
  {$ENDIF}
{$ENDIF}

interface

uses
  {$IFDEF FMX}
    FMX.Types,
    Z.ICS9.Ics.Fmx.OverbyteIcsWndControl,
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocket,
    Z.ICS9.Ics.Fmx.OverbyteIcsDnsQuery,
    Z.ICS9.Ics.Fmx.OverbyteIcsFtpCli,
    Z.ICS9.Ics.Fmx.OverbyteIcsFtpSrv,
    Z.ICS9.Ics.Fmx.OverbyteIcsMultipartFtpDownloader,
    Z.ICS9.Ics.Fmx.OverbyteIcsHttpProt,
    Z.ICS9.Ics.Fmx.OverbyteIcsHttpSrv,
    Z.ICS9.Ics.Fmx.OverbyteIcsMultipartHttpDownloader,
    Z.ICS9.Ics.Fmx.OverbyteIcsHttpAppServer,
    Z.ICS9.Ics.Fmx.OverbyteIcsCharsetComboBox,
    Z.ICS9.Ics.Fmx.OverbyteIcsPop3Prot,
    Z.ICS9.Ics.Fmx.OverbyteIcsSmtpProt,
    Z.ICS9.Ics.Fmx.OverbyteIcsNntpCli,
    Z.ICS9.Ics.Fmx.OverbyteIcsFingCli,
    Z.ICS9.Ics.Fmx.OverbyteIcsPing,
    {$IFDEF USE_SSL}
      Z.ICS9.Ics.Fmx.OverbyteIcsSslSessionCache,
      Z.ICS9.Ics.Fmx.OverbyteIcsProxy,
      Z.ICS9.Ics.Fmx.OverbyteIcsSslHttpRest,
      Z.ICS9.Ics.Fmx.OverbyteIcsSslX509Certs,
      Z.ICS9.Ics.Fmx.OverbyteIcsIpStreamLog,
      Z.ICS9.Ics.Fmx.OverbyteIcsMailQueue,
      Z.ICS9.Ics.Fmx.OverbyteIcsFtpMulti,
      Z.ICS9.Ics.Fmx.OverbyteIcsHttpMulti,
      Z.ICS9.Ics.Fmx.OverbyteIcsSslHttpOAuth,  { V8.69 }
      Z.ICS9.OverbyteIcsOAuthFormFmx,          { V8.71 }
      Z.ICS9.Ics.Fmx.OverbyteIcsWebSocketCli,  { V8.71 }
    {$ENDIF}
    Z.ICS9.Ics.Fmx.OverByteIcsWSocketE,
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocketS,
    Z.ICS9.Ics.Fmx.OverbyteIcsWhoisCli,
    Z.ICS9.Ics.Fmx.OverbyteIcsSntp,
    Z.ICS9.Ics.Fmx.OverbyteIcsBlacklist,
    Z.ICS9.Ics.Fmx.OverbyteIcsFileCopy,
  {$ENDIF FMX}
  {$IFDEF VCL}
    Controls,
    Z.ICS9.OverbyteIcsWndControl,
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsDnsQuery,
    Z.ICS9.OverbyteIcsFtpCli,
    Z.ICS9.OverbyteIcsFtpSrv,
    Z.ICS9.OverbyteIcsMultipartFtpDownloader,
    Z.ICS9.OverbyteIcsHttpProt,
    Z.ICS9.OverbyteIcsHttpSrv,
    Z.ICS9.OverbyteIcsMultipartHttpDownloader,
    Z.ICS9.OverbyteIcsHttpAppServer,
    Z.ICS9.OverbyteIcsCharsetComboBox,
    Z.ICS9.OverbyteIcsPop3Prot,
    Z.ICS9.OverbyteIcsSmtpProt,
    Z.ICS9.OverbyteIcsNntpCli,
    Z.ICS9.OverbyteIcsFingCli,
    Z.ICS9.OverbyteIcsPing,
    {$IFDEF USE_SSL}
      Z.ICS9.OverbyteIcsSslSessionCache,
      Z.ICS9.OverbyteIcsProxy,
      Z.ICS9.OverbyteIcsSslHttpRest,
      Z.ICS9.OverbyteIcsSslX509Certs,
      Z.ICS9.OverbyteIcsIpStreamLog,
      Z.ICS9.OverbyteIcsMailQueue,
      Z.ICS9.OverbyteIcsFtpMulti,
      Z.ICS9.OverbyteIcsHttpMulti,
      Z.ICS9.OverbyteIcsSslHttpOAuth,  { V8.69 }
      {$IFDEF COMPILER11_UP}
        Z.ICS9.OverbyteIcsOAuthFormVcl,  { V8.71 }
      {$ENDIF}
      Z.ICS9.OverbyteIcsWebSocketCli,  { V8.71 }
      Z.ICS9.OverbyteIcsMQTT,          { V8.71 }
    {$ENDIF}
    Z.ICS9.OverByteIcsWSocketE,
    Z.ICS9.OverbyteIcsWSocketS,
    Z.ICS9.OverbyteIcsSysLogClient,
    Z.ICS9.OverbyteIcsSysLogServer,
    Z.ICS9.OverbyteIcsSnmpCli,
    Z.ICS9.OverbyteIcsSmtpSrv,
    Z.ICS9.OverbyteIcsWhoisCli,
    Z.ICS9.OverbyteIcsSntp,
    Z.ICS9.OverbyteIcsBlacklist,
    Z.ICS9.OverbyteIcsFileCopy,
    Z.ICS9.OverbyteIcsMonCommon,     { V8.71 }
    Z.ICS9.OverbyteIcsMonSock,       { V8.71 }
    Z.ICS9.OverbyteIcsMonPcap,       { V8.71 }
    Z.ICS9.OverbyteIcsMonNdis,       { V8.71 }
    Z.ICS9.OverbyteIcsIpHlpApi,      { V8.71 }
   {$IFDEF DELPHI11}
      Z.ICS9.OverbyteIcsFtpCliW,
      Z.ICS9.OverbyteIcsFtpSrvW,
      Z.ICS9.OverbyteIcsFileCopyW,
      Z.ICS9.OverbyteIcsFtpMultiW,
      Z.ICS9.OverbyteIcsHttpMultiW,
   {$ENDIF}
    // VCL only
    Z.ICS9.OverbyteIcsMultiProgressBar,
    Z.ICS9.OverbyteIcsEmulVT, Z.ICS9.OverbyteIcsTnCnx, Z.ICS9.OverbyteIcsTnEmulVT, Z.ICS9.OverbyteIcsTnScript,
    {$IFNDEF BCB}
      Z.ICS9.OverbyteIcsWSocketTS,
    {$ENDIF}
  {$ENDIF VCL}
  {$IFDEF ICS_COMMON}
    Z.ICS9.OverbyteIcsMimeDec,
    Z.ICS9.OverbyteIcsMimeUtils,
    Z.ICS9.OverbyteIcsTimeList,
    Z.ICS9.OverbyteIcsLogger,
    {$IFNDEF BCB}
      Z.ICS9.OverbyteIcsCookies,
    {$ENDIF !BCB}
  {$ENDIF}
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF};

procedure Register;

implementation

uses
{$IFDEF MSWINDOWS}
  {$IFDEF COMPILER10_UP}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    ToolsApi,
  {$ENDIF}
  {$IFDEF COMPILER6_UP}
    DesignIntf, DesignEditors;
  {$ELSE}
    DsgnIntf;
  {$ENDIF}
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
{$IFDEF COMPILER16_UP}
{$IFDEF VCL_OR_FMX}
var
    LClassGroup: TPersistentClass;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF COMPILER16_UP}
  {$IFDEF VCL_OR_FMX}
    {$IFDEF FMX}
      LClassGroup := TFmxObject;
    {$ELSE}
      LClassGroup := TControl;
    {$ENDIF}
    GroupDescendentsWith(TIcsWndControl, LClassGroup);
    GroupDescendentsWith(TDnsQuery, LClassGroup);
    GroupDescendentsWith(TFingerCli, LClassGroup);
  {$ENDIF VCL_OR_FMX}
{$ENDIF COMPILER16_UP}

{$IFDEF VCL_OR_FMX}
    RegisterComponents('Overbyte ICS', [
      TWSocket, TWSocketServer,
      THttpCli, THttpServer, THttpAppSrv, TMultipartHttpDownloader,
      TFtpClient, TFtpServer, TMultipartFtpDownloader,
      TSmtpCli, TSyncSmtpCli, THtmlSmtpCli,
      TPop3Cli, TSyncPop3Cli,
      TNntpCli, THtmlNntpCli,
      TDnsQuery, TFingerCli, TPing,
      TIcsCharsetComboBox,
      {$IFDEF DELPHI11}
        TFtpClientW,    { V8.60 }
        TFtpServerW,    { V8.60 }
        TIcsFileCopyW,  { V8.60 }
      {$ENDIF}
      TIcsBlacklist,     { V8.60 }
      TIcsFileCopy,      { V8.60 }
      TIcsDomainNameCache  { V8.71 }
    ]);
{$ENDIF}
{$IFDEF VCL}
    RegisterComponents('Overbyte ICS', [
      { Not yet ported to FMX }
      TEmulVT, TTnCnx, TTnEmulVT, TTnScript,
      {$IFNDEF BCB}
        TWSocketThrdServer,
      {$ENDIF}
      TMultiProgressBar,
      TSysLogClient,
      TSysLogServer,
      TSnmpCli,
      TSmtpServer,
      TIcsWhoisCli,      { V8.60 }
      TIcsTimeServer,    { V8.60 }
      TIcsTimeClient,    { V8.60 }
      TIcsMonSocket,     { V8.71 }
      TIcsMonPcap,       { V8.71 }
      TIcsIpChanges,     { V8.71 }
      TIcsNeighbDevices  { V8.71 }
    ]);
{$ENDIF VCL}
{$IFDEF ICS_COMMON}
    RegisterComponents('Overbyte ICS', [
      { Components neither depending on the FMX nor on the VCL package }
      TMimeDecode,
      TMimeDecodeEx,
      TMimeDecodeW,
      TMimeTypesList,
   {$IFNDEF BCB}
      TIcsCookies,
   {$ENDIF !BCB}
      TTimeList, TIcsLogger
    ]);
{$ENDIF}

{$IFDEF USE_SSL}
  {$IFDEF COMPILER16_UP}
    {$IFDEF VCL_OR_FMX}
      GroupDescendentsWith(TSslBaseComponent, LClassGroup);
    {$ENDIF VCL_OR_FMX}
  {$ENDIF COMPILER16_UP}

  {$IFDEF VCL_OR_FMX}
    RegisterComponents('Overbyte ICS SSL', [
      TSslWSocket, TSslWSocketServer,
      TSslContext,
      TSslFtpClient, TSslFtpServer,
      TSslHttpCli, TSslHttpServer, TSslHttpAppSrv,
      TSslPop3Cli,
      TSslSmtpCli, TSslHtmlSmtpCli,
      TSslNntpCli,
      TSslAvlSessionCache,
      TIcsProxy,
      TIcsHttpProxy,
      TSslHttpRest,   { V8.54 }
      TSimpleWebSrv,  { V8.54 }
      TRestOAuth,     { V8.54 }
      TSslX509Certs,  { V8.54 }
      TIcsMailQueue,  { V8.60 }
      TIcsIpStrmLog,  { V8.60 }
      TIcsFtpMulti,   { V8.60 }
      TIcsHttpMulti,  { V8.60 }
      TDnsQueryHttps, { V8.61 }
      TIcsSms,        { V8.61 }
      TIcsTwitter,    { V8.65 }
      TIcsRestEmail,  { V8.65 }
      TOcspHttp,      { V8.69 }
      {$IFDEF COMPILER11_UP}
        TOAuthBrowser,       { V8.71 not Delphi 7 }
      {$ENDIF}
      TSslWebSocketCli,      { V8.71 }
      TIcsDomNameCacheHttps, { V8.71 }

      {$IFDEF DELPHI11}
        TSslFtpClientW,  { V8.60 }
        TSslFtpServerW,  { V8.60 }
        TIcsFtpMultiW,   { V8.60 }
        TIcsHttpMultiW,  { V8.60 }
      {$ENDIF}
    {$IFDEF VCL}
      {$IFNDEF BCB}
        TSslWSocketThrdServer,
      {$ENDIF}
        TSslSmtpServer,
        TIcsMQTTServer,      { V8.71 }
        TIcsMQTTClient,      { V8.71 }
    {$ENDIF VCL}
    {$IFNDEF OPENSSL_NO_ENGINE}
      TSslEngine,
    {$ENDIF}
      TIcsInetAlive   { V8.66 }
    ]);
  {$ENDIF VCL_OR_FMX}
{$ENDIF USE_SSL}

{$IFDEF VCL_OR_FMX}
    RegisterPropertyEditor(TypeInfo(AnsiString), TWSocket, 'LineEnd',
      TWSocketLineEndProperty);
{$ENDIF}

{$IFDEF COMPILER10_UP}
  {$IFNDEF COMPILER16_UP}
    {$IFDEF ICS_COMMON}
      ForceDemandLoadState(dlDisable); // Required to show our product icon on splash screen
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER10_UP}
{$IFDEF VCL}
{$R OverbyteIcsProductIcon.res}
const
{$IFDEF COMPILER14_UP}
    sIcsSplashImg       = 'ICSPRODUCTICONBLACK';
{$ELSE}
    {$IFDEF COMPILER10}
        sIcsSplashImg   = 'ICSPRODUCTICONBLACK';
    {$ELSE}
        sIcsSplashImg   = 'ICSPRODUCTICON';
    {$ENDIF}
{$ENDIF}
    sIcsLongProductName = 'Internet Component Suite V9.0';
    sIcsFreeware        = 'Freeware';
    sIcsDescription     = sIcsLongProductName + #13#10 +
                          //'Copyright (C) 1996-2023 by François PIETTE'+ #13#10 +
                          // Actually there's source included with different
                          // copyright, so either all or none should be mentioned
                          // here.
                          'https://www.overbyte.eu/' + #13#10 +
                          'Wiki: https://wiki.overbyte.eu/' + #13#10 +
                          'Support: https://en.delphipraxis.net/forum/37-ics-internet-component-suite/' + #13#10 +
                          'svn://svn.overbyte.be/ics/trunk' + #13#10 +
                          'https://svn.overbyte.be/svn/ics/trunk' + #13#10 +
                          'User and password = "ics"';

var
    AboutBoxServices: IOTAAboutBoxServices = nil;
    AboutBoxIndex: Integer = -1;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure PutIcsIconOnSplashScreen;
var
    hImage: HBITMAP;
begin
    if Assigned(SplashScreenServices) then begin
        hImage := LoadBitmap(FindResourceHInstance(HInstance), sIcsSplashImg);
        SplashScreenServices.AddPluginBitmap(sIcsLongProductName, hImage,
                                             FALSE, sIcsFreeware);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RegisterAboutBox;
begin
    if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then begin
        AboutBoxIndex := AboutBoxServices.AddPluginInfo(sIcsLongProductName,
          sIcsDescription, 0, FALSE, sIcsFreeware);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure UnregisterAboutBox;
begin
    if (AboutBoxIndex <> -1) and Assigned(AboutBoxServices) then begin
        AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
        AboutBoxIndex := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

initialization
    PutIcsIconOnSplashScreen;
    RegisterAboutBox;

finalization
    UnregisterAboutBox;
{$ENDIF VCL}
{$ENDIF COMPILER10_UP}
end.
