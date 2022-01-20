{$IFNDEF ICS_INCLUDE_MODE}
unit Z.OverbyteIcsReg;
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



}


{$I Include\Z.OverbyteIcsDefs.inc}
{$IFDEF USE_SSL}
    {$I Include\Z.OverbyteIcsSslDefs.inc}
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
    Z.Ics.Fmx.OverbyteIcsWndControl,
    Z.Ics.Fmx.OverbyteIcsWSocket,
    Z.Ics.Fmx.OverbyteIcsDnsQuery,
    Z.Ics.Fmx.OverbyteIcsFtpCli,
    Z.Ics.Fmx.OverbyteIcsFtpSrv,
    Z.Ics.Fmx.OverbyteIcsMultipartFtpDownloader,
    Z.Ics.Fmx.OverbyteIcsHttpProt,
    Z.Ics.Fmx.OverbyteIcsHttpSrv,
    Z.Ics.Fmx.OverbyteIcsMultipartHttpDownloader,
    Z.Ics.Fmx.OverbyteIcsHttpAppServer,
    Z.Ics.Fmx.OverbyteIcsCharsetComboBox,
    Z.Ics.Fmx.OverbyteIcsPop3Prot,
    Z.Ics.Fmx.OverbyteIcsSmtpProt,
    Z.Ics.Fmx.OverbyteIcsNntpCli,
    Z.Ics.Fmx.OverbyteIcsFingCli,
    Z.Ics.Fmx.OverbyteIcsPing,
    {$IFDEF USE_SSL}
      Z.Ics.Fmx.OverbyteIcsSslSessionCache,
      Z.Ics.Fmx.OverbyteIcsSslThrdLock,
      Z.Ics.Fmx.OverbyteIcsProxy,
      Z.Ics.Fmx.OverbyteIcsSslHttpRest,
      Z.Ics.Fmx.OverbyteIcsSslX509Certs,
    {$ENDIF}
    Z.Ics.Fmx.OverByteIcsWSocketE,
    Z.Ics.Fmx.OverbyteIcsWSocketS,
  {$ENDIF FMX}
  {$IFDEF VCL}
    Controls,
    Z.OverbyteIcsWndControl,
    Z.OverbyteIcsWSocket,
    Z.OverbyteIcsDnsQuery,
    Z.OverbyteIcsFtpCli,
    Z.OverbyteIcsFtpSrv,
    Z.OverbyteIcsMultipartFtpDownloader,
    Z.OverbyteIcsHttpProt,
    Z.OverbyteIcsHttpSrv,
    Z.OverbyteIcsMultipartHttpDownloader,
    Z.OverbyteIcsHttpAppServer,
    Z.OverbyteIcsCharsetComboBox,
    Z.OverbyteIcsPop3Prot,
    Z.OverbyteIcsSmtpProt,
    Z.OverbyteIcsNntpCli,
    Z.OverbyteIcsFingCli,
    Z.OverbyteIcsPing,
    {$IFDEF USE_SSL}
      Z.OverbyteIcsSslSessionCache,
      Z.OverbyteIcsSslThrdLock,
      Z.OverbyteIcsProxy,
      Z.OverbyteIcsSslHttpRest,
      Z.OverbyteIcsSslX509Certs,
    {$ENDIF}
    Z.OverByteIcsWSocketE,
    Z.OverbyteIcsWSocketS,
    Z.OverbyteIcsSysLogClient,
    Z.OverbyteIcsSysLogServer,
    Z.OverbyteIcsSnmpCli,
    Z.OverbyteIcsSmtpSrv,
    // VCL only
    Z.OverbyteIcsMultiProgressBar,
    Z.OverbyteIcsEmulVT, Z.OverbyteIcsTnCnx, Z.OverbyteIcsTnEmulVT, Z.OverbyteIcsTnScript,
    {$IFNDEF BCB}
      Z.OverbyteIcsWSocketTS,
    {$ENDIF}
  {$ENDIF VCL}
  {$IFDEF ICS_COMMON}
    Z.OverbyteIcsMimeDec,
    Z.OverbyteIcsMimeUtils,
    Z.OverbyteIcsTimeList,
    Z.OverbyteIcsLogger,
    {$IFNDEF BCB}
      Z.OverbyteIcsCookies,
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
      TIcsCharsetComboBox
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
      TSmtpServer
    ]);
{$ENDIF VCL}
{$IFDEF ICS_COMMON}
    RegisterComponents('Overbyte ICS', [
      { Components neither depending on the FMX nor on the VCL package }
      TMimeDecode, TMimeDecodeEx, TMimeDecodeW, TMimeTypesList,
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
      GroupDescendentsWith(TSslStaticLock, LClassGroup);
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
    {$IFDEF VCL}
      {$IFNDEF BCB}
        TSslWSocketThrdServer,
      {$ENDIF}
        TSslSmtpServer,
    {$ENDIF VCL}
    {$IFNDEF NO_DYNLOCK}
      TSslDynamicLock,
    {$ENDIF}
    {$IFNDEF OPENSSL_NO_ENGINE}
      TSslEngine,
    {$ENDIF}
      TSslStaticLock
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
    sIcsLongProductName = 'Internet Component Suite V8.58';
    sIcsFreeware        = 'Freeware';
    sIcsDescription     = sIcsLongProductName + #13#10 +
                          //'Copyright (C) 1996-2018 by Fran�ois PIETTE'+ #13#10 +
                          // Actually there's source included with different
                          // copyright, so either all or none should be mentioned
                          // here.
                          'http://www.overbyte.be/' + #13#10 +
                          'svn://svn.overbyte.be/ics/trunk' + #13#10 +
                          'http://svn.overbyte.be:8443/svn/ics/trunk' + #13#10 +
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
