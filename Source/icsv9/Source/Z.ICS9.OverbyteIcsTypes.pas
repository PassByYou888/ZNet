{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Common types and constants for ICS components, that should be added
              to most ICS applications to satisfy component event arguments.
Creation:     April 2004
Version:      V9.5
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2004-2025 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.

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

Note:

Consolidated constants and type for V9.3
----------------------------------------

This unit was originally just contained Delphi types added over the years and
not available in earlier versions of Delphi, like TBytes and UnicodeString.

From V9.3, a large number of types and constants used for component events
have been moved from other ICS units here, so those units don't generally
need to be added manually to ICS applications, just add OverbyteIcsTypes
instead.  For existing applications, most of the units listed below can
be removed from uses, but is not necessary.

Note, the following list is not exhaustive and x means many sub-options.

OverbyteIcsWinsock - IPPROTO_x, IPPORT_x, AF_x, SO_x, FD_x, WSAEx, IPV6_x
TInAddr, TSockAddrIn, TSockAddr, TSockProto, TInAddr6, TSockAddrIn6, TAddrInfoA,
TAddrInfoW, TIn6Addr, sockaddr

OverbyteIcsWSocket - TSocketFamily, TIcsIPv4Address, TSocketState, TSocketSendFlags,
TSocketErrs, THttpTunnelState, TSocksAuthState, TSslState, TSslMode, SocketStateNames,
SocketFamilyNames

OverbyteIcsLogger - TLogOption, TLogOptions, TLogOptionx

OverbyteIcsHttpProt - httperrx, THttpRequest, THttpState, THttpEncoding,
THttpAuthType, THttpCliOption, THttpCliOptions, TWWWAuthInfo, HttpCliAuthNames

OverbyteIcsSmtpProt - TSmtpState, TSmtpRequest, TSmtpDefaultEncoding, TSmtpSendMode,

OverbyteIcsFtpCli - TFtpCliSslType, TFtpOption, TFtpOptions, TFtpExtension,
TFtpExtensions. TFtpTransMode, TZStreamState, TFtpState, TFtpShareMode,
TFtpConnectionType

OverbyteIcsFileCopy - TIcsFileCopyType, TIcsFileCopyRepl, TIcsFileCopyState,
TIcsCopyLogLevel, TIcsTaskResult, TIcsSslCertCheck, IcsTaskResultNames,
IcsTaskResultStrings, IcsSslCertCheckStrings

OverbyteIcsSSLEAY - all global variables, GSSLxx, ICS_OPESSLx, OSSL_VER_x,
THttpDebugLevel, TCertVerMethod, TCertReadOpt, TChainResult, TSslSecLevel,
TSslSrvSecurity, TSslCliCertMethod, sslCliSecDefault, sslSrvSecDefault,
TSupplierProto, TChallengeType, TSslLoadSource, TEvpCipher, TEvpCipher,
TSslPrivKeyType, TSslPrivKeyCipher, SslPrivKeyEvpCipher

OverbyteIcsLIBEAY - X509_V_x, OCSP_x, V_OCSPx

OverbyteIcsSslX509Utils - DigestDispList,  DigestListLits, SslPrivKeyTypeLits,
SslPrivKeyCipherLits, SslCertFileOpenExts

OverbyteIcsWinCrypt (OverbyteIcsJwaWinCrypt include) - CERT_TRUST_x but renamed
 Ics_CERT_TRUST_x

OverbyteIcsUtils - IcsCRLF, etc, TicksPerx, IcsxBYTE



History:
Apr 10, 2009 Arno changed TBytes to an alias of SysUtils.TBytes in D2007 and
             better. Added alias EAbort.
Dec 03, 2009 Arno added some of the polymorphic integer types from
             Windows.pas/BaseTsd.h.
May 07, 2010 Arno added a few declarations.
Apr 15, 2011 Arno prepared for 64-bit.
May 06, 2011 Arno added TThreadID
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Feb 19, 2021 - V8.66 Added PUInt64.
Aug 08, 2023 V9.0  Updated version to major release 9.
Aug 30, 2024 V9.3  Moved many types and constants here for consolidation, see above.
                   Added GSSL_SignTest_Failed and GSSL_SignTest_FailError which are the
                     the results of testing the OpenSSL DLLs are digitally signed.
                   TCertVerMethod has new CertVerOwnEvent, added CertVerMethodNames.
Jan 28, 2025 V9.4  Changed u_long to LongWord from Longint since this is what Windows
                     uses, and IPv4 addresses are never negative, should prevent some
                     integer overflow errors.
                     This effects TInAddr, in_add, TSockAddrIn, sockaddr_in
                   Changed TIcsIPv4Address and in_addr6.u6_addr32 to LongWord from Integer.
                   Added EvpDigestLits for display.
                   Added some $HPPEMIT lines that add CPP header lines.
                   Added TFtpOption ftpNoExtV4 and TFtpExtension ftpFeatEpsv and ftpFeatEprt.
                   Added more date masks with short alphabetic months (Jan/Feb).
                   More external symbols for C++.
Sep 09, 2025 V9.5  Added TAcmeSupplier for specific Acme suppliers, SuppProtoCertCentre gone.
                   Added TSocketAddress and TCSAddrInfo for Windows APIs.
                   Added TIcsSessIpInfo set used for each TWSocket connection.
                   Added TTlsCertType for SslContext Raw Public Keys
                   Added GSSL_LOAD_ERRS which reports any OpenSSL loading errors.
                   Added WSABUF, TFlowSpec and TQualityOfService for Winsock2.
                   Added CF_xx responses from WSAAccept conditional filtering callback to
                     accept or reject connection.
                   Added in_addr.S_byte array.
                   Added TTcsIpBytes TBytes, IPv4 or IPv6 bytes, 4 or 16 long.
                   Added TIcsAppLogEvent application logging event to replace lots of
                     variants in different components.
                   Added TIcsGeoEvent, csCountryISOEvent and TIcsCountryNameEvent used
                     in components that need to lookup a geographic ISO code, ASN or country
                     name using the TIcsGeoTools component in an application.
Sep 24, 2025 V9.5  Corrected C++ symbols to compile with Win64, thanks to w0wbagger.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Z.ICS9.OverbyteIcsTypes;

interface

{$I Include\Z.ICS9.OverbyteIcsDefs.inc}

uses
{$IFDEF MSWINDOWS}
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF}
{$IFDEF POSIX}
//Posix.Pthread,
//Posix.SysTypes,
//Posix.Base,
  ,Posix.Errno,
  Posix.SysSocket,
  Posix.NetinetIn,
  Posix.NetinetIp6,
//  Posix.NetinetTCP,
  Posix.SysTime,
//  Posix.ArpaInet,
  Posix.NetDB,
//  Posix.UniStd,
//  Posix.Fcntl,
  Z.ICS9.Ics.Posix.WinTypes
{$ENDIF}
  {$IFDEF YuOpenSSL},YuOpenSSL{$ENDIF YuOpenSSL}
  ;

const
  OverbyteIcsTypesVersion = 905;
  CopyRight : String      = ' OverbyteIcsTypes (c) 2004-2025 F. Piette V9.5 ';


{$HPPEMIT '#include <mswsock.h>'}
{$HPPEMIT '#include <ws2ipdef.h>'}

type
{$IFNDEF COMPILER12_UP}
    UnicodeString = WideString;
    RawByteString = AnsiString;
{$ENDIF}
{$IFDEF COMPILER11_UP} // D2007 and better
  TBytes                    = {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.TBytes;
{$ELSE} // D7 - D2006
  TBytes                    = array of Byte;
{$ENDIF}

{$IFNDEF COMPILER15_UP}
  TThreadID                 = LongWord;
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$EXTERNALSYM size_t}
  {$IFDEF CPUX64}
    size_t                    = UInt64;
  {$ELSE}
    size_t                    = LongWord;
  {$ENDIF}
    Psize_t                   = ^size_t;
    PUInt64                   = ^UInt64;   { V8.66 }

  {$IFDEF COMPILER14_UP} // D2010 and better
      {$EXTERNALSYM HANDLE_PTR}
      HANDLE_PTR                = {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.HANDLE_PTR;
  {$ELSE} // D7 - D2009
      {$EXTERNALSYM HANDLE_PTR}
      HANDLE_PTR                = type LongWord;
  {$ENDIF}

  {$IFDEF COMPILER11_UP} // D2007 and better
      {$EXTERNALSYM INT_PTR}
      INT_PTR                   = {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.INT_PTR;
      {$EXTERNALSYM LONG_PTR}
      LONG_PTR                  = {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.LONG_PTR;
      {$EXTERNALSYM UINT_PTR}
      UINT_PTR                  = {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.UINT_PTR;
      {$EXTERNALSYM ULONG_PTR}
      ULONG_PTR                 = {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.ULONG_PTR;
      {$EXTERNALSYM DWORD_PTR}
      DWORD_PTR                 = {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.DWORD_PTR;
  {$ELSE} // D7 - D2006
      // From BaseTsd.h
      {$EXTERNALSYM INT_PTR}
      INT_PTR                   = Integer;
      {$EXTERNALSYM LONG_PTR}
      LONG_PTR                  = Longint;
      {$EXTERNALSYM UINT_PTR}
      UINT_PTR                  = Cardinal;
      {$EXTERNALSYM ULONG_PTR}
      ULONG_PTR                 = LongWord;
      {$EXTERNALSYM DWORD_PTR}
      DWORD_PTR                 = ULONG_PTR;
  {$ENDIF}
  {$EXTERNALSYM PINT_PTR}
  PINT_PTR                  = ^INT_PTR;
  {$EXTERNALSYM PUINT_PTR}
  PUINT_PTR                 = ^UINT_PTR;
{$ENDIF MSWINDOWS}


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ following moved from OverbyteIcsWinsock }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

type
    TSocketFamily = (sfAny, sfAnyIPv4, sfAnyIPv6, sfIPv4, sfIPv6);

var
  WSocketGCount   : Integer = 0;
  GReqVerLow      : BYTE    = 2;
  GReqVerHigh     : BYTE    = 2;
  GIPv6Available  : Integer = -1; { -1 = unchecked, 0 = FALSE, 1 = TRUE }
{$IFDEF MSWINDOWS}
{$IFDEF WIN64}
  GWSockCritSect: TRTLCriticalSection = ();                   { V9.5 }
{$ELSE}
  GWSockCritSect: TRTLCriticalSection;
{$ENDIF}
  {$EXTERNALSYM GWSockCritSect}                               { V9.4 }
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}

{$IFDEF WIN32}
  {$ALIGN 4}
{$ELSE}
  {$ALIGN ON}
{$ENDIF}
{$MINENUMSIZE 4}

{ Oct 21, 2016 V8.36 - Angus added new SO_xxx types }
{ Aug 08, 2023 V9.0  Updated version to major release 9. }

// the following emits are a workaround to the name conflict with
// procedure FD_SET and struct fd_set in winsock.h
(*$HPPEMIT 'namespace OverbyteIcsWinsock'*)
(*$HPPEMIT '{'*)
(*$HPPEMIT 'typedef fd_set *PFDSet;'*) // due to name conflict with procedure FD_SET
(*$HPPEMIT 'typedef fd_set TFDSet;'*)  // due to name conflict with procedure FD_SET
(*$HPPEMIT '}'*)

const
  WINSOCK_VERSION = $0202;
  {$EXTERNALSYM WINSOCK_VERSION}

type
  u_char = AnsiChar;
  {$EXTERNALSYM u_char}
  u_short = Word;
  {$EXTERNALSYM u_short}
  u_int = Integer;
  {$EXTERNALSYM u_int}
  u_long = LongWord;  { V9.4 changed from Longint since IPv4 addresses are never negative }
  {$EXTERNALSYM u_long}

{ The new type to be used in all
  instances which refer to sockets. }
{$IFDEF WIN32}
  TSocket = u_int;
{$ELSE}
  TSocket = IntPtr;
{$ENDIF}
  {$EXTERNALSYM TSocket}

const
  FD_SETSIZE     =   64;
  {$EXTERNALSYM FD_SETSIZE}

// WinSock 2 extension -- manifest constants for shutdown()
  SD_RECEIVE     = 0;
  {$EXTERNALSYM SD_RECEIVE}
  SD_SEND        = 1;
  {$EXTERNALSYM SD_SEND}
  SD_BOTH        = 2;
  {$EXTERNALSYM SD_BOTH}

type
  PFDSet = ^TFDSet;
  {$NODEFINE PFDSet}

  TFDSet = record
    fd_count: u_int;
    fd_array: array[0..FD_SETSIZE-1] of TSocket;
  end;
  {$NODEFINE TFDSet}

  PTimeVal = ^TTimeVal;
  timeval = record
    tv_sec: Longint;
    tv_usec: Longint;
  end;
  {$EXTERNALSYM timeval}
  TTimeVal = timeval;

const
  IOCPARM_MASK = $7f;
  {$EXTERNALSYM IOCPARM_MASK}
  IOC_VOID     = $20000000;
  {$EXTERNALSYM IOC_VOID}
  IOC_OUT      = $40000000;
  {$EXTERNALSYM IOC_OUT}
  IOC_IN       = $80000000;
  {$EXTERNALSYM IOC_IN}
  IOC_INOUT    = (IOC_IN or IOC_OUT);
  {$EXTERNALSYM IOC_INOUT}

  FIONREAD     = IOC_OUT or { get # bytes to read }
    ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 127;
  {$EXTERNALSYM FIONREAD}
  FIONBIO      = IOC_IN or { set/clear non-blocking i/o }
    ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 126;
  {$EXTERNALSYM FIONBIO}
  FIOASYNC     = IOC_IN or { set/clear async i/o }
    ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 125;
  {$EXTERNALSYM FIOASYNC}

type
  PHostEnt = ^THostEnt;
  hostent = record
    h_name: PAnsiChar;
    h_aliases: ^PAnsiChar;
    h_addrtype: Smallint;
    h_length: Smallint;
    case Byte of
      0: (h_addr_list: ^PAnsiChar);
      1: (h_addr: ^PAnsiChar)
  end;
  {$EXTERNALSYM hostent}
  THostEnt = hostent;

  PNetEnt = ^TNetEnt;
  netent = record
    n_name: PAnsiChar;
    n_aliases: ^PAnsiChar;
    n_addrtype: Smallint;
    n_net: u_long;
  end;
  {$EXTERNALSYM netent}
  TNetEnt = netent;

  PServEnt = ^TServEnt;
  servent = record
    s_name: PAnsiChar;
    s_aliases: ^PAnsiChar;
  {$IFDEF WIN64}
    s_proto: PAnsiChar;
    s_port: Word;
  {$ELSE}
    s_port: Word;
    s_proto: PAnsiChar;
  {$ENDIF}
  end;
  {$EXTERNALSYM servent}
  TServEnt = servent;

  PProtoEnt = ^TProtoEnt;
  protoent = record
    p_name: PAnsiChar;
    p_aliases: ^PAnsiChar;
    p_proto: Smallint;
  end;
  {$EXTERNALSYM protoent}
  TProtoEnt = protoent;

const

{ Protocols }

  IPPROTO_IP     =   0;             { dummy for IP }
  {$EXTERNALSYM IPPROTO_IP}
  IPPROTO_ICMP   =   1;             { control message protocol }
  {$EXTERNALSYM IPPROTO_ICMP}
  IPPROTO_IGMP   =   2;             { group management protocol }
  {$EXTERNALSYM IPPROTO_IGMP}
  IPPROTO_GGP    =   3;             { gateway^2 (deprecated) }
  {$EXTERNALSYM IPPROTO_GGP}
  IPPROTO_TCP    =   6;             { tcp }
  {$EXTERNALSYM IPPROTO_TCP}
  IPPROTO_PUP    =  12;             { pup }
  {$EXTERNALSYM IPPROTO_PUP}
  IPPROTO_UDP    =  17;             { user datagram protocol }
  {$EXTERNALSYM IPPROTO_UDP}
  IPPROTO_IDP    =  22;             { xns idp }
  {$EXTERNALSYM IPPROTO_IDP}
  IPPROTO_IPV6   =  41;
  {$EXTERNALSYM IPPROTO_IPV6}
  IPPROTO_ICMPV6 =  58;
  {$EXTERNALSYM IPPROTO_ICMPV6}
  IPPROTO_ND     =  77;             { UNOFFICIAL net disk proto }
  {$EXTERNALSYM IPPROTO_ND}

  IPPROTO_RAW    =  255;            { raw IP packet }
  {$EXTERNALSYM IPPROTO_RAW}
  IPPROTO_MAX    =  256;
  {$EXTERNALSYM IPPROTO_MAX}

{ Port/socket numbers: network standard functions}

  IPPORT_ECHO    =   7;
  {$EXTERNALSYM IPPORT_ECHO}
  IPPORT_DISCARD =   9;
  {$EXTERNALSYM IPPORT_DISCARD}
  IPPORT_SYSTAT  =   11;
  {$EXTERNALSYM IPPORT_SYSTAT}
  IPPORT_DAYTIME =   13;
  {$EXTERNALSYM IPPORT_DAYTIME}
  IPPORT_NETSTAT =   15;
  {$EXTERNALSYM IPPORT_NETSTAT}
  IPPORT_FTP     =   21;
  {$EXTERNALSYM IPPORT_FTP}
  IPPORT_TELNET  =   23;
  {$EXTERNALSYM IPPORT_TELNET}
  IPPORT_SMTP    =   25;
  {$EXTERNALSYM IPPORT_SMTP}
  IPPORT_TIMESERVER  =  37;
  {$EXTERNALSYM IPPORT_TIMESERVER}
  IPPORT_NAMESERVER  =  42;
  {$EXTERNALSYM IPPORT_NAMESERVER}
  IPPORT_WHOIS       =  43;
  {$EXTERNALSYM IPPORT_WHOIS}
  IPPORT_MTP         =  57;
  {$EXTERNALSYM IPPORT_MTP}

{ Port/socket numbers: host specific functions }

  IPPORT_TFTP        =  69;
  {$EXTERNALSYM IPPORT_TFTP}
  IPPORT_RJE         =  77;
  {$EXTERNALSYM IPPORT_RJE}
  IPPORT_FINGER      =  79;
  {$EXTERNALSYM IPPORT_FINGER}
  IPPORT_TTYLINK     =  87;
  {$EXTERNALSYM IPPORT_TTYLINK}
  IPPORT_SUPDUP      =  95;
  {$EXTERNALSYM IPPORT_SUPDUP}

{ UNIX TCP sockets }

  IPPORT_EXECSERVER  =  512;
  {$EXTERNALSYM IPPORT_EXECSERVER}
  IPPORT_LOGINSERVER =  513;
  {$EXTERNALSYM IPPORT_LOGINSERVER}
  IPPORT_CMDSERVER   =  514;
  {$EXTERNALSYM IPPORT_CMDSERVER}
  IPPORT_EFSSERVER   =  520;
  {$EXTERNALSYM IPPORT_EFSSERVER}

{ UNIX UDP sockets }

  IPPORT_BIFFUDP     =  512;
  {$EXTERNALSYM IPPORT_BIFFUDP}
  IPPORT_WHOSERVER   =  513;
  {$EXTERNALSYM IPPORT_WHOSERVER}
  IPPORT_ROUTESERVER =  520;
  {$EXTERNALSYM IPPORT_ROUTESERVER}

{ Ports < IPPORT_RESERVED are reserved for
  privileged processes (e.g. root). }

  IPPORT_RESERVED    =  1024;
  {$EXTERNALSYM IPPORT_RESERVED}

{ Link numbers }

  IMPLINK_IP         =  155;
  {$EXTERNALSYM IMPLINK_IP}
  IMPLINK_LOWEXPER   =  156;
  {$EXTERNALSYM IMPLINK_LOWEXPER}
  IMPLINK_HIGHEXPER  =  158;
  {$EXTERNALSYM IMPLINK_HIGHEXPER}

type
  SunB = record
    s_b1, s_b2, s_b3, s_b4: u_char;
  end;
  {$EXTERNALSYM SunB}

  SunW = record
    s_w1, s_w2: u_short;
  end;
  {$EXTERNALSYM SunW}

  PInAddr = ^TInAddr;
  in_addr = record
    case integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: u_long);
      3: (S_byte: array[0..3] of Byte);  { V9.5 }
  end;
  {$EXTERNALSYM in_addr}
  TInAddr = in_addr;

  PSockAddrIn = ^TSockAddrIn;
  sockaddr_in = record
    case Integer of
      0: (sin_family: u_short;
          sin_port: u_short;
          sin_addr: TInAddr;
          sin_zero: array[0..7] of AnsiChar);
      1: (sa_family: u_short;
          sa_data: array[0..13] of AnsiChar);    { V8.71 needs to be longer for IPv6 address }
  end;
  {$EXTERNALSYM sockaddr_in}
  TSockAddrIn = sockaddr_in;

const
  INADDR_ANY       = $00000000;
  {$EXTERNALSYM INADDR_ANY}
  INADDR_LOOPBACK  = $7F000001;
  {$EXTERNALSYM INADDR_LOOPBACK}
  INADDR_BROADCAST = DWORD($FFFFFFFF);
  {$EXTERNALSYM INADDR_BROADCAST}
  INADDR_NONE      = DWORD($FFFFFFFF);
  {$EXTERNALSYM INADDR_NONE}
  WSADESCRIPTION_LEN     =   256;
{$EXTERNALSYM WSADESCRIPTION_LEN}
  WSASYS_STATUS_LEN      =   128;
  {$EXTERNALSYM WSASYS_STATUS_LEN}

type
  PWSAData = ^TWSAData;
  WSAData = record // !!! also WSADATA
    wVersion: Word;
    wHighVersion: Word;
  {$IFDEF WIN64}
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PAnsiChar;
    szDescription: array[0..WSADESCRIPTION_LEN] of AnsiChar;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of AnsiChar;
  {$ELSE}
    szDescription: array[0..WSADESCRIPTION_LEN] of AnsiChar;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of AnsiChar;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PAnsiChar;
  {$ENDIF}
  end;
  {$EXTERNALSYM WSAData}
  TWSAData = WSAData;

{ V9.5 WinSock 2 extension -- WSABUF and QOS struct, include qos.h }
{ to pull in FLOWSPEC and related definitions }

  {$EXTERNALSYM WSABUF}
  WSABUF = record
    len: u_long;    { the length of the buffer }
    buf: PAnsiChar; { the pointer to the buffer }
  end;
  {$NODEFINE TWSABuf}
  TWSABuf = WSABUF;
  {$NODEFINE PWSABuf}
  PWSABuf = ^TWSABuf;
  {$EXTERNALSYM LPWSABUF}
  LPWSABUF = PWSABUF;

  {$EXTERNALSYM SERVICETYPE}
  SERVICETYPE = LongInt;
  {$NODEFINE TServiceType}
  TServiceType = SERVICETYPE;

  {$EXTERNALSYM FLOWSPEC}
  FLOWSPEC = record
    TokenRate,               // In Bytes/sec
    TokenBucketSize,         // In Bytes
    PeakBandwidth,           // In Bytes/sec
    Latency,                 // In microseconds
    DelayVariation : LongInt;// In microseconds
    ServiceType : TServiceType;
    MaxSduSize, MinimumPolicedSize : LongInt;// In Bytes
  end;
  {$NODEFINE TFlowSpec}
  TFlowSpec = FLOWSPEC;
  {$EXTERNALSYM PFLOWSPEC}
  PFLOWSPEC = ^TFlowSpec;
  {$EXTERNALSYM LPFLOWSPEC}
  LPFLOWSPEC = PFLOWSPEC;

  {$EXTERNALSYM QOS}
  QOS = record
    SendingFlowspec: TFlowSpec; { the flow spec for data sending }
    ReceivingFlowspec: TFlowSpec; { the flow spec for data receiving }
    ProviderSpecific: TWSABuf; { additional provider specific stuff }
  end;
  {$NODEFINE TQualityOfService}
  TQualityOfService = QOS;
  {$NODEFINE PQOS}
  PQOS = ^QOS;
  {$EXTERNALSYM LPQOS}
  LPQOS = PQOS;


  PTransmitFileBuffers = ^TTransmitFileBuffers;
  _TRANSMIT_FILE_BUFFERS = record
      Head: Pointer;
      HeadLength: DWORD;
      Tail: Pointer;
      TailLength: DWORD;
  end;
  {$EXTERNALSYM _TRANSMIT_FILE_BUFFERS}
  TRANSMIT_FILE_BUFFERS = _TRANSMIT_FILE_BUFFERS;
  {$EXTERNALSYM TRANSMIT_FILE_BUFFERS}
  TTransmitFileBuffers = _TRANSMIT_FILE_BUFFERS;

const
  TF_DISCONNECT           = $01;
  {$EXTERNALSYM TF_DISCONNECT}
  TF_REUSE_SOCKET         = $02;
  {$EXTERNALSYM TF_REUSE_SOCKET}
  TF_WRITE_BEHIND         = $04;
  {$EXTERNALSYM TF_WRITE_BEHIND}

  { Options for use with [gs]etsockopt at the IP level. }
  IP_OPTIONS              = 1;
  {$EXTERNALSYM IP_OPTIONS}

{/////////////////////////////////////////////////////////////////////////////}
  // New Version 2 //
{/////////////////////////////////////////////////////////////////////////////}

  IP_HDRINCL          = 2;
  {$EXTERNALSYM IP_HDRINCL}
  IP_TOS              = 3;           { set/get IP Type Of Service       }
  {$EXTERNALSYM IP_TOS}
  IP_TTL              = 4;           { set/get IP Time To Live          }
  {$EXTERNALSYM IP_TTL}
  IP_MULTICAST_IF     = 9;           { set/get IP multicast interface   }
  {$EXTERNALSYM IP_MULTICAST_IF}
  IP_MULTICAST_TTL    = 10;          { set/get IP multicast timetolive  }
  {$EXTERNALSYM IP_MULTICAST_TTL}
  IP_MULTICAST_LOOP   = 11;          { set/get IP multicast loopback    }
  {$EXTERNALSYM IP_MULTICAST_LOOP}
  IP_ADD_MEMBERSHIP   = 12;          { add  an IP group membership      }
  {$EXTERNALSYM IP_ADD_MEMBERSHIP}
  IP_DROP_MEMBERSHIP  = 13;          { drop an IP group membership      }
  {$EXTERNALSYM IP_DROP_MEMBERSHIP}
  IP_DONTFRAGMENT     = 14;          { set/get IP Don't Fragment flag   }
  {$EXTERNALSYM IP_DONTFRAGMENT}

  IP_DEFAULT_MULTICAST_TTL   = 1;    { normally limit m'casts to 1 hop  }
  {$EXTERNALSYM IP_DEFAULT_MULTICAST_TTL}
  IP_DEFAULT_MULTICAST_LOOP  = 1;    { normally hear sends if a member  }
  {$EXTERNALSYM IP_DEFAULT_MULTICAST_LOOP}
  IP_MAX_MEMBERSHIPS         = 20;   { per socket; must fit in one mbuf }
  {$EXTERNALSYM IP_MAX_MEMBERSHIPS}

type
  ip_mreq = record
    imr_multiaddr : in_addr;
    imr_interface : in_addr;
  end;
  {$EXTERNALSYM ip_mreq}
  TIpMReq = ip_mreq;
  PIpMReq = ^TIpMReq;

const

{ This is used instead of -1, since the
  TSocket type is unsigned.}

  INVALID_SOCKET    = TSocket(not(0));
  {$EXTERNALSYM INVALID_SOCKET}
  SOCKET_ERROR      = -1;
  {$EXTERNALSYM SOCKET_ERROR}

{ Types }

  SOCK_STREAM     = 1;               { stream socket }
  {$EXTERNALSYM SOCK_STREAM}
  SOCK_DGRAM      = 2;               { datagram socket }
  {$EXTERNALSYM SOCK_DGRAM}
  SOCK_RAW        = 3;               { raw-protocol interface }
  {$EXTERNALSYM SOCK_RAW}
  SOCK_RDM        = 4;               { reliably-delivered message }
  {$EXTERNALSYM SOCK_RDM}
  SOCK_SEQPACKET  = 5;               { sequenced packet stream }
  {$EXTERNALSYM SOCK_SEQPACKET}

{ Option flags per-socket. }

  SO_DEBUG        = $0001;          { turn on debugging info recording }
  {$EXTERNALSYM SO_DEBUG}
  SO_ACCEPTCONN   = $0002;          { socket has had listen() }
  {$EXTERNALSYM SO_ACCEPTCONN}
  SO_REUSEADDR    = $0004;          { allow local address reuse }
  {$EXTERNALSYM SO_REUSEADDR}
  SO_KEEPALIVE    = $0008;          { keep connections alive }
  {$EXTERNALSYM SO_KEEPALIVE}
  SO_DONTROUTE    = $0010;          { just use interface addresses }
  {$EXTERNALSYM SO_DONTROUTE}
  SO_BROADCAST    = $0020;          { permit sending of broadcast msgs }
  {$EXTERNALSYM SO_BROADCAST}
  SO_USELOOPBACK  = $0040;          { bypass hardware when possible }
  {$EXTERNALSYM SO_USELOOPBACK}
  SO_LINGER       = $0080;          { linger on close if data present }
  {$EXTERNALSYM SO_LINGER}
  SO_OOBINLINE    = $0100;          { leave received OOB data in line }
  {$EXTERNALSYM SO_OOBINLINE}

  SO_DONTLINGER  =   $ff7f;
  {$EXTERNALSYM SO_DONTLINGER}
  SO_EXCLUSIVEADDRUSE = NOT SO_REUSEADDR; { V8.36 disallow local address reuse }
  {$EXTERNALSYM SO_EXCLUSIVEADDRUSE}

{ Additional options. }

  SO_SNDBUF       = $1001;          { send buffer size }
  {$EXTERNALSYM SO_SNDBUF}
  SO_RCVBUF       = $1002;          { receive buffer size }
  {$EXTERNALSYM SO_RCVBUF}
  SO_SNDLOWAT     = $1003;          { send low-water mark }
  {$EXTERNALSYM SO_SNDLOWAT}
  SO_RCVLOWAT     = $1004;          { receive low-water mark }
  {$EXTERNALSYM SO_RCVLOWAT}
  SO_SNDTIMEO     = $1005;          { send timeout }
  {$EXTERNALSYM SO_SNDTIMEO}
  SO_RCVTIMEO     = $1006;          { receive timeout }
  {$EXTERNALSYM SO_RCVTIMEO}
  SO_ERROR        = $1007;          { get error status and clear }
  {$EXTERNALSYM SO_ERROR}
  SO_TYPE         = $1008;          { get socket type }
  {$EXTERNALSYM SO_TYPE}
  SO_BSP_STATE    = $1009;          { V8.36 get socket 5-tuple state }
  {$EXTERNALSYM SO_BSP_STATE}

{ V8.36 WinSock 2 extension -- new options }

  SO_GROUP_ID       = $2001; // ID of a socket group
  {$EXTERNALSYM SO_GROUP_ID}
  SO_GROUP_PRIORITY = $2002; // the relative priority within a group
  {$EXTERNALSYM SO_GROUP_PRIORITY}
  SO_MAX_MSG_SIZE   = $2003; // maximum message size
  {$EXTERNALSYM SO_MAX_MSG_SIZE}
  SO_PROTOCOL_INFOA = $2004; // WSAPROTOCOL_INFOA structure
  {$EXTERNALSYM SO_PROTOCOL_INFOA}
  SO_PROTOCOL_INFOW = $2005; // WSAPROTOCOL_INFOW structure
  {$EXTERNALSYM SO_PROTOCOL_INFOW}

  {$IFDEF UNICODE}
  SO_PROTOCOL_INFO = SO_PROTOCOL_INFOW;
  {$EXTERNALSYM SO_PROTOCOL_INFO}
  {$ELSE}
  SO_PROTOCOL_INFO = SO_PROTOCOL_INFOA;
  {$EXTERNALSYM SO_PROTOCOL_INFO}
  {$ENDIF UNICODE}

  PVD_CONFIG            = $3001; // configuration info for service provider
  {$EXTERNALSYM PVD_CONFIG}
  SO_CONDITIONAL_ACCEPT = $3002; // enable true conditional accept:
                                 //  connection is not ack-ed to the
                                 //  other side until conditional
                                 //  function returns CF_ACCEPT
  {$EXTERNALSYM SO_CONDITIONAL_ACCEPT}
  SO_PAUSE_ACCEPT     = $3003;   // pause accepting new connections
  {$EXTERNALSYM SO_PAUSE_ACCEPT}
  SO_COMPARTMENT_ID   = $3004;   // get/set the compartment for a socket
  {$EXTERNALSYM SO_COMPARTMENT_ID}
  SO_RANDOMIZE_PORT   = $3005;   // randomize assignment of wildcard ports
  {$EXTERNALSYM SO_RANDOMIZE_PORT}
  SO_PORT_SCALABILITY = $3006;   // enable port scalability
  {$EXTERNALSYM SO_PORT_SCALABILITY}

{ Options for connect and disconnect data and options.  Used only by
  non-TCP/IP transports such as DECNet, OSI TP4, etc. }

  SO_CONNDATA     = $7000;
  {$EXTERNALSYM SO_CONNDATA}
  SO_CONNOPT      = $7001;
  {$EXTERNALSYM SO_CONNOPT}
  SO_DISCDATA     = $7002;
  {$EXTERNALSYM SO_DISCDATA}
  SO_DISCOPT      = $7003;
  {$EXTERNALSYM SO_DISCOPT}
  SO_CONNDATALEN  = $7004;
  {$EXTERNALSYM SO_CONNDATALEN}
  SO_CONNOPTLEN   = $7005;
  {$EXTERNALSYM SO_CONNOPTLEN}
  SO_DISCDATALEN  = $7006;
  {$EXTERNALSYM SO_DISCDATALEN}
  SO_DISCOPTLEN   = $7007;
  {$EXTERNALSYM SO_DISCOPTLEN}

{ Option for opening sockets for synchronous access. }

  SO_OPENTYPE     = $7008;
  {$EXTERNALSYM SO_OPENTYPE}

  SO_SYNCHRONOUS_ALERT    = $10;
  {$EXTERNALSYM SO_SYNCHRONOUS_ALERT}

  SO_SYNCHRONOUS_NONALERT = $20;
  {$EXTERNALSYM SO_SYNCHRONOUS_NONALERT}

{ Other NT-specific options. }

  SO_MAXDG        = $7009;
  {$EXTERNALSYM SO_MAXDG}
  SO_MAXPATHDG    = $700A;
  {$EXTERNALSYM SO_MAXPATHDG}
  SO_UPDATE_ACCEPT_CONTEXT     = $700B;
  {$EXTERNALSYM SO_UPDATE_ACCEPT_CONTEXT}
  SO_CONNECT_TIME = $700C;
  {$EXTERNALSYM SO_CONNECT_TIME}

{ TCP options. }

  TCP_NODELAY     = $0001;
  {$EXTERNALSYM TCP_NODELAY}
  TCP_BSDURGENT   = $7000;
  {$EXTERNALSYM TCP_BSDURGENT}

{ Address families. }
(*
  AF_UNSPEC       = 0;               { unspecified }
  {$EXTERNALSYM AF_UNSPEC}
  AF_UNIX         = 1;               { local to host (pipes, portals) }
  {$EXTERNALSYM AF_UNIX}
  AF_INET         = 2;               { internetwork: UDP, TCP, etc. }
  {$EXTERNALSYM AF_INET}
  AF_IMPLINK      = 3;               { arpanet imp addresses }
  {$EXTERNALSYM AF_IMPLINK}
  AF_PUP          = 4;               { pup protocols: e.g. BSP }
  {$EXTERNALSYM AF_PUP}
  AF_CHAOS        = 5;               { mit CHAOS protocols }
  {$EXTERNALSYM AF_CHAOS}
  AF_NS           = 6;               { XEROX NS protocols }
  {$EXTERNALSYM AF_NS}
  AF_IPX          = AF_NS;           { IPX and SPX }
  {$EXTERNALSYM AF_IPX}
  AF_ISO          = 7;               { ISO protocols }
  {$EXTERNALSYM AF_ISO}
  AF_OSI          = AF_ISO;          { OSI is ISO }
  {$EXTERNALSYM AF_OSI}
  AF_ECMA         = 8;               { european computer manufacturers }
  {$EXTERNALSYM AF_ECMA}
  AF_DATAKIT      = 9;               { datakit protocols }
  {$EXTERNALSYM AF_DATAKIT}
  AF_CCITT        = 10;              { CCITT protocols, X.25 etc }
  {$EXTERNALSYM AF_CCITT}
  AF_SNA          = 11;              { IBM SNA }
  {$EXTERNALSYM AF_SNA}
  AF_DECnet       = 12;              { DECnet }
  {$EXTERNALSYM AF_DECnet}
  AF_DLI          = 13;              { Direct data link interface }
  {$EXTERNALSYM AF_DLI}
  AF_LAT          = 14;              { LAT }
  {$EXTERNALSYM AF_LAT}
  AF_HYLINK       = 15;              { NSC Hyperchannel }
  {$EXTERNALSYM AF_HYLINK}
  AF_APPLETALK    = 16;              { AppleTalk }
  {$EXTERNALSYM AF_APPLETALK}
  AF_NETBIOS      = 17;              { NetBios-style addresses }
  {$EXTERNALSYM AF_NETBIOS}
  AF_VOICEVIEW    = 18;              { VoiceView }
  {$EXTERNALSYM AF_VOICEVIEW}
  AF_FIREFOX      = 19;              { FireFox }
  {$EXTERNALSYM AF_FIREFOX}
  AF_UNKNOWN1     = 20;              { Somebody is using this! }
  {$EXTERNALSYM AF_UNKNOWN1}
  AF_BAN          = 21;              { Banyan }
  {$EXTERNALSYM AF_BAN}
  AF_MAX          = 22;
  {$EXTERNALSYM AF_MAX}

{.$IFNDEF NO_WINSOCK_2}
  AF_INET6        = 23;              { Internetwork Version 6 }
  {$EXTERNALSYM AF_INET6}
{.$ENDIF}
*)

type
  { Structure used by kernel to store most addresses. }

  PSOCKADDR = ^TSockAddr;
  {$EXTERNALSYM PSOCKADDR}
  TSockAddr = sockaddr_in;
  {$EXTERNALSYM TSockAddr}

  { Structure used by kernel to pass protocol information in raw sockets. }
  PSockProto = ^TSockProto;
  sockproto = record
    sp_family: u_short;
    sp_protocol: u_short;
  end;
  {$EXTERNALSYM sockproto}
  TSockProto = sockproto;

type
{ Structure used for manipulating linger option. }
  PLinger = ^TLinger;
  linger = record
    l_onoff: u_short;
    l_linger: u_short;
  end;
  {$EXTERNALSYM linger}
  TLinger = linger;

const
{ Level number for (get/set)sockopt() to apply to socket itself. }

  {$EXTERNALSYM SOL_SOCKET}
  SOL_SOCKET      = $ffff;          {options for socket level }

{ Maximum queue length specifiable by listen. }

  SOMAXCONN       = 5;
  {$EXTERNALSYM SOMAXCONN}

  MSG_OOB         = $1;             {process out-of-band data }
  {$EXTERNALSYM MSG_OOB}
  MSG_PEEK        = $2;             {peek at incoming message }
  {$EXTERNALSYM MSG_PEEK}
  MSG_DONTROUTE   = $4;             {send without using routing tables }
  {$EXTERNALSYM MSG_DONTROUTE}
  MSG_MAXIOVLEN   = 16;
  {$EXTERNALSYM MSG_MAXIOVLEN}
  MSG_PARTIAL     = $8000;          {partial send or recv for message xport }
  {$EXTERNALSYM MSG_PARTIAL}

{ Define constant based on rfc883, used by gethostbyxxxx() calls. }

  MAXGETHOSTSTRUCT        = 1024;
  {$EXTERNALSYM MAXGETHOSTSTRUCT}

{ Define flags to be used with the WSAAsyncSelect() call. }

{* * WinSock 2 extension -- bit values and indices for FD_XXX network events *}
  FD_READ_BIT             = 0;
  {$EXTERNALSYM FD_READ_BIT}
  FD_READ                 = 1 shl FD_READ_BIT;
  {$EXTERNALSYM FD_READ}

  FD_WRITE_BIT            = 1;
  {$EXTERNALSYM FD_WRITE_BIT}
  FD_WRITE                = 1 shl FD_WRITE_BIT;
  {$EXTERNALSYM FD_WRITE}

  FD_OOB_BIT              = 2;
  {$EXTERNALSYM FD_OOB_BIT}
  FD_OOB                  = 1 shl FD_OOB_BIT;
  {$EXTERNALSYM FD_OOB}

  FD_ACCEPT_BIT           = 3;
  {$EXTERNALSYM FD_ACCEPT_BIT}
  FD_ACCEPT               = 1 shl FD_ACCEPT_BIT;
  {$EXTERNALSYM FD_ACCEPT}

  FD_CONNECT_BIT          = 4;
  {$EXTERNALSYM FD_CONNECT_BIT}
  FD_CONNECT              = 1 shl FD_CONNECT_BIT;
  {$EXTERNALSYM FD_CONNECT}

  FD_CLOSE_BIT            = 5;
  {$EXTERNALSYM FD_CLOSE_BIT}
  FD_CLOSE                = 1 shl FD_CLOSE_BIT;
  {$EXTERNALSYM FD_CLOSE}

  FD_QOS_BIT              = 6;
  {$EXTERNALSYM FD_QOS_BIT}
  FD_QOS                  = 1 shl FD_QOS_BIT;
  {$EXTERNALSYM FD_QOS}

  FD_GROUP_QOS_BIT        = 7;
  {$EXTERNALSYM FD_GROUP_QOS_BIT}
  FD_GROUP_QOS            = 1 shl FD_GROUP_QOS_BIT;
  {$EXTERNALSYM FD_GROUP_QOS}

  FD_ROUTING_INTERFACE_CHANGE_BIT = 8;
  {$EXTERNALSYM FD_ROUTING_INTERFACE_CHANGE_BIT}
  FD_ROUTING_INTERFACE_CHANGE     = 1 shl FD_ROUTING_INTERFACE_CHANGE_BIT;
  {$EXTERNALSYM FD_ROUTING_INTERFACE_CHANGE}

  FD_ADDRESS_LIST_CHANGE_BIT      = 9;
  {$EXTERNALSYM FD_ADDRESS_LIST_CHANGE_BIT}
  FD_ADDRESS_LIST_CHANGE          = 1 shl FD_ADDRESS_LIST_CHANGE_BIT;
  {$EXTERNALSYM FD_ADDRESS_LIST_CHANGE}

  FD_MAX_EVENTS           = 10;
  {$EXTERNALSYM FD_MAX_EVENTS}
  FD_ALL_EVENTS           = ((1 shl FD_MAX_EVENTS) - 1);
  {$EXTERNALSYM FD_ALL_EVENTS}

{ All Windows Sockets error constants are biased by WSABASEERR from the "normal" }

  WSABASEERR              = 10000;
  {$EXTERNALSYM WSABASEERR}

{ Windows Sockets definitions of regular Microsoft C error constants }

  {$EXTERNALSYM WSAEINTR}
  WSAEINTR                = (WSABASEERR+4);
  {$EXTERNALSYM WSAEBADF}
  WSAEBADF                = (WSABASEERR+9);
  {$EXTERNALSYM WSAEACCES}
  WSAEACCES               = (WSABASEERR+13);
  {$EXTERNALSYM WSAEFAULT}
  WSAEFAULT               = (WSABASEERR+14);
  {$EXTERNALSYM WSAEINVAL}
  WSAEINVAL               = (WSABASEERR+22);
  {$EXTERNALSYM WSAEMFILE}
  WSAEMFILE               = (WSABASEERR+24);

{ Windows Sockets definitions of regular Berkeley error constants }

  WSAEWOULDBLOCK          = (WSABASEERR+35);
  {$EXTERNALSYM WSAEWOULDBLOCK}
  WSAEINPROGRESS          = (WSABASEERR+36);
  {$EXTERNALSYM WSAEINPROGRESS}
  WSAEALREADY             = (WSABASEERR+37);
  {$EXTERNALSYM WSAEALREADY}
  WSAENOTSOCK             = (WSABASEERR+38);
  {$EXTERNALSYM WSAENOTSOCK}
  WSAEDESTADDRREQ         = (WSABASEERR+39);
  {$EXTERNALSYM WSAEDESTADDRREQ}
  WSAEMSGSIZE             = (WSABASEERR+40);
  {$EXTERNALSYM WSAEMSGSIZE}
  WSAEPROTOTYPE           = (WSABASEERR+41);
  {$EXTERNALSYM WSAEPROTOTYPE}
  WSAENOPROTOOPT          = (WSABASEERR+42);
  {$EXTERNALSYM WSAENOPROTOOPT}
  WSAEPROTONOSUPPORT      = (WSABASEERR+43);
  {$EXTERNALSYM WSAEPROTONOSUPPORT}
  WSAESOCKTNOSUPPORT      = (WSABASEERR+44);
  {$EXTERNALSYM WSAESOCKTNOSUPPORT}
  WSAEOPNOTSUPP           = (WSABASEERR+45);
  {$EXTERNALSYM WSAEOPNOTSUPP}
  WSAEPFNOSUPPORT         = (WSABASEERR+46);
  {$EXTERNALSYM WSAEPFNOSUPPORT}
  WSAEAFNOSUPPORT         = (WSABASEERR+47);
  {$EXTERNALSYM WSAEAFNOSUPPORT}
  WSAEADDRINUSE           = (WSABASEERR+48);
  {$EXTERNALSYM WSAEADDRINUSE}
  WSAEADDRNOTAVAIL        = (WSABASEERR+49);
  {$EXTERNALSYM WSAEADDRNOTAVAIL}
  WSAENETDOWN             = (WSABASEERR+50);
  {$EXTERNALSYM WSAENETDOWN}
  WSAENETUNREACH          = (WSABASEERR+51);
  {$EXTERNALSYM WSAENETUNREACH}
  WSAENETRESET            = (WSABASEERR+52);
  {$EXTERNALSYM WSAENETRESET}
  WSAECONNABORTED         = (WSABASEERR+53);
  {$EXTERNALSYM WSAECONNABORTED}
  WSAECONNRESET           = (WSABASEERR+54);
  {$EXTERNALSYM WSAECONNRESET}
  WSAENOBUFS              = (WSABASEERR+55);
  {$EXTERNALSYM WSAENOBUFS}
  WSAEISCONN              = (WSABASEERR+56);
  {$EXTERNALSYM WSAEISCONN}
  WSAENOTCONN             = (WSABASEERR+57);
  {$EXTERNALSYM WSAENOTCONN}
  WSAESHUTDOWN            = (WSABASEERR+58);
  {$EXTERNALSYM WSAESHUTDOWN}
  WSAETOOMANYREFS         = (WSABASEERR+59);
  {$EXTERNALSYM WSAETOOMANYREFS}
  WSAETIMEDOUT            = (WSABASEERR+60);
  {$EXTERNALSYM WSAETIMEDOUT}
  WSAECONNREFUSED         = (WSABASEERR+61);
  {$EXTERNALSYM WSAECONNREFUSED}
  WSAELOOP                = (WSABASEERR+62);
  {$EXTERNALSYM WSAELOOP}
  WSAENAMETOOLONG         = (WSABASEERR+63);
  {$EXTERNALSYM WSAENAMETOOLONG}
  WSAEHOSTDOWN            = (WSABASEERR+64);
  {$EXTERNALSYM WSAEHOSTDOWN}
  WSAEHOSTUNREACH         = (WSABASEERR+65);
 {$EXTERNALSYM WSAEHOSTUNREACH}
  WSAENOTEMPTY            = (WSABASEERR+66);
  {$EXTERNALSYM WSAENOTEMPTY}
  WSAEPROCLIM             = (WSABASEERR+67);
  {$EXTERNALSYM WSAEPROCLIM}
  WSAEUSERS               = (WSABASEERR+68);
  {$EXTERNALSYM WSAEUSERS}
  WSAEDQUOT               = (WSABASEERR+69);
  {$EXTERNALSYM WSAEDQUOT}
  WSAESTALE               = (WSABASEERR+70);
  {$EXTERNALSYM WSAESTALE}
  WSAEREMOTE              = (WSABASEERR+71);
  {$EXTERNALSYM WSAEREMOTE}

{ Extended Windows Sockets error constant definitions }

  WSASYSNOTREADY          = (WSABASEERR+91);
  {$EXTERNALSYM WSASYSNOTREADY}
  WSAVERNOTSUPPORTED      = (WSABASEERR+92);
  {$EXTERNALSYM WSAVERNOTSUPPORTED}
  WSANOTINITIALISED       = (WSABASEERR+93);
  {$EXTERNALSYM WSANOTINITIALISED}
  WSAEDISCON              = (WSABASEERR+101);
  {$EXTERNALSYM WSAEDISCON}
  WSAENOMORE              = (WSABASEERR+102);
  {$EXTERNALSYM WSAENOMORE}
  WSAECANCELLED           = (WSABASEERR+103);
  {$EXTERNALSYM WSAECANCELLED}
  WSAEINVALIDPROCTABLE    = (WSABASEERR+104);
  {$EXTERNALSYM WSAEINVALIDPROCTABLE}
  WSAEINVALIDPROVIDER     = (WSABASEERR+105);
  {$EXTERNALSYM WSAEINVALIDPROVIDER}
  WSAEPROVIDERFAILEDINIT  = (WSABASEERR+106);
  {$EXTERNALSYM WSAEPROVIDERFAILEDINIT}
  WSASYSCALLFAILURE       = (WSABASEERR+107);
  {$EXTERNALSYM WSASYSCALLFAILURE}
  WSASERVICE_NOT_FOUND    = (WSABASEERR+108);
  {$EXTERNALSYM WSASERVICE_NOT_FOUND}
  WSATYPE_NOT_FOUND       = (WSABASEERR+109);
  {$EXTERNALSYM WSATYPE_NOT_FOUND}
  WSA_E_NO_MORE           = (WSABASEERR+110);
  {$EXTERNALSYM WSA_E_NO_MORE}
  WSA_E_CANCELLED         = (WSABASEERR+111);
  {$EXTERNALSYM WSA_E_CANCELLED}
  WSAEREFUSED             = (WSABASEERR+112);
  {$EXTERNALSYM WSAEREFUSED}

{ Error return codes from gethostbyname() and gethostbyaddr()
  (when using the resolver). Note that these errors are
  retrieved via WSAGetLastError() and must therefore follow
  the rules for avoiding clashes with error numbers from
  specific implementations or language run-time systems.
  For this reason the codes are based at WSABASEERR+1001.
  Note also that [WSA]NO_ADDRESS is defined only for
  compatibility purposes. }

{ Authoritative Answer: Host not found }

  WSAHOST_NOT_FOUND       = (WSABASEERR+1001);
  {$EXTERNALSYM WSAHOST_NOT_FOUND}
  HOST_NOT_FOUND          = WSAHOST_NOT_FOUND;
  {$EXTERNALSYM HOST_NOT_FOUND}

{ Non-Authoritative: Host not found, or SERVERFAIL }

  WSATRY_AGAIN            = (WSABASEERR+1002);
  {$EXTERNALSYM WSATRY_AGAIN}
  TRY_AGAIN               = WSATRY_AGAIN;
  {$EXTERNALSYM TRY_AGAIN}

{ Non recoverable errors, FORMERR, REFUSED, NOTIMP }

  WSANO_RECOVERY          = (WSABASEERR+1003);
  {$EXTERNALSYM WSANO_RECOVERY}
  NO_RECOVERY             = WSANO_RECOVERY;
  {$EXTERNALSYM NO_RECOVERY}

{ Valid name, no data record of requested type }

  WSANO_DATA              = (WSABASEERR+1004);
  {$EXTERNALSYM WSANO_DATA}
  NO_DATA                 = WSANO_DATA;
  {$EXTERNALSYM NO_DATA}

{ no address, look for MX record }

  WSANO_ADDRESS           = WSANO_DATA;
  {$EXTERNALSYM WSANO_ADDRESS}
  NO_ADDRESS              = WSANO_ADDRESS;
  {$EXTERNALSYM NO_ADDRESS}

{.$IFNDEF NO_WINSOCK_2}
{ Authoritative Answer: Host not found Securely }
  WSA_SECURE_HOST_NOT_FOUND  = (WSABASEERR+1032);
  {$EXTERNALSYM WSA_SECURE_HOST_NOT_FOUND}
{ Name based IPSEC policy could not be added }
  WSA_IPSEC_NAME_POLICY_ERROR = (WSABASEERR+1033);
  {$EXTERNALSYM WSA_IPSEC_NAME_POLICY_ERROR}
{.$ENDIF}

{ Windows Sockets errors redefined as regular Berkeley error constants.
  These are commented out in Windows NT to avoid conflicts with errno.h.
  Use the WSA constants instead. }

  EWOULDBLOCK        =  WSAEWOULDBLOCK;
  {$EXTERNALSYM EWOULDBLOCK}
  EINPROGRESS        =  WSAEINPROGRESS;
  {$EXTERNALSYM EINPROGRESS}
  EALREADY           =  WSAEALREADY;
  {$EXTERNALSYM EALREADY}
  ENOTSOCK           =  WSAENOTSOCK;
  {$EXTERNALSYM ENOTSOCK}
  EDESTADDRREQ       =  WSAEDESTADDRREQ;
  {$EXTERNALSYM EDESTADDRREQ}
  EMSGSIZE           =  WSAEMSGSIZE;
  {$EXTERNALSYM EMSGSIZE}
  EPROTOTYPE         =  WSAEPROTOTYPE;
  {$EXTERNALSYM EPROTOTYPE}
  ENOPROTOOPT        =  WSAENOPROTOOPT;
  {$EXTERNALSYM ENOPROTOOPT}
  EPROTONOSUPPORT    =  WSAEPROTONOSUPPORT;
  {$EXTERNALSYM EPROTONOSUPPORT}
  ESOCKTNOSUPPORT    =  WSAESOCKTNOSUPPORT;
  {$EXTERNALSYM ESOCKTNOSUPPORT}
  EOPNOTSUPP         =  WSAEOPNOTSUPP;
  {$EXTERNALSYM EOPNOTSUPP}
  EPFNOSUPPORT       =  WSAEPFNOSUPPORT;
  {$EXTERNALSYM EPFNOSUPPORT}
  EAFNOSUPPORT       =  WSAEAFNOSUPPORT;
  {$EXTERNALSYM EAFNOSUPPORT}
  EADDRINUSE         =  WSAEADDRINUSE;
  {$EXTERNALSYM EADDRINUSE}
  EADDRNOTAVAIL      =  WSAEADDRNOTAVAIL;
  {$EXTERNALSYM EADDRNOTAVAIL}
  ENETDOWN           =  WSAENETDOWN;
  {$EXTERNALSYM ENETDOWN}
  ENETUNREACH        =  WSAENETUNREACH;
  {$EXTERNALSYM ENETUNREACH}
  ENETRESET          =  WSAENETRESET;
  {$EXTERNALSYM ENETRESET}
  ECONNABORTED       =  WSAECONNABORTED;
  {$EXTERNALSYM ECONNABORTED}
  ECONNRESET         =  WSAECONNRESET;
  {$EXTERNALSYM ECONNRESET}
  ENOBUFS            =  WSAENOBUFS;
  {$EXTERNALSYM ENOBUFS}
  EISCONN            =  WSAEISCONN;
  {$EXTERNALSYM EISCONN}
  ENOTCONN           =  WSAENOTCONN;
  {$EXTERNALSYM ENOTCONN}
  ESHUTDOWN          =  WSAESHUTDOWN;
  {$EXTERNALSYM ESHUTDOWN}
  ETOOMANYREFS       =  WSAETOOMANYREFS;
  {$EXTERNALSYM ETOOMANYREFS}
  ETIMEDOUT          =  WSAETIMEDOUT;
  {$EXTERNALSYM ETIMEDOUT}
  ECONNREFUSED       =  WSAECONNREFUSED;
  {$EXTERNALSYM ECONNREFUSED}
  ELOOP              =  WSAELOOP;
  {$EXTERNALSYM ELOOP}
  ENAMETOOLONG       =  WSAENAMETOOLONG;
  {$EXTERNALSYM ENAMETOOLONG}
  EHOSTDOWN          =  WSAEHOSTDOWN;
  {$EXTERNALSYM EHOSTDOWN}
  EHOSTUNREACH       =  WSAEHOSTUNREACH;
 {$EXTERNALSYM EHOSTUNREACH}
  ENOTEMPTY          =  WSAENOTEMPTY;
  {$EXTERNALSYM ENOTEMPTY}
  EPROCLIM           =  WSAEPROCLIM;
  {$EXTERNALSYM EPROCLIM}
  EUSERS             =  WSAEUSERS;
  {$EXTERNALSYM EUSERS}
  EDQUOT             =  WSAEDQUOT;
  {$EXTERNALSYM EDQUOT}
  ESTALE             =  WSAESTALE;
  {$EXTERNALSYM ESTALE}
  EREMOTE            =  WSAEREMOTE;
  {$EXTERNALSYM EREMOTE}

{ WinSock 2 extension -- new error codes and type definition }

type
  WSAEVENT                      = THandle;
  {$EXTERNALSYM WSAEVENT}
  LPWSAEVENT                    = PHandle;
  {$EXTERNALSYM LPWSAEVENT}
  WSAOVERLAPPED                 = OVERLAPPED;
  {$EXTERNALSYM WSAOVERLAPPED}

const
  WSA_IO_PENDING                = (ERROR_IO_PENDING);
  {$EXTERNALSYM WSA_IO_PENDING}
  WSA_IO_INCOMPLETE             = (ERROR_IO_INCOMPLETE);
  {$EXTERNALSYM WSA_IO_INCOMPLETE}
  WSA_INVALID_HANDLE            = (ERROR_INVALID_HANDLE);
  {$EXTERNALSYM WSA_INVALID_HANDLE}
  WSA_INVALID_PARAMETER         = (ERROR_INVALID_PARAMETER);
  {$EXTERNALSYM WSA_INVALID_PARAMETER}
  WSA_NOT_ENOUGH_MEMORY         = (ERROR_NOT_ENOUGH_MEMORY);
  {$EXTERNALSYM WSA_NOT_ENOUGH_MEMORY}
  WSA_OPERATION_ABORTED         = (ERROR_OPERATION_ABORTED);
  {$EXTERNALSYM WSA_OPERATION_ABORTED}

  WSA_INVALID_EVENT             = WSAEVENT(nil);
  {$EXTERNALSYM WSA_INVALID_EVENT}
  WSA_MAXIMUM_WAIT_EVENTS       = (MAXIMUM_WAIT_OBJECTS);
  {$EXTERNALSYM WSA_MAXIMUM_WAIT_EVENTS}
  WSA_WAIT_FAILED               = (WAIT_FAILED);
  {$EXTERNALSYM WSA_WAIT_FAILED}
  WSA_WAIT_EVENT_0              = (WAIT_OBJECT_0);
  {$EXTERNALSYM WSA_WAIT_EVENT_0}
  WSA_WAIT_IO_COMPLETION        = (WAIT_IO_COMPLETION);
  {$EXTERNALSYM WSA_WAIT_IO_COMPLETION}
  WSA_WAIT_TIMEOUT              = (WAIT_TIMEOUT);
  {$EXTERNALSYM WSA_WAIT_TIMEOUT}
  WSA_INFINITE                  = (INFINITE);
  {$EXTERNALSYM WSA_INFINITE}

{ V9.5 response from WSAAccept conditional filtering callback to accept or reject connecton, renamed to avoid C conflicts }
  Ics_CF_ACCEPT = 0;
  Ics_CF_REJECT = 1;
  Ics_CF_DEFER  = 2;      // don't use, as it will call back with same parameters (most likely right away)   }

type
  PInAddr6 = ^in_addr6;
  in_addr6 = record
    case integer of
      0: (S6_addr: array [0..15] of Byte);
      1: (u6_addr8: array [0..15] of Byte);
      2: (u6_addr16: array [0..7] of Word);
      3: (u6_addr32: array [0..3] of LongWord);     { V9.4 was integer }
  end;
  {$EXTERNALSYM in_addr6}
  TInAddr6 = in_addr6;

  PSockAddrIn6 = ^sockaddr_in6;
  sockaddr_in6 = record
    sin6_family:   u_short;     // AF_INET6
    sin6_port:     u_short;     // Transport level port number
    sin6_flowinfo: u_long;      // IPv6 flow information
    sin6_addr:     TInAddr6;    // IPv6 address
{$IFDEF STILL_NEEDS_CHECK}
    case integer of
      0: (sin6_scope_id: u_long);      // Set of interfaces for a scope.
      1: (sin6_scope_struct: SCOPE_ID);
{$ELSE}
    sin6_scope_id: u_long;      // Scope Id: IF number for link-local SITE id for site-local
{$ENDIF}
  end;
  {$EXTERNALSYM sockaddr_in6}
  TSockAddrIn6 = sockaddr_in6;

  PIPV6_MREQ = ^ipv6_mreq;
  {$EXTERNALSYM PIPV6_MREQ}
  ipv6_mreq = record
    ipv6mr_multiaddr: TInAddr6; // IPv6 multicast address.
    ipv6mr_interface: u_long;   // Interface index.
  end;

  Socket_Address = record          { V9.5 used to store an IPv4 or IPv6 address }
    Sockaddr: PSockAddrIn6;
    SockaddrLength: Integer;      { 16 for IPv4, 28 for IPv6 }
  end;
  TSocketAddress = Socket_Address;

  CSADDR_INFO = record             { V9.5 used to connection IP information }
    LocalAddr: TSocketAddress;     { family, address and port  }
    RemoteAddr: TSocketAddress;
    iSocketType: Integer;          { SOCK_STREAM or SOCK_DGRAM }
    iProtocol: Integer;            { IPPROTO_TCP or IPPROTO_UDP }
    Buffer: array[0..63] of Byte;  { space for two PSockAddrIn6 records, max 56 }
 end;
 TCSAddrInfo = CSADDR_INFO;

 TIcsSessIpInfo = record           { V9.5 used for WSocket SessionIpInfo property }
    SocLocalAddr: TSockAddrIn6;    { family, address and port  }
    SocRemoteAddr: TSockAddrIn6;
    SocFamily: TSocketFamily;
    LocalAddr: String;             { string versions of Soc values }
    LocalPort: String;
    RemoteAddr: String;
    RemotePort: String;
    StartTick: Int64;              { when session started }
    SocType: Integer;              { SOCK_STREAM or SOCK_DGRAM }
    Proto: Integer;                { IPPROTO_TCP or IPPROTO_UDP }
    SocketType: String;
    Protocol: String;
    ISOA2: String;                 { two character country ISO code set from remote IP address and database for GEO blocking - FindISOA2Code }
    CountryName: String;           { looked up country name from ISOA2 }
    RegionName: String;            { lookup up sub-region name for country }
    RemoteRDNS: String;            { reverse DNS lookup for logs }
    AsnNum: Int64;                 { looked up ASN or ISP/Cloud number, et from remote IP address and database for GEO blocking - FindAsn }
    AsnName: String;               { ASN company name }
 end;

  {$EXTERNALSYM ipv6_mreq}
  TIPv6MReq = ipv6_mreq;
  PIPv6MReq = PIPV6_MREQ;

  PADDRINFOA = ^addrinfo;
  {$EXTERNALSYM PADDRINFOA}
  addrinfo = record
    ai_flags        : Integer;      // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    ai_family       : Integer;      // PF_xxx
    ai_socktype     : Integer;      // SOCK_xxx
    ai_protocol     : Integer;      // 0 or IPPROTO_xxx for IPv4 and IPv6
    ai_addrlen      : u_int;        // Length of ai_addr
    ai_canonname    : PAnsiChar;    // Canonical name for nodename
    ai_addr         : PSOCKADDR;    // Binary address
    ai_next         : PADDRINFOA;   // Next structure in linked list
  end;
  {$EXTERNALSYM addrinfo}
  ADDRINFOA = addrinfo;
  {$EXTERNALSYM ADDRINFOA}
  TAddrInfoA = ADDRINFOA;

  PADDRINFOW = ^addrinfoW;
  {$EXTERNALSYM PADDRINFOW}
  addrinfoW  = record
    ai_flags        : Integer;      // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    ai_family       : Integer;      // PF_xxx
    ai_socktype     : Integer;      // SOCK_xxx
    ai_protocol     : Integer;      // 0 or IPPROTO_xxx for IPv4 and IPv6
    ai_addrlen      : u_int;        // Length of ai_addr
    ai_canonname    : PWideChar;    // Canonical name for nodename
    ai_addr         : PSOCKADDR;    // Binary address
    ai_next         : PADDRINFOW;   // Next structure in linked list
  end;
  {$EXTERNALSYM addrinfoW}
  TAddrInfoW = addrinfoW;

{$IFDEF UNICODE}
  PAddrInfo = PADDRINFOW;
  TAddrInfo = TAddrInfoW;
{$ELSE}
  PAddrInfo = PADDRINFOA;
  TAddrInfo = TAddrInfoA;
{$ENDIF}


const
  AI_PASSIVE            = $1;   // Socket address will be used in bind() call
  {$EXTERNALSYM AI_PASSIVE}
  AI_CANONNAME          = $2;   // Return canonical name in first ai_canonname
  {$EXTERNALSYM AI_CANONNAME}
  AI_NUMERICHOST        = $4;   // Nodename must be a numeric address string
  {$EXTERNALSYM AI_NUMERICHOST}
  BlankSockAddr: TSockAddrIn6 = (sin6_family:  0);      { V9.5 }


  // Error codes from getaddrinfo().

  EAI_AGAIN             = WSATRY_AGAIN;
  {$EXTERNALSYM EAI_AGAIN}
  EAI_BADFLAGS          = WSAEINVAL;
  {$EXTERNALSYM EAI_BADFLAGS}
  EAI_FAIL              = WSANO_RECOVERY;
  {$EXTERNALSYM EAI_FAIL}
  EAI_FAMILY            = WSAEAFNOSUPPORT;
  {$EXTERNALSYM EAI_FAMILY}
  EAI_MEMORY            = WSA_NOT_ENOUGH_MEMORY;
  {$EXTERNALSYM EAI_MEMORY}
  EAI_NOSECURENAME      = WSA_SECURE_HOST_NOT_FOUND;
  {$EXTERNALSYM EAI_NOSECURENAME}
  EAI_NODATA            = WSANO_DATA;
  {$EXTERNALSYM EAI_NODATA}
  EAI_NONAME            = WSAHOST_NOT_FOUND;
  {$EXTERNALSYM EAI_NONAME}
  EAI_SERVICE           = WSATYPE_NOT_FOUND;
  {$EXTERNALSYM EAI_SERVICE}
  EAI_SOCKTYPE          = WSAESOCKTNOSUPPORT;
  {$EXTERNALSYM EAI_SOCKTYPE}
  EAI_IPSECPOLICY       = WSA_IPSEC_NAME_POLICY_ERROR;
  {$EXTERNALSYM EAI_IPSECPOLICY}

//
// Flags for getnameinfo()
//

  NI_NOFQDN               = $01;  // Only return nodename portion for local hosts
  {$EXTERNALSYM NI_NOFQDN}
  NI_NUMERICHOST          = $02;  // Return numeric form of the host's address
  {$EXTERNALSYM NI_NUMERICHOST}
  NI_NAMEREQD             = $04;  // Error if the host's name not in DNS
  {$EXTERNALSYM NI_NAMEREQD}
  NI_NUMERICSERV          = $08;  // Return numeric form of the service (port #)
  {$EXTERNALSYM NI_NUMERICSERV}
  NI_DGRAM                = $10;  // Service is a datagram service
  {$EXTERNALSYM NI_DGRAM}

  NI_MAXHOST              = 1025; // Max size of a fully-qualified domain name.
  {$EXTERNALSYM NI_MAXHOST}
  NI_MAXSERV              = 32;   // Max size of a service name.
  {$EXTERNALSYM NI_MAXSERV}

type
  sockaddr = record
     sa_family   : u_short;                    // address family
     sa_data     : array [0..13] of AnsiChar;  // up to 14 bytes of direct address   V8.71 needs to be longer for IPv6 address
  end;
  {$EXTERNALSYM sockaddr}

  IN6_ADDR = record
    case Integer of
        0: (Byte     : array [0..15] of u_char);
        1: (Word     : array [0..7]  of u_short);
        2: (s6_bytes : array [0..15] of Byte);
        3: (s6_addr  : array [0..15] of Byte);
        4: (s6_words : array [0..7]  of u_short);
  end;
  {$EXTERNALSYM IN6_ADDR}
  PIN6_ADDR  = ^IN6_ADDR;
  {$EXTERNALSYM PIN6_ADDR}
  TIn6Addr   = IN6_ADDR;
  {$EXTERNALSYM TIn6Addr}       { V9.4 }
  PIn6Addr   = ^TIn6Addr;
  {$EXTERNALSYM PIn6Addr}       { V9.4 }

var
{$IFDEF WIN64}
  in6addr_any: TIn6Addr = ();      { V9.5 }
{$ELSE}
  in6addr_any: TIn6Addr;
{$ENDIF}
  {$EXTERNALSYM in6addr_any}
  in6addr_loopback: TIn6Addr;
  {$EXTERNALSYM in6addr_loopback}
{$IFDEF STILL_NEEDS_CHECK}
  in6addr_v4mappedprefix: TIn6Addr;
  {$EXTERNALSYM in6addr_v4mappedprefix}
{$ENDIF}
const
//
// Options to use with [gs]etsockopt at the IPPROTO_IPV6 level.
// These are specified in RFCs 3493 and 3542.
// The values should be consistent with the IPv6 equivalents.
//
  IPV6_HOPOPTS                        = 1;  // Set/get IPv6 hop-by-hop options.
  {$EXTERNALSYM IPV6_HOPOPTS}
  IPV6_HDRINCL                        = 2;  // Header is included with data.
  {$EXTERNALSYM IPV6_HDRINCL}
  IPV6_UNICAST_HOPS                   = 4;  // IP unicast hop limit.
  {$EXTERNALSYM IPV6_UNICAST_HOPS}
  IPV6_MULTICAST_IF                   = 9;  // IP multicast interface.
  {$EXTERNALSYM IPV6_MULTICAST_IF}
  IPV6_MULTICAST_HOPS                 = 10;  // IP multicast hop limit.
  {$EXTERNALSYM IPV6_MULTICAST_HOPS}
  IPV6_MULTICAST_LOOP                 = 11;  // IP multicast loopback.
  {$EXTERNALSYM IPV6_MULTICAST_LOOP}
  IPV6_ADD_MEMBERSHIP                 = 12;  // Add an IP group membership.
  {$EXTERNALSYM IPV6_ADD_MEMBERSHIP}
  IPV6_JOIN_GROUP                     = IPV6_ADD_MEMBERSHIP;
  {$EXTERNALSYM IPV6_JOIN_GROUP}
  IPV6_DROP_MEMBERSHIP                = 13;  // Drop an IP group membership.
  {$EXTERNALSYM IPV6_DROP_MEMBERSHIP}
  IPV6_LEAVE_GROUP                    = IPV6_DROP_MEMBERSHIP;
  {$EXTERNALSYM IPV6_LEAVE_GROUP}
  IPV6_DONTFRAG                       = 14;  // Don't fragment IP datagrams.
  {$EXTERNALSYM IPV6_DONTFRAG}
  IPV6_PKTINFO                        = 19;  // Receive packet information.
  {$EXTERNALSYM IPV6_PKTINFO}
  IPV6_HOPLIMIT                       = 21;  // Receive packet hop limit.
  {$EXTERNALSYM IPV6_HOPLIMIT}
  IPV6_PROTECTION_LEVEL               = 23;  // Set/get IPv6 protection level.
  {$EXTERNALSYM IPV6_PROTECTION_LEVEL}
  IPV6_RECVIF                         = 24;  // Receive arrival interface.
  {$EXTERNALSYM IPV6_RECVIF}
  IPV6_RECVDSTADDR                    = 25;  // Receive destination address.
  {$EXTERNALSYM IPV6_RECVDSTADDR}
  IPV6_CHECKSUM                       = 26;  // Offset to checksum for raw IP socket send.
  {$EXTERNALSYM IPV6_CHECKSUM}
  IPV6_V6ONLY                         = 27;  // Treat wildcard bind as AF_INET6-only.
  {$EXTERNALSYM IPV6_V6ONLY}
  IPV6_IFLIST                         = 28;  // Enable/Disable an interface list.
  {$EXTERNALSYM IPV6_IFLIST}
  IPV6_ADD_IFLIST                     = 29;  // Add an interface list entry.
  {$EXTERNALSYM IPV6_ADD_IFLIST}
  IPV6_DEL_IFLIST                     = 30;  // Delete an interface list entry.
  {$EXTERNALSYM IPV6_DEL_IFLIST}
  IPV6_UNICAST_IF                     = 31;  // IP unicast interface.
  {$EXTERNALSYM IPV6_UNICAST_IF}
  IPV6_RTHDR                          = 32;  // Set/get IPv6 routing header.
  {$EXTERNALSYM IPV6_RTHDR}
  IPV6_RECVRTHDR                      = 38;  // Receive the routing header.
  {$EXTERNALSYM IPV6_RECVRTHDR}
  IPV6_TCLASS                         = 39;  // Packet traffic class.
  {$EXTERNALSYM IPV6_TCLASS}
  IPV6_RECVTCLASS                     = 40;  // Receive packet traffic class.
  {$EXTERNALSYM IPV6_RECVTCLASS}

  IP_UNSPECIFIED_HOP_LIMIT            = - 1;
  {$EXTERNALSYM IP_UNSPECIFIED_HOP_LIMIT}

{* * WinSock 2 extension -- manifest constants for WSAIoctl() *}

const
    IOC_UNIX             = $00000000;       { Do not use this in Windows     }
{$EXTERNALSYM IOC_UNIX}
    IOC_WS2              = $08000000;
{$EXTERNALSYM IOC_WS2}
    IOC_PROTOCOL         = $10000000;
{$EXTERNALSYM IOC_PROTOCOL}
    IOC_VENDOR           = $18000000;
{$EXTERNALSYM IOC_VENDOR}
    SIO_RCVALL           = IOC_IN or IOC_VENDOR or 1;
{$EXTERNALSYM SIO_RCVALL}
    SIO_RCVALL_MCAST     = IOC_IN or IOC_VENDOR or 2;
{$EXTERNALSYM SIO_RCVALL_MCAST}
    SIO_RCVALL_IGMPMCAST = IOC_IN or IOC_VENDOR or 3;
{$EXTERNALSYM SIO_RCVALL_IGMPMCAST}
    SIO_KEEPALIVE_VALS   = IOC_IN or IOC_VENDOR or 4;
{$EXTERNALSYM SIO_KEEPALIVE_VALS}
    SIO_ABSORB_RTRALERT  = IOC_IN or IOC_VENDOR or 5;
{$EXTERNALSYM SIO_ABSORB_RTRALERT}
    SIO_UCAST_IF         = IOC_IN or IOC_VENDOR or 6;
{$EXTERNALSYM SIO_UCAST_IF}
    SIO_LIMIT_BROADCASTS = IOC_IN or IOC_VENDOR or 7;
{$EXTERNALSYM SIO_LIMIT_BROADCASTS}
    SIO_INDEX_BIND       = IOC_IN or IOC_VENDOR or 8;
{$EXTERNALSYM SIO_INDEX_BIND}
    SIO_INDEX_MCASTIF    = IOC_IN or IOC_VENDOR or 9;
{$EXTERNALSYM SIO_INDEX_MCASTIF}
    SIO_INDEX_ADD_MCAST  = IOC_IN or IOC_VENDOR or 10;
{$EXTERNALSYM SIO_INDEX_ADD_MCAST}
    SIO_INDEX_DEL_MCAST  = IOC_IN or IOC_VENDOR or 11;
{$EXTERNALSYM SIO_INDEX_DEL_MCAST}
    SIO_GET_INTERFACE_LIST = IOC_OUT or ((SizeOf(u_long) and IOCPARM_MASK) shl 16) or (Ord('t') shl 8) or 127;
{$EXTERNALSYM SIO_GET_INTERFACE_LIST}

//#if (_WIN32_WINNT >= 0x0600)
{*
 * WSK-specific IO control codes are Winsock2 codes with the highest-order
 * 3 bits of the Vendor/AddressFamily-specific field set to 1.
 *}
  //{$EXTERNALSYM IOC_WSK}
  //IOC_WSK                             = IOC_WS2 or $07000000;
//#endif //(_WIN32_WINNT >= 0x0600)
{
#define _WSAIO(x,y)                   (IOC_VOID|(x)|(y))
#define _WSAIOR(x,y)                  (IOC_OUT|(x)|(y))
#define _WSAIOW(x,y)                  (IOC_IN|(x)|(y))
#define _WSAIORW(x,y)                 (IOC_INOUT|(x)|(y))
}
  SIO_ASSOCIATE_HANDLE                = IOC_IN or IOC_WS2 or 1;
  {$EXTERNALSYM SIO_ASSOCIATE_HANDLE}
  SIO_ENABLE_CIRCULAR_QUEUEING        = IOC_VOID or IOC_WS2 or 2;
  {$EXTERNALSYM SIO_ENABLE_CIRCULAR_QUEUEING}
  SIO_FIND_ROUTE                      = IOC_OUT or IOC_WS2 or 3;
  {$EXTERNALSYM SIO_FIND_ROUTE}
  SIO_FLUSH                           = IOC_VOID or IOC_WS2 or 4;
  {$EXTERNALSYM SIO_FLUSH}
  SIO_GET_BROADCAST_ADDRESS           = IOC_OUT or IOC_WS2 or 5;
  {$EXTERNALSYM SIO_GET_BROADCAST_ADDRESS}
  SIO_GET_EXTENSION_FUNCTION_POINTER  = IOC_INOUT or IOC_WS2 or 6;
  {$EXTERNALSYM SIO_GET_EXTENSION_FUNCTION_POINTER}
  SIO_GET_QOS                         = IOC_INOUT or IOC_WS2 or 7;
  {$EXTERNALSYM SIO_GET_QOS}
  SIO_GET_GROUP_QOS                   = IOC_INOUT or IOC_WS2 or 8;
  {$EXTERNALSYM SIO_GET_GROUP_QOS}
  SIO_MULTIPOINT_LOOPBACK             = IOC_IN or IOC_WS2 or 9;
  {$EXTERNALSYM SIO_MULTIPOINT_LOOPBACK}
  SIO_MULTICAST_SCOPE                 = IOC_IN or IOC_WS2 or 10;
  {$EXTERNALSYM SIO_MULTICAST_SCOPE}
  SIO_SET_QOS                         = IOC_IN or IOC_WS2 or 11;
  {$EXTERNALSYM SIO_SET_QOS}
  SIO_SET_GROUP_QOS                   = IOC_IN or IOC_WS2 or 12;
  {$EXTERNALSYM SIO_SET_GROUP_QOS}
  SIO_TRANSLATE_HANDLE                = IOC_INOUT or IOC_WS2 or 13;
  {$EXTERNALSYM SIO_TRANSLATE_HANDLE}
  SIO_ROUTING_INTERFACE_QUERY         = IOC_INOUT or IOC_WS2 or 20;
  {$EXTERNALSYM SIO_ROUTING_INTERFACE_QUERY}
  SIO_ROUTING_INTERFACE_CHANGE        = IOC_IN or IOC_WS2 or 21;
  {$EXTERNALSYM SIO_ROUTING_INTERFACE_CHANGE}
  SIO_ADDRESS_LIST_QUERY              = IOC_OUT or IOC_WS2 or 22;
  {$EXTERNALSYM SIO_ADDRESS_LIST_QUERY}
  SIO_ADDRESS_LIST_CHANGE             = IOC_VOID or IOC_WS2 or 23;
  {$EXTERNALSYM SIO_ADDRESS_LIST_CHANGE}
  SIO_QUERY_TARGET_PNP_HANDLE         = IOC_OUT or IOC_WS2 or 24;
  {$EXTERNALSYM SIO_QUERY_TARGET_PNP_HANDLE}

{$ENDIF MSWINDOWS}

{ V9.3 common Windows and Posix  }
const
{ Address families. }
  AF_UNSPEC       = 0;               { unspecified }
  {$EXTERNALSYM AF_UNSPEC}
  AF_UNIX         = 1;               { local to host (pipes, portals) }
  {$EXTERNALSYM AF_UNIX}
  AF_INET         = 2;               { internetwork: UDP, TCP, etc. - IPv4 }
  {$EXTERNALSYM AF_INET}
  AF_IMPLINK      = 3;               { arpanet imp addresses }
  {$EXTERNALSYM AF_IMPLINK}
  AF_PUP          = 4;               { pup protocols: e.g. BSP }
  {$EXTERNALSYM AF_PUP}
  AF_CHAOS        = 5;               { mit CHAOS protocols }
  {$EXTERNALSYM AF_CHAOS}
  AF_NS           = 6;               { XEROX NS protocols }
  {$EXTERNALSYM AF_NS}
  AF_IPX          = AF_NS;           { IPX and SPX }
  {$EXTERNALSYM AF_IPX}
  AF_ISO          = 7;               { ISO protocols }
  {$EXTERNALSYM AF_ISO}
  AF_OSI          = AF_ISO;          { OSI is ISO }
  {$EXTERNALSYM AF_OSI}
  AF_ECMA         = 8;               { european computer manufacturers }
  {$EXTERNALSYM AF_ECMA}
  AF_DATAKIT      = 9;               { datakit protocols }
  {$EXTERNALSYM AF_DATAKIT}
  AF_CCITT        = 10;              { CCITT protocols, X.25 etc }
  {$EXTERNALSYM AF_CCITT}
  AF_SNA          = 11;              { IBM SNA }
  {$EXTERNALSYM AF_SNA}
  AF_DECnet       = 12;              { DECnet }
  {$EXTERNALSYM AF_DECnet}
  AF_DLI          = 13;              { Direct data link interface }
  {$EXTERNALSYM AF_DLI}
  AF_LAT          = 14;              { LAT }
  {$EXTERNALSYM AF_LAT}
  AF_HYLINK       = 15;              { NSC Hyperchannel }
  {$EXTERNALSYM AF_HYLINK}
  AF_APPLETALK    = 16;              { AppleTalk }
  {$EXTERNALSYM AF_APPLETALK}
  AF_NETBIOS      = 17;              { NetBios-style addresses }
  {$EXTERNALSYM AF_NETBIOS}
  AF_VOICEVIEW    = 18;              { VoiceView }
  {$EXTERNALSYM AF_VOICEVIEW}
  AF_FIREFOX      = 19;              { FireFox }
  {$EXTERNALSYM AF_FIREFOX}
  AF_UNKNOWN1     = 20;              { Somebody is using this! }
  {$EXTERNALSYM AF_UNKNOWN1}
  AF_BAN          = 21;              { Banyan }
  {$EXTERNALSYM AF_BAN}
  AF_MAX          = 22;
  {$EXTERNALSYM AF_MAX}
  AF_INET6        = 23;              { Internetwork Version 6 - IPv6 }
  {$EXTERNALSYM AF_INET6}

const
{ Protocol families, same as address families for now. }

  PF_UNSPEC       = AF_UNSPEC;
  {$EXTERNALSYM PF_UNSPEC}
  PF_UNIX         = AF_UNIX;
  {$EXTERNALSYM PF_UNIX}
  PF_INET         = AF_INET;
  {$EXTERNALSYM PF_INET}
  PF_INET6         = AF_INET6;
  {$EXTERNALSYM PF_INET6}
  PF_IMPLINK      = AF_IMPLINK;
  {$EXTERNALSYM PF_IMPLINK}
  PF_PUP          = AF_PUP;
  {$EXTERNALSYM PF_PUP}
  PF_CHAOS        = AF_CHAOS;
  {$EXTERNALSYM PF_CHAOS}
  PF_NS           = AF_NS;
  {$EXTERNALSYM PF_NS}
  PF_IPX          = AF_IPX;
  {$EXTERNALSYM PF_IPX}
  PF_ISO          = AF_ISO;
  {$EXTERNALSYM PF_ISO}
  PF_OSI          = AF_OSI;
  {$EXTERNALSYM PF_OSI}
  PF_ECMA         = AF_ECMA;
  {$EXTERNALSYM PF_ECMA}
  PF_DATAKIT      = AF_DATAKIT;
  {$EXTERNALSYM PF_DATAKIT}
  PF_CCITT        = AF_CCITT;
  {$EXTERNALSYM PF_CCITT}
  PF_SNA          = AF_SNA;
  {$EXTERNALSYM PF_SNA}
  PF_DECnet       = AF_DECnet;
  {$EXTERNALSYM PF_DECnet}
  PF_DLI          = AF_DLI;
  {$EXTERNALSYM PF_DLI}
  PF_LAT          = AF_LAT;
  {$EXTERNALSYM PF_LAT}
  PF_HYLINK       = AF_HYLINK;
  {$EXTERNALSYM PF_HYLINK}
  PF_APPLETALK    = AF_APPLETALK;
  {$EXTERNALSYM PF_APPLETALK}
  PF_VOICEVIEW    = AF_VOICEVIEW;
  {$EXTERNALSYM PF_VOICEVIEW}
  PF_FIREFOX      = AF_FIREFOX;
  {$EXTERNALSYM PF_FIREFOX}
  PF_UNKNOWN1     = AF_UNKNOWN1;
  {$EXTERNALSYM PF_UNKNOWN1}
  PF_BAN          = AF_BAN;
  {$EXTERNALSYM PF_BAN}

  PF_MAX          = AF_MAX;
  {$EXTERNALSYM PF_MAX}

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ following moved from OverbyteIcsWsocket }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

const
    ICS_LOCAL_HOST_V4  = '127.0.0.1';
    ICS_LOCAL_HOST_V6  = '::1';
    ICS_LOCAL_HOST_NAME = 'localhost';    { V8.65 }
    ICS_ANY_HOST_V4    = '0.0.0.0';
    ICS_ANY_HOST_V6    = '::';
    ICS_BROADCAST_V4   = '255.255.255.255';
    ICS_BROADCAST_V6   = 'ffff::1';
    ICS_ANY_PORT       = '0';
    DefaultSocketFamily = sfIPv4;
    sHttpVersionError = 'Proxy server must support HTTP/1.1';


{$IFDEF POSIX}

  {.$DEFINE NO_ADV_MT}
const
  {$IFDEF LINUX} // ?
    FIONREAD                    = $541B;
    FIONBIO                     = $5421;
  {$ENDIF}
  {$IFDEF MACOS}
    FIONREAD                    = $4004667F;
    FIONBIO                     = $8004667E;
  {$ENDIF}
    INVALID_SOCKET              = -1;
    INVALID_FILE_HANDLE         = -1;
    SOCKET_ERROR                = -1;
    INADDR_NONE                 = $FFFFFFFF;
    //MSG_NOSIGNAL                = 0;

    FD_READ                     = $0001;
    FD_WRITE                    = $0002;
   // FD_OOB                      = $0004; Not implemented
    FD_ACCEPT                   = $0008;
    FD_CONNECT                  = $0010;
    FD_CLOSE                    = $0020;
  // winsock 2 not working
   { FD_QOS                      = $0040;
    FD_GROUP_QOS                = $0080; }
    FD_ROUTING_INTERFACE_CHANGE = $100;
    FD_ADDRESS_LIST_CHANGE      = $200;
    FD_MAX_EVENTS               = 10;

    { Socket error codes mapped }
    WSABASEERR                  = 0;//10000;
    WSAHOST_NOT_FOUND           = EPERM;                   // EAI_NONAME
    WSATRY_AGAIN                = ENOENT;                  // EAI_AGAIN
    WSANO_RECOVERY              = ESRCH;                   // EAI_FAIL
    WSAEINTR                    = EINTR;
    WSASERVICE_NOT_FOUND        = ENOEXEC;                 // EAI_SERVICE
    WSAEBADF                    = EBADF;
    WSAEACCES                   = EACCES;
    WSAEFAULT                   = EFAULT;
    WSAEINVAL                   = EINVAL;                  // EAI_BADFLAGS
    WSAEMFILE                   = EMFILE;
    WSAEWOULDBLOCK              = EWOULDBLOCK;
    WSAEINPROGRESS              = EINPROGRESS;
    WSAEALREADY                 = EALREADY;
    WSAENOTSOCK                 = ENOTSOCK;
    WSAEDESTADDRREQ             = EDESTADDRREQ;
    WSAEMSGSIZE                 = EMSGSIZE;
    WSAEPROTOTYPE               = EPROTOTYPE;
    WSAENOPROTOOPT              = ENOPROTOOPT;
    WSAEPROTONOSUPPORT          = EPROTONOSUPPORT;
    WSAESOCKTNOSUPPORT          = ESOCKTNOSUPPORT;
    WSAEOPNOTSUPP               = EOPNOTSUPP;
    WSAEPFNOSUPPORT             = EPFNOSUPPORT;
    WSAEAFNOSUPPORT             = EAFNOSUPPORT;            // EAI_FAMILY
    WSAEADDRINUSE               = EADDRINUSE;
    WSAEADDRNOTAVAIL            = EADDRNOTAVAIL;
    WSAENETDOWN                 = ENETDOWN;
    WSAENETUNREACH              = ENETUNREACH;
    WSAENETRESET                = ENETRESET;
    WSAECONNABORTED             = ECONNABORTED;
    WSAECONNRESET               = ECONNRESET;
    WSAENOBUFS                  = ENOBUFS;
    WSAEISCONN                  = EISCONN;
    WSAENOTCONN                 = ENOTCONN;
    WSAESHUTDOWN                = ESHUTDOWN;
    WSAETOOMANYREFS             = ETOOMANYREFS;
    WSAETIMEDOUT                = ETIMEDOUT;
    WSAECONNREFUSED             = ECONNREFUSED;
    WSAELOOP                    = ELOOP;
    WSAENAMETOOLONG             = ENAMETOOLONG;
    WSAEHOSTDOWN                = EHOSTDOWN;
    WSAEHOSTUNREACH             = EHOSTUNREACH;
    WSAENOTEMPTY                = ENOTEMPTY;
    WSAEPROCLIM                 = WSABASEERR + 67;    { V8.65 }
    WSAEUSERS                   = EUSERS;
    WSAEDQUOT                   = EDQUOT;
    WSAESTALE                   = ESTALE;
    WSAEREMOTE                  = EREMOTE;
    WSANO_DATA                  = ENODATA;

   { WSA startup etc
    WSASYSNOTREADY              = ELAST + 1;
    WSAVERNOTSUPPORTED          = ELAST + 2;
    WSANOTINITIALISED           = ELAST + 3; }

    WSAELAST                    = WSANO_DATA;

    MAXGETHOSTSTRUCT      = 1024;
    IPV6_ADD_MEMBERSHIP   = IPV6_JOIN_GROUP;

//    IsIPv6APIAvailable    = True;
//    IsIPv6Available       = True;  { V9.3 use functions }

type
    TSocket               = Integer;
    PSocket               = ^TSocket;
    TSockAddr             = sockaddr_in;  //sockaddr; as in windows
    PSockAddr             = ^TSockAddr;
    TInAddr               = in_addr;
    PInAddr               = Pin_addr;
    TSockAddrIn           = sockaddr_in;
    PSockAddrIn           = Psockaddr_in;
    TSockAddrIn6          = sockaddr_in6;
    PSockAddrIn6          = Psockaddr_in6;
    TInAddr6              = in6_addr;
    PInAddr6              = Pin6_addr;
    TAddrInfo             = addrinfo;      { in Posix.NetDB, NetDBTypes.inc }
    TIpv6MReq             = ipv6_mreq;

//    u_short               = UInt16;   { moved to Ics.Posix.WinTypes }
//    u_long                = UInt32;
//    u_int                 = UInt32;
    TLinger               = linger;

    SunB = packed record
      s_b1, s_b2, s_b3, s_b4: Byte;
    end;

    SunW = packed record
      s_w1, s_w2: Word;
    end;

    TIcsInAddr = record
      case integer of
        0: (S_un_b: SunB);
        1: (S_un_w: SunW);
        2: (S_addr: u_long);
    end;
    PIcsInAddr = ^TIcsInAddr;

    TPipeFd = record
      Read  : Integer;
      Write : Integer;
    end;
    PPipeFd = ^TPipeFd;

    TTimeVal = timeval;
    PTimeval = ^TTimeVal;

    IN6_ADDR = record
    case Integer of
        0: (Byte     : array [0..15] of AnsiChar);
        1: (Word     : array [0..7]  of Word);
        2: (s6_bytes : array [0..15] of Byte);
        3: (s6_addr  : array [0..15] of Byte);
        4: (s6_words : array [0..7]  of Word);
    end;
    TIn6Addr   = IN6_ADDR;
    PIn6Addr   = ^TIn6Addr;
    PIN6_ADDR  = PIn6Addr;

{$ENDIF POSIX}


{$IFDEF MSWINDOWS}
type
    TIcsInAddr = TInAddr;
    PIcsInAddr = ^TIcsInAddr;
{$ENDIF}

type
  { C++Builder < 2010 (2009?) cannot wrap an array as record with methods as }
  { function result. So changed type to a record :(                          }
  //TIcsIPv6Address    = array [0..7] of Word;
  TIcsIPv6Address = record
    Words : array [0..7] of Word;
  end;
  PIcsIPv6Address    = ^TIcsIPv6Address;
  TIcsIPv4Address    = LongWord;  // V9.4  {x$IFDEF POSIX} Cardinal {.$ELSE} Integer {.$ENDIF};
  PIcsIPv4Address    = ^TIcsIPv4Address;
  TTcsIpBytes        = TBytes;        { V9.5 IPv4 or IPv6 bytes, 4 or 16 long }

  TSocketState       = (wsInvalidState,
                        wsOpened,     wsBound,
                        wsConnecting, wsSocksConnected, wsConnected,
                        wsAccepting,  wsListening,
                        wsClosed, wsDnsLookup);  { V8.48 added DnsLookup }
  TSocketSendFlags   = (wsSendNormal, wsSendUrgent);
  TSocketLingerOnOff = (wsLingerOff, wsLingerOn, wsLingerNoSet);
  TSocketKeepAliveOnOff = (wsKeepAliveOff, wsKeepAliveOnCustom,
                           wsKeepAliveOnSystem);
  TSocketErrs        = (wsErrTech, wsErrFriendly);  { V8.36 }

  TWSocketOption       = (wsoNoReceiveLoop,
                          wsoTcpNoDelay,
                          wsoSIO_RCVALL,      { receive all packets for raw socket monitoring }
                          wsoNoHttp10Tunnel,  { The HTTP tunnel supports HTTP/1.1 this causes HTTP/1.0 responses to be treated as errors.  }
                          wsoNotifyAddressListChange,
                          wsoNotifyRoutingInterfaceChange,
                          wsoAsyncDnsLookup,     { V8.43 Connect uses Async lookup }
                          wsoIcsDnsLookup,       { V8.43 DNSLookup uses thread }
                          wsoUseSTD3AsciiRules,  { V8.64 stop illegal symbols in domain names }
                          wsoIgnoreIDNA,         { V8.64 old behaviour for ANSI domain names }
                          wsoNoSendException);   { V8.70 no exception for 'not connected' in Send }
  TWSocketOptions      = set of TWSocketOption;   { published as ComponentOptions }

  TTcpKeepAlive = packed record
    OnOff             : u_long;
    KeepAliveTime     : u_long;
    KeepAliveInterval : u_long;
  end;


  THttpTunnelServerAuthTypes = set of (htsatBasic, htsatNtlm, htsatDigest);
  THttpTunnelState = (htsData, htsConnecting, htsConnected,
                      htsWaitResp0, htsWaitResp1, htsWaitResp2);
  THttpTunnelChunkState  = (htcsGetSize,     htcsGetExt, htcsGetData,
                            htcsGetBoundary, htcsDone);
  THttpTunnelProto       = (htp11, htp10);
  THttpTunnelReconnectRequest = (htrrNone, htrrBasic, htrrDigest, htrrNtlm1);

  TSocksState          = (socksData, socksNegociateMethods, socksAuthenticate, socksConnect);
  TSocksAuthentication = (socksNoAuthentication, socksAuthenticateUsercode);
  TSocksAuthState      = (socksAuthStart, socksAuthSuccess, socksAuthFailure, socksAuthNotRequired);

const
    ICS_SOCKS_BASEERR                   = WSABASEERR + 10000;// 20000;
    ICS_SOCKS_MAXERR                    = ICS_SOCKS_BASEERR + 17;
    ICS_HTTP_TUNNEL_MAXSTAT             = 599; // Max. HTTP status code
    ICS_HTTP_TUNNEL_BASEERR             = ICS_SOCKS_BASEERR + 1000;//21000;
    ICS_HTTP_TUNNEL_MAXERR              = ICS_HTTP_TUNNEL_BASEERR + ICS_HTTP_TUNNEL_MAXSTAT;
    ICS_HTTP_TUNNEL_PROTERR             = ICS_HTTP_TUNNEL_BASEERR;
    ICS_HTTP_TUNNEL_GENERR              = ICS_HTTP_TUNNEL_BASEERR + 1;
    ICS_HTTP_TUNNEL_VERSIONERR          = ICS_HTTP_TUNNEL_GENERR  + 1;

    socksNoError              = ICS_SOCKS_BASEERR;
    socksProtocolError        = ICS_SOCKS_BASEERR + 1;
    socksVersionError         = ICS_SOCKS_BASEERR + 2;
    socksAuthMethodError      = ICS_SOCKS_BASEERR + 3;
    socksGeneralFailure       = ICS_SOCKS_BASEERR + 4;
    socksConnectionNotAllowed = ICS_SOCKS_BASEERR + 5;
    socksNetworkUnreachable   = ICS_SOCKS_BASEERR + 6;
    socksHostUnreachable      = ICS_SOCKS_BASEERR + 7;
    socksConnectionRefused    = ICS_SOCKS_BASEERR + 8;
    socksTtlExpired           = ICS_SOCKS_BASEERR + 9;
    socksUnknownCommand       = ICS_SOCKS_BASEERR + 10;
    socksUnknownAddressType   = ICS_SOCKS_BASEERR + 11;
    socksUnassignedError      = ICS_SOCKS_BASEERR + 12;
    socksInternalError        = ICS_SOCKS_BASEERR + 13;
    socksDataReceiveError     = ICS_SOCKS_BASEERR + 14;
    socksAuthenticationFailed = ICS_SOCKS_BASEERR + 15;
    socksRejectedOrFailed     = ICS_SOCKS_BASEERR + 16;
    socksHostResolutionFailed = ICS_SOCKS_BASEERR + 17;

type
  TSslState = (sslNone,
                 sslHandshakeInit,
                 sslHandshakeStarted,
                 sslHandshakeFailed,
                 sslEstablished,
                 sslInShutdown,
                 sslShutdownComplete);

  TSslEvent = (sslFdRead, sslFdWrite, sslFdClose);
  TSslPendingEvents = set of TSslEvent;
  TSslMode  = (sslModeClient, sslModeServer);

{ V9.5 application logging event to replace lots of variants in different components }
{ note: different to TIcsLogEvent in OverbyteIcsLogger.pas which is low level debug diagnostics only }
  TIcsAppLogEvent = procedure (Sender: TObject; const Msg: string) of object;     { V9.5 }

{ V9.5 events used in components that need to lookup a geographic ISO code or country name using the
    TIcsGeoTools component in an application }
  TIcsCountryISOEvent = procedure (Sender: TObject; const IpStr: string; var ISOA2: String) of object;            { V9.5 }
  TIcsCountryNameEvent = procedure (Sender: TObject; const ISOA2: string; var CountryName: String) of object;     { V9.5 }
  TIcsGeoEvent = procedure (Sender: TObject; const IpStr: string; var Geo: String) of object;                     { V9.5 }

const
  SocketStateNames: array [TSocketState] of PChar = ('Invalid', 'Opened', 'Bound',
      'Connecting', 'SocksConnected', 'Connected', 'Accepting', 'Listening', 'Closed', 'DnsLookup');                                                        { V8.48 }
  SocketFamilyNames: array [TSocketFamily] of PChar = ('Any', 'Prefer IPv4', 'Prefer IPv6', 'Only IPv4', 'Only IPv6');                                        { V8.60 made more descriptive }
  PROXY_PROTO_HTTP = 'http';                                                         { V8.66 }
  PROXY_PROTO_SOCKS5 = 'socks5';                                                     { V8.66 }



{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ following moved from OverbyteIcsLogger }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

type
    TLogOption = (loDestEvent,   loDestFile,     loDestOutDebug,  { Output Destinations }
                  loAddStamp,                                     { Adds something (slow) }
                  loWsockErr,    loWsockInfo,    loWsockDump,
                  loSslErr,      loSslInfo,      loSslDevel,   loSslDump,  { V8.40 added Devel }
                  loProtSpecErr, loProtSpecInfo, loProtSpecDump, loProgress); { V6.68 added Progress }
    TLogOptions = set of TLogOption;
    TLogFileOption = (lfoAppend, lfoOverwrite);
{$IFDEF COMPILER12_UP}
    TLogFileEncoding = (lfeUtf8, lfeUtf16);
{$ENDIF}
const
    LogAllOptErr  =  [loWsockErr, loSslErr, loProtSpecErr];
    LogAllOptInfo =  LogAllOptErr + [loWsockInfo, loSslInfo, loProtSpecInfo];
    LogAllOptDump =  LogAllOptInfo + [loWsockDump, loSslDump, loProtSpecDump];

type
    TNTEventType = (etError, etWarning, etInformation, etAuditSuccess, etAuditFailure);

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ following moved from OverbyteIcsHttpProt }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

const
    { EHttpException error code }
    httperrBase                     = {$IFDEF MSWINDOWS} 1 {$ELSE} 1001 {$ENDIF}; { V7.23 }
    httperrNoError                  = 0;
    httperrBusy                     = httperrBase;
    httperrNoData                   = httperrBusy + 1;
    httperrAborted                  = httperrNoData + 1;
    httperrOverflow                 = httperrAborted + 1;
    httperrVersion                  = httperrOverflow + 1;
    httperrInvalidAuthState         = httperrVersion + 1;
    httperrSslHandShake             = httperrInvalidAuthState + 1;
    httperrCustomTimeOut            = httperrSslHandShake + 1;
    httperrNoStatusCode             = httperrCustomTimeOut + 1;
    httperrOutOfMemory              = httperrNoStatusCode + 1;        { V8.68 }
    httperrBgException              = httperrOutOfMemory + 1;         { V8.68 }
    { Change next as well if new EHttpException error codes are added }
    httperrMax                      = httperrBgException;

type
    THttpBigInt = Int64;

    THttpEncoding    = (encUUEncode, encBase64, encMime);
    THttpRequest     = (httpABORT, httpGET, httpPOST, httpPUT, httpHEAD, httpDELETE, httpCLOSE, httpPATCH, httpOPTIONS, httpTRACE);  { V8.09 }
    THttpState       = (httpReady, httpNotConnected, httpConnected, httpDnsLookup, httpDnsLookupDone, httpWaitingHeader,
                            httpWaitingBody, httpBodyReceived, httpWaitingProxyConnect, httpClosing, httpAborting);
    THttpChunkState  = (httpChunkGetSize, httpChunkGetExt, httpChunkGetData, httpChunkSkipDataEnd, httpChunkDone);
    THttpBasicState  = (basicNone, basicMsg1, basicDone);
    THttpAuthType    = (httpAuthNone, httpAuthBasic, httpAuthNtlm, httpAuthDigest, httpAuthBearer, httpAuthToken,   { V8.54 }
                           httpAuthJWT, httpAuthOAuth, httpAuthDigest2);  { V8.62, V8.65, V8.69 }
    THttpCliOption = (httpoNoBasicAuth, httpoNoNTLMAuth, httpoBandwidthControl,
                      httpoEnableContentCoding, httpoUseQuality,
                      httpoNoDigestAuth, httpoAllowAnchor, httpoGetContent);     { V8.69 allow # fragment anchor in URL }
                                                                                 { V8.71 allow content in GET and DELETE, like PUT }
    THttpCliOptions = set of THttpCliOption;
    TWWWAuthInfo = record
       AuthType: THttpAuthType;
       Realm: String;
       CharSet: String;
       Uri: String;
       Header: String;
    end;                                                                { V8.69 parsed from WWW-Authenticate headers }
    TWWWAuthInfos = Array of TWWWAuthInfo;                              { V8.69 }
    THttpAuthTypes = Set of THttpAuthType;                              { V8.69 }

const
    HttpCliAuthNames: array [THttpAuthType] of PChar = (              { V8.69 }
        'None','Basic (Clear)','NTLM Domain','Digest MD5','Bearer Token','XAuth Token', 'Json Web Token','OAuth Bearer','Digest SHA-256');


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ following moved from OverbyteIcsSmtpProt }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
type
    TSmtpDefaultEncoding = (smtpEnc7bit, smtpEnc8bit, smtpEncQuotedPrintable, smtpEncBase64);   {AG}
    TSmtpSendMode    = (smtpToSocket, smtpToStream, smtpCopyToStream);
    TSmtpState       = (smtpReady, smtpDnsLookup, smtpConnecting, smtpConnected, smtpInternalReady,
                           smtpWaitingBanner, smtpWaitingResponse, smtpAbort, smtpInternalBusy);
    TSmtpMimeState   = (smtpMimeIntro, smtpMimePlainText, smtpMimeHtmlText, smtpMimeImages, smtpMimeAttach, smtpMimeDone);
    TSmtpRequest     = (smtpConnect, smtpHelo, smtpMailFrom, smtpVrfy, smtpRcptTo, smtpData, smtpQuit, smtpRset,
                          smtpOpen, smtpMail, smtpEhlo, smtpAuth, {$IFDEF USE_SSL} smtpStartTls, {$ENDIF}
                          smtpCalcMsgSize, smtpMailFromSIZE, smtpToFile, smtpCustom);
    TSmtpProxyType   = (smtpNoProxy, smtpSocks4, smtpSocks4A, smtpSocks5, smtpHttpProxy);
    TSmtpContentType = (smtpHtml, smtpPlainText);
    TSmtpAuthType    = (smtpAuthNone, smtpAuthPlain, smtpAuthLogin, smtpAuthCramMD5, smtpAuthCramSha1,
                          smtpAuthNtlm, smtpAuthAutoSelect, smtpAuthXOAuth2, smtpAuthOAuthBearer);  { V8.65 two OAuths }
    TSmtpShareMode   = (smtpShareCompat, smtpShareExclusive, smtpShareDenyWrite, smtpShareDenyRead, smtpShareDenyNone);
    TSmtpPriority    = (smtpPriorityNone, smtpPriorityHighest, smtpPriorityHigh, smtpPriorityNormal, smtpPriorityLow, smtpPriorityLowest);
    TSmtpEncoding    = (smtpEncodeNone, smtpEncodeBase64, smtpEncodeQP);

    TSmtpSslType     = (smtpTlsNone,  smtpTlsImplicit,  smtpTlsExplicit);
 { smtpTlsImplicit means SSL only, usually port 465, officially this port is obsolete }
 { smtpTlsExplicit means optional SSL, ports 25 or 587, sometimes 2525 }

const
    SmtpDefEncArray : array [TSmtpDefaultEncoding] of PChar = ('7bit', '8bit', 'quoted-printable', 'base64'); {AG}

{ List of separators accepted between email addresses }
const
    SmtpEMailSeparators = [';', ','];

{ V8.65 some literals for applications }
    SmtpAuthTypeNames: array [TSmtpAuthType] of PChar =
                ('None','Plain','Login','Cram-Md5','Cram-Sha1','NTLM','Auto Select','XOAuth2','OAuthBearer');

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ following moved from OverbyteIcsFtpCli }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
type
  { sslTypeAuthTls, sslTypeAuthSsl are known as explicit SSL }
  TFtpCliSslType  = (sslTypeNone, sslTypeAuthTls, sslTypeAuthSsl,        { V2.106 }
                     sslTypeImplicit);
  TFtpOption      = (ftpAcceptLF, ftpNoAutoResumeAt, ftpWaitUsingSleep,
                     ftpBandwidthControl, ftpAutoDetectCodePage,
                     ftpFixPasvLanIP, ftpNoExtV4); { V2.106 }{ AG V7.02 } { V8.63 FixPasvLan }  { V9.4 ftpNoExtV4 }
  TFtpOptions     = set of TFtpOption;
  TFtpExtension   = (ftpFeatNone, ftpFeatSize, ftpFeatRest, ftpFeatMDTMYY,
                     ftpFeatMDTM, ftpFeatMLST, ftpFeatMFMT, ftpFeatMD5,
                     ftpFeatAuthSSL, ftpFeatAuthTLS, ftpFeatProtP,
                     ftpFeatProtC, ftpFeatModeZ, ftpFeatCcc, ftpFeatPbsz,
                     ftpFeatXCrc,  ftpFeatXMD5,  ftpFeatSitePaswd,          { V2.113 }
                     ftpFeatSiteExec, ftpFeatSiteIndex, ftpFeatSiteZone,    { V2.113 }
                     ftpFeatSiteMsg, ftpFeatSiteCmlsd, ftpFeatSiteDmlsd,    { V2.113 }
                     ftpFeatClnt, ftpFeatComb, ftpFeatUtf8, ftpFeatLang,    { V2.113 }
                     ftpFeatHost, ftpFeatXCmlsd, ftpFeatXDmlsd,             { V7.01 }
                     ftpFeatEprt, ftpFeatEpsv);                             { V9.4 }
  TFtpExtensions  = set of TFtpExtension; { V2.94 which features server supports }
  TFtpTransMode   = (ftpTransModeStream, ftpTransModeZDeflate) ;  { V2.102 }
  TZStreamState   = (ftpZStateNone, ftpZStateSaveDecom, ftpZStateSaveComp{,
                     ftpZStateImmDecon, ftpZStateImmComp});   { V2.102 }
  TFtpState       = (ftpNotConnected,  ftpReady,         ftpInternalReady,
                     ftpDnsLookup,     ftpConnected,     ftpAbort,
                     ftpInternalAbort, ftpWaitingBanner, ftpWaitingResponse,
                     ftpPasvReady);
  TFtpShareMode   = (ftpShareCompat,    ftpShareExclusive,
                     ftpShareDenyWrite, ftpShareDenyRead,
                     ftpShareDenyNone);
  TFtpDisplayFileMode = (ftpLineByLine, ftpBinary);
  TFtpConnectionType  = (ftpDirect, ftpProxy, ftpSocks4, ftpSocks4A, ftpSocks5, ftpHttpProxy);


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ following moved from OverbyteIcsFileCopy }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

 type
// file copy selection and replace options
    TIcsFileCopyType = (FCTypeSingle, FCTypeMaskDir, FCTypeArchDir, FCTypeAllDir, FCTypeDates) ;
    TIcsFileCopyRepl = (FCReplNever, FCReplAlways, FCReplDiff, FCReplNewer) ;
    TIcsFileCopyState = (FCStateNone, FCStateIgnore,FCStateDir, FCStateSelect, FCStateCopying, FCStateOK, FCStateFailed) ;
    TIcsCopyLogLevel = (LogLevelInfo, LogLevelFile, LogLevelProg, LogLevelDiag, LogLevelDelimFile, LogLevelDelimTot) ;
    TIcsTaskResult = (TaskResNone, TaskResOKNew, TaskResOKNone, TaskResFail, TaskResAbort, TaskRunning) ;
    TIcsSslCertCheck = (SslCCNone, SslCCWarn, SslCCRequire) ;    // 11 Nov 2005

const
    IcsTaskResultNames: array [TIcsTaskResult] of string =
      ('No Result', 'OK New', 'OK None', 'Failed', 'Aborted', 'Running') ;
    IcsTaskResultStrings: array [TIcsTaskResult] of String =         { V8.70 was shortstring }
      ('No Result', 'OK New', 'OK None', 'Failed', 'Aborted', 'Running') ;
    IcsSslCertCheckStrings: array [TIcsSslCertCheck] of String =
      ('None', 'Warn', 'Require') ;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ following moved from OverbyteIcsSSLEAY }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

var
{ V9.3 set GSSL_LOAD_OPENSSL_MYSELF true in your project dpr file to prevent OpenSSL being loaded
       automatically on startup, effectively overriding DEFINE OpenSSL_AutoLoad_CA_Bundle  }
//    GSSL_LOAD_OPENSSL_MYSELF    : Boolean = False;

{$IFDEF YuOpenSSL}
    GLIBEAY_DLL_FileName        : String  = 'Statically Linked';  { V8.66 }
    GSSLEAY_DLL_FileName        : String  = 'Statically Linked';  { V8.66 }
    GSSLStaticLinked            : Boolean = True;                 { V8.66 }
{$ELSE}
    GLIBEAY_DLL_FileName        : String  = '*NOT LOADED*';
    GSSLEAY_DLL_FileName        : String  = '*NOT_LOADED*';
    GSSLStaticLinked            : Boolean = False;                 { V8.66 }
{$ENDIF YuOpenSSL}

    GSSLEAY_DLL_Handle          : THandle = 0;
    GSSLEAY_DLL_FileVersion     : String = '';
    GSSLEAY_DLL_FileDescription : String = '';
 { V8.65 don't attempt to find new name libcrypto-3_0.dll, use libcrypto-1_1.dll }
    GSSLEAY_DLL_IgnoreNew       : Boolean = False;
 { V8.69 don't attempt to use old name libcrypto-1_1.dll, use libcrypto-3_0.dll }
    GSSLEAY_DLL_IgnoreOld       : Boolean = False;  { V8.69 false, don't ignore 1.1 }
 { NOTE - both true now allowed, V9.1 both ignored since 1.1 no longer supported }
 { V8.27 write buffer size, was fixed at 4096, but send used a 16K buffer }
    GSSL_BUFFER_SIZE            : Integer = 16384;
 { V8.27 if set before OpenSSL loaded, will use this directory for DLLs, must have trailing \ }
    GSSL_DLL_DIR                : String = '';
{ V9.5 any OpenSSL loading error messages }
    GSSL_LOAD_ERRS              : String;

 { V8.38 wintrust stuff for authenticode digital signing checking }
    GSSL_SignTest_Check         : Boolean = False;    { check OpenSSL DLLs are digitally signed }
    GSSL_SignTest_Certificate   : Boolean = False;    { False only checks hash, True also checks certificate (longer) }
    GSSL_SignTest_Failed        : Boolean = False;    { V9.3 true if digital signing test failed and OpenSSL is not loaded }
    GSSL_SignTest_FailError     : String = '';        { V9.3 error result from failed digital signing test }

    { Version stuff added 07/12/05  = V8.27 moved from OverbyteIcsLIBEAY  }
    ICS_OPENSSL_VERSION_NUMBER  : Cardinal = {$IFDEF YuOpenSSL}YuOpenSSL.OPENSSL_VERSION_NUMBER{$ELSE}0{$ENDIF};
    ICS_SSL_NO_RENEGOTIATION    : Boolean = TRUE;    { V8.67 we removed renegotiation }
    ICS_RAND_INIT_DONE          : Boolean = FALSE;   { V8.35 have we initialised random numbers }

  { V8.67 note ICS_OPENSSL_VERSION_NUMBER is deprecated in OpenSSL 3.0 and later, use OPENSSL_VERSION_xx macros instead }
  { note need ICS prefix since OPENSSL_version_major is an export and we are case insensitive }
    ICS_OPENSSL_VERSION_MAJOR   : Integer = {$IFDEF YuOpenSSL}OPENSSL_VERSION_MAJOR_{$ELSE}0{$ENDIF};           { V8.67 1 or 3, macros in OpenSSL 3.0 }
    ICS_OPENSSL_VERSION_MINOR   : Integer = {$IFDEF YuOpenSSL}OPENSSL_VERSION_MINOR_{$ELSE}0{$ENDIF};           { V8.67 0 >  }
    ICS_OPENSSL_VERSION_PATCH   : Integer = {$IFDEF YuOpenSSL}OPENSSL_VERSION_PATCH_{$ELSE}0{$ENDIF};           { V8.67 0 >  }
    ICS_OPENSSL_VERSION_PRE_RELEASE : String = {$IFDEF YuOpenSSL}OPENSSL_VERSION_PRE_RELEASE_{$ELSE}''{$ENDIF}; { V8.67 0 >  }
    ICS_OSSL3_LOADED_LEGACY     : Boolean = False;   { V8.67 is OpenSSL 3.0 legacy provider loaded }
    ICS_OSSL3_LOADED_FIPS       : Boolean = False;   { V8.67 is OpenSSL 3.0 FIPS provider loaded }
    GSSLEAY_LOAD_LEGACY         : Boolean = False;   { V8.67 should legacy provider be loaded on start-up }
    GSSLEAY_PROVIDER_DEFAULT    : Pointer = Nil;     { V8.67 POSSL_PROVIDER for default provider, if loaded }
    GSSLEAY_PROVIDER_LEGACY     : Pointer = Nil;     { V8.67 POSSL_PROVIDER for legacy provider, if loaded }
    GSSLEAY_PROVIDER_FIPS       : Pointer = Nil;     { V8.67 POSSL_PROVIDER for FIPS provider, if loaded }

 { V9.1 sub-directory in GSSLEAY_PUBLIC_DIR where OpenSSL is expected }
    GSSL_RES_SUBDIR          : String = 'ICS-OpenSSL';
 { V9.1 if blank when OpenSSL loaded, set to TPath.GetPublicPath or c:\ProgramData, with GSSLEAY_RES_SUBDIR added }
 { ICS looks for OpenSSL DLLs and root CA bundles in this directory }
 { if the OpenSSL DLLs are linked as resource, a version sub-directory will be added }
    GSSL_PUBLIC_DIR         : string = '';

{ V9.1 directories built when SSL loads for ICS certificates and Root CAs }
    GSSL_CERTS_SUBDIR       : String = 'ICS-Certs\';
    GSSL_ROOTS_SUBDIR       : String = 'ICS-RootCAs\';
    GSSL_ROOTCA_NAME        : String = 'ICSRootCA.pem';
    GSSL_LOCALHOST_NAME     : String = 'localhost-bundle.pem';
    GSSL_INTER_NAME         : String = 'ICS_Intermediate_Short-bundle.pem';
    GSSL_DEFROOT_NAME       : String = 'DefRootCABundle.pem';
    GSSL_EXTRAROOT_NAME     : String = 'ExtraRootCABundle.pem';
    GSSL_CERTS_DIR          : String = '';      // C:\ProgramData\ICS-OpenSSL\ICS-Certs\
    GSSL_ROOTS_DIR          : String = '';      // C:\ProgramData\ICS-OpenSSL\ICS-RootCAs\
    GSSL_INTER_FILE         : String = 'Internal';   // means use resource file, change for a real file name
    GSSL_INTER_CNAME        : String = 'ICS Intermediate';   // part name, may have Short added

  { V8.62 dynamically added NID objects, if any set need call OBJ_cleanup on close down }
    ICS_NID_acmeIdentifier      : Integer = 0;


const
 { found in \include\openssl\opensslv.h, moved to \VERSION.dat for 3.0
{ only supporting versions with TLS 1.1, 1.2 and 1.3 }
{ last digit is 0 for dev/beta, F for final release }
{ V8.27 moved from OverbyteIcsLIBEAY  }
{ V8.66 1.0.2 and 1.1.0 support ceased Dec 2019, now removed from ICS }
{ V9.1 1.1.1 support ceased Sept 2023, now removed from ICS }
    OSSL_VER_MIN    = $0000000F; // minimum version     { V8.35 }
    OSSL_VER_1101   = $1010100F; // 1.1.1 base                { V8.57 }
    OSSL_VER_1101ZZ = $10101FFF; // 1.1.1zz not yet released  { V8.57 }
    OSSL_VER_3000   = $30000000; // 3.0.0 base                { V8.65 }
    OSSL_VER_3100   = $30100000; // 3.1.0 base                { V8.71 }
    OSSL_VER_3200   = $30200000; // 3.2.0 base                { V8.71 }
    OSSL_VER_3300   = $30300000; // 3.3.0 base                { V9.1 }
    OSSL_VER_3400   = $30400000; // 3.4.0 base                { V9.1 }
    OSSL_VER_3500   = $30500000; // 3.5.0 base                { V9.5 }
    OSSL_VER_3600   = $30600000; // 3.6.0 base                { V9.5 }
    OSSL_VER_3LAST  = $3FFFFFFF; // 3 last                    { V8.67 }
    OSSL_VER_4000   = $40000000; // 4.0.0 base                { V9.5 }
    OSSL_VER_MAX    = $FFFFFFFF; // maximum version           { V8.35 }

    { Basically versions listed above are tested if not otherwise commented.  }
    { Versions between are assumed to work, however they are untested.        }
    { OpenSSL libraries for ICS are available for download here:              }
    { http://wiki.overbyte.be/wiki/index.php/ICS_Download                     }

    MIN_OSSL_VER   = OSSL_VER_3000;     { V9.1 minimum is now 3.0 }
    MAX_OSSL_VER   = OSSL_VER_3LAST;    { V8.67 }

type
  { V8.57 whether an SSL server asks a client to send an SSL certificate }
    TSslCliCertMethod = (sslCliCertNone, sslCliCertOptional, sslCliCertRequire);

  { V8.57 certificate supplier protocol, determines which functions are used to get certificates }
    TSupplierProto = (SuppProtoNone, SuppProtoOwnCA, SuppProtoAcmeV2,    { V8.62 Acmev1 gone }
                      SuppProtoCertCentre, SuppProtoServtas);            { V9.5 CertCentre gone, Servtas never supported }

 { V9.5 Acme protocol now supported by most certificate providers }
    TAcmeSupplier = (AcmeLetsEncrypt, AcmeLetsEncryptTest, {AcmeBuypass, AcmeBuypassTest,}  { Bypass not supporting Acme }
                     AcmeZeroSSL, AcmeGoogle, AcmeGoogleTest, AcmeDigicert, AcmeDigicertTest,
                     AcmeSslcomRSA, AcmeSslcomECC);

 { V8.57 challenge types, differing certificate types support differing challenges,
     some have to be processed manually taking several days. }
    TChallengeType = (ChallNone, ChallFileUNC, ChallFileFtp, ChallFileSrv,
                      ChallFileApp, ChallDnsAuto, ChallDnsAcnt, ChallDnsMan,   { V8.64 DnsMan added, V9.5 ChallDnsAcnt added  }
                      ChallEmail, ChallAlpnUNC, ChallAlpnSrv, ChallAlpnApp,    { V8.62 App added }
                      ChallManual);

 { V8.71 IcsHosts SSL certificate loading source, file or Windows Store }
    TSslLoadSource = (CertLoadFile, CertWinStoreMachine, CertWinStoreUser);

 { V9.5 TLSEXT_cert_type_xx literals, for SslContext }
   TTlsCertType = (CertTypeX509, CertTypeRPK);
   TTlsCertTypes = set of TTlsCertType;

{ V8.40 OpenSSL streaming ciphers with various modes }
{ note OpenSSL 3 considers Blowfish, Cast, DES, IDEA, RC2/4/5 and SEED legacy and not supported by default }
{ pending support SM4, Camellia }
    TEvpCipher = (
        Cipher_none,
        Cipher_aes_128_cbc,
        Cipher_aes_128_cfb,
        Cipher_aes_128_ecb,
        Cipher_aes_128_ofb,
        Cipher_aes_128_gcm,
        Cipher_aes_128_ocb,
        Cipher_aes_128_ccm,
        Cipher_aes_192_cbc,
        Cipher_aes_192_cfb,
        Cipher_aes_192_ecb,
        Cipher_aes_192_ofb,
        Cipher_aes_192_gcm,
        Cipher_aes_192_ocb,
        Cipher_aes_192_ccm,
        Cipher_aes_256_cbc,    { used for PKC12 }
        Cipher_aes_256_cfb,
        Cipher_aes_256_ecb,
        Cipher_aes_256_ofb,
        Cipher_aes_256_gcm,
        Cipher_aes_256_ocb,
        Cipher_aes_256_ccm,
        Cipher_bf_cbc,        { blowfish needs key length set, 128, 192 or 256 }
        Cipher_bf_cfb64,
        Cipher_bf_ecb,
        Cipher_bf_ofb,
        Cipher_chacha20,      { chacha20 fixed 256 key }
        Cipher_des_ede3_cbc,
        Cipher_des_ede3_cfb64,
        Cipher_des_ede3_ecb,
        Cipher_des_ede3_ofb,
        Cipher_idea_cbc,      { IDEA fixed 128 key }
        Cipher_idea_cfb64,
        Cipher_idea_ecb,
        Cipher_idea_ofb);


{ V8.40 OpenSSL message digests or hashes }
{ note OpenSSL 3 considers MD5, MDC2, Whirlppol and RIPEMD legacy and not supported by default }
    TEvpDigest = (
        Digest_md5,
        Digest_mdc2,
        Digest_sha1,
        Digest_sha224,
        Digest_sha256,
        Digest_sha384,
        Digest_sha512,
        Digest_ripemd160,
        Digest_sha3_224,    { following V8.51 }
        Digest_sha3_256,
        Digest_sha3_384,
        Digest_sha3_512,
        Digest_shake128,
        Digest_shake256,
        Digest_None);       { V8.52 }

{ V8.40 ICS private key algorithm and key length in bits }
{ bracketed comment is security level and effective bits,
  beware long RSA key lengths increase SSL overhead heavily }
{ creating new RSA keys is computationally expensive, 4096 bits
  a couple of seconds, 7680 maybe a minute, 15360 hours }
    TSslPrivKeyType = (
        PrivKeyRsa1024,   { level 1 - 80 bits  }
        PrivKeyRsa2048,   { level 2 - 112 bits }
        PrivKeyRsa3072,   { level 3 - 128 bits }
        PrivKeyRsa4096,   { level 3 - 128 bits }
        PrivKeyRsa7680,   { level 4 - 192 bits }
        PrivKeyRsa15360,  { level 5 - 256 bits }
        PrivKeyECsecp256, { level 3 - 128 bits secp256r1 }
        PrivKeyECsecp384, { level 4 - 192 bits }
        PrivKeyECsecp512, { level 5 - 256 bits }
        PrivKeyEd25519,   { level 3 - 128 bits }    { V8.50 was PrivKeyECX25519 }
        PrivKeyRsaPss2048,   { level 2 - 112 bits } { V8.51 several RsaPss keys }
        PrivKeyRsaPss3072,   { level 3 - 128 bits }
        PrivKeyRsaPss4096,   { level 3 - 128 bits }
        PrivKeyRsaPss7680,   { level 4 - 192 bits }
        PrivKeyRsaPss15360,  { level 5 - 256 bits }
        PrivKeyECsecp256k);  { level 3 - 128 bits secp256k1 }    { V8.67 alternate kobitz curve }

{ V8.40 ICS private key file encryption and mapping to OpenSSL params }
   TSslPrivKeyCipher = (
        PrivKeyEncNone,
        PrivKeyEncTripleDES,    { V8.67 legacy algorithms need special loaded in OpenSSL 3.0 }
        PrivKeyEncIDEA,
        PrivKeyEncAES128,
        PrivKeyEncAES192,
        PrivKeyEncAES256);
  //      PrivKeyEncBlowfish128);
      {  PrivKeyEncBlowfish192,   V8.62 now sure we need these
        PrivKeyEncBlowfish256); }

const
   SslPrivKeyEvpCipher:  array[TSslPrivKeyCipher] of TEvpCipher = (
        Cipher_none,
        Cipher_des_ede3_cbc,
        Cipher_idea_cbc,
        Cipher_aes_128_cbc,
        Cipher_aes_192_cbc,
        Cipher_aes_256_cbc);
     //   Cipher_bf_cbc);

 //   SslPrivKeyEvpBits: array[TSslPrivKeyCipher] of integer = (    { V8.67 Blowfish gone }
 //        0,0,0,0,0,0,128{,192,256});

type
   { V8.57 SSL/TLS certificate root validation method, V9.3 added CertVerOwnEvent  }
    TCertVerMethod   = (CertVerNone, CertVerBundle, CertVerWinStore, CertVerOwnEvent);

   { V8.57 Logging debug level }
    THttpDebugLevel  = (DebugNone, DebugConn, DebugParams, DebugSsl, DebugHdr, DebugBody, DebugSslLow);

   { V8.40 options to read pkey and inters from cert PEM and P12 files,
     croTry will silently fail, croYes will fail with exception  }
    TCertReadOpt = (croNo, croTry, croYes);             { V8.39 }

   { V8.41 SSL/TLS certificate validation result, V8.57 added None }
    TChainResult = (chainOK, chainFail, chainWarn, chainNone);

   { V8.40 1.1.0 and later, sets OpenSSL security level to a number }
   { V8.65 NOTE OpenSSL 3.0.0 and later block SHA1, MD5, TLS1, TLS1.1 except for sslSecLevelAny }
    TSslSecLevel = (
                     sslSecLevelAny,        { 0 - anything allowed, old compatibility }
                     sslSecLevel80bits,     { 1 - default, RSA/DH keys=>1024, ECC=>160, no MD5 }
                     sslSecLevel112bits,    { 2 - RSA/DH keys=>2048, ECC=>224, no RC4, no SSL3, no SHA1 certs }
                     sslSecLevel128bits,    { 3 - RSA/DH keys=>3072, ECC=>256, FS forced, no TLS/1.0  }
                     sslSecLevel192bits,    { 4 - RSA/DH keys=>7680, ECC=>384, no SHA1 suites, no TLS/1.1  }
                     sslSecLevel256bits);   { 5 - RSA/DH keys=>15360, ECC=>512  }

   { V8.45 SSL server security level, used by TIcsHost, sets protocol, cipher and SslSecLevel }
   { warning, requiring key lengths higher than 2048 requires all SSL certificates in the chain to
     have that minimum key length, including the root }
   { V8.55 sslSrvSecInter/FS, sslCliSecInter now requires TLS1.1, PCI council EOF TLS1.0 30 June 2018 }
   { V8.60 added sslSrvSecTls12Less and sslSrvSecTls13Only to disable TLS1.3 if it fails }
   { V8.66 Updated SslSrvSecurity levels: sslSrvSecInter, sslSrvSecInterFS and sslSrvSecHigh now all the
      same TLSv1.2 or 1.3, sslSrvSecTls12Less now TLSv1.2 only, sslSrvSecSsl3 not supported, only
      sslSrvSecBack supports TLSv1 and 1.1, sslSrvSecTls13Only unchanged TLSv1.3 only. }

    TSslSrvSecurity = (
                     sslSrvSecNone,         { 0 - all protocols and ciphers, any key lengths }
                     sslSrvSecSsl3,         { 1 - was SSL3 only, no longer supported }
                     sslSrvSecBack,         { 2 - TLS1 or later, backward ciphers, RSA/DH keys=>1024, ECC=>160, no MD5, SHA1 }
                     sslSrvSecInter,        { 3 - now same as sslSrvSecHigh }
                     sslSrvSecInterFS,      { 4 - now same as sslSrvSecHigh }
                     sslSrvSecHigh,         { 5 - TLS1.2 or later, high ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     sslSrvSecHigh128,      { 6 - TLS1.2 or later, high ciphers, RSA/DH keys=>3072, ECC=>256, FS forced }
                     sslSrvSecHigh192,      { 7 - TLS1.2 or later, high ciphers, RSA/DH keys=>7680, ECC=>384, FS forced }
                     sslSrvSecTls12Less,    { 8 - TLSv1.2 only, high ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     sslSrvSecTls13Only);   { 9 - TLSv1.3 only, high ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }

const
    sslSrvSecDefault = sslSrvSecHigh;       { V8.55 recommended default, V8.64 improved default since TLSv1.1 disabled in most browsers from early 2020 }

type
   { V8.54 SSL client security level, used by context, sets protocol, cipher and SslSecLevel }
    TSslCliSecurity = (
                     sslCliSecIgnore,       { 0 - ignore, use old settings }
                     sslCliSecNone,         { 1 - all protocols and ciphers, any key lengths }
                     sslCliSecSsl3Only,     { 2 - SSLv3 only, all ciphers, any key lengths, MD5 }
                     sslCliSecTls1Only,     { 3 - TLSv1 only, all ciphers, RSA/DH keys=>2048 }
                     sslCliSecTls11Only,    { 4 - TLSv1.1 only, all ciphers, RSA/DH keys=>2048 }
                     sslCliSecTls12Only,    { 5 - TLSv1.2 only, all ciphers, RSA/DH keys=>2048 }
                     sslCliSecTls13Only,    { 6 - TLSv1.3 only, all ciphers, RSA/DH keys=>2048 }
                     sslCliSecTls1,         { 7 - TLSv1 or later, all ciphers, RSA/DH keys=>1024 }
                     sslCliSecTls11,        { 8 - TLSv1.1 or later, all ciphers, RSA/DH keys=>1024 }
                     sslCliSecTls12,        { 9 - TLSv1.2 or later, all ciphers, RSA/DH keys=>2048 }
                     sslCliSecBack,         { 10 - TLSv1 or later, backward ciphers, RSA/DH keys=>1024, ECC=>160, no MD5, SHA1 }
                     sslCliSecInter,        { 11 - TLSv1.1 or later, intermediate ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     sslCliSecHigh,         { 12 - TLSv1.2 or later, high ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     sslCliSecHigh128,      { 13 - TLSv1.2 or later, high ciphers, RSA/DH keys=>3072, ECC=>256, FS forced }
                     sslCliSecHigh192);     { 14 - TLSv1.2 or later, high ciphers, RSA/DH keys=>7680, ECC=>384, FS forced }

const
    sslCliSecDefault = sslCliSecTls12;  { V8.55 recommended default, V8.69 changed to 1.2 or better }

  SslSrvSecurityNames: array [TSslSrvSecurity ] of PChar = (   { V8.55, V8.60 added TLS version and 1.2/1.3 only }
                     'None',                                      { 0 - all protocols and ciphers, any key lengths }
                     'SSLv3 Only',                                { 1 - SSL3 only, all ciphers, any key lengths, MD5 }
                     'Backward Ciphers, TLS1 or Later',           { 2 - TLS1 or later, backward ciphers, RSA/DH keys=>1024, ECC=>160, no MD5, SHA1 }
                     'Intermediate Ciphers, TLS1.1 or Later',     { 3 - TLS1.1 or later, intermediate ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     'Intermediate Ciphers FS, TLS1.1 or Later',  { 4 - TLS1.1 or later, intermediate FS ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     'High 112 bit Ciphers, TLS1.2 or Later',     { 5 - TLS1.2 or later, high ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     'High 128 bit Ciphers, TLS1.2 or Later',     { 6 - TLS1.2 or later, high ciphers, RSA/DH keys=>3072, ECC=>256, FS forced }
                     'High 192 bit Ciphers, TLS1.2 or Later',     { 7 - TLS1.2 or later, high ciphers, RSA/DH keys=>7680, ECC=>384, FS forced }
                     'TLSv1.2 or Earlier',                        { 8 - TLSv1.2 or earlier, intermediate FS ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     'TLSv1.3 Only');                             { 9 - TLSv1.3 only, intermediate FS ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }

 SslCliSecurityNames: array [TSslCliSecurity] of PChar = (   { V8.55 }
                     'Ignore',         { 0 - ignore, use old settings }
                     'None',           { 1 - all protocols and ciphers, any key lengths }
                     'SSLv3 Only',     { 2 - SSLv3 only, all ciphers, any key lengths, MD5 }
                     'TLSv1 Only',     { 3 - TLSv1 only, all ciphers, RSA/DH keys=>2048 }
                     'TLSv1.1 Only',   { 4 - TLSv1.1 only, all ciphers, RSA/DH keys=>2048 }
                     'TLSv1.2 Only',   { 5 - TLSv1.2 only, all ciphers, RSA/DH keys=>2048 }
                     'TLSv1.3 Only',   { 6 - TLSv1.3 only, all ciphers, RSA/DH keys=>2048 }
                     'TLSv1 or Better',       { 7 - TLSv1 or later, all ciphers, RSA/DH keys=>1024 }
                     'TLSv1.1 or Better',     { 8 - TLSv1.1 or later, all ciphers, RSA/DH keys=>1024 }
                     'TLSv1.2 or Better',     { 9 - TLSv1.2 or later, all ciphers, RSA/DH keys=>2048 }
                     'Backward Ciphers',      { 10 - TLSv1 or later, backward ciphers, RSA/DH keys=>1024, ECC=>160, no MD5, SHA1 }
                     'Intermediate Ciphers',  { 11 - TLSv1.1 or later, intermediate ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     'High Ciphers, 2048 keys',  { 12 - TLSv1.2 or later, high ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     'High Ciphers, 3072 keys',  { 13 - TLSv1.2 or later, high ciphers, RSA/DH keys=>3072, ECC=>256, FS forced }
                     'High Ciphers, 7680 keys'); { 14 - TLSv1.2 or later, high ciphers, RSA/DH keys=>7680, ECC=>384, FS forced }

   { V9.3 literals for TCertVerMethod  }
  CertVerMethodNames: array [TCertVerMethod] of PChar = ('None', 'PEM Bundle File', 'Windows Cert Store', 'Custom Check');


{ V8.56 TLS Application-Layer Protocol Negotiation (ALPN) Protocol IDs }
{ received from one client: h2,h2-14,h2-15,h2-16,h2-17,spdy/1,spdy/2,spdy/3,spdy/3.1,spdy/4,http/1.1,h2-fb,webrtc,c-webrtc,ftp }
{ another: http/0.9,http/1.0,http/1.1,spdy/1,spdy/2,spdy/3,h2,h2c,hq  }
{ another: hq,h2c,h2,spdy/3,spdy/2,spdy/1,http/1.1,http/1.0,http/0.9 }
{ most common: h2,http/1.1 }
    ALPN_ID_HTTP10        = 'http/1.0';
    ALPN_ID_HTTP11        = 'http/1.1';
    ALPN_ID_HTTP2         = 'h2';       { HTTP/2 over TLS }
    ALPN_ID_HTTP3         = 'h3';       { HTTP/3 }
    ALPN_ID_HTTP2S        = 'h2s';      { HTTP/2 over TCP }
    ALPN_ID_HTTP214       = 'h2-14';   { and -15, -16, -17 versions }
//    ALPN_ID_SPDY1         = 'spdy/1';
//    ALPN_ID_SPDY2         = 'spdy/2';
    ALPN_ID_SPDY3         = 'spdy/3';
    ALPN_ID_SPDY31        = 'spdy/3.1';
    ALPN_ID_TURN          = 'stun.turn';
    ALPN_ID_STUN          = 'stun.nat-discovery';
    ALPN_ID_WEBRTC        = 'webrtc';
    ALPN_ID_CWEBRTC       = 'c-webrtc';
    ALPN_ID_FTP           = 'ftp';
    ALPN_ID_IMAP          = 'imap';
    ALPN_ID_POP3          = 'pop3';
    ALPN_ID_ACME_TLS1     = 'acme-tls/1';
    ALPN_ID_DNS_TCP       = 'dot';
    ALPN_ID_MQTT          = 'mqtt';

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ following moved from OverbyteIcsLIBEAY }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

    X509_V_OK                                           = 0;
    // illegal error (for uninitialized values, to avoid X509_V_OK): 1
    X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT                = 2;
    X509_V_ERR_UNABLE_TO_GET_CRL                        = 3;
    X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE         = 4;
    X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE          = 5;
    X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY       = 6;
    X509_V_ERR_CERT_SIGNATURE_FAILURE                   = 7;
    X509_V_ERR_CRL_SIGNATURE_FAILURE                    = 8;
    X509_V_ERR_CERT_NOT_YET_VALID                       = 9;
    X509_V_ERR_CERT_HAS_EXPIRED                         = 10;
    X509_V_ERR_CRL_NOT_YET_VALID                        = 11;
    X509_V_ERR_CRL_HAS_EXPIRED                          = 12;
    X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD           = 13;
    X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD            = 14;
    X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD           = 15;
    X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD           = 16;
    X509_V_ERR_OUT_OF_MEM                               = 17;
    X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT              = 18;
    X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN                = 19;
    X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY        = 20;
    X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE          = 21;
    X509_V_ERR_CERT_CHAIN_TOO_LONG                      = 22;
    X509_V_ERR_CERT_REVOKED                             = 23;
    X509_V_ERR_INVALID_CA                               = 24;
    X509_V_ERR_PATH_LENGTH_EXCEEDED                     = 25;
    X509_V_ERR_INVALID_PURPOSE                          = 26;
    X509_V_ERR_CERT_UNTRUSTED                           = 27;
    X509_V_ERR_CERT_REJECTED                            = 28;
    // These are 'informational' when looking for issuer cert
    X509_V_ERR_SUBJECT_ISSUER_MISMATCH                  = 29;
    X509_V_ERR_AKID_SKID_MISMATCH                       = 30;
    X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH              = 31;
    X509_V_ERR_KEYUSAGE_NO_CERTSIGN                     = 32;

    X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER                 = 33;
    X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION             = 34;
    X509_V_ERR_KEYUSAGE_NO_CRL_SIGN                     = 35;
    X509_V_ERR_UNHANDLED_CRITICAL_CRL_EXTENSION         = 36;
    X509_V_ERR_INVALID_NON_CA                           = 37;
    X509_V_ERR_PROXY_PATH_LENGTH_EXCEEDED               = 38;
    X509_V_ERR_KEYUSAGE_NO_DIGITAL_SIGNATURE            = 39;
    X509_V_ERR_PROXY_CERTIFICATES_NOT_ALLOWED           = 40;

    X509_V_ERR_INVALID_EXTENSION                        = 41;
    X509_V_ERR_INVALID_POLICY_EXTENSION                 = 42;
    X509_V_ERR_NO_EXPLICIT_POLICY                       = 43;
{ V8.39 added v_err 44 to 72 }
    X509_V_ERR_DIFFERENT_CRL_SCOPE                 = 44;
    X509_V_ERR_UNSUPPORTED_EXTENSION_FEATURE       = 45;
    X509_V_ERR_UNNESTED_RESOURCE                   = 46;
    X509_V_ERR_PERMITTED_VIOLATION                 = 47;
    X509_V_ERR_EXCLUDED_VIOLATION                  = 48;
    X509_V_ERR_SUBTREE_MINMAX                      = 49;
    // The application is not happy
    X509_V_ERR_APPLICATION_VERIFICATION            = 50;
    X509_V_ERR_UNSUPPORTED_CONSTRAINT_TYPE         = 51;
    X509_V_ERR_UNSUPPORTED_CONSTRAINT_SYNTAX       = 52;
    X509_V_ERR_UNSUPPORTED_NAME_SYNTAX             = 53;
    X509_V_ERR_CRL_PATH_VALIDATION_ERROR           = 54;
    // Another issuer check debug option
    X509_V_ERR_PATH_LOOP                           = 55;
    // Suite B mode algorithm violation
    X509_V_ERR_SUITE_B_INVALID_VERSION             = 56;
    X509_V_ERR_SUITE_B_INVALID_ALGORITHM           = 57;
    X509_V_ERR_SUITE_B_INVALID_CURVE               = 58;
    X509_V_ERR_SUITE_B_INVALID_SIGNATURE_ALGORITHM = 59;
    X509_V_ERR_SUITE_B_LOS_NOT_ALLOWED             = 60;
    X509_V_ERR_SUITE_B_CANNOT_SIGN_P_384_WITH_P_256 = 61;
    // Host, email and IP check errors
    X509_V_ERR_HOSTNAME_MISMATCH                   = 62;
    X509_V_ERR_EMAIL_MISMATCH                      = 63;
    X509_V_ERR_IP_ADDRESS_MISMATCH                 = 64;
    // DANE TLSA errors
    X509_V_ERR_DANE_NO_MATCH                       = 65;
    // security level errors
    X509_V_ERR_EE_KEY_TOO_SMALL                    = 66;
    X509_V_ERR_CA_KEY_TOO_SMALL                    = 67;
    X509_V_ERR_CA_MD_TOO_WEAK                      = 68;
    // Caller error
    X509_V_ERR_INVALID_CALL                        = 69;
    // Issuer lookup error
    X509_V_ERR_STORE_LOOKUP                        = 70;
    // Certificate transparency
    X509_V_ERR_NO_VALID_SCTS                       = 71;
    X509_V_ERR_PROXY_SUBJECT_NAME_VIOLATION        = 72;

{ V8.69 OCSP constants to check certificates }
    OCSP_REVOKED_STATUS_NOSTATUS                = -1;
    OCSP_REVOKED_STATUS_UNSPECIFIED             = 0;
    OCSP_REVOKED_STATUS_KEYCOMPROMISE           = 1;
    OCSP_REVOKED_STATUS_CACOMPROMISE            = 2;
    OCSP_REVOKED_STATUS_AFFILIATIONCHANGED      = 3;
    OCSP_REVOKED_STATUS_SUPERSEDED              = 4;
    OCSP_REVOKED_STATUS_CESSATIONOFOPERATION    = 5;
    OCSP_REVOKED_STATUS_CERTIFICATEHOLD         = 6;
    OCSP_REVOKED_STATUS_REMOVEFROMCRL           = 8;

    OCSP_DEFAULT_NONCE_LENGTH   = 16;

    OCSP_NOCERTS        = $1;
    OCSP_NOINTERN       = $2;
    OCSP_NOSIGS         = $4;
    OCSP_NOCHAIN        = $8;
    OCSP_NOVERIFY       = $10;
    OCSP_NOEXPLICIT     = $20;
    OCSP_NOCASIGN       = $40;
    OCSP_NODELEGATED    = $80;
    OCSP_NOCHECKS       = $100;
    OCSP_TRUSTOTHER     = $200;
    OCSP_RESPID_KEY     = $400;
    OCSP_NOTIME         = $800;
    OCSP_PARTIAL_CHAIN  = $1000;

    OCSP_RESPONSE_STATUS_SUCCESSFUL         = 0;
    OCSP_RESPONSE_STATUS_MALFORMEDREQUEST   = 1;
    OCSP_RESPONSE_STATUS_INTERNALERROR      = 2;
    OCSP_RESPONSE_STATUS_TRYLATER           = 3;
    OCSP_RESPONSE_STATUS_SIGREQUIRED        = 5;
    OCSP_RESPONSE_STATUS_UNAUTHORIZED       = 6;

    V_OCSP_RESPID_NAME  = 0;
    V_OCSP_RESPID_KEY   = 1;

    V_OCSP_CERTSTATUS_GOOD      = 0;
    V_OCSP_CERTSTATUS_REVOKED   = 1;
    V_OCSP_CERTSTATUS_UNKNOWN   = 2;

    PEM_STRING_OCSP_REQUEST     = 'OCSP REQUEST';
    PEM_STRING_OCSP_RESPONSE    = 'OCSP RESPONSE';

{ taken from OverbyteIcsWinCrypt (OverbyteIcsJwaWinCrypt insert), but renamed }
const
  Ics_CERT_TRUST_NO_ERROR                  = $00000000;
  Ics_CERT_TRUST_IS_NOT_TIME_VALID         = $00000001;
  Ics_CERT_TRUST_IS_NOT_TIME_NESTED        = $00000002;
  Ics_CERT_TRUST_IS_REVOKED                = $00000004;
  Ics_CERT_TRUST_IS_NOT_SIGNATURE_VALID    = $00000008;
  Ics_CERT_TRUST_IS_NOT_VALID_FOR_USAGE    = $00000010;
  Ics_CERT_TRUST_IS_UNTRUSTED_ROOT         = $00000020;
  Ics_CERT_TRUST_REVOCATION_STATUS_UNKNOWN = $00000040;
  Ics_CERT_TRUST_IS_CYCLIC                 = $00000080;
  Ics_CERT_TRUST_IS_OFFLINE_REVOCATION     = $01000000;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ following moved from OverbyteIcsSslX509Utils }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
 { digest lists are used in PemTool to display menu of common digest types }
   DigestDispList: array [0..8] of TEvpDigest =
        (Digest_sha1, Digest_sha224, Digest_sha256, Digest_sha384, Digest_sha512,
         Digest_sha3_224, Digest_sha3_256, Digest_sha3_384, Digest_sha3_512);

    DigestListLitsLast = 8;
    DigestListLits: array [0..DigestListLitsLast] of PChar = (   { V8.62 }
        'SHA1 (old)', 'SHA224', 'SHA256', 'SHA384', 'SHA512',
        'SHA3_224', 'SHA3_256', 'SHA3_384', 'SHA3_512');

    EvpDigestLits: array [TEvpDigest] of PChar = (   { V9.4 lits that match TEvpDigest }
        'MD5', 'MDC2', 'SHA1 (old)',
        'SHA224', 'SHA256', 'SHA384', 'SHA512',
        'RIPEMD_160',
        'SHA3_224', 'SHA3_256', 'SHA3_384', 'SHA3_512',
        'SHAKE_128', 'SHAKE_256', 'None');

  { not showing RSA PSS yet }
    SslPrivKeyTypeLitsLast1 = 9;
    SslPrivKeyTypeLitsLast2 = 15;
    SslPrivKeyTypeLits: array [0..SslPrivKeyTypeLitsLast2] of PChar = (   { V8.62 }
        'RSA 1,024 bits (level 1 - 80 bits)',
        'RSA 2,048 bits (level 2 - 112 bits)',
        'RSA 3,072 bits (level 3 - 128 bits, NIST min)',
        'RSA 4,096 bits (level 3 - 128 bits)',
        'RSA 7,680 bits (level 4 - 192 bits)',
        'RSA 15,360 bits (level 5 - 256 bits)',
        'Elliptic Curve secp256  (level 3 - 128 bits)',
        'Elliptic Curve secp384  (level 4 - 192 bits)',
        'Elliptic Curve secp512  (level 5 - 256 bits)',
        'EdDSA ED25519 (level 3 - 128 bits)',
        'RSA-PSS 2,048 bits (level 2 - 112 bits)',
        'RSA-PSS 3,072 bits (level 3 - 128 bits)',
        'RSA-PSS 4,096 bits (level 3 - 128 bits)',
        'RSA-PSS 7,680 bits (level 4 - 192 bits)',
        'RSA-PSS 15,360 bits (level 5 - 256 bits)',
        'Elliptic Curve secp256k (level 3 - 128 bits)');

    SslPrivKeyCipherLits: array[TSslPrivKeyCipher] of PChar = (   { V8.62 }
        'None', 'Triple DES', 'IDEA', 'AES128', 'AES192', 'AES256'{, 'Blowfish'});   { V8.67 Blowfish gone }

    SslCertFileOpenExts = 'Certs *.pem;*.cer;*.crt;*.der;*.p12;*.pfx;*.p7*;*.spc|' +
                 '*.pem;*.cer;*.crt;*.der;*.p12;*.pfx;*.p7*;*.spc|All Files *.*|*.*';    { V8.62 }

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ following moved from OverbyteIcsUtils }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.52 some useful string constants, make sure names are unique }
const
  IcsNULL            =  #0;
  IcsSTX             =  #2;
  IcsETX             =  #3;
  IcsEOT             =  #4;
  IcsBACKSPACE       =  #8;
  IcsTAB             =  #9;
  IcsLF              = #10;
  IcsFF              = #12;
  IcsCR              = #13;
  IcsEOF             = #26;
  IcsESC             = #27;
  IcsFIELDSEP        = #28;
  IcsRECSEP          = #30;
  IcsBLANK           = #32;
  IcsSQUOTE          = #39 ;
  IcsDQUOTE          = #34 ;
  IcsSPACE           = #32;
  IcsHEX_PREFIX      = '$';     { prefix for hexnumbers }
  IcsCRLF            = #13#10;
  IcsDoubleCRLF      = #13#10#13#10;
  IcsCOLON           = ':';     { V8.65 }
  IcsCOMMA           = ',';     { V8.65 }
  IcsCURLYO          = '{';     { V8.65 }
  IcsCURLYC          = '}';     { V8.65 }
  IcsAmpersand       = '&';     { V8.65 }

{ V8.70 a few byte constants, for use with TBytes }
  IcsbNULL            =  0;
  IcsbSTX             =  2;
  IcsbETX             =  3;
  IcsbEOT             =  4;
  IcsbBACKSPACE       =  8;
  IcsbTAB             =  9;
  IcsbLF              = 10;
  IcsbFF              = 12;
  IcsbCR              = 13;
  IcsbEOF             = 26;
  IcsbESC             = 27;
  IcsbFIELDSEP        = 28;
  IcsbRECSEP          = 30;
  IcsbBLANK           = 32;
  IcsbSQUOTE          = 39 ;
  IcsbDQUOTE          = 34 ;
  IcsbSPACE           = 32;

  { V8.54 Tick and Trigger constants }
  TicksPerDay      : longword =  24 * 60 * 60 * 1000 ;
  TicksPerHour     : longword = 60 * 60 * 1000 ;
  TicksPerMinute   : longword = 60 * 1000 ;
  TicksPerSecond   : longword = 1000 ;
  TriggerDisabled  : longword = $FFFFFFFF ;
  TriggerImmediate : longword = 0 ;
  OneSecondDT: TDateTime = 1 / SecsPerDay ;         { V8.60 }
  OneMinuteDT: TDateTime = 1 / (SecsPerDay / 60) ;  { V8.60 }
  MinutesPerDay      = 60.0 * 24.0;                 { V8.62 }

  { V8.60 date and time masks }
  ISOTimeMask = 'hh:nn:ss' ;
  ISOLongTimeMask = 'hh:nn:ss:zzz' ;
  ISODateMask = 'yyyy-mm-dd' ;
  ISODateTimeMask = 'yyyy-mm-dd"T"hh:nn:ss' ;
  ISODateLongTimeMask = 'yyyy-mm-dd"T"hh:nn:ss.zzz' ;
  DateAlphaMask = 'dd-mmm-yyyy' ;                    { V9.4 }
  SDateMaskPacked = 'yymmddhhnnss' ;                 { V9.4 }
  DateTimeAlphaMask = 'dd-mmm-yyyy hh:nn:ss' ;       { V9.4 }
  DateMmmMask = 'dd mmm yyyy' ;                      { V9.4 }

  { V8.64 International Domain Name support }
  ACE_PREFIX = 'xn--';

 { V8.67 common computer sizes }
  IcsKBYTE = Sizeof(Byte) shl 10;
  IcsMBYTE = IcsKBYTE shl 10;
  IcsGBYTE = IcsMBYTE shl 10;

  { V8.70 file system extended path names, if file system supports them, unicode APIs only }
  sPathExtended = '\\?\';         { \\?\d:\filepath }
  sPathExtendedUNC = '\\?\UNC\';  { \\?\UNC\server\share\filepath }
  IcsMaxPath = 260;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


implementation


end.
