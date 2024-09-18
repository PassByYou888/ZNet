{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Internet Protocol Helper Component for Windows.
Creation:     2000
Updated:      Aug 2023
Version:      V9.0
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2002-2023 by Angus Robertson, Magenta Systems Ltd,
              Croydon, England. delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
              Based on work by Dirk Claessens

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

------------------------------------------------------------------------------
     Partial translation of  IPHLPAPI.DLL ( IP-Helper API )
 http://users.pandora.be/dirk.claessens2/
     D. Claessens
------------------------------------------------------------------------------

v1.3 - 18th September 2001
  Angus Robertson, Magenta Systems Ltd, England
     delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
  All functions are dynamically loaded so program can be used on W95/NT4
  Added GetFriendlyIfIndex

v1.4 - 28th February 2002 - Angus
  Minor change to TIP_ADAPTER_INFO

v 1.5 - 26 July 2002 - Angus
  Added GetPerAdapterInfo and TIP_PER_ADAPTER_INFO

v 1.6 - 19 August 2002 - Angus
  Added AllocateAndGetTcpExTableFromStack and AllocateAndGetUdpExTableFromStack,
  which are APIs for XP only (not Vista), info from Netstatp at www.sysinternals.com
  Added MIB_TCP_STATE constants

v1.8 - 25th October 2005 - Angus

v1.9 - 8th August 2006 - Angus
   Corrected IF_xx_ADAPTER type literals, thanks to Jean-Pierre Turchi

v2.0 - 25th February 2007 - Angus
   Many more IF_xx_ADAPTER type literals, thanks to Jean-Pierre Turchi

v2.1 - 5th August 2008 - Angus
    Updated to be compatible with Delphi 2009
    Note there are only ANSI versions of the IP Helper APIs, no Wide/Unicode versions

v2.2 - 16th January 2009 - Angus
    Added GetAdaptersAddresses (XP and later) has IPv6 addresses (but IPv6 structures not done yet)
    Added GetExtendedTcpTable and GetExtendedUdpTable (XP SP2, W2K3 SP1, Vista and later),
      replacements for AllocateAndGetTcpExTableFromStack/etc

v2.3 - 3rd August 2009
    Changed ULONGLONG to LONGLONG for Delphi 7 compatability

v2.4 - 8th August 2010
    Fixed various cast warning for Delphi 2009 and later

v2.5 - 12th August 2011
    Removed packed for 64-bit compatibility in Delphi XE2 and later

v3.0 -  26th November 2018
    Only supporting XP SP3 and later, so removed AllocateAndGetTcpExTableFromStack/etc
   Added IPv6 support, numerous new structures and functions, Vista and later
   Added notification functions for interface changes
   Major clean up of Microsoft caps names with underscores to Delphi Txxxx type formats

xx - Added GetBestRoute.

v2.1 - 5th August 2008 - Angus
    Updated to be compatible with Delphi 2009

v2.2 - 16th January 2009 - Angus
    Added GetAdaptersAddresses (XP and later) has IPv6 addresses (but not yet getting them)
      Note: gateway IPs don't seem to be returned by GetAdaptersAddresses
    Added GetExtendedTcpTable and GetExtendedUdpTable (XP SP2, W2K3 SP1, Vista and later),
      replacements for AllocateAndGetTcpExTableFromStack/etc, added connection start time
    Using WideString for program paths and adaptor descriptions for Unicode compatibility
    Added two public variables:
     ShowExePath if true displays full program path for connection tables
     UseAdressesAPI if true uses GetAdaptersAddresses instead of GetAdaptersInfo

v2.3 - 3rd August 2009
    Changed ULONGLONG to LONGLONG for Delphi 7 compatability

v2.4 - 8th August 2010
    Fixed various cast warning for Delphi 2009 and later

v2.5 - 12th August 2011
    Tested with 32-bit and 64-bit in Delphi XE2

v3.0 - 26th November 2018
   Only supporting XP SP3 and later, so remove code for earlier OSs
   Added IPv6 support, numerous new structures and functions, Vista and later
   Still runs on XP SP3, but TCP and UDP connection lists not supported and
     some other functions return limited info, IP addresses in particular
   Added notification functions for interface changes, Vista and later
   UseAdressesAPI removed
   corrected MacAddr2Str so it does not skip first byte

Aug 16, 2023 - V8.71 Updated units for main ICS library.
               IpHlpAdaptersAddr now works on Win64, thanks to David Ulbrich.
               Using TSockAddrIn6 instead of TSockAddrInet to allow use of WSocket functions.
               Removed XP support, now Vista and Windows 7 and later.  Missing functions
                 should fail with an error, rather than an exception.
               Removed RtlIpv4/6xx exports since using WSocket IP address conversion functions.
               Added IpHlpIpNeighbTable and Get_IPNeighbourTable, similar to ARP but includes IPv6.
               Most MAC addresses now also show vendor name.
               TIpAddrInfo now has the MAC address and vendor.
               Added IpHlpConnsTable and Get_IpConnsTable for TCP and/or UDP connections.
               Added TIcsNeighbDevices component to build historic LAN neighbourhood MAC device
                 and IP address table from IpHlpIpNeighbTable, includes ARP IP range scanning
                 and reverse host lookup, shows MAC vendor name to help identify devices.
                 Runs in a thread continually checking for new devices.  Optionally uses
                 TIcsDomainNameCache component to reverse lookup names.
               Added TIcsIpChanges component that replaces the IpChangesStart/Stop functions
                 to monitor IP address changes dynamically.
               Added IpHlpGetDnsServers to get list of DNS servers for this PC.
Aug 08, 2023 V9.0  Updated version to major release 9.


Pending - TIcsNeighbDevices use multiple threads to avoid timeouts during ARP lookups.

}

unit Z.ICS9.OverbyteIcsIpHlpApi;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}
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
{$R-}             { must turn off range checking or arrays [0..ANY_SIZE] die !!!!! }
{$Q-}
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    Z.ICS9.OverbyteIcsWndControl,
    Z.ICS9.OverbyteIcsWinsock,
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsIpUtils,
    Z.ICS9.OverbyteIcsUtils,
    Z.ICS9.OverbyteIcsDnsQuery,
    Z.ICS9.OverbyteIcsTicks64;

const
    CopyRight    : String     = ' IcsIpHlpApi  (c) 2023 V9.0 ';


// following headers are for low level IpHelper Windows APIs
// later, are the headers for Delphi functions

//------------- headers from Microsoft IPTYPES.H--------------------------------

const
  ANY_SIZE = 1;
  TCPIP_OWNING_MODULE_SIZE = 16;
  MAX_ADAPTER_DESCRIPTION_LENGTH = 128; // arb.
  MAX_ADAPTER_NAME_LENGTH = 256; // arb.
  MAX_ADAPTER_ADDRESS_LENGTH = 8; // arb.
  DEFAULT_MINIMUM_ENTITIES = 32; // arb.
  MAX_HOSTNAME_LEN = 128; // arb.
  MAX_DOMAIN_NAME_LEN = 128; // arb.
  MAX_SCOPE_ID_LEN = 256; // arb.
  MAX_DHCPV6_DUID_LENGTH = 130; // RFC 3315
  NULL_IP = '  0.  0.  0.  0';
  MAX_INTERFACE_NAME_LEN = 256; { mrapi.h }
  MAXLEN_PHYSADDR = 8; { iprtrmib.h }
  MAXLEN_IFDESCR = 256; { --"---     }

 // Node Types ( NETBIOS)
  BROADCAST_NODETYPE = 1;
  PEER_TO_PEER_NODETYPE = 2;
  MIXED_NODETYPE = 4;
  HYBRID_NODETYPE = 8;

  NETBIOSTypes  : array[0..8] of string =
    ( 'UNKNOWN', 'BROADCAST', 'PEER_TO_PEER', '', 'MIXED', '', '', '', 'HYBRID'
      );

// Adapter Types
{  IF_OTHER_ADAPTERTYPE = 1;    // 8 August 2006 corrected literals to
MIB_IF_TYPE_xx in ipifcons.h
  IF_ETHERNET_ADAPTERTYPE = 6;
  IF_TOKEN_RING_ADAPTERTYPE = 9;
  IF_FDDI_ADAPTERTYPE = 15;
  IF_PPP_ADAPTERTYPE = 23;
  IF_LOOPBACK_ADAPTERTYPE = 24;
  IF_SLIP_ADAPTERTYPE = 28;   }

//  Adapted from Ipifcons.h : // JP Turchi, 9 Feb 2007

  //MIN_IF_TYPE                     = 1;

  IF_TYPE_OTHER                   = 1;   // None of the below
  IF_TYPE_REGULAR_1822            = 2;
  IF_TYPE_HDH_1822                = 3;
  IF_TYPE_DDN_X25                 = 4;
  IF_TYPE_RFC877_X25              = 5;
  IF_TYPE_ETHERNET_CSMACD         = 6;
  IF_TYPE_IS088023_CSMACD         = 7;
  IF_TYPE_ISO88024_TOKENBUS       = 8;
  IF_TYPE_ISO88025_TOKENRING      = 9;
  IF_TYPE_ISO88026_MAN            = 10;
  IF_TYPE_STARLAN                 = 11;
  IF_TYPE_PROTEON_10MBIT          = 12;
  IF_TYPE_PROTEON_80MBIT          = 13;
  IF_TYPE_HYPERCHANNEL            = 14;
  IF_TYPE_FDDI                    = 15;
  IF_TYPE_LAP_B                   = 16;
  IF_TYPE_SDLC                    = 17;
  IF_TYPE_DS1                     = 18;  // DS1-MIB
  IF_TYPE_E1                      = 19;  // Obsolete; see DS1-MIB
  IF_TYPE_BASIC_ISDN              = 20;
  IF_TYPE_PRIMARY_ISDN            = 21;
  IF_TYPE_PROP_POINT2POINT_SERIAL = 22;  // proprietary serial
  IF_TYPE_PPP                     = 23;
  IF_TYPE_SOFTWARE_LOOPBACK       = 24;
  IF_TYPE_EON                     = 25;  // CLNP over IP
  IF_TYPE_ETHERNET_3MBIT          = 26;
  IF_TYPE_NSIP                    = 27;  // XNS over IP
  IF_TYPE_SLIP                    = 28;  // Generic Slip
  IF_TYPE_ULTRA                   = 29;  // ULTRA Technologies
  IF_TYPE_DS3                     = 30;  // DS3-MIB
  IF_TYPE_SIP                     = 31;  // SMDS, coffee
  IF_TYPE_FRAMERELAY              = 32;  // DTE only
  IF_TYPE_RS232                   = 33;
  IF_TYPE_PARA                    = 34;  // Parallel port
  IF_TYPE_ARCNET                  = 35;
  IF_TYPE_ARCNET_PLUS             = 36;
  IF_TYPE_ATM                     = 37;  // ATM cells
  IF_TYPE_MIO_X25                 = 38;
  IF_TYPE_SONET                   = 39;  // SONET or SDH
  IF_TYPE_X25_PLE                 = 40;
  IF_TYPE_ISO88022_LLC            = 41;
  IF_TYPE_LOCALTALK               = 42;
  IF_TYPE_SMDS_DXI                = 43;
  IF_TYPE_FRAMERELAY_SERVICE      = 44;  // FRNETSERV-MIB
  IF_TYPE_V35                     = 45;
  IF_TYPE_HSSI                    = 46;
  IF_TYPE_HIPPI                   = 47;
  IF_TYPE_MODEM                   = 48;  // Generic Modem
  IF_TYPE_AAL5                    = 49;  // AAL5 over ATM
  IF_TYPE_SONET_PATH              = 50;
  IF_TYPE_SONET_VT                = 51;
  IF_TYPE_SMDS_ICIP               = 52;  // SMDS InterCarrier Interface
  IF_TYPE_PROP_VIRTUAL            = 53;  // Proprietary virtual/internal
  IF_TYPE_PROP_MULTIPLEXOR        = 54;  // Proprietary multiplexing
  IF_TYPE_IEEE80212               = 55;  // 100BaseVG
  IF_TYPE_FIBRECHANNEL            = 56;
  IF_TYPE_HIPPIINTERFACE          = 57;
  IF_TYPE_FRAMERELAY_INTERCONNECT = 58;  // Obsolete, use 32 or 44
  IF_TYPE_AFLANE_8023             = 59;  // ATM Emulated LAN for 802.3
  IF_TYPE_AFLANE_8025             = 60;  // ATM Emulated LAN for 802.5
  IF_TYPE_CCTEMUL                 = 61;  // ATM Emulated circuit
  IF_TYPE_FASTETHER               = 62;  // Fast Ethernet (100BaseT)
  IF_TYPE_ISDN                    = 63;  // ISDN and X.25
  IF_TYPE_V11                     = 64;  // CCITT V.11/X.21
  IF_TYPE_V36                     = 65;  // CCITT V.36
  IF_TYPE_G703_64K                = 66;  // CCITT G703 at 64Kbps
  IF_TYPE_G703_2MB                = 67;  // Obsolete; see DS1-MIB
  IF_TYPE_QLLC                    = 68;  // SNA QLLC
  IF_TYPE_FASTETHER_FX            = 69;  // Fast Ethernet (100BaseFX)
  IF_TYPE_CHANNEL                 = 70;
  IF_TYPE_IEEE80211               = 71;  // Radio spread spectrum
  IF_TYPE_IBM370PARCHAN           = 72;  // IBM System 360/370 OEMI Channel
  IF_TYPE_ESCON                   = 73;  // IBM Enterprise Systems Connection
  IF_TYPE_DLSW                    = 74;  // Data Link Switching
  IF_TYPE_ISDN_S                  = 75;  // ISDN S/T interface
  IF_TYPE_ISDN_U                  = 76;  // ISDN U interface
  IF_TYPE_LAP_D                   = 77;  // Link Access Protocol D
  IF_TYPE_IPSWITCH                = 78;  // IP Switching Objects
  IF_TYPE_RSRB                    = 79;  // Remote Source Route Bridging
  IF_TYPE_ATM_LOGICAL             = 80;  // ATM Logical Port
  IF_TYPE_DS0                     = 81;  // Digital Signal Level 0
  IF_TYPE_DS0_BUNDLE              = 82;  // Group of ds0s on the same ds1
  IF_TYPE_BSC                     = 83;  // Bisynchronous Protocol
  IF_TYPE_ASYNC                   = 84;  // Asynchronous Protocol
  IF_TYPE_CNR                     = 85;  // Combat Net Radio
  IF_TYPE_ISO88025R_DTR           = 86;  // ISO 802.5r DTR
  IF_TYPE_EPLRS                   = 87;  // Ext Pos Loc Report Sys
  IF_TYPE_ARAP                    = 88;  // Appletalk Remote Access Protocol
  IF_TYPE_PROP_CNLS               = 89;  // Proprietary Connectionless Proto
  IF_TYPE_HOSTPAD                 = 90;  // CCITT-ITU X.29 PAD Protocol
  IF_TYPE_TERMPAD                 = 91;  // CCITT-ITU X.3 PAD Facility
  IF_TYPE_FRAMERELAY_MPI          = 92;  // Multiproto Interconnect over FR
  IF_TYPE_X213                    = 93;  // CCITT-ITU X213
  IF_TYPE_ADSL                    = 94;  // Asymmetric Digital Subscrbr Loop
  IF_TYPE_RADSL                   = 95;  // Rate-Adapt Digital Subscrbr Loop
  IF_TYPE_SDSL                    = 96;  // Symmetric Digital Subscriber Loop
  IF_TYPE_VDSL                    = 97;  // Very H-Speed Digital Subscrb Loop
  IF_TYPE_ISO88025_CRFPRINT       = 98;  // ISO 802.5 CRFP
  IF_TYPE_MYRINET                 = 99;  // Myricom Myrinet
  IF_TYPE_VOICE_EM                = 100; // Voice recEive and transMit
  IF_TYPE_VOICE_FXO               = 101; // Voice Foreign Exchange Office
  IF_TYPE_VOICE_FXS               = 102; // Voice Foreign Exchange Station
  IF_TYPE_VOICE_ENCAP             = 103; // Voice encapsulation
  IF_TYPE_VOICE_OVERIP            = 104; // Voice over IP encapsulation
  IF_TYPE_ATM_DXI                 = 105; // ATM DXI
  IF_TYPE_ATM_FUNI                = 106; // ATM FUNI
  IF_TYPE_ATM_IMA                 = 107; // ATM IMA
  IF_TYPE_PPPMULTILINKBUNDLE      = 108; // PPP Multilink Bundle
  IF_TYPE_IPOVER_CDLC             = 109; // IBM ipOverCdlc
  IF_TYPE_IPOVER_CLAW             = 110; // IBM Common Link Access to Workstn
  IF_TYPE_STACKTOSTACK            = 111; // IBM stackToStack
  IF_TYPE_VIRTUALIPADDRESS        = 112; // IBM VIPA
  IF_TYPE_MPC                     = 113; // IBM multi-proto channel support
  IF_TYPE_IPOVER_ATM              = 114; // IBM ipOverAtm
  IF_TYPE_ISO88025_FIBER          = 115; // ISO 802.5j Fiber Token Ring
  IF_TYPE_TDLC                    = 116; // IBM twinaxial data link control
  IF_TYPE_GIGABITETHERNET         = 117;
  IF_TYPE_HDLC                    = 118;
  IF_TYPE_LAP_F                   = 119;
  IF_TYPE_V37                     = 120;
  IF_TYPE_X25_MLP                 = 121; // Multi-Link Protocol
  IF_TYPE_X25_HUNTGROUP           = 122; // X.25 Hunt Group
  IF_TYPE_TRANSPHDLC              = 123;
  IF_TYPE_INTERLEAVE              = 124; // Interleave channel
  IF_TYPE_FAST                    = 125; // Fast channel
  IF_TYPE_IP                      = 126; // IP (for APPN HPR in IP networks)
  IF_TYPE_DOCSCABLE_MACLAYER      = 127; // CATV Mac Layer
  IF_TYPE_DOCSCABLE_DOWNSTREAM    = 128; // CATV Downstream interface
  IF_TYPE_DOCSCABLE_UPSTREAM      = 129; // CATV Upstream interface
  IF_TYPE_A12MPPSWITCH            = 130; // Avalon Parallel Processor
  IF_TYPE_TUNNEL                  = 131; // Encapsulation interface
  IF_TYPE_COFFEE                  = 132; // Coffee pot
  IF_TYPE_CES                     = 133; // Circuit Emulation Service
  IF_TYPE_ATM_SUBINTERFACE        = 134; // ATM Sub Interface
  IF_TYPE_L2_VLAN                 = 135; // Layer 2 Virtual LAN using 802.1Q
  IF_TYPE_L3_IPVLAN               = 136; // Layer 3 Virtual LAN using IP
  IF_TYPE_L3_IPXVLAN              = 137; // Layer 3 Virtual LAN using IPX
  IF_TYPE_DIGITALPOWERLINE        = 138; // IP over Power Lines
  IF_TYPE_MEDIAMAILOVERIP         = 139; // Multimedia Mail over IP
  IF_TYPE_DTM                     = 140; // Dynamic syncronous Transfer Mode
  IF_TYPE_DCN                     = 141; // Data Communications Network
  IF_TYPE_IPFORWARD               = 142; // IP Forwarding Interface
  IF_TYPE_MSDSL                   = 143; // Multi-rate Symmetric DSL
  IF_TYPE_IEEE1394                = 144; // IEEE1394 High Perf Serial Bus
  IF_TYPE_IF_GSN = 145;           // following added Oct 2014
  IF_TYPE_DVBRCC_MACLAYER = 146;
  IF_TYPE_DVBRCC_DOWNSTREAM = 147;
  IF_TYPE_DVBRCC_UPSTREAM = 148;
  IF_TYPE_ATM_VIRTUAL = 149;
  IF_TYPE_MPLS_TUNNEL = 150;
  IF_TYPE_SRP = 151;
  IF_TYPE_VOICEOVERATM = 152;
  IF_TYPE_VOICEOVERFRAMERELAY = 153;
  IF_TYPE_IDSL = 154;
  IF_TYPE_COMPOSITELINK = 155;
  IF_TYPE_SS7_SIGLINK = 156;
  IF_TYPE_PROP_WIRELESS_P2P = 157;
  IF_TYPE_FR_FORWARD = 158;
  IF_TYPE_RFC1483 = 159;
  IF_TYPE_USB = 160;
  IF_TYPE_IEEE8023AD_LAG = 161;
  IF_TYPE_BGP_POLICY_ACCOUNTING = 162;
  IF_TYPE_FRF16_MFR_BUNDLE = 163;
  IF_TYPE_H323_GATEKEEPER = 164;
  IF_TYPE_H323_PROXY = 165;
  IF_TYPE_MPLS = 166;
  IF_TYPE_MF_SIGLINK = 167;
  IF_TYPE_HDSL2 = 168;
  IF_TYPE_SHDSL = 169;
  IF_TYPE_DS1_FDL = 170;
  IF_TYPE_POS = 171;
  IF_TYPE_DVB_ASI_IN = 172;
  IF_TYPE_DVB_ASI_OUT = 173;
  IF_TYPE_PLC = 174;
  IF_TYPE_NFAS = 175;
  IF_TYPE_TR008 = 176;
  IF_TYPE_GR303_RDT = 177;
  IF_TYPE_GR303_IDT = 178;
  IF_TYPE_ISUP = 179;
  IF_TYPE_PROP_DOCS_WIRELESS_MACLAYER = 180;
  IF_TYPE_PROP_DOCS_WIRELESS_DOWNSTREAM = 181;
  IF_TYPE_PROP_DOCS_WIRELESS_UPSTREAM = 182;
  IF_TYPE_HIPERLAN2 = 183;
  IF_TYPE_PROP_BWA_P2MP = 184;
  IF_TYPE_SONET_OVERHEAD_CHANNEL = 185;
  IF_TYPE_DIGITAL_WRAPPER_OVERHEAD_CHANNEL = 186;
  IF_TYPE_AAL2 = 187;
  IF_TYPE_RADIO_MAC = 188;
  IF_TYPE_ATM_RADIO = 189;
  IF_TYPE_IMT = 190;
  IF_TYPE_MVL = 191;
  IF_TYPE_REACH_DSL = 192;
  IF_TYPE_FR_DLCI_ENDPT = 193;
  IF_TYPE_ATM_VCI_ENDPT = 194;
  IF_TYPE_OPTICAL_CHANNEL = 195;
  IF_TYPE_OPTICAL_TRANSPORT = 196;
  IF_TYPE_IEEE80216_WMAN = 237;
  IF_TYPE_WWANPP = 243; // WWAN devices based on GSM technology
  IF_TYPE_WWANPP2 = 244;    // WWAN devices based on CDMA technology

  MAX_IF_TYPE                     = 244;

//-------------from other MS header files---------------------------------------

type
  TAddressFamily = Integer;
  TNetIfIndex = Integer;

// information for IPv6 stuff missing from winsock
  PScopeLevel = ^TScopeLevel;
  TScopeLevel = (
    ScopeLevelInterface    = 1,
    ScopeLevelLink         = 2,
    ScopeLevelSubnet       = 3,
    ScopeLevelAdmin        = 4,
    ScopeLevelSite         = 5,
    ScopeLevelOrganization = 8,
    ScopeLevelGlobal       = 14,
    ScopeLevelCount        = 16);

  PScopeID = ^TScopeID;
  TScopeID = record
      Value: ULONG; // Dummy actually a record with C bitfields
  end;

// Structure to hold a pair of source, destination addresses.
type
  _sockaddr_in6_pair = record
    SourceAddress: PSockAddrIn6;
    DestinationAddress: PSockAddrIn6;
  end;
  TSockAddrIn6Pair = _sockaddr_in6_pair;
  PSockAddrIn6Pair = ^_sockaddr_in6_pair;

type
  PSocketAddress = ^TSocketAddress;
  TSocketAddress = record
//    lpSockaddr: PSockAddrInet;       // both IPv4 and IPv6 versions, check family or length
    lpSockaddr: PSockAddrIn6;  { V8.71 }       // both IPv4 and IPv6 versions, check family or length
    iSockaddrLength: Integer;
  end;

const
{ Address families. - Oct 2014 }
  AF_UNSPEC       = 0;               { unspecified }
  AF_UNIX         = 1;               { local to host (pipes, portals) }
  AF_INET         = 2;               { internetwork: UDP, TCP, etc. aka IPv4 }
  AF_IMPLINK      = 3;               { arpanet imp addresses }
  AF_PUP          = 4;               { pup protocols: e.g. BSP }
  AF_CHAOS        = 5;               { mit CHAOS protocols }
  AF_NS           = 6;               { XEROX NS protocols }
  AF_IPX          = AF_NS;           { IPX and SPX }
  AF_ISO          = 7;               { ISO protocols }
  AF_OSI          = AF_ISO;          { OSI is ISO }
  AF_ECMA         = 8;               { european computer manufacturers }
  AF_DATAKIT      = 9;               { datakit protocols }
  AF_CCITT        = 10;              { CCITT protocols, X.25 etc }
  AF_SNA          = 11;              { IBM SNA }
  AF_DECnet       = 12;              { DECnet }
  AF_DLI          = 13;              { Direct data link interface }
  AF_LAT          = 14;              { LAT }
  AF_HYLINK       = 15;              { NSC Hyperchannel }
  AF_APPLETALK    = 16;              { AppleTalk }
  AF_NETBIOS      = 17;              { NetBios-style addresses }
  AF_VOICEVIEW    = 18;              { VoiceView }
  AF_FIREFOX      = 19;              { FireFox }
  AF_UNKNOWN1     = 20;              { Somebody is using this! }
  AF_BAN          = 21;              { Banyan }
  AF_INET6        = 23;              { Internetwork Version 6 aka IPv6 }
  AF_CLUSTER      = 24;              { Microsoft Wolfpack }
  AF_12844        = 25;              { IEEE 1284.4 WG AF }
  AF_IRDA         = 26;              { IrDA }
  AF_NETDES       = 28;              { Network Designers OSI & gateway enabled protocols }
  AF_TCNPROCESS   = 29;
  AF_TCNMESSAGE   = 30;
  AF_ICLFXBM      = 31;
  AF_MAX          = 32;

//------------------------------------------------------------------------------

type
  TPhysMacAddr = array[1..MAX_ADAPTER_ADDRESS_LENGTH] of byte;    // 8 bytes
  TInt24 = array[0..2] of Byte; // 3 bytes, need helper to convert to Int64

  PNetLuid = ^TNetLuid;
  TNetLuid = record               // May 2023
    case Integer of
      0: (Value: Int64);
      1: (Reserved:     TInt24;   // ULONG64 Reserved:24;
          NetLuidIndex: TInt24;   // ULONG64 NetLuidIndex:24;
          IfType:        Word);   // ULONG64 IfType:16;  equal to IANA IF type
  end;
 TIFLuid = TNetLuid;
 PIFLuid = PNetLuid;

type
  TIpPrefixOrigin = (
    IpPrefixOriginOther,
    IpPrefixOriginManual,
    IpPrefixOriginWellKnown,
    IpPrefixOriginDhcp,
    IpPrefixOriginRouterAdvertisement);

  TIpSuffixOrigin = (
    IpSuffixOriginOther,
    IpSuffixOriginManual,
    IpSuffixOriginWellKnown,
    IpSuffixOriginDhcp,
    IpSuffixOriginLinkLayerAddress,
    IpSuffixOriginRandom);

  TIpDadState= (
    IpDadStateInvalid,
    IpDadStateTentative,
    IpDadStateDuplicate,
    IpDadStateDeprecated,
    IpDadStatePreferred);

//------IP address structures---------------------------------------------------

  PIpAddressString = ^TIpAddressString;
  TIpAddressString = array[0..15] of AnsiChar; //  IP as string
  //
  PIpAddrString = ^TIpAddrString;
  TIpAddrString = record // for use in linked lists
    Next: PIpAddrString;
    IpAddress: TIpAddressString;
    IpMask: TIpAddressString;
    Context: DWORD;
  end;

//----------Fixed Info STRUCTURES---------------------------------------------

  PTFixedInfo = ^TFixedInfo;
  TFixedInfo = record
    HostName: array[1..MAX_HOSTNAME_LEN + 4] of AnsiChar;    // Angus
    DomainName: array[1..MAX_DOMAIN_NAME_LEN + 4] of AnsiChar;   // Angus
    CurrentDNSServer: PIpAddrString;
    DNSServerList: TIpAddrString;
    NodeType: UINT;
    ScopeID: array[1..MAX_SCOPE_ID_LEN + 4] of AnsiChar;   // Angus
    EnableRouting: UINT;
    EnableProxy: UINT;
    EnableDNS: UINT;
  end;

//----------INTERFACE STRUCTURES-------------------------------------------------

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// The following are the the operational states for WAN and LAN interfaces. //
// The order of the states seems weird, but is done for a purpose. All      //
// states >= CONNECTED can transmit data right away. States >= DISCONNECTED //
// can tx data but some set up might be needed. States < DISCONNECTED can   //
// not transmit data.                                                       //
// A card is marked UNREACHABLE if DIM calls InterfaceUnreachable for       //
// reasons other than failure to connect.                                   //
//                                                                          //
// NON_OPERATIONAL -- Valid for LAN Interfaces. Means the card is not       //
//                      working or not plugged in or has no address.        //
// UNREACHABLE     -- Valid for WAN Interfaces. Means the remote site is    //
//                      not reachable at this time.                         //
// DISCONNECTED    -- Valid for WAN Interfaces. Means the remote site is    //
//                      not connected at this time.                         //
// CONNECTING      -- Valid for WAN Interfaces. Means a connection attempt  //
//                      has been initiated to the remote site.              //
// CONNECTED       -- Valid for WAN Interfaces. Means the remote site is    //
//                      connected.                                          //
// OPERATIONAL     -- Valid for LAN Interfaces. Means the card is plugged   //
//                      in and working.                                     //
//                                                                          //
// It is the users duty to convert these values to MIB-II values if they    //
// are to be used by a subagent                                             //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

const
// Angus added from ipifcons.h
  IF_OPER_STATUS_NON_OPERATIONAL = 0 ;
  IF_OPER_STATUS_UNREACHABLE = 1 ;
  IF_OPER_STATUS_DISCONNECTED = 2 ;
  IF_OPER_STATUS_CONNECTING = 3 ;
  IF_OPER_STATUS_CONNECTED = 4 ;
  IF_OPER_STATUS_OPERATIONAL = 5 ;

  MIB_IF_TYPE_OTHER = 1 ;
  MIB_IF_TYPE_ETHERNET = 6 ;
  MIB_IF_TYPE_TOKENRING = 9 ;
  MIB_IF_TYPE_FDDI = 15 ;
  MIB_IF_TYPE_PPP = 23 ;
  MIB_IF_TYPE_LOOPBACK = 24 ;
  MIB_IF_TYPE_SLIP = 28 ;
  MIB_IF_TYPE_ATM = 37;
  MIB_IF_TYPE_IEEE80211 = 71;
  MIB_IF_TYPE_TUNNEL = 131;
  MIB_IF_TYPE_IEEE1394 = 144;
  MIB_IF_TYPE_IEEE80216_WMAN = 237;
  MIB_IF_TYPE_WWANPP = 243;
  MIB_IF_TYPE_WWANPP2 = 244;


  MIB_IF_OPER_STATUS_NON_OPERATIONAL = 0 ;
  MIB_IF_OPER_STATUS_UNREACHABLE = 1 ;
  MIB_IF_OPER_STATUS_DISCONNECTED = 2 ;
  MIB_IF_OPER_STATUS_CONNECTING = 3 ;
  MIB_IF_OPER_STATUS_CONNECTED = 4 ;
  MIB_IF_OPER_STATUS_OPERATIONAL = 5 ;

  MIB_TCP_STATE_CLOSED = 1 ;
  MIB_TCP_STATE_LISTEN = 2 ;
  MIB_TCP_STATE_SYN_SENT = 3 ;
  MIB_TCP_STATE_SYN_RCVD = 4 ;
  MIB_TCP_STATE_ESTAB = 5 ;
  MIB_TCP_STATE_FIN_WAIT1 = 6 ;
  MIB_TCP_STATE_FIN_WAIT2 = 7 ;
  MIB_TCP_STATE_CLOSE_WAIT = 8 ;
  MIB_TCP_STATE_CLOSING = 9 ;
  MIB_TCP_STATE_LAST_ACK = 10 ;
  MIB_TCP_STATE_TIME_WAIT = 11 ;
  MIB_TCP_STATE_DELETE_TCB = 12 ;

// wtype for MIB_IPADDRROW - Nov 2014
const
  MIB_IPADDR_PRIMARY = $0001;        // Primary ipaddr
  MIB_IPADDR_DYNAMIC = $0004;        // Dynamic ipaddr
  MIB_IPADDR_DISCONNECTED = $0008;   // Address is on disconnected interface
  MIB_IPADDR_DELETED = $0040;        // Address being deleted
  MIB_IPADDR_TRANSIENT = $0080;      // Transient address
  MIB_IPADDR_DNS_ELIGIBLE = $0100;   // Address is published in DNS.

// Bit values of IP_ADAPTER_UNICAST_ADDRESS Flags field.

const
  IP_ADAPTER_ADDRESS_DNS_ELIGIBLE   = $01;
  IP_ADAPTER_ADDRESS_TRANSIENT      = $02;

// Bit values of IP_ADAPTER_ADDRESSES Flags field.

const
  IP_ADAPTER_DDNS_ENABLED               = $00000001;
  IP_ADAPTER_REGISTER_ADAPTER_SUFFIX    = $00000002;
  IP_ADAPTER_DHCP_ENABLED               = $00000004;
  IP_ADAPTER_RECEIVE_ONLY               = $00000008;
  IP_ADAPTER_NO_MULTICAST               = $00000010;
  IP_ADAPTER_IPV6_OTHER_STATEFUL_CONFIG = $00000020;
  IP_ADAPTER_NETBIOS_OVER_TCPIP_ENABLED = $00000040;
  IP_ADAPTER_IPV4_ENABLED               = $00000080;
  IP_ADAPTER_IPV6_ENABLED               = $00000100;
  IP_ADAPTER_IPV6_MANAGE_ADDRESS_CONFIG = $00000200;

// Flags used as argument to GetAdaptersAddresses().
// "SKIP" flags are added when the default is to include the information.
// "INCLUDE" flags are added when the default is to skip the information.

const
  GAA_FLAG_SKIP_UNICAST = $0001;
  GAA_FLAG_SKIP_ANYCAST = $0002;
  GAA_FLAG_SKIP_MULTICAST = $0004;
  GAA_FLAG_SKIP_DNS_SERVER = $0008;
  GAA_FLAG_INCLUDE_PREFIX = $0010;
  GAA_FLAG_SKIP_FRIENDLY_NAME = $0020;
  GAA_FLAG_INCLUDE_WINS_INFO = $0040;
  GAA_FLAG_INCLUDE_GATEWAYS = $0080;
  GAA_FLAG_INCLUDE_ALL_INTERFACES = $0100;
  GAA_FLAG_INCLUDE_ALL_COMPARTMENTS = $0200;
  GAA_FLAG_INCLUDE_TUNNEL_BINDINGORDER = $0400;

type
// OperStatus for GetAdaptersAddresses().
  TIfOperStatus = (
    IF_OPER_STATUS_NONE,
    IF_OPER_STATUS_UP {= 1},
    IF_OPER_STATUS_DOWN {= 2},
    IF_OPER_STATUS_TESTING {= 3},
    IF_OPER_STATUS_UNKNOWN {= 4},
    IF_OPER_STATUS_DORMANT {= 5},
    IF_OPER_STATUS_NOT_PRESENT {= 6},
    IF_OPER_STATUS_LOWER_LAYER_DOWN {= 7 } );

  TAdminStatus = (
    IF_ADMIN_STATUS_None,
    IF_ADMIN_STATUS_UP, { = 1 }
    IF_ADMIN_STATUS_DOWN, { = 2 }
    IF_ADMIN_STATUS_TESTING {= 3 } ) ;

{/// Define compartment ID type: }
type
    Puint32 = ^DWORD;
    TNetIfCompartmentId = Puint32;
    TNetIfNetworkGuid = TGUID;

const
    NET_IF_COMPARTMENT_ID_UNSPECIFIED = 0;
    NET_IF_COMPARTMENT_ID_PRIMARY = 1;

    NET_IF_LINK_SPEED_UNKNOWN: Int64 = -1;

// Define datalink interface access types.
type
  TNetIfAccessTtype = (
    NET_IF_ACCESS_UNKNOWN,
    NET_IF_ACCESS_LOOPBACK,
    NET_IF_ACCESS_BROADCAST,
    NET_IF_ACCESS_POINT_TO_POINT,
    NET_IF_ACCESS_POINT_TO_MULTI_POINT,
    NET_IF_ACCESS_MAXIMUM );

// Define datalink interface direction types.
  TNetIfDirectionType = (
    NET_IF_DIRECTION_SENDRECEIVE,
    NET_IF_DIRECTION_SENDONLY,
    NET_IF_DIRECTION_RECEIVEONLY,
    NET_IF_DIRECTION_MAXIMUM  );

  TNetIfConnectionType = (
    NET_IF_CONNECTION_UNKNOWN,
    NET_IF_CONNECTION_DEDICATED,
    NET_IF_CONNECTION_PASSIVE,
    NET_IF_CONNECTION_DEMAND,
    NET_IF_CONNECTION_MAXIMUM );

  TNetIfMediaConnectState = (
    MediaConnectStateUnknown,
    MediaConnectStateConnected,
    MediaConnectStateDisconnected  );

  TMibIfTableLevel = (
    MibIfTableNormal,
    MibIfTableRaw);

// Types of tunnels (sub-type of IF_TYPE when IF_TYPE is IF_TYPE_TUNNEL). }
  TTunnelType = (
    TUNNEL_TYPE_NONE {= 0},
    TUNNEL_TYPE_OTHER {= 1},
    TUNNEL_TYPE_DIRECT {= 2},
    unused3,
    unused4,
    unused5,
    unused6,
    unused7,
    unused8,
    unused9,
    unused10,
    TUNNEL_TYPE_6TO4 {= 11},
    unused12,
    TUNNEL_TYPE_ISATAP {= 13},
    TUNNEL_TYPE_TEREDO {= 14} );

// Medium the Ndis Driver is running on (OID_GEN_MEDIA_SUPPORTED/ OID_GEN_MEDIA_IN_USE, MediaType).
  TNdisMedium = (
    NdisMedium802_3,
    NdisMedium802_5,
    NdisMediumFddi,
    NdisMediumWan,
    NdisMediumLocalTalk,
    NdisMediumDix,
    NdisMediumArcnetRaw,
    NdisMediumArcnet878_2,
    NdisMediumAtm,
    NdisMediumWirelessWan,
    NdisMediumIrda,
    NdisMediumBpc,
    NdisMediumCoWan,
    NdisMedium1394,
    NdisMediumInfiniBand,
    NdisMediumTunnel,
    NdisMediumNative802_11,
    NdisMediumLoopback,
    NdisMediumWiMax,
    NdisMediumIP );

// Physical Medium Type definitions. Used with OID_GEN_PHYSICAL_MEDIUM.
  TNdisPhysicalMedium = (
    NdisPhysicalMediumUnspecified,
    NdisPhysicalMediumWirelessLan,
    NdisPhysicalMediumCableModem,
    NdisPhysicalMediumPhoneLine,
    NdisPhysicalMediumPowerLine,
    NdisPhysicalMediumDSL,      // includes ADSL and UADSL (G.Lite)
    NdisPhysicalMediumFibreChannel,
    NdisPhysicalMedium1394,
    NdisPhysicalMediumWirelessWan,
    NdisPhysicalMediumNative802_11,
    NdisPhysicalMediumBluetooth,
    NdisPhysicalMediumInfiniband,
    NdisPhysicalMediumWiMax,
    NdisPhysicalMediumUWB,
    NdisPhysicalMedium802_3,
    NdisPhysicalMedium802_5,
    NdisPhysicalMediumIrda,
    NdisPhysicalMediumWiredWAN,
    NdisPhysicalMediumWiredCoWan,
    NdisPhysicalMediumOther,
    NdisPhysicalMediumMax);       // Not a real physical type, defined as an upper-bound

// Oct 2014
  TRouterDiscoveryBehaviour = (RouterDiscoveryDisabled, RouterDiscoveryEnabled,
                               RouterDiscoveryDhcp, RouterDiscoveryUnchanged = -1);

  TBandwidthFlag = (NlbwDisabled, NlbwEnabled, NlbwUnchanged = -1);

  TPathBandwidthRod = record
    Bandwidth: LONGLONG;
    Instability: LONGLONG;
    BandwidthPeaked: Boolean;
  end;

  TNetworkCategort = (NetworkCategoryPublic, NetworkCategoryPrivate,
                      NetworkCategoryDomainAuthenticated,
                      NetworkCategoryUnchanged = -1, NetworkCategoryUnknown = -1);

  TInterfaceOffloadRod = (NlatUnspecified, NlatUnicast, NlatAnycast, NlatMulticast,
                          NlatBroadcast, NlatInvalid );

  TRouteOrigin = (NlroManual, NlroWellKnown, NlroDHCP, NlroRouterAdvertisement, Nlro6to4);

  TNeighborState = (NlnsUnreachable, NlnsIncomplete, NlnsProbe, NlnsDelay,
                    NlnsStale, NlnsReachable, NlnsPermanent, NlnsMaximum );

  TLinkLocalAddressBehavior = (LinkLocalAlwaysOff, LinkLocalDelayed, LinkLocalAlwaysOn,
                               LinkLocalUnchanged = -1);


  TTcpConnectionOffloadState = (
    TcpConnectionOffloadStateInHost,
    TcpConnectionOffloadStateOffloading,
    TcpConnectionOffloadStateOffloaded,
    TcpConnectionOffloadStateUploading,
    TcpConnectionOffloadStateMax);

// ParameterChange.
  TMibNoticationType = (MibParameterNotification,
        MibAddInstance, MibDeleteInstance, MibInitialNotification);
//  PMIB_NOTIFICATION_TYPE = ^MIB_NOTIFICATION_TYPE;

// RouteProtocol - Too complex for type
const
  MibIpProtoOther   = 1;
  MibIpProtoLocal   = 2;
  MibIpProtoNetMgmt = 3;
  MibIpProtoIcmp    = 4;
  MibIpProtoEgp     = 5;
  MibIpProtoGgp     = 6;
  MibIpProtoHello   = 7;
  MibIpProtoRip     = 8;
  MibIpProtoIsIs    = 9;
  MibIpProtoEsIs    = 10;
  MibIpProtoCisco   = 11;
  MibIpProtoBbn     = 12;
  MibIpProtoOspf    = 13;
  MibIpProtoBgp     = 14;
  MibIpProtoNtAutostatic   = 10002;
  MibIpProtoNTStatic       = 10006;
  MibIpProtoNTStaticNonDod = 10007;

type
  TRouteInfo = record
    Id: Integer;     // MibIpProtoxxx
    Str: String;
 end;

type
  TIpAddressPrefix = record
    Prefix: TSockAddrIn6;  // TSockAddrInet;
    PrefixLength: byte;
  end;
  PIpAddressPrefix = ^TIpAddressPrefix;

 type
  PTMibIfRow = ^TMibIfRow;   // Windows 2000 and later, replaced by MibIfRow2
  TMibIfRow = record
    wszName: array[1..MAX_INTERFACE_NAME_LEN] of WCHAR;
    dwIndex: DWORD;
    dwType: DWORD;       // see MIB_IF_TYPE and IF_TYPE_xx
    dwMTU: DWORD;
    dwSpeed: DWORD;
    dwPhysAddrLen: DWORD;
    bPhysAddr: array[1..MAXLEN_PHYSADDR] of byte;
    AdminStatus: TAdminStatus;    // see MIB_IF_ADMIN_STATUS
    OperStatus: TIfOperStatus;     // see MIB_IF_OPER_STATUS
    dwLastChange: DWORD;
    dwInOctets: DWORD;
    dwInUcastPkts: DWORD;
    dwInNUCastPkts: DWORD;
    dwInDiscards: DWORD;
    dwInErrors: DWORD;
    dwInUnknownProtos: DWORD;
    dwOutOctets: DWORD;
    dwOutUCastPkts: DWORD;
    dwOutNUCastPkts: DWORD;
    dwOutDiscards: DWORD;
    dwOutErrors: DWORD;
    dwOutQLen: DWORD;
    dwDescrLen: DWORD;
    bDescr: array[1..MAXLEN_IFDESCR] of AnsiChar; //byte;
  end;

 //
  PTMibIfTable = ^TMIBIfTable;
  TMibIfTable = record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMibIfRow;
  end;

// 27 Oct 2014 -
const
// maximum string size in -wchar- units
    IF_MAX_STRING_SIZE = 256;
    IF_MAX_PHYS_ADDRESS_LENGTH = 32;

type
  TInterfaceAndOperStatus = (
    HardwareInterface,
    FilterInterface,
    ConnectorPresent,
    NotAuthenticated,
    NotMediaConnected,
    Paused,
    LowPower,
    EndPointInterface);

  TInterfaceAndOperStatusFlags = set of TInterfaceAndOperStatus;

  PMibIFRow2 = ^TMibIFRow2;        // Vista and later
  TMibIFRow2 = record
    InterfaceLuid: TNetLuid;
    InterfaceIndex: TNetIfIndex;
  // Read-Only fields.
    InterfaceGuid: TGUID;
    Alias: Array[0..IF_MAX_STRING_SIZE] of WCHAR;
    Description: Array[0..IF_MAX_STRING_SIZE] of WCHAR;
    PhysicalAddressLength: ULONG;
    PhysicalAddress: Array[0..IF_MAX_PHYS_ADDRESS_LENGTH-1] of byte;
    PermanentPhysicalAddress: Array[0..IF_MAX_PHYS_ADDRESS_LENGTH-1] of byte;
    Mtu: ULONG;
    IfType: DWORD;       // see MIB_IF_TYPE  and IF_TYPE_xx
    TunnelType: TTunnelType;
    MediaType: TNdisMedium;
    PhysicalMediumType: TNdisPhysicalMedium;
    AccessType: TNetIfAccessTtype;
    DirectionType: TNetIfDirectionType;
    InterfaceAndOperStatusFlags: TInterfaceAndOperStatusFlags;
    OperStatus: TIfOperStatus;
    AdminStatus: TAdminStatus;    // see MIB_IF_ADMIN_STATUS
    MediaConnectState: TNetIfMediaConnectState;
    NetworkGuid:  TNetIfNetworkGuid;
    ConnectionType: TNetIfConnectionType;
  // Statistics.
    TransmitLinkSpeed: LONGLONG;
    ReceiveLinkSpeed: LONGLONG;
    InOctets: LONGLONG;
    InUcastPkts: LONGLONG;
    InNUcastPkts: LONGLONG;
    InDiscards: LONGLONG;
    InErrors: LONGLONG;
    InUnknownProtos: LONGLONG;
    InUcastOctets: LONGLONG;
    InMulticastOctets: LONGLONG;
    InBroadcastOctets: LONGLONG;
    OutOctets: LONGLONG;
    OutUcastPkts: LONGLONG;
    OutNUcastPkts: LONGLONG;
    OutDiscards: LONGLONG;
    OutErrors: LONGLONG;
    OutUcastOctets: LONGLONG;
    OutMulticastOctets: LONGLONG;
    OutBroadcastOctets: LONGLONG;
    OutQLen: LONGLONG;
  end;

  PTMibIfTable2 = ^TMIBIfTable2;
  TMibIfTable2 = record
    NumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMibIfRow2;
  end;


//------ADAPTER INFO STRUCTURES-------------------------------------------------

  PIpAdapterInfo = ^TIpAdapterInfo;
  TIpAdapterInfo = record
    Next: PIpAdapterInfo;
    ComboIndex: DWORD;
    AdapterName: array[1..MAX_ADAPTER_NAME_LENGTH + 4] of AnsiChar;       // Angus
    Description: array[1..MAX_ADAPTER_DESCRIPTION_LENGTH + 4] of AnsiChar;    // Angus
    AddressLength: UINT;
    Address: array[1..MAX_ADAPTER_ADDRESS_LENGTH] of byte;      // Angus
    Index: DWORD;
    aType: UINT;
    DHCPEnabled: UINT;
    CurrentIPAddress: PIpAddrString;
    IPAddressList: TIpAddrString;
    GatewayList: TIpAddrString;
    DHCPServer: TIpAddrString;
    HaveWINS: BOOL;
    PrimaryWINSServer: TIpAddrString;
    SecondaryWINSServer: TIpAddrString;
    LeaseObtained: LongInt ; // UNIX time, seconds since 1970
    LeaseExpires: LongInt;   // UNIX time, seconds since 1970
    SpareStuff: array [1..200] of AnsiChar ;   // Angus - space for IP address lists
  end;

  PIpPerAdapterInfo = ^TIpPerAdapterInfo;  // Angus
  TIpPerAdapterInfo = record
    AutoconfigEnabled: UINT;
    AutoconfigActive: UINT;
    CurrentDnsServer: PIpAddrString;
    DnsServerList: TIpAddrString;
    SpareStuff: array [1..200] of AnsiChar ;   // space for IP address lists
  end;

// 12 Jan 2009 new stuff for GetAdaptersAddresses, requires winsock2

  PIpAdapterUnicastAddress = ^TIpAdapterUnicastAddress;
  TIpAdapterUnicastAddress = record
    Union: record
    case Integer of
        0: (Alignment: Int64);
        1: (Length: DWORD; Flags: DWORD);
    end;
    Next: PIpAdapterUnicastAddress;
    Address: TSocketAddress;
    PrefixOrigin: TIpSuffixOrigin;
    SuffixOrigin: TIpSuffixOrigin;
    DadState: TIpDadState;
    ValidLifetime: ULONG;
    PreferredLifetime: ULONG;
    LeaseLifetime: ULONG;
  end;

  PIpAdapterAnycaseAddress = ^TIpAdapterAnycaseAddress;
  TIpAdapterAnycaseAddress = record
    Union: record
      case Integer of
        0: (Alignment: int64);
        1: (Length: DWORD; Flags: DWORD);
    end;
    Next: PIpAdapterAnycaseAddress;
    Address: TSocketAddress;
  end;

  PIpAdapterMulticastAddress = ^TIpAdapterMulticastAddress;
  TIpAdapterMulticastAddress = record
    Union: record
      case Integer of
        0: (Alignment: Int64);
        1: (Length: DWORD; Flags: DWORD);
    end;
    Next: PIpAdapterMulticastAddress;
    Address: TSocketAddress;
  end;

  PIpAdapterDnsServerAddress = ^TIpAdapterDnsServerAddress;
  TIpAdapterDnsServerAddress = record
    Union: record
      case Integer of
        0: (Alignment: Int64);
        1: (Length: DWORD; Reserved: DWORD);
    end;
    Next: PIpAdapterDnsServerAddress;
    Address: TSocketAddress;
  end;

  PIpAdapterPrefix = ^TIpAdapterPrefix;
  TIpAdapterPrefix = record
    Union: record
    case Integer of
      0: (Alignment: LONGLONG);
      1: (Length: ULONG; Flags: DWORD);
    end;
    Next: PIpAdapterPrefix;
    Address: TSocketAddress;
    PrefixLength: ULONG;
  end;

  PIpAdapterWinsServerAddress = ^TIpAdapterWinsServerAddress;
  TIpAdapterWinsServerAddress = record
   Union: record
      case Integer of
        0: (Alignment: Int64);
        1: (Length: DWORD; Reserved: DWORD);
    end;
    Next: PIpAdapterWinsServerAddress;
    Address: TSocketAddress;
  end;

  PIpAdapterGatewayAddress = ^TIpAdapterGatewayAddress;
  TIpAdapterGatewayAddress = record
    Union: record
      case Integer of
        0: (Alignment: Int64);
        1: (Length: DWORD; Reserved: DWORD);
    end;
    Next: PIpAdapterGatewayAddress;
    Address: TSocketAddress;
  end;


// linked records (NEXT) filled by GetAdaptersAddresses(), XP and later, some elements XP SP1, some Vista
// length: XP SP3=144, Vista=
type
  PIpAdapterAddresses = ^TIpAdapterAddresses;
  TIpAdapterAddresses = record
    Union: record
      case Integer of
        0: (Alignment: int64);
        1: (Length: DWORD;
            IfIndex: DWORD);
    end;
    Next: PIpAdapterAddresses;
    AdapterName: PAnsiChar;
    FirstUnicastAddress: PIpAdapterUnicastAddress;
    FirstAnycastAddress: PIpAdapterAnycaseAddress;
    FirstMulticastAddress: PIpAdapterMulticastAddress;
    FirstDnsServerAddress: PIpAdapterDnsServerAddress;
    DnsSuffix: PWCHAR;
    Description: PWCHAR;
    FriendlyName: PWCHAR;
    PhysicalAddress: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
    PhysicalAddressLength: DWORD;
    Flags: DWORD;
    Mtu: DWORD;
    IfType: DWORD;
    OperStatus: TIfOperStatus;        // last element for XP no SP
    Ipv6IfIndex: DWORD;
    ZoneIndices: array [0..15] of DWORD;
    FirstPrefix: PIpAdapterPrefix;    // last element for XP SP1
    TransmitLinkSpeed: Int64;           // following elements Vista and later
    ReceiveLinkSpeed: Int64;
    FirstWinsServerAddress: PIpAdapterWinsServerAddress;
    FirstGatewayAddress: PIpAdapterGatewayAddress;
    Ipv4Metric: ULONG;
    Ipv6Metric: ULONG;
    Luid: TIfLuid;
    Dhcpv4Server: TSocketAddress;
    CompartmentId: TNetIfCompartmentId;
    NetworkGuid: TNetIfNetworkGuid;
    ConnectionType: TNetIfConnectionType;
    TunnelType: TTunnelType;
    // DHCP v6 Info.
    Dhcpv6Server: TSocketAddress;
    Dhcpv6ClientDuid: array [0..MAX_DHCPV6_DUID_LENGTH] of byte;
    Dhcpv6ClientDuidLength: ULONG;
    Dhcpv6Iaid: ULONG;
  end;

type
    PMibIPInterfaceRow = ^TMibIPInterfaceRow;
    TMibIPInterfaceRow = record
     // Key Structure;
        Family: TAddressFamily;
        InterfaceLuid: TNetLuid;
        InterfaceIndex: TNetIfIndex;
    // Read-Write fields.
    // Fields currently not exposed.
        MaxReassemblySize: ULONG;
        InterfaceIdentifier: LONGLONG;
        MinRouterAdvertisementInterval: ULONG;
        MaxRouterAdvertisementInterval: ULONG;
    // Fileds currently exposed.
        AdvertisingEnabled: BOOLEAN;
        ForwardingEnabled: BOOLEAN;
        WeakHostSend: BOOLEAN;
        WeakHostReceive: BOOLEAN;
        UseAutomaticMetric: BOOLEAN;
        UseNeighborUnreachabilityDetection: BOOLEAN;
        ManagedAddressConfigurationSupported: BOOLEAN;
        OtherStatefulConfigurationSupported: BOOLEAN;
        AdvertiseDefaultRoute: BOOLEAN;
        RouterDiscoveryBehavior: TRouterDiscoveryBehaviour;
        DadTransmits: ULONG;             // DupAddrDetectTransmits in RFC 2462.
        BaseReachableTime: ULONG;
        RetransmitTime: ULONG;
        PathMtuDiscoveryTimeout: ULONG;  // Path MTU discovery timeout (in ms).
        LinkLocalAddressBehavior: TLinkLocalAddressBehavior;
        LinkLocalAddressTimeout: ULONG;  // In ms.
        ZoneIndices : array[0..Ord(ScopeLevelCount)-1] of ULONG; // Zone part of a SCOPE_ID.
        SitePrefixLength: ULONG;
        Metric: ULONG;
        NlMtu: ULONG;
    // Read Only fields.
        Connected: BOOLEAN;
        SupportsWakeUpPatterns: BOOLEAN;
        SupportsNeighborDiscovery: BOOLEAN;
        SupportsRouterDiscovery: BOOLEAN;
        ReachableTime: ULONG;
        TransmitOffload: TInterfaceOffloadRod;
        ReceiveOffload: TInterfaceOffloadRod;
    // Disables using default route on the interface. This flag
    // can be used by VPN clients to restrict Split tunnelling.
        DisableDefaultRoutes: BOOLEAN;
    end;

type
    PMibIPInterfaceTable = ^TMibIPInterfaceTable;
    TMibIPInterfaceTable = record
        NumEntries: ULONG;
        Table : array[0..ANY_SIZE-1] of TMibIPInterfaceRow;
    end;

type
    PMibIpForwardRow2 = ^TMibIpForwardRow2;
    TMibIpForwardRow2 = record
        InterfaceLuid: TNetLuid;
        InterfaceIndex: TNetIfIndex;
        DestinationPrefix: TIpAddressPrefix;
        NextHop: TSockAddrIn6; // TSockAddrInet;  { V8.71 }
      // Read-Write Fields.
        SitePrefixLength: Byte;
        ValidLifetime: ULONG;
        PreferredLifetime: ULONG;
        Metric: ULONG;
        Protocol: ULONG; // TRouteProtocol;
        Loopback: BOOLEAN;
        AutoconfigureAddress: BOOLEAN;
        Publish: BOOLEAN;
        Immortal: BOOLEAN;
      // Read-Only Fields.
        Age: ULONG;
        Origin: TRouteOrigin;
    end;

type
    PMibIpForwardTable2 = ^TMibIpForwardTable2;
    TMibIpForwardTable2 = record
        NumEntries: ULONG;
        Table: Array[0..ANY_SIZE-1] of TMibIpForwardRow2;
    end;

type
    PMibIpPathRow = ^TMibIpPathRow;            // May 2023
    TMibIpPathRow = Record
        Source: TSockAddrIn6;
        Destination: TSockAddrIn6;
        InterfaceLuid: TNetLuid;
        InterfaceIndex: TNetIfIndex;
        CurrentNextHop: TSockAddrIn6;
        PathMtu: ULONG;
        RttMean: ULONG;
        RttDeviation: ULONG;
        ReachabilityTime: record
          case Integer of
            0: (LastReachable: ULONG);
            1: (LastUnreachable: ULONG);
        end;
        IsReachable: Boolean;
        LinkTransmitSpeed: Int64;
        LinkReceiveSpeed: Int64;
    end;

type
    PMibIpPathTable = ^TMibIpPathTable;
    TMibIpPathTable = record
        NumEntries: ULONG;
        Table: Array[0..ANY_SIZE-1] of TMibIpPathRow;
    end;

type
  PMibUnicastIpAddressRow = ^TMibUnicastIpAddressRow;
  TMibUnicastIpAddressRow = record
  // Key Structure.
    Address: TSockAddrIn6; // TSockAddrInet;  { V8.71 }
    InterfaceLuid: TNetLuid;
    InterfaceIndex: TNetIfIndex;
  // Read-Write Fileds.
    PrefixOrigin: TIpPrefixOrigin;
    SuffixOrigin: TIpSuffixOrigin;
    ValidLifetime: ULONG;
    PreferredLifetime: ULONG;
    OnLinkPrefixLength: byte;
    SkipAsSource: BOOLEAN;
  // Read-Only Fields.
    DadState: TIpDadState;
    ScopeId: TScopeId;
    CreationTimeStamp: TFileTime; {LARGE_INTEGER}
  end;

  PMibUnicastIpAddressTable = ^TMibUnicastIpAddressTable;
  TMibUnicastIpAddressTable = record
    NumEntries: ULONG;
    Table: Array[0..ANY_SIZE-1] of TMibUnicastIpAddressRow;
  end;

  PMibAnycastIpAddressRow = ^TMibAnycastIpAddressRow;
  TMibAnycastIpAddressRow = record
 // Key Structure.
    Address: TSockAddrIn6; // TSockAddrInet;  { V8.71 }
    InterfaceLuid: TNetLuid;
    InterfaceIndex: TNetIfIndex;
  //Read-Only Fields. }
    ScopeId: TScopeId;
  end;

  PMibAnycastIpAddressTable = ^TMibAnycastIpAddressTable;
  TMibAnycastIpAddressTable = record
    NumEntries: ULONG;
    Table: array[0..ANY_SIZE - 1] of TMibAnycastIpAddressRow;
  end;

  PMibMulticastIpAddressRow = ^TMibMulticastIpAddressRow;
  TMibMulticastIpAddressRow = record
  // Key Structure.
    Address: TSockAddrIn6; // TSockAddrInet;  { V8.71 }
    InterfaceIndex: TNetIfIndex;
    InterfaceLuid: TNetLuid;
  // Read-Only Fields.
    ScopeId: TScopeId;
  end;

  PMibMulticastIpAddressTable = ^TMibMulticastIpAddressTable;
  TMibMulticastIpAddressTable = record
    NumEntries: ULONG;
    Table: array[0..ANY_SIZE - 1] of TMibMulticastIpAddressRow;
  end;

//----------------TCP STRUCTURES------------------------------------------------

  PTMibTCPRow = ^TMibTCPRow;
  TMibTCPRow = record
    dwState: DWORD;
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwRemoteAddr: DWORD;
    dwRemotePort: DWORD;
  end;
  //
  PTMibTCPTable = ^TMibTCPTable;
  TMibTCPTable = record
    dwNumEntries: DWORD;
    Table: array[0..0] of TMibTCPRow;
  end;
  //
  PTMibTCP6Row = ^TMibTCP6Row;
  TMibTCP6Row = record
    dwState: DWORD;
    LocalAddr: IN6_ADDR;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
    RemoteAddr: IN6_ADDR;
    dwRemoteScopeId: DWORD;
    dwRemotePort: DWORD;
  end;
  //
  PTMibTCP6Table = ^TMibTCP6Table;
  TMibTCP6Table = record
    dwNumEntries: DWORD;
    Table: array[0..0] of TMibTCP6Row;
  end;
  //
  PTMibTCPRow2 = ^TMibTCPRow2;
  TMibTCPRow2 = record
    dwState: DWORD;
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwRemoteAddr: DWORD;
    dwRemotePort: DWORD;
    dwOwningPid: LongInt;
    dwOffloadState: TTcpConnectionOffloadState;
  end;
  //
  PTMibTCPTable2 = ^TMibTCPTable2;
  TMibTCPTable2 = record
    dwNumEntries: DWORD;
    Table: array[0..0] of TMibTCPRow2;
  end;
  //
  PTMibTCP6Row2 = ^TMibTCP6Row2;
  TMibTCP6Row2 = record
    LocalAddr: IN6_ADDR;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
    RemoteAddr: IN6_ADDR;
    dwRemoteScopeId: DWORD;
    dwRemotePort: DWORD;
    dwState: DWORD;
    dwOwningPid: LongInt;
    dwOffloadState: TTcpConnectionOffloadState;
  end;
  //
  PTMibTCP6Table2 = ^TMibTCP6Table2;
  TMibTCP6Table2 = record
    dwNumEntries: DWORD;
    Table: array[0..0] of TMibTCP6Row2;
  end;
  //
  PTMibTCPStats = ^TMibTCPStats;
  TMibTCPStats = record
    dwRTOAlgorithm: DWORD;
    dwRTOMin: DWORD;
    dwRTOMax: DWORD;
    dwMaxConn: DWORD;
    dwActiveOpens: DWORD;
    dwPassiveOpens: DWORD;
    dwAttemptFails: DWORD;
    dwEstabResets: DWORD;
    dwCurrEstab: DWORD;
    dwInSegs: DWORD;
    dwOutSegs: DWORD;
    dwRetransSegs: DWORD;
    dwInErrs: DWORD;
    dwOutRsts: DWORD;
    dwNumConns: DWORD;
  end;

  PTMibTCPStats2 = ^TMibTCPStats2;   // May 2023 Windows 10
  TMibTCPStats2 = record
    dwRTOAlgorithm: DWORD;
    dwRTOMin: DWORD;
    dwRTOMax: DWORD;
    dwMaxConn: DWORD;
    dwActiveOpens: DWORD;
    dwPassiveOpens: DWORD;
    dwAttemptFails: DWORD;
    dwEstabResets: DWORD;
    dwCurrEstab: DWORD;
    dw64InSegs: Int64;
    dw64OutSegs: Int64;
    dwRetransSegs: DWORD;
    dwInErrs: DWORD;
    dwOutRsts: DWORD;
    dwNumConns: DWORD;
  end;

//---------UDP STRUCTURES-------------------------------------------------------

  PTMibUDPRow = ^TMibUDPRow;
  TMibUDPRow = record
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
  end;
 //
  PTMibUDPTable = ^TMIBUDPTable;
  TMIBUDPTable = record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMibUDPRow;
  end;
 //
  PTMibUDP6Row = ^TMibUDP6Row;
  TMibUDP6Row = record
    dwLocalAddr: IN6_ADDR;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
  end;
 //
  PTMibUDP6Table = ^TMIBUDP6Table;
  TMIBUDP6Table = record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMibUDP6Row;
  end;
 //
  PTMibUdpStats = ^TMIBUdpStats;
  TMIBUdpStats = record
    dwInDatagrams: DWORD;
    dwNoPorts: DWORD;
    dwInErrors: DWORD;
    dwOutDatagrams: DWORD;
    dwNumAddrs: DWORD;
  end;

  PTMibUdpStats2 = ^TMIBUdpStats2;    // May 2023 Windows 10
  TMIBUdpStats2 = record
    dw64InDatagrams: Int64;
    dwNoPorts: DWORD;
    dwInErrors: DWORD;
    dw64OutDatagrams: Int64;
    dwNumAddrs: DWORD;
  end;

//-----------IP STRUCTURES------------------------------------------------------

 //
  PTMibIPNetRow = ^TMibIPNetRow;
  TMibIPNetRow = record
    dwIndex: DWord;
    dwPhysAddrLen: DWord;
    bPhysAddr: TPhysMacAddr;
    dwAddr: DWord;
    dwType: DWord;
  end;
  //
  PTMibIPNetTable = ^TMibIPNetTable;
  TMibIPNetTable = record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMibIPNetRow;
  end;
  //
  PTMibIPStats = ^TMibIPStats;
  TMibIPStats = record
    dwForwarding: DWORD;
    dwDefaultTTL: DWORD;
    dwInReceives: DWORD;
    dwInHdrErrors: DWORD;
    dwInAddrErrors: DWORD;
    dwForwDatagrams: DWORD;
    dwInUnknownProtos: DWORD;
    dwInDiscards: DWORD;
    dwInDelivers: DWORD;
    dwOutRequests: DWORD;
    dwRoutingDiscards: DWORD;
    dwOutDiscards: DWORD;
    dwOutNoRoutes: DWORD;
    dwReasmTimeOut: DWORD;
    dwReasmReqds: DWORD;
    dwReasmOKs: DWORD;
    dwReasmFails: DWORD;
    dwFragOKs: DWORD;
    dwFragFails: DWORD;
    dwFragCreates: DWORD;
    dwNumIf: DWORD;
    dwNumAddr: DWORD;
    dwNumRoutes: DWORD;
  end;
  //
  PTMibIPAddrRow = ^TMibIPAddrRow;
  TMibIPAddrRow = record
    dwAddr: DWORD;
    dwIndex: DWORD;
    dwMask: DWORD;
    dwBCastAddr: DWORD;
    dwReasmSize: DWORD;
    Unused1: WORD;
    wType: WORD;  // XP and later - MIB_IPADDR_xx literals - Nov 2014
  end;
  //
  PTMibIPAddrTable = ^TMibIPAddrTable;
  TMibIPAddrTable = record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMibIPAddrRow;
  end;

  //
  PTMibIPForwardRow = ^TMibIPForwardRow;
  TMibIPForwardRow = record
    dwForwardDest: DWORD;
    dwForwardMask: DWORD;
    dwForwardPolicy: DWORD;
    dwForwardNextHop: DWORD;
    dwForwardIFIndex: DWORD;
    dwForwardType: DWORD;
    dwForwardProto: DWORD;
    dwForwardAge: DWORD;
    dwForwardNextHopAS: DWORD;
    dwForwardMetric1: DWORD;
    dwForwardMetric2: DWORD;
    dwForwardMetric3: DWORD;
    dwForwardMetric4: DWORD;
    dwForwardMetric5: DWORD;
  end;
  //
  PTMibIPForwardTable = ^TMibIPForwardTable;
  TMibIPForwardTable = record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMibIPForwardRow;
  end;

//--------ICMP-STRUCTURES------------------------------------------------------

  PTMibICMPStats = ^TMibICMPStats;
  TMibICMPStats = record
    dwMsgs: DWORD;
    dwErrors: DWORD;
    dwDestUnreachs: DWORD;
    dwTimeEcxcds: DWORD;
    dwParmProbs: DWORD;
    dwSrcQuenchs: DWORD;
    dwRedirects: DWORD;
    dwEchos: DWORD;
    dwEchoReps: DWORD;
    dwTimeStamps: DWORD;
    dwTimeStampReps: DWORD;
    dwAddrMasks: DWORD;
    dwAddrReps: DWORD;
  end;

  PTMibICMPInfo = ^TMibICMPInfo;
  TMibICMPInfo = record
    InStats: TMibICMPStats;
    OutStats: TMibICMPStats;
  end;

  PMibICMPStatsEx = ^TMibICMPStatsEx;   // May 2023
  TMibICMPStatsEx = record
    dwMsgs: DWORD;
    dwErrors: DWORD;
    rgdwTypeCount: array[0..256] of DWORD;
  end;

  PTMibICMPExInfo = ^TMibICMPExInfo;    // May 2023
  TMibICMPExInfo = record
    InStats: TMibICMPStatsEx;
    OutStats: TMibICMPStatsEx;
  end;


// 13 Jan 2009 - GetExtendedTcpTable and GetExtendedUdpTable structures, XP SP2, Vista and better

type
   TTcpTableClass = (
    TCP_TABLE_BASIC_LISTENER,
    TCP_TABLE_BASIC_CONNECTIONS,
    TCP_TABLE_BASIC_ALL,
    TCP_TABLE_OWNER_PID_LISTENER,
    TCP_TABLE_OWNER_PID_CONNECTIONS,
    TCP_TABLE_OWNER_PID_ALL,
    TCP_TABLE_OWNER_MODULE_LISTENER,
    TCP_TABLE_OWNER_MODULE_CONNECTIONS,
    TCP_TABLE_OWNER_MODULE_ALL) ;

  TUdpTableClass = (
    UDP_TABLE_BASIC,
    UDP_TABLE_OWNER_PID,
    UDP_TABLE_OWNER_MODULE );

  TTcpIpOwnerModuleInfoClass = (
    TcpIpOwnerModuleInfoClassBasic  );

  TTcpIpOwnerModuleBasicInfo = record
    pModuleName: PWCHAR;
    pModulePath: PWCHAR;
  end;
  PTcpIpOwnerModuleBasicInfo = ^TTcpIpOwnerModuleBasicInfo;

  TTcpIpOwnerModuleBasicInfoEx = record
    TcpIpOwnerModuleBasicInfo: TTcpIpOwnerModuleBasicInfo ;
    Buffer: Array[0..1024] of byte;  // space for module name and path
  end;

  TMibTcpRowOwnerPID = record
    dwState: LongInt;
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwRemoteAddr: DWORD;
    dwRemotePort: DWORD;
    dwOwningPid: DWORD;
  end;
  PTMibTcpRowOwnerPID = ^TMibTcpRowOwnerPID;

  TMibTcpTableOwnerPID = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibTcpRowOwnerPID;
  end;
  PTMibTcpTableOwnerPID = ^TMibTcpTableOwnerPID;

  TMibTcp6RowOwnerPID = record
    ucLocalAddr: TInAddr6;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
    ucRemoteAddr: TInAddr6;
    dwRemoteScopeId: DWORD;
    dwRemotePort: DWORD;
    dwState: DWORD;
    dwOwningPid: DWORD;
  end;
  PTMibTcp6RowOwnerPID = ^TMibTcp6RowOwnerPID;

  TMibTcp6TableOwnerPID = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibTcp6RowOwnerPID;
  end;
  PTMibTcp6TableOwnerPID = ^TMibTcp6TableOwnerPID;

  TMibTcpRowOwnerModule = record
    dwState: DWORD;
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwRemoteAddr: DWORD;
    dwRemotePort: DWORD;
    dwOwningPid: DWORD;
    liCreateTimestamp: TFileTime; {LARGE_INTEGER}
    OwningModuleInfo: Array[0..TCPIP_OWNING_MODULE_SIZE-1] of LONGLONG;
  end;
  PTMibTcpRowOwnerModule = ^TMibTcpRowOwnerModule;

  TMibTcpTableOwnerModule = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibTcpRowOwnerModule;
  end;
  PTMibTcpTableOwnerModule = ^TMibTcpTableOwnerModule;

// Oct 2014
  TMibTcp6RowOwnerModule = record
    ucLocalAddr: TInAddr6;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
    ucRemoteAddr: TInAddr6;
    dwRemoteScopeId: DWORD;
    dwRemotePort: DWORD;
    dwState: DWORD;
    dwOwningPid: DWORD;
    liCreateTimestamp: TFileTime; {LARGE_INTEGER}
    OwningModuleInfo: Array[0..TCPIP_OWNING_MODULE_SIZE-1] of LONGLONG;
  end;
  PTMibTcp6RowOwnerModule = ^TMibTcp6RowOwnerModule;

  TMibTcp6TableOwnerModule = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibTcp6RowOwnerModule;
  end;
  PTMibTcp6TableOwnerModule = ^TMibTcp6TableOwnerModule;

  TMibUdpRowOwnerPID = record
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwOwningPid: DWORD;
  end;
  PTMibUdpRowOwnerPID = ^TMibUdpRowOwnerPID;

  _MIB_UDPTABLE_OWNER_PID = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibUdpRowOwnerPID;
  end;
  TMibUdpTableOwnerPID = _MIB_UDPTABLE_OWNER_PID;
  PTMibUdpTableOwnerPID = ^_MIB_UDPTABLE_OWNER_PID;

  TMibUdp6RowOwnerPID = record
    ucLocalAddr: TInAddr6;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
    dwOwningPid: DWORD;
  end;
  PTMibUdp6RowOwnerPID = ^TMibUdp6RowOwnerPID;

  TMibUdp6TableOwnerPID = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibUdp6RowOwnerPID;
  end;
  PTMibUdp6TableOwnerPID = ^TMibUdp6TableOwnerPID;

  TMibUdpRowOwnerModule = record
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwOwningPid: DWORD;
    unknown: DWORD;  // Angus - had to add this dummy element so the record is the correct length and timestamp works
    liCreateTimestamp: TFileTime; {LARGE_INTEGER}
    SpecificPortBind: integer;
    OwningModuleInfo: Array[0..TCPIP_OWNING_MODULE_SIZE-1] of LONGLONG;
  end;
  PTMibUdpRowOwnerModule = ^TMibUdpRowOwnerModule;

  TMibUdpTableOwnerModule = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibUdpRowOwnerModule;
  end;
  PTMibUdpTableOwnerModule = ^TMibUdpTableOwnerModule;

  TMibUdp6RowOwnerModule = record
    ucLocalAddr: TInAddr6;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
    dwOwningPid: DWORD;
    unknown: DWORD;  // Angus - had to add this dummy element so the record is the correct length and timestamp works
    liCreateTimestamp: TFileTime; {LARGE_INTEGER}
    SpecificPortBind: integer;
    OwningModuleInfo: Array[0..TCPIP_OWNING_MODULE_SIZE-1] of LONGLONG;
  end;
  PTMibUdp6RowOwnerModule = ^TMibUdp6RowOwnerModule;

  TMibUdp6TableOwnerModule = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibUdp6RowOwnerModule;
  end;
  PTMibUdp6TableOwnerModule = ^TMibUdp6TableOwnerModule;


// April 2023 ARP for IPv6, IP neighbour table IPs against MAC addresses
type
  _Mib_IpNet_Row2 = record
    Address: TSockAddrIn6;
    InterfaceIndex: TNetIfIndex;
    InterfaceLuid: TNetLuid;
    PhysicalAddress: array [0..IF_MAX_PHYS_ADDRESS_LENGTH - 1] of Byte;
    PhysicalAddressLength: ULONG;
    State: TNeighborState;
    Flags: Byte;
    ReachabilityTime: record
      case Integer of
        0: (LastReachable: ULONG);
        1: (LastUnreachable: ULONG);
    end;
  end;
  TMibIpNetRow2 = _Mib_IpNet_Row2;
  PMibIpNetRow2 = ^TMibIpNetRow2;

  _Mib_IpNet_Table2 = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibIpNetRow2;
  end;
  TMibIpNetTable2 = _Mib_IpNet_Table2;
  PMibIpNetTable2 = ^TMibIpNetTable2;



//------------------imports from IPHLPAPI.DLL-----------------------------------

var
GetAdaptersInfo: function ( pAdapterInfo: PIpAdapterInfo; pOutBufLen: PULONG ): DWORD; stdcall;
GetPerAdapterInfo: function (IfIndex: ULONG; pPerAdapterInfo: PIpPerAdapterInfo; pOutBufLen: PULONG):DWORD; stdcall;
GetNetworkParams: function ( FixedInfo: PTFixedInfo; pOutPutLen: PULONG ): DWORD; stdcall;
GetTcpTable: function ( pTCPTable: PTMibTCPTable; pDWSize: PDWORD; bOrder: BOOL ): DWORD; stdcall;
GetTcpStatistics: function ( pStats: PTMibTCPStats ): DWORD; stdcall;
GetUdpTable: function ( pUdpTable: PTMibUDPTable; pDWSize: PDWORD; bOrder: BOOL ): DWORD; stdcall;
GetUdpStatistics: function ( pStats: PTMibUdpStats ): DWORD; stdcall;
GetIpStatistics: function ( pStats: PTMibIPStats ): DWORD; stdcall;
GetIpNetTable: function ( pIpNetTable: PTMibIPNetTable; pdwSize: PULONG;  bOrder: BOOL ): DWORD; stdcall;
GetIpAddrTable: function ( pIpAddrTable: PTMibIPAddrTable; pdwSize: PULONG; bOrder: BOOL ): DWORD; stdcall;
GetIpForwardTable: function ( pIPForwardTable: PTMibIPForwardTable; pdwSize: PULONG; bOrder: BOOL ): DWORD; stdCall;
GetIcmpStatistics: function ( pStats: PTMibICMPInfo ): DWORD; stdCall;
GetRTTAndHopCount: function ( DestIPAddress: DWORD; HopCount: PULONG;  MaxHops: ULONG; RTT: PULONG ): BOOL; stdCall;
GetIfTable: function ( pIfTable: PTMibIfTable; pdwSize: PULONG;   bOrder: boolean ): DWORD; stdCall;
GetIfEntry: function ( pIfRow: PTMibIfRow ): DWORD; stdCall;
// warning - documentation is vague about where the result is provided
GetFriendlyIfIndex: function (var IfIndex: DWORD): DWORD; stdcall;

// 12 Jan 2009 replacement for GetAdaptersInfo, XP and later
GetAdaptersAddresses: function ( Family: LongWord; Flags: LongWord; Reserved: Pointer;
         AdapterAddresses: PIpAdapterAddresses; SizePointer: PULONG): DWORD stdcall ;
// 12 Jan 2009 - replacement for AllocateAndGetTcpExTableFromStack - XP SP2, W2K3 SP1, Vista and later
GetExtendedTcpTable: function ( pTCPTable: Pointer; pDWSize: PDWORD;
    bOrder: BOOL; ulAf: LongWord; TableClass: TTcpTableClass; Reserved: LongWord): DWORD; stdcall;
GetOwnerModuleFromTcpEntry: function( pTcpEntry: PTMibTcpRowOwnerModule;
      InfoClass: TTcpIpOwnerModuleInfoClass; pBuffer: Pointer; pdwSize: PDWORD): LongInt stdcall ;
GetExtendedUdpTable: function ( pUdpTable: Pointer; pdwSize: PDWORD;
      bOrder: BOOL; ulAf: LongWord; TableClass: TUdpTableClass; Reserved: LongWord): LongInt stdcall ;
GetOwnerModuleFromUdpEntry: function ( pUdpEntry: PTMibUdpRowOwnerModule;
      InfoClass: TTcpIpOwnerModuleInfoClass; pBuffer: Pointer; pdwSize: PDWORD): LongInt stdcall ;

// Nov 2014 - notify IP address and route changes, some Vista and later
type
TIpForwardChangeCallback = procedure (CallerContext: Pointer;  Row: PMibIpForwardRow2;
                                                NotificationType: TMibNoticationType); stdcall;
PIpForwardChangeCallback = ^TIpForwardChangeCallback;
TIpInterfaceChangeCallback = procedure (CallerContext: Pointer; Row: PMibIPInterfaceRow;
                                               NotificationType: TMibNoticationType); stdcall;
PIpInterfaceChangeCallback = ^TIpInterfaceChangeCallback;
TUnicastIpAddressChangeCallback = procedure (CallerContext: Pointer; Row: PMibUnicastIpAddressRow;
                                                NotificationType: TMibNoticationType); stdcall;
PUnicastIpAddressChangeCallback = ^TUnicastIpAddressChangeCallback;
TStableUnicastIpAddressTableCallback = procedure (CallerContext: Pointer;
                                                AddressTable: PMibUnicastIpAddressTable); stdcall;
PStableUnicastIpAddressTableCallback = ^TStableUnicastIpAddressTableCallback;

// Nov 2014 - get IP addresses and routes, some Vista and later
var
GetBestRoute: function(dwDestAddr, dwSourceAddr: DWORD; pBestRoute: PTMibIPForwardRow): DWORD; stdcall;
NotifyAddrChange: function (var Handle: THandle; overlapped: POVERLAPPED): DWORD; stdcall;
NotifyRouteChange: function (var Handle: THandle; overlapped: POVERLAPPED): DWORD; stdcall;
NotifyRouteChange2: function (Family: TAddressFamily; Callback: PIpForwardChangeCallback;
        CallerContext: Pointer; InitialNotification: BOOLEAN; var NotificationHandle: THandle): DWORD; stdcall;
CancelIPChangeNotify: function (notifyOverlapped: POVERLAPPED): BOOL; stdcall;
NotifyIpInterfaceChange: function (Family: TAddressFamily; Callback: PIpInterfaceChangeCallback;
                CallerContext: Pointer; InitialNotification: BOOLEAN; var NotificationHandle: THandle): DWORD; stdcall;
NotifyUnicastIpAddressChange: function (Family: TAddressFamily; Callback: PUnicastIpAddressChangeCallback;
                CallerContext: Pointer; InitialNotification: BOOLEAN; var NotificationHandle: THandle): DWORD; stdcall;
NotifyStableUnicastIpAddressTable: function (Family: TAddressFamily; var Table: PMibUnicastIpAddressTable;
    CallerCallback: TStableUnicastIpAddressTableCallback; CallerContext: Pointer; NotificationHandle: PHandle): DWORD; stdcall;
SetUnicastIpAddressEntry: function (const Row: PMibUnicastIpAddressRow): DWORD; stdcall;
CancelMibChangeNotify2: function (NotificationHandle: THandle): DWORD; stdcall;
GetIpInterfaceEntry: function (var Row: TMibIPInterfaceRow): DWORD; stdcall;
GetIpInterfaceTable: function (Family: TAddressFamily; var Table: PMibIPInterfaceTable): DWORD; stdcall;
InitializeIpInterfaceEntry: procedure (var Row: TMibIPInterfaceRow); stdcall;
SetIpInterfaceEntry: function (Row: PMibIPInterfaceRow): DWORD; stdcall;
CreateUnicastIpAddressEntry: function (const Row: PMibUnicastIpAddressRow): DWORD; stdcall;
DeleteUnicastIpAddressEntry: function (const Row: PMibUnicastIpAddressRow): DWORD; stdcall;
GetUnicastIpAddressEntry: function (var Row: TMibUnicastIpAddressRow): DWORD; stdcall;
GetUnicastIpAddressTable: function (Family: TAddressFamily; var Table: PMibUnicastIpAddressTable): DWORD; stdcall;
InitializeUnicastIpAddressEntry: function (var Row: TMibUnicastIpAddressRow): DWORD; stdcall;
CreateAnycastIpAddressEntry: function (const Row: PMibAnycastIpAddressRow): DWORD; stdcall;
DeleteAnycastIpAddressEntry: function (const Row: PMibAnycastIpAddressRow): DWORD; stdcall;
GetAnycastIpAddressEntry: function (var Row: TMibAnycastIpAddressRow): DWORD; stdcall;
GetAnycastIpAddressTable: function (Family: TAddressFamily; var Table: PMibAnycastIpAddressTable): DWORD; stdcall;
GetMulticastIpAddressEntry: function (var Row: TMibMulticastIpAddressRow): DWORD; stdcall;
GetMulticastIpAddressTable: function (Family: TAddressFamily; var Table: PMibMulticastIpAddressTable): DWORD; stdcall;
FreeMibTable: procedure (Memory: Pointer); stdcall;
ConvertInterfaceNameToLuidW: function (const InterfaceName: PWideChar; InterfaceLuid: PNetLuid): DWORD; stdcall;
ConvertInterfaceLuidToNameW: function (const InterfaceLuid: PNetLuid; InterfaceName: PWideChar; Length: DWORD): DWORD; stdcall;
ConvertInterfaceIndexToLuid: function (const InterfaceIndex: TNetIfIndex; InterfaceLuid: PNetLuid): DWORD; stdcall;

// 27 Oct 2014 - IPv6 versions of earlier APIs
GetIfEntry2: function (var pIfRow: TMibIfRow2): DWORD; stdCall;
GetIfTable2: function (var pIfTable: PTMibIfTable2): DWORD; stdCall;
GetIfTable2Ex: function (Level: TMibIfTableLevel; var pIfTable: PTMibIfTable2): DWORD; stdCall;
GetIpForwardTable2: function (Family: TAddressFamily; var pIPForwardTable: PMibIPForwardTable2): DWORD; stdCall;
GetTcp6Table: function (var pTCPTable: PTMibTCP6Table; pDWSize: PDWORD; bOrder: BOOL): DWORD; stdcall;
GetTcpStatisticsEx: function (dwFamily: DWORD; var pStats: PTMibTCPStats): DWORD; stdcall;
GetUdp6Table: function (var pUdpTable: PTMibUDP6Table; pDWSize: PDWORD; bOrder: BOOL ): DWORD; stdcall;
GetUdpStatisticsEx: function (dwFamily: DWORD; var pStats: PTMibUdpStats): DWORD; stdcall;
GetOwnerModuleFromTcp6Entry: function( pTcpEntry: PTMibTcp6RowOwnerModule;
    InfoClass: TTcpIpOwnerModuleInfoClass; pBuffer: Pointer; pdwSize: PDWORD): DWORD stdcall ;
GetOwnerModuleFromUdp6Entry: function ( pUdpEntry: PTMibUdp6RowOwnerModule;
    InfoClass: TTcpIpOwnerModuleInfoClass; pBuffer: Pointer; pdwSize: PDWORD): DWORD stdcall ;

// May 2023 ARP for IPv6, IP neighbour table IPs against MAC addresses, IP paths
CreateIpNetEntry2: function (const Row: PMibIpNetRow2): DWORD stdcall ;
GetIpNetEntry2: function (var Row: TMibIpNetRow2): DWORD stdcall ;
ResolveIpNetEntry2: function (var Row: TMibIpNetRow2; SourceAddress: TSockAddrIn6): DWORD stdcall ;    // replaces SendARP
SetIpNetEntry2: function (var Row: TMibIpNetRow2): DWORD stdcall ;
GetIpNetTable2: function (Family: TAddressFamily; var Table: PMibIpNetTable2): DWORD stdcall ;
FlushIpNetTable2: function (Family: TAddressFamily; InterfaceIndex: TNETIFINDEX): DWORD stdcall ;
ConvertLengthToIpv4Mask: function(MaskLength: ULONG; var Mask: ULONG): DWORD stdcall ;
GetIpPathTable: function(Family: TAddressFamily; var Table: PMibIpPathTable): DWORD stdcall ;
ConvertInterfaceLuidToAlias: function (const InterfaceLuid: PNetLuid; InterfaceAlias: PWideChar; Length: DWORD): DWORD; stdcall;
GetIpPathEntry: function(var Row: TMibIpPathRow): DWORD stdcall ;
GetIcmpStatisticsEx: function (pStats: PTMibICMPExInfo): DWORD; stdCall;
GetIpStatisticsEx: function (pStats: PTMibIPStats ): DWORD; stdcall;
GetTcpStatisticsEx2: function (Family: TAddressFamily; var pStats: PTMibTCPStats2): DWORD; stdcall;   // Windows 10
GetUdpStatisticsEx2: function (Family: TAddressFamily; var pStats: PTMibUdpStats2): DWORD; stdcall;   // Windows 10

const
    IpHlpDLL = 'IPHLPAPI.DLL';
var
    IpHlpModule: THandle;

// headers for Delphi functions accessing low level IpHelper APIs.

type
    TTransProt = (ProtoBoth, ProtoTcp, ProtoUdp);        // May 2023

//-----------conversion of ICMP error codes to strings--------------------------
             {taken from www.sockets.com/ms_icmp.c }

const
  ICMP_ERROR_BASE = 11000;
  IcmpErr : array[1..22] of string =
  (
   'IP_BUFFER_TOO_SMALL','IP_DEST_NET_UNREACHABLE', 'IP_DEST_HOST_UNREACHABLE',
   'IP_PROTOCOL_UNREACHABLE', 'IP_DEST_PORT_UNREACHABLE', 'IP_NO_RESOURCES',
   'IP_BAD_OPTION','IP_HARDWARE_ERROR', 'IP_PACKET_TOO_BIG', 'IP_REQUEST_TIMED_OUT',
   'IP_BAD_REQUEST','IP_BAD_ROUTE', 'IP_TTL_EXPIRED_TRANSIT',
   'IP_TTL_EXPIRED_REASSEM','IP_PARAMETER_PROBLEM', 'IP_SOURCE_QUENCH',
   'IP_OPTION_TOO_BIG', 'IP_BAD_DESTINATION','IP_ADDRESS_DELETED',
   'IP_SPEC_MTU_CHANGE', 'IP_MTU_CHANGE', 'IP_UNLOAD'
  );


//----------conversion of diverse enumerated values to strings------------------

  AdaptTypes    : array[1..MAX_IF_TYPE] of string = (    // 9 February 2007
    'Other', 'Reg_1822', 'HDH_1822', 'DDN_X25', 'RFC877X25', 'Ethernet', 'ISO88023',
    'ISO88024', 'Token Ring', 'ISO88026', 'StarLan', 'Proteon10', 'Proteon80', 'HyperChnl',
    'FDDI', 'LAP_B','SDLC', 'DS1', 'E1', 'Basic ISDN', 'Primary ISDN', 'Prop_P2P', 'PPP', 'Loopback',
    'EON','Eth_3MB', 'NSIP', 'SLIP','Ultra', 'DS3', 'SIP', 'FrameRly', 'RS232', 'Para', 'Arcnet',
    'Arcnet+', 'ATM', 'MIO_X25', 'Sonet', 'X25_PLE', 'ISO88022', 'LocalTalk', 'SMDS_DXI',
    'FrmRlySrv', 'V35', 'HSSI', 'HIPPI', 'Modem', 'AAL5', 'SonetPath', 'Sonet_VT', 'SMDS_ICIP',
    'Prop_Virt', 'Prop_Mux', 'IEEE80212','FibreChnl', 'HIPPIifce', 'FrmRlyIcn', 'ALanE8023',
    'ALanE8025', 'CCT_Emul', 'FastEther', 'ISDN', 'V11', 'V36', 'G703_64K', 'G703_2MB',
    'QLLC', 'FastEthFX', 'Channel', '802.11 Wireless', 'IBM370', 'Escon', 'DSLW', 'ISDN_S', 'ISDN_U',
    'LAP_D', 'IPSwitch', 'RSRB', 'ATM_Logic', 'DSO', 'DOSBundle', 'BSC', 'Async', 'CNR',
    'ISO88025',  'EPLRS', 'ARAP', 'Prop_CNLS', 'HostPad', 'TermPad', 'FrmRlyMPI', 'X213',
    'ADSL', 'RADSL', 'SDSL', 'VDSL', 'ISO88025', 'Myrinet', 'Voice_EM', 'Voice_FX0',
    'Voice_FXS', 'Voice_Cap','VOIP', 'ATM_DXI', 'ATM_FUNI', 'ATM_IMA', 'PPPMulti', 'IpOvCDLC',
    'IpOvCLAW', 'Stck2Stck',  'VirtIPAdr', 'MPC', 'IpOv_ATM', '88025Fibr', 'TDLC', 'GigaBit',
    'HDLC', 'LAP_F', 'V37', 'X25_MLP', 'X25_Hunt', 'TransHDLC', 'InterLeav', 'Fast', 'IP',
    'CATV_MACL', 'CATV_DwnS', 'CATV_UpSt', 'A12MPP_Sw', 'Tunnel', 'Coffee', 'CES', 'ATM_SubIF',
    'L2_VLAN', 'L3_IPVLAN', 'L3_IPXVLN', 'PowerLine',  'MedaiMail', 'DTM', 'DCN', 'IPForward',
    'MSDSL', '1394 Firewire', 'GSN',
     // following added Oct 2014
    'DVBRCC_MacLayer', 'DVBRCC_Downstream', 'DVBRCC_Upstream', 'ATM_Virtual', 'MPLS_Tunnel',
    'SRP', 'VoiceOverATM', 'VoiceOverFrameRelay', 'IDSL', 'CompositeLink', 'SS7_Siglink',
    'Prop_Wireless_P2P', 'FR_Forward', 'RFC1483', 'USB', 'IEEE8023AD_LAG', 'BGP_Policy_Accounting',
    'FRF16_MFR_Bundle', 'H323_Gatekeeper', 'H323_Proxy',  'MPLS', 'MF_Siglink', 'HDSL2',
    'SHDSL', 'DS1_FDL',  'POS', 'DVB_ASI_In', 'DVB_ASI_Out', 'PLC',  'NFAS', 'TR008',
    'GR303_RDT', 'GR303_IDT', 'ISUP', 'Prop_Docs_Wireless_MacLayer', 'Prop_Docs_Wireless_Downstream',
    'Prop_Docs_Wireless_Upstream', 'HiperLan2', 'Prop_BWA_P2MP','Sonet_Overhead_Channel',
    'Digital_Wrapper_Overhead_Channel', 'AAL2', 'Radio_Mac', 'ATM_Radio', 'IMT', 'MVL',
    'Reach_DSL', 'FR_DLCI_Endpt', 'ATM_VCI_Endpt', 'Optical_Channel', 'Optical_Transport',  // 196
    '','','',  // 197-199
    '','','','','','','','','','', // 200-209
    '','','','','','','','','','', // 210-219
    '','','','','','','','','','', // 220-229
    '','','','','','','', // 230-236
    '802.16 WiMax',     // 237
    '','','','','', // 238-242
    'WWAN GSM',   // 243 WWAN devices based on GSM technology
    'WWAN CDMA'   // 244 WWAN devices based on CDMA technology
    );

  ARPEntryType  : array[0..4] of string = ('', 'Other', 'Invalid',
    'Dynamic', 'Static'
    );
  TCPConnState  :
    array[0..12] of string =
    ('',  'closed', 'listening', 'syn_sent',
    'syn_rcvd', 'established', 'fin_wait1',
    'fin_wait2', 'close_wait', 'closing',
    'last_ack', 'time_wait', 'delete_tcb'
    );

  TCPToAlgo     : array[0..4] of string =
    ('',  'Const.Timeout', 'MIL-STD-1778',
    'Van Jacobson', 'Other' );

  IPForwTypes   : array[0..4] of string =
    ('',  'other', 'invalid', 'local', 'remote' );

{  IPForwProtos  : array[0..18] of string =
    ('',  'OTHER', 'LOCAL', 'NETMGMT', 'ICMP', 'EGP',
    'GGP', 'HELO', 'RIP', 'IS_IS', 'ES_IS',
    'CISCO', 'BBN', 'OSPF', 'BGP', 'BOOTP',
    'AUTO_STAT', 'STATIC', 'NOT_DOD' );   }

  MibIpAddrPrimary = 'Primary';
  MibIpAddrDynamic = 'Dynamic';
  MibIpAddrDisconnected = 'Disconnected';
  MibIpAddrDeleted = 'Being Deleted';
  MibIpAddrTransient = 'Transient';
  MibIpAddrDnsEligible = 'Published in DNS';

 // TInterfaceAndOperStatusFlags literals
  MibIfoHardwareInterface = 'Hardware';
  MibIfoFilterInterface = 'Filter';
  MibIfoConnectorPresent = 'Connector Present';
  MibIfoNotAuthenticated = 'Not Authenticated';
  MibIfoNotMediaConnected = 'Not Media Connected';
  MibIfoPaused = 'Paused';
  MibIfoLowPower = 'Low Power';
  MibIfoEndPointInterface = 'End Point';

  IpPrefixOrigins : array[0..4] of string =
      ('Other','Manual','Well Known','Dhcp','Router Advert');

  IpSuffixOrigins : array[0..5] of string =
    ('Other','Manual','Well Known','Dhcp','Link Layer','Random');

  DadStates : array[0..4] of string =
    ('Invalid','Tentative','Duplicate','Deprecated','Preferred');

  IfOperStatuses : array[0..7] of string =
    ('None', 'Up', 'Down', 'Testing', 'Unknown', 'Dormant', 'Not Present', 'Lower Layer Down');

  AdminStatuses : array[0..3] of string =
    ('None', 'Up', 'Down', 'Testing') ;

  NdisMediums : array[0..19] of string =
    ('Ethernet 802.3','Token Ring 802.5','FDDI','WAN','LocalTalk','DIX','Arcnet','Arcnet 878.2','ATM','Wireless WAN',
    'IrDA','Broadcast PC','CoWan','Firewire 1394','InfiniBand','Tunnel','Native 802.11','Loopback','WiMax','IP' );

  NdisPhysicalMediums : array[0..20] of string =
    ('Unspecified','Wireless LAN','Cable Modem','Phone Line','Power Line','xDSL','Fibre Channel',
    '1394 bus','Wireless WAN','Native 802.11','Bluetooth','Infiniband','WiMax','UWB','Ethernet 802.3',
    'Toekn Ring 802.5','IrDA','Wired WAN','Wired CoWan','Other','');

  TunnelTypes : array[0..14] of string =
    ('None','Other', 'Direct', 'u3', 'u4', 'u5', 'u6', 'u7', 'u8', 'u9', 'u10',
    '6to4', 'u12', 'ISATAP', 'Teredo') ;

  NetIfAccessTtypes : array[0..5] of string =
    ('Unknown','Loopback','Broadcast','Point to Point','Point to Multi Point','');

  NetIfDirectionTypes : array[0..3] of string =
    ('Send and Receive','Send Only','Receive Only',''  );

  NetIfConnectionTypes : array[0..4] of string =
    ('Unknown','Dedicated','Passive','Demand','');

  NetIfMediaConnectStates : array[0..2] of string =
    ('Unknown','Connected','Disconnected'  );

  NetNeighborStates : array[Low(TNeighborState)..High(TNeighborState)] of string =
    ('Unreachable', 'Incomplete', 'Probe', 'Delay', 'Stale', 'Reachable', 'Permanent', 'Maximum');

  NetRouteInfo: array[0..16] of TRouteInfo = (
    (Id: MibIpProtoOther; Str: 'Not Specified'),
    (Id: MibIpProtoLocal; Str: 'Local Interface'),
    (Id: MibIpProtoNetMgmt; Str: 'Static Route'),
    (Id: MibIpProtoIcmp; Str: 'ICMP Rediect'),
    (Id: MibIpProtoEgp; Str: 'EGP'),
    (Id: MibIpProtoGgp; Str: 'GGP'),
    (Id: MibIpProtoHello; Str: 'Hellospeak'),
    (Id: MibIpProtoRip; Str: 'RIP'),
    (Id: MibIpProtoIsIs; Str: 'IS-IS'),
    (Id: MibIpProtoEsIs; Str: 'ES-IS'),
    (Id: MibIpProtoCisco; Str: 'Cisco IGRP'),
    (Id: MibIpProtoBbn; Str: 'BBN'),
    (Id: MibIpProtoOspf; Str: 'OSPF'),
    (Id: MibIpProtoBgp; Str: 'BGP'),
    (Id: MibIpProtoNtAutostatic; Str: 'Auto Static'),
    (Id: MibIpProtoNTStatic; Str: 'Static'),
    (Id: MibIpProtoNTStaticNonDod; Str: 'Static No DoD')  );

    NetRouteOrigins: array[Low(TRouteOrigin)..High(TRouteOrigin)] of string =
        ('Manual','Well Known','DHCP','Router Advert','6to4');

    NetTransProto: array[Low(TTransProt)..High(TTransProt)] of string =
        ('TCP and UDP', 'TCP', 'UDP');        // May 2023

type
// for IpHlpNetworkParams
  TNetworkParams = record
    HostName: string ;
    DomainName: string ;
    CurrentDnsServer: string ;
    DnsServerTot: integer ;
    DnsServerNames: array [0..9] of string ;
    NodeType: UINT;
    ScopeID: string ;
    EnableRouting: Boolean;    // June 2023 was Uint
    EnableProxy: Boolean;
    EnableDNS: Boolean;
  end;

// for IpHlpIfTable and IpHlpIfTable2
  TIfRows = array of TMibIfRow ; // dynamic array of rows

  TIfRow2 = record      // Nov 2014
    Mib: TMibIfRow2;
    InterfaceName: WideString ;
    Description: WideString ;
    FriendlyName: WideString ;
    MacAddress: string ;       // May 2023
    MacVendor: String;         // May 2023
  end;
  TIfRows2 = array of TIfRow2 ; // dynamic array of rows

// for IpHlpAdaptersInfo
  TAdaptorInfo = record
    AdapterName: WideString ;  // 14 Jan 2009, was string
    Description: WideString ;  // 14 Jan 2009, was string
    MacAddress: string ;
    MacVendor: String;         // May 2023
    Index: DWORD;
    aType: UINT;
    DHCPEnabled: Boolean;      // May 2023 was Int
    CurrIPAddress: string ;
    CurrIPMask: string ;
    IPAddressTot: integer ;
    IPAddressList: array of string ;
    IPMaskList: array of string ;
    GatewayTot: integer ;
    GatewayList: array of string ;
    DHCPTot: integer ;
    DHCPServer: array of string ;
    HaveWINS: BOOL;
    PrimWINSTot: integer ;
    PrimWINSServer: array of string ;
    SecWINSTot: integer ;
    SecWINSServer: array of string ;
    LeaseObtained: LongInt ; // UNIX time, seconds since 1970
    LeaseExpires: LongInt;   // UNIX time, seconds since 1970
    AutoConfigEnabled: UINT ;  // next 4 from IP_Per_Adaptor_Info, W2K and later
    AutoConfigActive: UINT ;
    CurrentDNSServer: string ;
    DNSServerTot: integer ;
    DNSServerList: array of string ;
// following from GetAdaptersAddresses for Vista and later, a few for XP
    AnycastIPAddrList: array of string ;
    AnycastIPAddrTot: integer ;
    MulticastIPAddrList: array of string ;
    MulticastIPAddrTot: integer ;
    PrefixIPAddrList: array of string ;
    PrefixTot: Integer ; //  Nov 2014
    PrefixMaskList: array of string ;  //  Nov 2014
    FriendlyName: WideString ;
    Mtu: DWORD;
    IfType: DWORD;
    OperStatus: TIfOperStatus;
    Ipv6Index: DWORD;
    XmitLinkSpeed: Int64;
    RecvLinkSpeed: Int64;
    Ipv4Metric: ULONG;
    Ipv6Metric: ULONG;
    Luid: TIFLuid;
    CompartmentId: TNetIfCompartmentId;
    NetworkGuid: TNetIfNetworkGuid;
    ConnectionType: TNetIfConnectionType;
    TunnelType: TTunnelType;
    InterfaceName: WideString ;   // Nov 2014
    DnsSuffix: string;  // Nov 2014 was missing
    Flags: DWORD;  // Mov 2014 -  IP_ADAPTER_xxx flags
  end ;

  TAdaptorRows = array of TAdaptorInfo ;

// for IpHlpTCPTable and IpHlpUdpTable
  TConnInfo = record
    State: Integer ;
    Protocol: String;         // May 2023
    LocalAddr: String ;
    LocalPort: Integer ;
    RemoteAddr: String ;
    RemotePort: Integer ;
    ProcessID: DWORD ;
    LocalHost: string ;    // may be used by application for reverse lookup
    RemoteHost: string ;   // may be used by application for reverse lookup
    LocalPortDesc: String;    // May 2023
    RemotePortDesc: String;   // May 2023
    DispRow: integer ;
    ProcName: WideString ;    // 15 Jan 2009 - Unicode
    CreateDT: TDateTime ;     // 15 Jan 2009
    LocSockAddr: TSockAddrIn6; // TSockAddrInet;   // Nov 2014
    RemSockAddr: TSockAddrIn6; // TSockAddrInet;   // Nov 2014
  end;

  TConnRows = array of TConnInfo ;

// IP address record, IPv4 and IPv6, binary and string versions - Nov 2014
   TIpType = (IpTypeUnicast, IpTypeAnycast, IpTypeMulticast);

  TIpAddrInfo = record
    IpAddress: string ;
    IpMask: string ;
    IpType: TIpType ;
    TypeStr: string ;
    SockAddr: TSockAddrIn6; // TSockAddrInet ;
    MacAddress: string ;       // May 2023
    MacVendor: String;         // May 2023
    IFLuid: TNetLuid ;
    IFIndex: TNetIfIndex ;
    InterfaceName: WideString ;
    Description: WideString ;
    FriendlyName: WideString ;
    PrefixOrig: TIpPrefixOrigin ;
    SuffixOrig: TIpSuffixOrigin ;
    ValidSecs: Integer ;
    DupliState: TIpDadState ;
    IpScopeId: TScopeID ;
    CreationDT: TDateTime ;
  end;
  TIpAddrInfos = array of TIpAddrInfo ;

// IP Neighbourhood address cache, modern version of APR, records expire
  TNeighbRow = record
    IpAddress: String ;
    SockAddr: TSockAddrIn6;
    MacAddress: String ;
    MacVendor: String;
    RandomMac: Boolean;
    IFLuid: TNetLuid;
    IFIndex: TNetIfIndex;
    InterfaceName: String;
    IfType: Integer;
    IfTypeDesc: String;
    State: TNeighborState;
    StateDesc: String;
    Flags: Byte;
    ReachSecs: Integer;
  end;
  TNeighbRows = array of TNeighbRow;

// IP forward row 2
  TIpRouteRow = record
    DestIpAddr: string ;
    DestMask: string;
    DestinationPrefix: TIpAddressPrefix;
    NextIpAddr: string ;
    NextHop: TSockAddrIn6;
    IFLuid: TNetLuid;
    IFIndex: TNetIfIndex;
    InterfaceName: String;
    IfType: Integer;
    IfTypeDesc: String;
    SitePrefixLength: Byte;
    ValidLifetime: ULONG;
    PreferredLifetime: ULONG;
    Metric: ULONG;
    Protocol: ULONG; // TRouteProtocol;
    ProtDesc: String;
    Loopback: BOOLEAN;
    AutoconfigureAddress: BOOLEAN;
    Publish: BOOLEAN;
    Immortal: BOOLEAN;
    Age: ULONG;
    Origin: TRouteOrigin;
    OrigDesc: String;
  end;
  TIpRouteRows = array of TIpRouteRow;

  TIpPathRow = Record
    SourceIpAddr: String;
    Source: TSockAddrIn6;
    DestIpAddr: String;
    Destination: TSockAddrIn6;
    IFLuid: TNetLuid;
    IFIndex: TNetIfIndex;
    InterfaceName: String;
    IfType: Integer;
    IfTypeDesc: String;
    CurNextHopIpAddr: String;
    CurrentNextHop: TSockAddrIn6;
    PathMtu: ULONG;
    RttMean: ULONG;
    RttDeviation: ULONG;
    ReachSecs: Integer;
    IsReachable: Boolean;
    LinkTransmitSpeed: Int64;
    LinkReceiveSpeed: Int64;
 end;
  TIpPathRows = array of TIpPathRow;

  TAddrList = record     // multiple IP addresses for a MAC device
    IpAddr: String;
    IpIn6: TSockAddrIn6;
    HostName: String;
    HostTick: Int64;   // when lookup started
  end;

// Neighbourhood devices MAC historic addresses, doesn't expire
  TNeighbDevice = record
    MacAddress: String;
    MacVendor: String;
    RandomMac: Boolean;
    AddrList: array of TAddrList;
    TotAddrNr: Integer;
    LastState: TNeighborState;
    StateDesc: String;
    FirstDT: TDateTime;
    LastDT: TDateTime;
    Detections: Integer;
  end;
  TNeighbDevices = array of TNeighbDevice;

const
// column titles and widths for various ListViews
  NeighbDevRowHdrs: array[0..7] of String =
     ('Local IP','Host','Remote MAC','Vendor','State','First Seen','Last Seen','Seen Count' );
  NeighbDevRowWid: array[0..7] of Integer =
     (190,210,160,140,100,150,160,100);

  IpConnsRowHdrs: array[0..6] of String =
     ('Proto','Local IP and Port','Remote IP and Port','Host Name','State','Process Id and Name','Created' );
  IpConnsRowWid: array[0..6] of Integer =
     (60,250,250,250,100,200,160);

  IpAdptRowHdrs: array[0..21] of String =
     ('Index','Interface','Description','Friendly Name','Type', 'MAC Address','Vendor','DHCP',
     'Tot IPs','IP Addresse(s)','IP Prefixe(s)','DNS Server(s)','Gateway(s)','DHCP Server', 'WINS Server',
     'Metric','Op Status','DND Suffix','Xmit Speed','Conn Type', 'Tunnel', 'GUID'  );
  IpAdptRowWid: array[0..21] of Integer =
     (45,100,250,200,70,160,140,50,  50,400,200,200,200,100,100,   60,80,80,80,80,80,270  );

  IpIfaceRowHdrs: array[0..22] of String =
     ('Index', 'Interface', 'Description', 'Friendly Name', 'Type', 'MAC Address','Vendor',
     'Tot IPs','IP Addresse(s)', 'MTU', 'Speed', 'Admin St', 'Oper Status', 'Media Type',  'Phys Medium','Access Type',
     'Direction', 'Interface/Oper Status', 'Conn State', 'Conn Type', 'In Octets', 'Out Octets', 'Tunnel' );
  IpIfaceRowWid: array[0..22] of Integer =
     (45,100,250,200,70,160,140,   50,400,60,80,80,80,100,100,100,   120,140,90,90,100,100,80  );

  IpAddrRowHdrs: array[0..7] of String =
    ('IP Address', 'IP Mask', 'Type', 'Interface', 'MAC Address','Vendor','Description', 'Friendly Name');
  IpAddrRowWid: array[0..7] of Integer =
    (230,100,150,120,160,140,250,200 );

  IpPathRowHdrs: array[0..9] of String =
    ('Local IP Address', 'Remote IP Address', 'Next Hop IP Address', 'MTU', 'RTT', 'Reach', 'Link Xmit', 'Link Recv', 'Interface','IF Type');
  IpPathRowWid: array[0..9] of Integer =
    (220,220,220,60,60,60,100,100,200,100);

  IpRoutingRowHdrs: array[0..7] of String =
    ('Destination Prefix', 'Mask/Length', 'Next Hop IP Address', 'Route Protocol', 'Metric', 'Route Origin', 'Interface','IF Type');
  IpRoutingRowWid: array[0..7] of Integer =
    (220,100,220,120,60,100,200,100);

  NeighbRowHdrs: array[0..6] of String =
       ('Remote MAC','Vendor','Remote IP Address','State','Interface','IF Type','Reach Secs');
  NeighbRowWid: array[0..6] of Integer =
    (160,140,220,100,200,100,120);


type
  TIcsNeighbDevThread = class;     // forward
  TNeighbDevLogEvent = procedure (Sender: TObject; const Msg: string) of object;

  TIcsNeighbDevices = class(TComponent)  // class(TIcsWndControl)  // May 2023
  protected
    FDevThread: TIcsNeighbDevThread;
    FDevCritSect: TIcsCriticalSection;
    FIcsDNCache: TIcsDomainNameCache;
    FActive: Boolean;
    FDevices: TNeighbDevices;
    FDevMacIdx: TStringList;
    FDevIpIdx: TStringList;
    FDeviceTot: Integer;
    FDevFamily: TAddressFamily;
    FLocalIps: Boolean;
    FLocIpv4: String;
    FLocIpv6: String;
    FInterfaceLuid: TNetLuid;
    FScanStartIPv4: String;
    FScanStartIPv6: String;
    FScanTotIps: Integer;
    FUpdScanSecs: Integer;    // seconds between IP scans
    FUpdCacheSecs: Integer;   // seconds between cache updates
    FOnLogEvent: TNeighbDevLogEvent;
    FOnDevUpd: TNotifyEvent;
    function GetMacTot: Integer;
    function GetIpTot: Integer;
    function GetMacRow(Idx: Integer): Integer;
    function GetIpRow(Idx: Integer; var AddrNr: Integer): Integer;
    function FindAddr(const IP: String; Idx: Integer): Integer;
    procedure LogEvent (const Info: String);
    procedure DNUpdateEvent(Sender: TObject; ItemNr: Integer);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure EmptyCache;
    procedure DispMacDevs(List: TStrings);
    procedure DispIpDevs(List: TStrings);
    function GetIpList(Idx: Integer): String;
    procedure StartMonitor;
    procedure StopMonitor;
    property Active: Boolean                        read FActive;
    property NeighbDevices: TNeighbDevices          read FDevices;
    property MacTot: Integer                        read GetMacTot;
    property IpTot: Integer                         read GetIpTot;
    property MacRow[Idx: Integer]: Integer          read GetMacRow;
    property IpRow[Idx: Integer; var AddrNr: Integer]: Integer  read GetIpRow;
  published
    property DevFamily: TAddressFamily         read FDevFamily write FDevFamily;
    property LocalIps: Boolean                 read FLocalIps write FLocalIps;
    property LocIpv4: String                   read FLocIpv4 write FLocIpv4;
    property LocIpv6: String                   read FLocIpv6 write FLocIpv6;
    property InterfaceLuid: TNetLuid           read FInterfaceLuid write FInterfaceLuid;
    property IcsDNCache: TIcsDomainNameCache   read FIcsDNCache write FIcsDNCache;
    property ScanStartIPv4: String             read FScanStartIPv4 write FScanStartIPv4;
    property ScanStartIPv6: String             read FScanStartIPv6 write FScanStartIPv6;
    property ScanTotIps: Integer               read FScanTotIps write FScanTotIps;
    property UpdScanSecs: Integer              read FUpdScanSecs write FUpdScanSecs;
    property UpdCacheSecs: Integer             read FUpdCacheSecs write FUpdCacheSecs;
    property OnLogEvent: TNeighbDevLogEvent    read FOnLogEvent write FOnLogEvent;
    property OnDevUpd: TNotifyEvent            read FOnDevUpd write FOnDevUpd;
  end;

  TIcsNeighbDevThread = class(TThread)
  private
    FNeighbComp: TIcsNeighbDevices;
    FUpdRec: Boolean;
    FLogInfo: String;
    FCacheIP: String;
    FCacheTag: Integer;
    procedure UpdateRow(NeighbRow: TNeighbRow);
    procedure UpdateFromCache;
    procedure UpdateFromAdaptor;
    function ScanAddresses(StartIp: String; TotIps: Integer = 1): Integer;
    procedure ThreadLogEvent (const Info: String);
    procedure CacheLookupStart;
    procedure CallThreadEvent;
  public
     procedure Execute; override;
  end;

  TIpChangesEvent = Procedure (IpAddrInfo: TIpAddrInfo;  NotificationType: TMibNoticationType) of object ;

  TIcsIpChanges = class(TIcsWndControl)
  protected
    FNotificationHandle: THandle;
    FIpChangesEvent: TIpChangesEvent;
    FDevFamily: TAddressFamily;
    FMsg_WM_IPADDRCHANGE: UINT;
    procedure WndProc(var MsgRec: TMessage); override;
    function  MsgHandlersCount: Integer; override;
    procedure AllocateMsgHandlers; override;
    procedure FreeMsgHandlers; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function StartMonitor: Integer;
    procedure StopMonitor;
  published
    property DevFamily: TAddressFamily          read FDevFamily write FDevFamily;
    property OnIpChangesEvent: TIpChangesEvent  read FIpChangesEvent write FIpChangesEvent;
  end;


//---------------exported stuff-----------------------------------------------

function IpHlpAdaptersInfo(var AdpTot: integer;var AdpRows: TAdaptorRows): integer ;
procedure Get_AdaptersInfo( List: TStrings );
function IpHlpNetworkParams (var NetworkParams: TNetworkParams): integer ;
procedure Get_NetworkParams( List: TStrings );
procedure Get_ARPTable( List: TStrings );
function IpHlpConnsTable(var ConnRows: TConnRows; Family: TAddressFamily = AF_INET; Prot: TTransProt = ProtoBoth): integer ;   // May 2023
procedure List_ConnTable(ConnRows: TConnRows; List: TStrings );                                                                // May 2023
procedure Get_ConnsTable( List: TStrings; Family: TAddressFamily = AF_INET; Prot: TTransProt = ProtoBoth);                     // May 2023
function IpHlpTCPTable(var ConnRows: TConnRows; Family: TAddressFamily = AF_INET; StartRow: Integer = 0): integer ;
procedure Get_TCPTable( List: TStrings );
function IpHlpTCPStatistics (var TCPStats: TMibTCPStats): integer ;
procedure Get_TCPStatistics( List: TStrings );
function IpHlpUDPTable(var ConnRows: TConnRows; Family: TAddressFamily = AF_INET; StartRow: Integer = 0): integer ;
procedure Get_UDPTable( List: TStrings );
function IpHlpUdpStatistics (UdpStats: TMibUDPStats): integer ;
procedure Get_UDPStatistics( List: TStrings );
procedure Get_IPAddrTable( List: TStrings );
procedure Get_IPForwardTable( List: TStrings );
function IpHlpIPStatistics (var IPStats: TMibIPStats): integer ;
procedure Get_IPStatistics( List: TStrings );
function Get_RTTAndHopCount( IPAddr: DWORD; MaxHops: Longint; var RTT: longint; var HopCount: longint ): integer;
procedure Get_ICMPStats( ICMPIn, ICMPOut: TStrings );
function IpHlpIfTable2(var IfTot: integer; var IfRows2: TIfRows2): integer ;
procedure Get_IfTable2( List: TStrings );
function IpHlpIfEntry(Index: integer; var IfRow: TMibIfRow): integer ;
function IpHlpAdaptersAddr(Family: TAddressFamily; var AdpTot: integer; var AdpRows: TAdaptorRows): integer ;
function GetIpAddrType (wtype: DWORD): string;
function GetIfoFlags (Flags: TInterfaceAndOperStatusFlags): string ;
function IpHlpIpAddrTable(var IpAddrInfos: TIpAddrInfos; Family: TAddressFamily = AF_INET;
                                   AllIps: Boolean = True; Names: Boolean = True; AdptIdx: TNetIfIndex = 0): integer ;
procedure Get_IPNeighbourTable( List: TStrings; PermAddr: Boolean = False);               // May 2023
function IpHlpIpNeighbTable(var NeighbRows: TNeighbRows; Family: TAddressFamily = AF_INET): DWORD;  // May 2023  // AF_UNSPEC for IPv4 and IPv6
//procedure IpHlpIpNeighbDevsClear;  // May 2023
function IpHlpIpNeighbTabFlush(Family: TAddressFamily = AF_INET): DWORD;  // May 2023  // AF_UNSPEC for IPv4 and IPv6
function IpHlpConvIntLuidToStr (const InterfaceLuid: TNetLuid): WideString ;
function IpHlpConvIntIdxToStr (const InterfaceIndex: TNetIfIndex): WideString ;
function IpHlpIPForwardTable(var IpRouteRows: TIpRouteRows; Family: TAddressFamily = AF_INET): DWORD;   // AF_UNSPEC for IPv4 and IPv6  // May 2023
function IpHlpGetRouteProtocol(Id: Integer): String;
procedure Get_IpPathTable(List: TStrings);                // May 2023
function IpHlpIpPathTable(var IpPathRows: TIpPathRows; Family: TAddressFamily = AF_INET): DWORD;  // May 2023  // AF_UNSPEC for IPv4 and IPv6
function IpHlpConvIntLuidToAlias (const InterfaceLuid: TNetLuid): WideString ;  // May 2023 more friendly network name
function IpHlpResolveIpNet (const RemIp, LocIp: String; InterfaceLuid: TNetLuid; var NeighbRow: TNeighbRow): integer ;   // May 2023
function IpHlpAdpforIP (const IpAddr: String): TNetLuid;   // May 2023
function IcsGetYN(Value: Boolean): String;
function IpHlpGetDnsServers(DnsList: TStrings): Boolean;   // July 2023


function LoadIpHlp: Boolean;

var
    ShowExePath: boolean = false ;

implementation

//--------------General utilities-----------------------------------------------

function IcsGetYN(Value: Boolean): String;
begin
    if Value then
        Result := 'Y'
    else
        Result := 'N';
end;


{ extracts next "token" from string, then eats string }
function NextToken( var s: string; Separator: char ): string;
var
  Sep_Pos       : byte;
begin
  Result := '';
  if length( s ) > 0 then begin
    Sep_Pos := pos( Separator, s );
    if Sep_Pos > 0 then begin
      Result := copy( s, 1, Pred( Sep_Pos ) );
      Delete( s, 1, Sep_Pos );
    end
    else begin
      Result := s;
      s := '';
    end;
  end;
end;

//------------------------------------------------------------------------------
{ concerts physical numerical MAC-address to ww-xx-yy-zz string, up to 8 bytes, maybe more }
{ beware not the same as TMacAddr which is 6 bytes }
function PhysMacAddr2Str(MacAddr: array of byte; size: integer ): string;
var
    i: integer;
    blank: boolean;
begin
    if (Size = 0) or (Length (MacAddr) < size) then
    begin
        Result := 'Blank';
        exit;
    end
    else
        Result := '';
    blank := true ;
    for i := 0 to Size - 1 do   // Feb 2016 corrected to base 0
    begin
        if MacAddr[i] <> 0 then blank := false ;
        Result := Result + IntToHex( MacAddr[i], 2 );
        if i < size - 1 then Result := Result + '-';
    end;
    if blank then Result := 'Blank';
end;


//------------------------------------------------------------------------------
// create IPv4 subnet mask from prefix length, 30=255.255.255.0, etc
function IpHlpCreateMask (len: Integer): string ;
var
    mask: ULONG;
begin
    result := '' ;
    if ConvertLengthToIpv4Mask(Len, mask) <> NOERROR then   // May 2023
        Exit;
    Result := WSocketIPv4ToStr (TIcsIPv4Address(mask)) ;
end;


//------------------------------------------------------------------------------
function IpHlpConvIntLuidToStr (const InterfaceLuid: TNetLuid): WideString ;
var
    Buffer: array[0..MAX_ADAPTER_NAME_LENGTH] of WideChar ;
begin
    result := '' ;
    if NOT Assigned (ConvertInterfaceLuidToNameW) then Exit;
    if ConvertInterfaceLuidToNameW (@InterfaceLuid, Buffer, MAX_ADAPTER_NAME_LENGTH) <> 0 then exit ;
    Result := String (Buffer) ;
end ;


//------------------------------------------------------------------------------
function IpHlpConvIntLuidToAlias (const InterfaceLuid: TNetLuid): WideString ;  // May 2023 more friendly network name
var
    Buffer: array[0..MAX_ADAPTER_NAME_LENGTH] of WideChar ;
begin
    result := '' ;
    if NOT Assigned (ConvertInterfaceLuidToNameW) then Exit;
    if ConvertInterfaceLuidToAlias (@InterfaceLuid, Buffer, MAX_ADAPTER_NAME_LENGTH) <> 0 then exit ;
    Result := String (Buffer) ;
end ;


//------------------------------------------------------------------------------
function IpHlpConvIntIdxToStr (const InterfaceIndex: TNetIfIndex): WideString ;
var
    InterfaceLuid: TNetLuid;
begin
    result := '' ;
    if NOT Assigned (ConvertInterfaceIndexToLuid) then Exit;
    if ConvertInterfaceIndexToLuid (InterfaceIndex, @InterfaceLuid) <> 0 then exit ;
    Result := IpHlpConvIntLuidToStr (InterfaceLuid) ;
end ;

//------------------------------------------------------------------------------
{ converts dotted decimal IP-address to network byte order DWORD}
function Str2IpAddr( IPStr: string ): DWORD;
var
  i             : integer;
  Num           : DWORD;
begin
  Result := 0;
  for i := 1 to 4 do
  try
    Num := ( StrToInt( NextToken( IPStr, '.' ) ) ) shl 24;
    Result := ( Result shr 8 ) or Num;
  except
    Result := 0;
  end;

end;

//------------------------------------------------------------------------------
{ converts port number in network byte order to DWORD }
function Port2Wrd( nwoPort: DWORD ): DWORD;
begin
  Result := Swap( WORD( nwoPort ) );
end;

//------------------------------------------------------------------------------
{ converts port number in network byte order to string }
function Port2Str( nwoPort: DWORD ): string;
begin
  Result := IntToStr( Port2Wrd( nwoPort ) );
end;

//------------------------------------------------------------------------------
function FileTimeToInt64 (const FileTime: TFileTime): Int64 ;
begin
    Move (FileTime, result, SizeOf (result)) ;
end;

//------------------------------------------------------------------------------
const
  FileTimeBase = -109205.0;   // days between years 1601 and 1900
  FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; // 100 nsec per Day

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
begin
    Result := FileTimeToInt64 (FileTime) / FileTimeStep ;
    Result := Result + FileTimeBase ;
end;

//------------------------------------------------------------------------------
function GetIpAddrType (wtype: DWORD): string;

    procedure buildres (lit: string);
    begin
      if result <> '' then
        result := result + ', ';
      result := result + lit;
    end ;

begin
    result := '';
    if wtype = 0 then exit;
    if wtype AND MIB_IPADDR_PRIMARY <> 0 then
        buildres (MibIpAddrPrimary);
    if wtype AND MIB_IPADDR_DYNAMIC <> 0 then
        buildres (MibIpAddrDynamic);
    if wtype AND MIB_IPADDR_DISCONNECTED <> 0 then
        buildres (MibIpAddrDisconnected);
    if wtype AND MIB_IPADDR_DELETED <> 0 then
        buildres (MibIpAddrDeleted);
    if wtype AND MIB_IPADDR_TRANSIENT <> 0 then
        buildres (MibIpAddrTransient);
    if wtype AND MIB_IPADDR_DNS_ELIGIBLE <> 0 then
        buildres (MibIpAddrDnsEligible);
end ;


//------------------------------------------------------------------------------
function GetIfoFlags (Flags: TInterfaceAndOperStatusFlags): string ;

    procedure buildres (lit: string);
    begin
      if result <> '' then
        result := result + ', ';
      result := result + lit;
    end ;

begin
    result := '';
    if HardwareInterface in Flags then
        buildres (MibIfoHardwareInterface);
    if FilterInterface in Flags then
        buildres (MibIfoFilterInterface);
    if ConnectorPresent in Flags then
        buildres (MibIfoConnectorPresent);
    if NotAuthenticated in Flags then
        buildres (MibIfoNotAuthenticated);
    if NotMediaConnected in Flags then
        buildres (MibIfoNotMediaConnected);
    if Paused in Flags then
        buildres (MibIfoPaused);
    if LowPower in Flags then
        buildres (MibIfoLowPower);
    if EndPointInterface in Flags then
        buildres (MibIfoEndPointInterface);
end ;

//-----------------------------------------------------------------------------
{ general,  fixed network parameters }
procedure Get_NetworkParams( List: TStrings );
var
    NetworkParams: TNetworkParams ;
    I, ErrorCode: integer ;
begin
    if not Assigned( List ) then EXIT;
    List.Clear;
    ErrorCode := IpHlpNetworkParams (NetworkParams) ;
    if ErrorCode <> 0 then
    begin
        List.Add (SysErrorMessage (ErrorCode));
        exit;
    end ;
    with NetworkParams do
    begin
        List.Add( 'Hostname          : ' + HostName );
        List.Add( 'Domain            : ' + DomainName );
        List.Add( 'DHCP Scope        : ' + ScopeID );
        List.Add( 'NETBIOS Node Type : ' + NETBIOSTypes[NodeType] );
        List.Add( 'Routing Enabled   : ' + IcsGetYN( EnableRouting ) );
        List.Add( 'Proxy   Enabled   : ' + IcsGetYN( EnableProxy ) );
        List.Add( 'DNS     Enabled   : ' + IcsGetYN( EnableDNS ) );
        if DnsServerTot <> 0 then
        begin
            for I := 0 to Pred (DnsServerTot) do
                List.Add( 'DNS Server Addr   : ' + DnsServerNames [I] ) ;
        end ;
    end ;
end ;


//------------------------------------------------------------------------------
function IpHlpNetworkParams (var NetworkParams: TNetworkParams): integer ;
var
  FixedInfo     : PTFixedInfo;         // Angus
  InfoSize      : Longint;
  PDnsServer    : PIpAddrString ;   // Angus
begin
    InfoSize := 0 ;   // Angus
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then
        exit ;
    result := GetNetworkParams( Nil, @InfoSize );  // Angus
    if result <> ERROR_BUFFER_OVERFLOW then
        exit ; // Angus
    GetMem (FixedInfo, InfoSize) ;                    // Angus
    try
        result := GetNetworkParams( FixedInfo, @InfoSize );   // Angus
        if result <> ERROR_SUCCESS then
            exit ;
        NetworkParams.DnsServerTot := 0 ;
        with FixedInfo^ do begin
            NetworkParams.HostName := Trim (String (HostName)) ;      // 8 Aug 2010
            NetworkParams.DomainName := Trim (String (DomainName)) ;  // 8 Aug 2010
            NetworkParams.ScopeId := Trim (String (ScopeID)) ;        // 8 Aug 2010
            NetworkParams.NodeType := NodeType ;
            NetworkParams.EnableRouting := Boolean(EnableRouting) ;   // June 2023
            NetworkParams.EnableProxy := Boolean(EnableProxy) ;
            NetworkParams.EnableDNS := Boolean(EnableDNS) ;
            NetworkParams.DnsServerNames [0] := String (DNSServerList.IPAddress) ;  // 8 Aug 2010
            if NetworkParams.DnsServerNames [0] <> '' then
                NetworkParams.DnsServerTot := 1 ;
            PDnsServer := DnsServerList.Next;
            while PDnsServer <> Nil do begin
                NetworkParams.DnsServerNames [NetworkParams.DnsServerTot] := String (PDnsServer^.IPAddress) ;   // 8 Aug 2010
                inc (NetworkParams.DnsServerTot) ;
                if NetworkParams.DnsServerTot >= Length (NetworkParams.DnsServerNames) then exit ;
                PDnsServer := PDnsServer.Next ;
            end;
        end ;
    finally
       FreeMem (FixedInfo) ;                     // Angus
    end ;
end;


//------------------------------------------------------------------------------
// July 2023 get list of DNS servers defined for this PC
function IpHlpGetDnsServers(DnsList: TStrings): Boolean;
var
    IcsNetworkParams: TNetworkParams;
    I: Integer;
begin
    Result := False;
    if NOT Assigned(DnsList) then
        DnsList := TStringList.Create;
    DnsList.Clear;
    IpHlpNetworkParams(IcsNetworkParams);
    if (IcsNetworkParams.DnsServerTot > 0) then begin
        for I := 0 to IcsNetworkParams.DnsServerTot - 1 do
            DnsList.Add(IcsNetworkParams.DnsServerNames[I]) ;
        Result := True;
    end;
end;


//------------------------------------------------------------------------------
function ICMPErr2Str( ICMPErrCode: DWORD) : string;
begin
   Result := 'UnknownError : ' + IntToStr( ICMPErrCode );
   dec( ICMPErrCode, ICMP_ERROR_BASE );
   if ICMPErrCode in [Low(ICMpErr)..High(ICMPErr)] then
     Result := ICMPErr[ ICMPErrCode];
end;


//------------------------------------------------------------------------------
// interfaces on PC, similar to adaptors but no addresses
// include bytes in/out for each adaptor, Vista and later
function IpHlpIfTable2(var IfTot: integer; var IfRows2: TIfRows2): integer ;
var
    I: integer;
    pIfTable2: PTMibIfTable2;
begin
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    SetLength (IfRows2, 0) ;
    IfTot := 0 ;
    pIfTable2 := nil ;
 // get table pointer
    try
        result := GetIfTable2Ex (MibIfTableNormal, pIfTable2) ;
        if result <> NO_ERROR then exit ;
        IfTot := pIfTable2^.NumEntries ;
        if IfTot = 0 then exit ;
        SetLength (IfRows2, IfTot) ;
        for I := 0 to Pred (IfTot) do
        begin
            IfRows2 [I].Mib := pIfTable2^.Table [I] ;
            IfRows2 [I].FriendlyName := Trim(IfRows2 [I].Mib.Alias) ;
            IfRows2 [I].Description := Trim(IfRows2 [I].Mib.Description) ;
            IfRows2 [I].InterfaceName := IpHlpConvIntIdxToStr (IfRows2 [I].Mib.InterfaceIndex) ;
            IfRows2 [I].MacAddress :=  PhysMacAddr2Str(IfRows2 [I].Mib.PhysicalAddress, IfRows2 [I].Mib.PhysicalAddressLength);   // May 2023
            IfRows2 [I].MacVendor := IcsGetMacVendor(IfRows2 [I].MacAddress);    // May 2023
        end;
    finally
        FreeMibTable (pIfTable2) ;
    end;
end;


//-----------------------------------------------------------------------------
procedure Get_IfTable2( List: TStrings );
var
    IfRows2: TIfRows2 ;
    Error, I, J, NumEntries: integer ;
    IpAddrInfos: TIpAddrInfos ;
    S: string ;
begin
    if not Assigned( List ) then EXIT;
    List.Clear;
    Error := IpHlpIfTable2 (NumEntries, IfRows2) ;
    if (Error <> 0) then
        List.Add( SysErrorMessage( GetLastError ) )
    else if NumEntries = 0 then
        List.Add( 'no entries.' )
    else
    begin
        List.Add (Format ('%-8s|%-14s|%-40s|%-30s|%-10s|%-20s|%-20s|%-5s|%-12s|%-8s|%-11s|%-14s|%-14s|%-14s|%-16s|%-30s|%-12s|%-12s|%-12s|%-12s|%-8s',
               ['Index', 'Interface', 'Description', 'Friendly Name', 'Type', 'MAC Address','Vendor','MTU', 'Speed', 'Admin St', 'Oper Status',
               'Media Type',  'Phys Medium','Access Type', 'Direction', 'Interface/Oper Status', 'Conn State', 'Conn Type', 'In Octets', 'Out Octets', 'Tunnel' ] ) );
        List.Add('');
        for I := 0 to Pred (NumEntries) do
        begin
            with IfRows2 [I] do
            begin
                List.Add (Format (
                   '%0.8x|%-14s|%-40s|%-30s|%-10s|%-20s|%-20s|%5d|%12d|%-8s|%-11s|%-14s|%-14s|%-14s|%-16s|%-30s|%-12s|%-12s|%12d|%12d|%-8s',
                   [Mib.InterfaceIndex, InterfaceName, Copy (Description, 1, 40), Copy (FriendlyName, 1, 30), AdaptTypes[Mib.IfType],
                   MacAddress, MacVendor, Mib.MTU, Mib.TransmitLinkSpeed,                                                     // May 2023 vendor
                   AdminStatuses [Ord (Mib.AdminStatus)], IfOperStatuses [Ord (Mib.OperStatus)],
                   NdisMediums [Ord (Mib.MediaType)],  NdisPhysicalMediums [Ord (Mib.PhysicalMediumType)],
                   NetIfAccessTtypes [Ord (Mib.AccessType)], NetIfDirectionTypes [Ord (Mib.DirectionType)],
                   GetIfoFlags (Mib.InterfaceAndOperStatusFlags), NetIfMediaConnectStates [Ord (Mib.MediaConnectState)],
                   NetIfConnectionTypes  [Ord (Mib.ConnectionType)], Mib.InOctets, Mib.OutOctets, TunnelTypes [Ord (Mib.TunnelType)]] ) );

                if IpHlpIpAddrTable (IpAddrInfos, AF_UNSPEC, True, false, Mib.InterfaceIndex) = 0 then
                begin
                    if Length (IpAddrInfos) <> 0 then
                    begin
                        S := '' ;
                        for J := 0 to Pred (Length (IpAddrInfos)) do
                        begin
                            with IpAddrInfos [J] do
                            begin
                            S := S + IpAddress ;
                            if IPMask <> '' then
                                S := S + '=' + IPMask + ' | '
                            else
                                S := S + ' | ';
                            end;
                        end;
                        List.Add(IntToStr (Length (IpAddrInfos)) + ' IP Address(es): ' + S);
                        SetLength (IpAddrInfos, 0) ;  // free memory
                    end ;
                end;
                List.Add('');
            end;
        end ;
    end ;
    SetLength (IfRows2, 0) ;  // free memory
end ;


//-----------------------------------------------------------------------------
function IpHlpIfEntry(Index: integer; var IfRow: TMibIfRow): integer ;
begin
  result := ERROR_NOT_SUPPORTED ;
  if NOT LoadIpHlp then exit ;
  FillChar (IfRow, SizeOf (TMibIfRow), #0);  // clear buffer, since W98 does not
  IfRow.dwIndex := Index ;
  result := GetIfEntry (@IfRow) ;
end ;


//-----------------------------------------------------------------------------
{ Info on installed adapters, IPv4 and/or IPv6 addresses }
function IpHlpAdaptersAddr(Family: TAddressFamily; var AdpTot: integer; var AdpRows: TAdaptorRows): integer ;
var
  BufLen   : DWORD;
  PBuf     : PAnsiChar ;
  I        : integer ;
  len      : integer ;
  Flags : LongWord ;
  AdapterAddresses : PIpAdapterAddresses;
  UnicastAddress   : PIpAdapterUnicastAddress;
  AnycastAddress   : PIpAdapterAnycaseAddress;
  MulticastAddress : PIpAdapterMulticastAddress;
  DnsServerAddress : PIpAdapterDnsServerAddress;
  PrefixAddress    : PIpAdapterPrefix;  // aka mask
  WinsServerAddress: PIpAdapterWinsServerAddress;
  GatewayAddress   : PIpAdapterGatewayAddress;
  AdapterAddressLen: integer;
begin
  result := ERROR_NOT_SUPPORTED ;
  if NOT LoadIpHlp then exit ;
  SetLength (AdpRows, 4) ;
  AdpTot := 0 ;
  BufLen := 0 ;
  if NOT Assigned (GetAdaptersAddresses) then Exit;

  Flags := GAA_FLAG_INCLUDE_PREFIX ;
  Flags := Flags OR GAA_FLAG_INCLUDE_GATEWAYS or GAA_FLAG_INCLUDE_WINS_INFO or GAA_FLAG_INCLUDE_ALL_INTERFACES ;  // gateway, etc are Vista and later
  result := GetAdaptersAddresses ( Family, Flags, Nil, Nil, @BufLen) ;
  if (result <> ERROR_BUFFER_OVERFLOW) then exit ;
  GetMem( pBuf, BufLen );
  try
      FillChar (pBuf^, BufLen, #0);  // clear buffer
      result := GetAdaptersAddresses ( Family, Flags, Nil, PIpAdapterAddresses(PBuf), @BufLen );
      if result = NO_ERROR then
      begin
         AdapterAddresses := PIpAdapterAddresses(PBuf) ;
         while ( AdapterAddresses <> nil ) do
         begin
            AdapterAddressLen := AdapterAddresses^.Union.Length ; // 144 for XP SP3, 376 for Win7
            AdpRows [AdpTot].IPAddressTot := 0 ;
            SetLength (AdpRows [AdpTot].IPAddressList, 2) ;
            SetLength (AdpRows [AdpTot].IPMaskList, 2) ;
            AdpRows [AdpTot].GatewayTot := 0 ;
            SetLength (AdpRows [AdpTot].GatewayList, 2) ;
            AdpRows [AdpTot].DHCPTot := 0 ;
            SetLength (AdpRows [AdpTot].DHCPServer, 2) ;
            AdpRows [AdpTot].PrimWINSTot := 0 ;
            SetLength (AdpRows [AdpTot].PrimWINSServer, 2) ;
            AdpRows [AdpTot].SecWINSTot := 0 ;
            SetLength (AdpRows [AdpTot].SecWINSServer, 2) ;
            AdpRows [AdpTot].DNSServerTot := 0 ;
            SetLength (AdpRows [AdpTot].DNSServerList, 2) ;
            AdpRows [AdpTot].DNSServerList [0] := '' ;
            AdpRows [AdpTot].AnycastIPAddrTot := 0 ;
            SetLength (AdpRows [AdpTot].AnycastIPAddrList, 2) ;
            AdpRows [AdpTot].MulticastIPAddrTot := 0 ;
            SetLength (AdpRows [AdpTot].MulticastIPAddrList, 2) ;
            AdpRows [AdpTot].PrefixTot := 0 ;
            SetLength (AdpRows [AdpTot].PrefixIPAddrList, 2) ;
            SetLength (AdpRows [AdpTot].PrefixMaskList, 2) ;
            AdpRows [AdpTot].CurrIPAddress := NULL_IP;
            AdpRows [AdpTot].CurrIPMask := NULL_IP;
            AdpRows [AdpTot].AdapterName := Trim (WideString (AdapterAddresses^.AdapterName)) ; // 8 Aug 2010
            AdpRows [AdpTot].Description := Trim (AdapterAddresses^.Description) ;
            AdpRows [AdpTot].FriendlyName := Trim (AdapterAddresses^.FriendlyName) ;
            AdpRows [AdpTot].MacAddress := PhysMacAddr2Str( TPhysMacAddr(AdapterAddresses^.PhysicalAddress ), AdapterAddresses^.PhysicalAddressLength ) ;
            AdpRows [AdpTot].MacVendor := IcsGetMacVendor(AdpRows [AdpTot].MacAddress);
            AdpRows [AdpTot].Index := AdapterAddresses^.Union.IfIndex ;   // IP4 interface ID
            AdpRows [AdpTot].InterfaceName := IpHlpConvIntIdxToStr (AdpRows [AdpTot].Index) ;  // Nov 2014
            AdpRows [AdpTot].aType := AdapterAddresses^.IfType ;
            AdpRows [AdpTot].DHCPEnabled := False ;
            if ((AdapterAddresses^.Flags AND IP_ADAPTER_DHCP_ENABLED) = IP_ADAPTER_DHCP_ENABLED) then
                AdpRows [AdpTot].DHCPEnabled := True ;
            AdpRows [AdpTot].Mtu := AdapterAddresses^.Mtu ;
            AdpRows [AdpTot].IfType := AdapterAddresses^.IfType ;
            AdpRows [AdpTot].OperStatus := AdapterAddresses^.OperStatus ;
            AdpRows [AdpTot].DnsSuffix := Trim (AdapterAddresses^.DnsSuffix) ;  // Nov 2014

        // Unicast, IP for single interface, get list of IP addresses and masks for IPAddressList
            I := 0 ;
            UnicastAddress := AdapterAddresses^.FirstUnicastAddress ;
            while (UnicastAddress <> Nil) do
            begin
            //    len := UnicastAddress.Union.Length ;
            //    if len <> 48 then break ; // sanity check
                AdpRows [AdpTot].IPAddressList [I] := WSocketSockAddrToStr (UnicastAddress.Address.lpSockaddr^) ;  { V8.71 }
                UnicastAddress := UnicastAddress.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].IPAddressList) <= I then
                begin
                     SetLength (AdpRows [AdpTot].IPAddressList, I * 2) ;
                     SetLength (AdpRows [AdpTot].IPMaskList, I * 2) ;  // Nov 2014 NOT USED, NO MASKS!!
                end ;
            end ;
            AdpRows [AdpTot].IPAddressTot := I ;

        // Address Prefix, aka IP masks  - XP SP1 and later only
        // only one mask appears if they are all the same
            I := 0 ;
            PrefixAddress := AdapterAddresses^.FirstPrefix ;
            while (PrefixAddress <> Nil) do
            begin
            //    len := PrefixAddress.Union.Length ;
            //    if len <> 24 then break ; // sanity check - different for Win64, but not really needed
                if PrefixAddress.Address.lpSockaddr.sin6_family = AF_INET then        { V8.71 }
                    AdpRows [AdpTot].PrefixMaskList [I] := IpHlpCreateMask (PrefixAddress.PrefixLength)
                else
                    AdpRows [AdpTot].PrefixMaskList [I] := '/' + IntToStr(PrefixAddress.PrefixLength);
                AdpRows [AdpTot].PrefixIPAddrList [I] := WSocketSockAddrToStr (PrefixAddress.Address.lpSockaddr^) ;  { V8.71 } // ie 192.168.0.0 for mask 255.255.0.0, len=16
                PrefixAddress := PrefixAddress.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].PrefixMaskList) <= I then
                begin
                    SetLength (AdpRows [AdpTot].PrefixMaskList, I * 2) ;
                    SetLength (AdpRows [AdpTot].PrefixIPAddrList, I * 2) ;
                end ;
            end ;
            AdpRows [AdpTot].PrefixTot := I ;

        // keep first IP as current, best we can do
            if AdpRows [AdpTot].IPAddressTot > 0 then
            begin
                AdpRows [AdpTot].CurrIPAddress := AdpRows [AdpTot].IPAddressList [0] ;
                AdpRows [AdpTot].CurrIPMask := AdpRows [AdpTot].IPMaskList [0] ;
            end ;

        // Anycast IP6, group of IP addresses
            I := 0 ;
            AnycastAddress := AdapterAddresses^.FirstAnycastAddress ;
            while (AnycastAddress <> Nil) do
            begin
           //     len := AnycastAddress.Union.Length ;
           //     if len <> 24 then break ; // sanity check
                AdpRows [AdpTot].AnycastIPAddrList [I] := WSocketSockAddrToStr (AnycastAddress.Address.lpSockaddr^) ;  { V8.71 }
                inc (I) ;
                if Length (AdpRows [AdpTot].AnycastIPAddrList) <= I then
                     SetLength (AdpRows [AdpTot].AnycastIPAddrList, I * 2) ;
                AnycastAddress := AnycastAddress.Next ;
            end ;
            AdpRows [AdpTot].AnycastIPAddrTot := I ;

        // Multicast IP6, broadcast IP addresses
            I := 0 ;
            MulticastAddress := AdapterAddresses^.FirstMulticastAddress ;
            while (MulticastAddress <> Nil) do
            begin
             //   len := MulticastAddress.Union.Length ;
             //   if len <> 24 then break ; // sanity check
                AdpRows [AdpTot].MulticastIPAddrList [I] := WSocketSockAddrToStr (MulticastAddress.Address.lpSockaddr^) ;  { V8.71 }
                inc (I) ;
                if Length (AdpRows [AdpTot].MulticastIPAddrList) <= I then
                         SetLength (AdpRows [AdpTot].MulticastIPAddrList, I * 2) ;
                MulticastAddress := MulticastAddress.Next ;
            end ;
            AdpRows [AdpTot].MulticastIPAddrTot := I ;

        // get list of DNS server addresses
            I := 0 ;
            DnsServerAddress := AdapterAddresses^.FirstDnsServerAddress ;
            while (DnsServerAddress <> Nil) do
            begin
           //     len := DnsServerAddress.Union.Length ;
           //     if len <> 24 then break ; // sanity check
                AdpRows [AdpTot].DNSServerList [I] := WSocketSockAddrToStr (DnsServerAddress.Address.lpSockaddr^) ;  { V8.71 }
                DnsServerAddress := DnsServerAddress.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].DNSServerList) <= I then
                     SetLength (AdpRows [AdpTot].DNSServerList, I * 2) ;
                AdpRows [AdpTot].CurrentDNSServer := AdpRows [AdpTot].DNSServerList [0] ;
            end ;
            AdpRows [AdpTot].DNSServerTot := I ;

        // stuff only available for Vista and later
         //   AdpRows [AdpTot].PrimWINSServer [0] := 'AddrLen=' + IntToStr (AdapterAddressLen) ; // !! TEMP
            if (AdapterAddressLen > 300) then
            begin
                AdpRows [AdpTot].Ipv6Index := AdapterAddresses^.Ipv6IfIndex ;
                AdpRows [AdpTot].XmitLinkSpeed := AdapterAddresses^.TransmitLinkSpeed ;
                AdpRows [AdpTot].RecvLinkSpeed := AdapterAddresses^.ReceiveLinkSpeed ;
                AdpRows [AdpTot].Ipv4Metric := AdapterAddresses^.Ipv4Metric ;
                AdpRows [AdpTot].Ipv6Metric := AdapterAddresses^.Ipv6Metric ;
                AdpRows [AdpTot].Luid := AdapterAddresses^.Luid ;
                AdpRows [AdpTot].CompartmentId := AdapterAddresses^.CompartmentId ;
                AdpRows [AdpTot].NetworkGuid := AdapterAddresses^.NetworkGuid ;
                AdpRows [AdpTot].ConnectionType := AdapterAddresses^.ConnectionType ;
                AdpRows [AdpTot].TunnelType := AdapterAddresses^.TunnelType ;

            // get list of IP addresses for GatewayList
                I := 0 ;
                GatewayAddress := AdapterAddresses^.FirstGatewayAddress ;
                while (GatewayAddress <> Nil) do
                begin
                    len := GatewayAddress.Union.Length ;
                    if len <> 24 then break ; // sanity check
                    AdpRows [AdpTot].GatewayList [I] := WSocketSockAddrToStr (GatewayAddress.Address.lpSockaddr^) ;  { V8.71 }
                    GatewayAddress := GatewayAddress.Next ;
                    inc (I) ;
                    if Length (AdpRows [AdpTot].GatewayList) <= I then
                             SetLength (AdpRows [AdpTot].GatewayList, I * 2) ;
                end ;
                AdpRows [AdpTot].GatewayTot := I ;

            // get list of IP addresses for Primary WIIS Server
                I := 0 ;
                WinsServerAddress := AdapterAddresses^.FirstWinsServerAddress ;
                while (WinsServerAddress <> Nil) do
                begin
             //       len := WinsServerAddress.Union.Length ;
             //       if len <> 24 then break ; // sanity check
                    AdpRows [AdpTot].PrimWINSServer [I] := WSocketSockAddrToStr (WinsServerAddress.Address.lpSockaddr^) ;  { V8.71 }
                    WinsServerAddress := WinsServerAddress.Next ;
                    inc (I) ;
                    if Length (AdpRows [AdpTot].PrimWINSServer) <= I then
                             SetLength (AdpRows [AdpTot].PrimWINSServer, I * 2) ;
                end ;
                AdpRows [AdpTot].PrimWINSTot := I ;

            end;

        // get ready for next adaptor
            inc (AdpTot) ;
            if Length (AdpRows) <= AdpTot then SetLength (AdpRows, AdpTot * 2) ;  // more memory
            AdapterAddresses := AdapterAddresses^.Next;
         end ;
         SetLength (AdpRows, AdpTot) ;
      end;
  finally
      FreeMem( pBuf );
  end;
end;


//------------------------------------------------------------------------------
// adaptors and IPv4 addresses only
// obsolete, should it be removed ??
//------------------------------------------------------------------------------
function IpHlpAdaptersInfo(var AdpTot: integer; var AdpRows: TAdaptorRows): integer ;
var
  BufLen        : DWORD;
  AdapterInfo   : PIpAdapterInfo;
  PIpAddr       : PIpAddrString;
  PBuf          : PAnsiChar ;
  I          : integer ;
  PerAdapterInfo: TIpPerAdapterInfo ;
  ret      : integer ;
begin
  result := ERROR_NOT_SUPPORTED ;
  if NOT LoadIpHlp then exit ;
  SetLength (AdpRows, 4) ;
  AdpTot := 0 ;
  BufLen := 0 ;
  result := GetAdaptersInfo( Nil, @BufLen );
//  if (result <> ERROR_INSUFFICIENT_BUFFER) and (result = NO_ERROR) then exit ;
  if (result <> ERROR_BUFFER_OVERFLOW) then exit ;  // 11 Jan 2009 should be the only result
  GetMem( pBuf, BufLen );
  try
      FillChar (pBuf^, BufLen, #0);  // clear buffer
      result := GetAdaptersInfo( PIpAdapterInfo (PBuf), @BufLen );
      if result = NO_ERROR then
      begin
         AdapterInfo := PIpAdapterInfo (PBuf) ;
         while ( AdapterInfo <> nil ) do
         begin
            AdpRows [AdpTot].IPAddressTot := 0 ;
            SetLength (AdpRows [AdpTot].IPAddressList, 2) ;
            SetLength (AdpRows [AdpTot].IPMaskList, 2) ;
            AdpRows [AdpTot].GatewayTot := 0 ;
            SetLength (AdpRows [AdpTot].GatewayList, 2) ;
            AdpRows [AdpTot].DHCPTot := 0 ;
            SetLength (AdpRows [AdpTot].DHCPServer, 2) ;
            AdpRows [AdpTot].PrimWINSTot := 0 ;
            SetLength (AdpRows [AdpTot].PrimWINSServer, 2) ;
            AdpRows [AdpTot].SecWINSTot := 0 ;
            SetLength (AdpRows [AdpTot].SecWINSServer, 2) ;
            AdpRows [AdpTot].DNSServerTot := 0 ;
            SetLength (AdpRows [AdpTot].DNSServerList, 2) ;
            AdpRows [AdpTot].DNSServerList [0] := '' ;
            AdpRows [AdpTot].CurrIPAddress := NULL_IP;
            AdpRows [AdpTot].CurrIPMask := NULL_IP;
            AdpRows [AdpTot].AdapterName := Trim( string( AdapterInfo^.AdapterName ) );
            AdpRows [AdpTot].Description := Trim( string( AdapterInfo^.Description ) );
            AdpRows [AdpTot].MacAddress := PhysMacAddr2Str( TPhysMacAddr(AdapterInfo^.Address ), AdapterInfo^.AddressLength ) ;
            AdpRows [AdpTot].MacVendor := IcsGetMacVendor(AdpRows [AdpTot].MacAddress);
            AdpRows [AdpTot].Index := AdapterInfo^.Index ;
            AdpRows [AdpTot].InterfaceName := IpHlpConvIntIdxToStr (AdpRows [AdpTot].Index) ;  // Nov 2014
            AdpRows [AdpTot].aType := AdapterInfo^.aType ;
            AdpRows [AdpTot].DHCPEnabled := Boolean(AdapterInfo^.DHCPEnabled) ;
            if AdapterInfo^.CurrentIPAddress <> Nil then
            begin
                AdpRows [AdpTot].CurrIPAddress := String (AdapterInfo^.CurrentIPAddress.IpAddress) ;   // 8 Aug 2010
                AdpRows [AdpTot].CurrIPMask := String (AdapterInfo^.CurrentIPAddress.IpMask) ;   // 8 Aug 2010
            end ;

        // get list of IP addresses and masks for IPAddressList
            I := 0 ;
            PIpAddr := @AdapterInfo^.IPAddressList ;
            while (PIpAddr <> Nil) do
            begin
                AdpRows [AdpTot].IPAddressList [I] := String (PIpAddr.IpAddress) ; // 8 Aug 2010
                AdpRows [AdpTot].IPMaskList [I] := String (PIpAddr.IpMask) ;  // 8 Aug 2010
                PIpAddr := PIpAddr.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].IPAddressList) <= I then
                begin
                     SetLength (AdpRows [AdpTot].IPAddressList, I * 2) ;
                     SetLength (AdpRows [AdpTot].IPMaskList, I * 2) ;
                end ;
            end ;
            AdpRows [AdpTot].IPAddressTot := I ;

        // get list of IP addresses for GatewayList
            I := 0 ;
            PIpAddr := @AdapterInfo^.GatewayList ;
            while (PIpAddr <> Nil) do
            begin
                AdpRows [AdpTot].GatewayList [I] := String (PIpAddr.IpAddress) ; // 8 Aug 2010
                PIpAddr := PIpAddr.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].GatewayList) <= I then
                             SetLength (AdpRows [AdpTot].GatewayList, I * 2) ;
            end ;
            AdpRows [AdpTot].GatewayTot := I ;

        // get list of IP addresses for DHCP Server
            I := 0 ;
            PIpAddr := @AdapterInfo^.DHCPServer ;
            while (PIpAddr <> Nil) do
            begin
                AdpRows [AdpTot].DHCPServer [I] := String (PIpAddr.IpAddress) ; // 8 Aug 2010
                PIpAddr := PIpAddr.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].DHCPServer) <= I then
                             SetLength (AdpRows [AdpTot].DHCPServer, I * 2) ;
            end ;
            AdpRows [AdpTot].DHCPTot := I ;

        // get list of IP addresses for PrimaryWINSServer
            I := 0 ;
            PIpAddr := @AdapterInfo^.PrimaryWINSServer ;
            while (PIpAddr <> Nil) do
            begin
                AdpRows [AdpTot].PrimWINSServer [I] := String (PIpAddr.IpAddress) ; // 8 Aug 2010
                PIpAddr := PIpAddr.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].PrimWINSServer) <= I then
                             SetLength (AdpRows [AdpTot].PrimWINSServer, I * 2) ;
            end ;
            AdpRows [AdpTot].PrimWINSTot := I ;

       // get list of IP addresses for SecondaryWINSServer
            I := 0 ;
            PIpAddr := @AdapterInfo^.SecondaryWINSServer ;
            while (PIpAddr <> Nil) do
            begin
                AdpRows [AdpTot].SecWINSServer [I] := String (PIpAddr.IpAddress) ; // 8 Aug 2010
                PIpAddr := PIpAddr.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].SecWINSServer) <= I then
                             SetLength (AdpRows [AdpTot].SecWINSServer, I * 2) ;
            end ;
            AdpRows [AdpTot].SecWINSTot := I ;

            AdpRows [AdpTot].LeaseObtained := AdapterInfo^.LeaseObtained ;
            AdpRows [AdpTot].LeaseExpires := AdapterInfo^.LeaseExpires ;

       // get per adaptor info, W2K and later - 1.5 12 July 2002
            if Assigned (GetPerAdapterInfo) then
            begin
                BufLen := SizeOf (PerAdapterInfo) ;
                ret := GetPerAdapterInfo (AdpRows [AdpTot].Index, @PerAdapterInfo, @BufLen) ;
                if ret = 0 then
                begin
                    AdpRows [AdpTot].AutoConfigEnabled := PerAdapterInfo.AutoconfigEnabled ;
                    AdpRows [AdpTot].AutoConfigActive := PerAdapterInfo.AutoconfigActive ;
                    if PerAdapterInfo.CurrentDNSServer <> Nil then
                        AdpRows [AdpTot].CurrentDNSServer := String (PerAdapterInfo.CurrentDNSServer.IpAddress) ; // 8 Aug 2010

                // get list of DNS IP addresses
                    I := 0 ;
                    PIpAddr := @PerAdapterInfo.DNSServerList ;
                    while (PIpAddr <> Nil) do
                    begin
                        AdpRows [AdpTot].DNSServerList [I] := String (PIpAddr.IpAddress) ; // 8 Aug 2010
                        PIpAddr := PIpAddr.Next ;
                        inc (I) ;
                        if Length (AdpRows [AdpTot].DNSServerList) <= I then
                        begin
                             SetLength (AdpRows [AdpTot].DNSServerList, I * 2) ;
                        end ;
                    end ;
                    AdpRows [AdpTot].DNSServerTot := I ;
                end ;
            end ;

        // get ready for next adaptor
            inc (AdpTot) ;
            if Length (AdpRows) <= AdpTot then
                            SetLength (AdpRows, AdpTot * 2) ;  // more memory
            AdapterInfo := AdapterInfo^.Next;
         end ;
         SetLength (AdpRows, AdpTot) ;
      end ;
  finally
      FreeMem( pBuf );
  end;
end ;


//------------------------------------------------------------------------------
procedure Get_AdaptersInfo( List: TStrings );
var
  AdpTot: integer;
  AdpRows: TAdaptorRows ;
  Error: DWORD ;
  I, J: integer ;
  S: string ;
begin
    if not Assigned( List ) then EXIT;
    List.Clear;
    SetLength (AdpRows, 0) ;
    AdpTot := 0 ;
    List.Add('Network Adaptors List');
    Error := IpHlpAdaptersAddr(AF_UNSPEC, AdpTot, AdpRows) ; // IPv4 and IPv6
    if (Error <> 0) then
        List.Add( SysErrorMessage( GetLastError ) )
    else if AdpTot = 0 then
        List.Add( 'no entries.' )
    else
    begin
        List.Add( Format('%-8s|%-14s|%-40s|%-30s|%-10s|%-20s|%-20s|%-4s|%-16s|%-11s|%-16s|%-16s|%-6s|%-12s|%-10s|%-10s|%s',
                    ['Index', 'Interface', 'Description', 'Friendly Name',
                    'Type', 'MAC Address','Vendor','DHCP', 'DND Suffix', 'Xmit Speed', 'DHCP Server', 'WINS Server',
                    'Metric','Op Status', 'Conn Type', 'Tunnel', 'GUID' ])) ;
        List.Add('');
        for I := 0 to Pred (AdpTot) do
        begin
            with AdpRows [I] do
            begin
                List.Add( Format('%8.8x|%-14s|%-40s|%-30s|%-10s|%-20s|%-20s|%4s|%-16s|%11d|%-16s|%-16s|%6d|%-12s|%-10s|%-10s|%s',
                    [Index, InterfaceName, Copy (Description, 1, 40), Copy (FriendlyName, 1, 30), AdaptTypes [aType], MacAddress, MacVendor,
                    IcsGetYN(DHCPEnabled), DnsSuffix, XmitLinkSpeed, DHCPServer [0], PrimWINSServer [0], Ipv4Metric, IfOperStatuses [Ord (OperStatus)],
                    NetIfConnectionTypes [Ord (ConnectionType)], TunnelTypes [Ord (TunnelType)], AdapterName])) ;

                if IPAddressTot <> 0 then
                begin
                    S := '' ;
                    for J := 0 to Pred (IPAddressTot) do
                    begin
                        S := S + IPAddressList [J] ;
                        if IPMaskList [J] <> '' then
                            S := S + '/' + IPMaskList [J] + ' | '
                        else
                            S := S + ' | ';
                    end;
                    List.Add(IntToStr (IPAddressTot) + ' IP Addresse(s): ' + S);
                end ;
                if PrefixTot <> 0 then
                begin
                    S := '' ;
                    for J := 0 to Pred (PrefixTot) do
                    begin
                        S := S + PrefixIPAddrList [J] ;
                        if PrefixMaskList [J] <> '' then
                            S := S + '=' + PrefixMaskList [J] + ' | '
                        else
                            S := S + ' | ';
                    end;
                    List.Add(IntToStr (PrefixTot) + ' IP Prefixes(s): ' + S);
                end ;
                if DNSServerTot <> 0 then
                begin
                    S := '' ;
                    for J := 0 to Pred (DNSServerTot) do
                        S := S + DNSServerList [J] + ' | ';
                    List.Add(IntToStr (DNSServerTot) + ' DNS Server(s): ' + S);
                end ;
                if GatewayTot <> 0 then
                begin
                    S := '' ;
                    for J := 0 to Pred (GatewayTot) do
                        S := S + GatewayList [J] + ' | ';
                    List.Add(IntToStr (GatewayTot) + ' Gateway(s): ' + S);
                end ;
                List.Add( '  ' );
            end ;
       end ;
  end ;
  SetLength (AdpRows, 0) ;
end ;

//-----------------------------------------------------------------------------
{ get round trip time and hopcount to indicated IP }
function Get_RTTAndHopCount( IPAddr: DWORD; MaxHops: Longint; var RTT: Longint;
  var HopCount: Longint ): integer;
begin
  result := ERROR_NOT_SUPPORTED ;
  if NOT LoadIpHlp then exit ;
  if not GetRTTAndHopCount( IPAddr, @HopCount, MaxHops, @RTT ) then
  begin
    Result := GetLastError;
    RTT := -1; // Destination unreachable, BAD_HOST_NAME,etc...
    HopCount := -1;
  end
  else
    Result := NO_ERROR;
end;

//-----------------------------------------------------------------------------
{ ARP-table lists relations between remote IP and remote MAC-address.
 NOTE: these are cached entries ;when there is no more network traffic to a
 node, entry is deleted after a few minutes.
}
procedure Get_ARPTable( List: TStrings );
var
  IPNetRow      : TMibIPNetRow;
  TableSize     : DWORD;
  NumEntries    : DWORD;
  ErrorCode     : DWORD;
  i             : integer;
  pBuf          : PAnsiChar;
  MacAddr       : String;
begin
  if NOT LoadIpHlp then exit ;
  if not Assigned( List ) then EXIT;
  List.Clear;
  // first call: get table length
  TableSize := 0;
  ErrorCode := GetIPNetTable( Nil, @TableSize, false );   // Angus
  //
  if ErrorCode = ERROR_NO_DATA then
  begin
    List.Add( ' ARP-cache empty.' );
    EXIT;
  end;
  // get table
  GetMem( pBuf, TableSize );
  NumEntries := 0 ;
  try
      ErrorCode := GetIpNetTable( PTMIBIPNetTable( pBuf ), @TableSize, false );
      if ErrorCode = NO_ERROR then
      begin
        NumEntries := PTMIBIPNetTable( pBuf )^.dwNumEntries;
        if NumEntries > 0 then // paranoia striking, but you never know...
        begin
          inc( pBuf, SizeOf( DWORD ) ); // get past table size
          List.Add('ARP Cache Table');
          List.Add(Format( '%8s | %-20s | %-20s |%-16s| %-10s',['Index','Remote MAC','Vendor','Remote IP','Type']));
          for i := 1 to NumEntries do
          begin
            IPNetRow := PTMIBIPNetRow( PBuf )^;
            with IPNetRow do begin
                MacAddr := PhysMacAddr2Str( bPhysAddr, dwPhysAddrLen);
                List.Add( Format( '%8x | %-20s | %-20s |%-16s| %-10s',
                               [dwIndex, MacAddr, IcsGetMacVendor(MacAddr), WSocketIPv4ToStr (TIcsIPv4Address(dwAddr)), ARPEntryType[dwType] ]));
            end;
            inc( pBuf, SizeOf( IPNetRow ) );
          end;
        end
        else
          List.Add( ' ARP-cache empty.' );
      end
      else
        List.Add( SysErrorMessage( ErrorCode ) );

  // we _must_ restore pointer!
  finally
      dec( pBuf, SizeOf( DWORD ) + NumEntries * SizeOf( IPNetRow ) );
      FreeMem( pBuf );
  end ;
end;


//------------------------------------------------------------------------------
// May 2023 - convert MIB neighbour row into our better format
function IpHlpGetNeighbRow(Row: TMibIpNetRow2): TNeighbRow;
begin
    Result.SockAddr := Row.Address;
    Result.IpAddress := WSocketSockAddrToStr(Result.SockAddr);
    Result.MacAddress := PhysMacAddr2Str(Row.PhysicalAddress, Row.PhysicalAddressLength);
    Result.MacVendor := IcsGetMacVendor(Result.MacAddress);
    Result.RandomMac := IcsMacIsRandom (Result.MacAddress);
    Result.IFIndex := Row.InterfaceIndex;
    Result.IFLuid := Row.InterfaceLuid;
    Result.IfType := Result.IFLuid.IfType;
    Result.IfTypeDesc := AdaptTypes[Result.IfType];
    Result.InterfaceName := IpHlpConvIntLuidToAlias(Result.IFLuid);
    Result.State := Row.State;
    Result.StateDesc := NetNeighborStates[Result.State];
    Result.Flags := Row.Flags;
    Result.ReachSecs := Row.ReachabilityTime.LastReachable div 1000;
end;


//------------------------------------------------------------------------------
// May 2023 - get cached table of neighbourhood IP address entries
// IPv4, addresses determined used the Address Resolution Protocol (ARP).
// IPv6, addresses determined using the Neighbor Discovery (ND) protocol for IPv6 as specified in RFC 2461.
function IpHlpIpNeighbTable(var NeighbRows: TNeighbRows; Family: TAddressFamily = AF_INET): DWORD;   // AF_UNSPEC for IPv4 and IPv6
var
    PNetTable: PMibIpNetTable2;
    NumEntries, I: Integer;
begin
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    Result := GetIpNetTable2(Family, PNetTable);
    if Result <> NO_ERROR then
        Exit;
    NumEntries := PNetTable^.dwNumEntries;
    SetLength(NeighbRows, NumEntries);
    if NumEntries = 0 then
        Exit;
    for I := 0 to NumEntries - 1 do begin
        NeighbRows[I] := IpHlpGetNeighbRow(PNetTable^.table[I]);
    end;
    FreeMibTable(PNetTable);
end;


//------------------------------------------------------------------------------
procedure TIcsNeighbDevThread.UpdateRow(NeighbRow: TNeighbRow);
var
    I, Idx, CurAddrNr: Integer;
    MacKey, IpKey: String;
//    SocFamily: TSocketFamily;
begin
    NeighbRow.IpAddress := Trim(NeighbRow.IpAddress);
    NeighbRow.MacAddress := Trim(NeighbRow.MacAddress);
    if Terminated then
        Exit;
    MacKey := NeighbRow.MacAddress;
    if Length(MacKey) < 12 then  // sanity check, ignore Blank
        Exit;
    if NeighbRow.IpAddress = '' then
        Exit;
    if (NeighbRow.State in [NlnsIncomplete, NlnsPermanent]) then
        Exit;
//    if NeighbRow.SockAddr.sin6_family = AF_INET6 then
//        SocFamily := sfIPv6
//    else
//        SocFamily := sfIPv4;

 // see if need new row, then index into dynamic array
    if NOT FNeighbComp.FDevMacIdx.Find(MacKey, I) then begin
        Idx := FNeighbComp.FDeviceTot;
        FNeighbComp.FDeviceTot := FNeighbComp.FDeviceTot + 1;
        if Length(FNeighbComp.FDevices) < FNeighbComp.FDeviceTot then
            SetLength(FNeighbComp.FDevices, FNeighbComp.FDeviceTot + 32);

    // add new MAC record and first address
        with FNeighbComp.FDevices[Idx] do begin
            MacAddress := NeighbRow.MacAddress;
            MacVendor := NeighbRow.MacVendor;
            RandomMac := NeighbRow.RandomMac;
            SetLength(AddrList, 2);  // IPv4 and IPv6, maybe
            AddrList[0].IpAddr := NeighbRow.IpAddress;
            AddrList[0].IpIn6 := NeighbRow.SockAddr;
            if Assigned(FNeighbComp.FIcsDNCache) then begin
                FCacheIP := NeighbRow.IpAddress;
                FCacheTag := MakeLong(Idx, 0);
                {$IFDEF COMPILER14_UP}
                Queue(CacheLookupStart);  // call in context of main thread
                {$ELSE}
                Synchronize(CacheLookupStart);  // call in context of main thread
                {$ENDIF}
            end
            else begin
                AddrList[0].HostName := GetNetBiosHostByAddr(NeighbRow.IpAddress);   // blocking, maybe very slow
                if AddrList[0].HostName = '' then
                    AddrList[0].HostName := AddrList[0].IpAddr;
            end;
            AddrList[0].HostTick := IcsGetTickCount64;
            TotAddrNr := 1;
            FirstDT := Now;
            FUpdRec := True;
        end;
        FNeighbComp.FDevMacIdx.AddObject(MacKey, TObject(Idx));
        IpKey := IcsBufferToHex(NeighbRow.SockAddr, 24) + '=00';
        FNeighbComp.FDevIpIdx.AddObject(IpKey, TObject(Idx));
    end
    else begin
    // old MAC record, check if adding new IP address
        Idx := Integer(FNeighbComp.FDevMacIdx.Objects[I]);
        CurAddrNr := FNeighbComp.FindAddr(NeighbRow.IpAddress, Idx);
        with FNeighbComp.FDevices[Idx] do begin
            if CurAddrNr < 0 then begin
                if TotAddrNr >= Length(AddrList) then
                    SetLength(AddrList, Length(AddrList) * 2);
                AddrList[TotAddrNr].IpAddr := NeighbRow.IpAddress;
                AddrList[TotAddrNr].IpIn6 := NeighbRow.SockAddr;
                if Assigned(FNeighbComp.FIcsDNCache) then begin
                   FCacheIP := NeighbRow.IpAddress;
                   FCacheTag := MakeLong(Idx, TotAddrNr);
                    {$IFDEF COMPILER14_UP}
                    Queue(CacheLookupStart);  // call in context of main thread
                    {$ELSE}
                    Synchronize(CacheLookupStart);  // call in context of main thread
                    {$ENDIF}
                end
                else begin
                    AddrList[TotAddrNr].HostName := GetNetBiosHostByAddr(NeighbRow.IpAddress);  // blocking, maybe very slow
                    if AddrList[TotAddrNr].HostName = '' then
                        AddrList[TotAddrNr].HostName := AddrList[0].IpAddr;
                end;
                AddrList[TotAddrNr].HostTick := IcsGetTickCount64;
                IpKey := IntToStr(TotAddrNr);
                if TotAddrNr < 10 then
                    IpKey := '0' + IpKey;
                TotAddrNr := TotAddrNr + 1;
                IpKey := IcsBufferToHex(NeighbRow.SockAddr, 24) + '=' + IpKey;
                FNeighbComp.FDevIpIdx.AddObject(IpKey, TObject(Idx));
                FUpdRec := True;
            end
            else begin
              // could repeat DNS after several hours...
             //   if FNeighbComp.FDevices[Idx].AddrList[CurAddrNr].HostTick = 0 then
             //       xxStartRevDns(CurAddrNr);   // async lookup
            end;
        end
    end;

// update device row from neighbourhood row
    with FNeighbComp.FDevices[Idx] do begin
        LastState := NeighbRow.State;
        StateDesc := NeighbRow.StateDesc;
        LastDT := Now;
        Detections := Detections + 1;
    end;
end;


//------------------------------------------------------------------------------
procedure TIcsNeighbDevThread.UpdateFromCache;
var
    NeighbRows: TNeighbRows;
    NumEntries, I, Err: Integer;
begin
     Err := IpHlpIpNeighbTable(NeighbRows, FNeighbComp.FDevFamily);
     if Err <>  NO_ERROR then
        Exit;
     NumEntries := Length(NeighbRows);
     if NumEntries > 0 then begin
         for I := 0 to NumEntries - 1 do begin
            if Terminated then
                Exit;
            UpdateRow(NeighbRows[I]);
         end;
     end;
end;


//------------------------------------------------------------------------------
// sends an ARP or IPv6 ICMP packet to an IP address and waits for a respose to
// see if anyone there, beware a few seconds timeout for each failure so this
// function will be very slow if scanning more than a few IPs.
function TIcsNeighbDevThread.ScanAddresses(StartIp: String; TotIps: Integer = 1): Integer;
var
    ScanAddr, SrcAddr: TSockAddrIn6;
    SockAddrIn: TSockAddrIn absolute ScanAddr;
    Row: TMibIpNetRow2;
    I, Err, IpByte: Integer;
begin
    Result := 0;
    ScanAddr := WSocketIPAddrToSocAddr(StartIp);
    if ScanAddr.sin6_family = AF_UNSPEC then
        Exit;
    if ScanAddr.sin6_family = AF_INET6 then
        SrcAddr := WSocketIPAddrToSocAddr(FNeighbComp.FLocIpv6)
    else
        SrcAddr := WSocketIPAddrToSocAddr(FNeighbComp.FLocIpv4);
    if TotIps > 999 then
        TotIps := 999;     // sanity check
    for I := 0 to TotIps do begin
        if Terminated then
            Exit;
        Row.Address := ScanAddr;
        Row.InterfaceLuid := FNeighbComp.FInterfaceLuid;
        Err := ResolveIpNetEntry2 (Row, SrcAddr);   // blocking, waits for ARP reply
        if Err = NO_ERROR then begin
            UpdateRow(IpHlpGetNeighbRow(Row));
            Result := Result + 1;
        end;

    // increment IP address by one
        if ScanAddr.sin6_family = AF_INET then begin
            IpByte := Byte(SockAddrin.sin_addr.S_un_b.s_b4) + 1;
            if IpByte > 255 then begin  // handle wrapping to next byte
                IpByte := 0;
                SockAddrin.sin_addr.S_un_b.s_b3 := AnsiChar(Byte(SockAddrin.sin_addr.S_un_b.s_b3) + 1);
            end;
            SockAddrin.sin_addr.S_un_b.s_b4 := AnsiChar(IpByte);
        end
        else if ScanAddr.sin6_family = AF_INET6 then begin
            IpByte := ScanAddr.sin6_addr.S6_addr[15] + 1;
            if IpByte > 255 then begin  // handle wrapping to next byte, could use word but then need to swap bytes
                IpByte := 0;
                ScanAddr.sin6_addr.S6_addr[14] := ScanAddr.sin6_addr.S6_addr[14] + 1;
            end;
            ScanAddr.sin6_addr.S6_addr[15] := IpByte;
        end
        else
            Exit;
    end;
end;


//------------------------------------------------------------------------------
procedure TIcsNeighbDevThread.UpdateFromAdaptor;
var
    NeighbRow: TNeighbRow;
    AdpTot: integer;
    AdpRows: TAdaptorRows ;
    Error: DWORD ;
    I, J: integer ;
begin
    Error := IpHlpAdaptersAddr(AF_UNSPEC, AdpTot, AdpRows) ; // IPv4 and IPv6
    if (Error = 0) and (AdpTot > 0) then begin
        for I := 0 to Pred (AdpTot) do
        begin
            with AdpRows [I] do begin
                if (Luid.Value <> FNeighbComp.FInterfaceLuid.Value) then
                    Continue;

            // build minimal TNeighbRow for each local IP address
                if IPAddressTot <> 0 then begin
                    for J := 0 to Pred (IPAddressTot) do begin
                        NeighbRow.IpAddress := IPAddressList [J];
                        NeighbRow.MacAddress := MacAddress;
                        NeighbRow.MacVendor := IcsGetMacVendor(MacAddress);
                        NeighbRow.SockAddr := WSocketIPAddrToSocAddr(NeighbRow.IpAddress);
                        NeighbRow.State := NlnsReachable;
                        NeighbRow.StateDesc := NetNeighborStates[NlnsReachable];
                        UpdateRow(NeighbRow);
                    end;
                end ;
            end;
        end;
   end;
end;


//------------------------------------------------------------------------------
procedure TIcsNeighbDevThread.CacheLookupStart; // called in context name lookups
begin
    if Assigned(FNeighbComp.FIcsDNCache) then begin
        FNeighbComp.FIcsDNCache.LookupIPAsync(FCacheIP, FCacheTag, sfAny, FNeighbComp.DNUpdateEvent);
    end;
end;


//------------------------------------------------------------------------------
procedure TIcsNeighbDevThread.ThreadLogEvent (const Info: String);
begin
    if Assigned(FNeighbComp.FOnLogEvent) then begin
        FLogInfo := Info;
        {$IFDEF COMPILER14_UP}
        Queue(CallThreadEvent);
        {$ELSE}
        Synchronize(CallThreadEvent);
        {$ENDIF}
    end;
end;


//------------------------------------------------------------------------------
// called in context of main thread for events
procedure TIcsNeighbDevThread.CallThreadEvent;
begin
    if FUpdRec and Assigned(FNeighbComp.FOnDevUpd) then begin
        FNeighbComp.FOnDevUpd(FNeighbComp);
        FUpdRec := False;
    end;
    if FLogInfo <> '' then begin
        FNeighbComp.FOnLogEvent(FNeighbComp, FLogInfo);
        FLogInfo := '';
    end;
end;


//------------------------------------------------------------------------------
procedure TIcsNeighbDevThread.Execute;
var
    TrgGetCached, TrgScanNet: Int64;
begin
    TrgGetCached := IcsGetTrgSecs64(1);
    if FNeighbComp.FUpdScanSecs > 10 then
        TrgScanNet := IcsGetTrgSecs64(10)
    else
        TrgScanNet := Trigger64Disabled;
    ThreadLogEvent('Neighbour Device and IP Monitoring Thread Started');
    FNeighbComp.FActive := True;
    FUpdRec := False;
    if FNeighbComp.FLocalIps then begin
        ThreadLogEvent('Getting Local IPs');
        UpdateFromAdaptor;
        {$IFDEF COMPILER14_UP}
        Queue(CallThreadEvent);
        {$ELSE}
        Synchronize(CallThreadEvent);
        {$ENDIF}
        ThreadLogEvent('Completed Local IPs');
    end;
    while NOT Terminated do
    begin
        if NOT FNeighbComp.FActive then
            Break;
        FUpdRec := False;
        try
            if IcsTestTrgTick64(TrgGetCached) then begin
                ThreadLogEvent('Getting Cached Neighbour Devices');
                UpdateFromCache;
                if FUpdRec then
                    {$IFDEF COMPILER14_UP}
                    Queue(CallThreadEvent);
                    {$ELSE}
                    Synchronize(CallThreadEvent);
                    {$ENDIF}
                ThreadLogEvent('Completed Getting Cached Neighbour Devices');
                TrgGetCached := IcsGetTrgSecs64(FNeighbComp.FUpdCacheSecs);
                if IcsTestTrgTick64(TrgScanNet) then   // missed 10 secs wait so display is updated first
                   TrgScanNet := IcsGetTrgSecs64(10)
            end;
            if Terminated then
               Break;
            if IcsTestTrgTick64(TrgScanNet) then begin
                ThreadLogEvent('Scanning Network for New Devices');
                if FNeighbComp.FScanStartIPv4 <> '' then
                    ScanAddresses(FNeighbComp.FScanStartIPv4, FNeighbComp.FScanTotIps);
                if FNeighbComp.FScanStartIPv6 <> '' then
                    ScanAddresses(FNeighbComp.FScanStartIPv6, FNeighbComp.FScanTotIps);
                if FUpdRec then
                    {$IFDEF COMPILER14_UP}
                    Queue(CallThreadEvent);
                    {$ELSE}
                    Synchronize(CallThreadEvent);
                    {$ENDIF}
                ThreadLogEvent('Completed Scanning Network');
                TrgScanNet := IcsGetTrgSecs64(FNeighbComp.FUpdScanSecs);
            end;
        except
            ThreadLogEvent('Exception Updating Devices - ' + IcsGetExceptMess (ExceptObject)) ;
        end;
        Sleep (500);   // thread now stops for 500ms
    end;
    FNeighbComp.FActive := False;
    ThreadLogEvent('Neighbour Device and IP Monitoring Stopped');
end;

//------------------------------------------------------------------------------
// May 2023 - build historic table of neighbourhood IP address entries from cache, stored by
// MAC device address with one or more IP addresses per devices and reverse DNS hosts,
// accessible sort by MAC or IP address.
constructor TIcsNeighbDevices.Create;
//var
//    I: Integer;
begin
    Inherited;
    FDevMacIdx := TStringList.Create;
    FDevMacIdx.Sorted := True;
    FDevIpIdx := TStringList.Create;
    FDevIpIdx.Sorted := True;
    SetLength(FDevices, 32);
    FDeviceTot := 0;
    FDevCritSect := TIcsCriticalSection.Create;
    FLocIpv4 := '';
    FLocIpv6 := '';
    FScanStartIPv4 := '';
    FScanStartIPv6 := '';
    FScanTotIps := 1;
    FUpdScanSecs := 60;     // seconds between IP scans
    FUpdCacheSecs := 600;   // seconds between cache updates, zero for none
end;


//------------------------------------------------------------------------------
destructor TIcsNeighbDevices.Destroy;
begin
    SetLength(FDevices, 0);
    FDevMacIdx.Free;
    FDevIpIdx.Free;
    FDevCritSect.Free;
    Inherited;
end;


//------------------------------------------------------------------------------
procedure TIcsNeighbDevices.LogEvent (const Info: String);
begin
    if Assigned(FOnLogEvent) then begin
        FOnLogEvent(Self, Info);
    end;
end;


//------------------------------------------------------------------------------
// look for IP address in MAC device record, may be several, -1 not found
function TIcsNeighbDevices.FindAddr(const IP: String; Idx: Integer): Integer;
var
    I: Integer;
begin
    with FDevices[Idx] do begin
        Result := -1;
        if TotAddrNr <= 0 then
            Exit;
        for I := 0 to TotAddrNr - 1 do begin
            if IP = AddrList[I].IpAddr then begin
                Result := I;
                Exit;
            end;
        end;
    end;
end;


//------------------------------------------------------------------------------
procedure TIcsNeighbDevices.Clear;  // May 2023
begin
    if Assigned(FDevMacIdx) then begin
        FDevMacIdx.Clear;
        FDevIpIdx.Clear;
    end;
    SetLength(FDevices, 0);
    FDeviceTot := 0;
end;


//------------------------------------------------------------------------------
procedure TIcsNeighbDevices.EmptyCache;
begin
    FlushIpNetTable2(AF_UNSPEC, 0);
end;


//------------------------------------------------------------------------------
function TIcsNeighbDevices.GetIpList(Idx: Integer): String;
var
    I: Integer;
begin
    Result := '';
    if Idx < 0 then
       Exit;
    with FDevices [Idx] do begin
        if TotAddrNr = 0 then
            Exit;
        for I := 0 to TotAddrNr - 1 do begin
            if I > 0 then
                Result := Result + '|';
            Result := Result + AddrList[I].IpAddr;
        end;
    end;
end;


//------------------------------------------------------------------------------
// event called by TIcsDomainNameCache when name lookup done
procedure TIcsNeighbDevices.DNUpdateEvent(Sender: TObject; ItemNr: Integer);
var
    Idx, AddrNr: Integer;
    MyCache: TIcsDomainNameCache;
begin
    MyCache := (Sender as TIcsDomainNameCache);
    with MyCache.GetDNItem(ItemNr) do begin
        Idx := LongRec(ReqTag).Lo;
        AddrNr := LongRec(ReqTag).Hi;
        if Idx >= FDeviceTot then
            Exit;
        if AddrNr >= Length(FDevices[Idx].AddrList) then
            Exit;
        FDevices[Idx].AddrList[AddrNr].HostName := MyCache.BuildRespList(ItemNr);
    end;
end;


//------------------------------------------------------------------------------
procedure TIcsNeighbDevices.DispMacDevs(List: TStrings);
var
    I, Row: Integer;
begin
    if not Assigned( List ) then EXIT;
    List.Clear;
    if GetMacTot = 0 then begin
        List.Add('No Neighbourhood Devices Found');
        Exit;
    end;
    List.Add('Neighbourhood Devices');
    List.Add(Format('%-20s|%-20s|%-40s|%-40s|%-14s|%-20s|%-20s|%10s',
                                ['Remote MAC','Vendor','Local IP','Host','State','First Seen','Last Seen','Seen Count'] ));
    for I := 0 to GetMacTot - 1 do begin
        Row := GetMacRow(I);
        if Row < 0 then
            Continue;
        with FDevices [Row] do begin
            if TotAddrNr = 0 then
                Continue;
            List.Add( Format('%-20s|%-20s|%-40s|%-40s|%-14s|%-20s|%-20s|%10d', [MacAddress, MacVendor, GetIpList(Row), AddrList[0].HostName, StateDesc,
                DateTimetoStr(FirstDT), DateTimetoStr(LastDT), Detections ] ));
        end;
    end;
end;


//------------------------------------------------------------------------------
procedure TIcsNeighbDevices.DispIpDevs(List: TStrings);
var
    I, Row, AddrNr: Integer;
begin
    if not Assigned( List ) then EXIT;
    List.Clear;
    if GetIpTot = 0 then begin
        List.Add('No Neighbourhood IP Addresses Found');
        Exit;
    end;
    List.Add('Neighbourhood IP Addresses');
    List.Add(Format('%-40s|%-40s|%-20s|%-20s|%-14s|%-20s|%-20s|%10s',
                           ['Local IP','Host','Remote MAC','Vendor','State','First Seen','Last Seen','Seen Count'] ));
    for I := 0 to GetIpTot - 1 do begin
        Row := GetIpRow(I, AddrNr);  // which MAC row and address in that row
        if Row < 0 then
            Continue;
   //     LogEvent ('IpSort=' + FDevIpIdx.Strings[I] + ', Row=' + IntToStr(Row) + ', AddrNr=' + IntToStr(AddrNr) );   // TEMP !!!!
        with FDevices [Row] do begin
            if TotAddrNr = 0 then
                Continue;
            if AddrNr > TotAddrNr then  // sanity check
                AddrNr := 0;
            List.Add( Format('%-40s|%-40s|%-20s|%-20s|%-14s|%-20s|%-20s|%10d', [AddrList[AddrNr].IpAddr, AddrList[AddrNr].HostName,
                                                 MacAddress, MacVendor, StateDesc, DateTimetoStr(FirstDT), DateTimetoStr(LastDT), Detections ] ));
        end;
    end;
end;


//------------------------------------------------------------------------------
function TIcsNeighbDevices.GetMacTot: Integer;
begin
    Result := FDevMacIdx.Count;
end;


//------------------------------------------------------------------------------
function TIcsNeighbDevices.GetIpTot: Integer;
begin
    Result := FDevIpIdx.Count;
end;


//------------------------------------------------------------------------------
function TIcsNeighbDevices.GetMacRow(Idx: Integer): Integer;
begin
    if Idx >= FDevMacIdx.Count then
        Result := -1
    else
        Result := Integer(FDevMacIdx.Objects[Idx]);
end;


//------------------------------------------------------------------------------
function TIcsNeighbDevices.GetIpRow(Idx: Integer; var AddrNr: Integer): Integer;
begin
    AddrNr := 0;
    if (Idx >= FDevIpIdx.Count) then
        Result := -1
    else begin
        Result := Integer(FDevIpIdx.Objects[Idx]);
        AddrNr := atoi(FDevIpIdx.ValueFromIndex[Idx]);
    end;
end;


//------------------------------------------------------------------------------
procedure TIcsNeighbDevices.StartMonitor;
begin
    if NOT LoadIpHlp then exit ;
    if FInterfaceLuid.Value = 0 then
            FInterfaceLuid := IpHlpAdpforIP (FLocIpv4);
    if InterfaceLuid.Value = 0 then
        Exit;
  // Start Thread
    FDevThread := TIcsNeighbDevThread.Create (true) ;
    FDevThread.FNeighbComp := Self ;
    FDevThread.FreeOnTerminate := True;
    LogEvent ('Neighbour Device and IP Monitoring Starting');
{$IFDEF COMPILER14_UP}
    FDevThread.Start;
{$ELSE}
    FDevThread.Resume;
{$ENDIF}
end;


//------------------------------------------------------------------------------
procedure TIcsNeighbDevices.StopMonitor;
//var
//    ID: integer ;
//    Trg: Int64;
begin
  // stop thread
    FActive := False;
    if Assigned (FDevThread) then begin
        if FActive then
            LogEvent ('Neighbour Device and IP Monitoring Stopping');
//        ID := FDevThread.ThreadId ;
        FActive := False;
        FDevThread.Terminate ;
//        PostThreadMessage (ID, WM_QUIT, 0, 0);  // terminate thread
    {   Trg := IcsGetTrgSecs64 (2) ;
        while True do   // two seconds, idle thread should stop in 500ms
        begin
            if NOT FActive then break ;
            if IcsTestTrgTick64 (Trg) then
                break ;
        end;  }
    end;
 {
    if Assigned (FDevThread) then begin
        FDevThread.Terminate ;
        FDevThread.WaitFor ;
        FDevThread.Free ;
        FDevThread := nil ;
    end ;    }
end;


//------------------------------------------------------------------------------
// flush IP Neighbourhood table - Warning, requires administrator rights
function IpHlpIpNeighbTabFlush(Family: TAddressFamily = AF_INET): DWORD;  // May 2023  // AF_UNSPEC for IPv4 and IPv6
begin
    Result := FlushIpNetTable2(Family, 0);
end;


//------------------------------------------------------------------------------
// May 2023 - print table of neighbourhood IP address entries
procedure Get_IPNeighbourTable( List: TStrings; PermAddr: Boolean = False);
var
    NeighbRows: TNeighbRows;
    ErrorCode: DWORD;
    NumEntries, I: Integer;
begin
    if not Assigned( List ) then EXIT;
    List.Clear;
    ErrorCode := IpHlpIpNeighbTable(NeighbRows, AF_UNSPEC) ;  // both IPv4 and IPv6
    if ErrorCode <> NO_ERROR then
    begin
        List.Add (SysErrorMessage (ErrorCode));
        exit ;
    end;
    NumEntries := Length (NeighbRows) ;
    if NumEntries = 0 then
    begin
        List.Add ('No Neighbourhood IP Addresses Found') ;
        exit ;
    end ;
    List.Add('Neighbourhood IP Address Cache Table');
    List.Add(Format('%-20s|%-20s|%-40s|%-14s|%-30s|%-20s|%10s', ['Remote MAC','Vendor','Remote IP','State','Interface','IF Type','Reach Secs'] ));
    for I := 0 to Pred (NumEntries) do
    begin
        with NeighbRows [I] do begin
            if (NOT PermAddr) and (State = NlnsPermanent) then
              continue;
            List.Add( Format('%-20s|%-20s|%-40s|%-14s|%-30s|%-20s|%10d', [MacAddress, MacVendor, IpAddress, StateDesc, InterfaceName, IfTypeDesc, ReachSecs ] ));
        end;
    end;
end;


//------------------------------------------------------------------------------
// find network information about remote IP on LAN, sends ARP request if not in neighbourhood table
function IpHlpResolveIpNet (const RemIp, LocIp: String; InterfaceLuid: TNetLuid; var NeighbRow: TNeighbRow): integer ;   // May 2023
var
    Row: TMibIpNetRow2;
    DestAddr, SrcAddr: TSockAddrIn6;
begin
    Result :=  ERROR_NOT_SUPPORTED;
    NeighbRow.IpAddress := '';
    if NOT LoadIpHlp then exit ;
    if InterfaceLuid.Value = 0 then
         InterfaceLuid := IpHlpAdpforIP (LocIP);
    if InterfaceLuid.Value = 0 then
        Exit;
    DestAddr := WSocketIPAddrToSocAddr(RemIp);
    SrcAddr := WSocketIPAddrToSocAddr(LocIp);
    Row.Address := DestAddr;
    Row.InterfaceLuid := InterfaceLuid;
    Result := ResolveIpNetEntry2 (Row, SrcAddr);
    if Result <> NO_ERROR then
        Exit;
    NeighbRow := IpHlpGetNeighbRow(Row);
end;

//------------------------------------------------------------------------------

// get list of current TCP connections, get process Id so we can find EXE name
function IpHlpTCPTable(var ConnRows: TConnRows; Family: TAddressFamily = AF_INET; StartRow: Integer = 0): integer ;
var
  i, NumEntries, CurEntry, TableLen : integer;
  TableSize, ModSize : DWORD;
  ErrorCode2 : DWORD;
  pTCPTableEx2  : PTMibTCPTableOwnerModule;
  TcpIpOwnerModuleBasicInfoEx: TTcpIpOwnerModuleBasicInfoEx ;
  pTCP6TableEx2  : PTMibTCP6TableOwnerModule;
  LocalFileTime: TFileTime ;
begin
  result := ERROR_NOT_SUPPORTED ;
  if NOT LoadIpHlp then exit ;
  CurEntry := StartRow ;    // May 2023 allow to combine TCP and UDP connections
  TableLen := StartRow ;
  if StartRow = 0 then
    SetLength (ConnRows, 0) ;
  if not Assigned (GetExtendedTCPTable) then
    exit;    // only XP SP2, W2K3 SP1, Vista and later
  pTCPTableEx2 := Nil ;
  pTCP6TableEx2 := Nil ;

  try
    // IPv4 connections
      if Family in [AF_INET, AF_UNSPEC] then
      begin
          TableSize := 0 ;   // first call : get size of table
          result := GetExtendedTCPTable (Nil, @TableSize, false, AF_INET, TCP_TABLE_OWNER_MODULE_ALL, 0);
          if result <> ERROR_INSUFFICIENT_BUFFER then EXIT;
          // get required size of memory, call again
          GetMem (pTCPTableEx2, TableSize);
          // get table
          result := GetExtendedTCPTable (pTCPTableEx2, @TableSize, true, AF_INET, TCP_TABLE_OWNER_MODULE_ALL, 0) ;
          if result <> NO_ERROR then exit ;
          NumEntries := pTCPTableEx2^.dwNumEntries;
          if NumEntries >= 0 then
          begin
              TableLen := TableLen + NumEntries;
              SetLength (ConnRows, TableLen) ;
              for I := 0 to Pred (NumEntries) do
              begin
                  with ConnRows [CurEntry], pTCPTableEx2^.Table [I] do
                  begin
                      ProcName := '' ;
                      Protocol := 'TCP';             // May 2023
                      State := dwState ;
                      LocSockAddr := WSocketIPV4ToSocAddr (dwLocalAddr);      { V8.71 }
                      LocalAddr := WSocketSockAddrToStr (LocSockAddr);        { V8.71 }
                      LocalPort := Port2Wrd (dwLocalPort) ;
                      LocalPortDesc := Lowercase(IcsServName (LocalPort));      // May 2023
                      RemSockAddr := WSocketIPV4ToSocAddr (dwRemoteAddr);     { V8.71 }
                      RemoteAddr := WSocketSockAddrToStr (RemSockAddr);       { V8.71 }
                      RemotePort := Port2Wrd (dwRemotePort) ;
                      if dwRemoteAddr = 0 then RemotePort := 0;
                      RemotePortDesc := Lowercase(IcsServName (RemotePort));     // May 2023
                      FileTimeToLocalFileTime (liCreateTimestamp, LocalFileTime) ;
                      CreateDT := FileTimeToDateTime (LocalFileTime) ;
                      ProcessID := dwOwningPid ;
                      if ProcessID > 0 then
                      begin
                          ModSize := SizeOf (TcpIpOwnerModuleBasicInfoEx) ;
                          ErrorCode2 := GetOwnerModuleFromTcpEntry ( @pTCPTableEx2^.Table [I],
                                                   TcpIpOwnerModuleInfoClassBasic, @TcpIpOwnerModuleBasicInfoEx, @ModSize);
                          if ErrorCode2 = NO_ERROR then
                              ProcName := TcpIpOwnerModuleBasicInfoEx.TcpIpOwnerModuleBasicInfo.pModulePath
                          else
                               ProcName := 'Err: ' + SysErrorMessage (ErrorCode2);    // usually access denied except admins
                      end;
                  end;
                  inc (CurEntry) ;
              end ;
          end;
      end ;

   // IPv6 connections
      if Family in [AF_INET6, AF_UNSPEC] then
      begin
          TableSize := 0 ;
          result := GetExtendedTCPTable (Nil, @TableSize, false, AF_INET6, TCP_TABLE_OWNER_MODULE_ALL, 0);
          if result <> ERROR_INSUFFICIENT_BUFFER then EXIT;
          // get required size of memory, call again
          GetMem (pTCP6TableEx2, TableSize);
          // get table
          result := GetExtendedTCPTable (pTCP6TableEx2, @TableSize, true, AF_INET6, TCP_TABLE_OWNER_MODULE_ALL, 0) ;
          if result <> NO_ERROR then exit ;
          NumEntries := pTCP6TableEx2^.dwNumEntries;
          if NumEntries > 0 then
          begin
              TableLen := TableLen + NumEntries;
              SetLength (ConnRows, TableLen) ;
              for I := 0 to Pred (NumEntries) do
              begin
                  with ConnRows [CurEntry], pTCP6TableEx2^.Table [I] do
                  begin
                      ProcName := '' ;
                      Protocol := 'TCP6';            // May 2023
                      State := dwState ;
                      LocSockAddr.sin6_family := AF_INET6;           { V8.71 }
                      LocSockAddr.sin6_addr := ucLocalAddr;
                      LocSockAddr.sin6_scope_id := dwLocalScopeId ;
                      LocalAddr := WSocketSockAddrToStr(LocSockAddr);
                      LocalPort := Port2Wrd (dwLocalPort) ;
                      LocalPortDesc := Lowercase(IcsServName (LocalPort));      // May 2023
                      RemSockAddr.sin6_family := AF_INET6;          { V8.71 }
                      RemSockAddr.sin6_addr := ucRemoteAddr;
                      RemSockAddr.sin6_scope_id := dwRemoteScopeId ;
                      RemoteAddr := WSocketSockAddrToStr(RemSockAddr);
                      RemotePort := Port2Wrd (dwRemotePort) ;
                      if RemoteAddr  = '' then RemotePort := 0;
                      RemotePortDesc := Lowercase(IcsServName (RemotePort));     // May 2023
                      FileTimeToLocalFileTime (liCreateTimestamp, LocalFileTime) ;
                      CreateDT := FileTimeToDateTime (LocalFileTime) ;
                      ProcessID := dwOwningPid ;
                      if ProcessID > 0 then
                      begin
                          ModSize := SizeOf (TcpIpOwnerModuleBasicInfoEx) ;
                          ErrorCode2 := GetOwnerModuleFromTcp6Entry ( @pTCP6TableEx2^.Table [I],
                                             TcpIpOwnerModuleInfoClassBasic, @TcpIpOwnerModuleBasicInfoEx, @ModSize);
                          if ErrorCode2 = NO_ERROR then
                              ProcName := TcpIpOwnerModuleBasicInfoEx.TcpIpOwnerModuleBasicInfo.pModulePath
                          else
                               ProcName := 'Err: ' + SysErrorMessage (ErrorCode2);    // usually access denied except admins
                      end;
                  end;
                  inc (CurEntry) ;
              end ;
          end;
      end
  finally
        if pTCPTableEx2 <> Nil then
            FreeMem (pTCPTableEx2) ;
        if pTCP6TableEx2 <> Nil then
            FreeMem (pTCP6TableEx2) ;
  end ;
end;


//------------------------------------------------------------------------------
// display list of current TCP and UDP connections  - May 2023,
procedure List_ConnTable(ConnRows: TConnRows; List: TStrings );
var
  NumEntries, I: integer ;
  DispName, DispTime, DispRem, DispState: string ;
begin
    NumEntries := Length (ConnRows) ;
    if NumEntries = 0 then begin
        List.Add ('No connections') ;
        exit ;
    end ;
    List.Add (Format('%-15s|%-47s|%-47s|%-15s|%-45s|%-20s',
            [IpConnsRowHdrs[0],IpConnsRowHdrs[1],IpConnsRowHdrs[2],IpConnsRowHdrs[3],IpConnsRowHdrs[4], IpConnsRowHdrs[5]]));  // May 2023
    for I := 0 to Pred (NumEntries) do
    begin
        with ConnRows [I] do
        begin
        // build display for user
            if ShowExePath then       // 15 Jan 2009
                DispName := ProcName
            else
                DispName := ExtractFileName (ProcName) ;
          DispTime := '' ;
          if CreateDT > 0 then
            DispTime := DateTimeToStr (CreateDT) ;
            DispRem := '';
            DispState := '';
            if State >= 0 then begin  // TCP, no remote for UDP
                if State <> MIB_TCP_STATE_LISTEN then
                    DispRem := RemoteAddr + ':' + Lowercase(RemotePortDesc);
                DispState := TCPConnState[State];
            end;
            List.Add (Format('%-15s|%-47s|%-47s|%-15s| %8d|%-37s|%-20s',
                  [Protocol, LocalAddr + ':' +  IcsServName (LocalPort), DispRem, DispState, ProcessId, DispName, DispTime] ) );
        end ;
    end ;
end ;


//------------------------------------------------------------------------------
// display list of current TCP connections
procedure Get_TCPTable( List: TStrings );
var
  ConnRows: TConnRows ;
  ErrorCode: integer ;
begin
  if not Assigned( List ) then EXIT;
  List.Clear;
  ErrorCode := IpHlpTCPTable (ConnRows, AF_UNSPEC) ;  // both IPv4 and IPv6
  if ErrorCode <> NO_ERROR then
  begin
     List.Add (SysErrorMessage (ErrorCode));
     exit ;
  end;
  if Length (ConnRows) = 0 then
  begin
      List.Add ('No TCP/IP connections') ;
      exit ;
  end ;
  List_ConnTable(ConnRows, List);
end ;

//------------------------------------------------------------------------------

procedure Get_TCPStatistics( List: TStrings );
var
  TCPStats      : TMibTCPStats;
  ErrorCode     : DWORD;
begin
  if not Assigned( List ) then EXIT;
  List.Clear;
  if NOT LoadIpHlp then exit ;
  ErrorCode := GetTCPStatistics( @TCPStats );
  if ErrorCode = NO_ERROR then
    with TCPStats do
    begin
      List.Add( 'TCP Statistics');
      List.Add( 'Retransmission algorithm :' + TCPToAlgo[dwRTOAlgorithm] );
      List.Add( 'Minimum Time-Out         :' + IntToStr( dwRTOMin ) + ' ms' );
      List.Add( 'Maximum Time-Out         :' + IntToStr( dwRTOMax ) + ' ms' );
      List.Add( 'Maximum Pend.Connections :' + IntToStr( dwRTOAlgorithm ) );
      List.Add( 'Active Opens             :' + IntToStr( dwActiveOpens ) );
      List.Add( 'Passive Opens            :' + IntToStr( dwPassiveOpens ) );
      List.Add( 'Failed Open Attempts     :' + IntToStr( dwAttemptFails ) );
      List.Add( 'Established conn. Reset  :' + IntToStr( dwEstabResets ) );
      List.Add( 'Current Established Conn.:' + IntToStr( dwCurrEstab ) );
      List.Add( 'Segments Received        :' + IntToStr( dwInSegs ) );
      List.Add( 'Segments Sent            :' + IntToStr( dwOutSegs ) );
      List.Add( 'Segments Retransmitted   :' + IntToStr( dwReTransSegs ) );
      List.Add( 'Incoming Errors          :' + IntToStr( dwInErrs ) );
      List.Add( 'Outgoing Resets          :' + IntToStr( dwOutRsts ) );
      List.Add( 'Cumulative Connections   :' + IntToStr( dwNumConns ) );
    end
  else
    List.Add( SyserrorMessage( ErrorCode ) );
end;


//------------------------------------------------------------------------------
function IpHlpTCPStatistics (var TCPStats: TMibTCPStats): integer ;
begin
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    result := GetTCPStatistics( @TCPStats );
end;

//------------------------------------------------------------------------------

// get list of current UDP connections, XP gets process Id so we can find EXE

function IpHlpUDPTable(var ConnRows: TConnRows; Family: TAddressFamily = AF_INET; StartRow: Integer = 0): integer ;
var
  i, NumEntries, CurEntry, TableLen : integer;
  TableSize, ModSize    : DWORD;
  ErrorCode2 : DWORD;
  pUDPTableEx2: PTMibUDPTableOwnerModule;
  TcpIpOwnerModuleBasicInfoEx: TTcpIpOwnerModuleBasicInfoEx ;
  pUDP6TableEx2: PTMibUDP6TableOwnerModule;
  LocalFileTime: TFileTime ;
begin
  result := ERROR_NOT_SUPPORTED ;
  if NOT LoadIpHlp then exit ;
  CurEntry := StartRow ;    // May 2023 allow to combine TCP and UDP connections
  TableLen := StartRow ;
  if StartRow = 0 then
    SetLength (ConnRows, 0) ;
  if not Assigned (GetExtendedUDPTable) then exit;
  pUDPTableEx2 := Nil ;
  pUDP6TableEx2 := nil ;

  try
      // use latest API XP SP2, W2K3 SP1, Vista and later, first call : get size of table
      if Family in [AF_INET, AF_UNSPEC] then
      begin
          TableSize := 0 ;
          result := GetExtendedUDPTable (Nil, @TableSize, false, AF_INET, UDP_TABLE_OWNER_MODULE, 0);
          if result <> ERROR_INSUFFICIENT_BUFFER then EXIT;

          // get required size of memory, call again
          GetMem (pUDPTableEx2, TableSize);
          // get table
          result := GetExtendedUdpTable (pUDPTableEx2, @TableSize, true, AF_INET, UDP_TABLE_OWNER_MODULE, 0) ;
          if result <> NO_ERROR then exit ;
          NumEntries := pUDPTableEx2^.dwNumEntries;
          if NumEntries <> 0 then
          begin
              TableLen := TableLen + NumEntries;
              SetLength (ConnRows, TableLen) ;
              for I := 0 to Pred (NumEntries) do
              begin
                  with ConnRows [CurEntry], pUDPTableEx2^.Table [I] do
                  begin
                      ProcName := '' ;
                      Protocol := 'UDP';    // May 2023
                      State := -1 ;
                      LocSockAddr := WSocketIPV4ToSocAddr (dwLocalAddr);      { V8.71 }
                      LocalAddr := WSocketSockAddrToStr (LocSockAddr);        { V8.71 }
                      LocalPort := Port2Wrd (dwLocalPort) ;
                      LocalPortDesc := Lowercase(IcsServName (LocalPort));   // May 2023
                      RemoteAddr := '' ;
                      RemotePort := 0 ;
                      RemotePortDesc := '';
                      FileTimeToLocalFileTime (liCreateTimestamp, LocalFileTime) ;
                      CreateDT := FileTimeToDateTime (LocalFileTime) ;
                      ProcessID := dwOwningPid ;
                      if ProcessID > 0 then
                      begin
                          ModSize := SizeOf (TcpIpOwnerModuleBasicInfoEx) ;
                          ErrorCode2 := GetOwnerModuleFromUdpEntry ( @pUDPTableEx2^.Table [I],
                                TcpIpOwnerModuleInfoClassBasic, @TcpIpOwnerModuleBasicInfoEx, @ModSize);
                          if ErrorCode2 = NO_ERROR then
                                  ProcName := TcpIpOwnerModuleBasicInfoEx.TcpIpOwnerModuleBasicInfo.pModulePath
                          else
                               ProcName := 'Err: ' + SysErrorMessage (ErrorCode2);    // usually access denied except admins
                      end;
                  end;
                  inc (CurEntry) ;
              end;
          end ;
      end;
      if Family in [AF_INET6, AF_UNSPEC] then
      begin
          TableSize := 0 ;
          result := GetExtendedUDPTable (Nil, @TableSize, false, AF_INET6, UDP_TABLE_OWNER_MODULE, 0);
          if result <> ERROR_INSUFFICIENT_BUFFER then EXIT;

          // get required size of memory, call again
          GetMem (pUDP6TableEx2, TableSize);
          // get table
          result := GetExtendedUdpTable (pUDP6TableEx2, @TableSize, true, AF_INET6, UDP_TABLE_OWNER_MODULE, 0) ;
          if result <> NO_ERROR then exit ;
          NumEntries := pUDP6TableEx2^.dwNumEntries;
          if NumEntries <> 0 then
          begin
              TableLen := TableLen + NumEntries;
              SetLength (ConnRows, TableLen) ;
              for I := 0 to Pred (NumEntries) do
              begin
                  with ConnRows [CurEntry], pUDP6TableEx2^.Table [I] do
                  begin
                      Protocol := 'UDP6';    // May 2023
                      ProcName := '' ;
                      State := -1 ;
                      LocSockAddr.sin6_family := AF_INET6;           { V8.71 }
                      LocSockAddr.sin6_addr := ucLocalAddr;
                      LocSockAddr.sin6_scope_id := dwLocalScopeId ;
                      LocalAddr := WSocketSockAddrToStr(LocSockAddr);
                      LocalPort := Port2Wrd (dwLocalPort) ;
                      LocalPortDesc := Lowercase(IcsServName (LocalPort));   // May 2023
                      RemSockAddr.sin6_family := 0 ;
                      RemoteAddr := '' ;
                      RemotePort := 0 ;
                      RemotePortDesc := '';
                      FileTimeToLocalFileTime (liCreateTimestamp, LocalFileTime) ;
                      CreateDT := FileTimeToDateTime (LocalFileTime) ;
                      ProcessID := dwOwningPid ;
                      if ProcessID > 0 then
                      begin
                          ModSize := SizeOf (TcpIpOwnerModuleBasicInfoEx) ;
                          ErrorCode2 := GetOwnerModuleFromUdp6Entry ( @pUDP6TableEx2^.Table [I],
                                TcpIpOwnerModuleInfoClassBasic, @TcpIpOwnerModuleBasicInfoEx, @ModSize);
                          if ErrorCode2 = NO_ERROR then
                                  ProcName := TcpIpOwnerModuleBasicInfoEx.TcpIpOwnerModuleBasicInfo.pModulePath
                          else
                               ProcName := 'Err: ' + SysErrorMessage (ErrorCode2);    // usually access denied except admins
                      end;
                  end;
                  inc (CurEntry) ;
              end;
          end ;
      end;

  finally
     if pUdpTableEx2 <> Nil then
        FreeMem (pUdpTableEx2) ;
     if pUdp6TableEx2 <> Nil then
        FreeMem (pUdp6TableEx2) ;
  end ;
end;

//------------------------------------------------------------------------------

// display list of current UDP connections

procedure Get_UDPTable( List: TStrings );
var
  ConnRows: TConnRows ;
  ErrorCode: integer ;
begin
  if not Assigned( List ) then EXIT;
  List.Clear;
  ErrorCode := IpHlpUDPTable (ConnRows, AF_UNSPEC) ; // IPv4 and IPv6
  if ErrorCode <> NO_ERROR then
  begin
     List.Add (SysErrorMessage (ErrorCode));
     exit ;
  end;
  if Length (ConnRows) = 0 then
  begin
      List.Add ('No UDP Connections') ;
      exit ;
  end ;
  List_ConnTable(ConnRows, List);
end ;


//------------------------------------------------------------------------------
// get TCP and/or UDP connections table
function IpHlpConnsTable(var ConnRows: TConnRows; Family: TAddressFamily = AF_INET; Prot: TTransProt = ProtoBoth): integer ;   // May 2023
begin
    Result := ERROR_NOT_SUPPORTED;
    SetLength(ConnRows, 0);
    if Prot in [ProtoBoth, ProtoTcp] then begin
        Result := IpHlpTCPTable(ConnRows, Family, 0);
        if Result  <> NO_ERROR then
            Exit;
    end;
    if Prot in [ProtoBoth, ProtoUdp] then
        Result := IpHlpUDPTable(ConnRows, Family, Length(ConnRows));
end;


//------------------------------------------------------------------------------
// display TCP and/or UDP connections table
procedure Get_ConnsTable( List: TStrings; Family: TAddressFamily = AF_INET; Prot: TTransProt = ProtoBoth);       // May 2023                                                                              // May 2023
var
  ConnRows: TConnRows ;
  ErrorCode: integer ;
begin
    if not Assigned( List ) then
        EXIT;
    List.Clear;
    ErrorCode := IpHlpConnsTable (ConnRows, Family, Prot) ;
    if ErrorCode <> NO_ERROR then begin
        List.Add (SysErrorMessage (ErrorCode));
        exit ;
    end;
    if Length (ConnRows) = 0 then begin
        List.Add ('No Connections') ;
        exit ;
    end ;
    List_ConnTable(ConnRows, List);
end;


//------------------------------------------------------------------------------
function IpHlpConvUniRow (Row: TMibUnicastIpAddressRow): TIpAddrInfo ;
begin
    with Result, Row do
    begin
        IpAddress := WSocketSockAddrToStr (Address) ;  { V8.71 }
        if Address.sin6_family = AF_INET then
            IpMask := IpHlpCreateMask (OnLinkPrefixLength)
        else
            IpMask := '/' + IntToStr(OnLinkPrefixLength);
        IpType := IpTypeUnicast ;
        TypeStr := IpPrefixOrigins [Ord(PrefixOrigin)] ;
        if Ord(PrefixOrigin) <> Ord(SuffixOrigin) then
                TypeStr := TypeStr + ', ' + IpSuffixOrigins [Ord(SuffixOrigin)] ;
        SockAddr := Address ;
        IFLuid := InterfaceLuid ;
        IFIndex := InterfaceIndex ;
        PrefixOrig := PrefixOrigin ;
        SuffixOrig := SuffixOrigin ;
        ValidSecs := ValidLifetime ;
        DupliState := DadState ;
        IpScopeId := ScopeId ;
        CreationDT := FileTimeToDateTime (CreationTimeStamp) ;
    end;
end;

//------------------------------------------------------------------------------
{ returns IPv4 and IPv6 addresses for all or some adaptors }
function IpHlpIpAddrTable(var IpAddrInfos: TIpAddrInfos; Family: TAddressFamily = AF_INET;
                               AllIps: Boolean = True; Names: Boolean = True; AdptIdx: TNetIfIndex = 0): integer ;
var
    I, J, NumEntries, CurEntry, TableLen : integer;
    pIPAddrTable: PTMibIPAddrTable;
    PUuicastTable: PMibUnicastIpAddressTable;
    PMulticastTable: PMibMulticastIpAddressTable;
    PAnycastTable: PMibAnycastIpAddressTable;
    IfRows2: TIfRows2 ;
begin
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    CurEntry := 0 ;
    pIPAddrTable := nil ;
    PUuicastTable := Nil ;
    PMulticastTable := nil ;
    PAnycastTable := Nil ;
    SetLength (IpAddrInfos, 0) ;
    TableLen := 0 ;

    try
      // get Unicast table
        result := GetUnicastIpAddressTable (Family, PUuicastTable);
        if result = NO_ERROR then
        begin
            NumEntries := PUuicastTable^.NumEntries;
            TableLen := TableLen + NumEntries ;
            SetLength (IpAddrInfos, TableLen) ;
            if NumEntries > 0 then
            begin
                for I := 0 to Pred (NumEntries) do
                begin
                    if (AdptIdx <> 0) and (AdptIdx <> PUuicastTable^.Table [I].InterfaceIndex) then continue ;
                    IpAddrInfos [CurEntry] := IpHlpConvUniRow (PUuicastTable^.Table [I]) ;
                    inc (CurEntry) ;
                end;
            end;
        end;

      // see if getting multicast and broadcast addresses
        if AllIps then
        begin

      // get Multicast table
            result := GetMulticastIpAddressTable (Family, PMulticastTable);
            if result = NO_ERROR then
            begin
                NumEntries := PMulticastTable^.NumEntries;
                TableLen := TableLen + NumEntries ;
                SetLength (IpAddrInfos, TableLen) ;
                if NumEntries > 0 then
                begin
                    for I := 0 to Pred (NumEntries) do
                    begin
                        if (AdptIdx <> 0) and (AdptIdx <> PMulticastTable^.Table [I].InterfaceIndex) then continue ;
                        with IpAddrInfos [CurEntry], PMulticastTable^.Table [I] do
                        begin
                            IpAddress := WSocketSockAddrToStr (Address) ;  { V8.71 }
                            IpType := IpTypeMulticast ;
                            TypeStr := 'Multicast' ;
                            SockAddr := Address ;
                            IFLuid := InterfaceLuid ;
                            IFIndex := InterfaceIndex ;
                            IpScopeId := ScopeId ;
                        end;
                        inc (CurEntry) ;
                    end;
                end;
            end;

          // get Anycast table
            result := GetAnycastIpAddressTable (Family, PAnycastTable);
            if result = NO_ERROR then
            begin
                NumEntries := PAnycastTable^.NumEntries;
                TableLen := TableLen + NumEntries ;
                SetLength (IpAddrInfos, TableLen) ;
                if NumEntries > 0 then
                begin
                    for I := 0 to Pred (NumEntries) do
                    begin
                        if (AdptIdx <> 0) and (AdptIdx <> PAnycastTable^.Table [I].InterfaceIndex) then continue ;
                        with IpAddrInfos [CurEntry], PAnycastTable^.Table [I] do
                        begin
                            IpAddress := WSocketSockAddrToStr (Address) ;  { V8.71 }
                            IpType := IpTypeAnycast ;
                            TypeStr := 'Anycast' ;
                            SockAddr := Address ;
                            IFLuid := InterfaceLuid ;
                            IFIndex := InterfaceIndex ;
                            IpScopeId := ScopeId ;
                        end;
                        inc (CurEntry) ;
                    end;
                end;
            end;
        end;
    finally
        FreeMem (pIPAddrTable);
        if Assigned (PUuicastTable) then
            FreeMibTable (PUuicastTable);
        if Assigned (PMulticastTable) then
            FreeMibTable (PMulticastTable);
        if Assigned (PAnycastTable) then
            FreeMibTable (PAnycastTable);
    end;
    SetLength (IpAddrInfos, CurEntry) ;

// find adaptor names from interface table
    if NOT Names then
        Exit ;
    if IpHlpIfTable2 (NumEntries, IfRows2) <> 0 then Exit ;
    if NumEntries = 0 then Exit ;
    for I := 0 to Pred (CurEntry) do
    begin
        for J := 0 to Pred (NumEntries) do
        begin
            if IpAddrInfos [I].IFIndex = IfRows2 [J].Mib.InterfaceIndex then
            begin
                with IpAddrInfos [I] do
                begin
                    InterfaceName := IfRows2 [J].InterfaceName ;
                    Description := IfRows2 [J].Description ;
                    FriendlyName := IfRows2 [J].FriendlyName ;
                    MacAddress :=  PhysMacAddr2Str(IfRows2 [J].Mib.PhysicalAddress, IfRows2 [J].Mib.PhysicalAddressLength);   // May 2023
                    MacVendor := IcsGetMacVendor(MacAddress);    // May 2023
                    Break ;
                end;
            end;
        end;
    end;
    SetLength(IfRows2, 0);
end;


//------------------------------------------------------------------------------
{ returns addresses for all adaptors }
procedure Get_IPAddrTable( List: TStrings );
var
    IpAddrInfos: TIpAddrInfos ;
    ErrorCode, NumEntries, I: integer;
begin
    if not Assigned( List ) then EXIT;
    List.Clear;
    ErrorCode := IpHlpIpAddrTable (IpAddrInfos, AF_UNSPEC) ; // IPv4 and IPv6
    if ErrorCode <> NO_ERROR then
    begin
        List.Add (SysErrorMessage (ErrorCode));
        exit ;
    end;
    NumEntries := Length (IpAddrInfos) ;
    if NumEntries = 0 then
    begin
        List.Add ('No IP Addresses') ;
        exit ;
    end ;

    List.Add( 'IP Addresses Table');
    List.Add( Format( '%-38s|%-15s|%-22s|%-14s|%-20s|%-20s|%-40s|%-30s',
              ['IP Address', 'IP Mask', 'Type', 'Interface', 'MAC Address','Vendor','Description', 'Friendly Name'] ) );   // May 2023
     List.Add('');
    for I:= 0 to Pred (NumEntries) do
    begin
        with IpAddrInfos [I] do
          List.Add( Format( '%-38s|%-15s|%-22s|%-14s|%-20s|%-20s|%-40s|%-30s',
              [IpAddress, IpMask, TypeStr, InterfaceName, MacAddress, MacVendor, Copy (Description, 1, 40), Copy (FriendlyName, 1, 30)] ) );   // May 2023
    end ;
end;


//------------------------------------------------------------------------------
// find network adaptor that hosts a specific IP address
function IpHlpAdpforIP (const IpAddr: String): TNetLuid;   // May 2023
var
    IpAddrInfos: TIpAddrInfos ;
    ErrorCode, NumEntries, I: integer;
begin
    Result.Value := 0;
    ErrorCode := IpHlpIpAddrTable (IpAddrInfos, AF_UNSPEC) ; // IPv4 and IPv6
    if ErrorCode <> NO_ERROR then
        exit ;
    NumEntries := Length (IpAddrInfos) ;
    if NumEntries = 0 then
        exit ;
    for I:= 0 to Pred (NumEntries) do
    begin
        if IpAddrInfos [I].IpAddress = IpAddr then begin
            Result := IpAddrInfos [I].IFLuid;
            Exit;
        end;
    end;
end;


//------------------------------------------------------------------------------
// May 2023 - get route protocol description
function IpHlpGetRouteProtocol(Id: Integer): String;
var
    I: Integer;
begin
    Result := 'Unknown';
    for I := Low(NetRouteInfo) to High(NetRouteInfo) do begin
        if NetRouteInfo[I].Id = Id then begin
            Result := NetRouteInfo[I].Str;
            Break;
        end;
    end;
end;


//------------------------------------------------------------------------------
// May 2023 - get table of IP route entries on the local computer.
function IpHlpIPForwardTable(var IpRouteRows: TIpRouteRows; Family: TAddressFamily = AF_INET): DWORD;   // AF_UNSPEC for IPv4 and IPv6
var
    PTable2: PMibIpForwardTable2;
    NumEntries, I: Integer;
begin
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    Result := GetIpForwardTable2(Family, PTable2);
    if Result <> NO_ERROR then
        Exit;
    NumEntries := PTable2^.NumEntries;
    SetLength(IpRouteRows, NumEntries);
    if NumEntries = 0 then
        Exit;
    for I := 0 to NumEntries - 1 do begin
        IpRouteRows[I].DestinationPrefix := PTable2^.table[I].DestinationPrefix;
        IpRouteRows[I].DestIpAddr := WSocketSockAddrToStr(IpRouteRows[I].DestinationPrefix.Prefix);
        if IpRouteRows[I].DestinationPrefix.Prefix.sin6_family = AF_INET then
            IpRouteRows[I].DestMask := IpHlpCreateMask(IpRouteRows[I].DestinationPrefix.PrefixLength)
        else
            IpRouteRows[I].DestMask := '/' + IntToStr(IpRouteRows[I].DestinationPrefix.PrefixLength);
        IpRouteRows[I].NextHop := PTable2^.table[I].NextHop;
        IpRouteRows[I].NextIpAddr := WSocketSockAddrToStr(IpRouteRows[I].NextHop);
        IpRouteRows[I].IFIndex := PTable2^.table[I].InterfaceIndex;
        IpRouteRows[I].IFLuid := PTable2^.table[I].InterfaceLuid;
        IpRouteRows[I].IfType := IpRouteRows[I].IFLuid.IfType;
        IpRouteRows[I].IfTypeDesc := AdaptTypes[IpRouteRows[I].IfType];
        IpRouteRows[I].InterfaceName := IpHlpConvIntLuidToAlias(IpRouteRows[I].IFLuid);
        IpRouteRows[I].SitePrefixLength := PTable2^.table[I].SitePrefixLength;
        IpRouteRows[I].ValidLifetime := PTable2^.table[I].ValidLifetime;
        IpRouteRows[I].PreferredLifetime := PTable2^.table[I].PreferredLifetime;
        IpRouteRows[I].Metric := PTable2^.table[I].Metric;
        IpRouteRows[I].Protocol := PTable2^.table[I].Protocol;
        IpRouteRows[I].ProtDesc := IpHlpGetRouteProtocol(IpRouteRows[I].Protocol);
        IpRouteRows[I].Loopback := PTable2^.table[I].Loopback;
        IpRouteRows[I].AutoconfigureAddress := PTable2^.table[I].AutoconfigureAddress;
        IpRouteRows[I].Publish := PTable2^.table[I].Publish;
        IpRouteRows[I].Immortal := PTable2^.table[I].Immortal;
        IpRouteRows[I].Age := PTable2^.table[I].Age;
        IpRouteRows[I].Origin := PTable2^.table[I].Origin;
        IpRouteRows[I].OrigDesc := NetRouteOrigins[PTable2^.table[I].Origin];
    end;
    FreeMibTable(PTable2);
end;


//-----------------------------------------------------------------------------
{ May 2023 - gets entries in routing table; equivalent to "Route Print" }
procedure Get_IPForwardTable( List: TStrings );
var
    IpRouteRows: TIpRouteRows;
    I, NumEntries, ErrorCode: Integer;
begin
    if not Assigned(List) then
        EXIT;
    List.Clear;
    ErrorCode := IpHlpIPForwardTable(IpRouteRows, AF_UNSPEC);
    if ErrorCode = NO_ERROR then begin
        NumEntries := Length(IpRouteRows);
        if NumEntries > 0 then begin
            List.Add('IP Route Forwarding Table');
            List.Add(Format('%-40s|%-16s|%-30s|%-17s|%-7s|%-14s|%-30s|%-10s',
                         ['Destination Prefix', 'Mask/Length', 'Next Hop Address', 'Route Protocol', 'Metric', 'Route Origin', 'Interface','IF Type']));
            for I := 0 to NumEntries - 1 do begin
                with IpRouteRows[I] do begin
                    List.Add( Format('%-40s|%-16s|%-30s|%-17s|%-7d|%-14s|%-30s|%-10s',
                        [ DestIpAddr, DestMask, NextIpAddr, ProtDesc, Metric, OrigDesc, InterfaceName, IfTypeDesc] ) );
                end ;
            end;
        end
        else
            List.Add( 'no entries.' );
    end
    else
        List.Add( SysErrorMessage( ErrorCode ) );
end;


//------------------------------------------------------------------------------
// May 2023 IP path table on the local computer
function IpHlpIpPathTable(var IpPathRows: TIpPathRows; Family: TAddressFamily = AF_INET): DWORD;  // May 2023  // AF_UNSPEC for IPv4 and IPv6
var
    PTable: PMibIpPathTable;
    NumEntries, I: Integer;
begin
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    Result := GetIpPathTable(Family, PTable);
    if Result <> NO_ERROR then
        Exit;
    NumEntries := PTable^.NumEntries;
    SetLength(IpPathRows, NumEntries);
    if NumEntries = 0 then
        Exit;
    for I := 0 to NumEntries - 1 do begin
        IpPathRows[I].Source := PTable^.table[I].Source;
        IpPathRows[I].SourceIpAddr := WSocketSockAddrToStr(IpPathRows[I].Source);
        IpPathRows[I].Destination := PTable^.table[I].Destination;
        IpPathRows[I].DestIpAddr := WSocketSockAddrToStr(IpPathRows[I].Destination);
        IpPathRows[I].CurrentNextHop := PTable^.table[I].CurrentNextHop;
        IpPathRows[I].CurNextHopIpAddr := WSocketSockAddrToStr(IpPathRows[I].CurrentNextHop);
        IpPathRows[I].IFIndex := PTable^.table[I].InterfaceIndex;
        IpPathRows[I].IFLuid := PTable^.table[I].InterfaceLuid;
        IpPathRows[I].IfType := IpPathRows[I].IFLuid.IfType;
        IpPathRows[I].IfTypeDesc := AdaptTypes[IpPathRows[I].IfType];
        IpPathRows[I].InterfaceName := IpHlpConvIntLuidToAlias(IpPathRows[I].IFLuid);
        IpPathRows[I].PathMtu := PTable^.table[I].PathMtu;
        IpPathRows[I].RttMean := PTable^.table[I].RttMean;
        IpPathRows[I].RttDeviation := PTable^.table[I].RttDeviation;
        IpPathRows[I].ReachSecs := PTable^.table[I].ReachabilityTime.LastReachable div 1000;
        IpPathRows[I].IsReachable := PTable^.table[I].IsReachable;
        IpPathRows[I].LinkTransmitSpeed := PTable^.table[I].LinkTransmitSpeed;
        IpPathRows[I].LinkReceiveSpeed := PTable^.table[I].LinkReceiveSpeed;
    end;
    FreeMibTable(PTable);
end;


//------------------------------------------------------------------------------
// May 2023 IP path table on the local computer
procedure Get_IpPathTable(List: TStrings);                // May 2023
var
    IpPathRows: TIpPathRows;
    I, NumEntries, ErrorCode: Integer;
begin
    if not Assigned(List) then
        EXIT;
    List.Clear;
    ErrorCode := IpHlpIPPathTable(IpPathRows, AF_UNSPEC);
    if ErrorCode = NO_ERROR then begin
        NumEntries := Length(IpPathRows);
        if NumEntries > 0 then begin
            List.Add('IP Path Table');
            List.Add(Format('%-40s|%-40s|%-40s|%-7s|%-7s|%-7s|%-12s|%-12s|%-30s|%-10s',
                         ['Source Address', 'Destination Address', 'Next Hop Address', 'MTU', 'RTT', 'Reach', 'Link Xmit', 'Link Recv', 'Interface','IF Type']));
            for I := 0 to NumEntries - 1 do begin
                with IpPathRows[I] do begin
                    List.Add( Format('%-40s|%-40s|%-40s|%-7d|%-7d|%-7s|%-12d|%-12d|%-30s|%-10s',
                        [ SourceIpAddr, DestIpAddr, CurNextHopIpAddr, PathMtu, RttMean, IcsGetYN(IsReachable), LinkTransmitSpeed, LinkReceiveSpeed, InterfaceName, IfTypeDesc] ) );
                end ;
            end;
        end
        else
            List.Add( 'no entries.' );
    end
    else
        List.Add( SysErrorMessage( ErrorCode ) );
end;


//------------------------------------------------------------------------------
procedure Get_IPStatistics( List: TStrings );
var
  IPStats       : TMibIPStats;
  ErrorCode     : integer;
begin
  if not Assigned( List ) then EXIT;
  if NOT LoadIpHlp then exit ;
  ErrorCode := GetIPStatistics( @IPStats );
  if ErrorCode = NO_ERROR then
  begin
    List.Clear;
    with IPStats do
    begin
      List.Add( 'IP Statistics');
      if dwForwarding = 1 then
        List.add( 'Forwarding Enabled      : ' + 'Yes' )
      else
        List.add( 'Forwarding Enabled      : ' + 'No' );
      List.add( 'Default TTL             : ' + inttostr( dwDefaultTTL ) );
      List.add( 'Datagrams Received      : ' + inttostr( dwInReceives ) );
      List.add( 'Header Errors     (In)  : ' + inttostr( dwInHdrErrors ) );
      List.add( 'Address Errors    (In)  : ' + inttostr( dwInAddrErrors ) );
      List.add( 'Datagrams Forwarded     : ' + inttostr( dwForwDatagrams ) );   // Angus
      List.add( 'Unknown Protocols (In)  : ' + inttostr( dwInUnknownProtos ) );
      List.add( 'Datagrams Discarded     : ' + inttostr( dwInDiscards ) );
      List.add( 'Datagrams Delivered     : ' + inttostr( dwInDelivers ) );
      List.add( 'Requests Out            : ' + inttostr( dwOutRequests ) );
      List.add( 'Routings Discarded      : ' + inttostr( dwRoutingDiscards ) );
      List.add( 'No Routes          (Out): ' + inttostr( dwOutNoRoutes ) );
      List.add( 'Reassemble TimeOuts     : ' + inttostr( dwReasmTimeOut ) );
      List.add( 'Reassemble Requests     : ' + inttostr( dwReasmReqds ) );
      List.add( 'Succesfull Reassemblies : ' + inttostr( dwReasmOKs ) );
      List.add( 'Failed Reassemblies     : ' + inttostr( dwReasmFails ) );
      List.add( 'Succesful Fragmentations: ' + inttostr( dwFragOKs ) );
      List.add( 'Failed Fragmentations   : ' + inttostr( dwFragFails ) );
      List.add( 'Datagrams Fragmented    : ' + inttostr( dwFRagCreates ) );
      List.add( 'Number of Interfaces    : ' + inttostr( dwNumIf ) );
      List.add( 'Number of IP-addresses  : ' + inttostr( dwNumAddr ) );
      List.add( 'Routes in RoutingTable  : ' + inttostr( dwNumRoutes ) );
    end;
  end
  else
    List.Add( SysErrorMessage( ErrorCode ) );
end;


//-----------------------------------------------------------------------------
function IpHlpIPStatistics (var IPStats: TMibIPStats): integer ;      // Angus
begin
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    result := GetIPStatistics( @IPStats );
end ;


//------------------------------------------------------------------------------
procedure Get_UdpStatistics( List: TStrings );
var
  UdpStats      : TMibUDPStats;
  ErrorCode     : integer;
begin
  if NOT LoadIpHlp then exit ;
  if not Assigned( List ) then EXIT;
  ErrorCode := GetUDPStatistics( @UdpStats );
  if ErrorCode = NO_ERROR then
  begin
    List.Clear;
    with UDPStats do
    begin
      List.Add( 'UDP Statistics');
      List.add( 'Datagrams (In)    : ' + inttostr( dwInDatagrams ) );
      List.add( 'Datagrams (Out)   : ' + inttostr( dwOutDatagrams ) );
      List.add( 'No Ports          : ' + inttostr( dwNoPorts ) );
      List.add( 'Errors    (In)    : ' + inttostr( dwInErrors ) );
      List.add( 'UDP Listen Ports  : ' + inttostr( dwNumAddrs ) );
    end;
  end
  else
    List.Add( SysErrorMessage( ErrorCode ) );
end;


//-----------------------------------------------------------------------------
function IpHlpUdpStatistics (UdpStats: TMibUDPStats): integer ;          // Angus
begin
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    result := GetUDPStatistics (@UdpStats) ;
end ;

//------------------------------------------------------------------------------
procedure Get_ICMPStats( ICMPIn, ICMPOut: TStrings );
var
  ErrorCode     : DWORD;
  ICMPStats     : PTMibICMPInfo;
begin
  if NOT LoadIpHlp then exit ;
  if ( ICMPIn = nil ) or ( ICMPOut = nil ) then EXIT;
  ICMPIn.Clear;
  ICMPOut.Clear;
  New( ICMPStats );
  ErrorCode := GetICMPStatistics(ICMPStats);
  if ErrorCode = NO_ERROR then
  begin
    with ICMPStats.InStats do
    begin
      ICMPIn.Add( 'ICMP Statistics - Input');
      ICMPIn.Add( 'Messages received    : ' + IntToStr( dwMsgs ) );
      ICMPIn.Add( 'Errors               : ' + IntToStr( dwErrors ) );
      ICMPIn.Add( 'Dest. Unreachable    : ' + IntToStr( dwDestUnreachs ) );
      ICMPIn.Add( 'Time Exceeded        : ' + IntToStr( dwTimeEcxcds ) );
      ICMPIn.Add( 'Param. Problems      : ' + IntToStr( dwParmProbs ) );
      ICMPIn.Add( 'Source Quench        : ' + IntToStr( dwSrcQuenchs ) );
      ICMPIn.Add( 'Redirects            : ' + IntToStr( dwRedirects ) );
      ICMPIn.Add( 'Echo Requests        : ' + IntToStr( dwEchos ) );
      ICMPIn.Add( 'Echo Replies         : ' + IntToStr( dwEchoReps ) );
      ICMPIn.Add( 'Timestamp Requests   : ' + IntToStr( dwTimeStamps ) );
      ICMPIn.Add( 'Timestamp Replies    : ' + IntToStr( dwTimeStampReps ) );

      ICMPIn.Add( 'Addr. Masks Requests : ' + IntToStr( dwAddrMasks ) );
      ICMPIn.Add( 'Addr. Mask Replies   : ' + IntToStr( dwAddrReps ) );
    end;

    with ICMPStats.OutStats do
    begin
      ICMPOut.Add( 'ICMP Statistics - Output');
      ICMPOut.Add( 'Messages sent        : ' + IntToStr( dwMsgs ) );
      ICMPOut.Add( 'Errors               : ' + IntToStr( dwErrors ) );
      ICMPOut.Add( 'Dest. Unreachable    : ' + IntToStr( dwDestUnreachs ) );
      ICMPOut.Add( 'Time Exceeded        : ' + IntToStr( dwTimeEcxcds ) );
      ICMPOut.Add( 'Param. Problems      : ' + IntToStr( dwParmProbs ) );
      ICMPOut.Add( 'Source Quench        : ' + IntToStr( dwSrcQuenchs ) );
      ICMPOut.Add( 'Redirects            : ' + IntToStr( dwRedirects ) );
      ICMPOut.Add( 'Echo Requests        : ' + IntToStr( dwEchos ) );
      ICMPOut.Add( 'Echo Replies         : ' + IntToStr( dwEchoReps ) );
      ICMPOut.Add( 'Timestamp Requests   : ' + IntToStr( dwTimeStamps ) );
      ICMPOut.Add( 'Timestamp Replies    : ' + IntToStr( dwTimeStampReps ) );
      ICMPOut.Add( 'Addr. Masks Requests : ' + IntToStr( dwAddrMasks ) );
      ICMPOut.Add( 'Addr. Mask Replies   : ' + IntToStr( dwAddrReps ) );
    end;
  end
  else
    IcmpIn.Add( SysErrorMessage( ErrorCode ) );
  Dispose( ICMPStats );
end;


//------------------------------------------------------------------------------
procedure Get_ICMPStatsEx( ICMPIn, ICMPOut: TStrings );
var
  ErrorCode     : DWORD;
  ICMPStats     : PTMibICMPExInfo;
begin
  if NOT LoadIpHlp then exit ;
  if ( ICMPIn = nil ) or ( ICMPOut = nil ) then EXIT;
  ICMPIn.Clear;
  ICMPOut.Clear;
  New( ICMPStats );
  ErrorCode := GetICMPStatisticsEx(ICMPStats);   // May 2023 was
  if ErrorCode = NO_ERROR then
  begin
    with ICMPStats.InStats do
    begin
      ICMPIn.Add( 'ICMP Statistics - Input');
      ICMPIn.Add( 'Messages received    : ' + IntToStr( dwMsgs ) );
      ICMPIn.Add( 'Errors               : ' + IntToStr( dwErrors ) );
    end;
    with ICMPStats.OutStats do
    begin
      ICMPOut.Add( 'ICMP Statistics - Output');
      ICMPOut.Add( 'Messages sent        : ' + IntToStr( dwMsgs ) );
      ICMPOut.Add( 'Errors               : ' + IntToStr( dwErrors ) );
    end;
  end
  else
    IcmpIn.Add( SysErrorMessage( ErrorCode ) );
  Dispose( ICMPStats );
end;


//------------------------------------------------------------------------------
// July 2023 new component to monitor IP address changes, based on old functions
constructor TIcsIpChanges.Create(AOwner: TComponent);
begin
    Inherited;
    AllocateHWnd;
    FDevFamily := AF_UNSPEC;
    FNotificationHandle := 0;
end;


//------------------------------------------------------------------------------
destructor TIcsIpChanges.Destroy;
begin
    StopMonitor;
    Inherited;
end;


//------------------------------------------------------------------------------
function TIcsIpChanges.MsgHandlersCount : Integer;
begin
    Result := 1 + inherited MsgHandlersCount;
end;


//------------------------------------------------------------------------------
procedure TIcsIpChanges.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_IPADDRCHANGE := FWndHandler.AllocateMsgHandler(Self);
end;


//------------------------------------------------------------------------------
procedure TIcsIpChanges.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then
        FWndHandler.UnregisterMessage(FMsg_WM_IPADDRCHANGE);
    inherited FreeMsgHandlers;
end;


//------------------------------------------------------------------------------
procedure IpAddressChangeCallback (CallerContext: Pointer; Row: PMibUnicastIpAddressRow;
                                                                      NotificationType: TMibNoticationType); stdcall;
var
    MyIcsIpChanges: TIcsIpChanges;
begin
    if CallerContext = Nil then
        Exit;
    MyIcsIpChanges := TIcsIpChanges(CallerContext);
    if NOT Assigned(MyIcsIpChanges.FIpChangesEvent) then
        Exit;
    SendMessage(MyIcsIpChanges.Handle, MyIcsIpChanges.FMsg_WM_IPADDRCHANGE, WPARAM(NotificationType), LPARAM(Row));
end;

procedure TIcsIpChanges.WndProc(var MsgRec: TMessage);
var
    PartRow: PMibUnicastIpAddressRow;
    FullRow: TMibUnicastIpAddressRow;
    NotificationType: TMibNoticationType;
    IpAddrInfo: TIpAddrInfo;
    Ret: Integer;
begin
    with MsgRec do begin
        if Msg = FMsg_WM_IPADDRCHANGE then begin
            try
                NotificationType := TMibNoticationType(MsgRec.WParam);
                PartRow := PMibUnicastIpAddressRow(MsgRec.LParam);
                if (NotificationType <> MibInitialNotification) and Assigned (PartRow) then begin
                    FullRow.Address := PartRow^.Address;
                    FullRow.InterfaceLuid := PartRow^.InterfaceLuid;
                    FullRow.InterfaceIndex := PartRow^.InterfaceIndex;
                    Ret := GetUnicastIpAddressEntry(FullRow);
                    IpAddrInfo := IpHlpConvUniRow(FullRow);
                    IpAddrInfo.Description := IntToStr(Ret);    // !! TEMP DIAG
                end;
                FIpChangesEvent(IpAddrInfo, NotificationType);
            except
            end;
        end
        else
           inherited WndProc(MsgRec);
    end;
end;


//------------------------------------------------------------------------------
function TIcsIpChanges.StartMonitor: Integer ;
begin
    Result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then
        Exit ;
    if NOT Assigned(FIpChangesEvent) then
        Exit;
    StopMonitor;
    Result := NotifyUnicastIpAddressChange(FDevFamily, @IpAddressChangeCallback, Self, True, FNotificationHandle);
end ;


//------------------------------------------------------------------------------
procedure TIcsIpChanges.StopMonitor;
begin
    if FNotificationHandle = 0 then
        Exit;
    if NOT LoadIpHlp then
        Exit ;
    if NOT Assigned(CancelMibChangeNotify2) then
        Exit;
    CancelMibChangeNotify2(FNotificationHandle);
    FNotificationHandle := 0;
end ;


//------------------------------------------------------------------------------
function LoadIpHlp: Boolean;
begin
    Result := True;
    if IpHlpModule <> 0 then Exit;

// open DLL
    IpHlpModule := LoadLibrary (IpHlpDLL);
    if IpHlpModule = 0 then
    begin
        Result := false;
        exit ;
    end ;
    GetAdaptersInfo := GetProcAddress (IpHlpModule, 'GetAdaptersInfo') ;
    GetNetworkParams := GetProcAddress (IpHlpModule, 'GetNetworkParams') ;
    GetTcpTable := GetProcAddress (IpHlpModule, 'GetTcpTable') ;
    GetTcpStatistics := GetProcAddress (IpHlpModule, 'GetTcpStatistics') ;
    GetUdpTable := GetProcAddress (IpHlpModule, 'GetUdpTable') ;
    GetUdpStatistics := GetProcAddress (IpHlpModule, 'GetUdpStatistics') ;
    GetIpStatistics := GetProcAddress (IpHlpModule, 'GetIpStatistics') ;
    GetIpNetTable := GetProcAddress (IpHlpModule, 'GetIpNetTable') ;
    GetIpAddrTable := GetProcAddress (IpHlpModule, 'GetIpAddrTable') ;
    GetIpForwardTable := GetProcAddress (IpHlpModule, 'GetIpForwardTable') ;
    GetIcmpStatistics := GetProcAddress (IpHlpModule, 'GetIcmpStatistics') ;
    GetRTTAndHopCount := GetProcAddress (IpHlpModule, 'GetRTTAndHopCount') ;
    GetIfTable := GetProcAddress (IpHlpModule, 'GetIfTable') ;
    GetIfEntry := GetProcAddress (IpHlpModule, 'GetIfEntry') ;
    GetFriendlyIfIndex := GetProcAddress (IpHlpModule, 'GetFriendlyIfIndex') ;
    GetPerAdapterInfo := GetProcAddress (IpHlpModule, 'GetPerAdapterInfo') ;
    GetAdaptersAddresses := GetProcAddress (IpHlpModule, 'GetAdaptersAddresses') ;
    GetExtendedTcpTable := GetProcAddress (IpHlpModule, 'GetExtendedTcpTable') ;
    GetOwnerModuleFromTcpEntry := GetProcAddress (IpHlpModule, 'GetOwnerModuleFromTcpEntry') ;
    GetExtendedUdpTable := GetProcAddress (IpHlpModule, 'GetExtendedUdpTable') ;
    GetOwnerModuleFromUdpEntry := GetProcAddress (IpHlpModule, 'GetOwnerModuleFromUdpEntry') ;

    GetBestRoute := GetProcAddress ( IpHlpModule, 'GetBestRoute');
    NotifyAddrChange := GetProcAddress (IpHlpModule, 'NotifyAddrChange') ;
    NotifyRouteChange := GetProcAddress (IpHlpModule, 'NotifyRouteChange') ;
    NotifyRouteChange2 := GetProcAddress (IpHlpModule, 'NotifyRouteChange2') ;
    CancelIPChangeNotify := GetProcAddress (IpHlpModule, 'CancelIPChangeNotify') ;
    NotifyIpInterfaceChange := GetProcAddress (IpHlpModule, 'NotifyIpInterfaceChange') ;
    NotifyUnicastIpAddressChange := GetProcAddress (IpHlpModule, 'NotifyUnicastIpAddressChange') ;
    CancelMibChangeNotify2 := GetProcAddress (IpHlpModule, 'CancelMibChangeNotify2') ;
    GetIfTable2 := GetProcAddress (IpHlpModule, 'GetIfTable2') ;
    GetIfTable2Ex := GetProcAddress (IpHlpModule, 'GetIfTable2Ex') ;
    GetIfEntry2 := GetProcAddress (IpHlpModule, 'GetIfEntry2') ;
    GetIpForwardTable2 := GetProcAddress (IpHlpModule, 'GetIpForwardTable2') ;
    GetTcp6Table := GetProcAddress (IpHlpModule, 'GetTcp6Table') ;
    GetTcpStatisticsEx := GetProcAddress (IpHlpModule, 'GetTcpStatisticsEx') ;
    GetUdp6Table := GetProcAddress (IpHlpModule, 'GetUdp6Table') ;
    GetUdpStatisticsEx := GetProcAddress (IpHlpModule, 'GetUdpStatisticsEx') ;
    GetOwnerModuleFromTcp6Entry := GetProcAddress (IpHlpModule, 'GetOwnerModuleFromTcp6Entry') ;
    GetOwnerModuleFromUdp6Entry := GetProcAddress (IpHlpModule, 'GetOwnerModuleFromUdp6Entry') ;
    CreateUnicastIpAddressEntry := GetProcAddress (IpHlpModule, 'CreateUnicastIpAddressEntry') ;
    DeleteUnicastIpAddressEntry := GetProcAddress (IpHlpModule, 'DeleteUnicastIpAddressEntry') ;
    GetUnicastIpAddressEntry := GetProcAddress (IpHlpModule, 'GetUnicastIpAddressEntry') ;
    GetUnicastIpAddressTable := GetProcAddress (IpHlpModule, 'GetUnicastIpAddressTable') ;
    InitializeUnicastIpAddressEntry := GetProcAddress (IpHlpModule, 'InitializeUnicastIpAddressEntry') ;
    NotifyUnicastIpAddressChange := GetProcAddress (IpHlpModule, 'NotifyUnicastIpAddressChange') ;
    NotifyStableUnicastIpAddressTable := GetProcAddress (IpHlpModule, 'NotifyStableUnicastIpAddressTable') ;
    SetUnicastIpAddressEntry := GetProcAddress (IpHlpModule, 'SetUnicastIpAddressEntry') ;
    CreateAnycastIpAddressEntry := GetProcAddress (IpHlpModule, 'CreateAnycastIpAddressEntry') ;
    DeleteAnycastIpAddressEntry := GetProcAddress (IpHlpModule, 'DeleteAnycastIpAddressEntry') ;
    GetAnycastIpAddressEntry := GetProcAddress (IpHlpModule, 'GetAnycastIpAddressEntry') ;
    GetAnycastIpAddressTable := GetProcAddress (IpHlpModule, 'GetAnycastIpAddressTable') ;
    GetMulticastIpAddressEntry := GetProcAddress (IpHlpModule, 'GetMulticastIpAddressEntry') ;
    GetMulticastIpAddressTable := GetProcAddress (IpHlpModule, 'GetMulticastIpAddressTable') ;
    FreeMibTable := GetProcAddress (IpHlpModule, 'FreeMibTable') ;
    ConvertInterfaceNameToLuidW := GetProcAddress (IpHlpModule, 'ConvertInterfaceNameToLuidW') ;
    ConvertInterfaceLuidToNameW := GetProcAddress (IpHlpModule, 'ConvertInterfaceLuidToNameW') ;
    ConvertInterfaceIndexToLuid := GetProcAddress (IpHlpModule, 'ConvertInterfaceIndexToLuid') ;
// April 2023
    CreateIpNetEntry2 := GetProcAddress (IpHlpModule, 'CreateIpNetEntry2') ;
    GetIpNetEntry2 := GetProcAddress (IpHlpModule, 'GetIpNetEntry2') ;
    ResolveIpNetEntry2 := GetProcAddress (IpHlpModule, 'ResolveIpNetEntry2') ;
    SetIpNetEntry2 := GetProcAddress (IpHlpModule, 'SetIpNetEntry2') ;
    GetIpNetTable2 := GetProcAddress (IpHlpModule, 'GetIpNetTable2') ;
    FlushIpNetTable2 := GetProcAddress (IpHlpModule, 'FlushIpNetTable2') ;
    ConvertLengthToIpv4Mask := GetProcAddress (IpHlpModule, 'ConvertLengthToIpv4Mask') ;
    GetIpPathTable := GetProcAddress (IpHlpModule, 'GetIpPathTable') ;
    ConvertInterfaceLuidToAlias := GetProcAddress (IpHlpModule, 'ConvertInterfaceLuidToAlias') ;
    GetIpPathEntry := GetProcAddress (IpHlpModule, 'GetIpPathEntry') ;
    GetIcmpStatisticsEx := GetProcAddress (IpHlpModule, 'GetIcmpStatisticsEx') ;
    GetIpStatisticsEx := GetProcAddress (IpHlpModule, 'GetIpStatisticsEx') ;
    GetTcpStatisticsEx2 := GetProcAddress (IpHlpModule, 'GetTcpStatisticsEx2') ;  // Windows 10
    GetUdpStatisticsEx2 := GetProcAddress (IpHlpModule, 'GetUdpStatisticsEx2') ;  // Windows 10

end;

initialization
    IpHlpModule := 0 ;
finalization
    if IpHlpModule <> 0 then
    begin
        FreeLibrary (IpHlpModule) ;
        IpHlpModule := 0 ;
    end ;
end.
