{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Ethernet, IP and transport layer literals and functions.
Creation:     March 2009
Updated:      Aug 2023
Version:      V9.0
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2002-2023 by Angus Robertson, Magenta Systems Ltd,
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

This unit contains structures and literals for the various low level packets that
make up ethernet protocols using the OSI model of layers, specifically layers 2 to 4.
Layers 5 and 6 are handled by WSocket, Layer 7 by other ICS units.

Layer 1 - Physical Layer - hardware
Layer 2 - Data Link Layer - THdrEthernet (14 bytes) (MAC addresses)
Layer 3 - Network Layer - THdrIP (20 bytes), THdrIPv6 (40+ bytes), ARP, PPP (IP addresses)
Layer 4 - Transport Layer - THdrTcp (20 bytes), THdrUdp (8 bytes), ICMP (IP ports)
Layer 5 - Session Layer - Sockets (handles)
Layer 6 - Presentation Layer - TLS/SSL, MIME, Telnet
Layer 7 - Application Layer - HTTP, FTP, SMTP, POP3, etc


Some of the TCP/IP headers are taken from 'Hands-On TCP/IP Programming' by Alfred
Mirzagotov in The Delphi Magazine January 2004.

8 Aug 2008 - 1.2
Updated to support ICS V6 and V7, and Delphi 2009

Jul 24, 2023 - V8.71 Updated units for main ICS library.
                     Changed ethernet literals from PROTO_ to ETHERNET_ for clarity.
                     Added PPP ethernet literals.
                     Added IPv6 support.
                     Added more known service ports.
                     Load Organizationally Unique Network Interface Identifier (OUI) MAC vendor
                       list from https://linuxnet.ca/ieee/oui/nmap-mac-prefixes (nmap-mac-prefixes.txt),
                       lookup using IcsGetMacVendor.
Aug 08, 2023 V9.0  Updated version to major release 9.


Pending, load latest common ports list.




 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsIpUtils;
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

uses
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
{$IFDEF FMX}
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocket,
{$ELSE}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    Z.ICS9.OverbyteIcsWSocket,
{$ENDIF}
    Z.ICS9.OverbyteIcsUtils;

const
    CopyRight    : String     = ' IcsIpUtils  (c) 2023 V9.0 ';

const

// local network IPs in format suitable for simple range checking
  IP10_0_0_0 = 167772160;
  IP10_255_255_255 = 184549375;
  IP100_64_0_0 = 1681915904;          // CNAT
  IP100_127_255_255 = 1686110207;     // CNAT
  IP127_0_0_0 = 2130706432;
  IP127_255_255_255 = 2147483647;
  IP169_254_1_0 = 2851995904;
  IP169_254_254_255 = 2852060927;
  IP172_16_0_0 = 2886729728;
  IP172_31_255_255 = 2887778303;
  IP192_168_0_0 = 3232235520;
  IP192_168_255_255 = 3232301055;
  IP224_0_0_0 = 3758096384;           // multicast
  IP239_255_255_255 = 4026531839;     // multicast
  IP6_FF00 = $FF00;    // multicast
  IP6_FFFF = $FFFF;    // multicast
  IP6_FE80 = $FE80;    // link local

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

type
  TMacAddr = array [0..5] of Byte ;  // a MAC address

type
  THdrEthernet = packed record   // Ethernet frame header - Network Interface Layer 14 bytes
    dmac: TMacAddr;
    smac: TMacAddr;
    protocol: Word;
  end;
  PHdrEthernet = ^THdrEthernet ;

const     //rfc1340 ethernet protocols   Feb 2023 changed from PROTO_ to ETHERNET_ for clarity
  ETHERNET_PUP     =    $0200;
  ETHERNET_XNS     =    $0600;
  ETHERNET_IP      =    $0800;
  ETHERNET_ARP     =    $0806;
  ETHERNET_REVARP  =    $0835;
  ETHERNET_SCA     =    $6007;
  ETHERNET_ATALK   =    $809B;
  ETHERNET_AARP    =    $80F3;
  ETHERNET_IPX     =    $8137;
  ETHERNET_VLAN    =    $8100;    // Feb 2023  IEEE 802.1Q VLAN tagging
  ETHERNET_NOVELL  =    $8138;
  ETHERNET_SNMP    =    $814C;
  ETHERNET_IPV6    =    $86DD;
  ETHERNET_PPP     =    $880B;    // Feb 2023
  ETHERNET_PPPoEd  =    $8863;    // Feb 2023
  ETHERNET_PPPoEs  =    $8864;    // Feb 2023
  ETHERNET_XIMETA  =    $88AD;
  ETHERNET_LOOP    =    $900D;

  OFFSET_IP =   14;   // length of ethernet frame header
  IPV6HDR_LEN = 40;

  TCP_FLAG_FIN =    $01;   // TCP flags
  TCP_FLAG_SYN =    $02;
  TCP_FLAG_RST =    $04;
  TCP_FLAG_PSH =    $08;
  TCP_FLAG_ACK =    $10;
  TCP_FLAG_URG =    $20;
  TCP_FLAG_ECH =    $40;
  TCP_FLAG_CWR =    $80;

type

  THdrIP = packed record   // IP header (RFC 791) - Internet Layer     20 bytes
    ihl_ver   : BYTE;        // Combined field:
                           //   ihl:4 - IP header length divided by 4
                           //   version:4 - IP version
    tos       : BYTE;        // IP type-of-service field
    tot_len   : WORD;        // total length
    id        : WORD;        // unique ID
    frag_off  : WORD;        // Fragment Offset + fragmentation flags (3 bits)
    ttl       : BYTE;        // time to live
    protocol  : BYTE;        // protocol type
    check     : WORD;        // IP header checksum
    src_addr  : TIcsIPv4Address;     // source IP
    dest_addr : TIcsIPv4Address;     // destination IP
   {The options start here...}
  end;
  PHdrIP = ^THdrIP;

  (* Most of IP header is self-explanatory, but here are some
     extra details for the curious (more in RFC 791):

    -ih.ihl is header length in bytes divided by 4
     Internet Header Length is the length of the internet
     header in 32 bit words, and thus points to the beginning
     of the data.  Note that the minimum value for a correct
     header is 5.

    -ih.tos - IP type-of-service field provides an indication of the
     quality of service desired. Several networks offer service precedence,
     which somehow treats high precedence traffic as more important than
     other traffic (generally by accepting only traffic above a certain
     precedence at time of high load).

    -ih.id  - An identifying value assigned by the sender to aid in
     assembling the fragments of a datagram.

    -ih.frag_off contains 3 bit fragmentation flags and fragment offset.
     These are used to keep track of the pieces when a datagram has to
     be split up. This can happen when datagrams are forwarded through
     a network for which they are too big. See RFC815 about reassembly.
       Bit 0: reserved, must be zero
       Bit 1: (DF) 0 = May Fragment,  1 = Don't Fragment.
       Bit 2: (MF) 0 = Last Fragment, 1 = More Fragments.
       Bits?: indicates where in the datagram this fragment belongs

    -ih.protocol tells IP at the other end to send the datagram
     to TCP. Although most IP traffic uses TCP, there are other
     protocols that can use IP, so you have to tell IP which
     protocol to send the datagram to.

    -ih.check[sum] allows IP at the other end to verify that the header
     wasn't damaged in transit. Note that TCP and IP have separate
     checksums. IP only needs to be able to verify that the header
     didn't get damaged in transit, or it could send a message to
     the wrong place.
   *)

 // Feb 2023 an IPv6 main header  - always 40 bytes
 // may be followed by extension headers according to NEXTHDR_xx and/or upper layer protocol like TCP
 // beware raw sockets in windows does not return this IPv6 header, so we have no idea what upper layer follows or IPv6 addresses
  THdrIPv6 = packed record
    ver_traf1   : BYTE;    // IP version, always 6 - 4 bits
                           // traffic class - first four bits   aka type-of-service field
    traf2_flow1 : BYTE;    // traffic class - second four bits - total 8 bits
                           // flow label - first four bits
    flow2       : BYTE;    // flow label - next eight bits
    flow3       : BYTE;    // flow label - last eight bits - total 20 bits
    payload_len : WORD;    // payload length after this packet, ie extensions, TCP/UDP header and data
    next_hdr    : BYTE;    // next header - NEXTHDR_xx literals
    hop_limit   : BYTE;    // time to live
    src_addr    : TIcsIPv6Address; // source IP, 128 bits
    dest_addr   : TIcsIPv6Address; // destination IP, 128 bits
  end;
  PHdrIPv6 = ^THdrIPv6;

// IPv6 extension record, must be multiple of 8 bytes, may be several linked
// ignoring extension content for now, mostly for routing and delivery
  THdrIPv6Ext = packed record
    next_hdr    : BYTE;    // next header - NEXTHDR_xx literals
    hdr_ext_len : BYTE;    // length of extension, multiple of 8 bytes
    options     : array[0..5] of BYTE;  // ignoring options for now
  end;
  PHdrIPv6Ext = ^THdrIPv6Ext;

const
  NEXTHDR_HOP       = 0;   // Hop-by-hop option header.
  NEXTHDR_TCP       = 6;   // TCP segment.
  NEXTHDR_UDP       = 17;  // UDP message.
  NEXTHDR_IPV6      = 41;  // IPv6 in IPv6
  NEXTHDR_ROUTING   = 43;  // Routing header.
  NEXTHDR_FRAGMENT  = 44;  // Fragmentation/reassembly header.
  NEXTHDR_GRE       = 47;  // GRE header.
  NEXTHDR_ESP       = 50;  // Encapsulating security payload.
  NEXTHDR_AUTH      = 51;  // Authentication header.
  NEXTHDR_ICMP      = 58;  // ICMP for IPv6.
  NEXTHDR_NONE      = 59;  // No next header
  NEXTHDR_DEST      = 60;  // Destination options header.
  NEXTHDR_SCTP      = 132; // SCTP message.
  NEXTHDR_MOBILITY  = 135; // Mobility header.

type
  THdrTCP = packed record     // TCP header (RFC 793) - Transport Layer  20 bytes plus options
    source : WORD;  // source port
    dest   : WORD;  // destination port
    seq    : LONGWORD; // sequence number
    ack_seq: LONGWORD; // next sequence number
    flags  : WORD;  // Combined field:
                    //   res1:4 - reserved, must be 0
                    //   doff:4 - TCP header length divided by 4
                    //   fin:1  - FIN
                    //   syn:1  - SYN
                    //   rst:1  - Reset
                    //   psh:1  - Push
                    //   ack:1  - ACK
                    //   urg:1  - Urgent
                    //   res2:2 - reserved, must be 0
    window : WORD;  // window size
    check  : WORD;  // checksum, computed later
    urg_ptr: WORD;  // used for async messaging?
   // variable options
  end;
  PHdrTCP = ^THdrTCP;
  (* Details of TCP header can be found in RFC 793

    -th.seq - the sequence number of the first data octet in this segment
     (except when SYN is present). If SYN is present the sequence number
     is the initial sequence number (ISN) and the first data octet is ISN+1.

    -th.doff - data offset - the number of 32 bit words in the TCP Header.
     This indicates where the data begins. The TCP header (even one
     including options) is an integral number of 32 bits long.

    -th.ack_seq is used when ACK flag is set. If ACK is set this field
     contains the value of the next sequence number the sender of the
     segment is expecting to receive. Once a connection is established
     this is always sent. This simply means that receiver got all the
     octets up to the specific sequence number.
     For example, sending a packet with an acknowledgement of 1500
     indicates that you have received all the data up to octet
     number 1500. If the sender doesn't get an acknowledgement
     within a reasonable amount of time, it sends the data again.

    -th.window is used to control how much data can be in transit
     at any one time. It is not practical to wait for each datagram
     to be acknowledged before sending the next one. That would slow
     things down too much. On the other hand, you can't just keep
     sending, or a fast computer might overrun the capacity of a slow
     one to absorb data. Thus each end indicates how much new data
     it is currently prepared to absorb by putting the number of
     octets in its "window" field. As the computer receives data,
     the amount of space left in its window decreases. When it goes
     to zero, the sender has to stop. As the receiver processes
     the data, it increases its window, indicating that it is ready
     to accept more data.
     [ See RFC813 for details and "silly-window-syndrome" ]
     Often the same datagram can be used to acknowledge receipt of
     a set of data and to give permission for additional new data
     (by an updated window).

    -th.urgent field allows one end to tell the other to skip ahead
     in its processing to a particular octet. This is often useful
     for handling asynchronous events, for example when you type
     a control character or other command that interrupts output.
   *)

  THdrUDP = packed record  // UDP header (RFC 768)    - Transport Layer  8 bytes
    src_port: WORD;        // source port
    dst_port: WORD;        // destination port
    length  : WORD;        // length, including this header
    checksum: WORD;        // UDP checksum
  end;
  PHdrUDP = ^THdrUDP;

type
  TTcpFlagType = (ftFIN, ftSYN, ftRST, ftPSH, ftACK, ftURG);

var
  PortNameArray: array of string ;   // dynamic array for TCP and UDP port names, indexed by number
  TotalPortNames: integer = -1 ;
  ProtoNameArray: array of string ;  // dynamic array for IP protocol names, indexed by number
  TotalProtoNames: integer = -1 ;
  PortListFileName: string = 'ports.txt' ;
  ProtocolListFileName: string = 'protocols.txt' ;
  MacPrefixFileName: string = 'nmap-mac-prefixes.txt';
  MacPrefixes: TStringList;

// get name given a number
function IcsEtherProtoName (protocol: word): string ;
function IcsIPProtoName(protocol: integer): string ;
function IcsServiceName(s_port, d_port: Integer): string;
function IcsServName (port: integer): string ;
function IcsServiceNameEx(s_port, d_port: Integer): string;
function IcsICMPType (x: word): string ;
function IcsTCPFlags(flags: word): string ;
procedure IcsLoadPortNameList ;
function IsKnownService (s_port: integer): Boolean ;   // Feb 2023

// these routines manipulate combined fields (set/get nibbles or bits)
procedure SetTHdoff(VAR th: THdrTCP; value: Byte);
function  GetTHdoff(th: THdrTCP): Integer;
procedure SetTHflag(VAR th: THdrTCP; flag: TTcpFlagType; on: Boolean);
function  GetTHflag(th: THdrTCP; flag: TTcpFlagType): Boolean;
procedure SetIHver(VAR ih: THdrIP; value: Byte);
function  GetIHver(ih: THdrIP): Byte;
procedure SetIHlen(VAR ih: THdrIP; value: Byte);
function  GetIHlen(ih: THdrIP): Integer;

function IcsMacToStr(MacAddr: TMacAddr): string;
function IcsMacIsRandom(MacAddr: TMacAddr): Boolean;   overload;
function IcsMacIsRandom(MacAddr: String): Boolean;   overload;
function IcsLoadMacPrefixes: Boolean;
function IcsGetMacVendor(const PartMacAddr: String): string; overload;
function IcsGetMacVendor(MacAddr: TMacAddr): string; overload;
function IcsIsLocalIPv4(IPv4: TIcsIPv4Address): Boolean;
function IcsIsLocalIPv6(IPv6: TIcsIPv6Address): Boolean;


implementation

type

  TEtherProto = record
    iType: integer ;
    iName: string ;
  end ;

  TIPProto = record
    iType: integer ;
    iName: string ;
  end ;

  TWellKnownSvc = record
    port: integer ;
    svc: string ;
  end ;

var
  // Ethernet Protocol types
  EtherProto: array[1..18] Of TEtherProto = (
    (iType: ETHERNET_PUP;      iName: 'PUP'),
    (iType: ETHERNET_XNS;      iName: 'XNS'),
    (iType: ETHERNET_IP;       iName: 'IP'),
    (iType: ETHERNET_ARP;      iName: 'ARP'),
    (iType: ETHERNET_REVARP;   iName: 'RARP'),
    (iType: ETHERNET_SCA;      iName: 'SCA'),
    (iType: ETHERNET_ATALK;    iName: 'ATLK'),
    (iType: ETHERNET_AARP;     iName: 'AARP'),
    (iType: ETHERNET_VLAN;     iName: 'VLAN'),
    (iType: ETHERNET_IPX;      iName: 'IPX'),
    (iType: ETHERNET_NOVELL;   iName: 'NOVL'),
    (iType: ETHERNET_SNMP;     iName: 'SNMP'),
    (iType: ETHERNET_IPV6;     iName: 'IPV6'),
    (iType: ETHERNET_PPP;      iName: 'PPP'),
    (iType: ETHERNET_PPPoEd;   iName: 'PPPoE Dis'),
    (iType: ETHERNET_PPPoEs;   iName: 'PPPoE Ses'),
    (iType: ETHERNET_XIMETA;   iName: 'XIMT'),
    (iType: ETHERNET_LOOP;     iName: 'LOOP')
  );

  // IP Protocol types
  IpProto: array[1..6] Of TIPProto = (
    (iType: IPPROTO_IP;   iName: 'IP'),   // dummy
    (iType: IPPROTO_ICMP; iName: 'ICMP'),
    (iType: IPPROTO_IGMP; iName: 'IGMP'),
    (iType: IPPROTO_TCP;  iName: 'TCP'),
    (iType: IPPROTO_UDP;  iName: 'UDP'),
    (iType: $80;          iName: 'ISO-IP')
  );

// Well known service ports
// full list at http://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml
  WellKnownSvcs: array[1..65] of TWellKnownSvc = (
//    ( port:   0; svc: 'LOOPBACK'),
    ( port:   1; svc: 'TCPMUX'),    { TCP Port Service Multiplexer  }
    ( port:   7; svc: 'ECHO' ),     { Echo                          }
    ( port:   9; svc: 'DISCARD' ),  { Discard                       }
    ( port:  13; svc: 'DAYTIME' ),  { DayTime                       }
    ( port:  17; svc: 'QOTD' ),     { Quote Of The Day              }
    ( port:  19; svc: 'CHARGEN' ),  { Character Generator           }
    ( port:  20; svc: 'FTP_DATA' ), { Ftp                           }
    ( port:  21; svc: 'FTP_CTL' ),  { File Transfer Control Protocol}
    ( port:  22; svc: 'SSH' ),      { SSH Remote Login Protocol     }
    ( port:  23; svc: 'TELNET' ),   { TelNet                        }
    ( port:  25; svc: 'SMTP' ),     { Simple Mail Transfer Protocol }
    ( port:  37; svc: 'TIME' ),
    ( port:  42; svc: 'NAME' ),     { Host Name Server              }
    ( port:  43; svc: 'WHOIS' ),    { WHO IS service                }
    ( port:  53; svc: 'DNS' ),      { Domain Name Service           }
    ( port:  66; svc: 'SQL*NET' ),  { Oracle SQL*NET                }
    ( port:  67; svc: 'BOOTPS' ),   { BOOTP Server                  }
    ( port:  68; svc: 'BOOTPC' ),   { BOOTP Client                  }
    ( port:  69; svc: 'TFTP' ),     { Trivial FTP                   }
    ( port:  70; svc: 'GOPHER' ),   { Gopher                        }
    ( port:  79; svc: 'FINGER' ),   { Finger                        }
    ( port:  80; svc: 'HTTP' ),     { HTTP                          }
    ( port:  88; svc: 'KERBEROS' ), { Kerberos                      }
    ( port:  92; svc: 'NPP' ),      { Network Printing Protocol     }
    ( port:  93; svc: 'DCP' ),      { Device Control Protocol       }
    ( port: 109; svc: 'POP2' ),     { Post Office Protocol Version 2}
    ( port: 110; svc: 'POP3' ),     { Post Office Protocol Version 3}
    ( port: 111; svc: 'SUNRPC' ),   { SUN Remote Procedure Call     }
    ( port: 115; svc: 'SFTP' ),     { Simple FTP                    }
    ( port: 119; svc: 'NNTP' ),     { Network News Transfer Protocol}
    ( port: 123; svc: 'NTP' ),      { Network Time protocol         }
    ( port: 135; svc: 'LOCSVC' ),   { Location Service              }
    ( port: 137; svc: 'BIOS-NAME' ),  { NETBIOS Name service          }
    ( port: 138; svc: 'BIOS-DATA' ),  { NETBIOS Datagram Service      }
    ( port: 139; svc: 'BIOS-SESS' ),  { NETBIOS Session Service       }
    ( port: 143; svc: 'IMAP3' ),    { Interactive Mail Access Protocol v3 }
    ( port: 161; svc: 'SNMP' ),     { Simple Netw. Mgmt Protocol    }
    ( port: 162; svc: 'SNMPTRAP' ), { SNMP TRAP                     }
    ( port: 220; svc: 'IMAP3' ),    { Interactive Mail Access Protocol v3 }
    ( port: 389; svc: 'LDAP' ),     { LDAP                          }
    ( port: 443; svc: 'HTTPS' ),    { HTTPS                         }
    ( port: 445; svc: 'MS-DS-SMB'), { Microsoft Directory Services - SAMBA }
    ( port: 465; svc: 'SMTP' ),     { Simple Mail Transfer Protocol }
    ( port: 514; svc: 'SYSLOG' ),   { UDP Syslog                    }
    ( port: 520; svc: 'ROUTER' ),   { UDP Router                    }
    ( port: 563; svc: 'NNTP' ),     { Network News Transfer Protocol}
    ( port: 587; svc: 'SMTP' ),     { Simple Mail Transfer Protocol }
    ( port: 992; svc: 'TELNET' ),   { TelNet                        }
    ( port: 993; svc: 'IMAP3' ),    { Interactive Mail Access Protocol v3 }
    ( port: 995; svc: 'POP3' ),     { Post Office Protocol Version 3}
    ( port:1433; svc: 'MSSQLSRV' ), { MS SQL Server                 }
    ( port:1434; svc: 'MSSQLMON' ), { MS SQL Monitor                }
    ( port:1833; svc: 'MQTT' ),     { MQ Telemetry Transport        }
    ( port:1900; svc: 'UPnP' ),     { SSDP-uPnP                     }
    ( port:3306; svc: 'MYSQL' ),    { MySQL                         }
    ( port:3910; svc: 'PRINTREQ' ), { HP JetAdmin printer           }
    ( port:5352; svc: 'DNSLLQ'),    { DNS Long Lived Queries        }
    ( port:5353; svc: 'MDNS' ),     { Multicast Domain Name Resolution }
    ( port:5354; svc: 'MDNSR' ),    { Multicast Domain Name Resolution Reponser }
    ( port:5355; svc: 'LLMNR' ),    { Link Local Multicast Name Resolution }
    ( port:5900; svc: 'VNC' ),      { VNC - similar to PC Anywhere  }
    ( port:6667; svc: 'IRC' ),      { Internet Relay Chat           }
    ( port:6668; svc: 'IRC' ),      { Internet Relay Chat           }
    ( port:6669; svc: 'IRC' ),      { Internet Relay Chat           }
    ( port:8833; svc: 'MQTT' )      { MQ Telemetry Transport        }
  );

function IcsEtherProtoName (protocol: word): string ;
var
    I: integer;
begin
    result := 'x' + IntToHex (protocol, 4) ;    // Feb 2023
    for I := 1 To SizeOf (EtherProto) div SizeOf (TEtherProto) do
    begin
        if protocol = EtherProto [I].itype then result := EtherProto [I].iName ;
    end ;
end;

function IcsIPProtoName (protocol: integer): string ;
var
    I: integer;
begin
    result := IntToStr (protocol) ;
    if protocol = 0 then Exit;
    for I := 1 To SizeOf (IPPROTO) div SizeOf (TIPProto) do
    begin
        if protocol = IPPROTO [I].itype then
            result := IPPROTO [I].iName ;
    end ;
end;

function IcsServiceName (s_port, d_port: integer): string ;
var
    I: integer;
begin
    result := '';
    for I := 1 to SizeOf (WellKnownSvcs) div SizeOf (TWellKnownSvc) do
    begin
        if (s_port = WellKnownSvcs [I].port) OR (d_port = WellKnownSvcs [I].port) then
        begin
            result := WellKnownSvcs[I].svc;
            exit ;
        end;
    end ;
    if (result = '') and (s_port < 1024) then
        result := '<' + IntToStr (s_port) + '>' ;
    if (result = '') and (d_port < 1024) then
        result := '<' + IntToStr (d_port) + '>' ;
end ;

function IsKnownService (s_port: integer): Boolean ;   // Feb 2023
var
    I: integer;
begin
    result := False;
    for I := 1 to SizeOf (WellKnownSvcs) div SizeOf (TWellKnownSvc) do
    begin
        if (s_port = WellKnownSvcs [I].port) then
        begin
            result := True;
            exit ;
        end;
    end ;
end;


function  IcsICMPType(x: word): string ;
begin
    result := 'UNKNOWN';
    case x of
     0: Result := 'ECHO_REPLY'; // Echo Reply
     3: Result := 'DEST-UNREA'; // Destination Unreachable
     4: Result := 'SRC_Q';  // Source Quench
     5: Result := 'REDIR';  // Redirect
     8: Result := 'ECHO';   // Echo
    11: Result := 'TTLX';   // Time Exceeded
    12: Result := 'BADPAR'; // Parameter Problem
    13: Result := 'TIME';   // Timestamp
    14: Result := 'TIME_REPLY'; // Timestamp Reply
    15: Result := 'INFO';   // Information Request
    16: Result := 'INFO_REPLY'; // Information Reply
   end ;
end ;

// load well know port list from file ports.txt, which is copied from RFC 1700 with
// superflous lines removed or prefixed with #
// Note: currently using UDP port where TCP is different, should really have two arrays

procedure IcsLoadPortNameList ;
var
  PortInfo: TStringList ;
  line, port: string ;
  I, J, K, L, M: integer ;
begin
    TotalPortNames := 0 ;
    if FileExists (PortListFileName) then
    begin
        TotalPortNames := 10000 ;
        SetLength (PortNameArray, TotalPortNames) ;
        PortInfo := TStringList.Create ;
        try
            try
                PortInfo.LoadFromFile (PortListFileName) ;
                I := PortInfo.Count ;
            except
                I := 0 ;
            end ;
            if I <> 0 then
            begin
                for J := 0 to Pred (I)  do
                begin
                // sample line - ignore / onwards
                // echo              7/tcp    Echo
                    line := PortInfo [J] ;
                    if Length (line) < 5 then continue ;
                    if line [1] = '#' then continue ;
                    K := Pos (' ', line) ;
                    M := Pos ('/', line) ;
                    if (K < 2) or (M < K) then continue ;
                    port := Copy (line, K, M - K) ;
                    L := atoi (Trim (port)) ;
                    if (L = 0) then continue ;
                    if L >= TotalPortNames then continue ;  // ignore high ports
                  //if PortNameArray [L] = '' then
                    PortNameArray [L] := Copy (line, 1, Pred (K)) ;
                end ;
            end
            else
                TotalPortNames := 0 ;
        finally
              PortInfo.Destroy ;
        end ;
    end ;
end ;

function IcsServName (port: integer): string ;
var
    I: integer;
begin
    result := '' ;
    if TotalPortNames < 0 then
        IcsLoadPortNameList ;  // try and load list
    if (port > 0) and (port < TotalPortNames) then
        result := PortNameArray [port] ;
    if result = '' then  // nothing in list, try hard coded ports
    begin
        for I := 1 to SizeOf (WellKnownSvcs) div SizeOf (TWellKnownSvc) do
        begin
            if (port = WellKnownSvcs [I].port) then
            begin
                result := WellKnownSvcs[I].svc;
                exit ;
            end;
        end ;
    end ;
    if (result = '') then
        result := '<' + IntToStr (port) + '>' ;
end ;

function IcsServiceNameEx (s_port, d_port: integer): string ;
var
    I: integer;
    s_name, d_name: string ;
begin
    result := '';
    s_name := '' ;
    d_name := '';
    if TotalPortNames < 0 then
        IcsLoadPortNameList ;  // try and load list
    if (s_port > 0) and (s_port < TotalPortNames) then
        s_name := PortNameArray [s_port] ;
    if (d_port > 0) and (d_port < TotalPortNames) then
        d_name := PortNameArray [d_port] ;
    if d_name <> '' then
        result := d_name
    else
        result := s_name ;
    if result = '' then  // nothing in list, try hard coded ports
    begin
        for I := 1 to SizeOf (WellKnownSvcs) div SizeOf (TWellKnownSvc) do
        begin
            if (s_port = WellKnownSvcs [I].port) OR (d_port = WellKnownSvcs [I].port) then
            begin
                result := WellKnownSvcs[I].svc;
                exit ;
            end;
        end ;
    end ;
    if (result = '') and (s_port < 1024) then
        result := '<' + IntToStr (s_port) + '>' ;
    if (result = '') then
        result := '<' + IntToStr (d_port) + '>' ;
end ;

(* IP header record contains "ihl_ver" which is used
   to store two parameters: IP header length and IP version.
   IP version is stored in the high nibble of "ihl_ver"
   (it occupies 4 bits). IP header length is stored in the
   low nibble of "ihl_ver" (also uses 4 bits).
   IP header length is expressed in 32 bit words
   (4 8-bit bytes), therefore we divide or multiply
   the low nibble by 4 depending on the function.
*)

function GetIHlen(ih: THdrIP): Integer;  // IP header length
begin
  // multiply the low nibble by 4
  // and return the length in bytes
  Result := (ih.ihl_ver AND $0F) SHL 2
end;

procedure SetIHlen(VAR ih: THdrIP; value: Byte);
begin
  // divide the value by 4 and store it in low nibble
  value := value SHR 2;
  ih.ihl_ver := value OR (ih.ihl_ver AND $F0)
end;

function GetIHver(ih: THdrIP): Byte;  // IP version
begin
  // get the high nibble
  Result := ih.ihl_ver SHR 4
end;

procedure SetIHver(VAR ih: THdrIP; value: Byte);
begin
  // set the high nibble
  ih.ihl_ver := (value SHL 4) OR (ih.ihl_ver AND $0F)
end;

(* TCP header record contains "flags" which is used
   to store several parameters:
     Least Significant Bit
       res1:4 - reserved, must be 0
       doff:4 - TCP header length divided by 4
       fin:1  - FIN
       syn:1  - SYN
       rst:1  - Reset
       psh:1  - Push
       ack:1  - ACK
       urg:1  - Urgent
       res2:2 - reserved, must be 0
     MSB
*)

CONST
    flagMask: Array[ftFIN..ftURG] of Integer =
        ($100, $200, $400, $800, $1000, $2000);

function GetTHflag(th: THdrTCP; flag: TTcpFlagType): Boolean;
begin
  Result := Boolean(th.flags AND flagMask[flag])
end;


procedure SetTHflag(VAR th: THdrTCP; flag: TTcpFlagType; on: Boolean);
begin
  if on then
    th.flags := th.flags OR flagMask[flag]
  else
    th.flags := th.flags AND NOT flagMask[flag]
end;


function GetTHdoff(th: THdrTCP): Integer;
begin
  // doff (data offset) stored in 32 bit words,
  // multiply the value by 4 to get byte offset
  Result := (($00F0 AND th.flags) SHR 4) SHL 2;
end;


procedure SetTHdoff(VAR th: THdrTCP; value: Byte);
VAR x: Integer;
begin
  x := value SHR 2; // divide the value by 4
  th.flags := (x SHL 4) OR (th.flags AND $FF0F)
end;


function IcsTCPFlags(flags: word): string ;
begin
    result := '' ;
    if (flags AND TCP_FLAG_FIN) = TCP_FLAG_FIN then result := result + 'FIN ' ;
    if (flags AND TCP_FLAG_SYN) = TCP_FLAG_SYN then result := result + 'SYN ' ;
    if (flags AND TCP_FLAG_RST) = TCP_FLAG_RST then result := result + 'RST ' ;
    if (flags AND TCP_FLAG_PSH) = TCP_FLAG_PSH then result := result + 'PSH ' ;
    if (flags AND TCP_FLAG_ACK) = TCP_FLAG_ACK then result := result + 'ACK ' ;
    if (flags AND TCP_FLAG_URG) = TCP_FLAG_URG then result := result + 'URG ' ;
    if (flags AND TCP_FLAG_ECH) = TCP_FLAG_ECH then result := result + 'ECH ' ;
    if (flags AND TCP_FLAG_CWR) = TCP_FLAG_CWR then result := result + 'CWR ' ;
    result := trim (result) ;
end ;


function IcsMacToStr(MacAddr: TMacAddr): string ;
begin
    result := Format ('%.2x-%.2x-%.2x-%.2x-%.2x-%.2x',
                   [MacAddr [0], MacAddr [1], MacAddr [2],
                    MacAddr [3], MacAddr [4], MacAddr [5]]) ;
end ;


function IcsMacIsRandom(MacAddr: TMacAddr): Boolean ;
begin
//    Result := False;
//    if (MacAddr [0] = $FFFF) then  // broadcast
//        Exit;

 // bit 7 means MAC is locally generated and not unique
    Result := ((MacAddr [0] AND $02) = $02);
end;


function IcsMacIsRandom(MacAddr: String): Boolean ;
var
    MyMac: TMacAddr;
begin
    Result := False;
    if Length(MacAddr) < 6 then
        Exit;
    MyMac[0] := StrToIntDef('$' + Copy(MacAddr, 1, 2), 0);
    Result := IcsMacIsRandom (MyMac);
end;


// load Organization Unique Identifier (OUI) MAC list in smaller nmap-mac-prefixes.txt version
// https://linuxnet.ca/ieee/oui/nmap-mac-prefixes
function IcsLoadMacPrefixes: Boolean;
begin
    Result := False;
    try
        if NOT Assigned(MacPrefixes) then
           MacPrefixes := TStringList.Create;
        if FileExists(MacPrefixFileName) then begin
            MacPrefixes.LoadFromFile(MacPrefixFileName);
            if MacPrefixes.Count > 0 then begin
                MacPrefixes.Sort; // should be sorted already...
                Result := True;
            end;
        end;
    except
        // ignore errors
    end;
end;


// find vendor for MAC address from nmap-mac-prefixes.txt
// 90F052(tab)MEIZU
// 90F157(tab)Garmin
// 90F1AA(tab)Samsung
// only needs six hex characters, but strips off separators if provided
function IcsGetMacVendor(const PartMacAddr: String): string;
var
    J: Integer;
    SearchMac, Line: String;
begin
    Result := '';
    if NOT Assigned(MacPrefixes) then begin
        if NOT IcsLoadMacPrefixes then
            Exit;
    end;
    if MacPrefixes.Count = 0 then
        Exit;
    SearchMac := PartMacAddr;
    J := Length(SearchMac);
    if J <> 6 then begin
        if J < 6 then
            Exit;
        SearchMac := StringReplace(SearchMac, '-', '', [rfReplaceAll]);
        SearchMac := StringReplace(SearchMac, ':', '', [rfReplaceAll]);
    end;
    SetLength(SearchMac, 6);   // max to search
    if SearchMac = 'FFFFFF' then begin
        Result := '(Broadcast)';
        Exit;
    end;

    if IcsMacIsRandom(PartMacAddr) then begin
        Result := '(Locally Generated)';
        Exit;
    end;
    J := -1;
    MacPrefixes.Find(SearchMac, J);  // partial search so never succeeds, need to compare
    if (J < 0) or (J >= MacPrefixes.Count) then
        Exit;
    Line := MacPrefixes[J];
    if Copy(Line, 1, 6) <> SearchMac then  // ensure correct prefix found
        Exit;
    J := Pos(IcsTAB, Line);
    if J < 6 then
        Exit;
    Result := Copy(Line, J+1, 99);
end;


function IcsGetMacVendor(MacAddr: TMacAddr): string;
begin
    Result := IcsGetMacVendor(IcsMacToStr(MacAddr));
end;


function IcsIsLocalIPv4(IPv4: TIcsIPv4Address): Boolean;
var
    AddrLong: LongWord;
begin
    Result := False;
    AddrLong := LongWord(WSocket_ntohl(IPv4)); // must swap bytes to compare range
    if ((AddrLong >= IP10_0_0_0) and (AddrLong <= IP10_255_255_255)) then
        Result := True;
    if ((AddrLong >= IP100_64_0_0) and (AddrLong <= IP100_127_255_255)) then
        Result := True;
    if ((AddrLong >= IP127_0_0_0) and (AddrLong <= IP127_255_255_255)) then
        Result := True;
    if ((AddrLong >= IP169_254_1_0) and (AddrLong <= IP169_254_254_255)) then
        Result := True;
    if ((AddrLong >= IP192_168_0_0) and (AddrLong <= IP192_168_255_255)) then
        Result := True;
// look for broadcast and multicast addresses
    if ((AddrLong >= IP224_0_0_0) and (AddrLong <= IP239_255_255_255)) or (AddrLong = $FFFFFFFF) then
       Result := True;
end;

function IcsIsLocalIPv6(IPv6: TIcsIPv6Address): Boolean;
begin
    Result := False;
    if (IPv6.Words[0] = IP6_FE80) then    // link local
       Result := True;
    if (Swap(IPv6.Words[0]) >= IP6_FF00) then    // multicast
       Result := True;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
finalization
    if Assigned(MacPrefixes) then
        MacPrefixes.Free;
end.
