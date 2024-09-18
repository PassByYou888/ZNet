{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Component to query DNS records.
              Implement most of RFC 1035 (A, NS, AAAA, PTR, MX, etc).
Creation:     January 29, 1999
Version:      V9.0
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1999-2023 by François PIETTE
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

History:
Feb 14, 1999 V0.02 Indirectly call winsock functions using wsocket because
             wsocket provide runtime dynamic link instead of loadtime link.
             This allows a program to use DnsQuery if it discover that winsock
             is installed and still run if winsock is not installed.
Feb 24, 1999 V1.00 Added code for reverse lookup (PTR record).
Mar 07, 1999 V1.01 Adapted for Delphi 1
Aug 20, 1999 V1.02 Revise compile time option. Adapted for BCB4
Jul 27, 2001 V1.03 Holger Lembke <holger@hlembke.de> implemented a few new
                   queries or propreties (QueryAny, LongLatToDMS, Loc2Geo, Loc)
                   and related data types.
Sep 04, 2003 V1.04 Replaced all htons by WSocket_htons
May 31, 2004 V1.05 Used ICSDEFS.INC
Nov 19, 2004 V1.06 Added Multithreaded property
Mar 06, 2005 V1.07 DecodeAnswer has been fixed to avoid winsock ntohs and
                   ntohl function which have range check errors because Borland
                   defined the function as returning LongInt instead of Cardinal
May 29, 2005 V1.08 Jack <jlist9@gmail.com> added TCP support
Mar 26, 2006 V6.00 New version 6 started
Jun 05, 2008 A. Garrels made some changes to prepare code for Unicode
Aug 11, 2008 V6.02 A. Garrels - Type AnsiString rolled back to String.
Oct 09, 2009 V6.03 Yaroslav Chernykh fixed a bug in WSocketSessionConnected()
                   when using UDP.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Apr 22 2019 V8.61  Angus added more DnsQuery literals and removed obsolete ones.
                   DecodeWireResp split from WSocketDataAvailable so can be
                     used for DNS over HTTPS.
                   Queries now started with QueryAny.
                   Added QueryAll to return results from seven queries.
                   No longer ignores NS and Alternate results that sometimes
                     return extra useful records (like A for NS and CNAME).
                   New result array AnswerRecord with AnswerTotal records which
                     is a TRRRecord response record with all result information
                     so other arrays can be ignored, based on unpublished work
                     by Holger Lembke but implemented with backward compatibility.
                  Added DnsReqTable of common queries with descriptive literals.
                  Added DnsPublicServerTable and DnsPublicHttpsTabl of public
                    DNS server addresses.
                  Supporting all queries and responses including AAAA, NS and TXT,
                    common ones decoded in TRRRecord, rest returned as text or hex.
                  PTR query now supports IPv6 addresses as well as IPV4.
                  Call RequestDone if connection fails.
 May 04, 2020 V8.64 Make sure data not processed beyond end of response buffer.
                  Added support for International Domain Names for Applications (IDNA),
                  All methods and arrays use String instead of AnsiString to support
                    Unicode domain names.  This may need application changes.
                  All Unicode queries are converted to Punycode ASCII, and responses
                    with ACE zn-- prefix are converted back to Unicode.
                    TRRRecord has AnswerName and HostName for unicode responses.
May 25, 2020 V8.65 Handle NS and CNAME responses as host name with compressed
                     data correctly.
Jul 27, 2023 V8.71 Fixed a bug where a second query after calling AbortQuery might
                    have made the wrong query.
                  Changed default protocol from TCP to UDP which is easier. Beware
                    TCP is less friendly for rapid multiple connections due to long
                    connection timeouts, really need multiple wsockets for TCP.
                  Added AAAALookup method for IPv6, similar to ALookup, sets array
                    of IPv6 addresses in Address6[] property, thanks to djhfwk.
                    Note that sometimes ALookup will also returns AAAA records.
                  Added QueryBothA method that does both A and AAAA requests to
                    return all IP addresses for a host.
                  Added several synchronous methods, MXLookupSync, ALookupSync,
                    AAAALookupSync, PTRLookupSync, QueryAllSync, QueryAnySync,
                    QueryBothASync,  default sync Timeout property is 5 seconds,
                    thanks to djhfwk. Note sync methods return true for success,
                    unlike async methods which return a sequential ID to check
                    against the response.
                  Allow lookup using multiple DNS servers if one or more fail,
                     ServerStrat is SrvStratOne uses single host passed in method,
                     SrvStratList uses DNS servers from the ServerList property,
                     SrvStratPublic uses the internal DnsPublicServerTable list.
                     ServerMax property is how many servers to try, default 4.
                     Once the component finds a working DNS server, it will be
                     used for subsequent requests.  Only works with sync methods.
                     ServerCur may be set to start with a specific server in a list
                     if not the first, and show which is being used, as does ServerAddr.
                     The IpHlpGetDnsServers function in the OverbyteIcsIpHlpApi unit
                      may be used to set ServerList to the PC's defined DNS servers.
                  Added CAA request to AllRequests list.
                  When reading MX records, added new MXSortedExch[] property
                    that returns hosts in preference order, lowest first.
                  When reading TXT records, added new TXTRecord[] property.
                  Made MXRecordCount, ARecordCount, AAAARecordCount, TXTRecordCount
                    and PRTRecordCount public to ease reading result arrays.
                  LastError after request not zero, 999=timeout or abort, otherwise
                     winsock error.
                  Added OnLogEvent primarily for debugging multiple requests and
                    servers, may mostly be ignored.
                  Updated public DNS over Https table, Google URL changed.
                  Moved ReverseIP and ReverseIP6 to wsocket.
                  Re-arranged DnsReqTable so most common requests are near the top,
                    in particular AAAA follows A.
                  Added new TIcsDomainNameCache component to cache forward and reverse
                    DNS lookup requests, mainly for diagnostic components but also for
                    servers logging remote access.  Request are looked up asynchronously
                    or synchronously if not cached, and an event called when complete,
                    different events are supported for different requests to multiple
                    components can share the same cache.  Supports multiple IP
                    addresses for each lookup.  May be configured to use winsock
                    lookup, UDP/TCP using TDnsQuery or HTTPS (TIcsDomainNameCacheHttps
                    derived component). Defaults to 5 parallel lookups, but up to 100.
                  Added GetNetBiosHostByAddr for Windows only to lookup NetBios host
                    name on LAN by IPv4 address.
Aug 08, 2023 V9.0  Updated version to major release 9.



Note - OverbyteIcsHttpRest contains a derived component TDnsQueryHttps which makes
DNS over HTTPS requests per RFC8484, illustrated in the OverbyteIcsHttpRest sample
which displays results in a more friendly grid.

Pending - TIcsDomainNameCache support for maintenance deleting old record, save and
read from file.


TIcsDomainNameCache
-------------------

TIcsDomainNameCache is designed to simplify forward domain name and reverse IP address
lookup in applications, to avoid needing use of TWSocket or TDnsQuery components often
several to support parallel lookups.   There are  synchronous methods that wait
until a response is received, and asynchronous methods that return immediately with
an event called when the response is available.  If a name response is cached, the event
will be called before the method returns.  To allow ease of integration with applications,
all methods pass a Tag to identify where the response should go, such as a row in an array,
and optionally a pointer to a specific UpdateEvent, so that several different components in
an application can receive responses to different events, instead of checking a common
UpdateEvent.

There are several methods for forward and reverse lookups, depending on whether to wait for
a result from an asynchronously event in acceptable, wait synchronously for a result, or
return an immediate result from the cache but look-up asynchronously anyway if not cached
ready for next time, which is often acceptable for web logs. Each method has Tag property
that is returned later to help identify the source of the lookup, perhaps a row in a list.
Each method also has DNFamily which may be sfIPv4, sfIPv6 or sfAny depending on the IP
families that should be looked-up.

Methods LookupHostSync, LookupHostAsync, LookupIPSync and LookupIPAsync, each return an
ItemNr for a DNItem record that may already contain the results if cached, or which will be
updated once the optional UpdateEvent is called upon completion.  The DNItem record may
return several IPv4 and/or IPv6 addresses or host names, the BuildRespList method returns
a simple string of all results.

Methods GetHost and GetIP returns a single IP or Host from the cache or blank if not
cached, no new look-ups are started.

Methods LookupHostOne and LookupIPOne returns a single IP or Host from the cache or blank if not
cached, but start a new look-ups if not cached.

The GetDNItem method returns a TDNItem record for an ItemNr, as follows:

  TDNItem = record
    Request: String;              // host name or IP address
    ReqTag: Integer;              // for application use
    ReqUpdEvent: TDNUpdateEvent;  // which event to call
    ReqFamily: TSocketFamily;     // do we want one or other or both
    Responses4: array of String;  // often IPv4 multiple responses
    Responses6: array of String;  // often IPv6 multiple responses
    TotResp4: Integer;
    TotResp6: Integer;
    TimeStamp: TDateTime;        // if non-zero, last lookup attempt
    TTL: Integer;                // seconds time to live before expiry
    DNReqType: TDNReqType;       // forward or reverse
    DNState: TDNState;           // lookup progress
    Index: Integer;              // index into array, may change if compressed
  end;

DNState is typed as (StateNone, StateWaiting, StateOK, StateFailed), so should always be
checked before the record is used, Waiting means no response yet, OK or Failed are whether
the response was successful.  Responses4 and Responses6 are dynamic arrays potentially
with several IPv4 and IPv6 addresses, with the totals in TotResp4 and TotResp6.  There
is a method BuildRespList that returns a string with all the IP addresses for an ItemNr.
Note that reverse look-up IP addresses are in Responses6, DNReqType is (ReqTypeDnsForw,
ReqTypeDnsBack).  Index is ItemNr, but may change if when cache maintenance removes out
of date items.  ReqTag and ReqUpdEvent are those from the last lookup request, so will
change if the same name is looked up by different components.  TimeStamp and TTL are
used to refresh the record if it's been cached for more than TTL in seconds (generally
one day).

Note this component must run in the main application thread and methods only called from
that thread, otherwise new lookups will fail.

The DNMethod defines how look-ups are performed: MethodWinsock uses winsock so results
come from the operating system cache using the TWSocket component, TTL is assumed to be
one day; MethodUdp and MethodTcp use the TDnsQuery component to make requests to specific
domain name servers in DnsServerList which if left empty will use a list of public DNS
servers (Google, Cloudfare, etc, see DnsPublicServerTable) but may be set using the
IpHlpGetDnsServers function in the application to use the DNS servers defined for the
PC, TTL is set accurately; MethodHttps is available for the derived TIcsDomNameCacheHttps
component to use DNS over HTTPs for secure lookups using DnsSrvUrlList, defaulting to
public DoH servers.

The MaxLookups property defines how many simultaneous lookups may be performed, defaulting
to five but up to 100, lookups are done in the sequential order they are made using a
FIFO queue.  There is an DNLogEvent that shows internal activity in the cache component,
but may show down any lookups. The QTimeout property defines how long to wait for a
lookup, defaulting to five seconds. DefTTL sets the time to live for Winsock lookups,
defaulting to one day.  DefHostList contains localhost IPs which are often used by
applications and is added automatically on start-up, but may be cleared if not needed.
the DBLANlookup property defines how LAN IPv4 addresses are looked up: LanLookDef
does nothing so using public servers won't give a response; LanLookWSock uses Winsock
irrespective of DNMethod so usually works; LanLookNetBios sends a NetBIOS name request
to the local PC, which may respond, although NetBIOS is often blocked for security
reasons.

The MaintClearAll method clears the cache, ListCache returns a string containing the
cache contents.

A future version will add support for maintenance deleting old records, and saving and
reading the cache to and from a file.

TIcsDomainNameCache is illustrated in the OverbyteIcsNetTools, OverbyteIcsNetMon,
OverbyteIcsBatchDnsLookup, OverbyteIcsDDWebService and OverbyteIcsSslMultiWebServ
sample applications.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsDnsQuery;
{$ENDIF}

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I Include\Z.ICS9.OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    Z.ICS9.OverbyteIcsWinsock,
{$ENDIF}
{$IFDEF POSIX}
 //   System.IOUtils,
//    Posix.Unistd,
    Posix.SysSocket,
    Z.ICS9.Ics.Posix.WinTypes,
    Z.ICS9.Ics.Posix.PXMessages,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.TypInfo{$ELSE}TypInfo{$ENDIF},  { V8.71 }
{$IFDEF FMX}
    Z.ICS9.Ics.Fmx.OverbyteIcsWndControl, { V8.71 }
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocket,
    Z.ICS9.Ics.Fmx.OverbyteIcsIpUtils,    { V8.71 }
{$ELSE}
    Z.ICS9.OverbyteIcsWndControl,    { V8.71 }
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsIpUtils,       { V8.71 }
{$ENDIF}
    Z.ICS9.OverbyteIcsUtils,
    Z.ICS9.OverbyteIcsTypes,         { V8.71 }
    Z.ICS9.OverbyteIcsTicks64;       { V8.71 }

const
  DnsQueryVersion    = 900;
  CopyRight : String = ' TDnsQuery  (c) 1999-2023 F. Piette V9.0 ';

  { Maximum answers (responses) count }
  MAX_ANCOUNT     = 50;
  { Maximum number of MX records taken into account in responses }
  MAX_MX_RECORDS  = 20;
  MAX_A_RECORDS   = 30;
  MAX_PTR_RECORDS = 10;

// https://www.iana.org/assignments/dns-parameters/dns-parameters.txt

  { DNS Classes }
  DnsClassIN      = 1;   { The internet                                      }
  DnsClassCS      = 2;   { obsolete }
  DnsClassCH      = 3;   { The CHAOS class                                   }
  DnsClassHS      = 4;   { Hesiod name service                               }
  DnsClassALL     = 255; { Any class                                         }

  { Type of query/response a DNS can handle }
  { https://en.wikipedia.org/wiki/List_of_DNS_record_types }
  { V8.61 suppress obsolete types so we don't waste time using them }
  DnsQueryA       = 1;  { A     HostAddress                                  }
  DnsQueryNS      = 2;  { NS    Authoritative name server                    }
//DnsQueryMD      = 3;  { MD    MailDestination, obsolete, use Mail Exchange }
//DnsQueryMF      = 4;  { MF    MailForwarder, obsolete, use Mail Exchange   }
  DnsQueryCNAME   = 5;  { CNAME CanonicalName                                }
  DnsQuerySOA     = 6;  { SOA   Start of a Zone of Authority                 }
//DnsQueryMB      = 7;  { MB    MailBox, experimental                        }
//DnsQueryMG      = 8;  { MG    MailGroup, experimental                      }
//DnsQueryMR      = 9;  { MR    MailRename, experimental                     }
//DnsQueryNULL    = 10; { NULL  Experimental                                 }
//DnsQueryWKS     = 11; { WKS   Well Known Service Description               }
  DnsQueryPTR     = 12; { PTR   Domain Name Pointer                          }
//DnsQueryHINFO   = 13; { HINFO Host Information                             }
//DnsQueryMINFO   = 14; { MINFO Mailbox information                          }
  DnsQueryMX      = 15; { MX    Mail Exchange                                }
  DnsQueryTXT     = 16; { TXT   Text Strings                                 }
  { !!KAP!! }
//DnsQueryRP      = 17;
  DnsQueryAFSDB   = 18;
//DnsQueryX25     = 19;
//DnsQueryISDN    = 20;
//DnsQueryRT      = 21;
//DnsQueryNSAP    = 22;
//DnsQueryNSAPPTR = 23;
  DnsQuerySIG     = 24; { see RFC-2065                                       }
  DnsQueryKEY     = 25; { see RFC-2065                                       }
//DnsQueryPX      = 26;
//DnsQueryGPOS    = 27;
  DnsQueryAAAA    = 28; { see IP6 Address                                    }
  DnsQueryLOC     = 29; { see RFC-1876  http://rfc.net/rfc1876.html  }
//DnsQueryNXT     = 30; { see RFC-2065                                     }
//DnsQueryEID     = 31;                                    }
//DnsQueryNB      = 32;                                    }
//DnsQueryNBSTAT  = 33;                                    }
  DnsQuerySRV     = 33; { see RFC-2052                                       }
//DnsQueryATMA    = 34;                                    }
  DnsQueryNAPTR   = 35; { see RFC-2168                                       }
// following V8.61
  DnsQueryKX           = 36;  // Key Exchanger   [RFC2230]
  DnsQueryCERT         = 37;  // CERT            [RFC4398]
//DnsQueryA6           = 38;  // A6 (OBSOLETE -  use AAAA)
  DnsQueryDNAME        = 39;  // DNAME           [RFC6672]
//DnsQuerySINK         = 40;  // SINK            [Donald_E_Eastlake]
  DnsQueryOPT          = 41;  // OPT             [RFC6891][RFC3225]
//DnsQueryAPL          = 42;  // APL             [RFC3123]
  DnsQueryDS           = 43;  // Delegation Signer     [RFC4034][RFC3658]
  DnsQuerySSHFP        = 44;  // SSH Key Fingerprint        [RFC4255]
  DnsQueryIPSECKEY     = 45;  // IPSECKEY        [RFC4025]
  DnsQueryRRSIG        = 46;  // RRSIG           [RFC4034][RFC3755]
  DnsQueryNSEC         = 47;  // NSEC            [RFC4034][RFC3755]
  DnsQueryDNSKEY       = 48;  // DNSKEY          [RFC4034][RFC3755]
  DnsQueryDHCID        = 49;  // DHCID           [RFC4701]
  DnsQueryNSEC3        = 50;  // NSEC3           [RFC5155]
  DnsQueryNSEC3PARAM   = 51;  // NSEC3PARAM      [RFC5155]
  DnsQueryTLSA         = 52;  // TLSA            [RFC6698]
  DnsQuerySMIMEA       = 53;  // S/MIME cert association    [RFC8162]
  DnsQueryHIP          = 55;  // Host Identity Protocol   [RFC8005]
//DnsQueryNINFO        = 56;  // NINFO           [Jim_Reid]
//DnsQueryRKEY         = 57;  // RKEY            [Jim_Reid]
  DnsQueryTALINK       = 58;  // Trust Anchor LINK  [Wouter_Wijngaards]
  DnsQueryCDS          = 59;  // Child DS DNSKEY(s) the        [RFC7344]
  DnsQueryCDNSKEY      = 60;  // Child wants reflected in DS    [RFC7344]
  DnsQueryOPENPGPKEY   = 61;  // OpenPGP Key     [RFC7929]
  DnsQueryCSYNC        = 62;  // Child-To-Parent Synchronization [RFC7477]
  DnsQueryZONEMD       = 63;  // message digest for DNS zone  [draft-wessels-dns-zone-digest]
//DnsQuerySPF          = 99;  // [RFC7208]
//DnsQueryUINFO        = 100; // [IANA-Reserved]
//DnsQueryUID          = 101; // [IANA-Reserved]
//DnsQueryGID          = 102; // [IANA-Reserved]
//DnsQueryUNSPEC       = 103; // [IANA-Reserved]
  DnsQueryNID          = 104; // [Identifier-Locator Network Protocol (ILNP) RFC6742]
  DnsQueryL32          = 105; // [Identifier-Locator Network Protocol (ILNP) RFC6742]
  DnsQueryL64          = 106; // [Identifier-Locator Network Protocol (ILNP) RFC6742]
  DnsQueryLP           = 107; // [Identifier-Locator Network Protocol (ILNP) RFC6742]
  DnsQueryEUI48        = 108; // an EUI-48 address      [RFC7043]
  DnsQueryEUI64        = 109; // an EUI-64 address      [RFC7043]
  DnsQueryTKEY         = 249; // Transaction Key [RFC2930]
  DnsQueryTSIG         = 250; // Transaction Signature    [RFC2845]
  DnsQueryIXFR         = 251; // incremental transfer    [RFC1995]
  { Some additional type only allowed in queries }
  DnsQueryAXFR        = 252; { Transfer for an entire zone                       }
//DnsQueryMAILB        = 253; { Mailbox related records (MB, MG or MR)           }
//DnsQueryMAILA        = 254; { MailAgent, obsolete, use MX instead              }
//DnsQueryALL          = 255; { * Request ALL records  - offically dead          }
  DnsQueryURI          = 256; // URI Certification  [RFC7553]
  DnsQueryCAA          = 257; // Authority Restriction Application  [RFC6844]
  DnsQueryAVC          = 258; // Visibility and Control [Wolfgang_Riedel]
  DnsQueryDOA          = 259; // Digital Object Architecture [draft-durand-doa-over-dns]
  DnsQueryAMTRELAY     = 260; // Automatic Multicast Tunneling Relay      [draft-ietf-mboned-driad-amt-discovery]
  DnsQueryTA           = 32768;  // DNSSEC TrustAuthorities
  DnsQueryDLV          = 32769;  //DNSSEC Lookaside  Validation     [RFC4431]

{ Opcode field in query flags }
  DnsOpCodeQUERY  = 0;
  DnsOpCodeIQUERY = 1;
  DnsOpCodeSTATUS = 2;
  DnsOpCodeNOTIFY = 4;
  DnsOpCodeUPDATE = 5;
  DnsOpCodeDSO    = 6;

{ V8.61 status response codes }
  DnsRCodeNoError        = 0;
  DnsRCodeFormatError    = 1;
  DnsRCodeServerFailure  = 2;
  DnsRCodeNameError      = 3;
  DnsRCodeNotImplemented = 4;
  DnsRCodeRefused        = 5;


{ V8.61 table of common DNS record types }
type
    TQueryInfo = record
        Num  : Integer;
        Asc  : String;
        Desc : String;
    end;

const
    DnsReqTable: array[0..40] of TQueryInfo = (
      (Num: DnsQueryA;      Asc: 'A';      Desc: 'Host Address IPv4'),
      (Num: DnsQueryAAAA;   Asc: 'AAAA' ;  Desc: 'Host Address IPv6'),    { V8.71 re-arranged order }
      (Num: DnsQueryCNAME;  Asc: 'CNAME';  Desc: 'Canonical Name'),
      (Num: DnsQueryNS;     Asc: 'NS';     Desc: 'Name Server'),
      (Num: DnsQueryPTR;    Asc: 'PTR';    Desc: 'Domain Name Pointer'),
      (Num: DnsQueryMX;     Asc: 'MX';     Desc: 'Mail Exchange'),
      (Num: DnsQuerySOA;    Asc: 'SOA';    Desc: 'Start of a Zone of Authority'),
      (Num: DnsQueryTXT;    Asc: 'TXT';    Desc: 'SPF, DKIM, DMARC, etc'),
      (Num: DnsQueryOPT;    Asc: 'OPT';    Desc: 'OPT'),
      (Num: DnsQueryCAA;    Asc: 'CAA';    Desc: 'Authority Restriction Application'),
      (Num: DnsQueryAFSDB;  Asc: 'AFSDB';  Desc: 'AFS DB'),
      (Num: DnsQuerySIG;    Asc: 'SIG';    Desc: 'Signature'),
      (Num: DnsQueryKEY;    Asc: 'KEY';    Desc: 'Key record'),
      (Num: DnsQueryLOC;    Asc: 'LOC';    Desc: 'Location'),
      (Num: DnsQuerySRV;    Asc: 'SRV';    Desc: 'Service Locator'),
      (Num: DnsQueryNAPTR;  Asc: 'NAPTR';  Desc: 'Name Authority Pointer'),
      (Num: DnsQueryKX ;    Asc: 'KX';     Desc: 'Key Exchanger'),
      (Num: DnsQueryCERT;   Asc: 'CERT';   Desc: 'Certificate'),
      (Num: DnsQueryDNAME;  Asc: 'DNAME';  Desc: 'Canonical Name'),
      (Num: DnsQueryDS;     Asc: 'DS';     Desc: 'Delegation Signer (DNSSEC)'),
      (Num: DnsQuerySSHFP;  Asc: 'SSHFP';  Desc: 'SSH Key Fingerprint'),
      (Num: DnsQueryIPSECKEY;  Asc: 'IPSECKEY';   Desc: 'IPSec key'),
      (Num: DnsQueryRRSIG;  Asc: 'RRSIG';  Desc: 'DNSSEC Signature (DNSSEC)'),
      (Num: DnsQueryNSEC;   Asc: 'NSEC';   Desc: 'Next Secure Record (DNSSEC)'),
      (Num: DnsQueryDNSKEY; Asc: 'DNSKEY'; Desc: 'DNS key (DNSSEC)'),
      (Num: DnsQueryDHCID;  Asc: 'DHCID';  Desc: 'DHCP ID'),
      (Num: DnsQueryNSEC3;  Asc: 'NSEC3';  Desc: 'Next Secure Record v3 (DNSSEC)'),
      (Num: DnsQueryNSEC3PARAM;  Asc: 'NSEC3PARAM'; Desc: 'NSEC3 Params (DNSSEC)'),
      (Num: DnsQueryTLSA;   Asc: 'TLSA';   Desc: 'TLSA Certificate'),
      (Num: DnsQuerySMIMEA; Asc: 'SMIMEA'; Desc: 'S/MIME cert association'),
      (Num: DnsQueryHIP;    Asc: 'HIP';    Desc: 'Host Identity Protocol'),
      (Num: DnsQueryTALINK; Asc: 'TALINK'; Desc: 'Trust Anchor LINK'),
      (Num: DnsQueryCDS;    Asc: 'CDS';    Desc: 'Child DS DNSKEY (DNSSEC)'),
      (Num: DnsQueryCDNSKEY;Asc: 'CDNDKEY';Desc: 'Child copy of DNSKEY (DNSSEC)'),
      (Num: DnsQueryOPENPGPKEY;   Asc: 'OPENPGKEY'; Desc: 'OpenPGP Key'),
      (Num: DnsQueryCSYNC;  Asc: 'CSYNC';  Desc: 'Child-To-Parent Sync'),
      (Num: DnsQueryZONEMD; Asc: 'ZONEMD'; Desc: 'Message digest for DNS zone'),
      (Num: DnsQueryEUI48;  Asc: 'EUI48';  Desc: 'an EUI-48 address'),
      (Num: DnsQueryEUI64;  Asc: 'EUI64';  Desc: 'an EUI-64 address'),
      (Num: DnsQueryTKEY;   Asc: 'TKEY';   Desc: 'Transaction Key'),
      (Num: DnsQueryURI;    Asc: 'URI';    Desc: 'URI Certification') );

  { V8.61 status respoonse code literals }
    DnsRCodeTable: array[DnsRCodeNoError..DnsRCodeRefused] of String = (
      'Success', 'Format Error', 'Server Failure', 'Name Error', 'Not Implemented', 'Refused');

  { V8.61 perform all (or most) requests sequentually }
    DnsAllReqTot = 8;                                                { V8.71 added CAA to All }
    DnsAllReqTable:  array[1..DnsAllReqTot] of Integer = (
       DnsQueryA, DnsQueryAAAA, DnsQueryCNAME, DnsQueryNS, DnsQueryMX, DnsQuerySOA, DnsQueryTXT, DnsQueryCAA);

  { V8.61 public DNS servers }
    DnsPublicServerTable: array[0..15] of String = (
       '8.8.8.8 [Google]',
       '9.9.9.9 [Quad9]',
       '208.67.222.222 [OpenDNS]',
       '1.1.1.1 [Cloudfare]',        { V8.71 moved from top later since not always reliable with TXT records }
       '8.8.4.4 [Google]',
       '149.112.112.112 [Quad9]',
       '208.67.220.220 [OpenDNS]',
       '1.0.0.1 [Cloudfare]',
       '2606:4700:4700::1111 [Cloudfare]',
       '2001:4860:4860::8888 [Google]',
       '2620:fe::fe [Quad9]',
       '2620:119:35::35 [OpenDNS]',
       '2606:4700:4700::1001 [Cloudfare]',
       '2001:4860:4860::8844 [Google]',
       '2620:fe::9 [Quad9]',
       '2620:119:53::53 [OpenDNS]');

  { V8.61 public DNS servers using DOS - DNS over Https }
    DnsPublicHttpsTable: array[0..5] of String = (
        'https://cloudflare-dns.com/dns-query',
        'https://dns.google/dns-query',    // V8.71 only supports wire format
        'https://dns.google/resolve',      // V8.71 only supports Json
        'https://dns.quad9.net/dns-query',
        'https://dns.opendns.com/dns-query',     // V8.71 new
        'https://doh.appliedprivacy.net/query');

type
  TDnsRequestDoneEvent = procedure (Sender : TObject; Error : WORD) of Object;
  TDnsLogEvent = procedure (Sender: TObject; const Msg: string) of object;

  TDnsRequestHeader = packed record
      ID      : WORD;
      Flags   : WORD;
      QDCount : WORD;
      ANCount : WORD;
      NSCount : WORD;
      ARCount : WORD;
  end;
  PDnsRequestHeader = ^TDnsRequestHeader;

  // rfc 1035 p.19
  TSoaRecord = record
    mname   : AnsiString;
    rname   : AnsiString;
    serial  : Cardinal;
    refresh : Cardinal;
    retry   : Cardinal;
    expire  : Cardinal;
    minimum : Cardinal;
  end;

  // Question Data rfc1035 p.28
  TQuestion = record
    QuestionType   : word;
    QuestionClass  : word;
    QuestionName   : AnsiString;
  end;


  // rfc 1035 p.10
  TRRInternal = packed record
    rrtype   : word;     // r due to token conflict
    rrclass  : word;     // same
    rrttl    : cardinal; // same
    rdlength : word;
  end;
  pRRInternal = ^TRRInternal;


  TLOCInfo = packed record { need to be 16 bytes }
    version    : byte;
    size       : byte;
    horizpre   : byte;
    vertpre    : byte;
    latitude   : longint;
    longitude  : longint;
    altitude   : longint;
  end;
  PLOCInfo = ^TLOCInfo;

  { Decoded TLOCInfo }
  TLogGeo = record
    version             : byte;
    longsize            : integer;
    latsize             : integer;
    horizpre            : integer;
    vertpre             : integer;
    { Latitude, degree, minutes, seconds, milliseconds }
    lad, lam, las, lams : integer;
    lahem               : AnsiChar;
    { same for Longitude }
    lod, lom, los, loms : integer;
    lohem               : AnsiChar;
    altitude            : integer;
  end;

 // V8.61 Result Record
  TRRRecord = packed record
    RRName    : AnsiString;
    RRType    : Word;      // r due to token conflict
    RRClass   : Word;      // same
    TTL       : Cardinal;  // same
    RDLength  : Word;
    RDData    : AnsiString;  // actual result as raw string
    HostName  : String;      // V8.64 for MX and PTR hostnames, IDN
    AnswerName : String;     // V8.64 for MX and PTR hostnames, IDN xxxx
    IPV4      : TInAddr;
    IPv6      : TIcsIPv6Address;
    MxPref    : Integer;
    SOA       : TSoaRecord;
    Locdecode : TLogGeo;
 end;

  TDnsAnswerNameArray   = packed array [0..MAX_ANCOUNT - 1]     of String;     { V8.64 }
  TDnsAnswerTypeArray   = packed array [0..MAX_ANCOUNT - 1]     of Integer;
  TDnsAnswerClassArray  = packed array [0..MAX_ANCOUNT - 1]     of Integer;
  TDnsAnswerTTLArray    = packed array [0..MAX_ANCOUNT - 1]     of LongInt;
  TDnsAnswerTagArray    = packed array [0..MAX_ANCOUNT - 1]     of Integer;
  TDnsRRRecordArray     = packed array [0..MAX_ANCOUNT - 1]     of TRRRecord; // V8.61
  TDnsMXPreferenceArray = packed array [0..MAX_MX_RECORDS - 1]  of Integer;
  TDnsMXExchangeArray   = packed array [0..MAX_MX_RECORDS - 1]  of String;     { V8.64 }
  TDnsAddressArray      = packed array [0..MAX_A_RECORDS - 1]   of TInAddr;
  TDnsHostnameArray     = packed array [0..MAX_PTR_RECORDS - 1] of String;     { V8.64 }
  TDnsAddress6Array     = packed array [0..MAX_A_RECORDS - 1]   of TIcsIPv6Address;  { V8.71 }
  TDnsTXTRecordArray    = packed array [0..MAX_A_RECORDS - 1]   of String;     { V8.71 }

  TDnsState = (DnsStLookupStart, DnsStLookupDone);                                    { V8.71 }

 { V8.71 DNS server strategy, One=old single host, List=common DnsSrvList, Pub=DnsPublicServerTable }
  TDnsSrvStrat = (SrvStratOne, SrvStratList, SrvStratPub);

  TDnsQuery = class(TIcsWndControl)  { V8.71 was TComponment }
  protected
    FWSocket                    : TWSocket;
    FPort                       : String;
    FAddr                       : String;
    FIDCount                    : WORD;
    FResponseID                 : Integer;
    FResponseCode               : Integer;
    FResponseOpCode             : Integer;
    FResponseAuthoritative      : Boolean;
    FResponseTruncation         : Boolean;
    FResponseRecursionAvailable : Boolean;
    FResponseQDCount            : Integer;
    FResponseANCount            : Integer;
    FResponseNSCount            : Integer;
    FResponseARCount            : Integer;
    FQuestionType               : Integer;
    FQuestionClass              : Integer;
    FQuestionName               : String;              { V8.64 }
    FAnswerNameArray            : TDnsAnswerNameArray;
    FAnswerTypeArray            : TDnsAnswerTypeArray;
    FAnswerClassArray           : TDnsAnswerClassArray;
    FAnswerTTLArray             : TDnsAnswerTTLArray;
    FAnswerTagArray             : TDnsAnswerTagArray;
    FAnswerRecordArray          : TDnsRRRecordArray;   { V8.61 }
    FAnsTot                     : Integer;             { V8.61 }
    FMultiReqSeq                : Integer;             { V8.61 }
    FMultiHost                  : String;              { V8.64 }
    FMXRecordCount              : Integer;
    FMXPreferenceArray          : TDnsMXPreferenceArray; { For MX request  }
    FMXExchangeArray            : TDnsMXExchangeArray;   { For MX request  }
    FMXSortedExchArray          : TDnsMXExchangeArray;   { V8.71 MX sorted names }
    FARecordCount               : Integer;
    FAddressArray               : TDnsAddressArray;      { For A request   }
    FAAAARecordCount            : Integer;               { V8.71 }
    FAddress6Array              : TDnsAddress6Array;     { V8.71 For AAAA request   }
    FPTRRecordCount             : Integer;
    FHostnameArray              : TDnsHostnameArray;     { For PTR request }
    FTXTRecordArray             : TDnsTXTRecordArray;    { V8.71 for TXT requests }
    FTXTRecordCount             : Integer;               { V8.71 }
    FOnRequestDone              : TDnsRequestDoneEvent;
    FOnLogEvent                 : TDnsLogEvent;         { V8.71 }
    FProto                      : String;                { default to udp  }
    FGotPacketLength            : Boolean; { for tcp, set if packet length received }
    FLengthByte                 : array [0..1] of BYTE; {  for tcp         }
    fLOCInfo                    : TLOCInfo;
    FQueryBuf                   : array [0..511] of AnsiChar;
    FQueryLen                   : Integer;
    FResponseBuf                : array [0..2047] of AnsiChar;
    FResponseLen                : Integer;
    FTimeout                    : Integer; { Sync Timeout Seconds }  { V8.71 }
    FState                      : TDnsState; { sync state }          { V8.71 }
    FLastError                  : Integer;   { from OnRequestDone }  { V8.71 }
    FServerList                 : TStrings;  { DNS server list }     { V8.71 }
    FServerStrat                : TDnsSrvStrat; { DNS server strategy }   { V8.71 }
    FServerAddr                 : String;    { current server address }   { V8.71 }
    FServerCur                  : Integer;   { current server in list }   { V8.71 }
    FQueryCur                   : Integer;   { current query }            { V8.71 }
    FServerAttempt              : Integer;   { how many attempts {        { V8.71 }
    FServerMax                  : Integer;   { max server attempts }      { V8.71 }
    FMultiReq                   : Boolean;   { doing multi request }      { V8.71 }
    FBothAReq                   : Boolean;   { doing A then AAAA requests }    { V8.71 }
    function GetMXPreference(nIndex : Integer) : Integer;
    function GetMXExchange(nIndex : Integer)   : String;
    function GetMXSortedExch(nIndex : Integer) : String;      { V8.71 }
    function GetAnswerName(nIndex : Integer)   : String;
    function GetAnswerType(nIndex : Integer)   : Integer;
    function GetAnswerClass(nIndex : Integer)  : Integer;
    function GetAnswerTTL(nIndex : Integer)    : LongInt;
    function GetAnswerRecord(nIndex : Integer) : TRRRecord;   { V8.61 }
    function GetAnswerTag(nIndex : Integer)    : Integer;
    function GetAddress(nIndex : Integer)      : TInAddr;
    function GetAddress6(nIndex : Integer)     : TIcsIPv6Address;  { V8.71 }
    function GetHostname(nIndex : Integer)     : String;
    function GetTXTRecord(nIndex : Integer)    : String;           { V8.71 for TXT requests }
    procedure WSocketDataAvailable(Sender: TObject; Error: WORD); virtual;
    procedure WSocketSessionConnected(Sender: TObject; Error: WORD); virtual;
    procedure TriggerRequestDone(Error: WORD); virtual;
    function  GetResponseBuf : PAnsiChar;
    procedure SendQuerySync;                                       { V8.71 }
    procedure SendQuery;
    function  ExtractName(Base       : PAnsiChar;
                          From       : PAnsiChar;
                          var Name   : AnsiString) : PAnsiChar;
    function  GetMultiThreaded: Boolean;
    procedure SetMultiThreaded(const Value: Boolean); override;
    procedure SetProto(const Value : String);
    procedure SetAddr(const Value : String);
    function    DecodeWireResp(RespBuffer: PAnsiChar; BufLen: Integer): Boolean;
    procedure   BuildRequestHeader(Dst     : PDnsRequestHeader;
                                 ID        : WORD;
                                 OPCode    : BYTE;
                                 Recursion : Boolean;
                                 QDCount   : WORD;
                                 ANCount   : WORD;
                                 NSCount   : WORD;
                                 ARCount   : WORD); virtual;
    function    BuildQuestionSection(Dst       : PAnsiChar;
                                   QName       : String;   { V8.64 }
                                   QType       : WORD;
                                   QClass      : WORD) : Integer; virtual;
    procedure ResetCounters(QNumber: integer);                                        { V8.71 }
    procedure PrepQuery(Host : String; QNumber : integer);                            { V8.71 }
    function  StartQuery(Host : String; QNumber : integer): Integer;                  { V8.71 }
    function  StartQuerySync(Host: String; QNumber: integer): Boolean;                { V8.71 }
    procedure SetServerList(Value: TStrings);                                         { V8.71 }
    procedure SetServerStrat(Value: TDnsSrvStrat);                                    { V8.71 }
    procedure DiagLog(const S: String);                                               { V8.71 }
    procedure InternalAbort;                                                          { V8.71 }
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Notification(AComponent: TComponent; operation: TOperation); override;
    function    MXLookup(Domain : String) : Integer;    { V8.64 }
    function    ALookup(Host : String) : Integer;       { V8.64 }
    function    AAAALookup(Host : String) : Integer;    { V8.71 }
    function    PTRLookup(IP : String) : Integer;       { V8.64 }
    function    QueryAll(Host : String) : Integer;      { V8.64 }
    function    QueryBothA(Host : String) : Integer;    { V8.71 does A then AAAA }
    function    QueryAny(Host : String; QNumber : integer) : Integer;  { V8.64 }
    procedure   AbortQuery;                                { V8.61 }
    function    MXLookupSync(Domain : String) : Boolean;   { V8.71 }
    function    ALookupSync(Host : String) : Boolean;      { V8.71 }
    function    AAAALookupSync(Host : String) : Boolean;   { V8.71 }
    function    PTRLookupSync(IP : String) : Boolean;      { V8.71 }
    function    QueryAllSync(Host : String) : Boolean;     { V8.71 }
    function    QueryBothASync(Host : String) : Boolean;   { V8.71 }
    function    QueryAnySync(Host : String; QNumber : integer) : Boolean; { V8.71 }
    property ResponseID                 : Integer read FResponseID;
    property ResponseCode               : Integer read FResponseCode;
    property ResponseOpCode             : Integer read FResponseOpCode;
    property ResponseAuthoritative      : Boolean read FResponseAuthoritative;
    property ResponseTruncation         : Boolean read FResponseTruncation;
    property ResponseRecursionAvailable : Boolean read FResponseRecursionAvailable;
    property ResponseQDCount            : Integer read FResponseQDCount;
    property ResponseANCount            : Integer read FResponseANCount;
    property ResponseNSCount            : Integer read FResponseNSCount;
    property ResponseARCount            : Integer read FResponseARCount;
    property ResponseBuf                : PAnsiChar   read GetResponseBuf;
    property ResponseLen                : Integer read FResponseLen;
    property QuestionType               : Integer read FQuestionType;
    property QuestionClass              : Integer read FQuestionClass;
    property QuestionName               : String  read FQuestionName;       { V8.64 }
    property AnswerName[nIndex : Integer]   : String  read GetAnswerName;   { V8.64 }
    property AnswerType[nIndex : Integer]   : Integer read GetAnswerType;
    property AnswerClass[nIndex : Integer]  : Integer read GetAnswerClass;
    property AnswerTTL[nIndex : Integer]    : LongInt read GetAnswerTTL;
    property AnswerTag[nIndex : Integer]    : Integer read GetAnswerTag;
    property AnswerRecord[nIndex : Integer] : TRRRecord read GetAnswerRecord;  { V8.61 }
    property AnswerTotal                    : Integer read FAnsTot;            { V8.61 }
    property MXRecordCount                  : Integer read FMXRecordCount;     { V8.71 }
    property MXPreference[nIndex : Integer] : Integer read GetMXPreference;
    property MXExchange[nIndex : Integer]   : String  read GetMXExchange;    { V8.64 }
    property MXSortedExch[nIndex : Integer] : String  read GetMXSortedExch;  { V8.71 MX sorted names }
    property ARecordCount                   : Integer read FARecordCount;    { V8.71 }
    property Address[nIndex : Integer]      : TInAddr read GetAddress;
    property AAAARecordCount                : Integer read FAAAARecordCount; { V8.71 }
    property Address6[nIndex : Integer]     : TIcsIPv6Address read GetAddress6;  { V8.71 }
    property PTRRecordCount                 : Integer read FPTRRecordCount;  { V8.71 }
    property Hostname[nIndex : Integer]     : String  read GetHostname;      { V8.64 }
    property TXTRecordCount                 : Integer read FTXTRecordCount;  { V8.71 }
    property TXTRecord[nIndex : Integer]    : String  read GetTXTRecord;     { V8.71 for TXT requests }
    property Loc                            : TLOCInfo read fLOCInfo;
    property LastError                      : Integer read FLastError;      { V8.71 }
    property ServerAddr                     : String  read FServerAddr;    { current server address }  { V8.71 }
  published
    property Port    : String read  FPort  write FPort;
    property Addr    : String read  FAddr  write SetAddr;
    property Proto   : String read  FProto write SetProto;
    property MultiThreaded   : Boolean            read  GetMultiThreaded
                                                  write SetMultiThreaded;
    property Timeout : Integer read  FTimeout  write FTimeout; { Sync Timeout Seconds V8.71 }
    property ServerList : TStrings read  FServerList  write SetServerList;  { DNS server list V8.71 }
    property ServerStrat : TDnsSrvStrat read  FServerStrat  write SetServerStrat; { DNS server strategy } { V8.71 }
    property ServerCur : Integer read  FServerCur  write FServerCur;   { current multiple server V8.71 }
    property ServerMax : Integer read  FServerMax  write FServerMax;   { max server attempts V8.71 }
    property OnRequestDone : TDnsRequestDoneEvent read  FOnRequestDone
                                                  write FOnRequestDone;
    property OnLogEvent : TDnsLogEvent            read  FOnLogEvent
                                                  write FOnLogEvent;         { V8.71 }
  end;


{ V8.71 Domain Name cache, forward and backwards }
const
  { The following byte stream contains the necessary message to request a NetBios name from a machine on UDP port 137 }
  NetBiosNameReq: array[0..49] of Byte  = (
        $80, $94, $00, $00, $00, $01, $00, $00,   $00, $00, $00, $00, $20, $43, $4b, $41,
        $41, $41, $41, $41, $41, $41, $41, $41,   $41, $41, $41, $41, $41, $41, $41, $41,
        $41, $41, $41, $41, $41, $41, $41, $41,   $41, $41, $41, $41, $41, $00, $00, $21,
        $00, $01);

type
  TDNReqType = (ReqTypeDnsForw, ReqTypeDnsBack, ReqTypeNetBios);
  TDNState = (StateNone, StateWaiting, StateOK, StateFailed);
  TDNMethod = (MethodWinsock, MethodUdp, MethodTcp, MethodHttps);
  TDBLANlookup = (LanLookDef, LanLookWSock, LanLookNetBios);
  TDNUpdateEvent = procedure (Sender: TObject; ItemNr: Integer) of object;
  TDNLogEvent = procedure (Sender: TObject; const Msg: string) of object;

  TDNItem = record
    Request: String;              // host name or IP address
    ReqTag: Integer;              // for application use
    ReqUpdEvent: TDNUpdateEvent;  // which event to call
    ReqFamily: TSocketFamily;     // do we want one or other or both
    Responses4: array of String;  // often IPv4 multiple responses
    Responses6: array of String;  // often IPv6 multiple responses
    TotResp4: Integer;
    TotResp6: Integer;
    TimeStamp: TDateTime;        // if non-zero, last lookup attempt
    TTL: Integer;                // seconds time to live before expiry
    DNReqType: TDNReqType;       // forward or reverse
    DNState: TDNState;           // lookup progress
    Index: Integer;              // index into array, may change if compressed
  end;
  PDNItem = ^TDNItem;
  TDNItems = array of TDNItem;

  TCntlLookup = record
    Busy: Boolean;
    ItemNr: Integer;
    Request: String;
    StartTick: Int64;
  end;

  TIcsDomainNameCache = class(TIcsWndControl)
  protected
    FDNItems: TDNItems;
    FTotDNItems: Integer;
    FDNIndex: TIcsFindList;
    FDNMethod: TDNMethod;
    FDBLANlookup: TDBLANlookup;
    FDefTTL: Integer;
    FMaxLookups: Integer;            // how many parallel lookups to run
    FQTimeout: Integer;
    FDefHostList: TStrings;         // predefined hosts to add to cache on start-up
    FAddLocalhost: Boolean;
    FDnsServerList: TStrings;       // list of DNS servers for DnsQuery component
    FDnsServerStrat: TDnsSrvStrat;
    FDNUpdateEvent: TDNUpdateEvent;
    FDNLogEvent: TDNLogEvent;
    FDNCacheFile: String;
    FDnsQuerys: array of TDnsQuery;
    FDNWSockets: array of TWSocket;
    FCntlLookups: array of TCntlLookup;
    FLookupQu: TIcsIntegerList;      // FIFO queue of pending lookups, by ItemNr
    FMaintTimer: TIcsTimer;
    FDefListDone: Boolean;
    procedure LogEvent (const Info: String);
    procedure TriggerUpdate(ItemNr: Integer);
    procedure WsockDnsRequestDone(Sender : TObject; Error : WORD);
    procedure WsockDataAvailable(Sender: TObject; Error: WORD);
    procedure WsockSessionConnected(Sender: TObject; Error: WORD);
    procedure DnsQueryRequestDone(Sender: TObject; Error: Word);
    procedure DNLogEvent(Sender: TObject; const Msg: string);
    procedure DnsLogEvent(Sender: TObject; const Msg: string);
    procedure MaintTimerOnTimer(Sender : TObject);
    function StartWSocket(ItemNr: Integer): Boolean;
    function StartDnsQuery(ItemNr: Integer): Boolean;
    function StartRequest(ItemNr: Integer): Boolean; virtual;
    procedure StartPendingRequest; virtual;
    procedure SetMaxLookups(Value: Integer);
    procedure SetDNMethod(Value: TDNMethod);
    procedure RebuildIndex;
    function AddNewRec(DNItem: TDNItem): Integer;
    function DeleteRec(const Request: String): Boolean;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure AddtoCache(const IPandHost: String); overload;
    procedure AddtoCache(const IP, Host: String); overload;
    procedure AddListtoCache(HostList: TStrings);
    function FindRequest(const Request: String): Integer;
    function LookupRequest(const Req: String; Tag: Integer; Sync: Boolean; ReqType: TDNReqType; DNFamily: TSocketFamily = sfAny;
                                                                      UpdEvent: TDNUpdateEvent = Nil; SkipEvent: Boolean = False): Integer;
    function LookupHostSync(const Host: String; Tag: Integer = 0; DNFamily: TSocketFamily = sfAny; UpdEvent: TDNUpdateEvent = Nil): Integer;
    function LookupIPSync(const IP: String; Tag: Integer = 0; DNFamily: TSocketFamily = sfAny; UpdEvent: TDNUpdateEvent = Nil): Integer;
    function LookupHostAsync(const Host: String; Tag: Integer = 0; DNFamily: TSocketFamily = sfAny; UpdEvent: TDNUpdateEvent = Nil): Integer;
    function LookupIPAsync(const IP: String; Tag: Integer = 0; DNFamily: TSocketFamily = sfAny; UpdEvent: TDNUpdateEvent = Nil): Integer;
    function LookupHostOne(const Host: String; Tag: Integer = 0; DNFamily: TSocketFamily = sfAny; UpdEvent: TDNUpdateEvent = Nil): String;
    function LookupIPOne(const IP: String; Tag: Integer = 0; DNFamily: TSocketFamily = sfAny; UpdEvent: TDNUpdateEvent = Nil): String;
    function GetHost(const Host: String; DNFamily: TSocketFamily = sfAny): String;
    function GetIP(const IP: String): String;
    procedure MaintClearAll;
    procedure MaintUpdateAll;
    procedure CancelLookups;
    function GetDNItem(ItemNr: Integer): TDNItem;
    function BuildRespList(ItemNr: Integer): String;
    function PendingLookups: Integer;
    function ListCache: String;
    function SaveCacheToFile: Boolean;
    function ReadCacheFromFile: Boolean;
    property TotDNItems: Integer                read FTotDNItems;
  published
    property DNMethod: TDNMethod                read  FDNMethod  write SetDNMethod;
    property DnsServerList: TStrings            read  FDnsServerList  write FDnsServerList;
    property DnsServerStrat: TDnsSrvStrat       read  FDnsServerStrat  write FDnsServerStrat;
    property DefTTL : Integer                   read  FDefTTL  write FDefTTL;
    property MaxLookups: Integer                read  FMaxLookups  write SetMaxLookups;
    property DNCacheFile: String                read  FDNCacheFile  write FDNCacheFile;
    property QTimeout : Integer                 read  FQTimeout  write FQTimeout;
    property AddLocalhost: Boolean              read  FAddLocalhost  write FAddLocalhost;
    property DefHostList: TStrings              read  FDefHostList  write FDefHostList;
    property DBLANlookup: TDBLANlookup          read  FDBLANlookup  write FDBLANlookup;
    property OnDNUpdateEvent: TDNUpdateEvent    read  FDNUpdateEvent  write FDNUpdateEvent;
    property OnDNLogEvent: TDNLogEvent          read  FDNLogEvent write FDNLogEvent;
end;


function LongLatToDMS(longlat : longint; hemis : AnsiString) : AnsiString; { !!KAP!! }
function Loc2Geo(loc : TLOCInfo) : TLogGeo;                        { !!KAP!! }
function FindDnsReqTypeName(TypeID: Integer): String;  { V8.61 }
function FindDnsReqTypeId(TypeName: String): Integer;  { V8.61 }
{$IFDEF MSWINDOWS}
function GetNetBiosHostByAddr(IPv4: String): String;   { V8.71 }
{$ENDIF}

implementation

type
    PWORD  = ^WORD;
    PDWORD = ^DWORD;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FindDnsReqTypeName(TypeID: Integer): String;  { V8.61 }
var
    I: integer;
begin
    Result := '';
    for I := Low(DnsReqTable) to High(DnsReqTable) do begin
        if DnsReqTable[I].Num = TypeID then begin
             Result := DnsReqTable[I].Asc;
             Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FindDnsReqTypeId(TypeName: String): Integer;  { V8.61 }
var
    I: integer;
begin
    Result := 0;
    for I := Low(DnsReqTable) to High(DnsReqTable) do begin
        if DnsReqTable[I].Asc = TypeName then begin
             Result := DnsReqTable[I].Num;
             Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TDnsQuery.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FWSocket         := TWSocket.Create(nil);
    FPort            := '53';
    FProto           := 'udp';
    FGotPacketLength := FALSE;
    FMultiReqSeq     := 0;
    FAnsTot          := 0;
    FTimeOut         := 5;  { V8.71 sync timeout seconds }
    FServerList      := TStringList.Create;  { V8.71 }
    FServerCur       := 0;                   { V8.71 }
    FServerMax       := 4;                   { V8.71 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TDnsQuery.Destroy;
begin
    try
        FreeAndNil(FServerList);                 { V8.71 }
        if Assigned(FWSocket) then begin
            FWSocket.Destroy;
            FWSocket := nil;
        end;
    finally
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.DiagLog(const S: String);                                               { V8.71 }
begin
    if Assigned(FOnLogEvent) then
        FOnLogEvent(Self, S);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.SetMultiThreaded(const Value: Boolean);
begin
    if Assigned(FWSocket) then
        FWSocket.Multithreaded := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetMultiThreaded: Boolean;
begin
    if Assigned(FWSocket) then
        Result := FWSocket.Multithreaded
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.SetProto(const Value: String);
var
    Buf : String;
begin
    Buf := LowerCase(Value);
    if not ((Buf = 'tcp') or (Buf = 'udp')) then
        raise Exception.Create('TDnsQuery accept only TCP or UDP protocol');
    FProto := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.SetAddr(const Value : String);  { V8.61 }
var
    I: Integer;
begin
    FAddr := Value;
    I := Pos (' [', FAddr);  // remove comment after IP address
    if I > 1 then
        SetLength (FAddr, I - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.SetServerList(Value: TStrings);                                         { V8.71 }
begin
    FServerList.Assign(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.SetServerStrat(Value: TDnsSrvStrat);                                    { V8.71 }
begin
    if FServerStrat <> Value then begin
        FServerCur := 0;
    end;
    FServerStrat := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.Notification(AComponent: TComponent; operation: TOperation);
begin
    inherited Notification(AComponent, operation);
    if operation = opRemove then begin
        if AComponent = FWSocket then
            FWSocket := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetMXPreference(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FMXPreferenceArray)) or
       (nIndex > High(FMXPreferenceArray)) then
        Result := 0
    else
        Result := FMXPreferenceArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetMXExchange(nIndex : Integer) : String;  { V8.64 }
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FMXExchangeArray)) or
       (nIndex > High(FMXExchangeArray)) then
        Result := ''
    else
        Result := FMXExchangeArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetMXSortedExch(nIndex : Integer) : String;  { V8.71 }
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FMXSortedExchArray)) or
       (nIndex > High(FMXSortedExchArray)) then
        Result := ''
    else
        Result := FMXSortedExchArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerName(nIndex : Integer) : String;  { V8.64 }
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerNameArray)) or
       (nIndex > High(FAnswerNameArray)) then
        Result := ''
    else
        Result := FAnswerNameArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerType(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerTypeArray)) or
       (nIndex > High(FAnswerTypeArray)) then
        Result := 0
    else
        Result := FAnswerTypeArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerClass(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerClassArray)) or
       (nIndex > High(FAnswerClassArray)) then
        Result := 0
    else
        Result := FAnswerClassArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerTTL(nIndex : Integer) : LongInt;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerTTLArray)) or
       (nIndex > High(FAnswerTTLArray)) then
        Result := 0
    else
        Result := FAnswerTTLArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerTag(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerTagArray)) or
       (nIndex > High(FAnswerTagArray)) then
        Result := 0
    else
        Result := FAnswerTagArray[nIndex];
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerRecord(nIndex : Integer) : TRRRecord;   { V8.61 }
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerRecordArray)) or
       (nIndex > High(FAnswerRecordArray)) then
     //   Result := Nil
    else
        Result := FAnswerRecordArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAddress(nIndex : Integer) : TInAddr;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAddressArray)) or
       (nIndex > High(FAddressArray)) then
        Result.S_addr := 0
    else
        Result := FAddressArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAddress6(nIndex: Integer): TIcsIPv6Address;  { V8.71 }
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAddress6Array)) or
       (nIndex > High(FAddress6Array)) then
        FillChar(Result, SizeOf(TIcsIPv6Address), 0)
    else
        Result := FAddress6Array[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetHostname(nIndex : Integer) : String;  { V8.64 }
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FHostnameArray)) or
       (nIndex > High(FHostnameArray)) then
        Result := ''
    else
        Result := FHostnameArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetTXTRecord(nIndex : Integer) : String;           { V8.71 for TXT requests }
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FTXTRecordArray)) or
       (nIndex > High(FTXTRecordArray)) then
        Result := ''
    else
        Result := FTXTRecordArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetResponseBuf : PAnsiChar;
begin
    Result := @FResponseBuf;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.MXLookup(Domain : String) : Integer;
begin
    FMultiReq := False;
    Result := StartQuery(Domain, DnsQueryMX);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.MXLookupSync(Domain: String): Boolean;  { V8.71 }
begin
    FMultiReq := False;
    Result := StartQuerySync(Domain, DnsQueryMX);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.ALookup(Host : String) : Integer;
begin
    FMultiReq := False;
    Result := StartQuery(Host, DnsQueryA);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.ALookupSync(Host: String): Boolean;  { V8.71 }
begin
    FMultiReq := False;
    Result := StartQuerySync(Host, DnsQueryA);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.AAAALookup(Host: String): Integer;  { V8.71 }
begin
    FMultiReq := False;
    Result := StartQuery(Host, DnsQueryAAAA);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.AAAALookupSync(Host: String): Boolean;  { V8.71 }
begin
    FMultiReq := False;
    Result := StartQuerySync(Host, DnsQueryAAAA);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.PTRLookup(IP : String) : Integer;
begin
    FMultiReq := False;
    Result := StartQuery(IP, DnsQueryPTR);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.PTRLookupSync(IP: String): Boolean;  { V8.71 }
begin
    FMultiReq := False;
    Result := StartQuerySync(IP, DnsQueryPTR);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.61 support all requests, async triggers event when done }
{ returns IDCount which should be checked against response for correct query }
function TDnsQuery.QueryAny(Host : String; QNumber : integer) : Integer;
begin
    FMultiReq := False;
    Result := StartQuery(Host, QNumber);  { V8.71 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
 { V8.71 support all requests, sync waits until done, also triggers event }
function TDnsQuery.QueryAnySync(Host: String; QNumber: integer): Boolean;
begin
    FMultiReq := False;
    Result := StartQuerySync(Host, QNumber);  { V8.71 }
 end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
 { V8.61 simulate ALL by asking list of multiple questions }
function TDnsQuery.QueryAll(Host : String) : Integer;
begin
    FMultiReqSeq := 1;
    FMultiReq := True;
    FBothAReq := False;
    FAnsTot := 0;
    Result := StartQuery(Host, DnsAllReqTable[FMultiReqSeq]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.71 does A then AAAA }
function TDnsQuery.QueryBothA(Host : String) : Integer;
begin
    FMultiReqSeq := 1;
    FMultiReq := True;
    FBothAReq := True;
    FAnsTot := 0;
    Result := StartQuery(Host, DnsQueryA);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.QueryAllSync(Host: String): Boolean;  { V8.71 }
begin
    FMultiReqSeq := 1;
    FMultiReq := True;
    FBothAReq := False;
    FAnsTot := 0;
    Result := StartQuerySync(Host, DnsAllReqTable[FMultiReqSeq]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.QueryBothASync(Host: String): Boolean;  { V8.71 }
begin
    FMultiReqSeq := 1;
    FMultiReq := True;
    FBothAReq := True;
    FAnsTot := 0;
    Result := StartQuerySync(Host, DnsAllReqTable[FMultiReqSeq]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.71 reset result array counter for specific query }
procedure TDnsQuery.ResetCounters(QNumber: integer);
var
    I: Integer;
begin
    if QNumber = DnsQueryMX then begin
        FMXRecordCount := 0;
        for I := 0 to MAX_MX_RECORDS - 1 do begin
            FMXPreferenceArray[I] := 0;
            FMXExchangeArray[I] := '';
            FMXSortedExchArray[I] := '';
        end;
    end;
    if QNumber = DnsQueryA then begin
        FARecordCount  := 0;
        for I := 0 to MAX_A_RECORDS - 1 do begin
            FAddressArray[I].S_addr := 0;
        end;
    end;
    if QNumber in [DnsQueryA, DnsQueryAAAA] then begin
        FAAAARecordCount := 0;
        for I := 0 to MAX_A_RECORDS - 1 do begin
            FillChar(FAddress6Array[I], SizeOf(TIcsIPv6Address), 0)
        end;
    end;
    if QNumber in [DnsQueryPTR, DnsQueryNS, DnsQueryCNAME] then begin
        FPTRRecordCount := 0;
        for I := 0 to MAX_PTR_RECORDS - 1 do begin
            FHostnameArray[I] := '';
        end;
    end;
    if QNumber in [DnsQueryTXT] then begin
        FTXTRecordCount := 0;
        for I := 0 to MAX_A_RECORDS - 1 do begin
            FTXTRecordArray[I] := '';
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.71 prepare query }
procedure TDnsQuery.PrepQuery(Host : String; QNumber : integer);
begin
    FMultiHost := Host;
    FQueryCur := QNumber;
    FServerAttempt := 1;
    ResetCounters(QNumber);         { V8.71 }
    if NOT FMultiReq then
        FAnsTot := 0;  { V8.61 reset result records }
    Inc(FIDCount);
    FillChar(FQueryBuf, SizeOf(FQueryBuf), 0);  { V8.71 }
    BuildRequestHeader(PDnsRequestHeader(@FQueryBuf), FIDCount, DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
    FQueryLen := BuildQuestionSection(@FQueryBuf[SizeOf(TDnsRequestHeader)], Host, QNumber, DnsClassIN);
    FQueryLen := FQueryLen + SizeOf(TDnsRequestHeader);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.61 start an async DNS query, triggers event when done }
function TDnsQuery.StartQuery(Host : String; QNumber : integer): Integer;              { V8.71 }
begin
    PrepQuery(Host, QNumber);
    Result := FIDCount;
    SendQuery;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
 { V8.71 support all requests, sync waits until done, also triggers event }
function TDnsQuery.StartQuerySync(Host: String; QNumber: integer): Boolean;
begin
    PrepQuery(Host, QNumber);  { V8.71 }
    SendQuerySync;
    Result := (FAnsTot <> 0);
 end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.AbortQuery;                                { V8.61 called by app }
begin
    DiagLog('Aborting Query');
    FMultiReq := False;                  { ctop multiple requests and server attempts }
    FServerAttempt := FServerMax;
    InternalAbort;                                { V8.71 }
    FState := DnsStLookupDone;                    { V8.71 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.InternalAbort;                          { V8.71 called on query timeout }
begin
    FResponseLen := -1;
    FMultiReqSeq  := 0;
    FAnsTot := 0;
    if FWSocket.State <> wsClosed then
        FWSocket.Abort;
    TriggerRequestDone(999);          { may try another server }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.SendQuery;
var
    I: Integer;
begin
 { V8.71 allow list of DNS servers or public list to be used instead of Host }
    FLastError := 999;
    if FServerStrat = SrvStratOne then
        FServerAddr := FAddr      // backward compatible
    else if FServerStrat = SrvStratList then begin
        if (FServerCur >= FServerList.Count) then  // tried them all, start again
            FServerCur := 0;
        if FServerList.Count = 0  then
           FServerAddr := FAddr
        else
            FServerAddr := FServerList[FServerCur];
    end
    else if FServerStrat = SrvStratPub then begin
        if (FServerCur >= Length(DnsPublicServerTable)) then
            FServerCur := 0;
        FServerAddr := DnsPublicServerTable[FServerCur];
        I := Pos (' [', FServerAddr);  // remove comment after IP address
        if I > 1 then
            SetLength (FServerAddr, I - 1);
    end
    else
        Exit;
    if FServerAddr = '' then
        Exit;
    FState := DnsStLookupStart;              { V8.71 }
    FLastError := 0;                         { V8.71 }
    FResponseLen                := -1;
    FGotPacketLength            := FALSE;
    FWSocket.OnDataAvailable    := nil;
    if FWSocket.State <> wsClosed then
        FWSocket.Abort;
    FWSocket.OnDataAvailable    := WSocketDataAvailable;
    FWSocket.OnSessionConnected := WSocketSessionConnected;
    FWSocket.Proto              := FProto;
    FWSocket.Port               := FPort;
    FWSocket.Addr               := FServerAddr;
    DiagLog('Connecting to: ' + FServerAddr + ' for query: ' + FindDnsReqTypeName(FQueryCur));   { V8.71 }
    FWSocket.Connect;
    { Note: UDP is connectionless, nevertheless, TWSocket call              }
    { OnSessionConnected event handler immediately. For TCP the event       }
    { handler is called only when session is connected (or fails to)        }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.SendQuerySync;              { V8.71 }
var
    bFlag           : Boolean;
   {$IFDEF MSWINDOWS}
    dwWaitMs        : DWORD;
    {$ENDIF}
begin
    SendQuery;

  { now wait for query response }
    if not Assigned(FWSocket.Counter) then
        FWSocket.CreateCounter;
    FWSocket.Counter.LastSendTick := IcsGetTickCount64; // Reset counter
    {$IFDEF MSWINDOWS}
    if GetMultiThreaded then
       dwWaitMs:= 10 else
       dwWaitMs:= 1000;
    {$ENDIF}
    if FTimeOut = 0 then
        FTimeOut := 5;
    while FState <> DnsStLookupDone do begin
       {$IFDEF MSWINDOWS}
        if MsgWaitForMultipleObjects(0, Pointer(nil)^, FALSE, dwWaitMs, QS_ALLINPUT) = WAIT_OBJECT_0 then
       {$ENDIF}
            MessagePump;

        if (FState <> DnsStLookupDone) and (Terminated or (IcsElapsedSecs64(FWSocket.Counter.LastAliveTick) >= FTimeOut)) then begin
            bFlag := (FState = DnsStLookupStart);
            if bFlag then
            try
                InternalAbort;
            except
                { Ignore any exception }
            end;
            if FServerStrat = SrvStratOne then  // only only one DNS server, stop now otherwise keep looping
                FState := DnsStLookupDone;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.BuildQuestionSection(
    Dst         : PAnsiChar;
    QName       : String;   { V8.64 }
    QType       : WORD;
    QClass      : WORD) : Integer;
var
    I   : Integer;
    p   : PAnsiChar;
    Ptr : PAnsiChar;
    PunycodeHost: AnsiString;
    ErrFlag: Boolean;
begin
    Ptr := Dst;
    if Ptr = nil then begin
        Result := 0;
        Exit;
    end;
    I := 1;

// IPv6  4321:0:1:2:3:4:567:89ab becomes  b.a.9.8.7.6.5.0.4.0.0.0.3.0.0.0.2.0.0.0.1.0.0.0.0.0.0.0.1.2.3.4.ip6.arpa.
// IPv4  217.146.102.139 becomes 139.102.146.217.in-addr.arpa.
    if QType = DnsQueryPTR then begin   { V8.61 }
        if Pos (':', QName) > 1 then
            PunycodeHost := IcsReverseIPv6(QName) + '.ip6.arpa.'
        else
            PunycodeHost := IcsReverseIP(QName) + '.in-addr.arpa.';
    end
    else begin
    { V8.64 convert Unicode International Domain Name into Punycode ASCII }
    { ignore any conversion errors }
        PunycodeHost := AnsiString(IcsIDNAToASCII(IcsTrim(QName), False, ErrFlag));
        if ErrFlag then
            PunycodeHost := AnsiString(QName);
    end;

    while I <= Length(PunycodeHost) do begin
        p := Ptr;
        Inc(Ptr);
        while (I <= Length(PunycodeHost)) and (PunycodeHost[I] <> '.') do begin
            Ptr^ := PunycodeHost[I];
            Inc(Ptr);
            Inc(I);
        end;
        p^ := AnsiChar(Ptr - p - 1);
        Inc(I);
    end;
    Ptr^ := #0;
    Inc(Ptr);
    PWORD(Ptr)^ := WSocket_htons(QType);
    Inc(Ptr, 2);
    PWORD(Ptr)^ := WSocket_htons(QClass);
    Inc(Ptr, 2);
    Result := Ptr - Dst;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.BuildRequestHeader(
    Dst       : PDnsRequestHeader;
    ID        : WORD;
    OPCode    : BYTE;
    Recursion : Boolean;
    QDCount   : WORD;
    ANCount   : WORD;
    NSCount   : WORD;
    ARCount   : WORD);
begin
    if Dst = nil then
        Exit;
    Dst^.ID      := WSocket_htons(ID);
    Dst^.Flags   := WSocket_htons((OpCode shl 11) + (Ord(Recursion) shl 8));
    Dst^.QDCount := WSocket_htons(QDCount);
    Dst^.ANCount := WSocket_htons(ANCount);
    Dst^.NSCount := WSocket_htons(NSCount);
    Dst^.ARCount := WSocket_htons(ARCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.WSocketSessionConnected(Sender: TObject; Error: WORD);
var
    Buf: array [0..1] of BYTE;
begin
    if Error = 0 then begin
        if FProto = 'tcp' then begin { V6.03 }
            DiagLog('Connected, Sending Query') ; // ' + IcsBufferToHex(FQueryBuf, FQueryLen));
            Buf[0] := FQueryLen div 256;
            Buf[1] := FQueryLen mod 256;
            { Send 2 byte length for tcp packets, see RFC 1035 - 4.2.2. TCP usage }
            FWSocket.Send(@Buf[0], 2);
        end;
     // with UDP we send data without knowing if we are connected, no response means not connnected
        FWSocket.Send(@FQueryBuf, FQueryLen);
    end
    else begin
        DiagLog('Connect Failed, Error: ' + IntToStr(Error));
        TriggerRequestDone(Error);  { V8.61 don't ignore error }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.TriggerRequestDone(Error: WORD);
begin
    FLastError := Error;                       { V8.71 }
    if Error <> 0 then begin

     { V8.71 on timeout failure see if repeating using an alternate DNS server }
        if FServerStrat > SrvStratOne then begin
            Inc(FServerAttempt);
            if FWSocket.State = wsConnected then
                FWSocket.Abort;
            if (FServerAttempt < FServerMax) then begin  // stop if too many attempts
                Inc(FServerCur);
                DiagLog('Looking for next DNS server');
                if FServerStrat = SrvStratList then begin
                    SendQuery;
                    Exit;
                end
                else if FServerStrat = SrvStratPub then begin
                    SendQuery;
                    Exit;
                end;
            end;
        end;
    end;
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, Error);
    DiagLog('DNS query done, total answers: ' + IntToStr(FAnsTot));
    FState := DnsStLookupDone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.WSocketDataAvailable(Sender: TObject; Error: WORD);
var
    Len    : Integer;
begin
    FillChar(FResponseBuf, SizeOf(FResponseBuf), 0);  { V8.64 }
    if FProto = 'tcp' then begin
        if not FGotPacketLength then begin
            Len := FWSocket.PeekData(@FLengthByte, 2);
            if Len < 2 then
                Exit;
            FWSocket.Receive(@FLengthByte, 2);
            FGotPacketLength := TRUE;
        end;

        if not FGotPacketLength then
            Exit
        else begin
            Len := FWSocket.PeekData(@FResponseBuf, FLengthByte[0] * 256 + FLengthByte[1]);
            if Len < FLengthByte[0] * 256 + FLengthByte[1] then
                Exit;
            Len := FWSocket.Receive(@FResponseBuf, FLengthByte[0] * 256 + FLengthByte[1]);
            if Error <> 0 then begin
                FMultiReqSeq := 0;
                FMultiReq := False;
                FBothAReq := False;
                TriggerRequestDone(Error);
                Exit;
            end;
        end;
    end
    else begin
        Len := FWSocket.Receive(@FResponseBuf, SizeOf(FResponseBuf));
        if Error <> 0 then begin
            FMultiReqSeq := 0;
            FMultiReq := False;
            FBothAReq := False;
            TriggerRequestDone(Error);
            Exit;
        end;
    end;

 // get results
    if DecodeWireResp(@FResponseBuf, Len) then { V8.61 }
        DiagLog('Got DNS response OK')
    else
        DiagLog('No DNS response');
    FWSocket.Close;  // note TCP session closed each request

 // if simulating ALL request or both A and AAAA, make next request in sequence
    if FMultiReq then begin
        FMultiReqSeq := FMultiReqSeq + 1;
        if FBothAReq then begin
            if (FMultiReqSeq = 2) then begin
                StartQuery(FMultiHost, DnsQueryAAAA);
                Exit;
            end;
        end
        else if FMultiReqSeq <= DnsAllReqTot then begin
            StartQuery(FMultiHost, DnsAllReqTable[FMultiReqSeq]);
            Exit;
        end;
        FMultiReqSeq := 0;
        FMultiReq := False;
        FBothAReq := False;
    end;
    TriggerRequestDone(0);  // all done
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsDnsntohs(V : WORD) : Integer;         { V8.71 added IcsDns }
begin
    Result := ((V and $FF) shl 8) or ((V shr 8) and $FF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsDnsntohl(V : DWORD) : LongInt;        { V8.71 added IcsDns }
begin
    Result := (IcsDnsntohs(V and $FFFF) shl 16) or IcsDnsntohs((V shr 16) and $FFFF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.DecodeWireResp(RespBuffer: PAnsiChar; BufLen: Integer): Boolean; { V8.61 }
var
    AnsPtr: PDnsRequestHeader;
    Flags, I, J : Integer;
    P, PEnd  : PAnsiChar;
    Temp: AnsiString;   { V8.64 }
    MXList: TStringList ;  { V8.71 }

    function ProcessRespRecord: Boolean;
    var
        RRRecord : TRRRecord;  { V8.61 keep everything in single record }
        RDataPtr : PAnsiChar;
    begin
        Result := False;
        FillChar(RRRecord, SizeOf(RRRecord), 0);
        P := ExtractName(RespBuffer, P, RRRecord.RRName);
     { V8.64 if rrname has ACE xn--. convert it to Unicode, ignore errors }
        RRRecord.AnswerName := IcsIDNAToUnicode(String(RRRecord.RRName));
        RRRecord.RRType := IcsDnsntohs(PWORD(P)^);  { 06/03/2005 WSocket_ntohs(PWORD(P)^); }
      // ignore if SOA response to different question
        Inc(P, 2);
        RRRecord.RRClass := IcsDnsntohs(PWORD(P)^);  { 06/03/2005 WSocket_ntohs(PWORD(P)^); }
        Inc(P, 2);
        RRRecord.TTL := IcsDnsntohl(PDWORD(P)^); { 06/03/2005 WSocket_ntohl(PDWORD(P)^); }
        Inc(P, 4);
        RRRecord.RDLength := IcsDnsntohs(PWORD(P)^);  { 06/03/2005 WSocket_ntohs(PWORD(P)^) };
        Inc(P, 2);
        RDataPtr := P;
        P := P + RRRecord.RDLength;
        if (RRRecord.RRType = DnsQuerySOA) and (FQuestionType <> DnsQuerySOA) then Exit;
        if FAnsTot >= MAX_ANCOUNT then
            Exit;  // sanity test, too many results

     // keep backward compatible vy filling old arrays
        FAnswerNameArray[FAnsTot] := RRRecord.AnswerName;   { V8.64 }
        FAnswerTypeArray[FAnsTot] := RRRecord.RRType;
        FAnswerClassArray[FAnsTot] := RRRecord.RRClass;
        FAnswerTTLArray[FAnsTot] := RRRecord.TTL;
        FAnswerTagArray[FAnsTot] := -1;

        case RRRecord.RRType of
            DnsQueryMX:  begin
                    if FMXRecordCount <= High(FMXPreferenceArray) then begin
                        FAnswerTagArray[FAnsTot] := FMXRecordCount;
                        RRRecord.MxPref := WSocket_ntohs(PWORD(RDataPtr)^);
                        FMXPreferenceArray[FMXRecordCount] := RRRecord.MxPref;
                        Inc(RDataPtr, 2);
                        ExtractName(RespBuffer, RDataPtr, RRRecord.RDData);
                    { V8.64 if result has ACE xn--. convert it to Unicode, ignore errors }
                        RRRecord.HostName := IcsIDNAToUnicode(String(RRRecord.RDData));
                        FMXExchangeArray[FMXRecordCount] := RRRecord.HostName;
                        Inc(FMXRecordCount);
                    end;
            end;
            DnsQueryA: begin
                    if FARecordCount <= High(FAddressArray) then begin
                        FAnswerTagArray[FAnsTot] := FARecordCount;
                        RRRecord.IPv4.S_addr := Integer(PDWORD(RDataPtr)^);   { 06/03/2005 added cast }
                        FAddressArray[FARecordCount].S_addr := RRRecord.IPv4.S_addr;
                        RRRecord.RDData := WSocket_inet_ntoa(RRRecord.IPv4);
                        Inc(FARecordCount);
                    end;
            end;
            DnsQueryPTR, DnsQueryNS, DnsQueryCNAME: begin                    { V8.65 NS and CNAME have host response }
                    if FPTRRecordCount <= High(FHostnameArray) then begin
                        FAnswerTagArray[FAnsTot] := FPTRRecordCount;
                        ExtractName(RespBuffer, RDataPtr, RRRecord.RDData);   { V8.65 may be comprsseed data }

                    { V8.64 if result has ACE xn--. convert it to Unicode, ignore errors }
                        RRRecord.HostName := IcsIDNAToUnicode(String(RRRecord.RDData));
                        FHostnameArray[FPTRRecordCount] := RRRecord.HostName;
                        Inc(FPTRRecordCount);
                    end;
            end;
            { !!KAP!! }
            DnsQueryLOC: begin
                    { for security reasons, if recompiled with future versions of delphi alink.net return LOC!!  }
                    if (RRRecord.RDLength = 16) and (RRRecord.RDLength = sizeof(fLOCInfo)) then begin
                        Move(RDataPtr^, fLOCInfo, 16);
                        RRRecord.LocDecode := Loc2Geo(fLOCInfo);
                        RRRecord.RDData := AnsiString('Lat: ' + IntToStr(RRRecord.LocDecode.lad) + #$B0 +
                                         IntToStr(RRRecord.LocDecode.lam) + '''' +
                                         IntToStr(RRRecord.LocDecode.las) + '"' +
                                         ', Long: ' + IntToStr(RRRecord.LocDecode.lod) + #$B0 +
                                         IntToStr(RRRecord.LocDecode.lom) + '''' +
                                         IntToStr(RRRecord.LocDecode.los) + '"' +
                                         ', Alt: ' + IntToStr(RRRecord.LocDecode.altitude));
                    end
                    else
                        FillChar(fLOCInfo, SizeOf(fLOCInfo), 0);
            end;
            DnsQueryAAAA: begin
                    if FAAAARecordCount <= High(FAddress6Array) then begin       { V8.71 }
                        FAnswerTagArray[FAnsTot] := FAAAARecordCount;            { V8.71 }
                        Move(RDataPtr^, RRRecord.IPv6, sizeof(TIcsIPv6Address)); { V8.71 }
                        Move(RDataPtr^, FAddress6Array[FAAAARecordCount], sizeof(TIcsIPv6Address));  { V8.71 }
                        RRRecord.RDData := AnsiString(WSocketIPv6ToStr (RRRecord.IPv6));  // April 2013
                        Inc(FAAAARecordCount);
                    end;
            end;
            DnsQuerySOA: begin
                   RDataPtr := ExtractName(RespBuffer, RDataPtr, RRRecord.SOA.mname);
                   RDataPtr := ExtractName(RespBuffer, RDataPtr, RRRecord.SOA.rname);
{$R-} { range checking off }
                   RRRecord.SOA.serial := WSocket_ntohl(PDWORD(RDataPtr)^);
                   Inc(RDataPtr, 4);
                   RRRecord.SOA.refresh := WSocket_ntohl(PDWORD(RDataPtr)^);
                   Inc(RDataPtr, 4);
                   RRRecord.SOA.retry := WSocket_ntohl(PDWORD(RDataPtr)^);
                   Inc(RDataPtr, 4);
                   RRRecord.SOA.expire := WSocket_ntohl(PDWORD(RDataPtr)^);
                   Inc(RDataPtr, 4);
                   RRRecord.SOA.minimum := WSocket_ntohl(PDWORD(RDataPtr)^);
                   RRRecord.RDData := AnsiString('name: ' + String(RRRecord.SOA.mname) +
                                    ', email: ' + String(RRRecord.SOA.rname) +
                                    ', serial: ' + IntToStr(RRRecord.SOA.serial) +
                                    ', refresh: ' + IntToStr(RRRecord.SOA.refresh) +
                                    ', retry: ' + IntToStr(RRRecord.SOA.retry) +
                                    ', expire: ' + IntToStr(RRRecord.SOA.expire) +
                                    ', default TTL: ' + IntToStr(RRRecord.SOA.minimum));
            end;

        // pending, DNSSEC buffers contain several fields, should handle them properly
        // so tempoarily return them as hex
            DnsQueryRRSIG, DnsQueryDNSKEY, DnsQueryDS, DnsQueryNSEC, DnsQueryNSEC3,
              DnsQueryCDS, DnsQueryCDNSKEY, DnsQueryTLSA, DnsQuerySMIMEA: begin
                 SetLength(Temp, RRRecord.RDLength);   { V8.64 }
                 Move(RDataPtr^ , Temp[1], RRRecord.RDLength);
                 RRRecord.RDData := AnsiString(IcsBufferToHex(Temp, RRRecord.RDLength));
            end
            else begin   // assume all other records are textual, TXT, etc, without compression
            //    ExtractName(RespBuffer, RDataPtr, RRRecord.RDData);  failed if multiple TXT responses
                 SetLength(Temp, RRRecord.RDLength - 1);   { V8.64 }
                 Inc(RDataPtr, 1);  // skip length byte
                 Move(RDataPtr^ , Temp[1], RRRecord.RDLength - 1);
                 RRRecord.RDData := Temp;  // us ascii conversion
             { V8.71 keep TXT, used by Lets Encrypt }
                 if (RRRecord.RRType = DnsQueryTXT) and (FTXTRecordCount <= High(FTXTRecordArray)) then begin
                    FTXTRecordArray[FTXTRecordCount] := String(Temp);
                    inc(FTXTRecordCount);
                 end;
            end;
        end;
        FAnswerRecordArray[FAnsTot] := RRRecord;
        Result := True;
    end;

begin
    Result := False;
    { Check for minimum response length }
    if BufLen < SizeOf(TDnsRequestHeader) then
        Exit;
   AnsPtr := PDnsRequestHeader(RespBuffer);
   Flags := WSocket_ntohs(AnsPtr^.Flags);
    { Check if we got a response }
    if (Flags and $8000) = 0 then
        Exit;
    FResponseLen := BufLen;

    { Decode response header }
    FResponseID                 := WSocket_ntohs(AnsPtr^.ID);
    FResponseCode               := Flags and $000F;
//  fDnsRequestAnswer.qr        := (Flags and $8000) = $8000;
    FResponseOpCode             := (Flags shr 11) and $000F;
    FResponseAuthoritative      := (Flags and $0400) = $0400;
    FResponseTruncation         := (Flags and $0200) = $0200;
//  fDnsRequestAnswer.RecursionDesired := (Flags and $0100) = $0100;
    FResponseRecursionAvailable := (Flags and $0080) = $0080;
//  fDnsRequestAnswer.z         := (Flags shr 4) and $0007;
//  fDnsRequestAnswer.rcode     := (Flags and $000F);
    FResponseQDCount            := WSocket_ntohs(AnsPtr^.QDCount);
    FResponseANCount            := WSocket_ntohs(AnsPtr^.ANCount);
    FResponseNSCount            := WSocket_ntohs(AnsPtr^.NSCount);
    FResponseARCount            := WSocket_ntohs(AnsPtr^.ARCount);

    P := RespBuffer + SizeOf(TDnsRequestHeader);
    PEnd := RespBuffer + FResponseLen;
    PEnd^ := #0;  // V8.84 null at end of buffer
    if FResponseQDCount = 0 then begin
        { I don't think we could receive 0 questions }
        FQuestionName  := '';
        FQuestionType  := 0;
        FQuestionClass := 0;
    end
    else begin
        { Should never be greater than 1 because we sent only one question }
        P := ExtractName(RespBuffer, P, Temp);
        FQuestionName := String(Temp);              { V8.64 }
        FQuestionType := WSocket_ntohs(PWORD(P)^);
        Inc(P, 2);
        FQuestionClass := WSocket_ntohs(PWORD(P)^);
        Inc(P, 2);
    end;

 {   FMXRecordCount  := 0;   V8.71 don't reset any here
    FARecordCount   := 0;
    FPTRRecordCount := 0;   }
  // note we don't reset FAnsTot here to collect answers from multiple queries

 // read all answers
    while PEnd > P do begin
        if ProcessRespRecord then begin
          // special case, Cloudfare add empty record which we ignore
            if (FAnswerRecordArray[FAnsTot].RRType <> DnsQueryOPT) or
                            (FAnswerRecordArray[FAnsTot].RRName <> '') then
                                  FAnsTot := FAnsTot + 1;
         end;
    end;

// see if further processing needed
// V8.71 build new MX array in preference order
    if FMXRecordCount <> 0 then begin
        MXList := TStringList.Create ;
        try
            for I := 0 to FMXRecordCount - 1 do
               MXList.Add (IntToStr (FMXPreferenceArray [I] + 1000) + '=' + FMXExchangeArray [I]) ;
            MXList.Sort ;  // sort into preference order
            for I := 0 to FMXRecordCount - 1 do begin
                J := Pos ('=', MXList [I]) ;
                FMXSortedExchArray[I] := Copy (MXList [I], J + 1, 99) ;
            end;
        finally
            MXList.Free ;
        end ;
    end;
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.ExtractName(
    Base       : PAnsiChar;
    From       : PAnsiChar;
    var Name   : AnsiString) : PAnsiChar;
var
    N       : Integer;
    I       : Integer;
    P       : PAnsiChar;
    NameEnd : AnsiString;
begin
    P := From;
    if P^ = #0 then begin
        Name := '';
        Inc(P);
    end
    else begin
        Name := '';
        while TRUE do begin
            { Get name part length }
            N := Ord(P^);
            if (N and $C0) = $C0 then begin
                 { Message compression }
                 N := ((N and $3F) shl 8) + Ord(P[1]);
                 if Length(Name) = 0 then
                     Self.ExtractName(Base, Base + N, Name)
                 else begin
                     Self.ExtractName(Base, Base + N, NameEnd);
                     Name := Name + NameEnd;
                 end;
                 Inc(P, 2);
                 break;
            end;
            Inc(P);
            if N = 0 then
                break;
            { Copy name part }
            for I := 1 to N do begin
                Name := Name + P^;
                Inc(P);
            end;
            if P^ <> #0 then
                Name := Name + '.';
        end;
    end;
    Result := P;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{
  <0><1><129><128><0><1><0><1><0><4><0><5><7>inp
  rise<3>com<0><0><15><0><1><192><12><0>
  <15><0><1><0><1>QV<0><10><0><10><5>drui
  d<192><12><192><12><0><2><0><1><0><1>Qc<0><6><3>
  ns1<192><12><192><12><0><2><0><1><0><1>Qc<0>
  <20><3>NS1<10>SPRINTLINK
  <3>NET<0><192><12><0><2><0><1><0><1>Qc<0>
  <6><3>NS2<192>U<192><12><0><2><0><1><0><1>Q
  c<0><6><3>NS3<192>U<192>+<0><1><0><1><0>
  <1>QV<0><4><143><186><11>F<192>?<0><1><0><1><0>
  <1>Qc<0><4><207>iS<30><192>Q<0><1><0><1><0>
  <2><144>i<0><4><204>u<214><10><192>q<0><1><0><1><0>
  <2><144>i<0><4><199><2><252><10><192><131><0><1><0><1><0>
  <2><142><182><0><4><204>a<212><10>
}
{
  <0><3><129><128><0><1><0><1><0><2><0><3><4>rtf
  m<2>be<0><0><15><0><1><192><12><0><15><0><1><0>
  <1>.b<0><9><0><10><4>mail<192><12><192><12>
  <0><2><0><1><0><1>.b<0><11><2>ns<3>dn
  s<2>be<0><192><12><0><2><0><1><0><1>.b<0>
  <5><2>ns<192><12><192>'<0><1><0><1><0><1>.b
  <0><4><195><0>d<253><192>:<0><1><0><1><0><1>QY
  <0><4><134>:J!<192>Q<0><1><0><1><0><1>.b
  <0><4><195><0>d<253>
}
{
  <0><7><133><128><0><1><0><1><0><2><0><2><3>www
  <4>rtfm<2>be<0><0><1><0><1><192><12><0>
  <1><0><1><0><1>Q<128><0><4><195><0>d<253><4>rt
  fm<2>be<0><0><2><0><1><0><1>Q<128><0><5>
  <2>ns<192>-<192>-<0><2><0><1><0><1>Q<128><0>
  <9><2>ns<3>dns<192>2<192>@<0><1><0><1>
  <0><1>Q<128><0><4><195><0>d<253><192>Q<0><1><0><1>
  <0><0><26><132><0><4><134>:J!
}
(*
<0><1><129><128><0><1><0><1><0><5><0><5><9>fu-berlin
<2>de<0><0>

<29><0><1><192><12><0><29><0><1><0><0>,

<0><16><0><21><22><19><139>Av<167><130><218>L<242>
<0><152><156>\<192><12><0><2><0><1><0><0><12><176>
<0>"<4>arbi<10>informatik<13>uni-oldenburg<2>de<0>
<192><12><0><2><0><1><0><0><12><176><0><12><5>deneb<3>
dfn<192>d<192><12><0><2><0><1><0><0><12><176><0><6><3>
ns3<192><12><192><12><0><2><0><1><0><0><12><176><0><6>
<3>ns2<192><12><192><12><0><2><0><1><0><0><12><176><0>
<6><3>ns1<192><12><192>F<0><1><0><1><0><0>t<169><0><4>
<134>j<1><7><192>t<0><1><0><1><0><0>9<209><0><4><192>L
<176><9><192><140><0><1><0><1><0><0>T<19><0><4><130>
<133><1>9<192><158><0><1><0><1><0><0><28><206><0><4>
<160>-<10><12><192><176><0><1><0><1><0><0>1<198><0>
<4><160>-<8><8>
*)

{ !!KAP!! }
{raw translation of some perl-source LOC.pm from package Net::DNS::RR::LOC;

fu-berlin.de   LOC  52 27 19.591 N 13 17 40.978 E 15.00m 1000.00m 10000.00m 10.00m
}
const conv_sec = 1000.0;
      conv_min = 60.0 * conv_sec;
      conv_deg = 60.0 * conv_min;
      zh31     = 1 shl 31;

procedure SubLOCgeo(longlat : longint;
                    hemis : AnsiString;
                    var ldeg, lmin, lsec, lmsec : Extended;
                    var hemic : AnsiChar);
var
    Labs : Extended;
begin
    LongLat := WSocket_ntohl(LongLat);
    Labs    := Abs(1.0 * LongLat - zh31);
    Ldeg    := Trunc(labs / conv_deg);
    Labs    := Labs - ldeg * conv_deg;
    Lmin    := Trunc(labs / conv_min);
    Labs    := Labs - lmin * conv_min;
    Lsec    := Trunc(labs / conv_sec);
    Labs    := Labs - lsec * conv_sec;
    Lmsec   := Labs;
    Hemic   := Copy(Hemis, 1 + ord(LongLat <= zh31), 1)[1]; { yeah. }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LongLatToDMS(longlat : longint; hemis : AnsiString): AnsiString;
Var ldeg, lmin, lsec, lmsec : extended;
    hemi                    : AnsiChar;
begin
  SubLOCgeo(longlat,hemis,ldeg,lmin,lsec,lmsec,hemi);
  result := AnsiString(Format('%d %02d %02d.%03d',
               [round(ldeg), round(lmin), round(lsec),
                round(lmsec)]) + ' ' + Char(hemi));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ in cm!! }
function LocAltToAlt(Localt : LongInt) : LongInt;
begin
    Result := Round((WSocket_ntohl(localt) - 100000.0 * 100.0) / 100.0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ !!KAP!! }
function Loc2Geo(loc : TLOCInfo):TLogGeo;
  { dolle umwandlung }
  procedure du(longlat : Integer;
               hemis   : AnsiString;
               var ideg, imin, isec, imsec : Integer;
               var hemic : AnsiChar);
  var
      ldeg, lmin, lsec, lmsec : extended;
  begin
      SubLOCgeo(longlat, hemis, ldeg, lmin, lsec, lmsec, hemic);
      ideg  := Round(ldeg);
      imin  := Round(lmin);
      isec  := Round(lsec);
      imsec := Round(lmsec);
  end;

begin
    Result.version  := Loc.version;
    Result.longsize := Round(Exp(Ln(10)*(loc.size and $f)));
    Result.latsize  := Round(Exp(Ln(10)*(loc.size shr 4)));

    Result.horizpre := Loc.horizpre;
    Result.vertpre  := Loc.vertpre;

    du(loc.latitude, 'NS', result.lad, result.lam,
       result.las, result.lams, result.lahem);
    du(loc.longitude, 'EW', result.lod, result.lom,
       result.los, result.loms, result.lohem);

    Result.altitude := LocAltToAlt(loc.altitude);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
{ lookup up NetBios host name on LAN for an IPv4 address, Windows only }
{ seems to ignore AF_NETBIOS and do DNS as well }
{ this is a blocking function, it won't return for several seconds if IP not found }
function GetNetBiosHostByAddr(IPv4: String): String;
var
    Phe: PHostEnt;
    lAddr: TIcsIPv4Address;
    Success: Boolean;
    AStr: AnsiString;
begin
    Result := '';
    lAddr := WSocketStrToIPv4(IPv4, Success);
    if NOT Success then
        Exit;
    Phe := WSocket_gethostbyaddr(PAnsiChar(@lAddr), 4, AF_NETBIOS);
    if Phe <> nil then begin
        SetLength(AStr, StrLen(Phe^.h_name));
        StrCopy(@AStr[1], Phe^.h_name);
        Result := String(AStr);
        if Phe^.h_aliases <> Nil then begin
         //
        end;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.71 TIcsDomainNameCache }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsDomainNameCache.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    AllocateHWnd;
    SetLength(FDNItems, 32);
    FTotDNItems := 0;
    FMaxLookups := 5;
    FQTimeout := 5;
    FDNMethod := MethodWinsock;
    FDBLANlookup := LanLookDef;
    FDnsServerStrat := SrvStratOne;
    FDefTTL := 3600;
    FDNIndex := TIcsFindList.Create;
    FDNIndex.Sorted := True;
    FLookupQu := TIcsIntegerList.Create;
    FDefHostList := TStringList.Create;
    FDnsServerList := TStringList.Create;
    FMaintTimer := TIcsTimer.Create(Self);
    FMaintTimer.OnTimer := MaintTimerOnTimer;
    FMaintTimer.Enabled := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsDomainNameCache.Destroy;
begin
    if Length(FDNWSockets) > 0 then
        CancelLookups;
    SetLength(FDNWSockets, 0);
    SetLength(FDnsQuerys, 0);
    SetLength(FCntlLookups, 0);
    SetLength(FDNItems, 0);
    FDNIndex.Free;
    FDnsServerList.Free;
    FLookupQu.Free;
    FDefHostList.Free;
    Inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.SetDNMethod(Value: TDNMethod);
begin
    if FDNMethod <> Value then begin
        FDNMethod := Value;
        SetLength(FCntlLookups, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.DeleteRec(const Request: String): Boolean;
begin
    Result := False;
 // pending
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ adds forward and reverse entries to the cache for well known IP addresses }
procedure TIcsDomainNameCache.AddtoCache(const IP, Host: String);
var
    NewDNItem: TDNItem;
    ItemNr: Integer;
begin
    if NOT WSocketIsIP(IP, NewDNItem.ReqFamily) then
        Exit;

  // add or update forward lookup entry for Host
    NewDNItem.DNState := StateNone;
    NewDNItem.ReqTag := 0;
    NewDNItem.ReqUpdEvent := Nil;
    NewDNItem.TimeStamp := 0;
    NewDNItem.TTL := SecsPerDay*365;
    NewDNItem.TotResp4 := 0;
    NewDNItem.TotResp6 := 0;
    ItemNr := FindRequest(Host);
    if ItemNr < 0 then begin         // new record
        NewDNItem.Request := Host;
        NewDNItem.DNReqType := ReqTypeDnsForw;
        if NewDNItem.ReqFamily <> sfIPv6 then begin
            NewDNItem.TotResp4 := 1;
            SetLength(NewDNItem.Responses4, 1);
            NewDNItem.Responses4[0] := IP;
        end
        else begin
            NewDNItem.TotResp6 := 1;
            SetLength(NewDNItem.Responses6, 1);
            NewDNItem.Responses6[0] := IP;
        end;
        ItemNr := AddNewRec(NewDNItem);
    end
    else begin                      // update old record, assume only one IP
        if NewDNItem.ReqFamily <> sfIPv6 then begin
            FDNItems[ItemNr].TotResp4 := 1;
            SetLength(FDNItems[ItemNr].Responses4, 1);
            FDNItems[ItemNr].Responses4[0] := IP;
        end
        else begin
            FDNItems[ItemNr].TotResp6 := 1;
            SetLength(FDNItems[ItemNr].Responses6, 1);
            FDNItems[ItemNr].Responses6[0] := IP;
        end;
    end;
    FDNItems[ItemNr].DNState := StateOK;

 // add reverse lookup entry for IP
    ItemNr := FindRequest(IP);
    if ItemNr > 0 then       // duplicate
        Exit;
    NewDNItem.Request := IP;
    NewDNItem.DNReqType := ReqTypeDnsBack;
    NewDNItem.ReqFamily := sfAny;
    NewDNItem.TotResp4 := 0;
    SetLength(NewDNItem.Responses4, 0);
    NewDNItem.TotResp6 := 1;
    SetLength(NewDNItem.Responses6, 1);
    NewDNItem.Responses6[0] := Host;
    ItemNr := AddNewRec(NewDNItem);
    FDNItems[ItemNr].DNState := StateOK;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{
# This is a sample HOSTS file used by Microsoft TCP/IP for Windows.
127.0.0.1      localhost
::1            localhost #[IPv6]
::1            localhost6 #[IPv6]   }
procedure TIcsDomainNameCache.AddtoCache(const IPandHost: String);
var
    S, IP, Host: String;
    I: Integer;
begin
    S := Trim(IPandHost);
    if (Length(S) <  6) then
        Exit;
    if (S[1] = '#') then   // ignore comment line
        Exit;
    I := Pos(IcsSPACE, S);
    if I <= 3 then
        Exit;
    IP := Trim(Copy(S, 1, I - 1));   // don't check if it's an IP yet
    Host := Trim(Copy(S, I + 1, 999));
    I := Pos('#', S);   // trailing comment
    if I > 0 then
        SetLength(Host, I - 1);
    AddtoCache(IP, Host);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.AddListtoCache(HostList: TStrings);
var
    I: Integer;
begin
    if NOT Assigned(HostList) then
        Exit;
    if HostList.Count = 0 then
        Exit;
    for I := 0 to HostList.Count - 1 do
        AddtoCache(HostList[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.SetMaxLookups(Value: Integer);
begin
    if Value > FMaxLookups then
        FMaxLookups := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.LogEvent(const Info: String);
begin
    if Assigned(FDNLogEvent) then begin
        FDNLogEvent(Self, Info);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.DNLogEvent(Sender: TObject; const Msg: string);
begin
    LogEvent(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.DnsLogEvent(Sender: TObject; const Msg: string);
begin
    LogEvent(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.TriggerUpdate(ItemNr: Integer);
begin
    if (ItemNr < 0) or (ItemNr > FTotDNItems) then
        Exit;
    if Assigned(FDNItems[ItemNr].ReqUpdEvent) then
        FDNItems[ItemNr].ReqUpdEvent(Self, ItemNr)
    else if Assigned(FDNUpdateEvent) then begin
        FDNUpdateEvent(Self, ItemNr);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.WsockDnsRequestDone(Sender: TObject; Error: Word);
var
    MySocket: TWSocket ;
    SocNr, ItemNr, I, TotResp: Integer;
    IP: String;
begin
    if (FTotDNItems = 0) or (FDNIndex.Count = 0) then
        Exit;
    MySocket := Sender as TWSocket ;
    SocNr := MySocket.Tag;
    if (SocNr >= 1) and (SocNr <= FMaxLookups) then begin
        FCntlLookups[SocNr].Busy := False;
        ItemNr := FCntlLookups[SocNr].ItemNr;

    // keep DNS result in main array, tell application
        if (ItemNr >= 0) and  (ItemNr < FTotDNItems) then begin
            if Error <> 0 then begin
                FDNItems[ItemNr].DNState := StateFailed;
                LogEvent('WSock-' + IntToStr(SocNr) + ' Look-up failed for ' +  FDNItems[ItemNr].Request);
            end
            else begin
                FDNItems[ItemNr].DNState := StateOK;
                TotResp := MySocket.DnsResultList.Count;

            // keep IPv4 and IPv6 addresses separately, easier for applications
                FDNItems[ItemNr].TotResp4 := 0;
                FDNItems[ItemNr].TotResp6 := 0;
                SetLength(FDNItems[ItemNr].Responses4, TotResp);
                SetLength(FDNItems[ItemNr].Responses6, TotResp);
                if TotResp > 0 then begin
                    for I := 0 to TotResp - 1 do begin
                        IP := MySocket.DnsResultList[I];
                        if WSocketIsIPv4(IP) then begin
                            FDNItems[ItemNr].Responses4[FDNItems[ItemNr].TotResp4] := IP;
                            FDNItems[ItemNr].TotResp4 := FDNItems[ItemNr].TotResp4 + 1;
                        end
                        else begin
                            FDNItems[ItemNr].Responses6[FDNItems[ItemNr].TotResp6] := IP;
                            FDNItems[ItemNr].TotResp6 := FDNItems[ItemNr].TotResp6 + 1;
                        end;
                    end;
                    SetLength(FDNItems[ItemNr].Responses4, FDNItems[ItemNr].TotResp4);
                    SetLength(FDNItems[ItemNr].Responses6, FDNItems[ItemNr].TotResp6);
                end;
                FDNItems[ItemNr].TTL := FDefTTL; // don't have real TTL
                LogEvent('WSock-' + IntToStr(SocNr) + ' Look-up OK - ' + MySocket.DnsResultList.CommaText + ' for ' +  FDNItems[ItemNr].Request);
            end;
            TriggerUpdate(ItemNr);  // tell application we're done
        end;

    // see if any pending requests, start one
        StartPendingRequest;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ only used for NetBios name requests }
procedure TIcsDomainNameCache.WsockDataAvailable(Sender: TObject; Error: WORD);
var
    MySocket: TWSocket;
    SocNr, ItemNr, Len: Integer;
    NetBiosResp: TBytes;
    RemName, RemWorkgroup: String;
begin
    if (FTotDNItems = 0) or (FDNIndex.Count = 0) then
        Exit;
    MySocket := Sender as TWSocket ;
    SocNr := MySocket.Tag;
    if (SocNr >= 1) and (SocNr <= FMaxLookups) then begin
        FCntlLookups[SocNr].Busy := False;
        ItemNr := FCntlLookups[SocNr].ItemNr;
        FDNItems[ItemNr].TotResp4 := 0;
        FDNItems[ItemNr].TotResp6 := 0;
        NetBiosResp := MySocket.ReceiveTB;   // read UDP data
        Len := Length(NetBiosResp);
        if Len > 90 then begin
            IcsMoveTBytesToString(NetBiosResp, 57, RemName, 1, 15);
            RemName := Trim(RemName);
            IcsMoveTBytesToString(NetBiosResp, 75, RemWorkgroup, 1, 15);
            RemWorkgroup := Trim(RemWorkgroup);
        end;
      {  if Len > 0 then
            LogEvent('WSock-' + IntToStr(SocNr) + ' NetBIOS Response Length=' + IntToStr(Len) + IcsCRLF +
                                                                          IcsBufferToHex(NetBiosResp[1], Len))
        else
           LogEvent('WSock-' + IntToStr(SocNr) + ' NetBIOS No Response Data');   }
        if RemName <> '' then begin
            FDNItems[ItemNr].TotResp6 := 1;
            SetLength(FDNItems[ItemNr].Responses6, 1);
            FDNItems[ItemNr].Responses6[0] := RemName;
            FDNItems[ItemNr].DNState := StateOK;
            FDNItems[ItemNr].TTL := FDefTTL; // don't have real TTL
        end
        else
            FDNItems[ItemNr].DNState := StateFailed;
        MySocket.Close;
        TriggerUpdate(ItemNr);  // tell application we're done

    // see if any pending requests, start one
        StartPendingRequest;
   end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ only used for NetBios name requests }
procedure TIcsDomainNameCache.WsockSessionConnected(Sender: TObject; Error: WORD);
var
    MySocket: TWSocket ;
    SocNr, ItemNr: Integer;
    SocRemAddr: TSockAddrIn;
begin
    if (FTotDNItems = 0) or (FDNIndex.Count = 0) then
        Exit;
    MySocket := Sender as TWSocket ;
    SocNr := MySocket.Tag;
    if (SocNr >= 1) and (SocNr <= FMaxLookups) then begin
        ItemNr := FCntlLookups[SocNr].ItemNr;
        if Error <> 0 then begin
            FCntlLookups[SocNr].Busy := False;
            FDNItems[ItemNr].DNState := StateFailed;
            LogEvent('WSock-' + IntToStr(SocNr) + ' NetBios Connect Failed for ' + FDNItems[ItemNr].Request);
        end
        else begin
            LogEvent('WSock-' + IntToStr(SocNr) + ' NetBios Connected OK for ' + FDNItems[ItemNr].Request);
            SocRemAddr.sin_family := AF_INET;
            SocRemAddr.sin_addr.S_addr := WSocket_inet_addr (AnsiString (FDNItems[ItemNr].Request));
            SocRemAddr.sin_port := WSocket_htons (137);
            FDNWSockets[SocNr].SendTo(SocRemAddr, SizeOf (TSockAddrIn), @NetBiosNameReq, Length(NetBiosNameReq));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.StartWSocket(ItemNr: Integer): Boolean;
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
          // check socket not busy with last request
            if (I < Length(FDNWSockets)) and Assigned(FDNWSockets[I]) and
                                     (FDNWSockets[I].State in [wsDnsLookup, wsConnecting, wsConnected]) then
                Continue;
            Socnr := I ;
            Break;
        end;
    end;

// all sockets busy, add to lookup queue
    if SocNr <= 0 then begin
        FLookupQu.Add(ItemNr);
        LogEvent('WSockets all Busy, Request Queued for ' + FDNItems[ItemNr].Request);
        Exit;
    end;
    FDNItems[ItemNr].DNState := StateWaiting;
    FDNItems[ItemNr].TimeStamp := Now;
    FDNItems[ItemNr].TTL := 60*60; // one hour retry after failure
    FCntlLookups[SocNr].Busy := True;
    FCntlLookups[SocNr].ItemNr := ItemNr;
    FCntlLookups[SocNr].Request := FDNItems[ItemNr].Request;
    FCntlLookups[SocNr].StartTick := IcsGetTickCount64;
    try
        if Length(FDNWSockets) <= SocNr then begin
            SetLength(FDNWSockets, SocNr + 5);   // only create a max five spare sockets at a time
            for I := Socnr to Length(FDNWSockets) do
                FDNWSockets[I] := Nil;
        end;
        if NOT Assigned(FDNWSockets[SocNr]) then begin
            FDNWSockets[SocNr] := TWSocket.Create(Self);
            FDNWSockets[SocNr].ComponentOptions := FDNWSockets[SocNr].ComponentOptions + [wsoIcsDnsLookup];
            FDNWSockets[SocNr].OnDnsLookupDone := WsockDnsRequestDone;
            FDNWSockets[SocNr].OnDataAvailable := WsockDataAvailable;
            FDNWSockets[SocNr].OnSessionConnected := WsockSessionConnected;
            FDNWSockets[SocNr].Tag := SocNr;
        end;
        if FDNWSockets[SocNr].SocketFamily <> FDNItems[ItemNr].ReqFamily then
            FDNWSockets[SocNr].SocketFamily := FDNItems[ItemNr].ReqFamily;
        if (FDNItems[ItemNr].DNReqType = ReqTypeDnsBack) then
            FDNWSockets[SocNr].ReverseDnsLookup (FDNItems[ItemNr].Request)
        else if (FDNItems[ItemNr].DNReqType = ReqTypeDnsForw) then
            FDNWSockets[SocNr].DnsLookup (FDNItems[ItemNr].Request)
        else if (FDNItems[ItemNr].DNReqType = ReqTypeNetBios) then begin    // NETBIOS IPv4 LAN only
            if FDNWSockets[SocNr].State <> wsClosed then
                FDNWSockets[SocNr].Abort;
            FDNWSockets[SocNr].Proto := 'udp';
            FDNWSockets[SocNr].Addr := FDNItems[ItemNr].Request;
            FDNWSockets[SocNr].Port := '137';  // NetBIOS
            FDNWSockets[SocNr].Connect;
        end
        else
            Exit;
        LogEvent('WSock-' + IntToStr(SocNr) + ' Started Look-up for ' + FDNItems[ItemNr].Request);
        Result := True;
    except
        LogEvent('Exception Starting DNS Lookup - ' + IcsGetExceptMess (ExceptObject));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.DnsQueryRequestDone(Sender: TObject; Error: Word);
var
    MyDnsQuery: TDnsQuery;
    SocNr, ItemNr, I, TotResp: Integer;
    Reply, Replies: String;
begin
    if (FTotDNItems = 0) or (FDNIndex.Count = 0) then
        Exit;
    MyDnsQuery := Sender as TDnsQuery;
    SocNr := MyDnsQuery.Tag;
    if (SocNr >= 1) and (SocNr <= FMaxLookups) then begin
        FCntlLookups[SocNr].Busy := False;
        ItemNr := FCntlLookups[SocNr].ItemNr;

    // keep DNS result in main array, tell application
        if (ItemNr >= 0) and  (ItemNr < FTotDNItems) then begin
            if (Error <> 0) or (MyDnsQuery.FAnsTot = 0) then begin
                FDNItems[ItemNr].DNState := StateFailed;
                LogEvent('DnsQuery-' + IntToStr(SocNr) + ' Look-up failed for ' +  FDNItems[ItemNr].Request);
            end
            else begin
                FDNItems[ItemNr].DNState := StateOK;
                Replies := '';

            // keep IPv4 and IPv6 addresses separately, easier for applications
                if FDNItems[ItemNr].DNReqType = ReqTypeDnsForw then begin
                    TotResp := MyDnsQuery.ARecordCount;
                    if (TotResp > 0) then begin
                        FDNItems[ItemNr].TotResp4 := 0;
                        SetLength(FDNItems[ItemNr].Responses4, TotResp);
                        for I := 0 to TotResp - 1 do begin
                            Reply := WSocketIPv4ToStr(Integer(MyDnsQuery.Address[I]));
                            FDNItems[ItemNr].Responses4[FDNItems[ItemNr].TotResp4] := Reply;
                            FDNItems[ItemNr].TotResp4 := FDNItems[ItemNr].TotResp4 + 1;
                            Replies := Replies + Reply + ', ';
                        end;
                        SetLength(FDNItems[ItemNr].Responses4, FDNItems[ItemNr].TotResp4);
                    end;
                end;
                if (FDNItems[ItemNr].DNReqType = ReqTypeDnsForw) then
                    TotResp := MyDnsQuery.AAAARecordCount
                else
                    TotResp := MyDnsQuery.PTRRecordCount;
                if (TotResp > 0) then begin
                    FDNItems[ItemNr].TotResp6 := 0;
                    SetLength(FDNItems[ItemNr].Responses6, TotResp);
                    for I := 0 to TotResp - 1 do begin
                        if (FDNItems[ItemNr].DNReqType = ReqTypeDnsForw) then
                            Reply := WSocketIPv6ToStr(MyDnsQuery.Address6[I])
                        else
                            Reply := MyDnsQuery.Hostname[I];
                        FDNItems[ItemNr].Responses6[FDNItems[ItemNr].TotResp6] := Reply;
                        Replies := Replies + Reply + ', ';
                        FDNItems[ItemNr].TotResp6 := FDNItems[ItemNr].TotResp6 + 1;
                    end;
                    SetLength(FDNItems[ItemNr].Responses6, FDNItems[ItemNr].TotResp6);
                end;
                FDNItems[ItemNr].TTL := MyDnsQuery.AnswerTTL[0];
                LogEvent('DnsQuery-' + IntToStr(SocNr) + ' Look-up OK - ' + Replies + ' for ' +  FDNItems[ItemNr].Request);
            end;
            TriggerUpdate(ItemNr);  // tell application we're done
        end;

    // see if any pending requests, start one
        StartPendingRequest;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.StartDnsQuery(ItemNr: Integer): Boolean;
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
        LogEvent('DnsQueries all Busy, Request Queued for ' + FDNItems[ItemNr].Request);
        Exit;
    end;
    FDNItems[ItemNr].DNState := StateWaiting;
    FDNItems[ItemNr].TimeStamp := Now;
    FCntlLookups[SocNr].Busy := True;
    FCntlLookups[SocNr].ItemNr := ItemNr;
    FCntlLookups[SocNr].Request := FDNItems[ItemNr].Request;
    FCntlLookups[SocNr].StartTick := IcsGetTickCount64;
    try
        if Length(FDnsQuerys) <= SocNr then begin
            SetLength(FDnsQuerys, SocNr + 5);   // only create a max five spare components at a time
            for I := Socnr to Length(FDnsQuerys) do
                FDnsQuerys[I] := Nil;
        end;
        if NOT Assigned(FDnsQuerys[SocNr]) then begin
            FDnsQuerys[SocNr] := TDnsQuery.Create(Self);
            FDnsQuerys[SocNr].OnRequestDone := DnsQueryRequestDone;
            FDnsQuerys[SocNr].OnLogEvent := DnsLogEvent;
            FDnsQuerys[SocNr].Tag := SocNr;
            if FDNMethod <> MethodTcp then
              FDnsQuerys[SocNr].Proto := 'udp'
            else
                FDnsQuerys[SocNr].Proto := 'tcp';
            if FDnsServerList.Count > 0 then
                FDnsQuerys[SocNr].Addr := FDnsServerList[0]
            else
                FDnsServerStrat := SrvStratPub;
            FDnsQuerys[SocNr].ServerStrat := FDnsServerStrat;
            FDnsQuerys[SocNr].ServerList.Assign(FDnsServerList);
        end;
        if FDNItems[ItemNr].DNReqType = ReqTypeDnsBack then
            FDnsQuerys[SocNr].PTRLookup(FDNItems[ItemNr].Request)
        else begin
            if FDNItems[ItemNr].ReqFamily = sfIPv6 then
                FDnsQuerys[SocNr].AAAALookup(FDNItems[ItemNr].Request)
            else if FDNItems[ItemNr].ReqFamily = sfIPv4 then
                FDnsQuerys[SocNr].ALookup(FDNItems[ItemNr].Request)
            else
                FDnsQuerys[SocNr].QueryBothA(FDNItems[ItemNr].Request);
        end;
        LogEvent('DnsQuery-' + IntToStr(SocNr) + ' Started Look-up for ' + FDNItems[ItemNr].Request);
        Result := True;
    except
        LogEvent('Exception Starting DnsQuery - ' + IcsGetExceptMess (ExceptObject));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.StartRequest(ItemNr: Integer): Boolean;
begin
    if (FDNItems[ItemNr].DNReqType = ReqTypeNetBios) then begin
        if (FDBLANlookup = LanLookWSock) then    // normal winsock revert request type
            FDNItems[ItemNr].DNReqType := ReqTypeDnsBack;
        Result := StartWSocket(ItemNr);
    end
    else if (FDNMethod = MethodWinsock) then begin
        Result := StartWSocket(ItemNr);
    end
    else if FDNMethod in [MethodUdp, MethodTcp] then begin
        Result := StartDnsQuery(ItemNr);
    end
//    else if FDNMethod = MethodHttps then begin    // handled in derived class
//    end
    else
        Result := False;
    if NOT Result then
        LogEvent('Lookup Name Request Failed Start: ' + FDNItems[ItemNr].Request);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.StartPendingRequest;
var
    ItemNr: Integer;
begin
  // see if any pending requests, start one
    if FLookupQu.Count > 0 then begin
        ItemNr := FLookupQu.Items[0];  // top of queue
        FLookupQu.Delete(0);
        StartRequest(ItemNr);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// called by TIcsFindList for sort and find comparison of file records - case insensitive
function IcsCompareFNext(Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal
// and > 0 if Item1 is greater than Item2.
begin
    result := CompareText (PDNItem (Item1).Request, PDNItem (Item2).Request) ;  // case insensitive
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.RebuildIndex;
var
    I: Integer;
begin
    FDNIndex.Clear;
    if FTotDNItems > 0 then begin
        FDNIndex.Capacity := FTotDNItems;
        for I := 0 to FTotDNItems - 1 do begin
            if FDNItems[I].Request <> '' then
                FDNIndex.Add(@FDNItems[I]);
        end;
    end;
    FDNIndex.Sort(IcsCompareFNext);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.MaintTimerOnTimer(Sender : TObject);
var
    I, ItemNr, Duration: Integer;
begin
    if FTotDNItems = 0 then
        Exit;

 // check for expired lookups
    if (Length(FCntlLookups) >= FMaxLookups) then begin
        try
            for I := 1 to FMaxLookups do begin
                if (FCntlLookups[I].Busy) then begin
                    Duration := IcsElapsedSecs64(FCntlLookups[I].StartTick);
                    if Duration >= FQTimeout then begin
                        FCntlLookups[I].Busy := False;
                        ItemNr := FCntlLookups[I].ItemNr;
                        if (ItemNr < 0) or (ItemNr > FTotDNItems) then
                            Continue;
                        FDNItems[ItemNr].DNState := StateFailed;
                        if (FDNMethod = MethodWinsock) or (FDNItems[ItemNr].DNReqType = ReqTypeNetBios) then begin
                            LogEvent('WSock-' + IntToStr(I) + ' Aborting Request for ' + FDNItems[ItemNr].Request);
                            if FDNWSockets[I].State = wsDnsLookup then
                                FDNWSockets[I].CancelDnsLookup
                            else begin
                                FDNWSockets[I].Abort;
                                FDNWSockets[I].Close;
                            end;
                        end
                        else if FDNMethod in [MethodUdp, MethodTcp] then begin
                            LogEvent('DnsQuery-' + IntToStr(I) + ' Aborting Request for ' + FDNItems[ItemNr].Request);
                            FDnsQuerys[I].AbortQuery;
                        end;
                    // note, no checking for HTTPS, requests timeout after 30 secs or so
                        TriggerUpdate(ItemNr);  // tell application we're done

                     // see if any pending requests, start one
                        StartPendingRequest;
                    end;
                end;
            end;
         except
         end;
    end;

// check for expired records

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ adds new record or replaces old record, sets StateNone }
{ may have request and responses already set }
function TIcsDomainNameCache.AddNewRec(DNItem: TDNItem): Integer;
var
    ItemNr, Index: Integer;
begin
    Result := -1;
    if DNItem.Request = '' then
        Exit;
  // see if replacing existing record
    Index := -1;
    if (FDNIndex.Find(@DNItem.Request, IcsCompareFNext, Index)) then begin
        ItemNr := PDNItem(FDNIndex[Index])^.Index;
        FDNItems[ItemNr] := DNItem;
    end
    else begin
     // see if re-using a deleted record

     // else allocate new record, increasing array size if necessary
            ItemNr := FTotDNItems;
            if Length(FDNItems) <= ItemNr then begin
                SetLength(FDNItems, FTotDNItems + 32);
                RebuildIndex;
            end;
        FTotDNItems := FTotDNItems + 1;
        DNItem.Index := ItemNr;
        FDNItems[ItemNr] := DNItem;
        FDNIndex.AddSorted(@FDNItems[ItemNr], IcsCompareFNext);
    end;
    FDNItems[ItemNr].DNState := StateNone;
    Result := ItemNr;

 // start maintenance timer
    if NOT FMaintTimer.Enabled then
        FMaintTimer.Enabled := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ returns ItemNr into DNItems array }
function TIcsDomainNameCache.FindRequest(const Request: String): Integer;
var
    Index: Integer;
begin
    Result := -1;
    if NOT FDefListDone then begin
        FDefListDone := True;
        if FDefHostList.Count > 0 then begin
            AddListtoCache(FDefHostList);
            LogEvent('Loaded Default Host List');
        end;
        if FAddLocalhost then begin
            AddtoCache('127.0.0.1 localhost');
            AddtoCache('::1 localhost');
            AddtoCache('::1 localhost6');
            LogEvent('Loaded localhosts');
        end;
    end;
    if (FTotDNItems = 0) or (FDNIndex.Count = 0) then
        Exit;
    if FDNIndex.Find(@Request, IcsCompareFNext, Index) then begin
       if Request = PDNItem(FDNIndex[Index])^.Request then  // may be partial match
            Result := PDNItem(FDNIndex[Index])^.Index;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.LookupRequest(const Req: String; Tag: Integer; Sync: Boolean; ReqType: TDNReqType;
                         DNFamily: TSocketFamily = sfAny; UpdEvent: TDNUpdateEvent = Nil; SkipEvent: Boolean = False): Integer;
var
    NewDNItem: TDNItem;
    Flag: Boolean;
    DurDT: TDateTime;
    EndTimer: Int64;
    MyFamily: TSocketFamily;
    IPv4: TIcsIPv4Address;
begin
    Result := -1;

 // validate request
    if ReqType = ReqTypeDnsForw then begin
        if (Pos(IcsSPACE, Req) > 0) or (Pos(',', Req) > 0)  then  begin
            LogEvent('Invalid Forward Name Request: ' + Req);
            Exit;
        end;
    end
    else begin
        if (NOT WSocketIsIP(Req, MyFamily)) or (Req = '0.0.0.0') then begin
            LogEvent('Invalid Reverse Name Request: ' + Req);
            Exit;
        end;
    end;

 // see if cached already
    Result := FindRequest(Req);
    if Result >= 0 then begin
        FDNItems[Result].ReqTag := Tag;
        FDNItems[Result].ReqUpdEvent := UpdEvent;

     // not found something yet, exit
        if (FDNItems[Result].DNState in [StateNone, StateWaiting]) then
            Exit;
        if NOT SkipEvent then    // tell application we're done, unless skipping event for immediate response
            TriggerUpdate(Result);

      // check TTL see if need to refresh, give up if nothing to check
        if (FDNItems[Result].TimeStamp < 100) or (FDNItems[Result].TTL <= 0)  then
            Exit;
        DurDT := (Now - FDNItems[Result].TimeStamp)  * SecsPerDay ;  // convert to seconds
   //    LogEvent('Old Request Age: ' + IntToStr(Trunc(DurDT)) + ' seconds - ' + Req);   // !!! TEMP
        if (Trunc(DurDT) < FDNItems[Result].TTL) then  // still valid TTL, exit now
            Exit;
        LogEvent('Refreshing Name Request: ' + Req);
    end
    else begin
        LogEvent('Adding New Name Request: ' + Req);
        NewDNItem.Request := Req;
        NewDNItem.ReqTag := Tag;
        NewDNItem.DNReqType := ReqType;
        NewDNItem.TimeStamp := 0;
        NewDNItem.TTL := 0;
        NewDNItem.ReqFamily := DNFamily;
        NewDNItem.ReqUpdEvent := UpdEvent;
        NewDNItem.TotResp4 := 0;
        NewDNItem.TotResp6 := 0;
        Result := AddNewRec(NewDNItem);
    end;

 // special case of LAN IP, use WSock or NetBIOS instead
    if (FDBLANlookup > LanLookDef) and (ReqType = ReqTypeDnsBack) and (MyFamily = sfIPv4) then begin
        IPv4 := WSocketStrToIPv4(Req, Flag);
        if IcsIsLocalIPv4(IPv4) then
            FDNItems[Result].DNReqType := ReqTypeNetBios;
    end;

 // start look up name or IP by selected method
    Flag := StartRequest(Result);
    if NOT Flag then begin
        Result := -1;
        Exit;
    end;

 // wait for response in sync mode
    if Sync then begin
        if FQTimeout = 0 then
            FQTimeout := 5;
        EndTimer := IcsGetTrgSecs64(FQTimeout);
        while FDNItems[Result].DNState = StateWaiting do begin
          {$IFDEF MSWINDOWS}
            if MsgWaitForMultipleObjects(0, Pointer(nil)^, FALSE, 10, QS_ALLINPUT) = WAIT_OBJECT_0 then
          {$ENDIF}
                MessagePump;
            if IcsTestTrgTick64(EndTimer) then
                break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.LookupHostSync(const Host: String; Tag: Integer = 0; DNFamily: TSocketFamily = sfAny; UpdEvent: TDNUpdateEvent = Nil): Integer;
begin
    Result := LookupRequest(Host, Tag, True, ReqTypeDnsForw, DNFamily, UpdEvent, False);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.LookupIPSync(const IP: String; Tag: Integer = 0; DNFamily: TSocketFamily = sfAny; UpdEvent: TDNUpdateEvent = Nil): Integer;
begin
    Result := LookupRequest(IP, Tag, True, ReqTypeDnsBack, DNFamily, UpdEvent, False);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.LookupHostAsync(const Host: String; Tag: Integer = 0; DNFamily: TSocketFamily = sfAny; UpdEvent: TDNUpdateEvent = Nil): Integer;
begin
    Result := LookupRequest(Host, Tag, False, ReqTypeDnsForw, DNFamily, UpdEvent, False);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.LookupIPAsync(const IP: String; Tag: Integer = 0; DNFamily: TSocketFamily = sfAny; UpdEvent: TDNUpdateEvent = Nil): Integer;
begin
    Result := LookupRequest(IP, Tag, False, ReqTypeDnsBack, DNFamily, UpdEvent, False);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.LookupHostOne(const Host: String; Tag: Integer = 0; DNFamily: TSocketFamily = sfAny; UpdEvent: TDNUpdateEvent = Nil): String;
var
    ItemNr: Integer;
begin
    ItemNr := LookupRequest(Host, Tag, False, ReqTypeDnsForw, DNFamily, UpdEvent, True);
    if ItemNr >= 0 then begin
        with FDNItems[ItemNr] do begin
            if DNState = StateOK then begin
                if ((TotResp4 > 0) and (DNFamily in [sfAny, sfIPv4])) then
                    Result := Responses4[0]
                else if ((TotResp6 > 0) and (DNFamily in [sfAny, sfIPv6])) then
                    Result := Responses6[0];
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.LookupIPOne(const IP: String; Tag: Integer = 0; DNFamily: TSocketFamily = sfAny; UpdEvent: TDNUpdateEvent = Nil): String;
var
    ItemNr: Integer;
begin
    Result := IP;
    ItemNr := LookupRequest(IP, Tag, False, ReqTypeDnsBack, DNFamily, UpdEvent, True);
    if ItemNr >= 0 then begin
        if (FDNItems[ItemNr].DNState = StateOK) and (FDNItems[ItemNr].TotResp6 > 0) then
            Result := FDNItems[ItemNr].Responses6[0];
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.GetDNItem(ItemNr: Integer): TDNItem;
var
    DNItem: TDNItem;
begin
    if (ItemNr < 0) or (ItemNr > FTotDNItems) then
        Result := DNItem   // empty
    else
        Result := FDNItems[ItemNr];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ build list of IPv4 and IPv6 addresses for display }
function TIcsDomainNameCache.BuildRespList(ItemNr: Integer): String;
var
    J: Integer;
begin
    Result := '';
    if (ItemNr < 0) or (ItemNr > FTotDNItems) then
        Exit;
    with FDNItems[ItemNr] do begin
        if DNState = StateOK then begin
            if TotResp4 > 0 then begin
                for J := 0 to TotResp4 - 1 do begin
                    if J > 0 then
                        Result := Result + ' | ';
                    Result := Result + Responses4[J];
                end;
            end;
            if TotResp6 > 0 then begin
                for J := 0 to TotResp6 - 1 do begin
                    if Result <> '' then
                        Result := Result + ' | ';
                    Result := Result + Responses6[J];
                end;
            end;
        end
        else
            Result := 'No Response';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ gets a host from the cache, returns single IP address or blank if not cached }
function TIcsDomainNameCache.GetHost(const Host: String; DNFamily: TSocketFamily = sfAny): String;
var
    ItemNr: Integer;
begin
    Result := '';
    ItemNr := FindRequest(Host);
    if ItemNr >= 0 then begin
        with FDNItems[ItemNr] do begin
            if DNState = StateOK then begin
                if ((TotResp4 > 0) and (DNFamily in [sfAny, sfIPv4])) then
                    Result := Responses4[0]
                else if ((TotResp6 > 0) and (DNFamily in [sfAny, sfIPv6])) then
                    Result := Responses6[0];
            end;
        end;
     end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ gets an IP address from the cache, returns host name or IP address if not yet cached }
function TIcsDomainNameCache.GetIP(const IP: String): String;
var
    ItemNr: Integer;
begin
     Result := IP;
     ItemNr := FindRequest(IP);
     if ItemNr >= 0 then begin
        if (FDNItems[ItemNr].DNState = StateOK) and (FDNItems[ItemNr].TotResp6 > 0) then
            Result := FDNItems[ItemNr].Responses6[0];
     end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.ListCache: String;
var
    I: Integer;
    S: String;
begin
    if FDNIndex.Count = 0 then begin
        Result := 'Name Server Cache is Empty' + IcsCRLF;
        Exit;
    end;
    Result := 'Name Server Cache, Total Items: ' + IntToStr(FDNIndex.Count) + IcsCRLF;
    for I := 0 to FDNIndex.Count - 1 do begin
        with PDNItem(FDNIndex[I])^ do begin
            S := Request + ' > ' + BuildRespList(Index) + ' ' + GetEnumName(TypeInfo(TDNState), Ord(DNState)) +
                                ' Stamp=' + TimeToStr(TimeStamp) + ' TTL=' + IntToStr(TTL) + ' Index=' + IntToStr(Index);
            Result := Result + S + IcsCRLF;
        end;
    end;
    I := PendingLookups;
    if I = 0 then
        Result := Result + 'No Pending Lookups'
    else
       Result := Result + 'Number of Pending Lookups: ' + IntToStr(I);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.MaintClearAll;
var
    I: Integer;
begin
    FMaintTimer.Enabled := False;
    FDefListDone := False;
    FTotDNItems := 0;
    FDNIndex.Clear;
    FLookupQu.Clear;
    if Length(FCntlLookups) > 0 then begin
        for I := 1 to Length(FCntlLookups) do begin
            FCntlLookups[I].Busy := False;
        end;
    end;
    LogEvent('Cleared Name Server Cache');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.PendingLookups: Integer;
var
    I: Integer;
begin
    Result := FLookupQu.Count;
    if Length(FCntlLookups) > 0 then begin
        for I := 1 to Length(FCntlLookups) do begin
            if (FCntlLookups[I].Busy) then
                Result := Result + 1
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.MaintUpdateAll;
begin
//
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsDomainNameCache.CancelLookups;
var
    I: Integer;
begin
    if Length(FDNWSockets) > 0 then begin
        for I := 0 to Length(FDNWSockets) - 1 do begin
            if Assigned (FDNWSockets[I]) then begin
                try
                    if FDNWSockets[I].State = wsDnsLookup then
                        FDNWSockets[I].CancelDnsLookup;
                    FDNWSockets[I].Abort;
                    FDNWSockets[I].Free;
                    FDNWSockets[I] := nil ;
                except
                end ;
            end;
        end ;
    end;
    SetLength(FDNWSockets, 0);
    SetLength(FCntlLookups, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.SaveCacheToFile: Boolean;
begin
    Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsDomainNameCache.ReadCacheFromFile: Boolean;
begin
    Result := False;
end;


end.
