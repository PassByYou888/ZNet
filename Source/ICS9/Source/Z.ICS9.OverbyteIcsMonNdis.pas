{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Plibcap function calls for Npcap NDIS Packet Capture Driver.
Creation:     2000
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

Note
----

This unit requires the Npcap driver to be downloaded and installed from:

https://npcap.com/

Current version in Feb 2023 is 1.72 which supports Windows 7 to 11.

Wireshark and other sniffer tools also uses this driver, so it may be installed already.
Npcap should be installed with the WinPCap compatible box ticked, this component does not
yet support any advanced features of Npcap. Npcap may be installed so that administrator
program rights are required by applications for improved security.

Npcap works on Windows 7 and later by making use of the NDIS 6 Light-Weight Filter (LWF).


Original author

Conversion of BPF.H From C to ObjectPascal- By Lars Peter Christiansen

NDIS DEFINITIONS Collection of Types/consts/macros originally for the Plibcap function library - By Lars Peter Christiansen

API for the Packet Capture Driver by Politecnico di Torino Converted By Lars Peter Christiansen

Plibcap Highlevel function calls for Packet Capture Driver by Politecnico di Torino
Code converted and modified from C to Pascal by Lars Peter Christiansen

 TERMS AND CONDITIONS OF USE.
 some parts of this software is Copyright(C) 2000 Lars Peter Christiansen.

 The author of this software assumes no liability for damages caused under
 any circumstances whatsoever, and is under no obligation. Use of the software
 indicates acceptance of all conditions contained in this document. If you do
 not agree to these terms, you must delete this software immediately.

 You may distribute the archive in which this software is distributed, but
 under no circumstances must this archive be changed. Distributing a modified
 archive is a violation of the software license.

 If you do redistribute this software, please let me know at the email address
 given below.

 If you have any questions, requests, bug reports, etc., please contact me at
 the address given below.

Lars Peter Christiansen
Email  : lp@nzlab.dk
Website: http://www.nzlab.dk

Packet32.dll author:
old website: http://www.winpcap.org/

Npcap is an update of WinPcap. It is developed by the Nmap Project as a
continuation of the project started by Yang Luo under Google Summer of
Code 2013 and 2015. It also received many helpful tests from Wireshark and NetScanTools.
https://npcap.com/ and https://npcap.com/vs-winpcap.html


                          [ user application ]
                          [       PCAP       ] <- you are here!
                          [    PacketAPI     ]
            -------------------------------------------------------
            [ Windows 95/98 |  Windows NT  |  Windows2000/XP/W2K  ]
            [  Packet.dll   |  Packet.dll  |  packet.dll          ]
            [  npf.vxd      |  npf.sys     |  npf.sys             ]
            -------------------------------------------------------
                          [    Netadapter    ]


   Implemented Original Functions :
      Function pcap_open_live() : PPcap
      function pcap_read()      : integer
      function pcap_stats()     : integer
      function pcap_setbuff()   : integer
      function pcap_loop()      : integer
      function pcap_datalink    : integer

30th October 2005 - Angus Robertson, Magenta Systems Ltd, took over support.
Replaced static linkage with dynamic DLL loading
Added new winpcap website
Fixed Pcap_GetAdapternames not returning any names for Windows 2000 and 2003,
  now checking WinPcap version so 3.0 and 3.1 both supported transparently
Added Pcap_GetAdapternamesEx to return both adaptor names and friendly descriptions
Added Pcap_GetDriverVersion (3.1), Pcap_GetPacketVersion
Added Pcap_GetIPAddresses to get adapter IP addresses
Added Pcap_SetMinToCopy (3.1)
pcap_open_live tries to set SnapLen (3.1)
Added Pcap_GetMacAddress, useful to see if packets are being sent or received

8th August 2008 - Angus Robertson, Magenta Systems Ltd
Updated for Delphi 2009
Tested with WinPcap version 4.0.2 9th November 2007

9th August 2010 - Angus Robertson, Magenta Systems Ltd
Fixed various cast warnings with Delphi 2009 and later
Tested with WinPcap version 4.1.2 2nd July 2010

26 Nov 2018 - Angus Robertson, Magenta Systems Ltd
Tested with Npcap which has superceded Pcap but is compatible.
Updated for ICS V8.

Apr 20, 2023 - V8.71 Updated units for main ICS library.
                     Removed support for WinPCap 3 and earlier and XP and earlier.
                     Added IPv6 support.
Aug 08, 2023 V9.0  Updated version to major release 9.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Z.ICS9.OverbyteIcsMonNdis;

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
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    Z.ICS9.OverbyteIcsWinsock,
    Z.ICS9.OverbyteIcsUtils;


const
    CopyRight    : String     = ' Npcap NDIS Driver  (c) 2023 V9.0 ';

Type
  // For future combatibility
  Tbpf_u_int32 = LongWord;
  Tbpf_int32   = Integer;

  //[ taken from DRIVER\packet.h ]
  // Unix's way of timestamping.
  PunixTimeVal = ^TunixTimeVal;
  TunixTimeVal = record
    tv_Sec,            // Secs since 1/1/1970
    tv_uSec: LongWord; // microseconds
  end;

  // [ Gotten the following structs from LIBPCAP\Packet32.h]
  Tbpf_insn = record
    code : Word;
    jt   : Byte;
    jf   : Byte;
    k    : Integer;
  end;

  Pbpf_program = ^Tbpf_program;
  Tbpf_program = record
    bf_len  : LongWord;
    bf_insns: ^Tbpf_insn;
  end;

  Pbpf_stat = ^Tbpf_stat;
  Tbpf_stat = record
    bs_recv,
    bs_drop : LongWord;
  end;

  Pbpf_hdr = ^Tbpf_hdr;        //Structure prepended to each packet.
  Tbpf_hdr =record
    bh_tstamp :TunixTimeval;    //* time stamp */
    bh_caplen,              //* length of captured portion */
    bh_datalen: Tbpf_u_int32;   //* original length of packet */
    bh_hdrlen : Word ;          //* length of bpf header (this struct plus
                                //alignment padding) */
  end;

const
  BPF_ALIGNMENT = sizeof(Tbpf_int32);

  DLT_NULL   =0;    //* no link-layer encapsulation */
  DLT_EN10MB     =1;    //* Ethernet (10Mb) */
  DLT_EN3MB      =2;    //* Experimental Ethernet (3Mb) */
  DLT_AX25       =3;    //* Amateur Radio AX.25 */
  DLT_PRONET     =4;    //* Proteon ProNET Token Ring */
  DLT_CHAOS      =5;    //* Chaos */
  DLT_IEEE802    =6;    //* IEEE 802 Networks */
  DLT_ARCNET     =7;    //* ARCNET */
  DLT_SLIP   =8;    //* Serial Line IP */
  DLT_PPP    =9;    //* Point-to-point Protocol */
  DLT_FDDI   =10;   //* FDDI */
  DLT_ATM_RFC1483=11;   //* LLC/SNAP encapsulated atm */
  DLT_RAW    =12;   //* raw IP */
  DLT_SLIP_BSDOS =13;   //* BSD/OS Serial Line IP */
  DLT_PPP_BSDOS  =14;   //* BSD/OS Point-to-point Protocol */

  //New types for Win32
  DLT_EN100MB    =100;  //* Ethernet (100Mb) */
  DLT_PPP_WIN32  =101;  //* Win32 dial up connection */

{
[Copied from Ndis.h]
the following constants are to be used to direct the
underlying NIC driver to choose which type of packet can be delivered to the
upper bound driver, that is, our snoop driver.
}
const
  NDIS_PACKET_TYPE_DIRECTED =           $0001;
  NDIS_PACKET_TYPE_MULTICAST =          $0002;
  NDIS_PACKET_TYPE_ALL_MULTICAST =      $0004;
  NDIS_PACKET_TYPE_BROADCAST =          $0008;
  NDIS_PACKET_TYPE_SOURCE_ROUTING =     $0010;
  NDIS_PACKET_TYPE_PROMISCUOUS =        $0020; //for snoop
  NDIS_PACKET_TYPE_SMT =                $0040;
  NDIS_PACKET_TYPE_MAC_FRAME =          $8000;
  NDIS_PACKET_TYPE_FUNCTIONAL =         $4000;
  NDIS_PACKET_TYPE_ALL_FUNCTIONAL =     $2000;
  NDIS_PACKET_TYPE_GROUP =              $1000;

Type

   // Taken from NTDDNDIS.H
     TNDIS_MEDIUM = (NdisMedium802_3,
                     NdisMedium802_5,
                     NdisMediumFddi,
                     NdisMediumWan,
                     NdisMediumLocalTalk,
                     NdisMediumDix, // defined for convenience, not a real medium
                     NdisMediumArcnetRaw,
                     NdisMediumArcnet878_2,
                     NdisMediumAtm,
                     NdisMediumWirelessWan,
                     NdisMediumIrda,
                     NdisMediumMax );


 DEVICE_TYPE  = LONGWORD;


Const

// Object Identifiers used by NdisRequest Query/Set Information
//
// Taken from PACKET95\INC\ntddndis.h
//
//
// General Objects
OID_GEN_CURRENT_PACKET_FILTER      = $0001010E;
OID_GEN_MEDIA_IN_USE               = $00010104;
OID_GEN_LINK_SPEED                 = $00010107;

// 802.3 Objects (Ethernet)
//
OID_802_3_PERMANENT_ADDRESS        = $01010101;
OID_802_3_CURRENT_ADDRESS          = $01010102;
OID_802_3_MULTICAST_LIST           = $01010103;
OID_802_3_MAXIMUM_LIST_SIZE        = $01010104;
OID_802_3_MAC_OPTIONS              = $01010105;


// Defines the method codes for how buffers are passed for I/O and FS controls
METHOD_BUFFERED                 =0;
METHOD_IN_DIRECT                =1;
METHOD_OUT_DIRECT               =2;
METHOD_NEITHER                  =3;

// The FILE_READ_ACCESS and FILE_WRITE_ACCESS constants are also defined in
// ntioapi.h as FILE_READ_DATA and FILE_WRITE_DATA. The values for these
// constants *MUST* always be in sync.
//
FILE_ANY_ACCESS           =0;
FILE_READ_ACCESS          =1;   // file & pipe
FILE_WRITE_ACCESS         =2;   // file & pipe

// Define the various device type values.  Note that values used by Microsoft
// Corporation are in the range 0-32767, and 32768-65535 are reserved for use
// by customers.

FILE_DEVICE_PROTOCOL      =  $8000;

var
IOCTL_PROTOCOL_QUERY_OID,
IOCTL_PROTOCOL_SET_OID,
IOCTL_PROTOCOL_STATISTICS,
IOCTL_PROTOCOL_RESET,
IOCTL_PROTOCOL_WRITE,
IOCTL_PROTOCOL_MACNAME,
IOCTL_CLOSE,
IOCTL_OPEN,
IOCTL_PROTOCOL_READ        : LongWord;  // filled during initialisation

Const
  DLL  = 'packet.dll';            // Name of DLL file
  DEFAULT_DRIVERBUFFER = 1000000; // Dimension of the buffer in driver
  MAX_LINK_NAME_LENGTH = 64;      // Adapters symbolic names maximum length

type

  // Adapter with which the driver communicates
  Padapter = ^Tadapter;
  Tadapter = packed Record
    hFile        : LongWord;
    SymbolicLink : array [0..MAX_LINK_NAME_LENGTH-1] of AnsiChar;
  end;

  // Packet the driver uses as means of data transport.
  // both snooped data and certain device controlling
  Ppacket = ^Tpacket;
  Tpacket = packed record           // Changed Jan.1 2002. Thanks to Deheng Xu
    hevent             :Thandle;
    OverLapped         :TOVERLAPPED;
    Buffer             :Pointer;
   //Next               :Pointer;     // also commented out in "packet32.h"
    Length             :Longword;
    ulBytesReceived    :LongWord;
    bIoComplete        :Boolean;
  end;


  // [Gotten from LIBPCAP\ntddpack.h]
  // Data structure to control the device driver
  PPACKET_OID_DATA = ^TPACKET_OID_DATA;
  TPACKET_OID_DATA = packed record
    Oid   : LongWord;               // Device control code
    Length: LongWord;               // Length of data field
    Data  : Pointer;                // Start of data field
  end;

  // [Gotten from BPF.h? - more appropiate here!]
  Pnet_type = ^Tnet_type;
  Tnet_type = packed record
    LinkType,
    LinkSpeed : LongWord;
  end;

// from winsock2.h
// Portable socket structure (RFC 2553).

// Desired design of maximum size and alignment.
// These are implementation specific.

const
  _SS_MAXSIZE   = 128;               // Maximum size.
  {$EXTERNALSYM _SS_MAXSIZE}
  _SS_ALIGNSIZE = SizeOf(Int64);  // Desired alignment.
  {$EXTERNALSYM _SS_ALIGNSIZE}

// Definitions used for sockaddr_storage structure paddings design (holds both ip4 and ip6 addresses)

  _SS_PAD1SIZE = _SS_ALIGNSIZE - SizeOf(short);
  {$EXTERNALSYM _SS_PAD1SIZE}
  _SS_PAD2SIZE = _SS_MAXSIZE - (SizeOf(short) + _SS_PAD1SIZE + _SS_ALIGNSIZE);
  {$EXTERNALSYM _SS_PAD2SIZE}

type
// 128 byte structure,  only use ss_family member of the SOCKADDR_STORAGE. The remaining members
// ensure that the SOCKADDR_STORAGE can contain either an IPv6 or IPv4 address as sockaddr_in or TSockAddr
  sockaddr_storage = record
    ss_family: short;               // Address family.
    __ss_pad1: array [0.._SS_PAD1SIZE - 1] of AnsiChar;  // 6 byte pad, this is to make
                                   // implementation specific pad up to
                                   // alignment field that follows explicit
                                   // in the data structure.
    __ss_align: Int64;            // Field to force desired structure.
    __ss_pad2: array [0.._SS_PAD2SIZE - 1] of AnsiChar;  // 112 byte pad to achieve desired size;
                                   // _SS_MAXSIZE value minus size of
                                   // ss_family, __ss_pad1, and
                                   // __ss_align fields is 112.
  end;
  {$EXTERNALSYM sockaddr_storage}
  TSockAddrStorage = sockaddr_storage;
  PSockAddrStorage = ^sockaddr_storage;

 // from packet32.h - used by PacketGetNetInfoEx for 3.1 and later
  Pnpf_if_addr = ^Tnpf_if_addr ;
  Tnpf_if_addr = packed record
    IPAddress: TSockAddrStorage ;  // includes IP4 and IP6 addresses
    SubnetMask: TSockAddrStorage ;
    Broadcast: TSockAddrStorage ;
  end ;

var

//------------------------------------------------------------------------------
//ULONG PacketGetAdapterNames(PTSTR pStr, PULONG BufferSize)
//------------------------------------------------------------------------------
PacketGetAdapterNames: Function (pStr: PAnsiChar; BufferSize: PLongWord) : Boolean; cdecl ;
{
This is the first function that must be used to communicate with the driver.
It returns the names of the adapters installed in the system through the user
allocated buffer pStr. BufferSize is the length of the buffer.

Warning: the result of this function is obtained querying directly the registry,
therefore the format of the result in Windows NT is different from the one in
Windows 95/98. This is due to the fact that Windows 95 uses the ASCII
encoding method to store a string, while Windows NT uses UNICODE. After a
call to PacketGetAdapterNames in Windows 95,  pStr contains an ASCII string
with the names of the adapters separated by ASCII "\0". The string is
terminated by a double "\0". In Windows NT, pStr contains a UNICODE string
with the names of the adapters separated by a "\0" UNICODE character
(i.e. 2 ASCII "\0"), and the string ends with a double UNICODE "\0".

Angus - above warning only relates to WinPcap 3.0 and earlier
with WinPcap 3.1 and later only ASCII is returned

Returns:
If the function succeeds, the return value is nonzero. If the return value is zero, BufferSize contains the number of bytes that are needed to contain the adapter list.
Usually, this is the first function that should be used to communicate with the driver. It returns the names of the adapters installed on the system and supported by WinPcap. After the names of the adapters, pStr contains a string that describes each of them.
After a call to PacketGetAdapterNames pStr contains, in succession:

1 a variable number of ASCII (Unicode for 3.0 and earlier) strings, each with the
  names of an adapter, separated by a "\0"
2 a double "\0"
3 a number of ASCII strings (for all versions), each with the description of an
  adapter, separated by a "\0". The number of descriptions is the same of the one
  of names. The fisrt description corresponds to the first name, and so on.
4 a double "\0".

}

//------------------------------------------------------------------------------
// LPADAPTER PacketOpenAdapter(LPTSTR AdapterName)
//------------------------------------------------------------------------------
PacketOpenAdapter: Function (AdapterName:PAnsiChar) : PAdapter; cdecl ;
{
This function receives a string containing the name of the adapter to open and
returns the pointer to a properly initialized ADAPTER object. The names of the
adapters can be obtained calling the PacketGetAdapterNames function.

Note: as already said, the Windows 95 version of the capture driver works with
the ASCII format, the Windows NT version with UNICODE. Therefore, AdapterName
should be an ASCII string in Windows 95, and a UNICODE string in Windows NT.
This difference is not a problem if the string pointed by AdapterName was
obtained through the PacketGetAdapterNames function, because it returns the
names of the adapters in the proper format. Instead, some problems can arise
in Windows NT if the string is obtained from ANSI C functions like scanf,
because they use the ASCII format. This can be a relevant problem when porting
command-line applications from UNIX. To avoid it, we included in the Windows NT
version of PacketOpenAdapter a routine to convert strings from ASCII to UNICODE.
PacketOpenAdapter in Windows NT accepts both the ASCII and the UNICODE format.
If a ASCII string is received, it is converted to UNICODE before being passed
to the driver.
}

//------------------------------------------------------------------------------
// VOID PacketCloseAdapter(LPADAPTER lpAdapter)
//------------------------------------------------------------------------------
PacketCloseAdapter: Procedure (pAdapter:Padapter); cdecl ;
{
This function deallocates the ADAPTER structure lpAdapter, and closes the
adapter pointed by it.
}

//------------------------------------------------------------------------------
// LPPACKET PacketAllocatePacket(void)
//------------------------------------------------------------------------------
PacketAllocatePacket: Function: PPacket; cdecl ;
{
Allocates a PACKET structure and returns a pointer to it. The structure
returned must be properly initialized by calling the PacketInitPacket function.

Warning: The Buffer field of the PACKET structure is not set by this function.
The buffer must be allocated by the programmer, and associated to the PACKET
structure with a call to PacketInitPacket.
}

//------------------------------------------------------------------------------
// VOID PacketInitPacket(LPPACKET lpPacket, PVOID Buffer, UINT Length)
//------------------------------------------------------------------------------
PacketInitPacket: Procedure (pPacket:Ppacket;Buffer:Pointer;Length:LongWord); cdecl;
{
It initializes a structure PACKET. There are three input parameters:

* a pointer to the structure to initialize
* a pointer to the user-allocated buffer that will contain the packet data
* length of the buffer. This is the maximum length that will be transferred in a
  single read from the driver to the application.

Note: The dimension of the buffer associated with the PACKET structure is a
parameter that can sensibly influence the performances of the capture process.
This buffer will in fact receive the packets from the packet capture driver.
The driver is able to collect data from several packets, returning it with only
one read call (see the PacketReceivePacket function). The number of packets
that the driver can transfer to the application in a single call with this
method is limited only by the dimension of the buffer associated with the
PACKET structure used to perform the reading. Therefore setting a big buffer
with PacketInitPacket can throw down the number of system calls, improving the
capture speed. Notice also that, when the application performs a
PacketReceivePacket, it is usually NOT blocked until the buffer associated with
the PACKET structure full. The driver copies the data present in its buffer,
but awakes the application without filling its buffer if it has not enough data
at the moment. In this way, also with big buffers, the application works
efficiently when the data rate on the network is low.
}


//------------------------------------------------------------------------------
// VOID PacketFreePacket(LPPACKET lpPacket)
//------------------------------------------------------------------------------

PacketFreePacket: Procedure( pPacket:Ppacket); cdecl ;
{
This function frees the PACKET structure pointed by lpPacket.

Warning: The Buffer field of the PACKET structure is not deallocated by this
function, and must be deallocated by the programmer.
}

//------------------------------------------------------------------------------
// BOOLEAN PacketReceivePacket(LPADAPTER AdapterObject, LPPACKET lpPacket,
//                             BOOLEAN Sync)
//------------------------------------------------------------------------------
PacketReceivePacket: Function (AdapterObject:Padapter;pPacket:PPacket;
         Sync:Boolean):Longbool; cdecl ;

{
This function performs the capture of a set of packets. It has the following
input parameters:

* a pointer to an ADAPTER structure identifying the network adapter from which
  the packets must be captured
* a pointer to a PACKET structure that will contain the packets
* a flag that indicates if the operation will be done in a synchronous or
  asynchronous way. If the operation is synchronous, the function blocks the
  program, returning only when the it is completed. If the operation is
  asynchronous, the function doesn’t block the program, and the PacketWaitPacket
  procedure must be used to verify the correct completion.

The number of packets received with this function cannot be known before the
call and can vary a lot. It depends on the number of packets actually stored in
the driver’s buffer, on the size of these packets, and on the size of the buffer
associated with the lpPacket parameter. Figure 3.1 shows the method used by the
driver in order to send the packets to the application.

                 [BPF_HDR]
                 [ DATA  ]
                 [PADDING]
                 [BPF_HDR]
                 [ DATA  ]
                 [PADDING]

                 Figure 3.1: method used to encode the packets

Packets are stored in the buffer associated with the lpPacket PACKET structure.
Each packet has a trailer consisting in a bpf_hdr structure that defines its
length and holds its timestamp. At the end of the packet there is a padding
used to word-align the data in the buffer (to increase the speed of the copies).
In order to extract the packets from the buffer the bh_datalen and bh_hdrlen of
the bpf_hdr structures should be used. An example can be seen in the sample
application provided in the developer's pack, or in the pcap_read() function in
the pcap-win32.c file (that can be found in the source distribution). Pcap
extracts correctly each incoming packet before passing it to the application,
so an application that uses it will not have to do this operation.
}

//------------------------------------------------------------------------------
// BOOLEAN PacketWaitPacket(LPADAPTER AdapterObject, LPPACKET lpPacket)
//------------------------------------------------------------------------------
PacketWaitPacket: Function (AdapterObject:Padapter;lpPacket:Ppacket):LongBool; cdecl ;
{
This function is used to verify the completion of an I/O operation on the
packet capture driver. It is blocking if the operation has still to be
completed by the driver. The return value is TRUE if the operation was
successful, FALSE otherwise, and the SDK GetLastError function can be used in
order to retrieve the error code.
}



//------------------------------------------------------------------------------
// BOOLEAN PacketSendPacket(LPADAPTER AdapterObject, LPPACKET pPacket, BOOLEAN Sync)
//------------------------------------------------------------------------------
PacketSendPacket: Function ( AdapterObject:Padapter;pPacket:PPacket;Sync:boolean)
         :Longbool ;cdecl ;

{This function is used to send a packet to the network through the adapter
specified with the AdapterObject parameter. It has the same syntax of the
PacketReceivePacket function. This function can be used to send only a packet
at a time and the user will not have to put a bpf_hdr header before it.
}

//------------------------------------------------------------------------------
// BOOLEAN PacketResetAdapter(LPADAPTER AdapterObject)
//------------------------------------------------------------------------------
PacketResetAdapter: Function ( AdapterObject:Padapter):Longbool; cdecl ;
{
It resets the adapter passed as input parameter. Returns TRUE if the operation
is performed successfully.
}




//------------------------------------------------------------------------------
// BOOLEAN PacketSetHwFilter(LPADAPTER AdapterObject, ULONG Filter)
//------------------------------------------------------------------------------
PacketSetHwFilter: Function( AdapterObject:Pointer;Filter:Longword):Longbool; cdecl ;
{
This function sets a hardware filter on the incoming packets. The constants
that define the filters are declared in the file ntddndis.h. The input
parameters are the adapter on which the filter must be defined, and the
identifier of the filter. The value returned is TRUE if the operation was
successful. Here is a list of the most useful filters:

NDIS_PACKET_TYPE_PROMISCUOUS: set the promiscuous mode. Every incoming packet is
                              accepted by the adapter.
NDIS_PACKET_TYPE_DIRECTED   : only the packets destined to the adapter are
                              accepted.
NDIS_PACKET_TYPE_BROADCAST  : only the broadcast packets are accepted.
NDIS_PACKET_TYPE_MULTICAST  : only the multicast packets belonging to the groups
                              of which this adapter is a member are accepted.
NDIS_PACKET_TYPE_ALL_MULTICAST: every multicast packet is accepted
}


//------------------------------------------------------------------------------
// BOOLEAN PacketRequest(LPADAPTER AdapterObject,BOOLEAN Set, PPACKET_OID_DATA
//                       OidData)
//------------------------------------------------------------------------------
PacketRequest: Function ( AdapterObject:Padapter;isSet:Longbool;OidData:
                        PPacket_oid_data ):Longbool;cdecl ;

{This function is used to perform a query/set operation on the adapter pointed
by AdapterObject. The second parameter defines if the operation is a set
(set=1) or a query (set=0). The third parameter is a pointer to a
PACKET_OID_DATA structure (see the section on the data structures).
The return value is true if the function is completed without errors.
The constants that define the operations are declared in the file ntddndis.h.
More details on the argument can be found in the documentation provided with
the DDK.

NOTE: not all the network adapters implement all the query/set functions.
There is a set of mandatory OID functions that is granted to be present on all
the adapters, and a set of facultative functions, no provided by all the
adapters (see the DDKs to see which functions are mandatory). If you use a
facultative function, please be careful and enclose it in an if statement to
check the result.
}


//------------------------------------------------------------------------------
//BOOLEAN PacketSetBuff(LPADAPTER AdapterObject, int dim)
//------------------------------------------------------------------------------
PacketSetBuff: Function (AdapterObject: Padapter;dim:integer) : Longbool; cdecl ;

{This function is used to set a new dimension of the driver’s circular buffer
associated with the adapter pointed by AdapterObject. dim is the new dimension
in bytes. The function returns TRUE if successfully completed, FALSE if there
is not enough memory to allocate the new buffer. When a new dimension is set,
the data in the old buffer is discarded and the packets stored in it are lost.

Note: the dimension of the driver’s buffer affects HEAVILY the performances of
the capture process. In fact, a capture application needs to make operations on
each packet while the CPU is shared with other tasks. Therefore the application
should not be able to work at network speed during heavy traffic or bursts,
especially in presence of high CPU load due to other applications. This problem
is more noticeable on slower machines. The driver, on the other hand, runs in
kernel mode and is written explicitly to capture packets, so it is very fast
and usually does not loose packets. Therefore, an adequate buffer in the driver
to store the packets while the application is busy can compensate the slowness
of the application and can avoid the loss of packets during bursts or high
network activity. When an instance of the driver is opened the dimension of the
buffer is set to 0. The programmer must remember to set it to a proper value.

Libpcap calls this functions and sets the buffer size to 1MB. Therefore programs
written using libpcap usually do not need to cope with this problem.
}


//------------------------------------------------------------------------------
// BOOLEAN PacketSetBpf(LPADAPTER AdapterObject, struct bpf_program *fp)
//------------------------------------------------------------------------------
PacketSetBpf: Function ( AdapterObject:Padapter;fp:Pbpf_program):Longbool; cdecl;

{This function associates a new BPF filter with the adapter AdapterObject.
The filter, pointed by fp, is a set of instructions that the BPF
register-machine of the driver will execute on each packet. Details can be
found into the chapter on the driver, or in [McCanne and Jacobson 1993].
This function returns TRUE if the driver is set successfully, FALSE if an
error occurs or if the filter program is not accepted. The driver performs a
check on every new filter in order to avoid system crashes due to bogus or
buggy programs, and it rejects the invalid filters.

If you need to create a filter, use the pcap_compile function of libpcap.
It converts a text filter with the syntax of WinDump (see the manual of
WinDump for more details) into a BPF program. If you don't want to use libpcap,
but you need to know the code of a filter, launch WinDump with the -d or -dd
or -ddd parameters.
}

//------------------------------------------------------------------------------
//  BOOLEAN PacketGetStats(LPADAPTER AdapterObject, struct bpf_stat *s)
//------------------------------------------------------------------------------
PacketGetStats: Function ( AdapterObject:Padapter;s: Pbpf_stat):Longbool; cdecl;

{With this function, the programmer can know the value of two internal variables
of the driver:

* the number of packets that have been received by the adapter AdapterObject,
  starting at the time in which it was opened.
* the number of packets received by the adapter but that have been dropped by
  the kernel. A packet is dropped when the application is not ready to get it
  and the buffer associated with the adapter is full.

The two values are copied by the driver in a bpf_stat structure (see section 3
of this manual) provided by the application. These values are very useful to
know the situation of the network and the behavior of the capture application.
They are also very useful to tune the capture stack and to choose the
dimension of the buffers. In fact:

a high value of the bs_recv variable means that there is a lot of traffic on the
network. If the application doesn’t need all the packets (for example a monitor
application may want to capture only the traffic generated by a particular
protocol, or by a single host), it is better to set a selective BPF filter,
to minimize the number of packets that the application has to process. Since
the filter works at kernel level, an appropriate filter increases the
performances of the application and decreases the load on the system. In
this way a non interesting packet does not need to be transferred from kernel
to user space, avoiding the memory copy and the context switch between kernel
and user mode.
If bs_drop is greater than zero, the application is too slow and is loosing
packets. The programmer can try, as a first solution, to set a greater buffer
in the driver with the PacketSetBuff function. A proper dimension of the buffer
often decreases dramatically the packet loss. Another solution is to speed up
the capture process associating a bigger buffer with the PACKET structure used
in the PacketReceivePacket call (see the PacketInitPacket function). This
decreases the number of system calls, improving the speed.
If the application keeps on loosing packets, probably it should be rewritten or
optimized. The driver is already very fast, and probably it is better to modify
the application than the driver, where the main optimization that can be done
is the implementation of the word-alignment.
}

//------------------------------------------------------------------------------
// BOOLEAN PacketGetNetType (LPADAPTER AdapterObject,NetType *type)
//------------------------------------------------------------------------------
PacketGetNetType: Function (AdapterObject:Padapter; nettype:Pnet_Type):LongBool; cdecl;
{Returns the type of the adapter pointed by AdapterObject in a NetType structure.
The LinkType of the type paramter can be set to one of the following values:

NdisMedium802_3: Ethernet (802.3)
NdisMedium802_5: Token Ring (802.5)
NdisMediumFddi: FDDI
NdisMediumWan: WAN
NdisMediumLocalTalk: LocalTalk
NdisMediumDix: DIX
NdisMediumArcnetRaw: ARCNET (raw)
NdisMediumArcnet878_2: ARCNET (878.2)
NdisMediumWirelessWan: Various types of NdisWirelessXxx media.
The LinkSpeed field indicates the speed of the network in Bits per second.

The return value is TRUE if the operation is performed successfully.
}

//------------------------------------------------------------------------------
// BOOLEAN PacketGetNetType (LPADAPTER AdapterObject,NetType *type)
//------------------------------------------------------------------------------
PacketSetReadTimeout: Function (AdapterObject:Padapter;timeout:integer):boolean; cdecl;
{Sets the timeout value for the given adapter. }

//------------------------------------------------------------------------------
// PCHAR PacketGetDriverVersion  (    )   3.1 and later
//------------------------------------------------------------------------------
PacketGetDriverVersion: function: PAnsiChar ; cdecl;

{   Return a string with the version of the NPF.sys device driver.
Returns:
A char pointer to the version of the driver. }

//------------------------------------------------------------------------------
// PCHAR PacketGetVersion  (    )
//------------------------------------------------------------------------------
PacketGetVersion: function: PAnsiChar ; cdecl;

{   Return a string with the dll version.
Returns:
A char pointer to the version of the library. }

//------------------------------------------------------------------------------
// BOOLEAN PacketGetNetInfoEx  (  PCHAR  AdapterName,  npf_if_addr *  buffer,  PLONG  NEntries )
//------------------------------------------------------------------------------
PacketGetNetInfoEx: function (AdapterName: PAnsiChar; Buffer: Pnpf_if_addr;  NEntries: PInteger): boolean ; cdecl ;

{  Returns comprehensive information the addresses of an adapter.
Parameters:
 AdapterName  String that contains the name of the adapter.
 buffer  A user allocated array of npf_if_addr that will be filled by the function.
 NEntries  Size of the array (in npf_if_addr).

Returns:
If the function succeeds, the return value is nonzero.
This function grabs from the registry information like the IP addresses,
the netmasks and the broadcast addresses of an interface. The buffer passed
by the user is filled with npf_if_addr structures, each of which contains the
data for a single address. If the buffer is full, the reaming addresses are dropped,
therefore set its dimension to sizeof(npf_if_addr) if you want only the first address. }

//------------------------------------------------------------------------------
// BOOLEAN PacketSetMinToCopy  (  LPADAPTER  AdapterObject,  int  nbytes )
//------------------------------------------------------------------------------
PacketSetMinToCopy: function (AdapterObject: Padapter ; nbytes: integer): boolean ; cdecl ;

{   Defines the minimum amount of data that will be received in a read.

Parameters:
 AdapterObject  Pointer to an _ADAPTER structure
 nbytes  the minimum amount of data in the kernel buffer that will cause the driver
  to release a read on this adapter.

Returns:
If the function succeeds, the return value is nonzero.
In presence of a large value for nbytes, the kernel waits for the arrival of several
packets before copying the data to the user. This guarantees a low number of system
calls, i.e. lower processor usage, i.e. better performance, which is a good setting
for applications like sniffers. Vice versa, a small value means that the kernel will
copy the packets as soon as the application is ready to receive them. This is suggested
for real time applications (like, for example, a bridge) that need the better
responsiveness from the kernel.
note: this function has effect only in Windows NTx. The driver for Windows 9x doesn't
offer this possibility, therefore PacketSetMinToCopy is implemented under these systems
only for compatibility. }


//------------------------------------------------------------------------------
// INT PacketSetSnapLen  (  LPADAPTER  AdapterObject,  int  snaplen ) 3.1 and later
//------------------------------------------------------------------------------
PacketSetSnapLen: function (AdapterObject: Padapter ; snaplen: integer): integer ; cdecl ;

{  Sets the snap len on the adapters that allow it.
 Parameters:
 AdapterObject  Pointer to an _ADAPTER structure.
 snaplen  Desired snap len for this capture.

Returns:
If the function succeeds, the return value is nonzero and specifies the actual snaplen
that the card is using. If the function fails or if the card does't allow to set snap
length, the return value is 0.
The snap len is the amount of packet that is actually captured by the interface and
received by the application. Some interfaces allow to capture only a portion of any
packet for performance reasons.

Note:
: the return value can be different from the snaplen parameter, for example some
boards round the snaplen to 4 bytes. }

//------------------------------------------------------------------------------

const
  PCAP_ERRBUF_SIZE = 256;              //String size of error descriptions
  PcapBufSize      = 256000;           //Dimension of the buffer in TPcap


// [taken from interface.h]

  DEFAULT_SNAPLEN = 68;                //The default snapshot length.
                                       //This value allows most printers to
                                       //print useful information while keeping
                                       //the amount of unwanted data down.In
                                       //particular, it allows for an ethernet
                                       //header, tcp/ip header, and 14 bytes of
                                       //data (assuming no ip options).

type

  PPcap_Stat = ^TPcap_stat;
  Tpcap_stat = record
    ps_recv,                         //* number of packets received */
    ps_drop,                         //* number of packets dropped */
    ps_ifdrop : LongWord;                //* drops by interface not supported */
  end;

  TPcap_sf = record                      // Save file for offline reading.
    rfile : HFILE;
    swapped:integer;
    version_major : integer;
    Version_Minor : integer;
    base : Pointer;
  end;

  TPcap_md = record
    Stat : TPcap_stat;
    use_bpf : integer;
    TotPkts  : LongWord;               // Can owerflow after 79hours on ethernet
    TotAccepted:LongWord;              // accepted by filter/sniffer
    TotDrops : LongWord;               // dropped packets
    TotMissed: Longword;               // missed by i/f during this run
    OrigMissed:LongWord;               // missed by i/f before this run
  end;

  PPcap_PktHdr = ^Tpcap_pkthdr;        // Wrapped Drivers packetHeader
  TPcap_pkthdr = record
    ts     : TUnixTimeVal;             // Time of capture
    CapLen,                            // captured length
    Len    : Integer;                  // actual length of packet
  end;

  PPcap = ^TPcap;                      // THE MAIN INTERFACE HANDLE
  TPcap = record                       // used with allmost all Pcap calls.
    Adapter:Padapter;                  // ANSI name
    Packet :PPacket;                   // Global Driver packet. kind of a buffer
    snapshot:integer;
    linktype:integer;                  // Type and speed of net
    tzoff   :integer;                  // timezone offset
    offset  :integer;
    sf      :Tpcap_sf;                 // Save file
    md      :Tpcap_md;                 // Diagnostics
    //READ BUFFER
    bufsize :integer;
    buffer  :Pointer; //*u_char
    bp      :Pointer; //*u_char
    cc      :integer;
    //Place holder for pcap_next().
    pkt     :Pointer; //*U_char
    //Placeholder for filter code if bpf not in kernel.
    fcode   :Tbpf_program;
    errbuf  : array [0..PCAP_ERRBUF_SIZE-1] of AnsiChar;  //Last error message
  end;


  // Callback procedure
  Ppcap_handler =^Tpcap_handler;
  Tpcap_handler = procedure(User:pointer;const Header:Ppcap_pkthdr;const Data:PAnsiChar);

  // array of IPv4 and IPv6 addresses
  TIpAddrArray = array of TSockAddrIn6;   // V8.71 was TInAddr which is IPv4 only, now IPv4 and IPv6

  // a MAC address
  TMacAddr = array [0..5] of byte ;

//------------------------------------------------------------------------------

function CTL_CODE (Device, Func, Method, Access: LongWord): LongWord;
Function BPF_WORDALIGN (X: LongWord): LongWord;  //Force data to be aligned
function  pcap_open_live (const Device: String; SnapLen: LongWord; Promisc: boolean ; To_ms: integer; var errstr: String): ppcap;
function  pcap_read (p: PPcap; cnt: integer; CallBack: Tpcap_handler; User: pointer): integer;
function  pcap_stats (P: pPcap; ps: PPcap_stat): integer;
function  pcap_setbuff (p: Ppcap; dim: integer): integer;
procedure pcap_close (var p: ppcap);
function  pcap_lookupdev (var ErrStr: string): PAnsiChar;
function pcap_loop (P: Ppcap; cnt: integer; Callback: Tpcap_handler; user: pointer): integer;
function pcap_datalink(P: PPcap): integer;
function Pcap_getAdapternames(Delimiter: Char; var ErrStr: string): string;
function Pcap_GetAdapternamesEx (NameList, DescList: TStringList; var ErrStr: string): integer;
function Pcap_GetDriverVersion: string ;
function Pcap_GetPacketVersion: string ;
function Pcap_GetIPAddresses (const AdapterName: String ; var IPArray, MaskArray, BcastArray: TIPAddrArray; var ErrStr: string): integer ;
function Pcap_SetMinToCopy (P: pPcap; nbytes: integer) : integer;
function Pcap_GetMacAddress (P: pPcap; var ErrStr: string): TMacAddr ;

//------------------------------------------------------------------------------

var
    PacketDllModule: THandle;
    function LoadPacketDll: Boolean;

implementation

//------------------------------------------------------------------------------
// This was originally a C macro :
//
// #define BPF_WORDALIGN(x) (((x)+(BPF_ALIGNMENT-1))&~(BPF_ALIGNMENT-1))
//
//------------------------------------------------------------------------------
function BPF_WORDALIGN(X:LongWord) : LongWord;
begin
  result := (((X)+(BPF_ALIGNMENT-1))and not(BPF_ALIGNMENT-1));
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The original C Macro :
#define CTL_CODE( DeviceType, Function, Method, Access )
  (
    ((DeviceType) << 16) | ((Access) << 14) | ((Function) << 2) | (Method)
  )
}
function CTL_CODE(Device,Func,Method,Access:LongWord):LongWord;
begin
 result :=( (Device shl 16) OR (Access shl 14) OR (func shl 2) OR method );
end;

//------------------------------------------------------------------------------
//  pcap_t *pcap_open_live(char *device, int snaplen, int promisc,
//                         int to_ms, char *ebuf)
//------------------------------------------------------------------------------
function pcap_open_live(const Device: String; SnapLen: LongWord; Promisc: boolean; To_ms: integer; var errstr: String): ppcap;
var
     P : Ppcap;
     NetType : Tnet_type;

     procedure CleanUp;
     begin
       if P.adapter<>nil then PacketCloseAdapter(P.adapter);
       if P.buffer<>nil then FreeMem(P.buffer,PcapBufSize);
       Freemem(P,SizeOf(Tpcap));

     end;
begin
    result :=nil;
    if NOT LoadPacketDll then
      begin
        ErrStr := 'Cannot load packet.dll';
        exit;
      end;

    // CREATE PCAP OBJECT

    GetMem(P,SizeOf(Tpcap));
    if P=nil then
      begin
        ErrStr := 'Cannot allocate pcap object';
        exit;
      end;
    FillChar(p^,sizeof(Tpcap),0);
    P.Adapter := nil;

    // CREATE ADAPTER OBJECT
    P.Adapter := PacketOpenAdapter(PAnsiChar(AnsiString(Device)));
    if P.Adapter = nil then
      begin
        ErrStr := 'Cannot Open Adapter "' + Device +'"';
        CleanUp;
        exit;
      end;


   // SET FILTER MODE
    if Promisc then
      begin
        if not PacketSetHWFilter(P.adapter,NDIS_PACKET_TYPE_PROMISCUOUS) then
          Begin
            ErrStr:= 'Cannot set Device Filter to Promiscuous mode';
            cleanup;
            exit;
          end;
      end else if not PacketSetHWFilter(P.adapter,NDIS_PACKET_TYPE_DIRECTED) then
          begin
            ErrStr:= 'Cannot set Device Filter to Directed mode';
            cleanup;
            exit;
          end;

    // GET NETCARD SPEED AND TYPE
    if not PacketGetNetType(P.Adapter,@Nettype) then
       Begin
         ErrStr := 'Cannot determine network type and speed';
         Cleanup;
         exit;
       end;

    Case TNDIS_MEDIUM(nettype.LinkType) of

       NdisMediumWan   : P.linktype := DLT_PPP_WIN32;

       NdisMedium802_3 : begin
                           if nettype.LinkSpeed = 100000000 then
                              p.linktype := DLT_EN100MB
                           else if nettype.LinkSpeed=10000000 then
                              p.linktype := DLT_EN10MB
                           else p.linktype:=DLT_PPP_WIN32;
                         end;
       else p.linktype := DLT_EN10MB;
    end;

    // Allocate room for Link header

    p.bufsize := PcapBufSize;
    GetMem(p.buffer,PcapBufSize);
    if P.buffer = nil then
      begin
        ErrStr := 'Cannot allocate Link Header space';
        cleanup;
        exit;
      end;

    if Assigned (PacketSetSnapLen) then
        p.snapshot := PacketSetSnapLen(P.adapter, Snaplen)     // Angus - added, actually set it for 3.1
    else
        p.snapshot := Snaplen ;

    // Allocate Global Packet for capturing

    p.packet := PacketAllocatePacket;
    if p.packet = nil then
      begin
        ErrStr := 'Cannot allocate Global Packet Object';
        cleanup;
        exit;
      end;
    PacketInitPacket(p.Packet,p.buffer,p.bufsize);

    // Allocate Driver Buffer
    if not PacketSetBuff(p.adapter,DEFAULT_DRIVERBUFFER) then
      begin
        ErrStr := 'Not enough memory to allocate Driver buffer';
        CleanUp;
        exit;
      end;

    result := p;

end;


//------------------------------------------------------------------------------
//int pcap_read(pcap_t *p, int cnt, pcap_handler callback, u_char *user)
//
//------------------------------------------------------------------------------
function pcap_read(p: PPcap; cnt: integer; CallBack: Tpcap_handler; User: pointer): integer;
var cc   : Longword;//Counter ?
    n    : integer;
    bp,ep: pointer; //Begin and End Point ?
    //bhp  : Pbpf_hdr;//pointer to BPF header struct - removed by Lars Peter
    hdrlen,         //Length of Header
    caplen: integer;//Length of captured
begin
  if NOT LoadPacketDll then
  begin
     p.errbuf := 'Cannot load packet.dll';
     result:=-1;
     exit;
  end;
  cc := p.cc;
  n  := 0;

  if p.cc = 0 then
    begin

       // *Capture the Packets*
         if PacketReceivePacket(p.adapter,p.packet,TRUE)=false then
         begin
           // ERROR!
           p.errbuf :='Read Error: PacketRecievePacket failed';
           result:=-1;
           exit;
         end;
         cc := p.packet.ulBytesReceived;

         bp := p.buffer;

    end
    else
        bp := p.bp;


    // Loop through each packet.

    ep := ptr(longword(bp)+cc); //move end pointer
    while (longword(bp) < longword(ep) ) do
      begin
        caplen := Pbpf_hdr(bp).bh_caplen;
        hdrlen := Pbpf_hdr(bp).bh_hdrlen;

        // XXX A bpf_hdr matches apcap_pkthdr.

        callback(user, Ppcap_pkthdr(bp), ptr(longword(bp)+longword(HdrLen)));

    //    LongWord(bp) := LongWord(bp) + BPF_WORDALIGN(caplen + hdrlen);
        bp := Pointer( LongWord(bp) + BPF_WORDALIGN(caplen + hdrlen));   { V8.71 keeping Delphi 11.3 happy }
        inc(n);
        if (n >= cnt)and(cnt>0) then
          begin
            p.bp := bp;
            p.cc := longword(ep)-longword(bp);
            result := n;
            exit;
          end;
      end;

   p.cc := 0;
   result:=n;
end;


//------------------------------------------------------------------------------
// int pcap_stats(pcap_t *p, struct pcap_stat *ps)
//
//------------------------------------------------------------------------------
function pcap_stats(P: pPcap;ps:PPcap_stat) : integer;
var s:Tbpf_stat;
begin
    if NOT LoadPacketDll then
    begin
        p.errbuf := 'Cannot load packet.dll';
        result:=-1;
        exit;
    end;
    if PacketGetStats(
                      P.Adapter,
                      @s) = false then
    begin
      P.errbuf := 'PacketGetStats error';
      result := -1;
      exit;
    end;

    ps.ps_recv := s.bs_recv;
    ps.ps_drop := s.bs_drop;
    result:= 0;
end;

//------------------------------------------------------------------------------
// int pcap_setbuff(pcap_t *p, int dim)
//
//------------------------------------------------------------------------------
function pcap_setbuff(p : Ppcap;dim:integer) : integer;
begin

    if NOT LoadPacketDll then
    begin
        p.errbuf := 'Cannot load packet.dll';
        result:=-1;
        exit;
    end;
    if p=nil then
    begin
      result:=-2;
      P.errbuf := 'invalid pcap handle';
      exit;
    end;

    if PacketSetBuff(p.adapter,dim)=false then
    begin
      P.Errbuf := 'Driver error : Not enough memory to allocate buffer';
      result := -1;
      exit;
    end;
    result := 0;
end;


//------------------------------------------------------------------------------
//  void pcap_close(pcap_t *p)
//
// Very simplified from the original
//------------------------------------------------------------------------------
procedure pcap_close(var p : ppcap);
begin

  if NOT LoadPacketDll then exit ;
  if p=nil then exit;
  if p.Adapter<>nil then
    begin
      PacketCloseAdapter(p.adapter);
      p.adapter:=nil;
    end;

  if p.buffer<>nil then
    begin
      FreeMem(P.buffer,p.bufsize);
      p.buffer := nil;
    end;
  FreeMem(p,sizeof(Tpcap));
  p:=nil;
end;



//------------------------------------------------------------------------------
//
//     Following procedures is taken from inet.c part of Pcap
//
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//int pcap_loop(pcap_t *p, int cnt, pcap_handler callback, u_char *user)
//------------------------------------------------------------------------------
{pcap_loop() is similar to pcap_dispatch() except it keeps reading
packets until cnt packets are processed or an error occurs. It does
not return when live read timeouts occur. Rather, specifying a
non-zero read timeout to pcap_open_live() and then calling
pcap_dispatch() allows the reception and processing of any
packets that arrive when the timeout occurs. A negative cnt
causes pcap_loop() to loop forever (or at least until an error
occurs).
}
function pcap_loop(P:Ppcap;cnt:integer;Callback:Tpcap_handler;user:pointer):integer;
begin
  result:=-1;
  if NOT LoadPacketDll then
  begin
     p.errbuf := 'Cannot load packet.dll';
     exit;
  end;
  if p=nil then exit;
  while true do begin

      if p.sf.rfile<>0 then
        begin
          result:= -1; //pcap_offline_read(p,cnt,callback,user);
          exit;
        end
      else Repeat
          // Keep reading until we get something(or get an error)
             result := pcap_read(p,cnt,callback,user);
           until result<>0;

      if result<=0 then exit;

      if cnt>0 then
        begin
          cnt:=cnt-result;
          if cnt<=0 then
            begin
              result:=0;
              exit;
            end;
        end;
  end;
end;



//------------------------------------------------------------------------------
{int pcap_dispatch(pcap_t *p, int cnt, pcap_handler callback, u_char *user)}
//------------------------------------------------------------------------------
{pcap_dispatch() is used to collect and process packets. cnt
specifies the maximum number of packets to process before returning.
A cnt of -1 processes all the packets received in one buffer.
A cnt of 0 processes all packets until an error occurs, EOF is
reached, or the read times out (when doing live reads and a
non-zero read timeout is specified). callback specifies a routine
to be called with three arguments: a u_char pointer which is
passed in from pcap_dispatch(), a pointer to the pcap_pkthdr
struct (which precede the actual network headers and data),
and a u_char pointer to the packet data. The number of packets read
is returned. Zero is returned when EOF is reached in a
``savefile.'' A return of -1 indicates an error in which
case pcap_perror() or pcap_geterr() may be used to display the
error text.}

function pcap_dispatch(P :pPcap;cnt:integer;CallBack:Tpcap_handler;User:pointer):integer;
begin
  if NOT LoadPacketDll then
  begin
     p.errbuf := 'Cannot load packet.dll';
     result:=-1;
     exit;
  end;
  if P.sf.rfile<>0 Then
      result := -1//pcap_offline_read(p,cnt,callback,user)
  else
      result := pcap_read(p,cnt,callback,user)
end;


//------------------------------------------------------------------------------
//char * pcap_lookupdev(errbuf)
//------------------------------------------------------------------------------
//*
// * Return the name of a network interface attached to the system, or NULL
// * if none can be found.  The interface must be configured up; the
// * lowest unit number is preferred; loopback is ignored.
//
function pcap_lookupdev(var ErrStr:string) : PAnsiChar;
var
    NameLength   : integer;
    WadapterNames: array[0..1024-1] of WideChar;
    i            : integer;
    AdapterName1 : PAnsiChar;
begin
    Result := Nil ;
    if NOT LoadPacketDll then
    begin
        ErrStr:='Cannot load packet.dll';
        exit;
    end;
    NameLength := 1024;
    Getmem(AdapterName1, NameLength *Sizeof(AnsiChar));
    PacketGetAdapterNames(PAnsiChar(@wAdapterNames), @NameLength);
     for i:=0 to NameLength-1 do
     begin
        if (Wadapternames[i]=#0) and (wadapternames[i+1]=#0) then
            break;
        AdapterName1[i] := AnsiChar (wAdapterNames[i]);
     end;
     result := adaptername1;
end;

//------------------------------------------------------------------------------
// int pcap_datalink(pcap_t *p)
//------------------------------------------------------------------------------
// Returns the link type of the device
function pcap_datalink(P:PPcap) : integer;
begin
  result := p.linktype;
end;


//------------------------------------------------------------------------------
// Get All AdapterNames seperated with chosen delimiter // Added By Lars Peter
// angus - note this function does not return the adaptor friendly descriptions
//------------------------------------------------------------------------------
function Pcap_GetAdapternames(Delimiter: Char; var ErrStr: string): string;
var
    NameList : Array [0..(4096*2)-1] of AnsiChar;
    NameLength,  i :Longword;
begin
    result := '' ;
    ErrStr := '' ;
    if NOT LoadPacketDll then
    begin
        ErrStr:='Cannot load packet.dll';
        exit;
    end;
    NameLength := 4096;
    FillChar (NameList, Sizeof(NameList), 0) ;
    PacketGetAdapterNames(NameList,@NameLength);
     for i:=0 to NameLength-1 do
     begin
        if (PWideChar(@NameList)[i] = #0) and (PWideChar(@namelist)[i+1] = #0) then
            break
        else if (PWideChar(@NameList)[i] = #0) then
            PWideChar(@NameList)[i] := WideChar(delimiter);
     end;
     result := WideCharToString(PWideChar(@NameList)) ;
end;

//------------------------------------------------------------------------------
// Get All AdapterNames into two TStringLists, return total adaptors
// Added By Angus Robertson
//------------------------------------------------------------------------------
function Pcap_GetAdapternamesEx (NameList, DescList: TStringList; var ErrStr: string): integer ;
var
    NameBuff : Array [0..4096-1] of AnsiChar;
    CurChar, CurName: PAnsiChar ;
//    CurWChar, CurWName: PWideChar ;
    newname: string ;
    BuffLen: integer;
    descflag: boolean ;
begin
    result := 0 ;
    ErrStr := '' ;
    if NOT LoadPacketDll then
    begin
        ErrStr:='Cannot load packet.dll';
        exit;
    end;
    if (NOT Assigned (NameList)) or (NOT Assigned (DescList)) then
    begin
        ErrStr:='String List not intialised';
        exit;
    end;
    NameList.Clear ;
    DescList.Clear ;
    BuffLen := 4096;
    FillChar (NameBuff, BuffLen, 0) ;
    if NOT PacketGetAdapterNames (NameBuff, @BuffLen) then
    begin
        ErrStr:= 'Failed to get adaptor names';
        exit;
    end;
    descflag := false ;
    CurChar := NameBuff ;
    CurName := CurChar ;

// winpcap returns lists of ASCII adapter names followed by list of ASCII adapter descriptions
    while true do
    begin
        if NOT descflag then  // get adaptor names first
        begin
            if (CurChar^ = #0) then
            begin
                if (CurChar = CurName) then  // double null
                begin
                    descflag := true ;
                    inc (CurChar) ;
                    continue;
                end
                else
                begin
                    newname := Trim (String(CurName)) ;
                    NameList.Add (newname) ;
                end ;
                CurName := CurChar + 1 ;
            end ;
            inc (CurChar) ;
        end
        else
        begin         // getting ASCII adaptor descriptions
           if (CurChar^ = #0) then
           begin
                if (CurChar = CurName) then break ; // second double null
                newname := Trim (String (CurName)) ;  // convert PChar to string // 8 Aug 2010
                DescList.Add (newname) ;
                CurName := CurChar + 1 ;
                if NameList.Count = DescList.Count then break ;  // found same number, stop
            end ;
            inc (CurChar) ;
        end ;
    end;
    result := NameList.Count ;
end ;

//------------------------------------------------------------------------------
// Get netgroup packet filter driver version - npf.sys   - 3.1 and later only
// Added By Angus Robertson
//------------------------------------------------------------------------------
function Pcap_GetDriverVersion: string ;
begin
   result := '' ;
   if NOT LoadPacketDll then
   begin
      result:='Cannot load packet.dll';
      exit;
   end;
   if NOT Assigned (PacketGetDriverVersion) then
   begin
      result:='Version not available';
      exit;
   end;
   result := String (PacketGetDriverVersion) ;  // 8 Aug 2010
end ;

//------------------------------------------------------------------------------
// Get packet driver DLL version - packet.dll
// Added By Angus Robertson
//------------------------------------------------------------------------------
function Pcap_GetPacketVersion: string ;
begin
   result := '' ;
   if NOT LoadPacketDll then
   begin
      result:='Cannot load packet.dll';
      exit;
   end;
   result := String (PacketGetVersion) ; // 8 Aug 2010
end ;

//------------------------------------------------------------------------------
// Get adaptor link information, IP addresses, masks and broadcast addresses
// Added By Angus Robertson
//------------------------------------------------------------------------------
function Pcap_GetIPAddresses (const AdapterName: String ; var IPArray, MaskArray, BcastArray: TIPAddrArray; var ErrStr:string): integer ;
var
    NetInfo, CurInfo: Pnpf_if_addr ;
    BuffLen, MaxEntries, I: integer ;
begin
   result := 0 ;
   ErrStr := '' ;
   if NOT LoadPacketDll then
   begin
      ErrStr:='Cannot load packet.dll';
      exit;
   end;
   MaxEntries := 48 ;    // Feb 2022 was 10, but we have many more IP addresses now
   BuffLen := SizeOf (Tnpf_if_addr) * MaxEntries ;
   GetMem (NetInfo, BuffLen) ;
   FillChar (NetInfo^, BuffLen, 0) ;
   if NOT Assigned (PacketGetNetInfoEx) then
        exit ;
   if NOT PacketGetNetInfoEx (PAnsiChar (AnsiString(AdapterName)), NetInfo, @MaxEntries) then
   begin
      ErrStr:= 'Failed to get adaptor names';
      FreeMem (NetInfo) ;
      exit;
   end;
   SetLength (IPArray, MaxEntries) ;
   SetLength (MaskArray, MaxEntries) ;
   SetLength (BcastArray, MaxEntries) ;
   CurInfo := NetInfo ;
   for I := 0 to Pred (MaxEntries) do
   begin
     // V8.71, sockaddr_storage holds a TSockAddrIn with IPv4 and IPv6 addresses, easy to keep
        Move (CurInfo.IPAddress, IPArray [I], Sizeof (TSockAddrIn6));
        Move (CurInfo.SubnetMask, MaskArray [I], Sizeof (TSockAddrIn6));
        Move (CurInfo.Broadcast, BcastArray [I], Sizeof (TSockAddrIn6));
        PAnsiChar (CurInfo) := PAnsiChar (CurInfo) + SizeOf (Tnpf_if_addr) ;
   end ;
   FreeMem (NetInfo) ;
   result := MaxEntries ;
end ;

//------------------------------------------------------------------------------
// Set minimum data for driver to return
// Added By Angus Robertson
//------------------------------------------------------------------------------
function Pcap_SetMinToCopy (P: pPcap ; nbytes: integer) : integer;
begin
    if NOT LoadPacketDll then
    begin
        p.errbuf := 'Cannot load packet.dll';
        result:=-1;
        exit;
    end;
    if NOT PacketSetMinToCopy (P.Adapter, nbytes) then
    begin
      P.errbuf := 'PacketSetMinToCopy error';
      result := -1;
      exit;
    end;
    result:= 0;
end;

//------------------------------------------------------------------------------
// Get adaptor MAC address
// Added By Angus Robertson
//------------------------------------------------------------------------------
function Pcap_GetMacAddress (P: pPcap; var ErrStr:string): TMacAddr ;
var
    OidData: array [0..20] of AnsiChar ;
    POidData :PPACKET_OID_DATA ;
begin
    FillChar (Result, SizeOf (Result), 0) ;
    ErrStr := '' ;
    if NOT LoadPacketDll then
    begin
        ErrStr:='Cannot load packet.dll';
        exit;
    end;
    FillChar (OidData [0], SizeOf (OidData), 0) ;
    POidData := @OidData ;
    POidData.Oid := OID_802_3_CURRENT_ADDRESS ;
    POidData.Length := 6 ;
    if NOT PacketRequest (P.Adapter, false, POidData) then  // get data, not set it!
    begin
        ErrStr:= 'Failed to get adaptor MAC';
        exit;
    end;
    Move (POidData.Data, Result, SizeOf (Result)) ;
end ;


//------------------------------------------------------------------------------
function LoadPacketDll: Boolean;
begin
    Result := True;
    if PacketDllModule <> 0 then Exit;

// open DLL
    PacketDllModule := LoadLibrary (DLL);
    if PacketDllModule = 0 then
    begin
        Result := false;
        exit ;
    end ;
    PacketGetAdapterNames := GetProcAddress (PacketDllModule, 'PacketGetAdapterNames') ;
    PacketOpenAdapter := GetProcAddress (PacketDllModule, 'PacketOpenAdapter') ;
    PacketCloseAdapter := GetProcAddress (PacketDllModule, 'PacketCloseAdapter') ;
    PacketAllocatePacket := GetProcAddress (PacketDllModule, 'PacketAllocatePacket') ;
    PacketInitPacket := GetProcAddress (PacketDllModule, 'PacketInitPacket') ;
    PacketFreePacket := GetProcAddress (PacketDllModule, 'PacketFreePacket') ;
    PacketReceivePacket := GetProcAddress (PacketDllModule, 'PacketReceivePacket') ;
    PacketWaitPacket := GetProcAddress (PacketDllModule, 'PacketWaitPacket') ;
    PacketSendPacket := GetProcAddress (PacketDllModule, 'PacketSendPacket') ;
    PacketResetAdapter := GetProcAddress (PacketDllModule, 'PacketResetAdapter') ;
    PacketSetHwFilter := GetProcAddress (PacketDllModule, 'PacketSetHwFilter') ;
    PacketRequest := GetProcAddress (PacketDllModule, 'PacketRequest') ;
    PacketSetBuff := GetProcAddress (PacketDllModule, 'PacketSetBuff') ;
    PacketSetBpf := GetProcAddress (PacketDllModule, 'PacketSetBpf') ;
    PacketGetStats := GetProcAddress (PacketDllModule, 'PacketGetStats') ;
    PacketGetNetType := GetProcAddress (PacketDllModule, 'PacketGetNetType') ;
    PacketSetReadTimeout := GetProcAddress (PacketDllModule, 'PacketSetReadTimeout') ;
    PacketGetVersion := GetProcAddress (PacketDllModule, 'PacketGetVersion') ;
    PacketGetNetInfoEx := GetProcAddress (PacketDllModule, 'PacketGetNetInfoEx') ;
    PacketSetMinToCopy := GetProcAddress (PacketDllModule, 'PacketSetMinToCopy') ;
    PacketGetDriverVersion := GetProcAddress (PacketDllModule, 'PacketGetDriverVersion') ;  // 3.1 and later
    PacketSetSnapLen := GetProcAddress (PacketDllModule, 'PacketSetSnapLen') ;              // 3.1 and later
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

initialization

    PacketDllModule := 0 ;
    IOCTL_PROTOCOL_QUERY_OID  :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 0 , METHOD_BUFFERED, FILE_ANY_ACCESS);
    IOCTL_PROTOCOL_SET_OID    :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 1 , METHOD_BUFFERED, FILE_ANY_ACCESS);
    IOCTL_PROTOCOL_STATISTICS :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 2 , METHOD_BUFFERED, FILE_ANY_ACCESS);
    IOCTL_PROTOCOL_RESET      :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 3 , METHOD_BUFFERED, FILE_ANY_ACCESS);
    IOCTL_PROTOCOL_READ       :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 4 , METHOD_BUFFERED, FILE_ANY_ACCESS);
    IOCTL_PROTOCOL_WRITE      :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 5 , METHOD_BUFFERED, FILE_ANY_ACCESS);
    IOCTL_PROTOCOL_MACNAME    :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 6 , METHOD_BUFFERED, FILE_ANY_ACCESS);
    IOCTL_OPEN                :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 7 , METHOD_BUFFERED, FILE_ANY_ACCESS);
    IOCTL_CLOSE               :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 8 , METHOD_BUFFERED, FILE_ANY_ACCESS);

finalization
    if PacketDllModule <> 0 then
    begin
        FreeLibrary (PacketDllModule) ;
        PacketDllModule := 0 ;
    end ;

end.
