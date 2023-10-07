{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Internet monitoring using Npcap NDIS driver, TIcsMonPcap
              component.
Creation:     2005
Updated:      July 2023
Version:      8.71
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

Current version in July 2023 is 1.76 which supports Windows 7 to 11.

Wireshark and other sniffer tools also uses this driver, so it may be installed already.
Npcap should be installed with the WinPCap compatible box ticked, this component does not
yet support any advanced features of Npcap. Npcap may be installed so that administrator
program rights are required by applications for improved security.

Npcap works on Windows 7 and later by making use of the NDIS 6 Light-Weight Filter (LWF).


The Delphi conversion for packet.dll in OverbyteIcsMonNdis is
by Lars Peter Christiansen, http://www.nzlab.dk, but modified by Magenta Systems
in 2005 from static linkage to dynamic DLL loading to allow the application to load
without the DLL and to fix problems reading the adaptor list.

8 Aug 2008 - 1.2
Updated to support ICS V6 and V7, and Delphi 2009.

26 Nov 2018 - 1.4
No changes, testing with ICS V8, add Npcap support.

Jul 21, 2023 - V8.71 Updated units for main ICS library.
                     Added IPv6 support.
                     Added MAC adaptor vendor name.
                     Filtering of packets no longer handled here, but in the event.
                     TIcsMonPcap now calls events in context of main thread.
                     TIcsMonPcap uses IcsDomainNameCache component to lookup source
                       and destination host names.
Aug 08, 2023 V9.0  Updated version to major release 9.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Z.ICS9.OverbyteIcsMonPcap;

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
{$R-}             { Range errors off                    }
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    Z.ICS9.OverbyteIcsWinsock,
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsUtils,
    Z.ICS9.OverbyteIcsIpUtils,
    Z.ICS9.OverbyteIcsDnsQuery,
    Z.ICS9.OverbyteIcsMonCommon,
    Z.ICS9.OverbyteIcsMonNdis;

const
    CopyRight    : String     = ' TIcsMonPcap  (c) 2023 V8.71 ';

type
  TPcapThread = class ;  // forward declaration

  TIcsMonPcap = class(TComponent)
  protected
      FLastError: string ;
      FAddr: string ;
      FIgnoreData: boolean ;
      FPromiscuous: boolean ;
      FTotRecvBytes: int64 ;
      FTotSendBytes: int64 ;
      FTotRecvPackets: integer ;
      FTotSendPackets: integer ;
      FDriverVersion: string ;
      FPacketVersion: string ;
      FOnPacketEvent: TPacketDataEvent ;
      FOnLogEvent: TPacketLogEvent;
      FPcapThread: TPcapThread ;  // read packet thread
      FAdapterNameList: TStringList;  // ethernet adapters internal names
      FAdapterDescList: TStringList;  // ethernet adapters descriptions
      FMonAdapter: String ;       // adapter to monitor
      FConnected: boolean ;       // are we connected to PCap driver
      FAdapterMac: TMacAddr ;
      FLocalBiasUTC: TDateTime ;
      FIcsDNCache: TIcsDomainNameCache;
      function GetAdapters: boolean ;
      procedure ThreadTerminate (Sender: TObject);
      procedure MonDataAvailable (const Header: Ppcap_pkthdr ; const PackPtr: PAnsiChar) ;
      procedure PacketLog(const Msg: String);
  public
      FPcapHandle: PPCAP ;        // control record handle
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure StartMonitor;
      procedure StopMonitor;
      function GetIPAddresses (AdapterName: String; IPList, MaskList, BcastList: TStringList): integer ;
  published
      property AdapterNameList: TStringList read FAdapterNameList ;
      property AdapterDescList: TStringList read FAdapterDescList ;
      property DriverVersion: string    read FDriverVersion ;
      property PacketVersion: string    read FPacketVersion ;
      property IcsDNCache: TIcsDomainNameCache   read FIcsDNCache
                                                 write FIcsDNCache;
      property MonAdapter: String       read FMonAdapter
                                        write FMonAdapter ;
      property Connected: boolean       read FConnected ;
      property LastError: string        read FLastError ;
      property Addr: string             read FAddr
                                        write FAddr ;
      property IgnoreData: boolean      read FIgnoreData
                                        write FIgnoreData ;
      property Promiscuous: boolean     read FPromiscuous
                                        write FPromiscuous ;
      property TotRecvBytes: int64      read FTotRecvBytes ;
      property TotSendBytes: int64      read FTotSendBytes ;
      property TotRecvPackets: integer  read FTotRecvPackets ;
      property TotSendPackets: integer  read FTotSendPackets ;
      property OnPacketEvent: TPacketDataEvent read  FOnPacketEvent
                                               write FOnPacketEvent;
      property OnLogEvent: TPacketLogEvent     read  FOnLogEvent
                                               write FOnLogEvent;
  end;

  TPcapThread = class(TThread)
  private
      FMonitorPcap: TIcsMonPcap ;
      MyHeader: Ppcap_pkthdr ;
      MyPackPtr: PAnsiChar;
      procedure GetPackets ;
      procedure CallThreadEvent ;
  public
      procedure Execute; override;
  end;


implementation

function GetLocalBiasUTC: Integer;
var
    tzInfo : TTimeZoneInformation;
begin
    case GetTimeZoneInformation(tzInfo) of
    TIME_ZONE_ID_STANDARD: Result := tzInfo.Bias + tzInfo.StandardBias;
    TIME_ZONE_ID_DAYLIGHT: Result := tzInfo.Bias + tzInfo.DaylightBias;
    else
        Result := tzInfo.Bias;
    end;
end;

procedure CaptureCallBack (User: Pointer; const Header: Ppcap_pkthdr ; const PackPtr: PAnsiChar) ;
begin
    with TPcapThread (User) do begin
        MyHeader := Header;
        MyPackPtr := PackPtr;
        Synchronize(CallThreadEvent);   // July 2023 must called in context of main thread
    end;
end ;

// July 2023 called in context of main thread for events
procedure TPcapThread.CallThreadEvent;
begin
    FMonitorPcap.MonDataAvailable (MyHeader, MyPackPtr) ;
end;

procedure TPcapThread.GetPackets ;
begin
    Pcap_Read (FMonitorPcap.FPcapHandle, 0, CaptureCallBack, Pointer (Self)) ;
end ;

procedure TPcapThread.Execute;
begin
    if NOT Assigned (FMonitorPcap) then
        Exit ;
    if FMonitorPcap.FPcapHandle = Nil then
        Exit ;
    PacketSetReadTimeout (FMonitorPcap.FPcapHandle.Adapter, 100) ;
    while NOT Terminated do
    begin
        GetPackets ;
    end;
end ;

procedure TIcsMonPcap.ThreadTerminate (Sender: tobject);
begin
    FConnected := false ;
    Pcap_Close (FPcapHandle) ;
    FPcapHandle := Nil ;
end;

constructor TIcsMonPcap.Create(AOwner: TComponent);
begin
    FIgnoreData := false ;
    FAdapterDescList := TStringList.Create ;
    FAdapterNameList := TStringList.Create ;
    FLastError := '' ;
    FPcapHandle := Nil ;
    FConnected := false ;
    FDriverVersion := Pcap_GetDriverVersion ;
    FPacketVersion := Pcap_GetPacketVersion ;
    GetAdapters ;
    if FAdapterNameList.Count <> 0 then
          FMonAdapter := FAdapterNameList [0] ;
    inherited Create(AOwner);
end ;

destructor TIcsMonPcap.Destroy;
begin
    if FConnected then StopMonitor ;
    FreeAndNil (FAdapterNameList) ;
    FreeAndNil (FAdapterDescList) ;
    inherited Destroy;
end ;

procedure TIcsMonPcap.PacketLog(const Msg: String);
begin
    if Assigned(FOnLogEvent) then
        FOnLogEvent(Self, Msg);
end;

function TIcsMonPcap.GetAdapters: boolean ;
var
    total: integer ;
begin
    result := false;
    if NOT Assigned (FAdapterNameList) then
        exit ;
    FAdapterNameList.Clear ;
    FAdapterDescList.Clear ;
    total := Pcap_GetAdapterNamesEx (FAdapterNameList, FAdapterDescList, FLastError) ;
    if total = 0 then
        exit ;
    result := true;
end ;

function TIcsMonPcap.GetIPAddresses (AdapterName: String; IPList, MaskList, BcastList: TStringList): integer ;
var
    IPArray, MaskArray, BcastArray: TIPAddrArray ;  // Feb 2023 now IPv4 and IPv6
    I: integer ;
begin
    IPList.Clear ;
    MaskList.Clear ;
    BcastList.Clear ;
    result := Pcap_GetIPAddresses (AdapterName, IPArray, MaskArray, BcastArray, FLastError) ;
    if result = 0 then exit ;
    for I := 0 to Pred (result) do
    begin
        IPList.Add (WSocketSockAddrToStr (IPArray [I])) ;
        MaskList.Add (WSocketSockAddrToStr (MaskArray [I])) ;
        BcastList.Add (WSocketSockAddrToStr (BcastArray [I])) ;
    end ;
end ;

// called by TIcsFindList for sort and find comparison of file records

function CompareFNext (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal
// and > 0 if Item1 is greater than Item2.
begin
    result := 0 ;
    if longword (Item1) > longword (Item2) then result := 1 ;
    if longword (Item1) < longword (Item2) then result := -1 ;
end ;

// convert seconds since 1 Jan 1970 (UNIX time stamp) to proper Delphi stuff
// and micro seconds

function UnixStamptoDT (stamp: TunixTimeVal): TDateTime ;
begin
    result := ((stamp.tv_Sec / SecsPerDay) + 25569) +
                                    ((stamp.tv_uSec / 1000000) / SecsPerDay) ;
end ;

procedure TIcsMonPcap.MonDataAvailable (const Header: Ppcap_pkthdr ; const PackPtr: PAnsiChar) ;
var
    HdrLen: integer ;
    EthernetHdr: PHdrEthernet ;
    IpHdrv4: PHdrIP;
    IpHdrv6: PHdrIPv6;       // Feb 2023
    IpHdrv6ext: PHdrIPv6Ext; // Feb 2023
    Nextv6Hdr: Byte;         // Feb 2023
    TcpHdr: PHdrTCP;
    UdpHdr: PHdrUDP;
    PacketInfo: TPacketInfo ;  // the data we return in the event
    I: Integer;  // !! TEMP

    procedure GetDataByOffset (offset: integer) ;
    var
        datastart: PAnsiChar ;
    begin
        if FIgnoreData then exit ;
        datastart := PAnsiChar (PAnsiChar (iphdrv4) + offset) ;
        with PacketInfo do
        begin
            if WSocket_ntohs (iphdrv4.tot_len) < (Header.Len - OFFSET_IP) then
                DataLen := WSocket_ntohs (iphdrv4.tot_len) - offset
            else
                DataLen := Header.Len - OFFSET_IP - offset;
            if DataLen <= 0 then exit ;
            SetLength (DataBuf, DataLen) ;  // ANSI
            Move (datastart^, DataBuf [1], DataLen) ;
        end ;
    end;

    procedure GetData6ByOffset (offset: integer) ;
    var
        datastart: PAnsiChar ;
    begin
        if FIgnoreData then exit ;
        datastart := PAnsiChar (PAnsiChar (iphdrv6) + offset) ;
        with PacketInfo do
        begin
            if DataLen <= 0 then exit ;
            SetLength (DataBuf, DataLen) ;  // ANSI
            Move (datastart^, DataBuf [1], DataLen) ;
        end ;
    end;

begin
    FillChar (PacketInfo, Sizeof(PacketInfo), 0) ;
    with PacketInfo do
    begin
        PacketLen := Header.Len ;
        if PacketLen <= 0 then exit ;
    //  PacketLog('RecvBuff: ' + IcsBufferToHex(PackPtr, PacketLen));  // !!! TEMP DIAG
        EthernetHdr := PHdrEthernet (PackPtr) ;
        EtherProto := WSocket_ntohs (EthernetHdr.protocol) ;
        EtherSrc := EthernetHdr.smac ;
        EtherDest := EthernetHdr.dmac ;
        VendorSrc := IcsGetMacVendor(EtherSrc) ;
        VendorDest := IcsGetMacVendor (EtherDest) ;
        SendFlag := CompareMem (@EtherSrc, @FAdapterMac, SizeOf (TMacAddr)) ;
        PacketDT := UnixStamptoDT (Header.ts) + FLocalBiasUTC ; // Unix time stamp correct to local time

     // internet layer IPv4, lots to check
        if EtherProto = ETHERNET_IP then
        begin
            SocketFamily := sfIPv4;
            IpHdrv4 := PHdrIP (PAnsiChar (PackPtr) + OFFSET_IP) ;  // IP header is past ethernet header
            AddrSrc := IpHdrv4.src_addr ;        // 32-bit IP addresses
            AddrDest := IpHdrv4.dest_addr ;
            ProtoType := IpHdrv4.protocol ;   // TCP, UDP, ICMP
            IpTTL := IpHdrv4.ttl;

         // increment global traffic counters
            if SendFlag then
            begin
                inc (FTotSendBytes, packetlen) ;
                inc (FTotSendPackets) ;
            end
            else
            begin
                inc (FTotRecvBytes, packetlen) ;
                inc (FTotRecvPackets) ;
            end ;

        // check protocol and find ports and data
            if Assigned (FOnPacketEvent) then
            begin
                DataBuf := '' ;
                HdrLen := GetIHlen (IpHdrv4^) ;
                if ProtoType = IPPROTO_ICMP then
                begin
                    IcmpType := PByte (PAnsiChar (IpHdrv4) + HdrLen)^ ;
                    GetDataByOffset (HdrLen + 1) ;
                    DispServ := Lowercase (IcsICMPType (IcmpType));
                end
                else
                begin
                    if ProtoType = IPPROTO_TCP then
                    begin
                        TcpHdr := PHdrTCP (PAnsiChar (IpHdrv4) + HdrLen) ;
                        PortSrc := WSocket_ntohs (TcpHdr.source) ;
                        PortDest := WSocket_ntohs (TcpHdr.dest) ;
                        TcpFlags := WSocket_ntohs (TcpHdr.flags) ;
                        TCPCurSeq := WSocket_ntohl (TcpHdr.seq) ;
                        TCPNextSeq := WSocket_ntohl (TcpHdr.ack_seq) ;
                        TCPWinSize := WSocket_ntohs (TcpHdr.window) ;
                        GetDataByOffset (HdrLen + GetTHdoff (TcpHdr^)) ;
                    end;
                    if ProtoType = IPPROTO_UDP then
                    begin
                        UdpHdr := PHdrUDP (PAnsiChar (IpHdrv4) + HdrLen) ;
                        PortSrc := WSocket_ntohs (UdpHdr.src_port) ;
                        PortDest := WSocket_ntohs (UdpHdr.dst_port) ;
                        GetDataByOffset (HdrLen + Sizeof (THdrUDP));
                    end;
                    DispServ := Lowercase (IcsServiceNameEx (PortSrc, PortDest));
                end;
                DispAddrSrc := WSocketIPv4ToStr (AddrSrc) ;   // convert 32-bit IP address into dotted ASCII
                DispAddrDest := WSocketIPv4ToStr (AddrDest) ;
             // start lookup of host names, no event so only get cached name after first time
                 if Assigned(FIcsDNCache) then
                 begin
                    HostNameSrc := FIcsDNCache.LookupIPOne(DispAddrSrc, 0, sfAny, Nil);
                    HostNameDest := FIcsDNCache.LookupIPOne(DispAddrDest, 0, sfAny, Nil);
                 end;
                DispProto := IcsIPProtoName (ProtoType) + 'v4';
                FOnPacketEvent (Self, PacketInfo) ;
            end ;
        end

     // Feb 2023, internet layer IPv6, lots to check
        else if EtherProto = ETHERNET_IPV6 then
        begin
            IpHdrv6 := PHdrIPv6 (PAnsiChar (PackPtr) + OFFSET_IP) ;  // IPv6 header is past ethernet header
            SocketFamily := sfIPv6;
            Addr6Src := IpHdrv6.src_addr ;
            Addr6Dest := IpHdrv6.dest_addr ;
            IpTTL := IpHdrv6.hop_limit;

         // increment global traffic counters
            if SendFlag then
            begin
                inc (FTotSendBytes, packetlen) ;
                inc (FTotSendPackets) ;
            end
            else
            begin
                inc (FTotRecvBytes, packetlen) ;
                inc (FTotRecvPackets) ;
            end ;

        // check protocol and find ports and data
            if Assigned (FOnPacketEvent) then
            begin
                DataBuf := '' ;
                HdrLen := IPV6HDR_LEN ;  // fixed size
                Nextv6Hdr := iphdrv6.next_hdr;

            // skip past multiple extension headers, if any
                while NOT Nextv6Hdr in [NEXTHDR_TCP, NEXTHDR_UDP, NEXTHDR_ICMP, NEXTHDR_NONE] do
                begin
                    IpHdrv6ext := PHdrIPv6Ext (PAnsiChar (IpHdrv6) + HdrLen) ;
                    Nextv6Hdr := IpHdrv6ext.next_hdr;
                    HdrLen := HdrLen + IpHdrv6ext.hdr_ext_len;
                    if HdrLen >= PacketLen then Exit;  // sanity check
                end;
                if Nextv6Hdr = NEXTHDR_ICMP then
                begin
                    ProtoType := IPPROTO_ICMP;
                    IcmpType := PByte (PAnsiChar (IpHdrv6) + HdrLen)^ ;
                    DataLen := WSocket_ntohs (iphdrv6.payload_len) - 1;
                    GetData6ByOffset (HdrLen + 1) ;
                    DispServ := Lowercase (IcsICMPType (IcmpType));
                end
                else
                begin
                    if Nextv6Hdr = NEXTHDR_TCP then
                    begin
                        ProtoType := IPPROTO_TCP;
                        TcpHdr := PHdrTCP (PAnsiChar (IpHdrv6) + HdrLen) ;
                        PortSrc := WSocket_ntohs (TcpHdr.source) ;
                        PortDest := WSocket_ntohs (TcpHdr.dest) ;
                        TcpFlags := WSocket_ntohs (TcpHdr.flags) ;
                        TCPCurSeq := WSocket_ntohl (TcpHdr.seq) ;
                        TCPNextSeq := WSocket_ntohl (TcpHdr.ack_seq) ;
                        TCPWinSize := WSocket_ntohs (TcpHdr.window) ;
                        I := GetTHdoff (TcpHdr^);
                        DataLen := ntohs (iphdrv6.payload_len) - I;
                        GetData6ByOffset (HdrLen + I) ;
                    end;
                    if Nextv6Hdr = NEXTHDR_UDP then
                    begin
                        ProtoType := IPPROTO_UDP;
                        Udphdr := PHdrUDP (PAnsiChar (IpHdrv6) + HdrLen) ;
                        PortSrc := WSocket_ntohs (UdpHdr.src_port) ;
                        PortDest := WSocket_ntohs (UdpHdr.dst_port) ;
                        DataLen := WSocket_ntohs (iphdrv6.payload_len) - Sizeof (THdrUDP);
                        GetData6ByOffset (HdrLen + Sizeof (THdrUDP));
                    end;
                   DispServ := Lowercase (IcsServiceNameEx (PortSrc, PortDest));
               end;
               DispAddrSrc := WSocketIPv6ToStr(Addr6Src);
               DispAddrDest := WSocketIPv6ToStr(Addr6Dest) ;
             // start lookup of host names, no event so only get cached name after first time
                 if Assigned(FIcsDNCache) then
                 begin
                    HostNameSrc := FIcsDNCache.LookupIPOne(DispAddrSrc, 0, sfAny, Nil);
                    HostNameDest := FIcsDNCache.LookupIPOne(DispAddrDest, 0, sfAny, Nil);
                 end;
               DispProto := IcsIPProtoName (ProtoType) + 'v6';
               FOnPacketEvent (Self, PacketInfo) ;
            end;
        end
        else

     // otherwise ARP or something more obscure
        begin
            if SendFlag then
            begin
                inc (FTotSendBytes, packetlen) ;
                inc (FTotSendPackets) ;
            end
            else
            begin
                inc (FTotRecvBytes, packetlen) ;
                inc (FTotRecvPackets) ;
            end ;
            if Assigned (FOnPacketEvent) then
            begin
                DataLen := PacketLen - OFFSET_IP ;
                if (NOT FIgnoreData) and (DataLen > 0) then
                begin
                    SetLength (DataBuf, DataLen) ;
                    Move (PAnsiChar (PAnsiChar (PackPtr) + OFFSET_IP)^, DataBuf [1], DataLen) ;
                end;
                VendorSrc := IcsGetMacVendor(EtherSrc) ;
                VendorDest := IcsGetMacVendor (EtherDest) ;
                DispAddrSrc := VendorSrc + IcsSpace + Lowercase(IcsMacToStr (EtherSrc))  ;
                DispAddrDest := VendorDest + IcsSpace + Lowercase(IcsMacToStr (EtherDest)) ;
                DispProto := IcsEtherProtoName (EtherProto) ;
                DispServ := '';
                FOnPacketEvent (Self, PacketInfo) ;
            end ;
        end ;
    end ;
end ;

procedure TIcsMonPcap.StartMonitor;
var
    snaplen, mins: integer ;
begin
    if (FAdapterNameList.Count = 0) or (FMonAdapter = '') then
    begin
        FLastError := 'No Adaptors Found to Monitor' ;
        exit;
    end;
    if FConnected or (FPcapHandle <> nil) then
    begin
        FLastError := 'PCap Driver Already Running' ;
        exit;
    end;
    FTotRecvBytes := 0 ;
    FTotSendBytes := 0 ;
    FTotRecvPackets := 0 ;
    FTotSendPackets := 0 ;
    mins := GetLocalBiasUTC ;
    if mins < 0 then          // reverse minutes, -60 for GMT summer time
        mins := Abs (mins)
    else
        mins := 0 - mins ;
    FLocalBiasUTC := mins  / (60.0 * 24.0) ;  // keep local time bias

  // open winpcap driver for specific adaptor
    FConnected := false ;
    if FIgnoreData then
        snaplen := DEFAULT_SNAPLEN
    else
        snaplen := 2000 ;
    FPcapHandle := pcap_open_live (FMonAdapter, snaplen, FPromiscuous, 100, FLastError) ;
    if FPcapHandle = nil then exit;
//    Pcap_SetMinToCopy (FPcapHandle, 20000) ;  not sure if this is beneficial
    FAdapterMac := Pcap_GetMacAddress (FPcapHandle, FLastError) ;

  // Start Snoop Read Thread
    FPcapThread := TPcapThread.Create (true) ;
    FPcapThread.FMonitorPcap := Self ;
    FPcapThread.OnTerminate := ThreadTerminate ;
    FPcapThread.FreeOnTerminate := false;
{$IFDEF COMPILER14_UP}
    FPcapThread.Start;    { V8.71 }
{$ELSE}
    FPcapThread.Resume;
{$ENDIF}
    FConnected := true;
end ;

procedure  TIcsMonPcap.StopMonitor;
begin
    FConnected := false ;

  // stop thread
    if Assigned (FPcapThread) then
    begin
        FPcapThread.Terminate ;
        FPcapThread.WaitFor ;
        FPcapThread.Free ;
        FPcapThread := nil ;
    end ;
    if Assigned (FPcapHandle) then
    begin
        Pcap_Close (FPcapHandle) ;
        FPcapHandle := Nil ;
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
