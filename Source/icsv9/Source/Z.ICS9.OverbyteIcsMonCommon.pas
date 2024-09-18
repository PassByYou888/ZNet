{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Internet monitoring using raw sockets or Npcap NDIS driver, common
              headers and structures and TIcsTrafficClass.
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
Layer 2 - Data Link Layer - THdrEthernet (14 bytes) (MAC addresses), tunnels, PPP
Layer 3 - Network Layer - THdrIP (20 bytes), THdrIPv6 (40+ bytes), ARP, ICMP (IP addresses)
Layer 4 - Transport Layer - THdrTcp (20 bytes), THdrUdp (8 bytes), QUIC(?) (IP ports)
Layer 5 - Session Layer - Sockets (handles)
Layer 6 - Presentation Layer - TLS/SSL, MIME, Telnet
Layer 7 - Application Layer - HTTP, FTP, SMTP, POP3, etc


Some of the TCP/IP headers are taken from 'Hands-On TCP/IP Programming' by Alfred
Mirzagotov in The Delphi Magazine January 2004.

8 Aug 2008 - 1.2
Updated to support ICS V6 and V7, and Delphi 2009

Jul 21, 2023 - V8.71 Updated units for main ICS library.
                     Added TIcsMonFilterClass to filter traffic on protocols or IP addresses.
                     TIcsTrafficClass uses IcsDomainNameCache component instead of
                       internal wsocket reverse IP lookups.
Aug 08, 2023 V9.0  Updated version to major release 9.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Z.ICS9.OverbyteIcsMonCommon;

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
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    Z.ICS9.OverbyteIcsWinsock,
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsDnsQuery,
    Z.ICS9.OverbyteIcsUtils,
    Z.ICS9.OverbyteIcsIpUtils;

const
    CopyRight    : String     = ' TIcsTrafficClass  (c) 2023 V9.0 ';

const
  sTrafficMask = '%-50s %-50s %-12s %5s %7s %5s %7s %8s %8s' ;
//               pc09.magenta                                       ermintrude.digitalspy.co.uk                        www-http     1.51K    [10] 6.94K    [10] 19:54:17 19:54:27
  sTrafficHdr = 'Local IP                                           Remote IP                                          Service      Sent [packet] Recv [packet] First    Last' ;
  sServiceMask = '%-12s %5s %7s %5s %7s %5s' ;
//               www-http     1.51K    [10] 6.94K    [10]    22
  sServiceHdr = 'Service      Sent [packet] Recv [packet] Hosts' ;

  MaxDnsLookupAttempts = 6 ;  // total for both addresses
  InitialTrafficSize = 100 ;

type
// record used to return packet to application for both raw sockets and winpcap
    TPacketInfo = record
        PacketLen: integer ;   // total length of packet including network interface layer
        EtherProto: word ;     // ethernet protocol
        EtherSrc: TMacAddr ;   // ethernet MAC addresses
        EtherDest: TMacAddr ;
        VendorSrc: String ;    // ethernet MAC vendors
        VendorDest: String ;
        SocketFamily: TSocketFamily;  // sfIPv4 or sfIPv6
        AddrSrc: TIcsIPv4Address ;    // IPv4 addresses are 32-bit binary
        AddrDest: TIcsIPv4Address ;
        DispAddrSrc: String;          // IPv4, IPv6 or MAC for display
        Addr6Src: TIcsIPv6Address;    // IPv6 128-bit binary for display
        Addr6Dest: TIcsIPv6Address;
        DispAddrDest: String;         // IPv4, IPv6 or MAC
        HostNameSrc: String;
        HostNameDest: String;
        PortSrc: integer ;     // transport layer ports
        PortDest: integer ;
        ProtoType: byte ;      // transport layer protocol
        DispProto: String;     // protocol for for display
        DispServ: String;      // service name for display
        IpTTL: Integer;        // IP time to live
        TcpFlags: Word ;       // TCP packet type flags
        TCPCurSeq: DWORD;      // TCP current packet sequence
        TCPNextSeq: DWORD;     // TCP next packet sequence
        TCPWinSize: Word;      // TCP window size
        SendFlag: boolean ;    // true if packet being sent from our MAC address (or IP, less reliable)
        IcmpType: byte ;       // ICMP packet type
        DataLen: integer ;     // length of data (less headers)
        DataBuf: AnsiString ;  // packet data (may be blank even if datalen<>0)
        PacketDT: TDateTime ;  // when packet was captured
  end ;

  TPacketDataEvent = procedure (Sender: TObject; PacketInfo: TPacketInfo) of object;
  TPacketLogEvent = procedure (Sender: TObject; const Msg: string) of object;

// record used for maintaining traffic statistics
    TTrafficInfo = packed record  // first eight elements are used for sorting, keep together and packed
        SrcAddr6: TIcsIPv6Address;  // IPv6 16 bytes binary , zeros will sort first so IPv4 addresses shown first
        DestAddr6: TIcsIPv6Address;
        SrcAddr: LongWord ;  // IPv4 addresses are 4 bytes binary - swapped bytes for sorting
        DestAddr: LongWord ;
        ServPort: Word ;    // service port
        PackType: Word ;    // protocol or packet type, TCP, UDP, ARP, ICMP, etc  - 44 bytes to here
        SocFamily: TSocketFamily;  // sfIPv4 or sfIPv6
        SrcDispAddr: String;          // IPv4, IPv6 or MAC for display
        DestDispAddr: String;         // IPv4, IPv6 or MAC for display
        SrcHostName: string ;         // host domains for IP addresses, if available
        DestHostName: string ;
        ServName: string ;
        BytesSent: int64 ;     // traffic
        BytesRecv: int64 ;
        PacksSent: integer ;
        PacksRecv: integer ;
        LookupAttempts: integer ; // how many host name lookup attempts
        FirstDT: TDateTime ;   // when this traffic started
        LastDT: TDateTime ;    // last traffic update
  end ;
  PTrafficInfo = ^TTrafficInfo ;

  TServiceInfo = packed record  // first two elements are used for sorting, keep together and packed
        ServPort: word ;    // service port
        PackType: word ;    // protocol or packet type, TCP, UDP, ARP, ICMP, etc  - 4 bytes to here
        ServName: string ;     // looked up
        TotalHosts: integer;   // how many different hosts for this service
        BytesSent: int64 ;     // traffic
        BytesRecv: int64 ;
        PacksSent: integer ;
        PacksRecv: integer ;
  end ;
  PServiceInfo = ^TServiceInfo ;

const
    TrafficIPCompLen = 44;
    ServiceCompLen = 4 ;

// rows for display of one captured data packet in a window
    ColCapDataTime = 0 ;
    ColCapDataPlen = 1 ;
    ColCapDataEProto = 2 ;
    ColCapDataMacAddSrc = 3 ;
    ColCapDataMacVenSrc = 4 ;
    ColCapDataMacAddTar = 5 ;
    ColCapDataMacVenTar = 6 ;
    ColCapDataIpTTL = 7 ;
    ColCapDataSocFam = 8 ;
    ColCapDataAddrSrc = 9 ;
    ColCapDataHostSrc = 10 ;
    ColCapDataAddrDes = 11 ;
    ColCapDataHostDes = 12 ;
    ColCapDataTrans = 13 ;
    ColCapDataPortSrc = 14 ;
    ColCapDataPortDes = 15 ;
    ColCapDataServ = 16 ;
    ColCapDataTFlag = 17 ;
    ColCapDataTSeq = 18 ;
    ColCapDataTASeq = 19 ;
    ColCapDataTWin = 20 ;
    ColCapDataIcmp = 21 ;
    ColCapDataDLen = 22 ;
    ColCapDataData = 23 ;
    ColCapDataLast = 23 ;

var
    HdrsCapData: array[0..ColCapDataLast] of String = (
        'Time Stamp',
        'Packet Length',
        'Ethernet Protocol',
        'MAC Addr Source',
        'MAC Vendor Source',
        'MAC Addr Destination',
        'MAC Vendor Dest',
        'IP TTL',
        'Socket Family',
        'Address Source',
        'Host Name Source',
        'Address Destination',
        'Host Name Destination',
        'Transport Layer',
        'Port Source',
        'Port Destination',
        'Service',
        'TCP Flags',
        'TCP Seq',
        'TCP Ack Seq',
        'TCP Windows',
        'ICPM Type',
        'Data Length',
        'Data'
         );

// class used for maintaining traffic statistics
type
  TIcsTrafficClass = class(TComponent)
  protected
    { Protected declarations }
      FTrafficInfo: array of TTrafficInfo ;
      FServiceInfo: array of TServiceInfo ;
      FTrafficList: TIcsFindList ;
      FServiceList: TIcsFindList ;
      FTotTraffic: integer ;
      FTotService: integer ;
      FIcsDNCache: TIcsDomainNameCache;
      procedure DNUpdateEvent(Sender: TObject; ItemNr: Integer);
  public
    { Public declarations }
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure Clear ;
      procedure Add (PacketInfo: TPacketInfo) ;
      procedure UpdateService ;
      function GetUnSortTraf (item: integer): PTrafficInfo ;
      function GetSortedTraf (item: integer): PTrafficInfo ;
      function FmtTrafStr (TrafficRec: PTrafficInfo; HostNames: Boolean = False): string ;
      function GetFmtTrafStr (item: integer; HostNames: Boolean = False): string ;
      function GetSortedServ (item: integer): PServiceInfo ;
      function GetFmtServStr (item: integer): string ;
      function GetTotals: TServiceInfo ;
  published
      property TotTraffic: integer          read FTotTraffic ;
      property TotService: integer          read FTotService ;
      property IcsDNCache: TIcsDomainNameCache   read FIcsDNCache write FIcsDNCache;
  end;

  TMonFilterMeth = (MonFilterNone, MonFilterIgnore, MonFilterAllow);

  TIcsMonFilterClass = class
  private
  public
    FilterIpAddr: TMonFilterMeth;
    FilterProtocol: TMonFilterMeth;
    IPv4List: TIcsFindList;
    IPv6List: TIcsFindList;
    IPv6ItemsArray: array of TIcsIPv6Address;
    LocalIpAddr: Boolean;
    PortList: TIcsFindList;
    ProtoARP: Boolean;
    ProtoBroadcast: Boolean;
    ProtoDns: Boolean;
    ProtoHttp: Boolean;
    ProtoICMP: Boolean;
    ProtoIPv4: Boolean;
    ProtoIPv6: Boolean;
    ProtoIRC: Boolean;
    ProtoNonIp: Boolean;
    ProtoSNMP: Boolean;
    ProtoSyslog: Boolean;
    ProtoTCP: Boolean;
    ProtoUPnP: Boolean;
    ProtoUdp: Boolean;
    constructor Create;
    destructor Destroy; override;
    function AllowPacket (PacketInfo: TPacketInfo): Boolean;
    procedure SetIpAddr (IpAddr: String);
    procedure SetPort (Port: String);
    procedure ClearLists;
  end;


implementation

// called by TIcsFindList for sort and find comparison of traffic records
// sort is by source IP, then dest IP, then ServPort, then PackType

function CompareIPTraffic (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal
// and > 0 if Item1 is greater than Item2.
begin
    result := CompareGTMem (Item1, Item2, TrafficIPCompLen) ;  // warning record must be packed
end ;

function CompareServTraffic (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal
// and > 0 if Item1 is greater than Item2.
begin
    result := CompareGTMem (Item1, Item2, ServiceCompLen) ;  // warning record must be packed
end ;

constructor TIcsTrafficClass.Create(AOwner: TComponent);
begin
    SetLength (FTrafficInfo, InitialTrafficSize) ;
    FTrafficList := TIcsFindList.Create ;
    FTrafficList.Sorted := true ;
    FTrafficList.Capacity := InitialTrafficSize ;
    FTotTraffic := 0 ;
    SetLength (FServiceInfo, 0) ;
    FServiceList := TIcsFindList.Create ;
    FServiceList.Sorted := true ;
    FTotService := 0 ;
end;

destructor TIcsTrafficClass.Destroy;
begin
    Clear ;
    SetLength (FTrafficInfo, 0) ;
    FreeAndNil (FTrafficList) ;
    SetLength (FServiceInfo, 0) ;
    FreeAndNil (FServiceList) ;
end;

procedure TIcsTrafficClass.Clear ;
begin
    SetLength (FTrafficInfo, InitialTrafficSize) ;
    FTrafficList.Clear ;
    FTotTraffic := 0 ;
    SetLength (FServiceInfo, 0) ;
    FServiceList.Clear ;
    FTotService := 0 ;
end;

procedure TIcsTrafficClass.Add (PacketInfo: TPacketInfo) ;
var
    NewTraffic: TTrafficInfo ;
    TrafficRec: PTrafficInfo ;
    recnr, I: integer ;
begin
    FillChar (NewTraffic, Sizeof(NewTraffic), 0) ;
    with NewTraffic, PacketInfo do
    begin
        if NOT ((EtherProto = ETHERNET_IP) or (EtherProto = ETHERNET_IPV6)) then
            exit ;
        if NOT (ProtoType in [IPPROTO_TCP, IPPROTO_UDP, IPPROTO_ICMP]) then
            exit ;
        PackType := ProtoType ;
        SocFamily := SocketFamily;
        ServName := DispServ;
        if SendFlag then
        begin
            SrcAddr := LongWord(AddrSrc) ;
            DestAddr := LongWord(AddrDest) ;
            SrcAddr6 := Addr6Src ;
            DestAddr6 := Addr6Dest ;
            ServPort := PortDest ;
            BytesSent := PacketLen ;
            PacksSent := 1 ;
            SrcDispAddr := DispAddrSrc;
            DestDispAddr := DispAddrDest;
            SrcHostName := HostNameSrc;
            DestHostName := HostNameDest;
        end
        else
        begin
            SrcAddr := LongWord(AddrDest) ;
            DestAddr := LongWord(AddrSrc) ;
            SrcAddr6 := Addr6Dest ;
            DestAddr6 := Addr6Src ;
            ServPort := PortSrc ;
            BytesRecv := PacketLen ;
            PacksRecv := 1 ;
            SrcDispAddr := DispAddrDest;
            DestDispAddr := DispAddrSrc;
            SrcHostName := HostNameDest;
            DestHostName := HostNameSrc;
        end ;

     // IPv6 don't attempt reverse DNS of local IP addresses
     {   if SocketFamily = sfIPv6 then
        begin
            if IcsIsLocalIPv6(SrcAddr6) then
                SrcHostName := SrcDispAddr;
            if IcsIsLocalIPv6(DestAddr6) then
                DestHostName := DestDispAddr;
        end;  }
        if ProtoType = IPPROTO_ICMP then
        begin
            ServPort := IcmpType ;
            if ServPort = 0 then
                ServPort := 8 ;  // change echo-reply to echo (ie ping)
        end
        else
        begin
          // try and work out which is the server port, generally lower numbers
            if (ServPort >= 10000) and (PortSrc < 10000) then
                ServPort := PortSrc
            else if (ServPort >= 10000) and (PortDest < 10000) then
                ServPort := PortDest
        end ;
        LastDT := PacketDT ;
    end ;

  // see if only got a record for this traffic, update it
    if FTrafficList.Find (@NewTraffic, CompareIPTraffic, recnr) then
    begin
        TrafficRec := FTrafficList [recnr] ;
        if NOT Assigned (TrafficRec) then exit ;  // sanity check
        if CompareMem (TrafficRec, @NewTraffic, TrafficIPCompLen) then // double check for correct record
        begin
            inc (TrafficRec^.BytesSent, NewTraffic.BytesSent) ;
            inc (TrafficRec^.PacksSent, NewTraffic.PacksSent) ;
            inc (TrafficRec^.BytesRecv, NewTraffic.BytesRecv) ;
            inc (TrafficRec^.PacksRecv, NewTraffic.PacksRecv) ;
            TrafficRec^.LastDT := NewTraffic.LastDT ;
            exit ;
        end ;
    end ;

  // otherwise add a new traffic record
    if Length (FTrafficInfo) <= FTotTraffic then
    begin
        SetLength (FTrafficInfo, FTotTraffic * 2) ;  // allocate more records in dynamic array
      // must rebuild pointer list since resized array may have moved in memory
        FTrafficList.Clear ;
        FTrafficList.Capacity := FTotTraffic * 2 ;
        for I := 0 to Pred (FTotTraffic) do
            FTrafficList.Add (@FTrafficInfo [I]) ;
        FTrafficList.Sort (CompareIPTraffic) ;
    end ;
    NewTraffic.FirstDT := NewTraffic.LastDT ;
    FTrafficInfo [FTotTraffic] := NewTraffic ;
    recnr := FTrafficList.AddSorted (@FTrafficInfo [FTotTraffic], CompareIPTraffic) ;
    inc (FTotTraffic) ;

 // start lookup of host names if not got them yet
     if Assigned(FIcsDNCache) then
     begin
         TrafficRec := FTrafficList [recnr];
        with TrafficRec^ do begin
            if SrcHostName = SrcDispAddr then  // still got an IP address
                SrcHostName := FIcsDNCache.LookupIPOne(SrcDispAddr, MakeLong(recnr, 1), sfAny, DNUpdateEvent);
            if DestHostName = DestDispAddr then  // still got an IP address
                DestHostName := FIcsDNCache.LookupIPOne(DestDispAddr, MakeLong(recnr, 2), sfAny, DNUpdateEvent);
        end;
     end;
end ;

procedure TIcsTrafficClass.DNUpdateEvent(Sender: TObject; ItemNr: Integer);
var
    recnr, id: Integer;
    TrafficRec: PTrafficInfo ;
    MyCache: TIcsDomainNameCache;
begin
    MyCache := (Sender as TIcsDomainNameCache);
    with MyCache.GetDNItem(ItemNr) do begin
        recnr := LongRec(ReqTag).Lo;
        id := LongRec(ReqTag).Hi;
        if recnr >= FTrafficList.Count then
            Exit;
        if MyCache.GetDNItem(ItemNr).DNState <> StateOK then
           Exit;
        TrafficRec := FTrafficList [recnr] ;
        if (id = 1) and (TrafficRec^.SrcDispAddr = MyCache.GetDNItem(ItemNr).Request) then
            TrafficRec^.SrcHostName := MyCache.BuildRespList(ItemNr);
        if (id = 2) and (TrafficRec^.DestDispAddr = MyCache.GetDNItem(ItemNr).Request) then
            TrafficRec^.DestHostName := MyCache.BuildRespList(ItemNr);
    end;
end;


function TIcsTrafficClass.GetUnSortTraf (item: integer): PTrafficInfo ;
begin
    if item < FTotTraffic then
        Result := @FTrafficInfo [item]
    else
        Result := Nil ;
end;

function TIcsTrafficClass.GetSortedTraf (item: integer): PTrafficInfo ;
begin
    if item < FTotTraffic then
        Result := FTrafficList [item]
    else
        Result := Nil ;
end;

function TIcsTrafficClass.FmtTrafStr (TrafficRec: PTrafficInfo; HostNames: Boolean = False): string ;
var
    disploc, disprem, dispserv: string ;
begin
    result := '' ;
    if NOT Assigned (TrafficRec) then
        Exit ;  // sanity check
    with TrafficRec^ do
    begin
        disploc := SrcDispAddr;
        disprem :=  DestDispAddr;
        if HostNames then
        begin
            if SrcHostName <> '' then
                disploc := SrcHostName ;
            if DestHostName <> '' then
                disprem := DestHostName ;
        end;
        dispserv := ServName;
        result := Format (sTrafficMask, [disploc, disprem, dispserv, IntToKbyte (BytesSent), '[' + IntToKbyte (PacksSent) + ']',
                IntToKbyte (BytesRecv), '[' + IntToKbyte (PacksRecv) + ']', TimeToStr (FirstDT), TimeToStr (LastDT)  ]) ;
    end ;
end;

function TIcsTrafficClass.GetFmtTrafStr (item: integer; HostNames: Boolean = False): string ;
begin
    result := '' ;
    if item >= FTotTraffic then
        Exit ;
    Result := FmtTrafStr (FTrafficList [item], HostNames);
end;

procedure TIcsTrafficClass.UpdateService ;
var
    I, recnr: integer ;
    NewService: TServiceInfo ;
    ServiceRec: PServiceInfo ;

   procedure RebuildList ;
   var
        J: integer ;
   begin
        FServiceList.Clear ;
        for J := 0 to Pred (FTotService) do
            FServiceList.Add (@FServiceInfo [J]) ;
        FServiceList.Sort (CompareServTraffic) ;
   end ;

begin
    FServiceList.Clear ;
    FTotService := 0 ;
    if FTotTraffic = 0 then
    begin
        SetLength (FServiceInfo, 0) ;
        exit ;
    end ;
    SetLength (FServiceInfo, InitialTrafficSize) ;
    FServiceList.Capacity := InitialTrafficSize ;

// add total record
    FillChar (NewService, Sizeof(NewService), 0) ;
    NewService.ServName := 'TOTALS' ;
    FServiceInfo [FTotService] := NewService ;
    FServiceList.Add (@FServiceInfo [FTotService]) ;
    FTotService := 1 ;
    for I := 0 to Pred (FTotTraffic) do
    begin
        FillChar (NewService, Sizeof(NewService), 0) ;
        NewService.ServPort := FTrafficInfo [I].ServPort ;
        NewService.PackType := FTrafficInfo [I].PackType ;
        NewService.ServName := FTrafficInfo [I].ServName ;
        NewService.BytesSent := FTrafficInfo [I].BytesSent ;
        NewService.BytesRecv := FTrafficInfo [I].BytesRecv ;
        NewService.PacksSent := FTrafficInfo [I].PacksSent ;
        NewService.PacksRecv := FTrafficInfo [I].PacksRecv ;
        NewService.TotalHosts := 1 ;

     // increment totals
        inc (FServiceInfo [0].BytesSent, NewService.BytesSent) ;
        inc (FServiceInfo [0].PacksSent, NewService.PacksSent) ;
        inc (FServiceInfo [0].BytesRecv, NewService.BytesRecv) ;
        inc (FServiceInfo [0].PacksRecv, NewService.PacksRecv) ;
        inc (FServiceInfo [0].TotalHosts) ;

    // see if updating existing record
        if FServiceList.Find (@NewService, CompareServTraffic, recnr) then
        begin
            ServiceRec := FServiceList [recnr] ;
            if NOT Assigned (ServiceRec) then continue ; // sanity check
            if CompareMem (ServiceRec, @NewService, ServiceCompLen) then // double check for correct record
            begin
                inc (ServiceRec^.BytesSent, NewService.BytesSent) ;
                inc (ServiceRec^.PacksSent, NewService.PacksSent) ;
                inc (ServiceRec^.BytesRecv, NewService.BytesRecv) ;
                inc (ServiceRec^.PacksRecv, NewService.PacksRecv) ;
                inc (ServiceRec^.TotalHosts) ;
                continue ;    // next record
            end ;
        end ;

      // otherwise add a new service record
        if Length (FServiceInfo) <= FTotService then
        begin
            SetLength (FServiceInfo, FTotService * 2) ;  // allocate more records in dynamic array
          // must rebuild pointer list since resized array may have moved in memory
            FServiceList.Clear ;
            FServiceList.Capacity := FTotService * 2 ;
            RebuildList ;
        end ;
        FServiceInfo [FTotService] := NewService ;
        FServiceList.AddSorted (@FServiceInfo [FTotService], CompareServTraffic) ;
        inc (FTotService) ;
    end ;
    SetLength (FServiceInfo, FTotService) ;
    RebuildList ;     // keep Delphi 2006 happy
end ;

function TIcsTrafficClass.GetSortedServ (item: integer): PServiceInfo ;
begin
    if item < FTotService then
        result := @FServiceInfo [item]
    else
        FillChar (result, Sizeof(result), 0) ;
end ;

// get one service record as a formatted string for display
function TIcsTrafficClass.GetFmtServStr (item: integer): string ;
var
    ServiceRec: PServiceInfo ;
begin
    result := '' ;
    if item >= FTotService then exit ;
    if FServiceList [0] <> @FServiceInfo [0] then  // sanity check
    begin
        result := 'Dynamic Array Memory Error' ;
        exit ;
    end;
    ServiceRec := FServiceList [item] ;
    if NOT Assigned (ServiceRec) then
        exit ;  // sanity check
    with ServiceRec^ do
    begin
        result := Format (sServiceMask, [ServName,  IntToKbyte (BytesSent), '[' + IntToKbyte (PacksSent) + ']',
                             IntToKbyte (BytesRecv), '[' + IntToKbyte (PacksRecv) + ']', IcsIntToCStr (TotalHosts)]) ;
    end ;
end ;

// total all traffic records

function TIcsTrafficClass.GetTotals: TServiceInfo ;
var
    I: integer ;
begin
    FillChar (result, Sizeof(result), 0) ;
    if FTotTraffic = 0 then exit ;
    for I := 0 to Pred (FTotTraffic) do
    begin
        inc (result.BytesSent, FTrafficInfo [I].BytesSent) ;
        inc (result.BytesRecv, FTrafficInfo [I].BytesRecv) ;
        inc (result.PacksSent, FTrafficInfo [I].PacksSent) ;
        inc (result.PacksRecv, FTrafficInfo [I].PacksRecv) ;
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

constructor TIcsMonFilterClass.Create;
begin
    IPv4List := TIcsFindList.Create ;
    IPv4List.Sorted := true ;
    IPv6List := TIcsFindList.Create ;
    IPv6List.Sorted := true ;
    PortList := TIcsFindList.Create ;
    PortList.Sorted := true ;
end;

destructor TIcsMonFilterClass.Destroy;
begin
    ClearLists;
    FreeAndNil (IPv4List) ;
    FreeAndNil (IPv6List) ;
    FreeAndNil (PortList) ;
end;

procedure TIcsMonFilterClass.ClearLists;
begin
    IPv4List.Clear ;
    IPv6List.Clear ;
    SetLength (IPv6ItemsArray, 0);
end;

// called by TIcsFindList for sort and find comparison of file records

function IPv4CompFNext (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal
// and > 0 if Item1 is greater than Item2.
begin
    result := 0 ;
    if longword (Item1) > longword (Item2) then result := 1 ;
    if longword (Item1) < longword (Item2) then result := -1 ;
end ;

function IPv6CompFNext (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal
// and > 0 if Item1 is greater than Item2.
begin
    Result := CompareGTMem(Item1, Item2, SizeOf(TIcsIPv6Address)) ;
end ;

procedure TIcsMonFilterClass.SetIpAddr (IpAddr: String);
var
    ASocketFamily: TSocketFamily;
    InIPv4Addr: TIcsIPv4Address;
    InIPv6Addr: TIcsIPv6Address;
    Success: Boolean;
    iploc: Integer;
begin
    if NOT WSocketIsIP(Trim(IPAddr), ASocketFamily) then
        Exit;
    if ASocketFamily = sfIPv4 then
    begin
        InIPv4Addr := WSocketStrToIPv4 (Trim(IPAddr), Success);
        IPv4List.AddSorted (Pointer (InIPV4Addr), @IPv4CompFNext) ;
    end
    else if ASocketFamily = sfIPv6 then
    begin
        InIPv6Addr := WSocketStrToIPv6 (Trim(IPAddr), Success);
        if IPv6List.Find (@InIPv6Addr, @IPv6CompFNext, iploc) then
            Exit;
        if Length (IPv6ItemsArray) = 0 then
            SetLength(IPv6ItemsArray, 32);
        if Length (IPv6ItemsArray) < IPv6List.Count then
            SetLength(IPv6ItemsArray, Length (IPv6ItemsArray) * 2);
        IPv6ItemsArray[IPv6List.Count] := InIPv6Addr;
        IPv6List.AddSorted (@IPv6ItemsArray[IPv6List.Count], @IPv6CompFNext) ;
    end;
end;

procedure TIcsMonFilterClass.SetPort (Port: String);
var
    MyPort: Integer;
begin
    MyPort := atoi(Trim(Port));
    if MyPort = 0 then
        Exit;
    PortList.AddSorted (Pointer (MyPort), @IPv4CompFNext) ;
end;


function TIcsMonFilterClass.AllowPacket (PacketInfo: TPacketInfo): Boolean;
var
    Match: Boolean;
    IpLoc: Integer;
    AddrLongSrc, AddrLongDest: LongWord;
begin
    Result := True;
    if FilterProtocol > MonFilterNone then
    begin
        Match := False;
        with PacketInfo do
        begin
            if ProtoARP and (EtherProto = ETHERNET_ARP) then
                Match := True;
            if ProtoIPv4 and (EtherProto = ETHERNET_IP) then
                Match := True;
            if ProtoIPv6 and (EtherProto = ETHERNET_IPV6) then
                Match := True;
            if ProtoSNMP and (EtherProto = ETHERNET_SNMP) then
                Match := True;
            if ((EtherProto = ETHERNET_IP) or (EtherProto = ETHERNET_IPV6)) then
            begin
                if ProtoICMP and (ProtoType = IPPROTO_ICMP) then
                    Match := True;
                if ProtoTCP and (ProtoType = IPPROTO_TCP) then
                    Match := True;
                if ProtoUDP and (ProtoType = IPPROTO_UDP) then
                    Match := True;
                if ProtoUPnP and ((PortSrc = 1900) or (PortDest = 1900)) then
                    Match := True;
                if ProtoIRC and ((PortSrc = 6667) or (PortDest = 6667)) then
                    Match := True;
                if ProtoSyslog and ((PortSrc = 514) or (PortDest = 514)) then
                    Match := True;
                if ProtoHttp and ((PortSrc = 80) or (PortDest = 80) or (PortSrc = 443) or (PortDest = 443)) then
                    Match := True;
                if ProtoSNMP and ((PortSrc = 161) or (PortDest = 161) or (PortSrc = 162) or (PortDest = 162)) then
                    Match := True;
                if ProtoDns and ((PortSrc = 53) or (PortDest = 53) or (PortSrc = 137) or (PortDest = 137)) then
                    Match := True;
                if ProtoDns and ((PortSrc >= 5352) and (PortSrc <= 5355)) or ((PortDest >= 5352) and (PortDest <= 5355)) then
                    Match := True;
                if ProtoBroadcast then
                begin
                    AddrLongDest := LongWord(WSocket_ntohl(AddrDest));
                    if (((AddrLongDest >= IP224_0_0_0) and (AddrLongDest <= IP239_255_255_255)) or (AddrLongDest = INADDR_BROADCAST)) then
                       Match := True;
                    if (ntohs(Addr6Dest.Words[0]) >= IP6_FF00) then
                       Match := True;
                end;
                if (PortList.Count > 0) then
                begin
                    if PortList.Find (Pointer (PortSrc), @IPv4CompFNext, iploc) then
                        Match := True;
                    if PortList.Find (Pointer (PortDest), @IPv4CompFNext, iploc) then
                        Match := True;
                end;
            end
            else if ProtoNonIp then
                Match := True;
        end;
        if (FilterProtocol = MonFilterAllow) and (NOT Match) then
            Result := False;
        if (FilterProtocol = MonFilterIgnore) and Match then
            Result := False;
    end;
    if FilterIpAddr > MonFilterNone then
    begin
        Match := False;
        with PacketInfo do
        begin
            if IPv4List.Count > 0 then
            begin
                if (AddrSrc <> 0) and (IPv4List.Find (Pointer (AddrSrc), @IPv4CompFNext, iploc)) then
                   Match := True;
                if (AddrDest <> 0) and (IPv4List.Find (Pointer (AddrDest), @IPv4CompFNext, iploc)) then
                   Match := True;
            end;
            if IPv6List.Count > 0 then
            begin
                if (Addr6Src.Words[0] <> 0) and (IPv6List.Find (@Addr6Src, @IPv6CompFNext, iploc)) then
                   Match := True;
                if (Addr6Dest.Words[0] <> 0) and (IPv6List.Find (@Addr6Dest, @IPv6CompFNext, iploc)) then
                   Match := True;
            end;

            if LocalIpAddr then
            begin
                AddrLongSrc := LongWord(WSocket_ntohl(AddrSrc)); // must swap bytes to compare range
                AddrLongDest := LongWord(WSocket_ntohl(AddrDest));

            // look for private or local IP ranges
                if (AddrSrc <> 0) then
                begin
                    if ((AddrLongSrc >= IP10_0_0_0) and (AddrLongSrc <= IP10_255_255_255)) and
                              ((AddrLongDest >= IP10_0_0_0) and (AddrLongDest <= IP10_255_255_255)) then
                        Match := True;
                    if ((AddrLongSrc >= IP100_64_0_0) and (AddrLongSrc <= IP100_127_255_255)) and
                              ((AddrLongDest >= IP100_64_0_0) and (AddrLongDest <= IP100_127_255_255)) then    // carrier NAT
                        Match := True;
                    if ((AddrLongSrc >= IP127_0_0_0) and (AddrLongSrc <= IP127_255_255_255)) and
                              ((AddrLongDest >= IP127_0_0_0) and (AddrLongDest <= IP127_255_255_255)) then
                        Match := True;
                    if ((AddrLongSrc >= IP169_254_1_0) and (AddrLongSrc <= IP169_254_254_255)) and
                              ((AddrLongDest >= IP169_254_1_0) and (AddrLongDest <= IP169_254_254_255)) then
                        Match := True;
                    if ((AddrLongSrc >= IP192_168_0_0) and (AddrLongSrc <= IP192_168_255_255)) and
                              ((AddrLongDest >= IP192_168_0_0) and (AddrLongDest <= IP192_168_255_255)) then
                        Match := True;
                end;
                if ((Addr6Src.Words[0] = IP6_FE80) and (Addr6Dest.Words[0] = IP6_FE80)) then    // link local
                   Match := True;

            // look for broadcast and multicast addresses
                if (((AddrLongDest >= IP224_0_0_0) and (AddrLongDest <= IP239_255_255_255)) or (AddrLongDest = INADDR_BROADCAST)) then
                   Match := True;
                if (ntohs(Addr6Dest.Words[0]) >= IP6_FF00) then
                   Match := True;

            // non-IP traffic must be local
                if (AddrSrc = 0) and (Addr6Src.Words[0] = 0) and (AddrDest = 0) and (Addr6Dest.Words[0] = 0) then
                   Match := True;

            end;
        end;
        if (FilterIpAddr = MonFilterAllow) and (NOT Match) then
            Result := False;
        if (FilterIpAddr = MonFilterIgnore) and Match then
            Result := False;
    end;

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
