{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Internet monitoring using raw sockets, TIcsMonSocket component.
Creation:     2005
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




Note this component uses RAW sockets, which are only available in Windows 2000
and later, and only for administrator level users.

Microsoft is also restricting the use of RAW sockets in recent service packs,
XP SP2 stops raw sockets being used to send data but receive still works.

8 Aug 2008 - 1.2
Updated to support ICS V6 and V7, and Delphi 2009.

26 Nov 2018 - 1.4
No changes, testing with ICS V8.

Jul 27, 2023 - V8.71 Updated units for main ICS library.
                     Added IPv6 support for raw sockets.  Beware the Microsoft IPv6
                       implementation is compromised compared to IPv4 by not supplying
                       the IPv6 IP layer header packets, only the subsequent transport
                       packet but with no reliable way to tell if that packet is TCP,
                       UDP or ICMP.  We can guess UDP by checking the packet length,
                       but not easily ICMP from TCP.  Also the local source address
                       would have been in the IPv6 header so we don't know is packets
                       are being sent or received reliably.
                    Filtering of packets no longer handled here, but in the event.
                    TIcsMonSocket uses IcsDomainNameCache component to lookup source
                      and destination host names.
Aug 08, 2023 V9.0  Updated version to major release 9.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Z.ICS9.OverbyteIcsMonSock;

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
    Z.ICS9.OverbyteIcsTicks64,
    Z.ICS9.OverbyteIcsUtils,
    Z.ICS9.OverbyteIcsTypes,
    Z.ICS9.OverbyteIcsIpUtils,
    Z.ICS9.OverbyteIcsDnsQuery,
    Z.ICS9.OverbyteIcsMonCommon;


const
    CopyRight    : String     = ' TIcsMonSocket  (c) 2023 V9.0 ';

type
  TIcsMonSocket = class(TCustomWSocket)
  protected
      FMonIpAddr: String;
      FSocFamily: TSocketFamily;
      FInAddr4: TIcsIPv4Address ;
      FInAddr6: TIcsIPv6Address ;
      FIgnoreData: boolean ;
      FTotRecvBytes: int64 ;
      FTotSendBytes: int64 ;
      FTotRecvPackets: integer ;
      FTotSendPackets: integer ;
      FOnPacketEvent: TPacketDataEvent;
      FOnLogEvent: TPacketLogEvent;
      FIcsDNCache: TIcsDomainNameCache;
      procedure MonDataAvailable (Sender: TObject; ErrCode: Word) ;
      procedure PacketLog(const Msg: String);
  public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure StartMonitor;
      procedure StopMonitor;
  protected
  published
      property Addr ;
      property IcsDNCache: TIcsDomainNameCache read FIcsDNCache
                                                write FIcsDNCache;
      property IgnoreData: boolean      read FIgnoreData
                                        write FIgnoreData ;
      property TotRecvBytes: int64      read FTotRecvBytes ;
      property TotSendBytes: int64      read FTotSendBytes ;
      property TotRecvPackets: integer  read FTotRecvPackets ;
      property TotSendPackets: integer  read FTotSendPackets ;
      property OnDataAvailable ;
      property OnPacketEvent: TPacketDataEvent read  FOnPacketEvent
                                               write FOnPacketEvent;
      property OnLogEvent: TPacketLogEvent     read  FOnLogEvent
                                               write FOnLogEvent;
  end;


implementation


constructor TIcsMonSocket.Create(AOwner: TComponent);
begin
    ReqVerHigh := 2 ;
    ReqVerLow := 2 ;
    FIgnoreData := false ;
    inherited Create(AOwner);
    onDataAvailable := MonDataAvailable ;
end ;

destructor TIcsMonSocket.Destroy;
begin
    inherited Destroy;
end ;

procedure TIcsMonSocket.PacketLog(const Msg: String);
begin
    if Assigned(FOnLogEvent) then
        FOnLogEvent(Self, Msg);
end;

procedure TIcsMonSocket.MonDataAvailable (Sender: TObject; ErrCode: Word) ;
var
    hdrlen, I: integer ;
    PacketBuff: TBytes ;
    IpHdrv4: PHdrIP;
    TcpHdr: PHdrTCP;
    UdpHdr: PHdrUDP;
    PacketInfo: TPacketInfo ;  // the data we return in the event
    Ipv6From: TSockAddrIn6;
    FromLen: Integer;
    From6Addr: TIcsIPv6Address;

    procedure GetDataByOffset (offset: integer) ;
    var
        datastart: PAnsiChar ;
    begin
        datastart := PAnsiChar (PAnsiChar (@PacketBuff[0]) + offset) ;
        with PacketInfo do
        begin
            DataLen := Packetlen - offset;
            if DataLen <= 0 then exit ;
            if FIgnoreData then exit ;
            SetLength (DataBuf, DataLen) ;  // ANSI string
            Move (datastart^, DataBuf [1], DataLen) ;
        end ;
    end;

begin
    FillChar (PacketInfo, Sizeof(PacketInfo), 0) ;
    with PacketInfo do
    begin
        if FSocFamily = sfIpv4 then
        begin
            PacketLen := ReceiveTB(PacketBuff, 2048) ;
            if PacketLen <= 0 then exit ;
            IpHdrv4 := PHdrIP (@PacketBuff[0]);  // IP header is start of raw packet
            SocketFamily := sfIPv4;
            EtherProto := ETHERNET_IP ;     // IP v4
            AddrSrc := IpHdrv4.src_addr ;
            AddrDest := IpHdrv4.dest_addr ;
            SendFlag := (FInAddr4 = AddrSrc) ;  // did we sent this packet
            hdrlen := GetIHlen (IpHdrv4^) ;
            ProtoType := IpHdrv4.protocol ;   // TCP, UDP, ICMP
            DispAddrSrc := WSocketIPv4ToStr (AddrSrc) ;   // convert 32-bit IP address into dotted ASCII
            DispAddrDest := WSocketIPv4ToStr (AddrDest) ;
            DispProto := IcsIPProtoName (ProtoType) + 'v4';
        end
        else if FSocFamily = sfIpv6 then
        begin
            PacketLen := ReceiveFrom6TB(PacketBuff, IPv6From, FromLen, 2048) ;
            if PacketLen <= 0 then exit ;
            SocketFamily := sfIPv6;
            EtherProto := ETHERNET_IPV6 ;     // IP v6
            From6Addr := TIcsIPv6Address(IPv6From.sin6_addr);  // we only have dest address
            Addr6Src := FInAddr6;
            Addr6Dest := From6Addr;  // might be from ourself
            SendFlag := WSocketIPv6Same (From6Addr, FInAddr6);     // means we sent it

         // we don't know whether we have ICMP, UDP or TCP data
         // try and guess which header by checking some values - hopeless really!!!!!!
            hdrlen := 0;
            UdpHdr := PHdrUDP (@PacketBuff[0]) ;
            I := ntohs(UdpHdr.length);
        //    PacketLog('Packlen=' + IntToStr(PacketLen) + ', UDPlen=' + IntToStr(I) + ', FromPort=' + IntToStr(IPv6From.sin6_port)) ;  // !!! TEMP DIAG
            if I = PacketLen  then  // should really use checksum as well
            begin
                ProtoType := IPPROTO_UDP;
           //     PortSrc := ntohs (UdpHdr.src_port) ;
          //      PortDest := ntohs (UdpHdr.dst_port) ;
            end
            else
            begin
                ProtoType := IPPROTO_TCP;
            //    TcpHdr := PHdrTCP (PAnsiChar (@PacketBuff[0]) + hdrlen) ;
                  // should use checksum but we need a psuedo IPv6 header that needs reliable IP addresses, and we don't have that
           //     PortSrc := ntohs (TcpHdr.source) ;
           //     PortDest := ntohs (TcpHdr.dest) ;
            end;
            DispAddrSrc := WSocketIPv6ToStr(Addr6Src);
            DispAddrDest := WSocketIPv6ToStr(Addr6Dest) ;
            DispProto := IcsIPProtoName (ProtoType) + 'v6';
        end
        else
            Exit;

        PacketDT := IcsNowPC ;  // time using performance counter

     // start lookup of host names, no event so only get cached name after first time
         if Assigned(FIcsDNCache) then
         begin
            HostNameSrc := FIcsDNCache.LookupIPOne(DispAddrSrc, 0, sfAny, Nil);
            HostNameDest := FIcsDNCache.LookupIPOne(DispAddrDest, 0, sfAny, Nil);
         end;

     // increment global traffic counters
        if SendFlag then
        begin
            inc (FTotSendBytes, packetlen + OFFSET_IP) ;    // add 14-byte ethernet header length for statistics
            inc (FTotSendPackets) ;
        end
        else
        begin
            inc (FTotRecvBytes, packetlen + OFFSET_IP) ;
            inc (FTotRecvPackets) ;
        end ;

    // check protocol and find ports and data
        if Assigned (FOnPacketEvent) then
        begin
            DataBuf := '' ;
            if ProtoType = IPPROTO_ICMP then
            begin
                IcmpType := PByte (PAnsiChar (@PacketBuff[0]) + hdrlen)^ ;
                GetDataByOffset (hdrlen + 1)
            end
            else if ProtoType = IPPROTO_TCP then
            begin
                TcpHdr := PHdrTCP (PAnsiChar (@PacketBuff[0]) + hdrlen) ;
                PortSrc := ntohs (TcpHdr.source) ;
                PortDest := ntohs (TcpHdr.dest) ;
                TcpFlags := ntohs (TcpHdr.flags) ;
                GetDataByOffset (hdrlen + GetTHdoff (TcpHdr^));
                DispServ := Lowercase (IcsServiceNameEx (PortSrc, PortDest));
           end
            else if ProtoType = IPPROTO_UDP then
            begin
                UdpHdr := PHdrUDP (PAnsiChar (@PacketBuff[0]) + hdrlen) ;
                PortSrc := ntohs (UdpHdr.src_port) ;
                PortDest := ntohs (UdpHdr.dst_port) ;
                GetDataByOffset (hdrlen + Sizeof (THdrUDP));
                DispServ := Lowercase (IcsServiceNameEx (PortSrc, PortDest));
            end
            else
                GetDataByOffset (0);
        end ;
        FOnPacketEvent (Self, PacketInfo) ;
    end ;
end ;

procedure TIcsMonSocket.StartMonitor;
var
    Success: Boolean;
begin
    FMonIpAddr := Addr;
    if NOT WSocketIsIP(FMonIpAddr, FSocFamily) then   // Feb 2022 IPv6
    begin
        Exit;
    end;
    if FSocFamily = sfIPv4 then
        FInAddr4 := WSocketStrToIPv4 (FMonIpAddr, Success)   // keep 32-bit listen IP address
    else if FSocFamily = sfIPv6 then
        FInAddr6 := WSocketStrToIPv6 (FMonIpAddr, Success)   // keep IPv6 listen IP address
    else
        Exit;
    SocketFamily := FSocFamily; // Feb 2023
    FTotRecvBytes := 0 ;
    FTotSendBytes := 0 ;
    FTotRecvPackets := 0 ;
    FTotSendPackets := 0 ;
    Port := '0' ;  // all ports
    Proto := 'raw_ip' ;
    IcsAlignNowPC;    // force performance counter clock to align with system clock
    ComponentOptions := [wsoSIO_RCVALL] ;  // receive all packets on this address
    Listen ;
end ;

procedure  TIcsMonSocket.StopMonitor;
begin
    Close ;
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
