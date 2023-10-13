program XInternet;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Net,
  Z.Net.XNAT.Physics,
  Z.Net.XNAT.Service,
  Z.Status;

var
  XServ: TXNATService;

begin
  XServ := TXNATService.Create;
  {  Penetration Protocol Compression Options
Suggested usage scenarios:
If the proxy's data has already been compressed or encrypted using methods such as HTTPS, the compression will be invalid, and even the compressed data will be larger
If it is a raw data protocol, such as FTP, HTTP without S, Tennet, and compression switch, it can be turned on to slightly increase speed
Performance optimization ideas: The compression algorithm of ZLib is characterized by slow compression and very fast decompression, allowing the server to send data without compression and allowing the client to compress all data sent
Adjust SendTunnel.CompleteBufferCompressed in the TXServiceListen instance:=False;
Adjust SendTunnel.CompleteBufferCompressed in the TXClientMapping instance:=True;
At TXNAT_Adjust SendTunnel.CompleteBufferCompressed in MappingOnVirutalServer instance:=True;  }
  XServ.ProtocolCompressed := False; {  Closing can increase speed  }

  XServ.Host := '0.0.0.0';     {  Communication parameters with intranet server: the protocol tunnel binding address is IPv4 of all network cards. If it is IPv6, write '::'  }
  XServ.Port := '7890';        {  Communication parameters with intranet server: protocol port  }
  XServ.AuthToken := '123456'; {  Communication parameters with internal network servers: protocol verification string (this identifier uses an anti quantum cryptography model, please study the code yourself for related technologies)  }

  {  Listening Configuration  }
  {  On the server side, port 8000 needs to be mapped, and the binding address is ipv4 for all network cards. Because short connected HTTP is mounted, when the connection is idle for 1 minute and timeout occurs, the socket will be automatically released  }
  XServ.AddMapping('0.0.0.0', '8000', 'web8000', 60 * 1000);

  {  When the internal network server is not connected, temporarily disconnected, and mapping "ftp8021" is not requested, the 8021 port is in a non listening state. This 8021 will only start working when all internal network servers are working normally  }
  {  On the server side, port 8021 needs to be mapped, and the binding address is IPV4 for all network cards. As long connected FTP is mounted, the socket will be automatically released after a timeout of 15 minutes when the connection is idle  }
  XServ.AddMapping('0.0.0.0', '8021', 'ftp8021', 15 * 60 * 1000);
  XServ.OpenTunnel;

  while True do
    begin
      XServ.Progress;
      CheckThread(10);
    end;

end.
