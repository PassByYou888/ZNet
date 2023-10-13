program XNAT_Internet;

{$APPTYPE CONSOLE}

{$R *.res}

{  The public network server of xnat is used for the public network protocol interface
The mobile server of xnat is a server model that works on mobile phones or IOT devices
In the public network protocol interface, xnat uses P2P VM technology to demonstrate how to make mobile phones carry a large number of physically connected services
The maximum physical concurrent connection limit for xnat is 4000  }

uses
  SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Net,
  Z.Net.XNAT.Service,
  Z.Status;

var
  XServ: TXNATService;

begin
  try
    XServ := TXNATService.Create;
    {  Penetration Protocol Compression Options
Suggested usage scenarios:
If the proxy's data has already been compressed or encrypted using methods such as HTTPS, the compression will be invalid, and even the compressed data will be larger
If the raw data protocol, such as FTP, HTTP without S, Tennet, and compression switch can be turned on, it can increase speed slightly
Performance optimization ideas: The compression algorithm of ZLib is characterized by slow compression and very fast decompression, allowing the server to send data without compression and allowing the client to compress all data sent
Adjust SendTunnel.CompleteBufferCompressed in the TXServiceListen instance:=False;
Adjust SendTunnel.CompleteBufferCompressed in the TXClientMapping instance:=True;
At TXNAT_Adjust SendTunnel.CompleteBufferCompressed in MappingOnVirutalServer instance:=True;  }
    XServ.ProtocolCompressed := True;

    XServ.Host := '0.0.0.0';     {  Communication parameters with intranet server: the protocol tunnel binding address is IPv4 of all network cards. If it is IPv6, write '::'  }
    XServ.Port := '7890';        {  Communication parameters with intranet server: protocol port  }
    XServ.AuthToken := '123456'; {  Communication parameters with internal network servers: protocol verification string (this identifier uses an anti quantum cryptography model, please study the code yourself for related technologies)  }

    {  Listening configuration  }
    {  On the server side, port 8000 needs to be mapped, and the binding address is IPv4 for all network cards. As a custom service with long connections is mounted, the socket will be automatically released when the connection is idle for 10 minutes and times out  }
    XServ.AddMapping('0.0.0.0', '18888', 'my18888', 10 * 60 * 1000);

    XServ.OpenTunnel;

    while True do
      begin
        XServ.Progress;
        try
            Z.Core.CheckThreadSynchronize(1);
        except
        end;
      end;

  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
