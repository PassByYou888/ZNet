﻿program XLan;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Net,
  Z.Net.XNAT.Physics,
  Z.Net.XNAT.Client,
  Z.Status;

var
  XCli: TXNATClient;

begin
  { Txnatclient can be deployed to the main project, which only uses less than 10%of the CPU of the main project
    If multiple public network servers need to penetrate, you can create multiple txnatclients }
  try
    XCli := TXNATClient.Create;
    { Penetration protocol compression options
      Recommended usage scenarios:
      If the agent's data has been compressed or encrypted using HTTPS, the compression will be invalid, and even the compressed data will be larger
      If it is a bare data protocol, such as FTP, HTTP without s, TenneT, the compression switch can be turned on and the speed can be increased slightly }
    XCli.ProtocolCompressed := false; { Closing can increase speed }

    XCli.Host := '127.0.0.1'; { IP address of public network server }
    XCli.Port := '7890'; { The port number of the public server }
    XCli.AuthToken := '123456'; { Protocol Validation String }

    { 127.0.0.1 is the IP address of the intranet server }
    XCli.AddMapping('127.0.0.1', '80', 'web8000', 5000); { Reverse proxy port 8000 of the public server to port 80 of the local server }

    XCli.OpenTunnel; { Start intranet penetration }

    while True do
      begin
        XCli.Progress;
        CheckThread(10);
      end;
  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
