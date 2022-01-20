unit Z.diocp_vclreg;

interface

uses
  Z.diocp_tcp_server, Z.diocp_tcp_client, Z.diocp_tcp_blockClient, Z.diocp_ex_httpClient, 
  Z.diocp_coder_tcpServer, Z.diocp_coder_tcpClient, Z.diocp_ex_httpServer,
  Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DIOCP', [TDiocpTcpServer, TDiocpCoderTcpServer
                              , TDiocpTcpClient, TDiocpCoderTcpClient
                              , TDiocpBlockTcpClient]);
end;

end.
