program XLan;

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
  {
    TXNatClient�ܹ����������̣���ֻռ��������10%���µ�cpuʹ��
    �����ж��������������Ҫ��͸���������TXNATClient����
  }
  try
    XCli := TXNATClient.Create;
    {
      ��͸Э��ѹ��ѡ��
      ����ʹ�ó���:
      �������������Ѿ�ѹ����������ʹ��https���෽ʽ���ܹ���ѹ������Ч������ѹ�������ݸ���
      �����������Э�飬����ftp,����s��http,tennet��ѹ�����ؿ��Դ򿪣�����С������
    }
    XCli.ProtocolCompressed := false; // �رտ�������

    XCli.Host := '127.0.0.1';   // ������������IP
    XCli.Port := '7890';        // �����������Ķ˿ں�
    XCli.AuthToken := '123456'; // Э����֤�ַ���

    // 127.0.0.1��������������IP
    XCli.AddMapping('127.0.0.1', '80', 'web8000', 5000); // ��������������8000�˿ڷ����������80�˿�

    XCli.OpenTunnel; // ����������͸

    while True do
      begin
        XCli.Progress;
        CheckThread(1);
      end;
  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
