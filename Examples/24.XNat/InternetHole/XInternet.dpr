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
  {
    ��͸Э��ѹ��ѡ��
    ����ʹ�ó���:
    �������������Ѿ�ѹ����������ʹ��https���෽ʽ���ܹ���ѹ������Ч������ѹ�������ݸ���
    �����������Э�飬����ftp,����s��http,tennet��ѹ�����ؿ��Դ򿪣�����С������

    �����Ż�˼·��ZLib��ѹ���㷨������ѹ��������ѹ�ǳ��죬�÷�������������ʱ����ѹ�����ÿͻ��˷�������ȫ��ѹ��
    ��TXServiceListenʵ���е��� SendTunnel.CompleteBufferCompressed:=False;
    ��TXClientMappingʵ���е��� SendTunnel.CompleteBufferCompressed:=True;
    ��TXNAT_MappingOnVirutalServerʵ���е��� SendTunnel.CompleteBufferCompressed:=True;
  }
  XServ.ProtocolCompressed := False; // �رտ�������

  XServ.Host := '0.0.0.0';     // ��������������ͨѶ������Э������󶨵�ַΪ����������ipv4�������ipv6��д'::'
  XServ.Port := '7890';        // ��������������ͨѶ������Э��˿�
  XServ.AuthToken := '123456'; // ��������������ͨѶ������Э����֤�ַ���(�ñ�ʶ��ʹ���˿���������ģ�ͣ���ؼ����������о�����)

  {
    ��������
  }
  // �ڷ���������Ҫӳ��Ķ˿�8000���󶨵�ַΪ����������ipv4����Ϊ���ض����ӵ�http�������ӿ���1���ӳ�ʱ����Զ��ͷ�socket
  XServ.AddMapping('0.0.0.0', '8000', 'web8000', 60 * 1000);

  {
    ������������δ����,��ʱ����,δ����mapping "ftp8021"��8021�˿ڶ��Ƿ�����״̬��ֻ�е�����������ȫ����������,���8021�ŻῪʼ����
  }
  // �ڷ���������Ҫӳ��Ķ˿�8021���󶨵�ַΪ����������ipv4����Ϊ���ص��ǳ����ӵ�ftp�������ӿ���15���ӳ�ʱ����Զ��ͷ�socket
  XServ.AddMapping('0.0.0.0', '8021', 'ftp8021', 15 * 60 * 1000);
  XServ.OpenTunnel;

  while True do
    begin
      XServ.Progress;
      CheckThread(1);
    end;

end.
