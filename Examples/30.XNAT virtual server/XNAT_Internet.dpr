program XNAT_Internet;

{$APPTYPE CONSOLE}

{$R *.res}

{
  XNAT�Ĺ��������������ڹ���Э��ӿ�
  XNAT��MobileServer�ǹ������ֻ�����IOT�豸�ķ�����ģ��
  �ڹ���Э��ӿ��У�XNATʹ����P2PVM��������ʾ��������ֻ���������������ӵķ���

  XNAT������������������4000
}

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
    {
      ��͸Э��ѹ��ѡ��
      ����ʹ�ó���:
      �������������Ѿ�ѹ����������ʹ��https���෽ʽ���ܹ���ѹ������Ч������ѹ�������ݸ���
      ���ʱ������Э�飬����ftp,����s��http,tennet��ѹ�����ؿ��Դ򿪣�����С������

      �����Ż�˼·��ZLib��ѹ���㷨������ѹ��������ѹ�ǳ��죬�÷�������������ʱ����ѹ�����ÿͻ��˷�������ȫ��ѹ��
      ��TXServiceListenʵ���е��� SendTunnel.CompleteBufferCompressed:=False;
      ��TXClientMappingʵ���е��� SendTunnel.CompleteBufferCompressed:=True;
      ��TXNAT_MappingOnVirutalServerʵ���е��� SendTunnel.CompleteBufferCompressed:=True;
    }
    XServ.ProtocolCompressed := True;

    XServ.Host := '0.0.0.0';     // ��������������ͨѶ������Э������󶨵�ַΪ����������ipv4�������ipv6��д'::'
    XServ.Port := '7890';        // ��������������ͨѶ������Э��˿�
    XServ.AuthToken := '123456'; // ��������������ͨѶ������Э����֤�ַ���(�ñ�ʶ��ʹ���˿���������ģ�ͣ���ؼ����������о�����)

    {
      ��������
    }
    // �ڷ���������Ҫӳ��Ķ˿�8000���󶨵�ַΪ����������ipv4����Ϊ���س����ӵ��Զ�����񣬵����ӿ���10���ӳ�ʱ����Զ��ͷ�socket
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
