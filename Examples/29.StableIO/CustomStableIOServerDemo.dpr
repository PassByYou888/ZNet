program CustomStableIOServerDemo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
  Classes,
  Variants,
  Z.Core,
  Z.Status,
  Z.DFE,
  Z.PascalStrings,
  Z.ListEngine,
  Z.UnicodeMixedLib,
  Z.MemoryStream,
  Z.Net,
  Z.Net.Test,
  Z.Net.XNAT.Physics;

{
  StableIOԭ��
  StableIOͨ���ػ�����IO(����synapse,corss,diocp,indy,ico��IO)���շ��¼���������һ���ȶ���IO����StableIO
  StableIO���黯�ģ�������������IO�����������Ժ�ʵ�����ֳ������ƻ��ˣ�StableIO�ڶ����Ժ���Ȼ���ڹ�����StableIO����һ��offline�Ĺ���ģʽ��
  ������IO������StableIO���offline���뿪������onlineģʽ������ԭ�ֳ�

  StableIO�ķ����������ǣ�TCommunicationFramework_StableServer
  StableIO�Ŀͻ��˱����ǣ�TCommunicationFramework_StableClient
  �����߱���Ժţ����ܽ����Զ����Ķ��������ͻָ��ֳ�����

  ��ͨ�ͻ���Ҳ��ֱ��ʹ��StableIO�����������ǲ����ж�����������

  ����StableIO��Demo��ʹ����������ϵͳ�����ײ��Ժ���֤
}

type
  TMyServer = class(TZNet_StableServer)
  public
    Test: TCommunicationTestIntf;
    constructor Create; override;
    destructor Destroy; override;
  end;

constructor TMyServer.Create;
begin
  inherited;
  Test := TCommunicationTestIntf.Create;
  Test.RegCmd(Self);
end;

destructor TMyServer.Destroy;
begin
  DisposeObject(Test);
  inherited;
end;

procedure MainLoop;
var
  MyServer: TMyServer;
  iostate, discard, recv, send, sequmem, n: string;
begin
  MyServer := TMyServer.Create;

  // ��Ҫ����
  // ����ʹ��XNAT��ΪStableIOʹ�õ��������������Ϊ���ߺ���������ȥ�������ƽ̨ʹ�õĲ�ͬͨѶ�ӿ�
  MyServer.OwnerIOServer := TXPhysicsServer.Create;

  // ��Ҫ����
  // ���ͻ�������������Ӻ󣬻��Զ��������а��Ĺ���ģʽ
  // ���а���ʹ����������µ�2-3���ڴ�
  // LimitSequencePacketMemoryUsage ���������������а�����ģʽ���ڴ�ʹ��
  // 0��ʾ������ʹ���ڴ棬1024��ʾ����Ϊ1kb�ڴ�
  // ���Ҫ�����ƣ������ڷ������˸�64*1024*1024����ʾ�������ı���64M�����б��ڴ濪��������ͻ��˿���ֱ�Ӳ�����
  // ���������ƣ��黯��IO���ӻᱻǿ�ƹرգ����һ����ڴ�
  // ����������£�ֻ��2�ֳ��������ʹ�����а��ڴ�
  // һ:offline��ģʽ��Ϊ���͵������޷�����Ŀ�꣬��һֱ�������ڴ��У�ֱ�������ڴ氲ȫ���ƻ���ﵽ����ʱ������
  // ��:�ڸ��������£����籾�ضԱ��ػ���ǧ���ڣ��շ�����CompleteBuffer��BigStream�����շ��Ĺ����У����а�ͨѶģʽ��ʹ�������ڴ濪����2-3������
  // ʹ��StableIO���ƣ�����ʹ��64λ����ϵͳ��Windows or linux������ȷ���������㹻ʹ�õ��ڴ�
  MyServer.LimitSequencePacketMemoryUsage := 0;

  // ��Ҫ����
  // �ͻ������ߺ�StableIO�Ὺ��offline�Ĺ���ģʽ�����ж�offline�����ݷ��ͣ������ݴ浽�ڴ���
  // �������������ͻ������߶�ã���λ�Ǻ���
  // ����Ǹ�Ƶ�ʵ����������շ�������ʱ��̫���������ڴ����
  // ��������ʹ��5���ӵ��������ƣ��ڰε����ߵ�5�����ڣ��������ָܻ��ֳ�
  MyServer.OfflineTimeout := 5 * 60 * 1000;

  // �޹ؽ�Ҫ
  // myserver�ͷ�ʱ���Զ��ͷ����������TXPhysicsServer
  MyServer.AutoFreeOwnerIOServer := True;

  // �޹ؽ�Ҫ
  // myserver��ѭ������ʱ��Ҳ�������������TXPhysicsServer
  MyServer.AutoProgressOwnerIOServer := True;

  if MyServer.StartService('0.0.0.0', 11977) then
      DoStatus('stableIO listen success!');

  while True do
    begin
      MyServer.Progress;
{$IFDEF MSWINDOWS}
      // IOBusy ���������κ�ƽ̨��IO״̬�����ƣ���IO�������ڴ���ʱ���ͻ᷵��true
      if MyServer.IOBusy then
          iostate := 'Busy'
      else
          iostate := 'Idle';
      // SetConsoleTitle('Server IDLE');

      discard := Format(
        'discard: %d, size: %s', [MyServer.Statistics[TStatisticsType.stSequencePacketDiscard],
        umlSizeToStr(MyServer.Statistics[TStatisticsType.stSequencePacketDiscardSize]).Text]);

      recv := Format('received: %d', [MyServer.Statistics[TStatisticsType.stReceiveSize]]);
      send := Format('sending: %d', [MyServer.Statistics[TStatisticsType.stSendSize]]);
      sequmem := Format('swap memory: %s', [umlSizeToStr(MyServer.Statistics[TStatisticsType.stSequencePacketMemoryOnSending]).Text]);

      SetConsoleTitle(PWideChar(Format('%s - IO:%d PIO:%d - %s - %s - %s - %s',
        [iostate, MyServer.Count, MyServer.OwnerIOServer.Count, recv, send, discard, sequmem])));
{$ENDIF MSWINDOWS}
      Z.Core.CheckThreadSynchronize(10);
    end;
end;

begin
  try
      MainLoop;
  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
