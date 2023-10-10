unit CustomStableIOClientOnFMXMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox, FMX.Memo,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  Z.Core, Z.ZDB.Engine, Z.ZDB.LocalManager, Z.Status, Z.DFE, Z.PascalStrings,
  Z.ListEngine, Z.UnicodeMixedLib, Z.MemoryStream,
  Z.Net, Z.Net.Test, Z.Net.XNAT.Physics, FMX.Memo.Types;

type
  TMyClient = class(TZNet_StableClient)
  protected
    procedure DoConnected(Sender: TPeerIO); override;
    procedure DoDisconnect(Sender: TPeerIO); override;
  public
    Test: TCommunicationTestIntf;
    constructor Create; override;
    destructor Destroy; override;
    function isOffline: Boolean;
  end;

  TForm1 = class(TForm)
    Layout1: TLayout;
    Label1: TLabel;
    HostEdit: TEdit;
    connectButton: TButton;
    Memo1: TMemo;
    RunTestButton: TButton;
    DisconnectButton: TButton;
    Timer1: TTimer;
    IOStateLabel: TLabel;
    InfoLabel: TLabel;
    procedure connectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure RunTestButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MyClient: TMyClient;
    procedure backcall_DoStatus(AText: SystemString; const ID: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


constructor TMyClient.Create;
begin
  inherited;
  Test := TCommunicationTestIntf.Create;
  Test.RegCmd(Self);
end;

destructor TMyClient.Destroy;
begin
  DisposeObject(Test);
  inherited;
end;

procedure TMyClient.DoConnected(Sender: TPeerIO);
begin
  inherited;
end;

procedure TMyClient.DoDisconnect(Sender: TPeerIO);
begin
  inherited;
end;

function TMyClient.isOffline: Boolean;
begin
  Result := (not StableClientIO.Activted) or
    ((StableClientIO.Activted) and (StableClientIO.WaitConnecting));
end;

procedure TForm1.backcall_DoStatus(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TForm1.connectButtonClick(Sender: TObject);
begin
  MyClient.AsyncConnectP(HostEdit.Text, 11977, procedure(const cState: Boolean)
    begin
    end);
end;

procedure TForm1.DisconnectButtonClick(Sender: TObject);
begin
  MyClient.Disconnect;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DisposeObject(MyClient);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, backcall_DoStatus);
  MyClient := TMyClient.Create;

  // ��Ҫ����
  // ����ʹ��XNAT��ΪStableIOʹ�õ�����ͻ��ˣ���������ȥ�������ƽ̨ʹ�õĲ�ͬͨѶ�ӿ�
  MyClient.OwnerIOClient := TXPhysicsClient.Create;

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
  MyClient.LimitSequencePacketMemoryUsage := 0;

  // �޹ؽ�Ҫ
  // MyClient ���״�����ʱ��ʧ�ܣ���һֱ���ԣ������ں�̨���첽��ʽ�Զ������е�
  MyClient.AutomatedConnection := True;

  // �޹ؽ�Ҫ
  // MyClient �ͷ�ʱ���Զ��ͷ����������TXPhysicsClient
  MyClient.AutoFreeOwnerIOClient := True;

  // �޹ؽ�Ҫ
  // MyClient ��ѭ������ʱ��Ҳ�������������TXPhysicsClient
  MyClient.AutoProgressOwnerIOClient := True;
end;

procedure TForm1.RunTestButtonClick(Sender: TObject);
begin
  if MyClient.Connected then
      MyClient.Test.ExecuteAsyncTestWithBigStream(MyClient.ClientIO);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  discard, recv, send, sequmem: string;
begin
  CheckThread;
  MyClient.Progress;

  // IOBusy ���������κ�ƽ̨��IO״̬�����ƣ���IO�������ڴ���ʱ���ͻ᷵��true
  if MyClient.IOBusy then
      IOStateLabel.Text := 'IO Busy...'
  else
      IOStateLabel.Text := 'IO IDLE';

  discard := Format(
    'discard: %d, size: %s', [MyClient.Statistics[TStatisticsType.stSequencePacketDiscard],
    umlSizeToStr(MyClient.Statistics[TStatisticsType.stSequencePacketDiscardSize]).Text]);

  recv := Format('received: %d', [MyClient.Statistics[TStatisticsType.stReceiveSize]]);
  send := Format('sending: %d', [MyClient.Statistics[TStatisticsType.stSendSize]]);
  sequmem := Format('swap memory: %s', [umlSizeToStr(MyClient.ClientIO.SequencePacketUsagePhysicsMemory).Text]);

  InfoLabel.Text := Format('%s'#13#10'%s'#13#10'%s'#13#10'%s'#13#10'StopCommunicationTimeTick: %f ',
    [recv, send, discard, sequmem, MyClient.StopCommunicationTimeTick * 0.001]);
end;

end.
