unit VirtualServFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  Z.Core, Z.PascalStrings, Z.UnicodeMixedLib, Z.Net,
  Z.Net.XNAT.MappingOnVirutalService, Z.Net.XNAT.Physics, Z.Net.Test, Z.Status, Z.Notify,
  FMX.Memo.Types;

type
  TVirtualServForm = class(TForm)
    Memo1: TMemo;
    netTimer: TTimer;
    TestButton: TButton;
    OpenButton: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure netTimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    XCli: TXNAT_VS_Mapping;
    server: TZNet_Server;
    server_test: TCommunicationTestIntf;

    // ģ�������Կͻ���
    // ����ģ����Կ��Կ�����app���ɣ����ڷ���ֱ������ʵ��
    client: TZNet_Client;
    client_test: TCommunicationTestIntf;
    procedure DoStatusIntf(AText: SystemString; const ID: Integer);
  end;

var
  VirtualServForm: TVirtualServForm;

implementation

{$R *.fmx}


procedure TVirtualServForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusIntf);

  XCli := TXNAT_VS_Mapping.Create;

  {
    ��͸Э��ѹ��ѡ��
    ����ʹ�ó���:
    �������������Ѿ�ѹ����������ʹ��https���෽ʽ���ܹ���ѹ������Ч������ѹ�������ݸ���
    �����������Э�飬����ftp,����s��http,tennet��ѹ�����ؿ��Դ򿪣�����С������
  }
  XCli.ProtocolCompressed := True;

  XCli.Host := '127.0.0.1';                         // ������������IP
  XCli.Port := '7890';                              // �����������Ķ˿ں�
  XCli.AuthToken := '123456';                       // Э����֤�ַ���
  server := XCli.AddMappingServer('web8000', 1000); // ��������������8000�˿ڷ��������Ϊ���ط�����

  server_test := TCommunicationTestIntf.Create;
  server_test.RegCmd(server);

  client := TXPhysicsClient.Create;
  client_test := TCommunicationTestIntf.Create;
  client_test.RegCmd(client);
end;

procedure TVirtualServForm.DoStatusIntf(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TVirtualServForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // �ȿͻ�������Ͽ�
  client.Disconnect;
  while client.RemoteInited do
      client.Progress;

  // ע���Ķ�һ����ѭ����Ƕ�׹�ϵ
  // ����XCli��������progress,��ô�����Զ�����vs��������Աprogress
  server.StopService;
  while server.Count > 0 do
      XCli.Progress;

  DisposeObject(client);
  DisposeObject(XCli);
  DisposeObject(server_test);
  DisposeObject(client_test);
  DeleteDoStatusHook(Self);
end;

procedure TVirtualServForm.OpenButtonClick(Sender: TObject);
begin
  // ����������͸
  // ��������������͸�������󣬱��ط��������Զ�StartService�����ط��������������κζ˿�
  XCli.OpenTunnel;
end;

procedure TVirtualServForm.TestButtonClick(Sender: TObject);
begin
  // ģ����ԣ����ӵ�����������
  if client.RemoteInited then // ��������� ֱ�ӿ����Ժ���
      client_test.ExecuteAsyncTestWithBigStream(client.ClientIO)
  else
    begin
      // δ����,��ʱ�򴴽�һ���µ�����
      // �������õĿͻ��˷������õķ��������ͻ������������ƣ�ע����ѭ��
      // �ܿ���ѭ���ķ���ֱ��ʹ���첽��ʽ
      client.AsyncConnectP('127.0.0.1', 8000, procedure(const cState: Boolean)
        begin
          if cState then
            begin
              client_test.ExecuteAsyncTestWithBigStream(client.ClientIO);
            end;
        end);
    end;
end;

procedure TVirtualServForm.netTimerTimer(Sender: TObject);
begin
  if XCli <> nil then
    begin
      XCli.Progress;
    end;
  client.Progress;
end;

end.
