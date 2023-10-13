unit ADServFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.PascalStrings,
  Z.Net,
  Z.Net.Server.ICS,
  Z.Net.Server.Indy,
  Z.Net.Server.CrossSocket, Z.Status, Z.Core,
  Z.DFE, Z.Net.DoubleTunnelIO;

type
  TAuthDoubleServerForm = class;

  TMyVM_Tunnel = class(TZNet_Server_CrossSocket)
  public
    procedure p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean); override;
    // vm ����ձ�����ʱ
    procedure p2pVMTunnelOpenBefore(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM); override;
    // ����Ѿ����ֳɹ�
    procedure p2pVMTunnelOpen(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM); override;
    // ������ֳɹ����ӳ�һ��������
    procedure p2pVMTunnelOpenAfter(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM); override;
    // ����رգ����¼����ڷ���Զ�����󣬻������ʱ����
    procedure p2pVMTunnelClose(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM); override;
  end;

  TMyService = class(TZNet_DoubleTunnelService)
  private
    f: TAuthDoubleServerForm;
  protected
    procedure UserRegistedSuccess(UserID: string); override;
    procedure UserLinkSuccess(UserDefineIO: TService_RecvTunnel_UserDefine); override;
    procedure UserOut(UserDefineIO: TService_RecvTunnel_UserDefine); override;
  protected
    // reg cmd
    procedure cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
    procedure cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDFE);
    procedure cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDFE);
  public
    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TAuthDoubleServerForm = class(TForm)
    Memo1: TMemo;
    StartServiceButton: TButton;
    Timer1: TTimer;
    ChangeCaptionButton: TButton;
    GetClientValueButton: TButton;
    TimeLabel: TLabel;
    procedure StartServiceButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ChangeCaptionButtonClick(Sender: TObject);
    procedure GetClientValueButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }

    // vm���
    // vm������������������У�ͬʱ��������Э��ջ�Ĺ���
    // ����������ὫRecvTunnel+SendTunnelͬʱ����VMTunnel�У�ֻ��һ������ʵ��˫ͨ������
    VMTunnel: TMyVM_Tunnel;

    // zs������ͨѶ���
    RecvTunnel: TZNet_WithP2PVM_Server;
    SendTunnel: TZNet_WithP2PVM_Server;
    Service: TMyService;
  end;

var
  AuthDoubleServerForm: TAuthDoubleServerForm;

implementation

{$R *.dfm}

procedure TMyVM_Tunnel.p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TMyVM_Tunnel.p2pVMTunnelClose(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM);
begin
  // �������¼�ʱ��vm����Ѿ����

  // �ָ���
  Sender.p2pVM.UnInstallLogicFramework(AuthDoubleServerForm.RecvTunnel);
  Sender.p2pVM.UnInstallLogicFramework(AuthDoubleServerForm.SendTunnel);
  inherited p2pVMTunnelClose(Sender, p2pVMTunnel);
end;

procedure TMyVM_Tunnel.p2pVMTunnelOpenBefore(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM);
begin
  inherited;
  // �������¼�ʱ��vm����Ѿ�����������δ����

  // ����󶨵ĵ�ͨ���Ƿ��������ͣ�����һ�Զ�
  // һ�Զ���ͬһ�������������԰󶨵����vm����У��Ӷ�ʵ��vm�����������
  // ��vm�������������������ǲ����Ƶģ�һ������������Դ�100����������

  // ���һ�������ɹ���VMTunnelҲ�������շ����RecvTunnel+SendTunnel������󶨣�������Ӱ��VMTunnel
  // ���һ�������ɹ���VMTunnel��Э��ͻᷢ���仯�����������������Ҫ���׽�����

  // ������ͨ���󶨵�vm�����
  Sender.p2pVM.InstallLogicFramework(AuthDoubleServerForm.RecvTunnel);
  // ������ͨ���󶨵�vm�����
  Sender.p2pVM.InstallLogicFramework(AuthDoubleServerForm.SendTunnel);
end;

procedure TMyVM_Tunnel.p2pVMTunnelOpen(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM);
begin
  inherited;
  // �������¼�ʱ��vm�ѳɹ�������
end;

procedure TMyVM_Tunnel.p2pVMTunnelOpenAfter(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM);
begin
  inherited;
  // �������¼�ʱ��vm�ѳɹ������֣����Ҿ�����1��
end;

procedure TMyService.UserLinkSuccess(UserDefineIO: TService_RecvTunnel_UserDefine);
begin
  inherited UserLinkSuccess(UserDefineIO);
  DoStatus('user link success!');
end;

procedure TMyService.UserOut(UserDefineIO: TService_RecvTunnel_UserDefine);
begin
  inherited UserOut(UserDefineIO);
  DoStatus('user out!');
end;

procedure TMyService.UserRegistedSuccess(UserID: string);
begin
  inherited UserRegistedSuccess(UserID);
end;

procedure TMyService.cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
var
  UserIO: TService_RecvTunnel_UserDefine;
begin
  UserIO := GetUserDefineRecvTunnel(Sender);

  // �û�δ��¼�ɹ�
  if not UserIO.LoginSuccessed then
      exit;
  // ͨ��δ�ϲ�
  if not UserIO.LinkOK then
      exit;

  DoStatus('client: %s', [InData]);
end;

procedure TMyService.cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDFE);
var
  UserIO: TService_RecvTunnel_UserDefine;
begin
  UserIO := GetUserDefineRecvTunnel(Sender);

  // �û�δ��¼�ɹ�
  if not UserIO.LoginSuccessed then
      exit;
  // ͨ��δ�ϲ�
  if not UserIO.LinkOK then
      exit;

  DoStatus('client: %s', [InData.Reader.ReadString]);
end;

procedure TMyService.cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDFE);
var
  UserIO: TService_RecvTunnel_UserDefine;
begin
  UserIO := GetUserDefineRecvTunnel(Sender);

  // �û�δ��¼�ɹ�
  if not UserIO.LoginSuccessed then
      exit;
  // ͨ��δ�ϲ�
  if not UserIO.LinkOK then
      exit;

  OutData.WriteString('result 654321');
end;

procedure TMyService.RegisterCommand;
begin
  inherited RegisterCommand;
  RecvTunnel.RegisterDirectConsole('helloWorld_Console').OnExecute := cmd_helloWorld_Console;
  RecvTunnel.RegisterDirectStream('helloWorld_Stream').OnExecute := cmd_helloWorld_Stream;
  RecvTunnel.RegisterStream('helloWorld_Stream_Result').OnExecute := cmd_helloWorld_Stream_Result;
end;

procedure TMyService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  RecvTunnel.UnRegisted('helloWorld_Console');
  RecvTunnel.UnRegisted('helloWorld_Stream');
  RecvTunnel.UnRegisted('helloWorld_Stream_Result');
end;

procedure TAuthDoubleServerForm.ChangeCaptionButtonClick(Sender: TObject);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString('change caption as hello World,from server!');
  // �㲥�����������ֿͻ����Ƿ��е�¼���Ƿ����ɹ���˫ͨ��
  SendTunnel.BroadcastDirectStreamCmd('ChangeCaption', de);
  disposeObject(de);
end;

procedure TAuthDoubleServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TAuthDoubleServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  VMTunnel := TMyVM_Tunnel.Create;

  RecvTunnel := TZNet_WithP2PVM_Server.Create;
  SendTunnel := TZNet_WithP2PVM_Server.Create;
  Service := TMyService.Create(RecvTunnel, SendTunnel);

  // Ĭ������£�TMyService���ᱣ���û���Ϣ��UserDB����ÿ���˳�����������������û�õ�Ŀ¼
  // �����ǽ� AllowSaveUserInfo ���Ժ����е��û���Ϣ���������ü�¼
  // ע�⣺δ��������Ҫά��ʱ�û����ݿ�ʱ��ֻ��ͨ��������ɣ�ֱ�ӹ����ļ��Ƿ������
  Service.AllowSaveUserInfo := True;

  // ����֤��˫ͨ������������ʱ �����ֶ���ȡ�û����ݿ� ��һ�������ʹ�ý����ڴ� ���UserDB��СΪ300M ��ȡʱ�����Ҫ2G�ڴ濪��
  // ���û���¼��Ϊ�˼ӿ��û����ϼ����������û���Ϣ�������ڴ��У����UserDB��СΪ300M ����ʱ�����Ҫ1G���ڴ濪��
  // ����û�̫�࣬���糬��10����ôx86ƽ̨���ڴ��ǲ����õģ�����Ҫx64
  // LoadUserDB�ڲ�ʹ�����Ǹ���Hash�������������ȡ�ǳ��� ���ǳ������ڴ�
  Service.LoadUserDB;

  Service.f := self;
  Service.AllowRegisterNewUser := True;

  StartServiceButtonClick(nil);
end;

procedure TAuthDoubleServerForm.FormDestroy(Sender: TObject);
begin
  disposeObject([VMTunnel, RecvTunnel, SendTunnel, Service]);
  DeleteDoStatusHook(self);
end;

procedure TAuthDoubleServerForm.GetClientValueButtonClick(Sender: TObject);
begin
  SendTunnel.ProgressPeerIOP(procedure(PeerClient: TPeerClient)
    var
      c: TPeerClient;
      de: TDFE;
    begin
      c := PeerClient;
      // ����ͻ���û�е�¼�ɹ�
      if TService_SendTunnel_UserDefine(c.UserDefine).RecvTunnel = nil then
          exit;
      // ������һ��������ͻ���û�е�¼
      if not TService_SendTunnel_UserDefine(c.UserDefine).RecvTunnel.LinkOK then
          exit;

      de := TDFE.Create;
      de.WriteString('change caption as hello World,from server!');
      c.SendStreamCmdP('GetClientValue', de,
        procedure(Sender: TPeerClient; ResultData: TDFE)
        begin
          if ResultData.Count > 0 then
              DoStatus('getClientValue [%s] response:%s', [c.GetPeerIP, ResultData.Reader.ReadString]);
        end);
      disposeObject(de);
    end);
end;

procedure TAuthDoubleServerForm.StartServiceButtonClick(Sender: TObject);
begin
  // ����CrosssSocket�ٷ��ĵ��������Host�ӿ����Ϊ�գ�������IPV6+IPV4��IP��ַ
  // ���Host�ӿ�Ϊ0.0.0.0������IPV4��ַ��::������IPV6��ַ
  if VMTunnel.StartService('', 9899) then
      DoStatus('vm����˿� 9899 �����ɹ�')
  else
      DoStatus('vm����˿� 9899 ����ʧ�ܣ�ϵͳռ��');

  // VMֻ֧��ipv6��ʽ�ĵ�ַ������VM������Ӱ�����ϵͳ��������VM�������������
  // VM�����������������һ���������
  // ������vm������1��2�����˿�
  SendTunnel.StartService('::', 1);
  SendTunnel.StartService('::', 11);
  SendTunnel.StartService('::', 111);
  SendTunnel.StartService('::', 1111);
  SendTunnel.StartService('::33:ff', 1111);

  RecvTunnel.StartService('::', 2);
  RecvTunnel.StartService('::12:3e:87ef', 2);
  RecvTunnel.StartService('::4:ff08:e302', 2);

  Service.UnRegisterCommand;
  Service.RegisterCommand;
end;

procedure TAuthDoubleServerForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  VMTunnel.Progress;
  Service.Progress;
  TimeLabel.Caption := Format('sync time:%f', [Service.CadencerEngine.UpdateCurrentTime]);
end;

end.
