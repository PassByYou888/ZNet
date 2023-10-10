unit ADRestoreServFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.Net,
  Z.Net.Server.CrossSocket, Z.Status, Z.Core,
  Z.DFE, Z.Net.DoubleTunnelIO, Z.Net.DoubleTunnelIO.VirtualAuth;

type
  TAuthDoubleServerForm = class;

  TMyService = class(TZNet_DoubleTunnelService)
  private
    f: TAuthDoubleServerForm;
  protected
    procedure UserRegistedSuccess(UserID: string); override;
    procedure UserLinkSuccess(UserDefineIO: TService_RecvTunnel_UserDefine); override;
    procedure UserOut(UserDefineIO: TService_RecvTunnel_UserDefine); override;
  public
    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TAuthDoubleServerForm = class(TForm)
    Memo1: TMemo;
    StartServiceButton: TButton;
    Timer1: TTimer;
    procedure StartServiceButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    RecvTunnel: TZNet_Server_CrossSocket;
    SendTunnel: TZNet_Server_CrossSocket;
    Service   : TMyService;
  end;

var
  AuthDoubleServerForm: TAuthDoubleServerForm;

implementation

{$R *.dfm}


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

procedure TMyService.RegisterCommand;
begin
  inherited RegisterCommand;
end;

procedure TMyService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
end;

procedure TAuthDoubleServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TAuthDoubleServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  RecvTunnel := TZNet_Server_CrossSocket.Create;
  SendTunnel := TZNet_Server_CrossSocket.Create;
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
end;

procedure TAuthDoubleServerForm.FormDestroy(Sender: TObject);
begin
  disposeObject([RecvTunnel, SendTunnel, Service]);
  DeleteDoStatusHook(self);
end;

procedure TAuthDoubleServerForm.StartServiceButtonClick(Sender: TObject);
begin
  // ����ICS�ٷ��ĵ�����Host�ӿڲ���Ϊ�գ���Ҫָ��IPV4 or IPV6
  if SendTunnel.StartService('0.0.0.0', 9816) then
      DoStatus('listen send service success')
  else
      DoStatus('listen send service failed!');
  SendTunnel.IDCounter := 100;

  if RecvTunnel.StartService('0.0.0.0', 9815) then
      DoStatus('listen Recv service success')
  else
      DoStatus('listen Recv service failed!');

  Service.UnRegisterCommand;
  Service.RegisterCommand;
end;

procedure TAuthDoubleServerForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  Service.Progress;
end;

end.
