unit ADServFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.Net,
  Z.Net.Server.ICS,
  Z.Net.Server.Indy,
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
    RecvTunnel: TZNet_Server_ICS;
    SendTunnel: TZNet_Server_ICS;
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

  RecvTunnel := TZNet_Server_ICS.Create;
  SendTunnel := TZNet_Server_ICS.Create;
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
  Service.Progress;
  timeLabel.Caption:=Format('sync time:%f', [Service.CadencerEngine.UpdateCurrentTime]);
end;

end.
