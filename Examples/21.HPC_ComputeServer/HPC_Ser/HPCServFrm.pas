unit HPCServFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Threading,
  Z.Net,
  Z.Net.Server.ICS,
  Z.Net.Server.Indy,
  Z.Net.Server.CrossSocket, Z.Status, Z.Core,
  Z.DFE, Z.Net.DoubleTunnelIO.NoAuth;

type
  TDoubleServerForm = class;

  TMyService = class(TZNet_DoubleTunnelService_NoAuth)
  private
    f: TDoubleServerForm;
  protected
    procedure UserLinkSuccess(UserDefineIO: TService_RecvTunnel_UserDefine_NoAuth); override;
    procedure UserOut(UserDefineIO: TService_RecvTunnel_UserDefine_NoAuth); override;
  protected
    // reg cmd
    procedure cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDFE);
  public
    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TDoubleServerForm = class(TForm)
    Memo1: TMemo;
    StartServiceButton: TButton;
    Timer1: TTimer;
    ChangeCaptionButton: TButton;
    GetClientValueButton: TButton;
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
    RecvTunnel: TZNet_Server_CrossSocket;
    SendTunnel: TZNet_Server_CrossSocket;
    Service: TMyService;
  end;

var
  DoubleServerForm: TDoubleServerForm;

implementation

{$R *.dfm}


procedure TMyService.UserLinkSuccess(UserDefineIO: TService_RecvTunnel_UserDefine_NoAuth);
begin
  inherited UserLinkSuccess(UserDefineIO);
  DoStatus('user link success!');
end;

procedure TMyService.UserOut(UserDefineIO: TService_RecvTunnel_UserDefine_NoAuth);
begin
  inherited UserOut(UserDefineIO);
  DoStatus('user out!');
end;

procedure TMyService.cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDFE);
begin
  // hpc�ӳٺ�̨���������ʾ�����Ʒǳ��򵥣����Դ��ģ�������̻�����
  Z.Net.RunHPC_StreamP(Sender, nil, nil, InData, OutData,
    procedure(ThSender: THPC_Stream; ThInData, ThOutData: TDFE)
    begin
      // �������ĺ�̨����������е������ķ�������ManagerServer
      TCompute.sync(procedure
        begin
          // ����Ҫ�������̵����еش����ߵ������ķ��������ҿ�ʼ�����ģ���㹤����
        end);

      // hpc�ӳٺ�̨�����봫ͳ�����������������Ҫʹ��ThInData,ThOutData�����ͷ�������

      // ��hpc��̨�������ʹ��doStatus
      DoStatus('run compute thread');

      // ����Ĵ����ǹ������̵߳ش���
      ThOutData.WriteString('result 654321');

      // �����Ҫͬ�������̣߳���Ҫʹ��
      TCompute.sync(procedure
        begin
          // �����������̵�ͬ���ش��������ļ�������zdb���ݿ�����ȵ�
        end);

      // ��hpc�ĺ�̨�ӳ��߳��У����л��ǰ�ȫ��
      // ParallelFor����Delphi���õ�TParallel.For
      // ParallelFor����fpc���õ�mtprocs
      ParallelFor(0, 10000, procedure(pass: Integer)
        begin
          // �ڲ��д����У���Ϊemb��������⣬�����޷�ʹ�� TThread.Synchronize ����
          // �ڲ��д����У����ǿ���ʹ��zServer�ں�ԭ����
          LockObject(Sender);
          UnLockObject(Sender);

          // ��hpc��̨�������ʹ��doStatus
          if pass mod 1000 = 1 then
              DoStatus('run compute thread:%d', [pass]);
        end);

      // �������ĺ�̨����������е������ķ�������ManagerServer
      TCompute.sync(procedure
        begin
          // ����Ҫ�������̵����еش����ߵ������ķ��������ҵĴ��ģ���㹤��������
        end);

      // ������ӳ����н�����ThOutData�������͸��ͻ��ˣ�Ȼ���ͷŵ���ʱ�ڴ�
    end);
end;

procedure TMyService.RegisterCommand;
begin
  inherited RegisterCommand;
  RecvTunnel.RegisterStream('helloWorld_Stream_Result').OnExecute := cmd_helloWorld_Stream_Result;
end;

procedure TMyService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  RecvTunnel.UnRegisted('helloWorld_Stream_Result');
end;

procedure TDoubleServerForm.ChangeCaptionButtonClick(Sender: TObject);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString('change caption as hello World,from server!');
  SendTunnel.BroadcastDirectStreamCmd('ChangeCaption', de);
  disposeObject(de);
end;

procedure TDoubleServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TDoubleServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  RecvTunnel := TZNet_Server_CrossSocket.Create;
  SendTunnel := TZNet_Server_CrossSocket.Create;
  Service := TMyService.Create(RecvTunnel, SendTunnel);
  Service.f := self;
end;

procedure TDoubleServerForm.FormDestroy(Sender: TObject);
begin
  disposeObject([RecvTunnel, SendTunnel, Service]);
  DeleteDoStatusHook(self);
end;

procedure TDoubleServerForm.GetClientValueButtonClick(Sender: TObject);
begin
  SendTunnel.ProgressPeerIOP(procedure(PeerClient: TPeerClient)
    var
      c: TPeerClient;
      de: TDFE;
    begin
      c := PeerClient;
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

procedure TDoubleServerForm.StartServiceButtonClick(Sender: TObject);
begin
  // ����CrosssSocket�ٷ��ĵ������ַ������Ϊ�գ���IPV6+IPV4
  if SendTunnel.StartService('', 9816) then
      DoStatus('listen send service success')
  else
      DoStatus('listen send service failed!');
  SendTunnel.IDCounter := 100;

  if RecvTunnel.StartService('', 9815) then
      DoStatus('listen Recv service success')
  else
      DoStatus('listen Recv service failed!');

  Service.RegisterCommand;
end;

procedure TDoubleServerForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  Service.Progress;
end;

end.
