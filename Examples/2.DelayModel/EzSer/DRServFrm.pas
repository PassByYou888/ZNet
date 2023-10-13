unit DRServFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.Net,
  Z.Net.Server.ICS,
  Z.Net.Server.Indy,
  Z.Net.Server.CrossSocket, Z.Status, Z.Core,
  Z.DFE, Z.Cadencer, Z.Notify;

type
  TDRServerForm = class(TForm)
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

    procedure PostExecute_DelayResponse(Sender: TNPostExecute);
    procedure cmd_DelayResponse(Sender: TPeerClient; InData, OutData: TDFE);
  public
    { Public declarations }

    // ����ģʽ�ķ��������
    Server: TZNet_Server_CrossSocket;

    // ��ȷ�������ʱ�����棬����֧���ӳٴ�������
    cadencerEng: TCadencer;

    // �ӳ��¼��������棬����ģ����������첽�ӳ�
    ProgressPost: TNProgressPost;

    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
  end;

var
  DRServerForm: TDRServerForm;

implementation

{$R *.dfm}


procedure TDRServerForm.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  ProgressPost.Progress(deltaTime);
end;

procedure TDRServerForm.PostExecute_DelayResponse(Sender: TNPostExecute);
var
  ID: Cardinal;
  c: TPeerClient;
begin
  // �ӿͻ����������ID������ͻ��˲����ڣ�����nilֵ
  ID := Sender.Data3;
  c := Server.PeerIO[ID];
  // ���ӳ��ڼ䣬�ͻ����п����Ѿ�����
  if c = nil then
      exit;

  c.OutDataFrame.WriteString('ִ������ʱ��:' + TimeToStr(time));

  // �������ͻ��˷�����Ӧ���ݣ����Ҽ��������ڲ��ĵȴ�����״̬
  c.ContinueResultSend;
end;

procedure TDRServerForm.cmd_DelayResponse(Sender: TPeerClient; InData, OutData: TDFE);
begin
  // DelayResponse���ִ����ɺ󣬲����������ͻ��˷���
  // �ӳ���Ӧ���ƶ�����״̬��ʵ�֣�һ��ֹͣ��Ӧ�������е�ָ��ᴦ�ڵȴ�״̬
  // �ӳٻ�����Ҫ���ڿ��ͨѶ�����������
  Sender.PauseResultSend;

  OutData.WriteString('�յ�����ʱ��:' + TimeToStr(time));

  // ���ӳ��¼�������һ��3.5���Ժ�ִ�е�һ�����¼�
  // ���¼������ڷ������첽ģ������һ̨��������ͨѶ�ӳ�
  // ������һ̨��������3.5���Ժ󣬲���Ӧ�����ݣ���ʱ�����첽��ʽ�������������ټ������ͻ��˷�����ȥ
  // ���ӳٹ����ж����е�ָ��ᴦ�ڵȴ�״̬
  with ProgressPost.PostExecuteM(3.5, PostExecute_DelayResponse) do
    begin
      // �ӳ���Ҫ��¼�µ�ǰ�ͻ��˵�ΨһID
      Data3 := Sender.ID;
    end;
end;

procedure TDRServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TDRServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  Server := TZNet_Server_CrossSocket.Create;

  Server.RegisterStream('DelayResponse').OnExecute := cmd_DelayResponse;

  cadencerEng := TCadencer.Create;
  cadencerEng.OnProgress := CadencerProgress;
  ProgressPost := TNProgressPost.Create;
end;

procedure TDRServerForm.FormDestroy(Sender: TObject);
begin
  DisposeObject([Server, cadencerEng, ProgressPost]);
  DeleteDoStatusHook(self);
end;

procedure TDRServerForm.StartServiceButtonClick(Sender: TObject);
begin
  // ����CrosssSocket�ٷ��ĵ������ַ������Ϊ�գ���IPV6+IPV4
  if Server.StartService('', 9818) then
      DoStatus('start service success')
  else
      DoStatus('start service failed!')
end;

procedure TDRServerForm.Timer1Timer(Sender: TObject);
begin
  Checkthread;
  Server.Progress;
  cadencerEng.Progress;
end;

end.
