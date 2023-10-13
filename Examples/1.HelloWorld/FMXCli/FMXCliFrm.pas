unit FMXCliFrm;


interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,

  Z.Net.Client.Indy, Z.DFE,
  Z.Net, Z.Core, Z.Status, Z.UnicodeMixedLib, FMX.Memo.Types;

type
  TFMXClientForm = class(TForm)
    Memo1: TMemo;
    connectButton: TButton;
    HostEdit: TEdit;
    HelloWorldBtn: TButton;
    Timer1: TTimer;
    SendMiniStreamButton: TButton;
    Send128MBigStreamButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure HelloWorldBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SendMiniStreamButtonClick(Sender: TObject);
    procedure Send128MBigStreamButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
    procedure BackCall_helloWorld_Stream_Result(Sender: TPeerClient; ResultData: TDFE);
  public
    { Public declarations }
    client: TZNet_Client_Indy;
  end;

var
  FMXClientForm: TFMXClientForm;

implementation

{$R *.fmx}

{ TFMXClientForm }

procedure TFMXClientForm.BackCall_helloWorld_Stream_Result(Sender: TPeerClient; ResultData: TDFE);
begin
  if ResultData.Count > 0 then
      DoStatus('server response:%s', [ResultData.Reader.ReadString]);
end;

procedure TFMXClientForm.connectButtonClick(Sender: TObject);
begin
  // ����1������ʽ����
  // client.Connect(HostEdit.Text, 9818);

  // ����2���첽��������
  client.AsyncConnectP(HostEdit.Text, 9818, procedure(const cState: Boolean)
    begin
      if cState then
          DoStatus('���ӳɹ�')
      else
          DoStatus('����ʧ��');
    end);
end;

procedure TFMXClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure fmx_Indy_ProgressBackgroundProc;
begin
  Application.ProcessMessages;
end;

procedure TFMXClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  client := TZNet_Client_Indy.Create;
  ProgressBackgroundProc := fmx_Indy_ProgressBackgroundProc;
end;

procedure TFMXClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TFMXClientForm.HelloWorldBtnClick(Sender: TObject);
var
  SendDe, ResultDE: TDFE;
begin
  // ������������һ��console��ʽ��hello worldָ��
  client.SendDirectConsoleCmd('helloWorld_Console', '');

  // ������������һ��stream��ʽ��hello worldָ��
  SendDe := TDFE.Create;
  SendDe.WriteString('directstream 123456');
  client.SendDirectStreamCmd('helloWorld_Stream', SendDe);
  DisposeObject([SendDe]);

  // �첽��ʽ���ͣ����ҽ���Streamָ������Է����ص�����
  SendDe := TDFE.Create;
  SendDe.WriteString('123456');
  client.SendStreamCmdM('helloWorld_Stream_Result', SendDe, BackCall_helloWorld_Stream_Result);
  DisposeObject([SendDe]);

  // �첽��ʽ���ͣ����ҽ���Streamָ�������proc�ص�����
  SendDe := TDFE.Create;
  SendDe.WriteString('123456');
  client.SendStreamCmdP('helloWorld_Stream_Result', SendDe,
    procedure(Sender: TPeerClient; ResultData: TDFE)
    begin
      if ResultData.Count > 0 then
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
    end);
  DisposeObject([SendDe]);

  // ������ʽ���ͣ����ҽ���Streamָ��
  SendDe := TDFE.Create;
  ResultDE := TDFE.Create;
  SendDe.WriteString('123456');
  client.WaitSendStreamCmd('helloWorld_Stream_Result', SendDe, ResultDE, 5000);
  if ResultDE.Count > 0 then
      DoStatus('server response:%s', [ResultDE.Reader.ReadString]);
  DisposeObject([SendDe, ResultDE]);

end;

procedure TFMXClientForm.Send128MBigStreamButtonClick(Sender: TObject);
var
  ms: TMemoryStream;
  p : PInt64;
  i : Integer;
begin
  // ��ms�а�����16M�������ݣ��ڷ��������൱��ִ����1����ͨ����
  ms := TMemoryStream.Create;
  ms.Size := 16 * 1024 * 1024;

  DoStatus('����16M��ʱ��������');
  p := ms.Memory;
  for i := 1 to ms.Size div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('������ʱ��������md5');
  DoStatus('md5:' + umlMD5Char(ms.Memory, ms.Size).Text);

  // ������������һ��Big Stream��ʽ��ָ��
  client.SendBigStream('Test128MBigStream', ms, True);
end;

procedure TFMXClientForm.SendMiniStreamButtonClick(Sender: TObject);
var
  ms    : TMemoryStream;
  SendDe: TDFE;
  p     : PInt64;
  i     : Integer;
begin
  // ��SendDE�а�����512k�������ݣ��ڷ��������൱��ִ����512����ͨ����
  ms := TMemoryStream.Create;
  ms.Size := 512 * 1024;

  p := ms.Memory;
  for i := 1 to ms.Size div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus(umlMD5Char(ms.Memory, ms.Size).Text);

  // ������������һ��direct stream��ʽ��ָ��
  SendDe := TDFE.Create;
  SendDe.WriteStream(ms);
  client.SendDirectStreamCmd('TestMiniStream', SendDe);
  DisposeObject([SendDe, ms]);
end;

procedure TFMXClientForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  client.Progress;
end;

end.
