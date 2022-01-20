unit EzCliFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.Net,
  Z.Status, Z.Core,
  Z.Net.Client.CrossSocket,
  Z.Net.Client.ICS,
  Z.Cadencer, Z.DFE, Z.UnicodeMixedLib,
  Z.Net.Client.Indy;

type
  TEZClientForm = class(TForm)
    Memo1: TMemo;
    ConnectButton: TButton;
    HostEdit: TLabeledEdit;
    Timer1: TTimer;
    HelloWorldBtn: TButton;
    sendMiniStreamButton: TButton;
    SendBigStreamButton: TButton;
    SendCompletebufferButton: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure HelloWorldBtnClick(Sender: TObject);
    procedure sendMiniStreamButtonClick(Sender: TObject);
    procedure SendBigStreamButtonClick(Sender: TObject);
    procedure SendCompletebufferButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
    procedure BackCall_helloWorld_Stream_Result(Sender: TPeerClient; ResultData: TDataFrameEngine);
  public
    { Public declarations }
    client: TZNet_Client;
  end;

var
  EZClientForm: TEZClientForm;

implementation

{$R *.dfm}


procedure TEZClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TEZClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  client := TZNet_Client_ICS.Create;
end;

procedure TEZClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TEZClientForm.BackCall_helloWorld_Stream_Result(Sender: TPeerClient; ResultData: TDataFrameEngine);
begin
  if ResultData.Count > 0 then
      DoStatus('server response:%s', [ResultData.Reader.ReadString]);
end;

procedure TEZClientForm.HelloWorldBtnClick(Sender: TObject);
var
  SendDe, ResultDE: TDataFrameEngine;
begin
  // ������������һ��console��ʽ��hello worldָ��
  client.SendDirectConsoleCmd('helloWorld_Console', '');

  // ������������һ��stream��ʽ��hello worldָ��
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('directstream 123456');
  client.SendDirectStreamCmd('helloWorld_Stream', SendDe);
  DisposeObject([SendDe]);

  // �첽��ʽ���ͣ����ҽ���Streamָ������Է����ص�����
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  client.SendStreamCmdM('helloWorld_Stream_Result', SendDe, BackCall_helloWorld_Stream_Result);
  DisposeObject([SendDe]);

  // �첽��ʽ���ͣ����ҽ���Streamָ�������proc�ص�����
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  client.SendStreamCmdP('helloWorld_Stream_Result', SendDe,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    begin
      if ResultData.Count > 0 then
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
    end);
  DisposeObject([SendDe]);

  // ������ʽ���ͣ����ҽ���Streamָ��
  SendDe := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  client.WaitSendStreamCmd('helloWorld_Stream_Result', SendDe, ResultDE, 5000);
  if ResultDE.Count > 0 then
      DoStatus('server response:%s', [ResultDE.Reader.ReadString]);
  DisposeObject([SendDe, ResultDE]);
end;

procedure TEZClientForm.SendBigStreamButtonClick(Sender: TObject);
var
  ms: TMemoryStream;
  p : PInt64;
  i : Integer;
begin
  // ��ms�а�����128M�������ݣ��ڷ��������൱��ִ����1����ͨ����
  ms := TMemoryStream.Create;
  ms.SetSize(128 * 1024 * 1024);

  DoStatus('����128M��ʱ��������');
  p := ms.Memory;
  for i := 1 to ms.Size div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('������ʱ��������md5');
  DoStatus('bigstream md5:' + umlMD5Char(ms.Memory, ms.Size).Text);

  // ������������һ��Big Stream��ʽ��ָ��
  client.SendBigStream('Test128MBigStream', ms, True);
end;

procedure TEZClientForm.SendCompletebufferButtonClick(Sender: TObject);
var
  buff: Pointer;
  p   : PInt64;
  i   : Integer;
begin
  // ��ms�а�����128M�������ݣ��ڷ��������൱��ִ����1����ͨ����
  buff := GetMemory(128 * 1024 * 1024);

  DoStatus('����128M��ʱ��������');
  p := buff;
  for i := 1 to (128 * 1024 * 1024) div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('������ʱ��������md5');
  DoStatus('complete buffer md5:' + umlMD5String(buff, 128 * 1024 * 1024).Text);

  // ������������һ��CompleteBuffer��ʽ��ָ��
  // ���Ĳ���������ʾ�Ƿ�����ɷ��ͺ��ͷ�buff
  client.SendCompleteBuffer('TestCompleteBuffer', buff, 128 * 1024 * 1024, True);
end;

procedure TEZClientForm.sendMiniStreamButtonClick(Sender: TObject);
var
  ms    : TMemoryStream;
  SendDe: TDataFrameEngine;
  p     : PInt64;
  i     : Integer;
begin
  // ��SendDE�а�����512k�������ݣ��ڷ��������൱��ִ����512����ͨ����
  ms := TMemoryStream.Create;
  ms.SetSize(512 * 1024);

  p := ms.Memory;
  for i := 1 to ms.Size div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('mini stream md5:' + umlMD5Char(ms.Memory, ms.Size).Text);

  // ������������һ��direct stream��ʽ��ָ��
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteStream(ms);
  client.SendDirectStreamCmd('TestMiniStream', SendDe);
  DisposeObject([SendDe, ms]);
end;

procedure TEZClientForm.Timer1Timer(Sender: TObject);
begin
  client.Progress;
end;

procedure TEZClientForm.ConnectButtonClick(Sender: TObject);
begin
  // ����1������ʽ����
  // if client.Connect(HostEdit.Text, 9818) then
  // DoStatus('���ӳɹ�')
  // else
  // DoStatus('����ʧ��');

  // ����2���첽��������
  client.AsyncConnectP(HostEdit.Text, 9818, procedure(const cState: Boolean)
    begin
      if cState then
        begin
          DoStatus('���ӳɹ�');
          DoStatus('current client id: %d', [client.ClientIO.ID]);
        end
      else
          DoStatus('����ʧ��');
    end);

end;

end.
