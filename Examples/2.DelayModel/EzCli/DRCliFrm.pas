unit DRCliFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.Net,
  Z.Status, Z.Core,
  Z.Net.Client.CrossSocket,
  Z.Net.Client.ICS,
  Z.Net.Client.Indy,
  Z.Cadencer, Z.DFE;

type
  TDRClientForm = class(TForm)
    Memo1: TMemo;
    ConnectButton: TButton;
    HostEdit: TLabeledEdit;
    Timer1: TTimer;
    DelayResponseBtn: TButton;
    DelayResponse2Btn: TButton;
    procedure DelayResponse2BtnClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure DelayResponseBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    client: TZNet_Client_CrossSocket;
  end;

var
  DRClientForm: TDRClientForm;

implementation

{$R *.dfm}


procedure TDRClientForm.DelayResponse2BtnClick(Sender: TObject);
type
  TMyDefine = record
    a, b, c: Integer;
  end;

  PMyDefine = ^TMyDefine;

var
  SendDe: TDFE;
  p: PMyDefine;
begin
  // �����첽����,�ͻ�������������������������д,���,���Ǿ�������Ҫ�õ������ṹ
  // PMyDefine���ǽ����ṹ,��άϵ���첽���������һ����
  new(p);
  p^.a := 1;
  p^.b := 2;
  p^.c := 3;

  SendDe := TDFE.Create;
  client.SendStreamCmdP('DelayResponse', SendDe, p, nil,
    procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: TDFE)
    var
      p2: PMyDefine;
    begin
      // ����ͻ���δ����,�����ܵ�����������,�������¼�
      // ���¼�����ʱ,DelayResponse2BtnClick�Ѿ����ý���,��ʱ�����ǲ���ֱ�ӷ���p����,��Ϊ��ջ�Ѿ����ƻ�,������Ҫ���»�ȡPMyDefine��ָ�����ݵ�p2

      p2 := Param1;

      DoStatus('a:%d', [p2^.a]);
      DoStatus('b:%d', [p2^.b]);
      DoStatus('c:%d', [p2^.c]);

      while ResultData.Reader.NotEnd do
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);

      // ����Է�����,���¼����ᴥ��,���Ǹղ������PMyDefine�ڴ�Ҳ�ᶪʧ
      dispose(p2);
    end,
    procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE)
    var
      p2: PMyDefine;
    begin
      p2 := Param1;
      // ���ڵȷ�����,������,�������¼�
      DoStatus('δ�յ�����,�쳣����');
      // ���ڲ��ᴥ���ɹ����շ����¼�,������Ҫ�������ͷ�PMyDefine
      dispose(p2);
    end
    );
  disposeObject([SendDe]);
end;

procedure TDRClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TDRClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  client := TZNet_Client_CrossSocket.Create;
end;

procedure TDRClientForm.FormDestroy(Sender: TObject);
begin
  disposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TDRClientForm.DelayResponseBtnClick(Sender: TObject);
var
  SendDe: TDFE;
  a: Integer;
begin
  // �첽��ʽ���ͣ����ҽ���Streamָ�������proc�ص�����
  a := 123;
  SendDe := TDFE.Create;
  client.SendStreamCmdP('DelayResponse', SendDe,
    procedure(Sender: TPeerClient; ResultData: TDFE)
    begin
      // ������¼��ڴ���ʱ,��ʵDelayResponseBtnClick�Ѿ�ִ�����,����aҲ�Ѿ����ٴ���,��������������������Χ
      // ���첽�¼�����ʱ,a��һ��δ���ƻ��Ķ�ջ�ռ���,������������ʹ�÷�Χ��,��Ϊ�����첽�¼�
      // ��Ҫ���첽�¼������������local����,������ȫ�ֱ���,����ʹ��para��ʽ���첽�¼�,��������ָ�뷽ʽָ������,�ο�DelayResponse2BtnClickʵ��
      while ResultData.Reader.NotEnd do
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
      // ��ô��֪������a��������copy����ָ����?
      // ����ָ��,delphi�������������Զ��������õ��ⲿ������Ϊһ��ָ��,��������ʱ,���ڷ���һ��δ֪����Ķ���
      // �����ӡ������a��456
      DoStatus(a);
    end);
  disposeObject([SendDe]);
  a := 456;
end;

procedure TDRClientForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  client.Progress;
end;

procedure TDRClientForm.ConnectButtonClick(Sender: TObject);
begin
  if client.Connect(HostEdit.Text, 9818) then
      DoStatus('connect success');
end;

end.
