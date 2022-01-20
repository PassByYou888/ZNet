unit OrderStructFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Z.Core, Z.PascalStrings, Z.Status;

type
  TOrderStructForm = class(TForm)
    Memo: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure DoStatus_Backcall(Text_: SystemString; const ID: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TMyRecord = record
    a, b, c: Integer;
    obj: TObject;
  end;

  // TOrderPtrStruct���Զ���TMyRecordת����Ϊָ��
  // �ýṹ�����ڼ�record
  TMyRecordOrder_Decl = TOrderPtrStruct<TMyRecord>;

  // TOrderPtrStruct�Դ��ͷŻ��ƣ����ڷ�����ʾ����������һ���̳в����������ǽӿ��ͷ��¼�����TMyRecord->obj�ͷŵ�
  TMyRecordOrder = class(TMyRecordOrder_Decl)
  public
    procedure DoFree(data: TMyRecordOrder_Decl.PT_); override;
  end;

var
  OrderStructForm: TOrderStructForm;

implementation

{$R *.dfm}


procedure TMyRecordOrder.DoFree(data: TMyRecordOrder_Decl.PT_);
begin
  // TOrderPtrStruct�Դ��ͷŻ��ƣ����ڷ�����ʾ����������һ���̳в����������ǽӿ��ͷ��¼�����TMyRecord->obj�ͷŵ�
  data^.obj.Free;
end;

procedure TOrderStructForm.Button1Click(Sender: TObject);
var
  order: TMyRecordOrder;
  i: Integer;
  tmp: TMyRecord;
begin
  order := TMyRecordOrder.Create;
  for i := 1 to 10 do
    begin
      tmp.a := i;
      tmp.b := i;
      tmp.c := i;
      tmp.obj := TObject.Create;
      // ��TMyRecord����ѹ�����
      // push�Ὣtmp���¿�����һ���ڴ��
      // ���ڱ�����ʹ����������飬push�����Ƿǳ���ģ����ҿ���֧�ָ��ܼ����ã�����ÿ��1�ڴ�
      // pushû���������ƣ�push���ƿ��Գ������ڴ��ù�
      order.Push(tmp);
    end;

  if false then
    begin
      // ���ֻ�ȡ���ݵķ�ʽ��1��ֱ���ж�current�Ƿ�Ϊnil
      while order.Current <> nil do
        begin
          with order.Current^.data^ do
              DoStatus('a:%d, b:%d, c:%d', [a, b, c]);

          order.Next; // next���ͷŵ�ǰ�ṹ��ͬʱ�ͷ��ڴ棬��ָ����һ���ṹ
        end;
    end
  else
    begin
      // ���ֻ�ȡ���ݵķ�ʽ��2���жϵ�ǰ����num
      while order.Num > 0 do
        begin
          with order.Current^.data^ do
              DoStatus('a:%d, b:%d, c:%d', [a, b, c]);

          order.Next; // next���ͷŵ�ǰ�ṹ��ͬʱ�ͷ��ڴ棬��ָ����һ���ṹ
        end;
    end;

  order.Free;
end;

procedure TOrderStructForm.DoStatus_Backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

constructor TOrderStructForm.Create(AOwner: TComponent);
begin
  inherited;
  AddDoStatusHook(Self, DoStatus_Backcall);
end;

destructor TOrderStructForm.Destroy;
begin
  inherited;
end;

end.
