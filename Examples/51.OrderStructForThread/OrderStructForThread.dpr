program OrderStructForThread;
{$APPTYPE CONSOLE}
{$R *.res}


uses
  System.SysUtils, Z.Core, Z.Status;

// ��demo��ʾ���߳�2���߳�1�����ݲ����д���ı�̷�ʽ
// TCriticalOrderStruct ���нṹ���̰߳�ȫ��

type
  // ���ʹ��fpc������������������Ҫspecializeǰ׺
  // TDemoOrderStruct = specialize TCriticalOrderStruct<Integer>;
  // �������������ж�������
  TDemoOrderStruct = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalOrderStruct<Integer>;

var
  Activted: TAtomBool;
  ThNum: Integer;
  Order: TDemoOrderStruct;

procedure th1();
begin
  while Activted.V do
    begin
      if Order.Current <> nil then
        begin
          DoStatus('���: %d', [Order.Current^.Data]);
          Order.Next;
        end
      else
          TCompute.Sleep(1);
    end;
  AtomDec(ThNum);
end;

procedure th2();
var
  i: Integer;
begin
  for i := 1 to 15 do
    begin
      Order.Push(i);
      TCompute.Sleep(100);
    end;
  Activted.V := False;
  AtomDec(ThNum);
end;

begin
  Activted := TAtomBool.Create(True);
  Order := TDemoOrderStruct.Create;
  ThNum := 2;
  TCompute.RunC_NP(th1);
  TCompute.RunC_NP(th2);
  while ThNum > 0 do
    begin
      DoStatus;
      TCompute.Sleep(1);
    end;
  Activted.Free;
  Order.Free;
  DoStatus('Compute�߳��Ѿ���ȫ���������»س����˳�.');
  readln;
end.
