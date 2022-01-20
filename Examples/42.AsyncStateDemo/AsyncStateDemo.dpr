program AsyncStateDemo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  Variants,
  Z.Core,
  Z.Status;

// ����ʾ���߳���ʹ�ð�ȫ״̬��
procedure StateDemo();
var
  State: TAtomBool;
begin
  // �̰߳�ȫ:״̬����
  State := TAtomBool.Create(False);
  // �����߳�
  TCompute.RunP_NP(procedure
    var
      i: Cardinal;
    begin
      for i := 0 to $FFFFFFFF do
          nop;
      DoStatus('���һ������');
      // �ı�״̬����
      State.V := True;
    end);
  DoStatus('�ȴ�ѭ���������.');
  while not State.V do
    begin
      DoStatus();
      TCompute.Sleep(1);
    end;
  DisposeObject(State);
end;

// ����ʾ���߳���ʹ��Post����
// post��ͬ�ڰѴ��뷢�͸�һ�����ǹ�����Ŀ���߳���ִ��,post��������������,���������߳�/���߳�,�κ��̶߳���ʹ��
procedure PostDemo();
var
  P: TThreadPost; // TThreadPost,Post����֧��
  done: TAtomBool;        // �̰߳�ȫ:״̬����
  over: boolean;          // local����,�ڵ�����ɺ���Զ�����,�ʴ�,���Ǳ�����������һ������work_����״̬
begin
  // �̰߳�ȫ:״̬����
  done := TAtomBool.Create(False);
  P := TThreadPost.Create(0);
  // �����߳�
  TCompute.RunP_NP(procedure
    begin
      P.ThreadID := TCompute.CurrentThread.ThreadID;
      while not done.V do
        begin
          P.Progress(); // post��ѭ��
          TCompute.Sleep(1);
        end;
      over := True;
    end);
  // �������̷߳�����
  // ��Щ������ϸ�����˳��ִ��
  P.PostP1(procedure
    begin
      DoStatus(1);
    end);
  P.PostP1(procedure
    begin
      DoStatus(2);
    end);
  TCompute.RunP_NP(procedure
    var
      i: Integer;
    begin
      for i := 3 to 20 do
        begin
          TCompute.Sleep(100);
          P.PostP3(nil, nil, i, procedure(Data1: Pointer; Data2: TCore_Object; Data3: Variant)
            begin
              DoStatus(VarToStr(Data3));
            end);
        end;
      // �����߳̽���
      P.PostP1(procedure
        begin
          done.V := True;
        end);
    end);
  // ��ִ����
  while not over do
    begin
      DoStatus();
      TCompute.Sleep(1);
    end;
  DisposeObjectAndNil(P);
  DisposeObjectAndNil(done);
end;

begin
  StateDemo();
  PostDemo();
  DoStatus('�س�����������.');
  readln;

end.
