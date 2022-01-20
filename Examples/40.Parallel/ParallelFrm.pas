unit ParallelFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  System.Threading,
  Z.Core, Z.PascalStrings, Z.UnicodeMixedLib, Z.Status, Vcl.ExtCtrls;

type
  TParallelForm = class(TForm)
    ParaAddButton: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    ParaLockButton: TButton;
    Memo3: TMemo;
    Para19937Button: TButton;
    Memo4: TMemo;
    ParallelTRandomButton: TButton;
    Memo5: TMemo;
    ParaDelphiRandomButton: TButton;
    Memo6: TMemo;
    ComputeThreadButton: TButton;
    Timer1: TTimer;
    StateLabel: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ParaAddButtonClick(Sender: TObject);
    procedure ParaLockButtonClick(Sender: TObject);
    procedure Para19937ButtonClick(Sender: TObject);
    procedure ParallelTRandomButtonClick(Sender: TObject);
    procedure ParaDelphiRandomButtonClick(Sender: TObject);
    procedure ComputeThreadButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure Backcall_DoStatus(Text_: SystemString; const ID: Integer);
  public
    atomNum: TAtomInt64;     // ��TAtomXXX�����Ľṹ֧�֣����̺߳Ͳ��г���ʹ�ã�����ͨ�����������ڣ�ʹ��ǰ��Ҫ��ʼ��������ʱ��Ҫ�ͷ�
    atomString: TAtomString; // ��TAtomXXX�����Ľṹ֧�֣����̺߳Ͳ��г���ʹ�ã�����ͨ�����������ڣ�ʹ��ǰ��Ҫ��ʼ��������ʱ��Ҫ�ͷ�
  end;

var
  ParallelForm: TParallelForm;

implementation

{$R *.dfm}


procedure TParallelForm.FormDestroy(Sender: TObject);
begin
  atomNum.Free;
  atomString.Free;
end;

procedure TParallelForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, Backcall_DoStatus);

  // WorkInParallelCore�ڷ�ide�����²Ż�򿪣���Debugģʽ���ǹرյģ����ǽ���ǿ�ƴ�
  WorkInParallelCore.V := True;

  atomNum := TAtomInt64.Create(0);      // ��TAtomXXX�����Ľṹ֧�֣����̺߳Ͳ��г���ʹ�ã�����ͨ�����������ڣ�ʹ��ǰ��Ҫ��ʼ��������ʱ��Ҫ�ͷ�
  atomString := TAtomString.Create(''); // ��TAtomXXX�����Ľṹ֧�֣����̺߳Ͳ��г���ʹ�ã�����ͨ�����������ڣ�ʹ��ǰ��Ҫ��ʼ��������ʱ��Ҫ�ͷ�
end;

procedure TParallelForm.ParaAddButtonClick(Sender: TObject);
begin
  atomNum.Value := 0;
  DelphiParallelFor(0, 100000 - 1, procedure(pass: Integer)
    begin
      // ��������1����ס��ȡֵ����һ��inc�������ٽ���
      // 1,2���ֲ����������Լ���ʹ��
      atomNum.UnLock(atomNum.Lock + 1);
      // ��������2����ס��ȡֵ����һ��inc�������ٽ���
      // 1,2���ֲ����������Լ���ʹ��
      inc(atomNum.LockP^);
      atomNum.UnLock();
      // ͬ������ôҲ����д��
      atomNum.Lock;
      atomNum.p^ := atomNum.p^ + 1;
      atomNum.UnLock();

      // Ҫ����ĳ�ʶ�Գ������
      // ���´���Ĺ�����������
      // 1,��ס��ȡֵ�ٽ���
      // 2,ֵ+1
      // 3,��ס��ֵ�ٽ���
      // atomNum.V := atomNum.V + 1;
    end);
  DelphiParallelFor(0, 100000 - 1, procedure(pass: Integer)
    begin
      // ԭ��api�Ĳ��������������������ݣ�����ͬʱ����
      // ��������3��ͨ��ԭ��apiֱ�Ӳ���ָ��ֵ��ԭ��apiֻ��֧����������
      AtomInc(atomNum.p^);
    end);
  DelphiParallelFor(0, 100000 - 1, procedure(pass: Integer)
    begin
      // ԭ��api�Ĳ��������������������ݣ�����ͬʱ����
      // ��������4��ͨ��ԭ��apiֱ�Ӳ���ָ��ֵ��ԭ��apiֻ��֧����������
      AtomicIncrement(atomNum.p^);
    end);
  DoStatus(umlIntToStr(atomNum.Value));
end;

procedure TParallelForm.ParaLockButtonClick(Sender: TObject);
begin
  atomString.Value := '';
  DelphiParallelFor(0, 10000 - 1, procedure(pass: Integer)
    begin
      // ��ס��ֵ�ٽ���
      atomString.Value := umlIntToStr(pass);

      // ��ס��ȡֵ�����ֵ��'55'����������
      atomString.Lock;
      if atomString.p^ = '55' then
          DoStatus(atomString.p^);
      atomString.UnLock();

      // ��ס��ȡֵ�ٽ��������ֵ��'99'
      if atomString.Value = '99' then
          DoStatus('99');
    end);
  DoStatus();
end;

procedure TParallelForm.Para19937ButtonClick(Sender: TObject);
begin
  atomString.Value := '';
  DelphiParallelFor(0, CpuCount - 1, procedure(pass: Integer)
    var
      i: Integer;
      n: U_String;
      Buff: array of Integer;
      num: Integer;
    begin
      SetMT19937Seed(0);
      n := '';

      for i := 1 to 20 do
        begin
          // һ�β���һ�������
          // MT19937Rand32ÿ�ε���ʱ�����߳������MT19937ʵ�����߳�������ᷢ�����ݿ���
          // ��Ƶ�ʵ���Ӧ�ò���TRandom��������MT19937Rand32
          num := MT19937Rand32(10);

          if n.L > 0 then
              n.Append(#32);
          n.Append(umlIntToStr(num));
        end;

      atomString.Lock;
      atomString.p^ := atomString.p^ + Format('�߳�[%d]���������: %s'#13#10, [TCompute.CurrentThread.ThreadID, n.Text]);
      atomString.UnLock;
    end);
  DoStatus(atomString.V);
end;

procedure TParallelForm.ParallelTRandomButtonClick(Sender: TObject);
begin
  atomString.Value := '';
  DelphiParallelFor(0, CpuCount - 1, procedure(pass: Integer)
    var
      i: Integer;
      n: U_String;
      rnd: TRandom;
      num: Integer;
    begin
      rnd := TRandom.Create;
      rnd.seed := 0;
      n := '';

      for i := 1 to 20 do
        begin
          // ͨ��TRandom������������ͨ��TRandom����������������߳���ɿ��٣�������׷�������ܵĲ�������
          num := rnd.Rand32(10);

          if n.L > 0 then
              n.Append(#32);
          n.Append(umlIntToStr(num));
        end;

      atomString.Lock;
      atomString.p^ := atomString.p^ + Format('�߳�[%d]���������: %s'#13#10, [TCompute.CurrentThread.ThreadID, n.Text]);
      atomString.UnLock;

      rnd.Free;
    end);
  DoStatus(atomString.V);
end;

procedure TParallelForm.ParaDelphiRandomButtonClick(Sender: TObject);
begin
  if not MT19937CoreToDelphi then
    begin
      DoStatus('δ��InstallMT19937CoreToDelphi����ѡ��' + #13#10 +
        '��༭�ļ� zDefine.inc'#13#10 +
        '�򿪱���ѡ��: InstallMT19937CoreToDelphi'#13#10 +
        '�򿪱���ѡ��: MT19937SeedOnTComputeThreadIs0'#13#10);
      exit;
    end;
  atomString.Value := '';
  TParallel.For(0, CpuCount - 1, procedure(pass: Integer)
    var
      i: Integer;
      n: U_String;
      num: Integer;
    begin
      // ͨ��delphi�Դ�Random���������߳�ͳһ���������ʹ�øù���ǰ�������ʼ��
      SetMT19937Seed(0);

      n := '';
      for i := 1 to 20 do
        begin
          // ͨ��delphi�Դ�Random���������߳�ͳһ�������
          num := Random(10);
          if n.L > 0 then
              n.Append(#32);
          n.Append(umlIntToStr(num));
        end;

      atomString.Lock;
      atomString.p^ := atomString.p^ + Format('�߳�[%d]���������: %s'#13#10, [TCompute.CurrentThread.ThreadID, n.Text]);
      atomString.UnLock;
    end);
  DoStatus(atomString.V);
end;

procedure TParallelForm.ComputeThreadButtonClick(Sender: TObject);
var
  MyThreadNum: TAtomInteger;
  i: Integer;
begin
  MyThreadNum := TAtomInteger.Create(0);

  // ���ǿ�10��TCompute�߳�
  for i := 0 to 10 - 1 do
    begin
      // MyThreadNum�������Զ�����̼߳������������߳̾�����+1
      MyThreadNum.UnLock(MyThreadNum.Lock + 1);
      // RunP_NP = Run procedure no parameter��д, �÷����ṩ���޲����������߳�
      TCompute.RunP_NP(procedure
        var
          delTick: Integer;
        begin
          // TCompute.Sync ��һ����Ը���ݵ�ͬ����ʽ,��ͬ�� TThread.Synchronize
          TCompute.Sync(procedure
            begin
              TCompute.Sleep(umlRandomRange(10, 200));
            end);
          // �������������
          MT19937Randomize;
          // DoStatusNoLn=DoStatus No line������ӡ���У����������Ǵ�ӡ����յ�ǰ�У������̰߳�ȫ��
          DoStatusNoLn;
          DoStatusNoLn('�߳�����');
          // ����ӳ�1-5��
          delTick := umlRandomRange(1000, 5000);
          TCompute.Sleep(delTick);
          // doStatus ��״̬��ӡ֧�ַ����������̰߳�ȫ��
          DoStatusNoLn(' ģ���ӳ� %d ms', [delTick]);
          // MyThreadNum�������Զ�����̼߳��������߳̽���������-1
          MyThreadNum.UnLock(MyThreadNum.Lock - 1);
          DoStatusNoLn(' ���.');
          DoStatusNoLn;
        end);
    end;

  // ��ʵ�ֵȴ���10��TCompute�̼߳������
  while MyThreadNum.V > 0 do
    begin
      // ��������߳��е�TCompute��ֱ��д�� TThread.Sleep(10)

      // ��������̵߳�TCompute���������·���
      CheckThreadSynchronize(10);
      Application.ProcessMessages;
    end;

  MyThreadNum.Free;
  DoStatus('�����߳��ѽ���');
end;

procedure TParallelForm.Backcall_DoStatus(Text_: SystemString; const ID: Integer);
begin
  Memo6.Lines.Add(Text_);
end;

procedure TParallelForm.Timer1Timer(Sender: TObject);
begin
  StateLabel.Caption := TCompute.State;
  DoStatus();
end;

end.
 
