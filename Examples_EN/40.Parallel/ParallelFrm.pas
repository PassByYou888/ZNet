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
    atomNum: TAtomInt64;     {  The structure named tatomxxx supports multithreading and parallel programs. The difference from ordinary variables is that it needs to be initialized before use and released when not in use  }
    atomString: TAtomString; {  The structure named tatomxxx supports multithreading and parallel programs. The difference from ordinary variables is that it needs to be initialized before use and released when not in use  }
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

  {  WorkInParallelCore only opens in non ide environments and is closed in Debug mode. We will force it to open  }
  WorkInParallelCore.V := True;

  atomNum := TAtomInt64.Create(0);      {  The structure named tatomxxx supports multithreading and parallel programs. The difference from ordinary variables is that it needs to be initialized before use and released when not in use  }
  atomString := TAtomString.Create(''); {  The structure named tatomxxx supports multithreading and parallel programs. The difference from ordinary variables is that it needs to be initialized before use and released when not in use  }
end;

procedure TParallelForm.ParaAddButtonClick(Sender: TObject);
begin
  atomNum.Value := 0;
  DelphiParallelFor(0, 100000 - 1, procedure(pass: Integer)
    begin
      {  Operation method 1: take value after locking, perform Inc operation once, and then unlock  }
      {  1,2 Two operation methods can be compatible for use  }
      atomNum.UnLock(atomNum.Lock + 1);
      {  Operation method 2: take value after locking, perform Inc operation once, and then unlock  }
      {  1,2 Two operation methods can be compatible for use  }
      inc(atomNum.LockP^);
      atomNum.UnLock();
      {  Similarly, we can also write  }
      atomNum.Lock;
      atomNum.p^ := atomNum.p^ + 1;
      atomNum.UnLock();

      {  Common sense program errors to avoid  }
      {  The workflow of the following code is as follows  }
      {  1. Lock and then unlock  }
      {  2, value+1  }
      {  3. Unlock after locking  }
      // atomNum.V := atomNum.V + 1;
    end);
  DelphiParallelFor(0, 100000 - 1, procedure(pass: Integer)
    begin
      {  The operation method of the atomic API is incompatible with the lock method and cannot be operated at the same time  }
      {  Operation method 3: directly operate the pointer value through the atomic API, which can only support integer operation  }
      AtomInc(atomNum.p^);
    end);
  DelphiParallelFor(0, 100000 - 1, procedure(pass: Integer)
    begin
      {  The operation method of the atomic API is incompatible with the lock method and cannot be operated at the same time  }
      {  Operation method 4: directly operate the pointer value through the atomic API, which can only support integer operation  }
      AtomicIncrement(atomNum.p^);
    end);
  DoStatus(umlIntToStr(atomNum.Value));
end;

procedure TParallelForm.ParaLockButtonClick(Sender: TObject);
begin
  atomString.Value := '';
  DelphiParallelFor(0, 10000 - 1, procedure(pass: Integer)
    begin
      {  Unlock after locking  }
      atomString.Value := umlIntToStr(pass);

      {  Lock, value, if the value is' 55 ', process and unlock  }
      atomString.Lock;
      if atomString.p^ = '55' then
          DoStatus(atomString.p^);
      atomString.UnLock();

      {  Lock and then unlock the value, if the value is' 99 '  }
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
          {  Generate one random number at a time  }
          {  MT19937Rand32 will find the MT19937 instance bound by the thread each time it is called, and the thread will experience a brief lag here  }
          {  High frequency calls should use the TRandom class instead of MT19937Rand32  }
          num := MT19937Rand32(10);

          if n.L > 0 then
              n.Append(#32);
          n.Append(umlIntToStr(num));
        end;

      atomString.Lock;
      atomString.p^ := atomString.p^ + Format(#13#10'Thread [%d] Random number sequence:%s', [TCompute.CurrentThread.ThreadID, n.Text]);
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
          {  Generate random numbers through the TRandom class, which does not cause thread lag and is suitable for concurrent programs that pursue extreme performance  }
          num := rnd.Rand32(10);

          if n.L > 0 then
              n.Append(#32);
          n.Append(umlIntToStr(num));
        end;

      atomString.Lock;
      atomString.p^ := atomString.p^ + Format(#13#10'Thread [%d] Random number sequence:%s', [TCompute.CurrentThread.ThreadID, n.Text]);
      atomString.UnLock;

      rnd.Free;
    end);
  DoStatus(atomString.V);
end;

procedure TParallelForm.ParaDelphiRandomButtonClick(Sender: TObject);
begin
  if not MT19937CoreToDelphi then
    begin
      DoStatus('InstallMT19937CoreToDelphi compilation option not opened' + #13#10 +
        'Please edit the file zDefineinc' +
        'Open the compilation option: installmt19937coretodelphi' +
        'Open compilation option: mt19937seedontcomputethreadis0');
      exit;
    end;
  atomString.Value := '';
  TParallel.For(0, CpuCount - 1, procedure(pass: Integer)
    var
      i: Integer;
      n: U_String;
      num: Integer;
    begin
      {  Generate thread consistent random numbers through Delphi's built-in Random function. Before using this function, initialization is necessary  }
      SetMT19937Seed(0);

      n := '';
      for i := 1 to 20 do
        begin
          {  Generate thread uniform random number through Delphi's own random function  }
          num := Random(10);
          if n.L > 0 then
              n.Append(#32);
          n.Append(umlIntToStr(num));
        end;

      atomString.Lock;
      atomString.p^ := atomString.p^ + Format(#13#10'Thread [%d] Random number sequence:%s', [TCompute.CurrentThread.ThreadID, n.Text]);
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

  {  We open 10 tcompute threads  }
  for i := 0 to 10 - 1 do
    begin
      {  MyThreadNum is our custom thread counter, and when creating a thread, let it+1  }
      MyThreadNum.UnLock(MyThreadNum.Lock + 1);
      {  RunP_NP = run procedure no parameter abbreviation. This method provides an anonymous thread without parameters  }
      TCompute.RunP_NP(procedure
        var
          delTick: Integer;
        begin
          {  TComputer.Sync is a relatively faster synchronization method, equivalent to TThread.Synchronize  }
          TCompute.Sync(procedure
            begin
              TCompute.Sleep(umlRandomRange(10, 200));
            end);
          {  Disrupting random number seeds  }
          MT19937Randomize;
          {  DoStatusNoLn=DoStatus No line, do not print line breaks, do not give parameters to print and clear the current line, it is thread safe  }
          DoStatusNoLn;
          DoStatusNoLn('Thread start');
          {  Random delay 1-5 seconds  }
          delTick := umlRandomRange(1000, 5000);
          TCompute.Sleep(delTick);
          {  Dostatus is a status printing support method, which is thread safe  }
          DoStatusNoLn('Analog delay %dMS', [delTick]);
          {  MyThreadNum is our custom thread counter, which is set to -1 when the thread ends  }
          MyThreadNum.UnLock(MyThreadNum.Lock - 1);
          DoStatusNoLn('Complete');
          DoStatusNoLn;
        end);
    end;

  {  The simple implementation waits for the calculation of these 10 tcompute threads to end  }
  while MyThreadNum.V > 0 do
    begin
      {  If TCompute is in the sub thread, write it directly as TThread. Sleep (10)  }

      {  If tcomputed in the main thread, the following method can be used  }
      CheckThreadSynchronize(10);
      Application.ProcessMessages;
    end;

  MyThreadNum.Free;
  DoStatus('All threads have ended');
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
 
