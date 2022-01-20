type
  TComputeDispatch = record
    OnRun_C: TRunWithThread_C;
    OnRun_M: TRunWithThread_M;
    OnRun_P: TRunWithThread_P;
    OnRun_C_NP: TRunWithThread_C_NP;
    OnRun_M_NP: TRunWithThread_M_NP;
    OnRun_P_NP: TRunWithThread_P_NP;
    OnDone_C: TRunWithThread_C;
    OnDone_M: TRunWithThread_M;
    OnDone_P: TRunWithThread_P;
    UserData: Pointer;
    UserObject: TCore_Object;
    procedure Init;
    procedure AssignTo(th: TCompute);
  end;

  PComputeDispatchData = ^TComputeDispatch;

  TCoreComputeThreadPool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TCompute>;
  TComputeDispatchPool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PComputeDispatchData>;

  TParallelOverflow = record
  public
    ActivtedParallel: Integer;
    procedure Acquire;
    procedure Release;
    function Busy(): Boolean; inline;
  end;

var
  CoreThreadPool: TCoreComputeThreadPool;
  ComputeDispatchCritical: TCritical;
  ComputeThreadTaskRunning: TAtomInteger;
  ParallelGranularity: Integer;
  MaxActivtedParallel: Integer;
  ParallelOverflow: TParallelOverflow;
  ComputeDispatchPool: TComputeDispatchPool;
  IdleComputeThreadSum: TAtomInt;

procedure TComputeDispatch.Init;
begin
  OnRun_C := nil;
  OnRun_M := nil;
  OnRun_P := nil;
  OnRun_C_NP := nil;
  OnRun_M_NP := nil;
  OnRun_P_NP := nil;
  OnDone_C := nil;
  OnDone_M := nil;
  OnDone_P := nil;
  UserData := nil;
  UserObject := nil;
end;

procedure TComputeDispatch.AssignTo(th: TCompute);
begin
  th.OnRun_C := OnRun_C;
  th.OnRun_M := OnRun_M;
  th.OnRun_P := OnRun_P;
  th.OnRun_C_NP := OnRun_C_NP;
  th.OnRun_M_NP := OnRun_M_NP;
  th.OnRun_P_NP := OnRun_P_NP;
  th.OnDone_C := OnDone_C;
  th.OnDone_M := OnDone_M;
  th.OnDone_P := OnDone_P;
  th.UserData := UserData;
  th.UserObject := UserObject;
end;

procedure TParallelOverflow.Acquire;
begin
  while Busy() do
      TCore_Thread.Sleep(1);
  AtomInc(ActivtedParallel);
end;

procedure TParallelOverflow.Release;
begin
  AtomDec(ActivtedParallel);
end;

function TParallelOverflow.Busy(): Boolean;
begin
  Result := (MaxActivtedParallel > 0) and (ActivtedParallel >= MaxActivtedParallel);
end;

function PickOrCreateThread(): TCompute;
begin
  Result := TCompute.Create;
  CoreThreadPool.Add(Result);
end;

procedure PostComputeDispatchData(var Data: TComputeDispatch);
var
  tk: TTimeTick;
  done: Boolean;
  th: TCompute;
begin
  // check for idle thread, and again run.
  if IdleComputeThreadSum.V > 0 then
    begin
      ComputeDispatchCritical.Acquire;
      ComputeDispatchPool.Add(@Data);
      ComputeDispatchCritical.Release;
      tk := GetTimeTick();
      while (IdleComputeThreadSum.V > 0) and (GetTimeTick() - tk < 5) do
        begin
          ComputeDispatchCritical.Acquire;
          done := ComputeDispatchPool.IndexOf(@Data) < 0;
          ComputeDispatchCritical.Release;
          if done then
              exit;
        end;
      ComputeDispatchCritical.Acquire;
      done := ComputeDispatchPool.IndexOf(@Data) < 0;
      if not done then
          ComputeDispatchPool.Remove(@Data);
      ComputeDispatchCritical.Release;
      if done then
          exit;
    end;

  // create thread
  ComputeDispatchCritical.Acquire;
  inc(ComputeThreadTaskRunning.LockP()^);
  ComputeThreadTaskRunning.Unlock;
  th := PickOrCreateThread();
  Data.AssignTo(th);
  th.Start();
  ComputeDispatchCritical.Release;
end;

procedure InitCoreThreadPool(Thread_Num: Integer);
var
  th: TCompute;
begin
  CoreThreadPool := TCoreComputeThreadPool.Create;
  ComputeThreadTaskRunning := TAtomInteger.Create(0);
  ParallelGranularity := Thread_Num;
  ComputeDispatchCritical := TCritical.Create;
  MaxActivtedParallel := 0;
  ParallelOverflow.ActivtedParallel := 0;
  ComputeDispatchPool := TComputeDispatchPool.Create;
  IdleComputeThreadSum := TAtomInt.Create(0);
end;

procedure FreeCoreThreadPool;
begin
  while TCompute.ActivtedTask() > 0 do
      CheckThreadSynchronize(1);

  CoreThreadPool.Free;
  CoreThreadPool := nil;

  ComputeThreadTaskRunning.Free;
  ComputeThreadTaskRunning := nil;

  ComputeDispatchCritical.Free;
  ComputeDispatchCritical := nil;

  ComputeDispatchPool.Free;
  ComputeDispatchPool := nil;

  IdleComputeThreadSum.Free;
  IdleComputeThreadSum := nil;
end;

procedure TCompute.Execute;
var
  tk: TTimeTick;
  NoTask: Boolean;
  i: Integer;
begin
  FRndInstance := InternalMT19937__();
  AtomInc(PMD19937Core(FRndInstance)^.Instance);

  while True do
    begin
      try
{$IFDEF MT19937SeedOnTComputeThreadIs0} SetMT19937Seed(0); {$ELSE MT19937SeedOnTComputeThreadIs0} MT19937Randomize(); {$ENDIF MT19937SeedOnTComputeThreadIs0}
        if Assigned(OnRun_C) then
            OnRun_C(Self);
        if Assigned(OnRun_M) then
            OnRun_M(Self);
        if Assigned(OnRun_P) then
            OnRun_P(Self);
        if Assigned(OnRun_C_NP) then
            OnRun_C_NP();
        if Assigned(OnRun_M_NP) then
            OnRun_M_NP();
        if Assigned(OnRun_P_NP) then
            OnRun_P_NP();
      except
      end;

      if Assigned(OnDone_C) or Assigned(OnDone_M) or Assigned(OnDone_P) then
          Synchronize({$IFDEF FPC}@{$ENDIF FPC}Done_Sync);

      // check for idle thread, and again run.
      tk := GetTimeTick;
      NoTask := True;
      inc(IdleComputeThreadSum.LockP()^);
      IdleComputeThreadSum.Unlock();
      for i := 1 to 100 do
        begin
          while NoTask and (GetTimeTick - tk < 10) do
            begin
              ComputeDispatchCritical.Acquire;
              if ComputeDispatchPool.Count > 0 then
                begin
                  ComputeDispatchPool[0]^.AssignTo(Self);
                  ComputeDispatchPool.Delete(0);
                  NoTask := False;
                end;
              ComputeDispatchCritical.Release;
            end;
          if not NoTask then
              break;
          // little delay
          Sleep(1);
        end;
      dec(IdleComputeThreadSum.LockP()^);
      IdleComputeThreadSum.Unlock();
      if NoTask then
          break;
    end;

  dec(ComputeThreadTaskRunning.LockP()^);
  ComputeThreadTaskRunning.Unlock();

  ComputeDispatchCritical.Acquire;
  CoreThreadPool.Remove(Self);
  ComputeDispatchCritical.Release;

  AtomDec(PMD19937Core(FRndInstance)^.Instance);
  FRndInstance := nil;
  RemoveMT19937Thread(Self);
end;

procedure TCompute.Done_Sync;
begin
  try
    if Assigned(OnDone_C) then
        OnDone_C(Self);
    if Assigned(OnDone_M) then
        OnDone_M(Self);
    if Assigned(OnDone_P) then
        OnDone_P(Self);
  except
  end;
end;

constructor TCompute.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  OnRun_C := nil;
  OnRun_M := nil;
  OnRun_P := nil;
  OnRun_C_NP := nil;
  OnRun_M_NP := nil;
  OnRun_P_NP := nil;
  OnDone_C := nil;
  OnDone_M := nil;
  OnDone_P := nil;
  UserData := nil;
  UserObject := nil;
  FRndInstance := nil;
end;

destructor TCompute.Destroy;
begin
  inherited Destroy;
end;

class function TCompute.ActivtedTask(): Integer;
begin
  ComputeDispatchCritical.Acquire;
  Result := CoreThreadPool.Count;
  ComputeDispatchCritical.Release;
end;

class function TCompute.WaitTask(): Integer;
begin
  Result := IdleComputeThreadSum.V;
end;

class function TCompute.TotalTask(): Integer;
begin
  Result := ComputeThreadTaskRunning.V;
end;

class function TCompute.State(): string;
begin
  Result := Format('Compute:%d Activted: %d Suspend: %d Granularity:%d MaxParallel:%d/%d',
    [TotalTask(), ActivtedTask(), WaitTask(), ParallelGranularity, ParallelOverflow.ActivtedParallel, MaxActivtedParallel]);
end;

class function TCompute.GetParallelGranularity: Integer;
begin
  Result := ParallelGranularity;
end;

class function TCompute.GetMaxActivtedParallel: Integer;
begin
  Result := MaxActivtedParallel;
end;

type
  TSyncTmp = class
  private
    OnRunC: TRunWithThread_C_NP;
    OnRunM: TRunWithThread_M_NP;
    OnRunP: TRunWithThread_P_NP;
    procedure DoSync;
  public
    constructor Create;
  end;

procedure TSyncTmp.DoSync;
begin
  try
    if Assigned(OnRunC) then
        OnRunC();
    if Assigned(OnRunM) then
        OnRunM();
    if Assigned(OnRunP) then
        OnRunP();
  except
  end;
  Free;
end;

constructor TSyncTmp.Create;
begin
  inherited Create;
  OnRunC := nil;
  OnRunM := nil;
  OnRunP := nil;
end;

class procedure TCompute.Sync(const OnRun_: TRunWithThread_P_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunP := OnRun_;
  TCompute.Synchronize(TCompute.CurrentThread, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.Sync(const Thread_: TThread; OnRun_: TRunWithThread_P_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunP := OnRun_;
  TCompute.Synchronize(Thread_, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.SyncC(OnRun_: TRunWithThread_C_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunC := OnRun_;
  TCompute.Synchronize(TCompute.CurrentThread, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.SyncC(const Thread_: TThread; OnRun_: TRunWithThread_C_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunC := OnRun_;
  TCompute.Synchronize(Thread_, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.SyncM(OnRun_: TRunWithThread_M_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunM := OnRun_;
  TCompute.Synchronize(TCompute.CurrentThread, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.SyncM(const Thread_: TThread; OnRun_: TRunWithThread_M_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunM := OnRun_;
  TCompute.Synchronize(Thread_, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.SyncP(const OnRun_: TRunWithThread_P_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunP := OnRun_;
  TCompute.Synchronize(TCompute.CurrentThread, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.SyncP(const Thread_: TThread; OnRun_: TRunWithThread_P_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunP := OnRun_;
  TCompute.Synchronize(Thread_, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.RunC(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_C);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_C := OnRun;
  Dispatch_.OnDone_C := OnDone;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunC(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_C);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_C := OnRun;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunC(const OnRun: TRunWithThread_C);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_C := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunC_NP(const OnRun: TRunWithThread_C_NP);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_C_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunM(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_M);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_M := OnRun;
  Dispatch_.OnDone_M := OnDone;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunM(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_M);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_M := OnRun;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunM(const OnRun: TRunWithThread_M);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_M := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunM_NP(const OnRun: TRunWithThread_M_NP);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_M_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunP(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_P);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_P := OnRun;
  Dispatch_.OnDone_P := OnDone;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunP(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_P);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_P := OnRun;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunP(const OnRun: TRunWithThread_P);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_P := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunP_NP(const OnRun: TRunWithThread_P_NP);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_P_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.ProgressPost();
begin
  MainThreadProgress.Progress();
end;

class procedure TCompute.PostC1(OnSync: TThreadPost_C1);
begin
  MainThreadProgress.PostC1(OnSync);
end;

class procedure TCompute.PostC2(Data1: Pointer; OnSync: TThreadPost_C2);
begin
  MainThreadProgress.PostC2(Data1, OnSync);
end;

class procedure TCompute.PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3);
begin
  MainThreadProgress.PostC3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4);
begin
  MainThreadProgress.PostC4(Data1, Data2, OnSync);
end;

class procedure TCompute.PostM1(OnSync: TThreadPost_M1);
begin
  MainThreadProgress.PostM1(OnSync);
end;

class procedure TCompute.PostM2(Data1: Pointer; OnSync: TThreadPost_M2);
begin
  MainThreadProgress.PostM2(Data1, OnSync);
end;

class procedure TCompute.PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3);
begin
  MainThreadProgress.PostM3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4);
begin
  MainThreadProgress.PostM4(Data1, Data2, OnSync);
end;

class procedure TCompute.PostP1(OnSync: TThreadPost_P1);
begin
  MainThreadProgress.PostP1(OnSync);
end;

class procedure TCompute.PostP2(Data1: Pointer; OnSync: TThreadPost_P2);
begin
  MainThreadProgress.PostP2(Data1, OnSync);
end;

class procedure TCompute.PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3);
begin
  MainThreadProgress.PostP3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4);
begin
  MainThreadProgress.PostP4(Data1, Data2, OnSync);
end;
