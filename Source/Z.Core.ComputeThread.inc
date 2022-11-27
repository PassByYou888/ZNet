type
  PComputeDispatchData = ^TComputeDispatch;
  TCompute_Dispatch_Queue_Pool = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<PComputeDispatchData>;

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
    IsRuning_, IsExit_: PBoolean;
    // internal
    IsPicked__: PBoolean;
    Pool_Queue_Ptr__: TCompute_Dispatch_Queue_Pool.PQueueStruct;
    procedure Init;
    procedure AssignTo(Th: TCompute);
  end;

  TCompute_Dispatch_Order = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalOrderStruct<TComputeDispatch>;

  TParallelOverflow = record
  public
    ActivtedParallel: Integer;
    procedure Acquire;
    procedure Release;
    function Busy(): Boolean;
  end;

var
  Core_Dispatch_Order__: TCompute_Dispatch_Order;
  Core_Dispatch_Order_Activted__, Core_Dispatch_Order_IsExit__: Boolean;
  Core_Thread_Pool__: TCoreCompute_Thread_Pool;
  Compute_Dispatch_Critical__: TCritical;
  Compute_Thread_Task_Running__: TAtomInt;
  Parallel_Granularity__: Integer;
  Max_Activted_Parallel__: Integer;
  Parallel_Overflow__: TParallelOverflow;
  Compute_Dispatch_Queue_Pool__: TCompute_Dispatch_Queue_Pool;
  IDLE_Compute_Thread_Sum: TAtomInt;
  IDLE_Compute_Wait_Time_Tick: TTimeTick;

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
  IsRuning_ := nil;
  IsExit_ := nil;
  // internal
  IsPicked__ := nil;
  Pool_Queue_Ptr__ := nil;
end;

procedure TComputeDispatch.AssignTo(Th: TCompute);
begin
  Th.OnRun_C := OnRun_C;
  Th.OnRun_M := OnRun_M;
  Th.OnRun_P := OnRun_P;
  Th.OnRun_C_NP := OnRun_C_NP;
  Th.OnRun_M_NP := OnRun_M_NP;
  Th.OnRun_P_NP := OnRun_P_NP;
  Th.OnDone_C := OnDone_C;
  Th.OnDone_M := OnDone_M;
  Th.OnDone_P := OnDone_P;
  Th.UserData := UserData;
  Th.UserObject := UserObject;
  Th.IsRuning := IsRuning_;
  Th.IsExit := IsExit_;
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
  Result := (Max_Activted_Parallel__ > 0) and (ActivtedParallel >= Max_Activted_Parallel__);
end;

function Pick_Or_Create_Compute_Thread(): TCompute;
begin
  Result := TCompute.Create;
  Result.Thread_Pool_Queue_Data_Ptr := Core_Thread_Pool__.Add(Result);
end;

procedure PostComputeDispatchData(var Data: TComputeDispatch);
begin
  Core_Dispatch_Order__.Push(Data);
end;

function Max_Thread_Supported: Integer;
begin
  Result := Parallel_Granularity__ * 10;
end;

type
  TCore_Dispatch_Order_Thread = class(TCore_Thread)
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

procedure TCore_Dispatch_Order_Thread.Execute;
  procedure Do_Pick(var Data_: TComputeDispatch);
  var
    Tick: TTimeTick;
    IsPicked_: Boolean;
    Th: TCompute;
  begin
    // check for idle thread, and again run.
    while (IDLE_Compute_Thread_Sum.V > 0) or (Compute_Thread_Task_Running__.V > Max_Thread_Supported) do
      begin
        // init atom
        IsPicked_ := False;
        Compute_Dispatch_Critical__.Acquire;
        Data_.IsPicked__ := @IsPicked_;
        Data_.Pool_Queue_Ptr__ := Compute_Dispatch_Queue_Pool__.Add(@Data_);
        Tick := GetTimeTick();
        Compute_Dispatch_Critical__.Release;

        // check
        while (IDLE_Compute_Thread_Sum.V > 0) and (GetTimeTick() - Tick < IDLE_Compute_Wait_Time_Tick) do
          begin
            Compute_Dispatch_Critical__.Acquire;
            if IsPicked_ then
              begin
                Compute_Dispatch_Critical__.Release;
                exit;
              end
            else
              begin
                Compute_Dispatch_Critical__.Release;
              end;
          end;

        Compute_Dispatch_Critical__.Acquire;
        if IsPicked_ then
          begin
            Compute_Dispatch_Critical__.Release;
            exit;
          end
        else
          begin
            // remove
            Compute_Dispatch_Queue_Pool__.Remove_P(Data_.Pool_Queue_Ptr__);
            Data_.IsPicked__ := nil;
            Compute_Dispatch_Critical__.Release;
          end;
      end;

    // create thread
    Compute_Dispatch_Critical__.Acquire;
    inc(Compute_Thread_Task_Running__.LockP()^);
    Compute_Thread_Task_Running__.Unlock;
    Th := Pick_Or_Create_Compute_Thread();
    Data_.AssignTo(Th);
    Th.Start();
    Compute_Dispatch_Critical__.Release;
  end;

var
  Last_TK, IDLE_TK: TTimeTick;
begin
  Last_TK := GetTimeTick();
  while True do
    begin
      if Core_Dispatch_Order__.Num > 0 then
        begin
          Do_Pick(Core_Dispatch_Order__.First^.Data);
          Core_Dispatch_Order__.Next;
          Last_TK := GetTimeTick()
        end
      else if Core_Dispatch_Order_Activted__ then
        begin
          IDLE_TK := GetTimeTick() - Last_TK;
          if IDLE_TK > 1000 then
            begin
              TCompute.Sleep(10);
            end
          else if IDLE_TK > 100 then
            begin
              TCompute.Sleep(1);
            end;
        end
      else
          break;
    end;

  Core_Dispatch_Order_IsExit__ := True;
end;

constructor TCore_Dispatch_Order_Thread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

destructor TCore_Dispatch_Order_Thread.Destroy;
begin
  inherited Destroy;
end;

function Get_Parallel_Granularity: Integer;
begin
  Result := Parallel_Granularity__;
end;

procedure Set_Parallel_Granularity(Thread_Num: Integer);
begin
  Parallel_Granularity__ := Thread_Num;
end;

procedure Set_IDLE_Compute_Wait_Time_Tick(Tick_: TTimeTick);
begin
  IDLE_Compute_Wait_Time_Tick := Tick_;
end;

procedure InitCoreThreadPool(Thread_Num: Integer);
var
  Th: TCore_Dispatch_Order_Thread;
begin
{$IFDEF DEBUG}
  if IsConsole then
      Write('Init Compute.');
{$ENDIF DEBUG}
  Core_Dispatch_Order__ := TCompute_Dispatch_Order.Create;
  Core_Dispatch_Order_Activted__ := True;
  Core_Dispatch_Order_IsExit__ := False;
  Core_Thread_Pool__ := TCoreCompute_Thread_Pool.Create;
  Compute_Thread_Task_Running__ := TAtomInt.Create(0);
  Parallel_Granularity__ := Thread_Num;
  Compute_Dispatch_Critical__ := TCritical.Create;
  Max_Activted_Parallel__ := 0;
  Parallel_Overflow__.ActivtedParallel := 0;
  Compute_Dispatch_Queue_Pool__ := TCompute_Dispatch_Queue_Pool.Create;
  IDLE_Compute_Thread_Sum := TAtomInt.Create(0);
  IDLE_Compute_Wait_Time_Tick := 1000;

  // create dispatch thread
  Th := TCore_Dispatch_Order_Thread.Create;
  Th.Start();
{$IFDEF DEBUG}
  if IsConsole then
      WriteLn(Format('CPU_Thread=%d, Max_Thread=%d, IDLE_Thread_Time=%dms', [Thread_Num, Max_Thread_Supported, IDLE_Compute_Wait_Time_Tick]));
{$ENDIF DEBUG}
end;

procedure FreeCoreThreadPool;
begin
{$IFDEF DEBUG}
  if IsConsole then
      WriteLn('Free Compute Pool.');
{$ENDIF DEBUG}
  Core_Dispatch_Order_Activted__ := False;
  while not Core_Dispatch_Order_IsExit__ do
    begin
      TCompute.Sleep(1);
    end;

  while (TCompute.ActivtedTask() > 0) do
    begin
      CheckThreadSynchronize(10);
    end;

  Core_Thread_Pool__.Free;
  Core_Thread_Pool__ := nil;
  Core_Dispatch_Order__.Free;
  Core_Dispatch_Order__ := nil;

  Compute_Thread_Task_Running__.Free;
  Compute_Thread_Task_Running__ := nil;

  Compute_Dispatch_Critical__.Free;
  Compute_Dispatch_Critical__ := nil;

  Compute_Dispatch_Queue_Pool__.Free;
  Compute_Dispatch_Queue_Pool__ := nil;

  IDLE_Compute_Thread_Sum.Free;
  IDLE_Compute_Thread_Sum := nil;
end;

procedure TCompute.Execute;
var
  Tick: TTimeTick;
  NoTask: Boolean;
begin
  FRndInstance := InternalMT19937__();
  AtomInc(PMD19937Core(FRndInstance)^.Instance_TMT19937Random__);

  while True do
    begin
{$IFDEF MT19937SeedOnTComputeThreadIs0} SetMT19937Seed(0); {$ELSE MT19937SeedOnTComputeThreadIs0} MT19937Randomize(); {$ENDIF MT19937SeedOnTComputeThreadIs0}
      if IsRuning <> nil then
          IsRuning^ := True;
      if IsExit <> nil then
          IsExit^ := False;

      try
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

      if IsRuning <> nil then
          IsRuning^ := False;
      if IsExit <> nil then
          IsExit^ := True;

      if Assigned(OnDone_C) or Assigned(OnDone_M) or Assigned(OnDone_P) then
          Synchronize({$IFDEF FPC}@{$ENDIF FPC}Done_Sync);

      // check for idle thread, and again run.
      NoTask := True;
      inc(IDLE_Compute_Thread_Sum.LockP()^);
      IDLE_Compute_Thread_Sum.Unlock();
      Tick := GetTimeTick;
      repeat
        Compute_Dispatch_Critical__.Acquire;
        if Compute_Dispatch_Queue_Pool__.Num > 0 then
          begin
            Compute_Dispatch_Queue_Pool__.First^.Data^.AssignTo(Self);
            if Compute_Dispatch_Queue_Pool__.First^.Data^.IsPicked__ <> nil then
                Compute_Dispatch_Queue_Pool__.First^.Data^.IsPicked__^ := True;
            Compute_Dispatch_Queue_Pool__.Next;
            NoTask := False;
          end;
        Compute_Dispatch_Critical__.Release;
        if NoTask then
            Sleep(1);
      until (not NoTask) or (GetTimeTick - Tick > IDLE_Compute_Wait_Time_Tick);
      dec(IDLE_Compute_Thread_Sum.LockP()^);
      IDLE_Compute_Thread_Sum.Unlock();
      if NoTask then
          break;
    end;

  Compute_Dispatch_Critical__.Acquire;
  Core_Thread_Pool__.Remove_P(Thread_Pool_Queue_Data_Ptr);
  Compute_Dispatch_Critical__.Release;

  AtomDec(PMD19937Core(FRndInstance)^.Instance_TMT19937Random__);
  FRndInstance := nil;
  RemoveMT19937Thread(Self);

  dec(Compute_Thread_Task_Running__.LockP()^);
  Compute_Thread_Task_Running__.Unlock();
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
  Thread_Pool_Queue_Data_Ptr := nil;
  OnRun_C := nil;
  OnRun_M := nil;
  OnRun_P := nil;
  OnRun_C_NP := nil;
  OnRun_M_NP := nil;
  OnRun_P_NP := nil;
  OnDone_C := nil;
  OnDone_M := nil;
  OnDone_P := nil;
  FRndInstance := nil;
  IsRuning := nil;
  IsExit := nil;
  UserData := nil;
  UserObject := nil;
end;

destructor TCompute.Destroy;
begin
  inherited Destroy;
end;

class function TCompute.IDLE_Thread(): NativeInt;
begin
  Result := IDLE_Compute_Thread_Sum.V;
end;

class function TCompute.ActivtedTask(): NativeInt;
begin
  Result := Compute_Thread_Task_Running__.V;
end;

class function TCompute.WaitTask(): NativeInt;
begin
  Result := Core_Dispatch_Order__.Num;
end;

class function TCompute.TotalTask(): NativeInt;
begin
  Result := Compute_Thread_Task_Running__.V + Core_Dispatch_Order__.Num;
end;

class function TCompute.State(): string;
begin
  Result := Format('Compute:%d Activted:%d/%d Suspend:%d IDLE_Thread:%d Critical:%d MT19937:%d Lock:%d Parallel:%d/%d Post:%d',
    [TotalTask(), ActivtedTask(), Max_Thread_Supported, WaitTask(), IDLE_Thread(),
      Get_System_Critical_Recycle_Pool_Num(), Get_MT19937_POOL_Num(), Get_Object_Lock_Pool_Num(),
      Parallel_Overflow__.ActivtedParallel, Max_Activted_Parallel__,
      MainThreadProgress.Num]);
end;

class function TCompute.GetParallelGranularity: Integer;
begin
  Result := Parallel_Granularity__;
end;

class function TCompute.GetMaxActivtedParallel: Integer;
begin
  Result := Max_Activted_Parallel__;
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

class procedure TCompute.RunC(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_C; IsRuning_, IsExit_: PBoolean);
var
  Dispatch_: TComputeDispatch;
begin
  if IsRuning_ <> nil then
      IsRuning_^ := True;
  if IsExit_ <> nil then
      IsExit_^ := False;

  Dispatch_.Init;
  Dispatch_.OnRun_C := OnRun;
  Dispatch_.OnDone_C := OnDone;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  Dispatch_.IsRuning_ := IsRuning_;
  Dispatch_.IsExit_ := IsExit_;
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

class procedure TCompute.RunC(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_C; IsRuning_, IsExit_: PBoolean);
var
  Dispatch_: TComputeDispatch;
begin
  if IsRuning_ <> nil then
      IsRuning_^ := True;
  if IsExit_ <> nil then
      IsExit_^ := False;

  Dispatch_.Init;
  Dispatch_.OnRun_C := OnRun;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  Dispatch_.IsRuning_ := IsRuning_;
  Dispatch_.IsExit_ := IsExit_;
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

class procedure TCompute.RunC(const OnRun: TRunWithThread_C; IsRuning_, IsExit_: PBoolean);
var
  Dispatch_: TComputeDispatch;
begin
  if IsRuning_ <> nil then
      IsRuning_^ := True;
  if IsExit_ <> nil then
      IsExit_^ := False;

  Dispatch_.Init;
  Dispatch_.OnRun_C := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  Dispatch_.IsRuning_ := IsRuning_;
  Dispatch_.IsExit_ := IsExit_;
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

class procedure TCompute.RunC_NP(const OnRun: TRunWithThread_C_NP; IsRuning_, IsExit_: PBoolean);
var
  Dispatch_: TComputeDispatch;
begin
  if IsRuning_ <> nil then
      IsRuning_^ := True;
  if IsExit_ <> nil then
      IsExit_^ := False;

  Dispatch_.Init;
  Dispatch_.OnRun_C_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  Dispatch_.IsRuning_ := IsRuning_;
  Dispatch_.IsExit_ := IsExit_;
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

class procedure TCompute.RunM(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_M; IsRuning_, IsExit_: PBoolean);
var
  Dispatch_: TComputeDispatch;
begin
  if IsRuning_ <> nil then
      IsRuning_^ := True;
  if IsExit_ <> nil then
      IsExit_^ := False;

  Dispatch_.Init;
  Dispatch_.OnRun_M := OnRun;
  Dispatch_.OnDone_M := OnDone;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  Dispatch_.IsRuning_ := IsRuning_;
  Dispatch_.IsExit_ := IsExit_;
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

class procedure TCompute.RunM(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_M; IsRuning_, IsExit_: PBoolean);
var
  Dispatch_: TComputeDispatch;
begin
  if IsRuning_ <> nil then
      IsRuning_^ := True;
  if IsExit_ <> nil then
      IsExit_^ := False;

  Dispatch_.Init;
  Dispatch_.OnRun_M := OnRun;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  Dispatch_.IsRuning_ := IsRuning_;
  Dispatch_.IsExit_ := IsExit_;
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

class procedure TCompute.RunM(const OnRun: TRunWithThread_M; IsRuning_, IsExit_: PBoolean);
var
  Dispatch_: TComputeDispatch;
begin
  if IsRuning_ <> nil then
      IsRuning_^ := True;
  if IsExit_ <> nil then
      IsExit_^ := False;

  Dispatch_.Init;
  Dispatch_.OnRun_M := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  Dispatch_.IsRuning_ := IsRuning_;
  Dispatch_.IsExit_ := IsExit_;
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

class procedure TCompute.RunM_NP(const OnRun: TRunWithThread_M_NP; IsRuning_, IsExit_: PBoolean);
var
  Dispatch_: TComputeDispatch;
begin
  if IsRuning_ <> nil then
      IsRuning_^ := True;
  if IsExit_ <> nil then
      IsExit_^ := False;

  Dispatch_.Init;
  Dispatch_.OnRun_M_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  Dispatch_.IsRuning_ := IsRuning_;
  Dispatch_.IsExit_ := IsExit_;
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

class procedure TCompute.RunP(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_P; IsRuning_, IsExit_: PBoolean);
var
  Dispatch_: TComputeDispatch;
begin
  if IsRuning_ <> nil then
      IsRuning_^ := True;
  if IsExit_ <> nil then
      IsExit_^ := False;

  Dispatch_.Init;
  Dispatch_.OnRun_P := OnRun;
  Dispatch_.OnDone_P := OnDone;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  Dispatch_.IsRuning_ := IsRuning_;
  Dispatch_.IsExit_ := IsExit_;
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

class procedure TCompute.RunP(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_P; IsRuning_, IsExit_: PBoolean);
var
  Dispatch_: TComputeDispatch;
begin
  if IsRuning_ <> nil then
      IsRuning_^ := True;
  if IsExit_ <> nil then
      IsExit_^ := False;

  Dispatch_.Init;
  Dispatch_.OnRun_P := OnRun;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  Dispatch_.IsRuning_ := IsRuning_;
  Dispatch_.IsExit_ := IsExit_;
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

class procedure TCompute.RunP(const OnRun: TRunWithThread_P; IsRuning_, IsExit_: PBoolean);
var
  Dispatch_: TComputeDispatch;
begin
  if IsRuning_ <> nil then
      IsRuning_^ := True;
  if IsExit_ <> nil then
      IsExit_^ := False;

  Dispatch_.Init;
  Dispatch_.OnRun_P := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  Dispatch_.IsRuning_ := IsRuning_;
  Dispatch_.IsExit_ := IsExit_;
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

class procedure TCompute.RunP_NP(const OnRun: TRunWithThread_P_NP; IsRuning_, IsExit_: PBoolean);
var
  Dispatch_: TComputeDispatch;
begin
  if IsRuning_ <> nil then
      IsRuning_^ := True;
  if IsExit_ <> nil then
      IsExit_^ := False;

  Dispatch_.Init;
  Dispatch_.OnRun_P_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  Dispatch_.IsRuning_ := IsRuning_;
  Dispatch_.IsExit_ := IsExit_;
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

class procedure TCompute.PostC1(OnSync: TThreadPost_C1; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostC1(OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.PostC2(Data1: Pointer; OnSync: TThreadPost_C2);
begin
  MainThreadProgress.PostC2(Data1, OnSync);
end;

class procedure TCompute.PostC2(Data1: Pointer; OnSync: TThreadPost_C2; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostC2(Data1, OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3);
begin
  MainThreadProgress.PostC3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostC3(Data1, Data2, Data3, OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4);
begin
  MainThreadProgress.PostC4(Data1, Data2, OnSync);
end;

class procedure TCompute.PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostC4(Data1, Data2, OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.PostM1(OnSync: TThreadPost_M1);
begin
  MainThreadProgress.PostM1(OnSync);
end;

class procedure TCompute.PostM1(OnSync: TThreadPost_M1; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostM1(OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.PostM2(Data1: Pointer; OnSync: TThreadPost_M2);
begin
  MainThreadProgress.PostM2(Data1, OnSync);
end;

class procedure TCompute.PostM2(Data1: Pointer; OnSync: TThreadPost_M2; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostM2(Data1, OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3);
begin
  MainThreadProgress.PostM3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostM3(Data1, Data2, Data3, OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4);
begin
  MainThreadProgress.PostM4(Data1, Data2, OnSync);
end;

class procedure TCompute.PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostM4(Data1, Data2, OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.PostP1(OnSync: TThreadPost_P1);
begin
  MainThreadProgress.PostP1(OnSync);
end;

class procedure TCompute.PostP1(OnSync: TThreadPost_P1; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostP1(OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.PostP2(Data1: Pointer; OnSync: TThreadPost_P2);
begin
  MainThreadProgress.PostP2(Data1, OnSync);
end;

class procedure TCompute.PostP2(Data1: Pointer; OnSync: TThreadPost_P2; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostP2(Data1, OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3);
begin
  MainThreadProgress.PostP3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostP3(Data1, Data2, Data3, OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4);
begin
  MainThreadProgress.PostP4(Data1, Data2, OnSync);
end;

class procedure TCompute.PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostP4(Data1, Data2, OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.Sync_Wait_PostC1(OnSync: TThreadPost_C1);
begin
  MainThreadProgress.Sync_Wait_PostC1(OnSync);
end;

class procedure TCompute.Sync_Wait_PostC2(Data1: Pointer; OnSync: TThreadPost_C2);
begin
  MainThreadProgress.Sync_Wait_PostC2(Data1, OnSync);
end;

class procedure TCompute.Sync_Wait_PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3);
begin
  MainThreadProgress.Sync_Wait_PostC3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.Sync_Wait_PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4);
begin
  MainThreadProgress.Sync_Wait_PostC4(Data1, Data2, OnSync);
end;

class procedure TCompute.Sync_Wait_PostM1(OnSync: TThreadPost_M1);
begin
  MainThreadProgress.Sync_Wait_PostM1(OnSync);
end;

class procedure TCompute.Sync_Wait_PostM2(Data1: Pointer; OnSync: TThreadPost_M2);
begin
  MainThreadProgress.Sync_Wait_PostM2(Data1, OnSync);
end;

class procedure TCompute.Sync_Wait_PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3);
begin
  MainThreadProgress.Sync_Wait_PostM3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.Sync_Wait_PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4);
begin
  MainThreadProgress.Sync_Wait_PostM4(Data1, Data2, OnSync);
end;

class procedure TCompute.Sync_Wait_PostP1(OnSync: TThreadPost_P1);
begin
  MainThreadProgress.Sync_Wait_PostP1(OnSync);
end;

class procedure TCompute.Sync_Wait_PostP2(Data1: Pointer; OnSync: TThreadPost_P2);
begin
  MainThreadProgress.Sync_Wait_PostP2(Data1, OnSync);
end;

class procedure TCompute.Sync_Wait_PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3);
begin
  MainThreadProgress.Sync_Wait_PostP3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.Sync_Wait_PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4);
begin
  MainThreadProgress.Sync_Wait_PostP4(Data1, Data2, OnSync);
end;
