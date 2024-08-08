type
  PComputeDispatchData = ^TComputeDispatch;
  TCompute_Dispatch_Queue_Pool = TBigList<PComputeDispatchData>;

  TComputeDispatch = record
    OnRun_C: TRun_Thread_C;
    OnRun_M: TRun_Thread_M;
    OnRun_P: TRun_Thread_P;
    OnRun_C_NP: TRun_Thread_C_NP;
    OnRun_M_NP: TRun_Thread_M_NP;
    OnRun_P_NP: TRun_Thread_P_NP;
    OnDone_C: TRun_Thread_C;
    OnDone_M: TRun_Thread_M;
    OnDone_P: TRun_Thread_P;
    UserData: Pointer;
    UserObject: TCore_Object;
    IsRuning_, IsExit_: PBoolean;
    // internal
    IsPicked__: PBoolean;
    Pool_Queue_Ptr__: TCompute_Dispatch_Queue_Pool.PQueueStruct;
    procedure Init;
    procedure AssignTo(Th: TCompute);
  end;

  TCompute_Dispatch_Order = TCriticalOrderStruct<TComputeDispatch>;

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
  Core_Thread_Dispatch_Critical__: TCritical;
  Core_Thread_Task_Runing__: TAtomInt;
  Core_Thread_Dispatch_Queue_Pool__: TCompute_Dispatch_Queue_Pool;
  Core_Thread_Wait_Sum__: TAtomInt;
  Core_Thread_Life_Time_Tick__: TTimeTick;
  Parallel_Granularity__: Integer;
  Max_Activted_Parallel__: Integer;
  Parallel_Overflow__: TParallelOverflow;

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
    while (Core_Thread_Wait_Sum__.V > 0)
{$IFDEF LimitMaxComputeThread}
      or (Core_Thread_Task_Runing__.V > Max_Thread_Supported)
{$ENDIF LimitMaxComputeThread}
      do
      begin
        // init atom
        IsPicked_ := False;
        Core_Thread_Dispatch_Critical__.Acquire;
        Data_.IsPicked__ := @IsPicked_;
        Data_.Pool_Queue_Ptr__ := Core_Thread_Dispatch_Queue_Pool__.Add(@Data_);
        Tick := GetTimeTick();
        Core_Thread_Dispatch_Critical__.Release;

        // check
        while (Core_Thread_Wait_Sum__.V > 0) and (GetTimeTick() - Tick < Core_Thread_Life_Time_Tick__) do
          begin
            Core_Thread_Dispatch_Critical__.Acquire;
            if IsPicked_ then
              begin
                Core_Thread_Dispatch_Critical__.Release;
                exit;
              end
            else
              begin
                Core_Thread_Dispatch_Critical__.Release;
              end;
          end;

        Core_Thread_Dispatch_Critical__.Acquire;
        if IsPicked_ then
          begin
            Core_Thread_Dispatch_Critical__.Release;
            exit;
          end
        else
          begin
            // remove
            Core_Thread_Dispatch_Queue_Pool__.Remove_P(Data_.Pool_Queue_Ptr__);
            Data_.IsPicked__ := nil;
            Core_Thread_Dispatch_Critical__.Release;
          end;
      end;

    // create thread
    Core_Thread_Dispatch_Critical__.Acquire;
    inc(Core_Thread_Task_Runing__.LockP()^);
    Core_Thread_Task_Runing__.Unlock;
    Th := Pick_Or_Create_Compute_Thread();
    Data_.AssignTo(Th);
    Th.Start();
    Core_Thread_Dispatch_Critical__.Release;
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
  Core_Thread_Life_Time_Tick__ := Tick_;
end;

procedure InitCoreThreadPool(Thread_Num, Parallel_Granularity_: Integer);
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
  Core_Thread_Task_Runing__ := TAtomInt.Create(0);
  Core_Thread_Dispatch_Queue_Pool__ := TCompute_Dispatch_Queue_Pool.Create;
  Core_Thread_Wait_Sum__ := TAtomInt.Create(0);
  Core_Thread_Life_Time_Tick__ := 200;
  Parallel_Granularity__ := Parallel_Granularity_;
  Core_Thread_Dispatch_Critical__ := TCritical.Create;
  Max_Activted_Parallel__ := 0;
  Parallel_Overflow__.ActivtedParallel := 0;

  // create dispatch thread
  Th := TCore_Dispatch_Order_Thread.Create;
  Th.Start();
{$IFDEF DEBUG}
  if IsConsole then
      WriteLn(Format('CPU_Thread=%d, Max_Thread=%s, Parallel_Granularity=%d IDLE_Thread_Time=%dms',
        [Thread_Num,
{$IFDEF LimitMaxComputeThread}
          IntToStr(Max_Thread_Supported),
{$ELSE LimitMaxComputeThread}
          'infinite',
{$ENDIF LimitMaxComputeThread}
          Parallel_Granularity__,
          Core_Thread_Life_Time_Tick__]));
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
      Check_Soft_Thread_Synchronize(10);
    end;

  Core_Thread_Pool__.Free;
  Core_Thread_Pool__ := nil;
  Core_Dispatch_Order__.Free;
  Core_Dispatch_Order__ := nil;

  Core_Thread_Task_Runing__.Free;
  Core_Thread_Task_Runing__ := nil;

  Core_Thread_Dispatch_Critical__.Free;
  Core_Thread_Dispatch_Critical__ := nil;

  Core_Thread_Dispatch_Queue_Pool__.Free;
  Core_Thread_Dispatch_Queue_Pool__ := nil;

  Core_Thread_Wait_Sum__.Free;
  Core_Thread_Wait_Sum__ := nil;
end;

procedure TCompute.Execute;
var
  Tick: TTimeTick;
  NoTask: Boolean;
begin
  FRndInstance := InternalMT19937__();
  AtomInc(PMD19937Core(FRndInstance)^.Instance_TMT19937Random__);
  FStart_Time_Tick := GetTimeTick();

  while True do
    begin
{$IFDEF MT19937SeedOnTComputeThreadIs0} SetMT19937Seed(0); {$ELSE MT19937SeedOnTComputeThreadIs0} MT19937Randomize(); {$ENDIF MT19937SeedOnTComputeThreadIs0}
      if IsRuning <> nil then
          IsRuning^ := True;
      if IsExit <> nil then
          IsExit^ := False;

      FThread_Info := '<none>'; // reset thread-info

      try
        if Assigned(OnRun_C) then
            OnRun_C(Self)
        else if Assigned(OnRun_M) then
            OnRun_M(Self)
        else if Assigned(OnRun_P) then
            OnRun_P(Self)
        else if Assigned(OnRun_C_NP) then
            OnRun_C_NP()
        else if Assigned(OnRun_M_NP) then
            OnRun_M_NP()
        else if Assigned(OnRun_P_NP) then
            OnRun_P_NP();
      except
      end;

      if Assigned(OnDone_C) or Assigned(OnDone_M) or Assigned(OnDone_P) then
          SyncM(Self, Done_Sync);

      if IsRuning <> nil then
          IsRuning^ := False;
      if IsExit <> nil then
          IsExit^ := True;

      // check for idle thread, and again run.
      NoTask := True;
      Core_Thread_Wait_Sum__.Critical.Inc_(Core_Thread_Wait_Sum__.P^);
      Tick := GetTimeTick;
      repeat
        Core_Thread_Dispatch_Critical__.Acquire;
        if Core_Thread_Dispatch_Queue_Pool__.Num > 0 then
          begin
            Core_Thread_Dispatch_Queue_Pool__.First^.Data^.AssignTo(Self);
            if Core_Thread_Dispatch_Queue_Pool__.First^.Data^.IsPicked__ <> nil then
                Core_Thread_Dispatch_Queue_Pool__.First^.Data^.IsPicked__^ := True;
            Core_Thread_Dispatch_Queue_Pool__.Next;
            NoTask := False;
          end;
        Core_Thread_Dispatch_Critical__.Release;
        if NoTask then
            Sleep(1);
      until (not NoTask) or (GetTimeTick - Tick > Core_Thread_Life_Time_Tick__);
      Core_Thread_Wait_Sum__.Critical.Dec_(Core_Thread_Wait_Sum__.P^);
      if NoTask then
          break;
    end;

  Core_Thread_Dispatch_Critical__.Acquire;
  Core_Thread_Pool__.Remove_P(Thread_Pool_Queue_Data_Ptr);
  Core_Thread_Dispatch_Critical__.Release;

  AtomDec(PMD19937Core(FRndInstance)^.Instance_TMT19937Random__);
  FRndInstance := nil;
  RemoveMT19937Thread(Self);

  dec(Core_Thread_Task_Runing__.LockP()^);
  Core_Thread_Task_Runing__.Unlock();
end;

procedure TCompute.Done_Sync;
begin
  try
    if Assigned(OnDone_C) then
        OnDone_C(Self)
    else if Assigned(OnDone_M) then
        OnDone_M(Self)
    else if Assigned(OnDone_P) then
        OnDone_P(Self);
  except
  end;
end;

class procedure TCompute.Set_Thread_Info(Thread_Info_: string);
var
  Th: TCore_Thread;
begin
  Th := TCore_Thread.CurrentThread;
  if (Th is TCompute) then
      TCompute(Th).FThread_Info := Thread_Info_;
end;

class procedure TCompute.Set_Thread_Info(const Fmt: string; const Args: array of const);
var
  n: string;
begin
  try
      n := Format(Fmt, Args);
  except
      n := Fmt;
  end;
  TCompute.Set_Thread_Info(n);
end;

constructor TCompute.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FSync_Tool := nil;
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
  FStart_Time_Tick := GetTimeTick();
  FThread_Info := '<none>';
  IsRuning := nil;
  IsExit := nil;
  UserData := nil;
  UserObject := nil;
end;

destructor TCompute.Destroy;
begin
  DisposeObjectAndNil(FSync_Tool);
  inherited Destroy;
end;

function TCompute.Sync_Tool: TSoft_Synchronize_Tool;
begin
  if Self = Core_Main_Thread then
      Result := MainThread_Sync_Tool
  else
    begin
      if FSync_Tool = nil then
          Sync_Tool := TSoft_Synchronize_Tool.Create(Self);
      Result := Sync_Tool;
    end;
end;

class function TCompute.Get_Core_Thread_Pool: TCoreCompute_Thread_Pool;
begin
  Result := Core_Thread_Pool__;
end;

class function TCompute.Get_Core_Thread_Dispatch_Critical: TCritical;
begin
  Result := Core_Thread_Dispatch_Critical__;
end;

class function TCompute.Wait_Thread(): NativeInt;
begin
  Result := Core_Thread_Wait_Sum__.V;
end;

class function TCompute.ActivtedTask(): NativeInt;
begin
  Result := Core_Thread_Task_Runing__.V;
end;

class function TCompute.WaitTask(): NativeInt;
begin
  Result := Core_Dispatch_Order__.Num;
end;

class function TCompute.TotalTask(): NativeInt;
begin
  Result := Core_Thread_Task_Runing__.V + Core_Dispatch_Order__.Num;
end;

class function TCompute.State(): string;
begin
  Result := Format('Task:%d Thread:%d/%d Wait:%d/%d Critical:%d/%d 19937:%d Atom:%d Parallel:%d/%d Post:%d Sync:%d',
    [TotalTask(), ActivtedTask(), Max_Thread_Supported, WaitTask(), Wait_Thread(),
      Get_System_Critical_Num - Get_System_Critical_Recycle_Pool_Num, Get_System_Critical_Recycle_Pool_Num,
      Get_MT19937_POOL_Num(), Get_Atom_Lock_Pool_Num(),
      Parallel_Overflow__.ActivtedParallel, Max_Activted_Parallel__,
      MainThreadProgress.Num, MainThread_Sync_Tool.SyncQueue__.Num]);
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
    OnRunC: TRun_Thread_C_NP;
    OnRunM: TRun_Thread_M_NP;
    OnRunP: TRun_Thread_P_NP;
    procedure DoSync;
  public
    constructor Create;
  end;

procedure TSyncTmp.DoSync;
begin
  try
    if Assigned(OnRunC) then
        OnRunC()
    else if Assigned(OnRunM) then
        OnRunM()
    else if Assigned(OnRunP) then
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

class procedure TCompute.Sync(OnRun_: TRun_Thread_P_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunP := OnRun_;
  if Used_Soft_Synchronize then
      MainThread_Sync_Tool.Synchronize_M(TCompute.CurrentThread, tmp.DoSync)
  else
      TCompute.Synchronize(TCompute.CurrentThread, tmp.DoSync);
end;

class procedure TCompute.Sync(Thread_: TThread; OnRun_: TRun_Thread_P_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunP := OnRun_;
  if Used_Soft_Synchronize then
      MainThread_Sync_Tool.Synchronize_M(Thread_, tmp.DoSync)
  else
      TCompute.Synchronize(Thread_, tmp.DoSync);
end;

class procedure TCompute.SyncC(OnRun_: TRun_Thread_C_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunC := OnRun_;
  if Used_Soft_Synchronize then
      MainThread_Sync_Tool.Synchronize_M(TCompute.CurrentThread, tmp.DoSync)
  else
      TCompute.Synchronize(TCompute.CurrentThread, tmp.DoSync);
end;

class procedure TCompute.SyncC(Thread_: TThread; OnRun_: TRun_Thread_C_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunC := OnRun_;
  if Used_Soft_Synchronize then
      MainThread_Sync_Tool.Synchronize_M(Thread_, tmp.DoSync)
  else
      TCompute.Synchronize(Thread_, tmp.DoSync);
end;

class procedure TCompute.SyncM(OnRun_: TRun_Thread_M_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunM := OnRun_;
  if Used_Soft_Synchronize then
      MainThread_Sync_Tool.Synchronize_M(TCompute.CurrentThread, tmp.DoSync)
  else
      TCompute.Synchronize(TCompute.CurrentThread, tmp.DoSync);
end;

class procedure TCompute.SyncM(Thread_: TThread; OnRun_: TRun_Thread_M_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunM := OnRun_;
  if Used_Soft_Synchronize then
      MainThread_Sync_Tool.Synchronize_M(Thread_, tmp.DoSync)
  else
      TCompute.Synchronize(Thread_, tmp.DoSync);
end;

class procedure TCompute.SyncP(OnRun_: TRun_Thread_P_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunP := OnRun_;
  if Used_Soft_Synchronize then
      MainThread_Sync_Tool.Synchronize_M(TCompute.CurrentThread, tmp.DoSync)
  else
      TCompute.Synchronize(TCompute.CurrentThread, tmp.DoSync);
end;

class procedure TCompute.SyncP(Thread_: TThread; OnRun_: TRun_Thread_P_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunP := OnRun_;
  if Used_Soft_Synchronize then
      MainThread_Sync_Tool.Synchronize_M(Thread_, tmp.DoSync)
  else
      TCompute.Synchronize(Thread_, tmp.DoSync);
end;

class procedure TCompute.Sync_To(Dest_Thread_: TCompute; OnRun_: TRun_Thread_P_NP);
begin
  Dest_Thread_.Sync_Tool.Synchronize(TCompute.CurrentThread, OnRun_);
end;

class procedure TCompute.SyncC_To(Dest_Thread_: TCompute; OnRun_: TRun_Thread_C_NP);
begin
  Dest_Thread_.Sync_Tool.Synchronize_C(TCompute.CurrentThread, OnRun_);
end;

class procedure TCompute.SyncM_To(Dest_Thread_: TCompute; OnRun_: TRun_Thread_M_NP);
begin
  Dest_Thread_.Sync_Tool.Synchronize_M(TCompute.CurrentThread, OnRun_);
end;

class procedure TCompute.SyncP_To(Dest_Thread_: TCompute; OnRun_: TRun_Thread_P_NP);
begin
  Dest_Thread_.Sync_Tool.Synchronize_P(TCompute.CurrentThread, OnRun_);
end;

class procedure TCompute.RunC(const Data: Pointer; const Obj: TObject; const OnRun, OnDone: TRun_Thread_C);
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

class procedure TCompute.RunC(const Data: Pointer; const Obj: TObject; const OnRun, OnDone: TRun_Thread_C; IsRuning_, IsExit_: PBoolean);
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

class procedure TCompute.RunC(const Data: Pointer; const Obj: TObject; const OnRun: TRun_Thread_C);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_C := OnRun;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunC(const Data: Pointer; const Obj: TObject; const OnRun: TRun_Thread_C; IsRuning_, IsExit_: PBoolean);
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

class procedure TCompute.RunC(const OnRun: TRun_Thread_C);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_C := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunC(const OnRun: TRun_Thread_C; IsRuning_, IsExit_: PBoolean);
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

class procedure TCompute.RunC_NP(const OnRun: TRun_Thread_C_NP);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_C_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunC_NP(const OnRun: TRun_Thread_C_NP; IsRuning_, IsExit_: PBoolean);
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

class procedure TCompute.RunM(const Data: Pointer; const Obj: TObject; const OnRun, OnDone: TRun_Thread_M);
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

class procedure TCompute.RunM(const Data: Pointer; const Obj: TObject; const OnRun, OnDone: TRun_Thread_M; IsRuning_, IsExit_: PBoolean);
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

class procedure TCompute.RunM(const Data: Pointer; const Obj: TObject; const OnRun: TRun_Thread_M);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_M := OnRun;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunM(const Data: Pointer; const Obj: TObject; const OnRun: TRun_Thread_M; IsRuning_, IsExit_: PBoolean);
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

class procedure TCompute.RunM(const OnRun: TRun_Thread_M);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_M := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunM(const OnRun: TRun_Thread_M; IsRuning_, IsExit_: PBoolean);
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

class procedure TCompute.RunM_NP(const OnRun: TRun_Thread_M_NP);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_M_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunM_NP(const OnRun: TRun_Thread_M_NP; IsRuning_, IsExit_: PBoolean);
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

class procedure TCompute.RunP(const Data: Pointer; const Obj: TObject; const OnRun, OnDone: TRun_Thread_P);
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

class procedure TCompute.RunP(const Data: Pointer; const Obj: TObject; const OnRun, OnDone: TRun_Thread_P; IsRuning_, IsExit_: PBoolean);
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

class procedure TCompute.RunP(const Data: Pointer; const Obj: TObject; const OnRun: TRun_Thread_P);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_P := OnRun;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunP(const Data: Pointer; const Obj: TObject; const OnRun: TRun_Thread_P; IsRuning_, IsExit_: PBoolean);
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

class procedure TCompute.RunP(const OnRun: TRun_Thread_P);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_P := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunP(const OnRun: TRun_Thread_P; IsRuning_, IsExit_: PBoolean);
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

class procedure TCompute.RunP_NP(const OnRun: TRun_Thread_P_NP);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRun_P_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunP_NP(const OnRun: TRun_Thread_P_NP; IsRuning_, IsExit_: PBoolean);
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

procedure Do_PostFreeObjectInThread(ThSender: TCompute);
begin
  DisposeObject(ThSender.UserObject);
end;

class procedure TCompute.PostFreeObjectInThread(const Obj: TObject);
begin
  if Obj = nil then
      exit;
  TCompute.RunC(nil, Obj, Do_PostFreeObjectInThread, nil);
end;

class procedure TCompute.PostFreeObjectInThreadAndNil(var Obj);
begin
  if TObject(Obj) <> nil then
    begin
      TCompute.PostFreeObjectInThread(TObject(Obj));
      TObject(Obj) := nil;
    end;
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

class procedure TCompute.PostC3(Data1: Pointer; Data2: TObject; Data3: Variant; OnSync: TThreadPost_C3);
begin
  MainThreadProgress.PostC3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.PostC3(Data1: Pointer; Data2: TObject; Data3: Variant; OnSync: TThreadPost_C3; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostC3(Data1, Data2, Data3, OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.PostC4(Data1: Pointer; Data2: TObject; OnSync: TThreadPost_C4);
begin
  MainThreadProgress.PostC4(Data1, Data2, OnSync);
end;

class procedure TCompute.PostC4(Data1: Pointer; Data2: TObject; OnSync: TThreadPost_C4; IsRuning_, IsExit_: PBoolean);
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

class procedure TCompute.PostM3(Data1: Pointer; Data2: TObject; Data3: Variant; OnSync: TThreadPost_M3);
begin
  MainThreadProgress.PostM3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.PostM3(Data1: Pointer; Data2: TObject; Data3: Variant; OnSync: TThreadPost_M3; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostM3(Data1, Data2, Data3, OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.PostM4(Data1: Pointer; Data2: TObject; OnSync: TThreadPost_M4);
begin
  MainThreadProgress.PostM4(Data1, Data2, OnSync);
end;

class procedure TCompute.PostM4(Data1: Pointer; Data2: TObject; OnSync: TThreadPost_M4; IsRuning_, IsExit_: PBoolean);
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

class procedure TCompute.PostP3(Data1: Pointer; Data2: TObject; Data3: Variant; OnSync: TThreadPost_P3);
begin
  MainThreadProgress.PostP3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.PostP3(Data1: Pointer; Data2: TObject; Data3: Variant; OnSync: TThreadPost_P3; IsRuning_, IsExit_: PBoolean);
begin
  MainThreadProgress.PostP3(Data1, Data2, Data3, OnSync, IsRuning_, IsExit_);
end;

class procedure TCompute.PostP4(Data1: Pointer; Data2: TObject; OnSync: TThreadPost_P4);
begin
  MainThreadProgress.PostP4(Data1, Data2, OnSync);
end;

class procedure TCompute.PostP4(Data1: Pointer; Data2: TObject; OnSync: TThreadPost_P4; IsRuning_, IsExit_: PBoolean);
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

class procedure TCompute.Sync_Wait_PostC3(Data1: Pointer; Data2: TObject; Data3: Variant; OnSync: TThreadPost_C3);
begin
  MainThreadProgress.Sync_Wait_PostC3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.Sync_Wait_PostC4(Data1: Pointer; Data2: TObject; OnSync: TThreadPost_C4);
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

class procedure TCompute.Sync_Wait_PostM3(Data1: Pointer; Data2: TObject; Data3: Variant; OnSync: TThreadPost_M3);
begin
  MainThreadProgress.Sync_Wait_PostM3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.Sync_Wait_PostM4(Data1: Pointer; Data2: TObject; OnSync: TThreadPost_M4);
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

class procedure TCompute.Sync_Wait_PostP3(Data1: Pointer; Data2: TObject; Data3: Variant; OnSync: TThreadPost_P3);
begin
  MainThreadProgress.Sync_Wait_PostP3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.Sync_Wait_PostP4(Data1: Pointer; Data2: TObject; OnSync: TThreadPost_P4);
begin
  MainThreadProgress.Sync_Wait_PostP4(Data1, Data2, OnSync);
end;
