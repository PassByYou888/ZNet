var
  Simulator_Main_Thread_Activted___: Boolean = False;

type
  TSimulator_Main_Thread = class
  public
    Running: Boolean;
    bak_ThreadID: TThreadID;
    bak_Thread: TCore_Thread;
    Simulator_Main_Proc: TOnSynchronize_C_NP;
    constructor Create(Simulator_Main_Proc_: TOnSynchronize_C_NP);
    procedure Do_Execute_Main_Thread(Sender: TCompute);
  end;

constructor TSimulator_Main_Thread.Create(Simulator_Main_Proc_: TOnSynchronize_C_NP);
begin
  if (not Used_Soft_Synchronize) or (Simulator_Main_Thread_Activted___) then
      RaiseInfo('error!');
  inherited Create;
  Running := False;
  MainThreadProgress.Progress();
  Do_Check_System_Thread_Synchronize(0, True);
  bak_ThreadID := Core_Main_Thread_ID;
  bak_Thread := Core_Main_Thread;
  Simulator_Main_Proc := Simulator_Main_Proc_;
  Simulator_Main_Thread_Activted___ := True;
  TCompute.RunM(nil, nil, Do_Execute_Main_Thread, @Simulator_Main_Thread_Activted___, nil);
end;

procedure TSimulator_Main_Thread.Do_Execute_Main_Thread(Sender: TCompute);
begin
  TCompute.Set_Thread_Info('Main-Thread Simulator');
  Core_Main_Thread_ID := Sender.ThreadID;
  Core_Main_Thread := Sender;
  MainThreadProgress.ThreadID := Sender.ThreadID;
  MainThread_Sync_Tool.Soft_Synchronize_Main_Thread := Sender;
  try
      Simulator_Main_Proc();
  except
  end;
  Do_Check_Soft_Thread_Synchronize(100, True);
  Core_Main_Thread_ID := bak_ThreadID;
  Core_Main_Thread := bak_Thread;
  MainThreadProgress.ThreadID := Core_Main_Thread_ID;
  MainThread_Sync_Tool.Soft_Synchronize_Main_Thread := Core_Main_Thread;
  Free;
end;

procedure Begin_Simulator_Main_Thread(Simulator_Main_Proc_: TOnSynchronize_C_NP);
begin
  if not Simulator_Main_Thread_Activted___ then
      TSimulator_Main_Thread.Create(Simulator_Main_Proc_);
end;

function Simulator_Main_Thread_Activted: Boolean;
begin
  Result := Simulator_Main_Thread_Activted___;
end;

constructor TSoft_Synchronize_Tool.Create(Soft_Synchronize_Main_Thread_: TCore_Thread);
begin
  inherited Create;
  SyncQueue__ := TSynchronize_Queue___.Create;
  Soft_Synchronize_Main_Thread := Soft_Synchronize_Main_Thread_;
end;

destructor TSoft_Synchronize_Tool.Destroy;
begin
  DisposeObjectAndNil(SyncQueue__);
  inherited Destroy;
end;

function TSoft_Synchronize_Tool.Check_Synchronize(Check_Time_: TTimeTick): NativeInt;
var
  tk: TTimeTick;
begin
  Result := 0;
  if TCore_Thread.CurrentThread <> Soft_Synchronize_Main_Thread then
      exit;
  if Check_Time_ > 0 then
      tk := GetTimeTick;
  repeat
    while SyncQueue__.Num > 0 do
      begin
        try
          if Assigned(SyncQueue__.First^.Data.Primary) then
              SyncQueue__.First^.Data.Primary();
          if Assigned(SyncQueue__.First^.Data.Second) then
              SyncQueue__.First^.Data.Second();
          if Assigned(SyncQueue__.First^.Data.Third) then
              SyncQueue__.First^.Data.Third();
        except
        end;
        SyncQueue__.First^.Data.Fourth^ := False;
        SyncQueue__.Next;
        inc(Result);
      end;
  until (Check_Time_ = 0) or (GetTimeTick - tk >= Check_Time_);
end;

procedure TSoft_Synchronize_Tool.Synchronize(Thread_: TCore_Thread; OnSync: TOnSynchronize_P_NP);
begin
  Synchronize_P(Thread_, OnSync);
end;

procedure TSoft_Synchronize_Tool.Synchronize_C(Thread_: TCore_Thread; OnSync: TOnSynchronize_C_NP);
var
  Wait_Signal__: Boolean;
begin
  if Thread_ = Soft_Synchronize_Main_Thread then
    begin
      try
        Check_Synchronize(0);
        OnSync();
      except
      end;
    end
  else
    begin
      Wait_Signal__ := True;
      SyncQueue__.Add(TSynchronize_Data___.Init(OnSync, nil, nil, @Wait_Signal__));
      while Wait_Signal__ do
          ;
    end;
end;

procedure TSoft_Synchronize_Tool.Synchronize_M(Thread_: TCore_Thread; OnSync: TOnSynchronize_M_NP);
var
  Wait_Signal__: Boolean;
begin
  if Thread_ = Soft_Synchronize_Main_Thread then
    begin
      try
        Check_Synchronize(0);
        OnSync();
      except
      end;
    end
  else
    begin
      Wait_Signal__ := True;
      SyncQueue__.Add(TSynchronize_Data___.Init(nil, OnSync, nil, @Wait_Signal__));
      while Wait_Signal__ do
          ;
    end;
end;

procedure TSoft_Synchronize_Tool.Synchronize_P(Thread_: TCore_Thread; OnSync: TOnSynchronize_P_NP);
var
  Wait_Signal__: Boolean;
begin
  if Thread_ = Soft_Synchronize_Main_Thread then
    begin
      try
        Check_Synchronize(0);
        OnSync();
      except
      end;
    end
  else
    begin
      Wait_Signal__ := True;
      SyncQueue__.Add(TSynchronize_Data___.Init(nil, nil, OnSync, @Wait_Signal__));
      while Wait_Signal__ do
          ;
    end;
end;
