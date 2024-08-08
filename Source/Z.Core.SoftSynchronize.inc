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
      RaiseInfo('Main-Thread Simulator error!');
  inherited Create;
  Running := False;
  MainThreadProgress.Progress();
  Do_Check_System_Thread_Synchronize(0, True);
  bak_ThreadID := Core_Main_Thread_ID;
  bak_Thread := Core_Main_Thread;
  Simulator_Main_Proc := Simulator_Main_Proc_;
  Simulator_Main_Thread_Activted___ := True;
  On_Check_System_Thread_Synchronize := nil;
  CPS_Check_Soft_Thread.Reset;
  CPS_Check_System_Thread.Reset;
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
  On_Check_System_Thread_Synchronize := Do_Check_System_Thread_Synchronize;
  CPS_Check_Soft_Thread.Reset;
  CPS_Check_System_Thread.Reset;
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
  sync_: TSynchronize_Data___; // anti dead loop
begin
  Result := 0;
  if (TCore_Thread.CurrentThread <> Soft_Synchronize_Main_Thread) then
      exit;
  if Check_Time_ > 0 then
      tk := GetTimeTick;
  repeat
    while SyncQueue__.Num > 0 do
      begin
        sync_ := SyncQueue__.First^.Data; // anti dead loop
        SyncQueue__.Next; // anti dead loop
        try
          if Assigned(sync_.Primary) then
              sync_.Primary()
          else if Assigned(sync_.Second) then
              sync_.Second()
          else if Assigned(sync_.Third) then
              sync_.Third();
        except
        end;
        sync_.Fourth^ := False;
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

function Do_Check_Soft_Thread_Synchronize(Timeout: TTimeTick; Run_Hook_Event_: Boolean): Boolean;
begin
  Result := False;
  if Used_Soft_Synchronize then
    begin
      if (TCore_Thread.CurrentThread <> MainThread_Sync_Tool.Soft_Synchronize_Main_Thread) then
        begin
          if TCore_Thread.CurrentThread.ThreadID = MainThreadID then
              Result := CheckSynchronize(Timeout)
          else if Timeout > 0 then
              TCore_Thread.Sleep(Timeout);
        end
      else if not Main_Thread_Synchronize_Running then
        begin
          Main_Thread_Synchronize_Running := True;
          CPS_Check_Soft_Thread.Begin_Caller;
          MainThreadProgress.Progress(Core_Main_Thread_ID);

          try
              MainThread_Sync_Tool.Check_Synchronize(Timeout);
          except
          end;

          if Run_Hook_Event_ then
            begin
              try
                if Assigned(OnCheckThreadSynchronize) then
                    OnCheckThreadSynchronize();
              except
              end;
            end;

          if Core_Main_Thread_ID = MainThreadID then
            begin
              try
                  Result := CheckSynchronize(if_(Timeout > 0, 1, 0)) or Result;
              except
              end;
            end;

          CPS_Check_Soft_Thread.End_Caller;
          Main_Thread_Synchronize_Running := False;
        end;
    end
  else
      Result := Check_System_Thread_Synchronize(Timeout, Run_Hook_Event_);
end;

function Do_Check_System_Thread_Synchronize(Timeout: TTimeTick; Run_Hook_Event_: Boolean): Boolean;
begin
  Result := False;
  if (TCore_Thread.CurrentThread.ThreadID <> Core_Main_Thread_ID) then
    begin
      if Used_Soft_Synchronize and (TCore_Thread.CurrentThread = MainThread_Sync_Tool.Soft_Synchronize_Main_Thread) then
          Result := (MainThread_Sync_Tool.Check_Synchronize(Timeout) > 0)
      else if Timeout > 0 then
          TCore_Thread.Sleep(Timeout);
    end
  else if Enabled_Check_Thread_Synchronize_System then
    begin
      if not Main_Thread_Synchronize_Running then
        begin
          Main_Thread_Synchronize_Running := True;

          CPS_Check_System_Thread.Begin_Caller;
          MainThreadProgress.Progress(Core_Main_Thread_ID);

          if Core_Main_Thread_ID = MainThreadID then
            begin
              try
                  Result := CheckSynchronize(Timeout);
              except
              end;
            end;

          if Used_Soft_Synchronize and (MainThread_Sync_Tool.Soft_Synchronize_Main_Thread = Core_Main_Thread) then
            begin
              try
                  Result := (MainThread_Sync_Tool.Check_Synchronize(if_(Timeout > 0, 1, 0)) > 0) or Result;
              except
              end;
            end;

          if Run_Hook_Event_ then
            begin
              try
                if Assigned(OnCheckThreadSynchronize) then
                    OnCheckThreadSynchronize();
              except
              end;
            end;

          CPS_Check_System_Thread.End_Caller;

          Main_Thread_Synchronize_Running := False;
        end;
    end;
end;
