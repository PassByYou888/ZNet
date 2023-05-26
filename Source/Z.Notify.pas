{ ****************************************************************************** }
{ * notify and trigger imp                                                     * }
{ ****************************************************************************** }
unit Z.Notify;

{$I Z.Define.inc}

interface

uses Variants,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.DFE, Z.Cadencer;

type
  TN_Progress_Tool = class;
  TN_Post_Execute = class;
  TNProgressPost = TN_Progress_Tool;
  TNPostExecute = TN_Post_Execute;

  TN_Post_Execute_C = procedure(Sender: TN_Post_Execute);
  TN_Post_Execute_C_NP = procedure();
  TN_Post_Execute_M = procedure(Sender: TN_Post_Execute) of object;
  TN_Post_Execute_M_NP = procedure() of object;
{$IFDEF FPC}
  TN_Post_Execute_P = procedure(Sender: TN_Post_Execute) is nested;
  TN_Post_Execute_P_NP = procedure() is nested;
{$ELSE FPC}
  TN_Post_Execute_P = reference to procedure(Sender: TN_Post_Execute);
  TN_Post_Execute_P_NP = reference to procedure();
{$ENDIF FPC}
  TN_Post_Execute_List_Struct = {$IFDEF FPC}specialize {$ENDIF FPC} TCritical_BigList<TN_Post_Execute>;
  TN_Post_Execute_Temp_Order_Struct = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderStruct<TN_Post_Execute>;

  TN_Post_Execute = class(TCore_Object)
  private
    FOwner: TN_Progress_Tool;
    FPool_Data_Ptr: TN_Post_Execute_List_Struct.PQueueStruct;
    FDFE_Inst: TDFE;
    FNewTime: Double;
    FIsRuning, FIsExit: PBoolean;
    FIsReady: Boolean;
    procedure SetIsExit(const Value: PBoolean);
    procedure SetIsRuning(const Value: PBoolean);
  public
    Info: SystemString;
    Data1: TCore_Object;
    Data2: TCore_Object;
    Data3: Variant;
    Data4: Variant;
    Data5: Pointer;
    Delay: Double;
    OnExecute_C: TN_Post_Execute_C;
    OnExecute_C_NP: TN_Post_Execute_C_NP;
    OnExecute_M: TN_Post_Execute_M;
    OnExecute_M_NP: TN_Post_Execute_M_NP;
    OnExecute_P: TN_Post_Execute_P;
    OnExecute_P_NP: TN_Post_Execute_P_NP;
    property DataEng: TDFE read FDFE_Inst;
    property DFE_Inst: TDFE read FDFE_Inst;
    property Owner: TN_Progress_Tool read FOwner;
    property IsRuning: PBoolean read FIsRuning write SetIsRuning;
    property IsExit: PBoolean read FIsExit write SetIsExit;
    property IsReady: Boolean read FIsReady;
    property NewTime: Double read FNewTime;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute; virtual;
    procedure Ready;
  end;

  TN_Post_ExecuteClass = class of TN_Post_Execute;

  TN_Progress_Tool = class(TCore_InterfacedObject)
  protected
    FPostIsRun: Boolean;
    FPostExecute_Pool: TN_Post_Execute_List_Struct;
    FPostClass: TN_Post_ExecuteClass;
    FBusy: Boolean;
    FCurrentExecute: TN_Post_Execute;
    FBreakProgress: Boolean;
    FPaused: Boolean;
    procedure Do_Free(var Inst_: TN_Post_Execute);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ResetPost;
    procedure Clear;
    procedure Clean;
    // post
    function PostExecute(ready_: Boolean): TN_Post_Execute; overload;
    function PostExecute(ready_: Boolean; DataEng: TDFE): TN_Post_Execute; overload;
    function PostExecute(ready_: Boolean; Delay: Double): TN_Post_Execute; overload;
    function PostExecute(ready_: Boolean; Delay: Double; DataEng: TDFE): TN_Post_Execute; overload;
    //
    function PostExecuteC(DataEng: TDFE; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute; overload;
    function PostExecuteC(Delay: Double; DataEng: TDFE; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute; overload;
    function PostExecuteC(Delay: Double; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute; overload;
    function PostExecuteC_NP(Delay: Double; OnExecute_C: TN_Post_Execute_C_NP): TN_Post_Execute; overload;
    function PostExecuteC(ready_: Boolean; DataEng: TDFE; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute; overload;
    function PostExecuteC(ready_: Boolean; Delay: Double; DataEng: TDFE; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute; overload;
    function PostExecuteC(ready_: Boolean; Delay: Double; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute; overload;
    function PostExecuteC_NP(ready_: Boolean; Delay: Double; OnExecute_C: TN_Post_Execute_C_NP): TN_Post_Execute; overload;
    //
    function PostExecuteM(DataEng: TDFE; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute; overload;
    function PostExecuteM(Delay: Double; DataEng: TDFE; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute; overload;
    function PostExecuteM(Delay: Double; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute; overload;
    function PostExecuteM_NP(Delay: Double; OnExecute_M: TN_Post_Execute_M_NP): TN_Post_Execute; overload;
    function PostExecuteM(ready_: Boolean; DataEng: TDFE; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute; overload;
    function PostExecuteM(ready_: Boolean; Delay: Double; DataEng: TDFE; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute; overload;
    function PostExecuteM(ready_: Boolean; Delay: Double; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute; overload;
    function PostExecuteM_NP(ready_: Boolean; Delay: Double; OnExecute_M: TN_Post_Execute_M_NP): TN_Post_Execute; overload;
    //
    function PostExecuteP(DataEng: TDFE; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute; overload;
    function PostExecuteP(Delay: Double; DataEng: TDFE; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute; overload;
    function PostExecuteP(Delay: Double; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute; overload;
    function PostExecuteP_NP(Delay: Double; OnExecute_P: TN_Post_Execute_P_NP): TN_Post_Execute; overload;
    function PostExecuteP(ready_: Boolean; DataEng: TDFE; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute; overload;
    function PostExecuteP(ready_: Boolean; Delay: Double; DataEng: TDFE; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute; overload;
    function PostExecuteP(ready_: Boolean; Delay: Double; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute; overload;
    function PostExecuteP_NP(ready_: Boolean; Delay: Double; OnExecute_P: TN_Post_Execute_P_NP): TN_Post_Execute; overload;
    // state and dispatch
    procedure PostDelayFreeObject(Delay: Double; Obj1_, Obj2_: TCore_Object); overload;
    procedure PostDelayFreeObject(Delay: Double; Obj1_: TCore_Object); overload;
    procedure Remove(Inst_: TN_Post_Execute); overload; virtual;
    procedure Progress(deltaTime: Double); overload;
    property Paused: Boolean read FPaused write FPaused;
    property Busy: Boolean read FBusy;
    property CurrentExecute: TN_Post_Execute read FCurrentExecute;
    property PostClass: TN_Post_ExecuteClass read FPostClass write FPostClass;
  end;

  TCadencer_N_Progress_Tool = class(TN_Progress_Tool, ICadencerProgressInterface)
  protected
    FCadencerEngine: TCadencer;
    procedure CadencerProgress(const deltaTime, NewTime: Double);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Progress; overload;
    property CadencerEngine: TCadencer read FCadencerEngine;
  end;

  TN_Progress_ToolWithCadencer = TCadencer_N_Progress_Tool;
  TCadencerPost = TCadencer_N_Progress_Tool;
  TCadencerNProgressPost = TCadencer_N_Progress_Tool;
  TNProgressPostWithCadencer = TCadencerNProgressPost;

var
  SystemPostProgress: TCadencer_N_Progress_Tool;

function SysPostProgress: TCadencer_N_Progress_Tool;
function SysPost: TCadencer_N_Progress_Tool;
procedure DelayFreeObject(Delay: Double; Obj1_, Obj2_: TCore_Object); overload;
procedure DelayFreeObject(Delay: Double; Obj1_: TCore_Object); overload;
procedure DelayFreeObj(Delay: Double; Obj1_, Obj2_: TCore_Object); overload;
procedure DelayFreeObj(Delay: Double; Obj1_: TCore_Object); overload;

implementation

var
  Hooked_OnCheckThreadSynchronize: TOnCheckThreadSynchronize;

procedure DoCheckThreadSynchronize();
begin
  if Assigned(Hooked_OnCheckThreadSynchronize) then
    begin
      try
          Hooked_OnCheckThreadSynchronize();
      except
      end;
    end;
  SystemPostProgress.Progress;
end;

function SysPostProgress: TCadencer_N_Progress_Tool;
begin
  Result := SystemPostProgress;
end;

function SysPost: TCadencer_N_Progress_Tool;
begin
  Result := SystemPostProgress;
end;

procedure DelayFreeObject(Delay: Double; Obj1_, Obj2_: TCore_Object);
begin
  SystemPostProgress.PostDelayFreeObject(Delay, Obj1_, Obj2_);
end;

procedure DelayFreeObject(Delay: Double; Obj1_: TCore_Object);
begin
  SystemPostProgress.PostDelayFreeObject(Delay, Obj1_, nil);
end;

procedure DelayFreeObj(Delay: Double; Obj1_, Obj2_: TCore_Object);
begin
  SystemPostProgress.PostDelayFreeObject(Delay, Obj1_, Obj2_);
end;

procedure DelayFreeObj(Delay: Double; Obj1_: TCore_Object);
begin
  SystemPostProgress.PostDelayFreeObject(Delay, Obj1_, nil);
end;

procedure DoDelayFreeObject(Sender: TN_Post_Execute);
begin
  DisposeObject(Sender.Data1);
  DisposeObject(Sender.Data2);
end;

procedure TN_Post_Execute.SetIsExit(const Value: PBoolean);
begin
  FIsExit := Value;
  if FIsExit <> nil then
      FIsExit^ := False;
end;

procedure TN_Post_Execute.SetIsRuning(const Value: PBoolean);
begin
  FIsRuning := Value;
  if FIsRuning <> nil then
      FIsRuning^ := True;
end;

constructor TN_Post_Execute.Create;
begin
  inherited Create;
  FOwner := nil;
  FPool_Data_Ptr := nil;

  FDFE_Inst := TDFE.Create;
  FNewTime := 0;
  Info := '';
  Data1 := nil;
  Data2 := nil;
  Data3 := Null;
  Data4 := Null;
  Data5 := nil;
  Delay := 0;
  FIsRuning := nil;
  FIsExit := nil;
  FIsReady := False;

  OnExecute_C := nil;
  OnExecute_C_NP := nil;
  OnExecute_M := nil;
  OnExecute_M_NP := nil;
  OnExecute_P := nil;
  OnExecute_P_NP := nil;
end;

destructor TN_Post_Execute.Destroy;
begin
  if FOwner <> nil then
    begin
      if FOwner.FCurrentExecute = Self then
        begin
          FOwner.FBreakProgress := True;
          FOwner.FCurrentExecute := nil;
        end;

      if FPool_Data_Ptr <> nil then
        begin
          FPool_Data_Ptr^.Data := nil;
          FOwner.FPostExecute_Pool.Remove_P(FPool_Data_Ptr);
        end;
      FOwner := nil;
    end;
  DisposeObject(FDFE_Inst);
  inherited Destroy;
end;

procedure TN_Post_Execute.Execute;
begin
  if FIsRuning <> nil then
      FIsRuning^ := True;
  if FIsExit <> nil then
      FIsExit^ := False;

  if Assigned(OnExecute_C) then
    begin
      FDFE_Inst.Reader.index := 0;
      try
          OnExecute_C(Self);
      except
      end;
    end;

  if Assigned(OnExecute_C_NP) then
    begin
      FDFE_Inst.Reader.index := 0;
      try
          OnExecute_C_NP();
      except
      end;
    end;

  if Assigned(OnExecute_M) then
    begin
      FDFE_Inst.Reader.index := 0;
      try
          OnExecute_M(Self);
      except
      end;
    end;

  if Assigned(OnExecute_M_NP) then
    begin
      FDFE_Inst.Reader.index := 0;
      try
          OnExecute_M_NP();
      except
      end;
    end;

  if Assigned(OnExecute_P) then
    begin
      FDFE_Inst.Reader.index := 0;
      try
          OnExecute_P(Self);
      except
      end;
    end;
  if Assigned(OnExecute_P_NP) then
    begin
      FDFE_Inst.Reader.index := 0;
      try
          OnExecute_P_NP();
      except
      end;
    end;

  if FIsRuning <> nil then
      FIsRuning^ := False;
  if FIsExit <> nil then
      FIsExit^ := True;
end;

procedure TN_Post_Execute.Ready;
begin
  FIsReady := True;
end;

procedure TN_Progress_Tool.Do_Free(var Inst_: TN_Post_Execute);
begin
  if Inst_ <> nil then
    begin
      Inst_.FPool_Data_Ptr := nil;
      DisposeObjectAndNil(Inst_);
    end;
end;

constructor TN_Progress_Tool.Create;
begin
  inherited Create;
  FPostIsRun := False;
  FPostExecute_Pool := TN_Post_Execute_List_Struct.Create;
  FPostExecute_Pool.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Do_Free;
  FPostClass := TN_Post_Execute;
  FBusy := False;
  FCurrentExecute := nil;
  FBreakProgress := False;
  FPaused := False;
end;

destructor TN_Progress_Tool.Destroy;
begin
  ResetPost;
  DisposeObject(FPostExecute_Pool);
  inherited Destroy;
end;

procedure TN_Progress_Tool.ResetPost;
begin
  FPostExecute_Pool.Clear;
  FBreakProgress := True;
end;

procedure TN_Progress_Tool.Clear;
begin
  ResetPost;
end;

procedure TN_Progress_Tool.Clean;
begin
  ResetPost;
end;

function TN_Progress_Tool.PostExecute(ready_: Boolean): TN_Post_Execute;
begin
  Result := FPostClass.Create;
  Result.FOwner := Self;
  Result.FPool_Data_Ptr := FPostExecute_Pool.Add(Result);
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecute(ready_: Boolean; DataEng: TDFE): TN_Post_Execute;
begin
  Result := PostExecute(False);
  if DataEng <> nil then
      Result.FDFE_Inst.Assign(DataEng);
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecute(ready_: Boolean; Delay: Double): TN_Post_Execute;
begin
  Result := PostExecute(False);
  Result.Delay := Delay;
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecute(ready_: Boolean; Delay: Double; DataEng: TDFE): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay);
  if DataEng <> nil then
      Result.FDFE_Inst.Assign(DataEng);
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecuteC(DataEng: TDFE; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute;
begin
  Result := PostExecute(False, DataEng);
  Result.OnExecute_C := OnExecute_C;
  Result.Ready;
end;

function TN_Progress_Tool.PostExecuteC(Delay: Double; DataEng: TDFE; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay, DataEng);
  Result.OnExecute_C := OnExecute_C;
  Result.Ready;
end;

function TN_Progress_Tool.PostExecuteC(Delay: Double; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay);
  Result.OnExecute_C := OnExecute_C;
  Result.Ready;
end;

function TN_Progress_Tool.PostExecuteC_NP(Delay: Double; OnExecute_C: TN_Post_Execute_C_NP): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay);
  Result.OnExecute_C_NP := OnExecute_C;
  Result.Ready;
end;

function TN_Progress_Tool.PostExecuteC(ready_: Boolean; DataEng: TDFE; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute;
begin
  Result := PostExecute(False, DataEng);
  Result.OnExecute_C := OnExecute_C;
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecuteC(ready_: Boolean; Delay: Double; DataEng: TDFE; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay, DataEng);
  Result.OnExecute_C := OnExecute_C;
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecuteC(ready_: Boolean; Delay: Double; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay);
  Result.OnExecute_C := OnExecute_C;
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecuteC_NP(ready_: Boolean; Delay: Double; OnExecute_C: TN_Post_Execute_C_NP): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay);
  Result.OnExecute_C_NP := OnExecute_C;
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecuteM(DataEng: TDFE; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute;
begin
  Result := PostExecute(False, DataEng);
  Result.OnExecute_M := OnExecute_M;
  Result.Ready;
end;

function TN_Progress_Tool.PostExecuteM(Delay: Double; DataEng: TDFE; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay, DataEng);
  Result.OnExecute_M := OnExecute_M;
  Result.Ready;
end;

function TN_Progress_Tool.PostExecuteM(Delay: Double; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay);
  Result.OnExecute_M := OnExecute_M;
  Result.Ready;
end;

function TN_Progress_Tool.PostExecuteM_NP(Delay: Double; OnExecute_M: TN_Post_Execute_M_NP): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay);
  Result.OnExecute_M_NP := OnExecute_M;
  Result.Ready;
end;

function TN_Progress_Tool.PostExecuteM(ready_: Boolean; DataEng: TDFE; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute;
begin
  Result := PostExecute(False, DataEng);
  Result.OnExecute_M := OnExecute_M;
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecuteM(ready_: Boolean; Delay: Double; DataEng: TDFE; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay, DataEng);
  Result.OnExecute_M := OnExecute_M;
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecuteM(ready_: Boolean; Delay: Double; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay);
  Result.OnExecute_M := OnExecute_M;
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecuteM_NP(ready_: Boolean; Delay: Double; OnExecute_M: TN_Post_Execute_M_NP): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay);
  Result.OnExecute_M_NP := OnExecute_M;
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecuteP(DataEng: TDFE; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute;
begin
  Result := PostExecute(False, DataEng);
  Result.OnExecute_P := OnExecute_P;
  Result.Ready;
end;

function TN_Progress_Tool.PostExecuteP(Delay: Double; DataEng: TDFE; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay, DataEng);
  Result.OnExecute_P := OnExecute_P;
  Result.Ready;
end;

function TN_Progress_Tool.PostExecuteP(Delay: Double; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay);
  Result.OnExecute_P := OnExecute_P;
  Result.Ready;
end;

function TN_Progress_Tool.PostExecuteP_NP(Delay: Double; OnExecute_P: TN_Post_Execute_P_NP): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay);
  Result.OnExecute_P_NP := OnExecute_P;
  Result.Ready;
end;

function TN_Progress_Tool.PostExecuteP(ready_: Boolean; DataEng: TDFE; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute;
begin
  Result := PostExecute(False, DataEng);
  Result.OnExecute_P := OnExecute_P;
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecuteP(ready_: Boolean; Delay: Double; DataEng: TDFE; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay, DataEng);
  Result.OnExecute_P := OnExecute_P;
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecuteP(ready_: Boolean; Delay: Double; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay);
  Result.OnExecute_P := OnExecute_P;
  if ready_ then
      Result.Ready;
end;

function TN_Progress_Tool.PostExecuteP_NP(ready_: Boolean; Delay: Double; OnExecute_P: TN_Post_Execute_P_NP): TN_Post_Execute;
begin
  Result := PostExecute(False, Delay);
  Result.OnExecute_P_NP := OnExecute_P;
  if ready_ then
      Result.Ready;
end;

procedure TN_Progress_Tool.PostDelayFreeObject(Delay: Double; Obj1_, Obj2_: TCore_Object);
var
  tmp: TN_Post_Execute;
begin
  tmp := PostExecute(False, Delay);
  tmp.Data1 := Obj1_;
  tmp.Data2 := Obj2_;
  tmp.OnExecute_C := {$IFDEF FPC}@{$ENDIF FPC}DoDelayFreeObject;
  tmp.Ready;
end;

procedure TN_Progress_Tool.PostDelayFreeObject(Delay: Double; Obj1_: TCore_Object);
var
  tmp: TN_Post_Execute;
begin
  tmp := PostExecute(False, Delay);
  tmp.Data1 := Obj1_;
  tmp.Data2 := nil;
  tmp.OnExecute_C := {$IFDEF FPC}@{$ENDIF FPC}DoDelayFreeObject;
  tmp.Ready;
end;

procedure TN_Progress_Tool.Remove(Inst_: TN_Post_Execute);
begin
  DisposeObject(Inst_);
end;

procedure TN_Progress_Tool.Progress(deltaTime: Double);
var
  tmp_Order: TN_Post_Execute_Temp_Order_Struct;

  procedure Do_Run;
  var
    backup_state: Boolean;
  begin
    while tmp_Order.Num > 0 do
      begin
        FCurrentExecute := tmp_Order.First^.Data;
        if not FBreakProgress then
          begin
            FBusy := True;
            try
                FCurrentExecute.Execute;
            except
            end;
            FBusy := False;
          end;
        backup_state := FBreakProgress;
        DisposeObject(FCurrentExecute);
        FBreakProgress := backup_state;
        tmp_Order.Next;
      end;
  end;

var
  __Repeat__: TN_Post_Execute_List_Struct.TRepeat___;
begin
  if FPaused then
      Exit;
  if FPostIsRun then
      Exit;
  if FPostExecute_Pool.Num <= 0 then
      Exit;

  FPostIsRun := True;
  FBreakProgress := False;

  tmp_Order := TN_Post_Execute_Temp_Order_Struct.Create;
  try
    __Repeat__ := FPostExecute_Pool.Repeat_;
    repeat
      if __Repeat__.Queue^.Data.IsReady then
        begin
          __Repeat__.Queue^.Data.FNewTime := __Repeat__.Queue^.Data.FNewTime + deltaTime;
          if (__Repeat__.Queue^.Data.FNewTime >= __Repeat__.Queue^.Data.Delay) then
              tmp_Order.Push(__Repeat__.Queue^.Data);
        end;
    until not __Repeat__.Next;
    Do_Run();
    tmp_Order.Free;
  finally
      FPostIsRun := False;
  end;
end;

procedure TCadencer_N_Progress_Tool.CadencerProgress(const deltaTime, NewTime: Double);
begin
  inherited Progress(deltaTime);
end;

constructor TCadencer_N_Progress_Tool.Create;
begin
  inherited Create;
  FCadencerEngine := TCadencer.Create;
  FCadencerEngine.OnProgressInterface := Self;
end;

destructor TCadencer_N_Progress_Tool.Destroy;
begin
  FCadencerEngine.OnProgressInterface := nil;
  DisposeObject(FCadencerEngine);
  inherited Destroy;
end;

procedure TCadencer_N_Progress_Tool.Progress;
begin
  FCadencerEngine.Progress;
end;

initialization

Hooked_OnCheckThreadSynchronize := Z.Core.OnCheckThreadSynchronize;
Z.Core.OnCheckThreadSynchronize := {$IFDEF FPC}@{$ENDIF FPC}DoCheckThreadSynchronize;
SystemPostProgress := TCadencer_N_Progress_Tool.Create;

finalization

Z.Core.OnCheckThreadSynchronize := Hooked_OnCheckThreadSynchronize;
SystemPostProgress.Progress(10);
SystemPostProgress.Progress(10);
SystemPostProgress.Progress(10);
SystemPostProgress.Progress(10);
SystemPostProgress.Progress(10);
SystemPostProgress.Progress(10);
SystemPostProgress.Progress(10);
SystemPostProgress.Progress(10);
SystemPostProgress.Progress(10);
SystemPostProgress.Progress(10);
DisposeObject(SystemPostProgress);

end.
