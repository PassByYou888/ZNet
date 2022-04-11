{ ****************************************************************************** }
{ * trigger imp                                                                * }
{ ****************************************************************************** }
unit Z.Notify;

{$I Z.Define.inc}

interface

uses Variants,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.DFE, Z.Cadencer;

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
  TN_Post_Execute_List_Struct = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalBigList<TN_Post_Execute>;
  TN_Post_Execute_Temp_Order_Struct = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderStruct<TN_Post_Execute>;

  TN_Post_Execute = class(TCore_Object)
  private
    FOwner: TN_Progress_Tool;
    FPool_Data_Ptr: TN_Post_Execute_List_Struct.PQueueStruct;
    FDFE_Inst: TDFE;
    FNewTime: Double;
  public
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

    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute; virtual;
  end;

  TN_Post_ExecuteClass = class of TN_Post_Execute;

  TN_Progress_Tool = class(TCore_InterfacedObject)
  protected
    FPostIsRun: Boolean;
    FPostExecuteList: TN_Post_Execute_List_Struct;
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
    function PostExecute(): TN_Post_Execute; overload;
    function PostExecute(DataEng: TDFE): TN_Post_Execute; overload;
    function PostExecute(Delay: Double): TN_Post_Execute; overload;
    function PostExecute(Delay: Double; DataEng: TDFE): TN_Post_Execute; overload;
    function PostExecuteM(DataEng: TDFE; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute; overload;
    function PostExecuteM(Delay: Double; DataEng: TDFE; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute; overload;
    function PostExecuteM(Delay: Double; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute; overload;
    function PostExecuteM_NP(Delay: Double; OnExecute_M: TN_Post_Execute_M_NP): TN_Post_Execute; overload;
    function PostExecuteC(DataEng: TDFE; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute; overload;
    function PostExecuteC(Delay: Double; DataEng: TDFE; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute; overload;
    function PostExecuteC(Delay: Double; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute; overload;
    function PostExecuteC_NP(Delay: Double; OnExecute_C: TN_Post_Execute_C_NP): TN_Post_Execute; overload;
    function PostExecuteP(DataEng: TDFE; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute; overload;
    function PostExecuteP(Delay: Double; DataEng: TDFE; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute; overload;
    function PostExecuteP(Delay: Double; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute; overload;
    function PostExecuteP_NP(Delay: Double; OnExecute_P: TN_Post_Execute_P_NP): TN_Post_Execute; overload;
    // state and dispatch
    procedure PostDelayFreeObject(Delay: Double; Obj1_, Obj2_: TCore_Object);
    procedure Remove(Inst_: TN_Post_Execute); overload; virtual;
    procedure Progress(deltaTime: Double);
    property Paused: Boolean read FPaused write FPaused;
    property Busy: Boolean read FBusy;
    property CurrentExecute: TN_Post_Execute read FCurrentExecute;
    property PostClass: TN_Post_ExecuteClass read FPostClass write FPostClass;
  end;

  TCadencer_N_Progress_Tool = class(TN_Progress_Tool, ICadencerProgressInterface)
  protected
    FCadencerEngine: TCadencer;
    procedure CadencerProgress(const deltaTime, newTime: Double);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Progress;
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

constructor TN_Post_Execute.Create;
begin
  inherited Create;
  FOwner := nil;
  FPool_Data_Ptr := nil;

  FDFE_Inst := TDFE.Create;
  FNewTime := 0;
  Data1 := nil;
  Data2 := nil;
  Data3 := Null;
  Data4 := Null;
  Data5 := nil;
  Delay := 0;

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
      if FOwner.CurrentExecute = Self then
          FOwner.FBreakProgress := True;

      if FPool_Data_Ptr <> nil then
        begin
          FPool_Data_Ptr^.Data := nil;
          FOwner.FPostExecuteList.Remove(FPool_Data_Ptr);
        end;
      FOwner := nil;
    end;
  DisposeObject(FDFE_Inst);
  inherited Destroy;
end;

procedure TN_Post_Execute.Execute;
begin
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
end;

constructor TN_Progress_Tool.Create;
begin
  inherited Create;
  FPostIsRun := False;
  FPostExecuteList := TN_Post_Execute_List_Struct.Create;
  FPostExecuteList.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Do_Free;
  FPostClass := TN_Post_Execute;
  FBusy := False;
  FCurrentExecute := nil;
  FBreakProgress := False;
  FPaused := False;
end;

destructor TN_Progress_Tool.Destroy;
begin
  ResetPost;
  DisposeObject(FPostExecuteList);
  inherited Destroy;
end;

procedure TN_Progress_Tool.Do_Free(var Inst_: TN_Post_Execute);
begin
  if Inst_ <> nil then
    begin
      Inst_.FPool_Data_Ptr := nil;
      DisposeObjectAndNil(Inst_);
    end;
end;

procedure TN_Progress_Tool.ResetPost;
begin
  FPostExecuteList.Clear;
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

function TN_Progress_Tool.PostExecute(): TN_Post_Execute;
begin
  Result := FPostClass.Create;
  Result.FOwner := Self;
  Result.FPool_Data_Ptr := FPostExecuteList.Add(Result);
end;

function TN_Progress_Tool.PostExecute(DataEng: TDFE): TN_Post_Execute;
begin
  Result := PostExecute();
  if DataEng <> nil then
      Result.FDFE_Inst.Assign(DataEng);
end;

function TN_Progress_Tool.PostExecute(Delay: Double): TN_Post_Execute;
begin
  Result := PostExecute();
  Result.Delay := Delay;
end;

function TN_Progress_Tool.PostExecute(Delay: Double; DataEng: TDFE): TN_Post_Execute;
begin
  Result := PostExecute(Delay);
  if DataEng <> nil then
      Result.FDFE_Inst.Assign(DataEng);
end;

function TN_Progress_Tool.PostExecuteM(DataEng: TDFE; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute;
begin
  Result := PostExecute(DataEng);
  Result.OnExecute_M := OnExecute_M;
end;

function TN_Progress_Tool.PostExecuteM(Delay: Double; DataEng: TDFE; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute;
begin
  Result := PostExecute(Delay, DataEng);
  Result.OnExecute_M := OnExecute_M;
end;

function TN_Progress_Tool.PostExecuteM(Delay: Double; OnExecute_M: TN_Post_Execute_M): TN_Post_Execute;
begin
  Result := PostExecute(Delay);
  Result.OnExecute_M := OnExecute_M;
end;

function TN_Progress_Tool.PostExecuteM_NP(Delay: Double; OnExecute_M: TN_Post_Execute_M_NP): TN_Post_Execute;
begin
  Result := PostExecute(Delay);
  Result.OnExecute_M_NP := OnExecute_M;
end;

function TN_Progress_Tool.PostExecuteC(DataEng: TDFE; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute;
begin
  Result := PostExecute(DataEng);
  Result.OnExecute_C := OnExecute_C;
end;

function TN_Progress_Tool.PostExecuteC(Delay: Double; DataEng: TDFE; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute;
begin
  Result := PostExecute(Delay, DataEng);
  Result.OnExecute_C := OnExecute_C;
end;

function TN_Progress_Tool.PostExecuteC(Delay: Double; OnExecute_C: TN_Post_Execute_C): TN_Post_Execute;
begin
  Result := PostExecute(Delay);
  Result.OnExecute_C := OnExecute_C;
end;

function TN_Progress_Tool.PostExecuteC_NP(Delay: Double; OnExecute_C: TN_Post_Execute_C_NP): TN_Post_Execute;
begin
  Result := PostExecute(Delay);
  Result.OnExecute_C_NP := OnExecute_C;
end;

function TN_Progress_Tool.PostExecuteP(DataEng: TDFE; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute;
begin
  Result := PostExecute(DataEng);
  Result.OnExecute_P := OnExecute_P;
end;

function TN_Progress_Tool.PostExecuteP(Delay: Double; DataEng: TDFE; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute;
begin
  Result := PostExecute(Delay, DataEng);
  Result.OnExecute_P := OnExecute_P;
end;

function TN_Progress_Tool.PostExecuteP(Delay: Double; OnExecute_P: TN_Post_Execute_P): TN_Post_Execute;
begin
  Result := PostExecute(Delay);
  Result.OnExecute_P := OnExecute_P;
end;

function TN_Progress_Tool.PostExecuteP_NP(Delay: Double; OnExecute_P: TN_Post_Execute_P_NP): TN_Post_Execute;
begin
  Result := PostExecute(Delay);
  Result.OnExecute_P_NP := OnExecute_P;
end;

procedure TN_Progress_Tool.PostDelayFreeObject(Delay: Double; Obj1_, Obj2_: TCore_Object);
var
  tmp: TN_Post_Execute;
begin
  tmp := PostExecute(Delay);
  tmp.Data1 := Obj1_;
  tmp.Data2 := Obj2_;
  tmp.OnExecute_C := {$IFDEF FPC}@{$ENDIF FPC}DoDelayFreeObject;
end;

procedure TN_Progress_Tool.Remove(Inst_: TN_Post_Execute);
begin
  DisposeObject(Inst_);
end;

procedure TN_Progress_Tool.Progress(deltaTime: Double);
var
  tmp_Order: TN_Post_Execute_Temp_Order_Struct;
{$IFDEF FPC}
  procedure do_fpc_Progress(Index_: NativeInt; p: TN_Post_Execute_List_Struct.PQueueStruct; var Aborted: Boolean);
  begin
    p^.Data.FNewTime := p^.Data.FNewTime + deltaTime;
    if p^.Data.FNewTime >= p^.Data.Delay then
        tmp_Order.Push(p^.Data);
  end;
{$ENDIF FPC}
  procedure Do_Run;
  var
    Free_Order: TN_Post_Execute_Temp_Order_Struct;
  begin
    Free_Order := TN_Post_Execute_Temp_Order_Struct.Create;
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
        Free_Order.Push(FCurrentExecute);
        tmp_Order.Next;
      end;
    while Free_Order.Num > 0 do
      begin
        DisposeObject(Free_Order.First^.Data);
        Free_Order.Next;
      end;
    Free_Order.Free;
  end;

begin
  if FPaused then
      Exit;
  if FPostIsRun then
      Exit;
  if FPostExecuteList.Num <= 0 then
      Exit;

  FPostIsRun := True;
  FBreakProgress := False;

  tmp_Order := TN_Post_Execute_Temp_Order_Struct.Create;
  try
{$IFDEF FPC}
    FPostExecuteList.Progress_P(@do_fpc_Progress);
{$ELSE FPC}
    FPostExecuteList.Progress_P(procedure(Index_: NativeInt; p: TN_Post_Execute_List_Struct.PQueueStruct; var Aborted: Boolean)
      begin
        p^.Data.FNewTime := p^.Data.FNewTime + deltaTime;
        if p^.Data.FNewTime >= p^.Data.Delay then
            tmp_Order.Push(p^.Data);
      end);
{$ENDIF FPC}
    Do_Run;
    tmp_Order.Free;
  finally
      FPostIsRun := False;
  end;
end;

procedure TCadencer_N_Progress_Tool.CadencerProgress(const deltaTime, newTime: Double);
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
DisposeObject(SystemPostProgress);

end.
