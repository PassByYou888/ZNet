{ ****************************************************************************** }
{ * trigger imp                                                                * }
{ ****************************************************************************** }
unit Z.Notify;

{$I Z.Define.inc}

interface

uses Variants, Z.Core, Z.DFE, Z.Cadencer;

type
  TNotifyBase = class(TCore_InterfacedObject)
  protected
    FNotifyList: TCore_ListForObj;
    FSaveRegisted: TCore_ListForObj;
    procedure DeleteSaveNotifyIntf(p: TNotifyBase);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RegisterNotify(v: TNotifyBase);
    procedure UnRegisterNotify(v: TNotifyBase);
    procedure DoExecute(const State: Variant); virtual;
    procedure NotifyExecute(Sender: TNotifyBase; const State: Variant); virtual;
  end;

  TNProgressPost = class;
  TNPostExecute = class;

  TNPostExecute_C = procedure(Sender: TNPostExecute);
  TNPostExecute_C_NP = procedure();
  TNPostExecute_M = procedure(Sender: TNPostExecute) of object;
  TNPostExecute_M_NP = procedure() of object;
{$IFDEF FPC}
  TNPostExecute_P = procedure(Sender: TNPostExecute) is nested;
  TNPostExecute_P_NP = procedure() is nested;
{$ELSE FPC}
  TNPostExecute_P = reference to procedure(Sender: TNPostExecute);
  TNPostExecute_P_NP = reference to procedure();
{$ENDIF FPC}

  TNPostExecute = class(TCore_Object)
  private
    FOwner: TNProgressPost;
    FDataEng: TDFE;
    ProcessedTime: Double;
  public
    Data1: TCore_Object;
    Data2: TCore_Object;
    Data3: Variant;
    Data4: Variant;
    Data5: Pointer;
    Delay: Double;

    OnExecute_C: TNPostExecute_C;
    OnExecute_C_NP: TNPostExecute_C_NP;
    OnExecute_M: TNPostExecute_M;
    OnExecute_M_NP: TNPostExecute_M_NP;
    OnExecute_P: TNPostExecute_P;
    OnExecute_P_NP: TNPostExecute_P_NP;
    property DataEng: TDFE read FDataEng;
    property Owner: TNProgressPost read FOwner;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute; virtual;
  end;

  TNPostExecuteClass = class of TNPostExecute;

  TNProgressPost = class(TCore_InterfacedObject)
  protected
    FPostProcessIsRun: Boolean;
    FPostExecuteList: TCore_ListForObj;
    FPostClass: TNPostExecuteClass;
    FBusy: Boolean;
    FCurrentExecute: TNPostExecute;
    FBreakProgress: Boolean;
    FPaused: Boolean;
    Critical: TCritical;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ResetPost;
    procedure Clear;
    procedure Clean;
    function PostExecute(): TNPostExecute; overload;
    function PostExecute(DataEng: TDFE): TNPostExecute; overload;
    function PostExecute(Delay: Double): TNPostExecute; overload;
    function PostExecute(Delay: Double; DataEng: TDFE): TNPostExecute; overload;
    function PostExecuteM(DataEng: TDFE; OnExecute_M: TNPostExecute_M): TNPostExecute; overload;
    function PostExecuteM(Delay: Double; DataEng: TDFE; OnExecute_M: TNPostExecute_M): TNPostExecute; overload;
    function PostExecuteM(Delay: Double; OnExecute_M: TNPostExecute_M): TNPostExecute; overload;
    function PostExecuteM_NP(Delay: Double; OnExecute_M: TNPostExecute_M_NP): TNPostExecute; overload;
    function PostExecuteC(DataEng: TDFE; OnExecute_C: TNPostExecute_C): TNPostExecute; overload;
    function PostExecuteC(Delay: Double; DataEng: TDFE; OnExecute_C: TNPostExecute_C): TNPostExecute; overload;
    function PostExecuteC(Delay: Double; OnExecute_C: TNPostExecute_C): TNPostExecute; overload;
    function PostExecuteC_NP(Delay: Double; OnExecute_C: TNPostExecute_C_NP): TNPostExecute; overload;
    function PostExecuteP(DataEng: TDFE; OnExecute_P: TNPostExecute_P): TNPostExecute; overload;
    function PostExecuteP(Delay: Double; DataEng: TDFE; OnExecute_P: TNPostExecute_P): TNPostExecute; overload;
    function PostExecuteP(Delay: Double; OnExecute_P: TNPostExecute_P): TNPostExecute; overload;
    function PostExecuteP_NP(Delay: Double; OnExecute_P: TNPostExecute_P_NP): TNPostExecute; overload;
    procedure PostDelayFreeObject(Delay: Double; Obj1_, Obj2_: TCore_Object);
    procedure Delete(p: TNPostExecute); overload; virtual;
    procedure Progress(deltaTime: Double);
    property Paused: Boolean read FPaused write FPaused;
    property Busy: Boolean read FBusy;
    property CurrentExecute: TNPostExecute read FCurrentExecute;
    property PostClass: TNPostExecuteClass read FPostClass write FPostClass;
  end;

  TCadencerNProgressPost = class(TNProgressPost, ICadencerProgressInterface)
  protected
    FCadencerEngine: TCadencer;
    procedure CadencerProgress(const deltaTime, newTime: Double);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Progress;
    property CadencerEngine: TCadencer read FCadencerEngine;
  end;

  TNProgressPostWithCadencer = TCadencerNProgressPost;
  TCadencerPost = TCadencerNProgressPost;

var
  SystemPostProgress: TCadencerNProgressPost;

function SysPostProgress: TCadencerNProgressPost;
function SysPost: TCadencerNProgressPost;
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

function SysPostProgress: TCadencerNProgressPost;
begin
  Result := SystemPostProgress;
end;

function SysPost: TCadencerNProgressPost;
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

procedure DoDelayFreeObject(Sender: TNPostExecute);
begin
  DisposeObject(Sender.Data1);
  DisposeObject(Sender.Data2);
end;

procedure TNotifyBase.DeleteSaveNotifyIntf(p: TNotifyBase);
var
  i: Integer;
begin
  i := 0;
  while i < FSaveRegisted.Count do
    begin
      if FSaveRegisted[i] = TNotifyBase(p) then
          FSaveRegisted.Delete(i)
      else
          inc(i);
    end;
end;

constructor TNotifyBase.Create;
begin
  inherited Create;
  FNotifyList := TCore_ListForObj.Create;
  FSaveRegisted := TCore_ListForObj.Create;
end;

destructor TNotifyBase.Destroy;
begin
  while FSaveRegisted.Count > 0 do
      TNotifyBase(FSaveRegisted[0]).UnRegisterNotify(Self);

  DisposeObject(FSaveRegisted);

  while FNotifyList.Count > 0 do
      UnRegisterNotify(TNotifyBase(FNotifyList[0]));

  DisposeObject(FNotifyList);
  inherited Destroy;
end;

procedure TNotifyBase.RegisterNotify(v: TNotifyBase);
begin
  UnRegisterNotify(v);
  FNotifyList.Add(v);
  v.FSaveRegisted.Add(Self);
end;

procedure TNotifyBase.UnRegisterNotify(v: TNotifyBase);
var
  i: Integer;
begin
  i := 0;
  while i < FNotifyList.Count do
    begin
      if FNotifyList[i] = TNotifyBase(v) then
          FNotifyList.Delete(i)
      else
          inc(i);
    end;
  v.DeleteSaveNotifyIntf(Self);
end;

procedure TNotifyBase.DoExecute(const State: Variant);
var
  i: Integer;
begin
  i := 0;
  while i < FNotifyList.Count do
    begin
      try
          TNotifyBase(FNotifyList[i]).NotifyExecute(Self, State);
      except
      end;
      inc(i);
    end;
end;

procedure TNotifyBase.NotifyExecute(Sender: TNotifyBase; const State: Variant);
begin

end;

constructor TNPostExecute.Create;
begin
  inherited Create;
  FDataEng := TDFE.Create;
  ProcessedTime := 0;
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

destructor TNPostExecute.Destroy;
var
  i: Integer;
begin
  if FOwner <> nil then
    begin
      if FOwner.CurrentExecute = Self then
          FOwner.FBreakProgress := True;
      i := 0;
      while i < FOwner.FPostExecuteList.Count do
        begin
          if FOwner.FPostExecuteList[i] = Self then
              FOwner.FPostExecuteList.Delete(i)
          else
              inc(i);
        end;
      FOwner := nil;
    end;
  DisposeObject(FDataEng);
  inherited Destroy;
end;

procedure TNPostExecute.Execute;
begin
  if Assigned(OnExecute_C) then
    begin
      FDataEng.Reader.index := 0;
      try
          OnExecute_C(Self);
      except
      end;
    end;

  if Assigned(OnExecute_C_NP) then
    begin
      FDataEng.Reader.index := 0;
      try
          OnExecute_C_NP();
      except
      end;
    end;

  if Assigned(OnExecute_M) then
    begin
      FDataEng.Reader.index := 0;
      try
          OnExecute_M(Self);
      except
      end;
    end;

  if Assigned(OnExecute_M_NP) then
    begin
      FDataEng.Reader.index := 0;
      try
          OnExecute_M_NP();
      except
      end;
    end;

  if Assigned(OnExecute_P) then
    begin
      FDataEng.Reader.index := 0;
      try
          OnExecute_P(Self);
      except
      end;
    end;
  if Assigned(OnExecute_P_NP) then
    begin
      FDataEng.Reader.index := 0;
      try
          OnExecute_P_NP();
      except
      end;
    end;
end;

constructor TNProgressPost.Create;
begin
  inherited Create;
  FPostProcessIsRun := False;
  FPostExecuteList := TCore_ListForObj.Create;
  FPostClass := TNPostExecute;
  FBusy := False;
  FCurrentExecute := nil;
  FBreakProgress := False;
  FPaused := False;
  Critical := TCritical.Create;
end;

destructor TNProgressPost.Destroy;
begin
  ResetPost;
  DisposeObject(FPostExecuteList);
  DisposeObject(Critical);
  inherited Destroy;
end;

procedure TNProgressPost.ResetPost;
var
  i: Integer;
begin
  Critical.Acquire; // atom
  try
    try
      for i := 0 to FPostExecuteList.Count - 1 do
        begin
          TNPostExecute(FPostExecuteList[i]).FOwner := nil;
          DisposeObject(FPostExecuteList[i]);
        end;

      FPostExecuteList.Clear;
    except
    end;
  finally
      Critical.Release; // atom
  end;
  FBreakProgress := True;
end;

procedure TNProgressPost.Clear;
begin
  ResetPost;
end;

procedure TNProgressPost.Clean;
begin
  ResetPost;
end;

function TNProgressPost.PostExecute(): TNPostExecute;
begin
  Result := FPostClass.Create;
  Result.FOwner := Self;
  Critical.Acquire; // atom
  try
      FPostExecuteList.Add(Result);
  finally
      Critical.Release; // atom
  end;
end;

function TNProgressPost.PostExecute(DataEng: TDFE): TNPostExecute;
begin
  Result := PostExecute();
  if DataEng <> nil then
      Result.FDataEng.Assign(DataEng);
end;

function TNProgressPost.PostExecute(Delay: Double): TNPostExecute;
begin
  Result := PostExecute();
  Result.Delay := Delay;
end;

function TNProgressPost.PostExecute(Delay: Double; DataEng: TDFE): TNPostExecute;
begin
  Result := PostExecute(Delay);
  if DataEng <> nil then
      Result.FDataEng.Assign(DataEng);
end;

function TNProgressPost.PostExecuteM(DataEng: TDFE; OnExecute_M: TNPostExecute_M): TNPostExecute;
begin
  Result := PostExecute(DataEng);
  Result.OnExecute_M := OnExecute_M;
end;

function TNProgressPost.PostExecuteM(Delay: Double; DataEng: TDFE; OnExecute_M: TNPostExecute_M): TNPostExecute;
begin
  Result := PostExecute(Delay, DataEng);
  Result.OnExecute_M := OnExecute_M;
end;

function TNProgressPost.PostExecuteM(Delay: Double; OnExecute_M: TNPostExecute_M): TNPostExecute;
begin
  Result := PostExecute(Delay);
  Result.OnExecute_M := OnExecute_M;
end;

function TNProgressPost.PostExecuteM_NP(Delay: Double; OnExecute_M: TNPostExecute_M_NP): TNPostExecute;
begin
  Result := PostExecute(Delay);
  Result.OnExecute_M_NP := OnExecute_M;
end;

function TNProgressPost.PostExecuteC(DataEng: TDFE; OnExecute_C: TNPostExecute_C): TNPostExecute;
begin
  Result := PostExecute(DataEng);
  Result.OnExecute_C := OnExecute_C;
end;

function TNProgressPost.PostExecuteC(Delay: Double; DataEng: TDFE; OnExecute_C: TNPostExecute_C): TNPostExecute;
begin
  Result := PostExecute(Delay, DataEng);
  Result.OnExecute_C := OnExecute_C;
end;

function TNProgressPost.PostExecuteC(Delay: Double; OnExecute_C: TNPostExecute_C): TNPostExecute;
begin
  Result := PostExecute(Delay);
  Result.OnExecute_C := OnExecute_C;
end;

function TNProgressPost.PostExecuteC_NP(Delay: Double; OnExecute_C: TNPostExecute_C_NP): TNPostExecute;
begin
  Result := PostExecute(Delay);
  Result.OnExecute_C_NP := OnExecute_C;
end;

function TNProgressPost.PostExecuteP(DataEng: TDFE; OnExecute_P: TNPostExecute_P): TNPostExecute;
begin
  Result := PostExecute(DataEng);
  Result.OnExecute_P := OnExecute_P;
end;

function TNProgressPost.PostExecuteP(Delay: Double; DataEng: TDFE; OnExecute_P: TNPostExecute_P): TNPostExecute;
begin
  Result := PostExecute(Delay, DataEng);
  Result.OnExecute_P := OnExecute_P;
end;

function TNProgressPost.PostExecuteP(Delay: Double; OnExecute_P: TNPostExecute_P): TNPostExecute;
begin
  Result := PostExecute(Delay);
  Result.OnExecute_P := OnExecute_P;
end;

function TNProgressPost.PostExecuteP_NP(Delay: Double; OnExecute_P: TNPostExecute_P_NP): TNPostExecute;
begin
  Result := PostExecute(Delay);
  Result.OnExecute_P_NP := OnExecute_P;
end;

procedure TNProgressPost.PostDelayFreeObject(Delay: Double; Obj1_, Obj2_: TCore_Object);
var
  tmp: TNPostExecute;
begin
  tmp := PostExecute(Delay);
  tmp.Data1 := Obj1_;
  tmp.Data2 := Obj2_;
  tmp.OnExecute_C := {$IFDEF FPC}@{$ENDIF FPC}DoDelayFreeObject;
end;

procedure TNProgressPost.Delete(p: TNPostExecute);
var
  i: Integer;
begin
  Critical.Acquire; // atom
  try
    i := 0;
    while i < FPostExecuteList.Count do
      begin
        if FPostExecuteList[i] = p then
          begin
            TNPostExecute(FPostExecuteList[i]).FOwner := nil;
            DisposeObject(FPostExecuteList[i]);
            FPostExecuteList.Delete(i);
          end
        else
            inc(i);
      end;
  finally
      Critical.Release; // atom
  end;
end;

procedure TNProgressPost.Progress(deltaTime: Double);
var
  i: Integer;
  L: TCore_ListForObj;
  p: TNPostExecute;
begin
  if FPaused then
      Exit;
  if FPostProcessIsRun then
      Exit;

  FPostProcessIsRun := True;
  FBreakProgress := False;

  L := TCore_ListForObj.Create;

  Critical.Acquire; // atom
  i := 0;
  try
    while i < FPostExecuteList.Count do
      begin
        p := FPostExecuteList[i] as TNPostExecute;
        p.ProcessedTime := p.ProcessedTime + deltaTime;
        if p.ProcessedTime >= p.Delay then
          begin
            L.Add(p);
            FPostExecuteList.Delete(i);
          end
        else
            inc(i);
      end;
  finally
      Critical.Release; // atom
  end;

  i := 0;
  while (i < L.Count) do
    begin
      FBusy := True;
      FCurrentExecute := TNPostExecute(L[i]);
      try
          FCurrentExecute.Execute;
      except
      end;
      FBusy := False;

      FCurrentExecute.FOwner := nil;

      try
          DisposeObject(FCurrentExecute);
      except
      end;

      inc(i);
      if FBreakProgress then
          Break;
    end;

  DisposeObject(L);

  FPostProcessIsRun := False;
end;

procedure TCadencerNProgressPost.CadencerProgress(const deltaTime, newTime: Double);
begin
  inherited Progress(deltaTime);
end;

constructor TCadencerNProgressPost.Create;
begin
  inherited Create;
  FCadencerEngine := TCadencer.Create;
  FCadencerEngine.OnProgressInterface := Self;
end;

destructor TCadencerNProgressPost.Destroy;
begin
  FCadencerEngine.OnProgressInterface := nil;
  DisposeObject(FCadencerEngine);
  inherited Destroy;
end;

procedure TCadencerNProgressPost.Progress;
begin
  FCadencerEngine.Progress;
end;

initialization

Hooked_OnCheckThreadSynchronize := Z.Core.OnCheckThreadSynchronize;
Z.Core.OnCheckThreadSynchronize := {$IFDEF FPC}@{$ENDIF FPC}DoCheckThreadSynchronize;
SystemPostProgress := TCadencerNProgressPost.Create;

finalization

Z.Core.OnCheckThreadSynchronize := Hooked_OnCheckThreadSynchronize;
DisposeObject(SystemPostProgress);

end.
