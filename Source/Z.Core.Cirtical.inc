{ ****************************************************************************** }
{ * object lock                                                                * }
{ ****************************************************************************** }
// used Critical Simulate Atomic with TMonitor.Enter(obj) and TMonitor.Exit(obj)
// CriticalSimulateAtomic defined so performance to be reduced

// used soft Simulate Critical(ring)
// SoftCritical defined so performance to be reduced
constructor TSoftCritical.Create;
begin
  inherited Create;
  L := False;
end;

procedure TSoftCritical.Acquire;
{$IFDEF ANTI_DEAD_ATOMIC_LOCK}
var
  d: TTimeTick;
{$ENDIF ANTI_DEAD_ATOMIC_LOCK}
begin
{$IFDEF ANTI_DEAD_ATOMIC_LOCK}
  d := GetTimeTick;
  while L do
    if GetTimeTick - d < 5000 then
        TCompute.Sleep(1)
    else
        RaiseInfo('dead lock')
{$ELSE ANTI_DEAD_ATOMIC_LOCK}
  while L do
    TCompute.Sleep(1);
{$ENDIF ANTI_DEAD_ATOMIC_LOCK}
  L := True;
end;

procedure TSoftCritical.Release;
begin
  L := False;
end;

procedure TSoftCritical.Enter;
begin
  Acquire;
end;

procedure TSoftCritical.Leave;
begin
  Release;
end;

procedure TSoftCritical.Lock;
begin
  Acquire;
end;

procedure TSoftCritical.UnLock;
begin
  Release;
end;

type
  TSystem_Critical_Recycle_Pool__ = TOrderStruct<TSystem_Critical>;

var
  System_Critical__: TSystem_Critical;
  System_Critical_Recycle_Pool__: TSystem_Critical_Recycle_Pool__;
  System_Critical_Num__: NativeInt;

procedure Init_System_Critical_Recycle_Pool;
begin
  System_Critical__ := TSystem_Critical.Create;
  System_Critical_Recycle_Pool__ := TSystem_Critical_Recycle_Pool__.Create;
  System_Critical_Num__ := 0;
end;

procedure Free_System_Critical_Recycle_Pool;
begin
  while System_Critical_Recycle_Pool__.Num > 0 do
    begin
      System_Critical_Recycle_Pool__.First^.Data.Free;
      System_Critical_Recycle_Pool__.Next;
    end;
  System_Critical_Recycle_Pool__.Free;
  System_Critical_Recycle_Pool__ := nil;
  System_Critical__.Free;
  System_Critical__ := nil;
end;

function Get_System_Critical_Num: NativeInt;
begin
  Result := System_Critical_Num__;
end;

function Get_System_Critical_Recycle_Pool_Num: NativeInt;
begin
  Result := System_Critical_Recycle_Pool__.Num;
end;

constructor TCritical.Create;
begin
  inherited Create;
  System_Critical__.Acquire;
  if System_Critical_Recycle_Pool__.Num > 0 then
    begin
      Instance__ := System_Critical_Recycle_Pool__.First^.Data;
      System_Critical_Recycle_Pool__.Next;
    end
  else
    begin
      Instance__ := TSystem_Critical.Create;
      Inc(System_Critical_Num__);
    end;
  System_Critical__.Release;
  LNum := 0;
end;

destructor TCritical.Destroy;
begin
  while LNum > 0 do
      Release();
  System_Critical__.Acquire;
  System_Critical_Recycle_Pool__.Push(Instance__);
  System_Critical__.Release;
  inherited Destroy;
end;

procedure TCritical.Acquire;
begin
  Instance__.Acquire;
  Inc(LNum);
end;

procedure TCritical.Release;
begin
  Dec(LNum);
  Instance__.Release;
end;

procedure TCritical.Enter;
begin
  Acquire();
end;

procedure TCritical.Leave;
begin
  Release();
end;

procedure TCritical.Lock;
begin
  Acquire();
end;

procedure TCritical.UnLock;
begin
  Release();
end;

function TCritical.IsBusy: Boolean;
begin
  Result := LNum > 0;
end;

function TCritical.Get(var x: Int64): Int64;
begin
  Lock;
  Result := x;
  UnLock;
end;

function TCritical.Get(var x: UInt64): UInt64;
begin
  Lock;
  Result := x;
  UnLock;
end;

function TCritical.Get(var x: Integer): Integer;
begin
  Lock;
  Result := x;
  UnLock;
end;

function TCritical.Get(var x: Cardinal): Cardinal;
begin
  Lock;
  Result := x;
  UnLock;
end;

procedure TCritical.Inc_(var x: Int64);
begin
  Lock;
  Inc(x);
  UnLock;
end;

procedure TCritical.Inc_(var x: Int64; const v: Int64);
begin
  Lock;
  Inc(x, v);
  UnLock;
end;

procedure TCritical.Dec_(var x: Int64);
begin
  Lock;
  Dec(x);
  UnLock;
end;

procedure TCritical.Dec_(var x: Int64; const v: Int64);
begin
  Lock;
  Dec(x, v);
  UnLock;
end;

procedure TCritical.Inc_(var x: UInt64);
begin
  Lock;
  Inc(x);
  UnLock;
end;

procedure TCritical.Inc_(var x: UInt64; const v: UInt64);
begin
  Lock;
  Inc(x, v);
  UnLock;
end;

procedure TCritical.Dec_(var x: UInt64);
begin
  Lock;
  Dec(x);
  UnLock;
end;

procedure TCritical.Dec_(var x: UInt64; const v: UInt64);
begin
  Lock;
  Dec(x, v);
  UnLock;
end;

procedure TCritical.Inc_(var x: Integer);
begin
  Lock;
  Inc(x);
  UnLock;
end;

procedure TCritical.Inc_(var x: Integer; const v: Integer);
begin
  Lock;
  Inc(x, v);
  UnLock;
end;

procedure TCritical.Dec_(var x: Integer);
begin
  Lock;
  Dec(x);
  UnLock;
end;

procedure TCritical.Dec_(var x: Integer; const v: Integer);
begin
  Lock;
  Dec(x, v);
  UnLock;
end;

procedure TCritical.Inc_(var x: Cardinal);
begin
  Lock;
  Inc(x);
  UnLock;
end;

procedure TCritical.Inc_(var x: Cardinal; const v: Cardinal);
begin
  Lock;
  Inc(x, v);
  UnLock;
end;

procedure TCritical.Dec_(var x: Cardinal);
begin
  Lock;
  Dec(x);
  UnLock;
end;

procedure TCritical.Dec_(var x: Cardinal; const v: Cardinal);
begin
  Lock;
  Dec(x, v);
  UnLock;
end;

class procedure TSwap<T_>.Do_(var v1, v2: T_);
var
  tmp: T_;
begin
  tmp := v1;
  v1 := v2;
  v2 := tmp;
end;

class function TIF<T_>.Do_(Bool_: Boolean; Yes_, No_: T_): T_;
begin
  if Bool_ then
      Result := Yes_
  else
      Result := No_;
end;
