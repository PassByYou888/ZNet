// used Critical Simulate Atomic with TMonitor.Enter(obj) and TMonitor.Exit(obj)
// CriticalSimulateAtomic defined so performance to be reduced

// used soft Simulate Critical(ring)
// SoftCritical defined so performance to be reduced

{ ****************************************************************************** }
{ * object lock                                                                * }
{ ****************************************************************************** }
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
    if GetTimeTick - d >= 5000 then
        RaiseInfo('dead lock');
{$ELSE ANTI_DEAD_ATOMIC_LOCK}
  while L do
      NOP;
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

constructor TCritical.Create;
begin
  inherited Create;
  LNum := 0;
end;

destructor TCritical.Destroy;
begin
  inherited Destroy;
end;

procedure TCritical.Acquire;
begin
  inherited Acquire;
  Inc(LNum);
end;

procedure TCritical.Release;
begin
  Dec(LNum);
  inherited Release;
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

type
  PCritical_Struct = ^TCritical_Struct;
  TCritical_Struct_Pool_Decl__ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<PCritical_Struct>;

  TCritical_Struct_Pool__ = class(TCritical_Struct_Pool_Decl__)
  public
    procedure DoFree(var Data: PCritical_Struct); override;
  end;

  TCritical_Struct = record
    Obj: TObject;
    LEnter: Integer;
    Critical: TCritical;
    Pool_Ptr: TCritical_Struct_Pool_Decl__.PQueueStruct;
  end;

  TCritical_Recycle_Pool__ = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderStruct<TCritical>;

var
  Lock_Pool_Cirtical__: TCritical;
  Atom_Num_Critical__: TCritical;
  TimeTick_Critical__: TCritical;
  Lock_Pool__: TCritical_Struct_Pool__;
  Critical_Recycle_Pool__: TCritical_Recycle_Pool__;

procedure TCritical_Struct_Pool__.DoFree(var Data: PCritical_Struct);
begin
  if Data = nil then
      exit;
  Critical_Recycle_Pool__.Push(Data^.Critical);
  Dispose(Data);
end;

procedure Init_Critical_System;
begin
{$IFDEF DEBUG}
  if IsConsole then
      Writeln('Init Critical');
{$ENDIF DEBUG}
  Lock_Pool_Cirtical__ := TCritical.Create;
  Atom_Num_Critical__ := TCritical.Create;
  TimeTick_Critical__ := TCritical.Create;
  Lock_Pool__ := TCritical_Struct_Pool__.Create;
  Critical_Recycle_Pool__ := TCritical_Recycle_Pool__.Create;
end;

procedure Free_Critical_System;
begin
{$IFDEF DEBUG}
  if IsConsole then
      Write('Free Critical');
{$ENDIF DEBUG}
  Lock_Pool__.Free;
  Lock_Pool__ := nil;
  while Critical_Recycle_Pool__.Num > 0 do
    begin
      Critical_Recycle_Pool__.First^.Data.Free;
      Critical_Recycle_Pool__.Next;
{$IFDEF DEBUG}
      if IsConsole then
          Write('.');
{$ENDIF DEBUG}
    end;
  Critical_Recycle_Pool__.Free;
  Critical_Recycle_Pool__ := nil;
  Lock_Pool_Cirtical__.Free;
  Lock_Pool_Cirtical__ := nil;
  Atom_Num_Critical__.Free;
  Atom_Num_Critical__ := nil;
  TimeTick_Critical__.Free;
  TimeTick_Critical__ := nil;
{$IFDEF DEBUG}
  if IsConsole then
      Writeln('Done');
{$ENDIF DEBUG}
end;

procedure Get_Critical_Lock__(const Obj: TObject; var Critical_Result: PCritical_Struct; const Optimize: Boolean);
var
  found_Ptr: TCritical_Struct_Pool__.PQueueStruct;
{$IFDEF FPC}
  procedure do_fpc_progress_(Index_: NativeInt; p: TCritical_Struct_Pool__.PQueueStruct; var Aborted: Boolean);
  begin
    Aborted := p^.Data^.Obj = Obj;
    if Aborted then
        found_Ptr := p;
  end;
{$ENDIF FPC}


begin
  found_Ptr := nil;

{$IFDEF FPC}
  Lock_Pool__.Progress_P(@do_fpc_progress_);
{$ELSE FPC}
  Lock_Pool__.Progress_P(procedure(Index_: NativeInt; p: TCritical_Struct_Pool__.PQueueStruct; var Aborted: Boolean)
    begin
      Aborted := p^.Data^.Obj = Obj;
      if Aborted then
          found_Ptr := p;
    end);
{$ENDIF FPC}
  if found_Ptr <> nil then
    begin
      if Optimize then
          Lock_Pool__.MoveToFirst(found_Ptr);
      Critical_Result := found_Ptr^.Data;
      exit;
    end;

  new(Critical_Result);
  Critical_Result^.Obj := Obj;
  Critical_Result^.LEnter := 0;
  if Critical_Recycle_Pool__.Num > 0 then
    begin
      Critical_Result^.Critical := Critical_Recycle_Pool__.First^.Data;
      Critical_Recycle_Pool__.Next;
    end
  else
    begin
      Critical_Result^.Critical := TCritical.Create;
      Critical_Recycle_Pool__.Push(Critical_Result^.Critical);
    end;

  if Lock_Pool__.Num > 0 then
      Critical_Result^.Pool_Ptr := Lock_Pool__.Insert(Critical_Result, Lock_Pool__.First)
  else
      Critical_Result^.Pool_Ptr := Lock_Pool__.Add(Critical_Result);
end;

procedure Lock_Critical_Obj__(Obj: TObject);
var
  p: PCritical_Struct;
begin
  Lock_Pool_Cirtical__.Acquire;
  Get_Critical_Lock__(Obj, p, True);
  Inc(p^.LEnter);
  Lock_Pool_Cirtical__.Release;
  p^.Critical.Acquire;
end;

procedure UnLock_Critical_Obj__(Obj: TObject);
var
  p: PCritical_Struct;
begin
  Lock_Pool_Cirtical__.Acquire;
  Get_Critical_Lock__(Obj, p, False);
  Dec(p^.LEnter);
  if p^.LEnter = 0 then
    begin
      p^.Pool_Ptr^.Data := nil;
      Lock_Pool__.Remove(p^.Pool_Ptr);
      Lock_Pool_Cirtical__.Release;
      p^.Critical.Release;
      Lock_Pool_Cirtical__.Acquire;
      Critical_Recycle_Pool__.Push(p^.Critical);
      Lock_Pool_Cirtical__.Release;
      Dispose(p);
    end
  else if p^.LEnter > 0 then
    begin
      Lock_Pool_Cirtical__.Release;
      p^.Critical.Release;
    end
  else
    begin
      Lock_Pool_Cirtical__.Release;
      RaiseInfo('error: unlock failed: illegal unlock');
    end;
end;

function DeltaStep(const value_, Delta_: NativeInt): NativeInt;
begin
  if Delta_ > 0 then
      Result := (value_ + (Delta_ - 1)) and (not(Delta_ - 1))
  else
      Result := value_;
end;

procedure AtomInc(var x: Int64);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Inc(x);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicIncrement(x);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Int64; const v: Int64);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Inc(x, v);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicIncrement(x, v);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Int64);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Dec(x);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicDecrement(x);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Int64; const v: Int64);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Dec(x, v);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicDecrement(x, v);
{$ENDIF FPC}
end;

procedure AtomInc(var x: UInt64);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Inc(x);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicIncrement(x);
{$ENDIF FPC}
end;

procedure AtomInc(var x: UInt64; const v: UInt64);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Inc(x, v);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicIncrement(x, v);
{$ENDIF FPC}
end;

procedure AtomDec(var x: UInt64);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Dec(x);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicDecrement(x);
{$ENDIF FPC}
end;

procedure AtomDec(var x: UInt64; const v: UInt64);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Dec(x, v);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicDecrement(x, v);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Integer);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Inc(x);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicIncrement(x);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Integer; const v: Integer);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Inc(x, v);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicIncrement(x, v);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Integer);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Dec(x);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicDecrement(x);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Integer; const v: Integer);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Dec(x, v);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicDecrement(x, v);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Cardinal);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Inc(x);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicIncrement(x);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Cardinal; const v: Cardinal);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Inc(x, v);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicIncrement(x, v);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Cardinal);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Dec(x);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicDecrement(x);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Cardinal; const v: Cardinal);
begin
{$IFDEF FPC}
  Atom_Num_Critical__.Acquire;
  Dec(x, v);
  Atom_Num_Critical__.Release;
{$ELSE FPC}
  System.AtomicDecrement(x, v);
{$ENDIF FPC}
end;
