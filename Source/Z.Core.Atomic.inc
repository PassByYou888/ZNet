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

var
  Lock_Pool_Cirtical__: TCritical;
  Atom_Num_Critical__: TCritical;
  TimeTick_Critical__: TCritical;
  Lock_Pool__: TCritical_Struct_Pool__;

procedure TCritical_Struct_Pool__.DoFree(var Data: PCritical_Struct);
begin
  if Data = nil then
      exit;
  DisposeObject(Data^.Critical);
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
end;

procedure Free_Critical_System;
begin
{$IFDEF DEBUG}
  if IsConsole then
      Write('Free Critical');
{$ENDIF DEBUG}
  Lock_Pool__.Free;
  Lock_Pool__ := nil;
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

function Get_Object_Lock_Pool_Num: NativeInt;
begin
  Lock_Pool_Cirtical__.Acquire;
  Result := Lock_Pool__.Num;
  Lock_Pool_Cirtical__.Release;
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
  Critical_Result^.Critical := TCritical.Create;

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
      p^.Critical.Free;
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
