{ Aug 6, 2020 V8.65 Renamed InterlockedIncrement to AtomicIncrement and AtomicDecrement
                      for compilers XE2 and earlier that don't have them.  }
{ Aug 08, 2023 V9.0  Updated version to major release 9. }


{$IFNDEF COMPILER17_UP}
{$IFDEF CPUX64}

function AtomicIncrement(var Addend: Integer): Integer;
asm
       MOV   EAX,1
  LOCK XADD  [RCX].Integer,EAX
       INC   EAX
end;

function AtomicDecrement(var Addend: Integer): Integer;
asm
       MOV   EAX,-1
  LOCK XADD  [RCX].Integer,EAX
       DEC   EAX
end;

{$ELSE}

function AtomicIncrement(var Addend: Integer): Integer;
asm
       MOV   ECX,EAX
       MOV   EAX,1
  LOCK XADD  [ECX],EAX
       INC   EAX
end;

function AtomicDecrement(var Addend: Integer): Integer;
asm
       MOV   ECX,EAX
       MOV   EAX,-1
  LOCK XADD  [ECX],EAX
       DEC   EAX
end;

{$ENDIF CPUX86}
{$ENDIF COMPILER}



