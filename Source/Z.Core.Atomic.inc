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

const
  C_CRC32Table: array [0 .. 255] of Cardinal = (
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535,
    $9E6495A3, $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD,
    $E7B82D07, $90BF1D91, $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D,
    $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4,
    $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C,
    $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC,
    $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB,
    $B6662D3D, $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F,
    $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB,
    $086D3D2D, $91646C97, $E6635C01, $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950, $8BBEB8EA,
    $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65, $4DB26158, $3AB551CE,
    $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A,
    $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409,
    $CE61E49F, $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81,
    $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739,
    $9DD277AF, $04DB2615, $73DC1683, $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, $F00F9344, $8708A3D2, $1E01F268,
    $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7, $FED41B76, $89D32BE0,
    $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8,
    $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF,
    $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703,
    $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7,
    $B5D0CF31, $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713, $95BF4A82, $E2B87A14, $7BB12BAE,
    $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242,
    $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777, $88085AE6,
    $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D,
    $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5,
    $47B2CF7F, $30B5FFE9, $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605,
    $CDD70693, $54DE5729, $23D967BF, $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
    );

function Get_CRC32(Data: PByte; Size: NativeInt): THash;
var
  p: PByte;
  Count: NativeInt;
  hash: THash;
begin
  p := Data;
  Count := Size;
  hash := $FFFFFFFF;
  while Count > 0 do
    begin
      hash := ((hash shr 8) and $00FFFFFF) xor C_CRC32Table[(hash xor p^) and $000000FF];
      Inc(p);
      Dec(Count);
    end;
  Result := hash xor $FFFFFFFF;
end;

function Hash_Key_Mod(const hash: THash; const Num: Integer): Integer;
begin
  Result := 0;
  if (Num > 0) and (hash > 0) then
    begin
      Result := hash mod Num;
      if Result > Num - 1 then
          Result := Num - 1;
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
