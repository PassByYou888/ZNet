// *****************************************************************************************************************

constructor TPair_Pool{$IFNDEF FPC}<T1_, T2_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  List := TPair_BigList__.Create;
end;

destructor TPair_Pool{$IFNDEF FPC}<T1_, T2_>{$ENDIF FPC}.Destroy;
begin
  List.Free;
  inherited Destroy;
end;

// *****************************************************************************************************************

constructor TPair_Third_Pool{$IFNDEF FPC}<T1_, T2_, T3_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  List := TPair_BigList__.Create;
end;

destructor TPair_Third_Pool{$IFNDEF FPC}<T1_, T2_, T3_>{$ENDIF FPC}.Destroy;
begin
  List.Free;
  inherited Destroy;
end;

// *****************************************************************************************************************

constructor TPair_Fourth_Pool{$IFNDEF FPC}<T1_, T2_, T3_, T4_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  List := TPair_BigList__.Create;
end;

destructor TPair_Fourth_Pool{$IFNDEF FPC}<T1_, T2_, T3_, T4_>{$ENDIF FPC}.Destroy;
begin
  List.Free;
  inherited Destroy;
end;

// *****************************************************************************************************************

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Value_List(Key_: TKey_; var Key_Hash_: THash): TValue_Pair_Pool__;
var
  i: integer;
begin
  Key_Hash_ := Get_Key_Hash(Key_);
  i := Hash_Key_Mod(Key_Hash_, length(Hash_Buffer));
  if Hash_Buffer[i] = nil then
    begin
      Hash_Buffer[i] := TValue_Pair_Pool__.Create;
      Hash_Buffer[i].L.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Internal_Do_Free;
    end;
  Result := Hash_Buffer[i];
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Free_Value_List(Key_Hash_: THash);
begin
  DisposeObjectAndNil(Hash_Buffer[Hash_Key_Mod(Key_Hash_, length(Hash_Buffer))]);
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Data_Ptr(const Key_P: PKey_; var p: PByte; var Size: NativeInt);
begin
  p := PByte(Key_P);
  Size := SizeOf(TKey_);
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Internal_Do_Queue_Pool_Free(var Data: PPair_Pool_Value__);
begin
  if Data <> nil then
    begin
      Data^.Data.Third := nil;
      Data^.Instance___.Remove(Data);
    end;
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Internal_Do_Free(var Data: TPair);
begin
  if Data.Third <> nil then
    begin
      TPool_Queue_Ptr___(Data.Third)^.Data := nil;
      Queue_Pool.Remove(TPool_Queue_Ptr___(Data.Third));
    end;
  DoFree(Data.Primary, Data.Second);
end;

constructor TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Create(HashSize_: integer; Null_Value_: TValue_);
begin
  inherited Create;
  Queue_Pool := TPool___.Create;
  Queue_Pool.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Internal_Do_Queue_Pool_Free;
  SetLength(Hash_Buffer, HashSize_);
  Null_Value := Null_Value_;
  OnAdd := nil;
  OnFree := nil;
end;

destructor TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  Queue_Pool.Free;
  SetLength(Hash_Buffer, 0);
  inherited Destroy;
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.DoFree(var Key: TKey_; var Value: TValue_);
begin
  if Assigned(OnFree) then
      OnFree(Key, Value);
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.DoAdd(var Key: TKey_; var Value: TValue_);
begin
  if Assigned(OnAdd) then
      OnAdd(Key, Value);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Hash(Key_: TKey_): THash;
var
  p: PByte;
  Size: NativeInt;
begin
  Get_Key_Data_Ptr(@Key_, p, Size);
  Result := Get_CRC32(p, Size);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Compare_Key(Key_1, Key_2: TKey_): Boolean;
var
  p1, p2: PByte;
  s1, s2: NativeInt;
begin
  Get_Key_Data_Ptr(@Key_1, p1, s1);
  Get_Key_Data_Ptr(@Key_2, p2, s2);
  Result := CompareMemory(p1, p2, Min(s1, s2));
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Compare_Value(Value_1, Value_2: TValue_): Boolean;
begin
  Result := CompareMemory(@Value_1, @Value_2, SizeOf(TValue_));
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.SwapInstance(source: T___);
var
  tmp_Queue_Pool: TPool___;
  tmp_HashBuffer: TKey_Hash_Buffer;
  i: integer;
  L: TValue_Pair_Pool__;
begin
  tmp_Queue_Pool := Queue_Pool;
  tmp_HashBuffer := Hash_Buffer;

  Queue_Pool := source.Queue_Pool;
  Hash_Buffer := source.Hash_Buffer;

  source.Queue_Pool := tmp_Queue_Pool;
  source.Hash_Buffer := tmp_HashBuffer;

  for i := Low(Hash_Buffer) to high(Hash_Buffer) do
    begin
      L := Hash_Buffer[i];
      if L <> nil then
          L.L.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Internal_Do_Free;
    end;

  for i := Low(source.Hash_Buffer) to high(source.Hash_Buffer) do
    begin
      L := source.Hash_Buffer[i];
      if L <> nil then
          L.L.OnFree := {$IFDEF FPC}@{$ENDIF FPC}source.Internal_Do_Free;
    end;

  Queue_Pool.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Internal_Do_Queue_Pool_Free;
  source.Queue_Pool.OnFree := {$IFDEF FPC}@{$ENDIF FPC}source.Internal_Do_Queue_Pool_Free;
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Clear;
var
  i: integer;
begin
  for i := Low(Hash_Buffer) to high(Hash_Buffer) do
      DisposeObjectAndNil(Hash_Buffer[i]);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Exists_Key(Key: TKey_): Boolean;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := False;
  L := Get_Value_List(Key, Key_Hash_);
  if L.L.Num > 0 then
    begin
      i := 0;
      p := L.L.First;
      while i < L.L.Num do
        begin
          if (Key_Hash_ = p^.Data.Fourth) and Compare_Key(Key, p^.Data.Primary) then
            begin
              L.L.MoveToFirst(p);
              exit(True);
            end;
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Exists_Value(Data: TValue_): Boolean;
var
  L: TValue_Pair_Pool__;
  i, j: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := False;
  for i := low(Hash_Buffer) to high(Hash_Buffer) do
    begin
      L := Hash_Buffer[i];
      if (L <> nil) and (L.L.Num > 0) then
        begin
          j := 0;
          p := L.L.First;
          while j < L.L.Num do
            begin
              if Compare_Value(Data, p^.Data.Second) then
                  exit(True);
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Add(Key: TKey_; Value: TValue_; Overwrite_: Boolean): PPair_Pool_Value__;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  i: integer;
  p: PPair_Pool_Value__;
begin
  L := Get_Value_List(Key, Key_Hash_);

  if Overwrite_ and (L.L.Num > 0) then
    begin
      i := 0;
      p := L.L.First;
      while i < L.L.Num do
        begin
          if (Key_Hash_ = p^.Data.Fourth) and Compare_Key(Key, p^.Data.Primary) then
              L.L.Push_To_Recycle_Pool(p);
          Inc(i);
          p := p^.Next;
        end;
      L.L.Free_Recycle_Pool;
    end;

  L := Get_Value_List(Key, Key_Hash_);
  p := L.L.Add_Null();
  p^.Data.Primary := Key;
  p^.Data.Second := Value;
  p^.Data.Third := Queue_Pool.Add(p);
  p^.Data.Fourth := Key_Hash_;
  L.L.MoveToFirst(p);
  Result := p;
  DoAdd(p^.Data.Primary, p^.Data.Second);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Value(Key: TKey_): TValue_;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  R: TValue_;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  R := Null_Value;
  L := Get_Value_List(Key, Key_Hash_);
  if L.L.Num > 0 then
    begin
      i := 0;
      p := L.L.First;
      while i < L.L.Num do
        begin
          if (Key_Hash_ = p^.Data.Fourth) and Compare_Key(Key, p^.Data.Primary) then
            begin
              L.L.MoveToFirst(p);
              R := p^.Data.Second;
              break;
            end;
          Inc(i);
          p := p^.Next;
        end;
    end;
  Result := R;
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Set_Key_Value(Key: TKey_; Value: TValue_);
begin
  Add(Key, Value, True);
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Delete(Key: TKey_);
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  L := Get_Value_List(Key, Key_Hash_);
  if L.L.Num > 0 then
    begin
      i := 0;
      p := L.L.First;
      while i < L.L.Num do
        begin
          if (Key_Hash_ = p^.Data.Fourth) and Compare_Key(Key, p^.Data.Primary) then
              L.L.Push_To_Recycle_Pool(p);
          Inc(i);
          p := p^.Next;
        end;
    end;
  L.L.Free_Recycle_Pool;
  if L.L.Num <= 0 then
      Free_Value_List(Key_Hash_);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Num: NativeInt;
begin
  Result := Queue_Pool.Num;
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.GetSum: NativeInt;
var
  i: integer;
begin
  Result := 0;
  for i := Low(Hash_Buffer) to high(Hash_Buffer) do
    if Hash_Buffer[i] <> nil then
        Inc(Result, Hash_Buffer[i].L.Num);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Value_Ptr(Key: TKey_): PValue;
begin
  Result := Get_Value_Ptr(Key, Null_Value);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Value_Ptr(Key: TKey_; Default_: TValue_): PValue;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := nil;
  L := Get_Value_List(Key, Key_Hash_);
  if L.L.Num > 0 then
    begin
      i := 0;
      p := L.L.First;
      while i < L.L.Num do
        begin
          if (Key_Hash_ = p^.Data.Fourth) and Compare_Key(Key, p^.Data.Primary) then
            begin
              L.L.MoveToFirst(p);
              exit(@p^.Data.Second);
            end;
          Inc(i);
          p := p^.Next;
        end;
    end;

  if Result = nil then
    begin
      p := Add(Key, Default_, False);
      Result := @p^.Data.Second;
    end;
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Default_Value(Key: TKey_; Default_: TValue_): TValue_;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  R: TValue_;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  R := Default_;
  L := Get_Value_List(Key, Key_Hash_);
  if L.L.Num > 0 then
    begin
      i := 0;
      p := L.L.First;
      while i < L.L.Num do
        begin
          if (Key_Hash_ = p^.Data.Fourth) and Compare_Key(Key, p^.Data.Primary) then
            begin
              L.L.MoveToFirst(p);
              R := p^.Data.Second;
              break;
            end;
          Inc(i);
          p := p^.Next;
        end;
    end;
  Result := R;
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Set_Default_Value(Key: TKey_; Default_: TValue_);
begin
  Add(Key, Default_, True);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Repeat_(): TRepeat___;
begin
  Result := Queue_Pool.Repeat_();
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Repeat_(BI_, EI_: NativeInt): TRepeat___;
begin
  Result := Queue_Pool.Repeat_(BI_, EI_);
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.For_C(OnFor: TBig_Hash_Pool_For_C);
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
  Aborted: Boolean;
begin
  Aborted := False;
  for i := Low(Hash_Buffer) to high(Hash_Buffer) do
    begin
      L := Hash_Buffer[i];
      if (L <> nil) and (L.L.Num > 0) then
        begin
          j := 0;
          p := L.L.First;
          while j < L.L.Num do
            begin
              OnFor(p, Aborted);
              if Aborted then
                  exit;
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.For_M(OnFor: TBig_Hash_Pool_For_M);
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
  Aborted: Boolean;
begin
  Aborted := False;
  for i := Low(Hash_Buffer) to high(Hash_Buffer) do
    begin
      L := Hash_Buffer[i];
      if (L <> nil) and (L.L.Num > 0) then
        begin
          j := 0;
          p := L.L.First;
          while j < L.L.Num do
            begin
              OnFor(p, Aborted);
              if Aborted then
                  exit;
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.For_P(OnFor: TBig_Hash_Pool_For_P);
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
  Aborted: Boolean;
begin
  Aborted := False;
  for i := Low(Hash_Buffer) to high(Hash_Buffer) do
    begin
      L := Hash_Buffer[i];
      if (L <> nil) and (L.L.Num > 0) then
        begin
          j := 0;
          p := L.L.First;
          while j < L.L.Num do
            begin
              OnFor(p, Aborted);
              if Aborted then
                  exit;
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Push_To_Recycle_Pool(p: PPair_Pool_Value__);
begin
  p^.Instance___.Push_To_Recycle_Pool(p);
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Free_Recycle_Pool;
var
  i: integer;
  L: TValue_Pair_Pool__;
begin
  for i := Low(Hash_Buffer) to high(Hash_Buffer) do
    begin
      L := Hash_Buffer[i];
      if (L <> nil) then
        begin
          L.L.Free_Recycle_Pool();
          if L.L.Num <= 0 then
              DisposeObjectAndNil(Hash_Buffer[i]);
        end;
    end;
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.ToPool(): TPool___;
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := TPool___.Create;
  for i := Low(Hash_Buffer) to high(Hash_Buffer) do
    begin
      L := Hash_Buffer[i];
      if (L <> nil) and (L.L.Num > 0) then
        begin
          j := 0;
          p := L.L.First;
          while j < L.L.Num do
            begin
              Result.Add(@p^.Data);
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.ToArray_Key(): TArray_Key;
var
  i, k: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  k := 0;
  SetLength(Result, Num);
  for i := Low(Hash_Buffer) to high(Hash_Buffer) do
    begin
      L := Hash_Buffer[i];
      if (L <> nil) and (L.L.Num > 0) then
        begin
          j := 0;
          p := L.L.First;
          while j < L.L.Num do
            begin
              Result[k] := p^.Data.Primary;
              Inc(k);
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.ToOrder_Key(): TOrder_Key;
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := TOrder_Key.Create;
  for i := Low(Hash_Buffer) to high(Hash_Buffer) do
    begin
      L := Hash_Buffer[i];
      if (L <> nil) and (L.L.Num > 0) then
        begin
          j := 0;
          p := L.L.First;
          while j < L.L.Num do
            begin
              Result.Push(p^.Data.Primary);
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.ToArray_Value(): TArray_Value;
var
  i, k: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  k := 0;
  SetLength(Result, Num);
  for i := Low(Hash_Buffer) to high(Hash_Buffer) do
    begin
      L := Hash_Buffer[i];
      if (L <> nil) and (L.L.Num > 0) then
        begin
          j := 0;
          p := L.L.First;
          while j < L.L.Num do
            begin
              Result[k] := p^.Data.Second;
              Inc(k);
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.ToOrder_Value(): TOrder_Value;
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := TOrder_Value.Create;
  for i := Low(Hash_Buffer) to high(Hash_Buffer) do
    begin
      L := Hash_Buffer[i];
      if (L <> nil) and (L.L.Num > 0) then
        begin
          j := 0;
          p := L.L.First;
          while j < L.L.Num do
            begin
              Result.Push(p^.Data.Second);
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;


// *****************************************************************************************************************

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Value_List(Key_: TKey_; var Key_Hash_: THash): TValue_Pair_Pool__;
var
  i: integer;
begin
  Key_Hash_ := Get_Key_Hash(Key_);
  i := Hash_Key_Mod(Key_Hash_, length(Hash_Buffer));
  if Hash_Buffer[i] = nil then
    begin
      Hash_Buffer[i] := TValue_Pair_Pool__.Create;
      Hash_Buffer[i].L.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Internal_Do_Free;
    end;
  Result := Hash_Buffer[i];
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Free_Value_List(Key_Hash_: THash);
begin
  DisposeObjectAndNil(Hash_Buffer[Hash_Key_Mod(Key_Hash_, length(Hash_Buffer))]);
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Data_Ptr(const Key_P: PKey_; var p: PByte; var Size: NativeInt);
begin
  p := PByte(Key_P);
  Size := SizeOf(TKey_);
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Internal_Do_Queue_Pool_Free(var Data: PPair_Pool_Value__);
begin
  if Data <> nil then
    begin
      Data^.Data.Third := nil;
      Data^.Instance___.Remove(Data);
    end;
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Internal_Do_Free(var Data: TPair);
begin
  if Data.Third <> nil then
    begin
      TPool_Queue_Ptr___(Data.Third)^.Data := nil;
      Queue_Pool.Remove(TPool_Queue_Ptr___(Data.Third));
    end;
  DoFree(Data.Primary, Data.Second);
end;

constructor TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Create(HashSize_: integer; Null_Value_: TValue_);
begin
  inherited Create;
  Critical := TCritical.Create;
  Queue_Pool := TPool___.Create;
  Queue_Pool.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Internal_Do_Queue_Pool_Free;
  SetLength(Hash_Buffer, HashSize_);
  Null_Value := Null_Value_;
  OnAdd := nil;
  OnFree := nil;
end;

destructor TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  Queue_Pool.Free;
  SetLength(Hash_Buffer, 0);
  Critical.Free;
  inherited Destroy;
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.DoFree(var Key: TKey_; var Value: TValue_);
begin
  if Assigned(OnFree) then
      OnFree(Key, Value);
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.DoAdd(var Key: TKey_; var Value: TValue_);
begin
  if Assigned(OnAdd) then
      OnAdd(Key, Value);
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Hash(Key_: TKey_): THash;
var
  p: PByte;
  Size: NativeInt;
begin
  Get_Key_Data_Ptr(@Key_, p, Size);
  Result := Get_CRC32(p, Size);
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Compare_Key(Key_1, Key_2: TKey_): Boolean;
var
  p1, p2: PByte;
  s1, s2: NativeInt;
begin
  Get_Key_Data_Ptr(@Key_1, p1, s1);
  Get_Key_Data_Ptr(@Key_2, p2, s2);
  Result := CompareMemory(p1, p2, Min(s1, s2));
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Compare_Value(Value_1, Value_2: TValue_): Boolean;
begin
  Result := CompareMemory(@Value_1, @Value_2, SizeOf(TValue_));
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.SwapInstance(source: T___);
var
  tmp_Queue_Pool: TPool___;
  tmp_HashBuffer: TKey_Hash_Buffer;
  i: integer;
  L: TValue_Pair_Pool__;
begin
  Critical.Lock;
  try
    tmp_Queue_Pool := Queue_Pool;
    tmp_HashBuffer := Hash_Buffer;

    Queue_Pool := source.Queue_Pool;
    Hash_Buffer := source.Hash_Buffer;

    source.Queue_Pool := tmp_Queue_Pool;
    source.Hash_Buffer := tmp_HashBuffer;

    for i := Low(Hash_Buffer) to high(Hash_Buffer) do
      begin
        L := Hash_Buffer[i];
        if L <> nil then
            L.L.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Internal_Do_Free;
      end;

    for i := Low(source.Hash_Buffer) to high(source.Hash_Buffer) do
      begin
        L := source.Hash_Buffer[i];
        if L <> nil then
            L.L.OnFree := {$IFDEF FPC}@{$ENDIF FPC}source.Internal_Do_Free;
      end;

    Queue_Pool.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Internal_Do_Queue_Pool_Free;
    source.Queue_Pool.OnFree := {$IFDEF FPC}@{$ENDIF FPC}source.Internal_Do_Queue_Pool_Free;
  finally
      Critical.UnLock;
  end;
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Clear;
var
  i: integer;
begin
  Critical.Lock;
  try
    for i := Low(Hash_Buffer) to high(Hash_Buffer) do
        DisposeObjectAndNil(Hash_Buffer[i]);
  finally
      Critical.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Exists_Key(Key: TKey_): Boolean;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := False;
  Critical.Lock;
  try
    L := Get_Value_List(Key, Key_Hash_);
    if L.L.Num > 0 then
      begin
        i := 0;
        p := L.L.First;
        while i < L.L.Num do
          begin
            if (Key_Hash_ = p^.Data.Fourth) and Compare_Key(Key, p^.Data.Primary) then
              begin
                L.L.MoveToFirst(p);
                exit(True);
              end;
            Inc(i);
            p := p^.Next;
          end;
      end;
  finally
      Critical.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Exists_Value(Data: TValue_): Boolean;
var
  L: TValue_Pair_Pool__;
  i, j: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := False;
  Critical.Lock;
  try
    for i := low(Hash_Buffer) to high(Hash_Buffer) do
      begin
        L := Hash_Buffer[i];
        if (L <> nil) and (L.L.Num > 0) then
          begin
            j := 0;
            p := L.L.First;
            while j < L.L.Num do
              begin
                if Compare_Value(Data, p^.Data.Second) then
                    exit(True);
                Inc(j);
                p := p^.Next;
              end;
          end;
      end;
  finally
      Critical.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Add(Key: TKey_; Value: TValue_; Overwrite_: Boolean): PPair_Pool_Value__;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  i: integer;
  p: PPair_Pool_Value__;
begin
  Critical.Lock;
  try
    L := Get_Value_List(Key, Key_Hash_);

    if Overwrite_ and (L.L.Num > 0) then
      begin
        i := 0;
        p := L.L.First;
        while i < L.L.Num do
          begin
            if (Key_Hash_ = p^.Data.Fourth) and Compare_Key(Key, p^.Data.Primary) then
                L.L.Push_To_Recycle_Pool(p);
            Inc(i);
            p := p^.Next;
          end;
        L.L.Free_Recycle_Pool;
      end;

    L := Get_Value_List(Key, Key_Hash_);
    p := L.L.Add_Null();
    p^.Data.Primary := Key;
    p^.Data.Second := Value;
    p^.Data.Third := Queue_Pool.Add(p);
    p^.Data.Fourth := Key_Hash_;
    L.L.MoveToFirst(p);
    Result := p;
    DoAdd(p^.Data.Primary, p^.Data.Second);
  finally
      Critical.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Value(Key: TKey_): TValue_;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  R: TValue_;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  R := Null_Value;
  Critical.Lock;
  try
    L := Get_Value_List(Key, Key_Hash_);
    if L.L.Num > 0 then
      begin
        i := 0;
        p := L.L.First;
        while i < L.L.Num do
          begin
            if (Key_Hash_ = p^.Data.Fourth) and Compare_Key(Key, p^.Data.Primary) then
              begin
                L.L.MoveToFirst(p);
                R := p^.Data.Second;
                break;
              end;
            Inc(i);
            p := p^.Next;
          end;
      end;
  finally
      Critical.UnLock;
  end;
  Result := R;
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Set_Key_Value(Key: TKey_; Value: TValue_);
begin
  Add(Key, Value, True);
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Delete(Key: TKey_);
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  Critical.Lock;
  try
    L := Get_Value_List(Key, Key_Hash_);
    if L.L.Num > 0 then
      begin
        i := 0;
        p := L.L.First;
        while i < L.L.Num do
          begin
            if (Key_Hash_ = p^.Data.Fourth) and Compare_Key(Key, p^.Data.Primary) then
                L.L.Push_To_Recycle_Pool(p);
            Inc(i);
            p := p^.Next;
          end;
      end;
    L.L.Free_Recycle_Pool;
    if L.L.Num <= 0 then
        Free_Value_List(Key_Hash_);
  finally
      Critical.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Num: NativeInt;
begin
  Result := Queue_Pool.Num;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.GetSum: NativeInt;
var
  i: integer;
begin
  Result := 0;
  Critical.Lock;
  try
    for i := Low(Hash_Buffer) to high(Hash_Buffer) do
      if Hash_Buffer[i] <> nil then
          Inc(Result, Hash_Buffer[i].L.Num);
  finally
      Critical.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Value_Ptr(Key: TKey_): PValue;
begin
  Result := Get_Value_Ptr(Key, Null_Value);
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Value_Ptr(Key: TKey_; Default_: TValue_): PValue;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := nil;
  Critical.Lock;
  try
    L := Get_Value_List(Key, Key_Hash_);
    if L.L.Num > 0 then
      begin
        i := 0;
        p := L.L.First;
        while i < L.L.Num do
          begin
            if (Key_Hash_ = p^.Data.Fourth) and Compare_Key(Key, p^.Data.Primary) then
              begin
                L.L.MoveToFirst(p);
                exit(@p^.Data.Second);
              end;
            Inc(i);
            p := p^.Next;
          end;
      end;

    if Result = nil then
      begin
        p := Add(Key, Default_, False);
        Result := @p^.Data.Second;
      end;
  finally
      Critical.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Default_Value(Key: TKey_; Default_: TValue_): TValue_;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  R: TValue_;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  R := Default_;
  Critical.Lock;
  try
    L := Get_Value_List(Key, Key_Hash_);
    if L.L.Num > 0 then
      begin
        i := 0;
        p := L.L.First;
        while i < L.L.Num do
          begin
            if (Key_Hash_ = p^.Data.Fourth) and Compare_Key(Key, p^.Data.Primary) then
              begin
                L.L.MoveToFirst(p);
                R := p^.Data.Second;
                break;
              end;
            Inc(i);
            p := p^.Next;
          end;
      end;
  finally
      Critical.UnLock;
  end;
  Result := R;
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Set_Default_Value(Key: TKey_; Default_: TValue_);
begin
  Add(Key, Default_, True);
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Repeat_(): TRepeat___;
begin
  Result := Queue_Pool.Repeat_();
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Repeat_(BI_, EI_: NativeInt): TRepeat___;
begin
  Result := Queue_Pool.Repeat_(BI_, EI_);
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.For_C(OnFor: TBig_Hash_Pool_For_C);
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
  Aborted: Boolean;
begin
  Aborted := False;
  Critical.Lock;
  try
    for i := Low(Hash_Buffer) to high(Hash_Buffer) do
      begin
        L := Hash_Buffer[i];
        if (L <> nil) and (L.L.Num > 0) then
          begin
            j := 0;
            p := L.L.First;
            while j < L.L.Num do
              begin
                OnFor(p, Aborted);
                if Aborted then
                    exit;
                Inc(j);
                p := p^.Next;
              end;
          end;
      end;
  finally
      Critical.UnLock;
  end;
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.For_M(OnFor: TBig_Hash_Pool_For_M);
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
  Aborted: Boolean;
begin
  Aborted := False;
  Critical.Lock;
  try
    for i := Low(Hash_Buffer) to high(Hash_Buffer) do
      begin
        L := Hash_Buffer[i];
        if (L <> nil) and (L.L.Num > 0) then
          begin
            j := 0;
            p := L.L.First;
            while j < L.L.Num do
              begin
                OnFor(p, Aborted);
                if Aborted then
                    exit;
                Inc(j);
                p := p^.Next;
              end;
          end;
      end;
  finally
      Critical.UnLock;
  end;
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.For_P(OnFor: TBig_Hash_Pool_For_P);
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
  Aborted: Boolean;
begin
  Aborted := False;
  Critical.Lock;
  try
    for i := Low(Hash_Buffer) to high(Hash_Buffer) do
      begin
        L := Hash_Buffer[i];
        if (L <> nil) and (L.L.Num > 0) then
          begin
            j := 0;
            p := L.L.First;
            while j < L.L.Num do
              begin
                OnFor(p, Aborted);
                if Aborted then
                    exit;
                Inc(j);
                p := p^.Next;
              end;
          end;
      end;
  finally
      Critical.UnLock;
  end;
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Push_To_Recycle_Pool(p: PPair_Pool_Value__);
begin
  p^.Instance___.Push_To_Recycle_Pool(p);
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Free_Recycle_Pool;
var
  i: integer;
  L: TValue_Pair_Pool__;
begin
  Critical.Lock;
  try
    for i := Low(Hash_Buffer) to high(Hash_Buffer) do
      begin
        L := Hash_Buffer[i];
        if (L <> nil) then
          begin
            L.L.Free_Recycle_Pool();
            if L.L.Num <= 0 then
                DisposeObjectAndNil(Hash_Buffer[i]);
          end;
      end;
  finally
      Critical.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.ToPool(): TPool___;
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := TPool___.Create;
  Critical.Lock;
  try
    for i := Low(Hash_Buffer) to high(Hash_Buffer) do
      begin
        L := Hash_Buffer[i];
        if (L <> nil) and (L.L.Num > 0) then
          begin
            j := 0;
            p := L.L.First;
            while j < L.L.Num do
              begin
                Result.Add(@p^.Data);
                Inc(j);
                p := p^.Next;
              end;
          end;
      end;
  finally
      Critical.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.ToArray_Key(): TArray_Key;
var
  i, k: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  Critical.Lock;
  try
    k := 0;
    SetLength(Result, Queue_Pool.Num);
    for i := Low(Hash_Buffer) to high(Hash_Buffer) do
      begin
        L := Hash_Buffer[i];
        if (L <> nil) and (L.L.Num > 0) then
          begin
            j := 0;
            p := L.L.First;
            while j < L.L.Num do
              begin
                Result[k] := p^.Data.Primary;
                Inc(k);
                Inc(j);
                p := p^.Next;
              end;
          end;
      end;
  finally
      Critical.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.ToOrder_Key(): TOrder_Key;
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  Critical.Lock;
  try
    Result := TOrder_Key.Create;
    for i := Low(Hash_Buffer) to high(Hash_Buffer) do
      begin
        L := Hash_Buffer[i];
        if (L <> nil) and (L.L.Num > 0) then
          begin
            j := 0;
            p := L.L.First;
            while j < L.L.Num do
              begin
                Result.Push(p^.Data.Primary);
                Inc(j);
                p := p^.Next;
              end;
          end;
      end;
  finally
      Critical.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.ToArray_Value(): TArray_Value;
var
  i, k: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  Critical.Lock;
  try
    k := 0;
    SetLength(Result, Queue_Pool.Num);
    for i := Low(Hash_Buffer) to high(Hash_Buffer) do
      begin
        L := Hash_Buffer[i];
        if (L <> nil) and (L.L.Num > 0) then
          begin
            j := 0;
            p := L.L.First;
            while j < L.L.Num do
              begin
                Result[k] := p^.Data.Second;
                Inc(k);
                Inc(j);
                p := p^.Next;
              end;
          end;
      end;
  finally
      Critical.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.ToOrder_Value(): TOrder_Value;
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  Critical.Lock;
  try
    Result := TOrder_Value.Create;
    for i := Low(Hash_Buffer) to high(Hash_Buffer) do
      begin
        L := Hash_Buffer[i];
        if (L <> nil) and (L.L.Num > 0) then
          begin
            j := 0;
            p := L.L.First;
            while j < L.L.Num do
              begin
                Result.Push(p^.Data.Second);
                Inc(j);
                p := p^.Next;
              end;
          end;
      end;
  finally
      Critical.UnLock;
  end;
end;
