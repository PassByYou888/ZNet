constructor TPair_Pool{$IFNDEF FPC}<T1_, T2_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  List := TPair_BigList__.Create;
  List.OnFree := {$IFDEF FPC}@{$ENDIF FPC}DoFree;
  OnFree := nil;
end;

destructor TPair_Pool{$IFNDEF FPC}<T1_, T2_>{$ENDIF FPC}.Destroy;
begin
  List.Free;
  inherited Destroy;
end;

procedure TPair_Pool{$IFNDEF FPC}<T1_, T2_>{$ENDIF FPC}.DoFree(var Data: TPair);
begin
  if Assigned(OnFree) then
      OnFree(Data);
end;

function TPair_Pool{$IFNDEF FPC}<T1_, T2_>{$ENDIF FPC}.Add(Primary: T1_; Second: T2_): PPair__;
begin
  Result := List.Add_Null;
  Result^.Data.Primary := Primary;
  Result^.Data.Second := Second;
  Result^.Data.Data := Result;
end;

constructor TCritical_Pair_Pool{$IFNDEF FPC}<T1_, T2_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  List := TPair_BigList__.Create;
  List.OnFree := {$IFDEF FPC}@{$ENDIF FPC}DoFree;
  OnFree := nil;
end;

destructor TCritical_Pair_Pool{$IFNDEF FPC}<T1_, T2_>{$ENDIF FPC}.Destroy;
begin
  List.Free;
  inherited Destroy;
end;

procedure TCritical_Pair_Pool{$IFNDEF FPC}<T1_, T2_>{$ENDIF FPC}.DoFree(var Data: TPair);
begin
  if Assigned(OnFree) then
      OnFree(Data);
end;

function TCritical_Pair_Pool{$IFNDEF FPC}<T1_, T2_>{$ENDIF FPC}.Add(Primary: T1_; Second: T2_): PPair__;
begin
  Result := List.Add_Null;
  Result^.Data.Primary := Primary;
  Result^.Data.Second := Second;
  Result^.Data.Data := Result;
end;

function TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Value_List(Key_P: PKey_): TValue_Pair_Pool__;
var
  i: integer;
begin
  i := Hash_Key_Mod(Get_Key_Hash(Key_P), Length(HashBuffer));
  if HashBuffer[i] = nil then
    begin
      HashBuffer[i] := TValue_Pair_Pool__.Create;
      HashBuffer[i].OnFree := {$IFDEF FPC}@{$ENDIF FPC}DoFree;
    end;
  Result := HashBuffer[i];
end;

procedure TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Free_Value_List(Key_P: PKey_);
begin
  DisposeObjectAndNil(HashBuffer[Hash_Key_Mod(Get_Key_Hash(Key_P), Length(HashBuffer))]);
end;

constructor TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Create(HashSize_: integer; Null_Value_: TValue_);
begin
  inherited Create;
  Null_Value := Null_Value_;
  SetLength(HashBuffer, HashSize_);
  OnFree := nil;
end;

destructor TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  SetLength(HashBuffer, 0);
  inherited Destroy;
end;

function TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Hash(Key_P: PKey_): THash;
var
  p: PByte;
  Size: NativeInt;
begin
  Get_Key_Data_Ptr(Key_P, p, Size);
  Result := Get_CRC32(p, Size);
end;

function TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Compare_Key(Key_P1, Key_P2: PKey_): Boolean;
var
  p1, p2: PByte;
  s1, s2: NativeInt;
begin
  Get_Key_Data_Ptr(Key_P1, p1, s1);
  Get_Key_Data_Ptr(Key_P2, p2, s2);
  Result := CompareMemory(p1, p2, Min(s1, s2));
end;

procedure TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.DoFree(var Data: TValue_Pair_Pool__.TPair);
begin
  if Assigned(OnFree) then
      OnFree(Data.Second);
end;

procedure TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Data_Ptr(Key_P: PKey_; var p: PByte; var Size: NativeInt);
begin
  p := PByte(Key_P);
  Size := SizeOf(TKey_);
end;

procedure TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.SwapInstance(source: T__);
var
  tmp_HashBuffer: TKey_Hash_Buffer;
begin
  tmp_HashBuffer := HashBuffer;
  HashBuffer := source.HashBuffer;
  source.HashBuffer := tmp_HashBuffer;
end;

procedure TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Clear;
var
  i: integer;
begin
  for i := Low(HashBuffer) to high(HashBuffer) do
      DisposeObjectAndNil(HashBuffer[i]);
end;

procedure TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Resize_Hash_Pool(HashSize_: integer);
var
  tmp: T__;
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  tmp := T__.Create(HashSize_, Null_Value);
  for i := Low(HashBuffer) to high(HashBuffer) do
    begin
      L := HashBuffer[i];
      if (L <> nil) and (L.List.Num > 0) then
        begin
          j := 0;
          p := L.List.First;
          while j < L.List.Num do
            begin
              tmp.Set_Key_Value(p^.Data.Primary, p^.Data.Second);
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
  SwapInstance(tmp);
  tmp.Free;
end;

function TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Exists(Key: TKey_): Boolean;
var
  L: TValue_Pair_Pool__;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := False;
  L := Get_Value_List(@Key);
  if L.List.Num > 0 then
    begin
      i := 0;
      p := L.List.First;
      while i < L.List.Num do
        begin
          if Compare_Key(@Key, @p^.Data.Primary) then
              Exit(True);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

function TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Value(Key: TKey_): TValue_;
var
  L: TValue_Pair_Pool__;
  R: TValue_;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  R := Null_Value;
  L := Get_Value_List(@Key);
  if L.List.Num > 0 then
    begin
      i := 0;
      p := L.List.First;
      while i < L.List.Num do
        begin
          if Compare_Key(@Key, @p^.Data.Primary) then
            begin
              R := p^.Data.Second;
              break;
            end;
          Inc(i);
          p := p^.Next;
        end;
    end;
  Result := R;
end;

procedure TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Set_Key_Value(Key: TKey_; Value: TValue_);
var
  L: TValue_Pair_Pool__;
  i: integer;
  p: PPair_Pool_Value__;
  Found: Boolean;
begin
  L := Get_Value_List(@Key);

  Found := False;
  if L.List.Num > 0 then
    begin
      i := 0;
      p := L.List.First;
      while i < L.List.Num do
        begin
          Found := Compare_Key(@Key, @p^.Data.Primary);
          if Found then
              break;
          Inc(i);
          p := p^.Next;
        end;
    end;
  if not Found then
      p := L.List.Add_Null();
  p^.Data.Primary := Key;
  p^.Data.Second := Value;
  L.List.MoveToFirst(p);
end;

procedure TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Delete(Key: TKey_);
var
  L: TValue_Pair_Pool__;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  L := Get_Value_List(@Key);
  if L.List.Num > 0 then
    begin
      i := 0;
      p := L.List.First;
      while i < L.List.Num do
        begin
          if Compare_Key(@Key, @p^.Data.Primary) then
              L.List.Push_To_Recycle_Pool(p);
          Inc(i);
          p := p^.Next;
        end;
    end;
  L.List.Free_Recycle_Pool;
  if L.List.Num <= 0 then
      Free_Value_List(@Key);
end;

function TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Num: NativeInt;
var
  i: integer;
begin
  Result := 0;
  for i := Low(HashBuffer) to high(HashBuffer) do
    if HashBuffer[i] <> nil then
        Inc(Result, HashBuffer[i].List.Num);
end;

function TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Default_Value(Key: TKey_; Default_: TValue_): TValue_;
var
  L: TValue_Pair_Pool__;
  R: TValue_;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  R := Default_;
  L := Get_Value_List(@Key);
  if L.List.Num > 0 then
    begin
      i := 0;
      p := L.List.First;
      while i < L.List.Num do
        begin
          if Compare_Key(@Key, @p^.Data.Primary) then
            begin
              R := p^.Data.Second;
              break;
            end;
          Inc(i);
          p := p^.Next;
        end;
    end;
  Result := R;
end;

procedure TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Set_Default_Value(Key: TKey_; Default_: TValue_);
begin
  Set_Key_Value(Key, Default_);
end;

procedure TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Progress_C(OnProgress: TBig_Hash_Pool_Progress_C);
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
  Aborted: Boolean;
begin
  Aborted := False;
  for i := Low(HashBuffer) to high(HashBuffer) do
    begin
      L := HashBuffer[i];
      if (L <> nil) and (L.List.Num > 0) then
        begin
          j := 0;
          p := L.List.First;
          while j < L.List.Num do
            begin
              OnProgress(p^.Data, Aborted);
              if Aborted then
                  Exit;
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

procedure TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Progress_M(OnProgress: TBig_Hash_Pool_Progress_M);
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
  Aborted: Boolean;
begin
  Aborted := False;
  for i := Low(HashBuffer) to high(HashBuffer) do
    begin
      L := HashBuffer[i];
      if (L <> nil) and (L.List.Num > 0) then
        begin
          j := 0;
          p := L.List.First;
          while j < L.List.Num do
            begin
              OnProgress(p^.Data, Aborted);
              if Aborted then
                  Exit;
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

procedure TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Progress_P(OnProgress: TBig_Hash_Pool_Progress_P);
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
  Aborted: Boolean;
begin
  Aborted := False;
  for i := Low(HashBuffer) to high(HashBuffer) do
    begin
      L := HashBuffer[i];
      if (L <> nil) and (L.List.Num > 0) then
        begin
          j := 0;
          p := L.List.First;
          while j < L.List.Num do
            begin
              OnProgress(p^.Data, Aborted);
              if Aborted then
                  Exit;
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

function TBig_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Pool(): TPool;
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := TPool.Create;
  for i := Low(HashBuffer) to high(HashBuffer) do
    begin
      L := HashBuffer[i];
      if (L <> nil) and (L.List.Num > 0) then
        begin
          j := 0;
          p := L.List.First;
          while j < L.List.Num do
            begin
              Result.Add(@p^.Data);
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

function TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Value_List(Key_P: PKey_): TValue_Pair_Pool__;
var
  i: integer;
begin
  i := Hash_Key_Mod(Get_Key_Hash(Key_P), Length(HashBuffer));
  if HashBuffer[i] = nil then
    begin
      HashBuffer[i] := TValue_Pair_Pool__.Create;
      HashBuffer[i].OnFree := {$IFDEF FPC}@{$ENDIF FPC}DoFree;
    end;
  Result := HashBuffer[i];
end;

procedure TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Free_Value_List(Key_P: PKey_);
begin
  DisposeObjectAndNil(HashBuffer[Hash_Key_Mod(Get_Key_Hash(Key_P), Length(HashBuffer))]);
end;

constructor TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Create(HashSize_: integer; Null_Value_: TValue_);
begin
  inherited Create;
  Null_Value := Null_Value_;
  SetLength(HashBuffer, HashSize_);
  OnFree := nil;
end;

destructor TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  SetLength(HashBuffer, 0);
  inherited Destroy;
end;

function TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Hash(Key_P: PKey_): THash;
var
  p: PByte;
  Size: NativeInt;
begin
  Get_Key_Data_Ptr(Key_P, p, Size);
  Result := Get_CRC32(p, Size);
end;

function TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Compare_Key(Key_P1, Key_P2: PKey_): Boolean;
var
  p1, p2: PByte;
  s1, s2: NativeInt;
begin
  Get_Key_Data_Ptr(Key_P1, p1, s1);
  Get_Key_Data_Ptr(Key_P2, p2, s2);
  Result := CompareMemory(p1, p2, Min(s1, s2));
end;

procedure TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.DoFree(var Data: TValue_Pair_Pool__.TPair);
begin
  if Assigned(OnFree) then
      OnFree(Data.Second);
end;

procedure TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Data_Ptr(Key_P: PKey_; var p: PByte; var Size: NativeInt);
begin
  p := PByte(Key_P);
  Size := SizeOf(TKey_);
end;

procedure TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.SwapInstance(source: T__);
var
  tmp_HashBuffer: TKey_Hash_Buffer;
begin
  tmp_HashBuffer := HashBuffer;
  HashBuffer := source.HashBuffer;
  source.HashBuffer := tmp_HashBuffer;
end;

procedure TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Clear;
var
  i: integer;
begin
  for i := Low(HashBuffer) to high(HashBuffer) do
      DisposeObjectAndNil(HashBuffer[i]);
end;

procedure TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Resize_Hash_Pool(HashSize_: integer);
var
  tmp: T__;
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  tmp := T__.Create(HashSize_, Null_Value);
  for i := Low(HashBuffer) to high(HashBuffer) do
    begin
      L := HashBuffer[i];
      if (L <> nil) and (L.List.Num > 0) then
        begin
          j := 0;
          p := L.List.First;
          while j < L.List.Num do
            begin
              tmp.Set_Key_Value(p^.Data.Primary, p^.Data.Second);
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
  SwapInstance(tmp);
  tmp.Free;
end;

function TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Exists(Key: TKey_): Boolean;
var
  L: TValue_Pair_Pool__;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := False;
  L := Get_Value_List(@Key);
  if L.List.Num > 0 then
    begin
      i := 0;
      p := L.List.First;
      while i < L.List.Num do
        begin
          if Compare_Key(@Key, @p^.Data.Primary) then
              Exit(True);
          Inc(i);
          p := p^.Next;
        end;
    end;
end;

function TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Value(Key: TKey_): TValue_;
var
  L: TValue_Pair_Pool__;
  R: TValue_;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  R := Null_Value;
  L := Get_Value_List(@Key);
  if L.List.Num > 0 then
    begin
      i := 0;
      p := L.List.First;
      while i < L.List.Num do
        begin
          if Compare_Key(@Key, @p^.Data.Primary) then
            begin
              R := p^.Data.Second;
              break;
            end;
          Inc(i);
          p := p^.Next;
        end;
    end;
  Result := R;
end;

procedure TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Set_Key_Value(Key: TKey_; Value: TValue_);
var
  L: TValue_Pair_Pool__;
  i: integer;
  p: PPair_Pool_Value__;
  Found: Boolean;
begin
  L := Get_Value_List(@Key);

  Found := False;
  if L.List.Num > 0 then
    begin
      i := 0;
      p := L.List.First;
      while i < L.List.Num do
        begin
          Found := Compare_Key(@Key, @p^.Data.Primary);
          if Found then
              break;
          Inc(i);
          p := p^.Next;
        end;
    end;
  if not Found then
      p := L.List.Add_Null();
  p^.Data.Primary := Key;
  p^.Data.Second := Value;
  L.List.MoveToFirst(p);
end;

procedure TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Delete(Key: TKey_);
var
  L: TValue_Pair_Pool__;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  L := Get_Value_List(@Key);
  if L.List.Num > 0 then
    begin
      i := 0;
      p := L.List.First;
      while i < L.List.Num do
        begin
          if Compare_Key(@Key, @p^.Data.Primary) then
              L.List.Push_To_Recycle_Pool(p);
          Inc(i);
          p := p^.Next;
        end;
    end;
  L.List.Free_Recycle_Pool;
  if L.List.Num <= 0 then
      Free_Value_List(@Key);
end;

function TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Num: NativeInt;
var
  i: integer;
begin
  Result := 0;
  for i := Low(HashBuffer) to high(HashBuffer) do
    if HashBuffer[i] <> nil then
        Inc(Result, HashBuffer[i].List.Num);
end;

function TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Default_Value(Key: TKey_; Default_: TValue_): TValue_;
var
  L: TValue_Pair_Pool__;
  R: TValue_;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  R := Default_;
  L := Get_Value_List(@Key);
  if L.List.Num > 0 then
    begin
      i := 0;
      p := L.List.First;
      while i < L.List.Num do
        begin
          if Compare_Key(@Key, @p^.Data.Primary) then
            begin
              R := p^.Data.Second;
              break;
            end;
          Inc(i);
          p := p^.Next;
        end;
    end;
  Result := R;
end;

procedure TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Set_Default_Value(Key: TKey_; Default_: TValue_);
begin
  Set_Key_Value(Key, Default_);
end;

procedure TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Progress_C(OnProgress: TBig_Hash_Pool_Progress_C);
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
  Aborted: Boolean;
begin
  Aborted := False;
  for i := Low(HashBuffer) to high(HashBuffer) do
    begin
      L := HashBuffer[i];
      if (L <> nil) and (L.List.Num > 0) then
        begin
          j := 0;
          p := L.List.First;
          while j < L.List.Num do
            begin
              OnProgress(p^.Data, Aborted);
              if Aborted then
                  Exit;
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

procedure TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Progress_M(OnProgress: TBig_Hash_Pool_Progress_M);
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
  Aborted: Boolean;
begin
  Aborted := False;
  for i := Low(HashBuffer) to high(HashBuffer) do
    begin
      L := HashBuffer[i];
      if (L <> nil) and (L.List.Num > 0) then
        begin
          j := 0;
          p := L.List.First;
          while j < L.List.Num do
            begin
              OnProgress(p^.Data, Aborted);
              if Aborted then
                  Exit;
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

procedure TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Progress_P(OnProgress: TBig_Hash_Pool_Progress_P);
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
  Aborted: Boolean;
begin
  Aborted := False;
  for i := Low(HashBuffer) to high(HashBuffer) do
    begin
      L := HashBuffer[i];
      if (L <> nil) and (L.List.Num > 0) then
        begin
          j := 0;
          p := L.List.First;
          while j < L.List.Num do
            begin
              OnProgress(p^.Data, Aborted);
              if Aborted then
                  Exit;
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;

function TCritical_Big_Hash_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Pool(): TPool;
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := TPool.Create;
  for i := Low(HashBuffer) to high(HashBuffer) do
    begin
      L := HashBuffer[i];
      if (L <> nil) and (L.List.Num > 0) then
        begin
          j := 0;
          p := L.List.First;
          while j < L.List.Num do
            begin
              Result.Add(@p^.Data);
              Inc(j);
              p := p^.Next;
            end;
        end;
    end;
end;
