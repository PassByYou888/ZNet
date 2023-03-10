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

function TPair_Pool{$IFNDEF FPC}<T1_, T2_>{$ENDIF FPC}.Add_Pair(Primary: T1_; Second: T2_): PPair__;
begin
  Result := List.Add_Null();
  Result^.Data.Primary := Primary;
  Result^.Data.Second := Second;
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

function TPair_Third_Pool{$IFNDEF FPC}<T1_, T2_, T3_>{$ENDIF FPC}.Add_Pair(Primary: T1_; Second: T2_; Third: T3_): PPair__;
begin
  Result := List.Add_Null();
  Result^.Data.Primary := Primary;
  Result^.Data.Second := Second;
  Result^.Data.Third := Third;
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

function TPair_Fourth_Pool{$IFNDEF FPC}<T1_, T2_, T3_, T4_>{$ENDIF FPC}.Add_Pair(Primary: T1_; Second: T2_; Third: T3_; Fourth: T4_): PPair__;
begin
  Result := List.Add_Null();
  Result^.Data.Primary := Primary;
  Result^.Data.Second := Second;
  Result^.Data.Third := Third;
  Result^.Data.Fourth := Fourth;
end;

// *****************************************************************************************************************

constructor TPair_Five_Pool{$IFNDEF FPC}<T1_, T2_, T3_, T4_, T5_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  List := TPair_BigList__.Create;
end;

destructor TPair_Five_Pool{$IFNDEF FPC}<T1_, T2_, T3_, T4_, T5_>{$ENDIF FPC}.Destroy;
begin
  List.Free;
  inherited Destroy;
end;

function TPair_Five_Pool{$IFNDEF FPC}<T1_, T2_, T3_, T4_, T5_>{$ENDIF FPC}.Add_Pair(Primary: T1_; Second: T2_; Third: T3_; Fourth: T4_; Five: T5_): PPair__;
begin
  Result := List.Add_Null();
  Result^.Data.Primary := Primary;
  Result^.Data.Second := Second;
  Result^.Data.Third := Third;
  Result^.Data.Fourth := Fourth;
  Result^.Data.Five := Five;
end;

// *****************************************************************************************************************

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Value_List(const Key_: TKey_; var Key_Hash_: THash): TValue_Pair_Pool__;
var
  i: integer;
begin
  Key_Hash_ := Get_Key_Hash(Key_);
  i := Hash_Key_Mod(Key_Hash_, FHash_Buffer.Count);
  if FHash_Buffer[i] = nil then
    begin
      FHash_Buffer[i] := TValue_Pair_Pool__.Create;
      FHash_Buffer[i].L.FOnFree := {$IFDEF FPC}@{$ENDIF FPC}Internal_Do_Free;
    end;
  Result := FHash_Buffer[i];
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Free_Value_List(Key_Hash_: THash);
begin
  DisposeObject(FHash_Buffer[Hash_Key_Mod(Key_Hash_, FHash_Buffer.Count)]);
  FHash_Buffer[Hash_Key_Mod(Key_Hash_, FHash_Buffer.Count)] := nil;
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
      Data^.Instance___.Remove_P(Data);
    end;
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Internal_Do_Free(var Data: TPair);
begin
  if Data.Third <> nil then
    begin
      TPool_Queue_Ptr___(Data.Third)^.Data := nil;
      FQueue_Pool.Remove_P(TPool_Queue_Ptr___(Data.Third));
    end;
  DoFree(Data.Primary, Data.Second);
end;

class function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Null_Key: TKey_;
begin
  FillPtr(@Result, SizeOf(TKey_), 0);
end;

class function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Null_Value: TValue_;
begin
  FillPtr(@Result, SizeOf(TValue_), 0);
end;

constructor TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Create(const HashSize_: integer; const Null_Value_: TValue_);
var
  i: integer;
begin
  inherited Create;
  FQueue_Pool := TPool___.Create;
  FQueue_Pool.FOnFree := {$IFDEF FPC}@{$ENDIF FPC}Internal_Do_Queue_Pool_Free;
  FHash_Buffer := TKey_Hash_Buffer.Create;
  FHash_Buffer.Count := HashSize_;
  for i := 0 to FHash_Buffer.Count - 1 do
      FHash_Buffer[i] := nil;
  FNull_Value := Null_Value_;
  FOnAdd := nil;
  FOnFree := nil;
end;

destructor TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  FQueue_Pool.Free;
  DisposeObjectAndNil(FHash_Buffer);
  inherited Destroy;
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.DoFree(var Key: TKey_; var Value: TValue_);
begin
  if Assigned(FOnFree) then
      FOnFree(Key, Value);
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.DoAdd(var Key: TKey_; var Value: TValue_);
begin
  if Assigned(FOnAdd) then
      FOnAdd(Key, Value);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Hash(const Key_: TKey_): THash;
var
  p: PByte;
  Size: NativeInt;
begin
  Get_Key_Data_Ptr(@Key_, p, Size);
  Result := Get_CRC32(p, Size);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Compare_Key(const Key_1, Key_2: TKey_): Boolean;
var
  p1, p2: PByte;
  s1, s2: NativeInt;
begin
  Get_Key_Data_Ptr(@Key_1, p1, s1);
  Get_Key_Data_Ptr(@Key_2, p2, s2);
  Result := CompareMemory(p1, p2, Min(s1, s2));
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Compare_Value(const Value_1, Value_2: TValue_): Boolean;
begin
  Result := CompareMemory(@Value_1, @Value_2, SizeOf(TValue_));
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Extract_Queue_Pool_Third;
begin
  if FQueue_Pool.Num > 0 then
    with FQueue_Pool.Repeat_ do
      repeat
          Queue^.Data^.Data.Third := Queue;
      until not Next;
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Clear;
var
  i: integer;
begin
  for i := 0 to FHash_Buffer.Count - 1 do
    begin
      DisposeObject(FHash_Buffer[i]);
      FHash_Buffer[i] := nil;
    end;
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Exists_Key(const Key: TKey_): Boolean;
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

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Exists_Value(const Data: TValue_): Boolean;
var
  L: TValue_Pair_Pool__;
  i, j: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := False;
  for i := 0 to FHash_Buffer.Count - 1 do
    begin
      L := FHash_Buffer[i];
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

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Exists(const Key: TKey_): Boolean;
begin
  Result := Exists_Key(Key);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Add(const Key: TKey_; const Value: TValue_; Overwrite_: Boolean): PPair_Pool_Value__;
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
  p^.Data.Third := FQueue_Pool.Add(p);
  p^.Data.Fourth := Key_Hash_;
  L.L.MoveToFirst(p);
  Result := p;
  DoAdd(p^.Data.Primary, p^.Data.Second);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Value(const Key: TKey_): TValue_;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  R: TValue_;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  R := FNull_Value;
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

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Set_Key_Value(const Key: TKey_; const Value: TValue_);
begin
  Add(Key, Value, True);
end;

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Delete(const Key: TKey_);
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

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Remove(p: PPair_Pool_Value__);
var
  inst_: TValue_Pair_Pool__.TPair_BigList__;
begin
  inst_ := p^.Instance___;
  inst_.Remove_P(p);
  if inst_.Num <= 0 then
      Free_Recycle_Pool();
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Num: NativeInt;
begin
  Result := FQueue_Pool.Num;
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.GetSum: NativeInt;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FHash_Buffer.Count - 1 do
    if FHash_Buffer[i] <> nil then
        Inc(Result, FHash_Buffer[i].L.Num);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Value_Ptr(const Key: TKey_): PValue;
begin
  Result := Get_Value_Ptr(Key, FNull_Value);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Value_Ptr(const Key: TKey_; const Default_: TValue_): PValue;
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

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Default_Value(const Key: TKey_; const Default_: TValue_): TValue_;
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

procedure TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Set_Default_Value(const Key: TKey_; const Default_: TValue_);
begin
  Add(Key, Default_, True);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Repeat_(): TRepeat___;
begin
  Result := FQueue_Pool.Repeat_();
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Repeat_(BI_, EI_: NativeInt): TRepeat___;
begin
  Result := FQueue_Pool.Repeat_(BI_, EI_);
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Invert_Repeat_(): TInvert_Repeat___;
begin
  Result := FQueue_Pool.Invert_Repeat_();
end;

function TBig_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Invert_Repeat_(BI_, EI_: NativeInt): TInvert_Repeat___;
begin
  Result := FQueue_Pool.Invert_Repeat_(BI_, EI_);
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
  for i := 0 to FHash_Buffer.Count - 1 do
    begin
      L := FHash_Buffer[i];
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
  for i := 0 to FHash_Buffer.Count - 1 do
    begin
      L := FHash_Buffer[i];
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
  for i := 0 to FHash_Buffer.Count - 1 do
    begin
      L := FHash_Buffer[i];
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
  for i := 0 to FHash_Buffer.Count - 1 do
    begin
      L := FHash_Buffer[i];
      if (L <> nil) then
        begin
          L.L.Free_Recycle_Pool();
          if L.L.Num <= 0 then
            begin
              DisposeObject(FHash_Buffer[i]);
              FHash_Buffer[i] := nil;
            end;
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
  for i := 0 to FHash_Buffer.Count - 1 do
    begin
      L := FHash_Buffer[i];
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
  for i := 0 to FHash_Buffer.Count - 1 do
    begin
      L := FHash_Buffer[i];
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
  for i := 0 to FHash_Buffer.Count - 1 do
    begin
      L := FHash_Buffer[i];
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
  for i := 0 to FHash_Buffer.Count - 1 do
    begin
      L := FHash_Buffer[i];
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
  for i := 0 to FHash_Buffer.Count - 1 do
    begin
      L := FHash_Buffer[i];
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

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Value_List(const Key_: TKey_; var Key_Hash_: THash): TValue_Pair_Pool__;
var
  i: integer;
begin
  Key_Hash_ := Get_Key_Hash(Key_);
  i := Hash_Key_Mod(Key_Hash_, FHash_Buffer.Count);
  if FHash_Buffer[i] = nil then
    begin
      FHash_Buffer[i] := TValue_Pair_Pool__.Create;
      FHash_Buffer[i].L.FOnFree := {$IFDEF FPC}@{$ENDIF FPC}Internal_Do_Free;
    end;
  Result := FHash_Buffer[i];
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Free_Value_List(Key_Hash_: THash);
begin
  DisposeObject(FHash_Buffer[Hash_Key_Mod(Key_Hash_, FHash_Buffer.Count)]);
  FHash_Buffer[Hash_Key_Mod(Key_Hash_, FHash_Buffer.Count)] := nil;
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
      Data^.Instance___.Remove_P(Data);
    end;
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Internal_Do_Free(var Data: TPair);
begin
  if Data.Third <> nil then
    begin
      TPool_Queue_Ptr___(Data.Third)^.Data := nil;
      FQueue_Pool.Remove_P(TPool_Queue_Ptr___(Data.Third));
    end;
  DoFree(Data.Primary, Data.Second);
end;

class function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Null_Key: TKey_;
begin
  FillPtr(@Result, SizeOf(TKey_), 0);
end;

class function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Null_Value: TValue_;
begin
  FillPtr(@Result, SizeOf(TValue_), 0);
end;

constructor TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Create(const HashSize_: integer; const Null_Value_: TValue_);
var
  i: integer;
begin
  inherited Create;
  FCritical__ := TCritical.Create;
  FQueue_Pool := TPool___.Create;
  FQueue_Pool.FOnFree := {$IFDEF FPC}@{$ENDIF FPC}Internal_Do_Queue_Pool_Free;
  FHash_Buffer := TKey_Hash_Buffer.Create;
  FHash_Buffer.Count := HashSize_;
  for i := 0 to FHash_Buffer.Count - 1 do
      FHash_Buffer[i] := nil;
  FNull_Value := Null_Value_;
  FOnAdd := nil;
  FOnFree := nil;
end;

destructor TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  FQueue_Pool.Free;
  DisposeObjectAndNil(FHash_Buffer);
  FCritical__.Free;
  inherited Destroy;
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.DoFree(var Key: TKey_; var Value: TValue_);
begin
  if Assigned(FOnFree) then
      FOnFree(Key, Value);
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.DoAdd(var Key: TKey_; var Value: TValue_);
begin
  if Assigned(FOnAdd) then
      FOnAdd(Key, Value);
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Hash(const Key_: TKey_): THash;
var
  p: PByte;
  Size: NativeInt;
begin
  Get_Key_Data_Ptr(@Key_, p, Size);
  Result := Get_CRC32(p, Size);
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Compare_Key(const Key_1, Key_2: TKey_): Boolean;
var
  p1, p2: PByte;
  s1, s2: NativeInt;
begin
  Get_Key_Data_Ptr(@Key_1, p1, s1);
  Get_Key_Data_Ptr(@Key_2, p2, s2);
  Result := CompareMemory(p1, p2, Min(s1, s2));
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Compare_Value(const Value_1, Value_2: TValue_): Boolean;
begin
  Result := CompareMemory(@Value_1, @Value_2, SizeOf(TValue_));
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Extract_Queue_Pool_Third;
begin
  if FQueue_Pool.Num > 0 then
    with FQueue_Pool.Repeat_ do
      repeat
          Queue^.Data^.Data.Third := Queue;
      until not Next;
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Clear;
var
  i: integer;
begin
  FCritical__.Lock;
  try
    for i := 0 to FHash_Buffer.Count - 1 do
      begin
        DisposeObject(FHash_Buffer[i]);
        FHash_Buffer[i] := nil;
      end;
  finally
      FCritical__.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Exists_Key(const Key: TKey_): Boolean;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := False;
  FCritical__.Lock;
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
      FCritical__.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Exists_Value(const Data: TValue_): Boolean;
var
  L: TValue_Pair_Pool__;
  i, j: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := False;
  FCritical__.Lock;
  try
    for i := 0 to FHash_Buffer.Count - 1 do
      begin
        L := FHash_Buffer[i];
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
      FCritical__.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Exists(const Key: TKey_): Boolean;
begin
  Result := Exists_Key(Key);
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Add(const Key: TKey_; const Value: TValue_; Overwrite_: Boolean): PPair_Pool_Value__;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  i: integer;
  p: PPair_Pool_Value__;
begin
  FCritical__.Lock;
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
    p^.Data.Third := FQueue_Pool.Add(p);
    p^.Data.Fourth := Key_Hash_;
    L.L.MoveToFirst(p);
    Result := p;
    DoAdd(p^.Data.Primary, p^.Data.Second);
  finally
      FCritical__.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Key_Value(const Key: TKey_): TValue_;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  R: TValue_;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  R := FNull_Value;
  FCritical__.Lock;
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
      FCritical__.UnLock;
  end;
  Result := R;
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Set_Key_Value(const Key: TKey_; const Value: TValue_);
begin
  Add(Key, Value, True);
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Delete(const Key: TKey_);
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  FCritical__.Lock;
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
      FCritical__.UnLock;
  end;
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Remove(p: PPair_Pool_Value__);
var
  inst_: TValue_Pair_Pool__.TPair_BigList__;
begin
  FCritical__.Lock;
  try
    inst_ := p^.Instance___;
    inst_.Remove_P(p);
  finally
      FCritical__.UnLock;
  end;
  if inst_.Num <= 0 then
      Free_Recycle_Pool();
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Num: NativeInt;
begin
  Result := FQueue_Pool.Num;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.GetSum: NativeInt;
var
  i: integer;
begin
  Result := 0;
  FCritical__.Lock;
  try
    for i := 0 to FHash_Buffer.Count - 1 do
      if FHash_Buffer[i] <> nil then
          Inc(Result, FHash_Buffer[i].L.Num);
  finally
      FCritical__.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Value_Ptr(const Key: TKey_): PValue;
begin
  Result := Get_Value_Ptr(Key, FNull_Value);
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Value_Ptr(const Key: TKey_; const Default_: TValue_): PValue;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  Result := nil;
  FCritical__.Lock;
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
      FCritical__.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Get_Default_Value(const Key: TKey_; const Default_: TValue_): TValue_;
var
  Key_Hash_: THash;
  L: TValue_Pair_Pool__;
  R: TValue_;
  i: NativeInt;
  p: PPair_Pool_Value__;
begin
  R := Default_;
  FCritical__.Lock;
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
      FCritical__.UnLock;
  end;
  Result := R;
end;

procedure TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Set_Default_Value(const Key: TKey_; const Default_: TValue_);
begin
  Add(Key, Default_, True);
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Repeat_(): TRepeat___;
begin
  Result := FQueue_Pool.Repeat_();
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Repeat_(BI_, EI_: NativeInt): TRepeat___;
begin
  Result := FQueue_Pool.Repeat_(BI_, EI_);
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Invert_Repeat_(): TInvert_Repeat___;
begin
  Result := FQueue_Pool.Invert_Repeat_();
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.Invert_Repeat_(BI_, EI_: NativeInt): TInvert_Repeat___;
begin
  Result := FQueue_Pool.Invert_Repeat_(BI_, EI_);
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
  FCritical__.Lock;
  try
    for i := 0 to FHash_Buffer.Count - 1 do
      begin
        L := FHash_Buffer[i];
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
      FCritical__.UnLock;
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
  FCritical__.Lock;
  try
    for i := 0 to FHash_Buffer.Count - 1 do
      begin
        L := FHash_Buffer[i];
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
      FCritical__.UnLock;
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
  FCritical__.Lock;
  try
    for i := 0 to FHash_Buffer.Count - 1 do
      begin
        L := FHash_Buffer[i];
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
      FCritical__.UnLock;
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
  FCritical__.Lock;
  try
    for i := 0 to FHash_Buffer.Count - 1 do
      begin
        L := FHash_Buffer[i];
        if (L <> nil) then
          begin
            L.L.Free_Recycle_Pool();
            if L.L.Num <= 0 then
              begin
                DisposeObject(FHash_Buffer[i]);
                FHash_Buffer[i] := nil;
              end;
          end;
      end;
  finally
      FCritical__.UnLock;
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
  FCritical__.Lock;
  try
    for i := 0 to FHash_Buffer.Count - 1 do
      begin
        L := FHash_Buffer[i];
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
      FCritical__.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.ToArray_Key(): TArray_Key;
var
  i, k: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  FCritical__.Lock;
  try
    k := 0;
    SetLength(Result, FQueue_Pool.Num);
    for i := 0 to FHash_Buffer.Count - 1 do
      begin
        L := FHash_Buffer[i];
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
      FCritical__.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.ToOrder_Key(): TOrder_Key;
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  FCritical__.Lock;
  try
    Result := TOrder_Key.Create;
    for i := 0 to FHash_Buffer.Count - 1 do
      begin
        L := FHash_Buffer[i];
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
      FCritical__.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.ToArray_Value(): TArray_Value;
var
  i, k: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  FCritical__.Lock;
  try
    k := 0;
    SetLength(Result, FQueue_Pool.Num);
    for i := 0 to FHash_Buffer.Count - 1 do
      begin
        L := FHash_Buffer[i];
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
      FCritical__.UnLock;
  end;
end;

function TCritical_Big_Hash_Pair_Pool{$IFNDEF FPC}<TKey_, TValue_>{$ENDIF FPC}.ToOrder_Value(): TOrder_Value;
var
  i: integer;
  L: TValue_Pair_Pool__;
  j: NativeInt;
  p: PPair_Pool_Value__;
begin
  FCritical__.Lock;
  try
    Result := TOrder_Value.Create;
    for i := 0 to FHash_Buffer.Count - 1 do
      begin
        L := FHash_Buffer[i];
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
      FCritical__.UnLock;
  end;
end;
