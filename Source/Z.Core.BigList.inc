procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TRepeat___.Init_(Instance_: T___);
begin
  // Begin Index
  BI___ := 0;
  // End Index
  EI___ := Instance_.Num - 1;
  // Current index
  I___ := 0;
  Instance___ := Instance_;
  p___ := Instance___.First;
  while I___ < BI___ do
    begin
      p___ := p___^.Next;
      inc(I___);
    end;
  Is_Discard___ := False;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TRepeat___.Init_(Instance_: T___; BI_, EI_: NativeInt);
begin
  // Begin Index
  BI___ := BI_;
  // End Index
  EI___ := EI_;
  // check
  if BI___ > EI___ then
      Swap(BI___, BI___);
  // Current index
  I___ := 0;
  Instance___ := Instance_;
  p___ := Instance___.First;
  while I___ < BI___ do
    begin
      p___ := p___^.Next;
      inc(I___);
    end;
  Is_Discard___ := False;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TRepeat___.Discard;
begin
  Is_Discard___ := True;
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TRepeat___.Next: Boolean;
begin
  Result := False;
  if I___ > EI___ then
    begin
      p___ := nil;
      exit;
    end;
  p___ := p___^.Next;
  Result := I___ < EI___;
  if Is_Discard___ then
    begin
      Is_Discard___ := False;
      Instance___.Remove_P(p___^.Prev);
      Dec(EI___);
    end
  else
      inc(I___);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TInvert_Repeat___.Init_(Instance_: T___);
begin
  // Begin Index
  BI___ := Instance_.Num - 1;
  // End Index
  EI___ := 0;
  // Current index
  I___ := Instance_.Num - 1;
  Instance___ := Instance_;
  p___ := Instance___.Last;
  while I___ > BI___ do
    begin
      p___ := p___^.Prev;
      Dec(I___);
    end;
  Is_Discard___ := False;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TInvert_Repeat___.Init_(Instance_: T___; BI_, EI_: NativeInt);
begin
  // Begin Index
  BI___ := BI_;
  // End Index
  EI___ := EI_;
  // check
  if BI___ < EI___ then
      Swap(BI___, EI___);
  // Current index
  I___ := Instance_.Num - 1;
  Instance___ := Instance_;
  p___ := Instance___.Last;
  while I___ > BI___ do
    begin
      p___ := p___^.Prev;
      Dec(I___);
    end;
  Is_Discard___ := False;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TInvert_Repeat___.Discard;
begin
  Is_Discard___ := True;
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TInvert_Repeat___.Prev: Boolean;
begin
  Result := False;
  if I___ < EI___ then
    begin
      p___ := nil;
      exit;
    end;
  p___ := p___^.Prev;
  Result := I___ > EI___;
  if Is_Discard___ then
    begin
      Is_Discard___ := False;
      Instance___.Remove_P(p___^.Next);
      Dec(BI___);
    end;
  Dec(I___);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoInternalFree(p: PQueueStruct);
begin
{$IFDEF DEBUG}
  if p^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  DoFree(p^.Data);
  Dispose(p);
end;

constructor TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  FRecycle_Pool__ := nil;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FOnAdd := nil;
  FOnFree := nil;
  FChanged := False;
  FList := nil;
end;

destructor TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(var Data: T_);
begin
  if Assigned(FOnFree) then
      FOnFree(Data);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoAdd(var Data: T_);
begin
  if Assigned(FOnAdd) then
      FOnAdd(Data);
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.CompareData(const Data_1, Data_2: T_): Boolean;
begin
  Result := CompareMemory(@Data_1, @Data_2, SizeOf(T_));
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Push_To_Recycle_Pool(p: PQueueStruct);
begin
{$IFDEF DEBUG}
  if p^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  if FRecycle_Pool__ = nil then
      FRecycle_Pool__ := TRecycle_Pool__.Create;

  if not p^.Recycle___ then
    begin
      p^.Recycle___ := True;
      FRecycle_Pool__.Push(p);
    end;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Free_Recycle_Pool;
begin
  if FRecycle_Pool__ <> nil then
    begin
      while FRecycle_Pool__.Num > 0 do
        begin
          Remove_P(FRecycle_Pool__.First^.Data);
          FRecycle_Pool__.Next;
        end;
      FRecycle_Pool__.Free;
      FRecycle_Pool__ := nil;
    end;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Clear;
var
  i: NativeInt;
  p, N_P: PQueueStruct;
begin
  if FRecycle_Pool__ <> nil then
    begin
      FRecycle_Pool__.Free;
      FRecycle_Pool__ := nil;
    end;
  if FNum > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FNum do
        begin
          N_P := p^.Next;
          DoInternalFree(p);
          inc(i);
          p := N_P;
        end;
    end;
  FNum := 0;
  if FList <> nil then
    begin
      System.FreeMemory(FList);
      FList := nil;
    end;
  FFirst := nil;
  FLast := nil;
  FChanged := True;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Next;
begin
  if FFirst <> nil then
      Remove_P(FFirst);
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Add(const Data: T_): PQueueStruct;
var
  p: PQueueStruct;
begin
  new(p);
  p^.Data := Data;
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
  p^.Instance___ := Self;
  p^.Recycle___ := False;
  inc(FNum);
  FChanged := True;
  Result := p;
  DoAdd(p^.Data);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.AddL(L_: T___);
begin
  if L_.Num > 0 then
    with L_.Repeat_ do
      repeat
          Add(Queue^.Data);
      until not Next;
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Add_Null(): PQueueStruct;
var
  p: PQueueStruct;
begin
  new(p);
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
  p^.Instance___ := Self;
  p^.Recycle___ := False;
  inc(FNum);
  FChanged := True;
  Result := p;
  DoAdd(p^.Data);
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Insert(const Data: T_; To_: PQueueStruct): PQueueStruct;
var
  p: PQueueStruct;
  P_P: PQueueStruct;
begin
{$IFDEF DEBUG}
  if To_^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  new(p);
  p^.Data := Data;

  if FFirst = To_ then
      FFirst := p;
  P_P := To_^.Prev;
  if P_P^.Next = To_ then
      P_P^.Next := p;
  if To_^.Next = To_ then
      To_^.Next := p;
  To_^.Prev := p;
  p^.Prev := P_P;
  p^.Next := To_;
  p^.Instance___ := Self;
  p^.Recycle___ := False;
  inc(FNum);
  FChanged := True;
  Result := p;
  DoAdd(p^.Data);
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.CopyFrom(Source_: T___): NativeInt;
begin
  Result := Source_.Num;
  if Result > 0 then
    with Source_.Repeat_ do
      repeat
          Add(Queue^.Data);
      until not Next;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Remove_P(p: PQueueStruct);
var
  P_P, N_P: PQueueStruct;
begin
{$IFDEF DEBUG}
  if p^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  P_P := p^.Prev;
  N_P := p^.Next;
  if p = FFirst then
      FFirst := N_P;
  if p = FLast then
      FLast := P_P;
  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
      FNum := 0;
    end
  else
    begin
      P_P^.Next := N_P;
      N_P^.Prev := P_P;
      Dec(FNum);
    end;
  FChanged := True;
  DoInternalFree(p);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Remove_T(const Data: T_);
begin
  Remove_Data(Data);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Move_Before(p, To_: PQueueStruct);
var
  P_P, N_P: PQueueStruct;
  tmp: T_;
begin
{$IFDEF DEBUG}
  if p^.Instance___ <> Self then
      raiseInfo('error');
  if To_^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  if p = To_ then
      exit;
  // step 1: remove
  P_P := p^.Prev;
  N_P := p^.Next;
  if p = FFirst then
      FFirst := N_P;
  if p = FLast then
      FLast := P_P;
  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
    end
  else
    begin
      P_P^.Next := N_P;
      N_P^.Prev := P_P;
    end;

  // step 2: insert
  if FFirst = To_ then
      FFirst := p;
  P_P := To_^.Prev;
  if P_P^.Next = To_ then
      P_P^.Next := p;
  if To_^.Next = To_ then
      To_^.Next := p;
  To_^.Prev := p;
  p^.Prev := P_P;
  p^.Next := To_;

  FChanged := True;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.MoveToFirst(p: PQueueStruct);
begin
  Move_Before(p, First);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.MoveToLast(p: PQueueStruct);
var
  P_P, N_P: PQueueStruct;
  tmp: T_;
begin
{$IFDEF DEBUG}
  if p^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  if p = Last then
      exit;
  // step 1: remove
  P_P := p^.Prev;
  N_P := p^.Next;
  if p = FFirst then
      FFirst := N_P;
  if p = FLast then
      FLast := P_P;
  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
    end
  else
    begin
      P_P^.Next := N_P;
      N_P^.Prev := P_P;
    end;

  // step 2: append
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;

  FChanged := True;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Exchange(p1, p2: PQueueStruct);
var
  tmp: T_;
begin
{$IFDEF DEBUG}
  if p1^.Instance___ <> Self then
      raiseInfo('error');
  if p2^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  if p1 = p2 then
      exit;
  tmp := p1^.Data;
  p1^.Data := p2^.Data;
  p2^.Data := tmp;
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Found(p1: PQueueStruct): NativeInt;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  if FNum > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FNum do
        begin
          if p = p1 then
              exit(i);
          inc(i);
          p := p^.Next;
        end;
    end;
  Result := -1;
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Find_Data(const Data: T_): PQueueStruct;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  Result := nil;
  if FNum > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FNum do
        begin
          if CompareData(Data, p^.Data) then
              exit(p);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Search_Data_As_Array(const Data: T_): TArray_T_;
var
  L: TOrder_Data_Pool;
  i: Integer;
begin
  L := Search_Data_As_Order(Data);
  SetLength(Result, L.Num);
  i := 0;
  while L.Num > 0 do
    begin
      Result[i] := L.First^.Data;
      L.Next;
    end;
  DisposeObject(L);
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Search_Data_As_Order(const Data: T_): TOrder_Data_Pool;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  Result := TOrder_Data_Pool.Create;
  if FNum > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FNum do
        begin
          if CompareData(Data, p^.Data) then
              Result.Push(p^.Data);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Remove_Data(const Data: T_): Integer;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  if FNum > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FNum do
        begin
          if CompareData(Data, p^.Data) then
              Push_To_Recycle_Pool(p);
          inc(i);
          p := p^.Next;
        end;
    end;
  Result := FRecycle_Pool__.Num;
  Free_Recycle_Pool();
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Repeat_(): TRepeat___;
begin
  if Num <= 0 then
      raiseInfo('List is Null');
  Result.Init_(Self);
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Repeat_(BI_, EI_: NativeInt): TRepeat___;
begin
  if Num <= 0 then
      raiseInfo('List is Null');
  Result.Init_(Self, BI_, EI_);
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Invert_Repeat_(): TInvert_Repeat___;
begin
  if Num <= 0 then
      raiseInfo('List is Null');
  Result.Init_(Self);
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Invert_Repeat_(BI_, EI_: NativeInt): TInvert_Repeat___;
begin
  if Num <= 0 then
      raiseInfo('List is Null');
  Result.Init_(Self, BI_, EI_);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_C(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_C);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
{$IFDEF DEBUG}
  if BP_^.Instance___ <> Self then
      raiseInfo('error');
  if EP_^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  i := 0;
  p := BP_;
  Aborted := False;
  repeat
    N_P := p^.Next;
    OnFor(i, p, Aborted);
    p := N_P;
    inc(i);
  until Aborted or (p = EP_) or (BP_ = EP_) or (i >= FNum);
  if (p = EP_) and (p <> BP_) then
      OnFor(i, p, Aborted);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_M(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_M);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
{$IFDEF DEBUG}
  if BP_^.Instance___ <> Self then
      raiseInfo('error');
  if EP_^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  i := 0;
  p := BP_;
  Aborted := False;
  repeat
    N_P := p^.Next;
    OnFor(i, p, Aborted);
    p := N_P;
    inc(i);
  until Aborted or (p = EP_) or (BP_ = EP_) or (i >= FNum);
  if (p = EP_) and (p <> BP_) then
      OnFor(i, p, Aborted);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_P(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_P);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
{$IFDEF DEBUG}
  if BP_^.Instance___ <> Self then
      raiseInfo('error');
  if EP_^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  i := 0;
  p := BP_;
  Aborted := False;
  repeat
    N_P := p^.Next;
    OnFor(i, p, Aborted);
    p := N_P;
    inc(i);
  until Aborted or (p = EP_) or (BP_ = EP_) or (i >= FNum);
  if (p = EP_) and (p <> BP_) then
      OnFor(i, p, Aborted);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_C(OnFor: TQueneStructFor_C);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      i := 0;
      p := FFirst;
      Aborted := False;
      while (i < FNum) and (not Aborted) do
        begin
          N_P := p^.Next;
          OnFor(i, p, Aborted);
          p := N_P;
          inc(i);
        end;
    end;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_M(OnFor: TQueneStructFor_M);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      i := 0;
      p := FFirst;
      Aborted := False;
      while (i < FNum) and (not Aborted) do
        begin
          N_P := p^.Next;
          OnFor(i, p, Aborted);
          p := N_P;
          inc(i);
        end;
    end;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_P(OnFor: TQueneStructFor_P);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      i := 0;
      p := FFirst;
      Aborted := False;
      while (i < FNum) and (not Aborted) do
        begin
          N_P := p^.Next;
          OnFor(i, p, Aborted);
          p := N_P;
          inc(i);
        end;
    end;
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.ToArray(): TArray_T_;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  SetLength(Result, FNum);
  // copy to order buffer
  if FNum > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FNum do
        begin
          Result[i] := p^.Data;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.ToOrder(): TOrder_Data_Pool;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  Result := TOrder_Data_Pool.Create;
  // copy to order buffer
  if FNum > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FNum do
        begin
          Result.Push(p^.Data);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Sort_C(Arry_: PQueueArrayStruct; L, R: NativeInt; OnSort: TSort_C);
var
  i, j: NativeInt;
  p, tmp: T_;
begin
  if L < R then
    begin
      repeat
        if (R - L) = 1 then
          begin
            if OnSort(Arry_^[L]^.Data, Arry_^[R]^.Data) > 0 then
              begin
                tmp := Arry_^[L]^.Data;
                Arry_^[L]^.Data := Arry_^[R]^.Data;
                Arry_^[R]^.Data := tmp;
              end;
            break;
          end;
        i := L;
        j := R;
        p := Arry_^[(L + R) shr 1]^.Data;
        repeat
          while OnSort(Arry_^[i]^.Data, p) < 0 do
              inc(i);
          while OnSort(Arry_^[j]^.Data, p) > 0 do
              Dec(j);
          if i <= j then
            begin
              if i <> j then
                begin
                  tmp := Arry_^[i]^.Data;
                  Arry_^[i]^.Data := Arry_^[j]^.Data;
                  Arry_^[j]^.Data := tmp;
                end;
              inc(i);
              Dec(j);
            end;
        until i > j;
        if (j - L) > (R - i) then
          begin
            if i < R then
                Sort_C(Arry_, i, R, OnSort);
            R := j;
          end
        else
          begin
            if L < j then
                Sort_C(Arry_, L, j, OnSort);
            L := i;
          end;
      until L >= R;
    end;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Sort_C(OnSort: TSort_C);
var
  tmp: PQueueArrayStruct;
begin
  if (Num > 1) and (Assigned(OnSort)) then
    begin
      tmp := BuildArrayMemory;
      Sort_C(tmp, 0, Num - 1, OnSort);
      System.FreeMemory(tmp);
    end;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Sort_M(Arry_: PQueueArrayStruct; L, R: NativeInt; OnSort: TSort_M);
var
  i, j: NativeInt;
  p, tmp: T_;
begin
  if L < R then
    begin
      repeat
        if (R - L) = 1 then
          begin
            if OnSort(Arry_^[L]^.Data, Arry_^[R]^.Data) > 0 then
              begin
                tmp := Arry_^[L]^.Data;
                Arry_^[L]^.Data := Arry_^[R]^.Data;
                Arry_^[R]^.Data := tmp;
              end;
            break;
          end;
        i := L;
        j := R;
        p := Arry_^[(L + R) shr 1]^.Data;
        repeat
          while OnSort(Arry_^[i]^.Data, p) < 0 do
              inc(i);
          while OnSort(Arry_^[j]^.Data, p) > 0 do
              Dec(j);
          if i <= j then
            begin
              if i <> j then
                begin
                  tmp := Arry_^[i]^.Data;
                  Arry_^[i]^.Data := Arry_^[j]^.Data;
                  Arry_^[j]^.Data := tmp;
                end;
              inc(i);
              Dec(j);
            end;
        until i > j;
        if (j - L) > (R - i) then
          begin
            if i < R then
                Sort_M(Arry_, i, R, OnSort);
            R := j;
          end
        else
          begin
            if L < j then
                Sort_M(Arry_, L, j, OnSort);
            L := i;
          end;
      until L >= R;
    end;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Sort_M(OnSort: TSort_M);
var
  tmp: PQueueArrayStruct;
begin
  if (Num > 1) and (Assigned(OnSort)) then
    begin
      tmp := BuildArrayMemory;
      Sort_M(tmp, 0, Num - 1, OnSort);
      System.FreeMemory(tmp);
    end;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Sort_P(Arry_: PQueueArrayStruct; L, R: NativeInt; OnSort: TSort_P);
var
  i, j: NativeInt;
  p, tmp: T_;
begin
  if L < R then
    begin
      repeat
        if (R - L) = 1 then
          begin
            if OnSort(Arry_^[L]^.Data, Arry_^[R]^.Data) > 0 then
              begin
                tmp := Arry_^[L]^.Data;
                Arry_^[L]^.Data := Arry_^[R]^.Data;
                Arry_^[R]^.Data := tmp;
              end;
            break;
          end;
        i := L;
        j := R;
        p := Arry_^[(L + R) shr 1]^.Data;
        repeat
          while OnSort(Arry_^[i]^.Data, p) < 0 do
              inc(i);
          while OnSort(Arry_^[j]^.Data, p) > 0 do
              Dec(j);
          if i <= j then
            begin
              if i <> j then
                begin
                  tmp := Arry_^[i]^.Data;
                  Arry_^[i]^.Data := Arry_^[j]^.Data;
                  Arry_^[j]^.Data := tmp;
                end;
              inc(i);
              Dec(j);
            end;
        until i > j;
        if (j - L) > (R - i) then
          begin
            if i < R then
                Sort_P(Arry_, i, R, OnSort);
            R := j;
          end
        else
          begin
            if L < j then
                Sort_P(Arry_, L, j, OnSort);
            L := i;
          end;
      until L >= R;
    end;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Sort_P(OnSort: TSort_P);
var
  tmp: PQueueArrayStruct;
begin
  if (Num > 1) and (Assigned(OnSort)) then
    begin
      tmp := BuildArrayMemory;
      Sort_P(tmp, 0, Num - 1, OnSort);
      System.FreeMemory(tmp);
    end;
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.BuildArrayMemory: PQueueArrayStruct;
var
  i: NativeInt;
  p: PQueueStruct;
  tmp: PPQueueStruct;
begin
  Result := nil;
  // copy to order buffer
  if FNum > 0 then
    begin
      Result := System.GetMemory(DeltaStep(SizeOf(PQueueStruct) * FNum, 8));
      i := 0;
      p := FFirst;
      tmp := PPQueueStruct(Result);
      while i < FNum do
        begin
          tmp^ := p;
          inc(tmp);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.CheckList: PQueueArrayStruct;
begin
  // check change
  if FChanged then
    begin
      if FList <> nil then
        begin
          System.FreeMemory(FList);
          FList := nil;
        end;
      FChanged := False;
    end;

  if FList = nil then
      FList := BuildArrayMemory();

  Result := PQueueArrayStruct(FList);
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetList(const Index: NativeInt): PQueueStruct;
begin
  if (index >= 0) and (index < FNum) then
    begin
      Result := PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * index))^;
    end
  else
      raiseInfo('error');
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.SetList(const Index: NativeInt; const Value: PQueueStruct);
begin
  if (index >= 0) and (index < FNum) then
    begin
      PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * index))^ := Value;
    end
  else
      raiseInfo('error');
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetItems(const Index: NativeInt): T_;
begin
  if (index >= 0) and (index < FNum) then
    begin
      Result := PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * index))^^.Data;
    end
  else
      raiseInfo('error');
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.SetItems(const Index: NativeInt; const Value: T_);
begin
  if (index >= 0) and (index < FNum) then
    begin
      PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * index))^^.Data := Value;
    end
  else
      raiseInfo('error');
end;

class function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Null_Data: T_;
begin
  FillPtr(@Result, SizeOf(T_), 0);
end;

{$IFDEF DEBUG}


function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Test_Check__: Boolean;
var
  arry: PQueueArrayStruct;
  i: Integer;
  p, F_P, L_P, P_P, N_P: PQueueStruct;
  P_P_i, N_P_i: Integer;
begin
  Result := True;
  if Num = 0 then
      exit;
  arry := BuildArrayMemory();
  F_P := arry^[0];
  L_P := arry^[Num - 1];

  if F_P <> First then
      Result := False;
  if L_P <> Last then
      Result := False;

  for i := 0 to Num - 1 do
    begin
      if i <= 0 then
          P_P_i := Num - 1
      else
          P_P_i := i - 1;

      if i >= Num - 1 then
          N_P_i := 0
      else
          N_P_i := i + 1;

      P_P := arry^[P_P_i];
      N_P := arry^[N_P_i];

      p := arry^[i];

      if p^.Prev <> P_P then
          Result := False;
      if p^.Next <> N_P then
          Result := False;

      if not Result then
          raiseInfo('error.');
    end;
  System.FreeMemory(arry);
end;

class procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Test;
type
  TTest_BigList = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<Integer>;
var
  L: TTest_BigList;
  i, j: Integer;
begin
  L := TTest_BigList.Create;

  L.Add(3232);
  L.Test_Check__;
  L.Remove_P(L.List[0]);
  L.Test_Check__;
  L.Add(32321);
  L.Test_Check__;
  L.Insert(2323, L.List[0]);
  L.Test_Check__;
  L.Insert(2323, L.List[1]);
  L.Test_Check__;
  L.Remove_P(L.List[2]);
  L.Test_Check__;
  L.Remove_P(L.List[1]);
  L.Test_Check__;

  for i := 0 to 10 do
    begin
      L.Add(i);
      L.Test_Check__;
    end;
  L.Insert(9999, L.List[5]);
  L.Test_Check__;
  L.Insert(9919, L.First);
  L.Test_Check__;
  L.Insert(9929, L.Last);
  L.Test_Check__;

  L.Move_Before(L.List[0], L.List[5]);
  L.Test_Check__;
  L.Move_Before(L.List[0], L.List[L.Num - 2]);
  L.Test_Check__;
  L.Move_Before(L.List[L.Num - 2], L.List[1]);
  L.Test_Check__;
  L.Move_Before(L.List[L.Num - 2], L.List[0]);
  L.Test_Check__;
  L.Move_Before(L.List[0], L.Last);
  L.Test_Check__;
  L.Move_Before(L.Last, L.First);
  L.Test_Check__;

  for i := 0 to L.Num - 1 do
    begin
      L.Next;
      L.Test_Check__;
    end;

  L.Free;
end;
{$ENDIF DEBUG}

// *****************************************************************************************************************

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TRepeat___.Init_(Instance_: T___);
begin
  // Begin Index
  BI___ := 0;
  // End Index
  EI___ := Instance_.Num - 1;
  // Current index
  I___ := 0;
  Instance___ := Instance_;
  p___ := Instance___.First;
  while I___ < BI___ do
    begin
      p___ := p___^.Next;
      inc(I___);
    end;
  Is_Discard___ := False;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TRepeat___.Init_(Instance_: T___; BI_, EI_: NativeInt);
begin
  // Begin Index
  BI___ := BI_;
  // End Index
  EI___ := EI_;
  // check
  if BI___ > EI___ then
      Swap(BI___, BI___);
  // Current index
  I___ := 0;
  Instance___ := Instance_;
  p___ := Instance___.First;
  while I___ < BI___ do
    begin
      p___ := p___^.Next;
      inc(I___);
    end;
  Is_Discard___ := False;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TRepeat___.Discard;
begin
  Is_Discard___ := True;
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TRepeat___.Next: Boolean;
begin
  Result := False;
  if I___ > EI___ then
    begin
      p___ := nil;
      exit;
    end;
  p___ := p___^.Next;
  Result := I___ < EI___;
  if Is_Discard___ then
    begin
      Is_Discard___ := False;
      Instance___.Remove_P(p___^.Prev);
      Dec(EI___);
    end
  else
      inc(I___);
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TInvert_Repeat___.Init_(Instance_: T___);
begin
  // Begin Index
  BI___ := Instance_.Num - 1;
  // End Index
  EI___ := 0;
  // Current index
  I___ := Instance_.Num - 1;
  Instance___ := Instance_;
  p___ := Instance___.Last;
  while I___ > BI___ do
    begin
      p___ := p___^.Prev;
      Dec(I___);
    end;
  Is_Discard___ := False;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TInvert_Repeat___.Init_(Instance_: T___; BI_, EI_: NativeInt);
begin
  // Begin Index
  BI___ := BI_;
  // End Index
  EI___ := EI_;
  // check
  if BI___ < EI___ then
      Swap(BI___, EI___);
  // Current index
  I___ := Instance_.Num - 1;
  Instance___ := Instance_;
  p___ := Instance___.Last;
  while I___ > BI___ do
    begin
      p___ := p___^.Prev;
      Dec(I___);
    end;
  Is_Discard___ := False;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TInvert_Repeat___.Discard;
begin
  Is_Discard___ := True;
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TInvert_Repeat___.Prev: Boolean;
begin
  Result := False;
  if I___ < EI___ then
    begin
      p___ := nil;
      exit;
    end;
  p___ := p___^.Prev;
  Result := I___ > EI___;
  if Is_Discard___ then
    begin
      Is_Discard___ := False;
      Instance___.Remove_P(p___^.Next);
      Dec(BI___);
    end;
  Dec(I___);
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoInternalFree(p: PQueueStruct);
begin
{$IFDEF DEBUG}
  if p^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  DoFree(p^.Data);
  Dispose(p);
end;

constructor TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  FCritical__ := TCritical.Create;
  FRecycle_Pool__ := nil;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FOnAdd := nil;
  FOnFree := nil;
  FChanged := False;
  FList := nil;
end;

destructor TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  FCritical__.Free;
  inherited Destroy;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(var Data: T_);
begin
  if Assigned(FOnFree) then
      FOnFree(Data);
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoAdd(var Data: T_);
begin
  if Assigned(FOnAdd) then
      FOnAdd(Data);
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.CompareData(const Data_1, Data_2: T_): Boolean;
begin
  Result := CompareMemory(@Data_1, @Data_2, SizeOf(T_));
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Lock;
begin
  FCritical__.Lock;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.UnLock;
begin
  FCritical__.UnLock;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Push_To_Recycle_Pool(p: PQueueStruct);
begin
{$IFDEF DEBUG}
  if p^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  if FRecycle_Pool__ = nil then
      FRecycle_Pool__ := TRecycle_Pool__.Create;

  if not p^.Recycle___ then
    begin
      p^.Recycle___ := True;
      FRecycle_Pool__.Push(p);
    end;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Free_Recycle_Pool;
begin
  if FRecycle_Pool__ <> nil then
    begin
      while FRecycle_Pool__.Num > 0 do
        begin
          Remove_P(FRecycle_Pool__.First^.Data);
          FRecycle_Pool__.Next;
        end;
      FRecycle_Pool__.Free;
      FRecycle_Pool__ := nil;
    end;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Clear;
var
  i: NativeInt;
  p, N_P: PQueueStruct;
begin
  if FRecycle_Pool__ <> nil then
    begin
      FRecycle_Pool__.Free;
      FRecycle_Pool__ := nil;
    end;
  FCritical__.Lock;
  if FNum > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FNum do
        begin
          N_P := p^.Next;
          DoInternalFree(p);
          inc(i);
          p := N_P;
        end;
    end;
  FNum := 0;
  if FList <> nil then
    begin
      System.FreeMemory(FList);
      FList := nil;
    end;
  FFirst := nil;
  FLast := nil;
  FChanged := True;
  FCritical__.UnLock;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Next;
begin
  if FFirst <> nil then
      Remove_P(FFirst);
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Add(const Data: T_): PQueueStruct;
var
  p: PQueueStruct;
begin
  FCritical__.Lock;

  new(p);
  p^.Data := Data;
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
  p^.Instance___ := Self;
  p^.Recycle___ := False;
  inc(FNum);
  FChanged := True;
  FCritical__.UnLock;
  Result := p;
  DoAdd(p^.Data);
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.AddL(L_: T___);
begin
  L_.Lock;
  if L_.Num > 0 then
    with L_.Repeat_ do
      repeat
          Add(Queue^.Data);
      until not Next;
  L_.UnLock;
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Add_Null(): PQueueStruct;
var
  p: PQueueStruct;
begin
  FCritical__.Lock;
  new(p);
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
  p^.Instance___ := Self;
  p^.Recycle___ := False;
  inc(FNum);
  FChanged := True;
  FCritical__.UnLock;
  Result := p;
  DoAdd(p^.Data);
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Insert(const Data: T_; To_: PQueueStruct): PQueueStruct;
var
  p: PQueueStruct;
  P_P: PQueueStruct;
begin
{$IFDEF DEBUG}
  if To_^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  FCritical__.Lock;
  new(p);
  p^.Data := Data;

  if FFirst = To_ then
      FFirst := p;
  P_P := To_^.Prev;
  if P_P^.Next = To_ then
      P_P^.Next := p;
  if To_^.Next = To_ then
      To_^.Next := p;
  To_^.Prev := p;
  p^.Prev := P_P;
  p^.Next := To_;
  p^.Instance___ := Self;
  p^.Recycle___ := False;
  inc(FNum);
  FChanged := True;
  FCritical__.UnLock;
  Result := p;
  DoAdd(p^.Data);
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.CopyFrom(Source_: T___): NativeInt;
begin
  Result := Source_.Num;
  if Result > 0 then
    with Source_.Repeat_ do
      repeat
          Add(Queue^.Data);
      until not Next;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Remove_P(p: PQueueStruct);
var
  P_P, N_P: PQueueStruct;
begin
{$IFDEF DEBUG}
  if p^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  FCritical__.Lock;

  P_P := p^.Prev;
  N_P := p^.Next;
  if p = FFirst then
      FFirst := N_P;
  if p = FLast then
      FLast := P_P;
  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
      FNum := 0;
    end
  else
    begin
      P_P^.Next := N_P;
      N_P^.Prev := P_P;
      Dec(FNum);
    end;
  FChanged := True;
  FCritical__.UnLock;

  DoInternalFree(p);
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Remove_T(const Data: T_);
begin
  Remove_Data(Data);
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Move_Before(p, To_: PQueueStruct);
var
  P_P, N_P: PQueueStruct;
  tmp: T_;
begin
{$IFDEF DEBUG}
  if p^.Instance___ <> Self then
      raiseInfo('error');
  if To_^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  if p = To_ then
      exit;

  FCritical__.Lock;
  // step 1: remove
  P_P := p^.Prev;
  N_P := p^.Next;
  if p = FFirst then
      FFirst := N_P;
  if p = FLast then
      FLast := P_P;
  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
    end
  else
    begin
      P_P^.Next := N_P;
      N_P^.Prev := P_P;
    end;

  // step 2: insert
  if FFirst = To_ then
      FFirst := p;
  P_P := To_^.Prev;
  if P_P^.Next = To_ then
      P_P^.Next := p;
  if To_^.Next = To_ then
      To_^.Next := p;
  To_^.Prev := p;
  p^.Prev := P_P;
  p^.Next := To_;

  FChanged := True;
  FCritical__.UnLock;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.MoveToFirst(p: PQueueStruct);
begin
  Move_Before(p, First);
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.MoveToLast(p: PQueueStruct);
var
  P_P, N_P: PQueueStruct;
  tmp: T_;
begin
{$IFDEF DEBUG}
  if p^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  if p = Last then
      exit;

  FCritical__.Lock;
  // step 1: remove
  P_P := p^.Prev;
  N_P := p^.Next;
  if p = FFirst then
      FFirst := N_P;
  if p = FLast then
      FLast := P_P;
  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
    end
  else
    begin
      P_P^.Next := N_P;
      N_P^.Prev := P_P;
    end;

  // step 2: append
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;

  FChanged := True;
  FCritical__.UnLock;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Exchange(p1, p2: PQueueStruct);
var
  tmp: T_;
begin
{$IFDEF DEBUG}
  if p1^.Instance___ <> Self then
      raiseInfo('error');
  if p2^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  if p1 = p2 then
      exit;
  FCritical__.Lock;
  tmp := p1^.Data;
  p1^.Data := p2^.Data;
  p2^.Data := tmp;
  FCritical__.UnLock;
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Found(p1: PQueueStruct): NativeInt;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  Result := -1;
  FCritical__.Lock;
  try
    if FNum > 0 then
      begin
        i := 0;
        p := FFirst;
        while i < FNum do
          begin
            if p = p1 then
                exit(i);
            inc(i);
            p := p^.Next;
          end;
      end;
  finally
      FCritical__.UnLock;
  end;
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Find_Data(const Data: T_): PQueueStruct;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  Result := nil;
  FCritical__.Lock;
  try
    if FNum > 0 then
      begin
        i := 0;
        p := FFirst;
        while i < FNum do
          begin
            if CompareData(Data, p^.Data) then
                exit(p);
            inc(i);
            p := p^.Next;
          end;
      end;
  finally
      FCritical__.UnLock;
  end;
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Search_Data_As_Array(const Data: T_): TArray_T_;
var
  L: TOrder_Data_Pool;
  i: Integer;
begin
  L := Search_Data_As_Order(Data);
  SetLength(Result, L.Num);
  i := 0;
  while L.Num > 0 do
    begin
      Result[i] := L.First^.Data;
      L.Next;
    end;
  DisposeObject(L);
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Search_Data_As_Order(const Data: T_): TOrder_Data_Pool;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  Result := TOrder_Data_Pool.Create;
  FCritical__.Lock;
  try
    if FNum > 0 then
      begin
        i := 0;
        p := FFirst;
        while i < FNum do
          begin
            if CompareData(Data, p^.Data) then
                Result.Push(p^.Data);
            inc(i);
            p := p^.Next;
          end;
      end;
  finally
      FCritical__.UnLock;
  end;
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Remove_Data(const Data: T_): Integer;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  FCritical__.Lock;
  try
    if FNum > 0 then
      begin
        i := 0;
        p := FFirst;
        while i < FNum do
          begin
            if CompareData(Data, p^.Data) then
                Push_To_Recycle_Pool(p);
            inc(i);
            p := p^.Next;
          end;
      end;
    Result := FRecycle_Pool__.Num;
    Free_Recycle_Pool();
  finally
      FCritical__.UnLock;
  end;
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Repeat_(): TRepeat___;
begin
  if Num <= 0 then
      raiseInfo('List is Null');
  Result.Init_(Self);
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Repeat_(BI_, EI_: NativeInt): TRepeat___;
begin
  if Num <= 0 then
      raiseInfo('List is Null');
  Result.Init_(Self, BI_, EI_);
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Invert_Repeat_(): TInvert_Repeat___;
begin
  if Num <= 0 then
      raiseInfo('List is Null');
  Result.Init_(Self);
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Invert_Repeat_(BI_, EI_: NativeInt): TInvert_Repeat___;
begin
  if Num <= 0 then
      raiseInfo('List is Null');
  Result.Init_(Self, BI_, EI_);
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_C(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_C);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
{$IFDEF DEBUG}
  if BP_^.Instance___ <> Self then
      raiseInfo('error');
  if EP_^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  FCritical__.Lock;
  try
    i := 0;
    p := BP_;
    Aborted := False;
    repeat
      N_P := p^.Next;
      OnFor(i, p, Aborted);
      p := N_P;
      inc(i);
    until Aborted or (p = EP_) or (BP_ = EP_) or (i >= FNum);
    if (p = EP_) and (p <> BP_) then
        OnFor(i, p, Aborted);
  finally
      FCritical__.UnLock;
  end;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_M(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_M);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
{$IFDEF DEBUG}
  if BP_^.Instance___ <> Self then
      raiseInfo('error');
  if EP_^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  FCritical__.Lock;
  try
    i := 0;
    p := BP_;
    Aborted := False;
    repeat
      N_P := p^.Next;
      OnFor(i, p, Aborted);
      p := N_P;
      inc(i);
    until Aborted or (p = EP_) or (BP_ = EP_) or (i >= FNum);
    if (p = EP_) and (p <> BP_) then
        OnFor(i, p, Aborted);
  finally
      FCritical__.UnLock;
  end;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_P(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_P);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
{$IFDEF DEBUG}
  if BP_^.Instance___ <> Self then
      raiseInfo('error');
  if EP_^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  FCritical__.Lock;
  try
    i := 0;
    p := BP_;
    Aborted := False;
    repeat
      N_P := p^.Next;
      OnFor(i, p, Aborted);
      p := N_P;
      inc(i);
    until Aborted or (p = EP_) or (BP_ = EP_) or (i >= FNum);
    if (p = EP_) and (p <> BP_) then
        OnFor(i, p, Aborted);
  finally
      FCritical__.UnLock;
  end;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_C(OnFor: TQueneStructFor_C);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      FCritical__.Lock;
      try
        i := 0;
        p := FFirst;
        Aborted := False;
        while (i < FNum) and (not Aborted) do
          begin
            N_P := p^.Next;
            OnFor(i, p, Aborted);
            p := N_P;
            inc(i);
          end;
      finally
          FCritical__.UnLock;
      end;
    end;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_M(OnFor: TQueneStructFor_M);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      FCritical__.Lock;
      try
        i := 0;
        p := FFirst;
        Aborted := False;
        while (i < FNum) and (not Aborted) do
          begin
            N_P := p^.Next;
            OnFor(i, p, Aborted);
            p := N_P;
            inc(i);
          end;
      finally
          FCritical__.UnLock;
      end;
    end;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_P(OnFor: TQueneStructFor_P);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      FCritical__.Lock;
      try
        i := 0;
        p := FFirst;
        Aborted := False;
        while (i < FNum) and (not Aborted) do
          begin
            N_P := p^.Next;
            OnFor(i, p, Aborted);
            p := N_P;
            inc(i);
          end;
      finally
          FCritical__.UnLock;
      end;
    end;
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.ToArray(): TArray_T_;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  FCritical__.Lock;
  try
    SetLength(Result, FNum);
    // copy to order buffer
    if FNum > 0 then
      begin
        i := 0;
        p := FFirst;
        while i < FNum do
          begin
            Result[i] := p^.Data;
            inc(i);
            p := p^.Next;
          end;
      end;
  finally
      FCritical__.UnLock;
  end;
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.ToOrder(): TOrder_Data_Pool;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  Result := TOrder_Data_Pool.Create;
  FCritical__.Lock;
  try
    // copy to order buffer
    if FNum > 0 then
      begin
        i := 0;
        p := FFirst;
        while i < FNum do
          begin
            Result.Push(p^.Data);
            inc(i);
            p := p^.Next;
          end;
      end;
  finally
      FCritical__.UnLock;
  end;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Sort_C(Arry_: PQueueArrayStruct; L, R: NativeInt; OnSort: TSort_C);
var
  i, j: NativeInt;
  p, tmp: T_;
begin
  if L < R then
    begin
      repeat
        if (R - L) = 1 then
          begin
            if OnSort(Arry_^[L]^.Data, Arry_^[R]^.Data) > 0 then
              begin
                tmp := Arry_^[L]^.Data;
                Arry_^[L]^.Data := Arry_^[R]^.Data;
                Arry_^[R]^.Data := tmp;
              end;
            break;
          end;
        i := L;
        j := R;
        p := Arry_^[(L + R) shr 1]^.Data;
        repeat
          while OnSort(Arry_^[i]^.Data, p) < 0 do
              inc(i);
          while OnSort(Arry_^[j]^.Data, p) > 0 do
              Dec(j);
          if i <= j then
            begin
              if i <> j then
                begin
                  tmp := Arry_^[i]^.Data;
                  Arry_^[i]^.Data := Arry_^[j]^.Data;
                  Arry_^[j]^.Data := tmp;
                end;
              inc(i);
              Dec(j);
            end;
        until i > j;
        if (j - L) > (R - i) then
          begin
            if i < R then
                Sort_C(Arry_, i, R, OnSort);
            R := j;
          end
        else
          begin
            if L < j then
                Sort_C(Arry_, L, j, OnSort);
            L := i;
          end;
      until L >= R;
    end;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Sort_C(OnSort: TSort_C);
var
  tmp: PQueueArrayStruct;
begin
  if (Num > 1) and (Assigned(OnSort)) then
    begin
      FCritical__.Lock;
      tmp := BuildArrayMemory;
      Sort_C(tmp, 0, Num - 1, OnSort);
      System.FreeMemory(tmp);
      FCritical__.UnLock;
    end;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Sort_M(Arry_: PQueueArrayStruct; L, R: NativeInt; OnSort: TSort_M);
var
  i, j: NativeInt;
  p, tmp: T_;
begin
  if L < R then
    begin
      repeat
        if (R - L) = 1 then
          begin
            if OnSort(Arry_^[L]^.Data, Arry_^[R]^.Data) > 0 then
              begin
                tmp := Arry_^[L]^.Data;
                Arry_^[L]^.Data := Arry_^[R]^.Data;
                Arry_^[R]^.Data := tmp;
              end;
            break;
          end;
        i := L;
        j := R;
        p := Arry_^[(L + R) shr 1]^.Data;
        repeat
          while OnSort(Arry_^[i]^.Data, p) < 0 do
              inc(i);
          while OnSort(Arry_^[j]^.Data, p) > 0 do
              Dec(j);
          if i <= j then
            begin
              if i <> j then
                begin
                  tmp := Arry_^[i]^.Data;
                  Arry_^[i]^.Data := Arry_^[j]^.Data;
                  Arry_^[j]^.Data := tmp;
                end;
              inc(i);
              Dec(j);
            end;
        until i > j;
        if (j - L) > (R - i) then
          begin
            if i < R then
                Sort_M(Arry_, i, R, OnSort);
            R := j;
          end
        else
          begin
            if L < j then
                Sort_M(Arry_, L, j, OnSort);
            L := i;
          end;
      until L >= R;
    end;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Sort_M(OnSort: TSort_M);
var
  tmp: PQueueArrayStruct;
begin
  if (Num > 1) and (Assigned(OnSort)) then
    begin
      FCritical__.Lock;
      tmp := BuildArrayMemory;
      Sort_M(tmp, 0, Num - 1, OnSort);
      System.FreeMemory(tmp);
      FCritical__.UnLock;
    end;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Sort_P(Arry_: PQueueArrayStruct; L, R: NativeInt; OnSort: TSort_P);
var
  i, j: NativeInt;
  p, tmp: T_;
begin
  if L < R then
    begin
      repeat
        if (R - L) = 1 then
          begin
            if OnSort(Arry_^[L]^.Data, Arry_^[R]^.Data) > 0 then
              begin
                tmp := Arry_^[L]^.Data;
                Arry_^[L]^.Data := Arry_^[R]^.Data;
                Arry_^[R]^.Data := tmp;
              end;
            break;
          end;
        i := L;
        j := R;
        p := Arry_^[(L + R) shr 1]^.Data;
        repeat
          while OnSort(Arry_^[i]^.Data, p) < 0 do
              inc(i);
          while OnSort(Arry_^[j]^.Data, p) > 0 do
              Dec(j);
          if i <= j then
            begin
              if i <> j then
                begin
                  tmp := Arry_^[i]^.Data;
                  Arry_^[i]^.Data := Arry_^[j]^.Data;
                  Arry_^[j]^.Data := tmp;
                end;
              inc(i);
              Dec(j);
            end;
        until i > j;
        if (j - L) > (R - i) then
          begin
            if i < R then
                Sort_P(Arry_, i, R, OnSort);
            R := j;
          end
        else
          begin
            if L < j then
                Sort_P(Arry_, L, j, OnSort);
            L := i;
          end;
      until L >= R;
    end;
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Sort_P(OnSort: TSort_P);
var
  tmp: PQueueArrayStruct;
begin
  if (Num > 1) and (Assigned(OnSort)) then
    begin
      FCritical__.Lock;
      tmp := BuildArrayMemory;
      Sort_P(tmp, 0, Num - 1, OnSort);
      System.FreeMemory(tmp);
      FCritical__.UnLock;
    end;
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.BuildArrayMemory: PQueueArrayStruct;
var
  i: NativeInt;
  p: PQueueStruct;
  tmp: PPQueueStruct;
begin
  Result := nil;
  // copy to order buffer
  if FNum > 0 then
    begin
      Result := System.GetMemory(DeltaStep(SizeOf(PQueueStruct) * FNum, 8));
      i := 0;
      p := FFirst;
      tmp := PPQueueStruct(Result);
      while i < FNum do
        begin
          tmp^ := p;
          inc(tmp);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.CheckList: PQueueArrayStruct;
begin
  // check change
  FCritical__.Lock;
  try
    if FChanged then
      begin
        if FList <> nil then
          begin
            System.FreeMemory(FList);
            FList := nil;
          end;
        FChanged := False;
      end;

    if FList = nil then
        FList := BuildArrayMemory();

    Result := PQueueArrayStruct(FList);
  finally
      FCritical__.UnLock;
  end;
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetList(const Index: NativeInt): PQueueStruct;
begin
  if (index >= 0) and (index < FNum) then
    begin
      FCritical__.Lock;
      try
          Result := PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * index))^;
      finally
          FCritical__.UnLock;
      end;
    end
  else
      raiseInfo('error');
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.SetList(const Index: NativeInt; const Value: PQueueStruct);
begin
  if (index >= 0) and (index < FNum) then
    begin
      FCritical__.Lock;
      try
          PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * index))^ := Value;
      finally
          FCritical__.UnLock;
      end;
    end
  else
      raiseInfo('error');
end;

function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetItems(const Index: NativeInt): T_;
begin
  if (index >= 0) and (index < FNum) then
    begin
      Result := PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * index))^^.Data;
    end
  else
      raiseInfo('error');
end;

procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.SetItems(const Index: NativeInt; const Value: T_);
begin
  if (index >= 0) and (index < FNum) then
    begin
      PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * index))^^.Data := Value;
    end
  else
      raiseInfo('error');
end;

class function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Null_Data: T_;
begin
  FillPtr(@Result, SizeOf(T_), 0);
end;

{$IFDEF DEBUG}


function TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Test_Check__: Boolean;
var
  arry: PQueueArrayStruct;
  i: Integer;
  p, F_P, L_P, P_P, N_P: PQueueStruct;
  P_P_i, N_P_i: Integer;
begin
  Result := True;
  if Num = 0 then
      exit;
  arry := BuildArrayMemory();
  FCritical__.Lock;
  try
    F_P := arry^[0];
    L_P := arry^[Num - 1];

    if F_P <> First then
        Result := False;
    if L_P <> Last then
        Result := False;

    for i := 0 to Num - 1 do
      begin
        if i <= 0 then
            P_P_i := Num - 1
        else
            P_P_i := i - 1;

        if i >= Num - 1 then
            N_P_i := 0
        else
            N_P_i := i + 1;

        P_P := arry^[P_P_i];
        N_P := arry^[N_P_i];

        p := arry^[i];

        if p^.Prev <> P_P then
            Result := False;
        if p^.Next <> N_P then
            Result := False;

        if not Result then
            raiseInfo('error.');
      end;
  finally
      FCritical__.UnLock;
  end;
  System.FreeMemory(arry);
end;

class procedure TCritical_BigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Test;
type
  TCritical_Test_BigList___ = {$IFDEF FPC}specialize {$ENDIF FPC} TCritical_BigList<Integer>;
var
  L: TCritical_Test_BigList___;
  i, j: Integer;
begin
  L := TCritical_Test_BigList___.Create;

  L.Add(3232);
  L.Test_Check__;
  L.Remove_P(L.List[0]);
  L.Test_Check__;
  L.Add(32321);
  L.Test_Check__;
  L.Insert(2323, L.List[0]);
  L.Test_Check__;
  L.Insert(2323, L.List[1]);
  L.Test_Check__;
  L.Remove_P(L.List[2]);
  L.Test_Check__;
  L.Remove_P(L.List[1]);
  L.Test_Check__;

  for i := 0 to 10 do
    begin
      L.Add(i);
      L.Test_Check__;
    end;
  L.Insert(9999, L.List[5]);
  L.Test_Check__;
  L.Insert(9919, L.First);
  L.Test_Check__;
  L.Insert(9929, L.Last);
  L.Test_Check__;

  L.Move_Before(L.List[0], L.List[5]);
  L.Test_Check__;
  L.Move_Before(L.List[0], L.List[L.Num - 2]);
  L.Test_Check__;
  L.Move_Before(L.List[L.Num - 2], L.List[1]);
  L.Test_Check__;
  L.Move_Before(L.List[L.Num - 2], L.List[0]);
  L.Test_Check__;
  L.Move_Before(L.List[0], L.Last);
  L.Test_Check__;
  L.Move_Before(L.Last, L.First);
  L.Test_Check__;

  for i := 0 to L.Num - 1 do
    begin
      L.Next;
      L.Test_Check__;
    end;

  L.Free;
end;
{$ENDIF DEBUG}


constructor TBig_Object_List{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create(AutoFreeObject_: Boolean);
begin
  inherited Create;
  AutoFreeObject := AutoFreeObject_;
end;

procedure TBig_Object_List{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(var Data: T_);
begin
  inherited DoFree(Data);
  if AutoFreeObject then
      DisposeObjectAndNil(Data);
end;

constructor TCritical_Big_Object_List{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create(AutoFreeObject_: Boolean);
begin
  inherited Create;
  AutoFreeObject := AutoFreeObject_;
end;

procedure TCritical_Big_Object_List{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(var Data: T_);
begin
  inherited DoFree(Data);
  if AutoFreeObject then
      DisposeObjectAndNil(Data)
end;
