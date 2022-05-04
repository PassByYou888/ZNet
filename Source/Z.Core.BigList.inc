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
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TRepeat___.Init_(Instance_: T___; BI_, EI_: NativeInt);
begin
  // Begin Index
  BI___ := BI_;
  // End Index
  EI___ := EI_;
  // Current index
  I___ := 0;
  Instance___ := Instance_;
  p___ := Instance___.First;
  while I___ < BI___ do
    begin
      p___ := p___^.Next;
      inc(I___);
    end;
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
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TInvert_Repeat___.Init_(Instance_: T___; BI_, EI_: NativeInt);
begin
  // Begin Index
  BI___ := BI_;
  // End Index
  EI___ := EI_;
  // Current index
  I___ := Instance_.Num - 1;
  Instance___ := Instance_;
  p___ := Instance___.Last;
  while I___ > BI___ do
    begin
      p___ := p___^.Prev;
      Dec(I___);
    end;
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
  Dec(I___);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoInternalFree(p: PQueueStruct);
begin
{$IFDEF DEBUG}
  if p^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  DoFree(p^.Data);
{$IFDEF DEBUG}
{$IFNDEF Fill_0_On_Generic}
  FillPtr(p, SizeOf(TQueueStruct), 0);
{$ENDIF Fill_0_On_Generic}
{$ENDIF DEBUG}
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

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.CompareData(Data_1, Data_2: T_): Boolean;
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
  FRecycle_Pool__.Push(p);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Free_Recycle_Pool;
begin
  if FRecycle_Pool__ <> nil then
    begin
      while FRecycle_Pool__.Num > 0 do
        begin
          Remove(FRecycle_Pool__.First^.Data);
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
  FChanged := False;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Next;
begin
  if FFirst <> nil then
      Remove(FFirst);
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Add(Data: T_): PQueueStruct;
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
  inc(FNum);
  FChanged := True;
  Result := p;
  DoAdd(p^.Data);
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Add_Null(): PQueueStruct;
var
  p: PQueueStruct;
begin
  new(p);

{$IFNDEF Fill_0_On_Generic}
  FillPtr(@p^.Data, SizeOf(T_), 0);
{$ENDIF Fill_0_On_Generic}
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
  inc(FNum);
  FChanged := True;
  Result := p;
  DoAdd(p^.Data);
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Insert(Data: T_; To_: PQueueStruct): PQueueStruct;
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
  inc(FNum);
  FChanged := True;
  Result := p;
  DoAdd(p^.Data);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Remove(p: PQueueStruct);
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
  DoInternalFree(p);
  FChanged := True;
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

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Found(p1: PQueueStruct): Boolean;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  Result := True;
  if FNum > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FNum do
        begin
          if p = p1 then
              exit;
          inc(i);
          p := p^.Next;
        end;
    end;
  Result := False;
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Find_Data(Data: T_): PQueueStruct;
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

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Remove_Data(Data: T_): Integer;
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
      Result := System.GetMemory(SizeOf(PQueueStruct) * FNum);
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
  L.Remove(L.List[0]);
  L.Test_Check__;
  L.Add(32321);
  L.Test_Check__;
  L.Insert(2323, L.List[0]);
  L.Test_Check__;
  L.Insert(2323, L.List[1]);
  L.Test_Check__;
  L.Remove(L.List[2]);
  L.Test_Check__;
  L.Remove(L.List[1]);
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

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TRepeat___.Init_(Instance_: T___);
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
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TRepeat___.Init_(Instance_: T___; BI_, EI_: NativeInt);
begin
  // Begin Index
  BI___ := BI_;
  // End Index
  EI___ := EI_;
  // Current index
  I___ := 0;
  Instance___ := Instance_;
  p___ := Instance___.First;
  while I___ < BI___ do
    begin
      p___ := p___^.Next;
      inc(I___);
    end;
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TRepeat___.Next: Boolean;
begin
  Result := False;
  if I___ > EI___ then
    begin
      p___ := nil;
      exit;
    end;
  p___ := p___^.Next;
  Result := I___ < EI___;
  inc(I___);
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TInvert_Repeat___.Init_(Instance_: T___);
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
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TInvert_Repeat___.Init_(Instance_: T___; BI_, EI_: NativeInt);
begin
  // Begin Index
  BI___ := BI_;
  // End Index
  EI___ := EI_;
  // Current index
  I___ := Instance_.Num - 1;
  Instance___ := Instance_;
  p___ := Instance___.Last;
  while I___ > BI___ do
    begin
      p___ := p___^.Prev;
      Dec(I___);
    end;
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.TInvert_Repeat___.Prev: Boolean;
begin
  Result := False;
  if I___ < EI___ then
    begin
      p___ := nil;
      exit;
    end;
  p___ := p___^.Prev;
  Result := I___ > EI___;
  Dec(I___);
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoInternalFree(p: PQueueStruct);
begin
{$IFDEF DEBUG}
  if p^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  DoFree(p^.Data);
{$IFDEF DEBUG}
{$IFNDEF Fill_0_On_Generic}
  FillPtr(p, SizeOf(TQueueStruct), 0);
{$ENDIF Fill_0_On_Generic}
{$ENDIF DEBUG}
  Dispose(p);
end;

constructor TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FRecycle_Pool__ := nil;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FOnAdd := nil;
  FOnFree := nil;
  FChanged := False;
  FList := nil;
end;

destructor TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  FCritical.Free;
  inherited Destroy;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(var Data: T_);
begin
  if Assigned(FOnFree) then
      FOnFree(Data);
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoAdd(var Data: T_);
begin
  if Assigned(FOnAdd) then
      FOnAdd(Data);
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.CompareData(Data_1, Data_2: T_): Boolean;
begin
  Result := CompareMemory(@Data_1, @Data_2, SizeOf(T_));
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Lock;
begin
  FCritical.Lock;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.UnLock;
begin
  FCritical.UnLock;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Push_To_Recycle_Pool(p: PQueueStruct);
begin
{$IFDEF DEBUG}
  if p^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  if FRecycle_Pool__ = nil then
      FRecycle_Pool__ := TRecycle_Pool__.Create;
  FRecycle_Pool__.Push(p);
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Free_Recycle_Pool;
begin
  if FRecycle_Pool__ <> nil then
    begin
      while FRecycle_Pool__.Num > 0 do
        begin
          Remove(FRecycle_Pool__.First^.Data);
          FRecycle_Pool__.Next;
        end;
      FRecycle_Pool__.Free;
      FRecycle_Pool__ := nil;
    end;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Clear;
var
  i: NativeInt;
  p, N_P: PQueueStruct;
begin
  if FRecycle_Pool__ <> nil then
    begin
      FRecycle_Pool__.Free;
      FRecycle_Pool__ := nil;
    end;
  FCritical.Lock;
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
  FChanged := False;
  FCritical.UnLock;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Next;
begin
  if FFirst <> nil then
      Remove(FFirst);
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Add(Data: T_): PQueueStruct;
var
  p: PQueueStruct;
begin
  FCritical.Lock;

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
  inc(FNum);
  FChanged := True;
  FCritical.UnLock;
  Result := p;
  DoAdd(p^.Data);
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Add_Null(): PQueueStruct;
var
  p: PQueueStruct;
begin
  FCritical.Lock;

  new(p);

{$IFNDEF Fill_0_On_Generic}
  FillPtr(@p^.Data, SizeOf(T_), 0);
{$ENDIF Fill_0_On_Generic}
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
  inc(FNum);
  FChanged := True;
  FCritical.UnLock;
  Result := p;
  DoAdd(p^.Data);
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Insert(Data: T_; To_: PQueueStruct): PQueueStruct;
var
  p: PQueueStruct;
  P_P: PQueueStruct;
begin
{$IFDEF DEBUG}
  if To_^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  FCritical.Lock;
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
  inc(FNum);
  FChanged := True;
  FCritical.UnLock;
  Result := p;
  DoAdd(p^.Data);
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Remove(p: PQueueStruct);
var
  P_P, N_P: PQueueStruct;
begin
{$IFDEF DEBUG}
  if p^.Instance___ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  FCritical.Lock;

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

  DoInternalFree(p);

  FChanged := True;
  FCritical.UnLock;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Move_Before(p, To_: PQueueStruct);
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

  FCritical.Lock;
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
  FCritical.UnLock;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.MoveToFirst(p: PQueueStruct);
begin
  Move_Before(p, First);
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.MoveToLast(p: PQueueStruct);
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

  FCritical.Lock;
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
  FCritical.UnLock;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Exchange(p1, p2: PQueueStruct);
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
  FCritical.Lock;
  tmp := p1^.Data;
  p1^.Data := p2^.Data;
  p2^.Data := tmp;
  FCritical.UnLock;
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Found(p1: PQueueStruct): Boolean;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  Result := False;
  FCritical.Lock;
  try
    if FNum > 0 then
      begin
        i := 0;
        p := FFirst;
        while i < FNum do
          begin
            if p = p1 then
                exit(True);
            inc(i);
            p := p^.Next;
          end;
      end;
  finally
      FCritical.UnLock;
  end;
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Find_Data(Data: T_): PQueueStruct;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  Result := nil;
  FCritical.Lock;
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
      FCritical.UnLock;
  end;
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Remove_Data(Data: T_): Integer;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  FCritical.Lock;
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
      FCritical.UnLock;
  end;
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Repeat_(): TRepeat___;
begin
  if Num <= 0 then
      raiseInfo('List is Null');
  Result.Init_(Self);
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Repeat_(BI_, EI_: NativeInt): TRepeat___;
begin
  if Num <= 0 then
      raiseInfo('List is Null');
  Result.Init_(Self, BI_, EI_);
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Invert_Repeat_(): TInvert_Repeat___;
begin
  if Num <= 0 then
      raiseInfo('List is Null');
  Result.Init_(Self);
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Invert_Repeat_(BI_, EI_: NativeInt): TInvert_Repeat___;
begin
  if Num <= 0 then
      raiseInfo('List is Null');
  Result.Init_(Self, BI_, EI_);
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_C(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_C);
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
  FCritical.Lock;
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
      FCritical.UnLock;
  end;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_M(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_M);
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
  FCritical.Lock;
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
      FCritical.UnLock;
  end;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_P(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_P);
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
  FCritical.Lock;
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
      FCritical.UnLock;
  end;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_C(OnFor: TQueneStructFor_C);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      FCritical.Lock;
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
          FCritical.UnLock;
      end;
    end;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_M(OnFor: TQueneStructFor_M);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      FCritical.Lock;
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
          FCritical.UnLock;
      end;
    end;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.For_P(OnFor: TQueneStructFor_P);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      FCritical.Lock;
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
          FCritical.UnLock;
      end;
    end;
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.ToArray(): TArray_T_;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  FCritical.Lock;
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
      FCritical.UnLock;
  end;
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.ToOrder(): TOrder_Data_Pool;
var
  i: NativeInt;
  p: PQueueStruct;
begin
  Result := TOrder_Data_Pool.Create;
  FCritical.Lock;
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
      FCritical.UnLock;
  end;
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.BuildArrayMemory: PQueueArrayStruct;
var
  i: NativeInt;
  p: PQueueStruct;
  tmp: PPQueueStruct;
begin
  Result := nil;
  FCritical.Lock;
  try
    // copy to order buffer
    if FNum > 0 then
      begin
        Result := System.GetMemory(SizeOf(PQueueStruct) * FNum);
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
  finally
      FCritical.UnLock;
  end;
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.CheckList: PQueueArrayStruct;
begin
  // check change
  FCritical.Lock;
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
      FCritical.UnLock;
  end;
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetList(const Index: NativeInt): PQueueStruct;
begin
  if (index >= 0) and (index < FNum) then
    begin
      FCritical.Lock;
      try
          Result := PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * index))^;
      finally
          FCritical.UnLock;
      end;
    end
  else
      raiseInfo('error');
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.SetList(const Index: NativeInt; const Value: PQueueStruct);
begin
  if (index >= 0) and (index < FNum) then
    begin
      FCritical.Lock;
      try
          PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * index))^ := Value;
      finally
          FCritical.UnLock;
      end;
    end
  else
      raiseInfo('error');
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetItems(const Index: NativeInt): T_;
begin
  if (index >= 0) and (index < FNum) then
    begin
      Result := PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * index))^^.Data;
    end
  else
      raiseInfo('error');
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.SetItems(const Index: NativeInt; const Value: T_);
begin
  if (index >= 0) and (index < FNum) then
    begin
      PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * index))^^.Data := Value;
    end
  else
      raiseInfo('error');
end;

{$IFDEF DEBUG}


function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Test_Check__: Boolean;
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
  FCritical.Lock;
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
      FCritical.UnLock;
  end;
  System.FreeMemory(arry);
end;

class procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Test;
type
  TCritical_Test_BigList___ = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalBigList<Integer>;
var
  L: TCritical_Test_BigList___;
  i, j: Integer;
begin
  L := TCritical_Test_BigList___.Create;

  L.Add(3232);
  L.Test_Check__;
  L.Remove(L.List[0]);
  L.Test_Check__;
  L.Add(32321);
  L.Test_Check__;
  L.Insert(2323, L.List[0]);
  L.Test_Check__;
  L.Insert(2323, L.List[1]);
  L.Test_Check__;
  L.Remove(L.List[2]);
  L.Test_Check__;
  L.Remove(L.List[1]);
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
