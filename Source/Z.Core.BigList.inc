procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoInternalFree(p: PQueueStruct);
begin
{$IFDEF DEBUG}
  if p^.Instance_ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  try
    DoFree(p^.Data);
    Dispose(p);
  except
  end;
end;

constructor TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  FRecycle_Pool__ := nil;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FOnFreeQueueStruct := nil;
  FChanged := False;
  FList := nil;
  FProgress_Busy := 0;
end;

destructor TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(var Data: T_);
begin
  if Assigned(FOnFreeQueueStruct) then
      FOnFreeQueueStruct(Data);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Push_To_Recycle_Pool(p: PQueueStruct);
begin
{$IFDEF DEBUG}
  if p^.Instance_ <> Self then
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
          Inc(i);
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
{$IFDEF DEBUG}
  p^.Instance_ := Self;
{$ENDIF DEBUG}
  Inc(FNum);
  FChanged := True;
  Result := p;
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Insert(Data: T_; To_: PQueueStruct): PQueueStruct;
var
  p: PQueueStruct;
  P_P: PQueueStruct;
begin
{$IFDEF DEBUG}
  if To_^.Instance_ <> Self then
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
{$IFDEF DEBUG}
  p^.Instance_ := Self;
{$ENDIF DEBUG}
  Inc(FNum);
  FChanged := True;
  Result := p;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Remove(p: PQueueStruct);
var
  P_P, N_P: PQueueStruct;
begin
{$IFDEF DEBUG}
  if p^.Instance_ <> Self then
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
  if p^.Instance_ <> Self then
      raiseInfo('error');
  if To_^.Instance_ <> Self then
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
  if p^.Instance_ <> Self then
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
  if p1^.Instance_ <> Self then
      raiseInfo('error');
  if p2^.Instance_ <> Self then
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
          Inc(i);
          p := p^.Next;
        end;
    end;
  Result := False;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Progress_C(BP_, EP_: PQueueStruct; OnProgress: TQueneStructProgress_C);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
{$IFDEF DEBUG}
  if BP_^.Instance_ <> Self then
      raiseInfo('error');
  if EP_^.Instance_ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  Inc(FProgress_Busy);
  i := 0;
  p := BP_;
  Aborted := False;
  repeat
    N_P := p^.Next;
    OnProgress(i, p, Aborted);
    p := N_P;
    Inc(i);
  until Aborted or (p = EP_) or (BP_ = EP_) or (i >= FNum);
  Dec(FProgress_Busy);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Progress_M(BP_, EP_: PQueueStruct; OnProgress: TQueneStructProgress_M);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
{$IFDEF DEBUG}
  if BP_^.Instance_ <> Self then
      raiseInfo('error');
  if EP_^.Instance_ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  Inc(FProgress_Busy);
  i := 0;
  p := BP_;
  Aborted := False;
  repeat
    N_P := p^.Next;
    OnProgress(i, p, Aborted);
    p := N_P;
    Inc(i);
  until Aborted or (p = EP_) or (BP_ = EP_) or (i >= FNum);
  Dec(FProgress_Busy);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Progress_P(BP_, EP_: PQueueStruct; OnProgress: TQueneStructProgress_P);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
{$IFDEF DEBUG}
  if BP_^.Instance_ <> Self then
      raiseInfo('error');
  if EP_^.Instance_ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  Inc(FProgress_Busy);
  i := 0;
  p := BP_;
  Aborted := False;
  repeat
    N_P := p^.Next;
    OnProgress(i, p, Aborted);
    p := N_P;
    Inc(i);
  until Aborted or (p = EP_) or (BP_ = EP_) or (i >= FNum);
  Dec(FProgress_Busy);
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Progress_C(OnProgress: TQueneStructProgress_C);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      Inc(FProgress_Busy);
      i := 0;
      p := FFirst;
      Aborted := False;
      while (i < FNum) and (not Aborted) do
        begin
          N_P := p^.Next;
          OnProgress(i, p, Aborted);
          p := N_P;
          Inc(i);
        end;
      Dec(FProgress_Busy);
    end;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Progress_M(OnProgress: TQueneStructProgress_M);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      Inc(FProgress_Busy);
      i := 0;
      p := FFirst;
      Aborted := False;
      while (i < FNum) and (not Aborted) do
        begin
          N_P := p^.Next;
          OnProgress(i, p, Aborted);
          p := N_P;
          Inc(i);
        end;
      Dec(FProgress_Busy);
    end;
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Progress_P(OnProgress: TQueneStructProgress_P);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      Inc(FProgress_Busy);
      i := 0;
      p := FFirst;
      Aborted := False;
      while (i < FNum) and (not Aborted) do
        begin
          N_P := p^.Next;
          OnProgress(i, p, Aborted);
          p := N_P;
          Inc(i);
        end;
      Dec(FProgress_Busy);
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
          Inc(i);
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
          Inc(tmp);
          Inc(i);
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
  if (Index >= 0) and (Index < FNum) then
    begin
      Result := PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * Index))^;
    end
  else
      raiseInfo('error');
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.SetList(const Index: NativeInt; const Value: PQueueStruct);
begin
  if (Index >= 0) and (Index < FNum) then
    begin
      PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * Index))^ := Value;
    end
  else
      raiseInfo('error');
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetItems(const Index: NativeInt): T_;
begin
  if (Index >= 0) and (Index < FNum) then
    begin
      Result := PQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * Index))^.Data;
    end
  else
      raiseInfo('error');
end;

procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.SetItems(const Index: NativeInt; const Value: T_);
begin
  if (Index >= 0) and (Index < FNum) then
    begin
      PQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * Index))^.Data := Value;
    end
  else
      raiseInfo('error');
end;

function TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Check: Boolean;
var
  arry: PQueueArrayStruct;
  i: integer;
  p, F_P, L_P, P_P, N_P: PQueueStruct;
  P_P_i, N_P_i: integer;
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
    end;
  System.FreeMemory(arry);
end;

class procedure TBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Test;
type
  TTest_BigList = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<integer>;
var
  L: TTest_BigList;
  i, j: integer;
begin
  L := TTest_BigList.Create;

  L.Add(3232);
  L.Check;
  L.Remove(L.List[0]);
  L.Check;
  L.Add(32321);
  L.Check;
  L.Insert(2323, L.List[0]);
  L.Check;
  L.Insert(2323, L.List[1]);
  L.Check;
  L.Remove(L.List[2]);
  L.Check;
  L.Remove(L.List[1]);
  L.Check;

  for i := 0 to 10 do
    begin
      L.Add(i);
      L.Check;
    end;
  L.Insert(9999, L.List[5]);
  L.Check;
  L.Insert(9919, L.First);
  L.Check;
  L.Insert(9929, L.Last);
  L.Check;

  L.Move_Before(L.List[0], L.List[5]);
  L.Check;
  L.Move_Before(L.List[0], L.List[L.Num - 2]);
  L.Check;
  L.Move_Before(L.List[L.Num - 2], L.List[1]);
  L.Check;
  L.Move_Before(L.List[L.Num - 2], L.List[0]);
  L.Check;
  L.Move_Before(L.List[0], L.Last);
  L.Check;
  L.Move_Before(L.Last, L.First);
  L.Check;

  for i := 0 to L.Num - 1 do
    begin
      L.Next;
      L.Check;
    end;

  L.Free;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoInternalFree(p: PQueueStruct);
begin
{$IFDEF DEBUG}
  if p^.Instance_ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  try
    DoFree(p^.Data);
    Dispose(p);
  except
  end;
end;

constructor TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FRecycle_Pool__ := nil;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FOnFreeQueueStruct := nil;
  FChanged := False;
  FList := nil;
  FProgress_Busy := 0;
end;

destructor TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  FCritical.Free;
  inherited Destroy;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(var Data: T_);
begin
  if Assigned(FOnFreeQueueStruct) then
      FOnFreeQueueStruct(Data);
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Push_To_Recycle_Pool(p: PQueueStruct);
begin
{$IFDEF DEBUG}
  if p^.Instance_ <> Self then
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
          Inc(i);
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
{$IFDEF DEBUG}
  p^.Instance_ := Self;
{$ENDIF DEBUG}
  Inc(FNum);
  FChanged := True;
  FCritical.UnLock;
  Result := p;
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Insert(Data: T_; To_: PQueueStruct): PQueueStruct;
var
  p: PQueueStruct;
  P_P: PQueueStruct;
begin
{$IFDEF DEBUG}
  if To_^.Instance_ <> Self then
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
{$IFDEF DEBUG}
  p^.Instance_ := Self;
{$ENDIF DEBUG}
  Inc(FNum);
  FChanged := True;
  FCritical.UnLock;
  Result := p;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Remove(p: PQueueStruct);
var
  P_P, N_P: PQueueStruct;
begin
{$IFDEF DEBUG}
  if p^.Instance_ <> Self then
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
  if p^.Instance_ <> Self then
      raiseInfo('error');
  if To_^.Instance_ <> Self then
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
  if p^.Instance_ <> Self then
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
  if p1^.Instance_ <> Self then
      raiseInfo('error');
  if p2^.Instance_ <> Self then
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
  Result := True;
  if FNum > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FNum do
        begin
          if p = p1 then
              exit;
          Inc(i);
          p := p^.Next;
        end;
    end;
  Result := False;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Progress_C(BP_, EP_: PQueueStruct; OnProgress: TQueneStructProgress_C);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
{$IFDEF DEBUG}
  if BP_^.Instance_ <> Self then
      raiseInfo('error');
  if EP_^.Instance_ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  FCritical.Lock;
  Inc(FProgress_Busy);
  try
    i := 0;
    p := BP_;
    Aborted := False;
    repeat
      N_P := p^.Next;
      OnProgress(i, p, Aborted);
      p := N_P;
      Inc(i);
    until Aborted or (p = EP_) or (BP_ = EP_) or (i >= FNum);
  finally
    Dec(FProgress_Busy);
    FCritical.UnLock;
  end;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Progress_M(BP_, EP_: PQueueStruct; OnProgress: TQueneStructProgress_M);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
{$IFDEF DEBUG}
  if BP_^.Instance_ <> Self then
      raiseInfo('error');
  if EP_^.Instance_ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  FCritical.Lock;
  Inc(FProgress_Busy);
  try
    i := 0;
    p := BP_;
    Aborted := False;
    repeat
      N_P := p^.Next;
      OnProgress(i, p, Aborted);
      p := N_P;
      Inc(i);
    until Aborted or (p = EP_) or (BP_ = EP_) or (i >= FNum);
  finally
    Dec(FProgress_Busy);
    FCritical.UnLock;
  end;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Progress_P(BP_, EP_: PQueueStruct; OnProgress: TQueneStructProgress_P);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
{$IFDEF DEBUG}
  if BP_^.Instance_ <> Self then
      raiseInfo('error');
  if EP_^.Instance_ <> Self then
      raiseInfo('error');
{$ENDIF DEBUG}
  FCritical.Lock;
  Inc(FProgress_Busy);
  try
    i := 0;
    p := BP_;
    Aborted := False;
    repeat
      N_P := p^.Next;
      OnProgress(i, p, Aborted);
      p := N_P;
      Inc(i);
    until Aborted or (p = EP_) or (BP_ = EP_) or (i >= FNum);
  finally
    Dec(FProgress_Busy);
    FCritical.UnLock;
  end;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Progress_C(OnProgress: TQueneStructProgress_C);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      FCritical.Lock;
      Inc(FProgress_Busy);
      try
        i := 0;
        p := FFirst;
        Aborted := False;
        while (i < FNum) and (not Aborted) do
          begin
            N_P := p^.Next;
            OnProgress(i, p, Aborted);
            p := N_P;
            Inc(i);
          end;
      finally
        Dec(FProgress_Busy);
        FCritical.UnLock;
      end;
    end;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Progress_M(OnProgress: TQueneStructProgress_M);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      FCritical.Lock;
      Inc(FProgress_Busy);
      try
        i := 0;
        p := FFirst;
        Aborted := False;
        while (i < FNum) and (not Aborted) do
          begin
            N_P := p^.Next;
            OnProgress(i, p, Aborted);
            p := N_P;
            Inc(i);
          end;
      finally
        Dec(FProgress_Busy);
        FCritical.UnLock;
      end;
    end;
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Progress_P(OnProgress: TQueneStructProgress_P);
var
  i: NativeInt;
  p, N_P: PQueueStruct;
  Aborted: Boolean;
begin
  if FNum > 0 then
    begin
      FCritical.Lock;
      Inc(FProgress_Busy);
      try
        i := 0;
        p := FFirst;
        Aborted := False;
        while (i < FNum) and (not Aborted) do
          begin
            N_P := p^.Next;
            OnProgress(i, p, Aborted);
            p := N_P;
            Inc(i);
          end;
      finally
        Dec(FProgress_Busy);
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
            Inc(i);
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
            Inc(tmp);
            Inc(i);
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
  if (Index >= 0) and (Index < FNum) then
    begin
      FCritical.Lock;
      try
          Result := PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * Index))^;
      finally
          FCritical.UnLock;
      end;
    end
  else
      raiseInfo('error');
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.SetList(const Index: NativeInt; const Value: PQueueStruct);
begin
  if (Index >= 0) and (Index < FNum) then
    begin
      FCritical.Lock;
      try
          PPQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * Index))^ := Value;
      finally
          FCritical.UnLock;
      end;
    end
  else
      raiseInfo('error');
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetItems(const Index: NativeInt): T_;
begin
  if (Index >= 0) and (Index < FNum) then
    begin
      Result := PQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * Index))^.Data;
    end
  else
      raiseInfo('error');
end;

procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.SetItems(const Index: NativeInt; const Value: T_);
begin
  if (Index >= 0) and (Index < FNum) then
    begin
      PQueueStruct(GetOffset(CheckList(), SizeOf(PQueueStruct) * Index))^.Data := Value;
    end
  else
      raiseInfo('error');
end;

function TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Check: Boolean;
var
  arry: PQueueArrayStruct;
  i: integer;
  p, F_P, L_P, P_P, N_P: PQueueStruct;
  P_P_i, N_P_i: integer;
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
      end;
  finally
      FCritical.UnLock;
  end;
  System.FreeMemory(arry);
end;

class procedure TCriticalBigList{$IFNDEF FPC}<T_>{$ENDIF FPC}.Test;
type
  TCritical_Test_BigList___ = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalBigList<integer>;
var
  L: TCritical_Test_BigList___;
  i, j: integer;
begin
  L := TCritical_Test_BigList___.Create;

  L.Add(3232);
  L.Check;
  L.Remove(L.List[0]);
  L.Check;
  L.Add(32321);
  L.Check;
  L.Insert(2323, L.List[0]);
  L.Check;
  L.Insert(2323, L.List[1]);
  L.Check;
  L.Remove(L.List[2]);
  L.Check;
  L.Remove(L.List[1]);
  L.Check;

  for i := 0 to 10 do
    begin
      L.Add(i);
      L.Check;
    end;
  L.Insert(9999, L.List[5]);
  L.Check;
  L.Insert(9919, L.First);
  L.Check;
  L.Insert(9929, L.Last);
  L.Check;

  L.Move_Before(L.List[0], L.List[5]);
  L.Check;
  L.Move_Before(L.List[0], L.List[L.Num - 2]);
  L.Check;
  L.Move_Before(L.List[L.Num - 2], L.List[1]);
  L.Check;
  L.Move_Before(L.List[L.Num - 2], L.List[0]);
  L.Check;
  L.Move_Before(L.List[0], L.Last);
  L.Check;
  L.Move_Before(L.Last, L.First);
  L.Check;

  for i := 0 to L.Num - 1 do
    begin
      L.Next;
      L.Check;
    end;

  L.Free;
end;
