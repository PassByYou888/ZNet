procedure TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoInternalFree(p: POrderStruct);
begin
  try
    DoFree(p^.Data);
    Dispose(p);
  except
  end;
end;

constructor TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FOnFreeOrderStruct := nil;
end;

destructor TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(var Data: T_);
begin
  if Assigned(FOnFreeOrderStruct) then
      FOnFreeOrderStruct(Data);
end;

procedure TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Clear;
var
  p, N_P: POrderStruct;
begin
  p := FFirst;
  while p <> nil do
    begin
      N_P := p^.Next;
      DoInternalFree(p);
      p := N_P;
    end;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
end;

procedure TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Next;
var
  N_P: POrderStruct;
begin
  if FFirst <> nil then
    begin
      N_P := FFirst^.Next;
      DoInternalFree(FFirst);
      FFirst := N_P;
      if FFirst = nil then
          FLast := nil;
      Dec(FNum);
    end;
end;

function TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Push(Data: T_): POrderStruct;
var
  p: POrderStruct;
begin
  new(p);
  p^.Data := Data;
  p^.Next := nil;

  Inc(FNum);
  if (FFirst = nil) and (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
    end
  else if FLast <> nil then
    begin
      FLast^.Next := p;
      FLast := p;
    end;
  Result := p;
end;

procedure TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoInternalFree(p: POrderPtrStruct);
begin
  try
    DoFree(p^.Data);
    Dispose(p);
  except
  end;
end;

constructor TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FOnFreeOrderStruct := nil;
end;

destructor TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(Data: PT_);
begin
  if Assigned(FOnFreeOrderStruct) then
      FOnFreeOrderStruct(Data)
  else
      Dispose(Data);
end;

procedure TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Clear;
var
  p, N_P: POrderPtrStruct;
begin
  p := FFirst;
  while p <> nil do
    begin
      N_P := p^.Next;
      DoInternalFree(p);
      p := N_P;
    end;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
end;

procedure TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Next;
var
  N_P: POrderPtrStruct;
begin
  if FFirst <> nil then
    begin
      N_P := FFirst^.Next;
      DoInternalFree(FFirst);
      FFirst := N_P;
      if FFirst = nil then
          FLast := nil;
      Dec(FNum);
    end;
end;

function TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Push(Data: T_): POrderPtrStruct;
var
  p: POrderPtrStruct;
begin
  new(p);
  new(p^.Data);
  p^.Data^ := Data;
  p^.Next := nil;

  Inc(FNum);
  if (FFirst = nil) and (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
    end
  else if FLast <> nil then
    begin
      FLast^.Next := p;
      FLast := p;
    end;
  Result := p;
end;

function TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.PushPtr(Data: PT_): POrderPtrStruct;
var
  p: POrderPtrStruct;
begin
  new(p);
  p^.Data := Data;
  p^.Next := nil;

  Inc(FNum);
  if (FFirst = nil) and (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
    end
  else if FLast <> nil then
    begin
      FLast^.Next := p;
      FLast := p;
    end;
  Result := p;
end;

procedure TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoInternalFree(p: POrderStruct);
begin
  try
    DoFree(p^.Data);
    Dispose(p);
  except
  end;
end;

constructor TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FOnFreeCriticalOrderStruct := nil;
end;

destructor TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  FCritical.Free;
  inherited Destroy;
end;

procedure TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(var Data: T_);
begin
  if Assigned(FOnFreeCriticalOrderStruct) then
      FOnFreeCriticalOrderStruct(Data);
end;

procedure TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Clear;
var
  p, N_P: POrderStruct;
begin
  FCritical.Lock;
  p := FFirst;
  while p <> nil do
    begin
      N_P := p^.Next;
      DoInternalFree(p);
      p := N_P;
    end;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FCritical.UnLock;
end;

function TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetCurrent: POrderStruct;
begin
  FCritical.Lock;
  Result := FFirst;
  FCritical.UnLock;
end;

procedure TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Next;
var
  N_P: POrderStruct;
begin
  FCritical.Lock;
  if FFirst <> nil then
    begin
      N_P := FFirst^.Next;
      DoInternalFree(FFirst);
      FFirst := N_P;
      if FFirst = nil then
          FLast := nil;
      Dec(FNum);
    end;
  FCritical.UnLock;
end;

function TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Push(Data: T_): POrderStruct;
var
  p: POrderStruct;
begin
  new(p);
  p^.Data := Data;
  p^.Next := nil;

  FCritical.Lock;
  Inc(FNum);
  if (FFirst = nil) and (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
    end
  else if FLast <> nil then
    begin
      FLast^.Next := p;
      FLast := p;
    end;
  FCritical.UnLock;
  Result := p;
end;

function TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetNum: NativeInt;
begin
  FCritical.Lock;
  Result := FNum;
  FCritical.UnLock;
end;

procedure TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoInternalFree(p: POrderPtrStruct);
begin
  try
    DoFree(p^.Data);
    Dispose(p);
  except
  end;
end;

constructor TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FOnFreeCriticalOrderStruct := nil;
end;

destructor TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  FCritical.Free;
  inherited Destroy;
end;

procedure TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(Data: PT_);
begin
  if Assigned(FOnFreeCriticalOrderStruct) then
      FOnFreeCriticalOrderStruct(Data)
  else
      Dispose(Data);
end;

procedure TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Clear;
var
  p, N_P: POrderPtrStruct;
begin
  FCritical.Lock;
  p := FFirst;
  while p <> nil do
    begin
      N_P := p^.Next;
      DoInternalFree(p);
      p := N_P;
    end;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FCritical.UnLock;
end;

function TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetCurrent: POrderPtrStruct;
begin
  FCritical.Lock;
  Result := FFirst;
  FCritical.UnLock;
end;

procedure TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Next;
var
  N_P: POrderPtrStruct;
begin
  FCritical.Lock;
  if FFirst <> nil then
    begin
      N_P := FFirst^.Next;
      DoInternalFree(FFirst);
      FFirst := N_P;
      if FFirst = nil then
          FLast := nil;
      Dec(FNum);
    end;
  FCritical.UnLock;
end;

function TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Push(Data: T_): POrderPtrStruct;
var
  p: POrderPtrStruct;
begin
  new(p);
  new(p^.Data);
  p^.Data^ := Data;
  p^.Next := nil;

  FCritical.Lock;
  Inc(FNum);
  if (FFirst = nil) and (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
    end
  else if FLast <> nil then
    begin
      FLast^.Next := p;
      FLast := p;
    end;
  FCritical.UnLock;
  Result := p;
end;

function TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.PushPtr(Data: PT_): POrderPtrStruct;
var
  p: POrderPtrStruct;
begin
  new(p);
  p^.Data := Data;
  p^.Next := nil;

  FCritical.Lock;
  Inc(FNum);
  if (FFirst = nil) and (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
    end
  else if FLast <> nil then
    begin
      FLast^.Next := p;
      FLast := p;
    end;
  FCritical.UnLock;
  Result := p;
end;

function TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetNum: NativeInt;
begin
  FCritical.Lock;
  Result := FNum;
  FCritical.UnLock;
end;
