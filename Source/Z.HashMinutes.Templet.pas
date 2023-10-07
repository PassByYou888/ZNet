{ ****************************************************************************** }
{ * Generic Minutes hash Library                                               * }
{ ****************************************************************************** }
unit Z.HashMinutes.Templet;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses SysUtils, DateUtils,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core,
  Z.Status, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.ListEngine, Z.Geometry2D;

type
  // T_: only be a unique value, such as pointer or object, if using constant or variable T_ not be able to match search successfully.
  // TMinutes_Buffer_Pool is Thread safe
  TMinutes_Buffer_Pool<T_> = class(TBig_Hash_Pair_Pool < TDateTime, TBig_Hash_Pair_Pool < T_, TObject >> )
  public type
    TMK = record // minute key tool
      data: array [0 .. 4] of Word;
      procedure Init(T_: TDateTime);
      function ToHash: THash;
      function Compare(var key: TMK): Boolean;
      function To_Span_Time: TDateTime;
    end;

    TTime_Hash_Pool = TBig_Hash_Pair_Pool<T_, TObject>;

    TTime_Hash_Pool__ = class(TTime_Hash_Pool)
    private
      Span_Time: TDateTime;
    end;

    TTime_List = TBigList<T_>;
    TTime_Data_L = TBigList<TTime_Hash_Pool>;
    TTime_Data_Pool = TBig_Hash_Pair_Pool<T_, TTime_Data_L>;
  private
    FCritical: TCritical;
    FTime_Data_Pool: TTime_Data_Pool;
    FLevel_2_Hash_Size: Integer;
    procedure Do_Time_Data_Pool_Free(var key: T_; var Value: TTime_Data_L);
    function Get_Or_Create_Pool(const Key_: TDateTime): TTime_Hash_Pool;
    function Get_Pool(const Key_: TDateTime): TTime_Hash_Pool;
  public
    property Critical: TCritical read FCritical;
    property Time_Data_Pool: TTime_Data_Pool read FTime_Data_Pool;
    property Level_2_Hash_Size: Integer read FLevel_2_Hash_Size write FLevel_2_Hash_Size;
    constructor Create(const HashSize_: Integer);
    destructor Destroy; override;
    procedure Clear;
    function Get_Key_Hash(const Key_: TDateTime): THash; override;
    function Compare_Key(const Key_1, Key_2: TDateTime): Boolean; override;
    procedure DoFree(var key: TDateTime; var Value: TTime_Hash_Pool); override;
    procedure Add_Span(bDT, eDT: TDateTime; Value: T_); overload;
    procedure Add_Span(L_: Boolean; DT: TDateTime; Value: T_); overload;
    procedure Add_Span(DT: TDateTime; Value: T_); overload;
    procedure Remove_Span(Value: T_);
    function Get_Span_Num(Value: T_): Int64;
    procedure Sort_By_Span;
    function Search_Span(bDT, eDT: TDateTime): TTime_List; overload;
    function Search_Span(DT: TDateTime): TTime_List; overload;
    function Total: NativeInt;
  end;

procedure Test_TMinutes_Buffer_Pool;

implementation

procedure TMinutes_Buffer_Pool<T_>.TMK.Init(T_: TDateTime);
var
  Sec, MSec: Word;
begin
  DecodeDate(T_, data[0], data[1], data[2]);
  DecodeTime(T_, data[3], data[4], Sec, MSec);
end;

function TMinutes_Buffer_Pool<T_>.TMK.ToHash: THash;
begin
  Result := Get_CRC32(@data[0], 10);
end;

function TMinutes_Buffer_Pool<T_>.TMK.Compare(var key: TMK): Boolean;
begin
  Result := CompareMemory(@data[0], @key.data[0], 10);
end;

function TMinutes_Buffer_Pool<T_>.TMK.To_Span_Time: TDateTime;
begin
  Result := EncodeDateTime(data[0], data[1], data[2], data[3], data[4], 0, 0);
end;

procedure TMinutes_Buffer_Pool<T_>.Do_Time_Data_Pool_Free(var key: T_; var Value: TTime_Data_L);
begin
  DisposeObjectAndNil(Value);
end;

function TMinutes_Buffer_Pool<T_>.Get_Or_Create_Pool(const Key_: TDateTime): TTime_Hash_Pool;
var
  k: TMK;
begin
  Result := Key_Value[Key_];
  if Result = nil then
    begin
      Result := TTime_Hash_Pool__.Create(FLevel_2_Hash_Size, Self);
      Add(Key_, Result, False);
      k.Init(Key_);
      TTime_Hash_Pool__(Result).Span_Time := k.To_Span_Time;
    end;
end;

function TMinutes_Buffer_Pool<T_>.Get_Pool(const Key_: TDateTime): TTime_Hash_Pool;
begin
  Result := Key_Value[Key_];
end;

constructor TMinutes_Buffer_Pool<T_>.Create(const HashSize_: Integer);
begin
  inherited Create(HashSize_, nil);
  FCritical := TCritical.Create;
  FTime_Data_Pool := TTime_Data_Pool.Create(HashSize_, nil);
  FTime_Data_Pool.OnFree := Do_Time_Data_Pool_Free;
  FLevel_2_Hash_Size := $FF;
end;

destructor TMinutes_Buffer_Pool<T_>.Destroy;
begin
  disposeObject(FTime_Data_Pool);
  DisposeObjectAndNil(FCritical);
  inherited Destroy;
end;

procedure TMinutes_Buffer_Pool<T_>.Clear;
begin
  FCritical.Lock;
  try
    FTime_Data_Pool.Clear;
    inherited Clear;
  finally
      FCritical.UnLock;
  end;
end;

function TMinutes_Buffer_Pool<T_>.Get_Key_Hash(const Key_: TDateTime): THash;
var
  k: TMK;
begin
  k.Init(Key_);
  Result := k.ToHash;
end;

function TMinutes_Buffer_Pool<T_>.Compare_Key(const Key_1, Key_2: TDateTime): Boolean;
var
  k1, k2: TMK;
begin
  k1.Init(Key_1);
  k2.Init(Key_2);
  Result := k1.Compare(k2);
end;

procedure TMinutes_Buffer_Pool<T_>.DoFree(var key: TDateTime; var Value: TTime_Hash_Pool);
begin
  DisposeObjectAndNil(Value);
  inherited DoFree(key, Value);
end;

procedure TMinutes_Buffer_Pool<T_>.Add_Span(bDT, eDT: TDateTime; Value: T_);
var
  tmp: TDateTime;
begin
  if CompareDateTime(bDT, eDT) > 0 then
      Swap(bDT, eDT);

  FCritical.Lock;
  try
    tmp := bDT;
    Add_Span(False, tmp, Value);
    while CompareDateTime(tmp, eDT) <= 0 do
      begin
        Add_Span(False, tmp, Value);
        tmp := IncMinute(tmp);
      end;
    tmp := eDT;
    Add_Span(False, tmp, Value);
  finally
      FCritical.UnLock;
  end;
end;

procedure TMinutes_Buffer_Pool<T_>.Add_Span(L_: Boolean; DT: TDateTime; Value: T_);
var
  obj: TTime_Hash_Pool;
  obj2: TTime_Data_L;
begin
  if L_ then
      FCritical.Lock;
  try
    obj := Get_Or_Create_Pool(DT);
    if obj.Exists_Key(Value) then
        exit;

    obj.Add(Value, Self, False);
    obj2 := FTime_Data_Pool.Key_Value[Value];
    if obj2 = nil then
      begin
        obj2 := TTime_Data_L.Create;
        FTime_Data_Pool.Add(Value, obj2, False);
      end;
    obj2.Add(obj);
  finally
    if L_ then
        FCritical.UnLock;
  end;
end;

procedure TMinutes_Buffer_Pool<T_>.Add_Span(DT: TDateTime; Value: T_);
begin
  Add_Span(True, DT, Value);
end;

procedure TMinutes_Buffer_Pool<T_>.Remove_Span(Value: T_);
var
  obj2: TTime_Data_L;
begin
  FCritical.Lock;
  try
    obj2 := FTime_Data_Pool.Key_Value[Value];
    if obj2 = nil then
      begin
        if Num > 0 then
          with Repeat_ do
            repeat
                queue^.data^.data.Second.Delete(Value);
            until not Next;
      end
    else
      begin
        if obj2.Num > 0 then
          with obj2.Repeat_ do
            repeat
              queue^.data.Delete(Value);
              if queue^.data.Num <= 0 then
                begin
                  queue^.data.Clear;
                  Discard;
                end;
            until not Next;
        FTime_Data_Pool.Delete(Value);
      end;
  finally
      FCritical.UnLock;
  end;
end;

function TMinutes_Buffer_Pool<T_>.Get_Span_Num(Value: T_): Int64;
var
  obj2: TTime_Data_L;
begin
  Result := 0;
  FCritical.Lock;
  try
    obj2 := FTime_Data_Pool.Key_Value[Value];
    if obj2 <> nil then
        Result := obj2.Num
    else
        Result := 0;
  finally
      FCritical.UnLock;
  end;
end;

procedure TMinutes_Buffer_Pool<T_>.Sort_By_Span;
{$IFDEF FPC}
  function Do_Run_FPC_Sort_(var L, R: TTime_Hash_Pool): Integer;
  begin
    Result := CompareDateTime(TTime_Hash_Pool__(L).Span_Time, TTime_Hash_Pool__(R).Span_Time);
  end;
{$ENDIF FPC}


begin
  FCritical.Lock;
  try
{$IFDEF FPC}
    Sort_Value_P(Do_Run_FPC_Sort_);
{$ELSE FPC}
    Sort_Value_P(function(var L, R: TTime_Hash_Pool): Integer
      begin
        Result := CompareDateTime(TTime_Hash_Pool__(L).Span_Time, TTime_Hash_Pool__(R).Span_Time);
      end);
{$ENDIF FPC}
  finally
      FCritical.UnLock;
  end;
end;

function TMinutes_Buffer_Pool<T_>.Search_Span(bDT, eDT: TDateTime): TTime_List;
var
  tmp: TDateTime;
  obj: TTime_Hash_Pool;
  swap_obj: TTime_Hash_Pool;
begin
  if CompareDateTime(bDT, eDT) > 0 then
      Swap(bDT, eDT);

  Result := TTime_List.Create;
  swap_obj := TTime_Hash_Pool.Create($FFFF, Self);

  FCritical.Lock;
  try
    tmp := bDT;
    obj := Get_Pool(tmp);
    if obj <> nil then
      begin
        if obj.Num > 0 then
          with obj.Repeat_ do
            repeat
              if not swap_obj.Exists_Key(queue^.data^.data.Primary) then
                  swap_obj.Add(queue^.data^.data.Primary, Self, False);
            until not Next;
      end;

    while CompareDateTime(tmp, eDT) <= 0 do
      begin
        obj := Get_Pool(tmp);
        if obj <> nil then
          begin
            if obj.Num > 0 then
              with obj.Repeat_ do
                repeat
                  if not swap_obj.Exists_Key(queue^.data^.data.Primary) then
                      swap_obj.Add(queue^.data^.data.Primary, Self, True);
                until not Next;
          end;
        tmp := IncMinute(tmp);
      end;

    tmp := eDT;
    obj := Get_Pool(tmp);
    if obj <> nil then
      begin
        if obj.Num > 0 then
          with obj.Repeat_ do
            repeat
              if not swap_obj.Exists_Key(queue^.data^.data.Primary) then
                  swap_obj.Add(queue^.data^.data.Primary, Self, True);
            until not Next;
      end;

    if swap_obj.Num > 0 then
      with swap_obj.Repeat_ do
        repeat
            Result.Add(queue^.data^.data.Primary);
        until not Next;
  except
  end;
  FCritical.UnLock;

  disposeObject(swap_obj);
end;

function TMinutes_Buffer_Pool<T_>.Search_Span(DT: TDateTime): TTime_List;
var
  tmp: TDateTime;
  obj: TTime_Hash_Pool;
begin
  Result := TTime_List.Create;
  FCritical.Lock;
  try
    obj := Get_Pool(DT);
    if obj <> nil then
      begin
        if obj.Num > 0 then
          with obj.Repeat_ do
            repeat
                Result.Add(queue^.data^.data.Primary);
            until not Next;
      end;
  finally
      FCritical.UnLock;
  end;
end;

function TMinutes_Buffer_Pool<T_>.Total: NativeInt;
begin
  Result := FTime_Data_Pool.Num;
end;

procedure Test_TMinutes_Buffer_Pool;
type
  T_ = TMinutes_Buffer_Pool<PDateTime>;
var
  m: T_;
  i: Integer;
  L, tmp: T_.TTime_List;
  p: PDateTime;
  bDT, eDT: TDateTime;
  tk: TTimeTick;
begin
  m := T_.Create($FFFF);
  L := T_.TTime_List.Create;

  tk := GetTimeTick;

  bDT := umlDT('2009-10-6 1:30:34.000');
  eDT := umlDT('2009-10-6 2:31:34.000');
  new(p);
  p^ := bDT;
  L.Add(p);
  m.Add_Span(bDT, eDT, p);

  for i := 0 to 10 * 10000 - 1 do // test 1000*10000=40 second.
    begin
      bDT := IncMinute(umlNow, umlRR(-100000000, 100000000));
      eDT := IncMinute(bDT, umlRR(-10, 10));
      new(p);
      p^ := bDT;
      L.Add(p);
      m.Add_Span(bDT, eDT, p);
    end;

  DoStatus('hash build time:%dms', [GetTimeTick - tk]);

  m.Sort_By_Span;

  bDT := umlDT('2009-10-6 1:30:34.000');
  eDT := umlDT('2009-10-6 2:31:34.000');
  tmp := m.Search_Span(bDT, eDT);
  disposeObject(tmp);

  m.Remove_Span(L.First^.data);

  bDT := umlDT('2009-10-6 1:30:34.000');
  eDT := umlDT('2009-10-6 2:31:34.000');
  tmp := m.Search_Span(bDT, eDT);
  disposeObject(tmp);

  with L.Repeat_ do
    repeat
        m.Get_Span_Num(queue^.data);
    until not Next;

  with L.Repeat_ do
    repeat
        m.Remove_Span(queue^.data);
    until not Next;

  disposeObject(m);

  with L.Repeat_ do
    repeat
        dispose(queue^.data);
    until not Next;
  L.Clear;
  disposeObject(L);
end;

end.
