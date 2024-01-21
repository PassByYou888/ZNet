{ ****************************************************************************** }
{ * instance state and analysis tool                                           * }
{ ****************************************************************************** }
unit Z.Instance.Tool;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses SysUtils,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings;

type
  // Core_Instance_Count_Tool is used to count activity instances, which can affect performance after startup.
  // Core_Instance_Count_Tool help debug and analyze the state of large programs
  // Need "Intermediate_Instance_Tool" of Define

  TInstance_State_ = record
    Update_Time: TTimeTick;
    Update_Num, Instance_Num: Int64;
  end;

  TInstance_State_Tool__ = class(TBig_Hash_Pair_Pool<SystemString, TInstance_State_>)
  end;

  TInstance_State_Tool = class(TInstance_State_Tool__)
  private
    IsLock_: Boolean;
  public
    constructor Create(const HashSize_: integer);
    function Get_Key_Hash(const Key_: SystemString): THash; override;
    function Compare_Key(const Key_1, Key_2: SystemString): Boolean; override;
    procedure DoFree(var Key: SystemString; var Value: TInstance_State_); override;
    procedure IncValue(Key_: SystemString; Value_: Int64);
    function Do_Sort_By_Time(var L, R: TInstance_State_): integer;
    procedure Sort_By_Time();
    function Do_Sort_By_Instance(var L, R: TInstance_State_): integer;
    procedure Sort_By_Instance();
    function Do_Sort_By_Update(var L, R: TInstance_State_): integer;
    procedure Sort_By_Update();
    function Clone: TInstance_State_Tool;
    function Compare_State(Tool_: TInstance_State_Tool): TInstance_State_Tool;
  end;

var
  Instance_State_Tool: TInstance_State_Tool;

implementation

constructor TInstance_State_Tool.Create(const HashSize_: integer);
var
  tmp: TInstance_State_;
begin
  tmp.Update_Time := GetTimeTick();
  tmp.Update_Num := 0;
  tmp.Instance_Num := 0;
  inherited Create(HashSize_, tmp);
  IsLock_ := False;
end;

function TInstance_State_Tool.Get_Key_Hash(const Key_: SystemString): THash;
begin
  Result := FastHashSystemString(Key_);
  Result := Get_CRC32(@Result, SizeOf(THash));
end;

function TInstance_State_Tool.Compare_Key(const Key_1, Key_2: SystemString): Boolean;
begin
  Result := SameText(Key_1, Key_2);
end;

procedure TInstance_State_Tool.DoFree(var Key: SystemString; var Value: TInstance_State_);
begin
  Key := '';
  inherited DoFree(Key, Value);
end;

procedure TInstance_State_Tool.IncValue(Key_: SystemString; Value_: Int64);
var
  p: TInstance_State_Tool__.PValue;
begin
  if Value_ = 0 then
      exit;
  p := Get_Value_Ptr(Key_);
  p^.Instance_Num := p^.Instance_Num + Value_;
  if p^.Instance_Num < 0 then
      p^.Instance_Num := 0;
  inc(p^.Update_Num);
  p^.Update_Time := GetTimeTick();
end;

function TInstance_State_Tool.Do_Sort_By_Time(var L, R: TInstance_State_): integer;
begin
  if L.Update_Time < R.Update_Time then
      Result := -1
  else if L.Update_Time > R.Update_Time then
      Result := 1
  else
      Result := 0;
end;

procedure TInstance_State_Tool.Sort_By_Time;
begin
  Sort_Value_M(Do_Sort_By_Time);
end;

function TInstance_State_Tool.Do_Sort_By_Instance(var L, R: TInstance_State_): integer;
begin
  if L.Instance_Num < R.Instance_Num then
      Result := -1
  else if L.Instance_Num > R.Instance_Num then
      Result := 1
  else
      Result := 0;
end;

procedure TInstance_State_Tool.Sort_By_Instance;
begin
  Sort_Value_M(Do_Sort_By_Instance);
end;

function TInstance_State_Tool.Do_Sort_By_Update(var L, R: TInstance_State_): integer;
begin
  if L.Update_Num < R.Update_Num then
      Result := -1
  else if L.Update_Num > R.Update_Num then
      Result := 1
  else
      Result := 0;
end;

procedure TInstance_State_Tool.Sort_By_Update;
begin
  Sort_Value_M(Do_Sort_By_Update);
end;

function TInstance_State_Tool.Clone: TInstance_State_Tool;
begin
  Result := TInstance_State_Tool.Create(GetHashSize);
  if num > 0 then
    with repeat_ do
      repeat
          Result.Add(queue^.Data^.Data.Primary, queue^.Data^.Data.Second, False);
      until not Next;
end;

function TInstance_State_Tool.Compare_State(Tool_: TInstance_State_Tool): TInstance_State_Tool;
var
  v1, v2, v3: TInstance_State_;
begin
  Result := TInstance_State_Tool.Create(GetHashSize);
  if num > 0 then
    with repeat_ do
      repeat
        v1 := queue^.Data^.Data.Second;
        v2 := Tool_.Get_Key_Value(queue^.Data^.Data.Primary);
        v3.Update_Time := v1.Update_Time - v2.Update_Time;
        v3.Update_Num := v1.Update_Num - v2.Update_Num;
        v3.Instance_Num := v1.Instance_Num - v2.Instance_Num;
        Result.Add(queue^.Data^.Data.Primary, v3, False);
      until not Next;
end;

procedure Inc_Instance_Num___(const Instance_: string);
begin
  if Instance_State_Tool.IsLock_ then
      exit;
  Instance_State_Tool.Queue_Pool.Lock;
  Instance_State_Tool.IsLock_ := True;
  Instance_State_Tool.IncValue(Instance_, 1);
  Instance_State_Tool.IsLock_ := False;
  Instance_State_Tool.Queue_Pool.UnLock;
end;

procedure Dec_Instance_Num___(const Instance_: string);
begin
  if Instance_State_Tool.IsLock_ then
      exit;
  Instance_State_Tool.Queue_Pool.Lock;
  Instance_State_Tool.IsLock_ := True;
  Instance_State_Tool.IncValue(Instance_, -1);
  Instance_State_Tool.IsLock_ := False;
  Instance_State_Tool.Queue_Pool.UnLock;
end;

var
  Backup_Inc_Instance_Num: TOn_Instance_Info = nil;
  Backup_Dec_Instance_Num: TOn_Instance_Info = nil;

initialization

Instance_State_Tool := TInstance_State_Tool.Create($FFFF);
Backup_Inc_Instance_Num := Inc_Instance_Num;
Backup_Dec_Instance_Num := Dec_Instance_Num;
Z.Core.Inc_Instance_Num := Inc_Instance_Num___;
Z.Core.Dec_Instance_Num := Dec_Instance_Num___;

finalization

Inc_Instance_Num := Backup_Inc_Instance_Num;
Dec_Instance_Num := Backup_Dec_Instance_Num;
DisposeObjectAndNil(Instance_State_Tool);

end.
