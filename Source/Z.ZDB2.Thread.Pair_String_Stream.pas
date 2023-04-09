{ ****************************************************************************** }
{ * ZDB 2.0 Pair String-Stream for HPC                                         * }
{ ****************************************************************************** }
unit Z.ZDB2.Thread.Pair_String_Stream;

{$I Z.Define.inc}

interface

uses Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib,
  Z.MemoryStream, Z.HashList.Templet,
  Z.Status, Z.Cipher, Z.ZDB2, Z.ListEngine, Z.TextDataEngine, Z.Notify, Z.IOThread,
  Z.ZDB2.Thread.Queue, Z.ZDB2.Thread;

type
  TZDB2_Pair_String_Stream_Tool = class;
  TZDB2_Pair_String_Stream_Data = class;

  TZDB2_Pair_String_Stream_Pool__ = {$IFDEF FPC}specialize {$ENDIF FPC} TCritical_String_Big_Hash_Pair_Pool<TZDB2_Pair_String_Stream_Data>;

  TZDB2_Pair_String_Stream_Pool = class(TZDB2_Pair_String_Stream_Pool__)
  public
    procedure DoFree(var Key: SystemString; var Value: TZDB2_Pair_String_Stream_Data); override;
    function Compare_Value(const Value_1, Value_2: TZDB2_Pair_String_Stream_Data): Boolean; override;
  end;

  TZDB2_Pair_String_Stream_Data = class(TZDB2_Th_Engine_Data)
  public
    Owner_String_Fragment_Tool: TZDB2_Pair_String_Stream_Tool;
    String_Fragment_Pool_Ptr: TZDB2_Pair_String_Stream_Pool__.PPair_Pool_Value__;
    constructor Create(); override;
    destructor Destroy; override;
  end;

  TZDB2_Pair_String_Stream_Tool = class
  private
    procedure Do_Th_Data_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMS64);
  public
    ZDB2_Marshal: TZDB2_Th_Engine_Marshal;
    String_Pool: TZDB2_Pair_String_Stream_Pool;
    constructor Create(hash_size_: Integer);
    destructor Destroy; override;
    function BuildMemory(): TZDB2_Th_Engine;
    // if encrypt=true defualt password 'DTC40@ZSERVER'
    function BuildOrOpen(FileName_: U_String; OnlyRead_, Encrypt_: Boolean): TZDB2_Th_Engine; overload;
    // if encrypt=true defualt password 'DTC40@ZSERVER'
    function BuildOrOpen(FileName_: U_String; OnlyRead_, Encrypt_: Boolean; cfg: THashStringList): TZDB2_Th_Engine; overload;
    function Begin_Custom_Build: TZDB2_Th_Engine;
    function End_Custom_Build(Eng_: TZDB2_Th_Engine): Boolean;
    procedure Extract_String_Pool(ThNum_: Integer);
    procedure Clear(Delete_Data_: Boolean);
    procedure Delete(Key_: SystemString; Delete_Data_: Boolean);
    function Exists_String_Fragment(Key_: SystemString): Boolean;
    procedure Async_Get_String_Fragment_C(Key_: SystemString; Source: TMS64; OnResult: TOn_Stream_And_State_Event_C); overload;
    procedure Async_Get_String_Fragment_M(Key_: SystemString; Source: TMS64; OnResult: TOn_Stream_And_State_Event_M); overload;
    procedure Async_Get_String_Fragment_P(Key_: SystemString; Source: TMS64; OnResult: TOn_Stream_And_State_Event_P); overload;
    procedure Async_Get_String_Fragment_C(Key_: SystemString; Source: TMem64; OnResult: TOn_Mem64_And_State_Event_C); overload;
    procedure Async_Get_String_Fragment_M(Key_: SystemString; Source: TMem64; OnResult: TOn_Mem64_And_State_Event_M); overload;
    procedure Async_Get_String_Fragment_P(Key_: SystemString; Source: TMem64; OnResult: TOn_Mem64_And_State_Event_P); overload;
    function Get_String_Fragment(Key_: SystemString; IO_: TMS64): Boolean; // sync load fragment
    procedure Set_String_Fragment(Key_: SystemString; IO_: TMS64; Done_Free_IO_: Boolean); // async save fragment
    // check recycle pool
    procedure Check_Recycle_Pool;
    // progress
    function Progress: Boolean;
    // backup
    procedure Backup(Reserve_: Word);
    procedure Backup_If_No_Exists();
    // flush
    procedure Flush;
    // fragment number
    function Num: NativeInt;
    // recompute totalfragment number
    function Total: NativeInt;
    // database space state
    function Database_Size: Int64;
    function Database_Physics_Size: Int64;
    // RemoveDatabaseOnDestroy
    function GetRemoveDatabaseOnDestroy: Boolean;
    procedure SetRemoveDatabaseOnDestroy(const Value: Boolean);
    property RemoveDatabaseOnDestroy: Boolean read GetRemoveDatabaseOnDestroy write SetRemoveDatabaseOnDestroy;
    // wait queue
    procedure Wait();

    class procedure Test();
  end;

implementation

procedure TZDB2_Pair_String_Stream_Pool.DoFree(var Key: SystemString; var Value: TZDB2_Pair_String_Stream_Data);
begin
  if Value <> nil then
    begin
      Value.Owner_String_Fragment_Tool := nil;
      Value.String_Fragment_Pool_Ptr := nil;
      Value.Remove(False);
      Value := nil;
    end;
end;

function TZDB2_Pair_String_Stream_Pool.Compare_Value(const Value_1, Value_2: TZDB2_Pair_String_Stream_Data): Boolean;
begin
  Result := Value_1 = Value_2;
end;

constructor TZDB2_Pair_String_Stream_Data.Create;
begin
  inherited Create;
  Owner_String_Fragment_Tool := nil;
  String_Fragment_Pool_Ptr := nil;
end;

destructor TZDB2_Pair_String_Stream_Data.Destroy;
begin
  if (Owner_String_Fragment_Tool <> nil) and (String_Fragment_Pool_Ptr <> nil) then
    begin
      String_Fragment_Pool_Ptr^.Data.Second := nil;
      TZDB2_Pair_String_Stream_Pool__(Owner_String_Fragment_Tool.String_Pool).Remove(String_Fragment_Pool_Ptr);
    end;
  inherited Destroy;
end;

procedure TZDB2_Pair_String_Stream_Tool.Do_Th_Data_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMS64);
var
  Key_: SystemString;
  obj_: TZDB2_Pair_String_Stream_Data;
begin
  Key_ := IO_.ReadString;
  obj_ := Sender as TZDB2_Pair_String_Stream_Data;
  obj_.Owner_String_Fragment_Tool := self;
  obj_.String_Fragment_Pool_Ptr := String_Pool.Add(Key_, obj_, False);
end;

constructor TZDB2_Pair_String_Stream_Tool.Create(hash_size_: Integer);
begin
  inherited Create;
  ZDB2_Marshal := TZDB2_Th_Engine_Marshal.Create;
  ZDB2_Marshal.Current_Data_Class := TZDB2_Pair_String_Stream_Data;
  String_Pool := TZDB2_Pair_String_Stream_Pool.Create(hash_size_, nil);
end;

destructor TZDB2_Pair_String_Stream_Tool.Destroy;
begin
  DisposeObject(ZDB2_Marshal);
  DisposeObject(String_Pool);
  inherited Destroy;
end;

function TZDB2_Pair_String_Stream_Tool.BuildMemory(): TZDB2_Th_Engine;
begin
  Result := TZDB2_Th_Engine.Create(ZDB2_Marshal);
  Result.Mode := smBigData;
  Result.Database_File := '';
  Result.OnlyRead := False;
  Result.Cipher_Security := TCipherSecurity.csNone;
  Result.Build(ZDB2_Marshal.Current_Data_Class);
end;

function TZDB2_Pair_String_Stream_Tool.BuildOrOpen(FileName_: U_String; OnlyRead_, Encrypt_: Boolean): TZDB2_Th_Engine;
begin
  Result := TZDB2_Th_Engine.Create(ZDB2_Marshal);
  Result.Mode := smNormal;
  Result.Database_File := FileName_;
  Result.OnlyRead := OnlyRead_;

  if Encrypt_ then
      Result.Cipher_Security := TCipherSecurity.csRijndael
  else
      Result.Cipher_Security := TCipherSecurity.csNone;

  Result.Build(ZDB2_Marshal.Current_Data_Class);
  if not Result.Ready then
    begin
      DisposeObjectAndNil(Result);
      Result := BuildMemory();
    end;
end;

function TZDB2_Pair_String_Stream_Tool.BuildOrOpen(FileName_: U_String; OnlyRead_, Encrypt_: Boolean; cfg: THashStringList): TZDB2_Th_Engine;
begin
  Result := TZDB2_Th_Engine.Create(ZDB2_Marshal);
  Result.Mode := smNormal;
  Result.Database_File := FileName_;
  Result.OnlyRead := OnlyRead_;
  if cfg <> nil then
      Result.ReadConfig(FileName_, cfg);

  if Encrypt_ then
      Result.Cipher_Security := TCipherSecurity.csRijndael
  else
      Result.Cipher_Security := TCipherSecurity.csNone;

  Result.Build(ZDB2_Marshal.Current_Data_Class);
  if not Result.Ready then
    begin
      DisposeObjectAndNil(Result);
      Result := BuildMemory();
    end;
end;

function TZDB2_Pair_String_Stream_Tool.Begin_Custom_Build: TZDB2_Th_Engine;
begin
  Result := TZDB2_Th_Engine.Create(ZDB2_Marshal);
end;

function TZDB2_Pair_String_Stream_Tool.End_Custom_Build(Eng_: TZDB2_Th_Engine): Boolean;
begin
  Eng_.Build(ZDB2_Marshal.Current_Data_Class);
  Result := Eng_.Ready;
end;

procedure TZDB2_Pair_String_Stream_Tool.Extract_String_Pool(ThNum_: Integer);
begin
  String_Pool.Clear;
  ZDB2_Marshal.Parallel_Load_M(ThNum_, {$IFDEF FPC}@{$ENDIF FPC}Do_Th_Data_Loaded, nil);
end;

procedure TZDB2_Pair_String_Stream_Tool.Clear(Delete_Data_: Boolean);
begin
  String_Pool.Clear;
  if ZDB2_Marshal.Data_Marshal.Num <= 0 then
      exit;

  if Delete_Data_ then
    begin
      ZDB2_Marshal.Wait_Busy_Task();
      with ZDB2_Marshal.Data_Marshal.Repeat_ do
        repeat
            Queue^.Data.Remove(True);
        until not Next;
      ZDB2_Marshal.Wait_Busy_Task();
    end
  else
    begin
      ZDB2_Marshal.Clear;
    end;
end;

procedure TZDB2_Pair_String_Stream_Tool.Delete(Key_: SystemString; Delete_Data_: Boolean);
var
  data_: TZDB2_Pair_String_Stream_Data;
begin
  data_ := String_Pool[Key_];
  if data_ <> nil then
      data_.Remove(Delete_Data_);
end;

function TZDB2_Pair_String_Stream_Tool.Exists_String_Fragment(Key_: SystemString): Boolean;
begin
  Result := String_Pool.Exists_Key(Key_);
end;

procedure TZDB2_Pair_String_Stream_Tool.Async_Get_String_Fragment_C(Key_: SystemString; Source: TMS64; OnResult: TOn_Stream_And_State_Event_C);
var
  data_: TZDB2_Pair_String_Stream_Data;
  tmp: TZDB2_Th_CMD_Stream_And_State;
begin
  data_ := String_Pool[Key_];
  if data_ = nil then
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
      exit;
    end;
  data_.Async_Load_Data_C(Source, OnResult);
end;

procedure TZDB2_Pair_String_Stream_Tool.Async_Get_String_Fragment_M(Key_: SystemString; Source: TMS64; OnResult: TOn_Stream_And_State_Event_M);
var
  data_: TZDB2_Pair_String_Stream_Data;
  tmp: TZDB2_Th_CMD_Stream_And_State;
begin
  data_ := String_Pool[Key_];
  if data_ = nil then
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
      exit;
    end;
  data_.Async_Load_Data_M(Source, OnResult);
end;

procedure TZDB2_Pair_String_Stream_Tool.Async_Get_String_Fragment_P(Key_: SystemString; Source: TMS64; OnResult: TOn_Stream_And_State_Event_P);
var
  data_: TZDB2_Pair_String_Stream_Data;
  tmp: TZDB2_Th_CMD_Stream_And_State;
begin
  data_ := String_Pool[Key_];
  if data_ = nil then
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
      exit;
    end;
  data_.Async_Load_Data_P(Source, OnResult);
end;

procedure TZDB2_Pair_String_Stream_Tool.Async_Get_String_Fragment_C(Key_: SystemString; Source: TMem64; OnResult: TOn_Mem64_And_State_Event_C);
var
  data_: TZDB2_Pair_String_Stream_Data;
  tmp: TZDB2_Th_CMD_Mem64_And_State;
begin
  data_ := String_Pool[Key_];
  if data_ = nil then
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
      exit;
    end;
  data_.Async_Load_Data_C(Source, OnResult);
end;

procedure TZDB2_Pair_String_Stream_Tool.Async_Get_String_Fragment_M(Key_: SystemString; Source: TMem64; OnResult: TOn_Mem64_And_State_Event_M);
var
  data_: TZDB2_Pair_String_Stream_Data;
  tmp: TZDB2_Th_CMD_Mem64_And_State;
begin
  data_ := String_Pool[Key_];
  if data_ = nil then
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
      exit;
    end;
  data_.Async_Load_Data_M(Source, OnResult);
end;

procedure TZDB2_Pair_String_Stream_Tool.Async_Get_String_Fragment_P(Key_: SystemString; Source: TMem64; OnResult: TOn_Mem64_And_State_Event_P);
var
  data_: TZDB2_Pair_String_Stream_Data;
  tmp: TZDB2_Th_CMD_Mem64_And_State;
begin
  data_ := String_Pool[Key_];
  if data_ = nil then
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
      exit;
    end;
  data_.Async_Load_Data_P(Source, OnResult);
end;

function TZDB2_Pair_String_Stream_Tool.Get_String_Fragment(Key_: SystemString; IO_: TMS64): Boolean;
var
  obj_: TZDB2_Pair_String_Stream_Data;
  tmp: TMem64;
begin
  obj_ := String_Pool.Get_Key_Value(Key_);
  Result := obj_ <> nil;
  if Result then
    begin
      tmp := TMem64.Create;
      Result := obj_.Load_Data(tmp);
      if Result then
        begin
          tmp.IgnoreReadString;
          IO_.WritePtr(tmp.PosAsPtr, tmp.Size - tmp.Position);
          IO_.Position := 0;
        end;
      DisposeObject(tmp);
    end;
end;

procedure TZDB2_Pair_String_Stream_Tool.Set_String_Fragment(Key_: SystemString; IO_: TMS64; Done_Free_IO_: Boolean);
var
  obj_: TZDB2_Pair_String_Stream_Data;
  tmp: TMem64;
begin
  if String_Pool.Exists_Key(Key_) then
    begin
      if Done_Free_IO_ then
          DisposeObject(IO_);
      String_Pool.Key_Value[Key_].MoveToLast;
      exit;
    end;

  tmp := TMem64.CustomCreate(IO_.Size + 100);
  tmp.WriteString(Key_);
  tmp.WritePtr(IO_.Memory, IO_.Size);
  if Done_Free_IO_ then
      DisposeObject(IO_);

  obj_ := ZDB2_Marshal.Add_Data_To_Minimize_Size_Engine as TZDB2_Pair_String_Stream_Data;
  obj_.Owner_String_Fragment_Tool := self;
  obj_.String_Fragment_Pool_Ptr := String_Pool.Add(Key_, obj_, True);
  obj_.Async_Save_And_Free_Data(tmp);
end;

procedure TZDB2_Pair_String_Stream_Tool.Check_Recycle_Pool;
begin
  ZDB2_Marshal.Check_Recycle_Pool;
end;

function TZDB2_Pair_String_Stream_Tool.Progress: Boolean;
begin
  Result := ZDB2_Marshal.Progress;
end;

procedure TZDB2_Pair_String_Stream_Tool.Backup(Reserve_: Word);
begin
  ZDB2_Marshal.Backup(Reserve_);
end;

procedure TZDB2_Pair_String_Stream_Tool.Backup_If_No_Exists;
begin
  ZDB2_Marshal.Backup_If_No_Exists();
end;

procedure TZDB2_Pair_String_Stream_Tool.Flush;
begin
  ZDB2_Marshal.Flush;
end;

function TZDB2_Pair_String_Stream_Tool.Num: NativeInt;
begin
  Result := ZDB2_Marshal.Data_Marshal.Num;
end;

function TZDB2_Pair_String_Stream_Tool.Total: NativeInt;
begin
  Result := ZDB2_Marshal.Total;
end;

function TZDB2_Pair_String_Stream_Tool.Database_Size: Int64;
begin
  Result := ZDB2_Marshal.Database_Size;
end;

function TZDB2_Pair_String_Stream_Tool.Database_Physics_Size: Int64;
begin
  Result := ZDB2_Marshal.Database_Physics_Size;
end;

function TZDB2_Pair_String_Stream_Tool.GetRemoveDatabaseOnDestroy: Boolean;
begin
  Result := ZDB2_Marshal.RemoveDatabaseOnDestroy;
end;

procedure TZDB2_Pair_String_Stream_Tool.SetRemoveDatabaseOnDestroy(const Value: Boolean);
begin
  ZDB2_Marshal.RemoveDatabaseOnDestroy := Value;
end;

procedure TZDB2_Pair_String_Stream_Tool.Wait;
begin
  ZDB2_Marshal.Wait_Busy_Task;
end;

class procedure TZDB2_Pair_String_Stream_Tool.Test;
var
  inst_: TZDB2_Pair_String_Stream_Tool;
  data_List: TStringBigList;
  i: Integer;
  tmp: TMS64;
begin
  inst_ := TZDB2_Pair_String_Stream_Tool.Create($FF);
  // inst_.BuildOrOpen('c:\temp\1.ox2', False, False);
  inst_.BuildOrOpen('', False, False);
  inst_.Extract_String_Pool(4);
  data_List := TStringBigList.Create;

  if inst_.ZDB2_Marshal.Data_Marshal.Num > 0 then
    with inst_.ZDB2_Marshal.Data_Marshal.Repeat_ do
      repeat
          data_List.Add(TZDB2_Pair_String_Stream_Data(Queue^.Data).String_Fragment_Pool_Ptr^.Data.Primary);
      until not Next;

  for i := 0 to 100 do
    begin
      tmp := TMS64.Create;
      tmp.Size := umlRandomRange(16384, 1024 * 1024 * 2);
      TMT19937.Rand32(MaxInt, tmp.Memory, tmp.Size div 4);
      inst_.Set_String_Fragment(umlMD5ToStr(tmp.ToMD5), tmp, True);
      data_List.Add(inst_.String_Pool.Queue_Pool.Last^.Data^.Data.Primary);
    end;
  inst_.ZDB2_Marshal.Wait_Busy_Task;

  if False then
    while data_List.Num > 0 do
      begin
        inst_.Delete(data_List.First^.Data, True);
        data_List.Next;
      end;

  if data_List.Num > 0 then
    with data_List.Repeat_ do
      repeat
        tmp := TMS64.Create;
        inst_.Get_String_Fragment(Queue^.Data, tmp);
        if not umlMD5Compare(tmp.ToMD5, umlStrToMD5(Queue^.Data)) then
            raiseInfo('error.');
        DisposeObject(tmp);
        if i__ mod 2 = 0 then
            inst_.Delete(Queue^.Data, True);
      until not Next;

  inst_.ZDB2_Marshal.Wait_Busy_Task;
  inst_.ZDB2_Marshal.Flush;

  DisposeObject(data_List);
  DisposeObject(inst_);
end;

end.
