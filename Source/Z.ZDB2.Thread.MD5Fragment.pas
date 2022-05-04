{ ****************************************************************************** }
{ * ZDB 2.0 MD5 data Fragment for HPC                                          * }
{ ****************************************************************************** }
unit Z.ZDB2.Thread.MD5Fragment;

{$I Z.Define.inc}

interface

uses Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib,
  Z.MemoryStream, Z.GHashList,
  Z.Status, Z.Cipher, Z.ZDB2, Z.ListEngine, Z.TextDataEngine, Z.Notify, Z.IOThread,
  Z.ZDB2.Thread.Queue, Z.ZDB2.Thread;

type
  TZDB2_MD5_Fragment_Tool = class;
  TZDB2_MD5_Fragment_Data = class;

  TZDB2_MD5_Fragment_Pool__ = {$IFDEF FPC}specialize {$ENDIF FPC} TCritical_MD5_Big_Hash_Pair_Pool<TZDB2_MD5_Fragment_Data>;

  TZDB2_MD5_Fragment_Pool = class(TZDB2_MD5_Fragment_Pool__)
  public
    procedure DoFree(var Key: TMD5; var Value: TZDB2_MD5_Fragment_Data); override;
    function Compare_Key(Key_1, Key_2: TMD5): Boolean; override;
    function Compare_Value(Value_1, Value_2: TZDB2_MD5_Fragment_Data): Boolean; override;
  end;

  TZDB2_MD5_Fragment_Data = class(TZDB2_Th_Engine_Data)
  public
    Owner_MD5_Fragment_Tool: TZDB2_MD5_Fragment_Tool;
    MD5_Fragment_Pool_Ptr: TZDB2_MD5_Fragment_Pool__.PPair_Pool_Value__;
    constructor Create(); override;
    destructor Destroy; override;
  end;

  TZDB2_MD5_Fragment_Tool = class
  private
    procedure Do_Th_Data_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMS64);
  public
    ZDB2_Marshal: TZDB2_Th_Engine_Marshal;
    MD5_Pool: TZDB2_MD5_Fragment_Pool;
    constructor Create(hash_size_: Integer);
    destructor Destroy; override;
    function BuildMemory(): TZDB2_Th_Engine;
    function BuildOrOpen(FileName_: U_String; OnlyRead_, Encrypt_: Boolean): TZDB2_Th_Engine;
    procedure Extract_MD5_Pool(ThNum_, Max_Queue_: Integer);
    procedure Clear(Delete_Data_: Boolean);
    procedure Delete(Key: TMD5; Delete_Data_: Boolean);
    function Exists_MD5_Fragment(MD5_: TMD5): Boolean;
    function Get_MD5_Fragment(MD5_: TMD5; IO_: TMS64): Boolean;
    procedure Set_MD5_Fragment(buff: Pointer; buff_size: Int64); overload;
    procedure Set_MD5_Fragment(IO_: TMS64; Done_Free_IO_: Boolean); overload;
    procedure Set_MD5_Fragment(MD5_: TMD5; IO_: TMS64; Done_Free_IO_: Boolean); overload;
    procedure Flush;
    function Num: NativeInt;
    function Total: NativeInt;
    procedure Wait();

    class procedure Test();
  end;

implementation

procedure TZDB2_MD5_Fragment_Pool.DoFree(var Key: TMD5; var Value: TZDB2_MD5_Fragment_Data);
begin
  if Value <> nil then
    begin
      Value.Owner_MD5_Fragment_Tool := nil;
      Value.MD5_Fragment_Pool_Ptr := nil;
      Value.Remove(False);
      Value := nil;
    end;
end;

function TZDB2_MD5_Fragment_Pool.Compare_Key(Key_1, Key_2: TMD5): Boolean;
begin
  Result := umlMD5Compare(Key_1, Key_2);
end;

function TZDB2_MD5_Fragment_Pool.Compare_Value(Value_1, Value_2: TZDB2_MD5_Fragment_Data): Boolean;
begin
  Result := Value_1 = Value_2;
end;

constructor TZDB2_MD5_Fragment_Data.Create;
begin
  inherited Create;
  Owner_MD5_Fragment_Tool := nil;
  MD5_Fragment_Pool_Ptr := nil;
end;

destructor TZDB2_MD5_Fragment_Data.Destroy;
begin
  if (Owner_MD5_Fragment_Tool <> nil) and (MD5_Fragment_Pool_Ptr <> nil) then
    begin
      MD5_Fragment_Pool_Ptr^.Data.Second := nil;
      TZDB2_MD5_Fragment_Pool__(Owner_MD5_Fragment_Tool.MD5_Pool).Remove(MD5_Fragment_Pool_Ptr);
    end;
  inherited Destroy;
end;

procedure TZDB2_MD5_Fragment_Tool.Do_Th_Data_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMS64);
var
  MD5_: TMD5;
  obj_: TZDB2_MD5_Fragment_Data;
begin
  MD5_ := IO_.ToMD5;
  obj_ := Sender as TZDB2_MD5_Fragment_Data;
  obj_.Owner_MD5_Fragment_Tool := self;
  obj_.MD5_Fragment_Pool_Ptr := MD5_Pool.Add(MD5_, obj_, False);
end;

constructor TZDB2_MD5_Fragment_Tool.Create(hash_size_: Integer);
begin
  inherited Create;
  ZDB2_Marshal := TZDB2_Th_Engine_Marshal.Create;
  ZDB2_Marshal.Current_Data_Class := TZDB2_MD5_Fragment_Data;
  MD5_Pool := TZDB2_MD5_Fragment_Pool.Create(hash_size_, nil);
end;

destructor TZDB2_MD5_Fragment_Tool.Destroy;
begin
  DisposeObject(ZDB2_Marshal);
  DisposeObject(MD5_Pool);
  inherited Destroy;
end;

function TZDB2_MD5_Fragment_Tool.BuildMemory(): TZDB2_Th_Engine;
begin
  Result := TZDB2_Th_Engine.Create(ZDB2_Marshal);
  Result.Mode := smBigData;
  Result.Database_File := '';
  Result.OnlyRead := False;
  Result.Cipher_Security := TCipherSecurity.csNone;
  Result.Build(ZDB2_Marshal.Current_Data_Class);
end;

function TZDB2_MD5_Fragment_Tool.BuildOrOpen(FileName_: U_String; OnlyRead_, Encrypt_: Boolean): TZDB2_Th_Engine;
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
end;

procedure TZDB2_MD5_Fragment_Tool.Extract_MD5_Pool(ThNum_, Max_Queue_: Integer);
begin
  Clear(False);
  ZDB2_Marshal.Parallel_Load_M(ThNum_, Max_Queue_, {$IFDEF FPC}@{$ENDIF FPC}Do_Th_Data_Loaded, nil);
end;

procedure TZDB2_MD5_Fragment_Tool.Clear(Delete_Data_: Boolean);
begin
  MD5_Pool.Clear;
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

procedure TZDB2_MD5_Fragment_Tool.Delete(Key: TMD5; Delete_Data_: Boolean);
var
  data_: TZDB2_MD5_Fragment_Data;
begin
  data_ := MD5_Pool[Key];
  if data_ <> nil then
      data_.Remove(Delete_Data_);
end;

function TZDB2_MD5_Fragment_Tool.Exists_MD5_Fragment(MD5_: TMD5): Boolean;
begin
  Result := MD5_Pool.Exists_Key(MD5_);
end;

function TZDB2_MD5_Fragment_Tool.Get_MD5_Fragment(MD5_: TMD5; IO_: TMS64): Boolean;
var
  obj_: TZDB2_MD5_Fragment_Data;
begin
  obj_ := MD5_Pool.Get_Key_Value(MD5_);
  Result := obj_ <> nil;
  if Result then
      Result := obj_.Load_Data(IO_);
end;

procedure TZDB2_MD5_Fragment_Tool.Set_MD5_Fragment(buff: Pointer; buff_size: Int64);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  m64.Mapping(buff, buff_size);
  Set_MD5_Fragment(m64, True);
end;

procedure TZDB2_MD5_Fragment_Tool.Set_MD5_Fragment(IO_: TMS64; Done_Free_IO_: Boolean);
begin
  Set_MD5_Fragment(IO_.ToMD5, IO_, Done_Free_IO_);
end;

procedure TZDB2_MD5_Fragment_Tool.Set_MD5_Fragment(MD5_: TMD5; IO_: TMS64; Done_Free_IO_: Boolean);
var
  obj_: TZDB2_MD5_Fragment_Data;
begin
  if MD5_Pool.Exists_Key(MD5_) then
    begin
      if Done_Free_IO_ then
          DisposeObject(IO_);
      exit;
    end;

  obj_ := ZDB2_Marshal.Add_Data_To_Minimize_Size_Engine as TZDB2_MD5_Fragment_Data;
  obj_.Owner_MD5_Fragment_Tool := self;
  obj_.MD5_Fragment_Pool_Ptr := MD5_Pool.Add(MD5_, obj_, True);
  if Done_Free_IO_ then
      obj_.Async_Save_And_Free_Data(IO_)
  else
      obj_.Async_Save_And_Free_Data(IO_.Clone);
end;

procedure TZDB2_MD5_Fragment_Tool.Flush;
begin
  ZDB2_Marshal.Flush;
end;

function TZDB2_MD5_Fragment_Tool.Num: NativeInt;
begin
  Result := ZDB2_Marshal.Data_Marshal.Num;
end;

function TZDB2_MD5_Fragment_Tool.Total: NativeInt;
begin
  Result := ZDB2_Marshal.Total;
end;

procedure TZDB2_MD5_Fragment_Tool.Wait;
begin
  ZDB2_Marshal.Wait_Busy_Task;
end;

class procedure TZDB2_MD5_Fragment_Tool.Test;
var
  inst_: TZDB2_MD5_Fragment_Tool;
  data_List: TMD5_Big_Pool;
  i: Integer;
  tmp: TMS64;
begin
  inst_ := TZDB2_MD5_Fragment_Tool.Create($FF);
  // inst_.BuildOrOpen('c:\temp\1.ox2', False, False);
  inst_.BuildOrOpen('', False, False);
  inst_.Extract_MD5_Pool(4, 100);
  data_List := TMD5_Big_Pool.Create;

  if inst_.ZDB2_Marshal.Data_Marshal.Num > 0 then
    with inst_.ZDB2_Marshal.Data_Marshal.Repeat_ do
      repeat
          data_List.Add(TZDB2_MD5_Fragment_Data(Queue^.Data).MD5_Fragment_Pool_Ptr^.Data.Primary);
      until not Next;

  for i := 0 to 100 do
    begin
      tmp := TMS64.Create;
      tmp.Size := umlRandomRange(16384, 1024 * 1024 * 2);
      TMT19937.Rand32(MaxInt, tmp.Memory, tmp.Size div 4);
      inst_.Set_MD5_Fragment(tmp, True);
      data_List.Add(inst_.MD5_Pool.Queue_Pool.Last^.Data^.Data.Primary);
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
        if i__ mod 2 = 0 then
            inst_.Delete(Queue^.Data, True);
      until not Next;

  inst_.ZDB2_Marshal.Wait_Busy_Task;
  inst_.ZDB2_Marshal.Flush;

  DisposeObject(data_List);
  DisposeObject(inst_);
end;

end.
