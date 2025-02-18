(*
https://zpascal.net
https://github.com/PassByYou888/ZNet
https://github.com/PassByYou888/zRasterization
https://github.com/PassByYou888/ZSnappy
https://github.com/PassByYou888/Z-AI1.4
https://github.com/PassByYou888/ZAI_1.41
https://github.com/PassByYou888/InfiniteIoT
https://github.com/PassByYou888/zMonitor_3rd_Core
https://github.com/PassByYou888/tcmalloc4p
https://github.com/PassByYou888/jemalloc4p
https://github.com/PassByYou888/zCloud
https://github.com/PassByYou888/ZServer4D
https://github.com/PassByYou888/zShell
https://github.com/PassByYou888/ZDB2.0
https://github.com/PassByYou888/zGameWare
https://github.com/PassByYou888/CoreCipher
https://github.com/PassByYou888/zChinese
https://github.com/PassByYou888/zSound
https://github.com/PassByYou888/zExpression
https://github.com/PassByYou888/ZInstaller2.0
https://github.com/PassByYou888/zAI
https://github.com/PassByYou888/NetFileService
https://github.com/PassByYou888/zAnalysis
https://github.com/PassByYou888/PascalString
https://github.com/PassByYou888/zInstaller
https://github.com/PassByYou888/zTranslate
https://github.com/PassByYou888/zVision
https://github.com/PassByYou888/FFMPEG-Header
*)
{ ****************************************************************************** }
{ * Lite-Data templet for HPC                                                  * }
{ ****************************************************************************** }
unit Z.ZDB2.Thread.LiteData;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses DateUtils, SysUtils,
  Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ELSE FPC}
  System.IOUtils,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Int128,
  Z.MemoryStream,
  Z.Status, Z.Cipher, Z.ZDB2, Z.ListEngine, Z.TextDataEngine, Z.IOThread,
  Z.HashList.Templet, Z.DFE, Z.Geometry2D,
  Z.Notify, Z.ZDB2.Thread.Queue, Z.ZDB2.Thread;

type
  { Pre }
  TZDB2_Lite = class;
  TZDB2_Lite_Data = class;
  TZDB2_Lite_Data_Class = class of TZDB2_Lite_Data;

  { Sequence ID pool structure, providing reverse lookup function }
  TZDB2_Lite_Sequence_ID_Pool = TCritical_Big_Hash_Pair_Pool<Int64, TZDB2_Lite_Data>;

  { Chain tools, data exchange, caching, batching, computation, classification }
  TZDB2_Lite_Data_Pool = TBigList<TZDB2_Th_Engine_Data>;

  TZDB2_Lite_Sequence_ID_List = TBigList<Int64>;
  TZDB2_Lite_Sequence_ID_Array = TZDB2_Lite_Sequence_ID_List.TArray_T_;

  TZDB2_Lite_Data = class(TZDB2_Th_Engine_Data)
  private
    FOwner_Lite: TZDB2_Lite;
    FSequence_ID: Int64;
  public
    property Owner_Lite: TZDB2_Lite read FOwner_Lite;
    property Sequence_ID: Int64 read FSequence_ID write FSequence_ID;
    constructor Create(); override;
    destructor Destroy; override;
    procedure Do_Remove(); override;
    function Encode_To_ZDB2_Data(Data_Source: TMS64; AutoFree_: Boolean): TMem64; virtual;
    function Decode_From_ZDB2_Data(Data_Source: TMem64; Update_: Boolean): TMS64; virtual;
    procedure Encode_External_Header_Data(Data_Source: TMem64); virtual;
    procedure Decode_External_Header_Data(Data_Source: TMem64); virtual;
  end;

  TLite_Th_Engine_Marshal = class(TZDB2_Th_Engine_Marshal)
  public
    Owner_Lite: TZDB2_Lite;
    procedure Do_Add_Data(Sender: TZDB2_Th_Engine_Data); override;
    procedure Do_Remove_Data(Sender: TZDB2_Th_Engine_Data); override;
    procedure Prepare_Flush_External_Header(Th_Engine_: TZDB2_Th_Engine; var Sequence_Table: TZDB2_BlockHandle; Flush_Instance_Pool: TZDB2_Th_Engine_Data_Instance_Pool; External_Header_Data_: TMem64); override;
    procedure Do_Extract_Th_Eng(ThSender: TCompute); virtual;
    procedure Extract_External_Header(var Extract_Done: Boolean); virtual;
    function Begin_Custom_Build: TZDB2_Th_Engine; virtual;
    function End_Custom_Build(Eng_: TZDB2_Th_Engine): Boolean; virtual;
  end;

  TLite_Th_Engine_Marshal_Class = class of TLite_Th_Engine_Marshal;

  TBaseClass = class of TLite_Th_Engine_Marshal;

  TOn_Loaded_Sequence_ID = procedure(Sender: TZDB2_Lite; Data: TZDB2_Lite_Data) of object;

  TZDB2_Lite = class(TCore_Object_Intermediate)
  private
    FCritical: TCritical;
    FBatch_Post_Num: Integer;
    FCurrent_Lite_Sequence_ID: Int64;
    FLite_Data_Class: TZDB2_Lite_Data_Class;
    FLite_Th_Engine_Marshal_Class: TLite_Th_Engine_Marshal_Class;
    FLite: TLite_Th_Engine_Marshal;
    FLite_Sequence_Pool: TZDB2_Lite_Sequence_ID_Pool;
    FLite_Engine_External_Header_Optimzied_Technology: Boolean;
    FOn_Loaded_Sequence_ID: TOn_Loaded_Sequence_ID;
    procedure Set_Lite_Data_Class(const Value: TZDB2_Lite_Data_Class);
  public
    property Lite_Data_Class: TZDB2_Lite_Data_Class read FLite_Data_Class write Set_Lite_Data_Class;
    property Lite: TLite_Th_Engine_Marshal read FLite;
    property Lite_Sequence_Pool: TZDB2_Lite_Sequence_ID_Pool read FLite_Sequence_Pool;
    property On_Loaded_Sequence_ID: TOn_Loaded_Sequence_ID read FOn_Loaded_Sequence_ID write FOn_Loaded_Sequence_ID;
    property Batch_Post_Num: Integer read FBatch_Post_Num;
    property Post_Batch_Num: Integer read FBatch_Post_Num;

    constructor Create(); overload;
    constructor Create(Lite_Data_Class_: TZDB2_Lite_Data_Class); overload;
    constructor Create(Lite_Data_Class_: TZDB2_Lite_Data_Class; Lite_Th_Engine_Marshal_ClasLite_: TLite_Th_Engine_Marshal_Class); overload;
    destructor Destroy; override;

    procedure Build_DB_From_Script(Root_Path_: U_String; te: TTextDataEngine; OnlyRead_: Boolean); virtual;
    function Make_Script(Name_: U_String; Lite_Num: Integer;
      First_Inited_Physics_Space, Delta: Int64; BlockSize: Word;
      Cipher_Security_: TCipherSecurity; Temp_Runtime_Model: Boolean): TTextDataEngine;
    function Open_DB(script_conf_: U_String): Boolean; overload;
    function Open_DB(script_conf_: U_String; OnlyRead_: Boolean): Boolean; overload;
    function Open_DB(path_: U_String; script_code_: TTextDataEngine; OnlyRead_: Boolean): Boolean; overload;
    procedure Close_DB; virtual;

    property Lite_Engine_External_Header_Optimzied_Technology: Boolean read FLite_Engine_External_Header_Optimzied_Technology write FLite_Engine_External_Header_Optimzied_Technology;
    procedure Do_Th_Lite_Data_Full_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMS64); virtual;
    procedure Extract_Lite_Full(ThNum_: Integer); virtual;
    procedure Do_Th_Lite_Data_Block_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMem64); virtual;
    procedure Extract_Lite_Block(ThNum_: Integer; Block_Index, Block_Offset, Block_Read_Size: Integer); virtual;
    procedure Do_Th_Lite_Data_Position_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMS64); virtual;
    procedure Extract_Lite_Position(ThNum_: Integer; Position_Offset, Position_Read_Size: Int64); virtual;
    function Do_Lite_Data_Sort_By_Sequence_ID(var L, R: TZDB2_Th_Engine_Data): Integer; virtual;
    procedure Rebuild_Lite_Requence; virtual;
    function Create_Lite_Data(): TZDB2_Lite_Data; virtual;
    procedure Check_Recycle_Pool; virtual;
    function Progress: Integer; virtual;
    procedure Backup(Reserve_: Word); virtual;
    procedure Backup_If_No_Exists(); virtual;
    procedure Flush(WaitQueue_: Boolean); virtual;
    function Flush_Is_Busy: Boolean;
    function Database_Size: Int64;
    function Database_Physics_Size: Int64;
    function Total: NativeInt;
    function QueueNum: NativeInt;
    function Fragment_Buffer_Num: Int64;
    function Fragment_Buffer_Memory: Int64;

    { test case }
    class procedure Do_Test_Post(Eng_: TZDB2_Lite);
    class procedure Do_Test_Get_Data(Eng_: TZDB2_Lite);
    class procedure Test();
  end;

implementation

constructor TZDB2_Lite_Data.Create;
begin
  inherited Create;
  FSequence_ID := 0;
  FOwner_Lite := nil;
end;

destructor TZDB2_Lite_Data.Destroy;
begin
  if FOwner_Lite <> nil then
      FOwner_Lite.FLite_Sequence_Pool.Delete(FSequence_ID);
  inherited Destroy;
end;

procedure TZDB2_Lite_Data.Do_Remove();
begin
  if FOwner_Lite <> nil then
      FOwner_Lite.Lite_Sequence_Pool.Delete(FSequence_ID);
  inherited Do_Remove();
end;

function TZDB2_Lite_Data.Encode_To_ZDB2_Data(Data_Source: TMS64; AutoFree_: Boolean): TMem64;
begin
  Result := TMem64.Create;
  Result.Size := Data_Source.Size + 8;
  Result.Position := 0;
  Result.WriteInt64(FSequence_ID);
  Result.WritePtr(Data_Source.Memory, Data_Source.Size);
  if AutoFree_ then
      DisposeObject(Data_Source);
end;

function TZDB2_Lite_Data.Decode_From_ZDB2_Data(Data_Source: TMem64; Update_: Boolean): TMS64;
begin
  if Update_ then
    begin
      Data_Source.Position := 0;
      FSequence_ID := Data_Source.ReadInt64;
    end
  else
      Data_Source.Position := 8;
  Result := TMS64.Create;
  Result.Mapping(Data_Source.PosAsPtr, Data_Source.Size - Data_Source.Position);
  Do_Ready;
end;

procedure TZDB2_Lite_Data.Encode_External_Header_Data(Data_Source: TMem64);
begin
  Data_Source.WriteInt64(Sequence_ID);
end;

procedure TZDB2_Lite_Data.Decode_External_Header_Data(Data_Source: TMem64);
begin
  Sequence_ID := Data_Source.ReadInt64;
  Do_Ready;
end;

procedure TLite_Th_Engine_Marshal.Do_Add_Data(Sender: TZDB2_Th_Engine_Data);
begin
  TZDB2_Lite_Data(Sender).FOwner_Lite := Owner_Lite;
end;

procedure TLite_Th_Engine_Marshal.Do_Remove_Data(Sender: TZDB2_Th_Engine_Data);
begin
end;

procedure TLite_Th_Engine_Marshal.Prepare_Flush_External_Header(Th_Engine_: TZDB2_Th_Engine; var Sequence_Table: TZDB2_BlockHandle; Flush_Instance_Pool: TZDB2_Th_Engine_Data_Instance_Pool; External_Header_Data_: TMem64);
var
  tmp: TMem64;
begin
  if Flush_Instance_Pool.Num <= 0 then
      exit;
  if not TZDB2_Lite(Owner).Lite_Engine_External_Header_Optimzied_Technology then
      exit;

  External_Header_Data_.Clear;
  External_Header_Data_.WriteInt64(Flush_Instance_Pool.Num);
  with Flush_Instance_Pool.Repeat_ do
    repeat
      External_Header_Data_.WriteInt32(Queue^.Data.ID);
      tmp := TMem64.CustomCreate(1536);
      TZDB2_Lite_Data(Queue^.Data).Encode_External_Header_Data(tmp);
      External_Header_Data_.WriteInt32(tmp.Size);
      External_Header_Data_.WritePtr(tmp.Memory, tmp.Size);
      DisposeObject(tmp);
    until not Next;
end;

procedure TLite_Th_Engine_Marshal.Do_Extract_Th_Eng(ThSender: TCompute);
var
  Eng_: TZDB2_Th_Engine;
  Error_Num: PInt64;
  num_: Int64;
  ID_: Integer;
  siz_: Integer;
  Inst_: TZDB2_Lite_Data;
  tmp: TMem64;
begin
  Eng_ := ThSender.UserObject as TZDB2_Th_Engine;
  Error_Num := ThSender.UserData;

  Eng_.External_Header_Data.Position := 0;
  num_ := Eng_.External_Header_Data.ReadInt64;

  while num_ > 0 do
    begin
      ID_ := Eng_.External_Header_Data.ReadInt32;
      Inst_ := TZDB2_Lite_Data(Eng_.Th_Engine_ID_Data_Pool[ID_]);
      if Inst_ = nil then
        begin
          AtomInc(Error_Num^);
          break;
        end;
      try
        siz_ := Eng_.External_Header_Data.ReadInt32;
        tmp := TMem64.Create;
        tmp.Mapping(Eng_.External_Header_Data.PosAsPtr, siz_);
        Inst_.Decode_External_Header_Data(tmp);
        DisposeObject(tmp);
        Eng_.External_Header_Data.Position := Eng_.External_Header_Data.Position + siz_;
      except
        AtomInc(Error_Num^);
        break;
      end;
      dec(num_);
    end;
end;

procedure TLite_Th_Engine_Marshal.Extract_External_Header(var Extract_Done: Boolean);
var
  Error_Num: Int64;

  function Check_External_Header: NativeInt;
  begin
    { external-header optimize tech }
    Result := 0;
    if Engine_Pool.Num > 0 then
      with Engine_Pool.Repeat_ do
        repeat
          if Queue^.Data.External_Header_Data.Size >= 8 then
              Inc(Result);
        until not Next;
  end;

var
  Signal_: TBool_Signal_Array;
begin
  Extract_Done := False;
  if not TZDB2_Lite(Owner).Lite_Engine_External_Header_Optimzied_Technology then
      exit;
  Error_Num := 0;
  if Check_External_Header <> Engine_Pool.Num then
      exit;
  if Engine_Pool.Num > 0 then
    begin
      SetLength(Signal_, Engine_Pool.Num);
      with Engine_Pool.Repeat_ do
        repeat
            TCompute.RunM(@Error_Num, Queue^.Data, Do_Extract_Th_Eng, @Signal_[I__], nil);
        until not Next;
      Wait_All_Signal(Signal_, False);
    end;
  Extract_Done := Error_Num = 0;
end;

function TLite_Th_Engine_Marshal.Begin_Custom_Build: TZDB2_Th_Engine;
begin
  Result := TZDB2_Th_Engine.Create(Self);
end;

function TLite_Th_Engine_Marshal.End_Custom_Build(Eng_: TZDB2_Th_Engine): Boolean;
begin
  Eng_.Build(Current_Data_Class);
  Result := Eng_.Ready;
end;

procedure TZDB2_Lite.Set_Lite_Data_Class(const Value: TZDB2_Lite_Data_Class);
begin
  FLite_Data_Class := Value;
  FLite.Current_Data_Class := FLite_Data_Class;
end;

constructor TZDB2_Lite.Create();
begin
  inherited Create;
  FBatch_Post_Num := 0;
  FCurrent_Lite_Sequence_ID := 1;
  FCritical := TCritical.Create;
  FLite_Data_Class := TZDB2_Lite_Data;
  FLite_Th_Engine_Marshal_Class := TLite_Th_Engine_Marshal;
  FLite := FLite_Th_Engine_Marshal_Class.Create(Self);
  FLite.Current_Data_Class := FLite_Data_Class;
  FLite.Owner_Lite := Self;
  FLite_Sequence_Pool := TZDB2_Lite_Sequence_ID_Pool.Create(1024 * 1024, nil);
  FLite_Engine_External_Header_Optimzied_Technology := False;
  FOn_Loaded_Sequence_ID := nil;
end;

constructor TZDB2_Lite.Create(Lite_Data_Class_: TZDB2_Lite_Data_Class);
begin
  inherited Create;
  FBatch_Post_Num := 0;
  FCurrent_Lite_Sequence_ID := 1;
  FCritical := TCritical.Create;
  FLite_Data_Class := Lite_Data_Class_;
  FLite_Th_Engine_Marshal_Class := TLite_Th_Engine_Marshal;
  FLite := FLite_Th_Engine_Marshal_Class.Create(Self);
  FLite.Current_Data_Class := FLite_Data_Class;
  FLite.Owner_Lite := Self;
  FLite_Sequence_Pool := TZDB2_Lite_Sequence_ID_Pool.Create(1024 * 1024, nil);
  FLite_Engine_External_Header_Optimzied_Technology := False;
  FOn_Loaded_Sequence_ID := nil;
end;

constructor TZDB2_Lite.Create(Lite_Data_Class_: TZDB2_Lite_Data_Class; Lite_Th_Engine_Marshal_ClasLite_: TLite_Th_Engine_Marshal_Class);
begin
  inherited Create;
  FBatch_Post_Num := 0;
  FCurrent_Lite_Sequence_ID := 1;
  FCritical := TCritical.Create;
  FLite_Data_Class := Lite_Data_Class_;
  FLite_Th_Engine_Marshal_Class := Lite_Th_Engine_Marshal_ClasLite_;
  FLite := FLite_Th_Engine_Marshal_Class.Create(Self);
  FLite.Current_Data_Class := FLite_Data_Class;
  FLite.Owner_Lite := Self;
  FLite_Sequence_Pool := TZDB2_Lite_Sequence_ID_Pool.Create(1024 * 1024, nil);
  FLite_Engine_External_Header_Optimzied_Technology := False;
  FOn_Loaded_Sequence_ID := nil;
end;

destructor TZDB2_Lite.Destroy;
begin
  { stop copy }
  FLite.Stop_Copy;

  { safe flush }
  FLite.Flush(False);

  { reset owner }
  if FLite.Data_Marshal.Num > 0 then
    with FLite.Data_Marshal.Repeat_ do
      repeat
          TZDB2_Lite_Data(Queue^.Data).FOwner_Lite := nil;
      until not Next;

  { free db }
  DisposeObjectAndNil(FLite);
  DisposeObjectAndNil(FLite_Sequence_Pool);

  DisposeObjectAndNil(FCritical);
  inherited Destroy;
end;

procedure TZDB2_Lite.Build_DB_From_Script(Root_Path_: U_String; te: TTextDataEngine; OnlyRead_: Boolean);
var
  L: TPascalStringList;
  i: Integer;
  HL: THashStringList;
  n: U_String;
  Eng_: TZDB2_Th_Engine;
begin
  umlCreateDirectory(Root_Path_);
  L := TPascalStringList.Create;
  te.GetSectionList(L);

  if L.Count > 0 then
    begin
      for i := 0 to L.Count - 1 do
        begin
          HL := te.HStringList[L[i]];
          { prepare database file }
          n := umlTrimSpace(HL.GetDefaultValue('database', ''));
          if (n <> '') and (not n.Exists(['/', '\'])) then
              HL.SetDefaultValue('database', umlCombineFileName(Root_Path_, n));

          { update OnlyRead }
          HL.SetDefaultText_Bool('OnlyRead', OnlyRead_);

          { extract database type }
          Eng_ := TZDB2_Th_Engine.Create(FLite);
          Eng_.ReadConfig(L[i], HL);
          if Eng_.BlockSize < 1024 then
              Eng_.BlockSize := 1024;
        end;
    end;
  { free temp }
  DisposeObject(L);

  { create or open datgabase }
  FLite.Build();
end;

function TZDB2_Lite.Make_Script(Name_: U_String; Lite_Num: Integer;
  First_Inited_Physics_Space, Delta: Int64; BlockSize: Word;
  Cipher_Security_: TCipherSecurity; Temp_Runtime_Model: Boolean): TTextDataEngine;
var
  tmp: TZDB2_Th_Engine_Marshal;
  i: Integer;
  Eng_: TZDB2_Th_Engine;
  HL: THashStringList;
begin
  Result := TTextDataEngine.Create;
  tmp := FLite_Th_Engine_Marshal_Class.Create(nil);
  for i := 0 to Lite_Num - 1 do
    begin
      Eng_ := TZDB2_Th_Engine.Create(tmp);
      Eng_.Name := PFormat('%s(%d)', [Name_.Text, i + 1]);
      Eng_.Database_File := PFormat('%s(%d).ZDB2', [Name_.Text, i + 1]);
      Eng_.Fast_Alloc_Space := True;
      Eng_.First_Inited_Physics_Space := First_Inited_Physics_Space;
      Eng_.Auto_Append_Space := True;
      Eng_.Delta := Delta;
      Eng_.BlockSize := BlockSize;
      Eng_.Cipher_Security := Cipher_Security_;
      if Temp_Runtime_Model then
        begin
          Eng_.RemoveDatabaseOnDestroy := True;
          Eng_.Fragment_Space_Enabled := False;
          Eng_.External_Header_Technology := False;
        end;
      HL := Result.HStringList[Eng_.Name];
      Eng_.WriteConfig(HL);
    end;

  DisposeObject(tmp);
end;

function TZDB2_Lite.Open_DB(script_conf_: U_String): Boolean;
begin
  Result := Open_DB(script_conf_, False);
end;

function TZDB2_Lite.Open_DB(script_conf_: U_String; OnlyRead_: Boolean): Boolean;
var
  te: TTextDataEngine;
begin
  Result := False;
  if not umlFileExists(script_conf_) then
      exit;
  te := TTextDataEngine.Create;
  try
    te.LoadFromFile(script_conf_);
    Build_DB_From_Script(umlGetFilePath(script_conf_), te, OnlyRead_);
    Result := True;
  except
      Close_DB;
  end;
  DisposeObject(te);
end;

function TZDB2_Lite.Open_DB(path_: U_String; script_code_: TTextDataEngine; OnlyRead_: Boolean): Boolean;
begin
  Result := False;
  Close_DB;
  try
    Build_DB_From_Script(path_, script_code_, OnlyRead_);
    Result := True;
  except
  end;
end;

procedure TZDB2_Lite.Close_DB;
begin
  { stop copy }
  FLite.Stop_Copy;

  { safe flush }
  FLite.Flush(False);

  { reset owner }
  if FLite.Data_Marshal.Num > 0 then
    with FLite.Data_Marshal.Repeat_ do
      repeat
          TZDB2_Lite_Data(Queue^.Data).FOwner_Lite := nil;
      until not Next;

  { free db }
  DisposeObjectAndNil(FLite);
  DisposeObjectAndNil(FLite_Sequence_Pool);

  { rebuild }
  FCurrent_Lite_Sequence_ID := 1;

  FLite := FLite_Th_Engine_Marshal_Class.Create(Self);
  FLite.Current_Data_Class := FLite_Data_Class;
  FLite.Owner_Lite := Self;
  FLite_Sequence_Pool := TZDB2_Lite_Sequence_ID_Pool.Create(1024 * 1024, nil);
end;

procedure TZDB2_Lite.Do_Th_Lite_Data_Full_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMS64);
var
  Inst_: TZDB2_Lite_Data;
begin
  Inst_ := Sender as TZDB2_Lite_Data;
  DisposeObject(Inst_.Decode_From_ZDB2_Data(IO_.Mem64, True));
end;

procedure TZDB2_Lite.Extract_Lite_Full(ThNum_: Integer);
var
  Extract_Done: Boolean;
begin
  FLite_Sequence_Pool.Clear;

  Extract_Done := False;
  if FLite_Engine_External_Header_Optimzied_Technology then
      FLite.Extract_External_Header(Extract_Done); { external-header optimize tech }
  if not Extract_Done then
    begin
      FLite.Parallel_Load_M(ThNum_, Do_Th_Lite_Data_Full_Loaded, nil);
    end;

  Rebuild_Lite_Requence();
end;

procedure TZDB2_Lite.Do_Th_Lite_Data_Block_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMem64);
var
  Inst_: TZDB2_Lite_Data;
begin
  Inst_ := Sender as TZDB2_Lite_Data;
  DisposeObject(Inst_.Decode_From_ZDB2_Data(IO_, True));
end;

procedure TZDB2_Lite.Extract_Lite_Block(ThNum_: Integer; Block_Index, Block_Offset, Block_Read_Size: Integer);
var
  Extract_Done: Boolean;
begin
  FLite_Sequence_Pool.Clear;

  Extract_Done := False;
  if FLite_Engine_External_Header_Optimzied_Technology then
      FLite.Extract_External_Header(Extract_Done); { external-header optimize tech }
  if not Extract_Done then
    begin
      FLite.Parallel_Block_Load_M(ThNum_, Block_Index, Block_Offset, Block_Read_Size, Do_Th_Lite_Data_Block_Loaded, nil);
    end;

  Rebuild_Lite_Requence();
end;

procedure TZDB2_Lite.Do_Th_Lite_Data_Position_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMS64);
var
  Inst_: TZDB2_Lite_Data;
begin
  Inst_ := Sender as TZDB2_Lite_Data;
  DisposeObject(Inst_.Decode_From_ZDB2_Data(IO_.Mem64, True));
end;

procedure TZDB2_Lite.Extract_Lite_Position(ThNum_: Integer; Position_Offset, Position_Read_Size: Int64);
var
  Extract_Done: Boolean;
begin
  FLite_Sequence_Pool.Clear;

  Extract_Done := False;
  if FLite_Engine_External_Header_Optimzied_Technology then
      FLite.Extract_External_Header(Extract_Done); { external-header optimize tech }
  if not Extract_Done then
    begin
      FLite.Parallel_Position_Load_M(ThNum_, Position_Offset, Position_Read_Size, Do_Th_Lite_Data_Position_Loaded, nil);
    end;

  Rebuild_Lite_Requence();
end;

function TZDB2_Lite.Do_Lite_Data_Sort_By_Sequence_ID(var L, R: TZDB2_Th_Engine_Data): Integer;
begin
  Result := CompareInt64(TZDB2_Lite_Data(L).FSequence_ID, TZDB2_Lite_Data(R).FSequence_ID);
end;

procedure TZDB2_Lite.Rebuild_Lite_Requence;
begin
  FCurrent_Lite_Sequence_ID := 1;
  if FLite.Data_Marshal.Num > 0 then
    begin
      { Restore structure pools in order }
      DoStatus('Rebuild Sequence..');
      FLite.Sort_M(Do_Lite_Data_Sort_By_Sequence_ID);
      FCurrent_Lite_Sequence_ID := TZDB2_Lite_Data(FLite.Data_Marshal.Last^.Data).FSequence_ID + 1;

      { Build Sequence_ID reverse lookup structure }
      if Assigned(FOn_Loaded_Sequence_ID) then
        begin
          with FLite.Data_Marshal.Repeat_ do
            repeat
              FLite_Sequence_Pool.Add(TZDB2_Lite_Data(Queue^.Data).FSequence_ID, TZDB2_Lite_Data(Queue^.Data), False);
              FOn_Loaded_Sequence_ID(Self, TZDB2_Lite_Data(Queue^.Data));
            until not Next;
        end
      else
        begin
          with FLite.Data_Marshal.Repeat_ do
            repeat
                FLite_Sequence_Pool.Add(TZDB2_Lite_Data(Queue^.Data).FSequence_ID, TZDB2_Lite_Data(Queue^.Data), False);
            until not Next;
        end;
    end;
end;

function TZDB2_Lite.Create_Lite_Data: TZDB2_Lite_Data;
var
  data_inst_: TZDB2_Lite_Data;
begin
  FCritical.Lock;
  try
      data_inst_ := FLite.Add_Data_To_Minimize_Size_Engine as TZDB2_Lite_Data;
  except
      data_inst_ := nil;
  end;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FSequence_ID := FCurrent_Lite_Sequence_ID;
      FCritical.Inc_(FCurrent_Lite_Sequence_ID);
      FLite_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);
    end;
  Result := data_inst_;
end;

procedure TZDB2_Lite.Check_Recycle_Pool;
begin
  FLite.Check_Recycle_Pool;
end;

function TZDB2_Lite.Progress: Integer;
begin
  Result := 0;
  if FLite.Progress then
      Inc(Result);
end;

procedure TZDB2_Lite.Backup(Reserve_: Word);
begin
  FLite.Backup(Reserve_);
end;

procedure TZDB2_Lite.Backup_If_No_Exists;
begin
  FLite.Backup_If_No_Exists();
end;

procedure TZDB2_Lite.Flush(WaitQueue_: Boolean);
begin
  FLite.Flush(WaitQueue_);
end;

function TZDB2_Lite.Flush_Is_Busy: Boolean;
begin
  Result := FLite.Flush_Is_Busy;
end;

function TZDB2_Lite.Database_Size: Int64;
begin
  Result := FLite.Database_Size;
end;

function TZDB2_Lite.Database_Physics_Size: Int64;
begin
  Result := FLite.Database_Physics_Size;
end;

function TZDB2_Lite.Total: NativeInt;
begin
  Result := FLite.Total;
end;

function TZDB2_Lite.QueueNum: NativeInt;
begin
  Result := FLite.QueueNum;
end;

function TZDB2_Lite.Fragment_Buffer_Num: Int64;
begin
  Result := FLite.Fragment_Buffer_Num;
end;

function TZDB2_Lite.Fragment_Buffer_Memory: Int64;
begin
  Result := FLite.Fragment_Buffer_Memory;
end;

class procedure TZDB2_Lite.Do_Test_Post(Eng_: TZDB2_Lite);
var
  i: Integer;
  d: TDFE;
  M64: TMS64;
  tmp_m64: TMS64;
begin
  for i := 0 to 300 do
    begin
      d := TDFE.Create;

      d.WriteInt64(umlRR64(0, $FFFFFFFFFFFFFFF));

      tmp_m64 := TMS64.Create;
      d.FastEncodeTo(tmp_m64);
      DisposeObject(d);
      with Eng_.Create_Lite_Data do
          Async_Save_And_Free_Data(Encode_To_ZDB2_Data(tmp_m64, True));

      if i mod 10 = 0 then
          Eng_.Flush(True);
    end;
  Eng_.Flush(True);
end;

class procedure TZDB2_Lite.Do_Test_Get_Data(Eng_: TZDB2_Lite);
var
  num_: Integer;
begin
  num_ := 0;
{$IFDEF DELPHI}
  Eng_.Lite.Begin_Loop;
  if Eng_.Lite.Data_Marshal.Num > 0 then
    with Eng_.Lite.Repeat_ do
      repeat
        while num_ > 10 do
            TCompute.Sleep(1);
        AtomInc(num_);
        Queue^.Data.Async_Load_Stream_P(procedure(Sender: TZDB2_Th_Engine_Data; stream: TMS64; Successed: Boolean)
          var
            tmp: TMS64;
            Inst_: TZDB2_Lite_Data;
          begin
            if not Successed then
                exit;
            Inst_ := Sender as TZDB2_Lite_Data;
            tmp := Inst_.Decode_From_ZDB2_Data(stream.Mem64, False);
            DisposeObject(tmp);
            AtomDec(num_);
          end);
      until not Next;
  Eng_.Lite.End_Loop;
  while num_ > 0 do
      TCompute.Sleep(1);

  while num_ > 0 do
      TCompute.Sleep(1);
{$ENDIF DELPHI}
end;

class procedure TZDB2_Lite.Test;
var
  Eng_: TZDB2_Lite;
  te: TTextDataEngine;
begin
  Eng_ := TZDB2_Lite.Create;
  { make script templet }
  te := Eng_.Make_Script('test', 2, 1 * 1024 * 1024, 1 * 1024 * 1024, 1536, TCipherSecurity.csNone, False);
  Eng_.Build_DB_From_Script(umlCombinePath(umlCurrentPath, 'ZDB2_Lite_Test'), te, False);
  DisposeObject(te);
  Eng_.Lite_Engine_External_Header_Optimzied_Technology := True;
  Eng_.Extract_Lite_Full(10);

  if True then
      Do_Test_Post(Eng_);

  if True then
      Do_Test_Get_Data(Eng_);

  Eng_.Lite.Wait_Busy_Task;
  DisposeObject(Eng_);
end;

end.
 
