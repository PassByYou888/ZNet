{ ****************************************************************************** }
{ * cloud 4.0 File System 3.0 - ZDB2 Lite-Data Engine                          * }
{ ****************************************************************************** }
unit Z.Net.C4_FS3.ZDB2.LiteData;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ELSE FPC}
  System.IOUtils,
{$ENDIF FPC}
  Z.Core, Z.IOThread, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine,
  Z.Geometry2D, Z.DFE, Z.Json, Z.Expression, Z.OpCode,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.HashList.Templet,
  Z.ZDB2, Z.ZDB2.Thread, Z.ZDB2.Thread.APP, Z.ZDB2.Thread.Queue, Z.ZDB2.Thread.LiteData,
  Z.TextDataEngine;

type
  TZDB2_FS3_Lite = class;
  TZDB2_FS3_FileInfo = class;
  TZDB2_FS3_Link_Table = class;
  TZDB2_FS3_Body = class;
  TZDB2_FS3_FileInfo_Pair_Pool_ = TCritical_PascalString_Big_Hash_Pair_Pool<TZDB2_FS3_FileInfo>;

  TZDB2_FS3_FileInfo_Pair_Pool = class(TZDB2_FS3_FileInfo_Pair_Pool_)
  public
    procedure DoFree(var Key: TPascalString; var Value: TZDB2_FS3_FileInfo); override;
  end;

  TZDB2_FS3_Link_Table_MD5_Pair_Pool_ = TCritical_Big_Hash_Pair_Pool<TMD5, TZDB2_FS3_Link_Table>;

  TZDB2_FS3_Link_Table_MD5_Pair_Pool = class(TZDB2_FS3_Link_Table_MD5_Pair_Pool_)
  public
  end;

  TZDB2_FS3_Body_MD5_Pair_Pool_ = TCritical_Big_Hash_Pair_Pool<TMD5, TZDB2_FS3_Body>;

  TZDB2_FS3_Body_MD5_Pair_Pool = class(TZDB2_FS3_Body_MD5_Pair_Pool_)
  end;

  TZDB2_FS3_Life_Pool = TCritical_BigList<TZDB2_FS3_FileInfo>;

{$REGION 'Lite_Interface'}

  TZDB2_FS3_FileInfo = class(TZDB2_Lite_Data)
  private
    FOwner_FS3: TZDB2_FS3_Lite;
    FLife_Pool_Ptr: TZDB2_FS3_Life_Pool.PQueueStruct;
  public
    Relate_Link_Table: Int64; { Associated file link table }
    Life: Double;             { The file lifecycle is infinite if it is equal to or lower than 0 }
    File_Name: U_String;      { file name }
    FileSize: Int64;          { file size }
    Time_: TDateTime;         { File time }
    property Owner_FS3: TZDB2_FS3_Lite read FOwner_FS3;
    constructor Create(); override;
    destructor Destroy; override;
    procedure Do_Remove(); override;
    function Make_Data_Source: TMS64;
    function Decode_From_ZDB2_Data(Data_Source: TMem64; Update_: Boolean): TMS64; override;
    procedure Encode_External_Header_Data(Data_Source: TMem64); override;
    procedure Decode_External_Header_Data(Data_Source: TMem64); override;
  end;

  TZDB2_FS3_FileInfo_Th_Engine_Marshal = class(TLite_Th_Engine_Marshal)
  private
    FOwner_FS3: TZDB2_FS3_Lite;
  public
    property Owner_FS3: TZDB2_FS3_Lite read FOwner_FS3;
    procedure Do_Add_Data(Sender: TZDB2_Th_Engine_Data); override;
    procedure Do_Remove_Data(Sender: TZDB2_Th_Engine_Data); override;
  end;

  TRelate_Info_List = TBigList<Int64>;

  TZDB2_FS3_Link_Table = class(TZDB2_Lite_Data)
  private
    FOwner_FS3: TZDB2_FS3_Lite;
  public
    Reference_Count: Integer; { Reference from file-info }
    File_MD5: TMD5;           { file md5 }
    FileSize: Int64;
    Create_Time: TDateTime;         { create time }
    Relate_Body: TRelate_Info_List; { Associated file body }
    property Owner_FS3: TZDB2_FS3_Lite read FOwner_FS3;
    constructor Create(); override;
    destructor Destroy; override;
    procedure Do_Remove(); override;
    function Make_Data_Source: TMS64;
    procedure Inc_Reference_Count();
    procedure Dec_Reference_Count();
    function Decode_From_ZDB2_Data(Data_Source: TMem64; Update_: Boolean): TMS64; override;
    procedure Encode_External_Header_Data(Data_Source: TMem64); override;
    procedure Decode_External_Header_Data(Data_Source: TMem64); override;
  end;

  TZDB2_FS3_Link_Table_Th_Engine_Marshal = class(TLite_Th_Engine_Marshal)
  private
    FOwner_FS3: TZDB2_FS3_Lite;
  public
    property Owner_FS3: TZDB2_FS3_Lite read FOwner_FS3;
    procedure Do_Add_Data(Sender: TZDB2_Th_Engine_Data); override;
    procedure Do_Remove_Data(Sender: TZDB2_Th_Engine_Data); override;
  end;

  TZDB2_FS3_Body = class(TZDB2_Lite_Data)
  private
    FOwner_FS3: TZDB2_FS3_Lite;
  public
    Reference_Count: Integer; { Reference from link-table }
    Body_MD5: TMD5;
    Body_Size: Int64;
    Create_Time: TDateTime;
    property Owner_FS3: TZDB2_FS3_Lite read FOwner_FS3;
    constructor Create(); override;
    destructor Destroy; override;
    procedure Do_Remove(); override;
    procedure Inc_Reference_Count();
    procedure Dec_Reference_Count();
    function Make_Data_Source(Body: TCore_Stream; AutoFree_Body: Boolean): TMS64;
    function Decode_From_ZDB2_Data(Data_Source: TMem64; Update_: Boolean): TMS64; override;
    procedure Encode_External_Header_Data(Data_Source: TMem64); override;
    procedure Decode_External_Header_Data(Data_Source: TMem64); override;
  end;

  TZDB2_FS3_Body_List = TBigList<TZDB2_FS3_Body>;
  TZDB2_FS3_Body_Array = TZDB2_FS3_Body_List.TArray_T_;

  TZDB2_FS3_Body_Th_Engine_Marshal = class(TLite_Th_Engine_Marshal)
  private
    FOwner_FS3: TZDB2_FS3_Lite;
  public
    property Owner_FS3: TZDB2_FS3_Lite read FOwner_FS3;
    procedure Do_Add_Data(Sender: TZDB2_Th_Engine_Data); override;
    procedure Do_Remove_Data(Sender: TZDB2_Th_Engine_Data); override;
  end;
{$ENDREGION 'Lite_Interface'}
{$REGION 'Tool_instance'}

  TZDB2_FS3_Sync_Post_Done_C = procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean; fi: TZDB2_FS3_FileInfo);
  TZDB2_FS3_Sync_Post_Done_M = procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean; fi: TZDB2_FS3_FileInfo) of object;
{$IFDEF FPC}
  TZDB2_FS3_Sync_Post_Done_P = procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean; fi: TZDB2_FS3_FileInfo) is nested;
{$ELSE FPC}
  TZDB2_FS3_Sync_Post_Done_P = reference to procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean; fi: TZDB2_FS3_FileInfo);
{$ENDIF FPC}

  TZDB2_FS3_Sync_Post_Tool = class(TCore_Object_Intermediate)
  private
    FOwner_FS3: TZDB2_FS3_Lite;
    FStream: TCore_Stream;
    FAutoFree_Stream: Boolean;
    FFile_Name: U_String;
    FTime: TDateTime;
    FLife: Double;
    FOn_Done_C: TZDB2_FS3_Sync_Post_Done_C;
    FOn_Done_M: TZDB2_FS3_Sync_Post_Done_M;
    FOn_Done_P: TZDB2_FS3_Sync_Post_Done_P;
    FLast_Compute_MD5: TMD5;
    procedure Do_Done(Successed: Boolean; fi: TZDB2_FS3_FileInfo);
    procedure Do_Execute_Post_Stream();
  public
    constructor Create(Owner_FS3_: TZDB2_FS3_Lite);
    destructor Destroy; override;
    property Owner_FS3: TZDB2_FS3_Lite read FOwner_FS3;
    procedure Prepare_Post(Stream_: TCore_Stream; AutoFree_Stream_: Boolean; FileName_: U_String; Time__: TDateTime; Life_: Double);
  end;

  { Post_queue_tool is a non working mechanism that requires begin_post->post->end_post to be completed on the same focal thread }
  { Post_queue_tool does not wait for the command queue, and when executing end_post, it immediately triggers the on_rone event }
  TZDB2_FS3_Sync_Post_Queue_Tool = class(TCore_Object_Intermediate)
  private
    FOwner_FS3: TZDB2_FS3_Lite;
    FFile_Name: U_String;
    FFile_Size: Int64;
    FFile_MD5: TMD5;
    FTime: TDateTime;
    FLife: Double;
    FError, FCompleted: Boolean;
    FBody_List: TZDB2_FS3_Body_List;
    FOn_Done_C: TZDB2_FS3_Sync_Post_Done_C;
    FOn_Done_M: TZDB2_FS3_Sync_Post_Done_M;
    FOn_Done_P: TZDB2_FS3_Sync_Post_Done_P;
    procedure Do_Done(Successed: Boolean; fi: TZDB2_FS3_FileInfo);
  public
    constructor Create(Owner_FS3_: TZDB2_FS3_Lite);
    destructor Destroy; override;
    property Owner_FS3: TZDB2_FS3_Lite read FOwner_FS3;
    property On_Done_C: TZDB2_FS3_Sync_Post_Done_C read FOn_Done_C write FOn_Done_C;
    property On_Done_M: TZDB2_FS3_Sync_Post_Done_M read FOn_Done_M write FOn_Done_M;
    property On_Done_P: TZDB2_FS3_Sync_Post_Done_P read FOn_Done_P write FOn_Done_P;
    property Completed: Boolean read FCompleted;
    property Error: Boolean read FError;
    procedure Begin_Post(FileName_: U_String; File_Size_: Int64; File_MD5_: TMD5; Time_: TDateTime; Life_: Double);
    procedure Post(Mem_: TMem64; AutoFree_Mem_: Boolean);
    procedure End_Post;
    procedure End_Post_And_Free;
  end;

  TZDB2_FS3_Sync_Get_Done_C = procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean);
  TZDB2_FS3_Sync_Get_Done_M = procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean) of object;
  TZDB2_FS3_Sync_Get_Fragment_Data_C = procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean; Fragment: TMS64; Data_Pos: Int64);
  TZDB2_FS3_Sync_Get_Fragment_Data_M = procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean; Fragment: TMS64; Data_Pos: Int64) of object;
{$IFDEF FPC}
  TZDB2_FS3_Sync_Get_Done_P = procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean) is nested;
  TZDB2_FS3_Sync_Get_Fragment_Data_P = procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean; Fragment: TMS64; Data_Pos: Int64) is nested;
{$ELSE FPC}
  TZDB2_FS3_Sync_Get_Done_P = reference to procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean);
  TZDB2_FS3_Sync_Get_Fragment_Data_P = reference to procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean; Fragment: TMS64; Data_Pos: Int64);
{$ENDIF FPC}
  TZDB2_FS3_Sync_Get_Tool = class;

  TZDB2_FS3_Sync_Get_Tool = class(TCore_Object_Intermediate)
  private
    FOwner_FS3: TZDB2_FS3_Lite;
    FFileInfo_Sequence_ID: Int64;
    FBegin_Pos, FEnd_Pos: Int64;
    FOn_Fragment_Data_C: TZDB2_FS3_Sync_Get_Fragment_Data_C;
    FOn_Fragment_Data_M: TZDB2_FS3_Sync_Get_Fragment_Data_M;
    FOn_Fragment_Data_P: TZDB2_FS3_Sync_Get_Fragment_Data_P;
    FOn_Done_C: TZDB2_FS3_Sync_Get_Done_C;
    FOn_Done_M: TZDB2_FS3_Sync_Get_Done_M;
    FOn_Done_P: TZDB2_FS3_Sync_Get_Done_P;
    procedure Do_Done(Successed: Boolean);
    procedure Do_Fragment_Data(Successed: Boolean; Fragment: TMS64; Data_Pos: Int64);
    procedure Do_Execute_Get();
  public
    constructor Create(Owner_FS3_: TZDB2_FS3_Lite);
    destructor Destroy; override;
    property Owner_FS3: TZDB2_FS3_Lite read FOwner_FS3;
    class function Compute_Body_Range(Body_Arry: TZDB2_FS3_Body_Array; beginPos, endPos: Int64; var Output_Body_Range: TZDB2_FS3_Body_Array; var Begin_Body_Pos, End_Body_Pos: Int64): Boolean;
    procedure Prepare_Get(FileInfo_Sequence_ID_: Int64; Begin_Pos_, End_Pos_: Int64);
  end;

  TZDB2_FS3_Remove_Done_C = procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean; Sequence_ID__: Int64);
  TZDB2_FS3_Remove_Done_M = procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean; Sequence_ID__: Int64) of object;
{$IFDEF FPC}
  TZDB2_FS3_Remove_Done_P = procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean; Sequence_ID__: Int64) is nested;
{$ELSE FPC}
  TZDB2_FS3_Remove_Done_P = reference to procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean; Sequence_ID__: Int64);
{$ENDIF FPC}

  { Remove_tool is the standard threading model }
  TZDB2_FS3_Remove_Tool = class(TCore_Object_Intermediate)
  private
    FOwner_FS3: TZDB2_FS3_Lite;
    FArry: TZDB2_Lite_Sequence_ID_Array;
    FOn_Done_C: TZDB2_FS3_Remove_Done_C;
    FOn_Done_M: TZDB2_FS3_Remove_Done_M;
    FOn_Done_P: TZDB2_FS3_Remove_Done_P;
    procedure Do_Done(Successed: Boolean; Sequence_ID__: Int64);
    procedure Do_Execute_Remove();
  public
    constructor Create(Owner_FS3_: TZDB2_FS3_Lite);
    destructor Destroy; override;
    property Owner_FS3: TZDB2_FS3_Lite read FOwner_FS3;
    procedure Prepare_Remove(const arry_: TZDB2_Lite_Sequence_ID_Array);
  end;

{$ENDREGION 'Tool_instance'}

  TZDB2_FS3_Lite = class(TCore_Object_Intermediate)
  private
    FDebug_Mode: Boolean;
    FRoot_Path: U_String;
    FFileInfo: TZDB2_Lite;
    FLinkTable: TZDB2_Lite;
    FBody: TZDB2_Lite;
    FFileInfo_Pool: TZDB2_FS3_FileInfo_Pair_Pool;
    FLink_Table_MD5_Pool: TZDB2_FS3_Link_Table_MD5_Pair_Pool;
    FBody_MD5_Pool: TZDB2_FS3_Body_MD5_Pair_Pool;
    FLife_Pool: TZDB2_FS3_Life_Pool;
    FBody_Fragment_Size: Int64; { default 1024k }
    procedure Do_Remove_From_FileName(FileName_: U_String);
  public
    constructor Create(Root_Path_: U_String);
    destructor Destroy; override;
    procedure Build_Script_And_Open(PrefixName_: U_String);

    { post file-stream }
    procedure Sync_Post_Data(Stream_: TCore_Stream; AutoFree_Stream_: Boolean; FileName_: U_String; Time__: TDateTime; Life_: Double);
    procedure Sync_Post_Data_C(Stream_: TCore_Stream; AutoFree_Stream_: Boolean; FileName_: U_String; Time__: TDateTime; Life_: Double; OnResult: TZDB2_FS3_Sync_Post_Done_C);
    procedure Sync_Post_Data_M(Stream_: TCore_Stream; AutoFree_Stream_: Boolean; FileName_: U_String; Time__: TDateTime; Life_: Double; OnResult: TZDB2_FS3_Sync_Post_Done_M);
    procedure Sync_Post_Data_P(Stream_: TCore_Stream; AutoFree_Stream_: Boolean; FileName_: U_String; Time__: TDateTime; Life_: Double; OnResult: TZDB2_FS3_Sync_Post_Done_P);
    { post queue tool }
    function Create_Sync_Post_Queue: TZDB2_FS3_Sync_Post_Queue_Tool;
    { create file-info from search link-table md5 }
    function Create_FI_From_LT_MD5(FileName_: U_String; Time__: TDateTime; Life_: Double; MD5_: TMD5): Boolean;

    { get file-stream }
    procedure Sync_Get_Data_C(FileInfo_Sequence_ID_: Int64; Begin_Pos_, End_Pos_: Int64; OnData: TZDB2_FS3_Sync_Get_Fragment_Data_C; OnResult: TZDB2_FS3_Sync_Get_Done_C);
    procedure Sync_Get_Data_M(FileInfo_Sequence_ID_: Int64; Begin_Pos_, End_Pos_: Int64; OnData: TZDB2_FS3_Sync_Get_Fragment_Data_M; OnResult: TZDB2_FS3_Sync_Get_Done_M);
    procedure Sync_Get_Data_P(FileInfo_Sequence_ID_: Int64; Begin_Pos_, End_Pos_: Int64; OnData: TZDB2_FS3_Sync_Get_Fragment_Data_P; OnResult: TZDB2_FS3_Sync_Get_Done_P);

    { remove }
    procedure Remove(const arry_: TZDB2_Lite_Sequence_ID_Array);
    procedure Remove_C(const arry_: TZDB2_Lite_Sequence_ID_Array; OnResult: TZDB2_FS3_Remove_Done_C);
    procedure Remove_M(const arry_: TZDB2_Lite_Sequence_ID_Array; OnResult: TZDB2_FS3_Remove_Done_M);
    procedure Remove_P(const arry_: TZDB2_Lite_Sequence_ID_Array; OnResult: TZDB2_FS3_Remove_Done_P);

    { parameter }
    property Debug_Mode: Boolean read FDebug_Mode write FDebug_Mode;
    property Root_Path: U_String read FRoot_Path;
    property FileInfo: TZDB2_Lite read FFileInfo;
    property LinkTable: TZDB2_Lite read FLinkTable;
    property Body: TZDB2_Lite read FBody;
    property Life_Pool: TZDB2_FS3_Life_Pool read FLife_Pool;
    property FileInfo_Pool: TZDB2_FS3_FileInfo_Pair_Pool read FFileInfo_Pool;
    property Link_Table_MD5_Pool: TZDB2_FS3_Link_Table_MD5_Pair_Pool read FLink_Table_MD5_Pool;
    property Body_MD5_Pool: TZDB2_FS3_Body_MD5_Pair_Pool read FBody_MD5_Pool;
    property Body_Fragment_Size: Int64 read FBody_Fragment_Size write FBody_Fragment_Size; { default 1024k }

    { api }
    procedure Check_Life(deltaTime: Double);
    procedure Check_Recycle_Pool;
    function Progress: Integer;
    procedure SetBackupDirectory(Directory_: U_String);
    procedure Backup(Reserve_: Word);
    procedure Backup_If_No_Exists();
    procedure Flush;
    function Flush_Is_Busy: Boolean;
    function Database_Size: Int64;
    function Database_Physics_Size: Int64;
    function Total: NativeInt;
    function QueueNum: NativeInt;
    function Fragment_Buffer_Num: Int64;
    function Fragment_Buffer_Memory: Int64;

    { test }
    class procedure Test();

  private type
    TTest_Inst = class
      source: TMS64;
      md5: TMD5;
      Error: Boolean;
      Sequence_ID: Integer;
      constructor Create;
      destructor Destroy; override;
      procedure Do_Post_Done(Sender: TZDB2_FS3_Lite; Successed: Boolean; fi: TZDB2_FS3_FileInfo);
      procedure Do_Get_Fragment_Data(Sender: TZDB2_FS3_Lite; Successed: Boolean; Fragment: TMS64; Data_Pos: Int64);
      procedure Do_Get_Done(Sender: TZDB2_FS3_Lite; Successed: Boolean);
    end;

    TTest_Inst_Pool = TObject_BigList<TTest_Inst>;
  end;

implementation

procedure TZDB2_FS3_FileInfo_Pair_Pool.DoFree(var Key: TPascalString; var Value: TZDB2_FS3_FileInfo);
begin
  if Value <> nil then
      Value.FLife_Pool_Ptr := nil;
  inherited DoFree(Key, Value);
end;

constructor TZDB2_FS3_FileInfo.Create;
begin
  inherited Create;
  FOwner_FS3 := nil;
  FLife_Pool_Ptr := nil;
  Relate_Link_Table := -1;
  Life := 0;
  File_Name := '';
  FileSize := 0;
  Time_ := 0;
end;

destructor TZDB2_FS3_FileInfo.Destroy;
begin
  File_Name := '';
  inherited Destroy;
end;

procedure TZDB2_FS3_FileInfo.Do_Remove;
begin
  if (Owner_FS3 <> nil) then
    begin
      if (First_Operation_Ready) then
          Owner_FS3.FileInfo_Pool.Delete(File_Name);
      if FLife_Pool_Ptr <> nil then
          Owner_FS3.Life_Pool.Remove_P(FLife_Pool_Ptr);
    end;
  inherited Do_Remove;
end;

function TZDB2_FS3_FileInfo.Make_Data_Source: TMS64;
begin
  Result := TMS64.Create;
  Result.WriteInt64(Relate_Link_Table);
  Result.WriteDouble(Life);
  Result.WriteString(File_Name);
  Result.WriteInt64(FileSize);
  Result.WriteDouble(Time_);
  Owner_FS3.FileInfo_Pool.Add(File_Name, self, True);
end;

function TZDB2_FS3_FileInfo.Decode_From_ZDB2_Data(Data_Source: TMem64; Update_: Boolean): TMS64;
begin
  Result := inherited Decode_From_ZDB2_Data(Data_Source, Update_);
  if Update_ then
    begin
      Relate_Link_Table := Result.ReadInt64;
      Life := Result.ReadDouble;
      File_Name := Result.ReadString;
      FileSize := Result.ReadInt64;
      Time_ := Result.ReadDouble;
      Owner_FS3.FileInfo_Pool.Add(File_Name, self, True);
    end;
end;

procedure TZDB2_FS3_FileInfo.Encode_External_Header_Data(Data_Source: TMem64);
begin
  inherited Encode_External_Header_Data(Data_Source);
  Data_Source.WriteInt64(Relate_Link_Table);
  Data_Source.WriteDouble(Life);
  Data_Source.WriteString(File_Name);
  Data_Source.WriteInt64(FileSize);
  Data_Source.WriteDouble(Time_);
end;

procedure TZDB2_FS3_FileInfo.Decode_External_Header_Data(Data_Source: TMem64);
begin
  inherited Decode_External_Header_Data(Data_Source);
  Relate_Link_Table := Data_Source.ReadInt64;
  Life := Data_Source.ReadDouble;
  File_Name := Data_Source.ReadString;
  FileSize := Data_Source.ReadInt64;
  Time_ := Data_Source.ReadDouble;
  Owner_FS3.FileInfo_Pool.Add(File_Name, self, True);
end;

procedure TZDB2_FS3_FileInfo_Th_Engine_Marshal.Do_Add_Data(Sender: TZDB2_Th_Engine_Data);
begin
  inherited Do_Add_Data(Sender);
  TZDB2_FS3_FileInfo(Sender).FOwner_FS3 := FOwner_FS3;
end;

procedure TZDB2_FS3_FileInfo_Th_Engine_Marshal.Do_Remove_Data(Sender: TZDB2_Th_Engine_Data);
begin
  inherited Do_Remove_Data(Sender);
end;

constructor TZDB2_FS3_Link_Table.Create;
begin
  inherited Create;
  FOwner_FS3 := nil;
  Reference_Count := 0;
  File_MD5 := NULL_MD5;
  FileSize := 0;
  Create_Time := 0;
  Relate_Body := TRelate_Info_List.Create;
end;

destructor TZDB2_FS3_Link_Table.Destroy;
begin
  DisposeObject(Relate_Body);
  inherited Destroy;
end;

procedure TZDB2_FS3_Link_Table.Do_Remove;
begin
  if (Owner_FS3 <> nil) and (First_Operation_Ready) then
      Owner_FS3.Link_Table_MD5_Pool.Delete(File_MD5);
  inherited Do_Remove;
end;

function TZDB2_FS3_Link_Table.Make_Data_Source: TMS64;
begin
  Result := TMS64.Create;
  Result.WriteInt32(Reference_Count);
  Result.WriteMD5(File_MD5);
  Result.WriteInt64(FileSize);
  Result.WriteDouble(Create_Time);

  Result.WriteInt64(Relate_Body.Num);
  if Relate_Body.Num > 0 then
    with Relate_Body.Repeat_ do
      repeat
          Result.WriteInt64(Queue^.Data);
      until not Next;

  Owner_FS3.Link_Table_MD5_Pool.Add(File_MD5, self, True);
end;

procedure TZDB2_FS3_Link_Table.Inc_Reference_Count;
var
  m64: TMem64;
begin
  inc(Reference_Count);
  if (not First_Operation_Ready) or (DataSize < 12) or (Engine = nil) then
      exit;
  m64 := TMem64.Create;
  m64.WriteInt32(Reference_Count);
  Engine.Async_Modify_Block(ID, 0, 8, m64, True);
end;

procedure TZDB2_FS3_Link_Table.Dec_Reference_Count;
var
  m64: TMem64;
begin
  Dec(Reference_Count);
  if (not First_Operation_Ready) or (DataSize < 12) or (Engine = nil) then
      exit;
  m64 := TMem64.Create;
  m64.WriteInt32(Reference_Count);
  Engine.Async_Modify_Block(ID, 0, 8, m64, True);
end;

function TZDB2_FS3_Link_Table.Decode_From_ZDB2_Data(Data_Source: TMem64; Update_: Boolean): TMS64;
var
  n: Int64;
begin
  Result := inherited Decode_From_ZDB2_Data(Data_Source, Update_);
  if Update_ then
    begin
      Reference_Count := Result.ReadInt32;
      File_MD5 := Result.ReadMD5;
      FileSize := Result.ReadInt64;
      Create_Time := Result.ReadDouble;

      Relate_Body.Clear;
      n := Result.ReadInt64;
      while Relate_Body.Num < n do
          Relate_Body.Add(Result.ReadInt64);

      Owner_FS3.Link_Table_MD5_Pool.Add(File_MD5, self, True);
    end;
end;

procedure TZDB2_FS3_Link_Table.Encode_External_Header_Data(Data_Source: TMem64);
begin
  inherited Encode_External_Header_Data(Data_Source);

  Data_Source.WriteInt32(Reference_Count);
  Data_Source.WriteMD5(File_MD5);
  Data_Source.WriteInt64(FileSize);
  Data_Source.WriteDouble(Create_Time);

  Data_Source.WriteInt64(Relate_Body.Num);
  if Relate_Body.Num > 0 then
    with Relate_Body.Repeat_ do
      repeat
          Data_Source.WriteInt64(Queue^.Data);
      until not Next;
end;

procedure TZDB2_FS3_Link_Table.Decode_External_Header_Data(Data_Source: TMem64);
var
  n: Int64;
begin
  inherited Decode_External_Header_Data(Data_Source);

  Reference_Count := Data_Source.ReadInt32;
  File_MD5 := Data_Source.ReadMD5;
  FileSize := Data_Source.ReadInt64;
  Create_Time := Data_Source.ReadDouble;

  Relate_Body.Clear;
  n := Data_Source.ReadInt64;
  while Relate_Body.Num < n do
      Relate_Body.Add(Data_Source.ReadInt64);

  Owner_FS3.Link_Table_MD5_Pool.Add(File_MD5, self, True);
end;

procedure TZDB2_FS3_Link_Table_Th_Engine_Marshal.Do_Add_Data(Sender: TZDB2_Th_Engine_Data);
begin
  inherited Do_Add_Data(Sender);
  TZDB2_FS3_Link_Table(Sender).FOwner_FS3 := FOwner_FS3;
end;

procedure TZDB2_FS3_Link_Table_Th_Engine_Marshal.Do_Remove_Data(Sender: TZDB2_Th_Engine_Data);
begin
  inherited Do_Remove_Data(Sender);
end;

constructor TZDB2_FS3_Body.Create;
begin
  inherited Create;
  FOwner_FS3 := nil;
  Reference_Count := 0;
  Body_MD5 := NULL_MD5;
  Body_Size := 0;
  Create_Time := 0;
end;

destructor TZDB2_FS3_Body.Destroy;
begin
  inherited Destroy;
end;

procedure TZDB2_FS3_Body.Do_Remove;
begin
  if (Owner_FS3 <> nil) and (First_Operation_Ready) then
      Owner_FS3.Body_MD5_Pool.Delete(Body_MD5);
  inherited Do_Remove;
end;

procedure TZDB2_FS3_Body.Inc_Reference_Count;
var
  m64: TMem64;
begin
  inc(Reference_Count);
  if (not First_Operation_Ready) or (DataSize < 12) or (Engine = nil) then
      exit;
  m64 := TMem64.Create;
  m64.WriteInt32(Reference_Count);
  Engine.Async_Modify_Block(ID, 0, 8, m64, True);
end;

procedure TZDB2_FS3_Body.Dec_Reference_Count;
var
  m64: TMem64;
begin
  Dec(Reference_Count);
  if (not First_Operation_Ready) or (DataSize < 12) or (Engine = nil) then
      exit;
  m64 := TMem64.Create;
  m64.WriteInt32(Reference_Count);
  Engine.Async_Modify_Block(ID, 0, 8, m64, True);
end;

function TZDB2_FS3_Body.Make_Data_Source(Body: TCore_Stream; AutoFree_Body: Boolean): TMS64;
begin
  Result := TMS64.Create;
  Result.Size := 4 + 16 + 8 + Body.Size;

  Result.WriteInt32(Reference_Count);
  Result.WriteMD5(Body_MD5);
  Result.WriteInt64(Body_Size);
  Result.WriteDouble(Create_Time);

  Body.Position := 0;
  Result.CopyFrom(Body, Body.Size);
  if AutoFree_Body then
      DisposeObject(Body);

  Owner_FS3.Body_MD5_Pool.Add(Body_MD5, self, True);
end;

function TZDB2_FS3_Body.Decode_From_ZDB2_Data(Data_Source: TMem64; Update_: Boolean): TMS64;
begin
  Result := inherited Decode_From_ZDB2_Data(Data_Source, Update_);
  Reference_Count := Result.ReadInt32;
  Body_MD5 := Result.ReadMD5;
  Body_Size := Result.ReadInt64;
  Create_Time := Result.ReadDouble;
  Owner_FS3.Body_MD5_Pool.Add(Body_MD5, self, True);
end;

procedure TZDB2_FS3_Body.Encode_External_Header_Data(Data_Source: TMem64);
begin
  inherited Encode_External_Header_Data(Data_Source);
  Data_Source.WriteInt32(Reference_Count);
  Data_Source.WriteMD5(Body_MD5);
  Data_Source.WriteInt64(Body_Size);
  Data_Source.WriteDouble(Create_Time);
end;

procedure TZDB2_FS3_Body.Decode_External_Header_Data(Data_Source: TMem64);
begin
  inherited Decode_External_Header_Data(Data_Source);
  Reference_Count := Data_Source.ReadInt32;
  Body_MD5 := Data_Source.ReadMD5;
  Body_Size := Data_Source.ReadInt64;
  Create_Time := Data_Source.ReadDouble;
  Owner_FS3.Body_MD5_Pool.Add(Body_MD5, self, True);
end;

procedure TZDB2_FS3_Body_Th_Engine_Marshal.Do_Add_Data(Sender: TZDB2_Th_Engine_Data);
begin
  inherited Do_Add_Data(Sender);
  TZDB2_FS3_Body(Sender).FOwner_FS3 := FOwner_FS3;
end;

procedure TZDB2_FS3_Body_Th_Engine_Marshal.Do_Remove_Data(Sender: TZDB2_Th_Engine_Data);
begin
  inherited Do_Remove_Data(Sender);
end;

procedure TZDB2_FS3_Sync_Post_Tool.Do_Done(Successed: Boolean; fi: TZDB2_FS3_FileInfo);
begin
  if Owner_FS3.Debug_Mode then
      DoStatus('post-tool "%s" md5:%s %s', [FFile_Name.Text, umlMD5ToStr(FLast_Compute_MD5).Text, if_(Successed, 'successed', 'failed')]);

  if assigned(FOn_Done_C) then
      FOn_Done_C(Owner_FS3, Successed, fi)
  else if assigned(FOn_Done_M) then
      FOn_Done_M(Owner_FS3, Successed, fi)
  else if assigned(FOn_Done_P) then
      FOn_Done_P(Owner_FS3, Successed, fi);
end;

procedure TZDB2_FS3_Sync_Post_Tool.Do_Execute_Post_Stream();
var
  fi: TZDB2_FS3_FileInfo;
  qEng: TZDB2_Th_Queue;
  lt: TZDB2_FS3_Link_Table;
  Body_MD5: TMD5;
  Body: TZDB2_FS3_Body;
  m64: TMS64;
begin
  Owner_FS3.Do_Remove_From_FileName(FFile_Name);

  fi := Owner_FS3.FileInfo.Create_Lite_Data as TZDB2_FS3_FileInfo;
  if fi = nil then
    begin
      Do_Done(False, nil);
      if FAutoFree_Stream then
          DelayFreeObj(1.0, FStream);
      DelayFreeObj(1.0, self);
      exit;
    end;

  if FStream.Size = 0 then
    begin
      try
        { post file-info }
        fi.Relate_Link_Table := 0;
        fi.File_Name := FFile_Name;
        fi.FileSize := FStream.Size;
        fi.Time_ := FTime;
        fi.Life := FLife;
        fi.Async_Save_And_Free_Data(fi.Encode_To_ZDB2_Data(fi.Make_Data_Source, True));
        { check life }
        if fi.Life > 0 then
            fi.FLife_Pool_Ptr := Owner_FS3.Life_Pool.Add(fi);
      except
      end;
      { done }
      Do_Done(True, fi);
      if FAutoFree_Stream then
          DelayFreeObj(1.0, FStream);
      DelayFreeObj(1.0, self);
      exit;
    end;

  { compute md5 }
  FLast_Compute_MD5 := umlStreamMD5(FStream);

  lt := Owner_FS3.Link_Table_MD5_Pool[FLast_Compute_MD5];
  if lt <> nil then
    begin
      try
        { modify link-table and post }
        lt.Inc_Reference_Count;
        { post file-info }
        fi.Relate_Link_Table := lt.Sequence_ID;
        fi.File_Name := FFile_Name;
        fi.FileSize := lt.FileSize;
        fi.Time_ := FTime;
        fi.Life := FLife;
        fi.Async_Save_And_Free_Data(fi.Encode_To_ZDB2_Data(fi.Make_Data_Source, True));

        { check life }
        if fi.Life > 0 then
            fi.FLife_Pool_Ptr := Owner_FS3.Life_Pool.Add(fi);
      except
      end;
      { done }
      Do_Done(True, fi);
      if FAutoFree_Stream then
          DelayFreeObj(1.0, FStream);
      DelayFreeObj(1.0, self);
      exit;
    end;

  lt := Owner_FS3.LinkTable.Create_Lite_Data as TZDB2_FS3_Link_Table;
  if lt = nil then
    begin
      Do_Done(False, nil);
      if FAutoFree_Stream then
          DelayFreeObj(1.0, FStream);
      DelayFreeObj(1.0, self);
      exit;
    end;

  { post body }
  FStream.Position := 0;
  if FStream.Size > Owner_FS3.Body_Fragment_Size then
    begin
      while FStream.Position + Owner_FS3.Body_Fragment_Size < FStream.Size do
        begin
          m64 := TMS64.Create;
          m64.CopyFrom(FStream, Owner_FS3.Body_Fragment_Size);
          Body_MD5 := m64.ToMD5;

          try
            Body := Owner_FS3.Body_MD5_Pool[Body_MD5];
            if Body <> nil then
              begin
                Body.Inc_Reference_Count;
                lt.Relate_Body.Add(Body.Sequence_ID); { add body-sequence-id to link-table }
              end
            else
              begin
                Body := Owner_FS3.Body.Create_Lite_Data as TZDB2_FS3_Body;
                if Body = nil then
                  begin
                    Do_Done(False, nil);
                    if FAutoFree_Stream then
                        DelayFreeObj(1.0, FStream);
                    DelayFreeObj(1.0, self);
                    exit;
                  end;
                Body.Reference_Count := 1;
                Body.Body_MD5 := Body_MD5;
                Body.Body_Size := m64.Size;
                Body.Create_Time := umlNow;
                Body.Save_And_Free_Data(Body.Encode_To_ZDB2_Data(Body.Make_Data_Source(m64, True), True));
                lt.Relate_Body.Add(Body.Sequence_ID); { add body-sequence-id to link-table }
              end;
          except
          end;
        end;

      if FStream.Size - FStream.Position > 0 then
        begin
          m64 := TMS64.Create;
          m64.CopyFrom(FStream, FStream.Size - FStream.Position);
          Body_MD5 := m64.ToMD5;

          try
            Body := Owner_FS3.Body_MD5_Pool[Body_MD5];
            if Body <> nil then
              begin
                Body.Inc_Reference_Count;
                lt.Relate_Body.Add(Body.Sequence_ID); { add body-sequence-id to link-table }
              end
            else
              begin
                Body := Owner_FS3.Body.Create_Lite_Data as TZDB2_FS3_Body;
                if Body = nil then
                  begin
                    Do_Done(False, nil);
                    if FAutoFree_Stream then
                        DelayFreeObj(1.0, FStream);
                    DelayFreeObj(1.0, self);
                    exit;
                  end;
                Body.Reference_Count := 1;
                Body.Body_MD5 := Body_MD5;
                Body.Body_Size := m64.Size;
                Body.Create_Time := umlNow;
                Body.Save_And_Free_Data(Body.Encode_To_ZDB2_Data(Body.Make_Data_Source(m64, True), True));
                lt.Relate_Body.Add(Body.Sequence_ID); { add body-sequence-id to link-table }
              end;
          except
          end;
        end;
    end
  else
    begin
      m64 := TMS64.Create;
      m64.LoadFromStream(FStream);
      Body_MD5 := m64.ToMD5;

      try
        Body := Owner_FS3.Body_MD5_Pool[Body_MD5];
        if Body <> nil then
          begin
            Body.Inc_Reference_Count;
            lt.Relate_Body.Add(Body.Sequence_ID); { add body-sequence-id to link-table }
          end
        else
          begin
            Body := Owner_FS3.Body.Create_Lite_Data as TZDB2_FS3_Body;
            if Body = nil then
              begin
                Do_Done(False, nil);
                if FAutoFree_Stream then
                    DelayFreeObj(1.0, FStream);
                DelayFreeObj(1.0, self);
                exit;
              end;
            Body.Reference_Count := 1;
            Body.Body_MD5 := Body_MD5;
            Body.Body_Size := m64.Size;
            Body.Create_Time := umlNow;
            Body.Save_And_Free_Data(Body.Encode_To_ZDB2_Data(Body.Make_Data_Source(m64, True), True));
            lt.Relate_Body.Add(Body.Sequence_ID); { add body-sequence-id to link-table }
          end;
      except
      end;
    end;

  try
    { post link-table }
    lt.Reference_Count := 1;
    lt.File_MD5 := FLast_Compute_MD5;
    lt.FileSize := FStream.Size;
    lt.Create_Time := umlNow();
    lt.Save_And_Free_Data(lt.Encode_To_ZDB2_Data(lt.Make_Data_Source, True));

    { psot file-info }
    fi.Relate_Link_Table := lt.Sequence_ID;
    fi.File_Name := FFile_Name;
    fi.FileSize := lt.FileSize;
    fi.Time_ := FTime;
    fi.Life := FLife;
    fi.Save_And_Free_Data(fi.Encode_To_ZDB2_Data(fi.Make_Data_Source, True));

    { check life }
    if fi.Life > 0 then
        fi.FLife_Pool_Ptr := Owner_FS3.Life_Pool.Add(fi);
  except
  end;

  { done }
  Do_Done(True, fi);
  if FAutoFree_Stream then
      DelayFreeObj(1.0, FStream);
  DelayFreeObj(1.0, self);
end;

constructor TZDB2_FS3_Sync_Post_Tool.Create(Owner_FS3_: TZDB2_FS3_Lite);
begin
  inherited Create;
  FOwner_FS3 := Owner_FS3_;
  FStream := nil;
  FAutoFree_Stream := False;
  FFile_Name := '';
  FTime := 0;
  FLife := 0;
  FOn_Done_C := nil;
  FOn_Done_M := nil;
  FOn_Done_P := nil;
  FLast_Compute_MD5 := NULL_MD5;
end;

destructor TZDB2_FS3_Sync_Post_Tool.Destroy;
begin
  inherited Destroy;
end;

procedure TZDB2_FS3_Sync_Post_Tool.Prepare_Post(Stream_: TCore_Stream; AutoFree_Stream_: Boolean; FileName_: U_String; Time__: TDateTime; Life_: Double);
begin
  FStream := Stream_;
  FAutoFree_Stream := AutoFree_Stream_;
  FFile_Name := FileName_;
  FTime := Time__;
  FLife := Life_;
  try
      Do_Execute_Post_Stream;
  except
  end;
end;

procedure TZDB2_FS3_Sync_Post_Queue_Tool.Do_Done(Successed: Boolean; fi: TZDB2_FS3_FileInfo);
begin
  if Owner_FS3.Debug_Mode then
      DoStatus('post-queue-tool "%s" md5:%s %s', [FFile_Name.Text, umlMD5ToStr(FFile_MD5).Text, if_(Successed, 'successed', 'failed')]);

  if assigned(FOn_Done_C) then
      FOn_Done_C(Owner_FS3, Successed, fi)
  else if assigned(FOn_Done_M) then
      FOn_Done_M(Owner_FS3, Successed, fi)
  else if assigned(FOn_Done_P) then
      FOn_Done_P(Owner_FS3, Successed, fi);
end;

constructor TZDB2_FS3_Sync_Post_Queue_Tool.Create(Owner_FS3_: TZDB2_FS3_Lite);
begin
  inherited Create;
  FOwner_FS3 := Owner_FS3_;
  FFile_Name := '';
  FFile_Size := 0;
  FFile_MD5 := NULL_MD5;
  FTime := 0;
  FLife := 0;
  FError := False;
  FCompleted := False;
  FBody_List := TZDB2_FS3_Body_List.Create;
  FOn_Done_C := nil;
  FOn_Done_M := nil;
  FOn_Done_P := nil;
end;

destructor TZDB2_FS3_Sync_Post_Queue_Tool.Destroy;
begin
  DisposeObject(FBody_List);
  inherited Destroy;
end;

procedure TZDB2_FS3_Sync_Post_Queue_Tool.Begin_Post(FileName_: U_String; File_Size_: Int64; File_MD5_: TMD5; Time_: TDateTime; Life_: Double);
var
  fi: TZDB2_FS3_FileInfo;
  lt: TZDB2_FS3_Link_Table;
begin
  FFile_Name := FileName_;
  FFile_Size := File_Size_;
  FFile_MD5 := File_MD5_;
  FTime := Time_;
  FLife := Life_;

  if FFile_Size = 0 then
    begin
      Owner_FS3.Do_Remove_From_FileName(FFile_Name);
      try
        fi := Owner_FS3.FileInfo.Create_Lite_Data as TZDB2_FS3_FileInfo;
        { post file-info }
        fi.Relate_Link_Table := 0;
        fi.File_Name := FFile_Name;
        fi.FileSize := FFile_Size;
        fi.Time_ := FTime;
        fi.Life := FLife;
        fi.Async_Save_And_Free_Data(fi.Encode_To_ZDB2_Data(fi.Make_Data_Source, True));
        { check life }
        if fi.Life > 0 then
            fi.FLife_Pool_Ptr := Owner_FS3.Life_Pool.Add(fi);
      except
      end;

      FCompleted := True;
      { done }
      Do_Done(True, fi);
      exit;
    end;

  lt := Owner_FS3.Link_Table_MD5_Pool[FFile_MD5];
  if lt <> nil then
    begin
      Owner_FS3.Do_Remove_From_FileName(FFile_Name);
      try
        { modify link-table and post }
        lt.Inc_Reference_Count;

        fi := Owner_FS3.FileInfo.Create_Lite_Data as TZDB2_FS3_FileInfo;
        { post file-info }
        fi.Relate_Link_Table := lt.Sequence_ID;
        fi.File_Name := FFile_Name;
        fi.FileSize := lt.FileSize;
        fi.Time_ := FTime;
        fi.Life := FLife;
        fi.Async_Save_And_Free_Data(fi.Encode_To_ZDB2_Data(fi.Make_Data_Source, True));
        { check life }
        if fi.Life > 0 then
            fi.FLife_Pool_Ptr := Owner_FS3.Life_Pool.Add(fi);
      except
      end;

      FCompleted := True;
      { done }
      Do_Done(True, fi);
      exit;
    end;
end;

procedure TZDB2_FS3_Sync_Post_Queue_Tool.Post(Mem_: TMem64; AutoFree_Mem_: Boolean);
var
  Body_MD5: TMD5;
  Body: TZDB2_FS3_Body;
begin
  if FError or FCompleted then
    begin
      if AutoFree_Mem_ then
          DisposeObject(Mem_);
      exit;
    end;

  Body_MD5 := Mem_.ToMD5;

  try
    Body := Owner_FS3.Body_MD5_Pool[Body_MD5];
    if Body <> nil then
      begin
        Body.Inc_Reference_Count;
        FBody_List.Add(Body);
      end
    else
      begin
        Body := Owner_FS3.Body.Create_Lite_Data as TZDB2_FS3_Body;
        if Body <> nil then
          begin
            Body.Reference_Count := 1;
            Body.Body_MD5 := Body_MD5;
            Body.Body_Size := Mem_.Size;
            Body.Create_Time := umlNow;
            Body.Async_Save_And_Free_Data(Body.Encode_To_ZDB2_Data(Body.Make_Data_Source(Mem_.Stream64, False), True));
            FBody_List.Add(Body);
          end;
      end;
  except
  end;

  if AutoFree_Mem_ then
      DisposeObject(Mem_);
end;

procedure TZDB2_FS3_Sync_Post_Queue_Tool.End_Post;
var
  fi: TZDB2_FS3_FileInfo;
  lt: TZDB2_FS3_Link_Table;
begin
  if FCompleted then
      exit;

  if FError then
    begin
      Do_Done(False, nil);
      exit;
    end;

  fi := Owner_FS3.FileInfo.Create_Lite_Data as TZDB2_FS3_FileInfo;
  if fi = nil then
    begin
      Do_Done(False, nil);
      exit;
    end;

  if FFile_Size = 0 then
    begin
      Owner_FS3.Do_Remove_From_FileName(FFile_Name);
      try
        { post file-info }
        fi.Relate_Link_Table := 0;
        fi.File_Name := FFile_Name;
        fi.FileSize := FFile_Size;
        fi.Time_ := FTime;
        fi.Life := FLife;
        fi.Async_Save_And_Free_Data(fi.Encode_To_ZDB2_Data(fi.Make_Data_Source, True));
        { check life }
        if fi.Life > 0 then
            fi.FLife_Pool_Ptr := Owner_FS3.Life_Pool.Add(fi);
      except
      end;
      { done }
      Do_Done(True, fi);
      exit;
    end;

  lt := Owner_FS3.Link_Table_MD5_Pool[FFile_MD5];
  if lt <> nil then
    begin
      Owner_FS3.Do_Remove_From_FileName(FFile_Name);
      try
        { modify link-table and post }
        lt.Inc_Reference_Count;
        { post file-info }
        fi.Relate_Link_Table := lt.Sequence_ID;
        fi.File_Name := FFile_Name;
        fi.FileSize := lt.FileSize;
        fi.Time_ := FTime;
        fi.Life := FLife;
        fi.Async_Save_And_Free_Data(fi.Encode_To_ZDB2_Data(fi.Make_Data_Source, True));
        { check life }
        if fi.Life > 0 then
            fi.FLife_Pool_Ptr := Owner_FS3.Life_Pool.Add(fi);
      except
      end;
      { done }
      Do_Done(True, fi);
      exit;
    end;

  lt := Owner_FS3.LinkTable.Create_Lite_Data as TZDB2_FS3_Link_Table;
  if lt = nil then
    begin
      Do_Done(False, nil);
      exit;
    end;

  Owner_FS3.Do_Remove_From_FileName(FFile_Name);
  try
    { post link-table }
    lt.Reference_Count := 1;
    lt.File_MD5 := FFile_MD5;
    lt.FileSize := FFile_Size;
    lt.Create_Time := umlNow();
    if FBody_List.Num > 0 then
      with FBody_List.Repeat_ do
        repeat
            lt.Relate_Body.Add(Queue^.Data.Sequence_ID);
        until not Next;
    lt.Async_Save_And_Free_Data(lt.Encode_To_ZDB2_Data(lt.Make_Data_Source, True));

    { post file-info }
    fi.Relate_Link_Table := lt.Sequence_ID;
    fi.File_Name := FFile_Name;
    fi.FileSize := lt.FileSize;
    fi.Time_ := FTime;
    fi.Life := FLife;
    fi.Async_Save_And_Free_Data(fi.Encode_To_ZDB2_Data(fi.Make_Data_Source, True));
    { check life }
    if fi.Life > 0 then
        fi.FLife_Pool_Ptr := Owner_FS3.Life_Pool.Add(fi);
  except
  end;
  { done }
  Do_Done(True, fi);
end;

procedure TZDB2_FS3_Sync_Post_Queue_Tool.End_Post_And_Free;
begin
  End_Post();
  DelayFreeObj(1.0, self);
end;

procedure TZDB2_FS3_Sync_Get_Tool.Do_Done(Successed: Boolean);
begin
  if Owner_FS3.Debug_Mode then
      DoStatus('Get-Tool Sequence ID "%d" %s', [FFileInfo_Sequence_ID, if_(Successed, 'successed', 'failed')]);

  if assigned(FOn_Done_C) then
      FOn_Done_C(Owner_FS3, Successed)
  else if assigned(FOn_Done_M) then
      FOn_Done_M(Owner_FS3, Successed)
  else if assigned(FOn_Done_P) then
      FOn_Done_P(Owner_FS3, Successed);
end;

procedure TZDB2_FS3_Sync_Get_Tool.Do_Fragment_Data(Successed: Boolean; Fragment: TMS64; Data_Pos: Int64);
begin
  if assigned(FOn_Fragment_Data_C) then
      FOn_Fragment_Data_C(Owner_FS3, Successed, Fragment, Data_Pos)
  else if assigned(FOn_Fragment_Data_M) then
      FOn_Fragment_Data_M(Owner_FS3, Successed, Fragment, Data_Pos)
  else if assigned(FOn_Fragment_Data_P) then
      FOn_Fragment_Data_P(Owner_FS3, Successed, Fragment, Data_Pos);
end;

procedure TZDB2_FS3_Sync_Get_Tool.Do_Execute_Get;
var
  fi: TZDB2_FS3_FileInfo;
  lt: TZDB2_FS3_Link_Table;
  Body_List: TZDB2_FS3_Body_List;
  Body: TZDB2_FS3_Body;
  Body_Range: TZDB2_FS3_Body_Array;
  Begin_Body_Pos, End_Body_Pos: Int64;
  cp: Int64;

  procedure Do_Begin_Busy();
  var
    i: Integer;
  begin
    fi.Update_Instance_As_Busy;
    lt.Update_Instance_As_Busy;
    for i := 0 to Length(Body_Range) - 1 do
        Body_Range[i].Update_Instance_As_Busy;
  end;

  procedure Do_End_Busy();
  var
    i: Integer;
  begin
    fi.Update_Instance_As_Free;
    lt.Update_Instance_As_Free;
    for i := 0 to Length(Body_Range) - 1 do
        Body_Range[i].Update_Instance_As_Free;
  end;

var
  i: Integer;
  m64: TMS64;
  r: Boolean;
  successed_num, error_num: Integer;
begin
  fi := Owner_FS3.FileInfo.Lite_Sequence_Pool[FFileInfo_Sequence_ID] as TZDB2_FS3_FileInfo;
  { loss file-info }
  if fi = nil then
    begin
      Do_Done(False);
      DelayFreeObj(1.0, self);
      exit;
    end;

  { empty body }
  if (fi.FileSize <= 0) and (fi.Relate_Link_Table = 0) then
    begin
      m64 := TMS64.Create;
      Do_Fragment_Data(True, m64, 0);
      DisposeObject(m64);
      Do_Done(True);
      DelayFreeObj(1.0, self);
      exit;
    end;

  lt := Owner_FS3.LinkTable.Lite_Sequence_Pool[fi.Relate_Link_Table] as TZDB2_FS3_Link_Table;
  { loss link-table }
  if lt = nil then
    begin
      Do_Done(False);
      DelayFreeObj(1.0, self);
      exit;
    end;

  if lt.Relate_Body.Num = 0 then
    begin
      Do_Done(False);
      DelayFreeObj(1.0, self);
      exit;
    end;

  Body_List := TZDB2_FS3_Body_List.Create;

  if lt.Relate_Body.Num > 0 then
    with lt.Relate_Body.Repeat_ do
      repeat
        Body := Owner_FS3.Body.Lite_Sequence_Pool[Queue^.Data] as TZDB2_FS3_Body;
        { loss body }
        if Body = nil then
          begin
            DisposeObject(Body_List);
            Do_Done(False);
            DelayFreeObj(1.0, self);
            exit;
          end;
        Body_List.Add(Body);
      until not Next;

  if (FBegin_Pos = 0) and (FEnd_Pos <= 0) then
    begin
      FEnd_Pos := 0;
      if Body_List.Num > 0 then
        with Body_List.Repeat_ do
          repeat
              inc(FEnd_Pos, Queue^.Data.DataSize);
          until not Next;
    end;

  if Compute_Body_Range(Body_List.ToArray, FBegin_Pos, FEnd_Pos, Body_Range, Begin_Body_Pos, End_Body_Pos) then
    begin
      Do_Begin_Busy();
      successed_num := 0;
      error_num := 0;
      cp := 0;
      try
        if Length(Body_Range) > 1 then
          begin
            { get first }
            Body := Body_Range[0];
            m64 := TMS64.Create;
            r := Body.Get_Position_Data(m64, (Body.DataSize - Body.Body_Size) + Begin_Body_Pos, Body.Body_Size - Begin_Body_Pos);
            if r then
                inc(successed_num)
            else
                inc(error_num);
            Do_Fragment_Data(r, m64, cp);
            DisposeObject(m64);
            inc(cp, Body.Body_Size - Begin_Body_Pos);

            { get middle }
            for i := 1 to Length(Body_Range) - 2 do
              begin
                Body := Body_Range[i];
                m64 := TMS64.Create;
                r := Body.Get_Position_Data(m64, Body.DataSize - Body.Body_Size, Body.Body_Size);
                if r then
                    inc(successed_num)
                else
                    inc(error_num);
                Do_Fragment_Data(r, m64, cp);
                DisposeObject(m64);
                inc(cp, Body.Body_Size);
              end;

            { get last }
            Body := Body_Range[Length(Body_Range) - 1];
            m64 := TMS64.Create;
            r := Body.Get_Position_Data(m64, Body.DataSize - Body.Body_Size, End_Body_Pos);
            if r then
                inc(successed_num)
            else
                inc(error_num);
            Do_Fragment_Data(r, m64, cp);
            DisposeObject(m64);
            inc(cp, End_Body_Pos);
          end
        else if Length(Body_Range) = 1 then
          begin
            Body := Body_Range[0];
            m64 := TMS64.Create;
            r := Body.Get_Position_Data(m64, (Body.DataSize - Body.Body_Size) + Begin_Body_Pos, End_Body_Pos - Begin_Body_Pos);
            if r then
                inc(successed_num)
            else
                inc(error_num);
            Do_Fragment_Data(r, m64, cp);
            DisposeObject(m64);
            inc(cp, End_Body_Pos - Begin_Body_Pos);
          end;
      except
      end;
      Do_End_Busy();
      Do_Done(error_num = 0);
    end;
  DisposeObject(Body_List);
  DelayFreeObj(1.0, self);
end;

constructor TZDB2_FS3_Sync_Get_Tool.Create(Owner_FS3_: TZDB2_FS3_Lite);
begin
  inherited Create;
  FOwner_FS3 := Owner_FS3_;
  FFileInfo_Sequence_ID := -1;
  FBegin_Pos := 0;
  FEnd_Pos := 0;
  FOn_Fragment_Data_C := nil;
  FOn_Fragment_Data_M := nil;
  FOn_Fragment_Data_P := nil;
  FOn_Done_C := nil;
  FOn_Done_M := nil;
  FOn_Done_P := nil;
end;

destructor TZDB2_FS3_Sync_Get_Tool.Destroy;
begin
  inherited Destroy;
end;

class function TZDB2_FS3_Sync_Get_Tool.Compute_Body_Range(Body_Arry: TZDB2_FS3_Body_Array; beginPos, endPos: Int64; var Output_Body_Range: TZDB2_FS3_Body_Array; var Begin_Body_Pos, End_Body_Pos: Int64): Boolean;
var
  L, i, j, ID: Integer;
  cp: Int64; { compute pos }
  Body: TZDB2_FS3_Body;
  Output_Body_List: TZDB2_FS3_Body_List;
begin
  Result := False;
  if (beginPos < 0) or (endPos - beginPos < 0) then
      exit;
  Begin_Body_Pos := 0;
  End_Body_Pos := 0;
  cp := 0;
  L := Length(Body_Arry);
  Output_Body_List := TZDB2_FS3_Body_List.Create;

  for i := 0 to L - 1 do
    begin
      Body := Body_Arry[i];
      if (beginPos >= cp) and (beginPos <= cp + Body.Body_Size) then
        begin
          Begin_Body_Pos := beginPos - cp;
          for j := i to L - 1 do
            begin
              Body := Body_Arry[j];
              Output_Body_List.Add(Body);
              if (endPos >= cp) and (endPos <= cp + Body.Body_Size) then
                begin
                  End_Body_Pos := endPos - cp;
                  Output_Body_Range := Output_Body_List.ToArray;
                  DisposeObject(Output_Body_List);
                  Result := True;
                  exit;
                end;
              inc(cp, Body.Body_Size);
            end;
          End_Body_Pos := Body_Arry[L - 1].Body_Size;
          Output_Body_Range := Output_Body_List.ToArray;
          DisposeObject(Output_Body_List);
          Result := True;
          exit;
        end;
      inc(cp, Body.Body_Size);
    end;
  DisposeObject(Output_Body_List);
end;

procedure TZDB2_FS3_Sync_Get_Tool.Prepare_Get(FileInfo_Sequence_ID_: Int64; Begin_Pos_, End_Pos_: Int64);
begin
  FFileInfo_Sequence_ID := FileInfo_Sequence_ID_;
  FBegin_Pos := Begin_Pos_;
  FEnd_Pos := End_Pos_;
  try
      Do_Execute_Get;
  except
  end;
end;

procedure TZDB2_FS3_Remove_Tool.Do_Done(Successed: Boolean; Sequence_ID__: Int64);
begin
  if Owner_FS3.Debug_Mode then
    begin
      DoStatus('remove-tool %d %s', [Sequence_ID__, if_(Successed, 'successed', 'failed')])
    end;

  if assigned(FOn_Done_C) then
      FOn_Done_C(Owner_FS3, Successed, Sequence_ID__)
  else if assigned(FOn_Done_M) then
      FOn_Done_M(Owner_FS3, Successed, Sequence_ID__)
  else if assigned(FOn_Done_P) then
      FOn_Done_P(Owner_FS3, Successed, Sequence_ID__);
end;

procedure TZDB2_FS3_Remove_Tool.Do_Execute_Remove;
var
  Sequence_ID__: Int64;
  fi: TZDB2_FS3_FileInfo;
  lt: TZDB2_FS3_Link_Table;
  Body: TZDB2_FS3_Body;
  i: Integer;
  r: Boolean;
begin
  for Sequence_ID__ in FArry do
    begin
      fi := Owner_FS3.FileInfo.Lite_Sequence_Pool[Sequence_ID__] as TZDB2_FS3_FileInfo;
      { found file-info }
      if fi <> nil then
        begin
          try
            lt := Owner_FS3.LinkTable.Lite_Sequence_Pool[fi.Relate_Link_Table] as TZDB2_FS3_Link_Table;
            { found link-table }
            if lt <> nil then
              begin
                lt.Dec_Reference_Count;
                { link-table ref count <= 0 }
                if lt.Reference_Count <= 0 then
                  begin
                    if lt.Relate_Body.Num > 0 then
                      begin
                        with lt.Relate_Body.Repeat_ do
                          repeat
                            Body := Owner_FS3.Body.Lite_Sequence_Pool[Queue^.Data] as TZDB2_FS3_Body;
                            if Body <> nil then
                              begin
                                { check body ref count and remove }
                                Body.Dec_Reference_Count;
                                if Body.Reference_Count <= 0 then
                                  begin
                                    Owner_FS3.Body_MD5_Pool.Delete(Body.Body_MD5);
                                    Body.Remove;
                                  end;
                              end;
                          until not Next;
                      end;
                    Owner_FS3.Link_Table_MD5_Pool.Delete(lt.File_MD5);
                    lt.Remove;
                  end;
              end;
            Owner_FS3.FileInfo_Pool.Delete(fi.File_Name);
            r := fi.Remove;
          except
          end;
          Do_Done(r, Sequence_ID__);
        end
      else
        begin
          Do_Done(False, Sequence_ID__);
        end;
    end;
  DelayFreeObj(1.0, self);
end;

constructor TZDB2_FS3_Remove_Tool.Create(Owner_FS3_: TZDB2_FS3_Lite);
begin
  inherited Create;
  FOwner_FS3 := Owner_FS3_;
  SetLength(FArry, 0);
  FOn_Done_C := nil;
  FOn_Done_M := nil;
  FOn_Done_P := nil;
end;

destructor TZDB2_FS3_Remove_Tool.Destroy;
begin
  SetLength(FArry, 0);
  inherited Destroy;
end;

procedure TZDB2_FS3_Remove_Tool.Prepare_Remove(const arry_: TZDB2_Lite_Sequence_ID_Array);
var
  L, i: Integer;
begin
  L := Length(arry_);
  SetLength(FArry, L);
  for i := 0 to L - 1 do
      FArry[i] := arry_[i];

  try
      Do_Execute_Remove;
  except
  end;
end;

procedure TZDB2_FS3_Lite.Do_Remove_From_FileName(FileName_: U_String);
var
  fi: TZDB2_FS3_FileInfo;
  lt: TZDB2_FS3_Link_Table;
  Body__: TZDB2_FS3_Body;
begin
  fi := FileInfo_Pool[FileName_];
  if fi <> nil then
    begin
      try
        lt := LinkTable.Lite_Sequence_Pool[fi.Relate_Link_Table] as TZDB2_FS3_Link_Table;
        { found link-table }
        if lt <> nil then
          begin
            lt.Dec_Reference_Count;
            { link-table ref count <= 0 }
            if lt.Reference_Count <= 0 then
              begin
                if lt.Relate_Body.Num > 0 then
                  begin
                    with lt.Relate_Body.Repeat_ do
                      repeat
                        Body__ := Body.Lite_Sequence_Pool[Queue^.Data] as TZDB2_FS3_Body;
                        if Body__ <> nil then
                          begin
                            { check Body ref count and remove }
                            Body__.Dec_Reference_Count;
                            if Body__.Reference_Count <= 0 then
                              begin
                                Body_MD5_Pool.Delete(Body__.Body_MD5);
                                Body__.Remove;
                              end;
                          end;
                      until not Next;
                  end;
                Link_Table_MD5_Pool.Delete(lt.File_MD5);
                lt.Remove;
              end;
          end;
        FileInfo_Pool.Delete(fi.File_Name);
        fi.Remove;
      except
      end;
    end;
end;

constructor TZDB2_FS3_Lite.Create(Root_Path_: U_String);
begin
  inherited Create;
  FDebug_Mode := False;
  FRoot_Path := Root_Path_;
  { file-info }
  FFileInfo := TZDB2_Lite.Create(TZDB2_FS3_FileInfo, TZDB2_FS3_FileInfo_Th_Engine_Marshal);
  FFileInfo.Lite_Engine_External_Header_Optimzied_Technology := True;
  TZDB2_FS3_FileInfo_Th_Engine_Marshal(FFileInfo.Lite).FOwner_FS3 := self;
  { link-table }
  FLinkTable := TZDB2_Lite.Create(TZDB2_FS3_Link_Table, TZDB2_FS3_Link_Table_Th_Engine_Marshal);
  FLinkTable.Lite_Engine_External_Header_Optimzied_Technology := True;
  TZDB2_FS3_Link_Table_Th_Engine_Marshal(FLinkTable.Lite).FOwner_FS3 := self;
  { interface }
  FBody := TZDB2_Lite.Create(TZDB2_FS3_Body, TZDB2_FS3_Body_Th_Engine_Marshal);
  FBody.Lite_Engine_External_Header_Optimzied_Technology := True;
  TZDB2_FS3_Body_Th_Engine_Marshal(FBody.Lite).FOwner_FS3 := self;
  { pool }
  FFileInfo_Pool := TZDB2_FS3_FileInfo_Pair_Pool.Create(1024 * 1024, nil);
  FLink_Table_MD5_Pool := TZDB2_FS3_Link_Table_MD5_Pair_Pool.Create(1024 * 1024, nil);
  FBody_MD5_Pool := TZDB2_FS3_Body_MD5_Pair_Pool.Create(1024 * 1024, nil);
  { life pool }
  FLife_Pool := TZDB2_FS3_Life_Pool.Create;
  { param }
  FBody_Fragment_Size := 1024 * 1024;
end;

destructor TZDB2_FS3_Lite.Destroy;
begin
  DisposeObject(FFileInfo);
  DisposeObject(FLinkTable);
  DisposeObject(FBody);
  DisposeObject(FFileInfo_Pool);
  DisposeObject(FLink_Table_MD5_Pool);
  DisposeObject(FBody_MD5_Pool);
  DisposeObject(FLife_Pool);
  inherited Destroy;
end;

procedure TZDB2_FS3_Lite.Build_Script_And_Open(PrefixName_: U_String);
var
  te: TTextDataEngine;
  fn: U_String;
begin
  umlCreateDirectory(FRoot_Path);

  { file-info }
  fn := umlCombineFileName(FRoot_Path, PrefixName_ + '_FileInfo' + '.conf');
  if not umlFileExists(fn) then
    begin
      te := FFileInfo.Make_Script(PrefixName_ + '_FileInfo', 1, 10 * 1024 * 1024, 10 * 1024 * 1024, 100, TCipherSecurity.csNone);
      te.SaveToFile(fn);
      DisposeObject(te);
    end;
  FFileInfo.Open_DB(fn);

  { link-table }
  fn := umlCombineFileName(FRoot_Path, PrefixName_ + '_LinkTable' + '.conf');
  if not umlFileExists(fn) then
    begin
      te := FFileInfo.Make_Script(PrefixName_ + '_LinkTable', 1, 10 * 1024 * 1024, 10 * 1024 * 1024, 100, TCipherSecurity.csNone);
      te.SaveToFile(fn);
      DisposeObject(te);
    end;
  FLinkTable.Open_DB(fn);

  { body }
  fn := umlCombineFileName(FRoot_Path, PrefixName_ + '_Body' + '.conf');
  if not umlFileExists(fn) then
    begin
      te := FFileInfo.Make_Script(PrefixName_ + '_Body', 2, 1024 * 1024 * 1024, 1024 * 1024 * 1024, 16 * 1024, TCipherSecurity.csNone);
      te.SaveToFile(fn);
      DisposeObject(te);
    end;
  FBody.Open_DB(fn);

  { extract }
  FFileInfo.Extract_Lite_Full(4);
  FLinkTable.Extract_Lite_Full(4);
  FBody.Extract_Lite_Full(4);

  { process life }
  if FFileInfo.Lite.Data_Marshal.Num > 0 then
    with FFileInfo.Lite.Data_Marshal.Repeat_ do
      repeat
        if TZDB2_FS3_FileInfo(Queue^.Data).Life > 0 then
          begin
            TZDB2_FS3_FileInfo(Queue^.Data).FLife_Pool_Ptr := Life_Pool.Add(TZDB2_FS3_FileInfo(Queue^.Data));
          end;
      until not Next;
end;

procedure TZDB2_FS3_Lite.Sync_Post_Data(Stream_: TCore_Stream; AutoFree_Stream_: Boolean; FileName_: U_String; Time__: TDateTime; Life_: Double);
var
  inst: TZDB2_FS3_Sync_Post_Tool;
begin
  inst := TZDB2_FS3_Sync_Post_Tool.Create(self);
  inst.Prepare_Post(Stream_, AutoFree_Stream_, FileName_, Time__, Life_);
end;

procedure TZDB2_FS3_Lite.Sync_Post_Data_C(Stream_: TCore_Stream; AutoFree_Stream_: Boolean; FileName_: U_String; Time__: TDateTime; Life_: Double; OnResult: TZDB2_FS3_Sync_Post_Done_C);
var
  inst: TZDB2_FS3_Sync_Post_Tool;
begin
  inst := TZDB2_FS3_Sync_Post_Tool.Create(self);
  inst.FOn_Done_C := OnResult;
  inst.Prepare_Post(Stream_, AutoFree_Stream_, FileName_, Time__, Life_);
end;

procedure TZDB2_FS3_Lite.Sync_Post_Data_M(Stream_: TCore_Stream; AutoFree_Stream_: Boolean; FileName_: U_String; Time__: TDateTime; Life_: Double; OnResult: TZDB2_FS3_Sync_Post_Done_M);
var
  inst: TZDB2_FS3_Sync_Post_Tool;
begin
  inst := TZDB2_FS3_Sync_Post_Tool.Create(self);
  inst.FOn_Done_M := OnResult;
  inst.Prepare_Post(Stream_, AutoFree_Stream_, FileName_, Time__, Life_);
end;

procedure TZDB2_FS3_Lite.Sync_Post_Data_P(Stream_: TCore_Stream; AutoFree_Stream_: Boolean; FileName_: U_String; Time__: TDateTime; Life_: Double; OnResult: TZDB2_FS3_Sync_Post_Done_P);
var
  inst: TZDB2_FS3_Sync_Post_Tool;
begin
  inst := TZDB2_FS3_Sync_Post_Tool.Create(self);
  inst.FOn_Done_P := OnResult;
  inst.Prepare_Post(Stream_, AutoFree_Stream_, FileName_, Time__, Life_);
end;

function TZDB2_FS3_Lite.Create_Sync_Post_Queue: TZDB2_FS3_Sync_Post_Queue_Tool;
begin
  Result := TZDB2_FS3_Sync_Post_Queue_Tool.Create(self);
end;

function TZDB2_FS3_Lite.Create_FI_From_LT_MD5(FileName_: U_String; Time__: TDateTime; Life_: Double; MD5_: TMD5): Boolean;
var
  fi: TZDB2_FS3_FileInfo;
  lt: TZDB2_FS3_Link_Table;
begin
  Result := False;
  lt := Link_Table_MD5_Pool[MD5_];
  if lt <> nil then
    begin
      try
        Do_Remove_From_FileName(FileName_);
        { modify link-table and post }
        lt.Inc_Reference_Count;

        fi := FileInfo.Create_Lite_Data as TZDB2_FS3_FileInfo;
        { post file-info }
        fi.Relate_Link_Table := lt.Sequence_ID;
        fi.File_Name := FileName_;
        fi.FileSize := lt.FileSize;
        fi.Time_ := Time__;
        fi.Life := Life_;
        fi.Async_Save_And_Free_Data(fi.Encode_To_ZDB2_Data(fi.Make_Data_Source, True));
        { check life }
        if fi.Life > 0 then
            fi.FLife_Pool_Ptr := Life_Pool.Add(fi);
        Result := True;
      except
      end;
    end;
end;

procedure TZDB2_FS3_Lite.Sync_Get_Data_C(FileInfo_Sequence_ID_, Begin_Pos_, End_Pos_: Int64; OnData: TZDB2_FS3_Sync_Get_Fragment_Data_C; OnResult: TZDB2_FS3_Sync_Get_Done_C);
var
  inst: TZDB2_FS3_Sync_Get_Tool;
begin
  inst := TZDB2_FS3_Sync_Get_Tool.Create(self);
  inst.FOn_Fragment_Data_C := OnData;
  inst.FOn_Done_C := OnResult;
  inst.Prepare_Get(FileInfo_Sequence_ID_, Begin_Pos_, End_Pos_);
end;

procedure TZDB2_FS3_Lite.Sync_Get_Data_M(FileInfo_Sequence_ID_, Begin_Pos_, End_Pos_: Int64; OnData: TZDB2_FS3_Sync_Get_Fragment_Data_M; OnResult: TZDB2_FS3_Sync_Get_Done_M);
var
  inst: TZDB2_FS3_Sync_Get_Tool;
begin
  inst := TZDB2_FS3_Sync_Get_Tool.Create(self);
  inst.FOn_Fragment_Data_M := OnData;
  inst.FOn_Done_M := OnResult;
  inst.Prepare_Get(FileInfo_Sequence_ID_, Begin_Pos_, End_Pos_);
end;

procedure TZDB2_FS3_Lite.Sync_Get_Data_P(FileInfo_Sequence_ID_, Begin_Pos_, End_Pos_: Int64; OnData: TZDB2_FS3_Sync_Get_Fragment_Data_P; OnResult: TZDB2_FS3_Sync_Get_Done_P);
var
  inst: TZDB2_FS3_Sync_Get_Tool;
begin
  inst := TZDB2_FS3_Sync_Get_Tool.Create(self);
  inst.FOn_Fragment_Data_P := OnData;
  inst.FOn_Done_P := OnResult;
  inst.Prepare_Get(FileInfo_Sequence_ID_, Begin_Pos_, End_Pos_);
end;

procedure TZDB2_FS3_Lite.Remove(const arry_: TZDB2_Lite_Sequence_ID_Array);
var
  inst: TZDB2_FS3_Remove_Tool;
begin
  inst := TZDB2_FS3_Remove_Tool.Create(self);
  inst.Prepare_Remove(arry_);
end;

procedure TZDB2_FS3_Lite.Remove_C(const arry_: TZDB2_Lite_Sequence_ID_Array; OnResult: TZDB2_FS3_Remove_Done_C);
var
  inst: TZDB2_FS3_Remove_Tool;
begin
  inst := TZDB2_FS3_Remove_Tool.Create(self);
  inst.FOn_Done_C := OnResult;
  inst.Prepare_Remove(arry_);
end;

procedure TZDB2_FS3_Lite.Remove_M(const arry_: TZDB2_Lite_Sequence_ID_Array; OnResult: TZDB2_FS3_Remove_Done_M);
var
  inst: TZDB2_FS3_Remove_Tool;
begin
  inst := TZDB2_FS3_Remove_Tool.Create(self);
  inst.FOn_Done_M := OnResult;
  inst.Prepare_Remove(arry_);
end;

procedure TZDB2_FS3_Lite.Remove_P(const arry_: TZDB2_Lite_Sequence_ID_Array; OnResult: TZDB2_FS3_Remove_Done_P);
var
  inst: TZDB2_FS3_Remove_Tool;
begin
  inst := TZDB2_FS3_Remove_Tool.Create(self);
  inst.FOn_Done_P := OnResult;
  inst.Prepare_Remove(arry_);
end;

procedure TZDB2_FS3_Lite.Check_Life(deltaTime: Double);
var
  f: Double;
begin
  Life_Pool.Lock;
  try
    if Life_Pool.Num > 0 then
      begin
        with Life_Pool.Repeat_ do
          repeat
            Queue^.Data.Life := umlMax(Queue^.Data.Life - deltaTime, 0);
            if Queue^.Data.Life <= 0 then
              begin
                Queue^.Data.FLife_Pool_Ptr := nil;
                Life_Pool.Push_To_Recycle_Pool(Queue);
                Remove([Queue^.Data.Sequence_ID]);
              end;
          until not Next;
        Life_Pool.Free_Recycle_Pool(False);
      end;
  except
  end;
  Life_Pool.UnLock;
end;

procedure TZDB2_FS3_Lite.Check_Recycle_Pool;
begin
  FFileInfo.Check_Recycle_Pool;
  FLinkTable.Check_Recycle_Pool;
  FBody.Check_Recycle_Pool;
end;

function TZDB2_FS3_Lite.Progress: Integer;
begin
  Result :=
    FFileInfo.Progress +
    FLinkTable.Progress +
    FBody.Progress;
end;

procedure TZDB2_FS3_Lite.SetBackupDirectory(Directory_: U_String);
begin
  if FFileInfo.Lite.Engine_Pool.Num > 0 then
    with FFileInfo.Lite.Engine_Pool.Repeat_ do
      repeat
          Queue^.Data.Backup_Directory := Directory_;
      until not Next;

  if FLinkTable.Lite.Engine_Pool.Num > 0 then
    with FLinkTable.Lite.Engine_Pool.Repeat_ do
      repeat
          Queue^.Data.Backup_Directory := Directory_;
      until not Next;

  if FBody.Lite.Engine_Pool.Num > 0 then
    with FBody.Lite.Engine_Pool.Repeat_ do
      repeat
          Queue^.Data.Backup_Directory := Directory_;
      until not Next;
end;

procedure TZDB2_FS3_Lite.Backup(Reserve_: Word);
begin
  FFileInfo.Backup(Reserve_);
  FLinkTable.Backup(Reserve_);
  FBody.Backup(Reserve_);
end;

procedure TZDB2_FS3_Lite.Backup_If_No_Exists;
begin
  FFileInfo.Backup_If_No_Exists;
  FLinkTable.Backup_If_No_Exists;
  FBody.Backup_If_No_Exists;
end;

procedure TZDB2_FS3_Lite.Flush;
begin
  FFileInfo.Flush(False);
  FLinkTable.Flush(False);
  FBody.Flush(False);
end;

function TZDB2_FS3_Lite.Flush_Is_Busy: Boolean;
begin
  Result :=
    FFileInfo.Flush_Is_Busy or
    FLinkTable.Flush_Is_Busy or
    FBody.Flush_Is_Busy;
end;

function TZDB2_FS3_Lite.Database_Size: Int64;
begin
  Result :=
    FFileInfo.Database_Size +
    FLinkTable.Database_Size +
    FBody.Database_Size;
end;

function TZDB2_FS3_Lite.Database_Physics_Size: Int64;
begin
  Result :=
    FFileInfo.Database_Physics_Size +
    FLinkTable.Database_Physics_Size +
    FBody.Database_Physics_Size;
end;

function TZDB2_FS3_Lite.Total: NativeInt;
begin
  Result :=
    FFileInfo.Total +
    FLinkTable.Total +
    FBody.Total;
end;

function TZDB2_FS3_Lite.QueueNum: NativeInt;
begin
  Result :=
    FFileInfo.QueueNum +
    FLinkTable.QueueNum +
    FBody.QueueNum;
end;

function TZDB2_FS3_Lite.Fragment_Buffer_Num: Int64;
begin
  Result :=
    FFileInfo.Fragment_Buffer_Num +
    FLinkTable.Fragment_Buffer_Num +
    FBody.Fragment_Buffer_Num;
end;

function TZDB2_FS3_Lite.Fragment_Buffer_Memory: Int64;
begin
  Result :=
    FFileInfo.Fragment_Buffer_Memory +
    FLinkTable.Fragment_Buffer_Memory +
    FBody.Fragment_Buffer_Memory;
end;

class procedure TZDB2_FS3_Lite.Test;
var
  inst: TZDB2_FS3_Lite;
  i: Integer;
  test_inst: TTest_Inst;
  test_list: TTest_Inst_Pool;
  queue_tool: TZDB2_FS3_Sync_Post_Queue_Tool;
  m64: TMem64;
begin
  TMT19937.Randomize;
  test_list := TTest_Inst_Pool.Create(True);
  inst := TZDB2_FS3_Lite.Create(umlCombinePath(umlCurrentPath, 'FS3_Lite'));
  inst.Build_Script_And_Open('FS3');

  for i := 1 to 20 do
    begin
      test_inst := test_list.Add(TTest_Inst.Create)^.Data;
      test_inst.source.Size := umlRR(16 * 1024, 10 * 1024 * 1024);
      TMT19937.Rand32(MaxInt, test_inst.source.Memory, test_inst.source.Size shr 2);
      test_inst.md5 := test_inst.source.ToMD5;
      if i mod 2 = 0 then
        begin
          test_inst.source.Position := 0;
          queue_tool := TZDB2_FS3_Sync_Post_Queue_Tool.Create(inst);
          queue_tool.On_Done_M := test_inst.Do_Post_Done;
          queue_tool.Begin_Post(TPascalString.RandomString(10, [c0to9, cAtoZ]) + '_tmp_queue.dat', test_inst.source.Size, test_inst.md5, umlNow(), 0);
          if not queue_tool.Completed then
            begin
              while test_inst.source.Position + (16 * 1024) < test_inst.source.Size do
                begin
                  m64 := TMem64.Create;
                  m64.Mapping(test_inst.source.PosAsPtr, 16 * 1024);
                  queue_tool.Post(m64, True);
                  test_inst.source.Position := test_inst.source.Position + 16 * 1024;
                end;
              if test_inst.source.Position < test_inst.source.Size then
                begin
                  m64 := TMem64.Create;
                  m64.Mapping(test_inst.source.PosAsPtr, test_inst.source.Size - test_inst.source.Position);
                  queue_tool.Post(m64, True);
                  test_inst.source.Position := test_inst.source.Size;
                end;
            end;
          queue_tool.End_Post;
          DisposeObject(queue_tool);
        end
      else
          inst.Sync_Post_Data_M(test_inst.source, False, 'tmp.dat', umlNow(), 0, test_inst.Do_Post_Done);
    end;

  while (inst.QueueNum > 0) do
    begin
      inst.Check_Recycle_Pool;
      TCompute.Sleep(100);
    end;

  DisposeObject(inst);
  DisposeObject(test_list);
end;

constructor TZDB2_FS3_Lite.TTest_Inst.Create;
begin
  inherited Create;
  source := TMS64.Create;
  md5 := NULL_MD5;
  Error := True;
  Sequence_ID := 0;
end;

destructor TZDB2_FS3_Lite.TTest_Inst.Destroy;
begin
  DisposeObject(source);
  inherited Destroy;
end;

procedure TZDB2_FS3_Lite.TTest_Inst.Do_Post_Done(Sender: TZDB2_FS3_Lite; Successed: Boolean; fi: TZDB2_FS3_FileInfo);
begin
  if not Successed then
      exit;
  Error := False;
  Sequence_ID := fi.Sequence_ID;
  Sender.Sync_Get_Data_M(fi.Sequence_ID, 0, 0, Do_Get_Fragment_Data, Do_Get_Done);
end;

procedure TZDB2_FS3_Lite.TTest_Inst.Do_Get_Fragment_Data(Sender: TZDB2_FS3_Lite; Successed: Boolean; Fragment: TMS64; Data_Pos: Int64);
begin
  if not Successed then
      DoStatus('fragment error.')
  else if not CompareMemory(Fragment.Memory, source.PosAsPtr(Data_Pos), Fragment.Size) then
      DoStatus('verify error.');
end;

procedure TZDB2_FS3_Lite.TTest_Inst.Do_Get_Done(Sender: TZDB2_FS3_Lite; Successed: Boolean);
begin
  if Successed then
      Sender.Remove([Sequence_ID]);
end;

end.
