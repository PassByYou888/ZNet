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
{ * ZDB 2.0 Core                                                               * }
{ ****************************************************************************** }
unit Z.ZDB2;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib,
  Z.MemoryStream,
  Z.FragmentBuffer, // solve for discontinuous space
  Z.Status, Z.Cipher;

type
  TZDB2_Core_Space = class;
  TZDB2_Mem = TMem64;
  TZDB2_UserCustomHeader = array [0 .. 253] of Byte;
  PZDB2_UserCustomHeader = ^TZDB2_UserCustomHeader;

  TZDB2_FileHeader = packed record
    Flag: Cardinal;
    Major, Minor: WORD;
    Struct_Main: Int64;
    UserCustomHeader: TZDB2_UserCustomHeader;
    Modification: Boolean;
  end;

  TZDB2_Block = record
    Position: Int64;
    Size: WORD;
    UsedSpace: WORD;
    Prev, Next: Integer;
    ID: Integer;
  end;

  PZDB2_Block = ^TZDB2_Block;

  TZDB2_BlockCache = record
    Mem: TZDB2_Mem;
    FlushThisCacheToFile: Boolean;
  end;

  TZDB2_ID_Pool = TBigList<Integer>;
  TZDB2_ID_List = TGenericsList<Integer>;
  TZDB2_BlockPtrList_Decl = TGenericsList<PZDB2_Block>;

  TZDB2_BlockPtrList = class(TZDB2_BlockPtrList_Decl)
  public
    function TotalBlockSize: Int64;
    procedure Clean;
  end;

  IZDB2_Cipher = interface
    procedure Encrypt(buff: Pointer; Size: NativeInt);
    procedure Decrypt(buff: Pointer; Size: NativeInt);
    function Get_CipherSecurity: TCipherSecurity;
  end;

  TZDB2_BlockHandle = array of Integer;
  PZDB2_BlockHandle = ^TZDB2_BlockHandle;
  TZDB2_BlockBuffer = array of TZDB2_Block;
  TZDB2_BlockWriteCache = array of TZDB2_BlockCache;
  TZDB2_OnProgress = procedure(Total_, current_: Integer) of object;
  TZDB2_OnNoSpace = procedure(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean) of object;

  TZDB2_Block_File = packed record
    Position: Int64;
    Size: WORD;
    UsedSpace: WORD;
    Prev, Next: Integer;
  end;

  TZDB2_Block_File_Buffer = array of TZDB2_Block_File;

  TZDB2_Block_File_Header = packed record
    Flag1: Cardinal;
    Count: Integer;
    MD5: TMD5;
    Flag2: Cardinal;
  end;

  TZDB2_Block_File_Tail = packed record
    Flag1: Cardinal;
    NextPosition: Int64;
    Flag2: Cardinal;
  end;

  TZDB2_Block_File_Data_Instance = class(TCore_Object_Intermediate)
  private
    Position, NextPosition: Int64;
    Count: Integer;
    Buffer: TZDB2_Block_File_Buffer;
    Last_Update_Head_MD5: TMD5;
    Last_Update_Buffer_MD5: TMD5;
    Last_Update_Encrypt_Buffer_Copy: TMem64;
    Last_Update_Tail_MD5: TMD5;
  public
    constructor Create;
    destructor Destroy; override;
    function Read(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
    function Write(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
    procedure BuildBlockBuffer(var BlockBuffer_: TZDB2_BlockBuffer);
    class function ComputeSize(BlockNum_: Integer): Int64;
  end;

  TZDB2_Block_File_Data_Instance_List_ = TGenericsList<TZDB2_Block_File_Data_Instance>;

  TZDB2_Block_File_Data_Instance_List = class(TZDB2_Block_File_Data_Instance_List_)
  public
    function BlockSum(): Integer;
    procedure ExtractToStoreBuffer(var Buffer: TZDB2_Block_File_Buffer); overload;
    procedure ExtractToBlockBuffer(var Buffer: TZDB2_BlockBuffer; StartID_: Integer); overload;
    procedure ExtractToBlockBuffer(var Buffer: TZDB2_BlockBuffer); overload;
    function FillFromBlockBuffer(var Buffer: TZDB2_BlockBuffer; StartID_: Integer): Boolean; overload;
    function FillFromBlockBuffer(var Buffer: TZDB2_BlockBuffer): Boolean; overload;
    function Read(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
    function Write(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
    procedure Clean;
    procedure Recycle_Memory;
  end;

  TZDB2_Space_Planner = class(TCore_Object_Intermediate)
  private
    FCore: TZDB2_Core_Space;
    FStruct: TZDB2_Block_File_Data_Instance_List;
    FWriteID: Integer;
  public
    constructor Create(Core_: TZDB2_Core_Space);
    destructor Destroy; override;
    function WriteStream(Stream_: TCore_Stream; BlockSize_: WORD; var SpaceHnd: TZDB2_BlockHandle): Boolean; overload;
    function WriteStream(Stream_: TCore_Stream; BlockSize_: WORD; var ID: Integer): Boolean; overload;
    function WriteFile(FileName_: SystemString; BlockSize_: WORD; var SpaceHnd: TZDB2_BlockHandle): Boolean; overload;
    function WriteFile(FileName_: SystemString; BlockSize_: WORD; var ID: Integer): Boolean; overload;
    function Flush: Boolean;
  end;

  TZDB2_CRC16 = class(TCore_Object_Intermediate)
  public
    CRC16Buffer: array of WORD;
    constructor Create;
    destructor Destroy; override;
    function Build(Core_: TZDB2_Core_Space): Boolean; overload;
    function Build(Core_: TZDB2_Core_Space; Hnd: TZDB2_BlockHandle): Boolean; overload;
    procedure LoadFromStream(stream: TCore_Stream);
    procedure SaveToStream(stream: TCore_Stream);
    procedure LoadFromFile(FileName_: SystemString);
    procedure SaveToFile(FileName_: SystemString);
  end;

  TZDB2_Cipher = class(TCore_InterfacedObject_Intermediate, IZDB2_Cipher)
  private
    FCipher_: TCipher_Base;
  public
    class function GetCipherSecurity(CipherSecurityString_: U_String): TCipherSecurity;
    constructor Create(CipherSecurity_: TCipherSecurity; password_: U_String; Level_: Integer; Tail_, CBC_: Boolean); overload;
    constructor Create(CipherSecurityString_: U_String; password_: U_String; Level_: Integer; Tail_, CBC_: Boolean); overload;
    destructor Destroy; override;
    procedure Encrypt(buff: Pointer; Size: NativeInt);
    procedure Decrypt(buff: Pointer; Size: NativeInt);
    function Get_CipherSecurity: TCipherSecurity;
    class procedure Test;
  end;

  { stmBigData: DB Size > 100G, < 130TB, block number < 1000*10000, no cache }
  { stmNormal: DB size > 1G, < 100G, block number < 500*10000, open write cache }
  { stmFast: DB size > 100M, < 10G, block number < 100*10000, open read/write cache }
  TZDB2_SpaceMode = (smBigData, smNormal, smFast);

  TZDB2_SpaceState = record
    Physics: Int64;
    FreeSpace: Int64;
    Cache: Int64;
    ReadNum: Int64;
    ReadSize: Int64;
    WriteNum: Int64;
    WriteSize: Int64;
    procedure Reset;
  end;

  TZDB2_Atom_SpaceState = class(TAtomVar<TZDB2_SpaceState>)
  public
    constructor Create;
  end;

  PZDB2_Core_SpaceState = ^TZDB2_SpaceState;

  TZDB2_Core_Space_Info_Decl = TCritical_BigList<SystemString>;

  TZDB2_Core_Space_Info = class(TZDB2_Core_Space_Info_Decl)
  public
    procedure DoFree(var Data: SystemString); override;
  end;

  TZDB2_Core_Space_Error_Info = class(TZDB2_Core_Space_Info);
  TZDB2_Core_Space_Warning_Info = class(TZDB2_Core_Space_Info);

  TZDB2_Core_Space = class(TCore_Object_Intermediate)
  private
    FHeader: TZDB2_FileHeader;
    FFault_Shutdown: Boolean;
    FAutoCloseIOHnd: Boolean;
    FAutoFreeIOHnd: Boolean;
    FSpace_IOHnd: PIOHnd;
    FFreeSpaceIndexProbe: Integer;
    FBlockCount: Integer;
    FBlockBuffer: TZDB2_BlockBuffer;
    FBlock_File_Data_Instance_List: TZDB2_Block_File_Data_Instance_List;
    FMaxCacheMemory: Int64;
    FUsedReadCache: Boolean;
    FUsedWriteCache: Boolean;
    FBlockWriteCache: TZDB2_BlockWriteCache;
    FMode: TZDB2_SpaceMode;
    FCipher: IZDB2_Cipher;
    FCipherMem: TMem64;
    FState: TZDB2_SpaceState;
    FLast_Modification: TTimeTick;
    FLast_Error_Info: TZDB2_Core_Space_Error_Info;
    FLast_Warning_Info: TZDB2_Core_Space_Warning_Info;
    FOnProgress: TZDB2_OnProgress;
    FOnNoSpace: TZDB2_OnNoSpace;

    function Check_ReadCache(ID: Integer): Boolean;
    function ReadCacheBlock(buff: Pointer; ID: Integer): Boolean;
    function WriteCacheBlock(buff: Pointer; siz: Integer; ID: Integer; FlushThisCache_: Boolean): Boolean;
    procedure DeleteCache(ID: Integer);
    procedure ClearCache;
    procedure FlushCache;
    function WriteHeader(): Boolean;
    procedure Do_Modification();
    function WriteTable(): Boolean;
    procedure PrepareCacheBlock();
    function GetUserCustomHeader: PZDB2_UserCustomHeader;
    procedure SetMode(const Value: TZDB2_SpaceMode);
    procedure DoDecrypt(buff: Pointer; Size: NativeInt);
    procedure DoEncrypt(buff: Pointer; Size: NativeInt);
    function DoEncryptTemp(buff: Pointer; Size: NativeInt; BuffProtected_: Boolean): Pointer;
    function GetState: PZDB2_Core_SpaceState;
  public
    class function CheckStream(stream: TCore_Stream; Cipher_: IZDB2_Cipher): Boolean; overload;
    class function CheckStream(stream: TCore_Stream; Cipher_: IZDB2_Cipher; Check_Fault_Shutdown_: Boolean): Boolean; overload;
    constructor Create(IOHnd_: PIOHnd);
    destructor Destroy; override;

    // Last_Modification=0 indicates no write IO operations, else it is the last TimeTick
    property Last_Modification: TTimeTick read FLast_Modification;
    function Is_Modification: Boolean;
    // error
    property Last_Error_Info: TZDB2_Core_Space_Error_Info read FLast_Error_Info;
    procedure ErrorInfo(const Text_: SystemString);
    // warning
    property Last_Warning_Info: TZDB2_Core_Space_Warning_Info read FLast_Warning_Info;
    procedure WarningInfo(const Text_: SystemString);
    // flush
    procedure Flush();
    procedure Save();
    // open
    function Open(): Boolean;
    property Fault_Shutdown: Boolean read FFault_Shutdown;
    // scan space state and probe seek
    procedure ScanSpace;
    // fast build space, Do not perform 0 to fill on data blocks, use physics-natural data
    function Fast_BuildSpace(PhySpaceSize: Int64; BlockSize_: WORD): Boolean;
    // fast append space, Do not perform 0 to fill on data blocks, use physics-natural data
    function Fast_AppendSpace(NewSpaceSize_: Int64; DestBlockSize_: WORD): Boolean;
    // build space block used 0 fill
    function BuildSpace(PhySpaceSize: Int64; BlockSize_: WORD): Boolean;
    // append block used 0 fill
    function AppendSpace(NewSpaceSize_: Int64; DestBlockSize_: WORD): Boolean;
    function OptimizedSpaceTo(var Dest_IOHnd: TIOHnd): Boolean;
    // data
    function Check(ID_: Integer): Boolean;
    function GetSpaceHndID(ID_: Integer): Integer;
    function GetSpaceHnd(ID_: Integer): TZDB2_BlockHandle;
    function GetSpaceHndAsText(ID_: Integer): U_String;
    function GetSpaceHndPtr(ID_: Integer): TZDB2_BlockPtrList;
    function CheckWriteSpace(Siz_: Int64): Boolean; overload;
    function CheckWriteSpace(Siz_: Int64; Space_: TZDB2_BlockPtrList): Boolean; overload;
    function GetWriteSpaceBlock(): Integer;
    // block IO
    function Block_IO_Read(buff: Pointer; ID: Integer): WORD;
    function Block_IO_Write(buff: Pointer; ID: Integer): Boolean;
    function Block_IO_Custom_Read(buff: Pointer; ID, Block_Offset, Block_Read_Size: Integer): Boolean;
    // write stream
    function WriteStream(Stream_: TCore_Stream; var SpaceHnd: TZDB2_BlockHandle): Boolean; overload;
    function WriteStream(Stream_: TCore_Stream; var ID: Integer): Boolean; overload;
    // write memory
    function WriteData(buff: TZDB2_Mem; var SpaceHnd: TZDB2_BlockHandle; BuffProtected_: Boolean): Boolean; overload;
    function WriteData(buff: TZDB2_Mem; var SpaceHnd: TZDB2_BlockHandle): Boolean; overload;
    function WriteData(buff: TZDB2_Mem; var ID: Integer; BuffProtected_: Boolean): Boolean; overload;
    function WriteData(buff: TZDB2_Mem; var ID: Integer): Boolean; overload;
    // combine wirte: optimized structural copying for large-data
    function Write_Combine_Memory(const arry: TMS64_Array; var SpaceHnd: TZDB2_BlockHandle): Boolean; overload;
    function Write_Combine_Memory(const arry: TMS64_Array; var ID: Integer): Boolean; overload;
    function Write_Combine_Memory(const arry: TMem64_Array; var SpaceHnd: TZDB2_BlockHandle): Boolean; overload;
    function Write_Combine_Memory(const arry: TMem64_Array; var ID: Integer): Boolean; overload;
    function Write_Combine_Stream(const arry: TStream_Array; var SpaceHnd: TZDB2_BlockHandle): Boolean; overload;
    function Write_Combine_Stream(const arry: TStream_Array; var ID: Integer): Boolean; overload;
    // read stream
    function ReadStream(Stream_: TCore_Stream; SpaceHnd: TZDB2_BlockHandle): Boolean; overload;
    function ReadStream(Stream_: TCore_Stream; ID: Integer): Boolean; overload;
    // read memory
    function ReadData(buff: TZDB2_Mem; SpaceHnd: TZDB2_BlockHandle): Boolean; overload;
    function ReadData(buff: TZDB2_Mem; ID: Integer): Boolean; overload;
    // read position
    function Compute_Data_Position(SpaceHnd: TZDB2_BlockHandle; const Pos_: Int64; var BlockID_: Integer; var BlockPos_: Int64): Boolean;
    function Compute_Data_Range(SpaceHnd: TZDB2_BlockHandle; const Pos_, Size_: Int64; var Range_Space: TZDB2_ID_List; var BeginBlockPos_, EndBlockPos_: Int64): Boolean;
    function Read_Position(Stream_: TCore_Stream; SpaceHnd: TZDB2_BlockHandle; Begin_Position, Read_Size: Int64): Int64; overload;
    function Read_Position(Stream_: TCore_Stream; ID: Integer; Begin_Position, Read_Size: Int64): Int64; overload;
    // misc
    function ComputeMD5(SpaceHnd: TZDB2_BlockHandle; var MD5: TMD5): Boolean; overload;
    function ComputeMD5(ID: Integer; var MD5: TMD5): Boolean; overload;
    function RemoveData(SpaceHnd: TZDB2_BlockHandle; SafeClean_: Boolean): Boolean; overload;
    function RemoveData(ID: Integer; SafeClean_: Boolean): Boolean; overload;
    function GetDataSize(SpaceHnd: TZDB2_BlockHandle): Int64; overload;
    function GetDataSize(ID: Integer): Int64; overload;
    function GetDataPhysics(SpaceHnd: TZDB2_BlockHandle): Int64; overload;
    function GetDataPhysics(ID: Integer): Int64; overload;
    function BuildTableID: TZDB2_BlockHandle;
    procedure Format_Space();
    // hnd
    property AutoCloseIOHnd: Boolean read FAutoCloseIOHnd write FAutoCloseIOHnd;
    property AutoFreeIOHnd: Boolean read FAutoFreeIOHnd write FAutoFreeIOHnd;
    property Space_IOHnd: PIOHnd read FSpace_IOHnd write FSpace_IOHnd;
    property IOHnd_Ptr: PIOHnd read FSpace_IOHnd write FSpace_IOHnd;
    // custom
    property UserCustomHeader: PZDB2_UserCustomHeader read GetUserCustomHeader;
    property UserHeader: PZDB2_UserCustomHeader read GetUserCustomHeader;
    property HeaderUser: PZDB2_UserCustomHeader read GetUserCustomHeader;
    property CustomHeader: PZDB2_UserCustomHeader read GetUserCustomHeader;
    property HeaderCustom: PZDB2_UserCustomHeader read GetUserCustomHeader;
    // opt
    property BlockCount: Integer read FBlockCount;
    property BlockBuffer: TZDB2_BlockBuffer read FBlockBuffer;
    property MaxCacheMemory: Int64 read FMaxCacheMemory write FMaxCacheMemory;
    property CacheMemory: Int64 read FMaxCacheMemory write FMaxCacheMemory;
    property UsedReadCache: Boolean read FUsedReadCache write FUsedReadCache;
    property UsedWriteCache: Boolean read FUsedWriteCache write FUsedWriteCache;
    property Mode: TZDB2_SpaceMode read FMode write SetMode;
    property Cipher: IZDB2_Cipher read FCipher write FCipher;
    property State: PZDB2_Core_SpaceState read GetState;
    procedure DoProgress(Total_, current_: Integer);
    property OnProgress: TZDB2_OnProgress read FOnProgress write FOnProgress;
    property OnNoSpace: TZDB2_OnNoSpace read FOnNoSpace write FOnNoSpace;
    // public
    class function Combine_Handle(hnd1, hnd2: TZDB2_BlockHandle): TZDB2_BlockHandle; overload; static;
    class function Combine_Handle(L1, L2: TZDB2_ID_List): TZDB2_BlockHandle; overload; static;
    class function Get_Handle(Hnd: TZDB2_BlockHandle): TZDB2_BlockHandle; overload; static;
    class function Get_Handle(L: TZDB2_ID_List): TZDB2_BlockHandle; overload; static;
    class function Get_Handle(L: TZDB2_ID_Pool): TZDB2_BlockHandle; overload; static;
    // test
    class procedure Test();
    class procedure Test_Cache();
    class procedure Test_Read_Position();
    class procedure Test_Write_Combine();
  end;

  { ZDB2 extract swap define }
function Get_New_ZDB2_Extract_FileName(F: U_String): U_String;
procedure Check_And_Replace_ZDB2_Extract_FileName(F: U_String);

implementation

function Get_New_ZDB2_Extract_FileName(F: U_String): U_String;
begin
  Result := F + '.~Extract';
  DoStatus('extract define: %s -> %s', [umlGetFileName(F).Text, umlGetFileName(Result).Text]);
end;

procedure Check_And_Replace_ZDB2_Extract_FileName(F: U_String);
var
  OLD_F, New_F: U_String;
begin
  OLD_F := F + '.~OLD';
  New_F := F + '.~Extract';
  if umlFileExists(New_F) then
    begin
      DoStatus('rename %s -> %s', [umlGetFileName(New_F).Text, umlGetFileName(F).Text]);
      umlDeleteFile(OLD_F);
      DoStatus('remove %s', [umlGetFileName(OLD_F).Text]);
      umlRenameFile(F, OLD_F);
      umlRenameFile(New_F, F);
    end;
end;

const
  C_ZDB2_FileHead = $89898989;
  C_ZDB2_MinBlockSize = $40;
  C_ZDB2_SpaceTableHead_1 = $90909090;
  C_ZDB2_SpaceTableHead_2 = $91919191;
  C_ZDB2_SpaceTableTail_1 = $92929292;
  C_ZDB2_SpaceTableTail_2 = $93939393;
  C_ZDB2_HeaderSize = SizeOf(TZDB2_FileHeader);

var
  ZDB2_NULL_Data: array [WORD] of Byte;

procedure StoreToBlock(var store: TZDB2_Block_File; var block: TZDB2_Block); forward;
procedure BlockToStore(var block: TZDB2_Block; var store: TZDB2_Block_File); forward;

procedure StoreToBlock(var store: TZDB2_Block_File; var block: TZDB2_Block);
begin
  block.Position := store.Position;
  block.Size := store.Size;
  block.UsedSpace := store.UsedSpace;
  block.Prev := store.Prev;
  block.Next := store.Next;
end;

procedure BlockToStore(var block: TZDB2_Block; var store: TZDB2_Block_File);
begin
  store.Position := block.Position;
  store.Size := block.Size;
  store.UsedSpace := block.UsedSpace;
  store.Prev := block.Prev;
  store.Next := block.Next;
end;

function TZDB2_BlockPtrList.TotalBlockSize: Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      inc(Result, Items[i]^.UsedSpace);
end;

procedure TZDB2_BlockPtrList.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Dispose(Items[i]);
  inherited Clear;
end;

constructor TZDB2_Block_File_Data_Instance.Create;
begin
  inherited Create;
  Position := 0;
  NextPosition := 0;
  Count := 0;
  SetLength(Buffer, 0);
  Last_Update_Head_MD5 := NULLMD5;
  Last_Update_Buffer_MD5 := NULLMD5;
  Last_Update_Encrypt_Buffer_Copy := TMem64.Create;
  Last_Update_Tail_MD5 := NULLMD5;
end;

destructor TZDB2_Block_File_Data_Instance.Destroy;
begin
  DisposeObject(Last_Update_Encrypt_Buffer_Copy);
  SetLength(Buffer, 0);
  inherited;
end;

function TZDB2_Block_File_Data_Instance.Read(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
var
  head: TZDB2_Block_File_Header;
  Tail: TZDB2_Block_File_Tail;
  MD5___: TMD5;
begin
  Result := False;
  // head
  if not umlFileSeek(Hnd_, Position_) then
    begin
      Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Read Seek error.');
      exit;
    end;
  if not umlBlockRead(Hnd_, head, SizeOf(TZDB2_Block_File_Header)) then
    begin
      Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Read Header error.');
      exit;
    end;
  if Assigned(Cipher_) then
      Cipher_.Decrypt(@head, SizeOf(TZDB2_Block_File_Header));
  if head.Flag1 <> C_ZDB2_SpaceTableHead_1 then
    begin
      Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Read verify SpaceTableHead1 error.');
      exit;
    end;
  if head.Flag2 <> C_ZDB2_SpaceTableHead_2 then
    begin
      Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Read verify SpaceTableHead2 error.');
      exit;
    end;
  Count := head.Count;
  SetLength(Buffer, Count);
  // read space table
  if not umlBlockRead(Hnd_, Buffer[0], SizeOf(TZDB2_Block_File) * Count) then
    begin
      Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Read Buffer error.');
      exit;
    end;
  Last_Update_Encrypt_Buffer_Copy.Size := SizeOf(TZDB2_Block_File) * Count;
  CopyPtr(@Buffer[0], Last_Update_Encrypt_Buffer_Copy.Memory, Last_Update_Encrypt_Buffer_Copy.Size);
  if Assigned(Cipher_) then
      Cipher_.Decrypt(@Buffer[0], SizeOf(TZDB2_Block_File) * Count);
  // verify md5
  MD5___ := umlMD5(@Buffer[0], SizeOf(TZDB2_Block_File) * Count);
  if not umlMD5Compare(MD5___, head.MD5) then
    begin
      Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Read verify Buffer md5 error.');
      exit;
    end;
  // tail
  if not umlBlockRead(Hnd_, Tail, SizeOf(TZDB2_Block_File_Tail)) then
    begin
      Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Read Tail error.');
      exit;
    end;
  if Assigned(Cipher_) then
      Cipher_.Decrypt(@Tail, SizeOf(TZDB2_Block_File_Tail));
  if Tail.Flag1 <> C_ZDB2_SpaceTableTail_1 then
    begin
      Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Read verify Tail1 error.');
      exit;
    end;
  if Tail.Flag2 <> C_ZDB2_SpaceTableTail_2 then
    begin
      Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Read verify Tail2 error.');
      exit;
    end;
  NextPosition := Tail.NextPosition;
  Position := Position_;
  Last_Update_Head_MD5 := umlMD5(@head, SizeOf(TZDB2_Block_File_Header));
  Last_Update_Buffer_MD5 := MD5___;
  Last_Update_Tail_MD5 := umlMD5(@Tail, SizeOf(TZDB2_Block_File_Tail));
  Result := True;
end;

function TZDB2_Block_File_Data_Instance.Write(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
var
  head: TZDB2_Block_File_Header;
  tmp: TMem64;
  Tail: TZDB2_Block_File_Tail;
  Last_Update_Head_MD5_: TMD5;
  Last_Update_Buffer_MD5_: TMD5;
  Last_Update_Tail_MD5_: TMD5;
  bPos, buff_Pos, chunk_size: Int64;
begin
  Result := False;
  // preprocess head
  head.Flag1 := C_ZDB2_SpaceTableHead_1;
  head.Count := length(Buffer);
  head.MD5 := umlMD5(@Buffer[0], SizeOf(TZDB2_Block_File) * length(Buffer));
  head.Flag2 := C_ZDB2_SpaceTableHead_2;
  // preprocess tail
  Tail.Flag1 := C_ZDB2_SpaceTableTail_1;
  Tail.NextPosition := NextPosition;
  Tail.Flag2 := C_ZDB2_SpaceTableTail_2;
  // optimizd
  Last_Update_Head_MD5_ := umlMD5(@head, SizeOf(TZDB2_Block_File_Header));
  Last_Update_Buffer_MD5_ := head.MD5;
  Last_Update_Tail_MD5_ := umlMD5(@Tail, SizeOf(TZDB2_Block_File_Tail));
  if umlCompareMD5(Last_Update_Head_MD5, Last_Update_Head_MD5_) and
    umlCompareMD5(Last_Update_Buffer_MD5, Last_Update_Buffer_MD5_) and
    umlCompareMD5(Last_Update_Tail_MD5, Last_Update_Tail_MD5_) then
    begin
      Result := True;
      exit;
    end;
  // write head
  if not umlFileSeek(Hnd_, Position_) then
    begin
      Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Write Seek error.');
      exit;
    end;
  if Assigned(Cipher_) then
      Cipher_.Encrypt(@head, SizeOf(TZDB2_Block_File_Header));
  if not umlBlockWrite(Hnd_, head, SizeOf(TZDB2_Block_File_Header)) then
    begin
      Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Write head error.');
      exit;
    end;
  // prpeare space table
  tmp := TMem64.Create;
  tmp.Size := SizeOf(TZDB2_Block_File) * length(Buffer);
  if tmp.Size > 0 then
      CopyPtr(@Buffer[0], tmp.Memory, tmp.Size);
  if Assigned(Cipher_) then
      Cipher_.Encrypt(tmp.Memory, tmp.Size);
  // optmized write space table
  if tmp.Size = Last_Update_Encrypt_Buffer_Copy.Size then
    begin
      bPos := Position_ + SizeOf(TZDB2_Block_File_Header);
      buff_Pos := 0;
      chunk_size := umlMax(tmp.Size div 100, $FFFF * 10);
      while buff_Pos + chunk_size < tmp.Size do
        begin
          if not CompareMemory(Last_Update_Encrypt_Buffer_Copy.PosAsPtr(buff_Pos), tmp.PosAsPtr(buff_Pos), chunk_size) then
            begin
              if not umlFileSeek(Hnd_, bPos + buff_Pos) then
                begin
                  Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Write Seek error.');
                  DisposeObject(tmp);
                  exit;
                end;
              if not umlBlockWrite(Hnd_, tmp.PosAsPtr(buff_Pos)^, chunk_size) then
                begin
                  Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Write Buffer error.');
                  DisposeObject(tmp);
                  exit;
                end;
            end;
          buff_Pos := buff_Pos + chunk_size;
        end;
      if tmp.Size > buff_Pos then
        begin
          if not CompareMemory(Last_Update_Encrypt_Buffer_Copy.PosAsPtr(buff_Pos), tmp.PosAsPtr(buff_Pos), tmp.Size - buff_Pos) then
            begin
              if not umlFileSeek(Hnd_, bPos + buff_Pos) then
                begin
                  Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Write Seek error.');
                  DisposeObject(tmp);
                  exit;
                end;
              if not umlBlockWrite(Hnd_, tmp.PosAsPtr(buff_Pos)^, tmp.Size - buff_Pos) then
                begin
                  Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Write Buffer error.');
                  DisposeObject(tmp);
                  exit;
                end;
            end;
          buff_Pos := tmp.Size;
        end;
      if not umlFileSeek(Hnd_, bPos + tmp.Size) then
        begin
          Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Write Seek error.');
          DisposeObject(tmp);
          exit;
        end;
    end
  else if not umlBlockWrite(Hnd_, tmp.Memory^, tmp.Size) then // direct write space table
    begin
      Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Write Buffer error.');
      DisposeObject(tmp);
      exit;
    end;
  Last_Update_Encrypt_Buffer_Copy.SwapInstance(tmp);
  Last_Update_Encrypt_Buffer_Copy.Position := 0;
  DisposeObject(tmp);
  // write tail
  if Assigned(Cipher_) then
      Cipher_.Encrypt(@Tail, SizeOf(TZDB2_Block_File_Tail));
  if not umlBlockWrite(Hnd_, Tail, SizeOf(TZDB2_Block_File_Tail)) then
    begin
      Sender.ErrorInfo('TZDB2_Block_File_Data_Instance.Write tail error.');
      exit;
    end;
  Last_Update_Head_MD5 := Last_Update_Head_MD5_;
  Last_Update_Buffer_MD5 := Last_Update_Buffer_MD5_;
  Last_Update_Tail_MD5 := Last_Update_Tail_MD5_;
  Result := True;
end;

procedure TZDB2_Block_File_Data_Instance.BuildBlockBuffer(var BlockBuffer_: TZDB2_BlockBuffer);
var
  i: Integer;
begin
  Position := 0;
  NextPosition := 0;
  Count := length(BlockBuffer_);
  SetLength(Buffer, Count);
  for i := 0 to Count - 1 do
      BlockToStore(BlockBuffer_[i], Buffer[i]);
end;

class function TZDB2_Block_File_Data_Instance.ComputeSize(BlockNum_: Integer): Int64;
begin
  Result := Int64(SizeOf(TZDB2_Block_File_Header)) + (Int64(SizeOf(TZDB2_Block_File)) * Int64(BlockNum_)) + Int64(SizeOf(TZDB2_Block_File_Tail));
end;

function TZDB2_Block_File_Data_Instance_List.BlockSum(): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      inc(Result, Items[i].Count);
end;

procedure TZDB2_Block_File_Data_Instance_List.ExtractToStoreBuffer(var Buffer: TZDB2_Block_File_Buffer);
var
  i, j, ID: Integer;
  Block_File_Data_Inst_: TZDB2_Block_File_Data_Instance;
begin
  SetLength(Buffer, BlockSum());
  ID := 0;
  for i := 0 to Count - 1 do
    begin
      Block_File_Data_Inst_ := Items[i];
      for j := 0 to Block_File_Data_Inst_.Count - 1 do
        begin
          Buffer[ID] := Block_File_Data_Inst_.Buffer[j];
          inc(ID);
        end;
    end;
end;

procedure TZDB2_Block_File_Data_Instance_List.ExtractToBlockBuffer(var Buffer: TZDB2_BlockBuffer; StartID_: Integer);
var
  i, j, ID: Integer;
  Block_File_Data_Inst_: TZDB2_Block_File_Data_Instance;
begin
  SetLength(Buffer, StartID_ + BlockSum());
  ID := StartID_;
  for i := 0 to Count - 1 do
    begin
      Block_File_Data_Inst_ := Items[i];
      for j := 0 to Block_File_Data_Inst_.Count - 1 do
        begin
          StoreToBlock(Block_File_Data_Inst_.Buffer[j], Buffer[ID]);
          Buffer[ID].ID := ID;
          inc(ID);
        end;
    end;
end;

procedure TZDB2_Block_File_Data_Instance_List.ExtractToBlockBuffer(var Buffer: TZDB2_BlockBuffer);
begin
  ExtractToBlockBuffer(Buffer, 0);
end;

function TZDB2_Block_File_Data_Instance_List.FillFromBlockBuffer(var Buffer: TZDB2_BlockBuffer; StartID_: Integer): Boolean;
var
  i, j, ID: Integer;
  Block_File_Data_Inst_: TZDB2_Block_File_Data_Instance;
begin
  Result := False;
  ID := StartID_;
  for i := 0 to Count - 1 do
    begin
      Block_File_Data_Inst_ := Items[i];
      if length(Block_File_Data_Inst_.Buffer) <> Block_File_Data_Inst_.Count then
          SetLength(Block_File_Data_Inst_.Buffer, Block_File_Data_Inst_.Count);
      for j := 0 to Block_File_Data_Inst_.Count - 1 do
        begin
          BlockToStore(Buffer[ID], Block_File_Data_Inst_.Buffer[j]);
          inc(ID);
        end;
    end;
  Result := True;
end;

function TZDB2_Block_File_Data_Instance_List.FillFromBlockBuffer(var Buffer: TZDB2_BlockBuffer): Boolean;
begin
  Result := FillFromBlockBuffer(Buffer, 0);
end;

function TZDB2_Block_File_Data_Instance_List.Read(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
var
  Pos_: Int64;
  Block_File_Data_Inst_: TZDB2_Block_File_Data_Instance;
begin
  Clean;
  Result := False;
  Pos_ := Position_;
  while (Pos_ > 0) and (Pos_ < umlFileGetSize(Hnd_)) do
    begin
      Block_File_Data_Inst_ := TZDB2_Block_File_Data_Instance.Create;
      if Block_File_Data_Inst_.Read(Sender, Cipher_, Pos_, Hnd_) then
        begin
          Add(Block_File_Data_Inst_);
          Pos_ := Block_File_Data_Inst_.NextPosition;
          Result := True;
        end
      else
        begin
          DisposeObject(Block_File_Data_Inst_);
          Result := False;
          break;
        end;
    end;
end;

function TZDB2_Block_File_Data_Instance_List.Write(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
var
  i: Integer;
  Block_File_Data_Inst_: TZDB2_Block_File_Data_Instance;
begin
  Result := True;
  for i := 0 to Count - 1 do
    begin
      Block_File_Data_Inst_ := Items[i];
      Result := Result and Block_File_Data_Inst_.Write(Sender, Cipher_, Block_File_Data_Inst_.Position, Hnd_);
    end;
end;

procedure TZDB2_Block_File_Data_Instance_List.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(Items[i]);
  inherited Clear;
end;

procedure TZDB2_Block_File_Data_Instance_List.Recycle_Memory;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      SetLength(Items[i].Buffer, 0);
end;

constructor TZDB2_Space_Planner.Create(Core_: TZDB2_Core_Space);
begin
  inherited Create;
  FCore := Core_;
  FStruct := TZDB2_Block_File_Data_Instance_List.Create;
  if FCore.FBlock_File_Data_Instance_List.Count > 0 then
    begin
      FCore.Flush;
    end
  else
    begin
      FCore.FHeader.Struct_Main := 0;
      FCore.WriteHeader;
    end;
  FWriteID := FCore.FBlockCount;
end;

destructor TZDB2_Space_Planner.Destroy;
begin
  Flush;
  FStruct.Clean;
  DisposeObject(FStruct);
  inherited;
end;

function TZDB2_Space_Planner.WriteStream(Stream_: TCore_Stream; BlockSize_: WORD; var SpaceHnd: TZDB2_BlockHandle): Boolean;
var
  BlockSize: WORD;
  Total_: Int64;
  n: Integer;
  BlockBuffer_: TZDB2_BlockBuffer;
  BlockID_: Integer;
  bakPos: Int64;
  SwapBuff_: Pointer;
  Block_File_Data_Inst_: TZDB2_Block_File_Data_Instance;
  i: Integer;
begin
  Result := False;
  SetLength(SpaceHnd, 0);
  if Stream_.Size <= 0 then
      exit;

  BlockSize := umlMax(BlockSize_, C_ZDB2_MinBlockSize);
  Total_ := Stream_.Size;
  n := Total_ div BlockSize;
  if Total_ mod BlockSize > 0 then
      inc(n);
  SetLength(BlockBuffer_, n);
  BlockID_ := 0;
  bakPos := Stream_.Position;
  Stream_.Position := 0;
  SwapBuff_ := System.GetMemory(BlockSize);
  try
    umlFileSeek(FCore.FSpace_IOHnd^, umlFileGetSize(FCore.FSpace_IOHnd^));

    while Total_ > 0 do
      begin
        if Total_ > BlockSize then
          begin
            if Stream_.Read(SwapBuff_^, BlockSize) <> BlockSize then
              begin
                FCore.ErrorInfo('TZDB2_Space_Planner.WriteStream Read error.');
                exit;
              end;
            BlockBuffer_[BlockID_].Position := umlFileGetPOS(FCore.FSpace_IOHnd^);
            BlockBuffer_[BlockID_].Size := BlockSize;
            BlockBuffer_[BlockID_].UsedSpace := BlockSize;
            BlockBuffer_[BlockID_].Prev := -1;
            BlockBuffer_[BlockID_].Next := -1;
            BlockBuffer_[BlockID_].ID := -1;
            FCore.DoEncrypt(SwapBuff_, BlockSize);
            if not umlBlockWrite(FCore.FSpace_IOHnd^, SwapBuff_^, BlockSize) then
              begin
                FCore.ErrorInfo('TZDB2_Space_Planner.WriteStream umlBlockWrite error.');
                exit;
              end;
            dec(Total_, BlockSize);
          end
        else
          begin
            if Stream_.Read(SwapBuff_^, Total_) <> Total_ then
              begin
                FCore.ErrorInfo('TZDB2_Space_Planner.WriteStream Read error.');
                exit;
              end;
            BlockBuffer_[BlockID_].Position := umlFileGetPOS(FCore.FSpace_IOHnd^);
            BlockBuffer_[BlockID_].Size := Total_;
            BlockBuffer_[BlockID_].UsedSpace := Total_;
            BlockBuffer_[BlockID_].Prev := -1;
            BlockBuffer_[BlockID_].Next := -1;
            BlockBuffer_[BlockID_].ID := -1;

            FCore.DoEncrypt(SwapBuff_, Total_);
            if not umlBlockWrite(FCore.FSpace_IOHnd^, SwapBuff_^, Total_) then
              begin
                FCore.ErrorInfo('TZDB2_Space_Planner.WriteStream umlBlockWrite error.');
                exit;
              end;
            Total_ := 0;
          end;
        inc(BlockID_);
        if Assigned(FCore.FOnProgress) then
            FCore.FOnProgress(n, BlockID_);
      end;

    Block_File_Data_Inst_ := TZDB2_Block_File_Data_Instance.Create;
    Block_File_Data_Inst_.BuildBlockBuffer(BlockBuffer_);
    FStruct.Add(Block_File_Data_Inst_);

    SetLength(SpaceHnd, n);
    for i := 0 to n - 1 do
        SpaceHnd[i] := FWriteID + i;
    inc(FWriteID, n);
    Result := True;
  finally
      System.FreeMemory(SwapBuff_);
  end;
end;

function TZDB2_Space_Planner.WriteStream(Stream_: TCore_Stream; BlockSize_: WORD; var ID: Integer): Boolean;
var
  SpaceHnd: TZDB2_BlockHandle;
begin
  Result := WriteStream(Stream_, BlockSize_, SpaceHnd);
  if length(SpaceHnd) > 0 then
      ID := SpaceHnd[0]
end;

function TZDB2_Space_Planner.WriteFile(FileName_: SystemString; BlockSize_: WORD; var SpaceHnd: TZDB2_BlockHandle): Boolean;
var
  fs: TCore_FileStream;
begin
  Result := False;
  if not umlFileExists(FileName_) then
      exit;
  try
    fs := TCore_FileStream.Create(FileName_, fmOpenRead or fmShareDenyNone);
    Result := WriteStream(fs, BlockSize_, SpaceHnd);
    DisposeObject(fs);
  except
  end;
end;

function TZDB2_Space_Planner.WriteFile(FileName_: SystemString; BlockSize_: WORD; var ID: Integer): Boolean;
var
  fs: TCore_FileStream;
begin
  Result := False;
  if not umlFileExists(FileName_) then
      exit;
  try
    fs := TCore_FileStream.Create(FileName_, fmOpenRead or fmShareDenyNone);
    Result := WriteStream(fs, BlockSize_, ID);
    DisposeObject(fs);
  except
  end;
end;

function TZDB2_Space_Planner.Flush: Boolean;
var
  i, j, k: Integer;
  BlockBuffer_: TZDB2_BlockBuffer;
  Block_File_Data_Inst_: TZDB2_Block_File_Data_Instance;
begin
  Result := False;
  if FStruct.Count = 0 then
      exit;

  // update data link
  k := FCore.FBlockCount;
  for i := 0 to FStruct.Count - 1 do
    begin
      with FStruct[i] do
        for j := 0 to Count - 1 do
          begin
            Buffer[j].Prev := if_(j = 0, -1, k - 1);
            Buffer[j].Next := if_(j = Count - 1, -1, k + 1);
            inc(k);
          end;
    end;
  // merge store struct
  Block_File_Data_Inst_ := TZDB2_Block_File_Data_Instance.Create;
  FStruct.ExtractToStoreBuffer(Block_File_Data_Inst_.Buffer);
  Block_File_Data_Inst_.Position := umlFileGetSize(FCore.FSpace_IOHnd^);
  Block_File_Data_Inst_.NextPosition := 0;
  Block_File_Data_Inst_.Count := length(Block_File_Data_Inst_.Buffer);

  if FCore.FBlock_File_Data_Instance_List.Count > 0 then
    begin
      // AppendSpace
      FCore.FBlock_File_Data_Instance_List.Last.NextPosition := Block_File_Data_Inst_.Position;
      FCore.Flush;
      Block_File_Data_Inst_.Write(FCore, FCore.FCipher, Block_File_Data_Inst_.Position, FCore.FSpace_IOHnd^);
      FCore.Open;
    end
  else
    begin
      // BuildSpace
      FCore.FHeader.Struct_Main := Block_File_Data_Inst_.Position;
      FCore.WriteHeader;
      Block_File_Data_Inst_.Write(FCore, FCore.FCipher, Block_File_Data_Inst_.Position, FCore.FSpace_IOHnd^);
      FCore.Open;
    end;
  DisposeObject(Block_File_Data_Inst_);
  FStruct.Clean;
  FWriteID := FCore.FBlockCount;
  Result := True;
end;

constructor TZDB2_CRC16.Create;
begin
  inherited Create;
  SetLength(CRC16Buffer, 0);
end;

destructor TZDB2_CRC16.Destroy;
begin
  SetLength(CRC16Buffer, 0);
  inherited Destroy;
end;

function TZDB2_CRC16.Build(Core_: TZDB2_Core_Space): Boolean;
var
  i: Integer;
  SwapBuff_: Pointer;
begin
  Result := False;
  SetLength(CRC16Buffer, Core_.FBlockCount);
  SwapBuff_ := System.GetMemory($FFFF);
  try
    for i := 0 to Core_.FBlockCount - 1 do
      with Core_.FBlockBuffer[i] do
        begin
          if UsedSpace = 0 then
              CRC16Buffer[i] := 0
          else if Core_.ReadCacheBlock(SwapBuff_, i) then
              CRC16Buffer[i] := umlCRC16(SwapBuff_, UsedSpace)
          else
            begin
              if not umlFileSeek(Core_.FSpace_IOHnd^, Core_.FBlockBuffer[i].Position) then
                begin
                  Core_.ErrorInfo('CRC16Build: umlFileSeek error.');
                  exit;
                end;
              if not umlBlockRead(Core_.FSpace_IOHnd^, SwapBuff_^, Core_.FBlockBuffer[i].UsedSpace) then
                begin
                  Core_.ErrorInfo('CRC16Build: umlBlockRead error.');
                  exit;
                end;
              Core_.DoDecrypt(SwapBuff_, UsedSpace);
              CRC16Buffer[i] := umlCRC16(SwapBuff_, UsedSpace);
            end;
          if Assigned(Core_.FOnProgress) then
              Core_.FOnProgress(Core_.FBlockCount, i);
        end;
    Result := True;
  finally
      System.FreeMemory(SwapBuff_);
  end;
end;

function TZDB2_CRC16.Build(Core_: TZDB2_Core_Space; Hnd: TZDB2_BlockHandle): Boolean;
var
  i: Integer;
  ID: Integer;
  SwapBuff_: Pointer;
begin
  Result := False;
  SetLength(CRC16Buffer, length(Hnd));
  SwapBuff_ := System.GetMemory($FFFF);
  try
    for i := 0 to length(Hnd) - 1 do
      begin
        ID := Hnd[i];
        with Core_.FBlockBuffer[ID] do
          begin
            if UsedSpace = 0 then
                CRC16Buffer[i] := 0
            else if Core_.ReadCacheBlock(SwapBuff_, ID) then
                CRC16Buffer[i] := umlCRC16(SwapBuff_, UsedSpace)
            else
              begin
                if not umlFileSeek(Core_.FSpace_IOHnd^, Core_.FBlockBuffer[ID].Position) then
                  begin
                    Core_.ErrorInfo('CRC16Build: umlFileSeek error.');
                    exit;
                  end;
                if not umlBlockRead(Core_.FSpace_IOHnd^, SwapBuff_^, Core_.FBlockBuffer[ID].UsedSpace) then
                  begin
                    Core_.ErrorInfo('CRC16Build: umlBlockRead error.');
                    exit;
                  end;
                Core_.DoDecrypt(SwapBuff_, UsedSpace);
                CRC16Buffer[i] := umlCRC16(SwapBuff_, UsedSpace);
              end;
            if Assigned(Core_.FOnProgress) then
                Core_.FOnProgress(length(Hnd), i);
          end;
      end;
    Result := True;
  finally
      System.FreeMemory(SwapBuff_);
  end;
end;

procedure TZDB2_CRC16.LoadFromStream(stream: TCore_Stream);
var
  num: Integer;
begin
  stream.Read(num, 4);
  SetLength(CRC16Buffer, num);
  if num > 0 then
      stream.Read(CRC16Buffer[0], num * 2);
end;

procedure TZDB2_CRC16.SaveToStream(stream: TCore_Stream);
var
  num: Integer;
begin
  num := length(CRC16Buffer);
  stream.Write(num, 4);
  if num > 0 then
      stream.Write(CRC16Buffer[0], length(CRC16Buffer) * 2);
end;

procedure TZDB2_CRC16.LoadFromFile(FileName_: SystemString);
var
  stream: TCore_Stream;
begin
  stream := TCore_FileStream.Create(FileName_, fmOpenRead or fmShareDenyNone);
  try
      LoadFromStream(stream);
  finally
      DisposeObject(stream);
  end;
end;

procedure TZDB2_CRC16.SaveToFile(FileName_: SystemString);
var
  stream: TCore_Stream;
begin
  stream := TCore_FileStream.Create(FileName_, fmCreate);
  try
      SaveToStream(stream);
  finally
      DisposeObject(stream);
  end;
end;

class function TZDB2_Cipher.GetCipherSecurity(CipherSecurityString_: U_String): TCipherSecurity;
var
  arry: TCipherSecurityArray;
  i: Integer;
  cs: TCipherSecurity;
begin
  arry := TCipher.AllCipher;
  cs := TCipherSecurity.csNone;
  for i := low(arry) to high(arry) do
    if CipherSecurityString_.Same(TCipher.CCipherSecurityName[arry[i]]) then
        cs := arry[i];
  Result := cs;
end;

constructor TZDB2_Cipher.Create(CipherSecurity_: TCipherSecurity; password_: U_String; Level_: Integer; Tail_, CBC_: Boolean);
begin
  inherited Create;
  FCipher_ := CreateCipherClassFromPassword(CipherSecurity_, password_);
  FCipher_.Level := Level_;
  FCipher_.ProcessTail := Tail_;
  FCipher_.CBC := CBC_;
end;

constructor TZDB2_Cipher.Create(CipherSecurityString_: U_String; password_: U_String; Level_: Integer; Tail_, CBC_: Boolean);
var
  arry: TCipherSecurityArray;
  i: Integer;
  cs: TCipherSecurity;
begin
  arry := TCipher.AllCipher;
  cs := TCipherSecurity.csNone;
  for i := low(arry) to high(arry) do
    if CipherSecurityString_.Same(TCipher.CCipherSecurityName[arry[i]]) then
        cs := arry[i];
  Create(cs, password_, Level_, Tail_, CBC_);
end;

destructor TZDB2_Cipher.Destroy;
begin
  DisposeObject(FCipher_);
  inherited Destroy;
end;

procedure TZDB2_Cipher.Encrypt(buff: Pointer; Size: NativeInt);
begin
  FCipher_.Encrypt(buff, Size);
end;

procedure TZDB2_Cipher.Decrypt(buff: Pointer; Size: NativeInt);
begin
  FCipher_.Decrypt(buff, Size);
end;

function TZDB2_Cipher.Get_CipherSecurity: TCipherSecurity;
begin
  Result := FCipher_.CipherSecurity;
end;

class procedure TZDB2_Cipher.Test;
var
  cs: TCipherSecurity;
  c: TZDB2_Cipher;
  s1, s2: U_String;
  buff: TBytes;
begin
  for cs in TCipher.AllCipher do
    begin
      c := TZDB2_Cipher.Create(cs, '123456', 1, True, True);
      s1 := 'hello world,1234567890';
      buff := s1.ANSI;
      c.Encrypt(@buff[0], length(buff));
      c.Decrypt(@buff[0], length(buff));
      s2.ANSI := buff;
      if s1.Same(s2) then
          DoStatus('TZDB2_Cipher test ok')
      else
          DoStatus('TZDB2_Cipher test error');
      DisposeObject(c);
    end;
end;

procedure TZDB2_SpaceState.Reset;
begin
  Physics := 0;
  FreeSpace := 0;
  Cache := 0;
  ReadNum := 0;
  ReadSize := 0;
  WriteNum := 0;
  WriteSize := 0;
end;

constructor TZDB2_Atom_SpaceState.Create;
var
  tmp: TZDB2_SpaceState;
begin
  tmp.Reset;
  inherited Create(tmp);
end;

procedure TZDB2_Core_Space_Info.DoFree(var Data: SystemString);
begin
  Data := '';
end;

function TZDB2_Core_Space.Check_ReadCache(ID: Integer): Boolean;
var
  p: PZDB2_Block;
begin
  Result := False;
  p := @FBlockBuffer[ID];
  with FBlockWriteCache[p^.ID] do
      Result := (FUsedReadCache or FlushThisCacheToFile) and (p^.UsedSpace > 0) and (Mem <> nil); // fixed by qq600585, Mode = smNormal
end;

function TZDB2_Core_Space.ReadCacheBlock(buff: Pointer; ID: Integer): Boolean;
var
  p: PZDB2_Block;
begin
  Result := False;
  p := @FBlockBuffer[ID];
  with FBlockWriteCache[p^.ID] do
    if (FUsedReadCache or FlushThisCacheToFile) and (p^.UsedSpace > 0) and (Mem <> nil) then // fixed by qq600585, Mode = smNormal
      begin
        Mem.Position := 0;
        Mem.ReadPtr(buff, p^.UsedSpace);
        Mem.Position := 0;
        Result := True;
      end;
end;

function TZDB2_Core_Space.WriteCacheBlock(buff: Pointer; siz: Integer; ID: Integer; FlushThisCache_: Boolean): Boolean;
var
  p: PZDB2_Block;
begin
  Result := False;
  if not FUsedWriteCache then
      exit;
  p := @FBlockBuffer[ID];
  with FBlockWriteCache[ID] do
    begin
      if Mem = nil then
        begin
          Mem := TZDB2_Mem.Create;
          Mem.Size := p^.Size;
          FillPtr(Mem.Memory, p^.Size, 0);
          inc(FState.Cache, p^.Size);
        end;

      Mem.Position := 0;
      Mem.WritePtr(buff, siz);
      Mem.Position := 0;
      if FlushThisCache_ then
          p^.UsedSpace := siz;
      FlushThisCacheToFile := FlushThisCacheToFile or FlushThisCache_;
    end;
  Result := True;
  if FState.Cache > FMaxCacheMemory then
      FlushCache();
end;

procedure TZDB2_Core_Space.DeleteCache(ID: Integer);
begin
  with FBlockWriteCache[ID] do
    begin
      if Mem <> nil then
        begin
          dec(FState.Cache, Mem.Size);
          DisposeObjectAndNil(Mem);
        end;
      FlushThisCacheToFile := False;
    end;
end;

procedure TZDB2_Core_Space.ClearCache;
var
  i: Int64;
begin
  i := 0;
  while i < length(FBlockWriteCache) do
    with FBlockWriteCache[i] do
      begin
        DisposeObjectAndNil(Mem);
        FlushThisCacheToFile := False;
        inc(i);
      end;
  FState.Cache := 0;
end;

procedure TZDB2_Core_Space.FlushCache;
var
  i: Integer;
begin
  if FSpace_IOHnd^.IsOnlyRead then
    begin
      WarningInfo('FlushCache: OnlyRead.');
      exit;
    end;

  i := 0;
  while i < FBlockCount do
    begin
      with FBlockBuffer[i], FBlockWriteCache[i] do
        if Mem <> nil then
          begin
            if FlushThisCacheToFile then
              begin
                if not umlFileSeek(FSpace_IOHnd^, Position) then
                  begin
                    ErrorInfo('FlushCache: umlFileSeek error.');
                    exit;
                  end;
                DoEncrypt(Mem.Memory, UsedSpace);
                if not umlBlockWrite(FSpace_IOHnd^, Mem.Memory^, UsedSpace) then
                  begin
                    ErrorInfo('FlushCache: umlBlockWrite error.');
                    exit;
                  end;
                FlushThisCacheToFile := False;
                if Size - UsedSpace > 0 then
                  if not umlBlockWrite(FSpace_IOHnd^, ZDB2_NULL_Data, Size - UsedSpace) then
                    begin
                      ErrorInfo('FlushCache: umlBlockWrite error.');
                      exit;
                    end;
              end;
            DisposeObjectAndNil(Mem);
          end;
      inc(i);
    end;
  FState.Cache := 0;
end;

function TZDB2_Core_Space.WriteHeader: Boolean;
begin
  Result := False;
  if not umlFileSeek(FSpace_IOHnd^, 0) then
    begin
      ErrorInfo('WriteHeader: umlFileSeek error.');
      exit;
    end;
  FHeader.Flag := C_ZDB2_FileHead;
  FHeader.Major := 2;
  FHeader.Minor := 0;
  if not umlBlockWrite(FSpace_IOHnd^, FHeader, C_ZDB2_HeaderSize) then
    begin
      ErrorInfo('WriteHeader: umlBlockWrite Header error.');
      exit;
    end;
  Result := True;
end;

procedure TZDB2_Core_Space.Do_Modification();
begin
  FLast_Modification := GetTimeTick();
  if FSpace_IOHnd^.IsOnlyRead then
      exit;
  if FHeader.Modification then
      exit;
  FHeader.Modification := True;
  WriteHeader();
end;

function TZDB2_Core_Space.WriteTable(): Boolean;
begin
  Result := False;
  if FSpace_IOHnd^.IsOnlyRead then
    begin
      WarningInfo('WriteTable: OnlyRead.');
      exit;
    end;

  if not WriteHeader() then
      exit;
  FBlock_File_Data_Instance_List.FillFromBlockBuffer(FBlockBuffer);
  if not FBlock_File_Data_Instance_List.Write(Self, FCipher, FHeader.Struct_Main, FSpace_IOHnd^) then
    begin
      ErrorInfo('WriteTable: write BlockStoreDataStruct error.');
      exit;
    end;
  Result := True;
end;

procedure TZDB2_Core_Space.PrepareCacheBlock();
var
  i: Integer;
begin
  ClearCache;
  FFreeSpaceIndexProbe := 0;
  FBlockCount := length(FBlockBuffer);
  SetLength(FBlockWriteCache, FBlockCount);
  FState.Reset;

  i := 0;
  while i < FBlockCount do
    with FBlockWriteCache[i] do
      begin
        Mem := nil;
        FlushThisCacheToFile := False;
        inc(i);
      end;
end;

function TZDB2_Core_Space.GetUserCustomHeader: PZDB2_UserCustomHeader;
begin
  Result := @FHeader.UserCustomHeader
end;

procedure TZDB2_Core_Space.SetMode(const Value: TZDB2_SpaceMode);
begin
  FMode := Value;
  case FMode of
    smBigData:
      begin
        FUsedReadCache := False;
        FUsedWriteCache := False;
      end;
    smNormal:
      begin
        FUsedReadCache := False;
        FUsedWriteCache := True;
      end;
    smFast:
      begin
        FUsedReadCache := True;
        FUsedWriteCache := True;
      end;
  end;
end;

procedure TZDB2_Core_Space.DoDecrypt(buff: Pointer; Size: NativeInt);
begin
  if (Size > 0) and Assigned(FCipher) then
      FCipher.Decrypt(buff, Size);
end;

procedure TZDB2_Core_Space.DoEncrypt(buff: Pointer; Size: NativeInt);
begin
  if (Size > 0) and Assigned(FCipher) then
      FCipher.Encrypt(buff, Size);
end;

function TZDB2_Core_Space.DoEncryptTemp(buff: Pointer; Size: NativeInt; BuffProtected_: Boolean): Pointer;
begin
  if (Size > 0) and Assigned(FCipher) then
    begin
      if BuffProtected_ then
        begin
          FCipherMem.Position := 0;
          FCipherMem.WritePtr(buff, Size);
          Result := FCipherMem.Memory;
        end
      else
          Result := buff;
      FCipher.Encrypt(Result, Size);
    end
  else
      Result := buff;
end;

function TZDB2_Core_Space.GetState: PZDB2_Core_SpaceState;
begin
  Result := @FState;
end;

class function TZDB2_Core_Space.CheckStream(stream: TCore_Stream; Cipher_: IZDB2_Cipher): Boolean;
var
  bak_pos: Int64;
  ioHnd: TIOHnd;
  tmp: TZDB2_Core_Space;
begin
  Result := False;
  bak_pos := stream.Position;
  InitIOHnd(ioHnd);
  if umlFileOpenAsStream('', stream, ioHnd, True) then
    begin
      tmp := TZDB2_Core_Space.Create(@ioHnd);
      tmp.Cipher := Cipher_;
      Result := tmp.Open;
      DisposeObject(tmp);
    end;
  stream.Position := bak_pos;
end;

class function TZDB2_Core_Space.CheckStream(stream: TCore_Stream; Cipher_: IZDB2_Cipher; Check_Fault_Shutdown_: Boolean): Boolean;
var
  bak_pos: Int64;
  ioHnd: TIOHnd;
  tmp: TZDB2_Core_Space;
begin
  Result := False;
  bak_pos := stream.Position;
  InitIOHnd(ioHnd);
  if umlFileOpenAsStream('', stream, ioHnd, True) then
    begin
      tmp := TZDB2_Core_Space.Create(@ioHnd);
      tmp.Cipher := Cipher_;
      Result := tmp.Open;
      if Check_Fault_Shutdown_ then
          Result := Result and (not tmp.Fault_Shutdown);
      DisposeObject(tmp);
    end;
  stream.Position := bak_pos;
end;

constructor TZDB2_Core_Space.Create(IOHnd_: PIOHnd);
begin
  inherited Create;
  FillPtr(@FHeader, C_ZDB2_HeaderSize, 0);
  FFault_Shutdown := False;
  FAutoCloseIOHnd := False;
  FAutoFreeIOHnd := False;
  FSpace_IOHnd := IOHnd_;
  FFreeSpaceIndexProbe := 0;
  FBlockCount := 0;
  SetLength(FBlockBuffer, 0);
  FBlock_File_Data_Instance_List := TZDB2_Block_File_Data_Instance_List.Create;
  FMaxCacheMemory := 1024 * 1024 * 32;
  FUsedReadCache := False;
  FUsedWriteCache := True;
  SetLength(FBlockWriteCache, 0);
  FMode := smNormal;
  FCipher := nil;
  FCipherMem := TMem64.Create;

  FState.Reset;

  FLast_Modification := GetTimeTick();
  FLast_Error_Info := TZDB2_Core_Space_Error_Info.Create;
  FLast_Warning_Info := TZDB2_Core_Space_Warning_Info.Create;
  FOnProgress := nil;
  FOnNoSpace := nil;
end;

destructor TZDB2_Core_Space.Destroy;
begin
  if FHeader.Modification then
      Flush();

  SetLength(FBlockBuffer, 0);
  FBlock_File_Data_Instance_List.Clean;
  DisposeObject(FBlock_File_Data_Instance_List);
  ClearCache;
  SetLength(FBlockWriteCache, 0);
  DisposeObject(FCipherMem);

  if FAutoCloseIOHnd then
      umlFileClose(FSpace_IOHnd^);
  if FAutoFreeIOHnd then
    begin
      InitIOHnd(FSpace_IOHnd^);
      Dispose(FSpace_IOHnd);
    end;
  FCipher := nil;
  DisposeObject(FLast_Error_Info);
  DisposeObject(FLast_Warning_Info);
  inherited Destroy;
end;

function TZDB2_Core_Space.Is_Modification: Boolean;
begin
  Result := FHeader.Modification;
end;

procedure TZDB2_Core_Space.ErrorInfo(const Text_: SystemString);
begin
  FLast_Error_Info.Add('ZDB2 Core failed - ' + Text_);
  DoStatus(FLast_Error_Info.Last^.Data);
end;

procedure TZDB2_Core_Space.WarningInfo(const Text_: SystemString);
begin
  FLast_Warning_Info.Add('ZDB2 Core warning - ' + Text_);
  DoStatus(FLast_Warning_Info.Last^.Data);
end;

procedure TZDB2_Core_Space.Flush;
begin
  if FSpace_IOHnd^.IsOnlyRead then
      exit;
  FlushCache;
  FHeader.Modification := False;
  WriteTable();
  umlFileUpdate(FSpace_IOHnd^);
end;

procedure TZDB2_Core_Space.Save;
begin
  Flush();
end;

function TZDB2_Core_Space.Open(): Boolean;
var
  num: Integer;
begin
  Result := False;
  FBlock_File_Data_Instance_List.Clean;
  FillPtr(@FHeader, C_ZDB2_HeaderSize, 0);
  if umlFileGetSize(FSpace_IOHnd^) < C_ZDB2_HeaderSize then
      exit;
  umlFileSeek(FSpace_IOHnd^, 0);
  umlBlockRead(FSpace_IOHnd^, FHeader, C_ZDB2_HeaderSize);
  if FHeader.Flag <> C_ZDB2_FileHead then
    begin
      ErrorInfo('Open: header token error.');
      exit;
    end;
  if (FHeader.Major = 2) and (FHeader.Minor = 0) then
    begin
      FFault_Shutdown := FHeader.Modification;
      if FFault_Shutdown then
          WarningInfo('Open: Fault Shutdown');
      if FHeader.Struct_Main >= C_ZDB2_HeaderSize then
        if not FBlock_File_Data_Instance_List.Read(Self, FCipher, FHeader.Struct_Main, FSpace_IOHnd^) then
          begin
            ErrorInfo('Open: read BlockStoreDataStruct error.');
            exit;
          end;
      FBlock_File_Data_Instance_List.ExtractToBlockBuffer(FBlockBuffer);
      FBlock_File_Data_Instance_List.Recycle_Memory;
      PrepareCacheBlock();
      ScanSpace();
      FHeader.Modification := False;
      Result := True;
    end
  else
    begin
      ErrorInfo('Open: major/minor info error.');
      exit;
    end;
end;

procedure TZDB2_Core_Space.ScanSpace;
var
  i: Integer;
begin
  FFreeSpaceIndexProbe := FBlockCount;
  FState.Physics := 0;
  FState.FreeSpace := 0;
  i := 0;
  while i < FBlockCount do
    begin
      with FBlockBuffer[i] do
        begin
          inc(FState.Physics, Size);
          if UsedSpace = 0 then
            begin
              inc(FState.FreeSpace, Size);
              if i < FFreeSpaceIndexProbe then
                  FFreeSpaceIndexProbe := i;
            end;
        end;
      inc(i);
    end;
end;

function TZDB2_Core_Space.Fast_BuildSpace(PhySpaceSize: Int64; BlockSize_: WORD): Boolean;
var
  BlockSize: WORD;
  Block_File_Data_Inst_: TZDB2_Block_File_Data_Instance;
  headSiz: Int64;
  i: Integer;
  fp: Int64;
begin
  Result := False;
  if FSpace_IOHnd^.IsOnlyRead then
    begin
      ErrorInfo('Fast_BuildSpace: OnlyRead.');
      exit;
    end;
  if not umlFileSeek(FSpace_IOHnd^, 0) then
    begin
      ErrorInfo('Fast_BuildSpace: umlFileSeek 0 error.');
      exit;
    end;
  // prepare block
  FBlock_File_Data_Instance_List.Clean;
  BlockSize := umlMax(BlockSize_, C_ZDB2_MinBlockSize);
  headSiz := Int64(C_ZDB2_HeaderSize) + TZDB2_Block_File_Data_Instance.ComputeSize(PhySpaceSize div BlockSize);
  SetLength(FBlockBuffer, (PhySpaceSize - headSiz) div BlockSize);
  PrepareCacheBlock();
  // fast init space
  if not umlFileSetSize(FSpace_IOHnd^, headSiz + (Int64(BlockSize) * Int64(FBlockCount))) then // fast alloc space
    begin
      ErrorInfo(PFormat('Fast_BuildSpace: umlFileSetSize %d error.', [headSiz + (Int64(BlockSize) * Int64(FBlockCount))]));
      exit;
    end;
  if not umlFileSeek(FSpace_IOHnd^, 0) then // reseek
    begin
      ErrorInfo('Fast_BuildSpace: umlFileSeek 0 error.');
      exit;
    end;
  // init space table
  fp := headSiz;
  i := 0;
  while i < FBlockCount do
    begin
      FBlockBuffer[i].Position := fp;
      FBlockBuffer[i].Size := BlockSize;
      FBlockBuffer[i].UsedSpace := 0;
      FBlockBuffer[i].Next := -1;
      FBlockBuffer[i].Prev := -1;
      FBlockBuffer[i].ID := i;
      inc(fp, BlockSize);
      inc(i);
    end;
  // update struct entry
  FHeader.Struct_Main := C_ZDB2_HeaderSize;
  // builder store struct
  Block_File_Data_Inst_ := TZDB2_Block_File_Data_Instance.Create;
  Block_File_Data_Inst_.BuildBlockBuffer(FBlockBuffer);
  Block_File_Data_Inst_.Position := FHeader.Struct_Main;
  Block_File_Data_Inst_.NextPosition := 0;
  FBlock_File_Data_Instance_List.Add(Block_File_Data_Inst_);
  // table
  WriteTable();
  // finish
  ScanSpace();
  Do_Modification;
  Result := True;
end;

function TZDB2_Core_Space.Fast_AppendSpace(NewSpaceSize_: Int64; DestBlockSize_: WORD): Boolean;
var
  BlockSize: WORD;
  BlockNum_: Integer;
  tmp: TZDB2_BlockBuffer;
  headPos, headSiz: Int64;
  Block_File_Data_Inst_: TZDB2_Block_File_Data_Instance;
  fp: Int64;
  i: Integer;
begin
  if FSpace_IOHnd^.IsOnlyRead then
    begin
      ErrorInfo('Fast_AppendSpace: OnlyRead.');
      Result := False;
      exit;
    end;
  if FBlock_File_Data_Instance_List.Count = 0 then
    begin
      Result := Fast_BuildSpace(NewSpaceSize_, DestBlockSize_);
      exit;
    end;
  Result := False;
  // flush
  FlushCache;
  if not FBlock_File_Data_Instance_List.FillFromBlockBuffer(FBlockBuffer) then
    begin
      ErrorInfo('Fast_AppendSpace: FillFromBlockBuffer error.');
      exit;
    end;
  // prepare block
  BlockSize := umlMax(DestBlockSize_, C_ZDB2_MinBlockSize);
  BlockNum_ := NewSpaceSize_ div Int64(DestBlockSize_);
  SetLength(tmp, BlockNum_);
  headPos := umlFileGetSize(FSpace_IOHnd^);
  headSiz := TZDB2_Block_File_Data_Instance.ComputeSize(length(tmp));
  // fast append space
  if not umlFileSetSize(FSpace_IOHnd^, headPos + headSiz + (Int64(BlockSize) * Int64(BlockNum_))) then // fast alloc space
    begin
      ErrorInfo(PFormat('Fast_AppendSpace: umlFileSetSize %d error.', [headPos + headSiz + (Int64(BlockSize) * Int64(BlockNum_))]));
      exit;
    end;
  if not umlFileSeek(FSpace_IOHnd^, headPos) then // reseek
    begin
      ErrorInfo(PFormat('Fast_AppendSpace: umlFileSeek %d error.', [headPos]));
      exit;
    end;
  // fill free space
  fp := headPos + headSiz;
  i := 0;
  while i < length(tmp) do
    begin
      tmp[i].Position := fp;
      tmp[i].Size := BlockSize;
      tmp[i].UsedSpace := 0;
      tmp[i].Next := -1;
      tmp[i].Prev := -1;
      tmp[i].ID := i + FBlockCount;
      inc(fp, BlockSize);
      inc(i);
    end;
  // builder store struct
  Block_File_Data_Inst_ := TZDB2_Block_File_Data_Instance.Create;
  Block_File_Data_Inst_.BuildBlockBuffer(tmp);
  SetLength(tmp, 0);
  Block_File_Data_Inst_.Position := headPos;
  Block_File_Data_Inst_.NextPosition := 0;
  FBlock_File_Data_Instance_List.Last.NextPosition := headPos;
  FBlock_File_Data_Instance_List.Add(Block_File_Data_Inst_);
  FBlock_File_Data_Instance_List.ExtractToBlockBuffer(FBlockBuffer);
  // Rebuild cache
  PrepareCacheBlock();
  // finish
  ScanSpace();
  Do_Modification;
  Result := True;
end;

function TZDB2_Core_Space.BuildSpace(PhySpaceSize: Int64; BlockSize_: WORD): Boolean;
var
  BlockSize: WORD;
  Block_File_Data_Inst_: TZDB2_Block_File_Data_Instance;
  headSiz: Int64;
  m64: TZDB2_Mem;
  i: Integer;
begin
  Result := False;
  if FSpace_IOHnd^.IsOnlyRead then
    begin
      ErrorInfo('BuildSpace: OnlyRead.');
      exit;
    end;
  if not umlFileSeek(FSpace_IOHnd^, 0) then
    begin
      ErrorInfo('BuildSpace: umlFileSeek 0 error.');
      exit;
    end;
  // prepare block
  FBlock_File_Data_Instance_List.Clean;
  BlockSize := umlMax(BlockSize_, C_ZDB2_MinBlockSize);
  headSiz := Int64(C_ZDB2_HeaderSize) + TZDB2_Block_File_Data_Instance.ComputeSize(PhySpaceSize div Int64(BlockSize));
  SetLength(FBlockBuffer, (PhySpaceSize - headSiz) div Int64(BlockSize));
  PrepareCacheBlock();
  // prealloc header space
  m64 := TZDB2_Mem.Create;
  m64.Size := headSiz;
  m64.Position := 0;
  FillPtr(m64.Memory, m64.Size, 0);
  if not umlBlockWrite(FSpace_IOHnd^, m64.Memory^, m64.Size) then
    begin
      ErrorInfo('BuildSpace: umlBlockWrite zero head error.');
      exit;
    end;
  DisposeObject(m64);
  // fill free space
  i := 0;
  while i < FBlockCount do
    begin
      FBlockBuffer[i].Position := umlFileGetPOS(FSpace_IOHnd^);
      FBlockBuffer[i].Size := BlockSize;
      FBlockBuffer[i].UsedSpace := 0;
      FBlockBuffer[i].Next := -1;
      FBlockBuffer[i].Prev := -1;
      FBlockBuffer[i].ID := i;
      if not umlBlockWrite(FSpace_IOHnd^, ZDB2_NULL_Data, BlockSize) then
        begin
          ErrorInfo('BuildSpace: umlBlockWrite NullData error.');
          exit;
        end;
      inc(i);
      if Assigned(FOnProgress) then
          FOnProgress(FBlockCount, i);
    end;
  // update struct entry
  FHeader.Struct_Main := C_ZDB2_HeaderSize;
  // builder store struct
  Block_File_Data_Inst_ := TZDB2_Block_File_Data_Instance.Create;
  Block_File_Data_Inst_.BuildBlockBuffer(FBlockBuffer);
  Block_File_Data_Inst_.Position := FHeader.Struct_Main;
  Block_File_Data_Inst_.NextPosition := 0;
  FBlock_File_Data_Instance_List.Add(Block_File_Data_Inst_);
  // table
  WriteTable();
  // finish
  ScanSpace();
  Do_Modification;
  Result := True;
end;

function TZDB2_Core_Space.AppendSpace(NewSpaceSize_: Int64; DestBlockSize_: WORD): Boolean;
var
  BlockSize: WORD;
  BlockNum_: Integer;
  tmp: TZDB2_BlockBuffer;
  headPos, headSiz: Int64;
  Block_File_Data_Inst_: TZDB2_Block_File_Data_Instance;
  m64: TZDB2_Mem;
  i: Integer;
begin
  if FSpace_IOHnd^.IsOnlyRead then
    begin
      ErrorInfo('AppendSpace: OnlyRead.');
      Result := False;
      exit;
    end;
  if FBlock_File_Data_Instance_List.Count = 0 then
    begin
      Result := BuildSpace(NewSpaceSize_, DestBlockSize_);
      exit;
    end;
  Result := False;
  // flush
  FlushCache;
  if not FBlock_File_Data_Instance_List.FillFromBlockBuffer(FBlockBuffer) then
    begin
      ErrorInfo('AppendSpace: FillFromBlockBuffer error.');
      exit;
    end;
  // prepare block
  BlockSize := umlMax(DestBlockSize_, C_ZDB2_MinBlockSize);
  BlockNum_ := NewSpaceSize_ div Int64(DestBlockSize_);
  SetLength(tmp, BlockNum_);
  headPos := umlFileGetSize(FSpace_IOHnd^);
  headSiz := TZDB2_Block_File_Data_Instance.ComputeSize(length(tmp));
  // prealloc header space
  m64 := TZDB2_Mem.Create;
  m64.Size := headSiz;
  m64.Position := 0;
  FillPtr(m64.Memory, m64.Size, 0);
  if not umlFileSeek(FSpace_IOHnd^, headPos) then
    begin
      ErrorInfo('AppendSpace: umlFileSeek to tail error.');
      exit;
    end;
  if not umlBlockWrite(FSpace_IOHnd^, m64.Memory^, m64.Size) then
    begin
      ErrorInfo('AppendSpace: umlBlockWrite zero head error.');
      exit;
    end;
  DisposeObject(m64);
  // fill free space
  i := 0;
  while i < length(tmp) do
    begin
      tmp[i].Position := umlFileGetPOS(FSpace_IOHnd^);
      tmp[i].Size := BlockSize;
      tmp[i].UsedSpace := 0;
      tmp[i].Next := -1;
      tmp[i].Prev := -1;
      tmp[i].ID := i + FBlockCount;
      if not umlBlockWrite(FSpace_IOHnd^, ZDB2_NULL_Data, BlockSize) then
        begin
          ErrorInfo('AppendSpace: umlBlockWrite NullData error.');
          exit;
        end;
      inc(i);
      if Assigned(FOnProgress) then
          FOnProgress(length(tmp), i);
    end;
  // builder store struct
  Block_File_Data_Inst_ := TZDB2_Block_File_Data_Instance.Create;
  Block_File_Data_Inst_.BuildBlockBuffer(tmp);
  SetLength(tmp, 0);
  Block_File_Data_Inst_.Position := headPos;
  Block_File_Data_Inst_.NextPosition := 0;
  FBlock_File_Data_Instance_List.Last.NextPosition := headPos;
  FBlock_File_Data_Instance_List.Add(Block_File_Data_Inst_);
  FBlock_File_Data_Instance_List.ExtractToBlockBuffer(FBlockBuffer);
  // Rebuild cache
  PrepareCacheBlock();
  // finish
  ScanSpace();
  Do_Modification;
  Result := True;
end;

function TZDB2_Core_Space.OptimizedSpaceTo(var Dest_IOHnd: TIOHnd): Boolean;
var
  dest_Header: TZDB2_FileHeader;
  dest_BlockBuffer: TZDB2_BlockBuffer;
  dest_StoreData: TZDB2_Block_File_Data_Instance;
  i: Integer;
  headSize, headPos_: Int64;
  m64: TZDB2_Mem;
  SwapBuff_: Pointer;
begin
  Result := False;
  dest_StoreData := nil;
  FlushCache;
  headSize := Int64(C_ZDB2_HeaderSize) + TZDB2_Block_File_Data_Instance.ComputeSize(FBlockCount);
  headPos_ := C_ZDB2_HeaderSize;

  m64 := TZDB2_Mem.Create;
  m64.Size := headSize;
  FillPtr(m64.Memory, m64.Size, 0);
  umlFileSeek(Dest_IOHnd, 0);
  umlBlockWrite(Dest_IOHnd, m64.Memory^, m64.Size);
  DisposeObject(m64);

  SetLength(dest_BlockBuffer, FBlockCount);
  SwapBuff_ := System.GetMemory($FFFF);
  try
    for i := 0 to FBlockCount - 1 do
      begin
        dest_BlockBuffer[i].Position := umlFileGetPOS(Dest_IOHnd);
        dest_BlockBuffer[i].Size := FBlockBuffer[i].Size;
        dest_BlockBuffer[i].UsedSpace := FBlockBuffer[i].UsedSpace;
        dest_BlockBuffer[i].Prev := FBlockBuffer[i].Prev;
        dest_BlockBuffer[i].Next := FBlockBuffer[i].Next;
        dest_BlockBuffer[i].ID := FBlockBuffer[i].ID;

        if not umlFileSeek(FSpace_IOHnd^, FBlockBuffer[i].Position) then
          begin
            ErrorInfo('OptimizedSpaceTo: umlFileSeek source error.');
            exit;
          end;
        if not umlBlockRead(FSpace_IOHnd^, SwapBuff_^, FBlockBuffer[i].Size) then
          begin
            ErrorInfo('OptimizedSpaceTo: umlBlockRead source error.');
            exit;
          end;
        if not umlBlockWrite(Dest_IOHnd, SwapBuff_^, dest_BlockBuffer[i].Size) then
          begin
            ErrorInfo('OptimizedSpaceTo: umlBlockWrite dest error.');
            exit;
          end;
        if Assigned(FOnProgress) then
            FOnProgress(FBlockCount, i);
      end;
    FillPtr(@dest_Header, C_ZDB2_HeaderSize, 0);
    dest_Header.Flag := C_ZDB2_FileHead;
    dest_Header.Major := 2;
    dest_Header.Minor := 0;
    dest_Header.Struct_Main := headPos_;
    umlFileSeek(Dest_IOHnd, 0);
    umlBlockWrite(Dest_IOHnd, dest_Header, C_ZDB2_HeaderSize);

    dest_StoreData := TZDB2_Block_File_Data_Instance.Create;
    dest_StoreData.BuildBlockBuffer(dest_BlockBuffer);
    dest_StoreData.Position := headPos_;
    dest_StoreData.NextPosition := 0;
    dest_StoreData.Count := length(dest_StoreData.Buffer);
    dest_StoreData.Write(Self, FCipher, headPos_, Dest_IOHnd);
    Result := True;
  finally
    System.FreeMemory(SwapBuff_);
    SetLength(dest_BlockBuffer, 0);
    DisposeObject(dest_StoreData);
  end;
end;

function TZDB2_Core_Space.Check(ID_: Integer): Boolean;
var
  ID, i, num: Integer;
begin
  Result := False;

  if FBlockCount = 0 then
      exit;

  ID := ID_;
  if (ID < 0) or (ID >= FBlockCount) then
      exit;

  // safe check
  while FBlockBuffer[ID].Prev >= 0 do
    if (ID >= 0) and (ID < FBlockCount) and (FBlockBuffer[ID].UsedSpace > 0) then
        ID := FBlockBuffer[ID].Prev
    else
        exit;

  num := 0;
  i := ID;
  repeat
    if FBlockBuffer[i].UsedSpace = 0 then
        exit;
    inc(num);
    i := FBlockBuffer[i].Next;
    if (i < -1) or (i >= FBlockCount) then
        exit;
  until i < 0;

  Result := True;
end;

function TZDB2_Core_Space.GetSpaceHndID(ID_: Integer): Integer;
var
  ID, i, num: Integer;
begin
  Result := -1;

  if FBlockCount = 0 then
      exit;

  ID := ID_;
  if (ID < 0) or (ID >= FBlockCount) then
      exit;

  // safe check
  while FBlockBuffer[ID].Prev >= 0 do
    if (ID >= 0) and (ID < FBlockCount) and (FBlockBuffer[ID].UsedSpace > 0) then
        ID := FBlockBuffer[ID].Prev
    else
        exit;

  // safe check
  num := 0;
  i := ID;
  repeat
    if FBlockBuffer[i].UsedSpace = 0 then
        exit;
    inc(num);
    i := FBlockBuffer[i].Next;
    if (i < -1) or (i >= FBlockCount) then
        exit;
  until i < 0;

  Result := ID;
end;

function TZDB2_Core_Space.GetSpaceHnd(ID_: Integer): TZDB2_BlockHandle;
var
  ID, i, num: Integer;
begin
  SetLength(Result, 0);

  ID := ID_;
  if (ID < 0) or (ID >= BlockCount) then
      exit;

  // safe check
  while FBlockBuffer[ID].Prev >= 0 do
    if (ID >= 0) and (ID < FBlockCount) and (FBlockBuffer[ID].UsedSpace > 0) then
        ID := FBlockBuffer[ID].Prev
    else
        exit;

  // safe check
  num := 0;
  i := ID;
  repeat
    if FBlockBuffer[i].UsedSpace = 0 then
        exit;
    inc(num);
    i := FBlockBuffer[i].Next;
    if (i < -1) or (i >= FBlockCount) then
        exit;
  until i < 0;

  // extract
  SetLength(Result, num);
  i := ID;
  num := 0;
  repeat
    Result[num] := i;
    inc(num);
    i := FBlockBuffer[i].Next;
  until i < 0;
end;

function TZDB2_Core_Space.GetSpaceHndAsText(ID_: Integer): U_String;
var
  Hnd: TZDB2_BlockHandle;
  i: Integer;
  buff_: U_String;
  m64: TMem64;
begin
  Hnd := GetSpaceHnd(ID_);
  m64 := TMem64.CustomCreate(1024);
  for i in Hnd do
    begin
      buff_ := if_(m64.Position > 0, ', ', '') + umlIntToStr(i);
      if buff_.L > 0 then
          m64.WritePtr(@buff_.buff[0], buff_.L * SystemCharSize);
    end;
  Result.L := m64.Size div SystemCharSize;
  if Result.L > 0 then
      CopyPtr(m64.Memory, @Result.buff[0], Result.L * SystemCharSize);
  SetLength(Hnd, 0);
  DisposeObject(m64);
  buff_ := '';
end;

function TZDB2_Core_Space.GetSpaceHndPtr(ID_: Integer): TZDB2_BlockPtrList;
var
  ID, i, num: Integer;
begin
  Result := nil;

  ID := ID_;
  if (ID < 0) or (ID >= BlockCount) then
      exit;

  // safe check
  while FBlockBuffer[ID].Prev >= 0 do
    if (ID >= 0) and (ID < FBlockCount) and (FBlockBuffer[ID].UsedSpace > 0) then
        ID := FBlockBuffer[ID].Prev
    else
        exit;

  // safe check
  num := 0;
  i := ID;
  repeat
    if FBlockBuffer[i].UsedSpace = 0 then
        exit;
    inc(num);
    i := FBlockBuffer[i].Next;
    if (i < -1) or (i >= FBlockCount) then
        exit;
  until i < 0;

  // extract
  Result := TZDB2_BlockPtrList.Create;
  Result.Count := num;
  i := ID;
  num := 0;
  repeat
    Result[num] := @FBlockBuffer[i];
    inc(num);
    i := FBlockBuffer[i].Next;
  until i < 0;
end;

function TZDB2_Core_Space.CheckWriteSpace(Siz_: Int64): Boolean;
begin
  Result := CheckWriteSpace(Siz_, nil);
end;

function TZDB2_Core_Space.CheckWriteSpace(Siz_: Int64; Space_: TZDB2_BlockPtrList): Boolean;
var
  tmp: Int64;
  i: Integer;
begin
  if Space_ <> nil then
      Space_.Clear;
  Result := False;
  if Siz_ = 0 then
      exit;
  tmp := 0;
  i := FFreeSpaceIndexProbe;
  while (i < FBlockCount) and (tmp < Siz_) do
    with FBlockBuffer[i] do
      begin
        if UsedSpace = 0 then
          begin
            inc(tmp, Size);
            if Space_ <> nil then
                Space_.Add(@FBlockBuffer[i]);
          end;
        inc(i);
      end;
  Result := tmp >= Siz_;
end;

function TZDB2_Core_Space.GetWriteSpaceBlock(): Integer;
var
  i: Integer;
begin
  Result := -1;
  i := FFreeSpaceIndexProbe;
  while i < FBlockCount do
    with FBlockBuffer[i] do
      begin
        if UsedSpace = 0 then
          begin
            FFreeSpaceIndexProbe := i;
            Result := i;
            exit;
          end;
        inc(i);
      end;
end;

function TZDB2_Core_Space.Block_IO_Read(buff: Pointer; ID: Integer): WORD;
var
  p: PZDB2_Block;
begin
  Result := 0;
  if (ID < 0) or (ID >= FBlockCount) then
      exit;
  p := @FBlockBuffer[ID];
  if not ReadCacheBlock(buff, ID) then
    begin
      if not umlFileSeek(FSpace_IOHnd^, p^.Position) then
        begin
          ErrorInfo('Block_IO_Read: umlFileSeek error.');
          exit;
        end;
      if not umlBlockRead(FSpace_IOHnd^, buff^, p^.UsedSpace) then
        begin
          ErrorInfo('Block_IO_Read: umlBlockRead error.');
          exit;
        end;
      DoDecrypt(buff, FBlockBuffer[ID].UsedSpace);
      if FUsedReadCache then
          WriteCacheBlock(buff, p^.UsedSpace, ID, False);
    end;
  Result := p^.UsedSpace;
end;

function TZDB2_Core_Space.Block_IO_Write(buff: Pointer; ID: Integer): Boolean;
var
  p: PZDB2_Block;
begin
  Result := False;
  if (ID < 0) or (ID >= FBlockCount) then
      exit;

  Do_Modification;
  p := @FBlockBuffer[ID];
  if not WriteCacheBlock(buff, p^.UsedSpace, ID, True) then
    begin
      if not umlFileSeek(FSpace_IOHnd^, p^.Position) then
        begin
          ErrorInfo('Block_IO_Write: umlFileSeek Block error.');
          exit;
        end;
      if not umlBlockWrite(FSpace_IOHnd^, DoEncryptTemp(buff, p^.UsedSpace, True)^, p^.UsedSpace) then
        begin
          ErrorInfo('Block_IO_Write: umlBlockWrite Block error.');
          exit;
        end;
      if p^.Size - p^.UsedSpace > 0 then
        if not umlBlockWrite(FSpace_IOHnd^, ZDB2_NULL_Data, p^.Size - p^.UsedSpace) then
          begin
            ErrorInfo('Block_IO_Write: umlBlockWrite (NULL) error.');
            exit;
          end;
    end;
  Result := True;
end;

function TZDB2_Core_Space.Block_IO_Custom_Read(buff: Pointer; ID, Block_Offset, Block_Read_Size: Integer): Boolean;
var
  p: PZDB2_Block;
  SwapBuff_: Pointer;
begin
  Result := False;
  if (ID < 0) or (ID >= FBlockCount) then
      exit;
  if Block_Read_Size <= 0 then
      exit;
  p := @FBlockBuffer[ID];
  if p^.UsedSpace = 0 then
      exit;
  if not umlInRange(Block_Offset, 0, p^.UsedSpace - 1) then
      exit;
  if not umlInRange(Block_Offset + Block_Read_Size, 0, p^.UsedSpace - 1) then
      exit;

  if Check_ReadCache(ID) or (Assigned(Cipher) and (Cipher.Get_CipherSecurity <> TCipherSecurity.csNone)) then
    begin
      SwapBuff_ := System.GetMemory(p^.UsedSpace);
      if Block_IO_Read(SwapBuff_, ID) = p^.UsedSpace then
          CopyPtr(GetOffset(SwapBuff_, Block_Offset), buff, Block_Read_Size);
      System.FreeMemory(SwapBuff_);
    end
  else
    begin
      // optimized IO read
      if not umlFileSeek(FSpace_IOHnd^, p^.Position + Block_Offset) then
        begin
          ErrorInfo('Block_IO_Custom_Read: umlFileSeek error.');
          exit;
        end;
      if not umlBlockRead(FSpace_IOHnd^, buff^, Block_Read_Size) then
        begin
          ErrorInfo('Block_IO_Custom_Read: umlBlockRead error.');
          exit;
        end;
    end;
  Result := True;
end;

function TZDB2_Core_Space.WriteStream(Stream_: TCore_Stream; var SpaceHnd: TZDB2_BlockHandle): Boolean;
var
  Space_: TZDB2_BlockPtrList;
  tmp: Int64;
  i, j: Integer;
  n: TZDB2_BlockPtrList;
  retry: Boolean;
  SwapBuff_: Pointer;
  bakPos_: Int64;
begin
  Result := False;

  if Stream_.Size = 0 then
    begin
      ErrorInfo('WriteStream: Stream size 0.');
      exit;
    end;
  if FSpace_IOHnd^.IsOnlyRead then
    begin
      ErrorInfo('WriteStream: OnlyRead.');
      exit;
    end;

  { compute space }
  Space_ := TZDB2_BlockPtrList.Create;
  if not CheckWriteSpace(Stream_.Size, Space_) then
    begin
      DisposeObject(Space_);
      retry := False;
      if Assigned(FOnNoSpace) then
          FOnNoSpace(Self, Stream_.Size, retry);
      if retry then
          Result := WriteStream(Stream_, SpaceHnd)
      else
          ErrorInfo(PFormat('WriteStream: No Space. source: %d', [Stream_.Size]));
      exit;
    end;

  Do_Modification;

  SetLength(SpaceHnd, Space_.Count);

  { fill block }
  SwapBuff_ := System.GetMemory($FFFF);
  try
    tmp := Stream_.Size;
    bakPos_ := Stream_.Position;
    Stream_.Position := 0;
    i := 0;
    while i < Space_.Count do
      with Space_[i]^ do
        begin
          if tmp > Size then
            begin
              if Stream_.Read(SwapBuff_^, Size) <> Size then
                begin
                  ErrorInfo('WriteStream: read error.');
                  exit;
                end;
              UsedSpace := Size;
              if not WriteCacheBlock(SwapBuff_, Size, ID, True) then
                begin
                  if not umlFileSeek(FSpace_IOHnd^, Position) then
                    begin
                      ErrorInfo('WriteStream: umlFileSeek Block error.');
                      exit;
                    end;
                  DoEncrypt(SwapBuff_, Size);
                  if not umlBlockWrite(FSpace_IOHnd^, SwapBuff_^, Size) then
                    begin
                      ErrorInfo('WriteStream: umlBlockWrite Block error.');
                      exit;
                    end;
                end;

              dec(FState.FreeSpace, Size);

              dec(tmp, Size);
              SpaceHnd[i] := ID;
              inc(i);
            end
          else
            begin
              if Stream_.Read(SwapBuff_^, tmp) <> tmp then
                begin
                  ErrorInfo('WriteStream: read tail error.');
                  exit;
                end;
              UsedSpace := tmp;
              if not WriteCacheBlock(SwapBuff_, tmp, ID, True) then
                begin
                  if not umlFileSeek(FSpace_IOHnd^, Position) then
                    begin
                      ErrorInfo('WriteStream: umlFileSeek tail Block error.');
                      exit;
                    end;
                  DoEncrypt(SwapBuff_, tmp);
                  if not umlBlockWrite(FSpace_IOHnd^, SwapBuff_^, tmp) then
                    begin
                      ErrorInfo('WriteStream: umlBlockWrite tail Block error.');
                      exit;
                    end;
                  if Size - tmp > 0 then
                    if not umlBlockWrite(FSpace_IOHnd^, ZDB2_NULL_Data, Size - tmp) then
                      begin
                        ErrorInfo('WriteStream: umlBlockWrite tail (NULL) error.');
                        exit;
                      end;
                end;

              dec(FState.FreeSpace, Size);

              SpaceHnd[i] := ID;
              inc(i);
              Result := True;
              break;
            end;
        end;
    Stream_.Position := bakPos_;
    DisposeObject(Space_);

    // fill link
    j := 0;
    FBlockBuffer[SpaceHnd[0]].Prev := -1;
    while j < length(SpaceHnd) do
      begin
        if j > 0 then
          begin
            FBlockBuffer[SpaceHnd[j - 1]].Next := SpaceHnd[j];
            FBlockBuffer[SpaceHnd[j]].Prev := SpaceHnd[j - 1];
          end;
        inc(j);
      end;
    FBlockBuffer[SpaceHnd[j - 1]].Next := -1;

    // chagne state
    inc(FState.WriteNum);
    inc(FState.WriteSize, Stream_.Size);

    // prepare probe for next
    FFreeSpaceIndexProbe := FBlockCount;
    i := FBlockBuffer[SpaceHnd[j - 1]].ID + 1;
    while i < FBlockCount do
      with FBlockBuffer[i] do
        begin
          if UsedSpace = 0 then
            begin
              FFreeSpaceIndexProbe := i;
              break;
            end
          else
              inc(i);
        end;
  finally
      System.FreeMemory(SwapBuff_);
  end;
end;

function TZDB2_Core_Space.WriteStream(Stream_: TCore_Stream; var ID: Integer): Boolean;
var
  SpaceHnd: TZDB2_BlockHandle;
begin
  Result := WriteStream(Stream_, SpaceHnd);
  if Result then
      ID := SpaceHnd[0];
  SetLength(SpaceHnd, 0);
end;

function TZDB2_Core_Space.WriteData(buff: TZDB2_Mem; var SpaceHnd: TZDB2_BlockHandle; BuffProtected_: Boolean): Boolean;
var
  Space_: TZDB2_BlockPtrList;
  tmp: Int64;
  i, j: Integer;
  p: Pointer;
  n: TZDB2_BlockPtrList;
  retry: Boolean;
begin
  Result := False;

  if buff.Size = 0 then
    begin
      ErrorInfo('WriteData: buff size 0.');
      exit;
    end;

  if FSpace_IOHnd^.IsOnlyRead then
    begin
      ErrorInfo('WriteData: OnlyRead.');
      exit;
    end;

  { compute space }
  Space_ := TZDB2_BlockPtrList.Create;
  if not CheckWriteSpace(buff.Size, Space_) then
    begin
      DisposeObject(Space_);
      retry := False;
      if Assigned(FOnNoSpace) then
          FOnNoSpace(Self, buff.Size, retry);
      if retry then
          Result := WriteData(buff, SpaceHnd, BuffProtected_)
      else
          ErrorInfo(PFormat('WriteData: No Space. source: %d', [buff.Size]));
      exit;
    end;

  Do_Modification;

  SetLength(SpaceHnd, Space_.Count);

  { fill block }
  tmp := buff.Size;
  p := buff.Memory;
  i := 0;
  while i < Space_.Count do
    with Space_[i]^ do
      begin
        if tmp > Size then
          begin
            UsedSpace := Size;
            if not WriteCacheBlock(p, Size, ID, True) then
              begin
                if not umlFileSeek(FSpace_IOHnd^, Position) then
                  begin
                    ErrorInfo('WriteData: umlFileSeek Block error.');
                    exit;
                  end;
                if not umlBlockWrite(FSpace_IOHnd^, DoEncryptTemp(p, Size, BuffProtected_)^, Size) then
                  begin
                    ErrorInfo('WriteData: umlBlockWrite Block error.');
                    exit;
                  end;
              end;

            dec(FState.FreeSpace, Size);

            dec(tmp, Size);
            p := GetOffset(p, Size);
            SpaceHnd[i] := ID;
            inc(i);
          end
        else
          begin
            UsedSpace := tmp;
            if not WriteCacheBlock(p, tmp, ID, True) then
              begin
                if not umlFileSeek(FSpace_IOHnd^, Position) then
                  begin
                    ErrorInfo('WriteData: umlFileSeek tail Block error.');
                    exit;
                  end;
                if not umlBlockWrite(FSpace_IOHnd^, DoEncryptTemp(p, tmp, BuffProtected_)^, tmp) then
                  begin
                    ErrorInfo('WriteData: umlBlockWrite tail Block error.');
                    exit;
                  end;
                if Size - tmp > 0 then
                  if not umlBlockWrite(FSpace_IOHnd^, ZDB2_NULL_Data, Size - tmp) then
                    begin
                      ErrorInfo('WriteData: umlBlockWrite tail (NULL) error.');
                      exit;
                    end;
              end;

            dec(FState.FreeSpace, Size);

            SpaceHnd[i] := ID;
            inc(i);
            Result := True;
            break;
          end;
      end;

  DisposeObject(Space_);

  // fill link
  j := 0;
  FBlockBuffer[SpaceHnd[0]].Prev := -1;
  while j < length(SpaceHnd) do
    begin
      if j > 0 then
        begin
          FBlockBuffer[SpaceHnd[j - 1]].Next := SpaceHnd[j];
          FBlockBuffer[SpaceHnd[j]].Prev := SpaceHnd[j - 1];
        end;
      inc(j);
    end;
  FBlockBuffer[SpaceHnd[j - 1]].Next := -1;

  // chagne state
  inc(FState.WriteNum);
  inc(FState.WriteSize, buff.Size);

  // prepare probe
  FFreeSpaceIndexProbe := FBlockCount;
  i := FBlockBuffer[SpaceHnd[j - 1]].ID + 1;
  while i < FBlockCount do
    with FBlockBuffer[i] do
      begin
        if UsedSpace = 0 then
          begin
            FFreeSpaceIndexProbe := i;
            break;
          end
        else
            inc(i);
      end;
end;

function TZDB2_Core_Space.WriteData(buff: TZDB2_Mem; var SpaceHnd: TZDB2_BlockHandle): Boolean;
begin
  Result := WriteData(buff, SpaceHnd, True);
end;

function TZDB2_Core_Space.WriteData(buff: TZDB2_Mem; var ID: Integer; BuffProtected_: Boolean): Boolean;
var
  SpaceHnd: TZDB2_BlockHandle;
begin
  Result := WriteData(buff, SpaceHnd, BuffProtected_);
  if Result then
      ID := SpaceHnd[0];
  SetLength(SpaceHnd, 0);
end;

function TZDB2_Core_Space.WriteData(buff: TZDB2_Mem; var ID: Integer): Boolean;
var
  SpaceHnd: TZDB2_BlockHandle;
begin
  Result := WriteData(buff, SpaceHnd);
  if Result then
      ID := SpaceHnd[0];
  SetLength(SpaceHnd, 0);
end;

function TZDB2_Core_Space.Write_Combine_Memory(const arry: TMS64_Array; var SpaceHnd: TZDB2_BlockHandle): Boolean;
var
  L: Integer;
  arry_Size: Int64;
  arry_index: Integer;
  arry_pos: Int64;
  function Do_Read_Arry_Data(const buffer_ptr: Pointer; const Count__: Int64): Int64;
  var
    LCount, R: Int64;
    p: Pointer;
  begin
    Result := 0;
    LCount := Count__;
    p := buffer_ptr;
    R := arry[arry_index].Size - arry_pos;
    // chunk buffer
    while (arry_index < L) and (LCount > R) do
      begin
        CopyPtr(arry[arry_index].PosAsPtr(arry_pos), p, R);
        inc(Result, R);
        p := GetOffset(p, R);
        dec(LCount, R);
        inc(arry_index);
        arry_pos := 0;
        if arry_index < L then
            R := arry[arry_index].Size;
      end;
    // rest buffer
    if (arry_index < L) and (LCount <= R) then
      begin
        CopyPtr(arry[arry_index].PosAsPtr(arry_pos), p, LCount);
        inc(Result, LCount);
        p := GetOffset(p, LCount);
        inc(arry_pos, LCount);
        LCount := 0;
      end;
  end;

var
  Space_: TZDB2_BlockPtrList;
  tmp: Int64;
  i, j: Integer;
  n: TZDB2_BlockPtrList;
  retry: Boolean;
  SwapBuff_: Pointer;
  bakPos_: Int64;
begin
  Result := False;
  if FSpace_IOHnd^.IsOnlyRead then
    begin
      ErrorInfo('Write_Combine_Memory: OnlyRead.');
      exit;
    end;

  L := length(arry);

  { prepare array-stream }
  arry_Size := 0;
  for i := 0 to L - 1 do
      inc(arry_Size, arry[i].Size);
  arry_index := 0;
  arry_pos := 0;

  if arry_Size = 0 then
    begin
      ErrorInfo('Write_Combine_Memory: Stream size 0.');
      exit;
    end;

  { compute space }
  Space_ := TZDB2_BlockPtrList.Create;
  if not CheckWriteSpace(arry_Size, Space_) then
    begin
      DisposeObject(Space_);
      retry := False;
      if Assigned(FOnNoSpace) then
          FOnNoSpace(Self, arry_Size, retry);
      if retry then
          Result := Write_Combine_Memory(arry, SpaceHnd)
      else
          ErrorInfo(PFormat('Write_Combine_Memory: No Space. source: %d', [arry_Size]));
      exit;
    end;

  Do_Modification;

  SetLength(SpaceHnd, Space_.Count);

  { fill block }
  SwapBuff_ := System.GetMemory($FFFF);
  try
    tmp := arry_Size;
    i := 0;
    while i < Space_.Count do
      with Space_[i]^ do
        begin
          if tmp > Size then
            begin
              if Do_Read_Arry_Data(SwapBuff_, Size) <> Size then
                begin
                  ErrorInfo('Write_Combine_Memory: read error.');
                  exit;
                end;
              UsedSpace := Size;
              if not WriteCacheBlock(SwapBuff_, Size, ID, True) then
                begin
                  if not umlFileSeek(FSpace_IOHnd^, Position) then
                    begin
                      ErrorInfo('Write_Combine_Memory: umlFileSeek Block error.');
                      exit;
                    end;
                  DoEncrypt(SwapBuff_, Size);
                  if not umlBlockWrite(FSpace_IOHnd^, SwapBuff_^, Size) then
                    begin
                      ErrorInfo('Write_Combine_Memory: umlBlockWrite Block error.');
                      exit;
                    end;
                end;

              dec(FState.FreeSpace, Size);

              dec(tmp, Size);
              SpaceHnd[i] := ID;
              inc(i);
            end
          else
            begin
              if Do_Read_Arry_Data(SwapBuff_, tmp) <> tmp then
                begin
                  ErrorInfo('Write_Combine_Memory: read tail error.');
                  exit;
                end;
              UsedSpace := tmp;
              if not WriteCacheBlock(SwapBuff_, tmp, ID, True) then
                begin
                  if not umlFileSeek(FSpace_IOHnd^, Position) then
                    begin
                      ErrorInfo('Write_Combine_Memory: umlFileSeek tail Block error.');
                      exit;
                    end;
                  DoEncrypt(SwapBuff_, tmp);
                  if not umlBlockWrite(FSpace_IOHnd^, SwapBuff_^, tmp) then
                    begin
                      ErrorInfo('Write_Combine_Memory: umlBlockWrite tail Block error.');
                      exit;
                    end;
                  if Size - tmp > 0 then
                    if not umlBlockWrite(FSpace_IOHnd^, ZDB2_NULL_Data, Size - tmp) then
                      begin
                        ErrorInfo('Write_Combine_Memory: umlBlockWrite tail (NULL) error.');
                        exit;
                      end;
                end;

              dec(FState.FreeSpace, Size);

              SpaceHnd[i] := ID;
              inc(i);
              Result := True;
              break;
            end;
        end;
    DisposeObject(Space_);

    // fill link
    j := 0;
    FBlockBuffer[SpaceHnd[0]].Prev := -1;
    while j < length(SpaceHnd) do
      begin
        if j > 0 then
          begin
            FBlockBuffer[SpaceHnd[j - 1]].Next := SpaceHnd[j];
            FBlockBuffer[SpaceHnd[j]].Prev := SpaceHnd[j - 1];
          end;
        inc(j);
      end;
    FBlockBuffer[SpaceHnd[j - 1]].Next := -1;

    // chagne state
    inc(FState.WriteNum);
    inc(FState.WriteSize, arry_Size);

    // prepare probe for next
    FFreeSpaceIndexProbe := FBlockCount;
    i := FBlockBuffer[SpaceHnd[j - 1]].ID + 1;
    while i < FBlockCount do
      with FBlockBuffer[i] do
        begin
          if UsedSpace = 0 then
            begin
              FFreeSpaceIndexProbe := i;
              break;
            end
          else
              inc(i);
        end;
  finally
      System.FreeMemory(SwapBuff_);
  end;
end;

function TZDB2_Core_Space.Write_Combine_Memory(const arry: TMS64_Array; var ID: Integer): Boolean;
var
  SpaceHnd: TZDB2_BlockHandle;
begin
  Result := Write_Combine_Memory(arry, SpaceHnd);
  if Result then
      ID := SpaceHnd[0];
  SetLength(SpaceHnd, 0);
end;

function TZDB2_Core_Space.Write_Combine_Memory(const arry: TMem64_Array; var SpaceHnd: TZDB2_BlockHandle): Boolean;
var
  L: Integer;
  arry_Size: Int64;
  arry_index: Integer;
  arry_pos: Int64;
  function Do_Read_Arry_Data(const buffer_ptr: Pointer; const Count__: Int64): Int64;
  var
    LCount, R: Int64;
    p: Pointer;
  begin
    Result := 0;
    LCount := Count__;
    p := buffer_ptr;
    R := arry[arry_index].Size - arry_pos;
    // chunk buffer
    while (arry_index < L) and (LCount > R) do
      begin
        CopyPtr(arry[arry_index].PosAsPtr(arry_pos), p, R);
        inc(Result, R);
        p := GetOffset(p, R);
        dec(LCount, R);
        inc(arry_index);
        arry_pos := 0;
        if arry_index < L then
            R := arry[arry_index].Size;
      end;
    // rest buffer
    if (arry_index < L) and (LCount <= R) then
      begin
        CopyPtr(arry[arry_index].PosAsPtr(arry_pos), p, LCount);
        inc(Result, LCount);
        p := GetOffset(p, LCount);
        inc(arry_pos, LCount);
        LCount := 0;
      end;
  end;

var
  Space_: TZDB2_BlockPtrList;
  tmp: Int64;
  i, j: Integer;
  n: TZDB2_BlockPtrList;
  retry: Boolean;
  SwapBuff_: Pointer;
  bakPos_: Int64;
begin
  Result := False;
  if FSpace_IOHnd^.IsOnlyRead then
    begin
      ErrorInfo('Write_Combine_Memory: OnlyRead.');
      exit;
    end;

  L := length(arry);

  { prepare array-stream }
  arry_Size := 0;
  for i := 0 to L - 1 do
      inc(arry_Size, arry[i].Size);
  arry_index := 0;
  arry_pos := 0;

  if arry_Size = 0 then
    begin
      ErrorInfo('Write_Combine_Memory: Stream size 0.');
      exit;
    end;

  { compute space }
  Space_ := TZDB2_BlockPtrList.Create;
  if not CheckWriteSpace(arry_Size, Space_) then
    begin
      DisposeObject(Space_);
      retry := False;
      if Assigned(FOnNoSpace) then
          FOnNoSpace(Self, arry_Size, retry);
      if retry then
          Result := Write_Combine_Memory(arry, SpaceHnd)
      else
          ErrorInfo(PFormat('Write_Combine_Memory: No Space. source: %d', [arry_Size]));
      exit;
    end;

  Do_Modification;

  SetLength(SpaceHnd, Space_.Count);

  { fill block }
  SwapBuff_ := System.GetMemory($FFFF);
  try
    tmp := arry_Size;
    i := 0;
    while i < Space_.Count do
      with Space_[i]^ do
        begin
          if tmp > Size then
            begin
              if Do_Read_Arry_Data(SwapBuff_, Size) <> Size then
                begin
                  ErrorInfo('Write_Combine_Memory: read error.');
                  exit;
                end;
              UsedSpace := Size;
              if not WriteCacheBlock(SwapBuff_, Size, ID, True) then
                begin
                  if not umlFileSeek(FSpace_IOHnd^, Position) then
                    begin
                      ErrorInfo('Write_Combine_Memory: umlFileSeek Block error.');
                      exit;
                    end;
                  DoEncrypt(SwapBuff_, Size);
                  if not umlBlockWrite(FSpace_IOHnd^, SwapBuff_^, Size) then
                    begin
                      ErrorInfo('Write_Combine_Memory: umlBlockWrite Block error.');
                      exit;
                    end;
                end;

              dec(FState.FreeSpace, Size);

              dec(tmp, Size);
              SpaceHnd[i] := ID;
              inc(i);
            end
          else
            begin
              if Do_Read_Arry_Data(SwapBuff_, tmp) <> tmp then
                begin
                  ErrorInfo('Write_Combine_Memory: read tail error.');
                  exit;
                end;
              UsedSpace := tmp;
              if not WriteCacheBlock(SwapBuff_, tmp, ID, True) then
                begin
                  if not umlFileSeek(FSpace_IOHnd^, Position) then
                    begin
                      ErrorInfo('Write_Combine_Memory: umlFileSeek tail Block error.');
                      exit;
                    end;
                  DoEncrypt(SwapBuff_, tmp);
                  if not umlBlockWrite(FSpace_IOHnd^, SwapBuff_^, tmp) then
                    begin
                      ErrorInfo('Write_Combine_Memory: umlBlockWrite tail Block error.');
                      exit;
                    end;
                  if Size - tmp > 0 then
                    if not umlBlockWrite(FSpace_IOHnd^, ZDB2_NULL_Data, Size - tmp) then
                      begin
                        ErrorInfo('Write_Combine_Memory: umlBlockWrite tail (NULL) error.');
                        exit;
                      end;
                end;

              dec(FState.FreeSpace, Size);

              SpaceHnd[i] := ID;
              inc(i);
              Result := True;
              break;
            end;
        end;
    DisposeObject(Space_);

    // fill link
    j := 0;
    FBlockBuffer[SpaceHnd[0]].Prev := -1;
    while j < length(SpaceHnd) do
      begin
        if j > 0 then
          begin
            FBlockBuffer[SpaceHnd[j - 1]].Next := SpaceHnd[j];
            FBlockBuffer[SpaceHnd[j]].Prev := SpaceHnd[j - 1];
          end;
        inc(j);
      end;
    FBlockBuffer[SpaceHnd[j - 1]].Next := -1;

    // chagne state
    inc(FState.WriteNum);
    inc(FState.WriteSize, arry_Size);

    // prepare probe for next
    FFreeSpaceIndexProbe := FBlockCount;
    i := FBlockBuffer[SpaceHnd[j - 1]].ID + 1;
    while i < FBlockCount do
      with FBlockBuffer[i] do
        begin
          if UsedSpace = 0 then
            begin
              FFreeSpaceIndexProbe := i;
              break;
            end
          else
              inc(i);
        end;
  finally
      System.FreeMemory(SwapBuff_);
  end;
end;

function TZDB2_Core_Space.Write_Combine_Memory(const arry: TMem64_Array; var ID: Integer): Boolean;
var
  SpaceHnd: TZDB2_BlockHandle;
begin
  Result := Write_Combine_Memory(arry, SpaceHnd);
  if Result then
      ID := SpaceHnd[0];
  SetLength(SpaceHnd, 0);
end;

function TZDB2_Core_Space.Write_Combine_Stream(const arry: TStream_Array; var SpaceHnd: TZDB2_BlockHandle): Boolean;
var
  L: Integer;
  arry_backup_pos: array of Int64;
  arry_Size: Int64;
  arry_index: Integer;
  arry_pos: Int64;
  function Do_Read_Arry_Data(const buffer_ptr: Pointer; const Count__: Int64): Int64;
  var
    LCount, R: Int64;
    p: Pointer;
  begin
    Result := 0;
    LCount := Count__;
    p := buffer_ptr;
    R := arry[arry_index].Size - arry_pos;
    // chunk buffer
    while (arry_index < L) and (LCount > R) do
      begin
        arry[arry_index].Position := arry_pos;
        inc(Result, arry[arry_index].Read(p^, R));
        p := GetOffset(p, R);
        dec(LCount, R);
        inc(arry_index);
        arry_pos := 0;
        if arry_index < L then
            R := arry[arry_index].Size;
      end;
    // rest buffer
    if (arry_index < L) and (LCount <= R) then
      begin
        arry[arry_index].Position := arry_pos;
        inc(Result, arry[arry_index].Read(p^, LCount));
        p := GetOffset(p, LCount);
        inc(arry_pos, LCount);
        LCount := 0;
      end;
  end;

var
  Space_: TZDB2_BlockPtrList;
  tmp: Int64;
  i, j: Integer;
  n: TZDB2_BlockPtrList;
  retry: Boolean;
  SwapBuff_: Pointer;
  bakPos_: Int64;
begin
  Result := False;
  if FSpace_IOHnd^.IsOnlyRead then
    begin
      ErrorInfo('Write_Combine_Stream: OnlyRead.');
      exit;
    end;

  L := length(arry);

  { prepare array-stream }
  SetLength(arry_backup_pos, L);
  arry_Size := 0;
  for i := 0 to L - 1 do
    begin
      arry_backup_pos[i] := arry[i].Position;
      inc(arry_Size, arry[i].Size);
    end;
  arry_index := 0;
  arry_pos := 0;

  if arry_Size = 0 then
    begin
      ErrorInfo('Write_Combine_Stream: Stream size 0.');
      exit;
    end;

  { compute space }
  Space_ := TZDB2_BlockPtrList.Create;
  if not CheckWriteSpace(arry_Size, Space_) then
    begin
      DisposeObject(Space_);
      retry := False;
      if Assigned(FOnNoSpace) then
          FOnNoSpace(Self, arry_Size, retry);
      if retry then
          Result := Write_Combine_Stream(arry, SpaceHnd)
      else
          ErrorInfo(PFormat('Write_Combine_Stream: No Space. source: %d', [arry_Size]));
      exit;
    end;

  Do_Modification;

  SetLength(SpaceHnd, Space_.Count);

  { fill block }
  SwapBuff_ := System.GetMemory($FFFF);
  try
    tmp := arry_Size;
    i := 0;
    while i < Space_.Count do
      with Space_[i]^ do
        begin
          if tmp > Size then
            begin
              if Do_Read_Arry_Data(SwapBuff_, Size) <> Size then
                begin
                  ErrorInfo('Write_Combine_Stream: read error.');
                  exit;
                end;
              UsedSpace := Size;
              if not WriteCacheBlock(SwapBuff_, Size, ID, True) then
                begin
                  if not umlFileSeek(FSpace_IOHnd^, Position) then
                    begin
                      ErrorInfo('Write_Combine_Stream: umlFileSeek Block error.');
                      exit;
                    end;
                  DoEncrypt(SwapBuff_, Size);
                  if not umlBlockWrite(FSpace_IOHnd^, SwapBuff_^, Size) then
                    begin
                      ErrorInfo('Write_Combine_Stream: umlBlockWrite Block error.');
                      exit;
                    end;
                end;

              dec(FState.FreeSpace, Size);

              dec(tmp, Size);
              SpaceHnd[i] := ID;
              inc(i);
            end
          else
            begin
              if Do_Read_Arry_Data(SwapBuff_, tmp) <> tmp then
                begin
                  ErrorInfo('Write_Combine_Stream: read tail error.');
                  exit;
                end;
              UsedSpace := tmp;
              if not WriteCacheBlock(SwapBuff_, tmp, ID, True) then
                begin
                  if not umlFileSeek(FSpace_IOHnd^, Position) then
                    begin
                      ErrorInfo('Write_Combine_Stream: umlFileSeek tail Block error.');
                      exit;
                    end;
                  DoEncrypt(SwapBuff_, tmp);
                  if not umlBlockWrite(FSpace_IOHnd^, SwapBuff_^, tmp) then
                    begin
                      ErrorInfo('Write_Combine_Stream: umlBlockWrite tail Block error.');
                      exit;
                    end;
                  if Size - tmp > 0 then
                    if not umlBlockWrite(FSpace_IOHnd^, ZDB2_NULL_Data, Size - tmp) then
                      begin
                        ErrorInfo('Write_Combine_Stream: umlBlockWrite tail (NULL) error.');
                        exit;
                      end;
                end;

              dec(FState.FreeSpace, Size);

              SpaceHnd[i] := ID;
              inc(i);
              Result := True;
              break;
            end;
        end;
    // restore backup position
    for i := 0 to L - 1 do
        arry[i].Position := arry_backup_pos[i];
    DisposeObject(Space_);

    // fill link
    j := 0;
    FBlockBuffer[SpaceHnd[0]].Prev := -1;
    while j < length(SpaceHnd) do
      begin
        if j > 0 then
          begin
            FBlockBuffer[SpaceHnd[j - 1]].Next := SpaceHnd[j];
            FBlockBuffer[SpaceHnd[j]].Prev := SpaceHnd[j - 1];
          end;
        inc(j);
      end;
    FBlockBuffer[SpaceHnd[j - 1]].Next := -1;

    // chagne state
    inc(FState.WriteNum);
    inc(FState.WriteSize, arry_Size);

    // prepare probe for next
    FFreeSpaceIndexProbe := FBlockCount;
    i := FBlockBuffer[SpaceHnd[j - 1]].ID + 1;
    while i < FBlockCount do
      with FBlockBuffer[i] do
        begin
          if UsedSpace = 0 then
            begin
              FFreeSpaceIndexProbe := i;
              break;
            end
          else
              inc(i);
        end;
  finally
      System.FreeMemory(SwapBuff_);
  end;
end;

function TZDB2_Core_Space.Write_Combine_Stream(const arry: TStream_Array; var ID: Integer): Boolean;
var
  SpaceHnd: TZDB2_BlockHandle;
begin
  Result := Write_Combine_Stream(arry, SpaceHnd);
  if Result then
      ID := SpaceHnd[0];
  SetLength(SpaceHnd, 0);
end;

function TZDB2_Core_Space.ReadStream(Stream_: TCore_Stream; SpaceHnd: TZDB2_BlockHandle): Boolean;
var
  i: Integer;
  Siz_: Int64;
  SwapBuff_: Pointer;
  bak_pos: Int64;
begin
  Result := False;

  if length(SpaceHnd) = 0 then
    begin
      ErrorInfo('ReadStream: SpaceHnd null error.');
      exit;
    end;

  bak_pos := Stream_.Position;

  { compute queue space }
  i := 0;
  Siz_ := 0;
  while i < length(SpaceHnd) do
    with FBlockBuffer[SpaceHnd[i]] do
      begin
        inc(Siz_, UsedSpace);
        inc(i);
      end;

  if Siz_ = 0 then
      exit;

  { read }
  i := 0;
  SwapBuff_ := System.GetMemory($FFFF);
  try
    while i < length(SpaceHnd) do
      with FBlockBuffer[SpaceHnd[i]] do
        begin
          if not ReadCacheBlock(SwapBuff_, ID) then
            begin
              if not umlFileSeek(FSpace_IOHnd^, Position) then
                begin
                  ErrorInfo('ReadStream: umlFileSeek error.');
                  exit;
                end;
              if not umlBlockRead(FSpace_IOHnd^, SwapBuff_^, UsedSpace) then
                begin
                  ErrorInfo('ReadStream: umlBlockRead error.');
                  exit;
                end;
              DoDecrypt(SwapBuff_, UsedSpace);
              if FUsedReadCache then
                  WriteCacheBlock(SwapBuff_, UsedSpace, ID, False);
            end;
          if Stream_.Write(SwapBuff_^, UsedSpace) <> UsedSpace then
            begin
              ErrorInfo('ReadStream: write error.');
              exit;
            end;
          inc(i);
        end;

    inc(FState.ReadNum);
    inc(FState.ReadSize, Siz_);
    Result := True;
    Stream_.Position := bak_pos;
  finally
      System.FreeMemory(SwapBuff_);
  end;
end;

function TZDB2_Core_Space.ReadStream(Stream_: TCore_Stream; ID: Integer): Boolean;
begin
  Result := ReadStream(Stream_, GetSpaceHnd(ID));
end;

function TZDB2_Core_Space.ReadData(buff: TZDB2_Mem; SpaceHnd: TZDB2_BlockHandle): Boolean;
var
  i: Integer;
  Siz_: Int64;
  p: Pointer;
  bak_pos: Int64;
begin
  Result := False;

  if length(SpaceHnd) = 0 then
    begin
      ErrorInfo('ReadData: SpaceHnd null error.');
      exit;
    end;

  { compute queue space }
  i := 0;
  Siz_ := 0;
  while i < length(SpaceHnd) do
    with FBlockBuffer[SpaceHnd[i]] do
      begin
        inc(Siz_, UsedSpace);
        inc(i);
      end;

  { prepare memory }
  buff.Size := Siz_;

  if Siz_ = 0 then
      exit;

  bak_pos := buff.Position;

  { read }
  i := 0;
  p := buff.Memory;
  while i < length(SpaceHnd) do
    with FBlockBuffer[SpaceHnd[i]] do
      begin
        if not ReadCacheBlock(p, ID) then
          begin
            if not umlFileSeek(FSpace_IOHnd^, Position) then
              begin
                ErrorInfo('ReadData: umlFileSeek error.');
                exit;
              end;
            if not umlBlockRead(FSpace_IOHnd^, p^, UsedSpace) then
              begin
                ErrorInfo('ReadData: umlBlockRead error.');
                exit;
              end;
            DoDecrypt(p, UsedSpace);
            if FUsedReadCache then
                WriteCacheBlock(p, UsedSpace, ID, False);
          end;
        p := GetOffset(p, UsedSpace);
        inc(i);
      end;

  inc(FState.ReadNum);
  inc(FState.ReadSize, Siz_);
  Result := True;
  buff.Position := bak_pos;
end;

function TZDB2_Core_Space.ReadData(buff: TZDB2_Mem; ID: Integer): Boolean;
begin
  Result := ReadData(buff, GetSpaceHnd(ID));
end;

function TZDB2_Core_Space.Compute_Data_Position(SpaceHnd: TZDB2_BlockHandle; const Pos_: Int64; var BlockID_: Integer; var BlockPos_: Int64): Boolean;
var
  L, i, ID: Integer;
  cp: Int64; // compute pos
begin
  Result := False;
  if (Pos_ < 0) then
      exit;
  cp := 0;
  L := length(SpaceHnd);

  for i := 0 to L - 1 do
    begin
      ID := SpaceHnd[i];
      if (Pos_ >= cp) and (Pos_ <= cp + FBlockBuffer[ID].UsedSpace) then
        begin
          BlockPos_ := Pos_ - cp;
          BlockID_ := ID;
          Result := True;
          exit;
        end;
      inc(cp, FBlockBuffer[ID].UsedSpace);
    end;
end;

function TZDB2_Core_Space.Compute_Data_Range(SpaceHnd: TZDB2_BlockHandle; const Pos_, Size_: Int64; var Range_Space: TZDB2_ID_List; var BeginBlockPos_, EndBlockPos_: Int64): Boolean;
var
  L, i, j, ID: Integer;
  cp: Int64;   // compute pos
  ePos: Int64; // end pos
begin
  Result := False;
  if (Pos_ < 0) or (Size_ < 0) then
      exit;
  Range_Space.Clear;
  BeginBlockPos_ := 0;
  EndBlockPos_ := 0;
  cp := 0;
  ePos := Pos_ + Size_;
  L := length(SpaceHnd);

  for i := 0 to L - 1 do
    begin
      ID := SpaceHnd[i];
      if (Pos_ >= cp) and (Pos_ <= cp + FBlockBuffer[ID].UsedSpace) then
        begin
          BeginBlockPos_ := Pos_ - cp;
          for j := i to L - 1 do
            begin
              ID := SpaceHnd[j];
              Range_Space.Add(ID);
              if (ePos >= cp) and (ePos <= cp + FBlockBuffer[ID].UsedSpace) then
                begin
                  EndBlockPos_ := ePos - cp;
                  Result := True;
                  exit;
                end;
              inc(cp, FBlockBuffer[ID].UsedSpace);
            end;
          EndBlockPos_ := FBlockBuffer[SpaceHnd[L - 1]].UsedSpace;
          Result := True;
          exit;
        end;
      inc(cp, FBlockBuffer[ID].UsedSpace);
    end;
end;

function TZDB2_Core_Space.Read_Position(Stream_: TCore_Stream; SpaceHnd: TZDB2_BlockHandle; Begin_Position, Read_Size: Int64): Int64;
var
  Range_Space: TZDB2_ID_List;
  BeginBlockPos_, EndBlockPos_: Int64;
  i: Integer;
  Siz_: Int64;
  SwapBuff_: Pointer;
  bak_: Int64;
begin
  Result := 0;

  if Read_Size <= 0 then
      exit;

  if length(SpaceHnd) = 0 then
    begin
      ErrorInfo('Read_Position: SpaceHnd null error.');
      exit;
    end;

  Range_Space := TZDB2_ID_List.Create;
  if Compute_Data_Range(SpaceHnd, Begin_Position, Read_Size, Range_Space, BeginBlockPos_, EndBlockPos_) then
    begin
      bak_ := Stream_.Position;
      { alloc buffer }
      SwapBuff_ := System.GetMemory($FFFF);
      try
        if Range_Space.Count > 1 then
          begin
            { read begin body }
            with FBlockBuffer[Range_Space.First] do
              begin
                if not ReadCacheBlock(SwapBuff_, ID) then
                  begin
                    if not umlFileSeek(FSpace_IOHnd^, Position) then
                      begin
                        ErrorInfo('Read_Position: umlFileSeek error.');
                        exit;
                      end;
                    if not umlBlockRead(FSpace_IOHnd^, SwapBuff_^, UsedSpace) then
                      begin
                        ErrorInfo('Read_Position: umlBlockRead error.');
                        exit;
                      end;
                    DoDecrypt(SwapBuff_, UsedSpace);
                    if FUsedReadCache then
                        WriteCacheBlock(SwapBuff_, UsedSpace, ID, False);
                  end;
                if Stream_.Write(GetOffset(SwapBuff_, BeginBlockPos_)^, UsedSpace - BeginBlockPos_) <> UsedSpace - BeginBlockPos_ then
                  begin
                    ErrorInfo('Read_Position: write error.');
                    exit;
                  end;
                inc(Result, UsedSpace - BeginBlockPos_);
              end;

            { read sequence body }
            i := 1;
            while i < Range_Space.Count - 1 do
              with FBlockBuffer[Range_Space[i]] do
                begin
                  if not ReadCacheBlock(SwapBuff_, ID) then
                    begin
                      if not umlFileSeek(FSpace_IOHnd^, Position) then
                        begin
                          ErrorInfo('Read_Position: umlFileSeek error.');
                          exit;
                        end;
                      if not umlBlockRead(FSpace_IOHnd^, SwapBuff_^, UsedSpace) then
                        begin
                          ErrorInfo('Read_Position: umlBlockRead error.');
                          exit;
                        end;
                      DoDecrypt(SwapBuff_, UsedSpace);
                      if FUsedReadCache then
                          WriteCacheBlock(SwapBuff_, UsedSpace, ID, False);
                    end;
                  if Stream_.Write(SwapBuff_^, UsedSpace) <> UsedSpace then
                    begin
                      ErrorInfo('Read_Position: write error.');
                      exit;
                    end;
                  inc(Result, UsedSpace);
                  inc(i);
                end;

            with FBlockBuffer[Range_Space.Last] do
              begin
                if not ReadCacheBlock(SwapBuff_, ID) then
                  begin
                    if not umlFileSeek(FSpace_IOHnd^, Position) then
                      begin
                        ErrorInfo('Read_Position: umlFileSeek error.');
                        exit;
                      end;
                    if not umlBlockRead(FSpace_IOHnd^, SwapBuff_^, UsedSpace) then
                      begin
                        ErrorInfo('Read_Position: umlBlockRead error.');
                        exit;
                      end;
                    DoDecrypt(SwapBuff_, UsedSpace);
                    if FUsedReadCache then
                        WriteCacheBlock(SwapBuff_, UsedSpace, ID, False);
                  end;
                if Stream_.Write(SwapBuff_^, EndBlockPos_) <> EndBlockPos_ then
                  begin
                    ErrorInfo('Read_Position: write error.');
                    exit;
                  end;
                inc(Result, EndBlockPos_);
              end;
          end
        else if Range_Space.Count > 0 then
          begin
            // read block
            with FBlockBuffer[Range_Space.First] do
              begin
                if not ReadCacheBlock(SwapBuff_, ID) then
                  begin
                    if not umlFileSeek(FSpace_IOHnd^, Position) then
                      begin
                        ErrorInfo('Read_Position: umlFileSeek error.');
                        exit;
                      end;
                    if not umlBlockRead(FSpace_IOHnd^, SwapBuff_^, UsedSpace) then
                      begin
                        ErrorInfo('Read_Position: umlBlockRead error.');
                        exit;
                      end;
                    DoDecrypt(SwapBuff_, UsedSpace);
                    if FUsedReadCache then
                        WriteCacheBlock(SwapBuff_, UsedSpace, ID, False);
                  end;
                if Stream_.Write(GetOffset(SwapBuff_, BeginBlockPos_)^, EndBlockPos_ - BeginBlockPos_) <> EndBlockPos_ - BeginBlockPos_ then
                  begin
                    ErrorInfo('Read_Position: write error.');
                    exit;
                  end;
                inc(Result, EndBlockPos_ - BeginBlockPos_);
              end;
          end
        else
          begin
            ErrorInfo('Read_Position: SearchDataRange return Range_Space is NULL.');
            exit;
          end;

        inc(FState.ReadNum);
        inc(FState.ReadSize, Result);
        Stream_.Position := bak_;
      finally
        System.FreeMemory(SwapBuff_);
        DisposeObject(Range_Space);
      end;
    end
  else
      DisposeObject(Range_Space);
end;

function TZDB2_Core_Space.Read_Position(Stream_: TCore_Stream; ID: Integer; Begin_Position, Read_Size: Int64): Int64;
begin
  Result := Read_Position(Stream_, GetSpaceHnd(ID), Begin_Position, Read_Size);
end;

function TZDB2_Core_Space.ComputeMD5(SpaceHnd: TZDB2_BlockHandle; var MD5: TMD5): Boolean;
var
  i: Integer;
  Siz_: Int64;
  SwapBuff_: Pointer;
  MD5_Tool: TMD5_Tool;
begin
  Result := False;

  if length(SpaceHnd) = 0 then
    begin
      ErrorInfo('ReadStream: SpaceHnd null error.');
      exit;
    end;

  { compute queue space }
  i := 0;
  Siz_ := 0;
  while i < length(SpaceHnd) do
    with FBlockBuffer[SpaceHnd[i]] do
      begin
        inc(Siz_, UsedSpace);
        inc(i);
      end;

  if Siz_ = 0 then
      exit;

  { read }
  i := 0;
  MD5_Tool := TMD5_Tool.Create;
  SwapBuff_ := System.GetMemory($FFFF);
  try
    while i < length(SpaceHnd) do
      with FBlockBuffer[SpaceHnd[i]] do
        begin
          if not ReadCacheBlock(SwapBuff_, ID) then
            begin
              if not umlFileSeek(FSpace_IOHnd^, Position) then
                begin
                  ErrorInfo('ComputeMD5: umlFileSeek error.');
                  exit;
                end;
              if not umlBlockRead(FSpace_IOHnd^, SwapBuff_^, UsedSpace) then
                begin
                  ErrorInfo('ComputeMD5: umlBlockRead error.');
                  exit;
                end;
              DoDecrypt(SwapBuff_, UsedSpace);
              if FUsedReadCache then
                  WriteCacheBlock(SwapBuff_, UsedSpace, ID, False);
            end;
          MD5_Tool.Update(SwapBuff_, UsedSpace);
          inc(i);
        end;

    MD5 := MD5_Tool.FinalizeMD5;
    inc(FState.ReadNum);
    inc(FState.ReadSize, Siz_);
    Result := True;
  finally
    System.FreeMemory(SwapBuff_);
    DisposeObject(MD5_Tool);
  end;
end;

function TZDB2_Core_Space.ComputeMD5(ID: Integer; var MD5: TMD5): Boolean;
begin
  Result := ComputeMD5(GetSpaceHnd(ID), MD5);
end;

function TZDB2_Core_Space.RemoveData(SpaceHnd: TZDB2_BlockHandle; SafeClean_: Boolean): Boolean;
var
  i: Integer;
begin
  if FSpace_IOHnd^.IsOnlyRead then
    begin
      ErrorInfo('RemoveData: OnlyRead.');
      Result := False;
      exit;
    end;
  Result := (length(SpaceHnd) > 0) and Check(SpaceHnd[0]);
  i := 0;
  while i < length(SpaceHnd) do
    with FBlockBuffer[SpaceHnd[i]] do
      begin
        DeleteCache(ID);
        UsedSpace := 0;
        Prev := -1;
        Next := -1;

        if ID < FFreeSpaceIndexProbe then
            FFreeSpaceIndexProbe := ID;
        inc(FState.FreeSpace, Size);

        { safe remove }
        if SafeClean_ then
          begin
            if not umlFileSeek(FSpace_IOHnd^, Position) then
              begin
                ErrorInfo('RemoveData: umlFileSeek error.');
                exit;
              end;
            if not umlBlockWrite(FSpace_IOHnd^, ZDB2_NULL_Data, Size) then
              begin
                ErrorInfo('RemoveData: umlBlockWrite error.');
                exit;
              end;
          end;

        inc(i);
      end;
  Do_Modification;
end;

function TZDB2_Core_Space.RemoveData(ID: Integer; SafeClean_: Boolean): Boolean;
begin
  Result := RemoveData(GetSpaceHnd(ID), SafeClean_);
end;

function TZDB2_Core_Space.GetDataSize(SpaceHnd: TZDB2_BlockHandle): Int64;
var
  i: Integer;
begin
  Result := 0;
  i := 0;
  while i < length(SpaceHnd) do
    with FBlockBuffer[SpaceHnd[i]] do
      begin
        inc(Result, UsedSpace);
        inc(i);
      end;
end;

function TZDB2_Core_Space.GetDataSize(ID: Integer): Int64;
begin
  Result := GetDataSize(GetSpaceHnd(ID));
end;

function TZDB2_Core_Space.GetDataPhysics(SpaceHnd: TZDB2_BlockHandle): Int64;
var
  i: Integer;
begin
  Result := 0;
  i := 0;
  while i < length(SpaceHnd) do
    with FBlockBuffer[SpaceHnd[i]] do
      begin
        inc(Result, Size);
        inc(i);
      end;
end;

function TZDB2_Core_Space.GetDataPhysics(ID: Integer): Int64;
begin
  Result := GetDataPhysics(GetSpaceHnd(ID));
end;

function TZDB2_Core_Space.BuildTableID: TZDB2_BlockHandle;
var
  i, j: Integer;
  LBuff: array of Boolean;
  tmp: TZDB2_BlockHandle;
  L: TZDB2_ID_List;
begin
  SetLength(LBuff, FBlockCount);
  L := TZDB2_ID_List.Create;
  try
    for i := 0 to FBlockCount - 1 do
      if (not LBuff[i]) and (FBlockBuffer[i].UsedSpace > 0) then
        begin
          tmp := GetSpaceHnd(FBlockBuffer[i].ID);
          for j := 0 to length(tmp) - 1 do
              LBuff[tmp[j]] := True;
          L.Add(tmp[0]);
        end;
    SetLength(Result, L.Count);
    for i := 0 to L.Count - 1 do
        Result[i] := L[i];
  except
      SetLength(Result, 0);
  end;
  DisposeObject(L);
  SetLength(LBuff, 0);
end;

procedure TZDB2_Core_Space.Format_Space();
var
  i: Integer;
begin
  FillPtr(@FHeader.UserCustomHeader, SizeOf(TZDB2_UserCustomHeader), 0);
  for i := 0 to FBlockCount - 1 do
    begin
      FBlockBuffer[i].UsedSpace := 0;
      FBlockBuffer[i].Next := -1;
      FBlockBuffer[i].Prev := -1;
      FBlockBuffer[i].ID := i;
    end;
  Do_Modification;
end;

procedure TZDB2_Core_Space.DoProgress(Total_, current_: Integer);
begin
  if Assigned(FOnProgress) then
      FOnProgress(Total_, current_);
end;

class function TZDB2_Core_Space.Combine_Handle(hnd1, hnd2: TZDB2_BlockHandle): TZDB2_BlockHandle;
var
  i, j: Integer;
begin
  SetLength(Result, length(hnd1) + length(hnd2));
  j := 0;
  for i := low(hnd1) to high(hnd1) do
    begin
      Result[j] := hnd1[i];
      inc(j);
    end;
  for i := low(hnd2) to high(hnd2) do
    begin
      Result[j] := hnd2[i];
      inc(j);
    end;
end;

class function TZDB2_Core_Space.Combine_Handle(L1, L2: TZDB2_ID_List): TZDB2_BlockHandle;
var
  i, j: Integer;
begin
  SetLength(Result, L1.Count + L2.Count);
  j := 0;
  for i := 0 to L1.Count - 1 do
    begin
      Result[j] := L1[i];
      inc(j);
    end;
  for i := 0 to L2.Count - 1 do
    begin
      Result[j] := L2[i];
      inc(j);
    end;
end;

class function TZDB2_Core_Space.Get_Handle(Hnd: TZDB2_BlockHandle): TZDB2_BlockHandle;
var
  i: Integer;
begin
  SetLength(Result, length(Hnd));
  for i := low(Hnd) to high(Hnd) do
      Result[i] := Hnd[i];
end;

class function TZDB2_Core_Space.Get_Handle(L: TZDB2_ID_List): TZDB2_BlockHandle;
var
  i: Integer;
begin
  SetLength(Result, L.Count);
  for i := 0 to L.Count - 1 do
      Result[i] := L[i];
end;

class function TZDB2_Core_Space.Get_Handle(L: TZDB2_ID_Pool): TZDB2_BlockHandle;
begin
  SetLength(Result, L.num);
  if L.num > 0 then
    with L.Repeat_ do
      repeat
          Result[I__] := queue^.Data;
      until not Next;
end;

class procedure TZDB2_Core_Space.Test;
type
  TTest_ = record
    Data: TMS64;
    sMD5: TMD5;
    db1hnd, db2hnd: TZDB2_BlockHandle;
  end;

  PTest_ = ^TTest_;

  TTestList_ = TGenericsList<PTest_>;

var
  Cipher_: TZDB2_Cipher;
  TestArry: array [0 .. 5] of TTest_;
  testList: TTestList_;
  p: PTest_;
  hnd1, hnd2: TIOHnd;
  db1, db2: TZDB2_Core_Space;
  db1_place: TZDB2_Space_Planner;
  hnd1_1, hnd2_2: TIOHnd;
  db1_1, db2_2: TZDB2_Core_Space;
  i: Integer;
  db1_crc16: TZDB2_CRC16;
  db1Hnd_, db2Hnd_: TZDB2_BlockHandle;
begin
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csRijndael, 'hello world.', 1, False, True);

  testList := TTestList_.Create;

  InitIOHnd(hnd1);
  umlFileCreateAsMemory(hnd1);
  hnd1.Cache.UsedWriteCache := True;
  hnd1.Cache.UsedReadCache := True;

  InitIOHnd(hnd2);
  umlFileCreateAsMemory(hnd2);
  hnd2.Cache.UsedWriteCache := True;
  hnd2.Cache.UsedReadCache := True;

  db1 := TZDB2_Core_Space.Create(@hnd1);
  db1.AutoCloseIOHnd := True;
  db1.Cipher := Cipher_;
  db1_place := TZDB2_Space_Planner.Create(db1);

  db2 := TZDB2_Core_Space.Create(@hnd2);
  db2.Cipher := Cipher_;
  for i := 1 to 15 do
      db2.AppendSpace(1 * 1024 * 1024, 512);
  db2.AutoCloseIOHnd := True;

  for i := 0 to length(TestArry) - 1 do
    begin
      SetMT19937Seed(i);
      TestArry[i].Data := TMS64.Create;
      TestArry[i].Data.Size := 1024 * 16 + 1024 * 1024;
      MT19937Rand32(MaxInt, TestArry[i].Data.Memory, TestArry[i].Data.Size div 4);
      TestArry[i].sMD5 := umlStreamMD5(TestArry[i].Data);

      if db1_place.WriteStream(TestArry[i].Data, 1024, TestArry[i].db1hnd) then
          DoStatus('write TestArry[%d] ok', [i])
      else
          DoStatus('write TestArry[%d] failed', [i]);

      if db2.WriteStream(TestArry[i].Data, TestArry[i].db2hnd) then
          DoStatus('write TestArry[%d] ok', [i])
      else
          DoStatus('write TestArry[%d] failed', [i]);
      testList.Add(@TestArry[i]);
    end;

  db1_place.Flush;
  DisposeObject(db1_place);
  db1.Flush;

  db1Hnd_ := db1.BuildTableID;
  if length(db1Hnd_) = length(TestArry) then
    begin
      for i := 0 to length(TestArry) - 1 do
        begin
          if TestArry[i].db1hnd[0] = db1Hnd_[i] then
              DoStatus('BuildTableID verify successed!!', [])
          else
              DoStatus('BuildTableID verify error!!', []);
        end
    end
  else
      DoStatus('BuildTableID error!!', []);

  db2Hnd_ := db2.BuildTableID;
  if length(db2Hnd_) = length(TestArry) then
    begin
      for i := 0 to length(TestArry) - 1 do
        begin
          if TestArry[i].db2hnd[0] = db2Hnd_[i] then
              DoStatus('BuildTableID verify successed!!', [])
          else
              DoStatus('BuildTableID verify error!!', []);
        end
    end
  else
      DoStatus('BuildTableID error!!', []);

  db1_crc16 := TZDB2_CRC16.Create;
  db1_crc16.Build(db1);
  DisposeObject(db1_crc16);

  InitIOHnd(hnd1_1);
  umlFileCreateAsMemory(hnd1_1);
  if db1.OptimizedSpaceTo(hnd1_1) then
      DoStatus('optmized space test ok.')
  else
      DoStatus('optmized space test error.');
  db1_1 := TZDB2_Core_Space.Create(@hnd1_1);
  db1_1.Cipher := Cipher_;
  db1_1.AutoCloseIOHnd := True;
  db1_1.Open;

  InitIOHnd(hnd2_2);
  umlFileCreateAsMemory(hnd2_2);
  if db2.OptimizedSpaceTo(hnd2_2) then
      DoStatus('optmized space test ok.')
  else
      DoStatus('optmized space test error.');
  db2_2 := TZDB2_Core_Space.Create(@hnd2_2);
  db2_2.Cipher := Cipher_;
  db2_2.AutoCloseIOHnd := True;
  db2_2.Open;

  for i := 0 to testList.Count - 1 do
    begin
      p := testList[i];
      p^.Data.Clear;

      if db1.ReadStream(p^.Data, p^.db1hnd[0]) then
          DoStatus('read test ok')
      else
          DoStatus('read test failed');

      if umlCompareMD5(umlStreamMD5(p^.Data), p^.sMD5) then
          DoStatus('md5 verify ok')
      else
          DoStatus('md5 verify failed');

      if db1.RemoveData(p^.db1hnd, True) then
          DoStatus('remove test ok')
      else
          DoStatus('remove test failed');

      p^.Data.Clear;
      if db2.ReadStream(p^.Data, p^.db2hnd) then
          DoStatus('read test ok')
      else
          DoStatus('read test failed');

      if umlCompareMD5(umlStreamMD5(p^.Data), p^.sMD5) then
          DoStatus('md5 verify ok')
      else
          DoStatus('md5 verify failed');

      if db2.RemoveData(p^.db2hnd, True) then
          DoStatus('remove test ok')
      else
          DoStatus('remove test failed');
    end;

  for i := 0 to testList.Count - 1 do
    begin
      p := testList[i];
      p^.Data.Clear;
      if db1_1.ReadStream(p^.Data, p^.db1hnd) then
          DoStatus('read test ok')
      else
          DoStatus('read test failed');

      if umlCompareMD5(umlStreamMD5(p^.Data), p^.sMD5) then
          DoStatus('md5 verify ok')
      else
          DoStatus('md5 verify failed');

      if db1_1.RemoveData(p^.db1hnd, True) then
          DoStatus('remove test ok')
      else
          DoStatus('remove test failed');

      p^.Data.Clear;
      if db2_2.ReadStream(p^.Data, p^.db2hnd) then
          DoStatus('read test ok')
      else
          DoStatus('read test failed');

      if umlCompareMD5(umlStreamMD5(p^.Data), p^.sMD5) then
          DoStatus('md5 verify ok')
      else
          DoStatus('md5 verify failed');

      if db2_2.RemoveData(p^.db2hnd, True) then
          DoStatus('remove test ok')
      else
          DoStatus('remove test failed');
    end;

  DisposeObject([db1, db2, db1_1, db2_2]);
  for i := 0 to length(TestArry) - 1 do
    begin
      TestArry[i].Data.Free;
      SetLength(TestArry[i].db1hnd, 0);
      SetLength(TestArry[i].db2hnd, 0);
    end;
  DisposeObject(testList);
  DisposeObject(Cipher_);
end;

class procedure TZDB2_Core_Space.Test_Cache;
var
  sour: TMS64;
  hnd1: TIOHnd;
  space1: TZDB2_Core_Space;
  ID: Integer;
  id_buff: TZDB2_BlockHandle;
  tmp: TMS64;
begin
  sour := TMS64.Create;
  sour.Size := 1024 * 1024 * 2;
  MT19937Rand32(MaxInt, sour.Memory, sour.Size div 4);

  DoStatus('sour md5:%s', [umlMD5ToStr(sour.ToMD5).Text]);

  InitIOHnd(hnd1);
  umlFileCreateAsMemory(hnd1);

  space1 := TZDB2_Core_Space.Create(@hnd1);
  space1.AutoCloseIOHnd := True;
  space1.MaxCacheMemory := 1024 * 1024;
  space1.BuildSpace(8 * 1024 * 1024, 1536);
  space1.WriteStream(sour, id_buff);
  space1.FlushCache;

  tmp := TMS64.Create;
  space1.ReadStream(tmp, id_buff);
  DoStatus('read md5:%s', [umlMD5ToStr(tmp.ToMD5).Text]);

  DisposeObject(space1);
  SetLength(id_buff, 0);
end;

class procedure TZDB2_Core_Space.Test_Read_Position;
var
  Cipher_: TZDB2_Cipher;
  Hnd: TIOHnd;
  db: TZDB2_Core_Space;

  procedure Do_Test_Rand_Read();
  var
    m64, tmp: TMS64;
    ID: Integer;
    i: Integer;
    bPos, siz: Int64;
  begin
    m64 := TMS64.Create;
    m64.Size := umlRR(1024, 128 * 1024);
    TMT19937.Rand32(MaxInt, m64.Memory, m64.Size shr 2);

    if db.WriteStream(m64, ID) then
      begin
        for i := 1 to 100 do
          begin
            bPos := umlRR(0, m64.Size - 8192);
            siz := umlRR(1, 8192);
            if (bPos > 0) and (siz > 0) then
              begin
                tmp := TMS64.CustomCreate(8192);
                if db.Read_Position(tmp, ID, bPos, siz) <> siz then
                  begin
                    DoStatus('test read position failed!');
                  end
                else if not CompareMemory(tmp.Memory, m64.PosAsPtr(bPos), tmp.Size) then
                    DoStatus('test read position Data failed!');
                DisposeObject(tmp);
              end;
          end;
      end;

    DisposeObject(m64);
  end;

  procedure Do_Run_Test();
  var
    i: Integer;
  begin
    for i := 1 to 1000 do
        Do_Test_Rand_Read();
  end;

begin
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csAES128, 'hello world.', 1, False, True);
  InitIOHnd(Hnd);
  umlFileCreateAsMemory(Hnd);
  Hnd.Cache.UsedWriteCache := True;
  Hnd.Cache.UsedReadCache := True;

  db := TZDB2_Core_Space.Create(@Hnd);
  db.Cipher := Cipher_;
  db.AutoCloseIOHnd := True;
  db.Mode := smBigData;
  db.Fast_BuildSpace(1024 * 1024 * 500, 999);
  db.Save();

  Do_Run_Test();

  DisposeObject(db);
  DisposeObject(Cipher_);
end;

class procedure TZDB2_Core_Space.Test_Write_Combine();
var
  origin_mem: TMem64;
  Cipher_: TZDB2_Cipher;
  Hnd: TIOHnd;
  db: TZDB2_Core_Space;

  procedure Do_Test_Write_From_origin_1;
  var
    passed: Integer;
    arry_num: Integer;
    arry: TMS64_Array;
    i: Integer;
    bPos, p, p_siz, total: Int64;
    ID: Integer;
    tmp: TMS64;
  begin
    for passed := 1 to 100 do
      begin
        arry_num := umlRR(10, 100);
        // arry_num := 2;
        SetLength(arry, arry_num);
        bPos := umlRR(0, 32 * 1024 * 1024);
        p := bPos;
        total := 0;
        for i := 0 to arry_num - 1 do
          begin
            arry[i] := TMS64.Create;
            p_siz := umlRR(1536, 16384);
            arry[i].Mapping(origin_mem.PosAsPtr(p), p_siz);
            inc(p, p_siz);
            inc(total, p_siz);
          end;

        if not db.Write_Combine_Memory(arry, ID) then
            DoStatus('Test Write_Combine_Memory failed!')
        else
          begin
            tmp := TMS64.CustomCreate(1024 * 1024);
            if db.ReadStream(tmp, ID) then
              begin
                if not CompareMemory(origin_mem.PosAsPtr(bPos), tmp.Memory, tmp.Size) then
                  begin
                    DoStatus('Test Write_Combine_Memory data error!')
                  end;
              end
            else
              begin
                DoStatus('Test Write_Combine_Memory read failed!');
              end;
            DisposeObject(tmp);
            db.RemoveData(ID, False);
          end;

        for i := 0 to arry_num - 1 do
          begin
            DisposeObjectAndNil(arry[i]);
          end;
      end;
  end;

  procedure Do_Test_Write_From_origin_2;
  var
    passed: Integer;
    arry_num: Integer;
    arry: TMem64_Array;
    i: Integer;
    bPos, p, p_siz, total: Int64;
    ID: Integer;
    tmp: TMem64;
  begin
    for passed := 1 to 100 do
      begin
        arry_num := umlRR(10, 100);
        // arry_num := 2;
        SetLength(arry, arry_num);
        bPos := umlRR(0, 32 * 1024 * 1024);
        p := bPos;
        total := 0;
        for i := 0 to arry_num - 1 do
          begin
            arry[i] := TMem64.Create;
            p_siz := umlRR(1536, 16384);
            arry[i].Mapping(origin_mem.PosAsPtr(p), p_siz);
            inc(p, p_siz);
            inc(total, p_siz);
          end;

        if not db.Write_Combine_Memory(arry, ID) then
            DoStatus('Test Write_Combine_Memory failed!')
        else
          begin
            tmp := TMem64.CustomCreate(1024 * 1024);
            if db.ReadData(tmp, ID) then
              begin
                if not CompareMemory(origin_mem.PosAsPtr(bPos), tmp.Memory, tmp.Size) then
                  begin
                    DoStatus('Test Write_Combine_Memory data error!')
                  end;
              end
            else
              begin
                DoStatus('Test Write_Combine_Memory read failed!');
              end;
            DisposeObject(tmp);
            db.RemoveData(ID, False);
          end;

        for i := 0 to arry_num - 1 do
          begin
            DisposeObjectAndNil(arry[i]);
          end;
      end;
  end;

  procedure Do_Test_Write_From_origin_3;
  var
    passed: Integer;
    arry_num: Integer;
    arry: TStream_Array;
    i: Integer;
    bPos, p, p_siz, total: Int64;
    ID: Integer;
    tmp: TMS64;
  begin
    for passed := 1 to 100 do
      begin
        arry_num := umlRR(10, 100);
        // arry_num := 2;
        SetLength(arry, arry_num);
        bPos := umlRR(0, 32 * 1024 * 1024);
        p := bPos;
        total := 0;
        for i := 0 to arry_num - 1 do
          begin
            arry[i] := TMS64.Create;
            p_siz := umlRR(1536, 16384);
            TMS64(arry[i]).Mapping(origin_mem.PosAsPtr(p), p_siz);
            inc(p, p_siz);
            inc(total, p_siz);
          end;

        if not db.Write_Combine_Stream(arry, ID) then
            DoStatus('Test Write_Combine_Stream failed!')
        else
          begin
            tmp := TMS64.CustomCreate(1024 * 1024);
            if db.ReadStream(tmp, ID) then
              begin
                if not CompareMemory(origin_mem.PosAsPtr(bPos), tmp.Memory, tmp.Size) then
                  begin
                    DoStatus('Test Write_Combine_Memory data error!')
                  end;
              end
            else
              begin
                DoStatus('Test Write_Combine_Memory read failed!');
              end;
            DisposeObject(tmp);
            db.RemoveData(ID, False);
          end;

        for i := 0 to arry_num - 1 do
          begin
            DisposeObjectAndNil(arry[i]);
          end;
      end;
  end;

begin
  origin_mem := TMem64.Create;
  origin_mem.Size := 64 * 1024 * 1024;
  TMT19937.Rand32(MaxInt, origin_mem.Memory, origin_mem.Size shr 2);

  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csAES128, 'hello world.', 1, False, True);
  InitIOHnd(Hnd);
  umlFileCreateAsMemory(Hnd);
  Hnd.Cache.UsedWriteCache := True;
  Hnd.Cache.UsedReadCache := True;

  db := TZDB2_Core_Space.Create(@Hnd);
  db.Cipher := Cipher_;
  db.AutoCloseIOHnd := True;
  db.Mode := smBigData;
  db.Fast_BuildSpace(1024 * 1024 * 500, 999);
  db.Save();

  Do_Test_Write_From_origin_1();
  Do_Test_Write_From_origin_2();
  Do_Test_Write_From_origin_3();

  DisposeObject(db);
  DisposeObject(Cipher_);

  DisposeObject(origin_mem);
end;

initialization

FillPtr(@ZDB2_NULL_Data, $FFFF, 0);

finalization

end.
 
