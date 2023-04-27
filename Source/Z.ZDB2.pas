{ ****************************************************************************** }
{ * ZDB 2.0 Core                                                               * }
{ ****************************************************************************** }
unit Z.ZDB2;

{$I Z.Define.inc}

interface

uses Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib,
  Z.MemoryStream,
  Z.Status, Z.Cipher;

type
  TZDB2_Core_Space = class;
  TZDB2_Mem = TMem64;
  TZDB2_UserCustomHeader = array [0 .. 253] of Byte;
  PZDB2_UserCustomHeader = ^TZDB2_UserCustomHeader;

  TZDB2_FileHeader = packed record
    Flag: Cardinal;
    Major, Minor: WORD;
    StructEntry: Int64;
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

  TZDB2_ID_Pool = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<Integer>;
  TZDB2_ID_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<Integer>;
  TZDB2_BlockPtrList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PZDB2_Block>;

  TZDB2_BlockPtrList = class(TZDB2_BlockPtrList_Decl)
  public
    function TotalBlockSize: Int64;
    procedure Clean;
  end;

  IZDB2_Cipher = interface
    procedure Encrypt(buff: Pointer; Size: NativeInt);
    procedure Decrypt(buff: Pointer; Size: NativeInt);
  end;

  TZDB2_BlockHandle = array of Integer;
  PZDB2_BlockHandle = ^TZDB2_BlockHandle;
  TZDB2_BlockBuffer = array of TZDB2_Block;
  TZDB2_BlockWriteCache = array of TZDB2_BlockCache;
  TZDB2_OnProgress = procedure(Total_, current_: Integer) of object;
  TZDB2_OnNoSpace = procedure(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean) of object;

  TZDB2_BlockStore = packed record
    Position: Int64;
    Size: WORD;
    UsedSpace: WORD;
    Prev, Next: Integer;
  end;

  TZDB2_BlockStoreBuffer = array of TZDB2_BlockStore;

  TZDB2_BlockStoreData = class
  public
    Position, NextPosition: Int64;
    Count: Integer;
    MD5: TMD5;
    Buffer: TZDB2_BlockStoreBuffer;

    constructor Create;
    destructor Destroy; override;
    function Read(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
    function Write(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
    procedure BuildBlockBuffer(var BlockBuffer_: TZDB2_BlockBuffer);
    class function ComputeSize(BlockNum_: Integer): Int64;
  end;

  TZDB2_BlockStoreStruct_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TZDB2_BlockStoreData>;

  TZDB2_BlockStoreDataStruct = class(TZDB2_BlockStoreStruct_Decl)
  public
    function BlockSum: Integer;
    procedure ExtractToStoreBuffer(var Buffer: TZDB2_BlockStoreBuffer); overload;
    procedure ExtractToBlockBuffer(var Buffer: TZDB2_BlockBuffer; StartID_: Integer); overload;
    procedure ExtractToBlockBuffer(var Buffer: TZDB2_BlockBuffer); overload;
    function FillFromBlockBuffer(var Buffer: TZDB2_BlockBuffer; StartID_: Integer): Boolean; overload;
    function FillFromBlockBuffer(var Buffer: TZDB2_BlockBuffer): Boolean; overload;
    function Read(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
    function Write(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
    procedure Clean;
    procedure SavingMemory;
  end;

  TZDB2_Space_Planner = class
  private
    FCore: TZDB2_Core_Space;
    FStruct: TZDB2_BlockStoreDataStruct;
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

  TZDB2_CRC16 = class
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

  TZDB2_Cipher = class(TCore_InterfacedObject, IZDB2_Cipher)
  private
    FCipher_: TCipher_Base;
  public
    class function GetCipherSecurity(CipherSecurityString_: U_String): TCipherSecurity;
    constructor Create(CipherSecurity_: TCipherSecurity; password_: U_String; Level_: Integer; Tail_, CBC_: Boolean); overload;
    constructor Create(CipherSecurityString_: U_String; password_: U_String; Level_: Integer; Tail_, CBC_: Boolean); overload;
    destructor Destroy; override;
    procedure Encrypt(buff: Pointer; Size: NativeInt);
    procedure Decrypt(buff: Pointer; Size: NativeInt);
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
  end;

  PZDB2_Core_SpaceState = ^TZDB2_SpaceState;

  TZDB2_Core_Space_Info_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TCritical_BigList<SystemString>;

  TZDB2_Core_Space_Info = class(TZDB2_Core_Space_Info_Decl)
  public
    procedure DoFree(var Data: SystemString); override;
  end;

  TZDB2_Core_Space_Error_Info = class(TZDB2_Core_Space_Info);
  TZDB2_Core_Space_Warning_Info = class(TZDB2_Core_Space_Info);

  TZDB2_Core_Space = class
  private
    FHeader: TZDB2_FileHeader;
    FFault_Shutdown: Boolean;
    FAutoCloseIOHnd: Boolean;
    FAutoFreeIOHnd: Boolean;
    FSpace_IOHnd: PIOHnd;
    FFreeSpaceIndexProbe: Integer;
    FBlockCount: Integer;
    FBlockBuffer: TZDB2_BlockBuffer;
    FBlockStoreDataStruct: TZDB2_BlockStoreDataStruct;
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
    // error
    property Last_Error_Info: TZDB2_Core_Space_Error_Info read FLast_Error_Info;
    procedure ErrorInfo(const Text_: SystemString);
    // warning
    property Last_Warning_Info: TZDB2_Core_Space_Warning_Info read FLast_Warning_Info;
    procedure WarningInfo(const Text_: SystemString);
    // space
    procedure Save();
    function Open(): Boolean;
    property Fault_Shutdown: Boolean read FFault_Shutdown;
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
    function GetSpaceHnd(ID_: Integer): TZDB2_BlockHandle; inline;
    function GetSpaceHndAsText(ID_: Integer): U_String;
    function GetSpaceHndPtr(ID_: Integer): TZDB2_BlockPtrList;
    function CheckWriteSpace(Siz_: Int64): Boolean; overload;
    function CheckWriteSpace(Siz_: Int64; Space_: TZDB2_BlockPtrList): Boolean; overload;
    function GetWriteSpaceBlock(): Integer;
    // search postion
    function SearchDataPos(SpaceHnd: TZDB2_BlockPtrList; const Pos_: Int64; var BlockID_: Integer; var BlockPos_: Int64): Boolean; overload;
    function SearchDataPos(SpaceHnd: TZDB2_BlockHandle; const Pos_: Int64; var BlockID_: Integer; var BlockPos_: Int64): Boolean; overload;
    // block IO
    function Block_IO_Read(buff: Pointer; ID: Integer): WORD;
    function Block_IO_Write(buff: Pointer; ID: Integer): Boolean;
    // write stream
    function WriteStream(Stream_: TCore_Stream; var SpaceHnd: TZDB2_BlockHandle): Boolean; overload;
    function WriteStream(Stream_: TCore_Stream; var ID: Integer): Boolean; overload;
    // write memory
    function WriteData(buff: TZDB2_Mem; var SpaceHnd: TZDB2_BlockHandle; BuffProtected_: Boolean): Boolean; overload;
    function WriteData(buff: TZDB2_Mem; var SpaceHnd: TZDB2_BlockHandle): Boolean; overload;
    function WriteData(buff: TZDB2_Mem; var ID: Integer; BuffProtected_: Boolean): Boolean; overload;
    function WriteData(buff: TZDB2_Mem; var ID: Integer): Boolean; overload;
    // read stream
    function ReadStream(Stream_: TCore_Stream; SpaceHnd: TZDB2_BlockHandle): Boolean; overload;
    function ReadStream(Stream_: TCore_Stream; ID: Integer): Boolean; overload;
    // read memory
    function ReadData(buff: TZDB2_Mem; SpaceHnd: TZDB2_BlockHandle): Boolean; overload;
    function ReadData(buff: TZDB2_Mem; ID: Integer): Boolean; overload;
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
    // hnd
    property AutoCloseIOHnd: Boolean read FAutoCloseIOHnd write FAutoCloseIOHnd;
    property AutoFreeIOHnd: Boolean read FAutoFreeIOHnd write FAutoFreeIOHnd;
    property Space_IOHnd: PIOHnd read FSpace_IOHnd write FSpace_IOHnd;
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
  end;

implementation

type
  TBlockStoreHeader__ = packed record
    Flag1: Cardinal;
    Count: Integer;
    MD5: TMD5;
    Flag2: Cardinal;
  end;

  TBlockStoreTail__ = packed record
    Flag1: Cardinal;
    NextPosition: Int64;
    Flag2: Cardinal;
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

procedure StoreToBlock(var store: TZDB2_BlockStore; var block: TZDB2_Block); forward;
procedure BlockToStore(var block: TZDB2_Block; var store: TZDB2_BlockStore); forward;

procedure StoreToBlock(var store: TZDB2_BlockStore; var block: TZDB2_Block);
begin
  block.Position := store.Position;
  block.Size := store.Size;
  block.UsedSpace := store.UsedSpace;
  block.Prev := store.Prev;
  block.Next := store.Next;
end;

procedure BlockToStore(var block: TZDB2_Block; var store: TZDB2_BlockStore);
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

constructor TZDB2_BlockStoreData.Create;
begin
  inherited Create;
  Position := 0;
  NextPosition := 0;
  Count := 0;
  MD5 := NullMD5;
  SetLength(Buffer, 0);
end;

destructor TZDB2_BlockStoreData.Destroy;
begin
  SetLength(Buffer, 0);
  inherited;
end;

function TZDB2_BlockStoreData.Read(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
var
  head: TBlockStoreHeader__;
  Tail: TBlockStoreTail__;
begin
  Result := False;
  // head
  if not umlFileSeek(Hnd_, Position_) then
    begin
      Sender.ErrorInfo('TZDB2_BlockStoreData.Read Seek error.');
      exit;
    end;
  if not umlBlockRead(Hnd_, head, SizeOf(TBlockStoreHeader__)) then
    begin
      Sender.ErrorInfo('TZDB2_BlockStoreData.Read Header error.');
      exit;
    end;
  if Assigned(Cipher_) then
      Cipher_.Decrypt(@head, SizeOf(TBlockStoreHeader__));
  if head.Flag1 <> C_ZDB2_SpaceTableHead_1 then
    begin
      Sender.ErrorInfo('TZDB2_BlockStoreData.Read verify SpaceTableHead1 error.');
      exit;
    end;
  if head.Flag2 <> C_ZDB2_SpaceTableHead_2 then
    begin
      Sender.ErrorInfo('TZDB2_BlockStoreData.Read verify SpaceTableHead2 error.');
      exit;
    end;
  Count := head.Count;
  SetLength(Buffer, Count);
  // read space table
  if not umlBlockRead(Hnd_, Buffer[0], SizeOf(TZDB2_BlockStore) * Count) then
    begin
      Sender.ErrorInfo('TZDB2_BlockStoreData.Read Buffer error.');
      exit;
    end;
  if Assigned(Cipher_) then
      Cipher_.Decrypt(@Buffer[0], SizeOf(TZDB2_BlockStore) * Count);
  // verify md5
  MD5 := umlMD5(@Buffer[0], SizeOf(TZDB2_BlockStore) * Count);
  if not umlMD5Compare(MD5, head.MD5) then
    begin
      Sender.ErrorInfo('TZDB2_BlockStoreData.Read verify Buffer md5 error.');
      exit;
    end;
  // tail
  if not umlBlockRead(Hnd_, Tail, SizeOf(TBlockStoreTail__)) then
    begin
      Sender.ErrorInfo('TZDB2_BlockStoreData.Read Tail error.');
      exit;
    end;
  if Assigned(Cipher_) then
      Cipher_.Decrypt(@Tail, SizeOf(TBlockStoreTail__));
  if Tail.Flag1 <> C_ZDB2_SpaceTableTail_1 then
    begin
      Sender.ErrorInfo('TZDB2_BlockStoreData.Read verify Tail1 error.');
      exit;
    end;
  if Tail.Flag2 <> C_ZDB2_SpaceTableTail_2 then
    begin
      Sender.ErrorInfo('TZDB2_BlockStoreData.Read verify Tail2 error.');
      exit;
    end;
  NextPosition := Tail.NextPosition;
  Position := Position_;
  Result := True;
end;

function TZDB2_BlockStoreData.Write(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
var
  head: TBlockStoreHeader__;
  tmp: TMem64;
  Tail: TBlockStoreTail__;
begin
  Result := False;
  // head
  head.Flag1 := C_ZDB2_SpaceTableHead_1;
  head.Count := Length(Buffer);
  head.MD5 := umlMD5(@Buffer[0], SizeOf(TZDB2_BlockStore) * Length(Buffer));
  head.Flag2 := C_ZDB2_SpaceTableHead_2;
  if not umlFileSeek(Hnd_, Position_) then
    begin
      Sender.ErrorInfo('TZDB2_BlockStoreData.Write Seek error.');
      exit;
    end;
  if Assigned(Cipher_) then
      Cipher_.Encrypt(@head, SizeOf(TBlockStoreHeader__));
  if not umlBlockWrite(Hnd_, head, SizeOf(TBlockStoreHeader__)) then
    begin
      Sender.ErrorInfo('TZDB2_BlockStoreData.Write head error.');
      exit;
    end;
  // write space table
  tmp := TMem64.Create;
  tmp.Size := SizeOf(TZDB2_BlockStore) * Length(Buffer);
  if tmp.Size > 0 then
      CopyPtr(@Buffer[0], tmp.Memory, tmp.Size);
  if Assigned(Cipher_) then
      Cipher_.Encrypt(tmp.Memory, tmp.Size);
  if not umlBlockWrite(Hnd_, tmp.Memory^, tmp.Size) then
    begin
      Sender.ErrorInfo('TZDB2_BlockStoreData.Write Buffer error.');
      exit;
    end;
  DisposeObject(tmp);
  // tail
  Tail.Flag1 := C_ZDB2_SpaceTableTail_1;
  Tail.NextPosition := NextPosition;
  Tail.Flag2 := C_ZDB2_SpaceTableTail_2;
  if Assigned(Cipher_) then
      Cipher_.Encrypt(@Tail, SizeOf(TBlockStoreTail__));
  if not umlBlockWrite(Hnd_, Tail, SizeOf(TBlockStoreTail__)) then
    begin
      Sender.ErrorInfo('TZDB2_BlockStoreData.Write tail error.');
      exit;
    end;
  Result := True;
end;

procedure TZDB2_BlockStoreData.BuildBlockBuffer(var BlockBuffer_: TZDB2_BlockBuffer);
var
  i: Integer;
begin
  Position := 0;
  NextPosition := 0;
  Count := Length(BlockBuffer_);
  SetLength(Buffer, Count);
  for i := 0 to Count - 1 do
      BlockToStore(BlockBuffer_[i], Buffer[i]);
  MD5 := umlMD5(@Buffer[0], SizeOf(TZDB2_BlockStore) * Count);
end;

class function TZDB2_BlockStoreData.ComputeSize(BlockNum_: Integer): Int64;
begin
  Result := SizeOf(TBlockStoreHeader__) + (SizeOf(TZDB2_BlockStore) * BlockNum_) + SizeOf(TBlockStoreTail__);
end;

function TZDB2_BlockStoreDataStruct.BlockSum: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      inc(Result, Items[i].Count);
end;

procedure TZDB2_BlockStoreDataStruct.ExtractToStoreBuffer(var Buffer: TZDB2_BlockStoreBuffer);
var
  i, j, ID: Integer;
  StoreData_: TZDB2_BlockStoreData;
begin
  SetLength(Buffer, BlockSum);
  ID := 0;
  for i := 0 to Count - 1 do
    begin
      StoreData_ := Items[i];
      for j := 0 to StoreData_.Count - 1 do
        begin
          Buffer[ID] := StoreData_.Buffer[j];
          inc(ID);
        end;
    end;
end;

procedure TZDB2_BlockStoreDataStruct.ExtractToBlockBuffer(var Buffer: TZDB2_BlockBuffer; StartID_: Integer);
var
  i, j, ID: Integer;
  StoreData_: TZDB2_BlockStoreData;
begin
  SetLength(Buffer, StartID_ + BlockSum);
  ID := StartID_;
  for i := 0 to Count - 1 do
    begin
      StoreData_ := Items[i];
      for j := 0 to StoreData_.Count - 1 do
        begin
          StoreToBlock(StoreData_.Buffer[j], Buffer[ID]);
          Buffer[ID].ID := ID;
          inc(ID);
        end;
    end;
end;

procedure TZDB2_BlockStoreDataStruct.ExtractToBlockBuffer(var Buffer: TZDB2_BlockBuffer);
begin
  ExtractToBlockBuffer(Buffer, 0);
end;

function TZDB2_BlockStoreDataStruct.FillFromBlockBuffer(var Buffer: TZDB2_BlockBuffer; StartID_: Integer): Boolean;
var
  i, j, ID: Integer;
  StoreData_: TZDB2_BlockStoreData;
begin
  Result := False;
  ID := StartID_;
  for i := 0 to Count - 1 do
    begin
      StoreData_ := Items[i];
      if Length(StoreData_.Buffer) <> StoreData_.Count then
          SetLength(StoreData_.Buffer, StoreData_.Count);
      for j := 0 to StoreData_.Count - 1 do
        begin
          BlockToStore(Buffer[ID], StoreData_.Buffer[j]);
          inc(ID);
        end;
    end;
  Result := True;
end;

function TZDB2_BlockStoreDataStruct.FillFromBlockBuffer(var Buffer: TZDB2_BlockBuffer): Boolean;
begin
  Result := FillFromBlockBuffer(Buffer, 0);
end;

function TZDB2_BlockStoreDataStruct.Read(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
var
  Pos_: Int64;
  StoreData_: TZDB2_BlockStoreData;
begin
  Clean;
  Result := False;
  Pos_ := Position_;
  while (Pos_ > 0) and (Pos_ < umlFileGetSize(Hnd_)) do
    begin
      StoreData_ := TZDB2_BlockStoreData.Create;
      if StoreData_.Read(Sender, Cipher_, Pos_, Hnd_) then
        begin
          Add(StoreData_);
          Pos_ := StoreData_.NextPosition;
          Result := True;
        end
      else
        begin
          DisposeObject(StoreData_);
          Result := False;
          break;
        end;
    end;
end;

function TZDB2_BlockStoreDataStruct.Write(Sender: TZDB2_Core_Space; Cipher_: IZDB2_Cipher; Position_: Int64; var Hnd_: TIOHnd): Boolean;
var
  i: Integer;
  StoreData_: TZDB2_BlockStoreData;
begin
  Result := True;
  for i := 0 to Count - 1 do
    begin
      StoreData_ := Items[i];
      Result := Result and StoreData_.Write(Sender, Cipher_, StoreData_.Position, Hnd_);
    end;
end;

procedure TZDB2_BlockStoreDataStruct.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(Items[i]);
  inherited Clear;
end;

procedure TZDB2_BlockStoreDataStruct.SavingMemory;
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
  FStruct := TZDB2_BlockStoreDataStruct.Create;
  if FCore.FBlockStoreDataStruct.Count > 0 then
    begin
      FCore.Save;
    end
  else
    begin
      FCore.FHeader.StructEntry := 0;
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
  StoreData_: TZDB2_BlockStoreData;
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

    StoreData_ := TZDB2_BlockStoreData.Create;
    StoreData_.BuildBlockBuffer(BlockBuffer_);
    FStruct.Add(StoreData_);

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
  if Length(SpaceHnd) > 0 then
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
  StoreData_: TZDB2_BlockStoreData;
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
  StoreData_ := TZDB2_BlockStoreData.Create;
  FStruct.ExtractToStoreBuffer(StoreData_.Buffer);
  StoreData_.Position := umlFileGetSize(FCore.FSpace_IOHnd^);
  StoreData_.NextPosition := 0;
  StoreData_.Count := Length(StoreData_.Buffer);

  if FCore.FBlockStoreDataStruct.Count > 0 then
    begin
      // AppendSpace
      FCore.FBlockStoreDataStruct.Last.NextPosition := StoreData_.Position;
      FCore.Save;
      StoreData_.Write(FCore, FCore.FCipher, StoreData_.Position, FCore.FSpace_IOHnd^);
      FCore.Open;
    end
  else
    begin
      // BuildSpace
      FCore.FHeader.StructEntry := StoreData_.Position;
      FCore.WriteHeader;
      StoreData_.Write(FCore, FCore.FCipher, StoreData_.Position, FCore.FSpace_IOHnd^);
      FCore.Open;
    end;
  DisposeObject(StoreData_);
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
  SetLength(CRC16Buffer, Length(Hnd));
  SwapBuff_ := System.GetMemory($FFFF);
  try
    for i := 0 to Length(Hnd) - 1 do
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
                Core_.FOnProgress(Length(Hnd), i);
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
  num := Length(CRC16Buffer);
  stream.Write(num, 4);
  if num > 0 then
      stream.Write(CRC16Buffer[0], Length(CRC16Buffer) * 2);
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
      c.Encrypt(@buff[0], Length(buff));
      c.Decrypt(@buff[0], Length(buff));
      s2.ANSI := buff;
      if s1.Same(s2) then
          DoStatus('TZDB2_Cipher test ok')
      else
          DoStatus('TZDB2_Cipher test error');
      DisposeObject(c);
    end;
end;

procedure TZDB2_Core_Space_Info.DoFree(var Data: SystemString);
begin
  Data := '';
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
  while i < Length(FBlockWriteCache) do
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
  FBlockStoreDataStruct.FillFromBlockBuffer(FBlockBuffer);
  if not FBlockStoreDataStruct.Write(Self, FCipher, FHeader.StructEntry, FSpace_IOHnd^) then
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
  FBlockCount := Length(FBlockBuffer);
  SetLength(FBlockWriteCache, FBlockCount);
  FState.Physics := 0;
  FState.FreeSpace := 0;
  FState.Cache := 0;
  FState.ReadNum := 0;
  FState.ReadSize := 0;
  FState.WriteNum := 0;
  FState.WriteSize := 0;

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
  FBlockStoreDataStruct := TZDB2_BlockStoreDataStruct.Create;
  FMaxCacheMemory := 1024 * 1024 * 32;
  FUsedReadCache := False;
  FUsedWriteCache := True;
  SetLength(FBlockWriteCache, 0);
  FMode := smNormal;
  FCipher := nil;
  FCipherMem := TMem64.Create;

  FState.Physics := 0;
  FState.FreeSpace := 0;
  FState.Cache := 0;
  FState.ReadNum := 0;
  FState.ReadSize := 0;
  FState.WriteNum := 0;
  FState.WriteSize := 0;

  FLast_Modification := GetTimeTick();
  FLast_Error_Info := TZDB2_Core_Space_Error_Info.Create;
  FLast_Warning_Info := TZDB2_Core_Space_Warning_Info.Create;
  FOnProgress := nil;
  FOnNoSpace := nil;
end;

destructor TZDB2_Core_Space.Destroy;
begin
  if FHeader.Modification then
      Save();

  SetLength(FBlockBuffer, 0);
  FBlockStoreDataStruct.Clean;
  DisposeObject(FBlockStoreDataStruct);
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

procedure TZDB2_Core_Space.Save;
begin
  if FSpace_IOHnd^.IsOnlyRead then
      exit;
  WriteTable();
  FlushCache;
  FHeader.Modification := False;
  WriteHeader;
  umlFileUpdate(FSpace_IOHnd^);
end;

function TZDB2_Core_Space.Open(): Boolean;
var
  num: Integer;
begin
  Result := False;
  FBlockStoreDataStruct.Clean;
  FillPtr(@FHeader, C_ZDB2_HeaderSize, 0);
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
      if FHeader.StructEntry >= C_ZDB2_HeaderSize then
        if not FBlockStoreDataStruct.Read(Self, FCipher, FHeader.StructEntry, FSpace_IOHnd^) then
          begin
            ErrorInfo('Open: read BlockStoreDataStruct error.');
            exit;
          end;
      FBlockStoreDataStruct.ExtractToBlockBuffer(FBlockBuffer);
      FBlockStoreDataStruct.SavingMemory;
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
  StoreData_: TZDB2_BlockStoreData;
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
  FBlockStoreDataStruct.Clean;
  BlockSize := umlMax(BlockSize_, C_ZDB2_MinBlockSize);
  headSiz := C_ZDB2_HeaderSize + TZDB2_BlockStoreData.ComputeSize(PhySpaceSize div BlockSize);
  SetLength(FBlockBuffer, (PhySpaceSize - headSiz) div BlockSize);
  PrepareCacheBlock();
  // fast init space
  if not umlFileSetSize(FSpace_IOHnd^, headSiz + (BlockSize * FBlockCount)) then // fast alloc space
    begin
      ErrorInfo(PFormat('Fast_BuildSpace: umlFileSetSize %d error.', [headSiz + (BlockSize * FBlockCount)]));
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
  FHeader.StructEntry := C_ZDB2_HeaderSize;
  // builder store struct
  StoreData_ := TZDB2_BlockStoreData.Create;
  StoreData_.BuildBlockBuffer(FBlockBuffer);
  StoreData_.Position := FHeader.StructEntry;
  StoreData_.NextPosition := 0;
  FBlockStoreDataStruct.Add(StoreData_);
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
  StoreData_: TZDB2_BlockStoreData;
  fp: Int64;
  i: Integer;
begin
  if FSpace_IOHnd^.IsOnlyRead then
    begin
      ErrorInfo('Fast_AppendSpace: OnlyRead.');
      Result := False;
      exit;
    end;
  if FBlockStoreDataStruct.Count = 0 then
    begin
      Result := Fast_BuildSpace(NewSpaceSize_, DestBlockSize_);
      exit;
    end;
  Result := False;
  // flush
  FlushCache;
  if not FBlockStoreDataStruct.FillFromBlockBuffer(FBlockBuffer) then
    begin
      ErrorInfo('Fast_AppendSpace: FillFromBlockBuffer error.');
      exit;
    end;
  // prepare block
  BlockSize := umlMax(DestBlockSize_, C_ZDB2_MinBlockSize);
  BlockNum_ := NewSpaceSize_ div DestBlockSize_;
  SetLength(tmp, BlockNum_);
  headPos := umlFileGetSize(FSpace_IOHnd^);
  headSiz := TZDB2_BlockStoreData.ComputeSize(Length(tmp));
  // fast append space
  if not umlFileSetSize(FSpace_IOHnd^, headPos + headSiz + (BlockSize * BlockNum_)) then // fast alloc space
    begin
      ErrorInfo(PFormat('Fast_BuildSpace: umlFileSetSize %d error.', [headPos + headSiz + (BlockSize * BlockNum_)]));
      exit;
    end;
  if not umlFileSeek(FSpace_IOHnd^, headPos) then // reseek
    begin
      ErrorInfo(PFormat('Fast_BuildSpace: umlFileSeek %d error.', [headPos]));
      exit;
    end;
  // fill free space
  fp := headPos + headSiz;
  i := 0;
  while i < Length(tmp) do
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
  StoreData_ := TZDB2_BlockStoreData.Create;
  StoreData_.BuildBlockBuffer(tmp);
  SetLength(tmp, 0);
  StoreData_.Position := headPos;
  StoreData_.NextPosition := 0;
  FBlockStoreDataStruct.Last.NextPosition := headPos;
  FBlockStoreDataStruct.Add(StoreData_);
  FBlockStoreDataStruct.ExtractToBlockBuffer(FBlockBuffer);
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
  StoreData_: TZDB2_BlockStoreData;
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
  FBlockStoreDataStruct.Clean;
  BlockSize := umlMax(BlockSize_, C_ZDB2_MinBlockSize);
  headSiz := C_ZDB2_HeaderSize + TZDB2_BlockStoreData.ComputeSize(PhySpaceSize div BlockSize);
  SetLength(FBlockBuffer, (PhySpaceSize - headSiz) div BlockSize);
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
  FHeader.StructEntry := C_ZDB2_HeaderSize;
  // builder store struct
  StoreData_ := TZDB2_BlockStoreData.Create;
  StoreData_.BuildBlockBuffer(FBlockBuffer);
  StoreData_.Position := FHeader.StructEntry;
  StoreData_.NextPosition := 0;
  FBlockStoreDataStruct.Add(StoreData_);
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
  StoreData_: TZDB2_BlockStoreData;
  m64: TZDB2_Mem;
  i: Integer;
begin
  if FSpace_IOHnd^.IsOnlyRead then
    begin
      ErrorInfo('AppendSpace: OnlyRead.');
      Result := False;
      exit;
    end;
  if FBlockStoreDataStruct.Count = 0 then
    begin
      Result := BuildSpace(NewSpaceSize_, DestBlockSize_);
      exit;
    end;
  Result := False;
  // flush
  FlushCache;
  if not FBlockStoreDataStruct.FillFromBlockBuffer(FBlockBuffer) then
    begin
      ErrorInfo('AppendSpace: FillFromBlockBuffer error.');
      exit;
    end;
  // prepare block
  BlockSize := umlMax(DestBlockSize_, C_ZDB2_MinBlockSize);
  BlockNum_ := NewSpaceSize_ div DestBlockSize_;
  SetLength(tmp, BlockNum_);
  headPos := umlFileGetSize(FSpace_IOHnd^);
  headSiz := TZDB2_BlockStoreData.ComputeSize(Length(tmp));
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
  while i < Length(tmp) do
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
          FOnProgress(Length(tmp), i);
    end;
  // builder store struct
  StoreData_ := TZDB2_BlockStoreData.Create;
  StoreData_.BuildBlockBuffer(tmp);
  SetLength(tmp, 0);
  StoreData_.Position := headPos;
  StoreData_.NextPosition := 0;
  FBlockStoreDataStruct.Last.NextPosition := headPos;
  FBlockStoreDataStruct.Add(StoreData_);
  FBlockStoreDataStruct.ExtractToBlockBuffer(FBlockBuffer);
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
  dest_StoreData: TZDB2_BlockStoreData;
  i: Integer;
  headSize, headPos_: Int64;
  m64: TZDB2_Mem;
  SwapBuff_: Pointer;
begin
  Result := False;
  dest_StoreData := nil;
  FlushCache;
  headSize := C_ZDB2_HeaderSize + TZDB2_BlockStoreData.ComputeSize(FBlockCount);
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
    dest_Header.StructEntry := headPos_;
    umlFileSeek(Dest_IOHnd, 0);
    umlBlockWrite(Dest_IOHnd, dest_Header, C_ZDB2_HeaderSize);

    dest_StoreData := TZDB2_BlockStoreData.Create;
    dest_StoreData.BuildBlockBuffer(dest_BlockBuffer);
    dest_StoreData.Position := headPos_;
    dest_StoreData.NextPosition := 0;
    dest_StoreData.Count := Length(dest_StoreData.Buffer);
    dest_StoreData.Write(Self, FCipher, headPos_, Dest_IOHnd);
    umlFileUpdate(Dest_IOHnd);
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

function TZDB2_Core_Space.SearchDataPos(SpaceHnd: TZDB2_BlockPtrList; const Pos_: Int64; var BlockID_: Integer; var BlockPos_: Int64): Boolean;
var
  i: Integer;
  p: PZDB2_Block;
begin
  Result := False;
  BlockPos_ := 0;
  if SpaceHnd.Count <= 0 then
      exit;
  for i := 0 to SpaceHnd.Count - 1 do
    begin
      p := SpaceHnd[i];
      if (Pos_ >= 0) and umlInRange(Pos_, BlockPos_, BlockPos_ + p^.UsedSpace) then
        begin
          BlockID_ := p^.ID;
          exit(True);
        end;
      inc(BlockPos_, p^.UsedSpace);
    end;
  if Pos_ < 0 then
    begin
      BlockID_ := p^.ID;
      Result := True;
    end
  else
    begin
      BlockPos_ := 0;
      BlockID_ := -1;
    end;
end;

function TZDB2_Core_Space.SearchDataPos(SpaceHnd: TZDB2_BlockHandle; const Pos_: Int64; var BlockID_: Integer; var BlockPos_: Int64): Boolean;
var
  ID: Integer;
begin
  Result := False;
  BlockPos_ := 0;
  for ID in SpaceHnd do
    begin
      if (Pos_ >= 0) and umlInRange(Pos_, BlockPos_, BlockPos_ + FBlockBuffer[ID].UsedSpace) then
        begin
          BlockID_ := ID;
          exit(True);
        end;
      inc(BlockPos_, FBlockBuffer[ID].UsedSpace);
    end;
  if Pos_ < 0 then
    begin
      BlockID_ := SpaceHnd[Length(SpaceHnd) - 1];
      Result := True;
    end
  else
    begin
      BlockPos_ := 0;
      BlockID_ := -1;
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
      Do_Modification;
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
                      ErrorInfo('WriteData: umlFileSeek Block error.');
                      exit;
                    end;
                  DoEncrypt(SwapBuff_, Size);
                  if not umlBlockWrite(FSpace_IOHnd^, SwapBuff_^, Size) then
                    begin
                      ErrorInfo('WriteData: umlBlockWrite Block error.');
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
                      ErrorInfo('WriteData: umlFileSeek tail Block error.');
                      exit;
                    end;
                  DoEncrypt(SwapBuff_, tmp);
                  if not umlBlockWrite(FSpace_IOHnd^, SwapBuff_^, tmp) then
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
    Stream_.Position := bakPos_;
    DisposeObject(Space_);

    // fill link
    j := 0;
    FBlockBuffer[SpaceHnd[0]].Prev := -1;
    while j < Length(SpaceHnd) do
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
    Do_Modification;
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
  while j < Length(SpaceHnd) do
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
  Do_Modification;
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

function TZDB2_Core_Space.ReadStream(Stream_: TCore_Stream; SpaceHnd: TZDB2_BlockHandle): Boolean;
var
  i: Integer;
  Siz_: Int64;
  SwapBuff_: Pointer;
  bak_pos: Int64;
begin
  Result := False;

  if Length(SpaceHnd) = 0 then
    begin
      ErrorInfo('ReadStream: SpaceHnd null error.');
      exit;
    end;

  bak_pos := Stream_.Position;

  { compute queue space }
  i := 0;
  Siz_ := 0;
  while i < Length(SpaceHnd) do
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
    while i < Length(SpaceHnd) do
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

  if Length(SpaceHnd) = 0 then
    begin
      ErrorInfo('ReadData: SpaceHnd null error.');
      exit;
    end;

  { compute queue space }
  i := 0;
  Siz_ := 0;
  while i < Length(SpaceHnd) do
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
  while i < Length(SpaceHnd) do
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

function TZDB2_Core_Space.ComputeMD5(SpaceHnd: TZDB2_BlockHandle; var MD5: TMD5): Boolean;
var
  i: Integer;
  Siz_: Int64;
  SwapBuff_: Pointer;
  Context_: TMD5Context;
begin
  Result := False;

  if Length(SpaceHnd) = 0 then
    begin
      ErrorInfo('ReadStream: SpaceHnd null error.');
      exit;
    end;

  { compute queue space }
  i := 0;
  Siz_ := 0;
  while i < Length(SpaceHnd) do
    with FBlockBuffer[SpaceHnd[i]] do
      begin
        inc(Siz_, UsedSpace);
        inc(i);
      end;

  if Siz_ = 0 then
      exit;

  { read }
  THashMD5.InitMD5(Context_);
  i := 0;
  SwapBuff_ := System.GetMemory($FFFF);
  try
    while i < Length(SpaceHnd) do
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
          THashMD5.UpdateMD5(Context_, SwapBuff_^, UsedSpace);
          inc(i);
        end;

    THashMD5.FinalizeMD5(Context_, MD5);

    inc(FState.ReadNum);
    inc(FState.ReadSize, Siz_);
    Result := True;
  finally
      System.FreeMemory(SwapBuff_);
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
  Result := (Length(SpaceHnd) > 0) and Check(SpaceHnd[0]);
  i := 0;
  while i < Length(SpaceHnd) do
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
  while i < Length(SpaceHnd) do
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
  while i < Length(SpaceHnd) do
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
          for j := 0 to Length(tmp) - 1 do
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

procedure TZDB2_Core_Space.DoProgress(Total_, current_: Integer);
begin
  if Assigned(FOnProgress) then
      FOnProgress(Total_, current_);
end;

class function TZDB2_Core_Space.Combine_Handle(hnd1, hnd2: TZDB2_BlockHandle): TZDB2_BlockHandle;
var
  i, j: Integer;
begin
  SetLength(Result, Length(hnd1) + Length(hnd2));
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
  SetLength(Result, Length(Hnd));
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

  TTestList_ = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PTest_>;

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

  for i := 0 to Length(TestArry) - 1 do
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
  db1.Save;

  db1Hnd_ := db1.BuildTableID;
  if Length(db1Hnd_) = Length(TestArry) then
    begin
      for i := 0 to Length(TestArry) - 1 do
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
  if Length(db2Hnd_) = Length(TestArry) then
    begin
      for i := 0 to Length(TestArry) - 1 do
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
  for i := 0 to Length(TestArry) - 1 do
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

initialization

FillPtr(@ZDB2_NULL_Data, $FFFF, 0);

finalization

end.
