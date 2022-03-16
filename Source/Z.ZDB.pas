{ ****************************************************************************** }
{ * Z.ZDB Manager, base on Z.ZDB.ObjectData_LIB                                * }
{ ****************************************************************************** }
unit Z.ZDB;

{$I Z.Define.inc}

interface

uses Z.Core, Z.ZDB.ObjectData_LIB, Z.UnicodeMixedLib, Z.PascalStrings, Z.UPascalStrings, Z.ListEngine;

type
  TItemHandle = Z.ZDB.ObjectData_LIB.TItemHandle_;
  PItemHandle = ^TItemHandle;
  TFieldHandle = Z.ZDB.ObjectData_LIB.TField;
  PFieldHandle = ^TFieldHandle;
  TItemSearch = Z.ZDB.ObjectData_LIB.TSearchItem_;
  PItemSearch = ^TItemSearch;
  TFieldSearch = Z.ZDB.ObjectData_LIB.TSearchField_;
  PFieldSearch = ^TFieldSearch;
  TItemRecursionSearch = Z.ZDB.ObjectData_LIB.TRecursionSearch_;
  PItemRecursionSearch = ^TItemRecursionSearch;

  PObjectDataCacheHeader = PHeader;
  PObjectDataCacheItemBlock = PItemBlock;

  TObjectDataCacheItem = record
    Description: U_String;
    ExtID: Byte;
    FirstBlockPOS: Int64;
    LastBlockPOS: Int64;
    Size: Int64;
    BlockCount: Int64;
    CurrentBlockSeekPOS: Int64;
    CurrentFileSeekPOS: Int64;
    State: Integer;
    procedure Write(var wVal: TItem);
    procedure Read(var rVal: TItem);
  end;

  PObjectDataCacheItem = ^TObjectDataCacheItem;

  TObjectDataCacheField = record
    UpFieldPOS: Int64;
    Description: U_String;
    HeaderCount: Int64;
    FirstHeaderPOS: Int64;
    LastHeaderPOS: Int64;
    State: Integer;
    procedure Write(var wVal: TField);
    procedure Read(var rVal: TField);
  end;

  PObjectDataCacheField = ^TObjectDataCacheField;

  TSwapHead = packed record
    Size: Integer;
    MD5: TMD5;
    Position: Int64;
  end;

  TObjectDataManager = class;
{$IFDEF FPC}
  TDBImpNotifyProc = procedure(Sender: TObjectDataManager; sourFile: SystemString; fieldPos, ItemPos: Int64) is nested;
  TDBExpNotifyProc = procedure(Sender: TObjectDataManager; fieldPos, ItemPos: Int64; destFile: SystemString) is nested;
{$ELSE FPC}
  TDBImpNotifyProc = reference to procedure(Sender: TObjectDataManager; sourFile: SystemString; fieldPos, ItemPos: Int64);
  TDBExpNotifyProc = reference to procedure(Sender: TObjectDataManager; fieldPos, ItemPos: Int64; destFile: SystemString);
{$ENDIF FPC}

  TObjectDataManager = class(TCore_Object)
  protected
    FStreamEngine: TCore_Stream;
    FDB_HND: TObjectDataHandle;
    FNeedCreateNew, FOnlyRead: Boolean;
    FObjectName: SystemString;
    FDefaultItemID: Byte;
    FIsOpened: Boolean;
    FData: Pointer;

    function GetAutoFreeHandle: Boolean;
    procedure SetAutoFreeHandle(const Value: Boolean);
  protected
    procedure DoOpenBefore; virtual;
    procedure DoOpenAfter; virtual;

    function GetOverWriteItem: Boolean;
    function GetAllowSameHeaderName: Boolean;
    function GetDBTime: TDateTime;
    procedure SetOverWriteItem(Value: Boolean);
    procedure SetAllowSameHeaderName(Value: Boolean);

    procedure DBErrorProc(error: U_String);
    function DoOpen(): Boolean;
    function NewHandle(Stream_: TCore_Stream; const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, IsNewDB_: Boolean): Boolean; overload;
    function NewHandle(FixedStringL: Byte; Stream_: TCore_Stream; const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, IsNewDB_: Boolean): Boolean; overload;
  public
    // Open Database
    constructor Open(const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead: Boolean); overload;
    // create new Database
    constructor CreateNew(const dbFile: SystemString; const dbItemID: Byte); overload;
    constructor CreateNew(FixedStringL: Byte; const dbFile: SystemString; const dbItemID: Byte); overload;
    // create or Open form Stream IO
    constructor CreateAsStream(Stream_: TCore_Stream;
      const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, isNewDB, DestroyTimeFreeStream: Boolean); overload;
    constructor CreateAsStream(FixedStringL: Byte; Stream_: TCore_Stream;
      const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, isNewDB, DestroyTimeFreeStream: Boolean); overload;

    // destroy
    destructor Destroy; override;

    function CopyTo(DestDB: TObjectDataManager): Boolean;
    function CopyToPath(DestDB: TObjectDataManager; destPath: SystemString): Boolean;
    function CopyFieldToPath(fieldPos: Int64; DestDB: TObjectDataManager; destPath: SystemString): Boolean;
    function CopyItemToPath(const Path_, DBItemName: SystemString; DestDB: TObjectDataManager; destPath: SystemString): Integer;

    // export to stream
    procedure SaveToStream(stream: TCore_Stream);

    // export to ZLib Compressor for stream
    procedure SaveToZLibStream(stream: TCore_Stream);

    // export to parallel Compressor for stream
    procedure SaveToParallelCompressionStream(stream: TCore_Stream);

    // Import recursively
    procedure ImpFromPathP(ImpPath, Path_: SystemString; IncludeSub: Boolean; Notify: TDBImpNotifyProc); overload;
    procedure ImpFromPath(ImpPath, Path_: SystemString; IncludeSub: Boolean); overload;

    // Import batch
    procedure ImpFromFilesP(ImpFiles: TCore_Strings; Path_: SystemString; Notify: TDBImpNotifyProc); overload;
    procedure ImpFromFiles(ImpFiles: TCore_Strings; Path_: SystemString); overload;

    // split direct
    procedure SplitTo(RootPh, destFile: SystemString; SplitSiz: Int64);

    // split to ZLib compressor for Database
    procedure SplitToZLib(RootPh, destFile: SystemString; SplitSiz: Int64);

    // split to Parallel compressor for Database
    procedure SplitToParallelCompression(RootPh, destFile: SystemString; SplitSiz: Int64);

    // export to disk
    procedure ExpPathToDisk(Path_, ExpPath_: SystemString; IncludeSub: Boolean); overload;
    procedure ExpPathToDiskP(Path_, ExpPath_: SystemString; IncludeSub: Boolean; Notify: TDBExpNotifyProc); overload;
    procedure ExpItemToDisk(Path_, DBItem, ExpFilename_: SystemString);

    // state
    function Is_BACKUP_Mode: Boolean;
    function Is_Flush_Mode: Boolean;
    function isAbort: Boolean;
    function Close: Boolean;
    function ErrorNo: Int64;
    function Modification: Boolean;
    function Size: Int64;
    function IOReadSize: Int64;
    function IOWriteSize: Int64;
    procedure SetID(const ID_: Byte);
    procedure UpdateIO; virtual;

    // reserved
    procedure SetReserved(Value: TObjectDataHandle_Reserved_Data);
    function GetReserved: TObjectDataHandle_Reserved_Data;
    property Reserved: TObjectDataHandle_Reserved_Data read GetReserved write SetReserved;

    // field api
    function CreateField(const FieldName_, Field_Desc: SystemString): Boolean;
    function CreateRootField(const RootName: SystemString): Boolean;
    function DirectoryExists(const FieldName_: SystemString): Boolean;
    function FastDelete(const fieldPos: Int64; const fPos: Int64): Boolean;
    function FastFieldExists(const fieldPos: Int64; const FieldName: SystemString): Boolean;
    function FastFieldCreate(const fieldPos: Int64; const FieldName, FieldDescription: SystemString; var NewFieldPos: Int64): Boolean;
    function RootField: Int64;
    function SetRootField(const RootName: SystemString): Boolean;
    function GetRootFieldPos(const RootName: SystemString): Int64;
    function FieldRename(const fieldPos: Int64; const NewFieldName, NewFieldDescription: SystemString): Boolean;
    function FieldDelete(const Path_: SystemString; const FieldName: SystemString): Boolean;
    function FieldExists(const Path_: SystemString; const FieldName: SystemString): Boolean; overload;
    function FieldExists(const Path_: SystemString): Boolean; overload;
    function FieldFastFindFirst(const fieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindLast(const fieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindFirst(const Path_, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindLast(const Path_, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldMove(const Path_, FieldName, destPath: SystemString): Boolean;
    function GetFieldData(const fieldPos: Int64; var dest: TFieldHandle): Boolean;
    function GetFieldPath(const fieldPos: Int64): SystemString; overload;
    function GetFieldPath(const fieldPos, RootFieldPos: Int64): SystemString; overload;
    function GetPathField(const Path_: SystemString; var dest: Int64): Boolean;
    function GetPathFieldPos(const Path_: SystemString): Int64;
    function GetPathFieldHeaderCount(const Path_: SystemString): Int64;
    function GetPathFieldHeaderNames(const Path_: SystemString; var output: U_StringArray): Boolean;

    // header api
    function GetHeaderModificationTime(const hPos: Int64): TDateTime;
    function GetFirstHeaderFromField(fieldPos: Int64; var h: THeader): Boolean;
    function GetLastHeaderFromField(fieldPos: Int64; var h: THeader): Boolean;
    function GetHeader(hPos: Int64; var h: THeader): Boolean;

    // item api
    function GetItemSize(const Path_, DBItemName: SystemString): Int64;
    function ItemCreate(const Path_, DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemDelete(const Path_, DBItemName: SystemString): Boolean;
    function ItemExists(const Path_, DBItemName: SystemString): Boolean;
    function ItemOpen(const Path_, DBItemName: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemAutoOpenOrCreate(const Path_, DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemUpdate(var ItemHnd: TItemHandle): Boolean;
    function ItemClose(var ItemHnd: TItemHandle): Boolean;
    function ItemCopyTo(var ItemHnd: TItemHandle; DestDB: TObjectDataManager; var DestItemHandle: TItemHandle; const CopySize: Int64): Boolean;
    function ItemMove(const Path_, ItemName, destPath: SystemString): Boolean;
    function ItemRename(const fieldPos: Int64; var ItemHnd: TItemHandle; const NewName, NewDescription: SystemString): Boolean;
    function ItemFastInsertNew(const fieldPos, InsertHeaderPos: Int64; const DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemFastCreate(const fPos: Int64; const DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemFastOpen(const hPos: Int64; var ItemHnd: TItemHandle): Boolean;
    function ItemFastResetBody(const fPos: Int64): Boolean;
    function ItemFastExists(const fieldPos: Int64; const DBItemName: SystemString): Boolean;
    function ItemFastFindFirst(const fieldPos: Int64; const DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindLast(const fieldPos: Int64; const DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindNext(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindFirst(const Path_, DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindLast(const Path_, DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindNext(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindPrev(var ItemSearchHandle: TItemSearch): Boolean;

    // item block operation
    function ItemRead(var ItemHnd: TItemHandle; const siz: Int64; var Buffers): Boolean; overload;
    function ItemSeekStart(var ItemHnd: TItemHandle): Boolean;
    function ItemSeekLast(var ItemHnd: TItemHandle): Boolean;
    function ItemSeek(var ItemHnd: TItemHandle; const ItemOffset: Int64): Boolean;
    function ItemGetPos(var ItemHnd: TItemHandle): Int64;
    function ItemGetSize(var ItemHnd: TItemHandle): Int64;
    function ItemWrite(var ItemHnd: TItemHandle; const siz: Int64; const Buffers): Boolean;

    // item stream
    function ItemReadToStream(var ItemHnd: TItemHandle; stream: TCore_Stream): Boolean; overload;
    function ItemReadToStream(hPos: Int64; stream: TCore_Stream): Boolean; overload;
    function ItemWriteFromStream(var ItemHnd: TItemHandle; stream: TCore_Stream): Boolean; overload;
    function ItemReadToStream(const Path_, DBItemName: SystemString; stream: TCore_Stream): Boolean; overload;
    function ItemWriteFromStream(const Path_, DBItemName: SystemString; stream: TCore_Stream): Boolean; overload;

    // recursion
    function RecursionSearchFirst(const InitPath, Filter: SystemString; var RecursionSearchHnd: TItemRecursionSearch): Boolean;
    function RecursionSearchNext(var RecursionSearchHnd: TItemRecursionSearch): Boolean;

    // options
    function HandlePtr: PObjectDataHandle;
    property Handle: PObjectDataHandle read HandlePtr;
    property AutoFreeHandle: Boolean read GetAutoFreeHandle write SetAutoFreeHandle;
    property IsOnlyRead: Boolean read FOnlyRead;
    property NeedCreateNew: Boolean read FNeedCreateNew;
    property ObjectName: SystemString read FObjectName write FObjectName;
    property DefaultItemID: Byte read FDefaultItemID;
    property ID: Byte read FDefaultItemID;
    property StreamEngine: TCore_Stream read FStreamEngine;
    property DBTime: TDateTime read GetDBTime;
    property OverWriteItem: Boolean read GetOverWriteItem write SetOverWriteItem;
    property SameHeaderName: Boolean read GetAllowSameHeaderName write SetAllowSameHeaderName;

    // user custom data
    property Data: Pointer read FData write FData;
  end;

  TObjectDataManagerClass = class of TObjectDataManager;

  TObjectDataManagerOfCache = class(TObjectDataManager)
  protected
    FHeaderCache, FItemBlockCache, FItemCache, FFieldCache: TInt64HashPointerList;
    FPrepareWritePool: TInt64HashObjectList;

    procedure HeaderCache_DataFreeProc(p: Pointer);
    procedure ItemBlockCache_DataFreeProc(p: Pointer);
    procedure ItemCache_DataFreeProc(p: Pointer);
    procedure FieldCache_DataFreeProc(p: Pointer);
    procedure PrepareWritePool_DataFreeProc(obj: TCore_Object);

    function CheckPreapreWrite(fPos: Int64): Boolean;

    procedure DeleteHeaderProc(fPos: Int64);

    procedure PrepareHeaderWriteProc(fPos: Int64; var wVal: THeader; var Done: Boolean);
    procedure HeaderWriteProc(fPos: Int64; var wVal: THeader);
    procedure HeaderReadProc(fPos: Int64; var rVal: THeader; var Done: Boolean);

    procedure PrepareItemBlockWriteProc(fPos: Int64; var wVal: TItemBlock; var Done: Boolean);
    procedure ItemBlockWriteProc(fPos: Int64; var wVal: TItemBlock);
    procedure ItemBlockReadProc(fPos: Int64; var rVal: TItemBlock; var Done: Boolean);

    procedure PrepareItemWriteProc(fPos: Int64; var wVal: TItem; var Done: Boolean);
    procedure ItemWriteProc(fPos: Int64; var wVal: TItem);
    procedure ItemReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);

    procedure PrepareOnlyItemRecWriteProc(fPos: Int64; var wVal: TItem; var Done: Boolean);
    procedure OnlyItemRecWriteProc(fPos: Int64; var wVal: TItem);
    procedure OnlyItemRecReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);

    procedure PrepareFieldWriteProc(fPos: Int64; var wVal: TField; var Done: Boolean);
    procedure FieldWriteProc(fPos: Int64; var wVal: TField);
    procedure FieldReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);

    procedure PrepareOnlyFieldRecWriteProc(fPos: Int64; var wVal: TField; var Done: Boolean);
    procedure OnlyFieldRecWriteProc(fPos: Int64; var wVal: TField);
    procedure OnlyFieldRecReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);

    procedure PrepareTMDBWriteProc(fPos: Int64; const wVal: PObjectDataHandle; var Done: Boolean);
    procedure TMDBWriteProc(fPos: Int64; const wVal: PObjectDataHandle);
    procedure TMDBReadProc(fPos: Int64; const rVal: PObjectDataHandle; var Done: Boolean);

    procedure DoOpenBefore; override;
  public
    destructor Destroy; override;
    procedure BuildDBCacheIntf;
    procedure FreeDBCacheIntf;
    procedure CleaupCache;
    procedure ResetCachePool(const siz_: Integer);
    procedure UpdateIO; override;
    procedure Flush();
    function CacheStatus: SystemString;
  end;

  TObjectDataMarshal = class(TCore_Object)
  protected
    FID: Byte;
    FLibList: TCore_Strings;
    FUseWildcard: Boolean;
    function GetItems(aIndex: Integer): TObjectDataManager;
    function GetNames(Name_: SystemString): TObjectDataManager;
    procedure SetItems(aIndex: Integer; const Value: TObjectDataManager);
  public
    constructor Create(dbItemID: Byte);
    destructor Destroy; override;
    function GetAbsoluteFileName(fileName: SystemString): SystemString;
    function NewDB(dbFile: SystemString; dbOnlyRead: Boolean): TObjectDataManager; overload;
    function NewDB(FixedStringL: Byte; dbFile: SystemString; dbOnlyRead: Boolean): TObjectDataManager; overload;
    function Open(dbFile: SystemString; dbOnlyRead: Boolean): TObjectDataManager;
    procedure CloseDB(Database: TObjectDataManager);
    procedure Clear;
    function Count: Integer;
    procedure Delete(aIndex: Integer);
    procedure DeleteFromName(Name_: SystemString);
    procedure UpdateAll;
    procedure Disable;
    procedure Enabled;

    property LibList: TCore_Strings read FLibList;
    property Items[aIndex: Integer]: TObjectDataManager read GetItems write SetItems;
    property Names[Name_: SystemString]: TObjectDataManager read GetNames; default;
    property UseWildcard: Boolean read FUseWildcard write FUseWildcard;
    property ID: Byte read FID write FID;
  end;

function ObjectDataMarshal: TObjectDataMarshal;
function DBMarshal: TObjectDataMarshal;
procedure CheckAndRemoveFlush(PrepareOpenFile: U_String);
procedure CheckAndRestoreFlush(PrepareOpenFile: U_String);

procedure TestObjectData();

implementation

uses Z.ZDB.ItemStream_LIB, Types, Z.MemoryStream, Z.Status;

const
  SFlush = '.~flush';
  STmp = '.tmp';
  SOld = '.old';
  C_BufferChunkSize = $FFFF;

var
  Internal_ObjectDataMarshal: TObjectDataMarshal = nil;

function ObjectDataMarshal: TObjectDataMarshal;
begin
  if Internal_ObjectDataMarshal = nil then
      Internal_ObjectDataMarshal := TObjectDataMarshal.Create(0);
  Result := Internal_ObjectDataMarshal;
end;

function DBMarshal: TObjectDataMarshal;
begin
  Result := ObjectDataMarshal();
end;

procedure CheckAndRemoveFlush(PrepareOpenFile: U_String);
var
  swapFileName: TPascalString;
begin
  if not umlFileExists(PrepareOpenFile) then
      Exit;
  swapFileName := PrepareOpenFile.Text + SFlush;

  if not umlFileExists(swapFileName) then
      Exit;
  umlDeleteFile(swapFileName);
end;

procedure CheckAndRestoreFlush(PrepareOpenFile: U_String);
var
  IOHnd: TIOHnd;
  swapFileName: TPascalString;
  swapHnd: TCore_FileStream;
  swapCompleted, swapTotal: Integer;
  swapHead: TSwapHead;
  CheckSuccessed: Boolean;
  m64: TMS64;
  m5: TMD5;
  oldDBHnd, newDBHnd: TObjectDataHandle;
begin
  if not umlFileExists(PrepareOpenFile) then
      Exit;
  swapFileName := PrepareOpenFile.Text + SFlush;

  if not umlFileExists(swapFileName) then
      Exit;

  swapHnd := nil;
  try
      swapHnd := TCore_FileStream.Create(swapFileName, fmOpenReadWrite);
  except
    DisposeObject(swapHnd);
    umlDeleteFile(swapFileName);
    Exit;
  end;
  CheckSuccessed := True;
  m64 := TMS64.CustomCreate(8192);

  // check crc
  swapCompleted := 0;
  if swapHnd.Read(swapTotal, C_Integer_Size) = C_Integer_Size then
    while swapHnd.Position < swapHnd.Size do
      begin
        if swapHnd.Read(swapHead, SizeOf(swapHead)) <> SizeOf(swapHead) then
          begin
            DoStatus('%s CRC header errors, the database will be restored to previous state', [umlGetFileName(swapFileName).Text]);
            CheckSuccessed := False;
            break;
          end;
        m64.Clear;
        if m64.CopyFrom(swapHnd, swapHead.Size) <> swapHead.Size then
          begin
            DoStatus('%s CRC data loss, the database will be restored to previous state', [umlGetFileName(swapFileName).Text]);
            CheckSuccessed := False;
            break;
          end;
        m5 := umlMD5(m64.Memory, m64.Size);
        if not umlCompareMD5(m5, swapHead.MD5) then
          begin
            DoStatus('%s CRC validation errors, the database will be restored to previous state', [umlGetFileName(swapFileName).Text]);
            CheckSuccessed := False;
            break;
          end;
        inc(swapCompleted);
      end;

  CheckSuccessed := CheckSuccessed and (swapCompleted = swapTotal);

  // restore
  if CheckSuccessed then
    begin
      DoStatus('Start backup of old database.');
      Init_TTMDB(oldDBHnd);
      db_Open(PrepareOpenFile, oldDBHnd, True);
      Init_TTMDB(newDBHnd, oldDBHnd.FixedStringL);
      db_CreateNew(PrepareOpenFile.Text + SOld, newDBHnd);
      db_CopyAllTo(oldDBHnd, newDBHnd);
      db_ClosePack(oldDBHnd);
      db_ClosePack(newDBHnd);
      DoStatus('old database "%s" rename as -> "%s"', [umlGetFileName(PrepareOpenFile).Text, umlGetFileName(PrepareOpenFile).Text + SOld]);

      DoStatus('database to restored to previous state', []);
      InitIOHnd(IOHnd);
      if umlFileOpen(PrepareOpenFile, IOHnd, False) then
        begin
          swapCompleted := 0;
          swapHnd.Position := 0;
          if swapHnd.Read(swapTotal, C_Integer_Size) = C_Integer_Size then
            while swapHnd.Position < swapHnd.Size do
              begin
                swapHnd.Read(swapHead, SizeOf(swapHead));
                m64.Clear;
                m64.CopyFrom(swapHnd, swapHead.Size);

                umlFileSeek(IOHnd, swapHead.Position);
                umlBlockWrite(IOHnd, m64.Memory^, m64.Size);
                inc(swapCompleted);
                DoStatus('CRC %s restored %d / %d', [umlMD5ToStr(swapHead.MD5).Text, swapCompleted, swapTotal]);
              end;
        end;
      umlFileClose(IOHnd);
    end
  else
    begin
      DoStatus('%s CRC error!, Start repairing this database, please wait', [umlGetFileName(swapFileName).Text]);
      Init_TTMDB(oldDBHnd);
      db_Open(PrepareOpenFile, oldDBHnd, True);

      Init_TTMDB(newDBHnd, oldDBHnd.FixedStringL);
      db_CreateNew(PrepareOpenFile.Text + STmp, newDBHnd);

      DoStatus('Start backup of old database.');
      db_CopyAllTo(oldDBHnd, newDBHnd);

      db_ClosePack(oldDBHnd);
      db_ClosePack(newDBHnd);

      umlDeleteFile(PrepareOpenFile.Text + SOld);
      umlRenameFile(PrepareOpenFile, PrepareOpenFile.Text + SOld);
      umlRenameFile(PrepareOpenFile.Text + STmp, PrepareOpenFile);

      DoStatus('old database "%s" rename as -> "%s"', [umlGetFileName(PrepareOpenFile).Text, umlGetFileName(PrepareOpenFile).Text + SOld]);
    end;

  DisposeObject(swapHnd);
  DisposeObject(m64);
  umlDeleteFile(swapFileName);
end;

procedure TObjectDataCacheItem.Write(var wVal: TItem);
begin
  Description := wVal.Description;
  ExtID := wVal.ExtID;
  FirstBlockPOS := wVal.FirstBlockPOS;
  LastBlockPOS := wVal.LastBlockPOS;
  Size := wVal.Size;
  BlockCount := wVal.BlockCount;
  CurrentBlockSeekPOS := wVal.CurrentBlockSeekPOS;
  CurrentFileSeekPOS := wVal.CurrentFileSeekPOS;
  State := wVal.State;
end;

procedure TObjectDataCacheItem.Read(var rVal: TItem);
begin
  rVal.Description := Description;
  rVal.ExtID := ExtID;
  rVal.FirstBlockPOS := FirstBlockPOS;
  rVal.LastBlockPOS := LastBlockPOS;
  rVal.Size := Size;
  rVal.BlockCount := BlockCount;
  rVal.CurrentBlockSeekPOS := CurrentBlockSeekPOS;
  rVal.CurrentFileSeekPOS := CurrentFileSeekPOS;
  rVal.State := State;
end;

procedure TObjectDataCacheField.Write(var wVal: TField);
begin
  UpFieldPOS := wVal.UpFieldPOS;
  Description := wVal.Description;
  HeaderCount := wVal.HeaderCount;
  FirstHeaderPOS := wVal.FirstHeaderPOS;
  LastHeaderPOS := wVal.LastHeaderPOS;
  State := wVal.State;
end;

procedure TObjectDataCacheField.Read(var rVal: TField);
begin
  rVal.UpFieldPOS := UpFieldPOS;
  rVal.Description := Description;
  rVal.HeaderCount := HeaderCount;
  rVal.FirstHeaderPOS := FirstHeaderPOS;
  rVal.LastHeaderPOS := LastHeaderPOS;
  rVal.State := State;
end;

function TObjectDataManager.GetAutoFreeHandle: Boolean;
begin
  if not isAbort then
      Result := FDB_HND.IOHnd.AutoFree
  else
      Result := False;
end;

procedure TObjectDataManager.SetAutoFreeHandle(const Value: Boolean);
begin
  if not isAbort then
      FDB_HND.IOHnd.AutoFree := Value;
end;

procedure TObjectDataManager.DoOpenBefore;
begin
end;

procedure TObjectDataManager.DoOpenAfter;
begin
end;

function TObjectDataManager.GetOverWriteItem: Boolean;
begin
  Result := FDB_HND.OverWriteItem;
end;

function TObjectDataManager.GetAllowSameHeaderName: Boolean;
begin
  Result := FDB_HND.AllowSameHeaderName;
end;

function TObjectDataManager.GetDBTime: TDateTime;
begin
  Result := FDB_HND.CreateTime;
end;

procedure TObjectDataManager.SetOverWriteItem(Value: Boolean);
begin
  FDB_HND.OverWriteItem := Value;
end;

procedure TObjectDataManager.SetAllowSameHeaderName(Value: Boolean);
begin
  FDB_HND.AllowSameHeaderName := Value;
end;

procedure TObjectDataManager.DBErrorProc(error: U_String);
begin
  DoStatus('error: %s - %s!', [ObjectName, error.Text]);
end;

function TObjectDataManager.DoOpen(): Boolean;
begin
  Result := False;
  try
    if StreamEngine <> nil then
      begin
        if FNeedCreateNew then
          begin
            if not db_CreateAsStream(StreamEngine, ObjectName, '', FDB_HND) then
                Exit;
          end
        else
          begin
            if not db_OpenAsStream(StreamEngine, ObjectName, FDB_HND, IsOnlyRead) then
                Exit;
          end;
      end
    else if (FNeedCreateNew) or (not umlFileExists(ObjectName)) then
      begin
        if not db_CreateNew(ObjectName, FDB_HND) then
            Exit;
      end
    else if not db_Open(ObjectName, FDB_HND, IsOnlyRead) then
        Exit;
    Result := True;
  except
  end;
end;

function TObjectDataManager.NewHandle(Stream_: TCore_Stream; const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, IsNewDB_: Boolean): Boolean;
begin
  Result := NewHandle(65, Stream_, dbFile, dbItemID, dbOnlyRead, IsNewDB_);
end;

function TObjectDataManager.NewHandle(FixedStringL: Byte; Stream_: TCore_Stream; const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, IsNewDB_: Boolean): Boolean;
begin
  Close;
  Init_TTMDB(FDB_HND, FixedStringL);
  FDB_HND.OnError := {$IFDEF FPC}@{$ENDIF FPC}DBErrorProc;

  FStreamEngine := Stream_;
  FObjectName := dbFile;
  FNeedCreateNew := IsNewDB_;
  FOnlyRead := dbOnlyRead;
  FDefaultItemID := dbItemID;

  DoOpenBefore;
  try
      FIsOpened := DoOpen();
  except
    FIsOpened := False;
    Result := False;
    Close;
  end;
  DoOpenAfter;

  Result := FIsOpened;

  OverWriteItem := True;
  SameHeaderName := False;
  AutoFreeHandle := True;
  FData := nil;
end;

constructor TObjectDataManager.Open(const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead: Boolean);
begin
  inherited Create;
  if dbOnlyRead then
      CheckAndRemoveFlush(dbFile)
  else
      CheckAndRestoreFlush(dbFile);
  NewHandle(nil, dbFile, dbItemID, dbOnlyRead, False);
end;

constructor TObjectDataManager.CreateNew(const dbFile: SystemString; const dbItemID: Byte);
begin
  inherited Create;
  CheckAndRemoveFlush(dbFile);
  NewHandle(nil, dbFile, dbItemID, False, True);
end;

constructor TObjectDataManager.CreateNew(FixedStringL: Byte; const dbFile: SystemString; const dbItemID: Byte);
begin
  inherited Create;
  CheckAndRemoveFlush(dbFile);
  NewHandle(FixedStringL, nil, dbFile, dbItemID, False, True);
end;

constructor TObjectDataManager.CreateAsStream(Stream_: TCore_Stream;
  const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, isNewDB, DestroyTimeFreeStream: Boolean);
begin
  inherited Create;
  NewHandle(Stream_, dbFile, dbItemID, dbOnlyRead, isNewDB);
  AutoFreeHandle := DestroyTimeFreeStream;
end;

constructor TObjectDataManager.CreateAsStream(FixedStringL: Byte; Stream_: TCore_Stream;
  const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, isNewDB, DestroyTimeFreeStream: Boolean);
begin
  inherited Create;
  NewHandle(FixedStringL, Stream_, dbFile, dbItemID, dbOnlyRead, isNewDB);
  AutoFreeHandle := DestroyTimeFreeStream;
end;

destructor TObjectDataManager.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TObjectDataManager.CopyTo(DestDB: TObjectDataManager): Boolean;
begin
  Result := db_CopyAllTo(FDB_HND, DestDB.FDB_HND);
end;

function TObjectDataManager.CopyToPath(DestDB: TObjectDataManager; destPath: SystemString): Boolean;
begin
  Result := db_CopyAllToDestPath(FDB_HND, DestDB.FDB_HND, destPath);
end;

function TObjectDataManager.CopyFieldToPath(fieldPos: Int64; DestDB: TObjectDataManager; destPath: SystemString): Boolean;
var
  DestFieldPos: Int64;
begin
  Result := False;
  DestDB.CreateField(destPath, '');
  if DestDB.GetPathField(destPath, DestFieldPos) then
      Result := db_CopyFieldTo('*', FDB_HND, fieldPos, DestDB.FDB_HND, DestFieldPos);
end;

function TObjectDataManager.CopyItemToPath(const Path_, DBItemName: SystemString; DestDB: TObjectDataManager; destPath: SystemString): Integer;
var
  srHnd: TItemSearch;
  sourItmHnd, destItmHnd: TItemHandle;
begin
  Result := 0;
  DestDB.CreateField(destPath, '');
  if ItemFindFirst(Path_, DBItemName, srHnd) then
    begin
      repeat
        if ItemFastOpen(srHnd.HeaderPOS, sourItmHnd) then
          begin
            if DestDB.ItemCreate(destPath, sourItmHnd.Name, sourItmHnd.Description, destItmHnd) then
              begin
                ItemCopyTo(sourItmHnd, DestDB, destItmHnd, sourItmHnd.Item.Size);
                DestDB.ItemClose(destItmHnd);
                inc(Result);
              end;
            ItemClose(sourItmHnd);
          end;
      until not ItemFindNext(srHnd);
    end;
end;

procedure TObjectDataManager.SaveToStream(stream: TCore_Stream);
var
  E: TObjectDataManager;
begin
  E := TObjectDataManager.CreateAsStream(Handle^.IOHnd.FixedStringL, stream, ObjectName, DefaultItemID, False, True, False);
  E.Reserved := Reserved;
  E.OverWriteItem := False;
  CopyTo(E);
  DisposeObject(E);
end;

procedure TObjectDataManager.SaveToZLibStream(stream: TCore_Stream);
var
  m64: TMS64;
begin
  m64 := TMS64.CustomCreate(1024 * 1024);
  SaveToStream(m64);
  m64.Position := 0;
  MaxCompressStream(m64, stream);
  DisposeObject(m64);
end;

procedure TObjectDataManager.SaveToParallelCompressionStream(stream: TCore_Stream);
var
  m64: TMS64;
begin
  m64 := TMS64.CustomCreate(1024 * 1024);
  SaveToStream(m64);
  m64.Position := 0;
  ParallelCompressMemory(TSelectCompressionMethod.scmZLIB_Max, m64, stream);
  DisposeObject(m64);
end;

procedure TObjectDataManager.ImpFromPathP(ImpPath, Path_: SystemString; IncludeSub: Boolean; Notify: TDBImpNotifyProc);
var
  fAry: U_StringArray;
  n: SystemString;
  fPos: Int64;
  fs: TCore_FileStream;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  Path_ := umlCharReplace(Path_, '\', '/').Text;
  if not DirectoryExists(Path_) then
      CreateField(Path_, '');
  fPos := GetPathFieldPos(Path_);

  fAry := umlGetFileListWithFullPath(ImpPath);
  for n in fAry do
    begin
      fs := TCore_FileStream.Create(n, fmOpenRead or fmShareDenyNone);
      ItemFastCreate(fPos, umlGetFileName(n).Text, '', itmHnd);
      itmStream := TItemStream.Create(Self, itmHnd);
      try
          itmStream.CopyFrom64(fs, fs.Size)
      except
      end;
      itmStream.CloseHandle;
      DisposeObject(fs);
      DisposeObject(itmStream);
      if Assigned(Notify) then
          Notify(Self, n, fPos, itmHnd.Item.RHeader.CurrentHeader);
    end;

  if IncludeSub then
    begin
      fAry := umlGetDirListWithFullPath(ImpPath);
      for n in fAry do
          ImpFromPathP(n, umlCombineUnixPath(Path_, umlGetLastStr(n, '\/')).Text, IncludeSub, Notify);
    end;
end;

procedure TObjectDataManager.ImpFromPath(ImpPath, Path_: SystemString; IncludeSub: Boolean);
begin
  ImpFromPathP(ImpPath, Path_, IncludeSub, nil);
end;

procedure TObjectDataManager.ImpFromFilesP(ImpFiles: TCore_Strings; Path_: SystemString; Notify: TDBImpNotifyProc);
var
  i: Integer;
  n: SystemString;
  fPos: Int64;
  fs: TCore_FileStream;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  Path_ := umlCharReplace(Path_, '\', '/').Text;
  if not DirectoryExists(Path_) then
      CreateField(Path_, '');
  fPos := GetPathFieldPos(Path_);

  for i := 0 to ImpFiles.Count - 1 do
    begin
      n := ImpFiles[i];
      fs := TCore_FileStream.Create(n, fmOpenRead or fmShareDenyNone);
      ItemFastCreate(fPos, umlGetFileName(n).Text, '', itmHnd);
      itmStream := TItemStream.Create(Self, itmHnd);
      try
          itmStream.CopyFrom64(fs, fs.Size)
      except
      end;
      itmStream.CloseHandle;
      DisposeObject(fs);
      DisposeObject(itmStream);
      if Assigned(Notify) then
          Notify(Self, n, fPos, itmHnd.Item.RHeader.CurrentHeader);
    end;
end;

procedure TObjectDataManager.ImpFromFiles(ImpFiles: TCore_Strings; Path_: SystemString);
begin
  ImpFromFilesP(ImpFiles, Path_, nil);
end;

procedure TObjectDataManager.SplitTo(RootPh, destFile: SystemString; SplitSiz: Int64);
var
  fn: U_String;
  sr: TItemRecursionSearch;
  Ph: U_String;
  itmHnd, destItmHnd: TItemHandle;
  DestDB: TObjectDataManager;
  spID: Integer;
begin
  fn := destFile;
  DestDB := TObjectDataManagerOfCache.CreateNew(FDB_HND.FixedStringL, fn, DefaultItemID);
  DestDB.OverWriteItem := False;
  spID := 1;
  if RecursionSearchFirst(RootPh, '*', sr) then
    begin
      repeat
        if sr.ReturnHeader.ID = DB_Header_Item_ID then
          begin
            Ph := GetFieldPath(sr.CurrentField.RHeader.CurrentHeader);
            ItemFastOpen(sr.ReturnHeader.CurrentHeader, itmHnd);
            DestDB.CreateField(Ph, '');
            DestDB.ItemCreate(Ph, itmHnd.Name, itmHnd.Description, destItmHnd);
            ItemCopyTo(itmHnd, DestDB, destItmHnd, itmHnd.Item.Size);
            destItmHnd.Item.RHeader.UserProperty := itmHnd.Item.RHeader.UserProperty;
            destItmHnd.Item.RHeader.CreateTime := itmHnd.Item.RHeader.CreateTime;
            destItmHnd.Item.RHeader.ModificationTime := itmHnd.Item.RHeader.ModificationTime;
            DestDB.ItemClose(destItmHnd);
            ItemClose(itmHnd);

            if DestDB.Size > SplitSiz then
              begin
                DestDB.UpdateIO;
                DisposeObject(DestDB);

                fn := umlChangeFileExt(destFile, '') + umlIntToStr(spID) + umlGetFileExt(destFile);
                inc(spID);
                DestDB := TObjectDataManagerOfCache.CreateNew(FDB_HND.FixedStringL, fn, DefaultItemID);
                DestDB.OverWriteItem := False;
              end;
          end;

      until not RecursionSearchNext(sr);
    end;
  DestDB.UpdateIO;
  DisposeObject(DestDB);
end;

procedure TObjectDataManager.SplitToZLib(RootPh, destFile: SystemString; SplitSiz: Int64);
var
  fn: U_String;
  sr: TItemRecursionSearch;
  Ph: U_String;
  itmHnd, destItmHnd: TItemHandle;
  DestDB: TObjectDataManager;
  spID: Integer;
  fs: TCore_FileStream;
begin
  fn := destFile;
  DestDB := TObjectDataManagerOfCache.CreateAsStream(FDB_HND.FixedStringL,
    TMS64.CustomCreate(1024 * 1024), '', DefaultItemID, False, True, True);
  DestDB.OverWriteItem := False;
  spID := 1;
  if RecursionSearchFirst(RootPh, '*', sr) then
    begin
      repeat
        if sr.ReturnHeader.ID = DB_Header_Item_ID then
          begin
            Ph := GetFieldPath(sr.CurrentField.RHeader.CurrentHeader);
            ItemFastOpen(sr.ReturnHeader.CurrentHeader, itmHnd);
            DestDB.CreateField(Ph, '');
            DestDB.ItemCreate(Ph, itmHnd.Name, itmHnd.Description, destItmHnd);
            ItemCopyTo(itmHnd, DestDB, destItmHnd, itmHnd.Item.Size);
            destItmHnd.Item.RHeader.UserProperty := itmHnd.Item.RHeader.UserProperty;
            destItmHnd.Item.RHeader.CreateTime := itmHnd.Item.RHeader.CreateTime;
            destItmHnd.Item.RHeader.ModificationTime := itmHnd.Item.RHeader.ModificationTime;
            DestDB.ItemClose(destItmHnd);
            ItemClose(itmHnd);

            if DestDB.Size > SplitSiz then
              begin
                DestDB.UpdateIO;
                fs := TCore_FileStream.Create(fn, fmCreate);
                DestDB.StreamEngine.Position := 0;
                MaxCompressStream(DestDB.StreamEngine, fs);
                DisposeObject(fs);
                DisposeObject(DestDB);

                fn := umlChangeFileExt(destFile, '') + umlIntToStr(spID) + umlGetFileExt(destFile);
                inc(spID);
                DestDB := TObjectDataManagerOfCache.CreateAsStream(FDB_HND.FixedStringL,
                  TMS64.CustomCreate(1024 * 1024), '', DefaultItemID, False, True, True);
                DestDB.OverWriteItem := False;
              end;
          end;

      until not RecursionSearchNext(sr);
    end;
  DestDB.UpdateIO;
  fs := TCore_FileStream.Create(fn, fmCreate);
  DestDB.StreamEngine.Position := 0;
  MaxCompressStream(DestDB.StreamEngine, fs);
  DisposeObject(fs);
  DisposeObject(DestDB);
end;

procedure TObjectDataManager.SplitToParallelCompression(RootPh, destFile: SystemString; SplitSiz: Int64);
var
  fn: U_String;
  sr: TItemRecursionSearch;
  Ph: U_String;
  itmHnd, destItmHnd: TItemHandle;
  DestDB: TObjectDataManager;
  spID: Integer;
  fs: TCore_FileStream;
begin
  fn := destFile;
  DestDB := TObjectDataManagerOfCache.CreateAsStream(FDB_HND.FixedStringL,
    TMS64.CustomCreate(1024 * 1024), '', DefaultItemID, False, True, True);
  DestDB.OverWriteItem := False;
  spID := 1;
  if RecursionSearchFirst(RootPh, '*', sr) then
    begin
      repeat
        if sr.ReturnHeader.ID = DB_Header_Item_ID then
          begin
            Ph := GetFieldPath(sr.CurrentField.RHeader.CurrentHeader);
            ItemFastOpen(sr.ReturnHeader.CurrentHeader, itmHnd);
            DestDB.CreateField(Ph, '');
            DestDB.ItemCreate(Ph, itmHnd.Name, itmHnd.Description, destItmHnd);
            ItemCopyTo(itmHnd, DestDB, destItmHnd, itmHnd.Item.Size);
            destItmHnd.Item.RHeader.UserProperty := itmHnd.Item.RHeader.UserProperty;
            destItmHnd.Item.RHeader.CreateTime := itmHnd.Item.RHeader.CreateTime;
            destItmHnd.Item.RHeader.ModificationTime := itmHnd.Item.RHeader.ModificationTime;
            DestDB.ItemClose(destItmHnd);
            ItemClose(itmHnd);

            if DestDB.Size > SplitSiz then
              begin
                DestDB.UpdateIO;
                fs := TCore_FileStream.Create(fn, fmCreate);
                DestDB.StreamEngine.Position := 0;
                ParallelCompressMemory(TSelectCompressionMethod.scmZLIB_Max, TMS64(DestDB.StreamEngine), fs);
                DisposeObject(fs);
                DisposeObject(DestDB);

                fn := umlChangeFileExt(destFile, '') + umlIntToStr(spID) + umlGetFileExt(destFile);
                inc(spID);
                DestDB := TObjectDataManagerOfCache.CreateAsStream(FDB_HND.FixedStringL,
                  TMS64.CustomCreate(1024 * 1024), '', DefaultItemID, False, True, True);
                DestDB.OverWriteItem := False;
              end;
          end;

      until not RecursionSearchNext(sr);
    end;
  DestDB.UpdateIO;
  fs := TCore_FileStream.Create(fn, fmCreate);
  DestDB.StreamEngine.Position := 0;
  ParallelCompressMemory(TSelectCompressionMethod.scmZLIB_Max, TMS64(DestDB.StreamEngine), fs);
  DisposeObject(fs);
  DisposeObject(DestDB);
end;

procedure TObjectDataManager.ExpPathToDisk(Path_, ExpPath_: SystemString; IncludeSub: Boolean);
begin
  ExpPathToDiskP(Path_, ExpPath_, IncludeSub, nil);
end;

procedure TObjectDataManager.ExpPathToDiskP(Path_, ExpPath_: SystemString; IncludeSub: Boolean; Notify: TDBExpNotifyProc);
var
  rFieldPos: Int64;
  rs: TItemRecursionSearch;
  destPath: U_String;
  itmHnd: TItemHandle;
  fs: TCore_FileStream;
  sr: TItemSearch;
begin
  destPath := ExpPath_;
  umlCreateDirectory(destPath);
  if IncludeSub then
    begin
      if GetPathField(Path_, rFieldPos) then
        begin
          if RecursionSearchFirst(Path_, '*', rs) then
            begin
              repeat
                if rs.ReturnHeader.ID = DB_Header_Field_ID then
                  begin
                    destPath := umlCombinePath(ExpPath_, GetFieldPath(rs.ReturnHeader.CurrentHeader, rFieldPos));
                    umlCreateDirectory(destPath);
                  end
                else if ItemFastOpen(rs.ReturnHeader.CurrentHeader, itmHnd) then
                  begin
                    try
                      fs := TCore_FileStream.Create(umlCombineFileName(destPath, itmHnd.Name), fmCreate);
                      ItemReadToStream(itmHnd, fs);
                      DisposeObject(fs);
                    except
                        DoStatus('failed file stream %s', [umlCombineFileName(destPath, itmHnd.Name).Text]);
                    end;
                    ItemClose(itmHnd);
                    if Assigned(Notify) then
                        Notify(Self, rs.CurrentField.RHeader.CurrentHeader, itmHnd.Item.RHeader.CurrentHeader, umlCombineFileName(destPath, itmHnd.Name));
                  end;
              until not RecursionSearchNext(rs);
            end;
        end;
    end
  else if GetPathField(Path_, rFieldPos) then
    begin
      if ItemFastFindFirst(rFieldPos, '*', sr) then
        begin
          repeat
            if ItemFastOpen(sr.HeaderPOS, itmHnd) then
              begin
                try
                  fs := TCore_FileStream.Create(umlCombineFileName(destPath, itmHnd.Name), fmCreate);
                  ItemReadToStream(itmHnd, fs);
                  DisposeObject(fs);
                except
                    DoStatus('failed file stream %s', [umlCombineFileName(destPath, itmHnd.Name).Text]);
                end;
                ItemClose(itmHnd);
                if Assigned(Notify) then
                    Notify(Self, rFieldPos, itmHnd.Item.RHeader.CurrentHeader, umlCombineFileName(destPath, itmHnd.Name));
              end;
          until not ItemFastFindNext(sr);
        end;
    end;
end;

procedure TObjectDataManager.ExpItemToDisk(Path_, DBItem, ExpFilename_: SystemString);
var
  itmHnd: TItemHandle;
  fs: TCore_FileStream;
begin
  if ItemOpen(Path_, DBItem, itmHnd) then
    begin
      try
        fs := TCore_FileStream.Create(ExpFilename_, fmCreate);
        ItemReadToStream(itmHnd, fs);
        DisposeObject(fs);
      except
          DoStatus('failed file stream %s', [ExpFilename_]);
      end;
      ItemClose(itmHnd);
    end;
end;

function TObjectDataManager.Is_BACKUP_Mode: Boolean;
begin
{$IFDEF ZDB_BACKUP}
  Result := FDB_HND.IOHnd.Handle is TReliableFileStream;
{$ELSE ZDB_BACKUP}
  Result := False;
{$ENDIF ZDB_BACKUP}
end;

function TObjectDataManager.Is_Flush_Mode: Boolean;
begin
{$IFDEF ZDB_PHYSICAL_FLUSH}
  Result := (not FDB_HND.IOHnd.IsOnlyRead)
    and (FDB_HND.IOHnd.IsOpen)
    and (FDB_HND.IOHnd.Handle is TReliableFileStream);
{$ELSE ZDB_PHYSICAL_FLUSH}
  Result := False;
{$ENDIF ZDB_PHYSICAL_FLUSH}
end;

function TObjectDataManager.isAbort: Boolean;
begin
  Result := not FIsOpened;
end;

function TObjectDataManager.Close: Boolean;
begin
  Result := db_ClosePack(FDB_HND);
end;

function TObjectDataManager.ErrorNo: Int64;
begin
  Result := FDB_HND.State;
end;

function TObjectDataManager.Modification: Boolean;
begin
  Result := FDB_HND.IOHnd.ChangeFromWrite;
end;

function TObjectDataManager.Size: Int64;
begin
  Result := FDB_HND.IOHnd.Size;
end;

function TObjectDataManager.IOReadSize: Int64;
begin
  Result := FDB_HND.IOHnd.IORead;
end;

function TObjectDataManager.IOWriteSize: Int64;
begin
  Result := FDB_HND.IOHnd.IOWrite;
end;

procedure TObjectDataManager.SetID(const ID_: Byte);
begin
  FDefaultItemID := ID_;
end;

procedure TObjectDataManager.UpdateIO;
begin
  db_Update(FDB_HND);
end;

procedure TObjectDataManager.SetReserved(Value: TObjectDataHandle_Reserved_Data);
begin
  FDB_HND.ReservedData := Value;
  FDB_HND.IOHnd.ChangeFromWrite := True;
end;

function TObjectDataManager.GetReserved: TObjectDataHandle_Reserved_Data;
begin
  Result := FDB_HND.ReservedData;
end;

function TObjectDataManager.CreateField(const FieldName_, Field_Desc: SystemString): Boolean;
begin
  Result := db_CreateField(FieldName_, Field_Desc, FDB_HND);
end;

function TObjectDataManager.CreateRootField(const RootName: SystemString): Boolean;
begin
  Result := db_CreateRootField(RootName, RootName, FDB_HND);
end;

function TObjectDataManager.DirectoryExists(const FieldName_: SystemString): Boolean;
var
  Field: TFieldHandle;
begin
  Result := db_GetField(FieldName_, Field, FDB_HND);
end;

function TObjectDataManager.FastDelete(const fieldPos: Int64; const fPos: Int64): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Init_TField(FieldHnd);
  Result := False;
  if dbField_ReadRec(fieldPos, FDB_HND.IOHnd, FieldHnd) then
      Result := dbField_DeleteHeader(fPos, fieldPos, FDB_HND.IOHnd, FieldHnd);
end;

function TObjectDataManager.FastFieldExists(const fieldPos: Int64; const FieldName: SystemString): Boolean;
var
  FieldSearch: TFieldSearch;
begin
  Result := FieldFastFindFirst(fieldPos, FieldName, FieldSearch);
end;

function TObjectDataManager.FastFieldCreate(const fieldPos: Int64; const FieldName, FieldDescription: SystemString; var NewFieldPos: Int64): Boolean;
var
  NewField: TField;
begin
  Init_TField(NewField);
  NewField.Description := FieldDescription;
  Result := dbField_CreateField(FieldName, fieldPos, FDB_HND.IOHnd, NewField);
  NewFieldPos := NewField.RHeader.CurrentHeader;
end;

function TObjectDataManager.RootField: Int64;
begin
  Result := FDB_HND.DefaultFieldPOS;
end;

function TObjectDataManager.SetRootField(const RootName: SystemString): Boolean;
begin
  Result := db_SetCurrentRootField(RootName, FDB_HND);
end;

function TObjectDataManager.GetRootFieldPos(const RootName: SystemString): Int64;
var
  f: TFieldHandle;
begin
  Init_TField(f);
  if db_GetRootField(RootName, f, FDB_HND) then
      Result := f.RHeader.CurrentHeader
  else
      Result := -1;
end;

function TObjectDataManager.FieldRename(const fieldPos: Int64; const NewFieldName, NewFieldDescription: SystemString): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Result := False;
  if not umlExistsChar(NewFieldName, '\/') then
    begin
      Init_TField(FieldHnd);
      if dbField_ReadRec(fieldPos, FDB_HND.IOHnd, FieldHnd) then
        begin
          if (not FastFieldExists(FieldHnd.UpFieldPOS, NewFieldName)) and (FieldHnd.RHeader.CurrentHeader <> FDB_HND.DefaultFieldPOS) then
            begin
              FieldHnd.RHeader.Name := NewFieldName;
              FieldHnd.Description := NewFieldDescription;
              Result := dbField_WriteRec(fieldPos, FDB_HND.IOHnd, FieldHnd);
            end;
        end;
    end;
end;

function TObjectDataManager.FieldDelete(const Path_: SystemString; const FieldName: SystemString): Boolean;
begin
  Result := db_DeleteField(Path_, FieldName, FDB_HND);
end;

function TObjectDataManager.FieldExists(const Path_: SystemString; const FieldName: SystemString): Boolean;
var
  FieldSearch: TFieldSearch;
begin
  Result := FieldFindFirst(Path_, FieldName, FieldSearch);
end;

function TObjectDataManager.FieldExists(const Path_: SystemString): Boolean;
var
  fieldPos: Int64;
begin
  Result := GetPathField(Path_, fieldPos);
end;

function TObjectDataManager.FieldFastFindFirst(const fieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := db_FastFindFirstField(fieldPos, Filter, FieldSearchHandle, FDB_HND);
end;

function TObjectDataManager.FieldFastFindLast(const fieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := db_FastFindLastField(fieldPos, Filter, FieldSearchHandle, FDB_HND);
end;

function TObjectDataManager.FieldFastFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := db_FastFindNextField(FieldSearchHandle, FDB_HND);
end;

function TObjectDataManager.FieldFastFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := db_FastFindPrevField(FieldSearchHandle, FDB_HND);
end;

function TObjectDataManager.FieldFindFirst(const Path_, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := db_FindFirstField(Path_, Filter, FieldSearchHandle, FDB_HND);
end;

function TObjectDataManager.FieldFindLast(const Path_, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := db_FindLastField(Path_, Filter, FieldSearchHandle, FDB_HND);
end;

function TObjectDataManager.FieldFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := db_FindNextField(FieldSearchHandle, FDB_HND);
end;

function TObjectDataManager.FieldFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := db_FindPrevField(FieldSearchHandle, FDB_HND);
end;

function TObjectDataManager.FieldMove(const Path_, FieldName, destPath: SystemString): Boolean;
begin
  Result := db_MoveField(Path_, FieldName, destPath, FDB_HND);
end;

function TObjectDataManager.GetFieldData(const fieldPos: Int64; var dest: TFieldHandle): Boolean;
begin
  Init_TField(dest);
  Result := dbField_ReadRec(fieldPos, FDB_HND.IOHnd, dest);
end;

function TObjectDataManager.GetFieldPath(const fieldPos: Int64): SystemString;
var
  ReturnPath: U_String;
begin
  if db_GetPath(fieldPos, FDB_HND.DefaultFieldPOS, FDB_HND, ReturnPath) then
      Result := ReturnPath
  else
      Result := '';
end;

function TObjectDataManager.GetFieldPath(const fieldPos, RootFieldPos: Int64): SystemString;
var
  ReturnPath: U_String;
begin
  if db_GetPath(fieldPos, RootFieldPos, FDB_HND, ReturnPath) then
      Result := ReturnPath
  else
      Result := '';
end;

function TObjectDataManager.GetPathField(const Path_: SystemString; var dest: Int64): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Result := db_GetField(Path_, FieldHnd, FDB_HND);
  if Result then
      dest := FieldHnd.RHeader.CurrentHeader;
end;

function TObjectDataManager.GetPathFieldPos(const Path_: SystemString): Int64;
begin
  if not GetPathField(Path_, Result) then
      Result := 0;
end;

function TObjectDataManager.GetPathFieldHeaderCount(const Path_: SystemString): Int64;
var
  FieldHnd: TFieldHandle;
begin
  Result := 0;
  if db_GetField(Path_, FieldHnd, FDB_HND) then
      Result := FieldHnd.HeaderCount;
end;

function TObjectDataManager.GetPathFieldHeaderNames(const Path_: SystemString; var output: U_StringArray): Boolean;
var
  FieldHnd: TFieldHandle;
  i: Integer;
  h: Z.ZDB.ObjectData_LIB.THeader;
begin
  Result := False;
  SetLength(output, 0);
  if not db_GetField(Path_, FieldHnd, FDB_HND) then
      Exit;
  SetLength(output, FieldHnd.HeaderCount);
  i := 0;

  if FieldHnd.HeaderCount > 0 then
    if dbHeader_ReadRec(FieldHnd.FirstHeaderPOS, FDB_HND.IOHnd, h) then
      begin
        repeat
          output[i] := h.Name;
          inc(i);
          if h.PositionID in [DB_Header_1, DB_Header_Last] then
              break;
          dbHeader_ReadRec(h.NextHeader, FDB_HND.IOHnd, h);
        until False;
      end;
  Result := True;
end;

function TObjectDataManager.GetHeaderModificationTime(const hPos: Int64): TDateTime;
var
  h: THeader;
begin
  if dbHeader_ReadRec(hPos, FDB_HND.IOHnd, h) then
      Result := h.ModificationTime
  else
      Result := umlDefaultTime;
end;

function TObjectDataManager.GetFirstHeaderFromField(fieldPos: Int64; var h: THeader): Boolean;
var
  f: TField;
begin
  Result := (dbField_ReadRec(fieldPos, FDB_HND.IOHnd, f)) and (f.HeaderCount > 0);
  if Result then
    begin
      Result := GetHeader(f.FirstHeaderPOS, h);
    end;
end;

function TObjectDataManager.GetLastHeaderFromField(fieldPos: Int64; var h: THeader): Boolean;
var
  f: TField;
begin
  Result := (dbField_ReadRec(fieldPos, FDB_HND.IOHnd, f)) and (f.HeaderCount > 0);
  if Result then
      Result := GetHeader(f.LastHeaderPOS, h);
end;

function TObjectDataManager.GetHeader(hPos: Int64; var h: THeader): Boolean;
begin
  Result := dbHeader_ReadRec(hPos, FDB_HND.IOHnd, h);
end;

function TObjectDataManager.GetItemSize(const Path_, DBItemName: SystemString): Int64;
var
  DBItemHandle: TItemHandle;
begin
  Init_TTMDBItemHandle(DBItemHandle);
  if db_GetItem(Path_, DBItemName, FDefaultItemID, DBItemHandle.Item, FDB_HND) then
      Result := DBItemHandle.Item.Size
  else
      Result := 0;
end;

function TObjectDataManager.ItemCreate(const Path_, DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
{
  It can automatically create a path
}
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := db_ItemCreate(Path_, DBItemName, DBItemDescription, FDefaultItemID, ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemDelete(const Path_, DBItemName: SystemString): Boolean;
begin
  Result := db_DeleteItem(Path_, DBItemName, FDefaultItemID, FDB_HND);
end;

function TObjectDataManager.ItemExists(const Path_, DBItemName: SystemString): Boolean;
var
  ItemSearchHnd: TItemSearch;
begin
  Init_TTMDBSearchItem(ItemSearchHnd);
  Result := db_FindFirstItem(Path_, DBItemName, FDefaultItemID, ItemSearchHnd, FDB_HND);
end;

function TObjectDataManager.ItemOpen(const Path_, DBItemName: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := db_ItemOpen(Path_, DBItemName, FDefaultItemID, ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemAutoOpenOrCreate(const Path_, DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  if ItemExists(Path_, DBItemName) then
      Result := ItemOpen(Path_, DBItemName, ItemHnd)
  else
      Result := ItemCreate(Path_, DBItemName, DBItemDescription, ItemHnd);
end;

function TObjectDataManager.ItemUpdate(var ItemHnd: TItemHandle): Boolean;
begin
  Result := db_ItemUpdate(ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemClose(var ItemHnd: TItemHandle): Boolean;
begin
  Result := db_ItemClose(ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemCopyTo(var ItemHnd: TItemHandle; DestDB: TObjectDataManager; var DestItemHandle: TItemHandle; const CopySize: Int64): Boolean;
var
  i: Integer;
  buff: array [0 .. C_BufferChunkSize] of Byte;
begin
  Result := False;
  if CopySize > C_BufferChunkSize then
    begin
      for i := 1 to (CopySize div C_BufferChunkSize) do
        begin
          if not ItemRead(ItemHnd, C_BufferChunkSize, buff) then
              Exit;
          if not DestDB.ItemWrite(DestItemHandle, C_BufferChunkSize, buff) then
              Exit;
        end;
      if (CopySize mod C_BufferChunkSize) > 0 then
        begin
          if not ItemRead(ItemHnd, CopySize mod C_BufferChunkSize, buff) then
              Exit;
          if not DestDB.ItemWrite(DestItemHandle, CopySize mod C_BufferChunkSize, buff) then
              Exit;
        end;
    end
  else
    begin
      if CopySize > 0 then
        begin
          if not ItemRead(ItemHnd, CopySize, buff) then
              Exit;
          if not DestDB.ItemWrite(DestItemHandle, CopySize, buff) then
              Exit;
        end;
    end;
  Result := True;
end;

function TObjectDataManager.ItemMove(const Path_, ItemName, destPath: SystemString): Boolean;
begin
  Result := db_MoveItem(Path_, ItemName, destPath, FDefaultItemID, FDB_HND);
end;

function TObjectDataManager.ItemRename(const fieldPos: Int64; var ItemHnd: TItemHandle; const NewName, NewDescription: SystemString): Boolean;
begin
  Result := db_ItemReName(fieldPos, NewName, NewDescription, ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemFastInsertNew(const fieldPos, InsertHeaderPos: Int64; const DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := db_ItemFastInsertNew(DBItemName, DBItemDescription, fieldPos, InsertHeaderPos, FDefaultItemID, ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemFastCreate(const fPos: Int64; const DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := db_ItemFastCreate(DBItemName, DBItemDescription, fPos, FDefaultItemID, ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemFastOpen(const hPos: Int64; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := db_ItemFastOpen(hPos, FDefaultItemID, ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemFastResetBody(const fPos: Int64): Boolean;
var
  ItemHnd: TItemHandle;
begin
  Result := ItemFastOpen(fPos, ItemHnd)
    and db_ItemBodyReset(ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemFastExists(const fieldPos: Int64; const DBItemName: SystemString): Boolean;
var
  ItemSearchHnd: TItemSearch;
begin
  Init_TTMDBSearchItem(ItemSearchHnd);
  Result := db_FastFindFirstItem(fieldPos, DBItemName, FDefaultItemID, ItemSearchHnd, FDB_HND);
end;

function TObjectDataManager.ItemFastFindFirst(const fieldPos: Int64; const DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := db_FastFindFirstItem(fieldPos, DBItemName, FDefaultItemID, ItemSearchHandle, FDB_HND);
end;

function TObjectDataManager.ItemFastFindLast(const fieldPos: Int64; const DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := db_FastFindLastItem(fieldPos, DBItemName, FDefaultItemID, ItemSearchHandle, FDB_HND);
end;

function TObjectDataManager.ItemFastFindNext(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := db_FastFindNextItem(ItemSearchHandle, FDefaultItemID, FDB_HND);
end;

function TObjectDataManager.ItemFastFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := db_FastFindPrevItem(ItemSearchHandle, FDefaultItemID, FDB_HND);
end;

function TObjectDataManager.ItemFindFirst(const Path_, DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := db_FindFirstItem(Path_, DBItemName, FDefaultItemID, ItemSearchHandle, FDB_HND);
end;

function TObjectDataManager.ItemFindLast(const Path_, DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := db_FindLastItem(Path_, DBItemName, FDefaultItemID, ItemSearchHandle, FDB_HND);
end;

function TObjectDataManager.ItemFindNext(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := db_FindNextItem(ItemSearchHandle, FDefaultItemID, FDB_HND);
end;

function TObjectDataManager.ItemFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := db_FindPrevItem(ItemSearchHandle, FDefaultItemID, FDB_HND);
end;

function TObjectDataManager.ItemRead(var ItemHnd: TItemHandle; const siz: Int64; var Buffers): Boolean;
begin
  Result := db_ItemRead(siz, Buffers, ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemSeekStart(var ItemHnd: TItemHandle): Boolean;
begin
  Result := db_ItemSeekStartPos(ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemSeekLast(var ItemHnd: TItemHandle): Boolean;
begin
  Result := db_ItemSeekLastPos(ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemSeek(var ItemHnd: TItemHandle; const ItemOffset: Int64): Boolean;
var
  siz: Integer;
begin
  siz := db_ItemGetSize(ItemHnd, FDB_HND);
  if ItemOffset > siz then
      Result := db_AppendItemSize(ItemHnd, ItemOffset - siz, FDB_HND)
  else if ItemOffset = siz then
      Result := db_ItemSeekLastPos(ItemHnd, FDB_HND)
  else if ItemOffset = 0 then
      Result := db_ItemSeekStartPos(ItemHnd, FDB_HND)
  else
      Result := db_ItemSeekPos(ItemOffset, ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemGetPos(var ItemHnd: TItemHandle): Int64;
begin
  Result := db_ItemGetPos(ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemGetSize(var ItemHnd: TItemHandle): Int64;
begin
  Result := db_ItemGetSize(ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemWrite(var ItemHnd: TItemHandle; const siz: Int64; const Buffers): Boolean;
begin
  Result := db_ItemWrite(siz, Buffers, ItemHnd, FDB_HND);
end;

function TObjectDataManager.ItemReadToStream(var ItemHnd: TItemHandle; stream: TCore_Stream): Boolean;
var
  sour: TItemStream;
begin
  sour := TItemStream.Create(Self, ItemHnd);
  sour.SeekStart();
  Result := stream.CopyFrom(sour, sour.Size) = sour.Size;
  DisposeObject(sour);
end;

function TObjectDataManager.ItemReadToStream(hPos: Int64; stream: TCore_Stream): Boolean;
var
  ItemHnd: TItemHandle;
begin
  Result := ItemFastOpen(hPos, ItemHnd);
  if not Result then
      Exit;
  Result := ItemReadToStream(ItemHnd, stream);
end;

function TObjectDataManager.ItemWriteFromStream(var ItemHnd: TItemHandle; stream: TCore_Stream): Boolean;
var
  sour: TItemStream;
begin
  sour := TItemStream.Create(Self, ItemHnd);
  sour.SeekStart();
  stream.Position := 0;
  Result := sour.CopyFrom(stream, stream.Size) = stream.Size;
  sour.UpdateHandle;
  DisposeObject(sour);
end;

function TObjectDataManager.ItemReadToStream(const Path_, DBItemName: SystemString; stream: TCore_Stream): Boolean;
var
  itmHnd: TItemHandle;
begin
  Result := False;
  if not ItemOpen(Path_, DBItemName, itmHnd) then
      Exit;
  Result := ItemReadToStream(itmHnd, stream);
end;

function TObjectDataManager.ItemWriteFromStream(const Path_, DBItemName: SystemString; stream: TCore_Stream): Boolean;
var
  itmHnd: TItemHandle;
begin
  Result := False;
  if ItemCreate(Path_, DBItemName, DBItemName, itmHnd) then
      Result := ItemWriteFromStream(itmHnd, stream);
end;

function TObjectDataManager.RecursionSearchFirst(const InitPath, Filter: SystemString; var RecursionSearchHnd: TItemRecursionSearch): Boolean;
begin
  Init_TTMDBRecursionSearch(RecursionSearchHnd);
  Result := db_RecursionSearchFirst(InitPath, Filter, RecursionSearchHnd, FDB_HND);
end;

function TObjectDataManager.RecursionSearchNext(var RecursionSearchHnd: TItemRecursionSearch): Boolean;
begin
  Result := db_RecursionSearchNext(RecursionSearchHnd, FDB_HND);
end;

function TObjectDataManager.HandlePtr: PObjectDataHandle;
begin
  Result := @FDB_HND;
end;

procedure TObjectDataManagerOfCache.HeaderCache_DataFreeProc(p: Pointer);
begin
  Dispose(PObjectDataCacheHeader(p));
end;

procedure TObjectDataManagerOfCache.ItemBlockCache_DataFreeProc(p: Pointer);
begin
  Dispose(PObjectDataCacheItemBlock(p));
end;

procedure TObjectDataManagerOfCache.ItemCache_DataFreeProc(p: Pointer);
begin
  Dispose(PObjectDataCacheItem(p));
end;

procedure TObjectDataManagerOfCache.FieldCache_DataFreeProc(p: Pointer);
begin
  Dispose(PObjectDataCacheField(p));
end;

procedure TObjectDataManagerOfCache.PrepareWritePool_DataFreeProc(obj: TCore_Object);
begin
  DisposeObject(obj);
end;

function TObjectDataManagerOfCache.CheckPreapreWrite(fPos: Int64): Boolean;
begin
  Result := (FDB_HND.IOHnd.Handle is TReliableFileStream) and (not FDB_HND.IOHnd.IsOnlyRead) and (FDB_HND.IOHnd.IsOpen) and (fPos < FDB_HND.IOHnd.Size);
end;

procedure TObjectDataManagerOfCache.DeleteHeaderProc(fPos: Int64);
var
  h: THeader;
  itm: TItem;
  bPos: Int64;
  block: TItemBlock;
begin
  if not dbHeader_ReadRec(fPos, FDB_HND.IOHnd, h) then
    begin
      FPrepareWritePool.Delete(fPos);
      FHeaderCache.Delete(fPos);
      Exit;
    end;

  if h.ID = DB_Header_Field_ID then
      FFieldCache.Delete(h.DataPosition)
  else if h.ID = DB_Header_Item_ID then
    begin
      itm.RHeader := h;
      if dbItem_OnlyReadItemRec(h.DataPosition, FDB_HND.IOHnd, itm) then
        begin
          bPos := itm.FirstBlockPOS;
          while dbItem_OnlyReadItemBlockRec(bPos, FDB_HND.IOHnd, block) do
            begin
              FPrepareWritePool.Delete(bPos);
              FItemBlockCache.Delete(bPos);
              bPos := block.NextBlockPOS;
              if bPos = itm.LastBlockPOS then
                  break;
            end;
        end;
      FItemCache.Delete(h.DataPosition);
    end;

  FPrepareWritePool.Delete(fPos);
  FPrepareWritePool.Delete(h.DataPosition);
  FHeaderCache.Delete(fPos);
end;

procedure TObjectDataManagerOfCache.PrepareHeaderWriteProc(fPos: Int64; var wVal: THeader; var Done: Boolean);
var
  m64: TMS64;
  Hnd: TIOHnd;
begin
  Done := False;
  if not CheckPreapreWrite(fPos) then
      Exit;
  m64 := TMS64(FPrepareWritePool[fPos]);
  if m64 = nil then
    begin
      m64 := TMS64.CustomCreate(Get_DB_HeaderL(FDB_HND.IOHnd));
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  Hnd.FixedStringL := FDB_HND.FixedStringL;
  umlFileOpenAsStream('', m64, Hnd, False);
  dbHeader_WriteRec(0, Hnd, wVal);
  umlFileClose(Hnd);
  if m64.Position <> Get_DB_HeaderL(FDB_HND.IOHnd) then
      RaiseInfo('preapre write error!');
  m64.Position := 0;

  Done := True;
end;

procedure TObjectDataManagerOfCache.HeaderWriteProc(fPos: Int64; var wVal: THeader);
var
  p: PObjectDataCacheHeader;
begin
  p := PObjectDataCacheHeader(FHeaderCache[wVal.CurrentHeader]);
  if p = nil then
    begin
      new(p);
      p^ := wVal;
      FHeaderCache.Add(wVal.CurrentHeader, p, False);
    end
  else
      p^ := wVal;

  p^.State := DB_Header_ok;
end;

procedure TObjectDataManagerOfCache.HeaderReadProc(fPos: Int64; var rVal: THeader; var Done: Boolean);
var
  p: PObjectDataCacheHeader;
  m64: TMS64;
  Hnd: TIOHnd;
begin
  p := PObjectDataCacheHeader(FHeaderCache[fPos]);
  Done := p <> nil;
  if not Done then
    begin
      m64 := TMS64(FPrepareWritePool[fPos]);
      if m64 <> nil then
        begin
          InitIOHnd(Hnd);
          Hnd.FixedStringL := FDB_HND.FixedStringL;
          umlFileOpenAsStream('', m64, Hnd, False);
          Done := dbHeader_ReadRec(0, Hnd, rVal);
          umlFileClose(Hnd);
          m64.Position := 0;
          if Done then
            begin
              new(p);
              p^ := rVal;
              FHeaderCache.Add(rVal.CurrentHeader, p, False);
              p^.State := DB_Header_ok;
            end;
        end;
    end
  else
      rVal := p^;
end;

procedure TObjectDataManagerOfCache.PrepareItemBlockWriteProc(fPos: Int64; var wVal: TItemBlock; var Done: Boolean);
var
  m64: TMS64;
  Hnd: TIOHnd;
begin
  Done := False;
  if not CheckPreapreWrite(fPos) then
      Exit;
  m64 := TMS64(FPrepareWritePool[fPos]);
  if m64 = nil then
    begin
      m64 := TMS64.CustomCreate(Get_DB_BlockL(FDB_HND.IOHnd));
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  Hnd.FixedStringL := FDB_HND.FixedStringL;
  umlFileOpenAsStream('', m64, Hnd, False);
  dbItem_OnlyWriteItemBlockRec(0, Hnd, wVal);
  umlFileClose(Hnd);
  if m64.Position <> Get_DB_BlockL(FDB_HND.IOHnd) then
      RaiseInfo('preapre write error!');
  m64.Position := 0;

  Done := True;
end;

procedure TObjectDataManagerOfCache.ItemBlockWriteProc(fPos: Int64; var wVal: TItemBlock);
var
  p: PObjectDataCacheItemBlock;
begin
  p := PObjectDataCacheItemBlock(FItemBlockCache[wVal.CurrentBlockPOS]);
  if p = nil then
    begin
      new(p);
      p^ := wVal;
      FItemBlockCache.Add(wVal.CurrentBlockPOS, p, False);
    end
  else
      p^ := wVal;

  p^.State := DB_Item_ok;
end;

procedure TObjectDataManagerOfCache.ItemBlockReadProc(fPos: Int64; var rVal: TItemBlock; var Done: Boolean);
var
  p: PObjectDataCacheItemBlock;
  m64: TMS64;
  Hnd: TIOHnd;
begin
  p := PObjectDataCacheItemBlock(FItemBlockCache[fPos]);
  Done := p <> nil;
  if not Done then
    begin
      m64 := TMS64(FPrepareWritePool[fPos]);
      if m64 <> nil then
        begin
          InitIOHnd(Hnd);
          Hnd.FixedStringL := FDB_HND.FixedStringL;
          umlFileOpenAsStream('', m64, Hnd, False);
          Done := dbItem_OnlyReadItemBlockRec(0, Hnd, rVal);
          umlFileClose(Hnd);
          m64.Position := 0;
          if Done then
            begin
              new(p);
              p^ := rVal;
              FItemBlockCache.Add(rVal.CurrentBlockPOS, p, False);
              p^.State := DB_Item_ok;
            end;
        end;
    end
  else
      rVal := p^;
end;

procedure TObjectDataManagerOfCache.PrepareItemWriteProc(fPos: Int64; var wVal: TItem; var Done: Boolean);
begin
  Done := False;
  if not CheckPreapreWrite(fPos) then
      Exit;

  PrepareOnlyItemRecWriteProc(wVal.RHeader.DataPosition, wVal, Done);
  PrepareHeaderWriteProc(fPos, wVal.RHeader, Done);
end;

procedure TObjectDataManagerOfCache.ItemWriteProc(fPos: Int64; var wVal: TItem);
begin
  HeaderWriteProc(fPos, wVal.RHeader);
  OnlyItemRecWriteProc(wVal.RHeader.DataPosition, wVal);
end;

procedure TObjectDataManagerOfCache.ItemReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);
begin
  HeaderReadProc(fPos, rVal.RHeader, Done);

  if not Done then
    begin
      FDB_HND.IOHnd.Data := nil;
      Done := dbHeader_ReadRec(fPos, FDB_HND.IOHnd, rVal.RHeader);
      FDB_HND.IOHnd.Data := @FDB_HND;

      if Done then
          HeaderWriteProc(fPos, rVal.RHeader)
      else
          Exit;
    end;

  OnlyItemRecReadProc(rVal.RHeader.DataPosition, rVal, Done);
end;

procedure TObjectDataManagerOfCache.PrepareOnlyItemRecWriteProc(fPos: Int64; var wVal: TItem; var Done: Boolean);
var
  m64: TMS64;
  Hnd: TIOHnd;
begin
  Done := False;
  if not CheckPreapreWrite(fPos) then
      Exit;
  m64 := TMS64(FPrepareWritePool[fPos]);
  if m64 = nil then
    begin
      m64 := TMS64.CustomCreate(Get_DB_ItemL(FDB_HND.IOHnd));
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  Hnd.FixedStringL := FDB_HND.FixedStringL;
  umlFileOpenAsStream('', m64, Hnd, False);
  dbItem_OnlyWriteItemRec(0, Hnd, wVal);
  umlFileClose(Hnd);
  if m64.Position <> Get_DB_ItemL(FDB_HND.IOHnd) then
      RaiseInfo('preapre write error!');
  m64.Position := 0;

  Done := True;
end;

procedure TObjectDataManagerOfCache.OnlyItemRecWriteProc(fPos: Int64; var wVal: TItem);
var
  p: PObjectDataCacheItem;
begin
  p := PObjectDataCacheItem(FItemCache[fPos]);
  if p = nil then
    begin
      new(p);
      p^.Write(wVal);
      FItemCache.Add(fPos, p, False);
    end
  else
      p^.Write(wVal);

  p^.State := DB_Item_ok;
end;

procedure TObjectDataManagerOfCache.OnlyItemRecReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);
var
  p: PObjectDataCacheItem;
  m64: TMS64;
  Hnd: TIOHnd;
begin
  p := PObjectDataCacheItem(FItemCache[fPos]);
  Done := p <> nil;
  if not Done then
    begin
      m64 := TMS64(FPrepareWritePool[fPos]);
      if m64 <> nil then
        begin
          InitIOHnd(Hnd);
          Hnd.FixedStringL := FDB_HND.FixedStringL;
          umlFileOpenAsStream('', m64, Hnd, False);
          Done := dbItem_OnlyReadItemRec(0, Hnd, rVal);
          umlFileClose(Hnd);
          m64.Position := 0;
          if Done then
            begin
              new(p);
              p^.Read(rVal);
              FItemCache.Add(fPos, p, False);
              p^.State := DB_Item_ok;
            end;
        end;
    end
  else
      p^.Read(rVal);
end;

procedure TObjectDataManagerOfCache.PrepareFieldWriteProc(fPos: Int64; var wVal: TField; var Done: Boolean);
begin
  Done := False;
  if not CheckPreapreWrite(fPos) then
      Exit;

  PrepareOnlyFieldRecWriteProc(wVal.RHeader.DataPosition, wVal, Done);
  PrepareHeaderWriteProc(fPos, wVal.RHeader, Done);
end;

procedure TObjectDataManagerOfCache.FieldWriteProc(fPos: Int64; var wVal: TField);
begin
  HeaderWriteProc(fPos, wVal.RHeader);
  OnlyFieldRecWriteProc(wVal.RHeader.DataPosition, wVal);
end;

procedure TObjectDataManagerOfCache.FieldReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);
begin
  HeaderReadProc(fPos, rVal.RHeader, Done);

  if not Done then
    begin
      FDB_HND.IOHnd.Data := nil;
      Done := dbHeader_ReadRec(fPos, FDB_HND.IOHnd, rVal.RHeader);
      FDB_HND.IOHnd.Data := @FDB_HND;

      if Done then
          HeaderWriteProc(fPos, rVal.RHeader)
      else
          Exit;
    end;

  OnlyFieldRecReadProc(rVal.RHeader.DataPosition, rVal, Done);
end;

procedure TObjectDataManagerOfCache.PrepareOnlyFieldRecWriteProc(fPos: Int64; var wVal: TField; var Done: Boolean);
var
  m64: TMS64;
  Hnd: TIOHnd;
begin
  Done := False;
  if not CheckPreapreWrite(fPos) then
      Exit;
  m64 := TMS64(FPrepareWritePool[fPos]);
  if m64 = nil then
    begin
      m64 := TMS64.CustomCreate(Get_DB_FieldL(FDB_HND.IOHnd));
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  Hnd.FixedStringL := FDB_HND.FixedStringL;
  umlFileOpenAsStream('', m64, Hnd, False);
  dbField_OnlyWriteFieldRec(0, Hnd, wVal);
  umlFileClose(Hnd);
  if m64.Position <> Get_DB_FieldL(FDB_HND.IOHnd) then
      RaiseInfo('preapre write error!');
  m64.Position := 0;

  Done := True;
end;

procedure TObjectDataManagerOfCache.OnlyFieldRecWriteProc(fPos: Int64; var wVal: TField);
var
  p: PObjectDataCacheField;
begin
  p := PObjectDataCacheField(FFieldCache[fPos]);
  if p = nil then
    begin
      new(p);
      p^.Write(wVal);
      FFieldCache.Add(fPos, p, False);
    end
  else
      p^.Write(wVal);

  p^.State := DB_Field_ok;
end;

procedure TObjectDataManagerOfCache.OnlyFieldRecReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);
var
  p: PObjectDataCacheField;
  m64: TMS64;
  Hnd: TIOHnd;
begin
  p := PObjectDataCacheField(FFieldCache[fPos]);
  Done := p <> nil;
  if not Done then
    begin
      m64 := TMS64(FPrepareWritePool[fPos]);
      if m64 <> nil then
        begin
          InitIOHnd(Hnd);
          Hnd.FixedStringL := FDB_HND.FixedStringL;
          umlFileOpenAsStream('', m64, Hnd, False);
          Done := dbField_OnlyReadFieldRec(0, Hnd, rVal);
          umlFileClose(Hnd);
          m64.Position := 0;
          if Done then
            begin
              new(p);
              p^.Read(rVal);
              FFieldCache.Add(fPos, p, False);
              p^.State := DB_Item_ok;
            end;
        end;
    end
  else
      p^.Read(rVal);
end;

procedure TObjectDataManagerOfCache.PrepareTMDBWriteProc(fPos: Int64; const wVal: PObjectDataHandle; var Done: Boolean);
begin
  Done := CheckPreapreWrite(fPos);
end;

procedure TObjectDataManagerOfCache.TMDBWriteProc(fPos: Int64; const wVal: PObjectDataHandle);
var
  m64: TMS64;
  Hnd: TIOHnd;
begin
  m64 := TMS64(FPrepareWritePool[fPos]);
  if m64 = nil then
    begin
      m64 := TMS64.CustomCreate(Get_DB_L(FDB_HND.IOHnd));
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  Hnd.FixedStringL := FDB_HND.FixedStringL;
  umlFileOpenAsStream('', m64, Hnd, False);

  FDB_HND.IOHnd.Data := nil;
  db_WriteRec(0, Hnd, wVal^);
  FDB_HND.IOHnd.Data := @FDB_HND;

  umlFileClose(Hnd);
  if m64.Position <> Get_DB_L(FDB_HND.IOHnd) then
      RaiseInfo('write error!');
  m64.Position := 0;
end;

procedure TObjectDataManagerOfCache.TMDBReadProc(fPos: Int64; const rVal: PObjectDataHandle; var Done: Boolean);
var
  m64: TMS64;
  Hnd: TIOHnd;
begin
  m64 := TMS64(FPrepareWritePool[fPos]);
  if m64 <> nil then
    begin
      InitIOHnd(Hnd);
      Hnd.FixedStringL := FDB_HND.FixedStringL;
      umlFileOpenAsStream('', m64, Hnd, False);
      FDB_HND.IOHnd.Data := nil;
      Done := db_ReadRec(0, Hnd, rVal^);
      FDB_HND.IOHnd.Data := @FDB_HND;
      umlFileClose(Hnd);
      m64.Position := 0;
    end;
end;

procedure TObjectDataManagerOfCache.DoOpenBefore;
begin
  inherited DoOpenBefore;

  FHeaderCache := TInt64HashPointerList.CustomCreate(if_(FDB_HND.IOHnd.Handle is TReliableFileStream, 10 * 10000, 1 * 10000));
  FHeaderCache.AutoFreeData := True;
  FHeaderCache.AccessOptimization := True;

  FItemBlockCache := TInt64HashPointerList.CustomCreate(if_(FDB_HND.IOHnd.Handle is TReliableFileStream, 10 * 10000, 1 * 10000));
  FItemBlockCache.AutoFreeData := True;
  FItemBlockCache.AccessOptimization := True;

  FItemCache := TInt64HashPointerList.CustomCreate(if_(FDB_HND.IOHnd.Handle is TReliableFileStream, 10 * 10000, 1 * 10000));
  FItemCache.AutoFreeData := True;
  FItemCache.AccessOptimization := True;

  FFieldCache := TInt64HashPointerList.CustomCreate(if_(FDB_HND.IOHnd.Handle is TReliableFileStream, 10 * 10000, 1 * 10000));
  FFieldCache.AutoFreeData := True;
  FFieldCache.AccessOptimization := True;

  FPrepareWritePool := TInt64HashObjectList.CustomCreate(if_(FDB_HND.IOHnd.Handle is TReliableFileStream, 4 * 10 * 10000, 4 * 1 * 10000));
  FPrepareWritePool.AutoFreeData := True;
  FPrepareWritePool.AccessOptimization := True;

  FHeaderCache.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}HeaderCache_DataFreeProc;
  FItemBlockCache.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}ItemBlockCache_DataFreeProc;
  FItemCache.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}ItemCache_DataFreeProc;
  FFieldCache.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}FieldCache_DataFreeProc;
  FPrepareWritePool.OnObjectFreeProc := {$IFDEF FPC}@{$ENDIF FPC}PrepareWritePool_DataFreeProc;

  BuildDBCacheIntf;
end;

destructor TObjectDataManagerOfCache.Destroy;
begin
  FreeDBCacheIntf;
  DisposeObject([FHeaderCache, FItemBlockCache, FItemCache, FFieldCache, FPrepareWritePool]);
  inherited Destroy;
end;

procedure TObjectDataManagerOfCache.BuildDBCacheIntf;
begin
  FDB_HND.OnDeleteHeader := {$IFDEF FPC}@{$ENDIF FPC}DeleteHeaderProc;

  FDB_HND.OnPrepareWriteHeader := {$IFDEF FPC}@{$ENDIF FPC}PrepareHeaderWriteProc;
  FDB_HND.OnWriteHeader := {$IFDEF FPC}@{$ENDIF FPC}HeaderWriteProc;
  FDB_HND.OnReadHeader := {$IFDEF FPC}@{$ENDIF FPC}HeaderReadProc;

  FDB_HND.OnPrepareWriteItemBlock := {$IFDEF FPC}@{$ENDIF FPC}PrepareItemBlockWriteProc;
  FDB_HND.OnWriteItemBlock := {$IFDEF FPC}@{$ENDIF FPC}ItemBlockWriteProc;
  FDB_HND.OnReadItemBlock := {$IFDEF FPC}@{$ENDIF FPC}ItemBlockReadProc;

  FDB_HND.OnPrepareWriteItem := {$IFDEF FPC}@{$ENDIF FPC}PrepareItemWriteProc;
  FDB_HND.OnWriteItem := {$IFDEF FPC}@{$ENDIF FPC}ItemWriteProc;
  FDB_HND.OnReadItem := {$IFDEF FPC}@{$ENDIF FPC}ItemReadProc;

  FDB_HND.OnPrepareOnlyWriteItemRec := {$IFDEF FPC}@{$ENDIF FPC}PrepareOnlyItemRecWriteProc;
  FDB_HND.OnOnlyWriteItemRec := {$IFDEF FPC}@{$ENDIF FPC}OnlyItemRecWriteProc;
  FDB_HND.OnOnlyReadItemRec := {$IFDEF FPC}@{$ENDIF FPC}OnlyItemRecReadProc;

  FDB_HND.OnPrepareWriteField := {$IFDEF FPC}@{$ENDIF FPC}PrepareFieldWriteProc;
  FDB_HND.OnWriteField := {$IFDEF FPC}@{$ENDIF FPC}FieldWriteProc;
  FDB_HND.OnReadField := {$IFDEF FPC}@{$ENDIF FPC}FieldReadProc;

  FDB_HND.OnPrepareOnlyWriteFieldRec := {$IFDEF FPC}@{$ENDIF FPC}PrepareOnlyFieldRecWriteProc;
  FDB_HND.OnOnlyWriteFieldRec := {$IFDEF FPC}@{$ENDIF FPC}OnlyFieldRecWriteProc;
  FDB_HND.OnOnlyReadFieldRec := {$IFDEF FPC}@{$ENDIF FPC}OnlyFieldRecReadProc;

  FDB_HND.OnPrepareWriteTMDB := {$IFDEF FPC}@{$ENDIF FPC}PrepareTMDBWriteProc;
  FDB_HND.OnWriteTMDB := {$IFDEF FPC}@{$ENDIF FPC}TMDBWriteProc;
  FDB_HND.OnReadTMDB := {$IFDEF FPC}@{$ENDIF FPC}TMDBReadProc;
end;

procedure TObjectDataManagerOfCache.FreeDBCacheIntf;
begin
  CleaupCache;

  FDB_HND.OnDeleteHeader := nil;

  FDB_HND.OnPrepareWriteHeader := nil;
  FDB_HND.OnWriteHeader := nil;
  FDB_HND.OnReadHeader := nil;

  FDB_HND.OnPrepareWriteItemBlock := nil;
  FDB_HND.OnWriteItemBlock := nil;
  FDB_HND.OnReadItemBlock := nil;

  FDB_HND.OnPrepareWriteItem := nil;
  FDB_HND.OnWriteItem := nil;
  FDB_HND.OnReadItem := nil;

  FDB_HND.OnPrepareOnlyWriteItemRec := nil;
  FDB_HND.OnOnlyWriteItemRec := nil;
  FDB_HND.OnOnlyReadItemRec := nil;

  FDB_HND.OnPrepareWriteField := nil;
  FDB_HND.OnWriteField := nil;
  FDB_HND.OnReadField := nil;

  FDB_HND.OnPrepareOnlyWriteFieldRec := nil;
  FDB_HND.OnOnlyWriteFieldRec := nil;
  FDB_HND.OnOnlyReadFieldRec := nil;

  FDB_HND.OnPrepareWriteTMDB := nil;
  FDB_HND.OnWriteTMDB := nil;
  FDB_HND.OnReadTMDB := nil;
end;

procedure TObjectDataManagerOfCache.CleaupCache;
begin
  Flush;
  FHeaderCache.Clear;
  FItemBlockCache.Clear;
  FItemCache.Clear;
  FFieldCache.Clear;
  FPrepareWritePool.Clear;
end;

procedure TObjectDataManagerOfCache.ResetCachePool(const siz_: Integer);
begin
  CleaupCache();
  FHeaderCache.SetHashBlockCount(if_(FDB_HND.IOHnd.Handle is TReliableFileStream, siz_, 1 * 10000));
  FItemBlockCache.SetHashBlockCount(if_(FDB_HND.IOHnd.Handle is TReliableFileStream, siz_, 1 * 10000));
  FItemCache.SetHashBlockCount(if_(FDB_HND.IOHnd.Handle is TReliableFileStream, siz_, 1 * 10000));
  FFieldCache.SetHashBlockCount(if_(FDB_HND.IOHnd.Handle is TReliableFileStream, siz_, 1 * 10000));
  FPrepareWritePool.SetHashBlockCount(if_(FDB_HND.IOHnd.Handle is TReliableFileStream, 4 * siz_, 4 * 1 * 10000));
end;

procedure TObjectDataManagerOfCache.UpdateIO;
begin
  Flush();
end;

procedure TObjectDataManagerOfCache.Flush();
var
  i: NativeInt;
  swapTotal: Integer;
  p: PInt64HashListObjectStruct;
  m64: TMS64;
  swapFileName: TPascalString;
  swapHnd: TCore_FileStream;
  swapHead: TSwapHead;
begin
  // update Database header
  inherited UpdateIO;

  if (not FDB_HND.IOHnd.IsOnlyRead) and (FDB_HND.IOHnd.IsOpen) and (FPrepareWritePool.Count > 0) then
    begin
      // step 1: flush to swap file
{$IFDEF ZDB_PHYSICAL_FLUSH}
      if (FDB_HND.IOHnd.Handle is TReliableFileStream) then
        begin
          swapFileName := TReliableFileStream(FDB_HND.IOHnd.Handle).fileName + SFlush;
          swapHnd := nil;
          try
            swapHnd := TCore_FileStream.Create(swapFileName, fmCreate);

            swapTotal := FPrepareWritePool.Count;
            swapHnd.Write(swapTotal, C_Integer_Size);

            i := 0;
            p := FPrepareWritePool.FirstPtr;
            while i < FPrepareWritePool.Count do
              begin
                m64 := TMS64(p^.Data);
                if p^.i64 >= FDB_HND.IOHnd.Size then
                    RaiseInfo('flush: prepare write buffer error!');

                swapHead.Size := m64.Size;
                swapHead.MD5 := umlMD5(m64.Memory, m64.Size);
                swapHead.Position := p^.i64;
                swapHnd.Write(swapHead, SizeOf(swapHead));
                swapHnd.Write(m64.Memory^, m64.Size);
                inc(i);
                p := p^.Next;
              end;
          except
          end;
          DisposeObject(swapHnd);
        end;
{$ENDIF ZDB_PHYSICAL_FLUSH}
      // step 2: flash fragment
      i := 0;
      p := FPrepareWritePool.FirstPtr;
      while i < FPrepareWritePool.Count do
        begin
          m64 := TMS64(p^.Data);
          FDB_HND.IOHnd.Handle.Position := p^.i64;
          FDB_HND.IOHnd.Handle.Write(m64.Memory^, m64.Size);
          inc(i);
          p := p^.Next;
        end;

{$IFDEF ZDB_PHYSICAL_FLUSH}
      // step 3: delete swap file
      if (FDB_HND.IOHnd.Handle is TReliableFileStream) then
          umlDeleteFile(swapFileName);
{$ENDIF ZDB_PHYSICAL_FLUSH}
    end;
  FPrepareWritePool.Clear;
end;

function TObjectDataManagerOfCache.CacheStatus: SystemString;
begin
  Result := PFormat('header %d block %d item %d field %d prepare %d', [FHeaderCache.Count, FItemBlockCache.Count, FItemCache.Count, FFieldCache.Count, FPrepareWritePool.Count]);
end;

function TObjectDataMarshal.GetItems(aIndex: Integer): TObjectDataManager;
begin
  Result := TObjectDataManager(FLibList.Objects[aIndex]);
end;

function TObjectDataMarshal.GetNames(Name_: SystemString): TObjectDataManager;
var
  i: Integer;
  aUName: SystemString;
begin
  Result := nil;
  aUName := GetAbsoluteFileName(Name_);
  if FLibList.Count > 0 then
    begin
      if FUseWildcard then
        begin
          if not umlMatchChar('\', aUName) then
              aUName := '*\' + aUName;
          for i := 0 to FLibList.Count - 1 do
            begin
              if umlMultipleMatch(False, aUName, FLibList[i]) then
                begin
                  Result := TObjectDataManager(FLibList.Objects[i]);
                  Exit;
                end;
            end;
        end
      else
        begin
          if umlMatchChar('\', aUName) then
            begin
              for i := 0 to FLibList.Count - 1 do
                if umlSameText(aUName, FLibList[i]) then
                  begin
                    Result := TObjectDataManager(FLibList.Objects[i]);
                    Exit;
                  end;
            end
          else
            begin
              for i := 0 to FLibList.Count - 1 do
                if umlSameText(aUName, umlGetLastStr(FLibList[i], '\')) then
                  begin
                    Result := TObjectDataManager(FLibList.Objects[i]);
                    Exit;
                  end;
            end;
        end;
    end;
end;

procedure TObjectDataMarshal.SetItems(aIndex: Integer; const Value: TObjectDataManager);
begin
  if Value <> nil then
    begin
      FLibList.Objects[aIndex] := Value;
      FLibList[aIndex] := GetAbsoluteFileName(Value.ObjectName);
    end;
end;

constructor TObjectDataMarshal.Create(dbItemID: Byte);
begin
  inherited Create;
  FID := dbItemID;
  FLibList := TCore_StringList.Create;
  FUseWildcard := True;
end;

destructor TObjectDataMarshal.Destroy;
begin
  Clear;
  DisposeObject(FLibList);
  inherited Destroy;
end;

function TObjectDataMarshal.GetAbsoluteFileName(fileName: SystemString): SystemString;
begin
  Result := umlUpperCase(umlCharReplace(umlTrimSpace(fileName), '/', '\')).Text;
end;

function TObjectDataMarshal.NewDB(dbFile: SystemString; dbOnlyRead: Boolean): TObjectDataManager;
var
  i: Integer;
  aUName: SystemString;
begin
  Result := nil;
  aUName := GetAbsoluteFileName(dbFile);
  if FLibList.Count > 0 then
    for i := 0 to FLibList.Count - 1 do
      if umlSameText(aUName, FLibList[i]) then
          Result := TObjectDataManager(FLibList.Objects[i]);
  if Result = nil then
    begin
      Result := TObjectDataManager.CreateNew(dbFile, FID);
      if Result.isAbort then
        begin
          DisposeObject(Result);
          Result := nil;
        end
      else
        begin
          FLibList.AddObject(GetAbsoluteFileName(Result.ObjectName), Result);
        end;
    end;
end;

function TObjectDataMarshal.NewDB(FixedStringL: Byte; dbFile: SystemString; dbOnlyRead: Boolean): TObjectDataManager;
var
  i: Integer;
  aUName: SystemString;
begin
  Result := nil;
  aUName := GetAbsoluteFileName(dbFile);
  if FLibList.Count > 0 then
    for i := 0 to FLibList.Count - 1 do
      if umlSameText(aUName, FLibList[i]) then
          Result := TObjectDataManager(FLibList.Objects[i]);
  if Result = nil then
    begin
      Result := TObjectDataManager.CreateNew(FixedStringL, dbFile, FID);
      if Result.isAbort then
        begin
          DisposeObject(Result);
          Result := nil;
        end
      else
        begin
          FLibList.AddObject(GetAbsoluteFileName(Result.ObjectName), Result);
        end;
    end;
end;

function TObjectDataMarshal.Open(dbFile: SystemString; dbOnlyRead: Boolean): TObjectDataManager;
var
  i: Integer;
  aUName: SystemString;
begin
  Result := nil;
  aUName := GetAbsoluteFileName(dbFile);
  if FLibList.Count > 0 then
    for i := 0 to FLibList.Count - 1 do
      if umlSameText(aUName, FLibList[i]) then
          Result := TObjectDataManager(FLibList.Objects[i]);
  if Result = nil then
    begin
      Result := TObjectDataManager.Open(dbFile, FID, dbOnlyRead);
      if Result.isAbort then
        begin
          DisposeObject(Result);
          Result := nil;
        end
      else
        begin
          FLibList.AddObject(GetAbsoluteFileName(Result.ObjectName), Result);
        end;
    end;
end;

procedure TObjectDataMarshal.CloseDB(Database: TObjectDataManager);
var
  i: Integer;
begin
  i := 0;
  while i < Count do
    if Items[i] = Database then
        Delete(i)
    else
        inc(i);
end;

procedure TObjectDataMarshal.Clear;
begin
  while Count > 0 do
      Delete(0);
end;

function TObjectDataMarshal.Count: Integer;
begin
  Result := FLibList.Count;
end;

procedure TObjectDataMarshal.Delete(aIndex: Integer);
begin
  try
      DisposeObject(FLibList.Objects[aIndex]);
  except
  end;
  FLibList.Delete(aIndex);
end;

procedure TObjectDataMarshal.DeleteFromName(Name_: SystemString);
var
  i: Integer;
  aUName: SystemString;
begin
  aUName := GetAbsoluteFileName(Name_);
  if FLibList.Count > 0 then
    begin
      if FUseWildcard then
        begin
          if not umlMatchChar('\', aUName) then
              aUName := '*\' + aUName;
          i := 0;
          while i < FLibList.Count do
            begin
              if umlMultipleMatch(False, aUName, FLibList[i]) then
                begin
                  DisposeObject(FLibList.Objects[i]);
                  FLibList.Delete(i);
                end
              else
                  inc(i);
            end;
        end
      else
        begin
          if umlMatchChar('\', aUName) then
            begin
              i := 0;
              while i < FLibList.Count do
                begin
                  if umlSameText(aUName, FLibList[i]) then
                    begin
                      DisposeObject(FLibList.Objects[i]);
                      FLibList.Delete(i);
                    end
                  else
                      inc(i);
                end;
            end
          else
            begin
              i := 0;
              while i < FLibList.Count do
                begin
                  if umlSameText(aUName, umlGetLastStr(FLibList[i], '\')) then
                    begin
                      DisposeObject(FLibList.Objects[i]);
                      FLibList.Delete(i);
                    end
                  else
                      inc(i);
                end;
            end;
        end;
    end;
end;

procedure TObjectDataMarshal.UpdateAll;
var
  i: Integer;
begin
  if Count > 0 then
    for i := 0 to Count - 1 do
        Items[i].UpdateIO;
end;

procedure TObjectDataMarshal.Disable;
var
  i: Integer;
begin
  if Count > 0 then
    for i := 0 to Count - 1 do
        Items[i].Close;
end;

procedure TObjectDataMarshal.Enabled;
var
  i: Integer;
begin
  if Count > 0 then
    for i := 0 to Count - 1 do
        Items[i].DoOpen;
end;

procedure TestObjectData_(c: TObjectDataManagerClass);
var
  Database: TObjectDataManager;
  itmHnd1, itmHnd2, itmHnd3, itmHnd4: TItemHandle;
  buff: TBytes;
  nameL: U_StringArray;
  n: U_String;
begin
  Database := c.CreateAsStream($FF, TMS64.CustomCreate($FFFF), '', 0, False, True, True);
  Database.HandlePtr^.IOHnd.Cache.UsedWriteCache := True;
  Database.HandlePtr^.IOHnd.Cache.UsedReadCache := True;

  if Database.CreateRootField('_RootField') then
      DoStatus('CreateRootField ok')
  else
      DoStatus('CreateRootField error');
  if Database.SetRootField('_RootField') then
      DoStatus('SetRootField ok')
  else
      DoStatus('SetRootField error');

  if Database is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(Database).CacheStatus);

  DoStatus('Database size: %d', [Database.Size]);

  DoStatus('Database field test.');
  Database.CreateField('/a/b/c', '');

  if Database is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(Database).CacheStatus);

  if Database.GetPathFieldHeaderCount('/a') <> 1 then
      DoStatus('create field error');
  if Database.GetPathFieldHeaderCount('/a/b') <> 1 then
      DoStatus('create field error');
  if Database.GetPathFieldHeaderCount('/a/b/c') <> 0 then
      DoStatus('create field error');

  if Database is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(Database).CacheStatus);

  DoStatus('Database item body test');
  if not Database.ItemCreate('/a/b/c', '1', '', itmHnd1) then
      DoStatus('create item error');
  buff := umlBytesOf('1111');
  Database.ItemWrite(itmHnd1, length(buff), buff[0]);
  DoStatus('item1 size:%d', [itmHnd1.Item.Size]);

  if Database is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(Database).CacheStatus);

  if not Database.ItemCreate('/a/b/c', '2', '', itmHnd2) then
      DoStatus('create item error');
  buff := umlBytesOf('22222');
  Database.ItemWrite(itmHnd2, length(buff), buff[0]);
  DoStatus('item2 size:%d', [itmHnd2.Item.Size]);

  if Database is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(Database).CacheStatus);

  if not Database.ItemCreate('/a/b/c', '3', '', itmHnd3) then
      DoStatus('create item error');
  buff := umlBytesOf('3333');
  Database.ItemWrite(itmHnd3, length(buff), buff[0]);
  DoStatus('item3 size:%d', [itmHnd3.Item.Size]);

  if Database is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(Database).CacheStatus);

  if not Database.ItemCreate('/a/b/c', '4', '', itmHnd4) then
      DoStatus('create item error');
  buff := umlBytesOf('44444444');
  Database.ItemWrite(itmHnd4, length(buff), buff[0]);
  DoStatus('item4 size:%d', [itmHnd4.Item.Size]);

  if Database is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(Database).CacheStatus);

  buff := umlBytesOf('t12345');
  Database.ItemWrite(itmHnd1, length(buff), buff[0]);
  Database.ItemSeekStart(itmHnd1);
  SetLength(buff, itmHnd1.Item.Size);
  Database.ItemRead(itmHnd1, length(buff), buff[0]);
  if umlStringOf(buff).Same('1111t12345') then
      DoStatus('test fragment buffer ok!');
  DoStatus('item1 size:%d', [itmHnd1.Item.Size]);

  Database.ItemClose(itmHnd1);
  Database.ItemClose(itmHnd2);
  Database.ItemClose(itmHnd3);
  Database.ItemClose(itmHnd4);

  if Database is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(Database).CacheStatus);

  if not Database.GetPathFieldHeaderNames('/a/b/c', nameL) then
      DoStatus('get field list error');

  if Database is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(Database).CacheStatus);

  if Database.GetItemSize('/a/b/c', nameL[0]) <> 10 then
      DoStatus('item body error');
  if Database.GetItemSize('/a/b/c', nameL[1]) <> 5 then
      DoStatus('item body error');
  if Database.GetItemSize('/a/b/c', nameL[2]) <> 4 then
      DoStatus('item body error');
  if Database.GetItemSize('/a/b/c', nameL[3]) <> 8 then
      DoStatus('item body error');

  if Database is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(Database).CacheStatus);

  DoStatus('Database item delete test');

  Database.ItemDelete('/a/b/c', '3');
  if not Database.GetPathFieldHeaderNames('/a/b/c', nameL) then
      DoStatus('get field list error');

  if Database.GetItemSize('/a/b/c', nameL[0]) <> 10 then
      DoStatus('item body error');
  if Database.GetItemSize('/a/b/c', nameL[1]) <> 5 then
      DoStatus('item body error');
  if Database.GetItemSize('/a/b/c', nameL[2]) <> 8 then
      DoStatus('item body error');

  Database.ItemDelete('/a/b/c', '1');
  if not Database.GetPathFieldHeaderNames('/a/b/c', nameL) then
      DoStatus('get field list error');

  if Database.GetItemSize('/a/b/c', nameL[0]) <> 5 then
      DoStatus('item body error');
  if Database.GetItemSize('/a/b/c', nameL[1]) <> 8 then
      DoStatus('item body error');

  Database.ItemDelete('/a/b/c', '2');
  if not Database.GetPathFieldHeaderNames('/a/b/c', nameL) then
      DoStatus('get field list error');

  if Database.GetItemSize('/a/b/c', nameL[0]) <> 8 then
      DoStatus('item body error');

  Database.ItemDelete('/a/b/c', '4');
  if not Database.GetPathFieldHeaderNames('/a/b/c', nameL) then
      DoStatus('get field list error');

  if length(nameL) = 0 then
      DoStatus('delete test done!');

  if Database is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(Database).CacheStatus);

  DoStatus('Database field delete test');

  if not Database.GetPathFieldHeaderNames('/a/b', nameL) then
      DoStatus('get field list error');
  if (length(nameL) <> 1) then
      DoStatus('get field list error')
  else if nameL[0] = 'c' then
      DoStatus('test field delete ok');

  Database.FieldDelete('/a/b', 'c');
  if not Database.GetPathFieldHeaderNames('/a/b', nameL) then
      DoStatus('get field list error');
  if Database.GetPathFieldHeaderCount('/a/b') <> 0 then
      DoStatus('delete field error');

  Database.FieldDelete('/a', 'b');
  if Database.GetPathFieldHeaderCount('/a/b') <> 0 then
      DoStatus('delete field error');
  Database.FieldDelete('/', 'a');
  if Database.GetPathFieldHeaderCount('/') <> 0 then
      DoStatus('delete field error');

  if length(nameL) = 0 then
      DoStatus('field delete test done!');

  if Database is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(Database).CacheStatus);

  DisposeObject(Database);
end;

procedure TestObjectData();
var
  db: TObjectDataManagerOfCache;
  ns: TPascalStringList;
  itm: TItemStream;
begin
  TestObjectData_(TObjectDataManager);
  TestObjectData_(TObjectDataManagerOfCache);
  db := TObjectDataManagerOfCache.CreateAsStream(TMS64.CustomCreate(1024 * 8), '', 0, False, True, True);
  db.HandlePtr^.IOHnd.Cache.UsedWriteCache := True;
  db.HandlePtr^.IOHnd.Cache.UsedReadCache := True;
  ns := TPascalStringList.Create;
  ns.Add('hello world');
  itm := TItemStream.Create(db, '/', 'hello.txt');
  ns.SaveToStream(itm);
  ns.Free;
  itm.UpdateHandle;
  itm.Free;
  db.Free;
end;

initialization

Internal_ObjectDataMarshal := nil;
ObjectDataMarshal();

finalization

if Internal_ObjectDataMarshal <> nil then
  begin
    DisposeObject(Internal_ObjectDataMarshal);
    Internal_ObjectDataMarshal := nil;
  end;

end.
