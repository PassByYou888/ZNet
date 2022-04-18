{ ****************************************************************************** }
{ * MixedLibrary                                                               * }
{ ****************************************************************************** }
unit Z.UnicodeMixedLib;

{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Dynlibs,
  Z.FPC.GenericList,
{$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
{$ELSE FPC}
{$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
  System.IOUtils,
{$ENDIF FPC}
  SysUtils, Types, Math, Variants, Z.Core,
  Z.PascalStrings, Z.UPascalStrings, Z.ListEngine;

const
  C_Max_UInt32 = $FFFFFFFF;
  C_Address_Size = SizeOf(Pointer);
  C_Pointer_Size = C_Address_Size;
  C_Integer_Size = 4;
  C_Int64_Size = 8;
  C_UInt64_Size = 8;
  C_Single_Size = 4;
  C_Double_Size = 8;
  C_Small_Int_Size = 2;
  C_Byte_Size = 1;
  C_Short_Int_Size = 1;
  C_Word_Size = 2;
  C_DWord_Size = 4;
  C_Cardinal_Size = 4;
  C_Boolean_Size = 1;
  C_Bool_Size = 1;
  C_MD5_Size = 16;

  C_PrepareReadCacheSize = 512;
  C_MaxBufferFragmentSize = $F000;

  C_StringError = -911;
  C_SeekError = -910;
  C_FileWriteError = -909;
  C_FileReadError = -908;
  C_FileHandleError = -907;
  C_OpenFileError = -905;
  C_NotOpenFile = -904;
  C_CreateFileError = -903;
  C_FileIsActive = -902;
  C_NotFindFile = -901;
  C_NotError = -900;

type
  U_SystemString = SystemString;
  U_String = TPascalString;
  U_Char = SystemChar;
  U_StringArray = array of U_SystemString;
  U_ArrayString = U_StringArray;
  U_Bytes = TBytes;
  TSR = TSearchRec;
  U_Stream = TCore_Stream;

  TReliableFileStream = class(TCore_Stream)
  protected
    SourceIO, BackupFileIO: TCore_FileStream;
    FActivted: Boolean;
    FFileName, FBackupFileName: SystemString;

    procedure InitIO;
    procedure FreeIO;

    procedure SetSize(const NewSize: Int64); overload; override;
    procedure SetSize(NewSize: longint); overload; override;
  public
    constructor Create(const FileName_: SystemString; IsNew_, IsWrite_: Boolean);
    destructor Destroy; override;

    function Write(const buffer; Count: longint): longint; override;
    function Read(var buffer; Count: longint): longint; override;
    function Seek(const Offset: Int64; origin: TSeekOrigin): Int64; override;

    property FileName: SystemString read FFileName;
    property BackupFileName: SystemString read FBackupFileName;
    property Activted: Boolean read FActivted;
  end;

  PIOHnd = ^TIOHnd;

  TIOHnd_Cache = record
  private
    PrepareWriteBuff: U_Stream;
    PrepareReadPosition: Int64;
    PrepareReadBuff: U_Stream;
  public
    UsedWriteCache: Boolean;
    UsedReadCache: Boolean;
  end;

  TIOHnd = record
  public
    IsOnlyRead: Boolean;
    IsOpen: Boolean;
    AutoFree: Boolean;
    Handle: U_Stream;
    Time: TDateTime;
    Size: Int64;
    Position: Int64;
    FileName: U_String;
    Cache: TIOHnd_Cache;
    IORead, IOWrite: Int64;
    ChangeFromWrite: Boolean;
    FixedStringL: Byte;
    Data: Pointer;
    Return: Integer;

    function FixedString2Pascal(s: TBytes): TPascalString;
    procedure Pascal2FixedString(var n: TPascalString; var out_: TBytes);
    function CheckFixedStringLoss(s: TPascalString): Boolean;
  end;

  U_ByteArray = array [0 .. MaxInt div SizeOf(Byte) - 1] of Byte;
  P_ByteArray = ^U_ByteArray;

  TListPascalString_Helper_ = class helper for TListPascalString
  public
    procedure FillToArry(var Output_: U_StringArray);
  end;

function umlBytesOf(const s: TPascalString): TBytes;
function umlStringOf(const s: TBytes): TPascalString; overload;

function umlNewString(const s: TPascalString): PPascalString;
procedure umlFreeString(const p: PPascalString);

function umlComparePosStr(const s: TPascalString; Offset: Integer; const t: TPascalString): Boolean;
function umlPos(const SubStr, s: TPascalString; const Offset: Integer = 1): Integer;

function umlVarToStr(const v: Variant; const Base64Conver: Boolean): TPascalString; overload;
function umlVarToStr(const v: Variant): TPascalString; overload;
function umlStrToVar(const s: TPascalString): Variant;

function umlMax(const v1, v2: UInt64): UInt64; overload;
function umlMax(const v1, v2: Cardinal): Cardinal; overload;
function umlMax(const v1, v2: Word): Word; overload;
function umlMax(const v1, v2: Byte): Byte; overload;
function umlMax(const v1, v2: Int64): Int64; overload;
function umlMax(const v1, v2: Integer): Integer; overload;
function umlMax(const v1, v2: SmallInt): SmallInt; overload;
function umlMax(const v1, v2: ShortInt): ShortInt; overload;
function umlMax(const v1, v2: Double): Double; overload;
function umlMax(const v1, v2: Single): Single; overload;

function umlMin(const v1, v2: UInt64): UInt64; overload;
function umlMin(const v1, v2: Cardinal): Cardinal; overload;
function umlMin(const v1, v2: Word): Word; overload;
function umlMin(const v1, v2: Byte): Byte; overload;
function umlMin(const v1, v2: Int64): Int64; overload;
function umlMin(const v1, v2: Integer): Integer; overload;
function umlMin(const v1, v2: SmallInt): SmallInt; overload;
function umlMin(const v1, v2: ShortInt): ShortInt; overload;
function umlMin(const v1, v2: Double): Double; overload;
function umlMin(const v1, v2: Single): Single; overload;

function umlClamp(const v, min_, max_: Integer): Integer; overload;
function umlClamp(const v, min_, max_: UInt64): UInt64; overload;
function umlClamp(const v, min_, max_: Cardinal): Cardinal; overload;
function umlClamp(const v, min_, max_: Word): Word; overload;
function umlClamp(const v, min_, max_: Byte): Byte; overload;
function umlClamp(const v, min_, max_: Int64): Int64; overload;
function umlClamp(const v, min_, max_: SmallInt): SmallInt; overload;
function umlClamp(const v, min_, max_: ShortInt): ShortInt; overload;
function umlClamp(const v, min_, max_: Double): Double; overload;
function umlClamp(const v, min_, max_: Single): Single; overload;

function umlInRange(const v, min_, max_: Integer): Boolean; overload;
function umlInRange(const v, min_, max_: UInt64): Boolean; overload;
function umlInRange(const v, min_, max_: Cardinal): Boolean; overload;
function umlInRange(const v, min_, max_: Word): Boolean; overload;
function umlInRange(const v, min_, max_: Byte): Boolean; overload;
function umlInRange(const v, min_, max_: Int64): Boolean; overload;
function umlInRange(const v, min_, max_: SmallInt): Boolean; overload;
function umlInRange(const v, min_, max_: ShortInt): Boolean; overload;
function umlInRange(const v, min_, max_: Double): Boolean; overload;
function umlInRange(const v, min_, max_: Single): Boolean; overload;

function umlCompareText(s1, s2: TPascalString): Integer;

function umlGetResourceStream(const FileName: TPascalString): TCore_Stream;

function umlSameVarValue(const v1, v2: Variant): Boolean;
function umlSameVariant(const v1, v2: Variant): Boolean;

function umlRandom(const rnd: TMT19937Random): Integer; overload;
function umlRandom: Integer; overload;

function umlRandomRange(const rnd: TMT19937Random; const min_, max_: Integer): Integer; overload;
function umlRandomRange64(const rnd: TMT19937Random; const min_, max_: Int64): Int64; overload;
function umlRandomRangeS(const rnd: TMT19937Random; const min_, max_: Single): Single; overload;
function umlRandomRangeD(const rnd: TMT19937Random; const min_, max_: Double): Double; overload;
function umlRandomRangeF(const rnd: TMT19937Random; const min_, max_: Double): Double; overload;

function umlRandomRange(const min_, max_: Integer): Integer; overload;
function umlRandomRange64(const min_, max_: Int64): Int64; overload;
function umlRandomRangeS(const min_, max_: Single): Single; overload;
function umlRandomRangeD(const min_, max_: Double): Double; overload;
function umlRandomRangeF(const min_, max_: Double): Double; overload;

function umlDefaultTime: Double;
function umlNow: Double;
function umlDefaultAttrib: Integer;
function umlBoolToStr(const Value: Boolean): TPascalString;
function umlStrToBool(const Value: TPascalString): Boolean;

function umlFileExists(const FileName: TPascalString): Boolean;
function umlDirectoryExists(const DirectoryName: TPascalString): Boolean;
function umlCreateDirectory(const DirectoryName: TPascalString): Boolean;
function umlCurrentDirectory: TPascalString;
function umlCurrentPath: TPascalString;
function umlGetCurrentPath: TPascalString;
procedure umlSetCurrentPath(ph: TPascalString);

function umlFindFirstFile(const FileName: TPascalString; var SR: TSR): Boolean;
function umlFindNextFile(var SR: TSR): Boolean;
function umlFindFirstDir(const DirName: TPascalString; var SR: TSR): Boolean;
function umlFindNextDir(var SR: TSR): Boolean;
procedure umlFindClose(var SR: TSR);

function umlGetFileList(const FullPath: TPascalString; AsLst: TCore_Strings): Integer; overload;
function umlGetDirList(const FullPath: TPascalString; AsLst: TCore_Strings): Integer; overload;
function umlGetFileList(const FullPath: TPascalString; AsLst: TPascalStringList): Integer; overload;
function umlGetDirList(const FullPath: TPascalString; AsLst: TPascalStringList): Integer; overload;

function umlGetFileListWithFullPath(const FullPath: TPascalString): U_StringArray;
function umlGetDirListWithFullPath(const FullPath: TPascalString): U_StringArray;
function umlGetFileListPath(const FullPath: TPascalString): U_StringArray;
function umlGetDirListPath(const FullPath: TPascalString): U_StringArray;

function umlCombinePath(const s1, s2: TPascalString): TPascalString;
function umlCombineFileName(const pathName, FileName: TPascalString): TPascalString;
function umlCombineUnixPath(const s1, s2: TPascalString): TPascalString;
function umlCombineUnixFileName(const pathName, FileName: TPascalString): TPascalString;
function umlCombineWinPath(const s1, s2: TPascalString): TPascalString;
function umlCombineWinFileName(const pathName, FileName: TPascalString): TPascalString;
function umlGetFileName(platform_: TExecutePlatform; const s: TPascalString): TPascalString; overload;
function umlGetFileName(const s: TPascalString): TPascalString; overload;
function umlGetWindowsFileName(const s: TPascalString): TPascalString;
function umlGetUnixFileName(const s: TPascalString): TPascalString;
function umlGetFilePath(platform_: TExecutePlatform; const s: TPascalString): TPascalString; overload;
function umlGetFilePath(const s: TPascalString): TPascalString; overload;
function umlGetWindowsFilePath(const s: TPascalString): TPascalString;
function umlGetUnixFilePath(const s: TPascalString): TPascalString;
function umlChangeFileExt(const s, ext: TPascalString): TPascalString;
function umlGetFileExt(const s: TPascalString): TPascalString;

{ FileIO }
procedure InitIOHnd(var IOHnd: TIOHnd);
function umlFileCreateAsStream(const FileName: TPascalString; stream: U_Stream; var IOHnd: TIOHnd; OnlyRead_: Boolean): Boolean; overload;
function umlFileCreateAsStream(const FileName: TPascalString; stream: U_Stream; var IOHnd: TIOHnd): Boolean; overload;
function umlFileCreateAsStream(stream: U_Stream; var IOHnd: TIOHnd): Boolean; overload;
function umlFileCreateAsStream(stream: U_Stream; var IOHnd: TIOHnd; OnlyRead_: Boolean): Boolean; overload;
function umlFileOpenAsStream(const FileName: TPascalString; stream: U_Stream; var IOHnd: TIOHnd; OnlyRead_: Boolean): Boolean;
function umlFileCreateAsMemory(var IOHnd: TIOHnd): Boolean;
function umlFileCreate(const FileName: TPascalString; var IOHnd: TIOHnd): Boolean;
function umlFileOpen(const FileName: TPascalString; var IOHnd: TIOHnd; OnlyRead_: Boolean): Boolean;
function umlFileClose(var IOHnd: TIOHnd): Boolean;
function umlFileUpdate(var IOHnd: TIOHnd): Boolean;
function umlFileTest(var IOHnd: TIOHnd): Boolean;
procedure umlResetPrepareRead(var IOHnd: TIOHnd);
function umlFilePrepareRead(var IOHnd: TIOHnd; Size: Int64; var buff): Boolean;
function umlFileRead(var IOHnd: TIOHnd; const Size: Int64; var buff): Boolean;
function umlBlockRead(var IOHnd: TIOHnd; var buff; const Size: Int64): Boolean;
function umlFilePrepareWrite(var IOHnd: TIOHnd): Boolean;
function umlFileFlushWriteCache(var IOHnd: TIOHnd): Boolean;
function umlFileWrite(var IOHnd: TIOHnd; const Size: Int64; const buff): Boolean;
function umlBlockWrite(var IOHnd: TIOHnd; const buff; const Size: Int64): Boolean;
function umlFileWriteFixedString(var IOHnd: TIOHnd; var Value: TPascalString): Boolean;
function umlFileReadFixedString(var IOHnd: TIOHnd; var Value: TPascalString): Boolean;
function umlFileSeek(var IOHnd: TIOHnd; Pos_: Int64): Boolean;
function umlFileGetPOS(var IOHnd: TIOHnd): Int64;
function umlFileSetSize(var IOHnd: TIOHnd; siz_: Int64): Boolean;
function umlFilePOS(var IOHnd: TIOHnd): Int64;
function umlFileGetSize(var IOHnd: TIOHnd): Int64;
function umlFileSize(var IOHnd: TIOHnd): Int64;
function umlGetFileTime(const FileName: TPascalString): TDateTime;
procedure umlSetFileTime(const FileName: TPascalString; newTime: TDateTime);
function umlGetFileSize(const FileName: TPascalString): Int64;
function umlGetFileCount(const FileName: TPascalString): Integer;
function umlGetFileDateTime(const FileName: TPascalString): TDateTime;
function umlDeleteFile(const FileName: TPascalString; const _VerifyCheck: Boolean): Boolean; overload;
function umlDeleteFile(const FileName: TPascalString): Boolean; overload;
function umlCopyFile(const SourFile, DestFile: TPascalString): Boolean;
function umlRenameFile(const OldName, NewName: TPascalString): Boolean;

procedure umlSetLength(var sVal: TPascalString; L: Integer); overload;
procedure umlSetLength(var sVal: U_Bytes; L: Integer); overload;
procedure umlSetLength(var sVal: TArrayPascalString; L: Integer); overload;
function umlGetLength(const sVal: TPascalString): Integer; overload;
function umlGetLength(const sVal: U_Bytes): Integer; overload;
function umlGetLength(const sVal: TArrayPascalString): Integer; overload;

function umlUpperCase(const s: TPascalString): TPascalString; overload;
function umlUpperCase(const s: PPascalString): TPascalString; overload;
function umlLowerCase(const s: TPascalString): TPascalString; overload;
function umlLowerCase(const s: PPascalString): TPascalString; overload;
function umlCopyStr(const sVal: TPascalString; MainPosition, LastPosition: Integer): TPascalString;
function umlSameText(const s1, s2: TPascalString): Boolean; overload;
function umlSameText(const s1, s2: PPascalString): Boolean; overload;

function umlDeleteChar(const SText, Ch: TPascalString): TPascalString; overload;
function umlDeleteChar(const SText: TPascalString; const SomeChars: TArrayChar): TPascalString; overload;
function umlDeleteChar(const SText: TPascalString; const SomeCharsets: TOrdChars): TPascalString; overload;
function umlGetNumberCharInText(const n: TPascalString): TPascalString;

function umlMatchChar(CharValue: U_Char; cVal: PPascalString): Boolean; overload;
function umlMatchChar(CharValue: U_Char; cVal: TPascalString): Boolean; overload;
function umlExistsChar(StrValue: TPascalString; cVal: TPascalString): Boolean; overload;
function umlExistsChar(StrValue, cVal: PPascalString): Boolean; overload;

function umlTrimChar(const s, trim_s: TPascalString): TPascalString;

function umlGetFirstStr(const sVal, trim_s: TPascalString): TPascalString;
function umlGetLastStr(const sVal, trim_s: TPascalString): TPascalString;
function umlDeleteFirstStr(const sVal, trim_s: TPascalString): TPascalString;
function umlDeleteLastStr(const sVal, trim_s: TPascalString): TPascalString;
function umlGetIndexStrCount(const sVal, trim_s: TPascalString): Integer;
function umlGetIndexStr(const sVal: TPascalString; trim_s: TPascalString; index: Integer): TPascalString;

procedure umlGetSplitArray(const sour: TPascalString; var dest: TArrayPascalString; const splitC: TPascalString); overload;
procedure umlGetSplitArray(const sour: TPascalString; var dest: U_StringArray; const splitC: TPascalString); overload;
function ArrayStringToText(var ary: TArrayPascalString; const splitC: TPascalString): TPascalString;
function umlStringsToSplitText(lst: TCore_Strings; const splitC: TPascalString): TPascalString; overload;
function umlStringsToSplitText(lst: TListPascalString; const splitC: TPascalString): TPascalString; overload;

function umlGetFirstStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
function umlDeleteFirstStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
function umlGetLastStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
function umlDeleteLastStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
function umlGetIndexStrCount_Discontinuity(const sVal, trim_s: TPascalString): Integer;
function umlGetIndexStr_Discontinuity(const sVal: TPascalString; trim_s: TPascalString; index: Integer): TPascalString;

function umlGetFirstTextPos(const s: TPascalString; const TextArry: TArrayPascalString; var OutText: TPascalString): Integer;
function umlDeleteText(const sour: TPascalString; const bToken, eToken: TArrayPascalString; ANeedBegin, ANeedEnd: Boolean): TPascalString;
function umlGetTextContent(const sour: TPascalString; const bToken, eToken: TArrayPascalString): TPascalString;

type
  TTextType = (ntBool, ntInt, ntInt64, ntUInt64, ntWord, ntByte, ntSmallInt, ntShortInt, ntUInt, ntSingle, ntDouble, ntCurrency, ntUnknow);

function umlGetNumTextType(const s: TPascalString): TTextType;

function umlIsHex(const sVal: TPascalString): Boolean;
function umlIsNumber(const sVal: TPascalString): Boolean;
function umlIsIntNumber(const sVal: TPascalString): Boolean;
function umlIsFloatNumber(const sVal: TPascalString): Boolean;
function umlIsBool(const sVal: TPascalString): Boolean;
function umlNumberCount(const sVal: TPascalString): Integer;

function umlPercentageToFloat(OriginMax, OriginMin, ProcressParameter: Double): Double;
function umlPercentageToInt64(OriginParameter, ProcressParameter: Int64): Integer;
function umlPercentageToInt(OriginParameter, ProcressParameter: Integer): Integer;
function umlPercentageToStr(OriginParameter, ProcressParameter: Integer): TPascalString;
function umlSmartSizeToStr(Size: Int64): TPascalString;

function umlIntToStr(Parameter: Single): TPascalString; overload;
function umlIntToStr(Parameter: Double): TPascalString; overload;
function umlIntToStr(Parameter: Int64): TPascalString; overload;
function umlIntToStr(Parameter: UInt64): TPascalString; overload;

function umlPointerToStr(param: Pointer): TPascalString;

function umlMBPSToStr(Size: Int64): TPascalString;
function umlSizeToStr(Parameter: Int64): TPascalString;
function umlStrToDateTime(s: TPascalString): TDateTime;
function umlDateTimeToStr(t: TDateTime): TPascalString;
function umlTimeTickToStr(const t: TTimeTick): TPascalString;
function umlTimeToStr(t: TDateTime): TPascalString;
function umlDateToStr(t: TDateTime): TPascalString;
function umlFloatToStr(const f: Double): TPascalString;
function umlShortFloatToStr(const f: Double): TPascalString;

function umlStrToInt(const V_: TPascalString): Integer; overload;
function umlStrToInt(const V_: TPascalString; _Def: Integer): Integer; overload;
function umlStrToInt64(const V_: TPascalString; _Def: Int64): Int64; overload;
function umlStrToInt64(const V_: TPascalString): Int64; overload;
function umlStrToFloat(const V_: TPascalString; _Def: Double): Double; overload;
function umlStrToFloat(const V_: TPascalString): Double; overload;

function umlMultipleMatch(IgnoreCase: Boolean; const source, target, Multiple_, Character_: TPascalString): Boolean; overload;
function umlMultipleMatch(IgnoreCase: Boolean; const source, target: TPascalString): Boolean; overload;
function umlMultipleMatch(const source, target: TPascalString): Boolean; overload;
function umlMultipleMatch(const source: array of TPascalString; const target: TPascalString): Boolean; overload;
function umlSearchMatch(const source, target: TPascalString): Boolean; overload;
function umlSearchMatch(const source: TArrayPascalString; target: TPascalString): Boolean; overload;

// <prefix>.<postfix> formula, match sour -> dest
// example: <prefix>.*
// example: *.<postfix>
function umlMatchFileInfo(const exp_, sour_, dest_: TPascalString): Boolean;

function umlGetDateTimeStr(NowDateTime: TDateTime): TPascalString;
function umlDecodeTimeToStr(NowDateTime: TDateTime): TPascalString;
function umlMakeRanName: TPascalString;

type
  TBatch = record
    sour, dest: TPascalString;
    sum: Integer;
  end;

  PBatch = ^TBatch;

  TArrayBatch = array of TBatch;

  TBatchInfo = record
    Batch: Integer;
    sour_bPos, sour_ePos: Integer;
    dest_bPos, dest_ePos: Integer;
  end;

  TBatchInfoList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TBatchInfo>;

{$IFDEF FPC}
  TOnBatchProc = procedure(bPos, ePos: Integer; sour, dest: PPascalString; var Accept: Boolean) is nested;
{$ELSE FPC}
  TOnBatchProc = reference to procedure(bPos, ePos: Integer; sour, dest: PPascalString; var Accept: Boolean);
{$ENDIF FPC}

function umlBuildBatch(L: THashStringList): TArrayBatch; overload;
function umlBuildBatch(L: THashVariantList): TArrayBatch; overload;
procedure umlClearBatch(var arry: TArrayBatch);
procedure umlSortBatch(var arry: TArrayBatch);
function umlCharIsSymbol(c: SystemChar): Boolean; overload;
function umlCharIsSymbol(c: SystemChar; const CustomSymbol_: TArrayChar): Boolean; overload;
function umlIsWord(p: PPascalString; bPos, ePos: Integer): Boolean; overload;
function umlIsWord(s: TPascalString; bPos, ePos: Integer): Boolean; overload;
function umlExtractWord(s: TPascalString): TArrayPascalString; overload;
function umlExtractWord(s: TPascalString; const CustomSymbol_: TArrayChar): TArrayPascalString; overload;
function umlBatchSum(p: PPascalString; var arry: TArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList): Integer; overload;
function umlBatchSum(s: TPascalString; var arry: TArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList): Integer; overload;
function umlBatchSum(s: TPascalString; var arry: TArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): Integer; overload;
function umlBatchReplace(p: PPascalString; var arry: TArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList; On_P: TOnBatchProc): TPascalString; overload;
function umlBatchReplace(s: TPascalString; var arry: TArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList; On_P: TOnBatchProc): TPascalString; overload;
function umlBatchReplace(s: TPascalString; var arry: TArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList): TPascalString; overload;
function umlBatchReplace(s: TPascalString; var arry: TArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): TPascalString; overload;
function umlReplaceSum(p: PPascalString; Pattern: TPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList): Integer; overload;
function umlReplaceSum(s, Pattern: TPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList): Integer; overload;
function umlReplace(p: PPascalString; OldPattern, NewPattern: TPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList; On_P: TOnBatchProc): TPascalString; overload;
function umlReplace(s, OldPattern, NewPattern: TPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList; On_P: TOnBatchProc): TPascalString; overload;
function umlReplace(s, OldPattern, NewPattern: TPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList): TPascalString; overload;
function umlReplace(p: PPascalString; OldPattern, NewPattern: TPascalString; OnlyWord, IgnoreCase: Boolean): TPascalString; overload;
function umlReplace(s, OldPattern, NewPattern: TPascalString; OnlyWord, IgnoreCase: Boolean): TPascalString; overload;
function umlComputeTextPoint(p: PPascalString; Pos_: Integer): TPoint;

function umlStringReplace(const s, OldPattern, NewPattern: TPascalString; IgnoreCase: Boolean): TPascalString;
function umlReplaceString(const s, OldPattern, NewPattern: TPascalString; IgnoreCase: Boolean): TPascalString;
function umlCharReplace(const s: TPascalString; OldPattern, NewPattern: U_Char): TPascalString;
function umlReplaceChar(const s: TPascalString; OldPattern, NewPattern: U_Char): TPascalString;

function umlEncodeText2HTML(const psSrc: TPascalString): TPascalString;

function umlURLEncode(const Data: TPascalString): TPascalString;
function umlURLDecode(const Data: TPascalString; FormEncoded: Boolean): TPascalString;

type
  TBase64Context = record
    Tail: array [0 .. 3] of Byte;
    TailBytes: Integer;
    LineWritten: Integer;
    LineSize: Integer;
    TrailingEol: Boolean;
    PutFirstEol: Boolean;
    LiberalMode: Boolean;
    fEOL: array [0 .. 3] of Byte;
    EOLSize: Integer;
    OutBuf: array [0 .. 3] of Byte;
    EQUCount: Integer;
    UseUrlAlphabet: Boolean;
  end;

  TBase64EOLMarker = (emCRLF, emCR, emLF, emNone);
  TBase64ByteArray = array [0 .. MaxInt div SizeOf(Byte) - 1] of Byte;
  PBase64ByteArray = ^TBase64ByteArray;

const
  BASE64_DECODE_OK = 0;
  BASE64_DECODE_INVALID_CHARACTER = 1;
  BASE64_DECODE_WRONG_DATA_SIZE = 2;
  BASE64_DECODE_NOT_ENOUGH_SPACE = 3;

  Base64Symbols: array [0 .. 63] of Byte = (
    $41, $42, $43, $44, $45, $46, $47, $48, $49, $4A, $4B, $4C, $4D, $4E, $4F, $50,
    $51, $52, $53, $54, $55, $56, $57, $58, $59, $5A, $61, $62, $63, $64, $65, $66,
    $67, $68, $69, $6A, $6B, $6C, $6D, $6E, $6F, $70, $71, $72, $73, $74, $75, $76,
    $77, $78, $79, $7A, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $2B, $2F);

  Base64Values: array [0 .. 255] of Byte = (
    $FE, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FE, $FE, $FF, $FF, $FE, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FE, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $3E, $FF, $FF, $FF, $3F,
    $34, $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $FF, $FF, $FF, $FD, $FF, $FF,
    $FF, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E,
    $0F, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $FF, $FF, $FF, $FF, $FF,
    $FF, $1A, $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23, $24, $25, $26, $27, $28,
    $29, $2A, $2B, $2C, $2D, $2E, $2F, $30, $31, $32, $33, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);

function B64EstimateEncodedSize(cont: TBase64Context; InSize: Integer): Integer;
function B64InitializeDecoding(var cont: TBase64Context; LiberalMode: Boolean): Boolean;
function B64InitializeEncoding(var cont: TBase64Context; LineSize: Integer; fEOL: TBase64EOLMarker; TrailingEol: Boolean): Boolean;
function B64Encode(var cont: TBase64Context; buffer: PByte; Size: Integer; OutBuffer: PByte; var OutSize: Integer): Boolean;
function B64Decode(var cont: TBase64Context; buffer: PByte; Size: Integer; OutBuffer: PByte; var OutSize: Integer): Boolean;
function B64FinalizeEncoding(var cont: TBase64Context; OutBuffer: PByte; var OutSize: Integer): Boolean;
function B64FinalizeDecoding(var cont: TBase64Context; OutBuffer: PByte; var OutSize: Integer): Boolean;
function umlBase64Encode(InBuffer: PByte; InSize: Integer; OutBuffer: PByte; var OutSize: Integer; WrapLines: Boolean): Boolean;
function umlBase64Decode(InBuffer: PByte; InSize: Integer; OutBuffer: PByte; var OutSize: Integer; LiberalMode: Boolean): Integer;
procedure umlBase64EncodeBytes(var sour, dest: TBytes); overload;
procedure umlBase64DecodeBytes(var sour, dest: TBytes); overload;
procedure umlBase64EncodeBytes(var sour: TBytes; var dest: TPascalString); overload;
procedure umlBase64DecodeBytes(const sour: TPascalString; var dest: TBytes); overload;
procedure umlDecodeLineBASE64(const buffer: TPascalString; var output: TPascalString); overload;
procedure umlEncodeLineBASE64(const buffer: TPascalString; var output: TPascalString); overload;
function umlDecodeLineBASE64(const buffer: TPascalString): TPascalString; overload;
function umlEncodeLineBASE64(const buffer: TPascalString): TPascalString; overload;
procedure umlDecodeStreamBASE64(const buffer: TPascalString; output: TCore_Stream);
procedure umlEncodeStreamBASE64(buffer: TCore_Stream; var output: TPascalString);
function umlDivisionBase64Text(const buffer: TPascalString; width: Integer; DivisionAsPascalString: Boolean): TPascalString;
function umlTestBase64(const text: TPascalString): Boolean;

type
  PMD5 = ^TMD5;
  TMD5 = array [0 .. 15] of Byte;
  TMD5_Pool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TMD5>;
  TArrayMD5 = array of TMD5;
  TMD5_Pair_Pool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TBig_Hash_Pair_Pool<TMD5, TMD5>;

  TMD5_Pair_Pool = class(TMD5_Pair_Pool_Decl)
  public
    IsChanged: Boolean;
    constructor Create(HashSize_: Integer);
    procedure DoFree(var Key: TMD5; var Value: TMD5); override;
    procedure DoAdd(var Key: TMD5; var Value: TMD5); override;
    procedure LoadFromStream(stream: TCore_Stream);
    procedure SaveToStream(stream: TCore_Stream);
  end;

const
  NullMD5: TMD5 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  ZeroMD5: TMD5 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  umlNullMD5: TMD5 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  umlZeroMD5: TMD5 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  Null_Buff_MD5: TMD5 = (212, 29, 140, 217, 143, 0, 178, 4, 233, 128, 9, 152, 236, 248, 66, 126);

function umlStrToMD5(hex: TPascalString): TMD5;
procedure umlTransformMD5(var Accu; const Buf);
function umlMD5(const buffPtr: PByte; bufSiz: NativeUInt): TMD5;
function umlMD5Char(const buffPtr: PByte; const BuffSize: NativeUInt): TPascalString;
function umlMD5String(const buffPtr: PByte; const BuffSize: NativeUInt): TPascalString;
function umlMD5Str(const buffPtr: PByte; const BuffSize: NativeUInt): TPascalString;
function umlStreamMD5(stream: TCore_Stream; StartPos, EndPos: Int64): TMD5; overload;
function umlStreamMD5(stream: TCore_Stream): TMD5; overload;
function umlStreamMD5Char(stream: TCore_Stream): TPascalString; overload;
function umlStreamMD5String(stream: TCore_Stream): TPascalString; overload;
function umlStreamMD5Str(stream: TCore_Stream): TPascalString; overload;
function umlStringMD5(const Value: TPascalString): TPascalString;
function umlFileMD5___(FileName: TPascalString): TMD5; overload;
function umlFileMD5(FileName: TPascalString; StartPos, EndPos: Int64): TMD5; overload;
function umlCombineMD5(const m1: TMD5): TMD5; overload;
function umlCombineMD5(const m1, m2: TMD5): TMD5; overload;
function umlCombineMD5(const m1, m2, m3: TMD5): TMD5; overload;
function umlCombineMD5(const m1, m2, m3, m4: TMD5): TMD5; overload;
function umlCombineMD5(const buff: array of TMD5): TMD5; overload;
function umlMD5ToStr(md5: TMD5): TPascalString; overload;
function umlMD5ToStr(const buffPtr: PByte; bufSiz: NativeUInt): TPascalString; overload;
function umlMD5ToString(md5: TMD5): TPascalString; overload;
function umlMD5ToString(const buffPtr: PByte; bufSiz: NativeUInt): TPascalString; overload;
function umlMD52String(md5: TMD5): TPascalString; overload;
function umlMD5Compare(const m1, m2: TMD5): Boolean;
function umlCompareMD5(const m1, m2: TMD5): Boolean;
function umlIsNullMD5(m: TMD5): Boolean;
function umlWasNullMD5(m: TMD5): Boolean;

{$REGION 'crc16define'}


const
  CRC16Table: array [0 .. 255] of Word = (
    $0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241, $C601, $06C0, $0780,
    $C741, $0500, $C5C1, $C481, $0440, $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1,
    $CE81, $0E40, $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841, $D801,
    $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40, $1E00, $DEC1, $DF81, $1F40,
    $DD01, $1DC0, $1C80, $DC41, $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680,
    $D641, $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040, $F001, $30C0,
    $3180, $F141, $3300, $F3C1, $F281, $3240, $3600, $F6C1, $F781, $3740, $F501,
    $35C0, $3480, $F441, $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41,
    $FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840, $2800, $E8C1, $E981,
    $2940, $EB01, $2BC0, $2A80, $EA41, $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1,
    $EC81, $2C40, $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640, $2200,
    $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041, $A001, $60C0, $6180, $A141,
    $6300, $A3C1, $A281, $6240, $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480,
    $A441, $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41, $AA01, $6AC0,
    $6B80, $AB41, $6900, $A9C1, $A881, $6840, $7800, $B8C1, $B981, $7940, $BB01,
    $7BC0, $7A80, $BA41, $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
    $B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640, $7200, $B2C1, $B381,
    $7340, $B101, $71C0, $7080, $B041, $5000, $90C1, $9181, $5140, $9301, $53C0,
    $5280, $9241, $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440, $9C01,
    $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40, $5A00, $9AC1, $9B81, $5B40,
    $9901, $59C0, $5880, $9841, $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81,
    $4A40, $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41, $4400, $84C1,
    $8581, $4540, $8701, $47C0, $4680, $8641, $8201, $42C0, $4380, $8341, $4100,
    $81C1, $8081, $4040
    );
{$ENDREGION 'crc16define'}

function umlCRC16(const Value: PByte; const Count: NativeUInt): Word;
function umlStringCRC16(const Value: TPascalString): Word;
function umlStreamCRC16(stream: U_Stream; StartPos, EndPos: Int64): Word; overload;
function umlStreamCRC16(stream: U_Stream): Word; overload;

// crc32
function umlCRC32(const Value: PByte; const Count: NativeUInt): Cardinal;
function umlString2CRC32(const Value: TPascalString): Cardinal;
function umlStreamCRC32(stream: U_Stream; StartPos, EndPos: Int64): Cardinal; overload;
function umlStreamCRC32(stream: U_Stream): Cardinal; overload;

function umlTrimSpace(const s: TPascalString): TPascalString;

function umlSeparatorText(Text_: TPascalString; dest: TCore_Strings; SeparatorChar: TPascalString): Integer; overload;
function umlSeparatorText(Text_: TPascalString; dest: THashVariantList; SeparatorChar: TPascalString): Integer; overload;
function umlSeparatorText(Text_: TPascalString; dest: TListPascalString; SeparatorChar: TPascalString): Integer; overload;

function umlStringsMatchText(OriginValue: TCore_Strings; DestValue: TPascalString; IgnoreCase: Boolean): Boolean;

function umlStringsInExists(dest: TListPascalString; SText: TPascalString; IgnoreCase: Boolean): Boolean; overload;
function umlStringsInExists(dest: TCore_Strings; SText: TPascalString; IgnoreCase: Boolean): Boolean; overload;
function umlStringsInExists(dest: TCore_Strings; SText: TPascalString): Boolean; overload;

function umlTextInStrings(const SText: TPascalString; dest: TListPascalString; IgnoreCase: Boolean): Boolean; overload;
function umlTextInStrings(const SText: TPascalString; dest: TCore_Strings; IgnoreCase: Boolean): Boolean; overload;
function umlTextInStrings(const SText: TPascalString; dest: TCore_Strings): Boolean; overload;

function umlAddNewStrTo(source: TPascalString; dest: TListPascalString; IgnoreCase: Boolean): Boolean; overload;
function umlAddNewStrTo(source: TPascalString; dest: TCore_Strings; IgnoreCase: Boolean): Boolean; overload;
function umlAddNewStrTo(source: TPascalString; dest: TCore_Strings): Boolean; overload;
function umlAddNewStrTo(source, dest: TCore_Strings): Integer; overload;
function umlDeleteStrings(const SText: TPascalString; dest: TCore_Strings; IgnoreCase: Boolean): Integer;
function umlDeleteStringsNot(const SText: TPascalString; dest: TCore_Strings; IgnoreCase: Boolean): Integer;
function umlMergeStrings(source, dest: TCore_Strings; IgnoreCase: Boolean): Integer; overload;
function umlMergeStrings(source, dest: TListPascalString; IgnoreCase: Boolean): Integer; overload;

function umlConverStrToFileName(const Value: TPascalString): TPascalString;

function umlSplitTextMatch(const SText, Limit, MatchText: TPascalString; IgnoreCase: Boolean): Boolean;
function umlSplitTextTrimSpaceMatch(const SText, Limit, MatchText: TPascalString; IgnoreCase: Boolean): Boolean;
function umlSplitDeleteText(const SText, Limit, MatchText: TPascalString; IgnoreCase: Boolean): TPascalString;
function umlSplitTextAsList(const SText, Limit: TPascalString; AsLst: TCore_Strings): Boolean;
function umlSplitTextAsListAndTrimSpace(const SText, Limit: TPascalString; AsLst: TCore_Strings): Boolean;
function umlListAsSplitText(const List: TCore_Strings; Limit: TPascalString): TPascalString; overload;
function umlListAsSplitText(const List: TListPascalString; Limit: TPascalString): TPascalString; overload;
function umlDivisionText(const buffer: TPascalString; width: Integer; DivisionAsPascalString: Boolean): TPascalString;

function umlUpdateComponentName(const Name: TPascalString): TPascalString;
function umlMakeComponentName(Owner: TCore_Component; RefrenceName: TPascalString): TPascalString;
procedure umlReadComponent(stream: TCore_Stream; comp: TCore_Component);
procedure umlWriteComponent(stream: TCore_Stream; comp: TCore_Component);
procedure umlCopyComponentDataTo(comp, copyto: TCore_Component);

function umlProcessCycleValue(CurrentVal, DeltaVal, StartVal, OverVal: Single; var EndFlag: Boolean): Single;

{ csv }
type
  TCSVGetLine_C = procedure(var L: TPascalString; var IsEnd: Boolean);
  TCSVSave_C = procedure(const sour: TPascalString; const king, Data: TArrayPascalString);
  TCSVGetLine_M = procedure(var L: TPascalString; var IsEnd: Boolean) of object;
  TCSVSave_M = procedure(const sour: TPascalString; const king, Data: TArrayPascalString) of object;
{$IFDEF FPC}
  TCSVGetLine_P = procedure(var L: TPascalString; var IsEnd: Boolean) is nested;
  TCSVSave_P = procedure(const sour: TPascalString; const king, Data: TArrayPascalString) is nested;
{$ELSE FPC}
  TCSVGetLine_P = reference to procedure(var L: TPascalString; var IsEnd: Boolean);
  TCSVSave_P = reference to procedure(const sour: TPascalString; const king, Data: TArrayPascalString);
{$ENDIF FPC}

procedure ImportCSV_C(const sour: TArrayPascalString; OnNotify: TCSVSave_C);
procedure CustomImportCSV_C(const OnGetLine: TCSVGetLine_C; OnNotify: TCSVSave_C);
procedure ImportCSV_M(const sour: TArrayPascalString; OnNotify: TCSVSave_M);
procedure CustomImportCSV_M(const OnGetLine: TCSVGetLine_M; OnNotify: TCSVSave_M);
procedure ImportCSV_P(const sour: TArrayPascalString; OnNotify: TCSVSave_P);
procedure CustomImportCSV_P(const OnGetLine: TCSVGetLine_P; OnNotify: TCSVSave_P);

{ dynamic library }
function GetExtLib(LibName: SystemString): HMODULE;
function FreeExtLib(LibName: SystemString): Boolean;
function GetExtProc(const LibName, ProcName: SystemString): Pointer;

{ rawbyte }
type
  TArrayRawByte = array [0 .. MaxInt - 1] of Byte;
  PArrayRawByte = ^TArrayRawByte;

function umlCompareByteString(const s1: TPascalString; const s2: PArrayRawByte): Boolean; overload;
function umlCompareByteString(const s2: PArrayRawByte; const s1: TPascalString): Boolean; overload;
procedure umlSetByteString(const sour: TPascalString; const dest: PArrayRawByte); overload;
procedure umlSetByteString(const dest: PArrayRawByte; const sour: TPascalString); overload;
function umlGetByteString(const sour: PArrayRawByte; const L: Integer): TPascalString;

{ savememory }
procedure SaveMemory(p: Pointer; siz: NativeInt; DestFile: TPascalString);

{ fast cache for fileMD5 }
function umlFileMD5(FileName: TPascalString): TMD5; overload;
procedure Do_ThCacheFileMD5(ThSender: TCompute);
procedure umlCacheFileMD5(FileName: U_String);
procedure umlCacheFileMD5FromDirectory(Directory_, Filter_: U_String);

{ binary }
function umlBinToUInt8(Value: U_String): Byte;
function umlBinToUInt16(Value: U_String): Word;
function umlBinToUInt32(Value: U_String): Cardinal;
function umlBinToUInt64(Value: U_String): UInt64;
function umlUInt8ToBin(v: Byte): U_String;
function umlUInt16ToBin(v: Word): U_String;
function umlUInt32ToBin(v: Cardinal): U_String;
function umlUInt64ToBin(v: UInt64): U_String;

{ ascii detect }
function umlBufferIsASCII(buffer: Pointer; siz: NativeUInt): Boolean;

var
  Lib_DateTimeFormatSettings: TFormatSettings;

implementation

uses
{$IF Defined(WIN32) or Defined(WIN64)}
  Z.md5,
{$ENDIF}
  Z.Cipher, Z.Status, Z.MemoryStream;

procedure TReliableFileStream.InitIO;
begin
  if not FActivted then
      exit;

  DoStatus(PFormat('Reliable IO Open : %s', [umlGetFileName(FileName).text]));
  DoStatus(PFormat('Create Backup %s size: %s', [umlGetFileName(FileName).text, umlSizeToStr(SourceIO.Size).text]));

  BackupFileIO := TCore_FileStream.Create(FBackupFileName, fmCreate);
  BackupFileIO.Size := SourceIO.Size;
  SourceIO.Position := 0;
  BackupFileIO.Position := 0;
  BackupFileIO.CopyFrom(SourceIO, SourceIO.Size);
  BackupFileIO.Position := 0;
  DisposeObject(SourceIO);
  SourceIO := nil;
end;

procedure TReliableFileStream.FreeIO;
begin
  if not FActivted then
      exit;

  DisposeObject(BackupFileIO);
  BackupFileIO := nil;
  try
    umlDeleteFile(FFileName);
    umlRenameFile(FBackupFileName, FileName);
  except
  end;
  DoStatus(PFormat('Reliable IO Close : %s', [umlGetFileName(FileName).text]));
end;

procedure TReliableFileStream.SetSize(const NewSize: Int64);
begin
  SourceIO.Size := NewSize;
end;

procedure TReliableFileStream.SetSize(NewSize: longint);
begin
  SetSize(Int64(NewSize));
end;

constructor TReliableFileStream.Create(const FileName_: SystemString; IsNew_, IsWrite_: Boolean);
var
  m: Word;
begin
  inherited Create;
  if IsNew_ then
      m := fmCreate
  else if IsWrite_ then
      m := fmOpenReadWrite
  else
      m := fmOpenRead or fmShareDenyNone;
{$IFDEF ZDB_BACKUP}
  FActivted := IsNew_ or IsWrite_;
{$ELSE ZDB_BACKUP}
  FActivted := False;
{$ENDIF ZDB_BACKUP}
  SourceIO := TCore_FileStream.Create(FileName_, m);

  BackupFileIO := nil;
  FFileName := FileName_;
  FBackupFileName := FileName_ + '.save';
  umlDeleteFile(FBackupFileName);
  InitIO;
end;

destructor TReliableFileStream.Destroy;
begin
  DisposeObject(SourceIO);
  FreeIO;
  inherited Destroy;
end;

function TReliableFileStream.Write(const buffer; Count: longint): longint;
begin
  if FActivted then
    begin
      Result := BackupFileIO.Write(buffer, Count);
    end
  else
    begin
      Result := SourceIO.Write(buffer, Count);
    end;
end;

function TReliableFileStream.Read(var buffer; Count: longint): longint;
begin
  if FActivted then
    begin
      Result := BackupFileIO.Read(buffer, Count);
    end
  else
    begin
      Result := SourceIO.Read(buffer, Count);
    end;
end;

function TReliableFileStream.Seek(const Offset: Int64; origin: TSeekOrigin): Int64;
begin
  if FActivted then
    begin
      Result := BackupFileIO.Seek(Offset, origin);
    end
  else
    begin
      Result := SourceIO.Seek(Offset, origin);
    end;
end;

function TIOHnd.FixedString2Pascal(s: TBytes): TPascalString;
var
  buff: TBytes;
begin
  if (length(s) > 0) and (s[0] > 0) then
    begin
      SetLength(buff, s[0]);
      CopyPtr(@s[1], @buff[0], length(buff));
      Result.Bytes := buff;
      SetLength(buff, 0);
    end
  else
      Result := '';
end;

procedure TIOHnd.Pascal2FixedString(var n: TPascalString; var out_: TBytes);
var
  buff: TBytes;
begin
  while True do
    begin
      buff := n.Bytes;
      if length(buff) > FixedStringL - 1 then
          n.DeleteLast
      else
          break;
    end;

  SetLength(out_, FixedStringL);
  out_[0] := length(buff);
  if out_[0] > 0 then
      CopyPtr(@buff[0], @out_[1], out_[0]);
  SetLength(buff, 0);
end;

function TIOHnd.CheckFixedStringLoss(s: TPascalString): Boolean;
var
  buff: TBytes;
begin
  buff := s.Bytes;
  Result := length(buff) > FixedStringL - 1;
  SetLength(buff, 0);
end;

procedure TListPascalString_Helper_.FillToArry(var Output_: U_StringArray);
var
  i: Integer;
begin
  SetLength(Output_, Count);
  for i := 0 to Count - 1 do
      Output_[i] := Items[i];
end;

function umlBytesOf(const s: TPascalString): TBytes;
begin
  Result := s.Bytes
end;

function umlStringOf(const s: TBytes): TPascalString;
begin
  Result.Bytes := s;
end;

function umlNewString(const s: TPascalString): PPascalString;
var
  p: PPascalString;
begin
  new(p);
  p^ := s;
  Result := p;
end;

procedure umlFreeString(const p: PPascalString);
begin
  if p <> nil then
    begin
      p^ := '';
      Dispose(p);
    end;
end;

function umlComparePosStr(const s: TPascalString; Offset: Integer; const t: TPascalString): Boolean;
begin
  Result := s.ComparePos(Offset, @t);
end;

function umlPos(const SubStr, s: TPascalString; const Offset: Integer = 1): Integer;
begin
  Result := s.GetPos(SubStr, Offset);
end;

function umlVarToStr(const v: Variant; const Base64Conver: Boolean): TPascalString; overload;
var
  n, b64: TPascalString;
begin
  try
    case VarType(v) of
      varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord: Result := IntToStr(v);
      varInt64: Result := IntToStr(Int64(v));
      varUInt64: {$IFDEF FPC} Result := IntToStr(UInt64(v)); {$ELSE} Result := UIntToStr(UInt64(v)); {$ENDIF}
      varSingle, varDouble, varCurrency, varDate: Result := FloatToStr(v);
      varOleStr, varString, varUString:
        begin
          n.text := VarToStr(v);

          if Base64Conver and umlExistsChar(n, #10#13#9#8#0) then
            begin
              umlEncodeLineBASE64(n, b64);
              Result := '___base64:' + b64.text;
            end
          else
              Result := n.text;
        end;
      varBoolean: Result := umlBoolToStr(v);
      else
        Result := VarToStr(v);
    end;
  except
    try
        Result := VarToStr(v);
    except
        Result := '';
    end;
  end;
end;

function umlVarToStr(const v: Variant): TPascalString;
begin
  Result := umlVarToStr(v, True);
end;

function umlStrToVar(const s: TPascalString): Variant;
var
  b64: TPascalString;
begin
  if s.Exists([#10, #13, #9, #8, #0]) then
    begin
      umlEncodeLineBASE64(s, b64);
      Result := '___base64:' + b64.text;
    end
  else
      Result := s.text;
end;

function umlMax(const v1, v2: UInt64): UInt64;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Cardinal): Cardinal;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Word): Word;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Byte): Byte;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Int64): Int64;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Integer): Integer;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: SmallInt): SmallInt;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: ShortInt): ShortInt;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Double): Double;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Single): Single;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: UInt64): UInt64;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Cardinal): Cardinal;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Word): Word;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Byte): Byte;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Int64): Int64;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Integer): Integer;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: SmallInt): SmallInt;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: ShortInt): ShortInt;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Double): Double;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Single): Single;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlClamp(const v, min_, max_: Integer): Integer;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: UInt64): UInt64;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: Cardinal): Cardinal;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: Word): Word;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: Byte): Byte;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: Int64): Int64;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: SmallInt): SmallInt;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: ShortInt): ShortInt;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: Double): Double;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlClamp(const v, min_, max_: Single): Single;
begin
  if min_ > max_ then
      Result := umlClamp(v, max_, min_)
  else if v > max_ then
      Result := max_
  else if v < min_ then
      Result := min_
  else
      Result := v;
end;

function umlInRange(const v, min_, max_: Integer): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: UInt64): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: Cardinal): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: Word): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: Byte): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: Int64): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: SmallInt): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: ShortInt): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: Double): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlInRange(const v, min_, max_: Single): Boolean;
begin
  Result := (v >= umlMin(min_, max_)) and (v <= umlMax(min_, max_));
end;

function umlCompareText(s1, s2: TPascalString): Integer;
  function comp_size(const A, B: Integer): Integer;
  begin
    if A = B then
        Result := 0
    else if A < B then
        Result := -1
    else
        Result := 1;
  end;

  function IsWide_(p: PPascalString): Byte;
  var
    c: SystemChar;
  begin
    for c in p^.buff do
      if Ord(c) > 127 then
          exit(1);
    Result := 0;
  end;

begin
  Result := comp_size(IsWide_(@s1), IsWide_(@s2));
  if Result = 0 then
    begin
      Result := comp_size(s1.L, s2.L);
      if Result = 0 then
          Result := CompareText(s1, s2);
    end;
end;

function umlGetResourceStream(const FileName: TPascalString): TCore_Stream;
var
  n: TPascalString;
begin
  if FileName.Exists('.') then
      n := umlDeleteLastStr(FileName, '.')
  else
      n := FileName;

  Result := TCore_ResourceStream.Create(HInstance, n.text, RT_RCDATA);
end;

function umlSameVarValue(const v1, v2: Variant): Boolean;
begin
  try
      Result := VarSameValue(v1, v2);
  except
      Result := False;
  end;
end;

function umlSameVariant(const v1, v2: Variant): Boolean;
begin
  try
      Result := VarSameValue(v1, v2);
  except
      Result := False;
  end;
end;

function umlRandom(const rnd: TMT19937Random): Integer;
begin
  Result := rnd.Rand32(MaxInt);
end;

function umlRandom: Integer;
begin
  Result := MT19937Rand32(MaxInt);
end;

function umlRandomRange(const rnd: TMT19937Random; const min_, max_: Integer): Integer;
var
  mn, mx: Integer;
begin
  if min_ = max_ then
    begin
      Result := min_;
      exit;
    end;

  mn := min_;
  mx := max_;

  if mn > mx then
      inc(mn)
  else
      inc(mx);

  if mn > mx then
      Result := rnd.Rand32(mn - mx) + mx
  else
      Result := rnd.Rand32(mx - mn) + mn;
end;

function umlRandomRange64(const rnd: TMT19937Random; const min_, max_: Int64): Int64;
var
  mn, mx: Int64;
begin
  if min_ = max_ then
    begin
      Result := min_;
      exit;
    end;

  mn := min_;
  mx := max_;

  if mn > mx then
      inc(mn)
  else
      inc(mx);

  if mn > mx then
      Result := rnd.Rand64(mn - mx) + mx
  else
      Result := rnd.Rand64(mx - mn) + mn;
end;

function umlRandomRangeS(const rnd: TMT19937Random; const min_, max_: Single): Single;
begin
  Result := (umlRandomRange64(rnd, Trunc(min_ * 1000), Trunc(max_ * 1000))) * 0.001;
end;

function umlRandomRangeD(const rnd: TMT19937Random; const min_, max_: Double): Double;
begin
  Result := (umlRandomRange64(rnd, Trunc(min_ * 10000), Trunc(max_ * 10000))) * 0.0001;
end;

function umlRandomRangeF(const rnd: TMT19937Random; const min_, max_: Double): Double;
begin
  Result := (umlRandomRange64(rnd, Trunc(min_ * 10000), Trunc(max_ * 10000))) * 0.0001;
end;

function umlRandomRange(const min_, max_: Integer): Integer;
var
  mn, mx: Integer;
begin
  if min_ = max_ then
    begin
      Result := min_;
      exit;
    end;
  mn := min_;
  mx := max_;

  if mn > mx then
      inc(mn)
  else
      inc(mx);

  if mn > mx then
      Result := MT19937Rand32(mn - mx) + mx
  else
      Result := MT19937Rand32(mx - mn) + mn;
end;

function umlRandomRange64(const min_, max_: Int64): Int64;
var
  mn, mx: Int64;
begin
  if min_ = max_ then
    begin
      Result := min_;
      exit;
    end;
  mn := min_;
  mx := max_;

  if mn > mx then
      inc(mn)
  else
      inc(mx);

  if mn > mx then
      Result := MT19937Rand64(mn - mx) + mx
  else
      Result := MT19937Rand64(mx - mn) + mn;
end;

function umlRandomRangeS(const min_, max_: Single): Single;
begin
  Result := (umlRandomRange64(Trunc(min_ * 1000), Trunc(max_ * 1000))) * 0.001;
end;

function umlRandomRangeD(const min_, max_: Double): Double;
begin
  Result := (umlRandomRange64(Trunc(min_ * 10000), Trunc(max_ * 10000))) * 0.0001;
end;

function umlRandomRangeF(const min_, max_: Double): Double;
begin
  Result := (umlRandomRange64(Trunc(min_ * 10000), Trunc(max_ * 10000))) * 0.0001;
end;

function umlDefaultTime: Double;
begin
  Result := Now;
end;

function umlNow: Double;
begin
  Result := Now();
end;

function umlDefaultAttrib: Integer;
begin
  Result := 0;
end;

function umlBoolToStr(const Value: Boolean): TPascalString;
begin
  if Value then
      Result := 'True'
  else
      Result := 'False';
end;

function umlStrToBool(const Value: TPascalString): Boolean;
var
  NewValue: TPascalString;
begin
  NewValue := umlTrimSpace(Value);
  if NewValue.Same('Yes', 'ON') then
      Result := True
  else if NewValue.Same('No', 'OFF') then
      Result := False
  else if NewValue.Same('True') then
      Result := True
  else if NewValue.Same('False') then
      Result := False
  else if NewValue.Same('1') then
      Result := True
  else if NewValue.Same('0') then
      Result := False
  else
      Result := False;
end;

function umlFileExists(const FileName: TPascalString): Boolean;
begin
  if FileName.L > 0 then
      Result := FileExists(FileName.text)
  else
      Result := False;
end;

function umlDirectoryExists(const DirectoryName: TPascalString): Boolean;
begin
  if DirectoryName.L > 0 then
      Result := DirectoryExists(DirectoryName.text)
  else
      Result := False;
end;

function umlCreateDirectory(const DirectoryName: TPascalString): Boolean;
begin
  Result := umlDirectoryExists(DirectoryName);
  if Result then
      exit;

  try
      Result := ForceDirectories(DirectoryName.text);
  except
    try
        Result := CreateDir(DirectoryName.text);
    except
        Result := False;
    end;
  end;
end;

function umlCurrentDirectory: TPascalString;
begin
  Result.text := GetCurrentDir;
end;

function umlCurrentPath: TPascalString;
begin
  Result.text := GetCurrentDir;
  case CurrentPlatform of
    epWin32, epWin64: if (Result.L = 0) or (Result.Last <> '\') then
          Result := Result.text + '\';
    else
      if (Result.L = 0) or (Result.Last <> '/') then
          Result := Result.text + '/';
  end;
end;

function umlGetCurrentPath: TPascalString;
begin
  Result := umlCurrentPath();
end;

procedure umlSetCurrentPath(ph: TPascalString);
begin
  SetCurrentDir(ph.text);
end;

function umlFindFirstFile(const FileName: TPascalString; var SR: TSR): Boolean;
label SearchPoint;
begin
  if FindFirst(FileName.text, faAnyFile, SR) <> 0 then
    begin
      Result := False;
      exit;
    end;
  if ((SR.Attr and faDirectory) <> faDirectory) then
    begin
      Result := True;
      exit;
    end;
SearchPoint:
  if FindNext(SR) <> 0 then
    begin
      Result := False;
      exit;
    end;
  if ((SR.Attr and faDirectory) <> faDirectory) then
    begin
      Result := True;
      exit;
    end;
  goto SearchPoint;
end;

function umlFindNextFile(var SR: TSR): Boolean;
label SearchPoint;
begin
SearchPoint:
  if FindNext(SR) <> 0 then
    begin
      Result := False;
      exit;
    end;
  if ((SR.Attr and faDirectory) <> faDirectory) then
    begin
      Result := True;
      exit;
    end;
  goto SearchPoint;
end;

function umlFindFirstDir(const DirName: TPascalString; var SR: TSR): Boolean;
label SearchPoint;
begin
  if FindFirst(DirName.text, faAnyFile, SR) <> 0 then
    begin
      Result := False;
      exit;
    end;
  if ((SR.Attr and faDirectory) = faDirectory) and (SR.Name <> '.') and (SR.Name <> '..') then
    begin
      Result := True;
      exit;
    end;
SearchPoint:
  if FindNext(SR) <> 0 then
    begin
      Result := False;
      exit;
    end;
  if ((SR.Attr and faDirectory) = faDirectory) and (SR.Name <> '.') and (SR.Name <> '..') then
    begin
      Result := True;
      exit;
    end;
  goto SearchPoint;
end;

function umlFindNextDir(var SR: TSR): Boolean;
label SearchPoint;
begin
SearchPoint:
  if FindNext(SR) <> 0 then
    begin
      Result := False;
      exit;
    end;
  if ((SR.Attr and faDirectory) = faDirectory) and (SR.Name <> '.') and (SR.Name <> '..') then
    begin
      Result := True;
      exit;
    end;
  goto SearchPoint;
end;

procedure umlFindClose(var SR: TSR);
begin
  FindClose(SR);
end;

function umlGetFileList(const FullPath: TPascalString; AsLst: TCore_Strings): Integer;
var
  _SR: TSR;
begin
  Result := 0;
  if umlFindFirstFile(umlCombineFileName(FullPath, '*'), _SR) then
    begin
      repeat
        AsLst.Add(_SR.Name);
        inc(Result);
      until not umlFindNextFile(_SR);
    end;
  umlFindClose(_SR);
end;

function umlGetDirList(const FullPath: TPascalString; AsLst: TCore_Strings): Integer;
var
  _SR: TSR;
begin
  Result := 0;
  if umlFindFirstDir(umlCombineFileName(FullPath, '*'), _SR) then
    begin
      repeat
        AsLst.Add(_SR.Name);
        inc(Result);
      until not umlFindNextDir(_SR);
    end;
  umlFindClose(_SR);
end;

function umlGetFileList(const FullPath: TPascalString; AsLst: TPascalStringList): Integer;
var
  _SR: TSR;
begin
  Result := 0;
  if umlFindFirstFile(umlCombineFileName(FullPath, '*'), _SR) then
    begin
      repeat
        AsLst.Add(_SR.Name);
        inc(Result);
      until not umlFindNextFile(_SR);
    end;
  umlFindClose(_SR);
end;

function umlGetDirList(const FullPath: TPascalString; AsLst: TPascalStringList): Integer;
var
  _SR: TSR;
begin
  Result := 0;
  if umlFindFirstDir(umlCombineFileName(FullPath, '*'), _SR) then
    begin
      repeat
        AsLst.Add(_SR.Name);
        inc(Result);
      until not umlFindNextDir(_SR);
    end;
  umlFindClose(_SR);
end;

function umlGetFileListWithFullPath(const FullPath: TPascalString): U_StringArray;
var
  ph: TPascalString;
  L: TPascalStringList;
  i: Integer;
begin
  ph := FullPath;
  L := TPascalStringList.Create;
  umlGetFileList(FullPath, L);
  SetLength(Result, L.Count);
  for i := 0 to L.Count - 1 do
      Result[i] := umlCombineFileName(ph, L[i]).text;
  DisposeObject(L);
end;

function umlGetDirListWithFullPath(const FullPath: TPascalString): U_StringArray;
var
  ph: TPascalString;
  L: TPascalStringList;
  i: Integer;
begin
  ph := FullPath;
  L := TPascalStringList.Create;
  umlGetDirList(FullPath, L);
  SetLength(Result, L.Count);
  for i := 0 to L.Count - 1 do
      Result[i] := umlCombinePath(ph, L[i]).text;
  DisposeObject(L);
end;

function umlGetFileListPath(const FullPath: TPascalString): U_StringArray;
var
  ph: TPascalString;
  L: TPascalStringList;
  i: Integer;
begin
  ph := FullPath;
  L := TPascalStringList.Create;
  umlGetFileList(FullPath, L);
  SetLength(Result, L.Count);
  for i := 0 to L.Count - 1 do
      Result[i] := L[i];
  DisposeObject(L);
end;

function umlGetDirListPath(const FullPath: TPascalString): U_StringArray;
var
  ph: TPascalString;
  L: TPascalStringList;
  i: Integer;
begin
  ph := FullPath;
  L := TPascalStringList.Create;
  umlGetDirList(FullPath, L);
  SetLength(Result, L.Count);
  for i := 0 to L.Count - 1 do
      Result[i] := L[i];
  DisposeObject(L);
end;

function umlCombinePath(const s1, s2: TPascalString): TPascalString;
begin
  if CurrentPlatform in [epWin32, epWin64] then
      Result := umlCombineWinPath(s1, s2)
  else
      Result := umlCombineUnixPath(s1, s2);
end;

function umlCombineFileName(const pathName, FileName: TPascalString): TPascalString;
begin
  if CurrentPlatform in [epWin32, epWin64] then
      Result := umlCombineWinFileName(pathName, FileName)
  else
      Result := umlCombineUnixFileName(pathName, FileName);
end;

function umlCombineUnixPath(const s1, s2: TPascalString): TPascalString;
var
  n1, n2, n: TPascalString;
begin
  n1 := umlTrimSpace(s1);
  n2 := umlTrimSpace(s2);

  n1 := umlCharReplace(n1, '\', '/');
  n2 := umlCharReplace(n2, '\', '/');

  if (n2.L > 0) and (n2.First = '/') then
      n2.DeleteFirst;

  if n1.L > 0 then
    begin
      if n1.Last = '/' then
          Result := n1.text + n2.text
      else
          Result := n1.text + '/' + n2.text;
    end
  else
      Result := n2;

  repeat
    n := Result;
    Result := umlStringReplace(Result, '//', '/', True);
  until Result.Same(n);
  if (Result.L > 0) and (Result.Last <> '/') then
      Result.Append('/');
end;

function umlCombineUnixFileName(const pathName, FileName: TPascalString): TPascalString;
var
  pn, fn, n: TPascalString;
begin
  pn := umlTrimSpace(pathName);
  fn := umlTrimSpace(FileName);

  pn := umlCharReplace(pn, '\', '/');
  fn := umlCharReplace(fn, '\', '/');

  if (fn.L > 0) and (fn.First = '/') then
      fn.DeleteFirst;
  if (fn.L > 0) and (fn.Last = '/') then
      fn.DeleteLast;

  if pn.L > 0 then
    begin
      if pn.Last = '/' then
          Result := pn.text + fn.text
      else
          Result := pn.text + '/' + fn.text;
    end
  else
      Result := fn;

  repeat
    n := Result;
    Result := umlStringReplace(Result, '//', '/', True);
  until Result.Same(n);
end;

function umlCombineWinPath(const s1, s2: TPascalString): TPascalString;
var
  n1, n2, n: TPascalString;
begin
  n1 := umlTrimSpace(s1);
  n2 := umlTrimSpace(s2);

  n1 := umlCharReplace(n1, '/', '\');
  n2 := umlCharReplace(n2, '/', '\');

  if (n2.L > 0) and (n2.First = '\') then
      n2.DeleteFirst;

  if n1.L > 0 then
    begin
      if n1.Last = '\' then
          Result := n1.text + n2.text
      else
          Result := n1.text + '\' + n2.text;
    end
  else
      Result := n2;

  repeat
    n := Result;
    Result := umlStringReplace(Result, '\\', '\', True);
  until Result.Same(n);
  if (Result.L > 0) and (Result.Last <> '\') then
      Result.Append('\');
end;

function umlCombineWinFileName(const pathName, FileName: TPascalString): TPascalString;
var
  pn, fn, n: TPascalString;
begin
  pn := umlTrimSpace(pathName);
  fn := umlTrimSpace(FileName);

  pn := umlCharReplace(pn, '/', '\');
  fn := umlCharReplace(fn, '/', '\');

  if (fn.L > 0) and (fn.First = '\') then
      fn.DeleteFirst;
  if (fn.L > 0) and (fn.Last = '\') then
      fn.DeleteLast;

  if pn.L > 0 then
    begin
      if pn.Last = '\' then
          Result := pn.text + fn.text
      else
          Result := pn.text + '\' + fn.text;
    end
  else
      Result := fn;

  repeat
    n := Result;
    Result := umlStringReplace(Result, '\\', '\', True);
  until Result.Same(n);

  if Result.Last = '\' then
      Result.DeleteLast;
end;

function umlGetFileName(platform_: TExecutePlatform; const s: TPascalString): TPascalString;
var
  n: TPascalString;
begin
  case platform_ of
    epWin32, epWin64:
      begin
        n := umlCharReplace(umlTrimSpace(s), '/', '\');
        if n.L = 0 then
            Result := ''
        else if (n.Last = '\') then
            Result := ''
        else if n.Exists('\') then
            Result := umlGetLastStr(n, '\')
        else
            Result := n;
      end;
    else
      begin
        n := umlCharReplace(umlTrimSpace(s), '\', '/');
        if n.L = 0 then
            Result := ''
        else if (n.Last = '/') then
            Result := ''
        else if n.Exists('/') then
            Result := umlGetLastStr(n, '/')
        else
            Result := n;
      end;
  end;
end;

function umlGetFileName(const s: TPascalString): TPascalString;
begin
  Result := umlGetFileName(CurrentPlatform, s);
end;

function umlGetWindowsFileName(const s: TPascalString): TPascalString;
var
  n: TPascalString;
begin
  n := umlCharReplace(umlTrimSpace(s), '/', '\');
  if n.L = 0 then
      Result := ''
  else if (n.Last = '\') then
      Result := ''
  else if n.Exists('\') then
      Result := umlGetLastStr(n, '\')
  else
      Result := n;
end;

function umlGetUnixFileName(const s: TPascalString): TPascalString;
var
  n: TPascalString;
begin
  n := umlCharReplace(umlTrimSpace(s), '\', '/');
  if n.L = 0 then
      Result := ''
  else if (n.Last = '/') then
      Result := ''
  else if n.Exists('/') then
      Result := umlGetLastStr(n, '/')
  else
      Result := n;
end;

function umlGetFilePath(platform_: TExecutePlatform; const s: TPascalString): TPascalString;
var
  n: TPascalString;
begin
  case platform_ of
    epWin32, epWin64:
      begin
        n := umlCharReplace(umlTrimSpace(s), '/', '\');
        if n.L = 0 then
            Result := ''
        else if not n.Exists('\') then
            Result := ''
        else if (n.Last <> '\') then
            Result := umlDeleteLastStr(n, '\')
        else
            Result := n;
        if umlMultipleMatch('?:', Result) then
            Result.Append('\');
      end;
    else
      begin
        n := umlCharReplace(umlTrimSpace(s), '\', '/');
        if n.L = 0 then
            Result := ''
        else if not n.Exists('/') then
            Result := ''
        else if (n.Last <> '/') then
            Result := umlDeleteLastStr(n, '/')
        else
            Result := n;
      end;
  end;
end;

function umlGetFilePath(const s: TPascalString): TPascalString;
begin
  Result := umlGetFilePath(CurrentPlatform, s);
end;

function umlGetWindowsFilePath(const s: TPascalString): TPascalString;
var
  n: TPascalString;
begin
  n := umlCharReplace(umlTrimSpace(s), '/', '\');
  if n.L = 0 then
      Result := ''
  else if not n.Exists('\') then
      Result := ''
  else if (n.Last <> '\') then
      Result := umlDeleteLastStr(n, '\')
  else
      Result := n;
  if umlMultipleMatch('?:', Result) then
      Result.Append('\');
end;

function umlGetUnixFilePath(const s: TPascalString): TPascalString;
var
  n: TPascalString;
begin
  n := umlCharReplace(umlTrimSpace(s), '\', '/');
  if n.L = 0 then
      Result := ''
  else if not n.Exists('/') then
      Result := ''
  else if (n.Last <> '/') then
      Result := umlDeleteLastStr(n, '/')
  else
      Result := n;
end;

function umlChangeFileExt(const s, ext: TPascalString): TPascalString;
var
  ph, fn: TPascalString;
  n: TPascalString;
begin
  if s.L = 0 then
    begin
      Result := ext;
      exit;
    end;

  ph := umlGetFilePath(s);
  fn := umlGetFileName(s);

  n := ext;
  if (n.L > 0) and (n.First <> '.') then
      n.text := '.' + n.text;
  if umlExistsChar(fn, '.') then
      Result := umlDeleteLastStr(fn, '.') + n
  else
      Result := fn + n;

  if ph.L > 0 then
      Result := umlCombineFileName(ph, Result);
end;

function umlGetFileExt(const s: TPascalString): TPascalString;
begin
  if (s.L > 0) and (umlExistsChar(s, '.')) then
      Result := '.' + umlGetLastStr(s, '.')
  else
      Result := '';
end;

procedure InitIOHnd(var IOHnd: TIOHnd);
begin
  IOHnd.IsOnlyRead := True;
  IOHnd.IsOpen := False;
  IOHnd.AutoFree := False;
  IOHnd.Handle := nil;
  IOHnd.Time := 0;
  IOHnd.Size := 0;
  IOHnd.Position := 0;
  IOHnd.FileName := '';
  IOHnd.Cache.UsedWriteCache := False;
  IOHnd.Cache.PrepareWriteBuff := nil;
  IOHnd.Cache.UsedReadCache := False;
  IOHnd.Cache.PrepareReadPosition := -1;
  IOHnd.Cache.PrepareReadBuff := nil;
  IOHnd.IORead := 0;
  IOHnd.IOWrite := 0;
  IOHnd.ChangeFromWrite := False;
  IOHnd.FixedStringL := 64 + 1;
  IOHnd.Data := nil;
  IOHnd.Return := C_NotError;
end;

function umlFileCreateAsStream(const FileName: TPascalString; stream: U_Stream; var IOHnd: TIOHnd; OnlyRead_: Boolean): Boolean;
begin
  if IOHnd.IsOpen = True then
    begin
      IOHnd.Return := C_FileIsActive;
      Result := False;
      exit;
    end;
  stream.Position := 0;
  IOHnd.Handle := stream;
  IOHnd.Cache.UsedWriteCache := (IOHnd.Handle is TCore_FileStream) or (IOHnd.Handle is TReliableFileStream);
  IOHnd.Cache.UsedReadCache := IOHnd.Cache.UsedWriteCache;
  IOHnd.Return := C_NotError;
  IOHnd.Size := stream.Size;
  IOHnd.Position := stream.Position;
  IOHnd.Time := umlDefaultTime;
  IOHnd.FileName := FileName;
  IOHnd.IsOpen := True;
  IOHnd.IsOnlyRead := OnlyRead_;
  IOHnd.AutoFree := False;
  Result := True;
end;

function umlFileCreateAsStream(const FileName: TPascalString; stream: U_Stream; var IOHnd: TIOHnd): Boolean;
begin
  Result := umlFileCreateAsStream(FileName, stream, IOHnd, False);
end;

function umlFileCreateAsStream(stream: U_Stream; var IOHnd: TIOHnd): Boolean;
begin
  Result := umlFileCreateAsStream('', stream, IOHnd, False);
end;

function umlFileCreateAsStream(stream: U_Stream; var IOHnd: TIOHnd; OnlyRead_: Boolean): Boolean;
begin
  Result := umlFileCreateAsStream('', stream, IOHnd, False);
end;

function umlFileOpenAsStream(const FileName: TPascalString; stream: U_Stream; var IOHnd: TIOHnd; OnlyRead_: Boolean): Boolean;
begin
  if IOHnd.IsOpen = True then
    begin
      IOHnd.Return := C_FileIsActive;
      Result := False;
      exit;
    end;
  stream.Position := 0;
  IOHnd.Handle := stream;
  IOHnd.Cache.UsedWriteCache := (IOHnd.Handle is TCore_FileStream) or (IOHnd.Handle is TReliableFileStream);
  IOHnd.Cache.UsedReadCache := IOHnd.Cache.UsedWriteCache;
  IOHnd.Return := C_NotError;
  IOHnd.Size := stream.Size;
  IOHnd.Position := stream.Position;
  IOHnd.Time := umlDefaultTime;
  IOHnd.FileName := FileName;
  IOHnd.IsOpen := True;
  IOHnd.IsOnlyRead := OnlyRead_;
  IOHnd.AutoFree := False;
  Result := True;
end;

function umlFileCreateAsMemory(var IOHnd: TIOHnd): Boolean;
begin
  if IOHnd.IsOpen = True then
    begin
      IOHnd.Return := C_FileIsActive;
      Result := False;
      exit;
    end;
  IOHnd.Handle := TMS64.CustomCreate(8192);
  IOHnd.Cache.UsedWriteCache := False;
  IOHnd.Cache.UsedReadCache := False;
  IOHnd.Return := C_NotError;
  IOHnd.Size := IOHnd.Handle.Size;
  IOHnd.Position := IOHnd.Handle.Position;
  IOHnd.Time := umlDefaultTime;
  IOHnd.FileName := 'Memory';
  IOHnd.IsOpen := True;
  IOHnd.IsOnlyRead := False;
  IOHnd.AutoFree := True;
  Result := True;
end;

function umlFileCreate(const FileName: TPascalString; var IOHnd: TIOHnd): Boolean;
begin
  if IOHnd.IsOpen = True then
    begin
      IOHnd.Return := C_FileIsActive;
      Result := False;
      exit;
    end;
  try
      IOHnd.Handle := TReliableFileStream.Create(FileName.text, True, True);
  except
    IOHnd.Handle := nil;
    IOHnd.Return := C_CreateFileError;
    Result := False;
    exit;
  end;
  IOHnd.Cache.UsedWriteCache := True;
  IOHnd.Cache.UsedReadCache := True;
  IOHnd.Return := C_NotError;
  IOHnd.Size := 0;
  IOHnd.Position := 0;
  IOHnd.Time := Now;
  IOHnd.FileName := FileName;
  IOHnd.IsOpen := True;
  IOHnd.IsOnlyRead := False;
  IOHnd.AutoFree := True;
  Result := True;
end;

function umlFileOpen(const FileName: TPascalString; var IOHnd: TIOHnd; OnlyRead_: Boolean): Boolean;
begin
  if IOHnd.IsOpen = True then
    begin
      IOHnd.Return := C_FileIsActive;
      Result := False;
      exit;
    end;
  if not umlFileExists(FileName) then
    begin
      IOHnd.Return := C_NotFindFile;
      Result := False;
      exit;
    end;
  try
      IOHnd.Handle := TReliableFileStream.Create(FileName.text, False, not OnlyRead_);
  except
    IOHnd.Handle := nil;
    IOHnd.Return := C_OpenFileError;
    Result := False;
    exit;
  end;
  IOHnd.Cache.UsedWriteCache := True;
  IOHnd.Cache.UsedReadCache := True;
  IOHnd.IsOnlyRead := OnlyRead_;
  IOHnd.Return := C_NotError;
  IOHnd.Size := IOHnd.Handle.Size;
  IOHnd.Position := 0;
  IOHnd.Time := umlGetFileTime(FileName);
  IOHnd.FileName := FileName;
  IOHnd.IsOpen := True;
  IOHnd.AutoFree := True;
  Result := True;
end;

function umlFileClose(var IOHnd: TIOHnd): Boolean;
begin
  if IOHnd.IsOpen = False then
    begin
      IOHnd.Return := C_NotOpenFile;
      Result := False;
      exit;
    end;
  if IOHnd.Handle = nil then
    begin
      IOHnd.Return := C_FileHandleError;
      Result := False;
      exit;
    end;

  umlFileFlushWriteCache(IOHnd);

  if IOHnd.Cache.PrepareReadBuff <> nil then
      DisposeObject(IOHnd.Cache.PrepareReadBuff);
  IOHnd.Cache.PrepareReadBuff := nil;
  IOHnd.Cache.PrepareReadPosition := -1;

  try
    if IOHnd.AutoFree then
        DisposeObject(IOHnd.Handle)
    else
        IOHnd.Handle := nil;
  except
  end;
  IOHnd.Handle := nil;
  IOHnd.Return := C_NotError;
  IOHnd.Time := umlDefaultTime;
  IOHnd.FileName := '';
  IOHnd.IsOpen := False;
  IOHnd.ChangeFromWrite := False;
  Result := True;
end;

function umlFileUpdate(var IOHnd: TIOHnd): Boolean;
begin
  if (IOHnd.IsOpen = False) or (IOHnd.Handle = nil) then
    begin
      IOHnd.Return := C_FileHandleError;
      Result := False;
      exit;
    end;

  umlFileFlushWriteCache(IOHnd);
  umlResetPrepareRead(IOHnd);
  IOHnd.ChangeFromWrite := False;

  Result := True;
end;

function umlFileTest(var IOHnd: TIOHnd): Boolean;
begin
  if (IOHnd.IsOpen = False) or (IOHnd.Handle = nil) then
    begin
      IOHnd.Return := C_FileHandleError;
      Result := False;
      exit;
    end;
  IOHnd.Return := C_NotError;
  Result := True;
end;

procedure umlResetPrepareRead(var IOHnd: TIOHnd);
begin
  if IOHnd.Cache.PrepareReadBuff <> nil then
      DisposeObject(IOHnd.Cache.PrepareReadBuff);
  IOHnd.Cache.PrepareReadBuff := nil;
  IOHnd.Cache.PrepareReadPosition := -1;
end;

function umlFilePrepareRead(var IOHnd: TIOHnd; Size: Int64; var buff): Boolean;
var
  m64: TMS64;
  preRedSiz: Int64;
begin
  Result := False;

  if not IOHnd.Cache.UsedReadCache then
      exit;

  if Size > C_PrepareReadCacheSize then
    begin
      umlResetPrepareRead(IOHnd);
      IOHnd.Handle.Position := IOHnd.Position;
      exit;
    end;

  if IOHnd.Cache.PrepareReadBuff = nil then
      IOHnd.Cache.PrepareReadBuff := TMS64.Create;

  m64 := TMS64(IOHnd.Cache.PrepareReadBuff);

  if (IOHnd.Position < IOHnd.Cache.PrepareReadPosition) or (IOHnd.Cache.PrepareReadPosition + m64.Size < IOHnd.Position + Size) then
    begin
      // prepare read buffer
      IOHnd.Handle.Position := IOHnd.Position;
      IOHnd.Cache.PrepareReadPosition := IOHnd.Position;

      m64.Clear;
      IOHnd.Cache.PrepareReadPosition := IOHnd.Handle.Position;
      if IOHnd.Handle.Size - IOHnd.Handle.Position >= C_PrepareReadCacheSize then
        begin
          Result := m64.CopyFrom(IOHnd.Handle, C_PrepareReadCacheSize) = C_PrepareReadCacheSize;
          inc(IOHnd.IORead, C_PrepareReadCacheSize);
        end
      else
        begin
          preRedSiz := IOHnd.Handle.Size - IOHnd.Handle.Position;
          Result := m64.CopyFrom(IOHnd.Handle, preRedSiz) = preRedSiz;
          inc(IOHnd.IORead, preRedSiz);
        end;
    end;

  if (IOHnd.Position >= IOHnd.Cache.PrepareReadPosition) and (IOHnd.Cache.PrepareReadPosition + m64.Size >= IOHnd.Position + Size) then
    begin
      CopyPtr(GetOffset(m64.Memory, IOHnd.Position - IOHnd.Cache.PrepareReadPosition), @buff, Size);
      inc(IOHnd.Position, Size);
      Result := True;
    end
  else
    begin
      // safe process
      umlResetPrepareRead(IOHnd);
      IOHnd.Handle.Position := IOHnd.Position;
      exit;
    end;
end;

function umlFileRead(var IOHnd: TIOHnd; const Size: Int64; var buff): Boolean;
var
  BuffPointer: Pointer;
  i: NativeInt;
begin
  if not umlFileFlushWriteCache(IOHnd) then
    begin
      Result := False;
      exit;
    end;

  if Size = 0 then
    begin
      IOHnd.Return := C_NotError;
      Result := True;
      exit;
    end;

  if umlFilePrepareRead(IOHnd, Size, buff) then
    begin
      IOHnd.Return := C_NotError;
      Result := True;
      exit;
    end;

  try
    if Size > C_MaxBufferFragmentSize then
      begin
        // process Chunk buffer
        BuffPointer := @buff;
        for i := 1 to (Size div C_MaxBufferFragmentSize) do
          begin
            if IOHnd.Handle.Read(BuffPointer^, C_MaxBufferFragmentSize) <> C_MaxBufferFragmentSize then
              begin
                IOHnd.Return := C_FileReadError;
                Result := False;
                exit;
              end;
            BuffPointer := GetOffset(BuffPointer, C_MaxBufferFragmentSize);
          end;
        // process buffer rest
        i := Size mod C_MaxBufferFragmentSize;
        if IOHnd.Handle.Read(BuffPointer^, i) <> i then
          begin
            IOHnd.Return := C_FileReadError;
            Result := False;
            exit;
          end;
        inc(IOHnd.Position, Size);
        IOHnd.Return := C_NotError;
        Result := True;
        inc(IOHnd.IORead, Size);
        exit;
      end;
    if IOHnd.Handle.Read(buff, Size) <> Size then
      begin
        IOHnd.Return := C_FileReadError;
        Result := False;
        exit;
      end;
    inc(IOHnd.Position, Size);
    IOHnd.Return := C_NotError;
    Result := True;
    inc(IOHnd.IORead, Size);
  except
    IOHnd.Return := C_FileReadError;
    Result := False;
  end;
end;

function umlBlockRead(var IOHnd: TIOHnd; var buff; const Size: Int64): Boolean;
begin
  Result := umlFileRead(IOHnd, Size, buff);
end;

function umlFilePrepareWrite(var IOHnd: TIOHnd): Boolean;
begin
  Result := True;
  if umlFileTest(IOHnd) and IOHnd.Cache.UsedWriteCache and (IOHnd.Cache.PrepareWriteBuff = nil) then
      IOHnd.Cache.PrepareWriteBuff := TMS64.CustomCreate(1024 * 1024 * 8);
end;

function umlFileFlushWriteCache(var IOHnd: TIOHnd): Boolean;
var
  m64: TMS64;
begin
  if IOHnd.Cache.PrepareWriteBuff <> nil then
    begin
      m64 := TMS64(IOHnd.Cache.PrepareWriteBuff);
      IOHnd.Cache.PrepareWriteBuff := nil;
      if IOHnd.Handle.Write(m64.Memory^, m64.Size) <> m64.Size then
        begin
          IOHnd.Return := C_FileWriteError;
          Result := False;
          exit;
        end;
      inc(IOHnd.IOWrite, m64.Size);
      DisposeObject(m64);
      IOHnd.Handle.Position := IOHnd.Position;
    end;
  Result := True;
end;

function umlFileWrite(var IOHnd: TIOHnd; const Size: Int64; const buff): Boolean;
var
  BuffPointer: Pointer;
  i: NativeInt;
begin
  if (IOHnd.IsOnlyRead) or (not IOHnd.IsOpen) then
    begin
      IOHnd.Return := C_FileWriteError;
      Result := False;
      exit;
    end;
  if Size = 0 then
    begin
      IOHnd.Return := C_NotError;
      Result := True;
      exit;
    end;

  IOHnd.ChangeFromWrite := True;

  umlResetPrepareRead(IOHnd);

  if Size <= $F000 then
      umlFilePrepareWrite(IOHnd);

  if IOHnd.Cache.PrepareWriteBuff <> nil then
    begin
      if TMS64(IOHnd.Cache.PrepareWriteBuff).Write64(buff, Size) <> Size then
        begin
          IOHnd.Return := C_FileWriteError;
          Result := False;
          exit;
        end;

      inc(IOHnd.Position, Size);
      if IOHnd.Position > IOHnd.Size then
          IOHnd.Size := IOHnd.Position;
      IOHnd.Return := C_NotError;
      Result := True;

      // 8M flush buffer
      if IOHnd.Cache.PrepareWriteBuff.Size > 8 * 1024 * 1024 then
          umlFileFlushWriteCache(IOHnd);
      exit;
    end;

  try
    if Size > C_MaxBufferFragmentSize then
      begin
        // process buffer chunk
        BuffPointer := @buff;
        for i := 1 to (Size div C_MaxBufferFragmentSize) do
          begin
            if IOHnd.Handle.Write(BuffPointer^, C_MaxBufferFragmentSize) <> C_MaxBufferFragmentSize then
              begin
                IOHnd.Return := C_FileWriteError;
                Result := False;
                exit;
              end;
            BuffPointer := GetOffset(BuffPointer, C_MaxBufferFragmentSize);
          end;
        // process buffer rest
        i := Size mod C_MaxBufferFragmentSize;
        if IOHnd.Handle.Write(BuffPointer^, i) <> i then
          begin
            IOHnd.Return := C_FileWriteError;
            Result := False;
            exit;
          end;

        inc(IOHnd.Position, Size);
        if IOHnd.Position > IOHnd.Size then
            IOHnd.Size := IOHnd.Position;
        IOHnd.Return := C_NotError;
        Result := True;
        inc(IOHnd.IOWrite, Size);
        exit;
      end;
    if IOHnd.Handle.Write(buff, Size) <> Size then
      begin
        IOHnd.Return := C_FileWriteError;
        Result := False;
        exit;
      end;

    inc(IOHnd.Position, Size);
    if IOHnd.Position > IOHnd.Size then
        IOHnd.Size := IOHnd.Position;
    IOHnd.Return := C_NotError;
    Result := True;
    inc(IOHnd.IOWrite, Size);
  except
    IOHnd.Return := C_FileWriteError;
    Result := False;
  end;
end;

function umlBlockWrite(var IOHnd: TIOHnd; const buff; const Size: Int64): Boolean;
begin
  Result := umlFileWrite(IOHnd, Size, buff);
end;

function umlFileWriteFixedString(var IOHnd: TIOHnd; var Value: TPascalString): Boolean;
var
  buff: TBytes;
begin
  IOHnd.Pascal2FixedString(Value, buff);
  if umlFileWrite(IOHnd, IOHnd.FixedStringL, buff[0]) = False then
    begin
      IOHnd.Return := C_FileWriteError;
      Result := False;
      exit;
    end;

  IOHnd.Return := C_NotError;
  Result := True;
end;

function umlFileReadFixedString(var IOHnd: TIOHnd; var Value: TPascalString): Boolean;
var
  buff: TBytes;
begin
  try
    SetLength(buff, IOHnd.FixedStringL);
    if umlFileRead(IOHnd, IOHnd.FixedStringL, buff[0]) = False then
      begin
        IOHnd.Return := C_FileReadError;
        Result := False;
        exit;
      end;
    Value := IOHnd.FixedString2Pascal(buff);
    SetLength(buff, 0);
    IOHnd.Return := C_NotError;
    Result := True;
  except
    Value.text := '';
    IOHnd.Return := C_StringError;
    Result := False;
  end;
end;

function umlFileSeek(var IOHnd: TIOHnd; Pos_: Int64): Boolean;
begin
  if (Pos_ = IOHnd.Position) and (Pos_ = IOHnd.Handle.Position) then
    begin
      IOHnd.Return := C_NotError;
      Result := True;
      exit;
    end;

  if not umlFileFlushWriteCache(IOHnd) then
    begin
      Result := False;
      exit;
    end;

  IOHnd.Return := C_SeekError;
  Result := False;
  try
    IOHnd.Position := IOHnd.Handle.Seek(Pos_, TSeekOrigin.soBeginning);
    Result := IOHnd.Position <> -1;
    if Result then
        IOHnd.Return := C_NotError;
  except
  end;
end;

function umlFileGetPOS(var IOHnd: TIOHnd): Int64;
begin
  Result := IOHnd.Position;
end;

function umlFileSetSize(var IOHnd: TIOHnd; siz_: Int64): Boolean;
begin
  if not umlFileFlushWriteCache(IOHnd) then
    begin
      Result := False;
      exit;
    end;

  IOHnd.Handle.Size := siz_;
  Result := True;
  IOHnd.Return := C_NotError;
end;

function umlFilePOS(var IOHnd: TIOHnd): Int64;
begin
  Result := umlFileGetPOS(IOHnd);
end;

function umlFileGetSize(var IOHnd: TIOHnd): Int64;
begin
  Result := IOHnd.Size;
end;

function umlFileSize(var IOHnd: TIOHnd): Int64;
begin
  Result := umlFileGetSize(IOHnd);
end;

function umlGetFileTime(const FileName: TPascalString): TDateTime;
{$IFDEF MSWINDOWS}
  function CovFileDate_(Fd: TFileTime): TDateTime;
  var
    Tct: _SystemTime;
    t: TFileTime;
  begin
    FileTimeToLocalFileTime(Fd, t);
    FileTimeToSystemTime(t, Tct);
    CovFileDate_ := SystemTimeToDateTime(Tct);
  end;

var
  SR: TSR;
begin
  try
    if umlFindFirstFile(FileName, SR) then
        Result := CovFileDate_(SR.FindData.ftLastWriteTime)
    else
        Result := 0;
    umlFindClose(SR);
  except
      Result := 0;
  end;
end;
{$ELSE MSWINDOWS}


var
  f: THandle;
begin
  try
    f := FileOpen(FileName.text, fmOpenRead or fmShareDenyNone);
    if f <> THandle(-1) then
      begin
        Result := FileDateToDateTime(FileGetDate(f));
        FileClose(f);
      end
    else
        Result := 0;
  except
      Result := 0;
  end;
end;
{$ENDIF MSWINDOWS}


procedure umlSetFileTime(const FileName: TPascalString; newTime: TDateTime);
begin
  try
      FileSetDate(FileName.text, DateTimeToFileDate(newTime));
  except
  end;
end;

function umlGetFileSize(const FileName: TPascalString): Int64;
var
  SR: TSR;
begin
  Result := 0;
  try
    if umlFindFirstFile(FileName, SR) = True then
      begin
        Result := SR.Size;
        while umlFindNextFile(SR) do
            Result := Result + SR.Size;
      end;
    umlFindClose(SR);
  except
  end;
end;

function umlGetFileCount(const FileName: TPascalString): Integer;
var
  SR: TSR;
begin
  Result := 0;
  if umlFindFirstFile(FileName, SR) = True then
    begin
      Result := Result + 1;
      while umlFindNextFile(SR) = True do
          Result := Result + 1;
    end;
  umlFindClose(SR);
end;

function umlGetFileDateTime(const FileName: TPascalString): TDateTime;
begin
  if not FileAge(FileName.text, Result, False) then
      Result := Now;
end;

function umlDeleteFile(const FileName: TPascalString; const _VerifyCheck: Boolean): Boolean;
var
  _SR: TSR;
  ph: TPascalString;
begin
  if umlExistsChar(FileName, '*?') then
    begin
      ph := umlGetFilePath(FileName);
      if umlFindFirstFile(FileName, _SR) then
        begin
          repeat
            try
                DeleteFile(umlCombineFileName(ph, _SR.Name).text);
            except
            end;
          until not umlFindNextFile(_SR);
        end;
      umlFindClose(_SR);
      Result := True;
    end
  else
    begin
      try
          Result := DeleteFile(FileName.text);
      except
          Result := False;
      end;
      if Result and _VerifyCheck then
          Result := not umlFileExists(FileName)
      else
          Result := True;
    end;
end;

function umlDeleteFile(const FileName: TPascalString): Boolean;
begin
  Result := umlDeleteFile(FileName, False);
end;

function umlCopyFile(const SourFile, DestFile: TPascalString): Boolean;
var
  SH_, DH_: TCore_FileStream;
begin
  Result := False;
  SH_ := nil;
  DH_ := nil;
  try
    if not umlFileExists(SourFile) then
        exit;
    if umlMultipleMatch(True, ExpandFileName(SourFile.text), ExpandFileName(DestFile.text)) then
        exit;
    SH_ := TCore_FileStream.Create(SourFile.text, fmOpenRead or fmShareDenyNone);
    DH_ := TCore_FileStream.Create(DestFile.text, fmCreate);
    Result := DH_.CopyFrom(SH_, SH_.Size) = SH_.Size;
    DisposeObject(SH_);
    DisposeObject(DH_);
    umlSetFileTime(DestFile, umlGetFileTime(SourFile));
  except
    if SH_ <> nil then
        DisposeObject(SH_);
    if DH_ <> nil then
        DisposeObject(DH_);
  end;
end;

function umlRenameFile(const OldName, NewName: TPascalString): Boolean;
begin
  Result := RenameFile(OldName.text, NewName.text);
end;

procedure umlSetLength(var sVal: TPascalString; L: Integer);
begin
  sVal.L := L;
end;

procedure umlSetLength(var sVal: U_Bytes; L: Integer);
begin
  SetLength(sVal, L);
end;

procedure umlSetLength(var sVal: TArrayPascalString; L: Integer);
begin
  SetLength(sVal, L);
end;

function umlGetLength(const sVal: TPascalString): Integer;
begin
  Result := sVal.L;
end;

function umlGetLength(const sVal: U_Bytes): Integer;
begin
  Result := length(sVal);
end;

function umlGetLength(const sVal: TArrayPascalString): Integer;
begin
  Result := length(sVal);
end;

function umlUpperCase(const s: TPascalString): TPascalString;
begin
  Result := s.UpperText;
end;

function umlUpperCase(const s: PPascalString): TPascalString;
begin
  Result := s^.UpperText;
end;

function umlLowerCase(const s: TPascalString): TPascalString;
begin
  Result := s.LowerText;
end;

function umlLowerCase(const s: PPascalString): TPascalString;
begin
  Result := s^.LowerText;
end;

function umlCopyStr(const sVal: TPascalString; MainPosition, LastPosition: Integer): TPascalString;
begin
  Result := sVal.GetString(MainPosition, LastPosition);
end;

function umlSameText(const s1, s2: TPascalString): Boolean;
begin
  Result := s1.Same(@s2);
end;

function umlSameText(const s1, s2: PPascalString): Boolean;
begin
  Result := s1^.Same(s2);
end;

function umlDeleteChar(const SText, Ch: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  if SText.L > 0 then
    for i := 1 to SText.L do
      if not CharIn(SText[i], Ch) then
          Result.Append(SText[i]);
end;

function umlDeleteChar(const SText: TPascalString; const SomeChars: TArrayChar): TPascalString;
var
  i: Integer;
begin
  Result := '';
  if SText.L > 0 then
    for i := 1 to SText.L do
      if not CharIn(SText[i], SomeChars) then
          Result.Append(SText[i]);
end;

function umlDeleteChar(const SText: TPascalString; const SomeCharsets: TOrdChars): TPascalString; overload;
var
  i: Integer;
begin
  Result := '';
  if SText.L > 0 then
    for i := 1 to SText.L do
      if not CharIn(SText[i], SomeCharsets) then
          Result.Append(SText[i]);
end;

function umlGetNumberCharInText(const n: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  i := 0;
  if n.L = 0 then
      exit;

  while i <= n.L do
    begin
      if (not CharIn(n[i], c0to9)) then
        begin
          if (Result.L = 0) then
              inc(i)
          else
              exit;
        end
      else
        begin
          Result.Append(n[i]);
          inc(i);
        end;
    end;
end;

function umlMatchChar(CharValue: U_Char; cVal: PPascalString): Boolean;
begin
  Result := CharIn(CharValue, cVal);
end;

function umlMatchChar(CharValue: U_Char; cVal: TPascalString): Boolean;
begin
  Result := CharIn(CharValue, @cVal);
end;

function umlExistsChar(StrValue: TPascalString; cVal: TPascalString): Boolean;
var
  c: SystemChar;
begin
  Result := True;
  for c in StrValue.buff do
    if CharIn(c, @cVal) then
        exit;
  Result := False;
end;

function umlExistsChar(StrValue, cVal: PPascalString): Boolean;
var
  c: SystemChar;
begin
  Result := True;
  for c in StrValue^.buff do
    if CharIn(c, cVal) then
        exit;
  Result := False;
end;

function umlTrimChar(const s, trim_s: TPascalString): TPascalString;
var
  L, BP, EP: Integer;
begin
  Result := '';
  L := s.L;
  if L > 0 then
    begin
      BP := 1;
      while CharIn(s[BP], @trim_s) do
        begin
          inc(BP);
          if (BP > L) then
            begin
              Result := '';
              exit;
            end;
        end;
      if BP > L then
          Result := ''
      else
        begin
          EP := L;

          while CharIn(s[EP], @trim_s) do
            begin
              dec(EP);
              if (EP < 1) then
                begin
                  Result := '';
                  exit;
                end;
            end;
          Result := s.GetString(BP, EP + 1);
        end;
    end;
end;

function umlGetFirstStr(const sVal, trim_s: TPascalString): TPascalString;
var
  Next_Pos_, First_Pos_: Integer;
begin
  Result := sVal;
  if Result.L <= 0 then
    begin
      exit;
    end;
  First_Pos_ := 1;
  while umlMatchChar(Result[First_Pos_], @trim_s) do
    begin
      if First_Pos_ = Result.L then
        begin
          Result := '';
          exit;
        end;
      inc(First_Pos_);
    end;
  Next_Pos_ := First_Pos_;
  while not umlMatchChar(Result[First_Pos_], @trim_s) do
    begin
      if First_Pos_ = Result.L then
        begin
          Result := umlCopyStr(Result, Next_Pos_, First_Pos_ + 1);
          exit;
        end;
      inc(First_Pos_);
    end;
  Result := umlCopyStr(Result, Next_Pos_, First_Pos_);
end;

function umlGetLastStr(const sVal, trim_s: TPascalString): TPascalString;
var
  Prev_Pos_, Last_Pos_: Integer;
begin
  Result := sVal;
  Last_Pos_ := Result.L;
  if Last_Pos_ <= 0 then
    begin
      exit;
    end;
  while umlMatchChar(Result[Last_Pos_], @trim_s) do
    begin
      if Last_Pos_ = 1 then
        begin
          Result := '';
          exit;
        end;
      dec(Last_Pos_);
    end;
  Prev_Pos_ := Last_Pos_;
  while not umlMatchChar(Result[Last_Pos_], @trim_s) do
    begin
      if Last_Pos_ = 1 then
        begin
          Result := umlCopyStr(Result, Last_Pos_, Prev_Pos_ + 1);
          exit;
        end;
      dec(Last_Pos_);
    end;
  Result := umlCopyStr(Result, Last_Pos_ + 1, Prev_Pos_ + 1);
end;

function umlDeleteFirstStr(const sVal, trim_s: TPascalString): TPascalString;
var
  First_Pos_: Integer;
begin
  Result := sVal;
  if Result.L <= 0 then
    begin
      Result := '';
      exit;
    end;
  First_Pos_ := 1;
  while umlMatchChar(Result[First_Pos_], @trim_s) do
    begin
      if First_Pos_ = Result.L then
        begin
          Result := '';
          exit;
        end;
      inc(First_Pos_);
    end;
  while not umlMatchChar(Result[First_Pos_], @trim_s) do
    begin
      if First_Pos_ = Result.L then
        begin
          Result := '';
          exit;
        end;
      inc(First_Pos_);
    end;
  while umlMatchChar(Result[First_Pos_], @trim_s) do
    begin
      if First_Pos_ = Result.L then
        begin
          Result := '';
          exit;
        end;
      inc(First_Pos_);
    end;
  Result := umlCopyStr(Result, First_Pos_, Result.L + 1);
end;

function umlDeleteLastStr(const sVal, trim_s: TPascalString): TPascalString;
var
  Last_Pos_: Integer;
begin
  Result := sVal;
  Last_Pos_ := Result.L;
  if Last_Pos_ <= 0 then
    begin
      Result := '';
      exit;
    end;
  while umlMatchChar(Result[Last_Pos_], @trim_s) do
    begin
      if Last_Pos_ = 1 then
        begin
          Result := '';
          exit;
        end;
      dec(Last_Pos_);
    end;
  while not umlMatchChar(Result[Last_Pos_], @trim_s) do
    begin
      if Last_Pos_ = 1 then
        begin
          Result := '';
          exit;
        end;
      dec(Last_Pos_);
    end;
  while umlMatchChar(Result[Last_Pos_], @trim_s) do
    begin
      if Last_Pos_ = 1 then
        begin
          Result := '';
          exit;
        end;
      dec(Last_Pos_);
    end;
  umlSetLength(Result, Last_Pos_);
end;

function umlGetIndexStrCount(const sVal, trim_s: TPascalString): Integer;
var
  s: TPascalString;
  Pos_: Integer;
begin
  s := sVal;
  Result := 0;
  if s.L = 0 then
      exit;
  Pos_ := 1;
  while True do
    begin
      while umlMatchChar(s[Pos_], @trim_s) do
        begin
          if Pos_ >= s.L then
              exit;
          inc(Pos_);
        end;
      inc(Result);
      while not umlMatchChar(s[Pos_], @trim_s) do
        begin
          if Pos_ >= s.L then
              exit;
          inc(Pos_);
        end;
    end;
end;

function umlGetIndexStr(const sVal: TPascalString; trim_s: TPascalString; index: Integer): TPascalString;
var
  umlGetIndexName_Repeat: Integer;
begin
  case index of
    - 1:
      begin
        Result := '';
        exit;
      end;
    0, 1:
      begin
        Result := umlGetFirstStr(sVal, trim_s);
        exit;
      end;
  end;
  if index >= umlGetIndexStrCount(sVal, trim_s) then
    begin
      Result := umlGetLastStr(sVal, trim_s);
      exit;
    end;
  Result := sVal;
  for umlGetIndexName_Repeat := 2 to index do
    begin
      Result := umlDeleteFirstStr(Result, trim_s);
    end;
  Result := umlGetFirstStr(Result, trim_s);
end;

procedure umlGetSplitArray(const sour: TPascalString; var dest: TArrayPascalString; const splitC: TPascalString);
var
  i, idxCount: Integer;
  SText: TPascalString;
begin
  SText := sour;
  idxCount := umlGetIndexStrCount(SText, splitC);
  if (idxCount = 0) and (sour.L > 0) then
    begin
      SetLength(dest, 1);
      dest[0] := sour;
    end
  else
    begin
      SetLength(dest, idxCount);
      i := low(dest);
      while i < idxCount do
        begin
          dest[i] := umlGetFirstStr(SText, splitC);
          SText := umlDeleteFirstStr(SText, splitC);
          inc(i);
        end;
    end;
end;

procedure umlGetSplitArray(const sour: TPascalString; var dest: U_StringArray; const splitC: TPascalString);
var
  i, idxCount: Integer;
  SText: TPascalString;
begin
  SText := sour;
  idxCount := umlGetIndexStrCount(SText, splitC);
  if (idxCount = 0) and (sour.L > 0) then
    begin
      SetLength(dest, 1);
      dest[0] := sour;
    end
  else
    begin
      SetLength(dest, idxCount);
      i := low(dest);
      while i < idxCount do
        begin
          dest[i] := umlGetFirstStr(SText, splitC);
          SText := umlDeleteFirstStr(SText, splitC);
          inc(i);
        end;
    end;
end;

function ArrayStringToText(var ary: TArrayPascalString; const splitC: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := low(ary) to high(ary) do
    if i < high(ary) then
        Result := Result + ary[i] + splitC
    else
        Result := Result + ary[i];
end;

function umlStringsToSplitText(lst: TCore_Strings; const splitC: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to lst.Count - 1 do
    if i > 0 then
        Result.Append(splitC.text + lst[i])
    else
        Result := lst[i];
end;

function umlStringsToSplitText(lst: TListPascalString; const splitC: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to lst.Count - 1 do
    if i > 0 then
        Result.Append(splitC.text + lst[i])
    else
        Result := lst[i];
end;

function umlGetFirstStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
var
  Next_Pos_, First_Pos_: Integer;
begin
  Result := sVal;
  if Result.L <= 0 then
      exit;
  First_Pos_ := 1;
  if umlMatchChar(Result[First_Pos_], @trim_s) then
    begin
      inc(First_Pos_);
      Next_Pos_ := First_Pos_;
    end
  else
    begin
      Next_Pos_ := First_Pos_;
      while not umlMatchChar(Result[First_Pos_], @trim_s) do
        begin
          if First_Pos_ = Result.L then
            begin
              Result := umlCopyStr(Result, Next_Pos_, First_Pos_ + 1);
              exit;
            end;
          inc(First_Pos_);
        end;
    end;
  Result := umlCopyStr(Result, Next_Pos_, First_Pos_);
end;

function umlDeleteFirstStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
var
  First_Pos_: Integer;
begin
  Result := sVal;
  if Result.L <= 0 then
    begin
      Result := '';
      exit;
    end;
  First_Pos_ := 1;
  while not umlMatchChar(Result[First_Pos_], @trim_s) do
    begin
      if First_Pos_ = Result.L then
        begin
          Result := '';
          exit;
        end;
      inc(First_Pos_);
    end;
  if umlMatchChar(Result[First_Pos_], @trim_s) then
      inc(First_Pos_);
  Result := umlCopyStr(Result, First_Pos_, Result.L + 1);
end;

function umlGetLastStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
var
  Prev_Pos_, Last_Pos_: Integer;
begin
  Result := sVal;
  Last_Pos_ := Result.L;
  if Last_Pos_ <= 0 then
      exit;
  if Result[Last_Pos_] = trim_s then
      dec(Last_Pos_);
  Prev_Pos_ := Last_Pos_;
  while not umlMatchChar(Result[Last_Pos_], @trim_s) do
    begin
      if Last_Pos_ = 1 then
        begin
          Result := umlCopyStr(Result, Last_Pos_, Prev_Pos_ + 1);
          exit;
        end;
      dec(Last_Pos_);
    end;
  Result := umlCopyStr(Result, Last_Pos_ + 1, Prev_Pos_ + 1);
end;

function umlDeleteLastStr_Discontinuity(const sVal, trim_s: TPascalString): TPascalString;
var
  Last_Pos_: Integer;
begin
  Result := sVal;
  Last_Pos_ := Result.L;
  if Last_Pos_ <= 0 then
    begin
      Result := '';
      exit;
    end;
  if umlMatchChar(Result[Last_Pos_], @trim_s) then
      dec(Last_Pos_);
  while not umlMatchChar(Result[Last_Pos_], @trim_s) do
    begin
      if Last_Pos_ = 1 then
        begin
          Result := '';
          exit;
        end;
      dec(Last_Pos_);
    end;
  umlSetLength(Result, Last_Pos_);
end;

function umlGetIndexStrCount_Discontinuity(const sVal, trim_s: TPascalString): Integer;
var
  s: TPascalString;
  Pos_: Integer;
begin
  s := sVal;
  Result := 0;
  if s.L = 0 then
      exit;
  Pos_ := 1;
  Result := 1;
  while True do
    begin
      while not umlMatchChar(s[Pos_], @trim_s) do
        begin
          if Pos_ = s.L then
              exit;
          inc(Pos_);
        end;
      inc(Result);
      if Pos_ = s.L then
          exit;
      inc(Pos_);
    end;
end;

function umlGetIndexStr_Discontinuity(const sVal: TPascalString; trim_s: TPascalString; index: Integer): TPascalString;
var
  umlGetIndexName_Repeat: Integer;
begin
  case index of
    - 1:
      begin
        Result := '';
        exit;
      end;
    0, 1:
      begin
        Result := umlGetFirstStr_Discontinuity(sVal, trim_s);
        exit;
      end;
  end;
  if index >= umlGetIndexStrCount_Discontinuity(sVal, trim_s) then
    begin
      Result := umlGetLastStr_Discontinuity(sVal, trim_s);
      exit;
    end;
  Result := sVal;
  for umlGetIndexName_Repeat := 2 to index do
      Result := umlDeleteFirstStr_Discontinuity(Result, trim_s);
  Result := umlGetFirstStr_Discontinuity(Result, trim_s);
end;

function umlGetFirstTextPos(const s: TPascalString; const TextArry: TArrayPascalString; var OutText: TPascalString): Integer;
var
  i, j: Integer;
begin
  Result := -1;
  for i := 1 to s.L do
    begin
      for j := low(TextArry) to high(TextArry) do
        begin
          if s.ComparePos(i, @TextArry[j]) then
            begin
              OutText := TextArry[j];
              Result := i;
              exit;
            end;
        end;
    end;
end;

function umlDeleteText(const sour: TPascalString; const bToken, eToken: TArrayPascalString; ANeedBegin, ANeedEnd: Boolean): TPascalString;
var
  ABeginPos, AEndPos: Integer;
  ABeginText, AEndText, ANewStr: TPascalString;
begin
  Result := sour;
  if sour.L > 0 then
    begin
      ABeginPos := umlGetFirstTextPos(sour, bToken, ABeginText);
      if ABeginPos > 0 then
          ANewStr := umlCopyStr(sour, ABeginPos + ABeginText.L, sour.L + 1)
      else if ANeedBegin then
          exit
      else
          ANewStr := sour;

      AEndPos := umlGetFirstTextPos(ANewStr, eToken, AEndText);
      if AEndPos > 0 then
          ANewStr := umlCopyStr(ANewStr, (AEndPos + AEndText.L), ANewStr.L + 1)
      else if ANeedEnd then
          exit
      else
          ANewStr := '';

      if ABeginPos > 0 then
        begin
          if AEndPos > 0 then
              Result := umlCopyStr(sour, 0, ABeginPos - 1) + umlDeleteText(ANewStr, bToken, eToken, ANeedBegin, ANeedEnd)
          else
              Result := umlCopyStr(sour, 0, ABeginPos - 1) + ANewStr;
        end
      else if AEndPos > 0 then
          Result := ANewStr;
    end;
end;

function umlGetTextContent(const sour: TPascalString; const bToken, eToken: TArrayPascalString): TPascalString;
var
  ABeginPos, AEndPos: Integer;
  ABeginText, AEndText, ANewStr: TPascalString;
begin
  Result := '';
  if sour.L > 0 then
    begin
      ABeginPos := umlGetFirstTextPos(sour, bToken, ABeginText);
      if ABeginPos > 0 then
          ANewStr := umlCopyStr(sour, ABeginPos + ABeginText.L, sour.L + 1)
      else
          ANewStr := sour;

      AEndPos := umlGetFirstTextPos(ANewStr, eToken, AEndText);
      if AEndPos > 0 then
          Result := umlCopyStr(ANewStr, 0, AEndPos - 1)
      else
          Result := ANewStr;
    end;
end;

function umlGetNumTextType(const s: TPascalString): TTextType;
type
  TValSym = (vsSymSub, vsSymAdd, vsSymAddSub, vsSymDollar, vsDot, vsDotBeforNum, vsDotAfterNum, vsNum, vsAtoF, vsE, vsUnknow);
var
  cnt: array [TValSym] of Integer;
  n: TPascalString;
  v: TValSym;
  c: SystemChar;
  i: Integer;
begin
  n := umlTrimSpace(s);
  if n.Same('true') or n.Same('false') then
      exit(ntBool);

  for v := low(TValSym) to high(TValSym) do
      cnt[v] := 0;

  for i := 1 to n.L do
    begin
      c := n[i];
      if CharIn(c, [c0to9]) then
        begin
          inc(cnt[vsNum]);
          if cnt[vsDot] > 0 then
              inc(cnt[vsDotAfterNum]);
        end
      else if CharIn(c, [cLoAtoF, cHiAtoF]) then
        begin
          inc(cnt[vsAtoF]);
          if CharIn(c, 'eE') then
              inc(cnt[vsE]);
        end
      else if c = '.' then
        begin
          inc(cnt[vsDot]);
          cnt[vsDotBeforNum] := cnt[vsNum];
        end
      else if CharIn(c, '-') then
        begin
          inc(cnt[vsSymSub]);
          inc(cnt[vsSymAddSub]);
        end
      else if CharIn(c, '+') then
        begin
          inc(cnt[vsSymAdd]);
          inc(cnt[vsSymAddSub]);
        end
      else if CharIn(c, '$') and (i = 1) then
        begin
          inc(cnt[vsSymDollar]);
          if i <> 1 then
              exit(ntUnknow);
        end
      else
          exit(ntUnknow);
    end;

  if cnt[vsDot] > 1 then
      exit(ntUnknow);
  if cnt[vsSymDollar] > 1 then
      exit(ntUnknow);
  if (cnt[vsSymDollar] = 0) and (cnt[vsNum] = 0) then
      exit(ntUnknow);
  if (cnt[vsSymAdd] > 1) and (cnt[vsE] = 0) and (cnt[vsSymDollar] = 0) then
      exit(ntUnknow);

  if (cnt[vsSymDollar] = 0) and
    ((cnt[vsDot] = 1) or ((cnt[vsE] = 1) and ((cnt[vsSymAddSub] >= 1) and (cnt[vsSymDollar] = 0)))) then
    begin
      if cnt[vsSymDollar] > 0 then
          exit(ntUnknow);
      if (cnt[vsAtoF] <> cnt[vsE]) then
          exit(ntUnknow);

      if cnt[vsE] = 1 then
        begin
          Result := ntDouble
        end
      else if ((cnt[vsDotBeforNum] > 0)) and (cnt[vsDotAfterNum] > 0) then
        begin
          if cnt[vsDotAfterNum] < 5 then
              Result := ntCurrency
          else if cnt[vsNum] > 7 then
              Result := ntDouble
          else
              Result := ntSingle;
        end
      else
          exit(ntUnknow);
    end
  else
    begin
      if cnt[vsSymDollar] = 1 then
        begin
          if cnt[vsSymSub] > 0 then
            begin
              if cnt[vsNum] + cnt[vsAtoF] = 0 then
                  Result := ntUnknow
              else if cnt[vsNum] + cnt[vsAtoF] < 2 then
                  Result := ntShortInt
              else if cnt[vsNum] + cnt[vsAtoF] < 4 then
                  Result := ntSmallInt
              else if cnt[vsNum] + cnt[vsAtoF] < 7 then
                  Result := ntInt
              else if cnt[vsNum] + cnt[vsAtoF] < 13 then
                  Result := ntInt64
              else
                  Result := ntUnknow;
            end
          else
            begin
              if cnt[vsNum] + cnt[vsAtoF] = 0 then
                  Result := ntUnknow
              else if cnt[vsNum] + cnt[vsAtoF] < 3 then
                  Result := ntByte
              else if cnt[vsNum] + cnt[vsAtoF] < 5 then
                  Result := ntWord
              else if cnt[vsNum] + cnt[vsAtoF] < 8 then
                  Result := ntUInt
              else if cnt[vsNum] + cnt[vsAtoF] < 14 then
                  Result := ntUInt64
              else
                  Result := ntUnknow;
            end;
        end
      else if cnt[vsAtoF] > 0 then
          exit(ntUnknow)
      else if cnt[vsSymSub] > 0 then
        begin
          if cnt[vsNum] = 0 then
              Result := ntUnknow
          else if cnt[vsNum] < 3 then
              Result := ntShortInt
          else if cnt[vsNum] < 5 then
              Result := ntSmallInt
          else if cnt[vsNum] < 8 then
              Result := ntInt
          else if cnt[vsNum] < 15 then
              Result := ntInt64
          else
              Result := ntUnknow;
        end
      else
        begin
          if cnt[vsNum] = 0 then
              Result := ntUnknow
          else if cnt[vsNum] < 3 then
              Result := ntByte
          else if cnt[vsNum] < 5 then
              Result := ntWord
          else if cnt[vsNum] < 8 then
              Result := ntUInt
          else if cnt[vsNum] < 16 then
              Result := ntUInt64
          else
              Result := ntUnknow;
        end;
    end;
end;

function umlIsHex(const sVal: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(sVal) in
    [ntInt, ntInt64, ntUInt64, ntWord, ntByte, ntSmallInt, ntShortInt, ntUInt];
end;

function umlIsNumber(const sVal: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(sVal) <> ntUnknow;
end;

function umlIsIntNumber(const sVal: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(sVal) in
    [ntInt, ntInt64, ntUInt64, ntWord, ntByte, ntSmallInt, ntShortInt, ntUInt];
end;

function umlIsFloatNumber(const sVal: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(sVal) in [ntSingle, ntDouble, ntCurrency];
end;

function umlIsBool(const sVal: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(sVal) = ntBool;
end;

function umlNumberCount(const sVal: TPascalString): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to sVal.L do
    if CharIn(sVal[i], [c0to9]) then
        inc(Result);
end;

function umlPercentageToFloat(OriginMax, OriginMin, ProcressParameter: Double): Double;
begin
  Result := (ProcressParameter - OriginMin) * 100.0 / (OriginMax - OriginMin);
end;

function umlPercentageToInt64(OriginParameter, ProcressParameter: Int64): Integer;
begin
  if OriginParameter = 0 then
      Result := 0
  else
      Result := Round((ProcressParameter * 100.0) / OriginParameter);
end;

function umlPercentageToInt(OriginParameter, ProcressParameter: Integer): Integer;
begin
  if OriginParameter = 0 then
      Result := 0
  else
      Result := Round((ProcressParameter * 100.0) / OriginParameter);
end;

function umlPercentageToStr(OriginParameter, ProcressParameter: Integer): TPascalString;
begin
  Result := IntToStr(umlPercentageToInt(OriginParameter, ProcressParameter)) + '%';
end;

function umlSmartSizeToStr(Size: Int64): TPascalString;
begin
  if Size < 1 shl 10 then
      Result := Format('%d', [Size])
  else if Size < 1 shl 20 then
      Result := Format('%f Kb', [Size / (1 shl 10)])
  else
      Result := Format('%f M', [Size / (1 shl 20)]);
end;

function umlIntToStr(Parameter: Single): TPascalString;
begin
  Result := IntToStr(Round(Parameter));
end;

function umlIntToStr(Parameter: Double): TPascalString;
begin
  Result := IntToStr(Round(Parameter));
end;

function umlIntToStr(Parameter: Int64): TPascalString;
begin
  Result := IntToStr(Parameter);
end;

function umlIntToStr(Parameter: UInt64): TPascalString;
begin
  Result := UIntToStr(Parameter);
end;

function umlPointerToStr(param: Pointer): TPascalString;
begin
  Result := '0x' + IntToHex(NativeUInt(param), SizeOf(Pointer) * 2);
end;

function umlMBPSToStr(Size: Int64): TPascalString;
begin
  if Size < 1 shl 10 then
      Result := Format('%d bps', [Size * 10])
  else if Size < 1 shl 20 then
      Result := Format('%f Kbps', [Size / (1 shl 10) * 10])
  else
      Result := Format('%f Mbps', [Size / (1 shl 20) * 10]);
end;

function umlSizeToStr(Parameter: Int64): TPascalString;
begin
  try
      Result := umlSmartSizeToStr(Parameter);
  except
      Result := IntToStr(Parameter) + ' B';
  end;
end;

function umlStrToDateTime(s: TPascalString): TDateTime;
begin
  Result := StrToDateTime(s.text, Lib_DateTimeFormatSettings);
end;

function umlDateTimeToStr(t: TDateTime): TPascalString;
begin
  Result.text := DateTimeToStr(t, Lib_DateTimeFormatSettings);
end;

function umlTimeTickToStr(const t: TTimeTick): TPascalString;
var
  tmp, d, h, m, s: TTimeTick;
begin
{$IFDEF FPC}
  d := t div C_Tick_Day;
  tmp := t mod C_Tick_Day;

  h := tmp div C_Tick_Hour;
  tmp := t mod C_Tick_Hour;

  m := tmp div C_Tick_Minute;
  tmp := t mod C_Tick_Minute;

  s := tmp div C_Tick_Second;
  tmp := t mod C_Tick_Second;
{$ELSE FPC}
  DivMod(t, C_Tick_Day, d, tmp);
  DivMod(tmp, C_Tick_Hour, h, tmp);
  DivMod(tmp, C_Tick_Minute, m, tmp);
  DivMod(tmp, C_Tick_Second, s, tmp);
{$ENDIF FPC}
  Result := '';
  if (d > 0) then
      Result.Append(IntToStr(d) + ' day ');
  if (Result.L > 0) or (h > 0) then
      Result.Append(IntToStr(h) + ' hour ');
  if (Result.L > 0) or (m > 0) then
      Result.Append(IntToStr(m) + ' minute ');

  if (Result.L > 0) or (s > 0) then
      Result.Append(PFormat('%2.2f', [s + tmp / 1000]))
  else
      Result.Append('0');
end;

function umlTimeToStr(t: TDateTime): TPascalString;
begin
  Result := TimeToStr(t, Lib_DateTimeFormatSettings);
end;

function umlDateToStr(t: TDateTime): TPascalString;
begin
  Result := DateToStr(t, Lib_DateTimeFormatSettings);
end;

function umlFloatToStr(const f: Double): TPascalString;
begin
  Result := FloatToStr(f);
end;

function umlShortFloatToStr(const f: Double): TPascalString;
begin
  Result := Format('%f', [f]);
end;

function umlStrToInt(const V_: TPascalString): Integer;
begin
  Result := umlStrToInt(V_, 0);
end;

function umlStrToInt(const V_: TPascalString; _Def: Integer): Integer;
begin
  if umlIsNumber(V_) then
    begin
      try
          Result := StrToInt(V_.text);
      except
          Result := _Def;
      end;
    end
  else
      Result := _Def;
end;

function umlStrToInt64(const V_: TPascalString; _Def: Int64): Int64;
begin
  if umlIsNumber(V_) then
    begin
      try
          Result := StrToInt64(V_.text);
      except
          Result := _Def;
      end;
    end
  else
      Result := _Def;
end;

function umlStrToInt64(const V_: TPascalString): Int64;
begin
  Result := umlStrToInt64(V_, 0);
end;

function umlStrToFloat(const V_: TPascalString; _Def: Double): Double;
begin
  if umlIsNumber(V_) then
    begin
      try
          Result := StrToFloat(V_.text);
      except
          Result := _Def;
      end;
    end
  else
      Result := _Def;
end;

function umlStrToFloat(const V_: TPascalString): Double;
begin
  Result := umlStrToFloat(V_, 0);
end;

function umlMultipleMatch(IgnoreCase: Boolean; const source, target, Multiple_, Character_: TPascalString): Boolean;
label C_Proc_, MC_Proc_, MS_Proc_;
var
  uS, uT, swap_S: TPascalString;
  sC, tC, swap_C: U_Char;
  sI, tI, swap_I, sL, tL, swap_L: Integer;
begin
  sL := source.L;
  if sL = 0 then
    begin
      Result := True;
      exit;
    end;

  tL := target.L;
  if tL = 0 then
    begin
      Result := False;
      exit;
    end;

  if IgnoreCase then
    begin
      uS := source.UpperText;
      uT := target.UpperText;
    end
  else
    begin
      uS := source;
      uT := target;
    end;

  if (not umlExistsChar(@source, @Character_)) and (not umlExistsChar(@source, @Multiple_)) then
    begin
      Result := (sL = tL) and (uS = uT);
      exit;
    end;
  if sL = 1 then
    begin
      if umlMatchChar(uS[1], @Multiple_) then
          Result := True
      else
          Result := False;
      exit;
    end;
  sI := 1;
  tI := 1;
  sC := uS[sI];
  tC := uT[tI];

C_Proc_:
  while (sC = tC) and (not umlMatchChar(sC, @Character_)) and (not umlMatchChar(sC, @Multiple_)) do
    begin
      if sI = sL then
        begin
          if tI = tL then
            begin
              Result := True;
              exit;
            end;
          Result := False;
          exit;
        end;
      if tI = tL then
        begin
          inc(sI);
          if sI = sL then
            begin
              sC := uS[sI];
              Result := umlMatchChar(sC, @Multiple_) or umlMatchChar(sC, @Character_);
              exit;
            end;
          Result := False;
          exit;
        end;
      inc(sI);
      inc(tI);
      sC := uS[sI];
      tC := uT[tI];
    end;

MC_Proc_:
  while umlMatchChar(sC, @Character_) do
    begin
      if sI = sL then
        begin
          if tI = tL then
            begin
              Result := True;
              exit;
            end;
          Result := False;
          exit;
        end;
      if tI = tL then
        begin
          inc(sI);
          sC := uS[sI];
          if (sI = sL) and ((umlMatchChar(sC, @Multiple_)) or (umlMatchChar(sC, @Character_))) then
            begin
              Result := True;
              exit;
            end;
          Result := False;
          exit;
        end;
      inc(sI);
      inc(tI);
      sC := uS[sI];
      tC := uT[tI];
    end;

MS_Proc_:
  if umlMatchChar(sC, @Multiple_) then
    begin
      if sI = sL then
        begin
          Result := True;
          exit;
        end;
      inc(sI);
      sC := uS[sI];

      while (umlMatchChar(sC, @Multiple_)) or (umlMatchChar(sC, @Character_)) do
        begin
          if sI = sL then
            begin
              Result := True;
              exit;
            end;
          inc(sI);
          sC := uS[sI];
          while umlMatchChar(sC, @Character_) do
            begin
              if sI = sL then
                begin
                  Result := True;
                  exit;
                end;
              inc(sI);
              sC := uS[sI];
            end;
        end;
      swap_S := umlCopyStr(uS, sI, sL + 1);
      swap_L := swap_S.L;
      if swap_L = 0 then
        begin
          Result := (uS[sI] = Multiple_);
          exit;
        end;
      swap_I := 1;
      swap_C := swap_S[swap_I];
      while (not umlMatchChar(swap_C, @Character_)) and (not umlMatchChar(swap_C, @Multiple_)) and (swap_I < swap_L) do
        begin
          inc(swap_I);
          swap_C := swap_S[swap_I];
        end;
      if (umlMatchChar(swap_C, @Character_)) or (umlMatchChar(swap_C, @Multiple_)) then
          swap_S := umlCopyStr(swap_S, 1, swap_I)
      else
        begin
          swap_S := umlCopyStr(swap_S, 1, swap_I + 1);
          if swap_S = '' then
            begin
              Result := False;
              exit;
            end;
          swap_L := swap_S.L;
          swap_I := 1;
          swap_C := swap_S[swap_L];
          tC := uT[tL];
          while swap_C = tC do
            begin
              if swap_I = swap_L then
                begin
                  Result := True;
                  exit;
                end;
              if swap_I = tL then
                begin
                  Result := False;
                  exit;
                end;
              swap_C := swap_S[(swap_L) - swap_I];
              tC := uT[(tL) - swap_I];
              inc(swap_I);
            end;
          Result := False;
          exit;
        end;
      swap_C := swap_S[1];
      swap_I := 1;
      swap_L := swap_S.L;
      while swap_I <= swap_L do
        begin
          if (tI - 1) + swap_I > tL then
            begin
              Result := False;
              exit;
            end;
          swap_C := swap_S[swap_I];
          tC := uT[(tI - 1) + swap_I];
          while swap_C <> tC do
            begin
              if (tI + swap_L) > tL then
                begin
                  Result := False;
                  exit;
                end;
              inc(tI);
              swap_I := 1;
              swap_C := swap_S[swap_I];
              tC := uT[(tI - 1) + swap_I];
            end;
          inc(swap_I);
        end;
      tI := (tI - 1) + swap_L;
      sI := (sI - 1) + swap_L;
      tC := swap_C;
      sC := swap_C;
    end;

  if sC = tC then
      goto C_Proc_
  else if umlMatchChar(sC, @Character_) then
      goto MC_Proc_
  else if umlMatchChar(sC, @Multiple_) then
      goto MS_Proc_
  else
      Result := False;
end;

function umlMultipleMatch(IgnoreCase: Boolean; const source, target: TPascalString): Boolean;
begin
  if (source.L > 0) and (source.text <> '*') then
      Result := umlMultipleMatch(IgnoreCase, source, target, '*', '?')
  else
      Result := True;
end;

function umlMultipleMatch(const source, target: TPascalString): Boolean;
var
  fi: TArrayPascalString;
begin
  if (source.L > 0) and (source.text <> '*') then
    begin
      umlGetSplitArray(source, fi, ';');
      Result := umlMultipleMatch(fi, target);
    end
  else
      Result := True;
end;

function umlMultipleMatch(const source: array of TPascalString; const target: TPascalString): Boolean;
var
  i: Integer;
begin
  Result := False;
  if target.L > 0 then
    begin
      if high(source) >= 0 then
        begin
          Result := False;
          for i := low(source) to high(source) do
            begin
              Result := umlMultipleMatch(True, source[i], target);
              if Result then
                  exit;
            end;
        end
      else
          Result := True;
    end;
end;

function umlSearchMatch(const source, target: TPascalString): Boolean;
var
  fi: TArrayPascalString;
begin
  if (source.L > 0) and (source.text <> '*') then
    begin
      umlGetSplitArray(source, fi, ';,');
      Result := umlSearchMatch(fi, target);
    end
  else
      Result := True;
end;

function umlSearchMatch(const source: TArrayPascalString; target: TPascalString): Boolean;
var
  i: Integer;
begin
  Result := False;
  if target.L > 0 then
    begin
      if high(source) >= 0 then
        begin
          Result := False;
          for i := low(source) to high(source) do
            begin
              Result := (target.GetPos(source[i]) > 0) or (umlMultipleMatch(True, source[i], target));
              if Result then
                  exit;
            end;
        end
      else
          Result := True;
    end;
end;

function umlMatchFileInfo(const exp_, sour_, dest_: TPascalString): Boolean;
const
  prefix = '<prefix>';
  postfix = '<postfix>';
var
  sour, dest, dest_prefix, dest_postfix, n: TPascalString;
begin
  sour := umlGetFileName(sour_);
  dest := umlGetFileName(dest_);
  dest_prefix := umlChangeFileExt(dest, '');
  dest_postfix := umlGetFileExt(dest);
  n := umlStringReplace(exp_, prefix, dest_prefix, True);
  n := umlStringReplace(n, postfix, dest_postfix, True);
  Result := umlMultipleMatch(n, sour);
  sour := '';
  dest := '';
  dest_prefix := '';
  dest_postfix := '';
  n := '';
end;

function umlGetDateTimeStr(NowDateTime: TDateTime): TPascalString;
var
  Year, Month, Day: Word;
  Hour, min_, Sec, MSec: Word;
begin
  DecodeDate(NowDateTime, Year, Month, Day);
  DecodeTime(NowDateTime, Hour, min_, Sec, MSec);
  Result := IntToStr(Year) + '-' + IntToStr(Month) + '-' + IntToStr(Day) + ' ' + IntToStr(Hour) + '-' + IntToStr(min_) + '-' + IntToStr(Sec) + '-' + IntToStr(MSec);
end;

function umlDecodeTimeToStr(NowDateTime: TDateTime): TPascalString;
var
  Year, Month, Day: Word;
  Hour, min_, Sec, MSec: Word;
begin
  DecodeDate(NowDateTime, Year, Month, Day);
  DecodeTime(NowDateTime, Hour, min_, Sec, MSec);
  Result := IntToHex(Year, 4) + IntToHex(Month, 2) +
    IntToHex(Day, 2) + IntToHex(Hour, 1) + IntToHex(min_, 2) +
    IntToHex(Sec, 2) + IntToHex(MSec, 3);
end;

function umlMakeRanName: TPascalString;
type
  TRanData = packed record
    Year, Month, Day: Word;
    Hour, min_, Sec, MSec: Word;
  end;
var
  d: TDateTime;
  r: TRanData;
begin
  d := umlNow();
  with r do
    begin
      DecodeDate(d, Year, Month, Day);
      DecodeTime(d, Hour, min_, Sec, MSec);
    end;
  Result := umlMD5String(@r, SizeOf(TRanData));
end;

function umlBuildBatch(L: THashStringList): TArrayBatch;
var
  arry: TArrayBatch;
  i: Integer;
  p: PHashListData;
begin
  SetLength(arry, L.Count);
  if L.HashList.Count > 0 then
    begin
      i := 0;
      p := L.HashList.FirstPtr;
      while i < L.HashList.Count do
        begin
          arry[i].sour := p^.OriginName;
          arry[i].dest := PHashStringListData(p^.Data)^.v;
          inc(i);
          p := p^.Next;
        end;
    end;
  Result := arry;
end;

function umlBuildBatch(L: THashVariantList): TArrayBatch;
var
  arry: TArrayBatch;
  i: Integer;
  p: PHashListData;
begin
  SetLength(arry, L.Count);
  if L.HashList.Count > 0 then
    begin
      i := 0;
      p := L.HashList.FirstPtr;
      while i < L.HashList.Count do
        begin
          arry[i].sour := p^.OriginName;
          arry[i].dest := VarToStr(PHashVariantListData(p^.Data)^.v);
          inc(i);
          p := p^.Next;
        end;
    end;
  Result := arry;
end;

procedure umlClearBatch(var arry: TArrayBatch);
var
  i: Integer;
begin
  for i := low(arry) to high(arry) do
    begin
      arry[i].sour := '';
      arry[i].dest := '';
    end;
  SetLength(arry, 0);
end;

procedure umlSortBatch(var arry: TArrayBatch);

  function CompareInt_(const i1, i2: Integer): ShortInt;
  begin
    if i1 = i2 then
        Result := 0
    else if i1 < i2 then
        Result := -1
    else
        Result := 1;
  end;

  function Compare_(var Left, Right: TBatch): ShortInt;
  begin
    Result := CompareInt_(Right.sour.L, Left.sour.L);
  end;

  procedure fastSort_(L, r: Integer);
  var
    i, j: Integer;
    p: TBatch;
  begin
    repeat
      i := L;
      j := r;
      p := arry[(L + r) shr 1];
      repeat
        while Compare_(arry[i], p) < 0 do
            inc(i);
        while Compare_(arry[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                arry[i].sour.SwapInstance(arry[j].sour);
                arry[i].dest.SwapInstance(arry[j].dest);
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          fastSort_(L, j);
      L := i;
    until i >= r;
  end;

begin
  if length(arry) > 1 then
      fastSort_(0, length(arry) - 1);
end;

function umlCharIsSymbol(c: SystemChar): Boolean;
begin
  Result := CharIn(c,
    [#13, #10, #9, #32, #46, #44, #43, #45, #42, #47, #40, #41, #59, #58, #61, #35, #64, #94,
    #38, #37, #33, #34, #91, #93, #60, #62, #63, #123, #125, #39, #36, #124]);
end;

function umlCharIsSymbol(c: SystemChar; const CustomSymbol_: TArrayChar): Boolean;
begin
  Result := CharIn(c, CustomSymbol_);
end;

function umlIsWord(p: PPascalString; bPos, ePos: Integer): Boolean;
begin
  if (bPos > ePos) or (bPos < 1) or (ePos > p^.L) then
      Result := False
  else if bPos = 1 then
    begin
      if ePos = p^.L then
          Result := True
      else
          Result := umlCharIsSymbol(p^[ePos + 1]);
    end
  else if ePos = p^.L then
      Result := umlCharIsSymbol(p^[bPos - 1])
  else
      Result := umlCharIsSymbol(p^[bPos - 1]) and umlCharIsSymbol(p^[ePos + 1]);
end;

function umlIsWord(s: TPascalString; bPos, ePos: Integer): Boolean;
begin
  Result := umlIsWord(@s, bPos, ePos);
end;

function umlExtractWord(s: TPascalString): TArrayPascalString;
var
  i, bPos, ePos, j: Integer;
begin
  SetLength(Result, 0);
  if s.L = 0 then
      exit;

  // compute buff size
  j := 0;
  i := 1;
  while i <= s.L do
    begin
      bPos := i;
      while bPos <= s.L do
        if umlCharIsSymbol(s[bPos]) then
            inc(bPos)
        else
            break;

      ePos := bPos;
      while ePos <= s.L do
        if not umlCharIsSymbol(s[ePos]) then
            inc(ePos)
        else
            break;

      if ePos > bPos then
          inc(j);
      i := ePos;
    end;

  if j = 0 then
      exit;

  // fill buff
  SetLength(Result, j);
  j := 0;
  i := 1;
  while i <= s.L do
    begin
      bPos := i;
      while bPos <= s.L do
        if umlCharIsSymbol(s[bPos]) then
            inc(bPos)
        else
            break;

      ePos := bPos;
      while ePos <= s.L do
        if not umlCharIsSymbol(s[ePos]) then
            inc(ePos)
        else
            break;

      if ePos > bPos then
        begin
          Result[j] := s.GetString(bPos, ePos);
          inc(j);
        end;
      i := ePos;
    end;
end;

function umlExtractWord(s: TPascalString; const CustomSymbol_: TArrayChar): TArrayPascalString;
var
  i, bPos, ePos, j: Integer;
begin
  SetLength(Result, 0);
  if s.L = 0 then
      exit;

  // compute buff size
  j := 0;
  i := 1;
  while i <= s.L do
    begin
      bPos := i;
      while bPos <= s.L do
        if umlCharIsSymbol(s[bPos], CustomSymbol_) then
            inc(bPos)
        else
            break;

      ePos := bPos;
      while ePos <= s.L do
        if not umlCharIsSymbol(s[ePos], CustomSymbol_) then
            inc(ePos)
        else
            break;

      if ePos > bPos then
          inc(j);
      i := ePos;
    end;

  if j = 0 then
      exit;

  // fill buff
  SetLength(Result, j);
  j := 0;
  i := 1;
  while i <= s.L do
    begin
      bPos := i;
      while bPos <= s.L do
        if umlCharIsSymbol(s[bPos], CustomSymbol_) then
            inc(bPos)
        else
            break;

      ePos := bPos;
      while ePos <= s.L do
        if not umlCharIsSymbol(s[ePos], CustomSymbol_) then
            inc(ePos)
        else
            break;

      if ePos > bPos then
        begin
          Result[j] := s.GetString(bPos, ePos);
          inc(j);
        end;
      i := ePos;
    end;
end;

function umlBatchSum(p: PPascalString; var arry: TArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList): Integer;
  function Match_(Pos_: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := Low(arry) to high(arry) do
      if (arry[i].sour.L > 0) and ((not OnlyWord) or umlIsWord(p, Pos_, Pos_ + arry[i].sour.L - 1))
        and p^.ComparePos(Pos_, @arry[i].sour, IgnoreCase) then
          exit(i);
  end;

var
  i, r, BP, EP: Integer;
  found_: Boolean;
  BatchInfo: TBatchInfo;
begin
  Result := 0;
  if p^.L = 0 then
      exit;

  if (ePos <= 0) or (ePos > p^.L) then
      EP := p^.L
  else
      EP := ePos;

  if bPos < 1 then
      BP := 1
  else if bPos > EP then
      BP := EP
  else
      BP := bPos;

  for i := low(arry) to high(arry) do
      arry[i].sum := 0;

  i := 1;
  while i <= p^.L do
    begin
      found_ := False;
      if (i >= BP) and (i <= EP) then
        begin
          r := Match_(i);
          found_ := r >= 0;
          if found_ then
            begin
              if Info <> nil then
                begin
                  BatchInfo.Batch := r;
                  BatchInfo.sour_bPos := i;
                  BatchInfo.sour_ePos := i + arry[r].sour.L - 1;
                  BatchInfo.dest_bPos := BatchInfo.sour_bPos;
                  BatchInfo.dest_ePos := BatchInfo.sour_ePos;
                  Info.Add(BatchInfo);
                end;
              inc(i, arry[r].sour.L);
              inc(arry[r].sum);
              inc(Result);
            end;
        end;
      if not found_ then
        begin
          inc(i);
        end;
    end;
end;

function umlBatchSum(s: TPascalString; var arry: TArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList): Integer;
begin
  Result := umlBatchSum(@s, arry, OnlyWord, IgnoreCase, bPos, ePos, Info);
end;

function umlBatchSum(s: TPascalString; var arry: TArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): Integer;
begin
  Result := umlBatchSum(@s, arry, OnlyWord, IgnoreCase, bPos, ePos, nil);
end;

function umlBatchReplace(p: PPascalString; var arry: TArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList; On_P: TOnBatchProc): TPascalString;
  function Match_(Pos_: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := Low(arry) to high(arry) do
      if (arry[i].sour.L > 0) and ((not OnlyWord) or umlIsWord(p, Pos_, Pos_ + arry[i].sour.L - 1))
        and p^.ComparePos(Pos_, @arry[i].sour, IgnoreCase) then
          exit(i);
  end;

var
  i, r, BP, EP: Integer;
  found_: Boolean;
  m64: TMem64;
  BatchInfo: TBatchInfo;
begin
  Result := '';
  if p^.L = 0 then
      exit;
  m64 := TMem64.CustomCreate(p^.L);

  if (ePos <= 0) or (ePos > p^.L) then
      EP := p^.L
  else
      EP := ePos;

  if bPos < 1 then
      BP := 1
  else if bPos > EP then
      BP := EP
  else
      BP := bPos;

  for i := low(arry) to high(arry) do
      arry[i].sum := 0;

  i := 1;
  while i <= p^.L do
    begin
      found_ := False;
      if (i >= BP) and (i <= EP) then
        begin
          r := Match_(i);
          found_ := r >= 0;
          if found_ and Assigned(On_P) then
              On_P(i, i + (arry[r].sour.L - 1), @arry[r].sour, @arry[r].dest, found_);
          if found_ then
            begin
              if Info <> nil then
                begin
                  BatchInfo.Batch := r;
                  BatchInfo.sour_bPos := i;
                  BatchInfo.sour_ePos := i + (arry[r].sour.L - 1);
                  BatchInfo.dest_bPos := m64.Size div SystemCharSize + 1;
                  BatchInfo.dest_ePos := BatchInfo.dest_bPos + (arry[r].dest.L - 1);
                  Info.Add(BatchInfo);
                end;
              m64.Write64(arry[r].dest.buff[0], SystemCharSize * arry[r].dest.L);
              inc(arry[r].sum);
              inc(i, arry[r].sour.L);
            end;
        end;
      if not found_ then
        begin
          m64.Write64(p^.buff[i - 1], SystemCharSize);
          inc(i);
        end;
    end;
  Result.L := m64.Size div SystemCharSize;
  if Result.L > 0 then
      CopyPtr(m64.Memory, @Result.buff[0], m64.Size);
  DisposeObject(m64);
end;

function umlBatchReplace(s: TPascalString; var arry: TArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList; On_P: TOnBatchProc): TPascalString;
begin
  Result := umlBatchReplace(@s, arry, OnlyWord, IgnoreCase, bPos, ePos, Info, On_P);
end;

function umlBatchReplace(s: TPascalString; var arry: TArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList): TPascalString;
begin
  Result := umlBatchReplace(@s, arry, OnlyWord, IgnoreCase, bPos, ePos, Info, nil);
end;

function umlBatchReplace(s: TPascalString; var arry: TArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): TPascalString;
begin
  Result := umlBatchReplace(@s, arry, OnlyWord, IgnoreCase, bPos, ePos, nil, nil);
end;

function umlReplaceSum(p: PPascalString; Pattern: TPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList): Integer;
var
  i, BP, EP: Integer;
  found_: Boolean;
  BatchInfo: TBatchInfo;
begin
  Result := 0;
  if p^.L = 0 then
      exit;

  if (ePos <= 0) or (ePos > p^.L) then
      EP := p^.L
  else
      EP := ePos;

  if bPos < 1 then
      BP := 1
  else if bPos > EP then
      BP := EP
  else
      BP := bPos;

  i := 1;
  while i <= p^.L do
    begin
      found_ := False;
      if (i >= BP) and (i <= EP) then
        begin
          found_ := ((not OnlyWord) or umlIsWord(p, i, i + Pattern.L - 1)) and p^.ComparePos(i, @Pattern, IgnoreCase);
          if found_ then
            begin
              if Info <> nil then
                begin
                  BatchInfo.Batch := -1;
                  BatchInfo.sour_bPos := i;
                  BatchInfo.sour_ePos := BatchInfo.sour_bPos + (Pattern.L - 1);
                  BatchInfo.dest_bPos := BatchInfo.sour_bPos;
                  BatchInfo.dest_ePos := BatchInfo.sour_ePos;
                  Info.Add(BatchInfo);
                end;
              inc(i, Pattern.L);
              inc(Result);
            end;
        end;
      if not found_ then
        begin
          inc(i);
        end;
    end;
end;

function umlReplaceSum(s, Pattern: TPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList): Integer;
begin
  Result := umlReplaceSum(@s, Pattern, OnlyWord, IgnoreCase, bPos, ePos, Info);
end;

function umlReplace(p: PPascalString; OldPattern, NewPattern: TPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList; On_P: TOnBatchProc): TPascalString;
var
  i, BP, EP: Integer;
  found_: Boolean;
  m64: TMem64;
  BatchInfo: TBatchInfo;
begin
  Result := '';
  if p^.L = 0 then
      exit;
  if OldPattern.L = 0 then
    begin
      Result := p^;
      exit;
    end;
  m64 := TMem64.CustomCreate(p^.L);

  if (ePos <= 0) or (ePos > p^.L) then
      EP := p^.L
  else
      EP := ePos;

  if bPos < 1 then
      BP := 1
  else if bPos > EP then
      BP := EP
  else
      BP := bPos;

  i := 1;
  while i <= p^.L do
    begin
      found_ := False;
      if (i >= BP) and (i <= EP) then
        begin
          found_ := ((not OnlyWord) or umlIsWord(p, i, i + OldPattern.L - 1)) and p^.ComparePos(i, @OldPattern, IgnoreCase);
          if found_ and Assigned(On_P) then
              On_P(i, i + (OldPattern.L - 1), @OldPattern, @NewPattern, found_);
          if found_ then
            begin
              if Info <> nil then
                begin
                  BatchInfo.Batch := -1;
                  BatchInfo.sour_bPos := i;
                  BatchInfo.sour_ePos := i + (OldPattern.L - 1);
                  BatchInfo.dest_bPos := m64.Size div SystemCharSize + 1;
                  BatchInfo.dest_ePos := BatchInfo.dest_bPos + (NewPattern.L - 1);
                  Info.Add(BatchInfo);
                end;
              m64.Write64(NewPattern.buff[0], SystemCharSize * NewPattern.L);
              inc(i, OldPattern.L);
            end;
        end;
      if not found_ then
        begin
          m64.Write64(p^.buff[i - 1], SystemCharSize);
          inc(i);
        end;
    end;
  Result.L := m64.Size div SystemCharSize;
  if Result.L > 0 then
      CopyPtr(m64.Memory, @Result.buff[0], m64.Size);
  DisposeObject(m64);
end;

function umlReplace(s, OldPattern, NewPattern: TPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList; On_P: TOnBatchProc): TPascalString;
begin
  Result := umlReplace(@s, OldPattern, NewPattern, OnlyWord, IgnoreCase, bPos, ePos, Info, On_P);
end;

function umlReplace(s, OldPattern, NewPattern: TPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TBatchInfoList): TPascalString;
begin
  Result := umlReplace(@s, OldPattern, NewPattern, OnlyWord, IgnoreCase, bPos, ePos, Info, nil);
end;

function umlReplace(p: PPascalString; OldPattern, NewPattern: TPascalString; OnlyWord, IgnoreCase: Boolean): TPascalString;
var
  i, r: Integer;
  m64: TMem64;
begin
  Result := '';
  if p^.L = 0 then
      exit;
  if OldPattern.L = 0 then
    begin
      Result := p^;
      exit;
    end;
  m64 := TMem64.CustomCreate(p^.L);
  i := 1;
  while i <= p^.L do
    begin
      if ((not OnlyWord) or umlIsWord(p, i, i + OldPattern.L - 1)) and p^.ComparePos(i, @OldPattern, IgnoreCase) then
        begin
          m64.Write64(NewPattern.buff[0], SystemCharSize * NewPattern.L);
          inc(i, OldPattern.L);
        end
      else
        begin
          m64.Write64(p^.buff[i - 1], SystemCharSize);
          inc(i);
        end;
    end;
  Result.L := m64.Size div SystemCharSize;
  if Result.L > 0 then
      CopyPtr(m64.Memory, @Result.buff[0], m64.Size);
  DisposeObject(m64);
end;

function umlReplace(s, OldPattern, NewPattern: TPascalString; OnlyWord, IgnoreCase: Boolean): TPascalString;
begin
  Result := umlReplace(@s, OldPattern, NewPattern, OnlyWord, IgnoreCase);
end;

function umlComputeTextPoint(p: PPascalString; Pos_: Integer): TPoint;
var
  i, j: Integer;
begin
  Result.X := 1;
  Result.Y := 1;
  if Pos_ < p^.L then
      j := Pos_
  else
      j := p^.L;
  for i := 1 to j do
    if p^[i] = #10 then
      begin
        Result.X := 1;
        inc(Result.Y);
      end
    else if p^[i] = #13 then
        Result.X := 0
    else
        inc(Result.X);
end;

function umlStringReplace(const s, OldPattern, NewPattern: TPascalString; IgnoreCase: Boolean): TPascalString;
var
  f: TReplaceFlags;
begin
  f := [rfReplaceAll];
  if IgnoreCase then
      f := f + [rfIgnoreCase];
  Result.text := StringReplace(s.text, OldPattern.text, NewPattern.text, f);
end;

function umlReplaceString(const s, OldPattern, NewPattern: TPascalString; IgnoreCase: Boolean): TPascalString;
begin
  Result := umlStringReplace(s, OldPattern, NewPattern, IgnoreCase);
end;

function umlCharReplace(const s: TPascalString; OldPattern, NewPattern: U_Char): TPascalString;
begin
  Result := s.ReplaceChar(OldPattern, NewPattern);
end;

function umlReplaceChar(const s: TPascalString; OldPattern, NewPattern: U_Char): TPascalString;
begin
  Result := s.ReplaceChar(OldPattern, NewPattern);
end;

function umlEncodeText2HTML(const psSrc: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  if psSrc.L > 0 then
    begin
      i := 1;
      while i <= psSrc.L do
        begin
          case psSrc[i] of
            ' ': Result.Append('&nbsp;');
            '<': Result.Append('&lt;');
            '>': Result.Append('&gt;');
            '&': Result.Append('&amp;');
            '"': Result.Append('&quot;');
            #9: Result.Append('&nbsp;&nbsp;&nbsp;&nbsp;');
            #13:
              begin
                if i + 1 <= psSrc.L then
                  begin
                    if psSrc[i + 1] = #10 then
                        inc(i);
                    Result.Append('<br>');
                  end
                else
                  begin
                    Result.Append('<br>');
                  end;
              end;
            #10:
              begin
                if i + 1 <= psSrc.L then
                  begin
                    if psSrc[i + 1] = #13 then
                        inc(i);
                    Result.Append('<br>');
                  end
                else
                  begin
                    Result.Append('<br>');
                  end;
              end;
            else
              Result.Append(psSrc[i]);
          end;
          inc(i);
        end;
    end;
end;

function umlURLEncode(const Data: TPascalString): TPascalString;
var
  UTF8Src: TBytes;
  B: Byte;
  tmp: TMem64;
  s: TPascalString;
  c: SystemChar;
begin
  tmp := TMem64.CustomCreate(Data.L);
  UTF8Src := Data.Bytes;
  for B in UTF8Src do
    if ((B >= $41) and (B <= $5A))
      or ((B >= $61) and (B <= $7A))
      or ((B >= $30) and (B <= $39))
      or (B = $2D)
      or (B = $2E)
      or (B = $5F)
      or (B = $7E)
      or (B = $2F)
      or (B = $3A) then
        tmp.WriteUInt8(B)
    else
      begin
        s := '%' + IntToHex(B, 2);
        for c in s.buff do
            tmp.WriteUInt8(Byte(c));
      end;
  UTF8Src := tmp.ToBytes;
  Result.Bytes := UTF8Src;
  SetLength(UTF8Src, 0);
  DisposeObject(tmp);
end;

function umlURLDecode(const Data: TPascalString; FormEncoded: Boolean): TPascalString;
var
  i: Integer;
  State: Byte;
  B, BV, B1: Byte;
  arry: TBytes;
  tmp: TMem64;
const
  STATE_READ_DATA = 0;
  STATE_READ_PERCENT_ENCODED_BYTE_1 = 1;
  STATE_READ_PERCENT_ENCODED_BYTE_2 = 2;
begin
  B1 := 0;
  State := STATE_READ_DATA;
  arry := Data.Bytes;
  tmp := TMem64.CustomCreate(length(arry));
  for i := 0 to length(arry) - 1 do
    begin
      B := arry[i];
      if State = STATE_READ_DATA then
        begin
          if B = $25 then
              State := STATE_READ_PERCENT_ENCODED_BYTE_1
          else if FormEncoded and (B = $2B) then // + sign
              tmp.WriteUInt8($20)
          else
              tmp.WriteUInt8(Byte(Data[FirstCharPos + i]));
        end
      else if (State = STATE_READ_PERCENT_ENCODED_BYTE_1) or (State = STATE_READ_PERCENT_ENCODED_BYTE_2) then
        begin
          if (B >= 65) and (B <= 70) then
              BV := B - 55
          else if (B >= 97) and (B <= 102) then
              BV := B - 87
          else if (B >= $30) and (B <= $39) then
              BV := B - $30
          else
              raiseInfo('Unexpected character: 0x' + IntToHex(B, 2));

          if State = STATE_READ_PERCENT_ENCODED_BYTE_1 then
            begin
              B1 := BV;
              State := STATE_READ_PERCENT_ENCODED_BYTE_2;
            end
          else
            begin
              B := (B1 shl 4) or BV;
              tmp.WriteUInt8(B);
              State := STATE_READ_DATA;
            end;
        end;
    end;
  arry := tmp.ToBytes;
  Result.Bytes := arry;
  SetLength(arry, 0);
  DisposeObject(tmp);
end;

function B64EstimateEncodedSize(cont: TBase64Context; InSize: Integer): Integer;
begin
  Result := ((InSize + 2) div 3) shl 2;
  if (cont.EOLSize > 0) and (cont.LineSize > 0) then
    begin
      Result := Result + ((Result + cont.LineSize - 1) div cont.LineSize) * cont.EOLSize;
      if not cont.TrailingEol then
          Result := Result - cont.EOLSize;
    end;
end;

function B64InitializeDecoding(var cont: TBase64Context; LiberalMode: Boolean): Boolean;
begin
  cont.TailBytes := 0;
  cont.EQUCount := 0;
  cont.LiberalMode := LiberalMode;

  Result := True;
end;

function B64InitializeEncoding(var cont: TBase64Context; LineSize: Integer; fEOL: TBase64EOLMarker; TrailingEol: Boolean): Boolean;
begin

  Result := False;
  cont.TailBytes := 0;
  cont.LineSize := LineSize;
  cont.LineWritten := 0;
  cont.EQUCount := 0;
  cont.TrailingEol := TrailingEol;
  cont.PutFirstEol := False;

  if LineSize < 4 then
      exit;

  case fEOL of
    emCRLF:
      begin
        cont.fEOL[0] := $0D;
        cont.fEOL[1] := $0A;
        cont.EOLSize := 2;
      end;
    emCR:
      begin
        cont.fEOL[0] := $0D;
        cont.EOLSize := 1;
      end;
    emLF:
      begin
        cont.fEOL[0] := $0A;
        cont.EOLSize := 1;
      end;
    else
      cont.EOLSize := 0;
  end;

  Result := True;
end;

function B64Encode(var cont: TBase64Context; buffer: PByte; Size: Integer; OutBuffer: PByte; var OutSize: Integer): Boolean;
var
  EstSize, i, Chunks: Integer;
  PreserveLastEol: Boolean;
begin
  PreserveLastEol := False;

  EstSize := ((Size + cont.TailBytes) div 3) shl 2;
  if (cont.LineSize > 0) and (cont.EOLSize > 0) then
    begin
      if (EstSize > 0) and ((cont.LineWritten + EstSize) mod cont.LineSize = 0) and
        ((cont.TailBytes + Size) mod 3 = 0) then
          PreserveLastEol := True;
      EstSize := EstSize + ((EstSize + cont.LineWritten) div cont.LineSize) * cont.EOLSize;
      if PreserveLastEol then
          EstSize := EstSize - cont.EOLSize;
    end;
  if cont.PutFirstEol then
      EstSize := EstSize + cont.EOLSize;

  if OutSize < EstSize then
    begin
      OutSize := EstSize;
      Result := False;
      exit;
    end;

  OutSize := EstSize;

  if cont.PutFirstEol then
    begin
      CopyPtr(@cont.fEOL[0], OutBuffer, cont.EOLSize);
      inc(OutBuffer, cont.EOLSize);
      cont.PutFirstEol := False;
    end;

  if Size + cont.TailBytes < 3 then
    begin
      for i := 0 to Size - 1 do
          cont.Tail[cont.TailBytes + i] := PBase64ByteArray(buffer)^[i];
      inc(cont.TailBytes, Size);
      Result := True;
      exit;
    end;

  if cont.TailBytes > 0 then
    begin
      for i := 0 to 2 - cont.TailBytes do
          cont.Tail[cont.TailBytes + i] := PBase64ByteArray(buffer)^[i];

      inc(buffer, 3 - cont.TailBytes);
      dec(Size, 3 - cont.TailBytes);

      cont.TailBytes := 0;

      cont.OutBuf[0] := Base64Symbols[cont.Tail[0] shr 2];
      cont.OutBuf[1] := Base64Symbols[((cont.Tail[0] and 3) shl 4) or (cont.Tail[1] shr 4)];
      cont.OutBuf[2] := Base64Symbols[((cont.Tail[1] and $F) shl 2) or (cont.Tail[2] shr 6)];
      cont.OutBuf[3] := Base64Symbols[cont.Tail[2] and $3F];

      if (cont.LineSize = 0) or (cont.LineWritten + 4 < cont.LineSize) then
        begin
          CopyPtr(@cont.OutBuf[0], OutBuffer, 4);
          inc(OutBuffer, 4);
          inc(cont.LineWritten, 4);
        end
      else
        begin
          i := cont.LineSize - cont.LineWritten;
          CopyPtr(@cont.OutBuf[0], OutBuffer, i);
          inc(OutBuffer, i);
          if (Size > 0) or (i < 4) or (not PreserveLastEol) then
            begin
              CopyPtr(@cont.fEOL[0], OutBuffer, cont.EOLSize);
              inc(OutBuffer, cont.EOLSize);
            end;
          CopyPtr(@cont.OutBuf[i], OutBuffer, 4 - i);
          inc(OutBuffer, 4 - i);
          cont.LineWritten := 4 - i;
        end;
    end;

  while Size >= 3 do
    begin
      if cont.LineSize > 0 then
        begin
          Chunks := (cont.LineSize - cont.LineWritten) shr 2;
          if Chunks > Size div 3 then
              Chunks := Size div 3;
        end
      else
          Chunks := Size div 3;

      for i := 0 to Chunks - 1 do
        begin
          OutBuffer^ := Base64Symbols[PBase64ByteArray(buffer)^[0] shr 2];
          inc(OutBuffer);
          PByte(OutBuffer)^ := Base64Symbols[((PBase64ByteArray(buffer)^[0] and 3) shl 4) or (PBase64ByteArray(buffer)^[1] shr 4)];
          inc(OutBuffer);
          PByte(OutBuffer)^ := Base64Symbols[((PBase64ByteArray(buffer)^[1] and $F) shl 2) or (PBase64ByteArray(buffer)^[2] shr 6)];
          inc(OutBuffer);
          PByte(OutBuffer)^ := Base64Symbols[PBase64ByteArray(buffer)^[2] and $3F];
          inc(OutBuffer);
          inc(buffer, 3);
        end;

      dec(Size, 3 * Chunks);

      if cont.LineSize > 0 then
        begin
          inc(cont.LineWritten, Chunks shl 2);

          if (Size >= 3) and (cont.LineSize - cont.LineWritten > 0) then
            begin
              cont.OutBuf[0] := Base64Symbols[PBase64ByteArray(buffer)^[0] shr 2];
              cont.OutBuf[1] := Base64Symbols[((PBase64ByteArray(buffer)^[0] and 3) shl 4) or (PBase64ByteArray(buffer)^[1] shr 4)];
              cont.OutBuf[2] := Base64Symbols[((PBase64ByteArray(buffer)^[1] and $F) shl 2) or (PBase64ByteArray(buffer)^[2] shr 6)];
              cont.OutBuf[3] := Base64Symbols[PBase64ByteArray(buffer)^[2] and $3F];
              inc(buffer, 3);

              dec(Size, 3);

              i := cont.LineSize - cont.LineWritten;

              CopyPtr(@cont.OutBuf[0], OutBuffer, i);
              inc(OutBuffer, i);
              if (cont.EOLSize > 0) and ((i < 4) or (Size > 0) or (not PreserveLastEol)) then
                begin
                  CopyPtr(@cont.fEOL[0], OutBuffer, cont.EOLSize);
                  inc(OutBuffer, cont.EOLSize);
                end;

              CopyPtr(@cont.OutBuf[i], OutBuffer, 4 - i);
              inc(OutBuffer, 4 - i);

              cont.LineWritten := 4 - i;
            end
          else
            if cont.LineWritten = cont.LineSize then
            begin
              cont.LineWritten := 0;
              if (cont.EOLSize > 0) and ((Size > 0) or (not PreserveLastEol)) then
                begin
                  CopyPtr(@cont.fEOL[0], OutBuffer, cont.EOLSize);
                  inc(OutBuffer, cont.EOLSize);
                end;
            end;
        end;
    end;

  if Size > 0 then
    begin
      CopyPtr(buffer, @cont.Tail[0], Size);
      cont.TailBytes := Size;
    end
  else
    if PreserveLastEol then
      cont.PutFirstEol := True;

  Result := True;
end;

function B64Decode(var cont: TBase64Context; buffer: PByte; Size: Integer; OutBuffer: PByte; var OutSize: Integer): Boolean;
var
  i, EstSize, EQUCount: Integer;
  BufPtr: PByte;
  c: Byte;
begin
  if Size = 0 then
    begin
      Result := True;
      OutSize := 0;
      exit;
    end;

  EQUCount := cont.EQUCount;
  EstSize := cont.TailBytes;
  BufPtr := buffer;

  for i := 0 to Size - 1 do
    begin
      c := Base64Values[PByte(BufPtr)^];
      if c < 64 then
          inc(EstSize)
      else
        if c = $FF then
        begin
          if not cont.LiberalMode then
            begin
              Result := False;
              OutSize := 0;
              exit;
            end;
        end
      else
        if c = $FD then
        begin
          if EQUCount > 1 then
            begin
              Result := False;
              OutSize := 0;
              exit;
            end;
          inc(EQUCount);
        end;
      inc(BufPtr);
    end;

  EstSize := (EstSize shr 2) * 3;
  if OutSize < EstSize then
    begin
      OutSize := EstSize;
      Result := False;
      exit;
    end;

  cont.EQUCount := EQUCount;
  OutSize := EstSize;

  while Size > 0 do
    begin
      c := Base64Values[PByte(buffer)^];
      if c < 64 then
        begin
          cont.Tail[cont.TailBytes] := c;
          inc(cont.TailBytes);

          if cont.TailBytes = 4 then
            begin
              PByte(OutBuffer)^ := (cont.Tail[0] shl 2) or (cont.Tail[1] shr 4);
              inc(OutBuffer);

              PByte(OutBuffer)^ := ((cont.Tail[1] and $F) shl 4) or (cont.Tail[2] shr 2);
              inc(OutBuffer);

              PByte(OutBuffer)^ := ((cont.Tail[2] and $3) shl 6) or cont.Tail[3];
              inc(OutBuffer);

              cont.TailBytes := 0;
            end;
        end;
      inc(buffer);
      dec(Size);
    end;
  Result := True;
end;

function B64FinalizeEncoding(var cont: TBase64Context; OutBuffer: PByte; var OutSize: Integer): Boolean;
var
  EstSize: Integer;
begin
  if cont.TailBytes > 0 then
      EstSize := 4
  else
      EstSize := 0;

  if cont.TrailingEol then
      EstSize := EstSize + cont.EOLSize;

  if OutSize < EstSize then
    begin
      OutSize := EstSize;
      Result := False;
      exit;
    end;

  OutSize := EstSize;

  if cont.TailBytes = 0 then
    begin
      { writing trailing EOL }
      Result := True;
      if (cont.EOLSize > 0) and cont.TrailingEol then
        begin
          OutSize := cont.EOLSize;
          CopyPtr(@cont.fEOL[0], OutBuffer, cont.EOLSize);
        end;
      exit;
    end;

  if cont.TailBytes = 1 then
    begin
      PBase64ByteArray(OutBuffer)^[0] := Base64Symbols[cont.Tail[0] shr 2];
      PBase64ByteArray(OutBuffer)^[1] := Base64Symbols[((cont.Tail[0] and 3) shl 4)];
      PBase64ByteArray(OutBuffer)^[2] := $3D; // '='
      PBase64ByteArray(OutBuffer)^[3] := $3D; // '='
    end
  else if cont.TailBytes = 2 then
    begin
      PBase64ByteArray(OutBuffer)^[0] := Base64Symbols[cont.Tail[0] shr 2];
      PBase64ByteArray(OutBuffer)^[1] := Base64Symbols[((cont.Tail[0] and 3) shl 4) or (cont.Tail[1] shr 4)];
      PBase64ByteArray(OutBuffer)^[2] := Base64Symbols[((cont.Tail[1] and $F) shl 2)];
      PBase64ByteArray(OutBuffer)^[3] := $3D; // '='
    end;

  if (cont.EOLSize > 0) and (cont.TrailingEol) then
      CopyPtr(@cont.fEOL[0], @PBase64ByteArray(OutBuffer)^[4], cont.EOLSize);

  Result := True;
end;

function B64FinalizeDecoding(var cont: TBase64Context; OutBuffer: PByte; var OutSize: Integer): Boolean;
begin
  if (cont.EQUCount = 0) then
    begin
      OutSize := 0;
      Result := cont.TailBytes = 0;
      exit;
    end
  else
    if (cont.EQUCount = 1) then
    begin
      if cont.TailBytes <> 3 then
        begin
          Result := False;
          OutSize := 0;
          exit;
        end;

      if OutSize < 2 then
        begin
          OutSize := 2;
          Result := False;
          exit;
        end;

      PByte(OutBuffer)^ := (cont.Tail[0] shl 2) or (cont.Tail[1] shr 4);
      inc(OutBuffer);
      PByte(OutBuffer)^ := ((cont.Tail[1] and $F) shl 4) or (cont.Tail[2] shr 2);
      OutSize := 2;
      Result := True;
    end
  else if (cont.EQUCount = 2) then
    begin
      if cont.TailBytes <> 2 then
        begin
          Result := False;
          OutSize := 0;
          exit;
        end;

      if OutSize < 1 then
        begin
          OutSize := 1;
          Result := False;
          exit;
        end;

      PByte(OutBuffer)^ := (cont.Tail[0] shl 2) or (cont.Tail[1] shr 4);

      OutSize := 1;
      Result := True;
    end
  else
    begin
      Result := False;
      OutSize := 0;
    end;
end;

function umlBase64Encode(InBuffer: PByte; InSize: Integer; OutBuffer: PByte; var OutSize: Integer; WrapLines: Boolean): Boolean;
var
  cont: TBase64Context;
  TmpSize: Integer;
begin
  if WrapLines then
      B64InitializeEncoding(cont, 64, emCRLF, False)
  else
      B64InitializeEncoding(cont, 0, emNone, False);

  TmpSize := B64EstimateEncodedSize(cont, InSize);

  if (OutSize < TmpSize) then
    begin
      OutSize := TmpSize;
      Result := False;
      exit;
    end;

  TmpSize := OutSize;
  B64Encode(cont, InBuffer, InSize, OutBuffer, TmpSize);
  OutSize := OutSize - TmpSize;
  B64FinalizeEncoding(cont, PByte(NativeUInt(OutBuffer) + UInt32(TmpSize)), OutSize);
  OutSize := OutSize + TmpSize;

  Result := True;
end;

function umlBase64Decode(InBuffer: PByte; InSize: Integer; OutBuffer: PByte; var OutSize: Integer; LiberalMode: Boolean): Integer;
var
  i, TmpSize: Integer;
  ExtraSyms: Integer;
  cont: TBase64Context;
begin
  ExtraSyms := 0;
  try
    for i := 0 to InSize - 1 do
      if (PBase64ByteArray(InBuffer)^[i] in [$0D, $0A, $0]) then // some buggy software products insert 0x00 characters to BASE64 they produce
          inc(ExtraSyms);
  except
  end;

  if not LiberalMode then
    begin
      if ((InSize - ExtraSyms) and $3) <> 0 then
        begin
          Result := BASE64_DECODE_WRONG_DATA_SIZE;
          OutSize := 0;
          exit;
        end;
    end;

  TmpSize := ((InSize - ExtraSyms) shr 2) * 3;
  if OutSize < TmpSize then
    begin
      Result := BASE64_DECODE_NOT_ENOUGH_SPACE;
      OutSize := TmpSize;
      exit;
    end;

  B64InitializeDecoding(cont, LiberalMode);
  TmpSize := OutSize;
  if not B64Decode(cont, InBuffer, InSize, OutBuffer, TmpSize) then
    begin
      Result := BASE64_DECODE_INVALID_CHARACTER;
      OutSize := 0;
      exit;
    end;
  OutSize := OutSize - TmpSize;
  if not B64FinalizeDecoding(cont, @PBase64ByteArray(OutBuffer)^[TmpSize], OutSize) then
    begin
      Result := BASE64_DECODE_INVALID_CHARACTER;
      OutSize := 0;
      exit;
    end;
  OutSize := OutSize + TmpSize;
  Result := BASE64_DECODE_OK;
end;

procedure umlBase64EncodeBytes(var sour, dest: TBytes);
var
  Size: Integer;
begin
  if length(sour) = 0 then
      exit;

  Size := 0;
  SetLength(dest, 0);
  umlBase64Encode(@sour[0], length(sour), nil, Size, False);
  SetLength(dest, Size);
  umlBase64Encode(@sour[0], length(sour), @dest[0], Size, False);
  SetLength(dest, Size);
end;

procedure umlBase64DecodeBytes(var sour, dest: TBytes);
var
  Size: Integer;
begin
  if length(sour) = 0 then
    begin
      SetLength(dest, 0);
      exit;
    end;

  Size := 0;
  umlBase64Decode(@sour[0], length(sour), nil, Size, True);
  SetLength(dest, Size);
  umlBase64Decode(@sour[0], length(sour), @dest[0], Size, True);
  SetLength(dest, Size);
end;

procedure umlBase64EncodeBytes(var sour: TBytes; var dest: TPascalString);
var
  buff: TBytes;
begin
  umlBase64EncodeBytes(sour, buff);
  dest.Bytes := buff;
end;

procedure umlBase64DecodeBytes(const sour: TPascalString; var dest: TBytes);
var
  buff: TBytes;
begin
  buff := sour.Bytes;
  umlBase64DecodeBytes(buff, dest);
end;

procedure umlDecodeLineBASE64(const buffer: TPascalString; var output: TPascalString);
var
  B, nb: TBytes;
begin
  B := umlBytesOf(buffer);
  umlBase64DecodeBytes(B, nb);
  output := umlStringOf(nb);
end;

procedure umlEncodeLineBASE64(const buffer: TPascalString; var output: TPascalString);
var
  B, nb: TBytes;
begin
  B := umlBytesOf(buffer);
  umlBase64EncodeBytes(B, nb);
  output := umlStringOf(nb);
end;

function umlDecodeLineBASE64(const buffer: TPascalString): TPascalString;
begin
  umlDecodeLineBASE64(buffer, Result);
end;

function umlEncodeLineBASE64(const buffer: TPascalString): TPascalString;
begin
  umlEncodeLineBASE64(buffer, Result);
end;

procedure umlDecodeStreamBASE64(const buffer: TPascalString; output: TCore_Stream);
var
  B, nb: TBytes;
  bak: Int64;
begin
  B := umlBytesOf(buffer);
  umlBase64DecodeBytes(B, nb);
  bak := output.Position;
  output.WriteBuffer(nb[0], length(nb));
  output.Position := bak;
end;

procedure umlEncodeStreamBASE64(buffer: TCore_Stream; var output: TPascalString);
var
  B, nb: TBytes;
  bak: Int64;
begin
  bak := buffer.Position;

  buffer.Position := 0;
  SetLength(B, buffer.Size);
  buffer.ReadBuffer(B[0], buffer.Size);
  umlBase64EncodeBytes(B, nb);
  output := umlStringOf(nb);

  buffer.Position := bak;
end;

function umlDivisionBase64Text(const buffer: TPascalString; width: Integer; DivisionAsPascalString: Boolean): TPascalString;
var
  i, n: Integer;
begin
  Result := '';
  n := 0;
  for i := 1 to buffer.L do
    begin
      if (DivisionAsPascalString) and (n = 0) then
          Result.Append(#39);

      Result.Append(buffer[i]);
      inc(n);
      if n = width then
        begin
          if DivisionAsPascalString then
              Result.Append(#39 + '+' + #13#10)
          else
              Result.Append(#13#10);
          n := 0;
        end;
    end;
  if DivisionAsPascalString then
      Result.Append(#39);
end;

function umlTestBase64(const text: TPascalString): Boolean;
var
  sour, dest: TBytes;
begin
  sour := text.Bytes;
  SetLength(dest, 0);
  try
      umlBase64DecodeBytes(sour, dest);
  except
  end;
  Result := length(dest) > 0;
  if Result then
      SetLength(dest, 0);
end;

constructor TMD5_Pair_Pool.Create(HashSize_: Integer);
begin
  inherited Create(HashSize_, NullMD5);
  IsChanged := False;
end;

procedure TMD5_Pair_Pool.DoFree(var Key: TMD5; var Value: TMD5);
begin
  inherited;
  IsChanged := True;
end;

procedure TMD5_Pair_Pool.DoAdd(var Key: TMD5; var Value: TMD5);
begin
  inherited;
  IsChanged := True;
end;

procedure TMD5_Pair_Pool.LoadFromStream(stream: TCore_Stream);
begin
  Clear;
  while stream.Position + 32 <= stream.Size do
      Add(StreamReadMD5(stream), StreamReadMD5(stream), False);
end;

procedure TMD5_Pair_Pool.SaveToStream(stream: TCore_Stream);
begin
  with Repeat_ do
    repeat
      StreamWriteMD5(stream, Queue^.Data^.Data.Primary);
      StreamWriteMD5(stream, Queue^.Data^.Data.Second);
    until not Right;
end;

function umlStrToMD5(hex: TPascalString): TMD5;
begin
  TCipher.HexToBuffer(hex, Result[0], SizeOf(TMD5));
end;

procedure umlTransformMD5(var Accu; const Buf);
{$IF Defined(FastMD5) and Defined(Delphi) and (Defined(WIN32) or Defined(WIN64))}
begin
  MD5_Transform(Accu, Buf);
end;
{$ELSE}
  function ROL(const X: Cardinal; const n: Byte): Cardinal;
  begin
    Result := (X shl n) or (X shr (32 - n))
  end;

  function FF(const A, B, c, d, X: Cardinal; const s: Byte; const AC: Cardinal): Cardinal;
  begin
    Result := ROL(A + X + AC + (B and c or not B and d), s) + B
  end;

  function GG(const A, B, c, d, X: Cardinal; const s: Byte; const AC: Cardinal): Cardinal;
  begin
    Result := ROL(A + X + AC + (B and d or c and not d), s) + B
  end;

  function HH(const A, B, c, d, X: Cardinal; const s: Byte; const AC: Cardinal): Cardinal;
  begin
    Result := ROL(A + X + AC + (B xor c xor d), s) + B
  end;

  function II(const A, B, c, d, X: Cardinal; const s: Byte; const AC: Cardinal): Cardinal;
  begin
    Result := ROL(A + X + AC + (c xor (B or not d)), s) + B
  end;

type
  TDigestCardinal = array [0 .. 3] of Cardinal;
  TCardinalBuf = array [0 .. 15] of Cardinal;
var
  A, B, c, d: Cardinal;
begin
  A := TDigestCardinal(Accu)[0];
  B := TDigestCardinal(Accu)[1];
  c := TDigestCardinal(Accu)[2];
  d := TDigestCardinal(Accu)[3];

  A := FF(A, B, c, d, TCardinalBuf(Buf)[0], 7, $D76AA478);   { 1 }
  d := FF(d, A, B, c, TCardinalBuf(Buf)[1], 12, $E8C7B756);  { 2 }
  c := FF(c, d, A, B, TCardinalBuf(Buf)[2], 17, $242070DB);  { 3 }
  B := FF(B, c, d, A, TCardinalBuf(Buf)[3], 22, $C1BDCEEE);  { 4 }
  A := FF(A, B, c, d, TCardinalBuf(Buf)[4], 7, $F57C0FAF);   { 5 }
  d := FF(d, A, B, c, TCardinalBuf(Buf)[5], 12, $4787C62A);  { 6 }
  c := FF(c, d, A, B, TCardinalBuf(Buf)[6], 17, $A8304613);  { 7 }
  B := FF(B, c, d, A, TCardinalBuf(Buf)[7], 22, $FD469501);  { 8 }
  A := FF(A, B, c, d, TCardinalBuf(Buf)[8], 7, $698098D8);   { 9 }
  d := FF(d, A, B, c, TCardinalBuf(Buf)[9], 12, $8B44F7AF);  { 10 }
  c := FF(c, d, A, B, TCardinalBuf(Buf)[10], 17, $FFFF5BB1); { 11 }
  B := FF(B, c, d, A, TCardinalBuf(Buf)[11], 22, $895CD7BE); { 12 }
  A := FF(A, B, c, d, TCardinalBuf(Buf)[12], 7, $6B901122);  { 13 }
  d := FF(d, A, B, c, TCardinalBuf(Buf)[13], 12, $FD987193); { 14 }
  c := FF(c, d, A, B, TCardinalBuf(Buf)[14], 17, $A679438E); { 15 }
  B := FF(B, c, d, A, TCardinalBuf(Buf)[15], 22, $49B40821); { 16 }
  A := GG(A, B, c, d, TCardinalBuf(Buf)[1], 5, $F61E2562);   { 17 }
  d := GG(d, A, B, c, TCardinalBuf(Buf)[6], 9, $C040B340);   { 18 }
  c := GG(c, d, A, B, TCardinalBuf(Buf)[11], 14, $265E5A51); { 19 }
  B := GG(B, c, d, A, TCardinalBuf(Buf)[0], 20, $E9B6C7AA);  { 20 }
  A := GG(A, B, c, d, TCardinalBuf(Buf)[5], 5, $D62F105D);   { 21 }
  d := GG(d, A, B, c, TCardinalBuf(Buf)[10], 9, $02441453);  { 22 }
  c := GG(c, d, A, B, TCardinalBuf(Buf)[15], 14, $D8A1E681); { 23 }
  B := GG(B, c, d, A, TCardinalBuf(Buf)[4], 20, $E7D3FBC8);  { 24 }
  A := GG(A, B, c, d, TCardinalBuf(Buf)[9], 5, $21E1CDE6);   { 25 }
  d := GG(d, A, B, c, TCardinalBuf(Buf)[14], 9, $C33707D6);  { 26 }
  c := GG(c, d, A, B, TCardinalBuf(Buf)[3], 14, $F4D50D87);  { 27 }
  B := GG(B, c, d, A, TCardinalBuf(Buf)[8], 20, $455A14ED);  { 28 }
  A := GG(A, B, c, d, TCardinalBuf(Buf)[13], 5, $A9E3E905);  { 29 }
  d := GG(d, A, B, c, TCardinalBuf(Buf)[2], 9, $FCEFA3F8);   { 30 }
  c := GG(c, d, A, B, TCardinalBuf(Buf)[7], 14, $676F02D9);  { 31 }
  B := GG(B, c, d, A, TCardinalBuf(Buf)[12], 20, $8D2A4C8A); { 32 }
  A := HH(A, B, c, d, TCardinalBuf(Buf)[5], 4, $FFFA3942);   { 33 }
  d := HH(d, A, B, c, TCardinalBuf(Buf)[8], 11, $8771F681);  { 34 }
  c := HH(c, d, A, B, TCardinalBuf(Buf)[11], 16, $6D9D6122); { 35 }
  B := HH(B, c, d, A, TCardinalBuf(Buf)[14], 23, $FDE5380C); { 36 }
  A := HH(A, B, c, d, TCardinalBuf(Buf)[1], 4, $A4BEEA44);   { 37 }
  d := HH(d, A, B, c, TCardinalBuf(Buf)[4], 11, $4BDECFA9);  { 38 }
  c := HH(c, d, A, B, TCardinalBuf(Buf)[7], 16, $F6BB4B60);  { 39 }
  B := HH(B, c, d, A, TCardinalBuf(Buf)[10], 23, $BEBFBC70); { 40 }
  A := HH(A, B, c, d, TCardinalBuf(Buf)[13], 4, $289B7EC6);  { 41 }
  d := HH(d, A, B, c, TCardinalBuf(Buf)[0], 11, $EAA127FA);  { 42 }
  c := HH(c, d, A, B, TCardinalBuf(Buf)[3], 16, $D4EF3085);  { 43 }
  B := HH(B, c, d, A, TCardinalBuf(Buf)[6], 23, $04881D05);  { 44 }
  A := HH(A, B, c, d, TCardinalBuf(Buf)[9], 4, $D9D4D039);   { 45 }
  d := HH(d, A, B, c, TCardinalBuf(Buf)[12], 11, $E6DB99E5); { 46 }
  c := HH(c, d, A, B, TCardinalBuf(Buf)[15], 16, $1FA27CF8); { 47 }
  B := HH(B, c, d, A, TCardinalBuf(Buf)[2], 23, $C4AC5665);  { 48 }
  A := II(A, B, c, d, TCardinalBuf(Buf)[0], 6, $F4292244);   { 49 }
  d := II(d, A, B, c, TCardinalBuf(Buf)[7], 10, $432AFF97);  { 50 }
  c := II(c, d, A, B, TCardinalBuf(Buf)[14], 15, $AB9423A7); { 51 }
  B := II(B, c, d, A, TCardinalBuf(Buf)[5], 21, $FC93A039);  { 52 }
  A := II(A, B, c, d, TCardinalBuf(Buf)[12], 6, $655B59C3);  { 53 }
  d := II(d, A, B, c, TCardinalBuf(Buf)[3], 10, $8F0CCC92);  { 54 }
  c := II(c, d, A, B, TCardinalBuf(Buf)[10], 15, $FFEFF47D); { 55 }
  B := II(B, c, d, A, TCardinalBuf(Buf)[1], 21, $85845DD1);  { 56 }
  A := II(A, B, c, d, TCardinalBuf(Buf)[8], 6, $6FA87E4F);   { 57 }
  d := II(d, A, B, c, TCardinalBuf(Buf)[15], 10, $FE2CE6E0); { 58 }
  c := II(c, d, A, B, TCardinalBuf(Buf)[6], 15, $A3014314);  { 59 }
  B := II(B, c, d, A, TCardinalBuf(Buf)[13], 21, $4E0811A1); { 60 }
  A := II(A, B, c, d, TCardinalBuf(Buf)[4], 6, $F7537E82);   { 61 }
  d := II(d, A, B, c, TCardinalBuf(Buf)[11], 10, $BD3AF235); { 62 }
  c := II(c, d, A, B, TCardinalBuf(Buf)[2], 15, $2AD7D2BB);  { 63 }
  B := II(B, c, d, A, TCardinalBuf(Buf)[9], 21, $EB86D391);  { 64 }

  inc(TDigestCardinal(Accu)[0], A);
  inc(TDigestCardinal(Accu)[1], B);
  inc(TDigestCardinal(Accu)[2], c);
  inc(TDigestCardinal(Accu)[3], d)
end;
{$ENDIF}


function umlMD5(const buffPtr: PByte; bufSiz: NativeUInt): TMD5;
{$IF Defined(FastMD5) and Defined(Delphi) and (Defined(WIN32) or Defined(WIN64))}
begin
  Result := FastMD5(buffPtr, bufSiz);
end;
{$ELSE}


var
  Digest: TMD5;
  Lo, Hi: Cardinal;
  p: PByte;
  ChunkIndex: Byte;
  ChunkBuff: array [0 .. 63] of Byte;
begin
  Lo := 0;
  Hi := 0;
  PCardinal(@Digest[0])^ := $67452301;
  PCardinal(@Digest[4])^ := $EFCDAB89;
  PCardinal(@Digest[8])^ := $98BADCFE;
  PCardinal(@Digest[12])^ := $10325476;

  inc(Lo, bufSiz shl 3);
  inc(Hi, bufSiz shr 29);

  p := buffPtr;

  while bufSiz >= $40 do
    begin
      umlTransformMD5(Digest, p^);
      inc(p, $40);
      dec(bufSiz, $40);
    end;
  if bufSiz > 0 then
      CopyPtr(p, @ChunkBuff[0], bufSiz);

  Result := PMD5(@Digest[0])^;
  ChunkBuff[bufSiz] := $80;
  ChunkIndex := bufSiz + 1;
  if ChunkIndex > $38 then
    begin
      if ChunkIndex < $40 then
          FillPtrByte(@ChunkBuff[ChunkIndex], $40 - ChunkIndex, 0);
      umlTransformMD5(Result, ChunkBuff);
      ChunkIndex := 0
    end;
  FillPtrByte(@ChunkBuff[ChunkIndex], $38 - ChunkIndex, 0);
  PCardinal(@ChunkBuff[$38])^ := Lo;
  PCardinal(@ChunkBuff[$3C])^ := Hi;
  umlTransformMD5(Result, ChunkBuff);
end;
{$ENDIF}


function umlMD5Char(const buffPtr: PByte; const BuffSize: NativeUInt): TPascalString;
begin
  Result := umlMD5ToStr(umlMD5(buffPtr, BuffSize));
end;

function umlMD5String(const buffPtr: PByte; const BuffSize: NativeUInt): TPascalString;
begin
  Result := umlMD5ToStr(umlMD5(buffPtr, BuffSize));
end;

function umlMD5Str(const buffPtr: PByte; const BuffSize: NativeUInt): TPascalString;
begin
  Result := umlMD5ToStr(umlMD5(buffPtr, BuffSize));
end;

function umlStreamMD5(stream: TCore_Stream; StartPos, EndPos: Int64): TMD5;
{$IF Defined(FastMD5) and Defined(Delphi) and (Defined(WIN32) or Defined(WIN64))}
begin
  Result := FastMD5(stream, StartPos, EndPos);
end;
{$ELSE}


const
  deltaSize: Cardinal = $40 * $FFFF;

var
  Digest: TMD5;
  Lo, Hi: Cardinal;
  DeltaBuf: Pointer;
  bufSiz: Int64;
  Rest: Cardinal;
  p: PByte;
  ChunkIndex: Byte;
  ChunkBuff: array [0 .. 63] of Byte;
begin
  if StartPos > EndPos then
      Swap(StartPos, EndPos);
  StartPos := umlClamp(StartPos, 0, stream.Size);
  EndPos := umlClamp(EndPos, 0, stream.Size);
  if EndPos - StartPos <= 0 then
    begin
      Result := umlMD5(nil, 0);
      exit;
    end;
{$IFDEF OptimizationMemoryStreamMD5}
  if stream is TCore_MemoryStream then
    begin
      Result := umlMD5(GetOffset(TCore_MemoryStream(stream).Memory, StartPos), EndPos - StartPos);
      exit;
    end;
  if stream is TMS64 then
    begin
      Result := umlMD5(TMS64(stream).PositionAsPtr(StartPos), EndPos - StartPos);
      exit;
    end;
{$ENDIF OptimizationMemoryStreamMD5}
  //

  Lo := 0;
  Hi := 0;
  PCardinal(@Digest[0])^ := $67452301;
  PCardinal(@Digest[4])^ := $EFCDAB89;
  PCardinal(@Digest[8])^ := $98BADCFE;
  PCardinal(@Digest[12])^ := $10325476;

  bufSiz := EndPos - StartPos;
  Rest := 0;

  inc(Lo, bufSiz shl 3);
  inc(Hi, bufSiz shr 29);

  DeltaBuf := GetMemory(deltaSize);
  stream.Position := StartPos;

  if bufSiz < $40 then
    begin
      if stream.Read(DeltaBuf^, bufSiz) <> bufSiz then
        begin
          FreeMemory(DeltaBuf);
          exit(NullMD5);
        end;
      p := DeltaBuf;
    end
  else
    while bufSiz >= $40 do
      begin
        if Rest = 0 then
          begin
            if bufSiz >= deltaSize then
                Rest := deltaSize
            else
                Rest := bufSiz;
            if stream.Read(DeltaBuf^, Rest) <> Rest then
              begin
                FreeMemory(DeltaBuf);
                exit(NullMD5);
              end;

            p := DeltaBuf;
          end;
        umlTransformMD5(Digest, p^);
        inc(p, $40);
        dec(bufSiz, $40);
        dec(Rest, $40);
      end;

  if bufSiz > 0 then
      CopyPtr(p, @ChunkBuff[0], bufSiz);

  FreeMemory(DeltaBuf);

  Result := PMD5(@Digest[0])^;
  ChunkBuff[bufSiz] := $80;
  ChunkIndex := bufSiz + 1;
  if ChunkIndex > $38 then
    begin
      if ChunkIndex < $40 then
          FillPtrByte(@ChunkBuff[ChunkIndex], $40 - ChunkIndex, 0);
      umlTransformMD5(Result, ChunkBuff);
      ChunkIndex := 0
    end;
  FillPtrByte(@ChunkBuff[ChunkIndex], $38 - ChunkIndex, 0);
  PCardinal(@ChunkBuff[$38])^ := Lo;
  PCardinal(@ChunkBuff[$3C])^ := Hi;
  umlTransformMD5(Result, ChunkBuff);
end;
{$ENDIF}


function umlStreamMD5(stream: TCore_Stream): TMD5;
begin
  if stream.Size <= 0 then
    begin
      Result := NullMD5;
      exit;
    end;
  stream.Position := 0;
  Result := umlStreamMD5(stream, 0, stream.Size);
  stream.Position := 0;
end;

function umlStreamMD5Char(stream: TCore_Stream): TPascalString;
begin
  Result := umlMD5ToStr(umlStreamMD5(stream));
end;

function umlStreamMD5String(stream: TCore_Stream): TPascalString;
begin
  Result := umlMD5ToStr(umlStreamMD5(stream));
end;

function umlStreamMD5Str(stream: TCore_Stream): TPascalString;
begin
  Result := umlMD5ToStr(umlStreamMD5(stream));
end;

function umlStringMD5(const Value: TPascalString): TPascalString;
var
  B: TBytes;
begin
  B := umlBytesOf(Value);
  Result := umlMD5ToStr(umlMD5(@B[0], length(B)));
end;

function umlFileMD5___(FileName: TPascalString): TMD5;
var
  fs: TCore_FileStream;
begin
  try
      fs := TCore_FileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  except
    Result := NullMD5;
    exit;
  end;
  try
      Result := umlStreamMD5(fs);
  finally
      DisposeObject(fs);
  end;
end;

function umlFileMD5(FileName: TPascalString; StartPos, EndPos: Int64): TMD5;
var
  fs: TCore_FileStream;
begin
  if not umlFileExists(FileName) then
    begin
      Result := NullMD5;
      exit;
    end;

  if (StartPos = 0) and (EndPos = umlGetFileSize(FileName)) then
    begin
      Result := umlFileMD5(FileName);
      exit;
    end;

  try
      fs := TCore_FileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  except
    Result := NullMD5;
    exit;
  end;
  try
      Result := umlStreamMD5(fs, StartPos, EndPos);
  finally
      DisposeObject(fs);
  end;
end;

function umlCombineMD5(const m1: TMD5): TMD5;
begin
  Result := umlMD5(@m1, SizeOf(TMD5));
end;

function umlCombineMD5(const m1, m2: TMD5): TMD5;
var
  buff: array [0 .. 1] of TMD5;
begin
  buff[0] := m1;
  buff[1] := m2;
  Result := umlMD5(@buff[0], SizeOf(TMD5) * 2);
end;

function umlCombineMD5(const m1, m2, m3: TMD5): TMD5;
var
  buff: array [0 .. 2] of TMD5;
begin
  buff[0] := m1;
  buff[1] := m2;
  buff[2] := m3;
  Result := umlMD5(@buff[0], SizeOf(TMD5) * 3);
end;

function umlCombineMD5(const m1, m2, m3, m4: TMD5): TMD5;
var
  buff: array [0 .. 3] of TMD5;
begin
  buff[0] := m1;
  buff[1] := m2;
  buff[2] := m3;
  buff[3] := m4;
  Result := umlMD5(@buff[0], SizeOf(TMD5) * 4);
end;

function umlCombineMD5(const buff: array of TMD5): TMD5;
begin
  Result := umlMD5(@buff[0], length(buff) * SizeOf(TMD5));
end;

function umlMD5ToStr(md5: TMD5): TPascalString;
const
  HexArr: array [0 .. 15] of U_Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  i: Integer;
begin
  Result.L := 32;
  for i := 0 to 15 do
    begin
      Result.buff[i * 2] := HexArr[(md5[i] shr 4) and $0F];
      Result.buff[i * 2 + 1] := HexArr[md5[i] and $0F];
    end;
end;

function umlMD5ToStr(const buffPtr: PByte; bufSiz: NativeUInt): TPascalString;
begin
  Result := umlMD5ToStr(umlMD5(buffPtr, bufSiz));
end;

function umlMD5ToString(md5: TMD5): TPascalString;
const
  HexArr: array [0 .. 15] of U_Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  i: Integer;
begin
  Result.L := 32;
  for i := 0 to 15 do
    begin
      Result.buff[i * 2] := HexArr[(md5[i] shr 4) and $0F];
      Result.buff[i * 2 + 1] := HexArr[md5[i] and $0F];
    end;
end;

function umlMD5ToString(const buffPtr: PByte; bufSiz: NativeUInt): TPascalString;
begin
  Result := umlMD5ToString(umlMD5(buffPtr, bufSiz));
end;

function umlMD52String(md5: TMD5): TPascalString;
const
  HexArr: array [0 .. 15] of U_Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  i: Integer;
begin
  Result.L := 32;
  for i := 0 to 15 do
    begin
      Result.buff[i * 2] := HexArr[(md5[i] shr 4) and $0F];
      Result.buff[i * 2 + 1] := HexArr[md5[i] and $0F];
    end;
end;

function umlMD5Compare(const m1, m2: TMD5): Boolean;
begin
  Result := (PUInt64(@m1[0])^ = PUInt64(@m2[0])^) and (PUInt64(@m1[8])^ = PUInt64(@m2[8])^);
end;

function umlCompareMD5(const m1, m2: TMD5): Boolean;
begin
  Result := (PUInt64(@m1[0])^ = PUInt64(@m2[0])^) and (PUInt64(@m1[8])^ = PUInt64(@m2[8])^);
end;

function umlIsNullMD5(m: TMD5): Boolean;
begin
  Result := umlCompareMD5(m, NullMD5);
end;

function umlWasNullMD5(m: TMD5): Boolean;
begin
  Result := umlCompareMD5(m, NullMD5);
end;

function umlCRC16(const Value: PByte; const Count: NativeUInt): Word;
var
  i: NativeUInt;
  p: PByte;
begin
  p := Value;
  Result := 0;
  i := 0;
  while i < Count do
    begin
      Result := (Result shr 8) xor CRC16Table[p^ xor (Result and $00FF)];
      inc(i);
      inc(p);
    end;
end;

function umlStringCRC16(const Value: TPascalString): Word;
var
  B: TBytes;
begin
  B := umlBytesOf(Value);
  Result := umlCRC16(@B[0], length(B));
end;

function umlStreamCRC16(stream: U_Stream; StartPos, EndPos: Int64): Word;
const
  ChunkSize = 1024 * 1024;
  procedure CRC16BUpdate(var crc: Word; const Buf: Pointer; L: NativeUInt);
  var
    p: PByte;
    i: Integer;
  begin
    p := Buf;
    i := 0;
    while i < L do
      begin
        crc := (crc shr 8) xor CRC16Table[p^ xor (crc and $00FF)];
        inc(p);
        inc(i);
      end;
  end;

var
  j: NativeUInt;
  Num: NativeUInt;
  Rest: NativeUInt;
  Buf: Pointer;
  FSize: Int64;
begin
  if stream is TCore_MemoryStream then
    begin
      Result := umlCRC16(GetOffset(TCore_MemoryStream(stream).Memory, StartPos), EndPos - StartPos);
      exit;
    end;
  if stream is TMS64 then
    begin
      Result := umlCRC16(TMS64(stream).PositionAsPtr(StartPos), EndPos - StartPos);
      exit;
    end;
  { Allocate buffer to read file }
  Buf := GetMemory(ChunkSize);
  { Initialize CRC }
  Result := 0;

  { V1.03 calculate how much of the file we are processing }
  FSize := stream.Size;
  if (StartPos >= FSize) then
      StartPos := 0;
  if (EndPos > FSize) or (EndPos = 0) then
      EndPos := FSize;

  { Calculate number of full chunks that will fit into the buffer }
  Num := EndPos div ChunkSize;
  { Calculate remaining bytes }
  Rest := EndPos mod ChunkSize;

  { Set the stream to the beginning of the file }
  stream.Position := StartPos;

  { Process full chunks }
  for j := 0 to Num - 1 do begin
      stream.Read(Buf^, ChunkSize);
      CRC16BUpdate(Result, Buf, ChunkSize);
    end;

  { Process remaining bytes }
  if Rest > 0 then begin
      stream.Read(Buf^, Rest);
      CRC16BUpdate(Result, Buf, Rest);
    end;

  FreeMem(Buf, ChunkSize);
end;

function umlStreamCRC16(stream: U_Stream): Word;
begin
  stream.Position := 0;
  Result := umlStreamCRC16(stream, 0, stream.Size);
  stream.Position := 0;
end;

function umlCRC32(const Value: PByte; const Count: NativeUInt): Cardinal;
var
  i: NativeUInt;
  p: PByte;
begin
  p := Value;
  Result := $FFFFFFFF;
  i := 0;
  while i < Count do
    begin
      Result := ((Result shr 8) and $00FFFFFF) xor C_CRC32Table[(Result xor p^) and $000000FF];
      inc(i);
      inc(p);
    end;
  Result := Result xor $FFFFFFFF;
end;

function umlString2CRC32(const Value: TPascalString): Cardinal;
var
  B: TBytes;
begin
  B := umlBytesOf(Value);
  Result := umlCRC32(@B[0], length(B));
end;

function umlStreamCRC32(stream: U_Stream; StartPos, EndPos: Int64): Cardinal;
const
  ChunkSize = 1024 * 1024;

  procedure CRC32BUpdate(var crc: Cardinal; const Buf: Pointer; L: NativeUInt);
  var
    p: PByte;
    i: Integer;
  begin
    p := Buf;
    i := 0;
    while i < L do
      begin
        crc := ((crc shr 8) and $00FFFFFF) xor C_CRC32Table[(crc xor p^) and $000000FF];
        inc(p);
        inc(i);
      end;
  end;

var
  j: NativeUInt;
  Num: NativeUInt;
  Rest: NativeUInt;
  Buf: Pointer;
  FSize: Int64;
begin
  if stream is TCore_MemoryStream then
    begin
      Result := umlCRC32(GetOffset(TCore_MemoryStream(stream).Memory, StartPos), EndPos - StartPos);
      exit;
    end;
  if stream is TMS64 then
    begin
      Result := umlCRC32(TMS64(stream).PositionAsPtr(StartPos), EndPos - StartPos);
      exit;
    end;

  { Allocate buffer to read file }
  Buf := GetMemory(ChunkSize);

  { Initialize CRC }
  Result := $FFFFFFFF;

  { V1.03 calculate how much of the file we are processing }
  FSize := stream.Size;
  if (StartPos >= FSize) then
      StartPos := 0;
  if (EndPos > FSize) or (EndPos = 0) then
      EndPos := FSize;

  { Calculate number of full chunks that will fit into the buffer }
  Num := EndPos div ChunkSize;
  { Calculate remaining bytes }
  Rest := EndPos mod ChunkSize;

  { Set the stream to the beginning of the file }
  stream.Position := StartPos;

  { Process full chunks }
  for j := 0 to Num - 1 do begin
      stream.Read(Buf^, ChunkSize);
      CRC32BUpdate(Result, Buf, ChunkSize);
    end;

  { Process remaining bytes }
  if Rest > 0 then begin
      stream.Read(Buf^, Rest);
      CRC32BUpdate(Result, Buf, Rest);
    end;

  FreeMem(Buf, ChunkSize);

  Result := Result xor $FFFFFFFF;
end;

function umlStreamCRC32(stream: U_Stream): Cardinal;
begin
  stream.Position := 0;
  Result := umlStreamCRC32(stream, 0, stream.Size);
  stream.Position := 0;
end;

function umlTrimSpace(const s: TPascalString): TPascalString;
var
  L, BP, EP: Integer;
begin
  Result := '';
  L := s.L;
  if L > 0 then
    begin
      BP := 1;
      while CharIn(s[BP], [#32, #0]) do
        begin
          inc(BP);
          if (BP > L) then
            begin
              Result := '';
              exit;
            end;
        end;
      if BP > L then
          Result := ''
      else
        begin
          EP := L;

          while CharIn(s[EP], [#32, #0]) do
            begin
              dec(EP);
              if (EP < 1) then
                begin
                  Result := '';
                  exit;
                end;
            end;
          Result := s.GetString(BP, EP + 1);
        end;
    end;
end;

function umlSeparatorText(Text_: TPascalString; dest: TCore_Strings; SeparatorChar: TPascalString): Integer;
var
  NewText_, SeparatorText_: TPascalString;
begin
  Result := 0;
  if Assigned(dest) then
    begin
      NewText_ := Text_;
      SeparatorText_ := umlGetFirstStr(NewText_, SeparatorChar);
      while (SeparatorText_.L > 0) and (NewText_.L > 0) do
        begin
          dest.Add(SeparatorText_.text);
          inc(Result);
          NewText_ := umlDeleteFirstStr(NewText_, SeparatorChar);
          SeparatorText_ := umlGetFirstStr(NewText_, SeparatorChar);
        end;
    end;
end;

function umlSeparatorText(Text_: TPascalString; dest: THashVariantList; SeparatorChar: TPascalString): Integer;
var
  NewText_, SeparatorText_: TPascalString;
begin
  Result := 0;
  if Assigned(dest) then
    begin
      NewText_ := Text_;
      SeparatorText_ := umlGetFirstStr(NewText_, SeparatorChar);
      while (SeparatorText_.L > 0) and (NewText_.L > 0) do
        begin
          dest.IncValue(SeparatorText_.text, 1);
          inc(Result);
          NewText_ := umlDeleteFirstStr(NewText_, SeparatorChar);
          SeparatorText_ := umlGetFirstStr(NewText_, SeparatorChar);
        end;
    end;
end;

function umlSeparatorText(Text_: TPascalString; dest: TListPascalString; SeparatorChar: TPascalString): Integer;
var
  NewText_, SeparatorText_: TPascalString;
begin
  Result := 0;
  if Assigned(dest) then
    begin
      NewText_ := Text_;
      SeparatorText_ := umlGetFirstStr(NewText_, SeparatorChar);
      while (SeparatorText_.L > 0) and (NewText_.L > 0) do
        begin
          dest.Add(SeparatorText_);
          inc(Result);
          NewText_ := umlDeleteFirstStr(NewText_, SeparatorChar);
          SeparatorText_ := umlGetFirstStr(NewText_, SeparatorChar);
        end;
    end;
end;

function umlStringsMatchText(OriginValue: TCore_Strings; DestValue: TPascalString; IgnoreCase: Boolean): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Assigned(OriginValue) then
      exit;
  if OriginValue.Count > 0 then
    begin
      for i := 0 to OriginValue.Count - 1 do
        begin
          if umlMultipleMatch(IgnoreCase, OriginValue[i], DestValue) then
            begin
              Result := True;
              exit;
            end;
        end;
    end;
end;

function umlStringsInExists(dest: TListPascalString; SText: TPascalString; IgnoreCase: Boolean): Boolean;
var
  i: Integer;
  ns: TPascalString;
begin
  Result := False;
  if IgnoreCase then
      ns := umlUpperCase(SText)
  else
      ns := SText;
  if Assigned(dest) then
    begin
      if dest.Count > 0 then
        begin
          for i := 0 to dest.Count - 1 do
            begin
              if ((not IgnoreCase) and (SText = dest[i])) or ((IgnoreCase) and (umlSameText(SText, dest[i]))) then
                begin
                  Result := True;
                  exit;
                end;
            end;
        end;
    end;
end;

function umlStringsInExists(dest: TCore_Strings; SText: TPascalString; IgnoreCase: Boolean): Boolean;
var
  i: Integer;
  ns: TPascalString;
begin
  Result := False;
  if IgnoreCase then
      ns := umlUpperCase(SText)
  else
      ns := SText;
  if Assigned(dest) then
    begin
      if dest.Count > 0 then
        begin
          for i := 0 to dest.Count - 1 do
            begin
              if ((not IgnoreCase) and (SText = dest[i])) or ((IgnoreCase) and (umlSameText(SText, dest[i]))) then
                begin
                  Result := True;
                  exit;
                end;
            end;
        end;
    end;
end;

function umlStringsInExists(dest: TCore_Strings; SText: TPascalString): Boolean;
begin
  Result := umlStringsInExists(dest, SText, True);
end;

function umlTextInStrings(const SText: TPascalString; dest: TListPascalString; IgnoreCase: Boolean): Boolean;
begin
  Result := umlStringsInExists(dest, SText, IgnoreCase);
end;

function umlTextInStrings(const SText: TPascalString; dest: TCore_Strings; IgnoreCase: Boolean): Boolean;
begin
  Result := umlStringsInExists(dest, SText, IgnoreCase);
end;

function umlTextInStrings(const SText: TPascalString; dest: TCore_Strings): Boolean;
begin
  Result := umlStringsInExists(dest, SText);
end;

function umlAddNewStrTo(source: TPascalString; dest: TListPascalString; IgnoreCase: Boolean): Boolean;
begin
  Result := not umlStringsInExists(dest, source, IgnoreCase);
  if Result then
      dest.Append(source.text);
end;

function umlAddNewStrTo(source: TPascalString; dest: TCore_Strings; IgnoreCase: Boolean): Boolean;
begin
  Result := not umlStringsInExists(dest, source, IgnoreCase);
  if Result then
      dest.Append(source.text);
end;

function umlAddNewStrTo(source: TPascalString; dest: TCore_Strings): Boolean;
begin
  Result := not umlStringsInExists(dest, source, True);
  if Result then
      dest.Append(source.text);
end;

function umlAddNewStrTo(source, dest: TCore_Strings): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to source.Count - 1 do
    if umlAddNewStrTo(source[i], dest) then
        inc(Result);
end;

function umlDeleteStrings(const SText: TPascalString; dest: TCore_Strings; IgnoreCase: Boolean): Integer;
var
  i: Integer;
begin
  Result := 0;
  if Assigned(dest) then
    begin
      if dest.Count > 0 then
        begin
          i := 0;
          while i < dest.Count do
            begin
              if ((not IgnoreCase) and (SText = dest[i])) or ((IgnoreCase) and (umlMultipleMatch(IgnoreCase, SText, dest[i]))) then
                begin
                  dest.Delete(i);
                  inc(Result);
                end
              else
                  inc(i);
            end;
        end;
    end;
end;

function umlDeleteStringsNot(const SText: TPascalString; dest: TCore_Strings; IgnoreCase: Boolean): Integer;
var
  i: Integer;
begin
  Result := 0;
  if Assigned(dest) then
    begin
      if dest.Count > 0 then
        begin
          i := 0;
          while i < dest.Count do
            begin
              if ((not IgnoreCase) and (SText <> dest[i])) or ((IgnoreCase) and (not umlMultipleMatch(IgnoreCase, SText, dest[i]))) then
                begin
                  dest.Delete(i);
                  inc(Result);
                end
              else
                  inc(i);
            end;
        end;
    end;
end;

function umlMergeStrings(source, dest: TCore_Strings; IgnoreCase: Boolean): Integer;
var
  i: Integer;
begin
  Result := 0;
  if (source = nil) or (dest = nil) then
      exit;
  if source.Count > 0 then
    begin
      for i := 0 to source.Count - 1 do
        begin
          umlAddNewStrTo(source[i], dest, IgnoreCase);
          inc(Result);
        end;
    end;
end;

function umlMergeStrings(source, dest: TListPascalString; IgnoreCase: Boolean): Integer;
var
  i: Integer;
begin
  Result := 0;
  if (source = nil) or (dest = nil) then
      exit;
  if source.Count > 0 then
    begin
      for i := 0 to source.Count - 1 do
        begin
          umlAddNewStrTo(source[i], dest, IgnoreCase);
          inc(Result);
        end;
    end;
end;

function umlConverStrToFileName(const Value: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := Value;
  for i := 1 to umlGetLength(Result) do
    begin
      if CharIn(Result[i], '":;/\|<>?*%') then
          Result[i] := ' ';
    end;
end;

function umlSplitTextMatch(const SText, Limit, MatchText: TPascalString; IgnoreCase: Boolean): Boolean;
var
  n, t: TPascalString;
begin
  Result := True;
  if MatchText = '' then
      exit;
  n := SText;
  //
  if umlExistsChar(n, Limit) then
    begin
      repeat
        t := umlGetFirstStr(n, Limit);
        if umlMultipleMatch(IgnoreCase, MatchText, t) then
            exit;
        n := umlDeleteFirstStr(n, Limit);
      until n = '';
    end
  else
    begin
      t := n;
      if umlMultipleMatch(IgnoreCase, MatchText, t) then
          exit;
    end;
  //
  Result := False;
end;

function umlSplitTextTrimSpaceMatch(const SText, Limit, MatchText: TPascalString; IgnoreCase: Boolean): Boolean;
var
  n, t: TPascalString;
begin
  Result := True;
  if MatchText = '' then
      exit;
  n := SText;

  if umlExistsChar(n, Limit) then
    begin
      repeat
        t := umlTrimSpace(umlGetFirstStr(n, Limit));
        if umlMultipleMatch(IgnoreCase, MatchText, t) then
            exit;
        n := umlDeleteFirstStr(n, Limit);
      until n = '';
    end
  else
    begin
      t := umlTrimSpace(n);
      if umlMultipleMatch(IgnoreCase, MatchText, t) then
          exit;
    end;

  Result := False;
end;

function umlSplitDeleteText(const SText, Limit, MatchText: TPascalString; IgnoreCase: Boolean): TPascalString;
var
  n, t: TPascalString;
begin
  if (MatchText = '') or (Limit = '') then
    begin
      Result := SText;
      exit;
    end;
  Result := '';
  n := SText;
  //
  if umlExistsChar(n, Limit) then
    begin
      repeat
        t := umlGetFirstStr(n, Limit);
        if not umlMultipleMatch(IgnoreCase, MatchText, t) then
          begin
            if Result <> '' then
                Result := Result + Limit[1] + t
            else
                Result := t;
          end;
        n := umlDeleteFirstStr(n, Limit);
      until n = '';
    end
  else
    begin
      t := n;
      if not umlMultipleMatch(IgnoreCase, MatchText, t) then
          Result := SText;
    end;
end;

function umlSplitTextAsList(const SText, Limit: TPascalString; AsLst: TCore_Strings): Boolean;
var
  n, t: TPascalString;
begin
  AsLst.Clear;
  n := SText;
  //
  if umlExistsChar(n, Limit) then
    begin
      repeat
        t := umlGetFirstStr(n, Limit);
        AsLst.Append(t.text);
        n := umlDeleteFirstStr(n, Limit);
      until n = '';
    end
  else
    begin
      t := n;
      if umlGetLength(t) > 0 then
          AsLst.Append(t.text);
    end;
  //
  Result := AsLst.Count > 0;
end;

function umlSplitTextAsListAndTrimSpace(const SText, Limit: TPascalString; AsLst: TCore_Strings): Boolean;
var
  n, t: TPascalString;
begin
  AsLst.Clear;
  n := SText;
  //
  if umlExistsChar(n, Limit) then
    begin
      repeat
        t := umlGetFirstStr(n, Limit);
        AsLst.Append(umlTrimSpace(t).text);
        n := umlDeleteFirstStr(n, Limit);
      until n = '';
    end
  else
    begin
      t := n;
      if umlGetLength(t) > 0 then
          AsLst.Append(umlTrimSpace(t).text);
    end;
  //
  Result := AsLst.Count > 0;
end;

function umlListAsSplitText(const List: TCore_Strings; Limit: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to List.Count - 1 do
    if Result = '' then
        Result := List[i]
    else
        Result := Result + Limit + List[i];
end;

function umlListAsSplitText(const List: TListPascalString; Limit: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to List.Count - 1 do
    if Result = '' then
        Result := List[i]
    else
        Result.Append(Limit + List[i]);
end;

function umlDivisionText(const buffer: TPascalString; width: Integer; DivisionAsPascalString: Boolean): TPascalString;
var
  i, n: Integer;
begin
  Result := '';
  n := 0;
  for i := 1 to buffer.L do
    begin
      if (DivisionAsPascalString) and (n = 0) then
          Result.Append(#39);

      Result.Append(buffer[i]);
      inc(n);
      if n = width then
        begin
          if DivisionAsPascalString then
              Result.Append(#39 + '+' + #13#10)
          else
              Result.Append(#13#10);
          n := 0;
        end;
    end;
  if DivisionAsPascalString then
      Result.Append(#39);
end;

function umlUpdateComponentName(const Name: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to umlGetLength(name) do
    if umlGetLength(Result) > 0 then
      begin
        if CharIn(name[i], [c0to9, cLoAtoZ, cHiAtoZ], '-') then
            Result := Result + name[i];
      end
    else if CharIn(name[i], [cLoAtoZ, cHiAtoZ]) then
        Result := Result + name[i];
end;

function umlMakeComponentName(Owner: TCore_Component; RefrenceName: TPascalString): TPascalString;
var
  c: Cardinal;
begin
  c := 1;
  RefrenceName := umlUpdateComponentName(RefrenceName);
  Result := RefrenceName;
  while Owner.FindComponent(Result.text) <> nil do
    begin
      Result := RefrenceName + IntToStr(c);
      inc(c);
    end;
end;

procedure umlReadComponent(stream: TCore_Stream; comp: TCore_Component);
var
  r: TCore_Reader;
  needClearName: Boolean;
begin
  r := TCore_Reader.Create(stream, 4096);
  r.IgnoreChildren := True;
  try
    needClearName := (comp.Name = '');
    r.ReadRootComponent(comp);
    if needClearName then
        comp.Name := '';
  except
  end;
  DisposeObject(r);
end;

procedure umlWriteComponent(stream: TCore_Stream; comp: TCore_Component);
var
  w: TCore_Writer;
begin
  w := TCore_Writer.Create(stream, 4096);
  w.IgnoreChildren := True;
  w.WriteDescendent(comp, nil);
  DisposeObject(w);
end;

procedure umlCopyComponentDataTo(comp, copyto: TCore_Component);
var
  ms: TCore_MemoryStream;
begin
  if comp.ClassType <> copyto.ClassType then
      exit;
  ms := TCore_MemoryStream.Create;
  try
    umlWriteComponent(ms, comp);
    ms.Position := 0;
    umlReadComponent(ms, copyto);
  except
  end;
  DisposeObject(ms);
end;

function umlProcessCycleValue(CurrentVal, DeltaVal, StartVal, OverVal: Single; var EndFlag: Boolean): Single;
  function IfOut(Cur, Delta, dest: Single): Boolean;
  begin
    if Cur > dest then
        Result := Cur - Delta < dest
    else
        Result := Cur + Delta > dest;
  end;

  function GetOutValue(Cur, Delta, dest: Single): Single;
  begin
    if IfOut(Cur, Delta, dest) then
      begin
        if Cur > dest then
            Result := dest - (Cur - Delta)
        else
            Result := Cur + Delta - dest;
      end
    else
        Result := 0;
  end;

  function GetDeltaValue(Cur, Delta, dest: Single): Single;
  begin
    if Cur > dest then
        Result := Cur - Delta
    else
        Result := Cur + Delta;
  end;

begin
  if (DeltaVal > 0) and (StartVal <> OverVal) then
    begin
      if EndFlag then
        begin
          if IfOut(CurrentVal, DeltaVal, OverVal) then
            begin
              EndFlag := False;
              Result := umlProcessCycleValue(OverVal, GetOutValue(CurrentVal, DeltaVal, OverVal), StartVal, OverVal, EndFlag);
            end
          else
              Result := GetDeltaValue(CurrentVal, DeltaVal, OverVal);
        end
      else
        begin
          if IfOut(CurrentVal, DeltaVal, StartVal) then
            begin
              EndFlag := True;
              Result := umlProcessCycleValue(StartVal, GetOutValue(CurrentVal, DeltaVal, StartVal), StartVal, OverVal, EndFlag);
            end
          else
              Result := GetDeltaValue(CurrentVal, DeltaVal, StartVal);
        end
    end
  else
      Result := CurrentVal;
end;

procedure ImportCSV_C(const sour: TArrayPascalString; OnNotify: TCSVSave_C);
var
  i, j, BP, hc: NativeInt;
  n: TPascalString;
  king, buff: TArrayPascalString;
begin
  // csv head
  BP := -1;
  for i := low(sour) to high(sour) do
    begin
      n := sour[i];
      if n.L <> 0 then
        begin
          BP := i + 1;
          hc := n.GetCharCount(',') + 1;
          SetLength(buff, hc);
          SetLength(king, hc);

          for j := low(king) to high(king) do
              king[j] := '';
          j := 0;
          while (j < length(king)) and (n.L > 0) do
            begin
              king[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;

          break;
        end;
    end;

  // csv body
  if BP > 0 then
    for i := BP to high(sour) do
      begin
        n := sour[i];
        if n.L > 0 then
          begin
            for j := low(buff) to high(buff) do
                buff[j] := '';
            j := 0;
            while (j < length(buff)) and (n.L > 0) do
              begin
                buff[j] := umlGetFirstStr_Discontinuity(n, ',');
                n := umlDeleteFirstStr_Discontinuity(n, ',');
                inc(j);
              end;
            OnNotify(sour[i], king, buff);
          end;
      end;

  SetLength(buff, 0);
  SetLength(king, 0);
  n := '';
end;

procedure CustomImportCSV_C(const OnGetLine: TCSVGetLine_C; OnNotify: TCSVSave_C);
var
  IsEnd: Boolean;
  i, j, hc: NativeInt;
  n, s: TPascalString;
  king, buff: TArrayPascalString;
begin
  // csv head
  while True do
    begin
      IsEnd := False;
      n := '';
      OnGetLine(n, IsEnd);
      if IsEnd then
          exit;
      if n.L <> 0 then
        begin
          hc := n.GetCharCount(',') + 1;
          SetLength(buff, hc);
          SetLength(king, hc);

          for j := low(king) to high(king) do
              king[j] := '';
          j := 0;
          while (j < length(king)) and (n.L > 0) do
            begin
              king[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;

          break;
        end;
    end;

  // csv body
  while True do
    begin
      IsEnd := False;
      n := '';
      OnGetLine(n, IsEnd);
      if IsEnd then
          exit;
      if n.L > 0 then
        begin
          s := n;
          for j := low(buff) to high(buff) do
              buff[j] := '';
          j := 0;
          while (j < length(buff)) and (n.L > 0) do
            begin
              buff[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;
          OnNotify(s, king, buff);
        end;
    end;

  SetLength(buff, 0);
  SetLength(king, 0);
  n := '';
end;

procedure ImportCSV_M(const sour: TArrayPascalString; OnNotify: TCSVSave_M);
var
  i, j, BP, hc: NativeInt;
  n: TPascalString;
  king, buff: TArrayPascalString;
begin
  // csv head
  BP := -1;
  for i := low(sour) to high(sour) do
    begin
      n := sour[i];
      if n.L <> 0 then
        begin
          BP := i + 1;
          hc := n.GetCharCount(',') + 1;
          SetLength(buff, hc);
          SetLength(king, hc);

          for j := low(king) to high(king) do
              king[j] := '';
          j := 0;
          while (j < length(king)) and (n.L > 0) do
            begin
              king[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;

          break;
        end;
    end;

  // csv body
  if BP > 0 then
    for i := BP to high(sour) do
      begin
        n := sour[i];
        if n.L > 0 then
          begin
            for j := low(buff) to high(buff) do
                buff[j] := '';
            j := 0;
            while (j < length(buff)) and (n.L > 0) do
              begin
                buff[j] := umlGetFirstStr_Discontinuity(n, ',');
                n := umlDeleteFirstStr_Discontinuity(n, ',');
                inc(j);
              end;
            OnNotify(sour[i], king, buff);
          end;
      end;

  SetLength(buff, 0);
  SetLength(king, 0);
  n := '';
end;

procedure CustomImportCSV_M(const OnGetLine: TCSVGetLine_M; OnNotify: TCSVSave_M);
var
  IsEnd: Boolean;
  i, j, hc: NativeInt;
  n, s: TPascalString;
  king, buff: TArrayPascalString;
begin
  // csv head
  while True do
    begin
      IsEnd := False;
      n := '';
      OnGetLine(n, IsEnd);
      if IsEnd then
          exit;
      if n.L <> 0 then
        begin
          hc := n.GetCharCount(',') + 1;
          SetLength(buff, hc);
          SetLength(king, hc);

          for j := low(king) to high(king) do
              king[j] := '';
          j := 0;
          while (j < length(king)) and (n.L > 0) do
            begin
              king[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;

          break;
        end;
    end;

  // csv body
  while True do
    begin
      IsEnd := False;
      n := '';
      OnGetLine(n, IsEnd);
      if IsEnd then
          exit;
      if n.L > 0 then
        begin
          s := n;
          for j := low(buff) to high(buff) do
              buff[j] := '';
          j := 0;
          while (j < length(buff)) and (n.L > 0) do
            begin
              buff[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;
          OnNotify(s, king, buff);
        end;
    end;

  SetLength(buff, 0);
  SetLength(king, 0);
  n := '';
end;

procedure ImportCSV_P(const sour: TArrayPascalString; OnNotify: TCSVSave_P);
var
  i, j, BP, hc: NativeInt;
  n: TPascalString;
  king, buff: TArrayPascalString;
begin
  // csv head
  BP := -1;
  for i := low(sour) to high(sour) do
    begin
      n := sour[i];
      if n.L <> 0 then
        begin
          BP := i + 1;
          hc := n.GetCharCount(',') + 1;
          SetLength(buff, hc);
          SetLength(king, hc);

          for j := low(king) to high(king) do
              king[j] := '';
          j := 0;
          while (j < length(king)) and (n.L > 0) do
            begin
              king[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;

          break;
        end;
    end;

  // csv body
  if BP > 0 then
    for i := BP to high(sour) do
      begin
        n := sour[i];
        if n.L > 0 then
          begin
            for j := low(buff) to high(buff) do
                buff[j] := '';
            j := 0;
            while (j < length(buff)) and (n.L > 0) do
              begin
                buff[j] := umlGetFirstStr_Discontinuity(n, ',');
                n := umlDeleteFirstStr_Discontinuity(n, ',');
                inc(j);
              end;
            OnNotify(sour[i], king, buff);
          end;
      end;

  SetLength(buff, 0);
  SetLength(king, 0);
  n := '';
end;

procedure CustomImportCSV_P(const OnGetLine: TCSVGetLine_P; OnNotify: TCSVSave_P);
var
  IsEnd: Boolean;
  i, j, hc: NativeInt;
  n, s: TPascalString;
  king, buff: TArrayPascalString;
begin
  // csv head
  while True do
    begin
      IsEnd := False;
      n := '';
      OnGetLine(n, IsEnd);
      if IsEnd then
          exit;
      if n.L <> 0 then
        begin
          hc := n.GetCharCount(',') + 1;
          SetLength(buff, hc);
          SetLength(king, hc);

          for j := low(king) to high(king) do
              king[j] := '';
          j := 0;
          while (j < length(king)) and (n.L > 0) do
            begin
              king[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;

          break;
        end;
    end;

  // csv body
  while True do
    begin
      IsEnd := False;
      n := '';
      OnGetLine(n, IsEnd);
      if IsEnd then
          exit;
      if n.L > 0 then
        begin
          s := n;
          for j := low(buff) to high(buff) do
              buff[j] := '';
          j := 0;
          while (j < length(buff)) and (n.L > 0) do
            begin
              buff[j] := umlGetFirstStr_Discontinuity(n, ',');
              n := umlDeleteFirstStr_Discontinuity(n, ',');
              inc(j);
            end;
          OnNotify(s, king, buff);
        end;
    end;

  SetLength(buff, 0);
  SetLength(king, 0);
  n := '';
end;

var
  ExLibs: THashVariantList = nil;

function GetExtLib(LibName: SystemString): HMODULE;
begin
  Result := 0;
{$IF not(Defined(IOS) and Defined(CPUARM))}
  if ExLibs = nil then
      ExLibs := THashVariantList.Create;
  if not ExLibs.Exists(LibName) then
    begin
      try
{$IFNDEF FPC}
{$IFDEF ANDROID}
        Result := LoadLibrary(PChar(umlCombineFileName(System.IOUtils.TPath.GetLibraryPath, LibName).text));
{$ELSE ANDROID}
        Result := LoadLibrary(PChar(LibName));
{$ENDIF ANDROID}
{$ELSE FPC}
        Result := LoadLibrary(PChar(LibName));
{$ENDIF FPC}
        ExLibs.Add(LibName, Result);
      except
        FreeExtLib(LibName);
        Result := 0;
      end;
    end
  else
      Result := ExLibs[LibName];
{$ENDIF}
end;

function FreeExtLib(LibName: SystemString): Boolean;
begin
  Result := False;
{$IF not(Defined(IOS) and Defined(CPUARM))}
  if ExLibs = nil then
      ExLibs := THashVariantList.Create;
  if ExLibs.Exists(LibName) then
    begin
      try
          FreeLibrary(HMODULE(ExLibs[LibName]));
      except
      end;
      ExLibs.Delete(LibName);
      Result := True;
    end;
{$ENDIF}
end;

function GetExtProc(const LibName, ProcName: SystemString): Pointer;
{$IF not(Defined(IOS) and Defined(CPUARM))}
var
  h: HMODULE;
{$ENDIF}
begin
  Result := nil;
{$IF not(Defined(IOS) and Defined(CPUARM))}
  h := GetExtLib(LibName);
  if h <> 0 then
    begin
      Result := GetProcAddress(h, PChar(ProcName));
      if Result = nil then
          DoStatus('error external libray: %s - %s', [LibName, ProcName]);
    end;
{$ENDIF}
end;

{$IFDEF RangeCheck}{$R-}{$ENDIF}


function umlCompareByteString(const s1: TPascalString; const s2: PArrayRawByte): Boolean;
var
  tmp: TBytes;
  i: Integer;
begin
  SetLength(tmp, s1.L);
  for i := 0 to s1.L - 1 do
      tmp[i] := Byte(s1.buff[i]);

  Result := CompareMemory(@tmp[0], @s2^[0], s1.L);
end;

function umlCompareByteString(const s2: PArrayRawByte; const s1: TPascalString): Boolean;
var
  tmp: TBytes;
  i: Integer;
begin
  SetLength(tmp, s1.L);
  for i := 0 to s1.L - 1 do
      tmp[i] := Byte(s1.buff[i]);

  Result := CompareMemory(@tmp[0], @s2^[0], s1.L);
end;

procedure umlSetByteString(const sour: TPascalString; const dest: PArrayRawByte);
var
  i: Integer;
begin
  for i := 0 to sour.L - 1 do
      dest^[i] := Byte(sour.buff[i]);
end;

procedure umlSetByteString(const dest: PArrayRawByte; const sour: TPascalString);
var
  i: Integer;
begin
  for i := 0 to sour.L - 1 do
      dest^[i] := Byte(sour.buff[i]);
end;

function umlGetByteString(const sour: PArrayRawByte; const L: Integer): TPascalString;
var
  i: Integer;
begin
  Result.L := L;
  for i := 0 to L - 1 do
      Result.buff[i] := SystemChar(sour^[i]);
end;

{$IFDEF RangeCheck}{$R+}{$ENDIF}


procedure SaveMemory(p: Pointer; siz: NativeInt; DestFile: TPascalString);
var
  m64: TMem64;
begin
  m64 := TMem64.Create;
  m64.SetPointerWithProtectedMode(p, siz);
  m64.SaveToFile(DestFile);
  DisposeObject(m64);
end;

type
  TFileMD5_CacheData = record
    Time_: TDateTime;
    Size_: Int64;
    md5: TMD5;
  end;

  PFileMD5_CacheData = ^TFileMD5_CacheData;

  TFileMD5Cache = class
  private
    Critical: TCritical;
    FHash: THashList;
    procedure DoDataFreeProc(p: Pointer);
    function DoGetFileMD5(FileName: U_String): TMD5;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  end;

var
  FileMD5Cache: TFileMD5Cache = nil;

procedure TFileMD5Cache.DoDataFreeProc(p: Pointer);
begin
  Dispose(PFileMD5_CacheData(p));
end;

function TFileMD5Cache.DoGetFileMD5(FileName: U_String): TMD5;
var
  p: PFileMD5_CacheData;
  ft: TDateTime;
  fs: Int64;
begin
  if not umlFileExists(FileName) then
    begin
      Critical.Lock;
      FHash.Delete(FileName);
      Critical.UnLock;
      Result := NullMD5;
      exit;
    end;

  ft := umlGetFileTime(FileName);
  fs := umlGetFileSize(FileName);
  Critical.Lock;
  p := FHash[FileName];
  if p = nil then
    begin
      new(p);
      p^.Time_ := ft;
      p^.Size_ := fs;
      try
          p^.md5 := umlFileMD5___(FileName);
      except
          p^.md5 := Null_Buff_MD5;
      end;
      FHash.Add(FileName, p, False);
      Result := p^.md5;
    end
  else
    begin
      if (ft <> p^.Time_) or (fs <> p^.Size_) then
        begin
          p^.Time_ := ft;
          p^.Size_ := fs;
          try
              p^.md5 := umlFileMD5___(FileName);
          except
              p^.md5 := Null_Buff_MD5;
          end;
        end;
      Result := p^.md5;
    end;
  Critical.UnLock;
end;

constructor TFileMD5Cache.Create;
begin
  inherited Create;
  FHash := THashList.CustomCreate($FFFF);
  FHash.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DoDataFreeProc;
  FHash.IgnoreCase := True;
  FHash.AccessOptimization := True;
  Critical := TCritical.Create;
end;

destructor TFileMD5Cache.Destroy;
begin
  DisposeObject(Critical);
  DisposeObject(FHash);
  inherited Destroy;
end;

procedure TFileMD5Cache.Clear;
begin
  FHash.Clear;
end;

function umlFileMD5(FileName: TPascalString): TMD5;
begin
  Result := FileMD5Cache.DoGetFileMD5(FileName);
end;

procedure Do_ThCacheFileMD5(ThSender: TCompute);
var
  p: PPascalString;
begin
  p := ThSender.UserData;
  FileMD5Cache.DoGetFileMD5(p^);
  Dispose(p);
end;

procedure umlCacheFileMD5(FileName: U_String);
var
  p: PPascalString;
begin
  new(p);
  p^ := FileName;
  TCompute.RunC(p, nil, {$IFDEF FPC}@{$ENDIF FPC}Do_ThCacheFileMD5);
end;

type
  TCacheFileMD5FromDirectoryData_ = record
    Directory_, Filter_: U_String;
  end;

  PCacheFileMD5FromDirectoryData_ = ^TCacheFileMD5FromDirectoryData_;

var
  CacheThreadIsAcivted: Boolean = True;
  CacheFileMD5FromDirectory_Num: Integer = 0;

procedure DoCacheFileMD5FromDirectory(ThSender: TCompute);
var
  p: PCacheFileMD5FromDirectoryData_;
  arry: U_StringArray;
  n: U_SystemString;
begin
  p := ThSender.UserData;
  try
    arry := umlGetFileListWithFullPath(p^.Directory_);
    for n in arry do
      begin
        if umlMultipleMatch(p^.Filter_, umlGetFileName(n)) then
          if umlFileExists(n) then
              FileMD5Cache.DoGetFileMD5(n);
        if not CacheThreadIsAcivted then
            break;
      end;
    SetLength(arry, 0);
  except
  end;
  p^.Directory_ := '';
  p^.Filter_ := '';
  Dispose(p);
  AtomDec(CacheFileMD5FromDirectory_Num);
end;

procedure umlCacheFileMD5FromDirectory(Directory_, Filter_: U_String);
var
  p: PCacheFileMD5FromDirectoryData_;
begin
  AtomInc(CacheFileMD5FromDirectory_Num);
  new(p);
  p^.Directory_ := Directory_;
  p^.Filter_ := Filter_;
  TCompute.RunC(p, nil, {$IFDEF FPC}@{$ENDIF FPC}DoCacheFileMD5FromDirectory);
end;

function umlBinToUInt8(Value: U_String): Byte;
var
  i, Size: Integer;
begin
  Result := 0;
  Size := Value.L;
  for i := Size downto 1 do
    begin
      if Value[i] = '1' then
          Result := Result + (1 shl (Size - i));
    end;
end;

function umlBinToUInt16(Value: U_String): Word;
var
  i, Size: Integer;
begin
  Result := 0;
  Size := Value.L;
  for i := Size downto 1 do
    begin
      if Value[i] = '1' then
          Result := Result + (1 shl (Size - i));
    end;
end;

function umlBinToUInt32(Value: U_String): Cardinal;
var
  i, Size: Integer;
begin
  Result := 0;
  Size := Value.L;
  for i := Size downto 1 do
    begin
      if Value[i] = '1' then
          Result := Result + (1 shl (Size - i));
    end;
end;

function umlBinToUInt64(Value: U_String): UInt64;
var
  i, Size: Integer;
begin
  Result := 0;
  Size := Value.L;
  for i := Size downto 1 do
    begin
      if Value[i] = '1' then
          Result := Result + (1 shl (Size - i));
    end;
end;

function umlUInt8ToBin(v: Byte): U_String;
begin
  if v = 0 then
    begin
      Result := '0';
      exit;
    end;
  Result := '';
  while v > 0 do
    begin
      if v and $1 = 1 then
          Result := '1' + Result
      else
          Result := '0' + Result;
      v := v shr 1;
    end;
  while Result.First = '0' do
      Result.DeleteFirst;
end;

function umlUInt16ToBin(v: Word): U_String;
begin
  if v = 0 then
    begin
      Result := '0';
      exit;
    end;
  Result := '';
  while v > 0 do
    begin
      if v and $1 = 1 then
          Result := '1' + Result
      else
          Result := '0' + Result;
      v := v shr 1;
    end;
  while Result.First = '0' do
      Result.DeleteFirst;
end;

function umlUInt32ToBin(v: Cardinal): U_String;
begin
  if v = 0 then
    begin
      Result := '0';
      exit;
    end;
  Result := '';
  while v > 0 do
    begin
      if v and $1 = 1 then
          Result := '1' + Result
      else
          Result := '0' + Result;
      v := v shr 1;
    end;
  while Result.First = '0' do
      Result.DeleteFirst;
end;

function umlUInt64ToBin(v: UInt64): U_String;
begin
  if v = 0 then
    begin
      Result := '0';
      exit;
    end;
  Result := '';
  while v > 0 do
    begin
      if v and $1 = 1 then
          Result := '1' + Result
      else
          Result := '0' + Result;
      v := v shr 1;
    end;
  while Result.First = '0' do
      Result.DeleteFirst;
end;

function umlBufferIsASCII(buffer: Pointer; siz: NativeUInt): Boolean;
var
  i: NativeInt;
  p: PByte;
begin
  Result := False;
  i := 0;
  p := buffer;
  while i < siz do
    begin
      if p^ > $80 then
          exit;
      inc(p);
      inc(i);
    end;
  Result := True;
end;

initialization

FileMD5Cache := TFileMD5Cache.Create;
CacheFileMD5FromDirectory_Num := 0;
CacheThreadIsAcivted := True;

Lib_DateTimeFormatSettings := FormatSettings;
Lib_DateTimeFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
Lib_DateTimeFormatSettings.LongDateFormat := 'yyyy-MM-dd';
Lib_DateTimeFormatSettings.DateSeparator := '-';
Lib_DateTimeFormatSettings.TimeSeparator := ':';
Lib_DateTimeFormatSettings.DecimalSeparator := '.';
Lib_DateTimeFormatSettings.LongTimeFormat := 'hh:mm:ss.zz';
Lib_DateTimeFormatSettings.ShortTimeFormat := 'hh:mm:ss.zz';

finalization

CacheThreadIsAcivted := False;
while CacheFileMD5FromDirectory_Num > 0 do
    TCompute.Sleep(1);

if ExLibs <> nil then
    DisposeObject(ExLibs);
DisposeObject(FileMD5Cache);

end.
