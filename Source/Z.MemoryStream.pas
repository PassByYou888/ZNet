(*
https://zpascal.net
https://github.com/PassByYou888/ZNet
https://github.com/PassByYou888/zRasterization
https://github.com/PassByYou888/ZSnappy
https://github.com/PassByYou888/Z-AI1.4
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
{ * MemoryStream Imp                                                           * }
{ ****************************************************************************** }
unit Z.MemoryStream;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses
  SysUtils,
{$IFDEF FPC}
  zstream,
  Z.FPC.GenericList,
{$ELSE FPC}
  ZLib,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Int128;

type
  TMem64 = class;

  TMS64 = class(TCore_Stream)
  private
    FDelta: NativeInt;
    FMemory: Pointer;
    FSize: NativeUInt;
    FPosition: NativeUInt;
    FCapacity: NativeUInt;
    FProtectedMode: Boolean;
    FMem64: TMem64;
  protected
    procedure SetPointer(buffPtr: Pointer; const BuffSize: NativeUInt);
    procedure SetCapacity(NewCapacity: NativeUInt);
    function Realloc(var NewCapacity: NativeUInt): Pointer; virtual;
    procedure SetDelta(const Value: NativeInt);
    property Capacity: NativeUInt read FCapacity write SetCapacity;
  public
    constructor Create;
    constructor CustomCreate(const customDelta: NativeInt);
    destructor Destroy; override;

    function Mem64(Mapping_Begin_As_Position_: Boolean): TMem64; overload;
    function Mem64: TMem64; overload;
    function Clone: TMS64;
    function Create_Mapping_Instance: TMS64;        // Create a new instance, which is self mapping and must be manually released while maintaining a map-source state.
    function Create_Mapping_Instance_Mem64: TMem64; // Create a new instance, which is self mapping and must be manually released while maintaining a map-source state.
    function Swap_To_New_Instance: TMS64;
    procedure DiscardMemory;
    procedure Clear;
    procedure NewParam(source: TMS64); overload;
    procedure NewParam(source: TMem64); overload;
    procedure SwapInstance(source: TMS64); overload;
    procedure SwapInstance(source: TMem64); overload;
    function ToBytes: TBytes;
    function ToMD5: TMD5;
    property Delta: NativeInt read FDelta write SetDelta;
    property ProtectedMode: Boolean read FProtectedMode;
    procedure SetPointerWithProtectedMode(buffPtr: Pointer; const BuffSize: Int64);
    procedure Mapping(buffPtr: Pointer; const BuffSize: Int64); overload;
    procedure Mapping(m64: TMS64); overload;
    procedure Mapping(m64: TMem64); overload;
    function PositionAsPtr(const Position_: Int64): Pointer; overload;
    function PositionAsPtr: Pointer; overload;
    function PosAsPtr(const Position_: Int64): Pointer; overload;
    function PosAsPtr: Pointer; overload;
    procedure LoadFromStream(stream: TCore_Stream); virtual;
    procedure LoadFromFile(FileName: SystemString);
    procedure SaveToStream(stream: TCore_Stream); virtual;
    procedure SaveToFile(FileName: SystemString);

    procedure SetSize(const NewSize: Int64); overload; override;
    procedure SetSize(NewSize: longint); overload; override;

    function Write64(const buffer; Count: Int64): Int64; virtual;
    function WritePtr(const p: Pointer; Count: Int64): Int64;
    function write(const buffer; Count: longint): longint; overload; override;
    procedure WriteBytes(const buff: TBytes);

    function Read64(var buffer; Count: Int64): Int64; virtual;
    function ReadPtr(const p: Pointer; Count: Int64): Int64;
    function read(var buffer; Count: longint): longint; overload; override;

    // only delphi
{$IFDEF DELPHI}
    function write(const buffer: TBytes; Offset, Count: longint): longint; overload; override;
    function read(buffer: TBytes; Offset, Count: longint): longint; overload; override;
{$ENDIF DELPHI}
    // seek
    function Seek(const Offset: Int64; origin: TSeekOrigin): Int64; override;
    property Memory: Pointer read FMemory;

    // copy
    function CopyMem64(const source: TMem64; Count: Int64): Int64;
    function CopyFrom(const source: TCore_Stream; Count: Int64): Int64; overload;
    function CopyFrom(const source: TMem64; Count: Int64): Int64; overload;

    // Serialized writer
    procedure WriteBool(const buff: Boolean);
    procedure WriteInt8(const buff: ShortInt);
    procedure WriteInt16(const buff: SmallInt);
    procedure WriteInt32(const buff: Integer);
    procedure WriteInt64(const buff: Int64);
    procedure WriteInt128(const buff: Int128);
    procedure WriteUInt8(const buff: Byte);
    procedure WriteUInt16(const buff: Word);
    procedure WriteUInt32(const buff: Cardinal);
    procedure WriteUInt64(const buff: UInt64);
    procedure WriteUInt128(const buff: UInt128);
    procedure WriteSingle(const buff: Single);
    procedure WriteDouble(const buff: Double);
    procedure WriteCurrency(const buff: Currency);
    procedure WriteString(const buff: TPascalString);
    procedure WriteANSI(const buff: TPascalString); overload;
    procedure WriteANSI(const buff: TPascalString; const L: Integer); overload;
    procedure WriteMD5(const buff: TMD5);

    // Serialized reader
    function ReadBool: Boolean;
    function ReadInt8: ShortInt;
    function ReadInt16: SmallInt;
    function ReadInt32: Integer;
    function ReadInt64: Int64;
    function ReadInt128: Int128;
    function ReadUInt8: Byte;
    function ReadUInt16: Word;
    function ReadUInt32: Cardinal;
    function ReadUInt64: UInt64;
    function ReadUInt128: UInt128;
    function ReadSingle: Single;
    function ReadDouble: Double;
    function ReadCurrency: Currency;
    function PrepareReadString: Boolean;
    function ReadString: TPascalString;
    function ReadStringAsBuff: TBytes;
    procedure IgnoreReadString;
    function ReadANSI(L: Integer): TPascalString;
    function ReadMD5: TMD5;
  end;

  TMS64_Array = array of TMS64;
  TStream64_Array = TMS64_Array;
  TMemoryStream64_Array = TMS64_Array;

  TStream64 = TMS64;
  TMemoryStream64 = TMS64;

  TMemoryStream64List_Decl = TGenericsList<TMS64>;

  TMemoryStream64List = class(TMemoryStream64List_Decl)
  public
    procedure Clean;
    function To_Array: TMS64_Array;
  end;

  TStream64List = TMemoryStream64List;
  TMS64List = TMemoryStream64List;

  TMS64_Pool = TBig_Object_List<TMS64>;

  TMemoryStream64ThreadList = class(TMemoryStream64List_Decl)
  private
    FCritical: TCritical;
  public
    AutoFree_Stream: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
    procedure Remove(obj: TMS64);
    procedure Delete(index: Integer);
    procedure Clear;
    procedure Clean;
    function To_Array: TMS64_Array;
  end;

  TStream64CriticalList = TMemoryStream64ThreadList;
  TMS64CriticalList = TMemoryStream64ThreadList;
  TStream64ThreadList = TMemoryStream64ThreadList;
  TMS64ThreadList = TMemoryStream64ThreadList;

  IMemoryStream64WriteTrigger = interface
    procedure TriggerWrite64(Count: Int64);
  end;

  TMemoryStream64OfWriteTrigger = class(TMS64)
  public
    Trigger: IMemoryStream64WriteTrigger;
    constructor Create(ATrigger: IMemoryStream64WriteTrigger);
    function Write64(const buffer; Count: Int64): Int64; override;
  end;

  IMemoryStream64ReadTrigger = interface
    procedure TriggerRead64(Count: Int64);
  end;

  TMemoryStream64OfReadTrigger = class(TMS64)
  public
    Trigger: IMemoryStream64ReadTrigger;
    constructor Create(ATrigger: IMemoryStream64ReadTrigger);
    function Read64(var buffer; Count: Int64): Int64; override;
  end;

  IMemoryStream64ReadWriteTrigger = interface
    procedure TriggerWrite64(Count: Int64);
    procedure TriggerRead64(Count: Int64);
  end;

  TMemoryStream64OfReadWriteTrigger = class(TMS64)
  public
    Trigger: IMemoryStream64ReadWriteTrigger;
    constructor Create(ATrigger: IMemoryStream64ReadWriteTrigger);
    function Read64(var buffer; Count: Int64): Int64; override;
    function Write64(const buffer; Count: Int64): Int64; override;
  end;

  TMem64 = class(TCore_Object_Intermediate)
  private
    FDelta: NativeInt;
    FMemory: Pointer;
    FSize: Int64;
    FPosition: Int64;
    FCapacity: Int64;
    FProtectedMode: Boolean;
    FStream64: TMS64;
  protected
    procedure SetPointer(buffPtr: Pointer; const BuffSize: Int64);
    procedure SetCapacity(NewCapacity: Int64);
    function Realloc(var NewCapacity: Int64): Pointer;
    property Capacity: Int64 read FCapacity write SetCapacity;
    function GetDelta: NativeInt;
    procedure SetDelta(const Value: NativeInt);
    function GetMemory_: Pointer;
    function GetPosition: Int64;
    procedure SetPosition(const Value: Int64);
    function GetSize: Int64;
    procedure SetSize(const NewSize: Int64);
  public
    constructor Create;
    constructor CustomCreate(const customDelta: NativeInt);
    destructor Destroy; override;

    function Stream64(Mapping_Begin_As_Position_: Boolean): TMS64; overload;
    function Stream64: TMS64; overload;
    function Clone: TMem64;
    function Create_Mapping_Instance: TMem64;     // Create a new instance, which is self mapping and must be manually released while maintaining a map-source state.
    function Create_Mapping_Instance_MS64: TMS64; // Create a new instance, which is self mapping and must be manually released while maintaining a map-source state.
    function Swap_To_New_Instance: TMem64;
    procedure DiscardMemory;
    procedure Clear;
    procedure NewParam(source: TMS64); overload;
    procedure NewParam(source: TMem64); overload;
    procedure SwapInstance(source: TMS64); overload;
    procedure SwapInstance(source: TMem64); overload;
    function ToBytes: TBytes;
    function ToMD5: TMD5;
    property Delta: NativeInt read GetDelta write SetDelta;
    property Memory: Pointer read GetMemory_;
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize;
    property ProtectedMode: Boolean read FProtectedMode;
    procedure SetPointerWithProtectedMode(buffPtr: Pointer; const BuffSize: Int64);
    procedure Mapping(buffPtr: Pointer; const BuffSize: Int64); overload;
    procedure Mapping(m64: TMS64); overload;
    procedure Mapping(m64: TMem64); overload;
    function PositionAsPtr(const Position_: Int64): Pointer; overload;
    function PositionAsPtr: Pointer; overload;
    function PosAsPtr(const Position_: Int64): Pointer; overload;
    function PosAsPtr: Pointer; overload;
    procedure LoadFromStream(stream: TCore_Stream);
    procedure LoadFromFile(FileName: SystemString);
    procedure SaveToStream(stream: TCore_Stream);
    procedure SaveToFile(FileName: SystemString);

    function Write64(const buffer; Count: Int64): Int64;
    function WritePtr(const p: Pointer; Count: Int64): Int64;
    function write(const buffer; Count: Int64): Int64;
    function WriteBytes(const buffer: TBytes): Int64;
    function Read64(var buffer; Count: Int64): Int64;
    function ReadPtr(const p: Pointer; Count: Int64): Int64;
    function read(var buffer; Count: Int64): Int64;
    function Seek(const Offset: Int64; origin: TSeekOrigin): Int64;

    function CopyFrom(const source: TCore_Stream; Count: Int64): Int64; overload;
    function CopyFrom(const source: TMem64; Count: Int64): Int64; overload;

    // Serialized writer
    procedure WriteBool(const buff: Boolean);
    procedure WriteInt8(const buff: ShortInt);
    procedure WriteInt16(const buff: SmallInt);
    procedure WriteInt32(const buff: Integer);
    procedure WriteInt64(const buff: Int64);
    procedure WriteInt128(const buff: Int128);
    procedure WriteUInt8(const buff: Byte);
    procedure WriteUInt16(const buff: Word);
    procedure WriteUInt32(const buff: Cardinal);
    procedure WriteUInt64(const buff: UInt64);
    procedure WriteUInt128(const buff: UInt128);
    procedure WriteSingle(const buff: Single);
    procedure WriteDouble(const buff: Double);
    procedure WriteCurrency(const buff: Currency);
    procedure WriteString(const buff: TPascalString);
    procedure WriteANSI(const buff: TPascalString); overload;
    procedure WriteANSI(const buff: TPascalString; const L: Integer); overload;
    procedure WriteMD5(const buff: TMD5);

    // Serialized reader
    function ReadBool: Boolean;
    function ReadInt8: ShortInt;
    function ReadInt16: SmallInt;
    function ReadInt32: Integer;
    function ReadInt64: Int64;
    function ReadInt128: Int128;
    function ReadUInt8: Byte;
    function ReadUInt16: Word;
    function ReadUInt32: Cardinal;
    function ReadUInt64: UInt64;
    function ReadUInt128: UInt128;
    function ReadSingle: Single;
    function ReadDouble: Double;
    function ReadCurrency: Currency;
    function PrepareReadString: Boolean;
    function ReadString: TPascalString;
    function ReadStringAsBuff: TBytes;
    procedure IgnoreReadString;
    function ReadANSI(L: Integer): TPascalString;
    function ReadMD5: TMD5;
  end;

  TMem64_Array = array of TMem64;

  TM64 = TMem64;

  TMem64List_Decl = TGenericsList<TMem64>;

  TMem64List = class(TMem64List_Decl)
  public
    procedure Clean;
  end;

  TM64List = TMem64List;

{$IFDEF FPC}

  TDecompressionStream = class(zstream.TDecompressionStream)
  public
  end;

  { TCompressionStream }

  TCompressionStream = class(zstream.TCompressionStream)
  public
    constructor Create(stream: TCore_Stream); overload;
    constructor Create(level: Tcompressionlevel; stream: TCore_Stream); overload;
  end;
{$ELSE}

  TDecompressionStream = ZLib.TZDecompressionStream;
  TCompressionStream = ZLib.TZCompressionStream;
{$ENDIF}
  TSelectCompressionMethod = (scmNone, scmZLIB, scmZLIB_Fast, scmZLIB_Max, scmDeflate, scmBRRC);

function MaxCompressStream(sour, dest: TCore_Stream): Boolean;
function FastCompressStream(sour, dest: TCore_Stream): Boolean;
function CompressStream(sour, dest: TCore_Stream): Boolean; overload;
function DecompressStream(DataPtr: Pointer; siz: NativeInt; dest: TCore_Stream): Boolean; overload;
function DecompressStream(sour: TCore_Stream; dest: TCore_Stream): Boolean; overload;
function DecompressStreamToPtr(sour: TCore_Stream; var dest: Pointer): Boolean; overload;
function CompressFile(sour, dest: SystemString): Boolean;
function DecompressFile(sour, dest: SystemString): Boolean;
function SelectCompressStream(const scm: TSelectCompressionMethod; const sour, dest: TCore_Stream): Boolean;
function SelectDecompressStream(const sour, dest: TCore_Stream): Boolean; overload;
function SelectDecompressStream(const sour, dest: TCore_Stream; var scm: TSelectCompressionMethod): Boolean; overload;
procedure ParallelCompressMemory(const ThNum: Integer; const scm: TSelectCompressionMethod; const StripNum_: Integer; const sour: TMS64; const dest: TCore_Stream); overload;
procedure ParallelCompressMemory(const scm: TSelectCompressionMethod; const StripNum_: Integer; const sour: TMS64; const dest: TCore_Stream); overload;
procedure ParallelCompressMemory(const scm: TSelectCompressionMethod; const sour: TMS64; const dest: TCore_Stream); overload;
procedure ParallelCompressMemory(const sour: TMS64; const dest: TCore_Stream); overload;
procedure ParallelDecompressStream(const ThNum: Integer; const sour_, dest_: TCore_Stream); overload;
procedure ParallelDecompressStream(const sour_, dest_: TCore_Stream); overload;
procedure ParallelCompressFile(const sour, dest: SystemString);
procedure ParallelDecompressFile(const sour, dest: SystemString);
function CompressUTF8(const sour_: TBytes): TBytes;
function DecompressUTF8(const sour_: TBytes): TBytes;

// Serialized write
procedure StreamWriteBool(const stream: TCore_Stream; const buff: Boolean);
procedure StreamWriteInt8(const stream: TCore_Stream; const buff: ShortInt);
procedure StreamWriteInt16(const stream: TCore_Stream; const buff: SmallInt);
procedure StreamWriteInt32(const stream: TCore_Stream; const buff: Integer);
procedure StreamWriteInt64(const stream: TCore_Stream; const buff: Int64);
procedure StreamWriteInt128(const stream: TCore_Stream; const buff: Int128);
procedure StreamWriteUInt8(const stream: TCore_Stream; const buff: Byte);
procedure StreamWriteUInt16(const stream: TCore_Stream; const buff: Word);
procedure StreamWriteUInt32(const stream: TCore_Stream; const buff: Cardinal);
procedure StreamWriteUInt64(const stream: TCore_Stream; const buff: UInt64);
procedure StreamWriteUInt128(const stream: TCore_Stream; const buff: UInt128);
procedure StreamWriteSingle(const stream: TCore_Stream; const buff: Single);
procedure StreamWriteDouble(const stream: TCore_Stream; const buff: Double);
procedure StreamWriteCurrency(const stream: TCore_Stream; const buff: Currency);
procedure StreamWriteString(const stream: TCore_Stream; const buff: TPascalString);
function ComputeStreamWriteStringSize(buff: TPascalString): Integer;
procedure StreamWriteMD5(const stream: TCore_Stream; const buff: TMD5);

// Serialized read
function StreamReadBool(const stream: TCore_Stream): Boolean;
function StreamReadInt8(const stream: TCore_Stream): ShortInt;
function StreamReadInt16(const stream: TCore_Stream): SmallInt;
function StreamReadInt32(const stream: TCore_Stream): Integer;
function StreamReadInt64(const stream: TCore_Stream): Int64;
function StreamReadInt128(const stream: TCore_Stream): Int128;
function StreamReadUInt8(const stream: TCore_Stream): Byte;
function StreamReadUInt16(const stream: TCore_Stream): Word;
function StreamReadUInt32(const stream: TCore_Stream): Cardinal;
function StreamReadUInt64(const stream: TCore_Stream): UInt64;
function StreamReadUInt128(const stream: TCore_Stream): UInt128;
function StreamReadSingle(const stream: TCore_Stream): Single;
function StreamReadDouble(const stream: TCore_Stream): Double;
function StreamReadCurrency(const stream: TCore_Stream): Currency;
function StreamReadString(const stream: TCore_Stream): TPascalString;
function StreamReadStringAsBuff(const stream: TCore_Stream): TBytes;
procedure StreamIgnoreReadString(const stream: TCore_Stream);
function StreamReadMD5(const stream: TCore_Stream): TMD5;

procedure DoStatus(const v: TMS64); overload;
procedure DoStatus(const v: TMem64); overload;

implementation

uses Z.UnicodeMixedLib, Z.Status, Z.Compress, Z.Instance.Tool;

procedure TMS64.SetPointer(buffPtr: Pointer; const BuffSize: NativeUInt);
begin
  FMemory := buffPtr;
  FSize := BuffSize;
end;

procedure TMS64.SetCapacity(NewCapacity: NativeUInt);
begin
  if FProtectedMode then
      Exit;
  SetPointer(Realloc(NewCapacity), FSize);
  FCapacity := NewCapacity;
end;

function TMS64.Realloc(var NewCapacity: NativeUInt): Pointer;
begin
  if FProtectedMode then
      Exit(nil);

  if (NewCapacity > 0) and (NewCapacity <> FSize) then
      NewCapacity := DeltaStep(NewCapacity, FDelta);
  Result := Memory;
  if NewCapacity <> FCapacity then
    begin
      if NewCapacity = 0 then
        begin
          System.FreeMemory(Memory);
          Result := nil;
        end
      else
        begin
          if Capacity = 0 then
              Result := System.GetMemory(NewCapacity)
          else
              Result := System.ReallocMemory(Result, NewCapacity);
          if Result = nil then
              RaiseInfo('%s Out of memory while expanding memory stream', [umlSizeToStr(NewCapacity).Text]);
        end;
    end;
end;

procedure TMS64.SetDelta(const Value: NativeInt);
begin
  FDelta := umlClamp(Value, 64, 1024 * 1024);
end;

constructor TMS64.Create;
begin
  CustomCreate(256);
end;

constructor TMS64.CustomCreate(const customDelta: NativeInt);
begin
  inherited Create;
  Delta := customDelta;
  FMemory := nil;
  FSize := 0;
  FPosition := 0;
  FCapacity := 0;
  FProtectedMode := False;
  FMem64 := nil;
end;

destructor TMS64.Destroy;
begin
  if FMem64 <> nil then
      DisposeObject(FMem64);
  Clear;
  inherited Destroy;
end;

function TMS64.Mem64(Mapping_Begin_As_Position_: Boolean): TMem64;
begin
  if FMem64 = nil then
      FMem64 := TMem64.Create;
  if Mapping_Begin_As_Position_ then
      FMem64.Mapping(PosAsPtr, Size - Position)
  else
      FMem64.Mapping(self);
  Result := FMem64;
end;

function TMS64.Mem64: TMem64;
begin
  Result := Mem64(False);
end;

function TMS64.Clone: TMS64;
begin
  Result := TMS64.CustomCreate(FDelta);
  Result.Size := Size;
  CopyPtr(Memory, Result.Memory, Size);
  Result.Position := Position;
end;

function TMS64.Create_Mapping_Instance: TMS64;
begin
  Result := TMS64.Create;
  Result.Mapping(self);
end;

function TMS64.Create_Mapping_Instance_Mem64: TMem64;
begin
  Result := TMem64.Create;
  Result.Mapping(self);
end;

function TMS64.Swap_To_New_Instance: TMS64;
begin
  Result := TMS64.Create;
  SwapInstance(Result);
end;

procedure TMS64.DiscardMemory;
begin
  if FProtectedMode then
      Exit;
  FMemory := nil;
  FSize := 0;
  FPosition := 0;
  FCapacity := 0;
end;

procedure TMS64.Clear;
begin
  if FProtectedMode then
      Exit;
  SetCapacity(0);
  FSize := 0;
  FPosition := 0;
end;

procedure TMS64.NewParam(source: TMS64);
begin
  Clear;
  FDelta := source.FDelta;
  FMemory := source.FMemory;
  FSize := source.FSize;
  FPosition := source.FPosition;
  FCapacity := source.FCapacity;
  FProtectedMode := source.FProtectedMode;
end;

procedure TMS64.NewParam(source: TMem64);
begin
  Clear;
  FDelta := source.FDelta;
  FMemory := source.FMemory;
  FSize := source.FSize;
  FPosition := source.FPosition;
  FCapacity := source.FCapacity;
  FProtectedMode := source.FProtectedMode;
end;

procedure TMS64.SwapInstance(source: TMS64);
var
  FDelta_: NativeInt;
  FMemory_: Pointer;
  FSize_: NativeUInt;
  FPosition_: NativeUInt;
  FCapacity_: NativeUInt;
  FProtectedMode_: Boolean;
begin
  FDelta_ := FDelta;
  FMemory_ := FMemory;
  FSize_ := FSize;
  FPosition_ := FPosition;
  FCapacity_ := FCapacity;
  FProtectedMode_ := FProtectedMode;

  FDelta := source.FDelta;
  FMemory := source.FMemory;
  FSize := source.FSize;
  FPosition := source.FPosition;
  FCapacity := source.FCapacity;
  FProtectedMode := source.FProtectedMode;

  source.FDelta := FDelta_;
  source.FMemory := FMemory_;
  source.FSize := FSize_;
  source.FPosition := FPosition_;
  source.FCapacity := FCapacity_;
  source.FProtectedMode := FProtectedMode_;
end;

procedure TMS64.SwapInstance(source: TMem64);
var
  FDelta_: NativeInt;
  FMemory_: Pointer;
  FSize_: NativeUInt;
  FPosition_: NativeUInt;
  FCapacity_: NativeUInt;
  FProtectedMode_: Boolean;
begin
  FDelta_ := FDelta;
  FMemory_ := FMemory;
  FSize_ := FSize;
  FPosition_ := FPosition;
  FCapacity_ := FCapacity;
  FProtectedMode_ := FProtectedMode;

  FDelta := source.FDelta;
  FMemory := source.FMemory;
  FSize := source.FSize;
  FPosition := source.FPosition;
  FCapacity := source.FCapacity;
  FProtectedMode := source.FProtectedMode;

  source.FDelta := FDelta_;
  source.FMemory := FMemory_;
  source.FSize := FSize_;
  source.FPosition := FPosition_;
  source.FCapacity := FCapacity_;
  source.FProtectedMode := FProtectedMode_;
end;

function TMS64.ToBytes: TBytes;
begin
  SetLength(Result, Size);
  if Size > 0 then
      CopyPtr(Memory, @Result[0], Size);
end;

function TMS64.ToMD5: TMD5;
begin
  Result := umlMD5(Memory, Size);
end;

procedure TMS64.SetPointerWithProtectedMode(buffPtr: Pointer; const BuffSize: Int64);
begin
  Mapping(buffPtr, BuffSize);
end;

procedure TMS64.Mapping(buffPtr: Pointer; const BuffSize: Int64);
begin
  Clear;
  FMemory := buffPtr;
  FSize := BuffSize;
  FPosition := 0;
  FProtectedMode := True;
end;

procedure TMS64.Mapping(m64: TMS64);
begin
  Mapping(m64.Memory, m64.Size);
end;

procedure TMS64.Mapping(m64: TMem64);
begin
  Mapping(m64.Memory, m64.Size);
end;

function TMS64.PositionAsPtr(const Position_: Int64): Pointer;
begin
  Result := GetOffset(FMemory, Position_);
end;

function TMS64.PositionAsPtr: Pointer;
begin
  Result := GetOffset(FMemory, FPosition);
end;

function TMS64.PosAsPtr(const Position_: Int64): Pointer;
begin
  Result := PositionAsPtr(Position_);
end;

function TMS64.PosAsPtr: Pointer;
begin
  Result := PositionAsPtr();
end;

procedure TMS64.LoadFromStream(stream: TCore_Stream);
begin
  if FProtectedMode then
      Exit;
  Clear;
  stream.Position := 0;
  if CopyFrom(stream, stream.Size) <> stream.Size then
      RaiseInfo('load stream error.');
  Position := 0;
end;

procedure TMS64.LoadFromFile(FileName: SystemString);
var
  stream: TCore_Stream;
begin
  stream := TCore_FileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
      LoadFromStream(stream);
  finally
      DisposeObject(stream);
  end;
end;

procedure TMS64.SaveToStream(stream: TCore_Stream);
const
  ChunkSize = 64 * 1024 * 1024;
var
  p: Pointer;
  j: NativeInt;
  Num: NativeInt;
  Rest: NativeInt;
begin
  if stream is TMS64 then
    begin
      TMS64(stream).Clear;
      if Size > 0 then
          TMS64(stream).WritePtr(Memory, Size);
      TMS64(stream).Position := 0;
      Exit;
    end;

  if Size > 0 then
    begin
      p := FMemory;
      if Size > ChunkSize then
        begin
          { Calculate number of full chunks that will fit into the buffer }
          Num := Size div ChunkSize;
          { Calculate remaining bytes }
          Rest := Size mod ChunkSize;

          { Process full chunks }
          for j := 0 to Num - 1 do
            begin
              stream.WriteBuffer(p^, ChunkSize);
              p := GetOffset(p, ChunkSize);
            end;

          { Process remaining bytes }
          if Rest > 0 then
            begin
              stream.WriteBuffer(p^, Rest);
              p := GetOffset(p, Rest);
            end;
        end
      else
          stream.WriteBuffer(p^, Size);
    end;
end;

procedure TMS64.SaveToFile(FileName: SystemString);
var
  stream: TCore_Stream;
begin
  stream := TCore_FileStream.Create(FileName, fmCreate);
  try
      SaveToStream(stream);
  finally
      DisposeObject(stream);
  end;
end;

procedure TMS64.SetSize(const NewSize: Int64);
var
  OldPosition: Int64;
begin
  if FProtectedMode then
      Exit;

  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  if OldPosition > NewSize then
      Seek(0, TSeekOrigin.soEnd);
end;

procedure TMS64.SetSize(NewSize: longint);
begin
  SetSize(Int64(NewSize));
end;

function TMS64.Write64(const buffer; Count: Int64): Int64;
var
  p: Int64;
begin
  if (Count > 0) then
    begin
      p := FPosition;
      p := p + Count;
      if p > 0 then
        begin
          if p > FSize then
            begin
              if FProtectedMode then
                begin
                  Result := 0;
                  Exit;
                end;
              if p > FCapacity then
                  SetCapacity(p);
              FSize := p;
            end;
          CopyPtr(@buffer, GetOffset(FMemory, FPosition), Count);
          FPosition := p;
          Result := Count;
          Exit;
        end;
    end;
  Result := 0;
end;

function TMS64.WritePtr(const p: Pointer; Count: Int64): Int64;
begin
  Result := Write64(p^, Count);
end;

function TMS64.write(const buffer; Count: longint): longint;
begin
  Result := Write64(buffer, Count);
end;

procedure TMS64.WriteBytes(const buff: TBytes);
begin
  if Length(buff) > 0 then
      WritePtr(@buff[0], Length(buff));
end;

function TMS64.Read64(var buffer; Count: Int64): Int64;
begin
  if Count > 0 then
    begin
      Result := FSize;
      Result := Result - FPosition;
      if Result > 0 then
        begin
          if Result > Count then
              Result := Count;
          CopyPtr(GetOffset(FMemory, FPosition), @buffer, Result);
          inc(FPosition, Result);
          Exit;
        end;
    end;
  Result := 0;
end;

function TMS64.ReadPtr(const p: Pointer; Count: Int64): Int64;
begin
  Result := Read64(p^, Count);
end;

function TMS64.read(var buffer; Count: longint): longint;
begin
  Result := Read64(buffer, Count);
end;

{$IFDEF DELPHI}


function TMS64.write(const buffer: TBytes; Offset, Count: longint): longint;
var
  p: Int64;
begin
  if Count > 0 then
    begin
      p := FPosition;
      p := p + Count;
      if p > 0 then
        begin
          if p > FSize then
            begin
              if FProtectedMode then
                begin
                  Result := 0;
                  Exit;
                end;
              if p > FCapacity then
                  SetCapacity(p);
              FSize := p;
            end;
          CopyPtr(@buffer[Offset], GetOffset(FMemory, FPosition), Count);
          FPosition := p;
          Result := Count;
          Exit;
        end;
    end;
  Result := 0;
end;

function TMS64.read(buffer: TBytes; Offset, Count: longint): longint;
var
  p: Int64;
begin
  if Count > 0 then
    begin
      p := FSize;
      p := p - FPosition;
      if p > 0 then
        begin
          if p > Count then
              p := Count;

          CopyPtr(GetOffset(FMemory, FPosition), @buffer[Offset], p);
          inc(FPosition, p);
          Result := p;
          Exit;
        end;
    end;
  Result := 0;
end;
{$ENDIF DELPHI}


function TMS64.Seek(const Offset: Int64; origin: TSeekOrigin): Int64;
begin
  case origin of
    TSeekOrigin.soBeginning: FPosition := Offset;
    TSeekOrigin.soCurrent: inc(FPosition, Offset);
    TSeekOrigin.soEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

function TMS64.CopyMem64(const source: TMem64; Count: Int64): Int64;
begin
  if FProtectedMode then
      RaiseInfo('protected mode');
  WritePtr(source.PositionAsPtr, Count);
  source.Position := source.FPosition + Count;
  Result := Count;
end;

function TMS64.CopyFrom(const source: TCore_Stream; Count: Int64): Int64;
const
  MaxBufSize = $F000;
var
  BufSize, n, p: Int64;
begin
  if FProtectedMode then
      RaiseInfo('protected mode');

  if Count = 0 then
      Exit(0);

  if Count < 0 then
    begin
      source.Position := 0;
      Count := source.Size;
    end;

  if source is TMS64 then
    begin
      WritePtr(TMS64(source).PositionAsPtr, Count);
      TMS64(source).Position := TMS64(source).FPosition + Count;
      Result := Count;
      Exit;
    end;

  Result := Count;
  if Count > MaxBufSize then
      BufSize := MaxBufSize
  else
      BufSize := Count;

  p := Position;
  if p + Count > Size then
      Size := p + Count;

  while Count <> 0 do
    begin
      if Count > BufSize then
          n := BufSize
      else
          n := Count;

      // fast copy optimized
      if source.read(PosAsPtr(p)^, n) <> n then
          RaiseInfo('stream read error.');

      inc(p, n);
      dec(Count, n);
    end;
  Position := p;
end;

function TMS64.CopyFrom(const source: TMem64; Count: Int64): Int64;
begin
  if FProtectedMode then
      RaiseInfo('protected mode');
  WritePtr(source.PositionAsPtr, Count);
  source.Position := source.FPosition + Count;
  Result := Count;
end;

procedure TMS64.WriteBool(const buff: Boolean);
begin
  WritePtr(@buff, 1);
end;

procedure TMS64.WriteInt8(const buff: ShortInt);
begin
  WritePtr(@buff, 1);
end;

procedure TMS64.WriteInt16(const buff: SmallInt);
begin
  WritePtr(@buff, 2);
end;

procedure TMS64.WriteInt32(const buff: Integer);
begin
  WritePtr(@buff, 4);
end;

procedure TMS64.WriteInt64(const buff: Int64);
begin
  WritePtr(@buff, 8);
end;

procedure TMS64.WriteInt128(const buff: Int128);
begin
  WritePtr(@buff.b[0], 16);
end;

procedure TMS64.WriteUInt8(const buff: Byte);
begin
  WritePtr(@buff, 1);
end;

procedure TMS64.WriteUInt16(const buff: Word);
begin
  WritePtr(@buff, 2);
end;

procedure TMS64.WriteUInt32(const buff: Cardinal);
begin
  WritePtr(@buff, 4);
end;

procedure TMS64.WriteUInt64(const buff: UInt64);
begin
  WritePtr(@buff, 8);
end;

procedure TMS64.WriteUInt128(const buff: UInt128);
begin
  WritePtr(@buff.b[0], 16);
end;

procedure TMS64.WriteSingle(const buff: Single);
begin
  WritePtr(@buff, 4);
end;

procedure TMS64.WriteDouble(const buff: Double);
begin
  WritePtr(@buff, 8);
end;

procedure TMS64.WriteCurrency(const buff: Currency);
begin
  WriteDouble(buff);
end;

procedure TMS64.WriteString(const buff: TPascalString);
var
  b: TBytes;
begin
  b := buff.Bytes;
  WriteUInt32(Length(b));
  if Length(b) > 0 then
    begin
      WritePtr(@b[0], Length(b));
      SetLength(b, 0);
    end;
end;

procedure TMS64.WriteANSI(const buff: TPascalString);
var
  b: TBytes;
begin
  b := buff.ANSI;
  if Length(b) > 0 then
    begin
      WritePtr(@b[0], Length(b));
      SetLength(b, 0);
    end;
end;

procedure TMS64.WriteANSI(const buff: TPascalString; const L: Integer);
var
  b: TBytes;
begin
  b := buff.ANSI;
  if L > 0 then
    begin
      WritePtr(@b[0], L);
      SetLength(b, 0);
    end;
end;

procedure TMS64.WriteMD5(const buff: TMD5);
begin
  WritePtr(@buff, 16);
end;

function TMS64.ReadBool: Boolean;
begin
  ReadPtr(@Result, 1);
end;

function TMS64.ReadInt8: ShortInt;
begin
  ReadPtr(@Result, 1);
end;

function TMS64.ReadInt16: SmallInt;
begin
  ReadPtr(@Result, 2);
end;

function TMS64.ReadInt32: Integer;
begin
  ReadPtr(@Result, 4);
end;

function TMS64.ReadInt64: Int64;
begin
  ReadPtr(@Result, 8);
end;

function TMS64.ReadInt128: Int128;
begin
  ReadPtr(@Result.b[0], 16);
end;

function TMS64.ReadUInt8: Byte;
begin
  ReadPtr(@Result, 1);
end;

function TMS64.ReadUInt16: Word;
begin
  ReadPtr(@Result, 2);
end;

function TMS64.ReadUInt32: Cardinal;
begin
  ReadPtr(@Result, 4);
end;

function TMS64.ReadUInt64: UInt64;
begin
  ReadPtr(@Result, 8);
end;

function TMS64.ReadUInt128: UInt128;
begin
  ReadPtr(@Result.b[0], 16);
end;

function TMS64.ReadSingle: Single;
begin
  ReadPtr(@Result, 4);
end;

function TMS64.ReadDouble: Double;
begin
  ReadPtr(@Result, 8);
end;

function TMS64.ReadCurrency: Currency;
begin
  Result := ReadDouble();
end;

function TMS64.PrepareReadString: Boolean;
begin
  Result := (Position + 4 <= Size) and (Position + 4 + PCardinal(PositionAsPtr())^ <= Size);
end;

function TMS64.ReadString: TPascalString;
var
  L: Cardinal;
  b: TBytes;
begin
  try
    L := ReadUInt32;
    if L > 0 then
      begin
        SetLength(b, L);
        ReadPtr(@b[0], L);
        Result.Bytes := b;
        SetLength(b, 0);
      end;
  except
      Result := '';
  end;
end;

function TMS64.ReadStringAsBuff: TBytes;
var
  L: Cardinal;
begin
  try
    L := ReadUInt32;
    if L > 0 then
      begin
        SetLength(Result, L);
        ReadPtr(@Result[0], L);
      end
    else
        SetLength(Result, 0);
  except
      SetLength(Result, 0);
  end;
end;

procedure TMS64.IgnoreReadString;
var
  L: Cardinal;
  b: TBytes;
begin
  try
    L := ReadUInt32;
    if L > 0 then
      begin
        SetLength(b, L);
        ReadPtr(@b[0], L);
        SetLength(b, 0);
      end;
  except
  end;
end;

function TMS64.ReadANSI(L: Integer): TPascalString;
var
  b: TBytes;
begin
  if L > 0 then
    begin
      SetLength(b, L);
      ReadPtr(@b[0], L);
      Result.ANSI := b;
      SetLength(b, 0);
    end;
end;

function TMS64.ReadMD5: TMD5;
begin
  ReadPtr(@Result, 16);
end;

procedure TMemoryStream64List.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(Items[i]);
  Clear;
end;

function TMemoryStream64List.To_Array: TMS64_Array;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Items[i];
end;

constructor TMemoryStream64ThreadList.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  AutoFree_Stream := False;
end;

destructor TMemoryStream64ThreadList.Destroy;
begin
  DisposeObject(FCritical);
  Clear;
  inherited Destroy;
end;

procedure TMemoryStream64ThreadList.Lock;
begin
  FCritical.Lock;
end;

procedure TMemoryStream64ThreadList.UnLock;
begin
  FCritical.UnLock;
end;

procedure TMemoryStream64ThreadList.Remove(obj: TMS64);
begin
  if AutoFree_Stream then
      DisposeObject(obj);
  inherited Remove(obj);
end;

procedure TMemoryStream64ThreadList.Delete(index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      if AutoFree_Stream then
          DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TMemoryStream64ThreadList.Clear;
var
  i: Integer;
begin
  if AutoFree_Stream then
    for i := 0 to Count - 1 do
        DisposeObject(Items[i]);
  inherited Clear;
end;

procedure TMemoryStream64ThreadList.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(Items[i]);
  inherited Clear;
end;

function TMemoryStream64ThreadList.To_Array: TMS64_Array;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Items[i];
end;

constructor TMemoryStream64OfWriteTrigger.Create(ATrigger: IMemoryStream64WriteTrigger);
begin
  inherited Create;
  Trigger := ATrigger;
end;

function TMemoryStream64OfWriteTrigger.Write64(const buffer; Count: Int64): Int64;
begin
  Result := inherited Write64(buffer, Count);
  if Assigned(Trigger) then
      Trigger.TriggerWrite64(Count);
end;

constructor TMemoryStream64OfReadTrigger.Create(ATrigger: IMemoryStream64ReadTrigger);
begin
  inherited Create;
  Trigger := ATrigger;
end;

function TMemoryStream64OfReadTrigger.Read64(var buffer; Count: Int64): Int64;
begin
  Result := inherited Read64(buffer, Count);
  if Assigned(Trigger) then
      Trigger.TriggerRead64(Count);
end;

constructor TMemoryStream64OfReadWriteTrigger.Create(ATrigger: IMemoryStream64ReadWriteTrigger);
begin
  inherited Create;
  Trigger := ATrigger;
end;

function TMemoryStream64OfReadWriteTrigger.Read64(var buffer; Count: Int64): Int64;
begin
  Result := inherited Read64(buffer, Count);
  if Assigned(Trigger) then
      Trigger.TriggerRead64(Count);
end;

function TMemoryStream64OfReadWriteTrigger.Write64(const buffer; Count: Int64): Int64;
begin
  Result := inherited Write64(buffer, Count);
  if Assigned(Trigger) then
      Trigger.TriggerWrite64(Count);
end;

procedure TMem64.SetPointer(buffPtr: Pointer; const BuffSize: Int64);
begin
  FMemory := buffPtr;
  FSize := BuffSize;
end;

procedure TMem64.SetCapacity(NewCapacity: Int64);
begin
  if FProtectedMode then
      Exit;
  SetPointer(Realloc(NewCapacity), FSize);
  FCapacity := NewCapacity;
end;

function TMem64.Realloc(var NewCapacity: Int64): Pointer;
begin
  if FProtectedMode then
      Exit(nil);

  if (NewCapacity > 0) and (NewCapacity <> FSize) then
      NewCapacity := DeltaStep(NewCapacity, FDelta);
  Result := Memory;
  if NewCapacity <> FCapacity then
    begin
      if NewCapacity = 0 then
        begin
          System.FreeMemory(Memory);
          Result := nil;
        end
      else
        begin
          if Capacity = 0 then
              Result := System.GetMemory(NewCapacity)
          else
              Result := System.ReallocMemory(Result, NewCapacity);
          if Result = nil then
              RaiseInfo('%s Out of memory while expanding memory stream', [umlSizeToStr(NewCapacity).Text]);
        end;
    end;
end;

function TMem64.GetDelta: NativeInt;
begin
  Result := FDelta;
end;

procedure TMem64.SetDelta(const Value: NativeInt);
begin
  FDelta := umlClamp(Value, 64, 1024 * 1024);
end;

function TMem64.GetMemory_: Pointer;
begin
  Result := FMemory;
end;

function TMem64.GetPosition: Int64;
begin
  Result := Seek(0, TSeekOrigin.soCurrent);
end;

procedure TMem64.SetPosition(const Value: Int64);
begin
  Seek(Value, TSeekOrigin.soBeginning);
end;

function TMem64.GetSize: Int64;
var
  Pos_: Int64;
begin
  Pos_ := Seek(0, TSeekOrigin.soCurrent);
  Result := Seek(0, TSeekOrigin.soEnd);
  Seek(Pos_, TSeekOrigin.soBeginning);
end;

procedure TMem64.SetSize(const NewSize: Int64);
var
  OldPosition: Int64;
begin
  if FProtectedMode then
      Exit;

  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  if OldPosition > NewSize then
      Seek(0, TSeekOrigin.soEnd);
end;

constructor TMem64.Create;
begin
  CustomCreate(256);
end;

constructor TMem64.CustomCreate(const customDelta: NativeInt);
begin
  inherited Create;
  Delta := customDelta;
  FMemory := nil;
  FSize := 0;
  FPosition := 0;
  FCapacity := 0;
  FProtectedMode := False;
  FStream64 := nil;
end;

destructor TMem64.Destroy;
begin
  if FStream64 <> nil then
      DisposeObject(FStream64);
  Clear;
  inherited Destroy;
end;

function TMem64.Stream64(Mapping_Begin_As_Position_: Boolean): TMS64;
begin
  if FStream64 = nil then
      FStream64 := TMS64.Create;
  if Mapping_Begin_As_Position_ then
      FStream64.Mapping(PosAsPtr, Size - Position)
  else
      FStream64.Mapping(self);
  Result := FStream64;
end;

function TMem64.Stream64: TMS64;
begin
  Result := Stream64(False);
end;

function TMem64.Clone: TMem64;
begin
  Result := TMem64.CustomCreate(FDelta);
  Result.Size := Size;
  CopyPtr(Memory, Result.Memory, Size);
  Result.Position := Position;
end;

function TMem64.Create_Mapping_Instance: TMem64;
begin
  Result := TMem64.Create;
  Result.Mapping(self);
end;

function TMem64.Create_Mapping_Instance_MS64: TMS64;
begin
  Result := TMS64.Create;
  Result.Mapping(self);
end;

function TMem64.Swap_To_New_Instance: TMem64;
begin
  Result := TMem64.Create;
  SwapInstance(Result);
end;

procedure TMem64.DiscardMemory;
begin
  if FProtectedMode then
      Exit;
  FMemory := nil;
  FSize := 0;
  FPosition := 0;
  FCapacity := 0;
end;

procedure TMem64.Clear;
begin
  if FProtectedMode then
      Exit;
  SetCapacity(0);
  FSize := 0;
  FPosition := 0;
end;

procedure TMem64.NewParam(source: TMS64);
begin
  Clear;
  FDelta := source.FDelta;
  FMemory := source.FMemory;
  FSize := source.FSize;
  FPosition := source.FPosition;
  FCapacity := source.FCapacity;
  FProtectedMode := source.FProtectedMode;
end;

procedure TMem64.NewParam(source: TMem64);
begin
  Clear;
  FDelta := source.FDelta;
  FMemory := source.FMemory;
  FSize := source.FSize;
  FPosition := source.FPosition;
  FCapacity := source.FCapacity;
  FProtectedMode := source.FProtectedMode;
end;

procedure TMem64.SwapInstance(source: TMS64);
var
  FDelta_: NativeInt;
  FMemory_: Pointer;
  FSize_: Int64;
  FPosition_: Int64;
  FCapacity_: Int64;
  FProtectedMode_: Boolean;
begin
  FDelta_ := FDelta;
  FMemory_ := FMemory;
  FSize_ := FSize;
  FPosition_ := FPosition;
  FCapacity_ := FCapacity;
  FProtectedMode_ := FProtectedMode;

  FDelta := source.FDelta;
  FMemory := source.FMemory;
  FSize := source.FSize;
  FPosition := source.FPosition;
  FCapacity := source.FCapacity;
  FProtectedMode := source.FProtectedMode;

  source.FDelta := FDelta_;
  source.FMemory := FMemory_;
  source.FSize := FSize_;
  source.FPosition := FPosition_;
  source.FCapacity := FCapacity_;
  source.FProtectedMode := FProtectedMode_;
end;

procedure TMem64.SwapInstance(source: TMem64);
var
  FDelta_: NativeInt;
  FMemory_: Pointer;
  FSize_: Int64;
  FPosition_: Int64;
  FCapacity_: Int64;
  FProtectedMode_: Boolean;
begin
  FDelta_ := FDelta;
  FMemory_ := FMemory;
  FSize_ := FSize;
  FPosition_ := FPosition;
  FCapacity_ := FCapacity;
  FProtectedMode_ := FProtectedMode;

  FDelta := source.FDelta;
  FMemory := source.FMemory;
  FSize := source.FSize;
  FPosition := source.FPosition;
  FCapacity := source.FCapacity;
  FProtectedMode := source.FProtectedMode;

  source.FDelta := FDelta_;
  source.FMemory := FMemory_;
  source.FSize := FSize_;
  source.FPosition := FPosition_;
  source.FCapacity := FCapacity_;
  source.FProtectedMode := FProtectedMode_;
end;

function TMem64.ToBytes: TBytes;
begin
  SetLength(Result, Size);
  if Size > 0 then
      CopyPtr(Memory, @Result[0], Size);
end;

function TMem64.ToMD5: TMD5;
begin
  Result := umlMD5(Memory, Size);
end;

procedure TMem64.SetPointerWithProtectedMode(buffPtr: Pointer; const BuffSize: Int64);
begin
  Mapping(buffPtr, BuffSize);
end;

procedure TMem64.Mapping(buffPtr: Pointer; const BuffSize: Int64);
begin
  Clear;
  FMemory := buffPtr;
  FSize := BuffSize;
  FPosition := 0;
  FProtectedMode := True;
end;

procedure TMem64.Mapping(m64: TMS64);
begin
  Mapping(m64.Memory, m64.Size);
end;

procedure TMem64.Mapping(m64: TMem64);
begin
  Mapping(m64.Memory, m64.Size);
end;

function TMem64.PositionAsPtr(const Position_: Int64): Pointer;
begin
  Result := GetOffset(FMemory, Position_);
end;

function TMem64.PositionAsPtr: Pointer;
begin
  Result := GetOffset(FMemory, FPosition);
end;

function TMem64.PosAsPtr(const Position_: Int64): Pointer;
begin
  Result := PositionAsPtr(Position_);
end;

function TMem64.PosAsPtr: Pointer;
begin
  Result := PositionAsPtr();
end;

procedure TMem64.LoadFromStream(stream: TCore_Stream);
begin
  if FProtectedMode then
      Exit;
  Clear;
  stream.Position := 0;
  if CopyFrom(stream, stream.Size) <> stream.Size then
      RaiseInfo('load stream error.');
  Position := 0;
end;

procedure TMem64.LoadFromFile(FileName: SystemString);
var
  stream: TCore_Stream;
begin
  stream := TCore_FileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
      LoadFromStream(stream);
  finally
      DisposeObject(stream);
  end;
end;

procedure TMem64.SaveToStream(stream: TCore_Stream);
const
  ChunkSize = 64 * 1024 * 1024;
var
  p: Pointer;
  j: NativeInt;
  Num: NativeInt;
  Rest: NativeInt;
begin
  if Size > 0 then
    begin
      p := FMemory;
      if Size > ChunkSize then
        begin
          { Calculate number of full chunks that will fit into the buffer }
          Num := Size div ChunkSize;
          { Calculate remaining bytes }
          Rest := Size mod ChunkSize;

          { Process full chunks }
          for j := 0 to Num - 1 do
            begin
              stream.WriteBuffer(p^, ChunkSize);
              p := GetOffset(p, ChunkSize);
            end;

          { Process remaining bytes }
          if Rest > 0 then
            begin
              stream.WriteBuffer(p^, Rest);
              p := GetOffset(p, Rest);
            end;
        end
      else
          stream.WriteBuffer(p^, Size);
    end;
end;

procedure TMem64.SaveToFile(FileName: SystemString);
var
  stream: TCore_Stream;
begin
  stream := TCore_FileStream.Create(FileName, fmCreate);
  try
      SaveToStream(stream);
  finally
      DisposeObject(stream);
  end;
end;

function TMem64.Write64(const buffer; Count: Int64): Int64;
var
  p: Int64;
begin
  if (Count > 0) then
    begin
      p := FPosition;
      p := p + Count;
      if p > 0 then
        begin
          if p > FSize then
            begin
              if FProtectedMode then
                begin
                  Result := 0;
                  Exit;
                end;
              if p > FCapacity then
                  SetCapacity(p);
              FSize := p;
            end;
          CopyPtr(@buffer, GetOffset(FMemory, FPosition), Count);
          FPosition := p;
          Result := Count;
          Exit;
        end;
    end;
  Result := 0;
end;

function TMem64.WritePtr(const p: Pointer; Count: Int64): Int64;
begin
  Result := Write64(p^, Count);
end;

function TMem64.write(const buffer; Count: Int64): Int64;
begin
  Result := Write64(buffer, Count);
end;

function TMem64.WriteBytes(const buffer: TBytes): Int64;
begin
  if Length(buffer) > 0 then
      Result := WritePtr(@buffer[0], Length(buffer))
  else
      Result := 0;
end;

function TMem64.Read64(var buffer; Count: Int64): Int64;
begin
  if Count > 0 then
    begin
      Result := FSize;
      Result := Result - FPosition;
      if Result > 0 then
        begin
          if Result > Count then
              Result := Count;
          CopyPtr(GetOffset(FMemory, FPosition), @buffer, Result);
          inc(FPosition, Result);
          Exit;
        end;
    end;
  Result := 0;
end;

function TMem64.ReadPtr(const p: Pointer; Count: Int64): Int64;
begin
  Result := Read64(p^, Count);
end;

function TMem64.read(var buffer; Count: Int64): Int64;
begin
  Result := Read64(buffer, Count);
end;

function TMem64.Seek(const Offset: Int64; origin: TSeekOrigin): Int64;
begin
  case origin of
    TSeekOrigin.soBeginning: FPosition := Offset;
    TSeekOrigin.soCurrent: inc(FPosition, Offset);
    TSeekOrigin.soEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

function TMem64.CopyFrom(const source: TCore_Stream; Count: Int64): Int64;
const
  MaxBufSize = $F000;
var
  BufSize, n, p: Int64;
begin
  if FProtectedMode then
      RaiseInfo('protected mode');

  if Count = 0 then
      Exit(0);

  if Count < 0 then
    begin
      source.Position := 0;
      Count := source.Size;
    end;

  if source is TMS64 then
    begin
      WritePtr(TMS64(source).PositionAsPtr, Count);
      TMS64(source).Position := TMS64(source).FPosition + Count;
      Result := Count;
      Exit;
    end;

  Result := Count;
  if Count > MaxBufSize then
      BufSize := MaxBufSize
  else
      BufSize := Count;

  p := Position;
  if p + Count > Size then
      Size := p + Count;

  while Count <> 0 do
    begin
      if Count > BufSize then
          n := BufSize
      else
          n := Count;

      // fast copy optimized
      if source.read(PosAsPtr(p)^, n) <> n then
          RaiseInfo('stream read error.');

      inc(p, n);
      dec(Count, n);
    end;
  Position := p;
end;

function TMem64.CopyFrom(const source: TMem64; Count: Int64): Int64;
begin
  if FProtectedMode then
      RaiseInfo('protected mode');
  WritePtr(source.PositionAsPtr, Count);
  source.Position := source.FPosition + Count;
  Result := Count;
end;

procedure TMem64.WriteBool(const buff: Boolean);
begin
  WritePtr(@buff, 1);
end;

procedure TMem64.WriteInt8(const buff: ShortInt);
begin
  WritePtr(@buff, 1);
end;

procedure TMem64.WriteInt16(const buff: SmallInt);
begin
  WritePtr(@buff, 2);
end;

procedure TMem64.WriteInt32(const buff: Integer);
begin
  WritePtr(@buff, 4);
end;

procedure TMem64.WriteInt64(const buff: Int64);
begin
  WritePtr(@buff, 8);
end;

procedure TMem64.WriteInt128(const buff: Int128);
begin
  WritePtr(@buff.b[0], 16);
end;

procedure TMem64.WriteUInt8(const buff: Byte);
begin
  WritePtr(@buff, 1);
end;

procedure TMem64.WriteUInt16(const buff: Word);
begin
  WritePtr(@buff, 2);
end;

procedure TMem64.WriteUInt32(const buff: Cardinal);
begin
  WritePtr(@buff, 4);
end;

procedure TMem64.WriteUInt64(const buff: UInt64);
begin
  WritePtr(@buff, 8);
end;

procedure TMem64.WriteUInt128(const buff: UInt128);
begin
  WritePtr(@buff.b[0], 16);
end;

procedure TMem64.WriteSingle(const buff: Single);
begin
  WritePtr(@buff, 4);
end;

procedure TMem64.WriteDouble(const buff: Double);
begin
  WritePtr(@buff, 8);
end;

procedure TMem64.WriteCurrency(const buff: Currency);
begin
  WriteDouble(buff);
end;

procedure TMem64.WriteString(const buff: TPascalString);
var
  b: TBytes;
begin
  b := buff.Bytes;
  WriteUInt32(Length(b));
  if Length(b) > 0 then
    begin
      WritePtr(@b[0], Length(b));
      SetLength(b, 0);
    end;
end;

procedure TMem64.WriteANSI(const buff: TPascalString);
var
  b: TBytes;
begin
  b := buff.ANSI;
  if Length(b) > 0 then
    begin
      WritePtr(@b[0], Length(b));
      SetLength(b, 0);
    end;
end;

procedure TMem64.WriteANSI(const buff: TPascalString; const L: Integer);
var
  b: TBytes;
begin
  b := buff.ANSI;
  if L > 0 then
    begin
      WritePtr(@b[0], L);
      SetLength(b, 0);
    end;
end;

procedure TMem64.WriteMD5(const buff: TMD5);
begin
  WritePtr(@buff, 16);
end;

function TMem64.ReadBool: Boolean;
begin
  ReadPtr(@Result, 1);
end;

function TMem64.ReadInt8: ShortInt;
begin
  ReadPtr(@Result, 1);
end;

function TMem64.ReadInt16: SmallInt;
begin
  ReadPtr(@Result, 2);
end;

function TMem64.ReadInt32: Integer;
begin
  ReadPtr(@Result, 4);
end;

function TMem64.ReadInt64: Int64;
begin
  ReadPtr(@Result, 8);
end;

function TMem64.ReadInt128: Int128;
begin
  ReadPtr(@Result.b[0], 16);
end;

function TMem64.ReadUInt8: Byte;
begin
  ReadPtr(@Result, 1);
end;

function TMem64.ReadUInt16: Word;
begin
  ReadPtr(@Result, 2);
end;

function TMem64.ReadUInt32: Cardinal;
begin
  ReadPtr(@Result, 4);
end;

function TMem64.ReadUInt64: UInt64;
begin
  ReadPtr(@Result, 8);
end;

function TMem64.ReadUInt128: UInt128;
begin
  ReadPtr(@Result.b[0], 16);
end;

function TMem64.ReadSingle: Single;
begin
  ReadPtr(@Result, 4);
end;

function TMem64.ReadDouble: Double;
begin
  ReadPtr(@Result, 8);
end;

function TMem64.ReadCurrency: Currency;
begin
  Result := ReadDouble();
end;

function TMem64.PrepareReadString: Boolean;
begin
  Result := (Position + 4 <= Size) and (Position + 4 + PCardinal(PositionAsPtr())^ <= Size);
end;

function TMem64.ReadString: TPascalString;
var
  L: Cardinal;
  b: TBytes;
begin
  L := ReadUInt32;
  if L > 0 then
    begin
      try
        SetLength(b, L);
        ReadPtr(@b[0], L);
        Result.Bytes := b;
        SetLength(b, 0);
      except
          Result := '';
      end;
    end;
end;

function TMem64.ReadStringAsBuff: TBytes;
var
  L: Cardinal;
begin
  try
    L := ReadUInt32;
    if L > 0 then
      begin
        SetLength(Result, L);
        ReadPtr(@Result[0], L);
      end
    else
        SetLength(Result, 0);
  except
      SetLength(Result, 0);
  end;
end;

procedure TMem64.IgnoreReadString;
var
  L: Cardinal;
  b: TBytes;
begin
  L := ReadUInt32;
  if L > 0 then
    begin
      try
        SetLength(b, L);
        ReadPtr(@b[0], L);
        SetLength(b, 0);
      except
      end;
    end;
end;

function TMem64.ReadANSI(L: Integer): TPascalString;
var
  b: TBytes;
begin
  if L > 0 then
    begin
      SetLength(b, L);
      ReadPtr(@b[0], L);
      Result.ANSI := b;
      SetLength(b, 0);
    end;
end;

function TMem64.ReadMD5: TMD5;
begin
  ReadPtr(@Result, 16);
end;

procedure TMem64List.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(Items[i]);
  Clear;
end;

{$IFDEF FPC}


constructor TCompressionStream.Create(stream: TCore_Stream);
begin
  inherited Create(clFastest, stream);
end;

constructor TCompressionStream.Create(level: Tcompressionlevel; stream: TCore_Stream);
begin
  inherited Create(level, stream);
end;
{$ENDIF}


function MaxCompressStream(sour, dest: TCore_Stream): Boolean;
var
  cStream: TCompressionStream;
  siz_: Int64;
begin
  Result := False;
  try
    siz_ := sour.Size;
    dest.WriteBuffer(siz_, 8);
    if sour.Size > 0 then
      begin
        sour.Position := 0;
        cStream := TCompressionStream.Create(clMax, dest);
        Result := cStream.CopyFrom(sour, siz_) = siz_;
        DisposeObject(cStream);
      end;
  except
  end;
end;

function FastCompressStream(sour, dest: TCore_Stream): Boolean;
var
  cStream: TCompressionStream;
  siz_: Int64;
begin
  Result := False;
  try
    siz_ := sour.Size;
    dest.WriteBuffer(siz_, 8);
    if sour.Size > 0 then
      begin
        sour.Position := 0;
        cStream := TCompressionStream.Create(clFastest, dest);
        Result := cStream.CopyFrom(sour, siz_) = siz_;
        DisposeObject(cStream);
      end;
  except
  end;
end;

function CompressStream(sour, dest: TCore_Stream): Boolean;
var
  cStream: TCompressionStream;
  siz_: Int64;
begin
  Result := False;
  try
    siz_ := sour.Size;
    dest.WriteBuffer(siz_, 8);
    if sour.Size > 0 then
      begin
        sour.Position := 0;
        cStream := TCompressionStream.Create(clDefault, dest);
        Result := cStream.CopyFrom(sour, siz_) = siz_;
        DisposeObject(cStream);
      end;
  except
  end;
end;

function DecompressStream(DataPtr: Pointer; siz: NativeInt; dest: TCore_Stream): Boolean;
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  m64.SetPointer(DataPtr, siz);
  Result := DecompressStream(m64, dest);
  DisposeObject(m64);
end;

function DecompressStream(sour: TCore_Stream; dest: TCore_Stream): Boolean;
var
  dcStream: TDecompressionStream;
  dSiz: Int64;
  iPos: Int64;
begin
  Result := False;
  sour.ReadBuffer(dSiz, 8);
  if dSiz > 0 then
    begin
      iPos := dest.Position;
      dest.Size := iPos + dSiz;
      dest.Position := iPos;
      try
        dcStream := TDecompressionStream.Create(sour);
        Result := dest.CopyFrom(dcStream, dSiz) = dSiz;
        DisposeObject(dcStream);
      except
      end;
    end;
end;

function DecompressStreamToPtr(sour: TCore_Stream; var dest: Pointer): Boolean;
var
  dcStream: TDecompressionStream;
  dSiz: Int64;
begin
  Result := False;
  try
    sour.ReadBuffer(dSiz, 8);
    if dSiz > 0 then
      begin
        dcStream := TDecompressionStream.Create(sour);
        dest := System.GetMemory(dSiz);
        Result := dcStream.read(dest^, dSiz) = dSiz;
        DisposeObject(dcStream);
      end;
  except
  end;
end;

function CompressFile(sour, dest: SystemString): Boolean;
var
  s_fs, d_fs: TCore_FileStream;
begin
  s_fs := TCore_FileStream.Create(sour, fmOpenRead or fmShareDenyNone);
  d_fs := TCore_FileStream.Create(dest, fmCreate);
  Result := CompressStream(s_fs, d_fs);
  DisposeObject(s_fs);
  DisposeObject(d_fs);
end;

function DecompressFile(sour, dest: SystemString): Boolean;
var
  s_fs, d_fs: TCore_FileStream;
begin
  s_fs := TCore_FileStream.Create(sour, fmOpenRead or fmShareDenyNone);
  d_fs := TCore_FileStream.Create(dest, fmCreate);
  Result := DecompressStream(s_fs, d_fs);
  DisposeObject(s_fs);
  DisposeObject(d_fs);
end;

function SelectCompressStream(const scm: TSelectCompressionMethod; const sour, dest: TCore_Stream): Boolean;
var
  scm_b: Byte;
  siz_: Int64;
begin
  Result := False;
  scm_b := Byte(scm);
  if dest.write(scm_b, 1) <> 1 then
      Exit;
  sour.Position := 0;

  try
    case scm of
      scmNone:
        begin
          siz_ := sour.Size;
          dest.write(siz_, 8);
          Result := dest.CopyFrom(sour, siz_) = siz_;
        end;
      scmZLIB: Result := CompressStream(sour, dest);
      scmZLIB_Fast: Result := FastCompressStream(sour, dest);
      scmZLIB_Max: Result := MaxCompressStream(sour, dest);
      scmDeflate: Result := DeflateCompressStream(sour, dest);
      scmBRRC: Result := BRRCCompressStream(sour, dest);
    end;
  except
  end;
end;

function SelectDecompressStream(const sour, dest: TCore_Stream): Boolean;
var
  scm: TSelectCompressionMethod;
begin
  Result := SelectDecompressStream(sour, dest, scm);
end;

function SelectDecompressStream(const sour, dest: TCore_Stream; var scm: TSelectCompressionMethod): Boolean;
var
  scm_: Byte;
  siz_: Int64;
begin
  Result := False;
  if sour.read(scm_, 1) <> 1 then
      Exit;
  scm := TSelectCompressionMethod(scm_);
  try
    case scm of
      scmNone:
        begin
          if sour.read(siz_, 8) <> 8 then
              Exit;
          Result := dest.CopyFrom(sour, siz_) = siz_;
        end;
      scmZLIB, scmZLIB_Fast, scmZLIB_Max: Result := DecompressStream(sour, dest);
      scmDeflate: Result := DeflateDecompressStream(sour, dest);
      scmBRRC: Result := BRRCDecompressStream(sour, dest);
    end;
  except
  end;
end;

procedure ParallelCompressMemory(const ThNum: Integer; const scm: TSelectCompressionMethod; const StripNum_: Integer; const sour: TMS64; const dest: TCore_Stream);
var
  StripNum: Integer;
  sourStrips: TStream64List;
  StripArry: array of TMS64;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    SelectCompressStream(scm, sourStrips[pass], StripArry[pass]);
  end;
{$ENDIF FPC}
{$ENDIF Parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to Length(StripArry) - 1 do
      begin
        SelectCompressStream(scm, sourStrips[pass], StripArry[pass]);
      end;
  end;
  procedure BuildBuff;
  var
    strip_siz, strip_m: Int64;
    p: Int64;
    m64: TMS64;
    i: Integer;
  begin
    sourStrips := TStream64List.Create;
    strip_siz := sour.Size div StripNum;
    p := 0;
    while True do
      begin
        if p + strip_siz < sour.Size then
          begin
            m64 := TMS64.Create;
            m64.SetPointerWithProtectedMode(sour.PositionAsPtr(p), strip_siz);
            sourStrips.Add(m64);
            inc(p, strip_siz);
          end
        else
          begin
            if sour.Size - p > 0 then
              begin
                m64 := TMS64.Create;
                m64.SetPointerWithProtectedMode(sour.PositionAsPtr(p), sour.Size - p);
                sourStrips.Add(m64);
              end;
            break;
          end;
      end;

    SetLength(StripArry, sourStrips.Count);
    for i := 0 to sourStrips.Count - 1 do
        StripArry[i] := TMS64.CustomCreate(1024);
  end;

  procedure BuildOutput;
  var
    L: Integer;
    siz_: Int64;
    i: Integer;
  begin
    L := Length(StripArry);
    dest.write(L, 4);
    for i := 0 to L - 1 do
      begin
        siz_ := StripArry[i].Size;
        dest.write(siz_, 8);
        dest.write(StripArry[i].Memory^, StripArry[i].Size);

        DisposeObject(sourStrips[i]);
        DisposeObject(StripArry[i]);
      end;
  end;

  procedure FreeBuff;
  begin
    DisposeObject(sourStrips);
    SetLength(StripArry, 0);
  end;

begin
  if StripNum_ <= 0 then
      StripNum := 1
  else
      StripNum := StripNum_;
  BuildBuff;

  if Length(StripArry) < ThNum then
    begin
      DoFor;
    end
  else
    begin
{$IFDEF Parallel}
{$IFDEF FPC}
      FPCParallelFor(ThNum, True, 0, Length(StripArry) - 1, Nested_ParallelFor);
{$ELSE FPC}
      DelphiParallelFor(ThNum, True, 0, Length(StripArry) - 1, procedure(pass: Integer)
        begin
          SelectCompressStream(scm, sourStrips[pass], StripArry[pass]);
        end);
{$ENDIF FPC}
{$ELSE Parallel}
      DoFor;
{$ENDIF Parallel}
    end;
  BuildOutput;
  FreeBuff;
end;

procedure ParallelCompressMemory(const scm: TSelectCompressionMethod; const StripNum_: Integer; const sour: TMS64; const dest: TCore_Stream);
begin
  ParallelCompressMemory(umlMin(4, Get_Parallel_Granularity), scm, StripNum_, sour, dest);
end;

procedure ParallelCompressMemory(const scm: TSelectCompressionMethod; const sour: TMS64; const dest: TCore_Stream);
begin
  ParallelCompressMemory(scm, sour.Size div (16 * 1024), sour, dest);
end;

procedure ParallelCompressMemory(const sour: TMS64; const dest: TCore_Stream);
begin
  ParallelCompressMemory(scmZLIB, sour, dest);
end;

procedure ParallelDecompressStream(const ThNum: Integer; const sour_, dest_: TCore_Stream);
type
  TPara_strip_ = record
    sour, dest: TMS64;
  end;

  PPara_strip_ = ^TPara_strip_;
var
  StripArry: array of TPara_strip_;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    SelectDecompressStream(StripArry[pass].sour, StripArry[pass].dest);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
{$ENDIF Parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to Length(StripArry) - 1 do
      begin
        SelectDecompressStream(StripArry[pass].sour, StripArry[pass].dest);
      end;
  end;
  function BuildBuff_Stream64(stream: TMS64): Boolean;
  var
    strip_num: Integer;
    i: Integer;
    p, siz_, ss: Int64;
  begin
    Result := False;
    ss := stream.Size;
    p := stream.Position;
    if p + 4 > ss then
        Exit;
    strip_num := PInteger(stream.PositionAsPtr(p))^;
    inc(p, 4);

    SetLength(StripArry, strip_num);
    for i := 0 to strip_num - 1 do
      begin
        StripArry[i].sour := TMS64.Create;
        if p + 4 > ss then
            Exit;
        siz_ := PInt64(stream.PositionAsPtr(p))^;
        inc(p, 8);
        if p + siz_ > ss then
            Exit;
        StripArry[i].sour.SetPointerWithProtectedMode(stream.PositionAsPtr(p), siz_);
        inc(p, siz_);
        StripArry[i].sour.Position := 0;
        StripArry[i].dest := TMS64.CustomCreate(1024);
      end;
    stream.Position := p;
    Result := True;
  end;

  function BuildBuff_Stream(stream: TCore_Stream): Boolean;
  var
    strip_num: Integer;
    i: Integer;
    siz_: Int64;
  begin
    Result := False;
    if stream.read(strip_num, 4) <> 4 then
        Exit;

    SetLength(StripArry, strip_num);
    for i := 0 to strip_num - 1 do
      begin
        StripArry[i].sour := TMS64.CustomCreate(1024);
        StripArry[i].dest := TMS64.CustomCreate(1024);
      end;

    for i := 0 to strip_num - 1 do
      begin
        if stream.read(siz_, 8) <> 8 then
            Exit;
        if StripArry[i].sour.CopyFrom(stream, siz_) <> siz_ then
            Exit;
        StripArry[i].sour.Position := 0;
      end;
    Result := True;
  end;

  procedure BuildOutput;
  var
    i: Integer;
  begin
    for i := 0 to Length(StripArry) - 1 do
      begin
        dest_.write(StripArry[i].dest.Memory^, StripArry[i].dest.Size);
        DisposeObject(StripArry[i].sour);
        DisposeObject(StripArry[i].dest);
      end;
  end;

  procedure FreeBuff;
  begin
    SetLength(StripArry, 0);
  end;

var
  preDone: Boolean;
begin
  if sour_ is TMS64 then
      preDone := BuildBuff_Stream64(TMS64(sour_))
  else
      preDone := BuildBuff_Stream(sour_);

  if not preDone then
    begin
      FreeBuff;
      Exit;
    end;

  if Length(StripArry) < ThNum then
    begin
      DoFor;
    end
  else
    begin
{$IFDEF Parallel}
{$IFDEF FPC}
      FPCParallelFor(ThNum, True, 0, Length(StripArry) - 1, Nested_ParallelFor);
{$ELSE FPC}
      DelphiParallelFor(ThNum, True, 0, Length(StripArry) - 1, procedure(pass: Integer)
        begin
          SelectDecompressStream(StripArry[pass].sour, StripArry[pass].dest);
        end);
{$ENDIF FPC}
{$ELSE Parallel}
      DoFor;
{$ENDIF Parallel}
    end;
  BuildOutput;
  FreeBuff;
end;

procedure ParallelDecompressStream(const sour_, dest_: TCore_Stream);
begin
  ParallelDecompressStream(umlMin(4, Get_Parallel_Granularity), sour_, dest_);
end;

procedure ParallelCompressFile(const sour, dest: SystemString);
var
  s_fs: TMS64;
  d_fs: TCore_FileStream;
begin
  s_fs := TMS64.Create;
  s_fs.LoadFromFile(sour);
  d_fs := TCore_FileStream.Create(dest, fmCreate);
  ParallelCompressMemory(s_fs, d_fs);
  DisposeObject(s_fs);
  DisposeObject(d_fs);
end;

procedure ParallelDecompressFile(const sour, dest: SystemString);
var
  s_fs: TMS64;
  d_fs: TCore_FileStream;
begin
  s_fs := TMS64.Create;
  s_fs.LoadFromFile(sour);
  d_fs := TCore_FileStream.Create(dest, fmCreate);
  ParallelDecompressStream(s_fs, d_fs);
  DisposeObject(s_fs);
  DisposeObject(d_fs);
end;

function CompressUTF8(const sour_: TBytes): TBytes;
var
  cStream: TCompressionStream;
  dest: TMS64;
begin
  if Length(sour_) > 10 then
    begin
      dest := TMS64.Create;
      cStream := TCompressionStream.Create(clMax, dest);
      cStream.write(sour_[0], Length(sour_));
      DisposeObject(cStream);
      if dest.Size + 6 < Length(sour_) then
        begin
          SetLength(Result, dest.Size + 6);
          Result[0] := $FF;
          Result[1] := $FF;
          PInteger(@Result[2])^ := Length(sour_);
          CopyPtr(dest.Memory, @Result[6], dest.Size);
        end
      else
          Result := sour_;
      DisposeObject(dest);
    end
  else
      Result := sour_;
end;

function DecompressUTF8(const sour_: TBytes): TBytes;
var
  dcStream: TDecompressionStream;
  sour: TMS64;
  siz: Integer;
begin
  if Length(sour_) > 6 then
    begin
      if (sour_[0] = $FF) and (sour_[1] = $FF) then
        begin
          siz := PInteger(@sour_[2])^;
          sour := TMS64.Create();
          sour.SetPointer(@sour_[6], Length(sour_) - 6);
          dcStream := TDecompressionStream.Create(sour);
          SetLength(Result, siz);
          dcStream.read(Result[0], siz);
          DisposeObject(sour);
          DisposeObject(dcStream);
        end
      else
          Result := sour_;
    end
  else
      Result := sour_;
end;

procedure test_utf8;
var
  buff: TBytes;
  s: TPascalString;
begin
  buff := CompressUTF8(TPascalString('123456789abcdefg1111111111111111111111111111111111111').Bytes);
  s.Bytes := DecompressUTF8(buff);
end;

procedure StreamWriteBool(const stream: TCore_Stream; const buff: Boolean);
begin
  stream.write(buff, 1);
end;

procedure StreamWriteInt8(const stream: TCore_Stream; const buff: ShortInt);
begin
  stream.write(buff, 1);
end;

procedure StreamWriteInt16(const stream: TCore_Stream; const buff: SmallInt);
begin
  stream.write(buff, 2);
end;

procedure StreamWriteInt32(const stream: TCore_Stream; const buff: Integer);
begin
  stream.write(buff, 4);
end;

procedure StreamWriteInt64(const stream: TCore_Stream; const buff: Int64);
begin
  stream.write(buff, 8);
end;

procedure StreamWriteInt128(const stream: TCore_Stream; const buff: Int128);
begin
  stream.write(buff.b[0], 16);
end;

procedure StreamWriteUInt8(const stream: TCore_Stream; const buff: Byte);
begin
  stream.write(buff, 1);
end;

procedure StreamWriteUInt16(const stream: TCore_Stream; const buff: Word);
begin
  stream.write(buff, 2);
end;

procedure StreamWriteUInt32(const stream: TCore_Stream; const buff: Cardinal);
begin
  stream.write(buff, 4);
end;

procedure StreamWriteUInt64(const stream: TCore_Stream; const buff: UInt64);
begin
  stream.write(buff, 8);
end;

procedure StreamWriteUInt128(const stream: TCore_Stream; const buff: UInt128);
begin
  stream.write(buff.b[0], 16);
end;

procedure StreamWriteSingle(const stream: TCore_Stream; const buff: Single);
begin
  stream.write(buff, 4);
end;

procedure StreamWriteDouble(const stream: TCore_Stream; const buff: Double);
begin
  stream.write(buff, 8);
end;

procedure StreamWriteCurrency(const stream: TCore_Stream; const buff: Currency);
begin
  StreamWriteDouble(stream, buff);
end;

procedure StreamWriteString(const stream: TCore_Stream; const buff: TPascalString);
var
  b: TBytes;
begin
  b := buff.Bytes;
  StreamWriteUInt32(stream, Length(b));
  if Length(b) > 0 then
    begin
      stream.write(b[0], Length(b));
      SetLength(b, 0);
    end;
end;

function ComputeStreamWriteStringSize(buff: TPascalString): Integer;
var
  b: TBytes;
begin
  b := buff.Bytes;
  Result := 4 + Length(b);
  SetLength(b, 0);
end;

procedure StreamWriteMD5(const stream: TCore_Stream; const buff: TMD5);
begin
  stream.write(buff, 16);
end;

function StreamReadBool(const stream: TCore_Stream): Boolean;
begin
  stream.read(Result, 1);
end;

function StreamReadInt8(const stream: TCore_Stream): ShortInt;
begin
  stream.read(Result, 1);
end;

function StreamReadInt16(const stream: TCore_Stream): SmallInt;
begin
  stream.read(Result, 2);
end;

function StreamReadInt32(const stream: TCore_Stream): Integer;
begin
  stream.read(Result, 4);
end;

function StreamReadInt64(const stream: TCore_Stream): Int64;
begin
  stream.read(Result, 8);
end;

function StreamReadInt128(const stream: TCore_Stream): Int128;
begin
  stream.read(Result.b[0], 16);
end;

function StreamReadUInt8(const stream: TCore_Stream): Byte;
begin
  stream.read(Result, 1);
end;

function StreamReadUInt16(const stream: TCore_Stream): Word;
begin
  stream.read(Result, 2);
end;

function StreamReadUInt32(const stream: TCore_Stream): Cardinal;
begin
  stream.read(Result, 4);
end;

function StreamReadUInt64(const stream: TCore_Stream): UInt64;
begin
  stream.read(Result, 8);
end;

function StreamReadUInt128(const stream: TCore_Stream): UInt128;
begin
  stream.read(Result.b[0], 16);
end;

function StreamReadSingle(const stream: TCore_Stream): Single;
begin
  stream.read(Result, 4);
end;

function StreamReadDouble(const stream: TCore_Stream): Double;
begin
  stream.read(Result, 8);
end;

function StreamReadCurrency(const stream: TCore_Stream): Currency;
begin
  Result := StreamReadDouble(stream);
end;

function StreamReadString(const stream: TCore_Stream): TPascalString;
var
  L: Cardinal;
  b: TBytes;
begin
  try
    L := StreamReadUInt32(stream);
    if L > 0 then
      begin
        SetLength(b, L);
        stream.read(b[0], L);
        Result.Bytes := b;
        SetLength(b, 0);
      end;
  except
      Result := '';
  end;
end;

function StreamReadStringAsBuff(const stream: TCore_Stream): TBytes;
var
  L: Cardinal;
begin
  try
    L := StreamReadUInt32(stream);
    if L > 0 then
      begin
        SetLength(Result, L);
        stream.read(Result[0], L);
      end
    else
        SetLength(Result, 0);
  except
      SetLength(Result, 0);
  end;
end;

procedure StreamIgnoreReadString(const stream: TCore_Stream);
var
  L: Cardinal;
  b: TBytes;
begin
  try
    L := StreamReadUInt32(stream);
    if L > 0 then
      begin
        SetLength(b, L);
        stream.read(b[0], L);
        SetLength(b, 0);
      end;
  except
  end;
end;

function StreamReadMD5(const stream: TCore_Stream): TMD5;
begin
  stream.read(Result, 16);
end;

procedure DoStatus(const v: TMS64);
var
  p: PByte;
  i: Integer;
  n: SystemString;
begin
  p := v.Memory;
  for i := 0 to v.Size - 1 do
    begin
      if n <> '' then
          n := n + ',' + IntToStr(p^)
      else
          n := IntToStr(p^);
      inc(p);
    end;
  DoStatus(IntToHex(NativeInt(v), SizeOf(Pointer)) + ':' + n);
end;

procedure DoStatus(const v: TMem64);
var
  p: PByte;
  i: Integer;
  n: SystemString;
begin
  p := v.Memory;
  for i := 0 to v.Size - 1 do
    begin
      if n <> '' then
          n := n + ',' + IntToStr(p^)
      else
          n := IntToStr(p^);
      inc(p);
    end;
  DoStatus(IntToHex(NativeInt(v), SizeOf(Pointer)) + ':' + n);
end;

initialization

end.
 
