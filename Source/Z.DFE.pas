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
{ * Serializers Data Frame engine                                              * }
{ ****************************************************************************** }
unit Z.DFE;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses Types,
  Z.Core, Z.UnicodeMixedLib, Z.PascalStrings, Z.UPascalStrings,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ELSE FPC}
  Z.Delphi.JsonDataObjects,
{$ENDIF FPC}
  Z.ListEngine,
  Z.MemoryStream, Z.Cipher,
  Z.Status, Z.Geometry.Low, Z.Geometry2D, Z.Geometry3D, Z.Json,
  Z.Expression, Z.OpCode, Z.TextDataEngine, Z.Number,
  Z.Compress, Z.Int128;

type
  TDFE = class;
  TDataFrameEngine = TDFE;
  TDFEngine = TDFE;
  TDF = TDFE;
  TDataFrame = TDFE;

  TDFBase = class(TCore_Object_Intermediate)
  protected
    FID: Byte;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;
    procedure LoadFromStream(stream: TCore_Stream); virtual; abstract;
    procedure SaveToStream(stream: TCore_Stream); virtual; abstract;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); virtual; abstract;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); virtual; abstract;
    function ComputeEncodeSize: Int64; virtual; abstract;
  end;

  TDFString = class(TDFBase)
  public
    Buffer: TBytes;

    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;
  end;

  TDFInteger = class(TDFBase)
  protected
    FBuffer: Integer;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Integer read FBuffer write FBuffer;
  end;

  TDFCardinal = class(TDFBase)
  protected
    FBuffer: Cardinal;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Cardinal read FBuffer write FBuffer;
  end;

  TDFWord = class(TDFBase)
  protected
    FBuffer: Word;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Word read FBuffer write FBuffer;
  end;

  TDFByte = class(TDFBase)
  protected
    FBuffer: Byte;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Byte read FBuffer write FBuffer;
  end;

  TDFSingle = class(TDFBase)
  protected
    FBuffer: Single;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Single read FBuffer write FBuffer;
  end;

  TDFDouble = class(TDFBase)
  protected
    FBuffer: Double;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Double read FBuffer write FBuffer;
  end;

  TDFArrayInteger = class(TDFBase)
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Integer);
    function Count: Integer;
    procedure WriteArray(const arry_: array of Integer);

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer(index_: Integer): Integer;
    procedure SetBuffer(index_: Integer; Value: Integer);
    property Buffer[index_: Integer]: Integer read GetBuffer write SetBuffer; default;
    property Buffer__: TMS64 read FBuffer;
  end;

  TDFArrayShortInt = class(TDFBase)
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: ShortInt);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: ShortInt);
    function Count: Integer;
    procedure WriteArray(const arry_: array of ShortInt);

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer(index_: Integer): ShortInt;
    procedure SetBuffer(index_: Integer; Value: ShortInt);
    property Buffer[index_: Integer]: ShortInt read GetBuffer write SetBuffer; default;
    property Buffer__: TMS64 read FBuffer;
  end;

  TDFArrayByte = class(TDFBase)
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Byte);
    procedure AddPtrBuff(p: PByte; Size_: Integer);
    procedure AddI64(v: Int64);
    procedure AddU64(v: UInt64);
    procedure Addi(v: Integer);
    procedure AddWord(v: Word);
    function Count: Int64;
    property Size: Int64 read Count;
    procedure WriteArray(const arry_: array of Byte);
    procedure SetArray(const arry_: array of Byte);
    procedure SetBuff(p: PByte; Size_: Integer);
    procedure GetBuff(p: PByte);

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer(index_: Integer): Byte;
    procedure SetBuffer(index_: Integer; Value: Byte);
    property Buffer[index_: Integer]: Byte read GetBuffer write SetBuffer; default;
    property Buffer__: TMS64 read FBuffer;
  end;

  TDFArraySingle = class(TDFBase)
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Single);
    function Count: Integer;
    procedure WriteArray(const arry_: array of Single);

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer(index_: Integer): Single;
    procedure SetBuffer(index_: Integer; Value: Single);
    property Buffer[index_: Integer]: Single read GetBuffer write SetBuffer; default;
    property Buffer__: TMS64 read FBuffer;
  end;

  TDFArrayDouble = class(TDFBase)
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Double);
    function Count: Integer;
    procedure WriteArray(const arry_: array of Double);

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer(index_: Integer): Double;
    procedure SetBuffer(index_: Integer; Value: Double);
    property Buffer[index_: Integer]: Double read GetBuffer write SetBuffer; default;
    property Buffer__: TMS64 read FBuffer;
  end;

  TDFArrayInt64 = class(TDFBase)
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Int64);
    function Count: Integer;
    procedure WriteArray(const arry_: array of Int64);

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer(index_: Integer): Int64;
    procedure SetBuffer(index_: Integer; Value: Int64);
    property Buffer[index_: Integer]: Int64 read GetBuffer write SetBuffer; default;
    property Buffer__: TMS64 read FBuffer;
  end;

  TDFArrayInt128 = class(TDFBase)
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Int128);
    function Count: Integer;
    procedure WriteArray(const arry_: array of Int128);

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer(index_: Integer): Int128;
    procedure SetBuffer(index_: Integer; Value: Int128);
    property Buffer[index_: Integer]: Int128 read GetBuffer write SetBuffer; default;
    property Buffer__: TMS64 read FBuffer;
  end;

  TDFStream = class(TDFBase)
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer: TCore_Stream;
    procedure SetBuffer(Value_: TCore_Stream);
    property Buffer: TCore_Stream read GetBuffer write SetBuffer;
    property Buffer64: TMS64 read FBuffer;
  end;

  TDFVariant = class(TDFBase)
  protected
    FBuffer: Variant;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Variant read FBuffer write FBuffer;
  end;

  TDFInt64 = class(TDFBase)
  protected
    FBuffer: Int64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Int64 read FBuffer write FBuffer;
  end;

  TDFUInt64 = class(TDFBase)
  protected
    FBuffer: UInt64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: UInt64 read FBuffer write FBuffer;
  end;

  TDFInt128 = class(TDFBase)
  protected
    FBuffer: Int128;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Int128 read FBuffer write FBuffer;
  end;

  TDFUInt128 = class(TDFBase)
  protected
    FBuffer: UInt128;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCore_Stream); override;
    procedure SaveToStream(stream: TCore_Stream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: UInt128 read FBuffer write FBuffer;
  end;

  TDFEReader = class(TCore_Object_Intermediate)
  private
    FOwner: TDFE;
    FIndex: Integer;
  public
    constructor Create(Owner_: TDFE);
    destructor Destroy; override;
    property Index: Integer read FIndex write FIndex;
    property Owner: TDFE read FOwner;
    function IsEnd: Boolean;
    function NotEnd: Boolean;
    procedure Next;
    procedure GoNext;

    function ReadString: SystemString;
    function ReadInteger: Integer;
    function ReadCardinal: Cardinal;
    function ReadWord: Word;
    function ReadBool: Boolean;
    function ReadBoolean: Boolean;
    function ReadByte: Byte;
    function ReadSingle: Single;
    function ReadDouble: Double;
    function ReadArrayInteger: TDFArrayInteger;
    function ReadArrayShortInt: TDFArrayShortInt;
    function ReadArrayByte: TDFArrayByte;
    function ReadMD5: TMD5;
    function ReadArraySingle: TDFArraySingle;
    function ReadArrayDouble: TDFArrayDouble;
    function ReadArrayInt64: TDFArrayInt64;
    procedure ReadMem64(output: TMem64);
    procedure ReadStream(output: TCore_Stream);
    function ReadVariant: Variant;
    function ReadInt64: Int64;
    function ReadUInt64: UInt64;
    function ReadArrayInt128: TDFArrayInt128;
    function ReadInt128: Int128;
    function ReadUInt128: UInt128;
    procedure ReadStrings(output: TCore_Strings);
    procedure ReadListStrings(output: TListString);
    procedure ReadPascalStrings(output: TPascalStringList); overload;
    procedure ReadPascalStrings(var output: U_StringArray); overload;
    procedure ReadDataFrame(output: TDFE);
    procedure ReadDF(output: TDFE);
    procedure ReadDFE(output: TDFE);
    procedure ReadHashStringList(output: THashStringList);
    procedure ReadVariantList(output: THashVariantList);
    procedure ReadJson(output: TZ_JsonObject); overload;
{$IFDEF DELPHI} procedure ReadJson(output: TJsonObject); overload; {$ENDIF DELPHI}
    function ReadRect: TRect;
    function ReadRectf: TRectf;
    function ReadPoint: TPoint;
    function ReadPointf: TPointf;
    function ReadVector: TVector;
    function ReadAffineVector: TAffineVector;
    function ReadVec3: TVec3;
    function ReadVec4: TVec4;
    function ReadVector3: TVector3;
    function ReadVector4: TVector4;
    function ReadMat4: TMat4;
    function ReadMatrix4: TMatrix4;
    function Read2DPoint: T2DPoint;
    function ReadVec2: TVec2;
    function ReadRectV2: TRectV2;
    function ReadPointer: UInt64;
    function ReadPtr: UInt64;
    // auto read from stream data
    procedure Read(var Buf_; Count_: Int64); overload;
    procedure ReadNM(output: TNumberModule);
    procedure ReadNMPool(output: TNumberModulePool);
    procedure ReadOpCode(output: TOpCode);
    procedure ReadSectionText(output: THashTextEngine);
    procedure ReadTextSection(output: THashTextEngine);
    // read as TDFBase
    function Read: TDFBase; overload;
    // return current TDFBase
    function Current: TDFBase;
  end;

  TRunTimeDataType = (rdtString, rdtInteger, rdtLongWord, rdtWORD, rdtByte, rdtSingle, rdtDouble,
    rdtArrayInteger, rdtArraySingle, rdtArrayDouble, rdtStream, rdtVariant, rdtInt64, rdtArrayShortInt, rdtCardinal, rdtUInt64, rdtArrayByte,
    rdtArrayInt64,
    rdtArrayInt128, rdtInt128, rdtUInt128
    );

  TDFE_DataList_ = TGenericsList<TDFBase>;

  TDFE_DataList = class(TDFE_DataList_)
  public
    Owner: TDFE;
    function Add_DFBase(Data_: TDFBase): TDFBase;
    procedure Clear;
  end;

  TDFE = class(TCore_Object_Intermediate)
  private
    FBit_64_Condition: Int64;
    FDataList: TDFE_DataList;
    FReader: TDFEReader;
    FCompressorDeflate: TCompressorDeflate;
    FCompressorBRRC: TCompressorBRRC;
    FIsChanged: Boolean;
    function DataTypeToByte(v: TRunTimeDataType): Byte;
    function ByteToDataType(v: Byte): TRunTimeDataType;
  public
    constructor Create;
    destructor Destroy; override;
    function DelayFree: TDFE;

    property Reader: TDFEReader read FReader;
    property R: TDFEReader read FReader;
    property IsChanged: Boolean read FIsChanged write FIsChanged;
    // Bit_64_Condition determines the data format based on conditions, whether it is 32-bit or 64-bit
    property Bit_64_Condition: Int64 read FBit_64_Condition write FBit_64_Condition;
    procedure SwapInstance(source: TDFE);

    procedure Clear;
    function AddData(v: TRunTimeDataType): TDFBase;
    function GetData(index_: Integer): TDFBase;
    function GetDataInfo(Obj_: TDFBase): SystemString;
    function Count: Integer;
    function Delete(index_: Integer): Boolean;
    function DeleteFirst: Boolean;
    function DeleteLast: Boolean; overload;
    function DeleteLastCount(num_: Integer): Boolean; overload;
    function DeleteCount(index_, Count_: Integer): Boolean;
    function Append(source: TDFE): TDFE;
    function Assign(source: TDFE): TDFE;
    function Clone: TDFE;

    function WriteString(v: SystemString): TDFE; overload;
    function WriteString(v: TPascalString): TDFE; overload;
    function WriteString(const Fmt: SystemString; const Args: array of const): TDFE; overload;
    function WriteInteger(v: Integer): TDFE;
    function WriteCardinal(v: Cardinal): TDFE;
    function WriteWORD(v: Word): TDFE;
    function WriteBool(v: Boolean): TDFE;
    function WriteBoolean(v: Boolean): TDFE;
    function WriteByte(v: Byte): TDFE;
    function WriteSingle(v: Single): TDFE;
    function WriteDouble(v: Double): TDFE;
    function WriteArrayInteger: TDFArrayInteger;
    function WriteArrayShortInt: TDFArrayShortInt;
    function WriteArrayByte: TDFArrayByte;
    function WriteMD5(md5: TMD5): TDFE;
    function WriteArraySingle: TDFArraySingle;
    function WriteArrayDouble: TDFArrayDouble;
    function WriteArrayInt64: TDFArrayInt64;
    function WriteMem64(v: TMem64): TDFE;
    function WriteStream(v: TCore_Stream): TDFE; overload;
    function WriteStream(v: TMS64; bPos_, Size_: Int64): TDFE; overload;
    function WriteVariant(v: Variant): TDFE;
    function WriteInt64(v: Int64): TDFE;
    function WriteUInt64(v: UInt64): TDFE;
    function WriteArrayInt128: TDFArrayInt128;
    function WriteInt128(v: Int128): TDFE;
    function WriteUInt128(v: UInt128): TDFE;
    function WriteStrings(v: TCore_Strings): TDFE;
    function WriteListStrings(v: TListString): TDFE;
    function WritePascalStrings(v: TPascalStringList): TDFE; overload;
    function WritePascalStrings(v: U_StringArray): TDFE; overload;
    function WriteDataFrame(v: TDFE): TDFE;
    function WriteDataFrameCompressed(v: TDFE): TDFE; // select compresssion
    function WriteDataFrameZLib(v: TDFE): TDFE;       // zlib compression
    function WriteDF(v: TDFE): TDFE;
    function WriteDFE(v: TDFE): TDFE;
    function WriteHashStringList(v: THashStringList): TDFE;
    function WriteVariantList(v: THashVariantList): TDFE;
    function WriteJson(v: TZ_JsonObject): TDFE; overload;
    function WriteJson(v: TZ_JsonObject; Formated_: Boolean): TDFE; overload;
{$IFDEF DELPHI} function WriteJson(v: TJsonObject): TDFE; overload; {$ENDIF DELPHI}
    function WriteFile(fn: SystemString): TDFE;
    function WriteRect(v: TRect): TDFE;
    function WriteRectf(v: TRectf): TDFE;
    function WritePoint(v: TPoint): TDFE;
    function WritePointf(v: TPointf): TDFE;
    function WriteVector(v: TVector): TDFE;
    function WriteAffineVector(v: TAffineVector): TDFE;
    function WriteVec4(v: TVec4): TDFE;
    function WriteVec3(v: TVec3): TDFE;
    function WriteVector4(v: TVector4): TDFE;
    function WriteVector3(v: TVector3): TDFE;
    function WriteMat4(v: TMat4): TDFE;
    function WriteMatrix4(v: TMatrix4): TDFE;
    function Write2DPoint(v: T2DPoint): TDFE;
    function WriteVec2(v: TVec2): TDFE;
    function WriteRectV2(v: TRectV2): TDFE;
    function WritePointer(v: Pointer): TDFE; overload;
    function WritePointer(v: UInt64): TDFE; overload;
    function WritePtr(v: Pointer): TDFE; overload;
    function WritePtr(v: UInt64): TDFE; overload;
    // auto append new stream and write
    function write(const Buf_; Count_: Int64): TDFE;
    function WriteNM(NM: TNumberModule): TDFE;
    function WriteNMPool(NMPool: TNumberModulePool): TDFE;
    function WriteOpCode(v: TOpCode): TDFE;
    function WriteSectionText(v: THashTextEngine): TDFE;
    function WriteTextSection(v: THashTextEngine): TDFE;

    function ReadString(index_: Integer): SystemString;
    function ReadInteger(index_: Integer): Integer;
    function ReadCardinal(index_: Integer): Cardinal;
    function ReadWord(index_: Integer): Word;
    function ReadBool(index_: Integer): Boolean;
    function ReadBoolean(index_: Integer): Boolean;
    function ReadByte(index_: Integer): Byte;
    function ReadSingle(index_: Integer): Single;
    function ReadDouble(index_: Integer): Double;
    function ReadArrayInteger(index_: Integer): TDFArrayInteger;
    function ReadArrayShortInt(index_: Integer): TDFArrayShortInt;
    function ReadArrayByte(index_: Integer): TDFArrayByte;
    function ReadMD5(index_: Integer): TMD5;
    function ReadArraySingle(index_: Integer): TDFArraySingle;
    function ReadArrayDouble(index_: Integer): TDFArrayDouble;
    function ReadArrayInt64(index_: Integer): TDFArrayInt64;
    procedure ReadMem64(index_: Integer; output: TMem64);
    procedure ReadStream(index_: Integer; output: TCore_Stream);
    function ReadVariant(index_: Integer): Variant;
    function ReadInt64(index_: Integer): Int64;
    function ReadUInt64(index_: Integer): UInt64;
    function ReadArrayInt128(index_: Integer): TDFArrayInt128;
    function ReadInt128(index_: Integer): Int128;
    function ReadUInt128(index_: Integer): UInt128;
    procedure ReadStrings(index_: Integer; output: TCore_Strings);
    procedure ReadListStrings(index_: Integer; output: TListString);
    procedure ReadPascalStrings(index_: Integer; output: TPascalStringList); overload;
    procedure ReadPascalStrings(index_: Integer; var output: U_StringArray); overload;
    procedure ReadDataFrame(index_: Integer; output: TDFE);
    procedure ReadDF(index_: Integer; output: TDFE);
    procedure ReadDFE(index_: Integer; output: TDFE);
    procedure ReadHashStringList(index_: Integer; output: THashStringList);
    procedure ReadVariantList(index_: Integer; output: THashVariantList);
    procedure ReadJson(index_: Integer; output: TZ_JsonObject); overload;
{$IFDEF DELPHI} procedure ReadJson(index_: Integer; output: TJsonObject); overload; {$ENDIF DELPHI}
    function ReadRect(index_: Integer): TRect;
    function ReadRectf(index_: Integer): TRectf;
    function ReadPoint(index_: Integer): TPoint;
    function ReadPointf(index_: Integer): TPointf;
    function ReadVector(index_: Integer): TVector;
    function ReadAffineVector(index_: Integer): TAffineVector;
    function ReadVec3(index_: Integer): TVec3;
    function ReadVec4(index_: Integer): TVec4;
    function ReadVector3(index_: Integer): TVector3;
    function ReadVector4(index_: Integer): TVector4;
    function ReadMat4(index_: Integer): TMat4;
    function ReadMatrix4(index_: Integer): TMatrix4;
    function Read2DPoint(index_: Integer): T2DPoint;
    function ReadVec2(index_: Integer): TVec2;
    function ReadRectV2(index_: Integer): TRectV2;
    function ReadPointer(index_: Integer): UInt64;
    function ReadPtr(index_: Integer): UInt64;
    // read from stream data
    procedure Read(index_: Integer; var Buf_; Count_: Int64); overload;
    procedure ReadNM(index_: Integer; output: TNumberModule);
    procedure ReadNMPool(index_: Integer; output: TNumberModulePool);
    procedure ReadOpCode(index_: Integer; output: TOpCode);
    procedure ReadSectionText(index_: Integer; output: THashTextEngine);
    procedure ReadTextSection(index_: Integer; output: THashTextEngine);
    // read as TDFBase
    function Read(index_: Integer): TDFBase; overload;

    function ComputeEncodeSize: Int64;
    class procedure BuildEmptyStream(output: TCore_Stream);
    function FastEncode32To(output: TCore_Stream; SizeInfo32: Cardinal): Integer;
    function FastEncode64To(output: TCore_Stream; SizeInfo64: Int64): Integer;
    function FastEncodeTo(output: TCore_Stream): Integer;
    function EncodeTo(output: TCore_Stream; const FastMode, AutoCompressed: Boolean): Integer; overload;
    function EncodeTo(output: TCore_Stream; const FastMode: Boolean): Integer; overload;
    function EncodeTo(output: TCore_Stream): Integer; overload;
    // data security support
    // QuantumCryptographyPassword: used sha-3-512 cryptography as 512 bits password
    procedure Encrypt(output: TCore_Stream; Compressed_: Boolean; SecurityLevel: Integer; Key: TCipherKeyBuffer);
    function Decrypt(input: TCore_Stream; Key: TCipherKeyBuffer): Boolean;
    // json support
    procedure EncodeAsPublicJson(var output: TPascalString); overload;
    procedure EncodeAsPublicJson(output: TCore_Stream); overload;
    procedure EncodeAsJson(output: TCore_Stream); overload;
    procedure EncodeAsJson(Json: TZ_JsonObject); overload;
    procedure DecodeFromJson(stream: TCore_Stream); overload;
    procedure DecodeFromJson(const s: TPascalString); overload;
    procedure DecodeFromJson(Json: TZ_JsonObject); overload;
    // Parallel compressor
    function EncodeAsSelectCompressor(scm: TSelectCompressionMethod; output: TCore_Stream; const FastMode: Boolean): Integer; overload;
    function EncodeAsSelectCompressor(output: TCore_Stream; const FastMode: Boolean): Integer; overload;
    function EncodeAsSelectCompressor(output: TCore_Stream): Integer; overload;
    // ZLib compressor
    function EncodeAsZLib(output: TCore_Stream; const FastMode, AutoCompressed: Boolean): Integer; overload;
    function EncodeAsZLib(output: TCore_Stream; const FastMode: Boolean): Integer; overload;
    function EncodeAsZLib(output: TCore_Stream): Integer; overload;
    // Deflate compressor
    function EncodeAsDeflate(output: TCore_Stream; const FastMode, AutoCompressed: Boolean): Integer; overload;
    function EncodeAsDeflate(output: TCore_Stream; const FastMode: Boolean): Integer; overload;
    function EncodeAsDeflate(output: TCore_Stream): Integer; overload;
    // BRRC compressor
    function EncodeAsBRRC(output: TCore_Stream; const FastMode, AutoCompressed: Boolean): Integer; overload;
    function EncodeAsBRRC(output: TCore_Stream; const FastMode: Boolean): Integer; overload;
    function EncodeAsBRRC(output: TCore_Stream): Integer; overload;
    // state
    function IsCompressed(source: TCore_Stream): Boolean;
    // decoder
    function DecodeFrom(source: TCore_Stream; const FastMode: Boolean; const Max_Limit_Decode: Integer): Integer; overload;
    function DecodeFrom(source: TCore_Stream; const FastMode: Boolean): Integer; overload;
    function DecodeFrom(source: TCore_Stream): Integer; overload;
    function DecodeFromMemory(memory_: Pointer; mSize: Int64; const FastMode: Boolean): Integer; overload;
    function DecodeFromMemory(memory_: Pointer; mSize: Int64): Integer; overload;
    function DecodeFromMemory(stream: TMS64; const FastMode: Boolean): Integer; overload;
    function DecodeFromMemory(stream: TMem64; const FastMode: Boolean): Integer; overload;
    procedure EncodeToBytes(const Compressed, FastMode: Boolean; var output: TBytes);
    procedure DecodeFromBytes(var buff: TBytes); overload;
    procedure DecodeFromBytes(var buff: TBytes; const FastMode: Boolean); overload;
    // md5
    function GetMD5(const FastMode: Boolean): TMD5;
    function Compare(source: TDFE): Boolean;
    // file
    procedure LoadFromStream(stream: TCore_Stream);
    procedure SaveToStream(stream: TCore_Stream);
    procedure LoadFromFile(fileName_: U_String);
    procedure SaveToFile(fileName_: U_String);
    // list
    property Data[index_: Integer]: TDFBase read GetData; default;
    property List: TDFE_DataList read FDataList;
    // test
    class procedure Test();
  end;

  TDataWriter = class(TCore_Object_Intermediate)
  private
    FEngine: TDFE;
    FStream: TCore_Stream;
  public
    property Engine: TDFE read FEngine;
    constructor Create(Stream_: TCore_Stream);
    destructor Destroy; override;

    procedure Clear;

    procedure WriteString(v: SystemString);
    procedure WriteInteger(v: Integer);
    procedure WriteCardinal(v: Cardinal);
    procedure WriteWORD(v: Word);
    procedure WriteBool(v: Boolean);
    procedure WriteBoolean(v: Boolean);
    procedure WriteByte(v: Byte);
    procedure WriteSingle(v: Single);
    procedure WriteDouble(v: Double);
    procedure WriteArrayInteger(v: array of Integer);
    procedure WriteArrayShortInt(v: array of ShortInt);
    procedure WriteArrayByte(v: array of Byte);
    procedure WriteArraySingle(v: array of Single);
    procedure WriteArrayDouble(v: array of Double);
    procedure WriteArrayInt64(v: array of Int64);
    procedure WriteStream(v: TCore_Stream); overload;
    procedure WriteStream(v: TMS64; bPos_, Size_: Int64); overload;
    procedure WriteVariant(v: Variant);
    procedure WriteInt64(v: Int64);
    procedure WriteUInt64(v: UInt64);
    procedure WriteArrayInt128(v: array of Int128);
    procedure WriteInt128(v: Int128);
    procedure WriteUInt128(v: UInt128);
    procedure WriteStrings(v: TCore_Strings);
    procedure WriteListStrings(v: TListString);
    procedure WritePascalStrings(v: TPascalStringList); overload;
    procedure WritePascalStrings(v: U_StringArray); overload;
    procedure WriteDataFrame(v: TDFE);
    procedure WriteDataFrameCompressed(v: TDFE);
    procedure WriteHashStringList(v: THashStringList);
    procedure WriteVariantList(v: THashVariantList);
    procedure WriteJson(v: TZ_JsonObject); overload;
{$IFDEF DELPHI} procedure WriteJson(v: TJsonObject); overload; {$ENDIF DELPHI}
    procedure WriteRect(v: TRect);
    procedure WriteRectf(v: TRectf);
    procedure WritePoint(v: TPoint);
    procedure WritePointf(v: TPointf);
    procedure WriteVector(v: TVector);
    procedure WriteAffineVector(v: TAffineVector);
    procedure WriteVec4(v: TVec4);
    procedure WriteVec3(v: TVec3);
    procedure WriteVector4(v: TVector4);
    procedure WriteVector3(v: TVector3);
    procedure WriteMat4(v: TMat4);
    procedure WriteMatrix4(v: TMatrix4);
    procedure Write2DPoint(v: T2DPoint);
    procedure WriteVec2(v: TVec2);
    procedure WriteRectV2(v: TRectV2);
    procedure WritePointer(v: Pointer);
    procedure write(const Buf_; Count_: Int64);
    function WriteNM(NM: TNumberModule): TDFE;
    function WriteNMPool(NMPool: TNumberModulePool): TDFE;
    function WriteOpCode(v: TOpCode): TDFE;
    function WriteSectionText(v: THashTextEngine): TDFE;
    function WriteTextSection(v: THashTextEngine): TDFE;
  end;

  TDataReader = class(TCore_Object_Intermediate)
  private
    FEngine: TDFE;
  public
    property Engine: TDFE read FEngine;
    constructor Create(Stream_: TCore_Stream);
    destructor Destroy; override;

    function ReadString: SystemString;
    function ReadInteger: Integer;
    function ReadCardinal: Cardinal;
    function ReadWord: Word;
    function ReadBool: Boolean;
    function ReadBoolean: Boolean;
    function ReadByte: Byte;
    function ReadSingle: Single;
    function ReadDouble: Double;
    procedure ReadArrayInteger(var Data: array of Integer);
    procedure ReadArrayShortInt(var Data: array of ShortInt);
    procedure ReadArrayByte(var Data: array of Byte);
    procedure ReadArraySingle(var Data: array of Single);
    procedure ReadArrayDouble(var Data: array of Double);
    procedure ReadArrayInt64(var Data: array of Int64);
    procedure ReadStream(output: TCore_Stream);
    function ReadVariant: Variant;
    function ReadInt64: Int64;
    function ReadUInt64: UInt64;
    procedure ReadArrayInt128(var Data: array of Int128);
    function ReadInt128: Int128;
    function ReadUInt128: UInt128;
    procedure ReadStrings(output: TCore_Strings);
    procedure ReadListStrings(output: TListString);
    procedure ReadPascalStrings(output: TPascalStringList); overload;
    procedure ReadPascalStrings(var output: U_StringArray); overload;
    procedure ReadDataFrame(output: TDFE);
    procedure ReadHashStringList(output: THashStringList);
    procedure ReadVariantList(output: THashVariantList);
    procedure ReadJson(output: TZ_JsonObject); overload;
{$IFDEF DELPHI} procedure ReadJson(output: TJsonObject); overload; {$ENDIF DELPHI}
    function ReadRect: TRect;
    function ReadRectf: TRectf;
    function ReadPoint: TPoint;
    function ReadPointf: TPointf;
    function ReadVector: TVector;
    function ReadAffineVector: TAffineVector;
    function ReadVec3: TVec3;
    function ReadVec4: TVec4;
    function ReadVector3: TVector3;
    function ReadVector4: TVector4;
    function ReadMat4: TMat4;
    function ReadMatrix4: TMatrix4;
    function Read2DPoint: T2DPoint;
    function ReadVec2: TVec2;
    function ReadRectV2: TRectV2;
    function ReadPointer: UInt64;
    procedure ReadNM(output: TNumberModule);
    procedure ReadNMPool(output: TNumberModulePool);
    procedure ReadOpCode(output: TOpCode);
    procedure ReadSectionText(output: THashTextEngine);
    procedure ReadTextSection(output: THashTextEngine);
    procedure Read(var Buf_; Count_: Int64);
  end;

implementation

uses SysUtils, Variants, Z.Notify;

const
  // data label
  C_Bit_32 = $FF;
  C_Bit_64 = $FA;
  C_None_Compress = 0;
  C_ZLIB_32_Compress = 1;
  C_ZLIB_64_Compress = 11;
  C_Deflate_32_Compress = 2;
  C_Deflate_64_Compress = 22;
  C_BRRC_32_Compress = 3;
  C_BRRC_64_Compress = 33;
  C_Parallel_32_Compress = 4;
  C_Parallel_64_Compress = 44;

constructor TDFBase.Create(ID: Byte);
begin
  inherited Create;
  FID := ID;
end;

destructor TDFBase.Destroy;
begin
  inherited Destroy;
end;

constructor TDFString.Create(ID: Byte);
begin
  inherited Create(ID);
  SetLength(Buffer, 0);
end;

destructor TDFString.Destroy;
begin
  SetLength(Buffer, 0);
  inherited Destroy;
end;

procedure TDFString.LoadFromStream(stream: TCore_Stream);
var
  Size_: Integer;
begin
  stream.Read(Size_, C_Integer_Size);
  SetLength(Buffer, Size_);
  if (Size_ > 0) then
      stream.Read(Buffer[0], Size_);
end;

procedure TDFString.SaveToStream(stream: TCore_Stream);
var
  Size_: Integer;
begin
  Size_ := length(Buffer);
  stream.write(Size_, C_Integer_Size);
  if Size_ > 0 then
      stream.write(Buffer[0], Size_);
end;

procedure TDFString.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  Buffer := umlBytesOf(jarry.s[index_]);
end;

procedure TDFString.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(umlStringOf(Buffer));
end;

function TDFString.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + length(Buffer);
end;

constructor TDFInteger.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFInteger.Destroy;
begin
  inherited Destroy;
end;

procedure TDFInteger.LoadFromStream(stream: TCore_Stream);
begin
  stream.Read(FBuffer, C_Integer_Size);
end;

procedure TDFInteger.SaveToStream(stream: TCore_Stream);
begin
  stream.write(FBuffer, C_Integer_Size);
end;

procedure TDFInteger.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.i[index_];
end;

procedure TDFInteger.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(FBuffer);
end;

function TDFInteger.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size;
end;

constructor TDFCardinal.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFCardinal.Destroy;
begin
  inherited Destroy;
end;

procedure TDFCardinal.LoadFromStream(stream: TCore_Stream);
begin
  stream.Read(FBuffer, C_Cardinal_Size);
end;

procedure TDFCardinal.SaveToStream(stream: TCore_Stream);
begin
  stream.write(FBuffer, C_Cardinal_Size);
end;

procedure TDFCardinal.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.i[index_];
end;

procedure TDFCardinal.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(FBuffer);
end;

function TDFCardinal.ComputeEncodeSize: Int64;
begin
  Result := C_Cardinal_Size;
end;

constructor TDFWord.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFWord.Destroy;
begin
  inherited Destroy;
end;

procedure TDFWord.LoadFromStream(stream: TCore_Stream);
begin
  stream.Read(FBuffer, C_Word_Size);
end;

procedure TDFWord.SaveToStream(stream: TCore_Stream);
begin
  stream.write(FBuffer, C_Word_Size);
end;

procedure TDFWord.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.i[index_];
end;

procedure TDFWord.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(FBuffer);
end;

function TDFWord.ComputeEncodeSize: Int64;
begin
  Result := C_Word_Size;
end;

constructor TDFByte.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFByte.Destroy;
begin
  inherited Destroy;
end;

procedure TDFByte.LoadFromStream(stream: TCore_Stream);
begin
  stream.Read(FBuffer, C_Byte_Size);
end;

procedure TDFByte.SaveToStream(stream: TCore_Stream);
begin
  stream.write(FBuffer, C_Byte_Size);
end;

procedure TDFByte.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.i[index_];
end;

procedure TDFByte.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(FBuffer);
end;

function TDFByte.ComputeEncodeSize: Int64;
begin
  Result := C_Byte_Size;
end;

constructor TDFSingle.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFSingle.Destroy;
begin
  inherited Destroy;
end;

procedure TDFSingle.LoadFromStream(stream: TCore_Stream);
begin
  stream.Read(FBuffer, C_Single_Size);
end;

procedure TDFSingle.SaveToStream(stream: TCore_Stream);
begin
  stream.write(FBuffer, C_Single_Size);
end;

procedure TDFSingle.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.f[index_];
end;

procedure TDFSingle.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.AddF(FBuffer);
end;

function TDFSingle.ComputeEncodeSize: Int64;
begin
  Result := C_Single_Size;
end;

constructor TDFDouble.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFDouble.Destroy;
begin
  inherited Destroy;
end;

procedure TDFDouble.LoadFromStream(stream: TCore_Stream);
begin
  stream.Read(FBuffer, C_Double_Size);
end;

procedure TDFDouble.SaveToStream(stream: TCore_Stream);
begin
  stream.write(FBuffer, C_Double_Size);
end;

procedure TDFDouble.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.f[index_];
end;

procedure TDFDouble.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.AddF(FBuffer);
end;

function TDFDouble.ComputeEncodeSize: Int64;
begin
  Result := C_Double_Size;
end;

constructor TDFArrayInteger.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TMS64.CustomCreate(128);
end;

destructor TDFArrayInteger.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFArrayInteger.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFArrayInteger.Add(v: Integer);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WriteInt32(v);
end;

function TDFArrayInteger.Count: Integer;
begin
  Result := FBuffer.Size div C_Integer_Size;
end;

procedure TDFArrayInteger.WriteArray(const arry_: array of Integer);
begin
  if length(arry_) > 0 then
    begin
      FBuffer.Position := FBuffer.Size;
      FBuffer.WritePtr(@arry_[0], length(arry_) * C_Integer_Size);
    end;
end;

procedure TDFArrayInteger.LoadFromStream(stream: TCore_Stream);
var
  L: Integer;
begin
  Clear;
  stream.Read(L, C_Integer_Size);
  FBuffer.CopyFrom(stream, L * C_Integer_Size);
end;

procedure TDFArrayInteger.SaveToStream(stream: TCore_Stream);
var
  L: Integer;
begin
  L := Count;
  stream.write(L, C_Integer_Size);
  stream.write(FBuffer.Memory^, L * C_Integer_Size);
end;

procedure TDFArrayInteger.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.A[index_];
  for i := 0 to ja.Count - 1 do
      Add(ja.i[i]);
end;

procedure TDFArrayInteger.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;

function TDFArrayInteger.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + C_Integer_Size * Count;
end;

function TDFArrayInteger.GetBuffer(index_: Integer): Integer;
begin
  Result := PInteger(FBuffer.PositionAsPtr(index_ * C_Integer_Size))^;
end;

procedure TDFArrayInteger.SetBuffer(index_: Integer; Value: Integer);
begin
  PInteger(FBuffer.PositionAsPtr(index_ * C_Integer_Size))^ := Value;
end;

constructor TDFArrayShortInt.Create(ID: ShortInt);
begin
  inherited Create(ID);
  FBuffer := TMS64.CustomCreate(128);
end;

destructor TDFArrayShortInt.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFArrayShortInt.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFArrayShortInt.Add(v: ShortInt);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WriteInt8(v);
end;

function TDFArrayShortInt.Count: Integer;
begin
  Result := FBuffer.Size;
end;

procedure TDFArrayShortInt.WriteArray(const arry_: array of ShortInt);
begin
  if length(arry_) > 0 then
    begin
      FBuffer.Position := FBuffer.Size;
      FBuffer.WritePtr(@arry_[0], length(arry_));
    end;
end;

procedure TDFArrayShortInt.LoadFromStream(stream: TCore_Stream);
var
  L: Integer;
begin
  Clear;
  stream.Read(L, C_Integer_Size);
  FBuffer.CopyFrom(stream, L);
end;

procedure TDFArrayShortInt.SaveToStream(stream: TCore_Stream);
var
  L: Integer;
begin
  L := Count;
  stream.write(L, C_Integer_Size);
  stream.write(FBuffer.Memory^, L);
end;

procedure TDFArrayShortInt.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.A[index_];
  for i := 0 to ja.Count - 1 do
      Add(ja.i[i]);
end;

procedure TDFArrayShortInt.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;

function TDFArrayShortInt.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + C_Short_Int_Size * Count;
end;

function TDFArrayShortInt.GetBuffer(index_: Integer): ShortInt;
begin
  Result := PShortInt(FBuffer.PositionAsPtr(index_))^;
end;

procedure TDFArrayShortInt.SetBuffer(index_: Integer; Value: ShortInt);
begin
  PShortInt(FBuffer.PositionAsPtr(index_))^ := Value;
end;

constructor TDFArrayByte.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TMS64.CustomCreate(128);
end;

destructor TDFArrayByte.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFArrayByte.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFArrayByte.Add(v: Byte);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WriteUInt8(v);
end;

procedure TDFArrayByte.AddPtrBuff(p: PByte; Size_: Integer);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WritePtr(p, Size_);
end;

procedure TDFArrayByte.AddI64(v: Int64);
begin
  AddPtrBuff(@v, C_Int64_Size);
end;

procedure TDFArrayByte.AddU64(v: UInt64);
begin
  AddPtrBuff(@v, C_UInt64_Size);
end;

procedure TDFArrayByte.Addi(v: Integer);
begin
  AddPtrBuff(@v, C_Integer_Size);
end;

procedure TDFArrayByte.AddWord(v: Word);
begin
  AddPtrBuff(@v, C_Word_Size);
end;

function TDFArrayByte.Count: Int64;
begin
  Result := FBuffer.Size;
end;

procedure TDFArrayByte.WriteArray(const arry_: array of Byte);
begin
  if length(arry_) > 0 then
      AddPtrBuff(@arry_[0], length(arry_));
end;

procedure TDFArrayByte.SetArray(const arry_: array of Byte);
begin
  Clear;
  if length(arry_) > 0 then
      AddPtrBuff(@arry_[0], length(arry_));
end;

procedure TDFArrayByte.SetBuff(p: PByte; Size_: Integer);
begin
  Clear;
  AddPtrBuff(p, Size_);
end;

procedure TDFArrayByte.GetBuff(p: PByte);
begin
  CopyPtr(FBuffer.Memory, p, FBuffer.Size);
end;

procedure TDFArrayByte.LoadFromStream(stream: TCore_Stream);
var
  L: Integer;
begin
  Clear;
  stream.Read(L, C_Integer_Size);
  FBuffer.CopyFrom(stream, L);
end;

procedure TDFArrayByte.SaveToStream(stream: TCore_Stream);
var
  L: Integer;
begin
  L := Count;
  stream.write(L, C_Integer_Size);
  stream.write(FBuffer.Memory^, L);
end;

procedure TDFArrayByte.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.A[index_];
  for i := 0 to ja.Count - 1 do
      Add(ja.i[i]);
end;

procedure TDFArrayByte.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;

function TDFArrayByte.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + C_Byte_Size * Count;
end;

function TDFArrayByte.GetBuffer(index_: Integer): Byte;
begin
  Result := PByte(FBuffer.PositionAsPtr(index_))^;
end;

procedure TDFArrayByte.SetBuffer(index_: Integer; Value: Byte);
begin
  PByte(FBuffer.PositionAsPtr(index_))^ := Value;
end;

constructor TDFArraySingle.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TMS64.CustomCreate(128);
end;

destructor TDFArraySingle.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFArraySingle.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFArraySingle.Add(v: Single);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WriteSingle(v);
end;

function TDFArraySingle.Count: Integer;
begin
  Result := FBuffer.Size div C_Single_Size;
end;

procedure TDFArraySingle.WriteArray(const arry_: array of Single);
begin
  if length(arry_) > 0 then
    begin
      FBuffer.Position := FBuffer.Size;
      FBuffer.WritePtr(@arry_[0], length(arry_) * C_Single_Size);
    end;
end;

procedure TDFArraySingle.LoadFromStream(stream: TCore_Stream);
var
  L: Integer;
begin
  Clear;
  stream.Read(L, C_Integer_Size);
  FBuffer.CopyFrom(stream, L * C_Single_Size);
end;

procedure TDFArraySingle.SaveToStream(stream: TCore_Stream);
var
  L: Integer;
begin
  L := Count;
  stream.write(L, C_Integer_Size);
  stream.write(FBuffer.Memory^, L * C_Single_Size);
end;

procedure TDFArraySingle.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.A[index_];
  for i := 0 to ja.Count - 1 do
      Add(ja.f[i]);
end;

procedure TDFArraySingle.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.AddF(Buffer[i]);
end;

function TDFArraySingle.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + C_Single_Size * Count;
end;

function TDFArraySingle.GetBuffer(index_: Integer): Single;
begin
  Result := PSingle(FBuffer.PositionAsPtr(index_ * C_Single_Size))^;
end;

procedure TDFArraySingle.SetBuffer(index_: Integer; Value: Single);
begin
  PSingle(FBuffer.PositionAsPtr(index_ * C_Single_Size))^ := Value;
end;

constructor TDFArrayDouble.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TMS64.CustomCreate($FF);
end;

destructor TDFArrayDouble.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFArrayDouble.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFArrayDouble.Add(v: Double);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WriteDouble(v);
end;

function TDFArrayDouble.Count: Integer;
begin
  Result := FBuffer.Size div C_Double_Size;
end;

procedure TDFArrayDouble.WriteArray(const arry_: array of Double);
begin
  if length(arry_) > 0 then
    begin
      FBuffer.Position := FBuffer.Size;
      FBuffer.WritePtr(@arry_[0], length(arry_) * C_Double_Size);
    end;
end;

procedure TDFArrayDouble.LoadFromStream(stream: TCore_Stream);
var
  L: Integer;
begin
  Clear;
  stream.Read(L, C_Integer_Size);
  FBuffer.CopyFrom(stream, L * C_Double_Size);
end;

procedure TDFArrayDouble.SaveToStream(stream: TCore_Stream);
var
  L: Integer;
begin
  L := Count;
  stream.write(L, C_Integer_Size);
  stream.write(FBuffer.Memory^, L * C_Double_Size);
end;

procedure TDFArrayDouble.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.A[index_];
  for i := 0 to ja.Count - 1 do
      Add(ja.f[i]);
end;

procedure TDFArrayDouble.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.AddF(Buffer[i]);
end;

function TDFArrayDouble.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + C_Double_Size * Count;
end;

function TDFArrayDouble.GetBuffer(index_: Integer): Double;
begin
  Result := PDouble(FBuffer.PositionAsPtr(index_ * C_Double_Size))^;
end;

procedure TDFArrayDouble.SetBuffer(index_: Integer; Value: Double);
begin
  PDouble(FBuffer.PositionAsPtr(index_ * C_Double_Size))^ := Value;
end;

constructor TDFArrayInt64.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TMS64.CustomCreate($FF);
end;

destructor TDFArrayInt64.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFArrayInt64.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFArrayInt64.Add(v: Int64);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WriteInt64(v);
end;

function TDFArrayInt64.Count: Integer;
begin
  Result := FBuffer.Size div C_Int64_Size;
end;

procedure TDFArrayInt64.WriteArray(const arry_: array of Int64);
begin
  if length(arry_) > 0 then
    begin
      FBuffer.Position := FBuffer.Size;
      FBuffer.WritePtr(@arry_[0], length(arry_) * C_Int64_Size);
    end;
end;

procedure TDFArrayInt64.LoadFromStream(stream: TCore_Stream);
var
  L: Integer;
begin
  Clear;
  stream.Read(L, C_Integer_Size);
  FBuffer.CopyFrom(stream, L * C_Int64_Size);
end;

procedure TDFArrayInt64.SaveToStream(stream: TCore_Stream);
var
  L: Integer;
begin
  L := Count;
  stream.write(L, C_Integer_Size);
  stream.write(FBuffer.Memory^, L * C_Int64_Size);
end;

procedure TDFArrayInt64.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.A[index_];
  for i := 0 to ja.Count - 1 do
      Add(ja.L[i]);
end;

procedure TDFArrayInt64.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;

function TDFArrayInt64.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + C_Int64_Size * Count;
end;

function TDFArrayInt64.GetBuffer(index_: Integer): Int64;
begin
  Result := PInt64(FBuffer.PositionAsPtr(index_ * C_Int64_Size))^;
end;

procedure TDFArrayInt64.SetBuffer(index_: Integer; Value: Int64);
begin
  PInt64(FBuffer.PositionAsPtr(index_ * C_Int64_Size))^ := Value;
end;

constructor TDFArrayInt128.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TMS64.CustomCreate($FF);
end;

destructor TDFArrayInt128.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFArrayInt128.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFArrayInt128.Add(v: Int128);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WriteInt128(v);
end;

function TDFArrayInt128.Count: Integer;
begin
  Result := FBuffer.Size div C_Int128_Size;
end;

procedure TDFArrayInt128.WriteArray(const arry_: array of Int128);
var
  i: Integer;
begin
  if length(arry_) > 0 then
    begin
      FBuffer.Position := FBuffer.Size;
      for i := 0 to length(arry_) - 1 do
          FBuffer.WriteInt128(arry_[i]);
    end;
end;

procedure TDFArrayInt128.LoadFromStream(stream: TCore_Stream);
var
  L: Integer;
begin
  Clear;
  stream.Read(L, C_Integer_Size);
  FBuffer.CopyFrom(stream, L * C_Int128_Size);
end;

procedure TDFArrayInt128.SaveToStream(stream: TCore_Stream);
var
  L: Integer;
begin
  L := Count;
  stream.write(L, C_Integer_Size);
  stream.write(FBuffer.Memory^, L * C_Int128_Size);
end;

procedure TDFArrayInt128.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.A[index_];
  for i := 0 to ja.Count - 1 do
      Add(ja.I128[i]);
end;

procedure TDFArrayInt128.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;

function TDFArrayInt128.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + C_Int128_Size * Count;
end;

function TDFArrayInt128.GetBuffer(index_: Integer): Int128;
begin
  Result.b := PInt128_Buffer(FBuffer.PositionAsPtr(index_ * C_Int128_Size))^;
end;

procedure TDFArrayInt128.SetBuffer(index_: Integer; Value: Int128);
begin
  PInt128_Buffer(FBuffer.PositionAsPtr(index_ * C_Int128_Size))^ := Value.b;
end;

constructor TDFStream.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TMS64.Create;
end;

destructor TDFStream.Destroy;
begin
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFStream.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFStream.LoadFromStream(stream: TCore_Stream);
var
  Size_: Integer;
begin
  FBuffer.Clear;
  stream.Read(Size_, C_Integer_Size);
  if (Size_ > 0) then
      FBuffer.CopyFrom(stream, Size_);
end;

procedure TDFStream.SaveToStream(stream: TCore_Stream);
var
  Size_: Integer;
begin
  Size_ := FBuffer.Size;
  stream.write(Size_, C_Integer_Size);
  if Size_ > 0 then
    begin
      FBuffer.Position := 0;
      stream.CopyFrom(FBuffer, Size_);
    end;
end;

procedure TDFStream.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  b64: TPascalString;
begin
  FBuffer.Clear;
  b64.Text := jarry.s[index_];
  umlDecodeStreamBASE64(b64, FBuffer);
end;

procedure TDFStream.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  b64: TPascalString;
begin
  umlEncodeStreamBASE64(FBuffer, b64);
  jarry.Add(b64.Text);
end;

function TDFStream.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + FBuffer.Size;
end;

function TDFStream.GetBuffer: TCore_Stream;
begin
  Result := FBuffer;
end;

procedure TDFStream.SetBuffer(Value_: TCore_Stream);
var
  p_: Int64;
begin
  FBuffer.Clear;
  p_ := Value_.Position;
  Value_.Position := 0;
  if Value_.Size > 0 then
    begin
      FBuffer.Size := Value_.Size;
      FBuffer.Position := 0;
      FBuffer.CopyFrom(Value_, Value_.Size);
    end;
  Value_.Position := p_;
end;

constructor TDFVariant.Create(ID: Byte);
begin
  inherited Create(ID);
end;

destructor TDFVariant.Destroy;
begin
  inherited Destroy;
end;

procedure TDFVariant.LoadFromStream(stream: TCore_Stream);
var
  vt: TVarType;
begin
  vt := TVarType(StreamReadUInt16(stream));
  case vt of
    varEmpty, varNull: FBuffer := NULL;
    varSmallInt: FBuffer := StreamReadInt16(stream);
    varInteger: FBuffer := StreamReadInt32(stream);
    varSingle: FBuffer := StreamReadSingle(stream);
    varDouble: FBuffer := StreamReadDouble(stream);
    varCurrency: FBuffer := StreamReadCurrency(stream);
    varBoolean: FBuffer := StreamReadBool(stream);
    varShortInt: FBuffer := StreamReadInt8(stream);
    varByte: FBuffer := StreamReadUInt8(stream);
    varWord: FBuffer := StreamReadUInt16(stream);
    varLongWord: FBuffer := StreamReadUInt32(stream);
    varInt64: FBuffer := StreamReadInt64(stream);
    varUInt64: FBuffer := StreamReadUInt64(stream);
    varOleStr, varString, varUString: FBuffer := StreamReadString(stream).Text;
    else RaiseInfo('error variant type');
  end;
end;

procedure TDFVariant.SaveToStream(stream: TCore_Stream);
var
  vt: TVarType;
begin
  vt := TVarData(FBuffer).VType;
  StreamWriteUInt16(stream, Word(vt));
  case vt of
    varEmpty, varNull:;
    varSmallInt: StreamWriteInt16(stream, FBuffer);
    varInteger: StreamWriteInt32(stream, FBuffer);
    varSingle: StreamWriteSingle(stream, FBuffer);
    varDouble: StreamWriteDouble(stream, FBuffer);
    varCurrency: StreamWriteCurrency(stream, FBuffer);
    varBoolean: StreamWriteBool(stream, FBuffer);
    varShortInt: StreamWriteInt8(stream, FBuffer);
    varByte: StreamWriteUInt8(stream, FBuffer);
    varWord: StreamWriteUInt16(stream, FBuffer);
    varLongWord: StreamWriteUInt32(stream, FBuffer);
    varInt64: StreamWriteInt64(stream, FBuffer);
    varUInt64: StreamWriteUInt64(stream, FBuffer);
    varOleStr, varString, varUString: StreamWriteString(stream, SystemString(FBuffer));
    else
      RaiseInfo('error variant type');
  end;
end;

procedure TDFVariant.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := umlStrToVar(jarry.s[index_]);
end;

procedure TDFVariant.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(umlVarToStr(FBuffer, True).Text);
end;

function TDFVariant.ComputeEncodeSize: Int64;
begin
  case TVarData(FBuffer).VType of
    varEmpty, varNull: Result := 2 + 0;
    varSmallInt: Result := 2 + 2;
    varInteger: Result := 2 + 4;
    varSingle: Result := 2 + 4;
    varDouble: Result := 2 + 8;
    varCurrency: Result := 2 + 8;
    varBoolean: Result := 2 + 1;
    varShortInt: Result := 2 + 1;
    varByte: Result := 2 + 1;
    varWord: Result := 2 + 2;
    varLongWord: Result := 2 + 4;
    varInt64: Result := 2 + 8;
    varUInt64: Result := 2 + 8;
    varOleStr, varString, varUString: Result := 2 + ComputeStreamWriteStringSize(SystemString(FBuffer));
    else
      RaiseInfo('error variant type');
  end;
end;

constructor TDFInt64.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFInt64.Destroy;
begin
  inherited Destroy;
end;

procedure TDFInt64.LoadFromStream(stream: TCore_Stream);
begin
  stream.Read(FBuffer, C_Int64_Size);
end;

procedure TDFInt64.SaveToStream(stream: TCore_Stream);
begin
  stream.write(FBuffer, C_Int64_Size);
end;

procedure TDFInt64.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.L[index_];
end;

procedure TDFInt64.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(FBuffer);
end;

function TDFInt64.ComputeEncodeSize: Int64;
begin
  Result := C_Int64_Size;
end;

constructor TDFUInt64.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFUInt64.Destroy;
begin
  inherited Destroy;
end;

procedure TDFUInt64.LoadFromStream(stream: TCore_Stream);
begin
  stream.Read(FBuffer, C_UInt64_Size);
end;

procedure TDFUInt64.SaveToStream(stream: TCore_Stream);
begin
  stream.write(FBuffer, C_UInt64_Size);
end;

procedure TDFUInt64.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.u[index_];
end;

procedure TDFUInt64.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(FBuffer);
end;

function TDFUInt64.ComputeEncodeSize: Int64;
begin
  Result := C_UInt64_Size;
end;

constructor TDFInt128.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFInt128.Destroy;
begin
  inherited Destroy;
end;

procedure TDFInt128.LoadFromStream(stream: TCore_Stream);
begin
  stream.Read(FBuffer, C_Int128_Size);
end;

procedure TDFInt128.SaveToStream(stream: TCore_Stream);
begin
  stream.write(FBuffer, C_Int128_Size);
end;

procedure TDFInt128.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.I128[index_];
end;

procedure TDFInt128.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(FBuffer);
end;

function TDFInt128.ComputeEncodeSize: Int64;
begin
  Result := C_Int128_Size;
end;

constructor TDFUInt128.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFUInt128.Destroy;
begin
  inherited Destroy;
end;

procedure TDFUInt128.LoadFromStream(stream: TCore_Stream);
begin
  stream.Read(FBuffer, C_UInt128_Size);
end;

procedure TDFUInt128.SaveToStream(stream: TCore_Stream);
begin
  stream.write(FBuffer, C_UInt128_Size);
end;

procedure TDFUInt128.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.U128[index_];
end;

procedure TDFUInt128.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(FBuffer);
end;

function TDFUInt128.ComputeEncodeSize: Int64;
begin
  Result := C_UInt128_Size;
end;

constructor TDFEReader.Create(Owner_: TDFE);
begin
  inherited Create;
  FOwner := Owner_;
  FIndex := 0;
end;

destructor TDFEReader.Destroy;
begin
  inherited Destroy;
end;

function TDFEReader.IsEnd: Boolean;
begin
  Result := FIndex >= FOwner.Count;
end;

function TDFEReader.NotEnd: Boolean;
begin
  Result := FIndex < FOwner.Count;
end;

procedure TDFEReader.Next;
begin
  inc(FIndex);
end;

procedure TDFEReader.GoNext;
begin
  Next;
end;

function TDFEReader.ReadString: SystemString;
begin
  Result := Owner.ReadString(Index);
  Next;
end;

function TDFEReader.ReadInteger: Integer;
begin
  Result := Owner.ReadInteger(Index);
  Next;
end;

function TDFEReader.ReadCardinal: Cardinal;
begin
  Result := Owner.ReadCardinal(Index);
  Next;
end;

function TDFEReader.ReadWord: Word;
begin
  Result := Owner.ReadWord(Index);
  Next;
end;

function TDFEReader.ReadBool: Boolean;
begin
  Result := Owner.ReadBool(Index);
  Next;
end;

function TDFEReader.ReadBoolean: Boolean;
begin
  Result := ReadBool;
end;

function TDFEReader.ReadByte: Byte;
begin
  Result := Owner.ReadByte(Index);
  Next;
end;

function TDFEReader.ReadSingle: Single;
begin
  Result := Owner.ReadSingle(Index);
  Next;
end;

function TDFEReader.ReadDouble: Double;
begin
  Result := Owner.ReadDouble(Index);
  Next;
end;

function TDFEReader.ReadArrayInteger: TDFArrayInteger;
begin
  Result := Owner.ReadArrayInteger(Index);
  Next;
end;

function TDFEReader.ReadArrayShortInt: TDFArrayShortInt;
begin
  Result := Owner.ReadArrayShortInt(Index);
  Next;
end;

function TDFEReader.ReadArrayByte: TDFArrayByte;
begin
  Result := Owner.ReadArrayByte(Index);
  Next;
end;

function TDFEReader.ReadMD5: TMD5;
begin
  Result := Owner.ReadMD5(Index);
  Next;
end;

function TDFEReader.ReadArraySingle: TDFArraySingle;
begin
  Result := Owner.ReadArraySingle(Index);
  Next;
end;

function TDFEReader.ReadArrayDouble: TDFArrayDouble;
begin
  Result := Owner.ReadArrayDouble(Index);
  Next;
end;

function TDFEReader.ReadArrayInt64: TDFArrayInt64;
begin
  Result := Owner.ReadArrayInt64(Index);
  Next;
end;

procedure TDFEReader.ReadMem64(output: TMem64);
begin
  Owner.ReadMem64(Index, output);
  Next;
end;

procedure TDFEReader.ReadStream(output: TCore_Stream);
begin
  Owner.ReadStream(Index, output);
  Next;
end;

function TDFEReader.ReadVariant: Variant;
begin
  Result := Owner.ReadVariant(Index);
  Next;
end;

function TDFEReader.ReadInt64: Int64;
begin
  Result := Owner.ReadInt64(Index);
  Next;
end;

function TDFEReader.ReadUInt64: UInt64;
begin
  Result := Owner.ReadUInt64(Index);
  Next;
end;

function TDFEReader.ReadArrayInt128: TDFArrayInt128;
begin
  Result := Owner.ReadArrayInt128(Index);
  Next;
end;

function TDFEReader.ReadInt128: Int128;
begin
  Result := Owner.ReadInt128(Index);
  Next;
end;

function TDFEReader.ReadUInt128: UInt128;
begin
  Result := Owner.ReadUInt128(Index);
  Next;
end;

procedure TDFEReader.ReadStrings(output: TCore_Strings);
begin
  Owner.ReadStrings(Index, output);
  Next;
end;

procedure TDFEReader.ReadListStrings(output: TListString);
begin
  Owner.ReadListStrings(Index, output);
  Next;
end;

procedure TDFEReader.ReadPascalStrings(output: TPascalStringList);
begin
  Owner.ReadPascalStrings(Index, output);
  Next;
end;

procedure TDFEReader.ReadPascalStrings(var output: U_StringArray);
begin
  Owner.ReadPascalStrings(Index, output);
  Next;
end;

procedure TDFEReader.ReadDataFrame(output: TDFE);
begin
  Owner.ReadDataFrame(Index, output);
  Next;
end;

procedure TDFEReader.ReadDF(output: TDFE);
begin
  Owner.ReadDF(Index, output);
  Next;
end;

procedure TDFEReader.ReadDFE(output: TDFE);
begin
  Owner.ReadDFE(Index, output);
  Next;
end;

procedure TDFEReader.ReadHashStringList(output: THashStringList);
begin
  Owner.ReadHashStringList(Index, output);
  Next;
end;

procedure TDFEReader.ReadVariantList(output: THashVariantList);
begin
  Owner.ReadVariantList(Index, output);
  Next;
end;

procedure TDFEReader.ReadJson(output: TZ_JsonObject);
begin
  Owner.ReadJson(Index, output);
  Next;
end;

{$IFDEF DELPHI}


procedure TDFEReader.ReadJson(output: TJsonObject);
begin
  Owner.ReadJson(Index, output);
  Next;
end;
{$ENDIF DELPHI}


function TDFEReader.ReadRect: TRect;
begin
  Result := Owner.ReadRect(Index);
  Next;
end;

function TDFEReader.ReadRectf: TRectf;
begin
  Result := Owner.ReadRectf(Index);
  Next;
end;

function TDFEReader.ReadPoint: TPoint;
begin
  Result := Owner.ReadPoint(Index);
  Next;
end;

function TDFEReader.ReadPointf: TPointf;
begin
  Result := Owner.ReadPointf(Index);
  Next;
end;

function TDFEReader.ReadVector: TVector;
begin
  Result := Owner.ReadVector(Index);
  Next;
end;

function TDFEReader.ReadAffineVector: TAffineVector;
begin
  Result := Owner.ReadAffineVector(Index);
  Next;
end;

function TDFEReader.ReadVec3: TVec3;
begin
  Result := Owner.ReadVec3(Index);
  Next;
end;

function TDFEReader.ReadVec4: TVec4;
begin
  Result := Owner.ReadVec4(Index);
  Next;
end;

function TDFEReader.ReadVector3: TVector3;
begin
  Result := Owner.ReadVector3(Index);
  Next;
end;

function TDFEReader.ReadVector4: TVector4;
begin
  Result := Owner.ReadVector4(Index);
  Next;
end;

function TDFEReader.ReadMat4: TMat4;
begin
  Result := Owner.ReadMat4(Index);
  Next;
end;

function TDFEReader.ReadMatrix4: TMatrix4;
begin
  Result := Owner.ReadMatrix4(Index);
  Next;
end;

function TDFEReader.Read2DPoint: T2DPoint;
begin
  Result := Owner.Read2DPoint(Index);
  Next;
end;

function TDFEReader.ReadVec2: TVec2;
begin
  Result := Owner.ReadVec2(Index);
  Next;
end;

function TDFEReader.ReadRectV2: TRectV2;
begin
  Result := Owner.ReadRectV2(Index);
  Next;
end;

function TDFEReader.ReadPointer: UInt64;
begin
  Result := Owner.ReadPointer(Index);
  Next;
end;

function TDFEReader.ReadPtr: UInt64;
begin
  Result := Owner.ReadPointer(Index);
  Next;
end;

procedure TDFEReader.Read(var Buf_; Count_: Int64);
begin
  Owner.Read(Index, Buf_, Count_);
  Next;
end;

procedure TDFEReader.ReadNM(output: TNumberModule);
begin
  Owner.ReadNM(Index, output);
  Next;
end;

procedure TDFEReader.ReadNMPool(output: TNumberModulePool);
begin
  Owner.ReadNMPool(Index, output);
  Next;
end;

procedure TDFEReader.ReadOpCode(output: TOpCode);
begin
  Owner.ReadOpCode(Index, output);
  Next;
end;

procedure TDFEReader.ReadSectionText(output: THashTextEngine);
begin
  Owner.ReadSectionText(Index, output);
  Next;
end;

procedure TDFEReader.ReadTextSection(output: THashTextEngine);
begin
  Owner.ReadTextSection(Index, output);
  Next;
end;

function TDFEReader.Read: TDFBase;
begin
  Result := Owner.Read(Index);
  Next;
end;

function TDFEReader.Current: TDFBase;
begin
  Result := Owner.Read(Index);
end;

function TDFE_DataList.Add_DFBase(Data_: TDFBase): TDFBase;
begin
  inherited Add(Data_);
  Result := Data_;
  Owner.FIsChanged := True;
end;

procedure TDFE_DataList.Clear;
begin
  inherited Clear;
  Owner.FIsChanged := True;
end;

function TDFE.DataTypeToByte(v: TRunTimeDataType): Byte;
begin
  Result := Byte(v);
end;

function TDFE.ByteToDataType(v: Byte): TRunTimeDataType;
begin
  Result := TRunTimeDataType(v);
end;

constructor TDFE.Create;
begin
  inherited Create;
  FBit_64_Condition := C_Max_UInt32;
  FDataList := TDFE_DataList.Create;
  FDataList.Owner := Self;
  FReader := TDFEReader.Create(Self);
  FCompressorDeflate := nil;
  FCompressorBRRC := nil;
  FIsChanged := False;
end;

destructor TDFE.Destroy;
begin
  Clear;
  DisposeObject(FDataList);
  DisposeObject(FReader);
  if FCompressorDeflate <> nil then
      DisposeObject(FCompressorDeflate);
  if FCompressorBRRC <> nil then
      DisposeObject(FCompressorBRRC);
  inherited Destroy;
end;

function TDFE.DelayFree: TDFE;
begin
  DelayFreeObj(5.0, Self);
  Result := Self;
end;

procedure TDFE.SwapInstance(source: TDFE);
var
  tmp_DataList: TDFE_DataList;
  tmp_Reader: TDFEReader;
begin
  if Self = source then
      exit;
  tmp_DataList := FDataList;
  tmp_Reader := FReader;

  FDataList := source.FDataList;
  FReader := source.FReader;

  source.FDataList := tmp_DataList;
  source.FReader := tmp_Reader;

  FDataList.Owner := Self;
  source.FDataList.Owner := source;

  FReader.FOwner := Self;
  source.FReader.FOwner := source;

  TSwap<Integer>.Do_(Reader.FIndex, source.Reader.FIndex);
  TSwap<Int64>.Do_(FBit_64_Condition, source.FBit_64_Condition);

  FIsChanged := True;
  source.FIsChanged := True;
end;

procedure TDFE.Clear;
var
  i: Integer;
begin
  for i := 0 to FDataList.Count - 1 do
      DisposeObject(FDataList[i]);

  try
      FDataList.Clear;
  except
  end;

  FIsChanged := True;
  FReader.index := 0;
end;

function TDFE.AddData(v: TRunTimeDataType): TDFBase;
begin
  case v of
    rdtString: Result := TDFString.Create(DataTypeToByte(v));
    rdtInteger: Result := TDFInteger.Create(DataTypeToByte(v));
    rdtCardinal: Result := TDFCardinal.Create(DataTypeToByte(v));
    rdtWORD: Result := TDFWord.Create(DataTypeToByte(v));
    rdtByte: Result := TDFByte.Create(DataTypeToByte(v));
    rdtSingle: Result := TDFSingle.Create(DataTypeToByte(v));
    rdtDouble: Result := TDFDouble.Create(DataTypeToByte(v));
    rdtArrayInteger: Result := TDFArrayInteger.Create(DataTypeToByte(v));
    rdtArrayShortInt: Result := TDFArrayShortInt.Create(DataTypeToByte(v));
    rdtArrayByte: Result := TDFArrayByte.Create(DataTypeToByte(v));
    rdtArraySingle: Result := TDFArraySingle.Create(DataTypeToByte(v));
    rdtArrayDouble: Result := TDFArrayDouble.Create(DataTypeToByte(v));
    rdtArrayInt64: Result := TDFArrayInt64.Create(DataTypeToByte(v));
    rdtStream: Result := TDFStream.Create(DataTypeToByte(v));
    rdtVariant: Result := TDFVariant.Create(DataTypeToByte(v));
    rdtInt64: Result := TDFInt64.Create(DataTypeToByte(v));
    rdtUInt64: Result := TDFUInt64.Create(DataTypeToByte(v));
    rdtArrayInt128: Result := TDFArrayInt128.Create(DataTypeToByte(v));
    rdtInt128: Result := TDFInt128.Create(DataTypeToByte(v));
    rdtUInt128: Result := TDFUInt128.Create(DataTypeToByte(v));
    else
      Result := nil;
  end;
  if Result <> nil then
      FDataList.Add_DFBase(Result);
  FIsChanged := True;
end;

function TDFE.GetData(index_: Integer): TDFBase;
begin
  if (index_ >= 0) and (index_ < FDataList.Count) then
      Result := TDFBase(FDataList[index_])
  else
      Result := nil;
end;

function TDFE.GetDataInfo(Obj_: TDFBase): SystemString;
begin
  case ByteToDataType(Obj_.FID) of
    rdtString: Result := 'SystemString';
    rdtInteger: Result := 'Integer';
    rdtCardinal: Result := 'Cardinal';
    rdtWORD: Result := 'WORD';
    rdtByte: Result := 'Byte';
    rdtSingle: Result := 'Single';
    rdtDouble: Result := 'Double';
    rdtArrayInteger: Result := 'ArrayInteger';
    rdtArrayShortInt: Result := 'ShortInt';
    rdtArrayByte: Result := 'Byte';
    rdtArraySingle: Result := 'ArraySingle';
    rdtArrayDouble: Result := 'ArrayDouble';
    rdtArrayInt64: Result := 'ArrayInt64';
    rdtStream: Result := 'Stream';
    rdtVariant: Result := 'Variant';
    rdtInt64: Result := 'Int64';
    rdtUInt64: Result := 'UInt64';
    else
      Result := '';
  end;
end;

function TDFE.Count: Integer;
begin
  Result := FDataList.Count;
end;

function TDFE.Delete(index_: Integer): Boolean;
begin
  try
    DisposeObject(FDataList[index_]);
    FDataList.Delete(index_);
    FIsChanged := True;
    Result := True;
  except
      Result := False;
  end;
end;

function TDFE.DeleteFirst: Boolean;
begin
  Result := Delete(0);
end;

function TDFE.DeleteLast: Boolean;
begin
  Result := Delete(Count - 1);
end;

function TDFE.DeleteLastCount(num_: Integer): Boolean;
begin
  Result := True;
  while num_ > 0 do
    begin
      Result := Result and DeleteLast;
      dec(num_);
    end;
end;

function TDFE.DeleteCount(index_, Count_: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count_ - 1 do
      Result := Result and Delete(index_);
end;

function TDFE.Append(source: TDFE): TDFE;
var
  m64: TMS64;
  i: Integer;
  DataFrame_: TDFBase;
begin
  Result := Self;
  if Self = source then
      exit;
  m64 := TMS64.CustomCreate(64 * 1024);
  for i := 0 to source.Count - 1 do
    begin
      DataFrame_ := AddData(ByteToDataType(source[i].FID));
      source[i].SaveToStream(m64);
      m64.Position := 0;
      DataFrame_.LoadFromStream(m64);
      m64.Position := 0;
    end;
  DisposeObject(m64);
end;

function TDFE.Assign(source: TDFE): TDFE;
var
  m64: TMS64;
  i: Integer;
  DataFrame_: TDFBase;
begin
  Result := Self;
  if Self = source then
      exit;
  Clear;
  m64 := TMS64.CustomCreate(64 * 1024);
  for i := 0 to source.Count - 1 do
    begin
      DataFrame_ := AddData(ByteToDataType(source[i].FID));
      source[i].SaveToStream(m64);
      m64.Position := 0;
      DataFrame_.LoadFromStream(m64);
      m64.Position := 0;
    end;
  DisposeObject(m64);
  FBit_64_Condition := source.FBit_64_Condition;
end;

function TDFE.Clone: TDFE;
begin
  Result := TDFE.Create;
  Result.Assign(Self);
end;

function TDFE.WriteString(v: SystemString): TDFE;
var
  Obj_: TDFString;
begin
  Obj_ := TDFString.Create(DataTypeToByte(rdtString));
  Obj_.Buffer := umlBytesOf(v);
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteString(v: TPascalString): TDFE;
var
  Obj_: TDFString;
begin
  Obj_ := TDFString.Create(DataTypeToByte(rdtString));
  Obj_.Buffer := v.Bytes;
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteString(const Fmt: SystemString; const Args: array of const): TDFE;
begin
  Result := WriteString(PFormat(Fmt, Args));
end;

function TDFE.WriteInteger(v: Integer): TDFE;
var
  Obj_: TDFInteger;
begin
  Obj_ := TDFInteger.Create(DataTypeToByte(rdtInteger));
  Obj_.Buffer := v;
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteCardinal(v: Cardinal): TDFE;
var
  Obj_: TDFCardinal;
begin
  Obj_ := TDFCardinal.Create(DataTypeToByte(rdtCardinal));
  Obj_.Buffer := v;
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteWORD(v: Word): TDFE;
var
  Obj_: TDFWord;
begin
  Obj_ := TDFWord.Create(DataTypeToByte(rdtWORD));
  Obj_.Buffer := v;
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteBool(v: Boolean): TDFE;
begin
  if v then
      WriteByte(1)
  else
      WriteByte(0);
  Result := Self;
end;

function TDFE.WriteBoolean(v: Boolean): TDFE;
begin
  Result := WriteBool(v);
end;

function TDFE.WriteByte(v: Byte): TDFE;
var
  Obj_: TDFByte;
begin
  Obj_ := TDFByte.Create(DataTypeToByte(rdtByte));
  Obj_.Buffer := v;
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteSingle(v: Single): TDFE;
var
  Obj_: TDFSingle;
begin
  Obj_ := TDFSingle.Create(DataTypeToByte(rdtSingle));
  Obj_.Buffer := v;
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteDouble(v: Double): TDFE;
var
  Obj_: TDFDouble;
begin
  Obj_ := TDFDouble.Create(DataTypeToByte(rdtDouble));
  Obj_.Buffer := v;
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteArrayInteger: TDFArrayInteger;
begin
  Result := TDFArrayInteger.Create(DataTypeToByte(rdtArrayInteger));
  FDataList.Add_DFBase(Result);
end;

function TDFE.WriteArrayShortInt: TDFArrayShortInt;
begin
  Result := TDFArrayShortInt.Create(DataTypeToByte(rdtArrayShortInt));
  FDataList.Add_DFBase(Result);
end;

function TDFE.WriteArrayByte: TDFArrayByte;
begin
  Result := TDFArrayByte.Create(DataTypeToByte(rdtArrayByte));
  FDataList.Add_DFBase(Result);
end;

function TDFE.WriteMD5(md5: TMD5): TDFE;
begin
  WriteArrayByte.SetBuff(@md5[0], SizeOf(TMD5));
  Result := Self;
end;

function TDFE.WriteArraySingle: TDFArraySingle;
begin
  Result := TDFArraySingle.Create(DataTypeToByte(rdtArraySingle));
  FDataList.Add_DFBase(Result);
end;

function TDFE.WriteArrayDouble: TDFArrayDouble;
begin
  Result := TDFArrayDouble.Create(DataTypeToByte(rdtArrayDouble));
  FDataList.Add_DFBase(Result);
end;

function TDFE.WriteArrayInt64: TDFArrayInt64;
begin
  Result := TDFArrayInt64.Create(DataTypeToByte(rdtArrayInt64));
  FDataList.Add_DFBase(Result);
end;

function TDFE.WriteMem64(v: TMem64): TDFE;
var
  Obj_: TDFStream;
begin
  Obj_ := TDFStream.Create(DataTypeToByte(rdtStream));
  v.Position := 0;
  Obj_.Buffer64.CopyMem64(v, v.Size);
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteStream(v: TCore_Stream): TDFE;
var
  Obj_: TDFStream;
begin
  Obj_ := TDFStream.Create(DataTypeToByte(rdtStream));
  Obj_.Buffer := v;
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteStream(v: TMS64; bPos_, Size_: Int64): TDFE;
var
  Obj_: TDFStream;
begin
  Obj_ := TDFStream.Create(DataTypeToByte(rdtStream));
  Obj_.Buffer64.Clear;
  Obj_.Buffer64.WritePtr(v.PosAsPtr(bPos_), Size_);
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteVariant(v: Variant): TDFE;
var
  Obj_: TDFVariant;
begin
  Obj_ := TDFVariant.Create(DataTypeToByte(rdtVariant));
  Obj_.Buffer := v;
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteInt64(v: Int64): TDFE;
var
  Obj_: TDFInt64;
begin
  Obj_ := TDFInt64.Create(DataTypeToByte(rdtInt64));
  Obj_.Buffer := v;
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteUInt64(v: UInt64): TDFE;
var
  Obj_: TDFUInt64;
begin
  Obj_ := TDFUInt64.Create(DataTypeToByte(rdtUInt64));
  Obj_.Buffer := v;
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteArrayInt128: TDFArrayInt128;
begin
  Result := TDFArrayInt128.Create(DataTypeToByte(rdtArrayInt128));
  FDataList.Add_DFBase(Result);
end;

function TDFE.WriteInt128(v: Int128): TDFE;
var
  Obj_: TDFInt128;
begin
  Obj_ := TDFInt128.Create(DataTypeToByte(rdtInt128));
  Obj_.Buffer := v;
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteUInt128(v: UInt128): TDFE;
var
  Obj_: TDFUInt128;
begin
  Obj_ := TDFUInt128.Create(DataTypeToByte(rdtUInt128));
  Obj_.Buffer := v;
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteStrings(v: TCore_Strings): TDFE;
var
  m64: TMS64;
begin
  m64 := TMS64.CustomCreate(umlMax(8192, v.Count * 10));
{$IFDEF FPC}
  v.SaveToStream(m64);
{$ELSE}
  v.SaveToStream(m64, TEncoding.UTF8);
{$ENDIF}
  m64.Position := 0;
  Result := WriteStream(m64);
  DisposeObject(m64);
end;

function TDFE.WriteListStrings(v: TListString): TDFE;
var
  m64: TMS64;
begin
  m64 := TMS64.CustomCreate(umlMax(8192, v.Count * 10));
  v.SaveToStream(m64);
  m64.Position := 0;
  Result := WriteStream(m64);
  DisposeObject(m64);
end;

function TDFE.WritePascalStrings(v: TPascalStringList): TDFE;
var
  m64: TMS64;
begin
  m64 := TMS64.CustomCreate(umlMax(8192, v.Count * 10));
  v.SaveToStream(m64);
  m64.Position := 0;
  Result := WriteStream(m64);
  DisposeObject(m64);
end;

function TDFE.WritePascalStrings(v: U_StringArray): TDFE;
var
  L: TPascalStringList;
  i: Integer;
begin
  L := TPascalStringList.Create;
  for i := low(v) to high(v) do
      L.Add(v[i]);
  WritePascalStrings(L);
  DisposeObject(L);
end;

function TDFE.WriteDataFrame(v: TDFE): TDFE;
var
  Obj_: TDFStream;
begin
  Obj_ := TDFStream.Create(DataTypeToByte(rdtStream));
  v.FastEncodeTo(Obj_.Buffer);
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteDataFrameCompressed(v: TDFE): TDFE;
var
  Obj_: TDFStream;
begin
  Obj_ := TDFStream.Create(DataTypeToByte(rdtStream));
  v.EncodeAsSelectCompressor(Obj_.Buffer, True);
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteDataFrameZLib(v: TDFE): TDFE;
var
  Obj_: TDFStream;
begin
  Obj_ := TDFStream.Create(DataTypeToByte(rdtStream));
  v.EncodeAsZLib(Obj_.Buffer, True);
  FDataList.Add_DFBase(Obj_);
  Result := Self;
end;

function TDFE.WriteDF(v: TDFE): TDFE;
begin
  Result := WriteDataFrame(v);
end;

function TDFE.WriteDFE(v: TDFE): TDFE;
begin
  Result := WriteDataFrame(v);
end;

function TDFE.WriteHashStringList(v: THashStringList): TDFE;
var
  m64: TMS64;
  hash_: THashStringTextStream;
begin
  m64 := TMS64.Create;
  hash_ := THashStringTextStream.Create(v);
  hash_.SaveToStream(m64);
  DisposeObject(hash_);
  m64.Position := 0;
  Result := WriteStream(m64);
  DisposeObject(m64);
end;

function TDFE.WriteVariantList(v: THashVariantList): TDFE;
var
  m64: TMS64;
  hash_: THashVariantTextStream;
begin
  m64 := TMS64.Create;
  hash_ := THashVariantTextStream.Create(v);
  hash_.SaveToStream(m64);
  DisposeObject(hash_);
  m64.Position := 0;
  Result := WriteStream(m64);
  DisposeObject(m64);
end;

function TDFE.WriteJson(v: TZ_JsonObject): TDFE;
begin
  Result := WriteJson(v, False);
end;

function TDFE.WriteJson(v: TZ_JsonObject; Formated_: Boolean): TDFE;
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  v.SaveToStream(m64, Formated_);
  m64.Position := 0;
  Result := WriteStream(m64);
  DisposeObject(m64);
end;

{$IFDEF DELPHI}


function TDFE.WriteJson(v: TJsonObject): TDFE;
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  v.SaveToStream(m64, True, TEncoding.UTF8, True);
  m64.Position := 0;
  Result := WriteStream(m64);
  DisposeObject(m64);
end;
{$ENDIF DELPHI}


function TDFE.WriteFile(fn: SystemString): TDFE;
var
  fs: TCore_FileStream;
begin
  if umlFileExists(fn) then
    begin
      fs := TCore_FileStream.Create(fn, fmOpenRead or fmShareDenyNone);
      fs.Position := 0;
      Result := WriteStream(fs);
      DisposeObject(fs);
    end
  else
      Result := Self;
end;

function TDFE.WriteRect(v: TRect): TDFE;
begin
  with WriteArrayInteger do
    begin
      Add(v.Left);
      Add(v.Top);
      Add(v.Right);
      Add(v.Bottom);
    end;
  Result := Self;
end;

function TDFE.WriteRectf(v: TRectf): TDFE;
begin
  with WriteArraySingle do
    begin
      Add(v.Left);
      Add(v.Top);
      Add(v.Right);
      Add(v.Bottom);
    end;
  Result := Self;
end;

function TDFE.WritePoint(v: TPoint): TDFE;
begin
  with WriteArrayInteger do
    begin
      Add(v.x);
      Add(v.y);
    end;
  Result := Self;
end;

function TDFE.WritePointf(v: TPointf): TDFE;
begin
  with WriteArraySingle do
    begin
      Add(v.x);
      Add(v.y);
    end;
  Result := Self;
end;

function TDFE.WriteVector(v: TVector): TDFE;
begin
  WriteArraySingle.WriteArray(v);
  Result := Self;
end;

function TDFE.WriteAffineVector(v: TAffineVector): TDFE;
begin
  WriteArraySingle.WriteArray(v);
  Result := Self;
end;

function TDFE.WriteVec4(v: TVec4): TDFE;
begin
  WriteArraySingle.WriteArray(v);
  Result := Self;
end;

function TDFE.WriteVec3(v: TVec3): TDFE;
begin
  WriteArraySingle.WriteArray(v);
  Result := Self;
end;

function TDFE.WriteVector4(v: TVector4): TDFE;
begin
  WriteArraySingle.WriteArray(v.buff);
  Result := Self;
end;

function TDFE.WriteVector3(v: TVector3): TDFE;
begin
  WriteArraySingle.WriteArray(v.buff);
  Result := Self;
end;

function TDFE.WriteMat4(v: TMat4): TDFE;
begin
  with WriteArraySingle do
    begin
      WriteArray(v[0]);
      WriteArray(v[1]);
      WriteArray(v[2]);
      WriteArray(v[3]);
    end;
  Result := Self;
end;

function TDFE.WriteMatrix4(v: TMatrix4): TDFE;
begin
  Result := WriteMat4(v.buff);
end;

function TDFE.Write2DPoint(v: T2DPoint): TDFE;
begin
  with WriteArraySingle do
      WriteArray(v);
  Result := Self;
end;

function TDFE.WriteVec2(v: TVec2): TDFE;
begin
  Result := Write2DPoint(v);
end;

function TDFE.WriteRectV2(v: TRectV2): TDFE;
begin
  with WriteArraySingle do
    begin
      WriteArray(v[0]);
      WriteArray(v[1]);
    end;
  Result := Self;
end;

function TDFE.WritePointer(v: Pointer): TDFE;
begin
  Result := WriteUInt64(UInt64(v));
end;

function TDFE.WritePointer(v: UInt64): TDFE;
begin
  Result := WriteUInt64(v);
end;

function TDFE.WritePtr(v: Pointer): TDFE;
begin
  Result := WriteUInt64(UInt64(v));
end;

function TDFE.WritePtr(v: UInt64): TDFE;
begin
  Result := WriteUInt64(v);
end;

// append new stream and write
function TDFE.write(const Buf_; Count_: Int64): TDFE;
var
  s: TMS64;
begin
  s := TMS64.Create;
  s.Write64(Buf_, Count_);
  Result := WriteStream(s);
  DisposeObject(s);
end;

function TDFE.WriteNM(NM: TNumberModule): TDFE;
var
  D_: TDFE;
begin
  D_ := TDFE.Create;
  D_.WriteString(NM.Name);
  D_.WriteVariant(NM.Origin);
  D_.WriteVariant(NM.Value);
  Result := WriteDataFrame(D_);
  DisposeObject(D_);
end;

function TDFE.WriteNMPool(NMPool: TNumberModulePool): TDFE;
var
  D_: TDFE;
{$IFDEF FPC}
  procedure fpc_progress_(const Name: PSystemString; NM: TNumberModule);
  var
    Tmp_: TDFE;
  begin
    Tmp_ := TDFE.Create;
    Tmp_.WriteString(NM.Name);
    Tmp_.WriteVariant(NM.Origin);
    Tmp_.WriteVariant(NM.Value);
    D_.WriteDataFrame(Tmp_);
    DisposeObject(Tmp_);
  end;
{$ENDIF FPC}


begin
  D_ := TDFE.Create;

{$IFDEF FPC}
  NMPool.List.ProgressP(fpc_progress_);
{$ELSE FPC}
  NMPool.List.ProgressP(procedure(const Name: PSystemString; NM: TNumberModule)
    var
      Tmp_: TDFE;
    begin
      Tmp_ := TDFE.Create;
      Tmp_.WriteString(NM.Name);
      Tmp_.WriteVariant(NM.Origin);
      Tmp_.WriteVariant(NM.Value);
      D_.WriteDataFrame(Tmp_);
      DisposeObject(Tmp_);
    end);
{$ENDIF FPC}
  Result := WriteDataFrame(D_);
  DisposeObject(D_);
end;

function TDFE.WriteOpCode(v: TOpCode): TDFE;
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  v.SaveToStream(m64);
  m64.Position := 0;
  Result := WriteStream(m64);
  DisposeObject(m64);
end;

function TDFE.WriteSectionText(v: THashTextEngine): TDFE;
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  v.SaveToStream(m64);
  m64.Position := 0;
  Result := WriteStream(m64);
  DisposeObject(m64);
end;

function TDFE.WriteTextSection(v: THashTextEngine): TDFE;
begin
  Result := WriteSectionText(v);
end;

function TDFE.ReadString(index_: Integer): SystemString;
var
  Obj_: TDFBase;
  i: Integer;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFString then
      Result := umlStringOf(TDFString(Obj_).Buffer).Text
  else if Obj_ is TDFInteger then
      Result := IntToStr(TDFInteger(Obj_).Buffer)
  else if Obj_ is TDFCardinal then
      Result := IntToStr(TDFCardinal(Obj_).Buffer)
  else if Obj_ is TDFWord then
      Result := IntToStr(TDFWord(Obj_).Buffer)
  else if Obj_ is TDFByte then
      Result := IntToStr(TDFByte(Obj_).Buffer)
  else if Obj_ is TDFSingle then
      Result := FloatToStr(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := FloatToStr(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFArrayInteger then
    begin
      Result := '(';
      with TDFArrayInteger(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDFArrayShortInt then
    begin
      Result := '(';
      with TDFArrayShortInt(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDFArrayByte then
    begin
      Result := '(';
      with TDFArrayByte(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDFArraySingle then
    begin
      Result := '(';
      with TDFArraySingle(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + FloatToStr(Buffer[i])
            else
                Result := Result + FloatToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDFArrayDouble then
    begin
      Result := '(';
      with TDFArrayDouble(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + FloatToStr(Buffer[i])
            else
                Result := Result + FloatToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDFArrayInt64 then
    begin
      Result := '(';
      with TDFArrayInt64(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDFVariant then
      Result := umlVarToStr(TDFVariant(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := IntToStr(TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFUInt64 then
{$IFDEF FPC}
    Result := IntToStr(TDFUInt64(Obj_).Buffer)
{$ELSE}
    Result := UIntToStr(TDFUInt64(Obj_).Buffer)
{$ENDIF}
  else if Obj_ is TDFInt128 then
      Result := TDFInt128(Obj_).Buffer.ToString.Text
  else if Obj_ is TDFUInt128 then
      Result := TDFUInt128(Obj_).Buffer.ToString.Text
  else
      Result := '';
end;

function TDFE.ReadInteger(index_: Integer): Integer;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := umlStrToInt(umlStringOf(TDFString(Obj_).Buffer), 0)
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := Trunc(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := Trunc(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFVariant then
      Result := (TDFVariant(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := (TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFUInt64 then
      Result := (TDFUInt64(Obj_).Buffer)
  else if Obj_ is TDFInt128 then
      Result := TDFInt128(Obj_).Buffer.ToInt32
  else if Obj_ is TDFUInt128 then
      Result := TDFUInt128(Obj_).Buffer.ToInt32
  else
      Result := 0;
end;

function TDFE.ReadCardinal(index_: Integer): Cardinal;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := umlStrToInt(umlStringOf(TDFString(Obj_).Buffer), 0)
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := Trunc(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := Trunc(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFVariant then
      Result := (TDFVariant(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := (TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFUInt64 then
      Result := (TDFUInt64(Obj_).Buffer)
  else if Obj_ is TDFInt128 then
      Result := TDFInt128(Obj_).Buffer.ToUInt32
  else if Obj_ is TDFUInt128 then
      Result := TDFUInt128(Obj_).Buffer.ToUInt32
  else
      Result := 0;
end;

function TDFE.ReadWord(index_: Integer): Word;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := umlStrToInt(umlStringOf(TDFString(Obj_).Buffer), 0)
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := Trunc(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := Trunc(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFVariant then
      Result := (TDFVariant(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := (TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFUInt64 then
      Result := (TDFUInt64(Obj_).Buffer)
  else if Obj_ is TDFInt128 then
      Result := TDFInt128(Obj_).Buffer.ToUInt16
  else if Obj_ is TDFUInt128 then
      Result := TDFUInt128(Obj_).Buffer.ToUInt16
  else
      Result := 0;
end;

function TDFE.ReadBool(index_: Integer): Boolean;
begin
  Result := ReadByte(index_) = 1;
end;

function TDFE.ReadBoolean(index_: Integer): Boolean;
begin
  Result := ReadBool(index_);
end;

function TDFE.ReadByte(index_: Integer): Byte;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := umlStrToInt(umlStringOf(TDFString(Obj_).Buffer), 0)
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := Trunc(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := Trunc(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFVariant then
      Result := (TDFVariant(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := (TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFUInt64 then
      Result := (TDFUInt64(Obj_).Buffer)
  else if Obj_ is TDFInt128 then
      Result := TDFInt128(Obj_).Buffer.ToUInt8
  else if Obj_ is TDFUInt128 then
      Result := TDFUInt128(Obj_).Buffer.ToUInt8
  else
      Result := 0;
end;

function TDFE.ReadSingle(index_: Integer): Single;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFSingle then
      Result := TDFSingle(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := umlStrToFloat(umlStringOf(TDFString(Obj_).Buffer), 0)
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFDouble then
      Result := TDFDouble(Obj_).Buffer
  else if Obj_ is TDFVariant then
      Result := (TDFVariant(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := (TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFUInt64 then
      Result := (TDFUInt64(Obj_).Buffer)
  else if Obj_ is TDFInt128 then
      Result := TDFInt128(Obj_).Buffer.ToInt32
  else if Obj_ is TDFUInt128 then
      Result := TDFUInt128(Obj_).Buffer.ToInt32
  else
      Result := 0;
end;

function TDFE.ReadDouble(index_: Integer): Double;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFDouble then
      Result := TDFDouble(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := umlStrToFloat(umlStringOf(TDFString(Obj_).Buffer), 0)
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := TDFSingle(Obj_).Buffer
  else if Obj_ is TDFVariant then
      Result := (TDFVariant(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := (TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFUInt64 then
      Result := (TDFUInt64(Obj_).Buffer)
  else if Obj_ is TDFInt128 then
      Result := TDFInt128(Obj_).Buffer.ToInt64
  else if Obj_ is TDFUInt128 then
      Result := TDFUInt128(Obj_).Buffer.ToInt64
  else
      Result := 0;
end;

function TDFE.ReadArrayInteger(index_: Integer): TDFArrayInteger;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFArrayInteger then
      Result := TDFArrayInteger(Obj_)
  else
      Result := nil;
end;

function TDFE.ReadArrayShortInt(index_: Integer): TDFArrayShortInt;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFArrayShortInt then
      Result := TDFArrayShortInt(Obj_)
  else
      Result := nil;
end;

function TDFE.ReadArrayByte(index_: Integer): TDFArrayByte;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFArrayByte then
      Result := TDFArrayByte(Obj_)
  else
      Result := nil;
end;

function TDFE.ReadMD5(index_: Integer): TMD5;
begin
  with ReadArrayByte(index_) do
      GetBuff(@Result[0]);
end;

function TDFE.ReadArraySingle(index_: Integer): TDFArraySingle;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFArraySingle then
      Result := TDFArraySingle(Obj_)
  else
      Result := nil;
end;

function TDFE.ReadArrayDouble(index_: Integer): TDFArrayDouble;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFArrayDouble then
      Result := TDFArrayDouble(Obj_)
  else
      Result := nil;
end;

function TDFE.ReadArrayInt64(index_: Integer): TDFArrayInt64;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFArrayInt64 then
      Result := TDFArrayInt64(Obj_)
  else
      Result := nil;
end;

procedure TDFE.ReadMem64(index_: Integer; output: TMem64);
var
  Obj_: TDFBase;
  LNeedResetPos: Boolean;
begin
  Obj_ := Data[index_];
  LNeedResetPos := output.Size = 0;
  if Obj_ is TDFStream then
    begin
      with TDFStream(Obj_) do
        begin
          Buffer64.Position := 0;
          output.CopyFrom(Buffer64, Buffer64.Size);
          Buffer64.Position := 0;
        end;
    end
  else
      RaiseInfo('no support');
  if LNeedResetPos then
      output.Position := 0;
end;

procedure TDFE.ReadStream(index_: Integer; output: TCore_Stream);
var
  Obj_: TDFBase;
  LNeedResetPos: Boolean;
begin
  Obj_ := Data[index_];
  LNeedResetPos := output.Size = 0;
  if Obj_ is TDFStream then
    begin
      with TDFStream(Obj_) do
        begin
          if (output is TMS64) and (output.Size = 0) then
            begin
              output.Size := Buffer.Size;
              output.Position := 0;
            end;
          Buffer64.Position := 0;
          output.CopyFrom(Buffer64, Buffer64.Size);
          Buffer64.Position := 0;
        end;
    end
  else if output is TMS64 then
      Obj_.SaveToStream(TMS64(output))
  else
      RaiseInfo('no support');
  if LNeedResetPos then
      output.Position := 0;
end;

function TDFE.ReadVariant(index_: Integer): Variant;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFVariant then
      Result := TDFVariant(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := TDFString(Obj_).Buffer
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := TDFSingle(Obj_).Buffer
  else if Obj_ is TDFDouble then
      Result := TDFDouble(Obj_).Buffer
  else if Obj_ is TDFInt64 then
      Result := (TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFInt128 then
      Result := TDFInt128(Obj_).Buffer.ToString.Text
  else if Obj_ is TDFUInt128 then
      Result := TDFUInt128(Obj_).Buffer.ToString.Text
  else
      Result := 0;
end;

function TDFE.ReadInt64(index_: Integer): Int64;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFInt64 then
      Result := TDFInt64(Obj_).Buffer
  else if Obj_ is TDFUInt64 then
      Result := TDFUInt64(Obj_).Buffer
  else if Obj_ is TDFInt128 then
      Result := TDFInt128(Obj_).Buffer.ToInt64
  else if Obj_ is TDFUInt128 then
      Result := TDFUInt128(Obj_).Buffer.ToInt64
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := Trunc(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := Trunc(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFVariant then
      Result := TDFVariant(Obj_).Buffer
  else
      Result := 0;
end;

function TDFE.ReadUInt64(index_: Integer): UInt64;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFUInt64 then
      Result := TDFUInt64(Obj_).Buffer
  else if Obj_ is TDFInt64 then
      Result := TDFInt64(Obj_).Buffer
  else if Obj_ is TDFInt128 then
      Result := TDFInt128(Obj_).Buffer.ToUInt64
  else if Obj_ is TDFUInt128 then
      Result := TDFUInt128(Obj_).Buffer.ToUInt64
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := Trunc(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := Trunc(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFVariant then
      Result := TDFVariant(Obj_).Buffer
  else
      Result := 0;
end;

function TDFE.ReadArrayInt128(index_: Integer): TDFArrayInt128;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFArrayInt128 then
      Result := TDFArrayInt128(Obj_)
  else
      Result := nil;
end;

function TDFE.ReadInt128(index_: Integer): Int128;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFInt128 then
      Result := TDFInt128(Obj_).Buffer
  else if Obj_ is TDFUInt128 then
      Result := TDFUInt128(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := umlStringOf(TDFString(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := TDFInt64(Obj_).Buffer
  else if Obj_ is TDFUInt64 then
      Result := TDFUInt64(Obj_).Buffer
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := Trunc(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := Trunc(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFVariant then
      Result := VarToStr(TDFVariant(Obj_).Buffer)
  else
      Result := 0;
end;

function TDFE.ReadUInt128(index_: Integer): UInt128;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFUInt128 then
      Result := TDFUInt128(Obj_).Buffer
  else if Obj_ is TDFInt128 then
      Result := TDFInt128(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := umlStringOf(TDFString(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := TDFInt64(Obj_).Buffer
  else if Obj_ is TDFUInt64 then
      Result := TDFUInt64(Obj_).Buffer
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := Trunc(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := Trunc(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFVariant then
      Result := VarToStr(TDFVariant(Obj_).Buffer)
  else
      Result := 0;
end;

procedure TDFE.ReadStrings(index_: Integer; output: TCore_Strings);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  ReadStream(index_, m64);
  m64.Position := 0;

{$IFDEF FPC}
  output.LoadFromStream(m64);
{$ELSE}
  output.LoadFromStream(m64, TEncoding.UTF8);
{$ENDIF}
  DisposeObject(m64);
end;

procedure TDFE.ReadListStrings(index_: Integer; output: TListString);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  ReadStream(index_, m64);
  m64.Position := 0;

  output.LoadFromStream(m64);
  DisposeObject(m64);
end;

procedure TDFE.ReadPascalStrings(index_: Integer; output: TPascalStringList);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  ReadStream(index_, m64);
  m64.Position := 0;

  output.LoadFromStream(m64);
  DisposeObject(m64);
end;

procedure TDFE.ReadPascalStrings(index_: Integer; var output: U_StringArray);
var
  L: TPascalStringList;
  i: Integer;
begin
  L := TPascalStringList.Create;
  ReadPascalStrings(index_, L);
  SetLength(output, L.Count);
  for i := 0 to L.Count - 1 do
      output[i] := L[i];
  DisposeObject(L);
end;

procedure TDFE.ReadDataFrame(index_: Integer; output: TDFE);
var
  Obj_: TDFBase;
  m64: TMS64;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFStream then
    begin
      TDFStream(Obj_).Buffer.Position := 0;
      output.DecodeFrom(TDFStream(Obj_).Buffer, True);
      TDFStream(Obj_).Buffer.Position := 0;
    end
  else
    begin
      m64 := TMS64.Create;
      ReadStream(index_, m64);
      m64.Position := 0;
      output.DecodeFrom(m64, True);
      DisposeObject(m64);
    end;
end;

procedure TDFE.ReadDF(index_: Integer; output: TDFE);
begin
  ReadDataFrame(index_, output);
end;

procedure TDFE.ReadDFE(index_: Integer; output: TDFE);
begin
  ReadDataFrame(index_, output);
end;

procedure TDFE.ReadHashStringList(index_: Integer; output: THashStringList);
var
  m64: TMS64;
  hash_: THashStringTextStream;
begin
  m64 := TMS64.Create;
  ReadStream(index_, m64);
  m64.Position := 0;
  hash_ := THashStringTextStream.Create(output);
  hash_.LoadFromStream(m64);
  DisposeObject(hash_);
  DisposeObject(m64);
end;

procedure TDFE.ReadVariantList(index_: Integer; output: THashVariantList);
var
  m64: TMS64;
  hash_: THashVariantTextStream;
begin
  m64 := TMS64.Create;
  ReadStream(index_, m64);
  m64.Position := 0;
  hash_ := THashVariantTextStream.Create(output);
  hash_.LoadFromStream(m64);
  DisposeObject(hash_);
  DisposeObject(m64);
end;

procedure TDFE.ReadJson(index_: Integer; output: TZ_JsonObject);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  ReadStream(index_, m64);
  m64.Position := 0;
  output.Clear;
  output.LoadFromStream(m64);
  DisposeObject(m64);
end;

{$IFDEF DELPHI}


procedure TDFE.ReadJson(index_: Integer; output: TJsonObject);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  ReadStream(index_, m64);
  m64.Position := 0;
  output.Clear;
  output.LoadFromStream(m64, TEncoding.UTF8, True);
  DisposeObject(m64);
end;

{$ENDIF DELPHI}


function TDFE.ReadRect(index_: Integer): TRect;
begin
  with ReadArrayInteger(index_) do
    begin
      Result := Rect(Buffer[0], Buffer[1], Buffer[2], Buffer[3]);
    end;
end;

function TDFE.ReadRectf(index_: Integer): TRectf;
begin
  with ReadArraySingle(index_) do
    begin
      Result := Rectf(Buffer[0], Buffer[1], Buffer[2], Buffer[3]);
    end;
end;

function TDFE.ReadPoint(index_: Integer): TPoint;
begin
  with ReadArrayInteger(index_) do
    begin
      Result := Point(Buffer[0], Buffer[1]);
    end;
end;

function TDFE.ReadPointf(index_: Integer): TPointf;
begin
  with ReadArraySingle(index_) do
    begin
      Result := Pointf(Buffer[0], Buffer[1]);
    end;
end;

function TDFE.ReadVector(index_: Integer): TVector;
begin
  with ReadArraySingle(index_) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
      Result[2] := Buffer[2];
      Result[3] := Buffer[3];
    end;
end;

function TDFE.ReadAffineVector(index_: Integer): TAffineVector;
begin
  with ReadArraySingle(index_) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
      Result[2] := Buffer[2];
    end;
end;

function TDFE.ReadVec3(index_: Integer): TVec3;
begin
  with ReadArraySingle(index_) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
      Result[2] := Buffer[2];
    end;
end;

function TDFE.ReadVec4(index_: Integer): TVec4;
begin
  with ReadArraySingle(index_) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
      Result[2] := Buffer[2];
      Result[3] := Buffer[3];
    end;
end;

function TDFE.ReadVector3(index_: Integer): TVector3;
begin
  with ReadArraySingle(index_) do
    begin
      Result := Vector3(Buffer[0], Buffer[1], Buffer[2]);
    end;
end;

function TDFE.ReadVector4(index_: Integer): TVector4;
begin
  with ReadArraySingle(index_) do
    begin
      Result := Vector4(Buffer[0], Buffer[1], Buffer[2], Buffer[3]);
    end;
end;

function TDFE.ReadMat4(index_: Integer): TMat4;
var
  i, j: Integer;
begin
  with ReadArraySingle(index_) do
    begin
      for i := 0 to 3 do
        for j := 0 to 3 do
            Result[i][j] := Buffer[i * 4 + j];
    end;
end;

function TDFE.ReadMatrix4(index_: Integer): TMatrix4;
begin
  Result.buff := ReadMat4(index_);
end;

function TDFE.Read2DPoint(index_: Integer): T2DPoint;
begin
  with ReadArraySingle(index_) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
    end;
end;

function TDFE.ReadVec2(index_: Integer): TVec2;
begin
  Result := Read2DPoint(index_);
end;

function TDFE.ReadRectV2(index_: Integer): TRectV2;
begin
  with ReadArraySingle(index_) do
    begin
      Result[0][0] := Buffer[0];
      Result[0][1] := Buffer[1];
      Result[1][0] := Buffer[2];
      Result[1][1] := Buffer[3];
    end;
end;

function TDFE.ReadPointer(index_: Integer): UInt64;
begin
  Result := ReadUInt64(index_);
end;

function TDFE.ReadPtr(index_: Integer): UInt64;
begin
  Result := ReadUInt64(index_);
end;

procedure TDFE.Read(index_: Integer; var Buf_; Count_: Int64);
var
  s: TMS64;
begin
  s := TMS64.Create;
  ReadStream(index_, s);
  s.Read64(Buf_, Count_);
  DisposeObject(s);
end;

procedure TDFE.ReadNM(index_: Integer; output: TNumberModule);
var
  D_: TDFE;
begin
  D_ := TDFE.Create;
  ReadDataFrame(index_, D_);
  output.Name := D_.Reader.ReadString;
  output.DirectOrigin := D_.Reader.ReadVariant;
  output.DirectValue := D_.Reader.ReadVariant;
  DisposeObject(D_);
end;

procedure TDFE.ReadNMPool(index_: Integer; output: TNumberModulePool);
var
  D_, Tmp_: TDFE;
  DM: TNumberModule;
  N_: SystemString;
  L_: TCore_ListForObj;
  i: Integer;
begin
  L_ := TCore_ListForObj.Create;
  D_ := TDFE.Create;
  ReadDataFrame(index_, D_);
  while D_.Reader.NotEnd do
    begin
      Tmp_ := TDFE.Create;
      D_.Reader.ReadDataFrame(Tmp_);
      N_ := Tmp_.Reader.ReadString;
      DM := output[N_];
      DM.Name := N_;
      DM.DirectOrigin := Tmp_.Reader.ReadVariant;
      DM.DirectValue := Tmp_.Reader.ReadVariant;
      L_.Add(DM);
      DisposeObject(Tmp_);
    end;
  DisposeObject(D_);
  for i := 0 to L_.Count - 1 do
    begin
      DM := TNumberModule(L_[i]);
      DM.DoChange;
    end;
  DisposeObject(L_);
end;

procedure TDFE.ReadOpCode(index_: Integer; output: TOpCode);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  ReadStream(index_, m64);
  m64.Position := 0;
  TOpCode.LoadFromStream(m64, output);
  DisposeObject(m64);
end;

procedure TDFE.ReadSectionText(index_: Integer; output: THashTextEngine);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  ReadStream(index_, m64);
  m64.Position := 0;
  output.LoadFromStream(m64);
  DisposeObject(m64);
end;

procedure TDFE.ReadTextSection(index_: Integer; output: THashTextEngine);
begin
  ReadSectionText(index_, output);
end;

function TDFE.Read(index_: Integer): TDFBase;
begin
  Result := Data[index_];
end;

function TDFE.ComputeEncodeSize: Int64;
var
  i: Integer;
begin
  Result := C_Integer_Size;
  for i := 0 to Count - 1 do
      Result := Result + C_Byte_Size + GetData(i).ComputeEncodeSize;
end;

class procedure TDFE.BuildEmptyStream(output: TCore_Stream);
type
  THead32_ = packed record
    EditionToken: Byte;
    sizeInfo: Cardinal;
    compToken: Byte;
    md5: TMD5;
    num: Integer;
  end;
var
  head_: THead32_;
begin
  // make header
  head_.EditionToken := C_Bit_32;
  head_.sizeInfo := C_Integer_Size;
  head_.compToken := C_None_Compress;
  head_.md5 := NullMD5;
  head_.num := 0;
  output.write(head_, SizeOf(THead32_));
end;

function TDFE.FastEncode32To(output: TCore_Stream; SizeInfo32: Cardinal): Integer;
type
  THead32_ = packed record
    EditionToken: Byte;
    SizeInfo32: Cardinal;
    compToken: Byte;
    md5: TMD5;
  end;
var
  head_: THead32_;
  i: Integer;
  DataFrame_: TDFBase;
  ID: Byte;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  // make header
  head_.EditionToken := C_Bit_32;
  head_.SizeInfo32 := SizeInfo32;
  head_.compToken := C_None_Compress;
  head_.md5 := NullMD5;

  // write header
  output.write(head_, SizeOf(THead32_));

  // write body
  output.write(Result, C_Integer_Size);
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := GetData(i);
      ID := DataFrame_.FID;
      output.write(DataFrame_.FID, C_Byte_Size);
      DataFrame_.SaveToStream(output);
    end;
end;

function TDFE.FastEncode64To(output: TCore_Stream; SizeInfo64: Int64): Integer;
type
  THead64_ = packed record
    EditionToken: Byte;
    SizeInfo64: Int64;
    compToken: Byte;
    md5: TMD5;
  end;
var
  head_: THead64_;
  i: Integer;
  DataFrame_: TDFBase;
  ID: Byte;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  // make header
  head_.EditionToken := C_Bit_64;
  head_.SizeInfo64 := SizeInfo64;
  head_.compToken := C_None_Compress;
  head_.md5 := NullMD5;

  // write header
  output.write(head_, SizeOf(THead64_));

  // write body
  output.write(Result, C_Integer_Size);
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := GetData(i);
      ID := DataFrame_.FID;
      output.write(DataFrame_.FID, C_Byte_Size);
      DataFrame_.SaveToStream(output);
    end;
end;

function TDFE.FastEncodeTo(output: TCore_Stream): Integer;
var
  SizeInfo64: Int64;
begin
  SizeInfo64 := ComputeEncodeSize;
  if (output is TMS64) then
      TMS64(output).Delta := umlMax(TMS64(output).Delta, SizeInfo64);
  if SizeInfo64 > FBit_64_Condition then
      Result := FastEncode64To(output, SizeInfo64)
  else
      Result := FastEncode32To(output, SizeInfo64);
end;

function TDFE.EncodeTo(output: TCore_Stream; const FastMode, AutoCompressed: Boolean): Integer;
var
  i: Integer;
  DataFrame_: TDFBase;
  StoreStream, nStream: TMS64;
  ID: Byte;

  EditionToken: Byte;
  SizeInfo32: Cardinal;
  SizeInfo64: Int64;
  compToken: Byte;
  md5: TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  // if encode size too large(>1M), we use EncodeAsSelectCompressor
  if (AutoCompressed) and (ComputeEncodeSize > 1024 * 1024) then
    begin
      Result := EncodeAsSelectCompressor(TSelectCompressionMethod.scmZLIB_Fast, output, FastMode);
      exit;
    end;

  if FastMode and (not AutoCompressed) then
    begin
      Result := FastEncodeTo(output);
      exit;
    end;

  StoreStream := TMS64.CustomCreate(8192);

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMS64.Create;
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := GetData(i);
      ID := DataFrame_.FID;
      DataFrame_.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // make header
  SizeInfo32 := Cardinal(StoreStream.Size);
  SizeInfo64 := StoreStream.Size;
  if SizeInfo64 > FBit_64_Condition then
      EditionToken := C_Bit_64
  else
      EditionToken := C_Bit_32;
  compToken := C_None_Compress;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  // prepare write header
  nStream.Clear;
  nStream.write(EditionToken, C_Byte_Size);
  if SizeInfo64 > FBit_64_Condition then
      nStream.write(SizeInfo64, C_Int64_Size)
  else
      nStream.write(SizeInfo32, C_Cardinal_Size);
  nStream.write(compToken, C_Byte_Size);
  nStream.write(md5[0], C_MD5_Size);

  // write header
  nStream.Position := 0;
  output.CopyFrom(nStream, nStream.Size);
  DisposeObject(nStream);

  // write body
  StoreStream.Position := 0;
  output.CopyFrom(StoreStream, StoreStream.Size);
  DisposeObject(StoreStream);
end;

function TDFE.EncodeTo(output: TCore_Stream; const FastMode: Boolean): Integer;
begin
  Result := EncodeTo(output, FastMode, True);
end;

function TDFE.EncodeTo(output: TCore_Stream): Integer;
begin
  Result := EncodeTo(output, False);
end;

procedure TDFE.Encrypt(output: TCore_Stream; Compressed_: Boolean; SecurityLevel: Integer; Key: TCipherKeyBuffer);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  if Compressed_ then
      EncodeAsSelectCompressor(m64, True)
  else
      EncodeTo(m64, True);

  QuantumEncrypt(m64, output, SecurityLevel, Key);
  DisposeObject(m64);
end;

function TDFE.Decrypt(input: TCore_Stream; Key: TCipherKeyBuffer): Boolean;
var
  m64: TMS64;
begin
  if input.Size = 0 then
    begin
      Result := False;
      exit;
    end;
  m64 := TMS64.Create;
  Result := QuantumDecrypt(input, m64, Key);
  if Result then
    begin
      m64.Position := 0;
      DecodeFrom(m64, True);
    end;

  DisposeObject(m64);
end;

procedure TDFE.EncodeAsPublicJson(var output: TPascalString);
var
  m64: TMS64;
  buff: TBytes;
begin
  m64 := TMS64.Create;
  EncodeAsPublicJson(m64);
  SetLength(buff, m64.Size);
  CopyPtr(m64.Memory, @buff[0], m64.Size);
  DisposeObject(m64);
  output.Bytes := buff;
  SetLength(buff, 0);
end;

procedure TDFE.EncodeAsPublicJson(output: TCore_Stream);
var
  j: TZ_JsonObject;
  i: Integer;
begin
  j := TZ_JsonObject.Create;
  j.s['help'] := 'This JSON with TDFE encode';

  for i := 0 to Count - 1 do
    begin
      j.A['Ref'].Add(TDFBase(FDataList[i]).FID);
      TDFBase(FDataList[i]).SaveToJson(j.A['Data'], i);
    end;

  j.SaveToStream(output, True);

  DisposeObject(j);
end;

procedure TDFE.EncodeAsJson(output: TCore_Stream);
var
  j: TZ_JsonObject;
  i: Integer;
  DataFrame_: TDFBase;
begin
  j := TZ_JsonObject.Create;

  for i := 0 to Count - 1 do
    begin
      DataFrame_ := TDFBase(FDataList[i]);
      DataFrame_.SaveToJson(j.A['Data'], i);
      j.A['Ref'].Add(DataFrame_.FID);
    end;

  j.SaveToStream(output, False);

  DisposeObject(j);
end;

procedure TDFE.EncodeAsJson(Json: TZ_JsonObject);
var
  i: Integer;
  DataFrame_: TDFBase;
begin
  Json.Clear;
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := TDFBase(FDataList[i]);
      DataFrame_.SaveToJson(Json.A['Data'], i);
      Json.A['Ref'].Add(DataFrame_.FID);
    end;
end;

procedure TDFE.DecodeFromJson(stream: TCore_Stream);
var
  j: TZ_JsonObject;
  t: Byte;
  i: Integer;
  DataFrame_: TDFBase;
begin
  Clear;
  j := TZ_JsonObject.Create;
  try
      j.LoadFromStream(stream);
  except
    DisposeObject(j);
    exit;
  end;

  try
    for i := 0 to j.A['Ref'].Count - 1 do
      begin
        t := j.A['Ref'].i[i];
        DataFrame_ := AddData(ByteToDataType(t));
        DataFrame_.LoadFromJson(j.A['Data'], i);
      end;
  except
    DisposeObject(j);
    exit;
  end;

  DisposeObject(j);
end;

procedure TDFE.DecodeFromJson(const s: TPascalString);
var
  buff: TBytes;
  m64: TMS64;
begin
  buff := s.Bytes;
  m64 := TMS64.Create;
  m64.SetPointerWithProtectedMode(@buff[0], length(buff));
  m64.Position := 0;
  DecodeFromJson(m64);
  DisposeObject(m64);
  SetLength(buff, 0);
end;

procedure TDFE.DecodeFromJson(Json: TZ_JsonObject);
var
  t: Byte;
  i: Integer;
  DataFrame_: TDFBase;
begin
  Clear;

  for i := 0 to Json.A['Ref'].Count - 1 do
    begin
      t := Json.A['Ref'].i[i];
      DataFrame_ := AddData(ByteToDataType(t));
      DataFrame_.LoadFromJson(Json.A['Data'], i);
    end;
end;

function TDFE.EncodeAsSelectCompressor(scm: TSelectCompressionMethod; output: TCore_Stream; const FastMode: Boolean): Integer;
var
  i: Integer;
  DataFrame_: TDFBase;
  StoreStream, nStream, compStream: TMS64;
  ID: Byte;

  EditionToken: Byte;
  SizeInfo32: Cardinal;
  SizeInfo64: Int64;
  compToken: Byte;
  CompSizeInfo32: Cardinal;
  CompSizeInfo64: Int64;
  md5: TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  StoreStream := TMS64.CustomCreate(8192);

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMS64.Create;
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := GetData(i);
      ID := DataFrame_.FID;
      DataFrame_.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // compress body and make header
  CompSizeInfo32 := Cardinal(StoreStream.Size);
  CompSizeInfo64 := StoreStream.Size;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  compStream := TMS64.CustomCreate(64 * 1024);
  ParallelCompressMemory(scm, StoreStream, compStream);
  DisposeObject(StoreStream);

  // make header
  SizeInfo32 := Cardinal(compStream.Size);
  SizeInfo64 := compStream.Size;
  if SizeInfo64 > FBit_64_Condition then
      EditionToken := C_Bit_64
  else
      EditionToken := C_Bit_32;
  if CompSizeInfo64 > FBit_64_Condition then
      compToken := C_Parallel_64_Compress
  else
      compToken := C_Parallel_32_Compress;

  // prepare write header
  nStream.Clear;
  nStream.write(EditionToken, C_Byte_Size);
  if SizeInfo64 > FBit_64_Condition then
      nStream.write(SizeInfo64, C_Int64_Size)
  else
      nStream.write(SizeInfo32, C_Cardinal_Size);
  nStream.write(compToken, C_Byte_Size);
  if CompSizeInfo64 > FBit_64_Condition then
      nStream.write(CompSizeInfo64, C_Int64_Size)
  else
      nStream.write(CompSizeInfo32, C_Cardinal_Size);
  nStream.write(md5[0], C_MD5_Size);

  // write header
  nStream.Position := 0;
  output.CopyFrom(nStream, nStream.Size);
  DisposeObject(nStream);

  // write body
  compStream.Position := 0;
  output.CopyFrom(compStream, compStream.Size);
  DisposeObject(compStream);
end;

function TDFE.EncodeAsSelectCompressor(output: TCore_Stream; const FastMode: Boolean): Integer;
var
  scm: TSelectCompressionMethod;
begin
  if ComputeEncodeSize > 64 * 1024 then
    begin
      if FastMode then
          scm := TSelectCompressionMethod.scmZLIB_Fast
      else
          scm := TSelectCompressionMethod.scmZLIB_Max;
      Result := EncodeAsSelectCompressor(scm, output, FastMode);
    end
  else
      Result := EncodeAsZLib(output, FastMode);
end;

function TDFE.EncodeAsSelectCompressor(output: TCore_Stream): Integer;
begin
  Result := EncodeAsSelectCompressor(output, False);
end;

function TDFE.EncodeAsZLib(output: TCore_Stream; const FastMode, AutoCompressed: Boolean): Integer;
var
  i: Integer;
  DataFrame_: TDFBase;
  StoreStream, nStream, compStream: TMS64;
  ZCompStream: TCompressionStream;
  ID: Byte;

  EditionToken: Byte;
  SizeInfo32: Cardinal;
  SizeInfo64: Int64;
  compToken: Byte;
  CompSizeInfo32: Cardinal;
  CompSizeInfo64: Int64;
  md5: TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  // if encode size too large(>1M), we use EncodeAsSelectCompressor
  if AutoCompressed and (ComputeEncodeSize > 1024 * 1024) then
    begin
      Result := EncodeAsSelectCompressor(TSelectCompressionMethod.scmZLIB, output, FastMode);
      exit;
    end;

  StoreStream := TMS64.CustomCreate(8192);

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMS64.Create;
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := GetData(i);
      ID := DataFrame_.FID;
      DataFrame_.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // compress body and make header
  CompSizeInfo32 := Cardinal(StoreStream.Size);
  CompSizeInfo64 := StoreStream.Size;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  compStream := TMS64.CustomCreate(64 * 1024);
  ZCompStream := TCompressionStream.Create(compStream);
  StoreStream.Position := 0;
  ZCompStream.CopyFrom(StoreStream, StoreStream.Size);
  DisposeObject(ZCompStream);
  DisposeObject(StoreStream);

  // make header
  SizeInfo32 := compStream.Size;
  SizeInfo64 := compStream.Size;
  if SizeInfo64 > FBit_64_Condition then
      EditionToken := C_Bit_64
  else
      EditionToken := C_Bit_32;
  if CompSizeInfo64 > FBit_64_Condition then
      compToken := C_ZLIB_64_Compress
  else
      compToken := C_ZLIB_32_Compress;

  // prepare write header
  nStream.Clear;
  nStream.write(EditionToken, C_Byte_Size);
  if SizeInfo64 > FBit_64_Condition then
      nStream.write(SizeInfo64, C_Int64_Size)
  else
      nStream.write(SizeInfo32, C_Cardinal_Size);
  nStream.write(compToken, C_Byte_Size);
  if CompSizeInfo64 > FBit_64_Condition then
      nStream.write(CompSizeInfo64, C_Int64_Size)
  else
      nStream.write(CompSizeInfo32, C_Cardinal_Size);
  nStream.write(md5[0], C_MD5_Size);

  // write header
  nStream.Position := 0;
  output.CopyFrom(nStream, nStream.Size);
  DisposeObject(nStream);

  // write body
  compStream.Position := 0;
  output.CopyFrom(compStream, compStream.Size);
  DisposeObject(compStream);
end;

function TDFE.EncodeAsZLib(output: TCore_Stream; const FastMode: Boolean): Integer;
begin
  Result := EncodeAsZLib(output, FastMode, True);
end;

function TDFE.EncodeAsZLib(output: TCore_Stream): Integer;
begin
  Result := EncodeAsZLib(output, False);
end;

function TDFE.EncodeAsDeflate(output: TCore_Stream; const FastMode, AutoCompressed: Boolean): Integer;
var
  i: Integer;
  DataFrame_: TDFBase;
  StoreStream, nStream, compStream: TMS64;
  ID: Byte;

  EditionToken: Byte;
  SizeInfo32: Cardinal;
  SizeInfo64: Int64;
  compToken: Byte;
  CompSizeInfo32: Cardinal;
  CompSizeInfo64: Int64;
  md5: TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  // if encode size too large(>1M), we use EncodeAsSelectCompressor
  if AutoCompressed and (ComputeEncodeSize > 1024 * 1024) then
    begin
      Result := EncodeAsSelectCompressor(TSelectCompressionMethod.scmZLIB, output, FastMode);
      exit;
    end;

  StoreStream := TMS64.CustomCreate(8192);

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMS64.Create;
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := GetData(i);
      ID := DataFrame_.FID;
      DataFrame_.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // compress body and make header
  CompSizeInfo32 := Cardinal(StoreStream.Size);
  CompSizeInfo64 := StoreStream.Size;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  compStream := TMS64.Create;
  StoreStream.Position := 0;

  if FCompressorDeflate = nil then
      FCompressorDeflate := TCompressorDeflate.Create;

  CoreCompressStream(FCompressorDeflate, StoreStream, compStream);
  DisposeObject(StoreStream);

  // make header
  SizeInfo32 := Cardinal(compStream.Size);
  SizeInfo64 := compStream.Size;
  if SizeInfo64 > FBit_64_Condition then
      EditionToken := C_Bit_64
  else
      EditionToken := C_Bit_32;
  if CompSizeInfo64 > FBit_64_Condition then
      compToken := C_Deflate_64_Compress
  else
      compToken := C_Deflate_32_Compress;

  // prepare write header
  nStream.Clear;
  nStream.write(EditionToken, C_Byte_Size);
  if SizeInfo64 > FBit_64_Condition then
      nStream.write(SizeInfo64, C_Int64_Size)
  else
      nStream.write(SizeInfo32, C_Cardinal_Size);
  nStream.write(compToken, C_Byte_Size);
  if CompSizeInfo64 > FBit_64_Condition then
      nStream.write(CompSizeInfo64, C_Int64_Size)
  else
      nStream.write(CompSizeInfo32, C_Cardinal_Size);
  nStream.write(md5[0], C_MD5_Size);

  // write header
  nStream.Position := 0;
  output.CopyFrom(nStream, nStream.Size);
  DisposeObject(nStream);

  // write body
  compStream.Position := 0;
  output.CopyFrom(compStream, compStream.Size);
  DisposeObject(compStream);
end;

function TDFE.EncodeAsDeflate(output: TCore_Stream; const FastMode: Boolean): Integer;
begin
  Result := EncodeAsDeflate(output, FastMode, True);
end;

function TDFE.EncodeAsDeflate(output: TCore_Stream): Integer;
begin
  Result := EncodeAsDeflate(output, False);
end;

function TDFE.EncodeAsBRRC(output: TCore_Stream; const FastMode, AutoCompressed: Boolean): Integer;
var
  i: Integer;
  DataFrame_: TDFBase;
  StoreStream, nStream, compStream: TMS64;
  ID: Byte;
  EditionToken: Byte;
  SizeInfo32: Cardinal;
  SizeInfo64: Int64;
  compToken: Byte;
  CompSizeInfo32: Cardinal;
  CompSizeInfo64: Int64;
  md5: TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  // if encode size too large(>1M), we use EncodeAsSelectCompressor
  if AutoCompressed and (ComputeEncodeSize > 1024 * 1024) then
    begin
      Result := EncodeAsSelectCompressor(TSelectCompressionMethod.scmZLIB, output, FastMode);
      exit;
    end;

  StoreStream := TMS64.CustomCreate(8192);

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMS64.Create;
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := GetData(i);
      ID := DataFrame_.FID;
      DataFrame_.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // compress body and make header
  CompSizeInfo32 := Cardinal(StoreStream.Size);
  CompSizeInfo64 := StoreStream.Size;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  compStream := TMS64.Create;
  StoreStream.Position := 0;

  if FCompressorBRRC = nil then
      FCompressorBRRC := TCompressorBRRC.Create;

  CoreCompressStream(FCompressorBRRC, StoreStream, compStream);
  DisposeObject(StoreStream);

  // make header
  SizeInfo32 := Cardinal(compStream.Size);
  SizeInfo64 := compStream.Size;
  if SizeInfo64 > FBit_64_Condition then
      EditionToken := C_Bit_64
  else
      EditionToken := C_Bit_32;
  if CompSizeInfo64 > FBit_64_Condition then
      compToken := C_BRRC_64_Compress
  else
      compToken := C_BRRC_32_Compress;

  // prepare write header
  nStream.Clear;
  nStream.write(EditionToken, C_Byte_Size);
  if SizeInfo64 > FBit_64_Condition then
      nStream.write(SizeInfo64, C_Int64_Size)
  else
      nStream.write(SizeInfo32, C_Cardinal_Size);
  nStream.write(compToken, C_Byte_Size);
  if CompSizeInfo64 > FBit_64_Condition then
      nStream.write(CompSizeInfo64, C_Int64_Size)
  else
      nStream.write(CompSizeInfo32, C_Cardinal_Size);
  nStream.write(md5[0], C_MD5_Size);

  // write header
  nStream.Position := 0;
  output.CopyFrom(nStream, nStream.Size);
  DisposeObject(nStream);

  // write body
  compStream.Position := 0;
  output.CopyFrom(compStream, compStream.Size);
  DisposeObject(compStream);
end;

function TDFE.EncodeAsBRRC(output: TCore_Stream; const FastMode: Boolean): Integer;
begin
  Result := EncodeAsBRRC(output, FastMode, True);
end;

function TDFE.EncodeAsBRRC(output: TCore_Stream): Integer;
begin
  Result := EncodeAsBRRC(output, False);
end;

function TDFE.IsCompressed(source: TCore_Stream): Boolean;
var
  bakPos: Int64;
  EditionToken: Byte;
  SizeInfo32: Cardinal;
  SizeInfo64, sizeInfo: Int64;
  compToken: Byte;
begin
  bakPos := source.Position;
  Result := False;

  source.Read(EditionToken, C_Byte_Size);
  if (EditionToken in [C_Bit_32, C_Bit_64]) then
    begin
      if EditionToken = C_Bit_32 then
        begin
          source.Read(SizeInfo32, C_Cardinal_Size);
          sizeInfo := SizeInfo32;
        end
      else
        begin
          source.Read(SizeInfo64, C_Int64_Size);
          sizeInfo := SizeInfo64;
        end;

      source.Read(compToken, C_Byte_Size);

      Result := compToken in [
        C_ZLIB_32_Compress, C_ZLIB_64_Compress,
        C_Deflate_32_Compress, C_Deflate_64_Compress,
        C_BRRC_32_Compress, C_BRRC_64_Compress,
        C_Parallel_32_Compress, C_Parallel_64_Compress]
    end;

  source.Position := bakPos;
end;

function TDFE.DecodeFrom(source: TCore_Stream; const FastMode: Boolean; const Max_Limit_Decode: Integer): Integer;
var
  i, num_: Integer;
  ID: Byte;
  StoreStream: TMS64;
  ZDecompStream: TDecompressionStream;
  DataFrame_: TDFBase;

  EditionToken: Byte;
  SizeInfo32: Cardinal;
  SizeInfo64, sizeInfo: Int64;
  compToken: Byte;
  CompSizeInfo32: Cardinal;
  CompSizeInfo64, compsizeInfo: Int64;
  MD5_: TMD5;
begin
  Clear;

  Result := -1;
  if source.Read(EditionToken, C_Byte_Size) <> C_Byte_Size then
      exit;

  StoreStream := TMS64.CustomCreate(64 * 1024);

  if (EditionToken in [C_Bit_32, C_Bit_64]) then
    begin
      if EditionToken = C_Bit_32 then
        begin
          if source.Read(SizeInfo32, C_Cardinal_Size) <> C_Cardinal_Size then
            begin
              DisposeObject(StoreStream);
              exit;
            end;
          sizeInfo := SizeInfo32;
        end
      else
        begin
          if source.Read(SizeInfo64, C_Int64_Size) <> C_Int64_Size then
            begin
              DisposeObject(StoreStream);
              exit;
            end;
          sizeInfo := SizeInfo64;
        end;

      source.Read(compToken, C_Byte_Size);

      if compToken = C_None_Compress then
        begin
          if source.Read(MD5_[0], 16) <> 16 then
            begin
              DisposeObject(StoreStream);
              exit;
            end;

          if source is TMS64 then
            begin
              StoreStream.Mapping(TMS64(source).PositionAsPtr, sizeInfo);
              TMS64(source).Position := TMS64(source).Position + sizeInfo;
            end
          else
            begin
              if sizeInfo > 0 then
                  StoreStream.CopyFrom(source, sizeInfo);
            end;

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(MD5_)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), MD5_) then
              begin
                DoStatus('MD5 error!');
                DisposeObject(StoreStream);
                exit;
              end;
        end
      else if compToken in [C_ZLIB_32_Compress, C_ZLIB_64_Compress] then
        begin
          if compToken = C_ZLIB_32_Compress then
            begin
              if source.Read(CompSizeInfo32, C_Cardinal_Size) <> C_Cardinal_Size then
                begin
                  DisposeObject(StoreStream);
                  exit;
                end;
              compsizeInfo := CompSizeInfo32;
            end
          else
            begin
              if source.Read(CompSizeInfo64, C_Int64_Size) <> C_Int64_Size then
                begin
                  DisposeObject(StoreStream);
                  exit;
                end;
              compsizeInfo := CompSizeInfo64;
            end;

          if source.Read(MD5_[0], 16) <> 16 then
            begin
              DisposeObject(StoreStream);
              exit;
            end;

          ZDecompStream := TDecompressionStream.Create(source);
          StoreStream.CopyFrom(ZDecompStream, compsizeInfo);
          DisposeObject(ZDecompStream);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(MD5_)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), MD5_) then
              begin
                DoStatus('ZLIB MD5 error!');
                DisposeObject(StoreStream);
                exit;
              end;
        end
      else if compToken in [C_Deflate_32_Compress, C_Deflate_64_Compress] then
        begin
          if compToken = C_Deflate_32_Compress then
            begin
              if source.Read(CompSizeInfo32, C_Cardinal_Size) <> C_Cardinal_Size then
                begin
                  DisposeObject(StoreStream);
                  exit;
                end;
              compsizeInfo := CompSizeInfo32;
            end
          else
            begin
              if source.Read(CompSizeInfo64, C_Int64_Size) <> C_Int64_Size then
                begin
                  DisposeObject(StoreStream);
                  exit;
                end;
              compsizeInfo := CompSizeInfo64;
            end;

          if source.Read(MD5_[0], 16) <> 16 then
            begin
              DisposeObject(StoreStream);
              exit;
            end;

          if FCompressorDeflate = nil then
              FCompressorDeflate := TCompressorDeflate.Create;
          CoreDecompressStream(FCompressorDeflate, source, StoreStream);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(MD5_)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), MD5_) then
              begin
                DoStatus('Deflate MD5 error!');
                DisposeObject(StoreStream);
                exit;
              end;
        end
      else if compToken in [C_BRRC_32_Compress, C_BRRC_64_Compress] then
        begin
          if compToken = C_BRRC_32_Compress then
            begin
              if source.Read(CompSizeInfo32, C_Cardinal_Size) <> C_Cardinal_Size then
                begin
                  DisposeObject(StoreStream);
                  exit;
                end;
              compsizeInfo := CompSizeInfo32;
            end
          else
            begin
              if source.Read(CompSizeInfo64, C_Int64_Size) <> C_Int64_Size then
                begin
                  DisposeObject(StoreStream);
                  exit;
                end;
              compsizeInfo := CompSizeInfo64;
            end;

          if source.Read(MD5_[0], 16) <> 16 then
            begin
              DisposeObject(StoreStream);
              exit;
            end;

          if FCompressorBRRC = nil then
              FCompressorBRRC := TCompressorBRRC.Create;
          CoreDecompressStream(FCompressorBRRC, source, StoreStream);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(MD5_)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), MD5_) then
              begin
                DoStatus('BRRC MD5 error!');
                DisposeObject(StoreStream);
                exit;
              end;
        end
      else if compToken in [C_Parallel_32_Compress, C_Parallel_64_Compress] then
        begin
          if compToken = C_Parallel_32_Compress then
            begin
              if source.Read(CompSizeInfo32, C_Cardinal_Size) <> C_Cardinal_Size then
                begin
                  DisposeObject(StoreStream);
                  exit;
                end;
              compsizeInfo := CompSizeInfo32;
            end
          else
            begin
              if source.Read(CompSizeInfo64, C_Int64_Size) <> C_Int64_Size then
                begin
                  DisposeObject(StoreStream);
                  exit;
                end;
              compsizeInfo := CompSizeInfo64;
            end;

          if source.Read(MD5_[0], 16) <> 16 then
            begin
              DisposeObject(StoreStream);
              exit;
            end;

          ParallelDecompressStream(source, StoreStream);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(MD5_)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), MD5_) then
              begin
                DoStatus('select compression MD5 error!');
                DisposeObject(StoreStream);
                exit;
              end;
        end;

      StoreStream.Position := 0;

      StoreStream.Read64(num_, C_Integer_Size);

      if Max_Limit_Decode > 0 then
          num_ := umlMin(num_ - 1, Max_Limit_Decode);

      for i := 0 to num_ - 1 do
        begin
          StoreStream.Read64(ID, C_Byte_Size);
          DataFrame_ := AddData(ByteToDataType(ID));
          DataFrame_.LoadFromStream(StoreStream);
        end;
      DisposeObject(StoreStream);
      Result := num_;
    end
  else
    begin
      DoStatus('TDFE decode error!');
      DisposeObject(StoreStream);
      exit;
    end;
end;

function TDFE.DecodeFrom(source: TCore_Stream; const FastMode: Boolean): Integer;
begin
  Result := DecodeFrom(source, FastMode, 0);
end;

function TDFE.DecodeFrom(source: TCore_Stream): Integer;
begin
  Result := DecodeFrom(source, False);
end;

function TDFE.DecodeFromMemory(memory_: Pointer; mSize: Int64; const FastMode: Boolean): Integer;
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  m64.Mapping(memory_, mSize);
  Result := DecodeFrom(m64, FastMode);
  DisposeObject(m64);
end;

function TDFE.DecodeFromMemory(memory_: Pointer; mSize: Int64): Integer;
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  m64.Mapping(memory_, mSize);
  Result := DecodeFrom(m64, False);
  DisposeObject(m64);
end;

function TDFE.DecodeFromMemory(stream: TMS64; const FastMode: Boolean): Integer;
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  m64.Mapping(stream);
  Result := DecodeFrom(m64, FastMode);
  DisposeObject(m64);
end;

function TDFE.DecodeFromMemory(stream: TMem64; const FastMode: Boolean): Integer;
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  m64.Mapping(stream);
  Result := DecodeFrom(m64, FastMode);
  DisposeObject(m64);
end;

procedure TDFE.EncodeToBytes(const Compressed, FastMode: Boolean; var output: TBytes);
var
  enStream: TMS64;
begin
  enStream := TMS64.Create;
  if Compressed then
      EncodeAsSelectCompressor(enStream, FastMode)
  else
      EncodeTo(enStream, FastMode);

  SetLength(output, enStream.Size);
  CopyPtr(enStream.Memory, @output[0], enStream.Size);
  DisposeObject(enStream);
end;

procedure TDFE.DecodeFromBytes(var buff: TBytes);
begin
  DecodeFromBytes(buff, False);
end;

procedure TDFE.DecodeFromBytes(var buff: TBytes; const FastMode: Boolean);
var
  enStream: TMS64;
begin
  enStream := TMS64.Create;
  enStream.SetPointerWithProtectedMode(@buff[0], length(buff));
  DecodeFrom(enStream, FastMode);
  DisposeObject(enStream);
end;

function TDFE.GetMD5(const FastMode: Boolean): TMD5;
var
  enStream: TMS64;
begin
  enStream := TMS64.Create;
  FastEncodeTo(enStream);

  Result := umlMD5(enStream.Memory, enStream.Size);
  DisposeObject(enStream);
end;

function TDFE.Compare(source: TDFE): Boolean;
var
  i: Integer;
  s1, s2: TMS64;
begin
  Result := False;

  if Count <> source.Count then
      exit;

  s1 := TMS64.CustomCreate(8192);
  s2 := TMS64.CustomCreate(8192);
  try
    for i := 0 to Count - 1 do
      begin
        if FDataList[i].ClassType <> source[i].ClassType then
            exit;
        if TDFBase(FDataList[i]).FID <> TDFBase(source[i]).FID then
            exit;
        if TDFBase(FDataList[i]).ComputeEncodeSize <> TDFBase(source[i]).ComputeEncodeSize then
            exit;

        s1.Clear;
        s2.Clear;
        TDFBase(FDataList[i]).SaveToStream(s1);
        TDFBase(source[i]).SaveToStream(s2);
        if s1.Size <> s2.Size then
            exit;
        if not CompareMemory(s1.Memory, s2.Memory, s1.Size) then
            exit;
        s1.Clear;
        s2.Clear;
      end;
    Result := True;
  finally
    DisposeObject(s1);
    DisposeObject(s2);
  end;
end;

procedure TDFE.LoadFromStream(stream: TCore_Stream);
begin
  try
      DecodeFrom(stream);
  except
  end;
end;

procedure TDFE.SaveToStream(stream: TCore_Stream);
var
  siz: Integer;
begin
  try
    siz := ComputeEncodeSize;
    if siz > 1024 then
        EncodeAsSelectCompressor(stream)
    else
        EncodeTo(stream);
  except
  end;
end;

procedure TDFE.LoadFromFile(fileName_: U_String);
var
  fs: TCore_FileStream;
begin
  fs := TCore_FileStream.Create(fileName_, fmOpenRead or fmShareDenyNone);
  LoadFromStream(fs);
  DisposeObject(fs);
end;

procedure TDFE.SaveToFile(fileName_: U_String);
var
  fs: TCore_FileStream;
begin
  fs := TCore_FileStream.Create(fileName_, fmCreate);
  SaveToStream(fs);
  DisposeObject(fs);
end;

class procedure TDFE.Test();
  procedure Test_Encode_1(d: TDFE; m5: TMD5);
  var
    inst: TDFE;
    m64: TMS64;
  begin
    inst := d.Clone;
    m64 := TMS64.Create;
    m64.Size := 64 * 1024;
    inst.EncodeTo(m64, True, True);
    inst.Clear;
    m64.Position := 0;
    inst.DecodeFrom(m64);
    if not umlMD5Compare(m5, inst.GetMD5(True)) then
        DoStatus('encode error.');
    DisposeObject(m64);
    DisposeObject(inst);
  end;

  procedure Test_Encode_2(d: TDFE; m5: TMD5);
  var
    inst: TDFE;
    m64: TMS64;
  begin
    inst := d.Clone;
    m64 := TMS64.Create;
    m64.Size := 64 * 1024;
    inst.EncodeTo(m64, False, True);
    inst.Clear;
    m64.Position := 0;
    inst.DecodeFrom(m64);
    if not umlMD5Compare(m5, inst.GetMD5(True)) then
        DoStatus('encode error.');
    DisposeObject(m64);
    DisposeObject(inst);
  end;

  procedure Test_Encode_3(d: TDFE; m5: TMD5);
  var
    inst: TDFE;
    m64: TMS64;
  begin
    inst := d.Clone;
    m64 := TMS64.Create;
    m64.Size := 64 * 1024;
    inst.EncodeTo(m64, False, False);
    inst.Clear;
    m64.Position := 0;
    inst.DecodeFrom(m64);
    if not umlMD5Compare(m5, inst.GetMD5(True)) then
        DoStatus('encode error.');
    DisposeObject(m64);
    DisposeObject(inst);
  end;

  procedure Test_Fast_Encode32(d: TDFE; m5: TMD5);
  var
    inst: TDFE;
    m64: TMS64;
  begin
    inst := d.Clone;
    m64 := TMS64.Create;
    m64.Size := 64 * 1024;
    inst.FastEncode32To(m64, inst.ComputeEncodeSize);
    inst.Clear;
    m64.Position := 0;
    inst.DecodeFrom(m64);
    if not umlMD5Compare(m5, inst.GetMD5(True)) then
        DoStatus('encode error.');
    DisposeObject(m64);
    DisposeObject(inst);
  end;

  procedure Test_Fast_Encode64(d: TDFE; m5: TMD5);
  var
    inst: TDFE;
    m64: TMS64;
  begin
    inst := d.Clone;
    m64 := TMS64.Create;
    m64.Size := 64 * 1024;
    inst.FastEncode64To(m64, inst.ComputeEncodeSize);
    inst.Clear;
    m64.Position := 0;
    inst.DecodeFrom(m64);
    if not umlMD5Compare(m5, inst.GetMD5(True)) then
        DoStatus('encode error.');
    DisposeObject(m64);
    DisposeObject(inst);
  end;

  procedure Test_SelectCompressor_Encode(d: TDFE; m5: TMD5);
  var
    inst: TDFE;
    m64: TMS64;
  begin
    inst := d.Clone;
    m64 := TMS64.Create;
    m64.Size := 64 * 1024;
    inst.EncodeAsSelectCompressor(m64, True);
    inst.Clear;
    m64.Position := 0;
    inst.DecodeFrom(m64);
    if not umlMD5Compare(m5, inst.GetMD5(True)) then
        DoStatus('encode error.');
    DisposeObject(m64);
    DisposeObject(inst);
  end;

  procedure Test_ZLIB_Encode(d: TDFE; m5: TMD5);
  var
    inst: TDFE;
    m64: TMS64;
  begin
    inst := d.Clone;
    m64 := TMS64.Create;
    m64.Size := 64 * 1024;
    inst.EncodeAsZLib(m64, True, False);
    inst.Clear;
    m64.Position := 0;
    inst.DecodeFrom(m64);
    if not umlMD5Compare(m5, inst.GetMD5(True)) then
        DoStatus('encode error.');
    DisposeObject(m64);
    DisposeObject(inst);
  end;

  procedure Test_BRRC_Encode(d: TDFE; m5: TMD5);
  var
    inst: TDFE;
    m64: TMS64;
  begin
    inst := d.Clone;
    m64 := TMS64.Create;
    m64.Size := 64 * 1024;
    inst.EncodeAsBRRC(m64, True, False);
    inst.Clear;
    m64.Position := 0;
    inst.DecodeFrom(m64);
    if not umlMD5Compare(m5, inst.GetMD5(True)) then
        DoStatus('encode error.');
    DisposeObject(m64);
    DisposeObject(inst);
  end;

  procedure Test_Deflate_Encode(d: TDFE; m5: TMD5);
  var
    inst: TDFE;
    m64: TMS64;
  begin
    inst := d.Clone;
    m64 := TMS64.Create;
    m64.Size := 64 * 1024;
    inst.EncodeAsDeflate(m64, True, False);
    inst.Clear;
    m64.Position := 0;
    inst.DecodeFrom(m64);
    if not umlMD5Compare(m5, inst.GetMD5(True)) then
        DoStatus('encode error.');
    DisposeObject(m64);
    DisposeObject(inst);
  end;

var
  inst: TDFE;
  m64: TMS64;
  m5: TMD5;
begin
  inst := TDFE.Create;
  inst.WriteString('hello world');
  inst.WriteInteger(1);
  inst.WriteCardinal(2);
  inst.WriteWORD(3);
  inst.WriteBool(True);
  inst.WriteByte(5);
  inst.WriteSingle(6);
  inst.WriteDouble(7);
  inst.WriteArrayInteger.WriteArray([1, 2, 3, 4, 5]);
  inst.WriteArrayShortInt.WriteArray([1, 2, 3, 4, 5]);
  inst.WriteArrayByte.WriteArray([1, 2, 3, 4, 5]);
  inst.WriteArraySingle.WriteArray([1, 2, 3, 4, 5]);
  inst.WriteArrayDouble.WriteArray([1, 2, 3, 4, 5]);
  inst.WriteArrayInt64.WriteArray([1, 2, 3, 4, 5]);
  inst.WriteArrayInt128.WriteArray([1, 2, 3, 4, 5]);
  inst.WriteInt64(8);
  inst.WriteUInt64(9);
  inst.WriteInt128(10);
  inst.WriteUInt128(11);
  m64 := TMS64.Create;
  m64.Size := 1024 * 32;
  TMT19937.Rand32(MaxInt, m64.Memory, m64.Size shl 2);
  inst.WriteStream(m64);
  DisposeObject(m64);
  m5 := inst.GetMD5(True);

  // test 32 Bit
  inst.FBit_64_Condition := 1024 * 1024;
  Test_Encode_1(inst, m5);
  Test_Encode_2(inst, m5);
  Test_Encode_3(inst, m5);
  Test_Fast_Encode32(inst, m5);
  Test_Fast_Encode64(inst, m5);
  Test_SelectCompressor_Encode(inst, m5);
  Test_ZLIB_Encode(inst, m5);
  Test_BRRC_Encode(inst, m5);
  Test_Deflate_Encode(inst, m5);

  // simulate test 64 Bit
  inst.FBit_64_Condition := 1024;
  m5 := inst.GetMD5(True);
  Test_Encode_1(inst, m5);
  Test_Encode_2(inst, m5);
  Test_Encode_3(inst, m5);
  Test_Fast_Encode32(inst, m5);
  Test_Fast_Encode64(inst, m5);
  Test_SelectCompressor_Encode(inst, m5);
  Test_ZLIB_Encode(inst, m5);
  Test_BRRC_Encode(inst, m5);
  Test_Deflate_Encode(inst, m5);

  DisposeObject(inst);
end;

constructor TDataWriter.Create(Stream_: TCore_Stream);
begin
  inherited Create;
  FEngine := TDFE.Create;
  FStream := Stream_;
end;

destructor TDataWriter.Destroy;
var
  FlagCompressed: Boolean;
  verflag: TBytes;
  siz: Int64;
  M: TMS64;
begin
  if FStream <> nil then
    begin
      M := TMS64.Create;
      FEngine.FastEncodeTo(M);
      siz := M.Size;

      // write version flag
      verflag := umlBytesOf('0001');
      FStream.write(verflag, 4);

      // write compressed flag
      FlagCompressed := False;
      FStream.write(FlagCompressed, C_Boolean_Size);

      // write siz info
      FStream.write(siz, C_Int64_Size);

      // write buffer
      M.Position := 0;
      FStream.CopyFrom(M, siz);
      DisposeObject(M);
    end;

  DisposeObject(FEngine);
  inherited Destroy;
end;

procedure TDataWriter.Clear;
begin
  FEngine.Clear;
end;

procedure TDataWriter.WriteString(v: SystemString);
begin
  FEngine.WriteString(v);
end;

procedure TDataWriter.WriteInteger(v: Integer);
begin
  FEngine.WriteInteger(v);
end;

procedure TDataWriter.WriteCardinal(v: Cardinal);
begin
  FEngine.WriteCardinal(v);
end;

procedure TDataWriter.WriteWORD(v: Word);
begin
  FEngine.WriteWORD(v);
end;

procedure TDataWriter.WriteBool(v: Boolean);
begin
  FEngine.WriteBool(v);
end;

procedure TDataWriter.WriteBoolean(v: Boolean);
begin
  FEngine.WriteBoolean(v);
end;

procedure TDataWriter.WriteByte(v: Byte);
begin
  FEngine.WriteByte(v);
end;

procedure TDataWriter.WriteSingle(v: Single);
begin
  FEngine.WriteSingle(v);
end;

procedure TDataWriter.WriteDouble(v: Double);
begin
  FEngine.WriteDouble(v);
end;

procedure TDataWriter.WriteArrayInteger(v: array of Integer);
begin
  FEngine.WriteArrayInteger.WriteArray(v);
end;

procedure TDataWriter.WriteArrayShortInt(v: array of ShortInt);
begin
  FEngine.WriteArrayShortInt.WriteArray(v);
end;

procedure TDataWriter.WriteArrayByte(v: array of Byte);
begin
  FEngine.WriteArrayByte.WriteArray(v);
end;

procedure TDataWriter.WriteArraySingle(v: array of Single);
begin
  FEngine.WriteArraySingle.WriteArray(v);
end;

procedure TDataWriter.WriteArrayDouble(v: array of Double);
begin
  FEngine.WriteArrayDouble.WriteArray(v);
end;

procedure TDataWriter.WriteArrayInt64(v: array of Int64);
begin
  FEngine.WriteArrayInt64.WriteArray(v);
end;

procedure TDataWriter.WriteStream(v: TCore_Stream);
begin
  FEngine.WriteStream(v);
end;

procedure TDataWriter.WriteStream(v: TMS64; bPos_, Size_: Int64);
begin
  FEngine.WriteStream(v, bPos_, Size_);
end;

procedure TDataWriter.WriteVariant(v: Variant);
begin
  FEngine.WriteVariant(v);
end;

procedure TDataWriter.WriteInt64(v: Int64);
begin
  FEngine.WriteInt64(v);
end;

procedure TDataWriter.WriteUInt64(v: UInt64);
begin
  FEngine.WriteUInt64(v);
end;

procedure TDataWriter.WriteArrayInt128(v: array of Int128);
begin
  FEngine.WriteArrayInt128.WriteArray(v);
end;

procedure TDataWriter.WriteInt128(v: Int128);
begin
  FEngine.WriteInt128(v);
end;

procedure TDataWriter.WriteUInt128(v: UInt128);
begin
  FEngine.WriteUInt128(v);
end;

procedure TDataWriter.WriteStrings(v: TCore_Strings);
begin
  FEngine.WriteStrings(v);
end;

procedure TDataWriter.WriteListStrings(v: TListString);
begin
  FEngine.WriteListStrings(v);
end;

procedure TDataWriter.WritePascalStrings(v: TPascalStringList);
begin
  FEngine.WritePascalStrings(v);
end;

procedure TDataWriter.WritePascalStrings(v: U_StringArray);
begin
  FEngine.WritePascalStrings(v);
end;

procedure TDataWriter.WriteDataFrame(v: TDFE);
begin
  FEngine.WriteDataFrame(v);
end;

procedure TDataWriter.WriteDataFrameCompressed(v: TDFE);
begin
  FEngine.WriteDataFrameCompressed(v);
end;

procedure TDataWriter.WriteHashStringList(v: THashStringList);
begin
  FEngine.WriteHashStringList(v);
end;

procedure TDataWriter.WriteVariantList(v: THashVariantList);
begin
  FEngine.WriteVariantList(v);
end;

procedure TDataWriter.WriteJson(v: TZ_JsonObject);
begin
  FEngine.WriteJson(v);
end;

{$IFDEF DELPHI}


procedure TDataWriter.WriteJson(v: TJsonObject);
begin
  FEngine.WriteJson(v);
end;
{$ENDIF DELPHI}


procedure TDataWriter.WriteRect(v: TRect);
begin
  FEngine.WriteRect(v);
end;

procedure TDataWriter.WriteRectf(v: TRectf);
begin
  FEngine.WriteRectf(v);
end;

procedure TDataWriter.WritePoint(v: TPoint);
begin
  FEngine.WritePoint(v);
end;

procedure TDataWriter.WritePointf(v: TPointf);
begin
  FEngine.WritePointf(v);
end;

procedure TDataWriter.WriteVector(v: TVector);
begin
  FEngine.WriteVector(v);
end;

procedure TDataWriter.WriteAffineVector(v: TAffineVector);
begin
  FEngine.WriteAffineVector(v);
end;

procedure TDataWriter.WriteVec4(v: TVec4);
begin
  FEngine.WriteVec4(v);
end;

procedure TDataWriter.WriteVec3(v: TVec3);
begin
  FEngine.WriteVec3(v);
end;

procedure TDataWriter.WriteVector4(v: TVector4);
begin
  FEngine.WriteVector4(v);
end;

procedure TDataWriter.WriteVector3(v: TVector3);
begin
  FEngine.WriteVector3(v);
end;

procedure TDataWriter.WriteMat4(v: TMat4);
begin
  FEngine.WriteMat4(v);
end;

procedure TDataWriter.WriteMatrix4(v: TMatrix4);
begin
  FEngine.WriteMatrix4(v);
end;

procedure TDataWriter.Write2DPoint(v: T2DPoint);
begin
  FEngine.Write2DPoint(v);
end;

procedure TDataWriter.WriteVec2(v: TVec2);
begin
  FEngine.WriteVec2(v);
end;

procedure TDataWriter.WriteRectV2(v: TRectV2);
begin
  FEngine.WriteRectV2(v);
end;

procedure TDataWriter.WritePointer(v: Pointer);
begin
  FEngine.WritePointer(v);
end;

procedure TDataWriter.write(const Buf_; Count_: Int64);
begin
  FEngine.write(Buf_, Count_);
end;

function TDataWriter.WriteNM(NM: TNumberModule): TDFE;
begin
  FEngine.WriteNM(NM);
end;

function TDataWriter.WriteNMPool(NMPool: TNumberModulePool): TDFE;
begin
  FEngine.WriteNMPool(NMPool);
end;

function TDataWriter.WriteOpCode(v: TOpCode): TDFE;
begin
  FEngine.WriteOpCode(v);
end;

function TDataWriter.WriteSectionText(v: THashTextEngine): TDFE;
begin
  FEngine.WriteSectionText(v);
end;

function TDataWriter.WriteTextSection(v: THashTextEngine): TDFE;
begin
  FEngine.WriteTextSection(v);
end;

constructor TDataReader.Create(Stream_: TCore_Stream);
var
  verflag: TBytes;
  FlagCompressed: Boolean;
  siz: Int64;
  M: TMS64;
begin
  inherited Create;
  FEngine := TDFE.Create;
  if Stream_ <> nil then
    begin
      // read version flag
      SetLength(verflag, 4);
      Stream_.Read(verflag, 4);
      if umlStringOf(verflag) <> '0001' then
          raise Exception.Create('Version flag Does not match!');

      // read compressed flag
      Stream_.Read(FlagCompressed, C_Boolean_Size);

      // read size info
      Stream_.Read(siz, C_Int64_Size);

      // read buffer
      M := TMS64.Create;
      M.CopyFrom(Stream_, siz);
      M.Position := 0;
      FEngine.DecodeFrom(M);
      DisposeObject(M);
    end;
end;

destructor TDataReader.Destroy;
begin
  DisposeObject(FEngine);
  inherited Destroy;
end;

function TDataReader.ReadString: SystemString;
begin
  Result := FEngine.Reader.ReadString;
end;

function TDataReader.ReadInteger: Integer;
begin
  Result := FEngine.Reader.ReadInteger;
end;

function TDataReader.ReadCardinal: Cardinal;
begin
  Result := FEngine.Reader.ReadCardinal;
end;

function TDataReader.ReadWord: Word;
begin
  Result := FEngine.Reader.ReadWord;
end;

function TDataReader.ReadBool: Boolean;
begin
  Result := FEngine.Reader.ReadBool;
end;

function TDataReader.ReadBoolean: Boolean;
begin
  Result := FEngine.Reader.ReadBoolean;
end;

function TDataReader.ReadByte: Byte;
begin
  Result := FEngine.Reader.ReadByte;
end;

function TDataReader.ReadSingle: Single;
begin
  Result := FEngine.Reader.ReadSingle;
end;

function TDataReader.ReadDouble: Double;
begin
  Result := FEngine.Reader.ReadDouble;
end;

procedure TDataReader.ReadArrayInteger(var Data: array of Integer);
var
  i: Integer;
  rb: TDFArrayInteger;
begin
  rb := FEngine.Reader.ReadArrayInteger;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayShortInt(var Data: array of ShortInt);
var
  i: Integer;
  rb: TDFArrayShortInt;
begin
  rb := FEngine.Reader.ReadArrayShortInt;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayByte(var Data: array of Byte);
var
  i: Integer;
  rb: TDFArrayByte;
begin
  rb := FEngine.Reader.ReadArrayByte;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArraySingle(var Data: array of Single);
var
  i: Integer;
  rb: TDFArraySingle;
begin
  rb := FEngine.Reader.ReadArraySingle;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayDouble(var Data: array of Double);
var
  i: Integer;
  rb: TDFArrayDouble;
begin
  rb := FEngine.Reader.ReadArrayDouble;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayInt64(var Data: array of Int64);
var
  i: Integer;
  rb: TDFArrayInt64;
begin
  rb := FEngine.Reader.ReadArrayInt64;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadStream(output: TCore_Stream);
begin
  FEngine.Reader.ReadStream(output);
end;

function TDataReader.ReadVariant: Variant;
begin
  Result := FEngine.Reader.ReadVariant;
end;

function TDataReader.ReadInt64: Int64;
begin
  Result := FEngine.Reader.ReadInt64;
end;

function TDataReader.ReadUInt64: UInt64;
begin
  Result := FEngine.Reader.ReadUInt64;
end;

procedure TDataReader.ReadArrayInt128(var Data: array of Int128);
var
  i: Integer;
  rb: TDFArrayInt128;
begin
  rb := FEngine.Reader.ReadArrayInt128;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

function TDataReader.ReadInt128: Int128;
begin
  Result := FEngine.Reader.ReadInt128;
end;

function TDataReader.ReadUInt128: UInt128;
begin
  Result := FEngine.Reader.ReadUInt128;
end;

procedure TDataReader.ReadStrings(output: TCore_Strings);
begin
  FEngine.Reader.ReadStrings(output);
end;

procedure TDataReader.ReadListStrings(output: TListString);
begin
  FEngine.Reader.ReadListStrings(output);
end;

procedure TDataReader.ReadPascalStrings(output: TPascalStringList);
begin
  FEngine.Reader.ReadPascalStrings(output);
end;

procedure TDataReader.ReadPascalStrings(var output: U_StringArray);
begin
  FEngine.Reader.ReadPascalStrings(output);
end;

procedure TDataReader.ReadDataFrame(output: TDFE);
begin
  FEngine.Reader.ReadDataFrame(output);
end;

procedure TDataReader.ReadHashStringList(output: THashStringList);
begin
  FEngine.Reader.ReadHashStringList(output);
end;

procedure TDataReader.ReadVariantList(output: THashVariantList);
begin
  FEngine.Reader.ReadVariantList(output);
end;

procedure TDataReader.ReadJson(output: TZ_JsonObject);
begin
  FEngine.Reader.ReadJson(output);
end;

{$IFDEF DELPHI}


procedure TDataReader.ReadJson(output: TJsonObject);
begin
  FEngine.Reader.ReadJson(output);
end;
{$ENDIF DELPHI}


function TDataReader.ReadRect: TRect;
begin
  Result := FEngine.Reader.ReadRect;
end;

function TDataReader.ReadRectf: TRectf;
begin
  Result := FEngine.Reader.ReadRectf;
end;

function TDataReader.ReadPoint: TPoint;
begin
  Result := FEngine.Reader.ReadPoint;
end;

function TDataReader.ReadPointf: TPointf;
begin
  Result := FEngine.Reader.ReadPointf;
end;

function TDataReader.ReadVector: TVector;
begin
  Result := FEngine.Reader.ReadVector;
end;

function TDataReader.ReadAffineVector: TAffineVector;
begin
  Result := FEngine.Reader.ReadAffineVector;
end;

function TDataReader.ReadVec3: TVec3;
begin
  Result := FEngine.Reader.ReadVec3;
end;

function TDataReader.ReadVec4: TVec4;
begin
  Result := FEngine.Reader.ReadVec4;
end;

function TDataReader.ReadVector3: TVector3;
begin
  Result := FEngine.Reader.ReadVector3;
end;

function TDataReader.ReadVector4: TVector4;
begin
  Result := FEngine.Reader.ReadVector4;
end;

function TDataReader.ReadMat4: TMat4;
begin
  Result := FEngine.Reader.ReadMat4;
end;

function TDataReader.ReadMatrix4: TMatrix4;
begin
  Result := FEngine.Reader.ReadMatrix4;
end;

function TDataReader.Read2DPoint: T2DPoint;
begin
  Result := FEngine.Reader.Read2DPoint;
end;

function TDataReader.ReadVec2: TVec2;
begin
  Result := FEngine.Reader.ReadVec2;
end;

function TDataReader.ReadRectV2: TRectV2;
begin
  Result := FEngine.Reader.ReadRectV2;
end;

function TDataReader.ReadPointer: UInt64;
begin
  Result := FEngine.Reader.ReadPointer;
end;

procedure TDataReader.ReadNM(output: TNumberModule);
begin
  FEngine.Reader.ReadNM(output);
end;

procedure TDataReader.ReadNMPool(output: TNumberModulePool);
begin
  FEngine.Reader.ReadNMPool(output);
end;

procedure TDataReader.ReadOpCode(output: TOpCode);
begin
  FEngine.Reader.ReadOpCode(output);
end;

procedure TDataReader.ReadSectionText(output: THashTextEngine);
begin
  FEngine.Reader.ReadSectionText(output);
end;

procedure TDataReader.ReadTextSection(output: THashTextEngine);
begin
  FEngine.Reader.ReadTextSection(output);
end;

procedure TDataReader.Read(var Buf_; Count_: Int64);
begin
  FEngine.Reader.Read(Buf_, Count_);
end;

end.
 
