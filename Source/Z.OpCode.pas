{ ****************************************************************************** }
{ * Expression OpCode                                                          * }
{ ****************************************************************************** }
unit Z.OpCode;

{$I Z.Define.inc}

interface

uses SysUtils, Variants, Math,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.ListEngine, Z.UnicodeMixedLib, Z.HashList.Templet;

type
  TOpValueType = (
    ovtBool, ovtInt, ovtInt64, ovtUInt64, ovtWord, ovtByte, ovtSmallInt, ovtShortInt, ovtUInt,
    ovtSingle, ovtDouble, ovtCurrency,
    ovtString, ovtProc,
    ovtUnknow);

  TOpCode = class;
  TOpCustomRunTime = class;
  TOpParam = array of Variant;
  TOnOp_C = function(var OP_Param: TOpParam): Variant;
  TOnOp_M = function(var OP_Param: TOpParam): Variant of object;
  TOnObjectOp_C = function(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
  TOnObjectOp_M = function(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant of object;
{$IFDEF FPC}
  TOnOp_P = function(var OP_Param: TOpParam): Variant is nested;
  TOnObjectOp_P = function(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant is nested;
{$ELSE FPC}
  TOnOp_P = reference to function(var OP_Param: TOpParam): Variant;
  TOnObjectOp_P = reference to function(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
{$ENDIF FPC}
  TOpRT_Mode = (rtmDirect, rtmSync, rtmPost);

  TOpRTData = record
  public
    Name, Description, Category: SystemString;
    OnOp_C: TOnOp_C;
    OnOp_M: TOnOp_M;
    OnOp_P: TOnOp_P;
    OnObjectOp_C: TOnObjectOp_C;
    OnObjectOp_M: TOnObjectOp_M;
    OnObjectOp_P: TOnObjectOp_P;
    Mode: TOpRT_Mode;
    procedure Init;
  end;

  POpRTData = ^TOpRTData;

  TOpRT_Sync_Bridge = class
  public
    opRT: TOpCustomRunTime;
    OC: TOpCode;
    OD: POpRTData;
    R_: Variant;
    procedure Do_Sync_Run;
  end;

  TOpSystemAPI = class(TCore_Object)
  private
    function DoInt(var OP_Param: TOpParam): Variant;
    function DoFrac(var OP_Param: TOpParam): Variant;
    function DoExp(var OP_Param: TOpParam): Variant;
    function DoCos(var OP_Param: TOpParam): Variant;
    function DoSin(var OP_Param: TOpParam): Variant;
    function DoLn(var OP_Param: TOpParam): Variant;
    function DoArcTan(var OP_Param: TOpParam): Variant;
    function DoSqrt(var OP_Param: TOpParam): Variant;
    function DoSqr(var OP_Param: TOpParam): Variant;
    function DoTan(var OP_Param: TOpParam): Variant;
    function DoRound(var OP_Param: TOpParam): Variant;
    function DoTrunc(var OP_Param: TOpParam): Variant;
    function DoDeg(var OP_Param: TOpParam): Variant;
    function DoPower(var OP_Param: TOpParam): Variant;
    function DoSingle(var OP_Param: TOpParam): Variant;
    function DoDouble(var OP_Param: TOpParam): Variant;
    function DoExtended(var OP_Param: TOpParam): Variant;
    function DoByte(var OP_Param: TOpParam): Variant;
    function DoWord(var OP_Param: TOpParam): Variant;
    function DoCardinal(var OP_Param: TOpParam): Variant;
    function DoUInt64(var OP_Param: TOpParam): Variant;
    function DoShortInt(var OP_Param: TOpParam): Variant;
    function DoSmallInt(var OP_Param: TOpParam): Variant;
    function DoInteger(var OP_Param: TOpParam): Variant;
    function DoInt64(var OP_Param: TOpParam): Variant;
    function DoROL8(var OP_Param: TOpParam): Variant;
    function DoROL16(var OP_Param: TOpParam): Variant;
    function DoROL32(var OP_Param: TOpParam): Variant;
    function DoROL64(var OP_Param: TOpParam): Variant;
    function DoROR8(var OP_Param: TOpParam): Variant;
    function DoROR16(var OP_Param: TOpParam): Variant;
    function DoROR32(var OP_Param: TOpParam): Variant;
    function DoROR64(var OP_Param: TOpParam): Variant;
    function DoEndian16(var OP_Param: TOpParam): Variant;
    function DoEndian32(var OP_Param: TOpParam): Variant;
    function DoEndian64(var OP_Param: TOpParam): Variant;
    function DoEndianU16(var OP_Param: TOpParam): Variant;
    function DoEndianU32(var OP_Param: TOpParam): Variant;
    function DoEndianU64(var OP_Param: TOpParam): Variant;
    function DoSAR16(var OP_Param: TOpParam): Variant;
    function DoSAR32(var OP_Param: TOpParam): Variant;
    function DoSAR64(var OP_Param: TOpParam): Variant;
    function DoNot(var OP_Param: TOpParam): Variant;
    function DoPI(var OP_Param: TOpParam): Variant;
    function DoBool(var OP_Param: TOpParam): Variant;
    function DoTrue(var OP_Param: TOpParam): Variant;
    function DoFalse(var OP_Param: TOpParam): Variant;
    function DoRColor(var OP_Param: TOpParam): Variant;
    function DoVec2(var OP_Param: TOpParam): Variant;
    function DoVec3(var OP_Param: TOpParam): Variant;
    function DoVec4(var OP_Param: TOpParam): Variant;
    function DoRandom(var OP_Param: TOpParam): Variant;
    function DoRandomFloat(var OP_Param: TOpParam): Variant;
    function DoMax(var OP_Param: TOpParam): Variant;
    function DoMin(var OP_Param: TOpParam): Variant;
    function DoClamp(var OP_Param: TOpParam): Variant;
    function DoIfThen(var OP_Param: TOpParam): Variant;
    function DoStr(var OP_Param: TOpParam): Variant;
    function DoMultiple(var OP_Param: TOpParam): Variant;
    function DoPrint(var OP_Param: TOpParam): Variant;
    function ToHex(var OP_Param: TOpParam): Variant;
    function Hex8(var OP_Param: TOpParam): Variant;
    function Hex16(var OP_Param: TOpParam): Variant;
    function Hex32(var OP_Param: TOpParam): Variant;
    function Hex64(var OP_Param: TOpParam): Variant;
    function ToBin(var OP_Param: TOpParam): Variant;
    function Bin8(var OP_Param: TOpParam): Variant;
    function Bin16(var OP_Param: TOpParam): Variant;
    function Bin32(var OP_Param: TOpParam): Variant;
    function Bin64(var OP_Param: TOpParam): Variant;
  public
    procedure RegistationSystemAPI(RunTime: TOpCustomRunTime);
  end;

  TOpCustomRunTime = class(TCore_Object)
  protected
    procedure FreeNotifyProc(p: Pointer);
  public
    ProcList: THashList;
    Trigger: POpRTData;
    UserObject: TCore_Object;
    UserData: Pointer;
    constructor Create;
    constructor CustomCreate(maxHashSiz_: Integer); virtual;
    destructor Destroy; override;
    procedure Clean; virtual;
    procedure PrepareRegistation; virtual;
    function GetProcDescription(ProcName: SystemString): SystemString; overload;
    function GetAllProcDescription(): TPascalStringList; overload;
    function GetAllProcDescription(Category: U_String): TPascalStringList; overload;
    function GetAllProcDescription(InclSys_: Boolean; Category: U_String): TPascalStringList; overload;
    function GetProc(const ProcName: SystemString): POpRTData;
    property Proc_[const ProcName: SystemString]: POpRTData read GetProc;
    function RegOpC(ProcName: SystemString; On_P: TOnOp_C): POpRTData; overload;
    function RegOpC(ProcName, ProcDescription: SystemString; On_P: TOnOp_C): POpRTData; overload;
    function RegOpC(ProcName, ProcDescription: SystemString; On_P: TOnOp_C; Mode: TOpRT_Mode): POpRTData; overload;
    function RegOpM(ProcName: SystemString; On_P: TOnOp_M): POpRTData; overload;
    function RegOpM(ProcName, ProcDescription: SystemString; On_P: TOnOp_M): POpRTData; overload;
    function RegOpM(ProcName, ProcDescription: SystemString; On_P: TOnOp_M; Mode: TOpRT_Mode): POpRTData; overload;
    function RegOpP(ProcName: SystemString; On_P: TOnOp_P): POpRTData; overload;
    function RegOpP(ProcName, ProcDescription: SystemString; On_P: TOnOp_P): POpRTData; overload;
    function RegOpP(ProcName, ProcDescription: SystemString; On_P: TOnOp_P; Mode: TOpRT_Mode): POpRTData; overload;
    function RegObjectOpC(ProcName: SystemString; On_P: TOnObjectOp_C): POpRTData; overload;
    function RegObjectOpC(ProcName, ProcDescription: SystemString; On_P: TOnObjectOp_C): POpRTData; overload;
    function RegObjectOpC(ProcName, ProcDescription: SystemString; On_P: TOnObjectOp_C; Mode: TOpRT_Mode): POpRTData; overload;
    function RegObjectOpM(ProcName: SystemString; On_P: TOnObjectOp_M): POpRTData; overload;
    function RegObjectOpM(ProcName, ProcDescription: SystemString; On_P: TOnObjectOp_M): POpRTData; overload;
    function RegObjectOpM(ProcName, ProcDescription: SystemString; On_P: TOnObjectOp_M; Mode: TOpRT_Mode): POpRTData; overload;
    function RegObjectOpP(ProcName: SystemString; On_P: TOnObjectOp_P): POpRTData; overload;
    function RegObjectOpP(ProcName, ProcDescription: SystemString; On_P: TOnObjectOp_P): POpRTData; overload;
    function RegObjectOpP(ProcName, ProcDescription: SystemString; On_P: TOnObjectOp_P; Mode: TOpRT_Mode): POpRTData; overload;
  end;

  opClass = class of TOpCode;

  TOpCode_Pool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TCritical_String_Big_Hash_Pair_Pool<TOpCode>;

  TOpCode_Pool = class(TOpCode_Pool_Decl)
  public
    AutoFree_OpCode: Boolean;
    constructor Create(const AutoFree_OpCode_: Boolean; const HashSize_: Integer);
    procedure DoFree(var Key: SystemString; var Value: TOpCode); override;
  end;

  TOpCode = class(TCore_Object)
  private type
    POpData__ = ^TOpData__;

    TOpData__ = record
      Op: TOpCode;
      Value: Variant;
      ValueType: TOpValueType;
    end;

    TOpData_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<POpData__>;
  protected
    FParam: TOpData_List;
    FAutoFreeLink: Boolean;
    function DoExecute(opRT: TOpCustomRunTime): Variant; virtual;
    function GetParam(index: Integer): POpData__;
    procedure EvaluateParam(opRT: TOpCustomRunTime); overload;
    procedure EvaluateParam(printLog: Boolean; opRT: TOpCustomRunTime); overload;
  public
    Owner: TOpCode;
    ParsedInfo: SystemString;
    ParsedLineNo: Integer;
    constructor Create(FreeLink_: Boolean);
    destructor Destroy; override;
    procedure SaveToStream(stream: TCore_Stream);
    class function LoadFromStream(stream: TCore_Stream; out LoadedOp: TOpCode): Boolean;
    function AddValue(v: Variant): Integer; overload;
    function AddValueT(v: Variant; VT: TOpValueType): Integer; overload;
    function AddLink(Obj: TOpCode): Integer;
    function Clone: TOpCode;
    property Param[index: Integer]: POpData__ read GetParam; default;
    function Count: Integer;
    function Execute: Variant; overload;
    function Execute(opRT: TOpCustomRunTime): Variant; overload;
    function OwnerRoot: TOpCode;
    property AutoFreeLink: Boolean read FAutoFreeLink write FAutoFreeLink;
  end;

  op_Value = class sealed(TOpCode)
  private
    // a
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Proc = class sealed(TOpCode)
  private
    // proc(a,b,c...)
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Add_Prefix = class sealed(TOpCode)
  private
    // +proc
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Sub_Prefix = class sealed(TOpCode)
  private
    // -proc
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Add = class sealed(TOpCode)
  private
    // a + b + n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Sub = class sealed(TOpCode)
  private
    // a - b - n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Mul = class sealed(TOpCode)
  private
    // a * b * n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Div = class sealed(TOpCode)
  private
    // a / b / n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_IntDiv = class sealed(TOpCode)
  private
    // a div b div n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Pow = class sealed(TOpCode)
  private
    // a pow b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Mod = class sealed(TOpCode)
  private
    // a mod b mod n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Or = class sealed(TOpCode)
  private
    // a or b or n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_And = class sealed(TOpCode)
  private
    // a and b and n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Xor = class sealed(TOpCode)
  private
    // a xor b xor n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Shl = class sealed(TOpCode)
  private
    // a shl b shl n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Shr = class sealed(TOpCode)
  private
    // a shr b shr n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Equal = class sealed(TOpCode)
  private
    // a = b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_LessThan = class sealed(TOpCode)
  private
    // a < b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_EqualOrLessThan = class sealed(TOpCode)
  private
    // a <= b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_GreaterThan = class sealed(TOpCode)
  private
    // a > b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_EqualOrGreaterThan = class sealed(TOpCode)
  private
    // a >= b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_NotEqual = class sealed(TOpCode)
  private
    // a <> b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Symbol_Sub = class sealed(TOpCode)
  private
    // -a
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Symbol_Add = class sealed(TOpCode)
  private
    // +a
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

function LoadOpFromStream(stream: TCore_Stream; out LoadedOp: TOpCode): Boolean;

var
  OpSystemAPI: TOpSystemAPI;
  SystemOpRunTime: TOpCustomRunTime;

implementation

uses Z.Geometry2D, Z.Geometry3D, Z.DFE;

type
  opRegData = record
    opClass: opClass;
    OpName: TPascalString;
    hash: Cardinal;
  end;

  POpRegData = ^opRegData;
  TOpList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<POpRegData>;

var
  OpList: TOpList;

procedure TOpRTData.Init;
begin
  Name := '';
  Description := '';
  Category := '';
  OnOp_C := nil;
  OnOp_M := nil;
  OnOp_P := nil;
  OnObjectOp_C := nil;
  OnObjectOp_M := nil;
  OnObjectOp_P := nil;
  Mode := rtmDirect;
end;

procedure TOpRT_Sync_Bridge.Do_Sync_Run;
var
  i: Integer;
  tmp_Param: TOpParam;
begin
  if OC.Count > 1 then
    begin
      // copy param
      SetLength(tmp_Param, OC.Count - 1);
      for i := 1 to OC.Count - 1 do
          tmp_Param[i - 1] := OC.Param[i]^.Value;
    end
  else
      SetLength(tmp_Param, 0);

  opRT.Trigger := OD;
  R_ := NULL;
  try
    if Assigned(OD^.OnOp_C) then
        R_ := OD^.OnOp_C(tmp_Param);
    if Assigned(OD^.OnOp_M) then
        R_ := OD^.OnOp_M(tmp_Param);
    if Assigned(OD^.OnOp_P) then
        R_ := OD^.OnOp_P(tmp_Param);
    if Assigned(OD^.OnObjectOp_C) then
        R_ := OD^.OnObjectOp_C(opRT, tmp_Param);
    if Assigned(OD^.OnObjectOp_M) then
        R_ := OD^.OnObjectOp_M(opRT, tmp_Param);
    if Assigned(OD^.OnObjectOp_P) then
        R_ := OD^.OnObjectOp_P(opRT, tmp_Param);
  except
  end;
  SetLength(tmp_Param, 0);
end;

function GetRegistedOp(Name_P: PPascalString): POpRegData;
var
  i: Integer;
  p: POpRegData;
  hash: Cardinal;
begin
  Result := nil;
  hash := FastHashPPascalString(Name_P);
  for i := 0 to OpList.Count - 1 do
    begin
      p := OpList[i];
      if (p^.hash = hash) and Name_P^.Same(@p^.OpName) then
          Exit(p);
    end;
end;

procedure RegisterOp(c: opClass);
var
  n: TPascalString;
  p: POpRegData;
begin
  n := c.ClassName;
  if GetRegistedOp(@n) <> nil then
      RaiseInfo('repeat op ' + c.ClassName);
  new(p);
  p^.opClass := c;
  p^.OpName := p^.opClass.ClassName;
  p^.hash := FastHashPPascalString(@p^.OpName);
  OpList.Add(p);
end;

procedure FreeOpRegClass;
var
  i: Integer;
  p: POpRegData;
begin
  for i := 0 to OpList.Count - 1 do
    begin
      p := OpList[i];
      Dispose(p);
    end;
  DisposeObject(OpList);
end;

function LoadOpFromStream(stream: TCore_Stream; out LoadedOp: TOpCode): Boolean;

  function LoadFromDataFrame_(D_: TDFE): TOpCode;
  var
    Name_: TPascalString;
    RegPtr: POpRegData;
    i, cnt: Integer;
    NeedNewOp: Boolean;
    newDataEng: TDFE;
    v: Variant;
    VT: TOpValueType;
  begin
    Name_ := D_.Reader.ReadString;
    RegPtr := GetRegistedOp(@Name_);
    if RegPtr <> nil then
      begin
        Result := RegPtr^.opClass.Create(True);
        Result.ParsedInfo := D_.Reader.ReadString;
        Result.ParsedLineNo := D_.Reader.ReadInteger;
        cnt := D_.Reader.ReadInteger;
        for i := 0 to cnt - 1 do
          begin
            NeedNewOp := D_.Reader.ReadBool;

            if NeedNewOp then
              begin
                // create new TOpCode
                newDataEng := TDFE.Create;
                D_.Reader.ReadDataFrame(newDataEng);
                Result.AddLink(LoadFromDataFrame_(newDataEng));
                DisposeObject(newDataEng);
              end
            else
              begin
                v := D_.Reader.ReadVariant;
                VT := TOpValueType(D_.Reader.ReadInteger);
                Result.AddValueT(v, VT);
              end;
          end;
      end
    else
        raise Exception.Create('load OpCode failed');
  end;

var
  DataEng: TDFE;
  DataEdition: Integer;
begin
  Result := False;
  DataEng := TDFE.Create;
  try
    DataEng.DecodeFrom(stream, True);
    DataEdition := DataEng.Reader.ReadInteger;
    if DataEdition = 1 then
      begin
        LoadedOp := LoadFromDataFrame_(DataEng);
        Result := True;
      end
    else
        LoadedOp := nil;
  except
  end;
  DisposeObject(DataEng);
end;

function TOpSystemAPI.DoInt(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Int(v);
end;

function TOpSystemAPI.DoFrac(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Frac(v);
end;

function TOpSystemAPI.DoExp(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Exp(v);
end;

function TOpSystemAPI.DoCos(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Cos(v);
end;

function TOpSystemAPI.DoSin(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Sin(v);
end;

function TOpSystemAPI.DoLn(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := ln(v);
end;

function TOpSystemAPI.DoArcTan(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := ArcTan(v);
end;

function TOpSystemAPI.DoSqrt(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Sqrt(v);
end;

function TOpSystemAPI.DoSqr(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Sqr(v);
end;

function TOpSystemAPI.DoTan(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Tan(v);
end;

function TOpSystemAPI.DoRound(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Round(Double(v));
end;

function TOpSystemAPI.DoTrunc(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Trunc(Double(v));
end;

function TOpSystemAPI.DoDeg(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := NormalizeDegAngle(TGeoFloat(v));
end;

function TOpSystemAPI.DoPower(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  if length(OP_Param) = 2 then
      Result := Power(OP_Param[0], OP_Param[1])
  else
      Result := 0;
end;

function TOpSystemAPI.DoSingle(var OP_Param: TOpParam): Variant;
begin
  Result := Single(OP_Param[0]);
end;

function TOpSystemAPI.DoDouble(var OP_Param: TOpParam): Variant;
begin
  Result := Double(OP_Param[0]);
end;

function TOpSystemAPI.DoExtended(var OP_Param: TOpParam): Variant;
begin
  Result := Extended(OP_Param[0]);
end;

function TOpSystemAPI.DoByte(var OP_Param: TOpParam): Variant;
begin
  Result := Byte(OP_Param[0]);
end;

function TOpSystemAPI.DoWord(var OP_Param: TOpParam): Variant;
begin
  Result := Word(OP_Param[0]);
end;

function TOpSystemAPI.DoCardinal(var OP_Param: TOpParam): Variant;
begin
  Result := Cardinal(OP_Param[0]);
end;

function TOpSystemAPI.DoUInt64(var OP_Param: TOpParam): Variant;
begin
  Result := UInt64(OP_Param[0]);
end;

function TOpSystemAPI.DoShortInt(var OP_Param: TOpParam): Variant;
begin
  Result := ShortInt(OP_Param[0]);
end;

function TOpSystemAPI.DoSmallInt(var OP_Param: TOpParam): Variant;
begin
  Result := SmallInt(OP_Param[0]);
end;

function TOpSystemAPI.DoInteger(var OP_Param: TOpParam): Variant;
begin
  Result := Integer(OP_Param[0]);
end;

function TOpSystemAPI.DoInt64(var OP_Param: TOpParam): Variant;
begin
  Result := Int64(OP_Param[0]);
end;

function TOpSystemAPI.DoROL8(var OP_Param: TOpParam): Variant;
begin
  Result := ROL8(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoROL16(var OP_Param: TOpParam): Variant;
begin
  Result := ROL16(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoROL32(var OP_Param: TOpParam): Variant;
begin
  Result := ROL32(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoROL64(var OP_Param: TOpParam): Variant;
begin
  Result := ROL64(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoROR8(var OP_Param: TOpParam): Variant;
begin
  Result := ROR8(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoROR16(var OP_Param: TOpParam): Variant;
begin
  Result := ROR16(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoROR32(var OP_Param: TOpParam): Variant;
begin
  Result := ROR32(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoROR64(var OP_Param: TOpParam): Variant;
begin
  Result := ROR64(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoEndian16(var OP_Param: TOpParam): Variant;
begin
  Result := Endian(SmallInt(OP_Param[0]));
end;

function TOpSystemAPI.DoEndian32(var OP_Param: TOpParam): Variant;
begin
  Result := Endian(Integer(OP_Param[0]));
end;

function TOpSystemAPI.DoEndian64(var OP_Param: TOpParam): Variant;
begin
  Result := Endian(Int64(OP_Param[0]));
end;

function TOpSystemAPI.DoEndianU16(var OP_Param: TOpParam): Variant;
begin
  Result := Endian(Word(OP_Param[0]));
end;

function TOpSystemAPI.DoEndianU32(var OP_Param: TOpParam): Variant;
begin
  Result := Endian(Cardinal(OP_Param[0]));
end;

function TOpSystemAPI.DoEndianU64(var OP_Param: TOpParam): Variant;
begin
  Result := Endian(UInt64(OP_Param[0]));
end;

function TOpSystemAPI.DoSAR16(var OP_Param: TOpParam): Variant;
begin
  Result := SAR16(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoSAR32(var OP_Param: TOpParam): Variant;
begin
  Result := SAR32(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoSAR64(var OP_Param: TOpParam): Variant;
begin
  Result := SAR64(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoNot(var OP_Param: TOpParam): Variant;
var
  VT: TVarType;
begin
  VT := TVarData(OP_Param[0]).VType;
  case VT of
    varSmallInt, varWord: Result := not Word(OP_Param[0]);
    varInteger, varLongWord: Result := not Cardinal(OP_Param[0]);
    varShortInt, varByte: Result := not Byte(OP_Param[0]);
    varInt64, varUInt64: Result := not UInt64(OP_Param[0]);
    varBoolean: Result := not Boolean(OP_Param[0]);
    else Result := OP_Param[0];
  end;
end;

function TOpSystemAPI.DoPI(var OP_Param: TOpParam): Variant;
begin
  Result := PI;
end;

function TOpSystemAPI.DoBool(var OP_Param: TOpParam): Variant;
  function v2b(const v: Variant): Boolean;
  var
    n: TPascalString;
  begin
    if VarIsStr(v) then
      begin
        n := VarToStr(v);
        n := n.DeleteChar(#32#9);
        if n.Same('True') or n.Same('Yes') or n.Same('1') then
            Result := True
        else
            Result := False;
      end
    else if VarIsOrdinal(v) then
        Result := Boolean(v)
    else if VarIsFloat(v) then
        Result := Boolean(Round(Double(v)))
    else
        Result := Boolean(v);
  end;

var
  n: Boolean;
  i: Integer;
begin
  n := True;
  for i := low(OP_Param) to high(OP_Param) do
      n := n and v2b(OP_Param[i]);
  Result := n;
end;

function TOpSystemAPI.DoTrue(var OP_Param: TOpParam): Variant;
begin
  Result := True;
end;

function TOpSystemAPI.DoFalse(var OP_Param: TOpParam): Variant;
begin
  Result := False;
end;

function TOpSystemAPI.DoRColor(var OP_Param: TOpParam): Variant;
var
  buff: array [0 .. 3] of SystemString;
  i: Integer;
begin
  for i := 0 to 2 do
      buff[i] := '0.0';
  buff[3] := '1.0';

  for i := Low(OP_Param) to high(OP_Param) do
      buff[i] := VarToStr(OP_Param[i]);

  Result := Format('RColor(%s,%s,%s,%s)', [buff[0], buff[1], buff[2], buff[3]]);
end;

function TOpSystemAPI.DoVec2(var OP_Param: TOpParam): Variant;
var
  buff: array [0 .. 1] of SystemString;
  i: Integer;
begin
  for i := Low(buff) to high(buff) do
      buff[i] := '0.0';

  for i := Low(OP_Param) to high(OP_Param) do
      buff[i] := VarToStr(OP_Param[i]);

  Result := Format('Vec2(%s,%s)', [buff[0], buff[1]]);
end;

function TOpSystemAPI.DoVec3(var OP_Param: TOpParam): Variant;
var
  buff: array [0 .. 2] of SystemString;
  i: Integer;
begin
  for i := Low(buff) to high(buff) do
      buff[i] := '0.0';

  for i := Low(OP_Param) to high(OP_Param) do
      buff[i] := VarToStr(OP_Param[i]);

  Result := Format('Vec3(%s,%s,%s)', [buff[0], buff[1], buff[2]]);
end;

function TOpSystemAPI.DoVec4(var OP_Param: TOpParam): Variant;
var
  buff: array [0 .. 3] of SystemString;
  i: Integer;
begin
  for i := Low(buff) to high(buff) do
      buff[i] := '0.0';

  for i := Low(OP_Param) to high(OP_Param) do
      buff[i] := VarToStr(OP_Param[i]);

  Result := Format('Vec4(%s,%s,%s,%s)', [buff[0], buff[1], buff[2], buff[3]]);
end;

function TOpSystemAPI.DoRandom(var OP_Param: TOpParam): Variant;
var
  v: Integer;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];

  if v <> 0 then
      Result := MT19937Rand32(v)
  else
      Result := MT19937Rand32(MaxInt);
end;

function TOpSystemAPI.DoRandomFloat(var OP_Param: TOpParam): Variant;
begin
  Result := MT19937RandF;
end;

function TOpSystemAPI.DoMax(var OP_Param: TOpParam): Variant;
var
  i: Integer;
begin
  if length(OP_Param) = 0 then
    begin
      Result := NULL;
      Exit;
    end;
  Result := OP_Param[0];
  for i := 1 to length(OP_Param) - 1 do
    if OP_Param[i] > Result then
        Result := OP_Param[i];
end;

function TOpSystemAPI.DoMin(var OP_Param: TOpParam): Variant;
var
  i: Integer;
begin
  if length(OP_Param) = 0 then
    begin
      Result := NULL;
      Exit;
    end;
  Result := OP_Param[0];
  for i := 1 to length(OP_Param) - 1 do
    if OP_Param[i] < Result then
        Result := OP_Param[i];
end;

function TOpSystemAPI.DoClamp(var OP_Param: TOpParam): Variant;
var
  minv_, maxv_: Variant;
begin
  if length(OP_Param) <> 3 then
    begin
      if length(OP_Param) > 0 then
          Result := OP_Param[0]
      else
          Result := NULL;
      Exit;
    end;

  if OP_Param[1] > OP_Param[2] then
    begin
      minv_ := OP_Param[2];
      maxv_ := OP_Param[1];
    end
  else
    begin
      minv_ := OP_Param[1];
      maxv_ := OP_Param[2];
    end;

  if OP_Param[0] < minv_ then
      Result := minv_
  else if OP_Param[0] > maxv_ then
      Result := maxv_
  else
      Result := OP_Param[0];
end;

function TOpSystemAPI.DoIfThen(var OP_Param: TOpParam): Variant;
begin
  if length(OP_Param) <> 3 then
    begin
      Result := NULL;
      Exit;
    end;
  if Boolean(OP_Param[0]) = True then
      Result := OP_Param[1]
  else
      Result := OP_Param[2];
end;

function TOpSystemAPI.DoStr(var OP_Param: TOpParam): Variant;
var
  n: TPascalString;
  i: Integer;
begin
  n := '';
  for i := low(OP_Param) to high(OP_Param) do
      n.Append(VarToStr(OP_Param[i]));
  Result := n;
end;

function TOpSystemAPI.DoMultiple(var OP_Param: TOpParam): Variant;
var
  i: Integer;
begin
  if length(OP_Param) >= 2 then
    begin
      Result := True;
      for i := 1 to length(OP_Param) - 1 do
          Result := Result and umlMultipleMatch(VarToStr(OP_Param[0]), VarToStr(OP_Param[i]));
    end
  else
      Result := True;
end;

function TOpSystemAPI.DoPrint(var OP_Param: TOpParam): Variant;
var
  n: TPascalString;
  i: Integer;
begin
  n := '';
  for i := low(OP_Param) to high(OP_Param) do
    begin
      if n.L > 0 then
          n.Append(',');
      n.Append(VarToStr(OP_Param[i]));
    end;
  DoStatus(n);
  Result := n.Text;
end;

function TOpSystemAPI.ToHex(var OP_Param: TOpParam): Variant;
var
  n: TPascalString;
  i: Integer;
  VT: TVarType;
begin
  n := '';
  for i := low(OP_Param) to high(OP_Param) do
    begin
      if n.L > 0 then
          n.Append(',');
      VT := TVarData(OP_Param[i]).VType;
      case VT of
        varSmallInt, varWord: n.Append(IntToHex(Word(OP_Param[i]), 4));
        varInteger, varLongWord: n.Append(IntToHex(Cardinal(OP_Param[i]), 8));
        varShortInt, varByte: n.Append(IntToHex(Byte(OP_Param[i]), 2));
        varInt64, varUInt64: n.Append(IntToHex(UInt64(OP_Param[i]), 16));
        else n.Append(VarToStr(OP_Param[i]));
      end;
    end;
  Result := n.Text;
end;

function TOpSystemAPI.Hex8(var OP_Param: TOpParam): Variant;
begin
  Result := Byte(StrToInt('$' + VarToStr(OP_Param[0])));
end;

function TOpSystemAPI.Hex16(var OP_Param: TOpParam): Variant;
begin
  Result := Word(StrToInt('$' + VarToStr(OP_Param[0])));
end;

function TOpSystemAPI.Hex32(var OP_Param: TOpParam): Variant;
begin
  Result := StrToInt('$' + VarToStr(OP_Param[0]));
end;

function TOpSystemAPI.Hex64(var OP_Param: TOpParam): Variant;
begin
  Result := StrToUInt64('$' + VarToStr(OP_Param[0]));
end;

function TOpSystemAPI.ToBin(var OP_Param: TOpParam): Variant;
var
  n: TPascalString;
  i: Integer;
  VT: TVarType;
begin
  n := '';
  for i := low(OP_Param) to high(OP_Param) do
    begin
      if n.L > 0 then
          n.Append(',');
      VT := TVarData(OP_Param[i]).VType;
      case VT of
        varSmallInt, varWord: n.Append(umlUInt16ToBin(OP_Param[i]));
        varInteger, varLongWord: n.Append(umlUInt32ToBin(OP_Param[i]));
        varShortInt, varByte: n.Append(umlUInt8ToBin(OP_Param[i]));
        varInt64, varUInt64: n.Append(umlUInt64ToBin(OP_Param[i]));
        else n.Append(VarToStr(OP_Param[i]));
      end;
    end;
  Result := n.Text;
end;

function TOpSystemAPI.Bin8(var OP_Param: TOpParam): Variant;
begin
  Result := umlBinToUInt8(VarToStr(OP_Param[0]));
end;

function TOpSystemAPI.Bin16(var OP_Param: TOpParam): Variant;
begin
  Result := umlBinToUInt16(VarToStr(OP_Param[0]));
end;

function TOpSystemAPI.Bin32(var OP_Param: TOpParam): Variant;
begin
  Result := umlBinToUInt32(VarToStr(OP_Param[0]));
end;

function TOpSystemAPI.Bin64(var OP_Param: TOpParam): Variant;
begin
  Result := umlBinToUInt64(VarToStr(OP_Param[0]));
end;

procedure TOpSystemAPI.RegistationSystemAPI(RunTime: TOpCustomRunTime);
begin
  RunTime.RegOpM('Int', 'Int(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoInt)^.Category := 'Base Math';
  RunTime.RegOpM('Frac', 'Frac(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoFrac)^.Category := 'Base Math';
  RunTime.RegOpM('Exp', 'Exp(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoExp)^.Category := 'Base Math';
  RunTime.RegOpM('Cos', 'Cos(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoCos)^.Category := 'Base Math';
  RunTime.RegOpM('Sin', 'Sin(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoSin)^.Category := 'Base Math';
  RunTime.RegOpM('Ln', 'Ln(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoLn)^.Category := 'Base Math';
  RunTime.RegOpM('ArcTan', 'ArcTan(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoArcTan)^.Category := 'Base Math';
  RunTime.RegOpM('Sqrt', 'Sqrt(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoSqrt)^.Category := 'Base Math';
  RunTime.RegOpM('Sqr', 'Sqr(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoSqr)^.Category := 'Base Math';
  RunTime.RegOpM('Tan', 'Tan(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoTan)^.Category := 'Base Math';
  RunTime.RegOpM('Round', 'Round(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoRound)^.Category := 'Base Math';
  RunTime.RegOpM('Trunc', 'Trunc(0..n): math function', {$IFDEF FPC}@{$ENDIF FPC}DoTrunc)^.Category := 'Base Math';
  RunTime.RegOpM('Deg', 'Deg(0..n): NormalizeDegAngle function', {$IFDEF FPC}@{$ENDIF FPC}DoDeg)^.Category := 'Base Math';
  RunTime.RegOpM('Power', 'Power(float,float): Power: Raise base to any power function', {$IFDEF FPC}@{$ENDIF FPC}DoPower)^.Category := 'Base Math';
  RunTime.RegOpM('Pow', 'Pow(float,float): Power: Raise base to any power function', {$IFDEF FPC}@{$ENDIF FPC}DoPower)^.Category := 'Base Math';
  RunTime.RegOpM('Single', 'Single(value): math function', {$IFDEF FPC}@{$ENDIF FPC}DoSingle)^.Category := 'Base Math';
  RunTime.RegOpM('Double', 'Double(value): math function', {$IFDEF FPC}@{$ENDIF FPC}DoDouble)^.Category := 'Base Math';
  RunTime.RegOpM('Float', 'Float(value): math function', {$IFDEF FPC}@{$ENDIF FPC}DoDouble)^.Category := 'Base Math';
  RunTime.RegOpM('Extended', 'Extended(value): math function', {$IFDEF FPC}@{$ENDIF FPC}DoExtended)^.Category := 'Base Math';
  RunTime.RegOpM('Byte', 'Byte(value): math function', {$IFDEF FPC}@{$ENDIF FPC}DoByte)^.Category := 'Base Math';
  RunTime.RegOpM('Word', 'Word(value): math function', {$IFDEF FPC}@{$ENDIF FPC}DoWord)^.Category := 'Base Math';
  RunTime.RegOpM('Cardinal', 'Cardinal(value): math function', {$IFDEF FPC}@{$ENDIF FPC}DoCardinal)^.Category := 'Base Math';
  RunTime.RegOpM('UInt64', 'UInt64(value): math function', {$IFDEF FPC}@{$ENDIF FPC}DoUInt64)^.Category := 'Base Math';
  RunTime.RegOpM('ShortInt', 'ShortInt(value): math function', {$IFDEF FPC}@{$ENDIF FPC}DoShortInt)^.Category := 'Base Math';
  RunTime.RegOpM('SmallInt', 'SmallInt(value): math function', {$IFDEF FPC}@{$ENDIF FPC}DoSmallInt)^.Category := 'Base Math';
  RunTime.RegOpM('Integer', 'Integer(value): math function', {$IFDEF FPC}@{$ENDIF FPC}DoInteger)^.Category := 'Base Math';
  RunTime.RegOpM('Int64', 'Int64(value): math function', {$IFDEF FPC}@{$ENDIF FPC}DoInt64)^.Category := 'Base Math';
  RunTime.RegOpM('ROL8', 'ROL8(byte,Shift): math function', {$IFDEF FPC}@{$ENDIF FPC}DoROL8)^.Category := 'Base Math';
  RunTime.RegOpM('ROL16', 'ROL16(word,Shift): math function', {$IFDEF FPC}@{$ENDIF FPC}DoROL16)^.Category := 'Base Math';
  RunTime.RegOpM('ROL32', 'ROL32(cardinal,Shift): math function', {$IFDEF FPC}@{$ENDIF FPC}DoROL32)^.Category := 'Base Math';
  RunTime.RegOpM('ROL64', 'ROL64(uint64,Shift): math function', {$IFDEF FPC}@{$ENDIF FPC}DoROL64)^.Category := 'Base Math';
  RunTime.RegOpM('ROR8', 'ROR8(byte,Shift): math function', {$IFDEF FPC}@{$ENDIF FPC}DoROR8)^.Category := 'Base Math';
  RunTime.RegOpM('ROR16', 'ROR16(word,Shift): math function', {$IFDEF FPC}@{$ENDIF FPC}DoROR16)^.Category := 'Base Math';
  RunTime.RegOpM('ROR32', 'ROR32(cardinal,Shift): math function', {$IFDEF FPC}@{$ENDIF FPC}DoROR32)^.Category := 'Base Math';
  RunTime.RegOpM('ROR64', 'ROR64(uint64,Shift): math function', {$IFDEF FPC}@{$ENDIF FPC}DoROR64)^.Category := 'Base Math';
  RunTime.RegOpM('Endian16', 'Endian16(smallint): math function', {$IFDEF FPC}@{$ENDIF FPC}DoEndian16)^.Category := 'Base Math';
  RunTime.RegOpM('Endian32', 'Endian32(integer): math function', {$IFDEF FPC}@{$ENDIF FPC}DoEndian32)^.Category := 'Base Math';
  RunTime.RegOpM('Endian64', 'Endian64(int64): math function', {$IFDEF FPC}@{$ENDIF FPC}DoEndian64)^.Category := 'Base Math';
  RunTime.RegOpM('EndianU16', 'EndianU16(word): math function', {$IFDEF FPC}@{$ENDIF FPC}DoEndianU16)^.Category := 'Base Math';
  RunTime.RegOpM('EndianU32', 'EndianU32(cardinal): math function', {$IFDEF FPC}@{$ENDIF FPC}DoEndianU32)^.Category := 'Base Math';
  RunTime.RegOpM('EndianU64', 'EndianU64(uint64): math function', {$IFDEF FPC}@{$ENDIF FPC}DoEndianU64)^.Category := 'Base Math';
  RunTime.RegOpM('SAR16', 'SAR16(word,Shift): math function', {$IFDEF FPC}@{$ENDIF FPC}DoSAR16)^.Category := 'Base Math';
  RunTime.RegOpM('SAR32', 'SAR32(cardinal,Shift): math function', {$IFDEF FPC}@{$ENDIF FPC}DoSAR32)^.Category := 'Base Math';
  RunTime.RegOpM('SAR64', 'SAR64(uint64,Shift): math function', {$IFDEF FPC}@{$ENDIF FPC}DoSAR64)^.Category := 'Base Math';
  RunTime.RegOpM('Not', 'Not(Ordinal): not math function', {$IFDEF FPC}@{$ENDIF FPC}DoNot)^.Category := 'Base Math';
  RunTime.RegOpM('~', '~(Ordinal): not math function', {$IFDEF FPC}@{$ENDIF FPC}DoNot)^.Category := 'Base Math';
  RunTime.RegOpM('PI', 'PI(): return PI', {$IFDEF FPC}@{$ENDIF FPC}DoPI)^.Category := 'Base Math';
  RunTime.RegOpM('Bool', 'Bool(n..n): convert any variant as bool', {$IFDEF FPC}@{$ENDIF FPC}DoBool)^.Category := 'Base Math';
  RunTime.RegOpM('Boolean', 'Boolean(n..n): convert any variant as bool', {$IFDEF FPC}@{$ENDIF FPC}DoBool)^.Category := 'Base Math';
  RunTime.RegOpM('True', 'True(): return true', {$IFDEF FPC}@{$ENDIF FPC}DoTrue)^.Category := 'Base Math';
  RunTime.RegOpM('False', 'False(): return false', {$IFDEF FPC}@{$ENDIF FPC}DoFalse)^.Category := 'Base Math';
  RunTime.RegOpM('RColor', 'RColor(R,G,B,A): return RColor string', {$IFDEF FPC}@{$ENDIF FPC}DoRColor)^.Category := 'Base Math';
  RunTime.RegOpM('Vec2', 'Vec2(X,Y): return Vec2 string', {$IFDEF FPC}@{$ENDIF FPC}DoVec2)^.Category := 'Base Math';
  RunTime.RegOpM('Vec3', 'Vec3(X,Y,Z): return Vec3 string', {$IFDEF FPC}@{$ENDIF FPC}DoVec3)^.Category := 'Base Math';
  RunTime.RegOpM('Vec4', 'Vec4(X,Y,Z,W): return Vec4 string', {$IFDEF FPC}@{$ENDIF FPC}DoVec4)^.Category := 'Base Math';
  RunTime.RegOpM('Random', 'Random(0..n): return number', {$IFDEF FPC}@{$ENDIF FPC}DoRandom)^.Category := 'Base Math';
  RunTime.RegOpM('RandomFloat', 'RandomFloat(): return float', {$IFDEF FPC}@{$ENDIF FPC}DoRandomFloat)^.Category := 'Base Math';
  RunTime.RegOpM('RandomF', 'RandomF(): return float', {$IFDEF FPC}@{$ENDIF FPC}DoRandomFloat)^.Category := 'Base Math';
  RunTime.RegOpM('Max', 'Max(0..n): return max value', {$IFDEF FPC}@{$ENDIF FPC}DoMax)^.Category := 'Base Math';
  RunTime.RegOpM('Min', 'Min(0..n): return min value', {$IFDEF FPC}@{$ENDIF FPC}DoMin)^.Category := 'Base Math';
  RunTime.RegOpM('Clamp', 'Clamp(value, min, max): return clamp value', {$IFDEF FPC}@{$ENDIF FPC}DoClamp)^.Category := 'Base Math';
  RunTime.RegOpM('IfThen', 'IfThen(bool, if true then of value, if false then of value): return if value', {$IFDEF FPC}@{$ENDIF FPC}DoIfThen)^.Category := 'Base Math';
  RunTime.RegOpM('if_', 'if_(bool, if true then of value, if false then of value): return if value', {$IFDEF FPC}@{$ENDIF FPC}DoIfThen)^.Category := 'Base Math';
  RunTime.RegOpM('Str', 'Str(n..n): convert any variant as string', {$IFDEF FPC}@{$ENDIF FPC}DoStr)^.Category := 'Base String';
  RunTime.RegOpM('String', 'String(n..n): convert any variant as string', {$IFDEF FPC}@{$ENDIF FPC}DoStr)^.Category := 'Base String';
  RunTime.RegOpM('Text', 'Text(n..n): convert any variant as string', {$IFDEF FPC}@{$ENDIF FPC}DoStr)^.Category := 'Base String';
  RunTime.RegOpM('MultipleMatch', 'MultipleMatch(multile exp, n..n): return bool', {$IFDEF FPC}@{$ENDIF FPC}DoMultiple)^.Category := 'Base String';
  RunTime.RegOpM('Multiple', 'MultipleMatch(multile exp, n..n): return bool', {$IFDEF FPC}@{$ENDIF FPC}DoMultiple)^.Category := 'Base String';
  RunTime.RegOpM('Print', 'Print(multile exp, n..n): return text', {$IFDEF FPC}@{$ENDIF FPC}DoPrint)^.Category := 'Base Print';
  RunTime.RegOpM('DoStatus', 'DoStatus(multile exp, n..n): return text', {$IFDEF FPC}@{$ENDIF FPC}DoPrint)^.Category := 'Base Print';
  RunTime.RegOpM('Status', 'Status(multile exp, n..n): return text', {$IFDEF FPC}@{$ENDIF FPC}DoPrint)^.Category := 'Base Print';
  RunTime.RegOpM('ToHex', 'ToHex(multile exp, n..n): return hex text', {$IFDEF FPC}@{$ENDIF FPC}ToHex)^.Category := 'Hex';
  RunTime.RegOpM('Hex8', 'Hex8(text): return byte', {$IFDEF FPC}@{$ENDIF FPC}Hex8)^.Category := 'Hex';
  RunTime.RegOpM('Hex16', 'Hex16(text): return byte', {$IFDEF FPC}@{$ENDIF FPC}Hex16)^.Category := 'Hex';
  RunTime.RegOpM('Hex32', 'Hex32(text): return byte', {$IFDEF FPC}@{$ENDIF FPC}Hex32)^.Category := 'Hex';
  RunTime.RegOpM('Hex64', 'Hex64(text): return byte', {$IFDEF FPC}@{$ENDIF FPC}Hex64)^.Category := 'Hex';
  RunTime.RegOpM('ToBin', 'ToBin(multile exp, n..n): return Binary text', {$IFDEF FPC}@{$ENDIF FPC}ToBin)^.Category := 'Binary';
  RunTime.RegOpM('Bin8', 'Bin8(text): return byte', {$IFDEF FPC}@{$ENDIF FPC}Bin8)^.Category := 'Binary';
  RunTime.RegOpM('Bin16', 'Bin16(text): return byte', {$IFDEF FPC}@{$ENDIF FPC}Bin16)^.Category := 'Binary';
  RunTime.RegOpM('Bin32', 'Bin32(text): return byte', {$IFDEF FPC}@{$ENDIF FPC}Bin32)^.Category := 'Binary';
  RunTime.RegOpM('Bin64', 'Bin64(text): return byte', {$IFDEF FPC}@{$ENDIF FPC}Bin64)^.Category := 'Binary';
end;

procedure TOpCustomRunTime.FreeNotifyProc(p: Pointer);
begin
  POpRTData(p)^.Init;
  Dispose(POpRTData(p));
end;

constructor TOpCustomRunTime.Create;
begin
  CustomCreate($FF);
end;

constructor TOpCustomRunTime.CustomCreate(maxHashSiz_: Integer);
begin
  inherited Create;
  ProcList := THashList.CustomCreate(maxHashSiz_);
  ProcList.AutoFreeData := True;
  ProcList.AccessOptimization := False;
  ProcList.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}FreeNotifyProc;
  Trigger := nil;
  UserObject := nil;
  UserData := nil;
  PrepareRegistation;
end;

destructor TOpCustomRunTime.Destroy;
begin
  DisposeObject(ProcList);
  inherited Destroy;
end;

procedure TOpCustomRunTime.Clean;
begin
  ProcList.Clear;
end;

procedure TOpCustomRunTime.PrepareRegistation;
begin
end;

function TOpCustomRunTime.GetProcDescription(ProcName: SystemString): SystemString;
var
  p: POpRTData;
begin
  Result := ProcName + '(): no Descripion';
  p := ProcList[ProcName];
  if p <> nil then
    begin
      if p^.Description <> '' then
          Result := p^.Description;
    end
  else if self <> SystemOpRunTime then
      Result := SystemOpRunTime.GetProcDescription(ProcName);
end;

function TOpCustomRunTime.GetAllProcDescription(): TPascalStringList;
begin
  Result := GetAllProcDescription('*');
end;

function TOpCustomRunTime.GetAllProcDescription(Category: U_String): TPascalStringList;
begin
  Result := GetAllProcDescription(True, Category);
end;

function TOpCustomRunTime.GetAllProcDescription(InclSys_: Boolean; Category: U_String): TPascalStringList;
var
  arry: THashDataArray;
  hl: THashObjectList;
  ns, tmp: TPascalStringList;
  i, j: Integer;
  p: POpRTData;
  n: TPascalString;
begin
  if InclSys_ and (self <> SystemOpRunTime) then
      Result := SystemOpRunTime.GetAllProcDescription(Category)
  else
      Result := TPascalStringList.Create;

  arry := ProcList.GetHashDataArray();

  hl := THashObjectList.CustomCreate(True, $FF);
  for i := Low(arry) to High(arry) do
    begin
      p := arry[i]^.Data;
      if not hl.Exists(p^.Category) then
          hl.FastAdd(p^.Category, TPascalStringList.Create);
      tmp := hl[p^.Category] as TPascalStringList;

      if p^.Description <> '' then
        begin
          if umlReplaceSum(' ' + p^.Description, ' ' + p^.Name + '(', False, True, 0, 0, nil) > 0 then
              n := p^.Description
          else
              n := p^.Name + '(): ' + p^.Description;
        end
      else
          n := p^.Name + '(): no Descripion';

      tmp.Add(n);
    end;
  SetLength(arry, 0);

  ns := TPascalStringList.Create;
  hl.GetListData(ns);
  for i := 0 to ns.Count - 1 do
    if umlMultipleMatch(Category, ns[i]) then
      begin
        Result.Add(PFormat('%s:', [ns[i].Text]));
        tmp := ns.Objects[i] as TPascalStringList;
        for j := 0 to tmp.Count - 1 do
            Result.Add('  ' + tmp[j].Text);
        Result.Add('');
      end;
  n := '';
  DisposeObject(ns);
  DisposeObject(hl);
end;

function TOpCustomRunTime.GetProc(const ProcName: SystemString): POpRTData;
begin
  Result := ProcList[ProcName];
end;

function TOpCustomRunTime.RegOpC(ProcName: SystemString; On_P: TOnOp_C): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnOp_C := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegOpC(ProcName, ProcDescription: SystemString; On_P: TOnOp_C): POpRTData;
begin
  Result := RegOpC(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.RegOpC(ProcName, ProcDescription: SystemString; On_P: TOnOp_C; Mode: TOpRT_Mode): POpRTData;
begin
  Result := RegOpC(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode
end;

function TOpCustomRunTime.RegOpM(ProcName: SystemString; On_P: TOnOp_M): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnOp_M := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegOpM(ProcName, ProcDescription: SystemString; On_P: TOnOp_M): POpRTData;
begin
  Result := RegOpM(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.RegOpM(ProcName, ProcDescription: SystemString; On_P: TOnOp_M; Mode: TOpRT_Mode): POpRTData;
begin
  Result := RegOpM(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode
end;

function TOpCustomRunTime.RegOpP(ProcName: SystemString; On_P: TOnOp_P): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnOp_P := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegOpP(ProcName, ProcDescription: SystemString; On_P: TOnOp_P): POpRTData;
begin
  Result := RegOpP(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.RegOpP(ProcName, ProcDescription: SystemString; On_P: TOnOp_P; Mode: TOpRT_Mode): POpRTData;
begin
  Result := RegOpP(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode
end;

function TOpCustomRunTime.RegObjectOpC(ProcName: SystemString; On_P: TOnObjectOp_C): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnObjectOp_C := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegObjectOpC(ProcName, ProcDescription: SystemString; On_P: TOnObjectOp_C): POpRTData;
begin
  Result := RegObjectOpC(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.RegObjectOpC(ProcName, ProcDescription: SystemString; On_P: TOnObjectOp_C; Mode: TOpRT_Mode): POpRTData;
begin
  Result := RegObjectOpC(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode;
end;

function TOpCustomRunTime.RegObjectOpM(ProcName: SystemString; On_P: TOnObjectOp_M): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnObjectOp_M := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegObjectOpM(ProcName, ProcDescription: SystemString; On_P: TOnObjectOp_M): POpRTData;
begin
  Result := RegObjectOpM(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.RegObjectOpM(ProcName, ProcDescription: SystemString; On_P: TOnObjectOp_M; Mode: TOpRT_Mode): POpRTData;
begin
  Result := RegObjectOpM(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode;
end;

function TOpCustomRunTime.RegObjectOpP(ProcName: SystemString; On_P: TOnObjectOp_P): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnObjectOp_P := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegObjectOpP(ProcName, ProcDescription: SystemString; On_P: TOnObjectOp_P): POpRTData;
begin
  Result := RegObjectOpP(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.RegObjectOpP(ProcName, ProcDescription: SystemString; On_P: TOnObjectOp_P; Mode: TOpRT_Mode): POpRTData;
begin
  Result := RegObjectOpP(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode;
end;

constructor TOpCode_Pool.Create(const AutoFree_OpCode_: Boolean; const HashSize_: Integer);
begin
  inherited Create(HashSize_, nil);
  AutoFree_OpCode := AutoFree_OpCode_;
end;

procedure TOpCode_Pool.DoFree(var Key: SystemString; var Value: TOpCode);
begin
  if AutoFree_OpCode then
      DisposeObjectAndNil(Value)
  else
      Value := nil;
  inherited DoFree(Key, Value);
end;

function TOpCode.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := NULL;
end;

function TOpCode.GetParam(index: Integer): POpData__;
begin
  Result := FParam[index];
end;

procedure TOpCode.EvaluateParam(opRT: TOpCustomRunTime);
begin
  EvaluateParam(False, opRT);
end;

procedure TOpCode.EvaluateParam(printLog: Boolean; opRT: TOpCustomRunTime);
var
  i: Integer;
  p: POpData__;
begin
  for i := 0 to FParam.Count - 1 do
    begin
      p := FParam[i];
      if p^.Op <> nil then
        begin
          try
              p^.Op.EvaluateParam(printLog, opRT);
          except
          end;

          try
            p^.Value := p^.Op.DoExecute(opRT);

            if printLog then
                DoStatus('%s value:%s', [ClassName, VarToStr(p^.Value)]);
          except
          end;
        end;
    end;
end;

constructor TOpCode.Create(FreeLink_: Boolean);
begin
  inherited Create;
  Owner := nil;
  FParam := TOpData_List.Create;
  FAutoFreeLink := FreeLink_;
  ParsedInfo := '';
  ParsedLineNo := 0;
end;

destructor TOpCode.Destroy;
var
  i: Integer;
  p: POpData__;
begin
  if FParam <> nil then
    begin
      for i := 0 to FParam.Count - 1 do
        begin
          p := FParam[i];
          if (FAutoFreeLink) and (p^.Op <> nil) then
              DisposeObject(p^.Op);
          p^.Value := NULL;
          Dispose(p);
        end;
      FParam.Clear;
      DisposeObject(FParam);
    end;
  inherited Destroy;
end;

procedure TOpCode.SaveToStream(stream: TCore_Stream);
  procedure SaveToDataFrame(Op: TOpCode; D_: TDFE);
  var
    i: Integer;
    p: POpData__;
    newDataEng: TDFE;
  begin
    D_.WriteString(Op.ClassName);
    D_.WriteString(Op.ParsedInfo);
    D_.WriteInteger(Op.ParsedLineNo);
    D_.WriteInteger(Op.Count);
    for i := 0 to Op.Count - 1 do
      begin
        p := Op[i];
        if p^.Op <> nil then
          begin
            D_.WriteBool(True);
            newDataEng := TDFE.Create;
            SaveToDataFrame(p^.Op, newDataEng);
            D_.WriteDataFrame(newDataEng);
            DisposeObject(newDataEng);
          end
        else
          begin
            D_.WriteBool(False);
            D_.WriteVariant(p^.Value);
            D_.WriteInteger(Integer(p^.ValueType));
          end;
      end;
  end;

var
  DataEng: TDFE;
begin
  DataEng := TDFE.Create;
  DataEng.WriteInteger(1);
  SaveToDataFrame(self, DataEng);
  DataEng.FastEncodeTo(stream);
  DisposeObject(DataEng);
end;

class function TOpCode.LoadFromStream(stream: TCore_Stream; out LoadedOp: TOpCode): Boolean;
begin
  Result := LoadOpFromStream(stream, LoadedOp);
end;

function TOpCode.AddValue(v: Variant): Integer;
var
  p: POpData__;
begin
  new(p);
  p^.Op := nil;

  p^.Value := v;

  case VarType(v) of
    varSmallInt: p^.ValueType := ovtSmallInt;
    varInteger: p^.ValueType := ovtInt;
    varSingle: p^.ValueType := ovtSingle;
    varDouble: p^.ValueType := ovtDouble;
    varCurrency: p^.ValueType := ovtCurrency;
    varBoolean: p^.ValueType := ovtBool;
    varShortInt: p^.ValueType := ovtShortInt;
    varByte: p^.ValueType := ovtByte;
    varWord: p^.ValueType := ovtWord;
    varLongWord: p^.ValueType := ovtUInt;
    varInt64: p^.ValueType := ovtInt64;
    varUInt64: p^.ValueType := ovtUInt64;
    else
      begin
        if VarIsStr(v) then
            p^.ValueType := ovtString
        else
            p^.ValueType := ovtUnknow;
      end;
  end;

  Result := FParam.Add(p);
end;

function TOpCode.AddValueT(v: Variant; VT: TOpValueType): Integer;
var
  p: POpData__;
begin
  new(p);
  p^.Op := nil;
  p^.Value := v;
  p^.ValueType := VT;
  Result := FParam.Add(p);
end;

function TOpCode.AddLink(Obj: TOpCode): Integer;
var
  p: POpData__;
begin
  new(p);

  if Obj.Owner <> nil then
      p^.Op := Obj.Clone
  else
      p^.Op := Obj;

  p^.Op.Owner := self;

  p^.Value := NULL;
  p^.ValueType := ovtUnknow;
  Result := FParam.Add(p);
end;

function TOpCode.Clone: TOpCode;
var
  i: Integer;
  p: POpData__;
begin
  Result := opClass(self.ClassType).Create(True);
  Result.ParsedInfo := self.ParsedInfo;
  Result.ParsedLineNo := self.ParsedLineNo;

  for i := 0 to FParam.Count - 1 do
    begin
      p := FParam[i];
      if p^.Op <> nil then
          Result.AddLink(p^.Op.Clone)
      else
          Result.AddValueT(p^.Value, p^.ValueType);
    end;
end;

function TOpCode.Count: Integer;
begin
  Result := FParam.Count;
end;

function TOpCode.Execute: Variant;
begin
  Result := Execute(SystemOpRunTime);
end;

function TOpCode.Execute(opRT: TOpCustomRunTime): Variant;
begin
  try
      EvaluateParam(opRT);
  except
    Result := NULL;
    Exit;
  end;

  try
      Result := DoExecute(opRT);
  except
      Result := NULL;
  end;
end;

function TOpCode.OwnerRoot: TOpCode;
begin
  if Owner = nil then
      Result := self
  else
      Result := Owner.OwnerRoot;
end;

{ op_Value }

function op_Value.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value;
end;

{ op_Proc }

function op_Proc.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  p: POpRTData;
  i: Integer;
  tmp_Param: TOpParam;
  bridge_: TOpRT_Sync_Bridge;
begin
  Result := NULL;
  if (opRT = nil) then
      opRT := SystemOpRunTime;

  p := opRT.ProcList[VarToStr(Param[0]^.Value)];
  if p = nil then
    begin
      if opRT = SystemOpRunTime then
          Exit;
      p := SystemOpRunTime.ProcList[VarToStr(Param[0]^.Value)];
      if p = nil then
          Exit;
    end;

  if p^.Mode = rtmDirect then
    begin
      if Count > 1 then
        begin
          SetLength(tmp_Param, Count - 1);
          for i := 1 to Count - 1 do
              tmp_Param[i - 1] := Param[i]^.Value;
        end
      else
          SetLength(tmp_Param, 0);

      opRT.Trigger := p;
      if Assigned(p^.OnOp_C) then
          Result := p^.OnOp_C(tmp_Param);
      if Assigned(p^.OnOp_M) then
          Result := p^.OnOp_M(tmp_Param);
      if Assigned(p^.OnOp_P) then
          Result := p^.OnOp_P(tmp_Param);
      if Assigned(p^.OnObjectOp_C) then
          Result := p^.OnObjectOp_C(opRT, tmp_Param);
      if Assigned(p^.OnObjectOp_M) then
          Result := p^.OnObjectOp_M(opRT, tmp_Param);
      if Assigned(p^.OnObjectOp_P) then
          Result := p^.OnObjectOp_P(opRT, tmp_Param);

      SetLength(tmp_Param, 0);
    end
  else
    begin
      bridge_ := TOpRT_Sync_Bridge.Create;
      bridge_.opRT := opRT;
      bridge_.OC := self;
      bridge_.OD := p;
      bridge_.R_ := Result;
      if p^.Mode = rtmPost then
          TCompute.Sync_Wait_PostM1({$IFDEF FPC}@{$ENDIF FPC}bridge_.Do_Sync_Run)
      else
          TCompute.SyncM({$IFDEF FPC}@{$ENDIF FPC}bridge_.Do_Sync_Run);
      Result := bridge_.R_;
      DisposeObject(bridge_);
    end;
end;

{ op_Add_Prefix }

function op_Add_Prefix.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result + Param[i]^.Value;
  Result := - - Result;
end;

{ op_Sub_Prefix }

function op_Sub_Prefix.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result + Param[i]^.Value;
  Result := -Result;
end;

{ op_Add }

function op_Add.DoExecute(opRT: TOpCustomRunTime): Variant;

  function Fast_VarIsStr(var v: Variant): Boolean;
  var
    p: pVarData;
  begin
    // optimized
    p := @TVarData(v);
    while p^.VType = varByRef or varVariant do
        p := pVarData(p^.VPointer);
    Result := (p^.VType = varOleStr) or (p^.VType = varString) or (p^.VType = varUString);
  end;

var
  i: Integer;
  n1, n2: TPascalString;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;

  if Fast_VarIsStr(Result) then
    begin
      // optimized
      n1 := VarToStr(Result);
      for i := 1 to Count - 1 do
        begin
          try
              n1.Append(VarToStr(Param[i]^.Value));
          except
          end;
        end;
      Result := n1.Text;
    end
  else
    begin
      for i := 1 to Count - 1 do
        begin
          try
            if Fast_VarIsStr(Result) then
              begin
                // SystemString combine
                n1 := VarToStr(Result);
                if not umlIsNumber(n1) then
                  begin
                    Result := n1 + VarToStr(Param[i]^.Value);
                    Continue;
                  end
              end;

            if Fast_VarIsStr(Param[i]^.Value) then
              begin
                // SystemString combine
                n2 := VarToStr(Param[i]^.Value);
                if not umlIsNumber(n2) then
                  begin
                    Result := VarToStr(Result) + n2;
                    Continue;
                  end
              end;

            // logic compute
            Result := Result + Param[i]^.Value;
          except
          end;
        end;
    end;
end;

{ op_Sub }

function op_Sub.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result - Param[i]^.Value;
end;

{ op_Mul }

function op_Mul.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result * Param[i]^.Value;
end;

{ op_Div }

function op_Div.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result / Param[i]^.Value;
end;

{ op_IntDiv }

function op_IntDiv.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result div Param[i]^.Value;
end;

{ op_Pow }
function op_Pow.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Power(Result, Param[i]^.Value);
end;

{ op_Mod }

function op_Mod.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result mod Param[i]^.Value;
end;

{ op_Or }

function op_Or.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result or Param[i]^.Value;
end;

{ op_And }

function op_And.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result and Param[i]^.Value;
end;

{ op_Xor }

function op_Xor.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result xor Param[i]^.Value;
end;

{ op_shl }

function op_Shl.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := UInt64(Result) shl UInt64(Param[i]^.Value);
end;

{ op_shr }

function op_Shr.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := UInt64(Result) shr UInt64(Param[i]^.Value);
end;

{ op_Equal }

function op_Equal.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value = Param[1]^.Value;
end;

{ op_LessThan }

function op_LessThan.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value < Param[1]^.Value;
end;

{ op_EqualOrLessThan }

function op_EqualOrLessThan.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value <= Param[1]^.Value;
end;

{ op_GreaterThan }

function op_GreaterThan.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value > Param[1]^.Value;
end;

{ op_EqualOrGreaterThan }

function op_EqualOrGreaterThan.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value >= Param[1]^.Value;
end;

{ op_NotEqual }

function op_NotEqual.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value <> Param[1]^.Value;
end;

{ op_Symbol_Sub }

function op_Symbol_Sub.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := -Param[0]^.Value;
end;

{ op_Symbol_Add }

function op_Symbol_Add.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value;
end;

initialization

OpSystemAPI := TOpSystemAPI.Create;
SystemOpRunTime := TOpCustomRunTime.Create;
OpSystemAPI.RegistationSystemAPI(SystemOpRunTime);
OleVariantInt64AsDouble := True;
OpList := TOpList.Create;

RegisterOp(op_Value);
RegisterOp(op_Proc);
RegisterOp(op_Add_Prefix);
RegisterOp(op_Sub_Prefix);
RegisterOp(op_Add);
RegisterOp(op_Sub);
RegisterOp(op_Mul);
RegisterOp(op_Div);
RegisterOp(op_IntDiv);
RegisterOp(op_Pow);
RegisterOp(op_Mod);
RegisterOp(op_Or);
RegisterOp(op_And);
RegisterOp(op_Xor);
RegisterOp(op_Shl);
RegisterOp(op_Shr);
RegisterOp(op_Equal);
RegisterOp(op_LessThan);
RegisterOp(op_EqualOrLessThan);
RegisterOp(op_GreaterThan);
RegisterOp(op_EqualOrGreaterThan);
RegisterOp(op_NotEqual);
RegisterOp(op_Symbol_Sub);
RegisterOp(op_Symbol_Add);

finalization

DisposeObject(OpSystemAPI);
DisposeObject(SystemOpRunTime);
FreeOpRegClass;

end.
