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
{ * Int128 implementation https://github.com/eStreamSoftware/delphi-int128     * }
{ * fixed the compilation issue with FPC and provided the test function        * }
{ * by qq600585                                                                * }
{ ****************************************************************************** }
unit Z.Int128;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses SysUtils, Variants,
  Z.Core, Z.PascalStrings;

type
  TUInt128_Buffer = array [0 .. 15] of UInt8;
  PUInt128_Buffer = ^TUInt128_Buffer;
  PUInt128 = ^UInt128;

  UInt128 = packed record
  private
    class procedure DivModU128(a, b: UInt128; out DivResult: UInt128; out Remainder: UInt128); static;
    class procedure SetBit128(var a: UInt128; numBit: integer); static;
  public
    function ToVariant: Variant;
    function ToString: TPascalString;
    function ToLString: TPascalString;
    function ToUInt64: UInt64;
    function ToInt64: Int64;
    function ToUInt32: UInt32;
    function ToInt32: Int32;
    function ToUInt16: UInt16;
    function ToInt16: Int16;
    function ToUInt8: UInt8;
    function ToInt8: Int8;
    class operator Implicit(a: UInt8): UInt128;
    class operator Implicit(a: UInt16): UInt128;
    class operator Implicit(a: UInt32): UInt128;
    class operator Implicit(a: UInt64): UInt128;
    class operator Implicit(a: Single): UInt128;
    class operator Implicit(a: Double): UInt128;
    class operator Implicit(a: TPascalString): UInt128;
    class operator Implicit(a: SystemString): UInt128;
    class operator Implicit(a: UInt128): TPascalString;
    class operator Implicit(a: UInt128): SystemString;
    class operator Explicit(a: UInt128): UInt64;
    class operator Explicit(a: UInt128): UInt32;
    class operator Explicit(a: UInt128): Int32;
    class operator LogicalNot(a: UInt128): UInt128;
    class operator Negative(a: UInt128): UInt128;
    class operator Equal(a, b: UInt128): Boolean;
    class operator NotEqual(a, b: UInt128): Boolean;
    class operator GreaterThanOrEqual(a, b: UInt128): Boolean;
    class operator GreaterThan(a, b: UInt128): Boolean;
    class operator LessThanOrEqual(a, b: UInt128): Boolean;
    class operator LessThan(a, b: UInt128): Boolean;
    class operator RightShift(a: UInt128; b: Int64): UInt128;
    class operator RightShift(a, b: UInt128): UInt128;
    class operator LeftShift(a: UInt128; b: Int64): UInt128;
    class operator LeftShift(a, b: UInt128): UInt128;
    class operator Add(a, b: UInt128): UInt128;
    class operator Subtract(a, b: UInt128): UInt128;
    class operator Multiply(a, b: UInt128): UInt128;
    class operator IntDivide(a, b: UInt128): UInt128;
    class operator Modulus(a, b: UInt128): UInt128;
    class operator Modulus(a: UInt128; b: Int64): UInt128;
    class operator BitWiseAnd(a, b: UInt128): UInt128;
    class operator BitWiseOr(a, b: UInt128): UInt128;
    class operator BitWiseXor(a, b: UInt128): UInt128;
    class operator Inc(a: UInt128): UInt128;
    class operator Dec(a: UInt128): UInt128;
  public
    case Byte of
      0: (b: TUInt128_Buffer);
      1: (w: array [0 .. 7] of UInt16);
      2: (w0, w1, w2, w3, w4, w5, w6, w7: UInt16);
      3: (c0, c1, c2, c3: UInt32);
      4: (c: array [0 .. 3] of UInt32);
      5: (dc0, dc1: UInt64);
      6: (dc: array [0 .. 1] of UInt64);
  end;

  TUInt128_Array = array of UInt128;
  TAtomUInt128 = TAtomVar<UInt128>;

  TInt128_Buffer = TUInt128_Buffer;
  PInt128_Buffer = PUInt128_Buffer;
  PInt128 = ^Int128;

  Int128 = packed record
  private
    class procedure DivMod128(const Value1: Int128; const Value2: Int128; out DivResult: Int128; out Remainder: Int128); static;
    class procedure Inc128(var a: Int128); static;
    class procedure SetBit128(var a: Int128; numBit: integer); static;
    class function StrToInt128(a: TPascalString): Int128; static; inline;
    function Invert: Int128;
  public
    function ToVariant: Variant;
    function ToString: TPascalString;
    function ToLString: TPascalString;
    function ToUInt64: UInt64;
    function ToInt64: Int64;
    function ToUInt32: UInt32;
    function ToInt32: Int32;
    function ToUInt16: UInt16;
    function ToInt16: Int16;
    function ToUInt8: UInt8;
    function ToInt8: Int8;
    class operator Add(a, b: Int128): Int128;
    class operator Equal(a, b: Int128): Boolean;
    class operator GreaterThan(a, b: Int128): Boolean;
    class operator GreaterThanOrEqual(a, b: Int128): Boolean;
    class operator Implicit(a: Int8): Int128;
    class operator Implicit(a: UInt8): Int128;
    class operator Implicit(a: Int16): Int128;
    class operator Implicit(a: UInt16): Int128;
    class operator Implicit(a: Int32): Int128;
    class operator Implicit(a: UInt32): Int128;
    class operator Implicit(a: Int64): Int128;
    class operator Implicit(a: UInt64): Int128;
    class operator Implicit(a: Single): Int128;
    class operator Implicit(a: Double): Int128;
    class operator Implicit(a: Int128): TPascalString;
    class operator Implicit(a: Int128): SystemString;
    class operator Implicit(a: TPascalString): Int128;
    class operator Implicit(a: SystemString): Int128;
    class operator Implicit(a: UInt128): Int128;
    class operator Implicit(a: Int128): UInt128;
    class operator Explicit(a: UInt128): Int128;
    class operator Explicit(a: Int128): UInt128;
    class operator Explicit(a: Int128): Int32;
    class operator LeftShift(a: Int128; Shift: Int64): Int128;
    class operator LeftShift(a, Shift: Int128): Int128;
    class operator LessThan(a, b: Int128): Boolean;
    class operator LessThanOrEqual(a, b: Int128): Boolean;
    class operator LogicalNot(a: Int128): Int128;
    class operator Multiply(a, b: Int128): Int128;
    class operator NotEqual(a, b: Int128): Boolean;
    class operator RightShift(a: Int128; Shift: Int64): Int128;
    class operator RightShift(a, Shift: Int128): Int128;
    class operator Subtract(a, b: Int128): Int128;
    class operator IntDivide(a, b: Int128): Int128;
    class operator Modulus(a, b: Int128): Int128;
    class operator BitWiseOr(a, b: Int128): Int128;
    class operator BitWiseXor(a, b: Int128): Int128;
    class operator BitWiseAnd(a, b: Int128): Int128;
    class operator Negative(a: Int128): Int128;
    class operator Inc(a: Int128): Int128;
    class operator Dec(a: Int128): Int128;
  public
    case Byte of
      0: (b: TInt128_Buffer);
      1: (w: array [0 .. 7] of UInt16);
      2: (w0, w1, w2, w3, w4, w5, w6, w7: UInt16);
      3: (c0, c1, c2, c3: UInt32);
      4: (c: array [0 .. 3] of UInt32);
      5: (dc0, dc1: UInt64);
      6: (dc: array [0 .. 1] of UInt64);
  end;

  TInt128_Array = array of Int128;
  TAtomInt128 = TAtomVar<Int128>;

  TUInt128_VarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    V_Data: PUInt128;
    Reserved4: Cardinal;
{$IFDEF CPU64}
    Reserved5: Cardinal;
{$ENDIF CPU64}
  end;

  TUInt128_VariantType = class(TCustomVariantType)
  public
    procedure Clear(var v: TVarData); override;
    function IsClear(const v: TVarData): Boolean; override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
    procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Op: TVarOp); override;
    function CompareOp(const Left, Right: TVarData; const Operator: TVarOp): Boolean; override;
  end;

  TInt128_VarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    V_Data: PInt128;
    Reserved4: Cardinal;
{$IFDEF CPU64}
    Reserved5: Cardinal;
{$ENDIF CPU64}
  end;

  TInt128_VariantType = class(TCustomVariantType)
  public
    procedure Clear(var v: TVarData); override;
    function IsClear(const v: TVarData): Boolean; override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
    procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Op: TVarOp); override;
    function CompareOp(const Left, Right: TVarData; const Operator: TVarOp): Boolean; override;
  end;

  TCritical_Int128_Helper = class helper for TCritical
  public
    function Get(var x: Int128): Int128; overload;
    function Get(var x: UInt128): UInt128; overload;
    procedure Inc_(var x: Int128); overload;
    procedure Inc_(var x: Int128; const v: Int128); overload;
    procedure Dec_(var x: Int128); overload;
    procedure Dec_(var x: Int128; const v: Int128); overload;
    procedure Inc_(var x: UInt128); overload;
    procedure Inc_(var x: UInt128; const v: UInt128); overload;
    procedure Dec_(var x: UInt128); overload;
    procedure Dec_(var x: UInt128; const v: UInt128); overload;
  end;

const
  varType_UInt128 = $187;
  Ten: UInt128 = (dc0: $A; dc1: 0);
  Neg1: Int128 = (dc0: 0; dc1: $8000000000000000);

var
  UInt128_VariantType: TUInt128_VariantType;
  Int128_VariantType: TInt128_VariantType;

implementation

class procedure UInt128.DivModU128(a, b: UInt128; out DivResult: UInt128; out Remainder: UInt128);
var
  Shift: integer;
begin
  if b = 0 then
      RaiseInfo('Division by zero');

  if a = 0 then
    begin
      DivResult := 0;
      Remainder := 0;
      Exit;
    end;

  if a < b then
    begin
      DivResult := 0;
      Remainder := a;
      Exit;
    end;

  DivResult := 0;
  Remainder := a;
  Shift := 0;

  while (b < Remainder) and (b.c3 and $80000000 = 0) do
    begin
      if Shift = 124 then
          break;
      b := b shl 1;
      Inc(Shift);
      if b > Remainder then
        begin
          b := b shr 1;
          Dec(Shift);
          break;
        end;
    end;

  while True do
    begin
      if b <= Remainder then
        begin
          Remainder := Remainder - b;
          SetBit128(DivResult, Shift);
        end;

      if Shift > 0 then
        begin
          b := b shr 1;
          Dec(Shift);
        end
      else
          break;

    end;
end;

class procedure UInt128.SetBit128(var a: UInt128; numBit: integer);
begin
  a.c[numBit shr 5] := a.c[numBit shr 5] or longword(1 shl (numBit and 31));
end;

function UInt128.ToVariant: Variant;
begin
  VarClear(Result);
  TUInt128_VarData(Result).VType := UInt128_VariantType.VarType;
  New(TUInt128_VarData(Result).V_Data);
  TUInt128_VarData(Result).V_Data^ := Self;
end;

function UInt128.ToString: TPascalString;
begin
  Result := Self;
end;

function UInt128.ToLString: TPascalString;
begin
  Result := 'L' + ToString;
end;

function UInt128.ToUInt64: UInt64;
begin
  Result := dc0;
end;

function UInt128.ToInt64: Int64;
begin
  Result := dc0;
end;

function UInt128.ToUInt32: UInt32;
begin
  Result := c0;
end;

function UInt128.ToInt32: Int32;
begin
  Result := c0;
end;

function UInt128.ToUInt16: UInt16;
begin
  Result := w0;
end;

function UInt128.ToInt16: Int16;
begin
  Result := w0;
end;

function UInt128.ToUInt8: UInt8;
begin
  Result := b[0];
end;

function UInt128.ToInt8: Int8;
begin
  Result := b[0];
end;

class operator UInt128.Implicit(a: UInt8): UInt128;
begin
  Result.b[0] := a;
  FillPtr(@Result.b[1], SizeOf(Result) - SizeOf(Result.b[0]), 0);
end;

class operator UInt128.Implicit(a: UInt16): UInt128;
begin
  Result.w[0] := a;
  FillPtr(@Result.w[1], SizeOf(Result) - SizeOf(Result.w[0]), 0);
end;

class operator UInt128.Implicit(a: UInt32): UInt128;
begin
  Result.c[0] := a;
  FillPtr(@Result.c[1], SizeOf(Result) - SizeOf(Result.c[0]), 0);
end;

class operator UInt128.Implicit(a: UInt64): UInt128;
begin
  Result.dc0 := a;
  Result.dc1 := 0;
end;

class operator UInt128.Implicit(a: Single): UInt128;
begin
  Result.dc0 := round(abs(a));
  Result.dc1 := 0;
end;

class operator UInt128.Implicit(a: Double): UInt128;
begin
  Result.dc0 := round(abs(a));
  Result.dc1 := 0;
end;

class operator UInt128.Implicit(a: TPascalString): UInt128;
var
  i: integer;
begin
  Result := 0;

  if CharIn(a.First, ['L', 'l']) then
      a.DeleteFirst;

  for i := 1 to a.L do
    begin
      if CharIn(a[i], c0to9) then
        begin
          Result := Result * Ten;
          Result := Result + UInt32(Ord(a[i]) - Ord('0'));
        end
      else
          RaiseInfo(a + ' is not a valid Int128 a.');
    end;
end;

class operator UInt128.Implicit(a: SystemString): UInt128;
begin
  Result := TPascalString(a);
end;

class operator UInt128.Implicit(a: UInt128): TPascalString;
var
  digit: UInt128;
begin
  Result := '';

  while a <> 0 do
    begin
      DivModU128(a, Ten, a, digit);
      Result := Chr(Ord('0') + digit.c0) + Result;
    end;

  if Result = '' then
      Result := '0';
end;

class operator UInt128.Implicit(a: UInt128): SystemString;
begin
  Result := TPascalString(a).Text;
end;

class operator UInt128.Explicit(a: UInt128): UInt64;
begin
  Result := a.dc0;
end;

class operator UInt128.Explicit(a: UInt128): UInt32;
begin
  Result := a.c0;
end;

class operator UInt128.Explicit(a: UInt128): Int32;
begin
  Result := a.c0;
end;

class operator UInt128.LogicalNot(a: UInt128): UInt128;
begin
  Result.dc0 := not a.dc0;
  Result.dc1 := not a.dc1;
end;

class operator UInt128.Negative(a: UInt128): UInt128;
begin
  RaiseInfo('Integer overflow');
end;

class operator UInt128.Equal(a: UInt128; b: UInt128): Boolean;
begin
  Result := True;
  if a.dc0 <> b.dc0 then
      Result := false;
  if a.dc1 <> b.dc1 then
      Result := false;
end;

class operator UInt128.NotEqual(a: UInt128; b: UInt128): Boolean;
begin
  Result := (a.dc0 <> b.dc0) or (a.dc1 <> b.dc1);
end;

class operator UInt128.GreaterThanOrEqual(a, b: UInt128): Boolean;
begin
  Result := (a = b) or (a > b);
end;

class operator UInt128.GreaterThan(a, b: UInt128): Boolean;
begin
  Result := false;
  if a.dc1 > b.dc1 then
      Result := True;
  if a.dc1 = b.dc1 then
    if a.dc0 > b.dc0 then
        Result := True;
end;

class operator UInt128.LessThanOrEqual(a, b: UInt128): Boolean;
begin
  Result := (a = b) or (a < b);
end;

class operator UInt128.LessThan(a, b: UInt128): Boolean;
begin
  Result := false;
  if a.dc1 < b.dc1 then
      Result := True
  else if a.dc1 = b.dc1 then
    if a.dc0 < b.dc0 then
        Result := True;
end;

class operator UInt128.RightShift(a: UInt128; b: Int64): UInt128;
begin
  if b >= 128 then
      Result := a shr (b mod 128) // follow compiler
  else if b >= 64 then
    begin
      Result.dc1 := 0;
      Result.dc0 := a.dc1 shr (b - 64);
    end
  else if b > 0 then
    begin
      Result.dc0 := (a.dc0 shr b) or (a.dc1 shl (64 - b));
      Result.dc1 := a.dc1 shr b;
    end
  else if b = 0 then
      Result := a
  else if b < 0 then
      Result := a shr (128 - (abs(b) mod 128)); // follow compiler
end;

class operator UInt128.RightShift(a: UInt128; b: UInt128): UInt128;
begin
  Result := a shr UInt32(b mod 128);
end;

class operator UInt128.LeftShift(a: UInt128; b: Int64): UInt128;
begin
  if b >= 128 then
      Result := a shl (b mod 128) // follow compiler
  else if b >= 64 then
    begin
      Result.dc0 := 0;
      Result.dc1 := a.dc0 shl (b - 64);
    end
  else if b > 0 then
    begin
      Result.dc1 := (a.dc1 shl b) or (a.dc0 shr (64 - b));
      Result.dc0 := a.dc0 shl b;
    end
  else if b = 0 then
      Result := a
  else if b < 0 then
      Result := a shl (128 - (abs(b) mod 128)); // follow compiler
end;

class operator UInt128.LeftShift(a: UInt128; b: UInt128): UInt128;
begin
  Result := a shl UInt32(b mod 128);
end;

class operator UInt128.Add(a, b: UInt128): UInt128;
  procedure inc3;
  begin
    if Result.c3 = High(Result.c3) then
      begin
        RaiseInfo('Integer overflow');
      end
    else
        Inc(Result.c3);
  end;

  procedure inc2;
  begin
    if Result.c2 = High(Result.c2) then
      begin
        Result.c2 := 0;
        inc3;
      end
    else
        Inc(Result.c2);
  end;

  procedure inc1;
  begin
    if Result.c1 = High(Result.c1) then
      begin
        Result.c1 := 0;
        inc2;
      end
    else
        Inc(Result.c1);
  end;

var
  qw: UInt64;
  c0, c1, c2, c3: Boolean;
begin
  qw := UInt64(a.c0) + UInt64(b.c0);
  Result.c0 := qw and High(Result.c0);
  c0 := (qw shr 32) = 1;
  qw := UInt64(a.c1) + UInt64(b.c1);
  Result.c1 := qw and High(Result.c1);
  c1 := (qw shr 32) = 1;
  qw := UInt64(a.c2) + UInt64(b.c2);
  Result.c2 := qw and High(Result.c2);
  c2 := (qw shr 32) = 1;
  qw := UInt64(a.c3) + UInt64(b.c3);
  Result.c3 := qw and High(Result.c3);
  c3 := (qw shr 32) = 1;
  if c0 then
      inc1;
  if c1 then
      inc2;
  if c2 then
      inc3;
  if c3 then
      RaiseInfo('Integer overflow');
end;

class operator UInt128.Subtract(a, b: UInt128): UInt128;
begin
  if b > a then
      RaiseInfo('Integer overflow')
  else
    begin
      Result.dc0 := a.dc0 - b.dc0;
      Result.dc1 := a.dc1 - b.dc1;

      if Result.dc0 > a.dc0 then
          Dec(Result.dc1);
    end;
end;

class operator UInt128.Multiply(a: UInt128; b: UInt128): UInt128;
var
  qw: UInt64;
  v: UInt128;
  over: Boolean;
begin
  over := false;

  qw := UInt64(a.c0) * UInt64(b.c0);
  Result.c0 := qw and High(Result.c0);
  Result.c1 := qw shr 32;
  Result.c2 := 0;
  Result.c3 := 0;

  qw := UInt64(a.c0) * UInt64(b.c1);
  v.c0 := 0;
  v.c1 := qw and High(v.c1);
  v.c2 := qw shr 32;
  v.c3 := 0;
  Result := Result + v;

  qw := UInt64(a.c1) * UInt64(b.c0);
  v.c0 := 0;
  v.c1 := qw and High(v.c1);
  v.c2 := qw shr 32;
  v.c3 := 0;
  Result := Result + v;

  qw := UInt64(a.c0) * UInt64(b.c2);
  v.c0 := 0;
  v.c1 := 0;
  v.c2 := qw and High(v.c2);
  v.c3 := qw shr 32;
  Result := Result + v;

  qw := UInt64(a.c1) * UInt64(b.c1);
  v.c0 := 0;
  v.c1 := 0;
  v.c2 := qw and High(v.c2);
  v.c3 := qw shr 32;
  Result := Result + v;

  qw := UInt64(a.c2) * UInt64(b.c0);
  v.c0 := 0;
  v.c1 := 0;
  v.c2 := qw and High(v.c2);
  v.c3 := qw shr 32;
  Result := Result + v;

  qw := UInt64(a.c0) * UInt64(b.c3);
  v.c0 := 0;
  v.c1 := 0;
  v.c2 := 0;
  v.c3 := qw and High(v.c3);
  if qw shr 32 <> 0 then
      over := True;
  Result := Result + v;

  qw := UInt64(a.c1) * UInt64(b.c2);
  v.c0 := 0;
  v.c1 := 0;
  v.c2 := 0;
  v.c3 := qw and High(v.c3);
  if qw shr 32 <> 0 then
      over := True;
  Result := Result + v;

  qw := UInt64(a.c2) * UInt64(b.c1);
  v.c0 := 0;
  v.c1 := 0;
  v.c2 := 0;
  v.c3 := qw and High(v.c3);
  if qw shr 32 <> 0 then
      over := True;
  Result := Result + v;

  qw := UInt64(a.c3) * UInt64(b.c0);
  v.c0 := 0;
  v.c1 := 0;
  v.c2 := 0;
  v.c3 := qw and High(v.c3);
  if qw shr 32 <> 0 then
      over := True;
  Result := Result + v;

  if over then
      RaiseInfo('Integer overflow');
  if (Result = 0) and (a <> 0) and (b <> 0) then
      RaiseInfo('Integer overflow');
end;

class operator UInt128.IntDivide(a: UInt128; b: UInt128): UInt128;
var
  temp: UInt128;
begin
  DivModU128(a, b, Result, temp);
end;

class operator UInt128.Modulus(a, b: UInt128): UInt128;
var
  temp: UInt128;
begin
  DivModU128(a, b, temp, Result);
end;

class operator UInt128.Modulus(a: UInt128; b: Int64): UInt128;
var
  temp: UInt128;
begin
  if b < 0 then
      b := -b;
  DivModU128(a, b, temp, Result);
end;

class operator UInt128.BitWiseAnd(a, b: UInt128): UInt128;
begin
  Result.dc0 := a.dc0 and b.dc0;
  Result.dc1 := a.dc1 and b.dc1;
end;

class operator UInt128.BitWiseOr(a, b: UInt128): UInt128;
begin
  Result.dc0 := a.dc0 or b.dc0;
  Result.dc1 := a.dc1 or b.dc1;
end;

class operator UInt128.BitWiseXor(a, b: UInt128): UInt128;
begin
  Result.dc0 := a.dc0 xor b.dc0;
  Result.dc1 := a.dc1 xor b.dc1;
end;

class operator UInt128.Inc(a: UInt128): UInt128;
begin
  Result := a + UInt128(1);
end;

class operator UInt128.Dec(a: UInt128): UInt128;
begin
  Result := a - UInt128(1);
end;

class procedure Int128.DivMod128(const Value1: Int128; const Value2: Int128; out DivResult: Int128; out Remainder: Int128);
var
  curShift: integer;
  sub: Int128;
  neg, bIsNeg1: Boolean;
begin
  if Value2 = 0 then
      RaiseInfo('Division by zero');

  sub := Value2;
  Remainder := Value1;
  DivResult := 0;

  neg := (sub.c3 and $80000000 <> 0) xor (Remainder.c3 and $80000000 <> 0);

  if (sub.c3 and $80000000 <> 0) then
      sub := -sub;

  bIsNeg1 := Remainder = Neg1;

  if bIsNeg1 then
      Remainder := Remainder.Invert
  else if (Remainder.c3 and $80000000 <> 0) then
      Remainder := -Remainder;

  // if divisor = 1
  if sub = 1 then
    begin
      if (Value1 < 0) and neg then
          DivResult := Value1
      else if (Value1 > 0) and neg then
          DivResult := -Value1
      else if Value1 < 0 then
          DivResult := -Value1
      else
          DivResult := Value1;
      Remainder := 0;
      Exit;
    end;

  curShift := 0;
  while (sub.c3 and $80000000 = 0) and (sub < Remainder) do
    begin
      if curShift = 123 then
          break;
      sub := sub shl 1;
      Inc(curShift);
      if (sub > Remainder) then
        begin
          sub := sub shr 1;
          Dec(curShift);
          break;
        end;
    end;

  while True do
    begin
      if sub <= Remainder then
        begin
          Remainder := Remainder - sub;
          SetBit128(DivResult, curShift);
        end;
      if curShift > 0 then
        begin
          sub := sub shr 1;
          Dec(curShift);
        end
      else
          break;
    end;

  if neg then
      DivResult := -DivResult;

  if bIsNeg1 then
      Remainder := Remainder + 1;
end;

class procedure Int128.Inc128(var a: Int128);
begin
  if a.c0 <> High(a.c0) then
      Inc(a.c0)
  else
    begin
      a.c0 := 0;
      if a.c1 <> High(a.c1) then
          Inc(a.c1)
      else
        begin
          a.c1 := 0;
          if a.c2 <> High(a.c2) then
              Inc(a.c2)
          else
            begin
              a.c2 := 0;
              if a.c3 <> High(a.c3) then
                  Inc(a.c3)
              else
                  a.c3 := 0;
            end;
        end;
    end;
end;

class procedure Int128.SetBit128(var a: Int128; numBit: integer);
begin
  a.c[numBit shr 5] := a.c[numBit shr 5] or UInt32(1 shl (numBit and 31));
end;

class function Int128.StrToInt128(a: TPascalString): Int128;
var
  IsNeg: Boolean;
  i: integer;
  iDigit: Int128;
begin
  if a.L = 0 then
      Exit(0);

  if CharIn(a.First, ['L', 'l']) then
      a.DeleteFirst;

  Result := 0;
  IsNeg := a[1] = '-';
  i := 1;
  if IsNeg then
      i := 2;

  for i := i to a.L - 1 do
    begin
      if CharIn(a[i], c0to9) then
          Result := Result * Ten + (Ord(a[i]) - Ord('0'))
      else
          RaiseInfo(a + ' is not a valid Int128 a.');
    end;

  i := a.L;
  if CharIn(a[i], c0to9) then
    begin
      Result := Result * Ten;
      iDigit := Ord(a[i]) - Ord('0');
      if IsNeg then
          Result := -Result - iDigit
      else
          Result := Result + iDigit;
    end
  else
      RaiseInfo(a + ' is not a valid Int128 a.');
end;

function Int128.Invert: Int128;
begin
  Result.dc0 := Self.dc0 xor High(Self.dc0);
  Result.dc1 := Self.dc1 xor High(Self.dc1);
end;

function Int128.ToVariant: Variant;
begin
  VarClear(Result);
  TInt128_VarData(Result).VType := Int128_VariantType.VarType;
  New(TInt128_VarData(Result).V_Data);
  TInt128_VarData(Result).V_Data^ := Self;
end;

function Int128.ToString: TPascalString;
begin
  Result := Self;
end;

function Int128.ToLString: TPascalString;
begin
  Result := 'L' + ToString;
end;

function Int128.ToUInt64: UInt64;
begin
  Result := dc0;
end;

function Int128.ToInt64: Int64;
begin
  Result := dc0;
end;

function Int128.ToUInt32: UInt32;
begin
  Result := c0;
end;

function Int128.ToInt32: Int32;
begin
  Result := c0;
end;

function Int128.ToUInt16: UInt16;
begin
  Result := w0;
end;

function Int128.ToInt16: Int16;
begin
  Result := w0;
end;

function Int128.ToUInt8: UInt8;
begin
  Result := b[0];
end;

function Int128.ToInt8: Int8;
begin
  Result := b[0];
end;

class operator Int128.Add(a, b: Int128): Int128;
  procedure inc3;
  begin
    if Result.c3 = High(Result.c3) then
      begin
        Result.c3 := 0;
      end
    else
        Inc(Result.c3);
  end;

  procedure inc2;
  begin
    if Result.c2 = High(Result.c2) then
      begin
        Result.c2 := 0;
        inc3;
      end
    else
        Inc(Result.c2);
  end;

  procedure inc1;
  begin
    if Result.c1 = High(Result.c1) then
      begin
        Result.c1 := 0;
        inc2;
      end
    else
        Inc(Result.c1);
  end;

var
  qw: UInt64;
  c0, c1, c2: Boolean;
begin
  qw := UInt64(a.c0) + UInt64(b.c0);
  Result.c0 := qw and High(Result.c0);
  c0 := (qw shr 32) = 1;

  qw := UInt64(a.c1) + UInt64(b.c1);
  Result.c1 := qw and High(Result.c1);
  c1 := (qw shr 32) = 1;

  qw := UInt64(a.c2) + UInt64(b.c2);
  Result.c2 := qw and High(Result.c2);
  c2 := (qw shr 32) = 1;

  qw := UInt64(a.c3) + UInt64(b.c3);
  Result.c3 := qw and High(Result.c3);

  if c0 then
      inc1;
  if c1 then
      inc2;
  if c2 then
      inc3;

  if (Result < 0) and (a > 0) and (b > 0) then
      RaiseInfo('Integer overflow');

  if (Result > 0) and (a < 0) and (b < 0) then
      RaiseInfo('Integer overflow');
end;

class operator Int128.Equal(a, b: Int128): Boolean;
begin
  Result := (a.dc0 = b.dc0) and (a.dc1 = b.dc1);
end;

class operator Int128.GreaterThan(a, b: Int128): Boolean;
begin
  Result := false;
  if Int64(a.dc1) > Int64(b.dc1) then
      Result := True
  else if a.dc1 = b.dc1 then
    begin
      if a.dc0 > b.dc0 then
          Result := True;
    end;
end;

class operator Int128.GreaterThanOrEqual(a, b: Int128): Boolean;
begin
  Result := (a = b) or (a > b);
end;

class operator Int128.Implicit(a: Int8): Int128;
var
  Sign: Byte;
begin
  Sign := 0;
  Result.b[0] := UInt8(a);
  if a < 0 then
      Sign := $FF;
  FillPtr(@Result.b[1], SizeOf(Result) - SizeOf(Result.b[0]), Sign);
end;

class operator Int128.Implicit(a: UInt8): Int128;
begin
  Result.b[0] := a;
  FillPtr(@Result.b[1], SizeOf(Result) - SizeOf(Result.b[0]), 0);
end;

class operator Int128.Implicit(a: Int16): Int128;
var
  Sign: Byte;
begin
  Sign := 0;
  Result.w[0] := UInt16(a);
  if a < 0 then
      Sign := $FF;
  FillPtr(@Result.w[1], SizeOf(Result) - SizeOf(Result.w[0]), Sign);
end;

class operator Int128.Implicit(a: UInt16): Int128;
begin
  Result.w[0] := a;
  FillPtr(@Result.w[1], SizeOf(Result) - SizeOf(Result.w[0]), 0);
end;

class operator Int128.Implicit(a: Int32): Int128;
var
  Sign: Byte;
begin
  Sign := 0;
  Result.c[0] := UInt32(a);
  if a < 0 then
      Sign := $FF;
  FillPtr(@Result.c[1], SizeOf(Result) - SizeOf(Result.c[0]), Sign);
end;

class operator Int128.Implicit(a: UInt32): Int128;
begin
  Result.c[0] := a;
  FillPtr(@Result.c[1], SizeOf(Result) - SizeOf(Result.c[0]), 0);
end;

class operator Int128.Implicit(a: Int64): Int128;
var
  Sign: Byte;
begin
  Sign := 0;
  Result.dc[0] := UInt64(a);
  if a < 0 then
      Sign := $FF;
  FillPtr(@Result.dc[1], SizeOf(Result) - SizeOf(Result.dc[0]), Sign);
end;

class operator Int128.Implicit(a: UInt64): Int128;
begin
  Result.dc0 := a;
  Result.dc1 := 0;
end;

class operator Int128.Implicit(a: Single): Int128;
var
  Sign: Byte;
begin
  Sign := 0;
  Result.dc[0] := round(abs(a));
  if a < 0 then
      Sign := $FF;
  FillPtr(@Result.dc[1], SizeOf(Result) - SizeOf(Result.dc[0]), Sign);
end;

class operator Int128.Implicit(a: Double): Int128;
var
  Sign: Byte;
begin
  Sign := 0;
  Result.dc[0] := round(abs(a));
  if a < 0 then
      Sign := $FF;
  FillPtr(@Result.dc[1], SizeOf(Result) - SizeOf(Result.dc[0]), Sign);
end;

class operator Int128.Implicit(a: Int128): TPascalString;
var
  digit, curValue, nextValue: Int128;
  neg: Boolean;
begin
  Result := '';
  if a.b[15] shr 7 = 1 then
    begin
      curValue := UInt128(a.Invert()) + 1;
      neg := True;
    end
  else
    begin
      curValue := a;
      neg := false;
    end;

  while curValue <> 0 do
    begin
      DivMod128(curValue, Ten, nextValue, digit);
      Result := Chr(Ord('0') + digit.c0) + Result;
      curValue := nextValue;
    end;

  if Result = '' then
      Result := '0';
  if neg then
      Result := '-' + Result;
end;

class operator Int128.Implicit(a: Int128): SystemString;
begin
  Result := TPascalString(a).Text;
end;

class operator Int128.Implicit(a: TPascalString): Int128;
begin
  Result := StrToInt128(a);
end;

class operator Int128.Implicit(a: SystemString): Int128;
begin
  Result := StrToInt128(a);
end;

class operator Int128.Implicit(a: UInt128): Int128;
begin
  Result.dc0 := UInt64(a and High(UInt64));
  Result.dc1 := UInt64(a shr 64);
end;

class operator Int128.Implicit(a: Int128): UInt128;
begin
  // if a is -ve, implicit not able to convert
  // if (a.c3 and $80000000) <> 0 then
  // RaiseInfo('Range Check Error');

  Result := TPascalString(a);
end;

class operator Int128.Explicit(a: UInt128): Int128;
begin
  Result := a;
end;

class operator Int128.Explicit(a: Int128): UInt128;
begin
  Result := UInt128(a.dc0);
  Result := Result or (UInt128(a.dc1) shl 64);
end;

class operator Int128.Explicit(a: Int128): Int32;
begin
  Result := a.c0;
end;

class operator Int128.LeftShift(a: Int128; Shift: Int64): Int128;
begin
  if Shift >= 128 then
      Result := a shl (Shift mod 128)
  else if Shift >= 64 then
    begin
      Result.dc1 := a.dc0;
      Result.dc0 := 0;
      Result := Result shl (Shift - 64);
    end
  else if Shift > 0 then
    begin
      Result.dc1 := (a.dc1 shl Shift) or (a.dc0 shr (64 - Shift));
      Result.dc0 := a.dc0 shl Shift;
    end
  else if Shift = 0 then
      Result := a
  else if Shift < 0 then
      Result := a shl (128 - (abs(Shift) mod 128));
end;

class operator Int128.LeftShift(a, Shift: Int128): Int128;
begin
  if Shift < 0 then
      Result := a shl Int32(128 - ((-Shift) mod 128))
  else
      Result := a shl Int32(Shift mod 128);
end;

class operator Int128.LessThan(a, b: Int128): Boolean;
begin
  Result := false;
  if Int64(a.dc1) < Int64(b.dc1) then
      Result := True
  else if a.dc1 = b.dc1 then
    begin
      if a.dc0 < b.dc0 then
          Result := True;
    end;
end;

class operator Int128.LessThanOrEqual(a, b: Int128): Boolean;
begin
  Result := (a = b) or (a < b);
end;

class operator Int128.LogicalNot(a: Int128): Int128;
begin
  Result.dc0 := not a.dc0;
  Result.dc1 := not a.dc1;
end;

class operator Int128.Multiply(a, b: Int128): Int128;
var
  qw: UInt64;
  v: Int128;
  neg, over: Boolean;
begin
  neg := false;
  over := false;
  if (a < 0) xor (b < 0) then
      neg := True;

  if a < 0 then
      a := -a;
  if b < 0 then
      b := -b;

  qw := UInt64(a.c0) * UInt64(b.c0);
  Result.c0 := qw and High(Result.c0);
  Result.c1 := qw shr 32;
  Result.c2 := 0;
  Result.c3 := 0;

  qw := UInt64(a.c0) * UInt64(b.c1);
  v.c0 := 0;
  v.c1 := qw and High(v.c1);
  v.c2 := qw shr 32;
  v.c3 := 0;
  Result := Result + v;

  qw := UInt64(a.c1) * UInt64(b.c0);
  v.c0 := 0;
  v.c1 := qw and High(v.c1);
  v.c2 := qw shr 32;
  v.c3 := 0;
  Result := Result + v;

  qw := UInt64(a.c0) * UInt64(b.c2);
  v.c0 := 0;
  v.c1 := 0;
  v.c2 := qw and High(v.c2);
  v.c3 := qw shr 32;
  Result := Result + v;

  qw := UInt64(a.c1) * UInt64(b.c1);
  v.c0 := 0;
  v.c1 := 0;
  v.c2 := qw and High(v.c2);
  v.c3 := qw shr 32;
  Result := Result + v;

  qw := UInt64(a.c2) * UInt64(b.c0);
  v.c0 := 0;
  v.c1 := 0;
  v.c2 := qw and High(v.c2);
  v.c3 := qw shr 32;
  Result := Result + v;

  qw := UInt64(a.c0) * UInt64(b.c3);
  v.c0 := 0;
  v.c1 := 0;
  v.c2 := 0;
  v.c3 := qw and High(v.c3);
  if qw shr 32 <> 0 then
      over := True;
  Result := Result + v;

  qw := UInt64(a.c1) * UInt64(b.c2);
  v.c0 := 0;
  v.c1 := 0;
  v.c2 := 0;
  v.c3 := qw and High(v.c3);
  if qw shr 32 <> 0 then
      over := True;
  Result := Result + v;

  qw := UInt64(a.c2) * UInt64(b.c1);
  v.c0 := 0;
  v.c1 := 0;
  v.c2 := 0;
  v.c3 := qw and High(v.c3);
  if qw shr 32 <> 0 then
      over := True;
  Result := Result + v;

  qw := UInt64(a.c3) * UInt64(b.c0);
  v.c0 := 0;
  v.c1 := 0;
  v.c2 := 0;
  v.c3 := qw and High(v.c3);
  if qw shr 32 <> 0 then
      over := True;
  Result := Result + v;

  if (Result < a) and (Result < b) then
      RaiseInfo('Integer overflow');
  if (Result = 0) and (a <> 0) and (b <> 0) then
      RaiseInfo('Integer overflow');
  if over then
      RaiseInfo('Integer overflow');
  if neg then
      Result := -Result;
end;

class operator Int128.NotEqual(a, b: Int128): Boolean;
begin
  Result := not(a = b);
end;

class operator Int128.RightShift(a: Int128; Shift: Int64): Int128;
begin
  if Shift >= 128 then
      Result := a shr (Shift mod 128)
  else if Shift >= 64 then
    begin
      Result.dc0 := a.dc1;
      Result.dc1 := 0;
      Result := Result shr (Shift - 64);
    end
  else if Shift > 0 then
    begin
      Result.dc0 := (a.dc0 shr Shift) or (a.dc1 shl (64 - Shift));
      Result.dc1 := a.dc1 shr Shift;
    end
  else if Shift = 0 then
      Result := a
  else if Shift < 0 then
      Result := a shr (128 - (abs(Shift) mod 128))
end;

class operator Int128.RightShift(a, Shift: Int128): Int128;
begin
  if Shift < 0 then
      Result := a shr Int32(128 - ((-Shift) mod 128))
  else
      Result := a shr Int32(Shift mod 128);
end;

class operator Int128.Subtract(a, b: Int128): Int128;
var
  c: Int128;
begin
  c := not b;
  Inc128(c);
  Result := a + c;
end;

class operator Int128.IntDivide(a, b: Int128): Int128;
var
  temp: Int128;
begin
  DivMod128(a, b, Result, temp);
end;

class operator Int128.Modulus(a: Int128; b: Int128): Int128;
var
  temp: Int128;
begin
  DivMod128(a, b, temp, Result);
  if a < 0 then
      Result := -Result;
end;

class operator Int128.BitWiseOr(a, b: Int128): Int128;
begin
  Result.dc0 := a.dc0 or b.dc0;
  Result.dc1 := a.dc1 or b.dc1;
end;

class operator Int128.BitWiseXor(a, b: Int128): Int128;
begin
  Result.dc0 := a.dc0 xor b.dc0;
  Result.dc1 := a.dc1 xor b.dc1;
end;

class operator Int128.BitWiseAnd(a, b: Int128): Int128;
begin
  Result.dc0 := a.dc0 and b.dc0;
  Result.dc1 := a.dc1 and b.dc1;
end;

class operator Int128.Negative(a: Int128): Int128;
begin
  Result := not a + 1;
end;

class operator Int128.Inc(a: Int128): Int128;
begin
  Result := a + Int128(1);
end;

class operator Int128.Dec(a: Int128): Int128;
begin
  Result := a - Int128(1);
end;

procedure TUInt128_VariantType.Clear(var v: TVarData);
begin
  TUInt128_VarData(v).VType := varEmpty;
  Dispose(TUInt128_VarData(v).V_Data);
  TUInt128_VarData(v).V_Data := nil;
end;

function TUInt128_VariantType.IsClear(const v: TVarData): Boolean;
begin
  Result := (TUInt128_VarData(v).V_Data = nil) or (TUInt128_VarData(v).V_Data^ = 0);
end;

procedure TUInt128_VariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
      VarDataCopyNoInd(Dest, Source)
  else
    begin
      VarDataClear(Dest);
      TUInt128_VarData(Dest).VType := TUInt128_VarData(Source).VType;
      New(TUInt128_VarData(Dest).V_Data);
      TUInt128_VarData(Dest).V_Data^ := TUInt128_VarData(Source).V_Data^;
    end;
end;

procedure TUInt128_VariantType.Cast(var Dest: TVarData; const Source: TVarData);
var
  LSource, LTemp: TVarData;
begin
  VarDataInit(LSource);
  try
    VarDataCopyNoInd(LSource, Source);
    if VarDataIsStr(LSource) then
      begin
        New(TUInt128_VarData(Dest).V_Data);
        TUInt128_VarData(Dest).V_Data^ := TPascalString(VarDataToStr(LSource));
      end
    else
      begin
        VarDataInit(LTemp);
        try
          VarDataCastTo(LTemp, LSource, varString);
          New(TUInt128_VarData(Dest).V_Data);
          TUInt128_VarData(Dest).V_Data^ := TPascalString(VarDataToStr(LTemp));
        finally
            VarDataClear(LTemp);
        end;
      end;
    Dest.VType := VarType;
  finally
      VarDataClear(LSource);
  end;
end;

procedure TUInt128_VariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
var
  LTemp: TVarData;
begin
  if Source.VType <> VarType then
      RaiseCastError;

  case AVarType of
    varOleStr: VarDataFromOleStr(Dest, TUInt128_VarData(Source).V_Data^.ToString.Text);
    varString: VarDataFromStr(Dest, TUInt128_VarData(Source).V_Data^.ToString.Text);
    else
      begin
        VarDataInit(LTemp);
        try
          VarDataFromStr(LTemp, TUInt128_VarData(Source).V_Data^.ToString.Text);
          VarDataCastTo(Dest, LTemp, AVarType);
        finally
            VarDataClear(LTemp);
        end;
      end;
  end;
end;

procedure TUInt128_VariantType.BinaryOp(var Left: TVarData; const Right: TVarData; const Op: TVarOp);
begin
  case Op of
    opAdd: TUInt128_VarData(Left).V_Data^ := TUInt128_VarData(Left).V_Data^ + TUInt128_VarData(Right).V_Data^;
    opMultiply: TUInt128_VarData(Left).V_Data^ := TUInt128_VarData(Left).V_Data^ * TUInt128_VarData(Right).V_Data^;
    opIntDivide: TUInt128_VarData(Left).V_Data^ := TUInt128_VarData(Left).V_Data^ div TUInt128_VarData(Right).V_Data^;
    opModulus: TUInt128_VarData(Left).V_Data^ := TUInt128_VarData(Left).V_Data^ mod TUInt128_VarData(Right).V_Data^;
    opShiftLeft: TUInt128_VarData(Left).V_Data^ := TUInt128_VarData(Left).V_Data^ shl TUInt128_VarData(Right).V_Data^;
    opShiftRight: TUInt128_VarData(Left).V_Data^ := TUInt128_VarData(Left).V_Data^ shr TUInt128_VarData(Right).V_Data^;
    else
      RaiseInvalidOp;
  end;
end;

function TUInt128_VariantType.CompareOp(const Left, Right: TVarData; const Operator: TVarOp): Boolean;
begin
  Result := false;
  if (Left.VType = VarType) and (Right.VType = VarType) then
    begin
      case Operator of
        opCmpEQ: Result := TUInt128_VarData(Left).V_Data^ = TUInt128_VarData(Right).V_Data^;
        opCmpNE: Result := not(TUInt128_VarData(Left).V_Data^ = TUInt128_VarData(Right).V_Data^);
        opCmpLT: Result := TUInt128_VarData(Left).V_Data^ < TUInt128_VarData(Right).V_Data^;
        opCmpLE: Result := TUInt128_VarData(Left).V_Data^ <= TUInt128_VarData(Right).V_Data^;
        opCmpGT: Result := TUInt128_VarData(Left).V_Data^ > TUInt128_VarData(Right).V_Data^;
        opCmpGE: Result := TUInt128_VarData(Left).V_Data^ >= TUInt128_VarData(Right).V_Data^;
        else
          RaiseInvalidOp;
      end;
    end
  else
      RaiseInvalidOp;
end;

procedure TInt128_VariantType.Clear(var v: TVarData);
begin
  TInt128_VarData(v).VType := varEmpty;
  Dispose(TInt128_VarData(v).V_Data);
  TInt128_VarData(v).V_Data := nil;
end;

function TInt128_VariantType.IsClear(const v: TVarData): Boolean;
begin
  Result := (TInt128_VarData(v).V_Data = nil) or (TInt128_VarData(v).V_Data^ = 0);
end;

procedure TInt128_VariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
      VarDataCopyNoInd(Dest, Source)
  else
    begin
      VarDataClear(Dest);
      TInt128_VarData(Dest).VType := TInt128_VarData(Source).VType;
      New(TInt128_VarData(Dest).V_Data);
      TInt128_VarData(Dest).V_Data^ := TInt128_VarData(Source).V_Data^;
    end;
end;

procedure TInt128_VariantType.Cast(var Dest: TVarData; const Source: TVarData);
var
  LSource, LTemp: TVarData;
begin
  VarDataInit(LSource);
  try
    VarDataCopyNoInd(LSource, Source);
    if VarDataIsStr(LSource) then
      begin
        New(TInt128_VarData(Dest).V_Data);
        TInt128_VarData(Dest).V_Data^ := TPascalString(VarDataToStr(LSource));
      end
    else
      begin
        VarDataInit(LTemp);
        try
          VarDataCastTo(LTemp, LSource, varString);
          New(TInt128_VarData(Dest).V_Data);
          TInt128_VarData(Dest).V_Data^ := TPascalString(VarDataToStr(LTemp));
        finally
            VarDataClear(LTemp);
        end;
      end;
    Dest.VType := VarType;
  finally
      VarDataClear(LSource);
  end;
end;

procedure TInt128_VariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
var
  LTemp: TVarData;
begin
  if Source.VType <> VarType then
      RaiseCastError;

  case AVarType of
    varOleStr: VarDataFromOleStr(Dest, TInt128_VarData(Source).V_Data^.ToString.Text);
    varString: VarDataFromStr(Dest, TInt128_VarData(Source).V_Data^.ToString.Text);
    else
      begin
        VarDataInit(LTemp);
        try
          VarDataFromStr(LTemp, TInt128_VarData(Source).V_Data^.ToString.Text);
          VarDataCastTo(Dest, LTemp, AVarType);
        finally
            VarDataClear(LTemp);
        end;
      end;
  end;
end;

procedure TInt128_VariantType.BinaryOp(var Left: TVarData; const Right: TVarData; const Op: TVarOp);
begin
  case Op of
    opAdd: TInt128_VarData(Left).V_Data^ := TInt128_VarData(Left).V_Data^ + TInt128_VarData(Right).V_Data^;
    opMultiply: TInt128_VarData(Left).V_Data^ := TInt128_VarData(Left).V_Data^ * TInt128_VarData(Right).V_Data^;
    opIntDivide: TInt128_VarData(Left).V_Data^ := TInt128_VarData(Left).V_Data^ div TInt128_VarData(Right).V_Data^;
    opModulus: TInt128_VarData(Left).V_Data^ := TInt128_VarData(Left).V_Data^ mod TInt128_VarData(Right).V_Data^;
    opShiftLeft: TInt128_VarData(Left).V_Data^ := TInt128_VarData(Left).V_Data^ shl TInt128_VarData(Right).V_Data^;
    opShiftRight: TInt128_VarData(Left).V_Data^ := TInt128_VarData(Left).V_Data^ shr TInt128_VarData(Right).V_Data^;
    else
      RaiseInvalidOp;
  end;
end;

function TInt128_VariantType.CompareOp(const Left, Right: TVarData; const Operator: TVarOp): Boolean;
begin
  Result := false;
  if (Left.VType = VarType) and (Right.VType = VarType) then
    begin
      case Operator of
        opCmpEQ: Result := TInt128_VarData(Left).V_Data^ = TInt128_VarData(Right).V_Data^;
        opCmpNE: Result := not(TInt128_VarData(Left).V_Data^ = TInt128_VarData(Right).V_Data^);
        opCmpLT: Result := TInt128_VarData(Left).V_Data^ < TInt128_VarData(Right).V_Data^;
        opCmpLE: Result := TInt128_VarData(Left).V_Data^ <= TInt128_VarData(Right).V_Data^;
        opCmpGT: Result := TInt128_VarData(Left).V_Data^ > TInt128_VarData(Right).V_Data^;
        opCmpGE: Result := TInt128_VarData(Left).V_Data^ >= TInt128_VarData(Right).V_Data^;
        else
          RaiseInvalidOp;
      end;
    end
  else
      RaiseInvalidOp;
end;

function TCritical_Int128_Helper.Get(var x: Int128): Int128;
begin
  Lock;
  Result := x;
  UnLock;
end;

function TCritical_Int128_Helper.Get(var x: UInt128): UInt128;
begin
  Lock;
  Result := x;
  UnLock;
end;

procedure TCritical_Int128_Helper.Inc_(var x: Int128);
begin
  Lock;
  x := x + 1;
  UnLock;
end;

procedure TCritical_Int128_Helper.Inc_(var x: Int128; const v: Int128);
begin
  Lock;
  x := x + v;
  UnLock;
end;

procedure TCritical_Int128_Helper.Dec_(var x: Int128);
begin
  Lock;
  x := x - 1;
  UnLock;
end;

procedure TCritical_Int128_Helper.Dec_(var x: Int128; const v: Int128);
begin
  Lock;
  x := x - v;
  UnLock;
end;

procedure TCritical_Int128_Helper.Inc_(var x: UInt128);
begin
  Lock;
  x := x + 1;
  UnLock;
end;

procedure TCritical_Int128_Helper.Inc_(var x: UInt128; const v: UInt128);
begin
  Lock;
  x := x + v;
  UnLock;
end;

procedure TCritical_Int128_Helper.Dec_(var x: UInt128);
begin
  Lock;
  x := x - 1;
  UnLock;
end;

procedure TCritical_Int128_Helper.Dec_(var x: UInt128; const v: UInt128);
begin
  Lock;
  x := x - v;
  UnLock;
end;

initialization

UInt128_VariantType := TUInt128_VariantType.Create;
Int128_VariantType := TInt128_VariantType.Create;

finalization

FreeAndNil(UInt128_VariantType);
FreeAndNil(Int128_VariantType);

end.
 
