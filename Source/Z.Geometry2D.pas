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
{ * geometry 2D library                                                        * }
{ ****************************************************************************** }

unit Z.Geometry2D;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Types, Z.MemoryStream, Z.PascalStrings, Z.UPascalStrings, Z.Int128;

{$REGION 'BaseType define'}


type
  TGeoFloat = Single;
  TGeoFloatList = TGenericsList<TGeoFloat>;

  TGeoInt = Integer;
  TVec2 = array [0 .. 1] of TGeoFloat;
  PVec2 = ^TVec2;
  T2DPoint = TVec2;
  P2DPoint = PVec2;
  TPoint2 = T2DPoint;
  TArrayVec2 = array of TVec2;
  PArrayVec2 = ^TArrayVec2;
  TVec2Array = TArrayVec2;

  TMatrixVec2 = array of TArrayVec2;

  TArray2DPoint = TArrayVec2;
  PArray2DPoint = PArrayVec2;
  T2DPointArray = TArray2DPoint;

  TArrayPVec2 = array of PVec2;
  PArrayPVec2 = ^TArrayPVec2;
  TPVec2Array = TArrayPVec2;

  TRectV2 = array [0 .. 1] of TVec2;
  PRectV2 = ^TRectV2;
  TRect2 = TRectV2;
  TRect2D = TRectV2;
  TArrayRectV2 = array of TRectV2;
  TRectV2Array = TArrayRectV2;
  TMatrix_RectV2 = array of TArrayRectV2;

  TRectV2List = TGenericsList<TRectV2>;

  TLineV2 = array [0 .. 1] of TVec2;
  PLineV2 = ^TLineV2;
  TLine2 = TLineV2;
  TLine2D = TLineV2;

  TArrayLineV2 = array of TLineV2;
  PArrayLineV2 = ^TArrayLineV2;

  TLineV2_P = array [0 .. 1] of PVec2;
  PLineV2_P = ^TLineV2_P;

  TTriangle = array [0 .. 2] of TVec2;
  PTriangle = ^TTriangle;

  TTriangleArray = array of TTriangle;
  PTriangleArray = ^TTriangleArray;

  TGeoFloatArray = array of TGeoFloat;
  PGeoFloatArray = ^TGeoFloatArray;

  TArrayPoint = array of TPoint;

{$IFDEF FPC}

  TPointf = record
    X: TGeoFloat;
    Y: TGeoFloat;
  end;

  PPointf = ^TPointf;

  TRectf = record
    case TGeoInt of
      0: (Left, Top, Right, Bottom: TGeoFloat);
      1: (TopLeft, BottomRight: TPointf);
  end;

  PRectf = ^TRectf;
  TArrayPointf = array of TPointf;

function Pointf(X, Y: TGeoFloat): TPointf;
function Rectf(Left, Top, Right, Bottom: TGeoFloat): TRectf;
{$ELSE FPC}
  TArrayPointf = array of TPointf;
{$ENDIF FPC}
{$ENDREGION 'BaseType define'}
{$REGION 'Compute_Function'}

function CompareCardinal(const C1, C2: Cardinal): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function CompareInteger(const Int1, Int2: Integer): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function CompareInt64(const Int1, Int2: Int64): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function CompareUInt64(const Int1, Int2: UInt64): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function ComparePointer(const Item1, Item2: pointer): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function CompareBool(const Bool1, Bool2: Boolean): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function CompareDouble(const Double1, Double2: Double): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function CompareInt128(const Int1, Int2: Int128): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function CompareUInt128(const Int1, Int2: UInt128): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}

function Compute_PI(Num: Integer): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM}
function FAbs(const V: Single): Single; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function FAbs(const V: Double): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function FAbs(const v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Clamp(const Value_, Min_, Max_: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MaxF(const v1, v2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM} overload;
function MaxF(const V: TVec2): TGeoFloat; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM} overload;
function MaxF(const R: TRectV2): TGeoFloat; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM} overload;
function MinF(const v1, v2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM} overload;
function MinF(const V: TVec2): TGeoFloat; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM} overload;
function MinF(const R: TRectV2): TGeoFloat; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM} overload;
function CompareFloat(const f1, f2, Epsilon_: TGeoFloat): ShortInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function CompareFloat(const f1, f2: TGeoFloat): ShortInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function CompareGeoInt(const g1, g2: TGeoInt): ShortInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM}

function MakeVec2(const X, Y: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakeVec2(const X, Y: TGeoInt): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakePoint(const X, Y: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakePoint(const X, Y: TGeoInt): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakePoint(const pt: TVec2): TPoint; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Point2Point(const pt: TVec2): TPoint; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Point2Pointf(const pt: TVec2): TPointf; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointMake(const X, Y: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointMake(const pt: TPoint): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointMake(const pt: TPointf): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function Make2DPoint(const X, Y: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Make2DPoint(const X, Y: TGeoInt): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Make2DPoint(const pt: TPoint): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Make2DPoint(const pt: TPointf): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function vec2(const p: PVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function vec2(const f: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function vec2(const X, Y: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function vec2(const X, Y: TGeoInt): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function vec2(const X, Y: Int64): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function vec2(const pt: TPoint): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function vec2(const pt: TPointf): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function LineV2(const x1, y1, x2, y2: TGeoFloat): TLineV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function LineV2(const lb, le: TVec2): TLineV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function LineV2(const lb, le: TPoint): TLineV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function LineV2(const L: TLineV2_P): TLineV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function LineV2(const L: PLineV2_P): TLineV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function LineV2(const L: PLineV2): TLineV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RoundVec2(const V: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function MakePointf(const pt: TVec2): TPointf; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function IsZero(const V: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function IsZero(const pt: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function IsZero(const R: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function IsNan(const pt: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function IsNan(const X, Y: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function HypotX(const X, Y: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function PointNorm(const V: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointNegate(const V: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function Vec2Norm(const V: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Negate(const V: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function Vec2Inv(const V: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
procedure SetVec2(var V: TVec2; const vSrc: TVec2); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function Vec2Direction(sour, dest: TVec2): TVec2; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function RectDirection(sour, dest: TRectV2): TRectV2; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}

function Vec2Add(const v1, v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Add(const v1: TVec2; v2: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Add(const v1: TVec2; X, Y: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Add(const v1: TGeoFloat; v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Add(const v1: TArrayVec2; v2: TVec2): TArrayVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Add(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function Vec2Sub(const v1, v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Sub(const v1: TVec2; v2: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Sub(const v1: TGeoFloat; v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Sub(const v1: TArrayVec2; v2: TVec2): TArrayVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Sub(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function Vec2Mul(const v1, v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Mul(const v1, v2: TVec2; const v3: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Mul(const v1, v2: TVec2; const v3, v4: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Mul(const v1, v2, v3: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Mul(const v1, v2, v3, v4: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Mul(const v1: TVec2; const v2: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Mul(const v1: TVec2; const v2, v3: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Mul(const v1: TVec2; const v2, v3, v4: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Mul(const v1: TGeoFloat; const v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Mul(const v1: TArrayVec2; v2: TVec2): TArrayVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Mul(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function Vec2Div(const v1: TVec2; const v2: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Div(const v1, v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Div(const v1: TGeoFloat; const v2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function PointNormalize(const V: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Normalize(const V: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function NoLoss_PointNormalize(const V: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function NoLoss_Vec2Normalize(const V: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function PointLength(const V: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Length(const V: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

procedure PointScale(var V: TVec2; factor: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointDotProduct(const v1, v2: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2DotProduct(const v1, v2: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

// line segment
procedure MidPoint(const x1, y1, x2, y2: TGeoFloat; out midx, midy: TGeoFloat);
procedure ShortenSegment(const Amount: TGeoFloat; var x1, y1, x2, y2: TGeoFloat);
procedure LengthenSegment(const Amount: TGeoFloat; out x1, y1, x2, y2: TGeoFloat);

// (3d) Barycentric and Calculate
function Signed(const x1, y1, x2, y2, Px, Py: TGeoFloat): TGeoFloat;
function CalculateBarycentricBase(const x1, y1, x2, y2, x3, y3: TGeoFloat): TGeoFloat;
procedure ConvertCartesianToBarycentric(const x1, y1, x2, y2, x3, y3, Px, Py: TGeoFloat; out U, V, W: TGeoFloat);
procedure ConvertBarycentricToCartesian(const U, V, W, x1, y1, x2, y2, x3, y3: TGeoFloat; out X, Y: TGeoFloat);

procedure ClosestPointOnLineFromPoint(const x1, y1, x2, y2, Px, Py: TGeoFloat; out Nx, Ny: TGeoFloat);

// NonSymmetricMirror
procedure NonSymmetricMirror(const Px, Py, x1, y1, x2, y2: TGeoFloat; const Ratio: TGeoFloat; out Nx, Ny: TGeoFloat); overload;
function NonSymmetricMirror(const Point: TVec2; const Ratio: TGeoFloat; const Line: TLineV2): TVec2; overload;
function NonSymmetricMirror(const Rectangle: TRectV2; const Ratio: TGeoFloat; const Line: TLineV2): TRectV2; overload;
function NonSymmetricMirror(const Triangle: TTriangle; const Ratio: TGeoFloat; const Line: TLineV2): TTriangle; overload;

// Mirror
procedure Mirror(const Px, Py, x1, y1, x2, y2: TGeoFloat; out Nx, Ny: TGeoFloat); overload;
function Mirror(const Point: TVec2; const Line: TLineV2): TVec2; overload;
function Mirror(const Rectangle: TRectV2; const Line: TLineV2): TRectV2; overload;
function Mirror(const Triangle: TTriangle; const Line: TLineV2): TTriangle; overload;

function Distance(const x1, y1, x2, y2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Distance(const x1, y1, z1, x2, y2, z2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Distance(const L: TLineV2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Distance(const f1, f2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function FloatDistance(const f1, f2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointDistance(const x1, y1, x2, y2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointDistance(const v1, v2: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Distance(const v1, v2: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function LineDistance(const L: TLineV2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointLayDistance(const v1, v2: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function LayDistance(const x1, y1, x2, y2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM}
function SqrDistance(const v1, v2: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function PointLerp(const v1, v2: TVec2; t: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointLerpTo(const sour, dest: TVec2; const d: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Lerp(const v1, v2: TVec2; t: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2LerpTo(const sour, dest: TVec2; const d: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
procedure SwapPoint(var v1, v2: TVec2); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
procedure SwapVec2(var v1, v2: TVec2); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Pow(V: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Pow(const V, n: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MiddleVec2(const pt1, pt2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Middle(const pt1, pt2: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function IsEqual(const Val1, Val2, Epsilon_: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function IsEqual(const Val1, Val2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function IsEqual(const Val1, Val2: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function IsEqual(const Val1, Val2: TVec2; Epsilon_: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function IsEqual(const Val1, Val2: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function IsEqual_X(const Val1, Val2: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function IsEqual_Y(const Val1, Val2: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function NotEqual(const Val1, Val2, Epsilon_: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function NotEqual(const Val1, Val2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function NotEqual(const Val1, Val2: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function LessThanOrEqual(const Val1, Val2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function GreaterThanOrEqual(const Val1, Val2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function GetEquilateralTriangleCen(pt1, pt2: TVec2): TVec2; overload;

procedure Rotate(RotAng: TGeoFloat; const X, Y: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Rotate(const RotAng: TGeoFloat; const Point: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function NormalizeDegAngle(const Angle: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function VerticalMirror(const Angle: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM}
function HorizontalMirror(const Angle: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM}

// axis to pt angle
function PointAngle(const axis, pt: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Angle(const axis, pt: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
// null point to pt angle
function PointAngle(const pt: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Angle(const pt: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function AngleDistance(const s, a: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointRotation(const axis: TVec2; const Dist, Angle: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointRotation(const axis, pt: TVec2; const Angle: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Rotation(const axis: TVec2; const Dist, Angle: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Rotation(const axis, pt: TVec2; const Angle: TGeoFloat): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Rotation(const sour_r: TRectV2; const Angle: TGeoFloat; const pt: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Rotation(const sour_r: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const pt: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Rotation(const sour_r: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const R: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectRotation(const axis: TVec2; const R: TRectV2; const Angle: TGeoFloat): TRectV2;

function CircleInCircle(const cp1, cp2: TVec2; const r1, r2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function CircleInRect(const cp: TVec2; const radius: TGeoFloat; R: TRectV2): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function PointInRect(const Px, Py: TGeoFloat; const x1, y1, x2, y2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointInRect(const Px, Py: TGeoInt; const x1, y1, x2, y2: TGeoInt): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointInRect(const X, Y: TGeoInt; const R: TRect): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointInRect(const pt: TPoint; const R: TRect): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointInRect(const pt: TVec2; const R: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointInRect(const Px, Py: TGeoFloat; const R: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2InRect(const Px, Py: TGeoFloat; const x1, y1, x2, y2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2InRect(const Px, Py: TGeoInt; const x1, y1, x2, y2: TGeoInt): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2InRect(const pt: TVec2; const R: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2InRect(const Px, Py: TGeoFloat; const R: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectToRectIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectToRectIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectToRectIntersect(const r1, r2: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectToRectIntersect(const r1, r2: TRect): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectToRectIntersect(const r1, r2: TRectf): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectWithInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectWithInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectWithInRect(const r1, r2: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectWithInRect(const r1, r2: TRect): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectInRect(const r1, r2: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectInRect(const r1, r2: TRect): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function MakeRectV2(const centre: TVec2; const width, height: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakeRectV2(const X, Y, radius: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakeRectV2(const x1, y1, x2, y2: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakeRectV2(const p1, p2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakeRectV2(const X, Y: TGeoFloat; const p2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakeRectV2(const R: TRect): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakeRectV2(const R: TRectf): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RectV2(): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectV2(const centre: TVec2; const width, height: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectV2(const X, Y, radius: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectV2(const x1, y1, x2, y2: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectV2(const p1, p2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectV2(const p1, p2: TPointf): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectV2(const X, Y: TGeoFloat; const p2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectV2(const R: TRect): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectV2(const R: TRectf): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectV2(const R: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function MakeRect(const centre: TVec2; const width, height: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakeRect(const X, Y, radius: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakeRect(const x1, y1, x2, y2: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakeRect(const p1, p2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakeRect(const R: TRect): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakeRect(const R: TRectf): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RoundRect(const R: TRectV2): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RoundRectV2(const R: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function Rect2Rect(const R: TRectV2): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Rect2Rect(const R: TRect): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RectMake(const X, Y, radius: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectMake(const x1, y1, x2, y2: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectMake(const p1, p2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectMake(const R: TRect): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectMake(const R: TRectf): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RectAdd(const R: TRectV2; v2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectAdd(const r1, r2: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RectSub(const r1, r2: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectSub(const R: TRectV2; pt: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RectMul(const r1, r2: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectMul(const r1: TRectV2; v2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectMul(const r1: TRectV2; f2: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RectDiv(const r1, r2: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectDiv(const r1: TRectV2; f2: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectDiv(const r1: TRectV2; v2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RectOffset(const R: TRectV2; Offset: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectSizeLerp(const R: TRectV2; const rSizeLerp: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectCenScale(const R: TRectV2; const rSizeScale: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectEdge(const R: TRectV2; const Edge: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectEdge(const R: TRectV2; const Edge: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectCentre(const R: TRectV2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectCentre(const R: TRect): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectCentre(const R: TRectf): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectIOU(const r1, r2: TRectV2): TGeoFloat; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function RectDistance(const r1, r2: TRectV2): TGeoFloat; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}

function Tri(const v1, v2, v3: TVec2): TTriangle; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function TriAdd(const t: TTriangle; V: TVec2): TTriangle; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function TriSub(const t: TTriangle; V: TVec2): TTriangle; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function TriMul(const t: TTriangle; V: TVec2): TTriangle; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function TriDiv(const t: TTriangle; V: TVec2): TTriangle; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function TriCentre(const t: TTriangle): TVec2; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function TriExpand(const t: TTriangle; Dist: TGeoFloat): TTriangle;
function TriRound(const t: TTriangle): TTriangle; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function IsEquilateralTriangle(const x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM} overload;
function IsEquilateralTriangle(const t: TTriangle): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM} overload;
function IsIsoscelesTriangle(const x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}overload;
function IsIsoscelesTriangle(const t: TTriangle): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}overload;
function IsRightTriangle(const x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}overload;
function IsRightTriangle(const t: TTriangle): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}overload;
function IsScaleneTriangle(const x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}overload;
function IsScaleneTriangle(const t: TTriangle): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}overload;
function VertexAngle(x1, y1, x2, y2, x3, y3: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function IsObtuseTriangle(const x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}overload;
function IsObtuseTriangle(const t: TTriangle): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}overload;

type
  TTriangleType = (ttEquilateral, ttIsosceles, ttRight, ttScalene, ttObtuse, ttUnknown);
function TriangleType(const x1, y1, x2, y2, x3, y3: TGeoFloat): TTriangleType; overload;
function TriangleType(const t: TTriangle): TTriangleType; overload;

// edge line
function TriangleEdge(const Triangle: TTriangle; const Edge: Integer): TLineV2; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function RectangleEdge(const Rectangle: TRectV2; const Edge: Integer): TLineV2; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}

// projection transform
function Vec2Transform(const sour, dest: TRectV2; sour_pt: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2Transform(const sour, dest: TRectV2; const sourAngle, destAngle: TGeoFloat; const sour_pt: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectTransform(const sour, dest, sour_rect: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectTransform(const sour, dest: TRectV2; const sour_rect: TRect): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectTransform(const sour, dest: TRectV2; const sour_rect: TRectf): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

// scale space
function RectScaleSpace(const R: TRectV2; const SS_width, SS_height: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectScaleSpace(const R: TRect; const SS_width, SS_height: TGeoInt): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function ComputeScaleSpace(box: TRectV2; SS: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM}
function Rect_ScaleSpace_F(R: TRectV2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM}
function MinLoss_Rect(const SS_width, SS_height: TGeoFloat; const R: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MinLoss_Rect(const R: TRectV2; const SS_width, SS_height: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MinLoss_RectScaleSpace(const R: TRectV2; const SS_width, SS_height: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MinLoss_RectFit(const SS_width, SS_height: TGeoFloat; const R: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MinLoss_RectFit(const R: TRectV2; const SS_width, SS_height: TGeoFloat): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function CalibrationRectInRect(const R, Area: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function CalibrationRectInRect(const R, Area: TRect): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

// XY_Offset_Scale: minimize edge scale
function Make_Jitter_Box(rnd: TMT19937Random; XY_Offset_Scale_, Rotate_, Scale_: TGeoFloat; Fit_: Boolean;
  const source: TRectV2; var dest: TRectV2; var Angle: TGeoFloat): TGeoInt; overload;
function Make_Jitter_Box(XY_Offset_Scale_, Rotate_, Scale_: TGeoFloat; Fit_: Boolean;
  const source: TRectV2; var dest: TRectV2; var Angle: TGeoFloat): TGeoInt; overload;

// image jitter
procedure Make_Image_Jitter_Box(
  rand: TMT19937Random;
  image_bound_Box: TRectV2; // image bound box
  scale_size, scale_pos: TVec2; // matrix box
  XY_Offset_Scale_, Rotate_, Scale_: TGeoFloat;
  Fit_Matrix_Box_: Boolean; // min-loss fit
  output_Size: TVec2; // output size
  var dest: TRectV2; var Angle: TGeoFloat // output
  ); overload;
procedure Make_Image_Jitter_Box(
  image_bound_Box: TRectV2; // image bound box
  scale_size, scale_pos: TVec2; // matrix box
  XY_Offset_Scale_, Rotate_, Scale_: TGeoFloat;
  Fit_Matrix_Box_: Boolean; // min-loss fit
  output_Size: TVec2; // output size
  var dest: TRectV2; var Angle: TGeoFloat // output
  ); overload;

function Rect_Overlap_or_Intersect(r1, r2: TRectV2): Boolean;
function Rect_1Overlap2_or_Intersect(r1, r2: TRectV2): Boolean;

procedure FixRect(var Left, Top, Right, Bottom: TGeoInt); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
procedure FixRect(var Left, Top, Right, Bottom: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function FixRect(R: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function FixRect(R: TRect): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
procedure FixedRect(var Left, Top, Right, Bottom: TGeoInt); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
procedure FixedRect(var Left, Top, Right, Bottom: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function FixedRect(R: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function FixedRect(R: TRect): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
procedure ForwardRect(var Left, Top, Right, Bottom: TGeoInt); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
procedure ForwardRect(var Left, Top, Right, Bottom: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function ForwardRect(R: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function ForwardRect(R: TRect): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function MakeRect(const R: TRectV2): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MakeRectf(const R: TRectV2): TRectf; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RectWidth(const R: TRectV2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectHeight(const R: TRectV2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectWidth(const R: TRect): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectHeight(const R: TRect): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectWidth(const R: TRectf): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectHeight(const R: TRectf): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RoundWidth(const R: TRectV2): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RoundHeight(const R: TRectV2): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RoundWidth(const R: TRect): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RoundHeight(const R: TRect): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RoundWidth(const R: TRectf): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RoundHeight(const R: TRectf): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RectArea(const R: TRectV2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectArea(const R: TRect): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectSize(const R: TRectV2): TVec2; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function RectSizeR(const R: TRectV2): TRectV2; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function RectFit(const sour, dest: TRectV2; const Bound: Boolean): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectFit(const sour, dest: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectFit(const width, height: TGeoFloat; const bk: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function FitRect(const sour, dest: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function FitRect(const width, height: TGeoFloat; const bk: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BoundRect(const buff: TArrayPoint): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BoundRect(const p1, p2, p3: TPoint): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BoundRect(const p1, p2, p3, p4: TPoint): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BoundRect(const r1, r2: TRect): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BoundRect(const R: TRect; p: TPoint): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BoundRect(const buff: TArrayVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BoundRect(const p1, p2, p3: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BoundRect(const p1, p2, p3, p4: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BoundRect(const r1, r2: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BoundRect(const r1: TRectV2; const p1: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BoundRect(const r1: TRectV2; const p1, p2: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BoundRect(const r1: TRectV2; const p1, p2, p3: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BoundRect(const r1: TRectV2; const p1, p2, p3, p4: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BuffCentroid(const buff: TArrayVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BuffCentroid(const p1, p2, p3, p4: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function BuffCentroid(const p1, p2, p3: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointInPolygon(pt: TVec2; const PolygonBuff: TArrayVec2): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function PolygonArea(buff: TArrayVec2): TGeoFloat; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM} overload;

function FastRamerDouglasPeucker(var Points: TArrayVec2; Epsilon_: TGeoFloat): TGeoInt; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
procedure FastVertexReduction(Points: TArrayVec2; Epsilon_: TGeoFloat; var output: TArrayVec2);

function Clip(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat; out Cx1, Cy1, Cx2, Cy2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Clip(const f, b: TRectV2; var R: TRectV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Clip(const f, b: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Compute_IoU(const r1, r2: TRectV2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Compute_IoU(const r1, r2: TRectV2; var R: TRectV2; var IoU: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Compute_IoU(const r1, r2: TRectV2; var R: TRectV2; var IoU, R1A, R2A, RA: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function Orientation(const x1, y1, x2, y2, Px, Py: TGeoFloat): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Orientation(const x1, y1, z1, x2, y2, z2, x3, y3, z3, Px, Py, Pz: TGeoFloat): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Coplanar(const x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function SimpleIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function SimpleIntersect(const Point1, Point2, Point3, Point4: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function SimpleIntersect(const l1, l2: TLineV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Intersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Intersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat; out ix, iy: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Intersect(const pt1, pt2, pt3, pt4: TVec2; out pt: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Intersect(const l1, l2: TLineV2; out pt: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Intersect(const pt1, pt2, pt3, pt4: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function PointInCircle(const pt, cp: TVec2; radius: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Vec2InCircle(const pt, cp: TVec2; radius: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function PointInTriangle(const Px, Py, x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
procedure BuildSinCosCache(const oSin, oCos: PGeoFloatArray; const b, E: TGeoFloat); {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}

procedure ClosestPointOnSegmentFromPoint(const x1, y1, x2, y2, Px, Py: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function ClosestPointOnSegmentFromPoint(const lb, le, pt: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function ClosestPointOnSegmentFromLine(const L: TLineV2; const pt: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function ClosestPointOnSegmentFromLine(const pt: TVec2; const L: TLineV2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function MinimumDistanceFromPointToLine(const Px, Py, x1, y1, x2, y2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MinimumDistanceFromPointToLine(const pt: TVec2; const L: TLineV2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MinimumDistanceFromPointToLine(const L: TLineV2; const pt: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function MinimumDistanceFromPointToLine(const lb, le, pt: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function Compute_Scale_Position_To_Abs_Size(box: TRectV2; size, sPos: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Compute_Scale_Position_To_Box_Size(box: TRectV2; size, sPos: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Compute_Scale_Position_To_Min_Edge_Box_Size(box: TRectV2; size, sPos: TVec2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

// projection
function RectProjection(const sour, dest: TRectV2; const sour_pt: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectProjection(const sour, dest: TRectV2; const sour_rect: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectProjection(const sour, dest: TRectV2; const sour_arry: TArrayVec2): TArrayVec2; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM} overload;
function RectProjectionArrayV2(const sour, dest: TRectV2; const sour_arry: TArrayVec2): TArrayVec2; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}

function RectProjectionRotationDest(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectProjectionRotationDest(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectProjectionRotationSource(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectProjectionRotationSource(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RectProjectionRotationDest(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectProjectionRotationDest(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectProjectionRotationSource(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectProjectionRotationSource(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAxis, destAxis: TVec2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_pt: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAxis, destAxis: TVec2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_rect: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_pt: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_rect: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function Quadrant(const Angle: TGeoFloat): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
procedure ProjectionPoint(const Srcx, Srcy, Dstx, Dsty, Dist: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
procedure ProjectionPoint(const Srcx, Srcy, Srcz, Dstx, Dsty, Dstz, Dist: TGeoFloat; out Nx, Ny, Nz: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
procedure ProjectionPoint(const Px, Py, Angle, Distance: TGeoFloat; out Nx, Ny: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function GetCicleRadiusInPolyEdge(R: TGeoFloat; PolySlices: TGeoInt): TGeoFloat; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}

procedure Circle2LineIntersectionPoint(const lb, le, cp: TVec2; const radius: TGeoFloat;
  out pt1in, pt2in: Boolean; out ICnt: TGeoInt; out pt1, pt2: TVec2); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
procedure Circle2LineIntersectionPoint(const L: TLineV2; const cp: TVec2; radius: TGeoFloat;
  out pt1in, pt2in: Boolean; out ICnt: TGeoInt; out pt1, pt2: TVec2); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

procedure Circle2CircleIntersectionPoint(const cp1, cp2: TVec2; const r1, r2: TGeoFloat; out Point1, Point2: TVec2); {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

// circle collision Detector
function Detect_Circle2Circle(const p1, p2: TVec2; const r1, r2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function CircleCollision(const p1, p2: TVec2; const r1, r2: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function Detect_Circle2CirclePoint(const p1, p2: TVec2; const r1, r2: TGeoFloat; out op1, op2: TVec2): Boolean;

// circle 2 line collision
function Detect_Circle2Line(const cp: TVec2; const R: TGeoFloat; const lb, le: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;
function Detect_Circle2Line(const cp: TVec2; const R: TGeoFloat; const L: TLineV2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF INLINE_ASM} overload;

function RectangularHull(const buff: TArrayVec2): TRectV2;

function SameLinePtr(const lb1, le1, lb2, le2: PVec2): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}

function ComputeCurvePartPrecision(const pt1, pt2, pt3, pt4: TVec2): TGeoInt; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function Interpolation_OutSide(const T_: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function Interpolation_InSide(const t: TGeoFloat): TGeoFloat;
{$ENDREGION 'Compute_Function'}
{$REGION 'TV2Rect4'}


type
  TV2L = class;

  TV2Rect4 = record
  public
    LeftTop, RightTop, RightBottom, LeftBottom: TVec2;

    procedure Reset;
    function IsZero: Boolean;
    function BoundArea: TGeoFloat;
    function Area: TGeoFloat;
    function Rotation(Angle: TGeoFloat): TV2Rect4; overload;
    function Rotation(axis: TVec2; Angle: TGeoFloat): TV2Rect4; overload;
    function TransformToRect(box: TRectV2; Edge: TGeoFloat): TV2Rect4; overload;
    function TransformToRect(box: TRectV2; Angle, Edge: TGeoFloat): TV2Rect4; overload;
    function TransformToRect(box: TRectV2; axis: TVec2; Angle, Edge: TGeoFloat): TV2Rect4; overload;
    function Add(V: TVec2): TV2Rect4;
    function Sub(V: TVec2): TV2Rect4;
    function Mul(V: TVec2): TV2Rect4; overload;
    function Mul(V: TGeoFloat): TV2Rect4; overload;
    function Mul(X, Y: TGeoFloat): TV2Rect4; overload;
    function Div_(V: TVec2): TV2Rect4; overload;
    function Div_(V: TGeoFloat): TV2Rect4; overload;
    function MoveTo(Position: TVec2): TV2Rect4;
    function BoundRect: TRectV2;
    function BoundRectf: TRectf;
    function Centroid: TVec2;
    function Mid_LeftTop_RightTop: TVec2;
    property Mid_RightTop_LeftTop: TVec2 read Mid_LeftTop_RightTop;
    function Mid_LeftTop_RightBottom: TVec2;
    property Mid_RightBottom_LeftTop: TVec2 read Mid_LeftTop_RightBottom;
    function Mid_LeftTop_LeftBottom: TVec2;
    property Mid_LeftBottom_LeftTop: TVec2 read Mid_LeftTop_LeftBottom;
    function Mid_RightBottom_LeftBottom: TVec2;
    property Mid_LeftBottom_RightBottom: TVec2 read Mid_RightBottom_LeftBottom;
    function Mid_RightTop_RightBottom: TVec2;
    property Mid_RightBottom_RightTop: TVec2 read Mid_RightTop_RightBottom;
    function Transform(v2: TVec2): TV2Rect4; overload;
    function Transform(X, Y: TGeoFloat): TV2Rect4; overload;
    function Expands(Dist: TGeoFloat): TV2Rect4;
    function InHere(pt: TVec2): Boolean; overload;
    function InHere(R: TRectV2): Boolean; overload;
    function GetArrayVec2: TArrayVec2;
    function GetNear(pt: TVec2): TVec2;
    function GetNearLine(const V: TVec2; out lb, le: PVec2): TVec2; overload;
    function GetNearLine(const V: TVec2): TVec2; overload;
    function Projection(const sour, dest: TRectV2; const sourAxis, destAxis: TVec2; const sourAngle, destAngle: TGeoFloat): TV2Rect4; overload;
    function Projection(const sour, dest: TRectV2; sourAngle, destAngle: TGeoFloat): TV2Rect4; overload;
    function Projection(const sour, dest: TRectV2): TV2Rect4; overload;
    class function RebuildVertex(const buff: TArrayVec2): TV2Rect4; overload; static;
    class function RebuildVertex(const buff: TV2L): TV2Rect4; overload; static;
    class function Init(R: TRectV2): TV2Rect4; overload; static;
    class function Init(R: TRectV2; axis: TVec2; Ang: TGeoFloat): TV2Rect4; overload; static;
    class function Init(R: TRectV2; Ang: TGeoFloat): TV2Rect4; overload; static;
    class function Init(R: TRectf; Ang: TGeoFloat): TV2Rect4; overload; static;
    class function Init(R: TRect; Ang: TGeoFloat): TV2Rect4; overload; static;
    class function Init(R: TRect): TV2Rect4; overload; static;
    class function Init(CenPos: TVec2; width, height, Ang: TGeoFloat): TV2Rect4; overload; static;
    class function Init(width, height, Ang: TGeoFloat): TV2Rect4; overload; static;
    class function Init(width, height: TGeoFloat): TV2Rect4; overload; static;
    class function Init(): TV2Rect4; overload; static;
    class function Create(R: TRectV2): TV2Rect4; overload; static;
    class function Create(R: TRectV2; axis: TVec2; Ang: TGeoFloat): TV2Rect4; overload; static;
    class function Create(R: TRectV2; Ang: TGeoFloat): TV2Rect4; overload; static;
    class function Create(R: TRectf; Ang: TGeoFloat): TV2Rect4; overload; static;
    class function Create(R: TRect; Ang: TGeoFloat): TV2Rect4; overload; static;
    class function Create(R: TRect): TV2Rect4; overload; static;
    class function Create(CenPos: TVec2; width, height, Ang: TGeoFloat): TV2Rect4; overload; static;
    class function Create(width, height, Ang: TGeoFloat): TV2Rect4; overload; static;
    class function Create(width, height: TGeoFloat): TV2Rect4; overload; static;
    class function Create(): TV2Rect4; overload; static;
  end;

  TV2R4 = TV2Rect4;
  TArrayV2Rect4 = array of TV2Rect4;
  TArrayV2R4 = TArrayV2Rect4;
  PArrayV2R4 = ^TArrayV2R4;

  PV2Rect4 = ^TV2Rect4;
  PV2R4 = PV2Rect4;
  TV2Rect4List_ = TGenericsList<PV2Rect4>;

  TV2Rect4List = class(TV2Rect4List_)
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddRect(p: PV2Rect4); overload;
    procedure AddRect(r4: TV2Rect4); overload;

    procedure Remove(p: PV2Rect4);
    procedure Delete(index: Integer);
    procedure Clear;
  end;

  TV2R4List = TV2Rect4List;
{$ENDREGION 'TV2Rect4'}
{$REGION 'TVec2List'}
  TDeflectionPolygon = class;
  TDeflectionPolygonLines = class;

  TVec2_Buffer_ = TBigList<TVec2>;

  TVec2_Buffer = class(TVec2_Buffer_)
  private
    function Do_Sort_(var L, R: TVec2): Integer;
  public
    procedure Sort; // sort by X * Y
    function To_V2L: TV2L;
  end;

  TV2L = class(TCore_Object_Intermediate)
  private
    FList: TCore_List;
    FUserData: pointer;
    FUserObject: TCore_Object;
    function GetPoints(index: TGeoInt): PVec2;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddRandom(); overload;
    procedure AddRandom(rnd: TMT19937Random); overload;
    procedure Add(const X, Y: TGeoFloat); overload;
    procedure Add(const pt: TVec2); overload;
    procedure Add(pt: TPoint); overload;
    procedure Add(pt: TPointf); overload;
    procedure Add(v2l: TV2L); overload;
    procedure Add(R: TRectV2); overload;
    procedure Add(R: TRect); overload;
    procedure Add(R: TRectf); overload;
    procedure Add(R: TV2Rect4); overload;
    procedure Add(arry: TArrayV2Rect4); overload;
    procedure AddSubdivision(nbCount: TGeoInt; pt: TVec2); overload;
    procedure AddSubdivisionWithDistance(avgDist: TGeoFloat; pt: TVec2); overload;
    procedure AddCirclePoint(count_: Cardinal; axis: TVec2; dist_: TGeoFloat);
    procedure AddRectangle(R: TRectV2);
    procedure Insert(idx: TGeoInt; X, Y: TGeoFloat); overload;
    procedure Insert(idx: TGeoInt; pt: TVec2); overload;
    procedure Delete(idx: TGeoInt); overload;
    function Remove(p: PVec2): TGeoInt;
    procedure Clear; overload;
    function Count: TGeoInt; overload;
    procedure RemoveSame;
    procedure SwapData(dest: TV2L);
    procedure MoveDataTo(dest: TV2L);

    procedure Assign(source: TCore_Object);
    procedure AssignFromArrayV2(arry: TArrayVec2);

    function BuildArray: TArrayVec2;
    function BuildSplineSmoothInSideClosedArray: TArrayVec2;
    function BuildSplineSmoothOutSideClosedArray: TArrayVec2;
    function BuildSplineSmoothOpenedArray: TArrayVec2;

    function BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAxis, destAxis: TVec2; const sourAngle, destAngle: TGeoFloat): TArrayVec2; overload;
    function BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAngle, destAngle: TGeoFloat): TArrayVec2; overload;
    function BuildProjectionArray(const sour, dest: TRectV2): TArrayVec2; overload;
    function BuildProjectionArray(const dest: TRectV2): TArrayVec2; overload;

    procedure ProjectionTo(const sour, dest: TRectV2; const output: TDeflectionPolygon); overload;
    procedure ProjectionTo(const dest: TRectV2; const output: TDeflectionPolygon); overload;
    procedure ProjectionTo(const sour, dest: TRectV2; const output: TV2L); overload;
    procedure ProjectionTo(const dest: TRectV2; const output: TV2L); overload;

    procedure SaveToStream(stream: TMS64); overload;
    procedure LoadFromStream(stream: TMS64); overload;

    function BoundBox: TRectV2; overload;
    function BoundCentre: TVec2;
    function CircleRadius(Centroid_: TVec2): TGeoFloat; overload;
    function Centroid: TVec2; overload;
    function Area: TGeoFloat;

    function InHere(pt: TVec2): Boolean; overload;
    function InRect(R: TRectV2): Boolean;
    function Rect2Intersect(R: TRectV2): Boolean;

    procedure RotateAngle(axis: TVec2; Angle: TGeoFloat); overload;
    procedure Scale(Scale_: TGeoFloat); overload;

    procedure ConvexHull(output: TV2L); overload;
    procedure ConvexHull; overload;

    procedure SplineSmoothInSideClosed(output: TV2L); overload;
    procedure SplineSmoothInSideClosed; overload;
    procedure SplineSmoothOutSideClosed(output: TV2L); overload;
    procedure SplineSmoothOutSideClosed; overload;
    procedure SplineSmoothOpened(output: TV2L); overload;
    procedure SplineSmoothOpened; overload;

    procedure ExtractToBuff(var output: TArrayVec2); overload;
    procedure GiveListDataFromBuff(output: TArrayVec2); overload;

    function SumDistance: TGeoFloat;
    procedure InterpolationTo(count_: TGeoInt; output_: TV2L);

    procedure VertexReduction(Epsilon_: TGeoFloat); overload;
    procedure Reduction(Epsilon_: TGeoFloat); overload;

    function Line2Intersect(const lb, le: TVec2; ClosedPolyMode: Boolean): Boolean; overload;
    function Line2Intersect(const lb, le: TVec2; ClosedPolyMode: Boolean; output: TV2L): Boolean; overload;
    function Line2NearIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean; out idx1, idx2: TGeoInt; out IntersectPt: TVec2): Boolean; overload;

    procedure SortOfNear(const lb, le: TVec2); overload;
    procedure SortOfNear(const pt: TVec2); overload;

    procedure Reverse; overload;

    function GetNearLine(const pt: TVec2; const ClosedMode: Boolean; out lb, le: TGeoInt): TVec2; overload;
    function GetNearLine(const pt: TVec2; const ClosedMode: Boolean): TVec2; overload;
    function GetNearLine(const pt: TVec2; const ExpandDist: TGeoFloat): TVec2; overload;

    procedure CutLineBeginPtToIdx(const pt: TVec2; const toidx: TGeoInt);

    procedure Transform(X, Y: TGeoFloat); overload;
    procedure Transform(V: TVec2); overload;
    procedure Mul(X, Y: TGeoFloat); overload;
    procedure Mul(V: TVec2); overload;
    procedure Mul(V: TGeoFloat); overload;
    procedure FDiv(X, Y: TGeoFloat); overload;
    procedure FDiv(V: TVec2); overload;
    procedure FDiv(V: TGeoFloat); overload;

    property Points[index: TGeoInt]: PVec2 read GetPoints; default;
    function First: PVec2;
    function Last: PVec2;

    procedure ExpandDistanceAsList(ExpandDist: TGeoFloat; output: TV2L);
    procedure ExpandDistance(ExpandDist: TGeoFloat);
    procedure ExpandConvexHullAsList(ExpandDist: TGeoFloat; output: TV2L);

    function GetExpands(idx: TGeoInt; ExpandDist: TGeoFloat): TVec2;
    property Expands[idx: TGeoInt; ExpandDist: TGeoFloat]: TVec2 read GetExpands;

    property UserData: pointer read FUserData write FUserData;
    property UserObject: TCore_Object read FUserObject write FUserObject;
  end;

  TVec2_Pool = TV2L;
  TVec2Pool = TV2L;
{$ENDREGION 'TVec2List'}
{$REGION 'PolygonGraph'}

  TLines = class(TV2L)
  end;

  TLinesArray = array of TLines;

  TLinesList_Decl = TGenericsList<TLines>;

  TLinesList = class(TLinesList_Decl)
  public
    AutoFree: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure Remove(obj: TLines);
    procedure Delete(index: TGeoInt);
    procedure Clear;
  end;

  T2DPointList = TV2L;
  T2DPolygonGraph = class;

  T2DPolygon = class(TLines)
  public
    Owner: T2DPolygonGraph;
    constructor Create;
    destructor Destroy; override;
  end;

  T2DPolygonList = TGenericsList<T2DPolygon>;
  T2DPolygonArray = array of T2DPolygon;
  TCollapses = T2DPolygonArray;

  T2DPolygonGraph = class(TCore_Object_Intermediate)
  public
    Surround: T2DPolygon;
    Collapses: TCollapses;

    constructor Create;
    destructor Destroy; override;

    procedure Assign(source: TCore_Object);

    function NewCollapse(): T2DPolygon;
    procedure AddCollapse(polygon: T2DPolygon);
    procedure Clear;
    function CollapsesCount(): TGeoInt;
    function GetBands(const index: TGeoInt): T2DPolygon;
    property Bands[const index: TGeoInt]: T2DPolygon read GetBands;
    procedure Remove(p: PVec2); overload;
    procedure FreeAndRemove(polygon: T2DPolygon); overload;
    procedure RemoveNullPolygon();
    function Total: TGeoInt;
    function BuildArray: TArray2DPoint;
    function BuildPArray: TArrayPVec2;
    function ExistsPVec(p: PVec2): Boolean;
    procedure RotateAngle(axis: TVec2; Angle: TGeoFloat);
    procedure Scale(Scale_: TGeoFloat);
    procedure ProjectionTo(const sour, dest: TRectV2; const output: T2DPolygonGraph); overload;
    procedure ProjectionTo(const dest: TRectV2; const output: T2DPolygonGraph); overload;
    function InHere(pt: TVec2): Boolean;
    function InSurround(pt: TVec2): Boolean;
    function InCollapse(pt: TVec2): Boolean;
    function Pick(pt: TVec2): T2DPolygon;
    function BoundBox: TRectV2;
    function CollapseBounds: TRectV2Array;
    function Line2Intersect(const lb, le: TVec2; output: T2DPolygon): Boolean;
    function GetNearLine(const pt: TVec2; out output: T2DPolygon; out lb, le: TGeoInt): TVec2;
    procedure Transform(X, Y: TGeoFloat); overload;
    procedure Transform(V: TVec2); overload;
    procedure Mul(X, Y: TGeoFloat); overload;
    procedure Mul(V: TVec2); overload;
    procedure Mul(V: TGeoFloat); overload;
    procedure FDiv(X, Y: TGeoFloat); overload;
    procedure FDiv(V: TVec2); overload;
    procedure VertexReduction(Epsilon_: TGeoFloat); overload;
    procedure Reduction(Epsilon_: TGeoFloat); overload;
    procedure SaveToStream(stream: TMS64);
    procedure LoadFromStream(stream: TMS64);
    procedure Save_To_Bytes(var buff: TBytes);
    procedure Load_From_Bytes(var buff: TBytes);
  end;

  T2DPolygonGraphList_Decl = TGenericsList<T2DPolygonGraph>;

  T2DPolygonGraphList = class(T2DPolygonGraphList_Decl)
  end;

{$ENDREGION 'PolygonGraph'}
{$REGION 'DeflectionPolygon'}

  TDeflectionPolygonVec = record
    Owner: TDeflectionPolygon;
    Angle: TGeoFloat;
    Dist: TGeoFloat;
  end;

  PDeflectionPolygonVec = ^TDeflectionPolygonVec;

  TExpandMode = (emConvex, emConcave);

  TDeflectionPolygon = class(TCore_Object_Intermediate)
  private
    FList: TCore_List;
    FName: TPascalString;
    FClassifier: TPascalString;
    FScale: TGeoFloat;
    FAngle: TGeoFloat;
    FMaxRadius: TGeoFloat;
    FPosition: TVec2;
    FExpandMode: TExpandMode;

    FUserDataObject: TCore_Object;
    FUserData: pointer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset; overload;

    procedure Assign(source: TCore_Object);

    function BuildArray: TArrayVec2;
    function BuildSplineSmoothInSideClosedArray: TArrayVec2;
    function BuildSplineSmoothOutSideClosedArray: TArrayVec2;
    function BuildSplineSmoothOpenedArray: TArrayVec2;
    function BuildProjectionSplineSmoothInSideClosedArray(const sour, dest: TRectV2): TArrayVec2;
    function BuildProjectionSplineSmoothOutSideClosedArray(const sour, dest: TRectV2): TArrayVec2;

    function BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAxis, destAxis: TVec2; const sourAngle, destAngle: TGeoFloat): TArrayVec2; overload;
    function BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAngle, destAngle: TGeoFloat): TArrayVec2; overload;
    function BuildProjectionArray(const sour, dest: TRectV2): TArrayVec2; overload;
    function BuildProjectionArray(const dest: TRectV2): TArrayVec2; overload;

    procedure ProjectionTo(const sour, dest: TRectV2; const output: TDeflectionPolygon); overload;
    procedure ProjectionTo(const dest: TRectV2; const output: TDeflectionPolygon); overload;
    procedure ProjectionTo(const sour, dest: TRectV2; const output: TV2L); overload;
    procedure ProjectionTo(const dest: TRectV2; const output: TV2L); overload;

    procedure AddPoint(pt: TVec2); overload;
    procedure AddPoint(X, Y: TGeoFloat); overload;
    procedure AddRectangle(R: TRectV2); overload;
    procedure AddCirclePoint(count_: Cardinal; axis: TVec2; dist_: TGeoFloat);
    procedure Add(angle_, dist_: TGeoFloat); overload;
    procedure AddRectangle(R: TV2Rect4); overload;
    procedure AddRectangle(arry: TArrayV2Rect4); overload;
    procedure Insert(idx: TGeoInt; angle_, dist_: TGeoFloat); overload;
    procedure InsertPoint(idx: TGeoInt; pt: TVec2); overload;
    procedure Delete(idx: TGeoInt); overload;
    procedure Clear; overload;
    function Count: TGeoInt; overload;
    procedure CopyPoly(pl: TDeflectionPolygon; AReversed: Boolean);
    procedure CopyExpandPoly(pl: TDeflectionPolygon; AReversed: Boolean; Dist: TGeoFloat);
    procedure Reverse;
    function ScaleBeforeDistance: TGeoFloat;
    function ScaleAfterDistance: TGeoFloat;

    procedure RemoveSame;

    { * auto build opt from convex hull point * }
    procedure ConvexHullFrom(From_: TV2L); overload;

    { rebuild }
    procedure Rebuild(pl: TV2L; Scale_: TGeoFloat; angle_: TGeoFloat; ExpandMode_: TExpandMode; Position_: TVec2); overload;
    procedure Rebuild(pl: TV2L; reset_: Boolean); overload;
    procedure Rebuild; overload;
    procedure Rebuild(Scale_: TGeoFloat; angle_: TGeoFloat; ExpandMode_: TExpandMode; Position_: TVec2); overload;
    procedure Rebuild_From_Projection(Source_Box, Dest_Box: TRectV2); overload;

    function BoundBox: TRectV2; overload;
    function Centroid: TVec2; overload;
    function Area: TGeoFloat;

    function InHere(pt: TVec2): Boolean; overload;
    function InHere(ExpandDistance_: TGeoFloat; pt: TVec2): Boolean; overload;

    { * line intersect * }
    function LineIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean; overload;
    function LineIntersect(ExpandDistance_: TGeoFloat; const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean; overload;
    function LineIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean;
      out idx1, idx2: TGeoInt; out IntersectPt: TVec2): Boolean; overload;
    function LineIntersect(ExpandDistance_: TGeoFloat; const lb, le: TVec2; const ClosedPolyMode: Boolean;
      out idx1, idx2: TGeoInt; out IntersectPt: TVec2): Boolean; overload;

    { * sample line intersect * }
    function SimpleLineIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean; overload;

    { * get minimum point from Polygon * }
    function GetNearLine(const pt: TVec2; const ClosedPolyMode: Boolean; out lb, le: TGeoInt): TVec2; overload;
    function GetNearLine(ExpandDistance_: TGeoFloat; const pt: TVec2; const ClosedPolyMode: Boolean; out lb, le: TGeoInt): TVec2; overload;

    function Collision2Circle(cp: TVec2; R: TGeoFloat; ClosedPolyMode: Boolean): Boolean; overload;
    function Collision2Circle(cp: TVec2; R: TGeoFloat; ClosedPolyMode: Boolean; OutputLine: TDeflectionPolygonLines): Boolean; overload;
    function Collision2Circle(ExpandDistance_: TGeoFloat; cp: TVec2; R: TGeoFloat; ClosedPolyMode: Boolean; OutputLine: TDeflectionPolygonLines): Boolean; overload;

    function PolygonIntersect(Poly_: TDeflectionPolygon): Boolean; overload;
    function PolygonIntersect(vl_: TV2L): Boolean; overload;

    function LerpToEdge(pt: TVec2; ProjDistance_, ExpandDistance_: TGeoFloat; FromIdx, toidx: TGeoInt): TVec2;

    property Scale: TGeoFloat read FScale write FScale;
    property Angle: TGeoFloat read FAngle write FAngle;
    property Position: TVec2 read FPosition write FPosition;
    function GetDeflectionPolygon(index: TGeoInt): PDeflectionPolygonVec;
    property DeflectionPolygon[index: TGeoInt]: PDeflectionPolygonVec read GetDeflectionPolygon;
    property MaxRadius: TGeoFloat read FMaxRadius;
    property ExpandMode: TExpandMode read FExpandMode write FExpandMode;
    // user define
    property Name: TPascalString read FName write FName;
    property Classifier: TPascalString read FClassifier write FClassifier;

    function GetPoint(idx: TGeoInt): TVec2;
    procedure SetPoint(idx: TGeoInt; Value: TVec2);
    property Points[idx: TGeoInt]: TVec2 read GetPoint write SetPoint; default;
    function FirstPoint: TVec2;
    function LastPoint: TVec2;

    function GetExpands(idx: TGeoInt; ExpandDist: TGeoFloat): TVec2;
    property Expands[idx: TGeoInt; ExpandDist: TGeoFloat]: TVec2 read GetExpands;

    procedure SaveToStream(stream: TMS64); overload;
    procedure LoadFromStream(stream: TMS64); overload;

    property UserDataObject: TCore_Object read FUserDataObject write FUserDataObject;
    property UserData: pointer read FUserData write FUserData;
  end;

  TDeflectionPolygonList_ = TGenericsList<TDeflectionPolygon>;

  TDeflectionPolygonList = class(TDeflectionPolygonList_)
  public
    AutoFree: Boolean;
    BackgroundBox: TRectV2;
    constructor Create;
    destructor Destroy; override;
    procedure Remove(obj: TDeflectionPolygon);
    procedure Delete(index: TGeoInt);
    procedure Clear;
    function BoundBox: TRectV2;
    procedure Rebuild_From_New_Background_Box(NewBox: TRectV2);
    function FindPolygon(Name: TPascalString): TDeflectionPolygon;
    function MakePolygonName(Name: TPascalString): TPascalString;
    procedure SaveToStream(stream: TCore_Stream);
    procedure LoadFromStream(stream: TCore_Stream);
    procedure LoadFromBase64(const buff: TPascalString);
  end;

  TPoly = TDeflectionPolygon;

  TDeflectionPolygonLine = record
    buff: array [0 .. 1] of TVec2;
    OwnerDeflectionPolygon: TDeflectionPolygon;
    OwnerDeflectionPolygonIndex: array [0 .. 1] of TGeoInt;
    index: TGeoInt;
  public
    procedure SetLocation(const lb, le: TVec2);
    function ExpandPoly(ExpandDist: TGeoFloat): TDeflectionPolygonLine;
    function length: TGeoFloat;
    function MinimumDistance(const pt: TVec2): TGeoFloat; overload;
    function MinimumDistance(ExpandDist: TGeoFloat; const pt: TVec2): TGeoFloat; overload;
    function ClosestPointFromLine(const pt: TVec2): TVec2; overload;
    function ClosestPointFromLine(ExpandDist: TGeoFloat; const pt: TVec2): TVec2; overload;
    function MiddlePoint: TVec2;
  end;

  PDeflectionPolygonLine = ^TDeflectionPolygonLine;

  TDeflectionPolygonLines = class(TCore_Persistent_Intermediate)
  private
    FList: TCore_List;
    FUserData: pointer;
    FUserObject: TCore_Object;
    function GetItems(index: TGeoInt): PDeflectionPolygonLine;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(source: TCore_Persistent); override;

    property Items[index: TGeoInt]: PDeflectionPolygonLine read GetItems; default;
    function Add(V: TDeflectionPolygonLine): TGeoInt; overload;
    function Add(lb, le: TVec2): TGeoInt; overload;
    function Add(lb, le: TVec2; idx1, idx2: TGeoInt; polygon: TDeflectionPolygon): TGeoInt; overload;
    function Count: TGeoInt;
    procedure Clear;
    procedure Delete(index: TGeoInt);

    function NearLine(const ExpandDist: TGeoFloat; const pt: TVec2): PDeflectionPolygonLine;
    function FarLine(const ExpandDist: TGeoFloat; const pt: TVec2): PDeflectionPolygonLine;

    procedure SortOfNear(const pt: TVec2); overload;
    procedure SortOfFar(const pt: TVec2); overload;

    property UserData: pointer read FUserData write FUserData;
    property UserObject: TCore_Object read FUserObject write FUserObject;
  end;
{$ENDREGION 'DeflectionPolygon'}
{$REGION 'TriangleList'}

  TTriangleList_Decl = TGenericsList<PTriangle>;

  TTriangleList = class(TTriangleList_Decl)
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTri(T_: TTriangle);
    procedure Remove(p: PTriangle);
    procedure Delete(index: TGeoInt);
    procedure Clear;

    procedure BuildTriangle(polygon: TV2L); overload;
    procedure BuildTriangle(polygon: TV2L; MinAngle, MinSegmentLength, MaxElementSize: TGeoFloat); overload;
    procedure BuildTriangle(polygon: T2DPolygonGraph); overload;
    procedure BuildTriangle(polygon: T2DPolygonGraph; MinAngle, MinSegmentLength, MaxElementSize: TGeoFloat); overload;
  end;
{$ENDREGION 'TriangleList'}
{$REGION 'RectPacking'}

  TRectPackData = record
    Rect: TRectV2;
    error: Boolean;
    Data1: pointer;
    Data2: TCore_Object;
    ID: TGeoInt;
  end;

  PRectPackData = ^TRectPackData;

  TRectPackData_List = TGenericsList<PRectPackData>;

  TRectPacking_Style = (rsDynamic, rsL2R, rsL2R_Sorted, rsT2B, rsT2B_Sorted);

  TRectPacking = class(TCore_Persistent_Intermediate)
  private
    FList: TRectPackData_List;
    function Compute_XY_Pack(width, height: TGeoFloat; var X, Y: TGeoFloat): Boolean;
    function GetItems(const index: TGeoInt): PRectPackData;
  public
    Style: TRectPacking_Style;
    MaxWidth, MaxHeight: TGeoFloat;
    Margins: TGeoFloat;
    UserToken: TPascalString;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Data1: pointer; Data2: TCore_Object; X, Y, width, height: TGeoFloat); overload;
    procedure Add(const X, Y, width, height: TGeoFloat); overload;
    procedure Add(Data1: pointer; Data2: TCore_Object; R: TRectV2); overload;
    procedure Add(Data1: pointer; Data2: TCore_Object; width, height: TGeoFloat); overload;
    function Data1Exists(const Data1: pointer): Boolean;
    function Data2Exists(const Data2: TCore_Object): Boolean;
    function Count: TGeoInt;
    property Items[const index: TGeoInt]: PRectPackData read GetItems; default;

    // build box from style
    procedure Build;

    // build dynamic box sort
    procedure Build_Dynamic(SpaceWidth, SpaceHeight: TGeoFloat); overload;
    procedure Build_Dynamic; overload;

    // build left to right sort
    procedure Build_Left_To_Right(resort_width_: Boolean); overload;
    procedure Build_Left_To_Right(); overload;

    // build top to bottom sort
    procedure Build_Top_To_Bottom(resort_height_: Boolean); overload;
    procedure Build_Top_To_Bottom(); overload;

    function GetBoundsBox(): TRectV2;
  end;
{$ENDREGION 'RectPacking'}
{$REGION 'Nearest_Box_Tool'}

  TNearest_Box_List = class;

  PNearest_Box_Data = ^TNearest_Box_Data;

  TNearest_Box_IoU = record
    p1, p2: PNearest_Box_Data;
    Intersect_Box: TRectV2;
    IoU, R1A, R2A, RA: TGeoFloat;
  end;

  TNearest_Box_IoU_Tool_ = TBigList<TNearest_Box_IoU>;

  TNearest_Box_IoU_Tool = class(TNearest_Box_IoU_Tool_)
  public
    function Check_IoU(p1, p2: PNearest_Box_Data): Boolean;
  end;

  TNearest_Box_Data = record
    R: PRectV2;
    Free_R: Boolean;
    ID: Integer;
    UserData: pointer;
    UserObject: TCore_Object;
    Nearest_Box: TNearest_Box_List;
    procedure Init();
  end;

  TNearest_Box_Tool_ = TBigList<TNearest_Box_Data>;

  TNearest_Box_List_ = TBigList<PNearest_Box_Data>;

  TNearest_Box_List = class(TNearest_Box_List_)
  private
    procedure Update_Convex_Hull(Extract_Distance_: TGeoFloat);
  public
    ID: Integer;
    Convex_Hull: TV2L;
    constructor Create(ID_: Integer);
    destructor Destroy; override;
  end;

  TNearest_Box_Group_ = TBig_Hash_Pair_Pool<Integer, TNearest_Box_List>;

  TNearest_Box_Group = class(TNearest_Box_Group_)
  public
    procedure DoFree(var Key: Integer; var Value: TNearest_Box_List); override;
  end;

  TNearest_Box_Tool = class(TNearest_Box_Tool_)
  private
    function Do_Sort_Group(var L, R: TNearest_Box_Group_.PPair_Pool_Value__): Integer;
  public
    Nearest_Group: TNearest_Box_Group;
    IoU_Tool: TNearest_Box_IoU_Tool;
    constructor Create;
    destructor Destroy; override;
    procedure DoFree(var Data: TNearest_Box_Data); override;
    function Add_Box(R_: PRectV2): PNearest_Box_Data; overload;
    function Add_Box(R_: PRectV2; UserData: pointer; UserObject: TCore_Object): PNearest_Box_Data; overload;
    function Add_Box(R_: TRectV2; UserData: pointer; UserObject: TCore_Object): PNearest_Box_Data; overload;
    function Add_Box(R_: TRect; UserData: pointer; UserObject: TCore_Object): PNearest_Box_Data; overload;
    function Get_Box_Group(R_: PRectV2): TNearest_Box_List;
    function Get_UserData_Group(UserData: pointer): TNearest_Box_List;
    function Get_UserObject_Group(UserObject: TCore_Object): TNearest_Box_List;
    function Compute_Nearest_Box(Nearest_Distance_, Convex_Hull_Distance_: TGeoFloat): Integer;
  end;

{$ENDREGION 'Nearest_Box_Tool'}
{$REGION 'Hausdorf'}

  THausdorf = class(TCore_Object_Intermediate)
  private type
    PNode = ^TNode;

    TNode = record
      Prev, Next: PNode;
      Data: TVec2;
    end;

    { An implementation of a Linked List data structure. Fields that are used in it:
      'head', 'tail' - pointers referring to the first and las elements in the list
      'Num' - number of elements stored in the list
      'looped' - indicates, if the element following by the tail is the head of the list }
    PLinkedList = ^TLinkedList;

    TLinkedList = record
      Head, Tail: PNode;
      Num: TGeoInt;
      Looped: Boolean;
    end;

    TNodeList = TGenericsList<PNode>;
    TLinkList = TGenericsList<PLinkedList>;
  private
    FPolygon1, FPolygon2, FOutput: PLinkedList;
    FRoundKOEF: TGeoFloat; { A threshold. This value is used for imprecise comparisons. }

    { temp list }
    NodeList: TNodeList;
    LinkList: TLinkList;
  private
    procedure NewNode(var p: PNode);
    procedure NewLink(var p: PLinkedList);
    {
      The procedure creates a TVec2 using the passed coordinates and wraps it into passed wrapper.
      'wrapper' - the wrapper to wrap the TVec2 in.
      'x', 'y' - the coordinates of the TVec2.
    }
    procedure WrapVector(var wrapper: PNode; const X, Y: TGeoFloat);
    {
      The procedure THausdorf.initialises the list: allocates the memory for it,
      sets the 'head' and 'tail' fields to 'nil', sets zero as a starting value for the 'num' field.
      'target' - the field to be initialised.
    }
    procedure InitList(var target: PLinkedList);
    {
      The procedure THausdorf.initialises the list, reads the polygon from the specified input stream, and writes them to the specified list.
      The data format is the following. The single number on the first line specifies the number of points in the polygon.
      Each of the following lines contains two numbers with an x- and y-coordinates of the point. The list is looped after data are read.
      'target' - the list to write the data to.
      'source' - the input stream to read from.
    }
    procedure InitAndReadPolygon(var target: PLinkedList; const source: TV2L);
    {
      The function provides an access to the list's elements by their index. If the list is looped, the index could exceed the list's num.
      The index starts from 0.
      'target' - the list to get element from.
      'n' - the index of an element.
    }
    function Get(var target: PLinkedList; const n_: TGeoInt): PNode;
    {
      The procedure THausdorf.gets the longest vectors form the source list and puts them to the target list.
      Note: the comparison is performed impreciesly, using the FRoundKOEF threshold.
    }
    procedure GetMax(var target, source: PLinkedList);
    {
      The procedure THausdorf.gets the shortest vectors form the source list and puts them to the target list.
      Note: the comparison is performed impreciesly, using the FRoundKOEF threshold.
    }
    procedure GetMin(var target, source: PLinkedList);
    {
      The procedure THausdorf.adds wrapped items to the specified list. The list should be initialised prior to calling the procedure.
      'target' - the list to add item to.
      'item' - the item to be added.
    }
    procedure AddNodeTo(var target: PLinkedList; const item: PNode);
    {
      The procedure THausdorf.adds a TVec2 to the specified list. The list should be initialised prior to calling the procedure.
      'target' - the list to add item to.
      'p' - the TVec2 to be added to the list.
    }
    procedure AddTo(var target: PLinkedList; p: TVec2); overload;
    {
      The procedure THausdorf.adds a TVec2 specified by its coordinates to the specified list. The list should be initialised prior to calling the procedure.
      'target' - the list to add item to.
      'x', 'y' - the coordinates of the TVec2 to be added.
    }
    procedure AddTo(var target: PLinkedList; X, Y: TGeoFloat); overload;
    { The procedure THausdorf.adds the TVec2 to the specified list considering its order defined by the 'compare()' function. }
    procedure AddToQ(const target: PLinkedList; const p: TVec2);
    { The function THausdorf.defining the order of the vectors (based on the angle to the OX-axis). }
    function Compare(const p1, p2: TVec2): TGeoInt;
    {
      The functions tests whether the point belongs to the internal area of the polygon.
      Note: the method used in this procedures returns the valid answer if the polygon is convex. Otherwise it is not applicable.
      'pol' - the polygon.
      'p' - the point.
    }
    function Contains(const pol: PLinkedList; const p: TVec2): Boolean;
    { The procedure THausdorf.copies the source list to the target list removing the duplicates of the items in the source list. }
    procedure DeleteCopies(var target, source: PLinkedList);
    { The procedure THausdorf.calculates the Hausdorff distance from the first polygon to from second one. The data are stored in the specified list. }
    procedure HausdorfDistanceVectors(var target, Polygon1_, Polygon2_: PLinkedList);
    {
      The function THausdorf.returns true if the specified TVec2 is already present in the specified list.
      'target' - the list to be tested.
      'p' - the item to look for.
    }
    function IsInList(const target: PLinkedList; const p: TVec2): Boolean;
    { The function THausdorf.defines whether the current position of two convex polygons is optimal or not. }
    function IsOptimal(var distVecs: PLinkedList): Boolean;
    {
      The procedure THausdorf.loops the list. It means, that it makes the head item be the next item after the tail.
      'target' - the list to be looped.
    }
    procedure LoopTheList(var target: PLinkedList);
    {
      The procedure THausdorf.
      calculates the distance vectors from the point to the edges of the polygon.
      The results are stored in the specified list.
    }
    procedure PointPolygonDistanceVectors(var target, pol: PLinkedList; const p: TVec2);
    {
      The function THausdorf.calculates the distance TVec2 between a point and a section.
      'a', 'b' - the ends of the section.
      'p' - the point.
    }
    function PointSectionDistanceVector(const a, b, p: TVec2): TVec2;
    {
      The procedure THausdorf.
      calculates the shortest distance vectors from each vertex of the first polygon to the second polygon.
      The data are stored to the specified list.
    }
    procedure PolygonPolygonDistanceVectors(var target, Polygon1_, Polygon2_: PLinkedList);
    {
      The function THausdorf.calculates the pseudo scalar product of two vectors.
      Note: pseudo scalar product is defined as a product of vectors' lengths multiplied by the sinus of the angle between the vectors.
      'a', 'b' - the vectors to be multiplied.
    }
    function PseudoScalarProduct(const a, b: TVec2): TGeoFloat;
    { The function THausdorf.
      determines the quadrant which the point belongs to. Zero is considered as point of the first quadrant.
      Each half-axis belongs to the quadrant to its left.
    }
    function Quadrant(const p: TVec2): TGeoInt;
    {
      The procedure THausdorf.
      puts the vectors from the source list to the target list in the way, that the angle,
      closed to the OX-axis is non-decreasing.
    }
    procedure SortByAngle(var target, source: PLinkedList);
    { The function THausdorf.returns the normalised TVec2. }
    function Normalise(const vec: TVec2): TVec2;
    {
      The function THausdorf.calculates the scalar product of the vectors.
      'a', 'b' - the vectors to be multiplied.
    }
    function ScalarProduct(const a, b: TVec2): TGeoFloat;
  public
    class function Compute(const poly1_, poly2_: TV2L; const detail_: TGeoInt; const ROUND_KOEF: TGeoFloat): TGeoFloat; overload;
    class function Compute(
      const poly1_: TV2L; const poly1_b, poly1_e: Integer;
      const poly2_: TV2L; const poly2_b, poly2_e: Integer;
      const detail_: TGeoInt; const ROUND_KOEF: TGeoFloat): TGeoFloat; overload;

    constructor Create(const poly1_, poly2_: TV2L; const detail_: TGeoInt; const ROUND_KOEF: TGeoFloat);
    destructor Destroy; override;

    function HausdorffReached(): TArrayVec2;
    function HausdorffDistance(): TGeoFloat;
    function polygonsIsOptimal(): Boolean;

    class procedure TestAndPrint(const poly1_, poly2_: TV2L);
    class procedure Test1();
    class procedure Test2();
  end;
{$ENDREGION 'Hausdorf'}


function ArrayVec2(const V: TArrayVec2): TArrayVec2; overload;
function ArrayVec2(const R: TRectV2): TArrayVec2; overload;
function ArrayVec2(const R: TV2Rect4): TArrayVec2; overload;
function ArrayVec2(const L: TLineV2): TArrayVec2; overload;
function ArrayVec2(const t: TTriangle): TArrayVec2; overload;
function ArrayBoundRect(arry: TArrayVec2): TRectV2; overload;
function ArrayBoundRect(arry: TArrayRectV2): TRectV2; overload;
function ArrayBoundRect(arry: TArrayV2Rect4): TRectV2; overload;
function RectProjection(const sour, dest: TRectV2; const sour_r4: TV2R4): TV2R4; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM} overload;

const
  XPoint: T2DPoint = (1, 0);
  YPoint: T2DPoint = (0, 1);
  NULLPoint: T2DPoint = (0, 0);
  NULLVec2: T2DPoint = (0, 0);
  ZeroPoint: T2DPoint = (0, 0);
  ZeroVec2: T2DPoint = (0, 0);
  NULLRect: TRectV2 = ((0, 0), (0, 0));
  ZeroRect: TRectV2 = ((0, 0), (0, 0));
  NULLRectV2: TRectV2 = ((0, 0), (0, 0));
  ZeroRectV2: TRectV2 = ((0, 0), (0, 0));
  ZeroTriangle: TTriangle = ((0, 0), (0, 0), (0, 0));
  RightHandSide = -1;
  LeftHandSide = +1;
  CollinearOrientation = 0;
  AboveOrientation = +1;
  BelowOrientation = -1;
  CoplanarOrientation = 0;

implementation

uses Z.UnicodeMixedLib, Z.Geometry3D, Z.Geometry.Low, Z.DFE, Z.Status;

const
  C_Epsilon = 1.0E-12;
  Zero = 0.0;
  PIDiv180 = 0.017453292519943295769236907684886;

{$I Z.Geometry.Split.Header.inc}
{$I Z.Geometry.Split.Body.inc}
{$IFDEF FPC}


function Pointf(X, Y: TGeoFloat): TPointf;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Rectf(Left, Top, Right, Bottom: TGeoFloat): TRectf;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

{$ENDIF}


function Compute_PI(Num: Integer): Double;
  function f(const X: Double): Double; inline;
  begin
    f := 4 / (1 + X * X);
  end;

var
  i: Integer;
  Aux: Double;
begin
  Aux := 0;
  for i := 0 to Num - 1 do
    begin
      Aux := Aux + f(i / Num) + f((i + 1) / Num) + 4 * f((2 * i + 1) / (2 * Num));
    end;
  Result := Aux / (6 * Num);
end;

function FAbs(const V: Single): Single;
begin
  if V < 0 then
      Result := -V
  else
      Result := V;
end;

function FAbs(const V: Double): Double;
begin
  if V < 0 then
      Result := -V
  else
      Result := V;
end;

function FAbs(const v2: TVec2): TVec2;
begin
  Result[0] := if_(v2[0] < 0, -v2[0], v2[0]);
  Result[1] := if_(v2[1] < 0, -v2[1], v2[1]);
end;

function Clamp(const Value_, Min_, Max_: TGeoFloat): TGeoFloat;
begin
  Result := umlClamp(Value_, Min_, Max_);
end;

function MaxF(const v1, v2: TGeoFloat): TGeoFloat;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function MaxF(const V: TVec2): TGeoFloat;
begin
  Result := MaxF(V[0], V[1]);
end;

function MaxF(const R: TRectV2): TGeoFloat;
begin
  Result := MaxF(MaxF(R[0]), MaxF(R[1]));
end;

function MinF(const v1, v2: TGeoFloat): TGeoFloat;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function MinF(const V: TVec2): TGeoFloat;
begin
  Result := MinF(V[0], V[1]);
end;

function MinF(const R: TRectV2): TGeoFloat;
begin
  Result := MinF(MinF(R[0]), MinF(R[1]));
end;

function CompareFloat(const f1, f2, Epsilon_: TGeoFloat): ShortInt;
begin
  if IsEqual(f1, f2, Epsilon_) then
      Result := 0
  else if f1 < f2 then
      Result := -1
  else
      Result := 1;
end;

function CompareFloat(const f1, f2: TGeoFloat): ShortInt;
begin
  if IsEqual(f1, f2, C_Epsilon) then
      Result := 0
  else if f1 < f2 then
      Result := -1
  else
      Result := 1;
end;

function CompareGeoInt(const g1, g2: TGeoInt): ShortInt;
begin
  if g1 = g2 then
      Result := 0
  else if g1 < g2 then
      Result := -1
  else
      Result := 1;
end;

function MakeVec2(const X, Y: TGeoFloat): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function MakeVec2(const X, Y: TGeoInt): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function MakePoint(const X, Y: TGeoFloat): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function MakePoint(const X, Y: TGeoInt): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function MakePoint(const pt: TVec2): TPoint;
begin
  Result.X := Round(pt[0]);
  Result.Y := Round(pt[1]);
end;

function Point2Point(const pt: TVec2): TPoint;
begin
  Result.X := Round(pt[0]);
  Result.Y := Round(pt[1]);
end;

function Point2Pointf(const pt: TVec2): TPointf;
begin
  Result.X := pt[0];
  Result.Y := pt[1];
end;

function PointMake(const X, Y: TGeoFloat): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function PointMake(const pt: TPoint): TVec2;
begin
  Result[0] := pt.X;
  Result[1] := pt.Y;
end;

function PointMake(const pt: TPointf): TVec2;
begin
  Result[0] := pt.X;
  Result[1] := pt.Y;
end;

function Make2DPoint(const X, Y: TGeoFloat): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function Make2DPoint(const X, Y: TGeoInt): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function Make2DPoint(const pt: TPoint): TVec2;
begin
  Result[0] := pt.X;
  Result[1] := pt.Y;
end;

function Make2DPoint(const pt: TPointf): TVec2;
begin
  Result[0] := pt.X;
  Result[1] := pt.Y;
end;

function vec2(const p: PVec2): TVec2;
begin
  Result := p^;
end;

function vec2(const f: TGeoFloat): TVec2;
begin
  Result[0] := f;
  Result[1] := f;
end;

function vec2(const X, Y: TGeoFloat): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function vec2(const X, Y: TGeoInt): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function vec2(const X, Y: Int64): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function vec2(const pt: TPoint): TVec2;
begin
  Result[0] := pt.X;
  Result[1] := pt.Y;
end;

function vec2(const pt: TPointf): TVec2;
begin
  Result[0] := pt.X;
  Result[1] := pt.Y;
end;

function LineV2(const x1, y1, x2, y2: TGeoFloat): TLineV2;
begin
  Result[0, 0] := x1;
  Result[0, 1] := y1;
  Result[1, 0] := x2;
  Result[1, 1] := y2;
end;

function LineV2(const lb, le: TVec2): TLineV2;
begin
  Result[0] := lb;
  Result[1] := le;
end;

function LineV2(const lb, le: TPoint): TLineV2;
begin
  Result[0] := vec2(lb);
  Result[1] := vec2(le);
end;

function LineV2(const L: TLineV2_P): TLineV2;
begin
  Result[0] := L[0]^;
  Result[1] := L[1]^;
end;

function LineV2(const L: PLineV2_P): TLineV2;
begin
  Result[0] := L^[0]^;
  Result[1] := L^[1]^;
end;

function LineV2(const L: PLineV2): TLineV2;
begin
  Result := L^;
end;

function RoundVec2(const V: TVec2): TVec2;
begin
  Result[0] := Round(V[0]);
  Result[1] := Round(V[1]);
end;

function MakePointf(const pt: TVec2): TPointf;
begin
  Result.X := pt[0];
  Result.Y := pt[1];
end;

function IsZero(const V: TGeoFloat): Boolean;
begin
  Result := IsEqual(V, 0, C_Epsilon);
end;

function IsZero(const pt: TVec2): Boolean;
begin
  Result := IsEqual(pt[0], 0, C_Epsilon) and IsEqual(pt[1], 0, C_Epsilon);
end;

function IsZero(const R: TRectV2): Boolean;
begin
  Result := IsZero(R[0]) and IsZero(R[1]);
end;

function IsNan(const pt: TVec2): Boolean;
begin
  Result := IsNan(pt[0]) or IsNan(pt[1]);
end;

function IsNan(const X, Y: TGeoFloat): Boolean;
begin
  Result := IsNan(X) or IsNan(Y);
end;

function HypotX(const X, Y: TGeoFloat): TGeoFloat;
{
  formula: Sqrt(X*X + Y*Y)
  implemented as: |Y|*Sqrt(1+Sqr(X/Y)), |X| < |Y| for greater precision
}
var
  Temp, TempX, TempY: TGeoFloat;
begin
  TempX := FAbs(X);
  TempY := FAbs(Y);
  if TempX > TempY then
    begin
      Temp := TempX;
      TempX := TempY;
      TempY := Temp;
    end;
  if TempX = 0 then
      Result := TempY
  else // TempY > TempX, TempX <> 0, so TempY > 0
      Result := TempY * Sqrt(1 + Sqr(TempX / TempY));
end;

function PointNorm(const V: TVec2): TGeoFloat;
begin
  Result := V[0] * V[0] + V[1] * V[1];
end;

function PointNegate(const V: TVec2): TVec2;
begin
  Result[0] := -V[0];
  Result[1] := -V[1];
end;

function Vec2Norm(const V: TVec2): TGeoFloat;
begin
  Result := V[0] * V[0] + V[1] * V[1];
end;

function Vec2Negate(const V: TVec2): TVec2;
begin
  Result[0] := -V[0];
  Result[1] := -V[1];
end;

function Vec2Inv(const V: TVec2): TVec2;
begin
  Result[0] := V[1];
  Result[1] := V[0];
end;

procedure SetVec2(var V: TVec2; const vSrc: TVec2);
begin
  V[0] := vSrc[0];
  V[1] := vSrc[1];
end;

function Vec2Direction(sour, dest: TVec2): TVec2;
begin
  Result := Vec2Sub(dest, sour);
end;

function RectDirection(sour, dest: TRectV2): TRectV2;
begin
  Result := RectSub(dest, sour);
end;

function Vec2Add(const v1, v2: TVec2): TVec2;
begin
  Result[0] := v1[0] + v2[0];
  Result[1] := v1[1] + v2[1];
end;

function Vec2Add(const v1: TVec2; v2: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] + v2;
  Result[1] := v1[1] + v2;
end;

function Vec2Add(const v1: TVec2; X, Y: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] + X;
  Result[1] := v1[1] + Y;
end;

function Vec2Add(const v1: TGeoFloat; v2: TVec2): TVec2;
begin
  Result[0] := v1 + v2[0];
  Result[1] := v1 + v2[1];
end;

function Vec2Add(const v1: TArrayVec2; v2: TVec2): TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Add(v1[i], v2);
end;

function Vec2Add(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Add(v1[i], v2);
end;

function Vec2Sub(const v1, v2: TVec2): TVec2;
begin
  Result[0] := v1[0] - v2[0];
  Result[1] := v1[1] - v2[1];
end;

function Vec2Sub(const v1: TVec2; v2: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] - v2;
  Result[1] := v1[1] - v2;
end;

function Vec2Sub(const v1: TGeoFloat; v2: TVec2): TVec2;
begin
  Result[0] := v1 - v2[0];
  Result[1] := v1 - v2[1];
end;

function Vec2Sub(const v1: TArrayVec2; v2: TVec2): TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Sub(v1[i], v2);
end;

function Vec2Sub(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Sub(v1[i], v2);
end;

function Vec2Mul(const v1, v2: TVec2): TVec2;
begin
  Result[0] := v1[0] * v2[0];
  Result[1] := v1[1] * v2[1];
end;

function Vec2Mul(const v1, v2: TVec2; const v3: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2[0] * v3;
  Result[1] := v1[1] * v2[1] * v3;
end;

function Vec2Mul(const v1, v2: TVec2; const v3, v4: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2[0] * v3 * v4;
  Result[1] := v1[1] * v2[1] * v3 * v4;
end;

function Vec2Mul(const v1, v2, v3: TVec2): TVec2;
begin
  Result[0] := v1[0] * v2[0] * v3[0];
  Result[1] := v1[1] * v2[1] * v3[1];
end;

function Vec2Mul(const v1, v2, v3, v4: TVec2): TVec2;
begin
  Result[0] := v1[0] * v2[0] * v3[0] * v4[0];
  Result[1] := v1[1] * v2[1] * v3[1] * v4[1];
end;

function Vec2Mul(const v1: TVec2; const v2: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2;
  Result[1] := v1[1] * v2;
end;

function Vec2Mul(const v1: TVec2; const v2, v3: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2 * v3;
  Result[1] := v1[1] * v2 * v3;
end;

function Vec2Mul(const v1: TVec2; const v2, v3, v4: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2 * v3 * v4;
  Result[1] := v1[1] * v2 * v3 * v4;
end;

function Vec2Mul(const v1: TGeoFloat; const v2: TVec2): TVec2;
begin
  Result[0] := v1 * v2[0];
  Result[1] := v1 * v2[1];
end;

function Vec2Mul(const v1: TArrayVec2; v2: TVec2): TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Mul(v1[i], v2);
end;

function Vec2Mul(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Mul(v1[i], v2);
end;

function Vec2Div(const v1: TVec2; const v2: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] / v2;
  Result[1] := v1[1] / v2;
end;

function Vec2Div(const v1, v2: TVec2): TVec2;
begin
  Result[0] := v1[0] / v2[0];
  Result[1] := v1[1] / v2[1];
end;

function Vec2Div(const v1: TGeoFloat; const v2: TVec2): TVec2;
begin
  Result[0] := v1 / v2[0];
  Result[1] := v1 / v2[1];
end;

function PointNormalize(const V: TVec2): TVec2;
var
  InvLen: TGeoFloat;
  vn: TGeoFloat;
begin
  vn := PointNorm(V);
  if vn = 0 then
      Result := V
  else
    begin
      InvLen := 1 / Sqrt(vn);
      Result[0] := V[0] * InvLen;
      Result[1] := V[1] * InvLen;
    end;
end;

function Vec2Normalize(const V: TVec2): TVec2;
var
  InvLen: TGeoFloat;
  vn: TGeoFloat;
begin
  vn := Vec2Norm(V);
  if vn = 0 then
      Result := V
  else
    begin
      InvLen := 1 / Sqrt(vn);
      Result[0] := V[0] * InvLen;
      Result[1] := V[1] * InvLen;
    end;
end;

function NoLoss_PointNormalize(const V: TVec2): TVec2;
var
  f: TGeoFloat;
begin
  f := MaxF(V[0], V[1]);
  Result[0] := V[0] / f;
  Result[1] := V[1] / f;
end;

function NoLoss_Vec2Normalize(const V: TVec2): TVec2;
var
  f: TGeoFloat;
begin
  f := MaxF(V[0], V[1]);
  Result[0] := V[0] / f;
  Result[1] := V[1] / f;
end;

function PointLength(const V: TVec2): TGeoFloat;
begin
  Result := Sqrt(PointNorm(V));
end;

function Vec2Length(const V: TVec2): TGeoFloat;
begin
  Result := Sqrt(Vec2Norm(V));
end;

procedure PointScale(var V: TVec2; factor: TGeoFloat);
begin
  V[0] := V[0] * factor;
  V[1] := V[1] * factor;
end;

function PointDotProduct(const v1, v2: TVec2): TGeoFloat;
begin
  Result := v1[0] * v2[0] + v1[1] * v2[1];
end;

function Vec2DotProduct(const v1, v2: TVec2): TGeoFloat;
begin
  Result := v1[0] * v2[0] + v1[1] * v2[1];
end;

procedure MidPoint(const x1, y1, x2, y2: TGeoFloat; out midx, midy: TGeoFloat);
begin
  midx := (x1 + x2) * 0.5;
  midy := (y1 + y2) * 0.5;
end;

procedure ShortenSegment(const Amount: TGeoFloat; var x1, y1, x2, y2: TGeoFloat);
var
  SegmentLength: TGeoFloat;
  DistRatio: TGeoFloat;
  Dx, Dy: TGeoFloat;
begin
  SegmentLength := Distance(x1, y1, x2, y2);
  if SegmentLength < Amount then
    begin
      MidPoint(x1, y1, x2, y2, x1, y1);
      x2 := x1;
      y2 := y1;
      Exit;
    end;
  DistRatio := Amount / (2 * SegmentLength);
  Dx := x2 - x1;
  Dy := y2 - y1;
  x1 := x1 + DistRatio * Dx;
  y1 := y1 + DistRatio * Dy;
  x2 := x2 - DistRatio * Dx;
  y2 := y2 - DistRatio * Dy;
end;

procedure LengthenSegment(const Amount: TGeoFloat; out x1, y1, x2, y2: TGeoFloat);
var
  Cx, Cy: TGeoFloat;
  SegmentLength: TGeoFloat;
  Ratio: TGeoFloat;
begin
  SegmentLength := Distance(x1, y1, x2, y2);
  MidPoint(x1, y1, x2, y2, Cx, Cy);
  Ratio := (Amount + SegmentLength) / SegmentLength;
  x1 := Cx + Ratio * (x1 - Cx);
  y1 := Cy + Ratio * (y1 - Cy);
  x2 := Cx + Ratio * (x2 - Cx);
  y2 := Cy + Ratio * (y2 - Cy);
end;

function Signed(const x1, y1, x2, y2, Px, Py: TGeoFloat): TGeoFloat;
begin
  Result := (x2 - x1) * (Py - y1) - (Px - x1) * (y2 - y1);
end;

function CalculateBarycentricBase(const x1, y1, x2, y2, x3, y3: TGeoFloat): TGeoFloat;
begin
  Result := Signed(x1, y1, x2, y2, x3, y3);
end;

procedure ConvertCartesianToBarycentric(const x1, y1, x2, y2, x3, y3, Px, Py: TGeoFloat; out U, V, W: TGeoFloat);
var
  BarycentricBase: TGeoFloat;
begin
  BarycentricBase := 1 / CalculateBarycentricBase(x1, y1, x2, y2, x3, y3);
  U := CalculateBarycentricBase(Px, Py, x2, y2, x3, y3) * BarycentricBase;
  V := CalculateBarycentricBase(x1, y1, Px, Py, x3, y3) * BarycentricBase;
  W := CalculateBarycentricBase(x1, y1, x2, y2, Px, Py) * BarycentricBase;
end;

procedure ConvertBarycentricToCartesian(const U, V, W, x1, y1, x2, y2, x3, y3: TGeoFloat; out X, Y: TGeoFloat);
begin
  X := U * x1 + V * x2 + W * x3;
  Y := U * y1 + V * y2 + W * y3;
end;

function Distance(const x1, y1, x2, y2: TGeoFloat): TGeoFloat;
var
  Dx, Dy: TGeoFloat;
begin
  Dx := x2 - x1;
  Dy := y2 - y1;
  Result := Sqrt(Dx * Dx + Dy * Dy);
end;

procedure ClosestPointOnLineFromPoint(const x1, y1, x2, y2, Px, Py: TGeoFloat; out Nx, Ny: TGeoFloat);
var
  Vx, Vy: TGeoFloat;
  Wx, Wy: TGeoFloat;
  C1, C2: TGeoFloat;
  Ratio: TGeoFloat;
begin
  Vx := x2 - x1;
  Vy := y2 - y1;
  Wx := Px - x1;
  Wy := Py - y1;

  C1 := Vx * Wx + Vy * Wy;
  C2 := Vx * Vx + Vy * Vy;

  Ratio := C1 / C2;

  Nx := x1 + Ratio * Vx;
  Ny := y1 + Ratio * Vy;
end;

procedure NonSymmetricMirror(const Px, Py, x1, y1, x2, y2: TGeoFloat; const Ratio: TGeoFloat; out Nx, Ny: TGeoFloat);
var
  GeneralRatio: TGeoFloat;
begin
  ClosestPointOnLineFromPoint(x1, y1, x2, y2, Px, Py, Nx, Ny);
  GeneralRatio := 2 * Ratio;
  Nx := Px + GeneralRatio * (Nx - Px);
  Ny := Py + GeneralRatio * (Ny - Py);
end;

function NonSymmetricMirror(const Point: TVec2; const Ratio: TGeoFloat; const Line: TLineV2): TVec2;
begin
  NonSymmetricMirror(Point[0], Point[1], Ratio, Line[0, 0], Line[0, 1], Line[1, 0], Line[1, 1], Result[0], Result[1]);
end;

function NonSymmetricMirror(const Rectangle: TRectV2; const Ratio: TGeoFloat; const Line: TLineV2): TRectV2;
begin
  Result[0] := NonSymmetricMirror(Rectangle[0], Ratio, Line);
  Result[1] := NonSymmetricMirror(Rectangle[0], Ratio, Line);
end;

function NonSymmetricMirror(const Triangle: TTriangle; const Ratio: TGeoFloat; const Line: TLineV2): TTriangle;
begin
  Result[0] := NonSymmetricMirror(Triangle[0], Ratio, Line);
  Result[1] := NonSymmetricMirror(Triangle[1], Ratio, Line);
  Result[2] := NonSymmetricMirror(Triangle[2], Ratio, Line);
end;

procedure Mirror(const Px, Py, x1, y1, x2, y2: TGeoFloat; out Nx, Ny: TGeoFloat);
begin
  ClosestPointOnLineFromPoint(x1, y1, x2, y2, Px, Py, Nx, Ny);
  Nx := Px + 2 * (Nx - Px);
  Ny := Py + 2 * (Ny - Py);
end;

function Mirror(const Point: TVec2; const Line: TLineV2): TVec2;
begin
  Mirror(Point[0], Point[1], Line[0, 0], Line[0, 1], Line[1, 0], Line[1, 1], Result[0], Result[1]);
end;

function Mirror(const Rectangle: TRectV2; const Line: TLineV2): TRectV2;
begin
  Result[0] := Mirror(Rectangle[0], Line);
  Result[1] := Mirror(Rectangle[0], Line);
end;

function Mirror(const Triangle: TTriangle; const Line: TLineV2): TTriangle;
begin
  Result[0] := Mirror(Triangle[0], Line);
  Result[1] := Mirror(Triangle[1], Line);
  Result[2] := Mirror(Triangle[2], Line);
end;

function Distance(const x1, y1, z1, x2, y2, z2: TGeoFloat): TGeoFloat;
begin
  Result := Sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) + (z2 - z1) * (z2 - z1));
end;

function Distance(const L: TLineV2): TGeoFloat;
begin
  Result := Distance(L[0, 0], L[0, 1], L[1, 0], L[1, 1]);
end;

function Distance(const f1, f2: TGeoFloat): TGeoFloat;
begin
  if f2 > f1 then
      Result := f2 - f1
  else
      Result := f1 - f2;
end;

function FloatDistance(const f1, f2: TGeoFloat): TGeoFloat;
begin
  if f2 > f1 then
      Result := f2 - f1
  else
      Result := f1 - f2;
end;

function PointDistance(const x1, y1, x2, y2: TGeoFloat): TGeoFloat;
begin
  Result := Sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
end;

function PointDistance(const v1, v2: TVec2): TGeoFloat;
begin
  Result := Sqrt((v2[0] - v1[0]) * (v2[0] - v1[0]) + (v2[1] - v1[1]) * (v2[1] - v1[1]));
end;

function Vec2Distance(const v1, v2: TVec2): TGeoFloat;
begin
  Result := Sqrt((v2[0] - v1[0]) * (v2[0] - v1[0]) + (v2[1] - v1[1]) * (v2[1] - v1[1]));
end;

function LineDistance(const L: TLineV2): TGeoFloat;
begin
  Result := Distance(L[0, 0], L[0, 1], L[1, 0], L[1, 1]);
end;

function PointLayDistance(const v1, v2: TVec2): TGeoFloat;
begin
  Result := Pow(v2[0] - v1[0]) + Pow(v2[1] - v1[1]);
end;

function LayDistance(const x1, y1, x2, y2: TGeoFloat): TGeoFloat;
var
  Dx: TGeoFloat;
  Dy: TGeoFloat;
begin
  Dx := (x2 - x1);
  Dy := (y2 - y1);
  Result := Dx * Dx + Dy * Dy;
end;

function SqrDistance(const v1, v2: TVec2): TGeoFloat;
begin
  Result := Sqr(v2[0] - v1[0]) + Sqr(v2[1] - v1[1]);
end;

function PointLerp(const v1, v2: TVec2; t: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] + (v2[0] - v1[0]) * t;
  Result[1] := v1[1] + (v2[1] - v1[1]) * t;
end;

function PointLerpTo(const sour, dest: TVec2; const d: TGeoFloat): TVec2;
var
  Dx: TGeoFloat;
  Dy: TGeoFloat;
  k: Double;
begin
  Dx := dest[0] - sour[0];
  Dy := dest[1] - sour[1];
  if ((Dx <> 0) or (Dy <> 0)) and (d <> 0) then
    begin
      k := d / Sqrt(Dx * Dx + Dy * Dy);
      Result[0] := sour[0] + k * Dx;
      Result[1] := sour[1] + k * Dy;
    end
  else
    begin
      Result := sour;
    end;
end;

function Vec2Lerp(const v1, v2: TVec2; t: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] + (v2[0] - v1[0]) * t;
  Result[1] := v1[1] + (v2[1] - v1[1]) * t;
end;

function Vec2LerpTo(const sour, dest: TVec2; const d: TGeoFloat): TVec2;
var
  Dx: TGeoFloat;
  Dy: TGeoFloat;
  k: Double;
begin
  if d = 0 then
    begin
      Result := sour;
      Exit;
    end;

  Dx := dest[0] - sour[0];
  Dy := dest[1] - sour[1];
  k := d / Sqrt(Dx * Dx + Dy * Dy);
  Result[0] := sour[0] + k * Dx;
  Result[1] := sour[1] + k * Dy;
end;

procedure SwapPoint(var v1, v2: TVec2);
var
  V: TVec2;
begin
  V := v1;
  v1 := v2;
  v2 := V;
end;

procedure SwapVec2(var v1, v2: TVec2);
var
  V: TVec2;
begin
  V := v1;
  v1 := v2;
  v2 := V;
end;

function Pow(V: TGeoFloat): TGeoFloat;
begin
  Result := V * V;
end;

function Pow(const V, n: TGeoFloat): TGeoFloat;
begin
  Result := Power_(V, n);
end;

function MiddleVec2(const pt1, pt2: TVec2): TVec2;
begin
  Result[0] := (pt1[0] + pt2[0]) * 0.5;
  Result[1] := (pt1[1] + pt2[1]) * 0.5;
end;

function Vec2Middle(const pt1, pt2: TVec2): TVec2;
begin
  Result[0] := (pt1[0] + pt2[0]) * 0.5;
  Result[1] := (pt1[1] + pt2[1]) * 0.5;
end;

function IsEqual(const Val1, Val2, Epsilon_: TGeoFloat): Boolean;
var
  Diff: TGeoFloat;
begin
  Diff := Val1 - Val2;
  Assert(((-Epsilon_ <= Diff) and (Diff <= Epsilon_)) = (FAbs(Diff) <= Epsilon_), 'Error - Illogical error in equality Detect. (IsEqual)');
  Result := ((-Epsilon_ <= Diff) and (Diff <= Epsilon_));
end;

function IsEqual(const Val1, Val2: TGeoFloat): Boolean;
begin
  Result := IsEqual(Val1, Val2, C_Epsilon);
end;

function IsEqual(const Val1, Val2: TVec2): Boolean;
begin
  Result := IsEqual(Val1[0], Val2[0]) and IsEqual(Val1[1], Val2[1]);
end;

function IsEqual(const Val1, Val2: TVec2; Epsilon_: TGeoFloat): Boolean;
begin
  Result := IsEqual(Val1[0], Val2[0], Epsilon_) and IsEqual(Val1[1], Val2[1], Epsilon_);
end;

function IsEqual(const Val1, Val2: TRectV2): Boolean;
begin
  Result := IsEqual(Val1[0], Val2[0]) and IsEqual(Val1[1], Val2[1]);
end;

function IsEqual_X(const Val1, Val2: TVec2): Boolean;
begin
  Result := IsEqual(Val1[0], Val2[0]);
end;

function IsEqual_Y(const Val1, Val2: TVec2): Boolean;
begin
  Result := IsEqual(Val1[1], Val2[1]);
end;

function NotEqual(const Val1, Val2, Epsilon_: TGeoFloat): Boolean;
var
  Diff: TGeoFloat;
begin
  Diff := Val1 - Val2;
  Assert(((-Epsilon_ > Diff) or (Diff > Epsilon_)) = (FAbs(Val1 - Val2) > Epsilon_), 'Error - Illogical error in equality Detect. (NotEqual)');
  Result := ((-Epsilon_ > Diff) or (Diff > Epsilon_));
end;

function NotEqual(const Val1, Val2: TGeoFloat): Boolean;
begin
  Result := NotEqual(Val1, Val2, C_Epsilon);
end;

function NotEqual(const Val1, Val2: TVec2): Boolean;
begin
  Result := NotEqual(Val1[0], Val2[0]) or NotEqual(Val1[1], Val2[1]);
end;

function LessThanOrEqual(const Val1, Val2: TGeoFloat): Boolean;
begin
  Result := (Val1 < Val2) or IsEqual(Val1, Val2);
end;

function GreaterThanOrEqual(const Val1, Val2: TGeoFloat): Boolean;
begin
  Result := (Val1 > Val2) or IsEqual(Val1, Val2);
end;

function GetEquilateralTriangleCen(pt1, pt2: TVec2): TVec2;
const
  Sin60: TGeoFloat = 0.86602540378443864676372317075294;
  Cos60: TGeoFloat = 0.50000000000000000000000000000000;
var
  b, E, pt: TVec2;
begin
  b := pt1;
  E := pt2;
  E[0] := E[0] - b[0];
  E[1] := E[1] - b[1];
  pt[0] := ((E[0] * Cos60) - (E[1] * Sin60)) + b[0];
  pt[1] := ((E[1] * Cos60) + (E[0] * Sin60)) + b[1];
  Assert(Intersect(pt1, MiddleVec2(pt2, pt), pt2, MiddleVec2(pt1, pt), Result));
end;

procedure Rotate(RotAng: TGeoFloat; const X, Y: TGeoFloat; out Nx, Ny: TGeoFloat);
var
  SinVal: TGeoFloat;
  CosVal: TGeoFloat;
begin
  RotAng := RotAng * PIDiv180;
  SinVal := Sin(RotAng);
  CosVal := Cos(RotAng);
  Nx := (X * CosVal) - (Y * SinVal);
  Ny := (Y * CosVal) + (X * SinVal);
end;

function Rotate(const RotAng: TGeoFloat; const Point: TVec2): TVec2;
begin
  Rotate(RotAng, Point[0], Point[1], Result[0], Result[1]);
end;

function NormalizeDegAngle(const Angle: TGeoFloat): TGeoFloat;
begin
  Result := Angle - Int(Angle * (1 / 360)) * 360;
  if Result > 180 then
      Result := Result - 360
  else if Result < -180 then
      Result := Result + 360;
end;

function VerticalMirror(const Angle: TGeoFloat): TGeoFloat;
begin
  Result := Angle;
  if IsEqual(Angle, Zero) or
    IsEqual(Angle, 180.0) or
    IsEqual(Angle, 360.0) then
      Exit;
  Result := 360.0 - Result;
end;
(* Vertical Mirror *)

function HorizontalMirror(const Angle: TGeoFloat): TGeoFloat;
begin
  Result := Angle;
  if Result <= 180.0 then
      Result := 180.0 - Result
  else
      Result := 540.0 - Result;
end;

function PointAngle(const axis, pt: TVec2): TGeoFloat;
begin
  Result := NormalizeDegAngle(RadToDeg_(ArcTan2_(axis[1] - pt[1], axis[0] - pt[0])));
end;

function Vec2Angle(const axis, pt: TVec2): TGeoFloat;
begin
  Result := NormalizeDegAngle(RadToDeg_(ArcTan2_(axis[1] - pt[1], axis[0] - pt[0])));
end;

function PointAngle(const pt: TVec2): TGeoFloat;
begin
  Result := PointAngle(NULLPoint, pt);
end;

function Vec2Angle(const pt: TVec2): TGeoFloat;
begin
  Result := Vec2Angle(NULLPoint, pt);
end;

function AngleDistance(const s, a: TGeoFloat): TGeoFloat;
begin
  Result := FAbs(s - a);
  if Result > 180 then
      Result := 360 - Result;
end;

function PointRotation(const axis: TVec2; const Dist, Angle: TGeoFloat): TVec2;
begin
  Result[0] := axis[0] - (Cos(DegToRad_(Angle)) * Dist);
  Result[1] := axis[1] - (Sin(DegToRad_(Angle)) * Dist);
end;

function PointRotation(const axis, pt: TVec2; const Angle: TGeoFloat): TVec2;
begin
  Result := PointRotation(axis, PointDistance(axis, pt), Angle);
end;

function Vec2Rotation(const axis: TVec2; const Dist, Angle: TGeoFloat): TVec2;
begin
  Result[0] := axis[0] - (Cos(DegToRad_(Angle)) * Dist);
  Result[1] := axis[1] - (Sin(DegToRad_(Angle)) * Dist);
end;

function Vec2Rotation(const axis, pt: TVec2; const Angle: TGeoFloat): TVec2;
begin
  Result := Vec2Rotation(axis, Vec2Distance(axis, pt), Angle);
end;

function Vec2Rotation(const sour_r: TRectV2; const Angle: TGeoFloat; const pt: TVec2): TVec2;
begin
  Result := Vec2Rotation(sour_r, RectCentre(sour_r), Angle, pt);
end;

function Vec2Rotation(const sour_r: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const pt: TVec2): TVec2;
begin
  Result := Vec2Rotation(axis, pt, NormalizeDegAngle(Vec2Angle(axis, pt) - Angle));
end;

function Vec2Rotation(const sour_r: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const R: TRectV2): TRectV2;
begin
  Result[0] := Vec2Rotation(sour_r, axis, Angle, R[0]);
  Result[1] := Vec2Rotation(sour_r, axis, Angle, R[1]);
end;

function RectRotation(const axis: TVec2; const R: TRectV2; const Angle: TGeoFloat): TRectV2;
begin
  Result[0] := Vec2Rotation(axis, R[0], Angle);
  Result[1] := Vec2Rotation(axis, R[1], Angle);
end;

function CircleInCircle(const cp1, cp2: TVec2; const r1, r2: TGeoFloat): Boolean;
begin
  Result := (r2 - (PointDistance(cp1, cp2) + r1) >= Zero);
end;

function CircleInRect(const cp: TVec2; const radius: TGeoFloat; R: TRectV2): Boolean;
begin
  FixRect(R[0, 0], R[0, 1], R[1, 0], R[1, 1]);
  Result := PointInRect(cp, MakeRect(Vec2Sub(R[0], radius), Vec2Add(R[1], radius)));
end;

function PointInRect(const Px, Py: TGeoFloat; const x1, y1, x2, y2: TGeoFloat): Boolean;
begin
  Result := ((x1 <= Px) and (Px <= x2) and (y1 <= Py) and (Py <= y2)) or ((x2 <= Px) and (Px <= x1) and (y2 <= Py) and (Py <= y1));
end;

function PointInRect(const Px, Py: TGeoInt; const x1, y1, x2, y2: TGeoInt): Boolean;
begin
  Result := ((x1 <= Px) and (Px <= x2) and (y1 <= Py) and (Py <= y2)) or ((x2 <= Px) and (Px <= x1) and (y2 <= Py) and (Py <= y1));
end;

function PointInRect(const X, Y: TGeoInt; const R: TRect): Boolean;
begin
  Result := PointInRect(X, Y, R.Left, R.Top, R.Right, R.Bottom);
end;

function PointInRect(const pt: TPoint; const R: TRect): Boolean;
begin
  Result := PointInRect(pt.X, pt.Y, R.Left, R.Top, R.Right, R.Bottom);
end;

function PointInRect(const pt: TVec2; const R: TRectV2): Boolean;
begin
  Result := PointInRect(pt[0], pt[1], R[0, 0], R[0, 1], R[1, 0], R[1, 1]);
end;

function PointInRect(const Px, Py: TGeoFloat; const R: TRectV2): Boolean;
begin
  Result := PointInRect(Px, Py, R[0, 0], R[0, 1], R[1, 0], R[1, 1]);
end;

function Vec2InRect(const Px, Py: TGeoFloat; const x1, y1, x2, y2: TGeoFloat): Boolean;
begin
  Result := ((x1 <= Px) and (Px <= x2) and (y1 <= Py) and (Py <= y2)) or ((x2 <= Px) and (Px <= x1) and (y2 <= Py) and (Py <= y1));
end;

function Vec2InRect(const Px, Py: TGeoInt; const x1, y1, x2, y2: TGeoInt): Boolean;
begin
  Result := ((x1 <= Px) and (Px <= x2) and (y1 <= Py) and (Py <= y2)) or ((x2 <= Px) and (Px <= x1) and (y2 <= Py) and (Py <= y1));
end;

function Vec2InRect(const pt: TVec2; const R: TRectV2): Boolean;
begin
  Result := Vec2InRect(pt[0], pt[1], R[0, 0], R[0, 1], R[1, 0], R[1, 1]);
end;

function Vec2InRect(const Px, Py: TGeoFloat; const R: TRectV2): Boolean;
begin
  Result := Vec2InRect(Px, Py, R[0, 0], R[0, 1], R[1, 0], R[1, 1]);
end;

function RectToRectIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean;
begin
  Result := (x1 <= x4) and (x2 >= x3) and (y1 <= y4) and (y2 >= y3);
end;

function RectToRectIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean;
begin
  Result := (x1 <= x4) and (x2 >= x3) and (y1 <= y4) and (y2 >= y3);
end;

function RectToRectIntersect(const r1, r2: TRectV2): Boolean;
begin
  Result := RectToRectIntersect(r1[0, 0], r1[0, 1], r1[1, 0], r1[1, 1], r2[0, 0], r2[0, 1], r2[1, 0], r2[1, 1]);
end;

function RectToRectIntersect(const r1, r2: TRect): Boolean;
begin
  Result := RectToRectIntersect(r1.Left, r1.Top, r1.Right, r1.Bottom, r2.Left, r2.Top, r2.Right, r2.Bottom);
end;

function RectToRectIntersect(const r1, r2: TRectf): Boolean;
begin
  Result := RectToRectIntersect(r1.Left, r1.Top, r1.Right, r1.Bottom, r2.Left, r2.Top, r2.Right, r2.Bottom);
end;

function RectWithInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean;
begin
  Result := PointInRect(x1, y1, x3, y3, x4, y4) and PointInRect(x2, y2, x3, y3, x4, y4);
end;

function RectWithInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean;
begin
  Result := PointInRect(x1, y1, x3, y3, x4, y4) and PointInRect(x2, y2, x3, y3, x4, y4);
end;

function RectWithInRect(const r1, r2: TRectV2): Boolean;
begin
  Result := RectWithInRect(r1[0, 0], r1[0, 1], r1[1, 0], r1[1, 1], r2[0, 0], r2[0, 1], r2[1, 0], r2[1, 1]);
end;

function RectWithInRect(const r1, r2: TRect): Boolean;
begin
  Result := RectWithInRect(r1.Left, r1.Top, r1.Right, r1.Bottom, r2.Left, r2.Top, r2.Right, r2.Bottom);
end;

function RectInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean;
begin
  Result := PointInRect(x1, y1, x3, y3, x4, y4) and PointInRect(x2, y2, x3, y3, x4, y4);
end;

function RectInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean;
begin
  Result := PointInRect(x1, y1, x3, y3, x4, y4) and PointInRect(x2, y2, x3, y3, x4, y4);
end;

function RectInRect(const r1, r2: TRectV2): Boolean;
begin
  Result := RectInRect(r1[0, 0], r1[0, 1], r1[1, 0], r1[1, 1], r2[0, 0], r2[0, 1], r2[1, 0], r2[1, 1]);
end;

function RectInRect(const r1, r2: TRect): Boolean;
begin
  Result := RectInRect(r1.Left, r1.Top, r1.Right, r1.Bottom, r2.Left, r2.Top, r2.Right, r2.Bottom);
end;

function MakeRectV2(const centre: TVec2; const width, height: TGeoFloat): TRectV2;
begin
  Result[0, 0] := centre[0] - width * 0.5;
  Result[0, 1] := centre[1] - height * 0.5;
  Result[1, 0] := centre[0] + width * 0.5;
  Result[1, 1] := centre[1] + height * 0.5;
end;

function MakeRectV2(const X, Y, radius: TGeoFloat): TRectV2;
begin
  Result[0, 0] := X - radius;
  Result[0, 1] := Y - radius;
  Result[1, 0] := X + radius;
  Result[1, 1] := Y + radius;
end;

function MakeRectV2(const x1, y1, x2, y2: TGeoFloat): TRectV2;
begin
  Result[0, 0] := x1;
  Result[0, 1] := y1;
  Result[1, 0] := x2;
  Result[1, 1] := y2;
end;

function MakeRectV2(const p1, p2: TVec2): TRectV2;
begin
  Result[0] := p1;
  Result[1] := p2;
end;

function MakeRectV2(const X, Y: TGeoFloat; const p2: TVec2): TRectV2;
begin
  Result[0] := PointMake(X, Y);
  Result[1] := p2;
end;

function MakeRectV2(const R: TRect): TRectV2;
begin
  Result[0, 0] := R.Left;
  Result[0, 1] := R.Top;
  Result[1, 0] := R.Right;
  Result[1, 1] := R.Bottom;
end;

function MakeRectV2(const R: TRectf): TRectV2;
begin
  Result[0, 0] := R.Left;
  Result[0, 1] := R.Top;
  Result[1, 0] := R.Right;
  Result[1, 1] := R.Bottom;
end;

function RectV2(): TRectV2;
begin
  Result := ZeroRect;
end;

function RectV2(const centre: TVec2; const width, height: TGeoFloat): TRectV2;
begin
  Result[0, 0] := centre[0] - width * 0.5;
  Result[0, 1] := centre[1] - height * 0.5;
  Result[1, 0] := centre[0] + width * 0.5;
  Result[1, 1] := centre[1] + height * 0.5;
end;

function RectV2(const X, Y, radius: TGeoFloat): TRectV2;
begin
  Result[0, 0] := X - radius;
  Result[0, 1] := Y - radius;
  Result[1, 0] := X + radius;
  Result[1, 1] := Y + radius;
end;

function RectV2(const x1, y1, x2, y2: TGeoFloat): TRectV2;
begin
  Result[0, 0] := x1;
  Result[0, 1] := y1;
  Result[1, 0] := x2;
  Result[1, 1] := y2;
end;

function RectV2(const p1, p2: TVec2): TRectV2;
begin
  Result[0] := p1;
  Result[1] := p2;
end;

function RectV2(const p1, p2: TPointf): TRectV2;
begin
  Result[0] := vec2(p1);
  Result[1] := vec2(p2);
end;

function RectV2(const X, Y: TGeoFloat; const p2: TVec2): TRectV2;
begin
  Result[0] := PointMake(X, Y);
  Result[1] := p2;
end;

function RectV2(const R: TRect): TRectV2;
begin
  Result[0, 0] := R.Left;
  Result[0, 1] := R.Top;
  Result[1, 0] := R.Right;
  Result[1, 1] := R.Bottom;
end;

function RectV2(const R: TRectf): TRectV2;
begin
  Result[0, 0] := R.Left;
  Result[0, 1] := R.Top;
  Result[1, 0] := R.Right;
  Result[1, 1] := R.Bottom;
end;

function RectV2(const R: TRectV2): TRectV2;
begin
  Result := FixedRect(R);
end;

function MakeRect(const centre: TVec2; const width, height: TGeoFloat): TRectV2;
begin
  Result[0, 0] := centre[0] - width * 0.5;
  Result[0, 1] := centre[1] - height * 0.5;
  Result[1, 0] := centre[0] + width * 0.5;
  Result[1, 1] := centre[1] + height * 0.5;
end;

function MakeRect(const X, Y, radius: TGeoFloat): TRectV2;
begin
  Result[0, 0] := X - radius;
  Result[0, 1] := Y - radius;
  Result[1, 0] := X + radius;
  Result[1, 1] := Y + radius;
end;

function MakeRect(const x1, y1, x2, y2: TGeoFloat): TRectV2;
begin
  Result[0, 0] := x1;
  Result[0, 1] := y1;
  Result[1, 0] := x2;
  Result[1, 1] := y2;
end;

function MakeRect(const p1, p2: TVec2): TRectV2;
begin
  Result[0] := p1;
  Result[1] := p2;
end;

function MakeRect(const R: TRect): TRectV2;
begin
  Result[0, 0] := R.Left;
  Result[0, 1] := R.Top;
  Result[1, 0] := R.Right;
  Result[1, 1] := R.Bottom;
end;

function MakeRect(const R: TRectf): TRectV2;
begin
  Result[0, 0] := R.Left;
  Result[0, 1] := R.Top;
  Result[1, 0] := R.Right;
  Result[1, 1] := R.Bottom;
end;

function RoundRect(const R: TRectV2): TRect;
begin
  Result.Left := Round(R[0, 0]);
  Result.Top := Round(R[0, 1]);
  Result.Right := Round(R[1, 0]);
  Result.Bottom := Round(R[1, 1]);
end;

function RoundRectV2(const R: TRectV2): TRectV2;
begin
  Result[0, 0] := Round(R[0, 0]);
  Result[0, 1] := Round(R[0, 1]);
  Result[1, 0] := Round(R[1, 0]);
  Result[1, 1] := Round(R[1, 1]);
end;

function Rect2Rect(const R: TRectV2): TRect;
begin
  Result.Left := Round(R[0, 0]);
  Result.Top := Round(R[0, 1]);
  Result.Right := Round(R[1, 0]);
  Result.Bottom := Round(R[1, 1]);
end;

function Rect2Rect(const R: TRect): TRectV2;
begin
  Result[0, 0] := R.Left;
  Result[0, 1] := R.Top;
  Result[1, 0] := R.Right;
  Result[1, 1] := R.Bottom;
end;

function RectMake(const X, Y, radius: TGeoFloat): TRectV2;
begin
  Result[0, 0] := X - radius;
  Result[0, 1] := Y - radius;
  Result[1, 0] := X + radius;
  Result[1, 1] := Y + radius;
end;

function RectMake(const x1, y1, x2, y2: TGeoFloat): TRectV2;
begin
  Result[0, 0] := x1;
  Result[0, 1] := y1;
  Result[1, 0] := x2;
  Result[1, 1] := y2;
end;

function RectMake(const p1, p2: TVec2): TRectV2;
begin
  Result[0] := p1;
  Result[1] := p2;
end;

function RectMake(const R: TRect): TRectV2;
begin
  Result[0, 0] := R.Left;
  Result[0, 1] := R.Top;
  Result[1, 0] := R.Right;
  Result[1, 1] := R.Bottom;
end;

function RectMake(const R: TRectf): TRectV2;
begin
  Result[0, 0] := R.Left;
  Result[0, 1] := R.Top;
  Result[1, 0] := R.Right;
  Result[1, 1] := R.Bottom;
end;

function RectAdd(const R: TRectV2; v2: TVec2): TRectV2;
begin
  Result[0] := Vec2Add(R[0], v2);
  Result[1] := Vec2Add(R[1], v2);
end;

function RectAdd(const r1, r2: TRectV2): TRectV2;
begin
  Result[0] := Vec2Add(r1[0], r2[0]);
  Result[1] := Vec2Add(r1[1], r2[1]);
end;

function RectSub(const r1, r2: TRectV2): TRectV2;
begin
  Result[0] := Vec2Sub(r1[0], r2[0]);
  Result[1] := Vec2Sub(r1[1], r2[1]);
end;

function RectSub(const R: TRectV2; pt: TVec2): TRectV2;
begin
  Result[0] := Vec2Sub(R[0], pt);
  Result[1] := Vec2Sub(R[1], pt);
end;

function RectMul(const r1, r2: TRectV2): TRectV2;
begin
  Result[0] := Vec2Mul(r1[0], r2[0]);
  Result[1] := Vec2Mul(r1[1], r2[1]);
end;

function RectMul(const r1: TRectV2; v2: TVec2): TRectV2;
begin
  Result[0] := Vec2Mul(r1[0], v2[0]);
  Result[1] := Vec2Mul(r1[1], v2[1]);
end;

function RectMul(const r1: TRectV2; f2: TGeoFloat): TRectV2;
begin
  Result[0] := Vec2Mul(r1[0], f2);
  Result[1] := Vec2Mul(r1[1], f2);
end;

function RectDiv(const r1, r2: TRectV2): TRectV2;
begin
  Result[0] := Vec2Div(r1[0], r2[0]);
  Result[1] := Vec2Div(r1[1], r2[1]);
end;

function RectDiv(const r1: TRectV2; f2: TGeoFloat): TRectV2;
begin
  Result[0] := Vec2Div(r1[0], f2);
  Result[1] := Vec2Div(r1[1], f2);
end;

function RectDiv(const r1: TRectV2; v2: TVec2): TRectV2;
begin
  Result[0] := Vec2Div(r1[0], v2);
  Result[1] := Vec2Div(r1[1], v2);
end;

function RectOffset(const R: TRectV2; Offset: TVec2): TRectV2;
begin
  Result[0] := Vec2Add(R[0], Offset);
  Result[1] := Vec2Add(R[1], Offset);
end;

function RectSizeLerp(const R: TRectV2; const rSizeLerp: TGeoFloat): TRectV2;
begin
  Result[0] := R[0];
  Result[1] := PointLerp(R[0], R[1], rSizeLerp);
end;

function RectCenScale(const R: TRectV2; const rSizeScale: TGeoFloat): TRectV2;
var
  cen, siz: TVec2;
begin
  cen := PointLerp(R[0], R[1], 0.5);
  siz := Vec2Mul(RectSize(R), rSizeScale);
  Result[0] := Vec2Sub(cen, Vec2Mul(siz, 0.5));
  Result[1] := Vec2Add(cen, Vec2Mul(siz, 0.5));
end;

function RectEdge(const R: TRectV2; const Edge: TGeoFloat): TRectV2;
begin
  Result[0, 0] := R[0, 0] - Edge;
  Result[0, 1] := R[0, 1] - Edge;
  Result[1, 0] := R[1, 0] + Edge;
  Result[1, 1] := R[1, 1] + Edge;
end;

function RectEdge(const R: TRectV2; const Edge: TVec2): TRectV2;
begin
  Result[0, 0] := R[0, 0] - Edge[0];
  Result[0, 1] := R[0, 1] - Edge[1];
  Result[1, 0] := R[1, 0] + Edge[0];
  Result[1, 1] := R[1, 1] + Edge[1];
end;

function RectCentre(const R: TRectV2): TVec2;
begin
  Result := PointLerp(R[0], R[1], 0.5);
end;

function RectCentre(const R: TRect): TVec2;
begin
  Result := RectCentre(RectV2(R));
end;

function RectCentre(const R: TRectf): TVec2;
begin
  Result := RectCentre(RectV2(R));
end;

function RectIOU(const r1, r2: TRectV2): TGeoFloat;
var
  R1A, R2A, cA: TGeoFloat;
begin
  R1A := RectArea(r1);
  R2A := RectArea(r2);
  cA := RectArea(Clip(r1, r2));
  Result := cA / (R1A + R2A - cA);
end;

function RectDistance(const r1, r2: TRectV2): TGeoFloat;
begin
  Result := Vec2Distance(RectCentre(r1), RectCentre(r2));
end;

function Tri(const v1, v2, v3: TVec2): TTriangle;
begin
  Result[0] := v1;
  Result[1] := v2;
  Result[2] := v3;
end;

function TriAdd(const t: TTriangle; V: TVec2): TTriangle;
begin
  Result[0] := Vec2Add(t[0], V);
  Result[1] := Vec2Add(t[1], V);
  Result[2] := Vec2Add(t[2], V);
end;

function TriSub(const t: TTriangle; V: TVec2): TTriangle;
begin
  Result[0] := Vec2Sub(t[0], V);
  Result[1] := Vec2Sub(t[1], V);
  Result[2] := Vec2Sub(t[2], V);
end;

function TriMul(const t: TTriangle; V: TVec2): TTriangle;
begin
  Result[0] := Vec2Mul(t[0], V);
  Result[1] := Vec2Mul(t[1], V);
  Result[2] := Vec2Mul(t[2], V);
end;

function TriDiv(const t: TTriangle; V: TVec2): TTriangle;
begin
  Result[0] := Vec2Div(t[0], V);
  Result[1] := Vec2Div(t[1], V);
  Result[2] := Vec2Div(t[2], V);
end;

function TriCentre(const t: TTriangle): TVec2;
const
  TriCentre_OneThird = 1.0 / 3.0;
begin
  Result[0] := (t[0, 0] + t[1, 0] + t[2, 0]) * TriCentre_OneThird;
  Result[1] := (t[0, 1] + t[1, 1] + t[2, 1]) * TriCentre_OneThird;
end;

function TriExpand(const t: TTriangle; Dist: TGeoFloat): TTriangle;

  function getTriPt(idx: TGeoInt): TVec2;
  var
    lpt, pt, rpt: TVec2;
    ln, rn: TVec2;
    Dx, Dy, f, R: TGeoFloat;
    Cx, Cy: TGeoFloat;
  begin
    if idx > 0 then
        lpt := t[idx - 1]
    else
        lpt := t[3 - 1];
    if idx + 1 < 3 then
        rpt := t[idx + 1]
    else
        rpt := t[0];
    pt := t[idx];

    // normal : left to
    Dx := (pt[0] - lpt[0]);
    Dy := (pt[1] - lpt[1]);
    f := 1.0 / HypotX(Dx, Dy);
    ln[0] := (Dy * f);
    ln[1] := -(Dx * f);

    // normal : right to
    Dx := (rpt[0] - pt[0]);
    Dy := (rpt[1] - pt[1]);
    f := 1.0 / HypotX(Dx, Dy);
    rn[0] := (Dy * f);
    rn[1] := -(Dx * f);

    // compute the expand edge
    Dx := (ln[0] + rn[0]);
    Dy := (ln[1] + rn[1]);
    R := (ln[0] * Dx) + (ln[1] * Dy);
    if R = 0 then
        R := 1;
    Cx := (Dx * Dist / R);
    Cy := (Dy * Dist / R);

    Result[0] := pt[0] + Cx;
    Result[1] := pt[1] + Cy;
  end;

begin
  Result[0] := getTriPt(0);
  Result[1] := getTriPt(1);
  Result[2] := getTriPt(2);
end;

function TriRound(const t: TTriangle): TTriangle;
begin
  Result[0] := RoundVec2(t[0]);
  Result[1] := RoundVec2(t[1]);
  Result[2] := RoundVec2(t[2]);
end;

function IsEquilateralTriangle(const x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean;
var
  d1: TGeoFloat;
  d2: TGeoFloat;
  d3: TGeoFloat;
begin
  d1 := LayDistance(x1, y1, x2, y2);
  d2 := LayDistance(x2, y2, x3, y3);
  d3 := LayDistance(x3, y3, x1, y1);
  Result := (IsEqual(d1, d2) and IsEqual(d2, d3));
end;

function IsEquilateralTriangle(const t: TTriangle): Boolean;
begin
  Result := IsEquilateralTriangle(t[0, 0], t[0, 1], t[1, 0], t[1, 1], t[2, 0], t[2, 1]);
end;

function IsIsoscelesTriangle(const x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean;
var
  d1: TGeoFloat;
  d2: TGeoFloat;
  d3: TGeoFloat;
begin
  d1 := LayDistance(x1, y1, x2, y2);
  d2 := LayDistance(x2, y2, x3, y3);
  d3 := LayDistance(x3, y3, x1, y1);
  Result := ((IsEqual(d1, d2) or IsEqual(d1, d3)) and NotEqual(d2, d3)) or
    (IsEqual(d2, d3) and NotEqual(d2, d1));
end;

function IsIsoscelesTriangle(const t: TTriangle): Boolean;
begin
  Result := IsIsoscelesTriangle(t[0, 0], t[0, 1], t[1, 0], t[1, 1], t[2, 0], t[2, 1]);
end;

function IsRightTriangle(const x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean;
var
  d1: TGeoFloat;
  d2: TGeoFloat;
  d3: TGeoFloat;
begin
  d1 := LayDistance(x1, y1, x2, y2);
  d2 := LayDistance(x2, y2, x3, y3);
  d3 := LayDistance(x3, y3, x1, y1);

  Result := (
    IsEqual(d1 + d2, d3) or
      IsEqual(d1 + d3, d2) or
      IsEqual(d3 + d2, d1)
    );
end;

function IsRightTriangle(const t: TTriangle): Boolean;
begin
  Result := IsRightTriangle(t[0, 0], t[0, 1], t[1, 0], t[1, 1], t[2, 0], t[2, 1]);
end;

function IsScaleneTriangle(const x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean;
var
  d1: TGeoFloat;
  d2: TGeoFloat;
  d3: TGeoFloat;
begin
  d1 := LayDistance(x1, y1, x2, y2);
  d2 := LayDistance(x2, y2, x3, y3);
  d3 := LayDistance(x3, y3, x1, y1);
  Result := NotEqual(d1, d2) and NotEqual(d2, d3) and NotEqual(d3, d1);
end;

function IsScaleneTriangle(const t: TTriangle): Boolean;
begin
  Result := IsScaleneTriangle(t[0, 0], t[0, 1], t[1, 0], t[1, 1], t[2, 0], t[2, 1]);
end;

function VertexAngle(x1, y1, x2, y2, x3, y3: TGeoFloat): TGeoFloat;
var
  Dist: TGeoFloat;
  InputTerm: TGeoFloat;
begin
  (*
    Using the cosine identity:
    cosA = (b^2 + c^2 - a^2) / (2*b*c)
    A    = Cos'((b^2 + c^2 - a^2) / (2*b*c))

    Where:

    a,b and c : are edges in the triangle
    A         : is the angle at the vertex opposite edge 'a'
    aka the edge defined by the vertex <x1y1-x2y2-x3y3>

  *)
  (* Quantify coordinates *)
  x1 := x1 - x2;
  x3 := x3 - x2;
  y1 := y1 - y2;
  y3 := y3 - y2;

  (* Calculate Ley Distance *)
  Dist := (x1 * x1 + y1 * y1) * (x3 * x3 + y3 * y3);

  if IsEqual(Dist, Zero) then
      Result := Zero
  else
    begin
      InputTerm := (x1 * x3 + y1 * y3) / Sqrt(Dist);
      if IsEqual(InputTerm, 1.0) then
          Result := Zero
      else if IsEqual(InputTerm, -1.0) then
          Result := 180.0
      else
          Result := ArcCos_(InputTerm) * c180divPI;
    end;
end;

function IsObtuseTriangle(const x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean;
var
  a1: TGeoFloat;
  a2: TGeoFloat;
  a3: TGeoFloat;
begin
  a1 := VertexAngle(x1, y1, x2, y2, x3, y3);
  a2 := VertexAngle(x3, y3, x1, y1, x2, y2);
  a3 := VertexAngle(x2, y2, x3, y3, x1, y1);
  Result := (a1 > 90.0) or (a2 > 90.0) or (a3 > 90.0);
end;

function IsObtuseTriangle(const t: TTriangle): Boolean;
begin
  Result := IsObtuseTriangle(t[0, 0], t[0, 1], t[1, 0], t[1, 1], t[2, 0], t[2, 1]);
end;

function TriangleType(const x1, y1, x2, y2, x3, y3: TGeoFloat): TTriangleType;
begin
  if IsEquilateralTriangle(x1, y1, x2, y2, x3, y3) then
      Result := ttEquilateral
  else if IsIsoscelesTriangle(x1, y1, x2, y2, x3, y3) then
      Result := ttIsosceles
  else if IsRightTriangle(x1, y1, x2, y2, x3, y3) then
      Result := ttRight
  else if IsScaleneTriangle(x1, y1, x2, y2, x3, y3) then
      Result := ttScalene
  else if IsObtuseTriangle(x1, y1, x2, y2, x3, y3) then
      Result := ttObtuse
  else
      Result := ttUnknown;
end;

function TriangleType(const t: TTriangle): TTriangleType;
begin
  Result := TriangleType(t[0, 0], t[0, 1], t[1, 0], t[1, 1], t[2, 0], t[2, 1]);
end;

function TriangleEdge(const Triangle: TTriangle; const Edge: Integer): TLineV2;
begin
  case Edge of
    1: Result := LineV2(Triangle[0], Triangle[1]);
    2: Result := LineV2(Triangle[1], Triangle[2]);
    3: Result := LineV2(Triangle[2], Triangle[0]);
    else RaiseInfo('error');
  end;
end;

function RectangleEdge(const Rectangle: TRectV2; const Edge: Integer): TLineV2;
begin
  case Edge of
    1: Result := LineV2(Rectangle[0, 0], Rectangle[0, 1], Rectangle[0, 0], Rectangle[0, 1]);
    2: Result := LineV2(Rectangle[1, 0], Rectangle[0, 1], Rectangle[1, 0], Rectangle[1, 1]);
    3: Result := LineV2(Rectangle[1, 0], Rectangle[1, 1], Rectangle[0, 0], Rectangle[1, 1]);
    4: Result := LineV2(Rectangle[0, 0], Rectangle[1, 1], Rectangle[0, 0], Rectangle[0, 1]);
    else RaiseInfo('error');
  end;
end;

function Vec2Transform(const sour, dest: TRectV2; sour_pt: TVec2): TVec2;
begin
  Result := RectProjection(sour, dest, sour_pt);
end;

function Vec2Transform(const sour, dest: TRectV2; const sourAngle, destAngle: TGeoFloat; const sour_pt: TVec2): TVec2;
begin
  Result := RectRotationProjection(sour, dest, sourAngle, destAngle, sour_pt);
end;

function RectTransform(const sour, dest, sour_rect: TRectV2): TRectV2;
begin
  Result := RectProjection(sour, dest, sour_rect);
end;

function RectTransform(const sour, dest: TRectV2; const sour_rect: TRect): TRectV2;
begin
  Result := RectProjection(sour, dest, RectV2(sour_rect));
end;

function RectTransform(const sour, dest: TRectV2; const sour_rect: TRectf): TRectV2;
begin
  Result := RectProjection(sour, dest, RectV2(sour_rect));
end;

function RectScaleSpace(const R: TRectV2; const SS_width, SS_height: TGeoFloat): TRectV2;
begin
  Result := RectFit(RectV2(0, 0, SS_width, SS_height), R);
end;

function RectScaleSpace(const R: TRect; const SS_width, SS_height: TGeoInt): TRect;
begin
  Result := MakeRect(RectScaleSpace(RectV2(R), SS_width, SS_height));
end;

function ComputeScaleSpace(box: TRectV2; SS: TVec2): TRectV2;
begin
  Result := RectScaleSpace(box, SS[0], SS[1]);
end;

function Rect_ScaleSpace_F(R: TRectV2): TGeoFloat;
begin
  Result := RectWidth(R) / RectHeight(R);
end;

function MinLoss_Rect(const SS_width, SS_height: TGeoFloat; const R: TRectV2): TRectV2;
var
  cen, siz: TVec2;
begin
  cen := RectCentre(R);
  siz := RectSize(R);
  if SS_width < SS_height then
      siz[0] := siz[1] * (SS_width / SS_height)
  else
      siz[1] := siz[0] * (SS_height / SS_width);
  Result := RectV2(cen, siz[0], siz[1]);
end;

function MinLoss_Rect(const R: TRectV2; const SS_width, SS_height: TGeoFloat): TRectV2;
begin
  Result := MinLoss_Rect(SS_width, SS_height, R);
end;

function MinLoss_RectScaleSpace(const R: TRectV2; const SS_width, SS_height: TGeoFloat): TRectV2;
begin
  Result := MinLoss_Rect(SS_width, SS_height, R);
end;

function MinLoss_RectFit(const SS_width, SS_height: TGeoFloat; const R: TRectV2): TRectV2;
begin
  Result := MinLoss_Rect(SS_width, SS_height, R);
end;

function MinLoss_RectFit(const R: TRectV2; const SS_width, SS_height: TGeoFloat): TRectV2;
begin
  Result := MinLoss_Rect(SS_width, SS_height, R);
end;

function CalibrationRectInRect(const R, Area: TRectV2): TRectV2;
var
  nr: TRectV2;
begin
  nr := ForwardRect(R);

  if nr[0, 0] < Area[0, 0] then
      nr := RectOffset(nr, vec2(Area[0, 0] - nr[0, 0], 0));
  if nr[0, 1] < Area[0, 1] then
      nr := RectOffset(nr, vec2(0, Area[0, 1] - nr[0, 1]));
  if nr[1, 0] > Area[1, 0] then
      nr := RectOffset(nr, vec2(Area[1, 0] - nr[1, 0], 0));
  if nr[1, 1] > Area[1, 1] then
      nr := RectOffset(nr, vec2(0, Area[1, 1] - nr[1, 1]));

  Result := Clip(nr, Area);
end;

function CalibrationRectInRect(const R, Area: TRect): TRect;
begin
  Result := MakeRect(CalibrationRectInRect(RectV2(R), RectV2(Area)));
end;

function Make_Jitter_Box(rnd: TMT19937Random; XY_Offset_Scale_, Rotate_, Scale_: TGeoFloat; Fit_: Boolean;
  const source: TRectV2; var dest: TRectV2; var Angle: TGeoFloat): TGeoInt;
var
  source_axis: TVec2;
  source_size: TVec2;
  new_axis: TVec2;
  new_size: TVec2;
begin
  source_axis := RectCentre(source);
  source_size := RectSize(source);

  Result := 0;

  repeat
    if Fit_ and (Result > 0) then
        source_size := Vec2Sub(source_size, vec2(umlRRS(rnd, 0, source_size[0] * 0.01), umlRRS(rnd, 0, source_size[1] * 0.01)));

    if XY_Offset_Scale_ > 0 then
      begin
        new_axis[0] := source_axis[0] + umlRRS(rnd, 0 - MinF(source_size) * XY_Offset_Scale_, MinF(source_size) * XY_Offset_Scale_);
        new_axis[1] := source_axis[1] + umlRRS(rnd, 0 - MinF(source_size) * XY_Offset_Scale_, MinF(source_size) * XY_Offset_Scale_);
      end
    else
        new_axis := source_axis;

    if Scale_ > 0 then
      begin
        new_size[0] := source_size[0] + umlRRS(rnd, 0 - source_size[0] * Scale_, source_size[0] * Scale_);
        new_size[1] := source_size[1] + umlRRS(rnd, 0 - source_size[1] * Scale_, source_size[1] * Scale_);
      end
    else
        new_size := source_size;

    if Rotate_ > 0 then
        Angle := umlRRS(rnd, -Rotate_, Rotate_)
    else
        Angle := 0;

    dest := ComputeScaleSpace(RectV2(new_axis, new_size[0], new_size[1]), source_size);

    inc(Result);
  until (not Fit_) or RectInRect(TV2R4.Init(dest, Angle).BoundRect, source);
end;

function Make_Jitter_Box(XY_Offset_Scale_, Rotate_, Scale_: TGeoFloat; Fit_: Boolean;
  const source: TRectV2; var dest: TRectV2; var Angle: TGeoFloat): TGeoInt;
var
  source_axis: TVec2;
  source_size: TVec2;
  new_axis: TVec2;
  new_size: TVec2;
begin
  source_axis := RectCentre(source);
  source_size := RectSize(source);

  Result := 0;

  repeat
    if Fit_ and (Result > 0) then
        source_size := Vec2Sub(source_size, vec2(umlRRS(0, source_size[0] * 0.01), umlRRS(0, source_size[1] * 0.01)));

    if XY_Offset_Scale_ > 0 then
      begin
        new_axis[0] := source_axis[0] + umlRRS(0 - source_size[0] * XY_Offset_Scale_, source_size[0] * XY_Offset_Scale_);
        new_axis[1] := source_axis[1] + umlRRS(0 - source_size[1] * XY_Offset_Scale_, source_size[1] * XY_Offset_Scale_);
      end
    else
        new_axis := source_axis;

    if Scale_ > 0 then
      begin
        new_size[0] := source_size[0] + umlRRS(0 - source_size[0] * Scale_, source_size[0] * Scale_);
        new_size[1] := source_size[1] + umlRRS(0 - source_size[1] * Scale_, source_size[1] * Scale_);
      end
    else
        new_size := source_size;

    if Rotate_ > 0 then
        Angle := umlRRS(-Rotate_, Rotate_)
    else
        Angle := 0;

    dest := ComputeScaleSpace(RectV2(new_axis, new_size[0], new_size[1]), source_size);

    inc(Result);
  until (not Fit_) or RectInRect(TV2R4.Init(dest, Angle).BoundRect, source);
end;

procedure Make_Image_Jitter_Box(
  rand: TMT19937Random;
  image_bound_Box: TRectV2; // image bound box
  scale_size, scale_pos: TVec2; // matrix box
  XY_Offset_Scale_, Rotate_, Scale_: TGeoFloat;
  Fit_Matrix_Box_: Boolean; // min-loss fit
  output_Size: TVec2; // output size
  var dest: TRectV2; var Angle: TGeoFloat // output
  );
begin
  Make_Jitter_Box(rand, XY_Offset_Scale_, Rotate_, Scale_, False, Compute_Scale_Position_To_Min_Edge_Box_Size(image_bound_Box, scale_size, scale_pos), dest, Angle);
  if Fit_Matrix_Box_ then
      dest := MinLoss_RectFit(output_Size[0], output_Size[1], dest);
end;

procedure Make_Image_Jitter_Box(
  image_bound_Box: TRectV2; // image bound box
  scale_size, scale_pos: TVec2; // matrix box
  XY_Offset_Scale_, Rotate_, Scale_: TGeoFloat;
  Fit_Matrix_Box_: Boolean; // min-loss fit
  output_Size: TVec2; // output size
  var dest: TRectV2; var Angle: TGeoFloat // output
  );
begin
  Make_Jitter_Box(XY_Offset_Scale_, Rotate_, Scale_, False, Compute_Scale_Position_To_Min_Edge_Box_Size(image_bound_Box, scale_size, scale_pos), dest, Angle);
  if Fit_Matrix_Box_ then
      dest := MinLoss_RectFit(output_Size[0], output_Size[1], dest);
end;

function Rect_Overlap_or_Intersect(r1, r2: TRectV2): Boolean;
begin
  Result := RectWithInRect(r1, r2) or RectWithInRect(r2, r1) or RectToRectIntersect(r1, r2);
end;

function Rect_1Overlap2_or_Intersect(r1, r2: TRectV2): Boolean;
begin
  Result := RectWithInRect(r1, r2) or RectToRectIntersect(r1, r2);
end;

procedure FixRect(var Left, Top, Right, Bottom: TGeoInt);
begin
  if Bottom < Top then
      TSwap<TGeoInt>.Do_(Bottom, Top);
  if Right < Left then
      TSwap<TGeoInt>.Do_(Right, Left);
end;

procedure FixRect(var Left, Top, Right, Bottom: TGeoFloat);
begin
  if Bottom < Top then
      TSwap<TGeoFloat>.Do_(Bottom, Top);
  if Right < Left then
      TSwap<TGeoFloat>.Do_(Right, Left);
end;

function FixRect(R: TRectV2): TRectV2;
begin
  Result := R;
  FixRect(Result[0, 0], Result[0, 1], Result[1, 0], Result[1, 1]);
end;

function FixRect(R: TRect): TRect;
begin
  Result := R;
  FixRect(Result.Left, Result.Top, Result.Right, Result.Bottom);
end;

procedure FixedRect(var Left, Top, Right, Bottom: TGeoInt);
begin
  if Bottom < Top then
      TSwap<TGeoInt>.Do_(Bottom, Top);
  if Right < Left then
      TSwap<TGeoInt>.Do_(Right, Left);
end;

procedure FixedRect(var Left, Top, Right, Bottom: TGeoFloat);
begin
  if Bottom < Top then
      TSwap<TGeoFloat>.Do_(Bottom, Top);
  if Right < Left then
      TSwap<TGeoFloat>.Do_(Right, Left);
end;

function FixedRect(R: TRectV2): TRectV2;
begin
  Result := R;
  FixedRect(Result[0, 0], Result[0, 1], Result[1, 0], Result[1, 1]);
end;

function FixedRect(R: TRect): TRect;
begin
  Result := R;
  FixedRect(Result.Left, Result.Top, Result.Right, Result.Bottom);
end;

procedure ForwardRect(var Left, Top, Right, Bottom: TGeoInt);
begin
  if Bottom < Top then
      TSwap<TGeoInt>.Do_(Bottom, Top);
  if Right < Left then
      TSwap<TGeoInt>.Do_(Right, Left);
end;

procedure ForwardRect(var Left, Top, Right, Bottom: TGeoFloat);
begin
  if Bottom < Top then
      TSwap<TGeoFloat>.Do_(Bottom, Top);
  if Right < Left then
      TSwap<TGeoFloat>.Do_(Right, Left);
end;

function ForwardRect(R: TRectV2): TRectV2;
begin
  Result := R;
  ForwardRect(Result[0, 0], Result[0, 1], Result[1, 0], Result[1, 1]);
end;

function ForwardRect(R: TRect): TRect;
begin
  Result := R;
  ForwardRect(Result.Left, Result.Top, Result.Right, Result.Bottom);
end;

function MakeRect(const R: TRectV2): TRect;
begin
  Result.Left := Round(R[0, 0]);
  Result.Top := Round(R[0, 1]);
  Result.Right := Round(R[1, 0]);
  Result.Bottom := Round(R[1, 1]);
end;

function MakeRectf(const R: TRectV2): TRectf;
begin
  Result.Left := R[0, 0];
  Result.Top := R[0, 1];
  Result.Right := R[1, 0];
  Result.Bottom := R[1, 1];
end;

function RectWidth(const R: TRectV2): TGeoFloat;
begin
  if R[1, 0] > R[0, 0] then
      Result := R[1, 0] - R[0, 0]
  else
      Result := R[0, 0] - R[1, 0];
end;

function RectHeight(const R: TRectV2): TGeoFloat;
begin
  if R[1, 1] > R[0, 1] then
      Result := R[1, 1] - R[0, 1]
  else
      Result := R[0, 1] - R[1, 1];
end;

function RectWidth(const R: TRect): TGeoInt;
begin
  if R.Right > R.Left then
      Result := R.Right - R.Left
  else
      Result := R.Left - R.Right;
end;

function RectHeight(const R: TRect): TGeoInt;
begin
  if R.Bottom > R.Top then
      Result := R.Bottom - R.Top
  else
      Result := R.Top - R.Bottom;
end;

function RectWidth(const R: TRectf): TGeoFloat;
begin
  if R.Right > R.Left then
      Result := R.Right - R.Left
  else
      Result := R.Left - R.Right;
end;

function RectHeight(const R: TRectf): TGeoFloat;
begin
  if R.Bottom > R.Top then
      Result := R.Bottom - R.Top
  else
      Result := R.Top - R.Bottom;
end;

function RoundWidth(const R: TRectV2): TGeoInt;
begin
  if R[1, 0] > R[0, 0] then
      Result := Round(R[1, 0] - R[0, 0])
  else
      Result := Round(R[0, 0] - R[1, 0]);
end;

function RoundHeight(const R: TRectV2): TGeoInt;
begin
  if R[1, 1] > R[0, 1] then
      Result := Round(R[1, 1] - R[0, 1])
  else
      Result := Round(R[0, 1] - R[1, 1]);
end;

function RoundWidth(const R: TRect): TGeoInt;
begin
  if R.Right > R.Left then
      Result := R.Right - R.Left
  else
      Result := R.Left - R.Right;
end;

function RoundHeight(const R: TRect): TGeoInt;
begin
  if R.Bottom > R.Top then
      Result := R.Bottom - R.Top
  else
      Result := R.Top - R.Bottom;
end;

function RoundWidth(const R: TRectf): TGeoInt;
begin
  if R.Right > R.Left then
      Result := Round(R.Right - R.Left)
  else
      Result := Round(R.Left - R.Right);
end;

function RoundHeight(const R: TRectf): TGeoInt;
begin
  if R.Bottom > R.Top then
      Result := Round(R.Bottom - R.Top)
  else
      Result := Round(R.Top - R.Bottom);
end;

function RectArea(const R: TRectV2): TGeoFloat;
begin
  Result := RectWidth(R) * RectHeight(R);
end;

function RectArea(const R: TRect): TGeoInt;
begin
  Result := RoundWidth(R) * RoundHeight(R);
end;

function RectSize(const R: TRectV2): TVec2;
var
  n: TRectV2;
begin
  n := ForwardRect(R);
  Result := Vec2Sub(n[1], n[0]);
end;

function RectSizeR(const R: TRectV2): TRectV2;
begin
  Result[0] := ZeroVec2;
  Result[1] := RectSize(R)
end;

function RectFit(const sour, dest: TRectV2; const Bound: Boolean): TRectV2;
var
  k, kw, kh: TGeoFloat;
  rs, bs, siz, pt: TVec2;
begin
  rs := RectSize(sour);
  bs := RectSize(dest);

  kw := rs[0] / bs[0];
  kh := rs[1] / bs[1];

  if Bound then
      k := MinF(kw, kh)
  else
      k := MaxF(kw, kh);

  siz := Vec2Div(rs, k);
  pt := Vec2Mul(Vec2Sub(bs, siz), 0.5);
  Result[0] := Vec2Add(dest[0], pt);
  Result[1] := Vec2Add(Result[0], siz);
end;

function RectFit(const sour, dest: TRectV2): TRectV2;
begin
  Result := RectFit(sour, dest, False);
end;

function RectFit(const width, height: TGeoFloat; const bk: TRectV2): TRectV2;
begin
  Result := RectFit(MakeRectV2(0, 0, width, height), bk);
end;

function FitRect(const sour, dest: TRectV2): TRectV2;
begin
  Result := RectFit(sour, dest);
end;

function FitRect(const width, height: TGeoFloat; const bk: TRectV2): TRectV2;
begin
  Result := RectFit(MakeRectV2(0, 0, width, height), bk);
end;

function BoundRect(const buff: TArrayPoint): TRect;
var
  t: TPoint;
  MaxX: TGeoInt;
  MaxY: TGeoInt;
  MinX: TGeoInt;
  MinY: TGeoInt;
  i: TGeoInt;
begin
  if length(buff) > 2 then
    begin
      t := buff[0];
      MinX := t.X;
      MaxX := t.X;
      MinY := t.Y;
      MaxY := t.Y;

      for i := 1 to length(buff) - 1 do
        begin
          t := buff[i];
          if t.X < MinX then
              MinX := t.X
          else if t.X > MaxX then
              MaxX := t.X;
          if t.Y < MinY then
              MinY := t.Y
          else if t.Y > MaxY then
              MaxY := t.Y;
        end;
      Result.Left := MinX;
      Result.Top := MinY;
      Result.Right := MaxX;
      Result.Bottom := MaxY;
    end
  else
    begin
      Result.Left := 0;
      Result.Top := 0;
      Result.Right := 0;
      Result.Bottom := 0;
    end;
end;

function BoundRect(const p1, p2, p3: TPoint): TRect;
var
  buff: TArrayPoint;
begin
  SetLength(buff, 3);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  Result := BoundRect(buff);
  SetLength(buff, 0);
end;

function BoundRect(const p1, p2, p3, p4: TPoint): TRect;
var
  buff: TArrayPoint;
begin
  SetLength(buff, 4);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  buff[3] := p4;
  Result := BoundRect(buff);
  SetLength(buff, 0);
end;

function BoundRect(const r1, r2: TRect): TRect;
begin
  Result := BoundRect(r1.TopLeft, r1.BottomRight, r2.TopLeft, r2.BottomRight);
end;

function BoundRect(const R: TRect; p: TPoint): TRect;
begin
  Result := BoundRect(R.TopLeft, R.BottomRight, p);
end;

function BoundRect(const buff: TArrayVec2): TRectV2;
var
  t: TVec2;
  MaxX: TGeoFloat;
  MaxY: TGeoFloat;
  MinX: TGeoFloat;
  MinY: TGeoFloat;
  i: TGeoInt;
begin
  Result := MakeRectV2(Zero, Zero, Zero, Zero);
  if length(buff) < 2 then
      Exit;
  t := buff[0];
  MinX := t[0];
  MaxX := t[0];
  MinY := t[1];
  MaxY := t[1];

  for i := 1 to length(buff) - 1 do
    begin
      t := buff[i];
      if t[0] < MinX then
          MinX := t[0]
      else if t[0] > MaxX then
          MaxX := t[0];
      if t[1] < MinY then
          MinY := t[1]
      else if t[1] > MaxY then
          MaxY := t[1];
    end;
  Result := MakeRectV2(MinX, MinY, MaxX, MaxY);
end;

function BoundRect(const p1, p2, p3: TVec2): TRectV2;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 3);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  Result := BoundRect(buff);
  SetLength(buff, 0);
end;

function BoundRect(const p1, p2, p3, p4: TVec2): TRectV2;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 4);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  buff[3] := p4;
  Result := BoundRect(buff);
  SetLength(buff, 0);
end;

function BoundRect(const r1, r2: TRectV2): TRectV2;
begin
  Result := BoundRect(r1[0], r1[1], r2[0], r2[1]);
end;

function BoundRect(const r1: TRectV2; const p1: TVec2): TRectV2;
begin
  Result := BoundRect(r1[0], r1[1], p1);
end;

function BoundRect(const r1: TRectV2; const p1, p2: TVec2): TRectV2;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 4);
  buff[0] := r1[0];
  buff[1] := r1[1];
  buff[2] := p1;
  buff[3] := p2;
  Result := BoundRect(buff);
  SetLength(buff, 0);
end;

function BoundRect(const r1: TRectV2; const p1, p2, p3: TVec2): TRectV2;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 5);
  buff[0] := r1[0];
  buff[1] := r1[1];
  buff[2] := p1;
  buff[3] := p2;
  buff[4] := p3;
  Result := BoundRect(buff);
  SetLength(buff, 0);
end;

function BoundRect(const r1: TRectV2; const p1, p2, p3, p4: TVec2): TRectV2;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 6);
  buff[0] := r1[0];
  buff[1] := r1[1];
  buff[2] := p1;
  buff[3] := p2;
  buff[4] := p3;
  buff[5] := p4;
  Result := BoundRect(buff);
  SetLength(buff, 0);
end;

function BuffCentroid(const buff: TArrayVec2): TVec2;
var
  i, Count: TGeoInt;
  asum: TGeoFloat;
  term: TGeoFloat;

  t1, t2: TVec2;
begin
  Result := NULLPoint;
  Count := length(buff);

  if Count = 1 then
      Exit(buff[0]);

  if Count = 2 then
      Exit(MiddleVec2(buff[0], buff[1]));

  if Count < 3 then
      Exit;

  asum := Zero;
  t2 := buff[Count - 1];

  for i := 0 to Count - 1 do
    begin
      t1 := buff[i];

      term := ((t2[0] * t1[1]) - (t2[1] * t1[0]));
      asum := asum + term;
      Result[0] := Result[0] + (t2[0] + t1[0]) * term;
      Result[1] := Result[1] + (t2[1] + t1[1]) * term;
      t2 := t1;
    end;

  if NotEqual(asum, Zero) then
    begin
      Result[0] := Result[0] / (3.0 * asum);
      Result[1] := Result[1] / (3.0 * asum);
    end;
end;

function BuffCentroid(const p1, p2, p3, p4: TVec2): TVec2;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 4);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  buff[3] := p4;
  Result := BuffCentroid(buff);
end;

function BuffCentroid(const p1, p2, p3: TVec2): TVec2;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 3);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  Result := BuffCentroid(buff);
end;

function PointInPolygon(pt: TVec2; const PolygonBuff: TArrayVec2): Boolean;
var
  L, i: TGeoInt;
  pi, pj: TVec2;
begin
  Result := False;
  L := length(PolygonBuff);
  if L < 3 then
      Exit;
  pj := PolygonBuff[L - 1];
  for i := 0 to L - 1 do
    begin
      pi := PolygonBuff[i];
      (* upward crossing and downward crossing *)
      if ((pi[1] <= pt[1]) and (pt[1] < pj[1])) or ((pj[1] <= pt[1]) and (pt[1] < pi[1])) then
        (* compute the edge-ray intersect @ the x-coordinate *)
        if (pt[0] - pi[0] < ((pj[0] - pi[0]) * (pt[1] - pi[1]) / (pj[1] - pi[1]))) then
            Result := not Result;
      pj := pi;
    end;
end;

function PolygonArea(buff: TArrayVec2): TGeoFloat;
var
  i, j: TGeoInt;
begin
  Result := 0;
  if length(buff) < 3 then
      Exit;
  j := length(buff) - 1;
  for i := 0 to length(buff) - 1 do
    begin
      Result := Result + ((buff[j, 0] * buff[i, 1]) - (buff[j, 1] * buff[i, 0]));
      j := i;
    end;
  Result := Result * 0.5;
end;

function FastRamerDouglasPeucker(var Points: TArrayVec2; Epsilon_: TGeoFloat): TGeoInt;
var
  i: TGeoInt;
  Range: array of TGeoInt;
  FirstIndex: TGeoInt;
  LastIndex: TGeoInt;
  LastPoint: TVec2;
  FirstLastDelta: TVec2;
  DeltaMaxIndex: TGeoInt;
  Delta: TGeoFloat;
  DeltaMax: TGeoFloat;
begin
  Result := length(Points);
  if Result < 3 then
      Exit;
  FirstIndex := 0;
  LastIndex := Result - 1;
  SetLength(Range, Result);
  Range[0] := LastIndex;
  Range[LastIndex] := -1;
  Result := 0;

  repeat
    if LastIndex - FirstIndex > 1 then
      begin
        // find the point with the maximum distance
        DeltaMax := 0;
        DeltaMaxIndex := 0;
        LastPoint := Points[LastIndex];
        FirstLastDelta := Vec2Sub(Points[FirstIndex], LastPoint);
        for i := FirstIndex + 1 to LastIndex - 1 do
          begin
            Delta := FAbs((Points[i, 0] - LastPoint[0]) * FirstLastDelta[1] - (Points[i, 1] - LastPoint[1]) * FirstLastDelta[0]);
            if Delta > DeltaMax then
              begin
                DeltaMaxIndex := i;
                DeltaMax := Delta;
              end;
          end;

        // if max distance is greater than Epsilon_, split ranges
        if DeltaMax >= Epsilon_ * HypotX(FirstLastDelta[0], FirstLastDelta[1]) then
          begin
            Range[FirstIndex] := DeltaMaxIndex;
            Range[DeltaMaxIndex] := LastIndex;
            LastIndex := DeltaMaxIndex;
            Continue;
          end;
      end;

    // Include First and Last points only
    if Result <> FirstIndex then
        Points[Result] := Points[FirstIndex];
    inc(Result);
    if Result <> LastIndex then
        Points[Result] := Points[LastIndex];

    // Next range
    FirstIndex := Range[FirstIndex];
    LastIndex := Range[FirstIndex];

  until LastIndex < 0;
  inc(Result);
end;

procedure FastVertexReduction(Points: TArrayVec2; Epsilon_: TGeoFloat; var output: TArrayVec2);

  procedure FilterPoints;
  var
    index: TGeoInt;
    Count: TGeoInt;
    SqrEpsilon: TGeoFloat;
  begin
    SqrEpsilon := Sqr(Epsilon_);
    output := Points;
    Count := 1;
    for index := 1 to high(output) do
      begin
        if SqrDistance(output[Count - 1], Points[index]) > SqrEpsilon then
          begin
            if Count <> index then
                output[Count] := Points[index];
            inc(Count);
          end;
      end;
    SetLength(output, Count);
  end;

var
  Count: TGeoInt;
begin
  FilterPoints;
  Count := FastRamerDouglasPeucker(output, Epsilon_);
  SetLength(output, Count);
end;

function Clip(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat; out Cx1, Cy1, Cx2, Cy2: TGeoFloat): Boolean;
begin
  if RectToRectIntersect(x1, y1, x2, y2, x3, y3, x4, y4) then
    begin
      Result := True;
      if x1 < x3 then
          Cx1 := x3
      else
          Cx1 := x1;

      if x2 > x4 then
          Cx2 := x4
      else
          Cx2 := x2;

      if y1 < y3 then
          Cy1 := y3
      else
          Cy1 := y1;

      if y2 > y4 then
          Cy2 := y4
      else
          Cy2 := y2;
    end
  else
      Result := False;
end;

function Clip(const f, b: TRectV2; var R: TRectV2): Boolean;
var
  f_, b_: TRectV2;
begin
  f_ := ForwardRect(f);
  b_ := ForwardRect(b);

  Result := RectInRect(f_, b_);
  if Result then
      R := f
  else
    begin
      Result := RectInRect(b_, f_);
      if Result then
          R := b
      else
        begin
          Result := Clip(
            f_[0, 0], f_[0, 1], f_[1, 0], f_[1, 1],
            b_[0, 0], b_[0, 1], b_[1, 0], b_[1, 1],
            R[0, 0], R[0, 1], R[1, 0], R[1, 1]
            );
          if not Result then
              R := b_;
        end;
    end;
end;

function Clip(const f, b: TRectV2): TRectV2;
begin
  Clip(f, b, Result);
end;

function Compute_IoU(const r1, r2: TRectV2): TGeoFloat;
var
  R: TRectV2;
  f1, f2, f3: TGeoFloat;
begin
  if not Clip(r1, r2, R) then
      Exit(0);
  f1 := RectArea(r1);
  f2 := RectArea(r2);
  f3 := RectArea(R);
  Result := f3 / (f1 + f2 - f3);
end;

function Compute_IoU(const r1, r2: TRectV2; var R: TRectV2; var IoU: TGeoFloat): Boolean;
var
  f1, f2, f3: TGeoFloat;
begin
  Result := Clip(r1, r2, R);
  if Result then
    begin
      f1 := RectArea(r1);
      f2 := RectArea(r2);
      f3 := RectArea(R);
      IoU := f3 / (f1 + f2 - f3);
    end
  else
      IoU := 0;
end;

function Compute_IoU(const r1, r2: TRectV2; var R: TRectV2; var IoU, R1A, R2A, RA: TGeoFloat): Boolean;
begin
  R1A := RectArea(r1);
  R2A := RectArea(r2);
  Result := Clip(r1, r2, R);
  if Result then
    begin
      RA := RectArea(R);
      IoU := RA / (R1A + R2A - RA);
    end
  else
    begin
      R := NULLRect;
      IoU := 0;
      RA := 0;
    end;
end;

function Orientation(const x1, y1, x2, y2, Px, Py: TGeoFloat): TGeoInt;
var
  Orin: TGeoFloat;
begin
  (* Determinant of the 3 points *)
  Orin := (x2 - x1) * (Py - y1) - (Px - x1) * (y2 - y1);

  if Orin > Zero then
      Result := LeftHandSide (* Orientaion is to the left-hand side *)
  else if Orin < Zero then
      Result := RightHandSide (* Orientaion is to the right-hand side *)
  else
      Result := CollinearOrientation; (* Orientaion is neutral aka collinear *)
end;

function Orientation(const x1, y1, z1, x2, y2, z2, x3, y3, z3, Px, Py, Pz: TGeoFloat): TGeoInt;
var
  Px1: TGeoFloat;
  Px2: TGeoFloat;
  Px3: TGeoFloat;
  Py1: TGeoFloat;
  Py2: TGeoFloat;
  Py3: TGeoFloat;
  Pz1: TGeoFloat;
  Pz2: TGeoFloat;
  Pz3: TGeoFloat;
  Orin: TGeoFloat;
begin
  Px1 := x1 - Px;
  Px2 := x2 - Px;
  Px3 := x3 - Px;

  Py1 := y1 - Py;
  Py2 := y2 - Py;
  Py3 := y3 - Py;

  Pz1 := z1 - Pz;
  Pz2 := z2 - Pz;
  Pz3 := z3 - Pz;

  Orin := Px1 * (Py2 * Pz3 - Pz2 * Py3) +
    Px2 * (Py3 * Pz1 - Pz3 * Py1) +
    Px3 * (Py1 * Pz2 - Pz1 * Py2);

  if Orin < Zero then
      Result := BelowOrientation (* Orientaion is below plane *)
  else if Orin > Zero then
      Result := AboveOrientation (* Orientaion is above plane *)
  else
      Result := CoplanarOrientation; (* Orientaion is coplanar to plane if Result is 0 *)
end;

function Coplanar(const x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4: TGeoFloat): Boolean;
begin
  Result := (Orientation(x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4) = CoplanarOrientation);
end;

function SimpleIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean;
begin
  Result := (
    ((Orientation(x1, y1, x2, y2, x3, y3) * Orientation(x1, y1, x2, y2, x4, y4)) <= 0) and
      ((Orientation(x3, y3, x4, y4, x1, y1) * Orientation(x3, y3, x4, y4, x2, y2)) <= 0)
    );
end;

function SimpleIntersect(const Point1, Point2, Point3, Point4: TVec2): Boolean;
begin
  Result := SimpleIntersect(Point1[0], Point1[1], Point2[0], Point2[1], Point3[0], Point3[1], Point4[0], Point4[1]);
end;

function SimpleIntersect(const l1, l2: TLineV2): Boolean;
begin
  Result := SimpleIntersect(
    l1[0, 0], l1[0, 1], l1[1, 0], l1[1, 1],
    l2[0, 0], l2[0, 1], l2[1, 0], l2[1, 1]);
end;

function Intersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean;
var
  UpperX: TGeoFloat;
  UpperY: TGeoFloat;
  LowerX: TGeoFloat;
  LowerY: TGeoFloat;
  Ax: TGeoFloat;
  Bx: TGeoFloat;
  Cx: TGeoFloat;
  Ay: TGeoFloat;
  By: TGeoFloat;
  Cy: TGeoFloat;
  d: TGeoFloat;
  f: TGeoFloat;
  E: TGeoFloat;
begin
  Result := False;

  Ax := x2 - x1;
  Bx := x3 - x4;

  if Ax < Zero then
    begin
      LowerX := x2;
      UpperX := x1;
    end
  else
    begin
      UpperX := x2;
      LowerX := x1;
    end;

  if Bx > Zero then
    begin
      if (UpperX < x4) or (x3 < LowerX) then
          Exit;
    end
  else if (UpperX < x3) or (x4 < LowerX) then
      Exit;

  Ay := y2 - y1;
  By := y3 - y4;

  if Ay < Zero then
    begin
      LowerY := y2;
      UpperY := y1;
    end
  else
    begin
      UpperY := y2;
      LowerY := y1;
    end;

  if By > Zero then
    begin
      if (UpperY < y4) or (y3 < LowerY) then
          Exit;
    end
  else if (UpperY < y3) or (y4 < LowerY) then
      Exit;

  Cx := x1 - x3;
  Cy := y1 - y3;
  d := (By * Cx) - (Bx * Cy);
  f := (Ay * Bx) - (Ax * By);

  if f > Zero then
    begin
      if (d < Zero) or (d > f) then
          Exit;
    end
  else if (d > Zero) or (d < f) then
      Exit;

  E := (Ax * Cy) - (Ay * Cx);

  if f > Zero then
    begin
      if (E < Zero) or (E > f) then
          Exit;
    end
  else if (E > Zero) or (E < f) then
      Exit;

  Result := True;
end;

function Intersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat; out ix, iy: TGeoFloat): Boolean;
var
  UpperX: TGeoFloat;
  UpperY: TGeoFloat;
  LowerX: TGeoFloat;
  LowerY: TGeoFloat;
  Ax: TGeoFloat;
  Bx: TGeoFloat;
  Cx: TGeoFloat;
  Ay: TGeoFloat;
  By: TGeoFloat;
  Cy: TGeoFloat;
  d: TGeoFloat;
  f: TGeoFloat;
  E: TGeoFloat;
  Ratio: TGeoFloat;
begin
  Result := False;

  Ax := x2 - x1;
  Bx := x3 - x4;

  if Ax < Zero then
    begin
      LowerX := x2;
      UpperX := x1;
    end
  else
    begin
      UpperX := x2;
      LowerX := x1;
    end;

  if Bx > Zero then
    begin
      if (UpperX < x4) or (x3 < LowerX) then
          Exit;
    end
  else if (UpperX < x3) or (x4 < LowerX) then
      Exit;

  Ay := y2 - y1;
  By := y3 - y4;

  if Ay < Zero then
    begin
      LowerY := y2;
      UpperY := y1;
    end
  else
    begin
      UpperY := y2;
      LowerY := y1;
    end;

  if By > Zero then
    begin
      if (UpperY < y4) or (y3 < LowerY) then
          Exit;
    end
  else if (UpperY < y3) or (y4 < LowerY) then
      Exit;

  Cx := x1 - x3;
  Cy := y1 - y3;
  d := (By * Cx) - (Bx * Cy);
  f := (Ay * Bx) - (Ax * By);

  if f > Zero then
    begin
      if (d < Zero) or (d > f) then
          Exit;
    end
  else if (d > Zero) or (d < f) then
      Exit;

  E := (Ax * Cy) - (Ay * Cx);

  if f > Zero then
    begin
      if (E < Zero) or (E > f) then
          Exit;
    end
  else if (E > Zero) or (E < f) then
      Exit;

  Result := True;

  (*
    From IntersectionPoint Routine

    dx1 := x2 - x1; ->  Ax
    dx2 := x4 - x3; -> -Bx
    dx3 := x1 - x3; ->  Cx

    dy1 := y2 - y1; ->  Ay
    dy2 := y1 - y3; ->  Cy
    dy3 := y4 - y3; -> -By
  *)

  Ratio := (Ax * -By) - (Ay * -Bx);

  if NotEqual(Ratio, Zero) then
    begin
      Ratio := ((Cy * -Bx) - (Cx * -By)) / Ratio;
      ix := x1 + (Ratio * Ax);
      iy := y1 + (Ratio * Ay);
    end
  else
    begin
      if IsEqual((Ax * -Cy), (-Cx * Ay)) then
        begin
          ix := x3;
          iy := y3;
        end
      else
        begin
          ix := x4;
          iy := y4;
        end;
    end;
end;

function Intersect(const pt1, pt2, pt3, pt4: TVec2; out pt: TVec2): Boolean;
begin
  Result := Intersect(pt1[0], pt1[1], pt2[0], pt2[1], pt3[0], pt3[1], pt4[0], pt4[1], pt[0], pt[1]);
end;

function Intersect(const l1, l2: TLineV2; out pt: TVec2): Boolean;
begin
  Result := Intersect(
    l1[0, 0], l1[0, 1], l1[1, 0], l1[1, 1],
    l2[0, 0], l2[0, 1], l2[1, 0], l2[1, 1],
    pt[0], pt[1]);
end;

function Intersect(const pt1, pt2, pt3, pt4: TVec2): Boolean;
begin
  Result := Intersect(pt1[0], pt1[1], pt2[0], pt2[1], pt3[0], pt3[1], pt4[0], pt4[1]);
end;

function PointInCircle(const pt, cp: TVec2; radius: TGeoFloat): Boolean;
begin
  Result := (PointDistance(pt, cp) <= (radius + radius));
end;

function Vec2InCircle(const pt, cp: TVec2; radius: TGeoFloat): Boolean;
begin
  Result := (PointDistance(pt, cp) <= (radius + radius));
end;

function PointInTriangle(const Px, Py, x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean;
var
  Or1, Or2, Or3: TGeoInt;
begin
  Or1 := Orientation(x1, y1, x2, y2, Px, Py);
  Or2 := Orientation(x2, y2, x3, y3, Px, Py);

  if (Or1 * Or2) = -1 then
      Result := False
  else
    begin
      Or3 := Orientation(x3, y3, x1, y1, Px, Py);
      if (Or1 = Or3) or (Or3 = 0) then
          Result := True
      else if Or1 = 0 then
          Result := (Or2 * Or3) >= 0
      else if Or2 = 0 then
          Result := (Or1 * Or3) >= 0
      else
          Result := False;
    end;
end;

procedure BuildSinCosCache(const oSin, oCos: PGeoFloatArray; const b, E: TGeoFloat);
var
  i: TGeoInt;
  startAngle, stopAngle, d, alpha, beta: TGeoFloat;
begin
  startAngle := b;
  stopAngle := E + 1E-5;
  if high(oSin^) > low(oSin^) then
      d := PIDiv180 * (stopAngle - startAngle) / (high(oSin^) - low(oSin^))
  else
      d := 0;

  if high(oSin^) - low(oSin^) < 1000 then
    begin
      // Fast computation (approx 5.5x)
      alpha := 2 * Sqr(Sin(d * 0.5));
      beta := Sin(d);
      SinCos_(startAngle * PIDiv180, oSin^[low(oSin^)], oCos^[low(oSin^)]);
      for i := low(oSin^) to high(oSin^) - 1 do
        begin
          // Make use of the incremental formulae:
          // cos (theta+delta) = cos(theta) - [alpha*cos(theta) + beta*sin(theta)]
          // sin (theta+delta) = sin(theta) - [alpha*sin(theta) - beta*cos(theta)]
          oCos^[i + 1] := oCos^[i] - alpha * oCos^[i] - beta * oSin^[i];
          oSin^[i + 1] := oSin^[i] - alpha * oSin^[i] + beta * oCos^[i];
        end;
    end
  else
    begin
      // Slower, but maintains precision when steps are small
      startAngle := startAngle * PIDiv180;
      for i := low(oSin^) to high(oSin^) do
          SinCos_((i - low(oSin^)) * d + startAngle, oSin^[i], oCos^[i]);
    end;
end;

procedure ClosestPointOnSegmentFromPoint(const x1, y1, x2, y2, Px, Py: TGeoFloat; out Nx, Ny: TGeoFloat);
var
  Vx: TGeoFloat;
  Vy: TGeoFloat;
  Wx: TGeoFloat;
  Wy: TGeoFloat;
  C1: TGeoFloat;
  C2: TGeoFloat;
  Ratio: TGeoFloat;
begin
  Vx := x2 - x1;
  Vy := y2 - y1;
  Wx := Px - x1;
  Wy := Py - y1;

  C1 := Vx * Wx + Vy * Wy;

  if C1 <= 0.0 then
    begin
      Nx := x1;
      Ny := y1;
      Exit;
    end;

  C2 := Vx * Vx + Vy * Vy;

  if C2 <= C1 then
    begin
      Nx := x2;
      Ny := y2;
      Exit;
    end;

  Ratio := C1 / C2;

  Nx := x1 + Ratio * Vx;
  Ny := y1 + Ratio * Vy;
end;

function ClosestPointOnSegmentFromPoint(const lb, le, pt: TVec2): TVec2;
begin
  ClosestPointOnSegmentFromPoint(lb[0], lb[1], le[0], le[1], pt[0], pt[1], Result[0], Result[1]);
end;

function ClosestPointOnSegmentFromLine(const L: TLineV2; const pt: TVec2): TVec2;
begin
  ClosestPointOnSegmentFromPoint(L[0, 0], L[0, 1], L[1, 0], L[1, 1], pt[0], pt[1], Result[0], Result[1]);
end;

function ClosestPointOnSegmentFromLine(const pt: TVec2; const L: TLineV2): TVec2;
begin
  ClosestPointOnSegmentFromPoint(L[0, 0], L[0, 1], L[1, 0], L[1, 1], pt[0], pt[1], Result[0], Result[1]);
end;

function MinimumDistanceFromPointToLine(const Px, Py, x1, y1, x2, y2: TGeoFloat): TGeoFloat;
var
  Nx: TGeoFloat;
  Ny: TGeoFloat;
begin
  ClosestPointOnSegmentFromPoint(x1, y1, x2, y2, Px, Py, Nx, Ny);
  Result := Distance(Px, Py, Nx, Ny);
end;

function MinimumDistanceFromPointToLine(const pt: TVec2; const L: TLineV2): TGeoFloat;
begin
  Result := MinimumDistanceFromPointToLine(pt[0], pt[1], L[0, 0], L[0, 1], L[1, 0], L[1, 1]);
end;

function MinimumDistanceFromPointToLine(const L: TLineV2; const pt: TVec2): TGeoFloat;
begin
  Result := MinimumDistanceFromPointToLine(pt[0], pt[1], L[0, 0], L[0, 1], L[1, 0], L[1, 1]);
end;

function MinimumDistanceFromPointToLine(const lb, le, pt: TVec2): TGeoFloat;
begin
  Result := MinimumDistanceFromPointToLine(LineV2(lb, le), pt);
end;

function Compute_Scale_Position_To_Abs_Size(box: TRectV2; size, sPos: TVec2): TRectV2;
begin
  Result[0] := Vec2Add(box[0], Vec2Mul(Vec2Sub(RectSize(box), size), sPos));
  Result[1] := Vec2Add(Result[0], size);
end;

function Compute_Scale_Position_To_Box_Size(box: TRectV2; size, sPos: TVec2): TRectV2;
begin
  Result := Compute_Scale_Position_To_Abs_Size(box, Vec2Mul(FAbs(size), RectSize(box)), sPos);
end;

function Compute_Scale_Position_To_Min_Edge_Box_Size(box: TRectV2; size, sPos: TVec2): TRectV2;
begin
  Result := Compute_Scale_Position_To_Abs_Size(box, Vec2Mul(FAbs(size), MinF(RectSize(box))), sPos);
end;

function RectProjection(const sour, dest: TRectV2; const sour_pt: TVec2): TVec2;
var
  s, d: TRectV2;
  f: TVec2;
begin
  s := ForwardRect(sour);
  d := ForwardRect(dest);
  f := Vec2Div(Vec2Sub(d[1], d[0]), Vec2Sub(s[1], s[0]));
  Result := Vec2Add(Vec2Mul(Vec2Sub(sour_pt, s[0]), f), d[0]);
end;

function RectProjection(const sour, dest: TRectV2; const sour_rect: TRectV2): TRectV2;
var
  s, d: TRectV2;
  f: TVec2;
begin
  s := ForwardRect(sour);
  d := ForwardRect(dest);
  f := Vec2Div(Vec2Sub(d[1], d[0]), Vec2Sub(s[1], s[0]));
  Result[0] := Vec2Add(Vec2Mul(Vec2Sub(sour_rect[0], s[0]), f), d[0]);
  Result[1] := Vec2Add(Vec2Mul(Vec2Sub(sour_rect[1], s[0]), f), d[0]);
end;

function RectProjection(const sour, dest: TRectV2; const sour_arry: TArrayVec2): TArrayVec2;
begin
  Result := RectProjectionArrayV2(sour, dest, sour_arry);
end;

function RectProjection(const sour, dest: TRectV2; const sour_r4: TV2R4): TV2R4;
begin
  Result := sour_r4.Projection(sour, dest);
end;

function RectProjectionArrayV2(const sour, dest: TRectV2; const sour_arry: TArrayVec2): TArrayVec2;
var
  s, d: TRectV2;
  f: TVec2;
  i: Integer;
begin
  s := ForwardRect(sour);
  d := ForwardRect(dest);
  f := Vec2Div(Vec2Sub(d[1], d[0]), Vec2Sub(s[1], s[0]));
  SetLength(Result, length(sour_arry));
  for i := 0 to length(sour_arry) - 1 do
      Result[i] := Vec2Add(Vec2Mul(Vec2Sub(sour_arry[i], s[0]), f), d[0]);
end;

function RectProjectionRotationDest(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2;
var
  tmp: TVec2;
begin
  tmp := RectProjection(sour, dest, sour_pt);
  Result := Vec2Rotation(axis, tmp, NormalizeDegAngle(Vec2Angle(axis, tmp) + Angle));
end;

function RectProjectionRotationDest(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2;
begin
  Result[0] := RectProjectionRotationDest(sour, dest, axis, Angle, sour_rect[0]);
  Result[1] := RectProjectionRotationDest(sour, dest, axis, Angle, sour_rect[1]);
end;

function RectProjectionRotationSource(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2;
begin
  Result := RectProjection(sour, dest, Vec2Rotation(sour, axis, Angle, sour_pt));
end;

function RectProjectionRotationSource(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2;
begin
  Result[0] := RectProjectionRotationSource(sour, dest, axis, Angle, sour_rect[0]);
  Result[1] := RectProjectionRotationSource(sour, dest, axis, Angle, sour_rect[1]);
end;

function RectProjectionRotationDest(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2;
begin
  Result := RectProjectionRotationDest(sour, dest, RectCentre(dest), Angle, sour_pt);
end;

function RectProjectionRotationDest(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2;
begin
  Result[0] := RectProjectionRotationDest(sour, dest, Angle, sour_rect[0]);
  Result[1] := RectProjectionRotationDest(sour, dest, Angle, sour_rect[1]);
end;

function RectProjectionRotationSource(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2;
begin
  Result := RectProjectionRotationSource(sour, dest, RectCentre(sour), Angle, sour_pt);
end;

function RectProjectionRotationSource(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2;
begin
  Result[0] := RectProjectionRotationSource(sour, dest, Angle, sour_rect[0]);
  Result[1] := RectProjectionRotationSource(sour, dest, Angle, sour_rect[1]);
end;

function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAxis, destAxis: TVec2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_pt: TVec2): TVec2;
begin
  Result := RectProjectionRotationDest(sour, dest, destAxis, destAngle, Vec2Rotation(sour, sourAxis, sourAngle, sour_pt));
end;

function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAxis, destAxis: TVec2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_rect: TRectV2): TRectV2;
begin
  Result[0] := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, sour_rect[0]);
  Result[1] := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, sour_rect[1]);
end;

function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_pt: TVec2): TVec2;
begin
  Result := RectRotationProjection(sour, dest, RectCentre(sour), RectCentre(dest), sourAngle, destAngle, sour_pt);
end;

function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_rect: TRectV2): TRectV2;
begin
  Result[0] := RectRotationProjection(sour, dest, sourAngle, destAngle, sour_rect[0]);
  Result[1] := RectRotationProjection(sour, dest, sourAngle, destAngle, sour_rect[1]);
end;

function Quadrant(const Angle: TGeoFloat): TGeoInt;
begin
  Result := 0;
  if (Angle >= 0.0) and (Angle < 90.0) then
      Result := 1
  else if (Angle >= 90.0) and (Angle < 180.0) then
      Result := 2
  else if (Angle >= 180.0) and (Angle < 270.0) then
      Result := 3
  else if (Angle >= 270.0) and (Angle < 360.0) then
      Result := 4
  else if Angle = 360.0 then
      Result := 1;
end;

procedure ProjectionPoint(const Srcx, Srcy, Dstx, Dsty, Dist: TGeoFloat; out Nx, Ny: TGeoFloat);
var
  DistRatio: TGeoFloat;
begin
  DistRatio := Dist / Distance(Srcx, Srcy, Dstx, Dsty);
  Nx := Srcx + DistRatio * (Dstx - Srcx);
  Ny := Srcy + DistRatio * (Dsty - Srcy);
end;

procedure ProjectionPoint(const Srcx, Srcy, Srcz, Dstx, Dsty, Dstz, Dist: TGeoFloat; out Nx, Ny, Nz: TGeoFloat);
var
  DistRatio: TGeoFloat;
begin
  DistRatio := Dist / Distance(Srcx, Srcy, Srcz, Dstx, Dsty, Dstz);
  Nx := Srcx + DistRatio * (Dstx - Srcx);
  Ny := Srcy + DistRatio * (Dsty - Srcy);
  Nz := Srcz + DistRatio * (Dstz - Srcz);
end;
(* End of Project Point 3D *)

procedure ProjectionPoint(const Px, Py, Angle, Distance: TGeoFloat; out Nx, Ny: TGeoFloat);
var
  Dx: TGeoFloat;
  Dy: TGeoFloat;
begin
  Dx := Zero;
  Dy := Zero;
  case Quadrant(Angle) of
    1:
      begin
        Dx := Cos(Angle * PIDiv180) * Distance;
        Dy := Sin(Angle * PIDiv180) * Distance;
      end;
    2:
      begin
        Dx := Sin((Angle - 90.0) * PIDiv180) * Distance * -1.0;
        Dy := Cos((Angle - 90.0) * PIDiv180) * Distance;
      end;
    3:
      begin
        Dx := Cos((Angle - 180.0) * PIDiv180) * Distance * -1.0;
        Dy := Sin((Angle - 180.0) * PIDiv180) * Distance * -1.0;
      end;
    4:
      begin
        Dx := Sin((Angle - 270.0) * PIDiv180) * Distance;
        Dy := Cos((Angle - 270.0) * PIDiv180) * Distance * -1.0;
      end;
  end;
  Nx := Px + Dx;
  Ny := Py + Dy;
end;

function GetCicleRadiusInPolyEdge(R: TGeoFloat; PolySlices: TGeoInt): TGeoFloat;
begin
  Result := R / Sin((180 - 360 / PolySlices) * 0.5 / 180 * pi);
end;

procedure Circle2LineIntersectionPoint(const lb, le, cp: TVec2; const radius: TGeoFloat;
  out pt1in, pt2in: Boolean; out ICnt: TGeoInt; out pt1, pt2: TVec2);
var
  Px: TGeoFloat;
  Py: TGeoFloat;
  S1In: Boolean;
  s2In: Boolean;
  H: TGeoFloat;
  a: TGeoFloat;
begin
  ICnt := 0;

  S1In := PointInCircle(lb, cp, radius);
  s2In := PointInCircle(le, cp, radius);

  if S1In and s2In then
    begin
      ICnt := 2;
      pt1 := lb;
      pt2 := le;
      pt1in := True;
      pt2in := True;
      Exit;
    end;

  if S1In or s2In then
    begin
      pt1in := True;
      pt2in := False;
      ICnt := 2;
      ClosestPointOnSegmentFromPoint(lb[0], lb[1], le[0], le[1], cp[0], cp[1], Px, Py);
      if S1In then
        begin
          H := Distance(Px, Py, cp[0], cp[1]);
          a := Sqrt((radius * radius) - (H * H));
          pt1 := lb;
          ProjectionPoint(Px, Py, le[0], le[1], a, pt2[0], pt2[1]);
        end
      else if s2In then
        begin
          H := Distance(Px, Py, cp[0], cp[1]);
          a := Sqrt((radius * radius) - (H * H));
          pt1 := le;
          ProjectionPoint(Px, Py, lb[0], lb[1], a, pt2[0], pt2[1]);
        end;
      Exit;
    end;

  pt1in := False;
  pt2in := False;

  ClosestPointOnSegmentFromPoint(lb[0], lb[1], le[0], le[1], cp[0], cp[1], Px, Py);

  if (IsEqual(lb[0], Px) and IsEqual(lb[1], Py)) or (IsEqual(le[0], Px) and IsEqual(le[1], Py)) then
      Exit
  else
    begin
      H := Distance(Px, Py, cp[0], cp[1]);
      if H > radius then
          Exit
      else if IsEqual(H, radius) then
        begin
          ICnt := 1;
          pt1[0] := Px;
          pt1[1] := Py;
          Exit;
        end
      else if IsEqual(H, Zero) then
        begin
          ICnt := 2;
          ProjectionPoint(cp[0], cp[1], lb[0], lb[1], radius, pt1[0], pt1[1]);
          ProjectionPoint(cp[0], cp[1], le[0], le[1], radius, pt2[0], pt2[1]);
          Exit;
        end
      else
        begin
          ICnt := 2;
          a := Sqrt((radius * radius) - (H * H));
          ProjectionPoint(Px, Py, lb[0], lb[1], a, pt1[0], pt1[1]);
          ProjectionPoint(Px, Py, le[0], le[1], a, pt2[0], pt2[1]);
          Exit;
        end;
    end;
end;

procedure Circle2LineIntersectionPoint(const L: TLineV2; const cp: TVec2; radius: TGeoFloat;
  out pt1in, pt2in: Boolean; out ICnt: TGeoInt; out pt1, pt2: TVec2);
begin
  Circle2LineIntersectionPoint(L[0], L[1], cp, radius, pt1in, pt2in, ICnt, pt1, pt2);
end;

procedure Circle2CircleIntersectionPoint(const cp1, cp2: TVec2; const r1, r2: TGeoFloat; out Point1, Point2: TVec2);
var
  Dist: TGeoFloat;
  a: TGeoFloat;
  H: TGeoFloat;
  RatioA: TGeoFloat;
  RatioH: TGeoFloat;
  Dx: TGeoFloat;
  Dy: TGeoFloat;
  Phi: TVec2;
  r1Sqr: TGeoFloat;
  r2Sqr: TGeoFloat;
  dstSqr: TGeoFloat;
begin
  Dist := Distance(cp1[0], cp1[1], cp2[0], cp2[1]);

  dstSqr := Dist * Dist;
  r1Sqr := r1 * r1;
  r2Sqr := r2 * r2;

  a := (dstSqr - r2Sqr + r1Sqr) / (2 * Dist);
  H := Sqrt(r1Sqr - (a * a));

  RatioA := a / Dist;
  RatioH := H / Dist;

  Dx := cp2[0] - cp1[0];
  Dy := cp2[1] - cp1[1];

  Phi[0] := cp1[0] + (RatioA * Dx);
  Phi[1] := cp1[1] + (RatioA * Dy);

  Dx := Dx * RatioH;
  Dy := Dy * RatioH;

  Point1[0] := Phi[0] + Dy;
  Point1[1] := Phi[1] - Dx;

  Point2[0] := Phi[0] - Dy;
  Point2[1] := Phi[1] + Dx;
end;

function Detect_Circle2Circle(const p1, p2: TVec2; const r1, r2: TGeoFloat): Boolean;
begin
  // return point disace < sum
  Result := PointDistance(p1, p2) <= r1 + r2;
end;

function CircleCollision(const p1, p2: TVec2; const r1, r2: TGeoFloat): Boolean;
begin
  // return point disace < sum
  Result := PointDistance(p1, p2) <= r1 + r2;
end;

function Detect_Circle2CirclePoint(const p1, p2: TVec2; const r1, r2: TGeoFloat; out op1, op2: TVec2): Boolean;
var
  Dist: TGeoFloat;
  a: TGeoFloat;
  H: TGeoFloat;
  RatioA: TGeoFloat;
  RatioH: TGeoFloat;
  Dx: TGeoFloat;
  Dy: TGeoFloat;
  Phi: TVec2;
  r1Sqr: TGeoFloat;
  r2Sqr: TGeoFloat;
  dstSqr: TGeoFloat;
begin
  Dist := Distance(p1[0], p1[1], p2[0], p2[1]);
  Result := Dist <= r1 + r2;
  if Result then
    begin
      dstSqr := Dist * Dist;
      r1Sqr := r1 * r1;
      r2Sqr := r2 * r2;

      a := (dstSqr - r2Sqr + r1Sqr) / (2 * Dist);
      H := Sqrt(r1Sqr - (a * a));

      RatioA := a / Dist;
      RatioH := H / Dist;

      Dx := p2[0] - p1[0];
      Dy := p2[1] - p1[1];

      Phi[0] := p1[0] + (RatioA * Dx);
      Phi[1] := p1[1] + (RatioA * Dy);

      Dx := Dx * RatioH;
      Dy := Dy * RatioH;

      op1[0] := Phi[0] + Dy;
      op1[1] := Phi[1] - Dx;

      op2[0] := Phi[0] - Dy;
      op2[1] := Phi[1] + Dx;
    end;
end;

// circle 2 line collision

function Detect_Circle2Line(const cp: TVec2; const R: TGeoFloat; const lb, le: TVec2): Boolean;
var
  lineCen, v1, v2: TVec2;
begin
  lineCen := PointLerp(lb, le, 0.5);
  if Detect_Circle2Circle(cp, lineCen, R, PointDistance(lb, le) * 0.5) then
    begin
      v1 := Vec2Sub(lb, cp);
      v2 := Vec2Sub(le, cp);
      Result := GreaterThanOrEqual(((R * R) * PointLayDistance(v1, v2) - Sqr(v1[0] * v2[1] - v1[1] * v2[0])), Zero);
    end
  else
      Result := False;
end;

function Detect_Circle2Line(const cp: TVec2; const R: TGeoFloat; const L: TLineV2): Boolean;
begin
  Result := Detect_Circle2Line(cp, R, L[0], L[1]);
end;

function RectangularHull(const buff: TArrayVec2): TRectV2;
begin
  Result := BoundRect(buff);
end;

function SameLinePtr(const lb1, le1, lb2, le2: PVec2): Boolean;
begin
  Result := ((lb1 = lb2) and (le1 = le2)) or ((lb1 = le2) and (le1 = lb2));
end;

function ComputeCurvePartPrecision(const pt1, pt2, pt3, pt4: TVec2): TGeoInt;
const
  AcceptedDeviation = 0.1;
var
  len: TGeoFloat;
begin
  len := Sqr(pt1[0] - pt2[0]) + Sqr(pt1[1] - pt2[1]);
  len := MaxF(len, Sqr(pt3[0] - pt2[0]) + Sqr(pt3[1] - pt2[1]));
  len := MaxF(len, Sqr(pt3[0] - pt4[0]) + Sqr(pt3[1] - pt4[1]));
  Result := Round(Sqrt(Sqrt(len) / AcceptedDeviation) * 0.9);
  if Result <= 0 then
      Result := 1;
end;

function Interpolation_OutSide(const T_: TGeoFloat): TGeoFloat;
const
  Coeff = 0.5;
var
  t, tt, ttt: TGeoFloat;
begin
  t := abs(T_);
  tt := Sqr(t);
  ttt := tt * t;
  if t < 1 then
      Result := (2 - Coeff) * ttt - (3 - Coeff) * tt + 1
  else if t < 2 then
      Result := -Coeff * (ttt - 5 * tt + 8 * t - 4)
  else
      Result := 0;
end;

function Interpolation_InSide(const t: TGeoFloat): TGeoFloat;
  function pow3(X: TGeoFloat): TGeoFloat;
  begin
    if X <= 0.0 then
        Result := 0.0
    else
        Result := X * X * X;
  end;

const
  globalfactor = 1 / 6;

begin
  if t > 2 then
      Result := 0
  else
      Result := globalfactor * (pow3(t + 2) - 4 * pow3(t + 1) + 6 * pow3(t) - 4 * pow3(t - 1));
end;

procedure TV2Rect4.Reset;
begin
  LeftTop := NULLPoint;
  RightTop := NULLPoint;
  RightBottom := NULLPoint;
  LeftBottom := NULLPoint;
end;

function TV2Rect4.IsZero: Boolean;
begin
  Result :=
    Z.Geometry2D.IsZero(LeftTop) and
    Z.Geometry2D.IsZero(RightTop) and
    Z.Geometry2D.IsZero(RightBottom) and
    Z.Geometry2D.IsZero(LeftBottom);
end;

function TV2Rect4.BoundArea: TGeoFloat;
begin
  Result := RectArea(BoundRect());
end;

function TV2Rect4.Area: TGeoFloat;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 4);
  buff[0] := LeftTop;
  buff[1] := RightTop;
  buff[2] := RightBottom;
  buff[3] := LeftBottom;
  Result := PolygonArea(buff);
  SetLength(buff, 0);
end;

function TV2Rect4.Rotation(Angle: TGeoFloat): TV2Rect4;
var
  axis: TVec2;
begin
  axis := Centroid;
  Result.LeftTop := PointRotation(axis, LeftTop, PointAngle(axis, LeftTop) + Angle);
  Result.RightTop := PointRotation(axis, RightTop, PointAngle(axis, RightTop) + Angle);
  Result.RightBottom := PointRotation(axis, RightBottom, PointAngle(axis, RightBottom) + Angle);
  Result.LeftBottom := PointRotation(axis, LeftBottom, PointAngle(axis, LeftBottom) + Angle);
end;

function TV2Rect4.Rotation(axis: TVec2; Angle: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := PointRotation(axis, LeftTop, PointAngle(axis, LeftTop) + Angle);
  Result.RightTop := PointRotation(axis, RightTop, PointAngle(axis, RightTop) + Angle);
  Result.RightBottom := PointRotation(axis, RightBottom, PointAngle(axis, RightBottom) + Angle);
  Result.LeftBottom := PointRotation(axis, LeftBottom, PointAngle(axis, LeftBottom) + Angle);
end;

function TV2Rect4.TransformToRect(box: TRectV2; Edge: TGeoFloat): TV2Rect4;
var
  boxSelf, nArea: TRectV2;
begin
  boxSelf := BoundRect();
  nArea := RectEdge(box, -abs(Edge));
  Result.LeftTop := RectProjection(boxSelf, nArea, LeftTop);
  Result.RightTop := RectProjection(boxSelf, nArea, RightTop);
  Result.RightBottom := RectProjection(boxSelf, nArea, RightBottom);
  Result.LeftBottom := RectProjection(boxSelf, nArea, LeftBottom);
end;

function TV2Rect4.TransformToRect(box: TRectV2; Angle, Edge: TGeoFloat): TV2Rect4;
var
  boxSelf, nArea: TRectV2;
begin
  boxSelf := BoundRect();
  nArea := RectEdge(box, -abs(Edge));
  Result.LeftTop := RectProjectionRotationDest(boxSelf, nArea, Angle, LeftTop);
  Result.RightTop := RectProjectionRotationDest(boxSelf, nArea, Angle, RightTop);
  Result.RightBottom := RectProjectionRotationDest(boxSelf, nArea, Angle, RightBottom);
  Result.LeftBottom := RectProjectionRotationDest(boxSelf, nArea, Angle, LeftBottom);
end;

function TV2Rect4.TransformToRect(box: TRectV2; axis: TVec2; Angle, Edge: TGeoFloat): TV2Rect4;
var
  boxSelf, nArea: TRectV2;
begin
  boxSelf := BoundRect();
  nArea := RectEdge(box, -abs(Edge));
  Result.LeftTop := RectProjectionRotationDest(boxSelf, nArea, axis, Angle, LeftTop);
  Result.RightTop := RectProjectionRotationDest(boxSelf, nArea, axis, Angle, RightTop);
  Result.RightBottom := RectProjectionRotationDest(boxSelf, nArea, axis, Angle, RightBottom);
  Result.LeftBottom := RectProjectionRotationDest(boxSelf, nArea, axis, Angle, LeftBottom);
end;

function TV2Rect4.Add(V: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Add(LeftTop, V);
  Result.RightTop := Vec2Add(RightTop, V);
  Result.RightBottom := Vec2Add(RightBottom, V);
  Result.LeftBottom := Vec2Add(LeftBottom, V);
end;

function TV2Rect4.Sub(V: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Sub(LeftTop, V);
  Result.RightTop := Vec2Sub(RightTop, V);
  Result.RightBottom := Vec2Sub(RightBottom, V);
  Result.LeftBottom := Vec2Sub(LeftBottom, V);
end;

function TV2Rect4.Mul(V: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Mul(LeftTop, V);
  Result.RightTop := Vec2Mul(RightTop, V);
  Result.RightBottom := Vec2Mul(RightBottom, V);
  Result.LeftBottom := Vec2Mul(LeftBottom, V);
end;

function TV2Rect4.Mul(V: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := Vec2Mul(LeftTop, V);
  Result.RightTop := Vec2Mul(RightTop, V);
  Result.RightBottom := Vec2Mul(RightBottom, V);
  Result.LeftBottom := Vec2Mul(LeftBottom, V);
end;

function TV2Rect4.Mul(X, Y: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := Vec2Mul(LeftTop, X, Y);
  Result.RightTop := Vec2Mul(RightTop, X, Y);
  Result.RightBottom := Vec2Mul(RightBottom, X, Y);
  Result.LeftBottom := Vec2Mul(LeftBottom, X, Y);
end;

function TV2Rect4.Div_(V: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Div(LeftTop, V);
  Result.RightTop := Vec2Div(RightTop, V);
  Result.RightBottom := Vec2Div(RightBottom, V);
  Result.LeftBottom := Vec2Div(LeftBottom, V);
end;

function TV2Rect4.Div_(V: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := Vec2Div(LeftTop, V);
  Result.RightTop := Vec2Div(RightTop, V);
  Result.RightBottom := Vec2Div(RightBottom, V);
  Result.LeftBottom := Vec2Div(LeftBottom, V);
end;

function TV2Rect4.MoveTo(Position: TVec2): TV2Rect4;
begin
  Result := Init(Position, PointDistance(LeftTop, RightTop), PointDistance(LeftBottom, RightBottom), 0);
end;

function TV2Rect4.BoundRect: TRectV2;
begin
  Result := Z.Geometry2D.BoundRect(LeftTop, RightTop, RightBottom, LeftBottom);
end;

function TV2Rect4.BoundRectf: TRectf;
begin
  Result := MakeRectf(BoundRect);
end;

function TV2Rect4.Centroid: TVec2;
begin
  Result := BuffCentroid(LeftTop, RightTop, RightBottom, LeftBottom);
end;

function TV2Rect4.Mid_LeftTop_RightTop: TVec2;
begin
  Result := Vec2Middle(LeftTop, RightTop);
end;

function TV2Rect4.Mid_LeftTop_RightBottom: TVec2;
begin
  Result := Vec2Middle(LeftTop, RightBottom);
end;

function TV2Rect4.Mid_LeftTop_LeftBottom: TVec2;
begin
  Result := Vec2Middle(LeftTop, LeftBottom);
end;

function TV2Rect4.Mid_RightBottom_LeftBottom: TVec2;
begin
  Result := Vec2Middle(RightBottom, LeftBottom);
end;

function TV2Rect4.Mid_RightTop_RightBottom: TVec2;
begin
  Result := Vec2Middle(RightTop, RightBottom);
end;

function TV2Rect4.Transform(v2: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Add(LeftTop, v2);
  Result.RightTop := Vec2Add(RightTop, v2);
  Result.RightBottom := Vec2Add(RightBottom, v2);
  Result.LeftBottom := Vec2Add(LeftBottom, v2);
end;

function TV2Rect4.Transform(X, Y: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := Vec2Add(LeftTop, X, Y);
  Result.RightTop := Vec2Add(RightTop, X, Y);
  Result.RightBottom := Vec2Add(RightBottom, X, Y);
  Result.LeftBottom := Vec2Add(LeftBottom, X, Y);
end;

function TV2Rect4.Expands(Dist: TGeoFloat): TV2Rect4;
var
  vl: TV2L;
begin
  vl := TV2L.Create;
  vl.Add(LeftTop);
  vl.Add(RightTop);
  vl.Add(RightBottom);
  vl.Add(LeftBottom);
  Result.LeftTop := vl.Expands[0, Dist];
  Result.RightTop := vl.Expands[1, Dist];
  Result.RightBottom := vl.Expands[2, Dist];
  Result.LeftBottom := vl.Expands[3, Dist];
  DisposeObject(vl);
end;

function TV2Rect4.InHere(pt: TVec2): Boolean;
var
  buff: TArrayVec2;
begin
  buff := GetArrayVec2;
  Result := PointInPolygon(pt, buff);
  SetLength(buff, 0);
end;

function TV2Rect4.InHere(R: TRectV2): Boolean;
var
  buff: TArrayVec2;
begin
  buff := GetArrayVec2;
  Result := PointInPolygon(R[0], buff) and PointInPolygon(R[1], buff);
  SetLength(buff, 0);
end;

function TV2Rect4.GetArrayVec2: TArrayVec2;
begin
  SetLength(Result, 4);
  Result[0] := LeftTop;
  Result[1] := RightTop;
  Result[2] := RightBottom;
  Result[3] := LeftBottom;
end;

function TV2Rect4.GetNear(pt: TVec2): TVec2;
var
  tmpPt: TVec2;
  tmpDist, d: TGeoFloat;
begin
  tmpPt := ClosestPointOnSegmentFromPoint(LeftTop, RightTop, pt);
  tmpDist := Vec2Distance(tmpPt, pt);
  d := tmpDist;
  Result := tmpPt;

  tmpPt := ClosestPointOnSegmentFromPoint(RightTop, RightBottom, pt);
  tmpDist := Vec2Distance(tmpPt, pt);
  if tmpDist < d then
    begin
      d := tmpDist;
      Result := tmpPt;
    end;

  tmpPt := ClosestPointOnSegmentFromPoint(RightBottom, LeftBottom, pt);
  tmpDist := Vec2Distance(tmpPt, pt);
  if tmpDist < d then
    begin
      d := tmpDist;
      Result := tmpPt;
    end;

  tmpPt := ClosestPointOnSegmentFromPoint(LeftBottom, LeftTop, pt);
  tmpDist := Vec2Distance(tmpPt, pt);
  if tmpDist < d then
    begin
      d := tmpDist;
      Result := tmpPt;
    end;
end;

function TV2Rect4.GetNearLine(const V: TVec2; out lb, le: PVec2): TVec2;
var
  Arry_: array [0 .. 4] of PVec2;
  i: TGeoInt;
  pt1, pt2: PVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  Arry_[0] := @LeftTop;
  Arry_[1] := @RightTop;
  Arry_[2] := @RightBottom;
  Arry_[3] := @LeftBottom;
  Arry_[4] := @LeftTop;

  pt1 := Arry_[0];
  d := 0.0;
  for i := 1 to 4 do
    begin
      pt2 := Arry_[i];
      opt := ClosestPointOnSegmentFromPoint(pt1^, pt2^, V);
      d2 := PointDistance(V, opt);
      if (i = 1) or (d2 < d) then
        begin
          Result := opt;
          d := d2;
          lb := Arry_[i - 1];
          le := Arry_[i];
        end;
      pt1 := pt2;
    end;
end;

function TV2Rect4.GetNearLine(const V: TVec2): TVec2;
var
  Arry_: array [0 .. 4] of PVec2;
  i: TGeoInt;
  pt1, pt2: PVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  Arry_[0] := @LeftTop;
  Arry_[1] := @RightTop;
  Arry_[2] := @RightBottom;
  Arry_[3] := @LeftBottom;
  Arry_[4] := @LeftTop;

  pt1 := Arry_[0];
  d := 0.0;
  for i := 1 to 4 do
    begin
      pt2 := Arry_[i];
      opt := ClosestPointOnSegmentFromPoint(pt1^, pt2^, V);
      d2 := PointDistance(V, opt);
      if (i = 1) or (d2 < d) then
        begin
          Result := opt;
          d := d2;
        end;
      pt1 := pt2;
    end;
end;

function TV2Rect4.Projection(const sour, dest: TRectV2; const sourAxis, destAxis: TVec2; const sourAngle, destAngle: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, LeftTop);
  Result.RightTop := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, RightTop);
  Result.RightBottom := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, RightBottom);
  Result.LeftBottom := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, LeftBottom);
end;

function TV2Rect4.Projection(const sour, dest: TRectV2; sourAngle, destAngle: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := RectRotationProjection(sour, dest, sourAngle, destAngle, LeftTop);
  Result.RightTop := RectRotationProjection(sour, dest, sourAngle, destAngle, RightTop);
  Result.RightBottom := RectRotationProjection(sour, dest, sourAngle, destAngle, RightBottom);
  Result.LeftBottom := RectRotationProjection(sour, dest, sourAngle, destAngle, LeftBottom);
end;

function TV2Rect4.Projection(const sour, dest: TRectV2): TV2Rect4;
begin
  Result.LeftTop := RectProjection(sour, dest, LeftTop);
  Result.RightTop := RectProjection(sour, dest, RightTop);
  Result.RightBottom := RectProjection(sour, dest, RightBottom);
  Result.LeftBottom := RectProjection(sour, dest, LeftBottom);
end;

class function TV2Rect4.RebuildVertex(const buff: TArrayVec2): TV2Rect4;
begin
  if length(buff) <> 4 then
    begin
      Result := TV2Rect4.Init(Z.Geometry2D.BoundRect(buff));
    end
  else
    with Result do
      begin
        LeftTop := buff[0];
        RightTop := buff[1];
        RightBottom := buff[2];
        LeftBottom := buff[3];
      end;
end;

class function TV2Rect4.RebuildVertex(const buff: TV2L): TV2Rect4;
begin
  if buff.Count <> 4 then
    begin
      Result := TV2Rect4.Init(buff.BoundBox);
    end
  else
    with Result do
      begin
        LeftTop := buff[0]^;
        RightTop := buff[1]^;
        RightBottom := buff[2]^;
        LeftBottom := buff[3]^;
      end;
end;

class function TV2Rect4.Init(R: TRectV2): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := vec2(R[0, 0], R[0, 1]);
      RightTop := vec2(R[1, 0], R[0, 1]);
      RightBottom := vec2(R[1, 0], R[1, 1]);
      LeftBottom := vec2(R[0, 0], R[1, 1]);
    end;
end;

class function TV2Rect4.Init(R: TRectV2; axis: TVec2; Ang: TGeoFloat): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := vec2(R[0, 0], R[0, 1]);
      RightTop := vec2(R[1, 0], R[0, 1]);
      RightBottom := vec2(R[1, 0], R[1, 1]);
      LeftBottom := vec2(R[0, 0], R[1, 1]);
    end;
  if Ang <> 0 then
      Result := Result.Rotation(axis, Ang);
end;

class function TV2Rect4.Init(R: TRectV2; Ang: TGeoFloat): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := vec2(R[0, 0], R[0, 1]);
      RightTop := vec2(R[1, 0], R[0, 1]);
      RightBottom := vec2(R[1, 0], R[1, 1]);
      LeftBottom := vec2(R[0, 0], R[1, 1]);
    end;
  if Ang <> 0 then
      Result := Result.Rotation(Ang);
end;

class function TV2Rect4.Init(R: TRectf; Ang: TGeoFloat): TV2Rect4;
begin
  Result := Init(MakeRectV2(R), Ang);
end;

class function TV2Rect4.Init(R: TRect; Ang: TGeoFloat): TV2Rect4;
begin
  Result := Init(MakeRectV2(R), Ang);
end;

class function TV2Rect4.Init(R: TRect): TV2Rect4;
begin
  Result := Init(MakeRectV2(R), 0);
end;

class function TV2Rect4.Init(CenPos: TVec2; width, height, Ang: TGeoFloat): TV2Rect4;
var
  R: TRectV2;
begin
  R[0, 0] := CenPos[0] - width * 0.5;
  R[0, 1] := CenPos[1] - height * 0.5;
  R[1, 0] := CenPos[0] + width * 0.5;
  R[1, 1] := CenPos[1] + height * 0.5;
  Result := Init(R, Ang);
end;

class function TV2Rect4.Init(width, height, Ang: TGeoFloat): TV2Rect4;
begin
  Result := Init(MakeRectV2(0, 0, width, height), Ang);
end;

class function TV2Rect4.Init(width, height: TGeoFloat): TV2Rect4;
begin
  Result := Init(MakeRectV2(0, 0, width, height), 0);
end;

class function TV2Rect4.Init(): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := NULLPoint;
      RightTop := NULLPoint;
      RightBottom := NULLPoint;
      LeftBottom := NULLPoint;
    end;
end;

class function TV2Rect4.Create(R: TRectV2): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := vec2(R[0, 0], R[0, 1]);
      RightTop := vec2(R[1, 0], R[0, 1]);
      RightBottom := vec2(R[1, 0], R[1, 1]);
      LeftBottom := vec2(R[0, 0], R[1, 1]);
    end;
end;

class function TV2Rect4.Create(R: TRectV2; axis: TVec2; Ang: TGeoFloat): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := vec2(R[0, 0], R[0, 1]);
      RightTop := vec2(R[1, 0], R[0, 1]);
      RightBottom := vec2(R[1, 0], R[1, 1]);
      LeftBottom := vec2(R[0, 0], R[1, 1]);
    end;
  if Ang <> 0 then
      Result := Result.Rotation(axis, Ang);
end;

class function TV2Rect4.Create(R: TRectV2; Ang: TGeoFloat): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := vec2(R[0, 0], R[0, 1]);
      RightTop := vec2(R[1, 0], R[0, 1]);
      RightBottom := vec2(R[1, 0], R[1, 1]);
      LeftBottom := vec2(R[0, 0], R[1, 1]);
    end;
  if Ang <> 0 then
      Result := Result.Rotation(Ang);
end;

class function TV2Rect4.Create(R: TRectf; Ang: TGeoFloat): TV2Rect4;
begin
  Result := Create(MakeRectV2(R), Ang);
end;

class function TV2Rect4.Create(R: TRect; Ang: TGeoFloat): TV2Rect4;
begin
  Result := Create(MakeRectV2(R), Ang);
end;

class function TV2Rect4.Create(R: TRect): TV2Rect4;
begin
  Result := Create(MakeRectV2(R), 0);
end;

class function TV2Rect4.Create(CenPos: TVec2; width, height, Ang: TGeoFloat): TV2Rect4;
var
  R: TRectV2;
begin
  R[0, 0] := CenPos[0] - width * 0.5;
  R[0, 1] := CenPos[1] - height * 0.5;
  R[1, 0] := CenPos[0] + width * 0.5;
  R[1, 1] := CenPos[1] + height * 0.5;
  Result := Create(R, Ang);
end;

class function TV2Rect4.Create(width, height, Ang: TGeoFloat): TV2Rect4;
begin
  Result := Create(MakeRectV2(0, 0, width, height), Ang);
end;

class function TV2Rect4.Create(width, height: TGeoFloat): TV2Rect4;
begin
  Result := Create(MakeRectV2(0, 0, width, height), 0);
end;

class function TV2Rect4.Create(): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := NULLPoint;
      RightTop := NULLPoint;
      RightBottom := NULLPoint;
      LeftBottom := NULLPoint;
    end;
end;

constructor TV2Rect4List.Create;
begin
  inherited Create;
end;

destructor TV2Rect4List.Destroy;
begin
  Clear();
  inherited Destroy;
end;

procedure TV2Rect4List.AddRect(p: PV2Rect4);
begin
  inherited Add(p);
end;

procedure TV2Rect4List.AddRect(r4: TV2Rect4);
var
  p: PV2Rect4;
begin
  new(p);
  p^ := r4;
  AddRect(p);
end;

procedure TV2Rect4List.Remove(p: PV2Rect4);
begin
  Dispose(p);
  inherited Remove(p);
end;

procedure TV2Rect4List.Delete(index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      Dispose(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TV2Rect4List.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Dispose(Items[i]);
  inherited Clear;
end;

function TVec2_Buffer.Do_Sort_(var L, R: TVec2): Integer;
begin
  Result := CompareFloat(L[0] * L[1], R[0] * R[1]);
end;

procedure TVec2_Buffer.Sort;
begin
  Sort_M(Do_Sort_);
end;

function TVec2_Buffer.To_V2L: TV2L;
begin
  Result := TV2L.Create;
  if Num > 0 then
    with repeat_ do
      repeat
          Result.Add(Queue^.Data);
      until not Next;
end;

function TV2L.GetPoints(index: TGeoInt): PVec2;
begin
  Result := FList[index];
end;

constructor TV2L.Create;
begin
  inherited Create;
  FList := TCore_List.Create;
  FUserData := nil;
  FUserObject := nil;
end;

destructor TV2L.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TV2L.AddRandom;
begin
  Add(umlRRS(-10000, 10000), umlRRS(-10000, 10000));
end;

procedure TV2L.AddRandom(rnd: TMT19937Random);
begin
  Add(umlRRS(rnd, -10000, 10000), umlRRS(rnd, -10000, 10000));
end;

procedure TV2L.Add(const X, Y: TGeoFloat);
var
  p: PVec2;
begin
  new(p);
  p^ := PointMake(X, Y);
  FList.Add(p);
end;

procedure TV2L.Add(const pt: TVec2);
var
  p: PVec2;
begin
  new(p);
  p^ := pt;
  FList.Add(p);
end;

procedure TV2L.Add(pt: TPoint);
var
  p: PVec2;
begin
  new(p);
  p^ := vec2(pt);
  FList.Add(p);
end;

procedure TV2L.Add(pt: TPointf);
var
  p: PVec2;
begin
  new(p);
  p^ := vec2(pt);
  FList.Add(p);
end;

procedure TV2L.Add(v2l: TV2L);
var
  i: TGeoInt;
begin
  for i := 0 to v2l.Count - 1 do
      Add(v2l[i]^);
end;

procedure TV2L.Add(R: TRectV2);
begin
  Add(R[0, 0], R[0, 1]);
  Add(R[1, 0], R[0, 1]);
  Add(R[1, 0], R[1, 1]);
  Add(R[0, 0], R[1, 1]);
end;

procedure TV2L.Add(R: TRect);
begin
  Add(RectV2(R));
end;

procedure TV2L.Add(R: TRectf);
begin
  Add(RectV2(R));
end;

procedure TV2L.Add(R: TV2Rect4);
begin
  Add(R.LeftTop);
  Add(R.RightTop);
  Add(R.RightBottom);
  Add(R.LeftBottom);
end;

procedure TV2L.Add(arry: TArrayV2Rect4);
var
  i: Integer;
begin
  for i := low(arry) to high(arry) do
      Add(arry[i]);
end;

procedure TV2L.AddSubdivision(nbCount: TGeoInt; pt: TVec2);
var
  lpt: PVec2;
  i: TGeoInt;
  t: Double;
begin
  if Count > 0 then
    begin
      lpt := FList.Last;
      t := 1.0 / nbCount;
      for i := 1 to nbCount do
          Add(PointLerp(lpt^, pt, t * i));
    end
  else
      Add(pt);
end;

procedure TV2L.AddSubdivisionWithDistance(avgDist: TGeoFloat; pt: TVec2);
var
  lpt: PVec2;
  i, nbCount: TGeoInt;
  t: Double;
begin
  if (Count > 0) and (PointDistance(PVec2(FList.Last)^, pt) > avgDist) then
    begin
      lpt := FList.Last;
      nbCount := Trunc(PointDistance(PVec2(FList.Last)^, pt) / avgDist);
      t := 1.0 / nbCount;
      for i := 1 to nbCount do
          Add(PointLerp(lpt^, pt, t * i));
    end;
  Add(pt);
end;

procedure TV2L.AddCirclePoint(count_: Cardinal; axis: TVec2; dist_: TGeoFloat);
var
  i: TGeoInt;
begin
  for i := 0 to count_ - 1 do
      Add(PointRotation(axis, dist_, 360 / count_ * i));
end;

procedure TV2L.AddRectangle(R: TRectV2);
begin
  Add(R[0, 0], R[0, 1]);
  Add(R[1, 0], R[0, 1]);
  Add(R[1, 0], R[1, 1]);
  Add(R[0, 0], R[1, 1]);
end;

procedure TV2L.Insert(idx: TGeoInt; X, Y: TGeoFloat);
begin
  Insert(idx, vec2(X, Y));
end;

procedure TV2L.Insert(idx: TGeoInt; pt: TVec2);
var
  p: PVec2;
begin
  new(p);
  p^ := pt;
  FList.Insert(idx, p);
end;

procedure TV2L.Delete(idx: TGeoInt);
begin
  Dispose(PVec2(FList[idx]));
  FList.Delete(idx);
end;

function TV2L.Remove(p: PVec2): TGeoInt;
var
  i: TGeoInt;
begin
  Result := 0;
  i := 0;
  while i < FList.Count do
    begin
      if FList[i] = p then
        begin
          Dispose(PVec2(FList[i]));
          FList.Delete(i);
          inc(Result);
        end
      else
          inc(i);
    end;
end;

procedure TV2L.Clear;
var
  i: TGeoInt;
begin
  for i := 0 to FList.Count - 1 do
      Dispose(PVec2(FList[i]));
  FList.Clear;
end;

function TV2L.Count: TGeoInt;
begin
  Result := FList.Count;
end;

procedure TV2L.RemoveSame;
var
  L, p: PVec2;
  i: TGeoInt;
begin
  if Count < 2 then
      Exit;

  L := PVec2(FList[0]);
  p := PVec2(FList[Count - 1]);
  while (Count >= 2) and (IsEqual(p^, L^)) do
    begin
      Delete(Count - 1);
      p := PVec2(FList[Count - 1]);
    end;

  if Count < 2 then
      Exit;

  L := PVec2(FList[0]);
  i := 1;
  while i < Count do
    begin
      p := PVec2(FList[i]);
      if IsEqual(p^, L^) then
          Delete(i)
      else
        begin
          L := p;
          inc(i);
        end;
    end;
end;

procedure TV2L.SwapData(dest: TV2L);
var
  L: TCore_List;
begin
  L := FList;
  FList := dest.FList;
  dest.FList := L;
end;

procedure TV2L.MoveDataTo(dest: TV2L);
var
  i: TGeoInt;
begin
  for i := 0 to FList.Count - 1 do
      dest.FList.Add(FList[i]);
  FList.Clear;
end;

procedure TV2L.Assign(source: TCore_Object);
var
  i: TGeoInt;
begin
  if source is TV2L then
    begin
      Clear;
      for i := 0 to TV2L(source).Count - 1 do
          Add(TV2L(source)[i]^);
    end
  else if source is TDeflectionPolygon then
    begin
      Clear;
      for i := 0 to TDeflectionPolygon(source).Count - 1 do
          Add(TDeflectionPolygon(source).Points[i]);
    end;
end;

procedure TV2L.AssignFromArrayV2(arry: TArrayVec2);
var
  i: TGeoInt;
begin
  Clear;
  for i := low(arry) to high(arry) do
      Add(arry[i]);
end;

function TV2L.BuildArray: TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Points[i]^;
end;

function TV2L.BuildSplineSmoothInSideClosedArray: TArrayVec2;
var
  nl: TV2L;
begin
  nl := TV2L.Create;
  SplineSmoothInSideClosed(nl);
  Result := nl.BuildArray;
  DisposeObject(nl);
end;

function TV2L.BuildSplineSmoothOutSideClosedArray: TArrayVec2;
var
  nl: TV2L;
begin
  nl := TV2L.Create;
  SplineSmoothOutSideClosed(nl);
  Result := nl.BuildArray;
  DisposeObject(nl);
end;

function TV2L.BuildSplineSmoothOpenedArray: TArrayVec2;
var
  nl: TV2L;
begin
  nl := TV2L.Create;
  SplineSmoothOpened(nl);
  Result := nl.BuildArray;
  DisposeObject(nl);
end;

function TV2L.BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAxis, destAxis: TVec2; const sourAngle, destAngle: TGeoFloat): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, Points[i]^);
end;

function TV2L.BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAngle, destAngle: TGeoFloat): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := RectRotationProjection(sour, dest, sourAngle, destAngle, Points[i]^);
end;

function TV2L.BuildProjectionArray(const sour, dest: TRectV2): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := RectProjection(sour, dest, Points[i]^);
end;

function TV2L.BuildProjectionArray(const dest: TRectV2): TArrayVec2;
begin
  Result := BuildProjectionArray(BoundBox, dest);
end;

procedure TV2L.ProjectionTo(const sour, dest: TRectV2; const output: TDeflectionPolygon);
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      output.AddPoint(RectProjection(sour, dest, Points[i]^));
end;

procedure TV2L.ProjectionTo(const dest: TRectV2; const output: TDeflectionPolygon);
begin
  ProjectionTo(BoundBox, dest, output);
end;

procedure TV2L.ProjectionTo(const sour, dest: TRectV2; const output: TV2L);
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      output.Add(RectProjection(sour, dest, Points[i]^));
end;

procedure TV2L.ProjectionTo(const dest: TRectV2; const output: TV2L);
begin
  ProjectionTo(BoundBox, dest, output);
end;

procedure TV2L.SaveToStream(stream: TMS64);
var
  i: TGeoInt;
  p: PVec2;
begin
  stream.WriteInt32(Count);
  for i := 0 to Count - 1 do
    begin
      p := GetPoints(i);
      stream.WriteSingle(p^[0]);
      stream.WriteSingle(p^[1]);
    end;
end;

procedure TV2L.LoadFromStream(stream: TMS64);
var
  c: TGeoInt;
  i: TGeoInt;
  V: TVec2;
begin
  Clear;
  c := stream.ReadInt32;
  for i := 0 to c - 1 do
    begin
      V[0] := stream.ReadSingle;
      V[1] := stream.ReadSingle;
      Add(V);
    end;
end;

function TV2L.BoundBox: TRectV2;
var
  p: PVec2;
  MaxX: TGeoFloat;
  MaxY: TGeoFloat;
  MinX: TGeoFloat;
  MinY: TGeoFloat;
  i: TGeoInt;
begin
  Result := MakeRectV2(Zero, Zero, Zero, Zero);
  if Count < 2 then
      Exit;
  p := Points[0];
  MinX := p^[0];
  MaxX := p^[0];
  MinY := p^[1];
  MaxY := p^[1];

  for i := 1 to Count - 1 do
    begin
      p := Points[i];
      if p^[0] < MinX then
          MinX := p^[0]
      else if p^[0] > MaxX then
          MaxX := p^[0];
      if p^[1] < MinY then
          MinY := p^[1]
      else if p^[1] > MaxY then
          MaxY := p^[1];
    end;
  Result := MakeRectV2(MinX, MinY, MaxX, MaxY);
end;

function TV2L.BoundCentre: TVec2;
begin
  Result := RectCentre(BoundBox);
end;

function TV2L.CircleRadius(Centroid_: TVec2): TGeoFloat;
var
  i: TGeoInt;
  LayLen: TGeoFloat;
  LayDist: TGeoFloat;
begin
  Result := 0;
  if Count < 3 then
      Exit;
  LayLen := -1;
  for i := 0 to Count - 1 do
    begin
      LayDist := PointLayDistance(Centroid_, Points[i]^);
      if LayDist > LayLen then
          LayLen := LayDist;
    end;
  Result := Sqrt(LayLen);
end;

function TV2L.Centroid: TVec2;
var
  i: TGeoInt;
  asum: TGeoFloat;
  term: TGeoFloat;

  p1, p2: PVec2;
begin
  Result := NULLPoint;

  if Count = 1 then
      Exit(Points[0]^);

  if Count = 2 then
    begin
      p1 := Points[0];
      p2 := Points[1];
      Result := MiddleVec2(p1^, p2^);
      Exit;
    end;

  if Count < 3 then
      Exit;

  asum := Zero;
  p2 := Points[Count - 1];

  for i := 0 to Count - 1 do
    begin
      p1 := Points[i];

      term := ((p2^[0] * p1^[1]) - (p2^[1] * p1^[0]));
      asum := asum + term;
      Result[0] := Result[0] + (p2^[0] + p1^[0]) * term;
      Result[1] := Result[1] + (p2^[1] + p1^[1]) * term;
      p2 := p1;
    end;

  if NotEqual(asum, Zero) then
    begin
      Result[0] := Result[0] / (3.0 * asum);
      Result[1] := Result[1] / (3.0 * asum);
    end;
end;

function TV2L.Area: TGeoFloat;
var
  i, j: TGeoInt;
  v1, v2: TVec2;
begin
  Result := 0;
  if Count < 3 then
      Exit;
  j := Count - 1;
  v2 := GetPoints(j)^;
  for i := 0 to Count - 1 do
    begin
      v1 := GetPoints(i)^;
      Result := Result + ((v2[0] * v1[1]) - (v2[1] * v1[0]));
      v2 := v1;
      j := i;
    end;
  Result := Result * 0.5;
end;

function TV2L.InHere(pt: TVec2): Boolean;
var
  i: TGeoInt;
  pi, pj: PVec2;
begin
  Result := False;
  if Count < 3 then
      Exit;
  pj := Points[Count - 1];
  for i := 0 to Count - 1 do
    begin
      pi := Points[i];
      if ((pi^[1] <= pt[1]) and (pt[1] < pj^[1])) or // an upward crossing
        ((pj^[1] <= pt[1]) and (pt[1] < pi^[1])) then // a downward crossing
        begin
          (* compute the edge-ray intersect @ the x-coordinate *)
          if (pt[0] - pi^[0] < ((pj^[0] - pi^[0]) * (pt[1] - pi^[1]) / (pj^[1] - pi^[1]))) then
              Result := not Result;
        end;
      pj := pi;
    end;
end;

function TV2L.InRect(R: TRectV2): Boolean;
var
  i: TGeoInt;
begin
  Result := False;
  for i := 0 to Count - 1 do
      Result := Result or PointInRect(Points[i]^, R);
end;

function TV2L.Rect2Intersect(R: TRectV2): Boolean;
var
  i: TGeoInt;
  r4: TV2Rect4;
begin
  Result := False;
  for i := 0 to Count - 1 do
      Result := Result or PointInRect(Points[i]^, R);

  if not Result then
    begin
      r4 := TV2Rect4.Init(R);
      Result := Result or Line2Intersect(r4.LeftTop, r4.RightTop, True);
      Result := Result or Line2Intersect(r4.RightTop, r4.RightBottom, True);
      Result := Result or Line2Intersect(r4.RightBottom, r4.LeftBottom, True);
      Result := Result or Line2Intersect(r4.LeftBottom, r4.LeftTop, True);
    end;
end;

procedure TV2L.RotateAngle(axis: TVec2; Angle: TGeoFloat);
var
  i: TGeoInt;
  p: PVec2;
begin
  for i := 0 to Count - 1 do
    begin
      p := Points[i];
      p^ := PointRotation(axis, p^, PointAngle(axis, p^) + Angle);
    end;
end;

procedure TV2L.Scale(Scale_: TGeoFloat);
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      PointScale(Points[i]^, Scale_);
end;

procedure TV2L.ConvexHull(output: TV2L);

const
  RightHandSide = -1;
  LeftHandSide = +1;
  CounterClockwise = +1;
  CollinearOrientation = 0;

type
  T2DHullPoint = record
    X: TGeoFloat;
    Y: TGeoFloat;
    Ang: TGeoFloat;
  end;

  TCompareResult = (eGreaterThan, eLessThan, eEqual);

var
  Point: array of T2DHullPoint;
  Stack: array of T2DHullPoint;
  StackHeadPosition: TGeoInt;
  Anchor: T2DHullPoint;

  function CartesianAngle(const X, Y: TGeoFloat): TGeoFloat;
  const
    _180DivPI = 57.295779513082320876798154814105000;
  begin
    if (X > Zero) and (Y > Zero) then
        Result := (ArcTan(Y / X) * _180DivPI)
    else if (X < Zero) and (Y > Zero) then
        Result := (ArcTan(-X / Y) * _180DivPI) + 90.0
    else if (X < Zero) and (Y < Zero) then
        Result := (ArcTan(Y / X) * _180DivPI) + 180.0
    else if (X > Zero) and (Y < Zero) then
        Result := (ArcTan(-X / Y) * _180DivPI) + 270.0
    else if (X = Zero) and (Y > Zero) then
        Result := 90.0
    else if (X < Zero) and (Y = Zero) then
        Result := 180.0
    else if (X = Zero) and (Y < Zero) then
        Result := 270.0
    else
        Result := Zero;
  end;

  procedure Swap(i, j: TGeoInt; var Point: array of T2DHullPoint);
  var
    Temp: T2DHullPoint;
  begin
    Temp := Point[i];
    Point[i] := Point[j];
    Point[j] := Temp;
  end;

  function hEqual(const p1, p2: T2DHullPoint): Boolean;
  begin
    Result := IsEqual(p1.X, p2.X) and IsEqual(p1.Y, p2.Y);
  end;

  function CompareAngles(const p1, p2: T2DHullPoint): TCompareResult;
  begin
    if p1.Ang < p2.Ang then
        Result := eLessThan
    else if p1.Ang > p2.Ang then
        Result := eGreaterThan
    else if hEqual(p1, p2) then
        Result := eEqual
    else if Distance(Anchor.X, Anchor.Y, p1.X, p1.Y) < Distance(Anchor.X, Anchor.Y, p2.X, p2.Y) then
        Result := eLessThan
    else
        Result := eGreaterThan;
  end;

  procedure RQSort(Left, Right: TGeoInt; var Point: array of T2DHullPoint);
  var
    i: TGeoInt;
    j: TGeoInt;
    Middle: TGeoInt;
    Pivot: T2DHullPoint;
  begin
    repeat
      i := Left;
      j := Right;
      Middle := (Left + Right) div 2;
      (* Median of 3 Pivot Selection *)
      if CompareAngles(Point[Middle], Point[Left]) = eLessThan then
          Swap(Left, Middle, Point);
      if CompareAngles(Point[Right], Point[Middle]) = eLessThan then
          Swap(Right, Middle, Point);
      if CompareAngles(Point[Middle], Point[Left]) = eLessThan then
          Swap(Left, Middle, Point);
      Pivot := Point[Right];
      repeat
        while CompareAngles(Point[i], Pivot) = eLessThan do
            inc(i);
        while CompareAngles(Point[j], Pivot) = eGreaterThan do
            dec(j);
        if i <= j then
          begin
            Swap(i, j, Point);
            inc(i);
            dec(j);
          end;
      until i > j;
      if Left < j then
          RQSort(Left, j, Point);
      Left := i;
    until i >= Right;
  end;

  procedure Push(Pnt: T2DHullPoint);
  begin
    inc(StackHeadPosition);
    Stack[StackHeadPosition] := Pnt;
  end;

  function Pop: Boolean;
  begin
    Result := False;
    if StackHeadPosition >= 0 then
      begin
        Result := True;
        dec(StackHeadPosition);
      end;
  end;

  function Head: T2DHullPoint;
  begin
    Assert((StackHeadPosition >= 0) and (StackHeadPosition < length(Stack)), 'Invalid stack-head position.');
    Result := Stack[StackHeadPosition];
  end;

  function PreHead: T2DHullPoint;
  begin
    Assert(((StackHeadPosition - 1) >= 0) and ((StackHeadPosition - 1) < length(Stack)), 'Invalid pre stack-head position.');
    Result := Stack[StackHeadPosition - 1];
  end;

  function PreHeadExist: Boolean;
  begin
    Result := (StackHeadPosition > 0);
  end;

  function Orientation(p1, p2, p3: T2DHullPoint): TGeoInt;
    function Orientation2(const x1, y1, x2, y2, Px, Py: TGeoFloat): TGeoInt;
    var
      Orin: TGeoFloat;
    begin
      (* Determinant of the 3 points *)
      Orin := (x2 - x1) * (Py - y1) - (Px - x1) * (y2 - y1);
      if Orin > Zero then
          Result := LeftHandSide (* Orientaion is to the left-hand side *)
      else if Orin < Zero then
          Result := RightHandSide (* Orientaion is to the right-hand side *)
      else
          Result := CollinearOrientation; (* Orientaion is neutral aka collinear *)
    end;

  begin
    Result := Orientation2(p1.X, p1.Y, p2.X, p2.Y, p3.X, p3.Y);
  end;

  procedure GrahamScan;
  var
    i: TGeoInt;
    Orin: TGeoInt;
  begin
    Push(Point[0]);
    Push(Point[1]);
    i := 2;
    while i < length(Point) do
      begin
        if PreHeadExist then
          begin
            Orin := Orientation(PreHead, Head, Point[i]);
            if Orin = CounterClockwise then
              begin
                Push(Point[i]);
                inc(i);
              end
            else
                Pop;
          end
        else
          begin
            Push(Point[i]);
            inc(i);
          end;
      end;
  end;

var
  i: TGeoInt;
  j: TGeoInt;
  p: PVec2;
begin
  if Count <= 3 then
    begin
      for i := 0 to Count - 1 do
          output.Add(Points[i]^);
      Exit;
    end;
  StackHeadPosition := -1;

  try
    SetLength(Point, Count);
    SetLength(Stack, Count);
    j := 0;
    for i := 0 to Count - 1 do
      begin
        p := Points[i];
        Point[i].X := p^[0];
        Point[i].Y := p^[1];
        Point[i].Ang := 0.0;
        if Point[i].Y < Point[j].Y then
            j := i
        else if Point[i].Y = Point[j].Y then
          if Point[i].X < Point[j].X then
              j := i;
      end;

    Swap(0, j, Point);
    Point[0].Ang := 0;
    Anchor := Point[0];
    (* Calculate angle of the vertex ([ith point]-[anchorpoint]-[most left point]) *)
    for i := 1 to length(Point) - 1 do
        Point[i].Ang := CartesianAngle(Point[i].X - Anchor.X, Point[i].Y - Anchor.Y);
    (* Sort points in ascending order according to their angles *)
    RQSort(1, length(Point) - 1, Point);
    GrahamScan;
    (* output list *)
    for i := 0 to StackHeadPosition do
        output.Add(Stack[i].X, Stack[i].Y);
  finally
    (* Final clean-up *)
    Finalize(Stack);
    Finalize(Point);
  end;
end;

procedure TV2L.ConvexHull;
var
  nl: TV2L;
begin
  nl := TV2L.Create;
  ConvexHull(nl);
  SwapData(nl);
  DisposeObject(nl);
end;

procedure TV2L.SplineSmoothInSideClosed(output: TV2L);
var
  i, j, idx, pre: TGeoInt;
  ptPrev, ptPrev2, ptNext, ptNext2, V: TVec2;
  t: TGeoFloat;
begin
  if Count < 3 then
    begin
      output.Assign(Self);
      Exit;
    end;

  idx := 0;
  for i := 0 to Count - 1 do
    begin
      ptPrev2 := Points[(i + Count - 1) mod Count]^;
      ptPrev := Points[i]^;
      ptNext := Points[(i + 1) mod Count]^;
      ptNext2 := Points[(i + 2) mod Count]^;

      pre := ComputeCurvePartPrecision(ptPrev2, ptPrev, ptNext, ptNext2);

      if i = 0 then
          j := 0
      else
          j := 1;

      while j <= pre do
        begin
          t := j / pre;
          V := Vec2Mul(ptPrev2, Interpolation_InSide(t + 1));
          V := Vec2Add(V, Vec2Mul(ptPrev, Interpolation_InSide(t)));
          V := Vec2Add(V, Vec2Mul(ptNext, Interpolation_InSide(t - 1)));
          V := Vec2Add(V, Vec2Mul(ptNext2, Interpolation_InSide(t - 2)));
          if not IsNan(V) then
              output.Add(V);
          inc(idx);
          inc(j);
        end;
    end;
  RemoveSame;
end;

procedure TV2L.SplineSmoothInSideClosed;
var
  vl: TV2L;
begin
  vl := TV2L.Create;
  SwapData(vl);
  vl.SplineSmoothInSideClosed(Self);
  DisposeObject(vl);
end;

procedure TV2L.SplineSmoothOutSideClosed(output: TV2L);
var
  i, j, idx, pre: TGeoInt;
  ptPrev, ptPrev2, ptNext, ptNext2, V: TVec2;
  t: TGeoFloat;
begin
  if Count < 3 then
    begin
      output.Assign(Self);
      Exit;
    end;

  idx := 0;
  for i := 0 to Count - 1 do
    begin
      ptPrev2 := Points[(i + Count - 1) mod Count]^;
      ptPrev := Points[i]^;
      ptNext := Points[(i + 1) mod Count]^;
      ptNext2 := Points[(i + 2) mod Count]^;

      pre := ComputeCurvePartPrecision(ptPrev2, ptPrev, ptNext, ptNext2);

      if i = 0 then
          j := 0
      else
          j := 1;

      while j <= pre do
        begin
          t := j / pre;
          V := Vec2Mul(ptPrev2, Interpolation_OutSide(t + 1));
          V := Vec2Add(V, Vec2Mul(ptPrev, Interpolation_OutSide(t)));
          V := Vec2Add(V, Vec2Mul(ptNext, Interpolation_OutSide(t - 1)));
          V := Vec2Add(V, Vec2Mul(ptNext2, Interpolation_OutSide(t - 2)));
          if not IsNan(V) then
              output.Add(V);
          inc(idx);
          inc(j);
        end;
    end;
  RemoveSame;
end;

procedure TV2L.SplineSmoothOutSideClosed;
var
  vl: TV2L;
begin
  vl := TV2L.Create;
  SwapData(vl);
  vl.SplineSmoothOutSideClosed(Self);
  DisposeObject(vl);
end;

procedure TV2L.SplineSmoothOpened(output: TV2L);
const
  EndCoeff = 0;
var
  i, j, idx, pre: TGeoInt;
  ptPrev, ptPrev2, ptNext, ptNext2, V: TVec2;
  t: TGeoFloat;
begin
  if Count < 3 then
    begin
      output.Assign(Self);
      Exit;
    end;

  idx := 0;

  for i := 0 to Count - 2 do
    begin
      ptPrev := Points[i]^;
      ptNext := Points[i + 1]^;

      if i = 0 then
          ptPrev2 := Vec2Add(ptPrev, Vec2Mul(Vec2Mul(Vec2Add(ptNext, Points[i + 2]^), EndCoeff), (1 / (1 + 2 * EndCoeff))))
      else
          ptPrev2 := Points[i - 1]^;

      if i = Count - 2 then
          ptNext2 := Vec2Add(ptNext, Vec2Mul(Vec2Mul(Vec2Add(ptPrev, Points[i - 1]^), EndCoeff), (1 / (1 + 2 * EndCoeff))))
      else
          ptNext2 := Points[i + 2]^;

      pre := ComputeCurvePartPrecision(ptPrev2, ptPrev, ptNext, ptNext2);

      if i = 0 then
          j := 0
      else
          j := 1;

      while j <= pre do
        begin
          t := j / pre;
          V := Vec2Mul(ptPrev2, Interpolation_OutSide(t + 1));
          V := Vec2Add(V, Vec2Mul(ptPrev, Interpolation_OutSide(t)));
          V := Vec2Add(V, Vec2Mul(ptNext, Interpolation_OutSide(t - 1)));
          V := Vec2Add(V, Vec2Mul(ptNext2, Interpolation_OutSide(t - 2)));
          if not IsNan(V) then
              output.Add(V);
          inc(idx);
          inc(j);
        end;
    end;
  RemoveSame;
end;

procedure TV2L.SplineSmoothOpened;
var
  vl: TV2L;
begin
  vl := TV2L.Create;
  SwapData(vl);
  vl.SplineSmoothOpened(Self);
  DisposeObject(vl);
end;

procedure TV2L.ExtractToBuff(var output: TArrayVec2);
var
  i: TGeoInt;
begin
  SetLength(output, Count);
  for i := 0 to Count - 1 do
      output[i] := Points[i]^;
end;

procedure TV2L.GiveListDataFromBuff(output: TArrayVec2);
var
  i: TGeoInt;
begin
  Clear;
  for i := low(output) to high(output) do
      Add(output[i]);
end;

function TV2L.SumDistance: TGeoFloat;
var
  i: TGeoInt;
  p1, p2: PVec2;
begin
  Result := 0;
  if Count <= 0 then
      Exit;
  p1 := First;
  for i := 1 to Count - 1 do
    begin
      p2 := Points[i];
      Result := Result + Vec2Distance(p1^, p2^);
      p1 := p2;
    end;
end;

procedure TV2L.InterpolationTo(count_: TGeoInt; output_: TV2L);
var
  sum_: TGeoFloat;
  avgDist: TGeoFloat;
  i: TGeoInt;
  t: TVec2;
  d, tmp: TGeoFloat;
begin
  sum_ := SumDistance();
  avgDist := sum_ / count_;
  output_.Clear;

  i := 0;
  t := First^;
  d := avgDist;
  output_.Add(t);

  while i < Count do
    begin
      tmp := Vec2Distance(t, Points[i]^);
      if tmp < d then
        begin
          d := d - tmp;
          t := Points[i]^;
          inc(i);
        end
      else
        begin
          t := Vec2LerpTo(t, Points[i]^, d);
          output_.Add(t);
          d := avgDist;
        end;
    end;
  if output_.Count < count_ then
      output_.Add(Last^);

  if Vec2Distance(output_.First^, vec2(0, 0)) > Vec2Distance(output_.Last^, vec2(0, 0)) then
      output_.Reverse;
end;

procedure TV2L.VertexReduction(Epsilon_: TGeoFloat);
var
  buff, output: TArrayVec2;
  f, L: TVec2;
begin
  RemoveSame;
  f := First^;
  L := Last^;
  ExtractToBuff(buff);
  FastVertexReduction(buff, Epsilon_, output);
  GiveListDataFromBuff(output);
  Insert(0, f);
  Add(L);
  RemoveSame;
end;

procedure TV2L.Reduction(Epsilon_: TGeoFloat);
begin
  VertexReduction(Epsilon_);
end;

function TV2L.Line2Intersect(const lb, le: TVec2; ClosedPolyMode: Boolean): Boolean;
var
  i: TGeoInt;
  p1, p2: PVec2;
begin
  Result := False;
  if FList.Count > 1 then
    begin
      p1 := FList[0];
      for i := 1 to FList.Count - 1 do
        begin
          p2 := FList[i];
          if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1]) then
            begin
              Result := True;
              Exit;
            end;
          p1 := p2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          p2 := FList[0];
          if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1]) then
              Result := True;
        end;
    end;
end;

function TV2L.Line2Intersect(const lb, le: TVec2; ClosedPolyMode: Boolean; output: TV2L): Boolean;
var
  i: TGeoInt;
  p1, p2: PVec2;
  ox, oy: TGeoFloat;
begin
  Result := False;
  if FList.Count > 1 then
    begin
      p1 := FList[0];
      for i := 1 to FList.Count - 1 do
        begin
          p2 := FList[i];
          if output <> nil then
            begin
              if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1], ox, oy) then
                begin
                  output.Add(ox, oy);
                  Result := True;
                end;
            end
          else
            begin
              if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1]) then
                begin
                  Result := True;
                  Exit;
                end;
            end;
          p1 := p2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          p2 := FList[0];
          if output <> nil then
            begin
              if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1], ox, oy) then
                begin
                  output.Add(ox, oy);
                  Result := True;
                end;
            end
          else
            begin
              if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1]) then
                  Result := True;
            end;
        end;
    end;
end;

function TV2L.Line2NearIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean; out idx1, idx2: TGeoInt; out IntersectPt: TVec2): Boolean;
var
  i: TGeoInt;
  p1, p2: PVec2;
  ox, oy: TGeoFloat;
  d, d2: TGeoFloat;
begin
  Result := False;
  if FList.Count > 1 then
    begin
      p1 := FList[0];
      d := 0.0;
      for i := 1 to FList.Count - 1 do
        begin
          p2 := FList[i];
          if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1], ox, oy) then
            begin
              d2 := PointDistance(lb, PointMake(ox, oy));
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := PointMake(ox, oy);
                  d := d2;
                  idx1 := i - 1;
                  idx2 := i;
                  Result := True;
                end;
            end;
          p1 := p2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          p2 := FList[0];
          if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1], ox, oy) then
            begin
              d2 := PointDistance(lb, PointMake(ox, oy));
              if (d = 0) or (d2 < d) then
                begin
                  IntersectPt := PointMake(ox, oy);
                  // d := d2;
                  idx1 := FList.Count - 1;
                  idx2 := 0;
                  Result := True;
                end;
            end;
        end;
    end;
end;

procedure TV2L.SortOfNear(const lb, le: TVec2);

  function Compare_(Left, Right: pointer): ShortInt;
  var
    d1, d2: TGeoFloat;
  begin
    d1 := MinimumDistanceFromPointToLine(lb, le, PVec2(Left)^);
    d2 := MinimumDistanceFromPointToLine(lb, le, PVec2(Right)^);
    Result := CompareFloat(d1, d2);
  end;

  procedure fastSort_(var Arry_: TCore_PointerList; L, R: TGeoInt);
  var
    i, j: TGeoInt;
    p, tmp: pointer;
  begin
    if L < R then
      begin
        repeat
          if (R - L) = 1 then
            begin
              if Compare_(Arry_[L], Arry_[R]) > 0 then
                begin
                  tmp := Arry_[L];
                  Arry_[L] := Arry_[R];
                  Arry_[R] := tmp;
                end;
              break;
            end;
          i := L;
          j := R;
          p := Arry_[(L + R) shr 1];
          repeat
            while Compare_(Arry_[i], p) < 0 do
                inc(i);
            while Compare_(Arry_[j], p) > 0 do
                dec(j);
            if i <= j then
              begin
                if i <> j then
                  begin
                    tmp := Arry_[i];
                    Arry_[i] := Arry_[j];
                    Arry_[j] := tmp;
                  end;
                inc(i);
                dec(j);
              end;
          until i > j;
          if (j - L) > (R - i) then
            begin
              if i < R then
                  fastSort_(Arry_, i, R);
              R := j;
            end
          else
            begin
              if L < j then
                  fastSort_(Arry_, L, j);
              L := i;
            end;
        until L >= R;
      end;
  end;

begin
  if Count > 1 then
      fastSort_(FList.ListData^, 0, Count - 1);
end;

procedure TV2L.SortOfNear(const pt: TVec2);

  function Compare_(Left, Right: pointer): ShortInt;
  var
    d1, d2: TGeoFloat;
  begin
    d1 := PointDistance(PVec2(Left)^, pt);
    d2 := PointDistance(PVec2(Right)^, pt);
    Result := CompareFloat(d1, d2);
  end;

  procedure fastSort_(var Arry_: TCore_PointerList; L, R: TGeoInt);
  var
    i, j: TGeoInt;
    p, tmp: pointer;
  begin
    if L < R then
      begin
        repeat
          if (R - L) = 1 then
            begin
              if Compare_(Arry_[L], Arry_[R]) > 0 then
                begin
                  tmp := Arry_[L];
                  Arry_[L] := Arry_[R];
                  Arry_[R] := tmp;
                end;
              break;
            end;
          i := L;
          j := R;
          p := Arry_[(L + R) shr 1];
          repeat
            while Compare_(Arry_[i], p) < 0 do
                inc(i);
            while Compare_(Arry_[j], p) > 0 do
                dec(j);
            if i <= j then
              begin
                if i <> j then
                  begin
                    tmp := Arry_[i];
                    Arry_[i] := Arry_[j];
                    Arry_[j] := tmp;
                  end;
                inc(i);
                dec(j);
              end;
          until i > j;
          if (j - L) > (R - i) then
            begin
              if i < R then
                  fastSort_(Arry_, i, R);
              R := j;
            end
          else
            begin
              if L < j then
                  fastSort_(Arry_, L, j);
              L := i;
            end;
        until L >= R;
      end;
  end;

begin
  if Count > 1 then
      fastSort_(FList.ListData^, 0, Count - 1);
end;

procedure TV2L.Reverse;
var
  NewList: TCore_List;
  i, c: TGeoInt;
begin
  NewList := TCore_List.Create;
  c := Count - 1;
  NewList.Count := c + 1;
  for i := c downto 0 do
      NewList[c - i] := FList[i];
  DisposeObject(FList);
  FList := NewList;
end;

function TV2L.GetNearLine(const pt: TVec2; const ClosedMode: Boolean; out lb, le: TGeoInt): TVec2;
var
  i: TGeoInt;
  pt1, pt2: PVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];

          opt := ClosestPointOnSegmentFromPoint(pt1^, pt2^, pt);

          d2 := PointDistance(pt, opt);
          if (i = 1) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
              lb := i - 1;
              le := i;
            end;

          pt1 := pt2;
        end;
      if ClosedMode and (Count >= 3) then
        begin
          pt2 := Points[0];
          opt := ClosestPointOnSegmentFromPoint(pt1^, pt2^, pt);
          d2 := PointDistance(pt, opt);
          if (d2 < d) then
            begin
              Result := opt;
              lb := FList.Count - 1;
              le := 0;
            end;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Points[0]^;
          lb := 0;
          le := 0;
        end
      else
        begin
          Result := NULLPoint;
          lb := -1;
          le := -1;
        end;
    end;
end;

function TV2L.GetNearLine(const pt: TVec2; const ClosedMode: Boolean): TVec2;
var
  i: TGeoInt;
  pt1, pt2: PVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];

          opt := ClosestPointOnSegmentFromPoint(pt1^, pt2^, pt);

          d2 := PointDistance(pt, opt);
          if (i = 1) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
            end;

          pt1 := pt2;
        end;
      if ClosedMode and (Count >= 3) then
        begin
          pt2 := Points[0];
          opt := ClosestPointOnSegmentFromPoint(pt1^, pt2^, pt);
          d2 := PointDistance(pt, opt);
          if (d2 < d) then
            begin
              Result := opt;
            end;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Points[0]^;
        end
      else
        begin
          Result := NULLPoint;
        end;
    end;
end;

function TV2L.GetNearLine(const pt: TVec2; const ExpandDist: TGeoFloat): TVec2;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Expands[0, ExpandDist];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Expands[i, ExpandDist];

          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);

          d2 := PointDistance(pt, opt);
          if (i = 1) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
            end;

          pt1 := pt2;
        end;

      if (Count >= 3) then
        begin
          pt2 := Expands[0, ExpandDist];
          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);
          d2 := PointDistance(pt, opt);
          if (d2 < d) then
            begin
              Result := opt;
            end;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Points[0]^;
        end
      else
        begin
          Result := NULLPoint;
        end;
    end;
end;

procedure TV2L.CutLineBeginPtToIdx(const pt: TVec2; const toidx: TGeoInt);
var
  i: TGeoInt;
begin
  for i := 0 to toidx - 2 do
      Delete(0);
  Points[0]^ := pt;
end;

procedure TV2L.Transform(X, Y: TGeoFloat);
var
  i: TGeoInt;
  p: PVec2;
begin
  for i := 0 to Count - 1 do
    begin
      p := Points[i];
      p^[0] := p^[0] + X;
      p^[1] := p^[1] + Y;
    end;
end;

procedure TV2L.Transform(V: TVec2);
begin
  Transform(V[0], V[1]);
end;

procedure TV2L.Mul(X, Y: TGeoFloat);
var
  i: TGeoInt;
  p: PVec2;
begin
  for i := 0 to Count - 1 do
    begin
      p := Points[i];
      p^[0] := p^[0] * X;
      p^[1] := p^[1] * Y;
    end;
end;

procedure TV2L.Mul(V: TVec2);
begin
  Mul(V[0], V[1]);
end;

procedure TV2L.Mul(V: TGeoFloat);
begin
  Mul(V, V);
end;

procedure TV2L.FDiv(X, Y: TGeoFloat);
var
  i: TGeoInt;
  p: PVec2;
begin
  for i := 0 to Count - 1 do
    begin
      p := Points[i];
      p^[0] := p^[0] / X;
      p^[1] := p^[1] / Y;
    end;
end;

procedure TV2L.FDiv(V: TVec2);
begin
  FDiv(V[0], V[1]);
end;

procedure TV2L.FDiv(V: TGeoFloat);
begin
  FDiv(V, V);
end;

function TV2L.First: PVec2;
begin
  if Count > 0 then
      Result := Points[0]
  else
      Result := nil;
end;

function TV2L.Last: PVec2;
begin
  if Count > 0 then
      Result := Points[Count - 1]
  else
      Result := nil;
end;

procedure TV2L.ExpandDistanceAsList(ExpandDist: TGeoFloat; output: TV2L);
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      output.Add(GetExpands(i, ExpandDist));
end;

procedure TV2L.ExpandDistance(ExpandDist: TGeoFloat);
var
  vl: TV2L;
begin
  vl := TV2L.Create;
  SwapData(vl);
  vl.ExpandDistanceAsList(ExpandDist, Self);
  DisposeObject(vl);
end;

procedure TV2L.ExpandConvexHullAsList(ExpandDist: TGeoFloat; output: TV2L);
var
  pl: TV2L;
begin
  pl := TV2L.Create;
  ConvexHull(pl);
  pl.ExpandDistanceAsList(ExpandDist, output);
  DisposeObject(pl);
end;

function TV2L.GetExpands(idx: TGeoInt; ExpandDist: TGeoFloat): TVec2;
var
  lpt, pt, rpt: TVec2;
  ln, rn: TVec2;
  Dx, Dy, f, R: TGeoFloat;
  Cx, Cy: TGeoFloat;
begin
  if (ExpandDist = 0) or (Count < 2) then
    begin
      Result := Points[idx]^;
      Exit;
    end;

  if idx > 0 then
      lpt := Points[idx - 1]^
  else
      lpt := Points[Count - 1]^;
  if idx + 1 < Count then
      rpt := Points[idx + 1]^
  else
      rpt := Points[0]^;
  pt := Points[idx]^;

  // normal : left to
  Dx := (pt[0] - lpt[0]);
  Dy := (pt[1] - lpt[1]);
  f := 1.0 / HypotX(Dx, Dy);
  ln[0] := (Dy * f);
  ln[1] := -(Dx * f);

  // normal : right to
  Dx := (rpt[0] - pt[0]);
  Dy := (rpt[1] - pt[1]);
  f := 1.0 / HypotX(Dx, Dy);
  rn[0] := (Dy * f);
  rn[1] := -(Dx * f);

  // compute the expand edge
  Dx := (ln[0] + rn[0]);
  Dy := (ln[1] + rn[1]);
  R := (ln[0] * Dx) + (ln[1] * Dy);
  if R = 0 then
      R := 1;
  Cx := (Dx * ExpandDist / R);
  Cy := (Dy * ExpandDist / R);

  Result[0] := pt[0] + Cx;
  Result[1] := pt[1] + Cy;
end;

constructor TLinesList.Create;
begin
  inherited Create;
  AutoFree := False;
end;

destructor TLinesList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLinesList.Remove(obj: TLines);
begin
  if AutoFree then
      DisposeObject(obj);
  inherited Remove(obj);
end;

procedure TLinesList.Delete(index: TGeoInt);
begin
  if (index >= 0) and (index < Count) then
    begin
      if AutoFree then
          DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TLinesList.Clear;
var
  i: TGeoInt;
begin
  if AutoFree then
    for i := 0 to Count - 1 do
        DisposeObject(Items[i]);
  inherited Clear;
end;

constructor T2DPolygon.Create;
begin
  inherited Create;
  Owner := nil;
end;

destructor T2DPolygon.Destroy;
begin
  inherited Destroy;
end;

constructor T2DPolygonGraph.Create;
begin
  inherited Create;
  Surround := T2DPolygon.Create;
  Surround.Owner := Self;
  SetLength(Collapses, 0);
end;

destructor T2DPolygonGraph.Destroy;
begin
  Clear();
  DisposeObject(Surround);
  inherited Destroy;
end;

procedure T2DPolygonGraph.Assign(source: TCore_Object);
var
  i: TGeoInt;
begin
  Clear;
  if source is T2DPolygonGraph then
    begin
      Surround.Assign(T2DPolygonGraph(source).Surround);
      SetLength(Collapses, T2DPolygonGraph(source).CollapsesCount);
      for i := 0 to T2DPolygonGraph(source).CollapsesCount - 1 do
        begin
          Collapses[i] := T2DPolygon.Create;
          Collapses[i].Owner := Self;
          Collapses[i].Assign(T2DPolygonGraph(source).Collapses[i]);
        end;
    end
  else
      Surround.Assign(source);
end;

function T2DPolygonGraph.NewCollapse: T2DPolygon;
begin
  SetLength(Collapses, CollapsesCount + 1);
  Result := T2DPolygon.Create;
  Result.Owner := Self;
  Collapses[CollapsesCount - 1] := Result;
end;

procedure T2DPolygonGraph.AddCollapse(polygon: T2DPolygon);
begin
  polygon.Owner := Self;
  SetLength(Collapses, CollapsesCount + 1);
  Collapses[CollapsesCount - 1] := polygon;
  polygon.RemoveSame;
end;

procedure T2DPolygonGraph.Clear;
var
  i: TGeoInt;
begin
  Surround.Clear;
  for i := 0 to length(Collapses) - 1 do
      DisposeObject(Collapses[i]);
  SetLength(Collapses, 0);
end;

function T2DPolygonGraph.CollapsesCount(): TGeoInt;
begin
  Result := length(Collapses);
end;

function T2DPolygonGraph.GetBands(const index: TGeoInt): T2DPolygon;
begin
  Result := Collapses[index];
end;

procedure T2DPolygonGraph.Remove(p: PVec2);
var
  i: TGeoInt;
begin
  if Surround.Remove(p) > 0 then
      Exit;
  for i := 0 to CollapsesCount - 1 do
    if Collapses[i].Remove(p) > 0 then
        Exit;
end;

procedure T2DPolygonGraph.FreeAndRemove(polygon: T2DPolygon);
var
  i: TGeoInt;
  L: T2DPolygonList;
begin
  if polygon = Surround then
      Clear()
  else
    begin
      L := T2DPolygonList.Create;
      for i := Low(Collapses) to High(Collapses) do
        if Collapses[i] = polygon then
            DisposeObject(Collapses[i])
        else
            L.Add(Collapses[i]);

      SetLength(Collapses, L.Count);
      for i := 0 to L.Count - 1 do
          Collapses[i] := L[i];

      DisposeObject(L);
    end;
end;

procedure T2DPolygonGraph.RemoveNullPolygon;
var
  i: TGeoInt;
  L: T2DPolygonList;
begin
  L := T2DPolygonList.Create;
  for i := 0 to CollapsesCount - 1 do
    if Collapses[i].Count = 0 then
        L.Add(Collapses[i]);

  for i := 0 to L.Count - 1 do
      FreeAndRemove(L[i]);
  DisposeObject(L);
end;

function T2DPolygonGraph.Total: TGeoInt;
var
  i: TGeoInt;
begin
  Result := Surround.Count;
  for i := 0 to CollapsesCount - 1 do
      inc(Result, Collapses[i].Count);
end;

function T2DPolygonGraph.BuildArray: TArray2DPoint;
var
  i, j, k: TGeoInt;
begin
  SetLength(Result, Total);
  k := 0;
  for i := 0 to Surround.Count - 1 do
    begin
      Result[k] := Surround[i]^;
      inc(k);
    end;
  for i := 0 to CollapsesCount - 1 do
    for j := 0 to Collapses[i].Count - 1 do
      begin
        Result[k] := Collapses[i][j]^;
        inc(k);
      end;
end;

function T2DPolygonGraph.BuildPArray: TArrayPVec2;
var
  i, j, k: TGeoInt;
begin
  SetLength(Result, Total);
  k := 0;
  for i := 0 to Surround.Count - 1 do
    begin
      Result[k] := Surround[i];
      inc(k);
    end;
  for i := 0 to CollapsesCount - 1 do
    for j := 0 to Collapses[i].Count - 1 do
      begin
        Result[k] := Collapses[i][j];
        inc(k);
      end;
end;

function T2DPolygonGraph.ExistsPVec(p: PVec2): Boolean;
var
  i: TGeoInt;
begin
  Result := True;
  if Surround.FList.IndexOf(p) >= 0 then
      Exit;
  for i := 0 to CollapsesCount - 1 do
    if Collapses[i].FList.IndexOf(p) >= 0 then
        Exit;
  Result := False;
end;

procedure T2DPolygonGraph.RotateAngle(axis: TVec2; Angle: TGeoFloat);
var
  i: TGeoInt;
begin
  Surround.RotateAngle(axis, Angle);
  for i := 0 to CollapsesCount - 1 do
      Collapses[i].RotateAngle(axis, Angle);
end;

procedure T2DPolygonGraph.Scale(Scale_: TGeoFloat);
var
  i: TGeoInt;
begin
  Surround.Scale(Scale_);
  for i := 0 to CollapsesCount - 1 do
      Collapses[i].Scale(Scale_);
end;

procedure T2DPolygonGraph.ProjectionTo(const sour, dest: TRectV2; const output: T2DPolygonGraph);
var
  i, j: TGeoInt;
  geo: T2DPolygon;
begin
  output.Clear;

  for i := 0 to Surround.Count - 1 do
      output.Surround.Add(RectProjection(sour, dest, Surround[i]^));

  for j := 0 to CollapsesCount - 1 do
    begin
      geo := output.NewCollapse();
      for i := 0 to Collapses[j].Count - 1 do
          geo.Add(RectProjection(sour, dest, Collapses[j][i]^));
    end;
end;

procedure T2DPolygonGraph.ProjectionTo(const dest: TRectV2; const output: T2DPolygonGraph);
begin
  ProjectionTo(BoundBox, dest, output);
end;

function T2DPolygonGraph.InHere(pt: TVec2): Boolean;
begin
  Result := False;
  if not InSurround(pt) then
      Exit;
  if InCollapse(pt) then
      Exit;
  Result := True;
end;

function T2DPolygonGraph.InSurround(pt: TVec2): Boolean;
begin
  Result := Surround.InHere(pt);
end;

function T2DPolygonGraph.InCollapse(pt: TVec2): Boolean;
var
  i: TGeoInt;
begin
  Result := True;
  for i := 0 to CollapsesCount - 1 do
    if Collapses[i].InHere(pt) then
        Exit;
  Result := False;
end;

function T2DPolygonGraph.Pick(pt: TVec2): T2DPolygon;
var
  i: TGeoInt;
begin
  Result := nil;

  for i := 0 to CollapsesCount - 1 do
    if Collapses[i].InHere(pt) then
      begin
        Result := Collapses[i];
        Exit;
      end;

  if Surround.InHere(pt) then
      Result := Surround;
end;

function T2DPolygonGraph.BoundBox: TRectV2;
begin
  Result := Surround.BoundBox;
end;

function T2DPolygonGraph.CollapseBounds: TRectV2Array;
var
  i: TGeoInt;
begin
  SetLength(Result, CollapsesCount);
  for i := 0 to CollapsesCount - 1 do
      Result[i] := Collapses[i].BoundBox;
end;

function T2DPolygonGraph.Line2Intersect(const lb, le: TVec2; output: T2DPolygon): Boolean;
var
  i: TGeoInt;
begin
  Result := Surround.Line2Intersect(lb, le, True, output);
  for i := 0 to CollapsesCount - 1 do
      Result := Result or Collapses[i].Line2Intersect(lb, le, True, output);
end;

function T2DPolygonGraph.GetNearLine(const pt: TVec2; out output: T2DPolygon; out lb, le: TGeoInt): TVec2;
type
  TNearLineData = record
    L: T2DPolygon;
    lb, le: TGeoInt;
    near_pt: TVec2;
  end;

  PNearLineData = ^TNearLineData;
  TNearLineDataArray = array of TNearLineData;
  TNearLineDataPtrArray = array of PNearLineData;

var
  buff_ori: TNearLineDataArray;
  buff: TNearLineDataPtrArray;
  procedure Fill_buff;
  var
    i: TGeoInt;
  begin
    for i := 0 to length(buff) - 1 do
        buff[i] := @buff_ori[i];
  end;

  procedure extract_NearLine();
  var
    i: TGeoInt;
  begin
    buff_ori[0].L := Surround;
    buff_ori[0].near_pt := Surround.GetNearLine(pt, True, buff_ori[0].lb, buff_ori[0].le);

    for i := 0 to CollapsesCount - 1 do
      begin
        buff_ori[i + 1].L := Collapses[i];
        buff_ori[i + 1].near_pt := Collapses[i].GetNearLine(pt, True, buff_ori[i + 1].lb, buff_ori[i + 1].le);
      end;
  end;

  procedure Fill_Result;
  var
    i: TGeoInt;
  begin
    // write result
    output := buff[0]^.L;
    lb := buff[0]^.lb;
    le := buff[0]^.le;
    Result := buff[0]^.near_pt;

    for i := 1 to length(buff) - 1 do
      begin
        if PointDistance(buff[i]^.near_pt, pt) < PointDistance(Result, pt) then
          begin
            output := buff[i]^.L;
            lb := buff[i]^.lb;
            le := buff[i]^.le;
            Result := buff[i]^.near_pt;
          end;
      end;
  end;

begin
  SetLength(buff_ori, CollapsesCount + 1);
  SetLength(buff, CollapsesCount + 1);
  Fill_buff();
  extract_NearLine();
  Fill_Result();

  // free buff
  SetLength(buff_ori, 0);
  SetLength(buff, 0);
end;

procedure T2DPolygonGraph.Transform(X, Y: TGeoFloat);
var
  i: TGeoInt;
begin
  Surround.Transform(X, Y);
  for i := 0 to CollapsesCount - 1 do
      Collapses[i].Transform(X, Y);
end;

procedure T2DPolygonGraph.Transform(V: TVec2);
begin
  Transform(V[0], V[1]);
end;

procedure T2DPolygonGraph.Mul(X, Y: TGeoFloat);
var
  i: TGeoInt;
begin
  Surround.Mul(X, Y);
  for i := 0 to CollapsesCount - 1 do
      Collapses[i].Mul(X, Y);
end;

procedure T2DPolygonGraph.Mul(V: TVec2);
begin
  Mul(V[0], V[1]);
end;

procedure T2DPolygonGraph.Mul(V: TGeoFloat);
begin
  Mul(V, V);
end;

procedure T2DPolygonGraph.FDiv(X, Y: TGeoFloat);
var
  i: TGeoInt;
begin
  Surround.FDiv(X, Y);
  for i := 0 to CollapsesCount - 1 do
      Collapses[i].FDiv(X, Y);
end;

procedure T2DPolygonGraph.FDiv(V: TVec2);
begin
  FDiv(V[0], V[1]);
end;

procedure T2DPolygonGraph.VertexReduction(Epsilon_: TGeoFloat);
var
  i: TGeoInt;
begin
  Surround.VertexReduction(Epsilon_);
  for i := 0 to CollapsesCount - 1 do
      Collapses[i].VertexReduction(Epsilon_);
end;

procedure T2DPolygonGraph.Reduction(Epsilon_: TGeoFloat);
begin
  VertexReduction(Epsilon_);
end;

procedure T2DPolygonGraph.SaveToStream(stream: TMS64);
var
  d: TDFE;
  m64: TMS64;
  i: TGeoInt;
begin
  d := TDFE.Create;
  d.WriteInteger(CollapsesCount);

  m64 := TMS64.CustomCreate(64 * 1024);
  Surround.SaveToStream(m64);
  d.WriteStream(m64);
  DisposeObject(m64);

  for i := 0 to CollapsesCount - 1 do
    begin
      m64 := TMS64.CustomCreate(64 * 1024);
      Collapses[i].SaveToStream(m64);
      d.WriteStream(m64);
      DisposeObject(m64);
    end;

  d.EncodeTo(stream, True);
  DisposeObject(d);
end;

procedure T2DPolygonGraph.LoadFromStream(stream: TMS64);
var
  d: TDFE;
  m64: TMS64;
  c, i: TGeoInt;
begin
  Clear;
  d := TDFE.Create;
  d.DecodeFrom(stream, True);
  c := d.Reader.ReadInteger;

  m64 := TMS64.Create;
  d.Reader.ReadStream(m64);
  m64.Position := 0;
  Surround.LoadFromStream(m64);
  DisposeObject(m64);

  for i := 0 to c - 1 do
    begin
      m64 := TMS64.Create;
      d.Reader.ReadStream(m64);
      m64.Position := 0;
      NewCollapse.LoadFromStream(m64);
      DisposeObject(m64);
    end;

  DisposeObject(d);
end;

procedure T2DPolygonGraph.Save_To_Bytes(var buff: TBytes);
var
  m64: TMS64;
begin
  m64 := TMS64.CustomCreate($FFFF);
  SaveToStream(m64);
  SetLength(buff, m64.size);
  CopyPtr(m64.Memory, @buff[0], m64.size);
  DisposeObject(m64);
end;

procedure T2DPolygonGraph.Load_From_Bytes(var buff: TBytes);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  m64.Mapping(@buff[0], length(buff));
  LoadFromStream(m64);
  DisposeObject(m64);
end;

constructor TDeflectionPolygon.Create;
begin
  inherited Create;
  FMaxRadius := 0;
  FList := TCore_List.Create;
  FName := '';
  FClassifier := '';
  FPosition := PointMake(0, 0);
  FScale := 1.0;
  FAngle := 0;
  FExpandMode := emConvex;
  FUserDataObject := nil;
  FUserData := nil;
end;

destructor TDeflectionPolygon.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TDeflectionPolygon.Reset;
begin
  FPosition := PointMake(0, 0);
  FMaxRadius := 0;
  FScale := 1.0;
  FAngle := 0;
  Clear;
end;

procedure TDeflectionPolygon.Assign(source: TCore_Object);
var
  i: TGeoInt;
  p, p2: PDeflectionPolygonVec;
begin
  if source is TDeflectionPolygon then
    begin
      Clear;

      FScale := TDeflectionPolygon(source).FScale;
      FAngle := TDeflectionPolygon(source).FAngle;
      FMaxRadius := TDeflectionPolygon(source).FMaxRadius;
      FPosition := TDeflectionPolygon(source).FPosition;
      FExpandMode := TDeflectionPolygon(source).FExpandMode;

      for i := 0 to TDeflectionPolygon(source).FList.Count - 1 do
        begin
          new(p);
          p2 := TDeflectionPolygon(source).DeflectionPolygon[i];
          p^.Owner := Self;
          p^.Angle := p2^.Angle;
          p^.Dist := p2^.Dist;
          FList.Add(p);
        end;
    end
  else if source is TV2L then
    begin
      Rebuild(TV2L(source), False);
    end;
end;

function TDeflectionPolygon.BuildArray: TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Points[i];
end;

function TDeflectionPolygon.BuildSplineSmoothInSideClosedArray: TArrayVec2;
var
  vl: TV2L;
begin
  vl := TV2L.Create;
  vl.Assign(Self);
  Result := vl.BuildSplineSmoothInSideClosedArray;
  DisposeObject(vl);
end;

function TDeflectionPolygon.BuildSplineSmoothOutSideClosedArray: TArrayVec2;
var
  vl: TV2L;
begin
  vl := TV2L.Create;
  vl.Assign(Self);
  Result := vl.BuildSplineSmoothOutSideClosedArray;
  DisposeObject(vl);
end;

function TDeflectionPolygon.BuildSplineSmoothOpenedArray: TArrayVec2;
var
  vl: TV2L;
begin
  vl := TV2L.Create;
  vl.Assign(Self);
  Result := vl.BuildSplineSmoothOpenedArray;
  DisposeObject(vl);
end;

function TDeflectionPolygon.BuildProjectionSplineSmoothInSideClosedArray(const sour, dest: TRectV2): TArrayVec2;
var
  vl: TV2L;
begin
  vl := TV2L.Create;
  ProjectionTo(sour, dest, vl);
  Result := vl.BuildSplineSmoothInSideClosedArray;
  DisposeObject(vl);
end;

function TDeflectionPolygon.BuildProjectionSplineSmoothOutSideClosedArray(const sour, dest: TRectV2): TArrayVec2;
var
  vl: TV2L;
begin
  vl := TV2L.Create;
  ProjectionTo(sour, dest, vl);
  Result := vl.BuildSplineSmoothOutSideClosedArray;
  DisposeObject(vl);
end;

function TDeflectionPolygon.BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAxis, destAxis: TVec2; const sourAngle, destAngle: TGeoFloat): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, Points[i]);
end;

function TDeflectionPolygon.BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAngle, destAngle: TGeoFloat): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := RectRotationProjection(sour, dest, sourAngle, destAngle, Points[i]);
end;

function TDeflectionPolygon.BuildProjectionArray(const sour, dest: TRectV2): TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := RectProjection(sour, dest, Points[i]);
end;

function TDeflectionPolygon.BuildProjectionArray(const dest: TRectV2): TArrayVec2;
begin
  Result := BuildProjectionArray(BoundBox, dest);
end;

procedure TDeflectionPolygon.ProjectionTo(const sour, dest: TRectV2; const output: TDeflectionPolygon);
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      output.AddPoint(RectProjection(sour, dest, Points[i]));
end;

procedure TDeflectionPolygon.ProjectionTo(const dest: TRectV2; const output: TDeflectionPolygon);
begin
  ProjectionTo(BoundBox, dest, output);
end;

procedure TDeflectionPolygon.ProjectionTo(const sour, dest: TRectV2; const output: TV2L);
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      output.Add(RectProjection(sour, dest, Points[i]));
end;

procedure TDeflectionPolygon.ProjectionTo(const dest: TRectV2; const output: TV2L);
begin
  ProjectionTo(BoundBox, dest, output);
end;

procedure TDeflectionPolygon.AddPoint(pt: TVec2);
begin
  AddPoint(pt[0], pt[1]);
end;

procedure TDeflectionPolygon.AddPoint(X, Y: TGeoFloat);
var
  pt: TVec2;
begin
  pt := PointMake(X, Y);
  Add(PointAngle(FPosition, pt), PointDistance(FPosition, pt));
end;

procedure TDeflectionPolygon.AddRectangle(R: TRectV2);
begin
  AddPoint(R[0, 0], R[0, 1]);
  AddPoint(R[1, 0], R[0, 1]);
  AddPoint(R[1, 0], R[1, 1]);
  AddPoint(R[0, 0], R[1, 1]);
end;

procedure TDeflectionPolygon.AddCirclePoint(count_: Cardinal; axis: TVec2; dist_: TGeoFloat);
var
  i: TGeoInt;
begin
  for i := 0 to count_ - 1 do
      AddPoint(PointRotation(axis, dist_, 360 / count_ * i));
end;

procedure TDeflectionPolygon.Add(angle_, dist_: TGeoFloat);
var
  p: PDeflectionPolygonVec;
begin
  if dist_ > FMaxRadius then
      FMaxRadius := dist_;
  new(p);
  p^.Owner := Self;
  p^.Angle := angle_ - FAngle;
  p^.Dist := dist_ / FScale;
  FList.Add(p);
end;

procedure TDeflectionPolygon.AddRectangle(R: TV2Rect4);
begin
  AddPoint(R.LeftTop);
  AddPoint(R.RightTop);
  AddPoint(R.RightBottom);
  AddPoint(R.LeftBottom);
end;

procedure TDeflectionPolygon.AddRectangle(arry: TArrayV2Rect4);
var
  i: Integer;
begin
  for i := low(arry) to high(arry) do
      AddRectangle(arry[i]);
end;

procedure TDeflectionPolygon.Insert(idx: TGeoInt; angle_, dist_: TGeoFloat);
var
  p: PDeflectionPolygonVec;
begin
  if dist_ > FMaxRadius then
      FMaxRadius := dist_;
  new(p);
  p^.Owner := Self;
  p^.Angle := angle_;
  p^.Dist := dist_;
  FList.Insert(idx, p);
end;

procedure TDeflectionPolygon.InsertPoint(idx: TGeoInt; pt: TVec2);
begin
  Insert(idx, NormalizeDegAngle(PointAngle(FPosition, pt) - Angle), PointDistance(FPosition, pt) / Scale);
end;

procedure TDeflectionPolygon.Delete(idx: TGeoInt);
begin
  Dispose(PDeflectionPolygonVec(FList[idx]));
  FList.Delete(idx);
end;

procedure TDeflectionPolygon.Clear;
var
  i: TGeoInt;
begin
  for i := 0 to FList.Count - 1 do
      Dispose(PDeflectionPolygonVec(FList[i]));
  FList.Clear;
end;

function TDeflectionPolygon.Count: TGeoInt;
begin
  Result := FList.Count;
end;

procedure TDeflectionPolygon.CopyPoly(pl: TDeflectionPolygon; AReversed: Boolean);
  procedure _Append(a, d: TGeoFloat);
  var
    p: PDeflectionPolygonVec;
  begin
    if d > FMaxRadius then
        FMaxRadius := d;
    new(p);
    p^.Owner := Self;
    p^.Angle := a;
    p^.Dist := d;
    FList.Add(p);
  end;

var
  i: TGeoInt;
begin
  Clear;
  FScale := pl.FScale;
  FAngle := pl.FAngle;
  FPosition := pl.FPosition;
  FMaxRadius := 0;
  if AReversed then
    begin
      for i := pl.Count - 1 downto 0 do
        with pl.DeflectionPolygon[i]^ do
            _Append(Angle, Dist);
    end
  else
    begin
      for i := 0 to pl.Count - 1 do
        with pl.DeflectionPolygon[i]^ do
            _Append(Angle, Dist);
    end;
end;

procedure TDeflectionPolygon.CopyExpandPoly(pl: TDeflectionPolygon; AReversed: Boolean; Dist: TGeoFloat);
var
  i: TGeoInt;
begin
  Clear;
  FScale := pl.FScale;
  FAngle := pl.FAngle;
  FPosition := pl.FPosition;
  FMaxRadius := 0;
  if AReversed then
    begin
      for i := pl.Count - 1 downto 0 do
          AddPoint(pl.Expands[i, Dist]);
    end
  else
    for i := 0 to pl.Count - 1 do
        AddPoint(pl.Expands[i, Dist]);
end;

procedure TDeflectionPolygon.Reverse;
var
  NewList: TCore_List;
  i, c: TGeoInt;
begin
  NewList := TCore_List.Create;
  c := Count - 1;
  NewList.Count := c + 1;
  for i := c downto 0 do
      NewList[c - i] := FList[i];
  DisposeObject(FList);
  FList := NewList;
end;

function TDeflectionPolygon.ScaleBeforeDistance: TGeoFloat;
var
  i: TGeoInt;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      Result := Result + PDeflectionPolygonVec(FList[i])^.Dist;
end;

function TDeflectionPolygon.ScaleAfterDistance: TGeoFloat;
var
  i: TGeoInt;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      Result := Result + PDeflectionPolygonVec(FList[i])^.Dist * FScale;
end;

procedure TDeflectionPolygon.RemoveSame;
var
  L, p: PDeflectionPolygonVec;
  i: TGeoInt;
begin
  if Count < 2 then
      Exit;

  L := PDeflectionPolygonVec(FList[0]);
  p := PDeflectionPolygonVec(FList[Count - 1]);
  while (Count >= 2) and (IsEqual(p^.Angle, L^.Angle)) and (IsEqual(p^.Dist, L^.Dist)) do
    begin
      Delete(Count - 1);
      p := PDeflectionPolygonVec(FList[Count - 1]);
    end;

  if Count < 2 then
      Exit;

  L := PDeflectionPolygonVec(FList[0]);
  i := 1;
  while i < Count do
    begin
      p := PDeflectionPolygonVec(FList[i]);
      if (IsEqual(p^.Angle, L^.Angle)) and (IsEqual(p^.Dist, L^.Dist)) then
          Delete(i)
      else
        begin
          L := p;
          inc(i);
        end;
    end;
end;

procedure TDeflectionPolygon.ConvexHullFrom(From_: TV2L);

const
  RightHandSide = -1;
  LeftHandSide = +1;
  CounterClockwise = +1;
  CollinearOrientation = 0;

type
  T2DHullPoint = record
    X: TGeoFloat;
    Y: TGeoFloat;
    Ang: TGeoFloat;
  end;

  TCompareResult = (eGreaterThan, eLessThan, eEqual);

var
  Point: array of T2DHullPoint;
  Stack: array of T2DHullPoint;
  StackHeadPosition: TGeoInt;
  Anchor: T2DHullPoint;

  function CartesianAngle(const X, Y: TGeoFloat): TGeoFloat;
  const
    _180DivPI = 57.295779513082320876798154814105000;
  begin
    if (X > Zero) and (Y > Zero) then
        Result := (ArcTan(Y / X) * _180DivPI)
    else if (X < Zero) and (Y > Zero) then
        Result := (ArcTan(-X / Y) * _180DivPI) + 90.0
    else if (X < Zero) and (Y < Zero) then
        Result := (ArcTan(Y / X) * _180DivPI) + 180.0
    else if (X > Zero) and (Y < Zero) then
        Result := (ArcTan(-X / Y) * _180DivPI) + 270.0
    else if (X = Zero) and (Y > Zero) then
        Result := 90.0
    else if (X < Zero) and (Y = Zero) then
        Result := 180.0
    else if (X = Zero) and (Y < Zero) then
        Result := 270.0
    else
        Result := Zero;
  end;

  procedure Swap(i, j: TGeoInt; var Point: array of T2DHullPoint);
  var
    Temp: T2DHullPoint;
  begin
    Temp := Point[i];
    Point[i] := Point[j];
    Point[j] := Temp;
  end;

  function CompareAngles(const p1, p2: T2DHullPoint): TCompareResult;
    function hEqual(const p1, p2: T2DHullPoint): Boolean;
    begin
      Result := IsEqual(p1.X, p2.X) and IsEqual(p1.Y, p2.Y);
    end;

  begin
    if p1.Ang < p2.Ang then
        Result := eLessThan
    else if p1.Ang > p2.Ang then
        Result := eGreaterThan
    else if hEqual(p1, p2) then
        Result := eEqual
    else if Distance(Anchor.X, Anchor.Y, p1.X, p1.Y) < Distance(Anchor.X, Anchor.Y, p2.X, p2.Y) then
        Result := eLessThan
    else
        Result := eGreaterThan;
  end;

  procedure RQSort(Left, Right: TGeoInt; var Point: array of T2DHullPoint);
  var
    i: TGeoInt;
    j: TGeoInt;
    Middle: TGeoInt;
    Pivot: T2DHullPoint;
  begin
    repeat
      i := Left;
      j := Right;
      Middle := (Left + Right) div 2;
      (* Median of 3 Pivot Selection *)
      if CompareAngles(Point[Middle], Point[Left]) = eLessThan then
          Swap(Left, Middle, Point);
      if CompareAngles(Point[Right], Point[Middle]) = eLessThan then
          Swap(Right, Middle, Point);
      if CompareAngles(Point[Middle], Point[Left]) = eLessThan then
          Swap(Left, Middle, Point);
      Pivot := Point[Right];
      repeat
        while CompareAngles(Point[i], Pivot) = eLessThan do
            inc(i);
        while CompareAngles(Point[j], Pivot) = eGreaterThan do
            dec(j);
        if i <= j then
          begin
            Swap(i, j, Point);
            inc(i);
            dec(j);
          end;
      until i > j;
      if Left < j then
          RQSort(Left, j, Point);
      Left := i;
    until i >= Right;
  end;

  procedure Push(Pnt: T2DHullPoint);
  begin
    inc(StackHeadPosition);
    Stack[StackHeadPosition] := Pnt;
  end;

  function Pop: Boolean;
  begin
    Result := False;
    if StackHeadPosition >= 0 then
      begin
        Result := True;
        dec(StackHeadPosition);
      end;
  end;

  function Head: T2DHullPoint;
  begin
    Assert((StackHeadPosition >= 0) and (StackHeadPosition < length(Stack)), 'Invalid stack-head position.');
    Result := Stack[StackHeadPosition];
  end;

  function PreHead: T2DHullPoint;
  begin
    Assert(((StackHeadPosition - 1) >= 0) and ((StackHeadPosition - 1) < length(Stack)), 'Invalid pre stack-head position.');
    Result := Stack[StackHeadPosition - 1];
  end;

  function PreHeadExist: Boolean;
  begin
    Result := (StackHeadPosition > 0);
  end;

  function Orientation(p1, p2, p3: T2DHullPoint): TGeoInt;
    function Orientation2(const x1, y1, x2, y2, Px, Py: TGeoFloat): TGeoInt;
    var
      Orin: TGeoFloat;
    begin
      (* Determinant of the 3 points *)
      Orin := (x2 - x1) * (Py - y1) - (Px - x1) * (y2 - y1);
      if Orin > Zero then
          Result := LeftHandSide (* Orientaion is to the left-hand side *)
      else if Orin < Zero then
          Result := RightHandSide (* Orientaion is to the right-hand side *)
      else
          Result := CollinearOrientation; (* Orientaion is neutral aka collinear *)
    end;

  begin
    Result := Orientation2(p1.X, p1.Y, p2.X, p2.Y, p3.X, p3.Y);
  end;

  procedure GrahamScan;
  var
    i: TGeoInt;
    Orin: TGeoInt;
  begin
    Push(Point[0]);
    Push(Point[1]);
    i := 2;
    while i < length(Point) do
      begin
        if PreHeadExist then
          begin
            Orin := Orientation(PreHead, Head, Point[i]);
            if Orin = CounterClockwise then
              begin
                Push(Point[i]);
                inc(i);
              end
            else
                Pop;
          end
        else
          begin
            Push(Point[i]);
            inc(i);
          end;
      end;
  end;

  function CalcCentroid: TVec2;
  var
    i: TGeoInt;
    j: TGeoInt;
    asum: TGeoFloat;
    term: TGeoFloat;
  begin
    Result := NULLPoint;

    asum := Zero;
    j := StackHeadPosition;

    for i := 0 to StackHeadPosition do
      begin
        term := ((Stack[j].X * Stack[i].Y) - (Stack[j].Y * Stack[i].X));
        asum := asum + term;
        Result[0] := Result[0] + (Stack[j].X + Stack[i].X) * term;
        Result[1] := Result[1] + (Stack[j].Y + Stack[i].Y) * term;
        j := i;
      end;

    if NotEqual(asum, Zero) then
      begin
        Result[0] := Result[0] / (3.0 * asum);
        Result[1] := Result[1] / (3.0 * asum);
      end;
  end;

var
  i: TGeoInt;
  j: TGeoInt;
  pt: TVec2;
begin
  if From_.Count <= 3 then
      Exit;

  StackHeadPosition := -1;

  try
    SetLength(Point, From_.Count);
    SetLength(Stack, From_.Count);
    j := 0;
    for i := 0 to From_.Count - 1 do
      begin
        pt := From_[i]^;
        Point[i].X := pt[0];
        Point[i].Y := pt[1];
        Point[i].Ang := 0.0;
        if Point[i].Y < Point[j].Y then
            j := i
        else if Point[i].Y = Point[j].Y then
          if Point[i].X < Point[j].X then
              j := i;
      end;

    Swap(0, j, Point);
    Point[0].Ang := 0;
    Anchor := Point[0];
    (* Calculate angle of the vertex ([ith point]-[anchorpoint]-[most left point]) *)
    for i := 1 to length(Point) - 1 do
        Point[i].Ang := CartesianAngle(Point[i].X - Anchor.X, Point[i].Y - Anchor.Y);
    (* Sort points in ascending order according to their angles *)
    RQSort(1, length(Point) - 1, Point);
    GrahamScan;

    { * make Circle * }
    FPosition := CalcCentroid;
    FMaxRadius := 0;

    { * rebuild opt * }
    FScale := 1.0;
    FAngle := 0;

    { * clear * }
    Clear;

    (* output list to self *)
    for i := 0 to StackHeadPosition do
        AddPoint(Stack[i].X, Stack[i].Y);
  finally
    (* Final clean-up *)
    Finalize(Stack);
    Finalize(Point);
  end;
  Rebuild;
end;

procedure TDeflectionPolygon.Rebuild(pl: TV2L; Scale_: TGeoFloat; angle_: TGeoFloat; ExpandMode_: TExpandMode; Position_: TVec2);
var
  i: TGeoInt;
  Ply: TDeflectionPolygon;
begin
  { * rebuild opt * }
  FMaxRadius := 0;
  FScale := Scale_;
  FAngle := angle_;
  ExpandMode := ExpandMode_;
  FPosition := Position_;

  { * rebuild Polygon * }
  Clear;
  for i := 0 to pl.Count - 1 do
      AddPoint(pl[i]^);
end;

procedure TDeflectionPolygon.Rebuild(pl: TV2L; reset_: Boolean);
var
  i: TGeoInt;
  Ply: TDeflectionPolygon;
begin
  { * rebuild opt * }
  FMaxRadius := 0;
  if reset_ then
    begin
      FPosition := pl.BoundCentre;
      FScale := 1.0;
      FAngle := 0;
    end;

  { * rebuild Polygon * }
  Clear;
  for i := 0 to pl.Count - 1 do
      AddPoint(pl[i]^);

  Ply := TDeflectionPolygon.Create;
  with Ply do
    begin
      CopyExpandPoly(Self, False, 1);
      if (Self.FExpandMode = emConvex) and (Self.ScaleBeforeDistance > ScaleBeforeDistance) then
          Self.Reverse
      else if (Self.FExpandMode = emConcave) and (Self.ScaleBeforeDistance < ScaleBeforeDistance) then
          Self.Reverse;
    end;
  DisposeObject(Ply);
end;

procedure TDeflectionPolygon.Rebuild;
var
  pl: TV2L;
  i: TGeoInt;
begin
  pl := TV2L.Create;
  for i := 0 to Count - 1 do
      pl.Add(GetPoint(i));
  Rebuild(pl, True);
  DisposeObject(pl);
end;

procedure TDeflectionPolygon.Rebuild(Scale_, angle_: TGeoFloat; ExpandMode_: TExpandMode; Position_: TVec2);
var
  pl: TV2L;
  i: TGeoInt;
begin
  pl := TV2L.Create;
  for i := 0 to Count - 1 do
      pl.Add(GetPoint(i));
  Scale := Scale_;
  Angle := angle_;
  ExpandMode := ExpandMode_;
  Position := Position_;
  Rebuild(pl, False);
  DisposeObject(pl);
end;

procedure TDeflectionPolygon.Rebuild_From_Projection(Source_Box, Dest_Box: TRectV2);
var
  pl: TV2L;
  i: TGeoInt;
begin
  pl := TV2L.Create;
  for i := 0 to Count - 1 do
      pl.Add(RectProjection(Source_Box, Dest_Box, GetPoint(i)));
  Position := RectProjection(Source_Box, Dest_Box, Position);
  Rebuild(pl, False);
  DisposeObject(pl);
end;

function TDeflectionPolygon.BoundBox: TRectV2;
var
  p: TVec2;
  MaxX: TGeoFloat;
  MaxY: TGeoFloat;
  MinX: TGeoFloat;
  MinY: TGeoFloat;
  i: TGeoInt;
begin
  Result := MakeRectV2(Zero, Zero, Zero, Zero);
  if Count < 2 then
      Exit;
  p := Points[0];
  MinX := p[0];
  MaxX := p[0];
  MinY := p[1];
  MaxY := p[1];

  for i := 1 to Count - 1 do
    begin
      p := Points[i];
      if p[0] < MinX then
          MinX := p[0]
      else if p[0] > MaxX then
          MaxX := p[0];
      if p[1] < MinY then
          MinY := p[1]
      else if p[1] > MaxY then
          MaxY := p[1];
    end;
  Result := MakeRectV2(MinX, MinY, MaxX, MaxY);
end;

function TDeflectionPolygon.Centroid: TVec2;
var
  i: TGeoInt;
  asum: TGeoFloat;
  term: TGeoFloat;

  pt1, pt2: TVec2;
begin
  Result := NULLPoint;

  if Count = 1 then
      Exit(Points[0]);

  if Count = 2 then
    begin
      pt1 := Points[0];
      pt2 := Points[1];
      Result := MiddleVec2(pt1, pt2);
      Exit;
    end;

  if Count < 3 then
      Exit;

  asum := Zero;
  pt2 := Points[Count - 1];

  for i := 0 to Count - 1 do
    begin
      pt1 := Points[i];

      term := ((pt2[0] * pt1[1]) - (pt2[1] * pt1[0]));
      asum := asum + term;
      Result[0] := Result[0] + (pt2[0] + pt1[0]) * term;
      Result[1] := Result[1] + (pt2[1] + pt1[1]) * term;
      pt2 := pt1;
    end;

  if NotEqual(asum, Zero) then
    begin
      Result[0] := Result[0] / (3.0 * asum);
      Result[1] := Result[1] / (3.0 * asum);
    end;
end;

function TDeflectionPolygon.Area: TGeoFloat;
var
  i, j: TGeoInt;
  v1, v2: TVec2;
begin
  Result := 0;
  if Count < 3 then
      Exit;
  j := Count - 1;
  v2 := GetPoint(j);
  for i := 0 to Count - 1 do
    begin
      v1 := GetPoint(i);
      Result := Result + ((v2[0] * v1[1]) - (v2[1] * v1[0]));
      v2 := v1;
      j := i;
    end;
  Result := Result * 0.5;
end;

function TDeflectionPolygon.InHere(pt: TVec2): Boolean;
var
  i: TGeoInt;
  pi, pj: TVec2;
begin
  Result := False;
  if Count < 3 then
      Exit;
  if not PointInCircle(pt, FPosition, FMaxRadius * FScale) then
      Exit;
  pj := GetPoint(Count - 1);
  for i := 0 to Count - 1 do
    begin
      pi := GetPoint(i);
      if ((pi[1] <= pt[1]) and (pt[1] < pj[1])) or // an upward crossing
        ((pj[1] <= pt[1]) and (pt[1] < pi[1])) then // a downward crossing
        begin
          (* compute the edge-ray intersect @ the x-coordinate *)
          if (pt[0] - pi[0] < ((pj[0] - pi[0]) * (pt[1] - pi[1]) / (pj[1] - pi[1]))) then
              Result := not Result;
        end;
      pj := pi;
    end;
end;

function TDeflectionPolygon.InHere(ExpandDistance_: TGeoFloat; pt: TVec2): Boolean;
var
  i: TGeoInt;
  pi, pj: TVec2;
begin
  Result := False;
  if Count < 3 then
      Exit;
  if not PointInCircle(pt, FPosition, FMaxRadius * FScale + ExpandDistance_) then
      Exit;
  pj := Expands[Count - 1, ExpandDistance_];
  for i := 0 to Count - 1 do
    begin
      pi := Expands[i, ExpandDistance_];
      if ((pi[1] <= pt[1]) and (pt[1] < pj[1])) or // an upward crossing
        ((pj[1] <= pt[1]) and (pt[1] < pi[1])) then // a downward crossing
        begin
          (* compute the edge-ray intersect @ the x-coordinate *)
          if ((pt[0] - pi[0]) < ((pj[0] - pi[0]) * (pt[1] - pi[1]) / (pj[1] - pi[1]))) then
              Result := not Result;
        end;
      pj := pi;
    end;
end;

function TDeflectionPolygon.LineIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
begin
  Result := False;
  if not Detect_Circle2Line(FPosition, FMaxRadius * FScale, lb, le) then
      Exit;

  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];
          if Intersect(lb, le, pt1, pt2) then
            begin
              Result := True;
              Exit;
            end;
          pt1 := pt2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          pt2 := Points[0];
          if Intersect(lb, le, pt1, pt2) then
            begin
              Result := True;
            end;
        end;
    end;
end;

function TDeflectionPolygon.LineIntersect(ExpandDistance_: TGeoFloat; const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
begin
  Result := False;
  if not Detect_Circle2Line(FPosition, FMaxRadius * FScale + ExpandDistance_, lb, le) then
      Exit;

  if FList.Count > 1 then
    begin
      pt1 := Expands[0, ExpandDistance_];
      for i := 1 to Count - 1 do
        begin
          pt2 := Expands[i, ExpandDistance_];
          if SimpleIntersect(lb, le, pt1, pt2) then
            begin
              Result := True;
              Exit;
            end;
          pt1 := pt2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          pt2 := Expands[0, ExpandDistance_];
          if SimpleIntersect(lb, le, pt1, pt2) then
              Result := True;
        end;
    end;
end;

function TDeflectionPolygon.LineIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean; out idx1, idx2: TGeoInt; out IntersectPt: TVec2): Boolean;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  Result := False;
  if not Detect_Circle2Line(FPosition, FMaxRadius * FScale, lb, le) then
      Exit;

  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];
          if Intersect(lb, le, pt1, pt2, opt) then
            begin
              d2 := PointDistance(lb, opt);
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := opt;
                  d := d2;
                  idx1 := i - 1;
                  idx2 := i;
                  Result := True;
                end;
            end;
          pt1 := pt2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          pt2 := Points[0];
          if Intersect(lb, le, pt1, pt2, opt) then
            begin
              d2 := PointDistance(lb, opt);
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := opt;
                  // d := d2;
                  idx1 := FList.Count - 1;
                  idx2 := 0;
                  Result := True;
                end;
            end;
        end;
    end;
end;

function TDeflectionPolygon.LineIntersect(ExpandDistance_: TGeoFloat; const lb, le: TVec2; const ClosedPolyMode: Boolean; out idx1, idx2: TGeoInt; out IntersectPt: TVec2): Boolean;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  Result := False;
  if not Detect_Circle2Line(FPosition, FMaxRadius * FScale + ExpandDistance_, lb, le) then
      Exit;

  if FList.Count > 1 then
    begin
      pt1 := Expands[0, ExpandDistance_];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Expands[i, ExpandDistance_];
          if Intersect(lb, le, pt1, pt2, opt) then
            begin
              d2 := PointDistance(lb, opt);
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := opt;
                  d := d2;
                  idx1 := i - 1;
                  idx2 := i;
                  Result := True;
                end;
            end;
          pt1 := pt2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          pt2 := Expands[0, ExpandDistance_];
          if Intersect(lb, le, pt1, pt2, opt) then
            begin
              d2 := PointDistance(lb, opt);
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := opt;
                  // d := d2;
                  idx1 := FList.Count - 1;
                  idx2 := 0;
                  Result := True;
                end;
            end;
        end;
    end;
end;

function TDeflectionPolygon.SimpleLineIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
begin
  Result := False;
  if not Detect_Circle2Line(FPosition, FMaxRadius * FScale, lb, le) then
      Exit;

  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];
          if SimpleIntersect(lb, le, pt1, pt2) then
            begin
              Result := True;
              Exit;
            end;
          pt1 := pt2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          pt2 := Points[0];
          if SimpleIntersect(lb, le, pt1, pt2) then
              Result := True;
        end;
    end;
end;

function TDeflectionPolygon.GetNearLine(const pt: TVec2; const ClosedPolyMode: Boolean; out lb, le: TGeoInt): TVec2;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];

          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);

          d2 := PointDistance(pt, opt);
          if (i = 1) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
              lb := i - 1;
              le := i;
            end;

          pt1 := pt2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          pt2 := Points[0];
          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);
          d2 := PointDistance(pt, opt);
          if (d2 < d) then
            begin
              Result := opt;
              lb := FList.Count - 1;
              le := 0;
            end;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Points[0];
          lb := 0;
          le := 0;
        end
      else
        begin
          Result := NULLPoint;
          lb := -1;
          le := -1;
        end;
    end;
end;

function TDeflectionPolygon.GetNearLine(ExpandDistance_: TGeoFloat; const pt: TVec2; const ClosedPolyMode: Boolean; out lb, le: TGeoInt): TVec2;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Expands[0, ExpandDistance_];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Expands[i, ExpandDistance_];
          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);
          d2 := PointDistance(pt, opt);
          if (i = 1) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
              lb := i - 1;
              le := i;
            end;

          pt1 := pt2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          pt2 := Expands[0, ExpandDistance_];
          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);
          d2 := PointDistance(pt, opt);
          if (d2 < d) then
            begin
              Result := opt;
              lb := FList.Count - 1;
              le := 0;
            end;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Points[0];
          lb := 0;
          le := 0;
        end
      else
        begin
          Result := NULLPoint;
          lb := -1;
          le := -1;
        end;
    end;
end;

function TDeflectionPolygon.Collision2Circle(cp: TVec2; R: TGeoFloat; ClosedPolyMode: Boolean): Boolean;
var
  i: TGeoInt;
  curpt, destpt: TVec2;
begin
  if (Detect_Circle2Circle(FPosition, cp, FMaxRadius * FScale, R)) and (Count > 0) then
    begin
      Result := True;
      curpt := Points[0];
      for i := 1 to Count - 1 do
        begin
          destpt := Points[i];
          if Detect_Circle2Line(cp, R, curpt, destpt) then
              Exit;
          curpt := destpt;
        end;
      if ClosedPolyMode and (Count >= 3) then
        if Detect_Circle2Line(cp, R, curpt, Points[0]) then
            Exit;
    end;
  Result := False;
end;

function TDeflectionPolygon.Collision2Circle(cp: TVec2; R: TGeoFloat; ClosedPolyMode: Boolean; OutputLine: TDeflectionPolygonLines): Boolean;
var
  i: TGeoInt;
  curpt, destpt: TVec2;
begin
  Result := False;
  if (Detect_Circle2Circle(FPosition, cp, FMaxRadius * FScale, R)) and (Count > 0) then
    begin
      curpt := Points[0];
      for i := 1 to Count - 1 do
        begin
          destpt := Points[i];
          if Detect_Circle2Line(cp, R, curpt, destpt) then
            begin
              OutputLine.Add(curpt, destpt, i - 1, i, Self);
              Result := True;
            end;
          curpt := destpt;
        end;
      if ClosedPolyMode and (Count >= 3) then
        if Detect_Circle2Line(cp, R, curpt, Points[0]) then
          begin
            OutputLine.Add(curpt, Points[0], Count - 1, 0, Self);
            Result := True;
          end;
    end;
end;

function TDeflectionPolygon.Collision2Circle(ExpandDistance_: TGeoFloat; cp: TVec2; R: TGeoFloat; ClosedPolyMode: Boolean; OutputLine: TDeflectionPolygonLines): Boolean;
var
  i: TGeoInt;
  curpt, destpt: TVec2;
begin
  Result := False;
  if (Detect_Circle2Circle(FPosition, cp, FMaxRadius * FScale + ExpandDistance_, R)) and (Count > 0) then
    begin
      curpt := Expands[0, ExpandDistance_];
      for i := 1 to Count - 1 do
        begin
          destpt := Expands[i, ExpandDistance_];
          if Detect_Circle2Line(cp, R, curpt, destpt) then
            begin
              OutputLine.Add(curpt, destpt, i - 1, i, Self);
              Result := True;
            end;
          curpt := destpt;
        end;
      if ClosedPolyMode and (Count >= 3) then
        if Detect_Circle2Line(cp, R, curpt, Expands[0, ExpandDistance_]) then
          begin
            OutputLine.Add(curpt, Expands[0, ExpandDistance_], Count - 1, 0, Self);
            Result := True;
          end;
    end;
end;

function TDeflectionPolygon.PolygonIntersect(Poly_: TDeflectionPolygon): Boolean;
var
  i: TGeoInt;
begin
  Result := Detect_Circle2Circle(Position, Poly_.Position, MaxRadius * FScale, Poly_.MaxRadius * Poly_.Scale);
  if not Result then
      Exit;

  for i := 0 to Count - 1 do
    if Poly_.InHere(Points[i]) then
        Exit;

  for i := 0 to Poly_.Count - 1 do
    if InHere(Poly_.Points[i]) then
        Exit;

  // line intersect
  for i := 1 to Poly_.Count - 1 do
    if LineIntersect(Poly_.Points[i - 1], Poly_.Points[i], True) then
        Exit;

  // line intersect
  if LineIntersect(Poly_.Points[Count - 1], Poly_.Points[0], True) then
      Exit;

  Result := False;
end;

function TDeflectionPolygon.PolygonIntersect(vl_: TV2L): Boolean;
var
  i: TGeoInt;
begin
  Result := True;

  for i := 0 to Count - 1 do
    if vl_.InHere(Points[i]) then
        Exit;

  for i := 0 to vl_.Count - 1 do
    if InHere(vl_[i]^) then
        Exit;

  // line intersect
  for i := 1 to vl_.Count - 1 do
    if LineIntersect(vl_[i - 1]^, vl_[i]^, True) then
        Exit;

  // line intersect
  if LineIntersect(vl_[Count - 1]^, vl_[0]^, True) then
      Exit;

  Result := False;
end;

function TDeflectionPolygon.LerpToEdge(pt: TVec2; ProjDistance_, ExpandDistance_: TGeoFloat; FromIdx, toidx: TGeoInt): TVec2;
  function NextIndexStep(CurIdx: TGeoInt; curDir: ShortInt): TGeoInt;
  begin
    if curDir < 0 then
      begin
        if CurIdx = 0 then
            Result := Count - 1
        else if CurIdx > 0 then
            Result := CurIdx - 1
        else
            Result := Count + CurIdx - 1;
      end
    else
      begin
        if CurIdx = Count - 1 then
            Result := 0
        else if CurIdx < Count - 1 then
            Result := CurIdx + 1
        else
            Result := CurIdx - Count;
      end;
    if (Result < 0) or (Result >= Count) then
        Result := -1;
  end;

var
  idxDir: ShortInt;
  ToPt: TVec2;
  d: TGeoFloat;
begin
  Result := pt;
  if Count <= 1 then
      Exit;

  if (FromIdx = Count - 1) and (toidx = 0) then
      idxDir := 1
  else if (FromIdx = 0) and (toidx = Count - 1) then
      idxDir := -1
  else if toidx < FromIdx then
      idxDir := -1
  else
      idxDir := 1;

  while True do
    begin
      ToPt := Expands[toidx, ExpandDistance_];
      d := PointDistance(pt, ToPt);

      if ProjDistance_ < d then
        begin
          Result := PointLerpTo(pt, ToPt, ProjDistance_);
          Exit;
        end;

      if d > 0 then
        begin
          pt := PointLerpTo(pt, ToPt, d);
          ProjDistance_ := ProjDistance_ - d;
        end;
      toidx := NextIndexStep(toidx, idxDir);
    end;
end;

function TDeflectionPolygon.GetDeflectionPolygon(index: TGeoInt): PDeflectionPolygonVec;
begin
  Result := FList[index];
end;

function TDeflectionPolygon.GetPoint(idx: TGeoInt): TVec2;
var
  p: PDeflectionPolygonVec;
begin
  p := GetDeflectionPolygon(idx);
  Result := PointRotation(FPosition, p^.Dist * FScale, p^.Angle + FAngle);
end;

procedure TDeflectionPolygon.SetPoint(idx: TGeoInt; Value: TVec2);
var
  p: PDeflectionPolygonVec;
begin
  p := GetDeflectionPolygon(idx);
  p^.Angle := PointAngle(FPosition, Value) - FAngle;
  p^.Dist := PointDistance(FPosition, Value);
  if p^.Dist > FMaxRadius then
      FMaxRadius := p^.Dist;
  p^.Dist := p^.Dist / FScale;
end;

function TDeflectionPolygon.FirstPoint: TVec2;
begin
  Result := GetPoint(0);
end;

function TDeflectionPolygon.LastPoint: TVec2;
begin
  Result := GetPoint(Count - 1);
end;

function TDeflectionPolygon.GetExpands(idx: TGeoInt; ExpandDist: TGeoFloat): TVec2;
var
  lpt, pt, rpt: TVec2;
  ln, rn: TVec2;
  Dx, Dy, f, R: TGeoFloat;
  Cx, Cy: TGeoFloat;
begin
  if (ExpandDist = 0) or (Count < 2) then
    begin
      Result := Points[idx];
      Exit;
    end;

  if idx > 0 then
      lpt := Points[idx - 1]
  else
      lpt := Points[Count - 1];
  if idx + 1 < Count then
      rpt := Points[idx + 1]
  else
      rpt := Points[0];
  pt := Points[idx];

  // normal : left to
  Dx := (pt[0] - lpt[0]);
  Dy := (pt[1] - lpt[1]);
  f := 1.0 / HypotX(Dx, Dy);
  ln[0] := (Dy * f);
  ln[1] := -(Dx * f);

  // normal : right to
  Dx := (rpt[0] - pt[0]);
  Dy := (rpt[1] - pt[1]);
  f := 1.0 / HypotX(Dx, Dy);
  rn[0] := (Dy * f);
  rn[1] := -(Dx * f);

  // compute the expand edge
  Dx := (ln[0] + rn[0]);
  Dy := (ln[1] + rn[1]);
  R := (ln[0] * Dx) + (ln[1] * Dy);
  if R = 0 then
      R := 1;
  Cx := (Dx * ExpandDist / R);
  Cy := (Dy * ExpandDist / R);

  if FExpandMode = emConcave then
    begin
      Result[0] := pt[0] - Cx;
      Result[1] := pt[1] - Cy;
    end
  else
    begin
      Result[0] := pt[0] + Cx;
      Result[1] := pt[1] + Cy;
    end;
end;

procedure TDeflectionPolygon.SaveToStream(stream: TMS64);
var
  i: TGeoInt;
  p: PDeflectionPolygonVec;
begin
  stream.WriteSingle(FScale);
  stream.WriteSingle(FAngle);
  stream.WriteSingle(FPosition[0]);
  stream.WriteSingle(FPosition[1]);
  stream.WriteInt32(Count);
  for i := 0 to Count - 1 do
    begin
      p := DeflectionPolygon[i];
      stream.WriteSingle(p^.Angle);
      stream.WriteSingle(p^.Dist);
    end;
end;

procedure TDeflectionPolygon.LoadFromStream(stream: TMS64);
var
  c: TGeoInt;
  i: TGeoInt;
  p: PDeflectionPolygonVec;
begin
  Clear;
  FScale := stream.ReadSingle;
  FAngle := stream.ReadSingle;
  FPosition[0] := stream.ReadSingle;
  FPosition[1] := stream.ReadSingle;
  FMaxRadius := 0;
  c := stream.ReadInt32;
  for i := 0 to c - 1 do
    begin
      new(p);
      p^.Owner := Self;
      p^.Angle := stream.ReadSingle;
      p^.Dist := stream.ReadSingle;
      FList.Add(p);

      if p^.Dist > FMaxRadius then
          FMaxRadius := p^.Dist;
    end;
end;

constructor TDeflectionPolygonList.Create;
begin
  inherited Create;
  AutoFree := True;
  BackgroundBox := ZeroRectV2;
end;

destructor TDeflectionPolygonList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TDeflectionPolygonList.Remove(obj: TDeflectionPolygon);
begin
  if AutoFree then
      DisposeObject(obj);
  inherited Remove(obj);
end;

procedure TDeflectionPolygonList.Delete(index: TGeoInt);
begin
  if (index >= 0) and (index < Count) then
    begin
      if AutoFree then
          DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TDeflectionPolygonList.Clear;
var
  i: TGeoInt;
begin
  if AutoFree then
    for i := 0 to Count - 1 do
        DisposeObject(Items[i]);
  inherited Clear;
end;

function TDeflectionPolygonList.BoundBox: TRectV2;
var
  i: TGeoInt;
begin
  Result := ZeroRectV2;
  if Count > 0 then
    begin
      Result := First.BoundBox;
      for i := 1 to Count - 1 do
          Result := BoundRect(Result, Items[i].BoundBox);
    end;
end;

procedure TDeflectionPolygonList.Rebuild_From_New_Background_Box(NewBox: TRectV2);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Rebuild_From_Projection(BackgroundBox, NewBox);
  BackgroundBox := NewBox;
end;

function TDeflectionPolygonList.FindPolygon(Name: TPascalString): TDeflectionPolygon;
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
    if Name.Same(Items[i].Name) then
      begin
        Result := Items[i];
        Exit;
      end;
  Result := nil;
end;

function TDeflectionPolygonList.MakePolygonName(Name: TPascalString): TPascalString;
var
  i: TGeoInt;
begin
  Result := Name;
  i := 0;
  while FindPolygon(Result) <> nil do
    begin
      Result := PFormat('%s%d', [Name.Text, i]);
      inc(i);
    end;
end;

procedure TDeflectionPolygonList.SaveToStream(stream: TCore_Stream);
var
  d: TDFE;
  i: TGeoInt;
  dp: TDeflectionPolygon;
  m64: TMS64;
begin
  d := TDFE.Create;

  d.WriteRectV2(BackgroundBox);

  d.WriteInteger(Count);

  for i := 0 to Count - 1 do
    begin
      dp := Items[i];
      d.WriteString(dp.Name);
      d.WriteString(dp.Classifier);

      m64 := TMS64.Create;
      dp.SaveToStream(m64);
      d.WriteStream(m64);
      DisposeObject(m64);
    end;

  d.EncodeAsZLib(stream, False);
  DisposeObject(d);
end;

procedure TDeflectionPolygonList.LoadFromStream(stream: TCore_Stream);
var
  d: TDFE;
  i, c: TGeoInt;
  dp: TDeflectionPolygon;
  m64: TMS64;
begin
  Clear;
  d := TDFE.Create;
  d.DecodeFrom(stream, False);

  BackgroundBox := d.Reader.ReadRectV2;

  c := d.Reader.ReadInteger;

  for i := 0 to c - 1 do
    begin
      dp := TDeflectionPolygon.Create;
      dp.FName := d.Reader.ReadString;
      dp.FClassifier := d.Reader.ReadString;

      m64 := TMS64.Create;
      d.Reader.ReadStream(m64);
      m64.Position := 0;
      dp.LoadFromStream(m64);
      DisposeObject(m64);
      Add(dp);
    end;

  DisposeObject(d);
end;

procedure TDeflectionPolygonList.LoadFromBase64(const buff: TPascalString);
var
  m64: TMS64;
begin
  if not umlTestBase64(buff) then
      RaiseInfo('illegal base64 data.');

  m64 := TMS64.Create;
  umlDecodeStreamBASE64(buff, m64);
  m64.Position := 0;
  LoadFromStream(m64);
  DisposeObject(m64);
end;

procedure TDeflectionPolygonLine.SetLocation(const lb, le: TVec2);
begin
  buff[0] := lb;
  buff[1] := le;
end;

function TDeflectionPolygonLine.ExpandPoly(ExpandDist: TGeoFloat): TDeflectionPolygonLine;
begin
  Result := Self;
  if OwnerDeflectionPolygon <> nil then
    begin
      Result.buff[0] := OwnerDeflectionPolygon.Expands[OwnerDeflectionPolygonIndex[0], ExpandDist];
      Result.buff[1] := OwnerDeflectionPolygon.Expands[OwnerDeflectionPolygonIndex[1], ExpandDist];
    end;
end;

function TDeflectionPolygonLine.length: TGeoFloat;
begin
  Result := PointDistance(buff[0], buff[1]);
end;

function TDeflectionPolygonLine.MinimumDistance(const pt: TVec2): TGeoFloat;
begin
  Result := PointDistance(pt, ClosestPointFromLine(pt));
end;

function TDeflectionPolygonLine.MinimumDistance(ExpandDist: TGeoFloat; const pt: TVec2): TGeoFloat;
begin
  Result := PointDistance(pt, ClosestPointFromLine(ExpandDist, pt));
end;

function TDeflectionPolygonLine.ClosestPointFromLine(const pt: TVec2): TVec2;
begin
  Result := ClosestPointOnSegmentFromPoint(buff[0], buff[1], pt);
end;

function TDeflectionPolygonLine.ClosestPointFromLine(ExpandDist: TGeoFloat; const pt: TVec2): TVec2;
var
  E: TDeflectionPolygonLine;
begin
  E := ExpandPoly(ExpandDist);
  Result := ClosestPointOnSegmentFromPoint(E.buff[0], E.buff[1], pt);
end;

function TDeflectionPolygonLine.MiddlePoint: TVec2;
begin
  Result := MiddleVec2(buff[0], buff[1]);
end;

function TDeflectionPolygonLines.GetItems(index: TGeoInt): PDeflectionPolygonLine;
begin
  Result := FList[index];
end;

constructor TDeflectionPolygonLines.Create;
begin
  inherited Create;
  FList := TCore_List.Create;
  FUserData := nil;
  FUserObject := nil;
end;

destructor TDeflectionPolygonLines.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TDeflectionPolygonLines.Assign(source: TCore_Persistent);
var
  i: TGeoInt;
begin
  if source is TDeflectionPolygonLines then
    begin
      Clear;
      for i := 0 to TDeflectionPolygonLines(source).Count - 1 do
          Add(TDeflectionPolygonLines(source)[i]^);
    end;
end;

function TDeflectionPolygonLines.Add(V: TDeflectionPolygonLine): TGeoInt;
var
  p: PDeflectionPolygonLine;
begin
  new(p);
  p^ := V;
  Result := FList.Add(p);
  p^.index := Result;
end;

function TDeflectionPolygonLines.Add(lb, le: TVec2): TGeoInt;
var
  p: PDeflectionPolygonLine;
begin
  new(p);
  p^.buff[0] := lb;
  p^.buff[1] := le;
  p^.OwnerDeflectionPolygonIndex[0] := -1;
  p^.OwnerDeflectionPolygonIndex[1] := -1;
  p^.OwnerDeflectionPolygon := nil;
  Result := FList.Add(p);
  p^.index := Result;
end;

function TDeflectionPolygonLines.Add(lb, le: TVec2; idx1, idx2: TGeoInt; polygon: TDeflectionPolygon): TGeoInt;
var
  p: PDeflectionPolygonLine;
begin
  new(p);
  p^.buff[0] := lb;
  p^.buff[1] := le;
  p^.OwnerDeflectionPolygonIndex[0] := idx1;
  p^.OwnerDeflectionPolygonIndex[1] := idx2;
  p^.OwnerDeflectionPolygon := polygon;
  Result := FList.Add(p);
  p^.index := Result;
end;

function TDeflectionPolygonLines.Count: TGeoInt;
begin
  Result := FList.Count;
end;

procedure TDeflectionPolygonLines.Delete(index: TGeoInt);
var
  p: PDeflectionPolygonLine;
  i: TGeoInt;
begin
  p := FList[index];
  Dispose(p);
  FList.Delete(index);
  for i := index to Count - 1 do
      Items[i]^.index := i;
end;

procedure TDeflectionPolygonLines.Clear;
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      Dispose(PDeflectionPolygonLine(FList[i]));
  FList.Clear;
end;

function TDeflectionPolygonLines.NearLine(const ExpandDist: TGeoFloat; const pt: TVec2): PDeflectionPolygonLine;
var
  d, d2: TGeoFloat;
  L: PDeflectionPolygonLine;
  i: TGeoInt;
begin
  Result := nil;
  if Count = 1 then
    begin
      Result := Items[0];
    end
  else if Count > 1 then
    begin
      L := Items[0];
      if ExpandDist = 0 then
          d := L^.MinimumDistance(pt)
      else
          d := L^.MinimumDistance(ExpandDist, pt);
      Result := L;

      for i := 1 to Count - 1 do
        begin
          L := Items[i];

          if ExpandDist = 0 then
              d2 := L^.MinimumDistance(pt)
          else
              d2 := L^.MinimumDistance(ExpandDist, pt);

          if d2 < d then
            begin
              Result := L;
              d := d2;
            end;
        end;
    end;
end;

function TDeflectionPolygonLines.FarLine(const ExpandDist: TGeoFloat; const pt: TVec2): PDeflectionPolygonLine;
var
  d, d2: TGeoFloat;
  L: PDeflectionPolygonLine;
  i: TGeoInt;
begin
  Result := nil;
  if Count > 0 then
    begin
      L := Items[0];
      if ExpandDist = 0 then
          d := L^.MinimumDistance(pt)
      else
          d := L^.MinimumDistance(ExpandDist, pt);
      Result := L;

      for i := 1 to Count - 1 do
        begin
          L := Items[i];

          if ExpandDist = 0 then
              d2 := L^.MinimumDistance(pt)
          else
              d2 := L^.MinimumDistance(ExpandDist, pt);

          if d2 > d then
            begin
              Result := L;
              d := d2;
            end;
        end;
    end;
end;

procedure TDeflectionPolygonLines.SortOfNear(const pt: TVec2);

  function Compare_(Left, Right: pointer): ShortInt;
  var
    d1, d2: TGeoFloat;
  begin
    d1 := PDeflectionPolygonLine(Left)^.MinimumDistance(pt);
    d2 := PDeflectionPolygonLine(Right)^.MinimumDistance(pt);
    Result := CompareFloat(d1, d2);
  end;

  procedure fastSort_(var Arry_: TCore_PointerList; L, R: TGeoInt);
  var
    i, j: TGeoInt;
    p, tmp: pointer;
  begin
    if L < R then
      begin
        repeat
          if (R - L) = 1 then
            begin
              if Compare_(Arry_[L], Arry_[R]) > 0 then
                begin
                  tmp := Arry_[L];
                  Arry_[L] := Arry_[R];
                  Arry_[R] := tmp;
                end;
              break;
            end;
          i := L;
          j := R;
          p := Arry_[(L + R) shr 1];
          repeat
            while Compare_(Arry_[i], p) < 0 do
                inc(i);
            while Compare_(Arry_[j], p) > 0 do
                dec(j);
            if i <= j then
              begin
                if i <> j then
                  begin
                    tmp := Arry_[i];
                    Arry_[i] := Arry_[j];
                    Arry_[j] := tmp;
                  end;
                inc(i);
                dec(j);
              end;
          until i > j;
          if (j - L) > (R - i) then
            begin
              if i < R then
                  fastSort_(Arry_, i, R);
              R := j;
            end
          else
            begin
              if L < j then
                  fastSort_(Arry_, L, j);
              L := i;
            end;
        until L >= R;
      end;
  end;

var
  i: TGeoInt;
begin
  if Count > 1 then
      fastSort_(FList.ListData^, 0, Count - 1);
  for i := 0 to Count - 1 do
      Items[i]^.index := i;
end;

procedure TDeflectionPolygonLines.SortOfFar(const pt: TVec2);

  function Compare_(Left, Right: pointer): ShortInt;
  var
    d1, d2: TGeoFloat;
  begin
    d1 := PDeflectionPolygonLine(Left)^.MinimumDistance(pt);
    d2 := PDeflectionPolygonLine(Right)^.MinimumDistance(pt);
    Result := CompareFloat(d2, d1);
  end;

  procedure fastSort_(var Arry_: TCore_PointerList; L, R: TGeoInt);
  var
    i, j: TGeoInt;
    p, tmp: pointer;
  begin
    if L < R then
      begin
        repeat
          if (R - L) = 1 then
            begin
              if Compare_(Arry_[L], Arry_[R]) > 0 then
                begin
                  tmp := Arry_[L];
                  Arry_[L] := Arry_[R];
                  Arry_[R] := tmp;
                end;
              break;
            end;
          i := L;
          j := R;
          p := Arry_[(L + R) shr 1];
          repeat
            while Compare_(Arry_[i], p) < 0 do
                inc(i);
            while Compare_(Arry_[j], p) > 0 do
                dec(j);
            if i <= j then
              begin
                if i <> j then
                  begin
                    tmp := Arry_[i];
                    Arry_[i] := Arry_[j];
                    Arry_[j] := tmp;
                  end;
                inc(i);
                dec(j);
              end;
          until i > j;
          if (j - L) > (R - i) then
            begin
              if i < R then
                  fastSort_(Arry_, i, R);
              R := j;
            end
          else
            begin
              if L < j then
                  fastSort_(Arry_, L, j);
              L := i;
            end;
        until L >= R;
      end;
  end;

var
  i: TGeoInt;
begin
  if Count > 1 then
      fastSort_(FList.ListData^, 0, Count - 1);
  for i := 0 to Count - 1 do
      Items[i]^.index := i;
end;

constructor TTriangleList.Create;
begin
  inherited Create;
end;

destructor TTriangleList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTriangleList.AddTri(T_: TTriangle);
var
  p: PTriangle;
begin
  new(p);
  p^ := T_;
  inherited Add(p);
end;

procedure TTriangleList.Remove(p: PTriangle);
begin
  Dispose(p);
  inherited Remove(p);
end;

procedure TTriangleList.Delete(index: TGeoInt);
begin
  if (index >= 0) and (index < Count) then
    begin
      Dispose(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TTriangleList.Clear;
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      Dispose(Items[i]);
  inherited Clear;
end;

procedure TTriangleList.BuildTriangle(polygon: TV2L);
var
  Graph: TGraph2D_;
  mesh: TDelaunayMesh2D_;
  i: TGeoInt;
  v1, v2: TVec2;
  first_vert, Vert1, vert2: TVertex2D_;
  mesh_tri: TTriangle2D_;
  T_: TTriangle;
begin
  Clear;

  if polygon.Count < 3 then
      Exit;

  Graph := TGraph2D_.Create;
  mesh := TDelaunayMesh2D_.Create;

  v1 := polygon[0]^;
  Vert1 := TVertex2D_.CreateWithCoords(v1[0], v1[1]);
  first_vert := Vert1;
  Graph.Vertices.Add(Vert1);
  for i := 1 to polygon.Count - 1 do
    begin
      v2 := polygon[i]^;
      vert2 := TVertex2D_.CreateWithCoords(v2[0], v2[1]);
      Graph.Vertices.Add(vert2);
      Graph.Segments.Add(TSegment2D_.CreateWithVertices(Vert1, vert2));
      v1 := v2;
      Vert1 := vert2;
    end;
  Graph.Segments.Add(TSegment2D_.CreateWithVertices(vert2, first_vert));

  mesh.AddGraph(Graph);

  // If the mesh has no segments, we will add a convex hull
  if mesh.Segments.Count = 0 then
      mesh.ConvexHull;

  mesh.Triangulate(TRemovalStyle_.rsOutside);

  for i := 0 to mesh.Triangles.Count - 1 do
    begin
      mesh_tri := mesh.Triangles[i];
      with mesh_tri.Vertices[0].Point^ do
          T_[0] := vec2(X, Y);
      with mesh_tri.Vertices[1].Point^ do
          T_[1] := vec2(X, Y);
      with mesh_tri.Vertices[2].Point^ do
          T_[2] := vec2(X, Y);

      AddTri(T_);
    end;

  DisposeObject(mesh);
  DisposeObject(Graph);
end;

procedure TTriangleList.BuildTriangle(polygon: TV2L; MinAngle, MinSegmentLength, MaxElementSize: TGeoFloat);
var
  Graph: TGraph2D_;
  mesh: TQualityMesh2D_;
  i: TGeoInt;
  v1, v2: TVec2;
  first_vert, Vert1, vert2: TVertex2D_;
  mesh_tri: TTriangle2D_;
  T_: TTriangle;
begin
  Clear;

  if polygon.Count < 3 then
      Exit;

  Graph := TGraph2D_.Create;
  mesh := TQualityMesh2D_.Create;

  v1 := polygon[0]^;
  Vert1 := TVertex2D_.CreateWithCoords(v1[0], v1[1]);
  first_vert := Vert1;
  Graph.Vertices.Add(Vert1);
  for i := 1 to polygon.Count - 1 do
    begin
      v2 := polygon[i]^;
      vert2 := TVertex2D_.CreateWithCoords(v2[0], v2[1]);
      Graph.Vertices.Add(vert2);
      Graph.Segments.Add(TSegment2D_.CreateWithVertices(Vert1, vert2));
      v1 := v2;
      Vert1 := vert2;
    end;
  Graph.Segments.Add(TSegment2D_.CreateWithVertices(vert2, first_vert));

  mesh.AddGraph(Graph);

  // If the mesh has no segments, we will add a convex hull
  if mesh.Segments.Count = 0 then
      mesh.ConvexHull;

  mesh.MinimumAngle := MinAngle;
  mesh.MinimumSegmentLength := MinSegmentLength;
  mesh.MaximumElementSize := MaxElementSize;

  mesh.Triangulate(TRemovalStyle_.rsOutside);

  for i := 0 to mesh.Triangles.Count - 1 do
    begin
      mesh_tri := mesh.Triangles[i];
      with mesh_tri.Vertices[0].Point^ do
          T_[0] := vec2(X, Y);
      with mesh_tri.Vertices[1].Point^ do
          T_[1] := vec2(X, Y);
      with mesh_tri.Vertices[2].Point^ do
          T_[2] := vec2(X, Y);

      AddTri(T_);
    end;

  DisposeObject(mesh);
  DisposeObject(Graph);
end;

procedure TTriangleList.BuildTriangle(polygon: T2DPolygonGraph);
var
  Graph: TGraph2D_;
  mesh: TDelaunayMesh2D_;
  i, j: TGeoInt;
  poly: T2DPolygon;
  v1, v2: TVec2;
  first_vert, Vert1, vert2: TVertex2D_;
  mesh_tri: TTriangle2D_;
  T_: TTriangle;
begin
  Clear;

  if polygon.Surround.Count < 3 then
      Exit;

  Graph := TGraph2D_.Create;
  mesh := TDelaunayMesh2D_.Create;

  v1 := polygon.Surround[0]^;
  Vert1 := TVertex2D_.CreateWithCoords(v1[0], v1[1]);
  first_vert := Vert1;
  Graph.Vertices.Add(Vert1);
  for i := 1 to polygon.Surround.Count - 1 do
    begin
      v2 := polygon.Surround[i]^;
      vert2 := TVertex2D_.CreateWithCoords(v2[0], v2[1]);
      Graph.Vertices.Add(vert2);
      Graph.Segments.Add(TSegment2D_.CreateWithVertices(Vert1, vert2));
      v1 := v2;
      Vert1 := vert2;
    end;
  Graph.Segments.Add(TSegment2D_.CreateWithVertices(vert2, first_vert));

  for j := 0 to polygon.CollapsesCount - 1 do
    begin
      poly := polygon.Collapses[j];

      v1 := poly[0]^;
      Vert1 := TVertex2D_.CreateWithCoords(v1[0], v1[1]);
      first_vert := Vert1;
      Graph.Vertices.Add(Vert1);
      for i := 1 to poly.Count - 1 do
        begin
          v2 := poly[i]^;
          vert2 := TVertex2D_.CreateWithCoords(v2[0], v2[1]);
          Graph.Vertices.Add(vert2);
          Graph.Segments.Add(TSegment2D_.CreateWithVertices(Vert1, vert2));
          v1 := v2;
          Vert1 := vert2;
        end;
      Graph.Segments.Add(TSegment2D_.CreateWithVertices(vert2, first_vert));
    end;

  mesh.AddGraph(Graph);

  // If the mesh has no segments, we will add a convex hull
  if mesh.Segments.Count = 0 then
      mesh.ConvexHull;

  mesh.Triangulate(TRemovalStyle_.rsNone);

  for i := 0 to mesh.Triangles.Count - 1 do
    begin
      mesh_tri := mesh.Triangles[i];
      with mesh_tri.Vertices[0].Point^ do
          T_[0] := vec2(X, Y);
      with mesh_tri.Vertices[1].Point^ do
          T_[1] := vec2(X, Y);
      with mesh_tri.Vertices[2].Point^ do
          T_[2] := vec2(X, Y);

      if polygon.InHere(TriCentre(T_)) then
          AddTri(T_);
    end;

  DisposeObject(mesh);
  DisposeObject(Graph);
end;

procedure TTriangleList.BuildTriangle(polygon: T2DPolygonGraph; MinAngle, MinSegmentLength, MaxElementSize: TGeoFloat);
var
  Graph: TGraph2D_;
  mesh: TQualityMesh2D_;
  i, j: TGeoInt;
  poly: T2DPolygon;
  v1, v2: TVec2;
  first_vert, Vert1, vert2: TVertex2D_;
  mesh_tri: TTriangle2D_;
  T_: TTriangle;
begin
  Clear;

  if polygon.Surround.Count < 3 then
      Exit;

  Graph := TGraph2D_.Create;
  mesh := TQualityMesh2D_.Create;

  v1 := polygon.Surround[0]^;
  Vert1 := TVertex2D_.CreateWithCoords(v1[0], v1[1]);
  first_vert := Vert1;
  Graph.Vertices.Add(Vert1);
  for i := 1 to polygon.Surround.Count - 1 do
    begin
      v2 := polygon.Surround[i]^;
      vert2 := TVertex2D_.CreateWithCoords(v2[0], v2[1]);
      Graph.Vertices.Add(vert2);
      Graph.Segments.Add(TSegment2D_.CreateWithVertices(Vert1, vert2));
      v1 := v2;
      Vert1 := vert2;
    end;
  Graph.Segments.Add(TSegment2D_.CreateWithVertices(vert2, first_vert));

  for j := 0 to polygon.CollapsesCount - 1 do
    begin
      poly := polygon.Collapses[j];

      v1 := poly[0]^;
      Vert1 := TVertex2D_.CreateWithCoords(v1[0], v1[1]);
      first_vert := Vert1;
      Graph.Vertices.Add(Vert1);
      for i := 1 to poly.Count - 1 do
        begin
          v2 := poly[i]^;
          vert2 := TVertex2D_.CreateWithCoords(v2[0], v2[1]);
          Graph.Vertices.Add(vert2);
          Graph.Segments.Add(TSegment2D_.CreateWithVertices(Vert1, vert2));
          v1 := v2;
          Vert1 := vert2;
        end;
      Graph.Segments.Add(TSegment2D_.CreateWithVertices(vert2, first_vert));
    end;

  mesh.AddGraph(Graph);

  // If the mesh has no segments, we will add a convex hull
  if mesh.Segments.Count = 0 then
      mesh.ConvexHull;

  mesh.MinimumAngle := MinAngle;
  mesh.MinimumSegmentLength := MinSegmentLength;
  mesh.MaximumElementSize := MaxElementSize;

  mesh.Triangulate(TRemovalStyle_.rsNone);

  for i := 0 to mesh.Triangles.Count - 1 do
    begin
      mesh_tri := mesh.Triangles[i];
      with mesh_tri.Vertices[0].Point^ do
          T_[0] := vec2(X, Y);
      with mesh_tri.Vertices[1].Point^ do
          T_[1] := vec2(X, Y);
      with mesh_tri.Vertices[2].Point^ do
          T_[2] := vec2(X, Y);

      if polygon.InHere(TriCentre(T_)) then
          AddTri(T_);
    end;

  DisposeObject(mesh);
  DisposeObject(Graph);
end;

function TRectPacking.Compute_XY_Pack(width, height: TGeoFloat; var X, Y: TGeoFloat): Boolean;
var
  i: TGeoInt;
  p: PRectPackData;
  R, b: TGeoFloat;
begin
  MaxWidth := MaxF(MaxWidth, width);
  MaxHeight := MaxF(MaxHeight, height);

  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if (width <= RectWidth(p^.Rect)) and (height <= RectHeight(p^.Rect)) then
        begin
          FList.Delete(i);
          X := p^.Rect[0, 0];
          Y := p^.Rect[0, 1];
          R := X + width;
          b := Y + height;
          MaxWidth := MaxF(MaxWidth, MaxF(width, R));
          MaxHeight := MaxF(MaxHeight, MaxF(height, b));
          Add(X, b, width, p^.Rect[1, 1] - b);
          Add(R, Y, p^.Rect[1, 0] - R, height);
          Add(R, b, p^.Rect[1, 0] - R, p^.Rect[1, 1] - b);
          Result := True;
          Dispose(p);
          Exit;
        end;
      inc(i);
    end;
  X := 0;
  Y := 0;
  Result := False;
end;

function TRectPacking.GetItems(const index: TGeoInt): PRectPackData;
begin
  Result := FList[index];
end;

constructor TRectPacking.Create;
begin
  inherited Create;
  FList := TRectPackData_List.Create;
  Style := TRectPacking_Style.rsDynamic;
  MaxWidth := 0;
  MaxHeight := 0;
  Margins := 2;
  UserToken := '';
end;

destructor TRectPacking.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited;
end;

procedure TRectPacking.Clear;
var
  i: TGeoInt;
begin
  for i := 0 to FList.Count - 1 do
      Dispose(FList[i]);
  FList.Clear;
end;

procedure TRectPacking.Add(Data1: pointer; Data2: TCore_Object; X, Y, width, height: TGeoFloat);
var
  p: PRectPackData;
begin
  new(p);
  p^.Rect := RectV2(X, Y, X + width, Y + height);
  p^.error := True;
  p^.Data1 := Data1;
  p^.Data2 := Data2;
  p^.ID := FList.Add(p);
end;

procedure TRectPacking.Add(const X, Y, width, height: TGeoFloat);
begin
  Add(nil, nil, X, Y, width, height);
end;

procedure TRectPacking.Add(Data1: pointer; Data2: TCore_Object; R: TRectV2);
begin
  Add(Data1, Data2, 0, 0, RectWidth(R), RectHeight(R));
end;

procedure TRectPacking.Add(Data1: pointer; Data2: TCore_Object; width, height: TGeoFloat);
begin
  Add(Data1, Data2, 0, 0, width, height);
end;

function TRectPacking.Data1Exists(const Data1: pointer): Boolean;
var
  i: TGeoInt;
begin
  Result := True;
  for i := 0 to FList.Count - 1 do
    if (FList[i]^.Data1 = Data1) then
        Exit;
  Result := False;
end;

function TRectPacking.Data2Exists(const Data2: TCore_Object): Boolean;
var
  i: TGeoInt;
begin
  Result := True;
  for i := 0 to FList.Count - 1 do
    if (FList[i]^.Data2 = Data2) then
        Exit;
  Result := False;
end;

function TRectPacking.Count: TGeoInt;
begin
  Result := FList.Count;
end;

procedure TRectPacking.Build;
begin
  case Style of
    rsDynamic: Build_Dynamic;
    rsL2R: Build_Left_To_Right(False);
    rsL2R_Sorted: Build_Left_To_Right(True);
    rsT2B: Build_Top_To_Bottom(False);
    rsT2B_Sorted: Build_Top_To_Bottom(True);
  end;
end;

procedure TRectPacking.Build_Dynamic(SpaceWidth, SpaceHeight: TGeoFloat);

  function Compare_(Left, Right: PRectPackData): ShortInt;
  begin
    Result := CompareFloat(RectArea(Right^.Rect), RectArea(Left^.Rect));
    if Result = 0 then
        Result := CompareGeoInt(Left^.ID, Right^.ID);
  end;

  procedure fastSort_(L_Buff: TRectPackData_List; L, R: TGeoInt);
  var
    i, j: TGeoInt;
    p: PRectPackData;
  begin
    if L < R then
      begin
        repeat
          if (R - L) = 1 then
            begin
              if Compare_(L_Buff[L], L_Buff[R]) > 0 then
                begin
                  L_Buff.Exchange(L, R);
                end;
              break;
            end;
          i := L;
          j := R;
          p := L_Buff[(L + R) shr 1];
          repeat
            while Compare_(L_Buff[i], p) < 0 do
                inc(i);
            while Compare_(L_Buff[j], p) > 0 do
                dec(j);
            if i <= j then
              begin
                if i <> j then
                  begin
                    L_Buff.Exchange(i, j);
                  end;
                inc(i);
                dec(j);
              end;
          until i > j;
          if (j - L) > (R - i) then
            begin
              if i < R then
                  fastSort_(L_Buff, i, R);
              R := j;
            end
          else
            begin
              if L < j then
                  fastSort_(L_Buff, L, j);
              L := i;
            end;
        until L >= R;
      end;
  end;

var
  L_Buff: TRectPackData_List;
  newLst: TRectPacking;
  p: PRectPackData;
  i: TGeoInt;
  X, Y, W, H: TGeoFloat;
begin
  L_Buff := TRectPackData_List.Create;
  for i := 0 to FList.Count - 1 do
      L_Buff.Add(FList[i]);
  if L_Buff.Count > 1 then
      fastSort_(L_Buff, 0, Count - 1);

  newLst := TRectPacking.Create;
  newLst.Add(Margins, Margins, SpaceWidth, SpaceHeight);
  for i := 0 to L_Buff.Count - 1 do
    begin
      p := L_Buff[i];

      X := p^.Rect[0, 0];
      Y := p^.Rect[0, 1];

      W := RectWidth(p^.Rect);
      H := RectHeight(p^.Rect);

      p^.error := not newLst.Compute_XY_Pack(W + Margins * 2, H + Margins * 2, X, Y);

      if not p^.error then
          p^.Rect := RectV2(X, Y, X + W, Y + H);
    end;
  DisposeObject(L_Buff);

  MaxWidth := newLst.MaxWidth + Margins;
  MaxHeight := newLst.MaxHeight + Margins;

  DisposeObject(newLst);
end;

procedure TRectPacking.Build_Dynamic;
var
  i: TGeoInt;
  p: PRectPackData;
  W, H: TGeoFloat;
begin
  W := Margins;
  H := Margins;
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      W := W + RectWidth(p^.Rect) + Margins * 2 + 1;
      H := H + RectHeight(p^.Rect) + Margins * 2 + 1;
    end;
  Build_Dynamic(W, H);
end;

procedure TRectPacking.Build_Left_To_Right(resort_width_: Boolean);
  function Compare_(Left, Right: PRectPackData): ShortInt;
  begin
    Result := CompareFloat(RectWidth(Right^.Rect), RectWidth(Left^.Rect));
    if Result = 0 then
        Result := CompareGeoInt(Left^.ID, Right^.ID);
  end;

  procedure fastSort_(L_Buff: TRectPackData_List; L, R: TGeoInt);
  var
    i, j: TGeoInt;
    p: PRectPackData;
  begin
    if L < R then
      begin
        repeat
          if (R - L) = 1 then
            begin
              if Compare_(L_Buff[L], L_Buff[R]) > 0 then
                begin
                  L_Buff.Exchange(L, R);
                end;
              break;
            end;
          i := L;
          j := R;
          p := L_Buff[(L + R) shr 1];
          repeat
            while Compare_(L_Buff[i], p) < 0 do
                inc(i);
            while Compare_(L_Buff[j], p) > 0 do
                dec(j);
            if i <= j then
              begin
                if i <> j then
                  begin
                    L_Buff.Exchange(i, j);
                  end;
                inc(i);
                dec(j);
              end;
          until i > j;
          if (j - L) > (R - i) then
            begin
              if i < R then
                  fastSort_(L_Buff, i, R);
              R := j;
            end
          else
            begin
              if L < j then
                  fastSort_(L_Buff, L, j);
              L := i;
            end;
        until L >= R;
      end;
  end;

var
  L_Buff: TRectPackData_List;
  p: PRectPackData;
  i: TGeoInt;
  X, Y, W, H: TGeoFloat;
begin
  L_Buff := TRectPackData_List.Create;
  for i := 0 to FList.Count - 1 do
      L_Buff.Add(FList[i]);

  if resort_width_ and (L_Buff.Count > 1) then
      fastSort_(L_Buff, 0, Count - 1);

  MaxWidth := 0;
  MaxHeight := 0;
  for i := 0 to L_Buff.Count - 1 do
    begin
      p := L_Buff[i];

      X := Margins + MaxWidth;
      Y := 0;

      W := RectWidth(p^.Rect);
      H := RectHeight(p^.Rect);

      p^.Rect := RectV2(X, Y, X + W, Y + H);

      MaxWidth := MaxWidth + W + Margins * 2 + 1;
      MaxHeight := umlMax(MaxHeight, H);
    end;
  DisposeObject(L_Buff);
end;

procedure TRectPacking.Build_Left_To_Right;
begin
  Build_Left_To_Right(True);
end;

procedure TRectPacking.Build_Top_To_Bottom(resort_height_: Boolean);
  function Compare_(Left, Right: PRectPackData): ShortInt;
  begin
    Result := CompareFloat(RectHeight(Right^.Rect), RectHeight(Left^.Rect));
    if Result = 0 then
        Result := CompareGeoInt(Left^.ID, Right^.ID);
  end;

  procedure fastSort_(L_Buff: TRectPackData_List; L, R: TGeoInt);
  var
    i, j: TGeoInt;
    p: PRectPackData;
  begin
    if L < R then
      begin
        repeat
          if (R - L) = 1 then
            begin
              if Compare_(L_Buff[L], L_Buff[R]) > 0 then
                begin
                  L_Buff.Exchange(L, R);
                end;
              break;
            end;
          i := L;
          j := R;
          p := L_Buff[(L + R) shr 1];
          repeat
            while Compare_(L_Buff[i], p) < 0 do
                inc(i);
            while Compare_(L_Buff[j], p) > 0 do
                dec(j);
            if i <= j then
              begin
                if i <> j then
                  begin
                    L_Buff.Exchange(i, j);
                  end;
                inc(i);
                dec(j);
              end;
          until i > j;
          if (j - L) > (R - i) then
            begin
              if i < R then
                  fastSort_(L_Buff, i, R);
              R := j;
            end
          else
            begin
              if L < j then
                  fastSort_(L_Buff, L, j);
              L := i;
            end;
        until L >= R;
      end;
  end;

var
  L_Buff: TRectPackData_List;
  p: PRectPackData;
  i: TGeoInt;
  X, Y, W, H: TGeoFloat;
begin
  L_Buff := TRectPackData_List.Create;
  for i := 0 to FList.Count - 1 do
      L_Buff.Add(FList[i]);
  if resort_height_ and (L_Buff.Count > 1) then
      fastSort_(L_Buff, 0, Count - 1);

  MaxWidth := 0;
  MaxHeight := 0;
  for i := 0 to L_Buff.Count - 1 do
    begin
      p := L_Buff[i];

      X := 0;
      Y := Margins + MaxHeight;

      W := RectWidth(p^.Rect);
      H := RectHeight(p^.Rect);

      p^.Rect := RectV2(X, Y, X + W, Y + H);

      MaxWidth := umlMax(MaxWidth, W);
      MaxHeight := MaxHeight + H + Margins * 2 + 1;
    end;
  DisposeObject(L_Buff);
end;

procedure TRectPacking.Build_Top_To_Bottom;
begin
  Build_Top_To_Bottom(True);
end;

function TRectPacking.GetBoundsBox(): TRectV2;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULLRect);
  Result := Items[0]^.Rect;
  for i := 1 to Count - 1 do
      Result := BoundRect(Result, Items[i]^.Rect);
end;

function TNearest_Box_IoU_Tool.Check_IoU(p1, p2: PNearest_Box_Data): Boolean;
var
  Intersect_Box: TRectV2;
  IoU, R1A, R2A, RA: TGeoFloat;
begin
  Result := False;
  if p1 = p2 then
      Exit;
  if not Compute_IoU(p1^.R^, p2^.R^, Intersect_Box, IoU, R1A, R2A, RA) then
      Exit;
  Result := True;
  // found pair
  if Num > 0 then
    with repeat_ do
      repeat
        if ((Queue^.Data.p1 = p1) or (Queue^.Data.p1 = p2)) and ((Queue^.Data.p2 = p1) or (Queue^.Data.p2 = p2)) then
            Exit;
      until not Next;
  with Add_Null^ do
    begin
      Data.p1 := p1;
      Data.p2 := p2;
      Data.Intersect_Box := Intersect_Box;
      Data.IoU := IoU;
      Data.R1A := R1A;
      Data.R2A := R2A;
      Data.RA := RA;
    end;
end;

procedure TNearest_Box_Data.Init;
begin
  R := nil;
  Free_R := False;
  ID := -1;
  UserData := nil;
  UserObject := nil;
  Nearest_Box := nil;
end;

procedure TNearest_Box_List.Update_Convex_Hull(Extract_Distance_: TGeoFloat);
begin
  Convex_Hull.Clear;
  if Num > 0 then
    with repeat_ do
      repeat
        Convex_Hull.AddRectangle(RectEdge(Queue^.Data^.R^, Extract_Distance_));
        Queue^.Data^.Nearest_Box := Self;
      until not Next;
  Convex_Hull.ConvexHull;
end;

constructor TNearest_Box_List.Create(ID_: Integer);
begin
  inherited Create;
  ID := ID_;
  Convex_Hull := TV2L.Create;
end;

destructor TNearest_Box_List.Destroy;
begin
  DisposeObject(Convex_Hull);
  inherited Destroy;
end;

procedure TNearest_Box_Group.DoFree(var Key: Integer; var Value: TNearest_Box_List);
begin
  Key := -1;
  DisposeObjectAndNil(Value);
end;

function TNearest_Box_Tool.Do_Sort_Group(var L, R: TNearest_Box_Group_.PPair_Pool_Value__): Integer;
begin
  Result := CompareInteger(L^.Data.Second.Num, R^.Data.Second.Num);
  if Result = 0 then
      Result := CompareInteger(L^.Data.Primary, R^.Data.Primary);
end;

constructor TNearest_Box_Tool.Create;
begin
  inherited Create;
  Nearest_Group := TNearest_Box_Group.Create($FF, nil);
  IoU_Tool := TNearest_Box_IoU_Tool.Create;
end;

destructor TNearest_Box_Tool.Destroy;
begin
  DisposeObject(Nearest_Group);
  DisposeObject(IoU_Tool);
  inherited Destroy;
end;

procedure TNearest_Box_Tool.DoFree(var Data: TNearest_Box_Data);
begin
  if Data.Free_R then
      Dispose(Data.R);
  Data.R := nil;
  Data.Init();
end;

function TNearest_Box_Tool.Add_Box(R_: PRectV2): PNearest_Box_Data;
begin
  with Add_Null^ do
    begin
      Data.Init();
      Data.R := R_;
      Data.ID := -1;
      Result := @Data;
    end;
end;

function TNearest_Box_Tool.Add_Box(R_: PRectV2; UserData: pointer; UserObject: TCore_Object): PNearest_Box_Data;
begin
  with Add_Null^ do
    begin
      Data.Init();
      Data.R := R_;
      Data.ID := -1;
      Data.UserData := UserData;
      Data.UserObject := UserObject;
      Result := @Data;
    end;
end;

function TNearest_Box_Tool.Add_Box(R_: TRectV2; UserData: pointer; UserObject: TCore_Object): PNearest_Box_Data;
begin
  with Add_Null^ do
    begin
      Data.Init();
      new(Data.R);
      Data.R^ := R_;
      Data.Free_R := True;
      Data.ID := -1;
      Data.UserData := UserData;
      Data.UserObject := UserObject;
      Result := @Data;
    end;
end;

function TNearest_Box_Tool.Add_Box(R_: TRect; UserData: pointer; UserObject: TCore_Object): PNearest_Box_Data;
begin
  with Add_Null^ do
    begin
      Data.Init();
      new(Data.R);
      Data.R^ := RectV2(R_);
      Data.Free_R := True;
      Data.ID := -1;
      Data.UserData := UserData;
      Data.UserObject := UserObject;
      Result := @Data;
    end;
end;

function TNearest_Box_Tool.Get_Box_Group(R_: PRectV2): TNearest_Box_List;
begin
  Result := nil;
  if Num <= 0 then
      Exit;
  with repeat_ do
    repeat
      if Queue^.Data.R = R_ then
          Exit(Nearest_Group[Queue^.Data.ID]);
    until not Next;
end;

function TNearest_Box_Tool.Get_UserData_Group(UserData: pointer): TNearest_Box_List;
begin
  Result := nil;
  if Num <= 0 then
      Exit;
  with repeat_ do
    repeat
      if Queue^.Data.UserData = UserData then
          Exit(Nearest_Group[Queue^.Data.ID]);
    until not Next;
end;

function TNearest_Box_Tool.Get_UserObject_Group(UserObject: TCore_Object): TNearest_Box_List;
begin
  Result := nil;
  if Num <= 0 then
      Exit;
  with repeat_ do
    repeat
      if Queue^.Data.UserObject = UserObject then
          Exit(Nearest_Group[Queue^.Data.ID]);
    until not Next;
end;

function TNearest_Box_Tool.Compute_Nearest_Box(Nearest_Distance_, Convex_Hull_Distance_: TGeoFloat): Integer;

  procedure Search_Overlap(p: TNearest_Box_Tool_.PQueueStruct; overlap_id: Integer);
  var
    box: TRectV2;
  begin
    box := RectEdge(p^.Data.R^, Nearest_Distance_);
    with repeat_ do
      repeat
        IoU_Tool.Check_IoU(@Queue^.Data, @p^.Data);
        if (Queue <> p) and (Queue^.Data.ID < 0) and Rect_Overlap_or_Intersect(box, RectEdge(Queue^.Data.R^, Nearest_Distance_)) then
          begin
            Queue^.Data.ID := overlap_id;
            Search_Overlap(Queue, overlap_id);
          end;
      until not Next;
  end;

var
  overlap_id: Integer;
begin
  Nearest_Group.Clear;
  IoU_Tool.Clear;
  Result := 0;
  if Num <= 0 then
      Exit;

  Free_Recycle_Pool;
  with repeat_ do
    repeat
      if (Queue^.Data.R = nil) or LessThanOrEqual(RectArea(Queue^.Data.R^), 0) then
          Push_To_Recycle_Pool(Queue)
      else
          Queue^.Data.ID := -1;
    until not Next;
  Free_Recycle_Pool;

  if Num <= 0 then
      Exit;

  overlap_id := 0;
  with repeat_ do
    repeat
      if Queue^.Data.ID < 0 then
        begin
          Queue^.Data.ID := overlap_id;
          Search_Overlap(Queue, overlap_id);
          inc(overlap_id);
        end;
    until not Next;
  Result := overlap_id;

  with repeat_ do
    repeat
      if not Nearest_Group.Exists_Key(Queue^.Data.ID) then
          Nearest_Group.Add(Queue^.Data.ID, TNearest_Box_List.Create(Queue^.Data.ID), False);
      Nearest_Group[Queue^.Data.ID].Add(@Queue^.Data);
    until not Next;

  with Nearest_Group.repeat_ do
    repeat
        Queue^.Data^.Data.Second.Update_Convex_Hull(Convex_Hull_Distance_);
    until not Next;

  Nearest_Group.Queue_Pool.Sort_M(Do_Sort_Group);
  Nearest_Group.Extract_Queue_Pool_Third;
end;

procedure THausdorf.NewNode(var p: PNode);
begin
  new(p);
  NodeList.Add(p);
end;

procedure THausdorf.NewLink(var p: PLinkedList);
begin
  new(p);
  LinkList.Add(p);
end;

procedure THausdorf.WrapVector(var wrapper: PNode; const X, Y: TGeoFloat);
begin
  NewNode(wrapper);
  wrapper^.Prev := nil;
  wrapper^.Next := nil;
  wrapper^.Data[0] := X;
  wrapper^.Data[1] := Y;
end;

procedure THausdorf.InitList(var target: PLinkedList);
begin
  NewLink(target);
  target^.Head := nil;
  target^.Tail := nil;
  target^.Num := 0;
  target^.Looped := False;
end;

procedure THausdorf.InitAndReadPolygon(var target: PLinkedList; const source: TV2L);
var
  i: TGeoInt;
begin
  InitList(target);
  for i := 0 to source.Count - 1 do
      AddTo(target, source[i]^);
  LoopTheList(target);
end;

function THausdorf.Get(var target: PLinkedList; const n_: TGeoInt): PNode;
var
  curNode: PNode;
  n, i: TGeoInt;
begin
  if (n_ > target^.Num) and (not target^.Looped) then
      Result := nil
  else
    begin
      n := n_ mod target^.Num;
      curNode := target^.Head;
      for i := 0 to n - 1 do
          curNode := curNode^.Next;
      Result := curNode;
    end;
end;

procedure THausdorf.GetMax(var target, source: PLinkedList);
var
  i: TGeoInt;
  Max_: TGeoFloat;
  curNode: PNode;
begin
  curNode := source^.Head;
  Max_ := Vec2Length(curNode^.Data);
  curNode := curNode^.Next;

  for i := 2 to source^.Num do
    begin
      if (Vec2Length(curNode^.Data) > Max_) then
          Max_ := Vec2Length(curNode^.Data);
      curNode := curNode^.Next;
    end;

  curNode := source^.Head;

  for i := 1 to source^.Num do
    begin
      if abs(Vec2Length(curNode^.Data) - Max_) <= FRoundKOEF then
          AddTo(target, curNode^.Data);
      curNode := curNode^.Next;
    end;
end;

procedure THausdorf.GetMin(var target, source: PLinkedList);
var
  i: TGeoInt;
  Min_: TGeoFloat;
  curNode: PNode;
begin
  curNode := source^.Head;
  Min_ := Vec2Length(curNode^.Data);
  curNode := curNode^.Next;

  for i := 2 to source^.Num do
    begin
      if (Vec2Length(curNode^.Data) < Min_) then
          Min_ := Vec2Length(curNode^.Data);
      curNode := curNode^.Next;
    end;

  curNode := source^.Head;

  for i := 1 to source^.Num do
    begin
      if abs(Vec2Length(curNode^.Data) - Min_) <= FRoundKOEF then
          AddTo(target, curNode^.Data);
      curNode := curNode^.Next;
    end;
end;

procedure THausdorf.AddNodeTo(var target: PLinkedList; const item: PNode);
begin
  if target^.Tail = nil then
    begin
      target^.Head := item;
      target^.Tail := item;
      target^.Num := 1;
    end
  else
    begin
      target^.Tail^.Next := item;
      item^.Prev := target^.Tail;
      target^.Tail := item;
      target^.Num := target^.Num + 1;
    end;
end;

procedure THausdorf.AddTo(var target: PLinkedList; p: TVec2);
begin
  AddTo(target, p[0], p[1]);
end;

procedure THausdorf.AddTo(var target: PLinkedList; X, Y: TGeoFloat);
var
  curNode: PNode;
begin
  WrapVector(curNode, X, Y);
  AddNodeTo(target, curNode);
end;

procedure THausdorf.AddToQ(const target: PLinkedList; const p: TVec2);
var
  nd, current, pom: PNode;
begin
  { Wrap the node to the queue item container }
  NewNode(nd);
  nd^.Data := p;
  nd^.Next := nil;
  nd^.Prev := nil;

  if target^.Head = nil then
    begin
      target^.Head := nd;
      target^.Tail := nd;
      target^.Num := target^.Num + 1;
    end
  else { Walk along the queue until the place for the item is found. The item should be inserted before the 'current' item. We need to be careful in the situations when the 'current' item is the last one in the queue though. }
    begin
      current := target^.Head;
      while (Compare(nd^.Data, current^.Data) = -1) and (current^.Next <> nil) do
          current := current^.Next;

      if current = target^.Head then
        begin
          if Compare(nd^.Data, current^.Data) = 1 then
            begin
              nd^.Next := target^.Head;
              target^.Head^.Prev := nd;
              target^.Head := nd;
              target^.Num := target^.Num + 1;
            end
          else
            begin
              current^.Next := nd;
              nd^.Prev := current;
              target^.Num := target^.Num + 1;
            end;
        end
      else if Compare(nd^.Data, current^.Data) = 1 then
        begin
          nd^.Next := current;
          nd^.Prev := current^.Prev;
          current^.Prev := nd;

          pom := nd^.Prev;

          pom^.Next := nd;
          target^.Num := target^.Num + 1;
        end
      else
        begin
          current^.Next := nd;
          nd^.Prev := current;
          target^.Num := target^.Num + 1;
        end;
    end;
end;

function THausdorf.Compare(const p1, p2: TVec2): TGeoInt;
var
  nP1, nP2: TVec2;
begin
  if (Quadrant(p1) > Quadrant(p2)) then
    begin
      Result := -1;
      Exit
    end
  else if (Quadrant(p1) < Quadrant(p2)) then
    begin
      Result := 1;
      Exit
    end
  else
    begin
      nP1 := Normalise(p1);
      nP2 := Normalise(p2);

      if (Quadrant(nP1) = 1) or (Quadrant(nP1) = 2) then
        begin
          if (nP1[0] < nP2[0]) then
            begin
              Result := -1;
              Exit;
            end
          else if (nP1[0] < nP2[0]) then
            begin
              Result := 1;
              Exit;
            end
          else
            begin
              Result := 0;
              Exit;
            end;
        end
      else
        begin
          if (nP1[0] > nP2[0]) then
            begin
              Result := -1;
              Exit;
            end
          else if (nP1[0] > nP2[0]) then
            begin
              Result := 1;
              Exit;
            end
          else
            begin
              Result := 0;
              Exit;
            end;
        end;
    end;
end;

function THausdorf.Contains(const pol: PLinkedList; const p: TVec2): Boolean;
var
  ab, bc, ap_, bp: TVec2;
  pr1, pr2: TGeoFloat;
  i: TGeoInt;
  curNode: PNode;
begin
  if (pol^.Num < 2) then
    begin
      Result := False;
      Exit;
    end;

  curNode := pol^.Head;
  bc := Vec2Sub(curNode^.Next^.Data, curNode^.Data);
  bp := Vec2Sub(p, curNode^.Data);
  pr2 := PseudoScalarProduct(bp, bc);
  curNode := curNode^.Next;

  for i := 0 to pol^.Num - 1 do
    begin
      ab := bc;
      bc := Vec2Sub(curNode^.Next^.Data, curNode^.Data);
      ap_ := bp;
      bp := Vec2Sub(p, curNode^.Data);
      pr1 := pr2;
      pr2 := PseudoScalarProduct(bp, bc);

      curNode := curNode^.Next;

      if (abs(pr1) <= FRoundKOEF) and (abs(pr2) <= FRoundKOEF) then
        begin
          Result := True;
          Exit;
        end

      else if (abs(pr1) <= FRoundKOEF) or (abs(pr2) <= FRoundKOEF) then
          Continue;

      if (pr1 * pr2 < 0) then
        begin
          Result := False;
          Exit;
        end;
    end;
  Result := True;
end;

procedure THausdorf.DeleteCopies(var target, source: PLinkedList);
var
  i: TGeoInt;
  curNode: PNode;
begin
  curNode := source^.Head;
  for i := 0 to source^.Num - 1 do
    begin
      if (not IsInList(target, curNode^.Data)) then
          AddTo(target, curNode^.Data);
      curNode := curNode^.Next;
    end;
end;

procedure THausdorf.HausdorfDistanceVectors(var target, Polygon1_, Polygon2_: PLinkedList);
var
  pom1, pom2: PLinkedList;
  i: TGeoInt;
  curNode: PNode;
begin
  InitList(pom1);
  PolygonPolygonDistanceVectors(pom1, Polygon1_, Polygon2_);
  InitList(pom2);
  GetMax(pom2, pom1);
  InitList(pom1);
  PolygonPolygonDistanceVectors(pom1, Polygon2_, Polygon1_);

  curNode := pom1^.Head;
  for i := 1 to pom1^.Num do
    begin
      curNode^.Data := Vec2Negate(curNode^.Data);
      curNode := curNode^.Next;
    end;
  GetMax(pom2, pom1);
  InitList(pom1);
  GetMax(pom1, pom2);
  DeleteCopies(target, pom1);
end;

function THausdorf.IsInList(const target: PLinkedList; const p: TVec2): Boolean;
var
  i: TGeoInt;
  curNode: PNode;
begin
  if (target^.Head = nil) then
    begin
      Result := False;
      Exit;
    end;

  curNode := target^.Head;
  for i := 0 to target^.Num - 1 do
    begin
      if (abs(curNode^.Data[0] - p[0]) <= FRoundKOEF) and (abs(curNode^.Data[1] - p[1]) <= FRoundKOEF) then
        begin
          Result := True;
          Exit;
        end;
      curNode := curNode^.Next;
    end;

  Result := False;
end;

function THausdorf.IsOptimal(var distVecs: PLinkedList): Boolean;
var
  pom: PLinkedList;
  Zero: TVec2;
begin
  if distVecs^.Num = 1 then
      Result := Vec2Length(distVecs^.Head^.Data) <= FRoundKOEF
  else if distVecs^.Num = 2 then
      Result := (PseudoScalarProduct(distVecs^.Head^.Data, distVecs^.Head^.Next^.Data) <= 0) and
      (abs(PseudoScalarProduct(Normalise(distVecs^.Head^.Data), Normalise(distVecs^.Head^.Next^.Data))) <= FRoundKOEF)
  else
    begin
      InitList(pom);
      SortByAngle(pom, distVecs);
      Zero[0] := 0;
      Zero[1] := 0;
      Result := contains(pom, Zero);
    end;
end;

procedure THausdorf.LoopTheList(var target: PLinkedList);
begin
  target^.Tail^.Next := target^.Head;
  target^.Head^.Prev := target^.Tail;
  target^.Looped := True;
end;

procedure THausdorf.PointPolygonDistanceVectors(var target, pol: PLinkedList; const p: TVec2);
var
  i: TGeoInt;
  curNode: PNode;
begin
  if (contains(pol, p)) then
      AddTo(target, 0, 0)
  else
    begin
      curNode := pol^.Head;
      for i := 1 to pol^.Num do
        begin
          AddTo(target, PointSectionDistanceVector(curNode^.Data, curNode^.Next^.Data, p));
          curNode := curNode^.Next;
        end;
    end;
end;

function THausdorf.PointSectionDistanceVector(const a, b, p: TVec2): TVec2;
var
  ab, ap_, bp: TVec2;
  falls1, falls2: Boolean;
  t: TGeoFloat;
  tAB: TVec2;
begin
  ab := Vec2Sub(b, a);
  ap_ := Vec2Sub(p, a);
  bp := Vec2Sub(p, b);

  falls1 := ScalarProduct(ab, ap_) > 0;
  falls2 := ScalarProduct(Vec2Negate(ab), bp) > 0;

  if (not falls1) then
    begin
      Result := Vec2Negate(ap_);
      Exit;
    end;

  if (not falls2) then
    begin
      Result := Vec2Negate(bp);
      Exit;
    end;

  t := ScalarProduct(ab, ap_) / ScalarProduct(ab, ab);
  tAB := Vec2Mul(ab, t);
  tAB := Vec2Add(a, tAB);
  Result := Vec2Sub(tAB, p);
end;

procedure THausdorf.PolygonPolygonDistanceVectors(var target, Polygon1_, Polygon2_: PLinkedList);
var
  i: TGeoInt;
  curNode: PNode;
  pom1: PLinkedList;
begin
  curNode := Polygon1_^.Head;

  for i := 0 to Polygon1_^.Num - 1 do
    begin
      InitList(pom1);
      PointPolygonDistanceVectors(pom1, Polygon2_, curNode^.Data);
      GetMin(target, pom1);
      curNode := curNode^.Next;
    end;
end;

function THausdorf.PseudoScalarProduct(const a, b: TVec2): TGeoFloat;
begin
  PseudoScalarProduct := (a[0] * b[1]) - (a[1] * b[0]);
end;

function THausdorf.Quadrant(const p: TVec2): TGeoInt;
begin
  Quadrant := 1;
  if (p[0] > 0) and (p[1] >= 0) then
      Quadrant := 1
  else if (p[0] <= 0) and (p[1] > 0) then
      Quadrant := 2
  else if (p[0] < 0) and (p[1] <= 0) then
      Quadrant := 3
  else if (p[0] >= 0) and (p[1] < 0) then
      Quadrant := 4;
end;

procedure THausdorf.SortByAngle(var target, source: PLinkedList);
var
  i: TGeoInt;
  curNode: PNode;
begin
  curNode := source^.Head;

  for i := 1 to source^.Num do
    begin
      AddToQ(target, curNode^.Data);
      curNode := curNode^.Next;
    end;

  target^.Tail := Get(target, target^.Num - 1);
  LoopTheList(target);
end;

function THausdorf.Normalise(const vec: TVec2): TVec2;
var
  len: TGeoFloat;
begin
  len := Vec2Length(vec);
  Result[0] := vec[0] / len;
  Result[1] := vec[1] / len;
end;

function THausdorf.ScalarProduct(const a, b: TVec2): TGeoFloat;
begin
  ScalarProduct := (a[0] * b[0]) + (a[1] * b[1]);
end;

class function THausdorf.Compute(const poly1_, poly2_: TV2L; const detail_: TGeoInt; const ROUND_KOEF: TGeoFloat): TGeoFloat;
begin
  with THausdorf.Create(poly1_, poly2_, detail_, ROUND_KOEF) do
    begin
      Result := HausdorffDistance();
      Free;
    end;
end;

class function THausdorf.Compute(
  const poly1_: TV2L; const poly1_b, poly1_e: Integer;
  const poly2_: TV2L; const poly2_b, poly2_e: Integer;
  const detail_: TGeoInt; const ROUND_KOEF: TGeoFloat): TGeoFloat;
var
  i: Integer;
  nP1, nP2: TV2L;
begin
  nP1 := TV2L.Create;
  for i := poly1_b to poly1_e do
      nP1.Add(poly1_[i]^);

  nP2 := TV2L.Create;
  for i := poly2_b to poly2_e do
      nP2.Add(poly2_[i]^);

  with THausdorf.Create(nP1, nP2, detail_, ROUND_KOEF) do
    begin
      Result := HausdorffDistance();
      Free;
    end;
  DisposeObject(nP1);
  DisposeObject(nP2);
end;

constructor THausdorf.Create(const poly1_, poly2_: TV2L; const detail_: TGeoInt; const ROUND_KOEF: TGeoFloat);
var
  np: TV2L;
begin
  inherited Create;
  FPolygon1 := nil;
  FPolygon2 := nil;
  FOutput := nil;
  FRoundKOEF := if_(ROUND_KOEF <= 0, 0.0001, ROUND_KOEF);
  NodeList := TNodeList.Create;
  LinkList := TLinkList.Create;

  if detail_ > 0 then
    begin
      np := TV2L.Create;
      poly1_.InterpolationTo(detail_, np);
      InitAndReadPolygon(FPolygon1, np);
      DisposeObject(np);

      np := TV2L.Create;
      poly2_.InterpolationTo(detail_, np);
      InitAndReadPolygon(FPolygon2, np);
      DisposeObject(np);
    end
  else
    begin
      InitAndReadPolygon(FPolygon1, poly1_);
      InitAndReadPolygon(FPolygon2, poly2_);
    end;

  InitList(FOutput);
  HausdorfDistanceVectors(FOutput, FPolygon1, FPolygon2);
end;

destructor THausdorf.Destroy;
var
  i: TGeoInt;
begin
  for i := 0 to NodeList.Count - 1 do
      Dispose(NodeList[i]);
  DisposeObject(NodeList);

  for i := 0 to LinkList.Count - 1 do
      Dispose(LinkList[i]);
  DisposeObject(LinkList);

  inherited Destroy;
end;

function THausdorf.HausdorffReached: TArrayVec2;
var
  i: TGeoInt;
  curNode: PNode;
begin
  SetLength(Result, FOutput^.Num);
  curNode := FOutput^.Head;
  for i := 0 to FOutput^.Num - 1 do
    begin
      Result[i] := curNode^.Data;
      curNode := curNode^.Next;
    end;
end;

function THausdorf.HausdorffDistance: TGeoFloat;
begin
  Result := Vec2Length(FOutput^.Head^.Data);
end;

function THausdorf.polygonsIsOptimal: Boolean;
begin
  Result := IsOptimal(FOutput);
end;

class procedure THausdorf.TestAndPrint(const poly1_, poly2_: TV2L);
var
  buff: TArrayVec2;
  v2: TVec2;
begin
  with THausdorf.Create(poly1_, poly2_, 0, 0.0001) do
    begin
      DoStatus('The distance is reached on the following vectors:');
      buff := HausdorffReached();
      for v2 in buff do
          DoStatus('%f %f', [v2[0], v2[1]]);
      SetLength(buff, 0);

      DoStatus('Hausdorff distance: %f', [HausdorffDistance]);
      DoStatus('The mutual position of the polygons is optimal: %s', [umlBoolToStr(polygonsIsOptimal).Text]);
      Free;
    end;
end;

class procedure THausdorf.Test1;
var
  vl1, vl2: TV2L;
begin
  vl1 := TV2L.Create;
  vl1.Add(2, 1);
  vl1.Add(-2, 1);
  vl1.Add(-2, -1);
  vl1.Add(2, -1);

  vl2 := TV2L.Create;
  vl2.Add(1, 2);
  vl2.Add(-1, 2);
  vl2.Add(-1, -2);
  vl2.Add(1, -2);

  TestAndPrint(vl1, vl2);

  DisposeObject(vl1);
  DisposeObject(vl2);
end;

class procedure THausdorf.Test2;
var
  vl1, vl2: TV2L;
begin
  vl1 := TV2L.Create;
  vl1.Add(0, 200);
  vl1.Add(86.6025403, 50);
  vl1.Add(173.2050807, 200);

  vl2 := TV2L.Create;
  vl2.Add(213.3974597, 550.0);
  vl2.Add(386.6025403, 550.0);
  vl2.Add(300.0, 700.0);

  TestAndPrint(vl1, vl2);

  DisposeObject(vl1);
  DisposeObject(vl2);
end;

function ArrayVec2(const V: TArrayVec2): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, length(V));
  for i := 0 to length(V) - 1 do
      Result[i] := V[i];
end;

function ArrayVec2(const R: TRectV2): TArrayVec2;
begin
  SetLength(Result, 4);
  Result[0] := PointMake(R[0, 0], R[0, 1]);
  Result[1] := PointMake(R[1, 0], R[0, 1]);
  Result[2] := PointMake(R[1, 0], R[1, 1]);
  Result[3] := PointMake(R[0, 0], R[1, 1]);
end;

function ArrayVec2(const R: TV2Rect4): TArrayVec2;
begin
  SetLength(Result, 4);
  Result[0] := R.LeftTop;
  Result[1] := R.RightTop;
  Result[2] := R.RightBottom;
  Result[3] := R.LeftBottom;
end;

function ArrayVec2(const L: TLineV2): TArrayVec2;
begin
  SetLength(Result, 2);
  Result[0] := L[0];
  Result[1] := L[1];
end;

function ArrayVec2(const t: TTriangle): TArrayVec2;
begin
  SetLength(Result, 3);
  Result[0] := t[0];
  Result[1] := t[1];
  Result[2] := t[2];
end;

function ArrayBoundRect(arry: TArrayVec2): TRectV2;
var
  i: Integer;
begin
  if length(arry) > 0 then
    begin
      Result[0] := arry[0];
      Result[1] := arry[0];
      for i := 1 to High(arry) do
          Result := BoundRect(Result, arry[i]);
    end
  else
      Result := NULLRect;
end;

function ArrayBoundRect(arry: TArrayRectV2): TRectV2;
var
  i: Integer;
  Inited: Boolean;
begin
  if length(arry) > 0 then
    begin
      Inited := False;
      for i := 0 to High(arry) do
        if RectArea(arry[i]) > 0 then
          begin
            if Inited then
                Result := BoundRect(Result, arry[i])
            else
              begin
                Inited := True;
                Result := arry[i];
              end;
          end;
    end
  else
      Result := NULLRect;
end;

function ArrayBoundRect(arry: TArrayV2Rect4): TRectV2;
var
  i: Integer;
  Inited: Boolean;
begin
  if length(arry) > 0 then
    begin
      Inited := False;
      for i := 0 to High(arry) do
        if arry[i].Area > 0 then
          begin
            if Inited then
                Result := BoundRect(Result, arry[i].BoundRect)
            else
              begin
                Inited := True;
                Result := arry[i].BoundRect;
              end;
          end;
    end
  else
      Result := NULLRect;
end;

end.
 
