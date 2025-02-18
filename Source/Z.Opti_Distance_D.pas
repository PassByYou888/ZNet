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
{ * double float distance sse optmized                                         * }
{ ****************************************************************************** }
unit Z.Opti_Distance_D;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

type
  PDouble = ^Double;

{$IFDEF SSE_Optimize_Distance_Compute}
function SSE_Distance_D(n: Integer; sour, dest: PDouble): Double;
{$ENDIF SSE_Optimize_Distance_Compute}
function Pascal_Distance_D(n: Integer; sour, dest: PDouble): Double;
function Do_Test_SIMD_Distance_D: Boolean;

implementation

{$IFDEF SSE_Optimize_Distance_Compute}


type
  // sse alignment
  TDouble_2X = array [0 .. 1] of Double;

function SSE_SQR_2(sour, dest: PDouble): TDouble_2X;
asm
  movupd xmm0,[[dest]]
  movupd xmm1,[[sour]]
  subpd  xmm0,xmm1
  mulpd xmm0,xmm0
  movupd [Result],xmm0
end;

function SSE_SQR_4(sour, dest: PDouble): TDouble_2X;
asm
  movupd xmm0,[[dest]+0*2*8]
  movupd xmm1,[[sour]+0*2*8]
  subpd  xmm0,xmm1
  mulpd xmm0,xmm0
  movupd xmm2,xmm0

  movupd xmm0,[[dest]+1*2*8]
  movupd xmm1,[[sour]+1*2*8]
  subpd  xmm0,xmm1
  mulpd xmm0,xmm0
  addpd xmm2,xmm0

  movupd [Result],xmm2
end;

function SSE_SQR_16(sour, dest: PDouble): TDouble_2X;
asm
  movupd xmm0,[[dest]+0*2*8]
  movupd xmm1,[[sour]+0*2*8]
  subpd  xmm0,xmm1
  mulpd xmm0,xmm0
  movupd xmm2,xmm0

  movupd xmm0,[[dest]+1*2*8]
  movupd xmm1,[[sour]+1*2*8]
  subpd  xmm0,xmm1
  mulpd xmm0,xmm0
  addpd xmm2,xmm0

  movupd xmm0,[[dest]+2*2*8]
  movupd xmm1,[[sour]+2*2*8]
  subpd  xmm0,xmm1
  mulpd xmm0,xmm0
  addpd xmm2,xmm0

  movupd xmm0,[[dest]+3*2*8]
  movupd xmm1,[[sour]+3*2*8]
  subpd  xmm0,xmm1
  mulpd xmm0,xmm0
  addpd xmm2,xmm0

  movupd xmm0,[[dest]+4*2*8]
  movupd xmm1,[[sour]+4*2*8]
  subpd  xmm0,xmm1
  mulpd xmm0,xmm0
  addpd xmm2,xmm0

  movupd xmm0,[[dest]+5*2*8]
  movupd xmm1,[[sour]+5*2*8]
  subpd  xmm0,xmm1
  mulpd xmm0,xmm0
  addpd xmm2,xmm0

  movupd xmm0,[[dest]+6*2*8]
  movupd xmm1,[[sour]+6*2*8]
  subpd  xmm0,xmm1
  mulpd xmm0,xmm0
  addpd xmm2,xmm0

  movupd xmm0,[[dest]+7*2*8]
  movupd xmm1,[[sour]+7*2*8]
  subpd  xmm0,xmm1
  mulpd xmm0,xmm0
  addpd xmm2,xmm0

  movupd [Result],xmm2
end;

function SSE_Distance_D(n: Integer; sour, dest: PDouble): Double;
var
  tmp: TDouble_2X;
begin
  Result := 0;
  while n >= 16 do
    begin
      tmp := SSE_SQR_16(sour, dest);
      inc(sour, 16);
      inc(dest, 16);
      dec(n, 16);
      Result := Result + tmp[0] + tmp[1];
    end;
  while n >= 4 do
    begin
      tmp := SSE_SQR_4(sour, dest);
      inc(sour, 4);
      inc(dest, 4);
      dec(n, 4);
      Result := Result + tmp[0] + tmp[1];
    end;
  while n >= 2 do
    begin
      tmp := SSE_SQR_2(sour, dest);
      inc(sour, 2);
      inc(dest, 2);
      dec(n, 2);
      Result := Result + tmp[0] + tmp[1];
    end;
  while n > 0 do
    begin
      tmp[0] := sour^ - dest^;
      tmp[1] := tmp[0] * tmp[0];
      inc(sour);
      inc(dest);
      dec(n);
      Result := Result + tmp[1];
    end;
end;
{$ENDIF SSE_Optimize_Distance_Compute}


function Pascal_Distance_D(n: Integer; sour, dest: PDouble): Double;
var
  i: Integer;
  f: Double;
begin
  Result := 0;
  while n > 0 do
    begin
      f := dest^ - sour^;
      inc(sour);
      inc(dest);
      dec(n);
      Result := Result + f * f;
    end;
end;

function Do_Test_SIMD_Distance_D: Boolean;
const
  L_ = 2051;
type
  T_ = array [0 .. L_] of Double;
var
  i: Integer;
  c1_, c2_: T_;
  r1, r2: Double;
begin
  for i := 0 to L_ - 1 do
    begin
      c1_[i] := Random(1000) * 0.001;
      c2_[i] := Random(1000) * 0.001;
    end;
  r1 := Pascal_Distance_D(L_, @c1_, @c2_);
{$IFDEF SSE_Optimize_Distance_Compute}
  r2 := SSE_Distance_D(L_, @c1_, @c2_);
{$ELSE SSE_Optimize_Distance_Compute}
  r2 := r1;
{$ENDIF SSE_Optimize_Distance_Compute}
  Result := abs(r2 - r1) < 0.0001;
end;

end.
 
