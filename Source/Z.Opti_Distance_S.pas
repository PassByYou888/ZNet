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
{ * single float distance sse optmized                                         * }
{ ****************************************************************************** }
unit Z.Opti_Distance_S;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

type
  PSingle = ^Single;

{$IFDEF SSE_Optimize_Distance_Compute}
function SSE_Distance_S(n: Integer; sour, dest: PSingle): Single;
{$ENDIF SSE_Optimize_Distance_Compute}
function Pascal_Distance_S(n: Integer; sour, dest: PSingle): Single;
function Do_Test_SIMD_Distance_S: Boolean;

implementation

{$IFDEF SSE_Optimize_Distance_Compute}


type
  // sse alignment
  TSingle_4X = array [0 .. 3] of Single;

function SSE_SQR_4(sour, dest: PSingle): TSingle_4X;
asm
  movups xmm0,[[dest]]
  movups xmm1,[[sour]]
  subps  xmm0,xmm1
  mulps xmm0,xmm0
  movups [Result],xmm0
end;

function SSE_SQR_2(sour__, dest__: PSingle): TSingle_4X;
var
  sour, dest: TSingle_4X;
begin
  sour[0] := sour__^;
  inc(sour__);
  sour[1] := sour__^;
  sour[2] := 0;
  sour[3] := 0;

  dest[0] := dest__^;
  inc(dest__);
  dest[1] := dest__^;
  dest[2] := 0;
  dest[3] := 0;

  Result := SSE_SQR_4(@sour[0], @dest[0]);
end;

function SSE_SQR_3(sour__, dest__: PSingle): TSingle_4X;
var
  sour, dest: TSingle_4X;
begin
  sour[0] := sour__^;
  inc(sour__);
  sour[1] := sour__^;
  inc(sour__);
  sour[2] := sour__^;
  sour[3] := 0;

  dest[0] := dest__^;
  inc(dest__);
  dest[1] := dest__^;
  inc(dest__);
  dest[2] := dest__^;
  dest[3] := 0;

  Result := SSE_SQR_4(@sour[0], @dest[0]);
end;

function SSE_SQR_16(sour, dest: PSingle): TSingle_4X;
asm
  movups xmm0,[[dest]+0*2*8]
  movups xmm1,[[sour]+0*2*8]
  subps  xmm0,xmm1
  mulps xmm0,xmm0
  movups xmm2,xmm0

  movups xmm0,[[dest]+1*2*8]
  movups xmm1,[[sour]+1*2*8]
  subps  xmm0,xmm1
  mulps xmm0,xmm0
  addps xmm2,xmm0

  movups xmm0,[[dest]+2*2*8]
  movups xmm1,[[sour]+2*2*8]
  subps  xmm0,xmm1
  mulps xmm0,xmm0
  addps xmm2,xmm0

  movups xmm0,[[dest]+3*2*8]
  movups xmm1,[[sour]+3*2*8]
  subps  xmm0,xmm1
  mulps xmm0,xmm0
  addps xmm2,xmm0

  movups [Result],xmm2
end;

function SSE_SQR_32(sour, dest: PSingle): TSingle_4X;
asm
  movups xmm0,[[dest]+0*2*8]
  movups xmm1,[[sour]+0*2*8]
  subps  xmm0,xmm1
  mulps xmm0,xmm0
  movups xmm2,xmm0

  movups xmm0,[[dest]+1*2*8]
  movups xmm1,[[sour]+1*2*8]
  subps  xmm0,xmm1
  mulps xmm0,xmm0
  addps xmm2,xmm0

  movups xmm0,[[dest]+2*2*8]
  movups xmm1,[[sour]+2*2*8]
  subps  xmm0,xmm1
  mulps xmm0,xmm0
  addps xmm2,xmm0

  movups xmm0,[[dest]+3*2*8]
  movups xmm1,[[sour]+3*2*8]
  subps  xmm0,xmm1
  mulps xmm0,xmm0
  addps xmm2,xmm0

  movups xmm0,[[dest]+4*2*8]
  movups xmm1,[[sour]+4*2*8]
  subps  xmm0,xmm1
  mulps xmm0,xmm0
  addps xmm2,xmm0

  movups xmm0,[[dest]+5*2*8]
  movups xmm1,[[sour]+5*2*8]
  subps  xmm0,xmm1
  mulps xmm0,xmm0
  addps xmm2,xmm0

  movups xmm0,[[dest]+6*2*8]
  movups xmm1,[[sour]+6*2*8]
  subps  xmm0,xmm1
  mulps xmm0,xmm0
  addps xmm2,xmm0

  movups xmm0,[[dest]+7*2*8]
  movups xmm1,[[sour]+7*2*8]
  subps  xmm0,xmm1
  mulps xmm0,xmm0
  addps xmm2,xmm0

  movups [Result],xmm2
end;

function SSE_Distance_S(n: Integer; sour, dest: PSingle): Single;
var
  tmp: TSingle_4X;
begin
  Result := 0;
  while n >= 32 do
    begin
      tmp := SSE_SQR_32(sour, dest);
      inc(sour, 32);
      inc(dest, 32);
      dec(n, 32);
      Result := Result + tmp[0] + tmp[1] + tmp[2] + tmp[3];
    end;
  while n >= 16 do
    begin
      tmp := SSE_SQR_16(sour, dest);
      inc(sour, 16);
      inc(dest, 16);
      dec(n, 16);
      Result := Result + tmp[0] + tmp[1] + tmp[2] + tmp[3];
    end;
  while n >= 4 do
    begin
      tmp := SSE_SQR_4(sour, dest);
      inc(sour, 4);
      inc(dest, 4);
      dec(n, 4);
      Result := Result + tmp[0] + tmp[1] + tmp[2] + tmp[3];
    end;
  while n >= 3 do
    begin
      tmp := SSE_SQR_4(sour, dest);
      inc(sour, 3);
      inc(dest, 3);
      dec(n, 3);
      Result := Result + tmp[0] + tmp[1] + tmp[2];
    end;
  while n >= 2 do
    begin
      tmp := SSE_SQR_4(sour, dest);
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


function Pascal_Distance_S(n: Integer; sour, dest: PSingle): Single;
var
  i: Integer;
  f: Single;
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

function Do_Test_SIMD_Distance_S: Boolean;
const
  L_ = 2051;
type
  T_ = array [0 .. L_] of Single;
var
  i: Integer;
  c1_, c2_: T_;
  r1, r2: Single;
begin
  for i := 0 to L_ - 1 do
    begin
      c1_[i] := Random(1000) * 0.001;
      c2_[i] := Random(1000) * 0.001;
    end;
  r1 := Pascal_Distance_S(L_, @c1_, @c2_);
{$IFDEF SSE_Optimize_Distance_Compute}
  r2 := SSE_Distance_S(L_, @c1_, @c2_);
{$ELSE SSE_Optimize_Distance_Compute}
  r2 := r1;
{$ENDIF SSE_Optimize_Distance_Compute}
  Result := abs(r2 - r1) < 0.0001;
end;

end.
 
