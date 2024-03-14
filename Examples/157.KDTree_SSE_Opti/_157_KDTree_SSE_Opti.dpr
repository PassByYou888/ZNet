program _157_KDTree_SSE_Opti;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.MemoryStream,
  Z.Geometry2D,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.Opti_Distance_S,
  Z.Opti_Distance_D;

// Pascal_Distance主要用于Learn引擎中的K值计算
// 当内核使用SSE_Distance可以极大提速Learn引擎的工作效率,包括目标定位,读取,训练,候选计算
// SSE_Distance对于SSL母体模型有明显的提速作用
// 其它提速场景为目标分类器候选机制,候选规模可以从10万提升到50万
// 对于32+fpu浮点模型提速为900%,64+整数模拟浮点模型提速为400%,支持多线程并发
procedure do_test_perf_double;
const
  L_ = 1052;
type
  T_ = array [0 .. L_] of Double;
var
  i: Integer;
  c1_, c2_: T_;
  r1, r2: Double;
  tk: TTimeTick;
begin
  for i := 0 to L_ - 1 do
    begin
      c1_[i] := umlRRD(0, 1);
      c2_[i] := umlRRD(0, 1);
    end;

  tk := GetTimeTick();
  for i := 1 to 1000000 do
      r1 := Pascal_Distance_D(L_, @c1_, @c2_);
  DoStatus('(double float)pascal distance compute:%dms', [GetTimeTick - tk]);

  tk := GetTimeTick();
  for i := 1 to 1000000 do
      r2 := SSE_Distance_D(L_, @c1_, @c2_);
  DoStatus('(double float)sse optimized distance compute:%dms', [GetTimeTick - tk]);
end;

procedure do_test_perf_single;
const
  L_ = 1052;
type
  T_ = array [0 .. L_] of single;
var
  i: Integer;
  c1_, c2_: T_;
  r1, r2: single;
  tk: TTimeTick;
begin
  for i := 0 to L_ - 1 do
    begin
      c1_[i] := umlRRS(0, 1);
      c2_[i] := umlRRS(0, 1);
    end;

  tk := GetTimeTick();
  for i := 1 to 1000000 do
      r1 := Pascal_Distance_S(L_, @c1_, @c2_);
  DoStatus('(single float)pascal distance compute:%dms', [GetTimeTick - tk]);

  tk := GetTimeTick();
  for i := 1 to 1000000 do
      r2 := SSE_Distance_S(L_, @c1_, @c2_);
  DoStatus('(single float)sse optimized distance compute:%dms', [GetTimeTick - tk]);
end;

begin
  do_test_perf_double;
  do_test_perf_single;
  DoStatus('press return to exit.');
  readln;

end.
