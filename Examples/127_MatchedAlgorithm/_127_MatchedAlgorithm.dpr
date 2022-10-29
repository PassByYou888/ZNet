program _127_MatchedAlgorithm;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  System.Math,
  Z.Core,
  Z.Matched.Templet,
  Z.Status,
  Z.PascalStrings,
  Z.UPascalStrings,
  Z.UnicodeMixedLib;

type
  // 配对器广泛应用于数据分析与应用匹配，NLP,SIFT,Surf,CV都有使用配对
  // TBidirectional_Matched算法是双向配，可排除误配，正确率几乎100%
  // 下列demo演示了单维度的数字配对，结果会准确无误
  // 在众多配对算法中，许多配对都是并行化的，在单线程领域TBidirectional_Matched是世界上最快的单线匹配算法，机制决定算法性能无可挑剔，没有之一
  // TBidirectional_Matched配对算法非常适合服务器
  TNumMatched = class(TBidirectional_Matched<Single>)
  public
    // diff接口是给出两个数据间的差异值，剩下的工作，交给配对器
    // 数据可以是2d/3d坐标，可以是sift/surf特征子，可以是字符串，也可以是图像
    // diff的工作是将这些数据给度量化出来
    function Diff(const p1, p2: Single): Single; override;
  end;

function TNumMatched.Diff(const p1, p2: Single): Single;
begin
  Result := abs(p2 - p1);
end;

procedure DoRun;
var
  nm: TNumMatched;
  r: Integer;
begin
  // 构建参数是拒绝差，数据差异高于该值，不做配对操作
  nm := TNumMatched.Create(0.1);

  // 随机生成主要数据
  while nm.Primary_Pool.Num < 20000 do
      nm.Primary_Pool.Add(umlRandomRangeS(1, 100000));

  // 随机生成次要数据
  while nm.Second_Pool.Num < 10000 do
      nm.Second_Pool.Add(umlRandomRangeS(1, 100000));

  DoStatus('传统 Bidirectional 配对算法，需计算2亿次度量化差异，TBidirectional_Matched可在单线程中瞬间完成');

  // 计算匹配，返回完成的匹配数量
  r := nm.Compute_Matched();
  DoStatus('完成配对: %d', [r]);
  DoStatus('回车键显示配对结果..');
  readln;

  if nm.Pair_Pool.L.Num > 0 then
    with nm.Pair_Pool.L.Repeat_ do
      repeat
          DoStatus('配对结果(%d) %f <-> %f', [I__, Queue^.Data.Primary, Queue^.Data.Second]);
      until not Next;

  DoStatus('回车结束..');
  readln;
  DisposeObject(nm);
end;

begin
  DoRun;
end.
