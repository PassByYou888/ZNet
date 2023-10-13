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
  { Pairing devices are widely used in data analysis and application matching, with NLP, SIFT, Surf, and CV all using pairing }
  { TBidirectional_The Matched algorithm is bidirectional and can eliminate mismatches, with an accuracy rate of almost 100% }
  { The following demo demonstrates single dimensional number pairing, and the results will be accurate and accurate }
  { Among numerous pairing algorithms, many of them are parallelized, and in the single threaded domain, TBidirectional_Matched is the world's fastest single line matching algorithm, and its mechanism determines its impeccable performance, without one }
  { TBidirectional_The Matched pairing algorithm is very suitable for servers }
  TNumMatched = class(TBidirectional_Matched<Single>)
  public
    { The diff interface provides the difference value between two data, and the remaining work is handed over to the pairing device }
    { The data can be 2d/3d coordinates, can be sift/surf feature children, can be strings, or can be images }
    { Diff's job is to quantify these data }
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
  { The construction parameter is reject difference, and the data difference is higher than this value. No pairing operation will be performed }
  nm := TNumMatched.Create(0.1);

  { Randomly generate main data }
  while nm.Primary_Pool.Num < 20000 do
      nm.Primary_Pool.Add(umlRandomRangeS(1, 100000));

  { Randomly generate secondary data }
  while nm.Second_Pool.Num < 10000 do
      nm.Second_Pool.Add(umlRandomRangeS(1, 100000));

  DoStatus('Traditional Bidirectional pairing algorithms require the calculation of 200 million quantized differences, TBidirectional_Matched can be completed instantly in a single thread');

  { Calculate matches and return the number of completed matches }
  r := nm.Compute_Matched();
  DoStatus('Pairing completed:%d', [r]);
  DoStatus('Enter key to display pairing results');
  readln;

  if nm.Pair_Pool.L.Num > 0 then
    with nm.Pair_Pool.L.Repeat_ do
      repeat
          DoStatus('Pairing result (%d) %f<->%f', [I__, Queue^.Data.Primary, Queue^.Data.Second]);
      until not Next;

  DoStatus('End of carriage return');
  readln;
  DisposeObject(nm);
end;

begin
  DoRun;

end.
