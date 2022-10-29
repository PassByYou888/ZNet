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
  // ������㷺Ӧ�������ݷ�����Ӧ��ƥ�䣬NLP,SIFT,Surf,CV����ʹ�����
  // TBidirectional_Matched�㷨��˫���䣬���ų����䣬��ȷ�ʼ���100%
  // ����demo��ʾ�˵�ά�ȵ�������ԣ������׼ȷ����
  // ���ڶ�����㷨�У������Զ��ǲ��л��ģ��ڵ��߳�����TBidirectional_Matched�����������ĵ���ƥ���㷨�����ƾ����㷨�����޿����ޣ�û��֮һ
  // TBidirectional_Matched����㷨�ǳ��ʺϷ�����
  TNumMatched = class(TBidirectional_Matched<Single>)
  public
    // diff�ӿ��Ǹ����������ݼ�Ĳ���ֵ��ʣ�µĹ��������������
    // ���ݿ�����2d/3d���꣬������sift/surf�����ӣ��������ַ�����Ҳ������ͼ��
    // diff�Ĺ����ǽ���Щ���ݸ�����������
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
  // ���������Ǿܾ�����ݲ�����ڸ�ֵ��������Բ���
  nm := TNumMatched.Create(0.1);

  // ���������Ҫ����
  while nm.Primary_Pool.Num < 20000 do
      nm.Primary_Pool.Add(umlRandomRangeS(1, 100000));

  // ������ɴ�Ҫ����
  while nm.Second_Pool.Num < 10000 do
      nm.Second_Pool.Add(umlRandomRangeS(1, 100000));

  DoStatus('��ͳ Bidirectional ����㷨�������2�ڴζ��������죬TBidirectional_Matched���ڵ��߳���˲�����');

  // ����ƥ�䣬������ɵ�ƥ������
  r := nm.Compute_Matched();
  DoStatus('������: %d', [r]);
  DoStatus('�س�����ʾ��Խ��..');
  readln;

  if nm.Pair_Pool.L.Num > 0 then
    with nm.Pair_Pool.L.Repeat_ do
      repeat
          DoStatus('��Խ��(%d) %f <-> %f', [I__, Queue^.Data.Primary, Queue^.Data.Second]);
      until not Next;

  DoStatus('�س�����..');
  readln;
  DisposeObject(nm);
end;

begin
  DoRun;
end.
