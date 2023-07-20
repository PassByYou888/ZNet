program _135_StringReplaceTech;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UPascalStrings,
  Z.Status,
  Z.UnicodeMixedLib,
  Z.UReplace,
  Z.ListEngine;

// �ַ����滻����demo
// �ַ����滻��pascal����ת�弼��,�Լ�֧�ֺ깦�ܵĳ������ʹ��
// replace���ַ������������е���Ҫ����
// replace����֧����������,�ֱ���UnicdoeMixedLib��UReplace
// UnicdoeMixedLib=delphiͨ��,��fpc+lcl,UnicdoeMixedLib=���ֽ��ַ���
// UReplace=delphiͨ��,��fpc+lcl,UReplace=���ֽ��ַ���(����֧��)
// replace�ڲ��������̰������ܻ��Ʊ�д

// demo���滻
procedure demo_macro;
var
  L: THashStringList;
  i: Integer;
begin
  // L�����ڳ��������ж�̬��ֵ,���Ǹ��ٵ�
  L := THashStringList.Create;
  for i := 1 to 100 do
    if i mod 3 = 0 then
        L[Format('<%d>', [i])] := Format('macro(%d)', [i])
    else
        L[Format('<%d>', [i])] := Format('%d', [i]);

  DoStatus(L.Replace('<1>,<2>,<3>', false, true, 0, 0)); // ������: 1,2,macro(3)
  DisposeObject(L);
end;

// demo���ݽṹ
procedure demo_struct;
const
  c =
    '�ַ����滻����demo'#13#10 +
    '�ַ����滻��pascal����ת�弼��,�Լ�֧�ֺ깦�ܵĳ������ʹ��'#13#10 +
    'replace����֧����������,�ֱ���UnicdoeMixedLib��UReplace'#13#10 +
    'replace�������̰������ܻ��Ʊ�д,�߱����ٴ������������'#13#10;

var
  arry: TArrayBatch; // replace�������滻����������
  L: TBatchInfoList; // replace��״̬�ṹ
  i: Integer;
begin
  SetLength(arry, 2);
  arry[0].sour := 'demo';
  arry[0].dest := '<demo>';
  arry[1].sour := 'pascal';
  arry[1].dest := '<pascal>';
  umlSortBatch(arry);

  L := TBatchInfoList.Create; // L�������滻�����Ժ�����ݱ���״̬,�������滻��Դ�ı�λ��,Ŀ���ı���λ��
  DoStatus(umlBatchReplace(c, arry, false, true, 0, 0, L, nil)); // umlBatchReplace�������滻,�괦������ڵײ��ʹ��umlBatchReplaceʵ�������滻
  // L�ı�����Ϣ���Է����ⲿ�༭�������ݱ�ע
  for i := 0 to L.Count - 1 do
      DoStatus('�� %d �ַ��� "%s" �滻�� "%s"', [L[i].sour_bPos, arry[L[i].Batch].sour.Text, arry[L[i].Batch].dest.Text]);

  DisposeObject(L);
end;

begin
  demo_macro();
  demo_struct();
  readln;

end.
