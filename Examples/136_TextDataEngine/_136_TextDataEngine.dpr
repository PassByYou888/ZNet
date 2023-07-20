program _136_TextDataEngine;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Variants,
  Z.Core,
  Z.PascalStrings,
  Z.UPascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.ListEngine,
  Z.TextDataEngine;

// ���Ƕ�����һֱ��©�ĵײ��demo
// TextDataEngine�Ǹ���ini��,����ƽ̨�޹�,���ұ��������������
// ini���Ŷ��弫��,��д���׳���,ZAI���������ʹ��Ini�����ṹ�����̽ű�
// TextDataEngine����֧�ַǳ��޴��ini�ļ������ݿ�
// ��demo������ʾ��ini��д�ű���ʽ,���ڸ���
// ��demo��Ҫ����ʹ��TextDataEngine��ע������,ֻҪ���ʱ���ع���,�����ƴ��û����

procedure demo;
const
  c =
    '[a]'#13#10 +
    'a=12345'#13#10 +
    '[b]'#13#10 +
    'a=exp(1+1)'#13#10;
var
  te: TTextDataEngine;
begin
  te := TTextDataEngine.Create;
  te.AsText := c;

  // te�ڲ������ֽ����ṹ��
  // ʹ��teʱһ��Ҫ���������ֽ����ṹ��,һ��ֻ��ʹ��һ��,����,���ַ�����ȫ�����ַ�������te,��variant��ȫ��variant����te

  // ��һ�����ַ���
  DoStatus(te.HitS['a', 'a']); // ֱ���ַ���
  DoStatus(te.GetDefaultText_I32('a', 'a', 0)); // ȡ�ַ���,�����ַ���ת������������
  DoStatus(te.GetDefaultText_Float('a', 'a', 0)); // ȡ�ַ���,�����ַ���ת���ɸ��㷵��

  // �ڶ�����variant����
  DoStatus(VarToStr(te.Hit['b', 'a'])); // te�ڲ�����ʹ��expression��1+1����,�õ�variant����ֵ,Ȼ���ٰ�variantֱ��ת�����ַ���

  disposeObject(te);
end;

begin
  demo;
  readln;

end.
