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

// 字符串替换技术demo
// 字符串替换被pascal代码转义技术,以及支持宏功能的程序大量使用
// replace是字符串技术领域中的重要工具
// replace技术支持有两个库,分别是UnicdoeMixedLib和UReplace
// UnicdoeMixedLib=delphi通用,在fpc+lcl,UnicdoeMixedLib=单字节字符串
// UReplace=delphi通用,在fpc+lcl,UReplace=多字节字符串(中文支持)
// replace内部流程流程按高性能机制编写

// demo宏替换
procedure demo_macro;
var
  L: THashStringList;
  i: Integer;
begin
  // L可以在程序运行中动态赋值,这是高速的
  L := THashStringList.Create;
  for i := 1 to 100 do
    if i mod 3 = 0 then
        L[Format('<%d>', [i])] := Format('macro(%d)', [i])
    else
        L[Format('<%d>', [i])] := Format('%d', [i]);

  DoStatus(L.Replace('<1>,<2>,<3>', false, true, 0, 0)); // 输出结果: 1,2,macro(3)
  DisposeObject(L);
end;

// demo数据结构
procedure demo_struct;
const
  c =
    '字符串替换技术demo'#13#10 +
    '字符串替换被pascal代码转义技术,以及支持宏功能的程序大量使用'#13#10 +
    'replace技术支持有两个库,分别是UnicdoeMixedLib和UReplace'#13#10 +
    'replace处理流程按高性能机制编写,具备高速处理百万行能力'#13#10;

var
  arry: TArrayBatch; // replace批量化替换的数据输入
  L: TBatchInfoList; // replace的状态结构
  i: Integer;
begin
  SetLength(arry, 2);
  arry[0].sour := 'demo';
  arry[0].dest := '<demo>';
  arry[1].sour := 'pascal';
  arry[1].dest := '<pascal>';
  umlSortBatch(arry);

  L := TBatchInfoList.Create; // L是做完替换工作以后的数据报告状态,包括被替换的源文本位置,目标文本的位置
  DoStatus(umlBatchReplace(c, arry, false, true, 0, 0, L, nil)); // umlBatchReplace是批量替换,宏处理程序在底层均使用umlBatchReplace实现批量替换
  // L的报告信息可以方便外部编辑器做数据标注
  for i := 0 to L.Count - 1 do
      DoStatus('在 %d 字符将 "%s" 替换成 "%s"', [L[i].sour_bPos, arry[L[i].Batch].sour.Text, arry[L[i].Batch].dest.Text]);

  DisposeObject(L);
end;

begin
  demo_macro();
  demo_struct();
  readln;

end.
