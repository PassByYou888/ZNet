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

// 这是多年来一直遗漏的底层库demo
// TextDataEngine是个类ini库,它与平台无关,并且被大量框架所引用
// ini符号定义极简,书写不易出错,ZAI中有许多框架使用Ini来表达结构和流程脚本
// TextDataEngine可以支持非常巨大的ini文件或数据库
// 本demo不会演示在ini编写脚本范式,过于复杂
// 本demo主要叙述使用TextDataEngine的注意事项,只要编程时遵守规则,程序推大就没问题

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

  // te内部有两种解析结构器
  // 使用te时一定要区分这两种解析结构器,一次只能使用一种,例如,用字符串就全部走字符串操作te,用variant就全部variant操作te

  // 第一种是字符串
  DoStatus(te.HitS['a', 'a']); // 直接字符串
  DoStatus(te.GetDefaultText_I32('a', 'a', 0)); // 取字符串,并把字符串转换成整数返回
  DoStatus(te.GetDefaultText_Float('a', 'a', 0)); // 取字符串,并把字符串转换成浮点返回

  // 第二种是variant变量
  DoStatus(VarToStr(te.Hit['b', 'a'])); // te内部会先使用expression做1+1计算,得到variant返回值,然后再把variant直接转换成字符串

  disposeObject(te);
end;

begin
  demo;
  readln;

end.
