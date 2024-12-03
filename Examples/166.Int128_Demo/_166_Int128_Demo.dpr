program _166_Int128_Demo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  Z.Core,
  Z.Status,
  Z.Int128,
  Z.PascalStrings,
  Z.UPascalStrings,
  Variants;

{
  Int128区分有符号与无符号两种结构,主要用于解决极大数字的高速计算问题,Int128可以高速计算大数公式,但它不是无限大数
  int128/uint128已经通过testcase
  编写该demo时作者身负轻伤(摔伤),忍痛中完成(点赞)
}

procedure int128_demo;
var
  i128: Int128; // 带符号int128
  u128: UInt128; // 无符号int128
  v128: Int128; // 运算结果,临时变量
  v: Variant; // variant变量
begin
  // 赋值可以支持32位直接写整数,但是建议都给字符串表达式赋值
  // 具体赋值的细节处理可trace进去自己研究
  i128 := '-12345'; // 长整数表达式最大只能32位,如果128位需要给字符串表达式才行,int128会自动转换字符串表达式
  u128 := '123456';

  v128 := i128 + u128; // 高速计算
  DoStatus(v128.ToString); // 打印计算结果

  // 如果要使用variant变量必须使用ToVariant转换,细节见代码 "TInt128_VariantType"
  v := i128.ToVariant + u128.ToVariant;
  DoStatus(VarToStr(v)); // 打印variant计算结果
end;

begin
  int128_demo;

end.
