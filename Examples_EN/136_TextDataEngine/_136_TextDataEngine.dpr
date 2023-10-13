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

{ This is a low-level library demo that has been missing for many years }
{ TextDataEngine is an ini like library that is platform independent and referenced by a large number of frameworks }
{ The ini symbol definition is minimalist, and writing is not prone to errors. There are many frameworks in ZAI that use Ini to express structure and process scripts }
{ TextDataEngine can support very large ini files or databases }
{ This demo will not demonstrate the scripting paradigm in ini, which is too complex }
{ This demo mainly discusses the precautions for using TextDataEngine. As long as you follow the rules during programming, there is no problem with expanding the program }

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

  { There are two types of parsers inside TE }
  { When using TE, it is important to distinguish between these two types of parsing constructors. Only one can be used at a time. For example, if you use a string, you can perform all string operations on TE, and if you use a variant, you can perform all variant operations on TE }

  { The first type is a string }
  DoStatus(te.HitS['a', 'a']); { Direct String }
  DoStatus(te.GetDefaultText_I32('a', 'a', 0)); { Take a string and convert it to an integer to return }
  DoStatus(te.GetDefaultText_Float('a', 'a', 0)); { Take a string and convert it to a floating-point return }

  { The second type is the variant variable }
  DoStatus(VarToStr(te.Hit['b', 'a'])); { TE internally uses expression to perform 1+1 calculations to obtain the return value of variant, and then directly converts the variant into a string }

  disposeObject(te);
end;

begin
  demo;
  readln;

end.
