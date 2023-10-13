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

{ Demo of string replacement technology }
{ String replacement is heavily used by Pascal code escape techniques and programs that support macro functionality }
{ Replace is an important tool in the field of string technology }
{ There are two libraries for replace technical support, namely UnicdoeMixedLib and UReplace }
{ UnicdoeMixedLib=delphi universal, in fpc+lcl, UnicdoeMixedLib=single byte string }
{ UReplace=Delphi universal, in fpc+lcl, UReplace=multi byte string (supported in Chinese) }
{ Replace internal process flow is written according to high-performance mechanism }

{ Demo macro replacement }
procedure demo_macro;
var
  L: THashStringList;
  i: Integer;
begin
  { L can be dynamically assigned during program execution, which is high-speed }
  L := THashStringList.Create;
  for i := 1 to 100 do
    if i mod 3 = 0 then
        L[Format('<%d>', [i])] := Format('macro(%d)', [i])
    else
        L[Format('<%d>', [i])] := Format('%d', [i]);

  DoStatus(L.Replace('<1>,<2>,<3>', false, true, 0, 0)); { Output results: 1,2, macro (3) }
  DisposeObject(L);
end;

{ Demo data structure }
procedure demo_struct;
const
  c =
    'Demo of string replacement technology' +
    'String replacement is heavily used by Pascal code escape techniques and programs that support macro functionality' +
    'There are two libraries for replace technical support, namely UnicdoeMixedLib and UReplace' +
    'The replace processing process is written according to a high-performance mechanism, with the ability to process millions of lines at high speed';

var
  arry: TArrayBatch; { Replace Batch Replacement Data Input }
  L: TBatchInfoList; { The state structure of replace }
  i: Integer;
begin
  SetLength(arry, 2);
  arry[0].sour := 'demo';
  arry[0].dest := '<demo>';
  arry[1].sour := 'pascal';
  arry[1].dest := '<pascal>';
  umlSortBatch(arry);

  L := TBatchInfoList.Create; { L is the status of the data report after completing the replacement work, including the position of the replaced source text and the position of the target text }
  DoStatus(umlBatchReplace(c, arry, false, true, 0, 0, L, nil)); { UmlBatchReplace is a batch replacement, and macro handlers use umlBatchReplace at the bottom to achieve batch replacement }
  { The report information of L can facilitate external editors to annotate data }
  for i := 0 to L.Count - 1 do
      DoStatus('Replace '#39'%s'#39' as '#39'%s'#39' in%d characters', [arry[L[i].Batch].sour.Text, arry[L[i].Batch].dest.Text, L[i].sour_bPos]);

  DisposeObject(L);
end;

begin
  demo_macro();
  demo_struct();
  readln;

end.
