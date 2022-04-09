{ ****************************************************************************** }
{ * GBK Vector                                                                 * }
{ ****************************************************************************** }
unit Z.GBKVec;

{$I Z.Define.inc}

interface

uses Z.Status, Z.Core, Z.PascalStrings, Z.UPascalStrings, Variants,
  Z.MemoryStream, Z.ListEngine, Z.TextDataEngine, Z.UnicodeMixedLib;

function WordPart(const s: TUPascalString; const Unidentified, Completed: TListPascalString): Integer; overload;
function WordPart(const s: TUPascalString): TPascalString; overload;
function WordPartN(const s: TUPascalString): TPascalString;
function WordPartD(const s: TUPascalString): TPascalString;

function WillVec(const s: TUPascalString): Integer;
function WordVec(const s: TUPascalString): Integer;

function BadEmotion(const s: TUPascalString): Integer;
function BadRep(const s: TUPascalString): Integer;
function GoodEmotion(const s: TUPascalString): Integer;
function GoodRep(const s: TUPascalString): Integer;

implementation

uses Z.GBK, Z.GBKMediaCenter;

function WordPart(const s: TUPascalString; const Unidentified, Completed: TListPascalString): Integer;
var
  n, tmp: TUPascalString;
  i, j: Integer;
  Successed: Boolean;
begin
  WaitGBKMediaInit;
  n := GBKString(s);
  Result := 0;

  i := 1;
  while i <= n.L do
    begin
      Successed := False;
      j := umlMin(WordPartDict.MaxSectionNameSize, n.L - i + 1);
      while j > 1 do
        begin
          tmp := n.Copy(i, j);
          Successed := WordPartDict.Exists(tmp);
          if Successed then
            begin
              Completed.Add(tmp.Text, WordPartDict.VariantList[tmp.Text]);
              inc(Result);
              inc(i, j);
              Break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          Successed := WordPartDict.Exists(n[i]);
          if Successed then
            begin
              Completed.Add(n[i], WordPartDict.VariantList[n[i]]);
              inc(Result);
            end
          else
            begin
              Unidentified.Add(n[i]);
            end;
          inc(i);
        end;
    end;
end;

function WordPart(const s: TUPascalString): TPascalString;
var
  Unidentified: TListPascalString;
  Completed: TListPascalString;
  i: Integer;
begin
  WaitGBKMediaInit;
  Result := '';
  Unidentified := TListPascalString.Create;
  Completed := TListPascalString.Create;
  if WordPart(s, Unidentified, Completed) > 0 then
    begin
      for i := 0 to Completed.Count - 1 do
        begin
          if Result.L > 0 then
              Result.Append(',');
          Result.Append(Completed[i]);
        end;
    end;
  DisposeObject([Unidentified, Completed]);
end;

function WordPartN(const s: TUPascalString): TPascalString;
var
  Unidentified: TListPascalString;
  Completed: TListPascalString;
  i: Integer;
begin
  WaitGBKMediaInit;
  Result := '';
  Unidentified := TListPascalString.Create;
  Completed := TListPascalString.Create;
  if WordPart(s, Unidentified, Completed) > 0 then
    begin
      for i := 0 to Completed.Count - 1 do
        begin
          if Result.L > 0 then
              Result.Append(' ');
          Result.Append(Completed[i].Text + '\' + VarToStr(THashVariantList(Completed.Objects[i]).GetDefaultValue('token', '')));
        end;
    end;
  DisposeObject([Unidentified, Completed]);
end;

function WordPartD(const s: TUPascalString): TPascalString;
var
  Unidentified: TListPascalString;
  Completed: TListPascalString;
  i: Integer;
begin
  WaitGBKMediaInit;
  Result := '';
  Unidentified := TListPascalString.Create;
  Completed := TListPascalString.Create;
  if WordPart(s, Unidentified, Completed) > 0 then
    begin
      for i := 0 to Completed.Count - 1 do
        begin
          if Result.L > 0 then
              Result.Append(#13#10);
          Result.Append(Completed[i].Text + '(' + VarToStr(THashVariantList(Completed.Objects[i]).GetDefaultValue('desc', '')) + ')');
        end;
    end;
  DisposeObject([Unidentified, Completed]);
end;

function FullQuery_Table(const List: THashList; const s: TUPascalString): Integer; overload;
var
  n, tmp, n3: TUPascalString;
  i, j, L: Integer;
  Successed: Boolean;
begin
  WaitGBKMediaInit;
  n := GBKString(s);

  Result := 0;
  L := List.MaxNameSize;

  i := 1;
  while i <= n.L do
    begin
      Successed := False;
      j := umlMin(L, n.L - i + 1);
      while j > 1 do
        begin
          tmp := n.Copy(i, j);
          Successed := List.Exists(tmp);
          if Successed then
            begin
              inc(Result);
              inc(i, j);
              Break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          Successed := List.Exists(n[i]);
          if Successed then
              inc(Result);
          inc(i);
        end;
    end;
end;

function FullQuery_Table(const List: THashTextEngine; const s: TUPascalString): Integer; overload;
  function InternalQuery(const vl: THashVariantList; const n: TUPascalString): Integer;
  var
    tmp: TUPascalString;
    i, j, L: Integer;
    Successed: Boolean;
  begin
    Result := 0;
    L := vl.HashList.MaxNameSize;

    i := 1;
    while i <= n.L do
      begin
        Successed := False;
        j := umlMin(L, n.L - i + 1);
        while j > 1 do
          begin
            tmp := n.Copy(i, j);
            Successed := vl.Exists(tmp);
            if Successed then
              begin
                inc(Result);
                inc(i, j);
                Break;
              end;
            dec(j);
          end;

        if not Successed then
          begin
            Successed := vl.Exists(n[i]);
            if Successed then
                inc(Result);
            inc(i);
          end;
      end;
  end;

var
  n: TUPascalString;
  i, r: Integer;
  pl: TListPascalString;
begin
  WaitGBKMediaInit;
  n := GBKString(s);
  Result := 0;
  pl := TListPascalString.Create;
  List.GetSectionList(pl);
  for i := 0 to pl.Count - 1 do
    begin
      r := InternalQuery(List.VariantList[pl[i]], n);
      inc(Result, umlStrToInt(pl[i]) * r);
    end;
  DisposeObject(pl);
end;

function WillVec(const s: TUPascalString): Integer;
begin
  WaitGBKMediaInit;
  Result := FullQuery_Table(WillVecDict, s);
end;

function WordVec(const s: TUPascalString): Integer;
begin
  WaitGBKMediaInit;
  Result := FullQuery_Table(WordVecDict, s);
end;

function BadEmotion(const s: TUPascalString): Integer;
begin
  WaitGBKMediaInit;
  Result := FullQuery_Table(BadEmotionDict, s);
end;

function BadRep(const s: TUPascalString): Integer;
begin
  WaitGBKMediaInit;
  Result := FullQuery_Table(BadRepDict, s);
end;

function GoodEmotion(const s: TUPascalString): Integer;
begin
  WaitGBKMediaInit;
  Result := FullQuery_Table(GoodEmotionDict, s);
end;

function GoodRep(const s: TUPascalString): Integer;
begin
  WaitGBKMediaInit;
  Result := FullQuery_Table(GoodRepDict, s);
end;

end.
