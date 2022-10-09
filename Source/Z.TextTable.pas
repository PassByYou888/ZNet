{ ****************************************************************************** }
{ * Transform TextTable                                                        * }
{ ****************************************************************************** }

unit Z.TextTable;

{$I Z.Define.inc}

interface

uses SysUtils, Z.Core, Z.DFE, Z.ListEngine, Z.UnicodeMixedLib,
  Z.MemoryStream, Z.Parsing, Z.PascalStrings, Z.UPascalStrings;

type
  TTranlateStyle = (tsPascalText, tsPascalComment, tsCText, tsCComment, tsNormalText, tsDFMText);

  TTextTableItem = record
    // origin info
    OriginText: SystemString;
    Category: SystemString;
    // ext pick info
    Picked: Boolean;
    // encode and import info
    index: Integer;
    DefineText: SystemString;
    // text style
    TextStyle: TTranlateStyle;
    // fast hash
    OriginHash: THash;
    DefineHash: THash;
    // project language
    originLanguage: Integer;
    DefineLanguage: Integer;
    RepCount: Integer;
    procedure InitSelf;
    procedure SaveToStream(stream: TCore_Stream);
    procedure LoadFromStream(stream: TCore_Stream);
  end;

  PTextTableItem = ^TTextTableItem;

  TTextTable = class(TCore_Object)
  protected
    FList: TCore_List;
    function GetItems(index: Integer): PTextTableItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: Integer;
    property Items[index: Integer]: PTextTableItem read GetItems; default;
    procedure Delete(index: Integer);

    function GetMaxIndexNo: Integer;

    function GetOrigin(const s: SystemString): PTextTableItem;
    property origin[const s: SystemString]: PTextTableItem read GetOrigin;

    procedure AddCopy(var t: TTextTableItem);
    procedure AddText(OriginText_, Category_: SystemString; Picked_: Boolean);
    procedure AddPascalText(OriginText_, Category_: SystemString; Picked_: Boolean);
    procedure AddPascalComment(OriginText_, Category_: SystemString; Picked_: Boolean);
    procedure AddCText(OriginText_, Category_: SystemString; Picked_: Boolean);
    procedure AddCComment(OriginText_, Category_: SystemString; Picked_: Boolean);
    procedure AddDelphiFormText(OriginText_, Category_: SystemString; Picked_: Boolean);

    procedure ChangeDefineText(index: Integer; newDefine: U_String);
    function ExistsIndex(index: Integer): Boolean;

    function Search(OriginText_: SystemString): PTextTableItem;

    procedure SaveToStream(stream: TCore_Stream);
    procedure LoadFromStream(stream: TCore_Stream);

    procedure ExportToTextStream(stream: TCore_Stream);
    procedure ImportFromTextStream(stream: TCore_Stream);
  end;

implementation

procedure TTextTableItem.InitSelf;
begin
  OriginText := '';
  Category := '';
  Picked := False;
  index := -1;
  DefineText := '';
  TextStyle := tsNormalText;
  RepCount := 0;
  OriginHash := 0;
  DefineHash := 0;
end;

procedure TTextTableItem.LoadFromStream(stream: TCore_Stream);
var
  df: TDFE;
begin
  df := TDFE.Create;
  df.DecodeFrom(stream);

  OriginText := df.Reader.ReadString;
  Category := df.Reader.ReadString;
  Picked := df.Reader.ReadBool;
  index := df.Reader.ReadInteger;
  DefineText := df.Reader.ReadString;
  TextStyle := TTranlateStyle(df.Reader.ReadInteger);
  RepCount := df.Reader.ReadInteger;

  OriginHash := df.Reader.ReadCardinal;
  DefineHash := df.Reader.ReadCardinal;

  originLanguage := df.Reader.ReadInteger;
  DefineLanguage := df.Reader.ReadInteger;

  DisposeObject(df);
end;

procedure TTextTableItem.SaveToStream(stream: TCore_Stream);
var
  df: TDFE;
begin
  df := TDFE.Create;
  df.WriteString(OriginText);
  df.WriteString(Category);
  df.WriteBool(Picked);
  df.WriteInteger(index);
  df.WriteString(DefineText);
  df.WriteInteger(Integer(TextStyle));
  df.WriteInteger(RepCount);

  df.WriteCardinal(OriginHash);
  df.WriteCardinal(DefineHash);

  df.WriteInteger(originLanguage);
  df.WriteInteger(DefineLanguage);

  df.EncodeTo(stream);
  DisposeObject(df);
end;

function TTextTable.GetItems(index: Integer): PTextTableItem;
begin
  Result := FList[index];
end;

constructor TTextTable.Create;
begin
  inherited Create;
  FList := TCore_List.Create;
end;

destructor TTextTable.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TTextTable.Clear;
var
  i: Integer;
  p: PTextTableItem;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      Dispose(p);
    end;
  FList.Clear;
end;

function TTextTable.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TTextTable.Delete(index: Integer);
var
  p: PTextTableItem;
begin
  p := FList[index];
  Dispose(p);
  FList.Delete(index);
end;

function TTextTable.GetMaxIndexNo: Integer;
var
  i: Integer;
  p: PTextTableItem;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do
    begin
      p := PTextTableItem(FList[i]);
      if p^.index > Result then
          Result := p^.index;
    end;
end;

function TTextTable.GetOrigin(const s: SystemString): PTextTableItem;
var
  i: Integer;
  p: PTextTableItem;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
    begin
      p := PTextTableItem(FList[i]);
      if (s = p^.OriginText) then
          Exit(p);
    end;
end;

procedure TTextTable.AddCopy(var t: TTextTableItem);
var
  p: PTextTableItem;
begin
  p := GetOrigin(t.OriginText);
  if p = nil then
    begin
      new(p);
      p^ := t;
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.AddText(OriginText_, Category_: SystemString; Picked_: Boolean);
var
  p: PTextTableItem;
begin
  p := GetOrigin(OriginText_);
  if p = nil then
    begin
      new(p);
      p^.OriginText := OriginText_;
      p^.Category := Category_;
      p^.Picked := Picked_;
      p^.index := GetMaxIndexNo + 1;
      p^.DefineText := OriginText_;
      p^.TextStyle := tsNormalText;
      p^.OriginHash := FastHashPSystemString(@OriginText_);
      p^.DefineHash := FastHashPSystemString(@p^.DefineText);
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.AddPascalText(OriginText_, Category_: SystemString; Picked_: Boolean);
var
  p: PTextTableItem;
begin
  p := GetOrigin(OriginText_);
  if p = nil then
    begin
      new(p);
      p^.OriginText := OriginText_;
      p^.Category := Category_;
      p^.Picked := Picked_;
      p^.index := GetMaxIndexNo + 1;
      p^.DefineText := OriginText_;
      p^.TextStyle := tsPascalText;
      p^.OriginHash := FastHashPSystemString(@OriginText_);
      p^.DefineHash := FastHashPSystemString(@p^.DefineText);
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.AddPascalComment(OriginText_, Category_: SystemString; Picked_: Boolean);
var
  p: PTextTableItem;
begin
  p := GetOrigin(OriginText_);
  if p = nil then
    begin
      new(p);
      p^.OriginText := OriginText_;
      p^.Category := Category_;
      p^.Picked := Picked_;
      p^.index := GetMaxIndexNo + 1;
      p^.DefineText := OriginText_;
      p^.TextStyle := tsPascalComment;
      p^.OriginHash := FastHashPSystemString(@OriginText_);
      p^.DefineHash := FastHashPSystemString(@p^.DefineText);
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.AddCText(OriginText_, Category_: SystemString; Picked_: Boolean);
var
  p: PTextTableItem;
begin
  p := GetOrigin(OriginText_);
  if p = nil then
    begin
      new(p);
      p^.OriginText := OriginText_;
      p^.Category := Category_;
      p^.Picked := Picked_;
      p^.index := GetMaxIndexNo + 1;
      p^.DefineText := OriginText_;
      p^.TextStyle := tsCText;
      p^.OriginHash := FastHashPSystemString(@OriginText_);
      p^.DefineHash := FastHashPSystemString(@p^.DefineText);
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.AddCComment(OriginText_, Category_: SystemString; Picked_: Boolean);
var
  p: PTextTableItem;
begin
  p := GetOrigin(OriginText_);
  if p = nil then
    begin
      new(p);
      p^.OriginText := OriginText_;
      p^.Category := Category_;
      p^.Picked := Picked_;
      p^.index := GetMaxIndexNo + 1;
      p^.DefineText := OriginText_;
      p^.TextStyle := tsCComment;
      p^.OriginHash := FastHashPSystemString(@OriginText_);
      p^.DefineHash := FastHashPSystemString(@p^.DefineText);
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.AddDelphiFormText(OriginText_, Category_: SystemString; Picked_: Boolean);
var
  p: PTextTableItem;
begin
  p := GetOrigin(OriginText_);
  if p = nil then
    begin
      new(p);
      p^.OriginText := OriginText_;
      p^.Category := Category_;
      p^.Picked := Picked_;
      p^.index := GetMaxIndexNo + 1;
      p^.DefineText := OriginText_;
      p^.TextStyle := tsDFMText;
      p^.OriginHash := FastHashPSystemString(@OriginText_);
      p^.DefineHash := FastHashPSystemString(@p^.DefineText);
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.ChangeDefineText(index: Integer; newDefine: U_String);
var
  i: Integer;
  p: PTextTableItem;
begin
  newDefine := umlCharReplace(newDefine, #9, #32).Text;

  while (newDefine.Len > 0) and (CharIn(newDefine.Last, [#13, #10])) do
      newDefine.DeleteLast;

  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      if (p^.Picked) and (p^.index = index) then
        begin
          case p^.TextStyle of
            tsPascalText: p^.DefineText := TTextParsing.Translate_Text_To_Pascal_Decl(newDefine);
            tsPascalComment: p^.DefineText := TTextParsing.Translate_Text_To_Pascal_Decl_Comment(newDefine);
            tsCText: p^.DefineText := TTextParsing.Translate_Text_To_C_Decl(newDefine);
            tsCComment: p^.DefineText := TTextParsing.Translate_Text_To_C_Decl_Comment(newDefine);
            tsDFMText: p^.DefineText := TTextParsing.Translate_Text_To_Pascal_Decl_With_Unicode(newDefine);
            else p^.DefineText := newDefine;
          end;

          p^.DefineHash := FastHashPSystemString(@p^.DefineText);
        end;
    end;
end;

function TTextTable.ExistsIndex(index: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FList.Count - 1 do
    if index = PTextTableItem(FList[i])^.index then
        Exit;
  Result := False;
end;

function TTextTable.Search(OriginText_: SystemString): PTextTableItem;
var
  hash: THash;
  i: Integer;
  p: PTextTableItem;
begin
  hash := FastHashPSystemString(@OriginText_);
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      if (p^.OriginHash = hash) and (p^.OriginText = OriginText_) then
        begin
          Exit(p);
        end;
    end;
  Result := nil;
end;

procedure TTextTable.SaveToStream(stream: TCore_Stream);
var
  ms: TMS64;
  df: TDFE;
  i: Integer;
  p: PTextTableItem;
begin
  ms := TMS64.Create;
  df := TDFE.Create;

  df.WriteInteger(FList.Count);

  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      p^.SaveToStream(ms);
      ms.Position := 0;
      df.WriteStream(ms);
      ms.Clear;
    end;

  df.EncodeAsBRRC(stream);

  DisposeObject(ms);
  DisposeObject(df);
end;

procedure TTextTable.LoadFromStream(stream: TCore_Stream);
var
  ms: TMS64;
  df: TDFE;
  i, c: Integer;
  p: PTextTableItem;
begin
  Clear;

  ms := TMS64.Create;
  df := TDFE.Create;
  df.DecodeFrom(stream);

  c := df.Reader.ReadInteger;

  for i := 0 to c - 1 do
    begin
      new(p);
      df.Reader.ReadStream(ms);
      ms.Position := 0;
      p^.LoadFromStream(ms);
      ms.Clear;
      FList.Add(p);
    end;

  DisposeObject(ms);
  DisposeObject(df);
end;

procedure TTextTable.ExportToTextStream(stream: TCore_Stream);
var
  expList: THashList;
  i: Integer;
  p: PTextTableItem;
  ns: TCore_StringList;
  n: TPascalString;
begin
  expList := THashList.Create;
  ns := TCore_StringList.Create;
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      if p^.Picked then
        if not expList.Exists(p^.OriginText) then
          begin
            expList.Add(p^.OriginText, p, False);
            case p^.TextStyle of
              tsPascalText: ns.Add(Format('%d=%s', [p^.index, TTextParsing.Translate_Pascal_Decl_To_Text(p^.DefineText).Text]));
              tsCText: ns.Add(Format('%d=%s', [p^.index, TTextParsing.Translate_C_Decl_To_Text(p^.DefineText).Text]));
              tsPascalComment: ns.Add(Format('%d=%s', [p^.index, TTextParsing.Translate_Pascal_Decl_Comment_To_Text(p^.DefineText).Text]));
              tsCComment: ns.Add(Format('%d=%s', [p^.index, TTextParsing.Translate_C_Decl_Comment_To_Text(p^.DefineText).Text]));
              tsDFMText: ns.Add(Format('%d=%s', [p^.index, TTextParsing.Translate_Pascal_Decl_To_Text(p^.DefineText).Text]));
              else ns.Add(Format('%d=%s', [p^.index, p^.DefineText]));
            end;
          end;
    end;
  ns.SaveToStream(stream);
  DisposeObject(expList);
  DisposeObject(ns);
end;

procedure TTextTable.ImportFromTextStream(stream: TCore_Stream);
var
  ns: TCore_StringList;
  t: TTextParsing;
  CurrentItem: Integer;
  cp: Integer;
  nbPos, nePos: Integer;
  numText: U_String;
  Num: Integer;
  n: U_String;
begin
  ns := TCore_StringList.Create;
  ns.LoadFromStream(stream);
  t := TTextParsing.Create(ns.Text, TTextStyle.tsText, nil);

  cp := 1;
  n := '';
  Num := -1;
  CurrentItem := -1;
  while cp <= t.Len do
    begin
      if ((cp = 1) or (CharIn(t.GetChar(cp - 1), ns.LineBreak))) and (t.isNumber(cp)) then
        begin
          nbPos := cp;
          nePos := t.GetNumberEndPos(nbPos);
          numText := t.GetStr(nbPos, nePos);
          if CharIn(t.GetChar(nePos), ':=') then
            case umlGetNumTextType(numText) of
              ntUInt64, ntWord, ntByte, ntUInt:
                begin
                  Num := umlStrToInt(numText.Text, 0);
                  if n.Len >= length(ns.LineBreak) then
                      n.Len := n.Len - length(ns.LineBreak);
                  ChangeDefineText(CurrentItem, n);
                  n := '';
                  CurrentItem := Num;
                  cp := nePos + 1;
                  Continue;
                end;
            end;
        end;
      n := n.Text + t.GetChar(cp);
      inc(cp);
    end;
  if n.Len >= length(ns.LineBreak) then
      n.Len := n.Len - length(ns.LineBreak);
  ChangeDefineText(CurrentItem, n.Text);

  DisposeObject(ns);
  DisposeObject(t);
end;

end.

