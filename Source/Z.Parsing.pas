{ ****************************************************************************** }
{ * parsing imp                                                                * }
{ ****************************************************************************** }

unit Z.Parsing;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses Types,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core,
  Z.PascalStrings,
  Z.UPascalStrings,
  Z.ListEngine;

type
{$IFDEF FPC}
  TP_String = TUPascalString;
  TP_PString = PUPascalString;
  TP_SystemString = USystemString;
  TP_Char = USystemChar;
  TP_ArrayString = TUArrayPascalString;
  TP_OrdChar = TUOrdChar;
  TP_OrdChars = TUOrdChars;
{$ELSE FPC}
  TP_String = TPascalString;
  TP_PString = PPascalString;
  TP_SystemString = SystemString;
  TP_Char = SystemChar;
  TP_ArrayString = TArrayPascalString;
  TP_OrdChar = TOrdChar;
  TP_OrdChars = TOrdChars;
{$ENDIF FPC}
  TTextStyle = (tsPascal, tsC, tsText, tsXML);

  TTokenType = (ttTextDecl, ttComment, ttNumber, ttSymbol, ttAscii, ttSpecialSymbol, ttUnknow);
  TTokenTypes = set of TTokenType;

  TTokenStatistics = array [TTokenType] of Integer;

  TTextPos = record
    bPos, ePos: Integer;
    Text: TP_String;
  end;

  PTextPos = ^TTextPos;

  TTokenData = record
    bPos, ePos: Integer;
    LPos, LCPos: Integer;
    Text: TP_String;
    tokenType: TTokenType;
    Index: Integer;
    procedure Init;
  end;

  PTokenData = ^TTokenData;

  TTextPosList_Decl = TGenericsList<PTextPos>;
  TTokenDataList_Decl = TGenericsList<PTokenData>;

  TTextParsingCache = record
    CommentDecls, TextDecls: TTextPosList_Decl;
    TokenDataList: TTokenDataList_Decl;
    CharToken: array of PTokenData;
  end;

  TTextParsingData = record
    Cache: TTextParsingCache;
    Text: TP_String;
    L: Integer;
    property Len: Integer read L;
  end;

  TSymbolVector = TP_ArrayString;
  TSymbolMatrix = array of TSymbolVector;

  TTextParsing = class(TCore_Object)
  public
    TextStyle: TTextStyle;
    ParsingData: TTextParsingData;
    SymbolTable: TP_String;
    TokenStatistics: TTokenStatistics;
    SpecialSymbol: TListPascalString;
    RebuildCacheBusy: Boolean;

    { compare char }
    class function Char_is(c: TP_Char; SomeChars: array of TP_Char): Boolean; overload;
    class function Char_is(c: TP_Char; SomeChar: TP_Char): Boolean; overload;
    class function Char_is(c: TP_Char; s: TP_String): Boolean; overload;
    class function Char_is(c: TP_Char; p: TP_PString): Boolean; overload;
    class function Char_is(c: TP_Char; SomeCharsets: TP_OrdChars): Boolean; overload;
    class function Char_is(c: TP_Char; SomeCharset: TP_OrdChar): Boolean; overload;
    class function Char_is(c: TP_Char; SomeCharsets: TP_OrdChars; SomeChars: TP_String): Boolean; overload;
    class function Char_is(c: TP_Char; SomeCharsets: TP_OrdChars; p: TP_PString): Boolean; overload;
    function ComparePosStr(cOffset: Integer; t: TP_String): Boolean; overload;
    function ComparePosStr(cOffset: Integer; p: TP_PString): Boolean; overload;
    function ComparePosChar(cOffset: Integer; c: TP_Char): Boolean; overload;
    function ComparePosChar(cOffset: Integer; c: TP_Char; ignoreCase_: Boolean): Boolean; overload;

    { compare comment and text declaration: TokenCache }
    function CompareCommentGetEndPos(cOffset: Integer): Integer;
    function CompareTextDeclGetEndPos(cOffset: Integer): Integer;

    { rebuild }
    procedure RebuildParsingCache;
    procedure RebuildText;
    procedure RebuildToken;
    function FastRebuildTokenTo(): TP_String;

    { context }
    function GetContextBeginPos(cOffset: Integer): Integer;
    function GetContextEndPos(cOffset: Integer): Integer;

    { special symbol }
    function isSpecialSymbol(cOffset: Integer): Boolean; overload;
    function isSpecialSymbol(cOffset: Integer; var speicalSymbolEndPos: Integer): Boolean; overload;
    function GetSpecialSymbolEndPos(cOffset: Integer): Integer;

    { number decl }
    function isNumber(cOffset: Integer): Boolean; overload;
    function isNumber(cOffset: Integer; var NumberBegin: Integer; var IsHex: Boolean): Boolean; overload;
    function GetNumberEndPos(cOffset: Integer): Integer;

    { text }
    function isTextDecl(cOffset: Integer): Boolean;
    function GetTextDeclEndPos(cOffset: Integer): Integer;
    function GetTextDeclBeginPos(cOffset: Integer): Integer;
    function GetTextBody(Text_: TP_String): TP_String;
    function GetTextDeclPos(cOffset: Integer; var charBeginPos, charEndPos: Integer): Boolean;

    { symbol }
    function isSymbol(cOffset: Integer): Boolean;
    function GetSymbolEndPos(cOffset: Integer): Integer;

    { ascii }
    function isAscii(cOffset: Integer): Boolean;
    function GetAsciiBeginPos(cOffset: Integer): Integer;
    function GetAsciiEndPos(cOffset: Integer): Integer;

    { comment }
    function isComment(cOffset: Integer): Boolean;
    function GetCommentEndPos(cOffset: Integer): Integer;
    function GetCommentBeginPos(cOffset: Integer): Integer;
    function GetCommentPos(cOffset: Integer; var charBeginPos, charEndPos: Integer): Boolean;
    function GetDeletedCommentText: TP_String;

    { text }
    function isTextOrComment(cOffset: Integer): Boolean;
    function isCommentOrText(cOffset: Integer): Boolean;

    { word }
    class function isWordSplitChar(c: TP_Char): Boolean; overload;
    class function isWordSplitChar(c: TP_Char; Split_Token_Char: TP_String): Boolean; overload;
    class function isWordSplitChar(c: TP_Char; Include_C_0_to_32: Boolean; Split_Token_Char: TP_String): Boolean; overload;
    function GetWordBeginPos(cOffset: Integer; Split_Token_Char: TP_String): Integer; overload;
    function GetWordBeginPos(cOffset: Integer): Integer; overload;
    function GetWordBeginPos(cOffset: Integer; Include_C_0_to_32: Boolean; Split_Token_Char: TP_String): Integer; overload;
    function GetWordEndPos(cOffset: Integer; Split_Token_Char: TP_String): Integer; overload;
    function GetWordEndPos(cOffset: Integer): Integer; overload;
    function GetWordEndPos(cOffset: Integer; BeginSplitCharSet, EndSplitCharSet: TP_String): Integer; overload;
    function GetWordEndPos(cOffset: Integer; Include_C_0_to_32: Boolean; BeginSplitCharSet: TP_String; EndDefaultChar: Boolean; EndSplitCharSet: TP_String): Integer; overload;

    { sniffing }
    function SniffingNextChar(cOffset: Integer; declChar: TP_String): Boolean; overload;
    function SniffingNextChar(cOffset: Integer; declChar: TP_String; out OutPos: Integer): Boolean; overload;

    { split }
    function SplitChar(cOffset: Integer; var LastPos: Integer; Include_C_0_to_32: Boolean; Split_Token_Char, Split_End_Token_Char: TP_String; var SplitOutput: TSymbolVector): Integer; overload;
    function SplitChar(cOffset: Integer; var LastPos: Integer; Split_Token_Char, Split_End_Token_Char: TP_String; var SplitOutput: TSymbolVector): Integer; overload;
    function SplitChar(cOffset: Integer; Split_Token_Char, Split_End_Token_Char: TP_String; var SplitOutput: TSymbolVector): Integer; overload;
    function SplitString(cOffset: Integer; var LastPos: Integer; SplitTokenS, SplitEndTokenS: TP_String; var SplitOutput: TSymbolVector): Integer; overload;
    function SplitString(cOffset: Integer; SplitTokenS, SplitEndTokenS: TP_String; var SplitOutput: TSymbolVector): Integer; overload;

    { token }
    function CompareTokenText(cOffset: Integer; t: TP_String): Boolean;
    function CompareTokenChar(cOffset: Integer; c: array of TP_Char): Boolean;
    function GetToken(cOffset: Integer): PTokenData;
    property TokenPos[cOffset: Integer]: PTokenData read GetToken;
    property CharToken[cOffset: Integer]: PTokenData read GetToken;
    function GetTokenIndex(t: TTokenType; idx: Integer): PTokenData;
    property TokenIndex[t: TTokenType; idx: Integer]: PTokenData read GetTokenIndex;
    function TokenCount: Integer; overload;
    function TokenCountT(t: TTokenTypes): Integer; overload;
    function GetTokens(idx: Integer): PTokenData;
    property Tokens[idx: Integer]: PTokenData read GetTokens; default;
    property Token[idx: Integer]: PTokenData read GetTokens;
    property Count: Integer read TokenCount;
    function FirstToken: PTokenData;
    function LastToken: PTokenData;
    function NextToken(p: PTokenData): PTokenData;
    function PrevToken(p: PTokenData): PTokenData;
    function TokenCombine(bTokenI, eTokenI: Integer; acceptT: TTokenTypes): TP_String; overload;
    function TokenCombine(bTokenI, eTokenI: Integer): TP_String; overload;
    function Combine(bTokenI, eTokenI: Integer; acceptT: TTokenTypes): TP_String; overload;
    function Combine(bTokenI, eTokenI: Integer): TP_String; overload;

    { token probe Left }
    function TokenProbeL(startI: Integer; acceptT: TTokenTypes): PTokenData; overload;
    function TokenProbeL(startI: Integer; t: TP_String): PTokenData; overload;
    function TokenProbeL(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData; overload;
    function TokenProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2: TP_String): PTokenData; overload;
    function TokenProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2, t3: TP_String): PTokenData; overload;
    function TokenProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4: TP_String): PTokenData; overload;
    function TokenProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4, t5: TP_String): PTokenData; overload;

    { token probe Right }
    function TokenProbeR(startI: Integer; acceptT: TTokenTypes): PTokenData; overload;
    function TokenProbeR(startI: Integer; t: TP_String): PTokenData; overload;
    function TokenProbeR(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData; overload;
    function TokenProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2: TP_String): PTokenData; overload;
    function TokenProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2, t3: TP_String): PTokenData; overload;
    function TokenProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4: TP_String): PTokenData; overload;
    function TokenProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4, t5: TP_String): PTokenData; overload;

    { probe Left }
    function ProbeL(startI: Integer; acceptT: TTokenTypes): PTokenData; overload;
    function ProbeL(startI: Integer; t: TP_String): PTokenData; overload;
    function ProbeL(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData; overload;
    function ProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2: TP_String): PTokenData; overload;
    function ProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2, t3: TP_String): PTokenData; overload;
    function ProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4: TP_String): PTokenData; overload;
    function ProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4, t5: TP_String): PTokenData; overload;
    function LProbe(startI: Integer; acceptT: TTokenTypes): PTokenData; overload;
    function LProbe(startI: Integer; t: TP_String): PTokenData; overload;
    function LProbe(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData; overload;
    function LProbe(startI: Integer; acceptT: TTokenTypes; t1, t2: TP_String): PTokenData; overload;
    function LProbe(startI: Integer; acceptT: TTokenTypes; t1, t2, t3: TP_String): PTokenData; overload;
    function LProbe(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4: TP_String): PTokenData; overload;
    function LProbe(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4, t5: TP_String): PTokenData; overload;

    { token Right }
    function ProbeR(startI: Integer; acceptT: TTokenTypes): PTokenData; overload;
    function ProbeR(startI: Integer; t: TP_String): PTokenData; overload;
    function ProbeR(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData; overload;
    function ProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2: TP_String): PTokenData; overload;
    function ProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2, t3: TP_String): PTokenData; overload;
    function ProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4: TP_String): PTokenData; overload;
    function ProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4, t5: TP_String): PTokenData; overload;
    function RProbe(startI: Integer; acceptT: TTokenTypes): PTokenData; overload;
    function RProbe(startI: Integer; t: TP_String): PTokenData; overload;
    function RProbe(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData; overload;
    function RProbe(startI: Integer; acceptT: TTokenTypes; t1, t2: TP_String): PTokenData; overload;
    function RProbe(startI: Integer; acceptT: TTokenTypes; t1, t2, t3: TP_String): PTokenData; overload;
    function RProbe(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4: TP_String): PTokenData; overload;
    function RProbe(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4, t5: TP_String): PTokenData; overload;

    { free to match all strings from Token[StartIndex] left to right, including any symbols. return token }
    function TokenFullStringProbe(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData;
    function StringProbe(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData;

    { symbol Indent probe for end indent }
    function IndentSymbolEndProbeR(startI: Integer; indent_begin_symbol, indent_end_symbol: TP_String): PTokenData;

    { symbol Indent probe for begin indent }
    function IndentSymbolBeginProbeL(startI: Integer; indent_begin_symbol, indent_end_symbol: TP_String): PTokenData;

    { segmention text as symbol vector, L = output }
    function DetectSymbolVector: Boolean;
    function FillSymbolVector(L: TPascalStringList): Boolean; overload;
    function FillSymbolVector: TSymbolVector; overload;

    { segmention text as symbol matrix }
    function FillSymbolMatrix(W, H: Integer; var symbolMatrix: TSymbolMatrix): Boolean;

    { misc }
    function GetText(bPos, ePos: Integer): TP_String; overload;
    function GetStr(bPos, ePos: Integer): TP_String; overload;
    function GetStr(tp: TTextPos): TP_String; overload;
    function GetWord(cOffset: Integer): TP_String; overload;
    function GetPoint(cOffset: Integer): TPoint;
    function GetChar(cOffset: Integer): TP_Char;
    property Len: Integer read ParsingData.L;
    property ParseText: TP_String read ParsingData.Text;
    property Text: TP_String read ParsingData.Text;
    procedure DeletePos(bPos, ePos: Integer); overload;
    procedure DeletePos(tp: TTextPos); overload;
    procedure DeletedComment;
    procedure InsertTextBlock(bPos, ePos: Integer; InsertText_: TP_String); overload;
    procedure InsertTextBlock(tp: TTextPos; InsertText_: TP_String); overload;
    function SearchWordBody(initPos: Integer; wordInfo: TP_String; var OutPos: TTextPos): Boolean;

    { string declaration }
    class function Translate_Pascal_Decl_To_Text(Decl: TP_String): TP_String;
    class function Translate_Text_To_Pascal_Decl(Decl: TP_String): TP_String;
    class function Translate_Text_To_Pascal_Decl_With_Unicode(Decl: TP_String): TP_String;
    class function Translate_C_Decl_To_Text(Decl: TP_String): TP_String;
    class function Translate_Text_To_C_Decl(Decl: TP_String): TP_String;
    class function Translate_Text_To_XML_Decl(Decl: TP_String): TP_String;
    class function Translate_XML_Decl_To_Text(Decl: TP_String): TP_String;

    { comment declaration }
    class function Translate_Pascal_Decl_Comment_To_Text(Decl: TP_String): TP_String;
    class function Translate_Text_To_Pascal_Decl_Comment(Decl: TP_String): TP_String;
    class function Translate_C_Decl_Comment_To_Text(Decl: TP_String): TP_String;
    class function Translate_Text_To_C_Decl_Comment(Decl: TP_String): TP_String;
    class function Translate_XML_Decl_Comment_To_Text(Decl: TP_String): TP_String;
    class function Translate_Text_To_XML_Decl_Comment(Decl: TP_String): TP_String;

    { structor }
    constructor Create(Text_: TP_String; Style_: TTextStyle; SpecialSymbol_: TListPascalString; SpacerSymbol_: TP_SystemString); overload;
    constructor Create(Text_: TP_String; Style_: TTextStyle; SpecialSymbol_: TListPascalString); overload;
    constructor Create(Text_: TP_String; Style_: TTextStyle); overload;
    constructor Create(Text_: TP_String); overload;
    destructor Destroy; override;

    { external }
    procedure Init; virtual;
    function Parsing: Boolean; virtual;

    { debug }
    procedure Print;
  end;

  TTextParsingClass = class of TTextParsing;

const
  C_SpacerSymbol = #44#43#45#42#47#40#41#59#58#61#35#64#94#38#37#33#34#91#93#60#62#63#123#125#39#36#124;

var
  SpacerSymbol: TAtomString;

implementation

uses Z.Status, Z.UnicodeMixedLib, TypInfo;

type
  TCTranslateStruct = record
    s: TP_Char;
    c: TP_SystemString;
  end;

const
  NullTokenStatistics: TTokenStatistics = (0, 0, 0, 0, 0, 0, 0);
  CTranslateTable: array [0 .. 11] of TCTranslateStruct = (
    (s: #007; c: '\a'),
    (s: #008; c: '\b'),
    (s: #012; c: '\f'),
    (s: #010; c: '\n'),
    (s: #013; c: '\r'),
    (s: #009; c: '\t'),
    (s: #011; c: '\v'),
    (s: #092; c: '\\'),
    (s: #063; c: '\?'),
    (s: #039; c: '\'#39),
    (s: #034; c: '\"'),
    (s: #000; c: '\0'));

procedure TTokenData.Init;
begin
  bPos := -1;
  ePos := -1;
  LPos := -1;
  LCPos := -1;
  Text := '';
  tokenType := ttUnknow;
  Index := -1;
end;

class function TTextParsing.Char_is(c: TP_Char; SomeChars: array of TP_Char): Boolean;
begin
  Result := {$IFDEF FPC}UCharIn{$ELSE FPC}CharIn{$ENDIF FPC}(c, SomeChars);
end;

class function TTextParsing.Char_is(c: TP_Char; SomeChar: TP_Char): Boolean;
begin
  Result := {$IFDEF FPC}UCharIn{$ELSE FPC}CharIn{$ENDIF FPC}(c, SomeChar);
end;

class function TTextParsing.Char_is(c: TP_Char; s: TP_String): Boolean;
begin
  Result := {$IFDEF FPC}UCharIn{$ELSE FPC}CharIn{$ENDIF FPC}(c, s);
end;

class function TTextParsing.Char_is(c: TP_Char; p: TP_PString): Boolean;
begin
  Result := {$IFDEF FPC}UCharIn{$ELSE FPC}CharIn{$ENDIF FPC}(c, p);
end;

class function TTextParsing.Char_is(c: TP_Char; SomeCharsets: TP_OrdChars): Boolean;
begin
  Result := {$IFDEF FPC}UCharIn{$ELSE FPC}CharIn{$ENDIF FPC}(c, SomeCharsets);
end;

class function TTextParsing.Char_is(c: TP_Char; SomeCharset: TP_OrdChar): Boolean;
begin
  Result := {$IFDEF FPC}UCharIn{$ELSE FPC}CharIn{$ENDIF FPC}(c, SomeCharset);
end;

class function TTextParsing.Char_is(c: TP_Char; SomeCharsets: TP_OrdChars; SomeChars: TP_String): Boolean;
begin
  Result := {$IFDEF FPC}UCharIn{$ELSE FPC}CharIn{$ENDIF FPC}(c, SomeCharsets, SomeChars);
end;

class function TTextParsing.Char_is(c: TP_Char; SomeCharsets: TP_OrdChars; p: TP_PString): Boolean;
begin
  Result := {$IFDEF FPC}UCharIn{$ELSE FPC}CharIn{$ENDIF FPC}(c, SomeCharsets, p);
end;

function TTextParsing.ComparePosStr(cOffset: Integer; t: TP_String): Boolean;
begin
  Result := ParsingData.Text.ComparePos(cOffset, t);
end;

function TTextParsing.ComparePosStr(cOffset: Integer; p: TP_PString): Boolean;
begin
  Result := ParsingData.Text.ComparePos(cOffset, p);
end;

function TTextParsing.ComparePosChar(cOffset: Integer; c: TP_Char): Boolean;
begin
  Result := ParsingData.Text[cOffset] = c;
end;

function TTextParsing.ComparePosChar(cOffset: Integer; c: TP_Char; ignoreCase_: Boolean): Boolean;
begin
  if ignoreCase_ then
      Result := ComparePosStr(cOffset, c)
  else
      Result := ComparePosChar(cOffset, c);
end;

function TTextParsing.CompareCommentGetEndPos(cOffset: Integer): Integer;
var
  L: Integer;
  cPos: Integer;
  p: PTokenData;
  indent_num: Integer;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          if p^.tokenType = TTokenType.ttComment then
              Result := p^.ePos
          else
              Result := cOffset;
          exit;
        end;
    end;

  L := ParsingData.L;
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      cPos := L;

  Result := cPos;

  if (TextStyle in [tsPascal, tsC]) and (ComparePosStr(Result, '//')) then
    begin
      inc(Result, 2);
      while not Char_is(ParsingData.Text[Result], [#13, #10]) do
        begin
          if Result + 1 > L then
              Break;
          inc(Result);
        end;
    end
  else if (TextStyle = tsC) and (ComparePosChar(Result, '#')) then
    begin
      inc(Result, 1);
      while not Char_is(ParsingData.Text[Result], [#13, #10]) do
        begin
          if Result + 1 > L then
              Break;
          inc(Result);
        end;
    end
  else if (TextStyle = tsC) and (ComparePosStr(Result, '/*')) then
    begin
      inc(Result, 2);
      while not ComparePosStr(Result, '*/') do
        begin
          if Result + 1 > L then
              Break;
          inc(Result);
        end;
      inc(Result, 2);
    end
  else if (TextStyle = tsPascal) and (ComparePosChar(Result, '{')) then
    begin
      inc(Result, 1);
      while ParsingData.Text[Result] <> '}' do
        begin
          if Result + 1 > L then
              Break;
          inc(Result);
        end;
      inc(Result, 1);
    end
  else if (TextStyle = tsPascal) and (ComparePosStr(Result, '(*')) then
    begin
      inc(Result, 2);
      while not ComparePosStr(Result, '*)') do
        begin
          if Result + 1 > L then
              Break;
          inc(Result);
        end;
      inc(Result, 2);
    end
  else if (TextStyle = tsXML) and (ComparePosStr(Result, '<!--')) then
    begin
      inc(Result, 4);
      while not ComparePosStr(Result, '-->') do
        begin
          if Result + 1 > L then
              Break;
          inc(Result);
        end;
      inc(Result, 3);
    end
  else if (TextStyle = tsXML) and (ComparePosStr(Result, '<![CDATA[')) then
    begin
      inc(Result, 9);
      while not ComparePosStr(Result, ']]>') do
        begin
          if Result + 1 > L then
              Break;
          inc(Result);
        end;
      inc(Result, 3);
    end
  else if (TextStyle = tsXML) and (ComparePosStr(Result, '<!DOCTYPE')) then
    begin
      inc(Result, 9);
      indent_num := 1;
      while Result < L do
        begin
          if ComparePosChar(Result, '<') then
              inc(indent_num)
          else if ComparePosChar(Result, '>') then
              dec(indent_num);
          inc(Result);
          if indent_num = 0 then
              Break;
        end;
    end;
  if Result > L + 1 then
      Result := L + 1;
end;

function TTextParsing.CompareTextDeclGetEndPos(cOffset: Integer): Integer;
var
  L: Integer;
  cPos: Integer;
  tmpPos: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          if p^.tokenType = TTokenType.ttTextDecl then
              Result := p^.ePos
          else
              Result := cOffset;
          exit;
        end;
    end;

  L := ParsingData.L;
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      cPos := L;

  if (cPos + 1 < L) and (TextStyle = tsPascal) and (ParsingData.Text[cPos] = #39) then
    begin
      if ComparePosStr(cPos, #39#39#39#39) then
        begin
          cPos := CompareTextDeclGetEndPos(cPos + 4);
          exit(cPos);
        end;
      inc(cPos, 1);
      while ParsingData.Text[cPos] <> #39 do
        begin
          if cPos + 1 > L then
              Break;
          if ParsingData.Text[cPos] = #10 then
              exit(cPos);
          inc(cPos);
        end;
      inc(cPos, 1);
    end;

  if (cPos + 1 < L) and (TextStyle = tsC) and (ParsingData.Text[cPos] = #39) then
    begin
      inc(cPos, 1);
      while ParsingData.Text[cPos] <> #39 do
        begin
          if ComparePosStr(cPos, '\' + #39) then
              inc(cPos, 1);
          if cPos + 1 > L then
              Break;
          if ParsingData.Text[cPos] = #10 then
              exit(cPos);
          inc(cPos);
        end;
      inc(cPos, 1);
    end;

  if (cPos + 1 < L) and (TextStyle = tsC) and (ParsingData.Text[cPos] = '"') then
    begin
      inc(cPos, 1);
      while ParsingData.Text[cPos] <> '"' do
        begin
          if ComparePosStr(cPos, '\"') then
              inc(cPos, 1);
          if cPos + 1 > L then
              Break;
          if ParsingData.Text[cPos] = #10 then
              exit(cPos);
          inc(cPos);
        end;
      inc(cPos, 1);
    end;

  if (cPos + 1 < L) and (TextStyle = tsXML) and (ParsingData.Text[cPos] = '"') then
    begin
      inc(cPos, 1);
      while ParsingData.Text[cPos] <> '"' do
        begin
          if cPos + 1 > L then
              Break;
          if ParsingData.Text[cPos] = #10 then
              exit(cPos);
          inc(cPos);
        end;
      inc(cPos, 1);
    end;

  if (cPos + 1 < L) and (TextStyle = tsPascal) and (ParsingData.Text[cPos] = '#') then
    begin
      repeat
        inc(cPos, 1);
        while isWordSplitChar(ParsingData.Text[cPos], True, SymbolTable) do
          begin
            if cPos + 1 > L then
                exit(cPos);
            inc(cPos);
          end;
        while Char_is(ParsingData.Text[cPos], [{$IFDEF FPC}ucHex{$ELSE FPC}cHex{$ENDIF FPC}], '$') do
          begin
            if cPos + 1 > L then
                exit(cPos);
            inc(cPos);
          end;
        tmpPos := cPos;
        while isWordSplitChar(ParsingData.Text[cPos], True, SymbolTable) do
          begin
            if cPos + 1 > L then
                exit(cPos);
            inc(cPos);
          end;
      until not ComparePosStr(cPos, '#');
      cPos := CompareTextDeclGetEndPos(tmpPos);
    end;

  Result := cPos;
end;

procedure TTextParsing.RebuildParsingCache;
var
  i, j: Integer;
  L: Integer;
  bPos: Integer;
  ePos: Integer;
  textPosPtr: PTextPos;
  TokenDataPtr: PTokenData;
begin
  // clean cache
  RebuildCacheBusy := True;
  if ParsingData.Cache.CommentDecls <> nil then
    begin
      for i := 0 to ParsingData.Cache.CommentDecls.Count - 1 do
        begin
          ParsingData.Cache.CommentDecls[i]^.Text := '';
          Dispose(ParsingData.Cache.CommentDecls[i]);
        end;
      DisposeObject(ParsingData.Cache.CommentDecls);
      ParsingData.Cache.CommentDecls := nil;
    end;

  if ParsingData.Cache.TextDecls <> nil then
    begin
      for i := 0 to ParsingData.Cache.TextDecls.Count - 1 do
        begin
          ParsingData.Cache.TextDecls[i]^.Text := '';
          Dispose(ParsingData.Cache.TextDecls[i]);
        end;
      DisposeObject(ParsingData.Cache.TextDecls);
      ParsingData.Cache.TextDecls := nil;
    end;

  if ParsingData.Cache.TokenDataList <> nil then
    begin
      for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
        begin
          ParsingData.Cache.TokenDataList[i]^.Text := '';
          Dispose(ParsingData.Cache.TokenDataList[i]);
        end;
      DisposeObject(ParsingData.Cache.TokenDataList);
      ParsingData.Cache.TokenDataList := nil;
    end;

  TokenStatistics := NullTokenStatistics;
  ParsingData.Cache.CommentDecls := TTextPosList_Decl.Create;
  ParsingData.Cache.TextDecls := TTextPosList_Decl.Create;
  ParsingData.Cache.TokenDataList := TTokenDataList_Decl.Create;
  SetLength(ParsingData.Cache.CharToken, 0);

  // rebuild comment and text
  L := ParsingData.L;
  bPos := 1;
  ePos := bPos;
  while (bPos <= L) do
    begin
      ePos := CompareCommentGetEndPos(bPos);
      if ePos > bPos then
        begin
          new(textPosPtr);
          textPosPtr^.bPos := bPos;
          textPosPtr^.ePos := ePos;
          textPosPtr^.Text := GetStr(textPosPtr^);
          ParsingData.Cache.CommentDecls.Add(textPosPtr);
          bPos := ePos;
        end
      else
        begin
          ePos := CompareTextDeclGetEndPos(bPos);
          if ePos > bPos then
            begin
              new(textPosPtr);
              textPosPtr^.bPos := bPos;
              textPosPtr^.ePos := ePos;
              textPosPtr^.Text := GetStr(textPosPtr^);
              ParsingData.Cache.TextDecls.Add(textPosPtr);
              bPos := ePos;
            end
          else
            begin
              inc(bPos);
              ePos := bPos;
            end;
        end;
    end;

  // rebuild token
  bPos := 1;
  ePos := bPos;
  TokenDataPtr := nil;
  while bPos <= L do
    begin
      if isSpecialSymbol(bPos, ePos) then
        begin
          new(TokenDataPtr);
          TokenDataPtr^.Init;
          TokenDataPtr^.bPos := bPos;
          TokenDataPtr^.ePos := ePos;
          TokenDataPtr^.Text := GetStr(bPos, ePos);
          TokenDataPtr^.tokenType := ttSpecialSymbol;
          TokenDataPtr^.Index := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(TokenDataPtr);
          inc(TokenStatistics[TokenDataPtr^.tokenType]);
          bPos := ePos
        end
      else if isTextDecl(bPos) then
        begin
          ePos := GetTextDeclEndPos(bPos);
          new(TokenDataPtr);
          TokenDataPtr^.Init;
          TokenDataPtr^.bPos := bPos;
          TokenDataPtr^.ePos := ePos;
          TokenDataPtr^.Text := GetStr(bPos, ePos);
          TokenDataPtr^.tokenType := ttTextDecl;
          TokenDataPtr^.Index := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(TokenDataPtr);
          inc(TokenStatistics[TokenDataPtr^.tokenType]);
          bPos := ePos
        end
      else if isComment(bPos) then
        begin
          ePos := GetCommentEndPos(bPos);
          new(TokenDataPtr);
          TokenDataPtr^.Init;
          TokenDataPtr^.bPos := bPos;
          TokenDataPtr^.ePos := ePos;
          TokenDataPtr^.Text := GetStr(bPos, ePos);
          TokenDataPtr^.tokenType := ttComment;
          TokenDataPtr^.Index := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(TokenDataPtr);
          inc(TokenStatistics[TokenDataPtr^.tokenType]);
          bPos := ePos;
        end
      else if isNumber(bPos) then
        begin
          ePos := GetNumberEndPos(bPos);
          new(TokenDataPtr);
          TokenDataPtr^.Init;
          TokenDataPtr^.bPos := bPos;
          TokenDataPtr^.ePos := ePos;
          TokenDataPtr^.Text := GetStr(bPos, ePos);
          TokenDataPtr^.tokenType := ttNumber;
          TokenDataPtr^.Index := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(TokenDataPtr);
          inc(TokenStatistics[TokenDataPtr^.tokenType]);
          bPos := ePos;
        end
      else if isSymbol(bPos) then
        begin
          ePos := GetSymbolEndPos(bPos);
          new(TokenDataPtr);
          TokenDataPtr^.Init;
          TokenDataPtr^.bPos := bPos;
          TokenDataPtr^.ePos := ePos;
          TokenDataPtr^.Text := GetStr(bPos, ePos);
          TokenDataPtr^.tokenType := ttSymbol;
          TokenDataPtr^.Index := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(TokenDataPtr);
          inc(TokenStatistics[TokenDataPtr^.tokenType]);
          bPos := ePos;
        end
      else if isAscii(bPos) then
        begin
          ePos := GetAsciiEndPos(bPos);
          new(TokenDataPtr);
          TokenDataPtr^.Init;
          TokenDataPtr^.bPos := bPos;
          TokenDataPtr^.ePos := ePos;
          TokenDataPtr^.Text := GetStr(bPos, ePos);
          TokenDataPtr^.tokenType := ttAscii;
          TokenDataPtr^.Index := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(TokenDataPtr);
          inc(TokenStatistics[TokenDataPtr^.tokenType]);
          bPos := ePos;
        end
      else
        begin
          ePos := bPos + 1;
          if (TokenDataPtr = nil) or (TokenDataPtr^.tokenType <> ttUnknow) then
            begin
              new(TokenDataPtr);
              TokenDataPtr^.Init;
              TokenDataPtr^.bPos := bPos;
              TokenDataPtr^.ePos := ePos;
              TokenDataPtr^.Text := GetStr(bPos, ePos);
              TokenDataPtr^.tokenType := ttUnknow;
              TokenDataPtr^.Index := ParsingData.Cache.TokenDataList.Count;
              ParsingData.Cache.TokenDataList.Add(TokenDataPtr);
              inc(TokenStatistics[TokenDataPtr^.tokenType]);
            end
          else
            begin
              TokenDataPtr^.ePos := ePos;
              TokenDataPtr^.Text.Append(GetChar(bPos));
            end;
          bPos := ePos;
        end;
    end;

  SetLength(ParsingData.Cache.CharToken, L);
  for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      TokenDataPtr := ParsingData.Cache.TokenDataList[i];
      for j := TokenDataPtr^.bPos to TokenDataPtr^.ePos - 1 do
          ParsingData.Cache.CharToken[j - 1] := TokenDataPtr;
    end;

  TokenDataPtr := nil;
  L := 1;
  j := 1;
  for i := 1 to ParsingData.Text.L do
    begin
      if ParsingData.Cache.CharToken[i - 1] <> TokenDataPtr then
        begin
          TokenDataPtr := ParsingData.Cache.CharToken[i - 1];
          TokenDataPtr^.LPos := L;
          TokenDataPtr^.LCPos := j;
        end;
      if Char_is(ParsingData.Text[i], #10) then
        begin
          inc(L);
          j := 1;
        end
      else if Char_is(ParsingData.Text[i], #13) then
          j := 1
      else
          inc(j);
    end;
  RebuildCacheBusy := False;
end;

procedure TTextParsing.RebuildText;
  procedure Recompute(bPos, d: Integer);
  var
    i: Integer;
    p: PTextPos;
  begin
    for i := 0 to ParsingData.Cache.TextDecls.Count - 1 do
      begin
        p := PTextPos(ParsingData.Cache.TextDecls[i]);
        if bPos < p^.bPos then
          begin
            p^.bPos := p^.bPos - d;
            p^.ePos := p^.ePos - d;
          end;
      end;
    for i := 0 to ParsingData.Cache.CommentDecls.Count - 1 do
      begin
        p := PTextPos(ParsingData.Cache.CommentDecls[i]);
        if bPos < p^.bPos then
          begin
            p^.bPos := p^.bPos - d;
            p^.ePos := p^.ePos - d;
          end;
      end;
  end;

var
  p: PTextPos;
  i: Integer;
begin
  for i := 0 to ParsingData.Cache.TextDecls.Count - 1 do
    begin
      p := PTextPos(ParsingData.Cache.TextDecls[i]);
      if p^.ePos - p^.bPos <> (p^.Text.L) then
          Recompute(p^.bPos, (p^.ePos - p^.bPos) - p^.Text.L);

      ParsingData.Text := GetStr(1, p^.bPos) + p^.Text + GetStr(p^.ePos, ParsingData.Text.L + 1);
      ParsingData.L := ParsingData.Text.L;
      p^.ePos := p^.bPos + p^.Text.L;
    end;
  for i := 0 to ParsingData.Cache.CommentDecls.Count - 1 do
    begin
      p := PTextPos(ParsingData.Cache.CommentDecls[i]);
      if p^.ePos - p^.bPos <> (p^.Text.L) then
          Recompute(p^.bPos, (p^.ePos - p^.bPos) - p^.Text.L);

      ParsingData.Text := GetStr(1, p^.bPos) + p^.Text + GetStr(p^.ePos, ParsingData.Text.L + 1);
      ParsingData.L := ParsingData.Text.L;
      p^.ePos := p^.bPos + p^.Text.L;
    end;

  RebuildParsingCache;
end;

procedure TTextParsing.RebuildToken;
var
  p: PTokenData;
  i, j: Integer;
begin
  ParsingData.Text := '';

  // sum
  j := 0;
  for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
      inc(j, ParsingData.Cache.TokenDataList[i]^.Text.L);

  // extract
  ParsingData.Text.L := j;
  j := 0;
  for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[i]);
      if p^.Text.L > 0 then
        begin
          CopyPtr(@p^.Text.buff[0], @ParsingData.Text.buff[j], p^.Text.L * SizeOf(TP_Char));
          inc(j, p^.Text.L);
        end;
    end;
  ParsingData.L := ParsingData.Text.L;
  RebuildParsingCache;
end;

function TTextParsing.FastRebuildTokenTo(): TP_String;
var
  p: PTokenData;
  i, j: Integer;
begin
  Result := '';

  // sum
  j := 0;
  for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
      inc(j, ParsingData.Cache.TokenDataList[i]^.Text.L);

  // extract
  Result.L := j;
  j := 0;
  for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[i]);
      if p^.Text.L > 0 then
        begin
          CopyPtr(@p^.Text.buff[0], @Result.buff[j], p^.Text.L * SizeOf(TP_Char));
          inc(j, p^.Text.L);
        end;
    end;
end;

function TTextParsing.GetContextBeginPos(cOffset: Integer): Integer;
var
  p: PTokenData;
begin
  Result := cOffset;
  p := TokenPos[cOffset];
  if p = nil then
      exit;
  Result := p^.bPos;
end;

function TTextParsing.GetContextEndPos(cOffset: Integer): Integer;
var
  p: PTokenData;
begin
  Result := cOffset;
  p := TokenPos[cOffset];
  if p = nil then
      exit;
  Result := p^.ePos;
end;

function TTextParsing.isSpecialSymbol(cOffset: Integer): Boolean;
var
  ePos: Integer;
begin
  Result := isSpecialSymbol(cOffset, ePos);
end;

function TTextParsing.isSpecialSymbol(cOffset: Integer; var speicalSymbolEndPos: Integer): Boolean;
var
  i, EP: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          Result := p^.tokenType = TTokenType.ttSpecialSymbol;
          if Result then
              speicalSymbolEndPos := p^.ePos;
          exit;
        end;
    end;

  Result := False;
  speicalSymbolEndPos := cOffset;

  if SpecialSymbol.Count = 0 then
      exit;

  if isComment(cOffset) then
      exit;

  if isTextDecl(cOffset) then
      exit;

  speicalSymbolEndPos := cOffset;
  for i := 0 to SpecialSymbol.Count - 1 do
    if ComparePosStr(cOffset, SpecialSymbol.Items[i]) then
      begin
        EP := cOffset + SpecialSymbol[i].L;
        if EP > speicalSymbolEndPos then
            speicalSymbolEndPos := EP;
        Result := True;
      end;
end;

function TTextParsing.GetSpecialSymbolEndPos(cOffset: Integer): Integer;
begin
  if not isSpecialSymbol(cOffset, Result) then
      Result := cOffset;
end;

function TTextParsing.isNumber(cOffset: Integer): Boolean;
var
  tmp: Integer;
  IsHex: Boolean;
begin
  Result := isNumber(cOffset, tmp, IsHex);
end;

function TTextParsing.isNumber(cOffset: Integer; var NumberBegin: Integer; var IsHex: Boolean): Boolean;
var
  c: TP_Char;
  L: Integer;
  cPos, bkPos: Integer;
  nc: Integer;
  dotNum: Integer;
  eNum: Integer;
  eSymNum: Integer;
  pSym: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          Result := p^.tokenType = TTokenType.ttNumber;
          if Result then
            begin
              NumberBegin := p^.bPos;
              IsHex := p^.Text.ComparePos(1, '$') or p^.Text.ComparePos(1, '0x');
            end;
          exit;
        end;
    end;

  Result := False;

  cPos := cOffset;
  L := ParsingData.L;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      cPos := L;

  if cPos = L then
      exit;

  IsHex := False;
  try
    if (Char_is(ParsingData.Text[cPos], '$')) then
      begin
        // pascal style hex
        IsHex := True;
        inc(cPos);
        if cPos > L then
            exit;
      end
    else if ComparePosStr(cPos, '0x') then
      begin
        // c style hex
        IsHex := True;
        inc(cPos, 2);
        if cPos > L then
            exit;
      end;
  except
  end;

  if IsHex then
    begin
      bkPos := cPos;
      nc := 0;
      while True do
        begin
          cPos := GetTextDeclEndPos(GetCommentEndPos(cPos));

          if cPos > L then
              Break;
          c := ParsingData.Text[cPos];

          if isWordSplitChar(c, True, SymbolTable) then
            begin
              if nc > 0 then
                  Break;
            end
          else if Char_is(c, {$IFDEF FPC}ucHex{$ELSE FPC}cHex{$ENDIF FPC}) then
              inc(nc)
          else
            begin
              Result := False;
              exit;
            end;

          inc(cPos);
        end;

      Result := (nc > 0);
      NumberBegin := bkPos;
      exit;
    end;

  c := ParsingData.Text[cPos];
  if Char_is(c, {$IFDEF FPC}uc0to9{$ELSE FPC}c0to9{$ENDIF FPC}) then
    begin
      bkPos := cPos;
      nc := 0;
      dotNum := 0;
      eNum := 0;
      eSymNum := 0;
      while True do
        begin
          cPos := GetTextDeclEndPos(GetCommentEndPos(cPos));

          if cPos > L then
              Break;
          c := ParsingData.Text[cPos];

          if Char_is(c, '.') then
            begin
              inc(dotNum);
              if dotNum > 1 then
                  Break;
            end
          else if Char_is(c, {$IFDEF FPC}uc0to9{$ELSE FPC}c0to9{$ENDIF FPC}) then
              inc(nc)
          else if (nc > 0) and (eNum = 0) and Char_is(c, 'eE') then
            begin
              inc(eNum);
            end
          else if (nc > 0) and (eNum = 1) and Char_is(c, '-+') then
            begin
              inc(eSymNum);
            end
          else if isWordSplitChar(c, True, SymbolTable) then
            begin
              Break;
            end
          else if Char_is(c, [{$IFDEF FPC}ucAtoZ, ucDoubleChar{$ELSE FPC}cAtoZ, cDoubleChar{$ENDIF FPC}]) then
            begin
              Result := False;
              exit;
            end;

          inc(cPos);
        end;

      Result := (nc > 0) and (dotNum <= 1);
      NumberBegin := bkPos;
      exit;
    end
  else if Char_is(c, '+-.') then
    begin
      bkPos := cPos;
      nc := 0;
      dotNum := 0;
      eNum := 0;
      eSymNum := 0;
      pSym := 0;
      while True do
        begin
          cPos := GetTextDeclEndPos(GetCommentEndPos(cPos));

          if cPos > L then
              Break;
          c := ParsingData.Text[cPos];

          if (nc = 0) and (eSymNum = 0) and (eNum = 0) and Char_is(c, '-+') then
            begin
              inc(pSym);
            end
          else if Char_is(c, '.') then
            begin
              inc(dotNum);
              if dotNum > 1 then
                  Break;
            end
          else if Char_is(c, {$IFDEF FPC}uc0to9{$ELSE FPC}c0to9{$ENDIF FPC}) then
              inc(nc)
          else if (nc > 0) and (eNum = 0) and Char_is(c, 'eE') then
            begin
              inc(eNum);
            end
          else if (nc > 0) and (eNum = 1) and Char_is(c, '-+') then
            begin
              inc(eSymNum);
            end
          else if isWordSplitChar(c, True, SymbolTable) then
            begin
              Break
            end
          else if Char_is(c, [{$IFDEF FPC}ucAtoZ, ucDoubleChar{$ELSE FPC}cAtoZ, cDoubleChar{$ENDIF FPC}]) then
            begin
              Result := False;
              exit;
            end;

          inc(cPos);
        end;

      Result := (nc > 0) and (dotNum <= 1);
      NumberBegin := bkPos;
      exit;
    end;
end;

function TTextParsing.GetNumberEndPos(cOffset: Integer): Integer;
var
  IsHex: Boolean;
  L: Integer;
  cPos: Integer;
  c: TP_Char;
  nc: Integer;
  dotNum: Integer;
  eNum: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          if p^.tokenType = TTokenType.ttNumber then
              Result := p^.ePos
          else
              Result := cOffset;
          exit;
        end;
    end;

  L := ParsingData.L;
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      cPos := L;

  if isNumber(cPos, Result, IsHex) then
    begin
      nc := 0;
      dotNum := 0;
      eNum := 0;
      while True do
        begin
          if isComment(Result) or isTextDecl(Result) then
              Break;
          c := ParsingData.Text[Result];

          if (not Char_is(c, [{$IFDEF FPC}uc0to9{$ELSE FPC}c0to9{$ENDIF FPC}])) then
            begin
              if Char_is(c, '+-') then
                begin
                  if nc > 0 then
                    begin
                      if eNum = 1 then
                          inc(eNum)
                      else
                          exit;
                    end;
                end
              else if (not IsHex) and Char_is(c, '.') then
                begin
                  if (dotNum > 1) then
                      exit;
                  inc(dotNum);
                end
              else if (not IsHex) and Char_is(c, 'eE') then
                begin
                  if (eNum > 1) then
                      exit;
                  inc(eNum);
                end
              else if (IsHex and (Char_is(c, [{$IFDEF FPC}ucLoAtoF, ucHiAtoF{$ELSE FPC}cLoAtoF, cHiAtoF{$ENDIF FPC}]))) then
                  inc(nc)
              else
                  exit;
            end
          else
              inc(nc);

          inc(Result);
          if Result > L then
              exit;
        end;
    end
  else
      Result := cPos;
end;

function TTextParsing.isTextDecl(cOffset: Integer): Boolean;
var
  bPos, ePos: Integer;
begin
  Result := GetTextDeclPos(cOffset, bPos, ePos);
end;

function TTextParsing.GetTextDeclEndPos(cOffset: Integer): Integer;
var
  bPos, ePos: Integer;
begin
  if GetTextDeclPos(cOffset, bPos, ePos) then
      Result := ePos
  else
      Result := cOffset;
end;

function TTextParsing.GetTextDeclBeginPos(cOffset: Integer): Integer;
var
  bPos, ePos: Integer;
begin
  if GetTextDeclPos(cOffset, bPos, ePos) then
      Result := bPos
  else
      Result := cOffset;
end;

function TTextParsing.GetTextBody(Text_: TP_String): TP_String;
begin
  if TextStyle = tsPascal then
      Result := Translate_Pascal_Decl_To_Text(Text_)
  else if TextStyle = tsC then
      Result := Translate_C_Decl_To_Text(Text_)
  else if TextStyle = tsXML then
      Result := Translate_XML_Decl_To_Text(Text_)
  else
      Result := Text_;
end;

function TTextParsing.GetTextDeclPos(cOffset: Integer; var charBeginPos, charEndPos: Integer): Boolean;
  function CompLst(idx: Integer): Integer;
  begin
    with PTextPos(ParsingData.Cache.TextDecls[idx])^ do
      begin
        if (cOffset >= bPos) and (cOffset < ePos) then
            Result := 0
        else if (cOffset >= ePos) then
            Result := -1
        else if (cOffset < bPos) then
            Result := 1
        else
            Result := -2;
      end;
  end;

var
  cPos, L, r, M: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          Result := p^.tokenType = TTokenType.ttTextDecl;
          if Result then
            begin
              charBeginPos := p^.bPos;
              charEndPos := p^.ePos;
            end;
          exit;
        end;
    end;

  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > ParsingData.L then
      cPos := ParsingData.L;

  if ParsingData.Cache.TextDecls = nil then
      RebuildParsingCache;

  Result := False;

  L := 0;
  r := ParsingData.Cache.TextDecls.Count - 1;
  while L <= r do
    begin
      M := (L + r) div 2;
      case CompLst(M) of
        0:
          begin
            with PTextPos(ParsingData.Cache.TextDecls[M])^ do
              begin
                charBeginPos := bPos;
                charEndPos := ePos;
              end;
            Result := True;
            exit;
          end;
        -1: L := M + 1;
        1: r := M - 1;
        else RaiseInfo('struct error');
      end;
    end;
end;

function TTextParsing.isSymbol(cOffset: Integer): Boolean;
var
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          Result := p^.tokenType = TTokenType.ttSymbol;
          exit;
        end;
    end;
  Result := Char_is(ParsingData.Text[cOffset], SymbolTable)
end;

function TTextParsing.GetSymbolEndPos(cOffset: Integer): Integer;
begin
  if isSymbol(cOffset) then
      Result := cOffset + 1
  else
      Result := cOffset;
end;

function TTextParsing.isAscii(cOffset: Integer): Boolean;
var
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          Result := p^.tokenType = TTokenType.ttAscii;
          exit;
        end;
    end;
  Result := False;

  if isComment(cOffset) then
      exit;

  if isTextDecl(cOffset) then
      exit;

  if isSpecialSymbol(cOffset) then
      exit;

  Result := (not isSymbol(cOffset)) and (not isWordSplitChar(ParsingData.Text[cOffset], True, SymbolTable)) and (not isNumber(cOffset));
end;

function TTextParsing.GetAsciiBeginPos(cOffset: Integer): Integer;
var
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          if p^.tokenType = TTokenType.ttAscii then
              Result := p^.bPos
          else
              Result := cOffset;
          exit;
        end;
    end;
  Result := GetWordBeginPos(cOffset, True, SymbolTable);
end;

function TTextParsing.GetAsciiEndPos(cOffset: Integer): Integer;
var
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          if p^.tokenType = TTokenType.ttAscii then
              Result := p^.ePos
          else
              Result := cOffset;
          exit;
        end;
    end;
  Result := GetWordEndPos(cOffset, True, SymbolTable, True, SymbolTable);
end;

function TTextParsing.isComment(cOffset: Integer): Boolean;
var
  bPos, ePos: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          Result := p^.tokenType = TTokenType.ttComment;
          exit;
        end;
    end;
  Result := GetCommentPos(cOffset, bPos, ePos);
end;

function TTextParsing.GetCommentEndPos(cOffset: Integer): Integer;
var
  bPos, ePos: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          if p^.tokenType = TTokenType.ttComment then
              Result := p^.ePos
          else
              Result := cOffset;
          exit;
        end;
    end;

  if GetCommentPos(cOffset, bPos, ePos) then
      Result := ePos
  else
      Result := cOffset;
end;

function TTextParsing.GetCommentBeginPos(cOffset: Integer): Integer;
var
  bPos, ePos: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          if p^.tokenType = TTokenType.ttComment then
              Result := p^.bPos
          else
              Result := cOffset;
          exit;
        end;
    end;

  if GetCommentPos(cOffset, bPos, ePos) then
      Result := bPos
  else
      Result := cOffset;
end;

function TTextParsing.GetCommentPos(cOffset: Integer; var charBeginPos, charEndPos: Integer): Boolean;
  function CompLst(idx: Integer): Integer;
  begin
    with PTextPos(ParsingData.Cache.CommentDecls[idx])^ do
      begin
        if (cOffset >= bPos) and (cOffset < ePos) then
            Result := 0
        else if (cOffset >= ePos) then
            Result := -1
        else if (cOffset < bPos) then
            Result := 1
        else
            Result := -2;
      end;
  end;

var
  cPos, L, r, M: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          Result := p^.tokenType = TTokenType.ttComment;
          if Result then
            begin
              charBeginPos := p^.bPos;
              charEndPos := p^.ePos;
            end;
          exit;
        end;
    end;

  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > ParsingData.L then
      cPos := ParsingData.L;

  if ParsingData.Cache.CommentDecls = nil then
      RebuildParsingCache;

  Result := False;

  L := 0;
  r := ParsingData.Cache.CommentDecls.Count - 1;
  while L <= r do
    begin
      M := (L + r) div 2;
      case CompLst(M) of
        0:
          begin
            with PTextPos(ParsingData.Cache.CommentDecls[M])^ do
              begin
                charBeginPos := bPos;
                charEndPos := ePos;
              end;
            Result := True;
            exit;
          end;
        -1: L := M + 1;
        1: r := M - 1;
        else RaiseInfo('struct error');
      end;
    end;
end;

function TTextParsing.GetDeletedCommentText: TP_String;
var
  oriPos, cPos, nPos: Integer;
begin
  Result := '';

  cPos := 1;
  oriPos := cPos;

  while cPos < ParsingData.L do
    begin
      nPos := CompareCommentGetEndPos(cPos);
      if nPos > cPos then
        begin
          Result := Result + GetStr(oriPos, cPos);
          cPos := nPos;
          oriPos := cPos;
        end
      else
        begin
          inc(cPos);
        end;
    end;
  if oriPos <= ParsingData.L then
      Result := Result + GetStr(oriPos, ParsingData.L + 1);

  Result := Result.TrimChar(#32);
end;

function TTextParsing.isTextOrComment(cOffset: Integer): Boolean;
begin
  Result := isTextDecl(cOffset) or isComment(cOffset);
end;

function TTextParsing.isCommentOrText(cOffset: Integer): Boolean;
begin
  Result := isComment(cOffset) or isTextDecl(cOffset);
end;

class function TTextParsing.isWordSplitChar(c: TP_Char): Boolean;
begin
  Result := Char_is(c, [{$IFDEF FPC}uc0to32{$ELSE FPC}c0to32{$ENDIF FPC}]);
end;

class function TTextParsing.isWordSplitChar(c: TP_Char; Split_Token_Char: TP_String): Boolean;
begin
  Result := isWordSplitChar(c, True, Split_Token_Char);
end;

class function TTextParsing.isWordSplitChar(c: TP_Char; Include_C_0_to_32: Boolean; Split_Token_Char: TP_String): Boolean;
begin
  if Include_C_0_to_32 then
      Result := Char_is(c, [{$IFDEF FPC}uc0to32{$ELSE FPC}c0to32{$ENDIF FPC}], Split_Token_Char)
  else
      Result := Char_is(c, Split_Token_Char);
end;

function TTextParsing.GetWordBeginPos(cOffset: Integer; Split_Token_Char: TP_String): Integer;
begin
  Result := GetWordBeginPos(cOffset, True, Split_Token_Char);
end;

function TTextParsing.GetWordBeginPos(cOffset: Integer): Integer;
begin
  Result := GetWordBeginPos(cOffset, True, '');
end;

function TTextParsing.GetWordBeginPos(cOffset: Integer; Include_C_0_to_32: Boolean; Split_Token_Char: TP_String): Integer;
var
  L: Integer;
  cPos: Integer;
  tbPos: Integer;
begin
  L := ParsingData.L;
  cPos := cOffset;
  if cPos < 1 then
      exit(1);
  if cPos > L then
      exit(L);

  repeat
    cPos := GetCommentEndPos(cPos);

    tbPos := GetTextDeclBeginPos(cPos);
    if tbPos <> cPos then
        exit(tbPos);

    while (isWordSplitChar(ParsingData.Text[cPos], Include_C_0_to_32, Split_Token_Char)) do
      begin
        if cPos >= L then
            Break;
        inc(cPos);
      end;
  until not isComment(cPos);

  Result := cPos;
  while (not isWordSplitChar(ParsingData.Text[Result], Include_C_0_to_32, Split_Token_Char)) do
    begin
      if Result - 1 <= 0 then
          Break;
      dec(Result);
    end;

  if isWordSplitChar(ParsingData.Text[Result], Split_Token_Char) then
      inc(Result);
end;

function TTextParsing.GetWordEndPos(cOffset: Integer; Split_Token_Char: TP_String): Integer;
begin
  Result := GetWordEndPos(cOffset, True, Split_Token_Char, True, Split_Token_Char);
end;

function TTextParsing.GetWordEndPos(cOffset: Integer): Integer;
begin
  Result := GetWordEndPos(cOffset, True, '', True, '');
end;

function TTextParsing.GetWordEndPos(cOffset: Integer; BeginSplitCharSet, EndSplitCharSet: TP_String): Integer;
begin
  Result := GetWordEndPos(cOffset, True, BeginSplitCharSet, True, EndSplitCharSet);
end;

function TTextParsing.GetWordEndPos(cOffset: Integer; Include_C_0_to_32: Boolean; BeginSplitCharSet: TP_String; EndDefaultChar: Boolean; EndSplitCharSet: TP_String): Integer;
var
  L: Integer;
begin
  L := ParsingData.L;
  if cOffset < 1 then
      exit(1);
  if cOffset > L then
      exit(L);

  Result := GetWordBeginPos(cOffset, Include_C_0_to_32, BeginSplitCharSet);

  while (not isWordSplitChar(ParsingData.Text[Result], EndDefaultChar, EndSplitCharSet)) do
    begin
      inc(Result);
      if Result > L then
          Break;
    end;
end;

function TTextParsing.SniffingNextChar(cOffset: Integer; declChar: TP_String): Boolean;
var
  tmp: Integer;
begin
  Result := SniffingNextChar(cOffset, declChar, tmp);
end;

function TTextParsing.SniffingNextChar(cOffset: Integer; declChar: TP_String; out OutPos: Integer): Boolean;
var
  L: Integer;
  cPos: Integer;
begin
  L := ParsingData.L;
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      exit(False);

  while isWordSplitChar(ParsingData.Text[cPos], True, '') or (isTextOrComment(cPos)) do
    begin
      inc(cPos);
      if cPos > L then
          exit(False);
    end;

  if (cPos < L) then
      Result := Char_is(ParsingData.Text[cPos], declChar)
  else
      Result := False;

  if Result then
      OutPos := cPos;
end;

function TTextParsing.SplitChar(cOffset: Integer; var LastPos: Integer; Include_C_0_to_32: Boolean; Split_Token_Char, Split_End_Token_Char: TP_String; var SplitOutput: TSymbolVector): Integer;
  procedure AddS(s: TP_String);
  var
    n: TP_String;
    L: Integer;
  begin
    n := s.TrimChar(#32#0);
    if n.L = 0 then
        exit;
    L := Length(SplitOutput);
    SetLength(SplitOutput, L + 1);
    SplitOutput[L] := n;
    inc(Result);
  end;

type
  TLastSym = (lsBody, lsNone);

var
  L: Integer;
  c: TP_Char;
  cPos, bPos, ePos: Integer;
  LastSym: TLastSym;
begin
  Result := 0;
  SetLength(SplitOutput, 0);
  LastPos := cOffset;
  L := ParsingData.L;
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      exit;

  bPos := cPos;
  ePos := bPos;
  LastSym := lsNone;
  while (cPos <= L) do
    begin
      if isComment(cPos) then
        begin
          cPos := GetCommentEndPos(cPos);
          Continue;
        end;
      if isTextDecl(cPos) then
        begin
          cPos := GetTextDeclEndPos(cPos);
          Continue;
        end;
      c := ParsingData.Text[cPos];
      if isWordSplitChar(c, Include_C_0_to_32, Split_Token_Char) then
        begin
          if LastSym = lsBody then
            begin
              ePos := cPos;
              AddS(GetStr(bPos, ePos));
              LastSym := lsNone;
            end;
          inc(cPos);
          Continue;
        end;
      if (Split_End_Token_Char <> '') and (isWordSplitChar(c, False, Split_End_Token_Char)) then
        begin
          if LastSym = lsBody then
            begin
              ePos := cPos;
              AddS(GetStr(bPos, ePos));
              LastSym := lsNone;
            end;
          LastPos := cPos;
          exit;
        end;

      if LastSym = lsNone then
        begin
          bPos := cPos;
          LastSym := lsBody;
        end;
      inc(cPos);
    end;

  if LastSym = lsBody then
    begin
      ePos := cPos;
      AddS(GetStr(bPos, ePos));
      LastSym := lsNone;
    end;
  LastPos := cPos;
end;

function TTextParsing.SplitChar(cOffset: Integer; var LastPos: Integer; Split_Token_Char, Split_End_Token_Char: TP_String; var SplitOutput: TSymbolVector): Integer;
begin
  Result := SplitChar(cOffset, LastPos, False, Split_Token_Char, Split_End_Token_Char, SplitOutput);
end;

function TTextParsing.SplitChar(cOffset: Integer; Split_Token_Char, Split_End_Token_Char: TP_String; var SplitOutput: TSymbolVector): Integer;
var
  t: Integer;
begin
  Result := SplitChar(cOffset, t, Split_Token_Char, Split_End_Token_Char, SplitOutput);
end;

function TTextParsing.SplitString(cOffset: Integer; var LastPos: Integer; SplitTokenS, SplitEndTokenS: TP_String; var SplitOutput: TSymbolVector): Integer;
  procedure AddS(s: TP_String);
  var
    n: TP_String;
    L: Integer;
  begin
    n := s.TrimChar(#32#0);
    if n.L = 0 then
        exit;
    L := Length(SplitOutput);
    SetLength(SplitOutput, L + 1);
    SplitOutput[L] := n;
    inc(Result);
  end;

type
  TLastSym = (lsBody, lsNone);

var
  L: Integer;
  c: TP_Char;
  cPos, bPos, ePos: Integer;
  LastSym: TLastSym;
begin
  Result := 0;
  SetLength(SplitOutput, 0);
  LastPos := cOffset;
  L := ParsingData.L;
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      exit;

  bPos := cPos;
  ePos := bPos;
  LastSym := lsNone;
  while (cPos <= L) do
    begin
      if isComment(cPos) then
        begin
          cPos := GetCommentEndPos(cPos);
          Continue;
        end;
      if isTextDecl(cPos) then
        begin
          cPos := GetTextDeclEndPos(cPos);
          Continue;
        end;
      if ComparePosStr(cPos, SplitTokenS) then
        begin
          if LastSym = lsBody then
            begin
              ePos := cPos;
              AddS(GetStr(bPos, ePos));
              LastSym := lsNone;
            end;
          inc(cPos, SplitTokenS.L);
          Continue;
        end;
      if (SplitEndTokenS <> '') and ComparePosStr(cPos, SplitEndTokenS) then
        begin
          if LastSym = lsBody then
            begin
              ePos := cPos;
              AddS(GetStr(bPos, ePos));
              LastSym := lsNone;
            end;
          LastPos := cPos;
          exit;
        end;

      if LastSym = lsNone then
        begin
          bPos := cPos;
          LastSym := lsBody;
        end;
      inc(cPos);
    end;

  if LastSym = lsBody then
    begin
      ePos := cPos;
      AddS(GetStr(bPos, ePos));
      LastSym := lsNone;
    end;
  LastPos := cPos;
end;

function TTextParsing.SplitString(cOffset: Integer; SplitTokenS, SplitEndTokenS: TP_String; var SplitOutput: TSymbolVector): Integer;
var
  t: Integer;
begin
  Result := SplitString(cOffset, t, SplitTokenS, SplitEndTokenS, SplitOutput);
end;

function TTextParsing.CompareTokenText(cOffset: Integer; t: TP_String): Boolean;
var
  p: PTokenData;
begin
  Result := False;
  p := GetToken(cOffset);
  if p = nil then
      exit;
  Result := p^.Text.Same(t);
end;

function TTextParsing.CompareTokenChar(cOffset: Integer; c: array of TP_Char): Boolean;
var
  p: PTokenData;
begin
  Result := False;
  p := GetToken(cOffset);
  if p = nil then
      exit;
  if p^.Text.L <> 1 then
      exit;
  Result := Char_is(p^.Text.First, c);
end;

function TTextParsing.GetToken(cOffset: Integer): PTokenData;
begin
  if (cOffset - 1 >= 0) and (cOffset - 1 < Length(ParsingData.Cache.CharToken)) then
      Result := ParsingData.Cache.CharToken[cOffset - 1]
  else
      Result := nil;
end;

function TTextParsing.GetTokenIndex(t: TTokenType; idx: Integer): PTokenData;
var
  i, c: Integer;
  p: PTokenData;
begin
  Result := nil;
  c := 0;
  for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[i]);
      if p^.tokenType = t then
        begin
          if c = idx then
              exit(p)
          else
              inc(c);
        end;
    end;
end;

function TTextParsing.TokenCount: Integer;
begin
  Result := ParsingData.Cache.TokenDataList.Count;
end;

function TTextParsing.TokenCountT(t: TTokenTypes): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := ParsingData.Cache.TokenDataList.Count - 1 downto 0 do
    if GetTokens(i)^.tokenType in t then
        inc(Result);
end;

function TTextParsing.GetTokens(idx: Integer): PTokenData;
begin
  Result := PTokenData(ParsingData.Cache.TokenDataList[idx]);
end;

function TTextParsing.FirstToken: PTokenData;
begin
  Result := GetTokens(0);
end;

function TTextParsing.LastToken: PTokenData;
begin
  Result := GetTokens(TokenCount - 1);
end;

function TTextParsing.NextToken(p: PTokenData): PTokenData;
begin
  Result := nil;
  if (p = nil) or (p^.Index + 1 >= TokenCount) then
      exit;
  Result := Tokens[p^.Index + 1];
end;

function TTextParsing.PrevToken(p: PTokenData): PTokenData;
begin
  Result := nil;
  if (p = nil) or (p^.Index - 1 >= 0) then
      exit;
  Result := Tokens[p^.Index - 1];
end;

function TTextParsing.TokenCombine(bTokenI, eTokenI: Integer; acceptT: TTokenTypes): TP_String;
var
  bi, ei: Integer;
  p: PTokenData;
begin
  Result := '';

  if (bTokenI < 0) or (eTokenI < 0) then
      exit;

  if bTokenI > eTokenI then
    begin
      bi := eTokenI;
      ei := bTokenI;
    end
  else
    begin
      bi := bTokenI;
      ei := eTokenI;
    end;

  while (bi <= ei) and (bi < TokenCount) do
    begin
      p := Tokens[bi];
      if p^.tokenType in acceptT then
          Result.Append(p^.Text);
      inc(bi);
    end;

  if (bi >= TokenCount) then
    begin
      while (Result.L > 0) and (Result.Last = #0) do
          Result.DeleteLast;

      if (Result.L > 0) and (Result.Last = #32) then
          Result.DeleteLast;
    end;
end;

function TTextParsing.TokenCombine(bTokenI, eTokenI: Integer): TP_String;
begin
  Result := TokenCombine(bTokenI, eTokenI, [ttTextDecl, ttComment, ttNumber, ttSymbol, ttAscii, ttSpecialSymbol, ttUnknow]);
end;

function TTextParsing.Combine(bTokenI, eTokenI: Integer; acceptT: TTokenTypes): TP_String;
begin
  Result := TokenCombine(bTokenI, eTokenI, acceptT);
end;

function TTextParsing.Combine(bTokenI, eTokenI: Integer): TP_String;
begin
  Result := TokenCombine(bTokenI, eTokenI);
end;

function TTextParsing.TokenProbeL(startI: Integer; acceptT: TTokenTypes): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx >= 0 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) then
        begin
          Result := p;
          exit;
        end
      else
          dec(idx);
    end;
end;

function TTextParsing.TokenProbeL(startI: Integer; t: TP_String): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx >= 0 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.Text.Same(t)) then
        begin
          Result := p;
          exit;
        end
      else
          dec(idx);
    end;
end;

function TTextParsing.TokenProbeL(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx >= 0 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) and (p^.Text.Same(t)) then
        begin
          Result := p;
          exit;
        end
      else
          dec(idx);
    end;
end;

function TTextParsing.TokenProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2: TP_String): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx >= 0 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) and (p^.Text.Same(t1, t2)) then
        begin
          Result := p;
          exit;
        end
      else
          dec(idx);
    end;
end;

function TTextParsing.TokenProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2, t3: TP_String): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx >= 0 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) and (p^.Text.Same(t1, t2, t3)) then
        begin
          Result := p;
          exit;
        end
      else
          dec(idx);
    end;
end;

function TTextParsing.TokenProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4: TP_String): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx >= 0 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) and (p^.Text.Same(t1, t2, t3, t4)) then
        begin
          Result := p;
          exit;
        end
      else
          dec(idx);
    end;
end;

function TTextParsing.TokenProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4, t5: TP_String): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx >= 0 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) and (p^.Text.Same(t1, t2, t3, t4, t5)) then
        begin
          Result := p;
          exit;
        end
      else
          dec(idx);
    end;
end;

function TTextParsing.TokenProbeR(startI: Integer; acceptT: TTokenTypes): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx < ParsingData.Cache.TokenDataList.Count do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) then
        begin
          Result := p;
          exit;
        end
      else
          inc(idx);
    end;
end;

function TTextParsing.TokenProbeR(startI: Integer; t: TP_String): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx < ParsingData.Cache.TokenDataList.Count do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.Text.Same(t)) then
        begin
          Result := p;
          exit;
        end
      else
          inc(idx);
    end;
end;

function TTextParsing.TokenProbeR(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx < ParsingData.Cache.TokenDataList.Count do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) and (p^.Text.Same(t)) then
        begin
          Result := p;
          exit;
        end
      else
          inc(idx);
    end;
end;

function TTextParsing.TokenProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2: TP_String): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx < ParsingData.Cache.TokenDataList.Count do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) and (p^.Text.Same(t1, t2)) then
        begin
          Result := p;
          exit;
        end
      else
          inc(idx);
    end;
end;

function TTextParsing.TokenProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2, t3: TP_String): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx < ParsingData.Cache.TokenDataList.Count do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) and (p^.Text.Same(t1, t2, t3)) then
        begin
          Result := p;
          exit;
        end
      else
          inc(idx);
    end;
end;

function TTextParsing.TokenProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4: TP_String): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx < ParsingData.Cache.TokenDataList.Count do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) and (p^.Text.Same(t1, t2, t3, t4)) then
        begin
          Result := p;
          exit;
        end
      else
          inc(idx);
    end;
end;

function TTextParsing.TokenProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4, t5: TP_String): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx < ParsingData.Cache.TokenDataList.Count do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) and (p^.Text.Same(t1, t2, t3, t4, t5)) then
        begin
          Result := p;
          exit;
        end
      else
          inc(idx);
    end;
end;

function TTextParsing.ProbeL(startI: Integer; acceptT: TTokenTypes): PTokenData;
begin
  Result := TokenProbeL(startI, acceptT);
end;

function TTextParsing.ProbeL(startI: Integer; t: TP_String): PTokenData;
begin
  Result := TokenProbeL(startI, t);
end;

function TTextParsing.ProbeL(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData;
begin
  Result := TokenProbeL(startI, acceptT, t);
end;

function TTextParsing.ProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2: TP_String): PTokenData;
begin
  Result := TokenProbeL(startI, acceptT, t1, t2);
end;

function TTextParsing.ProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2, t3: TP_String): PTokenData;
begin
  Result := TokenProbeL(startI, acceptT, t1, t2, t3);
end;

function TTextParsing.ProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4: TP_String): PTokenData;
begin
  Result := TokenProbeL(startI, acceptT, t1, t2, t3, t4);
end;

function TTextParsing.ProbeL(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4, t5: TP_String): PTokenData;
begin
  Result := TokenProbeL(startI, acceptT, t1, t2, t3, t4, t5);
end;

function TTextParsing.LProbe(startI: Integer; acceptT: TTokenTypes): PTokenData;
begin
  Result := TokenProbeL(startI, acceptT);
end;

function TTextParsing.LProbe(startI: Integer; t: TP_String): PTokenData;
begin
  Result := TokenProbeL(startI, t);
end;

function TTextParsing.LProbe(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData;
begin
  Result := TokenProbeL(startI, acceptT, t);
end;

function TTextParsing.LProbe(startI: Integer; acceptT: TTokenTypes; t1, t2: TP_String): PTokenData;
begin
  Result := TokenProbeL(startI, acceptT, t1, t2);
end;

function TTextParsing.LProbe(startI: Integer; acceptT: TTokenTypes; t1, t2, t3: TP_String): PTokenData;
begin
  Result := TokenProbeL(startI, acceptT, t1, t2, t3);
end;

function TTextParsing.LProbe(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4: TP_String): PTokenData;
begin
  Result := TokenProbeL(startI, acceptT, t1, t2, t3, t4);
end;

function TTextParsing.LProbe(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4, t5: TP_String): PTokenData;
begin
  Result := TokenProbeL(startI, acceptT, t1, t2, t3, t4, t5);
end;

function TTextParsing.ProbeR(startI: Integer; acceptT: TTokenTypes): PTokenData;
begin
  Result := TokenProbeR(startI, acceptT);
end;

function TTextParsing.ProbeR(startI: Integer; t: TP_String): PTokenData;
begin
  Result := TokenProbeR(startI, t);
end;

function TTextParsing.ProbeR(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData;
begin
  Result := TokenProbeR(startI, acceptT, t);
end;

function TTextParsing.ProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2: TP_String): PTokenData;
begin
  Result := TokenProbeR(startI, acceptT, t1, t2);
end;

function TTextParsing.ProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2, t3: TP_String): PTokenData;
begin
  Result := TokenProbeR(startI, acceptT, t1, t2, t3);
end;

function TTextParsing.ProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4: TP_String): PTokenData;
begin
  Result := TokenProbeR(startI, acceptT, t1, t2, t3, t4);
end;

function TTextParsing.ProbeR(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4, t5: TP_String): PTokenData;
begin
  Result := TokenProbeR(startI, acceptT, t1, t2, t3, t4, t5);
end;

function TTextParsing.RProbe(startI: Integer; acceptT: TTokenTypes): PTokenData;
begin
  Result := TokenProbeR(startI, acceptT);
end;

function TTextParsing.RProbe(startI: Integer; t: TP_String): PTokenData;
begin
  Result := TokenProbeR(startI, t);
end;

function TTextParsing.RProbe(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData;
begin
  Result := TokenProbeR(startI, acceptT, t);
end;

function TTextParsing.RProbe(startI: Integer; acceptT: TTokenTypes; t1, t2: TP_String): PTokenData;
begin
  Result := TokenProbeR(startI, acceptT, t1, t2);
end;

function TTextParsing.RProbe(startI: Integer; acceptT: TTokenTypes; t1, t2, t3: TP_String): PTokenData;
begin
  Result := TokenProbeR(startI, acceptT, t1, t2, t3);
end;

function TTextParsing.RProbe(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4: TP_String): PTokenData;
begin
  Result := TokenProbeR(startI, acceptT, t1, t2, t3, t4);
end;

function TTextParsing.RProbe(startI: Integer; acceptT: TTokenTypes; t1, t2, t3, t4, t5: TP_String): PTokenData;
begin
  Result := TokenProbeR(startI, acceptT, t1, t2, t3, t4, t5);
end;

function TTextParsing.TokenFullStringProbe(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData;
begin
  Result := StringProbe(startI, acceptT, t);
end;

function TTextParsing.StringProbe(startI: Integer; acceptT: TTokenTypes; t: TP_String): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx < ParsingData.Cache.TokenDataList.Count do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) and (ComparePosStr(p^.bPos, t)) then
        begin
          Result := p;
          exit;
        end
      else
          inc(idx);
    end;
end;

function TTextParsing.IndentSymbolEndProbeR(startI: Integer; indent_begin_symbol, indent_end_symbol: TP_String): PTokenData;
var
  idx, bC, eC: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  bC := 0;
  eC := 0;
  while idx < ParsingData.Cache.TokenDataList.Count do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);

      if indent_begin_symbol.Exists(p^.Text.buff) then
          inc(bC)
      else if indent_end_symbol.Exists(p^.Text.buff) then
          inc(eC);

      if (bC > 0) and (eC = bC) then
        begin
          Result := p;
          exit;
        end;

      inc(idx);
    end;
end;

function TTextParsing.IndentSymbolBeginProbeL(startI: Integer; indent_begin_symbol, indent_end_symbol: TP_String): PTokenData;
var
  idx, bC, eC: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  bC := 0;
  eC := 0;
  while idx >= 0 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);

      if indent_begin_symbol.Exists(p^.Text.buff) then
          inc(bC)
      else if indent_end_symbol.Exists(p^.Text.buff) then
          inc(eC);

      if (eC > 0) and (eC = bC) then
        begin
          Result := p;
          exit;
        end;

      dec(idx);
    end;
end;

function TTextParsing.DetectSymbolVector: Boolean;
var
  i: Integer;
  p1, p2, paramB, paramE: PTokenData;
  vExp: TP_String;
  VectorNum: Integer;
begin
  Result := False;

  i := 0;
  p1 := nil;
  p2 := nil;
  paramB := FirstToken;
  paramE := paramB;
  VectorNum := 0;

  while i < TokenCount do
    begin
      p1 := TokenProbeR(i, [ttSymbol]);
      if p1 = nil then
        begin
          // successed
          inc(VectorNum);
          Break;
        end;
      if p1^.Text.Same(',', ';') then
        begin
          paramE := p1;
          inc(VectorNum);
          paramB := NextToken(paramE);
          // successed
          if paramB = nil then
              Break;
          // do loop.
          paramE := paramB;
          i := paramB^.Index;
        end
      else if p1^.Text.Same('(') then
        begin
          p2 := IndentSymbolEndProbeR(p1^.Index, '(', ')');
          // error
          if p2 = nil then
              exit;

          // do loop.
          paramE := paramB;
          i := p2^.Index + 1;
        end
      else if p1^.Text.Same('[') then
        begin
          p2 := IndentSymbolEndProbeR(p1^.Index, '[', ']');
          // error
          if p2 = nil then
              exit;
          // do loop.
          paramE := paramB;
          i := p2^.Index + 1;
        end
      else
          inc(i);
    end;

  Result := VectorNum > 1;
end;

function TTextParsing.FillSymbolVector(L: TPascalStringList): Boolean;
var
  i: Integer;
  p1, p2, paramB, paramE: PTokenData;
  vExp: TP_String;
begin
  Result := False;

  i := 0;
  p1 := nil;
  p2 := nil;
  paramB := FirstToken;
  paramE := paramB;

  while i < TokenCount do
    begin
      p1 := TokenProbeR(i, [ttSymbol]);
      if p1 = nil then
        begin
          // successed
          vExp := TokenCombine(paramB^.Index, TokenCount - 1);
          L.Add(vExp.Text);
          Break;
        end;
      if p1^.Text.Same(',', ';') then
        begin
          paramE := p1;
          if paramB <> paramE then
              vExp := TokenCombine(paramB^.Index, paramE^.Index - 1)
          else
              vExp := '';
          L.Add(vExp.Text);
          paramB := NextToken(paramE);
          // successed
          if paramB = nil then
              Break;
          // do loop.
          paramE := paramB;
          i := paramB^.Index;
        end
      else if p1^.Text.Same('(') then
        begin
          p2 := IndentSymbolEndProbeR(p1^.Index, '(', ')');
          // error
          if p2 = nil then
              exit;
          // do loop.
          paramE := paramB;
          i := p2^.Index + 1;
        end
      else if p1^.Text.Same('[') then
        begin
          p2 := IndentSymbolEndProbeR(p1^.Index, '[', ']');
          // error
          if p2 = nil then
              exit;
          // do loop.
          paramE := paramB;
          i := p2^.Index + 1;
        end
      else
          inc(i);
    end;

  Result := True;
end;

function TTextParsing.FillSymbolVector: TSymbolVector;
var
  L: TPascalStringList;
  i: Integer;
begin
  L := TPascalStringList.Create;
  if FillSymbolVector(L) then
    begin
      SetLength(Result, L.Count);
      for i := 0 to L.Count - 1 do
          Result[i] := L[i];
    end
  else
      SetLength(Result, 0);
  DisposeObject(L);
end;

function TTextParsing.FillSymbolMatrix(W, H: Integer; var symbolMatrix: TSymbolMatrix): Boolean;
var
  L: TPascalStringList;
  i, j, k: Integer;
begin
  SetLength(symbolMatrix, 0, 0);
  L := TPascalStringList.Create;
  Result := FillSymbolVector(L);
  if L.Count >= W * H then
    begin
      SetLength(symbolMatrix, H, W);
      k := 0;
      for j := 0 to H - 1 do
        for i := 0 to W - 1 do
          begin
            symbolMatrix[j, i] := L[k];
            inc(k);
          end;
    end;
  DisposeObject(L);
end;

function TTextParsing.GetText(bPos, ePos: Integer): TP_String;
begin
  Result := GetStr(bPos, ePos);
end;

function TTextParsing.GetStr(bPos, ePos: Integer): TP_String;
begin
  if ePos >= ParsingData.L then
    begin
      Result := ParsingData.Text.GetString(bPos, ePos + 1);
      while (Result.L > 0) and (Result.Last = #0) do
          Result.DeleteLast;
      if (Result.L > 0) and (Result.Last = #32) then
          Result.DeleteLast;
    end
  else
      Result := ParsingData.Text.GetString(bPos, ePos);
end;

function TTextParsing.GetStr(tp: TTextPos): TP_String;
begin
  Result := GetStr(tp.bPos, tp.ePos);
end;

function TTextParsing.GetWord(cOffset: Integer): TP_String;
begin
  Result := GetStr(GetAsciiBeginPos(cOffset), GetAsciiEndPos(cOffset));
end;

function TTextParsing.GetPoint(cOffset: Integer): TPoint;
var
  i: Integer;
  cPos: Integer;
begin
  cPos := cOffset;
  Result := Point(1, 1);
  if cPos > ParsingData.L then
      cPos := ParsingData.L;
  for i := 1 to cPos - 1 do
    begin
      if ParsingData.Text[i] = #10 then
        begin
          inc(Result.y);
          Result.x := 0;
        end
      else if not Char_is(ParsingData.Text[i], [#13]) then
          inc(Result.x);
    end;
end;

function TTextParsing.GetChar(cOffset: Integer): TP_Char;
begin
  Result := ParsingData.Text[cOffset];
end;

procedure TTextParsing.DeletePos(bPos, ePos: Integer);
begin
  ParsingData.Text := GetStr(1, bPos) + GetStr(ePos, Len);
  ParsingData.L := ParsingData.Text.L;
  RebuildParsingCache;
end;

procedure TTextParsing.DeletePos(tp: TTextPos);
begin
  DeletePos(tp.bPos, tp.ePos);
end;

procedure TTextParsing.DeletedComment;
begin
  ParsingData.Text := GetDeletedCommentText.TrimChar(#32);
  ParsingData.L := ParsingData.Text.L;
  RebuildParsingCache;
end;

procedure TTextParsing.InsertTextBlock(bPos, ePos: Integer; InsertText_: TP_String);
begin
  ParsingData.Text := GetStr(1, bPos) + InsertText_ + GetStr(ePos, Len + 1);
  ParsingData.L := ParsingData.Text.L;
  RebuildParsingCache;
end;

procedure TTextParsing.InsertTextBlock(tp: TTextPos; InsertText_: TP_String);
begin
  InsertTextBlock(tp.bPos, tp.ePos, InsertText_);
end;

function TTextParsing.SearchWordBody(initPos: Integer; wordInfo: TP_String; var OutPos: TTextPos): Boolean;
var
  cp: Integer;
  ePos: Integer;
begin
  Result := False;

  cp := initPos;

  while cp <= ParsingData.L do
    begin
      if isTextDecl(cp) then
        begin
          ePos := GetTextDeclEndPos(cp);
          cp := ePos;
        end
      else if isComment(cp) then
        begin
          ePos := GetCommentEndPos(cp);
          cp := ePos;
        end
      else if isNumber(cp) then
        begin
          ePos := GetNumberEndPos(cp);
          if GetStr(cp, ePos).Same(wordInfo) then
            begin
              OutPos.bPos := cp;
              OutPos.ePos := ePos;
              Result := True;
              Break;
            end;
          cp := ePos;
        end
      else if isSymbol(cp) then
        begin
          ePos := GetSymbolEndPos(cp);
          cp := ePos;
        end
      else if isAscii(cp) then
        begin
          ePos := GetAsciiEndPos(cp);
          if GetStr(cp, ePos).Same(wordInfo) then
            begin
              OutPos.bPos := cp;
              OutPos.ePos := ePos;
              Result := True;
              Break;
            end;
          cp := ePos;
        end
      else
          inc(cp);
    end;
end;

class function TTextParsing.Translate_Pascal_Decl_To_Text(Decl: TP_String): TP_String;
var
  cPos: Integer;

  // ext decl begin flag
  VIsTextDecl: Boolean;
  nText: TP_String;
begin
  cPos := 1;
  VIsTextDecl := False;
  Result := '';
  while cPos <= Decl.L do
    begin
      if Decl.ComparePos(cPos, #39#39#39#39) then
        begin
          Result.Append(#39);
          inc(cPos, 4);
        end
      else if Decl[cPos] = #39 then
        begin
          VIsTextDecl := not VIsTextDecl;
          inc(cPos);
        end
      else
        begin
          if VIsTextDecl then
            begin
              Result.Append(Decl[cPos]);
              inc(cPos);
            end
          else if Decl[cPos] = '#' then
            begin
              nText := '';
              inc(cPos);
              while cPos <= Decl.L do
                begin
                  if Char_is(Decl[cPos], [{$IFDEF FPC}ucHex{$ELSE FPC}cHex{$ENDIF FPC}], '$') then
                    begin
                      nText.Append(Decl[cPos]);
                      inc(cPos);
                    end
                  else
                      Break;
                end;
              Result.Append(TP_Char(umlStrToInt(nText, 0)));
            end
          else
              inc(cPos);
        end;
    end;
end;

class function TTextParsing.Translate_Text_To_Pascal_Decl(Decl: TP_String): TP_String;
var
  cPos: Integer;
  c: TP_Char;
  LastIsOrdChar: Boolean;
  ordCharInfo: TP_String;
begin
  if Decl.L = 0 then
    begin
      Result := #39#39;
      exit;
    end;

  ordCharInfo.L := 32;
  for cPos := 0 to 31 do
      ordCharInfo.buff[cPos] := TP_Char(Ord(cPos));
  ordCharInfo[32] := #39;

  Result := '';
  LastIsOrdChar := False;
  for cPos := 1 to Decl.L do
    begin
      c := Decl[cPos];
      if Char_is(c, ordCharInfo) then
        begin
          if Result.L = 0 then
              Result := '#' + umlIntToStr(Ord(c))
          else if LastIsOrdChar then
              Result.Append('#' + umlIntToStr(Ord(c)))
          else
              Result.Append(#39 + '#' + umlIntToStr(Ord(c)));
          LastIsOrdChar := True;
        end
      else
        begin
          if Result.L = 0 then
              Result := #39 + c
          else if LastIsOrdChar then
              Result.Append(#39 + c)
          else
              Result.Append(c);

          LastIsOrdChar := False;
        end;
    end;

  if not LastIsOrdChar then
      Result.Append(#39);
end;

class function TTextParsing.Translate_Text_To_Pascal_Decl_With_Unicode(Decl: TP_String): TP_String;
var
  cPos: Integer;
  c: TP_Char;
  LastIsOrdChar: Boolean;
  ordCharInfo: TP_String;
begin
  if Decl.L = 0 then
    begin
      Result := #39#39;
      exit;
    end;

  ordCharInfo.L := 32 + 1;
  for cPos := 0 to 31 do
      ordCharInfo[cPos + 1] := TP_Char(Ord(cPos));
  ordCharInfo[33] := #39;

  Result := '';
  LastIsOrdChar := False;
  for cPos := 1 to Decl.L do
    begin
      c := Decl[cPos];
      if Char_is(c, ordCharInfo) or (Ord(c) >= $80) then
        begin
          if Result.L = 0 then
              Result := '#' + umlIntToStr(Ord(c))
          else if LastIsOrdChar then
              Result.Append('#' + umlIntToStr(Ord(c)))
          else
              Result.Append(#39 + '#' + umlIntToStr(Ord(c)));
          LastIsOrdChar := True;
        end
      else
        begin
          if Result.L = 0 then
              Result := #39 + c
          else if LastIsOrdChar then
              Result.Append(#39 + c)
          else
              Result.Append(c);

          LastIsOrdChar := False;
        end;
    end;

  if not LastIsOrdChar then
      Result.Append(#39);
end;

class function TTextParsing.Translate_C_Decl_To_Text(Decl: TP_String): TP_String;
var
  cPos: Integer;
  i: Integer;

  // ext decl begin flag
  VIsCharDecl: Boolean;
  VIsTextDecl: Boolean;
  nText: TP_String;
  wasC: Boolean;
begin
  cPos := 1;
  VIsCharDecl := False;
  VIsTextDecl := False;
  Result := '';
  while cPos <= Decl.L do
    begin
      if Decl[cPos] = #39 then
        begin
          VIsCharDecl := not VIsCharDecl;
          inc(cPos);
        end
      else if Decl[cPos] = '"' then
        begin
          VIsTextDecl := not VIsTextDecl;
          inc(cPos);
        end
      else
        begin
          wasC := False;
          for i := low(CTranslateTable) to high(CTranslateTable) do
            begin
              if Decl.ComparePos(cPos, CTranslateTable[i].c) then
                begin
                  inc(cPos, Length(CTranslateTable[i].c));
                  Result.Append(CTranslateTable[i].s);
                  wasC := True;
                  Break;
                end;
            end;
          if (not wasC) then
            begin
              if VIsTextDecl or VIsCharDecl then
                  Result.Append(Decl[cPos]);
              inc(cPos);
            end;
        end;
    end;
end;

class function TTextParsing.Translate_Text_To_C_Decl(Decl: TP_String): TP_String;
  function GetCStyle(c: TP_Char): TP_SystemString;
  var
    i: Integer;
  begin
    Result := '';
    for i := low(CTranslateTable) to high(CTranslateTable) do
      if c = CTranslateTable[i].s then
        begin
          Result := CTranslateTable[i].c;
          Break;
        end;
  end;

var
  cPos: Integer;
  c: TP_Char;
  LastIsOrdChar: Boolean;
  n: TP_SystemString;
begin
  if Decl.L = 0 then
    begin
      Result := '""';
      exit;
    end;

  Result := '';
  LastIsOrdChar := False;
  for cPos := 1 to Decl.L do
    begin
      c := Decl[cPos];

      if Result.L = 0 then
          Result := '"' + c
      else
        begin
          n := GetCStyle(c);
          if n <> '' then
              Result.Append(n)
          else
              Result.Append(c);
        end;
    end;

  if not LastIsOrdChar then
      Result.Append('"');
end;

class function TTextParsing.Translate_Text_To_XML_Decl(Decl: TP_String): TP_String;
var
  c: TP_Char;
begin
  Result := '"';
  for c in Decl.buff do
    begin
      case c of
        #0 .. #31: Result.Append('&#' + umlIntToStr(Ord(c)) + ';');
        '&': Result.Append('&amp;');
        #39: Result.Append('&apos;');
        '"': Result.Append('&quot;');
        '<': Result.Append('&lt;');
        '>': Result.Append('&gt;');
        else
          Result.Append(c);
      end;
    end;
  Result.Append('"');
end;

class function TTextParsing.Translate_XML_Decl_To_Text(Decl: TP_String): TP_String;
var
  n: TP_String;
  i, bPos, ePos: Integer;
begin
  Result := '';
  if (Decl.First <> '"') or (Decl.Last <> '"') then
      exit;

  n := Decl;
  n.DeleteFirst;
  n.DeleteLast;
  i := 1;
  while i <= n.L do
    begin
      if n.ComparePos(i, '&#') then
        begin
          inc(i, 2);
          bPos := i;
          ePos := i;
          while (ePos <= n.L) and (n[ePos] <> ';') do
            begin
              inc(ePos);
            end;
          Result.Append(TP_Char(umlStrToInt(n.GetString(bPos, ePos))));
          i := ePos + 1;
        end
      else if n.ComparePos(i, '&amp;') then
        begin
          Result.Append('&');
          inc(i, 5);
        end
      else if n.ComparePos(i, '&apos;') then
        begin
          Result.Append(#39);
          inc(i, 6);
        end
      else if n.ComparePos(i, '&quot;') then
        begin
          Result.Append('"');
          inc(i, 6);
        end
      else if n.ComparePos(i, '&lt;') then
        begin
          Result.Append('<');
          inc(i, 4);
        end
      else if n.ComparePos(i, '&gt;') then
        begin
          Result.Append('>');
          inc(i, 4);
        end
      else
        begin
          Result.Append(n[i]);
          inc(i);
        end;
    end;
end;

class function TTextParsing.Translate_Pascal_Decl_Comment_To_Text(Decl: TP_String): TP_String;
begin
  Result := Decl.TrimChar(#32#9);
  if umlMultipleMatch(False, '{*}', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteLast;
      if umlMultipleMatch(False, '$*', Result.TrimChar(#32#9)) then
          Result := Decl;
    end
  else if umlMultipleMatch(False, '(*?*)', Result, '?', '') then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteLast;
      Result.DeleteLast;
    end
  else if umlMultipleMatch(False, '////*', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteFirst;
      while Char_is(Result.Last, [#13, #10]) do
          Result.DeleteLast;
    end
  else if umlMultipleMatch(False, '///*', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteFirst;
      while Char_is(Result.Last, [#13, #10]) do
          Result.DeleteLast;
    end
  else if umlMultipleMatch(False, '//*', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
      while Char_is(Result.Last, [#13, #10]) do
          Result.DeleteLast;
    end;
end;

class function TTextParsing.Translate_Text_To_Pascal_Decl_Comment(Decl: TP_String): TP_String;
var
  n: TP_String;
begin
  n := Decl.TrimChar(#32#9);
  if umlMultipleMatch(False, '(*?*)', n, '?', '') then
      Result := Decl
  else if umlMultipleMatch(False, '{*}', n) then
      Result := Decl
  else if n.Exists(['{', '}']) then
      Result := '(* ' + Decl.Text + ' *)'
  else
      Result := '{ ' + Decl.Text + ' }';
end;

class function TTextParsing.Translate_C_Decl_Comment_To_Text(Decl: TP_String): TP_String;
begin
  Result := Decl.TrimChar(#32#9);
  if umlMultipleMatch(False, '#*', Result) then
    begin
      Result := Decl;
    end
  else if umlMultipleMatch(False, '/*?*/', Result, '?', '') then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteLast;
      Result.DeleteLast;
    end
  else if umlMultipleMatch(False, '////*', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteFirst;
    end
  else if umlMultipleMatch(False, '///*', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteFirst;
    end
  else if umlMultipleMatch(False, '//*', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
    end;
end;

class function TTextParsing.Translate_Text_To_C_Decl_Comment(Decl: TP_String): TP_String;
var
  n: TP_String;
begin
  n := Decl.TrimChar(#32#9);
  if umlMultipleMatch(False, '#*', n) then
      Result := Decl
  else
      Result := '/* ' + n.Text + ' */';
end;

class function TTextParsing.Translate_XML_Decl_Comment_To_Text(Decl: TP_String): TP_String;
var
  i: Integer;
begin
  if Decl.ComparePos(1, '<!--') then
    begin
      i := 5;
      while (i <= Decl.L) do
        if Decl.ComparePos(i, '-->') then
            Break
        else
            inc(i);
      Result := Decl.GetString(5, i);
    end
  else if Decl.ComparePos(1, '<![CDATA[') then
    begin
      i := 9;
      while (i <= Decl.L) do
        if Decl.ComparePos(i, ']]>') then
            Break
        else
            inc(i);
      Result := Decl.GetString(9, i);
    end
  else
      Result := Decl;
end;

class function TTextParsing.Translate_Text_To_XML_Decl_Comment(Decl: TP_String): TP_String;
begin
  Result := '<!--' + Decl + '-->';
end;

constructor TTextParsing.Create(Text_: TP_String; Style_: TTextStyle; SpecialSymbol_: TListPascalString; SpacerSymbol_: TP_SystemString);
begin
  inherited Create;
  ParsingData.Cache.CommentDecls := nil;
  ParsingData.Cache.TextDecls := nil;
  ParsingData.Cache.TokenDataList := nil;
  SetLength(ParsingData.Cache.CharToken, 0);
  if Text_.L = 0 then
      ParsingData.Text := #13#10
  else
      ParsingData.Text := Text_.Text + #32;
  ParsingData.L := ParsingData.Text.L + 1;
  TextStyle := Style_;
  SymbolTable := SpacerSymbol_;
  TokenStatistics := NullTokenStatistics;
  SpecialSymbol := TListPascalString.Create;
  if SpecialSymbol_ <> nil then
      SpecialSymbol.Assign(SpecialSymbol_);
  RebuildCacheBusy := False;

  RebuildParsingCache;

  Init;
end;

constructor TTextParsing.Create(Text_: TP_String; Style_: TTextStyle; SpecialSymbol_: TListPascalString);
begin
  Create(Text_, Style_, SpecialSymbol_, SpacerSymbol.V);
end;

constructor TTextParsing.Create(Text_: TP_String; Style_: TTextStyle);
begin
  Create(Text_, Style_, nil, SpacerSymbol.V);
end;

constructor TTextParsing.Create(Text_: TP_String);
begin
  Create(Text_, tsText, nil, SpacerSymbol.V);
end;

destructor TTextParsing.Destroy;
var
  i: Integer;
begin
  if ParsingData.Cache.CommentDecls <> nil then
    begin
      for i := 0 to ParsingData.Cache.CommentDecls.Count - 1 do
        begin
          ParsingData.Cache.CommentDecls[i]^.Text := '';
          Dispose(ParsingData.Cache.CommentDecls[i]);
        end;
      DisposeObject(ParsingData.Cache.CommentDecls);
      ParsingData.Cache.CommentDecls := nil;
    end;

  if ParsingData.Cache.TextDecls <> nil then
    begin
      for i := 0 to ParsingData.Cache.TextDecls.Count - 1 do
        begin
          ParsingData.Cache.TextDecls[i]^.Text := '';
          Dispose(ParsingData.Cache.TextDecls[i]);
        end;
      DisposeObject(ParsingData.Cache.TextDecls);
      ParsingData.Cache.TextDecls := nil;
    end;

  if ParsingData.Cache.TokenDataList <> nil then
    begin
      for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
        begin
          ParsingData.Cache.TokenDataList[i]^.Text := '';
          Dispose(ParsingData.Cache.TokenDataList[i]);
        end;
      DisposeObject(ParsingData.Cache.TokenDataList);
      ParsingData.Cache.TokenDataList := nil;
    end;
  SetLength(ParsingData.Cache.CharToken, 0);

  TokenStatistics := NullTokenStatistics;
  DisposeObject(SpecialSymbol);
  inherited Destroy;
end;

procedure TTextParsing.Init;
begin

end;

function TTextParsing.Parsing: Boolean;
begin
  Result := False;
end;

procedure TTextParsing.Print;
var
  i: Integer;
  pt: PTokenData;
begin
  for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      pt := ParsingData.Cache.TokenDataList[i];
      DoStatus(PFormat('index: %d type: %s value: %s', [i, GetEnumName(TypeInfo(TTokenType), Ord(pt^.tokenType)), pt^.Text.Text]));
    end;
end;

procedure FillSymbol_Test_;
var
  t: TTextParsing;
  SM: TSymbolMatrix;
begin
  t := TTextParsing.Create('1,2,3,4,5,6,7,8,9', tsPascal);
  t.FillSymbolMatrix(3, 2, SM);
  DisposeObject(t);
end;

initialization

SpacerSymbol := TAtomString.Create(C_SpacerSymbol);

finalization

DisposeObjectAndNil(SpacerSymbol);

end.
