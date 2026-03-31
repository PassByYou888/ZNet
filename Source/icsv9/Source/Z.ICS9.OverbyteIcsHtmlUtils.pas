{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Creation:     Nov 2023
Description:  HTML utility functions, build and read web pages.
Version:      V9.5
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2003-2025 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Nov 16, 2023 V9.1  Baseline, split from OverbyteIcsFormDataDecoder.
                   Moved TextToHtmlText and IcsHtmlValuesToUnicode here to avoid
                     circular references.
                  Moved IcsFindHtmlCharset, IcsFindHtmlCodepage, IcsContentCodepage,
                     IcsHtmlToStr here from OverbyteIcsCharsetUtils.
Jul 06, 2024 V9.3 IcsContentCodepage now strips off any arguments following charset,
                    thanks to Murilo Beluco for finding this.
                  Added IcsPaseHttpHdr parse HTTP request or response header arguments,
                    returns number of arguments and array of arguments.
                  Added IcsFindHdrArg find an argument in an HTTP request or response
                    header, returns value.
                  Added IcsMimeIsTextual is a MIME type texual or printable text.
                  Added IcsHtmlToStrCh convert HTML page in stream to string with
                    correct codepage passing Charset.
Aug 15, 2025 V9.5 Added IcsEncodeHttpParam, IcsDecodeHttpParam, IcsEncHttp2Params
                    and IcsDecHttp2Params to RFC5987 UTF8/percent encode and decode
                    HTTP header parameters that contain non-ASCII characters, mainly
                    Content-Disposition with unicode file names.  Some headers have
                    double parameters, ASCII and Unicode versions.
                  IcsMimeIsTextual now accepts text without a /.
                  Added IcsWebReplaceCRLF which adds <BR> HTML line ending to
                    lines of text.
                  Added IcsHostDottedIp check if accessing server by IP address
                    instead of real host name.


Pending - move HtmlPageProducer and similar functions from HttpServer to here


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Z.ICS9.OverbyteIcsHtmlUtils;

{$I Include\Z.ICS9.OverbyteIcsDefs.inc}

interface

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.IniFiles{$ELSE}IniFiles{$ENDIF},
{$IFDEF COMPILER12_UP}
//    {$IFDEF RTL_NAMESPACES}System.AnsiStrings{$ELSE}AnsiStrings{$ENDIF},
{$ENDIF}
{$IFDEF COMPILER16_UP}
    {$IFDEF RTL_NAMESPACES}System.SyncObjs{$ELSE}SyncObjs{$ENDIF},
{$ENDIF}
    Z.ICS9.OverbyteIcsTypes, // for TBytes
    Z.ICS9.OverbyteIcsCharsetUtils,
    Z.ICS9.OverbyteIcsUtils,
    Z.ICS9.OverbyteIcsUrl;    { V9.5 }

type
    THdrArg = record                 { V9.3 parsed HTTP request or response header arguments }
        HName: String;
        HValue: String;
    end;
    THdrArgs = array of THdrArg;

function TextToHtmlText(const Src : UnicodeString) : String; overload;   { V8.04 }
function TextToHtmlText(const Src : RawByteString) : String; overload;   { V8.04 }
function RemoveHtmlSpecialChars(const S : String) : String;              { V8.04 }
function IcsWebReplaceCRLF(const Lines: String): String;                 { V9.5 }

{ find charset for HTML page in buffer from meta tags }
function IcsFindHtmlCharset(const HtmlData: TBytes; Count: Integer): String;      { V8.50, V8.64 }
{ find codepage for HTML page in buffer }
function IcsFindHtmlCodepage(const HtmlData: TBytes; Count: Integer; var BOMSize: Integer): Longword; overload;  { V8.50, V8.64 }
{ find codepage for HTML page in stream }
function IcsFindHtmlCodepage(HtmlStream: TStream; var BOMSize: Integer): Longword; overload;  { V8.50 }
{ find codepage from HTTP content-type header }
function IcsContentCodepage(ContentType: String): Longword;   { V8.50 }
{ convert HTML page in buffer to string with correct codepage }
function IcsHtmlToStr(const HtmlData: TBytes; Count: Integer; ACodePage: Longword; Entities: Boolean = False): UnicodeString; overload;   { V8.50, V8.64 }
{ convert HTML page in stream to string with correct codepage using codePage  }
function IcsHtmlToStr(HtmlStream: TStream; ACodePage: Longword; Entities: Boolean = False): UnicodeString; overload;    { V8.50 }
{ convert HTML page in stream to string with correct codepage using Content Header }
function IcsHtmlToStr(HtmlStream: TStream; const ContentHdr: String; Entities: Boolean = False): UnicodeString; overload;    { V8.50 }
{ convert HTML page in stream to string with correct codepage using Charset  }
function IcsHtmlToStrCh(HtmlStream: TStream; const Charset: String; Entities: Boolean = False): UnicodeString;   { V9.3 }

{ convert HTML single byte text to Unicode string, optionally detecting UTF8,
  optionally converting entities like &pound; and &#9741; to unicode, beware
  this includes &nbsp; and &amp; which may cause issues later }
function IcsHtmlValuesToUnicode(const ARawValue: RawByteString;  ASrcCodePage: LongWord = CP_ACP;  const ADetectUtf8 :
                                            Boolean = TRUE; const ADetectHtml : Boolean = TRUE): UnicodeString;     { V8.50 made public }

function IcsPaseHttpHdr(const Hdr: String; var HdrName: String; var HdrArgs: THdrArgs; Sep: String = ''): Integer;  { V9.3 }
function IcsMimeIsTextual(const MimeType: String): Boolean;                         { V9.3 }
function IcsFindHdrArg(const Hdr, ArgName: String; Sep: String = ''): String;       { V9.3 }
function IcsHostDottedIp (HostName: String): Boolean ;                              { V9.5 }

{ V9.5 RFC5987 UTF8/percent encode and decode an HTTP header parameters, and build header value }
function IcsEncodeHttpParam(const Param: String; const Language: String = ''): String;
function IcsDecodeHttpParam(const Param: String): String;
function IcsEncHttp2Params(const AName, AValue: String): String;
function IcsDecHttp2Params(const AHeader, AName: String): String;


implementation

const
  InvalidHtmlChars = [#$00..#$08, #$0B..#$0C, #$0E..#$1F, #$7F, #$80..#$9F];
  AlphaChars = ['A'..'Z', 'a'..'z'];
  NumChars = ['0'..'9'];
  HexChars = [ '0'..'9', 'A'..'F', 'a'..'f'];

var
  GHtmlEntityHashs : THashedStringList;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure InitEntityList(AList: TStrings); forward;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ convert HTML single byte text to Unicode string, optionally detecting UTF8,
  optionally converting entities like &pound; and &#9741; to unicode, beware
  this includes &nbsp; and &amp; which may cause issues later }
function IcsHtmlValuesToUnicode(
    const ARawValue   : RawByteString;
    ASrcCodePage      : LongWord = CP_ACP;
    const ADetectUtf8 : Boolean = TRUE;
    const ADetectHtml : Boolean = TRUE): UnicodeString;     { V8.50 made public }

    function HtmlEncodedTextToUnicode(const Src : RawByteString) : UnicodeString;
    var
        I       : Integer;
        V       : LongWord;
        SrcLen  : Integer;
        Ch      : AnsiChar;
     //   ChW     : WideChar;
        Tmp     : AnsiString;
        LHtmlEntityHashs : THashedStringList;
        OutLen  : Integer;         { V8.50 }

        procedure BuildResult(WC: WideChar);    { V8.50 }
        begin
            inc(OutLen);
            if OutLen > Length(Result) then SetLength(Result, OutLen + 1024);
            Result[OutLen] := WC;
        end;

        procedure CharFromEntity(E: RawByteString);        { V8.50 made procedure }
        var
            I: Integer;
        begin
            I := GHtmlEntityHashs.IndexOf(string(E));
            if I >= 0 then
                BuildResult(WideChar(Word(GHtmlEntityHashs.Objects[I])))
            else
                BuildResult('?');
        end;


        procedure UCS4CharToUnicode(const Ch: LongWord);   { V8.50 made procedure }
        begin
            if Ch > $0000FFFF then
            begin
                BuildResult(WideChar((((Ch - $00010000) shr 10) and $000003FF) or $D800));
                BuildResult(WideChar(((Ch - $00010000) and $000003FF)or $DC00));
            end
            else
                BuildResult(WideChar(Ch));
        end;

    begin
        Result := '';
        OutLen := 0;

        if GHtmlEntityHashs = nil then
        begin
            LHtmlEntityHashs := THashedStringList.Create;
            LHtmlEntityHashs.Capacity := 260;
            LHtmlEntityHashs.CaseSensitive := TRUE;
            InitEntityList(LHtmlEntityHashs);
            if IcsInterlockedCompareExchange(Pointer(GHtmlEntityHashs),
                                   Pointer(LHtmlEntityHashs), nil) <> nil then
                LHtmlEntityHashs.Free;
        end;

        SrcLen := Length(Src);
        SetLength(Result, SrcLen + 10);
        I := 1;
        while I <= SrcLen do    { V8.50 Replaced the string + string stuff }
        begin
            Ch := Src[I];
            if Ch in InvalidHtmlChars then
                Ch := '?';
            if Ch = '&' then
            begin
                Inc(I);
                if I <= SrcLen then
                begin
                    if Src[I] = '#' then // Numeric value
                    begin
                        V := Ord('?');
                        Inc(I);
                        if I <= SrcLen then
                        begin
                            if Src[I] in ['x', 'X'] then // Hex value
                            begin
                                Inc(I);
                                if I > SrcLen then
                                    Exit;
                                Tmp := '$';
                                while (I <= SrcLen) and (Src[I] <> ';') do
                                begin
                                    Tmp := Tmp + Src[I];
                                    Inc(I);
                                end;
                                V := StrToIntDef(String(Tmp), Ord('?'));
                            end
                            else begin // Decimal value
                                Tmp := '';
                                while (I <= SrcLen) and (Src[I] <> ';') do
                                begin
                                    Tmp := Tmp + Src[I];
                                    Inc(I);
                                end;
                                V := StrToIntDef(String(Tmp), Ord('?'));
                            end;
                        end;
                        if V > $10FFFF then // Max Unicode then
                            BuildResult('?')    { V8.50 }
                        else if V > High(Word) then // UCS4 Char
                            UCS4CharToUnicode(V)       { V8.50 }
                          //  Result := Result + UCS4CharToUnicode(V)
                        else
                            BuildResult(WideChar(V));    { V8.50 }
                         //   Result := Result + WideChar(V);
                    end
                    else begin // HTML Entity
                        Tmp := '';
                        while (I <= SrcLen) and (Src[I] <> ';') do
                        begin
                            Tmp := Tmp + Src[I];
                            Inc(I);
                        end;
                        CharFromEntity(Tmp);
                      //  if ChW = #0 then
                      //  Result := Result + '?'
                      //     else
                      //  Result := Result + ChW;
                    end;
                end;
            end
            else
                BuildResult(WideChar(Ch));    { V8.50 }
             //   Result := Result + WideChar(Ch);
            Inc(I);
        end;
        SetLength(Result, OutLen);   { V8.50 }
    end;

    function IsValidHtmlEncoding(S: RawByteString): Boolean;
    var
        HasHtmlChars : Boolean;
        I, J, Len    : Integer;
        Ch           : AnsiChar;
    begin
        HasHtmlChars := False;
        Result := False;
        I := 1;
        Len := Length(S);
        while I <= Len do
        begin
            Ch := S[I];
            if Ch in InvalidHtmlChars then
                Exit;
            if Ch = '&' then
            begin
                J := 0;
                Inc(I);
                if (I <= Len) and (S[I] = '#')then
                begin
                    Inc(I);
                    if (I <= Len) and (S[I] in ['x', 'X']) then
                    begin
                        Inc(I);
                        while (I <= Len) and (S[I] <> ';') do
                        begin
                            if not (S[I] in HexChars) then
                                Exit;
                            Inc(J);
                            if J > 8 then Exit;
                            Inc(I);
                        end;
                        if (I <= Len) and (J > 0) then
                            HasHtmlChars := True;
                    end
                    else begin
                        while (I <= Len) and (S[I] <> ';') do
                        begin
                            if not (S[I] in NumChars) then
                                Exit;
                            Inc(J);
                            if J > 8 then Exit;
                            Inc(I);
                        end;
                        if (I <= Len) and (J > 0) then
                            HasHtmlChars := True;
                    end;
                end
                else begin
                    while (I <= Len) and (S[I] <> ';') do
                    begin
                        if not (S[I] in AlphaChars) then
                            Exit;
                        Inc(J);
                        if J > 8 then Exit;
                        Inc(I);
                    end;
                    if (I <= Len) and (J > 2) then
                        HasHtmlChars := True;
                end;
            end;
            Inc(I);
        end;
        Result := HasHtmlChars;
    end;

begin
    if ADetectHtml and IsValidHtmlEncoding(ARawValue) then
    { We can't be sure it actually is HTML encoded, it's just very likely }
        Result := HtmlEncodedTextToUnicode(ARawValue)
    else begin
        if (ASrcCodePage <> CP_UTF8) and
           (ADetectUtf8 and IsUtf8Valid(ARawValue)) then
            ASrcCodePage := CP_UTF8;
        Result := AnsiToUnicode(ARawValue, ASrcCodePage);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure InitEntityList(AList: TStrings);
begin
  Alist.AddObject('nbsp', TObject(160)); {no-break space = non-breaking space,
                                  U+00A0 ISOnum}
  Alist.AddObject('iexcl', TObject(161)); {inverted exclamation mark, U+00A1 ISOnum}
  Alist.AddObject('cent', TObject(162)); {cent sign, U+00A2 ISOnum}
  Alist.AddObject('pound', TObject(163)); {pound sign, U+00A3 ISOnum}
  Alist.AddObject('curren', TObject(164)); {currency sign, U+00A4 ISOnum}
  Alist.AddObject('yen', TObject(165)); {yen sign = yuan sign, U+00A5 ISOnum}
  Alist.AddObject('brvbar', TObject(166)); {broken bar = broken vertical bar,
                                  U+00A6 ISOnum}
  Alist.AddObject('sect', TObject(167)); {section sign, U+00A7 ISOnum}
  Alist.AddObject('uml', TObject(168)); {diaeresis = spacing diaeresis,
                                  U+00A8 ISOdia}
  Alist.AddObject('copy', TObject(169)); {copyright sign, U+00A9 ISOnum}
  Alist.AddObject('ordf', TObject(170)); {feminine ordinal indicator, U+00AA ISOnum}
  Alist.AddObject('laquo', TObject(171)); {left-pointing double angle quotation mark
                                  = left pointing guillemet, U+00AB ISOnum}
  Alist.AddObject('not', TObject(172)); {not sign = angled dash,
                                  U+00AC ISOnum}
  Alist.AddObject('shy', TObject(173)); {soft hyphen = discretionary hyphen,
                                  U+00AD ISOnum}
  Alist.AddObject('reg', TObject(174)); {registered sign = registered trade mark sign,
                                  U+00AE ISOnum}
  Alist.AddObject('macr', TObject(175)); {macron = spacing macron = overline
                                  = APL overbar, U+00AF ISOdia}
  Alist.AddObject('deg', TObject(176)); {degree sign, U+00B0 ISOnum}
  Alist.AddObject('plusmn', TObject(177)); {plus-minus sign = plus-or-minus sign,
                                  U+00B1 ISOnum}
  Alist.AddObject('sup2', TObject(178)); {superscript two = superscript digit two
                                  = squared, U+00B2 ISOnum}
  Alist.AddObject('sup3', TObject(179)); {superscript three = superscript digit three
                                  = cubed, U+00B3 ISOnum}
  Alist.AddObject('acute', TObject(180)); {acute accent = spacing acute,
                                  U+00B4 ISOdia}
  Alist.AddObject('micro', TObject(181)); {micro sign, U+00B5 ISOnum}
  Alist.AddObject('para', TObject(182)); {pilcrow sign = paragraph sign,
                                  U+00B6 ISOnum}
  Alist.AddObject('middot', TObject(183)); {middle dot = Georgian comma
                                  = Greek middle dot, U+00B7 ISOnum}
  Alist.AddObject('cedil', TObject(184)); {cedilla = spacing cedilla, U+00B8 ISOdia}
  Alist.AddObject('sup1', TObject(185)); {superscript one = superscript digit one,
                                  U+00B9 ISOnum}
  Alist.AddObject('ordm', TObject(186)); {masculine ordinal indicator,
                                  U+00BA ISOnum}
  Alist.AddObject('raquo', TObject(187)); {right-pointing double angle quotation mark
                                  = right pointing guillemet, U+00BB ISOnum}
  Alist.AddObject('frac14', TObject(188)); {vulgar fraction one quarter
                                  = fraction one quarter, U+00BC ISOnum}
  Alist.AddObject('frac12', TObject(189)); {vulgar fraction one half
                                  = fraction one half, U+00BD ISOnum}
  Alist.AddObject('frac34', TObject(190)); {vulgar fraction three quarters
                                  = fraction three quarters, U+00BE ISOnum}
  Alist.AddObject('iquest', TObject(191)); {inverted question mark
                                  = turned question mark, U+00BF ISOnum}
  Alist.AddObject('Agrave', TObject(192)); {latin capital letter A with grave
                                  = latin capital letter A grave,
                                  U+00C0 ISOlat1}
  Alist.AddObject('Aacute', TObject(193)); {latin capital letter A with acute,
                                  U+00C1 ISOlat1}
  Alist.AddObject('Acirc', TObject(194)); {latin capital letter A with circumflex,
                                  U+00C2 ISOlat1}
  Alist.AddObject('Atilde', TObject(195)); {latin capital letter A with tilde,
                                  U+00C3 ISOlat1}
  Alist.AddObject('Auml', TObject(196)); {latin capital letter A with diaeresis,
                                  U+00C4 ISOlat1}
  Alist.AddObject('Aring', TObject(197)); {latin capital letter A with ring above
                                  = latin capital letter A ring,
                                  U+00C5 ISOlat1}
  Alist.AddObject('AElig', TObject(198)); {latin capital letter AE
                                  = latin capital ligature AE,
                                  U+00C6 ISOlat1}
  Alist.AddObject('Ccedil', TObject(199)); {latin capital letter C with cedilla,
                                  U+00C7 ISOlat1}
  Alist.AddObject('Egrave', TObject(200)); {latin capital letter E with grave,
                                  U+00C8 ISOlat1}
  Alist.AddObject('Eacute', TObject(201)); {latin capital letter E with acute,
                                  U+00C9 ISOlat1}
  Alist.AddObject('Ecirc', TObject(202)); {latin capital letter E with circumflex,
                                  U+00CA ISOlat1}
  Alist.AddObject('Euml', TObject(203)); {latin capital letter E with diaeresis,
                                  U+00CB ISOlat1}
  Alist.AddObject('Igrave', TObject(204)); {latin capital letter I with grave,
                                  U+00CC ISOlat1}
  Alist.AddObject('Iacute', TObject(205)); {latin capital letter I with acute,
                                  U+00CD ISOlat1}
  Alist.AddObject('Icirc', TObject(206)); {latin capital letter I with circumflex,
                                  U+00CE ISOlat1}
  Alist.AddObject('Iuml', TObject(207)); {latin capital letter I with diaeresis,
                                  U+00CF ISOlat1}
  Alist.AddObject('ETH', TObject(208)); {latin capital letter ETH, U+00D0 ISOlat1}
  Alist.AddObject('Ntilde', TObject(209)); {latin capital letter N with tilde,
                                  U+00D1 ISOlat1}
  Alist.AddObject('Ograve', TObject(210)); {latin capital letter O with grave,
                                  U+00D2 ISOlat1}
  Alist.AddObject('Oacute', TObject(211)); {latin capital letter O with acute,
                                  U+00D3 ISOlat1}
  Alist.AddObject('Ocirc', TObject(212)); {latin capital letter O with circumflex,
                                  U+00D4 ISOlat1}
  Alist.AddObject('Otilde', TObject(213)); {latin capital letter O with tilde,
                                  U+00D5 ISOlat1}
  Alist.AddObject('Ouml', TObject(214)); {latin capital letter O with diaeresis,
                                  U+00D6 ISOlat1}
  Alist.AddObject('times', TObject(215)); {multiplication sign, U+00D7 ISOnum}
  Alist.AddObject('Oslash', TObject(216)); {latin capital letter O with stroke
                                  = latin capital letter O slash,
                                  U+00D8 ISOlat1}
  Alist.AddObject('Ugrave', TObject(217)); {latin capital letter U with grave,
                                  U+00D9 ISOlat1}
  Alist.AddObject('Uacute', TObject(218)); {latin capital letter U with acute,
                                  U+00DA ISOlat1}
  Alist.AddObject('Ucirc', TObject(219)); {latin capital letter U with circumflex,
                                  U+00DB ISOlat1}
  Alist.AddObject('Uuml', TObject(220)); {latin capital letter U with diaeresis,
                                  U+00DC ISOlat1}
  Alist.AddObject('Yacute', TObject(221)); {latin capital letter Y with acute,
                                  U+00DD ISOlat1}
  Alist.AddObject('THORN', TObject(222)); {latin capital letter THORN,
                                  U+00DE ISOlat1}
  Alist.AddObject('szlig', TObject(223)); {latin small letter sharp s = ess-zed,
                                  U+00DF ISOlat1}
  Alist.AddObject('agrave', TObject(224)); {latin small letter a with grave
                                  = latin small letter a grave,
                                  U+00E0 ISOlat1}
  Alist.AddObject('aacute', TObject(225)); {latin small letter a with acute,
                                  U+00E1 ISOlat1}
  Alist.AddObject('acirc', TObject(226)); {latin small letter a with circumflex,
                                  U+00E2 ISOlat1}
  Alist.AddObject('atilde', TObject(227)); {latin small letter a with tilde,
                                  U+00E3 ISOlat1}
  Alist.AddObject('auml', TObject(228)); {latin small letter a with diaeresis,
                                  U+00E4 ISOlat1}
  Alist.AddObject('aring', TObject(229)); {latin small letter a with ring above
                                  = latin small letter a ring,
                                  U+00E5 ISOlat1}
  Alist.AddObject('aelig', TObject(230)); {latin small letter ae
                                  = latin small ligature ae, U+00E6 ISOlat1}
  Alist.AddObject('ccedil', TObject(231)); {latin small letter c with cedilla,
                                  U+00E7 ISOlat1}
  Alist.AddObject('egrave', TObject(232)); {latin small letter e with grave,
                                  U+00E8 ISOlat1}
  Alist.AddObject('eacute', TObject(233)); {latin small letter e with acute,
                                  U+00E9 ISOlat1}
  Alist.AddObject('ecirc', TObject(234)); {latin small letter e with circumflex,
                                  U+00EA ISOlat1}
  Alist.AddObject('euml', TObject(235)); {latin small letter e with diaeresis,
                                  U+00EB ISOlat1}
  Alist.AddObject('igrave', TObject(236)); {latin small letter i with grave,
                                  U+00EC ISOlat1}
  Alist.AddObject('iacute', TObject(237)); {latin small letter i with acute,
                                  U+00ED ISOlat1}
  Alist.AddObject('icirc', TObject(238)); {latin small letter i with circumflex,
                                  U+00EE ISOlat1}
  Alist.AddObject('iuml', TObject(239)); {latin small letter i with diaeresis,
                                  U+00EF ISOlat1}
  Alist.AddObject('eth', TObject(240)); {latin small letter eth, U+00F0 ISOlat1}
  Alist.AddObject('ntilde', TObject(241)); {latin small letter n with tilde,
                                  U+00F1 ISOlat1}
  Alist.AddObject('ograve', TObject(242)); {latin small letter o with grave,
                                  U+00F2 ISOlat1}
  Alist.AddObject('oacute', TObject(243)); {latin small letter o with acute,
                                  U+00F3 ISOlat1}
  Alist.AddObject('ocirc', TObject(244)); {latin small letter o with circumflex,
                                  U+00F4 ISOlat1}
  Alist.AddObject('otilde', TObject(245)); {latin small letter o with tilde,
                                  U+00F5 ISOlat1}
  Alist.AddObject('ouml', TObject(246)); {latin small letter o with diaeresis,
                                  U+00F6 ISOlat1}
  Alist.AddObject('divide', TObject(247)); {division sign, U+00F7 ISOnum}
  Alist.AddObject('oslash', TObject(248)); {latin small letter o with stroke,
                                  = latin small letter o slash,
                                  U+00F8 ISOlat1}
  Alist.AddObject('ugrave', TObject(249)); {latin small letter u with grave,
                                  U+00F9 ISOlat1}
  Alist.AddObject('uacute', TObject(250)); {latin small letter u with acute,
                                  U+00FA ISOlat1}
  Alist.AddObject('ucirc', TObject(251)); {latin small letter u with circumflex,
                                  U+00FB ISOlat1}
  Alist.AddObject('uuml', TObject(252)); {latin small letter u with diaeresis,
                                  U+00FC ISOlat1}
  Alist.AddObject('yacute', TObject(253)); {latin small letter y with acute,
                                  U+00FD ISOlat1}
  Alist.AddObject('thorn', TObject(254)); {latin small letter thorn,
                                  U+00FE ISOlat1}
  Alist.AddObject('yuml', TObject(255)); {latin small letter y with diaeresis,
                                  U+00FF ISOlat1}
  {C0 Controls and Basic Latin}
  Alist.AddObject('quot', TObject(34)); { quotation mark, U+0022 ISOnum}
  Alist.AddObject('amp', TObject(38)); { ampersand, U+0026 ISOnum}
  Alist.AddObject('lt', TObject(60)); { less-than sign, U+003C ISOnum}
  Alist.AddObject('gt', TObject(62)); { greater-than sign, U+003E ISOnum}
  Alist.AddObject('apos', TObject(39)); { apostrophe = APL quote, U+0027 ISOnum}  // XML

{Latin Extended-A}
  Alist.AddObject('OElig', TObject(338)); { latin capital ligature OE,
                                    U+0152 ISOlat2}
  Alist.AddObject('oelig', TObject(339)); { latin small ligature oe, U+0153 ISOlat2}
{ligature is a misnomer, this is a separate character in some languages}
  Alist.AddObject('Scaron', TObject(352)); { latin capital letter S with caron,
                                    U+0160 ISOlat2}
  Alist.AddObject('scaron', TObject(353)); { latin small letter s with caron,
                                    U+0161 ISOlat2}
  Alist.AddObject('Yuml', TObject(376)); { latin capital letter Y with diaeresis,
                                    U+0178 ISOlat2}

{Spacing Modifier Letters}
  Alist.AddObject('circ', TObject(710)); { modifier letter circumflex accent,
                                    U+02C6 ISOpub}
  Alist.AddObject('tilde', TObject(732)); { small tilde, U+02DC ISOdia}

{General Punctuation}
  Alist.AddObject('ensp', TObject(8194)); {en space, U+2002 ISOpub}
  Alist.AddObject('emsp', TObject(8195)); {em space, U+2003 ISOpub}
  Alist.AddObject('thinsp', TObject(8201)); {thin space, U+2009 ISOpub}
  Alist.AddObject('zwnj', TObject(8204)); {zero width non-joiner,
                                    U+200C NEW RFC 2070}
  Alist.AddObject('zwj', TObject(8205)); {zero width joiner, U+200D NEW RFC 2070}
  Alist.AddObject('lrm', TObject(8206)); {left-to-right mark, U+200E NEW RFC 2070}
  Alist.AddObject('rlm', TObject(8207)); {right-to-left mark, U+200F NEW RFC 2070}
  Alist.AddObject('ndash', TObject(8211)); {en dash, U+2013 ISOpub}
  Alist.AddObject('mdash', TObject(8212)); {em dash, U+2014 ISOpub}
  Alist.AddObject('lsquo', TObject(8216)); {left single quotation mark,
                                    U+2018 ISOnum}
  Alist.AddObject('rsquo', TObject(8217)); {right single quotation mark,
                                    U+2019 ISOnum}
  Alist.AddObject('sbquo', TObject(8218)); {single low-9 quotation mark, U+201A NEW}
  Alist.AddObject('ldquo', TObject(8220)); {left double quotation mark,
                                    U+201C ISOnum}
  Alist.AddObject('rdquo', TObject(8221)); {right double quotation mark,
                                    U+201D ISOnum}
  Alist.AddObject('bdquo', TObject(8222)); {double low-9 quotation mark, U+201E NEW}
  Alist.AddObject('dagger', TObject(8224)); {dagger, U+2020 ISOpub}
  Alist.AddObject('Dagger', TObject(8225)); {double dagger, U+2021 ISOpub}
  Alist.AddObject('permil', TObject(8240)); {per mille sign, U+2030 ISOtech}
  Alist.AddObject('lsaquo', TObject(8249)); {single left-pointing angle quotation mark,
                                    U+2039 ISO proposed}
{lsaquo is proposed but not yet ISO standardized}
  Alist.AddObject('rsaquo', TObject(8250)); {single right-pointing angle quotation mark,
                                    U+203A ISO proposed}
{rsaquo is proposed but not yet ISO standardized}

{Currency Symbols}
  Alist.AddObject('euro', TObject(8364)); { euro sign, U+20AC NEW}


{Latin Extended-B}
  Alist.AddObject('fnof', TObject(402)); {latin small letter f with hook = function
                                    = florin, U+0192 ISOtech}

{Greek}
  Alist.AddObject('Alpha', TObject(913)); {greek capital letter alpha, U+0391}
  Alist.AddObject('Beta', TObject(914)); {greek capital letter beta, U+0392}
  Alist.AddObject('Gamma', TObject(915)); {greek capital letter gamma,
                                    U+0393 ISOgrk3}
  Alist.AddObject('Delta', TObject(916)); {greek capital letter delta,
                                    U+0394 ISOgrk3}
  Alist.AddObject('Epsilon', TObject(917)); {greek capital letter epsilon, U+0395}
  Alist.AddObject('Zeta', TObject(918)); {greek capital letter zeta, U+0396}
  Alist.AddObject('Eta', TObject(919)); {greek capital letter eta, U+0397}
  Alist.AddObject('Theta', TObject(920)); {greek capital letter theta,
                                    U+0398 ISOgrk3}
  Alist.AddObject('Iota', TObject(921)); {greek capital letter iota, U+0399}
  Alist.AddObject('Kappa', TObject(922)); {greek capital letter kappa, U+039A}
  Alist.AddObject('Lambda', TObject(923)); {greek capital letter lamda,
                                    U+039B ISOgrk3}
  Alist.AddObject('Mu', TObject(924)); {greek capital letter mu, U+039C}
  Alist.AddObject('Nu', TObject(925)); {greek capital letter nu, U+039D}
  Alist.AddObject('Xi', TObject(926)); {greek capital letter xi, U+039E ISOgrk3}
  Alist.AddObject('Omicron', TObject(927)); {greek capital letter omicron, U+039F}
  Alist.AddObject('Pi', TObject(928)); {greek capital letter pi, U+03A0 ISOgrk3}
  Alist.AddObject('Rho', TObject(929)); {greek capital letter rho, U+03A1}
{there is no Sigmaf, and no U+03A2 character either}
  Alist.AddObject('Sigma', TObject(931)); {greek capital letter sigma,
                                    U+03A3 ISOgrk3}
  Alist.AddObject('Tau', TObject(932)); {greek capital letter tau, U+03A4}
  Alist.AddObject('Upsilon', TObject(933)); {greek capital letter upsilon,
                                    U+03A5 ISOgrk3}
  Alist.AddObject('Phi', TObject(934)); {greek capital letter phi,
                                    U+03A6 ISOgrk3}
  Alist.AddObject('Chi', TObject(935)); {greek capital letter chi, U+03A7}
  Alist.AddObject('Psi', TObject(936)); {greek capital letter psi,
                                    U+03A8 ISOgrk3}
  Alist.AddObject('Omega', TObject(937)); {greek capital letter omega,
                                    U+03A9 ISOgrk3}

  Alist.AddObject('alpha', TObject(945)); {greek small letter alpha,
                                    U+03B1 ISOgrk3}
  Alist.AddObject('beta', TObject(946)); {greek small letter beta, U+03B2 ISOgrk3}
  Alist.AddObject('gamma', TObject(947)); {greek small letter gamma,
                                    U+03B3 ISOgrk3}
  Alist.AddObject('delta', TObject(948)); {greek small letter delta,
                                    U+03B4 ISOgrk3}
  Alist.AddObject('epsilon', TObject(949)); {greek small letter epsilon,
                                    U+03B5 ISOgrk3}
  Alist.AddObject('zeta', TObject(950)); {greek small letter zeta, U+03B6 ISOgrk3}
  Alist.AddObject('eta', TObject(951)); {greek small letter eta, U+03B7 ISOgrk3}
  Alist.AddObject('theta', TObject(952)); {greek small letter theta,
                                    U+03B8 ISOgrk3}
  Alist.AddObject('iota', TObject(953)); {greek small letter iota, U+03B9 ISOgrk3}
  Alist.AddObject('kappa', TObject(954)); {greek small letter kappa,
                                    U+03BA ISOgrk3}
  Alist.AddObject('lambda', TObject(955)); {greek small letter lamda,
                                    U+03BB ISOgrk3}
  Alist.AddObject('mu', TObject(956)); {greek small letter mu, U+03BC ISOgrk3}
  Alist.AddObject('nu', TObject(957)); {greek small letter nu, U+03BD ISOgrk3}
  Alist.AddObject('xi', TObject(958)); {greek small letter xi, U+03BE ISOgrk3}
  Alist.AddObject('omicron', TObject(959)); {greek small letter omicron, U+03BF NEW}
  Alist.AddObject('pi', TObject(960)); {greek small letter pi, U+03C0 ISOgrk3}
  Alist.AddObject('rho', TObject(961)); {greek small letter rho, U+03C1 ISOgrk3}
  Alist.AddObject('sigmaf', TObject(962)); {greek small letter final sigma,
                                    U+03C2 ISOgrk3}
  Alist.AddObject('sigma', TObject(963)); {greek small letter sigma,
                                    U+03C3 ISOgrk3}
  Alist.AddObject('tau', TObject(964)); {greek small letter tau, U+03C4 ISOgrk3}
  Alist.AddObject('upsilon', TObject(965)); {greek small letter upsilon,
                                    U+03C5 ISOgrk3}
  Alist.AddObject('phi', TObject(966)); {greek small letter phi, U+03C6 ISOgrk3}
  Alist.AddObject('chi', TObject(967)); {greek small letter chi, U+03C7 ISOgrk3}
  Alist.AddObject('psi', TObject(968)); {greek small letter psi, U+03C8 ISOgrk3}
  Alist.AddObject('omega', TObject(969)); {greek small letter omega,
                                    U+03C9 ISOgrk3}
  Alist.AddObject('thetasym', TObject(977)); {greek theta symbol,
                                    U+03D1 NEW}
  Alist.AddObject('upsih', TObject(978)); {greek upsilon with hook symbol,
                                    U+03D2 NEW}
  Alist.AddObject('piv', TObject(982)); {greek pi symbol, U+03D6 ISOgrk3}

{General Punctuation}
  Alist.AddObject('bull', TObject(8226)); {bullet = black small circle,
                                     U+2022 ISOpub }
{bullet is NOT the same as bullet operator, U+2219}
  Alist.AddObject('hellip', TObject(8230)); {horizontal ellipsis = three dot leader,
                                     U+2026 ISOpub }
  Alist.AddObject('prime', TObject(8242)); {prime = minutes = feet, U+2032 ISOtech}
  Alist.AddObject('Prime', TObject(8243)); {double prime = seconds = inches,
                                     U+2033 ISOtech}
  Alist.AddObject('oline', TObject(8254)); {overline = spacing overscore,
                                     U+203E NEW}
  Alist.AddObject('frasl', TObject(8260)); {fraction slash, U+2044 NEW}

{Letterlike Symbols}
  Alist.AddObject('weierp', TObject(8472)); {script capital P = power set
                                     = Weierstrass p, U+2118 ISOamso}
  Alist.AddObject('image', TObject(8465)); {black-letter capital I = imaginary part,
                                     U+2111 ISOamso}
  Alist.AddObject('real', TObject(8476)); {black-letter capital R = real part symbol,
                                     U+211C ISOamso}
  Alist.AddObject('trade', TObject(8482)); {trade mark sign, U+2122 ISOnum}
  Alist.AddObject('alefsym', TObject(8501)); {alef symbol = first transfinite cardinal,
                                     U+2135 NEW}
{alef symbol is NOT the same as hebrew letter alef,
     U+05D0 although the same glyph could be used to depict both characters}

{Arrows}
  Alist.AddObject('larr', TObject(8592)); {leftwards arrow, U+2190 ISOnum}
  Alist.AddObject('uarr', TObject(8593)); {upwards arrow, U+2191 ISOnum}
  Alist.AddObject('rarr', TObject(8594)); {rightwards arrow, U+2192 ISOnum}
  Alist.AddObject('darr', TObject(8595)); {downwards arrow, U+2193 ISOnum}
  Alist.AddObject('harr', TObject(8596)); {left right arrow, U+2194 ISOamsa}
  Alist.AddObject('crarr', TObject(8629)); {downwards arrow with corner leftwards
                                     = carriage return, U+21B5 NEW}
  Alist.AddObject('lArr', TObject(8656)); {leftwards double arrow, U+21D0 ISOtech}
{Unicode does not say that lArr is the same as the 'is implied by' arrow
    but also does not have any other character for that function. So lArr can
    be used for 'is implied by' as ISOtech suggests}
  Alist.AddObject('uArr', TObject(8657)); {upwards double arrow, U+21D1 ISOamsa}
  Alist.AddObject('rArr', TObject(8658)); {rightwards double arrow,
                                     U+21D2 ISOtech}
{Unicode does not say this is the 'implies' character but does not have
     another character with this function so rArr can be used for 'implies'
     as ISOtech suggests}
  Alist.AddObject('dArr', TObject(8659)); {downwards double arrow, U+21D3 ISOamsa}
  Alist.AddObject('hArr', TObject(8660)); {left right double arrow,
                                     U+21D4 ISOamsa}

{Mathematical Operators}
  Alist.AddObject('forall', TObject(8704)); {for all, U+2200 ISOtech}
  Alist.AddObject('part', TObject(8706)); {partial differential, U+2202 ISOtech }
  Alist.AddObject('exist', TObject(8707)); {there exists, U+2203 ISOtech}
  Alist.AddObject('empty', TObject(8709)); {empty set = null set, U+2205 ISOamso}
  Alist.AddObject('nabla', TObject(8711)); {nabla = backward difference,
                                     U+2207 ISOtech}
  Alist.AddObject('isin', TObject(8712)); {element of, U+2208 ISOtech}
  Alist.AddObject('notin', TObject(8713)); {not an element of, U+2209 ISOtech}
  Alist.AddObject('ni', TObject(8715)); {contains as member, U+220B ISOtech}
  Alist.AddObject('prod', TObject(8719)); {n-ary product = product sign,
                                     U+220F ISOamsb}
{prod is NOT the same character as U+03A0 'greek capital letter pi' though
     the same glyph might be used for both}
  Alist.AddObject('sum', TObject(8721)); {n-ary summation, U+2211 ISOamsb}
{sum is NOT the same character as U+03A3 'greek capital letter sigma'
     though the same glyph might be used for both}
  Alist.AddObject('minus', TObject(8722)); {minus sign, U+2212 ISOtech}
  Alist.AddObject('lowast', TObject(8727)); {asterisk operator, U+2217 ISOtech}
  Alist.AddObject('radic', TObject(8730)); {square root = radical sign,
                                     U+221A ISOtech}
  Alist.AddObject('prop', TObject(8733)); {proportional to, U+221D ISOtech}
  Alist.AddObject('infin', TObject(8734)); {infinity, U+221E ISOtech}
  Alist.AddObject('ang', TObject(8736)); {angle, U+2220 ISOamso}
  Alist.AddObject('and', TObject(8743)); {logical and = wedge, U+2227 ISOtech}
  Alist.AddObject('or', TObject(8744)); {logical or = vee, U+2228 ISOtech}
  Alist.AddObject('cap', TObject(8745)); {intersection = cap, U+2229 ISOtech}
  Alist.AddObject('cup', TObject(8746)); {union = cup, U+222A ISOtech}
  Alist.AddObject('int', TObject(8747)); {integral, U+222B ISOtech}
  Alist.AddObject('there4', TObject(8756)); {therefore, U+2234 ISOtech}
  Alist.AddObject('sim', TObject(8764)); {tilde operator = varies with = similar to,
                                     U+223C ISOtech}
{tilde operator is NOT the same character as the tilde, U+007E,
     although the same glyph might be used to represent both }
  Alist.AddObject('cong', TObject(8773)); {approximately equal to, U+2245 ISOtech}
  Alist.AddObject('asymp', TObject(8776)); {almost equal to = asymptotic to,
                                     U+2248 ISOamsr}
  Alist.AddObject('ne', TObject(8800)); {not equal to, U+2260 ISOtech}
  Alist.AddObject('equiv', TObject(8801)); {identical to, U+2261 ISOtech}
  Alist.AddObject('le', TObject(8804)); {less-than or equal to, U+2264 ISOtech}
  Alist.AddObject('ge', TObject(8805)); {greater-than or equal to,
                                     U+2265 ISOtech}
  Alist.AddObject('sub', TObject(8834)); {subset of, U+2282 ISOtech}
  Alist.AddObject('sup', TObject(8835)); {superset of, U+2283 ISOtech}
  Alist.AddObject('nsub', TObject(8836)); {not a subset of, U+2284 ISOamsn}
  Alist.AddObject('sube', TObject(8838)); {subset of or equal to, U+2286 ISOtech}
  Alist.AddObject('supe', TObject(8839)); {superset of or equal to,
                                     U+2287 ISOtech}
  Alist.AddObject('oplus', TObject(8853)); {circled plus = direct sum,
                                     U+2295 ISOamsb}
  Alist.AddObject('otimes', TObject(8855)); {circled times = vector product,
                                     U+2297 ISOamsb}
  Alist.AddObject('perp', TObject(8869)); {up tack = orthogonal to = perpendicular,
                                     U+22A5 ISOtech}
  Alist.AddObject('sdot', TObject(8901)); {dot operator, U+22C5 ISOamsb}
{dot operator is NOT the same character as U+00B7 middle dot}

{Miscellaneous Technical}
  Alist.AddObject('lceil', TObject(8968)); {left ceiling = APL upstile,
                                     U+2308 ISOamsc }
  Alist.AddObject('rceil', TObject(8969)); {right ceiling, U+2309 ISOamsc }
  Alist.AddObject('lfloor', TObject(8970)); {left floor = APL downstile,
                                     U+230A ISOamsc }
  Alist.AddObject('rfloor', TObject(8971)); {right floor, U+230B ISOamsc }
  Alist.AddObject('lang', TObject(9001)); {left-pointing angle bracket = bra,
                                     U+2329 ISOtech}
{lang is NOT the same character as U+003C 'less than sign'
     or U+2039 'single left-pointing angle quotation mark'}
  Alist.AddObject('rang', TObject(9002)); {right-pointing angle bracket = ket,
                                     U+232A ISOtech}
{rang is NOT the same character as U+003E 'greater than sign'
     or U+203A 'single right-pointing angle quotation mark'}

{Geometric Shapes}
  Alist.AddObject('loz', TObject(9674)); {lozenge, U+25CA ISOpub}

{Miscellaneous Symbols}
  Alist.AddObject('spades', TObject(9824)); {black spade suit, U+2660 ISOpub}
{black here seems to mean filled as opposed to hollow}
  Alist.AddObject('clubs', TObject(9827)); {black club suit = shamrock,
                                     U+2663 ISOpub}
  Alist.AddObject('hearts', TObject(9829)); {black heart suit = valentine,
                                     U+2665 ISOpub}
  Alist.AddObject('diams', TObject(9830)) {black diamond suit, U+2666 ISOpub}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TextToHtmlText(const Src: RawByteString) : String;
begin
    { Convert the ANSI string to Unicode, HTML entities represent           }
    { iso-8859-1 (Latin1) and Unicode code points                           }
    Result := TextToHtmlText(UnicodeString(Src));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Convert a string in Windows character set to HTML texte. That is replace  }
{ all character with code between 160 and 255 by special sequences.         }
{ For example, 'fête' is replaced by 'f&ecirc;te'                           }
{ Also handle '<', '>', quote and double quote                              }
{ Replace multiple spaces by a single space followed by the required number }
{ of non-breaking-spaces (&nbsp;)                                           }
{ Replace TAB by a non-breaking-space.                                      }
function TextToHtmlText(const Src : UnicodeString) : String;
const
    HtmlSpecialChars : array [160..255] of String = (
        'nbsp'   , { #160 no-break space = non-breaking space               }
        'iexcl'  , { #161 inverted exclamation mark                         }
        'cent'   , { #162 cent sign                                         }
        'pound'  , { #163 pound sign                                        }
        'curren' , { #164 currency sign                                     }
        'yen'    , { #165 yen sign = yuan sign                              }
        'brvbar' , { #166 broken bar = broken vertical bar,                 }
        'sect'   , { #167 section sign                                      }
        'uml'    , { #168 diaeresis = spacing diaeresis                     }
        'copy'   , { #169 copyright sign                                    }
        'ordf'   , { #170 feminine ordinal indicator                        }
        'laquo'  , { #171 left-pointing double angle quotation mark         }
        'not'    , { #172 not sign                                          }
        'shy'    , { #173 soft hyphen = discretionary hyphen,               }
        'reg'    , { #174 registered sign = registered trade mark sign,     }
        'macr'   , { #175 macron = spacing macron = overline = APL overbar  }
        'deg'    , { #176 degree sign                                       }
        'plusmn' , { #177 plus-minus sign = plus-or-minus sign,             }
        'sup2'   , { #178 superscript two = superscript digit two = squared }
        'sup3'   , { #179 superscript three = superscript digit three = cubed }
        'acute'  , { #180 acute accent = spacing acute,                     }
        'micro'  , { #181 micro sign                                        }
        'para'   , { #182 pilcrow sign = paragraph sign,                    }
        'middot' , { #183 middle dot = Georgian comma = Greek middle dot    }
        'cedil'  , { #184 cedilla = spacing cedilla                         }
        'sup1'   , { #185 superscript one = superscript digit one           }
        'ordm'   , { #186 masculine ordinal indicator,                      }
        'raquo'  , { #187 right-pointing double angle quotation mark = right pointing guillemet }
        'frac14' , { #188 vulgar fraction one quarter = fraction one quarter}
        'frac12' , { #189 vulgar fraction one half = fraction one half      }
        'frac34' , { #190 vulgar fraction three quarters = fraction three quarters }
        'iquest' , { #191 inverted question mark = turned question mark     }
        'Agrave' , { #192 latin capital letter A with grave = latin capital letter A grave, }
        'Aacute' , { #193 latin capital letter A with acute,                }
        'Acirc'  , { #194 latin capital letter A with circumflex,           }
        'Atilde' , { #195 latin capital letter A with tilde,                }
        'Auml'   , { #196 latin capital letter A with diaeresis,            }
        'Aring'  , { #197 latin capital letter A with ring above = latin capital letter A ring, }
        'AElig'  , { #198 latin capital letter AE = latin capital ligature AE, }
        'Ccedil' , { #199 latin capital letter C with cedilla,              }
        'Egrave' , { #200 latin capital letter E with grave,                }
        'Eacute' , { #201 latin capital letter E with acute,                }
        'Ecirc'  , { #202 latin capital letter E with circumflex,           }
        'Euml'   , { #203 latin capital letter E with diaeresis,            }
        'Igrave' , { #204 latin capital letter I with grave,                }
        'Iacute' , { #205 latin capital letter I with acute,                }
        'Icirc'  , { #206 latin capital letter I with circumflex,           }
        'Iuml'   , { #207 latin capital letter I with diaeresis,            }
        'ETH'    , { #208 latin capital letter ETH                          }
        'Ntilde' , { #209 latin capital letter N with tilde,                }
        'Ograve' , { #210 latin capital letter O with grave,                }
        'Oacute' , { #211 latin capital letter O with acute,                }
        'Ocirc'  , { #212 latin capital letter O with circumflex,           }
        'Otilde' , { #213 latin capital letter O with tilde,                }
        'Ouml'   , { #214 latin capital letter O with diaeresis,            }
        'times'  , { #215 multiplication sign                               }
        'Oslash' , { #216 latin capital letter O with stroke = latin capital letter O slash, }
        'Ugrave' , { #217 latin capital letter U with grave,                }
        'Uacute' , { #218 latin capital letter U with acute,                }
        'Ucirc'  , { #219 latin capital letter U with circumflex,           }
        'Uuml'   , { #220 latin capital letter U with diaeresis,            }
        'Yacute' , { #221 latin capital letter Y with acute,                }
        'THORN'  , { #222 latin capital letter THORN,                       }
        'szlig'  , { #223 latin small letter sharp s = ess-zed,             }
        'agrave' , { #224 latin small letter a with grave = latin small letter a grave, }
        'aacute' , { #225 latin small letter a with acute,                  }
        'acirc'  , { #226 latin small letter a with circumflex,             }
        'atilde' , { #227 latin small letter a with tilde,                  }
        'auml'   , { #228 latin small letter a with diaeresis,              }
        'aring'  , { #229 latin small letter a with ring above = latin small letter a ring, }
        'aelig'  , { #230 latin small letter ae = latin small ligature ae   }
        'ccedil' , { #231 latin small letter c with cedilla,                }
        'egrave' , { #232 latin small letter e with grave,                  }
        'eacute' , { #233 latin small letter e with acute,                  }
        'ecirc'  , { #234 latin small letter e with circumflex,             }
        'euml'   , { #235 latin small letter e with diaeresis,              }
        'igrave' , { #236 latin small letter i with grave,                  }
        'iacute' , { #237 latin small letter i with acute,                  }
        'icirc'  , { #238 latin small letter i with circumflex,             }
        'iuml'   , { #239 latin small letter i with diaeresis,              }
        'eth'    , { #240 latin small letter eth                            }
        'ntilde' , { #241 latin small letter n with tilde,                  }
        'ograve' , { #242 latin small letter o with grave,                  }
        'oacute' , { #243 latin small letter o with acute,                  }
        'ocirc'  , { #244 latin small letter o with circumflex,             }
        'otilde' , { #245 latin small letter o with tilde,                  }
        'ouml'   , { #246 latin small letter o with diaeresis,              }
        'divide' , { #247 division sign                                     }
        'oslash' , { #248 latin small letter o with stroke, = latin small letter o slash, }
        'ugrave' , { #249 latin small letter u with grave,                  }
        'uacute' , { #250 latin small letter u with acute,                  }
        'ucirc'  , { #251 latin small letter u with circumflex,             }
        'uuml'   , { #252 latin small letter u with diaeresis,              }
        'yacute' , { #253 latin small letter y with acute,                  }
        'thorn'  , { #254 latin small letter thorn,                         }
        'yuml');   { #255 latin small letter y with diaeresis,              }
var
    I, J : Integer;
    Sub  : String;
begin
    Result := '';
    I := 1;
    while I <= Length(Src) do begin
        J   := I;
        Sub := '';
        while (I <= Length(Src)) and (Ord(Src[I]) < Low(HtmlSpecialChars)) do begin
            case Src[I] of
            ' '  : begin
                       if (I > 1) and (Src[I - 1] = ' ') then begin
                           { Replace multiple spaces by &nbsp; }
                           while (I <= Length(Src)) and (Src[I] = ' ') do begin
                               Sub := Sub + '&nbsp;';
                               Inc(I);
                           end;
                           Dec(I);
                       end
                       else
                           Inc(I);
                   end;
            '<'  : Sub := '&lt;';
            '>'  : Sub := '&gt;';
            '''' : sub := '&#39;';
            '"'  : Sub := '&#34;';
            '&'  : Sub := '&amp;';
            #9   : Sub := '&nbsp;';
            #10  : Sub := #10'<BR>';
            else
                Inc(I);
            end;
            if Length(Sub) > 0 then begin
                Result := Result + Copy(Src, J, I - J) + Sub;
                Inc(I);
                J      := I;
                Sub    := '';
            end;
        end;

        if I > Length(Src) then begin
            Result := Result + Copy(Src, J, I - J);
            Exit;
        end;
        if Ord(Src[I]) > 255 then
            Result := Result + Copy(Src, J, I - J) + '&#' + IntToStr(Ord(Src[I])) + ';'
        else
            Result := Result + Copy(Src, J, I - J) + '&' + HtmlSpecialChars[Ord(Src[I])] + ';';
        Inc(I);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function RemoveHtmlSpecialChars(const S : String) : String;
const
    SpecialChars : array [1..5] of char   = ('<',  '>',  '&',   '''',  '"');
    HtmlChars    : array [1..5] of String = ('lt', 'gt', 'amp', '#39', 'quot');
var
    I, J : Integer;
begin
    Result := '';
    I      := 1;
    while I <= Length(S) do begin
        J := Low(SpecialChars);
        while J <= High(SpecialChars) do begin
            if S[I] = SpecialChars[J] then
                break;
            J := J + 1;
        end;
        if J <= High(SpecialChars) then
            Result := Result + '&' + HtmlChars[J] + ';'
        else
            Result := Result + S[I];
        I := I + 1;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ find charset for HTML page in buffer from meta tags }
function IcsFindHtmlCharset(const HtmlData: TBytes; Count: Integer): String;    { V8.50, V8.64 }
var
    offset, I: Integer;
    value: String;

    procedure SkipBlanks;
    begin
        while (IsSpaceOrCRLF(AnsiChar(HtmlData[offset])) or (HtmlData[offset] = Byte('/'))) and (offset < count) do
            inc(offset);
    end;

    procedure FindValue;
    var
        delim: Byte;
        start: Integer;
    begin
        value := '';
        while (HtmlData[offset] <> Byte('=')) and (offset < count) do
            inc(offset);
        inc(offset);
        delim := HtmlData[offset];
        inc(offset);
        start := offset;
        while (HtmlData[offset] <> delim) and (offset < count) do
            inc(offset);
        if (HtmlData[offset] <> delim) then
        exit;
        IcsMoveTBytesToString(HtmlData, start, value, 1, offset - start);
        value := IcsLowercase(value);
        inc(offset);
    end;

begin
    Result := '';
    if Count > Length(HtmlData) then
        Count := Length(HtmlData);  { sanity check }

 { look for meta charset header }
    offset := 0;
    while (offset < Count) do begin

      { skip comments '<!-- xx --> }
        if HtmlData[offset] = Byte('<') then begin
            if IcsTbytesStarts(TBytes(@HtmlData[Offset]), '<!--'#0) then begin
                offset := offset + 4;
                while (HtmlData[offset] <> Byte('>')) and (HtmlData[offset - 2] <> Byte('-'))  and (offset < count) do
                    inc(offset);
            end;
        end ;

     { check for meta tag, then attribute }
     { ie <meta charset="UTF-8"/>   }
     { ie <meta http-equiv="Content-Type" content="text/html; charset=utf-8"> }
     { ie <meta content="text/html; charset=utf-8" http-equiv="Content-Type" > }
     { ignore <meta content="en-gb" http-equiv="Content-Language">  }
        if HtmlData[offset] = Byte('<') then begin
            if IcsTbytesStarts(TBytes(@HtmlData [Offset]), '<meta'#0) then begin  // META, Meta or meta
                offset := offset + 5;
                SkipBlanks;
                if IcsTbytesStarts(TBytes(@HtmlData[Offset]), 'charset'#0) then begin
                    FindValue;
                    if value <> '' then begin
                        Result := value;
                        Exit;
                    end;
                 end;
                SkipBlanks;
                if IcsTbytesStarts(TBytes(@HtmlData[Offset]), 'http-equiv='#0) then begin
                  //  FindValue;
                 //   if value = 'content-type' then pragma := True;  // don't really care
                 end;
                SkipBlanks;
                if IcsTbytesStarts(TBytes(@HtmlData[Offset]), 'content='#0) then begin
                    FindValue;
                    I := Pos('charset=', value);
                    if I > 0 then begin
                        Result := Copy(value, I + 8, 999);
                        Exit;
                    end;
                end;
            end;
        end ;
        inc(offset);
    end ;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ find codepage for HTML page in buffer }
function IcsFindHtmlCodepage(const HtmlData: TBytes; Count: Integer; var BOMSize: Integer): Longword;   { V8.50, V8.64 }
var
    charset: String;
    CharsetDetectResult: TCharsetDetectResult;
begin
    BOMSize := 0;
    Result := 0;
    if Count > Length(HtmlData) then
        Count := Length(HtmlData);  { sanity check }

 { html may have BOM bytes are the front, very easy }
    Result := IcsGetBufferCodepage (@HtmlData[0], 4, BOMSize);
    if BOMSize <> 0 then
        Exit;   // found it already, remember to remove it

 { check two byte unicode }
    if (HtmlData[1] = 0) and (HtmlData[3] = 0) then begin
        Result := CP_UTF16;
        Exit;
    end;

 { look for charset in meta headers }
    charset := IcsFindHtmlCharset(HtmlData, Count);
    if (charset <> '') then begin
        if MimeCharsetToCodePage(charset, Result) then
            Exit;
    end;

 { look for UTF8 characters, beware may only small part of page }
 { warning - MSIE and Firefox don't do this and assume page is ANSI }
    CharsetDetectResult := CharSetDetect(HtmlData, Count);
    if CharsetDetectResult = cdrUtf8 then
        Result := CP_UTF8;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ find codepage for HTML page in stream }
function IcsFindHtmlCodepage(HtmlStream: TStream; var BOMSize: Integer): Longword;     { V8.50 }
var
    HtmlData: TBytes;
    Count: Integer;
begin
    SetLength(HtmlData, 2048 + 2);
    HtmlStream.Position := 0; { V8.67 Seek(0, soFromBeginning); }
    count := HtmlStream.Size;
    if count > 2048 then count := 2048;
    HtmlStream.ReadBuffer(HtmlData[0], count);
    HtmlData[count] := 0;
    HtmlData[count+1] := 0;
    HtmlStream.Position := 0; { V8.67 Seek(0, soFromBeginning); }
    Result := IcsFindHtmlCodepage(HtmlData, Count, BOMSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ find codepage from HTTP content-type header }
{ content-type: application/json;charset=UTF-8;odata.metadata=minimal }
function IcsContentCodepage(ContentType: String): Longword;   { V8.50 }
var
    charset: String;
    I, J: Integer;
begin
    Result := 0;
    ContentType := IcsLowerCase(ContentType);
    I := Pos('charset=', ContentType);
    if I = 0 then
        Exit;
    charset := Copy(ContentType, I + 8, 999);
    J := Pos(';', charset);   { V9.3 strip off anything after charset argument }
    if J > 0 then
        charset := Copy(ContentType, 1, J - 1);
    if (charset <> '') then
         MimeCharsetToCodePage(charset, Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ convert HTML page in buffer to string with correct codepage }
function IcsHtmlToStr(const HtmlData: TBytes; Count: Integer; ACodePage: Longword;
                                                            Entities: Boolean = False): UnicodeString;   { V8.50, V8.64 }
var
    BOMSize: Integer;
begin
    BOMSize := 0;
    Result := '';
    if Count > Length(HtmlData) then Count := Length(HtmlData);  { sanity check }

 { html may have BOM bytes are the front, very easy }
    if Count >= 4 then  { V8.61 don't give up on tiny responses }
        IcsGetBufferCodepage(@HtmlData[0], 4, BOMSize);

    if (ACodePage = CP_UTF16) or (ACodePage = CP_UTF16Be) or (NOT Entities) then
 { convert to unicode, ignoring entities like &pound; and &#9741; }
        IcsMoveTBytesToString(HtmlData, BOMSize, Result, 1, Count - BOMSize, ACodePage)
     else
 { convert to unicode, including entities like &pound; and &#9741; }
        Result := IcsHtmlValuesToUnicode(PAnsiChar(@HtmlData[BOMSize]), ACodePage, False, True);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ convert HTML page in stream to string with correct codepage, pasing codepage }
function IcsHtmlToStr(HtmlStream: TStream; ACodePage: Longword; Entities: Boolean = False): UnicodeString;   { V8.50 }
var
    HtmlData: TBytes;
    Count: Integer;
begin
    Result := '';
    Count := HtmlStream.Size ;
    if Count < 1 then Exit;
    SetLength(HtmlData, Count  + 1);
    HtmlStream.Position := 0; { V8.67 Seek(0, soFromBeginning);   }
    HtmlStream.ReadBuffer (HtmlData[0], count);
    HtmlData[count] := 0;
    HtmlStream.Position := 0; { V8.67 Seek(0, soFromBeginning);   }
    Result := IcsHtmlToStr(HtmlData, Count, ACodePage, Entities);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ convert HTML page in stream to string with correct codepage, passing content header }
function IcsHtmlToStr(HtmlStream: TStream; const ContentHdr: String; Entities: Boolean = False): UnicodeString;   { V8.50 }
var
    HtmlData: TBytes;
    Count: Integer;
    ACodePage: Longword;
    BOMSize: Integer;
begin
    Result := '';
    if (ContentHdr <>'') then begin
        if (Pos ('text/', ContentHdr) <> 1) and
            (Pos ('json', ContentHdr) = 0)  and
               (Pos ('javascript', ContentHdr) = 0) and  { V8.61 }
                  (Pos ('xml', ContentHdr) = 0) then Exit;  { V8.54 json/xml is text }
    end;
    Count := HtmlStream.Size ;
    if Count < 1 then
        Exit;
    SetLength(HtmlData, Count  + 2);
    HtmlStream.Position := 0; { V8.67 Seek(0, soFromBeginning);  }
    HtmlStream.ReadBuffer (HtmlData[0], count);
    HtmlData[count] := 0;
    HtmlData[count+1] := 0;
    HtmlStream.Position := 0; { V8.67 Seek(0, soFromBeginning); }

// first look for charset in HTTP header }
    ACodepage := IcsContentCodepage(ContentHdr);

 // if none, look for charset in file BOM or META headers in HTML
    if ACodepage = 0 then
        ACodepage := IcsFindHtmlCodepage(HtmlData, Count, BOMSize);

  // convert html to unicode string
    Result := IcsHtmlToStr(HtmlData, Count, ACodePage, Entities);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ convert HTML page in stream to string with correct codepage passing Charset  }
function IcsHtmlToStrCh(HtmlStream: TStream; const Charset: String; Entities: Boolean = False): UnicodeString;   { V9.3 }
var
    HtmlData: TBytes;
    Count: Integer;
    ACodePage: Longword;
    BOMSize: Integer;
begin
    Result := '';
    Count := HtmlStream.Size ;
    if Count < 1 then
        Exit;
    SetLength(HtmlData, Count  + 2);
    HtmlStream.Position := 0;
    HtmlStream.ReadBuffer (HtmlData[0], count);
    HtmlData[count] := 0;
    HtmlData[count+1] := 0;
    HtmlStream.Position := 0;

  // try charset from HTTP header }
    ACodepage := 0;
    if (Charset <> '') then
         MimeCharsetToCodePage(charset, ACodePage);

 // if none, look for charset in file BOM or META headers in HTML
    if ACodepage = 0 then
        ACodepage := IcsFindHtmlCodepage(HtmlData, Count, BOMSize);

  // convert html to unicode string
    Result := IcsHtmlToStr(HtmlData, Count, ACodePage, Entities);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.3 parse HTTP request or response header, returns number of arguments and array of those arguments.
{ Sep is optional, for arguments with mixed , ; or space, to force something }
{ ie Content-Type: text/html; charset="iso-8859-1" returns two arguments in HdrsArray }
{ tested using OverbyteIcsMimeDemo }
function IcsPaseHttpHdr(const Hdr: String; var HdrName: String; var HdrArgs: THdrArgs; Sep: String = ''): Integer;
var
    Args, Delim: String;
    J, K: Integer;

begin
    Result := 0 ;
    SetLength(HdrArgs, 0);
    HdrName := '';

 // keep header name, if any, mixed case
    J := Pos(':', Hdr);
    if J > 0 then
        HdrName := Copy(Hdr, 1, J - 1);

// look for multiple argument separator
    Args := Trim(Copy(Hdr, J + 1, 999));
    Args := StringReplace(Args, IcsCRLF, '', [rfReplaceAll]);
    if (Sep = '') then begin ;
        if Pos(',', Args) > 0 then
            Sep := ','
        else if Pos(';', Args) > 0 then
            Sep := ';';
    end;

 // process multiple arguments
    while Length(Args) > 0 do begin
        SetLength(HdrArgs, Result + 1);

     // see if another argument
        J := Pos(Sep, Args);
        if J = 0 then
            J := Length(Args) + 1;

    // keep name (lopwer case) and value (mixed case) separately
        K := Pos('=', Args);
        if (K > 0) and (K < J) then begin
            HdrArgs[Result].HName := Lowercase(Trim(Copy (Args, 1, K - 1)));
            HdrArgs[Result].HValue := Trim(Copy (Args, K + 1, J - K - 1));
        end
        else begin
            HdrArgs[Result].HName := '';
            HdrArgs[Result].HValue := Trim(Copy (Args, 1, J - 1));
        end;

    // see if value is delimited, strip delims
        K := Length(HdrArgs[Result].HValue);
        if (K >= 3) then begin
            Delim := HdrArgs[Result].HValue[1];
            if ((Delim = '''') or (Delim = '"')) and (Delim = HdrArgs[Result].HValue[K]) then
                HdrArgs[Result].HValue := Copy(HdrArgs[Result].HValue, 2, K - 2);
        end;
        Result := Result + 1;

    // strip off argument processed
        if J >= Length(Args) then
            Exit;
        Args := Copy(Args, J + 1, 999);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.3 find an argument in an HTTP request or response header, returns value }
function IcsFindHdrArg(const Hdr, ArgName: String; Sep: String = ''): String;
var
    HdrArgs: THdrArgs;
    HdrName: String;
    I, Tot: Integer;
begin
    Result := '';
    Tot := IcsPaseHttpHdr(Hdr, HdrName, HdrArgs, Sep);
    if Tot > 0 then begin
        for I := 0 to Tot - 1 do begin
            if (Lowercase(ArgName) = HdrArgs[I].HName) then begin
                Result :=  HdrArgs[I].HValue;
                Exit;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.3 is a MIME type texual or printable text }
function IcsMimeIsTextual(const MimeType: String): Boolean;
begin
    Result := (Pos ('text', MimeType) = 1) or (Pos ('json', MimeType) > 0)  or     { V9.4 was text/, but not always }
               (Pos ('javascript', MimeType) > 0) or (Pos ('xml', MimeType) > 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 tests for these encoding and decoding functions in OverbyteIcsMimeDemo1.pas }
{ V9.5 RFC5987 UTF8/percent encode an HTTP header value, optional language, ie en  }
function IcsEncodeHttpParam(const Param: String; const Language: String = ''): String;
var
    UStr: AnsiString;
begin
    UStr := StringToUtf8(Param);
    Result := 'UTF-8' + IcsSQUOTE + Language + IcsSQUOTE + String(IcsPercentEncode(UStr));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 RFC5987 decode an HTTP header percent value, optional language, ie en  }
{ UTF-8''%c2%a3%20and%20%e2%82%ac%20rates }
{ iso-8859-1'en'%A3%20rates }
function IcsDecodeHttpParam(const Param: String): String;
var
    UStr: AnsiString;
    Charset, S: String;
    Delim: Integer;
    ACodePage: LongWord;
begin
    Delim := Pos(IcsSQUOTE, Param);
    if Delim < 5 then begin
        Result := Param;
        Exit;
    end;
    Charset := Copy(Param, 1, Delim - 1);
    if NOT MimeCharsetToCodePage(charset, ACodePage) then
        ACodePage := CP_UTF8;
    S := Copy(Param, Delim + 1, 9999);
    Delim := Pos(IcsSQUOTE, S);
    UStr := IcsPercentDecode(Copy(AnsiString(S), Delim + 1, 9999));
    Result := AnsiToUnicode(Pointer(UStr), Length(UStr), ACodePage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 RFC5987 UTF8 encode an HTTP header name=value, with Charset if non-ASCII }
{ ie title="EURO exchange rates"; title*=utf-8''%e2%82%ac%20exchange%20rates }
function IcsEncHttp2Params(const AName, AValue: String): String;
begin
    Result := AName + '=' + IcsDQUOTE + String(UnicodeToUsAscii(AValue, '_')) + IcsDQUOTE;
    if (NOT IsUsAscii(AValue)) or (Pos(IcsSPACE, AValue) > 0) then
        Result := Result + '; ' + AName + '*=' + IcsEncodeHttpParam(AValue);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 RFC5987 UTF8 decode an HTTP header name=value, with Charset if non-ASCII }
{ ie title="EURO exchange rates"; title*=utf-8''%e2%82%ac%20exchange%20rates }
{ returns best file name, or blank for name not found }
function IcsDecHttp2Params(const AHeader, AName: String): String;
begin
    Result := IcsFindHdrArg(AHeader, AName + '*', ';');   { attachment; filename*=UTF-8''file%20name.jpg   RFC5987 encoded }
    if Result <> '' then  // can not find encoded name, try non-encoded version
        Result := IcsDecodeHttpParam(Result)
    else
        Result := IcsFindHdrArg(AHeader, AName, ';');   { attachment; filename="file name.jpg" }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 add <BR> HTML line ending to lines of text }
function IcsWebReplaceCRLF(const Lines: String): String;       { V9.5 }
begin
    result := StringReplace(Lines, IcsCRLF, '<BR>' + IcsCRLF, [rfReplaceAll]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 check if accessing server by IP address instead of real host name }
function IcsHostDottedIp (HostName: String): Boolean ;   { V9.5 }
var
    I: Integer;
    ASocketFamily: TSocketFamily;
begin

    if (Pos('[', HostName) = 1) then begin //   [2a00:1940:1:2::115:84]:88   // host name with port
         I := Pos(']', HostName);
         HostName := Copy(HostName, 2, I - 2);
    end
    else begin if (Pos('.', HostName) > 1) then begin  // IPv4 only
            I := Pos(':', HostName); // look for port on end
            if I > 1 then
                SetLength(HostName, I - 1);
        end;
    end;
    Result := WSocketIsIP (HostName, ASocketFamily);
end;



initialization

finalization
FreeAndNil(GHtmlEntityHashs);

end.
