{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Aug 08, 2004 (extracted from various ICS components)
Version:      V9.5
Description:  This unit contain support routines for URL handling.
EMail:        francois.piette@overbyte.be         http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1997-2025 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>

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

Overview
--------

TRestParams
-----------
Defines a collection of REST parameters and allows them to be saved as
URL encoded, Json, XML or comma separated values, including arrays.  Many
options for varied REST schemes.  String, numeric, boolean and null types
are supported.


TRestParamsSrv
--------------
Allows a SQL database dataset to be converted into Json for REST servers,
including arrays, also handles Json error responses.



History:
Mar 26, 2006 V6.00 New version 6 started
Sep 28, 2008 V6.01 A. Garrels modified UrlEncode() and UrlDecode() to support
             UTF-8 encoding. Moved IsDigit, IsXDigit, XDigit, htoi2 and htoin
             to OverbyteIcsUtils.
Apr 17, 2009 V6.02 A. Garrels added argument CodePage to functions
             UrlEncode() and UrlDecode.
Dec 19, 2009 V6.03 A. Garrels added UrlEncodeToA().
Aug 07, 2010 V6.04 Bjørnar Nielsen suggested to add an overloaded UrlDecode()
                   that takes a RawByteString URL.
Jan 20, 2012 V6.05 RTT changed ParseUrl() to support URLs starting with "//".
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Mar 10, 2020 V8.64 Added IcsBuildURL, IcsURLtoASCII and IcsURLtoUnicode to
                     support International Domain Names, note these are primarily
                     for display purposes, ICS now handles IDNs internally.
Oct 17, 2020 V8.65 For UrlEncode RFC3986 section 2.1 says four unreserved chars
                      (- . _ -) should not be percent encoded, so added RfcStrict
                      option to ensure this.
                   IcsUrlEncode uses AnsiString and RfcStrict.
                   UrlEncodeEx always uses RfcStrict.
Sep 21, 2021 V8.67 Moved TRestParams here from OverbyteIcsSslHttpRest to
                     ease circular references.
                   RestParams has a new method AddItemNULL to add a null,
                     in Json this will be unquoted.
                   Added TRestParamsSrv component which provides methods for
                     creating REST server Json responses from a SQL database
                     resultset, one or more rows, also error responses. Note
                     this is only compiled if DATABASE is defined in OverbyteIcsDefs.inc
                     to avoid bringing in database units that are not available
                     on all Delphi editions.  There is a REST server sample
                      OverbyteIcsDDWebService.dpr that illustrates SQL lookups.
Aug 08, 2023 V9.0  Updated version to major release 9.
Jan 27, 2024  V9.1 Redesigned TRestParams to build parameters into ParamStream
                     using GetParamStream, to allow parameters including very
                     large files and since the HTTP component needs a post
                     stream rather than a string, mainly for multipart/form-data
                     parameters, see below, GetParams still returns an AnsiString
                     while GetParametersTB returns TBytes.
                   Added new TRestParams content type of PContNone to make them
                     easier to disable, beware ordial values have changed if this
                     saved rather than a literal.
                   Added new TRestParams content type PContFormData to create
                      multipart/form-data parameters, according to RFC7578 which
                      may include multiple binary files and _charset_ part.
                   The TRestParams AddItem method has a new optional ContentType
                     argument, currently used for PContFormData only.
                   Added TParamType of RPTypeFile for binary file content.
                   Added new TRestParams AddItemFile method that takes a full
                     binary file name with optional file size and ContentType,
                     the latter two will be looked up if not supplied, content
                     from file extension and a MIME table.
                   Added new TRestParams FormDataUtf8 property that if true
                     will add a FormData _charset_ part with utf-8 and send
                     all textual content as utf-8 without UrlEncoding.
                   Added GetEstParamSize that returns Int64 estimated size
                      of the parameters, to allow the application to allocate
                      a TFileStream instead of TMemoryStream if massive files
                      are included, typically more than 50MB.
                   Added IcsPercentEncode and IcsPercentDecode to percent encode and
                     decode any non 7-bit characters, ignore charsets. Similar to
                     UrlEncode but does not change spaces or special chars, except %.
                   Moved IcsExtractURLEncodedValue here from OverbyteIcsSslHttpRest.
                   Moved ExtractURLEncodedParamList and GetCookieValue here from
                     OverbyteIcsHttpSrv with Ics, old versions use these.
                   ExtractURLEncodedParamList has new optional Values parameter than adds
                     all values to the strings as name=value.
                   The ResultSet2Json method of TRestParamsSrv has a new optional query
                     parameter that is added to the Json to assist processing.
Jun 04, 2024 V9.2  Builds with D7 again.
                   RestParams MimeBoundary no longer includes extra -- at start that
                     are required preceding boundaries within parts, so multipart/form-data
                     has a shorter boundary.
Jan 17. 2024 V9.4  Removed fmShareCompat warning.
                   TRestParams AddItemSO now has Escape parameter defaulting to True, so
                      non-ASCII characters are escaped by default.
Aug 06, 2025 V9.5  Moved IcsParseExURL, IcsBuildExURL and IcsRelativeName from unit
                     OverbyteIcsHttpMulti.pas, added Ics, used to break down and
                     build URLs, including Section and Query parts.
                   Added new TRestParams RParamFmt property that for Json only defines
                     whether nested objects or an array should be formatted, default is
                     RPFmtNestObj (Nested Objects, same as previously), or  RPFmtArrayVal
                     (Array of Values) if first element is any array, or RPFmtArrayObj
                     (Array of Objects) where each element is treated as object in the
                     array. Note RPFmtArrayObj allows duplicate names in Add methods,
                     since output into different objects.
Sep 23, 2025 V9.6  Fixed typo in PContentLits.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Z.ICS9.OverbyteIcsUrl;

interface

{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$I Include\Z.ICS9.OverbyteIcsDefs.inc}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},      { V8.67 }
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},    { V8.67 }
{$IFDEF DATABASE}
    {$IFDEF RTL_NAMESPACES}Data.DB{$ELSE}DB{$ENDIF},   { V8.67 }
{$ENDIF}
    Z.ICS9.OverbyteIcsSuperObject,    { V8.67 }
    Z.ICS9.OverbyteIcsUtils,
    Z.ICS9.OverbyteIcsStreams,        { V9.1 }
    Z.ICS9.OverbyteIcsMimeUtils,      { V9.1 }
    Z.ICS9.OverbyteIcsTypes; // for TBytes and TThreadID V9.2

const
    IcsUrlVersion        = 905;
    CopyRight : String   = ' TIcsURL (c) 1997-2025 F. Piette V9.5 ';

{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path }
procedure ParseURL(const URL : String; var Proto, User, Pass, Host, Port, Path : String);
function  Posn(const s, t : String; count : Integer) : Integer;

{ V8.64 build a URL without changing any encoding }
function IcsBuildURL(const Proto, User, Pass, Host, Port, Path: string): string ;

{ V9.5 moved from OverbyteIcsHttpMulti.pas, added Ics }
procedure IcsParseExURL (const url: string; var Proto, User, Pass, Host, Port, Dirs, Fname, Section, Query: string) ;
function IcsBuildExURL (const Proto, User, Pass, Host, Port, Dirs, Fname, Section, Query: string): string ;
function IcsRelativeName (Dirs, Rname: string): string ;

{ V8.64 convert the Unicode domain host name in a URL to A-Label (Punycode ASCII) and vice versa }
function IcsURLtoASCII(const Input: string): string ;
function IcsURLtoUnicode(const Input: string): string ;

{ following functions are not for host domain names, but Unicode paths and queries in URLs }
function UrlEncode(const S: String; DstCodePage: LongWord = CP_UTF8; RfcStrict: Boolean = False): String;          { V8.65 }
function UrlDecode(const S: String; SrcCodePage: LongWord = CP_ACP;  DetectUtf8: Boolean = TRUE) : String;
{$IFDEF COMPILER12_UP}
                   overload;
function UrlDecode(const S: RawByteString; SrcCodePage: LongWord = CP_ACP; DetectUtf8: Boolean = TRUE) : UnicodeString; overload;
{$ENDIF}
function IcsUrlEncode(const AStr: AnsiString; RfcStrict: Boolean = False): AnsiString;  { V8.65 }
function UrlEncodeToA(const S: String; DstCodePage: LongWord = CP_UTF8;  RfcStrict: Boolean = False): AnsiString;   { V8.65 }
function UrlEncodeEx(const S: String): String;                                        { V8.65 }
{ V9.1 percent encode any non 7-bit characters }
function IcsPercentEncode(const AStr: AnsiString): AnsiString;                        { V9.1 }
function IcsPercentDecode(const AStr: AnsiString): AnsiString;                        { V9.1 }

function IcsEscapeJson(const AStr: AnsiString): AnsiString;  { V8.66 renamed and made public }

{ V9.1 moved from OverbyteIcsSslHttpRest }
{ Retrieve a single value by name out of an URL encoded data stream.        }
function IcsExtractURLEncodedValue(
    Msg         : PChar;            { URL Encoded stream                    }
    Name        : String;           { Variable name to look for             }
    var Value   : String;           { Where to put variable value           }
    SrcCodePage : LongWord = CP_ACP;{ D2006 and older CP_UTF8 only          }
    DetectUtf8  : Boolean  = TRUE): Boolean; overload;

function IcsExtractURLEncodedValue(
    const Msg   : String;           { URL Encoded stream                     }
    Name        : String;           { Variable name to look for              }
    var Value   : String;           { Where to put variable value            }
    SrcCodePage : LongWord = CP_ACP;{ D2006 and older CP_UTF8 only          }
    DetectUtf8  : Boolean  = TRUE): Boolean; overload;

 { V9.1 moved from OverbyteIcsHttpSrv, addd Ics, originals call these }
function IcsGetCookieValue(
    const CookieString : String;    { Cookie string from header line        }
    const Name         : String;    { Cookie name to look for               }
    var Value          : String)    { Where to put variable value           }
    : Boolean;                      { Found or not found that's the question}

function IcsExtractURLEncodedParamList(
    Msg       : PChar;             { URL Encoded stream                     }
    Params    : TStrings;          { Where to put the list of parameters    }
    Values    : Boolean = False;   { V9.1 Should values be added name=value }
    SrcCodePage : LongWord = CP_ACP;{ D2006 and older CP_UTF8 only          }
    DetectUtf8  : Boolean  = TRUE)
    : Integer; overload;           { Number of parameters found             }

function IcsExtractURLEncodedParamList(
    const Msg : String;            { URL Encoded stream                     }
    Params    : TStrings;          { Where to put the list of parameters    }
    Values    : Boolean = False;   { V9.1 Should values be added name=value }
    SrcCodePage : LongWord = CP_ACP;{ D2006 and older CP_UTF8 only          }
    DetectUtf8  : Boolean  = TRUE)
    : Integer; overload;           { Number of parameters found             }


{ TRestParams }

{ V8.64 added Body versions and XML }
{ V8.66 added CommaList for OAuth1
{ V8.67 moved from OverbyteIcsSslHttpRest to ease circular references }
{ V9.1 added PContNone and PContFormData, beware ordial values changed if this saved rather than a literal }
type
  TPContent = (PContNone, PContUrlencoded, PContJson, PContXML, PContBodyUrlEn, PContBodyJson, PContBodyXML,
                  PContFormData, PContCommaList);

{ V8.67 added Null }
{ V9.1 added File }
  TRParamType = (RPTypeStr, RPTypeInt, RPTypeDate, RPTypeFloat, RPTypeBool, RPTypeObj, RPTypeArray, RPTypeNull, RPTypeFile);
  TRPJsonFmt = (RPJFmtNestObj, RPJFmtArrayVal, RPJFmtArrayObj);                                                 { V9.5 }

const
  RParamTypeLits: array[TRParamType] of string =
        ('RPTypeStr','RPTypeInt','RPTypeDate','RPTypeFloat','RPTypeBool','RPTypeObj','RPTypeArray', 'RPTypeNull', 'RPTypeFile');
  RPJsonFmtLits: array[TRPJsonFmt] of string = ('Nested Objects', 'Array of Values', 'Array of Objects');   { V9.5 }
  PContentLits: array[TPContent] of string = ('None','URL Encoded ? URL', 'Json ? URL', 'XML ? URL',
        'URL Encoded Body','Json Body', 'XML Body', 'Form-Data Body', 'Comma List');                       { V9.5 }
  MaxMemoryStreamSize =  (IcsMBYTE * 500);   { V9.1 use TFileStream for params more than 50MB }

{ TRestParam is one REST parameter }
type
  TRestParam = class(TCollectionItem)
  private
    FPName: String;
    FPValue: String;
    FPRaw: Boolean;               { V8.65 gone }
    FRParamType: TRParamType;     { V8.65 }
    FPValObj: ISuperObject;       { V8.65 }
    FPValArray: TStrings;         { V8.65 }
    FPConType: String;            { V9.1 }
    FPFileSize: Int64;            { V9.1 }
  protected
    function GetDisplayName: string; override;
  published
    constructor Create (Collection: TCollection); Override ;
    destructor Destroy; Override ;                                              { V8.65 }
    property PName: String                read  FPName
                                          write FPName;
    property PValue : String              read  FPValue
                                          write FPValue;
    property PRaw : boolean               read  FPRaw
                                          write FPRaw;       { unused, don't break old code }
    property RParamType: TRParamType      read  FRParamType
                                          write FRParamType;
    property PValObj: ISuperObject        read  FPValObj
                                          write FPValObj;
    property PValArray: TStrings          read  FPValArray
                                          write FPValArray;
    property PConType : String            read  FPConType
                                          write FPConType;      { V9.1 }
    property PFileSize: Int64             read  FPFileSize
                                          write FPFileSize;     { V9.1 }
  end;

{ TRestParams defines a collection of  REST parameters }
  TRestParams = class(TCollection)
  private
    FOwner: TPersistent;
    FPContent: TPContent;
    FSortList: TStringList;    { V8.65 }
    FRfcStrict: Boolean;       { V8.65 RFC3986 strict urlencoding }
    FMimeBoundary: AnsiString;        { V9.1 }
    FMimeTypesList: TMimeTypesList;   { V9.1 }
    FFormDataUtf8: Boolean;           { V9.1 }
    FParamStream: TStream;            { V9.1 }
    FStreamFlag: Boolean;             { V9.1 }
    FRPJsonFmt: TRPJsonFmt;           { V9.5 }
    function GetItem(Index: Integer): TRestParam;
    procedure SetItem(Index: Integer; Value: TRestParam);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent);
    destructor Destroy; Override;
    procedure RebuildSortList;                                                  { V8.65 }
//    function GetParamStr(Sorted: Boolean = False): AnsiString;                  { V9.1 }
    function GetParameters(Sorted: Boolean = False): AnsiString;                { V8.65 added sorted }
    function GetParametersTB(Sorted: Boolean = False): TBytes;                  { V9.1  }
    function GetParamStream(Sorted: Boolean = False): Boolean;                  { V9.1 }
    function GetEstParamSize: Int64;                                            { V9.1 }
    function GetStreamSize: Int64;                                              { V9.1 }
    function IndexOf(const aName: String): Integer;
    procedure AddItem(const aName, aValue: String; aRaw: Boolean = False); overload;
    procedure AddItem(const aName, aValue: String; RParamType: TRParamType; aContype: String = ''); overload;   { V8.65. V9.1 aContype }
    procedure AddItemA(const aName: String; const aValue: AnsiString; aRaw: Boolean = False);      { V8.67 }
    procedure AddItem(const aName: String; aValue: Integer); overload;          { V8.65 }
    procedure AddItem(const aName: String; aValue: Double); overload;           { V8.65 }
    procedure AddItem(const aName: String; aValue: Boolean); overload;          { V8.65 }
    procedure AddItemSO(const aName: String; aValue: ISuperObject; Escape: Boolean = True);  { V8.65, V9.4 added Escape }
    procedure AddItemAR(const aName: String; aValue: TStrings);                 { V8.65 }
    procedure AddItemDT(const aName: String; aValue: TDateTime);                { V8.65 }
    procedure AddItemNULL(const aName: String);                                 { V8.67 }
    procedure AddItemFile(const aName: String; aFileName: String; aFileSize: Int64; aContype: String = ''); { V9.1 }
    procedure RemoveItem(const aName: String);                                  { V8.65 }
    property Items[Index: Integer]: TRestParam      read GetItem
                                                    write SetItem; default;
    property MimeBoundary: AnsiString               read  FMimeBoundary;       { V9.1 }
  published
    property PContent: TPContent                    read  FPContent
                                                    write FPContent;
    property RfcStrict: Boolean                     read  FRfcStrict            { V8.65 }
                                                    write FRfcStrict;
    property FormDataUtf8: Boolean                  read  FFormDataUtf8         { V9.1 }
                                                    write FFormDataUtf8;
    property ParamStream: TStream                   read  FParamStream          { V9.1 }
                                                    write FParamStream;
    property RPJsonFmt: TRPJsonFmt                  read  FRPJsonFmt
                                                    write FRPJsonFmt;           { V9.5 }
  end;

{ V8.67 TRestParamsSrv extends TRestParams for database REST servers }
{$IFDEF DATABASE}
type
  TRestErr = (RestErrUnknown,       // 0
              RestErrInternal,      // 1
              RestErrUnsupported,   // 2
              RestErrConvert,       // 3
              RestErrNoRecords,     // 4
              RestErrNoFields,      // 5
              RestErrGeneral,       // 6
              RestErrQuota,         // 7
              RestErrDatabase,      // 8
              RestErrAuthz);        // 9

const
  RestErrLits: array[TRestErr] of string = (
        'Unknown Error',
        'Internal Error',
        'Unsupported Request',
        'Failed to convert SQL resultset to JSON',
        'No Records Found',
        'No Fields Found',
        'General Error',   { normally use custom text }
        'Quota Exceeded',
        'Database is currently unavailable',
        'Authorisation Failed' );

type
  TRestParamsSrv = class(TObject)
  private
    FJsonRecord: TRestParams;
    FJsonResult: TRestParams;
    FJsonArray: TIcsStringBuild;
  public
    constructor  Create;
    destructor   Destroy; override;
    function     JsonErr(RestErr: TRestErr; const ErrDesc: String = ''; const Query: String = ''): AnsiString;   { V9.1 added Query }
    function     ResultSet2Json(DataSet: TDataSet; var JsonStr: AnsiString; MaxRecs: Integer = 0; const Query: String = ''): Boolean; { V9.1 added Query }
  end;
{$ENDIF}


implementation

type
    TCharSet = set of AnsiChar;
const
    UriProtocolSchemeAllowedChars : TCharSet = ['a'..'z','0'..'9','+','-','.'];


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Find the count'th occurence of the s string in the t string.              }
{ If count < 0 then look from the back                                      }
function Posn(const s , t : String; Count : Integer) : Integer;
var
    i, h, Last : Integer;
    u          : String;
begin
    u := t;
    if Count > 0 then begin
        Result := Length(t);
        for i := 1 to Count do begin
            h := Pos(s, u);
            if h > 0 then
                u := Copy(u, h + 1, Length(u))
            else begin
                u := '';
                Inc(Result);
            end;
        end;
        Result := Result - Length(u);
    end
    else if Count < 0 then begin
        Last := 0;
        for i := Length(t) downto 1 do begin
            u := Copy(t, i, Length(t));
            h := Pos(s, u);
            if (h <> 0) and ((h + i) <> Last) then begin
                Last := h + i - 1;
                Inc(count);
                if Count = 0 then
                    break;
            end;
        end;
        if Count = 0 then
            Result := Last
        else
            Result := 0;
    end
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path         }
procedure ParseURL(const url: String; var Proto, User, Pass, Host, Port, Path: String);
var
    p, q, i : Integer;
    s       : String;
    CurPath : String;
begin
    CurPath := Path;
    proto   := '';
    User    := '';
    Pass    := '';
    Host    := '';
    Port    := '';
    Path    := '';

    if Length(url) < 1 then
        Exit;

    { Handle path beginning with "./" or "../".          }
    { This code handle only simple cases !               }
    { Handle path relative to current document directory }
    if (Copy(url, 1, 2) = './') then begin
        p := Posn('/', CurPath, -1);
        if p > Length(CurPath) then
            p := 0;
        if p = 0 then
            CurPath := '/'
        else
            CurPath := Copy(CurPath, 1, p);
        Path := CurPath + Copy(url, 3, Length(url));
        Exit;
    end
    { Handle path relative to current document parent directory }
    else if (Copy(url, 1, 3) = '../') then begin
        p := Posn('/', CurPath, -1);
        if p > Length(CurPath) then
            p := 0;
        if p = 0 then
            CurPath := '/'
        else
            CurPath := Copy(CurPath, 1, p);

        s := Copy(url, 4, Length(url));
        { We could have several levels }
        while TRUE do begin
            CurPath := Copy(CurPath, 1, p-1);
            p := Posn('/', CurPath, -1);
            if p > Length(CurPath) then
                p := 0;
            if p = 0 then
                CurPath := '/'
            else
                CurPath := Copy(CurPath, 1, p);
            if (Copy(s, 1, 3) <> '../') then
                break;
            s := Copy(s, 4, Length(s));
        end;

        Path := CurPath + Copy(s, 1, Length(s));
        Exit;
    end;

    p := pos('://', url);
    q := p;
    if p <> 0 then begin
        S := IcsLowerCase(Copy(url, 1, p - 1));
        for i := 1 to Length(S) do begin
            if not (AnsiChar(S[i]) in UriProtocolSchemeAllowedChars) then begin
                q := i;
                Break;
            end;
        end;
        if q < p then begin
            p     := 0;
            proto := 'http';
        end;
    end;
    if p = 0 then begin
        if (url[1] = '/') then begin
            { Relative path without protocol specified }
            proto := 'http';
            //p     := 1;     { V6.05 }
            if (Length(url) > 1) then begin
                if (url[2] <> '/') then begin
                    { Relative path }
                    Path := Copy(url, 1, Length(url));
                    Exit;
                end
                else
                    p := 2;   { V6.05 }
            end
            else begin        { V6.05 }
                Path := '/';  { V6.05 }
                Exit;         { V6.05 }
            end;
        end
        else if IcsLowerCase(Copy(url, 1, 5)) = 'http:' then begin
            proto := 'http';
            p     := 6;
            if (Length(url) > 6) and (url[7] <> '/') then begin
                { Relative path }
                Path := Copy(url, 6, Length(url));
                Exit;
            end;
        end
        else if IcsLowerCase(Copy(url, 1, 7)) = 'mailto:' then begin
            proto := 'mailto';
            p := pos(':', url);
        end;
    end
    else begin
        proto := IcsLowerCase(Copy(url, 1, p - 1));
        inc(p, 2);
    end;
    s := Copy(url, p + 1, Length(url));

    p := pos('/', s);
    q := pos('?', s);
    if (q > 0) and ((q < p) or (p = 0)) then
        p := q;
    if p = 0 then
        p := Length(s) + 1;
    Path := Copy(s, p, Length(s));
    s    := Copy(s, 1, p-1);

    { IPv6 URL notation, for instance "[2001:db8::3]" }
    p := Pos('[', s);
    q := Pos(']', s);
    if (p = 1) and (q > 1) then
    begin
        Host := Copy(s, 2, q - 2);
        s := Copy(s, q + 1, Length(s));
    end;

    p := Posn(':', s, -1);
    if p > Length(s) then
        p := 0;
    q := Posn('@', s, -1);
    if q > Length(s) then
        q := 0;
    if (p = 0) and (q = 0) then begin   { no user, password or port }
        if Host = '' then
            Host := s;
        Exit;
    end
    else if q < p then begin  { a port given }
        Port := Copy(s, p + 1, Length(s));
        if Host = '' then
            Host := Copy(s, q + 1, p - q - 1);
        if q = 0 then
            Exit; { no user, password }
        s := Copy(s, 1, q - 1);
    end
    else begin
        if Host = '' then
            Host := Copy(s, q + 1, Length(s));
        s := Copy(s, 1, q - 1);
    end;
    p := pos(':', s);
    if p = 0 then
        User := s
    else begin
        User := Copy(s, 1, p - 1);
        Pass := Copy(s, p + 1, Length(s));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 build a URL without changing any encoding }
function IcsBuildURL (const Proto, User, Pass, Host, Port, Path: string): string ;
begin
    Result := Proto + '://' ;
    if User <> '' then begin
        Result := Result + User ;
        if Pass <> '' then
            Result := Result + ':' + Pass ;
        Result := Result + '@' ;
    end ;
    Result := Result + Host ;
    if Port <> '' then
        Result := Result + ':' + Port ;
    if Path <> '' then begin
        if Path[1] <> '/' then
            Result := Result + '/' ;
        Result := Result + Path ;
    end;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 convert the Unicode domain host name in a URL to A-Label (Punycode ASCII) }
function IcsURLtoASCII (const Input: string): string ;
var
    Proto, User, Pass, Host, Port, Path: string;
begin
    ParseURL(Input, Proto, User, Pass, Host, Port, Path);
    Result := IcsBuildURL(Proto, User, Pass, IcsIDNAToASCII(Host), Port, Path);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 convert the A-Label (Punycode ASCII) domain host name in a URL to Unicode }
{ not does not change path }
function IcsURLtoUnicode (const Input: string): string ;
var
    Proto, User, Pass, Host, Port, Path: string;
begin
    ParseURL(Input, Proto, User, Pass, Host, Port, Path);
    Result := IcsBuildURL(Proto, User, Pass, IcsIDNAToUnicode(Host), Port, Path);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.65 RFC3986 section 2.1 says four unreserved chars (- . _ -) should not
  be percent encoded, so added RfcStrict option to ensure this, AnsiStrings }
function IcsUrlEncode(const AStr: AnsiString; RfcStrict: Boolean = False): AnsiString;
var
    I, J   : Integer;
    RStr   : AnsiString;
    HexStr : String[2];
    ACh    : AnsiChar;
    ALen   : Integer;  { V9.5 }
begin
    Result := '';     { V9.5 }
    ALen := Length(AStr);
    if ALen = 0 then
        Exit;
    SetLength(RStr, ALen * 3);
    J := 0;
    for I := 1 to ALen do begin
        ACh := AStr[I];
        if ((ACh >= '0') and (ACh <= '9')) or ((ACh >= 'a') and (ACh <= 'z')) or ((ACh >= 'A') and (ACh <= 'Z')) then begin
            Inc(J);
            RStr[J] := ACh;
        end
        else if RfcStrict and ((ACh = '.') or (ACh = '-') or (ACh = '_')  or (ACh = '~')) then begin
            Inc(J);
            RStr[J] := ACh;
        end
        else begin
            Inc(J);
            RStr[J] := '%';
            HexStr  := IcsIntToHexA(Ord(ACh), 2);
            Inc(J);
            RStr[J] := HexStr[1];
            Inc(J);
            RStr[J] := HexStr[2];
        end;
    end;
    SetLength(RStr, J);
    Result := RStr;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.65 RFC3986 section 2.1 says four unreserved chars (- . _ -) should not
  be percent encoded, so added RfcStrict option to ensure this, Strings }
function UrlEncodeToA(const S: String; DstCodePage: LongWord = CP_UTF8; RfcStrict: Boolean = False): AnsiString;
var
    AStr   : AnsiString;
begin
{$IFDEF COMPILER12_UP}
    AStr := UnicodeToAnsi(S, DstCodePage);
{$ELSE}
    if DstCodePage = CP_UTF8 then
        AStr := StringToUtf8(S)
    else
        AStr := S;
{$ENDIF}
    Result := IcsUrlEncode(AStr, RfcStrict);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UrlEncode(const S: String; DstCodePage: LongWord = CP_UTF8; RfcStrict: Boolean = False): String; { V8.65 added RfcStrict }
begin
    Result := String(UrlEncodeToA(S, DstCodePage, RfcStrict));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UrlEncodeEx(const S: String): String;                                   { V8.65 }
begin
    Result := String(UrlEncodeToA(S, CP_UTF8, True));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.1 percent encode any non 7-bit characters, per RFC3986  }
{ similar to UrlEncode but does not change spaces or special characters, except % }
function IcsPercentEncode(const AStr: AnsiString): AnsiString;
var
    I, J   : Integer;
    RStr   : AnsiString;
    HexStr : String[2];
    ACh    : AnsiChar;
    ALen   : Integer;  { V9.5 }
begin
    Result := '';     { V9.5 }
    ALen := Length(AStr);
    if ALen = 0 then
        Exit;
    SetLength(RStr, ALen * 3);
    J := 0;
    for I := 1 to ALen do begin
        ACh := AStr[I];
        if (ACh <= #127) and (Ach <> '%') then begin
            Inc(J);
            RStr[J] := ACh;
        end
        else begin
            Inc(J);
            RStr[J] := '%';
            HexStr  := IcsIntToHexA(Ord(ACh), 2);
            Inc(J);
            RStr[J] := HexStr[1];
            Inc(J);
            RStr[J] := HexStr[2];
        end;
    end;
    SetLength(RStr, J);
    Result := RStr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.1 percent decode, ignores charsets, per RFC3986 }
function IcsPercentDecode(const AStr: AnsiString): AnsiString;
var
    I, J, L : Integer;
    ACh      : AnsiChar;
begin
    L := Length(AStr);
    SetLength(Result, L);
    I := 1;
    J := 0;
    while (I <= L) do begin
        ACh := AnsiChar(AStr[I]);
        if ACh = '%' then begin
            ACh := AnsiChar(htoi2(PAnsiChar(@AStr[I + 1])));
            Inc(I, 2);
        end;
        Inc(J);
        Result[J] := ACh;
        Inc(I);
    end;
    SetLength(Result, J);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UrlDecode(const S: String; SrcCodePage: LongWord = CP_ACP; DetectUtf8: Boolean = TRUE) : String;
var
    I, J, L : Integer;
    U8Str   : AnsiString;
    Ch      : AnsiChar;
begin
    L := Length(S);
    SetLength(U8Str, L);
    I := 1;
    J := 0;
    while (I <= L) and (S[I] <> '&') do begin
        Ch := AnsiChar(S[I]);
        if Ch = '%' then begin
            Ch := AnsiChar(htoi2(PChar(@S[I + 1])));
            Inc(I, 2);
        end
        else if Ch = '+' then
            Ch := ' ';
        Inc(J);
        U8Str[J] := Ch;
        Inc(I);
    end;
    SetLength(U8Str, J);
    if (SrcCodePage = CP_UTF8) or (DetectUtf8 and IsUtf8Valid(U8Str)) then
{$IFDEF COMPILER12_UP}
        Result := Utf8ToStringW(U8Str)
    else
        Result := AnsiToUnicode(U8Str, SrcCodePage);
{$ELSE}
        Result := Utf8ToStringA(U8Str)
    else
        Result := U8Str;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function UrlDecode(const S: RawByteString; SrcCodePage: LongWord = CP_ACP; DetectUtf8: Boolean = TRUE): UnicodeString;
var
    I, J, L : Integer;
    U8Str   : AnsiString;
    Ch      : AnsiChar;
begin
    L := Length(S);
    SetLength(U8Str, L);
    I := 1;
    J := 0;
    while (I <= L) and (S[I] <> '&') do begin
        Ch := AnsiChar(S[I]);
        if Ch = '%' then begin
            Ch := AnsiChar(htoi2(PAnsiChar(@S[I + 1])));
            Inc(I, 2);
        end
        else if Ch = '+' then
            Ch := ' ';
        Inc(J);
        U8Str[J] := Ch;
        Inc(I);
    end;
    SetLength(U8Str, J);
    if (SrcCodePage = CP_UTF8) or (DetectUtf8 and IsUtf8Valid(U8Str)) then
        Result := Utf8ToStringW(U8Str)
    else
        Result := AnsiToUnicode(U8Str, SrcCodePage);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TRestParam }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TRestParam.Create(Collection: TCollection);
begin
    inherited;
    FPRaw := False;
    FRParamType := RPTypeStr;  { V8.65 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TRestParam.Destroy;                                                  { V8.65 }
begin
    FreeAndNil(FPValArray);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRestParam.GetDisplayName: string;
begin
    if FPName <> '' then
        Result := FPName + '=' + FPValue
    else
        Result := Inherited GetDisplayName
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TRestParams }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TRestParams.Create(Owner: TPersistent);
begin
    FOwner := Owner;
    inherited Create(TRestParam);
    FPContent := PContUrlencoded;
    FFormDataUtf8 := True;
    FRfcStrict := false;  { V8.65 true means strict RFC URL encoding }
    FSortList := TStringList.Create;
    FSortList.Sorted := True;
    FSortList.CaseSensitive := True;
    FParamStream:= Nil;       { V9.1 }
    FRPJsonFmt := RPJFmtNestObj;           { V9.5 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TRestParams.Destroy;
begin
    FreeAndNil(FSortList);
    FreeAndNil(FMimeTypesList);
    if FStreamFlag then
        FreeAndNil(FParamStream);      { V9.1 }
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRestParams.GetItem(Index: Integer): TRestParam;
begin
  Result := TRestParam(inherited GetItem(Index));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.SetItem(Index: Integer; Value: TRestParam);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRestParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRestParams.IndexOf(const aName: string): Integer;
var
    I: Integer;
begin
    Result := -1;
    if Count = 0 then
        Exit;
    for I := 0 to Count - 1 do begin
        if Items[I].PName = aName then begin
            Result := I;
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItem(const aName, aValue: string; aRaw: Boolean = False);
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if (Index < 0) or (FRPJsonFmt = RPJFmtArrayObj) then begin   { V9.5 array of objects allows duplicates }
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    Items[Index].PValue := aValue;
    Items[Index].PRaw := aRaw;
    if aRaw then
        Items[Index].RParamType := RPTypeObj  { V8.65 }
    else
        Items[Index].RParamType := RPTypeStr;  { V8.65 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItem(const aName, aValue: String; RParamType: TRParamType; aContype: String = '');   { V8.65, V9.1 aContype }
var
    Index: Integer;
    MyInt: Integer;
    MyFloat: Double;
    MyStr: String;
begin
    Index := IndexOf(aName);
    if (Index < 0) or (FRPJsonFmt = RPJFmtArrayObj) then begin   { V9.5 array of objects allows duplicates }
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    Items[Index].PValue := aValue;
    Items[Index].FPConType := aContype;   { V9.1 optional for Form-Data }

{ check non-string types are acceptable to Json, otherwise make them strings }
    if RParamType = RPTypeInt then begin
        if NOT TryStrToInt(aValue, MyInt) then
            RParamType := RPTypeStr;
    end;
    if RParamType = RPTypeFloat then begin
        if NOT TryStrToFloat(aValue, MyFloat) then
            RParamType := RPTypeStr;
    end;
  { boolean made lower case, also accepts Y/N }
    if RParamType = RPTypeBool then begin
        MyStr := IcsLowercase(aValue);
        if Pos('y', MyStr) = 1 then
            MyStr := 'true';
        if Pos('n', MyStr) = 1 then
            MyStr := 'false';
        if (MyStr <> 'true') and (MyStr <> 'false') then
            RParamType := RPTypeStr
        else
            Items[Index].PValue := MyStr;
    end;
    if RParamType = RPTypeObj then begin
        MyInt := Length(aValue);
        if (MyInt < 2) then
            RParamType := RPTypeStr
        else begin
            if NOT (((aValue[1]='{') and (aValue[MyInt]='}')) or ((aValue[1]='[') and (aValue[MyInt]=']'))) then
                RParamType := RPTypeStr;
        end;
    end;
    if RParamType = RPTypeArray then begin
        if NOT Assigned(Items[Index].PValArray) then
            Items[Index].PValArray := TStringList.Create;
        Items[Index].PValArray.CommaText := aValue;
    end;
    Items[Index].RParamType := RParamType;
    Items[Index].PRaw := (RParamType in [RPTypeInt, RPTypeFloat, RPTypeBool, RPTypeObj]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItemA(const aName: String; const aValue: AnsiString; aRaw: Boolean = False);      { V8.67 }
begin
    AddItem(aName, String(aValue), aRaw);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItem(const aName: String; aValue: Integer);        { V8.65 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if (Index < 0) or (FRPJsonFmt = RPJFmtArrayObj) then begin   { V9.5 array of objects allows duplicates }
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    Items[Index].PValue := IntToStr(aValue);
    Items[Index].RParamType := RPTypeInt;
    Items[Index].PRaw := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItem(const aName: String; aValue: Double);           { V8.65 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if (Index < 0) or (FRPJsonFmt = RPJFmtArrayObj) then begin   { V9.5 array of objects allows duplicates }
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    Items[Index].PValue := FloatToStr(aValue);
    Items[Index].RParamType := RPTypeFloat;
    Items[Index].PRaw := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItem(const aName: String; aValue: Boolean);         { V8.65 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if (Index < 0) or (FRPJsonFmt = RPJFmtArrayObj) then begin   { V9.5 array of objects allows duplicates }
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    if aValue then
        Items[Index].PValue := 'true'
    else
        Items[Index].PValue := 'false';
    Items[Index].RParamType := RPTypeBool;
    Items[Index].PRaw := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItemSO(const aName: String; aValue: ISuperObject; Escape: Boolean = True);  { V8.65, V9.4 added Escape }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if (Index < 0) or (FRPJsonFmt = RPJFmtArrayObj) then begin   { V9.5 array of objects allows duplicates }
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    Items[Index].PValObj := aValue;
    if Assigned(aValue) then
        Items[Index].PValue := aValue.AsJson(false, Escape)  { no indent, escape optional }
    else
        Items[Index].PValue := '';
    Items[Index].RParamType := RPTypeObj;
    Items[Index].PRaw := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItemAR(const aName: String; aValue: TStrings);     { V8.65 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if (Index < 0) or (FRPJsonFmt = RPJFmtArrayObj) then begin   { V9.5 array of objects allows duplicates }
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    if NOT Assigned(Items[Index].PValArray) then
        Items[Index].PValArray := TStringList.Create;
    Items[Index].PValArray.Assign(aValue);
    Items[Index].PValue := aValue.CommaText;
    Items[Index].RParamType := RPTypeArray;
    Items[Index].PRaw := False;  // assume strings for now
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItemDT(const aName: String; aValue: TDateTime);    { V8.65 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if (Index < 0) or (FRPJsonFmt = RPJFmtArrayObj) then begin   { V9.5 array of objects allows duplicates }
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    Items[Index].PValue := RFC3339_DateToStr(aValue);
    Items[Index].RParamType := RPTypeDate;
    Items[Index].PRaw := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItemNULL(const aName: String);                                 { V8.67 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if (Index < 0) or (FRPJsonFmt = RPJFmtArrayObj) then begin   { V9.5 array of objects allows duplicates }
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    Items[Index].PValue := 'null';
    Items[Index].RParamType := RPTypeNull;
    Items[Index].PRaw := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.AddItemFile(const aName: String; aFileName: String; aFileSize: Int64; aContype: String = ''); { V9.1 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if (Index < 0) or (FRPJsonFmt = RPJFmtArrayObj) then begin   { V9.5 array of objects allows duplicates }
        Index := Count;
        Add;
    end;
    Items[Index].PName := aName;
    Items[Index].PValue := aFileName;
    if aFileSize > 0 then
        Items[Index].FPFileSize := aFileSize
    else
        Items[Index].FPFileSize := IcsGetFileSize(Trim(aFileName));
    if aContype = '' then begin
        if NOT Assigned(FMimeTypesList) then
            FMimeTypesList := TMimeTypesList.Create(Nil);
        Items[Index].FPConType := FMimeTypesList.TypeFromFile(Trim(aFileName));
    end
    else
        Items[Index].FPConType := aContype;
    Items[Index].RParamType := RPTypeFile;
    Items[Index].PRaw := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.RemoveItem(const aName: string);                      { V8.65 }
var
    Index: Integer;
begin
    Index := IndexOf(aName);
    if Index >= 0 then
        Delete(Index);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRestParams.RebuildSortList;                       { V8.65 build sorted list }
var
    Index: Integer;
begin
    FSortList.Clear;
    if Count = 0 then Exit;
    for Index := 0 to Count - 1 do
        FSortList.AddObject(Items[Index].PName, TObject(Index));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsEscapeJson(const AStr: AnsiString): AnsiString;  { V8.66 renamed and made public }
var
    I, outoff, inlen: integer;
    Ch: PAnsiChar;

    procedure AddEsc(NewCh: AnsiChar);
    begin
        Result[outoff] := '\';
        Inc(outoff);
        Result[outoff] := NewCh;
    end;

begin
    Result := '';
    outoff := 1;
    inlen := Length(AStr);
    if inlen = 0 then
        Exit;
    SetLength(Result, inlen * 2);
    Ch := Pointer(AStr);
    for I := 1 to inlen do begin
        if Ch^ = '\'  then
            AddEsc('\')
        else if Ch^ = '/' then
            AddEsc('/')
        else if Ch^ = '"' then
            AddEsc('"')
        else if Ch^ = IcsCR then
            AddEsc('r')
        else if Ch^ = IcsLF then
            AddEsc('n')
        else if Ch^ = IcsBACKSPACE  then
            AddEsc('b')
        else if Ch^ = IcsTab  then
            AddEsc('t')
        else
            Result[outoff] := Ch^;
        Inc(Ch);
        Inc(outoff);
    end;
    SetLength(Result, outoff - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function EscapeXML(const AStr: AnsiString): AnsiString;  { V8.64 }
var
    I, outoff, inlen: integer;
    Ch: PAnsiChar;

    procedure AddEntity(NewStr: AnsiString);
    var
        J: Integer;
    begin
        Result[outoff] := '&';
        Inc(outoff);
        for J := 1 to Length(NewStr) do begin
            Result[outoff] := NewStr[J];
            Inc(outoff);
        end;
        Result[outoff] := ';';
    end;

begin
    Result := '';
    outoff := 1;
    inlen := Length(AStr);
    if inlen = 0 then
        Exit;
    SetLength(Result, inlen * 2);
    Ch := Pointer(AStr);
    for I := 1 to inlen do begin
        if Ch^ = '&'  then
            AddEntity('amp')
        else if Ch^ = '''' then
            AddEntity('apos')
        else if Ch^ = '"' then
            AddEntity('quot')
        else if Ch^ = '<' then
            AddEntity('lt')
        else if Ch^ = '>' then
            AddEntity('gt')
        else
            Result[outoff] := Ch^;
        Inc(Ch);
        Inc(outoff);
    end;
    SetLength(Result, outoff - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.1 get estimated parameter size, to decide whether larger than memory so TFileStream is needed }
function TRestParams.GetEstParamSize: Int64;
var
    I: Integer;
begin
    Result := 0;
    if Count > 0 then begin
        for I := 0 to Count - 1 do begin
            Result := Result + Length(Items[I].PName) + 5;
            if (FPContent = PContFormData) then begin
                Result := Result + 100;  // boundary, headers
                if (Items[I].RParamType = RPTypeFile) then begin
                    if Items[I].PFileSize < 0 then
                        Items[I].PFileSize := IcsGetFileSize(Trim(Items[I].PValue));
                    Result := Result + Items[I].PFileSize;
                end
                else
                    Result := Result + Length(Items[I].PValue);
            end
            else
                Result := Result + Length(Items[I].PValue) + 5;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.1 build stream from parameters }
{ FParamStream must be created first and cleared, either TMemoryStream or TFileStream
  for very large parameters like gigabyte files }
function TRestParams.GetParamStream(Sorted: Boolean = False): Boolean;
var
    I, J, K, ArrayLen, Len: integer;
    UploadFile, PVS: String;
    PN, PV, PFN, PCT: AnsiString;
    JFlag: Boolean; { V8.62 }
    FileStream: TIcsBufferedFileStream;
    FLen, SLen: Int64;
    XBoundary: AnsiString;

    procedure AddToStream(const S: AnsiString);
    begin
        if S <> '' then
            ParamStream.Write(S[1], Length(S));
    end;

// V9.5 add Json object to stream
    procedure AddOneObject(ItemNr: Integer);
    var
        K: Integer;
    begin
        PN := StringToUtf8(Trim(Items[ItemNr].PName));
        if PN <> '' then begin
            ArrayLen := 0;
            JFlag := False;

         { V8.65 see if building Json array from StringList }
            if (Items[ItemNr].RParamType = RPTypeArray) and Assigned(Items[ItemNr].PValArray) then
                ArrayLen := Items[ItemNr].PValArray.Count;
            if ArrayLen > 0 then begin
                PV := '[';
                for K := 0 to ArrayLen - 1 do begin
                    if K >= 1 then
                        PV := PV + ',';
                    PV := PV + '"' + IcsEscapeJson(StringToUtf8(Trim(Items[ItemNr].PValArray[K]))) + '"';
                end;
                PV := PV + ']';
                JFlag := True;
            end
         { simple single value }
            else begin
                PV := StringToUtf8(Trim(Items[ItemNr].PValue));
                if NOT Items[ItemNr].PRaw then
                    PV := IcsEscapeJson(PV);
            end;
            Len := Length(PV);

          { V8.62 check if adding Json, don't quote it }
            if Len >= 2 then
                JFlag := ((PV[1]='{') and (PV[Len]='}')) or ((PV[1]='[') and (PV[Len]=']'));

          { V8.65 data types that don't need quotes }
            if Items[ItemNr].RParamType in [RPTypeInt, RPTypeFloat, RPTypeBool, RPTypeObj, RPTypeNull] then  { V8.67 added Null }
                JFlag := True;
            AddToStream('"' + PN + '":');
            if NOT JFlag then
                AddToStream('"');
            AddToStream(PV);
            if NOT JFlag then
                AddToStream('"');
        end;
    end;


begin
    Result := True;
    if NOT Assigned(FParamStream) then
        raise Exception.Create('Must set ParamStream before getting parameters');

    FParamStream.Position := 0;
    if FPContent = PContNone then     { V9.1 content disabled }
        Exit;
    if Sorted then
        RebuildSortList; { V8.65 do we need to build sort list }

 { build URL Encoded parameters, generally added after URL or as POST body }
    if FPContent in [PContUrlencoded, PContBodyUrlen] then begin  { V8.64 added Body version }
        if Count > 0 then begin
            for J := 0 to Count - 1 do begin
                if Sorted then
                    I := Integer(FSortList.Objects[J])
                else
                    I := J;
                PN := StringToUtf8(Trim(Items[I].PName));
                if PN <> '' then begin
                    K := 0;
                    ArrayLen := 0;

                { V8.65 array is added as multiple identical name=value pairs }
                { beware this may not always be supported, but works for Google APIs }
                    if (Items[I].RParamType = RPTypeArray) and Assigned(Items[I].PValArray) then
                        ArrayLen := Items[I].FPValArray.Count;
                    while true do begin
                        if ArrayLen > 0 then
                            PV := StringToUtf8(Trim(Items[I].PValArray[K]))
                        else
                            PV := StringToUtf8(Trim(Items[I].PValue));
                        if FParamStream.Size > 0 then
                            AddToStream('&');
                        AddToStream(PN + '=');
                        if Items[I].PRaw then
                            AddToStream(PV)
                        else
                            AddToStream(IcsUrlEncode(PV, FRfcStrict));  { V8.65 added strict }
                        inc(K);
                        if K >= ArrayLen then
                            break;
                    end;
                end;
            end;
        end;
    end

  { build block of Json from parameters }
    else if FPContent in [PContJson, PContBodyJson] then begin  { V8.64 added Body version }

      { V9.5 single Json objeect, that may have nested objects  }
        if FRPJsonFmt = RPJFmtNestObj then begin;
            AddToStream('{');
            if Count > 0 then begin
                for J := 0 to Count - 1 do begin
                    if Sorted then
                        I := Integer(FSortList.Objects[J])
                    else
                        I := J;
                    if FParamStream.Size > 1 then
                       AddToStream(',');
                    AddOneObject(I);   { V9.5 commonise code }
                end;
            end;
            AddToStream('}');
        end

     { V9.5 simple array using array value from first parameter only, ignored if more than one item or not RPTypeArray }
        else if FRPJsonFmt = RPJFmtArrayVal then begin;
            AddToStream('[');
            if Count = 1 then begin
                if (Items[0].RParamType = RPTypeArray) and Assigned(Items[0].PValArray) then
                    ArrayLen := Items[0].PValArray.Count;
                if ArrayLen > 0 then begin
                    for K := 0 to ArrayLen - 1 do begin
                        if K >= 1 then
                            AddToStream(',');
                        AddToStream(IcsEscapeJson(StringToUtf8(Trim(Items[0].PValArray[K]))));
                    end;
                end;
            end;
            AddToStream(']');
        end

      { V9.5 each item in parameter is output as an object within a array }
        else if FRPJsonFmt = RPJFmtArrayObj then begin;
            AddToStream('[');
            if Count > 0 then begin
                for J := 0 to Count - 1 do begin
                    if Sorted then
                        I := Integer(FSortList.Objects[J])
                    else
                        I := J;
                    if FParamStream.Size > 1 then
                       AddToStream(',');
                    AddToStream('{');
                    AddOneObject(I);
                    AddToStream('}');
                end;
            end;
            AddToStream(']');
        end;

    end

  { build block of XML }
    else if FPContent in [PContXml, PContBodyXml] then begin  { V8.64 new }
        AddToStream('<?xml version="1.0" encoding="UTF-8"><ICS>');
        if Count > 0 then begin
            for J := 0 to Count - 1 do begin
            { V8.65 XML does not supports arrays so use string instead }
                if Sorted then
                    I := Integer(FSortList.Objects[J])
                else
                    I := J;
                PN := StringToUtf8(Trim(Items[I].PName));
                if PN <> '' then begin
                    PV := StringToUtf8(Trim(Items[I].PValue));
                    AddToStream('<' + EscapeXML(PN) + '>');
                    if Items[I].PRaw then
                        AddToStream(PV)
                    else
                        AddToStream(EscapeXML(PV));
                    AddToStream('</' + EscapeXML(PN) + '>');
                end;
            end;
        end;
        AddToStream('</ICS>');
    end

  { V8.65 comma separate quoted values for OAuth1 Authhorize: header }
    else if FPContent = PContCommaList then begin
        if Count > 0 then begin
            for J := 0 to Count - 1 do begin
            { V8.65 OAuth1 does not need arrays so use string instead }
                if Sorted then
                    I := Integer(FSortList.Objects[J])
                else
                    I := J;
                PN := StringToUtf8(Trim(Items[I].PName));
                if PN <> '' then begin
                    PV := StringToUtf8(Trim(Items[I].PValue));
                    if FParamStream.Size > 0 then
                        AddToStream(', ');
                    AddToStream(PN + '="');
                    if Items[I].PRaw then
                        AddToStream(PV)
                    else
                        AddToStream(IcsUrlEncode(PV, FRfcStrict));
                    AddToStream('"');
                end;
            end;
        end;
    end

  { V9.1 create MIME multipart/form-data parameters  }
  { note all strings are converted to UTF-8 and sent as 8-bit if FormDataUtf8=True, otherwise percent encoded to 7-bit }
  { names should generally be 7-bit only }
    else if (FPContent = PContFormData) then begin
        if FMimeBoundary = '' then     { V9.2 replaced confusing --- with xX, this gets added to multipart/form-data header }
            FMimeBoundary := 'XxXx' + AnsiString(IntToHex(Random(MaxInt), 8) + IntToHex(Random(MaxInt), 8)) + 'XxXx';
        XBoundary := '--' + FMimeBoundary;  { V9.2 internal use with extra -- }
        if Count > 0 then begin
            FileStream := Nil;
            AddToStream(XBoundary + IcsCRLF);

        // if we set charset=utf-8, we can send utf8 without any encoding
            if FFormDataUtf8 then
                AddToStream('Content-Disposition: form-data; name="_charset_"' + IcsCRLF +
                                    IcsCRLF + 'utf-8' +
                                    IcsCRLF + XBoundary + IcsCRLF);
            for J := 0 to Count - 1 do begin
                if Sorted then
                    I := Integer(FSortList.Objects[J])
                else
                    I := J;
                PN := StringToUtf8(Trim(Items[I].PName));
                if NOT FFormDataUtf8 then
                    PN := IcsPercentEncode(PN);
                if PN <> '' then begin
                    PCT := AnsiString(Items[I].PConType);  { content-type may be optional set earlier }

               { open file and send binary content }
                    if (Items[I].RParamType = RPTypeFile) then begin
                        UploadFile := Trim(Items[I].PValue);
                        FLen := Items[I].PFileSize;
                        if FLen <= 0 then
                            FLen := IcsGetFileSize(UploadFile);
                        if (Flen > 0) then begin
                            PFN := StringToUtf8(ExtractFileName(UploadFile));
                            if NOT FFormDataUtf8 then
                                PFN := IcsPercentEncode(PFN);
                            if PCT = '' then  { required, should have been set according to file extension earlier }
                                PCT := 'application/octet-stream';
                            AddToStream('Content-Disposition: form-data; name="' + PN + '"; filename="' + PFN + '"' + IcsCRLF +
                                                                'Content-Type: ' + PCT + IcsCRLF + IcsCRLF);   // blank file before content
                            try
                                try
                                    FileStream := TIcsBufferedFileStream.Create(UploadFile, fmOpenRead, 0, IcsMBYTE);   { V9.4 remove warning }
                                    FileStream.Position := 0;
                               // need a new method with a callback for progress forGB files that may take minutes
                                    SLen := FParamStream.CopyFrom(FileStream, Flen);
                                    if (SLen <> Flen) then
                                        Result := False;  // failed, tell user
                                except
                                    Result := False;   // failed, tell user
                                end;
                            finally
                                FileStream.Destroy;
                            end;
                            AddToStream(IcsCRLF + XBoundary + IcsCRLF);  // CRLF and boundary at end of stream }
                        end
                        else
                           Result := False;   // failed, tell user
                    end
                    else begin
                      { assume all other parameters are textual, no processing for objects like Json }
                        K := 0;
                        ArrayLen := 0;
                        if (Items[I].RParamType = RPTypeArray) and Assigned(Items[I].FPValArray) then
                            ArrayLen := Items[I].PValArray.Count;
                        while true do begin
                            if ArrayLen > 0 then
                                PVS := Trim(Items[I].PValArray[K])
                            else
                                PVS := Trim(Items[I].PValue);
                            if Items[I].RParamType = RPTypeObj then
                                PV := AnsiString(PVS)
                            else begin
                                PV := StringToUtf8(PVS);
                                if NOT FFormDataUtf8 then
                                    PV := IcsPercentEncode(PV);
                            end;
                            AddToStream('Content-Disposition: form-data; name="' + PN + '"' + IcsCRLF);
                            if PCT <> '' then
                                AddToStream(PCT + IcsCRLF);  // ie'Content-Type: application/json
                            AddToStream(IcsCRLF + PV +
                                        IcsCRLF + XBoundary + IcsCRLF);
                            inc(K);
                            if K >= ArrayLen then break;
                        end;
                    end;
                end;
            end;
            FParamStream.Position := FParamStream.Size - 2;   // remove final CRLF
            AddToStream('--' + IcsCRLF);     // -- after boundary is end of content
        end;
    end;
    FParamStream.Position := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRestParams.GetStreamSize: Int64;        { V9.1 }
begin
    if Assigned(FParamStream) then
        Result := FParamStream.Size
    else
        Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRestParams.GetParametersTB(Sorted: Boolean = False): TBytes;  { V9.1  }
begin
    SetLength(Result, 0);
    if GetEstParamSize > MaxMemoryStreamSize then  // 50MB too large for memory stream
        Exit;
    if NOT Assigned(FParamStream) then begin
        FParamStream := TMemoryStream.Create;
        FStreamFlag := True;
    end;
    (FParamStream as TMemoryStream).Clear;
    if NOT GetParamStream(Sorted) then
        Exit;
    FParamStream.Position := 0;
    SetLength(Result, GetStreamSize);
    FParamStream.Read(Result[0], GetStreamSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRestParams.GetParameters(Sorted: Boolean = False): AnsiString;  { V8.65 added sorted }
begin
    Result := IcsTBytesToStringA(GetParametersTB(Sorted));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TRestParamsSrv }
{ V8.67 TRestParamsSrv extends TRestParams for database REST servers }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF DATABASE}
constructor TRestParamsSrv.Create;
begin
    inherited Create;
    FJsonRecord := TRestParams.Create(Nil);
    FJsonRecord.PContent := PContJson;
    FJsonResult := TRestParams.Create(Nil);
    FJsonResult.PContent := PContJson;
    FJsonArray := TIcsStringBuild.Create;
    FJsonArray.CharSize := 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TRestParamsSrv.Destroy;
begin
    FreeAndNil(FJsonRecord);
    FreeAndNil(FJsonRecord);
    FreeAndNil(FJsonArray);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{
  "success": false,
  "reccount":0,
  "errno":x,
  "errdesc":"xxxxxx"
  "query":"myquery"
  }
function TRestParamsSrv.JsonErr(RestErr: TRestErr; const ErrDesc: String = ''; const Query: String = ''): AnsiString;     { V9.1 added Query }
begin
    FJsonResult.Clear;
    FJsonResult.AddItem('success', false);
    FJsonResult.AddItem('reccount', 0);
    FJsonResult.AddItem('errno', Ord(RestErr));
    if ErrDesc <> '' then
        FJsonResult.AddItem('errdesc', ErrDesc)
    else
        FJsonResult.AddItem('errdesc', RestErrLits[RestErr]);
    if Query <> '' then                       { V9.1 }
            FJsonResult.AddItem('query', Query);
    Result := FJsonResult.GetParameters;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// convert database resultset into Json array
{
  "success": true,
  "reccount":1,
  "query":"myquery"
  "records":[]
  }
function TRestParamsSrv.ResultSet2Json(DataSet: TDataSet; var JsonStr: AnsiString; MaxRecs: Integer = 0; const Query: String = ''): Boolean; { V9.1 added Query }
var
    Recs, Flds, J: Integer;
    FName: String;
begin
    JsonStr := '';
    Result := False;
    Recs := 0;
    if NOT Assigned(DataSet) then begin
        JsonStr := JsonErr(RestErrInternal);
        Exit;
    end;
    try
        FJsonRecord.Clear;
        FJsonResult.Clear;
        FJsonArray.Clear;
        FJsonArray.CharSize := 1; // ansi buffer
        DataSet.First ;    // don't use RecCount, not always set
        FJsonArray.AppendBufA('[');
        while NOT DataSet.EOF do begin
            Recs := Recs + 1;
            FJsonRecord.Clear;
            Flds := DataSet.FieldCount ;
            for J := 0 to Flds - 1 do begin
                FName := DataSet.FieldDefs[J].DisplayName;
                if DataSet.Fields.Fields[J].IsNull then
                    FJsonRecord.AddItemNULL(FName)
                else if DataSet.Fields.Fields[J].DataType = ftBoolean then
                    FJsonRecord.AddItem(FName, DataSet.Fields.Fields[J].AsBoolean)
                else if DataSet.Fields.Fields[J].DataType in [ftInteger, ftSmallint, ftWord, ftLargeint] then
                    FJsonRecord.AddItem(FName, DataSet.Fields.Fields[J].AsInteger)
                else
                    FJsonRecord.AddItem(FName, DataSet.Fields.Fields[J].AsString);
            end;
            if Recs > 1 then
                FJsonArray.AppendBufA(',');
            FJsonArray.AppendBufA(FJsonRecord.GetParameters);   // UTF8
            if (MaxRecs > 0) and (Recs > MaxRecs) then
                break;
            DataSet.Next ;
        end;
        FJsonArray.AppendBufA(']');
        if Recs <= 0 then begin
            JsonStr := JsonErr(RestErrNoRecords);
            Exit;
        end;
        FJsonResult.AddItem('success', true);
        FJsonResult.AddItem('reccount', Recs);
        if Query <> '' then                       { V9.1 }
            FJsonResult.AddItem('query', Query);
        FJsonResult.AddItemA('records', FJsonArray.GetAString, True);
        JsonStr := FJsonResult.GetParameters;  // UTF8
        FJsonRecord.Clear;
        FJsonResult.Clear;
        FJsonArray.Clear;
        Result := True;
    except
       JsonStr := JsonErr(RestErrGeneral, 'ResultSet2Json - ' + IcsGetExceptMess (ExceptObject)) ;
    end;
end;

{$ENDIF}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ Retrieve a single value by name out of an URL encoded data stream         }
{ In the stream, every space is replaced by a '+'. The '%' character is     }
{ an escape character. The next two are 2 digits hexadecimal codes ascii    }
{ code value. The stream is constitued by name=value couples separated      }
{ by a single '&' character. The special characters are coded by the '%'    }
{ followed by hex-ascii character code.                                     }
function IcsExtractURLEncodedValue(
    Msg         : PChar;    { URL Encoded stream                     }
    Name        : String;   { Variable name to look for              }
    var Value   : String;   { Where to put variable value            }
    SrcCodePage : LongWord; { D2006 and older CP_UTF8 only           }
    DetectUtf8  : Boolean): Boolean;              { Found or not found that's the question }
var
    NameLen  : Integer;
    FoundLen : Integer; {tps}
    Ch       : AnsiChar;
    P, Q     : PChar;
    U8Str    : AnsiString;
begin
    Result  := FALSE;
    Value   := '';
    if Msg = nil then         { Empty source }
        Exit;

    NameLen := Length(Name);
    U8Str := '';
    P := Msg;
    while P^ <> #0 do begin
        Q := P;
        while (P^ <> #0) and (P^ <> '=') do
            Inc(P);
        FoundLen := P - Q; {tps}
        if P^ = '=' then
            Inc(P);
        if (StrLIComp(Q, @Name[1], NameLen) = 0) and
           (NameLen = FoundLen) then begin  {tps}
            while (P^ <> #0) and (P^ <> '&') do begin
                Ch := AnsiChar(Ord(P^)); // should contain nothing but < ord 128
                if Ch = '%' then begin
                    if P[1] <> #0 then    // V1.35 Added test
                        Ch := AnsiChar(htoi2(P + 1));
                    Inc(P, 2);
                end
                else if Ch = '+' then
                    Ch := ' ';
                U8Str := U8Str + Ch;
                Inc(P);
            end;
            Result := TRUE;
            break;
         end;
         while (P^ <> #0) and (P^ <> '&') do
             Inc(P);
        if P^ = '&' then
            Inc(P);
    end;
    if (SrcCodePage = CP_UTF8) or (DetectUtf8 and IsUtf8Valid(U8Str)) then
{$IFDEF COMPILER12_UP}
        Value := Utf8ToStringW(U8Str)
    else
        Value := AnsiToUnicode(U8Str, SrcCodePage);
{$ELSE}
        Value := Utf8ToStringA(U8Str)
    else
        Value := U8Str;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsExtractURLEncodedValue(
    const Msg   : String;           { URL Encoded stream                    }
    Name        : String;           { Variable name to look for             }
    var Value   : String;           { Where to put variable value           }
    SrcCodePage : LongWord = CP_ACP;{ D2006 and older CP_UTF8 only          }
    DetectUtf8  : Boolean  = TRUE): Boolean; overload;
begin
    Result := IcsExtractURLEncodedValue(PChar(Msg), Name, Value, SrcCodePage, DetectUtf8);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ return parameters as TStrings, name or name=value }
{ unlike ExtractURLEncodedValue control codes are left escaped too avoid multiple line values }
function IcsExtractURLEncodedParamList(
    Msg       : PChar;             { URL Encoded stream                     }
    Params    : TStrings;          { Where to put the list of parameters    }
    Values    : Boolean = False;   { V9.1 Should values be added name=value }
    SrcCodePage : LongWord = CP_ACP;{ D2006 and older CP_UTF8 only          }
    DetectUtf8  : Boolean  = TRUE): Integer;    {  Number of parameters found             }
var
    Name     : String;
    FoundLen : Integer;
    P, Q     : PChar;
    U8Str    : AnsiString;
    Value    : String;
    Ch       : AnsiChar;
begin
    Result  := 0;
    if Assigned(Params) then
        Params.Clear;
    if Msg = nil then         { Empty source }
        Exit;

    U8Str := '';
    P     := Msg;
    while P^ <> #0 do begin
        Q := P;
        while (P^ <> #0) and (P^ <> '=') do
            Inc(P);
        FoundLen := P - Q;
        if P^ = '=' then
            Inc(P);
        if Assigned(Params) then begin
            Name := Copy(Q, 0, FoundLen);
    //        Params.Add(Name);
        end;
        Inc(Result);
         if NOT Values then begin  { V9.1 are we skipping values }
            Params.Add(Name);
            while (P^ <> #0) and (P^ <> '&') do
                Inc(P);
         end
         else begin    { V9.1 get value }
            U8Str := '';
            while (P^ <> #0) and (P^ <> '&') do begin
                Ch := AnsiChar(Ord(P^)); // should contain nothing but < ord 128
                if Ch = '%' then begin
                    if P[1] <> #0 then begin
                        Ch := AnsiChar(htoi2(P + 1));
                        if Ch >= IcsSpace then  // leave control codes escaped
                            Inc(P, 2)
                        else
                            Ch := '%';
                    end;
                end
                else if Ch = '+' then
                    Ch := ' ';
                U8Str := U8Str + Ch;
                Inc(P);
            end;
            if (SrcCodePage = CP_UTF8) or (DetectUtf8 and IsUtf8Valid(U8Str)) then
{$IFDEF COMPILER12_UP}
                Value := Utf8ToStringW(U8Str)
            else
                Value := AnsiToUnicode(U8Str, SrcCodePage);
{$ELSE}
            Value := Utf8ToStringA(U8Str)
            else
            Value := U8Str;
{$ENDIF}
            Params.Add(Name + '=' + Value);
         end;
         if P^ = '&' then
            Inc(P);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsExtractURLEncodedParamList(
    const Msg : String;            { URL Encoded stream                     }
    Params    : TStrings;          { Where to put the list of parameters    }
    Values    : Boolean = False;   { V9.1 Should values be added name=value }
    SrcCodePage : LongWord = CP_ACP;{ D2006 and older CP_UTF8 only          }
    DetectUtf8  : Boolean  = TRUE)
    : Integer;                     { Number of parameters found             }
begin
    Result := IcsExtractURLEncodedParamList(PChar(Msg), Params, Values, SrcCodePage, DetectUtf8);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetCookieValue(
    const CookieString : String;   { Cookie string from header line         }
    const Name         : String;   { Cookie name to look for                }
    var   Value        : String)   { Where to put variable value            }
    : Boolean;                     { Found or not found that's the question }
var
    NameLen : Integer;
    FoundLen : Integer; { V7.42 }
    Ch      : Char;
    P, Q    : PChar;
begin
    Value   := '';
    Result  := FALSE;

    if (CookieString = '') or (Name = '') then
        Exit;

    NameLen := Length(Name);
    P := @CookieString[1];
    while P^ <> #0 do begin
        while (P^ <> #0) and (P^ = ' ') do
            Inc(P);
        Q := P;
        while (P^ <> #0) and (P^ <> '=') do
            Inc(P);
        FoundLen := P - Q; { V7.42 }
        if P^ = '=' then
            Inc(P);
        if (StrLIComp(Q, @Name[1], NameLen) = 0) and
           (NameLen = FoundLen) then begin  { V7.42 }
            while (P^ <> #0) and (P^ <> ';') do begin
                Ch := P^;
                if Ch = '%' then begin
                    Ch := chr(htoi2(P + 1));
                    Inc(P, 2);
                end
                else if Ch = '+' then
                    Ch := ' ';
                Value := Value + Ch;
                Inc(P);
            end;
            Result := TRUE;
            break;
        end;
        while (P^ <> #0) and (P^ <> ';') do
            Inc(P);
        if P^ = ';' then
            Inc(P);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 moved from OverbyteIcsHttpMulti.pas, added Ics }
{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path[?query]         }

// break down URL into its constituents
procedure IcsParseExURL (const url: string; var Proto, User, Pass, Host, Port, Dirs, Fname, Section, Query: string) ;
var
    path: string ;
    nsep1, nsep2: integer ;
begin
    Z.ICS9.OverbyteIcsUrl.ParseURL (url, Proto, User, Pass, Host, Port, path) ;
    Dirs := '' ;
    Fname := '' ;
    Section := '' ;
    Query := '' ;
    if path = '' then
        exit ;
    nsep1 := Pos ('#', path) ;

// remove section from path and keep it
    if (nsep1 > 0) then begin
        if nsep1 < length (path) then begin
            Section := copy (path, succ (nsep1), 999) ;
            nsep2 := Pos ('?', Section) ;
            if (nsep2 > 0) then begin
                if nsep2 < length (Section) then
                    Query := copy (Section, succ (nsep2), 999) ;
                Section := copy (Section, 1, pred (nsep2)) ;
            end ;
        end ;
        path := copy (path, 1, pred (nsep1)) ;
    end
// remove query from path and keep it
    else begin
        nsep2 := Pos ('?', path) ;
        if (nsep2 > 0) then begin
            if nsep2 < length (path) then
                Query := copy (path, succ (nsep2), 999) ;
            path := copy (path, 1, pred (nsep2)) ;
        end ;
    end ;

// remove file name from path and keep it and Dirs separately (no leading / on either)
    nsep1 := LastDelimiter ('/', path);
    if nsep1 > 1 then Dirs := copy (path, 2, pred (nsep1)) ;
    Fname := copy (path, succ (nsep1), 99) ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// build URL from the many (optional) constituent parts
function IcsBuildExURL (const Proto, User, Pass, Host, Port, Dirs, Fname, Section, Query: string): string ;
begin
    result := Proto + '://' ;
    if User <> '' then begin
        Result := Result + User ;
        if Pass <> '' then Result := Result + ':' + Pass ;
        Result := Result + '@' ;
    end ;
    Result := Result + Host ;
    if Port <> '' then
        Result := Result + ':' + Port ;
    Result := Result + '/' ;
    if Dirs <> '' then begin
        Result := Result + Dirs ;
        if Dirs [Length (Dirs)] <> '/' then
            Result := Result + '/' ;
    end ;
    if Pos('/', Fname) = 1 then        // V8.66 strip leading / from file
        Result := Result + Copy(Fname, 2, 999)
    else
        Result := Result + Fname ;
    if Section <> '' then
        Result := Result + '#' + Section ;
    if Query <> '' then
        Result := Result + '?' + Query ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// builds a new relative combined Dirs/Fname from relative name
// /magsys/other/ and ../mbs/index.htm gives /magsys/mbs/index.htm
// /magsys/other/ and /mbs/index.htm gives /mbs/index.htm
function IcsRelativeName (Dirs, Rname: String): string ;
var
    nsep: integer ;
begin
    result := Rname ;
    Dirs := trim (Dirs) ;
    if Dirs <> '' then begin
        if Dirs [Length (Dirs)] = '/' then
           SetLength (Dirs, Pred (Length (Dirs))) ;  // remove last /
    end ;
    while (Pos ('../', Rname) = 1) do  begin
        Rname := Copy (Rname, 4, 999) ;  // remove ../
        nsep := LastDelimiter ('/', Dirs) ;
        if nsep > 1 then
            Dirs := Copy (Dirs, 1, pred (nsep))
        else
            Dirs := '' ;
    end ;
    if Length (Rname) = 0 then
        exit ;
    if (Rname [1] = '/') then
        Dirs := '' ;    // assume root
    if Dirs <> '' then
        Result := Dirs + '/' + Rname
    else
        Result := Rname ;
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
