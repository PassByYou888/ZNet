{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Creation:     15 December 2005
Version:      V9.0
Description:  High level functions for ZLIB compression and decompression
Credit:       Based on work by Gabriel Corneanu <gabrielcorneanu(AT)yahoo.com>
              Derived from original sources by Bob Dellaca and Cosmin Truta.
              ZLIB is Copyright (C) 1995-2022 Jean-loup Gailly and Mark Adler
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2004-2023 by François PIETTE
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
Mar 26, 2006 V6.00 F. Piette started new version 6
9 Dec 2007   V6.01 Angus added missing compression levels
                  added ZlibCompressStreamEx overload with numeric level for ease of use
                    and callback functions
                  added ZlibDecompressStreamEx with callback functions
                  added ZlibErrMess to report zlib error messages as literals
                  added TZlibProgress callback
May 02, 2008 V6.02 A.Garrels prepared code for Unicode, type-changes from String
                   and PChar to AnsiString and PAnsiChar.
Aug 05, 2008 V6.03 F. Piette reverted ZlibErrMess to from AnsiString to String.
Sep 10, 2010 V7.00 Angus and Arno updated ZLIB to 1.2.5, subdirectory now lowercase
Apr 15, 2011 V7.01 Arno prepared for 64-bit.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory

Aug 12, 2020 V8.65 Lots of Longint to Integer and LongWord to Cardinal to keep MacOS64
                     happy in Obj and Dll ubits.
May 24, 2021 V8.67 Replaced soFromCurrent with soCurrent.
Apr 11, 2022 V8.69 Testing with Zlib 1.2.12.
Oct 20, 2022 V8.70 Now using USE_ZLIB_OBJ in OverbyteIcsDefs.inc instead of OverbyteIcsZlib.inc.
                   Support USE_DELPHI_ZLIB to use System.Zib to avoid linking ZLIB object files
                     twice.  Automatically defined for Delphi 11 and later which have ZLIB 1.2.12
                     (added in D11.1), but can be changed if older ZLIB versions are acceptable.
                   Changed some PAnsiChar to PBytes and integers to cardinals.
                   Note USE_ZLIB_OBJ and USE_DELPHI_ZLIB are no longer needed in any other
                     ICS units, all constants and functions are now here.
                   Added ZlibDecompressStream2 used in OverbyteIcsHttpCCodZLib which writes
                     to a callback function instead of a stream for HTTP content decompress.
Aug 08, 2023 V9.0  Updated version to major release 9.


pending: compress callback not correct total count


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Z.ICS9.OverbyteIcsZlibHigh;

interface

{$R-}
{$Q-}
{$I Include\Z.ICS9.OverbyteIcsDefs.inc}
//  Include\OverbyteIcsZlib.inc}   { V8.70 no longer used }
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
uses
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
{$IFDEF USE_ZLIB_OBJ}
   {$IFDEF USE_DELPHI_ZLIB}     { V8.70 }
   System.ZLib;
   {$ELSE}
    Z.ICS9.OverbyteIcsZLibObj;     {interface to access ZLIB C OBJ files}
   {$ENDIF}
{$ELSE}
    Z.ICS9.OverbyteIcsZLibDll;             {interface to access zLib1.dll} { AG V6.02 }
{$ENDIF}

const

{ V8.70 }
   { Allowed flush values; see deflate() below for details }
   Z_NO_FLUSH      = 0;
   Z_PARTIAL_FLUSH = 1;
   Z_SYNC_FLUSH    = 2;
   Z_FULL_FLUSH    = 3;
   Z_FINISH        = 4;
   Z_BLOCK         = 5;
   Z_TREES         = 6;     { V8.69 }

   Z_OK            = 0;
   Z_STREAM_END    = 1;
   Z_NEED_DICT     = 2;
   Z_ERRNO         = -1;
   Z_STREAM_ERROR  = -2;
   Z_DATA_ERROR    = -3;
   Z_MEM_ERROR     = -4;
   Z_BUF_ERROR     = -5;
   Z_VERSION_ERROR = -6;

   { compression levels }
   Z_NO_COMPRESSION         = 0;
   Z_BEST_SPEED             = 1;
   Z_BEST_COMPRESSION       = 9;
   Z_DEFAULT_COMPRESSION    = -1;

   { compression strategy; see deflateInit2() below for details }
   Z_FILTERED            = 1;
   Z_HUFFMAN_ONLY        = 2;
   Z_RLE                 = 3;
   Z_FIXED               = 4;       { V8.69 }
   Z_DEFAULT_STRATEGY    = 0;
   Z_MAX_STRATEGY        = 3;

   { Possible values of the data_type field }
   Z_BINARY   = 0;
   Z_ASCII    = 1;
   Z_TEXT     = 1;         { V8.69 }
   Z_UNKNOWN  = 2;

   { The deflate compression method (the only one supported in this version) }
   Z_DEFLATED = 8;

   Z_NULL  = nil;  { for initializing zalloc, zfree, opaque }

{other constants}
   Z_BUFSIZE = 16384;

  MAX_WBITS     = 15; { 32K LZ77 window }
  MAX_MEM_LEVEL = 9;
  DEF_MEM_LEVEL = 8;  { if MAX_MEM_LEVEL > 8 }

{xlb constants and variables, V8.70 moved from ZlibObj}
const
   Z_DLL_NOT_FOUND               = -97;
   Z_UNKNOWN_COMPRESSION_VERSION = -98;
   Z_CHECK_PROBLEM               = -99;

const
  WindowSize = 1 shl MAX_WBITS;

type
  TZlibProg = procedure(Sender: TObject; Count: Int64; var Cancel: Boolean);  { V6.01 }
  TZlibWrtFn = procedure(Sender: TObject; Buf: PByte; Size: Integer; var Cancel: Boolean);  { V8.70 }


type
  PZBack = ^TZBack;
  TZBack = record
    InStream  : TStream;
    OutStream : TStream;
    InMem     : PByte; //direct memory access     { V8.70 was AnsiChar }
    InMemSize : Cardinal;                         { V8.70 was Integer }
    ReadBuf   : array[word] of PByte;             { V8.70 was AnsiChar }
    Window    : array[0..WindowSize] of PByte;    { V8.70 was AnsiChar }
    MainObj   : TObject; // Angus
    ProgressCB : TZlibProg;    { V6.01 }
    Count     : Int64;         { V6.01 }
    WrtFnCB   : TZlibWrtFn;    { V8.70 }
  end;

type
  TZStreamType = (                       { V8.70 }
    zsZLib,  //standard zlib stream
    zsGZip,  //gzip stream
    zsRaw);  //raw stream (without any header)

  EZlibError = class(Exception);
  ECompressionError = class(EZlibError);
  EDecompressionError = class(EZlibError);

  TCompressionLevel = (clNone, clFastest, clDefault, clMax,
                       clLev0, clLev1, clLev2, clLev3, clLev4,     { V6.01 }
                       clLev5, clLev6, clLev7, clLev8, clLev9);

const
  Levels: array [TCompressionLevel] of ShortInt =
    (Z_NO_COMPRESSION, Z_BEST_SPEED, Z_DEFAULT_COMPRESSION, Z_BEST_COMPRESSION,
     Z_NO_COMPRESSION, Z_BEST_SPEED, 2, 3, 4, 5, 6, 7, 8, Z_BEST_COMPRESSION) ;

function deflateInitEx(var strm: z_stream; level: Integer; streamtype: TZStreamType = zsZLib): Integer;     { V8.70 }
function inflateInitEx(var strm: z_stream; streamtype: TZStreamType = zsZLib): Integer;                     { V8.70 }

function ZlibGetDllLoaded: boolean ;
function ZlibGetVersionDll: AnsiString ;
function ZlibCCheck(code: Integer): Integer;
function ZlibDCheck(code: Integer): Integer;

procedure ZlibDecompressStream(InStream, OutStream: TStream);
procedure ZlibDecompressStreamEx(InStream, OutStream: TStream; Sender: TObject; ProgCallback: TZlibProg); { V6.01 }
{ this version writes to a callback instead of a stream, for OverbyteIcsHttpCCodZLib }
procedure ZlibDecompressStream2(InStream: TStream; Sender: TObject; WrtFnCallback: TZlibWrtFn);   { V8.70 }

procedure ZlibCompressStreamEx(InStream, OutStream: TStream; NumLevel: Integer; StreamType : TZStreamType;
                                UseDirectOut: boolean; Sender: TObject; ProgCallback: TZlibProg); overload;  { V6.01 }
procedure ZlibCompressStreamEx(InStream, OutStream: TStream; Level: TCompressionLevel;
                                                StreamType : TZStreamType; UseDirectOut: boolean); overload;

function ZlibCheckInitInflateStream (var strm: TZStreamRec; gzheader: gz_headerp): TZStreamType;

function Strm_in_func(BackObj: PZBack; var buf: PByte): Integer; cdecl;
function Strm_out_func(BackObj: PZBack; buf: PByte; size: Integer): Integer; cdecl;
function DMAOfStream(AStream: TStream; out Available: Cardinal): Pointer;      { V8.70 was integer }
function ZlibErrMess(code: Integer): String;                        { V6.01 }

{ V8.70 moved from ZlibObj }
function  ZLibCheck(Code : Integer) : Integer;
procedure ZLibError;

{ V8.70 moved from ZlibObj }
var
   zlibProblemString : AnsiString;
   zlibProblemAlert  : boolean;
   zlibRaiseError    : boolean;

implementation

const
  WBits : array[TZStreamType] of integer = (MAX_WBITS, MAX_WBITS + 16, -MAX_WBITS);

const
   {return code messages}

   ZLibErrMsg  : array[-6..2] of PAnsiChar = (                                    { V8.70 }
     'incompatible version', // Z_VERSION_ERROR  (-6)
     'buffer error',         // Z_BUF_ERROR      (-5)
     'insufficient memory',  // Z_MEM_ERROR      (-4)
     'data error',           // Z_DATA_ERROR     (-3)
     'stream error',         // Z_STREAM_ERROR   (-2)
     'file error',           // Z_ERRNO          (-1)
     '',                     // Z_OK             (0)
     'stream end',           // Z_STREAM_END     (1)
     'need dictionary'       // Z_NEED_DICT      (2)
   );

   SZLibInvalid = 'Invalid ZStream operation!';                                 { V8.70 }

type
   EZLibCheckError = class(Exception);

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function deflateInitEx(var strm: z_stream; level: Integer; streamtype: TZStreamType = zsZLib): Integer;   { V8.70 }
begin
  Result := deflateInit2(strm, level, Z_DEFLATED, WBits[streamtype], MAX_MEM_LEVEL, Z_DEFAULT_STRATEGY);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function inflateInitEx(var strm: z_stream; streamtype: TZStreamType = zsZLib): Integer;                   { V8.70 }
begin
  Result := inflateInit2(strm, WBits[streamtype]);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ZlibGetDllLoaded: boolean ;
begin
{$IFDEF USE_ZLIB_OBJ}       { V8.70 was DLL only }
    Result     := true ;
{$ELSE}
    result := ZlibDllLoaded ;
{$ENDIF}
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ZlibGetVersionDll: AnsiString ;
begin
    result := zlibVersion ;                                                        { V8.70 was DLL only }
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ZlibErrMess(code: Integer): String;              { V6.01 }
begin
    case code of
        Z_OK            : Result := 'No error';
        Z_STREAM_END    : Result := 'Stream end';
        Z_NEED_DICT     : Result := 'Need dictionary';
        Z_ERRNO         : Result := 'Errno';
        Z_STREAM_ERROR  : Result := 'Stream error';
        Z_DATA_ERROR    : Result := 'Data error';
        Z_MEM_ERROR     : Result := 'Memory error';
        Z_BUF_ERROR     : Result := 'Buffer error';
        Z_VERSION_ERROR : Result := 'Version error';
    else
        Result := 'Unknown error ' + IntToStr(code);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ZlibCCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise ECompressionError.Create(ZlibErrMess(code));  //!! angus added code
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ZlibDCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise EDecompressionError.Create(ZlibErrMess(code));  //!! angus added code
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DMAOfStream(AStream: TStream; out Available: Cardinal): Pointer;           { V8.70 was integer }
begin
  if AStream.inheritsFrom(TCustomMemoryStream) then
    Result := TCustomMemoryStream(AStream).Memory
  else if AStream.inheritsFrom(TStringStream) then
    Result := Pointer(TStringStream(AStream).DataString)
  else
    Result := nil;
  if Result <> nil then
  begin
    //what if integer overflow?
    Available := AStream.Size - AStream.Position;
    Inc(PAnsiChar(Result), AStream.Position);
  end
  else Available := 0;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CanResizeDMAStream(AStream: TStream): boolean;
begin
  Result := AStream.inheritsFrom(TMemoryStream) or
            AStream.inheritsFrom(TStringStream);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
//tries to get the stream info
//strm.next_in and available_in needs enough data!
//strm should not contain an initialized inflate

function ZlibCheckInitInflateStream (var strm: TZStreamRec; gzheader: gz_headerp): TZStreamType;
var
  InitBuf: PByte; { V8.70 was PAnsiChar; }
  InitIn : integer;

  function TryStreamType(AStreamType: TZStreamType): boolean;
  begin
    ZlibDCheck(inflateInitEx(strm, AStreamType));

    if (AStreamType = zsGZip) and (gzheader <> nil) then
                  ZlibDCheck(inflateGetHeader(strm, gzheader^));

    Result := inflate(strm, Z_BLOCK) = Z_OK;
    ZlibDCheck(inflateEnd(strm));

    if Result then exit;
    //rollback
    strm.next_in  := InitBuf;
    strm.avail_in := InitIn;
  end;

begin
  if strm.next_out = nil then
    //needed for reading, but not used
    strm.next_out := strm.next_in;

  try
    InitBuf := strm.next_in;
    InitIn  := strm.avail_in;
    for Result := zsZLib to zsGZip do
      if TryStreamType(Result) then exit;
    Result := zsRaw;
  finally

  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Strm_in_func(BackObj: PZBack; var buf: PByte): Integer; cdecl;
var
  S : TStream;
  Cancel: boolean;
begin
  S := BackObj.InStream; //help optimizations
  if BackObj.InMem <> nil then
  begin
    //direct memory access if available!
    buf := Pointer(BackObj.InMem);
    //what if integer overflow?
    Result := S.Size - S.Position;
    S.Seek(Result, soCurrent);    { V8.67 was soFromCurrent }
  end
  else
  begin
    buf    := @BackObj.ReadBuf;
    Result := S.Read(buf^, SizeOf(BackObj.ReadBuf));
  end;
  if Assigned(BackObj.ProgressCB) then begin   { V6.01 tell user }
    inc (BackObj.Count, Result);    { V6.01 keep track of data read }
    Cancel := false;
    BackObj.ProgressCB(BackObj.MainObj, BackObj.Count, Cancel);
    if Cancel then Result := 0;  { pretend end of stream }
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Strm_out_func(BackObj: PZBack; buf: PByte; size: Integer): Integer; cdecl;
var
  Cancel: boolean;
begin
  Result := BackObj.OutStream.Write(buf^, size) - size;
  if Assigned(BackObj.ProgressCB) then begin   { V6.01 tell user }
    inc (BackObj.Count, size);    { V6.01 keep track of data read }
    Cancel := false;
    BackObj.ProgressCB(BackObj.MainObj, BackObj.Count, Cancel);
    if Cancel then Result := -1; { pretend write failed }
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  V6.01 }
procedure ZlibDecompressStreamEx(InStream, OutStream: TStream; Sender: TObject; ProgCallback: TZlibProg);
var
  strm   : z_stream;
  BackObj: PZBack;
begin
  FillChar(strm, sizeof(strm), 0);
  GetMem(BackObj, SizeOf(BackObj^));
  try
    //direct memory access if possible!
    BackObj.InMem := DMAOfStream(InStream, BackObj.InMemSize);

    BackObj.InStream  := InStream;
    BackObj.OutStream := OutStream;
    BackObj.MainObj := Sender;               { V6.01 stuff for callback }
    BackObj.ProgressCB := ProgCallback;
    BackObj.Count := 0;

    //use our own function for reading
    strm.avail_in := Strm_in_func(BackObj, PByte(strm.next_in));
    strm.next_out := @BackObj.Window;
    strm.avail_out := 0;

    ZlibCheckInitInflateStream(strm, nil);

    strm.next_out := nil;
    strm.avail_out := 0;
    ZlibDCheck(inflateBackInit(strm, MAX_WBITS, @BackObj.Window[0]));    { V8.70 }
    try
      ZlibDCheck(inflateBack(strm, @Strm_in_func, BackObj, @Strm_out_func, BackObj));
      //seek back when unused data
      InStream.Seek(-strm.avail_in, soCurrent);    { V8.67 was soFromCurrent }
      //now trailer can be checked
    finally
      ZlibDCheck(inflateBackEnd(strm));
    end;
  finally
    FreeMem(BackObj);
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ZlibDecompressStream(InStream, OutStream: TStream);
begin
    ZlibDecompressStreamEx(InStream, OutStream, Nil, Nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Strm_out_func2(BackObj: PZBack; buf: PByte; size: Integer): Integer; cdecl;       { V8.70 }
var
  Cancel: boolean;
begin
  Result := 0;
  Cancel := false;
  BackObj.WrtFnCB(BackObj.MainObj, buf, size, Cancel);
  if Cancel then
    Result := -1; { pretend write failed }
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   }
{ this version writes to a callback instead of a stream, for OverbyteIcsHttpCCodZLib }
procedure ZlibDecompressStream2(InStream: TStream; Sender: TObject; WrtFnCallback: TZlibWrtFn);   { V8.70 }
var
  strm   : z_stream;
  BackObj: PZBack;
begin
  FillChar(strm, sizeof(strm), 0);
  GetMem(BackObj, SizeOf(BackObj^));
  try
    //direct memory access if possible!
    BackObj.InMem := DMAOfStream(InStream, BackObj.InMemSize);

    BackObj.InStream  := InStream;
    BackObj.OutStream := Nil;
    BackObj.MainObj := Sender;
    BackObj.WrtFnCB := WrtFnCallback;
    BackObj.Count := 0;

    //use our own function for reading
    strm.avail_in := Strm_in_func(BackObj, PByte(strm.next_in));
    strm.next_out := @BackObj.Window;
    strm.avail_out := 0;

    ZlibCheckInitInflateStream(strm, nil);

    strm.next_out := nil;
    strm.avail_out := 0;
    ZlibDCheck(inflateBackInit(strm, MAX_WBITS, @BackObj.Window[0]));    { V8.70 }
    try
      ZlibDCheck(inflateBack(strm, @Strm_in_func, BackObj, @Strm_out_func2, BackObj));
      //seek back when unused data
      InStream.Seek(-strm.avail_in, soCurrent);    { V8.67 was soFromCurrent }
      //now trailer can be checked
    finally
      ZlibDCheck(inflateBackEnd(strm));
    end;
  finally
    FreeMem(BackObj);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
type
  TMemStreamHack = class(TMemoryStream);

function ExpandStream(AStream: TStream; const ACapacity : Int64): boolean;
begin
  Result := true;
  AStream.Size := ACapacity;
  if AStream.InheritsFrom(TMemoryStream) then
    AStream.Size := TMemStreamHack(AStream).Capacity;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    V6.01 }
procedure ZlibCompressStreamEx(InStream, OutStream: TStream;
            NumLevel: Integer; StreamType : TZStreamType; UseDirectOut: boolean;
                                            Sender: TObject; ProgCallback: TZlibProg);
const
  //64 KB buffer
  BufSize = 65536;
var
  strm   : z_stream;
  InBuf, OutBuf : PByte;    { V8.70  PAnsiChar; }
  UseInBuf, UseOutBuf : boolean;
  LastOutCount : Cardinal;  { V8.70 integer; }
  Finished : boolean;
  Cancel: boolean;
  Totcount: int64;

  procedure WriteOut;
  begin
    if UseOutBuf then
    begin
      if LastOutCount > 0 then
        OutStream.Write(OutBuf^, LastOutCount - strm.avail_out);
      strm.avail_out := BufSize;
      strm.next_out  := OutBuf;
    end
    else
    begin
      if (strm.avail_out = 0) then
        ExpandStream(OutStream, OutStream.Size + BufSize);
      OutStream.Seek(LastOutCount - strm.avail_out, soCurrent);   { V8.67 was soFromCurrent }
      strm.next_out  := DMAOfStream(OutStream, strm.avail_out);
      //because we can't really know how much resize is increasing!
    end;
    LastOutCount := strm.avail_out;
  end;

begin
  FillChar(strm, sizeof(strm), 0);

  InBuf          := nil;
  OutBuf         := nil;
  LastOutCount   := 0;
  Totcount       := 0;

  strm.next_in   := DMAOfStream(InStream, strm.avail_in);
  UseInBuf := strm.next_in = nil;

  if UseInBuf then
    GetMem(InBuf, BufSize);

  UseOutBuf := not (UseDirectOut and CanResizeDMAStream(OutStream));
  if UseOutBuf then
    GetMem(OutBuf, BufSize);

  ZlibCCheck(deflateInitEx(strm, NumLevel, StreamType));          { V6.01 }
  try
    repeat
      if strm.avail_in = 0 then
      begin
        if UseInBuf then
        begin
          strm.avail_in := InStream.Read(InBuf^, BufSize);
          strm.next_in  := InBuf;
        end;
        if strm.avail_in = 0 then break;
      end;
      if strm.avail_out = 0 then WriteOut;

      ZlibCCheck(deflate(strm, Z_NO_FLUSH));
      if Assigned(ProgCallback) then begin   { V6.01 tell user }
      {  inc (Totcount, strm.avail_in);    { V6.01 keep track of data read }
        Cancel := false;
        ProgCallback(Sender, TotCount, Cancel);
        if Cancel then break;
      end;
    until false;

    repeat
      if strm.avail_out = 0 then WriteOut;
      Finished := ZlibCCheck(deflate(strm, Z_FINISH)) = Z_STREAM_END;
      WriteOut;
    until Finished;

    if not UseOutBuf then
    begin
      //truncate when using direct output
      OutStream.Size := OutStream.Position;
    end;

    //adjust position of the input stream
    if UseInBuf then
      //seek back when unused data
      InStream.Seek(-strm.avail_in, soCurrent)  { V8.67 was soFromCurrent }
    else
      //simple seek
      InStream.Seek(strm.total_in, soCurrent);  { V8.67 was soFromCurrent }

    ZlibCCheck(deflateEnd(strm));
  finally
    if InBuf <> nil then FreeMem(InBuf);
    if OutBuf <> nil then FreeMem(OutBuf);
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   V6.01 }
procedure ZlibCompressStreamEx(InStream, OutStream: TStream; Level:
     TCompressionLevel; StreamType : TZStreamType; UseDirectOut: boolean);
begin
    ZlibCompressStreamEx(InStream, OutStream, Levels[Level], StreamType,
                                                     UseDirectOut, nil, nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ZLibCheck(Code : Integer) : Integer;                               { V8.70 moved from ZlibObj }
begin
     Result := Code;
     if (Code < 0) and (length(zlibProblemString) = 0) then
     begin
          zlibProblemAlert := true;
          if (Code < Z_VERSION_ERROR) then
          begin
               case Code of
                  Z_DLL_NOT_FOUND               : zlibProblemString := 'DLL not found';
                  Z_UNKNOWN_COMPRESSION_VERSION : zlibProblemString := 'Unknown compression stream version';
                  Z_CHECK_PROBLEM               : zlibProblemString := 'Check problem';
                                           else   zlibProblemString := 'Error #' + AnsiString(IntToStr(-Code));
               end;
          end else
               zlibProblemString := ZLibErrMsg[Code];

          if zlibRaiseError then
              raise EZLibCheckError.Create(String(zlibProblemString));
     end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ZLibError;                                                        { V8.70 moved from ZlibObj }
begin
     if (length(zlibProblemString) = 0) then
     begin
          zlibProblemAlert := true;
          zlibProblemString := SZLibInvalid;
          if zlibRaiseError then raise EZLibCheckError.Create(SZLibInvalid);
     end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
   zlibProblemString := '';                                                 { V8.70 }
   { EZLibCheckError }
   zlibProblemAlert  := false;
   zlibRaiseError    := true;

end.
