{
  file   : IcsZLibDll.pas
  date   : 11 June 2006
  author : Xavier Le Bris
  e-mail : xavier.lebris@free.fr
  ICS version: Angus Robertson

  Subject
  -------
  A Borland Delphi unit to interface zlib.dll functions
  see also in zLib package (zlib 1.2.5) \contrib\delphi\

  Acknowledgements
  ----------------
  Thanks to Jean-loup Gailly and Mark Adler for zLib library
         and Gilles Vollant for zlibwapi.dll

  zLib library version 1.2.5
  Copyright (C) 1995-2005 Jean-loup Gailly and Mark Adler
  Informations at http://www.zlib.net (or http://www.zlib.org)

  zlibwapi.dll 18/07/2005 17:46 (73�k)
  built by Gilles Vollant

  Adaptation
  ----------
  Xavier Le Bris
  xavier.lebris@free.fr   (english or french)
  06/03/2001; 29/12/2002 for zlib.dll
  07/12/2003 for zlib1.dll
  24/01/2004 for adaptation to calling convention stdcall or cdecl
  18/10/2004 for version 1.2.2
  26/07/2005 for version 1.2.3 and some bugs fixed (Thanks to Maurizio Lotauro)

  27 Nov 2005 by Angus Robertson, Magenta Systems
  delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
  Renamed the units for use with ICS from http://www.overbyte.be

  11 June 2006 by Angus Robertson, Magenta Systems
  added missing functions needed to support streaming

  02 May 2008 by A.Garrels <arno.garrels@gmx.de>
              Prepared code for Unicode, changed most types from String
              and PChar to AnsiString and PAnsiChar.
              Added missing function ZLibFlagsString.
              Compiles, however untested so far!

  24 Dec 2008 Two explicit string casts added.

  Sep 10, 2010 Angus and Arno updated ZLIB to 1.2.5
               Always use zlib1.dll if found in precedence to zlib.dll which is not 1.2.5
  Dec 29, 2010 Arno - Small change to support MacOS
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Aug 27, 2020 V8.65 - lots of Longint to Integer and LongWord to Cardinal to keep MacOS64 happy.
                     Never used gz stuff.
                     Changed MACOS to POSIX for Unix support.
Oct 20, 2022 V8.70 - Moved some literals and functions to ZlibHigh.
                     Changed some PAnsiChar to PBytes and integers to cardinals.
                     Corrected ZLIB version for latest DLL.
Aug 08, 2023 V9.0  Updated version to major release 9.



  My own work was to wrap access to dll functions in zlib.dll
  So, no copyright for this code, but don't copyright it !

  This code was tested with Borland Delphi 6. I guess it works with Delphi 2+

  This software is provided 'as-is', without any express or implied warranty.
  In no event will the author be held liable for any damages arising from the use of this software.

  This code is free under the same terms and conditions as 'zlib.'

  Note
  ----
  I use Gilles's dll because his implementation with assembler is faster than the official zlib1.dll
  So, in this implementation, the calling convention is stdcall (zlib1.dll uses cdecl)
  I rename it zlib.dll
  This unit can manage the both implementations (zlib.dll and zlib1.dll).
  See xZlib.inc for parameters.
}

unit Z.ICS9.OverbyteIcsZLibDll;

{$A-} {no 32 bits alignment for records}

interface

{$I Include\Z.ICS9.OverbyteIcsDefs.inc}

{$IFDEF MSWINDOWS}
uses
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF};
{$ENDIF}

const
{$IFDEF POSIX}
   ZLibDllName = '/usr/lib/libz.dylib'; // version 1.2.3 in OSX 10.6.8
{$ENDIF}
{$IFDEF MSWINDOWS}
   ZLibDllName    = 'ZLIB1.DLL';       // for official dll version 1.2.5 with cdecl
   ZLibDllNameBis = 'ZLIBXLB.DLL';
   ZLibDllNameTer = 'ZLIB.DLL';        // unofficial dll but maybe faster, with stdcall
{$ENDIF}

{xlb constants and variables}
const
   Z_DLL_NOT_FOUND               = -97;
   Z_UNKNOWN_COMPRESSION_VERSION = -98;
   Z_CHECK_PROBLEM               = -99;

var
   ZLibDllDirectory  : string;
   ZLibDllActualName : string;
   zlibDllLoaded     : boolean;
   zlibDllStartAt    : double;
   zlibProblemAlert  : boolean;
   zlibProblemString : AnsiString;
   zlibRaiseError    : boolean;
   ZLibWinapi        : boolean;

{zLib constants}
const
   ZLIB_VERSION    = '1.2.12';

   { Allowed flush values; see deflate() below for details }
(*   Z_NO_FLUSH      = 0;
   Z_PARTIAL_FLUSH = 1;
   Z_SYNC_FLUSH    = 2;
   Z_FULL_FLUSH    = 3;
   Z_FINISH        = 4;
   Z_BLOCK         = 5;

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
   Z_DEFAULT_STRATEGY    = 0;
   Z_MAX_STRATEGY        = 3;

   { Possible values of the data_type field }
   Z_BINARY   = 0;
   Z_ASCII    = 1;
   Z_UNKNOWN  = 2;

   { The deflate compression method (the only one supported in this version) }
   Z_DEFLATED = 8;

   Z_NULL  = nil;  { for initializing zalloc, zfree, opaque }

{other constants}
   Z_BUFSIZE = 16384;

   MAX_WBITS     = 15; { 32K LZ77 window }
   MAX_MEM_LEVEL = 9;
   DEF_MEM_LEVEL = 8;  { if MAX_MEM_LEVEL > 8 }
*)

{zLib types}
type
   tZLibCompressionStrategy = (ctStandard, ctFiltered, ctHuffmanOnly, ctRle);
   tZLibFlag = (zfuInt, zfuLong, zfvoidpf, zfz_off_t, zfdebug, zfasm, zfwinapi, zfbuildfixed, zfdynamic_crc_table, zfno_gzcompress, zfno_gzip, zfpkzip_bug, zffastest);

   alloc_func = function(opaque: Pointer; items, size: Integer): Pointer; cdecl;
   free_func  = procedure(opaque, address: Pointer); cdecl;

   in_func    = function(opaque: Pointer; var buf: PByte): Integer; cdecl;
   out_func   = function(opaque: Pointer; buf: PByte; size: Integer): Integer; cdecl;

   internal_state  = record end;
   pinternal_state = ^internal_state;

 { V8.65 all longint to Integer and Longword to Cardinal }
   z_streamp = ^z_stream;
   z_stream = record
      next_in     : pointer;           // next input byte
      avail_in    : Cardinal;          // number of bytes available at next_in  V8.70 was integer
      total_in    : Integer;           // total nb of input bytes read so far

      next_out    : pointer;           // next output byte should be put there
      avail_out   : Cardinal;          // remaining free space at next_out     V8.70 was integer
      total_out   : Integer;           // total nb of bytes output so far

      msg         : PAnsiChar;         // last error message, NULL if no error
      state       : pinternal_state;   // not visible by applications

      zalloc      : alloc_func;        // used to allocate the internal state
      zfree       : free_func;         // used to free the internal state
      AppData     : pointer;           // private data object passed to zalloc and zfree

      data_type   : Integer;           // best guess about the data type: ascii or binary
      adler       : Cardinal;          // adler32 value of the uncompressed data
      reserved    : Integer;           // reserved for future use
   end;

   pZStreamRec = ^tZStreamRec;
   tZStreamRec = z_stream;

   gz_stream = record
      stream      : z_stream;
      z_err       : Integer;           // error code for last stream operation
      z_eof       : Integer;           // set if end of input file
      gzfile      : pointer;           // .gz file
      inbuf       : pointer;           // input buffer
      outbuf      : pointer;           // output buffer
      crc         : Cardinal;          // crc32 of uncompressed data
      msg         : pAnsiChar;             // error message
      path        : pAnsiChar;             // path name for debugging only
      transparent : Integer;           // 1 if input file is not a .gz file
      mode        : AnsiChar;              // 'w' or 'r'
      start       : Integer;           // start of compressed data in file (header skipped)
      into        : Integer;           // bytes into deflate or inflate
      outof       : Integer;           // bytes out of deflate or inflate
      back        : Integer;           // one character push-back
      last        : Integer;           // true if push-back is last character
   end;

   pGzStreamRec = ^tGzStreamRec;
   tGzStreamRec = gz_stream;
   tGzFile      = pGzStreamRec;

(*
  gzip header information passed to and from zlib routines.  See RFC 1952
  for more details on the meanings of these fields.
*)
  gz_headerp = ^gz_header;
  gz_header = packed record
    text       : integer;   //* true if compressed data believed to be text */
    time       : Cardinal;  //* modification time */
    xflags     : integer;   //* extra flags (not used when writing a gzip file) */
    os         : integer;   //* operating system */
    extra      : PByte;     //* pointer to extra field or Z_NULL if none */
    extra_len  : Cardinal;  //* extra field length (valid if extra != Z_NULL) */
    extra_max  : Cardinal;  //* space at extra (only when reading header) */
    name       : PAnsiChar;     //* pointer to zero-terminated file name or Z_NULL */
    name_max   : Cardinal;  //* space at name (only when reading header) */
    comment    : PAnsiChar;     //* pointer to zero-terminated comment or Z_NULL */
    comm_max   : Cardinal;  //* space at comment (only when reading header) */
    hcrc       : integer;   //* true if there was or will be a header crc */
    done       : integer;   //* true when done reading gzip header (not used when writing a gzip file) */
  end;

  TZStreamType = (
    zsZLib,  //standard zlib stream
    zsGZip,  //gzip stream
    zsRaw);  //raw stream (without any header)
(*

   { Allowed flush values; see deflate() below for details }
   Z_NO_FLUSH      = 0;
   Z_PARTIAL_FLUSH = 1;
   Z_SYNC_FLUSH    = 2;
   Z_FULL_FLUSH    = 3;
   Z_FINISH        = 4;
   Z_BLOCK         = 5;

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
   Z_DEFAULT_STRATEGY    = 0;
   Z_MAX_STRATEGY        = 3;

   { Possible values of the data_type field }
   Z_BINARY   = 0;
   Z_ASCII    = 1;
   Z_UNKNOWN  = 2;

   { The deflate compression method (the only one supported in this version) }
   Z_DEFLATED = 8;

   Z_NULL  = nil;  { for initializing zalloc, zfree, opaque }

{other constants}
   Z_BUFSIZE = 16384;

   MAX_WBITS     = 15; { 32K LZ77 window }
   MAX_MEM_LEVEL = 9;
   DEF_MEM_LEVEL = 8;  { if MAX_MEM_LEVEL > 8 }

{zLib types}
type
   tZLibCompressionStrategy = (ctStandard, ctFiltered, ctHuffmanOnly, ctRle);
   tZLibFlag = (zfuInt, zfuLong, zfvoidpf, zfz_off_t, zfdebug, zfasm, zfwinapi, zfbuildfixed, zfdynamic_crc_table, zfno_gzcompress, zfno_gzip, zfpkzip_bug, zffastest);

   {for Dll calls}
   tAlloc = function (AppData : Pointer; Items, Size : longint) : pointer; cdecl;
   tFree = procedure (AppData, Block : pointer); cdecl;

   internal_state  = record end;
   pinternal_state = ^internal_state;

   z_streamp = ^z_stream;
   z_stream = record
      next_in     : pointer;           // next input byte
      avail_in    : longint;           // number of bytes available at next_in
      total_in    : longint;           // total nb of input bytes read so far

      next_out    : pointer;           // next output byte should be put there
      avail_out   : longint;           // remaining free space at next_out
      total_out   : longint;           // total nb of bytes output so far

      msg         : pChar;             // last error message, NULL if no error
      state       : pinternal_state;   // not visible by applications

      zalloc      : tAlloc;            // used to allocate the internal state
      zfree       : tFree;             // used to free the internal state
      AppData     : pointer;           // private data object passed to zalloc and zfree

      data_type   : longint;           // best guess about the data type: ascii or binary
      adler       : longword;          // adler32 value of the uncompressed data
      reserved    : longint;           // reserved for future use
   end;

   pZStreamRec = ^tZStreamRec;
   tZStreamRec = z_stream;

   gz_stream = record
      stream      : z_stream;
      z_err       : longint;           // error code for last stream operation
      z_eof       : longint;           // set if end of input file
      gzfile      : pointer;           // .gz file
      inbuf       : pointer;           // input buffer
      outbuf      : pointer;           // output buffer
      crc         : longword;          // crc32 of uncompressed data
      msg         : pChar;             // error message
      path        : pChar;             // path name for debugging only
      transparent : longint;           // 1 if input file is not a .gz file
      mode        : char;              // 'w' or 'r'
      start       : longint;           // start of compressed data in file (header skipped)
      into        : longint;           // bytes into deflate or inflate
      outof       : longint;           // bytes out of deflate or inflate
      back        : longint;           // one character push-back
      last        : longint;           // true if push-back is last character
   end;

   pGzStreamRec = ^tGzStreamRec;
   tGzStreamRec = gz_stream;
   tGzFile      = pGzStreamRec;

*)

{zLib functions}
{ V8.65 all longint to Integer and Longword to Cardinal }
function zlibVersionDll                : AnsiString;
function zlibVersion                   : AnsiString;    { V8.70 }
function zlibCompileFlags              : Cardinal;
function crc32                         (crc : Cardinal; const buf : pByte; len : Cardinal): Cardinal;

function deflateInit                   (var strm : TZStreamRec; level : Integer): Integer;
function deflateInit_                  (var strm : TZStreamRec; level : Integer; version : AnsiString; stream_size : Integer): Integer;
function deflateInit2                  (var strm : TZStreamRec; level, method, windowBits, memLevel, strategy : Integer) : Integer;
function deflateInit2_                 (var strm : TZStreamRec; level, method, windowBits, memLevel, strategy : Integer; version : AnsiString; stream_size : Integer): Integer;
function deflateParams                 (var strm : TZStreamRec; level, strategy: Integer): Integer;
function deflateBound                  (var strm : TZStreamRec; sourceLen : Cardinal): Cardinal;
function deflate                       (var strm : TZStreamRec; flush : Integer) : Integer;
function deflateEnd                    (var strm : TZStreamRec) : Integer;

function inflateInit                   (var strm : TZStreamRec) : Integer;
function inflateInit_                  (var strm : TZStreamRec; version : AnsiString; stream_size : Integer) : Integer;
function inflateInit2                  (var strm : TZStreamRec; windowBits : Integer): Integer;
function inflateInit2_                 (var strm : TZStreamRec; windowBits : Integer; version : AnsiString; stream_size : Integer) : Integer;
function inflate                       (var strm : TZStreamRec; flush : Integer) : Integer;
function inflateEnd                    (var strm : TZStreamRec) : Integer;
function inflateReset                  (var strm : TZStreamRec) : Integer;

{gzip functions}
{function gzOpen                        (path : AnsiString; mode : AnsiString) : tGzFile;
function gzSetParams                   (gzFile : tGzFile; level, strategy : Cardinal) : Integer;
function gzRead                        (gzFile : tGzFile; out buf; len : Cardinal): Integer;
function gzWrite                       (gzFile : tGzFile; const buf; len : Cardinal): Integer;
function gzClose                       (gzFile : tGzFile) : Integer;  }

{added functions}
function  ZLibCheck                    (Code : Integer) : Integer;
procedure ZLibError;
function  ZLibFlagsAnsiString              (ZLibFlag : tZLibFlag) : AnsiString;
procedure ZLibSetDeflateStateItem      (strm : TZStreamRec; Index : integer; Value : integer);
function  ZLibGetDeflateStateItem      (strm : TZStreamRec; Index : integer) : integer;

{dll functions}
procedure ZLibLoadDll                  (AZLibDllName : String);
procedure ZLibUnLoadDll;

{ angus - added more functions }
function inflateBackInit(var strm: z_stream;
                         windowBits: Integer; window: PAnsiChar): Integer;
function inflateBackInit_(var strm: z_stream;
                          windowBits: Integer; window: PAnsiChar;
                          const version: PAnsiChar; stream_size: Integer): Integer;
function inflateBack(var strm: z_stream; in_fn: in_func; in_desc: Pointer;
                     out_fn: out_func; out_desc: Pointer): Integer;
function inflateBackEnd(var strm: z_stream): Integer;

function deflateInitEx(var strm: z_stream; level: Integer; streamtype: TZStreamType = zsZLib): Integer;
function inflateInitEx(var strm: z_stream; streamtype: TZStreamType = zsZLib): Integer;

function deflateSetHeader(var strm: z_stream; var head: gz_header): integer;
function inflateGetHeader(var strm: z_stream; var head: gz_header): integer;

var
    Z_DS_MaxChainLen : integer;
    Z_DS_LazyMatch   : integer;
    Z_DS_GoodMatch   : integer;
    Z_DS_NiceMatch   : integer;

{==============================================================================}
implementation

uses {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    Z.ICS9.OverbyteIcsZlibHigh;                                              { V8.70 }


const
   {return code messages}
   ZLibErrMsg  : array[-6..2] of pAnsiChar = (
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

   SZLibInvalid = 'Invalid ZStream operation!';

type
   EZLibCheckError = class(Exception);

var
   zlibVersionDll_stdcall   : function         : pAnsiChar; stdcall;        {for both stdcall and cdecl because no argument}
   zlibCompileFlags_stdcall : function         : Cardinal; stdcall;

   {stdcall}
   crc32_stdcall            : function         (crc : Cardinal; const buf : pByte; len : Cardinal): Cardinal; stdcall;

   deflateInit_stdcall      : function         (var strm : TZStreamRec; level : Integer; version : PAnsiChar; stream_size : Integer) : Integer; stdcall;
   deflateInit2_stdcall     : function         (var strm : TZStreamRec; level, method, windowBits, memLevel, strategy : Integer; const version : PAnsiChar; stream_size : Integer): Integer; stdcall;
   deflate_stdcall          : function         (var strm : TZStreamRec; flush : Integer) : Integer; stdcall;
   deflateEnd_stdcall       : function         (var strm : TZStreamRec) : Integer; stdcall;

   inflateInit_stdcall      : function         (var strm : TZStreamRec; version : PAnsiChar; stream_size : Integer) : Integer; stdcall;
   inflateInit2_stdcall     : function         (var strm : TZStreamRec; windowBits : Integer; const version : PAnsiChar; stream_size : Integer) : Integer; stdcall;
   inflate_stdcall          : function         (var strm : TZStreamRec; flush : Integer) : Integer; stdcall;
   inflateEnd_stdcall       : function         (var strm : TZStreamRec) : Integer; stdcall;

   inflateReset_stdcall     : function         (var strm : TZStreamRec) : Integer; stdcall;
   deflateParams_stdcall    : function         (var strm : TZStreamRec; level, strategy: Integer): Integer; stdcall;
   deflateBound_stdcall     : function         (var strm : TZStreamRec; sourceLen : Cardinal): Cardinal;stdcall;

   {gzip functions}
{   gzOpen_stdcall           : function         (const path : pAnsiChar; const mode : pAnsiChar) : tGzFile; stdcall;
   gzSetParams_stdcall      : function         (gzFile : tGzFile; level, strategy : Cardinal) : Integer; stdcall;
   gzRead_stdcall           : function         (gzFile : tGzFile; out buf; len : Cardinal): Integer; stdcall;
   gzWrite_stdcall          : function         (gzFile : tGzFile; const buf; len : Cardinal): Integer; stdcall;
   gzClose_stdcall          : function         (gzFile : tGzFile) : Integer; stdcall;  }

   { angus - added more functions }
   inflateBackInit__stdcall : function        (var strm: z_stream;  windowBits: Integer; window: PAnsiChar;
                                                      const version: PAnsiChar; stream_size: Integer): Integer; stdcall;
   inflateBack_stdcall      : function        (var strm: z_stream; in_fn: in_func; in_desc: Pointer;
                                                 out_fn: out_func; out_desc: Pointer): Integer; stdcall;
   inflateBackEnd_stdcall   : function        (var strm: z_stream): Integer; stdcall;

   deflateSetHeader_stdcall : function        (var strm: z_stream; var head: gz_header): integer; stdcall;
   inflateGetHeader_stdcall : function        (var strm: z_stream; var head: gz_header): integer; stdcall;

   {cdecl}
   crc32_cdecl              : function         (crc : Cardinal; const buf : pByte; len : Cardinal): Cardinal; cdecl;

   deflateInit_cdecl        : function         (var strm : TZStreamRec; level : Integer; version : PAnsiChar; stream_size : Integer) : Integer; cdecl;
   deflateInit2_cdecl       : function         (var strm : TZStreamRec; level, method, windowBits, memLevel, strategy : Integer; const version : PAnsiChar; stream_size : Integer): Integer; cdecl;
   deflate_cdecl            : function         (var strm : TZStreamRec; flush : Integer) : Integer; cdecl;
   deflateEnd_cdecl         : function         (var strm : TZStreamRec) : Integer; cdecl;

   inflateInit_cdecl        : function         (var strm : TZStreamRec; version : PAnsiChar; stream_size : Integer) : Integer; cdecl;
   inflateInit2_cdecl       : function         (var strm : TZStreamRec; windowBits : Integer; const version : PAnsiChar; stream_size : Integer) : Integer; cdecl;
   inflate_cdecl            : function         (var strm : TZStreamRec; flush : Integer) : Integer; cdecl;
   inflateEnd_cdecl         : function         (var strm : TZStreamRec) : Integer; cdecl;

   inflateReset_cdecl       : function         (var strm : TZStreamRec) : Integer; cdecl;
   deflateParams_cdecl      : function         (var strm : TZStreamRec; level, strategy: Integer): Integer; cdecl;
   deflateBound_cdecl       : function         (var strm : TZStreamRec; sourceLen : Cardinal): Cardinal;cdecl;

   {gzip functions}
{   gzOpen_cdecl             : function         (const path : pAnsiChar; const mode : pAnsiChar) : tGzFile; cdecl;
   gzSetParams_cdecl        : function         (gzFile : tGzFile; level, strategy : Integer) : Integer; cdecl;
   gzRead_cdecl             : function         (gzFile : tGzFile; out buf; len : Cardinal): Integer; cdecl;
   gzWrite_cdecl            : function         (gzFile : tGzFile; const buf; len : Cardinal): Integer; cdecl;
   gzClose_cdecl            : function         (gzFile : tGzFile) : Integer; cdecl;   }

   { angus - added more functions }
   inflateBackInit__cdecl   : function        (var strm: z_stream;  windowBits: Integer; window: PAnsiChar;
                                                 const version: PAnsiChar; stream_size: Integer): Integer; cdecl;
   inflateBack_cdecl        : function        (var strm: z_stream; in_fn: in_func; in_desc: Pointer;
                                                 out_fn: out_func; out_desc: Pointer): Integer; cdecl;
   inflateBackEnd_cdecl     : function        (var strm: z_stream): Integer; cdecl;

   deflateSetHeader_cdecl   : function        (var strm: z_stream; var head: gz_header): integer; cdecl;
   inflateGetHeader_cdecl   : function        (var strm: z_stream; var head: gz_header): integer; cdecl;

(*
   function adler32                       (adler: uLong; const buf: pBytef; len: uInt): uLong;
   function deflateCopy                   (dest, source: z_streamp): int;
   function deflateReset                  (strm: z_streamp): int;
   function deflateSetDictionary          (strm: z_streamp; const dictionary: pBytef; dictLength: uInt): int;
   function inflateSetDictionary          (strm: z_streamp; const dictionary: pBytef; dictLength: uInt): int;
   function inflateSync                   (strm: z_streamp): int;
*)

{==============================================================================}
function ZLibFlagsString(ZLibFlag : tZLibFlag) : AnsiString;
var  Flags : Cardinal;

     function FlagSize(L : Cardinal) : AnsiString;
     var  N : Cardinal;
     begin
          N := (Flags shr L) and $0003;
          case N of
             0 : Result := '16';            {uInt}
             1 : Result := '32';            {uLong}
             2 : Result := '64';            {voidpf}
             3 : Result := '0';             {z_off_t}
          end;
     end;

     function FlagBit(L : Cardinal) : boolean;
     begin
          Result := (((Flags shr L) and $0001) = 1);
     end;
begin
     Result := '';
     Flags := zlibCompileFlags;
     case  ZLibFlag of
        zfuInt               : Result := 'uInt : ' + FlagSize(0);
        zfuLong              : Result := 'uLong : ' + FlagSize(2);
        zfvoidpf             : Result := 'voidpf : ' + FlagSize(4);
        zfz_off_t            : Result := 'z_off_t : ' + FlagSize(6);
        zfdebug              : if FlagBit(8)  then Result := 'debug';
        zfasm                : if FlagBit(9)  then Result := 'asm' else Result := 'noasm';
        zfwinapi             : if FlagBit(10) then Result := 'stdcall' else Result := 'cdecl';
        zfbuildfixed         : if FlagBit(12) then Result := 'buildfixed';
        zfdynamic_crc_table  : if FlagBit(13) then Result := 'dynamic_crc_table';
        zfno_gzcompress      : if FlagBit(16) then Result := 'no_gzcompress';
        zfno_gzip            : if FlagBit(17) then Result := 'no_gzip';
        zfpkzip_bug          : if FlagBit(20) then Result := 'pkzip_bug';
        zffastest            : if FlagBit(21) then Result := 'fastest';
     end;
end;
{==============================================================================}

function ZLibCheck(Code : Integer) : Integer;
begin
     Result := Code;
     if (Code < 0) and (length(zlibProblemString) = 0) then
     begin
          zlibProblemAlert := true;
          if (Code < Z_VERSION_ERROR) then
          begin
               case Code of
                  Z_DLL_NOT_FOUND               : zlibProblemString := 'Dll not found';
                  Z_UNKNOWN_COMPRESSION_VERSION : zlibProblemString := 'Unknwon compression stream version';
                  Z_CHECK_PROBLEM               : zlibProblemString := 'Check problem';
                                           else   zlibProblemString := 'Error ' + AnsiString(inttostr(-Code));
               end;
          end else
               zlibProblemString := ZLibErrMsg[Code];

          if zlibRaiseError then raise EZLibCheckError.Create(string(zlibProblemString));
     end;
end;
{==============================================================================}

procedure ZLibError;
begin
     if (length(zlibProblemString) = 0) then
     begin
          zlibProblemAlert := true;
          zlibProblemString := SZLibInvalid;
          if zlibRaiseError then raise EZLibCheckError.Create(SZLibInvalid);
     end;
end;
{==============================================================================}

function zlibVersionDll : AnsiString;
begin
     if Assigned(@zlibVersionDll_stdcall) then Result := zlibVersionDll_stdcall else Result := '';
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

{V8.70 }
function zlibVersion : AnsiString;
begin
    Result := zlibVersionDll ;
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function zlibCompileFlags : Cardinal;
begin
     if Assigned(@zlibCompileFlags_stdcall) then Result := zlibCompileFlags_stdcall else Result := 0;
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

{ Adaptative call to stdcall or cdecl }

function crc32(crc : Cardinal; const buf : pByte; len : Cardinal): Cardinal;
begin
     if ZLibWinapi then Result := crc32_stdcall(crc, buf, len)
                   else Result := crc32_cdecl  (crc, buf, len);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function deflateInit_(var strm : TZStreamRec; level : Integer; version : AnsiString; stream_size : Integer): Integer;
begin
     if ZLibWinapi then Result := deflateInit_stdcall(strm, level, pAnsiChar(version), stream_size)
                   else Result := deflateInit_cdecl  (strm, level, pAnsiChar(version), stream_size);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function deflateInit2_(var strm : TZStreamRec; level, method, windowBits, memLevel, strategy : Integer; version : AnsiString; stream_size : Integer): Integer;
begin
     if ZLibWinapi then Result := deflateInit2_stdcall(strm, level, method, windowBits, memLevel, strategy, pAnsiChar(version), stream_size)
                   else Result := deflateInit2_cdecl  (strm, level, method, windowBits, memLevel, strategy, pAnsiChar(version), stream_size);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function deflate(var strm : TZStreamRec; flush : Integer): Integer;
begin
     if ZLibWinapi then Result := deflate_stdcall(strm, flush)
                   else Result := deflate_cdecl  (strm, flush);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function deflateEnd(var strm : TZStreamRec): Integer;
begin
     if ZLibWinapi then Result := deflateEnd_stdcall(strm)
                   else Result := deflateEnd_cdecl  (strm);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function inflateInit_(var strm : TZStreamRec; version : AnsiString; stream_size : Integer): Integer;
begin
     if ZLibWinapi then Result := inflateInit_stdcall(strm, pAnsiChar(version), stream_size)
                   else Result := inflateInit_cdecl  (strm, pAnsiChar(version), stream_size);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function inflateInit2_(var strm : TZStreamRec; windowBits : Integer; version : AnsiString; stream_size : Integer): Integer;
begin
     if ZLibWinapi then Result := inflateInit2_stdcall(strm, windowBits, pAnsiChar(version), stream_size)
                   else Result := inflateInit2_cdecl  (strm, windowBits, pAnsiChar(version), stream_size);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function inflate(var strm : TZStreamRec; flush : Integer): Integer;
begin
     if ZLibWinapi then Result := inflate_stdcall(strm, flush)
                   else Result := inflate_cdecl  (strm, flush);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function inflateEnd(var strm : TZStreamRec): Integer;
begin
     if ZLibWinapi then Result := inflateEnd_stdcall(strm)
                   else Result := inflateEnd_cdecl  (strm);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function inflateReset(var strm : TZStreamRec): Integer;
begin
     if ZLibWinapi then Result := inflateReset_stdcall(strm)
                   else Result := inflateReset_cdecl  (strm);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function deflateParams(var strm : TZStreamRec; level, strategy : Integer) : Integer;
begin
     if ZLibWinapi then Result := deflateParams_stdcall(strm, level, strategy)
                   else Result := deflateParams_cdecl  (strm, level, strategy);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function deflateBound(var strm : TZStreamRec; sourceLen : Cardinal): Cardinal;
begin
     if ZLibWinapi then
     begin
          if Assigned(@deflateBound_stdcall) then Result := deflateBound_stdcall(strm, sourceLen)
                                             else Result := sourceLen + (sourceLen div 10) + 12 + 255;  {versions <= 1.1.4}
     end else
          Result := deflateBound_cdecl  (strm, sourceLen);
end;
{==============================================================================}

function deflateInit(var strm : TZStreamRec; level : Integer) : Integer;
begin
     Result := deflateInit_(strm, level, ZLIB_VERSION, sizeof(z_stream));
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function deflateInit2(var strm : TZStreamRec; level, method, windowBits, memLevel, strategy : Integer) : Integer;
begin
     Result := deflateInit2_(strm, level, method, windowBits, memLevel, strategy, ZLIB_VERSION, sizeof(z_stream));
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function inflateInit(var strm : TZStreamRec) : Integer;
begin
     Result := inflateInit_(strm, ZLIB_VERSION, sizeof(z_stream));
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function inflateInit2(var strm : TZStreamRec; windowBits : Integer): Integer;
begin
     Result := inflateInit2_(strm, windowBits, ZLIB_VERSION, sizeof(z_stream));
end;
{==============================================================================}
(*
function gzOpen (path : AnsiString; mode : AnsiString) : tGzFile;
begin
     if ZLibWinapi then Result := gzOpen_stdcall(pAnsiChar(path), pAnsiChar(mode))
                   else Result := gzOpen_cdecl(pAnsiChar(path), pAnsiChar(mode));
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function gzSetParams (gzFile : tGzFile; level, strategy : Integer) : Integer;
begin
     if ZLibWinapi then Result := gzSetParams_stdcall(gzFile, level, strategy)
                   else Result := gzSetParams_cdecl(gzFile, level, strategy);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function gzRead (gzFile : tGzFile; out buf; len : Cardinal): Integer;
begin
     if ZLibWinapi then Result := gzRead_stdcall(gzFile, buf, len)
                   else Result := gzRead_cdecl(gzFile, buf, len);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function gzWrite (gzFile : tGzFile; const buf; len : Cardinal): Integer;
begin
     if ZLibWinapi then Result := gzWrite_stdcall(gzFile, buf, len)
                   else Result := gzWrite_cdecl(gzFile, buf, len);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function gzClose (gzFile : tGzFile) : Integer;
begin
     if ZLibWinapi then Result := gzClose_stdcall(gzFile)
                   else Result := gzClose_cdecl(gzFile);
end;
*)
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

{ angus - added more functions }
function inflateBackInit(var strm: z_stream;
                         windowBits: Integer; window: PAnsiChar): Integer;
begin
    Result := inflateBackInit_(strm, windowBits, window,
                                 ZLIB_VERSION, sizeof(z_stream));
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}
function inflateBackInit_(var strm: z_stream; windowBits: Integer; window: PAnsiChar;
                          const version: PAnsiChar; stream_size: Integer): Integer;
begin
     if ZLibWinapi then Result := inflateBackInit__stdcall(strm, windowBits, window, version, stream_size)
                   else Result := inflateBackInit__cdecl(strm, windowBits, window, version, stream_size);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}
function inflateBack(var strm: z_stream; in_fn: in_func; in_desc: Pointer;
                     out_fn: out_func; out_desc: Pointer): Integer;
begin
     if ZLibWinapi then Result := inflateBack_stdcall(strm, in_fn, in_desc, out_fn, out_desc)
                   else Result := inflateBack_cdecl(strm, in_fn, in_desc, out_fn, out_desc) ;
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}
function inflateBackEnd(var strm: z_stream): Integer;
begin
     if ZLibWinapi then Result := inflateBackEnd_stdcall(strm)
                   else Result := inflateBackEnd_cdecl(strm);
end;

{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

const
  WBits : array[TZStreamType] of integer = (MAX_WBITS, MAX_WBITS + 16, -MAX_WBITS);

function deflateInitEx(var strm: z_stream; level: Integer; streamtype: TZStreamType = zsZLib): Integer;
begin
  Result := deflateInit2(strm, level, Z_DEFLATED, WBits[streamtype],
    MAX_MEM_LEVEL, Z_DEFAULT_STRATEGY);
end;

{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}
function inflateInitEx(var strm: z_stream; streamtype: TZStreamType = zsZLib): Integer;
begin
  Result := inflateInit2(strm, WBits[streamtype]);
end;

{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}
function deflateSetHeader(var strm: z_stream; var head: gz_header): integer;
begin
     if ZLibWinapi then Result := deflateSetHeader_stdcall(strm, head)
                   else Result := deflateSetHeader_cdecl(strm, head);
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}
function inflateGetHeader(var strm: z_stream; var head: gz_header): integer;
begin
     if ZLibWinapi then Result := inflateGetHeader_stdcall(strm, head)
                   else Result := inflateGetHeader_cdecl(strm, head);
end;

{==============================================================================}
function ZLibFlagsAnsiString(ZLibFlag : tZLibFlag) : AnsiString;
var  Flags : Cardinal;

     function FlagSize(L : Cardinal) : AnsiString;
     var  N : Cardinal;
     begin
          N := (Flags shr L) and $0003;
          case N of
             0 : Result := '16';            {uInt}
             1 : Result := '32';            {uLong}
             2 : Result := '64';            {voidpf}
             3 : Result := '0';             {z_off_t}
          end;
     end;

     function FlagBit(L : Cardinal) : boolean;
     begin
          Result := (((Flags shr L) and $0001) = 1);
     end;
begin
     Result := '';
     if not ZLibDllLoaded or not Assigned(@zlibCompileFlags_stdcall) then exit;
     Flags := zlibCompileFlags;
     case  ZLibFlag of
        zfuInt               : Result := 'uInt : ' + FlagSize(0);
        zfuLong              : Result := 'uLong : ' + FlagSize(2);
        zfvoidpf             : Result := 'voidpf : ' + FlagSize(4);
        zfz_off_t            : Result := 'z_off_t : ' + FlagSize(6);
        zfdebug              : if FlagBit(8)  then Result := 'debug';
        zfasm                : if FlagBit(9)  then Result := 'asm' else Result := 'noasm';
        zfwinapi             : if FlagBit(10) then Result := 'stdcall' else Result := 'cdecl';
        zfbuildfixed         : if FlagBit(12) then Result := 'buildfixed';
        zfdynamic_crc_table  : if FlagBit(13) then Result := 'dynamic_crc_table';
        zfno_gzcompress      : if FlagBit(16) then Result := 'no_gzcompress';
        zfno_gzip            : if FlagBit(17) then Result := 'no_gzip';
        zfpkzip_bug          : if FlagBit(20) then Result := 'pkzip_bug';
        zffastest            : if FlagBit(21) then Result := 'fastest';
     end;
end;
{==============================================================================}
{ for XLB purposes only : access to internal zlib variables : only for versions 1.1.4 and 1.2.x; must be updated in future versions }

const
     Z_DS_MaxItemsMax = 34;
var
     Z_DS_MaxItems    : integer;

type pDeflateState = ^tDeflateState;
     tDeflateState = array[0..Z_DS_MaxItemsMax] of Integer;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}
(*   dans match.s
     WSize 36 WMask 44 Window 48 Prev 56 MatchLen 88 PrevMatch 92 StrStart 100 MatchStart 104 Lookahead 108 PrevLen 112
     MaxChainLen 116 GoodMatch 132 NiceMatch 136 *)

procedure ZLibDeflateStateInit;
var  V : AnsiString;
begin
     Z_DS_MaxItems    := 0;
     Z_DS_MaxChainLen := 0;
     Z_DS_LazyMatch   := 0;
     Z_DS_GoodMatch   := 0;
     Z_DS_NiceMatch   := 0;

     if ZLibDllLoaded then V := zlibVersionDll else exit;
     if (V = '1.1.4') or (copy(V,1,3) = '1.2') then
     begin
          Z_DS_MaxItems    := Z_DS_MaxItemsMax;  {34}
          Z_DS_MaxChainLen := 29; {116 / 4}
          Z_DS_LazyMatch   := 30;
          Z_DS_GoodMatch   := 33; {132 / 4}
          Z_DS_NiceMatch   := 34; {136 / 4}
     end;
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

procedure ZLibSetDeflateStateItem(strm : TZStreamRec; Index : integer; Value : integer);
var  PtrDS : pDeflateState;
     V : AnsiString;
begin
     if ZLibDllLoaded then V := zlibVersionDll else exit;
     if (Z_DS_MaxItems > 0) and (V = '1.1.4') or (copy(V,1,3) = '1.2') then
     begin
          PtrDS := pDeflateState(strm.state);
          if (PtrDS <> nil) and (Index in [0..Z_DS_MaxItems]) then PtrDS^[Index] := Value;
     end;
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

function ZLibGetDeflateStateItem(strm : TZStreamRec; Index : integer) : integer;
var  PtrDS : pDeflateState;
     V : AnsiString;
begin
     Result := 0;
     if ZLibDllLoaded then V := zlibVersionDll else exit;
     if (Z_DS_MaxItems > 0) and (V = '1.1.4') or (copy(V,1,3) = '1.2') then
     begin
          PtrDS := pDeflateState(strm.state);
          if (PtrDS <> nil) and (Index in [0..Z_DS_MaxItems]) then Result := PtrDS^[Index];
     end;
end;
{==============================================================================}
{ Interface of the dll : this code comes from zlib.pas with minor adaptations  }
{==============================================================================}

type
    TBuffer = array[0..511] of Char;

var ZLibDLLHandle : HModule;

{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

procedure ZLibUnLoadDll;
begin
     FreeLibrary(ZLibDLLHandle);
     ZLibDLLHandle := 0;
     ZLibDllLoaded := false;
     zlibVersionDll_stdcall := nil;
     ZLibDllDirectory := '';
     ZLibWinapi := false;
     ZLibDeflateStateInit;
end;
{覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧覧}

procedure ZLibLoadDll(AZLibDllName : string);
var  Buffer : TBuffer;
begin
     if (ZLibDLLHandle > 0) then ZLibUnLoadDll;

     ZLibDllActualName := AZLibDllName;
   {$IFDEF MSWINDOWS}
     SetErrorMode($8000 {SEM_NoOpenFileErrorBox});
   {$ENDIF}
     ZLibDLLHandle := LoadLibrary(pChar(ZLibDllActualName));
   {$IFDEF MSWINDOWS}
     if (ZLibDLLHandle = 0) then
     begin
          ZLibDLLHandle := LoadLibrary(pChar(ZLibDllNameBis));
          if (ZLibDLLHandle > 0) then ZLibDllActualName := ZLibDllNameBis;
     end;
     if (ZLibDLLHandle = 0) then
     begin
          ZLibDLLHandle := LoadLibrary(pChar(ZLibDllNameTer));
          if (ZLibDLLHandle > 0) then ZLibDllActualName := ZLibDllNameTer;
     end;
   {$ENDIF}
     if (ZLibDLLHandle > 0) then
     begin
          @zlibVersionDll_stdcall          := GetProcAddress(ZLibDLLHandle,'zlibVersion');
          ZLibDllLoaded                    := Assigned(@zlibVersionDll_stdcall);
          if not ZLibDllLoaded then exit;

          @zlibCompileFlags_stdcall        := GetProcAddress(ZLibDLLHandle,'zlibCompileFlags');   {for 1.2.1 and more}
          if Assigned(@zlibCompileFlags_stdcall) then ZLibWinapi := (ZLibFlagsString(zfwinapi) = 'stdcall')
                                                 else ZLibWinapi := true;                         {before 1.2.1}

          if ZLibWinapi then
          begin
             @crc32_stdcall                := GetProcAddress(ZLibDLLHandle,'crc32');
             @deflateParams_stdcall        := GetProcAddress(ZLibDLLHandle,'deflateParams');
             @deflateBound_stdcall         := GetProcAddress(ZLibDLLHandle,'deflateBound');
             @deflateInit_stdcall          := GetProcAddress(ZLibDLLHandle,'deflateInit_');
             @deflateInit2_stdcall         := GetProcAddress(ZLibDLLHandle,'deflateInit2_');
             @deflate_stdcall              := GetProcAddress(ZLibDLLHandle,'deflate');
             @deflateEnd_stdcall           := GetProcAddress(ZLibDLLHandle,'deflateEnd');

             @inflateReset_stdcall         := GetProcAddress(ZLibDLLHandle,'inflateReset');
             @inflateInit_stdcall          := GetProcAddress(ZLibDLLHandle,'inflateInit_');
             @inflateInit2_stdcall         := GetProcAddress(ZLibDLLHandle,'inflateInit2_');
             @inflate_stdcall              := GetProcAddress(ZLibDLLHandle,'inflate');
             @inflateEnd_stdcall           := GetProcAddress(ZLibDLLHandle,'inflateEnd');

        {     @gzOpen_stdcall               := GetProcAddress(ZLibDLLHandle,'gzopen');
             @gzRead_stdcall               := GetProcAddress(ZLibDLLHandle,'gzread');
             @gzWrite_stdcall              := GetProcAddress(ZLibDLLHandle,'gzwrite');
             @gzClose_stdcall              := GetProcAddress(ZLibDLLHandle,'gzclose');   }

             { angus - added more functions }
             @inflateBack_stdcall          := GetProcAddress(ZLibDLLHandle,'inflateBack');
             @inflateBackEnd_stdcall       := GetProcAddress(ZLibDLLHandle,'inflateBackEnd');
             @inflateBackInit__stdcall     := GetProcAddress(ZLibDLLHandle,'inflateBackInit_');
             @deflateSetHeader_stdcall     := GetProcAddress(ZLibDLLHandle,'deflateSetHeader');
             @inflateGetHeader_stdcall     := GetProcAddress(ZLibDLLHandle,'inflateGetHeader');

          end else
          begin
             @crc32_cdecl                  := GetProcAddress(ZLibDLLHandle,'crc32');
             @deflateParams_cdecl          := GetProcAddress(ZLibDLLHandle,'deflateParams');
             @deflateBound_cdecl           := GetProcAddress(ZLibDLLHandle,'deflateBound');
             @deflateInit_cdecl            := GetProcAddress(ZLibDLLHandle,'deflateInit_');
             @deflateInit2_cdecl           := GetProcAddress(ZLibDLLHandle,'deflateInit2_');
             @deflate_cdecl                := GetProcAddress(ZLibDLLHandle,'deflate');
             @deflateEnd_cdecl             := GetProcAddress(ZLibDLLHandle,'deflateEnd');

             @inflateReset_cdecl           := GetProcAddress(ZLibDLLHandle,'inflateReset');
             @inflateInit_cdecl            := GetProcAddress(ZLibDLLHandle,'inflateInit_');
             @inflateInit2_cdecl           := GetProcAddress(ZLibDLLHandle,'inflateInit2_');
             @inflate_cdecl                := GetProcAddress(ZLibDLLHandle,'inflate');
             @inflateEnd_cdecl             := GetProcAddress(ZLibDLLHandle,'inflateEnd');

         {    @gzOpen_cdecl                 := GetProcAddress(ZLibDLLHandle,'gzopen');
             @gzRead_cdecl                 := GetProcAddress(ZLibDLLHandle,'gzread');
             @gzWrite_cdecl                := GetProcAddress(ZLibDLLHandle,'gzwrite');
             @gzClose_cdecl                := GetProcAddress(ZLibDLLHandle,'gzclose');  }

             { angus - added more functions }
             @inflateBack_cdecl            := GetProcAddress(ZLibDLLHandle,'inflateBack');
             @inflateBackEnd_cdecl         := GetProcAddress(ZLibDLLHandle,'inflateBackEnd');
             @inflateBackInit__cdecl       := GetProcAddress(ZLibDLLHandle,'inflateBackInit_');
             @deflateSetHeader_cdecl       := GetProcAddress(ZLibDLLHandle,'deflateSetHeader');
             @inflateGetHeader_cdecl       := GetProcAddress(ZLibDLLHandle,'inflateGetHeader');
          end;
(*
             @deflateSetDictionary := GetProcAddress(ZLibDLLHandle,'deflateSetDictionary');
             @deflateCopy          := GetProcAddress(ZLibDLLHandle,'deflateCopy');
             @deflateReset         := GetProcAddress(ZLibDLLHandle,'deflateReset');

             @inflateSetDictionary := GetProcAddress(ZLibDLLHandle,'inflateSetDictionary');
             @inflateSync          := GetProcAddress(ZLibDLLHandle,'inflateSync');

             @compress             := GetProcAddress(ZLibDLLHandle,'compress');
             @uncompress           := GetProcAddress(ZLibDLLHandle,'uncompress');

             @gzdopen              := GetProcAddress(ZLibDLLHandle,'gzdopen');
             @gzerror              := GetProcAddress(ZLibDLLHandle,'gzerror');

             @adler32              := GetProcAddress(ZLibDLLHandle, 'adler32');
*)
             zlibDllStartAt := Now;

             GetModuleFileName(ZLibDLLHandle, Buffer, SizeOf(Buffer));
             ZLibDllDirectory := ExtractFilePath(string(Buffer));
     end else
     begin
          ZLibDllLoaded := false;
          ZLibWinapi := false;
          ZLibDllDirectory := '';
     end;
     ZLibDeflateStateInit;
end;
{==============================================================================}

initialization
   ZLibDllDirectory := '';
   ZLibDllActualName := '';
   zlibDllLoaded := false;
   zlibDllStartAt := 0;
   zlibProblemAlert := false;
   zlibProblemString := '';
   zlibRaiseError := true;
   ZLibDLLHandle := 0;
   ZLibWinapi := false;
   ZLibDeflateStateInit;
   ZLibLoadDll(ZLibDllName);

finalization
   ZLibUnLoadDll;
{==============================================================================}
end.
