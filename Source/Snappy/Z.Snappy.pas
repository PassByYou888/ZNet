(*
https://zpascal.net
https://github.com/PassByYou888/ZNet
https://github.com/PassByYou888/zRasterization
https://github.com/PassByYou888/ZSnappy
https://github.com/PassByYou888/Z-AI1.4
https://github.com/PassByYou888/InfiniteIoT
https://github.com/PassByYou888/zMonitor_3rd_Core
https://github.com/PassByYou888/tcmalloc4p
https://github.com/PassByYou888/jemalloc4p
https://github.com/PassByYou888/zCloud
https://github.com/PassByYou888/ZServer4D
https://github.com/PassByYou888/zShell
https://github.com/PassByYou888/ZDB2.0
https://github.com/PassByYou888/zGameWare
https://github.com/PassByYou888/CoreCipher
https://github.com/PassByYou888/zChinese
https://github.com/PassByYou888/zSound
https://github.com/PassByYou888/zExpression
https://github.com/PassByYou888/ZInstaller2.0
https://github.com/PassByYou888/zAI
https://github.com/PassByYou888/NetFileService
https://github.com/PassByYou888/zAnalysis
https://github.com/PassByYou888/PascalString
https://github.com/PassByYou888/zInstaller
https://github.com/PassByYou888/zTranslate
https://github.com/PassByYou888/zVision
https://github.com/PassByYou888/FFMPEG-Header
*)
(*
  Snappy is a compression/decompression library.
  It does not aim for maximum compression, or compatibility with any other compression library; instead,
  it aims for very high speeds and reasonable compression.
  For instance, compared to the fastest mode of zlib, Snappy is an order of magnitude faster for most inputs,
  but the resulting compressed files are anywhere from 20% to 100% bigger. (For more information, see "Performance", below.)

  Snappy has the following properties:

  Fast: Compression speeds at 250 MB/sec and beyond, with no assembler code. See "Performance" below.

  Stable: Over the last few years, Snappy has compressed and decompressed petabytes of data in Google's production environment.
  The Snappy bitstream format is stable and will not change between versions.

  Robust: The Snappy decompressor is designed not to crash in the face of corrupted or malicious input.

  Free and open source software: Snappy is licensed under a BSD-type license. For more information, see the included COPYING file.

  Snappy has previously been called "Zippy" in some Google presentations and the like.

  ************************************************************************************************

  Snappy is intended to be fast. On a single core of a Core i7 processor in 64-bit mode,
  it compresses at about 250 MB/sec or more and decompresses at about 500 MB/sec or more.
  (These numbers are for the slowest inputs in our benchmark suite; others are much faster.)
  In our tests, Snappy usually is faster than algorithms in the same class (e.g. LZO, LZF, QuickLZ, etc.) while achieving comparable compression ratios.

  Typical compression ratios (based on the benchmark suite) are
  about 1.5-1.7x for plain text,
  about 2-4x for HTML,
  and of course 1.0x for JPEGs, PNGs and other already-compressed data.
  Similar numbers for zlib in its fastest mode are 2.6-2.8x, 3-7x and 1.0x, respectively.
  More sophisticated algorithms are capable of achieving yet higher compression rates, although usually at the expense of speed.
  Of course, compression ratio will vary significantly with the input.

  Although Snappy should be fairly portable, it is primarily optimized for 64-bit x86-compatible processors,
  and may run slower in other environments. In particular:

  Snappy uses 64-bit operations in several places to process more data at once than would otherwise be possible.

  Snappy assumes unaligned 32 and 64-bit loads and stores are cheap. On some platforms,
  these must be emulated with single-byte loads and stores, which is much slower.

  Snappy assumes little-endian throughout, and needs to byte-swap data in several places if running on a big-endian platform.

  Experience has shown that even heavily tuned code can be improved.
  Performance optimizations, whether for 64-bit x86 or other platforms, are of course most welcome; see "Contact", below.
*)
unit Z.Snappy;

{$I ..\Z.Define.inc}

interface

uses Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.MemoryStream;

const
{$IF Defined(WIN32)}
  C_Snappy_Lib = 'snappy_x86.dll';
  C_FuncPre = '';
{$ELSEIF Defined(WIN64)}
  C_Snappy_Lib = 'snappy_x64.dll';
  C_FuncPre = '';
{$ELSEIF Defined(Linux)}
  C_Snappy_Lib = 'libsnappy.so';
  C_FuncPre = '';
{$ELSE}
{$MESSAGE Error 'Unsupported platform'}
{$ENDIF}


type
  TMS64_Snappy_Helper_ = class helper for TMS64
  public
    function snappy_compress_To(inst: TMS64): Boolean; overload;
    function snappy_compress_To(inst: TMem64): Boolean; overload;
    function snappy_compress_From(inst: TMS64): Boolean; overload;
    function snappy_compress_From(inst: TMem64): Boolean; overload;
    function snappy_uncompress_From(inst: TMS64): Boolean; overload;
    function snappy_uncompress_From(inst: TMem64): Boolean; overload;
    function snappy_uncompress_To(inst: TMS64): Boolean; overload;
    function snappy_uncompress_To(inst: TMem64): Boolean; overload;
    function is_snappy_compressed: Boolean;
  end;

  TMem64_Snappy_Helper_ = class helper for TMem64
  public
    function snappy_compress_To(inst: TMS64): Boolean; overload;
    function snappy_compress_To(inst: TMem64): Boolean; overload;
    function snappy_compress_From(inst: TMS64): Boolean; overload;
    function snappy_compress_From(inst: TMem64): Boolean; overload;
    function snappy_uncompress_From(inst: TMS64): Boolean; overload;
    function snappy_uncompress_From(inst: TMem64): Boolean; overload;
    function snappy_uncompress_To(inst: TMS64): Boolean; overload;
    function snappy_uncompress_To(inst: TMem64): Boolean; overload;
    function is_snappy_compressed: Boolean;
  end;

  TSnappy_Status = (
    SNAPPY_OK = 0,
    SNAPPY_INVALID_INPUT = 1,
    SNAPPY_BUFFER_TOO_SMALL = 2
    );

function snappy_compress(input: Pointer; input_length: NativeUInt; compressed: Pointer; var compressed_length: NativeUInt): TSnappy_Status; cdecl;
  external C_Snappy_Lib name C_FuncPre + 'snappy_compress' {$IFDEF DELPHI}delayed {$ENDIF DELPHI};

function snappy_uncompress(compressed: Pointer; compressed_length: NativeUInt; uncompressed: Pointer; var uncompressed_length: NativeUInt): TSnappy_Status; cdecl;
  external C_Snappy_Lib name C_FuncPre + 'snappy_uncompress' {$IFDEF DELPHI}delayed {$ENDIF DELPHI};

function snappy_max_compressed_length(source_length: NativeUInt): NativeUInt; cdecl;
  external C_Snappy_Lib name C_FuncPre + 'snappy_max_compressed_length' {$IFDEF DELPHI}delayed {$ENDIF DELPHI};

function snappy_uncompressed_length(compressed: Pointer; compressed_length: NativeUInt; var result_: NativeUInt): TSnappy_Status; cdecl;
  external C_Snappy_Lib name C_FuncPre + 'snappy_uncompressed_length' {$IFDEF DELPHI}delayed {$ENDIF DELPHI};

function snappy_validate_compressed_buffer(compressed: Pointer; compressed_length: NativeUInt): TSnappy_Status; cdecl;
  external C_Snappy_Lib name C_FuncPre + 'snappy_validate_compressed_buffer' {$IFDEF DELPHI}delayed {$ENDIF DELPHI};

implementation

function TMS64_Snappy_Helper_.snappy_compress_To(inst: TMS64): Boolean;
var
  L: NativeUInt;
begin
  L := snappy_max_compressed_length(size);
  inst.size := L;
  Result := snappy_compress(memory, size, inst.memory, L) = TSnappy_Status.SNAPPY_OK;
  inst.size := L;
end;

function TMS64_Snappy_Helper_.snappy_compress_To(inst: TMem64): Boolean;
var
  L: NativeUInt;
begin
  L := snappy_max_compressed_length(size);
  inst.size := L;
  Result := snappy_compress(memory, size, inst.memory, L) = TSnappy_Status.SNAPPY_OK;
  inst.size := L;
end;

function TMS64_Snappy_Helper_.snappy_compress_From(inst: TMS64): Boolean;
begin
  Result := inst.snappy_compress_To(self);
end;

function TMS64_Snappy_Helper_.snappy_compress_From(inst: TMem64): Boolean;
begin
  Result := inst.snappy_compress_To(self);
end;

function TMS64_Snappy_Helper_.snappy_uncompress_From(inst: TMS64): Boolean;
var
  L: NativeUInt;
begin
  Result := False;
  if snappy_uncompressed_length(inst.memory, inst.size, L) = TSnappy_Status.SNAPPY_OK then
    begin
      size := L;
      Result := snappy_uncompress(inst.memory, inst.size, memory, L) = TSnappy_Status.SNAPPY_OK;
      if Result then
          size := L;
    end;
end;

function TMS64_Snappy_Helper_.snappy_uncompress_From(inst: TMem64): Boolean;
var
  L: NativeUInt;
begin
  Result := False;
  if snappy_uncompressed_length(inst.memory, inst.size, L) = TSnappy_Status.SNAPPY_OK then
    begin
      size := L;
      Result := snappy_uncompress(inst.memory, inst.size, memory, L) = TSnappy_Status.SNAPPY_OK;
      if Result then
          size := L;
    end;
end;

function TMS64_Snappy_Helper_.snappy_uncompress_To(inst: TMS64): Boolean;
begin
  Result := inst.snappy_uncompress_From(self);
end;

function TMS64_Snappy_Helper_.snappy_uncompress_To(inst: TMem64): Boolean;
begin
  Result := inst.snappy_uncompress_From(self);
end;

function TMS64_Snappy_Helper_.is_snappy_compressed: Boolean;
begin
  Result := snappy_validate_compressed_buffer(memory, size) = TSnappy_Status.SNAPPY_OK;
end;

function TMem64_Snappy_Helper_.snappy_compress_To(inst: TMS64): Boolean;
var
  L: NativeUInt;
begin
  L := snappy_max_compressed_length(size);
  inst.size := L;
  Result := snappy_compress(memory, size, inst.memory, L) = TSnappy_Status.SNAPPY_OK;
  inst.size := L;
end;

function TMem64_Snappy_Helper_.snappy_compress_To(inst: TMem64): Boolean;
var
  L: NativeUInt;
begin
  L := snappy_max_compressed_length(size);
  inst.size := L;
  Result := snappy_compress(memory, size, inst.memory, L) = TSnappy_Status.SNAPPY_OK;
  inst.size := L;
end;

function TMem64_Snappy_Helper_.snappy_compress_From(inst: TMS64): Boolean;
begin
  Result := inst.snappy_compress_To(self);
end;

function TMem64_Snappy_Helper_.snappy_compress_From(inst: TMem64): Boolean;
begin
  Result := inst.snappy_compress_To(self);
end;

function TMem64_Snappy_Helper_.snappy_uncompress_From(inst: TMS64): Boolean;
var
  L: NativeUInt;
begin
  Result := False;
  if snappy_uncompressed_length(inst.memory, inst.size, L) = TSnappy_Status.SNAPPY_OK then
    begin
      size := L;
      Result := snappy_uncompress(inst.memory, inst.size, memory, L) = TSnappy_Status.SNAPPY_OK;
      if Result then
          size := L;
    end;
end;

function TMem64_Snappy_Helper_.snappy_uncompress_From(inst: TMem64): Boolean;
var
  L: NativeUInt;
begin
  Result := False;
  if snappy_uncompressed_length(inst.memory, inst.size, L) = TSnappy_Status.SNAPPY_OK then
    begin
      size := L;
      Result := snappy_uncompress(inst.memory, inst.size, memory, L) = TSnappy_Status.SNAPPY_OK;
      if Result then
          size := L;
    end;
end;

function TMem64_Snappy_Helper_.snappy_uncompress_To(inst: TMS64): Boolean;
begin
  Result := inst.snappy_uncompress_From(self);
end;

function TMem64_Snappy_Helper_.snappy_uncompress_To(inst: TMem64): Boolean;
begin
  Result := inst.snappy_uncompress_From(self);
end;

function TMem64_Snappy_Helper_.is_snappy_compressed: Boolean;
begin
  Result := snappy_validate_compressed_buffer(memory, size) = TSnappy_Status.SNAPPY_OK;
end;

end.
 
