unit Z.ICS9.OverbyteIcsHttpCCodZLib;
{
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2004-2023 by Fran�ois PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.

To enable ContentCoding, in THttpCli set httpoEnableContentCoding in Options
and include this unit in your application!!!

May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Oct 8, 2012  V8.01 - Angus, announce deflate is supported as well as gzip
May 24, 2021 V8.67 Replaced soFromCurrent with soCurrent.
Apr 11, 2022 V8.69 Testing with Zlib 1.2.12.
Oct 20, 2022 V8.70 Rewrote Complete to use high level ZLIB function instead of low
                     level ZLIB functions and OverbyteIcsZLibObj so ZLIB choice is
                     made in OverbyteIcsZlibHigh which now also supports System.ZLIB
                     in modern compilers.
Aug 08, 2023 V9.0  Updated version to major release 9.

}

interface

{$I Include\Z.ICS9.OverbyteIcsDefs.inc}

uses
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    Z.ICS9.OverbyteIcsHttpContCod,
    Z.ICS9.OverbyteIcsZlibHigh;

type
  THttpCCodzlib = class(THttpContentCoding)
  private
    FStream: TMemoryStream;
  protected
    class function GetActive: Boolean; override;
    class function GetCoding: String; override;
  public
    constructor Create(WriteBufferProc: TWriteBufferProcedure); override;
    destructor Destroy; override;

    procedure Complete; override;
    procedure WriteBuffer(Buffer: Pointer; Count: Integer); override;
  end;

implementation

{ THttpCCodzlib }

constructor THttpCCodzlib.Create(WriteBufferProc: TWriteBufferProcedure);
begin
    inherited Create(WriteBufferProc);
    FStream := TMemoryStream.Create;
end;

destructor THttpCCodzlib.Destroy;
begin
    FStream.Free;
    inherited Destroy;
end;

class function THttpCCodzlib.GetActive: Boolean;
begin
    Result := ZlibGetDllLoaded and (Pos('1.2', String(ZlibGetVersionDll)) = 1);
end;

class function THttpCCodzlib.GetCoding: String;
begin
    Result := 'gzip, deflate';   // V8.01
end;

(*
function Strm_Write(BackObj: PZBack; buf: PByte; size: Integer): Integer; cdecl;
begin
    THttpCCodzlib (BackObj.MainObj).OutputWriteBuffer(buf, size) ;
    Result := 0 ;  // assume we wrote all the data OK
end;

{ write the content stream a block at a time, it handles gzip, zlib or raw streams }
procedure THttpCCodzlib.Complete;
var
    strm: z_stream;
    BackObj: PZBack;
begin
    FStream.Position := 0;
    FillChar (strm, sizeof(strm), 0);
    GetMem (BackObj, SizeOf(BackObj^));
    try
        BackObj.InMem := FStream.Memory;  //direct memory access
        BackObj.InStream  := FStream;
        BackObj.OutStream := Nil ;        // not used
        BackObj.MainObj := Self;
        BackObj.ProgressCallback :=  nil;
    //use our own function for reading
        strm.avail_in := Strm_in_func (BackObj, PByte(strm.next_in));
        strm.next_out := @BackObj.Window;  // buffer
        strm.avail_out := 0;
        ZlibCheckInitInflateStream (strm, nil);  // returns stream type which we ignore
        strm.next_out := nil;
        strm.avail_out := 0;
        ZlibDCheck (inflateBackInit (strm, MAX_WBITS, @BackObj.Window[0]));    { V8.70 }
        try
            ZlibDCheck (inflateBack (strm, @Strm_in_func, BackObj, @Strm_Write, BackObj));
          //seek back when unused data
            FStream.Seek (-strm.avail_in, soCurrent);   { V8.67 was soFromCurrent }
          //now trailer can be checked
         finally
            ZlibDCheck (inflateBackEnd (strm));
         end;
    finally
        FreeMem (BackObj);
    end;
end ;      *)

// save next block of decompresed data, writing to stream and DocData event
procedure ZlibWrtFn(Sender: TObject; Buf: PByte; Size: Integer; var Cancel: Boolean);  { V8.70 }
begin
    (Sender as THttpCCodzlib).OutputWriteBuffer(Buf, Size);
end;

{ V8.70 rewrote to use higher level function }
procedure THttpCCodzlib.Complete;
begin
    FStream.Position := 0;
    ZlibDecompressStream2(FStream, Self, ZlibWrtFn);   { V8.70 }
end ;

// save received HTTP content to stream, may be raw or compressed
procedure THttpCCodzlib.WriteBuffer(Buffer: Pointer; Count: Integer);
begin
  FStream.WriteBuffer(Buffer^, Count);
end;

initialization
  THttpContCodHandler.RegisterContentCoding(1, THttpCCodzlib);

finalization
  THttpContCodHandler.UnregisterAuthenticateClass(THttpCCodzlib);

end.
