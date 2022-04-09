{ ****************************************************************************** }
{ * item mapping as Stream                                                     * }
{ ****************************************************************************** }

unit Z.ZDB.ItemStream_LIB;

{$I Z.Define.inc}

interface

uses SysUtils, Z.Core, Classes, Z.UnicodeMixedLib, Z.ZDB.ObjectData_LIB, Z.ZDB, Z.MemoryStream,
  Z.PascalStrings, Z.UPascalStrings;

type
  TItemStream = class(TCore_Stream)
  private
    DB_Engine: TObjectDataManager;
    ItemHnd_Ptr: PItemHandle;
    AutoFreeHnd: Boolean;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(eng_: TObjectDataManager; DBPath, DBItem: SystemString); overload;
    constructor Create(eng_: TObjectDataManager; var ItemHnd: TItemHandle); overload;
    constructor Create(eng_: TObjectDataManager; const ItemHeaderPos: Int64); overload;
    destructor Destroy; override;

    procedure SaveToFile(fn: SystemString);
    procedure LoadFromFile(fn: SystemString);

    function Read64(var buffer; Count: Int64): Int64;
    function Read(var buffer; Count: longint): longint; override;

    function Write64(const buffer; Count: Int64): Int64;
    function Write(const buffer; Count: longint): longint; override;

    function Seek(Offset: longint; origin: Word): longint; overload; override;
    function Seek(const Offset: Int64; origin: TSeekOrigin): Int64; overload; override;

    function CopyFrom64(const Source: TCore_Stream; Count: Int64): Int64;

    procedure SeekStart;
    procedure SeekLast;
    function UpdateHandle: Boolean;
    function CloseHandle: Boolean;
    property Hnd: PItemHandle read ItemHnd_Ptr;
  end;

  TMS64_Helper__ = class helper for TMS64
  public
    procedure LoadFrom_ZDB_File(eng_: TObjectDataManager; FileName: SystemString);
    procedure SaveTo_ZDB_File(eng_: TObjectDataManager; FileName: SystemString);
  end;

  TMem64_Helper__ = class helper for TMem64
  public
    procedure LoadFrom_ZDB_File(eng_: TObjectDataManager; FileName: SystemString);
    procedure SaveTo_ZDB_File(eng_: TObjectDataManager; FileName: SystemString);
  end;

implementation

function TItemStream.GetSize: Int64;
begin
  Result := DB_Engine.ItemGetSize(ItemHnd_Ptr^);
end;

constructor TItemStream.Create(eng_: TObjectDataManager; DBPath, DBItem: SystemString);
begin
  inherited Create;
  DB_Engine := eng_;
  New(ItemHnd_Ptr);
  eng_.ItemAutoOpenOrCreate(DBPath, DBItem, DBItem, ItemHnd_Ptr^);
  AutoFreeHnd := True;
end;

constructor TItemStream.Create(eng_: TObjectDataManager; var ItemHnd: TItemHandle);
begin
  inherited Create;
  DB_Engine := eng_;
  ItemHnd_Ptr := @ItemHnd;
  AutoFreeHnd := False;
end;

constructor TItemStream.Create(eng_: TObjectDataManager; const ItemHeaderPos: Int64);
begin
  inherited Create;
  DB_Engine := eng_;
  New(ItemHnd_Ptr);
  eng_.ItemFastOpen(ItemHeaderPos, ItemHnd_Ptr^);
  AutoFreeHnd := True;
end;

destructor TItemStream.Destroy;
begin
  UpdateHandle();
  if AutoFreeHnd then
    begin
      Dispose(ItemHnd_Ptr);
      ItemHnd_Ptr := nil;
    end;
  inherited Destroy;
end;

procedure TItemStream.SaveToFile(fn: SystemString);
var
  stream: TCore_Stream;
begin
  stream := TCore_FileStream.Create(fn, fmCreate);
  try
      stream.CopyFrom(Self, Size);
  finally
      DisposeObject(stream);
  end;
end;

procedure TItemStream.LoadFromFile(fn: SystemString);
var
  stream: TCore_Stream;
begin
  stream := TCore_FileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
      CopyFrom(stream, stream.Size);
  finally
      DisposeObject(stream);
  end;
end;

function TItemStream.Read64(var buffer; Count: Int64): Int64;
var
  Pos_: Int64;
  Size_: Int64;
begin
  Result := 0;
  if (Count > 0) then
    begin
      Pos_ := DB_Engine.ItemGetPos(ItemHnd_Ptr^);
      Size_ := DB_Engine.ItemGetSize(ItemHnd_Ptr^);
      if Pos_ + Count <= Size_ then
        begin
          if DB_Engine.ItemRead(ItemHnd_Ptr^, Count, PByte(@buffer)^) then
              Result := Count;
        end
      else if DB_Engine.ItemRead(ItemHnd_Ptr^, Size_ - Pos_, PByte(@buffer)^) then
          Result := Size_ - Pos_;
    end;
end;

function TItemStream.Read(var buffer; Count: longint): longint;
begin
  Result := Read64(buffer, Count);
end;

function TItemStream.Write64(const buffer; Count: Int64): Int64;
begin
  Result := Count;
  if (Count > 0) then
    if not DB_Engine.ItemWrite(ItemHnd_Ptr^, Count, PByte(@buffer)^) then
      begin
        Result := 0;
      end;
end;

function TItemStream.Write(const buffer; Count: longint): longint;
begin
  Result := Write64(buffer, Count);
end;

function TItemStream.Seek(Offset: longint; origin: Word): longint;
begin
  case origin of
    soFromBeginning:
      begin
        DB_Engine.ItemSeek(ItemHnd_Ptr^, Offset);
      end;
    soFromCurrent:
      begin
        if Offset <> 0 then
            DB_Engine.ItemSeek(ItemHnd_Ptr^, DB_Engine.ItemGetPos(ItemHnd_Ptr^) + Offset);
      end;
    soFromEnd:
      begin
        DB_Engine.ItemSeek(ItemHnd_Ptr^, DB_Engine.ItemGetSize(ItemHnd_Ptr^) + Offset);
      end;
  end;
  Result := DB_Engine.ItemGetPos(ItemHnd_Ptr^);
end;

function TItemStream.Seek(const Offset: Int64; origin: TSeekOrigin): Int64;
begin
  case origin of
    TSeekOrigin.soBeginning:
      begin
        DB_Engine.ItemSeek(ItemHnd_Ptr^, Offset);
      end;
    TSeekOrigin.soCurrent:
      begin
        if Offset <> 0 then
            DB_Engine.ItemSeek(ItemHnd_Ptr^, DB_Engine.ItemGetPos(ItemHnd_Ptr^) + Offset);
      end;
    TSeekOrigin.soEnd:
      begin
        DB_Engine.ItemSeek(ItemHnd_Ptr^, DB_Engine.ItemGetSize(ItemHnd_Ptr^) + Offset);
      end;
  end;
  Result := DB_Engine.ItemGetPos(ItemHnd_Ptr^);
end;

function TItemStream.CopyFrom64(const Source: TCore_Stream; Count: Int64): Int64;
const
  MaxBufSize = $F000;
var
  BufSize, N: Int64;
  buffer: Pointer;
begin
  if Count <= 0 then
    begin
      Source.Position := 0;
      Count := Source.Size;
    end;

  if Source is TMS64 then
    begin
      Result := Write64(TMS64(Source).Memory^, Count);
      exit;
    end;

  Result := Count;
  if Count > MaxBufSize then
      BufSize := MaxBufSize
  else
      BufSize := Count;
  buffer := System.GetMemory(BufSize);
  try
    while Count <> 0 do
      begin
        if Count > BufSize then
            N := BufSize
        else
            N := Count;
        if Source.Read(buffer^, N) <> N then
            RaiseInfo('item read error.');
        if Write64(buffer^, N) <> N then
            RaiseInfo('item write error.');
        Dec(Count, N);
      end;
  finally
      System.FreeMemory(buffer);
  end;
end;

procedure TItemStream.SeekStart;
begin
  DB_Engine.ItemSeekStart(ItemHnd_Ptr^);
end;

procedure TItemStream.SeekLast;
begin
  DB_Engine.ItemSeekLast(ItemHnd_Ptr^);
end;

function TItemStream.UpdateHandle: Boolean;
begin
  if DB_Engine.IsOnlyRead then
      Result := False
  else
      Result := DB_Engine.ItemUpdate(ItemHnd_Ptr^);
end;

function TItemStream.CloseHandle: Boolean;
begin
  Result := DB_Engine.ItemClose(ItemHnd_Ptr^);
end;

procedure TMS64_Helper__.LoadFrom_ZDB_File(eng_: TObjectDataManager; FileName: SystemString);
var
  field_path_, item_name_: U_String;
  Hnd: TItemHandle;
begin
  clear;
  field_path_ := umlGetUnixFilePath(FileName);
  item_name_ := umlGetUnixFileName(FileName);

  if eng_.ItemOpen(field_path_, item_name_, Hnd) then
    begin
      eng_.ItemReadToStream(Hnd, Self);
      eng_.ItemClose(Hnd);
    end
  else
      RaiseInfo('no found %s', [FileName]);
end;

procedure TMS64_Helper__.SaveTo_ZDB_File(eng_: TObjectDataManager; FileName: SystemString);
var
  field_path_, item_name_: U_String;
begin
  field_path_ := umlGetUnixFilePath(FileName);
  item_name_ := umlGetUnixFileName(FileName);
  eng_.ItemWriteFromStream(field_path_, item_name_, Self);
end;

procedure TMem64_Helper__.LoadFrom_ZDB_File(eng_: TObjectDataManager; FileName: SystemString);
var
  field_path_, item_name_: U_String;
  Hnd: TItemHandle;
begin
  field_path_ := umlGetUnixFilePath(FileName);
  item_name_ := umlGetUnixFileName(FileName);

  if eng_.ItemOpen(field_path_, item_name_, Hnd) then
    begin
      Size := Hnd.Item.Size;
      eng_.ItemRead(Hnd, Size, Memory^);
      eng_.ItemClose(Hnd);
    end
  else
      RaiseInfo('no found %s', [FileName]);
end;

procedure TMem64_Helper__.SaveTo_ZDB_File(eng_: TObjectDataManager; FileName: SystemString);
var
  field_path_, item_name_: U_String;
  Hnd: TItemHandle;
begin
  field_path_ := umlGetUnixFilePath(FileName);
  item_name_ := umlGetUnixFileName(FileName);
  eng_.ItemWriteFromStream(field_path_, item_name_, Self.Stream64);
end;

end.
