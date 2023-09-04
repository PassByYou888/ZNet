{ ****************************************************************************** }
{ * ZDB 2.0 file support                                                       * }
{ ****************************************************************************** }
unit Z.ZDB2.FileEncoder;

{$I Z.Define.inc}

interface

uses Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status, Z.MemoryStream, Z.ListEngine,
  Z.ZDB.ObjectData_LIB, Z.ZDB, Z.ZDB.ItemStream_LIB,
  Z.HashList.Templet, Z.DFE, Z.ZDB2, Z.IOThread, Z.Cipher;

type
  TZDB2_File_HndList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<Integer>;

  TZDB2_FI = class
  public
    FileName: U_String;
    FileMD5: TMD5;
    FimeTime: TDateTime;
    Size: Int64;
    Compressed: Int64;
    OwnerPath: U_String;
    HandleArray: TZDB2_File_HndList;

    constructor Create();
    destructor Destroy; override;
    procedure SaveToStream(stream: TMS64);
    procedure LoadFromStream(stream: TMS64);
  end;

  TZDB2_FI_Pool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<TZDB2_FI>;
  TZDB2_FI_Hash_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TPascalString_Big_Hash_Pair_Pool<TZDB2_FI>;

  TZDB2_FI_Hash = class(TZDB2_FI_Hash_Decl)
  public
    constructor Create;
    function Compare_Value(const Value_1, Value_2: TZDB2_FI): Boolean; override;
    procedure DoFree(var Key: TPascalString; var Value: TZDB2_FI); override;
  end;

  TZDB2_FI_Pool = class(TZDB2_FI_Pool_Decl)
  public
    AutoFree: Boolean;
    constructor Create;
    procedure DoFree(var Data: TZDB2_FI); override;
    function CompareData(const Data_1, Data_2: TZDB2_FI): Boolean; override;
    function FindFile(FileName: U_String): TZDB2_FI;
    function SearchFile(FileName, OwnerPath: U_String): TZDB2_FI_Pool;
    function Build_Hash_Pool(OwnerPath_: Boolean): TZDB2_FI_Hash;
  end;

  TZDB2_FE_IO = class(TIO_Thread_Data)
  public
    Source: TMS64;
    Dest: TMS64;
    CM: TSelectCompressionMethod;
    constructor Create; override;
    destructor Destroy; override;
    procedure Process; override;
  end;

  TOn_ZDB2_File_OnProgress = procedure(State_: SystemString; Total, Current1, Current2: Int64) of object;

  TZDB2_File_Encoder = class
  private
    FCore: TZDB2_Core_Space;
    FPlace: TZDB2_Space_Planner;
    FIO_Thread: TIO_Thread_Base;
    FEncoderFiles: TZDB2_FI_Pool;
    FMaxQueue: Integer;
    FProgressInfo: SystemString;
    FOnProgress: TOn_ZDB2_File_OnProgress;
    FAborted: Boolean;
    FFlushed: Boolean;
  public
    constructor Create(Cipher_: IZDB2_Cipher; ZDB2_Stream: TCore_Stream; ThNum_: Integer); overload;
    constructor CreateFile(Cipher_: IZDB2_Cipher; ZDB2_FileName: U_String; ThNum_: Integer); overload;
    constructor Create(ZDB2_Stream: TCore_Stream; ThNum_: Integer); overload;
    constructor CreateFile(ZDB2_FileName: U_String; ThNum_: Integer); overload;
    destructor Destroy; override;
    function EncodeFromStream(stream: TCore_Stream; chunkSize_: Int64; CM: TSelectCompressionMethod; BlockSize_: Word): TZDB2_FI;
    function EncodeFromFile(FileName, OwnerPath: U_String; chunkSize_: Int64; CM: TSelectCompressionMethod; BlockSize_: Word): TZDB2_FI;
    procedure EncodeFromDirectory(Directory_: U_String; IncludeSub: Boolean; OwnerPath_: U_String; chunkSize_: Int64; CM: TSelectCompressionMethod; BlockSize_: Word);
    function Flush: Int64;
    property MaxQueue: Integer read FMaxQueue write FMaxQueue;
    property OnProgress: TOn_ZDB2_File_OnProgress read FOnProgress write FOnProgress;
    property Aborted: Boolean read FAborted write FAborted;
    property Core: TZDB2_Core_Space read FCore;

    class procedure Test;
  end;

  TZDB2_FD_IO = class(TIO_Thread_Data)
  public
    Source: TMS64;
    Dest: TMS64;
    constructor Create; override;
    destructor Destroy; override;
    procedure Process; override;
  end;

  TZDB2_File_Decoder = class
  private
    FCore: TZDB2_Core_Space;
    FIO_Thread: TIO_Thread_Base;
    FDecoderFiles: TZDB2_FI_Pool;
    FDecoderFile_Hash: TZDB2_FI_Hash;
    FDecoderPath_Hash: TZDB2_FI_Hash;
    FMaxQueue: Integer;
    FFileLog: TPascalStringList;
    FProgressInfo: SystemString;
    FOnProgress: TOn_ZDB2_File_OnProgress;
    FAborted: Boolean;
  public
    class function Check(Cipher_: IZDB2_Cipher; ZDB2_Stream: TCore_Stream): Boolean; overload;
    class function CheckFile(Cipher_: IZDB2_Cipher; ZDB2_FileName: U_String): Boolean; overload;
    class function Check(ZDB2_Stream: TCore_Stream): Boolean; overload;
    class function CheckFile(ZDB2_FileName: U_String): Boolean; overload;
    constructor Create(Cipher_: IZDB2_Cipher; ZDB2_Stream: TCore_Stream; ThNum_: Integer); overload;
    constructor CreateFile(Cipher_: IZDB2_Cipher; ZDB2_FileName: U_String; ThNum_: Integer); overload;
    constructor Create(ZDB2_Stream: TCore_Stream; ThNum_: Integer); overload;
    constructor CreateFile(ZDB2_FileName: U_String; ThNum_: Integer); overload;
    destructor Destroy; override;
    function CheckFileInfo(FileInfo_: TZDB2_FI): Boolean;
    function DecodeToStream(source_: TZDB2_FI; Dest_: TCore_Stream): Boolean;
    function DecodeToDirectory(source_: TZDB2_FI; DestDirectory_: U_String; var dest_file: U_String): Boolean; overload;
    function DecodeToDirectory(source_: TZDB2_FI; DestDirectory_: U_String): Boolean; overload;
    property Files: TZDB2_FI_Pool read FDecoderFiles;
    property FileHash: TZDB2_FI_Hash read FDecoderFile_Hash;
    property PathHash: TZDB2_FI_Hash read FDecoderPath_Hash;
    property MaxQueue: Integer read FMaxQueue write FMaxQueue;
    property FileLog: TPascalStringList read FFileLog;
    property OnProgress: TOn_ZDB2_File_OnProgress read FOnProgress write FOnProgress;
    property Aborted: Boolean read FAborted write FAborted;
    property Core: TZDB2_Core_Space read FCore;

    class procedure Test;
  end;

implementation

constructor TZDB2_FI.Create;
begin
  inherited Create;
  FileName := '';
  FileMD5 := NullMD5;
  FimeTime := umlNow();
  OwnerPath := '';
  Size := 0;
  Compressed := 0;
  HandleArray := TZDB2_File_HndList.Create;
end;

destructor TZDB2_FI.Destroy;
begin
  FileName := '';
  OwnerPath := '';
  DisposeObject(HandleArray);
  inherited Destroy;
end;

procedure TZDB2_FI.SaveToStream(stream: TMS64);
var
  d: TDFE;
  i: Integer;
begin
  d := TDFE.Create;
  d.WriteString(FileName);
  d.WriteMD5(FileMD5);
  d.WriteDouble(FimeTime);
  d.WriteInt64(Size);
  d.WriteInt64(Compressed);
  d.WriteString(OwnerPath);
  with d.WriteArrayInteger do
    for i := 0 to HandleArray.Count - 1 do
        Add(HandleArray[i]);
  d.EncodeTo(stream, True, False);
  DisposeObject(d);
end;

procedure TZDB2_FI.LoadFromStream(stream: TMS64);
var
  d: TDFE;
  i: Integer;
begin
  d := TDFE.Create;
  d.DecodeFrom(stream, True);
  FileName := d.Reader.ReadString;
  FileMD5 := d.Reader.ReadMD5;
  FimeTime := d.Reader.ReadDouble;
  Size := d.Reader.ReadInt64;
  Compressed := d.Reader.ReadInt64;
  OwnerPath := d.Reader.ReadString;
  with d.Reader.ReadArrayInteger do
    for i := 0 to Count - 1 do
        HandleArray.Add(Buffer[i]);
  DisposeObject(d);
end;

constructor TZDB2_FI_Hash.Create;
begin
  inherited Create($FFFF, nil);
end;

function TZDB2_FI_Hash.Compare_Value(const Value_1, Value_2: TZDB2_FI): Boolean;
begin
  Result := Value_1 = Value_2;
end;

procedure TZDB2_FI_Hash.DoFree(var Key: TPascalString; var Value: TZDB2_FI);
begin
  Value := nil;
  inherited DoFree(Key, Value);
end;

constructor TZDB2_FI_Pool.Create;
begin
  inherited Create;
  AutoFree := True;
end;

procedure TZDB2_FI_Pool.DoFree(var Data: TZDB2_FI);
begin
  if AutoFree then
      DisposeObjectAndNil(Data)
  else
      Data := nil;
end;

function TZDB2_FI_Pool.CompareData(const Data_1, Data_2: TZDB2_FI): Boolean;
begin
  Result := Data_1 = Data_2;
end;

function TZDB2_FI_Pool.FindFile(FileName: U_String): TZDB2_FI;
var
  r_: TZDB2_FI_Pool_Decl.TInvert_Repeat___;
begin
  Result := nil;
  if num <= 0 then
      exit;
  r_ := Invert_Repeat_;
  repeat
    if FileName.Same(r_.Queue^.Data.FileName) then
        exit(r_.Queue^.Data);
  until not r_.Prev;
end;

function TZDB2_FI_Pool.SearchFile(FileName, OwnerPath: U_String): TZDB2_FI_Pool;
var
  r_: TZDB2_FI_Pool_Decl.TInvert_Repeat___;
begin
  Result := TZDB2_FI_Pool.Create;
  Result.AutoFree := False;
  if num <= 0 then
      exit;
  r_ := Invert_Repeat_;
  repeat
    if umlSearchMatch(FileName, r_.Queue^.Data.FileName) and umlSearchMatch(OwnerPath, r_.Queue^.Data.OwnerPath) then
        Result.Add(r_.Queue^.Data);
  until not r_.Prev;
end;

function TZDB2_FI_Pool.Build_Hash_Pool(OwnerPath_: Boolean): TZDB2_FI_Hash;
var
  r_: TZDB2_FI_Pool_Decl.TInvert_Repeat___;
begin
  Result := TZDB2_FI_Hash.Create;
  if num <= 0 then
      exit;
  r_ := Invert_Repeat_;
  repeat
    if OwnerPath_ then
        Result.Add(umlCombineUnixFileName(r_.Queue^.Data.OwnerPath, r_.Queue^.Data.FileName), r_.Queue^.Data, True)
    else
        Result.Add(r_.Queue^.Data.FileName, r_.Queue^.Data, True);
  until not r_.Prev;
end;

constructor TZDB2_FE_IO.Create;
begin
  inherited Create;
  Source := TMS64.Create;
  Dest := TMS64.Create;
  CM := TSelectCompressionMethod.scmZLIB;
end;

destructor TZDB2_FE_IO.Destroy;
begin
  DisposeObject(Source);
  DisposeObject(Dest);
  inherited Destroy;
end;

procedure TZDB2_FE_IO.Process;
begin
  Source.Position := 0;
  Dest.Clear;
  if Source.Size < 128 then
      CM := TSelectCompressionMethod.scmNone;
  SelectCompressStream(CM, Source, Dest);
end;

constructor TZDB2_File_Encoder.Create(Cipher_: IZDB2_Cipher; ZDB2_Stream: TCore_Stream; ThNum_: Integer);
var
  P: PIOHnd;
begin
  inherited Create;
  new(P);
  InitIOHnd(P^);
  if not umlFileCreateAsStream(ZDB2_Stream, P^) then
      RaiseInfo('create stream error.');
  FCore := TZDB2_Core_Space.Create(P);
  FCore.Cipher := Cipher_;
  FCore.AutoCloseIOHnd := True;
  FCore.AutoFreeIOHnd := True;
  FPlace := TZDB2_Space_Planner.Create(FCore);

  if ThNum_ > 0 then
      FIO_Thread := TIO_Thread.Create(ThNum_)
  else
      FIO_Thread := TIO_Direct.Create;
  FEncoderFiles := TZDB2_FI_Pool.Create;
  FMaxQueue := umlMax(1, ThNum_) * 5;
  FProgressInfo := '';
  FOnProgress := nil;
  FAborted := False;
  FFlushed := False;
end;

constructor TZDB2_File_Encoder.CreateFile(Cipher_: IZDB2_Cipher; ZDB2_FileName: U_String; ThNum_: Integer);
var
  fs: TCore_FileStream;
begin
  fs := TCore_FileStream.Create(ZDB2_FileName, fmCreate);
  Create(Cipher_, fs, ThNum_);
  FCore.Space_IOHnd^.AutoFree := True;
end;

constructor TZDB2_File_Encoder.Create(ZDB2_Stream: TCore_Stream; ThNum_: Integer);
begin
  Create(nil, ZDB2_Stream, ThNum_);
end;

constructor TZDB2_File_Encoder.CreateFile(ZDB2_FileName: U_String; ThNum_: Integer);
begin
  CreateFile(nil, ZDB2_FileName, ThNum_);
end;

destructor TZDB2_File_Encoder.Destroy;
begin
  if not FFlushed then
      Flush;
  DisposeObject(FIO_Thread);
  DisposeObject(FEncoderFiles);
  DisposeObject(FPlace);
  DisposeObject(FCore);
  inherited Destroy;
end;

function TZDB2_File_Encoder.EncodeFromStream(stream: TCore_Stream; chunkSize_: Int64; CM: TSelectCompressionMethod; BlockSize_: Word): TZDB2_FI;
var
  Activted: TAtomBool;

{$IFDEF FPC}
  procedure FPC_ThRun_;
  var
    Total_: Int64;
    thIOData_: TZDB2_FE_IO;
  begin
    Total_ := stream.Size;
    stream.Position := 0;

    while (Total_ > 0) and (not FAborted) do
      begin
        thIOData_ := TZDB2_FE_IO.Create;
        thIOData_.CM := CM;
        if Total_ > chunkSize_ then
          begin
            thIOData_.Source.Size := chunkSize_;
            dec(Total_, chunkSize_);
          end
        else
          begin
            thIOData_.Source.Size := Total_;
            Total_ := 0;
          end;
        if stream.Read(thIOData_.Source.Memory^, thIOData_.Source.Size) <> thIOData_.Source.Size then
            break;
        FIO_Thread.Enqueue(thIOData_);
        while FIO_Thread.Count > FMaxQueue do
            TCompute.Sleep(1);
      end;

    FIO_Thread.Wait();
    Activted.V := False;
  end;
{$ENDIF FPC}


var
  FileInfo: TZDB2_FI;
  ioData: TZDB2_FE_IO;
  id: Integer;
  CompleteSize_: Int64;
begin
  if FFlushed then
      RaiseInfo('only work before flash');

  FileInfo := TZDB2_FI.Create;
  FileInfo.Size := stream.Size;
  FileInfo.Compressed := 0;
  FileInfo.FileMD5 := umlStreamMD5(stream);

  if FileInfo.Size = 0 then
    begin
      FEncoderFiles.Add(FileInfo);
      Result := FileInfo;
      exit;
    end;

  Activted := TAtomBool.Create(True);
  CompleteSize_ := 0;

{$IFDEF FPC}
  TCompute.RunP_NP(@FPC_ThRun_);
{$ELSE FPC}
  TCompute.RunP_NP(procedure
    var
      Total_: Int64;
      thIOData_: TZDB2_FE_IO;
    begin
      Total_ := stream.Size;
      stream.Position := 0;

      while (Total_ > 0) and (not FAborted) do
        begin
          thIOData_ := TZDB2_FE_IO.Create;
          thIOData_.CM := CM;
          if Total_ > chunkSize_ then
            begin
              thIOData_.Source.Size := chunkSize_;
              dec(Total_, chunkSize_);
            end
          else
            begin
              thIOData_.Source.Size := Total_;
              Total_ := 0;
            end;
          if stream.Read(thIOData_.Source.Memory^, thIOData_.Source.Size) <> thIOData_.Source.Size then
              break;
          FIO_Thread.Enqueue(thIOData_);
          while FIO_Thread.Count > FMaxQueue do
              TCompute.Sleep(1);
        end;

      FIO_Thread.Wait();
      Activted.V := False;
    end);
{$ENDIF FPC}
  CompleteSize_ := 0;
  while Activted.V do
    begin
      ioData := TZDB2_FE_IO(FIO_Thread.Dequeue);
      if ioData <> nil then
        begin
          inc(CompleteSize_, ioData.Source.Size);
          inc(FileInfo.Compressed, ioData.Dest.Size);
          if FPlace.WriteStream(ioData.Dest, BlockSize_, id) then
              FileInfo.HandleArray.Add(id)
          else
              DoStatus('TZDB2_File_Encoder error.');
          DisposeObject(ioData);
          if Assigned(FOnProgress) then
              FOnProgress(FProgressInfo + PFormat('(%s/%s compress:%s)',
              [umlSizeToStr(FileInfo.Size).Text, umlSizeToStr(CompleteSize_).Text, umlSizeToStr(FileInfo.Compressed).Text]),
              FileInfo.Size, CompleteSize_, FileInfo.Compressed);
        end
      else
          TCompute.Sleep(1);
    end;

  DisposeObject(Activted);
  FileInfo.FileName := umlMD5ToStr(FileInfo.FileMD5);
  FileInfo.FimeTime := umlNow();
  FileInfo.OwnerPath := '/';
  FEncoderFiles.Add(FileInfo);
  Result := FileInfo;
end;

function TZDB2_File_Encoder.EncodeFromFile(FileName, OwnerPath: U_String; chunkSize_: Int64; CM: TSelectCompressionMethod; BlockSize_: Word): TZDB2_FI;
var
  fs: TCore_FileStream;
  prefix: SystemString;
begin
  Result := nil;
  if not umlFileExists(FileName) then
      exit;
  FProgressInfo := umlCombineFileName(OwnerPath, umlGetFileName(FileName));
  fs := TCore_FileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  Result := EncodeFromStream(fs, chunkSize_, CM, BlockSize_);
  DisposeObject(fs);
  Result.FileName := umlGetFileName(FileName);
  Result.OwnerPath := OwnerPath;
  Result.FimeTime := umlGetFileTime(FileName);

  if (FCore.Space_IOHnd^.Handle is TCore_FileStream) then
      prefix := umlGetFileName(TCore_FileStream(FCore.Space_IOHnd^.Handle).FileName)
  else if (FCore.Space_IOHnd^.Handle is TReliableFileStream) then
      prefix := umlGetFileName(TReliableFileStream(FCore.Space_IOHnd^.Handle).FileName)
  else
      prefix := 'encode';

  DoStatus('%s %s %s->%s ratio:%d%%',
    [
    prefix,
    FProgressInfo,
    umlSizeToStr(Result.Size).Text,
    umlSizeToStr(Result.Compressed).Text,
    100 - umlPercentageToInt64(Result.Size, Result.Compressed)]);
end;

procedure TZDB2_File_Encoder.EncodeFromDirectory(Directory_: U_String; IncludeSub: Boolean; OwnerPath_: U_String; chunkSize_: Int64; CM: TSelectCompressionMethod; BlockSize_: Word);
var
  fAry: U_StringArray;
  n: SystemString;
begin
  fAry := umlGet_File_Full_Array(Directory_);
  for n in fAry do
    if not FAborted then
        EncodeFromFile(n, OwnerPath_, chunkSize_, CM, BlockSize_)
    else
        exit;

  if IncludeSub then
    begin
      fAry := umlGet_Path_Full_Array(Directory_);
      for n in fAry do
        if not FAborted then
            EncodeFromDirectory(n, IncludeSub, umlCombineWinPath(OwnerPath_, umlGetLastStr(n, '\/')), chunkSize_, CM, BlockSize_)
        else
            exit;
    end;
end;

function TZDB2_File_Encoder.Flush: Int64;
var
  d: TDFE;
  m64: TMS64;
  FileInfo_ID: Integer;
begin
  if FFlushed then
      RaiseInfo('repeat flash');

  Result := 0;
  d := TDFE.Create;
  if FEncoderFiles.num > 0 then
    with FEncoderFiles.repeat_ do
      repeat
        m64 := TMS64.Create;
        Queue^.Data.SaveToStream(m64);
        inc(Result, Queue^.Data.Size);
        d.WriteStream(m64);
        DisposeObject(m64);
      until not Next;
  m64 := TMS64.Create;
  d.EncodeAsZLib(m64, False);
  if not FPlace.WriteStream(m64, 1024, FileInfo_ID) then
      RaiseInfo('flush error.');
  DisposeObject(m64);
  DisposeObject(d);
  FPlace.Flush;
  PInteger(@FCore.UserCustomHeader^[$F0])^ := FileInfo_ID;
  FCore.Flush;
  FEncoderFiles.Clear;
  FFlushed := True;
end;

class procedure TZDB2_File_Encoder.Test;
var
  zdb_stream: TMS64;
  en: TZDB2_File_Encoder;
  tmp: TMS64;
  i: Integer;
begin
  zdb_stream := TMS64.CustomCreate(1024 * 1024 * 2);
  en := TZDB2_File_Encoder.Create(zdb_stream, 8);

  for i := 0 to 10 do
    begin
      tmp := TMS64.Create;
      tmp.Size := umlRandomRange(16 * 1024 * 1024, 4 * 1024 * 1024);
      MT19937Rand32(MaxInt, tmp.Memory, tmp.Size div 4);
      en.EncodeFromStream(tmp, 512 * 1024, TSelectCompressionMethod.scmZLIB, 8192);
      DisposeObject(tmp);
    end;
  en.Flush;

  DisposeObject(en);
  DisposeObject(zdb_stream);
end;

constructor TZDB2_FD_IO.Create;
begin
  inherited Create;
  Source := TMS64.Create;
  Dest := TMS64.Create;
end;

destructor TZDB2_FD_IO.Destroy;
begin
  DisposeObject(Source);
  DisposeObject(Dest);
  inherited Destroy;
end;

procedure TZDB2_FD_IO.Process;
begin
  SelectDecompressStream(Source, Dest);
end;

class function TZDB2_File_Decoder.Check(Cipher_: IZDB2_Cipher; ZDB2_Stream: TCore_Stream): Boolean;
var
  ioHnd: TIOHnd;
  tmp: TZDB2_Core_Space;
  id: Integer;
  mem: TZDB2_Mem;
  d: TDFE;
begin
  Result := False;
  InitIOHnd(ioHnd);
  if umlFileOpenAsStream('', ZDB2_Stream, ioHnd, True) then
    begin
      tmp := TZDB2_Core_Space.Create(@ioHnd);
      tmp.Cipher := Cipher_;
      if tmp.Open then
        begin
          id := PInteger(@tmp.UserCustomHeader^[$F0])^;
          if tmp.Check(id) then
            begin
              mem := TZDB2_Mem.Create.Create;
              if tmp.ReadData(mem, id) then
                begin
                  d := TDFE.Create;
                  try
                    d.LoadFromStream(mem.Stream64);
                    Result := d.Count >= 0;
                  except
                      Result := False;
                  end;
                  DisposeObject(d);
                end;
              DisposeObject(mem);
            end;
        end;
      DisposeObject(tmp);
    end;
end;

class function TZDB2_File_Decoder.CheckFile(Cipher_: IZDB2_Cipher; ZDB2_FileName: U_String): Boolean;
var
  fs: TCore_FileStream;
begin
  fs := TCore_FileStream.Create(ZDB2_FileName, fmOpenRead or fmShareDenyNone);
  try
      Result := Check(Cipher_, fs);
  finally
      DisposeObject(fs);
  end;
end;

class function TZDB2_File_Decoder.Check(ZDB2_Stream: TCore_Stream): Boolean;
begin
  Result := TZDB2_File_Decoder.Check(nil, ZDB2_Stream);
end;

class function TZDB2_File_Decoder.CheckFile(ZDB2_FileName: U_String): Boolean;
begin
  Result := TZDB2_File_Decoder.CheckFile(nil, ZDB2_FileName);
end;

constructor TZDB2_File_Decoder.Create(Cipher_: IZDB2_Cipher; ZDB2_Stream: TCore_Stream; ThNum_: Integer);
var
  P: PIOHnd;
  mem: TZDB2_Mem;
  d: TDFE;
  i: Integer;
  m64: TMS64;
  fi: TZDB2_FI;
begin
  inherited Create;
  new(P);
  InitIOHnd(P^);
  if not umlFileOpenAsStream('', ZDB2_Stream, P^, True) then
      RaiseInfo('create stream error.');
  FCore := TZDB2_Core_Space.Create(P);
  FCore.Cipher := Cipher_;
  FCore.AutoCloseIOHnd := True;
  FCore.AutoFreeIOHnd := True;
  FCore.Open;

  if ThNum_ > 0 then
      FIO_Thread := TIO_Thread.Create(ThNum_)
  else
      FIO_Thread := TIO_Direct.Create;

  FMaxQueue := umlMax(1, ThNum_) * 10;
  FDecoderFiles := TZDB2_FI_Pool.Create;

  mem := TZDB2_Mem.Create.Create;
  if FCore.ReadData(mem, PInteger(@FCore.UserCustomHeader^[$F0])^) then
    begin
      d := TDFE.Create;
      d.LoadFromStream(mem.Stream64);
      while d.Reader.NotEnd do
        begin
          m64 := TMS64.Create;
          d.Reader.ReadStream(m64);
          m64.Position := 0;
          fi := TZDB2_FI.Create;
          fi.LoadFromStream(m64);
          FDecoderFiles.Add(fi);
          DisposeObject(m64);
        end;
      DisposeObject(d);
    end;
  DisposeObject(mem);

  FDecoderFile_Hash := FDecoderFiles.Build_Hash_Pool(False);
  FDecoderPath_Hash := FDecoderFiles.Build_Hash_Pool(True);

  FFileLog := TPascalStringList.Create;
  FProgressInfo := '';
  FOnProgress := nil;
  FAborted := False;
end;

constructor TZDB2_File_Decoder.CreateFile(Cipher_: IZDB2_Cipher; ZDB2_FileName: U_String; ThNum_: Integer);
var
  fs: TCore_FileStream;
begin
  fs := TCore_FileStream.Create(ZDB2_FileName, fmOpenRead or fmShareDenyNone);
  Create(Cipher_, fs, ThNum_);
  FCore.Space_IOHnd^.AutoFree := True;
end;

constructor TZDB2_File_Decoder.Create(ZDB2_Stream: TCore_Stream; ThNum_: Integer);
begin
  Create(nil, ZDB2_Stream, ThNum_);
end;

constructor TZDB2_File_Decoder.CreateFile(ZDB2_FileName: U_String; ThNum_: Integer);
begin
  CreateFile(nil, ZDB2_FileName, ThNum_);
end;

destructor TZDB2_File_Decoder.Destroy;
begin
  DisposeObject(FIO_Thread);
  DisposeObject(FDecoderFile_Hash);
  DisposeObject(FDecoderPath_Hash);
  DisposeObject(FDecoderFiles);
  DisposeObject(FFileLog);
  DisposeObject(FCore);
  inherited Destroy;
end;

function TZDB2_File_Decoder.CheckFileInfo(FileInfo_: TZDB2_FI): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FileInfo_.HandleArray.Count - 1 do
      Result := Result and FCore.Check(FileInfo_.HandleArray[i]);
end;

function TZDB2_File_Decoder.DecodeToStream(source_: TZDB2_FI; Dest_: TCore_Stream): Boolean;
var
  Activted: TAtomBool;

{$IFDEF FPC}
  procedure FPC_ThRun_;
  var
    i: Integer;
    thIOData_: TZDB2_FD_IO;
  begin
    for i := 0 to source_.HandleArray.Count - 1 do
      begin
        thIOData_ := TZDB2_FD_IO.Create;
        FCore.ReadStream(thIOData_.Source, source_.HandleArray[i]);
        thIOData_.Source.Position := 0;
        FIO_Thread.Enqueue(thIOData_);
        while FIO_Thread.Count > FMaxQueue do
            TCompute.Sleep(1);
        if FAborted then
            break;
      end;

    FIO_Thread.Wait();
    Activted.V := False;
  end;
{$ENDIF FPC}


var
  compSiz_: Int64;
  ioData: TZDB2_FD_IO;
begin
  Result := False;
  if source_ = nil then
      exit;
  Activted := TAtomBool.Create(True);

{$IFDEF FPC}
  TCompute.RunP_NP(@FPC_ThRun_);
{$ELSE FPC}
  TCompute.RunP_NP(procedure
    var
      i: Integer;
      thIOData_: TZDB2_FD_IO;
    begin
      for i := 0 to source_.HandleArray.Count - 1 do
        begin
          thIOData_ := TZDB2_FD_IO.Create;
          FCore.ReadStream(thIOData_.Source, source_.HandleArray[i]);
          thIOData_.Source.Position := 0;
          FIO_Thread.Enqueue(thIOData_);
          while FIO_Thread.Count > FMaxQueue do
              TCompute.Sleep(1);
          if FAborted then
              break;
        end;

      FIO_Thread.Wait();
      Activted.V := False;
    end);
{$ENDIF FPC}
  compSiz_ := 0;
  while Activted.V do
    begin
      ioData := TZDB2_FD_IO(FIO_Thread.Dequeue);
      if ioData <> nil then
        begin
          inc(compSiz_, ioData.Source.Size);
          Dest_.Write(ioData.Dest.Memory^, ioData.Dest.Size);
          DisposeObject(ioData);

          if Assigned(FOnProgress) then
              FOnProgress(FProgressInfo + PFormat(' %s -> %s',
              [umlSizeToStr(Dest_.Size).Text, umlSizeToStr(source_.Size).Text]), source_.Size, Dest_.Size, compSiz_);
        end
      else
          TCompute.Sleep(1);
    end;

  DisposeObject(Activted);
  Result := True;
end;

function TZDB2_File_Decoder.DecodeToDirectory(source_: TZDB2_FI; DestDirectory_: U_String): Boolean;
var
  dest_file: U_String;
begin
  Result := DecodeToDirectory(source_, DestDirectory_, dest_file);
end;

function TZDB2_File_Decoder.DecodeToDirectory(source_: TZDB2_FI; DestDirectory_: U_String; var dest_file: U_String): Boolean;
var
  path_, fn: U_String;
  fs: TCore_FileStream;
begin
  Result := False;
  dest_file := '';
  if source_ = nil then
      exit;
  if source_.FileName.L = 0 then
      exit;
  if not CheckFileInfo(source_) then
    begin
      DoStatus('ZDB2 data error: %s', [source_.FileName.Text]);
      exit;
    end;
  path_ := umlCombinePath(DestDirectory_, source_.OwnerPath);
  if not umlDirectoryExists(path_) then
    begin
      umlCreateDirectory(path_);
      if not umlDirectoryExists(path_) then
        begin
          DoStatus('illegal directory %s', [path_.Text]);
          exit;
        end;
    end;
  fn := umlCombineFileName(path_, source_.FileName);

  try
    fs := TCore_FileStream.Create(fn, fmCreate);
    try
      FProgressInfo := umlGetFileName(fn);
      Result := DecodeToStream(source_, fs);
    finally
      DisposeObject(fs);
      umlSetFileTime(fn, source_.FimeTime);
      DoStatus('decode %s %s -> %s ratio:%d%%',
        [FProgressInfo, umlSizeToStr(source_.Compressed).Text, umlSizeToStr(source_.Size).Text, 100 - umlPercentageToInt64(source_.Size, source_.Compressed)]);
      FFileLog.Add(fn);
      dest_file := fn;
    end;
  except
    DoStatus('illegal file %s', [fn.Text]);
    exit;
  end;
end;

class procedure TZDB2_File_Decoder.Test;
var
  Cipher_: TZDB2_Cipher;
  zdb_stream: TMS64;
  en: TZDB2_File_Encoder;
  de: TZDB2_File_Decoder;
  tmp: TMS64;
  i: Integer;
  fi: TZDB2_FI;
begin
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csRC6, 'hello world.', 1, False, True);
  zdb_stream := TMS64.CustomCreate(1024 * 1024 * 8);

  en := TZDB2_File_Encoder.Create(Cipher_, zdb_stream, 4);
  for i := 0 to 10 do
    begin
      tmp := TMS64.Create;
      tmp.Size := umlRandomRange(16 * 1024, 64 * 1024);
      MT19937Rand32(MaxInt, tmp.Memory, tmp.Size div 4);
      fi := en.EncodeFromStream(tmp, 8192, TSelectCompressionMethod.scmZLIB_Max, 1024);
      fi.FileName := umlIntToStr(i);
      fi.OwnerPath := umlIntToStr(i * i);
      DisposeObject(tmp);
    end;
  en.Flush;
  DisposeObject(en);

  if TZDB2_File_Decoder.Check(Cipher_, zdb_stream) then
      DoStatus('TZDB2_File_Decoder check ok.')
  else
      DoStatus('TZDB2_File_Decoder check error.');

  de := TZDB2_File_Decoder.Create(Cipher_, zdb_stream, 4);
  if de.Files.num > 0 then
    with de.Files.repeat_ do
      repeat
        tmp := TMS64.Create;
        fi := Queue^.Data;
        if de.DecodeToStream(fi, tmp) then
          begin
            if umlCompareMD5(umlStreamMD5(tmp), fi.FileMD5) then
                DoStatus('TZDB2_File_Decoder md5 ok.')
            else
                DoStatus('TZDB2_File_Decoder md5 error.');
          end
        else
            DoStatus('TZDB2_File_Decoder error.');
        DisposeObject(tmp);
      until not Next;
  DisposeObject(de);

  DisposeObject(zdb_stream);
  DisposeObject(Cipher_);
end;

initialization

finalization

end.
