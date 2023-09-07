{ ****************************************************************************** }
{ * solved for discontinuous space.                                            * }
{ ****************************************************************************** }
unit Z.FragmentBuffer;

{$I Z.Define.inc}

interface

uses
{$IFDEF MSWINDOWS}
{$IFDEF FPC}
  Windows,
{$ELSE FPC}
  Winapi.Windows,
{$ENDIF FPC}
{$ENDIF MSWINDOWS}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.ListEngine, Z.MemoryStream;

type
  TPosition_Data = class;
  TFragment_Space_Tool = class;

  TPosition_Buffer_Tool_ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<TPosition_Data>;

  TPosition_Data = class
  public
    Owner_Buffer: TPosition_Buffer_Tool_.PQueueStruct;
    bPos: Int64;
    Mem: TMem64;
    Size: Int64;
    constructor Create;
    destructor Destroy; override;
    function ePos(): Int64;
    function IsOverlap(b, e: Int64): Boolean;
    procedure Copy_To_Ptr(b, buff_size: Int64; p: Pointer);
  end;

  TPosition_Buffer_Tool = class(TPosition_Buffer_Tool_)
  private
    function Do_Sort_bPos(var L, R: TPosition_Data): Integer;
  public
    Owner: TFragment_Space_Tool;
    constructor Create(Owner_: TFragment_Space_Tool);
    destructor Destroy; override;
    procedure Sort;
    procedure Extract_To_Mem(Mem: TMem64; Mem_Pos: Int64);
  end;

  TOn_Get_Fragment = procedure(Sender: TFragment_Space_Tool; Position_: Int64; buff_: Pointer; Size_: Int64; var successed: Boolean) of object;

  TFragment_Space_Stream_Head = packed record
    Edition: Word;
    MD5: TMD5;
    Num: Int64;
  end;

  TFragment_Space_Tool = class
  private
    FBuffer: TPosition_Buffer_Tool;
    FRead_Buffer_Cache: Boolean;
    FMin_Pos, FMax_Pos: Int64;
    FOn_Get_Fragment: TOn_Get_Fragment;
    procedure DoFree(var Data: TPosition_Data);
    function IsOverlap(b, e: Int64): Boolean;
    function Found_Overlap(bPos, Size: Int64): TPosition_Buffer_Tool;
    function Process_Overlap_Buffer(bPos: Int64; p: Pointer; Size: Int64): Boolean;
    function Get_Fragment(Position: Int64; buff: Pointer; Size: Int64): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Write_Buffer(bPos: Int64; p: Pointer; Size: Int64);
    property Read_Buffer_Cache: Boolean read FRead_Buffer_Cache write FRead_Buffer_Cache;
    function Read_Buffer(bPos: Int64; p: Pointer; Size: Int64): Boolean;
    procedure Save_To_Stream(stream: TCore_Stream);
    function Load_From_Stream(stream: TCore_Stream): Boolean;
    procedure Save_To_File(FileName_: TPascalString);
    function Load_From_File(FileName_: TPascalString): Boolean;
    procedure Sort;
    function Num: NativeInt;
    function Fragment_Memory: Int64;
    function Repeat_: TPosition_Buffer_Tool_.TRepeat___;
    property Buffer: TPosition_Buffer_Tool read FBuffer;
    property On_Get_Fragment: TOn_Get_Fragment read FOn_Get_Fragment write FOn_Get_Fragment;
  end;

  TFragment_Space_Tool_Test = class
  public
    tool: TFragment_Space_Tool;
    mirror: TMem64;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Get_Fragment(Sender: TFragment_Space_Tool; Position_: Int64; buff_: Pointer; Size_: Int64; var successed: Boolean);
    class procedure Test();
  end;

  TSafe_Flush_Stream = class(TCore_Stream)
  private
    FFileName: TPascalString;
    FIsNew: Boolean;
    FIsWrite: Boolean;
    Source_IO: TCore_Stream;
    FFragment_Space: TFragment_Space_Tool;
    FMax_Write_Memory: Int64;
    FMax_Write_Fragment: Int64;
    procedure Do_Get_Fragment(Sender: TFragment_Space_Tool; Position_: Int64; buff_: Pointer; Size_: Int64; var successed: Boolean);
    procedure SetSize(const NewSize: Int64); overload; override;
    procedure SetSize(NewSize: longint); overload; override;
    function Get_Flush_Temp_File_Name: TPascalString;
    function Get_Read_Buffer_Cache: Boolean;
    procedure Set_Read_Buffer_Cache(const Value: Boolean);
  public
    constructor Create(const FileName_: TPascalString; IsNew_, IsWrite_: Boolean); overload;
    constructor Create(const FileName_: TPascalString; IsNew_, IsWrite_, IsReliableFile_: Boolean); overload;
    destructor Destroy; override;
    property IsNew: Boolean read FIsNew;
    property IsWrite: Boolean read FIsWrite;
    function IsOnlyRead: Boolean;
    property FileName: TPascalString read FFileName;
    property Max_Write_Memory: Int64 read FMax_Write_Memory;
    property Max_Write_Fragment: Int64 read FMax_Write_Fragment;
    property Read_Buffer_Cache: Boolean read Get_Read_Buffer_Cache write Set_Read_Buffer_Cache;
    property Fragment_Space: TFragment_Space_Tool read FFragment_Space;

    function Write(const Buffer; Count: longint): longint; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Seek(const Offset: Int64; origin: TSeekOrigin): Int64; override;

    procedure Build_Flush_File;
    procedure Flush;

    class procedure Test();
  end;

implementation

uses Z.Geometry2D, Z.UnicodeMixedLib, Z.Status;

constructor TPosition_Data.Create;
begin
  inherited Create;
  Owner_Buffer := nil;
  bPos := 0;
  Mem := TMem64.CustomCreate(1024);
  Size := 0;
end;

destructor TPosition_Data.Destroy;
begin
  DisposeObjectAndNil(Mem);
  inherited Destroy;
end;

function TPosition_Data.ePos(): Int64;
begin
  Result := bPos + Size;
end;

function TPosition_Data.IsOverlap(b, e: Int64): Boolean;
begin
  Result :=
    umlInRange(bPos, b, e) or
    umlInRange(ePos, b, e) or
    umlInRange(b, bPos, ePos) or
    umlInRange(e, bPos, ePos);
end;

procedure TPosition_Data.Copy_To_Ptr(b, buff_size: Int64; p: Pointer);
begin
  if bPos >= b then
      CopyPtr(Mem.Memory, GetOffset(p, bPos - b), umlMin(buff_size - (bPos - b), Size))
  else
      CopyPtr(Mem.PosAsPtr(b - bPos), p, umlMin(buff_size, Size - (b - bPos)));
end;

function TPosition_Buffer_Tool.Do_Sort_bPos(var L, R: TPosition_Data): Integer;
begin
  Result := CompareInt64(L.bPos, R.bPos);
  if Result = 0 then
      Result := CompareInt64(L.Size, R.Size);
end;

constructor TPosition_Buffer_Tool.Create(Owner_: TFragment_Space_Tool);
begin
  inherited Create;
  Owner := Owner_;
end;

destructor TPosition_Buffer_Tool.Destroy;
begin
  inherited Destroy;
end;

procedure TPosition_Buffer_Tool.Sort;
begin
  Sort_M({$IFDEF FPC}@{$ENDIF FPC}Do_Sort_bPos);
end;

procedure TPosition_Buffer_Tool.Extract_To_Mem(Mem: TMem64; Mem_Pos: Int64);
var
  bPos, ePos: Int64;
begin
  if Num <= 0 then
      exit;
  bPos := First^.Data.bPos;
  ePos := Last^.Data.ePos;
  Mem.Size := Mem_Pos + (ePos - bPos);
  with Repeat_ do
    repeat
        CopyPtr(queue^.Data.Mem.Memory, Mem.PosAsPtr(Mem_Pos + (queue^.Data.bPos - bPos)), queue^.Data.Size);
    until not Next;
end;

procedure TFragment_Space_Tool.DoFree(var Data: TPosition_Data);
begin
  DisposeObjectAndNil(Data);
end;

function TFragment_Space_Tool.IsOverlap(b, e: Int64): Boolean;
begin
  Result :=
    umlInRange(FMin_Pos, b, e) or
    umlInRange(FMax_Pos, b, e) or
    umlInRange(b, FMin_Pos, FMax_Pos) or
    umlInRange(e, FMin_Pos, FMax_Pos);
end;

function TFragment_Space_Tool.Found_Overlap(bPos, Size: Int64): TPosition_Buffer_Tool;
begin
  Result := nil;

  if not IsOverlap(bPos, bPos + Size) then // optmized
      exit;

  if Num > 0 then
    with Repeat_ do
      repeat
        if queue^.Data.IsOverlap(bPos, bPos + Size) then
          begin
            if Result = nil then
                Result := TPosition_Buffer_Tool.Create(self);
            Result.Add(queue^.Data);
          end;
      until not Next;
  if Result <> nil then
      Result.Sort;
end;

function TFragment_Space_Tool.Process_Overlap_Buffer(bPos: Int64; p: Pointer; Size: Int64): Boolean;
var
  L: TPosition_Buffer_Tool;
  inst: TPosition_Data;
begin
  Result := False;
  L := Found_Overlap(bPos, Size);
  if L = nil then
      exit;
  // optimzie
  if (L.Num = 1) then
    begin
      inst := L.First^.Data;
      if (inst.bPos <= bPos) and (inst.ePos >= bPos + Size) then // overwrite
        begin
          CopyPtr(p, inst.Mem.PosAsPtr(bPos - inst.bPos), Size);
          DisposeObject(L);
          Result := True;
          exit;
        end;
      if (inst.ePos = bPos) then // append
        begin
          inst.Mem.Position := inst.Size;
          inst.Mem.WritePtr(p, Size);
          inst.Size := inst.Size + Size;
          DisposeObject(L);
          Result := True;
          exit;
        end;
    end;
  // compare overlap buffer
  inst := TPosition_Data.Create;
  if L.First^.Data.bPos < bPos then
    begin
      inst.bPos := L.First^.Data.bPos;
      L.Extract_To_Mem(inst.Mem, 0);
      inst.Mem.Position := bPos - L.First^.Data.bPos;
    end
  else
    begin
      inst.bPos := bPos;
      L.Extract_To_Mem(inst.Mem, L.First^.Data.bPos - bPos);
      inst.Mem.Position := 0;
    end;
  // overwrite overlap buffer
  inst.Mem.WritePtr(p, Size);
  inst.Size := inst.Mem.Size;
  // new queue
  inst.Owner_Buffer := FBuffer.Add(inst);
  if (FMin_Pos = 0) and (FMax_Pos = 0) then
    begin
      FMin_Pos := inst.bPos;
      FMax_Pos := inst.ePos;
    end
  else
    begin
      FMin_Pos := umlMin(FMin_Pos, inst.bPos);
      FMax_Pos := umlMax(FMax_Pos, inst.ePos);
    end;
  // recycle overlap buffer
  with L.Repeat_ do
    repeat
        FBuffer.Push_To_Recycle_Pool(queue^.Data.Owner_Buffer);
    until not Next;
  FBuffer.Free_Recycle_Pool;

  DisposeObject(L);
  Result := True;
end;

function TFragment_Space_Tool.Get_Fragment(Position: Int64; buff: Pointer; Size: Int64): Boolean;
begin
  Result := False;
  if Assigned(FOn_Get_Fragment) then
      FOn_Get_Fragment(self, Position, buff, Size, Result);
end;

constructor TFragment_Space_Tool.Create;
begin
  inherited Create;
  FBuffer := TPosition_Buffer_Tool.Create(self);
  FBuffer.OnFree := {$IFDEF FPC}@{$ENDIF FPC}DoFree;
  FRead_Buffer_Cache := False;
  FOn_Get_Fragment := nil;
  FMin_Pos := 0;
  FMax_Pos := 0;
end;

destructor TFragment_Space_Tool.Destroy;
begin
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TFragment_Space_Tool.Clear;
begin
  FBuffer.Clear;
  FMin_Pos := 0;
  FMax_Pos := 0;
end;

procedure TFragment_Space_Tool.Write_Buffer(bPos: Int64; p: Pointer; Size: Int64);
var
  inst: TPosition_Data;
begin
  if Process_Overlap_Buffer(bPos, p, Size) then
      exit;
  inst := TPosition_Data.Create;
  inst.bPos := bPos;
  inst.Mem.WritePtr(p, Size);
  inst.Size := Size;
  inst.Owner_Buffer := FBuffer.Add(inst);
  if (FMin_Pos = 0) and (FMax_Pos = 0) then
    begin
      FMin_Pos := inst.bPos;
      FMax_Pos := inst.ePos;
    end
  else
    begin
      FMin_Pos := umlMin(FMin_Pos, inst.bPos);
      FMax_Pos := umlMax(FMax_Pos, inst.ePos);
    end;
end;

function TFragment_Space_Tool.Read_Buffer(bPos: Int64; p: Pointer; Size: Int64): Boolean;
var
  L: TPosition_Buffer_Tool;
  inst: TPosition_Data;
begin
  Result := False;
  L := Found_Overlap(bPos, Size);
  if L = nil then
      exit(Get_Fragment(bPos, p, Size));
  // optimzie
  if L.Num = 1 then
    begin
      inst := L.First^.Data;
      if (inst.bPos <= bPos) and (inst.ePos >= bPos + Size) then
        begin
          CopyPtr(inst.Mem.PosAsPtr(bPos - inst.bPos), p, Size);
          DisposeObject(L);
          Result := True;
          exit;
        end;
    end;
  // request fragment
  if not Get_Fragment(bPos, p, Size) then
    begin
      DisposeObject(L);
      exit;
    end;
  // overwrite buffer
  with L.Repeat_ do
    repeat
        queue^.Data.Copy_To_Ptr(bPos, Size, p);
    until not Next;
  DisposeObject(L);
  Result := True;
  if FRead_Buffer_Cache then
      Write_Buffer(bPos, p, Size);
end;

procedure TFragment_Space_Tool.Save_To_Stream(stream: TCore_Stream);
var
  m64: TMem64;
  head: TFragment_Space_Stream_Head;
  inst: TPosition_Data;
begin
  m64 := TMem64.CustomCreate(SizeOf(TFragment_Space_Stream_Head) + (Num * 16) + Fragment_Memory);
  head.Edition := 1;
  head.MD5 := NULL_MD5;
  head.Num := Num;
  m64.Position := 0;
  m64.WritePtr(@head, SizeOf(TFragment_Space_Stream_Head));
  if Num > 0 then
    with Repeat_ do
      repeat
        inst := queue^.Data;
        m64.WriteInt64(inst.bPos);
        m64.WriteInt64(inst.Size);
        m64.WritePtr(inst.Mem.Memory, inst.Size);
      until not Next;
  head.MD5 := umlMD5(m64.PosAsPtr(SizeOf(TFragment_Space_Stream_Head)), m64.Size - SizeOf(TFragment_Space_Stream_Head));
  m64.Position := 0;
  m64.Write(head, SizeOf(TFragment_Space_Stream_Head));

  try
    stream.Position := 0;
    m64.SaveToStream(stream);
  except
  end;
  DisposeObject(m64);
end;

function TFragment_Space_Tool.Load_From_Stream(stream: TCore_Stream): Boolean;
var
  m64: TMem64;
  head: TFragment_Space_Stream_Head;
  MD5: TMD5;
  i: Int64;
  inst: TPosition_Data;
  n: TPascalString;
begin
  Result := False;
  Clear;
  m64 := TMem64.Create;
  m64.LoadFromStream(stream);
  m64.Position := 0;
  m64.ReadPtr(@head, SizeOf(TFragment_Space_Stream_Head));
  MD5 := umlMD5(m64.PosAsPtr(SizeOf(TFragment_Space_Stream_Head)), m64.Size - SizeOf(TFragment_Space_Stream_Head));

  if umlCompareMD5(MD5, head.MD5) then
    begin
      i := 0;
      while i < head.Num do
        begin
          inst := TPosition_Data.Create;
          inst.bPos := m64.ReadInt64;
          inst.Size := m64.ReadInt64;
          inst.Mem.Size := inst.Size;
          m64.ReadPtr(inst.Mem.Memory, inst.Size);
          inst.Owner_Buffer := FBuffer.Add(inst);
          if (FMin_Pos = 0) and (FMax_Pos = 0) then
            begin
              FMin_Pos := inst.bPos;
              FMax_Pos := inst.ePos;
            end
          else
            begin
              FMin_Pos := umlMin(FMin_Pos, inst.bPos);
              FMax_Pos := umlMax(FMax_Pos, inst.ePos);
            end;
          inc(i);
        end;
      Result := True;
    end
  else
    begin
      if stream is TCore_FileStream then
          n := TCore_FileStream(stream).FileName
      else if stream is TReliableFileStream then
          n := TReliableFileStream(stream).FileName
      else if stream is TSafe_Flush_Stream then
          n := TSafe_Flush_Stream(stream).FileName
      else
          n := '<Memory>';
      DoStatus('"%s" Data is corrupt.', [n.Text]);
    end;
  DisposeObject(m64);
end;

procedure TFragment_Space_Tool.Save_To_File(FileName_: TPascalString);
var
  stream: TCore_Stream;
begin
  stream := TCore_FileStream.Create(FileName_, fmCreate);
  try
      Save_To_Stream(stream);
  except
  end;
  DisposeObject(stream);
end;

function TFragment_Space_Tool.Load_From_File(FileName_: TPascalString): Boolean;
var
  stream: TCore_Stream;
begin
  Result := False;
  if not umlFileExists(FileName_) then
      exit;
  try
    stream := TCore_FileStream.Create(FileName_, fmOpenRead or fmShareDenyNone);
    Result := Load_From_Stream(stream);
    DisposeObject(stream);
  except
  end;
end;

procedure TFragment_Space_Tool.Sort;
begin
  FBuffer.Sort;
end;

function TFragment_Space_Tool.Num: NativeInt;
begin
  Result := FBuffer.Num;
end;

function TFragment_Space_Tool.Fragment_Memory: Int64;
begin
  Result := 0;
  if Num <= 0 then
      exit;
  with Repeat_ do
    repeat
        inc(Result, queue^.Data.Size);
    until not Next;
end;

function TFragment_Space_Tool.Repeat_: TPosition_Buffer_Tool_.TRepeat___;
begin
  Result := FBuffer.Repeat_;
end;

constructor TFragment_Space_Tool_Test.Create;
begin
  inherited Create;
  tool := TFragment_Space_Tool.Create;
  tool.On_Get_Fragment := {$IFDEF FPC}@{$ENDIF FPC}Do_Get_Fragment;
  mirror := TMem64.Create;
end;

destructor TFragment_Space_Tool_Test.Destroy;
begin
  DisposeObject(tool);
  DisposeObject(mirror);
  inherited Destroy;
end;

procedure TFragment_Space_Tool_Test.Do_Get_Fragment(Sender: TFragment_Space_Tool; Position_: Int64; buff_: Pointer; Size_: Int64; var successed: Boolean);
begin
  CopyPtr(mirror.PosAsPtr(Position_), buff_, Size_);
  successed := True;
end;

class procedure TFragment_Space_Tool_Test.Test;
var
  t_: TFragment_Space_Tool_Test;

  function DoCheck(): Boolean;
  var
    repeat___: TPosition_Buffer_Tool.TRepeat___;
  begin
    Result := True;
    repeat___ := t_.tool.Repeat_;
    repeat
      if not CompareMemory(repeat___.queue^.Data.Mem.Memory, t_.mirror.PosAsPtr(repeat___.queue^.Data.bPos), repeat___.queue^.Data.Size) then
        begin
          Result := False;
          break;
        end;
    until not repeat___.Next;
  end;

  procedure W(pos: Int64; p: Pointer; Size: Int64);
  begin
    t_.tool.Write_Buffer(pos, p, Size);
    t_.mirror.Position := pos;
    t_.mirror.WritePtr(p, Size);
    if not DoCheck() then
        DoStatus('error.');
  end;

  procedure R(pos: Int64; Size: Int64);
  var
    m1, m2: TMem64;
  begin
    m1 := TMem64.Create;
    m2 := TMem64.Create;
    m1.Size := Size;
    m2.Size := Size;

    t_.tool.Read_Buffer(pos, m1.Memory, Size);
    CopyPtr(t_.mirror.PosAsPtr(pos), m2.Memory, Size);

    if not CompareMemory(m1.Memory, m2.Memory, Size) then
        DoStatus('error.');

    DisposeObject([m1, m2]);
  end;

var
  buff: TBytes;
  i: Integer;
  tmp: TMS64;
begin
  t_ := TFragment_Space_Tool_Test.Create;
  setLength(buff, 500 * 1024);
  TMT19937.Rand32(MaxInt, @buff[0], length(buff) div 4);
  t_.mirror.WritePtr(@buff[0], length(buff));

  W(100, @buff[100], 100);
  W(200, @buff[300], 100);

  for i := 0 to 500 * 2 do
    begin
      W(umlRR(0, length(buff) - 400), @buff[umlRR(0, length(buff) - 400)], umlRR(4, 400));
    end;

  tmp := TMS64.Create;
  t_.tool.Save_To_Stream(tmp);
  t_.tool.Load_From_Stream(tmp);
  DisposeObject(tmp);

  for i := 0 to 100000 do
    begin
      R(umlRR(0, length(buff) - 100), umlRR(20, 100));
    end;

  DisposeObject(t_);
end;

procedure TSafe_Flush_Stream.Do_Get_Fragment(Sender: TFragment_Space_Tool; Position_: Int64; buff_: Pointer; Size_: Int64; var successed: Boolean);
var
  bak_pos: Int64;
begin
  bak_pos := Source_IO.Position;
  Source_IO.Position := Position_;
  successed := Source_IO.Read(buff_^, Size_) = Size_;
  Source_IO.Position := bak_pos;
end;

procedure TSafe_Flush_Stream.SetSize(const NewSize: Int64);
begin
  Source_IO.Size := NewSize;
end;

procedure TSafe_Flush_Stream.SetSize(NewSize: longint);
begin
  SetSize(Int64(NewSize));
end;

function TSafe_Flush_Stream.Get_Flush_Temp_File_Name: TPascalString;
begin
  Result := FFileName + '.Safe_Flush';
end;

function TSafe_Flush_Stream.Get_Read_Buffer_Cache: Boolean;
begin
  Result := FFragment_Space.Read_Buffer_Cache;
end;

procedure TSafe_Flush_Stream.Set_Read_Buffer_Cache(const Value: Boolean);
begin
  FFragment_Space.Read_Buffer_Cache := Value;
end;

constructor TSafe_Flush_Stream.Create(const FileName_: TPascalString; IsNew_, IsWrite_, IsReliableFile_: Boolean);
var
  mode_: Word;
  bak_pos: Int64;
begin
  inherited Create;
  FFileName := FileName_;
  FIsNew := IsNew_;
  FIsWrite := IsWrite_;

  if (not umlFileExists(FileName_)) and (IsWrite_) then
      FIsNew := True;

  if FIsNew then
      mode_ := fmCreate
  else if FIsWrite then
      mode_ := fmOpenReadWrite
  else
      mode_ := fmOpenRead or fmShareDenyNone;

  if IsReliableFile_ then
      Source_IO := TReliableFileStream.Create(FileName_, FIsNew, FIsWrite)
  else
      Source_IO := TCore_FileStream.Create(FileName_, mode_);

  FFragment_Space := TFragment_Space_Tool.Create;
  FFragment_Space.On_Get_Fragment := {$IFDEF FPC}@{$ENDIF FPC}Do_Get_Fragment;
  FMax_Write_Memory := 0;
  FMax_Write_Fragment := 0;

  if (not FIsNew) and (FIsWrite) and (umlFileExists(Get_Flush_Temp_File_Name)) then
    begin
      DoStatus('"%s" It has been abnormally closed and has now been restored.', [umlGetFileName(FFileName).Text]);
      if FFragment_Space.Load_From_File(Get_Flush_Temp_File_Name) then
        begin
          bak_pos := Source_IO.Position;
          if Source_IO.Size < FFragment_Space.FMax_Pos then
              Source_IO.Size := FFragment_Space.FMax_Pos;
          with FFragment_Space.Repeat_ do
            repeat
              Source_IO.Position := queue^.Data.bPos;
              Source_IO.Write(queue^.Data.Mem.Memory^, queue^.Data.Size);
            until not Next;
          Source_IO.Position := bak_pos;
          FFragment_Space.Clear;
        end;
    end;
end;

constructor TSafe_Flush_Stream.Create(const FileName_: TPascalString; IsNew_, IsWrite_: Boolean);
begin
  Create(FileName_, IsNew_, IsWrite_, False);
end;

destructor TSafe_Flush_Stream.Destroy;
begin
  Flush();
  umlDeleteFile(Get_Flush_Temp_File_Name);
  DisposeObject(FFragment_Space);
  DisposeObject(Source_IO);
  inherited Destroy;
end;

function TSafe_Flush_Stream.IsOnlyRead: Boolean;
begin
  Result := not FIsWrite;
end;

function TSafe_Flush_Stream.Write(const Buffer; Count: longint): longint;
var
  pos_: Int64;
begin
  if not FIsWrite then
      exit(0);
  pos_ := Source_IO.Position;
  FFragment_Space.Write_Buffer(pos_, @Buffer, Count);
  if Source_IO.Size < FFragment_Space.FMax_Pos then
      Source_IO.Size := FFragment_Space.FMax_Pos;
  if ((FMax_Write_Fragment > 0) and (FFragment_Space.Num > FMax_Write_Fragment)) or
    ((FMax_Write_Memory > 0) and (FFragment_Space.Fragment_Memory > FMax_Write_Memory)) then
      Flush();
  Source_IO.Position := pos_ + Count;
  Result := Count;
end;

function TSafe_Flush_Stream.Read(var Buffer; Count: longint): longint;
var
  pos_: Int64;
begin
  if FIsWrite then
      pos_ := Source_IO.Position;
  if FIsWrite and FFragment_Space.Read_Buffer(pos_, @Buffer, Count) then
    begin
      Source_IO.Position := pos_ + Count;
      Result := Count;
    end
  else
    begin
      Result := Source_IO.Read(Buffer, Count);
    end;
end;

function TSafe_Flush_Stream.Seek(const Offset: Int64; origin: TSeekOrigin): Int64;
begin
  Result := Source_IO.Seek(Offset, origin);
end;

procedure TSafe_Flush_Stream.Build_Flush_File;
begin
  FFragment_Space.Sort();
  FFragment_Space.Save_To_File(Get_Flush_Temp_File_Name);
end;

procedure TSafe_Flush_Stream.Flush;
var
  bak_pos: Int64;
begin
  if not FIsWrite then
      exit;
  if FFragment_Space.Num <= 0 then
      exit;

  try
    Build_Flush_File;

    bak_pos := Source_IO.Position;
    if Source_IO.Size < FFragment_Space.FMax_Pos then
        Source_IO.Size := FFragment_Space.FMax_Pos;
    with FFragment_Space.Repeat_ do
      repeat
        Source_IO.Position := queue^.Data.bPos;
        Source_IO.Write(queue^.Data.Mem.Memory^, queue^.Data.Size);
      until not Next;
    Source_IO.Position := bak_pos;
    FFragment_Space.Clear;
  except
  end;

{$IFDEF MSWINDOWS}
  if Source_IO is TCore_FileStream then
    begin
      FlushFileBuffers(TCore_FileStream(Source_IO).Handle);
    end;
{$ENDIF MSWINDOWS}
end;

class procedure TSafe_Flush_Stream.Test;
var
  mirror: TMS64;
  fs: TSafe_Flush_Stream;

  procedure W(p: Pointer; Size: Int64);
  begin
    mirror.Position := fs.Position;
    fs.Write(p^, Size);
    mirror.Write(p^, Size);
  end;

var
  buff: TBytes;
  i: Integer;
  tmp: TMS64;
begin
  fs := TSafe_Flush_Stream.Create(umlCombineFileName(umlCurrentPath, 'test.dat'), False, True);
  mirror := TMS64.CustomCreate(1024 * 1024);
  setLength(buff, 500 * 1024);
  TMT19937.Rand32(MaxInt, @buff[0], length(buff) div 4);

  W(@buff[100], 100);
  W(@buff[300], 100);

  for i := 0 to 500 * 2 do
    begin
      W(@buff[umlRR(0, length(buff) - 400)], umlRR(4, 400));
    end;

  fs.Build_Flush_File;

  DisposeObject(fs);
  DisposeObject(mirror);
end;

end.
