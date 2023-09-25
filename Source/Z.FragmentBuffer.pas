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
  DateUtils,
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.MemoryStream;

type
  TPart_Data = class;
  TFragment_Space_Tool = class;

  TPart_Buffer_Tool_ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<TPart_Data>;

  TPart_Data = class
  public
    Owner_Buffer: TPart_Buffer_Tool_.PQueueStruct;
    bPos: Int64;
    Memory: TMem64;
    Size: Int64;
    constructor Create;
    destructor Destroy; override;
    function ePos(): Int64;
    function IsOverlap(b, e: Int64): Boolean;
    procedure Copy_To_Ptr(b, buff_size: Int64; p: Pointer);
  end;

  TPart_Buffer_Tool = class(TPart_Buffer_Tool_)
  private
    function Do_Sort_bPos(var L, R: TPart_Data): Integer;
  public
    Owner: TFragment_Space_Tool;
    constructor Create(Owner_: TFragment_Space_Tool);
    destructor Destroy; override;
    procedure Sort;
    procedure Extract_To_Mem(Memory: TMem64; Mem_Pos: Int64);
  end;

  TOn_Get_Fragment = procedure(Sender: TFragment_Space_Tool; Position_: Int64; buff_: Pointer; Size_: Int64; var successed: Boolean) of object;

  TFragment_Space_Tool = class
  private type
    TSpace_Span_Tool = {$IFDEF FPC}specialize {$ENDIF FPC} TBig_Hash_Pair_Pool<Int64, Boolean>;

    TFragment_Space_Stream_Head = packed record
      Edition: Byte;
      TimeTick: TDateTime;
      MD5: TMD5;
      Num: Int64;
    end;
  private
    FSpace_Span_Tool: TSpace_Span_Tool;
    FSpace_Span: Int64; // default 1024*1024
    FBuffer: TPart_Buffer_Tool;
    FRead_Buffer_Cache: Boolean;
    FMin_Pos, FMax_Pos: Int64;
    FFlushFileName: TPascalString;
    FFLushTime: TDateTime;
    FLoad_Error: Boolean;
    FOn_Get_Fragment: TOn_Get_Fragment;
    procedure DoFree(var Data: TPart_Data);
    procedure Update_Space_Span(bPos, ePos: Int64; Is_Cache_: Boolean);
    function In_Space_Span(bPos, ePos: Int64): Boolean;
    function IsOverlap(b, e: Int64): Boolean;
    function Found_Overlap(bPos, Size: Int64): TPart_Buffer_Tool;
    function Process_Overlap_Buffer(bPos: Int64; p: Pointer; Size: Int64): Boolean;
    function Get_Fragment(Position: Int64; buff: Pointer; Size: Int64): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Write_Buffer(bPos: Int64; p: Pointer; Size: Int64);
    property Read_Buffer_Cache: Boolean read FRead_Buffer_Cache write FRead_Buffer_Cache;
    function Read_Buffer(bPos: Int64; p: Pointer; Size: Int64): Boolean;
    procedure Flush_To_Stream_And_Clear_Buffer(stream: TCore_Stream);
    property FlushFileName: TPascalString read FFlushFileName;
    property FLushTime: TDateTime read FFLushTime;
    procedure Save_To_Stream(stream: TCore_Stream);
    procedure Save_To_File(FileName_: TPascalString);
    function Load_From_Stream(stream: TCore_Stream): Boolean;
    function Load_From_File(FileName_: TPascalString): Boolean;
    property Load_Error: Boolean read FLoad_Error;
    procedure Sort;
    function Num: NativeInt;
    function Fragment_Memory: Int64;
    function Repeat_: TPart_Buffer_Tool_.TRepeat___;
    property Buffer: TPart_Buffer_Tool read FBuffer;
    property Space_Span: Int64 read FSpace_Span write FSpace_Span; // default 1024*1024
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

  TFragment_Space_Tool_History_ = {$IFDEF FPC}specialize {$ENDIF FPC} TBig_Object_List<TFragment_Space_Tool>;

  TFragment_Space_Tool_History = class(TFragment_Space_Tool_History_)
  private
    function do_sort(var L, R: TFragment_Space_Tool): Integer;
  public
    procedure Sort();
  end;

  TSafe_Flush_Restore_Mode = (sfLastHistory, sfAllHistory, sfIgnore);

  TSafe_Flush_Stream = class(TCore_Stream)
  private
    FCritical: TCritical;
    FFileName: TPascalString;
    FIsNew: Boolean;
    FIsWrite: Boolean;
    FWait_Hardware: Boolean;
    Source_IO: TCore_Stream;
    FFlush_Num: Int64;
    FMax_Flush_History_Num: Int64;
    FHistory: TFragment_Space_Tool_History;
    FFragment_Space: TFragment_Space_Tool;
    FFragment_Space_Space_Span: Int64;
    FFragment_Space_Read_Buffer_Cache: Boolean;
    procedure Do_Get_Fragment(Sender: TFragment_Space_Tool; Position_: Int64; buff_: Pointer; Size_: Int64; var successed: Boolean);
    procedure Fixed_From_Last_Flush_History; // restore last history
    procedure Fixed_From_Sequence_Flush_History; // restore sequence history
  protected
    procedure SetSize(const NewSize: Int64); overload; override;
    procedure SetSize(NewSize: longint); overload; override;
  public
    constructor Create(const FileName_: TPascalString; IsNew_, IsWrite_, IsReliableFile_, Wait_Hardware_: Boolean; RMode: TSafe_Flush_Restore_Mode); overload;
    constructor Create(const FileName_: TPascalString; IsNew_, IsWrite_, IsReliableFile_: Boolean); overload;
    constructor Create(const FileName_: TPascalString; IsNew_, IsWrite_: Boolean); overload;
    destructor Destroy; override;
    property IsNew: Boolean read FIsNew;
    property IsWrite: Boolean read FIsWrite;
    property Wait_Hardware: Boolean read FWait_Hardware write FWait_Hardware;
    function IsOnlyRead: Boolean;
    property FileName: TPascalString read FFileName;
    property Fragment_Space: TFragment_Space_Tool read FFragment_Space;
    property Fragment_Space_Read_Buffer_Cache: Boolean read FFragment_Space_Read_Buffer_Cache write FFragment_Space_Read_Buffer_Cache;
    property Fragment_Space_Space_Span: Int64 read FFragment_Space_Space_Span write FFragment_Space_Space_Span;
    property Flush_Num: Int64 read FFlush_Num;
    property Max_Flush_History_Num: Int64 read FMax_Flush_History_Num write FMax_Flush_History_Num;
    property History: TFragment_Space_Tool_History read FHistory;
    procedure Flush;

    function Write(const Buffer; Count: longint): longint; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Seek(const Offset: Int64; origin: TSeekOrigin): Int64; override;

    class procedure Test();
  end;

implementation

uses Z.Geometry2D, Z.UnicodeMixedLib, Z.Status;

constructor TPart_Data.Create;
begin
  inherited Create;
  Owner_Buffer := nil;
  bPos := 0;
  Memory := TMem64.CustomCreate(1024);
  Size := 0;
end;

destructor TPart_Data.Destroy;
begin
  DisposeObjectAndNil(Memory);
  inherited Destroy;
end;

function TPart_Data.ePos(): Int64;
begin
  Result := bPos + Size;
end;

function TPart_Data.IsOverlap(b, e: Int64): Boolean;
begin
  Result :=
    umlInRange(bPos, b, e) or
    umlInRange(ePos, b, e) or
    umlInRange(b, bPos, ePos) or
    umlInRange(e, bPos, ePos);
end;

procedure TPart_Data.Copy_To_Ptr(b, buff_size: Int64; p: Pointer);
begin
  if bPos >= b then
      CopyPtr(Memory.Memory, GetOffset(p, bPos - b), umlMin(buff_size - (bPos - b), Size))
  else
      CopyPtr(Memory.PosAsPtr(b - bPos), p, umlMin(buff_size, Size - (b - bPos)));
end;

function TPart_Buffer_Tool.Do_Sort_bPos(var L, R: TPart_Data): Integer;
begin
  Result := CompareInt64(L.bPos, R.bPos);
  if Result = 0 then
      Result := CompareInt64(L.Size, R.Size);
end;

constructor TPart_Buffer_Tool.Create(Owner_: TFragment_Space_Tool);
begin
  inherited Create;
  Owner := Owner_;
end;

destructor TPart_Buffer_Tool.Destroy;
begin
  inherited Destroy;
end;

procedure TPart_Buffer_Tool.Sort;
begin
  Sort_M({$IFDEF FPC}@{$ENDIF FPC}Do_Sort_bPos);
end;

procedure TPart_Buffer_Tool.Extract_To_Mem(Memory: TMem64; Mem_Pos: Int64);
var
  bPos, ePos: Int64;
begin
  if Num <= 0 then
      exit;
  bPos := First^.Data.bPos;
  ePos := Last^.Data.ePos;
  Memory.Size := Mem_Pos + (ePos - bPos);
  with Repeat_ do
    repeat
        CopyPtr(Queue^.Data.Memory.Memory, Memory.PosAsPtr(Mem_Pos + (Queue^.Data.bPos - bPos)), Queue^.Data.Size);
    until not Next;
end;

procedure TFragment_Space_Tool.DoFree(var Data: TPart_Data);
begin
  DisposeObjectAndNil(Data);
end;

procedure TFragment_Space_Tool.Update_Space_Span(bPos, ePos: Int64; Is_Cache_: Boolean);
var
  i, bI, eI: Int64;
begin
  bI := (bPos div FSpace_Span);
  eI := (ePos div FSpace_Span);
  i := umlMax(bI - 1, 0);
  while i <= eI + 1 do
    begin
      FSpace_Span_Tool[i * FSpace_Span] := Is_Cache_;
      inc(i);
    end;
end;

function TFragment_Space_Tool.In_Space_Span(bPos, ePos: Int64): Boolean;
var
  i, bI, eI: Int64;
begin
  Result := True;
  bI := (bPos div FSpace_Span);
  eI := (ePos div FSpace_Span);
  i := umlMax(bI - 1, 0);
  while i <= eI + 1 do
    if not FSpace_Span_Tool.Get_Default_Value(i * FSpace_Span, False) then
        inc(i)
    else
        exit;
  Result := False;
end;

function TFragment_Space_Tool.IsOverlap(b, e: Int64): Boolean;
begin
  Result :=
    umlInRange(FMin_Pos, b, e) or
    umlInRange(FMax_Pos, b, e) or
    umlInRange(b, FMin_Pos, FMax_Pos) or
    umlInRange(e, FMin_Pos, FMax_Pos);
end;

function TFragment_Space_Tool.Found_Overlap(bPos, Size: Int64): TPart_Buffer_Tool;
begin
  Result := nil;

  if not IsOverlap(bPos, bPos + Size) then // lv1 optmized
      exit;

  if not In_Space_Span(bPos, bPos + Size) then // lv2 optimized
      exit;

  if Num > 0 then
    with Repeat_ do
      repeat
        if Queue^.Data.IsOverlap(bPos, bPos + Size) then
          begin
            if Result = nil then
                Result := TPart_Buffer_Tool.Create(self);
            Result.Add(Queue^.Data);
          end;
      until not Next;
  if Result <> nil then
      Result.Sort;
end;

function TFragment_Space_Tool.Process_Overlap_Buffer(bPos: Int64; p: Pointer; Size: Int64): Boolean;
var
  L: TPart_Buffer_Tool;
  inst: TPart_Data;
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
          CopyPtr(p, inst.Memory.PosAsPtr(bPos - inst.bPos), Size);
          DisposeObject(L);
          Result := True;
          Update_Space_Span(bPos, bPos + Size, True);
          exit;
        end;
      if (inst.ePos = bPos) then // append
        begin
          inst.Memory.Delta := umlMin(1024 * 1024, inst.Size);
          inst.Memory.Position := inst.Size;
          inst.Memory.WritePtr(p, Size);
          inst.Size := inst.Size + Size;
          DisposeObject(L);
          Result := True;
          Update_Space_Span(bPos, bPos + Size, True);
          exit;
        end;
    end;
  // compare overlap buffer
  inst := TPart_Data.Create;
  if L.First^.Data.bPos < bPos then
    begin
      inst.bPos := L.First^.Data.bPos;
      L.Extract_To_Mem(inst.Memory, 0);
      inst.Memory.Position := bPos - L.First^.Data.bPos;
    end
  else
    begin
      inst.bPos := bPos;
      L.Extract_To_Mem(inst.Memory, L.First^.Data.bPos - bPos);
      inst.Memory.Position := 0;
    end;
  // overwrite overlap buffer
  inst.Memory.WritePtr(p, Size);
  inst.Size := inst.Memory.Size;
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
        FBuffer.Push_To_Recycle_Pool(Queue^.Data.Owner_Buffer);
    until not Next;
  FBuffer.Free_Recycle_Pool;

  DisposeObject(L);
  Result := True;
  Update_Space_Span(bPos, bPos + Size, True);
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
  FSpace_Span_Tool := TSpace_Span_Tool.Create($FFFF, False);
  FSpace_Span := 1024 * 1024;
  FBuffer := TPart_Buffer_Tool.Create(self);
  FBuffer.OnFree := {$IFDEF FPC}@{$ENDIF FPC}DoFree;
  FRead_Buffer_Cache := False;
  FMin_Pos := 0;
  FMax_Pos := 0;
  FFlushFileName := '';
  FFLushTime := 0;
  FLoad_Error := False;
  FOn_Get_Fragment := nil;
end;

destructor TFragment_Space_Tool.Destroy;
begin
  DisposeObject(FBuffer);
  DisposeObject(FSpace_Span_Tool);
  inherited Destroy;
end;

procedure TFragment_Space_Tool.Clear;
begin
  FSpace_Span_Tool.Clear;
  FBuffer.Clear;
  FMin_Pos := 0;
  FMax_Pos := 0;
end;

procedure TFragment_Space_Tool.Write_Buffer(bPos: Int64; p: Pointer; Size: Int64);
var
  inst: TPart_Data;
begin
  if Process_Overlap_Buffer(bPos, p, Size) then
      exit;
  inst := TPart_Data.Create;
  inst.bPos := bPos;
  inst.Memory.WritePtr(p, Size);
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
  Update_Space_Span(bPos, bPos + Size, True);
end;

function TFragment_Space_Tool.Read_Buffer(bPos: Int64; p: Pointer; Size: Int64): Boolean;
var
  L: TPart_Buffer_Tool;
  inst: TPart_Data;
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
          CopyPtr(inst.Memory.PosAsPtr(bPos - inst.bPos), p, Size);
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
        Queue^.Data.Copy_To_Ptr(bPos, Size, p);
    until not Next;
  DisposeObject(L);
  Result := True;
  if FRead_Buffer_Cache then
      Write_Buffer(bPos, p, Size);
end;

procedure TFragment_Space_Tool.Flush_To_Stream_And_Clear_Buffer(stream: TCore_Stream);
begin
  if Num > 0 then
    begin
      with Repeat_ do
        repeat
          stream.Position := Queue^.Data.bPos;
          stream.Write(Queue^.Data.Memory.Memory^, Queue^.Data.Size);
        until not Next;
      Clear;
    end;
end;

procedure TFragment_Space_Tool.Save_To_Stream(stream: TCore_Stream);
var
  m64: TMem64;
  head: TFragment_Space_Stream_Head;
  inst: TPart_Data;
begin
  FFLushTime := umlNow();
  m64 := TMem64.CustomCreate(SizeOf(TFragment_Space_Stream_Head) + (Num * 16) + Fragment_Memory);
  head.Edition := 1;
  head.TimeTick := FFLushTime;
  head.MD5 := NULL_MD5;
  head.Num := Num;
  m64.Position := 0;
  m64.WritePtr(@head, SizeOf(TFragment_Space_Stream_Head));
  if Num > 0 then
    with Repeat_ do
      repeat
        inst := Queue^.Data;
        m64.WriteInt64(inst.bPos);
        m64.WriteInt64(inst.Size);
        m64.WritePtr(inst.Memory.Memory, inst.Size);
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

procedure TFragment_Space_Tool.Save_To_File(FileName_: TPascalString);
var
  stream: TCore_Stream;
begin
  FFlushFileName := FileName_;
  stream := TCore_FileStream.Create(FileName_, fmCreate);
  try
      Save_To_Stream(stream);
  except
  end;
  DisposeObject(stream);
end;

function TFragment_Space_Tool.Load_From_Stream(stream: TCore_Stream): Boolean;
var
  m64: TMem64;
  head: TFragment_Space_Stream_Head;
  MD5: TMD5;
  i: Int64;
  inst: TPart_Data;
  n: TPascalString;
begin
  FLoad_Error := True;
  Result := False;
  Clear;
  m64 := TMem64.Create;
  m64.LoadFromStream(stream);
  m64.Position := 0;
  if m64.ReadPtr(@head, SizeOf(TFragment_Space_Stream_Head)) = SizeOf(TFragment_Space_Stream_Head) then
    if head.Edition = 1 then
      begin
        MD5 := umlMD5(m64.PosAsPtr(SizeOf(TFragment_Space_Stream_Head)), m64.Size - SizeOf(TFragment_Space_Stream_Head));
        if umlCompareMD5(MD5, head.MD5) then
          begin
            i := 0;
            while i < head.Num do
              begin
                inst := TPart_Data.Create;
                inst.bPos := m64.ReadInt64;
                inst.Size := m64.ReadInt64;
                inst.Memory.Size := inst.Size;
                m64.ReadPtr(inst.Memory.Memory, inst.Size);
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
                Update_Space_Span(inst.bPos, inst.ePos, True);
              end;
            FFLushTime := head.TimeTick;
            FLoad_Error := False;
            Result := True;
          end
        else
          begin
            FLoad_Error := True;
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
      end;
  DisposeObject(m64);
end;

function TFragment_Space_Tool.Load_From_File(FileName_: TPascalString): Boolean;
var
  stream: TCore_Stream;
begin
  FFlushFileName := FileName_;
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
        inc(Result, Queue^.Data.Size);
    until not Next;
end;

function TFragment_Space_Tool.Repeat_: TPart_Buffer_Tool_.TRepeat___;
begin
  Result := FBuffer.Repeat_;
end;

constructor TFragment_Space_Tool_Test.Create;
begin
  inherited Create;
  tool := TFragment_Space_Tool.Create;
  tool.Space_Span := 10;
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
    repeat___: TPart_Buffer_Tool.TRepeat___;
  begin
    Result := True;
    repeat___ := t_.tool.Repeat_;
    repeat
      if not CompareMemory(repeat___.Queue^.Data.Memory.Memory, t_.mirror.PosAsPtr(repeat___.Queue^.Data.bPos), repeat___.Queue^.Data.Size) then
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
  TMT19937.SetSeed(0);
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

function TFragment_Space_Tool_History.do_sort(var L, R: TFragment_Space_Tool): Integer;
begin
  Result := CompareDateTime(L.FFLushTime, R.FFLushTime);
end;

procedure TFragment_Space_Tool_History.Sort;
begin
  Sort_M({$IFDEF FPC}@{$ENDIF FPC}do_sort);
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

procedure TSafe_Flush_Stream.Fixed_From_Last_Flush_History;
var
  tmp: TFragment_Space_Tool;
  file_path: U_String;
  file_arry: U_StringArray;
  fn: U_SystemString;
begin
  file_path := umlGetFilePath(FFileName);
  if file_path = '' then
      file_path := umlCurrentPath;

  // search .~SafeFlush
  file_arry := umlGet_File_Array(file_path);
  for fn in file_arry do
    if umlMultipleMatch(True, umlGetFileName(FFileName) + '.~SafeFlush(*)', fn) then
      begin
        tmp := TFragment_Space_Tool.Create;
        if tmp.Load_From_File(umlCombineFileName(file_path, fn)) then
          begin
            FHistory.Add(tmp);
          end
        else
          begin
            umlDeleteFile(tmp.FFlushFileName);
            DisposeObject(tmp);
          end;
      end;

  // write .~SafeFlush
  if FHistory.Num > 0 then
    begin
      // sort
      FHistory.Sort;
      // write last update
      if Source_IO.Size < FHistory.Last^.Data.FMax_Pos then
          Source_IO.Size := FHistory.Last^.Data.FMax_Pos;
      FHistory.Last^.Data.Flush_To_Stream_And_Clear_Buffer(Source_IO);

{$IFDEF MSWINDOWS}
      if FWait_Hardware then
        if Source_IO is TCore_FileStream then
            FlushFileBuffers(TCore_FileStream(Source_IO).Handle);
{$ENDIF MSWINDOWS}
      with FHistory.Repeat_ do
        repeat
            Queue^.Data.Clear;
        until not Next;
      Source_IO.Position := 0;
    end;
end;

procedure TSafe_Flush_Stream.Fixed_From_Sequence_Flush_History;
var
  tmp: TFragment_Space_Tool;
  file_path: U_String;
  file_arry: U_StringArray;
  fn: U_SystemString;
begin
  file_path := umlGetFilePath(FFileName);
  if file_path = '' then
      file_path := umlCurrentPath;

  // search .~SafeFlush
  file_arry := umlGet_File_Array(file_path);
  for fn in file_arry do
    if umlMultipleMatch(True, umlGetFileName(FFileName) + '.~SafeFlush(*)', fn) then
      begin
        tmp := TFragment_Space_Tool.Create;
        if tmp.Load_From_File(umlCombineFileName(file_path, fn)) then
          begin
            FHistory.Add(tmp);
          end
        else
          begin
            umlDeleteFile(tmp.FFlushFileName);
            DisposeObject(tmp);
          end;
      end;

  // write .~SafeFlush
  if FHistory.Num > 0 then
    begin
      // sort
      FHistory.Sort;

      // sequence write history
      with FHistory.Repeat_ do
        repeat
          if not Queue^.Data.Load_Error then
            begin
              if Source_IO.Size < Queue^.Data.FMax_Pos then
                  Source_IO.Size := Queue^.Data.FMax_Pos;
              Queue^.Data.Flush_To_Stream_And_Clear_Buffer(Source_IO);
              Queue^.Data.Clear;
            end;
        until not Next;

{$IFDEF MSWINDOWS}
      if FWait_Hardware then
        if Source_IO is TCore_FileStream then
            FlushFileBuffers(TCore_FileStream(Source_IO).Handle);
{$ENDIF MSWINDOWS}
      with FHistory.Repeat_ do
        repeat
            Queue^.Data.Clear;
        until not Next;
      Source_IO.Position := 0;
    end;
end;

procedure TSafe_Flush_Stream.SetSize(const NewSize: Int64);
begin
  FCritical.Lock;
  try
      Source_IO.Size := NewSize;
  finally
      FCritical.UnLock;
  end;
end;

procedure TSafe_Flush_Stream.SetSize(NewSize: longint);
begin
  SetSize(Int64(NewSize));
end;

constructor TSafe_Flush_Stream.Create(const FileName_: TPascalString; IsNew_, IsWrite_, IsReliableFile_, Wait_Hardware_: Boolean; RMode: TSafe_Flush_Restore_Mode);
var
  mode_: Word;
begin
  inherited Create;
  FCritical := TCritical.Create;

  FFileName := FileName_;
  FIsNew := IsNew_;
  FIsWrite := IsWrite_;
  FWait_Hardware := Wait_Hardware_;

  if umlGetFilePath(FFileName) = '' then
      FFileName := umlCombineFileName(umlCurrentPath, FFileName);

  if (not umlFileExists(FFileName)) and (FIsWrite) then
      FIsNew := True;

  if FIsNew then
      mode_ := fmCreate
  else if FIsWrite then
      mode_ := fmOpenReadWrite
  else
      mode_ := fmOpenRead or fmShareDenyNone;

  if IsReliableFile_ then
      Source_IO := TReliableFileStream.Create(FFileName, FIsNew, FIsWrite)
  else
      Source_IO := TCore_FileStream.Create(FFileName, mode_);

  FFlush_Num := 1;
  FMax_Flush_History_Num := 10;

  FHistory := TFragment_Space_Tool_History.Create(True);

  FFragment_Space := TFragment_Space_Tool.Create;
  FFragment_Space.Read_Buffer_Cache := True;
  FFragment_Space.On_Get_Fragment := {$IFDEF FPC}@{$ENDIF FPC}Do_Get_Fragment;
  FFragment_Space_Space_Span := FFragment_Space.FSpace_Span;
  FFragment_Space_Read_Buffer_Cache := True;

  if (not FIsNew) and (FIsWrite) then
    begin
      if RMode = TSafe_Flush_Restore_Mode.sfLastHistory then
          Fixed_From_Last_Flush_History()
      else if RMode = TSafe_Flush_Restore_Mode.sfAllHistory then
          Fixed_From_Sequence_Flush_History();
    end;
end;

constructor TSafe_Flush_Stream.Create(const FileName_: TPascalString; IsNew_, IsWrite_, IsReliableFile_: Boolean);
begin
  Create(FileName_, IsNew_, IsWrite_, False, True, TSafe_Flush_Restore_Mode.sfLastHistory);
end;

constructor TSafe_Flush_Stream.Create(const FileName_: TPascalString; IsNew_, IsWrite_: Boolean);
begin
  Create(FileName_, IsNew_, IsWrite_, False);
end;

destructor TSafe_Flush_Stream.Destroy;
begin
  Flush();
  DisposeObject(Source_IO);
  // remove all history file
  if FHistory.Num > 0 then
    with FHistory.Repeat_ do
      repeat
          umlDeleteFile(Queue^.Data.FFlushFileName);
      until not Next;
  DisposeObject(FHistory);
  DisposeObject(FFragment_Space);
  DisposeObject(FCritical);
  inherited Destroy;
end;

function TSafe_Flush_Stream.IsOnlyRead: Boolean;
begin
  Result := not FIsWrite;
end;

procedure TSafe_Flush_Stream.Flush;
var
  bak_pos: Int64;
begin
  if not FIsWrite then
      exit;
  if FFragment_Space.Num <= 0 then
    begin
      // rebuild instance
      DisposeObject(FFragment_Space);
      FFragment_Space := TFragment_Space_Tool.Create;
      FFragment_Space.Read_Buffer_Cache := FFragment_Space_Read_Buffer_Cache;
      FFragment_Space.Space_Span := FFragment_Space_Space_Span;
      FFragment_Space.On_Get_Fragment := {$IFDEF FPC}@{$ENDIF FPC}Do_Get_Fragment;
      exit;
    end;

  FCritical.Lock;
  try
    // Save .~SafeFlush
    FFragment_Space.Sort();
    FFragment_Space.Save_To_File(FFileName + PFormat('.~SafeFlush(%d)', [FFlush_Num]));
    inc(FFlush_Num);

    // write modifyciation
    bak_pos := Source_IO.Position;
    if Source_IO.Size < FFragment_Space.FMax_Pos then
        Source_IO.Size := FFragment_Space.FMax_Pos;
    FFragment_Space.Flush_To_Stream_And_Clear_Buffer(Source_IO);
    Source_IO.Position := bak_pos;
    FFragment_Space.Clear;

{$IFDEF MSWINDOWS}
    if FWait_Hardware then
      if Source_IO is TCore_FileStream then
          FlushFileBuffers(TCore_FileStream(Source_IO).Handle);
{$ENDIF MSWINDOWS}
    // save to history
    FFragment_Space.On_Get_Fragment := nil;
    FHistory.Add(FFragment_Space);

    // rebuild instance
    FFragment_Space := TFragment_Space_Tool.Create;
    FFragment_Space.Read_Buffer_Cache := FFragment_Space_Read_Buffer_Cache;
    FFragment_Space.Space_Span := FFragment_Space_Space_Span;
    FFragment_Space.On_Get_Fragment := {$IFDEF FPC}@{$ENDIF FPC}Do_Get_Fragment;

    // check and remove history
    if FHistory.Num > 0 then
      while (FHistory.Num > FMax_Flush_History_Num) do
        begin
          umlDeleteFile(FHistory.First^.Data.FFlushFileName);
          FHistory.Next;
        end;
  finally
      FCritical.UnLock;
  end;
end;

function TSafe_Flush_Stream.Write(const Buffer; Count: longint): longint;
var
  pos_: Int64;
begin
  if not FIsWrite then
      exit(0);
  FCritical.Lock;
  try
    pos_ := Source_IO.Position;
    FFragment_Space.Write_Buffer(pos_, @Buffer, Count);
    if Source_IO.Size < FFragment_Space.FMax_Pos then
        Source_IO.Size := FFragment_Space.FMax_Pos;
    Source_IO.Position := pos_ + Count;
    Result := Count;
  finally
      FCritical.UnLock;
  end;
end;

function TSafe_Flush_Stream.Read(var Buffer; Count: longint): longint;
var
  pos_: Int64;
begin
  FCritical.Lock;
  try
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
  finally
      FCritical.UnLock;
  end;
end;

function TSafe_Flush_Stream.Seek(const Offset: Int64; origin: TSeekOrigin): Int64;
begin
  FCritical.Lock;
  try
      Result := Source_IO.Seek(Offset, origin);
  finally
      FCritical.UnLock;
  end;
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
  i, j: Integer;
  tmp: TMS64;
begin
  fs := TSafe_Flush_Stream.Create(umlCombineFileName(umlCurrentPath, 'test.dat'), False, True);
  fs.Max_Flush_History_Num := 5;
  mirror := TMS64.CustomCreate(1024 * 1024);
  setLength(buff, 500 * 1024);
  TMT19937.Rand32(MaxInt, @buff[0], length(buff) div 4);

  W(@buff[100], 100);
  W(@buff[300], 100);

  for i := 0 to 500 * 2 do
    begin
      W(@buff[umlRR(0, length(buff) - 400)], umlRR(4, 400));
    end;
  fs.Flush;

  if not umlCompareMD5(umlStreamMD5(fs), umlStreamMD5(mirror)) then
      DoStatus('TSafe_Flush_Stream test failed!')
  else
      DoStatus('TSafe_Flush_Stream test passed!');

  DisposeObject(fs);
  DisposeObject(mirror);
end;

end.
