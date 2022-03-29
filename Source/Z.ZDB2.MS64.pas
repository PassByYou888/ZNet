{ ****************************************************************************** }
{ * ZDB 2.0 automated fragment for MemoryStream support                        * }
{ ****************************************************************************** }
unit Z.ZDB2.MS64;

{$I Z.Define.inc}

interface

uses Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status, Z.MemoryStream,
  Z.DFE, Z.ZDB2, Z.Cipher;

type
  TZDB2_List_MS64 = class;

  TZDB2_MS64 = class
  private
    FTimeOut: TTimeTick;
    FAlive: TTimeTick;
    FID: Integer;
    FData: TMS64;
    FData_MD5: TMD5;
  public
    CoreSpace: TZDB2_Core_Space;
    Keep: Integer;
    constructor Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer); virtual;
    destructor Destroy; override;
    procedure Progress; virtual;
    procedure Load;
    procedure Save;
    procedure RecycleMemory;
    procedure Remove;
    function GetData: TMS64;
    property Data: TMS64 read GetData;
    property Data_Direct: TMS64 read FData;
    property ID: Integer read FID;
    property Data_MD5: TMD5 read FData_MD5;
  end;

  TZDB2_MS64_Class = class of TZDB2_MS64;

  TZDB2_List_MS64_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TZDB2_MS64>;

  TOnCreate_ZDB2_MS64 = procedure(Sender: TZDB2_List_MS64; Obj: TZDB2_MS64) of object;

  TZDB2_List_MS64 = class(TZDB2_List_MS64_Decl)
  private type
    THead_ = packed record
      Identifier: Word;
      ID: Integer;
    end;

    PHead_ = ^THead_;
  private
    procedure DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
    function GetAutoFreeStream: Boolean;
    procedure SetAutoFreeStream(const Value: Boolean);
  public
    MS64_Class: TZDB2_MS64_Class;
    TimeOut: TTimeTick;
    DeltaSpace: Int64;
    BlockSize: Word;
    IOHnd: TIOHnd;
    CoreSpace: TZDB2_Core_Space;
    OnCreateClass: TOnCreate_ZDB2_MS64;
    constructor Create(MS64_Class_: TZDB2_MS64_Class; OnCreateClass_: TOnCreate_ZDB2_MS64; TimeOut_: TTimeTick;
      Stream_: TCore_Stream; OnlyRead_: Boolean; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
    destructor Destroy; override;
    property AutoFreeStream: Boolean read GetAutoFreeStream write SetAutoFreeStream;
    property IsOnlyRead: Boolean read IOHnd.IsOnlyRead;
    procedure Remove(Obj: TZDB2_MS64; RemoveData_: Boolean);
    procedure Delete(Index: Integer; RemoveData_: Boolean);
    procedure Clear(RemoveData_: Boolean);
    function NewDataFrom(ID_: Integer): TZDB2_MS64; overload;
    function NewData: TZDB2_MS64; overload;
    procedure Flush(flush_core_space: Boolean); overload;
    procedure Flush; overload;
    procedure ExtractTo(Stream_: TCore_Stream);
    procedure Progress;

    class procedure Test;
  end;

implementation

constructor TZDB2_MS64.Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer);
begin
  inherited Create;
  FTimeOut := 5 * 1000;
  FAlive := GetTimeTick;
  Keep := 0;
  FID := ID_;
  CoreSpace := CoreSpace_;
  FData := nil;
  FData_MD5 := NullMD5;
end;

destructor TZDB2_MS64.Destroy;
begin
  Save;
  inherited Destroy;
end;

procedure TZDB2_MS64.Progress;
begin
  if FData = nil then
      exit;
  if (Keep <= 0) and (GetTimeTick - FAlive > FTimeOut) then
    begin
      Save;
{$IFDEF DEBUG}
      DoStatus('%s -> %s Space Recycle ID %s size:%d', [UnitName, ClassName, CoreSpace.GetSpaceHndAsText(FID).Text, CoreSpace.GetDataSize(FID)]);
{$ENDIF DEBUG}
    end;
end;

procedure TZDB2_MS64.Load;
begin
  FData_MD5 := NullMD5;
  if FID < 0 then
      exit;
  FData.Clear;
  if CoreSpace.ReadStream(FData, FID) then
    begin
      FData_MD5 := umlStreamMD5(FData);
    end
  else
      FData.Clear;
end;

procedure TZDB2_MS64.Save;
var
  tmp_md5: TMD5;
  old_ID: Integer;
begin
  if FData = nil then
      exit;
  if not CoreSpace.Space_IOHnd^.IsOnlyRead then
    begin
      try
        tmp_md5 := umlStreamMD5(FData);
        if umlMD5Compare(FData_MD5, NullMD5) or (not umlMD5Compare(tmp_md5, FData_MD5)) or (FID < 0) then
          begin
            old_ID := FID;
            CoreSpace.WriteStream(FData, FID);
            FData_MD5 := tmp_md5;
            if old_ID >= 0 then
                CoreSpace.RemoveData(old_ID, False);
          end;
      except
      end;
    end;
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_MS64.RecycleMemory;
begin
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_MS64.Remove;
begin
  if CoreSpace.Space_IOHnd^.IsOnlyRead then
      exit;
  if FID >= 0 then
      CoreSpace.RemoveData(FID, False);
  DisposeObjectAndNil(FData);
  FID := -1;
  FData_MD5 := NullMD5;
end;

function TZDB2_MS64.GetData: TMS64;
begin
  if FData = nil then
    begin
      FData := TMS64.Create;
      Load;
    end;
  Result := FData;
  FAlive := GetTimeTick;
end;

procedure TZDB2_List_MS64.DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
begin
  retry := Trigger.AppendSpace(DeltaSpace, BlockSize);
end;

function TZDB2_List_MS64.GetAutoFreeStream: Boolean;
begin
  Result := IOHnd.AutoFree;
end;

procedure TZDB2_List_MS64.SetAutoFreeStream(const Value: Boolean);
begin
  IOHnd.AutoFree := Value;
end;

constructor TZDB2_List_MS64.Create(MS64_Class_: TZDB2_MS64_Class; OnCreateClass_: TOnCreate_ZDB2_MS64; TimeOut_: TTimeTick;
  Stream_: TCore_Stream; OnlyRead_: Boolean; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
var
  buff: TZDB2_BlockHandle;
  ID_: Integer;
  m64: TMem64;
begin
  inherited Create;
  MS64_Class := MS64_Class_;
  TimeOut := TimeOut_;
  DeltaSpace := DeltaSpace_;
  BlockSize := BlockSize_;
  InitIOHnd(IOHnd);
  umlFileCreateAsStream(Stream_, IOHnd, OnlyRead_);
  CoreSpace := TZDB2_Core_Space.Create(@IOHnd);
  CoreSpace.Cipher := Cipher_;
  CoreSpace.Mode := smNormal;
  CoreSpace.AutoCloseIOHnd := True;
  CoreSpace.OnNoSpace := {$IFDEF FPC}@{$ENDIF FPC}DoNoSpace;
  if umlFileSize(IOHnd) > 0 then
    begin
      if not CoreSpace.Open then
          RaiseInfo('error.');
    end;
  OnCreateClass := OnCreateClass_;
  if CoreSpace.BlockCount = 0 then
      exit;

  if (PHead_(@CoreSpace.UserCustomHeader^[0])^.Identifier = $FFFF) and
    CoreSpace.Check(PHead_(@CoreSpace.UserCustomHeader^[0])^.ID) then
    begin
      m64 := TMem64.Create;
      CoreSpace.ReadData(m64, PHead_(@CoreSpace.UserCustomHeader^[0])^.ID);
      SetLength(buff, m64.Size shr 2);
      if length(buff) > 0 then
          CopyPtr(m64.Memory, @buff[0], length(buff) shl 2);
      DisposeObject(m64);
      CoreSpace.RemoveData(PHead_(@CoreSpace.UserCustomHeader^[0])^.ID, False);
      FillPtr(@CoreSpace.UserCustomHeader^[0], SizeOf(THead_), 0);
    end
  else
      buff := CoreSpace.BuildTableID;

  for ID_ in buff do
      NewDataFrom(ID_);
  SetLength(buff, 0);
end;

destructor TZDB2_List_MS64.Destroy;
begin
  Flush;
  Clear(False);
  DisposeObjectAndNil(CoreSpace);
  inherited Destroy;
end;

procedure TZDB2_List_MS64.Remove(Obj: TZDB2_MS64; RemoveData_: Boolean);
begin
  if IOHnd.IsOnlyRead then
      exit;
  if RemoveData_ then
      Obj.Remove;
  DisposeObject(Obj);
  inherited Remove(Obj);
end;

procedure TZDB2_List_MS64.Delete(Index: Integer; RemoveData_: Boolean);
begin
  if IOHnd.IsOnlyRead then
      exit;
  if (index >= 0) and (index < Count) then
    begin
      if RemoveData_ then
          Items[index].Remove;
      DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TZDB2_List_MS64.Clear(RemoveData_: Boolean);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    begin
      if RemoveData_ then
          Items[i].Remove;
      DisposeObject(Items[i]);
    end;
  inherited Clear;
end;

function TZDB2_List_MS64.NewDataFrom(ID_: Integer): TZDB2_MS64;
begin
  Result := MS64_Class.Create(CoreSpace, ID_);
  Result.FTimeOut := TimeOut;
  Add(Result);
  if Assigned(OnCreateClass) then
      OnCreateClass(self, Result);
end;

function TZDB2_List_MS64.NewData: TZDB2_MS64;
begin
  if IOHnd.IsOnlyRead then
      Result := nil
  else
      Result := NewDataFrom(-1);
end;

procedure TZDB2_List_MS64.Flush(flush_core_space: Boolean);
var
  sum_: Integer;
  buff: TZDB2_BlockHandle;
  m64: TMem64;
  i, j: Integer;
begin
  if IOHnd.IsOnlyRead then
      exit;

  if (PHead_(@CoreSpace.UserCustomHeader^[0])^.Identifier = $FFFF) and
    CoreSpace.Check(PHead_(@CoreSpace.UserCustomHeader^[0])^.ID) then
    begin
      CoreSpace.RemoveData(PHead_(@CoreSpace.UserCustomHeader^[0])^.ID, False);
      FillPtr(@CoreSpace.UserCustomHeader^[0], SizeOf(THead_), 0);
    end;

  sum_ := 0;
  if Count > 0 then
    for i := 0 to Count - 1 do
      with Items[i] do
        if (FID >= 0) or (FData <> nil) then
            inc(sum_);

  if sum_ > 0 then
    begin
      // remove invalid
      SetLength(buff, sum_);
      j := 0;
      for i := 0 to Count - 1 do
        with Items[i] do
          if (FID >= 0) or (FData <> nil) then
            begin
              Save;
              buff[j] := FID;
              inc(j);
            end;

      // rebuild
      if sum_ < Count then
        begin
          Clear(False);
          for i in buff do
              NewDataFrom(i);
        end;

      // store
      if flush_core_space then
        begin
          m64 := TMem64.Create;
          m64.Mapping(@buff[0], length(buff) shl 2);
          PHead_(@CoreSpace.UserCustomHeader^[0])^.Identifier := $FFFF;
          CoreSpace.WriteData(m64, PHead_(@CoreSpace.UserCustomHeader^[0])^.ID, False);
          DisposeObject(m64);
          SetLength(buff, 0);
        end;
    end
  else
      FillPtr(@CoreSpace.UserCustomHeader^[0], SizeOf(THead_), 0);

  if flush_core_space then
      CoreSpace.Save;
end;

procedure TZDB2_List_MS64.Flush;
begin
  Flush(True);
end;

procedure TZDB2_List_MS64.ExtractTo(Stream_: TCore_Stream);
var
  TmpIOHnd: TIOHnd;
  TmpSpace: TZDB2_Core_Space;
  buff: TZDB2_BlockHandle;
  i: Integer;
  m64: TMem64;
begin
  Flush(False);
  InitIOHnd(TmpIOHnd);
  umlFileCreateAsStream(Stream_, TmpIOHnd);
  TmpSpace := TZDB2_Core_Space.Create(@TmpIOHnd);
  TmpSpace.Cipher := CoreSpace.Cipher;
  TmpSpace.Mode := smBigData;
  TmpSpace.OnNoSpace := {$IFDEF FPC}@{$ENDIF FPC}DoNoSpace;
  TmpSpace.BuildSpace(CoreSpace.State^.Physics, BlockSize);

  if Count > 0 then
    begin
      SetLength(buff, Count);
      for i := 0 to Count - 1 do
        begin
          m64 := TMem64.Create;
          if CoreSpace.ReadData(m64, Items[i].FID) then
            if not TmpSpace.WriteData(m64, buff[i], False) then
                RaiseInfo('error');
          DisposeObject(m64);
          CoreSpace.DoProgress(Count - 1, i);
        end;

      m64 := TMem64.Create;
      m64.Mapping(@buff[0], length(buff) shl 2);
      PHead_(@TmpSpace.UserCustomHeader^[0])^.Identifier := $FFFF;
      TmpSpace.WriteData(m64, PHead_(@TmpSpace.UserCustomHeader^[0])^.ID, False);
      DisposeObject(m64);
      SetLength(buff, 0);
    end
  else
      FillPtr(@TmpSpace.UserCustomHeader^[0], SizeOf(THead_), 0);

  TmpSpace.Save;
  DisposeObject(TmpSpace);
end;

procedure TZDB2_List_MS64.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
end;

class procedure TZDB2_List_MS64.Test;
var
  Cipher_: TZDB2_Cipher;
  M64_1, M64_2: TMS64;
  i: Integer;
  tmp: TZDB2_MS64;
  L: TZDB2_List_MS64;
  tk: TTimeTick;
begin
  TCompute.Sleep(5000);
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csRijndael, 'hello world', 1, True, True);
  M64_1 := TMS64.CustomCreate(16 * 1024 * 1024);
  M64_2 := TMS64.CustomCreate(16 * 1024 * 1024);

  tk := GetTimeTick;
  with TZDB2_List_MS64.Create(TZDB2_MS64, nil, 5000, M64_1, False, 64 * 1048576, 200, Cipher_) do
    begin
      AutoFreeStream := False;
      for i := 0 to 2000 do
        begin
          tmp := NewData();
          tmp.Data.WriteString('hello world');
          tmp.Save;
        end;
      DoStatus('build %d of MS64,time:%dms', [Count, GetTimeTick - tk]);
      Free;
    end;

  tk := GetTimeTick;
  L := TZDB2_List_MS64.Create(TZDB2_MS64, nil, 5000, M64_1, False, 64 * 1048576, 200, Cipher_);
  for i := 0 to L.Count - 1 do
    begin
      if not L[i].Data.ReadString().Same('hello world') then
          DoStatus('%s - test error.', [L.ClassName]);
    end;
  DoStatus('load %d of MS64,time:%dms', [L.Count, GetTimeTick - tk]);
  L.ExtractTo(M64_2);
  L.Free;

  tk := GetTimeTick;
  L := TZDB2_List_MS64.Create(TZDB2_MS64, nil, 5000, M64_2, False, 64 * 1048576, 200, Cipher_);
  for i := 0 to L.Count - 1 do
    begin
      if not L[i].Data.ReadString().Same('hello world') then
          DoStatus('%s - test error.', [L.ClassName]);
    end;
  DoStatus('load %d extract stream of MS64,time:%dms', [L.Count, GetTimeTick - tk]);
  L.Free;

  DisposeObject(M64_1);
  DisposeObject(M64_2);
  DisposeObject(Cipher_);
end;

end.
