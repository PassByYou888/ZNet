{ ****************************************************************************** }
{ * ZDB 2.0 automated fragment for TObjectDataManager support                  * }
{ ****************************************************************************** }
unit Z.ZDB2.ObjectDataManager;

{$I Z.Define.inc}

interface

uses Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status, Z.MemoryStream,
  Z.ZDB, Z.ZDB.ObjectData_LIB,
  Z.ZDB2, Z.Cipher, Z.ListEngine;

type
  TZDB2_List_ObjectDataManager = class;

  TZDB2_ObjectDataManager = class
  private
    FTimeOut: TTimeTick;
    FAlive: TTimeTick;
    FID: Integer;
    FData: TObjectDataManager;
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
    function GetData: TObjectDataManager;
    property Data: TObjectDataManager read GetData;
    property Data_Direct: TObjectDataManager read FData;
    property ID: Integer read FID;
  end;

  TZDB2_ObjectDataManager_Class = class of TZDB2_ObjectDataManager;

  TZDB2_List_ObjectDataManager_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TZDB2_ObjectDataManager>;

  TOnCreate_ZDB2_ObjectDataManager = procedure(Sender: TZDB2_List_ObjectDataManager; Obj: TZDB2_ObjectDataManager) of object;

  TZDB2_List_ObjectDataManager = class(TZDB2_List_ObjectDataManager_Decl)
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
    ObjectDataManager_Class: TZDB2_ObjectDataManager_Class;
    TimeOut: TTimeTick;
    DeltaSpace: Int64;
    BlockSize: Word;
    IOHnd: TIOHnd;
    CoreSpace: TZDB2_Core_Space;
    OnCreateClass: TOnCreate_ZDB2_ObjectDataManager;
    constructor Create(ObjectDataManager_Class_: TZDB2_ObjectDataManager_Class; OnCreateClass_: TOnCreate_ZDB2_ObjectDataManager; TimeOut_: TTimeTick;
      Stream_: TCore_Stream; OnlyRead_: Boolean; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
    destructor Destroy; override;
    property AutoFreeStream: Boolean read GetAutoFreeStream write SetAutoFreeStream;
    property IsOnlyRead: Boolean read IOHnd.IsOnlyRead;
    procedure Remove(Obj: TZDB2_ObjectDataManager; RemoveData_: Boolean);
    procedure Delete(Index: Integer; RemoveData_: Boolean);
    procedure Clear(RemoveData_: Boolean);
    function NewDataFrom(ID_: Integer): TZDB2_ObjectDataManager; overload;
    function NewData: TZDB2_ObjectDataManager; overload;
    procedure Flush(flush_core_space: Boolean); overload;
    procedure Flush; overload;
    procedure ExtractTo(Stream_: TCore_Stream);
    procedure Progress;

    class procedure Test;
  end;

implementation

constructor TZDB2_ObjectDataManager.Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer);
begin
  inherited Create;
  FTimeOut := 5 * 1000;
  FAlive := GetTimeTick;
  Keep := 0;
  FID := ID_;
  CoreSpace := CoreSpace_;
  FData := nil;
end;

destructor TZDB2_ObjectDataManager.Destroy;
begin
  Save;
  inherited Destroy;
end;

procedure TZDB2_ObjectDataManager.Progress;
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

procedure TZDB2_ObjectDataManager.Load;
var
  m64: TMS64;
begin
  if FID < 0 then
      exit;
  m64 := TMS64.CustomCreate(8 * 1024);

  if CoreSpace.ReadStream(m64, FID) then
    begin
      try
          FData := TObjectDataManager.CreateAsStream(m64, '', DBMarshal.ID, False, False, True);
      except
      end;
    end
  else
    begin
      DisposeObject(m64);
    end;
end;

procedure TZDB2_ObjectDataManager.Save;
var
  m64: TMS64;
  old_ID: Integer;
begin
  if FData = nil then
      exit;
  if not CoreSpace.Space_IOHnd^.IsOnlyRead then
    begin
      try
        if FData.Modification or (FID < 0) then
          begin
            m64 := TMS64.CustomCreate(1024 * 1024);
            FData.SaveToStream(m64);
            old_ID := FID;
            CoreSpace.WriteData(m64.Mem64, FID, False);
            if old_ID >= 0 then
                CoreSpace.RemoveData(old_ID, False);
            DisposeObject(m64);
          end;
      except
      end;
    end;
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_ObjectDataManager.RecycleMemory;
begin
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_ObjectDataManager.Remove;
begin
  if CoreSpace.Space_IOHnd^.IsOnlyRead then
      exit;
  if FID >= 0 then
      CoreSpace.RemoveData(FID, False);
  DisposeObjectAndNil(FData);
  FID := -1;
end;

function TZDB2_ObjectDataManager.GetData: TObjectDataManager;
begin
  if FData = nil then
    begin
      if FID >= 0 then
          Load
      else
          FData := TObjectDataManager.CreateAsStream($FF, TMS64.CustomCreate(8 * 1024), '', DBMarshal.ID, False, True, True);
    end;
  Result := FData;
  FAlive := GetTimeTick;
end;

procedure TZDB2_List_ObjectDataManager.DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
begin
  retry := Trigger.AppendSpace(DeltaSpace, BlockSize);
end;

function TZDB2_List_ObjectDataManager.GetAutoFreeStream: Boolean;
begin
  Result := IOHnd.AutoFree;
end;

procedure TZDB2_List_ObjectDataManager.SetAutoFreeStream(const Value: Boolean);
begin
  IOHnd.AutoFree := Value;
end;

constructor TZDB2_List_ObjectDataManager.Create(ObjectDataManager_Class_: TZDB2_ObjectDataManager_Class; OnCreateClass_: TOnCreate_ZDB2_ObjectDataManager; TimeOut_: TTimeTick;
  Stream_: TCore_Stream; OnlyRead_: Boolean; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
var
  buff: TZDB2_BlockHandle;
  ID_: Integer;
  m64: TMem64;
begin
  inherited Create;
  ObjectDataManager_Class := ObjectDataManager_Class_;
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

destructor TZDB2_List_ObjectDataManager.Destroy;
begin
  Flush;
  Clear(False);
  DisposeObjectAndNil(CoreSpace);
  inherited Destroy;
end;

procedure TZDB2_List_ObjectDataManager.Remove(Obj: TZDB2_ObjectDataManager; RemoveData_: Boolean);
begin
  if IOHnd.IsOnlyRead then
      exit;
  if RemoveData_ then
      Obj.Remove;
  DisposeObject(Obj);
  inherited Remove(Obj);
end;

procedure TZDB2_List_ObjectDataManager.Delete(Index: Integer; RemoveData_: Boolean);
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

procedure TZDB2_List_ObjectDataManager.Clear(RemoveData_: Boolean);
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

function TZDB2_List_ObjectDataManager.NewDataFrom(ID_: Integer): TZDB2_ObjectDataManager;
begin
  Result := ObjectDataManager_Class.Create(CoreSpace, ID_);
  Result.FTimeOut := TimeOut;
  Add(Result);
  if Assigned(OnCreateClass) then
      OnCreateClass(self, Result);
end;

function TZDB2_List_ObjectDataManager.NewData: TZDB2_ObjectDataManager;
begin
  if IOHnd.IsOnlyRead then
      Result := nil
  else
      Result := NewDataFrom(-1);
end;

procedure TZDB2_List_ObjectDataManager.Flush(flush_core_space: Boolean);
var
  sum_: Integer;
  buff: TZDB2_BlockHandle;
  m64: TMem64;
  i, j: Integer;
begin
  if IOHnd.IsOnlyRead then
      exit;
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

procedure TZDB2_List_ObjectDataManager.Flush;
begin
  Flush(True);
end;

procedure TZDB2_List_ObjectDataManager.ExtractTo(Stream_: TCore_Stream);
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

procedure TZDB2_List_ObjectDataManager.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
end;

class procedure TZDB2_List_ObjectDataManager.Test;
var
  Cipher_: TZDB2_Cipher;
  M64_1, M64_2: TMS64;
  i: Integer;
  tmp: TZDB2_ObjectDataManager;
  L: TZDB2_List_ObjectDataManager;
  tk: TTimeTick;
begin
  TCompute.Sleep(5000);
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csRijndael, 'hello world', 1, True, True);
  M64_1 := TMS64.CustomCreate(16 * 1024 * 1024);
  M64_2 := TMS64.CustomCreate(16 * 1024 * 1024);

  tk := GetTimeTick;
  with TZDB2_List_ObjectDataManager.Create(TZDB2_ObjectDataManager, nil, 5000, M64_1, False, 64 * 1048576, 200, Cipher_) do
    begin
      AutoFreeStream := False;
      for i := 1 to 20000 do
        begin
          tmp := NewData();
          tmp.Data.CreateField(PFormat('/%d', [i - 1]), '');
          tmp.Save;
        end;
      DoStatus('build %d of ObjectDataManager,time:%dms', [Count, GetTimeTick - tk]);
      Free;
    end;

  tk := GetTimeTick;
  L := TZDB2_List_ObjectDataManager.Create(TZDB2_ObjectDataManager, nil, 5000, M64_1, False, 64 * 1048576, 200, Cipher_);
  for i := 0 to L.Count - 1 do
    begin
      if not L[i].Data.FieldExists(PFormat('/%d', [i])) then
          DoStatus('%s - test error.', [L.ClassName]);
    end;
  DoStatus('load %d of ObjectDataManager,time:%dms', [L.Count, GetTimeTick - tk]);
  L.ExtractTo(M64_2);
  L.Free;

  tk := GetTimeTick;
  L := TZDB2_List_ObjectDataManager.Create(TZDB2_ObjectDataManager, nil, 5000, M64_2, False, 64 * 1048576, 200, Cipher_);
  for i := 0 to L.Count - 1 do
    begin
      if not L[i].Data.FieldExists(PFormat('/%d', [i])) then
          DoStatus('%s - test error.', [L.ClassName]);
    end;
  DoStatus('load %d extract stream of ObjectDataManager,time:%dms', [L.Count, GetTimeTick - tk]);
  L.Free;

  DisposeObject(M64_1);
  DisposeObject(M64_2);
  DisposeObject(Cipher_);
end;

end.
