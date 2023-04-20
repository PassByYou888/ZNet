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
  TZDB2_ObjectDataManager = class;
  TZDB2_Big_List_ObjectDataManager_Decl__ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<TZDB2_ObjectDataManager>;

  TZDB2_ObjectDataManager = class
  private
    FPool_Ptr: TZDB2_Big_List_ObjectDataManager_Decl__.PQueueStruct;
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

  TOnCreate_ZDB2_ObjectDataManager = procedure(Sender: TZDB2_List_ObjectDataManager; Obj: TZDB2_ObjectDataManager) of object;

  TZDB2_List_ObjectDataManager = class
  private
    procedure DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
    function GetAutoFreeStream: Boolean;
    procedure SetAutoFreeStream(const Value: Boolean);
    procedure Do_Free(var obj_: TZDB2_ObjectDataManager);
  public
    List: TZDB2_Big_List_ObjectDataManager_Decl__;
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
    procedure Clear;
    function NewDataFrom(ID_: Integer): TZDB2_ObjectDataManager; overload;
    function NewData: TZDB2_ObjectDataManager; overload;
    procedure Flush(flush_core_space: Boolean); overload;
    procedure Flush; overload;
    procedure ExtractTo(Stream_: TCore_Stream);
    procedure Progress;
    procedure Push_To_Recycle_Pool(obj_: TZDB2_ObjectDataManager; RemoveData_: Boolean); // remove from repeat
    procedure Free_Recycle_Pool;                                                         // remove from repeat
    function Count: NativeInt;
    function Repeat_: TZDB2_Big_List_ObjectDataManager_Decl__.TRepeat___;               // flow simulate
    function Invert_Repeat_: TZDB2_Big_List_ObjectDataManager_Decl__.TInvert_Repeat___; // flow simulate

    class procedure Test;
  end;

implementation

uses Z.ZDB2.Thread.Queue;

constructor TZDB2_ObjectDataManager.Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer);
begin
  inherited Create;
  FPool_Ptr := nil;
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
{$IFDEF SHOW_ZDB2_Data_Free_LOG}
      DoStatus('%s -> %s Space Recycle ID %s size:%d', [UnitName, ClassName, CoreSpace.GetSpaceHndAsText(FID).Text, CoreSpace.GetDataSize(FID)]);
{$ENDIF SHOW_ZDB2_Data_Free_LOG}
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

procedure TZDB2_List_ObjectDataManager.Do_Free(var obj_: TZDB2_ObjectDataManager);
begin
  DisposeObjectAndNil(obj_);
end;

constructor TZDB2_List_ObjectDataManager.Create(ObjectDataManager_Class_: TZDB2_ObjectDataManager_Class; OnCreateClass_: TOnCreate_ZDB2_ObjectDataManager; TimeOut_: TTimeTick;
  Stream_: TCore_Stream; OnlyRead_: Boolean; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
var
  buff: TZDB2_BlockHandle;
  ID_: Integer;
  m64: TMem64;
begin
  inherited Create;
  List := TZDB2_Big_List_ObjectDataManager_Decl__.Create;
  List.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Do_Free;

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

  if (PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.Identifier = $FFFF) and
    CoreSpace.Check(PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.ID) then
    begin
      m64 := TMem64.Create;
      CoreSpace.ReadData(m64, PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.ID);
      SetLength(buff, m64.Size shr 2);
      if length(buff) > 0 then
          CopyPtr(m64.Memory, @buff[0], length(buff) shl 2);
      DisposeObject(m64);
      CoreSpace.RemoveData(PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.ID, False);
      FillPtr(@CoreSpace.UserCustomHeader^[0], SizeOf(TSequence_Table_Head), 0);
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
  Clear;
  DisposeObjectAndNil(CoreSpace);
  List.Free;
  inherited Destroy;
end;

procedure TZDB2_List_ObjectDataManager.Remove(Obj: TZDB2_ObjectDataManager; RemoveData_: Boolean);
begin
  if RemoveData_ then
      Obj.Remove;
  List.Remove_P(Obj.FPool_Ptr);
end;

procedure TZDB2_List_ObjectDataManager.Clear;
begin
  List.Clear;
end;

function TZDB2_List_ObjectDataManager.NewDataFrom(ID_: Integer): TZDB2_ObjectDataManager;
begin
  Result := ObjectDataManager_Class.Create(CoreSpace, ID_);
  Result.FTimeOut := TimeOut;
  Result.FPool_Ptr := List.Add(Result);
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
  __For__: TZDB2_Big_List_ObjectDataManager_Decl__.TRepeat___;
  buff: TZDB2_BlockHandle;
  m64: TMem64;
begin
  if IOHnd.IsOnlyRead then
      exit;

  if (PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.Identifier = $FFFF) and
    CoreSpace.Check(PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.ID) then
    begin
      CoreSpace.RemoveData(PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.ID, False);
      FillPtr(@CoreSpace.UserCustomHeader^[0], SizeOf(TSequence_Table_Head), 0);
    end;

  if List.num > 0 then
    begin
      __For__ := List.Repeat_;
      repeat
        if (__For__.Queue^.Data.FID < 0) and (__For__.Queue^.Data.FData = nil) then
          begin
            List.Push_To_Recycle_Pool(__For__.Queue);
            DisposeObjectAndNil(__For__.Queue^.Data);
          end
        else
            __For__.Queue^.Data.Save;
      until not __For__.Next;
      List.Free_Recycle_Pool;
    end;

  if List.num > 0 then
    begin
      // remove invalid
      SetLength(buff, List.num);

      __For__ := List.Repeat_;
      repeat
          buff[__For__.I__] := __For__.Queue^.Data.FID;
      until not __For__.Next;

      // store
      if flush_core_space then
        begin
          m64 := TMem64.Create;
          m64.Mapping(@buff[0], length(buff) shl 2);
          PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.Identifier := $FFFF;
          CoreSpace.WriteData(m64, PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.ID, False);
          DisposeObject(m64);
          SetLength(buff, 0);
        end;
    end
  else
    begin
      FillPtr(@CoreSpace.UserCustomHeader^[0], SizeOf(TSequence_Table_Head), 0);
    end;

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
  __For__: TZDB2_Big_List_ObjectDataManager_Decl__.TRepeat___;
  buff: TZDB2_BlockHandle;
  m64: TMem64;
begin
  Flush(False);
  InitIOHnd(TmpIOHnd);
  umlFileCreateAsStream(Stream_, TmpIOHnd);
  TmpSpace := TZDB2_Core_Space.Create(@TmpIOHnd);
  TmpSpace.Cipher := CoreSpace.Cipher;
  TmpSpace.Mode := smBigData;
  TmpSpace.OnNoSpace := {$IFDEF FPC}@{$ENDIF FPC}DoNoSpace;

  if List.num > 0 then
    begin
      SetLength(buff, List.num);
      __For__ := List.Repeat_();
      repeat
        m64 := TMem64.Create;
        if CoreSpace.ReadData(m64, __For__.Queue^.Data.FID) then
          if not TmpSpace.WriteData(m64, buff[__For__.I__], False) then
              RaiseInfo('error');
        DisposeObject(m64);
        CoreSpace.DoProgress(List.num - 1, __For__.I__);
      until not __For__.Next;

      m64 := TMem64.Create;
      m64.Mapping(@buff[0], length(buff) shl 2);
      PSequence_Table_Head(@TmpSpace.UserCustomHeader^[0])^.Identifier := $FFFF;
      TmpSpace.WriteData(m64, PSequence_Table_Head(@TmpSpace.UserCustomHeader^[0])^.ID, False);
      DisposeObject(m64);
      SetLength(buff, 0);
    end
  else
      FillPtr(@TmpSpace.UserCustomHeader^[0], SizeOf(TSequence_Table_Head), 0);

  TmpSpace.Save;
  DisposeObject(TmpSpace);
end;

procedure TZDB2_List_ObjectDataManager.Progress;
var
  __For__: TZDB2_Big_List_ObjectDataManager_Decl__.TRepeat___;
begin
  if List.num > 0 then
    begin
      __For__ := List.Repeat_();
      repeat
          __For__.Queue^.Data.Progress;
      until not __For__.Next;
    end;
end;

procedure TZDB2_List_ObjectDataManager.Push_To_Recycle_Pool(obj_: TZDB2_ObjectDataManager; RemoveData_: Boolean);
begin
  List.Push_To_Recycle_Pool(obj_.FPool_Ptr);
  if RemoveData_ then
      obj_.Remove;
end;

procedure TZDB2_List_ObjectDataManager.Free_Recycle_Pool;
begin
  List.Free_Recycle_Pool;
end;

function TZDB2_List_ObjectDataManager.Count: NativeInt;
begin
  Result := List.num;
end;

function TZDB2_List_ObjectDataManager.Repeat_: TZDB2_Big_List_ObjectDataManager_Decl__.TRepeat___;
begin
  Result := List.Repeat_;
end;

function TZDB2_List_ObjectDataManager.Invert_Repeat_: TZDB2_Big_List_ObjectDataManager_Decl__.TInvert_Repeat___;
begin
  Result := List.Invert_Repeat_;
end;

class procedure TZDB2_List_ObjectDataManager.Test;
var
  Cipher_: TZDB2_Cipher;
  M64_1, M64_2: TMS64;
  i: Integer;
  tmp_ObjectDataManager: TZDB2_ObjectDataManager;
  L: TZDB2_List_ObjectDataManager;
  tk: TTimeTick;
begin
  TCompute.Sleep(1000);
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csRijndael, 'hello world', 1, True, True);
  M64_1 := TMS64.CustomCreate(1 * 1024 * 1024);
  M64_2 := TMS64.CustomCreate(1 * 1024 * 1024);

  tk := GetTimeTick;
  with TZDB2_List_ObjectDataManager.Create(TZDB2_ObjectDataManager, nil, 5000, M64_1, False, 64 * 1048576, 200, Cipher_) do
    begin
      AutoFreeStream := False;
      for i := 0 to 200 - 1 do
        begin
          tmp_ObjectDataManager := NewData();
          tmp_ObjectDataManager.Data.CreateField(PFormat('/%d', [i]), '');
          tmp_ObjectDataManager.Save;
        end;
      DoStatus('build %d of ObjectDataManager,time:%dms', [List.num, GetTimeTick - tk]);
      Free;
    end;

  tk := GetTimeTick;
  L := TZDB2_List_ObjectDataManager.Create(TZDB2_ObjectDataManager, nil, 5000, M64_1, False, 64 * 1048576, 200, Cipher_);
  with L.Repeat_ do
    repeat
      if not Queue^.Data.Data.FieldExists(PFormat('/%d', [I__])) then
          DoStatus('%s - test error. no exists %d', [L.ClassName, I__]);
      Queue^.Data.Data.CreateField(PFormat('/%d_ttt', [I__]), '');
    until not Next;
  DoStatus('load %d of ObjectDataManager,time:%dms', [L.List.num, GetTimeTick - tk]);
  L.ExtractTo(M64_2);
  L.Free;

  tk := GetTimeTick;
  L := TZDB2_List_ObjectDataManager.Create(TZDB2_ObjectDataManager, nil, 5000, M64_2, False, 64 * 1048576, 200, Cipher_);
  with L.Invert_Repeat_ do
    repeat
      if not Queue^.Data.Data.FieldExists(PFormat('/%d', [I__])) then
          DoStatus('%s - test error.', [L.ClassName]);
      if not Queue^.Data.Data.FieldExists(PFormat('/%d_ttt', [I__])) then
          DoStatus('%s - test error.', [L.ClassName]);
      if I__ mod 2 = 0 then
          L.Push_To_Recycle_Pool(Queue^.Data, True);
    until not Prev;
  L.Free_Recycle_Pool;
  DoStatus('extract and remove done num=%d, stream of ObjectDataManager,time:%dms', [L.List.num, GetTimeTick - tk]);
  L.Free;

  DisposeObject(M64_1);
  DisposeObject(M64_2);
  DisposeObject(Cipher_);
end;

end.
