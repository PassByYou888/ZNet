{ ****************************************************************************** }
{ * ZDB 2.0 automated fragment for Hash Variant support                        * }
{ ****************************************************************************** }
unit Z.ZDB2.HV;

{$I Z.Define.inc}

interface

uses Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status, Z.MemoryStream,
  Z.DFE, Z.ZDB2, Z.Cipher, Z.ListEngine;

type
  TZDB2_List_HashVariant = class;
  TZDB2_HashVariant = class;
  TZDB2_Big_List_HashVariant_Decl__ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<TZDB2_HashVariant>;

  TZDB2_HashVariant = class
  private
    FPool_Ptr: TZDB2_Big_List_HashVariant_Decl__.PQueueStruct;
    FTimeOut: TTimeTick;
    FAlive: TTimeTick;
    FID: Integer;
    FData: THashVariantList;
    FIsChanged: Boolean;
    procedure DoHashVariantChange(Sender: THashVariantList; Name_: SystemString; OLD_, New_: Variant);
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
    function GetData: THashVariantList;
    property Data: THashVariantList read GetData;
    property Data_Direct: THashVariantList read FData;
    property ID: Integer read FID;
  end;

  TZDB2_HashVariant_Class = class of TZDB2_HashVariant;

  TOnCreate_ZDB2_HashVariant = procedure(Sender: TZDB2_List_HashVariant; Obj: TZDB2_HashVariant) of object;

  TZDB2_List_HashVariant = class
  private
    procedure DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
    function GetAutoFreeStream: Boolean;
    procedure SetAutoFreeStream(const Value: Boolean);
    procedure Do_Free(var obj_: TZDB2_HashVariant);
  public
    List: TZDB2_Big_List_HashVariant_Decl__;
    HashVariant_Class: TZDB2_HashVariant_Class;
    TimeOut: TTimeTick;
    DeltaSpace: Int64;
    BlockSize: Word;
    IOHnd: TIOHnd;
    CoreSpace: TZDB2_Core_Space;
    OnCreateClass: TOnCreate_ZDB2_HashVariant;
    constructor Create(HashVariant_Class_: TZDB2_HashVariant_Class; OnCreateClass_: TOnCreate_ZDB2_HashVariant; TimeOut_: TTimeTick;
      Stream_: TCore_Stream; OnlyRead_: Boolean; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
    destructor Destroy; override;
    property AutoFreeStream: Boolean read GetAutoFreeStream write SetAutoFreeStream;
    property IsOnlyRead: Boolean read IOHnd.IsOnlyRead;
    procedure Remove(Obj: TZDB2_HashVariant; RemoveData_: Boolean);
    procedure Clear;
    function NewDataFrom(ID_: Integer): TZDB2_HashVariant; overload;
    function NewData: TZDB2_HashVariant; overload;
    procedure Flush(flush_core_space: Boolean); overload;
    procedure Flush; overload;
    procedure ExtractTo(Stream_: TCore_Stream);
    procedure Progress;
    procedure Push_To_Recycle_Pool(obj_: TZDB2_HashVariant; RemoveData_: Boolean); // remove from repeat
    procedure Free_Recycle_Pool;                                                   // remove from repeat
    function Count: NativeInt;
    function Repeat_: TZDB2_Big_List_HashVariant_Decl__.TRepeat___;               // flow simulate
    function Invert_Repeat_: TZDB2_Big_List_HashVariant_Decl__.TInvert_Repeat___; // flow simulate

    class procedure Test;
  end;

implementation

uses Z.ZDB2.Thread.Queue;

procedure TZDB2_HashVariant.DoHashVariantChange(Sender: THashVariantList; Name_: SystemString; OLD_, New_: Variant);
begin
  FIsChanged := True;
end;

constructor TZDB2_HashVariant.Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer);
begin
  inherited Create;
  FPool_Ptr := nil;
  FTimeOut := 5 * 1000;
  FAlive := GetTimeTick;
  Keep := 0;
  FID := ID_;
  CoreSpace := CoreSpace_;
  FData := nil;
  FIsChanged := False;
end;

destructor TZDB2_HashVariant.Destroy;
begin
  Save;
  inherited Destroy;
end;

procedure TZDB2_HashVariant.Progress;
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

procedure TZDB2_HashVariant.Load;
var
  m64: TZDB2_Mem;
begin
  if FID < 0 then
      exit;
  m64 := TZDB2_Mem.Create;

  if CoreSpace.ReadData(m64, FID) then
    begin
      try
        FData.LoadFromStream(m64.Stream64);
        FIsChanged := False;
      except
        FID := -1;
        FData.Clear;
      end;
    end
  else
      FData.Clear;

  DisposeObject(m64);
end;

procedure TZDB2_HashVariant.Save;
var
  m64: TMS64;
  old_ID: Integer;
begin
  if FData = nil then
      exit;
  if not CoreSpace.Space_IOHnd^.IsOnlyRead then
    begin
      m64 := TMS64.Create;
      try
        if FIsChanged or (FID < 0) then
          begin
            FData.SaveToStream(m64);
            old_ID := FID;
            CoreSpace.WriteData(m64.Mem64, FID, False);
            if old_ID >= 0 then
                CoreSpace.RemoveData(old_ID, False);
          end;
      except
      end;
      DisposeObject(m64);
    end;
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_HashVariant.RecycleMemory;
begin
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_HashVariant.Remove;
begin
  if CoreSpace.Space_IOHnd^.IsOnlyRead then
      exit;
  if FID >= 0 then
      CoreSpace.RemoveData(FID, False);
  DisposeObjectAndNil(FData);
  FID := -1;
end;

function TZDB2_HashVariant.GetData: THashVariantList;
begin
  if FData = nil then
    begin
      FData := THashVariantList.CustomCreate(64);
      FData.OnValueChangeNotify := {$IFDEF FPC}@{$ENDIF FPC}DoHashVariantChange;
      Load;
      FIsChanged := False;
    end;
  Result := FData;
  FAlive := GetTimeTick;
end;

procedure TZDB2_List_HashVariant.DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
begin
  retry := Trigger.AppendSpace(DeltaSpace, BlockSize);
end;

function TZDB2_List_HashVariant.GetAutoFreeStream: Boolean;
begin
  Result := IOHnd.AutoFree;
end;

procedure TZDB2_List_HashVariant.SetAutoFreeStream(const Value: Boolean);
begin
  IOHnd.AutoFree := Value;
end;

procedure TZDB2_List_HashVariant.Do_Free(var obj_: TZDB2_HashVariant);
begin
  DisposeObjectAndNil(obj_);
end;

constructor TZDB2_List_HashVariant.Create(HashVariant_Class_: TZDB2_HashVariant_Class; OnCreateClass_: TOnCreate_ZDB2_HashVariant; TimeOut_: TTimeTick;
  Stream_: TCore_Stream; OnlyRead_: Boolean; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
var
  buff: TZDB2_BlockHandle;
  ID_: Integer;
  m64: TMem64;
begin
  inherited Create;
  List := TZDB2_Big_List_HashVariant_Decl__.Create;
  List.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Do_Free;

  HashVariant_Class := HashVariant_Class_;
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
    if CoreSpace.Check(ID_) then
        NewDataFrom(ID_);
  SetLength(buff, 0);
end;

destructor TZDB2_List_HashVariant.Destroy;
begin
  Flush;
  Clear;
  DisposeObjectAndNil(CoreSpace);
  List.Free;
  inherited Destroy;
end;

procedure TZDB2_List_HashVariant.Remove(Obj: TZDB2_HashVariant; RemoveData_: Boolean);
begin
  if RemoveData_ then
      Obj.Remove;
  List.Remove(Obj.FPool_Ptr);
end;

procedure TZDB2_List_HashVariant.Clear;
begin
  List.Clear;
end;

function TZDB2_List_HashVariant.NewDataFrom(ID_: Integer): TZDB2_HashVariant;
begin
  Result := HashVariant_Class.Create(CoreSpace, ID_);
  Result.FTimeOut := TimeOut;
  Result.FPool_Ptr := List.Add(Result);
  if Assigned(OnCreateClass) then
      OnCreateClass(self, Result);
end;

function TZDB2_List_HashVariant.NewData: TZDB2_HashVariant;
begin
  if IOHnd.IsOnlyRead then
      Result := nil
  else
      Result := NewDataFrom(-1);
end;

procedure TZDB2_List_HashVariant.Flush(flush_core_space: Boolean);
var
  __For__: TZDB2_Big_List_HashVariant_Decl__.TRepeat___;
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

procedure TZDB2_List_HashVariant.Flush;
begin
  Flush(True);
end;

procedure TZDB2_List_HashVariant.ExtractTo(Stream_: TCore_Stream);
var
  TmpIOHnd: TIOHnd;
  TmpSpace: TZDB2_Core_Space;
  __For__: TZDB2_Big_List_HashVariant_Decl__.TRepeat___;
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
  TmpSpace.BuildSpace(CoreSpace.State^.Physics, BlockSize);

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

procedure TZDB2_List_HashVariant.Progress;
var
  __For__: TZDB2_Big_List_HashVariant_Decl__.TRepeat___;
begin
  if List.num > 0 then
    begin
      __For__ := List.Repeat_();
      repeat
          __For__.Queue^.Data.Progress;
      until not __For__.Next;
    end;
end;

procedure TZDB2_List_HashVariant.Push_To_Recycle_Pool(obj_: TZDB2_HashVariant; RemoveData_: Boolean);
begin
  List.Push_To_Recycle_Pool(obj_.FPool_Ptr);
  if RemoveData_ then
      obj_.Remove;
end;

procedure TZDB2_List_HashVariant.Free_Recycle_Pool;
begin
  List.Free_Recycle_Pool;
end;

function TZDB2_List_HashVariant.Count: NativeInt;
begin
  Result := List.num;
end;

function TZDB2_List_HashVariant.Repeat_: TZDB2_Big_List_HashVariant_Decl__.TRepeat___;
begin
  Result := List.Repeat_;
end;

function TZDB2_List_HashVariant.Invert_Repeat_: TZDB2_Big_List_HashVariant_Decl__.TInvert_Repeat___;
begin
  Result := List.Invert_Repeat_;
end;

class procedure TZDB2_List_HashVariant.Test;
var
  Cipher_: TZDB2_Cipher;
  M64_1, M64_2: TMS64;
  i: Integer;
  tmp_HashVariant: TZDB2_HashVariant;
  L: TZDB2_List_HashVariant;
  tk: TTimeTick;
begin
  TCompute.Sleep(5000);
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csRijndael, 'hello world', 1, True, True);
  M64_1 := TMS64.CustomCreate(16 * 1024 * 1024);
  M64_2 := TMS64.CustomCreate(16 * 1024 * 1024);

  tk := GetTimeTick;
  with TZDB2_List_HashVariant.Create(TZDB2_HashVariant, nil, 5000, M64_1, False, 64 * 1048576, 200, Cipher_) do
    begin
      AutoFreeStream := False;
      for i := 0 to 20000 - 1 do
        begin
          tmp_HashVariant := NewData();
          tmp_HashVariant.Data['a' + umlIntToStr(i).Text] := i;
          tmp_HashVariant.Save;
        end;
      DoStatus('build %d of HashVariant,time:%dms', [List.num, GetTimeTick - tk]);
      Free;
    end;

  tk := GetTimeTick;
  L := TZDB2_List_HashVariant.Create(TZDB2_HashVariant, nil, 5000, M64_1, False, 64 * 1048576, 200, Cipher_);
  with L.Repeat_ do
    repeat
      if Queue^.Data.Data['a' + umlIntToStr(I__).Text] <> I__ then
          DoStatus('%s - test error.', [L.ClassName]);
      Queue^.Data.Data['b' + umlIntToStr(I__).Text] := I__;
    until not Next;
  DoStatus('load %d of HashVariant,time:%dms', [L.List.num, GetTimeTick - tk]);
  L.ExtractTo(M64_2);
  L.Free;

  tk := GetTimeTick;
  L := TZDB2_List_HashVariant.Create(TZDB2_HashVariant, nil, 5000, M64_2, False, 64 * 1048576, 200, Cipher_);
  with L.Invert_Repeat_ do
    repeat
      if Queue^.Data.Data['a' + umlIntToStr(I__).Text] <> I__ then
          DoStatus('%s - test error.', [L.ClassName]);
      if Queue^.Data.Data['b' + umlIntToStr(I__).Text] <> I__ then
          DoStatus('%s - test error.', [L.ClassName]);
      if I__ mod 2 = 0 then
          L.Push_To_Recycle_Pool(Queue^.Data, True);
    until not Prev;
  L.Free_Recycle_Pool;
  DoStatus('extract and remove done num=%d, stream of HashVariant,time:%dms', [L.List.num, GetTimeTick - tk]);
  L.Free;

  DisposeObject(M64_1);
  DisposeObject(M64_2);
  DisposeObject(Cipher_);
end;

end.
