{ ****************************************************************************** }
{ * ZDB 2.0 automated fragment for TMem64 support                              * }
{ ****************************************************************************** }
unit Z.ZDB2.MEM64;

{$I Z.Define.inc}

interface

uses Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status, Z.MemoryStream,
  Z.DFE, Z.ZDB2, Z.Cipher;

type
  TZDB2_List_Mem64 = class;
  TZDB2_Mem64 = class;
  TZDB2_Big_List_Mem64_Decl__ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<TZDB2_Mem64>;

  TZDB2_Mem64 = class
  private
    FPool_Ptr: TZDB2_Big_List_Mem64_Decl__.PQueueStruct;
    FTimeOut: TTimeTick;
    FAlive: TTimeTick;
    FID: Integer;
    FData: TMem64;
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
    function GetData: TMem64;
    property Data: TMem64 read GetData;
    property Data_Direct: TMem64 read FData;
    property ID: Integer read FID;
    property Data_MD5: TMD5 read FData_MD5;
  end;

  TZDB2_Mem64_Class = class of TZDB2_Mem64;

  TOnCreate_ZDB2_Mem64 = procedure(Sender: TZDB2_List_Mem64; Obj: TZDB2_Mem64) of object;

  TZDB2_List_Mem64 = class
  private
    procedure DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
    function GetAutoFreeStream: Boolean;
    procedure SetAutoFreeStream(const Value: Boolean);
    procedure Do_Free(var obj_: TZDB2_Mem64);
  public
    List: TZDB2_Big_List_Mem64_Decl__;
    Mem64_Class: TZDB2_Mem64_Class;
    TimeOut: TTimeTick;
    DeltaSpace: Int64;
    BlockSize: Word;
    IOHnd: TIOHnd;
    CoreSpace: TZDB2_Core_Space;
    OnCreateClass: TOnCreate_ZDB2_Mem64;
    constructor Create(Mem64_Class_: TZDB2_Mem64_Class; OnCreateClass_: TOnCreate_ZDB2_Mem64; TimeOut_: TTimeTick;
      Stream_: TCore_Stream; OnlyRead_: Boolean; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
    destructor Destroy; override;
    property AutoFreeStream: Boolean read GetAutoFreeStream write SetAutoFreeStream;
    property IsOnlyRead: Boolean read IOHnd.IsOnlyRead;
    procedure Remove(Obj: TZDB2_Mem64; RemoveData_: Boolean);
    procedure Clear;
    function NewDataFrom(ID_: Integer): TZDB2_Mem64; overload;
    function NewData: TZDB2_Mem64; overload;
    procedure Flush(flush_core_space: Boolean); overload;
    procedure Flush; overload;
    procedure ExtractTo(Stream_: TCore_Stream);
    procedure Progress;
    procedure Push_To_Recycle_Pool(obj_: TZDB2_Mem64; RemoveData_: Boolean); // remove from repeat
    procedure Free_Recycle_Pool;                                             // remove from repeat
    function Count: NativeInt;
    function Repeat_: TZDB2_Big_List_Mem64_Decl__.TRepeat___;               // flow simulate
    function Invert_Repeat_: TZDB2_Big_List_Mem64_Decl__.TInvert_Repeat___; // flow simulate

    class procedure Test;
  end;

implementation

uses Z.ZDB2.Thread.Queue;

constructor TZDB2_Mem64.Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer);
begin
  inherited Create;
  FPool_Ptr := nil;
  FTimeOut := 5 * 1000;
  FAlive := GetTimeTick;
  Keep := 0;
  FID := ID_;
  CoreSpace := CoreSpace_;
  FData := nil;
  FData_MD5 := NullMD5;
end;

destructor TZDB2_Mem64.Destroy;
begin
  Save;
  inherited Destroy;
end;

procedure TZDB2_Mem64.Progress;
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

procedure TZDB2_Mem64.Load;
begin
  FData_MD5 := NullMD5;
  if FID < 0 then
      exit;
  FData.Clear;
  try
    if CoreSpace.ReadData(FData, FID) then
      begin
        FData_MD5 := umlMD5(FData.Memory, FData.Size);
      end
    else
        FData.Clear;
  except
    FID := -1;
    FData.Clear;
  end;
end;

procedure TZDB2_Mem64.Save;
var
  tmp_md5: TMD5;
  old_ID: Integer;
begin
  if FData = nil then
      exit;
  if not CoreSpace.Space_IOHnd^.IsOnlyRead then
    begin
      try
        tmp_md5 := umlMD5(FData.Memory, FData.Size);
        if umlMD5Compare(FData_MD5, NullMD5) or (not umlMD5Compare(tmp_md5, FData_MD5)) or (FID < 0) then
          begin
            old_ID := FID;
            CoreSpace.WriteData(FData, FID, False);
            FData_MD5 := tmp_md5;
            if old_ID >= 0 then
                CoreSpace.RemoveData(old_ID, False);
          end;
      except
      end;
    end;
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_Mem64.RecycleMemory;
begin
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_Mem64.Remove;
begin
  if CoreSpace.Space_IOHnd^.IsOnlyRead then
      exit;
  if FID >= 0 then
      CoreSpace.RemoveData(FID, False);
  DisposeObjectAndNil(FData);
  FID := -1;
  FData_MD5 := NullMD5;
end;

function TZDB2_Mem64.GetData: TMem64;
begin
  if FData = nil then
    begin
      FData := TMem64.Create;
      Load;
    end;
  Result := FData;
  FAlive := GetTimeTick;
end;

procedure TZDB2_List_Mem64.DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
begin
  retry := Trigger.AppendSpace(DeltaSpace, BlockSize);
end;

function TZDB2_List_Mem64.GetAutoFreeStream: Boolean;
begin
  Result := IOHnd.AutoFree;
end;

procedure TZDB2_List_Mem64.SetAutoFreeStream(const Value: Boolean);
begin
  IOHnd.AutoFree := Value;
end;

procedure TZDB2_List_Mem64.Do_Free(var obj_: TZDB2_Mem64);
begin
  DisposeObjectAndNil(obj_);
end;

constructor TZDB2_List_Mem64.Create(Mem64_Class_: TZDB2_Mem64_Class; OnCreateClass_: TOnCreate_ZDB2_Mem64; TimeOut_: TTimeTick;
  Stream_: TCore_Stream; OnlyRead_: Boolean; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
var
  buff: TZDB2_BlockHandle;
  ID_: Integer;
  m64: TMem64;
begin
  inherited Create;
  List := TZDB2_Big_List_Mem64_Decl__.Create;
  List.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Do_Free;

  Mem64_Class := Mem64_Class_;
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

destructor TZDB2_List_Mem64.Destroy;
begin
  Flush;
  Clear;
  DisposeObjectAndNil(CoreSpace);
  List.Free;
  inherited Destroy;
end;

procedure TZDB2_List_Mem64.Remove(Obj: TZDB2_Mem64; RemoveData_: Boolean);
begin
  if RemoveData_ then
      Obj.Remove;
  List.Remove_P(Obj.FPool_Ptr);
end;

procedure TZDB2_List_Mem64.Clear;
begin
  List.Clear;
end;

function TZDB2_List_Mem64.NewDataFrom(ID_: Integer): TZDB2_Mem64;
begin
  Result := Mem64_Class.Create(CoreSpace, ID_);
  Result.FTimeOut := TimeOut;
  Result.FPool_Ptr := List.Add(Result);
  if Assigned(OnCreateClass) then
      OnCreateClass(self, Result);
end;

function TZDB2_List_Mem64.NewData: TZDB2_Mem64;
begin
  if IOHnd.IsOnlyRead then
      Result := nil
  else
      Result := NewDataFrom(-1);
end;

procedure TZDB2_List_Mem64.Flush(flush_core_space: Boolean);
var
  __For__: TZDB2_Big_List_Mem64_Decl__.TRepeat___;
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

procedure TZDB2_List_Mem64.Flush;
begin
  Flush(True);
end;

procedure TZDB2_List_Mem64.ExtractTo(Stream_: TCore_Stream);
var
  TmpIOHnd: TIOHnd;
  TmpSpace: TZDB2_Core_Space;
  __For__: TZDB2_Big_List_Mem64_Decl__.TRepeat___;
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

procedure TZDB2_List_Mem64.Progress;
var
  __For__: TZDB2_Big_List_Mem64_Decl__.TRepeat___;
begin
  if List.num > 0 then
    begin
      __For__ := List.Repeat_();
      repeat
          __For__.Queue^.Data.Progress;
      until not __For__.Next;
    end;
end;

procedure TZDB2_List_Mem64.Push_To_Recycle_Pool(obj_: TZDB2_Mem64; RemoveData_: Boolean);
begin
  List.Push_To_Recycle_Pool(obj_.FPool_Ptr);
  if RemoveData_ then
      obj_.Remove;
end;

procedure TZDB2_List_Mem64.Free_Recycle_Pool;
begin
  List.Free_Recycle_Pool;
end;

function TZDB2_List_Mem64.Count: NativeInt;
begin
  Result := List.num;
end;

function TZDB2_List_Mem64.Repeat_: TZDB2_Big_List_Mem64_Decl__.TRepeat___;
begin
  Result := List.Repeat_;
end;

function TZDB2_List_Mem64.Invert_Repeat_: TZDB2_Big_List_Mem64_Decl__.TInvert_Repeat___;
begin
  Result := List.Invert_Repeat_;
end;

class procedure TZDB2_List_Mem64.Test;
var
  Cipher_: TZDB2_Cipher;
  M64_1, M64_2: TMS64;
  i: Integer;
  tmp_Mem64: TZDB2_Mem64;
  L: TZDB2_List_Mem64;
  tk: TTimeTick;
begin
  TCompute.Sleep(5000);
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csRijndael, 'hello world', 1, True, True);
  M64_1 := TMS64.CustomCreate(16 * 1024 * 1024);
  M64_2 := TMS64.CustomCreate(16 * 1024 * 1024);

  tk := GetTimeTick;
  with TZDB2_List_Mem64.Create(TZDB2_Mem64, nil, 5000, M64_1, False, 64 * 1048576, 200, Cipher_) do
    begin
      AutoFreeStream := False;
      for i := 0 to 20000 - 1 do
        begin
          tmp_Mem64 := NewData();
          tmp_Mem64.Data.WriteString('a' + umlIntToStr(i).Text);
          tmp_Mem64.Save;
        end;
      DoStatus('build %d of Mem64,time:%dms', [List.num, GetTimeTick - tk]);
      Free;
    end;

  tk := GetTimeTick;
  L := TZDB2_List_Mem64.Create(TZDB2_Mem64, nil, 5000, M64_1, False, 64 * 1048576, 200, Cipher_);
  with L.Repeat_ do
    repeat
      if Queue^.Data.Data.ReadString <> 'a' + umlIntToStr(I__).Text then
          DoStatus('%s - test error.', [L.ClassName]);
      Queue^.Data.Data.WriteString('x' + umlIntToStr(I__).Text);
    until not Next;
  DoStatus('load %d of Mem64,time:%dms', [L.List.num, GetTimeTick - tk]);
  L.ExtractTo(M64_2);
  L.Free;

  tk := GetTimeTick;
  L := TZDB2_List_Mem64.Create(TZDB2_Mem64, nil, 5000, M64_2, False, 64 * 1048576, 200, Cipher_);
  with L.Invert_Repeat_ do
    repeat
      if Queue^.Data.Data.ReadString <> 'a' + umlIntToStr(I__).Text then
          DoStatus('%s - test error.', [L.ClassName]);
      if Queue^.Data.Data.ReadString <> 'x' + umlIntToStr(I__).Text then
          DoStatus('%s - test error.', [L.ClassName]);
      if I__ mod 2 = 0 then
          L.Push_To_Recycle_Pool(Queue^.Data, True);
    until not Prev;
  L.Free_Recycle_Pool;
  DoStatus('extract and remove done num=%d, stream of Mem64,time:%dms', [L.List.num, GetTimeTick - tk]);
  L.Free;

  DisposeObject(M64_1);
  DisposeObject(M64_2);
  DisposeObject(Cipher_);
end;

end.
