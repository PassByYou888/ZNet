{ ****************************************************************************** }
{ * ZDB 2.0 automated fragment for Json support                                * }
{ ****************************************************************************** }
unit Z.ZDB2.Json;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status, Z.MemoryStream,
  Z.DFE, Z.ZDB2, Z.Cipher, Z.Json;

type
  TZDB2_List_Json = class;
  TZDB2_Json = class;
  TZDB2_Big_List_Json_Decl__ = TBigList<TZDB2_Json>;

  TZDB2_Json = class(TCore_Object_Intermediate)
  private
    FPool_Ptr: TZDB2_Big_List_Json_Decl__.PQueueStruct;
    FTimeOut: TTimeTick;
    FAlive: TTimeTick;
    FID: Integer;
    FData: TZJ;
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
    function GetData: TZJ;
    property Data: TZJ read GetData;
    property Data_Direct: TZJ read FData;
    property ID: Integer read FID;
    property Data_MD5: TMD5 read FData_MD5;
  end;

  TZDB2_Json_Class = class of TZDB2_Json;

  TOnCreate_ZDB2_Json = procedure(Sender: TZDB2_List_Json; Obj: TZDB2_Json) of object;

  TZDB2_List_Json = class(TCore_Object_Intermediate)
  private
    procedure DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
    function GetAutoFreeStream: Boolean;
    procedure SetAutoFreeStream(const Value: Boolean);
    procedure Do_Free(var obj_: TZDB2_Json);
  public
    List: TZDB2_Big_List_Json_Decl__;
    Json_Class: TZDB2_Json_Class;
    TimeOut: TTimeTick;
    DeltaSpace: Int64;
    BlockSize: Word;
    IOHnd: TIOHnd;
    CoreSpace: TZDB2_Core_Space;
    OnCreateClass: TOnCreate_ZDB2_Json;
    constructor Create(Json_Class_: TZDB2_Json_Class; OnCreateClass_: TOnCreate_ZDB2_Json; TimeOut_: TTimeTick;
      Stream_: TCore_Stream; OnlyRead_: Boolean; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
    destructor Destroy; override;
    property AutoFreeStream: Boolean read GetAutoFreeStream write SetAutoFreeStream;
    property IsOnlyRead: Boolean read IOHnd.IsOnlyRead;
    procedure Remove(Obj: TZDB2_Json; RemoveData_: Boolean);
    procedure Clear;
    function NewDataFrom(ID_: Integer): TZDB2_Json; overload;
    function NewData: TZDB2_Json; overload;
    procedure Flush(flush_core_space: Boolean); overload;
    procedure Flush; overload;
    procedure ExtractTo(Stream_: TCore_Stream);
    procedure Progress;
    procedure Push_To_Recycle_Pool(obj_: TZDB2_Json; RemoveData_: Boolean); // remove from repeat
    procedure Free_Recycle_Pool; // remove from repeat
    function Count: NativeInt;
    function Repeat_: TZDB2_Big_List_Json_Decl__.TRepeat___; // flow simulate
    function Invert_Repeat_: TZDB2_Big_List_Json_Decl__.TInvert_Repeat___; // flow simulate

    class procedure Test;
  end;

implementation

uses Z.ZDB2.Thread.Queue;

constructor TZDB2_Json.Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer);
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

destructor TZDB2_Json.Destroy;
begin
  Save;
  inherited Destroy;
end;

procedure TZDB2_Json.Progress;
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

procedure TZDB2_Json.Load;
var
  m64: TZDB2_Mem;
begin
  FData_MD5 := NullMD5;
  if FID < 0 then
      exit;
  m64 := TZDB2_Mem.Create;

  if CoreSpace.ReadData(m64, FID) then
    begin
      FData_MD5 := umlMD5(m64.Memory, m64.Size);
      try
          FData.LoadFromStream(m64.Stream64);
      except
        FID := -1;
        FData.Clear;
        DoStatus('failed load Json');
        DoStatus(m64.Memory, m64.Size, 80);
        DoStatus('');
      end;
    end
  else
      FData.Clear;

  DisposeObject(m64);
end;

procedure TZDB2_Json.Save;
var
  m64: TMS64;
  tmp_md5: TMD5;
  old_ID: Integer;
begin
  if FData = nil then
      exit;
  if not CoreSpace.Space_IOHnd^.IsOnlyRead then
    begin
      m64 := TMS64.Create;
      try
        FData.SaveToStream(m64, False);
        tmp_md5 := umlMD5(m64.Memory, m64.Size);
        if umlMD5Compare(FData_MD5, NullMD5) or (not umlMD5Compare(tmp_md5, FData_MD5)) or (FID < 0) then
          begin
            old_ID := FID;
            CoreSpace.WriteData(m64.Mem64, FID, False);
            FData_MD5 := tmp_md5;
            if old_ID >= 0 then
                CoreSpace.RemoveData(old_ID, False);
          end;
      except
      end;
      DisposeObject(m64);
    end;
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_Json.RecycleMemory;
begin
  DisposeObjectAndNil(FData);
end;

procedure TZDB2_Json.Remove;
begin
  if CoreSpace.Space_IOHnd^.IsOnlyRead then
      exit;
  if FID >= 0 then
      CoreSpace.RemoveData(FID, False);
  DisposeObjectAndNil(FData);
  FID := -1;
  FData_MD5 := NullMD5;
end;

function TZDB2_Json.GetData: TZJ;
begin
  if FData = nil then
    begin
      FData := TZJ.Create;
      Load;
    end;
  Result := FData;
  FAlive := GetTimeTick;
end;

procedure TZDB2_List_Json.DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
begin
  retry := Trigger.Fast_AppendSpace(DeltaSpace, BlockSize);
end;

function TZDB2_List_Json.GetAutoFreeStream: Boolean;
begin
  Result := IOHnd.AutoFree;
end;

procedure TZDB2_List_Json.SetAutoFreeStream(const Value: Boolean);
begin
  IOHnd.AutoFree := Value;
end;

procedure TZDB2_List_Json.Do_Free(var obj_: TZDB2_Json);
begin
  DisposeObjectAndNil(obj_);
end;

constructor TZDB2_List_Json.Create(Json_Class_: TZDB2_Json_Class; OnCreateClass_: TOnCreate_ZDB2_Json; TimeOut_: TTimeTick;
  Stream_: TCore_Stream; OnlyRead_: Boolean; DeltaSpace_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
var
  buff: TZDB2_BlockHandle;
  ID_: Integer;
  m64: TMem64;
begin
  inherited Create;
  List := TZDB2_Big_List_Json_Decl__.Create;
  List.OnFree := Do_Free;

  Json_Class := Json_Class_;
  TimeOut := TimeOut_;
  DeltaSpace := DeltaSpace_;
  BlockSize := BlockSize_;
  InitIOHnd(IOHnd);
  umlFileCreateAsStream(Stream_, IOHnd, OnlyRead_);
  CoreSpace := TZDB2_Core_Space.Create(@IOHnd);
  CoreSpace.Cipher := Cipher_;
  CoreSpace.Mode := smNormal;
  CoreSpace.AutoCloseIOHnd := True;
  CoreSpace.OnNoSpace := DoNoSpace;
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

destructor TZDB2_List_Json.Destroy;
begin
  Flush;
  Clear;
  DisposeObjectAndNil(CoreSpace);
  List.Free;
  inherited Destroy;
end;

procedure TZDB2_List_Json.Remove(Obj: TZDB2_Json; RemoveData_: Boolean);
begin
  if RemoveData_ then
      Obj.Remove;
  List.Remove_P(Obj.FPool_Ptr);
end;

procedure TZDB2_List_Json.Clear;
begin
  List.Clear;
end;

function TZDB2_List_Json.NewDataFrom(ID_: Integer): TZDB2_Json;
begin
  Result := Json_Class.Create(CoreSpace, ID_);
  Result.FTimeOut := TimeOut;
  Result.FPool_Ptr := List.Add(Result);
  if Assigned(OnCreateClass) then
      OnCreateClass(self, Result);
end;

function TZDB2_List_Json.NewData: TZDB2_Json;
begin
  if IOHnd.IsOnlyRead then
      Result := nil
  else
      Result := NewDataFrom(-1);
end;

procedure TZDB2_List_Json.Flush(flush_core_space: Boolean);
var
  __For__: TZDB2_Big_List_Json_Decl__.TRepeat___;
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

procedure TZDB2_List_Json.Flush;
begin
  Flush(True);
end;

procedure TZDB2_List_Json.ExtractTo(Stream_: TCore_Stream);
var
  TmpIOHnd: TIOHnd;
  TmpSpace: TZDB2_Core_Space;
  __For__: TZDB2_Big_List_Json_Decl__.TRepeat___;
  buff: TZDB2_BlockHandle;
  m64: TMem64;
begin
  Flush(False);
  InitIOHnd(TmpIOHnd);
  umlFileCreateAsStream(Stream_, TmpIOHnd);
  TmpSpace := TZDB2_Core_Space.Create(@TmpIOHnd);
  TmpSpace.Cipher := CoreSpace.Cipher;
  TmpSpace.Mode := smBigData;
  TmpSpace.OnNoSpace := DoNoSpace;

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

procedure TZDB2_List_Json.Progress;
var
  __For__: TZDB2_Big_List_Json_Decl__.TRepeat___;
begin
  if List.num > 0 then
    begin
      __For__ := List.Repeat_();
      repeat
          __For__.Queue^.Data.Progress;
      until not __For__.Next;
    end;
end;

procedure TZDB2_List_Json.Push_To_Recycle_Pool(obj_: TZDB2_Json; RemoveData_: Boolean);
begin
  List.Push_To_Recycle_Pool(obj_.FPool_Ptr);
  if RemoveData_ then
      obj_.Remove;
end;

procedure TZDB2_List_Json.Free_Recycle_Pool;
begin
  List.Free_Recycle_Pool;
end;

function TZDB2_List_Json.Count: NativeInt;
begin
  Result := List.num;
end;

function TZDB2_List_Json.Repeat_: TZDB2_Big_List_Json_Decl__.TRepeat___;
begin
  Result := List.Repeat_;
end;

function TZDB2_List_Json.Invert_Repeat_: TZDB2_Big_List_Json_Decl__.TInvert_Repeat___;
begin
  Result := List.Invert_Repeat_;
end;

class procedure TZDB2_List_Json.Test;
var
  Cipher_: TZDB2_Cipher;
  M64_1, M64_2: TMS64;
  i: Integer;
  tmp_Json: TZDB2_Json;
  L: TZDB2_List_Json;
  tk: TTimeTick;
begin
  TCompute.Sleep(1000);
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csRijndael, 'hello world', 1, True, True);
  M64_1 := TMS64.CustomCreate(1 * 1024 * 1024);
  M64_2 := TMS64.CustomCreate(1 * 1024 * 1024);

  tk := GetTimeTick;
  with TZDB2_List_Json.Create(TZDB2_Json, nil, 5000, M64_1, False, 64 * 1048576, 200, Cipher_) do
    begin
      AutoFreeStream := False;
      for i := 0 to 2000 - 1 do
        begin
          tmp_Json := NewData();
          tmp_Json.Data.S['a' + umlIntToStr(i).Text] := 'abcdefg' + umlIntToStr(i).Text;
          tmp_Json.Save;
        end;
      DoStatus('build %d of Json,time:%dms', [List.num, GetTimeTick - tk]);
      Free;
    end;

  tk := GetTimeTick;
  L := TZDB2_List_Json.Create(TZDB2_Json, nil, 5000, M64_1, False, 64 * 1048576, 200, Cipher_);
  with L.Repeat_ do
    repeat
      if Queue^.Data.Data.S['a' + umlIntToStr(I__).Text] <> 'abcdefg' + umlIntToStr(I__).Text then
          DoStatus('%s - test error.', [L.ClassName]);
      Queue^.Data.Data.S['x' + umlIntToStr(I__).Text] := 'abc_12345_' + umlIntToStr(I__).Text;
    until not Next;
  DoStatus('load %d of Json,time:%dms', [L.List.num, GetTimeTick - tk]);
  L.ExtractTo(M64_2);
  L.Free;

  tk := GetTimeTick;
  L := TZDB2_List_Json.Create(TZDB2_Json, nil, 5000, M64_2, False, 64 * 1048576, 200, Cipher_);
  with L.Invert_Repeat_ do
    repeat
      if Queue^.Data.Data.S['a' + umlIntToStr(I__).Text] <> 'abcdefg' + umlIntToStr(I__).Text then
          DoStatus('%s - test error.', [L.ClassName]);
      if Queue^.Data.Data.S['x' + umlIntToStr(I__).Text] <> 'abc_12345_' + umlIntToStr(I__).Text then
          DoStatus('%s - test error.', [L.ClassName]);
      if I__ mod 2 = 0 then
          L.Push_To_Recycle_Pool(Queue^.Data, True);
    until not Prev;
  L.Free_Recycle_Pool;
  DoStatus('extract and remove done num=%d, stream of Json,time:%dms', [L.List.num, GetTimeTick - tk]);
  L.Free;

  DisposeObject(M64_1);
  DisposeObject(M64_2);
  DisposeObject(Cipher_);
end;

end.
