{ ****************************************************************************** }
{ * ZDB 2.0 custom                                                             * }
{ ****************************************************************************** }
unit Z.ZDB2.Custom;

{$I Z.Define.inc}

interface

uses Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status, Z.ZDB2, Z.IOThread, Z.MemoryStream, Z.ListEngine;

type
  TZDB2 = class;
  TZDB2_Traversal = class;

  { call }
  TOn_ZDB2_Result_C = procedure(ZSender: TZDB2; UserData: Pointer; ID: Integer; Successed: Boolean);
  TOn_ZDB2_GetData_C = procedure(ZSender: TZDB2; UserData: Pointer; ID: Integer; Mem: TZDB2_Mem; Successed: Boolean);
  TOn_ZDB2_Traversal_C = procedure(ZSender: TZDB2; Traversal: TZDB2_Traversal; Mem: TZDB2_Mem; var Running: Boolean);
  TOn_ZDB2_TraversalDone_C = procedure(ZSender: TZDB2; Traversal: TZDB2_Traversal);

  { method }
  TOn_ZDB2_Result_M = procedure(ZSender: TZDB2; UserData: Pointer; ID: Integer; Successed: Boolean) of object;
  TOn_ZDB2_GetData_M = procedure(ZSender: TZDB2; UserData: Pointer; ID: Integer; Mem: TZDB2_Mem; Successed: Boolean) of object;
  TOn_ZDB2_Traversal_M = procedure(ZSender: TZDB2; Traversal: TZDB2_Traversal; Mem: TZDB2_Mem; var Running: Boolean) of object;
  TOn_ZDB2_TraversalDone_M = procedure(ZSender: TZDB2; Traversal: TZDB2_Traversal) of object;

{$IFDEF FPC}
  { fpc local nested }
  TOn_ZDB2_Result_P = procedure(ZSender: TZDB2; UserData: Pointer; ID: Integer; Successed: Boolean) is nested;
  TOn_ZDB2_GetData_P = procedure(ZSender: TZDB2; UserData: Pointer; ID: Integer; Mem: TZDB2_Mem; Successed: Boolean) is nested;
  TOn_ZDB2_Traversal_P = procedure(ZSender: TZDB2; Traversal: TZDB2_Traversal; Mem: TZDB2_Mem; var Running: Boolean) is nested;
  TOn_ZDB2_TraversalDone_P = procedure(ZSender: TZDB2; Traversal: TZDB2_Traversal) is nested;
{$ELSE FPC}
  { delphi anonymous }
  TOn_ZDB2_Result_P = reference to procedure(ZSender: TZDB2; UserData: Pointer; ID: Integer; Successed: Boolean);
  TOn_ZDB2_GetData_P = reference to procedure(ZSender: TZDB2; UserData: Pointer; ID: Integer; Mem: TZDB2_Mem; Successed: Boolean);
  TOn_ZDB2_Traversal_P = reference to procedure(ZSender: TZDB2; Traversal: TZDB2_Traversal; Mem: TZDB2_Mem; var Running: Boolean);
  TOn_ZDB2_TraversalDone_P = reference to procedure(ZSender: TZDB2; Traversal: TZDB2_Traversal);
{$ENDIF FPC}

  TZDB2_Traversal = class
  private
    FCompletedIndex: TUInt32HashPointerList;
    StartTime: TTimeTick;
    Reverse: Boolean;
    Running: Boolean;
    FMemory: TZDB2_Mem;
    OnTraversal_C: TOn_ZDB2_Traversal_C;
    OnTraversal_M: TOn_ZDB2_Traversal_M;
    OnTraversal_P: TOn_ZDB2_Traversal_P;
    OnDone_C: TOn_ZDB2_TraversalDone_C;
    OnDone_M: TOn_ZDB2_TraversalDone_M;
    OnDone_P: TOn_ZDB2_TraversalDone_P;
    DoneSignal: TAtomBool;

    procedure DoGetDataResult(ZSender: TZDB2; UserData_: Pointer; ID: Integer; Mem_: TZDB2_Mem; Successed: Boolean);
  public
    Total: Integer;
    Current: Integer;
    CompletedNum: Integer;
    LossNum: Integer;
    IOSize: Int64;
    Hnd: TZDB2_BlockHandle;
    UserData: Pointer;

    constructor Create;
    destructor Destroy; override;
    function Timer: TTimeTick;
    function GetCompletedIndex(): TZDB2_BlockHandle;
    function IsCompleted(ID: Integer): Boolean;
  end;

  TZDB2_Traversals = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TZDB2_Traversal>;

  TIDChange = record
    sID, dID: Integer;
    Successed: Boolean;
  end;

  PIDChange = ^TIDChange;
  TIDChanges_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PIDChange>;

  TIDChanges = class(TIDChanges_Decl)
  public
    procedure Clean;
  end;

  TZDB2_NoSpace = (nsRotationWrite, nsAppendDeltaSpace, nsError);

  TZDB2 = class
  private
    FCritical: TCritical;
    FSpace: TZDB2_Core_Space;
    FIndexBuffer: TUInt32HashPointerList;
    FThreadPost: TThreadPost;
    FTraversals: TZDB2_Traversals;
    FRunning, FActivted: TAtomBool;
    FOnCoreProgress: TZDB2_OnProgress;
    FNoSpace: TZDB2_NoSpace;
    FNoSpaceExpansionSize: Int64;
    FNoSpaceExpansionBlockSize: Word;
    FNoSpaceMaxSize: Int64;

    procedure SetOnCoreProgress(const Value: TZDB2_OnProgress);
    procedure DoOnNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
    procedure ReloadIndexPtr;
    procedure LoadIndex;
    function IndexSpaceSize(PhyBlockNum_: Integer): Int64;
    function MakeIndexBuffer(Siz_: Int64): TZDB2_Mem;
    function SaveIndex(Space_: TZDB2_Core_Space): Boolean;
    procedure Cmd_Save();
    procedure Cmd_AppendSpace(Data: Pointer);
    procedure CopyTo_PostResult(ZSender: TZDB2; UserData: Pointer; ID: Integer; Successed: Boolean);
    procedure Cmd_CopyTo(Data: Pointer);
    procedure Cmd_Post(Data: Pointer);
    procedure Cmd_Insert(Data: Pointer);
    procedure Cmd_SyncRemove(Data: Pointer);
    procedure Cmd_Remove(Data: Pointer);
    procedure Cmd_SyncGetData(Data: Pointer);
    procedure Cmd_GetData(Data: Pointer);
    procedure Cmd_SyncWaitQueue(Data: Pointer);
    function TraversalRunning: Boolean;
    procedure AddTraversal(Traversal_: TZDB2_Traversal);
    procedure RemoveTraversal(Traversal_: TZDB2_Traversal);
    procedure ThRun(ThSender: TCompute);
    function GetState: PZDB2_Core_SpaceState;
  public
    property NoSpace: TZDB2_NoSpace read FNoSpace write FNoSpace;
    property NoSpaceExpansionSize: Int64 read FNoSpaceExpansionSize write FNoSpaceExpansionSize;
    property NoSpaceExpansionBlockSize: Word read FNoSpaceExpansionBlockSize write FNoSpaceExpansionBlockSize;
    property NoSpaceMaxSize: Int64 read FNoSpaceMaxSize write FNoSpaceMaxSize;
    property Space: TZDB2_Core_Space read FSpace;
    property OnCoreProgress: TZDB2_OnProgress read FOnCoreProgress write SetOnCoreProgress;
    property State: PZDB2_Core_SpaceState read GetState;

    constructor Create;
    destructor Destroy; override;

    class function Check(Cipher_: IZDB2_Cipher; Stream: TCore_Stream): Boolean; overload;
    class function Check(Stream: TCore_Stream): Boolean; overload;
    class function CheckFile(Cipher_: IZDB2_Cipher; Filename: U_String): Boolean; overload;
    class function CheckFile(Filename: U_String): Boolean; overload;

    procedure NewStream(Cipher_: IZDB2_Cipher; Stream: TCore_Stream; Space_: Int64; BlockSize_: Word; Mode: TZDB2_SpaceMode); overload;
    procedure NewStream(Stream: TCore_Stream; Space_: Int64; BlockSize_: Word; Mode: TZDB2_SpaceMode); overload;
    procedure OpenStream(Cipher_: IZDB2_Cipher; Stream: TCore_Stream; OnlyRead: Boolean; Mode: TZDB2_SpaceMode); overload;
    procedure OpenStream(Stream: TCore_Stream; OnlyRead: Boolean; Mode: TZDB2_SpaceMode); overload;
    procedure NewFile(Cipher_: IZDB2_Cipher; Filename: U_String; Space_: Int64; BlockSize_: Word; Mode: TZDB2_SpaceMode); overload;
    procedure NewFile(Filename: U_String; Space_: Int64; BlockSize_: Word; Mode: TZDB2_SpaceMode); overload;
    procedure OpenFile(Cipher_: IZDB2_Cipher; Filename: U_String; OnlyRead: Boolean; Mode: TZDB2_SpaceMode); overload;
    procedure OpenFile(Filename: U_String; OnlyRead: Boolean; Mode: TZDB2_SpaceMode); overload;

    { save/flush,thread supported }
    procedure Save(Wait_: Boolean);
    { one step signal for Post/insert/Remove/GetData }
    procedure WaitQueue();
    { extract index data,thread supported }
    function GetIndex(): TZDB2_BlockHandle;
    function GetCount: NativeInt;
    property Count: NativeInt read GetCount;

    { append of fixed size space,thread supported }
    procedure AppendSpace(Space_: Int64; BlockSize_: Word);
    { copy and optimize for destroy and rebuild ID structure,thread supported }
    procedure CopyTo(Dest: TZDB2; IDChanges: TIDChanges); overload;
    procedure CopyTo(Dest: TZDB2); overload;
    procedure CopyFrom(Source: TZDB2; IDChanges: TIDChanges); overload;
    procedure CopyFrom(Source: TZDB2); overload;

    { append post data,thread supported }
    procedure Post(Mem: TZDB2_Mem; DoneFreeMem: Boolean);
    procedure PostC(Mem: TZDB2_Mem; DoneFreeMem: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_C);
    procedure PostM(Mem: TZDB2_Mem; DoneFreeMem: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_M);
    procedure PostP(Mem: TZDB2_Mem; DoneFreeMem: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_P);

    { insert post data,thread supported }
    procedure Insert(InsertBeforeIndex: Integer; Mem: TZDB2_Mem; DoneFreeMem: Boolean);
    procedure InsertC(InsertBeforeIndex: Integer; Mem: TZDB2_Mem; DoneFreeMem: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_C);
    procedure InsertM(InsertBeforeIndex: Integer; Mem: TZDB2_Mem; DoneFreeMem: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_M);
    procedure InsertP(InsertBeforeIndex: Integer; Mem: TZDB2_Mem; DoneFreeMem: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_P);

    { remove data,thread supported }
    function Remove(ID: Integer; SafeClean: Boolean): Boolean;
    procedure RemoveC(ID: Integer; SafeClean: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_C);
    procedure RemoveM(ID: Integer; SafeClean: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_M);
    procedure RemoveP(ID: Integer; SafeClean: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_P);

    { get data,thread supported }
    function GetData(ID: Integer; Mem: TZDB2_Mem): Boolean;
    procedure GetDataC(ID: Integer; Mem: TZDB2_Mem; UserData: Pointer; OnResult: TOn_ZDB2_GetData_C);
    procedure GetDataM(ID: Integer; Mem: TZDB2_Mem; UserData: Pointer; OnResult: TOn_ZDB2_GetData_M);
    procedure GetDataP(ID: Integer; Mem: TZDB2_Mem; UserData: Pointer; OnResult: TOn_ZDB2_GetData_P);

    { Traversal,thread supported }
    procedure TraversalC(WaitDone_, Reverse_: Boolean; UserData: Pointer; OnTraversal: TOn_ZDB2_Traversal_C; OnDone: TOn_ZDB2_TraversalDone_C);
    procedure TraversalM(WaitDone_, Reverse_: Boolean; UserData: Pointer; OnTraversal: TOn_ZDB2_Traversal_M; OnDone: TOn_ZDB2_TraversalDone_M);
    procedure TraversalP(WaitDone_, Reverse_: Boolean; UserData: Pointer; OnTraversal: TOn_ZDB2_Traversal_P; OnDone: TOn_ZDB2_TraversalDone_P);

    class procedure Test();
  end;

implementation

type
  TOn_ZDB2_AppendSpace = record
    Space_: Int64;
    BlockSize_: Word;
  end;

  PZDB2_OnAppendSpace = ^TOn_ZDB2_AppendSpace;

  TOn_ZDB2_CopyTo = record
    Dest: TZDB2;
    IDChanges: TIDChanges;
  end;

  PZDB2_OnCopyTo = ^TOn_ZDB2_CopyTo;

  TOn_ZDB2_Post = record
    Mem: TZDB2_Mem;
    DoneFreeMem: Boolean;
    UserData: Pointer;
    On_C: TOn_ZDB2_Result_C;
    On_M: TOn_ZDB2_Result_M;
    On_P: TOn_ZDB2_Result_P;
  end;

  PZDB2_OnPost = ^TOn_ZDB2_Post;

  TOn_ZDB2_Insert = record
    InsertBeforeIndex: Integer;
    Mem: TZDB2_Mem;
    DoneFreeMem: Boolean;
    UserData: Pointer;
    On_C: TOn_ZDB2_Result_C;
    On_M: TOn_ZDB2_Result_M;
    On_P: TOn_ZDB2_Result_P;
  end;

  PZDB2_OnInsert = ^TOn_ZDB2_Insert;

  TOn_ZDB2_SyncRemove = record
    ID: Integer;
    SafeClean: Boolean;
    Successed: Boolean;
    Done: TAtomBool;
  end;

  PZDB2_OnSyncRemove = ^TOn_ZDB2_SyncRemove;

  TOn_ZDB2_Remove = record
    ID: Integer;
    SafeClean: Boolean;
    UserData: Pointer;
    On_C: TOn_ZDB2_Result_C;
    On_M: TOn_ZDB2_Result_M;
    On_P: TOn_ZDB2_Result_P;
  end;

  PZDB2_OnRemove = ^TOn_ZDB2_Remove;

  TOn_ZDB2_SyncGetData = record
    ID: Integer;
    Mem: TZDB2_Mem;
    Successed: Boolean;
    Done: TAtomBool;
  end;

  PZDB2_OnSyncGetData = ^TOn_ZDB2_SyncGetData;

  TOn_ZDB2_GetData = record
    ID: Integer;
    Mem: TZDB2_Mem;
    UserData: Pointer;
    On_C: TOn_ZDB2_GetData_C;
    On_M: TOn_ZDB2_GetData_M;
    On_P: TOn_ZDB2_GetData_P;
  end;

  PZDB2_OnGetData = ^TOn_ZDB2_GetData;

  TOn_ZDB2_SyncWaitQueue = record
    Done: TAtomBool;
  end;

  PZDB2_OnSyncWaitQueue = ^TOn_ZDB2_SyncWaitQueue;

procedure TZDB2_Traversal.DoGetDataResult(ZSender: TZDB2; UserData_: Pointer; ID: Integer; Mem_: TZDB2_Mem; Successed: Boolean);
var
  found_: Boolean;
begin
  UserData := UserData_;
  if Successed then
    begin
      inc(CompletedNum);
      inc(IOSize, Mem_.Size);

      Mem_.Position := 0;
      if Assigned(OnTraversal_C) then
          OnTraversal_C(ZSender, Self, Mem_, Running);

      Mem_.Position := 0;
      if Assigned(OnTraversal_M) then
          OnTraversal_M(ZSender, Self, Mem_, Running);

      Mem_.Position := 0;
      if Assigned(OnTraversal_P) then
          OnTraversal_P(ZSender, Self, Mem_, Running);

      FCompletedIndex.Add(ID, @ZSender.FSpace.BlockBuffer[ID], False);
    end
  else
    begin
      inc(LossNum);
    end;

  if Reverse then
      dec(Current)
  else
      inc(Current);

  if (Current < 0) or (Current >= Total) or (not ZSender.FActivted.V) then
      Running := False;

  if Running then
    begin
      ZSender.GetDataM(Hnd[Current], Mem_, UserData_, {$IFDEF FPC}@{$ENDIF FPC}DoGetDataResult);
    end
  else
    begin
      if Assigned(OnDone_C) then
          OnDone_C(ZSender, Self);
      if Assigned(OnDone_M) then
          OnDone_M(ZSender, Self);
      if Assigned(OnDone_P) then
          OnDone_P(ZSender, Self);

      ZSender.RemoveTraversal(Self);
      DisposeObject(Self);
    end;
end;

constructor TZDB2_Traversal.Create;
begin
  inherited Create;
  // internal
  FCompletedIndex := TUInt32HashPointerList.CustomCreate($FFFF);
  StartTime := GetTimeTick;
  Reverse := False;
  Running := True;
  FMemory := TZDB2_Mem.Create;
  OnTraversal_C := nil;
  OnTraversal_M := nil;
  OnTraversal_P := nil;
  OnDone_C := nil;
  OnDone_M := nil;
  OnDone_P := nil;
  DoneSignal := nil;
  // public
  Total := 0;
  Current := 0;
  CompletedNum := 0;
  LossNum := 0;
  IOSize := 0;
  SetLength(Hnd, 0);
  UserData := nil;
end;

destructor TZDB2_Traversal.Destroy;
begin
  if DoneSignal <> nil then
      DoneSignal.V := True;
  DisposeObject(FCompletedIndex);
  DisposeObject(FMemory);
  SetLength(Hnd, 0);
  inherited Destroy;
end;

function TZDB2_Traversal.Timer: TTimeTick;
begin
  Result := GetTimeTick - StartTime;
end;

function TZDB2_Traversal.GetCompletedIndex(): TZDB2_BlockHandle;
var
  i: NativeInt;
  p: PUInt32HashListPointerStruct;
begin
  SetLength(Result, FCompletedIndex.Count);
  if FCompletedIndex.Count > 0 then
    begin
      i := 0;
      p := FCompletedIndex.FirstPtr;
      while i < FCompletedIndex.Count do
        begin
          Result[i] := p^.u32;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function TZDB2_Traversal.IsCompleted(ID: Integer): Boolean;
begin
  Result := FCompletedIndex.Exists(ID);
end;

procedure TIDChanges.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Dispose(items[i]);
  inherited Clear;
end;

procedure TZDB2.SetOnCoreProgress(const Value: TZDB2_OnProgress);
begin
  FOnCoreProgress := Value;
  if FSpace <> nil then
      FSpace.OnProgress := FOnCoreProgress;
end;

procedure TZDB2.DoOnNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
var
  TK: TTimeTick;
  ID: Integer;
begin
  retry := False;
  if FSpace.Space_IOHnd^.IsOnlyRead then
      exit;

  if (FNoSpace = nsRotationWrite) and ((Siz_ * 2) < FSpace.State^.Physics) then
    begin
      TK := GetTimeTick;
      while FSpace.State^.FreeSpace < Siz_ do
        begin
          if GetTimeTick - TK > 1000 then
              break;
          if FIndexBuffer.Count = 0 then
              break;
          FCritical.Lock;
          if FIndexBuffer.FirstPtr <> nil then
            begin
              ID := FIndexBuffer.FirstPtr^.u32;
              FIndexBuffer.Delete(ID);
            end;
          FCritical.UnLock;
          if not FSpace.RemoveData(ID, False) then
              break;
        end;
      retry := FSpace.State^.FreeSpace >= Siz_;
    end
  else if FNoSpace = nsAppendDeltaSpace then
    begin
      if (FNoSpaceMaxSize <= 0) or (FNoSpaceMaxSize > FSpace.State^.Physics) then
        if FSpace.AppendSpace(FNoSpaceExpansionSize, FNoSpaceExpansionBlockSize) then
          begin
            ReloadIndexPtr;
            retry := True;
          end;
    end
  else if FNoSpace = nsError then
    begin
    end;
end;

procedure TZDB2.ReloadIndexPtr;
var
  i: NativeInt;
  p: PUInt32HashListPointerStruct;
begin
  FCritical.Lock;
  if FIndexBuffer.Count > 0 then
    begin
      i := 0;
      p := FIndexBuffer.FirstPtr;
      while i < FIndexBuffer.Count do
        begin
          p^.Data := @FSpace.BlockBuffer[p^.u32];
          inc(i);
          p := p^.Next;
        end;
    end;
  FCritical.UnLock;
end;

procedure TZDB2.LoadIndex;
var
  ID_: Integer;
  num: Int64;
  Mem: TZDB2_Mem;
  Successed: Boolean;
  i: Integer;
  tmp: TZDB2_BlockHandle;
begin
  FCritical.Lock;
  FIndexBuffer.Clear;
  Mem := TZDB2_Mem.Create;
  Successed := False;
  if FSpace.Check(PInteger(@FSpace.UserCustomHeader^[0])^) then
    if FSpace.ReadData(Mem, PInteger(@FSpace.UserCustomHeader^[0])^) then
      begin
        Mem.Position := 0;
        num := Mem.ReadInt64;
        while FIndexBuffer.Count < num do
          begin
            ID_ := Mem.ReadInt32;
            FIndexBuffer.Add(ID_, @FSpace.BlockBuffer[ID_], False);
          end;
        Successed := True;
      end;
  DisposeObject(Mem);

  if not Successed then
    begin
      // rebuild index
      tmp := FSpace.BuildTableID;
      for i := 0 to length(tmp) - 1 do
          FIndexBuffer.Add(tmp[i], @FSpace.BlockBuffer[tmp[i]], False);
    end;

  FCritical.UnLock;
end;

function TZDB2.IndexSpaceSize(PhyBlockNum_: Integer): Int64;
begin
  Result := 8 + PhyBlockNum_ shl 2;
end;

function TZDB2.MakeIndexBuffer(Siz_: Int64): TZDB2_Mem;
var
  i: NativeInt;
  p: PUInt32HashListPointerStruct;
begin
  Result := TZDB2_Mem.Create;
  Result.Size := Siz_;
  Result.Position := 0;

  FCritical.Lock;
  Result.WriteInt64(FIndexBuffer.Count);
  if FIndexBuffer.Count > 0 then
    begin
      i := 0;
      p := FIndexBuffer.FirstPtr;
      while i < FIndexBuffer.Count do
        begin
          Result.WriteInt32(p^.u32);
          inc(i);
          p := p^.Next;
        end;
    end;
  FCritical.UnLock;
end;

function TZDB2.SaveIndex(Space_: TZDB2_Core_Space): Boolean;
var
  Mem: TZDB2_Mem;
begin
  if Space_.Check(PInteger(@Space_.UserCustomHeader^[0])^) then
      Space_.RemoveData(PInteger(@Space_.UserCustomHeader^[0])^, True);

  Mem := MakeIndexBuffer(IndexSpaceSize(Space_.BlockCount));
  Result := Space_.WriteData(Mem, PInteger(@Space_.UserCustomHeader^[0])^);

  if not Result then
    if Mem.Size >= FSpace.State^.FreeSpace then
      if FSpace.AppendSpace(Mem.Size + FNoSpaceExpansionSize, FNoSpaceExpansionBlockSize) then
          Result := Space_.WriteData(Mem, PInteger(@Space_.UserCustomHeader^[0])^);

  DisposeObject(Mem);
end;

procedure TZDB2.Cmd_Save;
begin
  SaveIndex(FSpace);
  FSpace.Save();
end;

procedure TZDB2.Cmd_AppendSpace(Data: Pointer);
var
  p: PZDB2_OnAppendSpace;
begin
  Cmd_Save;
  p := Data;
  FSpace.AppendSpace(p^.Space_, p^.BlockSize_);
  Dispose(p);
end;

procedure TZDB2.CopyTo_PostResult(ZSender: TZDB2; UserData: Pointer; ID: Integer; Successed: Boolean);
var
  Change_: PIDChange;
begin
  Change_ := UserData;
  Change_^.dID := ID;
  Change_^.Successed := Successed;
end;

procedure TZDB2.Cmd_CopyTo(Data: Pointer);
var
  p: PZDB2_OnCopyTo;
  Hnd: TZDB2_BlockHandle;
  i: Integer;
  Mem: TZDB2_Mem;
  Change_: PIDChange;
begin
  p := Data;
  Hnd := GetIndex;
  for i in Hnd do
    begin
      Mem := TZDB2_Mem.Create;
      if FSpace.ReadData(Mem, i) then
        begin
          if p^.IDChanges <> nil then
            begin
              new(Change_);
              Change_^.sID := i;
              Change_^.dID := -1;
              Change_^.Successed := False;
              p^.IDChanges.Add(Change_);
              p^.Dest.PostM(Mem, True, Change_, {$IFDEF FPC}@{$ENDIF FPC}CopyTo_PostResult)
            end
          else
              p^.Dest.Post(Mem, True);
        end
      else
          DisposeObject(Mem);
    end;
  Dispose(p);
end;

procedure TZDB2.Cmd_Post(Data: Pointer);
var
  p: PZDB2_OnPost;
  ID: Integer;
  Successed: Boolean;
begin
  p := Data;
  Successed := FSpace.WriteData(p^.Mem, ID);
  if Successed then
    begin
      FCritical.Lock;
      FIndexBuffer.Add(ID, @FSpace.BlockBuffer[ID], False);
      FCritical.UnLock;
    end;

  if Assigned(p^.On_C) then
      p^.On_C(Self, p^.UserData, ID, Successed);
  if Assigned(p^.On_M) then
      p^.On_M(Self, p^.UserData, ID, Successed);
  if Assigned(p^.On_P) then
      p^.On_P(Self, p^.UserData, ID, Successed);
  if p^.DoneFreeMem then
      DisposeObject(p^.Mem);
  Dispose(p);
end;

procedure TZDB2.Cmd_Insert(Data: Pointer);
var
  p: PZDB2_OnInsert;
  ID: Integer;
  Successed: Boolean;
begin
  p := Data;
  Successed := FSpace.Check(p^.InsertBeforeIndex) and FSpace.WriteData(p^.Mem, ID);
  if Successed then
    begin
      FCritical.Lock;
      FIndexBuffer.Insert(ID, p^.InsertBeforeIndex, @FSpace.BlockBuffer[ID], False);
      FCritical.UnLock;
    end;

  if Assigned(p^.On_C) then
      p^.On_C(Self, p^.UserData, ID, Successed);
  if Assigned(p^.On_M) then
      p^.On_M(Self, p^.UserData, ID, Successed);
  if Assigned(p^.On_P) then
      p^.On_P(Self, p^.UserData, ID, Successed);
  if p^.DoneFreeMem then
      DisposeObject(p^.Mem);
  Dispose(p);
end;

procedure TZDB2.Cmd_SyncRemove(Data: Pointer);
var
  p: PZDB2_OnSyncRemove;
begin
  p := Data;
  p^.Successed := FSpace.RemoveData(p^.ID, p^.SafeClean);
  if p^.Successed then
    begin
      FCritical.Lock;
      FIndexBuffer.Delete(p^.ID);
      FCritical.UnLock;
    end;
  p^.Done.V := True;
end;

procedure TZDB2.Cmd_Remove(Data: Pointer);
var
  p: PZDB2_OnRemove;
  Successed: Boolean;
begin
  p := Data;
  Successed := FSpace.RemoveData(p^.ID, p^.SafeClean);
  if Successed then
    begin
      FCritical.Lock;
      FIndexBuffer.Delete(p^.ID);
      FCritical.UnLock;
    end;
  if Assigned(p^.On_C) then
      p^.On_C(Self, p^.UserData, p^.ID, Successed);
  if Assigned(p^.On_M) then
      p^.On_M(Self, p^.UserData, p^.ID, Successed);
  if Assigned(p^.On_P) then
      p^.On_P(Self, p^.UserData, p^.ID, Successed);
  Dispose(p);
end;

procedure TZDB2.Cmd_SyncGetData(Data: Pointer);
var
  p: PZDB2_OnSyncGetData;
begin
  p := Data;
  p^.Successed := FSpace.ReadData(p^.Mem, p^.ID);
  p^.Done.V := True;
end;

procedure TZDB2.Cmd_GetData(Data: Pointer);
var
  p: PZDB2_OnGetData;
  Successed: Boolean;
begin
  p := Data;
  Successed := FSpace.ReadData(p^.Mem, p^.ID);
  if Assigned(p^.On_C) then
      p^.On_C(Self, p^.UserData, p^.ID, p^.Mem, Successed);
  if Assigned(p^.On_M) then
      p^.On_M(Self, p^.UserData, p^.ID, p^.Mem, Successed);
  if Assigned(p^.On_P) then
      p^.On_P(Self, p^.UserData, p^.ID, p^.Mem, Successed);
  Dispose(p);
end;

procedure TZDB2.Cmd_SyncWaitQueue(Data: Pointer);
var
  p: PZDB2_OnSyncWaitQueue;
begin
  p := Data;
  p^.Done.V := True;
end;

function TZDB2.TraversalRunning: Boolean;
begin
  FCritical.Lock;
  Result := FTraversals.Count > 0;
  FCritical.UnLock;
end;

procedure TZDB2.AddTraversal(Traversal_: TZDB2_Traversal);
begin
  FCritical.Lock;
  FTraversals.Add(Traversal_);
  FCritical.UnLock;
end;

procedure TZDB2.RemoveTraversal(Traversal_: TZDB2_Traversal);
var
  i: Integer;
begin
  FCritical.Lock;
  i := 0;
  while i < FTraversals.Count do
    if FTraversals[i] = Traversal_ then
        FTraversals.Delete(i)
    else
        inc(i);
  FCritical.UnLock;
end;

procedure TZDB2.ThRun(ThSender: TCompute);
var
  L: Integer;
  LastTK, IdleTK: TTimeTick;
begin
  FThreadPost.ThreadID := ThSender.ThreadID;
  FThreadPost.OneStep := False;
  FThreadPost.ResetRandomSeed := False;

  LastTK := GetTimeTick();
  while FActivted.V or TraversalRunning do
    begin
      L := FThreadPost.Progress(FThreadPost.ThreadID);
      if L > 0 then
          LastTK := GetTimeTick()
      else
        begin
          IdleTK := GetTimeTick() - LastTK;
          if IdleTK > 1000 then
              TCompute.Sleep(1);
        end;
    end;

  DisposeObject(FSpace);
  DisposeObject(FIndexBuffer);
  DisposeObject(FThreadPost);
  DisposeObject(FTraversals);
  FRunning.V := False;
end;

function TZDB2.GetState: PZDB2_Core_SpaceState;
begin
  if FSpace <> nil then
      Result := FSpace.State
  else
      Result := nil;
end;

constructor TZDB2.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FSpace := nil;
  FThreadPost := nil;
  FTraversals := nil;
  FRunning := TAtomBool.Create(False);
  FActivted := TAtomBool.Create(False);
  FOnCoreProgress := nil;
  FNoSpace := nsError;
  FNoSpaceExpansionSize := Int64(16 * 1024 * 1024);
  FNoSpaceExpansionBlockSize := 1024;
  FNoSpaceMaxSize := Int64(500) * Int64(1024 * 1024 * 1024);
end;

destructor TZDB2.Destroy;
begin
  FActivted.V := False;
  while (FRunning.V) do
      CheckThreadSynchronize(1);
  DisposeObject(FActivted);
  DisposeObject(FRunning);
  FCritical.Free;
  inherited Destroy;
end;

class function TZDB2.Check(Cipher_: IZDB2_Cipher; Stream: TCore_Stream): Boolean;
var
  ioHnd: TIOHnd;
  tmp: TZDB2_Core_Space;
begin
  Result := False;
  InitIOHnd(ioHnd);
  if umlFileOpenAsStream('', Stream, ioHnd, True) then
    begin
      tmp := TZDB2_Core_Space.Create(@ioHnd);
      tmp.Cipher := Cipher_;
      Result := tmp.Open;
      DisposeObject(tmp);
    end;
  umlFileClose(ioHnd);
end;

class function TZDB2.Check(Stream: TCore_Stream): Boolean;
begin
  Result := TZDB2.Check(nil, Stream);
end;

class function TZDB2.CheckFile(Cipher_: IZDB2_Cipher; Filename: U_String): Boolean;
var
  ioHnd: TIOHnd;
  tmp: TZDB2_Core_Space;
begin
  Result := False;
  InitIOHnd(ioHnd);
  if umlFileOpen(Filename, ioHnd, True) then
    begin
      tmp := TZDB2_Core_Space.Create(@ioHnd);
      tmp.Cipher := Cipher_;
      Result := tmp.Open;
      DisposeObject(tmp);
    end;
  umlFileClose(ioHnd);
end;

class function TZDB2.CheckFile(Filename: U_String): Boolean;
begin
  Result := TZDB2.CheckFile(nil, Filename);
end;

procedure TZDB2.NewStream(Cipher_: IZDB2_Cipher; Stream: TCore_Stream; Space_: Int64; BlockSize_: Word; Mode: TZDB2_SpaceMode);
var
  P_IO: PIOHnd;
begin
  new(P_IO);
  InitIOHnd(P_IO^);
  umlFileCreateAsStream(Stream, P_IO^);
  FSpace := TZDB2_Core_Space.Create(P_IO);
  FSpace.Cipher := Cipher_;
  FSpace.OnProgress := FOnCoreProgress;
  FSpace.OnNoSpace := {$IFDEF FPC}@{$ENDIF FPC}DoOnNoSpace;
  FSpace.AutoCloseIOHnd := True;
  FSpace.AutoFreeIOHnd := True;
  FSpace.Mode := Mode;
  FSpace.BuildSpace(Space_, BlockSize_);
  FIndexBuffer := TUInt32HashPointerList.CustomCreate(1024 * 1024);
  FIndexBuffer.AccessOptimization := True;
  FThreadPost := TThreadPost.Create(0);
  FTraversals := TZDB2_Traversals.Create;
  FActivted.V := True;
  FRunning.V := True;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}ThRun);
  Save(True);
end;

procedure TZDB2.NewStream(Stream: TCore_Stream; Space_: Int64; BlockSize_: Word; Mode: TZDB2_SpaceMode);
begin
  NewStream(nil, Stream, Space_, BlockSize_, Mode);
end;

procedure TZDB2.OpenStream(Cipher_: IZDB2_Cipher; Stream: TCore_Stream; OnlyRead: Boolean; Mode: TZDB2_SpaceMode);
var
  P_IO: PIOHnd;
begin
  new(P_IO);
  InitIOHnd(P_IO^);
  umlFileCreateAsStream(Stream, P_IO^, OnlyRead);
  FSpace := TZDB2_Core_Space.Create(P_IO);
  FSpace.Cipher := Cipher_;
  FSpace.OnProgress := FOnCoreProgress;
  FSpace.OnNoSpace := {$IFDEF FPC}@{$ENDIF FPC}DoOnNoSpace;
  FSpace.AutoCloseIOHnd := True;
  FSpace.AutoFreeIOHnd := True;
  FSpace.Mode := Mode;
  FIndexBuffer := TUInt32HashPointerList.CustomCreate($FFFF);
  FIndexBuffer.AccessOptimization := True;
  FSpace.Open;
  LoadIndex;
  FThreadPost := TThreadPost.Create(0);
  FTraversals := TZDB2_Traversals.Create;
  FActivted.V := True;
  FRunning.V := True;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}ThRun);
end;

procedure TZDB2.OpenStream(Stream: TCore_Stream; OnlyRead: Boolean; Mode: TZDB2_SpaceMode);
begin
  OpenStream(nil, Stream, OnlyRead, Mode);
end;

procedure TZDB2.NewFile(Cipher_: IZDB2_Cipher; Filename: U_String; Space_: Int64; BlockSize_: Word; Mode: TZDB2_SpaceMode);
var
  P_IO: PIOHnd;
begin
  new(P_IO);
  InitIOHnd(P_IO^);
  umlFileCreate(Filename, P_IO^);
  FSpace := TZDB2_Core_Space.Create(P_IO);
  FSpace.Cipher := Cipher_;
  FSpace.OnProgress := FOnCoreProgress;
  FSpace.OnNoSpace := {$IFDEF FPC}@{$ENDIF FPC}DoOnNoSpace;
  FSpace.AutoCloseIOHnd := True;
  FSpace.AutoFreeIOHnd := True;
  FSpace.Mode := Mode;
  FSpace.BuildSpace(Space_, BlockSize_);
  FIndexBuffer := TUInt32HashPointerList.CustomCreate($FFFF);
  FIndexBuffer.AccessOptimization := True;
  FThreadPost := TThreadPost.Create(0);
  FTraversals := TZDB2_Traversals.Create;
  FActivted.V := True;
  FRunning.V := True;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}ThRun);
  Save(True);
end;

procedure TZDB2.NewFile(Filename: U_String; Space_: Int64; BlockSize_: Word; Mode: TZDB2_SpaceMode);
begin
  NewFile(nil, Filename, Space_, BlockSize_, Mode);
end;

procedure TZDB2.OpenFile(Cipher_: IZDB2_Cipher; Filename: U_String; OnlyRead: Boolean; Mode: TZDB2_SpaceMode);
var
  P_IO: PIOHnd;
begin
  new(P_IO);
  InitIOHnd(P_IO^);
  umlFileOpen(Filename, P_IO^, OnlyRead);
  FSpace := TZDB2_Core_Space.Create(P_IO);
  FSpace.Cipher := Cipher_;
  FSpace.OnProgress := FOnCoreProgress;
  FSpace.OnNoSpace := {$IFDEF FPC}@{$ENDIF FPC}DoOnNoSpace;
  FSpace.AutoCloseIOHnd := True;
  FSpace.AutoFreeIOHnd := True;
  FSpace.Mode := Mode;
  FIndexBuffer := TUInt32HashPointerList.CustomCreate($FFFF);
  FIndexBuffer.AccessOptimization := True;
  FSpace.Open;
  LoadIndex;
  FThreadPost := TThreadPost.Create(0);
  FTraversals := TZDB2_Traversals.Create;
  FActivted.V := True;
  FRunning.V := True;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}ThRun);
end;

procedure TZDB2.OpenFile(Filename: U_String; OnlyRead: Boolean; Mode: TZDB2_SpaceMode);
begin
  OpenFile(nil, Filename, OnlyRead, Mode);
end;

procedure TZDB2.Save(Wait_: Boolean);
begin
  FThreadPost.PostM1({$IFDEF FPC}@{$ENDIF FPC}Cmd_Save);
  if Wait_ then
      WaitQueue();
end;

procedure TZDB2.WaitQueue;
var
  p: PZDB2_OnSyncWaitQueue;
begin
  new(p);
  p^.Done := TAtomBool.Create(False);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_SyncWaitQueue);
  while not p^.Done.V do
      TCompute.Sleep(1);
  DisposeObject(p^.Done);
  Dispose(p);
end;

function TZDB2.GetIndex: TZDB2_BlockHandle;
var
  i: NativeInt;
  p: PUInt32HashListPointerStruct;
begin
  FCritical.Lock;
  SetLength(Result, FIndexBuffer.Count);
  if FIndexBuffer.Count > 0 then
    begin
      i := 0;
      p := FIndexBuffer.FirstPtr;
      while i < FIndexBuffer.Count do
        begin
          Result[i] := p^.u32;
          inc(i);
          p := p^.Next;
        end;
    end;
  FCritical.UnLock;
end;

function TZDB2.GetCount: NativeInt;
begin
  FCritical.Lock;
  Result := FIndexBuffer.Count;
  FCritical.UnLock;
end;

procedure TZDB2.AppendSpace(Space_: Int64; BlockSize_: Word);
var
  p: PZDB2_OnAppendSpace;
begin
  new(p);
  p^.Space_ := Space_;
  p^.BlockSize_ := BlockSize_;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_AppendSpace);
end;

procedure TZDB2.CopyTo(Dest: TZDB2; IDChanges: TIDChanges);
var
  p: PZDB2_OnCopyTo;
begin
  new(p);
  p^.Dest := Dest;
  p^.IDChanges := IDChanges;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_CopyTo);
  WaitQueue();
  Dest.WaitQueue();
end;

procedure TZDB2.CopyTo(Dest: TZDB2);
begin
  CopyTo(Dest, nil);
end;

procedure TZDB2.CopyFrom(Source: TZDB2; IDChanges: TIDChanges);
begin
  Source.CopyTo(Self, IDChanges);
end;

procedure TZDB2.CopyFrom(Source: TZDB2);
begin
  CopyFrom(Source, nil);
end;

procedure TZDB2.Post(Mem: TZDB2_Mem; DoneFreeMem: Boolean);
var
  p: PZDB2_OnPost;
begin
  new(p);
  p^.Mem := Mem;
  p^.DoneFreeMem := DoneFreeMem;
  p^.UserData := nil;
  p^.On_C := nil;
  p^.On_M := nil;
  p^.On_P := nil;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_Post);
end;

procedure TZDB2.PostC(Mem: TZDB2_Mem; DoneFreeMem: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_C);
var
  p: PZDB2_OnPost;
begin
  new(p);
  p^.Mem := Mem;
  p^.DoneFreeMem := DoneFreeMem;
  p^.UserData := UserData;
  p^.On_C := OnResult;
  p^.On_M := nil;
  p^.On_P := nil;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_Post);
end;

procedure TZDB2.PostM(Mem: TZDB2_Mem; DoneFreeMem: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_M);
var
  p: PZDB2_OnPost;
begin
  new(p);
  p^.Mem := Mem;
  p^.DoneFreeMem := DoneFreeMem;
  p^.UserData := UserData;
  p^.On_C := nil;
  p^.On_M := OnResult;
  p^.On_P := nil;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_Post);
end;

procedure TZDB2.PostP(Mem: TZDB2_Mem; DoneFreeMem: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_P);
var
  p: PZDB2_OnPost;
begin
  new(p);
  p^.Mem := Mem;
  p^.DoneFreeMem := DoneFreeMem;
  p^.UserData := UserData;
  p^.On_C := nil;
  p^.On_M := nil;
  p^.On_P := OnResult;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_Post);
end;

procedure TZDB2.Insert(InsertBeforeIndex: Integer; Mem: TZDB2_Mem; DoneFreeMem: Boolean);
var
  p: PZDB2_OnInsert;
begin
  new(p);
  p^.InsertBeforeIndex := InsertBeforeIndex;
  p^.Mem := Mem;
  p^.DoneFreeMem := DoneFreeMem;
  p^.UserData := nil;
  p^.On_C := nil;
  p^.On_M := nil;
  p^.On_P := nil;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_Insert);
end;

procedure TZDB2.InsertC(InsertBeforeIndex: Integer; Mem: TZDB2_Mem; DoneFreeMem: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_C);
var
  p: PZDB2_OnInsert;
begin
  new(p);
  p^.InsertBeforeIndex := InsertBeforeIndex;
  p^.Mem := Mem;
  p^.DoneFreeMem := DoneFreeMem;
  p^.UserData := UserData;
  p^.On_C := OnResult;
  p^.On_M := nil;
  p^.On_P := nil;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_Insert);
end;

procedure TZDB2.InsertM(InsertBeforeIndex: Integer; Mem: TZDB2_Mem; DoneFreeMem: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_M);
var
  p: PZDB2_OnInsert;
begin
  new(p);
  p^.InsertBeforeIndex := InsertBeforeIndex;
  p^.Mem := Mem;
  p^.DoneFreeMem := DoneFreeMem;
  p^.UserData := UserData;
  p^.On_C := nil;
  p^.On_M := OnResult;
  p^.On_P := nil;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_Insert);
end;

procedure TZDB2.InsertP(InsertBeforeIndex: Integer; Mem: TZDB2_Mem; DoneFreeMem: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_P);
var
  p: PZDB2_OnInsert;
begin
  new(p);
  p^.InsertBeforeIndex := InsertBeforeIndex;
  p^.Mem := Mem;
  p^.DoneFreeMem := DoneFreeMem;
  p^.UserData := UserData;
  p^.On_C := nil;
  p^.On_M := nil;
  p^.On_P := OnResult;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_Insert);
end;

function TZDB2.Remove(ID: Integer; SafeClean: Boolean): Boolean;
var
  p: PZDB2_OnSyncRemove;
begin
  new(p);
  p^.ID := ID;
  p^.SafeClean := SafeClean;
  p^.Successed := False;
  p^.Done := TAtomBool.Create(False);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_SyncRemove);
  while not p^.Done.V do
      TCompute.Sleep(1);
  Result := p^.Successed;
  DisposeObject(p^.Done);
  Dispose(p);
end;

procedure TZDB2.RemoveC(ID: Integer; SafeClean: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_C);
var
  p: PZDB2_OnRemove;
begin
  new(p);
  p^.ID := ID;
  p^.SafeClean := SafeClean;
  p^.UserData := UserData;
  p^.On_C := OnResult;
  p^.On_M := nil;
  p^.On_P := nil;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_Remove);
end;

procedure TZDB2.RemoveM(ID: Integer; SafeClean: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_M);
var
  p: PZDB2_OnRemove;
begin
  new(p);
  p^.ID := ID;
  p^.SafeClean := SafeClean;
  p^.UserData := UserData;
  p^.On_C := nil;
  p^.On_M := OnResult;
  p^.On_P := nil;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_Remove);
end;

procedure TZDB2.RemoveP(ID: Integer; SafeClean: Boolean; UserData: Pointer; OnResult: TOn_ZDB2_Result_P);
var
  p: PZDB2_OnRemove;
begin
  new(p);
  p^.ID := ID;
  p^.SafeClean := SafeClean;
  p^.UserData := UserData;
  p^.On_C := nil;
  p^.On_M := nil;
  p^.On_P := OnResult;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_Remove);
end;

function TZDB2.GetData(ID: Integer; Mem: TZDB2_Mem): Boolean;
var
  p: PZDB2_OnSyncGetData;
begin
  new(p);
  p^.ID := ID;
  p^.Mem := Mem;
  p^.Successed := False;
  p^.Done := TAtomBool.Create(False);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_SyncGetData);
  while not p^.Done.V do
      TCompute.Sleep(1);
  Result := p^.Successed;
  DisposeObject(p^.Done);
  Dispose(p);
end;

procedure TZDB2.GetDataC(ID: Integer; Mem: TZDB2_Mem; UserData: Pointer; OnResult: TOn_ZDB2_GetData_C);
var
  p: PZDB2_OnGetData;
begin
  new(p);
  p^.ID := ID;
  p^.Mem := Mem;
  p^.UserData := UserData;
  p^.On_C := OnResult;
  p^.On_M := nil;
  p^.On_P := nil;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_GetData);
end;

procedure TZDB2.GetDataM(ID: Integer; Mem: TZDB2_Mem; UserData: Pointer; OnResult: TOn_ZDB2_GetData_M);
var
  p: PZDB2_OnGetData;
begin
  new(p);
  p^.ID := ID;
  p^.Mem := Mem;
  p^.UserData := UserData;
  p^.On_C := nil;
  p^.On_M := OnResult;
  p^.On_P := nil;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_GetData);
end;

procedure TZDB2.GetDataP(ID: Integer; Mem: TZDB2_Mem; UserData: Pointer; OnResult: TOn_ZDB2_GetData_P);
var
  p: PZDB2_OnGetData;
begin
  new(p);
  p^.ID := ID;
  p^.Mem := Mem;
  p^.UserData := UserData;
  p^.On_C := nil;
  p^.On_M := nil;
  p^.On_P := OnResult;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}Cmd_GetData);
end;

procedure TZDB2.TraversalC(WaitDone_, Reverse_: Boolean; UserData: Pointer; OnTraversal: TOn_ZDB2_Traversal_C; OnDone: TOn_ZDB2_TraversalDone_C);
var
  Traversal: TZDB2_Traversal;
  Done_: TAtomBool;
begin
  Traversal := TZDB2_Traversal.Create;
  Traversal.Reverse := Reverse_;
  Traversal.Hnd := GetIndex();
  Traversal.Total := length(Traversal.Hnd);
  if Reverse_ then
      Traversal.Current := Traversal.Total - 1
  else
      Traversal.Current := 0;
  Traversal.OnTraversal_C := OnTraversal;
  Traversal.OnDone_C := OnDone;

  if (Traversal.Current >= 0) and (Traversal.Current < Traversal.Total) then
    begin
      if WaitDone_ then
        begin
          Done_ := TAtomBool.Create(False);
          Traversal.DoneSignal := Done_;
        end;

      AddTraversal(Traversal);
      GetDataM(Traversal.Hnd[Traversal.Current], Traversal.FMemory, UserData, {$IFDEF FPC}@{$ENDIF FPC}Traversal.DoGetDataResult);

      if WaitDone_ then
        begin
          while not Done_.V do
              TCompute.Sleep(1);
          DisposeObject(Done_);
        end;
    end
  else
    begin
      Traversal.UserData := UserData;
      if Assigned(OnDone) then
          OnDone(Self, Traversal);
      DisposeObject(Traversal);
    end;
end;

procedure TZDB2.TraversalM(WaitDone_, Reverse_: Boolean; UserData: Pointer; OnTraversal: TOn_ZDB2_Traversal_M; OnDone: TOn_ZDB2_TraversalDone_M);
var
  Traversal: TZDB2_Traversal;
  Done_: TAtomBool;
begin
  Traversal := TZDB2_Traversal.Create;
  Traversal.Reverse := Reverse_;
  Traversal.Hnd := GetIndex();
  Traversal.Total := length(Traversal.Hnd);
  if Reverse_ then
      Traversal.Current := Traversal.Total - 1
  else
      Traversal.Current := 0;
  Traversal.OnTraversal_M := OnTraversal;
  Traversal.OnDone_M := OnDone;

  if (Traversal.Current >= 0) and (Traversal.Current < Traversal.Total) then
    begin
      if WaitDone_ then
        begin
          Done_ := TAtomBool.Create(False);
          Traversal.DoneSignal := Done_;
        end;

      AddTraversal(Traversal);
      GetDataM(Traversal.Hnd[Traversal.Current], Traversal.FMemory, UserData, {$IFDEF FPC}@{$ENDIF FPC}Traversal.DoGetDataResult);

      if WaitDone_ then
        begin
          while not Done_.V do
              TCompute.Sleep(1);
          DisposeObject(Done_);
        end;
    end
  else
    begin
      Traversal.UserData := UserData;
      if Assigned(OnDone) then
          OnDone(Self, Traversal);
      DisposeObject(Traversal);
    end;
end;

procedure TZDB2.TraversalP(WaitDone_, Reverse_: Boolean; UserData: Pointer; OnTraversal: TOn_ZDB2_Traversal_P; OnDone: TOn_ZDB2_TraversalDone_P);
var
  Traversal: TZDB2_Traversal;
  Done_: TAtomBool;
begin
  Traversal := TZDB2_Traversal.Create;
  Traversal.Reverse := Reverse_;
  Traversal.Hnd := GetIndex();
  Traversal.Total := length(Traversal.Hnd);
  if Reverse_ then
      Traversal.Current := Traversal.Total - 1
  else
      Traversal.Current := 0;
  Traversal.OnTraversal_P := OnTraversal;
  Traversal.OnDone_P := OnDone;

  if (Traversal.Current >= 0) and (Traversal.Current < Traversal.Total) then
    begin
      if WaitDone_ then
        begin
          Done_ := TAtomBool.Create(False);
          Traversal.DoneSignal := Done_;
        end;

      AddTraversal(Traversal);
      GetDataM(Traversal.Hnd[Traversal.Current], Traversal.FMemory, UserData, {$IFDEF FPC}@{$ENDIF FPC}Traversal.DoGetDataResult);

      if WaitDone_ then
        begin
          while not Done_.V do
              TCompute.Sleep(1);
          DisposeObject(Done_);
        end;
    end
  else
    begin
      Traversal.UserData := UserData;
      if Assigned(OnDone) then
          OnDone(Self, Traversal);
      DisposeObject(Traversal);
    end;
end;

class procedure TZDB2.Test;
var
  Mem1, Mem2: TStream64;
  tmp: TZDB2_Mem;
  db1, db2, db3, db4: TZDB2;
  i: Integer;
begin
  Mem1 := TStream64.Create;
  Mem2 := TStream64.Create;
  db1 := TZDB2.Create;
  db1.NewStream(Mem1, 1024 * 1024 * 10, $FF, smBigData);
  DisposeObject(db1);

  db2 := TZDB2.Create;
  db2.NoSpace := nsRotationWrite;
  db2.OpenStream(Mem1, False, smBigData);
  for i := 0 to 100000 - 1 do
    begin
      tmp := TZDB2_Mem.Create;
      tmp.Size := umlRandomRange($40, 512);
      MT19937Rand32(MaxInt, tmp.Memory, tmp.Size div 4);
      db2.Post(tmp, True);
    end;
  db2.WaitQueue;
  db2.AppendSpace(1024 * 1024 * 1, $FF);
  db2.WaitQueue;
  db2.Save(True);
  db2.TraversalC(True, False, nil, nil, nil);
  db2.TraversalM(True, False, nil, nil, nil);
  db2.TraversalP(True, False, nil, nil, nil);
  DisposeObject(db2);

  TZDB2.Check(Mem1);
  db1 := TZDB2.Create;
  db1.OpenStream(Mem1, False, smBigData);

  db3 := TZDB2.Create;
  db3.NewStream(Mem1, 1024 * 1024 * 10, $FF, smBigData);
  db3.CopyFrom(db1);
  db3.TraversalC(True, False, nil, nil, nil);
  db3.TraversalM(True, False, nil, nil, nil);
  db3.TraversalP(True, False, nil, nil, nil);
  DisposeObject(db1);
  DisposeObject(db3);

  DisposeObject(Mem1);
  DisposeObject(Mem2);
  DoStatus('TZDB2.Test passed.');
end;

initialization

finalization

end.
