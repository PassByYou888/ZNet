{ ****************************************************************************** }
{ * ZDB 2.0 Core-Thread for HPC                                                * }
{ ****************************************************************************** }
unit Z.ZDB2.Thread;

{$I Z.Define.inc}

interface

uses DateUtils, SysUtils,
  Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ELSE FPC}
  System.IOUtils,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib,
  Z.MemoryStream,
  Z.Status, Z.Cipher, Z.ZDB2, Z.ListEngine, Z.TextDataEngine, Z.IOThread,
  Z.HashList.Templet,
  Z.Notify, Z.ZDB2.Thread.Queue;

type
  TZDB2_Th_Engine_Marshal = class;
  TZDB2_Th_Engine_Data = class;
  TZDB2_Th_Engine = class;
  TZDB2_Th_Engine_Data_BigList___ = {$IFDEF FPC}specialize {$ENDIF FPC} TCritical_BigList<TZDB2_Th_Engine_Data>;
  TZDB2_Th_Engine_Marshal_BigList___ = {$IFDEF FPC}specialize {$ENDIF FPC} TCritical_BigList<TZDB2_Th_Engine_Data>;
  TZDB2_Th_Engine_Data_Instance_Recycle_Tool___ = {$IFDEF FPC}specialize {$ENDIF FPC} TCritical_BigList<TZDB2_Th_Engine_Data>;
  TZDB2_Th_Engine_Data_Link_Recycle_Tool___ = {$IFDEF FPC}specialize {$ENDIF FPC} TCritical_BigList<TZDB2_Th_Engine_Data>;

  TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge = class
  public
    Source: TZDB2_Th_Engine_Data;
    OnResult_C: TOn_Mem64_And_State_Event_C;
    OnResult_M: TOn_Mem64_And_State_Event_M;
    OnResult_P: TOn_Mem64_And_State_Event_P;
    constructor Create;
    procedure Do_Result(var Sender: TZDB2_Th_CMD_Mem64_And_State);
  end;

  TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge = class
  public
    Source: TZDB2_Th_Engine_Data;
    OnResult_C: TOn_Stream_And_State_Event_C;
    OnResult_M: TOn_Stream_And_State_Event_M;
    OnResult_P: TOn_Stream_And_State_Event_P;
    constructor Create;
    procedure Do_Result(var Sender: TZDB2_Th_CMD_Stream_And_State);
  end;

  TZDB2_Th_Engine_Data = class(TCore_Object)
  private
    FOwner: TZDB2_Th_Engine_Marshal; // marshal
    FOwner_Data_Ptr: TZDB2_Th_Engine_Marshal_BigList___.PQueueStruct; // marshal data ptr
    FTh_Engine: TZDB2_Th_Engine; // engine
    FTh_Engine_Data_Ptr: TZDB2_Th_Engine_Data_BigList___.PQueueStruct; // engine data ptr
    FID: Integer; // FTh_Engine data ID
    FInstance_Busy: Integer; // instance user support
    FLocked: Boolean; // lock
    FAsync_Load_Num: Integer; // async Load number
    FAsync_Save_Num: Integer; // async save number
    FPost_Free_Runing: Boolean; // free check
    FLoad_Data_Error: Boolean; // last load state
    procedure Wait_Unlock(timeOut: TTimeTick); overload;
    procedure Do_Async_Save_Result(var Sender: TZDB2_Th_CMD_ID_And_State);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Progress(); virtual;
    // lock
    procedure Lock; virtual;
    procedure UnLock; virtual;
    property is_Locked: Boolean read FLocked;
    function is_UnLocked: Boolean;
    procedure Update_Instance_As_Busy();
    procedure Update_Instance_As_Free();
    procedure Reset_Instance_As_Free();
    // state
    function IsOnlyRead: Boolean;
    function Engine: TZDB2_Th_Queue;
    property ID: Integer read FID;
    function Can_Load: Boolean;
    function Can_Progress: Boolean;
    function Can_Free: Boolean;
    // position
    procedure MoveToLast;
    procedure MoveToFirst;
    // async delete and delay free, file is only do remove memory
    procedure Remove(Delete_Data_: Boolean); overload;
    procedure Remove(); overload;
    // sync load.
    function Load_Data(Source: TMS64): Boolean; overload;
    function Load_Data(Source: TMem64): Boolean; overload;
    // async load
    procedure Async_Load_Data_C(Source: TMS64; OnResult: TOn_Stream_And_State_Event_C); overload;
    procedure Async_Load_Data_C(Source: TMem64; OnResult: TOn_Mem64_And_State_Event_C); overload;
    procedure Async_Load_Data_M(Source: TMS64; OnResult: TOn_Stream_And_State_Event_M); overload;
    procedure Async_Load_Data_M(Source: TMem64; OnResult: TOn_Mem64_And_State_Event_M); overload;
    procedure Async_Load_Data_P(Source: TMS64; OnResult: TOn_Stream_And_State_Event_P); overload;
    procedure Async_Load_Data_P(Source: TMem64; OnResult: TOn_Mem64_And_State_Event_P); overload;
    // sync save
    function Save_Data(Source: TMS64): Boolean; overload;
    function Save_Data(Source: TMem64): Boolean; overload;
    // async save
    procedure Async_Save_And_Free_Data(Source: TMS64); overload;
    procedure Async_Save_And_Free_Data(Source: TMem64); overload;
    procedure Async_Save(Source: TMS64); overload;
    procedure Async_Save(Source: TMem64); overload;
    // state update
    procedure Update_State_Loading_Error; // if loading error then remove it.
  end;

  TZDB2_Th_Engine_Data_Class = class of TZDB2_Th_Engine_Data;

  TZDB2_Th_Engine_Backup = class
  public
    Owner: TZDB2_Th_Engine;
    Queue_ID_List_: TZDB2_ID_List;
    backup_file: U_String;
    constructor Create(Owner_: TZDB2_Th_Engine);
    destructor Destroy; override;
    procedure Do_Run(Sender: TCompute);
  end;

  // this multithreaded model.
  // Try to avoid calling the methods here at the application
  TZDB2_Th_Engine = class(TCore_InterfacedObject)
  private
    FBackup_Is_Busy: Boolean;
    FBackup_Directory: U_String; // the current database directory will be used if the backup directory is empty
    procedure DoFree(var Data: TZDB2_Th_Engine_Data);
  public
    Name: U_String;
    Owner: TZDB2_Th_Engine_Marshal;
    RemoveDatabaseOnDestroy: Boolean;
    Mode: TZDB2_SpaceMode;
    Database_File: U_String;
    OnlyRead: Boolean;
    Delta: Int64;
    BlockSize: Word;
    Cipher: TZDB2_Cipher;
    Cipher_Security: TCipherSecurity;
    Cipher_password: U_String;
    Cipher_Level: Integer;
    Cipher_Tail: Boolean;
    Cipher_CBC: Boolean;
    Engine: TZDB2_Th_Queue; // th-queue-engine
    Th_Engine_Data_Pool: TZDB2_Th_Engine_Data_BigList___;
    Last_Build_Class: TZDB2_Th_Engine_Data_Class;
    constructor Create(Owner_: TZDB2_Th_Engine_Marshal); virtual;
    destructor Destroy; override;
    procedure ReadConfig(Name_: U_String; cfg: THashStringList); overload;
    procedure ReadConfig(cfg: THashStringList); overload;
    procedure WriteConfig(cfg: THashStringList);
    procedure Update_Engine_Data_Ptr(); // reset FTh_Engine and FTh_Engine_Data_Ptr
    procedure Clear;
    procedure Format_Database;
    function Ready: Boolean;
    // backup
    property Backup_Directory: U_String read FBackup_Directory write FBackup_Directory;
    property Backup_Is_Busy: Boolean read FBackup_Is_Busy;
    function Get_Backup_Directory: U_String;
    procedure Backup(Reserve_: Word);
    function Found_Backup(): Boolean;
    function Revert_Backup(remove_backup_, Build_: Boolean): Boolean;
    function Revert_Backup_From(FileName: U_String; Build_: Boolean): Boolean;
    // create or open
    procedure Build(Data_Class: TZDB2_Th_Engine_Data_Class);
    procedure Rebuild_Sequence_Data_Pool(Data_Class: TZDB2_Th_Engine_Data_Class); // rebuild sequence
    function Flush: Boolean;
    function Add(Data_Class: TZDB2_Th_Engine_Data_Class; ID: Integer): TZDB2_Th_Engine_Data; overload;
    function Add(Data_Class: TZDB2_Th_Engine_Data_Class): TZDB2_Th_Engine_Data; overload;
    procedure Progress();
  end;

  TZDB2_Th_Engine_Pool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TCritical_BigList<TZDB2_Th_Engine>;

  TZDB2_Th_Engine_Pool = class(TZDB2_Th_Engine_Pool_Decl)
  public
    constructor Create();
    procedure DoFree(var Data: TZDB2_Th_Engine); override;
    function Get_Minimize_Size_Engine(): TZDB2_Th_Engine;
    function Get_Minimize_Workload_Engine(): TZDB2_Th_Engine;
    function AllIsOnlyRead(): Boolean;
  end;

  TZDB2_Th_Engine_On_Data_Event_C = procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMS64);
  TZDB2_Th_Engine_On_Data_Event_M = procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMS64) of object;
{$IFDEF FPC}
  TZDB2_Th_Engine_On_Data_Event_P = procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMS64) is nested;
{$ELSE FPC}
  TZDB2_Th_Engine_On_Data_Event_P = reference to procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMS64);
{$ENDIF FPC}
  TZDB2_Th_Engine_Load_Processor = class;

  TZDB2_Th_Engine_Data_Load_Instance = class(TIO_Thread_Data)
  private
    FStream: TMS64;
    FLoad_Processor: TZDB2_Th_Engine_Load_Processor;
    FData: TZDB2_Th_Engine_Data;
    FOnRun_C: TZDB2_Th_Engine_On_Data_Event_C;
    FOnRun_M: TZDB2_Th_Engine_On_Data_Event_M;
    FOnRun_P: TZDB2_Th_Engine_On_Data_Event_P;
    procedure Do_Read_Stream_Result(var Sender: TZDB2_Th_CMD_Stream_And_State);
  public
    constructor Create(Load_Processor_: TZDB2_Th_Engine_Load_Processor; Data_: TZDB2_Th_Engine_Data);
    destructor Destroy; override;
    procedure Process; override;
  end;

  TOn_Wait_C = procedure(Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance);
  TOn_Wait_M = procedure(Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance) of object;
{$IFDEF FPC}
  TOn_Wait_P = procedure(Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance) is nested;
{$ELSE FPC}
  TOn_Wait_P = reference to procedure(Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance);
{$ENDIF FPC}

  TZDB2_Th_Engine_Load_Processor = class
  private
    tatal_data_num_: Int64;
    buff: TZDB2_Th_Engine_Marshal_BigList___.PQueueArrayStruct;
    Load_Task_Num: TAtomInt64;
    Task_Is_Run: Boolean;
    OnRun_C: TZDB2_Th_Engine_On_Data_Event_C;
    OnRun_M: TZDB2_Th_Engine_On_Data_Event_M;
    OnRun_P: TZDB2_Th_Engine_On_Data_Event_P;
    FTh_Pool: TIO_Thread_Base;
    procedure Do_Thread_Run();
  public
    constructor Create(ThNum_: Integer);
    destructor Destroy; override;
    procedure Run();
    procedure Wait();
    procedure Wait_C(On_Wait: TOn_Wait_C);
    procedure Wait_M(On_Wait: TOn_Wait_M);
    procedure Wait_P(On_Wait: TOn_Wait_P);
  end;

  TZDB2_Th_Engine_Marshal_Pool = {$IFDEF FPC}specialize {$ENDIF FPC} TCritical_BigList<TZDB2_Th_Engine_Marshal>;

  TZDB2_Th_Engine_Marshal_For_C = procedure(Sender: TZDB2_Th_Engine_Data; Index: Int64; var Aborted: Boolean);
  TZDB2_Th_Engine_Marshal_For_M = procedure(Sender: TZDB2_Th_Engine_Data; Index: Int64; var Aborted: Boolean) of object;
{$IFDEF FPC}
  TZDB2_Th_Engine_Marshal_For_P = procedure(Sender: TZDB2_Th_Engine_Data; Index: Int64; var Aborted: Boolean) is nested;
{$ELSE FPC}
  TZDB2_Th_Engine_Marshal_For_P = reference to procedure(Sender: TZDB2_Th_Engine_Data; Index: Int64; var Aborted: Boolean);
{$ENDIF FPC}

  // TZDB2_Th_Engine_Marshal is a parallel marshal manager.
  // TZDB2_Th_Engine_Marshal all methods is thread safe
  TZDB2_Th_Engine_Marshal = class(TCore_InterfacedObject)
  private
    FCritical: TCritical;
    FLong_Loop_Num: Integer;
    Pool_Ptr: TZDB2_Th_Engine_Marshal_Pool.PQueueStruct;
    procedure DoFree(var Data: TZDB2_Th_Engine_Data);
  public
    Data_Marshal: TZDB2_Th_Engine_Marshal_BigList___;
    Engine_Pool: TZDB2_Th_Engine_Pool;
    Instance_Recycle_Tool: TZDB2_Th_Engine_Data_Instance_Recycle_Tool___;
    Data_Link_Recycle_Tool: TZDB2_Th_Engine_Data_Link_Recycle_Tool___;
    Current_Data_Class: TZDB2_Th_Engine_Data_Class;
    constructor Create();
    destructor Destroy; override;
    // thread
    procedure Lock;
    procedure UnLock;
    // load data as TZDB2_Th_Engine_Data_Class
    procedure Build(Data_Class: TZDB2_Th_Engine_Data_Class); overload;
    procedure Build(); overload;
    // ready state
    function Check_Engine: Boolean;
    // update all ptr
    procedure Update_Data_Ptr();
    procedure Sort_C(OnSort: TZDB2_Th_Engine_Marshal_BigList___.TSort_C);
    procedure Sort_M(OnSort: TZDB2_Th_Engine_Marshal_BigList___.TSort_M);
    procedure Sort_P(OnSort: TZDB2_Th_Engine_Marshal_BigList___.TSort_P);
    // clear
    procedure Clear;
    // database space state
    function Database_Size: Int64;
    function Database_Physics_Size: Int64;
    // data num
    function Total: NativeInt;
    // task queue
    function QueueNum: NativeInt;
    // pick engine
    function Add_Data_To_Minimize_Workload_Engine(): TZDB2_Th_Engine_Data;
    function Add_Data_To_Minimize_Size_Engine(): TZDB2_Th_Engine_Data;
    function Add_Data_To_Engine(Eng_: TZDB2_Th_Engine): TZDB2_Th_Engine_Data;
    // wait busy task
    procedure Wait_Busy_Task;
    // check recycle pool
    procedure Check_Recycle_Pool;
    // progress
    function Progress: Boolean;
    // backup
    procedure Backup(Reserve_: Word);
    // flush
    procedure Flush;
    // remove and rebuild datgabase
    procedure Format_Database;
    // parallel load
    procedure Parallel_Load_C(ThNum_: Integer; On_Run: TZDB2_Th_Engine_On_Data_Event_C; On_Wait: TOn_Wait_C);
    procedure Parallel_Load_M(ThNum_: Integer; On_Run: TZDB2_Th_Engine_On_Data_Event_M; On_Wait: TOn_Wait_M);
    procedure Parallel_Load_P(ThNum_: Integer; On_Run: TZDB2_Th_Engine_On_Data_Event_P; On_Wait: TOn_Wait_P);
    // for-thread safe
    procedure For_C(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_Marshal_For_C);
    procedure For_M(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_Marshal_For_M);
    procedure For_P(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_Marshal_For_P);
    // RemoveDatabaseOnDestroy
    function GetRemoveDatabaseOnDestroy: Boolean;
    procedure SetRemoveDatabaseOnDestroy(const Value: Boolean);
    property RemoveDatabaseOnDestroy: Boolean read GetRemoveDatabaseOnDestroy write SetRemoveDatabaseOnDestroy;
    // test
    class procedure Test();
    class procedure Test_Backup_Support();
  end;

var
  Th_Engine_Marshal_Pool__: TZDB2_Th_Engine_Marshal_Pool;

implementation

uses Z.Expression;

constructor TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge.Create;
begin
  inherited Create;
  Source := nil;
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
end;

procedure TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge.Do_Result(var Sender: TZDB2_Th_CMD_Mem64_And_State);
begin
  try
    if Assigned(OnResult_C) then
        OnResult_C(Sender);
    if Assigned(OnResult_M) then
        OnResult_M(Sender);
    if Assigned(OnResult_P) then
        OnResult_P(Sender);
  except
  end;
  AtomDec(Source.FAsync_Load_Num);
  Free;
end;

constructor TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Create;
begin
  inherited Create;
  Source := nil;
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
end;

procedure TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Do_Result(var Sender: TZDB2_Th_CMD_Stream_And_State);
begin
  try
    if Assigned(OnResult_C) then
        OnResult_C(Sender);
    if Assigned(OnResult_M) then
        OnResult_M(Sender);
    if Assigned(OnResult_P) then
        OnResult_P(Sender);
  except
  end;
  AtomDec(Source.FAsync_Load_Num);
  Free;
end;

procedure TZDB2_Th_Engine_Data.Wait_Unlock(timeOut: TTimeTick);
var
  tk: TTimeTick;
begin
  tk := GetTimeTick;
  while FLocked do
    begin
      TCompute.Sleep(1);
      if (timeOut > 0) and (GetTimeTick - tk > timeOut) then // anti dead
          break;
    end;
end;

procedure TZDB2_Th_Engine_Data.Do_Async_Save_Result(var Sender: TZDB2_Th_CMD_ID_And_State);
begin
  if Sender.State = TCMD_State.csDone then
      FID := Sender.ID
  else
      FID := -1;
  AtomDec(FAsync_Save_Num);
end;

constructor TZDB2_Th_Engine_Data.Create;
begin
  inherited Create;
  FOwner := nil;
  FOwner_Data_Ptr := nil;
  FTh_Engine := nil;
  FTh_Engine_Data_Ptr := nil;
  FID := -1;
  FInstance_Busy := 0;
  FLocked := False;
  FAsync_Load_Num := 0;
  FAsync_Save_Num := 0;
  FPost_Free_Runing := False;
  FLoad_Data_Error := False;
end;

destructor TZDB2_Th_Engine_Data.Destroy;
begin
  if not FPost_Free_Runing then
      RaiseInfo('error');
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Data.Progress();
begin

end;

procedure TZDB2_Th_Engine_Data.Lock;
begin
  Wait_Unlock(5000);
  FLocked := True;
end;

procedure TZDB2_Th_Engine_Data.UnLock;
begin
  FLocked := False;
end;

function TZDB2_Th_Engine_Data.is_UnLocked: Boolean;
begin
  Result := not FLocked;
end;

procedure TZDB2_Th_Engine_Data.Update_Instance_As_Busy;
begin
  Lock;
  Inc(FInstance_Busy);
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Update_Instance_As_Free;
begin
  Lock;
  Dec(FInstance_Busy);
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Reset_Instance_As_Free();
begin
  Lock;
  FInstance_Busy := 0;
  UnLock;
end;

function TZDB2_Th_Engine_Data.IsOnlyRead: Boolean;
begin
  if Engine <> nil then
      Result := Engine.IsOnlyRead
  else
      Result := True;
end;

function TZDB2_Th_Engine_Data.Engine: TZDB2_Th_Queue;
begin
  if FTh_Engine <> nil then
      Result := FTh_Engine.Engine
  else
      Result := nil;
end;

function TZDB2_Th_Engine_Data.Can_Load: Boolean;
begin
  Result := (FID >= 0) and (FTh_Engine <> nil) and (FTh_Engine_Data_Ptr <> nil) and (not FPost_Free_Runing);
end;

function TZDB2_Th_Engine_Data.Can_Progress: Boolean;
begin
  Result := (not FPost_Free_Runing) and (is_UnLocked) and (not FPost_Free_Runing);
end;

function TZDB2_Th_Engine_Data.Can_Free: Boolean;
begin
  Result := (FInstance_Busy <= 0) and (is_UnLocked) and (FAsync_Load_Num <= 0) and (FAsync_Save_Num <= 0);
end;

procedure TZDB2_Th_Engine_Data.MoveToLast;
begin
  if (FTh_Engine <> nil) and (FTh_Engine_Data_Ptr <> nil) then
    begin
      FTh_Engine.Th_Engine_Data_Pool.MoveToLast(FTh_Engine_Data_Ptr);
    end;
  if (FOwner <> nil) and (FOwner_Data_Ptr <> nil) then
    begin
      FOwner.Data_Marshal.MoveToLast(FOwner_Data_Ptr);
    end;
end;

procedure TZDB2_Th_Engine_Data.MoveToFirst;
begin
  if (FTh_Engine <> nil) and (FTh_Engine_Data_Ptr <> nil) then
    begin
      FTh_Engine.Th_Engine_Data_Pool.MoveToFirst(FTh_Engine_Data_Ptr);
    end;
  if (FOwner <> nil) and (FOwner_Data_Ptr <> nil) then
    begin
      FOwner.Data_Marshal.MoveToFirst(FOwner_Data_Ptr);
    end;
end;

procedure TZDB2_Th_Engine_Data.Remove(Delete_Data_: Boolean);
var
  Engine__: TZDB2_Th_Engine;
  Eng_Marshal__: TZDB2_Th_Engine_Marshal;
begin
  Lock;
  if not FPost_Free_Runing then
    begin
      FPost_Free_Runing := True;

      if Delete_Data_ and (Engine <> nil) then
        begin
          if (FID >= 0) then
              Engine.Async_Remove(FID);
          FID := -1;
        end;

      if (FOwner <> nil) and (FOwner.FLong_Loop_Num > 0) then
        begin
          FOwner.Data_Link_Recycle_Tool.Add(self); // post link
        end
      else if (FTh_Engine <> nil) and (FTh_Engine_Data_Ptr <> nil) then
        begin
          Engine__ := FTh_Engine;
          Engine__.Th_Engine_Data_Pool.Lock;
          FTh_Engine.Th_Engine_Data_Pool.Push_To_Recycle_Pool(FTh_Engine_Data_Ptr); // remove link
          FTh_Engine := nil;
          FTh_Engine_Data_Ptr := nil;
          Engine__.Th_Engine_Data_Pool.UnLock;
        end
      else if (FOwner <> nil) and (FOwner_Data_Ptr <> nil) then
        begin
          Eng_Marshal__ := FOwner;
          Eng_Marshal__.Data_Marshal.Lock;
          FOwner.Data_Marshal.Push_To_Recycle_Pool(FOwner_Data_Ptr); // remove link
          FOwner := nil;
          FOwner_Data_Ptr := nil;
          Eng_Marshal__.Data_Marshal.UnLock;
        end;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Remove();
begin
  Remove(True);
end;

function TZDB2_Th_Engine_Data.Load_Data(Source: TMS64): Boolean;
begin
  Result := False;
  Lock;
  if (FID >= 0) and (not FPost_Free_Runing) then
    begin
      AtomInc(FAsync_Load_Num);
      try
          Result := Engine.Sync_GetData(Source, FID);
      except
      end;
      AtomDec(FAsync_Load_Num);
    end;
  UnLock;
end;

function TZDB2_Th_Engine_Data.Load_Data(Source: TMem64): Boolean;
begin
  Result := False;
  Lock;
  if (FID >= 0) and (not FPost_Free_Runing) then
    begin
      AtomInc(FAsync_Load_Num);
      try
          Result := Engine.Sync_GetData(Source, FID);
      except
      end;
      AtomDec(FAsync_Load_Num);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Data_C(Source: TMS64; OnResult: TOn_Stream_And_State_Event_C);
var
  tmp: TZDB2_Th_CMD_Stream_And_State;
  bridge_: TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Create;
      bridge_.Source := self;
      bridge_.OnResult_C := OnResult;
      Engine.Async_GetData_AsStream_M(FID, Source, {$IFDEF FPC}@{$ENDIF FPC}bridge_.Do_Result);
    end
  else
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Data_C(Source: TMem64; OnResult: TOn_Mem64_And_State_Event_C);
var
  tmp: TZDB2_Th_CMD_Mem64_And_State;
  bridge_: TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge.Create;
      bridge_.Source := self;
      bridge_.OnResult_C := OnResult;
      Engine.Async_GetData_AsMem64_M(FID, Source, {$IFDEF FPC}@{$ENDIF FPC}bridge_.Do_Result);
    end
  else
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Data_M(Source: TMS64; OnResult: TOn_Stream_And_State_Event_M);
var
  tmp: TZDB2_Th_CMD_Stream_And_State;
  bridge_: TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Create;
      bridge_.Source := self;
      bridge_.OnResult_M := OnResult;
      Engine.Async_GetData_AsStream_M(FID, Source, {$IFDEF FPC}@{$ENDIF FPC}bridge_.Do_Result);
    end
  else
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Data_M(Source: TMem64; OnResult: TOn_Mem64_And_State_Event_M);
var
  tmp: TZDB2_Th_CMD_Mem64_And_State;
  bridge_: TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge.Create;
      bridge_.Source := self;
      bridge_.OnResult_M := OnResult;
      Engine.Async_GetData_AsMem64_M(FID, Source, {$IFDEF FPC}@{$ENDIF FPC}bridge_.Do_Result);
    end
  else
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Data_P(Source: TMS64; OnResult: TOn_Stream_And_State_Event_P);
var
  tmp: TZDB2_Th_CMD_Stream_And_State;
  bridge_: TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Create;
      bridge_.Source := self;
      bridge_.OnResult_P := OnResult;
      Engine.Async_GetData_AsStream_M(FID, Source, {$IFDEF FPC}@{$ENDIF FPC}bridge_.Do_Result);
    end
  else
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Data_P(Source: TMem64; OnResult: TOn_Mem64_And_State_Event_P);
var
  tmp: TZDB2_Th_CMD_Mem64_And_State;
  bridge_: TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge.Create;
      bridge_.Source := self;
      bridge_.OnResult_P := OnResult;
      Engine.Async_GetData_AsMem64_M(FID, Source, {$IFDEF FPC}@{$ENDIF FPC}bridge_.Do_Result);
    end
  else
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end;
  UnLock;
end;

function TZDB2_Th_Engine_Data.Save_Data(Source: TMS64): Boolean;
begin
  Result := False;
  Lock;
  if not FPost_Free_Runing then
    begin
      AtomInc(FAsync_Save_Num);
      Result := Engine.Sync_SetData(Source, FID);
      AtomDec(FAsync_Save_Num);
    end;
  UnLock;
end;

function TZDB2_Th_Engine_Data.Save_Data(Source: TMem64): Boolean;
begin
  Result := False;
  Lock;
  if not FPost_Free_Runing then
    begin
      AtomInc(FAsync_Save_Num);
      Result := Engine.Sync_SetData(Source, FID);
      AtomDec(FAsync_Save_Num);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Data(Source: TMS64);
begin
  Lock;
  if FPost_Free_Runing then
    begin
      disposeObjectAndNil(Source);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      Engine.Async_SetData_M(Source, True, FID, {$IFDEF FPC}@{$ENDIF FPC}Do_Async_Save_Result);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Data(Source: TMem64);
begin
  Lock;
  if FPost_Free_Runing then
    begin
      disposeObjectAndNil(Source);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      Engine.Async_SetData_M(Source, True, FID, {$IFDEF FPC}@{$ENDIF FPC}Do_Async_Save_Result);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save(Source: TMS64);
begin
  Lock;
  if FPost_Free_Runing then
    begin
      disposeObjectAndNil(Source);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      Engine.Async_SetData_M(Source, False, FID, {$IFDEF FPC}@{$ENDIF FPC}Do_Async_Save_Result);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save(Source: TMem64);
begin
  Lock;
  if FPost_Free_Runing then
    begin
      disposeObjectAndNil(Source);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      Engine.Async_SetData_M(Source, False, FID, {$IFDEF FPC}@{$ENDIF FPC}Do_Async_Save_Result);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Update_State_Loading_Error;
begin
  FLoad_Data_Error := True;
end;

constructor TZDB2_Th_Engine_Backup.Create(Owner_: TZDB2_Th_Engine);
begin
  inherited Create;
  Owner := Owner_;
  Queue_ID_List_ := TZDB2_ID_List.Create;
  backup_file := '';
end;

destructor TZDB2_Th_Engine_Backup.Destroy;
begin
  DisposeObject(Queue_ID_List_);
  backup_file := '';
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Backup.Do_Run(Sender: TCompute);
var
  hnd: TZDB2_BlockHandle;
begin
  hnd := TZDB2_Core_Space.Get_Handle(Queue_ID_List_);
  Owner.Engine.Sync_Extract_To_File(hnd, backup_file, nil);
  DoStatus('backup to "%s" done', [backup_file.Text]);
  SetLength(hnd, 0);
  Owner.FBackup_Is_Busy := False;
end;

procedure TZDB2_Th_Engine.DoFree(var Data: TZDB2_Th_Engine_Data);
begin
  if Data = nil then
      exit;
  Data.FTh_Engine := nil;
  Data.FTh_Engine_Data_Ptr := nil;
  Data.FID := -1;
  if (Data.FOwner <> nil) and (Data.FOwner_Data_Ptr <> nil) then
      Data.FOwner.Data_Marshal.Remove_P(Data.FOwner_Data_Ptr)
  else
    begin
      Owner.Instance_Recycle_Tool.Add(Data);
      Data.FOwner := nil;
      Data.FOwner_Data_Ptr := nil;
      Data.FTh_Engine := nil;
      Data.FTh_Engine_Data_Ptr := nil;
      Data := nil;
    end;
end;

constructor TZDB2_Th_Engine.Create(Owner_: TZDB2_Th_Engine_Marshal);
begin
  inherited Create;
  FBackup_Is_Busy := False;
  FBackup_Directory := '';
  Name := '';
  Owner := Owner_;
  RemoveDatabaseOnDestroy := False;
  Mode := smBigData;
  Database_File := '';
  OnlyRead := False;
  Delta := 16 * 1024 * 1024;
  BlockSize := 1536;
  Cipher := nil;
  Cipher_Security := TCipherSecurity.csNone;
  Cipher_password := 'DTC40@ZSERVER';
  Cipher_Level := 1;
  Cipher_Tail := True;
  Cipher_CBC := True;
  Engine := nil;
  Th_Engine_Data_Pool := TZDB2_Th_Engine_Data_BigList___.Create;
  Th_Engine_Data_Pool.OnFree := {$IFDEF FPC}@{$ENDIF FPC}DoFree;
  Owner.Engine_Pool.Add(self);
  Last_Build_Class := TZDB2_Th_Engine_Data;
end;

destructor TZDB2_Th_Engine.Destroy;
begin
  try
    disposeObjectAndNil(Th_Engine_Data_Pool);
    disposeObjectAndNil(Engine);
    disposeObjectAndNil(Cipher);
    if RemoveDatabaseOnDestroy and umlFileExists(Database_File) then
        umlDeleteFile(Database_File);
  except
  end;
  inherited Destroy;
end;

procedure TZDB2_Th_Engine.ReadConfig(Name_: U_String; cfg: THashStringList);
begin
  Name := Name_;
  RemoveDatabaseOnDestroy := EStrToBool(cfg.GetDefaultValue('RemoveDatabaseOnDestroy', umlBoolToStr(RemoveDatabaseOnDestroy)), RemoveDatabaseOnDestroy);
  Database_File := cfg.GetDefaultValue('database', Database_File);
  OnlyRead := EStrToBool(cfg.GetDefaultValue('OnlyRead', umlBoolToStr(OnlyRead)), OnlyRead);
  Delta := EStrToInt(cfg.GetDefaultValue('Delta', umlIntToStr(Delta)), Delta);
  BlockSize := EStrToInt(cfg.GetDefaultValue('BlockSize', umlIntToStr(BlockSize)), BlockSize);
  Cipher_Security := TZDB2_Cipher.GetCipherSecurity(cfg.GetDefaultValue('Security', TCipher.CCipherSecurityName[Cipher_Security]));
  Cipher_password := cfg.GetDefaultValue('Password', Cipher_password);
  Cipher_Level := EStrToInt(cfg.GetDefaultValue('Level', umlIntToStr(Cipher_Level)), Cipher_Level);
  Cipher_Tail := EStrToBool(cfg.GetDefaultValue('Tail', umlBoolToStr(Cipher_Tail)), Cipher_Tail);
  Cipher_CBC := EStrToBool(cfg.GetDefaultValue('CBC', umlBoolToStr(Cipher_CBC)), Cipher_CBC);
end;

procedure TZDB2_Th_Engine.ReadConfig(cfg: THashStringList);
begin
  ReadConfig(Name, cfg);
end;

procedure TZDB2_Th_Engine.WriteConfig(cfg: THashStringList);
begin
  cfg.SetDefaultValue('RemoveDatabaseOnDestroy', umlBoolToStr(RemoveDatabaseOnDestroy));
  cfg.SetDefaultValue('database', Database_File);
  cfg.SetDefaultValue('OnlyRead', umlBoolToStr(OnlyRead));
  cfg.SetDefaultValue('Delta', umlIntToStr(Delta));
  cfg.SetDefaultValue('BlockSize', umlIntToStr(BlockSize));
  cfg.GetDefaultValue('Security', TCipher.CCipherSecurityName[Cipher_Security]);
  cfg.SetDefaultValue('Password', Cipher_password);
  cfg.SetDefaultValue('Level', umlIntToStr(Cipher_Level));
  cfg.SetDefaultValue('Tail', umlBoolToStr(Cipher_Tail));
  cfg.SetDefaultValue('CBC', umlBoolToStr(Cipher_CBC));
end;

procedure TZDB2_Th_Engine.Update_Engine_Data_Ptr();
begin
  Th_Engine_Data_Pool.Lock;
  if Th_Engine_Data_Pool.Num > 0 then
    begin
      with Th_Engine_Data_Pool.Repeat_ do
        repeat
          Queue^.Data.FTh_Engine := self;
          Queue^.Data.FTh_Engine_Data_Ptr := Queue;
        until not Next;
    end;
  Th_Engine_Data_Pool.UnLock;
end;

procedure TZDB2_Th_Engine.Clear;
begin
  Flush;
  Th_Engine_Data_Pool.Clear;
end;

procedure TZDB2_Th_Engine.Format_Database;
begin
  if Engine <> nil then
    begin
      while Engine.QueueNum > 0 do
          TCompute.Sleep(10);
      Th_Engine_Data_Pool.Clear;
      disposeObjectAndNil(Engine);
      disposeObjectAndNil(Cipher);
    end;

  if umlFileExists(Database_File) then
      umlDeleteFile(Database_File);
end;

function TZDB2_Th_Engine.Ready: Boolean;
begin
  Result := (Engine <> nil);
end;

function TZDB2_Th_Engine.Get_Backup_Directory: U_String;
begin
  if umlTrimSpace(FBackup_Directory) = '' then
    begin
      if umlTrimSpace(Database_File) = '' then
          Result := ''
      else
          Result := umlGetFilePath(Database_File);
    end
  else
      Result := FBackup_Directory;
end;

procedure TZDB2_Th_Engine.Backup(Reserve_: Word);
type
  TFile_Time_Info = record
    FileName: SystemString;
    FileTime_: TDateTime;
  end;

  TFileTime_Sorted = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<TFile_Time_Info>;

var
  db_path: U_String;
  db_file: U_String;
  arry: U_StringArray;
  n: U_SystemString;
  L: TFileTime_Sorted;
  backup_inst: TZDB2_Th_Engine_Backup;
  __repeat__: TZDB2_Th_Engine_Data_BigList___.TRepeat___;

  function Make_backup_File_Name: U_String;
  var
    now_: TDateTime;
    Year, Month, Day, Hour, min_, Sec, MSec: Word;
  begin
    repeat
      TCompute.Sleep(100);
      now_ := Now();
      DecodeDate(now_, Year, Month, Day);
      DecodeTime(now_, Hour, min_, Sec, MSec);
      Result := umlCombineFileName(db_path, PFormat('%s.backup(%d_%d_%d_%d_%d_%d_%d)', [db_file.Text, Year, Month, Day, Hour, min_, Sec, MSec]));
    until not umlFileExists(Result);
  end;

{$IFDEF FPC}
  function do_fpc_sort(var L, R: TFile_Time_Info): Integer;
  begin
    Result := CompareDateTime(L.FileTime_, R.FileTime_);
  end;
{$ENDIF FPC}


begin
  if Engine = nil then
      exit;
  if Engine.Is_Memory_Data then
      exit;
  if FBackup_Is_Busy then
      exit;

  if not umlFileExists(Engine.Get_Database_FileName) then
      exit;
  db_path := Get_Backup_Directory();
  db_file := umlGetFileName(Engine.Get_Database_FileName);

  FBackup_Is_Busy := True;

  DoStatus('scan backup file:' + db_file + '.backup(*)');
  arry := umlGetFileListPath(db_path);
  L := TFileTime_Sorted.Create;
  for n in arry do
    if umlMultipleMatch(True, db_file + '.backup(*)', n) then
      with L.Add_Null^ do
        begin
          Data.FileName := n;
          Data.FileTime_ := umlGetFileTime(umlCombineFileName(db_path, n));
        end;

  // sort backup file by time
{$IFDEF FPC}
  L.Sort_P(@do_fpc_sort);
{$ELSE FPC}
  L.Sort_P(function(var L, R: TFile_Time_Info): Integer
    begin
      Result := CompareDateTime(L.FileTime_, R.FileTime_);
    end);
{$ENDIF FPC}
  // remove old backup
  while L.Num > Reserve_ do
    begin
      DoStatus('remove old backup "%s"', [L.First^.Data.FileName]);
      umlDeleteFile(L.First^.Data.FileName);
      L.Next;
    end;
  DisposeObject(L); // free pool

  // backup instance
  backup_inst := TZDB2_Th_Engine_Backup.Create(self);
  // check busy queue
  while Engine.QueueNum > 0 do
      TCompute.Sleep(1);
  Th_Engine_Data_Pool.Lock; // safe lock
  try
    // rebuild sequece
    if Th_Engine_Data_Pool.Num > 0 then
      begin
        __repeat__ := Th_Engine_Data_Pool.Repeat_;
        repeat
          if __repeat__.Queue^.Data <> nil then
            begin
              if __repeat__.Queue^.Data.FID >= 0 then
                  backup_inst.Queue_ID_List_.Add(__repeat__.Queue^.Data.FID)
            end;
        until not __repeat__.Next;
      end;
  finally
      Th_Engine_Data_Pool.UnLock; // safe unlock
  end;
  backup_inst.backup_file := Make_backup_File_Name();
  TCompute.RunM(nil, self, {$IFDEF FPC}@{$ENDIF FPC}backup_inst.Do_Run); // run backup thread
end;

function TZDB2_Th_Engine.Found_Backup(): Boolean;
var
  db_path: U_String;
  db_file: U_String;
  arry: U_StringArray;
begin
  Result := False;

  if umlTrimSpace(Database_File) = '' then
      exit;

  while FBackup_Is_Busy do
      TCompute.Sleep(100);

  db_path := Get_Backup_Directory();
  db_file := umlGetFileName(Database_File);

  DoStatus('scan Backup for "%s"', [Database_File.Text]);
  arry := umlGetFileListPath(db_path);

  Result := length(arry) > 0;

  db_path := '';
  db_file := '';
  SetLength(arry, 0);
end;

function TZDB2_Th_Engine.Revert_Backup(remove_backup_, Build_: Boolean): Boolean;
type
  TFile_Time_Info = record
    FileName: SystemString;
    FileTime_: TDateTime;
  end;

  TFileTime_Sorted = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<TFile_Time_Info>;

var
  db_path: U_String;
  db_file: U_String;
  arry: U_StringArray;
  n: U_SystemString;
  L: TFileTime_Sorted;

{$IFDEF FPC}
  function do_fpc_sort(var L, R: TFile_Time_Info): Integer;
  begin
    Result := CompareDateTime(L.FileTime_, R.FileTime_);
  end;
{$ENDIF FPC}


begin
  Result := False;

  if umlTrimSpace(Database_File) = '' then
      exit;

  while FBackup_Is_Busy do
      TCompute.Sleep(100);

  FBackup_Is_Busy := True;
  try
    Th_Engine_Data_Pool.Clear;
    disposeObjectAndNil(Engine);
    disposeObjectAndNil(Cipher);

    db_path := Get_Backup_Directory();
    db_file := umlGetFileName(Database_File);

    DoStatus('scan Backup for "%s"', [Database_File.Text]);
    arry := umlGetFileListPath(db_path);
    L := TFileTime_Sorted.Create;
    for n in arry do
      if umlMultipleMatch(True, db_file + '.backup(*)', n) then
        with L.Add_Null^ do
          begin
            Data.FileName := n;
            Data.FileTime_ := umlGetFileTime(umlCombineFileName(db_path, n));
          end;

    // sort backup file by time
{$IFDEF FPC}
    L.Sort_P(@do_fpc_sort);
{$ELSE FPC}
    L.Sort_P(function(var L, R: TFile_Time_Info): Integer
      begin
        Result := CompareDateTime(L.FileTime_, R.FileTime_);
      end);
{$ENDIF FPC}
    if L.Num > 0 then
      begin
        umlDeleteFile(Database_File);
        Result := umlCopyFile(L.Last^.Data.FileName, Database_File);
        if Result then
          begin
            DoStatus('Done Revert %s -> %s', [L.Last^.Data.FileName, Database_File.Text]);
            if remove_backup_ then
              begin
                umlDeleteFile(L.Last^.Data.FileName);
                DoStatus('Remove Backup %s', [L.Last^.Data.FileName]);
              end;
            if Build_ then
                Build(Last_Build_Class);
          end;
      end
    else
      begin
        DoStatus('no found Backup file for "%s"', [Database_File.Text]);
      end;
    DisposeObject(L);
  finally
      FBackup_Is_Busy := True;
  end;
end;

function TZDB2_Th_Engine.Revert_Backup_From(FileName: U_String; Build_: Boolean): Boolean;
begin
  Result := False;

  if umlTrimSpace(Database_File) = '' then
      exit;
  if not umlFileExists(FileName) then
      exit;

  while FBackup_Is_Busy do
      TCompute.Sleep(100);

  FBackup_Is_Busy := True;
  try
    Th_Engine_Data_Pool.Clear;
    disposeObjectAndNil(Engine);
    disposeObjectAndNil(Cipher);

    umlDeleteFile(Database_File);
    Result := umlCopyFile(FileName, Database_File);
    if Result then
      if Build_ then
          Build(Last_Build_Class);
    DoStatus('Done Revert %s -> %s', [FileName.Text, Database_File.Text]);
  finally
      FBackup_Is_Busy := False;
  end;
end;

procedure TZDB2_Th_Engine.Build(Data_Class: TZDB2_Th_Engine_Data_Class);
var
  Hash_L: THashStringList;
  Stream: TCore_Stream;
  Queue_Table_: TZDB2_BlockHandle;
  ID: Integer;
begin
  if isDebug and IsConsole then
    begin
      DoStatus('');
      Hash_L := THashStringList.Create;
      WriteConfig(Hash_L);
      DoStatus('ZDB2.Thread Build %s', [Data_Class.ClassName]);
      Hash_L['Password'] := '(hide)';
      DoStatus(Hash_L.AsText);
      DisposeObject(Hash_L);
      DoStatus('');
    end;

  Th_Engine_Data_Pool.Clear;
  disposeObjectAndNil(Engine);
  disposeObjectAndNil(Cipher);

  // init cipher
  if Cipher_Security <> TCipherSecurity.csNone then
      Cipher := TZDB2_Cipher.Create(Cipher_Security, Cipher_password, Cipher_Level, Cipher_Tail, Cipher_CBC);

  // init stream
  try
    if umlTrimSpace(Database_File) = '' then
      begin
        Stream := TMS64.CustomCreate(if_(CPU64, 128 * 1024 * 1024, 8 * 1024 * 1024));
      end
    else
      begin
        if RemoveDatabaseOnDestroy then
            umlDeleteFile(Database_File);
        Stream := TReliableFileStream.Create(Database_File, not umlFileExists(Database_File), not OnlyRead);
      end;
  except
      exit;
  end;

  try
    if (Stream.Size = 0) then
      begin
        // check stream
        Engine := TZDB2_Th_Queue.Create(Mode, Stream, True, OnlyRead, Delta, BlockSize, Cipher);
      end
    else if TZDB2_Core_Space.CheckStream(Stream, Cipher, Found_Backup()) then // check open from cipher and check Fault Shutdown
      begin
        Engine := TZDB2_Th_Queue.Create(Mode, Stream, True, OnlyRead, Delta, BlockSize, Cipher);
        // init sequence
        if Engine.Sync_Get_And_Clean_Sequence_Table(Queue_Table_) then
          begin
            for ID in Queue_Table_ do
                Add(Data_Class, ID);
            SetLength(Queue_Table_, 0);
          end;
      end
    else
      begin
        // Automated Backup Restore
        disposeObjectAndNil(Stream);
        if umlFileExists(Database_File) then // check backup
          begin
            DoStatus('Execute Automated Backup Restore for "%s"', [Database_File.Text]);
            if Revert_Backup(True, False) then
              begin
                Build(Data_Class);
                exit;
              end;
          end;
        DoStatus('"%s" password error or data corruption.', [Database_File.Text]);
      end;
  except
  end;

  if Engine = nil then
      disposeObjectAndNil(Stream);
  Last_Build_Class := Data_Class;
end;

procedure TZDB2_Th_Engine.Rebuild_Sequence_Data_Pool(Data_Class: TZDB2_Th_Engine_Data_Class);
var
  Queue_Table_: TZDB2_BlockHandle;
  ID: Integer;
begin
  if Engine = nil then
      exit;
  Th_Engine_Data_Pool.Clear;

  if Engine.Sync_Rebuild_And_Get_Sequence_Table(Queue_Table_) then
    begin
      for ID in Queue_Table_ do
          Add(Data_Class, ID);
      SetLength(Queue_Table_, 0);
    end;
  Last_Build_Class := Data_Class;
end;

function TZDB2_Th_Engine.Flush: Boolean;
var
  Queue_ID_List_: TZDB2_ID_List;
  __repeat__: TZDB2_Th_Engine_Data_BigList___.TRepeat___;
begin
  if Engine = nil then
      exit(False);

  if Engine.IsOnlyRead then
      exit;

  while Engine.QueueNum > 0 do
      TCompute.Sleep(1);
  Th_Engine_Data_Pool.Free_Recycle_Pool;
  try
    Queue_ID_List_ := TZDB2_ID_List.Create;
    if Th_Engine_Data_Pool.Num > 0 then
      begin
        __repeat__ := Th_Engine_Data_Pool.Repeat_;
        repeat
          if __repeat__.Queue^.Data <> nil then
            begin
              if __repeat__.Queue^.Data.FID >= 0 then
                  Queue_ID_List_.Add(__repeat__.Queue^.Data.FID)
              else
                  Th_Engine_Data_Pool.Push_To_Recycle_Pool(__repeat__.Queue);
            end;
        until not __repeat__.Next;
        Th_Engine_Data_Pool.Free_Recycle_Pool;
      end;
    Result := Engine.Sync_Flush_Sequence_Table(Queue_ID_List_);
    DisposeObject(Queue_ID_List_);
  except
      Engine.Async_Flush;
  end;
end;

function TZDB2_Th_Engine.Add(Data_Class: TZDB2_Th_Engine_Data_Class; ID: Integer): TZDB2_Th_Engine_Data;
var
  Data_Instance: TZDB2_Th_Engine_Data;
begin
  Result := nil;
  if Engine = nil then
      exit;
  try
      Data_Instance := Data_Class.Create();
  except
      exit(Add(Data_Class, ID));
  end;

  Data_Instance.Lock;
  Data_Instance.FOwner := Owner;
  Data_Instance.FOwner_Data_Ptr := Owner.Data_Marshal.Add(Data_Instance);
  Data_Instance.FTh_Engine := self;
  Data_Instance.FTh_Engine_Data_Ptr := Th_Engine_Data_Pool.Add(Data_Instance);
  Data_Instance.FID := ID;
  Data_Instance.UnLock;
  Result := Data_Instance;
end;

function TZDB2_Th_Engine.Add(Data_Class: TZDB2_Th_Engine_Data_Class): TZDB2_Th_Engine_Data;
begin
  Result := Add(Data_Class, -1);
end;

procedure TZDB2_Th_Engine.Progress();
begin
  if Engine = nil then
      exit;

  Th_Engine_Data_Pool.Lock;
  try
    if Th_Engine_Data_Pool.Num > 0 then
      begin
        with Th_Engine_Data_Pool.Repeat_ do
          repeat
            if (Queue^.Data <> nil) and Queue^.Data.Can_Progress then
                Queue^.Data.Progress();
          until not Next;
      end;
    Th_Engine_Data_Pool.Free_Recycle_Pool;
  except
  end;
  Th_Engine_Data_Pool.UnLock;
end;

constructor TZDB2_Th_Engine_Pool.Create;
begin
  inherited Create;
end;

procedure TZDB2_Th_Engine_Pool.DoFree(var Data: TZDB2_Th_Engine);
begin
  disposeObjectAndNil(Data);
end;

function TZDB2_Th_Engine_Pool.Get_Minimize_Size_Engine: TZDB2_Th_Engine;
var
  Eng_: PQueueStruct;
begin
  Result := nil;
  if Num > 0 then
    begin
      Eng_ := nil;
      Lock;
      with Repeat_ do
        repeat
          if (Queue^.Data.Engine <> nil) and (not Queue^.Data.Engine.IsOnlyRead) then
            begin
              if Eng_ = nil then
                  Eng_ := Queue
              else if Queue^.Data.Engine.CoreSpace_Size < Eng_^.Data.Engine.CoreSpace_Size then
                  Eng_ := Queue;
            end;
        until not Next;
      UnLock;

      if Eng_ <> nil then
        begin
          Result := Eng_^.Data;
          MoveToLast(Eng_);
        end;
    end;
end;

function TZDB2_Th_Engine_Pool.Get_Minimize_Workload_Engine: TZDB2_Th_Engine;
var
  Eng_: PQueueStruct;
begin
  Result := nil;
  if Num > 0 then
    begin
      Eng_ := nil;
      Lock;
      with Repeat_ do
        repeat
          if (Queue^.Data.Engine <> nil) and (not Queue^.Data.Engine.IsOnlyRead) then
            begin
              if Eng_ = nil then
                  Eng_ := Queue
              else if Queue^.Data.Engine.QueueNum < Eng_^.Data.Engine.QueueNum then
                  Eng_ := Queue;
            end;
        until not Next;
      UnLock;

      if Eng_ <> nil then
        begin
          Result := Eng_^.Data;
          MoveToLast(Eng_);
        end;
    end;
end;

function TZDB2_Th_Engine_Pool.AllIsOnlyRead(): Boolean;
begin
  Result := False;
  if Num > 0 then
    with Repeat_ do
      begin
        repeat
          if not Queue^.Data.OnlyRead then
              exit;
        until not Next;
      end;
  Result := True;
end;

procedure TZDB2_Th_Engine_Data_Load_Instance.Do_Read_Stream_Result(var Sender: TZDB2_Th_CMD_Stream_And_State);
begin
  if Sender.State = TCMD_State.csDone then
    begin
      FLoad_Processor.FTh_Pool.Enqueue(self);
      FLoad_Processor.Load_Task_Num.UnLock(FLoad_Processor.Load_Task_Num.LockP^ - 1);
    end
  else
    begin
      FLoad_Processor.Load_Task_Num.UnLock(FLoad_Processor.Load_Task_Num.LockP^ - 1);
      DisposeObject(self);
    end;
end;

constructor TZDB2_Th_Engine_Data_Load_Instance.Create(Load_Processor_: TZDB2_Th_Engine_Load_Processor; Data_: TZDB2_Th_Engine_Data);
begin
  inherited Create;
  FStream := TMS64.Create;
  FLoad_Processor := Load_Processor_;
  FData := Data_;
  FOnRun_C := nil;
  FOnRun_M := nil;
  FOnRun_P := nil;
end;

destructor TZDB2_Th_Engine_Data_Load_Instance.Destroy;
begin
  DisposeObject(FStream);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Data_Load_Instance.Process;
begin
  FStream.Position := 0;
  try
    if Assigned(FOnRun_C) then
        FOnRun_C(FData, FStream);
    if Assigned(FOnRun_M) then
        FOnRun_M(FData, FStream);
    if Assigned(FOnRun_P) then
        FOnRun_P(FData, FStream);
  except
      FData.Update_State_Loading_Error;
  end;
end;

procedure TZDB2_Th_Engine_Load_Processor.Do_Thread_Run();
var
  i: Int64;
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance;
begin
  i := 0;
  while i < tatal_data_num_ do
    begin
      try
        if (buff^[i]^.Data <> nil) and (buff^[i]^.Data.Can_Load) then
          begin
            Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance.Create(self, buff^[i]^.Data);
            Load_Inst_.FOnRun_C := OnRun_C;
            Load_Inst_.FOnRun_M := OnRun_M;
            Load_Inst_.FOnRun_P := OnRun_P;
            Load_Inst_.FData.Async_Load_Data_M(Load_Inst_.FStream, {$IFDEF FPC}@{$ENDIF FPC}Load_Inst_.Do_Read_Stream_Result);
            Load_Task_Num.UnLock(Load_Task_Num.LockP^ + 1);
          end;
      except
      end;
      Inc(i);
    end;
  while (Load_Task_Num.V + FTh_Pool.Count > 0) do
      TCompute.Sleep(10);
  Task_Is_Run := False;
end;

constructor TZDB2_Th_Engine_Load_Processor.Create(ThNum_: Integer);
begin
  inherited Create;
  tatal_data_num_ := 0;
  buff := nil;
  Load_Task_Num := TAtomInt64.Create(0);
  Task_Is_Run := False;
  OnRun_C := nil;
  OnRun_M := nil;
  OnRun_P := nil;

{$IFDEF Enabled_ZDB2_Load_Thread}
  FTh_Pool := TIO_Thread.Create(ThNum_);
{$ELSE Enabled_ZDB2_Load_Thread}
  FTh_Pool := TIO_Direct.Create();
{$ENDIF Enabled_ZDB2_Load_Thread}
end;

destructor TZDB2_Th_Engine_Load_Processor.Destroy;
begin
  DisposeObject(FTh_Pool);
  if buff <> nil then
      System.FreeMemory(buff);
  DisposeObject(Load_Task_Num);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Load_Processor.Run();
begin
  Task_Is_Run := True;
  TCompute.RunM_NP({$IFDEF FPC}@{$ENDIF FPC}Do_Thread_Run);
end;

procedure TZDB2_Th_Engine_Load_Processor.Wait();
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance;
begin
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance(FTh_Pool.Dequeue);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
          TCompute.Sleep(1);
    end;
end;

procedure TZDB2_Th_Engine_Load_Processor.Wait_C(On_Wait: TOn_Wait_C);
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance;
begin
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
          TCompute.Sleep(1);
    end;
end;

procedure TZDB2_Th_Engine_Load_Processor.Wait_M(On_Wait: TOn_Wait_M);
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance;
begin
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
          TCompute.Sleep(1);
    end;
end;

procedure TZDB2_Th_Engine_Load_Processor.Wait_P(On_Wait: TOn_Wait_P);
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance;
begin
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
          TCompute.Sleep(1);
    end;
end;

procedure TZDB2_Th_Engine_Marshal.DoFree(var Data: TZDB2_Th_Engine_Data);
begin
  if Data = nil then
      exit;
  Data.FOwner := nil;
  Data.FOwner_Data_Ptr := nil;
  if (Data.FTh_Engine <> nil) and (Data.FTh_Engine_Data_Ptr <> nil) then
      Data.FTh_Engine.Th_Engine_Data_Pool.Remove_P(Data.FTh_Engine_Data_Ptr)
  else
    begin
      Instance_Recycle_Tool.Add(Data);
      Data.FOwner := nil;
      Data.FOwner_Data_Ptr := nil;
      Data.FTh_Engine := nil;
      Data.FTh_Engine_Data_Ptr := nil;
      Data := nil;
    end;
end;

constructor TZDB2_Th_Engine_Marshal.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FLong_Loop_Num := 0;
  Data_Marshal := TZDB2_Th_Engine_Marshal_BigList___.Create;
  Data_Marshal.OnFree := {$IFDEF FPC}@{$ENDIF FPC}DoFree;
  Engine_Pool := TZDB2_Th_Engine_Pool.Create;
  Instance_Recycle_Tool := TZDB2_Th_Engine_Data_Instance_Recycle_Tool___.Create;
  Data_Link_Recycle_Tool := TZDB2_Th_Engine_Data_Link_Recycle_Tool___.Create;
  Current_Data_Class := TZDB2_Th_Engine_Data;
  Pool_Ptr := Th_Engine_Marshal_Pool__.Add(self);
end;

destructor TZDB2_Th_Engine_Marshal.Destroy;
begin
  try
    Check_Recycle_Pool;
    Flush;
    // free link
    if Data_Link_Recycle_Tool.Num > 0 then
      begin
        with Data_Link_Recycle_Tool.Repeat_ do
          repeat
            try
              if (Queue^.Data.FTh_Engine <> nil) and (Queue^.Data.FTh_Engine_Data_Ptr <> nil) then
                begin
                  Queue^.Data.FTh_Engine.Th_Engine_Data_Pool.Push_To_Recycle_Pool(Queue^.Data.FTh_Engine_Data_Ptr);
                  Queue^.Data.FTh_Engine := nil;
                  Queue^.Data.FTh_Engine_Data_Ptr := nil;
                end
              else if (Queue^.Data.FOwner <> nil) and (Queue^.Data.FOwner_Data_Ptr <> nil) then
                begin
                  Data_Marshal.Push_To_Recycle_Pool(Queue^.Data.FOwner_Data_Ptr);
                  Queue^.Data.FOwner := nil;
                  Queue^.Data.FOwner_Data_Ptr := nil;
                end;
              Instance_Recycle_Tool.Add(Queue^.Data);
            except
            end;
          until not Next;
      end;
    disposeObjectAndNil(Data_Link_Recycle_Tool);
    // free thread engine recycle pool
    if Engine_Pool.Num > 0 then
      with Engine_Pool.Repeat_ do
        repeat
          try
              Queue^.Data.Th_Engine_Data_Pool.Free_Recycle_Pool;
          except
          end;
        until not Next;
    // free local recycle pool
    try
        Data_Marshal.Free_Recycle_Pool;
    except
    end;
    // free pool
    disposeObjectAndNil(Engine_Pool);
    disposeObjectAndNil(Data_Marshal);
    // free data instance
    if Instance_Recycle_Tool.Num > 0 then
      with Instance_Recycle_Tool.Repeat_ do
        repeat
          if Queue^.Data <> nil then
            begin
              Queue^.Data.FPost_Free_Runing := True;
              disposeObjectAndNil(Queue^.Data);
            end;
        until not Next;
    disposeObjectAndNil(Instance_Recycle_Tool);
    disposeObjectAndNil(FCritical);
  except
  end;
  Th_Engine_Marshal_Pool__.Remove_P(Pool_Ptr);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Marshal.Lock;
begin
  FCritical.Lock;
end;

procedure TZDB2_Th_Engine_Marshal.UnLock;
begin
  FCritical.UnLock;
end;

procedure TZDB2_Th_Engine_Marshal.Build(Data_Class: TZDB2_Th_Engine_Data_Class);
begin
  Current_Data_Class := Data_Class;
  Build();
end;

procedure TZDB2_Th_Engine_Marshal.Build();
begin
  if Engine_Pool.Num > 0 then
    begin
      Lock;
      try
        with Engine_Pool.Repeat_ do
          repeat
              Queue^.Data.Build(Current_Data_Class);
          until not Next;
      except
      end;
      UnLock;
    end;
end;

function TZDB2_Th_Engine_Marshal.Check_Engine: Boolean;
var
  ready_num: Integer;
begin
  ready_num := 0;
  if Engine_Pool.Num > 0 then
    begin
      with Engine_Pool.Repeat_ do
        repeat
          if Queue^.Data.Engine <> nil then
              Inc(ready_num);
        until not Next;
    end;
  Result := (Engine_Pool.Num > 0) and (Engine_Pool.Num = ready_num);
end;

procedure TZDB2_Th_Engine_Marshal.Update_Data_Ptr();
begin
  Lock;
  try
    // update FOwner_Data_Ptr
    Data_Marshal.Lock;
    if Data_Marshal.Num > 0 then
      with Data_Marshal.Repeat_ do
        repeat
          Queue^.Data.FOwner := self;
          Queue^.Data.FOwner_Data_Ptr := Queue;
        until not Next;
    Data_Marshal.UnLock;

    // update FTh_Engine_Data_Ptr
    if Engine_Pool.Num > 0 then
      with Engine_Pool.Repeat_ do
        repeat
            Queue^.Data.Update_Engine_Data_Ptr;
        until not Next;
  except
  end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Marshal.Sort_C(OnSort: TZDB2_Th_Engine_Marshal_BigList___.TSort_C);
begin
  Lock;
  try
      Data_Marshal.Sort_C(OnSort);
  except
  end;
  UnLock;
  Update_Data_Ptr();
end;

procedure TZDB2_Th_Engine_Marshal.Sort_M(OnSort: TZDB2_Th_Engine_Marshal_BigList___.TSort_M);
begin
  Lock;
  try
      Data_Marshal.Sort_M(OnSort);
  except
  end;
  UnLock;
  Update_Data_Ptr();
end;

procedure TZDB2_Th_Engine_Marshal.Sort_P(OnSort: TZDB2_Th_Engine_Marshal_BigList___.TSort_P);
begin
  Lock;
  try
      Data_Marshal.Sort_P(OnSort);
  except
  end;
  UnLock;
  Update_Data_Ptr();
end;

procedure TZDB2_Th_Engine_Marshal.Clear;
begin
  Flush;
  Lock;
  try
      Data_Marshal.Clear;
  except
  end;
  UnLock;
  Check_Recycle_Pool;
end;

function TZDB2_Th_Engine_Marshal.Database_Size: Int64;
begin
  Result := 0;
  if Engine_Pool.Num > 0 then
    begin
      with Engine_Pool.Repeat_ do
        repeat
          if Queue^.Data.Engine <> nil then
              Inc(Result, Queue^.Data.Engine.CoreSpace_Size);
        until not Next;
    end;
end;

function TZDB2_Th_Engine_Marshal.Database_Physics_Size: Int64;
begin
  Result := 0;
  if Engine_Pool.Num > 0 then
    begin
      with Engine_Pool.Repeat_ do
        repeat
          if Queue^.Data.Engine <> nil then
              Inc(Result, Queue^.Data.Engine.CoreSpace_Physics_Size);
        until not Next;
    end;
end;

function TZDB2_Th_Engine_Marshal.Total: NativeInt;
begin
  Result := Data_Marshal.Num;
end;

function TZDB2_Th_Engine_Marshal.QueueNum: NativeInt;
begin
  Result := 0;
  if Engine_Pool.Num > 0 then
    begin
      with Engine_Pool.Repeat_ do
        repeat
          if Queue^.Data.Engine <> nil then
              Inc(Result, Queue^.Data.Engine.QueueNum);
        until not Next;
    end;
end;

function TZDB2_Th_Engine_Marshal.Add_Data_To_Minimize_Workload_Engine(): TZDB2_Th_Engine_Data;
var
  Eng_: TZDB2_Th_Engine;
begin
  Result := nil;
  Lock;
  try
    repeat
        Eng_ := Engine_Pool.Get_Minimize_Workload_Engine;
    until (Eng_ = nil) or (not Eng_.OnlyRead);

    if Eng_ <> nil then
        Result := Eng_.Add(Current_Data_Class);
  except
  end;
  UnLock;
end;

function TZDB2_Th_Engine_Marshal.Add_Data_To_Minimize_Size_Engine(): TZDB2_Th_Engine_Data;
var
  Eng_: TZDB2_Th_Engine;
begin
  Result := nil;
  Lock;
  try
    repeat
        Eng_ := Engine_Pool.Get_Minimize_Size_Engine;
    until (Eng_ = nil) or (not Eng_.OnlyRead);

    if Eng_ <> nil then
        Result := Eng_.Add(Current_Data_Class);
  except
  end;
  UnLock;
end;

function TZDB2_Th_Engine_Marshal.Add_Data_To_Engine(Eng_: TZDB2_Th_Engine): TZDB2_Th_Engine_Data;
begin
  Result := nil;
  Lock;
  try
      Result := Eng_.Add(Current_Data_Class)
  except
  end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Marshal.Wait_Busy_Task;
begin
  while QueueNum + FLong_Loop_Num > 0 do
      TCompute.Sleep(10);
end;

procedure TZDB2_Th_Engine_Marshal.Check_Recycle_Pool;
begin
  Lock;
  if (FLong_Loop_Num <= 0) then
    begin
      AtomInc(FLong_Loop_Num);

      // link recycle
      Data_Link_Recycle_Tool.Lock;
      if Data_Link_Recycle_Tool.Num > 0 then
        begin
          with Data_Link_Recycle_Tool.Repeat_ do
            repeat
              try
                if (Queue^.Data.FTh_Engine <> nil) and (Queue^.Data.FTh_Engine_Data_Ptr <> nil) then
                  begin
                    Queue^.Data.FTh_Engine.Th_Engine_Data_Pool.Push_To_Recycle_Pool(Queue^.Data.FTh_Engine_Data_Ptr);
                    Queue^.Data.FTh_Engine := nil;
                    Queue^.Data.FTh_Engine_Data_Ptr := nil;
                  end
                else if (Queue^.Data.FOwner <> nil) and (Queue^.Data.FOwner_Data_Ptr <> nil) then
                  begin
                    Data_Marshal.Push_To_Recycle_Pool(Queue^.Data.FOwner_Data_Ptr);
                    Queue^.Data.FOwner := nil;
                    Queue^.Data.FOwner_Data_Ptr := nil;
                  end;
              except
              end;
              Data_Link_Recycle_Tool.Push_To_Recycle_Pool(Queue);
            until not Next;
        end;
      Data_Link_Recycle_Tool.Free_Recycle_Pool;
      Data_Link_Recycle_Tool.UnLock;

      // free thread engine recycle pool
      if Engine_Pool.Num > 0 then
        with Engine_Pool.Repeat_ do
          repeat
            Queue^.Data.Th_Engine_Data_Pool.Lock;
            try
                Queue^.Data.Th_Engine_Data_Pool.Free_Recycle_Pool;
            except
            end;
            Queue^.Data.Th_Engine_Data_Pool.UnLock;
          until not Next;
      // free local recycle pool
      Data_Marshal.Lock;
      try
          Data_Marshal.Free_Recycle_Pool;
      except
      end;
      Data_Marshal.UnLock;

      // recycle tool
      Instance_Recycle_Tool.Lock;
      try
        if Instance_Recycle_Tool.Num > 0 then
          begin
            with Instance_Recycle_Tool.Repeat_ do
              repeat
                if Queue^.Data = nil then
                    Instance_Recycle_Tool.Push_To_Recycle_Pool(Queue)
                else if Queue^.Data.Can_Free then
                  begin
                    Queue^.Data.FPost_Free_Runing := True;
                    DisposeObject(Queue^.Data);
                    Queue^.Data := nil;
                    Instance_Recycle_Tool.Push_To_Recycle_Pool(Queue);
                  end;
              until not Next;
            Instance_Recycle_Tool.Free_Recycle_Pool;
          end;
      except
      end;
      Instance_Recycle_Tool.UnLock;
      AtomDec(FLong_Loop_Num);
    end;
  UnLock;
end;

function TZDB2_Th_Engine_Marshal.Progress: Boolean;
begin
  Result := False;
  Lock;
  if (FLong_Loop_Num <= 0) and (Total > 0) then
    begin
      AtomInc(FLong_Loop_Num);
      try
        if Engine_Pool.Num > 0 then
          with Engine_Pool.Repeat_ do
            repeat
                Queue^.Data.Progress;
            until not Next;
      except
      end;
      AtomDec(FLong_Loop_Num);
      Result := True;
    end;
  UnLock;
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.Backup(Reserve_: Word);
begin
  Wait_Busy_Task;
  Check_Recycle_Pool;
  if Engine_Pool.Num > 0 then
    begin
      Lock;
      try
        with Engine_Pool.Repeat_ do
          repeat
            try
                Queue^.Data.Backup(Reserve_);
            except
            end;
          until not Next;
      except
      end;
      UnLock;
    end;
  Wait_Busy_Task;
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.Flush;
begin
  Wait_Busy_Task;
  Check_Recycle_Pool;
  if Engine_Pool.Num > 0 then
    begin
      Lock;
      try
        with Engine_Pool.Repeat_ do
          repeat
            try
                Queue^.Data.Flush;
            except
            end;
          until not Next;
      except
      end;
      UnLock;
    end;
  Wait_Busy_Task;
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.Format_Database;
begin
  Wait_Busy_Task;
  Check_Recycle_Pool;
  Lock;
  try
    Data_Marshal.Clear;
    if Engine_Pool.Num > 0 then
      begin
        with Engine_Pool.Repeat_ do
          repeat
              Queue^.Data.Format_Database;
          until not Next;
      end;
  except
  end;
  UnLock;
  Build(Current_Data_Class);
end;

procedure TZDB2_Th_Engine_Marshal.Parallel_Load_C(ThNum_: Integer; On_Run: TZDB2_Th_Engine_On_Data_Event_C; On_Wait: TOn_Wait_C);
var
  Load_Inst_: TZDB2_Th_Engine_Load_Processor;
begin
  Check_Recycle_Pool;
  if Data_Marshal.Num <= 0 then
      exit;

  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  Load_Inst_ := TZDB2_Th_Engine_Load_Processor.Create(umlMin(Data_Marshal.Num shr 4, ThNum_));
  Load_Inst_.tatal_data_num_ := Data_Marshal.Num;
  Load_Inst_.buff := Data_Marshal.BuildArrayMemory();
  Load_Inst_.OnRun_C := On_Run;
  Data_Marshal.UnLock;
  UnLock;
  try
    Load_Inst_.Run();
    Load_Inst_.Wait_C(On_Wait);
    DisposeObject(Load_Inst_);
  except
  end;
  AtomDec(FLong_Loop_Num);
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.Parallel_Load_M(ThNum_: Integer; On_Run: TZDB2_Th_Engine_On_Data_Event_M; On_Wait: TOn_Wait_M);
var
  Load_Inst_: TZDB2_Th_Engine_Load_Processor;
begin
  Check_Recycle_Pool;
  if Data_Marshal.Num <= 0 then
      exit;

  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  Load_Inst_ := TZDB2_Th_Engine_Load_Processor.Create(umlMin(Data_Marshal.Num shr 4, ThNum_));
  Load_Inst_.tatal_data_num_ := Data_Marshal.Num;
  Load_Inst_.buff := Data_Marshal.BuildArrayMemory();
  Load_Inst_.OnRun_M := On_Run;
  Data_Marshal.UnLock;
  UnLock;
  try
    Load_Inst_.Run();
    Load_Inst_.Wait_M(On_Wait);
    DisposeObject(Load_Inst_);
  except
  end;
  AtomDec(FLong_Loop_Num);
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.Parallel_Load_P(ThNum_: Integer; On_Run: TZDB2_Th_Engine_On_Data_Event_P; On_Wait: TOn_Wait_P);
var
  Load_Inst_: TZDB2_Th_Engine_Load_Processor;
begin
  Check_Recycle_Pool;
  if Data_Marshal.Num <= 0 then
      exit;

  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  Load_Inst_ := TZDB2_Th_Engine_Load_Processor.Create(umlMin(Data_Marshal.Num shr 4, ThNum_));
  Load_Inst_.tatal_data_num_ := Data_Marshal.Num;
  Load_Inst_.buff := Data_Marshal.BuildArrayMemory();
  Load_Inst_.OnRun_P := On_Run;
  Data_Marshal.UnLock;
  UnLock;

  try
    Load_Inst_.Run();
    Load_Inst_.Wait_P(On_Wait);
    DisposeObject(Load_Inst_);
  except
  end;
  AtomDec(FLong_Loop_Num);
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.For_C(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_Marshal_For_C);
var
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Marshal_BigList___.PQueueArrayStruct;
  Aborted: Boolean;

{$IFDEF FPC}
  procedure fpc_ParallelFor(pass: Int64);
  var
    inst: TZDB2_Th_Engine_Data;
    Can_Load: Boolean;
  begin
    if Aborted then
        exit;
    if not Assigned(On_Run) then
        exit;
    try
      inst := buff^[pass]^.Data;
      if inst = nil then
          exit;
    except
        Aborted := True;
    end;

    if Aborted then
        exit;
    inst.Lock;
    try
        Can_Load := inst.Can_Load;
    except
        Aborted := True;
    end;
    inst.UnLock;

    try
      if Can_Load and (not Aborted) then
          On_Run(inst, pass, Aborted);
    except
        Aborted := True;
    end;
  end;
{$ENDIF FPC}


begin
  Check_Recycle_Pool;
  if Data_Marshal.Num <= 0 then
      exit;
  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  tatal_data_num_ := Data_Marshal.Num;
  buff := Data_Marshal.BuildArrayMemory();
  Data_Marshal.UnLock;
  UnLock;
  Aborted := False;

{$IFDEF FPC}
  ParallelFor(ThNum_, Parallel_, 0, tatal_data_num_ - 1, @fpc_ParallelFor);
{$ELSE FPC}
  ParallelFor(ThNum_, Parallel_, 0, tatal_data_num_ - 1, procedure(pass: Int64)
    var
      inst: TZDB2_Th_Engine_Data;
      Can_Load: Boolean;
    begin
      if Aborted then
          exit;
      if not Assigned(On_Run) then
          exit;
      try
        inst := buff^[pass]^.Data;
        if inst = nil then
            exit;
      except
          Aborted := True;
      end;

      if Aborted then
          exit;
      inst.Lock;
      try
          Can_Load := inst.Can_Load;
      except
          Aborted := True;
      end;
      inst.UnLock;

      try
        if Can_Load and (not Aborted) then
            On_Run(inst, pass, Aborted);
      except
          Aborted := True;
      end;
    end);
{$ENDIF FPC}
  AtomDec(FLong_Loop_Num);
  System.FreeMemory(buff);
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.For_M(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_Marshal_For_M);
var
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Marshal_BigList___.PQueueArrayStruct;
  Aborted: Boolean;

{$IFDEF FPC}
  procedure fpc_ParallelFor(pass: Int64);
  var
    inst: TZDB2_Th_Engine_Data;
    Can_Load: Boolean;
  begin
    if Aborted then
        exit;
    if not Assigned(On_Run) then
        exit;
    try
      inst := buff^[pass]^.Data;
      if inst = nil then
          exit;
    except
        Aborted := True;
    end;

    if Aborted then
        exit;
    inst.Lock;
    try
        Can_Load := inst.Can_Load;
    except
        Aborted := True;
    end;
    inst.UnLock;

    try
      if Can_Load and (not Aborted) then
          On_Run(inst, pass, Aborted);
    except
        Aborted := True;
    end;
  end;
{$ENDIF FPC}


begin
  Check_Recycle_Pool;
  if Data_Marshal.Num <= 0 then
      exit;
  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  tatal_data_num_ := Data_Marshal.Num;
  buff := Data_Marshal.BuildArrayMemory();
  Data_Marshal.UnLock;
  UnLock;
  Aborted := False;

{$IFDEF FPC}
  ParallelFor(ThNum_, Parallel_, 0, tatal_data_num_ - 1, @fpc_ParallelFor);
{$ELSE FPC}
  ParallelFor(ThNum_, Parallel_, 0, tatal_data_num_ - 1, procedure(pass: Int64)
    var
      inst: TZDB2_Th_Engine_Data;
      Can_Load: Boolean;
    begin
      if Aborted then
          exit;
      if not Assigned(On_Run) then
          exit;
      try
        inst := buff^[pass]^.Data;
        if inst = nil then
            exit;
      except
          Aborted := True;
      end;

      if Aborted then
          exit;
      inst.Lock;
      try
          Can_Load := inst.Can_Load;
      except
          Aborted := True;
      end;
      inst.UnLock;

      try
        if Can_Load and (not Aborted) then
            On_Run(inst, pass, Aborted);
      except
          Aborted := True;
      end;
    end);
{$ENDIF FPC}
  AtomDec(FLong_Loop_Num);
  System.FreeMemory(buff);
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.For_P(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_Marshal_For_P);
var
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Marshal_BigList___.PQueueArrayStruct;
  Aborted: Boolean;

{$IFDEF FPC}
  procedure fpc_ParallelFor(pass: Int64);
  var
    inst: TZDB2_Th_Engine_Data;
    Can_Load: Boolean;
  begin
    if Aborted then
        exit;
    if not Assigned(On_Run) then
        exit;
    try
      inst := buff^[pass]^.Data;
      if inst = nil then
          exit;
    except
        Aborted := True;
    end;

    if Aborted then
        exit;
    inst.Lock;
    try
        Can_Load := inst.Can_Load;
    except
        Aborted := True;
    end;
    inst.UnLock;

    try
      if Can_Load and (not Aborted) then
          On_Run(inst, pass, Aborted);
    except
        Aborted := True;
    end;
  end;
{$ENDIF FPC}


begin
  Check_Recycle_Pool;
  if Data_Marshal.Num <= 0 then
      exit;
  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  tatal_data_num_ := Data_Marshal.Num;
  buff := Data_Marshal.BuildArrayMemory();
  Data_Marshal.UnLock;
  UnLock;
  Aborted := False;

{$IFDEF FPC}
  ParallelFor(ThNum_, Parallel_, 0, tatal_data_num_ - 1, @fpc_ParallelFor);
{$ELSE FPC}
  ParallelFor(ThNum_, Parallel_, 0, tatal_data_num_ - 1, procedure(pass: Int64)
    var
      inst: TZDB2_Th_Engine_Data;
      Can_Load: Boolean;
    begin
      if Aborted then
          exit;
      if not Assigned(On_Run) then
          exit;
      try
        inst := buff^[pass]^.Data;
        if inst = nil then
            exit;
      except
          Aborted := True;
      end;

      if Aborted then
          exit;
      inst.Lock;
      try
          Can_Load := inst.Can_Load;
      except
          Aborted := True;
      end;
      inst.UnLock;

      try
        if Can_Load and (not Aborted) then
            On_Run(inst, pass, Aborted);
      except
          Aborted := True;
      end;
    end);
{$ENDIF FPC}
  AtomDec(FLong_Loop_Num);
  System.FreeMemory(buff);
  Check_Recycle_Pool;
end;

function TZDB2_Th_Engine_Marshal.GetRemoveDatabaseOnDestroy: Boolean;
begin
  Result := False;
  if Engine_Pool.Num > 0 then
    begin
      Lock;
      try
        with Engine_Pool.Repeat_ do
          repeat
              Result := Result or Queue^.Data.RemoveDatabaseOnDestroy;
          until not Next;
      finally
          UnLock;
      end;
    end;
end;

procedure TZDB2_Th_Engine_Marshal.SetRemoveDatabaseOnDestroy(const Value: Boolean);
begin
  if Engine_Pool.Num > 0 then
    begin
      Lock;
      try
        with Engine_Pool.Repeat_ do
          repeat
              Queue^.Data.RemoveDatabaseOnDestroy := Value;
          until not Next;
      finally
          UnLock;
      end;
    end;
end;

class procedure TZDB2_Th_Engine_Marshal.Test;
const
  C_cfg = '[1]'#13#10 +
    'database='#13#10 +
    'OnlyRead=False'#13#10 +
    'Delta=100*1024*1024'#13#10 +
    'BlockSize=1536'#13#10 +
    'Security=None'#13#10 +
    'Password=ZDB_2.0'#13#10 +
    'Level=1'#13#10 +
    'Tail=True'#13#10 +
    'CBC=True'#13#10 +
    #13#10 +
    '[2]'#13#10 +
    'database='#13#10 +
    'OnlyRead=False'#13#10 +
    'Delta=100*1024*1024'#13#10 +
    'BlockSize=1536'#13#10 +
    'Security=None'#13#10 +
    'Password=ZDB_2.0'#13#10 +
    'Level=1'#13#10 +
    'Tail=True'#13#10 +
    'CBC=True'#13#10 +
    #13#10 +
    '[3]'#13#10 +
    'database='#13#10 +
    'OnlyRead=False'#13#10 +
    'Delta=100*1024*1024'#13#10 +
    'BlockSize=1536'#13#10 +
    'Security=None'#13#10 +
    'Password=ZDB_2.0'#13#10 +
    'Level=1'#13#10 +
    'Tail=True'#13#10 +
    'CBC=True'#13#10 +
    #13#10 +
    '[4]'#13#10 +
    'database='#13#10 +
    'OnlyRead=False'#13#10 +
    'Delta=100*1024*1024'#13#10 +
    'BlockSize=1536'#13#10 +
    'Security=None'#13#10 +
    'Password=ZDB_2.0'#13#10 +
    'Level=1'#13#10 +
    'Tail=True'#13#10 +
    'CBC=True'#13#10;

var
  DM: TZDB2_Th_Engine_Marshal;
  TE: THashTextEngine;
  L: TListPascalString;
  Eng_: TZDB2_Th_Engine;
  i: Integer;
  tmp: TMem64;
begin
  DM := TZDB2_Th_Engine_Marshal.Create;
  TE := THashTextEngine.Create;
  TE.AsText := C_cfg;

  L := TListPascalString.Create;
  TE.GetSectionList(L);
  for i := 0 to L.Count - 1 do
    begin
      Eng_ := TZDB2_Th_Engine.Create(DM);
      Eng_.ReadConfig(L[i], TE.HStringList[L[i]]);
    end;
  DisposeObject(L);
  TE.Free;
  DM.Build;

  DM.Engine_Pool.Get_Minimize_Size_Engine;

  for i := 0 to 10000 do
    begin
      tmp := TMem64.Create;
      tmp.Size := umlRandomRange(1192, 8192);
      MT19937Rand32(MaxInt, tmp.Memory, tmp.Size shr 2);
      DM.Add_Data_To_Minimize_Workload_Engine.Async_Save_And_Free_Data(tmp);
      while DM.QueueNum > 1000 do
          TCompute.Sleep(1);
    end;

  DM.Parallel_Load_C(4, nil, nil);

  DoStatus('db total:%d', [DM.Total]);

  DisposeObject(DM);
end;

class procedure TZDB2_Th_Engine_Marshal.Test_Backup_Support;
const
  C_cfg = '[1]'#13#10 +
    'database=%temp%db1.ox2'#13#10 +
    'OnlyRead=False'#13#10 +
    'Delta=100*1024*1024'#13#10 +
    'BlockSize=1536'#13#10 +
    'Security=None'#13#10 +
    'Password=ZDB_2.0'#13#10 +
    'Level=1'#13#10 +
    'Tail=True'#13#10 +
    'CBC=True'#13#10 +
    #13#10 +
    '[2]'#13#10 +
    'database=%temp%db2.ox2'#13#10 +
    'OnlyRead=False'#13#10 +
    'Delta=100*1024*1024'#13#10 +
    'BlockSize=1536'#13#10 +
    'Security=None'#13#10 +
    'Password=ZDB_2.0'#13#10 +
    'Level=1'#13#10 +
    'Tail=True'#13#10 +
    'CBC=True'#13#10 +
    #13#10 +
    '[3]'#13#10 +
    'database=%temp%db3.ox2'#13#10 +
    'OnlyRead=False'#13#10 +
    'Delta=100*1024*1024'#13#10 +
    'BlockSize=1536'#13#10 +
    'Security=None'#13#10 +
    'Password=ZDB_2.0'#13#10 +
    'Level=1'#13#10 +
    'Tail=True'#13#10 +
    'CBC=True'#13#10 +
    #13#10 +
    '[4]'#13#10 +
    'database=%temp%db4.ox2'#13#10 +
    'OnlyRead=False'#13#10 +
    'Delta=100*1024*1024'#13#10 +
    'BlockSize=1536'#13#10 +
    'Security=None'#13#10 +
    'Password=ZDB_2.0'#13#10 +
    'Level=1'#13#10 +
    'Tail=True'#13#10 +
    'CBC=True'#13#10;

var
  DM: TZDB2_Th_Engine_Marshal;
  TE: THashTextEngine;
  L: TListPascalString;
  Eng_: TZDB2_Th_Engine;
  i: Integer;
  tmp: TMem64;
begin
  DM := TZDB2_Th_Engine_Marshal.Create;
  TE := THashTextEngine.Create;
  TE.AsText := umlReplace(C_cfg, '%temp%', umlCurrentPath, False, True);

  L := TListPascalString.Create;
  TE.GetSectionList(L);
  for i := 0 to L.Count - 1 do
    begin
      Eng_ := TZDB2_Th_Engine.Create(DM);
      Eng_.ReadConfig(L[i], TE.HStringList[L[i]]);
    end;
  DisposeObject(L);
  TE.Free;
  DM.Build;
  DM.RemoveDatabaseOnDestroy := True;

  DM.Engine_Pool.Get_Minimize_Size_Engine;

  for i := 0 to 10000 do
    begin
      tmp := TMem64.Create;
      tmp.Size := umlRandomRange(1192, 8192);
      MT19937Rand32(MaxInt, tmp.Memory, tmp.Size shr 2);
      DM.Add_Data_To_Minimize_Workload_Engine.Async_Save_And_Free_Data(tmp);
      while DM.QueueNum > 1000 do
          TCompute.Sleep(1);
    end;

  DM.Wait_Busy_Task;
  DM.Parallel_Load_C(4, nil, nil);
  DM.Wait_Busy_Task;

  for i := 0 to 10 do
    begin
      DM.Backup(3);
      DM.Wait_Busy_Task;
      TCompute.Sleep(1000);
    end;

  DoStatus('db total:%d', [DM.Total]);

  DisposeObject(DM);
end;

initialization

Th_Engine_Marshal_Pool__ := TZDB2_Th_Engine_Marshal_Pool.Create;

finalization

disposeObjectAndNil(Th_Engine_Marshal_Pool__);

end.
