{ ****************************************************************************** }
{ * ZDB 2.0 Core-Thread for HPC                                                * }
{ ****************************************************************************** }
unit Z.ZDB2.Thread;

{$I Z.Define.inc}

interface

uses Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib,
  Z.MemoryStream,
  Z.Status, Z.Cipher, Z.ZDB2, Z.ListEngine, Z.TextDataEngine, Z.IOThread,
  Z.Notify, Z.ZDB2.Thread.Queue;

type
  TZDB2_Th_Engine_Marshal = class;
  TZDB2_Th_Engine_Data = class;
  TZDB2_Th_Engine = class;
  TZDB2_Th_Engine_Data_BigList__ = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalBigList<TZDB2_Th_Engine_Data>;
  TZDB2_Th_Engine_Marshal_BigList__ = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalBigList<TZDB2_Th_Engine_Data>;

  TZDB2_Th_Engine_Data = class(TCore_InterfacedObject)
  private
    FOwner: TZDB2_Th_Engine_Marshal;
    FOwner_Data_Ptr: TZDB2_Th_Engine_Marshal_BigList__.PQueueStruct;
    FTh_Engine: TZDB2_Th_Engine;
    FTh_Engine_Data_Ptr: TZDB2_Th_Engine_Data_BigList__.PQueueStruct;
    FID: Integer;
    FBusy_Task_Num: Integer;
    FPost_Free_Runing: Boolean;
    procedure Do_Save_ID_And_State_Event(var Sender: TZDB2_Th_CMD_ID_And_State);
    procedure Wait_Busy_Done;
    procedure Do_Post_To_Pool_Free();
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Progress(); virtual;
    // share
    function IsOnlyRead: Boolean;
    function Engine: TZDB2_Th_Queue;
    property Th_Engine: TZDB2_Th_Engine read FTh_Engine;
    property ID: Integer read FID;
    property Busy_Task_Num: Integer read FBusy_Task_Num;
    // position
    procedure MoveToLast;
    procedure MoveToFirst;
    // assync delete and delay free
    procedure Remove(Delete_Data_: Boolean);
    // sync load.
    function Load_Data(Stream: TMS64): Boolean; overload;
    function Load_Data(Mem64: TMem64): Boolean; overload;
    // hint: stream is auto free
    procedure Async_Save_And_Free_Data(Stream: TMS64); overload;
    // hint: mem64 is auto free
    procedure Async_Save_And_Free_Data(Mem64: TMem64); overload;
  end;

  TZDB2_Th_Engine_Data_Class = class of TZDB2_Th_Engine_Data;

  TZDB2_Th_Engine = class(TCore_InterfacedObject)
  private
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
    Engine: TZDB2_Th_Queue;
    Th_Engine_Data_Pool: TZDB2_Th_Engine_Data_BigList__;
    Last_Build_Class: TZDB2_Th_Engine_Data_Class;
    constructor Create(Owner_: TZDB2_Th_Engine_Marshal);
    destructor Destroy; override;
    procedure ReadConfig(Name_: U_String; cfg: THashStringList);
    procedure WriteConfig(cfg: THashStringList);
    procedure Clear;
    procedure Format_Database;
    procedure Build(Data_Class: TZDB2_Th_Engine_Data_Class);
    procedure Rebuild_Data_Pool(Data_Class: TZDB2_Th_Engine_Data_Class);
    function Flush: Boolean;
    function Add(Data_Class: TZDB2_Th_Engine_Data_Class; ID: Integer): TZDB2_Th_Engine_Data; overload;
    function Add(Data_Class: TZDB2_Th_Engine_Data_Class): TZDB2_Th_Engine_Data; overload;
    procedure Progress;
    function Get_Busy_Task_Num: Int64;
  end;

  TZDB2_Th_Engine_List_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalBigList<TZDB2_Th_Engine>;

  TZDB2_Th_Engine_List = class(TZDB2_Th_Engine_List_Decl)
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

  TZDB2_Th_Engine_Data_Load_Instance = class(TIO_Thread_Data)
  private
    FStream: TMS64;
    FTh_Pool: TIO_Thread_Base;
    FData: TZDB2_Th_Engine_Data;
    FOnRun_C: TZDB2_Th_Engine_On_Data_Event_C;
    FOnRun_M: TZDB2_Th_Engine_On_Data_Event_M;
    FOnRun_P: TZDB2_Th_Engine_On_Data_Event_P;
    procedure Do_Read_Stream_Result(var Sender: TZDB2_Th_CMD_Stream_And_State);
  public
    property Stream: TMS64 read FStream;
    property Data: TZDB2_Th_Engine_Data read FData;
    constructor Create(Th_Pool_: TIO_Thread_Base; Data_: TZDB2_Th_Engine_Data);
    destructor Destroy; override;
    procedure Process; override;
    procedure Run;
  end;

  TZDB2_Th_Engine_Load_Processor = class;

  TOn_Wait_C = procedure(Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance);
  TOn_Wait_M = procedure(Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance) of object;
{$IFDEF FPC}
  TOn_Wait_P = procedure(Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance) is nested;
{$ELSE FPC}
  TOn_Wait_P = reference to procedure(Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance);
{$ENDIF FPC}

  TZDB2_Th_Engine_Load_Processor = class
  private
    IsBusy: Boolean;
    OnRun_C: TZDB2_Th_Engine_On_Data_Event_C;
    OnRun_M: TZDB2_Th_Engine_On_Data_Event_M;
    OnRun_P: TZDB2_Th_Engine_On_Data_Event_P;
    FTh_Pool: TIO_Thread;
    FMax_Queue: Integer;
    procedure Do_ThRun_Marshal_Load(ThSender: TCompute);
  public
    constructor Create(ThNum_: Integer);
    destructor Destroy; override;
    procedure Run(Marshal_: TZDB2_Th_Engine_Marshal); overload;
    procedure Wait();
    procedure Wait_C(On_Wait: TOn_Wait_C);
    procedure Wait_M(On_Wait: TOn_Wait_M);
    procedure Wait_P(On_Wait: TOn_Wait_P);
  end;

  TZDB2_Th_Engine_Marshal_Pool = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalBigList<TZDB2_Th_Engine_Marshal>;

  TZDB2_Th_Engine_Marshal = class(TCore_InterfacedObject)
  private
    Pool_Ptr: TZDB2_Th_Engine_Marshal_Pool.PQueueStruct;
    procedure DoFree(var Data: TZDB2_Th_Engine_Data);
  public
    Data_Marshal: TZDB2_Th_Engine_Marshal_BigList__;
    Engine_Pool: TZDB2_Th_Engine_List;
    Current_Data_Class: TZDB2_Th_Engine_Data_Class;
    constructor Create();
    destructor Destroy; override;
    // load data as TZDB2_Th_Engine_Data_Class
    procedure Build(Data_Class: TZDB2_Th_Engine_Data_Class);
    // clear
    procedure Clear;
    // database space state
    function Database_Size: Int64;
    function Database_Physics_Size: Int64;
    // data num
    function Total: NativeInt;
    // task queue
    function QueueNum: NativeInt;
    // queue task num
    function Get_Busy_Task_Num: Int64;
    // pick engine
    function Add_Data_To_Minimize_Workload_Engine(): TZDB2_Th_Engine_Data; overload;
    function Add_Data_To_Minimize_Size_Engine(): TZDB2_Th_Engine_Data; overload;
    // wait busy task
    procedure Wait_Busy_Task;
    // check recycle pool
    procedure Check_Recycle_Pool;
    // flush
    procedure Flush;
    // remove and rebuild datgabase
    procedure Format_Database;
    // parallel load
    procedure Parallel_Load_C(ThNum_, Max_Queue_: Integer; On_Run: TZDB2_Th_Engine_On_Data_Event_C; On_Wait: TOn_Wait_C);
    procedure Parallel_Load_M(ThNum_, Max_Queue_: Integer; On_Run: TZDB2_Th_Engine_On_Data_Event_M; On_Wait: TOn_Wait_M);
    procedure Parallel_Load_P(ThNum_, Max_Queue_: Integer; On_Run: TZDB2_Th_Engine_On_Data_Event_P; On_Wait: TOn_Wait_P);
    // progress
    procedure Progress;
    // RemoveDatabaseOnDestroy
    function GetRemoveDatabaseOnDestroy: Boolean;
    procedure SetRemoveDatabaseOnDestroy(const Value: Boolean);
    property RemoveDatabaseOnDestroy: Boolean read GetRemoveDatabaseOnDestroy write SetRemoveDatabaseOnDestroy;
    // test
    class procedure Test();
  end;

var
  Th_Engine_Marshal_Pool__: TZDB2_Th_Engine_Marshal_Pool;

implementation

uses Z.Expression;

procedure TZDB2_Th_Engine_Data.Do_Save_ID_And_State_Event(var Sender: TZDB2_Th_CMD_ID_And_State);
begin
  if Sender.State = TCMD_State.csDone then
      FID := Sender.ID
  else
      FID := -1;
  AtomDec(FBusy_Task_Num);
  if FBusy_Task_Num > 0 then
      exit;
  if Sender.State = TCMD_State.csError then
      Do_Post_To_Pool_Free;
end;

procedure TZDB2_Th_Engine_Data.Wait_Busy_Done;
begin
  while FBusy_Task_Num > 0 do
      TCompute.Sleep(10);
end;

procedure TZDB2_Th_Engine_Data.Do_Post_To_Pool_Free;
begin
  if FPost_Free_Runing then
      exit;

  if (FTh_Engine <> nil) and (FTh_Engine_Data_Ptr <> nil) then
    begin
      FTh_Engine.Th_Engine_Data_Pool.Push_To_Recycle_Pool(FTh_Engine_Data_Ptr);
      FTh_Engine := nil;
      FTh_Engine_Data_Ptr := nil;
      FPost_Free_Runing := True;
    end
  else if (FOwner <> nil) and (FOwner_Data_Ptr <> nil) then
    begin
      FOwner.Data_Marshal.Push_To_Recycle_Pool(FOwner_Data_Ptr);
      FOwner := nil;
      FOwner_Data_Ptr := nil;
      FPost_Free_Runing := True;
    end;
end;

constructor TZDB2_Th_Engine_Data.Create;
begin
  inherited Create;
  FOwner := nil;
  FOwner_Data_Ptr := nil;
  FTh_Engine := nil;
  FTh_Engine_Data_Ptr := nil;
  FID := -1;
  FBusy_Task_Num := 0;
  FPost_Free_Runing := False;
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

procedure TZDB2_Th_Engine_Data.MoveToLast;
begin
  if (FTh_Engine <> nil) and (FTh_Engine_Data_Ptr <> nil) then
      FTh_Engine.Th_Engine_Data_Pool.MoveToLast(FTh_Engine_Data_Ptr);
  if (FOwner <> nil) and (FOwner_Data_Ptr <> nil) then
      FOwner.Data_Marshal.MoveToLast(FOwner_Data_Ptr);
end;

procedure TZDB2_Th_Engine_Data.MoveToFirst;
begin
  if (FTh_Engine <> nil) and (FTh_Engine_Data_Ptr <> nil) then
      FTh_Engine.Th_Engine_Data_Pool.MoveToFirst(FTh_Engine_Data_Ptr);
  if (FOwner <> nil) and (FOwner_Data_Ptr <> nil) then
      FOwner.Data_Marshal.MoveToFirst(FOwner_Data_Ptr);
end;

procedure TZDB2_Th_Engine_Data.Remove(Delete_Data_: Boolean);
begin
  if FPost_Free_Runing then
      exit;

  if Delete_Data_ then
    begin
      Wait_Busy_Done;

      if (FID >= 0) then
          Engine.Async_Remove(FID);
      FID := -1;
    end;

  Do_Post_To_Pool_Free();
end;

function TZDB2_Th_Engine_Data.Load_Data(Stream: TMS64): Boolean;
begin
  Result := False;
  if FPost_Free_Runing then
      exit;
  Wait_Busy_Done;
  if (FID >= 0) then
    begin
      AtomInc(FBusy_Task_Num);
      Result := Engine.Sync_GetData(Stream, FID);
      AtomDec(FBusy_Task_Num);
    end;
end;

function TZDB2_Th_Engine_Data.Load_Data(Mem64: TMem64): Boolean;
begin
  Result := False;
  if FPost_Free_Runing then
      exit;
  Wait_Busy_Done;
  if (FID >= 0) then
    begin
      AtomInc(FBusy_Task_Num);
      Result := Engine.Sync_GetData(Mem64, FID);
      AtomDec(FBusy_Task_Num);
    end;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Data(Stream: TMS64);
begin
  if FPost_Free_Runing then
    begin
      disposeObjectAndNil(Stream);
      exit;
    end;
  Wait_Busy_Done;
  AtomInc(FBusy_Task_Num);
  Engine.Async_SetData_M(Stream, True, FID, {$IFDEF FPC}@{$ENDIF FPC}Do_Save_ID_And_State_Event);
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Data(Mem64: TMem64);
begin
  if FPost_Free_Runing then
    begin
      disposeObjectAndNil(Mem64);
      exit;
    end;
  Wait_Busy_Done;
  AtomInc(FBusy_Task_Num);
  Engine.Async_SetData_M(Mem64, True, FID, {$IFDEF FPC}@{$ENDIF FPC}Do_Save_ID_And_State_Event);
end;

procedure TZDB2_Th_Engine.DoFree(var Data: TZDB2_Th_Engine_Data);
begin
  if Data = nil then
      exit;
  Data.FTh_Engine := nil;
  Data.FTh_Engine_Data_Ptr := nil;
  Data.FID := -1;
  if (Data.FOwner <> nil) and (Data.FOwner_Data_Ptr <> nil) then
      Data.FOwner.Data_Marshal.Remove(Data.FOwner_Data_Ptr)
  else
    begin
      Data.FPost_Free_Runing := True;
      disposeObject(Data);
    end;
end;

constructor TZDB2_Th_Engine.Create(Owner_: TZDB2_Th_Engine_Marshal);
begin
  inherited Create;
  Name := '';
  Owner := Owner_;
  RemoveDatabaseOnDestroy := False;
  Mode := smNormal;
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
  Th_Engine_Data_Pool := TZDB2_Th_Engine_Data_BigList__.Create;
  Th_Engine_Data_Pool.OnFree := {$IFDEF FPC}@{$ENDIF FPC}DoFree;
  Owner.Engine_Pool.Add(self);
  Last_Build_Class := TZDB2_Th_Engine_Data;
end;

destructor TZDB2_Th_Engine.Destroy;
begin
  Flush;
  disposeObjectAndNil(Th_Engine_Data_Pool);
  disposeObjectAndNil(Engine);
  disposeObjectAndNil(Cipher);
  if RemoveDatabaseOnDestroy and umlFileExists(Database_File) then
      umlDeleteFile(Database_File);
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

procedure TZDB2_Th_Engine.Clear;
begin
  Flush;
  Th_Engine_Data_Pool.Clear;
end;

procedure TZDB2_Th_Engine.Format_Database;
begin
  if Engine = nil then
      exit;
  while Engine.QueueNum > 0 do
      TCompute.Sleep(1);
  Th_Engine_Data_Pool.Clear;
  disposeObjectAndNil(Engine);
  disposeObjectAndNil(Cipher);

  if umlFileExists(Database_File) then
      umlDeleteFile(Database_File);

  Build(Last_Build_Class);
end;

procedure TZDB2_Th_Engine.Build(Data_Class: TZDB2_Th_Engine_Data_Class);
var
  Stream: TCore_Stream;
  Queue_Table_: TZDB2_BlockHandle;
  ID: Integer;
begin
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
        Stream := TMS64.CustomCreate(128 * 1024 * 1024);
      end
    else
      begin
        Stream := TReliableFileStream.Create(Database_File, not umlFileExists(Database_File), not OnlyRead);
      end;
  except
      exit;
  end;

  try
    if (Stream.Size = 0) then
      begin
        // check stream
        if not OnlyRead then
            Engine := TZDB2_Th_Queue.Create(Mode, Stream, True, OnlyRead, Delta, BlockSize, Cipher);
      end
    else if TZDB2_Core_Space.CheckStream(Stream, Cipher) then // check open from cipher
      begin
        Engine := TZDB2_Th_Queue.Create(Mode, Stream, True, OnlyRead, Delta, BlockSize, Cipher);
        if Engine.Sync_Get_And_Clean_Sequence_Table(Queue_Table_) then
          begin
            for ID in Queue_Table_ do
                Add(Data_Class, ID);
            SetLength(Queue_Table_, 0);
          end;
      end
    else
        DoStatus('"%s" password error or data corruption.', [Database_File.Text]);
  except
  end;

  if Engine = nil then
      disposeObject(Stream)
  else
      Last_Build_Class := Data_Class;
end;

procedure TZDB2_Th_Engine.Rebuild_Data_Pool(Data_Class: TZDB2_Th_Engine_Data_Class);
var
  Queue_Table_: TZDB2_BlockHandle;
  ID: Integer;
begin
  if Engine = nil then
      exit;
  Th_Engine_Data_Pool.Clear;

  if Engine.Sync_Rebuild_Sequence_Table(Queue_Table_) then
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
begin
  if Engine = nil then
      exit(False);

  while Engine.QueueNum > 0 do
      TCompute.Sleep(1);
  Th_Engine_Data_Pool.Free_Recycle_Pool;
  Engine.Async_Flush;
  Queue_ID_List_ := TZDB2_ID_List.Create;
  if Th_Engine_Data_Pool.Num > 0 then
    begin
      with Th_Engine_Data_Pool.Repeat_ do
        repeat
          if Queue^.Data <> nil then
            begin
              if Queue^.Data.FID >= 0 then
                  Queue_ID_List_.Add(Queue^.Data.FID)
              else
                  Th_Engine_Data_Pool.Push_To_Recycle_Pool(Queue);
            end;
        until not Next;
      Th_Engine_Data_Pool.Free_Recycle_Pool;
    end;
  Result := Engine.Sync_Flush_Sequence_Table(Queue_ID_List_);
  disposeObject(Queue_ID_List_);
end;

function TZDB2_Th_Engine.Add(Data_Class: TZDB2_Th_Engine_Data_Class; ID: Integer): TZDB2_Th_Engine_Data;
var
  Data_Instance: TZDB2_Th_Engine_Data;
begin
  if Engine = nil then
      exit(nil);
  Data_Instance := Data_Class.Create();
  Data_Instance.FOwner := Owner;
  Data_Instance.FOwner_Data_Ptr := Owner.Data_Marshal.Add(Data_Instance);
  Data_Instance.FTh_Engine := self;
  Data_Instance.FTh_Engine_Data_Ptr := Th_Engine_Data_Pool.Add(Data_Instance);
  Data_Instance.FID := ID;
  Result := Data_Instance;
end;

function TZDB2_Th_Engine.Add(Data_Class: TZDB2_Th_Engine_Data_Class): TZDB2_Th_Engine_Data;
begin
  Result := Add(Data_Class, -1);
end;

procedure TZDB2_Th_Engine.Progress;
begin
  if Engine = nil then
      exit;

  if Th_Engine_Data_Pool.Num > 0 then
    with Th_Engine_Data_Pool.Repeat_ do
      repeat
        if (Queue^.Data <> nil) and (Queue^.Data.FBusy_Task_Num = 0) and (Queue^.Data.FTh_Engine <> nil) then
            Queue^.Data.Progress();
      until not Next;
end;

function TZDB2_Th_Engine.Get_Busy_Task_Num: Int64;
begin
  Result := 0;
  if Engine = nil then
      exit;
  if Th_Engine_Data_Pool.Num > 0 then
    with Th_Engine_Data_Pool.Repeat_ do
      repeat
          inc(Result, Queue^.Data.FBusy_Task_Num);
      until not Next;
end;

constructor TZDB2_Th_Engine_List.Create;
begin
  inherited Create;
end;

procedure TZDB2_Th_Engine_List.DoFree(var Data: TZDB2_Th_Engine);
begin
  disposeObjectAndNil(Data);
end;

function TZDB2_Th_Engine_List.Get_Minimize_Size_Engine: TZDB2_Th_Engine;
var
  Eng_: PQueueStruct;
begin
  Result := nil;
  if Num = 0 then
      exit;

  Eng_ := nil;

  with Repeat_ do
    repeat
      if Queue^.Data.Engine <> nil then
        begin
          if Eng_ = nil then
              Eng_ := Queue
          else if Queue^.Data.Engine.CoreSpace_Size < Eng_^.Data.Engine.CoreSpace_Size then
              Eng_ := Queue;
        end;
    until not Next;

  if Eng_ <> nil then
    begin
      Result := Eng_^.Data;
      MoveToLast(Eng_);
    end;
end;

function TZDB2_Th_Engine_List.Get_Minimize_Workload_Engine: TZDB2_Th_Engine;
var
  Eng_: PQueueStruct;
begin
  Result := nil;
  if Num = 0 then
      exit;

  Eng_ := nil;

  with Repeat_ do
    repeat
      if Queue^.Data.Engine <> nil then
        begin
          if Eng_ = nil then
              Eng_ := Queue
          else if Queue^.Data.Engine.QueueNum < Eng_^.Data.Engine.QueueNum then
              Eng_ := Queue;
        end;
    until not Next;

  if Eng_ <> nil then
    begin
      Result := Eng_^.Data;
      MoveToLast(Eng_);
    end;
end;

function TZDB2_Th_Engine_List.AllIsOnlyRead(): Boolean;
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
      FTh_Pool.Enqueue(self);
    end
  else
    begin
      DoStatus('IO Read error');
      FData.Remove(False);
      DelayFreeObj(0.1, self);
    end;
end;

constructor TZDB2_Th_Engine_Data_Load_Instance.Create(Th_Pool_: TIO_Thread_Base; Data_: TZDB2_Th_Engine_Data);
begin
  inherited Create;
  FStream := TMS64.Create;
  FTh_Pool := Th_Pool_;
  FData := Data_;
  FOnRun_C := nil;
  FOnRun_M := nil;
  FOnRun_P := nil;
end;

destructor TZDB2_Th_Engine_Data_Load_Instance.Destroy;
begin
  disposeObject(FStream);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Data_Load_Instance.Process;
begin
  FStream.Position := 0;
  if Assigned(FOnRun_C) then
      FOnRun_C(Data, FStream);
  if Assigned(FOnRun_M) then
      FOnRun_M(Data, FStream);
  if Assigned(FOnRun_P) then
      FOnRun_P(Data, FStream);
end;

procedure TZDB2_Th_Engine_Data_Load_Instance.Run;
begin
  FData.Engine.Async_GetData_AsStream_M(FData.ID, FStream, {$IFDEF FPC}@{$ENDIF FPC}Do_Read_Stream_Result);
end;

procedure TZDB2_Th_Engine_Load_Processor.Do_ThRun_Marshal_Load(ThSender: TCompute);
var
  Owner_: TZDB2_Th_Engine_Marshal;
  __Repeat__: TZDB2_Th_Engine_Marshal_BigList__.TRepeat___;
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance;
begin
  Owner_ := ThSender.UserObject as TZDB2_Th_Engine_Marshal;
  __Repeat__ := Owner_.Data_Marshal.Repeat_;
  repeat
    Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance.Create(FTh_Pool, __Repeat__.Queue^.Data);
    Load_Inst_.FOnRun_C := OnRun_C;
    Load_Inst_.FOnRun_M := OnRun_M;
    Load_Inst_.FOnRun_P := OnRun_P;
    Load_Inst_.Run;
    while FTh_Pool.Count + Owner_.QueueNum > FMax_Queue do
        TCompute.Sleep(10);
  until not __Repeat__.Next;
  Owner_.Wait_Busy_Task();
  FTh_Pool.Wait;
  IsBusy := False;
end;

constructor TZDB2_Th_Engine_Load_Processor.Create(ThNum_: Integer);
begin
  inherited Create;
  IsBusy := True;
  OnRun_C := nil;
  OnRun_M := nil;
  OnRun_P := nil;
  FTh_Pool := TIO_Thread.Create(ThNum_);
  FMax_Queue := Max_Thread_Supported;
end;

destructor TZDB2_Th_Engine_Load_Processor.Destroy;
begin
  disposeObject(FTh_Pool);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Load_Processor.Run(Marshal_: TZDB2_Th_Engine_Marshal);
begin
  TCompute.RunM(nil, Marshal_, {$IFDEF FPC}@{$ENDIF FPC}Do_ThRun_Marshal_Load);
end;

procedure TZDB2_Th_Engine_Load_Processor.Wait();
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance;
begin
  while IsBusy do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance(FTh_Pool.Dequeue);
      if Load_Inst_ <> nil then
          disposeObject(Load_Inst_)
      else
          TCompute.Sleep(1);
    end;
end;

procedure TZDB2_Th_Engine_Load_Processor.Wait_C(On_Wait: TOn_Wait_C);
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance;
begin
  while IsBusy do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          disposeObject(Load_Inst_)
      else
          TCompute.Sleep(1);
    end;
end;

procedure TZDB2_Th_Engine_Load_Processor.Wait_M(On_Wait: TOn_Wait_M);
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance;
begin
  while IsBusy do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          disposeObject(Load_Inst_)
      else
          TCompute.Sleep(1);
    end;
end;

procedure TZDB2_Th_Engine_Load_Processor.Wait_P(On_Wait: TOn_Wait_P);
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance;
begin
  while IsBusy do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          disposeObject(Load_Inst_)
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
      Data.FTh_Engine.Th_Engine_Data_Pool.Remove(Data.FTh_Engine_Data_Ptr)
  else
    begin
      Data.FPost_Free_Runing := True;
      disposeObject(Data);
    end;
end;

constructor TZDB2_Th_Engine_Marshal.Create;
begin
  inherited Create;
  Data_Marshal := TZDB2_Th_Engine_Marshal_BigList__.Create;
  Data_Marshal.OnFree := {$IFDEF FPC}@{$ENDIF FPC}DoFree;
  Engine_Pool := TZDB2_Th_Engine_List.Create;
  Current_Data_Class := TZDB2_Th_Engine_Data;
  Pool_Ptr := Th_Engine_Marshal_Pool__.Add(self);
end;

destructor TZDB2_Th_Engine_Marshal.Destroy;
begin
  Th_Engine_Marshal_Pool__.Remove(Pool_Ptr);
  Flush;
  disposeObjectAndNil(Engine_Pool);
  disposeObjectAndNil(Data_Marshal);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Marshal.Build(Data_Class: TZDB2_Th_Engine_Data_Class);
begin
  if Engine_Pool.Num > 0 then
    with Engine_Pool.Repeat_ do
      repeat
          Queue^.Data.Build(Data_Class);
      until not Next;
  Current_Data_Class := Data_Class;
end;

procedure TZDB2_Th_Engine_Marshal.Clear;
begin
  Flush;
  Data_Marshal.Clear;
end;

function TZDB2_Th_Engine_Marshal.Database_Size: Int64;
begin
  Result := 0;
  if Engine_Pool.Num > 0 then
    with Engine_Pool.Repeat_ do
      repeat
        if Queue^.Data.Engine <> nil then
            inc(Result, Queue^.Data.Engine.CoreSpace_Size);
      until not Next;
end;

function TZDB2_Th_Engine_Marshal.Database_Physics_Size: Int64;
begin
  Result := 0;
  if Engine_Pool.Num > 0 then
    with Engine_Pool.Repeat_ do
      repeat
        if Queue^.Data.Engine <> nil then
            inc(Result, Queue^.Data.Engine.CoreSpace_Physics_Size);
      until not Next;
end;

function TZDB2_Th_Engine_Marshal.Total: NativeInt;
begin
  Result := 0;
  if Engine_Pool.Num > 0 then
    with Engine_Pool.Repeat_ do
      repeat
          inc(Result, Queue^.Data.Th_Engine_Data_Pool.Num);
      until not Next;
end;

function TZDB2_Th_Engine_Marshal.QueueNum: NativeInt;
begin
  Result := 0;
  if Engine_Pool.Num > 0 then
    with Engine_Pool.Repeat_ do
      repeat
          inc(Result, Queue^.Data.Engine.QueueNum);
      until not Next;
end;

function TZDB2_Th_Engine_Marshal.Get_Busy_Task_Num: Int64;
begin
  Result := 0;
  if Engine_Pool.Num > 0 then
    with Engine_Pool.Repeat_ do
      repeat
          inc(Result, Queue^.Data.Get_Busy_Task_Num);
      until not Next;
end;

function TZDB2_Th_Engine_Marshal.Add_Data_To_Minimize_Workload_Engine(): TZDB2_Th_Engine_Data;
var
  Eng_: TZDB2_Th_Engine;
begin
  repeat
      Eng_ := Engine_Pool.Get_Minimize_Workload_Engine;
  until not Eng_.OnlyRead;

  if Eng_ <> nil then
      Result := Eng_.Add(Current_Data_Class)
  else
      Result := nil;
end;

function TZDB2_Th_Engine_Marshal.Add_Data_To_Minimize_Size_Engine(): TZDB2_Th_Engine_Data;
var
  Eng_: TZDB2_Th_Engine;
begin
  repeat
      Eng_ := Engine_Pool.Get_Minimize_Size_Engine;
  until not Eng_.OnlyRead;

  if Eng_ <> nil then
      Result := Eng_.Add(Current_Data_Class)
  else
      Result := nil;
end;

procedure TZDB2_Th_Engine_Marshal.Wait_Busy_Task;
begin
  // wait task
  while (Get_Busy_Task_Num + QueueNum > 0) do
    begin
      Progress;
      TCompute.Sleep(10);
    end;
end;

procedure TZDB2_Th_Engine_Marshal.Check_Recycle_Pool;
begin
  // free thread engine recycle pool
  if Engine_Pool.Num > 0 then
    with Engine_Pool.Repeat_ do
      repeat
          Queue^.Data.Th_Engine_Data_Pool.Free_Recycle_Pool;
      until not Next;
  // free local recycle pool
  Data_Marshal.Free_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.Flush;
begin
  Wait_Busy_Task;
  Check_Recycle_Pool;
  if Engine_Pool.Num > 0 then
    with Engine_Pool.Repeat_ do
      repeat
          Queue^.Data.Flush;
      until not Next;
  Check_Recycle_Pool;
  Wait_Busy_Task;
end;

procedure TZDB2_Th_Engine_Marshal.Format_Database;
begin
  Wait_Busy_Task;
  Check_Recycle_Pool;
  if Engine_Pool.Num > 0 then
    with Engine_Pool.Repeat_ do
      repeat
          Queue^.Data.Format_Database;
      until not Next;
  Check_Recycle_Pool;
  Wait_Busy_Task;
end;

procedure TZDB2_Th_Engine_Marshal.Parallel_Load_C(ThNum_, Max_Queue_: Integer; On_Run: TZDB2_Th_Engine_On_Data_Event_C; On_Wait: TOn_Wait_C);
var
  processor_: TZDB2_Th_Engine_Load_Processor;
begin
  if Data_Marshal.Num <= 0 then
      exit;

  Wait_Busy_Task;
  Check_Recycle_Pool;

  processor_ := TZDB2_Th_Engine_Load_Processor.Create(ThNum_);
  processor_.OnRun_C := On_Run;
  processor_.FMax_Queue := if_(Max_Queue_ <= 0, Max_Thread_Supported, Max_Queue_);
  processor_.Run(self);
  processor_.Wait_C(On_Wait);
  processor_.Free;

  Wait_Busy_Task;
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.Parallel_Load_M(ThNum_, Max_Queue_: Integer; On_Run: TZDB2_Th_Engine_On_Data_Event_M; On_Wait: TOn_Wait_M);
var
  processor_: TZDB2_Th_Engine_Load_Processor;
begin
  if Data_Marshal.Num <= 0 then
      exit;

  Wait_Busy_Task;

  Check_Recycle_Pool;

  processor_ := TZDB2_Th_Engine_Load_Processor.Create(ThNum_);
  processor_.OnRun_M := On_Run;
  processor_.FMax_Queue := if_(Max_Queue_ <= 0, Max_Thread_Supported, Max_Queue_);
  processor_.Run(self);
  processor_.Wait_M(On_Wait);
  processor_.Free;

  Wait_Busy_Task;
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.Parallel_Load_P(ThNum_, Max_Queue_: Integer; On_Run: TZDB2_Th_Engine_On_Data_Event_P; On_Wait: TOn_Wait_P);
var
  processor_: TZDB2_Th_Engine_Load_Processor;
begin
  if Data_Marshal.Num <= 0 then
      exit;

  Wait_Busy_Task;

  Check_Recycle_Pool;

  processor_ := TZDB2_Th_Engine_Load_Processor.Create(ThNum_);
  processor_.OnRun_P := On_Run;
  processor_.FMax_Queue := if_(Max_Queue_ <= 0, Max_Thread_Supported, Max_Queue_);
  processor_.Run(self);
  processor_.Wait_P(On_Wait);
  processor_.Free;

  Wait_Busy_Task;
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.Progress;
begin
  Check_Recycle_Pool;

  // progress thread engine
  if Engine_Pool.Num > 0 then
    with Engine_Pool.Repeat_ do
      repeat
          Queue^.Data.Progress;
      until not Next;

  Check_Recycle_Pool;
end;

function TZDB2_Th_Engine_Marshal.GetRemoveDatabaseOnDestroy: Boolean;
begin
  Result := False;
  if Engine_Pool.Num > 0 then
    with Engine_Pool.Repeat_ do
      repeat
          Result := Result or Queue^.Data.RemoveDatabaseOnDestroy;
      until not Next;
end;

procedure TZDB2_Th_Engine_Marshal.SetRemoveDatabaseOnDestroy(const Value: Boolean);
begin
  if Engine_Pool.Num > 0 then
    with Engine_Pool.Repeat_ do
      repeat
          Queue^.Data.RemoveDatabaseOnDestroy := Value;
      until not Next;
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
  tk: TTimeTick;
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
  disposeObject(L);
  TE.Free;
  DM.Build(TZDB2_Th_Engine_Data);

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

  DM.Parallel_Load_C(4, 10, nil, nil);

  DoStatus('db total:%d', [DM.Total]);

  disposeObject(DM);
end;

initialization

Th_Engine_Marshal_Pool__ := TZDB2_Th_Engine_Marshal_Pool.Create;

finalization

disposeObjectAndNil(Th_Engine_Marshal_Pool__);

end.
