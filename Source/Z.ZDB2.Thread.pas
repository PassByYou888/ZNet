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
  Z.Status, Z.Cipher, Z.ZDB2, Z.ListEngine, Z.TextDataEngine;

type
{$REGION 'Command_Queue'}
  TZDB2_Th_Engine = class;

  TCMD_State = (csDefault, csDone, csError);
  PCMD_State = ^TCMD_State;

  TZDB2_Th_CMD_ID_And_State = record
    ID: Integer;
    State: TCMD_State;
  end;

  PZDB2_Th_CMD_ID_And_State = ^TZDB2_Th_CMD_ID_And_State;
  TZDB2_Th_CMD_ID_And_State_Array = array of TZDB2_Th_CMD_ID_And_State;
  PZDB2_Th_CMD_ID_And_State_Array = ^TZDB2_Th_CMD_ID_And_State_Array;

  TZDB2_Th_CMD_Mem64_And_State = record
    Mem64: TMem64;
    State: TCMD_State;
  end;

  PZDB2_Th_CMD_Mem64_And_State = ^TZDB2_Th_CMD_Mem64_And_State;
  TZDB2_Th_CMD_Mem64_And_State_Array = array of TZDB2_Th_CMD_Mem64_And_State;
  PZDB2_Th_CMD_Mem64_And_State_Array = ^TZDB2_Th_CMD_Mem64_And_State_Array;

  TZDB2_Th_CMD_Stream_And_State = record
    Stream: TCore_Stream;
    State: TCMD_State;
  end;

  PZDB2_Th_CMD_Stream_And_State = ^TZDB2_Th_CMD_Stream_And_State;
  TZDB2_Th_CMD_Stream_And_State_Array = array of TZDB2_Th_CMD_Stream_And_State;
  PZDB2_Th_CMD_Stream_And_State_Array = ^TZDB2_Th_CMD_Stream_And_State_Array;

  TOn_CMD_Done = procedure() of object;

  TZDB2_Th_CMD = class
  private
    OnDone: TOn_CMD_Done;
    Engine: TZDB2_Th_Engine;
    State_Ptr: PCMD_State;
    procedure DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State); virtual; abstract;
  public
    constructor Create(const ThEng_: TZDB2_Th_Engine);
    destructor Destroy; override;
    procedure Init; virtual;
    procedure Ready(var State_: TCMD_State); overload;
    procedure Execute;
  end;

  TZDB2_Th_CMD_GetDataAsMem64 = class(TZDB2_Th_CMD)
  protected
    Param_ID: Integer;
    Param_M64: TMem64;
    procedure DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Engine; const Mem64: TMem64; const ID: Integer);
  end;

  TZDB2_Th_CMD_GetDataAsStream = class(TZDB2_Th_CMD)
  protected
    Param_ID: Integer;
    Param_Stream: TCore_Stream;
    procedure DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Engine; const Stream: TCore_Stream; const ID: Integer);
  end;

  TZDB2_Th_CMD_SetDataFromMem64 = class(TZDB2_Th_CMD)
  protected
    Param_ID_Ptr: PInteger;
    Param_M64: TMem64;
    procedure DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State); override;
  public
    AutoFree_Data: Boolean;
    constructor Create(const ThEng_: TZDB2_Th_Engine; const Mem64: TMem64; var ID: Integer);
  end;

  TZDB2_Th_CMD_SetDataFromStream = class(TZDB2_Th_CMD)
  protected
    Param_ID_Ptr: PInteger;
    Param_Stream: TCore_Stream;
    procedure DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State); override;
  public
    AutoFree_Data: Boolean;
    constructor Create(const ThEng_: TZDB2_Th_Engine; const Stream: TCore_Stream; var ID: Integer);
  end;

  TZDB2_Th_CMD_AppendFromMem64 = class(TZDB2_Th_CMD)
  protected
    Param_ID_Ptr: PInteger;
    Param_M64: TMem64;
    procedure DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State); override;
  public
    AutoFree_Data: Boolean;
    constructor Create(const ThEng_: TZDB2_Th_Engine; const Mem64: TMem64; var ID: Integer);
  end;

  TZDB2_Th_CMD_AppendFromStream = class(TZDB2_Th_CMD)
  protected
    Param_ID_Ptr: PInteger;
    Param_Stream: TCore_Stream;
    procedure DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State); override;
  public
    AutoFree_Data: Boolean;
    constructor Create(const ThEng_: TZDB2_Th_Engine; const Stream: TCore_Stream; var ID: Integer);
  end;

  TZDB2_Th_CMD_Remove = class(TZDB2_Th_CMD)
  protected
    Param_ID: Integer;
    procedure DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Engine; const ID: Integer);
  end;

  TZDB2_Th_CMD_Exit = class(TZDB2_Th_CMD)
  protected
    procedure DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Engine);
  end;

  TZDB2_Th_CMD_Flush = class(TZDB2_Th_CMD)
  protected
    procedure DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Engine);
  end;

  TSequence_Table_Head = packed record
    Identifier: Word;
    ID: Integer;
  end;

  PSequence_Table_Head = ^TSequence_Table_Head;

  TZDB2_Th_CMD_Rebuild_Sequence_Table = class(TZDB2_Th_CMD)
  protected
    Table_Ptr: PZDB2_BlockHandle;
    procedure DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Engine; var Table_: TZDB2_BlockHandle);
  end;

  TZDB2_Th_CMD_Get_And_Clean_Sequence_Table = class(TZDB2_Th_CMD)
  protected
    Table_Ptr: PZDB2_BlockHandle;
    procedure DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Engine; var Table_: TZDB2_BlockHandle);
  end;

  TZDB2_Th_CMD_Flush_Sequence_Table = class(TZDB2_Th_CMD)
  protected
    Table_Ptr: PZDB2_BlockHandle;
    procedure DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Engine; var Table_: TZDB2_BlockHandle);
  end;

  TZDB2_Th_CMD_Extract_To = class(TZDB2_Th_CMD)
  protected
    Input_Ptr: PZDB2_BlockHandle;
    Dest_Th_Engine: TZDB2_Th_Engine;
    Output_Ptr: PZDB2_Th_CMD_ID_And_State_Array;
    procedure DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State); override;
  public
    Max_Queue: Integer;
    Wait_Queue: Boolean;
    constructor Create(const ThEng_: TZDB2_Th_Engine;
      var Input_: TZDB2_BlockHandle;
      const Dest_Th_Engine_: TZDB2_Th_Engine; var Output_: TZDB2_Th_CMD_ID_And_State_Array);
  end;

  TOn_Mem64_And_State_Event_C = procedure(var Sender: TZDB2_Th_CMD_Mem64_And_State);
  TOn_Mem64_And_State_Event_M = procedure(var Sender: TZDB2_Th_CMD_Mem64_And_State) of object;
{$IFDEF FPC}
  TOn_Mem64_And_State_Event_P = procedure(var Sender: TZDB2_Th_CMD_Mem64_And_State) is nested;
{$ELSE FPC}
  TOn_Mem64_And_State_Event_P = reference to procedure(var Sender: TZDB2_Th_CMD_Mem64_And_State);
{$ENDIF FPC}

  TZDB2_Th_CMD_Bridge_Mem64_And_State = class
  private
    CMD: TZDB2_Th_CMD;
    Mem64_And_State: TZDB2_Th_CMD_Mem64_And_State;
    OnResult_C: TOn_Mem64_And_State_Event_C;
    OnResult_M: TOn_Mem64_And_State_Event_M;
    OnResult_P: TOn_Mem64_And_State_Event_P;
    procedure CMD_Done;
  public
    constructor Create;
    procedure Init(CMD_: TZDB2_Th_CMD);
    procedure Ready;
  end;

  TOn_Stream_And_State_Event_C = procedure(var Sender: TZDB2_Th_CMD_Stream_And_State);
  TOn_Stream_And_State_Event_M = procedure(var Sender: TZDB2_Th_CMD_Stream_And_State) of object;
{$IFDEF FPC}
  TOn_Stream_And_State_Event_P = procedure(var Sender: TZDB2_Th_CMD_Stream_And_State) is nested;
{$ELSE FPC}
  TOn_Stream_And_State_Event_P = reference to procedure(var Sender: TZDB2_Th_CMD_Stream_And_State);
{$ENDIF FPC}

  TZDB2_Th_CMD_Bridge_Stream_And_State = class
  private
    CMD: TZDB2_Th_CMD;
    Stream_And_State: TZDB2_Th_CMD_Stream_And_State;
    OnResult_C: TOn_Stream_And_State_Event_C;
    OnResult_M: TOn_Stream_And_State_Event_M;
    OnResult_P: TOn_Stream_And_State_Event_P;
    procedure CMD_Done;
  public
    constructor Create;
    procedure Init(CMD_: TZDB2_Th_CMD);
    procedure Ready;
  end;

  TOn_ID_And_State_Event_C = procedure(var Sender: TZDB2_Th_CMD_ID_And_State);
  TOn_ID_And_State_Event_M = procedure(var Sender: TZDB2_Th_CMD_ID_And_State) of object;
{$IFDEF FPC}
  TOn_ID_And_State_Event_P = procedure(var Sender: TZDB2_Th_CMD_ID_And_State) is nested;
{$ELSE FPC}
  TOn_ID_And_State_Event_P = reference to procedure(var Sender: TZDB2_Th_CMD_ID_And_State);
{$ENDIF FPC}

  TZDB2_Th_CMD_Bridge_ID_And_State = class
  private
    CMD: TZDB2_Th_CMD;
    ID_And_State: TZDB2_Th_CMD_ID_And_State;
    OnResult_C: TOn_ID_And_State_Event_C;
    OnResult_M: TOn_ID_And_State_Event_M;
    OnResult_P: TOn_ID_And_State_Event_P;
    procedure CMD_Done;
  public
    constructor Create;
    procedure Init(CMD_: TZDB2_Th_CMD);
    procedure Ready;
  end;

  TZDB2_Th_CMD_Queue = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalOrderStruct<TZDB2_Th_CMD>;
{$ENDREGION 'Command_Queue'}

  TZDB2_Th_Engine = class
  private
    CMD_Queue: TZDB2_Th_CMD_Queue;
    FCMD_Execute_Thread_Is_Runing, FCMD_Execute_Thread_Is_Exit: Boolean;
    CoreSpace_Delta: Int64;
    CoreSpace_BlockSize: Word;
    CoreSpace_Cipher: IZDB2_Cipher;
    CoreSpace_IOHnd: TIOHnd;
    CoreSpace: TZDB2_Core_Space;
    procedure ZDB2_ThRun_Proc(ThSender: TCompute);
    procedure Do_Free_CMD(var p: TZDB2_Th_CMD);
    procedure DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
  public
    constructor Create(Stream_: TCore_Stream; AutoFree_, OnlyRead_: Boolean; Delta_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
    destructor Destroy; override;

    // queue state
    function QueueNum: NativeInt;
    function CoreSpace_Size: Int64;

    // sync
    function Sync_GetData(Mem64: TMem64; ID: Integer): Boolean; overload;
    function Sync_SetData(Mem64: TMem64; var ID: Integer): Boolean; overload;
    function Sync_Append(Mem64: TMem64; var ID: Integer): Boolean; overload;
    function Sync_GetData(Stream: TCore_Stream; ID: Integer): Boolean; overload;
    function Sync_SetData(Stream: TCore_Stream; var ID: Integer): Boolean; overload;
    function Sync_Append(Stream: TCore_Stream; var ID: Integer): Boolean; overload;
    function Sync_Remove(ID: Integer): Boolean;
    function Sync_Flush(): Boolean;
    function Sync_Rebuild_Sequence_Table(var Table_: TZDB2_BlockHandle): Boolean;
    function Sync_Get_And_Clean_Sequence_Table(var Table_: TZDB2_BlockHandle): Boolean;
    function Sync_Flush_Sequence_Table(var Table_: TZDB2_BlockHandle): Boolean; overload;
    function Sync_Flush_Sequence_Table(L: TZDB2_ID_List): Boolean; overload;
    function Sync_Extract_To(var Input_: TZDB2_BlockHandle; const Dest_Th_Engine_: TZDB2_Th_Engine; var Output_: TZDB2_Th_CMD_ID_And_State_Array): Boolean;

    // async
    procedure Async_GetData_AsMem64_C(ID: Integer; Mem64: TMem64; OnResult: TOn_Mem64_And_State_Event_C); overload;
    procedure Async_GetData_AsMem64_M(ID: Integer; Mem64: TMem64; OnResult: TOn_Mem64_And_State_Event_M); overload;
    procedure Async_GetData_AsMem64_P(ID: Integer; Mem64: TMem64; OnResult: TOn_Mem64_And_State_Event_P); overload;
    procedure Async_GetData_AsStream_C(ID: Integer; Stream: TCore_Stream; OnResult: TOn_Stream_And_State_Event_C); overload;
    procedure Async_GetData_AsStream_M(ID: Integer; Stream: TCore_Stream; OnResult: TOn_Stream_And_State_Event_M); overload;
    procedure Async_GetData_AsStream_P(ID: Integer; Stream: TCore_Stream; OnResult: TOn_Stream_And_State_Event_P); overload;
    procedure Async_SetData(Mem64: TMem64; AutoFree_Data: Boolean; ID: Integer); overload;
    procedure Async_SetData_C(Mem64: TMem64; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_C); overload;
    procedure Async_SetData_M(Mem64: TMem64; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_M); overload;
    procedure Async_SetData_P(Mem64: TMem64; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_P); overload;
    procedure Async_Append(Mem64: TMem64; AutoFree_Data: Boolean); overload;
    procedure Async_Append_C(Mem64: TMem64; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_C); overload;
    procedure Async_Append_M(Mem64: TMem64; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_M); overload;
    procedure Async_Append_P(Mem64: TMem64; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_P); overload;
    procedure Async_SetData(Stream: TCore_Stream; AutoFree_Data: Boolean; ID: Integer); overload;
    procedure Async_SetData_C(Stream: TCore_Stream; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_C); overload;
    procedure Async_SetData_M(Stream: TCore_Stream; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_M); overload;
    procedure Async_SetData_P(Stream: TCore_Stream; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_P); overload;
    procedure Async_Append(Stream: TCore_Stream; AutoFree_Data: Boolean); overload;
    procedure Async_Append_C(Stream: TCore_Stream; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_C); overload;
    procedure Async_Append_M(Stream: TCore_Stream; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_M); overload;
    procedure Async_Append_P(Stream: TCore_Stream; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_P); overload;
    procedure Async_Remove(ID: Integer);
    procedure Async_Flush();

    // misc
    class function Get_Handle(var buff: TZDB2_Th_CMD_ID_And_State_Array): TZDB2_BlockHandle;

    // test
    class procedure Test;
  end;

  TZDB2_Th_Engine_ABI_Data_Marshal = class;
  TZDB2_Th_Engine_ABI_Data = class;
  TZDB2_Th_Engine_ABI_Config = class;
  TZDB2_Th_Engine_ABI_Data_BigList_Decl__ = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalBigList<TZDB2_Th_Engine_ABI_Data>;
  TZDB2_Th_Engine_ABI_Data_BigList__ = class(TZDB2_Th_Engine_ABI_Data_BigList_Decl__);
  TZDB2_Th_Engine_ABI_Data_Marshal_BigList_Decl__ = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalBigList<TZDB2_Th_Engine_ABI_Data>;
  TZDB2_Th_Engine_ABI_Data_Marshal_BigList__ = class(TZDB2_Th_Engine_ABI_Data_Marshal_BigList_Decl__);

  TZDB2_Th_Engine_ABI_Data = class(TCore_InterfacedObject)
  private
    FOwner: TZDB2_Th_Engine_ABI_Data_Marshal;
    FOwner_Data_Ptr: TZDB2_Th_Engine_ABI_Data_Marshal_BigList_Decl__.PQueueStruct;
    FTh_Engine: TZDB2_Th_Engine_ABI_Config;
    FTh_Engine_Data_Ptr: TZDB2_Th_Engine_ABI_Data_BigList_Decl__.PQueueStruct;
    FID: Integer;
    FBusy_Task_Num: Integer;
    procedure Do_Save_ID_And_State_Event(var Sender: TZDB2_Th_CMD_ID_And_State);
    procedure Wait_Busy_Done;
  public
    constructor Create(); virtual;
    destructor Destroy; override;
    procedure Do_Progress(); virtual;
    // share
    function Engine: TZDB2_Th_Engine;
    property Th_Engine: TZDB2_Th_Engine_ABI_Config read FTh_Engine;
    property ID: Integer read FID;
    property Busy_Task_Num: Integer read FBusy_Task_Num;
    procedure Delete; // async delete and free
    function Do_Load_Data(Stream: TMS64): Boolean; overload;
    function Do_Load_Data(Mem64: TMem64): Boolean; overload;
    procedure Do_Async_Save_And_Free_Data(var Stream: TMS64); overload;
    procedure Do_Async_Save_And_Free_Data(var Mem64: TMem64); overload;
    procedure Do_Post_To_Pool_Free();
    // parallel load supported.
    procedure Check_Data_And_Loaded_On_Thread(const StreamM: TMS64); virtual;
  end;

  TZDB2_Th_Engine_ABI_Data_Mem64 = class(TZDB2_Th_Engine_ABI_Data)
  protected
    FAlive: TTimeTick;
    FData_Mem64: TMem64;
    FData_Mem64_MD5: TMD5;
  public
    constructor Create(); override;
    destructor Destroy; override;
    procedure Do_Progress(); override;
    procedure Load; virtual;
    procedure Save; virtual;
    procedure RecycleMemory; virtual;
    function GetData_Mem64: TMem64; virtual;
    property Data_Mem64: TMem64 read GetData_Mem64;
    property Data_Mem64_MD5: TMD5 read FData_Mem64_MD5;
  end;

  TZDB2_Th_Engine_ABI_Data_MS64 = class(TZDB2_Th_Engine_ABI_Data)
  protected
    FAlive: TTimeTick;
    FData_MS64: TMS64;
    FData_MS64_MD5: TMD5;
  public
    constructor Create(); override;
    destructor Destroy; override;
    procedure Do_Progress(); override;
    procedure Load; virtual;
    procedure Save; virtual;
    procedure RecycleMemory; virtual;
    function GetData_MS64: TMS64; virtual;
    property Data_MS64: TMS64 read GetData_MS64;
    property Data_MS64_MD5: TMD5 read FData_MS64_MD5;
  end;

  TZDB2_Th_Engine_ABI_Data_Class = class of TZDB2_Th_Engine_ABI_Data;

  TZDB2_Th_Engine_ABI_Config = class(TCore_InterfacedObject)
  private
    procedure Do_Progress(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Data_BigList_Decl__.PQueueStruct; var Aborted: Boolean);
    procedure DoFree(var Data: TZDB2_Th_Engine_ABI_Data);
  public
    Name: U_String;
    Owner: TZDB2_Th_Engine_ABI_Data_Marshal;
    TimeOut: TTimeTick;
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
    Engine: TZDB2_Th_Engine;
    Th_Engine_Data_Pool: TZDB2_Th_Engine_ABI_Data_BigList__;
    constructor Create(Owner_: TZDB2_Th_Engine_ABI_Data_Marshal); virtual;
    destructor Destroy; override;
    procedure ReadConfig(Name_: U_String; cfg: THashStringList);
    procedure WriteConfig(cfg: THashStringList);
    procedure Build(Data_Class: TZDB2_Th_Engine_ABI_Data_Class);
    procedure Rebuild_Data_Pool(Data_Class: TZDB2_Th_Engine_ABI_Data_Class);
    function Flush: Boolean;
    function Add(Data_Class: TZDB2_Th_Engine_ABI_Data_Class; ID: Integer): TZDB2_Th_Engine_ABI_Data; overload;
    function Add(Data_Class: TZDB2_Th_Engine_ABI_Data_Class): TZDB2_Th_Engine_ABI_Data; overload;
    procedure Progress;
    function Get_Busy_Data_Num: Int64;
    function Get_Busy_Task_Num: Int64;
  end;

  TZDB2_Th_Engine_ABI_Config_List_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalBigList<TZDB2_Th_Engine_ABI_Config>;

  TZDB2_Th_Engine_ABI_Config_List = class(TZDB2_Th_Engine_ABI_Config_List_Decl)
  public
    constructor Create();
    destructor Destroy; override;
    procedure DoFree(var Data: TZDB2_Th_Engine_ABI_Config); override;
    function Get_Minimize_Size_Engine(): TZDB2_Th_Engine_ABI_Config;
    function Get_Minimize_Workload_Engine(): TZDB2_Th_Engine_ABI_Config;
  end;

  TZDB2_Th_Engine_ABI_Data_Marshal = class(TCore_InterfacedObject)
  private
    procedure DoFree(var Data: TZDB2_Th_Engine_ABI_Data);
  public
    Marshal_Data_Pool: TZDB2_Th_Engine_ABI_Data_Marshal_BigList__;
    Th_Engine_List: TZDB2_Th_Engine_ABI_Config_List;
    constructor Create();
    destructor Destroy; override;
    procedure Build(Data_Class: TZDB2_Th_Engine_ABI_Data_Class);
    function QueueNum: NativeInt;
    procedure Progress;
    function Get_Busy_Data_Num: Int64;
    function Get_Busy_Task_Num: Int64;
    function Add_Data_To_Minimize_Workload_Engine(Data_Class: TZDB2_Th_Engine_ABI_Data_Class): TZDB2_Th_Engine_ABI_Data; overload;
    function Add_Data_To_Minimize_Size_Engine(Data_Class: TZDB2_Th_Engine_ABI_Data_Class): TZDB2_Th_Engine_ABI_Data; overload;
    class procedure Test();
  end;

implementation

uses Z.Expression;

constructor TZDB2_Th_CMD.Create(const ThEng_: TZDB2_Th_Engine);
begin
  inherited Create;
  OnDone := nil;
  Engine := ThEng_;
  State_Ptr := nil;
end;

destructor TZDB2_Th_CMD.Destroy;
begin
  inherited Destroy;
end;

procedure TZDB2_Th_CMD.Init;
begin

end;

procedure TZDB2_Th_CMD.Ready(var State_: TCMD_State);
begin
  State_Ptr := @State_;
  State_Ptr^ := TCMD_State.csDefault;
  Engine.CMD_Queue.Push(self);
end;

procedure TZDB2_Th_CMD.Execute;
begin
  try
    DoExecute(Engine.CoreSpace, State_Ptr);
    if State_Ptr^ = TCMD_State.csDefault then
        State_Ptr^ := TCMD_State.csDone;
  except
      State_Ptr^ := TCMD_State.csError;
  end;

  try
    if Assigned(OnDone) then
        OnDone();
  except
  end;
end;

procedure TZDB2_Th_CMD_GetDataAsMem64.DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State);
begin
  if not CoreSpace.ReadData(Param_M64, Param_ID) then
      State^ := TCMD_State.csError;
end;

constructor TZDB2_Th_CMD_GetDataAsMem64.Create(const ThEng_: TZDB2_Th_Engine; const Mem64: TMem64; const ID: Integer);
begin
  inherited Create(ThEng_);
  Param_ID := ID;
  Param_M64 := Mem64;
  Init();
end;

procedure TZDB2_Th_CMD_GetDataAsStream.DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State);
begin
  if not CoreSpace.ReadStream(Param_Stream, Param_ID) then
      State^ := TCMD_State.csError;
end;

constructor TZDB2_Th_CMD_GetDataAsStream.Create(const ThEng_: TZDB2_Th_Engine; const Stream: TCore_Stream; const ID: Integer);
begin
  inherited Create(ThEng_);
  Param_ID := ID;
  Param_Stream := Stream;
  Init();
end;

procedure TZDB2_Th_CMD_SetDataFromMem64.DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State);
var
  old_ID: Integer;
begin
  if Engine.CoreSpace_IOHnd.IsOnlyRead then
      State^ := TCMD_State.csError
  else
    begin
      old_ID := Param_ID_Ptr^;
      if not CoreSpace.WriteData(Param_M64, Param_ID_Ptr^, not AutoFree_Data) then
          State^ := TCMD_State.csError
      else if not CoreSpace.RemoveData(old_ID, False) then
          State^ := TCMD_State.csError;
    end;
  if AutoFree_Data then
      disposeObject(Param_M64);
end;

constructor TZDB2_Th_CMD_SetDataFromMem64.Create(const ThEng_: TZDB2_Th_Engine; const Mem64: TMem64; var ID: Integer);
begin
  inherited Create(ThEng_);
  Param_ID_Ptr := @ID;
  Param_M64 := Mem64;
  AutoFree_Data := False;
  Init();
end;

procedure TZDB2_Th_CMD_SetDataFromStream.DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State);
var
  old_ID: Integer;
begin
  if Engine.CoreSpace_IOHnd.IsOnlyRead then
      State^ := TCMD_State.csError
  else
    begin
      old_ID := Param_ID_Ptr^;
      if not CoreSpace.WriteStream(Param_Stream, Param_ID_Ptr^) then
          State^ := TCMD_State.csError
      else if not CoreSpace.RemoveData(old_ID, False) then
          State^ := TCMD_State.csError;
    end;
  if AutoFree_Data then
      disposeObject(Param_Stream);
end;

constructor TZDB2_Th_CMD_SetDataFromStream.Create(const ThEng_: TZDB2_Th_Engine; const Stream: TCore_Stream; var ID: Integer);
begin
  inherited Create(ThEng_);
  Param_ID_Ptr := @ID;
  Param_Stream := Stream;
  AutoFree_Data := False;
  Init();
end;

procedure TZDB2_Th_CMD_AppendFromMem64.DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State);
begin
  if Engine.CoreSpace_IOHnd.IsOnlyRead then
      State^ := TCMD_State.csError
  else if not CoreSpace.WriteData(Param_M64, Param_ID_Ptr^, not AutoFree_Data) then
      State^ := TCMD_State.csError;
  if AutoFree_Data then
      disposeObject(Param_M64);
end;

constructor TZDB2_Th_CMD_AppendFromMem64.Create(const ThEng_: TZDB2_Th_Engine; const Mem64: TMem64; var ID: Integer);
begin
  inherited Create(ThEng_);
  ID := -1;
  Param_ID_Ptr := @ID;
  Param_M64 := Mem64;
  AutoFree_Data := False;
  Init();
end;

procedure TZDB2_Th_CMD_AppendFromStream.DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State);
begin
  if Engine.CoreSpace_IOHnd.IsOnlyRead then
      State^ := TCMD_State.csError
  else if not CoreSpace.WriteStream(Param_Stream, Param_ID_Ptr^) then
      State^ := TCMD_State.csError;
  if AutoFree_Data then
      disposeObject(Param_Stream);
end;

constructor TZDB2_Th_CMD_AppendFromStream.Create(const ThEng_: TZDB2_Th_Engine; const Stream: TCore_Stream; var ID: Integer);
begin
  inherited Create(ThEng_);
  ID := -1;
  Param_ID_Ptr := @ID;
  Param_Stream := Stream;
  AutoFree_Data := False;
  Init();
end;

procedure TZDB2_Th_CMD_Remove.DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State);
begin
  if Engine.CoreSpace_IOHnd.IsOnlyRead then
      State^ := TCMD_State.csError
  else if not CoreSpace.RemoveData(Param_ID, False) then
      State^ := TCMD_State.csError;
end;

constructor TZDB2_Th_CMD_Remove.Create(const ThEng_: TZDB2_Th_Engine; const ID: Integer);
begin
  inherited Create(ThEng_);
  Param_ID := ID;
  Init();
end;

procedure TZDB2_Th_CMD_Exit.DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State);
begin
  Engine.FCMD_Execute_Thread_Is_Runing := False;
end;

constructor TZDB2_Th_CMD_Exit.Create(const ThEng_: TZDB2_Th_Engine);
begin
  inherited Create(ThEng_);
  Init();
end;

procedure TZDB2_Th_CMD_Flush.DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State);
begin
  CoreSpace.Save;
end;

constructor TZDB2_Th_CMD_Flush.Create(const ThEng_: TZDB2_Th_Engine);
begin
  inherited Create(ThEng_);
  Init();
end;

procedure TZDB2_Th_CMD_Rebuild_Sequence_Table.DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State);
var
  R_: TCMD_State;
begin
  R_ := TCMD_State.csDone;

  if not Engine.CoreSpace_IOHnd.IsOnlyRead then
    begin
      // remove identifier
      if (PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.Identifier = $FFFF) and
        CoreSpace.Check(PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.ID) then
        begin
          if not CoreSpace.RemoveData(PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.ID, False) then
              R_ := TCMD_State.csError;
          FillPtr(@CoreSpace.UserCustomHeader^[0], SizeOf(TSequence_Table_Head), 0);
        end;
    end;

  // rebuild identifier
  Table_Ptr^ := CoreSpace.BuildTableID;
  State^ := R_;
end;

constructor TZDB2_Th_CMD_Rebuild_Sequence_Table.Create(const ThEng_: TZDB2_Th_Engine; var Table_: TZDB2_BlockHandle);
begin
  inherited Create(ThEng_);
  Table_Ptr := @Table_;
  Init();
end;

procedure TZDB2_Th_CMD_Get_And_Clean_Sequence_Table.DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State);
var
  R_: TCMD_State;
  Mem64: TMem64;
begin
  R_ := TCMD_State.csDone;
  if (PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.Identifier = $FFFF) and
    CoreSpace.Check(PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.ID) then
    begin
      // read identifier
      Mem64 := TMem64.Create;
      if CoreSpace.ReadData(Mem64, PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.ID) then
        begin
          SetLength(Table_Ptr^, Mem64.Size shr 2);
          if length(Table_Ptr^) > 0 then
              CopyPtr(Mem64.Memory, @Table_Ptr^[0], length(Table_Ptr^) shl 2);
          disposeObject(Mem64);
        end
      else
          R_ := TCMD_State.csError;
      if not Engine.CoreSpace_IOHnd.IsOnlyRead then
        begin
          // remove identifier
          if not CoreSpace.RemoveData(PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.ID, False) then
              R_ := TCMD_State.csError;
          FillPtr(@CoreSpace.UserCustomHeader^[0], SizeOf(TSequence_Table_Head), 0);
        end;
    end
  else
      Table_Ptr^ := CoreSpace.BuildTableID;
  State^ := R_;
end;

constructor TZDB2_Th_CMD_Get_And_Clean_Sequence_Table.Create(const ThEng_: TZDB2_Th_Engine; var Table_: TZDB2_BlockHandle);
begin
  inherited Create(ThEng_);
  Table_Ptr := @Table_;
  Init();
end;

procedure TZDB2_Th_CMD_Flush_Sequence_Table.DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State);
var
  R_: TCMD_State;
  Mem64: TMem64;
  i, j: Integer;
begin
  if Engine.CoreSpace_IOHnd.IsOnlyRead then
    begin
      State^ := TCMD_State.csError;
      exit;
    end;

  R_ := TCMD_State.csDone;

  if (PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.Identifier = $FFFF) and
    CoreSpace.Check(PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.ID) then
    begin
      // remove identifier
      if not CoreSpace.RemoveData(PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.ID, False) then
          R_ := TCMD_State.csError;
      FillPtr(@CoreSpace.UserCustomHeader^[0], SizeOf(TSequence_Table_Head), 0);
    end;

  if length(Table_Ptr^) > 0 then
    begin
      // save identifier
      Mem64 := TMem64.Create;
      Mem64.Mapping(@Table_Ptr^[0], length(Table_Ptr^) shl 2);
      PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.Identifier := $FFFF;
      if not CoreSpace.WriteData(Mem64, PSequence_Table_Head(@CoreSpace.UserCustomHeader^[0])^.ID, True) then
          R_ := TCMD_State.csError;
      disposeObject(Mem64);
    end
  else
    begin
      FillPtr(@CoreSpace.UserCustomHeader^[0], SizeOf(TSequence_Table_Head), 0);
    end;
  State^ := R_;
end;

constructor TZDB2_Th_CMD_Flush_Sequence_Table.Create(const ThEng_: TZDB2_Th_Engine; var Table_: TZDB2_BlockHandle);
begin
  inherited Create(ThEng_);
  Table_Ptr := @Table_;
  Init();
end;

procedure TZDB2_Th_CMD_Extract_To.DoExecute(CoreSpace: TZDB2_Core_Space; State: PCMD_State);
var
  R_: TCMD_State;
  i: Integer;
  Mem64: TMem64;
  tmp_inst: TZDB2_Th_CMD_AppendFromMem64;
  p: PZDB2_Th_CMD_ID_And_State;
begin
  R_ := TCMD_State.csDone;
  SetLength(Output_Ptr^, length(Input_Ptr^));

  if length(Input_Ptr^) > 0 then
    begin
      for i := low(Input_Ptr^) to high(Input_Ptr^) do
        begin
          Mem64 := TMem64.Create;
          p := @Output_Ptr^[i];
          if CoreSpace.ReadData(Mem64, Input_Ptr^[i]) then
            begin
              tmp_inst := TZDB2_Th_CMD_AppendFromMem64.Create(Dest_Th_Engine, Mem64, p^.ID);
              tmp_inst.AutoFree_Data := True;
              tmp_inst.Ready(p^.State);
            end
          else
            begin
              R_ := TCMD_State.csError;
              p^.ID := -1;
              p^.State := TCMD_State.csError;
              disposeObject(Mem64);
            end;

          // wait queue
          if Wait_Queue and (Max_Queue > 0) then
            while Dest_Th_Engine.QueueNum > Max_Queue do
                TCompute.Sleep(1);
        end;

      // wait done
      if Wait_Queue then
        while p^.State = TCMD_State.csDefault do
            TCompute.Sleep(1);
    end;
  State^ := R_;
end;

constructor TZDB2_Th_CMD_Extract_To.Create(const ThEng_: TZDB2_Th_Engine;
  var Input_: TZDB2_BlockHandle;
  const Dest_Th_Engine_: TZDB2_Th_Engine; var Output_: TZDB2_Th_CMD_ID_And_State_Array);
begin
  inherited Create(ThEng_);
  Input_Ptr := @Input_;
  Dest_Th_Engine := Dest_Th_Engine_;
  Output_Ptr := @Output_;
  Max_Queue := 100;
  Wait_Queue := True;
  Init();
end;

procedure TZDB2_Th_CMD_Bridge_Mem64_And_State.CMD_Done;
begin
  if Assigned(OnResult_C) then
      OnResult_C(Mem64_And_State);
  if Assigned(OnResult_M) then
      OnResult_M(Mem64_And_State);
  if Assigned(OnResult_P) then
      OnResult_P(Mem64_And_State);
  Free;
end;

constructor TZDB2_Th_CMD_Bridge_Mem64_And_State.Create;
begin
  inherited Create;
  CMD := nil;
  Mem64_And_State.Mem64 := nil;
  Mem64_And_State.State := TCMD_State.csDefault;
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
end;

procedure TZDB2_Th_CMD_Bridge_Mem64_And_State.Init(CMD_: TZDB2_Th_CMD);
begin
  CMD := CMD_;
  CMD.OnDone := {$IFDEF FPC}@{$ENDIF FPC}CMD_Done;
end;

procedure TZDB2_Th_CMD_Bridge_Mem64_And_State.Ready;
begin
  CMD.Ready(Mem64_And_State.State);
end;

procedure TZDB2_Th_CMD_Bridge_Stream_And_State.CMD_Done;
begin
  if Assigned(OnResult_C) then
      OnResult_C(Stream_And_State);
  if Assigned(OnResult_M) then
      OnResult_M(Stream_And_State);
  if Assigned(OnResult_P) then
      OnResult_P(Stream_And_State);
  Free;
end;

constructor TZDB2_Th_CMD_Bridge_Stream_And_State.Create;
begin
  inherited Create;
  CMD := nil;
  Stream_And_State.Stream := nil;
  Stream_And_State.State := TCMD_State.csDefault;
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
end;

procedure TZDB2_Th_CMD_Bridge_Stream_And_State.Init(CMD_: TZDB2_Th_CMD);
begin
  CMD := CMD_;
  CMD.OnDone := {$IFDEF FPC}@{$ENDIF FPC}CMD_Done;
end;

procedure TZDB2_Th_CMD_Bridge_Stream_And_State.Ready;
begin
  CMD.Ready(Stream_And_State.State);
end;

procedure TZDB2_Th_CMD_Bridge_ID_And_State.CMD_Done;
begin
  if Assigned(OnResult_C) then
      OnResult_C(ID_And_State);
  if Assigned(OnResult_M) then
      OnResult_M(ID_And_State);
  if Assigned(OnResult_P) then
      OnResult_P(ID_And_State);
  Free;
end;

constructor TZDB2_Th_CMD_Bridge_ID_And_State.Create;
begin
  inherited Create;
  CMD := nil;
  ID_And_State.ID := -1;
  ID_And_State.State := TCMD_State.csDefault;
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
end;

procedure TZDB2_Th_CMD_Bridge_ID_And_State.Init(CMD_: TZDB2_Th_CMD);
begin
  CMD := CMD_;
  CMD.OnDone := {$IFDEF FPC}@{$ENDIF FPC}CMD_Done;
end;

procedure TZDB2_Th_CMD_Bridge_ID_And_State.Ready;
begin
  CMD.Ready(ID_And_State.State);
end;

procedure TZDB2_Th_Engine.ZDB2_ThRun_Proc(ThSender: TCompute);
var
  LTK, tmp: TTimeTick;
  CMD_: TZDB2_Th_CMD;
begin
  CoreSpace := TZDB2_Core_Space.Create(@CoreSpace_IOHnd);
  CoreSpace.Cipher := CoreSpace_Cipher;
  CoreSpace.Mode := smNormal;
  CoreSpace.AutoCloseIOHnd := True;
  CoreSpace.OnNoSpace := {$IFDEF FPC}@{$ENDIF FPC}DoNoSpace;
  if umlFileSize(CoreSpace_IOHnd) > 0 then
    if not CoreSpace.Open then
      begin
        try
            CoreSpace.Free;
        except
        end;

        FCMD_Execute_Thread_Is_Runing := False;
        FCMD_Execute_Thread_Is_Exit := True;
        exit;
      end;

  FCMD_Execute_Thread_Is_Runing := True;
  FCMD_Execute_Thread_Is_Exit := False;

  LTK := GetTimeTick();
  while FCMD_Execute_Thread_Is_Runing do
    begin
      if CMD_Queue.Num > 0 then
        begin
          CMD_ := CMD_Queue.First^.Data;
          CMD_.Execute();
          CMD_Queue.Next();
          LTK := GetTimeTick();
        end
      else
        begin
          tmp := GetTimeTick() - LTK;
          if tmp > 10000 then
              TCompute.Sleep(10)
          else if tmp > 1000 then
              TCompute.Sleep(1);
        end;
    end;

  try
      CoreSpace.Free;
  except
  end;

  FCMD_Execute_Thread_Is_Runing := False;
  FCMD_Execute_Thread_Is_Exit := True;
end;

procedure TZDB2_Th_Engine.Do_Free_CMD(var p: TZDB2_Th_CMD);
begin
  disposeObject(p);
end;

procedure TZDB2_Th_Engine.DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
begin
  retry := Trigger.AppendSpace(CoreSpace_Delta, CoreSpace_BlockSize);
end;

constructor TZDB2_Th_Engine.Create(Stream_: TCore_Stream; AutoFree_, OnlyRead_: Boolean; Delta_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
begin
  inherited Create;
  CMD_Queue := TZDB2_Th_CMD_Queue.Create;
  CMD_Queue.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Do_Free_CMD;
  FCMD_Execute_Thread_Is_Runing := False;
  FCMD_Execute_Thread_Is_Exit := False;
  CoreSpace_Delta := Delta_;
  CoreSpace_BlockSize := BlockSize_;
  CoreSpace_Cipher := Cipher_;
  InitIOHnd(CoreSpace_IOHnd);
  umlFileCreateAsStream(Stream_, CoreSpace_IOHnd, OnlyRead_);
  CoreSpace_IOHnd.AutoFree := AutoFree_;
  CoreSpace := nil;

  // test
  CoreSpace_IOHnd.Cache.UsedWriteCache := True;
  CoreSpace_IOHnd.Cache.UsedReadCache := True;

  // thread
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}ZDB2_ThRun_Proc);
  while not FCMD_Execute_Thread_Is_Runing do
      TCompute.Sleep(1);
end;

destructor TZDB2_Th_Engine.Destroy;
var
  tmp: TCMD_State;
begin
  Async_Flush;
  TZDB2_Th_CMD_Exit.Create(self).Ready(tmp);
  while not FCMD_Execute_Thread_Is_Exit do
      TCompute.Sleep(1);
  CMD_Queue.Free;
  inherited Destroy;
end;

function TZDB2_Th_Engine.QueueNum: NativeInt;
begin
  Result := CMD_Queue.Num;
end;

function TZDB2_Th_Engine.CoreSpace_Size: Int64;
begin
  CMD_Queue.Critical.Lock;
  if CoreSpace <> nil then
      Result := CoreSpace.State^.Physics - CoreSpace.State^.FreeSpace
  else
      Result := 0;
  CMD_Queue.Critical.UnLock;
end;

function TZDB2_Th_Engine.Sync_GetData(Mem64: TMem64; ID: Integer): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_GetDataAsMem64.Create(self, Mem64, ID).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Engine.Sync_SetData(Mem64: TMem64; var ID: Integer): Boolean;
var
  tmp: TCMD_State;
begin
  if ID < 0 then
      exit(Sync_Append(Mem64, ID));
  TZDB2_Th_CMD_SetDataFromMem64.Create(self, Mem64, ID).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Engine.Sync_Append(Mem64: TMem64; var ID: Integer): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_AppendFromMem64.Create(self, Mem64, ID).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Engine.Sync_GetData(Stream: TCore_Stream; ID: Integer): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_GetDataAsStream.Create(self, Stream, ID).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Engine.Sync_SetData(Stream: TCore_Stream; var ID: Integer): Boolean;
var
  tmp: TCMD_State;
begin
  if ID < 0 then
      exit(Sync_Append(Stream, ID));
  TZDB2_Th_CMD_SetDataFromStream.Create(self, Stream, ID).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Engine.Sync_Append(Stream: TCore_Stream; var ID: Integer): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_AppendFromStream.Create(self, Stream, ID).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Engine.Sync_Remove(ID: Integer): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_Remove.Create(self, ID).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Engine.Sync_Flush(): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_Flush.Create(self).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Engine.Sync_Rebuild_Sequence_Table(var Table_: TZDB2_BlockHandle): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_Rebuild_Sequence_Table.Create(self, Table_).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Engine.Sync_Get_And_Clean_Sequence_Table(var Table_: TZDB2_BlockHandle): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_Get_And_Clean_Sequence_Table.Create(self, Table_).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Engine.Sync_Flush_Sequence_Table(var Table_: TZDB2_BlockHandle): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_Flush_Sequence_Table.Create(self, Table_).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Engine.Sync_Flush_Sequence_Table(L: TZDB2_ID_List): Boolean;
var
  Table_: TZDB2_BlockHandle;
begin
  Table_ := TZDB2_Core_Space.Get_Handle(L);
  Result := Sync_Flush_Sequence_Table(Table_);
  SetLength(Table_, 0);
end;

function TZDB2_Th_Engine.Sync_Extract_To(var Input_: TZDB2_BlockHandle;
  const Dest_Th_Engine_: TZDB2_Th_Engine; var Output_: TZDB2_Th_CMD_ID_And_State_Array): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_Extract_To.Create(self, Input_, Dest_Th_Engine_, Output_).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

procedure TZDB2_Th_Engine.Async_GetData_AsMem64_C(ID: Integer; Mem64: TMem64; OnResult: TOn_Mem64_And_State_Event_C);
var
  tmp: TZDB2_Th_CMD_Bridge_Mem64_And_State;
  Inst_: TZDB2_Th_CMD_GetDataAsMem64;
begin
  tmp := TZDB2_Th_CMD_Bridge_Mem64_And_State.Create;
  tmp.Mem64_And_State.Mem64 := Mem64;
  Inst_ := TZDB2_Th_CMD_GetDataAsMem64.Create(self, tmp.Mem64_And_State.Mem64, ID);
  tmp.Init(Inst_);
  tmp.OnResult_C := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_GetData_AsMem64_M(ID: Integer; Mem64: TMem64; OnResult: TOn_Mem64_And_State_Event_M);
var
  tmp: TZDB2_Th_CMD_Bridge_Mem64_And_State;
  Inst_: TZDB2_Th_CMD_GetDataAsMem64;
begin
  tmp := TZDB2_Th_CMD_Bridge_Mem64_And_State.Create;
  tmp.Mem64_And_State.Mem64 := Mem64;
  Inst_ := TZDB2_Th_CMD_GetDataAsMem64.Create(self, tmp.Mem64_And_State.Mem64, ID);
  tmp.Init(Inst_);
  tmp.OnResult_M := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_GetData_AsMem64_P(ID: Integer; Mem64: TMem64; OnResult: TOn_Mem64_And_State_Event_P);
var
  tmp: TZDB2_Th_CMD_Bridge_Mem64_And_State;
  Inst_: TZDB2_Th_CMD_GetDataAsMem64;
begin
  tmp := TZDB2_Th_CMD_Bridge_Mem64_And_State.Create;
  tmp.Mem64_And_State.Mem64 := Mem64;
  Inst_ := TZDB2_Th_CMD_GetDataAsMem64.Create(self, tmp.Mem64_And_State.Mem64, ID);
  tmp.Init(Inst_);
  tmp.OnResult_P := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_GetData_AsStream_C(ID: Integer; Stream: TCore_Stream; OnResult: TOn_Stream_And_State_Event_C);
var
  tmp: TZDB2_Th_CMD_Bridge_Stream_And_State;
  Inst_: TZDB2_Th_CMD_GetDataAsStream;
begin
  tmp := TZDB2_Th_CMD_Bridge_Stream_And_State.Create;
  tmp.Stream_And_State.Stream := Stream;
  Inst_ := TZDB2_Th_CMD_GetDataAsStream.Create(self, tmp.Stream_And_State.Stream, ID);
  tmp.Init(Inst_);
  tmp.OnResult_C := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_GetData_AsStream_M(ID: Integer; Stream: TCore_Stream; OnResult: TOn_Stream_And_State_Event_M);
var
  tmp: TZDB2_Th_CMD_Bridge_Stream_And_State;
  Inst_: TZDB2_Th_CMD_GetDataAsStream;
begin
  tmp := TZDB2_Th_CMD_Bridge_Stream_And_State.Create;
  tmp.Stream_And_State.Stream := Stream;
  Inst_ := TZDB2_Th_CMD_GetDataAsStream.Create(self, tmp.Stream_And_State.Stream, ID);
  tmp.Init(Inst_);
  tmp.OnResult_M := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_GetData_AsStream_P(ID: Integer; Stream: TCore_Stream; OnResult: TOn_Stream_And_State_Event_P);
var
  tmp: TZDB2_Th_CMD_Bridge_Stream_And_State;
  Inst_: TZDB2_Th_CMD_GetDataAsStream;
begin
  tmp := TZDB2_Th_CMD_Bridge_Stream_And_State.Create;
  tmp.Stream_And_State.Stream := Stream;
  Inst_ := TZDB2_Th_CMD_GetDataAsStream.Create(self, tmp.Stream_And_State.Stream, ID);
  tmp.Init(Inst_);
  tmp.OnResult_P := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_SetData(Mem64: TMem64; AutoFree_Data: Boolean; ID: Integer);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_SetDataFromMem64;
begin
  if ID < 0 then
    begin
      Async_Append(Mem64, AutoFree_Data);
      exit;
    end;
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  tmp.ID_And_State.ID := ID;
  Inst_ := TZDB2_Th_CMD_SetDataFromMem64.Create(self, Mem64, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_SetData_C(Mem64: TMem64; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_C);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_SetDataFromMem64;
begin
  if ID < 0 then
    begin
      Async_Append_C(Mem64, AutoFree_Data, OnResult);
      exit;
    end;
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  tmp.ID_And_State.ID := ID;
  Inst_ := TZDB2_Th_CMD_SetDataFromMem64.Create(self, Mem64, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.OnResult_C := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_SetData_M(Mem64: TMem64; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_M);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_SetDataFromMem64;
begin
  if ID < 0 then
    begin
      Async_Append_M(Mem64, AutoFree_Data, OnResult);
      exit;
    end;
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  tmp.ID_And_State.ID := ID;
  Inst_ := TZDB2_Th_CMD_SetDataFromMem64.Create(self, Mem64, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.OnResult_M := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_SetData_P(Mem64: TMem64; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_P);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_SetDataFromMem64;
begin
  if ID < 0 then
    begin
      Async_Append_P(Mem64, AutoFree_Data, OnResult);
      exit;
    end;
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  tmp.ID_And_State.ID := ID;
  Inst_ := TZDB2_Th_CMD_SetDataFromMem64.Create(self, Mem64, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.OnResult_P := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_Append(Mem64: TMem64; AutoFree_Data: Boolean);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_AppendFromMem64;
begin
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  Inst_ := TZDB2_Th_CMD_AppendFromMem64.Create(self, Mem64, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_Append_C(Mem64: TMem64; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_C);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_AppendFromMem64;
begin
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  Inst_ := TZDB2_Th_CMD_AppendFromMem64.Create(self, Mem64, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.OnResult_C := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_Append_M(Mem64: TMem64; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_M);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_AppendFromMem64;
begin
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  Inst_ := TZDB2_Th_CMD_AppendFromMem64.Create(self, Mem64, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.OnResult_M := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_Append_P(Mem64: TMem64; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_P);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_AppendFromMem64;
begin
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  Inst_ := TZDB2_Th_CMD_AppendFromMem64.Create(self, Mem64, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.OnResult_P := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_SetData(Stream: TCore_Stream; AutoFree_Data: Boolean; ID: Integer);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_SetDataFromStream;
begin
  if ID < 0 then
    begin
      Async_Append(Stream, AutoFree_Data);
      exit;
    end;
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  tmp.ID_And_State.ID := ID;
  Inst_ := TZDB2_Th_CMD_SetDataFromStream.Create(self, Stream, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_SetData_C(Stream: TCore_Stream; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_C);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_SetDataFromStream;
begin
  if ID < 0 then
    begin
      Async_Append_C(Stream, AutoFree_Data, OnResult);
      exit;
    end;
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  tmp.ID_And_State.ID := ID;
  Inst_ := TZDB2_Th_CMD_SetDataFromStream.Create(self, Stream, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.OnResult_C := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_SetData_M(Stream: TCore_Stream; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_M);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_SetDataFromStream;
begin
  if ID < 0 then
    begin
      Async_Append_M(Stream, AutoFree_Data, OnResult);
      exit;
    end;
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  tmp.ID_And_State.ID := ID;
  Inst_ := TZDB2_Th_CMD_SetDataFromStream.Create(self, Stream, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.OnResult_M := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_SetData_P(Stream: TCore_Stream; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_P);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_SetDataFromStream;
begin
  if ID < 0 then
    begin
      Async_Append_P(Stream, AutoFree_Data, OnResult);
      exit;
    end;
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  tmp.ID_And_State.ID := ID;
  Inst_ := TZDB2_Th_CMD_SetDataFromStream.Create(self, Stream, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.OnResult_P := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_Append(Stream: TCore_Stream; AutoFree_Data: Boolean);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_AppendFromStream;
begin
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  Inst_ := TZDB2_Th_CMD_AppendFromStream.Create(self, Stream, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_Append_C(Stream: TCore_Stream; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_C);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_AppendFromStream;
begin
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  Inst_ := TZDB2_Th_CMD_AppendFromStream.Create(self, Stream, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.OnResult_C := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_Append_M(Stream: TCore_Stream; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_M);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_AppendFromStream;
begin
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  Inst_ := TZDB2_Th_CMD_AppendFromStream.Create(self, Stream, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.OnResult_M := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_Append_P(Stream: TCore_Stream; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_P);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_AppendFromStream;
begin
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  Inst_ := TZDB2_Th_CMD_AppendFromStream.Create(self, Stream, tmp.ID_And_State.ID);
  Inst_.AutoFree_Data := AutoFree_Data;
  tmp.Init(Inst_);
  tmp.OnResult_P := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_Remove(ID: Integer);
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
  Inst_: TZDB2_Th_CMD_Remove;
begin
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  tmp.ID_And_State.ID := ID;
  Inst_ := TZDB2_Th_CMD_Remove.Create(self, tmp.ID_And_State.ID);
  tmp.Init(Inst_);
  tmp.Ready;
end;

procedure TZDB2_Th_Engine.Async_Flush;
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
begin
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  tmp.Init(TZDB2_Th_CMD_Flush.Create(self));
  tmp.Ready;
end;

class function TZDB2_Th_Engine.Get_Handle(var buff: TZDB2_Th_CMD_ID_And_State_Array): TZDB2_BlockHandle;
var
  i: Integer;
begin
  SetLength(Result, length(buff));
  for i := low(buff) to high(buff) do
      Result[i] := buff[i].ID;
end;

class procedure TZDB2_Th_Engine.Test;
var
  tmp_inst1: TZDB2_Th_Engine;
  tmp_inst2: TZDB2_Th_Engine;
  tmp_inst3: TZDB2_Th_Engine;
  Mem64: TMS64;
  arry: TZDB2_BlockHandle;
  Output_: TZDB2_Th_CMD_ID_And_State_Array;
  i: Integer;
begin
  tmp_inst1 := TZDB2_Th_Engine.Create(TMS64.CustomCreate(1024 * 1024), True, False, 1024 * 1024, 4096, nil);
  tmp_inst2 := TZDB2_Th_Engine.Create(TMS64.CustomCreate(1024 * 1024), True, False, 1024 * 1024, 1000, nil);
  tmp_inst3 := TZDB2_Th_Engine.Create(TMS64.CustomCreate(1024 * 1024), True, False, 1024 * 1024, 500, nil);

  Mem64 := TMS64.Create;
  Mem64.Size := 3992;

  SetLength(arry, 4);
  for i := low(arry) to high(arry) do
    begin
      MT19937Rand32(MaxInt, Mem64.Memory, Mem64.Size shr 2);
      tmp_inst1.Sync_Append(Mem64, arry[i]);
      DoStatus(umlMD5ToStr(Mem64.ToMD5));
    end;
  tmp_inst1.Sync_Flush_Sequence_Table(arry);

  DoStatus('');

  tmp_inst1.Sync_Extract_To(arry, tmp_inst2, Output_);
  arry := Get_Handle(Output_);
  tmp_inst2.Sync_Flush_Sequence_Table(arry);
  SetLength(arry, 0);
  tmp_inst2.Sync_Get_And_Clean_Sequence_Table(arry);
  tmp_inst2.Sync_Flush_Sequence_Table(arry);
  for i := low(arry) to high(arry) do
    begin
      arry[i] := arry[i];
      Mem64.Clear;
      tmp_inst2.Sync_GetData(Mem64, arry[i]);
      DoStatus(umlMD5ToStr(Mem64.ToMD5));
    end;

  DoStatus('');

  tmp_inst2.Sync_Extract_To(arry, tmp_inst3, Output_);
  arry := Get_Handle(Output_);
  tmp_inst3.Sync_Flush_Sequence_Table(arry);

  Mem64.Free;

  tmp_inst1.Free;
  tmp_inst2.Free;
  tmp_inst3.Free;
end;

procedure TZDB2_Th_Engine_ABI_Data.Do_Save_ID_And_State_Event(var Sender: TZDB2_Th_CMD_ID_And_State);
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

procedure TZDB2_Th_Engine_ABI_Data.Wait_Busy_Done;
begin
  while FBusy_Task_Num > 0 do
      TCompute.Sleep(10);
end;

constructor TZDB2_Th_Engine_ABI_Data.Create();
begin
  inherited Create;
  FOwner := nil;
  FOwner_Data_Ptr := nil;
  FTh_Engine := nil;
  FTh_Engine_Data_Ptr := nil;
  FID := -1;
  FBusy_Task_Num := 0;
end;

destructor TZDB2_Th_Engine_ABI_Data.Destroy;
begin
  Wait_Busy_Done;
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_ABI_Data.Do_Progress();
begin

end;

function TZDB2_Th_Engine_ABI_Data.Engine: TZDB2_Th_Engine;
begin
  if FTh_Engine <> nil then
      Result := FTh_Engine.Engine
  else
      Result := nil;
end;

procedure TZDB2_Th_Engine_ABI_Data.Delete;
begin
  if FBusy_Task_Num > 0 then
      exit;

  if (FID >= 0) then
    begin
      AtomInc(FBusy_Task_Num);
      Engine.Async_Remove(FID);
      AtomDec(FBusy_Task_Num);
    end;
  FID := -1;

  Do_Post_To_Pool_Free();
end;

function TZDB2_Th_Engine_ABI_Data.Do_Load_Data(Stream: TMS64): Boolean;
begin
  Result := False;
  Wait_Busy_Done;
  if (FID >= 0) then
    begin
      AtomInc(FBusy_Task_Num);
      Result := Engine.Sync_GetData(Stream, FID);
      AtomDec(FBusy_Task_Num);
    end;
end;

function TZDB2_Th_Engine_ABI_Data.Do_Load_Data(Mem64: TMem64): Boolean;
begin
  Result := False;
  Wait_Busy_Done;
  if (FID >= 0) then
    begin
      AtomInc(FBusy_Task_Num);
      Result := Engine.Sync_GetData(Mem64, FID);
      AtomDec(FBusy_Task_Num);
    end;
end;

procedure TZDB2_Th_Engine_ABI_Data.Do_Async_Save_And_Free_Data(var Stream: TMS64);
begin
  Wait_Busy_Done;
  AtomInc(FBusy_Task_Num);
  Engine.Async_SetData_M(Stream, True, FID, {$IFDEF FPC}@{$ENDIF FPC}Do_Save_ID_And_State_Event);
  Stream := nil;
end;

procedure TZDB2_Th_Engine_ABI_Data.Do_Async_Save_And_Free_Data(var Mem64: TMem64);
begin
  Wait_Busy_Done;
  AtomInc(FBusy_Task_Num);
  Engine.Async_SetData_M(Mem64, True, FID, {$IFDEF FPC}@{$ENDIF FPC}Do_Save_ID_And_State_Event);
  Mem64 := nil;
end;

procedure TZDB2_Th_Engine_ABI_Data.Do_Post_To_Pool_Free;
begin
  if FBusy_Task_Num > 0 then
      exit;

  if (FTh_Engine <> nil) and (FTh_Engine_Data_Ptr <> nil) then
    begin
      FTh_Engine.Th_Engine_Data_Pool.Push_To_Recycle_Pool(FTh_Engine_Data_Ptr);
    end
  else if (FOwner <> nil) and (FOwner_Data_Ptr <> nil) then
    begin
      FOwner.Marshal_Data_Pool.Push_To_Recycle_Pool(FOwner_Data_Ptr);
    end;
end;

procedure TZDB2_Th_Engine_ABI_Data.Check_Data_And_Loaded_On_Thread(const StreamM: TMS64);
begin

end;

constructor TZDB2_Th_Engine_ABI_Data_Mem64.Create;
begin
  inherited Create;
  FAlive := GetTimeTick();
  FData_Mem64 := nil;
  FData_Mem64_MD5 := NullMD5;
end;

destructor TZDB2_Th_Engine_ABI_Data_Mem64.Destroy;
begin
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_ABI_Data_Mem64.Do_Progress();
begin
  if FData_Mem64 = nil then
      exit;
  if (GetTimeTick() - FAlive > FTh_Engine.TimeOut) then
      Save();
end;

procedure TZDB2_Th_Engine_ABI_Data_Mem64.Load;
begin
  FData_Mem64_MD5 := NullMD5;
  FData_Mem64.Clear;
  if Do_Load_Data(FData_Mem64) then
      FData_Mem64_MD5 := FData_Mem64.ToMD5;
end;

procedure TZDB2_Th_Engine_ABI_Data_Mem64.Save;
var
  tmp_md5: TMD5;
begin
  if FData_Mem64 = nil then
      exit;

  tmp_md5 := FData_Mem64.ToMD5;
  if (FID < 0) or umlMD5Compare(FData_Mem64_MD5, NullMD5) or (not umlMD5Compare(tmp_md5, FData_Mem64_MD5)) then
    begin
      FData_Mem64_MD5 := tmp_md5;
      Do_Async_Save_And_Free_Data(FData_Mem64);
    end
  else
      DisposeObjectAndNil(FData_Mem64);
end;

procedure TZDB2_Th_Engine_ABI_Data_Mem64.RecycleMemory;
begin
  DisposeObjectAndNil(FData_Mem64);
end;

function TZDB2_Th_Engine_ABI_Data_Mem64.GetData_Mem64: TMem64;
begin
  if FData_Mem64 = nil then
    begin
      FData_Mem64 := TMem64.Create;
      Load;
    end;
  Result := FData_Mem64;
  FAlive := GetTimeTick;
end;

constructor TZDB2_Th_Engine_ABI_Data_MS64.Create;
begin
  inherited Create;
  FAlive := GetTimeTick();
  FData_MS64 := nil;
  FData_MS64_MD5 := NullMD5;
end;

destructor TZDB2_Th_Engine_ABI_Data_MS64.Destroy;
begin
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_ABI_Data_MS64.Do_Progress();
begin
  if FData_MS64 = nil then
      exit;
  if (GetTimeTick() - FAlive > FTh_Engine.TimeOut) then
      Save();
end;

procedure TZDB2_Th_Engine_ABI_Data_MS64.Load;
begin
  FData_MS64_MD5 := NullMD5;
  FData_MS64.Clear;
  if Do_Load_Data(FData_MS64) then
      FData_MS64_MD5 := FData_MS64.ToMD5;
end;

procedure TZDB2_Th_Engine_ABI_Data_MS64.Save;
var
  tmp_md5: TMD5;
begin
  if FData_MS64 = nil then
      exit;

  tmp_md5 := FData_MS64.ToMD5;
  if (FID < 0) or umlMD5Compare(FData_MS64_MD5, NullMD5) or (not umlMD5Compare(tmp_md5, FData_MS64_MD5)) then
    begin
      FData_MS64_MD5 := tmp_md5;
      Do_Async_Save_And_Free_Data(FData_MS64);
    end
  else
      DisposeObjectAndNil(FData_MS64);
end;

procedure TZDB2_Th_Engine_ABI_Data_MS64.RecycleMemory;
begin
  DisposeObjectAndNil(FData_MS64);
end;

function TZDB2_Th_Engine_ABI_Data_MS64.GetData_MS64: TMS64;
begin
  if FData_MS64 = nil then
    begin
      FData_MS64 := TMS64.Create;
      Load;
    end;
  Result := FData_MS64;
  FAlive := GetTimeTick;
end;

procedure TZDB2_Th_Engine_ABI_Config.Do_Progress(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Data_BigList_Decl__.PQueueStruct; var Aborted: Boolean);
begin
  if (p^.Data = nil) or (p^.Data.FBusy_Task_Num > 0) or (p^.Data.FTh_Engine = nil) then
      exit;
  p^.Data.Do_Progress();
end;

procedure TZDB2_Th_Engine_ABI_Config.DoFree(var Data: TZDB2_Th_Engine_ABI_Data);
begin
  if Data = nil then
      exit;
  Data.Wait_Busy_Done;
  Data.FTh_Engine := nil;
  Data.FTh_Engine_Data_Ptr := nil;
  Data.FID := -1;
  if (Data.FOwner <> nil) and (Data.FOwner_Data_Ptr <> nil) then
      Data.FOwner.Marshal_Data_Pool.Remove(Data.FOwner_Data_Ptr)
  else
      disposeObject(Data);
end;

constructor TZDB2_Th_Engine_ABI_Config.Create(Owner_: TZDB2_Th_Engine_ABI_Data_Marshal);
begin
  inherited Create;
  Name := '';
  Owner := Owner_;
  TimeOut := 5000;
  Database_File := '';
  OnlyRead := False;
  Delta := 16 * 1024 * 1024;
  BlockSize := 1536;
  Cipher := nil;
  Cipher_Security := TCipherSecurity.csRijndael;
  Cipher_password := 'DTC40@ZSERVER';
  Cipher_Level := 1;
  Cipher_Tail := True;
  Cipher_CBC := True;
  Engine := nil;
  Th_Engine_Data_Pool := TZDB2_Th_Engine_ABI_Data_BigList__.Create;
  Th_Engine_Data_Pool.OnFree := {$IFDEF FPC}@{$ENDIF FPC}DoFree;
  Owner.Th_Engine_List.Add(self);
end;

destructor TZDB2_Th_Engine_ABI_Config.Destroy;
begin
  Flush;
  DisposeObjectAndNil(Th_Engine_Data_Pool);
  DisposeObjectAndNil(Engine);
  DisposeObjectAndNil(Cipher);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_ABI_Config.ReadConfig(Name_: U_String; cfg: THashStringList);
begin
  Name := Name_;
  TimeOut := EStrToInt(cfg.GetDefaultValue('TimeOut', umlIntToStr(TimeOut)), TimeOut);
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

procedure TZDB2_Th_Engine_ABI_Config.WriteConfig(cfg: THashStringList);
begin
  cfg.SetDefaultValue('TimeOut', umlIntToStr(TimeOut));
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

procedure TZDB2_Th_Engine_ABI_Config.Build(Data_Class: TZDB2_Th_Engine_ABI_Data_Class);
var
  Stream: TCore_Stream;
  Queue_Table_: TZDB2_BlockHandle;
  ID: Integer;
begin
  DisposeObjectAndNil(Engine);
  DisposeObjectAndNil(Cipher);
  Th_Engine_Data_Pool.Clear;

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
            Engine := TZDB2_Th_Engine.Create(Stream, True, OnlyRead, Delta, BlockSize, Cipher);
      end
    else if TZDB2_Core_Space.CheckStream(Stream, Cipher) then // check open from cipher
      begin
        Engine := TZDB2_Th_Engine.Create(Stream, True, OnlyRead, Delta, BlockSize, Cipher);
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
      disposeObject(Stream);
end;

procedure TZDB2_Th_Engine_ABI_Config.Rebuild_Data_Pool(Data_Class: TZDB2_Th_Engine_ABI_Data_Class);
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
end;

function TZDB2_Th_Engine_ABI_Config.Flush: Boolean;
var
  Queue_ID_List_: TZDB2_ID_List;
{$IFDEF FPC}
  procedure fpc_progress_(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Data_BigList__.PQueueStruct; var Aborted: Boolean);
  begin
    p^.Data.Wait_Busy_Done;
    if (p^.Data <> nil) and (p^.Data.FID >= 0) then
        Queue_ID_List_.Add(p^.Data.FID);
  end;
{$ENDIF FPC}


begin
  if Engine = nil then
      exit(False);
  Engine.Async_Flush;
  Queue_ID_List_ := TZDB2_ID_List.Create;
  if Th_Engine_Data_Pool.Num > 0 then
    begin
{$IFDEF FPC}
      Th_Engine_Data_Pool.Progress_P(@fpc_progress_);
{$ELSE FPC}
      Th_Engine_Data_Pool.Progress_P(procedure(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Data_BigList__.PQueueStruct; var Aborted: Boolean)
        begin
          p^.Data.Wait_Busy_Done;
          if (p^.Data <> nil) and (p^.Data.FID >= 0) then
              Queue_ID_List_.Add(p^.Data.FID);
        end);
{$ENDIF FPC}
    end;
  Result := Engine.Sync_Flush_Sequence_Table(Queue_ID_List_);
  disposeObject(Queue_ID_List_);
end;

function TZDB2_Th_Engine_ABI_Config.Add(Data_Class: TZDB2_Th_Engine_ABI_Data_Class; ID: Integer): TZDB2_Th_Engine_ABI_Data;
var
  Data_Instance: TZDB2_Th_Engine_ABI_Data;
begin
  if Engine = nil then
      exit(nil);
  Data_Instance := Data_Class.Create();
  Data_Instance.FOwner := Owner;
  Data_Instance.FOwner_Data_Ptr := Owner.Marshal_Data_Pool.Add(Data_Instance);
  Data_Instance.FTh_Engine := self;
  Data_Instance.FTh_Engine_Data_Ptr := Th_Engine_Data_Pool.Add(Data_Instance);
  Data_Instance.FID := ID;
  Result := Data_Instance;
end;

function TZDB2_Th_Engine_ABI_Config.Add(Data_Class: TZDB2_Th_Engine_ABI_Data_Class): TZDB2_Th_Engine_ABI_Data;
begin
  Result := Add(Data_Class, -1);
end;

procedure TZDB2_Th_Engine_ABI_Config.Progress;
begin
  if Engine = nil then
      exit;
  Th_Engine_Data_Pool.Progress_M({$IFDEF FPC}@{$ENDIF FPC}Do_Progress);
end;

function TZDB2_Th_Engine_ABI_Config.Get_Busy_Data_Num: Int64;
var
  R_: Int64;
{$IFDEF FPC}
  procedure fpc_progress_(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Data_BigList__.PQueueStruct; var Aborted: Boolean);
  begin
    if p^.Data.FBusy_Task_Num > 0 then
        inc(R_);
  end;
{$ENDIF FPC}


begin
  if Engine = nil then
      exit(0);
  R_ := 0;
{$IFDEF FPC}
  Th_Engine_Data_Pool.Progress_P(@fpc_progress_);
{$ELSE FPC}
  Th_Engine_Data_Pool.Progress_P(procedure(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Data_BigList__.PQueueStruct; var Aborted: Boolean)
    begin
      if p^.Data.FBusy_Task_Num > 0 then
          inc(R_);
    end);
{$ENDIF FPC}
  Result := R_;
end;

function TZDB2_Th_Engine_ABI_Config.Get_Busy_Task_Num: Int64;
var
  R_: Int64;
{$IFDEF FPC}
  procedure fpc_progress_(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Data_BigList__.PQueueStruct; var Aborted: Boolean);
  begin
    inc(R_, p^.Data.FBusy_Task_Num);
  end;
{$ENDIF FPC}


begin
  if Engine = nil then
      exit(0);
  R_ := 0;
{$IFDEF FPC}
  Th_Engine_Data_Pool.Progress_P(@fpc_progress_);
{$ELSE FPC}
  Th_Engine_Data_Pool.Progress_P(procedure(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Data_BigList__.PQueueStruct; var Aborted: Boolean)
    begin
      inc(R_, p^.Data.FBusy_Task_Num);
    end);
{$ENDIF FPC}
  Result := R_;
end;

constructor TZDB2_Th_Engine_ABI_Config_List.Create;
begin
  inherited Create;
end;

destructor TZDB2_Th_Engine_ABI_Config_List.Destroy;
begin
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_ABI_Config_List.DoFree(var Data: TZDB2_Th_Engine_ABI_Config);
begin
  disposeObject(Data);
end;

function TZDB2_Th_Engine_ABI_Config_List.Get_Minimize_Size_Engine: TZDB2_Th_Engine_ABI_Config;
var
  Eng_: PQueueStruct;
{$IFDEF FPC}
  procedure do_fpc_Progress(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean);
  begin
    if p^.Data.Engine = nil then
        exit;
    if Eng_ = nil then
        Eng_ := p
    else if p^.Data.Engine.CoreSpace_Size < Eng_^.Data.Engine.CoreSpace_Size then
        Eng_ := p;
  end;
{$ENDIF FPC}


begin
  Result := nil;
  if Num = 0 then
      exit;
  Eng_ := nil;
{$IFDEF FPC}
  Progress_P(@do_fpc_Progress);
{$ELSE FPC}
  Progress_P(procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean)
    begin
      if p^.Data.Engine = nil then
          exit;
      if Eng_ = nil then
          Eng_ := p
      else if p^.Data.Engine.CoreSpace_Size < Eng_^.Data.Engine.CoreSpace_Size then
          Eng_ := p;
    end);
{$ENDIF FPC}
  if Eng_ <> nil then
    begin
      Result := Eng_^.Data;
      MoveToLast(Eng_);
    end;
end;

function TZDB2_Th_Engine_ABI_Config_List.Get_Minimize_Workload_Engine: TZDB2_Th_Engine_ABI_Config;
var
  Eng_: PQueueStruct;
{$IFDEF FPC}
  procedure do_fpc_Progress(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean);
  begin
    if p^.Data.Engine = nil then
        exit;
    if Eng_ = nil then
        Eng_ := p
    else if p^.Data.Engine.QueueNum < Eng_^.Data.Engine.QueueNum then
        Eng_ := p;
  end;
{$ENDIF FPC}


begin
  Result := nil;
  if Num = 0 then
      exit;
  Eng_ := nil;
{$IFDEF FPC}
  Progress_P(@do_fpc_Progress);
{$ELSE FPC}
  Progress_P(procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean)
    begin
      if p^.Data.Engine = nil then
          exit;
      if Eng_ = nil then
          Eng_ := p
      else if p^.Data.Engine.QueueNum < Eng_^.Data.Engine.QueueNum then
          Eng_ := p;
    end);
{$ENDIF FPC}
  if Eng_ <> nil then
    begin
      Result := Eng_^.Data;
      MoveToLast(Eng_);
    end;
end;

procedure TZDB2_Th_Engine_ABI_Data_Marshal.DoFree(var Data: TZDB2_Th_Engine_ABI_Data);
begin
  if Data = nil then
      exit;
  Data.Wait_Busy_Done;
  Data.FOwner := nil;
  Data.FOwner_Data_Ptr := nil;
  if (Data.FTh_Engine <> nil) and (Data.FTh_Engine_Data_Ptr <> nil) then
      Data.FTh_Engine.Th_Engine_Data_Pool.Remove(Data.FTh_Engine_Data_Ptr)
  else
      disposeObject(Data);
end;

constructor TZDB2_Th_Engine_ABI_Data_Marshal.Create;
begin
  inherited Create;
  Marshal_Data_Pool := TZDB2_Th_Engine_ABI_Data_Marshal_BigList__.Create;
  Marshal_Data_Pool.OnFree := {$IFDEF FPC}@{$ENDIF FPC}DoFree;
  Th_Engine_List := TZDB2_Th_Engine_ABI_Config_List.Create;
end;

destructor TZDB2_Th_Engine_ABI_Data_Marshal.Destroy;
begin
  DisposeObjectAndNil(Th_Engine_List);
  DisposeObjectAndNil(Marshal_Data_Pool);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_ABI_Data_Marshal.Build(Data_Class: TZDB2_Th_Engine_ABI_Data_Class);
{$IFDEF FPC}
  procedure do_fpc_Progress(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Config_List.PQueueStruct; var Aborted: Boolean);
  begin
    p^.Data.Build(Data_Class);
  end;
{$ENDIF FPC}


begin
{$IFDEF FPC}
  Th_Engine_List.Progress_P(@do_fpc_Progress);
{$ELSE FPC}
  Th_Engine_List.Progress_P(procedure(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Config_List.PQueueStruct; var Aborted: Boolean)
    begin
      p^.Data.Build(Data_Class);
    end);
{$ENDIF FPC}
end;

function TZDB2_Th_Engine_ABI_Data_Marshal.QueueNum: NativeInt;
var
  QueueNum_Reuslt_: NativeInt;
{$IFDEF FPC}
  procedure do_fpc_Progress(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Config_List.PQueueStruct; var Aborted: Boolean);
  begin
    inc(QueueNum_Reuslt_, p^.Data.Engine.QueueNum);
  end;
{$ENDIF FPC}


begin
  QueueNum_Reuslt_ := 0;
{$IFDEF FPC}
  Th_Engine_List.Progress_P(@do_fpc_Progress);
{$ELSE FPC}
  Th_Engine_List.Progress_P(procedure(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Config_List.PQueueStruct; var Aborted: Boolean)
    begin
      inc(QueueNum_Reuslt_, p^.Data.Engine.QueueNum);
    end);
{$ENDIF FPC}
  Result := QueueNum_Reuslt_;
end;

procedure TZDB2_Th_Engine_ABI_Data_Marshal.Progress;
{$IFDEF FPC}
  procedure do_fpc_Progress(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Config_List.PQueueStruct; var Aborted: Boolean);
  begin
    p^.Data.Progress;
  end;
  procedure do_fpc_Recycle(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Config_List.PQueueStruct; var Aborted: Boolean);
  begin
    p^.Data.Th_Engine_Data_Pool.Free_Recycle_Pool;
  end;
{$ENDIF FPC}


begin
  // free thread engine recycle pool
{$IFDEF FPC}
  Th_Engine_List.Progress_P(@do_fpc_Recycle);
{$ELSE FPC}
  Th_Engine_List.Progress_P(procedure(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Config_List.PQueueStruct; var Aborted: Boolean)
    begin
      p^.Data.Th_Engine_Data_Pool.Free_Recycle_Pool;
    end);
{$ENDIF FPC}
  // free local recycle pool
  Marshal_Data_Pool.Free_Recycle_Pool;

  // progress thread engine
{$IFDEF FPC}
  Th_Engine_List.Progress_P(@do_fpc_Progress);
{$ELSE FPC}
  Th_Engine_List.Progress_P(procedure(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Config_List.PQueueStruct; var Aborted: Boolean)
    begin
      p^.Data.Progress;
    end);
{$ENDIF FPC}

  // free thread engine recycle pool
{$IFDEF FPC}
  Th_Engine_List.Progress_P(@do_fpc_Recycle);
{$ELSE FPC}
  Th_Engine_List.Progress_P(procedure(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Config_List.PQueueStruct; var Aborted: Boolean)
    begin
      p^.Data.Th_Engine_Data_Pool.Free_Recycle_Pool;
    end);
{$ENDIF FPC}
  // free local recycle pool
  Marshal_Data_Pool.Free_Recycle_Pool;
end;

function TZDB2_Th_Engine_ABI_Data_Marshal.Get_Busy_Data_Num: Int64;
var
  R_: Int64;
{$IFDEF FPC}
  procedure fpc_progress_(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Config_List.PQueueStruct; var Aborted: Boolean);
  begin
    inc(R_, p^.Data.Get_Busy_Data_Num);
  end;
{$ENDIF FPC}


begin
  R_ := 0;
{$IFDEF FPC}
  Th_Engine_List.Progress_P(@fpc_progress_);
{$ELSE FPC}
  Th_Engine_List.Progress_P(procedure(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Config_List.PQueueStruct; var Aborted: Boolean)
    begin
      inc(R_, p^.Data.Get_Busy_Data_Num);
    end);
{$ENDIF FPC}
  Result := R_;
end;

function TZDB2_Th_Engine_ABI_Data_Marshal.Get_Busy_Task_Num: Int64;
var
  R_: Int64;
{$IFDEF FPC}
  procedure fpc_progress_(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Config_List.PQueueStruct; var Aborted: Boolean);
  begin
    inc(R_, p^.Data.Get_Busy_Task_Num);
  end;
{$ENDIF FPC}


begin
  R_ := 0;
{$IFDEF FPC}
  Th_Engine_List.Progress_P(@fpc_progress_);
{$ELSE FPC}
  Th_Engine_List.Progress_P(procedure(Index_: NativeInt; p: TZDB2_Th_Engine_ABI_Config_List.PQueueStruct; var Aborted: Boolean)
    begin
      inc(R_, p^.Data.Get_Busy_Task_Num);
    end);
{$ENDIF FPC}
  Result := R_;
end;

function TZDB2_Th_Engine_ABI_Data_Marshal.Add_Data_To_Minimize_Workload_Engine(Data_Class: TZDB2_Th_Engine_ABI_Data_Class): TZDB2_Th_Engine_ABI_Data;
var
  Eng_: TZDB2_Th_Engine_ABI_Config;
begin
  Eng_ := Th_Engine_List.Get_Minimize_Workload_Engine;
  if Eng_ <> nil then
      Result := Eng_.Add(Data_Class)
  else
      Result := nil;
end;

function TZDB2_Th_Engine_ABI_Data_Marshal.Add_Data_To_Minimize_Size_Engine(Data_Class: TZDB2_Th_Engine_ABI_Data_Class): TZDB2_Th_Engine_ABI_Data;
var
  Eng_: TZDB2_Th_Engine_ABI_Config;
begin
  Eng_ := Th_Engine_List.Get_Minimize_Size_Engine;
  if Eng_ <> nil then
      Result := Eng_.Add(Data_Class)
  else
      Result := nil;
end;

class procedure TZDB2_Th_Engine_ABI_Data_Marshal.Test;
const
  C_cfg = '[1]'#13#10 +
    'database='#13#10 +
    'OnlyRead=False'#13#10 +
    'Delta=1*1024*1024'#13#10 +
    'BlockSize=1536'#13#10 +
    'Security=Rijndael'#13#10 +
    'Password=ZDB_2.0'#13#10 +
    'Level=1'#13#10 +
    'Tail=True'#13#10 +
    'CBC=True'#13#10 +
    'TimeOut=5000'#13#10 +
    #13#10 +
    '[2]'#13#10 +
    'database='#13#10 +
    'OnlyRead=False'#13#10 +
    'Delta=1*1024*1024'#13#10 +
    'BlockSize=1536'#13#10 +
    'Security=Rijndael'#13#10 +
    'Password=ZDB_2.0'#13#10 +
    'Level=1'#13#10 +
    'Tail=True'#13#10 +
    'CBC=True'#13#10 +
    'TimeOut=5000'#13#10 +
    #13#10 +
    '[3]'#13#10 +
    'database='#13#10 +
    'OnlyRead=False'#13#10 +
    'Delta=1*1024*1024'#13#10 +
    'BlockSize=1536'#13#10 +
    'Security=Rijndael'#13#10 +
    'Password=ZDB_2.0'#13#10 +
    'Level=1'#13#10 +
    'Tail=True'#13#10 +
    'CBC=True'#13#10 +
    'TimeOut=5000'#13#10 +
    #13#10 +
    '[4]'#13#10 +
    'database='#13#10 +
    'OnlyRead=False'#13#10 +
    'Delta=1*1024*1024'#13#10 +
    'BlockSize=1536'#13#10 +
    'Security=Rijndael'#13#10 +
    'Password=ZDB_2.0'#13#10 +
    'Level=1'#13#10 +
    'Tail=True'#13#10 +
    'CBC=True'#13#10 +
    'TimeOut=5000'#13#10;

var
  DM: TZDB2_Th_Engine_ABI_Data_Marshal;
  TE: THashTextEngine;
  L: TListPascalString;
  Eng_: TZDB2_Th_Engine_ABI_Config;
  tk: TTimeTick;
  i: Integer;
  tmp: TZDB2_Th_Engine_ABI_Data_Mem64;
begin
  DM := TZDB2_Th_Engine_ABI_Data_Marshal.Create;
  TE := THashTextEngine.Create;
  TE.AsText := C_cfg;

  L := TListPascalString.Create;
  TE.GetSectionList(L);
  for i := 0 to L.count - 1 do
    begin
      Eng_ := TZDB2_Th_Engine_ABI_Config.Create(DM);
      Eng_.ReadConfig(L[i], TE.HStringList[L[i]]);
    end;
  disposeObject(L);
  TE.Free;
  DM.Build(TZDB2_Th_Engine_ABI_Data_Mem64);

  DM.Th_Engine_List.Get_Minimize_Size_Engine;

  for i := 0 to 500 do
    begin
      Eng_ := DM.Th_Engine_List.Get_Minimize_Workload_Engine;
      tmp := TZDB2_Th_Engine_ABI_Data_Mem64(Eng_.Add(TZDB2_Th_Engine_ABI_Data_Mem64));
      tmp.Data_Mem64.Size := umlRandomRange(1192, 16384);
      MT19937Rand32(MaxInt, tmp.Data_Mem64.Memory, tmp.Data_Mem64.Size shr 2);
      tmp.Save;
    end;

  tk := GetTimeTick;
  while (DM.Get_Busy_Task_Num > 0) do
    begin
      DM.Progress;
      TCompute.Sleep(100);
    end;

  disposeObject(DM);
end;

end.
