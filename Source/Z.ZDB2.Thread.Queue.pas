{ ****************************************************************************** }
{ * ZDB 2.0 Core-Thread Queue for HPC                                          * }
{ ****************************************************************************** }
unit Z.ZDB2.Thread.Queue;

{$I Z.Define.inc}

interface

uses Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib,
  Z.MemoryStream,
  Z.Status, Z.Cipher, Z.ZDB2, Z.ListEngine, Z.TextDataEngine, Z.IOThread,
  Z.Notify;

type
{$REGION 'Command_Queue'}
  TZDB2_Th_Queue = class;

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
    Engine: TZDB2_Th_Queue;
    State_Ptr: PCMD_State;
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); virtual; abstract;
  public
    constructor Create(const ThEng_: TZDB2_Th_Queue);
    destructor Destroy; override;
    procedure Init; virtual;
    procedure Ready(var State_: TCMD_State); overload;
    procedure Execute;
  end;

  TZDB2_Th_CMD_GetDataAsMem64 = class(TZDB2_Th_CMD)
  private
    Param_ID: Integer;
    Param_M64: TMem64;
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Queue; const Mem64: TMem64; const ID: Integer);
  end;

  TZDB2_Th_CMD_GetDataAsStream = class(TZDB2_Th_CMD)
  private
    Param_ID: Integer;
    Param_Stream: TCore_Stream;
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Queue; const Stream: TCore_Stream; const ID: Integer);
  end;

  TZDB2_Th_CMD_SetDataFromMem64 = class(TZDB2_Th_CMD)
  private
    Param_ID_Ptr: PInteger;
    Param_M64: TMem64;
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    AutoFree_Data: Boolean;
    constructor Create(const ThEng_: TZDB2_Th_Queue; const Mem64: TMem64; var ID: Integer);
  end;

  TZDB2_Th_CMD_SetDataFromStream = class(TZDB2_Th_CMD)
  private
    Param_ID_Ptr: PInteger;
    Param_Stream: TCore_Stream;
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    AutoFree_Data: Boolean;
    constructor Create(const ThEng_: TZDB2_Th_Queue; const Stream: TCore_Stream; var ID: Integer);
  end;

  TZDB2_Th_CMD_AppendFromMem64 = class(TZDB2_Th_CMD)
  private
    Param_ID_Ptr: PInteger;
    Param_M64: TMem64;
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    AutoFree_Data: Boolean;
    constructor Create(const ThEng_: TZDB2_Th_Queue; const Mem64: TMem64; var ID: Integer);
  end;

  TZDB2_Th_CMD_AppendFromStream = class(TZDB2_Th_CMD)
  private
    Param_ID_Ptr: PInteger;
    Param_Stream: TCore_Stream;
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    AutoFree_Data: Boolean;
    constructor Create(const ThEng_: TZDB2_Th_Queue; const Stream: TCore_Stream; var ID: Integer);
  end;

  TZDB2_Th_CMD_Remove = class(TZDB2_Th_CMD)
  private
    Param_ID: Integer;
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Queue; const ID: Integer);
  end;

  TZDB2_Th_CMD_Exit = class(TZDB2_Th_CMD)
  private
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Queue);
  end;

  TZDB2_Th_CMD_Flush = class(TZDB2_Th_CMD)
  private
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Queue);
  end;

  TSequence_Table_Head = packed record
    Identifier: Word;
    ID: Integer;
  end;

  PSequence_Table_Head = ^TSequence_Table_Head;

  TZDB2_Th_CMD_Rebuild_And_Get_Sequence_Table = class(TZDB2_Th_CMD)
  private
    Table_Ptr: PZDB2_BlockHandle;
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Queue; var Table_: TZDB2_BlockHandle);
  end;

  TZDB2_Th_CMD_Get_And_Clean_Sequence_Table = class(TZDB2_Th_CMD)
  private
    Table_Ptr: PZDB2_BlockHandle;
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Queue; var Table_: TZDB2_BlockHandle);
  end;

  TZDB2_Th_CMD_Flush_Sequence_Table = class(TZDB2_Th_CMD)
  private
    Table_Ptr: PZDB2_BlockHandle;
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    AutoFree_Data: Boolean;
    constructor Create(const ThEng_: TZDB2_Th_Queue; const Table_: PZDB2_BlockHandle);
  end;

  TZDB2_Th_CMD_Extract_To = class(TZDB2_Th_CMD)
  private
    Input_Ptr: PZDB2_BlockHandle;
    Dest_Th_Engine: TZDB2_Th_Queue;
    Output_Ptr: PZDB2_Th_CMD_ID_And_State_Array;
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    Max_Queue: Integer;
    Wait_Queue: Boolean;
    AutoFree_Data: Boolean;
    constructor Create(const ThEng_: TZDB2_Th_Queue;
      const Input_: PZDB2_BlockHandle;
      const Dest_Th_Engine_: TZDB2_Th_Queue; const Output_: PZDB2_Th_CMD_ID_And_State_Array);
  end;

  TZDB2_Th_CMD_Format_Custom_Space = class(TZDB2_Th_CMD)
  private
    Param_Space: Int64;
    Param_Block: Word;
    Param_OnProgress: TZDB2_OnProgress;
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Queue; const Space: Int64; const Block: Word; const OnProgress: TZDB2_OnProgress);
  end;

  TZDB2_Th_CMD_Append_Custom_Space = class(TZDB2_Th_CMD)
  private
    Param_Space: Int64;
    Param_Block: Word;
    Param_OnProgress: TZDB2_OnProgress;
    procedure DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State); override;
  public
    constructor Create(const ThEng_: TZDB2_Th_Queue; const Space: Int64; const Block: Word; const OnProgress: TZDB2_OnProgress);
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
    OnResult_C: TOn_Mem64_And_State_Event_C;
    OnResult_M: TOn_Mem64_And_State_Event_M;
    OnResult_P: TOn_Mem64_And_State_Event_P;
    procedure CMD_Done;
  public
    CMD: TZDB2_Th_CMD;
    Mem64_And_State: TZDB2_Th_CMD_Mem64_And_State;
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
    OnResult_C: TOn_Stream_And_State_Event_C;
    OnResult_M: TOn_Stream_And_State_Event_M;
    OnResult_P: TOn_Stream_And_State_Event_P;
    procedure CMD_Done;
  public
    CMD: TZDB2_Th_CMD;
    Stream_And_State: TZDB2_Th_CMD_Stream_And_State;
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
    OnResult_C: TOn_ID_And_State_Event_C;
    OnResult_M: TOn_ID_And_State_Event_M;
    OnResult_P: TOn_ID_And_State_Event_P;
    procedure CMD_Done;
  public
    CMD: TZDB2_Th_CMD;
    ID_And_State: TZDB2_Th_CMD_ID_And_State;
    constructor Create;
    procedure Init(CMD_: TZDB2_Th_CMD);
    procedure Ready;
  end;

  TOn_State_Event_C = procedure(var Sender: TCMD_State);
  TOn_State_Event_M = procedure(var Sender: TCMD_State) of object;
{$IFDEF FPC}
  TOn_State_Event_P = procedure(var Sender: TCMD_State) is nested;
{$ELSE FPC}
  TOn_State_Event_P = reference to procedure(var Sender: TCMD_State);
{$ENDIF FPC}

  TZDB2_Th_CMD_Bridge_State = class
  private
    OnResult_C: TOn_State_Event_C;
    OnResult_M: TOn_State_Event_M;
    OnResult_P: TOn_State_Event_P;
    procedure CMD_Done;
  public
    CMD: TZDB2_Th_CMD;
    State: TCMD_State;
    constructor Create;
    procedure Init(CMD_: TZDB2_Th_CMD);
    procedure Ready;
  end;

  TZDB2_Th_CMD_Queue = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalOrderStruct<TZDB2_Th_CMD>;
{$ENDREGION 'Command_Queue'}
{$REGION 'Command_Dispatch'}

  TZDB2_Th_Queue = class
  private
    FCMD_Queue: TZDB2_Th_CMD_Queue;
    FCMD_Execute_Thread_Is_Runing, FCMD_Execute_Thread_Is_Exit: Boolean;
    FCoreSpace_Auto_Append_Space: Boolean;
    FCoreSpace_Mode: TZDB2_SpaceMode;
    FCoreSpace_Delta: Int64;
    CoreSpace_BlockSize: Word;
    FCoreSpace_Cipher: IZDB2_Cipher;
    FCoreSpace_IOHnd: TIOHnd;
    CoreSpace__: TZDB2_Core_Space;
    procedure ZDB2_ThRun_Proc(ThSender: TCompute);
    procedure Do_Free_CMD(var p: TZDB2_Th_CMD);
    procedure DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
  public
    class function CheckStream(Stream_: TCore_Stream; Cipher_: IZDB2_Cipher): Boolean;
    constructor Create(Mode_: TZDB2_SpaceMode;
      Stream_: TCore_Stream; AutoFree_, OnlyRead_: Boolean; Delta_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
    destructor Destroy; override;

    // internal thread instance. Be careful and practical to avoid assignment
    property CMD_Queue: TZDB2_Th_CMD_Queue read FCMD_Queue;
    property CoreSpace: TZDB2_Core_Space read CoreSpace__;
    function CoreSpace_IOHnd: PIOHnd;

    // auto space
    property Auto_Append_Space: Boolean read FCoreSpace_Auto_Append_Space write FCoreSpace_Auto_Append_Space; // default is true

    // queue state
    function QueueNum: NativeInt;
    function CoreSpace_Size: Int64;
    function CoreSpace_Physics_Size: Int64;
    function CoreSpace_Free_Space_Size: Int64;
    function CoreSpace_BlockCount: Integer;
    function IsOnlyRead: Boolean;
    function Is_Memory_Data: Boolean;
    function Get_Database_FileName: U_String;
    procedure Wait_Queue;

    // sync
    function Sync_GetData(Mem64: TMem64; ID: Integer): Boolean; overload;
    function Sync_SetData(Mem64: TMem64; var ID: Integer): Boolean; overload;
    function Sync_Append(Mem64: TMem64; var ID: Integer): Boolean; overload;
    function Sync_GetData(Stream: TCore_Stream; ID: Integer): Boolean; overload;
    function Sync_SetData(Stream: TCore_Stream; var ID: Integer): Boolean; overload;
    function Sync_Append(Stream: TCore_Stream; var ID: Integer): Boolean; overload;
    function Sync_Remove(ID: Integer): Boolean;
    function Sync_Flush(): Boolean;
    function Sync_Rebuild_And_Get_Sequence_Table(var Table_: TZDB2_BlockHandle): Boolean;
    function Sync_Get_And_Clean_Sequence_Table(var Table_: TZDB2_BlockHandle): Boolean;
    function Sync_Flush_Sequence_Table(var Table_: TZDB2_BlockHandle): Boolean; overload;
    function Sync_Flush_Sequence_Table(L: TZDB2_ID_List): Boolean; overload;
    function Sync_Flush_Sequence_Table(L: TZDB2_ID_Pool): Boolean; overload;
    function Sync_Extract_To(var Input_: TZDB2_BlockHandle; const Dest_Th_Engine_: TZDB2_Th_Queue; var Output_: TZDB2_Th_CMD_ID_And_State_Array): Boolean;
    function Sync_Extract_To_Stream(var Input_: TZDB2_BlockHandle; const Dest: TCore_Stream; const Cipher_: IZDB2_Cipher): Integer;
    function Sync_Extract_To_File(var Input_: TZDB2_BlockHandle; const Dest: U_String; const Cipher_: IZDB2_Cipher): Integer;
    function Sync_Format_Custom_Space(const Space_: Int64; const Block_: Word; const OnProgress_: TZDB2_OnProgress): Boolean;
    function Sync_Append_Custom_Space(const Space_: Int64; const Block_: Word; const OnProgress_: TZDB2_OnProgress): Boolean;

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
    procedure Async_Flush_Sequence_Table_C(const Table_: TZDB2_BlockHandle; OnResult: TOn_State_Event_C); overload;
    procedure Async_Flush_Sequence_Table_C(const L: TZDB2_ID_List; OnResult: TOn_State_Event_C); overload;
    procedure Async_Flush_Sequence_Table_M(const Table_: TZDB2_BlockHandle; OnResult: TOn_State_Event_M); overload;
    procedure Async_Flush_Sequence_Table_M(const L: TZDB2_ID_List; OnResult: TOn_State_Event_M); overload;
    procedure Async_Flush_Sequence_Table_P(const Table_: TZDB2_BlockHandle; OnResult: TOn_State_Event_P); overload;
    procedure Async_Flush_Sequence_Table_P(const L: TZDB2_ID_List; OnResult: TOn_State_Event_P); overload;

    // misc
    class function Get_Handle(var buff: TZDB2_Th_CMD_ID_And_State_Array): TZDB2_BlockHandle;

    // test
    class procedure Test;
  end;
{$ENDREGION 'Command_Dispatch'}

implementation

constructor TZDB2_Th_CMD.Create(const ThEng_: TZDB2_Th_Queue);
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
  Engine.FCMD_Queue.Push(self);
end;

procedure TZDB2_Th_CMD.Execute;
begin
  try
    DoExecute(Engine.CoreSpace__, State_Ptr);
    if State_Ptr^ = TCMD_State.csDefault then
        State_Ptr^ := TCMD_State.csDone;
  except
      State_Ptr^ := TCMD_State.csError;
  end;

  if State_Ptr^ = TCMD_State.csError then
      DoStatus('%s error.', [ClassName]);

  try
    if Assigned(OnDone) then
        OnDone();
  except
  end;
end;

procedure TZDB2_Th_CMD_GetDataAsMem64.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
begin
  if not CoreSpace__.ReadData(Param_M64, Param_ID) then
      State^ := TCMD_State.csError;
end;

constructor TZDB2_Th_CMD_GetDataAsMem64.Create(const ThEng_: TZDB2_Th_Queue; const Mem64: TMem64; const ID: Integer);
begin
  inherited Create(ThEng_);
  Param_ID := ID;
  Param_M64 := Mem64;
  Init();
end;

procedure TZDB2_Th_CMD_GetDataAsStream.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
begin
  if not CoreSpace__.ReadStream(Param_Stream, Param_ID) then
      State^ := TCMD_State.csError;
end;

constructor TZDB2_Th_CMD_GetDataAsStream.Create(const ThEng_: TZDB2_Th_Queue; const Stream: TCore_Stream; const ID: Integer);
begin
  inherited Create(ThEng_);
  Param_ID := ID;
  Param_Stream := Stream;
  Init();
end;

procedure TZDB2_Th_CMD_SetDataFromMem64.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
var
  old_ID: Integer;
begin
  if Engine.FCoreSpace_IOHnd.IsOnlyRead then
      State^ := TCMD_State.csError
  else
    begin
      old_ID := Param_ID_Ptr^;
      if not CoreSpace__.WriteData(Param_M64, Param_ID_Ptr^, not AutoFree_Data) then
          State^ := TCMD_State.csError
      else if not CoreSpace__.RemoveData(old_ID, False) then
          State^ := TCMD_State.csError;
    end;
  if AutoFree_Data then
      disposeObject(Param_M64);
end;

constructor TZDB2_Th_CMD_SetDataFromMem64.Create(const ThEng_: TZDB2_Th_Queue; const Mem64: TMem64; var ID: Integer);
begin
  inherited Create(ThEng_);
  Param_ID_Ptr := @ID;
  Param_M64 := Mem64;
  AutoFree_Data := False;
  Init();
end;

procedure TZDB2_Th_CMD_SetDataFromStream.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
var
  old_ID: Integer;
begin
  if Engine.FCoreSpace_IOHnd.IsOnlyRead then
      State^ := TCMD_State.csError
  else
    begin
      old_ID := Param_ID_Ptr^;
      if not CoreSpace__.WriteStream(Param_Stream, Param_ID_Ptr^) then
          State^ := TCMD_State.csError
      else if not CoreSpace__.RemoveData(old_ID, False) then
          State^ := TCMD_State.csError;
    end;
  if AutoFree_Data then
      disposeObject(Param_Stream);
end;

constructor TZDB2_Th_CMD_SetDataFromStream.Create(const ThEng_: TZDB2_Th_Queue; const Stream: TCore_Stream; var ID: Integer);
begin
  inherited Create(ThEng_);
  Param_ID_Ptr := @ID;
  Param_Stream := Stream;
  AutoFree_Data := False;
  Init();
end;

procedure TZDB2_Th_CMD_AppendFromMem64.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
begin
  if Engine.FCoreSpace_IOHnd.IsOnlyRead then
      State^ := TCMD_State.csError
  else if not CoreSpace__.WriteData(Param_M64, Param_ID_Ptr^, not AutoFree_Data) then
      State^ := TCMD_State.csError;
  if AutoFree_Data then
      disposeObject(Param_M64);
end;

constructor TZDB2_Th_CMD_AppendFromMem64.Create(const ThEng_: TZDB2_Th_Queue; const Mem64: TMem64; var ID: Integer);
begin
  inherited Create(ThEng_);
  ID := -1;
  Param_ID_Ptr := @ID;
  Param_M64 := Mem64;
  AutoFree_Data := False;
  Init();
end;

procedure TZDB2_Th_CMD_AppendFromStream.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
begin
  if Engine.FCoreSpace_IOHnd.IsOnlyRead then
      State^ := TCMD_State.csError
  else if not CoreSpace__.WriteStream(Param_Stream, Param_ID_Ptr^) then
      State^ := TCMD_State.csError;
  if AutoFree_Data then
      disposeObject(Param_Stream);
end;

constructor TZDB2_Th_CMD_AppendFromStream.Create(const ThEng_: TZDB2_Th_Queue; const Stream: TCore_Stream; var ID: Integer);
begin
  inherited Create(ThEng_);
  ID := -1;
  Param_ID_Ptr := @ID;
  Param_Stream := Stream;
  AutoFree_Data := False;
  Init();
end;

procedure TZDB2_Th_CMD_Remove.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
begin
  if Engine.FCoreSpace_IOHnd.IsOnlyRead then
      State^ := TCMD_State.csError
  else if not CoreSpace__.RemoveData(Param_ID, False) then
      State^ := TCMD_State.csError;
end;

constructor TZDB2_Th_CMD_Remove.Create(const ThEng_: TZDB2_Th_Queue; const ID: Integer);
begin
  inherited Create(ThEng_);
  Param_ID := ID;
  Init();
end;

procedure TZDB2_Th_CMD_Exit.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
begin
  Engine.FCMD_Execute_Thread_Is_Runing := False;
end;

constructor TZDB2_Th_CMD_Exit.Create(const ThEng_: TZDB2_Th_Queue);
begin
  inherited Create(ThEng_);
  Init();
end;

procedure TZDB2_Th_CMD_Flush.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
begin
  CoreSpace__.Save;
end;

constructor TZDB2_Th_CMD_Flush.Create(const ThEng_: TZDB2_Th_Queue);
begin
  inherited Create(ThEng_);
  Init();
end;

procedure TZDB2_Th_CMD_Rebuild_And_Get_Sequence_Table.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
var
  R_: TCMD_State;
begin
  R_ := TCMD_State.csDone;

  if not Engine.FCoreSpace_IOHnd.IsOnlyRead then
    begin
      // remove identifier
      if (PSequence_Table_Head(@CoreSpace__.UserCustomHeader^[0])^.Identifier = $FFFF) and
        CoreSpace__.Check(PSequence_Table_Head(@CoreSpace__.UserCustomHeader^[0])^.ID) then
        begin
          if not CoreSpace__.RemoveData(PSequence_Table_Head(@CoreSpace__.UserCustomHeader^[0])^.ID, False) then
              R_ := TCMD_State.csError;
          FillPtr(@CoreSpace__.UserCustomHeader^[0], SizeOf(TSequence_Table_Head), 0);
        end;
    end;

  // rebuild identifier
  Table_Ptr^ := CoreSpace__.BuildTableID;
  State^ := R_;
end;

constructor TZDB2_Th_CMD_Rebuild_And_Get_Sequence_Table.Create(const ThEng_: TZDB2_Th_Queue; var Table_: TZDB2_BlockHandle);
begin
  inherited Create(ThEng_);
  Table_Ptr := @Table_;
  Init();
end;

procedure TZDB2_Th_CMD_Get_And_Clean_Sequence_Table.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
var
  R_: TCMD_State;
  Mem64: TMem64;
begin
  R_ := TCMD_State.csDone;
  if (PSequence_Table_Head(@CoreSpace__.UserCustomHeader^[0])^.Identifier = $FFFF) and
    CoreSpace__.Check(PSequence_Table_Head(@CoreSpace__.UserCustomHeader^[0])^.ID) then
    begin
      // read identifier
      Mem64 := TMem64.Create;
      if CoreSpace__.ReadData(Mem64, PSequence_Table_Head(@CoreSpace__.UserCustomHeader^[0])^.ID) then
        begin
          SetLength(Table_Ptr^, Mem64.Size shr 2);
          if length(Table_Ptr^) > 0 then
              CopyPtr(Mem64.Memory, @Table_Ptr^[0], length(Table_Ptr^) shl 2);
          disposeObject(Mem64);
        end
      else
          R_ := TCMD_State.csError;
      if not Engine.FCoreSpace_IOHnd.IsOnlyRead then
        begin
          // remove identifier
          if not CoreSpace__.RemoveData(PSequence_Table_Head(@CoreSpace__.UserCustomHeader^[0])^.ID, False) then
              R_ := TCMD_State.csError;
          FillPtr(@CoreSpace__.UserCustomHeader^[0], SizeOf(TSequence_Table_Head), 0);
        end;
    end
  else
      Table_Ptr^ := CoreSpace__.BuildTableID;
  State^ := R_;
end;

constructor TZDB2_Th_CMD_Get_And_Clean_Sequence_Table.Create(const ThEng_: TZDB2_Th_Queue; var Table_: TZDB2_BlockHandle);
begin
  inherited Create(ThEng_);
  Table_Ptr := @Table_;
  Init();
end;

procedure TZDB2_Th_CMD_Flush_Sequence_Table.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
var
  R_: TCMD_State;
  Mem64: TMem64;
  i, j: Integer;
begin
  if Engine.FCoreSpace_IOHnd.IsOnlyRead then
    begin
      State^ := TCMD_State.csError;
    end
  else
    begin
      R_ := TCMD_State.csDone;

      if (PSequence_Table_Head(@CoreSpace__.UserCustomHeader^[0])^.Identifier = $FFFF) and
        CoreSpace__.Check(PSequence_Table_Head(@CoreSpace__.UserCustomHeader^[0])^.ID) then
        begin
          // remove identifier
          if not CoreSpace__.RemoveData(PSequence_Table_Head(@CoreSpace__.UserCustomHeader^[0])^.ID, False) then
              R_ := TCMD_State.csError;
          FillPtr(@CoreSpace__.UserCustomHeader^[0], SizeOf(TSequence_Table_Head), 0);
        end;

      if length(Table_Ptr^) > 0 then
        begin
          // save identifier
          Mem64 := TMem64.Create;
          Mem64.Mapping(@Table_Ptr^[0], length(Table_Ptr^) shl 2);
          PSequence_Table_Head(@CoreSpace__.UserCustomHeader^[0])^.Identifier := $FFFF;
          if not CoreSpace__.WriteData(Mem64, PSequence_Table_Head(@CoreSpace__.UserCustomHeader^[0])^.ID, True) then
              R_ := TCMD_State.csError;
          disposeObject(Mem64);
        end
      else
        begin
          FillPtr(@CoreSpace__.UserCustomHeader^[0], SizeOf(TSequence_Table_Head), 0);
        end;
      State^ := R_;
    end;
  if AutoFree_Data then
    begin
      SetLength(Table_Ptr^, 0);
      Dispose(Table_Ptr);
    end;
end;

constructor TZDB2_Th_CMD_Flush_Sequence_Table.Create(const ThEng_: TZDB2_Th_Queue; const Table_: PZDB2_BlockHandle);
begin
  inherited Create(ThEng_);
  Table_Ptr := Table_;
  AutoFree_Data := False;
  Init();
end;

procedure TZDB2_Th_CMD_Extract_To.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
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
          if CoreSpace__.ReadData(Mem64, Input_Ptr^[i]) then
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
    end;
  State^ := R_;
  if AutoFree_Data then
    begin
      SetLength(Input_Ptr^, 0);
      Dispose(Input_Ptr);
    end;
end;

constructor TZDB2_Th_CMD_Extract_To.Create(const ThEng_: TZDB2_Th_Queue;
  const Input_: PZDB2_BlockHandle;
  const Dest_Th_Engine_: TZDB2_Th_Queue; const Output_: PZDB2_Th_CMD_ID_And_State_Array);
begin
  inherited Create(ThEng_);
  Input_Ptr := Input_;
  Dest_Th_Engine := Dest_Th_Engine_;
  Output_Ptr := Output_;
  Max_Queue := 100;
  Wait_Queue := True;
  AutoFree_Data := False;
  Init();
end;

procedure TZDB2_Th_CMD_Format_Custom_Space.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
var
  backup_: TZDB2_OnProgress;
begin
  backup_ := CoreSpace__.OnProgress;
  CoreSpace__.OnProgress := Param_OnProgress;
  try
    if not CoreSpace__.BuildSpace(Param_Space, Param_Block) then
        State^ := TCMD_State.csError;
  finally
      CoreSpace__.OnProgress := backup_;
  end;
end;

constructor TZDB2_Th_CMD_Format_Custom_Space.Create(const ThEng_: TZDB2_Th_Queue; const Space: Int64; const Block: Word; const OnProgress: TZDB2_OnProgress);
begin
  inherited Create(ThEng_);
  Param_Space := Space;
  Param_Block := Block;
  Param_OnProgress := OnProgress;
  Init();
end;

procedure TZDB2_Th_CMD_Append_Custom_Space.DoExecute(CoreSpace__: TZDB2_Core_Space; State: PCMD_State);
var
  backup_: TZDB2_OnProgress;
begin
  backup_ := CoreSpace__.OnProgress;
  CoreSpace__.OnProgress := Param_OnProgress;
  try
    if not CoreSpace__.AppendSpace(Param_Space, Param_Block) then
        State^ := TCMD_State.csError;
  finally
      CoreSpace__.OnProgress := backup_;
  end;
end;

constructor TZDB2_Th_CMD_Append_Custom_Space.Create(const ThEng_: TZDB2_Th_Queue; const Space: Int64; const Block: Word; const OnProgress: TZDB2_OnProgress);
begin
  inherited Create(ThEng_);
  Param_Space := Space;
  Param_Block := Block;
  Param_OnProgress := OnProgress;
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
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
  CMD := nil;
  Mem64_And_State.Mem64 := nil;
  Mem64_And_State.State := TCMD_State.csDefault;
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
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
  CMD := nil;
  Stream_And_State.Stream := nil;
  Stream_And_State.State := TCMD_State.csDefault;
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
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
  CMD := nil;
  ID_And_State.ID := -1;
  ID_And_State.State := TCMD_State.csDefault;
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

procedure TZDB2_Th_CMD_Bridge_State.CMD_Done;
begin
  if Assigned(OnResult_C) then
      OnResult_C(State);
  if Assigned(OnResult_M) then
      OnResult_M(State);
  if Assigned(OnResult_P) then
      OnResult_P(State);
  Free;
end;

constructor TZDB2_Th_CMD_Bridge_State.Create;
begin
  inherited Create;
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
  CMD := nil;
  State := TCMD_State.csDefault;
end;

procedure TZDB2_Th_CMD_Bridge_State.Init(CMD_: TZDB2_Th_CMD);
begin
  CMD := CMD_;
  CMD.OnDone := {$IFDEF FPC}@{$ENDIF FPC}CMD_Done;
end;

procedure TZDB2_Th_CMD_Bridge_State.Ready;
begin
  CMD.Ready(State);
end;

procedure TZDB2_Th_Queue.ZDB2_ThRun_Proc(ThSender: TCompute);
var
  LTK, tmp: TTimeTick;
  CMD_: TZDB2_Th_CMD;
begin
  CoreSpace__ := TZDB2_Core_Space.Create(@FCoreSpace_IOHnd);
  CoreSpace__.Cipher := FCoreSpace_Cipher;
  CoreSpace__.Mode := FCoreSpace_Mode;
  CoreSpace__.AutoCloseIOHnd := True;
  CoreSpace__.OnNoSpace := {$IFDEF FPC}@{$ENDIF FPC}DoNoSpace;
  if umlFileSize(FCoreSpace_IOHnd) > 0 then
    if not CoreSpace__.Open then
      begin
        try
            CoreSpace__.Free;
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
      if FCMD_Queue.Num > 0 then
        begin
          CMD_ := FCMD_Queue.First^.Data;
          CMD_.Execute();
          FCMD_Queue.Next();
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
      CoreSpace__.Free;
  except
  end;

  FCMD_Execute_Thread_Is_Runing := False;
  FCMD_Execute_Thread_Is_Exit := True;
end;

procedure TZDB2_Th_Queue.Do_Free_CMD(var p: TZDB2_Th_CMD);
begin
  disposeObject(p);
end;

procedure TZDB2_Th_Queue.DoNoSpace(Trigger: TZDB2_Core_Space; Siz_: Int64; var retry: Boolean);
begin
  if FCoreSpace_Auto_Append_Space then
      retry := Trigger.AppendSpace(FCoreSpace_Delta, CoreSpace_BlockSize)
  else
      retry := False;
end;

class function TZDB2_Th_Queue.CheckStream(Stream_: TCore_Stream; Cipher_: IZDB2_Cipher): Boolean;
begin
  Result := TZDB2_Core_Space.CheckStream(Stream_, Cipher_);
end;

constructor TZDB2_Th_Queue.Create(Mode_: TZDB2_SpaceMode;
  Stream_: TCore_Stream; AutoFree_, OnlyRead_: Boolean; Delta_: Int64; BlockSize_: Word; Cipher_: IZDB2_Cipher);
begin
  inherited Create;
  FCMD_Queue := TZDB2_Th_CMD_Queue.Create;
  FCMD_Queue.OnFree := {$IFDEF FPC}@{$ENDIF FPC}Do_Free_CMD;
  FCMD_Execute_Thread_Is_Runing := False;
  FCMD_Execute_Thread_Is_Exit := False;
  FCoreSpace_Auto_Append_Space := True;
  FCoreSpace_Mode := Mode_;
  FCoreSpace_Delta := Delta_;
  CoreSpace_BlockSize := BlockSize_;
  FCoreSpace_Cipher := Cipher_;
  InitIOHnd(FCoreSpace_IOHnd);
  umlFileCreateAsStream(Stream_, FCoreSpace_IOHnd, OnlyRead_);
  FCoreSpace_IOHnd.AutoFree := AutoFree_;
  CoreSpace__ := nil;

  // test
  FCoreSpace_IOHnd.Cache.UsedWriteCache := True;
  FCoreSpace_IOHnd.Cache.UsedReadCache := True;

  // thread
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}ZDB2_ThRun_Proc);
  while not FCMD_Execute_Thread_Is_Runing do
      TCompute.Sleep(1);
end;

destructor TZDB2_Th_Queue.Destroy;
var
  tmp: TCMD_State;
begin
  Async_Flush;
  TZDB2_Th_CMD_Exit.Create(self).Ready(tmp);
  while not FCMD_Execute_Thread_Is_Exit do
      TCompute.Sleep(1);
  FCMD_Queue.Free;
  inherited Destroy;
end;

function TZDB2_Th_Queue.CoreSpace_IOHnd: PIOHnd;
begin
  Result := @FCoreSpace_IOHnd;
end;

function TZDB2_Th_Queue.QueueNum: NativeInt;
begin
  if FCMD_Queue <> nil then
      Result := FCMD_Queue.Num
  else
      Result := 0;
end;

function TZDB2_Th_Queue.CoreSpace_Size: Int64;
begin
  FCMD_Queue.Critical__.Lock;
  if CoreSpace__ <> nil then
      Result := CoreSpace__.State^.Physics - CoreSpace__.State^.FreeSpace
  else
      Result := 0;
  FCMD_Queue.Critical__.UnLock;
end;

function TZDB2_Th_Queue.CoreSpace_Physics_Size: Int64;
begin
  FCMD_Queue.Critical__.Lock;
  if CoreSpace__ <> nil then
      Result := CoreSpace__.State^.Physics
  else
      Result := 0;
  FCMD_Queue.Critical__.UnLock;
end;

function TZDB2_Th_Queue.CoreSpace_Free_Space_Size: Int64;
begin
  FCMD_Queue.Critical__.Lock;
  if CoreSpace__ <> nil then
      Result := CoreSpace__.State^.FreeSpace
  else
      Result := 0;
  FCMD_Queue.Critical__.UnLock;
end;

function TZDB2_Th_Queue.CoreSpace_BlockCount: Integer;
begin
  FCMD_Queue.Critical__.Lock;
  Result := CoreSpace__.BlockCount;
  FCMD_Queue.Critical__.UnLock;
end;

function TZDB2_Th_Queue.IsOnlyRead: Boolean;
begin
  FCMD_Queue.Critical__.Lock;
  Result := FCoreSpace_IOHnd.IsOnlyRead;
  FCMD_Queue.Critical__.UnLock;
end;

function TZDB2_Th_Queue.Is_Memory_Data: Boolean;
begin
  FCMD_Queue.Critical__.Lock;
  Result := (FCoreSpace_IOHnd.Handle is TMS64) or (FCoreSpace_IOHnd.Handle is TCore_MemoryStream);
  FCMD_Queue.Critical__.UnLock;
end;

function TZDB2_Th_Queue.Get_Database_FileName: U_String;
begin
  Result := '';
  FCMD_Queue.Critical__.Lock;
  if FCoreSpace_IOHnd.Handle is TCore_FileStream then
      Result := TCore_FileStream(FCoreSpace_IOHnd.Handle).FileName
  else if FCoreSpace_IOHnd.Handle is TReliableFileStream then
      Result := TReliableFileStream(FCoreSpace_IOHnd.Handle).FileName;
  FCMD_Queue.Critical__.UnLock;
end;

procedure TZDB2_Th_Queue.Wait_Queue;
begin
  while QueueNum > 0 do
      TCompute.Sleep(1);
end;

function TZDB2_Th_Queue.Sync_GetData(Mem64: TMem64; ID: Integer): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_GetDataAsMem64.Create(self, Mem64, ID).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Queue.Sync_SetData(Mem64: TMem64; var ID: Integer): Boolean;
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

function TZDB2_Th_Queue.Sync_Append(Mem64: TMem64; var ID: Integer): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_AppendFromMem64.Create(self, Mem64, ID).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Queue.Sync_GetData(Stream: TCore_Stream; ID: Integer): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_GetDataAsStream.Create(self, Stream, ID).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Queue.Sync_SetData(Stream: TCore_Stream; var ID: Integer): Boolean;
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

function TZDB2_Th_Queue.Sync_Append(Stream: TCore_Stream; var ID: Integer): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_AppendFromStream.Create(self, Stream, ID).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Queue.Sync_Remove(ID: Integer): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_Remove.Create(self, ID).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Queue.Sync_Flush(): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_Flush.Create(self).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Queue.Sync_Rebuild_And_Get_Sequence_Table(var Table_: TZDB2_BlockHandle): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_Rebuild_And_Get_Sequence_Table.Create(self, Table_).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Queue.Sync_Get_And_Clean_Sequence_Table(var Table_: TZDB2_BlockHandle): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_Get_And_Clean_Sequence_Table.Create(self, Table_).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Queue.Sync_Flush_Sequence_Table(var Table_: TZDB2_BlockHandle): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_Flush_Sequence_Table.Create(self, @Table_).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Queue.Sync_Flush_Sequence_Table(L: TZDB2_ID_List): Boolean;
var
  Table_: TZDB2_BlockHandle;
begin
  Table_ := TZDB2_Core_Space.Get_Handle(L);
  Result := Sync_Flush_Sequence_Table(Table_);
  SetLength(Table_, 0);
end;

function TZDB2_Th_Queue.Sync_Flush_Sequence_Table(L: TZDB2_ID_Pool): Boolean;
var
  Table_: TZDB2_BlockHandle;
begin
  Table_ := TZDB2_Core_Space.Get_Handle(L);
  Result := Sync_Flush_Sequence_Table(Table_);
  SetLength(Table_, 0);
end;

function TZDB2_Th_Queue.Sync_Extract_To(var Input_: TZDB2_BlockHandle;
  const Dest_Th_Engine_: TZDB2_Th_Queue; var Output_: TZDB2_Th_CMD_ID_And_State_Array): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_Extract_To.Create(self, @Input_, Dest_Th_Engine_, @Output_).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Queue.Sync_Extract_To_Stream(var Input_: TZDB2_BlockHandle; const Dest: TCore_Stream; const Cipher_: IZDB2_Cipher): Integer;
var
  th: TZDB2_Th_Queue;
  Output_: TZDB2_Th_CMD_ID_And_State_Array;
  i: Integer;
  tmp: TZDB2_ID_Pool;
begin
  Result := 0;
  th := TZDB2_Th_Queue.Create(FCoreSpace_Mode, Dest, False, False, FCoreSpace_Delta, CoreSpace_BlockSize, Cipher_);
  if Sync_Extract_To(Input_, th, Output_) then
    begin
      tmp := TZDB2_ID_Pool.Create;
      for i := 0 to length(Output_) - 1 do
        begin
          if Output_[i].State = TCMD_State.csDone then
              tmp.Add(Output_[i].ID);
        end;
      SetLength(Output_, 0);
      Result := tmp.Num;
      th.Sync_Flush_Sequence_Table(tmp);
      disposeObject(tmp);
    end;
  disposeObject(th);
end;

function TZDB2_Th_Queue.Sync_Extract_To_File(var Input_: TZDB2_BlockHandle; const Dest: U_String; const Cipher_: IZDB2_Cipher): Integer;
var
  fs: TCore_FileStream;
  th: TZDB2_Th_Queue;
  Output_: TZDB2_Th_CMD_ID_And_State_Array;
  i: Integer;
  tmp: TZDB2_ID_Pool;
begin
  Result := 0;
  try
    fs := TCore_FileStream.Create(Dest, fmCreate);
    th := TZDB2_Th_Queue.Create(FCoreSpace_Mode, fs, True, False, FCoreSpace_Delta, CoreSpace_BlockSize, Cipher_);
    if Sync_Extract_To(Input_, th, Output_) then
      begin
        tmp := TZDB2_ID_Pool.Create;
        for i := 0 to length(Output_) - 1 do
          begin
            if Output_[i].State = TCMD_State.csDone then
                tmp.Add(Output_[i].ID);
          end;
        SetLength(Output_, 0);
        Result := tmp.Num;
        th.Sync_Flush_Sequence_Table(tmp);
        disposeObject(tmp);
      end;
    disposeObject(th);
  except
  end;
end;

function TZDB2_Th_Queue.Sync_Format_Custom_Space(const Space_: Int64; const Block_: Word; const OnProgress_: TZDB2_OnProgress): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_Format_Custom_Space.Create(self, Space_, Block_, OnProgress_).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

function TZDB2_Th_Queue.Sync_Append_Custom_Space(const Space_: Int64; const Block_: Word; const OnProgress_: TZDB2_OnProgress): Boolean;
var
  tmp: TCMD_State;
begin
  TZDB2_Th_CMD_Append_Custom_Space.Create(self, Space_, Block_, OnProgress_).Ready(tmp);
  while tmp = TCMD_State.csDefault do
      TCompute.Sleep(1);
  Result := tmp = TCMD_State.csDone;
end;

procedure TZDB2_Th_Queue.Async_GetData_AsMem64_C(ID: Integer; Mem64: TMem64; OnResult: TOn_Mem64_And_State_Event_C);
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

procedure TZDB2_Th_Queue.Async_GetData_AsMem64_M(ID: Integer; Mem64: TMem64; OnResult: TOn_Mem64_And_State_Event_M);
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

procedure TZDB2_Th_Queue.Async_GetData_AsMem64_P(ID: Integer; Mem64: TMem64; OnResult: TOn_Mem64_And_State_Event_P);
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

procedure TZDB2_Th_Queue.Async_GetData_AsStream_C(ID: Integer; Stream: TCore_Stream; OnResult: TOn_Stream_And_State_Event_C);
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

procedure TZDB2_Th_Queue.Async_GetData_AsStream_M(ID: Integer; Stream: TCore_Stream; OnResult: TOn_Stream_And_State_Event_M);
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

procedure TZDB2_Th_Queue.Async_GetData_AsStream_P(ID: Integer; Stream: TCore_Stream; OnResult: TOn_Stream_And_State_Event_P);
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

procedure TZDB2_Th_Queue.Async_SetData(Mem64: TMem64; AutoFree_Data: Boolean; ID: Integer);
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

procedure TZDB2_Th_Queue.Async_SetData_C(Mem64: TMem64; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_C);
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

procedure TZDB2_Th_Queue.Async_SetData_M(Mem64: TMem64; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_M);
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

procedure TZDB2_Th_Queue.Async_SetData_P(Mem64: TMem64; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_P);
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

procedure TZDB2_Th_Queue.Async_Append(Mem64: TMem64; AutoFree_Data: Boolean);
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

procedure TZDB2_Th_Queue.Async_Append_C(Mem64: TMem64; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_C);
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

procedure TZDB2_Th_Queue.Async_Append_M(Mem64: TMem64; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_M);
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

procedure TZDB2_Th_Queue.Async_Append_P(Mem64: TMem64; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_P);
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

procedure TZDB2_Th_Queue.Async_SetData(Stream: TCore_Stream; AutoFree_Data: Boolean; ID: Integer);
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

procedure TZDB2_Th_Queue.Async_SetData_C(Stream: TCore_Stream; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_C);
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

procedure TZDB2_Th_Queue.Async_SetData_M(Stream: TCore_Stream; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_M);
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

procedure TZDB2_Th_Queue.Async_SetData_P(Stream: TCore_Stream; AutoFree_Data: Boolean; ID: Integer; OnResult: TOn_ID_And_State_Event_P);
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

procedure TZDB2_Th_Queue.Async_Append(Stream: TCore_Stream; AutoFree_Data: Boolean);
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

procedure TZDB2_Th_Queue.Async_Append_C(Stream: TCore_Stream; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_C);
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

procedure TZDB2_Th_Queue.Async_Append_M(Stream: TCore_Stream; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_M);
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

procedure TZDB2_Th_Queue.Async_Append_P(Stream: TCore_Stream; AutoFree_Data: Boolean; OnResult: TOn_ID_And_State_Event_P);
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

procedure TZDB2_Th_Queue.Async_Remove(ID: Integer);
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

procedure TZDB2_Th_Queue.Async_Flush;
var
  tmp: TZDB2_Th_CMD_Bridge_ID_And_State;
begin
  tmp := TZDB2_Th_CMD_Bridge_ID_And_State.Create;
  tmp.Init(TZDB2_Th_CMD_Flush.Create(self));
  tmp.Ready;
end;

procedure TZDB2_Th_Queue.Async_Flush_Sequence_Table_C(const Table_: TZDB2_BlockHandle; OnResult: TOn_State_Event_C);
var
  Table_Ptr: PZDB2_BlockHandle;
  i: Integer;
  Inst_: TZDB2_Th_CMD_Flush_Sequence_Table;
  tmp: TZDB2_Th_CMD_Bridge_State;
begin
  New(Table_Ptr);
  SetLength(Table_Ptr^, length(Table_));
  for i := 0 to length(Table_) - 1 do
      Table_Ptr^[i] := Table_[i];
  Inst_ := TZDB2_Th_CMD_Flush_Sequence_Table.Create(self, Table_Ptr);;
  Inst_.AutoFree_Data := True;
  tmp := TZDB2_Th_CMD_Bridge_State.Create;
  tmp.Init(Inst_);
  tmp.OnResult_C := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Queue.Async_Flush_Sequence_Table_C(const L: TZDB2_ID_List; OnResult: TOn_State_Event_C);
begin
  Async_Flush_Sequence_Table_C(TZDB2_Core_Space.Get_Handle(L), OnResult);
end;

procedure TZDB2_Th_Queue.Async_Flush_Sequence_Table_M(const Table_: TZDB2_BlockHandle; OnResult: TOn_State_Event_M);
var
  Table_Ptr: PZDB2_BlockHandle;
  i: Integer;
  Inst_: TZDB2_Th_CMD_Flush_Sequence_Table;
  tmp: TZDB2_Th_CMD_Bridge_State;
begin
  New(Table_Ptr);
  SetLength(Table_Ptr^, length(Table_));
  for i := 0 to length(Table_) - 1 do
      Table_Ptr^[i] := Table_[i];
  Inst_ := TZDB2_Th_CMD_Flush_Sequence_Table.Create(self, Table_Ptr);;
  Inst_.AutoFree_Data := True;
  tmp := TZDB2_Th_CMD_Bridge_State.Create;
  tmp.Init(Inst_);
  tmp.OnResult_M := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Queue.Async_Flush_Sequence_Table_M(const L: TZDB2_ID_List; OnResult: TOn_State_Event_M);
begin
  Async_Flush_Sequence_Table_M(TZDB2_Core_Space.Get_Handle(L), OnResult);
end;

procedure TZDB2_Th_Queue.Async_Flush_Sequence_Table_P(const Table_: TZDB2_BlockHandle; OnResult: TOn_State_Event_P);
var
  Table_Ptr: PZDB2_BlockHandle;
  i: Integer;
  Inst_: TZDB2_Th_CMD_Flush_Sequence_Table;
  tmp: TZDB2_Th_CMD_Bridge_State;
begin
  New(Table_Ptr);
  SetLength(Table_Ptr^, length(Table_));
  for i := 0 to length(Table_) - 1 do
      Table_Ptr^[i] := Table_[i];
  Inst_ := TZDB2_Th_CMD_Flush_Sequence_Table.Create(self, Table_Ptr);;
  Inst_.AutoFree_Data := True;
  tmp := TZDB2_Th_CMD_Bridge_State.Create;
  tmp.Init(Inst_);
  tmp.OnResult_P := OnResult;
  tmp.Ready;
end;

procedure TZDB2_Th_Queue.Async_Flush_Sequence_Table_P(const L: TZDB2_ID_List; OnResult: TOn_State_Event_P);
begin
  Async_Flush_Sequence_Table_P(TZDB2_Core_Space.Get_Handle(L), OnResult);
end;

class function TZDB2_Th_Queue.Get_Handle(var buff: TZDB2_Th_CMD_ID_And_State_Array): TZDB2_BlockHandle;
var
  i: Integer;
begin
  SetLength(Result, length(buff));
  for i := low(buff) to high(buff) do
      Result[i] := buff[i].ID;
end;

class procedure TZDB2_Th_Queue.Test;
var
  tmp_inst1: TZDB2_Th_Queue;
  tmp_inst2: TZDB2_Th_Queue;
  tmp_inst3: TZDB2_Th_Queue;
  tmp_inst4: TZDB2_Th_Queue;
  Mem64: TMS64;
  arry: TZDB2_BlockHandle;
  Output_: TZDB2_Th_CMD_ID_And_State_Array;
  i: Integer;
  tmp_extract_stream: TMS64;
begin
  tmp_inst1 := TZDB2_Th_Queue.Create(smNormal, TMS64.CustomCreate(1024 * 1024), True, False, 1024 * 1024, 4096, nil);
  tmp_inst2 := TZDB2_Th_Queue.Create(smNormal, TMS64.CustomCreate(1024 * 1024), True, False, 1024 * 1024, 1000, nil);
  tmp_inst3 := TZDB2_Th_Queue.Create(smNormal, TMS64.CustomCreate(1024 * 1024), True, False, 1024 * 1024, 500, nil);

  Mem64 := TMS64.Create;
  Mem64.Size := 3992;
  tmp_extract_stream := TMS64.CustomCreate(1024 * 1024);

  SetLength(arry, 8);
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
      Mem64.Clear;
      tmp_inst2.Sync_GetData(Mem64, arry[i]);
      DoStatus(umlMD5ToStr(Mem64.ToMD5));
    end;

  DoStatus('');

  tmp_inst2.Sync_Extract_To(arry, tmp_inst3, Output_);
  arry := Get_Handle(Output_);
  tmp_inst3.Sync_Flush_Sequence_Table(arry);
  tmp_inst3.Sync_Extract_To_Stream(arry, tmp_extract_stream, nil);

  tmp_inst4 := TZDB2_Th_Queue.Create(smNormal, tmp_extract_stream, True, False, 1024 * 1024, 500, nil);
  tmp_inst4.Sync_Get_And_Clean_Sequence_Table(arry);

  for i := low(arry) to high(arry) do
    begin
      Mem64.Clear;
      tmp_inst4.Sync_GetData(Mem64, arry[i]);
      DoStatus(umlMD5ToStr(Mem64.ToMD5));
    end;

  Wait_DoStatus_Queue();

  Mem64.Free;

  tmp_inst1.Free;
  tmp_inst2.Free;
  tmp_inst3.Free;
  tmp_inst4.Free;
end;

end.
