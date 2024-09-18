(*
https://zpascal.net
https://github.com/PassByYou888/ZNet
https://github.com/PassByYou888/zRasterization
https://github.com/PassByYou888/ZSnappy
https://github.com/PassByYou888/Z-AI1.4
https://github.com/PassByYou888/InfiniteIoT
https://github.com/PassByYou888/zMonitor_3rd_Core
https://github.com/PassByYou888/tcmalloc4p
https://github.com/PassByYou888/jemalloc4p
https://github.com/PassByYou888/zCloud
https://github.com/PassByYou888/ZServer4D
https://github.com/PassByYou888/zShell
https://github.com/PassByYou888/ZDB2.0
https://github.com/PassByYou888/zGameWare
https://github.com/PassByYou888/CoreCipher
https://github.com/PassByYou888/zChinese
https://github.com/PassByYou888/zSound
https://github.com/PassByYou888/zExpression
https://github.com/PassByYou888/ZInstaller2.0
https://github.com/PassByYou888/zAI
https://github.com/PassByYou888/NetFileService
https://github.com/PassByYou888/zAnalysis
https://github.com/PassByYou888/PascalString
https://github.com/PassByYou888/zInstaller
https://github.com/PassByYou888/zTranslate
https://github.com/PassByYou888/zVision
https://github.com/PassByYou888/FFMPEG-Header
*)
{ ****************************************************************************** }
{ * ZDB 2.0 Thread-Model for HPC                                               * }
{ ****************************************************************************** }
unit Z.ZDB2.Thread;

{$DEFINE FPC_DELPHI_MODE}
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
  Z.FragmentBuffer, // solve for discontinuous space
  Z.Notify, Z.ZDB2.Thread.Queue;

type
  TZDB2_Th_Engine_Marshal = class;
  TZDB2_Th_Engine_Data = class;
  TZDB2_Th_Engine = class;
  TZDB2_Th_Engine_Static_Copy_Tech = class;
  TZDB2_Th_Engine_Dynamic_Copy_Tech = class;
  TZDB2_Th_Engine_Data_Instance_Pool = TBigList<TZDB2_Th_Engine_Data>;
  TZDB2_Th_Engine_Data_BigList___ = TCritical_BigList<TZDB2_Th_Engine_Data>;
  TZDB2_Th_Engine_Marshal_BigList___ = TCritical_BigList<TZDB2_Th_Engine_Data>;
  TZDB2_Th_Engine_Data_Instance_Recycle_Tool___ = TCritical_BigList<TZDB2_Th_Engine_Data>;
  TZDB2_Th_Engine_Data_Link_Recycle_Tool___ = TCritical_BigList<TZDB2_Th_Engine_Data>;
  TZDB2_Th_Engine_Static_Copy_Instance_Pool = TCritical_BigList<TZDB2_Th_Engine_Static_Copy_Tech>;
  TZDB2_Th_Engine_Dynamic_Copy_Instance_Pool = TCritical_BigList<TZDB2_Th_Engine_Dynamic_Copy_Tech>;
  TZDB2_Th_Engine_ID_Data_Pool = TCritical_Big_Hash_Pair_Pool<Integer, TZDB2_Th_Engine_Data>;

{$REGION 'Data_Engine'}

  TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge = class(TCore_Object_Intermediate)
  public
    Source: TZDB2_Th_Engine_Data;
    OnResult_C: TOn_Mem64_And_State_Event_C;
    OnResult_M: TOn_Mem64_And_State_Event_M;
    OnResult_P: TOn_Mem64_And_State_Event_P;
    State: PCMD_State;
    constructor Create;
    procedure Do_Result(var Sender: TZDB2_Th_CMD_Mem64_And_State);
  end;

  TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge = class(TCore_Object_Intermediate)
  public
    Source: TZDB2_Th_Engine_Data;
    OnResult_C: TOn_Stream_And_State_Event_C;
    OnResult_M: TOn_Stream_And_State_Event_M;
    OnResult_P: TOn_Stream_And_State_Event_P;
    State: PCMD_State;
    constructor Create;
    procedure Do_Result(var Sender: TZDB2_Th_CMD_Stream_And_State);
  end;

  TOn_ZDB2_Th_Engine_Load_Mem64_Data_Event_C = procedure(Sender: TZDB2_Th_Engine_Data; Mem64: TMem64; Successed: Boolean);
  TOn_ZDB2_Th_Engine_Load_Mem64_Data_Event_M = procedure(Sender: TZDB2_Th_Engine_Data; Mem64: TMem64; Successed: Boolean) of object;
{$IFDEF FPC}
  TOn_ZDB2_Th_Engine_Load_Mem64_Data_Event_P = procedure(Sender: TZDB2_Th_Engine_Data; Mem64: TMem64; Successed: Boolean) is nested;
{$ELSE FPC}
  TOn_ZDB2_Th_Engine_Load_Mem64_Data_Event_P = reference to procedure(Sender: TZDB2_Th_Engine_Data; Mem64: TMem64; Successed: Boolean);
{$ENDIF FPC}

  TZDB2_Th_Engine_Load_Mem64_Data_Event_Bridge = class(TCore_Object_Intermediate)
  private
    R_State: TCMD_State;
    procedure Do_Event;
  public
    Source: TZDB2_Th_Engine_Data;
    Mem64: TMem64;
    OnResult_C: TOn_ZDB2_Th_Engine_Load_Mem64_Data_Event_C;
    OnResult_M: TOn_ZDB2_Th_Engine_Load_Mem64_Data_Event_M;
    OnResult_P: TOn_ZDB2_Th_Engine_Load_Mem64_Data_Event_P;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Result(var Sender: TZDB2_Th_CMD_Mem64_And_State);
  end;

  TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_C = procedure(Sender: TZDB2_Th_Engine_Data; Stream: TMS64; Successed: Boolean);
  TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_M = procedure(Sender: TZDB2_Th_Engine_Data; Stream: TMS64; Successed: Boolean) of object;
{$IFDEF FPC}
  TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_P = procedure(Sender: TZDB2_Th_Engine_Data; Stream: TMS64; Successed: Boolean) is nested;
{$ELSE FPC}
  TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_P = reference to procedure(Sender: TZDB2_Th_Engine_Data; Stream: TMS64; Successed: Boolean);
{$ENDIF FPC}

  TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge = class(TCore_Object_Intermediate)
  private
    R_State: TCMD_State;
    procedure Do_Event;
  public
    Source: TZDB2_Th_Engine_Data;
    Stream: TMS64;
    OnResult_C: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_C;
    OnResult_M: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_M;
    OnResult_P: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_P;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Result(var Sender: TZDB2_Th_CMD_Stream_And_State);
  end;

  TOn_ZDB2_Th_Engine_Save_Data_Event_C = procedure(Sender: TZDB2_Th_Engine_Data; Successed: Boolean);
  TOn_ZDB2_Th_Engine_Save_Data_Event_M = procedure(Sender: TZDB2_Th_Engine_Data; Successed: Boolean) of object;
{$IFDEF FPC}
  TOn_ZDB2_Th_Engine_Save_Data_Event_P = procedure(Sender: TZDB2_Th_Engine_Data; Successed: Boolean) is nested;
{$ELSE FPC}
  TOn_ZDB2_Th_Engine_Save_Data_Event_P = reference to procedure(Sender: TZDB2_Th_Engine_Data; Successed: Boolean);
{$ENDIF FPC}

  TZDB2_Th_Engine_Save_Data_Event_Bridge = class(TCore_Object_Intermediate)
  public
    Source: TZDB2_Th_Engine_Data;
    OnResult_C: TOn_ZDB2_Th_Engine_Save_Data_Event_C;
    OnResult_M: TOn_ZDB2_Th_Engine_Save_Data_Event_M;
    OnResult_P: TOn_ZDB2_Th_Engine_Save_Data_Event_P;
    constructor Create;
    procedure Do_Result(var Sender: TZDB2_Th_CMD_ID_And_State);
  end;

  TZDB2_Th_Engine_Data = class(TCore_Object_Intermediate)
  private
    FOwner: TZDB2_Th_Engine_Marshal;                                   // marshal
    FOwner_Data_Ptr: TZDB2_Th_Engine_Marshal_BigList___.PQueueStruct;  // marshal data ptr
    FTh_Engine: TZDB2_Th_Engine;                                       // engine
    FTh_Engine_Data_Ptr: TZDB2_Th_Engine_Data_BigList___.PQueueStruct; // engine data ptr
    FID: Integer;                                                      // Th_Engine data ID
    FSize: Int64;                                                      // data size

    // temp data swap technology
    // When the data is in a long loop, it is not appended to the data structure, but stored in the underlying ZDB2 database and Temp_Swap_Pool.
    // after the long loop ends, the data will truly become a engine structure
    FIs_Temp_Swap_Pool: Boolean;

    // In a multithreaded instance, data will be busy-loaded by multiple threads, First_Operation_Ready indicates that the data is ready
    // If added to the data, it will be false and true after completion
    FFirst_Operation_Ready: Boolean; // instance and data is first operation

    FInstance_Busy: Integer; // instance user support
    // if a user program or system malfunction prevents unlocking FInstance_Busy, engine will reset based on time
    FInstance_Last_Busy_Time: TTimeTick; // user bug or system
    FLocked: Boolean;                    // lock
    FSaveFailed_Do_Remove: Boolean;      // free instance and remove data on save failure
    FAsync_Load_Num: Integer;            // async Load number
    FAsync_Save_Num: Integer;            // async save number
    FPost_Free_Runing: Boolean;          // free check
    FLoad_Data_Error: Boolean;           // last load state
    procedure Wait_Unlock(timeOut: TTimeTick); overload;
  public
    property Owner: TZDB2_Th_Engine_Marshal read FOwner;                                                // marshal
    property Owner_Data_Ptr: TZDB2_Th_Engine_Marshal_BigList___.PQueueStruct read FOwner_Data_Ptr;      // marshal data ptr
    property Th_Engine: TZDB2_Th_Engine read FTh_Engine;                                                // engine
    property Th_Engine_Data_Ptr: TZDB2_Th_Engine_Data_BigList___.PQueueStruct read FTh_Engine_Data_Ptr; // engine data ptr

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

    // size state
    property DataSize: Int64 read FSize; // data size

    // In a multithreaded instance, data will be loaded by multiple threads, First_Operation_Ready indicates that the data is ready
    property First_Operation_Ready: Boolean read FFirst_Operation_Ready; // instance and data is first operation
    procedure Do_Ready(); virtual;

    // free instance and remove data on save failure, default is true
    property SaveFailed_Do_Remove: Boolean read FSaveFailed_Do_Remove write FSaveFailed_Do_Remove;

    // misc
    function IsOnlyRead: Boolean;
    function Engine: TZDB2_Th_Queue; // queue-engine
    property ID: Integer read FID;
    procedure Update_Owner_ID_Pool(OLD_, New_: Integer); virtual;
    function Can_Load: Boolean;
    function Can_Progress: Boolean;
    function Can_Free: Boolean;

    // position
    procedure MoveToLast;
    procedure MoveToFirst;

    // async delete and delay free, file is only do remove memory
    procedure Do_Remove(); virtual; // external event support
    function Remove(Delete_Data_: Boolean): Boolean; overload;
    function Remove(): Boolean; overload;

    // sync load.
    function Load_Data(Source: TCore_Stream): Boolean; overload;
    function Load_Data(Source: TMem64): Boolean; overload;
    function Get_Position_Data(Source: TCore_Stream; Begin_Position, Read_Size: Int64): Boolean;
    // async load
    procedure Async_Load_Data(Source: TCore_Stream; State: PCMD_State); overload;
    procedure Async_Load_Data(Source: TMem64; State: PCMD_State); overload;
    procedure Async_Load_Data_C(Source: TCore_Stream; OnResult: TOn_Stream_And_State_Event_C); overload; // Queue-Engine Do OnReturn Event
    procedure Async_Load_Data_C(Source: TMem64; OnResult: TOn_Mem64_And_State_Event_C); overload;        // Queue-Engine Do OnReturn Event
    procedure Async_Load_Data_M(Source: TCore_Stream; OnResult: TOn_Stream_And_State_Event_M); overload; // Queue-Engine Do OnReturn Event
    procedure Async_Load_Data_M(Source: TMem64; OnResult: TOn_Mem64_And_State_Event_M); overload;        // Queue-Engine Do OnReturn Event
    procedure Async_Load_Data_P(Source: TCore_Stream; OnResult: TOn_Stream_And_State_Event_P); overload; // Queue-Engine Do OnReturn Event
    procedure Async_Load_Data_P(Source: TMem64; OnResult: TOn_Mem64_And_State_Event_P); overload;        // Queue-Engine Do OnReturn Event
    procedure Async_Load_Stream_C(OnResult: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_C);                // Post Thread Do OnReturn Event
    procedure Async_Load_Mem64_C(OnResult: TOn_ZDB2_Th_Engine_Load_Mem64_Data_Event_C);                  // Post Thread Do OnReturn Event
    procedure Async_Load_Stream_M(OnResult: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_M);                // Post Thread Do OnReturn Event
    procedure Async_Load_Mem64_M(OnResult: TOn_ZDB2_Th_Engine_Load_Mem64_Data_Event_M);                  // Post Thread Do OnReturn Event
    procedure Async_Load_Stream_P(OnResult: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_P);                // Post Thread Do OnReturn Event
    procedure Async_Load_Mem64_P(OnResult: TOn_ZDB2_Th_Engine_Load_Mem64_Data_Event_P);                  // Post Thread Do OnReturn Event
    // async get-position-stream
    procedure Async_Get_Position_Data(Begin_Position, Read_Size: Int64; Source: TCore_Stream; State: PCMD_State);
    procedure Async_Get_Position_Data_C(Begin_Position, Read_Size: Int64; Source: TCore_Stream; OnResult: TOn_Stream_And_State_Event_C); overload;
    procedure Async_Get_Position_Data_M(Begin_Position, Read_Size: Int64; Source: TCore_Stream; OnResult: TOn_Stream_And_State_Event_M); overload;
    procedure Async_Get_Position_Data_P(Begin_Position, Read_Size: Int64; Source: TCore_Stream; OnResult: TOn_Stream_And_State_Event_P); overload;
    procedure Async_Get_Position_Data_C(Begin_Position, Read_Size: Int64; OnResult: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_C); overload;
    procedure Async_Get_Position_Data_M(Begin_Position, Read_Size: Int64; OnResult: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_M); overload;
    procedure Async_Get_Position_Data_P(Begin_Position, Read_Size: Int64; OnResult: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_P); overload;

    // sync save
    function Save_Data(Source: TCore_Stream): Boolean; overload;
    function Save_Data(Source: TMem64): Boolean; overload;
    function Save_And_Free_Data(Source: TCore_Stream): Boolean; overload;
    function Save_And_Free_Data(Source: TMem64): Boolean; overload;
    function Modify_Block(Block_Index, Block_Offset: Integer; Mem64: TMem64): Boolean;
    // async save
    procedure Do_Async_Save_Result(var Sender: TZDB2_Th_CMD_ID_And_State); virtual;                                       // save done
    procedure Async_Save_And_Free_Data_C(Source: TCore_Stream; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C); overload; // Queue-Engine Do OnReturn Event
    procedure Async_Save_And_Free_Data_C(Source: TMem64; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C); overload;       // Queue-Engine Do OnReturn Event
    procedure Async_Save_And_Free_Data_M(Source: TCore_Stream; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M); overload; // Queue-Engine Do OnReturn Event
    procedure Async_Save_And_Free_Data_M(Source: TMem64; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M); overload;       // Queue-Engine Do OnReturn Event
    procedure Async_Save_And_Free_Data_P(Source: TCore_Stream; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P); overload; // Queue-Engine Do OnReturn Event
    procedure Async_Save_And_Free_Data_P(Source: TMem64; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P); overload;       // Queue-Engine Do OnReturn Event
    procedure Async_Save_And_Free_Data(Source: TCore_Stream); overload;
    procedure Async_Save_And_Free_Data(Source: TMem64); overload;
    procedure Async_Save(Source: TCore_Stream); overload;
    procedure Async_Save(Source: TMem64); overload;
    procedure Async_Modify_Block_And_Free_Data(Block_Index, Block_Offset: Integer; Mem64: TMem64);
    // async save type: TMS64_Array
    procedure Async_Save_And_Free_Combine_Memory_C(const Arry: TMS64_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C); overload; // write-combine
    procedure Async_Save_And_Free_Combine_Memory_M(const Arry: TMS64_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M); overload; // write-combine
    procedure Async_Save_And_Free_Combine_Memory_P(const Arry: TMS64_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P); overload; // write-combine
    procedure Async_Save_And_Free_Combine_Memory(const Arry: TMS64_Array); overload;                                                   // write-combine
    procedure Async_Save_Combine_Memory(const Arry: TMS64_Array); overload;                                                            // write-combine
    // async save type: TMem64_Array
    procedure Async_Save_And_Free_Combine_Memory_C(const Arry: TMem64_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C); overload; // write-combine
    procedure Async_Save_And_Free_Combine_Memory_M(const Arry: TMem64_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M); overload; // write-combine
    procedure Async_Save_And_Free_Combine_Memory_P(const Arry: TMem64_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P); overload; // write-combine
    procedure Async_Save_And_Free_Combine_Memory(const Arry: TMem64_Array); overload;                                                   // write-combine
    procedure Async_Save_Combine_Memory(const Arry: TMem64_Array); overload;                                                            // write-combine
    // async save type: TStream_Array
    procedure Async_Save_And_Free_Combine_Stream_C(const Arry: TStream_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C); overload; // write-combine
    procedure Async_Save_And_Free_Combine_Stream_M(const Arry: TStream_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M); overload; // write-combine
    procedure Async_Save_And_Free_Combine_Stream_P(const Arry: TStream_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P); overload; // write-combine
    procedure Async_Save_And_Free_Combine_Stream(const Arry: TStream_Array); overload;                                                   // write-combine
    procedure Async_Save_Combine_Stream(const Arry: TStream_Array); overload;                                                            // write-combine

    // state update
    procedure Update_State_Loading_Error; // if loading error then remove it.
  end;

  TZDB2_Th_Engine_Data_List = TBigList<TZDB2_Th_Engine_Data>;

  TZDB2_Th_Engine_Data_Class = class of TZDB2_Th_Engine_Data;

{$ENDREGION 'Data_Engine'}
{$REGION 'Copy_Technology'}
  TZDB2_Th_Engine_For_C = procedure(Sender: TZDB2_Th_Engine_Data; Index: Int64; var Aborted: Boolean);
  TZDB2_Th_Engine_For_M = procedure(Sender: TZDB2_Th_Engine_Data; Index: Int64; var Aborted: Boolean) of object;
{$IFDEF FPC}
  TZDB2_Th_Engine_For_P = procedure(Sender: TZDB2_Th_Engine_Data; Index: Int64; var Aborted: Boolean) is nested;
{$ELSE FPC}
  TZDB2_Th_Engine_For_P = reference to procedure(Sender: TZDB2_Th_Engine_Data; Index: Int64; var Aborted: Boolean);
{$ENDIF FPC}

  // static copy technology
  TZDB2_Th_Engine_Static_Copy_Tech = class(TCore_Object_Intermediate)
  private
    Instance_Ptr: TZDB2_Th_Engine_Static_Copy_Instance_Pool.PQueueStruct;
  public
    Owner: TZDB2_Th_Engine;
    Queue_ID_List_: TZDB2_ID_List;
    Copy_To_Dest: U_String;
    Aborted: Boolean;
    Quiet: Boolean;
    constructor Create(Owner_: TZDB2_Th_Engine);
    destructor Destroy; override;
    procedure Do_Run(Sender: TCompute);
  end;

  // dynamic copy technology
  TZDB2_Th_Engine_Dynamic_Copy_Tech = class(TCore_Object_Intermediate)
  private
    Instance_Ptr: TZDB2_Th_Engine_Dynamic_Copy_Instance_Pool.PQueueStruct;
  public
    Owner: TZDB2_Th_Engine;
    Copy_To_Dest: U_String;
    Dynamic_Copy_Tech_Max_Queue: Integer; // default 500
    Aborted: Boolean;
    Quiet: Boolean;
    constructor Create(Owner_: TZDB2_Th_Engine);
    destructor Destroy; override;
    procedure Do_Run(Sender: TCompute);
  end;

  TZDB2_Copy_Mode =
    (
    // cmStatic is High speed IO copy, but there may be read-write data waiting for the copy queue to complete.
    // If the physical media is m2, nvme, ssd, they can be directly used
    cmStatic,
    // bmDynamicis is Slow and secure copy mode,
    // supporting level is TB/PB large-scale copy, suitable for hard drives with storage is HDD Group/Pool,
    cmDynamic,
    // cmAuto: When the data size > Static_Copy_Tech_Physics_Limit, use bmDynamicis, normal is cmStatic
    // TZDB2_Th_Engine.Copy_Mode default is cmAuto
    cmAuto
    );
{$ENDREGION 'Copy_Technology'}
{$REGION 'Thread_Engine'}

  // this multithreaded model.
  // Try to avoid calling the methods here at the application
  TZDB2_Th_Engine = class(TCore_InterfacedObject_Intermediate)
  private
    FLast_Backup_Execute_Time: TTimeTick;
    FCopy_Is_Busy: Boolean;
    FBackup_Directory: U_String; // the current database directory will be used if the backup directory is empty
    procedure DoFree(var Data: TZDB2_Th_Engine_Data);
    procedure Do_Start_Backup_Thread(thSender: TCompute);
  private
    // temp data swap technology
    // When the data is in a long loop, it is not appended to the data structure, but stored in the underlying ZDB2 database and Temp_Swap_Pool.
    // after the long loop ends, the data will truly become a engine structure
    Temp_Swap_Pool: TZDB2_Th_Engine_Data_BigList___;
    procedure Flush_Temp_Swap_Pool();
    function Get_Temp_Swap_Pool_Memory_Size: Int64;
  private
    // flush state
    FFlush_Activted_Num: Integer;
    FFlush_Total_Run_Num: Integer;
    // rolling state
    FRolling_Activted_Num: Integer;
    // limit Physics Space thread state
    FLimit_Physics_Space_Is_Running: Boolean;
  public
    // base
    Name: U_String; // default NULL
    Owner: TZDB2_Th_Engine_Marshal;
    RemoveDatabaseOnDestroy: Boolean;  // default is False
    Cache_Mode: TZDB2_SpaceMode;       // default is smBigData
    Cache_Memory: Int64;               // default is 64*1024*1024
    Database_File: U_String;           // Database_File is empty creating an in memory database, otherwise it is a file database
    OnlyRead: Boolean;                 // onlyread work in file Cache_Mode
    Delta: Int64;                      // append space delta, default is 16 * 1024 * 1024
    BlockSize: Word;                   // blocksize default is 1536
    Fast_Alloc_Space: Boolean;         // default is true
    First_Inited_Physics_Space: Int64; // initialized when creating a new database size. default is 16 * 1024 * 1024
    Limit_Max_Physics_Space: Int64;    // Limit the maximum physical storage space, and when the limit is reached, it will enter the rolling model, default is 0
    Rolling_Space_Step: Integer;       // one-step rolling remove num
    Auto_Append_Space: Boolean;        // default is true
    // cipher support
    Cipher: TZDB2_Cipher;
    // TCipherSecurity
    // 'None',
    // 'DES64', 'DES128', 'DES192',
    // 'Blowfish', 'LBC', 'LQC', 'RNG32', 'RNG64', 'LSC',
    // 'XXTea512',
    // 'RC6', 'Serpent', 'Mars', 'Rijndael', 'TwoFish',
    // 'AES128', 'AES192', 'AES256'
    Cipher_Security: TCipherSecurity; // Default is csNone
    Cipher_password: U_String;        // Default is 'DTC40@ZSERVER'
    Cipher_Level: Integer;            // Default is 1
    Cipher_Tail: Boolean;             // Default is True
    Cipher_CBC: Boolean;              // Default is True
    // backup and copy technology
    Copy_Mode: TZDB2_Copy_Mode;            // copy Cache_Mode
    Static_Copy_Tech_Physics_Limit: Int64; // this value is exceeded, dynamic-copy tech will be used
    Dynamic_Copy_Tech_Max_Queue: Integer;  // default 500
    // pool
    Engine: TZDB2_Th_Queue;                               // th-queue-engine
    Th_Engine_Data_Pool: TZDB2_Th_Engine_Data_BigList___; // queue data pool
    Th_Engine_ID_Data_Pool: TZDB2_Th_Engine_ID_Data_Pool; // ID data pool
    Last_Build_Class: TZDB2_Th_Engine_Data_Class;         // default is TZDB2_Th_Engine_Data
    // external-header-data
    External_Header_Technology: Boolean;
    External_Header_Data: TMem64;
    // fragment space
    Fragment_Space_Enabled: Boolean;
    Fragment_Space_Span: Int64;
    Fragment_Space_Read_Buffer_Cache: Boolean;
    Fragment_Space_Wait_hardware: Boolean;
    Fragment_Space_Restore_Mode: TSafe_Flush_Restore_Mode;
    Fragment_Space_Max_Flush_History_Num: Integer;
    // automated
    Password_Error_Or_Fault_Shutdown_Remove_Database: Boolean;
    // api
    constructor Create(Owner_: TZDB2_Th_Engine_Marshal); virtual;
    destructor Destroy; override;
    procedure ReadConfig(const Name_: U_String; cfg: THashStringList); overload;
    procedure ReadConfig(cfg: THashStringList); overload;
    procedure WriteConfig(cfg: THashStringList);
    procedure Update_Engine_Data_Ptr(); // reset Th_Engine and Th_Engine_Data_Ptr
    procedure Clear;
    procedure Format_Database;
    function Ready: Boolean;
    // backup technology
    function Get_Last_Backup_Execute_Time: TTimeTick;
    property Backup_Directory: U_String read FBackup_Directory write FBackup_Directory;
    property Backup_Is_Busy: Boolean read FCopy_Is_Busy;
    function Get_Backup_Directory: U_String;
    procedure Backup(Reserve_: Word);
    function Found_Backup(): Boolean;
    function Revert_Backup(remove_backup_, Build_: Boolean): Boolean;
    function Revert_Backup_From(FileName: U_String; Build_: Boolean): Boolean;
    procedure Remove_Backup;
    procedure Stop_Backup;
    procedure Wait_Backup;
    // copy technology
    procedure Copy_To_File(Dest_: U_String);
    procedure Stop_Copy;
    procedure Wait_Copy;
    // for-thread safe
    procedure For_C(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_For_C);
    procedure For_M(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_For_M);
    procedure For_P(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_For_P);
    // create or open
    procedure Build(Data_Class: TZDB2_Th_Engine_Data_Class);
    procedure Rebuild_Sequence_Data_Pool(Data_Class: TZDB2_Th_Engine_Data_Class); // rebuild sequence
    // flush
    property Flush_Activted_Num: Integer read FFlush_Activted_Num;
    property Flush_Total_Run_Num: Integer read FFlush_Total_Run_Num;
    procedure Do_Get_Sequence_Table(Sender: TZDB2_Th_Queue; var Sequence_Table: TZDB2_BlockHandle); virtual;
    procedure Flush(WaitQueue_: Boolean);
    // append
    property Temp_Swap_Pool_Memory_Size: Int64 read Get_Temp_Swap_Pool_Memory_Size;
    procedure Do_Limit_Physics_Space; // limit physics space run in thread
    function Add(Data_Class: TZDB2_Th_Engine_Data_Class; ID: Integer; ID_Size: Int64): TZDB2_Th_Engine_Data; overload;
    function Add(Data_Class: TZDB2_Th_Engine_Data_Class): TZDB2_Th_Engine_Data; overload;
    function Is_Overflow: Boolean;
    procedure Progress();
    // solved for discontinuous space.
    function Fragment_Buffer_Num: Int64;
    function Fragment_Buffer_Memory: Int64;
  end;

  TZDB2_Th_Engine_Class = class of TZDB2_Th_Engine;

  TZDB2_Th_Engine_Pool_ = TCritical_BigList<TZDB2_Th_Engine>;

  TZDB2_Th_Engine_Pool = class(TZDB2_Th_Engine_Pool_)
  private
    FLast_Minimize_Size_Engine: TZDB2_Th_Engine_Pool_.PQueueStruct;
  public
    constructor Create();
    procedure DoFree(var Data: TZDB2_Th_Engine); override;
    function Get_Minimize_Size_Engine(): TZDB2_Th_Engine;
    function Get_Minimize_Workload_Engine(): TZDB2_Th_Engine;
    function All_Is_OnlyRead(): Boolean;
  end;
{$ENDREGION 'Thread_Engine'}
{$REGION 'Data_Parallel_Load'}

  TOn_ZDB2_Th_Engine_Data_Event_C = procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMS64);
  TOn_ZDB2_Th_Engine_Data_Event_M = procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMS64) of object;
{$IFDEF FPC}
  TOn_ZDB2_Th_Engine_Data_Event_P = procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMS64) is nested;
{$ELSE FPC}
  TOn_ZDB2_Th_Engine_Data_Event_P = reference to procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMS64);
{$ENDIF FPC}
  TZDB2_Th_Engine_Data_Load_Processor = class;

  TZDB2_Th_Engine_Data_Load_Instance = class(TIO_Thread_Data)
  private
    FStream: TMS64;
    FLoad_Processor: TZDB2_Th_Engine_Data_Load_Processor;
    FData: TZDB2_Th_Engine_Data;
    FOnRun_C: TOn_ZDB2_Th_Engine_Data_Event_C;
    FOnRun_M: TOn_ZDB2_Th_Engine_Data_Event_M;
    FOnRun_P: TOn_ZDB2_Th_Engine_Data_Event_P;
    procedure Do_Read_Stream_Result(var Sender: TZDB2_Th_CMD_Stream_And_State);
  public
    constructor Create(Load_Processor_: TZDB2_Th_Engine_Data_Load_Processor; Data_: TZDB2_Th_Engine_Data);
    destructor Destroy; override;
    procedure Process; override;
  end;

  TOn_ZDB2_Th_Engine_Data_Wait_C = procedure(Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance);
  TOn_ZDB2_Th_Engine_Data_Wait_M = procedure(Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance) of object;
{$IFDEF FPC}
  TOn_ZDB2_Th_Engine_Data_Wait_P = procedure(Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance) is nested;
{$ELSE FPC}
  TOn_ZDB2_Th_Engine_Data_Wait_P = reference to procedure(Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance);
{$ENDIF FPC}

  TZDB2_Th_Engine_Data_Load_Processor = class(TCore_Object_Intermediate)
  private
    tatal_data_num_: Int64;
    buff: TZDB2_Th_Engine_Marshal_BigList___.PQueueArrayStruct;
    Max_Wait_Task_Num: Int64;
    IO_Thread_Task_Num: TAtomInt64;
    Loaded_Num, Error_Num: TAtomInt64;
    Task_Is_Run: Boolean;
    OnRun_C: TOn_ZDB2_Th_Engine_Data_Event_C;
    OnRun_M: TOn_ZDB2_Th_Engine_Data_Event_M;
    OnRun_P: TOn_ZDB2_Th_Engine_Data_Event_P;
    FTh_Pool: TIO_Thread_Base;
    procedure Do_Thread_Run();
  public
    constructor Create(ThNum_: Integer);
    destructor Destroy; override;
    procedure Run();
    procedure Wait();
    procedure Wait_C(On_Wait: TOn_ZDB2_Th_Engine_Data_Wait_C);
    procedure Wait_M(On_Wait: TOn_ZDB2_Th_Engine_Data_Wait_M);
    procedure Wait_P(On_Wait: TOn_ZDB2_Th_Engine_Data_Wait_P);
  end;
{$ENDREGION 'Data_Parallel_Load'}
{$REGION 'Position_Parallel_Load'}

  TOn_ZDB2_Th_Engine_Position_Event_C = procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMS64);
  TOn_ZDB2_Th_Engine_Position_Event_M = procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMS64) of object;
{$IFDEF FPC}
  TOn_ZDB2_Th_Engine_Position_Event_P = procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMS64) is nested;
{$ELSE FPC}
  TOn_ZDB2_Th_Engine_Position_Event_P = reference to procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMS64);
{$ENDIF FPC}
  TZDB2_Th_Engine_Position_Load_Processor = class;

  TZDB2_Th_Engine_Position_Load_Instance = class(TIO_Thread_Data)
  private
    FStream: TMS64;
    FLoad_Processor: TZDB2_Th_Engine_Position_Load_Processor;
    FData: TZDB2_Th_Engine_Data;
    FOnRun_C: TOn_ZDB2_Th_Engine_Position_Event_C;
    FOnRun_M: TOn_ZDB2_Th_Engine_Position_Event_M;
    FOnRun_P: TOn_ZDB2_Th_Engine_Position_Event_P;
    procedure Do_Read_Position_Result(var Sender: TZDB2_Th_CMD_Stream_And_State);
  public
    constructor Create(Load_Processor_: TZDB2_Th_Engine_Position_Load_Processor; Data_: TZDB2_Th_Engine_Data);
    destructor Destroy; override;
    procedure Process; override;
  end;

  TOn_ZDB2_Th_Engine_Position_Wait_C = procedure(Load_Inst_: TZDB2_Th_Engine_Position_Load_Instance);
  TOn_ZDB2_Th_Engine_Position_Wait_M = procedure(Load_Inst_: TZDB2_Th_Engine_Position_Load_Instance) of object;
{$IFDEF FPC}
  TOn_ZDB2_Th_Engine_Position_Wait_P = procedure(Load_Inst_: TZDB2_Th_Engine_Position_Load_Instance) is nested;
{$ELSE FPC}
  TOn_ZDB2_Th_Engine_Position_Wait_P = reference to procedure(Load_Inst_: TZDB2_Th_Engine_Position_Load_Instance);
{$ENDIF FPC}

  TZDB2_Th_Engine_Position_Load_Processor = class(TCore_Object_Intermediate)
  private
    tatal_data_num_: Int64;
    buff: TZDB2_Th_Engine_Marshal_BigList___.PQueueArrayStruct;
    Position_Offset, Position_ReadSize: Int64;
    Max_Wait_Task_Num: Int64;
    IO_Thread_Task_Num: TAtomInt64;
    Loaded_Num, Error_Num: TAtomInt64;
    Task_Is_Run: Boolean;
    OnRun_C: TOn_ZDB2_Th_Engine_Position_Event_C;
    OnRun_M: TOn_ZDB2_Th_Engine_Position_Event_M;
    OnRun_P: TOn_ZDB2_Th_Engine_Position_Event_P;
    FTh_Pool: TIO_Thread_Base;
    procedure Do_Thread_Run();
  public
    constructor Create(ThNum_: Integer);
    destructor Destroy; override;
    procedure Run();
    procedure Wait();
    procedure Wait_C(On_Wait: TOn_ZDB2_Th_Engine_Position_Wait_C);
    procedure Wait_M(On_Wait: TOn_ZDB2_Th_Engine_Position_Wait_M);
    procedure Wait_P(On_Wait: TOn_ZDB2_Th_Engine_Position_Wait_P);
  end;
{$ENDREGION 'Position_Parallel_Load'}
{$REGION 'Block_Parallel_Load'}

  TOn_ZDB2_Th_Engine_Block_Event_C = procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMem64);
  TOn_ZDB2_Th_Engine_Block_Event_M = procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMem64) of object;
{$IFDEF FPC}
  TOn_ZDB2_Th_Engine_Block_Event_P = procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMem64) is nested;
{$ELSE FPC}
  TOn_ZDB2_Th_Engine_Block_Event_P = reference to procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMem64);
{$ENDIF FPC}
  TZDB2_Th_Engine_Block_Load_Processor = class;

  TZDB2_Th_Engine_Block_Load_Instance = class(TIO_Thread_Data)
  private
    FMem: TMem64;
    FLoad_Processor: TZDB2_Th_Engine_Block_Load_Processor;
    FData: TZDB2_Th_Engine_Data;
    FOnRun_C: TOn_ZDB2_Th_Engine_Block_Event_C;
    FOnRun_M: TOn_ZDB2_Th_Engine_Block_Event_M;
    FOnRun_P: TOn_ZDB2_Th_Engine_Block_Event_P;
    procedure Do_Read_Block_Result(var Sender: TZDB2_Th_CMD_Mem64_And_State);
  public
    constructor Create(Load_Processor_: TZDB2_Th_Engine_Block_Load_Processor; Data_: TZDB2_Th_Engine_Data);
    destructor Destroy; override;
    procedure Process; override;
  end;

  TOn_ZDB2_Th_Engine_Block_Wait_C = procedure(Load_Inst_: TZDB2_Th_Engine_Block_Load_Instance);
  TOn_ZDB2_Th_Engine_Block_Wait_M = procedure(Load_Inst_: TZDB2_Th_Engine_Block_Load_Instance) of object;
{$IFDEF FPC}
  TOn_ZDB2_Th_Engine_Block_Wait_P = procedure(Load_Inst_: TZDB2_Th_Engine_Block_Load_Instance) is nested;
{$ELSE FPC}
  TOn_ZDB2_Th_Engine_Block_Wait_P = reference to procedure(Load_Inst_: TZDB2_Th_Engine_Block_Load_Instance);
{$ENDIF FPC}

  TZDB2_Th_Engine_Block_Load_Processor = class(TCore_Object_Intermediate)
  private
    tatal_data_num_: Int64;
    buff: TZDB2_Th_Engine_Marshal_BigList___.PQueueArrayStruct;
    Block_Index, Block_Offset, Block_ReadSize: Integer;
    Max_Wait_Task_Num: Int64;
    IO_Thread_Task_Num: TAtomInt64;
    Loaded_Num, Error_Num: TAtomInt64;
    Task_Is_Run: Boolean;
    OnRun_C: TOn_ZDB2_Th_Engine_Block_Event_C;
    OnRun_M: TOn_ZDB2_Th_Engine_Block_Event_M;
    OnRun_P: TOn_ZDB2_Th_Engine_Block_Event_P;
    FTh_Pool: TIO_Thread_Base;
    procedure Do_Thread_Run();
  public
    constructor Create(ThNum_: Integer);
    destructor Destroy; override;
    procedure Run();
    procedure Wait();
    procedure Wait_C(On_Wait: TOn_ZDB2_Th_Engine_Block_Wait_C);
    procedure Wait_M(On_Wait: TOn_ZDB2_Th_Engine_Block_Wait_M);
    procedure Wait_P(On_Wait: TOn_ZDB2_Th_Engine_Block_Wait_P);
  end;
{$ENDREGION 'Block_Parallel_Load'}
{$REGION 'Engine_Marshal'}

  TZDB2_Th_Engine_Marshal_Pool = TCritical_BigList<TZDB2_Th_Engine_Marshal>;

  // TZDB2_Th_Engine_Marshal is a parallel marshal manager.
  TZDB2_Th_Engine_Marshal = class(TCore_InterfacedObject_Intermediate) // all methods is thread safe.
  private
    FCritical: TCritical;
    FLong_Loop_Num: Integer;
    Pool_Ptr: TZDB2_Th_Engine_Marshal_Pool.PQueueStruct;
    procedure DoFree(var Data: TZDB2_Th_Engine_Data);
    procedure Do_Remove_First_Data_From_ThEngine(eng: TZDB2_Th_Engine; Recycle_Space_Size: Int64);
  public
    Owner: TCore_Object;
    Data_Marshal: TZDB2_Th_Engine_Marshal_BigList___;
    Engine_Pool: TZDB2_Th_Engine_Pool;
    Instance_Recycle_Tool: TZDB2_Th_Engine_Data_Instance_Recycle_Tool___;
    Data_Link_Recycle_Tool: TZDB2_Th_Engine_Data_Link_Recycle_Tool___;
    Current_Data_Class: TZDB2_Th_Engine_Data_Class;
    property Long_Loop_Num: Integer read FLong_Loop_Num;
    constructor Create(Owner_: TCore_Object); virtual;
    destructor Destroy; override;
    // data event
    procedure Do_Add_Data(Sender: TZDB2_Th_Engine_Data); virtual;
    procedure Do_Remove_Data(Sender: TZDB2_Th_Engine_Data); virtual;
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
    // solved for discontinuous space.
    function Fragment_Buffer_Num: Int64;
    function Fragment_Buffer_Memory: Int64;
    // pick engine and create TZDB2_Th_Engine_Data
    function Add_Data_To_Minimize_Workload_Engine(): TZDB2_Th_Engine_Data;
    function Add_Data_To_Minimize_Size_Engine(): TZDB2_Th_Engine_Data;
    function Add_Data_To_Engine(Eng_: TZDB2_Th_Engine): TZDB2_Th_Engine_Data;
    // wait busy task
    procedure Wait_Busy_Task;
    // wait long loop and backup
    procedure Wait_Long_Loop(wait_backup_: Boolean);
    // check recycle pool
    procedure Check_Recycle_Pool;
    // progress
    function Progress: Boolean;
    // backup
    function Get_Last_Backup_Execute_Time: TTimeTick;
    procedure Backup(Reserve_: Word);
    procedure Backup_If_No_Exists();
    procedure Stop_Backup;
    procedure Remove_Backup;
    // copy
    procedure Stop_Copy;
    // flush-build external-header backcall api
    procedure Prepare_Flush_External_Header(Th_Engine_: TZDB2_Th_Engine; var Sequence_Table: TZDB2_BlockHandle; Flush_Instance_Pool: TZDB2_Th_Engine_Data_Instance_Pool; External_Header_Data_: TMem64); virtual;
    // flush
    procedure Flush; overload;
    procedure Flush(WaitQueue_: Boolean); overload;
    function Flush_Is_Busy: Boolean;
    // remove and rebuild datgabase
    procedure Format_Database;
    // parallel data model
    procedure Parallel_Load_C(ThNum_: Integer; On_Run: TOn_ZDB2_Th_Engine_Data_Event_C; On_Wait: TOn_ZDB2_Th_Engine_Data_Wait_C);
    procedure Parallel_Load_M(ThNum_: Integer; On_Run: TOn_ZDB2_Th_Engine_Data_Event_M; On_Wait: TOn_ZDB2_Th_Engine_Data_Wait_M);
    procedure Parallel_Load_P(ThNum_: Integer; On_Run: TOn_ZDB2_Th_Engine_Data_Event_P; On_Wait: TOn_ZDB2_Th_Engine_Data_Wait_P);
    // parallel block model
    procedure Parallel_Block_Load_C(ThNum_, Block_Index, Block_Offset, Block_Read_Size: Integer; On_Run: TOn_ZDB2_Th_Engine_Block_Event_C; On_Wait: TOn_ZDB2_Th_Engine_Block_Wait_C);
    procedure Parallel_Block_Load_M(ThNum_, Block_Index, Block_Offset, Block_Read_Size: Integer; On_Run: TOn_ZDB2_Th_Engine_Block_Event_M; On_Wait: TOn_ZDB2_Th_Engine_Block_Wait_M);
    procedure Parallel_Block_Load_P(ThNum_, Block_Index, Block_Offset, Block_Read_Size: Integer; On_Run: TOn_ZDB2_Th_Engine_Block_Event_P; On_Wait: TOn_ZDB2_Th_Engine_Block_Wait_P);
    // parallel Position model
    procedure Parallel_Position_Load_C(ThNum_: Integer; Position_Offset, Position_Read_Size: Int64; On_Run: TOn_ZDB2_Th_Engine_Position_Event_C; On_Wait: TOn_ZDB2_Th_Engine_Position_Wait_C);
    procedure Parallel_Position_Load_M(ThNum_: Integer; Position_Offset, Position_Read_Size: Int64; On_Run: TOn_ZDB2_Th_Engine_Position_Event_M; On_Wait: TOn_ZDB2_Th_Engine_Position_Wait_M);
    procedure Parallel_Position_Load_P(ThNum_: Integer; Position_Offset, Position_Read_Size: Int64; On_Run: TOn_ZDB2_Th_Engine_Position_Event_P; On_Wait: TOn_ZDB2_Th_Engine_Position_Wait_P);
    // parallel model
    procedure Parallel_For_C(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_For_C);
    procedure Parallel_For_M(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_For_M);
    procedure Parallel_For_P(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_For_P);
    // one-way model
    procedure For_C(Max_Loop_: Int64; On_Run: TZDB2_Th_Engine_For_C);
    procedure For_M(Max_Loop_: Int64; On_Run: TZDB2_Th_Engine_For_M);
    procedure For_P(Max_Loop_: Int64; On_Run: TZDB2_Th_Engine_For_P);
    procedure Invert_For_C(Max_Loop_: Int64; On_Run: TZDB2_Th_Engine_For_C);
    procedure Invert_For_M(Max_Loop_: Int64; On_Run: TZDB2_Th_Engine_For_M);
    procedure Invert_For_P(Max_Loop_: Int64; On_Run: TZDB2_Th_Engine_For_P);
    // remove first data, scrolling support
    procedure Remove_First_Data(Num_: Int64; remove_data_: Boolean);
    procedure Remove_First_Data_For_All_Th_Engine(Th_Engine_Max_Space_Size: Int64);
    // custom loop
    procedure Begin_Loop;
    function Repeat_: TZDB2_Th_Engine_Marshal_BigList___.TRepeat___;
    function Invert_Repeat_: TZDB2_Th_Engine_Marshal_BigList___.TInvert_Repeat___;
    procedure End_Loop;
    // RemoveDatabaseOnDestroy
    function GetRemoveDatabaseOnDestroy: Boolean;
    procedure SetRemoveDatabaseOnDestroy(const Value: Boolean);
    property RemoveDatabaseOnDestroy: Boolean read GetRemoveDatabaseOnDestroy write SetRemoveDatabaseOnDestroy;
    // state info
    function Get_State_Info(): U_String;
    // test
    class procedure Test();
    class procedure Test_Backup_Support();
    class procedure Test_Remove_First_Data_Support();
  end;
{$ENDREGION 'Engine_Marshal'}


procedure Stop_All_ZDB2_Thread_Backup_Task;

var
  Th_Engine_Marshal_Pool__: TZDB2_Th_Engine_Marshal_Pool;
  Static_Copy_Instance_Pool__: TZDB2_Th_Engine_Static_Copy_Instance_Pool;   // static copy and backup technology
  Dynamic_Copy_Instance_Pool__: TZDB2_Th_Engine_Dynamic_Copy_Instance_Pool; // dynamic copy and backup technology

  // if a user program or system malfunction prevents unlocking FInstance_Busy, engine will reset based on time
  Max_Busy_Instance_Recycle_Time: TTimeTick; // 15 Minute

implementation

uses Z.Expression;

procedure Stop_All_ZDB2_Thread_Backup_Task;
begin
  Static_Copy_Instance_Pool__.Lock;
  try
    if Static_Copy_Instance_Pool__.num > 0 then
      begin
        with Static_Copy_Instance_Pool__.Repeat_ do
          repeat
              Queue^.Data.Aborted := True;
          until not Next;
      end;
  finally
      Static_Copy_Instance_Pool__.UnLock;
  end;

  Dynamic_Copy_Instance_Pool__.Lock;
  try
    if Dynamic_Copy_Instance_Pool__.num > 0 then
      begin
        with Dynamic_Copy_Instance_Pool__.Repeat_ do
          repeat
              Queue^.Data.Aborted := True;
          until not Next;
      end;
  finally
      Dynamic_Copy_Instance_Pool__.UnLock;
  end;
end;

constructor TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge.Create;
begin
  inherited Create;
  Source := nil;
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
  State := nil;
end;

procedure TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge.Do_Result(var Sender: TZDB2_Th_CMD_Mem64_And_State);
begin
  try
    if Sender.State = TCMD_State.csDone then
      begin
        Source.FSize := Sender.Mem64.Size;
        Source.Do_Ready;
      end
    else
      begin
        Source.FSize := 0;
        Source.FFirst_Operation_Ready := False;
      end;

    if Assigned(OnResult_C) then
        OnResult_C(Sender);
    if Assigned(OnResult_M) then
        OnResult_M(Sender);
    if Assigned(OnResult_P) then
        OnResult_P(Sender);
    if State <> nil then
        State^ := Sender.State;
  except
  end;
  AtomDec(Source.FAsync_Load_Num);
  DelayFreeObj(1.0, Self);
end;

constructor TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Create;
begin
  inherited Create;
  Source := nil;
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
  State := nil;
end;

procedure TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Do_Result(var Sender: TZDB2_Th_CMD_Stream_And_State);
begin
  try
    if Sender.State = TCMD_State.csDone then
      begin
        Source.FSize := Sender.Stream.Size;
        Source.Do_Ready;
      end
    else
      begin
        Source.FSize := 0;
        Source.FFirst_Operation_Ready := False;
      end;

    if Assigned(OnResult_C) then
        OnResult_C(Sender);
    if Assigned(OnResult_M) then
        OnResult_M(Sender);
    if Assigned(OnResult_P) then
        OnResult_P(Sender);
    if State <> nil then
        State^ := Sender.State;
  except
  end;
  AtomDec(Source.FAsync_Load_Num);
  DelayFreeObj(1.0, Self);
end;

procedure TZDB2_Th_Engine_Load_Mem64_Data_Event_Bridge.Do_Event;
begin
  try
    if R_State = TCMD_State.csDone then
      begin
        Source.FSize := Mem64.Size;
        Source.Do_Ready;
      end
    else
      begin
        Source.FSize := 0;
        Source.FFirst_Operation_Ready := False;
      end;

    if Assigned(OnResult_C) then
        OnResult_C(Source, Mem64, R_State = TCMD_State.csDone);
    if Assigned(OnResult_M) then
        OnResult_M(Source, Mem64, R_State = TCMD_State.csDone);
    if Assigned(OnResult_P) then
        OnResult_P(Source, Mem64, R_State = TCMD_State.csDone);
  except
  end;
  AtomDec(Source.FAsync_Load_Num);
  DelayFreeObj(1.0, Self);
end;

constructor TZDB2_Th_Engine_Load_Mem64_Data_Event_Bridge.Create;
begin
  inherited Create;
  R_State := TCMD_State.csDefault;
  Source := nil;
  Mem64 := TMem64.CustomCreate(8192);
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
end;

destructor TZDB2_Th_Engine_Load_Mem64_Data_Event_Bridge.Destroy;
begin
  DisposeObject(Mem64);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Load_Mem64_Data_Event_Bridge.Do_Result(var Sender: TZDB2_Th_CMD_Mem64_And_State);
begin
  R_State := Sender.State;
  TCompute.RunM_NP(Do_Event);
end;

procedure TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge.Do_Event;
begin
  try
    if R_State = TCMD_State.csDone then
      begin
        Source.FSize := Stream.Size;
        Source.Do_Ready;
      end
    else
      begin
        Source.FSize := 0;
        Source.FFirst_Operation_Ready := False;
      end;

    if Assigned(OnResult_C) then
        OnResult_C(Source, Stream, R_State = TCMD_State.csDone);
    if Assigned(OnResult_M) then
        OnResult_M(Source, Stream, R_State = TCMD_State.csDone);
    if Assigned(OnResult_P) then
        OnResult_P(Source, Stream, R_State = TCMD_State.csDone);
  except
  end;
  AtomDec(Source.FAsync_Load_Num);
  DelayFreeObj(1.0, Self);
end;

constructor TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge.Create;
begin
  inherited Create;
  R_State := TCMD_State.csDefault;
  Source := nil;
  Stream := TMS64.CustomCreate(8192);
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
end;

destructor TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge.Destroy;
begin
  DisposeObject(Stream);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge.Do_Result(var Sender: TZDB2_Th_CMD_Stream_And_State);
begin
  R_State := Sender.State;
  TCompute.RunM_NP(Do_Event);
end;

constructor TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
begin
  inherited Create;
  Source := nil;
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
end;

procedure TZDB2_Th_Engine_Save_Data_Event_Bridge.Do_Result(var Sender: TZDB2_Th_CMD_ID_And_State);
begin
  try
    if Assigned(OnResult_C) then
        OnResult_C(Source, Sender.State = TCMD_State.csDone);
    if Assigned(OnResult_M) then
        OnResult_M(Source, Sender.State = TCMD_State.csDone);
    if Assigned(OnResult_P) then
        OnResult_P(Source, Sender.State = TCMD_State.csDone);
  except
  end;
  if Source <> nil then
      Source.Do_Async_Save_Result(Sender);

  DelayFreeObj(1.0, Self);
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

constructor TZDB2_Th_Engine_Data.Create;
begin
  inherited Create;
  FOwner := nil;
  FOwner_Data_Ptr := nil;
  FTh_Engine := nil;
  FTh_Engine_Data_Ptr := nil;
  FID := -1;
  FSize := 0;
  FIs_Temp_Swap_Pool := False;
  FFirst_Operation_Ready := False;
  FInstance_Busy := 0;
  FInstance_Last_Busy_Time := 0;
  FLocked := False;
  FSaveFailed_Do_Remove := True;
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
  Inc(FInstance_Busy);
  FInstance_Last_Busy_Time := GetTimeTick;
end;

procedure TZDB2_Th_Engine_Data.Update_Instance_As_Free;
begin
  Dec(FInstance_Busy);
  FInstance_Last_Busy_Time := GetTimeTick;
end;

procedure TZDB2_Th_Engine_Data.Reset_Instance_As_Free();
begin
  Lock;
  FInstance_Busy := 0;
  UnLock;
  FInstance_Last_Busy_Time := 0;
end;

procedure TZDB2_Th_Engine_Data.Do_Ready;
begin
  FFirst_Operation_Ready := True;
end;

function TZDB2_Th_Engine_Data.IsOnlyRead: Boolean;
begin
  if Engine <> nil then
      Result := Engine.Is_OnlyRead
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

procedure TZDB2_Th_Engine_Data.Update_Owner_ID_Pool(OLD_, New_: Integer);
begin
  if FTh_Engine = nil then
      exit;
  if OLD_ >= 0 then
      FTh_Engine.Th_Engine_ID_Data_Pool.Delete(OLD_);
  if New_ >= 0 then
      FTh_Engine.Th_Engine_ID_Data_Pool.Add(New_, Self, True);
end;

function TZDB2_Th_Engine_Data.Can_Load: Boolean;
begin
  Result := (FID >= 0) and (FTh_Engine <> nil) and (FTh_Engine_Data_Ptr <> nil) and (not FPost_Free_Runing) and (not FLoad_Data_Error) and (FAsync_Save_Num <= 0);
end;

function TZDB2_Th_Engine_Data.Can_Progress: Boolean;
begin
  Result := (not FPost_Free_Runing) and (is_UnLocked) and (not FPost_Free_Runing) and (not FLoad_Data_Error) and (FTh_Engine <> nil) and (FTh_Engine_Data_Ptr <> nil);
end;

function TZDB2_Th_Engine_Data.Can_Free: Boolean;
begin
  Result := ((FInstance_Busy <= 0) or (GetTimeTick - FInstance_Last_Busy_Time > Max_Busy_Instance_Recycle_Time))
    and (is_UnLocked) and (FAsync_Load_Num <= 0) and (FAsync_Save_Num <= 0) and (not FIs_Temp_Swap_Pool);
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

procedure TZDB2_Th_Engine_Data.Do_Remove;
begin

end;

function TZDB2_Th_Engine_Data.Remove(Delete_Data_: Boolean): Boolean;
var
  Engine__: TZDB2_Th_Engine;
  Eng_Marshal__: TZDB2_Th_Engine_Marshal;
begin
  Result := False;
  if not Can_Free then
      exit;
  Lock;
  if not FPost_Free_Runing then
    begin
      try
        FPost_Free_Runing := True;
        Do_Remove();
        if FOwner <> nil then
            FOwner.Do_Remove_Data(Self);
        if Delete_Data_ and (Engine <> nil) then
          begin
            if (FID >= 0) then
              begin
                Engine.Async_Remove(FID);
                Update_Owner_ID_Pool(FID, -1);
              end;
            FID := -1;
            FSize := 0;
            FFirst_Operation_Ready := False;
            Result := True;
          end;

        if (FOwner <> nil) and (FOwner.FLong_Loop_Num > 0) then
          begin
            FOwner.Data_Link_Recycle_Tool.Add(Self); // post link to recycle pool
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
      except
      end;
    end;
  UnLock;
end;

function TZDB2_Th_Engine_Data.Remove(): Boolean;
begin
  Result := Remove(True);
end;

function TZDB2_Th_Engine_Data.Load_Data(Source: TCore_Stream): Boolean;
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
      if Result then
        begin
          FSize := Source.Size;
          Do_Ready;
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
      if Result then
        begin
          FSize := Source.Size;
          Do_Ready;
        end;
      AtomDec(FAsync_Load_Num);
    end;
  UnLock;
end;

function TZDB2_Th_Engine_Data.Get_Position_Data(Source: TCore_Stream; Begin_Position, Read_Size: Int64): Boolean;
begin
  Result := False;
  Lock;
  if (FID >= 0) and (not FPost_Free_Runing) then
    begin
      AtomInc(FAsync_Load_Num);
      try
          Result := Engine.Sync_Get_Position_Data_As_Stream(Source, FID, Begin_Position, Read_Size);
      except
      end;
      AtomDec(FAsync_Load_Num);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Data(Source: TCore_Stream; State: PCMD_State);
var
  bridge_: TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing or (Source = nil) then
    begin
      if State <> nil then
          State^ := TCMD_State.csError;
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.State := State;
      if State <> nil then
          State^ := TCMD_State.csDefault;
      Engine.Async_GetData_AsStream_M(FID, Source, bridge_.Do_Result);
    end
  else
    begin
      if State <> nil then
          State^ := TCMD_State.csError;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Data(Source: TMem64; State: PCMD_State);
var
  bridge_: TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing or (Source = nil) then
    begin
      if State <> nil then
          State^ := TCMD_State.csError;
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.State := State;
      if State <> nil then
          State^ := TCMD_State.csDefault;
      Engine.Async_GetData_AsMem64_M(FID, Source, bridge_.Do_Result);
    end
  else
    begin
      if State <> nil then
          State^ := TCMD_State.csError;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Data_C(Source: TCore_Stream; OnResult: TOn_Stream_And_State_Event_C);
var
  tmp: TZDB2_Th_CMD_Stream_And_State;
  bridge_: TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing or (Source = nil) then
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_C := OnResult;
      Engine.Async_GetData_AsStream_M(FID, Source, bridge_.Do_Result);
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
  if FPost_Free_Runing or (Source = nil) then
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_C := OnResult;
      Engine.Async_GetData_AsMem64_M(FID, Source, bridge_.Do_Result);
    end
  else
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Data_M(Source: TCore_Stream; OnResult: TOn_Stream_And_State_Event_M);
var
  tmp: TZDB2_Th_CMD_Stream_And_State;
  bridge_: TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing or (Source = nil) then
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_M := OnResult;
      Engine.Async_GetData_AsStream_M(FID, Source, bridge_.Do_Result);
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
  if FPost_Free_Runing or (Source = nil) then
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_M := OnResult;
      Engine.Async_GetData_AsMem64_M(FID, Source, bridge_.Do_Result);
    end
  else
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Data_P(Source: TCore_Stream; OnResult: TOn_Stream_And_State_Event_P);
var
  tmp: TZDB2_Th_CMD_Stream_And_State;
  bridge_: TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing or (Source = nil) then
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_P := OnResult;
      Engine.Async_GetData_AsStream_M(FID, Source, bridge_.Do_Result);
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
  if FPost_Free_Runing or (Source = nil) then
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Mem64_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_P := OnResult;
      Engine.Async_GetData_AsMem64_M(FID, Source, bridge_.Do_Result);
    end
  else
    begin
      tmp.Mem64 := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Stream_C(OnResult: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_C);
var
  bridge_: TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      OnResult(Self, nil, False);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_C := OnResult;
      Engine.Async_GetData_AsStream_M(FID, bridge_.Stream, bridge_.Do_Result);
    end
  else
    begin
      OnResult(Self, nil, False);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Mem64_C(OnResult: TOn_ZDB2_Th_Engine_Load_Mem64_Data_Event_C);
var
  bridge_: TZDB2_Th_Engine_Load_Mem64_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      OnResult(Self, nil, False);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Load_Mem64_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_C := OnResult;
      Engine.Async_GetData_AsMem64_M(FID, bridge_.Mem64, bridge_.Do_Result);
    end
  else
    begin
      OnResult(Self, nil, False);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Stream_M(OnResult: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_M);
var
  bridge_: TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      OnResult(Self, nil, False);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_M := OnResult;
      Engine.Async_GetData_AsStream_M(FID, bridge_.Stream, bridge_.Do_Result);
    end
  else
    begin
      OnResult(Self, nil, False);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Mem64_M(OnResult: TOn_ZDB2_Th_Engine_Load_Mem64_Data_Event_M);
var
  bridge_: TZDB2_Th_Engine_Load_Mem64_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      OnResult(Self, nil, False);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Load_Mem64_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_M := OnResult;
      Engine.Async_GetData_AsMem64_M(FID, bridge_.Mem64, bridge_.Do_Result);
    end
  else
    begin
      OnResult(Self, nil, False);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Stream_P(OnResult: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_P);
var
  bridge_: TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      OnResult(Self, nil, False);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_P := OnResult;
      Engine.Async_GetData_AsStream_M(FID, bridge_.Stream, bridge_.Do_Result);
    end
  else
    begin
      OnResult(Self, nil, False);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Load_Mem64_P(OnResult: TOn_ZDB2_Th_Engine_Load_Mem64_Data_Event_P);
var
  bridge_: TZDB2_Th_Engine_Load_Mem64_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      OnResult(Self, nil, False);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Load_Mem64_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_P := OnResult;
      Engine.Async_GetData_AsMem64_M(FID, bridge_.Mem64, bridge_.Do_Result);
    end
  else
    begin
      OnResult(Self, nil, False);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Get_Position_Data(Begin_Position, Read_Size: Int64; Source: TCore_Stream; State: PCMD_State);
var
  bridge_: TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing or (Source = nil) then
    begin
      if State <> nil then
          State^ := TCMD_State.csError;
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.State := State;
      if State <> nil then
          State^ := TCMD_State.csDefault;
      Engine.Async_Get_Position_Data_As_Stream_M(Source, FID, Begin_Position, Read_Size, bridge_.Do_Result);
    end
  else
    begin
      if State <> nil then
          State^ := TCMD_State.csError;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Get_Position_Data_C(Begin_Position, Read_Size: Int64; Source: TCore_Stream; OnResult: TOn_Stream_And_State_Event_C);
var
  tmp: TZDB2_Th_CMD_Stream_And_State;
  bridge_: TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing or (Source = nil) then
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_C := OnResult;
      Engine.Async_Get_Position_Data_As_Stream_M(Source, FID, Begin_Position, Read_Size, bridge_.Do_Result);
    end
  else
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Get_Position_Data_M(Begin_Position, Read_Size: Int64; Source: TCore_Stream; OnResult: TOn_Stream_And_State_Event_M);
var
  tmp: TZDB2_Th_CMD_Stream_And_State;
  bridge_: TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing or (Source = nil) then
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_M := OnResult;
      Engine.Async_Get_Position_Data_As_Stream_M(Source, FID, Begin_Position, Read_Size, bridge_.Do_Result);
    end
  else
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Get_Position_Data_P(Begin_Position, Read_Size: Int64; Source: TCore_Stream; OnResult: TOn_Stream_And_State_Event_P);
var
  tmp: TZDB2_Th_CMD_Stream_And_State;
  bridge_: TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing or (Source = nil) then
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Get_Stream_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_P := OnResult;
      Engine.Async_Get_Position_Data_As_Stream_M(Source, FID, Begin_Position, Read_Size, bridge_.Do_Result);
    end
  else
    begin
      tmp.Stream := Source;
      tmp.State := TCMD_State.csError;
      OnResult(tmp);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Get_Position_Data_C(Begin_Position, Read_Size: Int64; OnResult: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_C);
var
  bridge_: TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      OnResult(Self, nil, False);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge.Create;
      bridge_.Stream.Delta := Read_Size;
      bridge_.Source := Self;
      bridge_.OnResult_C := OnResult;
      Engine.Async_Get_Position_Data_As_Stream_M(bridge_.Stream, FID, Begin_Position, Read_Size, bridge_.Do_Result);
    end
  else
    begin
      OnResult(Self, nil, False);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Get_Position_Data_M(Begin_Position, Read_Size: Int64; OnResult: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_M);
var
  bridge_: TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      OnResult(Self, nil, False);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge.Create;
      bridge_.Stream.Delta := Read_Size;
      bridge_.Source := Self;
      bridge_.OnResult_M := OnResult;
      Engine.Async_Get_Position_Data_As_Stream_M(bridge_.Stream, FID, Begin_Position, Read_Size, bridge_.Do_Result);
    end
  else
    begin
      OnResult(Self, nil, False);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Get_Position_Data_P(Begin_Position, Read_Size: Int64; OnResult: TOn_ZDB2_Th_Engine_Load_Stream_Data_Event_P);
var
  bridge_: TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      OnResult(Self, nil, False);
    end
  else if (FID >= 0) then
    begin
      AtomInc(FAsync_Load_Num);
      bridge_ := TZDB2_Th_Engine_Load_Stream_Data_Event_Bridge.Create;
      bridge_.Stream.Delta := Read_Size;
      bridge_.Source := Self;
      bridge_.OnResult_P := OnResult;
      Engine.Async_Get_Position_Data_As_Stream_M(bridge_.Stream, FID, Begin_Position, Read_Size, bridge_.Do_Result);
    end
  else
    begin
      OnResult(Self, nil, False);
    end;
  UnLock;
end;

function TZDB2_Th_Engine_Data.Save_Data(Source: TCore_Stream): Boolean;
begin
  Result := False;
  Lock;
  if not FPost_Free_Runing then
    begin
      AtomInc(FAsync_Save_Num);
      Result := Engine.Sync_SetData(Source, FID);
      AtomDec(FAsync_Save_Num);
      if Result then
        begin
          FSize := Source.Size;
          Do_Ready;
        end;
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
      if Result then
        begin
          FSize := Source.Size;
          Do_Ready;
        end;
    end;
  UnLock;
end;

function TZDB2_Th_Engine_Data.Save_And_Free_Data(Source: TCore_Stream): Boolean;
begin
  Result := Save_Data(Source);
  DisposeObject(Source);
end;

function TZDB2_Th_Engine_Data.Save_And_Free_Data(Source: TMem64): Boolean;
begin
  Result := Save_Data(Source);
  DisposeObject(Source);
end;

function TZDB2_Th_Engine_Data.Modify_Block(Block_Index, Block_Offset: Integer; Mem64: TMem64): Boolean;
begin
  Result := False;
  Lock;
  if not FPost_Free_Runing then
    begin
      AtomInc(FAsync_Save_Num);
      Result := Engine.Sync_Modify_Block(ID, Block_Index, Block_Offset, Mem64);
      AtomDec(FAsync_Save_Num);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Do_Async_Save_Result(var Sender: TZDB2_Th_CMD_ID_And_State);
begin
  if Sender.State = TCMD_State.csDone then
    begin
      Update_Owner_ID_Pool(FID, Sender.ID);
      FID := Sender.ID;
      Do_Ready;
      AtomDec(FAsync_Save_Num);
    end
  else
    begin
      Update_Owner_ID_Pool(FID, -1);
      FID := -1;
      FSize := 0;
      FFirst_Operation_Ready := False;
      AtomDec(FAsync_Save_Num);
      if FSaveFailed_Do_Remove then
          Remove(False);
    end;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Data_C(Source: TCore_Stream; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C);
var
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      DisposeObject(Source);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_C := OnResult;
      Engine.Async_SetData_M(Source, True, FID, bridge_.Do_Result);
      FSize := Source.Size;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Data_C(Source: TMem64; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C);
var
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      DisposeObject(Source);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_C := OnResult;
      Engine.Async_SetData_M(Source, True, FID, bridge_.Do_Result);
      FSize := Source.Size;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Data_M(Source: TCore_Stream; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M);
var
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      DisposeObject(Source);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_M := OnResult;
      Engine.Async_SetData_M(Source, True, FID, bridge_.Do_Result);
      FSize := Source.Size;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Data_M(Source: TMem64; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M);
var
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      DisposeObject(Source);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_M := OnResult;
      Engine.Async_SetData_M(Source, True, FID, bridge_.Do_Result);
      FSize := Source.Size;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Data_P(Source: TCore_Stream; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P);
var
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      DisposeObject(Source);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_P := OnResult;
      Engine.Async_SetData_M(Source, True, FID, bridge_.Do_Result);
      FSize := Source.Size;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Data_P(Source: TMem64; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P);
var
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      DisposeObject(Source);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_P := OnResult;
      Engine.Async_SetData_M(Source, True, FID, bridge_.Do_Result);
      FSize := Source.Size;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Data(Source: TCore_Stream);
begin
  Lock;
  if FPost_Free_Runing then
    begin
      DisposeObject(Source);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      Engine.Async_SetData_M(Source, True, FID, Do_Async_Save_Result);
      FSize := Source.Size;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Data(Source: TMem64);
begin
  Lock;
  if FPost_Free_Runing then
    begin
      DisposeObject(Source);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      Engine.Async_SetData_M(Source, True, FID, Do_Async_Save_Result);
      FSize := Source.Size;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save(Source: TCore_Stream);
begin
  Lock;
  if not FPost_Free_Runing then
    begin
      AtomInc(FAsync_Save_Num);
      Engine.Async_SetData_M(Source, False, FID, Do_Async_Save_Result);
      FSize := Source.Size;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save(Source: TMem64);
begin
  Lock;
  if not FPost_Free_Runing then
    begin
      AtomInc(FAsync_Save_Num);
      Engine.Async_SetData_M(Source, False, FID, Do_Async_Save_Result);
      FSize := Source.Size;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Modify_Block_And_Free_Data(Block_Index, Block_Offset: Integer; Mem64: TMem64);
begin
  Lock;
  if not FPost_Free_Runing then
    begin
      Engine.Async_Modify_Block(FID, Block_Index, Block_Offset, Mem64, True);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Combine_Memory_C(const Arry: TMS64_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C);
var
  i: Integer;
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      for i := 0 to length(Arry) - 1 do
          DisposeObject(Arry[i]);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_C := OnResult;
      Engine.Async_SetData_From_Combine_Memory_M(Arry, True, FID, bridge_.Do_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Combine_Memory_M(const Arry: TMS64_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M);
var
  i: Integer;
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      for i := 0 to length(Arry) - 1 do
          DisposeObject(Arry[i]);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_M := OnResult;
      Engine.Async_SetData_From_Combine_Memory_M(Arry, True, FID, bridge_.Do_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Combine_Memory_P(const Arry: TMS64_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P);
var
  i: Integer;
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      for i := 0 to length(Arry) - 1 do
          DisposeObject(Arry[i]);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_P := OnResult;
      Engine.Async_SetData_From_Combine_Memory_M(Arry, True, FID, bridge_.Do_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Combine_Memory(const Arry: TMS64_Array);
var
  i: Integer;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      for i := 0 to length(Arry) - 1 do
          DisposeObject(Arry[i]);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      Engine.Async_SetData_From_Combine_Memory_M(Arry, True, FID, Do_Async_Save_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_Combine_Memory(const Arry: TMS64_Array);
var
  i: Integer;
begin
  Lock;
  if not FPost_Free_Runing then
    begin
      AtomInc(FAsync_Save_Num);
      Engine.Async_SetData_From_Combine_Memory_M(Arry, False, FID, Do_Async_Save_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Combine_Memory_C(const Arry: TMem64_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C);
var
  i: Integer;
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      for i := 0 to length(Arry) - 1 do
          DisposeObject(Arry[i]);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_C := OnResult;
      Engine.Async_SetData_From_Combine_Memory_M(Arry, True, FID, bridge_.Do_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Combine_Memory_M(const Arry: TMem64_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M);
var
  i: Integer;
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      for i := 0 to length(Arry) - 1 do
          DisposeObject(Arry[i]);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_M := OnResult;
      Engine.Async_SetData_From_Combine_Memory_M(Arry, True, FID, bridge_.Do_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Combine_Memory_P(const Arry: TMem64_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P);
var
  i: Integer;
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      for i := 0 to length(Arry) - 1 do
          DisposeObject(Arry[i]);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_P := OnResult;
      Engine.Async_SetData_From_Combine_Memory_M(Arry, True, FID, bridge_.Do_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Combine_Memory(const Arry: TMem64_Array);
var
  i: Integer;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      for i := 0 to length(Arry) - 1 do
          DisposeObject(Arry[i]);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      Engine.Async_SetData_From_Combine_Memory_M(Arry, True, FID, Do_Async_Save_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_Combine_Memory(const Arry: TMem64_Array);
var
  i: Integer;
begin
  Lock;
  if not FPost_Free_Runing then
    begin
      AtomInc(FAsync_Save_Num);
      Engine.Async_SetData_From_Combine_Memory_M(Arry, False, FID, Do_Async_Save_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Combine_Stream_C(const Arry: TStream_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C);
var
  i: Integer;
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      for i := 0 to length(Arry) - 1 do
          DisposeObject(Arry[i]);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_C := OnResult;
      Engine.Async_SetData_From_Combine_Stream_M(Arry, True, FID, bridge_.Do_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Combine_Stream_M(const Arry: TStream_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M);
var
  i: Integer;
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      for i := 0 to length(Arry) - 1 do
          DisposeObject(Arry[i]);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_M := OnResult;
      Engine.Async_SetData_From_Combine_Stream_M(Arry, True, FID, bridge_.Do_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Combine_Stream_P(const Arry: TStream_Array; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P);
var
  i: Integer;
  bridge_: TZDB2_Th_Engine_Save_Data_Event_Bridge;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      for i := 0 to length(Arry) - 1 do
          DisposeObject(Arry[i]);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      bridge_ := TZDB2_Th_Engine_Save_Data_Event_Bridge.Create;
      bridge_.Source := Self;
      bridge_.OnResult_P := OnResult;
      Engine.Async_SetData_From_Combine_Stream_M(Arry, True, FID, bridge_.Do_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_And_Free_Combine_Stream(const Arry: TStream_Array);
var
  i: Integer;
begin
  Lock;
  if FPost_Free_Runing then
    begin
      for i := 0 to length(Arry) - 1 do
          DisposeObject(Arry[i]);
    end
  else
    begin
      AtomInc(FAsync_Save_Num);
      Engine.Async_SetData_From_Combine_Stream_M(Arry, True, FID, Do_Async_Save_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Async_Save_Combine_Stream(const Arry: TStream_Array);
var
  i: Integer;
begin
  Lock;
  if not FPost_Free_Runing then
    begin
      AtomInc(FAsync_Save_Num);
      Engine.Async_SetData_From_Combine_Stream_M(Arry, False, FID, Do_Async_Save_Result);
      FSize := 0;
      for i := 0 to length(Arry) - 1 do
          Inc(FSize, Arry[i].Size);
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Data.Update_State_Loading_Error;
begin
  FLoad_Data_Error := True;
end;

constructor TZDB2_Th_Engine_Static_Copy_Tech.Create(Owner_: TZDB2_Th_Engine);
begin
  inherited Create;
  Instance_Ptr := Static_Copy_Instance_Pool__.Add(Self);
  Owner := Owner_;
  Queue_ID_List_ := TZDB2_ID_List.Create;
  Copy_To_Dest := '';
  Aborted := False;
  Quiet := False;
end;

destructor TZDB2_Th_Engine_Static_Copy_Tech.Destroy;
begin
  if Instance_Ptr <> nil then
    begin
      Static_Copy_Instance_Pool__.Remove_P(Instance_Ptr);
      Instance_Ptr := nil;
    end;
  DisposeObject(Queue_ID_List_);
  Copy_To_Dest := '';
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Static_Copy_Tech.Do_Run(Sender: TCompute);
var
  fs: TCore_FileStream;
  th: TZDB2_Th_Queue;
  __repeat__: TZDB2_Th_Engine_Data_BigList___.TRepeat___;
  hnd: TZDB2_BlockHandle;
begin
  Sender.Thread_Info := 'static copy technology';
  if not Quiet then
      DoStatus('static copy to %s', [umlGetFileName(Copy_To_Dest).Text]);

  fs := TCore_FileStream.Create(Copy_To_Dest, fmCreate);
  th := TZDB2_Th_Queue.Create(Owner.Cache_Mode, Owner.Cache_Memory, fs, True, False, Owner.Delta, Owner.BlockSize, Owner.Cipher);
  if Owner.Fast_Alloc_Space then
      th.Sync_Fast_Format_Custom_Space(umlMax(Owner.Engine.CoreSpace_Size, Owner.Delta), Owner.BlockSize)
  else
      th.Sync_Format_Custom_Space(umlMax(Owner.Engine.CoreSpace_Size, Owner.Delta), Owner.BlockSize, nil);

  // check busy queue
  while Owner.Engine.QueueNum > 0 do
      TCompute.Sleep(1);
  Owner.Owner.Check_Recycle_Pool;
  Owner.Th_Engine_Data_Pool.Lock;      // safe lock
  AtomInc(Owner.Owner.FLong_Loop_Num); // lock loop
  try
    // rebuild sequece
    if Owner.Th_Engine_Data_Pool.num > 0 then
      begin
        __repeat__ := Owner.Th_Engine_Data_Pool.Repeat_;
        repeat
          if __repeat__.Queue^.Data <> nil then
            begin
              if __repeat__.Queue^.Data.Can_Load then
                  Queue_ID_List_.Add(__repeat__.Queue^.Data.FID)
            end;
        until not __repeat__.Next;
      end;
  finally
      Owner.Th_Engine_Data_Pool.UnLock; // safe unlock
  end;
  try
    hnd := TZDB2_Core_Space.Get_Handle(Queue_ID_List_);
    Owner.Engine.Sync_Extract_To_Queue_Engine_And_Copy_Sequence_Table(hnd, th, @Aborted);
    SetLength(hnd, 0);
  except
  end;
  disposeObjectAndNil(th);
  Owner.FLast_Backup_Execute_Time := GetTimeTick();
  Owner.FCopy_Is_Busy := False;
  AtomDec(Owner.Owner.FLong_Loop_Num); // unlock loop

  // check aborted state
  if Aborted then
      umlDeleteFile(Copy_To_Dest);
  DelayFreeObj(1.0, Self);
end;

constructor TZDB2_Th_Engine_Dynamic_Copy_Tech.Create(Owner_: TZDB2_Th_Engine);
begin
  inherited Create;
  Instance_Ptr := Dynamic_Copy_Instance_Pool__.Add(Self);
  Owner := Owner_;
  Copy_To_Dest := '';
  Dynamic_Copy_Tech_Max_Queue := 500;
  Aborted := False;
  Quiet := False;
end;

destructor TZDB2_Th_Engine_Dynamic_Copy_Tech.Destroy;
begin
  if Instance_Ptr <> nil then
    begin
      Dynamic_Copy_Instance_Pool__.Remove_P(Instance_Ptr);
      Instance_Ptr := nil;
    end;
  Copy_To_Dest := '';
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Dynamic_Copy_Tech.Do_Run(Sender: TCompute);
type
  TData_State_ = record
    Mem64: TMem64;
    State: TCMD_State;
    ID: Integer;
  end;

  TDynamic_Copy_Tech_Tech_Data_State_Order_ = TOrderStruct<TData_State_>;
  PData_State_ = TDynamic_Copy_Tech_Tech_Data_State_Order_.POrderStruct;

var
  fs: TCore_FileStream;
  th: TZDB2_Th_Queue;
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Data_BigList___.PQueueArrayStruct;
  sour: TDynamic_Copy_Tech_Tech_Data_State_Order_;
  i: Int64;
  p: PData_State_;
  Table_: TZDB2_BlockHandle;
begin
  Sender.Thread_Info := 'dynamic copy technology';
  if not Quiet then
      DoStatus('dynamic copy to %s', [umlGetFileName(Copy_To_Dest).Text]);

  fs := TCore_FileStream.Create(Copy_To_Dest, fmCreate);
  th := TZDB2_Th_Queue.Create(Owner.Cache_Mode, Owner.Cache_Memory, fs, True, False, Owner.Delta, Owner.BlockSize, nil);
  if Owner.Fast_Alloc_Space then
      th.Sync_Fast_Format_Custom_Space(umlMax(Owner.Engine.CoreSpace_Size, Owner.Delta), Owner.BlockSize)
  else
      th.Sync_Format_Custom_Space(umlMax(Owner.Engine.CoreSpace_Size, Owner.Delta), Owner.BlockSize, nil);

  Aborted := False;
  // check busy queue
  while Owner.Engine.QueueNum > 0 do
      TCompute.Sleep(1);
  Owner.Th_Engine_Data_Pool.Lock;
  AtomInc(Owner.Owner.FLong_Loop_Num);
  tatal_data_num_ := Owner.Th_Engine_Data_Pool.num;
  buff := Owner.Th_Engine_Data_Pool.BuildArrayMemory();
  Owner.Th_Engine_Data_Pool.UnLock;

  sour := TDynamic_Copy_Tech_Tech_Data_State_Order_.Create;
  i := 0;
  while i < tatal_data_num_ do
    begin
      if (buff^[i]^.Data <> nil) and (buff^[i]^.Data.Can_Load) then
        begin
          p := sour.Push_Null();
          p^.Data.Mem64 := TMem64.Create;
          p^.Data.State := TCMD_State.csDefault;
          p^.Data.ID := buff^[i]^.Data.ID;
          Owner.Engine.Async_GetData_AsMem64(p^.Data.ID, p^.Data.Mem64, @p^.Data.State);
        end;
      if sour.num > Dynamic_Copy_Tech_Max_Queue then
        begin
          repeat
            while sour.First^.Data.State = TCMD_State.csDefault do
                TCompute.Sleep(1);

            if sour.First^.Data.State = TCMD_State.csDone then
                th.Async_Append(sour.First^.Data.Mem64, True)
            else
                disposeObjectAndNil(sour.First^.Data.Mem64);

            sour.Next;
          until sour.num <= umlMax(0, Dynamic_Copy_Tech_Max_Queue shr 1);
        end;
      if th.QueueNum > Dynamic_Copy_Tech_Max_Queue then
        while th.QueueNum >= umlMax(0, Dynamic_Copy_Tech_Max_Queue shr 1) do
            TCompute.Sleep(1);

      if Aborted then
          break;
      Inc(i);
    end;
  while sour.num > 0 do
    begin
      while sour.First^.Data.State = TCMD_State.csDefault do
          TCompute.Sleep(1);

      if sour.First^.Data.State = TCMD_State.csDone then
          th.Async_Append(sour.First^.Data.Mem64, True)
      else
          disposeObjectAndNil(sour.First^.Data.Mem64);

      sour.Next;
    end;
  DisposeObject(sour);

  // wait done
  th.Wait_Queue();

  // rebuild sequence table
  th.Sync_Rebuild_And_Get_Sequence_Table(Table_);
  th.Sync_Flush_Sequence_Table(Table_);

  if not Quiet then
      DoStatus('dynamic copy done total num:%d size:%s file:%s', [length(Table_), umlSizeToStr(th.CoreSpace_Size).Text, Copy_To_Dest.Text]);

  SetLength(Table_, 0);
  // free backup dest
  DisposeObject(th);
  // free backup
  System.FreeMemory(buff);
  // restore state
  Owner.FLast_Backup_Execute_Time := GetTimeTick();
  Owner.FCopy_Is_Busy := False;
  AtomDec(Owner.Owner.FLong_Loop_Num);
  // check aborted state
  if Aborted then
      umlDeleteFile(Copy_To_Dest);
  // delay free self
  DelayFreeObj(1.0, Self);
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

procedure TZDB2_Th_Engine.Do_Start_Backup_Thread(thSender: TCompute);
type
  TFile_Time_Info = record
    FileName: SystemString;
    FileTime_: TDateTime;
  end;

  TFileTime_Sort_Tool = TBigList<TFile_Time_Info>;

var
  Reserve_: Word;
  db_path: U_String;
  db_file: U_String;
  Arry: U_StringArray;
  n: U_SystemString;
  L: TFileTime_Sort_Tool;

  // static backup technology
  Static_Copy_Tech_inst: TZDB2_Th_Engine_Static_Copy_Tech;
  // dynamic backup technology
  Dynamic_Copy_Tech_inst: TZDB2_Th_Engine_Dynamic_Copy_Tech;

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
  Reserve_ := PWORD(thSender.UserData)^;
  Dispose(PWORD(thSender.UserData));

  db_path := Get_Backup_Directory();
  db_file := umlGetFileName(Engine.Database_FileName);

  DoStatus('scan backup file:' + db_file + '.backup(*)');
  Arry := umlGet_File_Array(db_path);
  L := TFileTime_Sort_Tool.Create;
  for n in Arry do
    if umlMultipleMatch(True, db_file + '.backup(*)', n) then
      with L.Add_Null^ do
        begin
          Data.FileName := umlCombineFileName(db_path, n);
          Data.FileTime_ := umlGetFileTime(Data.FileName);
        end;

  // sort backup file by time
{$IFDEF FPC}
  L.Sort_P(do_fpc_sort);
{$ELSE FPC}
  L.Sort_P(function(var L, R: TFile_Time_Info): Integer
    begin
      Result := CompareDateTime(L.FileTime_, R.FileTime_);
    end);
{$ENDIF FPC}
  // remove old backup
  while L.num > Reserve_ do
    begin
      DoStatus('remove old backup "%s"', [umlGetFileName(L.First^.Data.FileName).Text]);
      umlDeleteFile(L.First^.Data.FileName);
      L.Next;
    end;
  DisposeObject(L); // free pool

  // static backup technology
  if (Copy_Mode = TZDB2_Copy_Mode.cmStatic) or
    ((Copy_Mode = TZDB2_Copy_Mode.cmAuto) and (Engine.CoreSpace_Physics_Size < Static_Copy_Tech_Physics_Limit)) then
    begin
      // backup instance
      Static_Copy_Tech_inst := TZDB2_Th_Engine_Static_Copy_Tech.Create(Self);
      Owner.Check_Recycle_Pool;
      Static_Copy_Tech_inst.Copy_To_Dest := Make_backup_File_Name();
      Static_Copy_Tech_inst.Quiet := False;
      TCompute.RunM(nil, Self, Static_Copy_Tech_inst.Do_Run); // run static-backup thread
    end
  else
    begin
      // dynamic backup technology
      Dynamic_Copy_Tech_inst := TZDB2_Th_Engine_Dynamic_Copy_Tech.Create(Self);
      Owner.Check_Recycle_Pool;
      Dynamic_Copy_Tech_inst.Dynamic_Copy_Tech_Max_Queue := Dynamic_Copy_Tech_Max_Queue;
      Dynamic_Copy_Tech_inst.Copy_To_Dest := Make_backup_File_Name();
      Dynamic_Copy_Tech_inst.Quiet := False;
      TCompute.RunM(nil, Self, Dynamic_Copy_Tech_inst.Do_Run); // run dynamic-backup thread
    end;
end;

procedure TZDB2_Th_Engine.Flush_Temp_Swap_Pool;
begin
  Temp_Swap_Pool.Lock;
  try
    Temp_Swap_Pool.Free_Recycle_Pool;
    if Temp_Swap_Pool.num > 0 then
      with Temp_Swap_Pool.Repeat_ do
        repeat
          Queue^.Data.FOwner_Data_Ptr := Owner.Data_Marshal.Add(Queue^.Data);
          Queue^.Data.FTh_Engine_Data_Ptr := Th_Engine_Data_Pool.Add(Queue^.Data);
          Queue^.Data.FIs_Temp_Swap_Pool := False;
          Queue^.Data.Update_Owner_ID_Pool(-1, Queue^.Data.FID);
          Temp_Swap_Pool.Push_To_Recycle_Pool(Queue);
        until not Next;
    Temp_Swap_Pool.Free_Recycle_Pool;
  finally
      Temp_Swap_Pool.UnLock;
  end;
end;

function TZDB2_Th_Engine.Get_Temp_Swap_Pool_Memory_Size: Int64;
begin
  Result := 0;
  try
    Temp_Swap_Pool.Free_Recycle_Pool;
    if Temp_Swap_Pool.num > 0 then
      with Temp_Swap_Pool.Repeat_ do
        repeat
            Inc(Result, Queue^.Data.DataSize);
        until not Next;
  finally
      Temp_Swap_Pool.UnLock;
  end;
end;

constructor TZDB2_Th_Engine.Create(Owner_: TZDB2_Th_Engine_Marshal);
begin
  inherited Create;
  FLast_Backup_Execute_Time := GetTimeTick();
  FCopy_Is_Busy := False;
  FBackup_Directory := '';
  Temp_Swap_Pool := TZDB2_Th_Engine_Data_BigList___.Create; // temp data pool
  FFlush_Activted_Num := 0;
  FFlush_Total_Run_Num := 0;
  FRolling_Activted_Num := 0;
  FLimit_Physics_Space_Is_Running := False;
  Name := '';
  Owner := Owner_;
  RemoveDatabaseOnDestroy := False;
  Cache_Mode := smBigData;
  Cache_Memory := 64 * 1024 * 1024;
  Database_File := '';
  OnlyRead := False;
  Delta := 16 * 1024 * 1024;
  BlockSize := 1536;
  Fast_Alloc_Space := True;
  First_Inited_Physics_Space := Delta;
  Limit_Max_Physics_Space := 0;
  Rolling_Space_Step := 100;
  Auto_Append_Space := True;
  Cipher := nil;
  Cipher_Security := TCipherSecurity.csNone;
  Cipher_password := 'DTC40@ZSERVER';
  Cipher_Level := 1;
  Cipher_Tail := True;
  Cipher_CBC := True;
  Copy_Mode := TZDB2_Copy_Mode.cmAuto;
  Static_Copy_Tech_Physics_Limit := 1024 * 1024 * 1024;
  Dynamic_Copy_Tech_Max_Queue := 500;
  Engine := nil;
  Th_Engine_Data_Pool := TZDB2_Th_Engine_Data_BigList___.Create;
  Th_Engine_Data_Pool.OnFree := DoFree;
  Th_Engine_ID_Data_Pool := TZDB2_Th_Engine_ID_Data_Pool.Create($FFFF, nil); // ID data pool
  Owner.Engine_Pool.Add(Self);
  Last_Build_Class := TZDB2_Th_Engine_Data;
  External_Header_Technology := True;
  External_Header_Data := TMem64.CustomCreate(8 * 1024 * 1024);
  Fragment_Space_Enabled := True;
  Fragment_Space_Span := 1024 * 1024;
  Fragment_Space_Read_Buffer_Cache := True;
  Fragment_Space_Wait_hardware := True;
  Fragment_Space_Restore_Mode := TSafe_Flush_Restore_Mode.sfLastHistory;
  Fragment_Space_Max_Flush_History_Num := 10;
  Password_Error_Or_Fault_Shutdown_Remove_Database := True;
end;

destructor TZDB2_Th_Engine.Destroy;
begin
  try
    if FCopy_Is_Busy then
      begin
        Stop_Copy();
        if Database_File <> '' then
            DoStatus('"%s" wait copy task...', [Database_File.Text])
        else
            DoStatus('wait copy task...');
        while FCopy_Is_Busy do
            TCompute.Sleep(100);
      end;

    Th_Engine_ID_Data_Pool.Clear;
    disposeObjectAndNil(Th_Engine_Data_Pool);
    disposeObjectAndNil(Th_Engine_ID_Data_Pool);
    disposeObjectAndNil(Temp_Swap_Pool);
    disposeObjectAndNil(Engine);
    disposeObjectAndNil(Cipher);
    if RemoveDatabaseOnDestroy and umlFileExists(Database_File) then
        umlDeleteFile(Database_File);
    if Database_File <> '' then
        DoStatus('Close file %s', [Database_File.Text])
    else
        DoStatus('free memory %s', [Self.ClassName]);
    DisposeObject(External_Header_Data);
  except
  end;
  inherited Destroy;
end;

procedure TZDB2_Th_Engine.ReadConfig(const Name_: U_String; cfg: THashStringList);
var
  n: U_String;
begin
  Name := Name_;
  RemoveDatabaseOnDestroy := EStrToBool(cfg.GetDefaultValue('RemoveDatabaseOnDestroy', umlBoolToStr(RemoveDatabaseOnDestroy)), RemoveDatabaseOnDestroy);
  n := cfg.GetDefaultValue('Cache_Mode', '');
  if n.Same('CacheAll', 'Cache_All', 'CacheFull', 'Cache_Full') then
      Cache_Mode := TZDB2_SpaceMode.smFast
  else if n.Same('CacheNormal', 'Cache_Normal', 'CacheDefault', 'Cache_Default') then
      Cache_Mode := TZDB2_SpaceMode.smNormal
  else if n.Same('NoCache', 'No_Cache') then
      Cache_Mode := TZDB2_SpaceMode.smBigData
  else
      Cache_Mode := TZDB2_SpaceMode.smBigData;
  Cache_Memory := EStrToInt64(cfg.GetDefaultValue('Cache_Memory', umlIntToStr(Cache_Memory)), Cache_Memory);
  Database_File := cfg.GetDefaultValue('database', Database_File);
  OnlyRead := EStrToBool(cfg.GetDefaultValue('OnlyRead', umlBoolToStr(OnlyRead)), OnlyRead);
  Delta := EStrToInt64(cfg.GetDefaultValue('Delta', umlIntToStr(Delta)), Delta);
  Fast_Alloc_Space := EStrToBool(cfg.GetDefaultValue('Fast_Alloc_Space', umlBoolToStr(Fast_Alloc_Space)), Fast_Alloc_Space);
  BlockSize := EStrToInt(cfg.GetDefaultValue('BlockSize', umlIntToStr(BlockSize)), BlockSize);
  First_Inited_Physics_Space := EStrToInt64(cfg.GetDefaultValue('First_Inited_Physics_Space', umlIntToStr(First_Inited_Physics_Space)), First_Inited_Physics_Space);
  Limit_Max_Physics_Space := EStrToInt64(cfg.GetDefaultValue('Limit_Max_Physics_Space', umlIntToStr(Limit_Max_Physics_Space)), Limit_Max_Physics_Space);
  Rolling_Space_Step := EStrToInt(cfg.GetDefaultValue('Rolling_Space_Step', umlIntToStr(Rolling_Space_Step)), Rolling_Space_Step);
  Auto_Append_Space := EStrToBool(cfg.GetDefaultValue('Auto_Append_Space', umlBoolToStr(Auto_Append_Space)), Auto_Append_Space);
  Cipher_Security := TZDB2_Cipher.GetCipherSecurity(cfg.GetDefaultValue('Security', TCipher.CCipherSecurityName[Cipher_Security]));
  Cipher_password := cfg.GetDefaultValue('Password', Cipher_password);
  Cipher_Level := EStrToInt(cfg.GetDefaultValue('Level', umlIntToStr(Cipher_Level)), Cipher_Level);
  Cipher_Tail := EStrToBool(cfg.GetDefaultValue('Tail', umlBoolToStr(Cipher_Tail)), Cipher_Tail);
  Cipher_CBC := EStrToBool(cfg.GetDefaultValue('CBC', umlBoolToStr(Cipher_CBC)), Cipher_CBC);

  Copy_Mode := TZDB2_Copy_Mode.cmAuto;
  n := cfg.GetDefaultValue('Copy_Mode', '');
  if n.Same('Auto', '') then
      Copy_Mode := TZDB2_Copy_Mode.cmAuto
  else if n.Same('Static', 'Safe') then
      Copy_Mode := TZDB2_Copy_Mode.cmStatic
  else if n.Same('Dynamic') then
      Copy_Mode := TZDB2_Copy_Mode.cmDynamic;

  Static_Copy_Tech_Physics_Limit := EStrToInt64(cfg.GetDefaultValue('Static_Copy_Tech_Physics_Limit', umlIntToStr(Static_Copy_Tech_Physics_Limit)), Static_Copy_Tech_Physics_Limit);
  Dynamic_Copy_Tech_Max_Queue := EStrToInt(cfg.GetDefaultValue('Dynamic_Copy_Tech_Max_Queue', umlIntToStr(Dynamic_Copy_Tech_Max_Queue)), Dynamic_Copy_Tech_Max_Queue);
  FBackup_Directory := cfg.GetDefaultValue('Backup_Directory', FBackup_Directory);

  External_Header_Technology := EStrToBool(cfg.GetDefaultValue('External_Header_Technology', umlBoolToStr(External_Header_Technology)), External_Header_Technology);

  Fragment_Space_Enabled := EStrToBool(cfg.GetDefaultValue('Fragment_Space_Enabled', umlBoolToStr(Fragment_Space_Enabled)), Fragment_Space_Enabled);
  Fragment_Space_Span := EStrToInt64(cfg.GetDefaultValue('Fragment_Space_Span', umlIntToStr(Fragment_Space_Span)), Fragment_Space_Span);
  Fragment_Space_Read_Buffer_Cache := EStrToBool(cfg.GetDefaultValue('Fragment_Space_Read_Buffer_Cache', umlBoolToStr(Fragment_Space_Read_Buffer_Cache)), Fragment_Space_Read_Buffer_Cache);
  Fragment_Space_Wait_hardware := EStrToBool(cfg.GetDefaultValue('Fragment_Space_Wait_hardware', umlBoolToStr(Fragment_Space_Wait_hardware)), Fragment_Space_Wait_hardware);
  n := cfg.GetDefaultValue('Fragment_Space_Restore_Mode', '');
  if n.Same('Last') then
      Fragment_Space_Restore_Mode := TSafe_Flush_Restore_Mode.sfLastHistory
  else if n.Same('All') then
      Fragment_Space_Restore_Mode := TSafe_Flush_Restore_Mode.sfAllHistory
  else
      Fragment_Space_Restore_Mode := TSafe_Flush_Restore_Mode.sfIgnore;
  Fragment_Space_Max_Flush_History_Num := EStrToInt64(cfg.GetDefaultValue('Fragment_Space_Max_Flush_History_Num', umlIntToStr(Fragment_Space_Max_Flush_History_Num)), Fragment_Space_Max_Flush_History_Num);
  Password_Error_Or_Fault_Shutdown_Remove_Database := EStrToBool(cfg.GetDefaultValue('Password_Error_Or_Fault_Shutdown_Remove_Database', umlBoolToStr(Password_Error_Or_Fault_Shutdown_Remove_Database)), Password_Error_Or_Fault_Shutdown_Remove_Database);
end;

procedure TZDB2_Th_Engine.ReadConfig(cfg: THashStringList);
begin
  ReadConfig(Name, cfg);
end;

procedure TZDB2_Th_Engine.WriteConfig(cfg: THashStringList);
begin
  cfg.SetDefaultValue('RemoveDatabaseOnDestroy', umlBoolToStr(RemoveDatabaseOnDestroy));
  cfg.SetDefaultValue('; Cache_Mode', 'NoCache(default),CacheNormal,CacheAll');
  case Cache_Mode of
    smBigData: cfg.SetDefaultValue('Cache_Mode', 'NoCache');
    smNormal: cfg.SetDefaultValue('Cache_Mode', 'CacheNormal');
    smFast: cfg.SetDefaultValue('Cache_Mode', 'CacheAll');
    else cfg.SetDefaultValue('Cache_Mode', 'NoCache');
  end;
  cfg.SetDefaultValue('Cache_Memory', umlIntToStr(Cache_Memory));
  cfg.SetDefaultValue('database', Database_File);
  cfg.SetDefaultValue('OnlyRead', umlBoolToStr(OnlyRead));
  cfg.SetDefaultValue('Delta', umlIntToStr(Delta));
  cfg.SetDefaultValue('Fast_Alloc_Space', umlBoolToStr(Fast_Alloc_Space));
  cfg.SetDefaultValue('BlockSize', umlIntToStr(BlockSize));
  cfg.SetDefaultValue('First_Inited_Physics_Space', umlIntToStr(First_Inited_Physics_Space));
  cfg.SetDefaultValue('Limit_Max_Physics_Space', umlIntToStr(Limit_Max_Physics_Space));
  cfg.SetDefaultValue('Rolling_Space_Step', umlIntToStr(Rolling_Space_Step));
  cfg.SetDefaultValue('Auto_Append_Space', umlBoolToStr(Auto_Append_Space));
  cfg.SetDefaultValue('; Security', 'None(Default),DES64,DES128,DES192,Blowfish,LBC,LQC,RNG32,RNG64,LSC,XXTea512,RC6,Serpent,Mars,Rijndael,TwoFish,AES128,AES192,AES256');
  cfg.SetDefaultValue('Security', TCipher.CCipherSecurityName[Cipher_Security]);
  cfg.SetDefaultValue('Password', Cipher_password);
  cfg.SetDefaultValue('Level', umlIntToStr(Cipher_Level));
  cfg.SetDefaultValue('Tail', umlBoolToStr(Cipher_Tail));
  cfg.SetDefaultValue('CBC', umlBoolToStr(Cipher_CBC));

  // backup
  cfg.SetDefaultValue('; Copy_Mode', 'Static,Dynamic,Auto(default)');
  case Copy_Mode of
    cmStatic: cfg.SetDefaultValue('Copy_Mode', 'Static');
    cmDynamic: cfg.SetDefaultValue('Copy_Mode', 'Dynamic');
    else cfg.SetDefaultValue('Copy_Mode', 'Auto');
  end;
  cfg.SetDefaultValue('Static_Copy_Tech_Physics_Limit', umlIntToStr(Static_Copy_Tech_Physics_Limit));
  cfg.SetDefaultValue('Dynamic_Copy_Tech_Max_Queue', umlIntToStr(Dynamic_Copy_Tech_Max_Queue));
  cfg.SetDefaultValue('Backup_Directory', FBackup_Directory);

  // external header technology
  cfg.SetDefaultValue('External_Header_Technology', umlBoolToStr(External_Header_Technology));

  // fragment space
  cfg.SetDefaultValue('Fragment_Space_Enabled', umlBoolToStr(Fragment_Space_Enabled));
  cfg.SetDefaultValue('Fragment_Space_Span', umlIntToStr(Fragment_Space_Span));
  cfg.SetDefaultValue('Fragment_Space_Read_Buffer_Cache', umlBoolToStr(Fragment_Space_Read_Buffer_Cache));
  cfg.SetDefaultValue('Fragment_Space_Wait_hardware', umlBoolToStr(Fragment_Space_Wait_hardware));
  cfg.SetDefaultValue('; Fragment_Space_Restore_Mode', 'Last,All,Ignore');
  case Fragment_Space_Restore_Mode of
    sfLastHistory: cfg.SetDefaultValue('Fragment_Space_Restore_Mode', 'Last');
    sfAllHistory: cfg.SetDefaultValue('Fragment_Space_Restore_Mode', 'All');
    sfIgnore: cfg.SetDefaultValue('Fragment_Space_Restore_Mode', 'Ignore');
  end;
  cfg.SetDefaultValue('Fragment_Space_Max_Flush_History_Num', umlIntToStr(Fragment_Space_Max_Flush_History_Num));
  cfg.SetDefaultValue('Password_Error_Or_Fault_Shutdown_Remove_Database', umlBoolToStr(Password_Error_Or_Fault_Shutdown_Remove_Database));
end;

procedure TZDB2_Th_Engine.Update_Engine_Data_Ptr();
begin
  Th_Engine_Data_Pool.Lock;
  if Th_Engine_Data_Pool.num > 0 then
    begin
      with Th_Engine_Data_Pool.Repeat_ do
        repeat
          Queue^.Data.FTh_Engine := Self;
          Queue^.Data.FTh_Engine_Data_Ptr := Queue;
        until not Next;
    end;
  Th_Engine_Data_Pool.UnLock;
end;

procedure TZDB2_Th_Engine.Clear;
begin
  Flush(True);
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

function TZDB2_Th_Engine.Get_Last_Backup_Execute_Time: TTimeTick;
begin
  if FCopy_Is_Busy then
      Result := GetTimeTick()
  else
      Result := FLast_Backup_Execute_Time;
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
var
  p: PWORD;
begin
  if Engine = nil then
      exit;
  if Engine.Is_Memory_Database then
      exit;
  if not umlFileExists(Engine.Database_FileName) then
      exit;
  if FCopy_Is_Busy then
      exit;
  FCopy_Is_Busy := True;
  new(p);
  p^ := Reserve_;
  TCompute.RunM(p, nil, Do_Start_Backup_Thread);
end;

function TZDB2_Th_Engine.Found_Backup(): Boolean;
var
  db_path: U_String;
  db_file: U_String;
  Arry: U_StringArray;
  n: U_SystemString;
begin
  Result := False;

  if umlTrimSpace(Database_File) = '' then
      exit;

  try
    db_path := Get_Backup_Directory();
    db_file := umlGetFileName(Database_File);
    Arry := umlGet_File_Array(db_path);
    for n in Arry do
      if umlMultipleMatch(True, db_file + '.backup(*)', n) then
        begin
          Result := True;
          break;
        end;
  except
  end;

  db_path := '';
  db_file := '';
  SetLength(Arry, 0);
end;

function TZDB2_Th_Engine.Revert_Backup(remove_backup_, Build_: Boolean): Boolean;
type
  TFile_Time_Info = record
    FileName: SystemString;
    FileTime_: TDateTime;
  end;

  TFileTime_Sort_Tool = TBigList<TFile_Time_Info>;

var
  db_path: U_String;
  db_file: U_String;
  Arry: U_StringArray;
  n: U_SystemString;
  L: TFileTime_Sort_Tool;

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

  while FCopy_Is_Busy do
      TCompute.Sleep(100);
  FCopy_Is_Busy := True;

  try
    Th_Engine_Data_Pool.Clear;
    disposeObjectAndNil(Engine);
    disposeObjectAndNil(Cipher);

    db_path := Get_Backup_Directory();
    db_file := umlGetFileName(Database_File);

    DoStatus('scan Backup-Revert for "%s"', [umlGetFileName(Database_File).Text]);
    Arry := umlGet_File_Array(db_path);
    L := TFileTime_Sort_Tool.Create;
    for n in Arry do
      if umlMultipleMatch(True, db_file + '.backup(*)', n) then
        with L.Add_Null^ do
          begin
            Data.FileName := umlCombineFileName(db_path, n);
            Data.FileTime_ := umlGetFileTime(Data.FileName);
          end;

    // sort backup file by time
{$IFDEF FPC}
    L.Sort_P(do_fpc_sort);
{$ELSE FPC}
    L.Sort_P(function(var L, R: TFile_Time_Info): Integer
      begin
        Result := CompareDateTime(L.FileTime_, R.FileTime_);
      end);
{$ENDIF FPC}
    if L.num > 0 then
      begin
        umlDeleteFile(Database_File);
        Result := umlCopyFile(L.Last^.Data.FileName, Database_File);
        if Result then
          begin
            DoStatus('Done Revert %s -> %s', [umlGetFileName(L.Last^.Data.FileName).Text, umlGetFileName(Database_File).Text]);
            if remove_backup_ then
              begin
                umlDeleteFile(L.Last^.Data.FileName);
                DoStatus('Remove Backup %s', [umlGetFileName(L.Last^.Data.FileName).Text]);
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
      FCopy_Is_Busy := False;
  end;
end;

function TZDB2_Th_Engine.Revert_Backup_From(FileName: U_String; Build_: Boolean): Boolean;
begin
  Result := False;

  if umlTrimSpace(Database_File) = '' then
      exit;
  if not umlFileExists(FileName) then
      exit;

  while FCopy_Is_Busy do
      TCompute.Sleep(100);
  FCopy_Is_Busy := True;

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
      FCopy_Is_Busy := False;
  end;
end;

procedure TZDB2_Th_Engine.Remove_Backup;
var
  db_path: U_String;
  db_file: U_String;
  Arry: U_StringArray;
  n, fn: U_SystemString;
begin
  if FCopy_Is_Busy then
      exit;
  if umlTrimSpace(Database_File) = '' then
      exit;

  try
    db_path := Get_Backup_Directory();
    db_file := umlGetFileName(Database_File);
    Arry := umlGet_File_Array(db_path);
    for n in Arry do
      if umlMultipleMatch(True, db_file + '.backup(*)', n) then
        begin
          fn := umlCombineFileName(db_path, n);
          umlDeleteFile(fn);
          DoStatus('remove %s', [fn]);
        end;
  except
  end;

  db_path := '';
  db_file := '';
  SetLength(Arry, 0);
end;

procedure TZDB2_Th_Engine.Stop_Backup;
begin
  Stop_Copy;
end;

procedure TZDB2_Th_Engine.Wait_Backup;
begin
  Wait_Copy;
end;

procedure TZDB2_Th_Engine.Copy_To_File(Dest_: U_String);
var
  // static backup technology
  Static_Copy_Tech_inst: TZDB2_Th_Engine_Static_Copy_Tech;
  // dynamic backup technology
  Dynamic_Copy_Tech_inst: TZDB2_Th_Engine_Dynamic_Copy_Tech;
begin
  // static backup technology
  if (Copy_Mode = TZDB2_Copy_Mode.cmStatic) or
    ((Copy_Mode = TZDB2_Copy_Mode.cmAuto) and (Engine.CoreSpace_Physics_Size < Static_Copy_Tech_Physics_Limit)) then
    begin
      // backup instance
      Static_Copy_Tech_inst := TZDB2_Th_Engine_Static_Copy_Tech.Create(Self);
      Owner.Check_Recycle_Pool;
      Static_Copy_Tech_inst.Copy_To_Dest := Dest_;
      Static_Copy_Tech_inst.Quiet := False;
      TCompute.RunM(nil, Self, Static_Copy_Tech_inst.Do_Run); // run static-backup thread
    end
  else
    begin
      // dynamic backup technology
      Dynamic_Copy_Tech_inst := TZDB2_Th_Engine_Dynamic_Copy_Tech.Create(Self);
      Owner.Check_Recycle_Pool;
      Dynamic_Copy_Tech_inst.Dynamic_Copy_Tech_Max_Queue := Dynamic_Copy_Tech_Max_Queue;
      Dynamic_Copy_Tech_inst.Copy_To_Dest := Dest_;
      Dynamic_Copy_Tech_inst.Quiet := False;
      TCompute.RunM(nil, Self, Dynamic_Copy_Tech_inst.Do_Run); // run dynamic-backup thread
    end;
end;

procedure TZDB2_Th_Engine.Stop_Copy;
begin
  Static_Copy_Instance_Pool__.Lock;
  try
    if Static_Copy_Instance_Pool__.num > 0 then
      begin
        with Static_Copy_Instance_Pool__.Repeat_ do
          repeat
            if Queue^.Data.Owner = Self then
                Queue^.Data.Aborted := True;
          until not Next;
      end;
  finally
      Static_Copy_Instance_Pool__.UnLock;
  end;

  Dynamic_Copy_Instance_Pool__.Lock;
  try
    if Dynamic_Copy_Instance_Pool__.num > 0 then
      begin
        with Dynamic_Copy_Instance_Pool__.Repeat_ do
          repeat
            if Queue^.Data.Owner = Self then
                Queue^.Data.Aborted := True;
          until not Next;
      end;
  finally
      Dynamic_Copy_Instance_Pool__.UnLock;
  end;
end;

procedure TZDB2_Th_Engine.Wait_Copy;
begin
  if FCopy_Is_Busy then
    begin
      if Database_File <> '' then
          DoStatus('"%s" wait copy task...', [Database_File.Text])
      else
          DoStatus('wait copy task...');
      while FCopy_Is_Busy do
          TCompute.Sleep(100);
    end;
end;

procedure TZDB2_Th_Engine.For_C(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_For_C);
var
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Data_BigList___.PQueueArrayStruct;
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
        Can_Load := inst.Can_Load and inst.First_Operation_Ready;
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
  Owner.Check_Recycle_Pool;
  if Th_Engine_Data_Pool.num <= 0 then
      exit;

  Th_Engine_Data_Pool.Lock;
  AtomInc(Owner.FLong_Loop_Num);
  tatal_data_num_ := Th_Engine_Data_Pool.num;
  buff := Th_Engine_Data_Pool.BuildArrayMemory();
  Th_Engine_Data_Pool.UnLock;
  Aborted := False;

{$IFDEF FPC}
  ParallelFor(ThNum_, Parallel_, 0, tatal_data_num_ - 1, fpc_ParallelFor);
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
          Can_Load := inst.Can_Load and inst.First_Operation_Ready;
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
  AtomDec(Owner.FLong_Loop_Num);
  System.FreeMemory(buff);
  Owner.Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine.For_M(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_For_M);
var
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Data_BigList___.PQueueArrayStruct;
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
        Can_Load := inst.Can_Load and inst.First_Operation_Ready;
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
  Owner.Check_Recycle_Pool;
  if Th_Engine_Data_Pool.num <= 0 then
      exit;

  Th_Engine_Data_Pool.Lock;
  AtomInc(Owner.FLong_Loop_Num);
  tatal_data_num_ := Th_Engine_Data_Pool.num;
  buff := Th_Engine_Data_Pool.BuildArrayMemory();
  Th_Engine_Data_Pool.UnLock;
  Aborted := False;

{$IFDEF FPC}
  ParallelFor(ThNum_, Parallel_, 0, tatal_data_num_ - 1, fpc_ParallelFor);
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
          Can_Load := inst.Can_Load and inst.First_Operation_Ready;
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
  AtomDec(Owner.FLong_Loop_Num);
  System.FreeMemory(buff);
  Owner.Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine.For_P(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_For_P);
var
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Data_BigList___.PQueueArrayStruct;
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
        Can_Load := inst.Can_Load and inst.First_Operation_Ready;
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
  Owner.Check_Recycle_Pool;
  if Th_Engine_Data_Pool.num <= 0 then
      exit;

  Th_Engine_Data_Pool.Lock;
  AtomInc(Owner.FLong_Loop_Num);
  tatal_data_num_ := Th_Engine_Data_Pool.num;
  buff := Th_Engine_Data_Pool.BuildArrayMemory();
  Th_Engine_Data_Pool.UnLock;
  Aborted := False;

{$IFDEF FPC}
  ParallelFor(ThNum_, Parallel_, 0, tatal_data_num_ - 1, fpc_ParallelFor);
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
          Can_Load := inst.Can_Load and inst.First_Operation_Ready;
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
  AtomDec(Owner.FLong_Loop_Num);
  System.FreeMemory(buff);
  Owner.Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine.Build(Data_Class: TZDB2_Th_Engine_Data_Class);
var
  Hash_L: THashStringList;
  Stream: TCore_Stream;
  Queue_Table_: TZDB2_BlockHandle;
  ID_Size_Buffer: TSequence_Table_ID_Size_Buffer;
  i: Integer;
begin
{$IFDEF SHOW_ZDB2_THREAD_BUILD_LOG}
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
{$ENDIF SHOW_ZDB2_THREAD_BUILD_LOG}
  Th_Engine_Data_Pool.Clear;
  disposeObjectAndNil(Engine);
  disposeObjectAndNil(Cipher);
  External_Header_Data.Clear;

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
          begin
            umlDeleteFile(Database_File);
            Stream := TCore_FileStream.Create(Database_File, fmCreate);
          end
        else
          begin
{$IFDEF ZDB2_Thread_Engine_Safe_Flush}
            if Fragment_Space_Enabled then
              begin
                Stream := TSafe_Flush_Stream.Create(Database_File,
                  not umlFileExists(Database_File),
                  not OnlyRead,
                  False,
                  Fragment_Space_Wait_hardware, Fragment_Space_Restore_Mode);

                TSafe_Flush_Stream(Stream).Fragment_Space_Span := Fragment_Space_Span;
                TSafe_Flush_Stream(Stream).Fragment_Space_Read_Buffer_Cache := Fragment_Space_Read_Buffer_Cache;
                TSafe_Flush_Stream(Stream).Max_Flush_History_Num := Fragment_Space_Max_Flush_History_Num;
                TSafe_Flush_Stream(Stream).Flush;
              end
            else
              begin
                Stream := TReliableFileStream.Create(Database_File, not umlFileExists(Database_File), not OnlyRead);
              end;
{$ELSE ZDB2_Thread_Engine_Safe_Flush}
            Stream := TReliableFileStream.Create(Database_File, not umlFileExists(Database_File), not OnlyRead);
{$ENDIF ZDB2_Thread_Engine_Safe_Flush}
          end;
        DoStatus('Open ZDB2 DB %s', [Database_File.Text]);
      end;
  except
      exit;
  end;

  try
    if (Stream.Size = 0) then
      begin
        // check stream
        Engine := TZDB2_Th_Queue.Create(Cache_Mode, Cache_Memory, Stream, True, OnlyRead, Delta, BlockSize, Cipher);
        Engine.Fast_Append_Space := Fast_Alloc_Space;
        Engine.Auto_Append_Space := Auto_Append_Space;
        if Fast_Alloc_Space then
            Engine.Async_Fast_Format_Custom_Space(First_Inited_Physics_Space, BlockSize)
        else
            Engine.Async_Format_Custom_Space(First_Inited_Physics_Space, BlockSize);
      end
    else if TZDB2_Core_Space.CheckStream(Stream, Cipher, Found_Backup()) then // check open from cipher and check Fault Shutdown
      begin
        Engine := TZDB2_Th_Queue.Create(Cache_Mode, Cache_Memory, Stream, True, OnlyRead, Delta, BlockSize, Cipher);
        Engine.Fast_Append_Space := Fast_Alloc_Space;
        Engine.Auto_Append_Space := Auto_Append_Space;
        // init sequence
        DoStatus('"%s" load sequence table', [umlGetFileName(Database_File).Text]);
        if Engine.Sync_Get_And_Clean_Sequence_Table(Queue_Table_) then
          begin
            DoStatus('"%s" compute sequence space', [umlGetFileName(Database_File).Text]);
            if Engine.Sync_Get_ID_Size_From_Sequence_Table(Queue_Table_, ID_Size_Buffer) then
              begin
                for i := Low(Queue_Table_) to high(Queue_Table_) do
                    Add(Data_Class, Queue_Table_[i], ID_Size_Buffer[i]);
              end;
            SetLength(Queue_Table_, 0);
            SetLength(ID_Size_Buffer, 0);
          end;
        if Engine.Sync_Get_And_Reset_External_Header(External_Header_Data) then
          begin
            DoStatus('"%s" load external header data %s', [umlGetFileName(Database_File).Text, umlSizeToStr(External_Header_Data.Size).Text]);
          end;
        DoStatus('"%s" open done.', [umlGetFileName(Database_File).Text]);
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
        if Password_Error_Or_Fault_Shutdown_Remove_Database then
          begin
            DoStatus('automation restore: a data error occurred and the system has removed the database: %s', [Database_File.Text]);
            umlDeleteFile(Database_File);
            Build(Data_Class);
            exit;
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
  ID_Size_Buffer: TSequence_Table_ID_Size_Buffer;
  i: Integer;
begin
  if Engine = nil then
      exit;
  Th_Engine_Data_Pool.Clear;

  if Engine.Sync_Rebuild_And_Get_Sequence_Table(Queue_Table_) then
    begin
      if Engine.Sync_Get_ID_Size_From_Sequence_Table(Queue_Table_, ID_Size_Buffer) then
        begin
          for i := Low(Queue_Table_) to high(Queue_Table_) do
              Add(Data_Class, Queue_Table_[i], ID_Size_Buffer[i]);
        end;
      SetLength(Queue_Table_, 0);
      SetLength(ID_Size_Buffer, 0);
    end;
  Last_Build_Class := Data_Class;
end;

procedure TZDB2_Th_Engine.Do_Get_Sequence_Table(Sender: TZDB2_Th_Queue; var Sequence_Table: TZDB2_BlockHandle);
var
  tmp: TZDB2_Th_Engine_Data_Instance_Pool;
  i: Integer;
  __repeat__: TZDB2_Th_Engine_Data_BigList___.TRepeat___;
begin
  tmp := TZDB2_Th_Engine_Data_Instance_Pool.Create;
  Th_Engine_Data_Pool.Lock;
  try
    Th_Engine_Data_Pool.Free_Recycle_Pool;
    SetLength(Sequence_Table, Th_Engine_Data_Pool.num);
    i := 0;
    if Th_Engine_Data_Pool.num > 0 then
      begin
        __repeat__ := Th_Engine_Data_Pool.Repeat_;
        repeat
          if __repeat__.Queue^.Data <> nil then
            begin
              if __repeat__.Queue^.Data.Can_Load then
                begin
                  Sequence_Table[i] := __repeat__.Queue^.Data.FID;
                  tmp.Add(__repeat__.Queue^.Data);
                  Inc(i);
                end;
            end;
        until not __repeat__.Next;
        SetLength(Sequence_Table, i)
      end;
  except
      SetLength(Sequence_Table, 0);
  end;
  Th_Engine_Data_Pool.UnLock;
  External_Header_Data.Clear;
  if External_Header_Technology then
      Owner.Prepare_Flush_External_Header(Self, Sequence_Table, tmp, External_Header_Data); // external header-data
  DisposeObject(tmp);
end;

procedure TZDB2_Th_Engine.Flush(WaitQueue_: Boolean);
begin
  if Engine = nil then
      exit;

  if RemoveDatabaseOnDestroy then
      exit;

  if Engine.Is_Memory_Database or Engine.Is_OnlyRead then
      exit;

  if WaitQueue_ then
    begin
      while Engine.QueueNum > 0 do // wait queue
          TCompute.Sleep(1);
    end;

  // check Modification
  if Engine.QueueNum = 0 then
    if (not Engine.Is_Modification) and (FFlush_Total_Run_Num > 0) then
        exit;

  if not OnlyRead then
    begin
      Engine.Async_Flush_Backcall_Sequence_Table(Do_Get_Sequence_Table);
      Engine.Async_Flush_External_Header(External_Header_Data, False);
      Engine.Async_Flush();
      Engine.Async_NOP(FFlush_Activted_Num);
      Engine.Async_INC(FFlush_Total_Run_Num);

      if WaitQueue_ then
        while Engine.QueueNum > 0 do
            TCompute.Sleep(1);
    end;
end;

procedure TZDB2_Th_Engine.Do_Limit_Physics_Space;
var
  tmp: TZDB2_ID_Pool;
  __repeat__: TZDB2_Th_Engine_Data_BigList___.TRepeat___;
  tmp_Instance: TZDB2_Th_Engine_Data;
begin
  tmp := TZDB2_ID_Pool.Create;
  Th_Engine_Data_Pool.Lock;
  try
    Th_Engine_Data_Pool.Free_Recycle_Pool;
    if Th_Engine_Data_Pool.num > 0 then
      begin
        __repeat__ := Th_Engine_Data_Pool.Repeat_;
        repeat
          if __repeat__.Queue^.Data <> nil then
            begin
              if __repeat__.Queue^.Data.Can_Free then
                  tmp.Add(__repeat__.Queue^.Data.ID);
            end;
        until (tmp.num >= Rolling_Space_Step) or (not __repeat__.Next);
      end;
  except
  end;
  Th_Engine_Data_Pool.UnLock;

  try
    if tmp.num > 0 then
      with tmp.Repeat_ do
        repeat
          tmp_Instance := Th_Engine_ID_Data_Pool[Queue^.Data];
          if tmp_Instance <> nil then
              tmp_Instance.Remove(True);
        until not Next;
  except
  end;
  DisposeObject(tmp);
  Engine.Async_NOP(FRolling_Activted_Num);
end;

function TZDB2_Th_Engine.Add(Data_Class: TZDB2_Th_Engine_Data_Class; ID: Integer; ID_Size: Int64): TZDB2_Th_Engine_Data;
var
  Data_Instance: TZDB2_Th_Engine_Data;
begin
  Result := nil;
  if Engine = nil then
      exit;

  Data_Instance := Data_Class.Create();
  Data_Instance.FOwner := Owner;
  Data_Instance.FTh_Engine := Self;
  Data_Instance.FID := ID;
  Data_Instance.FSize := ID_Size;

  if Owner.FLong_Loop_Num > 0 then
    begin
      Data_Instance.FIs_Temp_Swap_Pool := True;
      Temp_Swap_Pool.Add(Data_Instance);
    end
  else
    begin
      // temp data swap technology
      // When the data is in a long loop, it is not appended to the data structure, but stored in the underlying ZDB2 database and Temp_Swap_Pool.
      // after the long loop ends, the data will truly become a engine structure
      try
          Flush_Temp_Swap_Pool;
      except
      end;

      // complete data instance
      Data_Instance.FIs_Temp_Swap_Pool := False;
      Data_Instance.Lock;
      Data_Instance.FOwner_Data_Ptr := Owner.Data_Marshal.Add(Data_Instance);
      Data_Instance.FTh_Engine_Data_Ptr := Th_Engine_Data_Pool.Add(Data_Instance);
      Data_Instance.UnLock;
      Data_Instance.Update_Owner_ID_Pool(-1, Data_Instance.FID);

      // check physics space
      if (Limit_Max_Physics_Space > 0) and (Engine.CoreSpace_Size >= Limit_Max_Physics_Space) and (FRolling_Activted_Num <= 0) then
        begin
          if FLimit_Physics_Space_Is_Running then
              exit;
          TCompute.RunM_NP(Do_Limit_Physics_Space, @FLimit_Physics_Space_Is_Running, nil);
        end;
    end;
  Result := Data_Instance;
  Owner.Do_Add_Data(Data_Instance);
end;

function TZDB2_Th_Engine.Add(Data_Class: TZDB2_Th_Engine_Data_Class): TZDB2_Th_Engine_Data;
begin
  Result := Add(Data_Class, -1, 0);
end;

function TZDB2_Th_Engine.Is_Overflow: Boolean;
begin
  Result := (Limit_Max_Physics_Space > 0) and (Engine.CoreSpace_Size + (Limit_Max_Physics_Space div 10) >= Limit_Max_Physics_Space);
end;

procedure TZDB2_Th_Engine.Progress();
begin
  if Engine = nil then
      exit;

  Th_Engine_Data_Pool.Lock;
  try
    if Th_Engine_Data_Pool.num > 0 then
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

function TZDB2_Th_Engine.Fragment_Buffer_Num: Int64;
begin
  if Engine <> nil then
      Result := Engine.Fragment_Buffer_Memory
  else
      Result := 0;
end;

function TZDB2_Th_Engine.Fragment_Buffer_Memory: Int64;
begin
  if Engine <> nil then
      Result := Engine.Fragment_Buffer_Num
  else
      Result := 0;
end;

constructor TZDB2_Th_Engine_Pool.Create;
begin
  inherited Create;
  FLast_Minimize_Size_Engine := nil;
end;

procedure TZDB2_Th_Engine_Pool.DoFree(var Data: TZDB2_Th_Engine);
begin
  disposeObjectAndNil(Data);
end;

function TZDB2_Th_Engine_Pool.Get_Minimize_Size_Engine: TZDB2_Th_Engine;
var
  found_state: Boolean;
  Eng_: PQueueStruct;
begin
  Result := nil;
  if num > 0 then
    begin
      Lock;
      try
        Eng_ := nil;
        found_state := False;
        with Repeat_ do
          repeat
            if FLast_Minimize_Size_Engine = Queue then
                found_state := True;
            if (Queue^.Data.Engine <> nil) and (not Queue^.Data.Engine.Is_OnlyRead) and (not Queue^.Data.Is_Overflow) then
              begin
                if Eng_ = nil then
                    Eng_ := Queue
                else if Queue^.Data.Engine.CoreSpace_Size + Queue^.Data.Engine.Queue_Write_IO_Size.V <
                  Eng_^.Data.Engine.CoreSpace_Size + Eng_^.Data.Engine.Queue_Write_IO_Size.V then
                    Eng_ := Queue;
              end;
          until not Next;
        if not found_state then
            FLast_Minimize_Size_Engine := nil;

        if Eng_ <> nil then
          begin
            FLast_Minimize_Size_Engine := Eng_;
            Result := FLast_Minimize_Size_Engine^.Data;
            exit;
          end;
        // rolling model
        if FLast_Minimize_Size_Engine = nil then
          begin
            if First^.Data.Engine.Is_OnlyRead then
                exit;
            FLast_Minimize_Size_Engine := First;
            Result := FLast_Minimize_Size_Engine^.Data;
            exit;
          end;
        if not FLast_Minimize_Size_Engine^.Data.Engine.Is_OnlyRead then
            Result := FLast_Minimize_Size_Engine^.Data;
        FLast_Minimize_Size_Engine := FLast_Minimize_Size_Engine^.Next;
      finally
          UnLock;
      end;
    end;
end;

function TZDB2_Th_Engine_Pool.Get_Minimize_Workload_Engine: TZDB2_Th_Engine;
var
  Eng_: PQueueStruct;
begin
  Result := nil;
  if num > 0 then
    begin
      Lock;
      try
        Eng_ := nil;
        with Repeat_ do
          repeat
            if (Queue^.Data.Engine <> nil) and (not Queue^.Data.Engine.Is_OnlyRead) then
              begin
                if Eng_ = nil then
                    Eng_ := Queue
                else if Queue^.Data.Engine.QueueNum < Eng_^.Data.Engine.QueueNum then
                    Eng_ := Queue;
              end;
          until not Next;
        if Eng_ <> nil then
            Result := Eng_^.Data;
      finally
          UnLock;
      end;
    end;
end;

function TZDB2_Th_Engine_Pool.All_Is_OnlyRead(): Boolean;
begin
  Result := False;
  if num > 0 then
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
      FLoad_Processor.FTh_Pool.Enqueue(Self);
      FLoad_Processor.IO_Thread_Task_Num.UnLock(FLoad_Processor.IO_Thread_Task_Num.LockP^ - 1);
      FLoad_Processor.Loaded_Num.UnLock(FLoad_Processor.Loaded_Num.LockP^ + 1);
    end
  else
    begin
      FLoad_Processor.IO_Thread_Task_Num.UnLock(FLoad_Processor.IO_Thread_Task_Num.LockP^ - 1);
      FLoad_Processor.Error_Num.UnLock(FLoad_Processor.Error_Num.LockP^ + 1);
      DelayFreeObj(1.0, Self);
    end;
end;

constructor TZDB2_Th_Engine_Data_Load_Instance.Create(Load_Processor_: TZDB2_Th_Engine_Data_Load_Processor; Data_: TZDB2_Th_Engine_Data);
begin
  inherited Create;
  FStream := TMS64.CustomCreate(8192);
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

procedure TZDB2_Th_Engine_Data_Load_Processor.Do_Thread_Run();
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
            Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance.Create(Self, buff^[i]^.Data);
            Load_Inst_.FOnRun_C := OnRun_C;
            Load_Inst_.FOnRun_M := OnRun_M;
            Load_Inst_.FOnRun_P := OnRun_P;
            Load_Inst_.FData.Async_Load_Data_M(Load_Inst_.FStream, Load_Inst_.Do_Read_Stream_Result);
            IO_Thread_Task_Num.UnLock(IO_Thread_Task_Num.LockP^ + 1);
          end;
        while IO_Thread_Task_Num.V > Max_Wait_Task_Num do
            TCompute.Sleep(10);
      except
      end;
      Inc(i);
    end;
  while (IO_Thread_Task_Num.V + FTh_Pool.Count > 0) do
      TCompute.Sleep(10);
  Task_Is_Run := False;
end;

constructor TZDB2_Th_Engine_Data_Load_Processor.Create(ThNum_: Integer);
begin
  inherited Create;
  tatal_data_num_ := 0;
  buff := nil;
  Max_Wait_Task_Num := 1000;
  IO_Thread_Task_Num := TAtomInt64.Create(0);
  Loaded_Num := TAtomInt64.Create(0);
  Error_Num := TAtomInt64.Create(0);
  Task_Is_Run := False;
  OnRun_C := nil;
  OnRun_M := nil;
  OnRun_P := nil;

{$IFDEF Enabled_ZDB2_Load_Thread}
  if ThNum_ < 2 then
      FTh_Pool := TIO_Direct.Create()
  else
      FTh_Pool := TIO_Thread.Create(ThNum_);
{$ELSE Enabled_ZDB2_Load_Thread}
  FTh_Pool := TIO_Direct.Create();
{$ENDIF Enabled_ZDB2_Load_Thread}
end;

destructor TZDB2_Th_Engine_Data_Load_Processor.Destroy;
begin
  DisposeObject(FTh_Pool);
  if buff <> nil then
      System.FreeMemory(buff);
  DisposeObject(IO_Thread_Task_Num);
  DisposeObject(Loaded_Num);
  DisposeObject(Error_Num);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Data_Load_Processor.Run();
begin
  Task_Is_Run := True;
  TCompute.RunM_NP(Do_Thread_Run);
end;

procedure TZDB2_Th_Engine_Data_Load_Processor.Wait();
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance;
  tk: TTimeTick;
begin
  tk := GetTimeTick;
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance(FTh_Pool.Dequeue);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
        begin
          TCompute.Sleep(1);
          DoStatus();
        end;
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('full load %d/%d error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('all load done: %d/%d, error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
end;

procedure TZDB2_Th_Engine_Data_Load_Processor.Wait_C(On_Wait: TOn_ZDB2_Th_Engine_Data_Wait_C);
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance;
  tk: TTimeTick;
begin
  tk := GetTimeTick;
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
        begin
          TCompute.Sleep(1);
          DoStatus();
        end;
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('full load %d/%d error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('all load done: %d/%d, error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
end;

procedure TZDB2_Th_Engine_Data_Load_Processor.Wait_M(On_Wait: TOn_ZDB2_Th_Engine_Data_Wait_M);
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance;
  tk: TTimeTick;
begin
  tk := GetTimeTick;
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
        begin
          TCompute.Sleep(1);
          DoStatus();
        end;
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('full load %d/%d error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('all load done: %d/%d, error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
end;

procedure TZDB2_Th_Engine_Data_Load_Processor.Wait_P(On_Wait: TOn_ZDB2_Th_Engine_Data_Wait_P);
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Instance;
  tk: TTimeTick;
begin
  tk := GetTimeTick;
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Data_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
        begin
          TCompute.Sleep(1);
          DoStatus();
        end;
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('full load %d/%d error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('all load done: %d/%d, error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
end;

procedure TZDB2_Th_Engine_Position_Load_Instance.Do_Read_Position_Result(var Sender: TZDB2_Th_CMD_Stream_And_State);
begin
  if Sender.State = TCMD_State.csDone then
    begin
      FLoad_Processor.FTh_Pool.Enqueue(Self);
      FLoad_Processor.IO_Thread_Task_Num.UnLock(FLoad_Processor.IO_Thread_Task_Num.LockP^ - 1);
      FLoad_Processor.Loaded_Num.UnLock(FLoad_Processor.Loaded_Num.LockP^ + 1);
    end
  else
    begin
      FLoad_Processor.IO_Thread_Task_Num.UnLock(FLoad_Processor.IO_Thread_Task_Num.LockP^ - 1);
      FLoad_Processor.Error_Num.UnLock(FLoad_Processor.Error_Num.LockP^ + 1);
      DelayFreeObj(1.0, Self);
    end;
end;

constructor TZDB2_Th_Engine_Position_Load_Instance.Create(Load_Processor_: TZDB2_Th_Engine_Position_Load_Processor; Data_: TZDB2_Th_Engine_Data);
begin
  inherited Create;
  FStream := TMS64.CustomCreate(8192);
  FLoad_Processor := Load_Processor_;
  FData := Data_;
  FOnRun_C := nil;
  FOnRun_M := nil;
  FOnRun_P := nil;
end;

destructor TZDB2_Th_Engine_Position_Load_Instance.Destroy;
begin
  DisposeObject(FStream);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Position_Load_Instance.Process;
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
  end;
end;

procedure TZDB2_Th_Engine_Position_Load_Processor.Do_Thread_Run();
var
  i: Int64;
  Load_Inst_: TZDB2_Th_Engine_Position_Load_Instance;
begin
  i := 0;
  while i < tatal_data_num_ do
    begin
      try
        if (buff^[i]^.Data <> nil) and (buff^[i]^.Data.Can_Load) then
          begin
            Load_Inst_ := TZDB2_Th_Engine_Position_Load_Instance.Create(Self, buff^[i]^.Data);
            Load_Inst_.FOnRun_C := OnRun_C;
            Load_Inst_.FOnRun_M := OnRun_M;
            Load_Inst_.FOnRun_P := OnRun_P;
            Load_Inst_.FData.Engine.Async_Get_Position_Data_As_Stream_M(Load_Inst_.FStream,
              Load_Inst_.FData.ID, Position_Offset, Position_ReadSize, Load_Inst_.Do_Read_Position_Result);
            IO_Thread_Task_Num.UnLock(IO_Thread_Task_Num.LockP^ + 1);
          end;
        while IO_Thread_Task_Num.V > Max_Wait_Task_Num do
            TCompute.Sleep(10);
      except
      end;
      Inc(i);
    end;
  while (IO_Thread_Task_Num.V + FTh_Pool.Count > 0) do
      TCompute.Sleep(10);
  Task_Is_Run := False;
end;

constructor TZDB2_Th_Engine_Position_Load_Processor.Create(ThNum_: Integer);
begin
  inherited Create;
  tatal_data_num_ := 0;
  buff := nil;
  Position_Offset := 0;
  Position_ReadSize := 0;
  Max_Wait_Task_Num := 1000;
  IO_Thread_Task_Num := TAtomInt64.Create(0);
  Loaded_Num := TAtomInt64.Create(0);
  Error_Num := TAtomInt64.Create(0);
  Task_Is_Run := False;
  OnRun_C := nil;
  OnRun_M := nil;
  OnRun_P := nil;

{$IFDEF Enabled_ZDB2_Load_Thread}
  if ThNum_ < 2 then
      FTh_Pool := TIO_Direct.Create()
  else
      FTh_Pool := TIO_Thread.Create(ThNum_);
{$ELSE Enabled_ZDB2_Load_Thread}
  FTh_Pool := TIO_Direct.Create();
{$ENDIF Enabled_ZDB2_Load_Thread}
end;

destructor TZDB2_Th_Engine_Position_Load_Processor.Destroy;
begin
  DisposeObject(FTh_Pool);
  if buff <> nil then
      System.FreeMemory(buff);
  DisposeObject(IO_Thread_Task_Num);
  DisposeObject(Loaded_Num);
  DisposeObject(Error_Num);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Position_Load_Processor.Run();
begin
  Task_Is_Run := True;
  TCompute.RunM_NP(Do_Thread_Run);
end;

procedure TZDB2_Th_Engine_Position_Load_Processor.Wait();
var
  Load_Inst_: TZDB2_Th_Engine_Position_Load_Instance;
  tk: TTimeTick;
begin
  tk := GetTimeTick;
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Position_Load_Instance(FTh_Pool.Dequeue);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
        begin
          TCompute.Sleep(1);
          DoStatus();
        end;
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('Position load %d/%d error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('all Position load done: %d/%d, error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
end;

procedure TZDB2_Th_Engine_Position_Load_Processor.Wait_C(On_Wait: TOn_ZDB2_Th_Engine_Position_Wait_C);
var
  Load_Inst_: TZDB2_Th_Engine_Position_Load_Instance;
  tk: TTimeTick;
begin
  tk := GetTimeTick;
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Position_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
        begin
          TCompute.Sleep(1);
          DoStatus();
        end;
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('Position load %d/%d error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('all Position load done: %d/%d, error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
end;

procedure TZDB2_Th_Engine_Position_Load_Processor.Wait_M(On_Wait: TOn_ZDB2_Th_Engine_Position_Wait_M);
var
  Load_Inst_: TZDB2_Th_Engine_Position_Load_Instance;
  tk: TTimeTick;
begin
  tk := GetTimeTick;
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Position_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
        begin
          TCompute.Sleep(1);
          DoStatus();
        end;
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('Position load %d/%d error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('all Position load done: %d/%d, error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
end;

procedure TZDB2_Th_Engine_Position_Load_Processor.Wait_P(On_Wait: TOn_ZDB2_Th_Engine_Position_Wait_P);
var
  Load_Inst_: TZDB2_Th_Engine_Position_Load_Instance;
  tk: TTimeTick;
begin
  tk := GetTimeTick;
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Position_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
        begin
          TCompute.Sleep(1);
          DoStatus();
        end;
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('Position load %d/%d error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('all Position load done: %d/%d, error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
end;

procedure TZDB2_Th_Engine_Block_Load_Instance.Do_Read_Block_Result(var Sender: TZDB2_Th_CMD_Mem64_And_State);
begin
  if Sender.State = TCMD_State.csDone then
    begin
      FLoad_Processor.FTh_Pool.Enqueue(Self);
      FLoad_Processor.IO_Thread_Task_Num.UnLock(FLoad_Processor.IO_Thread_Task_Num.LockP^ - 1);
      FLoad_Processor.Loaded_Num.UnLock(FLoad_Processor.Loaded_Num.LockP^ + 1);
    end
  else
    begin
      FLoad_Processor.IO_Thread_Task_Num.UnLock(FLoad_Processor.IO_Thread_Task_Num.LockP^ - 1);
      FLoad_Processor.Error_Num.UnLock(FLoad_Processor.Error_Num.LockP^ + 1);
      DelayFreeObj(1.0, Self);
    end;
end;

constructor TZDB2_Th_Engine_Block_Load_Instance.Create(Load_Processor_: TZDB2_Th_Engine_Block_Load_Processor; Data_: TZDB2_Th_Engine_Data);
begin
  inherited Create;
  FMem := TMem64.CustomCreate(8192);
  FLoad_Processor := Load_Processor_;
  FData := Data_;
  FOnRun_C := nil;
  FOnRun_M := nil;
  FOnRun_P := nil;
end;

destructor TZDB2_Th_Engine_Block_Load_Instance.Destroy;
begin
  DisposeObject(FMem);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Block_Load_Instance.Process;
begin
  FMem.Position := 0;
  try
    if Assigned(FOnRun_C) then
        FOnRun_C(FData, FMem);
    if Assigned(FOnRun_M) then
        FOnRun_M(FData, FMem);
    if Assigned(FOnRun_P) then
        FOnRun_P(FData, FMem);
  except
  end;
end;

procedure TZDB2_Th_Engine_Block_Load_Processor.Do_Thread_Run();
var
  i: Int64;
  Load_Inst_: TZDB2_Th_Engine_Block_Load_Instance;
begin
  i := 0;
  while i < tatal_data_num_ do
    begin
      try
        if (buff^[i]^.Data <> nil) and (buff^[i]^.Data.Can_Load) then
          begin
            Load_Inst_ := TZDB2_Th_Engine_Block_Load_Instance.Create(Self, buff^[i]^.Data);
            Load_Inst_.FOnRun_C := OnRun_C;
            Load_Inst_.FOnRun_M := OnRun_M;
            Load_Inst_.FOnRun_P := OnRun_P;
            Load_Inst_.FData.Engine.Async_Get_Block_Data_AsMem64_M(Load_Inst_.FMem,
              Load_Inst_.FData.ID, Block_Index, Block_Offset, Block_ReadSize, Load_Inst_.Do_Read_Block_Result);
            IO_Thread_Task_Num.UnLock(IO_Thread_Task_Num.LockP^ + 1);
          end;
        while IO_Thread_Task_Num.V > Max_Wait_Task_Num do
            TCompute.Sleep(10);
      except
      end;
      Inc(i);
    end;
  while (IO_Thread_Task_Num.V + FTh_Pool.Count > 0) do
      TCompute.Sleep(10);
  Task_Is_Run := False;
end;

constructor TZDB2_Th_Engine_Block_Load_Processor.Create(ThNum_: Integer);
begin
  inherited Create;
  tatal_data_num_ := 0;
  buff := nil;
  Block_Index := 0;
  Block_Offset := 0;
  Block_ReadSize := 0;
  Max_Wait_Task_Num := 1000;
  IO_Thread_Task_Num := TAtomInt64.Create(0);
  Loaded_Num := TAtomInt64.Create(0);
  Error_Num := TAtomInt64.Create(0);
  Task_Is_Run := False;
  OnRun_C := nil;
  OnRun_M := nil;
  OnRun_P := nil;

{$IFDEF Enabled_ZDB2_Load_Thread}
  if ThNum_ < 2 then
      FTh_Pool := TIO_Direct.Create()
  else
      FTh_Pool := TIO_Thread.Create(ThNum_);
{$ELSE Enabled_ZDB2_Load_Thread}
  FTh_Pool := TIO_Direct.Create();
{$ENDIF Enabled_ZDB2_Load_Thread}
end;

destructor TZDB2_Th_Engine_Block_Load_Processor.Destroy;
begin
  DisposeObject(FTh_Pool);
  if buff <> nil then
      System.FreeMemory(buff);
  DisposeObject(IO_Thread_Task_Num);
  DisposeObject(Loaded_Num);
  DisposeObject(Error_Num);
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Block_Load_Processor.Run();
begin
  Task_Is_Run := True;
  TCompute.RunM_NP(Do_Thread_Run);
end;

procedure TZDB2_Th_Engine_Block_Load_Processor.Wait();
var
  Load_Inst_: TZDB2_Th_Engine_Block_Load_Instance;
  tk: TTimeTick;
begin
  tk := GetTimeTick;
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Block_Load_Instance(FTh_Pool.Dequeue);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
        begin
          TCompute.Sleep(1);
          DoStatus();
        end;
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('block load %d/%d error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('all block load done: %d/%d, error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
end;

procedure TZDB2_Th_Engine_Block_Load_Processor.Wait_C(On_Wait: TOn_ZDB2_Th_Engine_Block_Wait_C);
var
  Load_Inst_: TZDB2_Th_Engine_Block_Load_Instance;
  tk: TTimeTick;
begin
  tk := GetTimeTick;
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Block_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
        begin
          TCompute.Sleep(1);
          DoStatus();
        end;
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('block load %d/%d error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('all block load done: %d/%d, error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
end;

procedure TZDB2_Th_Engine_Block_Load_Processor.Wait_M(On_Wait: TOn_ZDB2_Th_Engine_Block_Wait_M);
var
  Load_Inst_: TZDB2_Th_Engine_Block_Load_Instance;
  tk: TTimeTick;
begin
  tk := GetTimeTick;
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Block_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
        begin
          TCompute.Sleep(1);
          DoStatus();
        end;
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('block load %d/%d error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('all block load done: %d/%d, error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
end;

procedure TZDB2_Th_Engine_Block_Load_Processor.Wait_P(On_Wait: TOn_ZDB2_Th_Engine_Block_Wait_P);
var
  Load_Inst_: TZDB2_Th_Engine_Block_Load_Instance;
  tk: TTimeTick;
begin
  tk := GetTimeTick;
  while Task_Is_Run do
    begin
      Load_Inst_ := TZDB2_Th_Engine_Block_Load_Instance(FTh_Pool.Dequeue);
      if Assigned(On_Wait) then
          On_Wait(Load_Inst_);
      if Load_Inst_ <> nil then
          DisposeObject(Load_Inst_)
      else
        begin
          TCompute.Sleep(1);
          DoStatus();
        end;
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('block load %d/%d error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('all block load done: %d/%d, error:%d', [Loaded_Num.V, tatal_data_num_, Error_Num.V]);
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

procedure TZDB2_Th_Engine_Marshal.Do_Remove_First_Data_From_ThEngine(eng: TZDB2_Th_Engine; Recycle_Space_Size: Int64);
var
  sour_size: Int64;
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Data_BigList___.PQueueArrayStruct;
  i, removed_Size, tmp: Int64;
  Can_Load: Boolean;
begin
  if eng.Owner <> Self then
      exit;
  if Recycle_Space_Size <= 0 then
      exit;
  if eng.Th_Engine_Data_Pool.num <= 0 then
      exit;

  Lock;
  eng.Th_Engine_Data_Pool.Lock;
  sour_size := eng.Engine.CoreSpace_Size;
  tatal_data_num_ := eng.Th_Engine_Data_Pool.num;
  buff := eng.Th_Engine_Data_Pool.BuildArrayMemory();
  eng.Th_Engine_Data_Pool.UnLock;
  UnLock;

  removed_Size := 0;
  i := 0;
  while (i < tatal_data_num_) and (removed_Size < Recycle_Space_Size) do
    begin
      try
        if (buff^[i]^.Data <> nil) then
          begin
            buff^[i]^.Data.Lock;
            Can_Load := buff^[i]^.Data.Can_Load and buff^[i]^.Data.First_Operation_Ready;
            buff^[i]^.Data.UnLock;
            if Can_Load then
              begin
                tmp := buff^[i]^.Data.DataSize;
                if buff^[i]^.Data.Remove() then
                    Inc(removed_Size, tmp);
              end;
          end;
      except
      end;
      Inc(i);
    end;

  System.FreeMemory(buff);

{$IFDEF DEBUG}
  DoStatus('%s Recycle Space %s -> %s ', [if_(eng.Database_File = '', '(memory)', umlGetFileName(eng.Database_File).Text),
    umlSizeToStr(sour_size).Text, umlSizeToStr(sour_size - removed_Size).Text]);
{$ENDIF DEBUG}
end;

constructor TZDB2_Th_Engine_Marshal.Create(Owner_: TCore_Object);
begin
  inherited Create;
  Owner := Owner_;
  FCritical := TCritical.Create;
  FLong_Loop_Num := 0;
  Data_Marshal := TZDB2_Th_Engine_Marshal_BigList___.Create;
  Data_Marshal.OnFree := DoFree;
  Engine_Pool := TZDB2_Th_Engine_Pool.Create;
  Instance_Recycle_Tool := TZDB2_Th_Engine_Data_Instance_Recycle_Tool___.Create;
  Data_Link_Recycle_Tool := TZDB2_Th_Engine_Data_Link_Recycle_Tool___.Create;
  Current_Data_Class := TZDB2_Th_Engine_Data;
  Pool_Ptr := Th_Engine_Marshal_Pool__.Add(Self);
end;

destructor TZDB2_Th_Engine_Marshal.Destroy;
begin
  try
    // stop all copy task.
    Stop_Copy;
    // flush now.
    Flush(True);
    // temp data swap technology
    // When the data is in a long loop, it is not appended to the data structure, but stored in the underlying ZDB2 database and Temp_Swap_Pool.
    // after the long loop ends, the data will truly become a engine structure
    if Engine_Pool.num > 0 then
      with Engine_Pool.Repeat_ do
        repeat
          try
              Queue^.Data.Flush_Temp_Swap_Pool;
          except
          end;
        until not Next;
    // free link
    if Data_Link_Recycle_Tool.num > 0 then
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
    if Engine_Pool.num > 0 then
      with Engine_Pool.Repeat_ do
        repeat
          try
            Queue^.Data.Th_Engine_Data_Pool.Lock;
            Queue^.Data.Th_Engine_Data_Pool.Free_Recycle_Pool;
            Queue^.Data.Th_Engine_Data_Pool.UnLock;
          except
          end;
        until not Next;
    // free local recycle pool
    try
      Data_Marshal.Lock;
      Data_Marshal.Free_Recycle_Pool;
      Data_Marshal.UnLock;
    except
    end;
    // free pool
    disposeObjectAndNil(Engine_Pool);
    disposeObjectAndNil(Data_Marshal);
    // free data instance
    if Instance_Recycle_Tool.num > 0 then
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

procedure TZDB2_Th_Engine_Marshal.Do_Add_Data(Sender: TZDB2_Th_Engine_Data);
begin

end;

procedure TZDB2_Th_Engine_Marshal.Do_Remove_Data(Sender: TZDB2_Th_Engine_Data);
begin

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
  if Engine_Pool.num > 0 then
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
  if Engine_Pool.num > 0 then
    begin
      with Engine_Pool.Repeat_ do
        repeat
          if Queue^.Data.Engine <> nil then
              Inc(ready_num);
        until not Next;
    end;
  Result := (Engine_Pool.num > 0) and (Engine_Pool.num = ready_num);
end;

procedure TZDB2_Th_Engine_Marshal.Update_Data_Ptr();
begin
  Lock;
  try
    // update FOwner_Data_Ptr
    Data_Marshal.Lock;
    if Data_Marshal.num > 0 then
      with Data_Marshal.Repeat_ do
        repeat
          Queue^.Data.FOwner := Self;
          Queue^.Data.FOwner_Data_Ptr := Queue;
        until not Next;
    Data_Marshal.UnLock;

    // update FTh_Engine_Data_Ptr
    if Engine_Pool.num > 0 then
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
  Flush(True);
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
  if Engine_Pool.num > 0 then
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
  if Engine_Pool.num > 0 then
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
  Result := Data_Marshal.num;
end;

function TZDB2_Th_Engine_Marshal.QueueNum: NativeInt;
begin
  Result := 0;
  if Engine_Pool.num > 0 then
    begin
      with Engine_Pool.Repeat_ do
        repeat
          if Queue^.Data.Engine <> nil then
              Inc(Result, Queue^.Data.Engine.QueueNum);
        until not Next;
    end;
end;

function TZDB2_Th_Engine_Marshal.Fragment_Buffer_Num: Int64;
begin
  Result := 0;
  if Engine_Pool.num > 0 then
    begin
      with Engine_Pool.Repeat_ do
        repeat
          if Queue^.Data.Engine <> nil then
              Inc(Result, Queue^.Data.Fragment_Buffer_Num);
        until not Next;
    end;
end;

function TZDB2_Th_Engine_Marshal.Fragment_Buffer_Memory: Int64;
begin
  Result := 0;
  if Engine_Pool.num > 0 then
    begin
      with Engine_Pool.Repeat_ do
        repeat
          if Queue^.Data.Engine <> nil then
              Inc(Result, Queue^.Data.Fragment_Buffer_Memory);
        until not Next;
    end;
end;

function TZDB2_Th_Engine_Marshal.Add_Data_To_Minimize_Workload_Engine(): TZDB2_Th_Engine_Data;
var
  Eng_: TZDB2_Th_Engine;
begin
  Result := nil;
  try
    Eng_ := Engine_Pool.Get_Minimize_Workload_Engine;
    if Eng_ <> nil then
        Result := Eng_.Add(Current_Data_Class);
  except
  end;
end;

function TZDB2_Th_Engine_Marshal.Add_Data_To_Minimize_Size_Engine(): TZDB2_Th_Engine_Data;
var
  Eng_: TZDB2_Th_Engine;
begin
  Result := nil;
  try
    Eng_ := Engine_Pool.Get_Minimize_Size_Engine;
    if Eng_ <> nil then
        Result := Eng_.Add(Current_Data_Class);
  except
  end;
end;

function TZDB2_Th_Engine_Marshal.Add_Data_To_Engine(Eng_: TZDB2_Th_Engine): TZDB2_Th_Engine_Data;
begin
  Result := nil;
  try
      Result := Eng_.Add(Current_Data_Class)
  except
  end;
end;

procedure TZDB2_Th_Engine_Marshal.Wait_Busy_Task;
begin
  while QueueNum + FLong_Loop_Num > 0 do
      TCompute.Sleep(10);
end;

procedure TZDB2_Th_Engine_Marshal.Wait_Long_Loop(wait_backup_: Boolean);
var
  backup_task_num: NativeInt;
begin
  repeat
    backup_task_num := 0;
    if wait_backup_ and (Engine_Pool.num > 0) then
      begin
        Lock;
        try
          with Engine_Pool.Repeat_ do
            repeat
              if Queue^.Data.FCopy_Is_Busy then
                  Inc(backup_task_num);
            until not Next;
        except
        end;
        UnLock;
      end;
    if (Long_Loop_Num > 0) or (backup_task_num > 0) then
        TCompute.Sleep(100);
  until (Long_Loop_Num <= 0) and (backup_task_num <= 0);
end;

procedure TZDB2_Th_Engine_Marshal.Check_Recycle_Pool;
begin
  Lock;
  if (FLong_Loop_Num <= 0) then
    begin
      AtomInc(FLong_Loop_Num);

      // link recycle
      Data_Link_Recycle_Tool.Lock;
      if Data_Link_Recycle_Tool.num > 0 then
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
      if Engine_Pool.num > 0 then
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
        if Instance_Recycle_Tool.num > 0 then
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

      // temp data swap technology
      // When the data is in a long loop, it is not appended to the data structure, but stored in the underlying ZDB2 database and Temp_Swap_Pool.
      // after the long loop ends, the data will truly become a engine structure
      if Engine_Pool.num > 0 then
        with Engine_Pool.Repeat_ do
          repeat
            try
                Queue^.Data.Flush_Temp_Swap_Pool;
            except
            end;
          until not Next;

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
        if Engine_Pool.num > 0 then
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

function TZDB2_Th_Engine_Marshal.Get_Last_Backup_Execute_Time: TTimeTick;
begin
  if Engine_Pool.num > 0 then
    begin
      Result := 0;
      Lock;
      try
        with Engine_Pool.Repeat_ do
          repeat
            if Queue^.Data.Get_Last_Backup_Execute_Time > Result then
                Result := Queue^.Data.Get_Last_Backup_Execute_Time;
          until not Next;
      except
      end;
      UnLock;
    end
  else
      Result := GetTimeTick;
end;

procedure TZDB2_Th_Engine_Marshal.Backup(Reserve_: Word);
begin
  if Engine_Pool.num > 0 then
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
end;

procedure TZDB2_Th_Engine_Marshal.Backup_If_No_Exists;
begin
  if Engine_Pool.num > 0 then
    begin
      Lock;
      try
        with Engine_Pool.Repeat_ do
          repeat
            try
              if (not Queue^.Data.Found_Backup) and (not Queue^.Data.FCopy_Is_Busy) then
                  Queue^.Data.Backup(1);
            except
            end;
          until not Next;
      except
      end;
      UnLock;
    end;
end;

procedure TZDB2_Th_Engine_Marshal.Stop_Backup;
begin
  if Engine_Pool.num > 0 then
    begin
      Lock;
      try
        with Engine_Pool.Repeat_ do
          repeat
            try
                Queue^.Data.Stop_Backup;
            except
            end;
          until not Next;
      except
      end;
      UnLock;
    end;
end;

procedure TZDB2_Th_Engine_Marshal.Remove_Backup;
begin
  if Engine_Pool.num > 0 then
    begin
      Lock;
      try
        with Engine_Pool.Repeat_ do
          repeat
            try
                Queue^.Data.Remove_Backup;
            except
            end;
          until not Next;
      except
      end;
      UnLock;
    end;
end;

procedure TZDB2_Th_Engine_Marshal.Stop_Copy;
begin
  if Engine_Pool.num > 0 then
    begin
      Lock;
      try
        with Engine_Pool.Repeat_ do
          repeat
            try
                Queue^.Data.Stop_Copy;
            except
            end;
          until not Next;
      except
      end;
      UnLock;
    end;
end;

procedure TZDB2_Th_Engine_Marshal.Prepare_Flush_External_Header(Th_Engine_: TZDB2_Th_Engine; var Sequence_Table: TZDB2_BlockHandle; Flush_Instance_Pool: TZDB2_Th_Engine_Data_Instance_Pool; External_Header_Data_: TMem64);
begin

end;

procedure TZDB2_Th_Engine_Marshal.Flush;
begin
  Flush(False);
end;

procedure TZDB2_Th_Engine_Marshal.Flush(WaitQueue_: Boolean);
begin
  if WaitQueue_ then
      Wait_Busy_Task;
  Check_Recycle_Pool;
  Lock;
  if Engine_Pool.num > 0 then
    begin
      try
        with Engine_Pool.Repeat_ do
          repeat
            try
                Queue^.Data.Flush(WaitQueue_);
            except
            end;
          until not Next;
      except
      end;
    end;
  UnLock;
  if WaitQueue_ then
      Wait_Busy_Task;
  Check_Recycle_Pool;
end;

function TZDB2_Th_Engine_Marshal.Flush_Is_Busy: Boolean;
begin
  Result := False;
  Lock;
  if Engine_Pool.num > 0 then
    begin
      try
        with Engine_Pool.Repeat_ do
          repeat
              Result := Result or (Queue^.Data.Flush_Activted_Num > 0);
          until not Next;
      except
      end;
    end;
  UnLock;
end;

procedure TZDB2_Th_Engine_Marshal.Format_Database;
begin
  Wait_Busy_Task;
  Check_Recycle_Pool;
  Lock;
  try
    Data_Marshal.Clear;
    if Engine_Pool.num > 0 then
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

procedure TZDB2_Th_Engine_Marshal.Parallel_Load_C(ThNum_: Integer; On_Run: TOn_ZDB2_Th_Engine_Data_Event_C; On_Wait: TOn_ZDB2_Th_Engine_Data_Wait_C);
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Processor;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;

  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  Load_Inst_ := TZDB2_Th_Engine_Data_Load_Processor.Create(umlMin(Data_Marshal.num shr 4, ThNum_));
  Load_Inst_.tatal_data_num_ := Data_Marshal.num;
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

procedure TZDB2_Th_Engine_Marshal.Parallel_Load_M(ThNum_: Integer; On_Run: TOn_ZDB2_Th_Engine_Data_Event_M; On_Wait: TOn_ZDB2_Th_Engine_Data_Wait_M);
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Processor;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;

  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  Load_Inst_ := TZDB2_Th_Engine_Data_Load_Processor.Create(umlMin(Data_Marshal.num shr 4, ThNum_));
  Load_Inst_.tatal_data_num_ := Data_Marshal.num;
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

procedure TZDB2_Th_Engine_Marshal.Parallel_Load_P(ThNum_: Integer; On_Run: TOn_ZDB2_Th_Engine_Data_Event_P; On_Wait: TOn_ZDB2_Th_Engine_Data_Wait_P);
var
  Load_Inst_: TZDB2_Th_Engine_Data_Load_Processor;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;

  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  Load_Inst_ := TZDB2_Th_Engine_Data_Load_Processor.Create(umlMin(Data_Marshal.num shr 4, ThNum_));
  Load_Inst_.tatal_data_num_ := Data_Marshal.num;
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

procedure TZDB2_Th_Engine_Marshal.Parallel_Block_Load_C(ThNum_, Block_Index, Block_Offset, Block_Read_Size: Integer; On_Run: TOn_ZDB2_Th_Engine_Block_Event_C; On_Wait: TOn_ZDB2_Th_Engine_Block_Wait_C);
var
  Load_Inst_: TZDB2_Th_Engine_Block_Load_Processor;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;

  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  Load_Inst_ := TZDB2_Th_Engine_Block_Load_Processor.Create(umlMin(Data_Marshal.num shr 4, ThNum_));
  Load_Inst_.tatal_data_num_ := Data_Marshal.num;
  Load_Inst_.buff := Data_Marshal.BuildArrayMemory();
  Load_Inst_.Block_Index := Block_Index;
  Load_Inst_.Block_Offset := Block_Offset;
  Load_Inst_.Block_ReadSize := Block_Read_Size;
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

procedure TZDB2_Th_Engine_Marshal.Parallel_Block_Load_M(ThNum_, Block_Index, Block_Offset, Block_Read_Size: Integer; On_Run: TOn_ZDB2_Th_Engine_Block_Event_M; On_Wait: TOn_ZDB2_Th_Engine_Block_Wait_M);
var
  Load_Inst_: TZDB2_Th_Engine_Block_Load_Processor;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;

  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  Load_Inst_ := TZDB2_Th_Engine_Block_Load_Processor.Create(umlMin(Data_Marshal.num shr 4, ThNum_));
  Load_Inst_.tatal_data_num_ := Data_Marshal.num;
  Load_Inst_.buff := Data_Marshal.BuildArrayMemory();
  Load_Inst_.Block_Index := Block_Index;
  Load_Inst_.Block_Offset := Block_Offset;
  Load_Inst_.Block_ReadSize := Block_Read_Size;
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

procedure TZDB2_Th_Engine_Marshal.Parallel_Block_Load_P(ThNum_, Block_Index, Block_Offset, Block_Read_Size: Integer; On_Run: TOn_ZDB2_Th_Engine_Block_Event_P; On_Wait: TOn_ZDB2_Th_Engine_Block_Wait_P);
var
  Load_Inst_: TZDB2_Th_Engine_Block_Load_Processor;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;

  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  Load_Inst_ := TZDB2_Th_Engine_Block_Load_Processor.Create(umlMin(Data_Marshal.num shr 4, ThNum_));
  Load_Inst_.tatal_data_num_ := Data_Marshal.num;
  Load_Inst_.buff := Data_Marshal.BuildArrayMemory();
  Load_Inst_.Block_Index := Block_Index;
  Load_Inst_.Block_Offset := Block_Offset;
  Load_Inst_.Block_ReadSize := Block_Read_Size;
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

procedure TZDB2_Th_Engine_Marshal.Parallel_Position_Load_C(ThNum_: Integer; Position_Offset, Position_Read_Size: Int64; On_Run: TOn_ZDB2_Th_Engine_Position_Event_C; On_Wait: TOn_ZDB2_Th_Engine_Position_Wait_C);
var
  Load_Inst_: TZDB2_Th_Engine_Position_Load_Processor;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;

  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  Load_Inst_ := TZDB2_Th_Engine_Position_Load_Processor.Create(umlMin(Data_Marshal.num shr 4, ThNum_));
  Load_Inst_.tatal_data_num_ := Data_Marshal.num;
  Load_Inst_.buff := Data_Marshal.BuildArrayMemory();
  Load_Inst_.Position_Offset := Position_Offset;
  Load_Inst_.Position_ReadSize := Position_Read_Size;
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

procedure TZDB2_Th_Engine_Marshal.Parallel_Position_Load_M(ThNum_: Integer; Position_Offset, Position_Read_Size: Int64; On_Run: TOn_ZDB2_Th_Engine_Position_Event_M; On_Wait: TOn_ZDB2_Th_Engine_Position_Wait_M);
var
  Load_Inst_: TZDB2_Th_Engine_Position_Load_Processor;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;

  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  Load_Inst_ := TZDB2_Th_Engine_Position_Load_Processor.Create(umlMin(Data_Marshal.num shr 4, ThNum_));
  Load_Inst_.tatal_data_num_ := Data_Marshal.num;
  Load_Inst_.buff := Data_Marshal.BuildArrayMemory();
  Load_Inst_.Position_Offset := Position_Offset;
  Load_Inst_.Position_ReadSize := Position_Read_Size;
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

procedure TZDB2_Th_Engine_Marshal.Parallel_Position_Load_P(ThNum_: Integer; Position_Offset, Position_Read_Size: Int64; On_Run: TOn_ZDB2_Th_Engine_Position_Event_P; On_Wait: TOn_ZDB2_Th_Engine_Position_Wait_P);
var
  Load_Inst_: TZDB2_Th_Engine_Position_Load_Processor;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;

  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  Load_Inst_ := TZDB2_Th_Engine_Position_Load_Processor.Create(umlMin(Data_Marshal.num shr 4, ThNum_));
  Load_Inst_.tatal_data_num_ := Data_Marshal.num;
  Load_Inst_.buff := Data_Marshal.BuildArrayMemory();
  Load_Inst_.Position_Offset := Position_Offset;
  Load_Inst_.Position_ReadSize := Position_Read_Size;
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

procedure TZDB2_Th_Engine_Marshal.Parallel_For_C(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_For_C);
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
        Can_Load := inst.Can_Load and inst.First_Operation_Ready;
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
  if Data_Marshal.num <= 0 then
      exit;
  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  tatal_data_num_ := Data_Marshal.num;
  buff := Data_Marshal.BuildArrayMemory();
  Data_Marshal.UnLock;
  UnLock;
  Aborted := False;

{$IFDEF FPC}
  ParallelFor(ThNum_, Parallel_, 0, tatal_data_num_ - 1, fpc_ParallelFor);
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
          Can_Load := inst.Can_Load and inst.First_Operation_Ready;
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

procedure TZDB2_Th_Engine_Marshal.Parallel_For_M(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_For_M);
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
        Can_Load := inst.Can_Load and inst.First_Operation_Ready;
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
  if Data_Marshal.num <= 0 then
      exit;
  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  tatal_data_num_ := Data_Marshal.num;
  buff := Data_Marshal.BuildArrayMemory();
  Data_Marshal.UnLock;
  UnLock;
  Aborted := False;

{$IFDEF FPC}
  ParallelFor(ThNum_, Parallel_, 0, tatal_data_num_ - 1, fpc_ParallelFor);
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
          Can_Load := inst.Can_Load and inst.First_Operation_Ready;
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

procedure TZDB2_Th_Engine_Marshal.Parallel_For_P(Parallel_: Boolean; ThNum_: Integer; On_Run: TZDB2_Th_Engine_For_P);
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
        Can_Load := inst.Can_Load and inst.First_Operation_Ready;
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
  if Data_Marshal.num <= 0 then
      exit;
  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  tatal_data_num_ := Data_Marshal.num;
  buff := Data_Marshal.BuildArrayMemory();
  Data_Marshal.UnLock;
  UnLock;
  Aborted := False;

{$IFDEF FPC}
  ParallelFor(ThNum_, Parallel_, 0, tatal_data_num_ - 1, fpc_ParallelFor);
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
          Can_Load := inst.Can_Load and inst.First_Operation_Ready;
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

procedure TZDB2_Th_Engine_Marshal.For_C(Max_Loop_: Int64; On_Run: TZDB2_Th_Engine_For_C);
var
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Marshal_BigList___.PQueueArrayStruct;
  Aborted: Boolean;
  i: Int64;
  inst: TZDB2_Th_Engine_Data;
  Can_Load: Boolean;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;
  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  tatal_data_num_ := Data_Marshal.num;
  buff := Data_Marshal.BuildArrayMemory();
  Data_Marshal.UnLock;
  UnLock;
  Aborted := False;

  i := 0;
  while i < tatal_data_num_ do
    begin
      try
        inst := buff^[i]^.Data;
        if (inst <> nil) then
          begin
            inst.Lock;
            try
                Can_Load := inst.Can_Load and inst.First_Operation_Ready;
            finally
                inst.UnLock;
            end;

            if Can_Load and (not Aborted) then
                On_Run(inst, i, Aborted);
          end;
      except
          Aborted := True;
      end;

      Inc(i);
      if Aborted or ((Max_Loop_ > 0) and (i >= Max_Loop_)) then
          break;
    end;

  AtomDec(FLong_Loop_Num);
  System.FreeMemory(buff);
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.For_M(Max_Loop_: Int64; On_Run: TZDB2_Th_Engine_For_M);
var
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Marshal_BigList___.PQueueArrayStruct;
  Aborted: Boolean;
  i: Int64;
  inst: TZDB2_Th_Engine_Data;
  Can_Load: Boolean;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;
  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  tatal_data_num_ := Data_Marshal.num;
  buff := Data_Marshal.BuildArrayMemory();
  Data_Marshal.UnLock;
  UnLock;
  Aborted := False;

  i := 0;
  while i < tatal_data_num_ do
    begin
      try
        inst := buff^[i]^.Data;
        if (inst <> nil) then
          begin
            inst.Lock;
            try
                Can_Load := inst.Can_Load and inst.First_Operation_Ready;
            finally
                inst.UnLock;
            end;

            if Can_Load and (not Aborted) then
                On_Run(inst, i, Aborted);
          end;
      except
          Aborted := True;
      end;

      Inc(i);
      if Aborted or ((Max_Loop_ > 0) and (i >= Max_Loop_)) then
          break;
    end;

  AtomDec(FLong_Loop_Num);
  System.FreeMemory(buff);
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.For_P(Max_Loop_: Int64; On_Run: TZDB2_Th_Engine_For_P);
var
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Marshal_BigList___.PQueueArrayStruct;
  Aborted: Boolean;
  i: Int64;
  inst: TZDB2_Th_Engine_Data;
  Can_Load: Boolean;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;
  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  tatal_data_num_ := Data_Marshal.num;
  buff := Data_Marshal.BuildArrayMemory();
  Data_Marshal.UnLock;
  UnLock;
  Aborted := False;

  i := 0;
  while i < tatal_data_num_ do
    begin
      try
        inst := buff^[i]^.Data;
        if (inst <> nil) then
          begin
            inst.Lock;
            try
                Can_Load := inst.Can_Load and inst.First_Operation_Ready;
            finally
                inst.UnLock;
            end;

            if Can_Load and (not Aborted) then
                On_Run(inst, i, Aborted);
          end;
      except
          Aborted := True;
      end;

      Inc(i);
      if Aborted or ((Max_Loop_ > 0) and (i >= Max_Loop_)) then
          break;
    end;

  AtomDec(FLong_Loop_Num);
  System.FreeMemory(buff);
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.Invert_For_C(Max_Loop_: Int64; On_Run: TZDB2_Th_Engine_For_C);
var
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Marshal_BigList___.PQueueArrayStruct;
  Aborted: Boolean;
  i: Int64;
  inst: TZDB2_Th_Engine_Data;
  Can_Load: Boolean;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;
  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  tatal_data_num_ := Data_Marshal.num;
  buff := Data_Marshal.BuildArrayMemory();
  Data_Marshal.UnLock;
  UnLock;
  Aborted := False;

  i := tatal_data_num_ - 1;
  while i >= 0 do
    begin
      try
        inst := buff^[i]^.Data;
        if (inst <> nil) then
          begin
            inst.Lock;
            try
                Can_Load := inst.Can_Load and inst.First_Operation_Ready;
            finally
                inst.UnLock;
            end;

            if Can_Load and (not Aborted) then
                On_Run(inst, i, Aborted);
          end;
      except
          Aborted := True;
      end;

      Dec(i);
      if Aborted or ((Max_Loop_ > 0) and (tatal_data_num_ - i >= Max_Loop_)) then
          break;
    end;

  AtomDec(FLong_Loop_Num);
  System.FreeMemory(buff);
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.Invert_For_M(Max_Loop_: Int64; On_Run: TZDB2_Th_Engine_For_M);
var
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Marshal_BigList___.PQueueArrayStruct;
  Aborted: Boolean;
  i: Int64;
  inst: TZDB2_Th_Engine_Data;
  Can_Load: Boolean;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;
  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  tatal_data_num_ := Data_Marshal.num;
  buff := Data_Marshal.BuildArrayMemory();
  Data_Marshal.UnLock;
  UnLock;
  Aborted := False;

  i := tatal_data_num_ - 1;
  while i >= 0 do
    begin
      try
        inst := buff^[i]^.Data;
        if (inst <> nil) then
          begin
            inst.Lock;
            try
                Can_Load := inst.Can_Load and inst.First_Operation_Ready;
            finally
                inst.UnLock;
            end;

            if Can_Load and (not Aborted) then
                On_Run(inst, i, Aborted);
          end;
      except
          Aborted := True;
      end;

      Dec(i);
      if Aborted or ((Max_Loop_ > 0) and (tatal_data_num_ - i >= Max_Loop_)) then
          break;
    end;

  AtomDec(FLong_Loop_Num);
  System.FreeMemory(buff);
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.Invert_For_P(Max_Loop_: Int64; On_Run: TZDB2_Th_Engine_For_P);
var
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Marshal_BigList___.PQueueArrayStruct;
  Aborted: Boolean;
  i: Int64;
  inst: TZDB2_Th_Engine_Data;
  Can_Load: Boolean;
begin
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;
  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  tatal_data_num_ := Data_Marshal.num;
  buff := Data_Marshal.BuildArrayMemory();
  Data_Marshal.UnLock;
  UnLock;
  Aborted := False;

  i := tatal_data_num_ - 1;
  while i >= 0 do
    begin
      try
        inst := buff^[i]^.Data;
        if (inst <> nil) then
          begin
            inst.Lock;
            try
                Can_Load := inst.Can_Load and inst.First_Operation_Ready;
            finally
                inst.UnLock;
            end;

            if Can_Load and (not Aborted) then
                On_Run(inst, i, Aborted);
          end;
      except
          Aborted := True;
      end;

      Dec(i);
      if Aborted or ((Max_Loop_ > 0) and (tatal_data_num_ - i >= Max_Loop_)) then
          break;
    end;

  AtomDec(FLong_Loop_Num);
  System.FreeMemory(buff);
  Check_Recycle_Pool;
end;

procedure TZDB2_Th_Engine_Marshal.Remove_First_Data(Num_: Int64; remove_data_: Boolean);
var
  tatal_data_num_: Int64;
  buff: TZDB2_Th_Engine_Marshal_BigList___.PQueueArrayStruct;
  i, j: Int64;
  Can_Load: Boolean;
begin
  if FLong_Loop_Num > 0 then
      exit;
  Check_Recycle_Pool;
  if Data_Marshal.num <= 0 then
      exit;
  Lock;
  Data_Marshal.Lock;
  AtomInc(FLong_Loop_Num);
  tatal_data_num_ := Data_Marshal.num;
  buff := Data_Marshal.BuildArrayMemory();
  Data_Marshal.UnLock;
  UnLock;

  try
    i := 0;
    j := 0;
    while (i < tatal_data_num_) and (j < Num_) do
      begin
        try
          if buff^[i]^.Data <> nil then
            begin
              buff^[i]^.Data.Lock;
              Can_Load := buff^[i]^.Data.Can_Load and buff^[i]^.Data.First_Operation_Ready;
              buff^[i]^.Data.UnLock;
              if Can_Load and buff^[i]^.Data.Remove(remove_data_) then
                  Inc(j);
            end;
        except
        end;
        Inc(i);
      end;

  finally
    AtomDec(FLong_Loop_Num);
    System.FreeMemory(buff);
    Check_Recycle_Pool;
  end;
end;

procedure TZDB2_Th_Engine_Marshal.Remove_First_Data_For_All_Th_Engine(Th_Engine_Max_Space_Size: Int64);
begin
  if FLong_Loop_Num > 0 then
      exit;
  if Flush_Is_Busy() then
      exit;
  if Engine_Pool.num > 0 then
    begin
      Check_Recycle_Pool;
      AtomInc(FLong_Loop_Num);
      try
        with Engine_Pool.Repeat_ do
          repeat
            if Queue^.Data.FFlush_Activted_Num <= 0 then
              if Queue^.Data.Engine.CoreSpace_Size > Th_Engine_Max_Space_Size then
                  Do_Remove_First_Data_From_ThEngine(Queue^.Data, Queue^.Data.Engine.CoreSpace_Size - Th_Engine_Max_Space_Size);
          until not Next;
      finally
          AtomDec(FLong_Loop_Num);
      end;
      Check_Recycle_Pool;
    end;
end;

procedure TZDB2_Th_Engine_Marshal.Begin_Loop;
begin
  Check_Recycle_Pool;
  AtomInc(FLong_Loop_Num);
end;

function TZDB2_Th_Engine_Marshal.Repeat_: TZDB2_Th_Engine_Marshal_BigList___.TRepeat___;
begin
  Result := Data_Marshal.Repeat_;
end;

function TZDB2_Th_Engine_Marshal.Invert_Repeat_: TZDB2_Th_Engine_Marshal_BigList___.TInvert_Repeat___;
begin
  Result := Data_Marshal.Invert_Repeat_;
end;

procedure TZDB2_Th_Engine_Marshal.End_Loop;
begin
  AtomDec(FLong_Loop_Num);
  Check_Recycle_Pool;
end;

function TZDB2_Th_Engine_Marshal.GetRemoveDatabaseOnDestroy: Boolean;
begin
  Result := False;
  if Engine_Pool.num > 0 then
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
  if Engine_Pool.num > 0 then
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

function TZDB2_Th_Engine_Marshal.Get_State_Info: U_String;
begin
  Result := '';
  if Engine_Pool.num > 0 then
    with Engine_Pool.Repeat_ do
      repeat
        if Queue^.Data.Engine <> nil then
            Result.Append('%d: %s Data-Num:%d IO-Queue:%d Size:%s/%s Fragment:%d/%s' + #13#10,
            [I__ + 1, umlGetFileName(Queue^.Data.Database_File).Text,
            Queue^.Data.Th_Engine_Data_Pool.num,
            Queue^.Data.Engine.QueueNum,
            umlSizeToStr(Queue^.Data.Engine.CoreSpace_Size).Text,
            umlSizeToStr(Queue^.Data.Engine.CoreSpace_Physics_Size).Text,
            Queue^.Data.Engine.Fragment_Buffer_Num,
            umlSizeToStr(Queue^.Data.Engine.Fragment_Buffer_Memory).Text
            ]);
      until not Next;

  Result.Append('Total Data/Queue:%d/%d'#13#10, [Total, QueueNum]);
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
  te: THashTextEngine;
  L: TListPascalString;
  Eng_: TZDB2_Th_Engine;
  i: Integer;
  tmp: TMem64;
begin
  DM := TZDB2_Th_Engine_Marshal.Create(nil);
  te := THashTextEngine.Create;
  te.AsText := C_cfg;

  L := TListPascalString.Create;
  te.GetSectionList(L);
  for i := 0 to L.Count - 1 do
    begin
      Eng_ := TZDB2_Th_Engine.Create(DM);
      Eng_.ReadConfig(L[i], te.HStringList[L[i]]);
    end;
  DisposeObject(L);
  te.Free;
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

  DM.Parallel_Block_Load_C(4, 0, 0, 16, nil, nil);
  DM.Parallel_Load_C(4, nil, nil);

  DoStatus('db total:%d', [DM.Total]);

  DisposeObject(DM);
end;

class procedure TZDB2_Th_Engine_Marshal.Test_Backup_Support;
const
  C_cfg = '[1]'#13#10 +
    'database=%temp%db1.ox2'#13#10 +
    'OnlyRead=False'#13#10 +
    'First_Inited_Physics_Space=100*1024*1024'#13#10 +
    'Fast_Alloc_Space=True'#13#10 +
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
    'First_Inited_Physics_Space=100*1024*1024'#13#10 +
    'Fast_Alloc_Space=True'#13#10 +
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
    'First_Inited_Physics_Space=100*1024*1024'#13#10 +
    'Fast_Alloc_Space=True'#13#10 +
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
    'First_Inited_Physics_Space=100*1024*1024'#13#10 +
    'Fast_Alloc_Space=True'#13#10 +
    'Delta=100*1024*1024'#13#10 +
    'BlockSize=1536'#13#10 +
    'Security=None'#13#10 +
    'Password=ZDB_2.0'#13#10 +
    'Level=1'#13#10 +
    'Tail=True'#13#10 +
    'CBC=True'#13#10;
var
  DM: TZDB2_Th_Engine_Marshal;
  te: THashTextEngine;
  L: TListPascalString;
  Eng_: TZDB2_Th_Engine;
  i: Integer;
  tmp: TMem64;
begin
  DM := TZDB2_Th_Engine_Marshal.Create(nil);
  te := THashTextEngine.Create;
  te.AsText := umlReplace(C_cfg, '%temp%', umlCurrentPath, False, True);

  L := TListPascalString.Create;
  te.GetSectionList(L);
  for i := 0 to L.Count - 1 do
    begin
      Eng_ := TZDB2_Th_Engine.Create(DM);
      Eng_.ReadConfig(L[i], te.HStringList[L[i]]);
    end;
  DisposeObject(L);
  te.Free;
  DM.Build;
  DM.RemoveDatabaseOnDestroy := True;

  DM.Engine_Pool.Get_Minimize_Size_Engine;

  for i := 0 to 20000 do
    begin
      tmp := TMem64.Create;
      tmp.Size := umlRandomRange(1192, 8192);
      MT19937Rand32(MaxInt, tmp.Memory, tmp.Size shr 2);
      DM.Add_Data_To_Minimize_Workload_Engine.Async_Save_And_Free_Data(tmp);
      while DM.QueueNum > 1000 do
          TCompute.Sleep(1);
    end;

  DM.Flush;
  DM.Wait_Busy_Task;
  DM.Parallel_Load_C(4, nil, nil);
  DM.Wait_Busy_Task;

  for i := 0 to 10 do
    begin
      DM.Backup(3);
      DM.Wait_Busy_Task;
      DM.Wait_Long_Loop(True);
      TCompute.Sleep(1000);
    end;
  DoStatus('db total:%d', [DM.Total]);

  DM.Wait_Long_Loop(True);
  DM.Remove_Backup;
  DisposeObject(DM);
end;

class procedure TZDB2_Th_Engine_Marshal.Test_Remove_First_Data_Support;
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
  te: THashTextEngine;
  L: TListPascalString;
  Eng_: TZDB2_Th_Engine;
  i: Integer;
  tmp: TMem64;
begin
  DM := TZDB2_Th_Engine_Marshal.Create(nil);
  te := THashTextEngine.Create;
  te.AsText := C_cfg;

  L := TListPascalString.Create;
  te.GetSectionList(L);
  for i := 0 to L.Count - 1 do
    begin
      Eng_ := TZDB2_Th_Engine.Create(DM);
      Eng_.ReadConfig(L[i], te.HStringList[L[i]]);
    end;
  DisposeObject(L);
  te.Free;
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

  DM.Wait_Busy_Task;
  DoStatus('db total:%d', [DM.Total]);
  with DM.Engine_Pool.Repeat_ do
    repeat
        DoStatus('engine(%d) size: %s', [I__, umlSizeToStr(Queue^.Data.Engine.CoreSpace_Size).Text]);
    until not Next;
  DM.Wait_Busy_Task;

  DoStatus('recycle space.');
  DM.Remove_First_Data_For_All_Th_Engine(8 * 1024 * 1024);
  DM.Wait_Busy_Task;

  with DM.Engine_Pool.Repeat_ do
    repeat
        DoStatus('engine(%d) size: %s', [I__, umlSizeToStr(Queue^.Data.Engine.CoreSpace_Size).Text]);
    until not Next;

  DisposeObject(DM);
end;

initialization

Th_Engine_Marshal_Pool__ := TZDB2_Th_Engine_Marshal_Pool.Create;
Static_Copy_Instance_Pool__ := TZDB2_Th_Engine_Static_Copy_Instance_Pool.Create;
Dynamic_Copy_Instance_Pool__ := TZDB2_Th_Engine_Dynamic_Copy_Instance_Pool.Create;
Max_Busy_Instance_Recycle_Time := 15 * C_Tick_Minute;

finalization

disposeObjectAndNil(Th_Engine_Marshal_Pool__);
disposeObjectAndNil(Static_Copy_Instance_Pool__);
disposeObjectAndNil(Dynamic_Copy_Instance_Pool__);

end.
 
