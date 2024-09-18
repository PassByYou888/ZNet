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
{ * Expression OpCode                                                          * }
{ ****************************************************************************** }
unit Z.OpCode;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses SysUtils, Variants, Math,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.MemoryStream, Z.Status, Z.ListEngine, Z.UnicodeMixedLib, Z.HashList.Templet, Z.Parsing;

type
  TOpValueType = (
    ovtBool, ovtInt, ovtInt64, ovtUInt64, ovtWord, ovtByte, ovtSmallInt, ovtShortInt, ovtUInt,
    ovtSingle, ovtDouble, ovtCurrency,
    ovtString, ovtProc,
    ovtUnknow);

  TOpCode = class;
  TOpCustomRunTime = class;
  TOpCode_NonLinear = class;

  POpRTData = ^TOpRTData;
  TOpParam = array of Variant;

  TOn_Param_Op_C = function(var OP_Param: TOpParam): Variant;
  TOn_Param_Op_M = function(var OP_Param: TOpParam): Variant of object;
  TOn_RT_Op_C = function(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; var OP_Param: TOpParam): Variant;
  TOn_RT_Op_M = function(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; var OP_Param: TOpParam): Variant of object;
  TOn_Code_Op_C = function(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; OP_Code: TOpCode; var OP_Param: TOpParam): Variant;
  TOn_Code_Op_M = function(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; OP_Code: TOpCode; var OP_Param: TOpParam): Variant of object;
{$IFDEF FPC}
  TOn_Param_Op_P = function(var OP_Param: TOpParam): Variant is nested;
  TOn_RT_Op_P = function(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; var OP_Param: TOpParam): Variant is nested;
  TOn_Code_Op_P = function(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; OP_Code: TOpCode; var OP_Param: TOpParam): Variant is nested;
{$ELSE FPC}
  TOn_Param_Op_P = reference to function(var OP_Param: TOpParam): Variant;
  TOn_RT_Op_P = reference to function(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; var OP_Param: TOpParam): Variant;
  TOn_Code_Op_P = reference to function(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; OP_Code: TOpCode; var OP_Param: TOpParam): Variant;
{$ENDIF FPC}
  TOpRT_Mode = (rtmDirect, rtmSync, rtmPost);

  TOpRTData = record
  public
    Name, Description, Category: SystemString;
    On_Param_Op_C: TOn_Param_Op_C;
    On_Param_Op_M: TOn_Param_Op_M;
    On_Param_Op_P: TOn_Param_Op_P;
    On_RT_Op_C: TOn_RT_Op_C;
    On_RT_Op_M: TOn_RT_Op_M;
    On_RT_Op_P: TOn_RT_Op_P;
    On_Code_Op_C: TOn_Code_Op_C;
    On_Code_Op_M: TOn_Code_Op_M;
    On_Code_Op_P: TOn_Code_Op_P;
    Mode: TOpRT_Mode;
    procedure Init;
  end;

  TOpRT_Sync_Bridge = class(TCore_Object_Intermediate)
  public
    opRT: TOpCustomRunTime;
    OC: TOpCode;
    OD: POpRTData;
    R_: Variant;
    procedure Do_Sync_Run;
  end;

{$REGION 'OpCode_Runtime'}

  TOpSystemAPI = class(TCore_Object_Intermediate)
  private
    function DoNop(var OP_Param: TOpParam): Variant;
    function DoInt(var OP_Param: TOpParam): Variant;
    function DoFrac(var OP_Param: TOpParam): Variant;
    function DoExp(var OP_Param: TOpParam): Variant;
    function DoCos(var OP_Param: TOpParam): Variant;
    function DoSin(var OP_Param: TOpParam): Variant;
    function DoLn(var OP_Param: TOpParam): Variant;
    function DoArcTan(var OP_Param: TOpParam): Variant;
    function DoSqrt(var OP_Param: TOpParam): Variant;
    function DoSqr(var OP_Param: TOpParam): Variant;
    function DoTan(var OP_Param: TOpParam): Variant;
    function DoRound(var OP_Param: TOpParam): Variant;
    function DoTrunc(var OP_Param: TOpParam): Variant;
    function DoDeg(var OP_Param: TOpParam): Variant;
    function DoPower(var OP_Param: TOpParam): Variant;
    function DoSingle(var OP_Param: TOpParam): Variant;
    function DoDouble(var OP_Param: TOpParam): Variant;
    function DoExtended(var OP_Param: TOpParam): Variant;
    function DoByte(var OP_Param: TOpParam): Variant;
    function DoWord(var OP_Param: TOpParam): Variant;
    function DoCardinal(var OP_Param: TOpParam): Variant;
    function DoUInt64(var OP_Param: TOpParam): Variant;
    function DoShortInt(var OP_Param: TOpParam): Variant;
    function DoSmallInt(var OP_Param: TOpParam): Variant;
    function DoInteger(var OP_Param: TOpParam): Variant;
    function DoInt64(var OP_Param: TOpParam): Variant;
    function DoROL8(var OP_Param: TOpParam): Variant;
    function DoROL16(var OP_Param: TOpParam): Variant;
    function DoROL32(var OP_Param: TOpParam): Variant;
    function DoROL64(var OP_Param: TOpParam): Variant;
    function DoROR8(var OP_Param: TOpParam): Variant;
    function DoROR16(var OP_Param: TOpParam): Variant;
    function DoROR32(var OP_Param: TOpParam): Variant;
    function DoROR64(var OP_Param: TOpParam): Variant;
    function DoEndian16(var OP_Param: TOpParam): Variant;
    function DoEndian32(var OP_Param: TOpParam): Variant;
    function DoEndian64(var OP_Param: TOpParam): Variant;
    function DoEndianU16(var OP_Param: TOpParam): Variant;
    function DoEndianU32(var OP_Param: TOpParam): Variant;
    function DoEndianU64(var OP_Param: TOpParam): Variant;
    function DoSAR16(var OP_Param: TOpParam): Variant;
    function DoSAR32(var OP_Param: TOpParam): Variant;
    function DoSAR64(var OP_Param: TOpParam): Variant;
    function DoNot(var OP_Param: TOpParam): Variant;
    function DoPI(var OP_Param: TOpParam): Variant;
    function DoBool(var OP_Param: TOpParam): Variant;
    function DoTrue(var OP_Param: TOpParam): Variant;
    function DoFalse(var OP_Param: TOpParam): Variant;
    function DoRColor(var OP_Param: TOpParam): Variant;
    function DoVec2(var OP_Param: TOpParam): Variant;
    function DoVec3(var OP_Param: TOpParam): Variant;
    function DoVec4(var OP_Param: TOpParam): Variant;
    function DoRandom(var OP_Param: TOpParam): Variant;
    function DoRandomFloat(var OP_Param: TOpParam): Variant;
    function DoMax(var OP_Param: TOpParam): Variant;
    function DoMin(var OP_Param: TOpParam): Variant;
    function DoClamp(var OP_Param: TOpParam): Variant;
    function DoIfThen(var OP_Param: TOpParam): Variant;
    function FitXY(var OP_Param: TOpParam): Variant;
    function DoStr(var OP_Param: TOpParam): Variant;
    function DoMultiple(var OP_Param: TOpParam): Variant;
    function DoSearchStr(var OP_Param: TOpParam): Variant;
    function DoReplaceStr(var OP_Param: TOpParam): Variant;
    function DoPrint(var OP_Param: TOpParam): Variant;
    function ToHex(var OP_Param: TOpParam): Variant;
    function Hex8(var OP_Param: TOpParam): Variant;
    function Hex16(var OP_Param: TOpParam): Variant;
    function Hex32(var OP_Param: TOpParam): Variant;
    function Hex64(var OP_Param: TOpParam): Variant;
    function ToBin(var OP_Param: TOpParam): Variant;
    function Bin8(var OP_Param: TOpParam): Variant;
    function Bin16(var OP_Param: TOpParam): Variant;
    function Bin32(var OP_Param: TOpParam): Variant;
    function Bin64(var OP_Param: TOpParam): Variant;
  public
    procedure RegistationSystemAPI(RunTime: TOpCustomRunTime);
  end;

  TOpCustomRunTime = class(TCore_Object_Intermediate)
  protected
    procedure FreeNotifyProc(p: Pointer);
  public
    ProcList: THashList;
    UserObject: TCore_Object;
    UserData: Pointer;
    constructor Create;
    constructor CustomCreate(maxHashSiz_: Integer); virtual;
    destructor Destroy; override;
    procedure Begin_Op_Proc(Sender: TOpCode); virtual;
    procedure End_Op_Proc(Sender: TOpCode; var Result_: Variant); virtual;
    procedure Clean; virtual;
    procedure PrepareRegistation; virtual;
    function GetProcDescription(ProcName: SystemString): SystemString; overload;
    function GetAllProcDescription(): TPascalStringList; overload;
    function GetAllProcDescription(Category: U_String): TPascalStringList; overload;
    function GetAllProcDescription(InclSys_: Boolean; Category: U_String): TPascalStringList; overload;
    function GetProc(const ProcName: SystemString): POpRTData;
    property Proc_[const ProcName: SystemString]: POpRTData read GetProc;
    // compatible param-caller
    function RegOpC(ProcName: SystemString; On_P: TOn_Param_Op_C): POpRTData; overload;
    function RegOpC(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_C): POpRTData; overload;
    function RegOpC(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_C; Mode: TOpRT_Mode): POpRTData; overload;
    function RegOpM(ProcName: SystemString; On_P: TOn_Param_Op_M): POpRTData; overload;
    function RegOpM(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_M): POpRTData; overload;
    function RegOpM(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_M; Mode: TOpRT_Mode): POpRTData; overload;
    function RegOpP(ProcName: SystemString; On_P: TOn_Param_Op_P): POpRTData; overload;
    function RegOpP(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_P): POpRTData; overload;
    function RegOpP(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_P; Mode: TOpRT_Mode): POpRTData; overload;
    // param-caller
    function Reg_Param_OpC(ProcName: SystemString; On_P: TOn_Param_Op_C): POpRTData; overload;
    function Reg_Param_OpC(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_C): POpRTData; overload;
    function Reg_Param_OpC(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_C; Mode: TOpRT_Mode): POpRTData; overload;
    function Reg_Param_OpM(ProcName: SystemString; On_P: TOn_Param_Op_M): POpRTData; overload;
    function Reg_Param_OpM(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_M): POpRTData; overload;
    function Reg_Param_OpM(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_M; Mode: TOpRT_Mode): POpRTData; overload;
    function Reg_Param_OpP(ProcName: SystemString; On_P: TOn_Param_Op_P): POpRTData; overload;
    function Reg_Param_OpP(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_P): POpRTData; overload;
    function Reg_Param_OpP(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_P; Mode: TOpRT_Mode): POpRTData; overload;
    // compatible object-caller
    function RegObjectOpC(ProcName: SystemString; On_P: TOn_RT_Op_C): POpRTData; overload;
    function RegObjectOpC(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_C): POpRTData; overload;
    function RegObjectOpC(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_C; Mode: TOpRT_Mode): POpRTData; overload;
    function RegObjectOpM(ProcName: SystemString; On_P: TOn_RT_Op_M): POpRTData; overload;
    function RegObjectOpM(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_M): POpRTData; overload;
    function RegObjectOpM(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_M; Mode: TOpRT_Mode): POpRTData; overload;
    function RegObjectOpP(ProcName: SystemString; On_P: TOn_RT_Op_P): POpRTData; overload;
    function RegObjectOpP(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_P): POpRTData; overload;
    function RegObjectOpP(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_P; Mode: TOpRT_Mode): POpRTData; overload;
    // object-caller
    function Reg_RT_OpC(ProcName: SystemString; On_P: TOn_RT_Op_C): POpRTData; overload;
    function Reg_RT_OpC(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_C): POpRTData; overload;
    function Reg_RT_OpC(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_C; Mode: TOpRT_Mode): POpRTData; overload;
    function Reg_RT_OpM(ProcName: SystemString; On_P: TOn_RT_Op_M): POpRTData; overload;
    function Reg_RT_OpM(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_M): POpRTData; overload;
    function Reg_RT_OpM(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_M; Mode: TOpRT_Mode): POpRTData; overload;
    function Reg_RT_OpP(ProcName: SystemString; On_P: TOn_RT_Op_P): POpRTData; overload;
    function Reg_RT_OpP(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_P): POpRTData; overload;
    function Reg_RT_OpP(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_P; Mode: TOpRT_Mode): POpRTData; overload;
    // non-linear caller
    function Reg_Code_OpC(ProcName: SystemString; On_P: TOn_Code_Op_C): POpRTData; overload;
    function Reg_Code_OpC(ProcName, ProcDescription: SystemString; On_P: TOn_Code_Op_C): POpRTData; overload;
    function Reg_Code_OpC(ProcName, ProcDescription: SystemString; On_P: TOn_Code_Op_C; Mode: TOpRT_Mode): POpRTData; overload;
    function Reg_Code_OpM(ProcName: SystemString; On_P: TOn_Code_Op_M): POpRTData; overload;
    function Reg_Code_OpM(ProcName, ProcDescription: SystemString; On_P: TOn_Code_Op_M): POpRTData; overload;
    function Reg_Code_OpM(ProcName, ProcDescription: SystemString; On_P: TOn_Code_Op_M; Mode: TOpRT_Mode): POpRTData; overload;
    function Reg_Code_OpP(ProcName: SystemString; On_P: TOn_Code_Op_P): POpRTData; overload;
    function Reg_Code_OpP(ProcName, ProcDescription: SystemString; On_P: TOn_Code_Op_P): POpRTData; overload;
    function Reg_Code_OpP(ProcName, ProcDescription: SystemString; On_P: TOn_Code_Op_P; Mode: TOpRT_Mode): POpRTData; overload;
  end;
{$ENDREGION 'OpCode_Runtime'}
{$REGION 'OpCode_Base'}

  TOp_Class = class of TOpCode;

  TOpCode_Pool_Decl = TCritical_String_Big_Hash_Pair_Pool<TOpCode>;

  TOpCode_Pool = class(TOpCode_Pool_Decl)
  public
    AutoFree_OpCode: Boolean;
    constructor Create(const AutoFree_OpCode_: Boolean; const HashSize_: Integer);
    procedure DoFree(var Key: SystemString; var Value: TOpCode); override;
  end;

  POpData__ = ^TOpData__;

  TOpData__ = record
    Op: TOpCode;
    Value: Variant;
    ValueType: TOpValueType;
  end;

  TOpData_List = TGenericsList<POpData__>;

  TOpCode = class(TCore_Object_Intermediate)
  protected
    FOwner: TOpCode;
    FParam: TOpData_List;
    FAutoFreeLink: Boolean;
    function DoExecute(opRT: TOpCustomRunTime): Variant; virtual;
  private
    // TOpCode calling mechanism: call and execute from deep to shallow in a stack manner
    procedure OpCode_EvaluateParam(opRT: TOpCustomRunTime); overload;
    procedure OpCode_EvaluateParam(printLog: Boolean; opRT: TOpCustomRunTime); overload;
  public
    NonLinear: TOpCode_NonLinear;
    Parsed_Info: SystemString;
    Parsed_Line_Num: Integer;
    constructor Create(FreeLink_: Boolean);
    destructor Destroy; override;
    procedure SaveToStream(stream: TCore_Stream);
    class function LoadFromStream(stream: TCore_Stream; var LoadedOp: TOpCode): Boolean;
    function AddValue(v: Variant): Integer; overload;
    function AddValueT(v: Variant; VT: TOpValueType): Integer; overload;
    function AddLink(Obj: TOpCode): Integer;
    function Clone(): TOpCode; // fast clone
    function GetParam(index: Integer): POpData__;
    property Param[index: Integer]: POpData__ read GetParam; default;
    function Count: Integer;
    function OpCode_Execute: Variant; overload;
    function OpCode_Execute(opRT: TOpCustomRunTime): Variant; overload;
    function Execute: Variant; overload;
    function Execute(opRT: TOpCustomRunTime): Variant; overload;
    function Owner_Root: TOpCode;
    property Owner: TOpCode read FOwner;
    property AutoFreeLink: Boolean read FAutoFreeLink write FAutoFreeLink;
  end;
{$ENDREGION 'OpCode_Base'}
{$REGION 'OpCode_NonLinear'}

  TOpCode_NonLinear_Pool_ = TBigList<TOpCode_NonLinear>;

  TOpCode_NonLinear_Stack = TOrderStruct<POpData__>;

  TOn_OpCode_NonLinear_Done_C = procedure(Sender: TOpCode_NonLinear);
  TOn_OpCode_NonLinear_Done_M = procedure(Sender: TOpCode_NonLinear) of object;
  TOn_OpCode_NonLinear_Step_C = procedure(Sender: TOpCode_NonLinear; OpCode_: TOpCode);
  TOn_OpCode_NonLinear_Step_M = procedure(Sender: TOpCode_NonLinear; OpCode_: TOpCode) of object;
{$IFDEF FPC}
  TOn_OpCode_NonLinear_Done_P = procedure(Sender: TOpCode_NonLinear) is nested;
  TOn_OpCode_NonLinear_Step_P = procedure(Sender: TOpCode_NonLinear; OpCode_: TOpCode) is nested;
{$ELSE FPC}
  TOn_OpCode_NonLinear_Done_P = reference to procedure(Sender: TOpCode_NonLinear);
  TOn_OpCode_NonLinear_Step_P = reference to procedure(Sender: TOpCode_NonLinear; OpCode_: TOpCode);
{$ENDIF FPC}

  TOpCode_NonLinear_Pool = class(TOpCode_NonLinear_Pool_)
  private type
    TPost_Data___ = record
      TS_: TTextStyle;
      Expression_: SystemString;
      opRT_: TOpCustomRunTime;
      On_Done_C: TOn_OpCode_NonLinear_Done_C;
      On_Done_M: TOn_OpCode_NonLinear_Done_M;
      On_Done_P: TOn_OpCode_NonLinear_Done_P;
    end;

    PPost_Data___ = ^TPost_Data___;
  private
    FPost___: TThreadPost;
    procedure Do_Post_Execute(Data1: Pointer);
  public
    constructor Create;
    destructor Destroy; override;
    property Post___: TThreadPost read FPost___;
    procedure DoFree(var Data: TOpCode_NonLinear); override;
    procedure Process;
    function Prepare(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime): TOpCode_NonLinear;
    procedure Execute(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime);
    procedure Execute_C(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime; On_Done_C: TOn_OpCode_NonLinear_Done_C);
    procedure Execute_M(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime; On_Done_M: TOn_OpCode_NonLinear_Done_M);
    procedure Execute_P(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime; On_Done_P: TOn_OpCode_NonLinear_Done_P);
    // post to thread
    procedure Post_Execute(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime);
    procedure Post_Execute_C(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime; On_Done_C: TOn_OpCode_NonLinear_Done_C);
    procedure Post_Execute_M(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime; On_Done_M: TOn_OpCode_NonLinear_Done_M);
    procedure Post_Execute_P(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime; On_Done_P: TOn_OpCode_NonLinear_Done_P);
    procedure Post_Execute_Vector_Expression(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime);
    class procedure Test_Post_Execute();
  end;

  // TOpCode_NonLinear calling mechanism: no longer call in the stack mode,
  // the new mechanism is to pave the way for the calling sequence structure from deep to shallow, and then execute the sequence once again
  // The TOpCode_NonLinear call mechanism can support non-linear processes and accurately identify locations such as exceptions and trackers within the stack model
  TOpCode_NonLinear = class(TCore_Object_Intermediate)
  private
    FAuto_Free_OpCode: Boolean;
    FRoot_OpCode: TOpCode;
    FOpCode_RunTime: TOpCustomRunTime;
    FOwner_Pool_Ptr: TOpCode_NonLinear_Pool_.PQueueStruct;
    FStack___: TOpCode_NonLinear_Stack;
    FFirst_Execute_Done: Boolean;
    FIs_Running: Boolean;
    FIs_Wait_End: Boolean;
    FEnd_Result: Variant;
    FOn_Done_C: TOn_OpCode_NonLinear_Done_C;
    FOn_Done_M: TOn_OpCode_NonLinear_Done_M;
    FOn_Done_P: TOn_OpCode_NonLinear_Done_P;
    FOn_Step_C: TOn_OpCode_NonLinear_Step_C;
    FOn_Step_M: TOn_OpCode_NonLinear_Step_M;
    FOn_Step_P: TOn_OpCode_NonLinear_Step_P;
  protected
    procedure Build_Stack();
    procedure Reset_OpCode_None_Linear();
    procedure Do_Init(); virtual;
  public
    constructor Create_From_OpCode(Auto_Free_OpCode_: Boolean; Root_OpCode_: TOpCode; opRT_: TOpCustomRunTime);
    constructor Create_From_Expression(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime);
    destructor Destroy; override;
    procedure Reinit(); virtual;
    procedure Execute(); virtual;
    procedure Process(); virtual; // main-loop
    function Wait_End(): Variant;

    // The begin+end is a nonlinear process controller in OpCode event
    procedure Do_Begin();
    procedure Do_End(); overload;
    property End_Result: Variant read FEnd_Result write FEnd_Result;
    property Result_: Variant read FEnd_Result write FEnd_Result;
    procedure Do_End(Result___: Variant); overload;
    procedure Do_Error();

    property First_Execute_Done: Boolean read FFirst_Execute_Done;
    property Is_Running: Boolean read FIs_Running;
    property Is_Wait_End: Boolean read FIs_Wait_End;
    property Auto_Free_OpCode: Boolean read FAuto_Free_OpCode write FAuto_Free_OpCode;
    property OpCode: TOpCode read FRoot_OpCode;
    property OpRunTime: TOpCustomRunTime read FOpCode_RunTime;
    property Stack___: TOpCode_NonLinear_Stack read FStack___;
    property On_Done: TOn_OpCode_NonLinear_Done_M read FOn_Done_M write FOn_Done_M;
    property On_Done_C: TOn_OpCode_NonLinear_Done_C read FOn_Done_C write FOn_Done_C;
    property On_Done_M: TOn_OpCode_NonLinear_Done_M read FOn_Done_M write FOn_Done_M;
    property On_Done_P: TOn_OpCode_NonLinear_Done_P read FOn_Done_P write FOn_Done_P;
    property On_Step: TOn_OpCode_NonLinear_Step_M read FOn_Step_M write FOn_Step_M;
    property On_Step_C: TOn_OpCode_NonLinear_Step_C read FOn_Step_C write FOn_Step_C;
    property On_Step_M: TOn_OpCode_NonLinear_Step_M read FOn_Step_M write FOn_Step_M;
    property On_Step_P: TOn_OpCode_NonLinear_Step_P read FOn_Step_P write FOn_Step_P;

    class procedure Test();
  end;

{$ENDREGION 'OpCode_NonLinear'}
{$REGION 'OpCode_Operation'}

  op_Value = class sealed(TOpCode)
  protected
    // a
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Proc = class sealed(TOpCode)
  protected
    // proc(a,b,c...)
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Add_Prefix = class sealed(TOpCode)
  protected
    // +proc
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Sub_Prefix = class sealed(TOpCode)
  protected
    // -proc
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Add = class sealed(TOpCode)
  protected
    // a + b + n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Sub = class sealed(TOpCode)
  protected
    // a - b - n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Mul = class sealed(TOpCode)
  protected
    // a * b * n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Div = class sealed(TOpCode)
  protected
    // a / b / n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_IntDiv = class sealed(TOpCode)
  protected
    // a div b div n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Pow = class sealed(TOpCode)
  protected
    // a pow b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Mod = class sealed(TOpCode)
  protected
    // a mod b mod n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Or = class sealed(TOpCode)
  protected
    // a or b or n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_And = class sealed(TOpCode)
  protected
    // a and b and n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Xor = class sealed(TOpCode)
  protected
    // a xor b xor n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Shl = class sealed(TOpCode)
  protected
    // a shl b shl n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Shr = class sealed(TOpCode)
  protected
    // a shr b shr n...
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Equal = class sealed(TOpCode)
  protected
    // a == b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_LessThan = class sealed(TOpCode)
  protected
    // a < b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_EqualOrLessThan = class sealed(TOpCode)
  protected
    // a <= b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_GreaterThan = class sealed(TOpCode)
  protected
    // a > b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_EqualOrGreaterThan = class sealed(TOpCode)
  protected
    // a >= b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_NotEqual = class sealed(TOpCode)
  protected
    // a <> b
    // a != b
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Symbol_Sub = class sealed(TOpCode)
  protected
    // -a
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Symbol_Add = class sealed(TOpCode)
  protected
    // +a
    function DoExecute(opRT: TOpCustomRunTime): Variant; override;
  end;
{$ENDREGION 'OpCode_Operation'}
{$REGION 'OpCode_Reg'}

  TOpRegData = record
    opClass: TOp_Class;
    OpName: TPascalString;
    hash: Cardinal;
  end;

  POpRegData = ^TOpRegData;
  TOpReg_Tool = TGenericsList<POpRegData>;
{$ENDREGION 'OpCode_Reg'}

function LoadOpFromStream(stream: TCore_Stream; out LoadedOp: TOpCode): Boolean;

var
  OpRegTool: TOpReg_Tool;
  OpSystemAPI: TOpSystemAPI;
  SystemOpRunTime: TOpCustomRunTime;
  System_NonLinear_Pool: TOpCode_NonLinear_Pool;

implementation

uses Z.Geometry2D, Z.Geometry3D, Z.DFE, Z.Expression;

var
  Hooked_OnCheckThreadSynchronize: TOn_Check_Thread_Synchronize;

procedure DoCheckThreadSynchronize();
begin
  if Assigned(Hooked_OnCheckThreadSynchronize) then
    begin
      try
          Hooked_OnCheckThreadSynchronize();
      except
      end;
    end;
  System_NonLinear_Pool.Process;
end;

procedure TOpRTData.Init;
begin
  Name := '';
  Description := '';
  Category := '';
  On_Param_Op_C := nil;
  On_Param_Op_M := nil;
  On_Param_Op_P := nil;
  On_RT_Op_C := nil;
  On_RT_Op_M := nil;
  On_RT_Op_P := nil;
  On_Code_Op_C := nil;
  On_Code_Op_M := nil;
  On_Code_Op_P := nil;
  Mode := rtmDirect;
end;

procedure TOpRT_Sync_Bridge.Do_Sync_Run;
var
  i: Integer;
  tmp_Param: TOpParam;
begin
  if OC.Count > 1 then
    begin
      // copy param
      SetLength(tmp_Param, OC.Count - 1);
      for i := 1 to OC.Count - 1 do
          tmp_Param[i - 1] := OC.Param[i]^.Value;
    end
  else
      SetLength(tmp_Param, 0);

  R_ := NULL;
  try
    opRT.Begin_Op_Proc(OC);
    if Assigned(OD^.On_Param_Op_C) then
        R_ := OD^.On_Param_Op_C(tmp_Param)
    else if Assigned(OD^.On_Param_Op_M) then
        R_ := OD^.On_Param_Op_M(tmp_Param)
    else if Assigned(OD^.On_Param_Op_P) then
        R_ := OD^.On_Param_Op_P(tmp_Param)
    else if Assigned(OD^.On_RT_Op_C) then
        R_ := OD^.On_RT_Op_C(opRT, OD, tmp_Param)
    else if Assigned(OD^.On_RT_Op_M) then
        R_ := OD^.On_RT_Op_M(opRT, OD, tmp_Param)
    else if Assigned(OD^.On_RT_Op_P) then
        R_ := OD^.On_RT_Op_P(opRT, OD, tmp_Param)
    else if Assigned(OD^.On_Code_Op_C) then
        R_ := OD^.On_Code_Op_C(opRT, OD, OC, tmp_Param)
    else if Assigned(OD^.On_Code_Op_M) then
        R_ := OD^.On_Code_Op_M(opRT, OD, OC, tmp_Param)
    else if Assigned(OD^.On_Code_Op_P) then
        R_ := OD^.On_Code_Op_P(opRT, OD, OC, tmp_Param);
    opRT.End_Op_Proc(OC, R_);
  except
  end;
  SetLength(tmp_Param, 0);
end;

function GetRegistedOp(Name_P: PPascalString): POpRegData;
var
  i: Integer;
  p: POpRegData;
  hash: Cardinal;
begin
  Result := nil;
  hash := FastHashPPascalString(Name_P);
  for i := 0 to OpRegTool.Count - 1 do
    begin
      p := OpRegTool[i];
      if (p^.hash = hash) and Name_P^.Same(@p^.OpName) then
          Exit(p);
    end;
end;

procedure RegisterOp(c: TOp_Class);
var
  n: TPascalString;
  p: POpRegData;
begin
  n := c.ClassName;
  if GetRegistedOp(@n) <> nil then
      RaiseInfo('repeat reg OP ' + c.ClassName);
  new(p);
  p^.opClass := c;
  p^.OpName := p^.opClass.ClassName;
  p^.hash := FastHashPPascalString(@p^.OpName);
  OpRegTool.Add(p);
end;

procedure FreeOpRegClass;
var
  i: Integer;
  p: POpRegData;
begin
  for i := 0 to OpRegTool.Count - 1 do
    begin
      p := OpRegTool[i];
      Dispose(p);
    end;
  DisposeObject(OpRegTool);
end;

function LoadOpFromStream(stream: TCore_Stream; out LoadedOp: TOpCode): Boolean;

  function LoadFromDataFrame_(D_: TDFE): TOpCode;
  var
    Name_: TPascalString;
    RegPtr: POpRegData;
    i, Num_: Integer;
    NeedNewOp: Boolean;
    newDataEng: TDFE;
    v: Variant;
    VT: TOpValueType;
  begin
    Name_ := D_.Reader.ReadString;
    RegPtr := GetRegistedOp(@Name_);
    if RegPtr <> nil then
      begin
        Result := RegPtr^.opClass.Create(True);
        Result.Parsed_Info := D_.Reader.ReadString;
        Result.Parsed_Line_Num := D_.Reader.ReadInteger;
        Num_ := D_.Reader.ReadInteger;
        for i := 0 to Num_ - 1 do
          begin
            NeedNewOp := D_.Reader.ReadBool;

            if NeedNewOp then
              begin
                // create new TOpCode
                newDataEng := TDFE.Create;
                D_.Reader.ReadDataFrame(newDataEng);
                Result.AddLink(LoadFromDataFrame_(newDataEng));
                DisposeObject(newDataEng);
              end
            else
              begin
                v := D_.Reader.ReadVariant;
                VT := TOpValueType(D_.Reader.ReadInteger);
                Result.AddValueT(v, VT);
              end;
          end;
      end
    else
        raise Exception.Create('load OpCode failed');
  end;

var
  DataEng: TDFE;
  DataEdition: Integer;
begin
  Result := False;
  DataEng := TDFE.Create;
  try
    DataEng.DecodeFrom(stream, True);
    DataEdition := DataEng.Reader.ReadInteger;
    if DataEdition = 1 then // edition
      begin
        LoadedOp := LoadFromDataFrame_(DataEng);
        Result := True;
      end
    else
        LoadedOp := nil;
  except
  end;
  DisposeObject(DataEng);
end;

function TOpSystemAPI.DoNop(var OP_Param: TOpParam): Variant;
begin
  Result := 0;
end;

function TOpSystemAPI.DoInt(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Int(v);
end;

function TOpSystemAPI.DoFrac(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Frac(v);
end;

function TOpSystemAPI.DoExp(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Exp(v);
end;

function TOpSystemAPI.DoCos(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Cos(v);
end;

function TOpSystemAPI.DoSin(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Sin(v);
end;

function TOpSystemAPI.DoLn(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := ln(v);
end;

function TOpSystemAPI.DoArcTan(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := ArcTan(v);
end;

function TOpSystemAPI.DoSqrt(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Sqrt(v);
end;

function TOpSystemAPI.DoSqr(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Sqr(v);
end;

function TOpSystemAPI.DoTan(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Tan(v);
end;

function TOpSystemAPI.DoRound(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Round(Double(v));
end;

function TOpSystemAPI.DoTrunc(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := Trunc(Double(v));
end;

function TOpSystemAPI.DoDeg(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];
  Result := NormalizeDegAngle(TGeoFloat(v));
end;

function TOpSystemAPI.DoPower(var OP_Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  if length(OP_Param) = 2 then
      Result := Power(OP_Param[0], OP_Param[1])
  else
      Result := 0;
end;

function TOpSystemAPI.DoSingle(var OP_Param: TOpParam): Variant;
begin
  Result := Single(OP_Param[0]);
end;

function TOpSystemAPI.DoDouble(var OP_Param: TOpParam): Variant;
begin
  Result := Double(OP_Param[0]);
end;

function TOpSystemAPI.DoExtended(var OP_Param: TOpParam): Variant;
begin
  Result := Extended(OP_Param[0]);
end;

function TOpSystemAPI.DoByte(var OP_Param: TOpParam): Variant;
begin
  Result := Byte(OP_Param[0]);
end;

function TOpSystemAPI.DoWord(var OP_Param: TOpParam): Variant;
begin
  Result := Word(OP_Param[0]);
end;

function TOpSystemAPI.DoCardinal(var OP_Param: TOpParam): Variant;
begin
  Result := Cardinal(OP_Param[0]);
end;

function TOpSystemAPI.DoUInt64(var OP_Param: TOpParam): Variant;
begin
  Result := UInt64(OP_Param[0]);
end;

function TOpSystemAPI.DoShortInt(var OP_Param: TOpParam): Variant;
begin
  Result := ShortInt(OP_Param[0]);
end;

function TOpSystemAPI.DoSmallInt(var OP_Param: TOpParam): Variant;
begin
  Result := SmallInt(OP_Param[0]);
end;

function TOpSystemAPI.DoInteger(var OP_Param: TOpParam): Variant;
begin
  Result := Integer(OP_Param[0]);
end;

function TOpSystemAPI.DoInt64(var OP_Param: TOpParam): Variant;
begin
  Result := Int64(OP_Param[0]);
end;

function TOpSystemAPI.DoROL8(var OP_Param: TOpParam): Variant;
begin
  Result := ROL8(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoROL16(var OP_Param: TOpParam): Variant;
begin
  Result := ROL16(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoROL32(var OP_Param: TOpParam): Variant;
begin
  Result := ROL32(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoROL64(var OP_Param: TOpParam): Variant;
begin
  Result := ROL64(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoROR8(var OP_Param: TOpParam): Variant;
begin
  Result := ROR8(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoROR16(var OP_Param: TOpParam): Variant;
begin
  Result := ROR16(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoROR32(var OP_Param: TOpParam): Variant;
begin
  Result := ROR32(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoROR64(var OP_Param: TOpParam): Variant;
begin
  Result := ROR64(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoEndian16(var OP_Param: TOpParam): Variant;
begin
  Result := Endian(SmallInt(OP_Param[0]));
end;

function TOpSystemAPI.DoEndian32(var OP_Param: TOpParam): Variant;
begin
  Result := Endian(Integer(OP_Param[0]));
end;

function TOpSystemAPI.DoEndian64(var OP_Param: TOpParam): Variant;
begin
  Result := Endian(Int64(OP_Param[0]));
end;

function TOpSystemAPI.DoEndianU16(var OP_Param: TOpParam): Variant;
begin
  Result := Endian(Word(OP_Param[0]));
end;

function TOpSystemAPI.DoEndianU32(var OP_Param: TOpParam): Variant;
begin
  Result := Endian(Cardinal(OP_Param[0]));
end;

function TOpSystemAPI.DoEndianU64(var OP_Param: TOpParam): Variant;
begin
  Result := Endian(UInt64(OP_Param[0]));
end;

function TOpSystemAPI.DoSAR16(var OP_Param: TOpParam): Variant;
begin
  Result := SAR16(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoSAR32(var OP_Param: TOpParam): Variant;
begin
  Result := SAR32(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoSAR64(var OP_Param: TOpParam): Variant;
begin
  Result := SAR64(OP_Param[0], OP_Param[1]);
end;

function TOpSystemAPI.DoNot(var OP_Param: TOpParam): Variant;
var
  VT: TVarType;
begin
  VT := TVarData(OP_Param[0]).VType;
  case VT of
    varSmallInt, varWord: Result := not Word(OP_Param[0]);
    varInteger, varLongWord: Result := not Cardinal(OP_Param[0]);
    varShortInt, varByte: Result := not Byte(OP_Param[0]);
    varInt64, varUInt64: Result := not UInt64(OP_Param[0]);
    varBoolean: Result := not Boolean(OP_Param[0]);
    else Result := OP_Param[0];
  end;
end;

function TOpSystemAPI.DoPI(var OP_Param: TOpParam): Variant;
begin
  Result := PI;
end;

function TOpSystemAPI.DoBool(var OP_Param: TOpParam): Variant;
  function v2b(const v: Variant): Boolean;
  var
    n: TPascalString;
  begin
    if VarIsStr(v) then
      begin
        n := VarToStr(v);
        n := n.DeleteChar(#32#9);
        if n.Same('True') or n.Same('Yes') or n.Same('1') then
            Result := True
        else
            Result := False;
      end
    else if VarIsOrdinal(v) then
        Result := Boolean(v)
    else if VarIsFloat(v) then
        Result := Boolean(Round(Double(v)))
    else
        Result := Boolean(v);
  end;

var
  n: Boolean;
  i: Integer;
begin
  n := True;
  for i := low(OP_Param) to high(OP_Param) do
      n := n and v2b(OP_Param[i]);
  Result := n;
end;

function TOpSystemAPI.DoTrue(var OP_Param: TOpParam): Variant;
begin
  Result := True;
end;

function TOpSystemAPI.DoFalse(var OP_Param: TOpParam): Variant;
begin
  Result := False;
end;

function TOpSystemAPI.DoRColor(var OP_Param: TOpParam): Variant;
var
  buff: array [0 .. 3] of SystemString;
  i: Integer;
begin
  for i := 0 to 2 do
      buff[i] := '0.0';
  buff[3] := '1.0';

  for i := Low(OP_Param) to high(OP_Param) do
      buff[i] := VarToStr(OP_Param[i]);

  Result := Format('%s,%s,%s,%s', [buff[0], buff[1], buff[2], buff[3]]);
end;

function TOpSystemAPI.DoVec2(var OP_Param: TOpParam): Variant;
var
  buff: array [0 .. 1] of SystemString;
  i: Integer;
begin
  for i := Low(buff) to high(buff) do
      buff[i] := '0.0';

  for i := Low(OP_Param) to high(OP_Param) do
      buff[i] := VarToStr(OP_Param[i]);

  Result := Format('%s,%s', [buff[0], buff[1]]);
end;

function TOpSystemAPI.DoVec3(var OP_Param: TOpParam): Variant;
var
  buff: array [0 .. 2] of SystemString;
  i: Integer;
begin
  for i := Low(buff) to high(buff) do
      buff[i] := '0.0';

  for i := Low(OP_Param) to high(OP_Param) do
      buff[i] := VarToStr(OP_Param[i]);

  Result := Format('%s,%s,%s', [buff[0], buff[1], buff[2]]);
end;

function TOpSystemAPI.DoVec4(var OP_Param: TOpParam): Variant;
var
  buff: array [0 .. 3] of SystemString;
  i: Integer;
begin
  for i := Low(buff) to high(buff) do
      buff[i] := '0.0';

  for i := Low(OP_Param) to high(OP_Param) do
      buff[i] := VarToStr(OP_Param[i]);

  Result := Format('%s,%s,%s,%s', [buff[0], buff[1], buff[2], buff[3]]);
end;

function TOpSystemAPI.DoRandom(var OP_Param: TOpParam): Variant;
var
  v: Integer;
  i: Integer;
begin
  v := 0;
  for i := low(OP_Param) to high(OP_Param) do
      v := v + OP_Param[i];

  if v <> 0 then
      Result := MT19937Rand32(v)
  else
      Result := MT19937Rand32(MaxInt);
end;

function TOpSystemAPI.DoRandomFloat(var OP_Param: TOpParam): Variant;
begin
  Result := MT19937RandF;
end;

function TOpSystemAPI.DoMax(var OP_Param: TOpParam): Variant;
var
  i: Integer;
begin
  if length(OP_Param) = 0 then
    begin
      Result := NULL;
      Exit;
    end;
  Result := OP_Param[0];
  for i := 1 to length(OP_Param) - 1 do
    if OP_Param[i] > Result then
        Result := OP_Param[i];
end;

function TOpSystemAPI.DoMin(var OP_Param: TOpParam): Variant;
var
  i: Integer;
begin
  if length(OP_Param) = 0 then
    begin
      Result := NULL;
      Exit;
    end;
  Result := OP_Param[0];
  for i := 1 to length(OP_Param) - 1 do
    if OP_Param[i] < Result then
        Result := OP_Param[i];
end;

function TOpSystemAPI.DoClamp(var OP_Param: TOpParam): Variant;
var
  minv_, maxv_: Variant;
begin
  if length(OP_Param) <> 3 then
    begin
      if length(OP_Param) > 0 then
          Result := OP_Param[0]
      else
          Result := NULL;
      Exit;
    end;

  if OP_Param[1] > OP_Param[2] then
    begin
      minv_ := OP_Param[2];
      maxv_ := OP_Param[1];
    end
  else
    begin
      minv_ := OP_Param[1];
      maxv_ := OP_Param[2];
    end;

  if OP_Param[0] < minv_ then
      Result := minv_
  else if OP_Param[0] > maxv_ then
      Result := maxv_
  else
      Result := OP_Param[0];
end;

function TOpSystemAPI.DoIfThen(var OP_Param: TOpParam): Variant;
begin
  if length(OP_Param) <> 3 then
    begin
      Result := NULL;
      Exit;
    end;
  if Boolean(OP_Param[0]) = True then
      Result := OP_Param[1]
  else
      Result := OP_Param[2];
end;

function TOpSystemAPI.FitXY(var OP_Param: TOpParam): Variant;
var
  r: TRectV2;
begin
  r := FitRect(OP_Param[0], OP_Param[1], RectV2(0, 0, OP_Param[2], OP_Param[3]));
  Result := VecToStr(r[1]);
end;

function TOpSystemAPI.DoStr(var OP_Param: TOpParam): Variant;
var
  n: TPascalString;
  i: Integer;
begin
  n := '';
  for i := low(OP_Param) to high(OP_Param) do
      n.Append(VarToStr(OP_Param[i]));
  Result := n;
end;

function TOpSystemAPI.DoMultiple(var OP_Param: TOpParam): Variant;
var
  i: Integer;
begin
  if length(OP_Param) >= 2 then
    begin
      Result := True;
      for i := 1 to length(OP_Param) - 1 do
          Result := Result and umlMultipleMatch(VarToStr(OP_Param[0]), VarToStr(OP_Param[i]));
    end
  else
      Result := True;
end;

function TOpSystemAPI.DoSearchStr(var OP_Param: TOpParam): Variant;
var
  i: Integer;
begin
  if length(OP_Param) >= 2 then
    begin
      Result := True;
      for i := 1 to length(OP_Param) - 1 do
          Result := Result and umlSearchMatch(VarToStr(OP_Param[0]), VarToStr(OP_Param[i]));
    end
  else
      Result := True;
end;

function TOpSystemAPI.DoReplaceStr(var OP_Param: TOpParam): Variant;
begin
  Result := umlReplace(VarToStr(OP_Param[0]), VarToStr(OP_Param[1]), VarToStr(OP_Param[2]), OP_Param[3], OP_Param[4]).Text;
end;

function TOpSystemAPI.DoPrint(var OP_Param: TOpParam): Variant;
var
  n: TPascalString;
  i: Integer;
begin
  n := '';
  for i := low(OP_Param) to high(OP_Param) do
    begin
      if n.L > 0 then
          n.Append(',');
      n.Append(VarToStr(OP_Param[i]));
    end;
  DoStatus(n);
  Result := n.Text;
end;

function TOpSystemAPI.ToHex(var OP_Param: TOpParam): Variant;
var
  n: TPascalString;
  i: Integer;
  VT: TVarType;
begin
  n := '';
  for i := low(OP_Param) to high(OP_Param) do
    begin
      if n.L > 0 then
          n.Append(',');
      VT := TVarData(OP_Param[i]).VType;
      case VT of
        varSmallInt, varWord: n.Append(IntToHex(Word(OP_Param[i]), 4));
        varInteger, varLongWord: n.Append(IntToHex(Cardinal(OP_Param[i]), 8));
        varShortInt, varByte: n.Append(IntToHex(Byte(OP_Param[i]), 2));
        varInt64, varUInt64: n.Append(IntToHex(UInt64(OP_Param[i]), 16));
        else n.Append(VarToStr(OP_Param[i]));
      end;
    end;
  Result := n.Text;
end;

function TOpSystemAPI.Hex8(var OP_Param: TOpParam): Variant;
begin
  Result := Byte(StrToInt('$' + VarToStr(OP_Param[0])));
end;

function TOpSystemAPI.Hex16(var OP_Param: TOpParam): Variant;
begin
  Result := Word(StrToInt('$' + VarToStr(OP_Param[0])));
end;

function TOpSystemAPI.Hex32(var OP_Param: TOpParam): Variant;
begin
  Result := StrToInt('$' + VarToStr(OP_Param[0]));
end;

function TOpSystemAPI.Hex64(var OP_Param: TOpParam): Variant;
begin
  Result := StrToUInt64('$' + VarToStr(OP_Param[0]));
end;

function TOpSystemAPI.ToBin(var OP_Param: TOpParam): Variant;
var
  n: TPascalString;
  i: Integer;
  VT: TVarType;
begin
  n := '';
  for i := low(OP_Param) to high(OP_Param) do
    begin
      if n.L > 0 then
          n.Append(',');
      VT := TVarData(OP_Param[i]).VType;
      case VT of
        varSmallInt, varWord: n.Append(umlUInt16ToBin(OP_Param[i]));
        varInteger, varLongWord: n.Append(umlUInt32ToBin(OP_Param[i]));
        varShortInt, varByte: n.Append(umlUInt8ToBin(OP_Param[i]));
        varInt64, varUInt64: n.Append(umlUInt64ToBin(OP_Param[i]));
        else n.Append(VarToStr(OP_Param[i]));
      end;
    end;
  Result := n.Text;
end;

function TOpSystemAPI.Bin8(var OP_Param: TOpParam): Variant;
begin
  Result := umlBinToUInt8(VarToStr(OP_Param[0]));
end;

function TOpSystemAPI.Bin16(var OP_Param: TOpParam): Variant;
begin
  Result := umlBinToUInt16(VarToStr(OP_Param[0]));
end;

function TOpSystemAPI.Bin32(var OP_Param: TOpParam): Variant;
begin
  Result := umlBinToUInt32(VarToStr(OP_Param[0]));
end;

function TOpSystemAPI.Bin64(var OP_Param: TOpParam): Variant;
begin
  Result := umlBinToUInt64(VarToStr(OP_Param[0]));
end;

procedure TOpSystemAPI.RegistationSystemAPI(RunTime: TOpCustomRunTime);
begin
  RunTime.Reg_Param_OpM('Nop', 'Nop(): null function,return 0', DoInt)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Int', 'Int(0..n): math function', DoInt)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Frac', 'Frac(0..n): math function', DoFrac)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Exp', 'Exp(0..n): math function', DoExp)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Cos', 'Cos(0..n): math function', DoCos)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Sin', 'Sin(0..n): math function', DoSin)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Ln', 'Ln(0..n): math function', DoLn)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('ArcTan', 'ArcTan(0..n): math function', DoArcTan)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Sqrt', 'Sqrt(0..n): math function', DoSqrt)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Sqr', 'Sqr(0..n): math function', DoSqr)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Tan', 'Tan(0..n): math function', DoTan)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Round', 'Round(0..n): math function', DoRound)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Trunc', 'Trunc(0..n): math function', DoTrunc)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Deg', 'Deg(0..n): NormalizeDegAngle function', DoDeg)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Power', 'Power(float,float): Power: Raise base to any power function', DoPower)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Pow', 'Pow(float,float): Power: Raise base to any power function', DoPower)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Single', 'Single(value): math function', DoSingle)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Double', 'Double(value): math function', DoDouble)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Float', 'Float(value): math function', DoDouble)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Extended', 'Extended(value): math function', DoExtended)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Byte', 'Byte(value): math function', DoByte)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Word', 'Word(value): math function', DoWord)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Cardinal', 'Cardinal(value): math function', DoCardinal)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('UInt64', 'UInt64(value): math function', DoUInt64)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('ShortInt', 'ShortInt(value): math function', DoShortInt)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('SmallInt', 'SmallInt(value): math function', DoSmallInt)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Integer', 'Integer(value): math function', DoInteger)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Int64', 'Int64(value): math function', DoInt64)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('ROL8', 'ROL8(byte,Shift): math function', DoROL8)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('ROL16', 'ROL16(word,Shift): math function', DoROL16)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('ROL32', 'ROL32(cardinal,Shift): math function', DoROL32)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('ROL64', 'ROL64(uint64,Shift): math function', DoROL64)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('ROR8', 'ROR8(byte,Shift): math function', DoROR8)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('ROR16', 'ROR16(word,Shift): math function', DoROR16)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('ROR32', 'ROR32(cardinal,Shift): math function', DoROR32)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('ROR64', 'ROR64(uint64,Shift): math function', DoROR64)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Endian16', 'Endian16(smallint): math function', DoEndian16)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Endian32', 'Endian32(integer): math function', DoEndian32)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Endian64', 'Endian64(int64): math function', DoEndian64)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('EndianU16', 'EndianU16(word): math function', DoEndianU16)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('EndianU32', 'EndianU32(cardinal): math function', DoEndianU32)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('EndianU64', 'EndianU64(uint64): math function', DoEndianU64)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('SAR16', 'SAR16(word,Shift): math function', DoSAR16)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('SAR32', 'SAR32(cardinal,Shift): math function', DoSAR32)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('SAR64', 'SAR64(uint64,Shift): math function', DoSAR64)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Not', 'Not(Ordinal): not math function', DoNot)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('~', '~(Ordinal): not math function', DoNot)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('PI', 'PI(): return PI', DoPI)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Bool', 'Bool(n..n): convert any variant as bool', DoBool)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Boolean', 'Boolean(n..n): convert any variant as bool', DoBool)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('True', 'True(): return true', DoTrue)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('False', 'False(): return false', DoFalse)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('RColor', 'RColor(R,G,B,A): return RColor string', DoRColor)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Vec2', 'Vec2(X,Y): return Vec2 string', DoVec2)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Vec3', 'Vec3(X,Y,Z): return Vec3 string', DoVec3)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Vec4', 'Vec4(X,Y,Z,W): return Vec4 string', DoVec4)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Random', 'Random(0..n): return number', DoRandom)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('RandomFloat', 'RandomFloat(): return float', DoRandomFloat)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('RandomF', 'RandomF(): return float', DoRandomFloat)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Max', 'Max(0..n): return max value', DoMax)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Min', 'Min(0..n): return min value', DoMin)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Clamp', 'Clamp(value, min, max): return clamp value', DoClamp)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('IfThen', 'IfThen(bool, if true then of value, if false then of value): return if value', DoIfThen)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('if_', 'if_(bool, if true then of value, if false then of value): return if value', DoIfThen)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('FitXY', 'FitXY(width, height, new_width, new_height): return size', FitXY)^.Category := 'Base Math';
  RunTime.Reg_Param_OpM('Str', 'Str(n..n): convert any variant as string', DoStr)^.Category := 'Base String';
  RunTime.Reg_Param_OpM('String', 'String(n..n): convert any variant as string', DoStr)^.Category := 'Base String';
  RunTime.Reg_Param_OpM('Text', 'Text(n..n): convert any variant as string', DoStr)^.Category := 'Base String';
  RunTime.Reg_Param_OpM('MultipleMatch', 'MultipleMatch(multile exp, n..n): return bool', DoMultiple)^.Category := 'Base String';
  RunTime.Reg_Param_OpM('Multiple', 'MultipleMatch(multile exp, n..n): return bool', DoMultiple)^.Category := 'Base String';
  RunTime.Reg_Param_OpM('SearchStr', 'SearchStr(multile exp, n..n): return bool', DoSearchStr)^.Category := 'Base String';
  RunTime.Reg_Param_OpM('ReplaceStr', 'ReplaceStr(source, OldPattern, NewPattern: string; OnlyWord, IgnoreCase: Bool): return string', DoReplaceStr)^.Category := 'Base String';
  RunTime.Reg_Param_OpM('Print', 'Print(multile exp, n..n): return text', DoPrint)^.Category := 'Base Print';
  RunTime.Reg_Param_OpM('DoStatus', 'DoStatus(multile exp, n..n): return text', DoPrint)^.Category := 'Base Print';
  RunTime.Reg_Param_OpM('Status', 'Status(multile exp, n..n): return text', DoPrint)^.Category := 'Base Print';
  RunTime.Reg_Param_OpM('ToHex', 'ToHex(multile exp, n..n): return hex text', ToHex)^.Category := 'Hex';
  RunTime.Reg_Param_OpM('Hex8', 'Hex8(text): return byte', Hex8)^.Category := 'Hex';
  RunTime.Reg_Param_OpM('Hex16', 'Hex16(text): return byte', Hex16)^.Category := 'Hex';
  RunTime.Reg_Param_OpM('Hex32', 'Hex32(text): return byte', Hex32)^.Category := 'Hex';
  RunTime.Reg_Param_OpM('Hex64', 'Hex64(text): return byte', Hex64)^.Category := 'Hex';
  RunTime.Reg_Param_OpM('ToBin', 'ToBin(multile exp, n..n): return Binary text', ToBin)^.Category := 'Binary';
  RunTime.Reg_Param_OpM('Bin8', 'Bin8(text): return byte', Bin8)^.Category := 'Binary';
  RunTime.Reg_Param_OpM('Bin16', 'Bin16(text): return byte', Bin16)^.Category := 'Binary';
  RunTime.Reg_Param_OpM('Bin32', 'Bin32(text): return byte', Bin32)^.Category := 'Binary';
  RunTime.Reg_Param_OpM('Bin64', 'Bin64(text): return byte', Bin64)^.Category := 'Binary';
end;

procedure TOpCustomRunTime.FreeNotifyProc(p: Pointer);
begin
  POpRTData(p)^.Init;
  Dispose(POpRTData(p));
end;

constructor TOpCustomRunTime.Create;
begin
  CustomCreate($FF);
end;

constructor TOpCustomRunTime.CustomCreate(maxHashSiz_: Integer);
begin
  inherited Create;
  ProcList := THashList.CustomCreate(maxHashSiz_);
  ProcList.AutoFreeData := True;
  ProcList.AccessOptimization := False;
  ProcList.OnFreePtr := FreeNotifyProc;
  UserObject := nil;
  UserData := nil;
  PrepareRegistation;
end;

destructor TOpCustomRunTime.Destroy;
begin
  DisposeObject(ProcList);
  inherited Destroy;
end;

procedure TOpCustomRunTime.Begin_Op_Proc(Sender: TOpCode);
begin

end;

procedure TOpCustomRunTime.End_Op_Proc(Sender: TOpCode; var Result_: Variant);
begin

end;

procedure TOpCustomRunTime.Clean;
begin
  ProcList.Clear;
end;

procedure TOpCustomRunTime.PrepareRegistation;
begin
end;

function TOpCustomRunTime.GetProcDescription(ProcName: SystemString): SystemString;
var
  p: POpRTData;
begin
  Result := ProcName + '(): no Descripion';
  p := ProcList[ProcName];
  if p <> nil then
    begin
      if p^.Description <> '' then
          Result := p^.Description;
    end
  else if self <> SystemOpRunTime then
      Result := SystemOpRunTime.GetProcDescription(ProcName);
end;

function TOpCustomRunTime.GetAllProcDescription(): TPascalStringList;
begin
  Result := GetAllProcDescription('*');
end;

function TOpCustomRunTime.GetAllProcDescription(Category: U_String): TPascalStringList;
begin
  Result := GetAllProcDescription(True, Category);
end;

function TOpCustomRunTime.GetAllProcDescription(InclSys_: Boolean; Category: U_String): TPascalStringList;
var
  arry: THashDataArray;
  hl: THashObjectList;
  ns, tmp: TPascalStringList;
  i, j: Integer;
  p: POpRTData;
  n: TPascalString;
begin
  if InclSys_ and (self <> SystemOpRunTime) then
      Result := SystemOpRunTime.GetAllProcDescription(Category)
  else
      Result := TPascalStringList.Create;

  arry := ProcList.GetHashDataArray();

  hl := THashObjectList.CustomCreate(True, $FF);
  for i := Low(arry) to High(arry) do
    begin
      p := arry[i]^.Data;
      if not hl.Exists(p^.Category) then
          hl.FastAdd(p^.Category, TPascalStringList.Create);
      tmp := hl[p^.Category] as TPascalStringList;

      if p^.Description <> '' then
        begin
          if umlReplaceSum(' ' + p^.Description, ' ' + p^.Name + '(', False, True, 0, 0, nil) > 0 then
              n := p^.Description
          else
              n := p^.Name + '(): ' + p^.Description;
        end
      else
          n := p^.Name + '(): no Descripion';

      tmp.Add(n);
    end;
  SetLength(arry, 0);

  ns := TPascalStringList.Create;
  hl.GetListData(ns);
  for i := 0 to ns.Count - 1 do
    if umlMultipleMatch(Category, ns[i]) then
      begin
        Result.Add(PFormat('%s:', [ns[i].Text]));
        tmp := ns.Objects[i] as TPascalStringList;
        for j := 0 to tmp.Count - 1 do
            Result.Add('  ' + tmp[j].Text);
        Result.Add('');
      end;
  n := '';
  DisposeObject(ns);
  DisposeObject(hl);
end;

function TOpCustomRunTime.GetProc(const ProcName: SystemString): POpRTData;
begin
  Result := ProcList[ProcName];
end;

function TOpCustomRunTime.RegOpC(ProcName: SystemString; On_P: TOn_Param_Op_C): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_Param_Op_C := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegOpC(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_C): POpRTData;
begin
  Result := RegOpC(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.RegOpC(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_C; Mode: TOpRT_Mode): POpRTData;
begin
  Result := RegOpC(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode
end;

function TOpCustomRunTime.RegOpM(ProcName: SystemString; On_P: TOn_Param_Op_M): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_Param_Op_M := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegOpM(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_M): POpRTData;
begin
  Result := RegOpM(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.RegOpM(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_M; Mode: TOpRT_Mode): POpRTData;
begin
  Result := RegOpM(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode
end;

function TOpCustomRunTime.RegOpP(ProcName: SystemString; On_P: TOn_Param_Op_P): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_Param_Op_P := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegOpP(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_P): POpRTData;
begin
  Result := RegOpP(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.RegOpP(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_P; Mode: TOpRT_Mode): POpRTData;
begin
  Result := RegOpP(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode
end;

function TOpCustomRunTime.Reg_Param_OpC(ProcName: SystemString; On_P: TOn_Param_Op_C): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_Param_Op_C := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.Reg_Param_OpC(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_C): POpRTData;
begin
  Result := Reg_Param_OpC(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.Reg_Param_OpC(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_C; Mode: TOpRT_Mode): POpRTData;
begin
  Result := Reg_Param_OpC(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode
end;

function TOpCustomRunTime.Reg_Param_OpM(ProcName: SystemString; On_P: TOn_Param_Op_M): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_Param_Op_M := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.Reg_Param_OpM(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_M): POpRTData;
begin
  Result := Reg_Param_OpM(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.Reg_Param_OpM(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_M; Mode: TOpRT_Mode): POpRTData;
begin
  Result := Reg_Param_OpM(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode
end;

function TOpCustomRunTime.Reg_Param_OpP(ProcName: SystemString; On_P: TOn_Param_Op_P): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_Param_Op_P := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.Reg_Param_OpP(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_P): POpRTData;
begin
  Result := Reg_Param_OpP(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.Reg_Param_OpP(ProcName, ProcDescription: SystemString; On_P: TOn_Param_Op_P; Mode: TOpRT_Mode): POpRTData;
begin
  Result := Reg_Param_OpP(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode
end;

function TOpCustomRunTime.RegObjectOpC(ProcName: SystemString; On_P: TOn_RT_Op_C): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_RT_Op_C := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegObjectOpC(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_C): POpRTData;
begin
  Result := RegObjectOpC(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.RegObjectOpC(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_C; Mode: TOpRT_Mode): POpRTData;
begin
  Result := RegObjectOpC(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode;
end;

function TOpCustomRunTime.RegObjectOpM(ProcName: SystemString; On_P: TOn_RT_Op_M): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_RT_Op_M := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegObjectOpM(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_M): POpRTData;
begin
  Result := RegObjectOpM(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.RegObjectOpM(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_M; Mode: TOpRT_Mode): POpRTData;
begin
  Result := RegObjectOpM(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode;
end;

function TOpCustomRunTime.RegObjectOpP(ProcName: SystemString; On_P: TOn_RT_Op_P): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_RT_Op_P := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.RegObjectOpP(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_P): POpRTData;
begin
  Result := RegObjectOpP(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.RegObjectOpP(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_P; Mode: TOpRT_Mode): POpRTData;
begin
  Result := RegObjectOpP(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode;
end;

function TOpCustomRunTime.Reg_RT_OpC(ProcName: SystemString; On_P: TOn_RT_Op_C): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_RT_Op_C := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.Reg_RT_OpC(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_C): POpRTData;
begin
  Result := Reg_RT_OpC(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.Reg_RT_OpC(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_C; Mode: TOpRT_Mode): POpRTData;
begin
  Result := Reg_RT_OpC(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode;
end;

function TOpCustomRunTime.Reg_RT_OpM(ProcName: SystemString; On_P: TOn_RT_Op_M): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_RT_Op_M := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.Reg_RT_OpM(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_M): POpRTData;
begin
  Result := Reg_RT_OpM(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.Reg_RT_OpM(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_M; Mode: TOpRT_Mode): POpRTData;
begin
  Result := Reg_RT_OpM(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode;
end;

function TOpCustomRunTime.Reg_RT_OpP(ProcName: SystemString; On_P: TOn_RT_Op_P): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_RT_Op_P := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.Reg_RT_OpP(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_P): POpRTData;
begin
  Result := Reg_RT_OpP(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.Reg_RT_OpP(ProcName, ProcDescription: SystemString; On_P: TOn_RT_Op_P; Mode: TOpRT_Mode): POpRTData;
begin
  Result := Reg_RT_OpP(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode;
end;

function TOpCustomRunTime.Reg_Code_OpC(ProcName: SystemString; On_P: TOn_Code_Op_C): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_Code_Op_C := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.Reg_Code_OpC(ProcName, ProcDescription: SystemString; On_P: TOn_Code_Op_C): POpRTData;
begin
  Result := Reg_Code_OpC(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.Reg_Code_OpC(ProcName, ProcDescription: SystemString; On_P: TOn_Code_Op_C; Mode: TOpRT_Mode): POpRTData;
begin
  Result := Reg_Code_OpC(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode;
end;

function TOpCustomRunTime.Reg_Code_OpM(ProcName: SystemString; On_P: TOn_Code_Op_M): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_Code_Op_M := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.Reg_Code_OpM(ProcName, ProcDescription: SystemString; On_P: TOn_Code_Op_M): POpRTData;
begin
  Result := Reg_Code_OpM(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.Reg_Code_OpM(ProcName, ProcDescription: SystemString; On_P: TOn_Code_Op_M; Mode: TOpRT_Mode): POpRTData;
begin
  Result := Reg_Code_OpM(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode;
end;

function TOpCustomRunTime.Reg_Code_OpP(ProcName: SystemString; On_P: TOn_Code_Op_P): POpRTData;
var
  p: POpRTData;
begin
  new(p);
  p^.Init;
  p^.Name := ProcName;
  p^.On_Code_Op_P := On_P;
  ProcList.Add(ProcName, p, True);
  Result := p;
end;

function TOpCustomRunTime.Reg_Code_OpP(ProcName, ProcDescription: SystemString; On_P: TOn_Code_Op_P): POpRTData;
begin
  Result := Reg_Code_OpP(ProcName, On_P);
  Result^.Description := ProcDescription;
end;

function TOpCustomRunTime.Reg_Code_OpP(ProcName, ProcDescription: SystemString; On_P: TOn_Code_Op_P; Mode: TOpRT_Mode): POpRTData;
begin
  Result := Reg_Code_OpP(ProcName, ProcDescription, On_P);
  Result^.Mode := Mode;
end;

constructor TOpCode_Pool.Create(const AutoFree_OpCode_: Boolean; const HashSize_: Integer);
begin
  inherited Create(HashSize_, nil);
  AutoFree_OpCode := AutoFree_OpCode_;
end;

procedure TOpCode_Pool.DoFree(var Key: SystemString; var Value: TOpCode);
begin
  if AutoFree_OpCode then
      DisposeObjectAndNil(Value)
  else
      Value := nil;
  inherited DoFree(Key, Value);
end;

function TOpCode.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := NULL;
end;

procedure TOpCode.OpCode_EvaluateParam(opRT: TOpCustomRunTime);
begin
  OpCode_EvaluateParam({$IFDEF Print_OPCode_Debug}True{$ELSE Print_OPCode_Debug}False{$ENDIF Print_OPCode_Debug}, opRT);
end;

procedure TOpCode.OpCode_EvaluateParam(printLog: Boolean; opRT: TOpCustomRunTime);
var
  i: Integer;
  p: POpData__;
begin
  for i := 0 to FParam.Count - 1 do
    begin
      p := FParam[i];
      if p^.Op <> nil then
        begin
          try
              p^.Op.OpCode_EvaluateParam(printLog, opRT);
          except
              Exit;
          end;

          try
            p^.Value := p^.Op.DoExecute(opRT);
            if printLog then
                DoStatus('parsed:%s %s value:%s', [Parsed_Info, ClassName, VarToStr(p^.Value)]);
          except
            if printLog then
                DoStatus('parsed:%s OpCode error:%s', [Parsed_Info, ClassName]);
            Exit;
          end;
        end;
    end;
end;

constructor TOpCode.Create(FreeLink_: Boolean);
begin
  inherited Create;
  FOwner := nil;
  FParam := TOpData_List.Create;
  FAutoFreeLink := FreeLink_;
  NonLinear := nil;
  Parsed_Info := '';
  Parsed_Line_Num := 0;
end;

destructor TOpCode.Destroy;
var
  i: Integer;
  p: POpData__;
begin
  for i := 0 to FParam.Count - 1 do
    begin
      p := FParam[i];
      if (FAutoFreeLink) and (p^.Op <> nil) then
          DisposeObject(p^.Op);
      p^.Value := NULL;
      Dispose(p);
    end;
  FParam.Clear;
  DisposeObject(FParam);
  inherited Destroy;
end;

procedure TOpCode.SaveToStream(stream: TCore_Stream);
  procedure SaveToDFE(Op: TOpCode; D_: TDFE);
  var
    i: Integer;
    p: POpData__;
    newDataEng: TDFE;
  begin
    D_.WriteString(Op.ClassName);
    D_.WriteString(Op.Parsed_Info);
    D_.WriteInteger(Op.Parsed_Line_Num);
    D_.WriteInteger(Op.Count);
    for i := 0 to Op.Count - 1 do
      begin
        p := Op[i];
        if p^.Op <> nil then
          begin
            D_.WriteBool(True);
            newDataEng := TDFE.Create;
            SaveToDFE(p^.Op, newDataEng);
            D_.WriteDataFrame(newDataEng);
            DisposeObject(newDataEng);
          end
        else
          begin
            D_.WriteBool(False);
            D_.WriteVariant(p^.Value);
            D_.WriteInteger(Integer(p^.ValueType));
          end;
      end;
  end;

var
  DataEng: TDFE;
begin
  DataEng := TDFE.Create;
  DataEng.WriteInteger(1); // edition
  SaveToDFE(self, DataEng);
  DataEng.FastEncodeTo(stream);
  DisposeObject(DataEng);
end;

class function TOpCode.LoadFromStream(stream: TCore_Stream; var LoadedOp: TOpCode): Boolean;
begin
  Result := LoadOpFromStream(stream, LoadedOp);
end;

function TOpCode.AddValue(v: Variant): Integer;
var
  p: POpData__;
begin
  new(p);
  p^.Op := nil;

  p^.Value := v;

  case VarType(v) of
    varSmallInt: p^.ValueType := ovtSmallInt;
    varInteger: p^.ValueType := ovtInt;
    varSingle: p^.ValueType := ovtSingle;
    varDouble: p^.ValueType := ovtDouble;
    varCurrency: p^.ValueType := ovtCurrency;
    varBoolean: p^.ValueType := ovtBool;
    varShortInt: p^.ValueType := ovtShortInt;
    varByte: p^.ValueType := ovtByte;
    varWord: p^.ValueType := ovtWord;
    varLongWord: p^.ValueType := ovtUInt;
    varInt64: p^.ValueType := ovtInt64;
    varUInt64: p^.ValueType := ovtUInt64;
    else
      begin
        if VarIsStr(v) then
            p^.ValueType := ovtString
        else
            p^.ValueType := ovtUnknow;
      end;
  end;

  Result := FParam.Add(p);
end;

function TOpCode.AddValueT(v: Variant; VT: TOpValueType): Integer;
var
  p: POpData__;
begin
  new(p);
  p^.Op := nil;
  p^.Value := v;
  p^.ValueType := VT;
  Result := FParam.Add(p);
end;

function TOpCode.AddLink(Obj: TOpCode): Integer;
var
  p: POpData__;
begin
  new(p);

  if Obj.FOwner <> nil then
      p^.Op := Obj.Clone
  else
      p^.Op := Obj;

  p^.Op.FOwner := self;

  p^.Value := NULL;
  p^.ValueType := ovtUnknow;
  Result := FParam.Add(p);
end;

function TOpCode.Clone(): TOpCode;
var
  i: Integer;
  p: POpData__;
begin
  Result := TOp_Class(self.ClassType).Create(True);
  Result.Parsed_Info := Parsed_Info;
  Result.Parsed_Line_Num := Parsed_Line_Num;

  for i := 0 to FParam.Count - 1 do
    begin
      p := FParam[i];
      if p^.Op <> nil then
          Result.AddLink(p^.Op.Clone)
      else
          Result.AddValueT(p^.Value, p^.ValueType);
    end;
end;

function TOpCode.GetParam(index: Integer): POpData__;
begin
  Result := FParam[index];
end;

function TOpCode.Count: Integer;
begin
  Result := FParam.Count;
end;

function TOpCode.OpCode_Execute: Variant;
begin
  Result := OpCode_Execute(SystemOpRunTime);
end;

function TOpCode.OpCode_Execute(opRT: TOpCustomRunTime): Variant;
begin
  try
      OpCode_EvaluateParam(opRT);
  except
    Result := NULL;
    Exit;
  end;

  try
      Result := DoExecute(opRT);
  except
      Result := NULL;
  end;
end;

function TOpCode.Execute: Variant;
begin
  Result := OpCode_Execute();
end;

function TOpCode.Execute(opRT: TOpCustomRunTime): Variant;
begin
  Result := OpCode_Execute(opRT);
end;

function TOpCode.Owner_Root: TOpCode;
begin
  if FOwner = nil then
      Result := self
  else
      Result := FOwner.Owner_Root;
end;

procedure TOpCode_NonLinear_Pool.Do_Post_Execute(Data1: Pointer);
var
  p: PPost_Data___;
begin
  p := Data1;
  if Assigned(p^.On_Done_C) then
      Execute_C(p^.TS_, p^.Expression_, p^.opRT_, p^.On_Done_C)
  else if Assigned(p^.On_Done_M) then
      Execute_M(p^.TS_, p^.Expression_, p^.opRT_, p^.On_Done_M)
  else if Assigned(p^.On_Done_P) then
      Execute_P(p^.TS_, p^.Expression_, p^.opRT_, p^.On_Done_P)
  else
      Execute(p^.TS_, p^.Expression_, p^.opRT_);

  p^.Expression_ := '';
  Dispose(p);
end;

constructor TOpCode_NonLinear_Pool.Create;
begin
  inherited Create;
  FPost___ := TThreadPost.Create(0);
end;

destructor TOpCode_NonLinear_Pool.Destroy;
begin
  FPost___.Progress(0);
  Clear;
  DisposeObject(FPost___);
  inherited Destroy;
end;

procedure TOpCode_NonLinear_Pool.DoFree(var Data: TOpCode_NonLinear);
begin
  if Data <> nil then
    begin
      Data.FOwner_Pool_Ptr := nil;
      DisposeObjectAndNil(Data);
    end;
  inherited DoFree(Data);
end;

procedure TOpCode_NonLinear_Pool.Process;
begin
  try
    if FPost___.Num > 0 then
        FPost___.Progress(0);
    if Num > 0 then
      begin
        Free_Recycle_Pool;
        with repeat_ do
          repeat
            if (queue^.Data <> nil) then
              begin
                if (not queue^.Data.Is_Running) and (queue^.Data.First_Execute_Done) then
                    Push_To_Recycle_Pool(queue)
                else
                    queue^.Data.Process;
              end;
          until not Next;
        Free_Recycle_Pool;
      end;
  except
  end;
end;

function TOpCode_NonLinear_Pool.Prepare(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime): TOpCode_NonLinear;
begin
  Result := TOpCode_NonLinear.Create_From_Expression(TS_, Expression_, opRT_);
  Result.FOwner_Pool_Ptr := Add(Result);
end;

procedure TOpCode_NonLinear_Pool.Execute(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime);
begin
  Prepare(TS_, Expression_, opRT_).Execute();
end;

procedure TOpCode_NonLinear_Pool.Execute_C(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime; On_Done_C: TOn_OpCode_NonLinear_Done_C);
var
  NonLinear_: TOpCode_NonLinear;
begin
  NonLinear_ := Prepare(TS_, Expression_, opRT_);
  NonLinear_.On_Done_C := On_Done_C;
  NonLinear_.Execute;
end;

procedure TOpCode_NonLinear_Pool.Execute_M(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime; On_Done_M: TOn_OpCode_NonLinear_Done_M);
var
  NonLinear_: TOpCode_NonLinear;
begin
  NonLinear_ := Prepare(TS_, Expression_, opRT_);
  NonLinear_.On_Done_M := On_Done_M;
  NonLinear_.Execute;
end;

procedure TOpCode_NonLinear_Pool.Execute_P(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime; On_Done_P: TOn_OpCode_NonLinear_Done_P);
var
  NonLinear_: TOpCode_NonLinear;
begin
  NonLinear_ := Prepare(TS_, Expression_, opRT_);
  NonLinear_.On_Done_P := On_Done_P;
  NonLinear_.Execute;
end;

procedure TOpCode_NonLinear_Pool.Post_Execute(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime);
var
  p: PPost_Data___;
begin
  new(p);
  p^.TS_ := TS_;
  p^.Expression_ := Expression_;
  p^.opRT_ := opRT_;
  p^.On_Done_C := nil;
  p^.On_Done_M := nil;
  p^.On_Done_P := nil;
  FPost___.PostM2(p, Do_Post_Execute);
end;

procedure TOpCode_NonLinear_Pool.Post_Execute_C(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime; On_Done_C: TOn_OpCode_NonLinear_Done_C);
var
  p: PPost_Data___;
begin
  new(p);
  p^.TS_ := TS_;
  p^.Expression_ := Expression_;
  p^.opRT_ := opRT_;
  p^.On_Done_C := On_Done_C;
  p^.On_Done_M := nil;
  p^.On_Done_P := nil;
  FPost___.PostM2(p, Do_Post_Execute);
end;

procedure TOpCode_NonLinear_Pool.Post_Execute_M(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime; On_Done_M: TOn_OpCode_NonLinear_Done_M);
var
  p: PPost_Data___;
begin
  new(p);
  p^.TS_ := TS_;
  p^.Expression_ := Expression_;
  p^.opRT_ := opRT_;
  p^.On_Done_C := nil;
  p^.On_Done_M := On_Done_M;
  p^.On_Done_P := nil;
  FPost___.PostM2(p, Do_Post_Execute);
end;

procedure TOpCode_NonLinear_Pool.Post_Execute_P(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime; On_Done_P: TOn_OpCode_NonLinear_Done_P);
var
  p: PPost_Data___;
begin
  new(p);
  p^.TS_ := TS_;
  p^.Expression_ := Expression_;
  p^.opRT_ := opRT_;
  p^.On_Done_C := nil;
  p^.On_Done_M := nil;
  p^.On_Done_P := On_Done_P;
  FPost___.PostM2(p, Do_Post_Execute);
end;

procedure TOpCode_NonLinear_Pool.Post_Execute_Vector_Expression(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime);
var
  i: Integer;
  T: TTextParsing;
  L: TPascalStringList;
begin
  if IsNullExpression(Expression_, TS_) then
      Exit;
  if IsSymbolVectorExpression(Expression_, TS_) then
    begin
      T := TTextParsing.Create(Expression_, TS_, nil, SpacerSymbol.v);
      L := TPascalStringList.Create;
      if T.Extract_Symbol_Vector(L) then
        for i := 0 to L.Count - 1 do
            Post_Execute(TS_, L[i], opRT_);
      DisposeObject(T);
      DisposeObject(L);
    end
  else
      Post_Execute(TS_, Expression_, opRT_);
end;

class procedure TOpCode_NonLinear_Pool.Test_Post_Execute();
{$IFDEF DELPHI}
var
  rt: TOpCustomRunTime;
  inst: TOpCode_NonLinear_Pool;
{$ENDIF DELPHI}
begin
{$IFDEF DELPHI}
  rt := TOpCustomRunTime.Create;
  rt.Reg_Code_OpP('test_delay', '', function(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; OP_Code: TOpCode; var OP_Param: TOpParam): Variant
    begin
      Result := 'error';
      OP_Code.NonLinear.Do_Begin;
      TCompute.RunP_NP(procedure
        begin
          TCompute.Sleep(2 * 1000);
          OP_Code.NonLinear.Do_End('ok');
        end);
    end);
  inst := TOpCode_NonLinear_Pool.Create;
  inst.Post_Execute(tsC, 'print("test-"+test_delay)', rt);
  while inst.Num + inst.FPost___.Num > 0 do
      inst.Process;
  DisposeObject(inst);
  DisposeObject(rt);
{$ENDIF DELPHI}
end;

procedure TOpCode_NonLinear.Build_Stack();
  procedure Do_Build_OpCode(const OpCode_: TOpCode);
  var
    i: Integer;
    p: POpData__;
  begin
    for i := 0 to OpCode_.FParam.Count - 1 do
      begin
        p := OpCode_.FParam[i];
        if p^.Op <> nil then
            Do_Build_OpCode(p^.Op);
        FStack___.Push(p);
      end;
    OpCode_.NonLinear := self;
  end;

begin
  Do_Build_OpCode(FRoot_OpCode);
end;

procedure TOpCode_NonLinear.Reset_OpCode_None_Linear;
  procedure Do_Reset_(const OpCode_: TOpCode);
  var
    i: Integer;
    p: POpData__;
  begin
    for i := 0 to OpCode_.FParam.Count - 1 do
      begin
        p := OpCode_.FParam[i];
        if p^.Op <> nil then
            Do_Reset_(p^.Op);
      end;
    OpCode_.NonLinear := nil;
  end;

begin
  Do_Reset_(FRoot_OpCode);
end;

procedure TOpCode_NonLinear.Do_Init;
begin
  FOwner_Pool_Ptr := nil;
  FStack___ := TOpCode_NonLinear_Stack.Create;
  FFirst_Execute_Done := False;
  FIs_Running := False;
  FIs_Wait_End := False;
  FEnd_Result := NULL;
  FOn_Done_C := nil;
  FOn_Done_M := nil;
  FOn_Done_P := nil;
  FOn_Step_C := nil;
  FOn_Step_M := nil;
  FOn_Step_P := nil;
  Build_Stack();
end;

constructor TOpCode_NonLinear.Create_From_OpCode(Auto_Free_OpCode_: Boolean; Root_OpCode_: TOpCode; opRT_: TOpCustomRunTime);
begin
  inherited Create;
  FAuto_Free_OpCode := Auto_Free_OpCode_;
  FRoot_OpCode := Root_OpCode_;
  FOpCode_RunTime := opRT_;
  Do_Init();
end;

constructor TOpCode_NonLinear.Create_From_Expression(TS_: TTextStyle; Expression_: SystemString; opRT_: TOpCustomRunTime);
var
  exp_: SystemString;
  Op: TOpCode;
begin
  inherited Create;
  FAuto_Free_OpCode := True;
  if IsNullExpression(Expression_, TS_) then
      exp_ := 'Nop'
  else if IsSymbolVectorExpression(Expression_, TS_) then
      RaiseInfo('TOpCode_NonLinear does not support vector and matrix expressions.' + #13#10 +
      'Please use TExpression_Sequence to solve the running of vector and matrix expressions.')
  else
      exp_ := Expression_;

  FRoot_OpCode := nil;
  Op := OpCache[exp_];
  if (Op <> nil) then
    begin
      if Op <> nil then
          FRoot_OpCode := Op.Clone;
    end
  else
    begin
      Op := BuildAsOpCode({$IFDEF Print_OPCode_Debug}True{$ELSE Print_OPCode_Debug}False{$ENDIF Print_OPCode_Debug}, TS_, exp_, opRT_);
      if Op <> nil then
        begin
          OpCache.Add(exp_, Op, True);
          FRoot_OpCode := Op.Clone;
        end;
    end;

  if FRoot_OpCode = nil then
    begin
      DoStatus('Fatal error during compilation %s', [exp_]);
      FRoot_OpCode := BuildAsOpCode(False, tsPascal, 'Nop', opRT_);
    end;
  FOpCode_RunTime := opRT_;
  Do_Init();
end;

destructor TOpCode_NonLinear.Destroy;
begin
  if FOwner_Pool_Ptr <> nil then
    begin
      FOwner_Pool_Ptr^.Data := nil;
      FOwner_Pool_Ptr^.Instance___.Remove_P(FOwner_Pool_Ptr);
      FOwner_Pool_Ptr := nil;
    end;

  Reset_OpCode_None_Linear;
  DisposeObjectAndNil(FStack___);
  FEnd_Result := NULL;
  if FAuto_Free_OpCode then
      DisposeObjectAndNil(FRoot_OpCode);
  inherited Destroy;
end;

procedure TOpCode_NonLinear.Reinit();
begin
  FStack___.Clear;
  FFirst_Execute_Done := False;
  FIs_Running := False;
  FIs_Wait_End := False;
  FEnd_Result := NULL;
  Build_Stack();
end;

procedure TOpCode_NonLinear.Execute();
begin
  if FFirst_Execute_Done or FIs_Running or FIs_Wait_End or (FStack___.Num <= 0) then
      Exit;

  FFirst_Execute_Done := True;
  FIs_Running := True;
  FIs_Wait_End := False;

  while FStack___.Num > 0 do
    begin
      if FStack___.First^.Data^.Op <> nil then
        begin
          try
            FStack___.First^.Data^.Value := FStack___.First^.Data^.Op.DoExecute(FOpCode_RunTime);
            if Assigned(FOn_Step_C) then
                FOn_Step_C(self, FStack___.First^.Data^.Op);
            if Assigned(FOn_Step_M) then
                FOn_Step_M(self, FStack___.First^.Data^.Op);
            if Assigned(FOn_Step_P) then
                FOn_Step_P(self, FStack___.First^.Data^.Op);
          except
            FIs_Running := False;
            FIs_Wait_End := False;
            FEnd_Result := NULL;
            FStack___.Clear;
            Exit;
          end;
        end;
      FEnd_Result := FStack___.First^.Data^.Value;
      if FIs_Wait_End then
          Exit;
      FStack___.Next;
    end;

  // compute root-OpCode
  FEnd_Result := FRoot_OpCode.DoExecute(FOpCode_RunTime);

  FIs_Running := False;
  FIs_Wait_End := False;
  Reset_OpCode_None_Linear();
  if Assigned(FOn_Done_C) then
      FOn_Done_C(self);
  if Assigned(FOn_Done_M) then
      FOn_Done_M(self);
  if Assigned(FOn_Done_P) then
      FOn_Done_P(self);
end;

procedure TOpCode_NonLinear.Process();
begin
  if not FFirst_Execute_Done then
      Execute();
  if FIs_Wait_End or (not FIs_Running) or (FStack___.Num <= 0) then
      Exit;

  FStack___.First^.Data^.Value := FEnd_Result;
  FStack___.Next;
  while FStack___.Num > 0 do
    begin
      if FStack___.First^.Data^.Op <> nil then
        begin
          try
            FStack___.First^.Data^.Value := FStack___.First^.Data^.Op.DoExecute(FOpCode_RunTime);
            if Assigned(FOn_Step_C) then
                FOn_Step_C(self, FStack___.First^.Data^.Op);
            if Assigned(FOn_Step_M) then
                FOn_Step_M(self, FStack___.First^.Data^.Op);
            if Assigned(FOn_Step_P) then
                FOn_Step_P(self, FStack___.First^.Data^.Op);
          except
            FIs_Running := False;
            FIs_Wait_End := False;
            FEnd_Result := NULL;
            FStack___.Clear;
            Exit;
          end;
        end;
      FEnd_Result := FStack___.First^.Data^.Value;
      if FIs_Wait_End then
          Exit;
      FStack___.Next;
    end;

  // compute root-OpCode
  FEnd_Result := FRoot_OpCode.DoExecute(FOpCode_RunTime);

  FIs_Running := False;
  FIs_Wait_End := False;
  Reset_OpCode_None_Linear();
  if Assigned(FOn_Done_C) then
      FOn_Done_C(self);
  if Assigned(FOn_Done_M) then
      FOn_Done_M(self);
  if Assigned(FOn_Done_P) then
      FOn_Done_P(self);
end;

function TOpCode_NonLinear.Wait_End(): Variant;
begin
  if not FFirst_Execute_Done then
      Execute();
  while FIs_Running do
      Process();
  Result := FEnd_Result;
end;

procedure TOpCode_NonLinear.Do_Begin();
begin
  FIs_Wait_End := True;
end;

procedure TOpCode_NonLinear.Do_End();
begin
  FIs_Wait_End := False;
end;

procedure TOpCode_NonLinear.Do_End(Result___: Variant);
begin
  FEnd_Result := Result___;
  Do_End();
end;

procedure TOpCode_NonLinear.Do_Error;
begin
  FEnd_Result := NULL;
  Do_End();
end;

class procedure TOpCode_NonLinear.Test();
{$IFDEF DELPHI}
var
  rt: TOpCustomRunTime;
  inst: TOpCode_NonLinear;
{$ENDIF DELPHI}
begin
{$IFDEF DELPHI}
  rt := TOpCustomRunTime.Create;
  rt.Reg_Code_OpP('test_delay', '', function(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; OP_Code: TOpCode; var OP_Param: TOpParam): Variant
    begin
      Result := 'error';
      OP_Code.NonLinear.Do_Begin;
      TCompute.RunP_NP(procedure
        begin
          TCompute.Sleep(2 * 1000);
          OP_Code.NonLinear.Do_End('ok');
        end);
    end);
  inst := TOpCode_NonLinear.Create_From_Expression(tsC, 'print("test-"+test_delay)', rt);
  inst.Execute;
  while inst.Is_Running do
      inst.Process;
  DisposeObject(inst);
  DisposeObject(rt);
{$ENDIF DELPHI}
end;

{ op_Value }

function op_Value.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value;
end;

{ op_Proc }

function op_Proc.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  focus_runtime: TOpCustomRunTime;
  p: POpRTData;
  i: Integer;
  tmp_Param: TOpParam;
  bridge_: TOpRT_Sync_Bridge;
begin
  Result := NULL;
  if (opRT = nil) then
      opRT := SystemOpRunTime;

  p := opRT.ProcList[VarToStr(Param[0]^.Value)];
  focus_runtime := opRT;
  if p = nil then
    begin
      if opRT = SystemOpRunTime then
          Exit;
      p := SystemOpRunTime.ProcList[VarToStr(Param[0]^.Value)];
      if p = nil then
          Exit;
      focus_runtime := SystemOpRunTime;
    end;

  if p^.Mode = rtmDirect then
    begin
      if Count > 1 then
        begin
          SetLength(tmp_Param, Count - 1);
          for i := 1 to Count - 1 do
              tmp_Param[i - 1] := Param[i]^.Value;
        end
      else
          SetLength(tmp_Param, 0);

      focus_runtime.Begin_Op_Proc(self);
      if Assigned(p^.On_Param_Op_C) then
          Result := p^.On_Param_Op_C(tmp_Param)
      else if Assigned(p^.On_Param_Op_M) then
          Result := p^.On_Param_Op_M(tmp_Param)
      else if Assigned(p^.On_Param_Op_P) then
          Result := p^.On_Param_Op_P(tmp_Param)
      else if Assigned(p^.On_RT_Op_C) then
          Result := p^.On_RT_Op_C(opRT, p, tmp_Param)
      else if Assigned(p^.On_RT_Op_M) then
          Result := p^.On_RT_Op_M(opRT, p, tmp_Param)
      else if Assigned(p^.On_RT_Op_P) then
          Result := p^.On_RT_Op_P(opRT, p, tmp_Param)
      else if Assigned(p^.On_Code_Op_C) then
          Result := p^.On_Code_Op_C(opRT, p, self, tmp_Param)
      else if Assigned(p^.On_Code_Op_M) then
          Result := p^.On_Code_Op_M(opRT, p, self, tmp_Param)
      else if Assigned(p^.On_Code_Op_P) then
          Result := p^.On_Code_Op_P(opRT, p, self, tmp_Param);
      focus_runtime.End_Op_Proc(self, Result);
      SetLength(tmp_Param, 0);
    end
  else
    begin
      bridge_ := TOpRT_Sync_Bridge.Create;
      bridge_.opRT := opRT;
      bridge_.OC := self;
      bridge_.OD := p;
      bridge_.R_ := Result;
      if p^.Mode = rtmPost then
          TCompute.Sync_Wait_PostM1(bridge_.Do_Sync_Run)
      else
          TCompute.SyncM(bridge_.Do_Sync_Run);
      Result := bridge_.R_;
      DisposeObject(bridge_);
    end;
end;

{ op_Add_Prefix }

function op_Add_Prefix.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result + Param[i]^.Value;
  Result := - - Result;
end;

{ op_Sub_Prefix }

function op_Sub_Prefix.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result + Param[i]^.Value;
  Result := -Result;
end;

{ op_Add }

function op_Add.DoExecute(opRT: TOpCustomRunTime): Variant;

  function Fast_VarIsStr(var v: Variant): Boolean;
  var
    p: pVarData;
  begin
    // optimized
    p := @TVarData(v);
    while p^.VType = varByRef or varVariant do
        p := pVarData(p^.VPointer);
    Result := (p^.VType = varOleStr) or (p^.VType = varString) or (p^.VType = varUString);
  end;

var
  i: Integer;
  n1, n2: TPascalString;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;

  if Fast_VarIsStr(Result) then
    begin
      // optimized
      n1 := VarToStr(Result);
      for i := 1 to Count - 1 do
        begin
          try
              n1.Append(VarToStr(Param[i]^.Value));
          except
          end;
        end;
      Result := n1.Text;
    end
  else
    begin
      for i := 1 to Count - 1 do
        begin
          try
            if Fast_VarIsStr(Result) then
              begin
                // SystemString combine
                n1 := VarToStr(Result);
                if not umlIsNumber(n1) then
                  begin
                    Result := n1 + VarToStr(Param[i]^.Value);
                    Continue;
                  end
              end;

            if Fast_VarIsStr(Param[i]^.Value) then
              begin
                // SystemString combine
                n2 := VarToStr(Param[i]^.Value);
                if not umlIsNumber(n2) then
                  begin
                    Result := VarToStr(Result) + n2;
                    Continue;
                  end
              end;

            // logic compute
            Result := Result + Param[i]^.Value;
          except
          end;
        end;
    end;
end;

{ op_Sub }

function op_Sub.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result - Param[i]^.Value;
end;

{ op_Mul }

function op_Mul.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result * Param[i]^.Value;
end;

{ op_Div }

function op_Div.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result / Param[i]^.Value;
end;

{ op_IntDiv }

function op_IntDiv.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result div Param[i]^.Value;
end;

{ op_Pow }
function op_Pow.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Power(Result, Param[i]^.Value);
end;

{ op_Mod }

function op_Mod.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result mod Param[i]^.Value;
end;

{ op_Or }

function op_Or.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result or Param[i]^.Value;
end;

{ op_And }

function op_And.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result and Param[i]^.Value;
end;

{ op_Xor }

function op_Xor.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result xor Param[i]^.Value;
end;

{ op_shl }

function op_Shl.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := UInt64(Result) shl UInt64(Param[i]^.Value);
end;

{ op_shr }

function op_Shr.DoExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := UInt64(Result) shr UInt64(Param[i]^.Value);
end;

{ op_Equal }

function op_Equal.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value = Param[1]^.Value;
end;

{ op_LessThan }

function op_LessThan.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value < Param[1]^.Value;
end;

{ op_EqualOrLessThan }

function op_EqualOrLessThan.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value <= Param[1]^.Value;
end;

{ op_GreaterThan }

function op_GreaterThan.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value > Param[1]^.Value;
end;

{ op_EqualOrGreaterThan }

function op_EqualOrGreaterThan.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value >= Param[1]^.Value;
end;

{ op_NotEqual }

function op_NotEqual.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value <> Param[1]^.Value;
end;

{ op_Symbol_Sub }

function op_Symbol_Sub.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := -Param[0]^.Value;
end;

{ op_Symbol_Add }

function op_Symbol_Add.DoExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value;
end;

initialization

OpSystemAPI := TOpSystemAPI.Create;
SystemOpRunTime := TOpCustomRunTime.Create;
OpSystemAPI.RegistationSystemAPI(SystemOpRunTime);
OleVariantInt64AsDouble := True;
OpRegTool := TOpReg_Tool.Create;

RegisterOp(op_Value);
RegisterOp(op_Proc);
RegisterOp(op_Add_Prefix);
RegisterOp(op_Sub_Prefix);
RegisterOp(op_Add);
RegisterOp(op_Sub);
RegisterOp(op_Mul);
RegisterOp(op_Div);
RegisterOp(op_IntDiv);
RegisterOp(op_Pow);
RegisterOp(op_Mod);
RegisterOp(op_Or);
RegisterOp(op_And);
RegisterOp(op_Xor);
RegisterOp(op_Shl);
RegisterOp(op_Shr);
RegisterOp(op_Equal);
RegisterOp(op_LessThan);
RegisterOp(op_EqualOrLessThan);
RegisterOp(op_GreaterThan);
RegisterOp(op_EqualOrGreaterThan);
RegisterOp(op_NotEqual);
RegisterOp(op_Symbol_Sub);
RegisterOp(op_Symbol_Add);

System_NonLinear_Pool := TOpCode_NonLinear_Pool.Create;
Hooked_OnCheckThreadSynchronize := Z.Core.OnCheckThreadSynchronize;
Z.Core.OnCheckThreadSynchronize := DoCheckThreadSynchronize;

finalization

Z.Core.OnCheckThreadSynchronize := Hooked_OnCheckThreadSynchronize;

DisposeObject(System_NonLinear_Pool);
DisposeObject(OpSystemAPI);
DisposeObject(SystemOpRunTime);
FreeOpRegClass;

end.
 
