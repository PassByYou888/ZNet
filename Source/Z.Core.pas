{ ****************************************************************************** }
{ * Core library                                                               * }
{ ****************************************************************************** }
unit Z.Core;

{$I Z.Define.inc}

interface

uses SysUtils, Classes, Types, Variants,
  {$IFDEF Parallel}
  {$IFNDEF FPC}
  {$IFDEF SystemParallel}
  Threading,
  {$ENDIF SystemParallel}
  {$ENDIF FPC}
  {$ENDIF Parallel}
  SyncObjs,
  {$IFDEF FPC}
  Z.FPC.GenericList, fgl,
  {$ELSE FPC}
  System.Generics.Collections,
  {$ENDIF FPC}
  Math;

{$Region 'core defines + class'}
type
  TBytes = SysUtils.TBytes;
  TPoint = Types.TPoint;
  TTimeTick = UInt64;
  PTimeTick = ^TTimeTick;
  TSeekOrigin = Classes.TSeekOrigin;
  TNotify = Classes.TNotifyEvent;
  TArrayBool = Array of Boolean;
  TArrayInt64 = Array of Int64;
  TArrayUInt64 = Array of UInt64;
  TInt64Buffer = Array [0 .. MaxInt div SizeOf(Int64) - 1] of Int64;
  PInt64Buffer = ^TInt64Buffer;
  TUInt64Buffer = Array [0 .. MaxInt div SizeOf(UInt64) - 1] of UInt64;
  PUInt64Buffer = ^TUInt64Buffer;

  TCore_Object = TObject;
  TCore_Persistent = TPersistent;
  TCore_Stream = TStream;
  TCore_FileStream = TFileStream;
  TCore_StringStream = TStringStream;
  TCore_ResourceStream = TResourceStream;
  TCore_Thread = TThread;
  TCore_MemoryStream = TMemoryStream;
  TCore_Strings = TStrings;
  TCore_StringList = TStringList;
  TCore_Reader = TReader;
  TCore_Writer = TWriter;
  TCore_Component = TComponent;
  Core_Exception = Exception;

  TExecutePlatform = (epWin32, epWin64, epOSX32, epOSX64, epIOS, epIOSSIM, epANDROID32, epANDROID64, epLinux64, epLinux32, epUnknow);

  {$IFDEF FPC}
  // freepascal
  PUInt64 = ^UInt64;

  TCore_InterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef: longint; {$IFNDEF WINDOWS} cdecl {$ELSE WINDOWS} stdcall {$ENDIF WINDOWS};
    function _Release: longint; {$IFNDEF WINDOWS} cdecl {$ELSE WINDOWS} stdcall {$ENDIF WINDOWS};
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  PCore_PointerList = Classes.PPointerList;
  TCore_PointerList = Classes.TPointerList;
  TCore_ListSortCompare = Classes.TListSortCompare;
  TCore_ListNotification = Classes.TListNotification;

  TCore_List = class(TList)
    property ListData: PPointerList read GetList;
  end;

  TCore_ListForObj = specialize TGenericsList<TCore_Object>;
  TCore_ForObjectList = array of TCore_Object;
  PCore_ForObjectList = ^TCore_ForObjectList;
  {$ELSE FPC}
  // delphi
  TCore_InterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TGenericsList<t> = class(System.Generics.Collections.TList<t>)
  private type
    TGArry = array of t;
  public var Arry:TGArry;
    function ListData: Pointer;
  end;

  TGenericsObjectList<t: class> = class(System.Generics.Collections.TList<t>)
  private type
    TGArry = array of t;
  public var Arry:TGArry;
    function ListData: Pointer;
  end;

  TCore_PointerList = array of Pointer;
  PCore_PointerList = ^TCore_PointerList;

  TCore_List = class(TGenericsList<Pointer>)
    function ListData: PCore_PointerList;
  end;

  TCore_ForObjectList = array of TCore_Object;
  PCore_ForObjectList = ^TCore_ForObjectList;

  TCore_ListForObj = class(TGenericsList<TCore_Object>)
    function ListData: PCore_ForObjectList;
  end;
  {$ENDIF FPC}

  TCore_ObjectList = class(TCore_ListForObj)
  public
    AutoFreeObj: Boolean;
    constructor Create; overload;
    constructor Create(AutoFreeObj_: Boolean); overload;
    destructor Destroy; override;

    procedure Remove(obj: TCore_Object);
    procedure Delete(index: Integer);
    procedure Clear;
  end;
{$EndRegion 'core defines + class'}
{$Region 'Critical'}
  TSoftCritical = class(TCore_Object)
  private
    L: Boolean;
  public
    constructor Create;
    procedure Acquire; virtual;
    procedure Release; virtual;
    procedure Enter; virtual;
    procedure Leave; virtual;
  end;

{$IFDEF SoftCritical}
  TCritical_ = TSoftCritical;
{$ELSE SoftCritical}
  TCritical_ = TCriticalSection;
{$ENDIF SoftCritical}

{$IFDEF FPC}generic{$ENDIF FPC}TAtomVar<T_> = class
  public type
    PT_ = ^T_;
  private
    FValue__: T_;
    Critical: TCritical_;
    function GetValue: T_;
    procedure SetValue(const Value_: T_);
    function GetValueP: PT_;
  public
    constructor Create(Value_: T_);
    destructor Destroy; override;
    // operation
    function Lock: T_;
    function LockP: PT_;
    property P: PT_ read GetValueP;
    property Pointer_: PT_ read GetValueP;
    procedure UnLock(const Value_: T_); overload;
    procedure UnLock(const Value_: PT_); overload;
    procedure UnLock(); overload;
    // value work in atom read and write
    property V: T_ read GetValue write SetValue;
    property Value: T_ read GetValue write SetValue;
  end;
  // Bool
  TAtomBoolean = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Boolean>;
  TAtomBool = TAtomBoolean;
  // number
  TAtomSmallInt = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<SmallInt>;
  TAtomShortInt = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<ShortInt>;
  TAtomInteger = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Integer>;
  TAtomInt8 = TAtomSmallInt;
  TAtomInt16 = TAtomShortInt;
  TAtomInt32 = TAtomInteger;
  TAtomInt = TAtomInteger;
  TAtomInt64 = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Int64>;
  TAtomByte = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Byte>;
  TAtomWord = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Word>;
  TAtomCardinal = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Cardinal>;
  TAtomUInt8 = TAtomByte;
  TAtomUInt16 = TAtomWord;
  TAtomUInt32 = TAtomCardinal;
  TAtomDWord = TAtomCardinal;
  TAtomUInt64 = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<UInt64>;
  // float
  TAtomSingle = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Single>;
  TAtomFloat = TAtomSingle;
  TAtomDouble = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Double>;
  TAtomExtended = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Extended>;
  // string
  TAtomString = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<string>;

  TCritical = class(TCritical_)
  private
    LNum: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Acquire;
    procedure Release;
    procedure Enter;
    procedure Leave;
    procedure Lock;
    procedure UnLock;
    function IsBusy: Boolean;
    property Busy: Boolean read IsBusy;
    // atom
    procedure Inc_(var x: Int64); overload;
    procedure Inc_(var x: Int64; const v: Int64); overload;
    procedure Dec_(var x: Int64); overload;
    procedure Dec_(var x: Int64; const v: Int64); overload;
    procedure Inc_(var x: UInt64); overload;
    procedure Inc_(var x: UInt64; const v: UInt64); overload;
    procedure Dec_(var x: UInt64); overload;
    procedure Dec_(var x: UInt64; const v: UInt64); overload;
    procedure Inc_(var x: Integer); overload;
    procedure Inc_(var x: Integer; const v:Integer); overload;
    procedure Dec_(var x: Integer); overload;
    procedure Dec_(var x: Integer; const v:Integer); overload;
    procedure Inc_(var x: Cardinal); overload;
    procedure Inc_(var x: Cardinal; const v:Cardinal); overload;
    procedure Dec_(var x: Cardinal); overload;
    procedure Dec_(var x: Cardinal; const v:Cardinal); overload;
  end;
{$EndRegion 'Critical'}
{$Region 'OrderStruct'}
  {$IFDEF FPC}generic{$ENDIF FPC}TOrderStruct<T_> = class(TCore_Object)
  public type
    T = T_;
    PT_ = ^T_;
    POrderStruct_ = ^TOrderStruct_;
    TOrderStruct_ = record
      Data: T_;
      Next: POrderStruct_;
    end;
    TOnFreeOrderStruct = procedure(var p: T_) of object;
  private
    FFirst: POrderStruct_;
    FLast: POrderStruct_;
    FNum: NativeInt;
    FOnFreeOrderStruct: TOnFreeOrderStruct;
    procedure DoInternalFree(p: POrderStruct_);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DoFree(var Data: T_); virtual;
    procedure Clear;
    property Current: POrderStruct_ read FFirst;
    property First: POrderStruct_ read FFirst;
    property Last: POrderStruct_ read FLast;
    procedure Next;
    procedure Push(Data: T_);
    property Num: NativeInt read FNum;
    property OnFreeOrderStruct: TOnFreeOrderStruct read FOnFreeOrderStruct write FOnFreeOrderStruct;
    property OnFree: TOnFreeOrderStruct read FOnFreeOrderStruct write FOnFreeOrderStruct;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}TOrderPtrStruct<T_> = class(TCore_Object)
  public type
    T = T_;
    PT_ = ^T_;
    POrderPtrStruct_ = ^TOrderPtrStruct_;
    TOrderPtrStruct_ = record
      Data: PT_;
      Next: POrderPtrStruct_;
    end;
    TOnFreeOrderPtrStruct = procedure(p: PT_) of object;
  private
    FFirst: POrderPtrStruct_;
    FLast: POrderPtrStruct_;
    FNum: NativeInt;
    FOnFreeOrderStruct: TOnFreeOrderPtrStruct;
    procedure DoInternalFree(p: POrderPtrStruct_);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DoFree(Data: PT_); virtual;
    procedure Clear;
    property Current: POrderPtrStruct_ read FFirst;
    property First: POrderPtrStruct_ read FFirst;
    property Last: POrderPtrStruct_ read FLast;
    procedure Next;
    procedure Push(Data: T_);
    procedure PushPtr(Data: PT_);
    property Num: NativeInt read FNum;
    property OnFreeOrderStruct: TOnFreeOrderPtrStruct read FOnFreeOrderStruct write FOnFreeOrderStruct;
    property OnFree: TOnFreeOrderPtrStruct read FOnFreeOrderStruct write FOnFreeOrderStruct;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}TCriticalOrderStruct<T_> = class(TCore_Object)
  public type
    T = T_;
    PT_ = ^T_;
    POrderStruct_ = ^TOrderStruct_;
    TOrderStruct_ = record
      Data: T_;
      Next: POrderStruct_;
    end;
    TOnFreeCriticalOrderStruct = procedure(var p: T_) of object;
  private
    FCritical: TCritical;
    FFirst: POrderStruct_;
    FLast: POrderStruct_;
    FNum: NativeInt;
    FOnFreeCriticalOrderStruct: TOnFreeCriticalOrderStruct;
    procedure DoInternalFree(p: POrderStruct_);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DoFree(var Data: T_); virtual;
    procedure Clear;
    function GetCurrent: POrderStruct_;
    property Current: POrderStruct_ read GetCurrent;
    property First: POrderStruct_ read GetCurrent;
    procedure Next;
    procedure Push(Data: T_);
    function GetNum: NativeInt;
    property Num: NativeInt read GetNum;
    property OnFreeCriticalOrderStruct: TOnFreeCriticalOrderStruct read FOnFreeCriticalOrderStruct write FOnFreeCriticalOrderStruct;
    property OnFree: TOnFreeCriticalOrderStruct read FOnFreeCriticalOrderStruct write FOnFreeCriticalOrderStruct;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}TCriticalOrderPtrStruct<T_> = class(TCore_Object)
  public type
    T = T_;
    PT_ = ^T_;
    POrderPtrStruct_ = ^TOrderPtrStruct_;
    TOrderPtrStruct_ = record
      Data: PT_;
      Next: POrderPtrStruct_;
    end;
    TOnFreeCriticalOrderPtrStruct = procedure(p: PT_) of object;
  private
    FCritical: TCritical;
    FFirst: POrderPtrStruct_;
    FLast: POrderPtrStruct_;
    FNum: NativeInt;
    FOnFreeCriticalOrderStruct: TOnFreeCriticalOrderPtrStruct;
    procedure DoInternalFree(p: POrderPtrStruct_);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DoFree(Data: PT_); virtual;
    procedure Clear;
    function GetCurrent: POrderPtrStruct_;
    property Current: POrderPtrStruct_ read GetCurrent;
    property First: POrderPtrStruct_ read GetCurrent;
    procedure Next;
    procedure Push(Data: T_);
    procedure PushPtr(Data: PT_);
    function GetNum: NativeInt;
    property Num: NativeInt read GetNum;
    property OnFreeCriticalOrderStruct: TOnFreeCriticalOrderPtrStruct read FOnFreeCriticalOrderStruct write FOnFreeCriticalOrderStruct;
    property OnFree: TOnFreeCriticalOrderPtrStruct read FOnFreeCriticalOrderStruct write FOnFreeCriticalOrderStruct;
  end;

{$EndRegion 'OrderStruct'}
{$Region 'ThreadPost'}
  TThreadPost_C1 = procedure();
  TThreadPost_C2 = procedure(Data1: Pointer);
  TThreadPost_C3 = procedure(Data1: Pointer; Data2: TCore_Object; Data3: Variant);
  TThreadPost_C4 = procedure(Data1: Pointer; Data2: TCore_Object);
  TThreadPost_M1 = procedure() of object;
  TThreadPost_M2 = procedure(Data1: Pointer) of object;
  TThreadPost_M3 = procedure(Data1: Pointer; Data2: TCore_Object; Data3: Variant) of object;
  TThreadPost_M4 = procedure(Data1: Pointer; Data2: TCore_Object) of object;
{$IFDEF FPC}
  TThreadPost_P1 = procedure() is nested;
  TThreadPost_P2 = procedure(Data1: Pointer) is nested;
  TThreadPost_P3 = procedure(Data1: Pointer; Data2: TCore_Object; Data3: Variant) is nested;
  TThreadPost_P4 = procedure(Data1: Pointer; Data2: TCore_Object) is nested;
{$ELSE FPC}
  TThreadPost_P1 = reference to procedure();
  TThreadPost_P2 = reference to procedure(Data1: Pointer);
  TThreadPost_P3 = reference to procedure(Data1: Pointer; Data2: TCore_Object; Data3: Variant);
  TThreadPost_P4 = reference to procedure(Data1: Pointer; Data2: TCore_Object);
{$ENDIF FPC}

  TThreadPostData = record
    On_C1: TThreadPost_C1;
    On_C2: TThreadPost_C2;
    On_C3: TThreadPost_C3;
    On_C4: TThreadPost_C4;
    On_M1: TThreadPost_M1;
    On_M2: TThreadPost_M2;
    On_M3: TThreadPost_M3;
    On_M4: TThreadPost_M4;
    On_P1: TThreadPost_P1;
    On_P2: TThreadPost_P2;
    On_P3: TThreadPost_P3;
    On_P4: TThreadPost_P4;
    Data1: Pointer;
    Data2: TCore_Object;
    Data3: Variant;
    procedure Init;
  end;

  TThreadPostDataOrder = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderPtrStruct<TThreadPostData>;

  TThreadPost = class(TCore_Object)
  protected
    FCritical: TCritical;
    FThreadID: TThreadID;
    FSyncPool: TThreadPostDataOrder;
    FProgressing: TAtomBool;
    FOneStep: Boolean;
    FResetRandomSeed: Boolean;
    procedure FreeThreadProgressPostData(p: TThreadPostDataOrder.PT_);
  public
    constructor Create(ThreadID_: TThreadID);
    destructor Destroy; override;
    property ThreadID: TThreadID read FThreadID write FThreadID;
    property OneStep: Boolean read FOneStep write FOneStep;
    property ResetRandomSeed: Boolean read FResetRandomSeed write FResetRandomSeed;
    property SyncPool: TThreadPostDataOrder read FSyncPool;
    function Count: Integer;
    function Busy: Boolean;

    function Progress(ThreadID_: TThreadID): Integer; overload;
    function Progress(Thread_: TThread): Integer; overload;
    function Progress(): Integer; overload;

    // post thread synchronization
    procedure PostC1(OnSync: TThreadPost_C1);
    procedure PostC2(Data1: Pointer; OnSync: TThreadPost_C2);
    procedure PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3);
    procedure PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4);

    procedure PostM1(OnSync: TThreadPost_M1);
    procedure PostM2(Data1: Pointer; OnSync: TThreadPost_M2);
    procedure PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3);
    procedure PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4);

    procedure PostP1(OnSync: TThreadPost_P1);
    procedure PostP2(Data1: Pointer; OnSync: TThreadPost_P2);
    procedure PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3);
    procedure PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4);
  end;

{$EndRegion 'ThreadPost'}
{$Region 'ComputeThread'}
  TCompute = class;

  TRunWithThread_C = procedure(ThSender: TCompute);
  TRunWithThread_M = procedure(ThSender: TCompute) of object;
  TRunWithThread_C_NP = procedure();
  TRunWithThread_M_NP = procedure() of object;
  {$IFDEF FPC}
  TRunWithThread_P = procedure(ThSender: TCompute) is nested;
  TRunWithThread_P_NP = procedure() is nested;
  {$ELSE FPC}
  TRunWithThread_P = reference to procedure(ThSender: TCompute);
  TRunWithThread_P_NP = reference to procedure();
  {$ENDIF FPC}

  TCompute = class(TCore_Thread)
  private
    OnRun_C: TRunWithThread_C;
    OnRun_M: TRunWithThread_M;
    OnRun_P: TRunWithThread_P;
    OnRun_C_NP: TRunWithThread_C_NP;
    OnRun_M_NP: TRunWithThread_M_NP;
    OnRun_P_NP: TRunWithThread_P_NP;
    OnDone_C: TRunWithThread_C;
    OnDone_M: TRunWithThread_M;
    OnDone_P: TRunWithThread_P;
    FRndInstance: Pointer;
  protected
    procedure Execute; override;
    procedure Done_Sync;
  public
    UserData: Pointer;
    UserObject: TCore_Object;

    constructor Create;
    destructor Destroy; override;
    class function ActivtedTask(): Integer;
    class function WaitTask(): Integer;
    class function TotalTask(): Integer;
    class function State(): string;
    class function GetParallelGranularity(): Integer;
    class function GetMaxActivtedParallel(): Integer;

    // build-in synchronization Proc
    class procedure Sync(const OnRun_: TRunWithThread_P_NP); overload;
    class procedure Sync(const Thread_: TThread; OnRun_: TRunWithThread_P_NP); overload;
    // build-in synchronization call
    class procedure SyncC(OnRun_: TRunWithThread_C_NP); overload;
    class procedure SyncC(const Thread_: TThread; OnRun_: TRunWithThread_C_NP); overload;
    // build-in synchronization method
    class procedure SyncM(OnRun_: TRunWithThread_M_NP); overload;
    class procedure SyncM(const Thread_: TThread; OnRun_: TRunWithThread_M_NP); overload;
    // build-in synchronization Proc
    class procedure SyncP(const OnRun_: TRunWithThread_P_NP); overload;
    class procedure SyncP(const Thread_: TThread; OnRun_: TRunWithThread_P_NP); overload;
    // build-in asynchronous call
    class procedure RunC(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_C); overload;
    class procedure RunC(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_C); overload;
    class procedure RunC(const OnRun: TRunWithThread_C); overload;
    class procedure RunC_NP(const OnRun: TRunWithThread_C_NP); overload;
    // build-in asynchronous methoc
    class procedure RunM(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_M); overload;
    class procedure RunM(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_M); overload;
    class procedure RunM(const OnRun: TRunWithThread_M); overload;
    class procedure RunM_NP(const OnRun: TRunWithThread_M_NP); overload;
    // build-in asynchronous proc
    class procedure RunP(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_P); overload;
    class procedure RunP(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_P); overload;
    class procedure RunP(const OnRun: TRunWithThread_P); overload;
    class procedure RunP_NP(const OnRun: TRunWithThread_P_NP); overload;

    // main thread
    class procedure ProgressPost();
    // post to main thread call
    class procedure PostC1(OnSync: TThreadPost_C1);
    class procedure PostC2(Data1: Pointer; OnSync: TThreadPost_C2);
    class procedure PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3);
    class procedure PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4);
    // post to main thread method
    class procedure PostM1(OnSync: TThreadPost_M1);
    class procedure PostM2(Data1: Pointer; OnSync: TThreadPost_M2);
    class procedure PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3);
    class procedure PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4);
    // post to main thread proc
    class procedure PostP1(OnSync: TThreadPost_P1);
    class procedure PostP2(Data1: Pointer; OnSync: TThreadPost_P2);
    class procedure PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3);
    class procedure PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4);
  end;

  // TCompute alias
  TComputeThread = TCompute;
{$EndRegion 'ComputeThread'}
{$Region 'MT19937Random'}
  TMT19937Random = class(TCore_Object)
  private
    FInternalCritical: TCritical;
    FRndInstance: Pointer;
    function GetSeed: Integer;
    procedure SetSeed(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Rndmize();
    function Rand32(L: Integer): Integer; overload;
    procedure Rand32(L: Integer; dest: PInteger; num: NativeInt); overload;
    function Rand64(L: Int64): Int64; overload;
    procedure Rand64(L: Int64; dest: PInt64; num: NativeInt); overload;
    function RandE: Extended; overload;
    procedure RandE(dest: PExtended; num: NativeInt); overload;
    function RandF: Single; overload;
    procedure RandF(dest: PSingle; num: NativeInt); overload;
    function RandD: Double; overload;
    procedure RandD(dest: PDouble; num: NativeInt); overload;
    function RandBool: Boolean;
    property seed: Integer read GetSeed write SetSeed;
  end;

  TRandom = TMT19937Random;
{$EndRegion 'MT19937Random'}
{$Region 'LineProcessor'}
  {$IFDEF FPC}generic{$ENDIF FPC}TLineProcessor<T_> = class
  public type
    TTArry_ = array [0 .. 0] of T_;
    PTArry_ = ^TTArry_;
    PT_ = ^T_;
  private var
    FData: PTArry_;
    FWidth, FHeight: NativeInt;
    FValue: T_;
    FLineTail: Boolean;
  public
    procedure CreateDone; virtual;
    constructor Create(const data_: Pointer; const width_, height_: NativeInt; const Value_: T_; const LineTail_: Boolean);
    destructor Destroy; override;
    procedure VertLine(X, y1, y2: NativeInt);
    procedure HorzLine(x1, Y, x2: NativeInt);
    procedure Line(x1, y1, x2, y2: NativeInt);
    procedure FillBox(x1, y1, x2, y2: NativeInt);
    procedure Process(const vp: PT_; const v: T_); virtual;
    property Value: T_ read FValue;
  end;
{$EndRegion 'LineProcessor'}
{$Region 'core const'}
const
  {$IF Defined(WIN32)}
  CurrentPlatform: TExecutePlatform = epWin32;
  {$ELSEIF Defined(WIN64)}
  CurrentPlatform: TExecutePlatform = epWin64;
  {$ELSEIF Defined(OSX)}
    {$IFDEF CPU64}
      CurrentPlatform: TExecutePlatform = epOSX64;
    {$ELSE CPU64}
      CurrentPlatform: TExecutePlatform = epOSX32;
    {$IFEND CPU64}
  {$ELSEIF Defined(IOS)}
    {$IFDEF CPUARM}
    CurrentPlatform: TExecutePlatform = epIOS;
    {$ELSE CPUARM}
    CurrentPlatform: TExecutePlatform = epIOSSIM;
    {$ENDIF CPUARM}
  {$ELSEIF Defined(ANDROID)}
    {$IFDEF CPU64}
    CurrentPlatform: TExecutePlatform = epANDROID64;
    {$ELSE CPU64}
    CurrentPlatform: TExecutePlatform = epANDROID32;
    {$IFEND CPU64}
  {$ELSEIF Defined(Linux)}
    {$IFDEF CPU64}
      CurrentPlatform: TExecutePlatform = epLinux64;
    {$ELSE CPU64}
      CurrentPlatform: TExecutePlatform = epLinux32;
    {$IFEND CPU64}
  {$ELSE}
  CurrentPlatform: TExecutePlatform = epUnknow;
  {$IFEND}

  CPU64 = {$IFDEF CPU64}True{$ELSE CPU64}False{$IFEND CPU64};
  X64 = CPU64;

  IsDebug = {$IFDEF DEBUG}True{$ELSE DEBUG}False{$ENDIF DEBUG};

  // timetick define
  C_Tick_Second = TTimeTick(1000);
  C_Tick_Minute = TTimeTick(C_Tick_Second) * 60;
  C_Tick_Hour   = TTimeTick(C_Tick_Minute) * 60;
  C_Tick_Day    = TTimeTick(C_Tick_Hour) * 24;
  C_Tick_Week   = TTimeTick(C_Tick_Day) * 7;
  C_Tick_Year   = TTimeTick(C_Tick_Day) * 365;

  // memory align
  C_MH_MemoryDelta = 0;

  // file mode
  fmCreate         = Classes.fmCreate;
  soFromBeginning  = Classes.soFromBeginning;
  soFromCurrent    = Classes.soFromCurrent;
  soFromEnd        = Classes.soFromEnd;
  fmOpenRead       = SysUtils.fmOpenRead;
  fmOpenWrite      = SysUtils.fmOpenWrite;
  fmOpenReadWrite  = SysUtils.fmOpenReadWrite;
  fmShareExclusive = SysUtils.fmShareExclusive;
  fmShareDenyWrite = SysUtils.fmShareDenyWrite;
  fmShareDenyNone  = SysUtils.fmShareDenyNone;
{$EndRegion 'core const'}
{$Region 'Parallel API'}

function GetParallelGranularity: Integer;
procedure SetParallelGranularity(Thread_Num: Integer);

{$IFDEF FPC}
  // freepascal
type
  TFPCParallel_P32 = procedure(pass: Integer) is nested;
  TFPCParallel_P64 = procedure(pass: Int64) is nested;
procedure FPCParallelFor_Block(parallel: Boolean; b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure FPCParallelFor_Block(parallel: Boolean; b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure FPCParallelFor_Fold(parallel: Boolean; b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure FPCParallelFor_Fold(parallel: Boolean; b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure FPCParallelFor(parallel: Boolean; b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure FPCParallelFor(parallel: Boolean; b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure FPCParallelFor(b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure FPCParallelFor(b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure FPCParallelFor(OnFor: TFPCParallel_P32; b, e: Integer); overload;
procedure FPCParallelFor(OnFor: TFPCParallel_P64; b, e: Int64); overload;
procedure FPCParallelFor(parallel: Boolean; OnFor: TFPCParallel_P32; b, e: Integer); overload;
procedure FPCParallelFor(parallel: Boolean; OnFor: TFPCParallel_P64; b, e: Int64); overload;
procedure ParallelFor(parallel: Boolean; b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure ParallelFor(parallel: Boolean; b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure ParallelFor(b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure ParallelFor(b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure ParallelFor(OnFor: TFPCParallel_P32; b, e: Integer); overload;
procedure ParallelFor(OnFor: TFPCParallel_P64; b, e: Int64); overload;
procedure ParallelFor(parallel: Boolean; OnFor: TFPCParallel_P32; b, e: Integer); overload;
procedure ParallelFor(parallel: Boolean; OnFor: TFPCParallel_P64; b, e: Int64); overload;
{$ELSE FPC}
type
  // delphi
{$IFDEF SystemParallel}
  TDelphiParallel_P32 = TProc<Integer>;
  TDelphiParallel_P64 = TProc<Int64>;
{$ELSE SystemParallel}
  TDelphiParallel_P32 = reference to procedure(pass: Integer);
  TDelphiParallel_P64 = reference to procedure(pass: Int64);
  procedure DelphiParallelFor_Block(parallel: Boolean; b, e: Integer; OnFor: TDelphiParallel_P32); overload;
  procedure DelphiParallelFor_Block(parallel: Boolean; b, e: Int64; OnFor: TDelphiParallel_P64); overload;
  procedure DelphiParallelFor_Fold(parallel: Boolean; b, e: Integer; OnFor: TDelphiParallel_P32); overload;
  procedure DelphiParallelFor_Fold(parallel: Boolean; b, e: Int64; OnFor: TDelphiParallel_P64); overload;
{$ENDIF SystemParallel}

procedure DelphiParallelFor(parallel: Boolean; b, e: Integer; OnFor: TDelphiParallel_P32); overload;
procedure DelphiParallelFor(parallel: Boolean; b, e: Int64; OnFor: TDelphiParallel_P64); overload;
procedure DelphiParallelFor(b, e: Integer; OnFor: TDelphiParallel_P32); overload;
procedure DelphiParallelFor(b, e: Int64; OnFor: TDelphiParallel_P64); overload;
procedure DelphiParallelFor(OnFor: TDelphiParallel_P32; b, e: Integer); overload;
procedure DelphiParallelFor(OnFor: TDelphiParallel_P64; b, e: Int64); overload;
procedure DelphiParallelFor(parallel: Boolean; OnFor: TDelphiParallel_P32; b, e: Integer); overload;
procedure DelphiParallelFor(parallel: Boolean; OnFor: TDelphiParallel_P64; b, e: Int64); overload;
procedure ParallelFor(parallel: Boolean; b, e: Integer; OnFor: TDelphiParallel_P32); overload;
procedure ParallelFor(parallel: Boolean; b, e: Int64; OnFor: TDelphiParallel_P64); overload;
procedure ParallelFor(b, e: Integer; OnFor: TDelphiParallel_P32); overload;
procedure ParallelFor(b, e: Int64; OnFor: TDelphiParallel_P64); overload;
procedure ParallelFor(OnFor: TDelphiParallel_P32; b, e: Integer); overload;
procedure ParallelFor(OnFor: TDelphiParallel_P64; b, e: Int64); overload;
procedure ParallelFor(parallel: Boolean; OnFor: TDelphiParallel_P32; b, e: Integer); overload;
procedure ParallelFor(parallel: Boolean; OnFor: TDelphiParallel_P64; b, e: Int64); overload;
{$ENDIF FPC}

{$EndRegion 'Parallel API'}
{$Region 'core api'}

// NoP = No Operation. It's the empty function, whose purpose is only for the
// debugging, or for the piece of code where intentionaly nothing is planned to be.
procedure Nop;

// process Synchronize
procedure CheckThreadSynchronize; overload;
function CheckThreadSynchronize(Timeout: Integer): Boolean; overload;
procedure CheckThreadSync; overload;
function CheckThreadSync(Timeout: Integer): Boolean; overload;
procedure CheckThread; overload;
function CheckThread(Timeout: Integer): Boolean; overload;

// core thread pool
procedure FreeCoreThreadPool;

procedure DisposeObject(const Obj: TObject); overload;
procedure DisposeObject(const objs: array of TObject); overload;
procedure FreeObj(const Obj: TObject);
procedure FreeObject(const Obj: TObject); overload;
procedure FreeObject(const objs: array of TObject); overload;
procedure DisposeObjectAndNil(var Obj);
procedure FreeObjAndNil(var Obj);

procedure LockObject(Obj: TObject);
procedure UnLockObject(Obj: TObject);

function DeltaStep(const value_, Delta_: NativeInt): NativeInt;
procedure AtomInc(var x: Int64); overload;
procedure AtomInc(var x: Int64; const v: Int64); overload;
procedure AtomDec(var x: Int64); overload;
procedure AtomDec(var x: Int64; const v: Int64); overload;
procedure AtomInc(var x: UInt64); overload;
procedure AtomInc(var x: UInt64; const v: UInt64); overload;
procedure AtomDec(var x: UInt64); overload;
procedure AtomDec(var x: UInt64; const v: UInt64); overload;
procedure AtomInc(var x: Integer); overload;
procedure AtomInc(var x: Integer; const v:Integer); overload;
procedure AtomDec(var x: Integer); overload;
procedure AtomDec(var x: Integer; const v:Integer); overload;
procedure AtomInc(var x: Cardinal); overload;
procedure AtomInc(var x: Cardinal; const v:Cardinal); overload;
procedure AtomDec(var x: Cardinal); overload;
procedure AtomDec(var x: Cardinal; const v:Cardinal); overload;

procedure FillPtrByte(const dest:Pointer; Count: NativeUInt; const Value: Byte);
procedure FillPtr(const dest:Pointer; Count: NativeUInt; const Value: Byte);
procedure FillByte(const dest:Pointer; Count: NativeUInt; const Value: Byte);
function CompareMemory(const p1, p2: Pointer; Count: NativeUInt): Boolean;
procedure CopyPtr(const sour, dest: Pointer; Count: NativeUInt);

procedure RaiseInfo(const n: string); overload;
procedure RaiseInfo(const n: string; const Args: array of const); overload;

function IsMobile: Boolean;

function GetTimeTick(): TTimeTick;
function GetTimeTickCount(): TTimeTick;
function GetCrashTimeTick(): TTimeTick;
function SameF(const A, B: Double; Epsilon: Double = 0): Boolean; overload;
function SameF(const A, B: Single; Epsilon: Single = 0): Boolean; overload;

// MT19937 random num
function MT19937CoreToDelphi: Boolean;
function MT19937InstanceNum(): Integer;
procedure SetMT19937Seed(seed: Integer);
function GetMT19937Seed(): Integer;
procedure MT19937Randomize();
function MT19937Rand32(L: Integer): Integer; overload;
procedure MT19937Rand32(L: Integer; dest: PInteger; num: NativeInt); overload;
function MT19937Rand64(L: Int64): Int64; overload;
procedure MT19937Rand64(L: Int64; dest: PInt64; num: NativeInt); overload;
function MT19937RandE: Extended; overload;
procedure MT19937RandE(dest: PExtended; num: NativeInt); overload;
function MT19937RandF: Single; overload;
procedure MT19937RandF(dest: PSingle; num: NativeInt); overload;
function MT19937RandD: Double; overload;
procedure MT19937RandD(dest: PDouble; num: NativeInt); overload;
procedure MT19937SaveToStream(stream: TCore_Stream);
procedure MT19937LoadFromStream(stream: TCore_Stream);

function ROL8(const Value: Byte; Shift: Byte): Byte;
function ROL16(const Value: Word; Shift: Byte): Word;
function ROL32(const Value: Cardinal; Shift: Byte): Cardinal;
function ROL64(const Value: UInt64; Shift: Byte): UInt64;
function ROR8(const Value: Byte; Shift: Byte): Byte;
function ROR16(const Value: Word; Shift: Byte): Word;
function ROR32(const Value: Cardinal; Shift: Byte): Cardinal;
function ROR64(const Value: UInt64; Shift: Byte): UInt64;

function Endian(const Value: SmallInt): SmallInt; overload;
function Endian(const Value: Word): Word; overload;
function Endian(const Value: Integer): Integer; overload;
function Endian(const Value: Cardinal): Cardinal; overload;
function Endian(const Value: Int64): Int64; overload;
function Endian(const Value: UInt64): UInt64; overload;

function BE2N(const Value: SmallInt): SmallInt; overload;
function BE2N(const Value: Word): Word; overload;
function BE2N(const Value: Integer): Integer; overload;
function BE2N(const Value: Cardinal): Cardinal; overload;
function BE2N(const Value: Int64): Int64; overload;
function BE2N(const Value: UInt64): UInt64; overload;

function LE2N(const Value: SmallInt): SmallInt; overload;
function LE2N(const Value: Word): Word; overload;
function LE2N(const Value: Integer): Integer; overload;
function LE2N(const Value: Cardinal): Cardinal; overload;
function LE2N(const Value: Int64): Int64; overload;
function LE2N(const Value: UInt64): UInt64; overload;

function N2BE(const Value: SmallInt): SmallInt; overload;
function N2BE(const Value: Word): Word; overload;
function N2BE(const Value: Integer): Integer; overload;
function N2BE(const Value: Cardinal): Cardinal; overload;
function N2BE(const Value: Int64): Int64; overload;
function N2BE(const Value: UInt64): UInt64; overload;

function N2LE(const Value: SmallInt): SmallInt; overload;
function N2LE(const Value: Word): Word; overload;
function N2LE(const Value: Integer): Integer; overload;
function N2LE(const Value: Cardinal): Cardinal; overload;
function N2LE(const Value: Int64): Int64; overload;
function N2LE(const Value: UInt64): UInt64; overload;

procedure Swap(var v1, v2: Byte); overload;
procedure Swap(var v1, v2: Word); overload;
procedure Swap(var v1, v2: Integer); overload;
procedure Swap(var v1, v2: Cardinal); overload;
procedure Swap(var v1, v2: Int64); overload;
procedure Swap(var v1, v2: UInt64); overload;
{$IFDEF OVERLOAD_NATIVEINT}
procedure Swap(var v1, v2: NativeInt); overload;
procedure Swap(var v1, v2: NativeUInt); overload;
{$ENDIF OVERLOAD_NATIVEINT}
procedure Swap(var v1, v2: string); overload;
procedure Swap(var v1, v2: Single); overload;
procedure Swap(var v1, v2: Double); overload;
procedure Swap(var v1, v2: Pointer); overload;
procedure SwapVariant(var v1, v2: Variant);
function Swap(const v: Word): Word; overload;
function Swap(const v: Cardinal): Cardinal; overload;
function Swap(const v: UInt64): UInt64; overload;

function SAR16(const Value: SmallInt; const Shift: Byte): SmallInt;
function SAR32(const Value: Integer; Shift: Byte): Integer;
function SAR64(const Value: Int64; Shift: Byte): Int64;

function MemoryAlign(addr: Pointer; alignment_: NativeUInt): Pointer;

function if_(const bool_: Boolean; const True_, False_: Boolean): Boolean; overload;
function if_(const bool_: Boolean; const True_, False_: ShortInt): ShortInt; overload;
function if_(const bool_: Boolean; const True_, False_: SmallInt): SmallInt; overload;
function if_(const bool_: Boolean; const True_, False_: Integer): Integer; overload;
function if_(const bool_: Boolean; const True_, False_: Int64): Int64; overload;
function if_(const bool_: Boolean; const True_, False_: Byte): Byte; overload;
function if_(const bool_: Boolean; const True_, False_: Word): Word; overload;
function if_(const bool_: Boolean; const True_, False_: Cardinal): Cardinal; overload;
function if_(const bool_: Boolean; const True_, False_: UInt64): UInt64; overload;
function if_(const bool_: Boolean; const True_, False_: Single): Single; overload;
function if_(const bool_: Boolean; const True_, False_: Double): Double; overload;
function if_(const bool_: Boolean; const True_, False_: string): string; overload;
function ifv_(const bool_: Boolean; const True_, False_: Variant): Variant;
function GetOffset(p_: Pointer; offset_: NativeInt): Pointer; inline;
function GetPtr(p_: Pointer; offset_: NativeInt): Pointer; inline;

{$EndRegion 'core api'}
{$Region 'core var'}

type TOnCheckThreadSynchronize = procedure();

var
  OnCheckThreadSynchronize: TOnCheckThreadSynchronize;

  // DelphiParallelFor and FPCParallelFor work in parallel
  WorkInParallelCore: TAtomBool;
  // same WorkInParallelCore
  ParallelCore: TAtomBool;

  // default is True
  GlobalMemoryHook: TAtomBool;

  // core init time
  CoreInitedTimeTick: TTimeTick;

  // The life time of working in asynchronous thread consistency,
  MT19937LifeTime: TTimeTick;

  // MainThread TThreadPost
  MainThreadProgress: TThreadPost;
  MainThreadPost: TThreadPost;
  SysProgress: TThreadPost;
{$EndRegion 'core var'}
{$Region 'compatible'}
{$I Z.Core.Compatible.inc}
{$EndRegion 'compatible'}

implementation

{$I Z.Core.Atomic.inc}
{$I Z.Core.MT19937.inc}

procedure DisposeObject(const Obj: TObject);
begin
  if Obj <> nil then
    begin
      try
        {$IFDEF AUTOREFCOUNT}
        Obj.DisposeOf;
        {$ELSE AUTOREFCOUNT}
        Obj.Free;
        {$ENDIF AUTOREFCOUNT}
        {$IFDEF CriticalSimulateAtomic}
        _RecycleLocker(Obj);
        {$ENDIF CriticalSimulateAtomic}
      except
      end;
    end;
end;

procedure DisposeObject(const objs: array of TObject);
var
  Obj: TObject;
begin
  for Obj in objs do
      DisposeObject(Obj);
end;

procedure FreeObj(const Obj: TObject);
begin
  DisposeObject(Obj);
end;

procedure FreeObject(const Obj: TObject);
begin
  DisposeObject(Obj);
end;

procedure FreeObject(const objs: array of TObject);
var
  Obj: TObject;
begin
  for Obj in objs do
      DisposeObject(Obj);
end;

procedure DisposeObjectAndNil(var Obj);
begin
  if TObject(Obj) <> nil then
    begin
      DisposeObject(TObject(Obj));
      TObject(Obj) := nil;
    end;
end;

procedure FreeObjAndNil(var Obj);
begin
  DisposeObjectAndNil(Obj);
end;

procedure LockObject(Obj: TObject);
{$IFNDEF CriticalSimulateAtomic}
{$IFDEF ANTI_DEAD_ATOMIC_LOCK}
var
  d: TTimeTick;
{$ENDIF ANTI_DEAD_ATOMIC_LOCK}
{$ENDIF CriticalSimulateAtomic}
begin
{$IFDEF FPC}
  _LockCriticalObj(Obj);
{$ELSE FPC}
{$IFDEF CriticalSimulateAtomic}
  _LockCriticalObj(Obj);
{$ELSE CriticalSimulateAtomic}
  {$IFDEF ANTI_DEAD_ATOMIC_LOCK}
  d := GetTimeTick;
  TMonitor.Enter(Obj, 5000);
  if GetTimeTick - d >= 5000 then
      RaiseInfo('dead');
  {$ELSE ANTI_DEAD_ATOMIC_LOCK}
  TMonitor.Enter(Obj);
  {$ENDIF ANTI_DEAD_ATOMIC_LOCK}
{$ENDIF CriticalSimulateAtomic}
{$ENDIF FPC}
end;

procedure UnLockObject(Obj: TObject);
begin
{$IFDEF FPC}
  _UnLockCriticalObj(Obj);
{$ELSE FPC}
  {$IFDEF CriticalSimulateAtomic}
  _UnLockCriticalObj(Obj);
  {$ELSE CriticalSimulateAtomic}
  TMonitor.Exit(Obj);
  {$ENDIF CriticalSimulateAtomic}
{$ENDIF FPC}
end;

procedure FillPtrByte(const dest: Pointer; Count: NativeUInt; const Value: Byte);
{$IFDEF FillPtr_Used_FillChar}
begin
  FillChar(dest^, Count, Value);
end;
{$ELSE FillPtr_Used_FillChar}
var
  d: PByte;
  v: UInt64;
begin
  if Count = 0 then
      Exit;
  v := Value or (Value shl 8) or (Value shl 16) or (Value shl 24);
  v := v or (v shl 32);
  d := dest;
  while Count >= 8 do
    begin
      PUInt64(d)^ := v;
      dec(Count, 8);
      inc(d, 8);
    end;
  if Count >= 4 then
    begin
      PCardinal(d)^ := PCardinal(@v)^;
      dec(Count, 4);
      inc(d, 4);
    end;
  if Count >= 2 then
    begin
      PWORD(d)^ := PWORD(@v)^;
      dec(Count, 2);
      inc(d, 2);
    end;
  if Count > 0 then
      d^ := Value;
end;
{$ENDIF FillPtr_Used_FillChar}

procedure FillPtr(const dest:Pointer; Count: NativeUInt; const Value: Byte);
begin
  FillPtrByte(dest, Count, Value);
end;

procedure FillByte(const dest:Pointer; Count: NativeUInt; const Value: Byte);
begin
  FillPtrByte(dest, Count, Value);
end;

function CompareMemory(const p1, p2: Pointer; Count: NativeUInt): Boolean;
var
  b1, b2: PByte;
begin;
  if Count = 0 then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
  b1 := p1;
  b2 := p2;
  while (Count >= 8) do
    begin
      if PUInt64(b2)^ <> PUInt64(b1)^ then
          Exit;
      dec(Count, 8);
      inc(b2, 8);
      inc(b1, 8);
    end;
  if Count >= 4 then
    begin
      if PCardinal(b2)^ <> PCardinal(b1)^ then
          Exit;
      dec(Count, 4);
      inc(b2, 4);
      inc(b1, 4);
    end;
  if Count >= 2 then
    begin
      if PWORD(b2)^ <> PWORD(b1)^ then
          Exit;
      dec(Count, 2);
      inc(b2, 2);
      inc(b1, 2);
    end;
  if Count > 0 then
    if b2^ <> b1^ then
        Exit;
  Result := True;
end;

procedure CopyPtr(const sour, dest: Pointer; Count: NativeUInt);
{$IFDEF CopyPtr_Used_Move}
begin
  Move(sour^, dest^, Count);
end;
{$ELSE CopyPtr_Used_Move}
var
  s, d: NativeUInt;
begin
  if Count = 0 then
      exit;
  if sour = dest then
      exit;

  s := NativeUInt(sour);
  d := NativeUInt(dest);
  // overlap solve
  // thanks,qq122742470,wang
  // thanks,qq4700653,LOK
  if d > s then
    begin
      inc(s, Count);
      inc(d, Count);
      while Count >= 8 do
        begin
          dec(d, 8);
          dec(s, 8);
          dec(Count, 8);
          PUInt64(d)^ := PUInt64(s)^;
        end;
      if Count >= 4 then
        begin
          dec(d, 4);
          dec(s, 4);
          dec(Count, 4);
          PCardinal(d)^ := PCardinal(s)^;
        end;
      if Count >= 2 then
        begin
          dec(d, 2);
          dec(s, 2);
          dec(Count, 2);
          PWORD(d)^ := PWORD(s)^;
        end;
      if Count > 0 then
          PByte(d - 1)^ := PByte(s - 1)^;
    end
  else
    begin
      while Count >= 8 do
        begin
          PUInt64(d)^ := PUInt64(s)^;
          dec(Count, 8);
          inc(d, 8);
          inc(s, 8);
        end;
      if Count >= 4 then
        begin
          PCardinal(d)^ := PCardinal(s)^;
          dec(Count, 4);
          inc(d, 4);
          inc(s, 4);
        end;
      if Count >= 2 then
        begin
          PWORD(d)^ := PWORD(s)^;
          dec(Count, 2);
          inc(d, 2);
          inc(s, 2);
        end;
      if Count > 0 then
          PByte(d)^ := PByte(s)^;
    end;
end;
{$ENDIF CopyPtr_Used_Move}

procedure RaiseInfo(const n: string);
begin
  raise Exception.Create(n);
end;

procedure RaiseInfo(const n: string; const Args: array of const);
begin
  raise Exception.Create(Format(n, Args));
end;

function IsMobile: Boolean;
begin
  case CurrentPlatform of
    epIOS, epIOSSIM, epANDROID32, epANDROID64: Result := True;
    else Result := False;
  end;
end;

var
  Core_RunTime_Tick: TTimeTick;
  Core_Step_Tick: Cardinal;

function GetTimeTick(): TTimeTick;
var
  tick: Cardinal;
begin
  CoreTimeTickCritical.Acquire;
  try
    tick := TCore_Thread.GetTickCount();
    inc(Core_RunTime_Tick, tick - Core_Step_Tick);
    Core_Step_Tick := tick;
    Result := Core_RunTime_Tick;
  finally
      CoreTimeTickCritical.Release;
  end;
end;

function GetTimeTickCount(): TTimeTick;
begin
  Result := GetTimeTick();
end;

function GetCrashTimeTick(): TTimeTick;
begin
  Result := $FFFFFFFFFFFFFFFF - GetTimeTick();
end;

function SameF(const A, B: Double; Epsilon: Double = 0): Boolean;
const
  C_DoubleResolution = 1E-15 * 1000;
begin
  if Epsilon = 0 then
      Epsilon := Max(Min(Abs(A), Abs(B)) * C_DoubleResolution, C_DoubleResolution);
  if A > B then
      Result := (A - B) <= Epsilon
  else
      Result := (B - A) <= Epsilon;
end;

function SameF(const A, B: Single; Epsilon: Single = 0): Boolean;
const
  C_SingleResolution = 1E-7 * 1000;
begin
  if Epsilon = 0 then
      Epsilon := Max(Min(Abs(A), Abs(B)) * C_SingleResolution, C_SingleResolution);
  if A > B then
      Result := (A - B) <= Epsilon
  else
      Result := (B - A) <= Epsilon;
end;

{$I Z.Core.Endian.inc}

{$IFDEF FPC}

function TCore_InterfacedObject._AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := 1;
end;

function TCore_InterfacedObject._Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := 1;
end;

procedure TCore_InterfacedObject.AfterConstruction;
begin
end;

procedure TCore_InterfacedObject.BeforeDestruction;
begin
end;

{$ELSE}


function TCore_InterfacedObject._AddRef: Integer;
begin
  Result := 1;
end;

function TCore_InterfacedObject._Release: Integer;
begin
  Result := 1;
end;

procedure TCore_InterfacedObject.AfterConstruction;
begin
end;

procedure TCore_InterfacedObject.BeforeDestruction;
begin
end;

function TGenericsList<t>.ListData: Pointer;
begin
  // set array pointer
  Arry := TGArry(Pointer(inherited List));
  // @ array
  Result := @Arry;
end;

function TGenericsObjectList<t>.ListData: Pointer;
begin
  // set array pointer
  Arry := TGArry(Pointer(inherited List));
  // @ array
  Result := @Arry;
end;

function TCore_List.ListData: PCore_PointerList;
begin
  Result := PCore_PointerList(inherited ListData);
end;

function TCore_ListForObj.ListData: PCore_ForObjectList;
begin
  Result := PCore_ForObjectList(inherited ListData);
end;

{$ENDIF}

constructor TCore_ObjectList.Create;
begin
  inherited Create;
  AutoFreeObj := True;
end;

constructor TCore_ObjectList.Create(AutoFreeObj_: Boolean);
begin
  inherited Create;
  AutoFreeObj := AutoFreeObj_;
end;

destructor TCore_ObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCore_ObjectList.Remove(obj: TCore_Object);
begin
  if AutoFreeObj then
      DisposeObject(obj);
  inherited Remove(obj);
end;

procedure TCore_ObjectList.Delete(index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      if AutoFreeObj then
          disposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TCore_ObjectList.Clear;
var
  i: Integer;
begin
  if AutoFreeObj then
    for i := 0 to Count - 1 do
        disposeObject(Items[i]);
  inherited Clear;
end;

{$I Z.Core.ThreadPost.inc}
{$I Z.Core.ComputeThread.inc}

{$IFDEF FPC}
{$I Z.Core.FPCParallelFor.inc}
{$ELSE FPC}
{$I Z.Core.DelphiParallelFor.inc}
{$ENDIF FPC}
{$I Z.Core.AtomVar.inc}
{$I Z.Core.LineProcessor.inc}
{$I Z.Core.OrderData.inc}

function GetParallelGranularity: Integer;
begin
  Result := ParallelGranularity;
end;

procedure SetParallelGranularity(Thread_Num: Integer);
begin
  ParallelGranularity := Thread_Num;
end;

procedure Nop;
begin
end;

procedure CheckThreadSynchronize;
begin
  CheckThreadSynchronize(0);
end;

var
  MainThSynchronizeRunning: Boolean;

function CheckThreadSynchronize(Timeout: Integer): Boolean;
begin
  if TCore_Thread.CurrentThread.ThreadID <> MainThreadID then
    begin
      if Timeout > 0 then
        TCore_Thread.Sleep(Timeout);
      Result := False;
    end
  else
    begin
      if MainThSynchronizeRunning then
        Exit;
      MainThSynchronizeRunning := True;
      MainThreadProgress.Progress(MainThreadID);
      try
        Result := CheckSynchronize(Timeout);
      except
        Result := False;
      end;
      MainThSynchronizeRunning := False;
    end;

  if MainThSynchronizeRunning then
    Exit;
  MainThSynchronizeRunning := True;
  try
    if Assigned(OnCheckThreadSynchronize) then
      OnCheckThreadSynchronize();
  except
  end;
  MainThSynchronizeRunning := False;
end;

procedure CheckThreadSync;
begin
  CheckThreadSynchronize(0);
end;

function CheckThreadSync(Timeout: Integer): Boolean;
begin
  Result := CheckThreadSynchronize(Timeout);
end;

procedure CheckThread;
begin
  CheckThreadSynchronize(0);
end;

function CheckThread(Timeout: Integer): Boolean;
begin
  Result := CheckThreadSynchronize(Timeout);
end;

initialization
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  OnCheckThreadSynchronize := nil;
  WorkInParallelCore := TAtomBool.Create({$IFDEF FPC}True{$ELSE FPC}DebugHook = 0{$ENDIF FPC});
  ParallelCore := WorkInParallelCore;
  GlobalMemoryHook := TAtomBool.Create(True);
  Core_RunTime_Tick := C_Tick_Day * 3;
  Core_Step_Tick := TCore_Thread.GetTickCount();
  InitCriticalLock();
  InitMT19937Rand();
  CoreInitedTimeTick := GetTimeTick();
  InitCoreThreadPool(CpuCount);
  MainThreadProgress := TThreadPost.Create(MainThreadID);
  MainThSynchronizeRunning := False;
  MainThreadPost := MainThreadProgress;
  SysProgress := MainThreadProgress;
finalization
  FreeCoreThreadPool;
  MainThreadProgress.Free;
  FreeMT19937Rand();
  FreeCriticalLock;
  WorkInParallelCore.Free;
  WorkInParallelCore := nil;
  GlobalMemoryHook.Free;
  GlobalMemoryHook := nil;
end.
