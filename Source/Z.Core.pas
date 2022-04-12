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
  // fpc
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
  TSoftCritical = class
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
  TSystem_Critical = TSoftCritical;
{$ELSE SoftCritical}
  TSystem_Critical = TCriticalSection;
{$ENDIF SoftCritical}
  TCritical = class;

{$IFDEF FPC}generic{$ENDIF FPC}TAtomVar<T_> = class
  public type
    PT_ = ^T_;
  private
    FValue__: T_;
    Critical: TCritical;
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
  TAtomTimeTick = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<TTimeTick>;
  // float
  TAtomSingle = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Single>;
  TAtomFloat = TAtomSingle;
  TAtomDouble = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Double>;
  TAtomExtended = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<Extended>;
  // string
  TAtomString = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<string>;

  TCritical = class
  private
    Instance__: TSystem_Critical;
    LNum: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Acquire; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
    procedure Release; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
    procedure Enter; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
    procedure Leave; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
    procedure Lock; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
    procedure UnLock; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
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
    procedure Inc_(var x: Integer; const v: Integer); overload;
    procedure Dec_(var x: Integer); overload;
    procedure Dec_(var x: Integer; const v: Integer); overload;
    procedure Inc_(var x: Cardinal); overload;
    procedure Inc_(var x: Cardinal; const v: Cardinal); overload;
    procedure Dec_(var x: Cardinal); overload;
    procedure Dec_(var x: Cardinal; const v: Cardinal); overload;
  end;
{$EndRegion 'Critical'}
{$Region 'OrderStruct'}
  {$IFDEF FPC}generic{$ENDIF FPC}TOrderStruct<T_> = class(TCore_Object)
  public type
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
    property OnFree: TOnFreeOrderStruct read FOnFreeOrderStruct write FOnFreeOrderStruct;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}TOrderPtrStruct<T_> = class(TCore_Object)
  public type
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
    property OnFree: TOnFreeOrderPtrStruct read FOnFreeOrderStruct write FOnFreeOrderStruct;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}TCriticalOrderStruct<T_> = class(TCore_Object)
  public type
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
    property Critical: TCritical read FCritical;
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
    property OnFree: TOnFreeCriticalOrderStruct read FOnFreeCriticalOrderStruct write FOnFreeCriticalOrderStruct;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}TCriticalOrderPtrStruct<T_> = class(TCore_Object)
  public type
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
    property Critical: TCritical read FCritical;
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
    property OnFree: TOnFreeCriticalOrderPtrStruct read FOnFreeCriticalOrderStruct write FOnFreeCriticalOrderStruct;
  end;
{$EndRegion 'OrderStruct'}
{$Region 'BigList'}
  {$IFDEF FPC}generic{$ENDIF FPC} TBigList<T_> = class(TCore_Object)
  public type
    PQueueStruct = ^TQueueStruct;
    PPQueueStruct = ^PQueueStruct;

    TQueueStruct = record
      Data: T_;
      Next: PQueueStruct;
      Prev: PQueueStruct;
{$IFDEF DEBUG}
      Instance_: TCore_Object;
{$ENDIF DEBUG}
    end;

    TArray_T_ = array of T_;
    TRecycle_Pool__ = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderStruct<PQueueStruct>;
    TQueueArrayStruct = array [0 .. (MaxInt div SizeOf(Pointer) - 1)] of PQueueStruct;
    PQueueArrayStruct = ^TQueueArrayStruct;
    TOnFreeQueueStruct = procedure(var p: T_) of object;
    TQueneStructProgress_C = procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean);
    TQueneStructProgress_M = procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean) of object;
{$IFDEF FPC}
    TQueneStructProgress_P = procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean) is nested;
{$ELSE FPC}
    TQueneStructProgress_P = reference to procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean);
{$ENDIF FPC}
  private
    FRecycle_Pool__: TRecycle_Pool__;
    FFirst: PQueueStruct;
    FLast: PQueueStruct;
    FNum: NativeInt;
    FOnFreeQueueStruct: TOnFreeQueueStruct;
    FChanged: Boolean;
    FList: Pointer;
    FProgress_Busy: NativeInt;
    procedure DoInternalFree(p: PQueueStruct);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoFree(var Data: T_); virtual;
    procedure Push_To_Recycle_Pool(p: PQueueStruct);
    procedure Free_Recycle_Pool;
    procedure Clear;
    property First: PQueueStruct read FFirst;
    property Last: PQueueStruct read FLast;
    procedure Next; // queue support
    function Add(Data: T_): PQueueStruct;
    function Insert(Data: T_; To_: PQueueStruct): PQueueStruct;
    procedure Remove(p: PQueueStruct);
    procedure Move_Before(p, To_: PQueueStruct);
    procedure MoveToFirst(p: PQueueStruct);
    procedure MoveToLast(p: PQueueStruct);
    procedure Exchange(p1, p2: PQueueStruct);
    function Found(p1: PQueueStruct): Boolean;
    property Progress_Busy: NativeInt read FProgress_Busy;
    procedure Progress_C(BP_, EP_:PQueueStruct; OnProgress: TQueneStructProgress_C); overload;
    procedure Progress_M(BP_, EP_:PQueueStruct; OnProgress: TQueneStructProgress_M); overload;
    procedure Progress_P(BP_, EP_:PQueueStruct; OnProgress: TQueneStructProgress_P); overload;
    procedure Progress_C(OnProgress: TQueneStructProgress_C); overload;
    procedure Progress_M(OnProgress: TQueneStructProgress_M); overload;
    procedure Progress_P(OnProgress: TQueneStructProgress_P); overload;
    function ToArray(): TArray_T_;
    function BuildArrayMemory: PQueueArrayStruct;
    function CheckList: PQueueArrayStruct;
    function GetList(const Index: NativeInt): PQueueStruct;
    procedure SetList(const Index: NativeInt; const Value: PQueueStruct);
    property List[const Index: NativeInt]: PQueueStruct read GetList write SetList;
    function GetItems(const Index: NativeInt): T_;
    procedure SetItems(const Index: NativeInt; const Value: T_);
    property Items[const Index: NativeInt]: T_ read GetItems write SetItems; default;
    property Num: NativeInt read FNum;
    property Count: NativeInt read FNum;
    property OnFree: TOnFreeQueueStruct read FOnFreeQueueStruct write FOnFreeQueueStruct;
    function Check: Boolean;
    class procedure Test;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC} TCriticalBigList<T_> = class(TCore_Object)
  public type
    PQueueStruct = ^TQueueStruct;
    PPQueueStruct = ^PQueueStruct;

    TQueueStruct = record
      Data: T_;
      Next: PQueueStruct;
      Prev: PQueueStruct;
{$IFDEF DEBUG}
      Instance_: TCore_Object;
{$ENDIF DEBUG}
    end;

    TArray_T_ = array of T_;
    TRecycle_Pool__ = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderStruct<PQueueStruct>;
    TQueueArrayStruct = array [0 .. (MaxInt div SizeOf(Pointer) - 1)] of PQueueStruct;
    PQueueArrayStruct = ^TQueueArrayStruct;
    TOnFreeQueueStruct = procedure(var p: T_) of object;
    TQueneStructProgress_C = procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean);
    TQueneStructProgress_M = procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean) of object;
{$IFDEF FPC}
    TQueneStructProgress_P = procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean) is nested;
{$ELSE FPC}
    TQueneStructProgress_P = reference to procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean);
{$ENDIF FPC}
  private
    FCritical: TCritical;
    FRecycle_Pool__: TRecycle_Pool__;
    FFirst: PQueueStruct;
    FLast: PQueueStruct;
    FNum: NativeInt;
    FOnFreeQueueStruct: TOnFreeQueueStruct;
    FChanged: Boolean;
    FList: Pointer;
    FProgress_Busy: NativeInt;
    procedure DoInternalFree(p: PQueueStruct);
  public
    property Critical: TCritical read FCritical;
    constructor Create;
    destructor Destroy; override;
    procedure DoFree(var Data: T_); virtual;
    procedure Push_To_Recycle_Pool(p: PQueueStruct);
    procedure Free_Recycle_Pool;
    procedure Clear;
    property First: PQueueStruct read FFirst;
    property Last: PQueueStruct read FLast;
    procedure Next; // queue support
    function Add(Data: T_): PQueueStruct;
    function Insert(Data: T_; To_: PQueueStruct): PQueueStruct;
    procedure Remove(p: PQueueStruct);
    procedure Move_Before(p, To_: PQueueStruct);
    procedure MoveToFirst(p: PQueueStruct);
    procedure MoveToLast(p: PQueueStruct);
    procedure Exchange(p1, p2: PQueueStruct);
    function Found(p1: PQueueStruct): Boolean;
    property Progress_Busy: NativeInt read FProgress_Busy;
    procedure Progress_C(BP_, EP_:PQueueStruct; OnProgress: TQueneStructProgress_C); overload;
    procedure Progress_M(BP_, EP_:PQueueStruct; OnProgress: TQueneStructProgress_M); overload;
    procedure Progress_P(BP_, EP_:PQueueStruct; OnProgress: TQueneStructProgress_P); overload;
    procedure Progress_C(OnProgress: TQueneStructProgress_C); overload;
    procedure Progress_M(OnProgress: TQueneStructProgress_M); overload;
    procedure Progress_P(OnProgress: TQueneStructProgress_P); overload;
    function ToArray(): TArray_T_;
    function BuildArrayMemory: PQueueArrayStruct;
    function CheckList: PQueueArrayStruct;
    function GetList(const Index: NativeInt): PQueueStruct;
    procedure SetList(const Index: NativeInt; const Value: PQueueStruct);
    property List[const Index: NativeInt]: PQueueStruct read GetList write SetList;
    function GetItems(const Index: NativeInt): T_;
    procedure SetItems(const Index: NativeInt; const Value: T_);
    property Items[const Index: NativeInt]: T_ read GetItems write SetItems; default;
    property Num: NativeInt read FNum;
    property Count: NativeInt read FNum;
    property OnFree: TOnFreeQueueStruct read FOnFreeQueueStruct write FOnFreeQueueStruct;
    function Check: Boolean;
    class procedure Test;
  end;
{$EndRegion 'BigList'}
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

  TThreadPost = class(TCore_Object)
  private type
    TThread_Post_Data = record
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
      IsRuning, IsExit: PBoolean;
      procedure Init;
    end;

    TThread_Post_Data_Order_Struct__ = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderPtrStruct<TThread_Post_Data>;
  protected
    FCritical: TCritical;
    FThreadID: TThreadID;
    FSyncPool: TThread_Post_Data_Order_Struct__;
    FProgressing: TAtomBool;
    FOneStep: Boolean;
    FResetRandomSeed: Boolean;
    procedure FreeThreadProgressPostData(p: TThread_Post_Data_Order_Struct__.PT_);
  public
    constructor Create(ThreadID_: TThreadID);
    destructor Destroy; override;
    property ThreadID: TThreadID read FThreadID write FThreadID;
    property OneStep: Boolean read FOneStep write FOneStep;
    property ResetRandomSeed: Boolean read FResetRandomSeed write FResetRandomSeed;
    property SyncPool: TThread_Post_Data_Order_Struct__ read FSyncPool;
    function Count: NativeInt;
    property Num: NativeInt read Count;
    function Busy: Boolean;

    function Progress(ThreadID_: TThreadID): NativeInt; overload;
    function Progress(Thread_: TThread): NativeInt; overload;
    function Progress(): Integer; overload;

    // post thread synchronization,Call
    procedure PostC1(OnSync: TThreadPost_C1); overload;
    procedure PostC1(OnSync: TThreadPost_C1; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostC2(Data1: Pointer; OnSync: TThreadPost_C2); overload;
    procedure PostC2(Data1: Pointer; OnSync: TThreadPost_C2; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3); overload;
    procedure PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4); overload;
    procedure PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4; IsRuning_, IsExit_: PBoolean); overload;
    // post thread synchronization,Method
    procedure PostM1(OnSync: TThreadPost_M1); overload;
    procedure PostM1(OnSync: TThreadPost_M1; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostM2(Data1: Pointer; OnSync: TThreadPost_M2); overload;
    procedure PostM2(Data1: Pointer; OnSync: TThreadPost_M2; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3); overload;
    procedure PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4); overload;
    procedure PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4; IsRuning_, IsExit_: PBoolean); overload;
    // post thread synchronization,Proc
    procedure PostP1(OnSync: TThreadPost_P1); overload;
    procedure PostP1(OnSync: TThreadPost_P1; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostP2(Data1: Pointer; OnSync: TThreadPost_P2); overload;
    procedure PostP2(Data1: Pointer; OnSync: TThreadPost_P2; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3); overload;
    procedure PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4); overload;
    procedure PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4; IsRuning_, IsExit_: PBoolean); overload;
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
  TCoreCompute_Thread_Pool = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<TCompute>;

  TCompute = class(TCore_Thread)
  private
    Thread_Pool_Queue_Data_Ptr: TCoreCompute_Thread_Pool.PQueueStruct;
    OnRun_C: TRunWithThread_C;
    OnRun_M: TRunWithThread_M;
    OnRun_P: TRunWithThread_P;
    OnRun_C_NP: TRunWithThread_C_NP;
    OnRun_M_NP: TRunWithThread_M_NP;
    OnRun_P_NP: TRunWithThread_P_NP;
    OnDone_C: TRunWithThread_C;
    OnDone_M: TRunWithThread_M;
    OnDone_P: TRunWithThread_P;
    IsRuning, IsExit: PBoolean;
    FRndInstance: Pointer;
  protected
    procedure Execute; override;
    procedure Done_Sync;
  public
    UserData: Pointer;
    UserObject: TCore_Object;

    constructor Create;
    destructor Destroy; override;
    class function IDLE_Thread(): NativeInt;
    class function ActivtedTask(): NativeInt;
    class function WaitTask(): NativeInt;
    class function TotalTask(): NativeInt;
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
    class procedure RunC(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_C; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunC(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_C); overload;
    class procedure RunC(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_C; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunC(const OnRun: TRunWithThread_C); overload;
    class procedure RunC(const OnRun: TRunWithThread_C; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunC_NP(const OnRun: TRunWithThread_C_NP); overload;
    class procedure RunC_NP(const OnRun: TRunWithThread_C_NP; IsRuning_, IsExit_: PBoolean); overload;
    // build-in asynchronous methoc
    class procedure RunM(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_M); overload;
    class procedure RunM(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_M; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunM(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_M); overload;
    class procedure RunM(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_M; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunM(const OnRun: TRunWithThread_M); overload;
    class procedure RunM(const OnRun: TRunWithThread_M; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunM_NP(const OnRun: TRunWithThread_M_NP); overload;
    class procedure RunM_NP(const OnRun: TRunWithThread_M_NP; IsRuning_, IsExit_: PBoolean); overload;
    // build-in asynchronous proc
    class procedure RunP(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_P); overload;
    class procedure RunP(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_P; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunP(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_P); overload;
    class procedure RunP(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_P; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunP(const OnRun: TRunWithThread_P); overload;
    class procedure RunP(const OnRun: TRunWithThread_P; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunP_NP(const OnRun: TRunWithThread_P_NP); overload;
    class procedure RunP_NP(const OnRun: TRunWithThread_P_NP; IsRuning_, IsExit_: PBoolean); overload;

    // main thread
    class procedure ProgressPost();
    // post main thread synchronization,Call
    class procedure PostC1(OnSync: TThreadPost_C1); overload;
    class procedure PostC1(OnSync: TThreadPost_C1; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostC2(Data1: Pointer; OnSync: TThreadPost_C2); overload;
    class procedure PostC2(Data1: Pointer; OnSync: TThreadPost_C2; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3); overload;
    class procedure PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4); overload;
    class procedure PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4; IsRuning_, IsExit_: PBoolean); overload;
    // post main thread synchronization,Method
    class procedure PostM1(OnSync: TThreadPost_M1); overload;
    class procedure PostM1(OnSync: TThreadPost_M1; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostM2(Data1: Pointer; OnSync: TThreadPost_M2); overload;
    class procedure PostM2(Data1: Pointer; OnSync: TThreadPost_M2; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3); overload;
    class procedure PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4); overload;
    class procedure PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4; IsRuning_, IsExit_: PBoolean); overload;
    // post main thread synchronization,Proc
    class procedure PostP1(OnSync: TThreadPost_P1); overload;
    class procedure PostP1(OnSync: TThreadPost_P1; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostP2(Data1: Pointer; OnSync: TThreadPost_P2); overload;
    class procedure PostP2(Data1: Pointer; OnSync: TThreadPost_P2; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3); overload;
    class procedure PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4); overload;
    class procedure PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4; IsRuning_, IsExit_: PBoolean); overload;
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

  TMT19937 = class
  public
    class function CoreToDelphi: Boolean; static;
    class function InstanceNum(): Integer; static;
    class procedure SetSeed(seed: Integer); static;
    class function GetSeed(): Integer; static;
    class procedure Randomize(); static;
    class function Rand32(L: Integer): Integer; overload; static;
    class procedure Rand32(L: Integer; dest: PInteger; num: NativeInt); overload; static;
    class function Rand64(L: Int64): Int64; overload; static;
    class procedure Rand64(L: Int64; dest: PInt64; num: NativeInt); overload; static;
    class function RandE: Extended; overload; static;
    class procedure RandE(dest: PExtended; num: NativeInt); overload; static;
    class function RandF: Single; overload; static;
    class procedure RandF(dest: PSingle; num: NativeInt); overload; static;
    class function RandD: Double; overload; static;
    class procedure RandD(dest: PDouble; num: NativeInt); overload; static;
    class procedure SaveToStream(stream: TCore_Stream); static;
    class procedure LoadFromStream(stream: TCore_Stream); static;
  end;
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

  CPU64 = {$IFDEF CPU64}True{$ELSE CPU64}False{$ENDIF CPU64};
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

function Get_System_Critical_Recycle_Pool_Num: NativeInt;
function Get_MT19937_POOL_Num: NativeInt;
function Get_Object_Lock_Pool_Num: NativeInt;
function Get_Parallel_Granularity: Integer;
procedure Set_Parallel_Granularity(Thread_Num: Integer);
procedure Set_IDLE_Compute_Wait_Time_Tick(Tick_: TTimeTick);

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

procedure DisposeObject(const Obj: TObject);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure DisposeObject(const objs: array of TObject); overload;
procedure FreeObj(const Obj: TObject);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
procedure FreeObject(const Obj: TObject);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure FreeObject(const objs: array of TObject); overload;
procedure DisposeObjectAndNil(var Obj);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
procedure FreeObjAndNil(var Obj);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}

procedure LockObject(Obj: TObject);
procedure UnLockObject(Obj: TObject);

function DeltaStep(const value_, Delta_: NativeInt): NativeInt;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
procedure AtomInc(var x: Int64);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomInc(var x: Int64; const v: Int64);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: Int64);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: Int64; const v: Int64);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomInc(var x: UInt64);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomInc(var x: UInt64; const v: UInt64);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: UInt64);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: UInt64; const v: UInt64);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomInc(var x: Integer);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomInc(var x: Integer; const v:Integer);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: Integer);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: Integer; const v:Integer);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomInc(var x: Cardinal);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomInc(var x: Cardinal; const v:Cardinal);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: Cardinal);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: Cardinal; const v:Cardinal);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

procedure FillPtrByte(const dest:Pointer; Size: NativeUInt; const Value: Byte); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
procedure FillPtr(const dest:Pointer; Size: NativeUInt; const Value: Byte); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
procedure FillByte(const dest:Pointer; Size: NativeUInt; const Value: Byte); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function CompareMemory(const p1, p2: Pointer; Size: NativeUInt): Boolean; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
procedure CopyPtr(const sour, dest: Pointer; Size: NativeUInt); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}

procedure RaiseInfo(const n: string); overload;
procedure RaiseInfo(const n: string; const Args: array of const); overload;

function IsMobile: Boolean;

function GetTimeTick(): TTimeTick;
function GetTimeTickCount(): TTimeTick;
function GetCrashTimeTick(): TTimeTick;
function SameF(const A, B: Double; Epsilon: Double = 0): Boolean;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function SameF(const A, B: Single; Epsilon: Single = 0): Boolean;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

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

function ROL8(const Value: Byte; Shift: Byte): Byte;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function ROL16(const Value: Word; Shift: Byte): Word;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function ROL32(const Value: Cardinal; Shift: Byte): Cardinal;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function ROL64(const Value: UInt64; Shift: Byte): UInt64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function ROR8(const Value: Byte; Shift: Byte): Byte;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function ROR16(const Value: Word; Shift: Byte): Word;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function ROR32(const Value: Cardinal; Shift: Byte): Cardinal;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function ROR64(const Value: UInt64; Shift: Byte): UInt64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}

function Endian(const Value: SmallInt): SmallInt;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function Endian(const Value: Word): Word;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function Endian(const Value: Integer): Integer;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function Endian(const Value: Cardinal): Cardinal;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function Endian(const Value: Int64): Int64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function Endian(const Value: UInt64): UInt64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

function BE2N(const Value: SmallInt): SmallInt;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function BE2N(const Value: Word): Word;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function BE2N(const Value: Integer): Integer;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function BE2N(const Value: Cardinal): Cardinal;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function BE2N(const Value: Int64): Int64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function BE2N(const Value: UInt64): UInt64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

function LE2N(const Value: SmallInt): SmallInt;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function LE2N(const Value: Word): Word;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function LE2N(const Value: Integer): Integer;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function LE2N(const Value: Cardinal): Cardinal;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function LE2N(const Value: Int64): Int64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function LE2N(const Value: UInt64): UInt64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

function N2BE(const Value: SmallInt): SmallInt;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2BE(const Value: Word): Word;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2BE(const Value: Integer): Integer;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2BE(const Value: Cardinal): Cardinal;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2BE(const Value: Int64): Int64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2BE(const Value: UInt64): UInt64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

function N2LE(const Value: SmallInt): SmallInt;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2LE(const Value: Word): Word;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2LE(const Value: Integer): Integer;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2LE(const Value: Cardinal): Cardinal;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2LE(const Value: Int64): Int64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2LE(const Value: UInt64): UInt64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

procedure Swap(var v1, v2: Byte);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: Word);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: Integer);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: Cardinal);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: Int64);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: UInt64);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
{$IFDEF OVERLOAD_NATIVEINT}
procedure Swap(var v1, v2: NativeInt);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: NativeUInt);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
{$ENDIF OVERLOAD_NATIVEINT}
procedure Swap(var v1, v2: string);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: Single);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: Double);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: Pointer);{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure SwapVariant(var v1, v2: Variant);
function Swap(const v: Word): Word;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function Swap(const v: Cardinal): Cardinal;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function Swap(const v: UInt64): UInt64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

function SAR16(const Value: SmallInt; const Shift: Byte): SmallInt;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function SAR32(const Value: Integer; Shift: Byte): Integer;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function SAR64(const Value: Int64; Shift: Byte): Int64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}

function MemoryAlign(addr: Pointer; alignment_: NativeUInt): Pointer;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}

function if_(const bool_: Boolean; const True_, False_: Boolean): Boolean;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: ShortInt): ShortInt;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: SmallInt): SmallInt;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: Integer): Integer;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: Int64): Int64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: Byte): Byte;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: Word): Word;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: Cardinal): Cardinal;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: UInt64): UInt64;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: Single): Single;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: Double): Double;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: string): string;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function ifv_(const bool_: Boolean; const True_, False_: Variant): Variant;{$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function GetOffset(p_: Pointer; offset_: NativeInt): Pointer; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function GetPtr(p_: Pointer; offset_: NativeInt): Pointer; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}

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

{$I Z.Core.Cirtical.inc}
{$I Z.Core.Atomic.inc}
{$I Z.Core.MT19937.inc}

procedure DisposeObject(const Obj: TObject);
begin
  if Obj = nil then
    exit;
  try
    {$IFDEF AUTOREFCOUNT}
    Obj.DisposeOf;
    {$ELSE AUTOREFCOUNT}
    Obj.Free;
    {$ENDIF AUTOREFCOUNT}
  except
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
  Lock_Critical_Obj__(Obj);
{$ELSE FPC}
{$IFDEF CriticalSimulateAtomic}
  Lock_Critical_Obj__(Obj);
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
  UnLock_Critical_Obj__(Obj);
{$ELSE FPC}
  {$IFDEF CriticalSimulateAtomic}
  UnLock_Critical_Obj__(Obj);
  {$ELSE CriticalSimulateAtomic}
  TMonitor.Exit(Obj);
  {$ENDIF CriticalSimulateAtomic}
{$ENDIF FPC}
end;

procedure FillPtrByte(const dest: Pointer; Size: NativeUInt; const Value: Byte);
{$IFDEF FillPtr_Used_FillChar}
begin
  FillChar(dest^, Size, Value);
end;
{$ELSE FillPtr_Used_FillChar}
var
  d: PByte;
  v: UInt64;
begin
  if Size = 0 then
      Exit;
  v := Value or (Value shl 8) or (Value shl 16) or (Value shl 24);
  v := v or (v shl 32);
  d := dest;
  while Size >= 8 do
    begin
      PUInt64(d)^ := v;
      dec(Size, 8);
      inc(d, 8);
    end;
  if Size >= 4 then
    begin
      PCardinal(d)^ := PCardinal(@v)^;
      dec(Size, 4);
      inc(d, 4);
    end;
  if Size >= 2 then
    begin
      PWORD(d)^ := PWORD(@v)^;
      dec(Size, 2);
      inc(d, 2);
    end;
  if Size > 0 then
      d^ := Value;
end;
{$ENDIF FillPtr_Used_FillChar}

procedure FillPtr(const dest:Pointer; Size: NativeUInt; const Value: Byte);
begin
  FillPtrByte(dest, Size, Value);
end;

procedure FillByte(const dest:Pointer; Size: NativeUInt; const Value: Byte);
begin
  FillPtrByte(dest, Size, Value);
end;

function CompareMemory(const p1, p2: Pointer; Size: NativeUInt): Boolean;
var
  b1, b2: PByte;
begin;
  if Size = 0 then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
  b1 := p1;
  b2 := p2;
  while (Size >= 8) do
    begin
      if PUInt64(b2)^ <> PUInt64(b1)^ then
          Exit;
      dec(Size, 8);
      inc(b2, 8);
      inc(b1, 8);
    end;
  if Size >= 4 then
    begin
      if PCardinal(b2)^ <> PCardinal(b1)^ then
          Exit;
      dec(Size, 4);
      inc(b2, 4);
      inc(b1, 4);
    end;
  if Size >= 2 then
    begin
      if PWORD(b2)^ <> PWORD(b1)^ then
          Exit;
      dec(Size, 2);
      inc(b2, 2);
      inc(b1, 2);
    end;
  if Size > 0 then
    if b2^ <> b1^ then
        Exit;
  Result := True;
end;

procedure CopyPtr(const sour, dest: Pointer; Size: NativeUInt);
{$IFDEF CopyPtr_Used_Move}
begin
  Move(sour^, dest^, Size);
end;
{$ELSE CopyPtr_Used_Move}
var
  s, d: NativeUInt;
begin
  if Size = 0 then
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
      inc(s, Size);
      inc(d, Size);
      while Size >= 8 do
        begin
          dec(d, 8);
          dec(s, 8);
          dec(Size, 8);
          PUInt64(d)^ := PUInt64(s)^;
        end;
      if Size >= 4 then
        begin
          dec(d, 4);
          dec(s, 4);
          dec(Size, 4);
          PCardinal(d)^ := PCardinal(s)^;
        end;
      if Size >= 2 then
        begin
          dec(d, 2);
          dec(s, 2);
          dec(Size, 2);
          PWORD(d)^ := PWORD(s)^;
        end;
      if Size > 0 then
          PByte(d - 1)^ := PByte(s - 1)^;
    end
  else
    begin
      while Size >= 8 do
        begin
          PUInt64(d)^ := PUInt64(s)^;
          dec(Size, 8);
          inc(d, 8);
          inc(s, 8);
        end;
      if Size >= 4 then
        begin
          PCardinal(d)^ := PCardinal(s)^;
          dec(Size, 4);
          inc(d, 4);
          inc(s, 4);
        end;
      if Size >= 2 then
        begin
          PWORD(d)^ := PWORD(s)^;
          dec(Size, 2);
          inc(d, 2);
          inc(s, 2);
        end;
      if Size > 0 then
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
  Sys_Tick: Cardinal;
begin
  TimeTick_Critical__.Acquire;
  Sys_Tick := TCore_Thread.GetTickCount();
  inc(Core_RunTime_Tick, Sys_Tick - Core_Step_Tick);
  Core_Step_Tick := Sys_Tick;
  Result := Core_RunTime_Tick;
  TimeTick_Critical__.Release;
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
{$I Z.Core.BigList.inc}

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
  Init_System_Critical_Recycle_Pool();
  OnCheckThreadSynchronize := nil;
  WorkInParallelCore := TAtomBool.Create(True);
  ParallelCore := WorkInParallelCore;
  GlobalMemoryHook := TAtomBool.Create(True);
  Core_RunTime_Tick := C_Tick_Day * 3;
  Core_Step_Tick := TCore_Thread.GetTickCount();
  Init_Critical_System();
  InitMT19937Rand();
  CoreInitedTimeTick := GetTimeTick();
  InitCoreThreadPool(CpuCount * 2);
  MainThreadProgress := TThreadPost.Create(MainThreadID);
  MainThSynchronizeRunning := False;
  MainThreadPost := MainThreadProgress;
  SysProgress := MainThreadProgress;
finalization
  FreeCoreThreadPool;
  MainThreadProgress.Free;
  FreeMT19937Rand();
  Free_Critical_System;
  WorkInParallelCore.Free;
  WorkInParallelCore := nil;
  GlobalMemoryHook.Free;
  GlobalMemoryHook := nil;
  Free_System_Critical_Recycle_Pool();
end.
