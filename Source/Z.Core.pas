{ ****************************************************************************** }
{ * Core library                                                               * }
{ ****************************************************************************** }
unit Z.Core;

{$I Z.Define.inc}

interface

uses SysUtils, Classes, Types, Variants, SyncObjs,
  {$IFDEF FPC}
  Z.FPC.GenericList, fgl,
  {$ELSE FPC}
  System.Generics.Collections,
  {$ENDIF FPC}
  Math;

{$Region 'core defines + class'}
type
  THash = Cardinal;
  THash64 = UInt64;
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
    PGArry = ^TGArry;
  public var Arry: TGArry;
    function ListData: PGArry;
  end;

  TGenericsObjectList<t: class> = class(System.Generics.Collections.TList<t>)
  private type
    TGArry = array of t;
    PGArry = ^TGArry;
  public var Arry: TGArry;
    function ListData: PGArry;
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

  {$IFDEF FPC}generic{$ENDIF FPC}
  TAtomVar<T_> = class
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
  {$IFDEF FPC}generic{$ENDIF FPC}
  TOrderStruct<T_> = class(TCore_Object)
  public type
    POrderStruct = ^TOrderStruct_;
    TOrderStruct_ = record
      Data: T_;
      Next: POrderStruct;
    end;
    TOnFreeOrderStruct = procedure(var p: T_) of object;
  private
    FFirst: POrderStruct;
    FLast: POrderStruct;
    FNum: NativeInt;
    FOnFreeOrderStruct: TOnFreeOrderStruct;
    procedure DoInternalFree(const p: POrderStruct);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DoFree(var Data: T_); virtual;
    procedure Clear;
    property Current: POrderStruct read FFirst;
    property First: POrderStruct read FFirst;
    property Last: POrderStruct read FLast;
    procedure Next;
    function Push(const Data: T_): POrderStruct;
    function Push_Null: POrderStruct;
    property Num: NativeInt read FNum;
    property OnFree: TOnFreeOrderStruct read FOnFreeOrderStruct write FOnFreeOrderStruct;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}
  TOrderPtrStruct<T_> = class(TCore_Object)
  public type
    PT_ = ^T_;
    POrderPtrStruct = ^TOrderPtrStruct_;
    TOrderPtrStruct_ = record
      Data: PT_;
      Next: POrderPtrStruct;
    end;
    TOnFreeOrderPtrStruct = procedure(p: PT_) of object;
  private
    FFirst: POrderPtrStruct;
    FLast: POrderPtrStruct;
    FNum: NativeInt;
    FOnFreeOrderStruct: TOnFreeOrderPtrStruct;
    procedure DoInternalFree(p: POrderPtrStruct);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DoFree(Data: PT_); virtual;
    procedure Clear;
    property Current: POrderPtrStruct read FFirst;
    property First: POrderPtrStruct read FFirst;
    property Last: POrderPtrStruct read FLast;
    procedure Next;
    function Push(const Data: T_): POrderPtrStruct;
    function PushPtr(Data: PT_): POrderPtrStruct;
    property Num: NativeInt read FNum;
    property OnFree: TOnFreeOrderPtrStruct read FOnFreeOrderStruct write FOnFreeOrderStruct;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}
  TCriticalOrderStruct<T_> = class(TCore_Object)
  public type
    POrderStruct = ^TOrderStruct_;
    TOrderStruct_ = record
      Data: T_;
      Next: POrderStruct;
    end;
    TOnFreeCriticalOrderStruct = procedure(var p: T_) of object;
  private
    FCritical__: TCritical;
    FFirst: POrderStruct;
    FLast: POrderStruct;
    FNum: NativeInt;
    FOnFreeCriticalOrderStruct: TOnFreeCriticalOrderStruct;
    procedure DoInternalFree(const p: POrderStruct);
  public
    property Critical__: TCritical read FCritical__;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DoFree(var Data: T_); virtual;
    procedure Clear;
    function GetCurrent: POrderStruct;
    property Current: POrderStruct read GetCurrent;
    property First: POrderStruct read GetCurrent;
    procedure Next;
    function Push(const Data: T_): POrderStruct;
    function Push_Null: POrderStruct;
    function GetNum: NativeInt;
    property Num: NativeInt read GetNum;
    property OnFree: TOnFreeCriticalOrderStruct read FOnFreeCriticalOrderStruct write FOnFreeCriticalOrderStruct;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}
  TCriticalOrderPtrStruct<T_> = class(TCore_Object)
  public type
    PT_ = ^T_;
    POrderPtrStruct = ^TOrderPtrStruct_;
    TOrderPtrStruct_ = record
      Data: PT_;
      Next: POrderPtrStruct;
    end;
    TOnFreeCriticalOrderPtrStruct = procedure(p: PT_) of object;
  private
    FCritical__: TCritical;
    FFirst: POrderPtrStruct;
    FLast: POrderPtrStruct;
    FNum: NativeInt;
    FOnFreeCriticalOrderStruct: TOnFreeCriticalOrderPtrStruct;
    procedure DoInternalFree(p: POrderPtrStruct);
  public
    property Critical__: TCritical read FCritical__;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DoFree(Data: PT_); virtual;
    procedure Clear;
    function GetCurrent: POrderPtrStruct;
    property Current: POrderPtrStruct read GetCurrent;
    property First: POrderPtrStruct read GetCurrent;
    procedure Next;
    function Push(const Data: T_): POrderPtrStruct;
    function PushPtr(Data: PT_): POrderPtrStruct;
    function GetNum: NativeInt;
    property Num: NativeInt read GetNum;
    property OnFree: TOnFreeCriticalOrderPtrStruct read FOnFreeCriticalOrderStruct write FOnFreeCriticalOrderStruct;
  end;
{$EndRegion 'OrderStruct'}
{$REGION 'BigList'}
  {$IFDEF FPC}generic{$ENDIF FPC}
  TBigList<T_> = class(TCore_Object)
  public type

    PQueueStruct = ^TQueueStruct;
    PPQueueStruct = ^PQueueStruct;
    T___ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<T_>;

    TQueueStruct = record
      Data: T_;
      Next: PQueueStruct;
      Prev: PQueueStruct;
      Instance___: T___;
    end;

    TRepeat___ = record
    private
      // Begin Index
      BI___: NativeInt;
      // End Index
      EI___: NativeInt;
      // Current index
      I___: NativeInt;
      Instance___: T___;
      p___: PQueueStruct;
      Is_Discard___: Boolean;
      procedure Init_(Instance_: T___); overload;
      procedure Init_(Instance_: T___; BI_, EI_: NativeInt); overload;
    public
      property Work: T___ read Instance___;
      property BI: NativeInt read BI___;
      property EI: NativeInt read EI___;
      property I__: NativeInt read I___;
      property Queue: PQueueStruct read p___;
      procedure Discard;
      function Next: Boolean;
      property Right: Boolean read Next;
    end;

    TInvert_Repeat___ = record
    private
      // Begin Index
      BI___: NativeInt;
      // End Index
      EI___: NativeInt;
      // Current index
      I___: NativeInt;
      Instance___: T___;
      p___: PQueueStruct;
      Is_Discard___: Boolean;
      procedure Init_(Instance_: T___); overload;
      procedure Init_(Instance_: T___; BI_, EI_: NativeInt); overload;
    public
      property Work: T___ read Instance___;
      property BI: NativeInt read BI___;
      property EI: NativeInt read EI___;
      property I__: NativeInt read I___;
      property Queue: PQueueStruct read p___;
      procedure Discard;
      function Prev: Boolean;
      property Left: Boolean read Prev;
    end;

    TArray_T_ = array of T_;
    TOrder_Data_Pool = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderStruct<T_>;
    TRecycle_Pool__ = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderStruct<PQueueStruct>;
    TQueueArrayStruct = array [0 .. (MaxInt div SizeOf(Pointer) - 1)] of PQueueStruct;
    PQueueArrayStruct = ^TQueueArrayStruct;
    TOnStruct_Event = procedure(var p: T_) of object;
    TSort_C = function(var Left, Right: T_): ShortInt;
    TQueneStructFor_C = procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean);
    TSort_M = function(var Left, Right: T_): ShortInt of object;
    TQueneStructFor_M = procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean) of object;
{$IFDEF FPC}
    TQueneStructFor_P = procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean) is nested;
    TSort_P = function(var Left, Right: T_): ShortInt is nested;
{$ELSE FPC}
    TQueneStructFor_P = reference to procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean);
    TSort_P = reference to function(var Left, Right: T_): ShortInt;
{$ENDIF FPC}
  private
    FRecycle_Pool__: TRecycle_Pool__;
    FFirst: PQueueStruct;
    FLast: PQueueStruct;
    FNum: NativeInt;
    FOnAdd: TOnStruct_Event;
    FOnFree: TOnStruct_Event;
    FChanged: Boolean;
    FList: Pointer;
    procedure DoInternalFree(p: PQueueStruct);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoFree(var Data: T_); virtual;
    procedure DoAdd(var Data: T_); virtual;
    function CompareData(const Data_1, Data_2: T_): Boolean; virtual;
    property Recycle_Pool: TRecycle_Pool__ read FRecycle_Pool__;
    procedure Push_To_Recycle_Pool(p: PQueueStruct);
    procedure Free_Recycle_Pool;
    procedure Clear;
    property First: PQueueStruct read FFirst;
    property Last: PQueueStruct read FLast;
    procedure Next; // queue support
    function Add(const Data: T_): PQueueStruct;
    procedure AddL(L_: T___);
    function Add_Null(): PQueueStruct;
    function Insert(const Data: T_; To_: PQueueStruct): PQueueStruct;
    procedure Remove_P(p: PQueueStruct);
    procedure Remove_T(const Data: T_);
    procedure Move_Before(p, To_: PQueueStruct);
    procedure MoveToFirst(p: PQueueStruct);
    procedure MoveToLast(p: PQueueStruct);
    procedure Exchange(p1, p2: PQueueStruct);
    function Found(p1: PQueueStruct): NativeInt;
    function Find_Data(const Data: T_): PQueueStruct;
    function Search_Data_As_Array(const Data: T_): TArray_T_;
    function Search_Data_As_Order(const Data: T_): TOrder_Data_Pool;
    function Remove_Data(const Data: T_): Integer;
    function Repeat_(): TRepeat___; overload;
    function Repeat_(BI_, EI_: NativeInt): TRepeat___; overload;
    function Invert_Repeat_(): TInvert_Repeat___; overload;
    function Invert_Repeat_(BI_, EI_: NativeInt): TInvert_Repeat___; overload;
    procedure For_C(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_C); overload;
    procedure For_M(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_M); overload;
    procedure For_P(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_P); overload;
    procedure For_C(OnFor: TQueneStructFor_C); overload;
    procedure For_M(OnFor: TQueneStructFor_M); overload;
    procedure For_P(OnFor: TQueneStructFor_P); overload;
    function ToArray(): TArray_T_;
    function ToOrder(): TOrder_Data_Pool;
    class procedure Swap_(var Left, Right: T_);
    procedure Sort_C(Arry_: PQueueArrayStruct; L, R: NativeInt; OnSort: TSort_C); overload;
    procedure Sort_C(OnSort: TSort_C); overload;
    procedure Sort_M(Arry_: PQueueArrayStruct; L, R: NativeInt; OnSort: TSort_M); overload;
    procedure Sort_M(OnSort: TSort_M); overload;
    procedure Sort_P(Arry_: PQueueArrayStruct; L, R: NativeInt; OnSort: TSort_P); overload;
    procedure Sort_P(OnSort: TSort_P); overload;
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
    property OnFree: TOnStruct_Event read FOnFree write FOnFree;
    property OnAdd: TOnStruct_Event read FOnAdd write FOnAdd;
{$IFDEF DEBUG}
    function Test_Check__: Boolean;
    class procedure Test;
{$ENDIF DEBUG}
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}
  TCriticalBigList<T_> = class(TCore_Object)
  public type

    PQueueStruct = ^TQueueStruct;
    PPQueueStruct = ^PQueueStruct;
    T___ = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalBigList<T_>;

    TQueueStruct = record
      Data: T_;
      Next: PQueueStruct;
      Prev: PQueueStruct;
      Instance___: T___;
    end;

    TRepeat___ = record
    private
      // Begin Index
      BI___: NativeInt;
      // End Index
      EI___: NativeInt;
      // Current index
      I___: NativeInt;
      Instance___: T___;
      p___: PQueueStruct;
      Is_Discard___: Boolean;
      procedure Init_(Instance_: T___); overload;
      procedure Init_(Instance_: T___; BI_, EI_: NativeInt); overload;
    public
      property Work: T___ read Instance___;
      property BI: NativeInt read BI___;
      property EI: NativeInt read EI___;
      property I__: NativeInt read I___;
      property Queue: PQueueStruct read p___;
      procedure Discard;
      function Next: Boolean;
      property Right: Boolean read Next;
    end;

    TInvert_Repeat___ = record
    private
      // Begin Index
      BI___: NativeInt;
      // End Index
      EI___: NativeInt;
      // Current index
      I___: NativeInt;
      Instance___: T___;
      p___: PQueueStruct;
      Is_Discard___: Boolean;
      procedure Init_(Instance_: T___); overload;
      procedure Init_(Instance_: T___; BI_, EI_: NativeInt); overload;
    public
      property Work: T___ read Instance___;
      property BI: NativeInt read BI___;
      property EI: NativeInt read EI___;
      property I__: NativeInt read I___;
      property Queue: PQueueStruct read p___;
      procedure Discard;
      function Prev: Boolean;
      property Left: Boolean read Prev;
    end;

    TArray_T_ = array of T_;
    TOrder_Data_Pool = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderStruct<T_>;
    TRecycle_Pool__ = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderStruct<PQueueStruct>;
    TQueueArrayStruct = array [0 .. (MaxInt div SizeOf(Pointer) - 1)] of PQueueStruct;
    PQueueArrayStruct = ^TQueueArrayStruct;
    TOnStruct_Event = procedure(var p: T_) of object;
    TSort_C = function(var Left, Right: T_): ShortInt;
    TQueneStructFor_C = procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean);
    TSort_M = function(var Left, Right: T_): ShortInt of object;
    TQueneStructFor_M = procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean) of object;
{$IFDEF FPC}
    TQueneStructFor_P = procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean) is nested;
    TSort_P = function(var Left, Right: T_): ShortInt is nested;
{$ELSE FPC}
    TQueneStructFor_P = reference to procedure(Index_: NativeInt; p: PQueueStruct; var Aborted: Boolean);
    TSort_P = reference to function(var Left, Right: T_): ShortInt;
{$ENDIF FPC}
  private
    FCritical__: TCritical;
    FRecycle_Pool__: TRecycle_Pool__;
    FFirst: PQueueStruct;
    FLast: PQueueStruct;
    FNum: NativeInt;
    FOnAdd: TOnStruct_Event;
    FOnFree: TOnStruct_Event;
    FChanged: Boolean;
    FList: Pointer;
    procedure DoInternalFree(p: PQueueStruct);
  public
    property Critical__: TCritical read FCritical__;
    constructor Create;
    destructor Destroy; override;
    procedure DoFree(var Data: T_); virtual;
    procedure DoAdd(var Data: T_); virtual;
    function CompareData(const Data_1, Data_2: T_): Boolean; virtual;
    procedure Lock;
    procedure UnLock;
    property Recycle_Pool: TRecycle_Pool__ read FRecycle_Pool__;
    procedure Push_To_Recycle_Pool(p: PQueueStruct);
    procedure Free_Recycle_Pool;
    procedure Clear;
    property First: PQueueStruct read FFirst;
    property Last: PQueueStruct read FLast;
    procedure Next; // queue support
    function Add(const Data: T_): PQueueStruct;
    procedure AddL(L_: T___);
    function Add_Null(): PQueueStruct;
    function Insert(const Data: T_; To_: PQueueStruct): PQueueStruct;
    procedure Remove_P(p: PQueueStruct);
    procedure Remove_T(const Data: T_);
    procedure Move_Before(p, To_: PQueueStruct);
    procedure MoveToFirst(p: PQueueStruct);
    procedure MoveToLast(p: PQueueStruct);
    procedure Exchange(p1, p2: PQueueStruct);
    function Found(p1: PQueueStruct): NativeInt;
    function Find_Data(const Data: T_): PQueueStruct;
    function Search_Data_As_Array(const Data: T_): TArray_T_;
    function Search_Data_As_Order(const Data: T_): TOrder_Data_Pool;
    function Remove_Data(const Data: T_): Integer;
    function Repeat_(): TRepeat___; overload;
    function Repeat_(BI_, EI_: NativeInt): TRepeat___; overload;
    function Invert_Repeat_(): TInvert_Repeat___; overload;
    function Invert_Repeat_(BI_, EI_: NativeInt): TInvert_Repeat___; overload;
    procedure For_C(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_C); overload;
    procedure For_M(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_M); overload;
    procedure For_P(BP_, EP_: PQueueStruct; OnFor: TQueneStructFor_P); overload;
    procedure For_C(OnFor: TQueneStructFor_C); overload;
    procedure For_M(OnFor: TQueneStructFor_M); overload;
    procedure For_P(OnFor: TQueneStructFor_P); overload;
    function ToArray(): TArray_T_;
    function ToOrder(): TOrder_Data_Pool;
    class procedure Swap_(var Left, Right: T_);
    procedure Sort_C(Arry_: PQueueArrayStruct; L, R: NativeInt; OnSort: TSort_C); overload;
    procedure Sort_C(OnSort: TSort_C); overload;
    procedure Sort_M(Arry_: PQueueArrayStruct; L, R: NativeInt; OnSort: TSort_M); overload;
    procedure Sort_M(OnSort: TSort_M); overload;
    procedure Sort_P(Arry_: PQueueArrayStruct; L, R: NativeInt; OnSort: TSort_P); overload;
    procedure Sort_P(OnSort: TSort_P); overload;
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
    property OnFree: TOnStruct_Event read FOnFree write FOnFree;
    property OnAdd: TOnStruct_Event read FOnAdd write FOnAdd;
{$IFDEF DEBUG}
    function Test_Check__: Boolean;
    class procedure Test;
{$ENDIF DEBUG}
  end;

{$IFDEF FPC}
  generic TBig_Object_List<T_: TCore_Object> = class(specialize TBigList<T_>)
{$ELSE FPC}
  TBig_Object_List<T_: class> = class(TBigList<T_>)
{$ENDIF FPC}
  public
    AutoFreeObject: Boolean;
    constructor Create(AutoFreeObject_: Boolean);
    procedure DoFree(var Data: T_); override;
  end;

{$IFDEF FPC}
  generic TCritical_Big_Object_List<T_: TCore_Object> = class(specialize TCriticalBigList<T_>)
{$ELSE FPC}
  TCritical_Big_Object_List<T_: class> = class(TCriticalBigList<T_>)
{$ENDIF FPC}
  public
    AutoFreeObject: Boolean;
    constructor Create(AutoFreeObject_: Boolean);
    procedure DoFree(var Data: T_); override;
  end;

{$ENDREGION 'BigList'}
{$REGION 'Hash-Pair'}
  {$IFDEF FPC}generic{$ENDIF FPC}
  TPair_Pool<T1_, T2_> = class(TCore_Object)
  public type

    TPair = record
      Primary: T1_;
      Second: T2_;
    end;

    PPair = ^TPair;
    TPair_BigList__ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<TPair>;
    PPair__ = TPair_BigList__.PQueueStruct;
  public
    List: TPair_BigList__;
    property L: TPair_BigList__ read List;
    constructor Create;
    destructor Destroy; override;
    function Add_Pair(Primary: T1_; Second: T2_): PPair__;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}
  TPair_Third_Pool<T1_, T2_, T3_> = class(TCore_Object)
  public type

    TPair = record
      Primary: T1_;
      Second: T2_;
      Third: T3_;
    end;

    PPair = ^TPair;
    TPair_BigList__ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<TPair>;
    PPair__ = TPair_BigList__.PQueueStruct;
  public
    List: TPair_BigList__;
    property L: TPair_BigList__ read List;
    constructor Create;
    destructor Destroy; override;
    function Add_Pair(Primary: T1_; Second: T2_; Third: T3_): PPair__;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}
  TPair_Fourth_Pool<T1_, T2_, T3_, T4_> = class(TCore_Object)
  public type

    TPair = record
      Primary: T1_;
      Second: T2_;
      Third: T3_;
      Fourth: T4_;
    end;

    PPair = ^TPair;
    TPair_BigList__ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<TPair>;
    PPair__ = TPair_BigList__.PQueueStruct;
  public
    List: TPair_BigList__;
    property L: TPair_BigList__ read List;
    constructor Create;
    destructor Destroy; override;
    function Add_Pair(Primary: T1_; Second: T2_; Third: T3_; Fourth: T4_): PPair__;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}
  TBig_Hash_Pair_Pool<TKey_, TValue_> = class(TCore_Object)
  public type
    PKey_ = ^TKey_;
    PValue = ^TValue_;
    T___ = {$IFDEF FPC}specialize {$ENDIF FPC} TBig_Hash_Pair_Pool<TKey_, TValue_>;
    TValue_Pair_Pool__ = {$IFDEF FPC}specialize {$ENDIF FPC} TPair_Fourth_Pool<TKey_, TValue_, Pointer, THash>;
    PPair_Pool_Value__ = TValue_Pair_Pool__.PPair__;
    TPair = TValue_Pair_Pool__.TPair;
    TKey_Hash_Buffer = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TValue_Pair_Pool__>;
    TPool___ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<PPair_Pool_Value__>;
    TPool_Queue_Ptr___ = TPool___.PQueueStruct;
    TRepeat___ = TPool___.TRepeat___;
    TInvert_Repeat___ = TPool___.TInvert_Repeat___;
    TArray_Key = array of TKey_;
    TOrder_Key = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderStruct<TKey_>;
    TArray_Value = array of TValue_;
    TOrder_Value = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderStruct<TValue_>;
    TOn_Event = procedure(var Key: TKey_; var Value: TValue_) of object;
    TBig_Hash_Pool_For_C = procedure(p: PPair_Pool_Value__; var Aborted: Boolean);
    TBig_Hash_Pool_For_M = procedure(p: PPair_Pool_Value__; var Aborted: Boolean) of object;
{$IFDEF FPC}
    TBig_Hash_Pool_For_P = procedure(p: PPair_Pool_Value__; var Aborted: Boolean) is nested;
{$ELSE FPC}
    TBig_Hash_Pool_For_P = reference to procedure(p: PPair_Pool_Value__; var Aborted: Boolean);
{$ENDIF FPC}
  private
    FQueue_Pool: TPool___;
    FHash_Buffer: TKey_Hash_Buffer;
    Null_Value: TValue_;
    FOnAdd: TOn_Event;
    FOnFree: TOn_Event;
    function Get_Value_List(const Key_: TKey_; var Key_Hash_: THash): TValue_Pair_Pool__;
    procedure Free_Value_List(Key_Hash_: THash);
    procedure Get_Key_Data_Ptr(const Key_P: PKey_; var p: PByte; var Size: NativeInt);
    procedure Internal_Do_Queue_Pool_Free(var Data: PPair_Pool_Value__);
    procedure Internal_Do_Free(var Data: TPair);
  public
    property Queue_Pool: TPool___ read FQueue_Pool;
    property OnAdd: TOn_Event read FOnAdd write FOnAdd;
    property OnFree: TOn_Event read FOnFree write FOnFree;
    constructor Create(const HashSize_: integer; const Null_Value_: TValue_);
    destructor Destroy; override;
    procedure DoFree(var Key: TKey_; var Value: TValue_); virtual;
    procedure DoAdd(var Key: TKey_; var Value: TValue_); virtual;
    function Get_Key_Hash(const Key_: TKey_): THash; virtual;
    function Compare_Key(const Key_1, Key_2: TKey_): Boolean; virtual;
    function Compare_Value(const Value_1, Value_2: TValue_): Boolean; virtual;
    procedure Clear;
    function Exists_Key(const Key: TKey_): Boolean;
    function Exists_Value(const Data: TValue_): Boolean;
    function Exists(const Key: TKey_): Boolean;
    function Add(const Key: TKey_; const Value: TValue_; Overwrite_: Boolean): PPair_Pool_Value__;
    function Get_Key_Value(const Key: TKey_): TValue_;
    procedure Set_Key_Value(const Key: TKey_; const Value: TValue_);
    property Key_Value[const Key: TKey_]: TValue_ read Get_Key_Value write Set_Key_Value; default;
    procedure Delete(const Key: TKey_);
    procedure Remove(p: PPair_Pool_Value__);
    function Num: NativeInt;
    property Count: NativeInt read Num;
    function GetSum: NativeInt;
    property Sum: NativeInt read GetSum;
    function Get_Value_Ptr(const Key: TKey_): PValue; overload;
    function Get_Value_Ptr(const Key: TKey_; const Default_: TValue_): PValue; overload;
    function Get_Default_Value(const Key: TKey_; const Default_: TValue_): TValue_;
    procedure Set_Default_Value(const Key: TKey_; const Default_: TValue_);
    function Repeat_(): TRepeat___; overload;
    function Repeat_(BI_, EI_: NativeInt): TRepeat___; overload;
    function Invert_Repeat_(): TInvert_Repeat___; overload;
    function Invert_Repeat_(BI_, EI_: NativeInt): TInvert_Repeat___; overload;
    procedure For_C(OnFor: TBig_Hash_Pool_For_C); overload;
    procedure For_M(OnFor: TBig_Hash_Pool_For_M); overload;
    procedure For_P(OnFor: TBig_Hash_Pool_For_P); overload;
    procedure Push_To_Recycle_Pool(p: PPair_Pool_Value__);
    procedure Free_Recycle_Pool;
    function ToPool(): TPool___;
    function ToArray_Key(): TArray_Key;
    function ToOrder_Key(): TOrder_Key;
    function ToArray_Value(): TArray_Value;
    function ToOrder_Value(): TOrder_Value;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}
  TCritical_Big_Hash_Pair_Pool<TKey_, TValue_> = class(TCore_Object)
  public type
    PKey_ = ^TKey_;
    PValue = ^TValue_;
    T___ = {$IFDEF FPC}specialize {$ENDIF FPC} TCritical_Big_Hash_Pair_Pool<TKey_, TValue_>;
    TValue_Pair_Pool__ = {$IFDEF FPC}specialize {$ENDIF FPC} TPair_Fourth_Pool<TKey_, TValue_, Pointer, THash>;
    PPair_Pool_Value__ = TValue_Pair_Pool__.PPair__;
    TPair = TValue_Pair_Pool__.TPair;
    TKey_Hash_Buffer = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TValue_Pair_Pool__>;
    TPool___ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<PPair_Pool_Value__>;
    TPool_Queue_Ptr___ = TPool___.PQueueStruct;
    TRepeat___ = TPool___.TRepeat___;
    TInvert_Repeat___ = TPool___.TInvert_Repeat___;
    TArray_Key = array of TKey_;
    TOrder_Key = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderStruct<TKey_>;
    TArray_Value = array of TValue_;
    TOrder_Value = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderStruct<TValue_>;
    TOn_Event = procedure(var Key: TKey_; var Value: TValue_) of object;
    TBig_Hash_Pool_For_C = procedure(p: PPair_Pool_Value__; var Aborted: Boolean);
    TBig_Hash_Pool_For_M = procedure(p: PPair_Pool_Value__; var Aborted: Boolean) of object;
{$IFDEF FPC}
    TBig_Hash_Pool_For_P = procedure(p: PPair_Pool_Value__; var Aborted: Boolean) is nested;
{$ELSE FPC}
    TBig_Hash_Pool_For_P = reference to procedure(p: PPair_Pool_Value__; var Aborted: Boolean);
{$ENDIF FPC}
  private
    FCritical__: TCritical;
    FQueue_Pool: TPool___;
    FHash_Buffer: TKey_Hash_Buffer;
    Null_Value: TValue_;
    FOnAdd: TOn_Event;
    FOnFree: TOn_Event;
    function Get_Value_List(const Key_: TKey_; var Key_Hash_: THash): TValue_Pair_Pool__;
    procedure Free_Value_List(Key_Hash_: THash);
    procedure Get_Key_Data_Ptr(const Key_P: PKey_; var p: PByte; var Size: NativeInt);
    procedure Internal_Do_Queue_Pool_Free(var Data: PPair_Pool_Value__);
    procedure Internal_Do_Free(var Data: TPair);
  public
    property Critical__: TCritical read FCritical__;
    property Queue_Pool: TPool___ read FQueue_Pool;
    property OnAdd: TOn_Event read FOnAdd write FOnAdd;
    property OnFree: TOn_Event read FOnFree write FOnFree;
    constructor Create(const HashSize_: integer; const Null_Value_: TValue_);
    destructor Destroy; override;
    procedure DoFree(var Key: TKey_; var Value: TValue_); virtual;
    procedure DoAdd(var Key: TKey_; var Value: TValue_); virtual;
    function Get_Key_Hash(const Key_: TKey_): THash; virtual;
    function Compare_Key(const Key_1, Key_2: TKey_): Boolean; virtual;
    function Compare_Value(const Value_1, Value_2: TValue_): Boolean; virtual;
    procedure Clear;
    function Exists_Key(const Key: TKey_): Boolean;
    function Exists_Value(const Data: TValue_): Boolean;
    function Exists(const Key: TKey_): Boolean;
    function Add(const Key: TKey_; const Value: TValue_; Overwrite_: Boolean): PPair_Pool_Value__;
    function Get_Key_Value(const Key: TKey_): TValue_;
    procedure Set_Key_Value(const Key: TKey_; const Value: TValue_);
    property Key_Value[const Key: TKey_]: TValue_ read Get_Key_Value write Set_Key_Value; default;
    procedure Delete(const Key: TKey_);
    procedure Remove(p: PPair_Pool_Value__);
    function Num: NativeInt;
    property Count: NativeInt read Num;
    function GetSum: NativeInt;
    property Sum: NativeInt read GetSum;
    function Get_Value_Ptr(const Key: TKey_): PValue; overload;
    function Get_Value_Ptr(const Key: TKey_; const Default_: TValue_): PValue; overload;
    function Get_Default_Value(const Key: TKey_; const Default_: TValue_): TValue_;
    procedure Set_Default_Value(const Key: TKey_; const Default_: TValue_);
    function Repeat_(): TRepeat___; overload;
    function Repeat_(BI_, EI_: NativeInt): TRepeat___; overload;
    function Invert_Repeat_(): TInvert_Repeat___; overload;
    function Invert_Repeat_(BI_, EI_: NativeInt): TInvert_Repeat___; overload;
    procedure For_C(OnFor: TBig_Hash_Pool_For_C); overload;
    procedure For_M(OnFor: TBig_Hash_Pool_For_M); overload;
    procedure For_P(OnFor: TBig_Hash_Pool_For_P); overload;
    procedure Push_To_Recycle_Pool(p: PPair_Pool_Value__);
    procedure Free_Recycle_Pool;
    function ToPool(): TPool___;
    function ToArray_Key(): TArray_Key;
    function ToOrder_Key(): TOrder_Key;
    function ToArray_Value(): TArray_Value;
    function ToOrder_Value(): TOrder_Value;
  end;

{$ENDREGION 'Hash-Pair'}
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

    // post thread
    procedure PostC1(OnSync: TThreadPost_C1); overload;
    procedure PostC1(OnSync: TThreadPost_C1; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostC2(Data1: Pointer; OnSync: TThreadPost_C2); overload;
    procedure PostC2(Data1: Pointer; OnSync: TThreadPost_C2; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3); overload;
    procedure PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4); overload;
    procedure PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostM1(OnSync: TThreadPost_M1); overload;
    procedure PostM1(OnSync: TThreadPost_M1; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostM2(Data1: Pointer; OnSync: TThreadPost_M2); overload;
    procedure PostM2(Data1: Pointer; OnSync: TThreadPost_M2; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3); overload;
    procedure PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4); overload;
    procedure PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostP1(OnSync: TThreadPost_P1); overload;
    procedure PostP1(OnSync: TThreadPost_P1; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostP2(Data1: Pointer; OnSync: TThreadPost_P2); overload;
    procedure PostP2(Data1: Pointer; OnSync: TThreadPost_P2; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3); overload;
    procedure PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4); overload;
    procedure PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4; IsRuning_, IsExit_: PBoolean); overload;
    // post thread and wait sync
    procedure Sync_Wait_PostC1(OnSync: TThreadPost_C1);
    procedure Sync_Wait_PostC2(Data1: Pointer; OnSync: TThreadPost_C2);
    procedure Sync_Wait_PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3);
    procedure Sync_Wait_PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4);
    procedure Sync_Wait_PostM1(OnSync: TThreadPost_M1);
    procedure Sync_Wait_PostM2(Data1: Pointer; OnSync: TThreadPost_M2);
    procedure Sync_Wait_PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3);
    procedure Sync_Wait_PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4);
    procedure Sync_Wait_PostP1(OnSync: TThreadPost_P1);
    procedure Sync_Wait_PostP2(Data1: Pointer; OnSync: TThreadPost_P2);
    procedure Sync_Wait_PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3);
    procedure Sync_Wait_PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4);
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

    // build-in synchronization
    class procedure Sync(const OnRun_: TRunWithThread_P_NP); overload;
    class procedure Sync(const Thread_: TThread; OnRun_: TRunWithThread_P_NP); overload;
    class procedure SyncC(OnRun_: TRunWithThread_C_NP); overload;
    class procedure SyncC(const Thread_: TThread; OnRun_: TRunWithThread_C_NP); overload;
    class procedure SyncM(OnRun_: TRunWithThread_M_NP); overload;
    class procedure SyncM(const Thread_: TThread; OnRun_: TRunWithThread_M_NP); overload;
    class procedure SyncP(const OnRun_: TRunWithThread_P_NP); overload;
    class procedure SyncP(const Thread_: TThread; OnRun_: TRunWithThread_P_NP); overload;

    // build-in asynchronous thread
    class procedure RunC(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_C); overload;
    class procedure RunC(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_C; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunC(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_C); overload;
    class procedure RunC(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_C; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunC(const OnRun: TRunWithThread_C); overload;
    class procedure RunC(const OnRun: TRunWithThread_C; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunC_NP(const OnRun: TRunWithThread_C_NP); overload;
    class procedure RunC_NP(const OnRun: TRunWithThread_C_NP; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunM(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_M); overload;
    class procedure RunM(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_M; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunM(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_M); overload;
    class procedure RunM(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_M; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunM(const OnRun: TRunWithThread_M); overload;
    class procedure RunM(const OnRun: TRunWithThread_M; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunM_NP(const OnRun: TRunWithThread_M_NP); overload;
    class procedure RunM_NP(const OnRun: TRunWithThread_M_NP; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunP(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_P); overload;
    class procedure RunP(const Data: Pointer; const Obj: TCore_Object; const OnRun, OnDone: TRunWithThread_P; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunP(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_P); overload;
    class procedure RunP(const Data: Pointer; const Obj: TCore_Object; const OnRun: TRunWithThread_P; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunP(const OnRun: TRunWithThread_P); overload;
    class procedure RunP(const OnRun: TRunWithThread_P; IsRuning_, IsExit_: PBoolean); overload;
    class procedure RunP_NP(const OnRun: TRunWithThread_P_NP); overload;
    class procedure RunP_NP(const OnRun: TRunWithThread_P_NP; IsRuning_, IsExit_: PBoolean); overload;

    // main thread progress
    class procedure ProgressPost();

    // post main thread synchronization
    class procedure PostC1(OnSync: TThreadPost_C1); overload;
    class procedure PostC1(OnSync: TThreadPost_C1; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostC2(Data1: Pointer; OnSync: TThreadPost_C2); overload;
    class procedure PostC2(Data1: Pointer; OnSync: TThreadPost_C2; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3); overload;
    class procedure PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4); overload;
    class procedure PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostM1(OnSync: TThreadPost_M1); overload;
    class procedure PostM1(OnSync: TThreadPost_M1; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostM2(Data1: Pointer; OnSync: TThreadPost_M2); overload;
    class procedure PostM2(Data1: Pointer; OnSync: TThreadPost_M2; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3); overload;
    class procedure PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4); overload;
    class procedure PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostP1(OnSync: TThreadPost_P1); overload;
    class procedure PostP1(OnSync: TThreadPost_P1; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostP2(Data1: Pointer; OnSync: TThreadPost_P2); overload;
    class procedure PostP2(Data1: Pointer; OnSync: TThreadPost_P2; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3); overload;
    class procedure PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3; IsRuning_, IsExit_: PBoolean); overload;
    class procedure PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4); overload;
    class procedure PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4; IsRuning_, IsExit_: PBoolean); overload;
    // post main thread and wait synchronization
    class procedure Sync_Wait_PostC1(OnSync: TThreadPost_C1);
    class procedure Sync_Wait_PostC2(Data1: Pointer; OnSync: TThreadPost_C2);
    class procedure Sync_Wait_PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3);
    class procedure Sync_Wait_PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4);
    class procedure Sync_Wait_PostM1(OnSync: TThreadPost_M1);
    class procedure Sync_Wait_PostM2(Data1: Pointer; OnSync: TThreadPost_M2);
    class procedure Sync_Wait_PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3);
    class procedure Sync_Wait_PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4);
    class procedure Sync_Wait_PostP1(OnSync: TThreadPost_P1);
    class procedure Sync_Wait_PostP2(Data1: Pointer; OnSync: TThreadPost_P2);
    class procedure Sync_Wait_PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3);
    class procedure Sync_Wait_PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4);
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
    class function Rand32: Integer; overload; static;
    class function Rand32(L: Integer): Integer; overload; static;
    class procedure Rand32(L: Integer; dest: PInteger; num: NativeInt); overload; static;
    class function Rand64: Int64; overload; static;
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
{$Region 'core-const'}
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
{$EndRegion 'core-const'}
{$Region 'Parallel-API'}

function Max_Thread_Supported: Integer;
function Get_System_Critical_Recycle_Pool_Num: NativeInt;
function Get_MT19937_POOL_Num: NativeInt;
function Get_Object_Lock_Pool_Num: NativeInt;
function Get_Parallel_Granularity: Integer;
procedure Set_Parallel_Granularity(Thread_Num: Integer);
procedure Set_IDLE_Compute_Wait_Time_Tick(Tick_: TTimeTick);

{$IFDEF FPC}
type
  // freepascal
  TFPCParallel_P32 = procedure(pass: Integer) is nested;
  TFPCParallel_P64 = procedure(pass: Int64) is nested;
// parallel core
procedure FPCParallelFor_Block(ThNum: Integer; parallel: Boolean; b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure FPCParallelFor_Block(parallel: Boolean; b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure FPCParallelFor_Block(ThNum: Integer; parallel: Boolean; b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure FPCParallelFor_Block(parallel: Boolean; b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure FPCParallelFor_Fold(ThNum: Integer; parallel: Boolean; b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure FPCParallelFor_Fold(parallel: Boolean; b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure FPCParallelFor_Fold(ThNum: Integer; parallel: Boolean; b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure FPCParallelFor_Fold(parallel: Boolean; b, e: Int64; OnFor: TFPCParallel_P64); overload;
// parallel package
procedure FPCParallelFor(ThNum: Integer; parallel: Boolean; b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure FPCParallelFor(ThNum: Integer; parallel: Boolean; b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure FPCParallelFor(parallel: Boolean; b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure FPCParallelFor(parallel: Boolean; b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure FPCParallelFor(b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure FPCParallelFor(b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure FPCParallelFor(OnFor: TFPCParallel_P32; b, e: Integer); overload;
procedure FPCParallelFor(OnFor: TFPCParallel_P64; b, e: Int64); overload;
procedure FPCParallelFor(parallel: Boolean; OnFor: TFPCParallel_P32; b, e: Integer); overload;
procedure FPCParallelFor(parallel: Boolean; OnFor: TFPCParallel_P64; b, e: Int64); overload;
procedure ParallelFor(ThNum: Integer; parallel: Boolean; b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure ParallelFor(ThNum: Integer; parallel: Boolean; b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure ParallelFor(parallel: Boolean; b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure ParallelFor(parallel: Boolean; b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure ParallelFor(b, e: Integer; OnFor: TFPCParallel_P32); overload;
procedure ParallelFor(b, e: Int64; OnFor: TFPCParallel_P64); overload;
procedure ParallelFor(OnFor: TFPCParallel_P32; b, e: Integer); overload;
procedure ParallelFor(OnFor: TFPCParallel_P64; b, e: Int64); overload;
procedure ParallelFor(ThNum: Integer; parallel: Boolean; OnFor: TFPCParallel_P32; b, e: Integer); overload;
procedure ParallelFor(ThNum: Integer; parallel: Boolean; OnFor: TFPCParallel_P64; b, e: Int64); overload;
procedure ParallelFor(parallel: Boolean; OnFor: TFPCParallel_P32; b, e: Integer); overload;
procedure ParallelFor(parallel: Boolean; OnFor: TFPCParallel_P64; b, e: Int64); overload;
{$ELSE FPC}
type
  // delphi
  TDelphiParallel_P32 = reference to procedure(pass: Integer);
  TDelphiParallel_P64 = reference to procedure(pass: Int64);
// parallel core
procedure DelphiParallelFor_Block(ThNum: Integer; parallel: Boolean; b, e: Integer; OnFor: TDelphiParallel_P32); overload;
procedure DelphiParallelFor_Block(parallel: Boolean; b, e: Integer; OnFor: TDelphiParallel_P32); overload;
procedure DelphiParallelFor_Block(ThNum: Integer; parallel: Boolean; b, e: Int64; OnFor: TDelphiParallel_P64); overload;
procedure DelphiParallelFor_Block(parallel: Boolean; b, e: Int64; OnFor: TDelphiParallel_P64); overload;
procedure DelphiParallelFor_Fold(ThNum: Integer; parallel: Boolean; b, e: Integer; OnFor: TDelphiParallel_P32); overload;
procedure DelphiParallelFor_Fold(parallel: Boolean; b, e: Integer; OnFor: TDelphiParallel_P32); overload;
procedure DelphiParallelFor_Fold(ThNum: Integer; parallel: Boolean; b, e: Int64; OnFor: TDelphiParallel_P64); overload;
procedure DelphiParallelFor_Fold(parallel: Boolean; b, e: Int64; OnFor: TDelphiParallel_P64); overload;
// parallel package
procedure DelphiParallelFor(ThNum: Integer; parallel: Boolean; b, e: Integer; OnFor: TDelphiParallel_P32); overload;
procedure DelphiParallelFor(ThNum: Integer; parallel: Boolean; b, e: Int64; OnFor: TDelphiParallel_P64); overload;
procedure DelphiParallelFor(parallel: Boolean; b, e: Integer; OnFor: TDelphiParallel_P32); overload;
procedure DelphiParallelFor(parallel: Boolean; b, e: Int64; OnFor: TDelphiParallel_P64); overload;
procedure DelphiParallelFor(b, e: Integer; OnFor: TDelphiParallel_P32); overload;
procedure DelphiParallelFor(b, e: Int64; OnFor: TDelphiParallel_P64); overload;
procedure DelphiParallelFor(OnFor: TDelphiParallel_P32; b, e: Integer); overload;
procedure DelphiParallelFor(OnFor: TDelphiParallel_P64; b, e: Int64); overload;
procedure DelphiParallelFor(parallel: Boolean; OnFor: TDelphiParallel_P32; b, e: Integer); overload;
procedure DelphiParallelFor(parallel: Boolean; OnFor: TDelphiParallel_P64; b, e: Int64); overload;
procedure ParallelFor(ThNum: Integer; parallel: Boolean; b, e: Integer; OnFor: TDelphiParallel_P32); overload;
procedure ParallelFor(ThNum: Integer; parallel: Boolean; b, e: Int64; OnFor: TDelphiParallel_P64); overload;
procedure ParallelFor(parallel: Boolean; b, e: Integer; OnFor: TDelphiParallel_P32); overload;
procedure ParallelFor(parallel: Boolean; b, e: Int64; OnFor: TDelphiParallel_P64); overload;
procedure ParallelFor(b, e: Integer; OnFor: TDelphiParallel_P32); overload;
procedure ParallelFor(b, e: Int64; OnFor: TDelphiParallel_P64); overload;
procedure ParallelFor(OnFor: TDelphiParallel_P32; b, e: Integer); overload;
procedure ParallelFor(OnFor: TDelphiParallel_P64; b, e: Int64); overload;
procedure ParallelFor(ThNum: Integer; parallel: Boolean; OnFor: TDelphiParallel_P32; b, e: Integer); overload;
procedure ParallelFor(ThNum: Integer; parallel: Boolean; OnFor: TDelphiParallel_P64; b, e: Int64); overload;
procedure ParallelFor(parallel: Boolean; OnFor: TDelphiParallel_P32; b, e: Integer); overload;
procedure ParallelFor(parallel: Boolean; OnFor: TDelphiParallel_P64; b, e: Int64); overload;
{$ENDIF FPC}

{$EndRegion 'Parallel-API'}
{$Region 'core api'}

// NoP = No Operation. It's the empty function, whose purpose is only for the
// debugging, or for the piece of code where intentionaly nothing is planned to be.
procedure Nop;

// debug state
function IsDebuging: Boolean;

// process Synchronize
function IsMainThread: Boolean;
procedure CheckThreadSynchronize; overload;
function CheckThreadSynchronize(Timeout: Integer): Boolean; overload;
procedure CheckThreadSync; overload;
function CheckThreadSync(Timeout: Integer): Boolean; overload;
procedure CheckThread; overload;
function CheckThread(Timeout: Integer): Boolean; overload;

// core thread pool
procedure FreeCoreThreadPool;

function DisposeObject(const Obj: TObject): Boolean; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure DisposeObject(const objs: array of TObject); overload;
function FreeObj(const Obj: TObject): Boolean; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function FreeObject(const Obj: TObject): Boolean; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure FreeObject(const objs: array of TObject); overload;
function DisposeObjectAndNil(var Obj): Boolean; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function FreeObjAndNil(var Obj): Boolean; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}

procedure LockObject(Obj: TObject);
procedure UnLockObject(Obj: TObject);

const
  C_CRC32Table: array [0 .. 255] of Cardinal = (
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535,
    $9E6495A3, $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD,
    $E7B82D07, $90BF1D91, $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D,
    $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4,
    $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C,
    $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC,
    $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB,
    $B6662D3D, $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F,
    $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB,
    $086D3D2D, $91646C97, $E6635C01, $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950, $8BBEB8EA,
    $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65, $4DB26158, $3AB551CE,
    $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A,
    $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409,
    $CE61E49F, $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81,
    $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739,
    $9DD277AF, $04DB2615, $73DC1683, $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, $F00F9344, $8708A3D2, $1E01F268,
    $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7, $FED41B76, $89D32BE0,
    $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8,
    $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF,
    $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703,
    $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7,
    $B5D0CF31, $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713, $95BF4A82, $E2B87A14, $7BB12BAE,
    $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242,
    $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777, $88085AE6,
    $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D,
    $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5,
    $47B2CF7F, $30B5FFE9, $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605,
    $CDD70693, $54DE5729, $23D967BF, $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
    );

function Get_CRC32(Data: PByte; Size: NativeInt): THash; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function Hash_Key_Mod(const hash: THash; const Num: integer): integer; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function DeltaStep(const value_, Delta_: NativeInt): NativeInt; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
procedure AtomInc(var x: Int64); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomInc(var x: Int64; const v: Int64); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: Int64); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: Int64; const v: Int64); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomInc(var x: UInt64); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomInc(var x: UInt64; const v: UInt64); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: UInt64); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: UInt64; const v: UInt64); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomInc(var x: Integer); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomInc(var x: Integer; const v:Integer); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: Integer); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: Integer; const v:Integer); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomInc(var x: Cardinal); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomInc(var x: Cardinal; const v:Cardinal); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: Cardinal); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure AtomDec(var x: Cardinal; const v:Cardinal); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

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
function SameF(const A, B: Double; Epsilon: Double = 0): Boolean; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function SameF(const A, B: Single; Epsilon: Single = 0): Boolean; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

// MT19937 random num
function MT19937CoreToDelphi: Boolean;
function MT19937InstanceNum(): Integer;
procedure SetMT19937Seed(seed: Integer);
function GetMT19937Seed(): Integer;
procedure MT19937Randomize();
function MT19937Rand32: Integer; overload;
function MT19937Rand32(L: Integer): Integer; overload;
procedure MT19937Rand32(L: Integer; dest: PInteger; num: NativeInt); overload;
function MT19937Rand64: Int64; overload;
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

function ROL8(const Value: Byte; Shift: Byte): Byte; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function ROL16(const Value: Word; Shift: Byte): Word; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function ROL32(const Value: Cardinal; Shift: Byte): Cardinal; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function ROL64(const Value: UInt64; Shift: Byte): UInt64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function ROR8(const Value: Byte; Shift: Byte): Byte; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function ROR16(const Value: Word; Shift: Byte): Word; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function ROR32(const Value: Cardinal; Shift: Byte): Cardinal; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function ROR64(const Value: UInt64; Shift: Byte): UInt64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}

function Endian(const Value: SmallInt): SmallInt; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function Endian(const Value: Word): Word; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function Endian(const Value: Integer): Integer; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function Endian(const Value: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function Endian(const Value: Int64): Int64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function Endian(const Value: UInt64): UInt64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

function BE2N(const Value: SmallInt): SmallInt; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function BE2N(const Value: Word): Word; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function BE2N(const Value: Integer): Integer; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function BE2N(const Value: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function BE2N(const Value: Int64): Int64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function BE2N(const Value: UInt64): UInt64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

function LE2N(const Value: SmallInt): SmallInt; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function LE2N(const Value: Word): Word; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function LE2N(const Value: Integer): Integer; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function LE2N(const Value: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function LE2N(const Value: Int64): Int64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function LE2N(const Value: UInt64): UInt64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

function N2BE(const Value: SmallInt): SmallInt; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2BE(const Value: Word): Word; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2BE(const Value: Integer): Integer; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2BE(const Value: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2BE(const Value: Int64): Int64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2BE(const Value: UInt64): UInt64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

function N2LE(const Value: SmallInt): SmallInt; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2LE(const Value: Word): Word; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2LE(const Value: Integer): Integer; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2LE(const Value: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2LE(const Value: Int64): Int64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function N2LE(const Value: UInt64): UInt64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

procedure Swap(var v1, v2: Byte); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: Word); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: Integer); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: Cardinal); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: Int64); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: UInt64); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
{$IFDEF OVERLOAD_NATIVEINT}
procedure Swap(var v1, v2: NativeInt); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: NativeUInt); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
{$ENDIF OVERLOAD_NATIVEINT}
procedure Swap(var v1, v2: string); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: Single); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: Double); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure Swap(var v1, v2: Pointer); {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
procedure SwapVariant(var v1, v2: Variant);
function Swap(const v: Word): Word; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function Swap(const v: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function Swap(const v: UInt64): UInt64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;

function SAR16(const Value: SmallInt; const Shift: Byte): SmallInt; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function SAR32(const Value: Integer; Shift: Byte): Integer; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function SAR64(const Value: Int64; Shift: Byte): Int64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}

function MemoryAlign(addr: Pointer; alignment_: NativeUInt): Pointer; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}

function if_(const bool_: Boolean; const True_, False_: Boolean): Boolean; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: ShortInt): ShortInt; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: SmallInt): SmallInt; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: Integer): Integer; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: Int64): Int64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: Byte): Byte; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: Word): Word; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: UInt64): UInt64; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: Single): Single; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: Double): Double; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function if_(const bool_: Boolean; const True_, False_: string): string; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM} overload;
function ifv_(const bool_: Boolean; const True_, False_: Variant): Variant; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function GetOffset(p_: Pointer; offset_: NativeInt): Pointer; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}
function GetPtr(p_: Pointer; offset_: NativeInt): Pointer; {$IFDEF INLINE_ASM} inline;{$ENDIF INLINE_ASM}

{$EndRegion 'core api'}
{$Region 'core var'}

type TOnCheckThreadSynchronize = procedure();

var
  Enabled_Check_Thread_Synchronize_System: Boolean;
  Main_Thread_Synchronize_Running: Boolean;
  Main_Thread_OnCheck_Runing: Boolean;
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

function DisposeObject(const Obj: TObject): Boolean;
begin
  Result := False;
  if Obj = nil then
    exit;
  try
    {$IFDEF AUTOREFCOUNT}Obj.DisposeOf;{$ELSE AUTOREFCOUNT}Obj.Free;{$ENDIF AUTOREFCOUNT}
    Result := True;
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

function FreeObj(const Obj: TObject): Boolean;
begin
  Result := DisposeObject(Obj);
end;

function FreeObject(const Obj: TObject): Boolean;
begin
  Result := DisposeObject(Obj);
end;

procedure FreeObject(const objs: array of TObject);
var
  Obj: TObject;
begin
  for Obj in objs do
      DisposeObject(Obj);
end;

function DisposeObjectAndNil(var Obj): Boolean;
begin
  Result := False;
  if TObject(Obj) <> nil then
    begin
      Result := DisposeObject(TObject(Obj));
      TObject(Obj) := nil;
    end;
end;

function FreeObjAndNil(var Obj): Boolean;
begin
  Result := DisposeObjectAndNil(Obj);
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

function TGenericsList<t>.ListData: PGArry;
begin
  // set array pointer
  Arry := TGArry(Pointer(inherited List));
  // @ array
  Result := @Arry;
end;

function TGenericsObjectList<t>.ListData: PGArry;
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

procedure Nop;
begin
end;

function IsDebuging: Boolean;
begin
  Result := False;
{$IFDEF DELPHI}
{$IFDEF MSWINDOWS}
  Result := DebugHook > 0;
{$ENDIF MSWINDOWS}
{$ENDIF DELPHI}
end;

function IsMainThread: Boolean;
begin
  Result := TCore_Thread.CurrentThread.ThreadID = MainThreadID;
end;

procedure CheckThreadSynchronize;
begin
  CheckThreadSynchronize(0);
end;

function CheckThreadSynchronize(Timeout: Integer): Boolean;
begin
  Result := False;

  if (TCore_Thread.CurrentThread.ThreadID <> MainThreadID) then
    begin
      if Timeout > 0 then
          TCore_Thread.Sleep(Timeout);
      Result := False;
    end
  else if Enabled_Check_Thread_Synchronize_System then
    begin
      MainThreadProgress.Progress(MainThreadID);

      if not Main_Thread_Synchronize_Running then
        begin
          Main_Thread_Synchronize_Running := True;
          try
              Result := CheckSynchronize(Timeout);
          except
              Result := False;
          end;
          Main_Thread_Synchronize_Running := False;
        end;

      if not Main_Thread_OnCheck_Runing then
        begin
          Main_Thread_OnCheck_Runing := True;
          try
            if Assigned(OnCheckThreadSynchronize) then
                OnCheckThreadSynchronize();
          except
              Result := False;
          end;
          Main_Thread_OnCheck_Runing := False;
        end;
    end;
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

{$I Z.Core.OrderData.inc}
{$I Z.Core.BigList.inc}
{$I Z.Core.Hash_Pair.inc}

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
  InitCoreThreadPool(if_(IsDebuging, 2, CpuCount * 2));
  MainThreadProgress := TThreadPost.Create(MainThreadID);
  MainThreadProgress.OneStep := False;
  Enabled_Check_Thread_Synchronize_System := True;
  Main_Thread_Synchronize_Running := False;
  Main_Thread_OnCheck_Runing := False;
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

