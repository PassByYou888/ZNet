{ ****************************************************************************** }
{ * List Library                                                               * }
{ ****************************************************************************** }
unit Z.ListEngine;

{$I Z.Define.inc}

interface

uses SysUtils, Classes, Variants,
  Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings;

type
  TSeedCounter = NativeUInt;

  TListBuffer = array of TCore_List;
  PListBuffer = ^TListBuffer;

  THashObjectList = class;
  THashVariantList = class;
  THashStringList = class;
  TListPascalString = class;
  TListString = class;
  TPascalStringList = TListPascalString;
  TPascalStrings = TListPascalString;
  TPascalStringHashList = THashStringList;
  TPascalStringHash = THashStringList;

{$REGION 'THashList'}
  PHashListData = ^THashListData;

  THashListData = record
    qHash: THash;
    LowerCaseName, OriginName: SystemString;
    Data: Pointer;
    ID: TSeedCounter;
    Prev, Next: PHashListData;
  end;

  TOnPtr = procedure(p: Pointer) of object;

  THashDataArray = array of PHashListData;

  THashListLoop_C = procedure(Name_: PSystemString; hData: PHashListData);
  THashListLoop_M = procedure(Name_: PSystemString; hData: PHashListData) of object;
{$IFDEF FPC}
  THashListLoop_P = procedure(Name_: PSystemString; hData: PHashListData) is nested;
{$ELSE FPC}
  THashListLoop_P = reference to procedure(Name_: PSystemString; hData: PHashListData);
{$ENDIF FPC}

  THashList = class(TCore_Object)
  private
    FListBuffer: TListBuffer;
    FAutoFreeData: Boolean;
    FCount: NativeInt;
    FIDSeed: TSeedCounter;
    FIgnoreCase: Boolean;
    FAccessOptimization: Boolean;
    FOnFreePtr: TOnPtr;

    FFirst: PHashListData;
    FLast: PHashListData;

    FMaxNameSize, FMinNameSize: NativeInt;

    function GetListTable(hash: THash; AutoCreate: Boolean): TCore_List;
    function GetKeyData(const Name: SystemString): PHashListData;
    function GetKeyValue(const Name: SystemString): Pointer;

    procedure RebuildIDSeedCounter;

    procedure DoAdd(p: PHashListData);
    procedure DoInsertBefore(p, To_: PHashListData);
    procedure DoDelete(p: PHashListData);
    procedure DefaultDataFreeProc(p: Pointer);

    procedure DoDataFreeProc(p: Pointer);
  public
    constructor Create;
    constructor CustomCreate(HashPoolSize_: Integer);
    destructor Destroy; override;
    procedure Clear;
    procedure MergeTo(dest: THashList);
    procedure GetNameList(var Output_: TArrayPascalString); overload;
    procedure GetNameList(OutputList: TListString); overload;
    procedure GetNameList(OutputList: TListPascalString); overload;
    procedure GetNameList(OutputList: TCore_Strings); overload;
    procedure GetListData(OutputList: TCore_List);
    function GetHashDataArray(): THashDataArray;
    procedure Delete(const Name: SystemString);
    function Add(const Name: SystemString; Data_: Pointer; const Overwrite_: Boolean): PHashListData; overload;
    procedure Add(const Name: SystemString; Data_: Pointer); overload;
    procedure SetValue(const Name: SystemString; const Data_: Pointer);
    function Insert(Name, InsertToBefore_: SystemString; Data_: Pointer; const Overwrite_: Boolean): PHashListData;
    function Find(const Name: SystemString): Pointer;
    function Exists(const Name: SystemString): Boolean;
    procedure SetHashBlockCount(HashPoolSize_: Integer);

    property FirstPtr: PHashListData read FFirst write FFirst;
    property LastPtr: PHashListData read FLast write FLast;

    function First: Pointer;
    function Last: Pointer;
    function GetNext(const Name: SystemString): Pointer;
    function GetPrev(const Name: SystemString): Pointer;
    function ListBuffer: PListBuffer;

    procedure ProgressC(const OnProgress: THashListLoop_C);
    procedure ProgressM(const OnProgress: THashListLoop_M);
    procedure ProgressP(const OnProgress: THashListLoop_P);
    procedure PrintHashReport;

    property AutoFreeData: Boolean read FAutoFreeData write FAutoFreeData;
    property IgnoreCase: Boolean read FIgnoreCase write FIgnoreCase;
    property AccessOptimization: Boolean read FAccessOptimization write FAccessOptimization;
    property Count: NativeInt read FCount write FCount;

    property KeyValue[const Name: SystemString]: Pointer read GetKeyValue write SetValue; default;
    property NameValue[const Name: SystemString]: Pointer read GetKeyValue write SetValue;

    property KeyData[const Name: SystemString]: PHashListData read GetKeyData;
    property NameData[const Name: SystemString]: PHashListData read GetKeyData;

    property OnFreePtr: TOnPtr read FOnFreePtr write FOnFreePtr;

    property MaxKeySize: NativeInt read FMaxNameSize;
    property MinKeySize: NativeInt read FMinNameSize;
    property MaxNameSize: NativeInt read FMaxNameSize;
    property MinNameSize: NativeInt read FMinNameSize;
    // alias
    property MaxKeyLen: NativeInt read FMaxNameSize;
    property MinKeyLen: NativeInt read FMinNameSize;
    property MaxNameLen: NativeInt read FMaxNameSize;
    property MinNameLen: NativeInt read FMinNameSize;
  end;

  PHashList = ^THashList;
{$ENDREGION 'THashList'}
{$REGION 'TInt64HashObjectList'}
  PInt64HashListObjectStruct = ^TInt64HashListObjectStruct;

  TInt64HashListObjectStruct = record
    qHash: THash;
    i64: Int64;
    Data: TCore_Object;
    ID: TSeedCounter;
    Prev, Next: PInt64HashListObjectStruct;
  end;

  TObjectFreeProc = procedure(Obj: TCore_Object) of object;

  TInt64HashObjectListLoop_C = procedure(i64: Int64; Value: TCore_Object);
  TInt64HashObjectListLoop_M = procedure(i64: Int64; Value: TCore_Object) of object;
{$IFDEF FPC}
  TInt64HashObjectListLoop_P = procedure(i64: Int64; Value: TCore_Object) is nested;
{$ELSE FPC}
  TInt64HashObjectListLoop_P = reference to procedure(i64: Int64; Value: TCore_Object);
{$ENDIF FPC}

  TInt64HashObjectList = class(TCore_Object)
  private
    FListBuffer: TListBuffer;
    FCount: NativeInt;
    FIDSeed: TSeedCounter;
    FAccessOptimization: Boolean;
    FAutoFreeData: Boolean;
    FFirst: PInt64HashListObjectStruct;
    FLast: PInt64HashListObjectStruct;
    FOnObjectFreeProc: TObjectFreeProc;

    function GetListTable(hash: THash; AutoCreate: Boolean): TCore_List;
    function Geti64Data(i64: Int64): PInt64HashListObjectStruct;
    function Geti64Val(i64: Int64): TCore_Object;

    procedure RebuildIDSeedCounter;

    procedure DoAdd(p: PInt64HashListObjectStruct);
    procedure DoInsertBefore(p, To_: PInt64HashListObjectStruct);
    procedure DoDelete(p: PInt64HashListObjectStruct);
    procedure DefaultObjectFreeProc(Obj: TCore_Object);
    procedure DoDataFreeProc(Obj: TCore_Object);
  public
    constructor Create;
    constructor CustomCreate(HashPoolSize_: Integer);
    destructor Destroy; override;
    procedure Clear;
    procedure GetListData(OutputList: TCore_List);
    procedure Delete(i64: Int64);
    function Add(i64: Int64; Data_: TCore_Object; const Overwrite_: Boolean): PInt64HashListObjectStruct;
    procedure SetValue(i64: Int64; Data_: TCore_Object);
    function Insert(i64, InsertToBefore_: Int64; Data_: TCore_Object; const Overwrite_: Boolean): PInt64HashListObjectStruct;
    function Exists(i64: Int64): Boolean;
    procedure SetHashBlockCount(HashPoolSize_: Integer);

    procedure DeleteFirst;
    procedure DeleteLast;

    property FirstPtr: PInt64HashListObjectStruct read FFirst write FFirst;
    property LastPtr: PInt64HashListObjectStruct read FLast write FLast;

    function First: TCore_Object;
    function Last: TCore_Object;
    function GetNext(i64: Int64): TCore_Object;
    function GetPrev(i64: Int64): TCore_Object;
    function ListBuffer: PListBuffer;

    procedure ProgressC(const OnProgress: TInt64HashObjectListLoop_C);
    procedure ProgressM(const OnProgress: TInt64HashObjectListLoop_M);
    procedure ProgressP(const OnProgress: TInt64HashObjectListLoop_P);
    // print hash status
    procedure PrintHashReport;

    property AutoFreeData: Boolean read FAutoFreeData write FAutoFreeData;
    property AccessOptimization: Boolean read FAccessOptimization write FAccessOptimization;
    property Count: NativeInt read FCount write FCount;
    property i64Val[i64: Int64]: TCore_Object read Geti64Val write SetValue; default;
    property i64Data[i64: Int64]: PInt64HashListObjectStruct read Geti64Data;
    property OnObjectFreeProc: TObjectFreeProc read FOnObjectFreeProc write FOnObjectFreeProc;
  end;
{$ENDREGION 'TInt64HashObjectList'}
{$REGION 'TInt64HashPointerList'}

  PInt64HashListPointerStruct = ^TInt64HashListPointerStruct;

  TInt64HashListPointerStruct = record
    qHash: THash;
    i64: Int64;
    Data: Pointer;
    ID: TSeedCounter;
    Prev, Next: PInt64HashListPointerStruct;
  end;

  TInt64HashPointerListLoop_C = procedure(i64: Int64; Value: Pointer);
  TInt64HashPointerListLoop_M = procedure(i64: Int64; Value: Pointer) of object;
{$IFDEF FPC}
  TInt64HashPointerListLoop_P = procedure(i64: Int64; Value: Pointer) is nested;
{$ELSE FPC}
  TInt64HashPointerListLoop_P = reference to procedure(i64: Int64; Value: Pointer);
{$ENDIF FPC}

  TInt64HashPointerList = class(TCore_Object)
  private
    FListBuffer: TListBuffer;
    FCount: NativeInt;
    FIDSeed: TSeedCounter;
    FAccessOptimization: Boolean;
    FAutoFreeData: Boolean;
    FFirst: PInt64HashListPointerStruct;
    FLast: PInt64HashListPointerStruct;
    FOnFreePtr: TOnPtr;
    FOnAddPtr: TOnPtr;

    function GetListTable(hash: THash; AutoCreate: Boolean): TCore_List;
    function Geti64Data(i64: Int64): PInt64HashListPointerStruct;
    function Geti64Val(i64: Int64): Pointer;

    procedure RebuildIDSeedCounter;

    procedure DoAdd(p: PInt64HashListPointerStruct);
    procedure DoInsertBefore(p, To_: PInt64HashListPointerStruct);
    procedure DoDelete(p: PInt64HashListPointerStruct);
    procedure DefaultDataFreeProc(p: Pointer);
    procedure DoDataFreeProc(p: Pointer);
    procedure DoAddDataNotifyProc(p: Pointer);
  public
    constructor Create;
    constructor CustomCreate(HashPoolSize_: Integer);
    destructor Destroy; override;
    procedure Clear;
    procedure GetListData(OutputList: TCore_List);
    procedure Delete(i64: Int64);
    function Add(i64: Int64; Data_: Pointer; const Overwrite_: Boolean): PInt64HashListPointerStruct;
    procedure SetValue(i64: Int64; Data_: Pointer);
    function Insert(i64, InsertToBefore_: Int64; Data_: Pointer; const Overwrite_: Boolean): PInt64HashListPointerStruct;
    function Exists(i64: Int64): Boolean;
    procedure SetHashBlockCount(HashPoolSize_: Integer);

    property FirstPtr: PInt64HashListPointerStruct read FFirst write FFirst;
    property LastPtr: PInt64HashListPointerStruct read FLast write FLast;

    function First: Pointer;
    function Last: Pointer;
    function GetNext(i64: Int64): Pointer;
    function GetPrev(i64: Int64): Pointer;
    function ListBuffer: PListBuffer;

    procedure ProgressC(const OnProgress: TInt64HashPointerListLoop_C);
    procedure ProgressM(const OnProgress: TInt64HashPointerListLoop_M);
    procedure ProgressP(const OnProgress: TInt64HashPointerListLoop_P);
    // print hash status
    procedure PrintHashReport;

    property AutoFreeData: Boolean read FAutoFreeData write FAutoFreeData;
    property AccessOptimization: Boolean read FAccessOptimization write FAccessOptimization;
    property Count: NativeInt read FCount write FCount;
    property i64Val[i64: Int64]: Pointer read Geti64Val write SetValue; default;
    property i64Data[i64: Int64]: PInt64HashListPointerStruct read Geti64Data;
    property OnFreePtr: TOnPtr read FOnFreePtr write FOnFreePtr;
    property OnAddPtr: TOnPtr read FOnAddPtr write FOnAddPtr;
  end;
{$ENDREGION 'TInt64HashPointerList'}
{$REGION 'TUInt32HashObjectList'}

  PUInt32HashListObjectStruct = ^TUInt32HashListObjectStruct;

  TUInt32HashListObjectStruct = record
    qHash: THash;
    u32: UInt32;
    Data: TCore_Object;
    ID: TSeedCounter;
    Prev, Next: PUInt32HashListObjectStruct;
  end;

  TUInt32HashObjectListLoop_C = procedure(u32: UInt32; Value: TCore_Object);
  TUInt32HashObjectListLoop_M = procedure(u32: UInt32; Value: TCore_Object) of object;
{$IFDEF FPC}
  TUInt32HashObjectListLoop_P = procedure(u32: UInt32; Value: TCore_Object) is nested;
{$ELSE FPC}
  TUInt32HashObjectListLoop_P = reference to procedure(u32: UInt32; Value: TCore_Object);
{$ENDIF FPC}

  TUInt32HashObjectList = class(TCore_Object)
  private
    FListBuffer: TListBuffer;
    FCount: NativeInt;
    FIDSeed: TSeedCounter;
    FAccessOptimization: Boolean;
    FAutoFreeData: Boolean;
    FFirst: PUInt32HashListObjectStruct;
    FLast: PUInt32HashListObjectStruct;

    function GetListTable(hash: THash; AutoCreate: Boolean): TCore_List;
    function Getu32Data(u32: UInt32): PUInt32HashListObjectStruct;
    function Getu32Val(u32: UInt32): TCore_Object;

    procedure RebuildIDSeedCounter;

    procedure DoAdd(p: PUInt32HashListObjectStruct);
    procedure DoInsertBefore(p, To_: PUInt32HashListObjectStruct);
    procedure DoDelete(p: PUInt32HashListObjectStruct);
    procedure DoDataFreeProc(Obj: TCore_Object);
  public
    constructor Create;
    constructor CustomCreate(HashPoolSize_: Integer);
    destructor Destroy; override;
    procedure Clear;
    procedure GetListData(OutputList: TCore_List);
    procedure Delete(u32: UInt32);
    function Add(u32: UInt32; Data_: TCore_Object; const Overwrite_: Boolean): PUInt32HashListObjectStruct;
    procedure SetValue(u32: UInt32; Data_: TCore_Object);
    function Insert(u32, InsertToBefore_: UInt32; Data_: TCore_Object; const Overwrite_: Boolean): PUInt32HashListObjectStruct;
    function Exists(u32: UInt32): Boolean;
    procedure SetHashBlockCount(HashPoolSize_: Integer);

    property FirstPtr: PUInt32HashListObjectStruct read FFirst write FFirst;
    property LastPtr: PUInt32HashListObjectStruct read FLast write FLast;

    function First: TCore_Object;
    function Last: TCore_Object;
    function GetNext(u32: UInt32): TCore_Object;
    function GetPrev(u32: UInt32): TCore_Object;
    function ListBuffer: PListBuffer;
    procedure ProgressC(const OnProgress: TUInt32HashObjectListLoop_C);
    procedure ProgressM(const OnProgress: TUInt32HashObjectListLoop_M);
    procedure ProgressP(const OnProgress: TUInt32HashObjectListLoop_P);
    //
    function ExistsObject(Obj: TCore_Object): Boolean;

    procedure PrintHashReport;

    property AutoFreeData: Boolean read FAutoFreeData write FAutoFreeData;
    property AccessOptimization: Boolean read FAccessOptimization write FAccessOptimization;
    property Count: NativeInt read FCount write FCount;
    property u32Val[u32: UInt32]: TCore_Object read Getu32Val write SetValue; default;
    property u32Data[u32: UInt32]: PUInt32HashListObjectStruct read Getu32Data;
  end;
{$ENDREGION 'TUInt32HashObjectList'}
{$REGION 'TUInt32HashPointerList'}

  PUInt32HashListPointerStruct = ^TUInt32HashListPointerStruct;

  TUInt32HashListPointerStruct = record
    qHash: THash;
    u32: UInt32;
    Data: Pointer;
    ID: TSeedCounter;
    Prev, Next: PUInt32HashListPointerStruct;
  end;

  TUInt32HashPointerListLoop_C = procedure(u32: UInt32; pData: Pointer);
  TUInt32HashPointerListLoop_M = procedure(u32: UInt32; pData: Pointer) of object;
{$IFDEF FPC}
  TUInt32HashPointerListLoop_P = procedure(u32: UInt32; pData: Pointer) is nested;
{$ELSE FPC}
  TUInt32HashPointerListLoop_P = reference to procedure(u32: UInt32; pData: Pointer);
{$ENDIF FPC}

  TUInt32HashPointerList = class(TCore_Object)
  private
    FListBuffer: TListBuffer;
    FCount: NativeInt;
    FIDSeed: TSeedCounter;
    FAccessOptimization: Boolean;
    FAutoFreeData: Boolean;
    FFirst: PUInt32HashListPointerStruct;
    FLast: PUInt32HashListPointerStruct;
    FOnFreePtr: TOnPtr;
    FOnAddPtr: TOnPtr;

    function GetListTable(hash: THash; AutoCreate: Boolean): TCore_List;
    function Getu32Data(u32: UInt32): PUInt32HashListPointerStruct;
    function Getu32Val(u32: UInt32): Pointer;

    procedure RebuildIDSeedCounter;

    procedure DoAdd(p: PUInt32HashListPointerStruct);
    procedure DoInsertBefore(p, To_: PUInt32HashListPointerStruct);
    procedure DoDelete(p: PUInt32HashListPointerStruct);
    procedure DoDataFreeProc(pData: Pointer);
    procedure DoAddDataNotifyProc(pData: Pointer);
  public
    constructor Create;
    constructor CustomCreate(HashPoolSize_: Integer);
    destructor Destroy; override;
    procedure Clear;
    procedure GetListData(OutputList: TCore_List);
    function Delete(u32: UInt32): Boolean;
    function Add(u32: UInt32; Data_: Pointer; const Overwrite_: Boolean): PUInt32HashListPointerStruct;
    procedure SetValue(u32: UInt32; Data_: Pointer);
    function Insert(u32, InsertToBefore_: UInt32; Data_: Pointer; const Overwrite_: Boolean): PUInt32HashListPointerStruct;
    function Exists(u32: UInt32): Boolean;
    procedure SetHashBlockCount(HashPoolSize_: Integer);

    property FirstPtr: PUInt32HashListPointerStruct read FFirst write FFirst;
    property LastPtr: PUInt32HashListPointerStruct read FLast write FLast;

    function First: Pointer;
    function Last: Pointer;
    function GetNext(u32: UInt32): Pointer;
    function GetPrev(u32: UInt32): Pointer;
    function ListBuffer: PListBuffer;
    procedure ProgressC(const OnProgress: TUInt32HashPointerListLoop_C);
    procedure ProgressM(const OnProgress: TUInt32HashPointerListLoop_M);
    procedure ProgressP(const OnProgress: TUInt32HashPointerListLoop_P);
    //
    function ExistsPointer(pData: Pointer): Boolean;

    procedure PrintHashReport;

    property AutoFreeData: Boolean read FAutoFreeData write FAutoFreeData;
    property AccessOptimization: Boolean read FAccessOptimization write FAccessOptimization;
    property Count: NativeInt read FCount write FCount;
    property u32Val[u32: UInt32]: Pointer read Getu32Val write SetValue; default;
    property u32Data[u32: UInt32]: PUInt32HashListPointerStruct read Getu32Data;
    property OnFreePtr: TOnPtr read FOnFreePtr write FOnFreePtr;
    property OnAddPtr: TOnPtr read FOnAddPtr write FOnAddPtr;
  end;
{$ENDREGION 'TUInt32HashPointerList'}
{$REGION 'TPointerHashNativeUIntList'}

  PPointerHashListNativeUIntStruct = ^TPointerHashListNativeUIntStruct;

  TPointerHashListNativeUIntStruct = record
    qHash: THash;
    NPtr: Pointer;
    Data: NativeUInt;
    ID: TSeedCounter;
    Prev, Next: PPointerHashListNativeUIntStruct;
  end;

  TPointerHashNativeUIntListLoop_C = procedure(NPtr: Pointer; uData: NativeUInt);
  TPointerHashNativeUIntListLoop_M = procedure(NPtr: Pointer; uData: NativeUInt) of object;
{$IFDEF FPC}
  TPointerHashNativeUIntListLoop_P = procedure(NPtr: Pointer; uData: NativeUInt) is nested;
{$ELSE FPC}
  TPointerHashNativeUIntListLoop_P = reference to procedure(NPtr: Pointer; uData: NativeUInt);
{$ENDIF FPC}

  TPointerHashNativeUIntList = class(TCore_Object)
  public const
    NullValue = 0;
  private
    FListBuffer: TListBuffer;
    FCount: NativeInt;
    FIDSeed: TSeedCounter;
    FAccessOptimization: Boolean;
    FFirst: PPointerHashListNativeUIntStruct;
    FLast: PPointerHashListNativeUIntStruct;
    FTotal: UInt64;
    FMinimizePtr, FMaximumPtr: Pointer;

    function GetListTable(hash: THash; AutoCreate: Boolean): TCore_List;
    function GetNPtrData(NPtr: Pointer): PPointerHashListNativeUIntStruct;
    function GetNPtrVal(NPtr: Pointer): NativeUInt;

    procedure RebuildIDSeedCounter;

    procedure DoAdd(p: PPointerHashListNativeUIntStruct);
    procedure DoInsertBefore(p, To_: PPointerHashListNativeUIntStruct);
    procedure DoDelete(p: PPointerHashListNativeUIntStruct);
  public
    constructor Create;
    constructor CustomCreate(HashPoolSize_: Integer);
    destructor Destroy; override;
    procedure Clear;
    procedure FastClear;
    procedure GetListData(OutputList: TCore_List);
    function Delete(NPtr: Pointer): Boolean;
    function Add(NPtr: Pointer; Data_: NativeUInt; const Overwrite_: Boolean): PPointerHashListNativeUIntStruct;
    procedure SetValue(NPtr: Pointer; Data_: NativeUInt);
    function Insert(NPtr, InsertToBefore_: Pointer; Data_: NativeUInt; const Overwrite_: Boolean): PPointerHashListNativeUIntStruct;
    function Exists(NPtr: Pointer): Boolean;
    procedure SetHashBlockCount(HashPoolSize_: Integer);

    property FirstPtr: PPointerHashListNativeUIntStruct read FFirst write FFirst;
    property LastPtr: PPointerHashListNativeUIntStruct read FLast write FLast;

    function First: NativeUInt;
    function Last: NativeUInt;
    function GetNext(NPtr: Pointer): NativeUInt;
    function GetPrev(NPtr: Pointer): NativeUInt;
    function ListBuffer: PListBuffer;
    procedure ProgressC(const OnProgress: TPointerHashNativeUIntListLoop_C);
    procedure ProgressM(const OnProgress: TPointerHashNativeUIntListLoop_M);
    procedure ProgressP(const OnProgress: TPointerHashNativeUIntListLoop_P);
    //
    function ExistsNaviveUInt(Obj: NativeUInt): Boolean;

    procedure PrintHashReport;

    property Total: UInt64 read FTotal;
    property MinimizePtr: Pointer read FMinimizePtr;
    property MaximumPtr: Pointer read FMaximumPtr;
    property AccessOptimization: Boolean read FAccessOptimization write FAccessOptimization;
    property Count: NativeInt read FCount write FCount;
    property NPtrVal[NPtr: Pointer]: NativeUInt read GetNPtrVal write SetValue; default;
    property NPtrData[NPtr: Pointer]: PPointerHashListNativeUIntStruct read GetNPtrData;
  end;
{$ENDREGION 'TPointerHashNativeUIntList'}
{$REGION 'THashObjectList'}

  THashObjectChangeEvent = procedure(Sender: THashObjectList; Name: SystemString; OLD_, New_: TCore_Object) of object;

  THashObjectListData = record
    Obj: TCore_Object;
    OnChnage: THashObjectChangeEvent;
  end;

  PHashObjectListData = ^THashObjectListData;

  THashObjectListLoop_C = procedure(const Name: PSystemString; Obj: TCore_Object);
  THashObjectListLoop_M = procedure(const Name: PSystemString; Obj: TCore_Object) of object;
{$IFDEF FPC}
  THashObjectListLoop_P = procedure(const Name: PSystemString; Obj: TCore_Object) is nested;
{$ELSE FPC}
  THashObjectListLoop_P = reference to procedure(const Name: PSystemString; Obj: TCore_Object);
{$ENDIF FPC}

  THashObjectList = class(TCore_Object)
  private
    FAutoFreeObject: Boolean;
    FHashList: THashList;
    FIncremental: NativeInt;

    function GetCount: NativeInt;

    function GetIgnoreCase: Boolean;
    procedure SetIgnoreCase(const Value: Boolean);

    function GetKeyValue(const Name: SystemString): TCore_Object;
    procedure SetKeyValue(const Name: SystemString; const Value: TCore_Object);

    function GetOnChange(const Name: SystemString): THashObjectChangeEvent;
    procedure SetOnChange(const Name: SystemString; const Value_: THashObjectChangeEvent);

    function GetAccessOptimization: Boolean;
    procedure SetAccessOptimization(const Value: Boolean);

    procedure DefaultDataFreeProc(p: Pointer);
  protected
  public
    constructor Create(AutoFreeData_: Boolean);
    constructor CustomCreate(AutoFreeData_: Boolean; HashPoolSize_: Integer);
    destructor Destroy; override;

    procedure Assign(sour: THashObjectList);

    procedure ProgressC(const OnProgress: THashObjectListLoop_C);
    procedure ProgressM(const OnProgress: THashObjectListLoop_M);
    procedure ProgressP(const OnProgress: THashObjectListLoop_P);
    //
    procedure Clear;
    procedure GetNameList(OutputList: TCore_Strings); overload;
    procedure GetNameList(OutputList: TListString); overload;
    procedure GetNameList(OutputList: TListPascalString); overload;
    procedure GetListData(OutputList: TCore_Strings); overload;
    procedure GetListData(OutputList: TListString); overload;
    procedure GetListData(OutputList: TListPascalString); overload;
    procedure GetAsList(OutputList: TCore_ListForObj);
    function GetObjAsName(Obj: TCore_Object): SystemString;
    procedure Delete(const Name: SystemString);
    function Add(const Name: SystemString; Obj_: TCore_Object): TCore_Object;
    function FastAdd(const Name: SystemString; Obj_: TCore_Object): TCore_Object;
    function Find(const Name: SystemString): TCore_Object;
    function Exists(const Name: SystemString): Boolean;
    function ExistsObject(Obj: TCore_Object): Boolean;
    procedure CopyFrom(const Source: THashObjectList);
    function ReName(_OLDName, _NewName: SystemString): Boolean;
    function MakeName: SystemString;
    function MakeRefName(RefrenceName: SystemString): SystemString;

    property AccessOptimization: Boolean read GetAccessOptimization write SetAccessOptimization;
    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
    property AutoFreeObject: Boolean read FAutoFreeObject write FAutoFreeObject;
    property Count: NativeInt read GetCount;

    property KeyValue[const Name: SystemString]: TCore_Object read GetKeyValue write SetKeyValue; default;
    property NameValue[const Name: SystemString]: TCore_Object read GetKeyValue write SetKeyValue;
    property OnChange[const Name: SystemString]: THashObjectChangeEvent read GetOnChange write SetOnChange;
    property HashList: THashList read FHashList;
  end;
{$ENDREGION 'THashObjectList'}
{$REGION 'THashStringList'}

  THashStringChangeEvent = procedure(Sender: THashStringList; Name_, OLD_, New_: SystemString) of object;

  THashStringListData = record
    V: SystemString;
    OnChnage: THashStringChangeEvent;
  end;

  PHashStringListData = ^THashStringListData;

  THashStringListLoop_C = procedure(Sender: THashStringList; Name_: PSystemString; const V: SystemString);
  THashStringListLoop_M = procedure(Sender: THashStringList; Name_: PSystemString; const V: SystemString) of object;
{$IFDEF FPC}
  THashStringListLoop_P = procedure(Sender: THashStringList; Name_: PSystemString; const V: SystemString) is nested;
{$ELSE FPC}
  THashStringListLoop_P = reference to procedure(Sender: THashStringList; Name_: PSystemString; const V: SystemString);
{$ENDIF FPC}

  THashStringList = class(TCore_Object)
  private
    FHashList: THashList;
    FAutoUpdateDefaultValue: Boolean;
    FOnValueChangeNotify: THashStringChangeEvent;

    function GetCount: NativeInt;

    function GetIgnoreCase: Boolean;
    procedure SetIgnoreCase(const Value: Boolean);

    function GetKeyValue(const Name: SystemString): SystemString;
    procedure SetKeyValue(const Name: SystemString; const Value: SystemString);

    function GetOnChange(const Name: SystemString): THashStringChangeEvent;
    procedure SetOnChange(const Name: SystemString; const Value_: THashStringChangeEvent);

    function GetAccessOptimization: Boolean;
    procedure SetAccessOptimization(const Value: Boolean);

    procedure DefaultDataFreeProc(p: Pointer);
  protected
  public
    constructor Create;
    constructor CustomCreate(HashPoolSize_: Integer);
    destructor Destroy; override;
    //
    procedure Assign(sour: THashStringList);
    procedure MergeTo(dest: THashStringList);
    //
    procedure ProgressC(const OnProgress: THashStringListLoop_C);
    procedure ProgressM(const OnProgress: THashStringListLoop_M);
    procedure ProgressP(const OnProgress: THashStringListLoop_P);
    //
    function FirstName: SystemString;
    function LastName: SystemString;
    function FirstData: PHashStringListData;
    function LastData: PHashStringListData;
    //
    procedure Clear;
    //
    procedure GetNameList(OutputList: TCore_Strings); overload;
    procedure GetNameList(OutputList: TListString); overload;
    procedure GetNameList(OutputList: TListPascalString); overload;
    //
    procedure Delete(const Name: SystemString);
    function Add(const Name: SystemString; V: SystemString): SystemString;
    function FastAdd(const Name: SystemString; V: SystemString): SystemString;
    function Find(const Name: SystemString): SystemString;
    function FindValue(const Value_: SystemString): SystemString;
    function Exists(const Name: SystemString): Boolean;
    procedure CopyFrom(const Source: THashStringList);
    function IncValue(const Name: SystemString; V: SystemString): SystemString; overload;
    procedure IncValue(const vl: THashStringList); overload;

    function GetDefaultValue(const Name: SystemString; Value_: SystemString): SystemString;
    procedure SetDefaultValue(const Name: SystemString; Value_: SystemString);

    function ProcessMacro(const Text_, HeadToken, TailToken: SystemString; var Output_: SystemString): Boolean;
    function Replace(const Text_: SystemString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): SystemString; overload;
    function UReplace(const Text_: USystemString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): USystemString; overload;

    property AutoUpdateDefaultValue: Boolean read FAutoUpdateDefaultValue write FAutoUpdateDefaultValue;
    property AccessOptimization: Boolean read GetAccessOptimization write SetAccessOptimization;
    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
    property Count: NativeInt read GetCount;

    property KeyValue[const Name: SystemString]: SystemString read GetKeyValue write SetKeyValue; default;
    property NameValue[const Name: SystemString]: SystemString read GetKeyValue write SetKeyValue;

    property OnChange[const Name: SystemString]: THashStringChangeEvent read GetOnChange write SetOnChange;
    property OnValueChangeNotify: THashStringChangeEvent read FOnValueChangeNotify write FOnValueChangeNotify;
    property HashList: THashList read FHashList;

    procedure LoadFromStream(stream: TCore_Stream);
    procedure SaveToStream(stream: TCore_Stream);
    procedure LoadFromFile(FileName: SystemString);
    procedure SaveToFile(FileName: SystemString);
    procedure ExportAsStrings(Output_: TListPascalString); overload;
    procedure ExportAsStrings(Output_: TCore_Strings); overload;
    procedure ImportFromStrings(input: TListPascalString); overload;
    procedure ImportFromStrings(input: TCore_Strings); overload;
    function GetAsText: SystemString;
    procedure SetAsText(const Value: SystemString);
    property AsText: SystemString read GetAsText write SetAsText;
  end;

  THashStringTextStream = class(TCore_Object)
  private
    FStringList: THashStringList;

    function GetKeyValue(Name_: SystemString): SystemString;
    procedure SetKeyValue(Name_: SystemString; const Value: SystemString);
  public
    constructor Create(_VList: THashStringList);
    destructor Destroy; override;
    procedure Clear;

    class function VToStr(const V: SystemString): SystemString;
    class function StrToV(const S: SystemString): SystemString;

    procedure DataImport(TextList: TListPascalString); overload;
    procedure DataImport(TextList: TCore_Strings); overload;
    procedure DataExport(TextList: TListPascalString); overload;
    procedure DataExport(TextList: TCore_Strings); overload;

    procedure LoadFromStream(stream: TCore_Stream);
    procedure SaveToStream(stream: TCore_Stream);
    procedure LoadFromFile(FileName: SystemString);
    procedure SaveToFile(FileName: SystemString);

    procedure LoadFromText(Text_: SystemString);
    procedure SaveToText(var Text_: SystemString);
    function Text: SystemString;

    property StringList: THashStringList read FStringList write FStringList;
  end;

  PHashStringList = ^THashStringList;
{$ENDREGION 'THashStringList'}
{$REGION 'THashVariantList'}
  THashVariantChangeEvent = procedure(Sender: THashVariantList; Name_: SystemString; OLD_, New_: Variant) of object;

  THashVariantListData = record
    V: Variant;
    OnChnage: THashVariantChangeEvent;
  end;

  PHashVariantListData = ^THashVariantListData;

  THashVariantListLoop_C = procedure(Sender: THashVariantList; Name_: PSystemString; const V: Variant);
  THashVariantListLoop_M = procedure(Sender: THashVariantList; Name_: PSystemString; const V: Variant) of object;
{$IFDEF FPC}
  THashVariantListLoop_P = procedure(Sender: THashVariantList; Name_: PSystemString; const V: Variant) is nested;
{$ELSE FPC}
  THashVariantListLoop_P = reference to procedure(Sender: THashVariantList; Name_: PSystemString; const V: Variant);
{$ENDIF FPC}

  THashVariantList = class(TCore_Object)
  private
    FHashList: THashList;
    FAutoUpdateDefaultValue: Boolean;
    FOnValueChangeNotify: THashVariantChangeEvent;

    function GetCount: NativeInt;

    function GetIgnoreCase: Boolean;
    procedure SetIgnoreCase(const Value: Boolean);

    function GetKeyValue(const Name: SystemString): Variant;
    procedure SetKeyValue(const Name: SystemString; const Value: Variant);

    function GetOnChange(const Name: SystemString): THashVariantChangeEvent;
    procedure SetOnChange(const Name: SystemString; const Value_: THashVariantChangeEvent);

    function GetAccessOptimization: Boolean;
    procedure SetAccessOptimization(const Value: Boolean);

    procedure DefaultDataFreeProc(p: Pointer);

    function GetI64(const Name: SystemString): Int64;
    procedure SetI64(const Name: SystemString; const Value: Int64);
    function GetI32(const Name: SystemString): Integer;
    procedure SetI32(const Name: SystemString; const Value: Integer);
    function GetF(const Name: SystemString): Double;
    procedure SetF(const Name: SystemString; const Value: Double);
    function GetS(const Name: SystemString): SystemString;
    procedure SetS(const Name, Value: SystemString);
  protected
  public
    constructor Create;
    constructor CustomCreate(HashPoolSize_: Integer);
    destructor Destroy; override;
    //
    procedure Assign(sour: THashVariantList);
    //
    procedure ProgressC(const OnProgress: THashVariantListLoop_C);
    procedure ProgressM(const OnProgress: THashVariantListLoop_M);
    procedure ProgressP(const OnProgress: THashVariantListLoop_P);
    //
    function FirstName: SystemString;
    function LastName: SystemString;
    function FirstData: PHashVariantListData;
    function LastData: PHashVariantListData;
    //
    procedure Clear;
    //
    procedure GetNameList(OutputList: TCore_Strings); overload;
    procedure GetNameList(OutputList: TListString); overload;
    procedure GetNameList(OutputList: TListPascalString); overload;
    //
    procedure Delete(const Name: SystemString);
    function Add(const Name: SystemString; V: Variant): Variant;
    function FastAdd(const Name: SystemString; V: Variant): Variant;
    function Find(const Name: SystemString): Variant;
    function FindValue(const Value_: Variant): SystemString;
    function Exists(const Name: SystemString): Boolean;
    procedure CopyFrom(const Source: THashVariantList);
    function GetType(const Name: SystemString): Word;
    function IncValue(const Name: SystemString; V: Variant): Variant; overload;
    procedure IncValue(const vl: THashVariantList); overload;

    function SetMax(const Name: SystemString; V: Variant): Variant; overload;
    procedure SetMax(const vl: THashVariantList); overload;

    function SetMin(const Name: SystemString; V: Variant): Variant; overload;
    procedure SetMin(const vl: THashVariantList); overload;

    function GetDefaultValue(const Name: SystemString; Value_: Variant): Variant;
    procedure SetDefaultValue(const Name: SystemString; Value_: Variant);

    function ProcessMacro(const Text_, HeadToken, TailToken: SystemString; var Output_: SystemString): Boolean;
    function Replace(const Text_: SystemString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): SystemString;

    property AutoUpdateDefaultValue: Boolean read FAutoUpdateDefaultValue write FAutoUpdateDefaultValue;
    property AccessOptimization: Boolean read GetAccessOptimization write SetAccessOptimization;
    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
    property Count: NativeInt read GetCount;

    property i64[const Name: SystemString]: Int64 read GetI64 write SetI64;
    property i32[const Name: SystemString]: Integer read GetI32 write SetI32;
    property F[const Name: SystemString]: Double read GetF write SetF;
    property S[const Name: SystemString]: SystemString read GetS write SetS;

    property KeyValue[const Name: SystemString]: Variant read GetKeyValue write SetKeyValue; default;
    property NameValue[const Name: SystemString]: Variant read GetKeyValue write SetKeyValue;

    property OnChange[const Name: SystemString]: THashVariantChangeEvent read GetOnChange write SetOnChange;
    property OnValueChangeNotify: THashVariantChangeEvent read FOnValueChangeNotify write FOnValueChangeNotify;

    procedure LoadFromStream(stream: TCore_Stream);
    procedure SaveToStream(stream: TCore_Stream);
    procedure LoadFromFile(FileName: SystemString);
    procedure SaveToFile(FileName: SystemString);
    procedure ExportAsStrings(Output_: TListPascalString); overload;
    procedure ExportAsStrings(Output_: TCore_Strings); overload;
    procedure ImportFromStrings(input: TListPascalString); overload;
    procedure ImportFromStrings(input: TCore_Strings); overload;
    function GetAsText: SystemString;
    procedure SetAsText(const Value: SystemString);
    property AsText: SystemString read GetAsText write SetAsText;

    property HashList: THashList read FHashList;
  end;

  THashVariantTextStream = class(TCore_Object)
  private
    FVariantList: THashVariantList;

    function GetKeyValue(Name_: SystemString): Variant;
    procedure SetKeyValue(Name_: SystemString; const Value: Variant);
  public
    constructor Create(_VList: THashVariantList);
    destructor Destroy; override;
    procedure Clear;

    class function VToStr(const V: Variant): SystemString;
    class function StrToV(const S: SystemString): Variant;

    procedure DataImport(TextList: TListPascalString); overload;
    procedure DataImport(TextList: TCore_Strings); overload;
    procedure DataExport(TextList: TListPascalString); overload;
    procedure DataExport(TextList: TCore_Strings); overload;

    procedure LoadFromStream(stream: TCore_Stream);
    procedure SaveToStream(stream: TCore_Stream);
    procedure LoadFromFile(FileName: SystemString);
    procedure SaveToFile(FileName: SystemString);

    procedure LoadFromText(Text_: SystemString);
    procedure SaveToText(var Text_: SystemString); overload;
    function Text: SystemString;

    function GetValue(Name_: SystemString; V: Variant): Variant;

    property NameValue[Name_: SystemString]: Variant read GetKeyValue write SetKeyValue; default;
    property VariantList: THashVariantList read FVariantList write FVariantList;
  end;

  PHashVariantList = ^THashVariantList;
{$ENDREGION 'THashVariantList'}
{$REGION 'TListString'}

  TListStringData = record
    Data: SystemString;
    Obj: TCore_Object;
    hash: THash;
  end;

  PListStringData = ^TListStringData;

  TListString = class(TCore_Object)
  private
    FList: TCore_List;
  protected
    function GetItems(idx: Integer): SystemString;
    procedure SetItems(idx: Integer; Value: SystemString);

    function GetObjects(idx: Integer): TCore_Object;
    procedure SetObjects(idx: Integer; Value: TCore_Object);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: SystemString): Integer; overload;
    function Add(Value: SystemString; Obj: TCore_Object): Integer; overload;
    function Delete(idx: Integer): Integer;
    function DeleteString(Value: SystemString): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: SystemString): Integer;
    procedure Assign(SameObj: TListString);

    procedure LoadFromStream(stream: TCore_Stream);
    procedure SaveToStream(stream: TCore_Stream);
    procedure LoadFromFile(fn: SystemString);
    procedure SaveToFile(fn: SystemString);

    property Items[idx: Integer]: SystemString read GetItems write SetItems; default;
    property Objects[idx: Integer]: TCore_Object read GetObjects write SetObjects;
  end;
{$ENDREGION 'TListString'}
{$REGION 'TListPascalString'}

  TListPascalStringData = record
    Data: TPascalString;
    Obj: TCore_Object;
    hash: THash;
  end;

  PListPascalStringData = ^TListPascalStringData;
  TListPascalStringData_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PListPascalStringData>;

  TListPascalString = class(TCore_Object)
  private
    FList: TListPascalStringData_List;
  protected
    function GetText: SystemString;
    procedure SetText(const Value: SystemString);

    function GetItems(idx: Integer): TPascalString;
    procedure SetItems(idx: Integer; Value: TPascalString);

    function GetItems_PPascalString(idx: Integer): PPascalString;

    function GetObjects(idx: Integer): TCore_Object;
    procedure SetObjects(idx: Integer; Value: TCore_Object);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Fmt: SystemString; const Args: array of const): Integer; overload;
    function Add(Value: SystemString): Integer; overload;
    function Add(Value: TPascalString): Integer; overload;
    function Add(Value: TUPascalString): Integer; overload;
    function Add(Value: SystemString; Obj: TCore_Object): Integer; overload;
    function Add(Value: TPascalString; Obj: TCore_Object): Integer; overload;
    function Add(Value: TUPascalString; Obj: TCore_Object): Integer; overload;
    function Append(Value: SystemString): Integer; overload;
    function Delete(idx: Integer): Integer;
    function DeleteString(Value: TPascalString): Integer;
    procedure Clear;
    function Count: Integer;
    function ExistsValue(Value: TPascalString): Integer;
    procedure Exchange(const idx1, idx2: Integer);
    function ReplaceSum(Pattern: TPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): Integer;

    procedure Assign(SameObj: TListPascalString); overload;
    procedure Assign(sour: TCore_Strings); overload;
    procedure AssignTo(dest: TCore_Strings); overload;
    procedure AssignTo(dest: TListPascalString); overload;

    procedure AddStrings(sour: TListPascalString); overload;
    procedure AddStrings(sour: TCore_Strings); overload;

    procedure FillTo(var Output_: TArrayPascalString); overload;
    procedure FillFrom(const InData: TArrayPascalString);

    procedure LoadFromStream(stream: TCore_Stream);
    procedure SaveToStream(stream: TCore_Stream);
    procedure LoadFromFile(fn: SystemString);
    procedure SaveToFile(fn: SystemString);

    property AsText: SystemString read GetText write SetText;

    property Items[idx: Integer]: TPascalString read GetItems write SetItems; default;
    property Items_PPascalString[idx: Integer]: PPascalString read GetItems_PPascalString;
    property Objects[idx: Integer]: TCore_Object read GetObjects write SetObjects;

    property List: TListPascalStringData_List read FList;
  end;
{$ENDREGION 'TListPascalString'}
{$REGION 'TBackcall_Pool'}

  TBackcall_Pool = class;
  TOn_Backcall_C = procedure(Sender: TBackcall_Pool; TriggerObject: TCore_Object; Param1, Param2, Param3: Variant);
  TOn_Backcall_M = procedure(Sender: TBackcall_Pool; TriggerObject: TCore_Object; Param1, Param2, Param3: Variant) of object;

{$IFDEF FPC}
  TOn_Backcall_P = procedure(Sender: TBackcall_Pool; TriggerObject: TCore_Object; Param1, Param2, Param3: Variant) is nested;
{$ELSE FPC}
  TOn_Backcall_P = reference to procedure(Sender: TBackcall_Pool; TriggerObject: TCore_Object; Param1, Param2, Param3: Variant);
{$ENDIF FPC}
  POn_Backcall_ = ^TOn_Backcall_;

  TOn_Backcall_ = record
    Obj_: TCore_Object;
    On_C: TOn_Backcall_C;
    On_M: TOn_Backcall_M;
    On_P: TOn_Backcall_P;
    procedure Init;
  end;

  TBackcall_List_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<POn_Backcall_>;

  TBackcall_Pool = class(TCore_Object)
  private
    FList: TBackcall_List_Decl;
    FVariantList: THashVariantList;
    FObjectList: THashObjectList;
    FOwner: TCore_Object;

    function GetVariantList: THashVariantList;
    function GetObjectList: THashObjectList;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterBackcallC(Obj_: TCore_Object; On_C_: TOn_Backcall_C);
    procedure RegisterBackcallM(Obj_: TCore_Object; On_M_: TOn_Backcall_M);
    procedure RegisterBackcallP(Obj_: TCore_Object; On_P_: TOn_Backcall_P);
    procedure UnRegisterBackcall(Obj_: TCore_Object);

    procedure Clear;

    procedure ExecuteBackcall(TriggerObject: TCore_Object; Param1, Param2, Param3: Variant);

    property VariantList: THashVariantList read GetVariantList;
    property ObjectList: THashObjectList read GetObjectList;
    property Owner: TCore_Object read FOwner write FOwner;
  end;
{$ENDREGION 'TBackcall_Pool'}
{$REGION 'Generics decl'}

  TStringBigList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<SystemString>;

  TStringBigList = class(TStringBigList_Decl)
  public
    procedure DoFree(var Data: SystemString); override;
  end;

  TPascalStringBigList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<TPascalString>;

  TPascalStringBigList = class(TPascalStringBigList_Decl)
  public
    procedure DoFree(var Data: TPascalString); override;
  end;

  TUInt8List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<Byte>;
  TByteList = TUInt8List;
  TInt8List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<ShortInt>;
  TUInt16List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<Word>;
  TWordList = TUInt16List;
  TInt16List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<SmallInt>;
  TUInt32List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<Cardinal>;
  TInt32List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<Integer>;
  TUInt64List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<UInt64>;
  TInt64List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<Int64>;
  TSingleList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<Single>;
  TFloatList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<Single>;
  TDoubleList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<Double>;
  TVariantList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<Variant>;
{$ENDREGION 'Generics decl'}

function HashMod(const h: THash; const m: Integer): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
// fast hash support
function MakeHashS(const S: PSystemString): THash; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function MakeHashPas(const S: PPascalString): THash; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function MakeHashI64(const i64: Int64): THash; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function MakeHashU32(const c32: Cardinal): THash; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}
function MakeHashP(const p: Pointer): THash; {$IFDEF INLINE_ASM}inline; {$ENDIF INLINE_ASM}

procedure DoStatusL(const V: TListPascalString); overload;
procedure DoStatusL(const V: TListString); overload;

implementation

uses
{$IFDEF FPC}
  streamex,
{$ENDIF FPC}
  Z.MemoryStream, Z.Status, Z.UnicodeMixedLib, Z.Parsing, Z.Expression, Z.UReplace;

function HashMod(const h: THash; const m: Integer): Integer;
begin
  if (m > 0) and (h > 0) then
      Result := umlMax(0, umlMin(h mod m, m - 1))
  else
      Result := 0;
end;

function MakeHashS(const S: PSystemString): THash;
begin
  Result := FastHashPSystemString(S);
  Result := Get_CRC32(@Result, SizeOf(THash));
end;

function MakeHashPas(const S: PPascalString): THash;
begin
  Result := FastHashPPascalString(S);
  Result := Get_CRC32(@Result, SizeOf(THash));
end;

function MakeHashI64(const i64: Int64): THash;
begin
  Result := Get_CRC32(@i64, C_Int64_Size);
end;

function MakeHashU32(const c32: Cardinal): THash;
begin
  Result := Get_CRC32(@c32, C_Cardinal_Size);
end;

function MakeHashP(const p: Pointer): THash;
begin
  Result := Get_CRC32(@p, C_Pointer_Size);
end;

procedure DoStatusL(const V: TListPascalString);
var
  i: Integer;
  o: TCore_Object;
begin
  for i := 0 to V.Count - 1 do
    begin
      o := V.Objects[i];
      if o <> nil then
          DoStatus('%s<%s>', [V[i].Text, o.ClassName])
      else
          DoStatus(V[i].Text);
    end;
end;

procedure DoStatusL(const V: TListString);
var
  i: Integer;
  o: TCore_Object;
begin
  for i := 0 to V.Count - 1 do
    begin
      o := V.Objects[i];
      if o <> nil then
          DoStatus('%s<%s>', [V[i], o.ClassName])
      else
          DoStatus(V[i]);
    end;
end;

function THashList.GetListTable(hash: THash; AutoCreate: Boolean): TCore_List;
var
  i: Integer;
begin
  i := HashMod(hash, Length(FListBuffer));

  if (AutoCreate) and (FListBuffer[i] = nil) then
      FListBuffer[i] := TCore_List.Create;
  Result := FListBuffer[i];
end;

function THashList.GetKeyData(const Name: SystemString): PHashListData;
var
  Low_Name: SystemString;
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  pData: PHashListData;
begin
  Result := nil;
  if FIgnoreCase then
      Low_Name := LowerCase(Name)
  else
      Low_Name := Name;
  New_Hash := MakeHashS(@Low_Name);
  lst := GetListTable(New_Hash, False);
  if (lst <> nil) and (lst.Count > 0) then
    for i := lst.Count - 1 downto 0 do
      begin
        pData := PHashListData(lst[i]);
        if (New_Hash = pData^.qHash) and (Low_Name = pData^.LowerCaseName) then
          begin
            Result := pData;

            if (FAccessOptimization) and (pData^.ID < FIDSeed - 1) then
              begin
                DoDelete(pData);
                if i < lst.Count - 1 then
                  begin
                    lst.Delete(i);
                    lst.Add(pData);
                  end;
                pData^.ID := FIDSeed;
                DoAdd(pData);

                if FIDSeed > FIDSeed + 1 then
                    RebuildIDSeedCounter // rebuild seed
                else
                    inc(FIDSeed);
              end;

            Exit;
          end;
      end;
end;

function THashList.GetKeyValue(const Name: SystemString): Pointer;
var
  p: PHashListData;
begin
  p := GetKeyData(Name);
  if p <> nil then
      Result := p^.Data
  else
      Result := nil;
end;

procedure THashList.RebuildIDSeedCounter;
var
  i: Integer;
  p: PHashListData;
begin
  i := 0;
  p := FFirst;
  while i < FCount do
    begin
      p^.ID := i + 1;
      inc(i);
      p := p^.Next;
    end;

  FIDSeed := i + 1;
end;

procedure THashList.DoAdd(p: PHashListData);
begin
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
end;

procedure THashList.DoInsertBefore(p, To_: PHashListData);
var
  P_P: PHashListData;
begin
  if p = To_ then
      Exit;
  if FFirst = To_ then
      FFirst := p;
  P_P := To_^.Prev;
  if P_P^.Next = To_ then
      P_P^.Next := p;
  if To_^.Next = To_ then
      To_^.Next := p;
  To_^.Prev := p;
  p^.Prev := P_P;
  p^.Next := To_;
end;

procedure THashList.DoDelete(p: PHashListData);
var
  P_P, N_P: PHashListData;
begin
  P_P := p^.Prev;
  N_P := p^.Next;

  if p = FFirst then
      FFirst := N_P;
  if p = FLast then
      FLast := P_P;

  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
    end
  else
    begin
      P_P^.Next := N_P;
      N_P^.Prev := P_P;
      p^.Prev := nil;
      p^.Next := nil;
    end;
end;

procedure THashList.DefaultDataFreeProc(p: Pointer);
begin
{$IFDEF FPC}
{$ELSE}
  Dispose(p);
{$ENDIF}
end;

procedure THashList.DoDataFreeProc(p: Pointer);
begin
  if p <> nil then
      FOnFreePtr(p);
end;

constructor THashList.Create;
begin
  CustomCreate(64);
end;

constructor THashList.CustomCreate(HashPoolSize_: Integer);
begin
  inherited Create;
  FCount := 0;
  FIDSeed := 0;
  FAutoFreeData := False;
  FIgnoreCase := True;
  FAccessOptimization := False;

  FOnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FFirst := nil;
  FLast := nil;
  FMaxNameSize := -1;
  FMinNameSize := -1;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(HashPoolSize_);
end;

destructor THashList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure THashList.Clear;
var
  i: Integer;
  j: Integer;
  lst: TCore_List;
  pData: PHashListData;
begin
  FCount := 0;
  FIDSeed := 0;
  FFirst := nil;
  FLast := nil;
  FMaxNameSize := -1;
  FMinNameSize := -1;
  if Length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for j := lst.Count - 1 downto 0 do
                begin
                  pData := lst[j];
                  try
                    if (FAutoFreeData) and (pData^.Data <> nil) then
                        DoDataFreeProc(pData^.Data);
                    Dispose(pData);
                  except
                  end;
                end;
            end;
          DisposeObject(lst);
          FListBuffer[i] := nil;
        end;
    end;
end;

procedure THashList.MergeTo(dest: THashList);
var
  i: Integer;
  p: PHashListData;
begin
  if FCount > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          dest.Add(p^.OriginName, p^.Data, True);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashList.GetNameList(var Output_: TArrayPascalString);
var
  i: Integer;
  p: PHashListData;
begin
  SetLength(Output_, Count);
  if FCount > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          Output_[i] := p^.OriginName;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashList.GetNameList(OutputList: TListString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList.Add(p^.OriginName);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashList.GetNameList(OutputList: TListPascalString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList.Add(p^.OriginName);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashList.GetNameList(OutputList: TCore_Strings);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList.Add(p^.OriginName);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashList.GetListData(OutputList: TCore_List);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Count := FCount;
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList[i] := p;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function THashList.GetHashDataArray(): THashDataArray;
var
  i: Integer;
  p: PHashListData;
begin
  SetLength(Result, FCount);
  if FCount > 0 then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          Result[i] := p;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashList.Delete(const Name: SystemString);
var
  New_Hash: THash;
  i: Integer;
  Low_Name: SystemString;
  lst: TCore_List;
  _ItemData: PHashListData;
begin
  if FCount = 0 then
      Exit;
  if FIgnoreCase then
      Low_Name := LowerCase(Name)
  else
      Low_Name := Name;
  New_Hash := MakeHashS(@Low_Name);
  lst := GetListTable(New_Hash, False);
  if lst <> nil then
    begin
      i := 0;
      while i < lst.Count do
        begin
          _ItemData := lst[i];
          if (New_Hash = _ItemData^.qHash) and (Low_Name = _ItemData^.LowerCaseName) then
            begin
              DoDelete(_ItemData);
              if (FAutoFreeData) and (_ItemData^.Data <> nil) then
                begin
                  try
                    DoDataFreeProc(_ItemData^.Data);
                    _ItemData^.Data := nil;
                  except
                  end;
                end;
              Dispose(_ItemData);
              lst.Delete(i);
              dec(FCount);
            end
          else
              inc(i);
        end;
    end;

  if FCount = 0 then
    begin
      FIDSeed := 1;
      FMaxNameSize := -1;
      FMinNameSize := -1;
    end;
end;

function THashList.Add(const Name: SystemString; Data_: Pointer; const Overwrite_: Boolean): PHashListData;
var
  New_Hash: THash;
  L: NativeInt;
  lst: TCore_List;
  i: Integer;
  Low_Name: SystemString;
  pData: PHashListData;
begin
  if FIgnoreCase then
      Low_Name := LowerCase(Name)
  else
      Low_Name := Name;
  New_Hash := MakeHashS(@Low_Name);

  L := Length(Low_Name);
  if Count > 0 then
    begin
      if L > FMaxNameSize then
          FMaxNameSize := L;
      if L < FMinNameSize then
          FMinNameSize := L;
    end
  else
    begin
      FMaxNameSize := L;
      FMinNameSize := L;
    end;

  lst := GetListTable(New_Hash, True);
  if (lst.Count > 0) and (Overwrite_) then
    begin
      for i := 0 to lst.Count - 1 do
        begin
          pData := PHashListData(lst[i]);
          if (New_Hash = pData^.qHash) and (Low_Name = pData^.LowerCaseName) then
            begin
              DoDelete(pData);
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := Data_;
              Result := pData;

              DoAdd(pData);

              if (pData^.ID < FIDSeed - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDSeed;

                  if FIDSeed > FIDSeed + 1 then
                      RebuildIDSeedCounter // rebuild seed
                  else
                      inc(FIDSeed);
                end;

              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := New_Hash;
  pData^.LowerCaseName := Low_Name;
  pData^.OriginName := Name;
  pData^.Data := Data_;
  pData^.ID := FIDSeed;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  inc(FCount);
  DoAdd(pData);

  if FIDSeed > FIDSeed + 1 then
      RebuildIDSeedCounter // rebuild seed
  else
      inc(FIDSeed);
end;

procedure THashList.Add(const Name: SystemString; Data_: Pointer);
begin
  Add(Name, Data_, True);
end;

procedure THashList.SetValue(const Name: SystemString; const Data_: Pointer);
var
  New_Hash: THash;
  L: NativeInt;
  lst: TCore_List;
  i: Integer;
  Low_Name: SystemString;
  pData: PHashListData;
  Done: Boolean;
begin
  if FIgnoreCase then
      Low_Name := LowerCase(Name)
  else
      Low_Name := Name;
  New_Hash := MakeHashS(@Low_Name);

  L := Length(Low_Name);
  if Count > 0 then
    begin
      if L > FMaxNameSize then
          FMaxNameSize := L;
      if L < FMinNameSize then
          FMinNameSize := L;
    end
  else
    begin
      FMaxNameSize := L;
      FMinNameSize := L;
    end;

  lst := GetListTable(New_Hash, True);
  Done := False;
  if (lst.Count > 0) then
    for i := 0 to lst.Count - 1 do
      begin
        pData := PHashListData(lst[i]);
        if (New_Hash = pData^.qHash) and (Low_Name = pData^.LowerCaseName) then
          begin
            if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
              begin
                try
                    DoDataFreeProc(pData^.Data);
                except
                end;
              end;
            pData^.Data := Data_;
            Done := True;
          end;
      end;

  if not Done then
    begin
      new(pData);
      pData^.qHash := New_Hash;
      pData^.LowerCaseName := Low_Name;
      pData^.OriginName := Name;
      pData^.Data := Data_;
      pData^.ID := FIDSeed;
      pData^.Prev := nil;
      pData^.Next := nil;
      lst.Add(pData);
      inc(FCount);

      DoAdd(pData);

      if FIDSeed > FIDSeed + 1 then
          RebuildIDSeedCounter // rebuild seed
      else
          inc(FIDSeed);
    end;
end;

function THashList.Insert(Name, InsertToBefore_: SystemString; Data_: Pointer; const Overwrite_: Boolean): PHashListData;
var
  New_Hash: THash;
  L: NativeInt;
  lst: TCore_List;
  i: Integer;
  Low_Name: SystemString;
  InsertDest_, pData: PHashListData;
begin
  InsertDest_ := NameData[InsertToBefore_];
  if InsertDest_ = nil then
    begin
      Result := Add(Name, Data_, Overwrite_);
      Exit;
    end;

  if FIgnoreCase then
      Low_Name := LowerCase(Name)
  else
      Low_Name := Name;
  New_Hash := MakeHashS(@Low_Name);

  L := Length(Low_Name);
  if Count > 0 then
    begin
      if L > FMaxNameSize then
          FMaxNameSize := L;
      if L < FMinNameSize then
          FMinNameSize := L;
    end
  else
    begin
      FMaxNameSize := L;
      FMinNameSize := L;
    end;

  lst := GetListTable(New_Hash, True);
  if (lst.Count > 0) and (Overwrite_) then
    begin
      for i := 0 to lst.Count - 1 do
        begin
          pData := PHashListData(lst[i]);
          if (New_Hash = pData^.qHash) and (Low_Name = pData^.LowerCaseName) then
            begin
              DoDelete(pData);
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := Data_;
              Result := pData;

              DoInsertBefore(pData, InsertDest_);

              if (pData^.ID < FIDSeed - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDSeed;

                  if FIDSeed > FIDSeed + 1 then
                      RebuildIDSeedCounter // rebuild seed
                  else
                      inc(FIDSeed);
                end;

              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := New_Hash;
  pData^.LowerCaseName := Low_Name;
  pData^.OriginName := Name;
  pData^.Data := Data_;
  pData^.ID := FIDSeed;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  inc(FCount);
  DoInsertBefore(pData, InsertDest_);

  if FIDSeed > FIDSeed + 1 then
      RebuildIDSeedCounter // rebuild seed
  else
      inc(FIDSeed);
end;

function THashList.Find(const Name: SystemString): Pointer;
var
  i: Integer;
  j: Integer;
  lst: TCore_List;
  pData: PHashListData;
begin
  Result := nil;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for j := lst.Count - 1 downto 0 do
                begin
                  pData := PHashListData(lst[j]);
                  if (umlMultipleMatch(True, Name, pData^.OriginName)) then
                    begin
                      Result := pData^.Data;
                      Exit;
                    end;
                end;
            end;
        end;
    end;
end;

function THashList.Exists(const Name: SystemString): Boolean;
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  pData: PHashListData;
  Low_Name: SystemString;
begin
  Result := False;
  if FCount = 0 then
      Exit;
  if FIgnoreCase then
      Low_Name := LowerCase(Name)
  else
      Low_Name := Name;
  New_Hash := MakeHashS(@Low_Name);
  lst := GetListTable(New_Hash, False);
  if lst <> nil then
    begin
      if lst.Count > 0 then
        for i := lst.Count - 1 downto 0 do
          begin
            pData := PHashListData(lst[i]);
            if (New_Hash = pData^.qHash) and (Low_Name = pData^.LowerCaseName) then
                Exit(True);
          end;
    end;
end;

procedure THashList.SetHashBlockCount(HashPoolSize_: Integer);
var
  i: Integer;
begin
  Clear;
  SetLength(FListBuffer, HashPoolSize_);
  for i := low(FListBuffer) to high(FListBuffer) do
      FListBuffer[i] := nil;
end;

function THashList.First: Pointer;
begin
  if FFirst <> nil then
      Result := FFirst^.Data
  else
      Result := nil;
end;

function THashList.Last: Pointer;
begin
  if FLast <> nil then
      Result := FLast^.Data
  else
      Result := nil;
end;

function THashList.GetNext(const Name: SystemString): Pointer;
var
  p: PHashListData;
begin
  Result := nil;
  p := GetKeyData(Name);
  if (p = nil) or (p = FLast) or (p^.Next = p) then
      Exit;
  Result := p^.Next^.Data;
end;

function THashList.GetPrev(const Name: SystemString): Pointer;
var
  p: PHashListData;
begin
  Result := nil;
  p := GetKeyData(Name);
  if (p = nil) or (p = FFirst) or (p^.Prev = p) then
      Exit;
  Result := p^.Prev^.Data;
end;

function THashList.ListBuffer: PListBuffer;
begin
  Result := @FListBuffer;
end;

procedure THashList.ProgressC(const OnProgress: THashListLoop_C);
var
  i: NativeInt;
  p: PHashListData;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(@p^.OriginName, p);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashList.ProgressM(const OnProgress: THashListLoop_M);
var
  i: NativeInt;
  p: PHashListData;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(@p^.OriginName, p);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashList.ProgressP(const OnProgress: THashListLoop_P);
var
  i: NativeInt;
  p: PHashListData;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(@p^.OriginName, p);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashList.PrintHashReport;
var
  i: NativeInt;
  L: TCore_List;
  Total: NativeInt;
  usaged, aMax, aMin: NativeInt;
  inited: Boolean;
begin
  inited := False;
  usaged := 0;
  aMax := 0;
  aMin := 0;
  Total := 0;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      L := FListBuffer[i];
      if L <> nil then
        begin
          inc(usaged);
          Total := Total + L.Count;
          if inited then
            begin
              if L.Count > aMax then
                  aMax := L.Count;
              if aMin > L.Count then
                  aMin := L.Count;
            end
          else
            begin
              aMax := L.Count;
              aMin := L.Count;
              inited := True;
            end;
        end;
    end;
  DoStatus(Format('usaged container:%d item total:%d Max:%d min:%d', [usaged, Total, aMax, aMin]));
end;

function TInt64HashObjectList.GetListTable(hash: THash; AutoCreate: Boolean): TCore_List;
var
  i: Integer;
begin
  i := HashMod(hash, Length(FListBuffer));

  if (AutoCreate) and (FListBuffer[i] = nil) then
      FListBuffer[i] := TCore_List.Create;
  Result := FListBuffer[i];
end;

function TInt64HashObjectList.Geti64Data(i64: Int64): PInt64HashListObjectStruct;
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  pData: PInt64HashListObjectStruct;
begin
  Result := nil;
  New_Hash := MakeHashI64(i64);
  lst := GetListTable(New_Hash, False);
  if (lst <> nil) and (lst.Count > 0) then
    for i := lst.Count - 1 downto 0 do
      begin
        pData := PInt64HashListObjectStruct(lst[i]);
        if (New_Hash = pData^.qHash) and (i64 = pData^.i64) then
          begin
            Result := pData;
            if (FAccessOptimization) and (pData^.ID < FIDSeed - 1) then
              begin
                DoDelete(pData);
                if i < lst.Count - 1 then
                  begin
                    lst.Delete(i);
                    lst.Add(pData);
                  end;
                pData^.ID := FIDSeed;
                DoAdd(pData);

                if FIDSeed > FIDSeed + 1 then
                    RebuildIDSeedCounter // rebuild seed
                else
                    inc(FIDSeed);
              end;
            Exit;
          end;
      end;
end;

function TInt64HashObjectList.Geti64Val(i64: Int64): TCore_Object;
var
  p: PInt64HashListObjectStruct;
begin
  p := Geti64Data(i64);
  if p <> nil then
      Result := p^.Data
  else
      Result := nil;
end;

procedure TInt64HashObjectList.RebuildIDSeedCounter;
var
  i: Integer;
  p: PInt64HashListObjectStruct;
begin
  i := 0;
  p := FFirst;
  while i < FCount do
    begin
      p^.ID := i + 1;
      inc(i);
      p := p^.Next;
    end;

  FIDSeed := i + 1;
end;

procedure TInt64HashObjectList.DoAdd(p: PInt64HashListObjectStruct);
begin
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
end;

procedure TInt64HashObjectList.DoInsertBefore(p, To_: PInt64HashListObjectStruct);
var
  P_P: PInt64HashListObjectStruct;
begin
  if p = To_ then
      Exit;
  if FFirst = To_ then
      FFirst := p;
  P_P := To_^.Prev;
  if P_P^.Next = To_ then
      P_P^.Next := p;
  if To_^.Next = To_ then
      To_^.Next := p;
  To_^.Prev := p;
  p^.Prev := P_P;
  p^.Next := To_;
end;

procedure TInt64HashObjectList.DoDelete(p: PInt64HashListObjectStruct);
var
  P_P, N_P: PInt64HashListObjectStruct;
begin
  P_P := p^.Prev;
  N_P := p^.Next;

  if p = FFirst then
      FFirst := N_P;
  if p = FLast then
      FLast := P_P;

  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
    end
  else
    begin
      P_P^.Next := N_P;
      N_P^.Prev := P_P;
      p^.Prev := nil;
      p^.Next := nil;
    end;
end;

procedure TInt64HashObjectList.DefaultObjectFreeProc(Obj: TCore_Object);
begin
  DisposeObject(Obj);
end;

procedure TInt64HashObjectList.DoDataFreeProc(Obj: TCore_Object);
begin
  if Obj <> nil then
      FOnObjectFreeProc(Obj);
end;

constructor TInt64HashObjectList.Create;
begin
  CustomCreate(256);
end;

constructor TInt64HashObjectList.CustomCreate(HashPoolSize_: Integer);
begin
  inherited Create;
  FCount := 0;
  FIDSeed := 0;
  FAccessOptimization := False;
  FAutoFreeData := False;
  FOnObjectFreeProc := {$IFDEF FPC}@{$ENDIF FPC}DefaultObjectFreeProc;
  FFirst := nil;
  FLast := nil;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(HashPoolSize_);
end;

destructor TInt64HashObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TInt64HashObjectList.Clear;
var
  i: Integer;
  j: Integer;
  lst: TCore_List;
  pData: PInt64HashListObjectStruct;
begin
  FCount := 0;
  FIDSeed := 0;
  FFirst := nil;
  FLast := nil;

  if Length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for j := lst.Count - 1 downto 0 do
                begin
                  pData := lst[j];
                  try
                    if (FAutoFreeData) and (pData^.Data <> nil) then
                        DoDataFreeProc(pData^.Data);
                    Dispose(pData);
                  except
                  end;
                end;
            end;
          DisposeObject(lst);
          FListBuffer[i] := nil;
        end;
    end;
end;

procedure TInt64HashObjectList.GetListData(OutputList: TCore_List);
var
  i: Integer;
  p: PInt64HashListObjectStruct;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Count := FCount;
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList[i] := p;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TInt64HashObjectList.Delete(i64: Int64);
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  _ItemData: PInt64HashListObjectStruct;
begin
  if FCount = 0 then
      Exit;
  New_Hash := MakeHashI64(i64);
  lst := GetListTable(New_Hash, False);
  if lst <> nil then
    begin
      i := 0;
      while i < lst.Count do
        begin
          _ItemData := lst[i];
          if (New_Hash = _ItemData^.qHash) and (i64 = _ItemData^.i64) then
            begin
              DoDelete(_ItemData);
              if (FAutoFreeData) and (_ItemData^.Data <> nil) then
                begin
                  try
                    DoDataFreeProc(_ItemData^.Data);
                    _ItemData^.Data := nil;
                  except
                  end;
                end;
              Dispose(_ItemData);
              lst.Delete(i);
              dec(FCount);
            end
          else
              inc(i);
        end;
    end;

  if FCount = 0 then
      FIDSeed := 1;
end;

function TInt64HashObjectList.Add(i64: Int64; Data_: TCore_Object; const Overwrite_: Boolean): PInt64HashListObjectStruct;
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  pData: PInt64HashListObjectStruct;
begin
  New_Hash := MakeHashI64(i64);

  lst := GetListTable(New_Hash, True);
  if (lst.Count > 0) and (Overwrite_) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PInt64HashListObjectStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (i64 = pData^.i64) then
            begin
              DoDelete(pData);
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := Data_;
              Result := pData;

              DoAdd(pData);

              if (pData^.ID < FIDSeed - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDSeed;

                  if FIDSeed > FIDSeed + 1 then
                      RebuildIDSeedCounter // rebuild seed
                  else
                      inc(FIDSeed);
                end;

              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := New_Hash;
  pData^.i64 := i64;
  pData^.Data := Data_;
  pData^.ID := FIDSeed;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  inc(FCount);
  DoAdd(pData);

  if FIDSeed > FIDSeed + 1 then
      RebuildIDSeedCounter // rebuild seed
  else
      inc(FIDSeed);
end;

procedure TInt64HashObjectList.SetValue(i64: Int64; Data_: TCore_Object);
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  pData: PInt64HashListObjectStruct;
  Done: Boolean;
begin
  New_Hash := MakeHashI64(i64);

  lst := GetListTable(New_Hash, True);
  Done := False;
  if (lst.Count > 0) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PInt64HashListObjectStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (i64 = pData^.i64) then
            begin
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := Data_;
              Done := True;
            end;
        end;
    end;

  if not Done then
    begin
      new(pData);
      pData^.qHash := New_Hash;
      pData^.i64 := i64;
      pData^.Data := Data_;
      pData^.ID := FIDSeed;
      pData^.Prev := nil;
      pData^.Next := nil;
      lst.Add(pData);
      inc(FCount);
      DoAdd(pData);

      if FIDSeed > FIDSeed + 1 then
          RebuildIDSeedCounter // rebuild seed
      else
          inc(FIDSeed);
    end;
end;

function TInt64HashObjectList.Insert(i64, InsertToBefore_: Int64; Data_: TCore_Object; const Overwrite_: Boolean): PInt64HashListObjectStruct;
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  InsertDest_, pData: PInt64HashListObjectStruct;
begin
  InsertDest_ := i64Data[InsertToBefore_];
  if InsertDest_ = nil then
    begin
      Result := Add(i64, Data_, Overwrite_);
      Exit;
    end;

  New_Hash := MakeHashI64(i64);

  lst := GetListTable(New_Hash, True);
  if (lst.Count > 0) and (Overwrite_) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PInt64HashListObjectStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (i64 = pData^.i64) then
            begin
              DoDelete(pData);
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := Data_;
              Result := pData;

              DoInsertBefore(pData, InsertDest_);

              if (pData^.ID < FIDSeed - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDSeed;

                  if FIDSeed > FIDSeed + 1 then
                      RebuildIDSeedCounter // rebuild seed
                  else
                      inc(FIDSeed);
                end;

              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := New_Hash;
  pData^.i64 := i64;
  pData^.Data := Data_;
  pData^.ID := FIDSeed;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  inc(FCount);
  DoInsertBefore(pData, InsertDest_);

  if FIDSeed > FIDSeed + 1 then
      RebuildIDSeedCounter // rebuild seed
  else
      inc(FIDSeed);
end;

function TInt64HashObjectList.Exists(i64: Int64): Boolean;
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  pData: PInt64HashListObjectStruct;
begin
  Result := False;
  if FCount = 0 then
      Exit;
  New_Hash := MakeHashI64(i64);
  lst := GetListTable(New_Hash, False);
  if lst <> nil then
    begin
      if lst.Count > 0 then
        for i := lst.Count - 1 downto 0 do
          begin
            pData := PInt64HashListObjectStruct(lst[i]);
            if (New_Hash = pData^.qHash) and (i64 = pData^.i64) then
                Exit(True);
          end;
    end;
end;

procedure TInt64HashObjectList.SetHashBlockCount(HashPoolSize_: Integer);
var
  i: Integer;
begin
  Clear;
  SetLength(FListBuffer, HashPoolSize_);
  for i := low(FListBuffer) to high(FListBuffer) do
      FListBuffer[i] := nil;
end;

procedure TInt64HashObjectList.DeleteFirst;
begin
  if FFirst <> nil then
      Delete(FFirst^.i64);
end;

procedure TInt64HashObjectList.DeleteLast;
begin
  if FLast <> nil then
      Delete(FLast^.i64);
end;

function TInt64HashObjectList.First: TCore_Object;
begin
  if FFirst <> nil then
      Result := FFirst^.Data
  else
      Result := nil;
end;

function TInt64HashObjectList.Last: TCore_Object;
begin
  if FLast <> nil then
      Result := FLast^.Data
  else
      Result := nil;
end;

function TInt64HashObjectList.GetNext(i64: Int64): TCore_Object;
var
  p: PInt64HashListObjectStruct;
begin
  Result := nil;
  p := Geti64Data(i64);
  if (p = nil) or (p = FLast) or (p^.Next = p) then
      Exit;
  Result := p^.Next^.Data;
end;

function TInt64HashObjectList.GetPrev(i64: Int64): TCore_Object;
var
  p: PInt64HashListObjectStruct;
begin
  Result := nil;
  p := Geti64Data(i64);
  if (p = nil) or (p = FFirst) or (p^.Prev = p) then
      Exit;
  Result := p^.Prev^.Data;
end;

function TInt64HashObjectList.ListBuffer: PListBuffer;
begin
  Result := @FListBuffer;
end;

procedure TInt64HashObjectList.ProgressC(const OnProgress: TInt64HashObjectListLoop_C);
var
  i: NativeInt;
  p: PInt64HashListObjectStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.i64, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TInt64HashObjectList.ProgressM(const OnProgress: TInt64HashObjectListLoop_M);
var
  i: NativeInt;
  p: PInt64HashListObjectStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.i64, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TInt64HashObjectList.ProgressP(const OnProgress: TInt64HashObjectListLoop_P);
var
  i: NativeInt;
  p: PInt64HashListObjectStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.i64, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TInt64HashObjectList.PrintHashReport;
var
  i: NativeInt;
  L: TCore_List;
  Total: NativeInt;
  usaged, aMax, aMin: NativeInt;
  inited: Boolean;
begin
  inited := False;
  usaged := 0;
  aMax := 0;
  aMin := 0;
  Total := 0;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      L := FListBuffer[i];
      if L <> nil then
        begin
          inc(usaged);
          Total := Total + L.Count;
          if inited then
            begin
              if L.Count > aMax then
                  aMax := L.Count;
              if aMin > L.Count then
                  aMin := L.Count;
            end
          else
            begin
              aMax := L.Count;
              aMin := L.Count;
              inited := True;
            end;
        end;
    end;
  DoStatus(Format('usaged container:%d item total:%d Max:%d min:%d', [usaged, Total, aMax, aMin]));
end;

function TInt64HashPointerList.GetListTable(hash: THash; AutoCreate: Boolean): TCore_List;
var
  i: Integer;
begin
  i := HashMod(hash, Length(FListBuffer));

  if (AutoCreate) and (FListBuffer[i] = nil) then
      FListBuffer[i] := TCore_List.Create;
  Result := FListBuffer[i];
end;

function TInt64HashPointerList.Geti64Data(i64: Int64): PInt64HashListPointerStruct;
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  pData: PInt64HashListPointerStruct;
begin
  Result := nil;
  New_Hash := MakeHashI64(i64);
  lst := GetListTable(New_Hash, False);
  if (lst <> nil) and (lst.Count > 0) then
    for i := lst.Count - 1 downto 0 do
      begin
        pData := PInt64HashListPointerStruct(lst[i]);
        if (New_Hash = pData^.qHash) and (i64 = pData^.i64) then
          begin
            Result := pData;
            if (FAccessOptimization) and (pData^.ID < FIDSeed - 1) then
              begin
                DoDelete(pData);
                if i < lst.Count - 1 then
                  begin
                    lst.Delete(i);
                    lst.Add(pData);
                  end;
                pData^.ID := FIDSeed;
                DoAdd(pData);

                if FIDSeed > FIDSeed + 1 then
                    RebuildIDSeedCounter // rebuild seed
                else
                    inc(FIDSeed);
              end;
            Exit;
          end;
      end;
end;

function TInt64HashPointerList.Geti64Val(i64: Int64): Pointer;
var
  p: PInt64HashListPointerStruct;
begin
  p := Geti64Data(i64);
  if p <> nil then
      Result := p^.Data
  else
      Result := nil;
end;

procedure TInt64HashPointerList.RebuildIDSeedCounter;
var
  i: Integer;
  p: PInt64HashListPointerStruct;
begin
  i := 0;
  p := FFirst;
  while i < FCount do
    begin
      p^.ID := i + 1;
      inc(i);
      p := p^.Next;
    end;

  FIDSeed := i + 1;
end;

procedure TInt64HashPointerList.DoAdd(p: PInt64HashListPointerStruct);
begin
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
end;

procedure TInt64HashPointerList.DoInsertBefore(p, To_: PInt64HashListPointerStruct);
var
  P_P: PInt64HashListPointerStruct;
begin
  if p = To_ then
      Exit;
  if FFirst = To_ then
      FFirst := p;
  P_P := To_^.Prev;
  if P_P^.Next = To_ then
      P_P^.Next := p;
  if To_^.Next = To_ then
      To_^.Next := p;
  To_^.Prev := p;
  p^.Prev := P_P;
  p^.Next := To_;
end;

procedure TInt64HashPointerList.DoDelete(p: PInt64HashListPointerStruct);
var
  P_P, N_P: PInt64HashListPointerStruct;
begin
  P_P := p^.Prev;
  N_P := p^.Next;

  if p = FFirst then
      FFirst := N_P;
  if p = FLast then
      FLast := P_P;

  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
    end
  else
    begin
      P_P^.Next := N_P;
      N_P^.Prev := P_P;
      p^.Prev := nil;
      p^.Next := nil;
    end;
end;

procedure TInt64HashPointerList.DefaultDataFreeProc(p: Pointer);
begin
{$IFDEF FPC}
{$ELSE}
  Dispose(p);
{$ENDIF}
end;

procedure TInt64HashPointerList.DoDataFreeProc(p: Pointer);
begin
  if p <> nil then
      FOnFreePtr(p);
end;

procedure TInt64HashPointerList.DoAddDataNotifyProc(p: Pointer);
begin
  if Assigned(FOnAddPtr) then
      FOnAddPtr(p);
end;

constructor TInt64HashPointerList.Create;
begin
  CustomCreate(256);
end;

constructor TInt64HashPointerList.CustomCreate(HashPoolSize_: Integer);
begin
  inherited Create;
  FCount := 0;
  FIDSeed := 0;
  FAccessOptimization := False;
  FAutoFreeData := False;
  FFirst := nil;
  FLast := nil;
  FOnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FOnAddPtr := nil;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(HashPoolSize_);
end;

destructor TInt64HashPointerList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TInt64HashPointerList.Clear;
var
  i: Integer;
  j: Integer;
  lst: TCore_List;
  pData: PInt64HashListPointerStruct;
begin
  FCount := 0;
  FIDSeed := 0;
  FFirst := nil;
  FLast := nil;

  if Length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for j := lst.Count - 1 downto 0 do
                begin
                  pData := lst[j];
                  try
                    if (FAutoFreeData) and (pData^.Data <> nil) then
                        DoDataFreeProc(pData^.Data);
                    Dispose(pData);
                  except
                  end;
                end;
            end;
          DisposeObject(lst);
          FListBuffer[i] := nil;
        end;
    end;
end;

procedure TInt64HashPointerList.GetListData(OutputList: TCore_List);
var
  i: Integer;
  p: PInt64HashListPointerStruct;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Count := FCount;
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList[i] := p;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TInt64HashPointerList.Delete(i64: Int64);
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  _ItemData: PInt64HashListPointerStruct;
begin
  if FCount = 0 then
      Exit;
  New_Hash := MakeHashI64(i64);
  lst := GetListTable(New_Hash, False);
  if lst <> nil then
    begin
      i := 0;
      while i < lst.Count do
        begin
          _ItemData := lst[i];
          if (New_Hash = _ItemData^.qHash) and (i64 = _ItemData^.i64) then
            begin
              DoDelete(_ItemData);
              if (FAutoFreeData) and (_ItemData^.Data <> nil) then
                begin
                  try
                    DoDataFreeProc(_ItemData^.Data);
                    _ItemData^.Data := nil;
                  except
                  end;
                end;
              Dispose(_ItemData);
              lst.Delete(i);
              dec(FCount);
            end
          else
              inc(i);
        end;
    end;

  if FCount = 0 then
      FIDSeed := 1;
end;

function TInt64HashPointerList.Add(i64: Int64; Data_: Pointer; const Overwrite_: Boolean): PInt64HashListPointerStruct;
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  pData: PInt64HashListPointerStruct;
begin
  New_Hash := MakeHashI64(i64);

  lst := GetListTable(New_Hash, True);
  if (lst.Count > 0) and (Overwrite_) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PInt64HashListPointerStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (i64 = pData^.i64) then
            begin
              DoDelete(pData);
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := Data_;
              Result := pData;

              DoAdd(pData);

              if (pData^.ID < FIDSeed - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDSeed;

                  if FIDSeed > FIDSeed + 1 then
                      RebuildIDSeedCounter // rebuild seed
                  else
                      inc(FIDSeed);
                end;

              DoAddDataNotifyProc(Data_);

              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := New_Hash;
  pData^.i64 := i64;
  pData^.Data := Data_;
  pData^.ID := FIDSeed;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  inc(FCount);
  DoAdd(pData);

  if FIDSeed > FIDSeed + 1 then
      RebuildIDSeedCounter // rebuild seed
  else
      inc(FIDSeed);

  DoAddDataNotifyProc(Data_);
end;

procedure TInt64HashPointerList.SetValue(i64: Int64; Data_: Pointer);
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  pData: PInt64HashListPointerStruct;
  Done: Boolean;
begin
  New_Hash := MakeHashI64(i64);

  lst := GetListTable(New_Hash, True);
  Done := False;
  if (lst.Count > 0) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PInt64HashListPointerStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (i64 = pData^.i64) then
            begin
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := Data_;
              Done := True;
              DoAddDataNotifyProc(pData^.Data);
            end;
        end;
    end;

  if not Done then
    begin
      new(pData);
      pData^.qHash := New_Hash;
      pData^.i64 := i64;
      pData^.Data := Data_;
      pData^.ID := FIDSeed;
      pData^.Prev := nil;
      pData^.Next := nil;
      lst.Add(pData);
      inc(FCount);
      DoAdd(pData);

      if FIDSeed > FIDSeed + 1 then
          RebuildIDSeedCounter // rebuild seed
      else
          inc(FIDSeed);

      DoAddDataNotifyProc(Data_);
    end;
end;

function TInt64HashPointerList.Insert(i64, InsertToBefore_: Int64; Data_: Pointer; const Overwrite_: Boolean): PInt64HashListPointerStruct;
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  InsertDest_, pData: PInt64HashListPointerStruct;
begin
  InsertDest_ := i64Data[InsertToBefore_];
  if InsertDest_ = nil then
    begin
      Result := Add(i64, Data_, Overwrite_);
      Exit;
    end;

  New_Hash := MakeHashI64(i64);

  lst := GetListTable(New_Hash, True);
  if (lst.Count > 0) and (Overwrite_) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PInt64HashListPointerStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (i64 = pData^.i64) then
            begin
              DoDelete(pData);
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := Data_;
              Result := pData;

              DoInsertBefore(pData, InsertDest_);

              if (pData^.ID < FIDSeed - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDSeed;

                  if FIDSeed > FIDSeed + 1 then
                      RebuildIDSeedCounter // rebuild seed
                  else
                      inc(FIDSeed);
                end;

              DoAddDataNotifyProc(Data_);

              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := New_Hash;
  pData^.i64 := i64;
  pData^.Data := Data_;
  pData^.ID := FIDSeed;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  inc(FCount);
  DoInsertBefore(pData, InsertDest_);

  if FIDSeed > FIDSeed + 1 then
      RebuildIDSeedCounter // rebuild seed
  else
      inc(FIDSeed);

  DoAddDataNotifyProc(Data_);
end;

function TInt64HashPointerList.Exists(i64: Int64): Boolean;
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  pData: PInt64HashListPointerStruct;
begin
  Result := False;
  if FCount = 0 then
      Exit;
  New_Hash := MakeHashI64(i64);
  lst := GetListTable(New_Hash, False);
  if lst <> nil then
    begin
      if lst.Count > 0 then
        for i := lst.Count - 1 downto 0 do
          begin
            pData := PInt64HashListPointerStruct(lst[i]);
            if (New_Hash = pData^.qHash) and (i64 = pData^.i64) then
                Exit(True);
          end;
    end;
end;

procedure TInt64HashPointerList.SetHashBlockCount(HashPoolSize_: Integer);
var
  i: Integer;
begin
  Clear;
  SetLength(FListBuffer, HashPoolSize_);
  for i := low(FListBuffer) to high(FListBuffer) do
      FListBuffer[i] := nil;
end;

function TInt64HashPointerList.First: Pointer;
begin
  if FFirst <> nil then
      Result := FFirst^.Data
  else
      Result := nil;
end;

function TInt64HashPointerList.Last: Pointer;
begin
  if FLast <> nil then
      Result := FLast^.Data
  else
      Result := nil;
end;

function TInt64HashPointerList.GetNext(i64: Int64): Pointer;
var
  p: PInt64HashListPointerStruct;
begin
  Result := nil;
  p := Geti64Data(i64);
  if (p = nil) or (p = FLast) or (p^.Next = p) then
      Exit;
  Result := p^.Next^.Data;
end;

function TInt64HashPointerList.GetPrev(i64: Int64): Pointer;
var
  p: PInt64HashListPointerStruct;
begin
  Result := nil;
  p := Geti64Data(i64);
  if (p = nil) or (p = FFirst) or (p^.Prev = p) then
      Exit;
  Result := p^.Prev^.Data;
end;

function TInt64HashPointerList.ListBuffer: PListBuffer;
begin
  Result := @FListBuffer;
end;

procedure TInt64HashPointerList.ProgressC(const OnProgress: TInt64HashPointerListLoop_C);
var
  i: NativeInt;
  p: PInt64HashListPointerStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.i64, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TInt64HashPointerList.ProgressM(const OnProgress: TInt64HashPointerListLoop_M);
var
  i: NativeInt;
  p: PInt64HashListPointerStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.i64, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TInt64HashPointerList.ProgressP(const OnProgress: TInt64HashPointerListLoop_P);
var
  i: NativeInt;
  p: PInt64HashListPointerStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.i64, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TInt64HashPointerList.PrintHashReport;
var
  i: NativeInt;
  L: TCore_List;
  Total: NativeInt;
  usaged, aMax, aMin: NativeInt;
  inited: Boolean;
begin
  inited := False;
  usaged := 0;
  aMax := 0;
  aMin := 0;
  Total := 0;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      L := FListBuffer[i];
      if L <> nil then
        begin
          inc(usaged);
          Total := Total + L.Count;
          if inited then
            begin
              if L.Count > aMax then
                  aMax := L.Count;
              if aMin > L.Count then
                  aMin := L.Count;
            end
          else
            begin
              aMax := L.Count;
              aMin := L.Count;
              inited := True;
            end;
        end;
    end;
  DoStatus(Format('usaged container:%d item total:%d Max:%d min:%d', [usaged, Total, aMax, aMin]));
end;

function TUInt32HashObjectList.GetListTable(hash: THash; AutoCreate: Boolean): TCore_List;
var
  i: Integer;
begin
  i := HashMod(hash, Length(FListBuffer));

  if (AutoCreate) and (FListBuffer[i] = nil) then
      FListBuffer[i] := TCore_List.Create;
  Result := FListBuffer[i];
end;

function TUInt32HashObjectList.Getu32Data(u32: UInt32): PUInt32HashListObjectStruct;
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  pData: PUInt32HashListObjectStruct;
begin
  Result := nil;
  New_Hash := MakeHashU32(u32);
  lst := GetListTable(New_Hash, False);
  if (lst <> nil) and (lst.Count > 0) then
    for i := lst.Count - 1 downto 0 do
      begin
        pData := PUInt32HashListObjectStruct(lst[i]);
        if (New_Hash = pData^.qHash) and (u32 = pData^.u32) then
          begin
            Result := pData;
            if (FAccessOptimization) and (pData^.ID < FIDSeed - 1) then
              begin
                DoDelete(pData);
                if i < lst.Count - 1 then
                  begin
                    lst.Delete(i);
                    lst.Add(pData);
                  end;
                pData^.ID := FIDSeed;
                DoAdd(pData);

                if FIDSeed > FIDSeed + 1 then
                    RebuildIDSeedCounter // rebuild seed
                else
                    inc(FIDSeed);
              end;
            Exit;
          end;
      end;
end;

function TUInt32HashObjectList.Getu32Val(u32: UInt32): TCore_Object;
var
  p: PUInt32HashListObjectStruct;
begin
  p := Getu32Data(u32);
  if p <> nil then
      Result := p^.Data
  else
      Result := nil;
end;

procedure TUInt32HashObjectList.RebuildIDSeedCounter;
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  i := 0;
  p := FFirst;
  while i < FCount do
    begin
      p^.ID := i + 1;
      inc(i);
      p := p^.Next;
    end;

  FIDSeed := i + 1;
end;

procedure TUInt32HashObjectList.DoAdd(p: PUInt32HashListObjectStruct);
begin
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
end;

procedure TUInt32HashObjectList.DoInsertBefore(p, To_: PUInt32HashListObjectStruct);
var
  P_P: PUInt32HashListObjectStruct;
begin
  if p = To_ then
      Exit;
  if FFirst = To_ then
      FFirst := p;
  P_P := To_^.Prev;
  if P_P^.Next = To_ then
      P_P^.Next := p;
  if To_^.Next = To_ then
      To_^.Next := p;
  To_^.Prev := p;
  p^.Prev := P_P;
  p^.Next := To_;
end;

procedure TUInt32HashObjectList.DoDelete(p: PUInt32HashListObjectStruct);
var
  P_P, N_P: PUInt32HashListObjectStruct;
begin
  P_P := p^.Prev;
  N_P := p^.Next;

  if p = FFirst then
      FFirst := N_P;
  if p = FLast then
      FLast := P_P;

  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
    end
  else
    begin
      P_P^.Next := N_P;
      N_P^.Prev := P_P;
      p^.Prev := nil;
      p^.Next := nil;
    end;
end;

procedure TUInt32HashObjectList.DoDataFreeProc(Obj: TCore_Object);
begin
  DisposeObject(Obj);
end;

constructor TUInt32HashObjectList.Create;
begin
  CustomCreate(256);
end;

constructor TUInt32HashObjectList.CustomCreate(HashPoolSize_: Integer);
begin
  inherited Create;
  FCount := 0;
  FIDSeed := 0;
  FAccessOptimization := False;
  FAutoFreeData := False;
  FFirst := nil;
  FLast := nil;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(HashPoolSize_);
end;

destructor TUInt32HashObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TUInt32HashObjectList.Clear;
var
  i: Integer;
  j: Integer;
  lst: TCore_List;
  pData: PUInt32HashListObjectStruct;
begin
  FCount := 0;
  FIDSeed := 0;
  FFirst := nil;
  FLast := nil;

  if Length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for j := lst.Count - 1 downto 0 do
                begin
                  pData := lst[j];
                  try
                    if (FAutoFreeData) and (pData^.Data <> nil) then
                        DoDataFreeProc(pData^.Data);
                    Dispose(pData);
                  except
                  end;
                end;
            end;
          DisposeObject(lst);
          FListBuffer[i] := nil;
        end;
    end;
end;

procedure TUInt32HashObjectList.GetListData(OutputList: TCore_List);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Count := FCount;
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList[i] := p;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TUInt32HashObjectList.Delete(u32: UInt32);
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  _ItemData: PUInt32HashListObjectStruct;
begin
  if FCount = 0 then
      Exit;
  New_Hash := MakeHashU32(u32);
  lst := GetListTable(New_Hash, False);
  if lst <> nil then
    begin
      i := 0;
      while i < lst.Count do
        begin
          _ItemData := lst[i];
          if (New_Hash = _ItemData^.qHash) and (u32 = _ItemData^.u32) then
            begin
              DoDelete(_ItemData);
              if (FAutoFreeData) and (_ItemData^.Data <> nil) then
                begin
                  try
                    DoDataFreeProc(_ItemData^.Data);
                    _ItemData^.Data := nil;
                  except
                  end;
                end;
              Dispose(_ItemData);
              lst.Delete(i);
              dec(FCount);
            end
          else
              inc(i);
        end;
    end;

  if FCount = 0 then
      FIDSeed := 1;
end;

function TUInt32HashObjectList.Add(u32: UInt32; Data_: TCore_Object; const Overwrite_: Boolean): PUInt32HashListObjectStruct;
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  pData: PUInt32HashListObjectStruct;
begin
  New_Hash := MakeHashU32(u32);

  lst := GetListTable(New_Hash, True);
  if (lst.Count > 0) and (Overwrite_) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PUInt32HashListObjectStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (u32 = pData^.u32) then
            begin
              DoDelete(pData);
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := Data_;
              Result := pData;

              DoAdd(pData);

              if (pData^.ID < FIDSeed - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDSeed;

                  if FIDSeed > FIDSeed + 1 then
                      RebuildIDSeedCounter // rebuild seed
                  else
                      inc(FIDSeed);
                end;

              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := New_Hash;
  pData^.u32 := u32;
  pData^.Data := Data_;
  pData^.ID := FIDSeed;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  inc(FCount);
  DoAdd(pData);

  if FIDSeed > FIDSeed + 1 then
      RebuildIDSeedCounter // rebuild seed
  else
      inc(FIDSeed);
end;

procedure TUInt32HashObjectList.SetValue(u32: UInt32; Data_: TCore_Object);
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  pData: PUInt32HashListObjectStruct;
  Done: Boolean;
begin
  New_Hash := MakeHashU32(u32);

  lst := GetListTable(New_Hash, True);
  Done := False;
  if (lst.Count > 0) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PUInt32HashListObjectStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (u32 = pData^.u32) then
            begin
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := Data_;
              Done := True;
            end;
        end;
    end;

  if not Done then
    begin
      new(pData);
      pData^.qHash := New_Hash;
      pData^.u32 := u32;
      pData^.Data := Data_;
      pData^.ID := FIDSeed;
      pData^.Prev := nil;
      pData^.Next := nil;
      lst.Add(pData);
      inc(FCount);
      DoAdd(pData);

      if FIDSeed > FIDSeed + 1 then
          RebuildIDSeedCounter // rebuild seed
      else
          inc(FIDSeed);
    end;
end;

function TUInt32HashObjectList.Insert(u32, InsertToBefore_: UInt32; Data_: TCore_Object; const Overwrite_: Boolean): PUInt32HashListObjectStruct;
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  InsertDest_, pData: PUInt32HashListObjectStruct;
begin
  InsertDest_ := u32Data[InsertToBefore_];
  if InsertDest_ = nil then
    begin
      Result := Add(u32, Data_, Overwrite_);
      Exit;
    end;

  New_Hash := MakeHashU32(u32);

  lst := GetListTable(New_Hash, True);
  if (lst.Count > 0) and (Overwrite_) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PUInt32HashListObjectStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (u32 = pData^.u32) then
            begin
              DoDelete(pData);
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
                begin
                  DoDataFreeProc(pData^.Data);
                end;
              pData^.Data := Data_;
              Result := pData;

              DoInsertBefore(pData, InsertDest_);

              if (pData^.ID < FIDSeed - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDSeed;

                  if FIDSeed > FIDSeed + 1 then
                      RebuildIDSeedCounter // rebuild seed
                  else
                      inc(FIDSeed);
                end;

              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := New_Hash;
  pData^.u32 := u32;
  pData^.Data := Data_;
  pData^.ID := FIDSeed;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  inc(FCount);
  DoInsertBefore(pData, InsertDest_);

  if FIDSeed > FIDSeed + 1 then
      RebuildIDSeedCounter // rebuild seed
  else
      inc(FIDSeed);
end;

function TUInt32HashObjectList.Exists(u32: UInt32): Boolean;
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  pData: PUInt32HashListObjectStruct;
begin
  Result := False;
  if FCount = 0 then
      Exit;
  New_Hash := MakeHashU32(u32);
  lst := GetListTable(New_Hash, False);
  if lst <> nil then
    begin
      if lst.Count > 0 then
        for i := lst.Count - 1 downto 0 do
          begin
            pData := PUInt32HashListObjectStruct(lst[i]);
            if (New_Hash = pData^.qHash) and (u32 = pData^.u32) then
                Exit(True);
          end;
    end;
end;

procedure TUInt32HashObjectList.SetHashBlockCount(HashPoolSize_: Integer);
var
  i: Integer;
begin
  Clear;
  SetLength(FListBuffer, HashPoolSize_);
  for i := low(FListBuffer) to high(FListBuffer) do
      FListBuffer[i] := nil;
end;

function TUInt32HashObjectList.First: TCore_Object;
begin
  if FFirst <> nil then
      Result := FFirst^.Data
  else
      Result := nil;
end;

function TUInt32HashObjectList.Last: TCore_Object;
begin
  if FLast <> nil then
      Result := FLast^.Data
  else
      Result := nil;
end;

function TUInt32HashObjectList.GetNext(u32: UInt32): TCore_Object;
var
  p: PUInt32HashListObjectStruct;
begin
  Result := nil;
  p := Getu32Data(u32);
  if (p = nil) or (p = FLast) or (p^.Next = p) then
      Exit;
  Result := p^.Next^.Data;
end;

function TUInt32HashObjectList.GetPrev(u32: UInt32): TCore_Object;
var
  p: PUInt32HashListObjectStruct;
begin
  Result := nil;
  p := Getu32Data(u32);
  if (p = nil) or (p = FFirst) or (p^.Prev = p) then
      Exit;
  Result := p^.Prev^.Data;
end;

function TUInt32HashObjectList.ListBuffer: PListBuffer;
begin
  Result := @FListBuffer;
end;

procedure TUInt32HashObjectList.ProgressC(const OnProgress: TUInt32HashObjectListLoop_C);
var
  i: NativeInt;
  p: PUInt32HashListObjectStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.u32, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TUInt32HashObjectList.ProgressM(const OnProgress: TUInt32HashObjectListLoop_M);
var
  i: NativeInt;
  p: PUInt32HashListObjectStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.u32, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TUInt32HashObjectList.ProgressP(const OnProgress: TUInt32HashObjectListLoop_P);
var
  i: NativeInt;
  p: PUInt32HashListObjectStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.u32, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function TUInt32HashObjectList.ExistsObject(Obj: TCore_Object): Boolean;
var
  i: NativeInt;
  p: PUInt32HashListObjectStruct;
begin
  Result := False;
  if (FCount > 0) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          if p^.Data = Obj then
            begin
              Result := True;
              Exit;
            end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TUInt32HashObjectList.PrintHashReport;
var
  i: NativeInt;
  L: TCore_List;
  Total: NativeInt;
  usaged, aMax, aMin: NativeInt;
  inited: Boolean;
begin
  inited := False;
  usaged := 0;
  aMax := 0;
  aMin := 0;
  Total := 0;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      L := FListBuffer[i];
      if L <> nil then
        begin
          inc(usaged);
          Total := Total + L.Count;
          if inited then
            begin
              if L.Count > aMax then
                  aMax := L.Count;
              if aMin > L.Count then
                  aMin := L.Count;
            end
          else
            begin
              aMax := L.Count;
              aMin := L.Count;
              inited := True;
            end;
        end;
    end;
  DoStatus(Format('usaged container:%d item total:%d Max:%d min:%d', [usaged, Total, aMax, aMin]));
end;

function TUInt32HashPointerList.GetListTable(hash: THash; AutoCreate: Boolean): TCore_List;
var
  i: Integer;
begin
  i := HashMod(hash, Length(FListBuffer));

  if (AutoCreate) and (FListBuffer[i] = nil) then
      FListBuffer[i] := TCore_List.Create;
  Result := FListBuffer[i];
end;

function TUInt32HashPointerList.Getu32Data(u32: UInt32): PUInt32HashListPointerStruct;
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  pData: PUInt32HashListPointerStruct;
begin
  Result := nil;
  New_Hash := MakeHashU32(u32);
  lst := GetListTable(New_Hash, False);
  if (lst <> nil) and (lst.Count > 0) then
    for i := lst.Count - 1 downto 0 do
      begin
        pData := PUInt32HashListPointerStruct(lst[i]);
        if (New_Hash = pData^.qHash) and (u32 = pData^.u32) then
          begin
            Result := pData;
            if (FAccessOptimization) and (pData^.ID < FIDSeed - 1) then
              begin
                DoDelete(pData);
                if i < lst.Count - 1 then
                  begin
                    lst.Delete(i);
                    lst.Add(pData);
                  end;
                pData^.ID := FIDSeed;
                DoAdd(pData);

                if FIDSeed > FIDSeed + 1 then
                    RebuildIDSeedCounter // rebuild seed
                else
                    inc(FIDSeed);
              end;
            Exit;
          end;
      end;
end;

function TUInt32HashPointerList.Getu32Val(u32: UInt32): Pointer;
var
  p: PUInt32HashListPointerStruct;
begin
  p := Getu32Data(u32);
  if p <> nil then
      Result := p^.Data
  else
      Result := nil;
end;

procedure TUInt32HashPointerList.RebuildIDSeedCounter;
var
  i: Integer;
  p: PUInt32HashListPointerStruct;
begin
  i := 0;
  p := FFirst;
  while i < FCount do
    begin
      p^.ID := i + 1;
      inc(i);
      p := p^.Next;
    end;

  FIDSeed := i + 1;
end;

procedure TUInt32HashPointerList.DoAdd(p: PUInt32HashListPointerStruct);
begin
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
end;

procedure TUInt32HashPointerList.DoInsertBefore(p, To_: PUInt32HashListPointerStruct);
var
  P_P: PUInt32HashListPointerStruct;
begin
  if p = To_ then
      Exit;
  if FFirst = To_ then
      FFirst := p;
  P_P := To_^.Prev;
  if P_P^.Next = To_ then
      P_P^.Next := p;
  if To_^.Next = To_ then
      To_^.Next := p;
  To_^.Prev := p;
  p^.Prev := P_P;
  p^.Next := To_;
end;

procedure TUInt32HashPointerList.DoDelete(p: PUInt32HashListPointerStruct);
var
  P_P, N_P: PUInt32HashListPointerStruct;
begin
  P_P := p^.Prev;
  N_P := p^.Next;

  if p = FFirst then
      FFirst := N_P;
  if p = FLast then
      FLast := P_P;

  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
    end
  else
    begin
      P_P^.Next := N_P;
      N_P^.Prev := P_P;
      p^.Prev := nil;
      p^.Next := nil;
    end;
end;

procedure TUInt32HashPointerList.DoDataFreeProc(pData: Pointer);
begin
  if Assigned(FOnFreePtr) then
      FOnFreePtr(pData);
end;

procedure TUInt32HashPointerList.DoAddDataNotifyProc(pData: Pointer);
begin
  if Assigned(FOnAddPtr) then
      FOnAddPtr(pData);
end;

constructor TUInt32HashPointerList.Create;
begin
  CustomCreate(256);
end;

constructor TUInt32HashPointerList.CustomCreate(HashPoolSize_: Integer);
begin
  inherited Create;
  FCount := 0;
  FIDSeed := 0;
  FAccessOptimization := False;
  FAutoFreeData := False;
  FFirst := nil;
  FLast := nil;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(HashPoolSize_);
  FOnFreePtr := nil;
  FOnAddPtr := nil;
end;

destructor TUInt32HashPointerList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TUInt32HashPointerList.Clear;
var
  i: Integer;
  j: Integer;
  lst: TCore_List;
  pData: PUInt32HashListPointerStruct;
begin
  FCount := 0;
  FIDSeed := 0;
  FFirst := nil;
  FLast := nil;

  if Length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for j := lst.Count - 1 downto 0 do
                begin
                  pData := lst[j];
                  try
                    if (FAutoFreeData) and (pData^.Data <> nil) then
                        DoDataFreeProc(pData^.Data);
                    Dispose(pData);
                  except
                  end;
                end;
            end;
          DisposeObject(lst);
          FListBuffer[i] := nil;
        end;
    end;
end;

procedure TUInt32HashPointerList.GetListData(OutputList: TCore_List);
var
  i: Integer;
  p: PUInt32HashListPointerStruct;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Count := FCount;
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList[i] := p;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function TUInt32HashPointerList.Delete(u32: UInt32): Boolean;
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  _ItemData: PUInt32HashListPointerStruct;
begin
  Result := False;
  if FCount = 0 then
      Exit;
  New_Hash := MakeHashU32(u32);
  lst := GetListTable(New_Hash, False);
  if lst <> nil then
    begin
      i := 0;
      while i < lst.Count do
        begin
          _ItemData := lst[i];
          if (New_Hash = _ItemData^.qHash) and (u32 = _ItemData^.u32) then
            begin
              DoDelete(_ItemData);
              if (FAutoFreeData) and (_ItemData^.Data <> nil) then
                begin
                  try
                    DoDataFreeProc(_ItemData^.Data);
                    _ItemData^.Data := nil;
                  except
                  end;
                end;
              Dispose(_ItemData);
              lst.Delete(i);
              dec(FCount);
              Result := True;
            end
          else
              inc(i);
        end;
    end;

  if FCount = 0 then
      FIDSeed := 1;
end;

function TUInt32HashPointerList.Add(u32: UInt32; Data_: Pointer; const Overwrite_: Boolean): PUInt32HashListPointerStruct;
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  pData: PUInt32HashListPointerStruct;
begin
  New_Hash := MakeHashU32(u32);

  lst := GetListTable(New_Hash, True);
  if (lst.Count > 0) and (Overwrite_) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PUInt32HashListPointerStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (u32 = pData^.u32) then
            begin
              DoDelete(pData);
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
                begin
                  DoDataFreeProc(pData^.Data);
                end;
              pData^.Data := Data_;
              Result := pData;

              DoAdd(pData);

              if (pData^.ID < FIDSeed - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDSeed;

                  if FIDSeed > FIDSeed + 1 then
                      RebuildIDSeedCounter // rebuild seed
                  else
                      inc(FIDSeed);
                end;
              DoAddDataNotifyProc(pData^.Data);

              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := New_Hash;
  pData^.u32 := u32;
  pData^.Data := Data_;
  pData^.ID := FIDSeed;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  inc(FCount);
  DoAdd(pData);

  if FIDSeed > FIDSeed + 1 then
      RebuildIDSeedCounter // rebuild seed
  else
      inc(FIDSeed);

  DoAddDataNotifyProc(pData^.Data);
end;

procedure TUInt32HashPointerList.SetValue(u32: UInt32; Data_: Pointer);
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  pData: PUInt32HashListPointerStruct;
  Done: Boolean;
begin
  New_Hash := MakeHashU32(u32);

  lst := GetListTable(New_Hash, True);
  Done := False;
  if (lst.Count > 0) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PUInt32HashListPointerStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (u32 = pData^.u32) then
            begin
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
                begin
                  try
                      DoDataFreeProc(pData^.Data);
                  except
                  end;
                end;
              pData^.Data := Data_;
              Done := True;
              DoAddDataNotifyProc(Data_);
            end;
        end;
    end;

  if not Done then
    begin
      new(pData);
      pData^.qHash := New_Hash;
      pData^.u32 := u32;
      pData^.Data := Data_;
      pData^.ID := FIDSeed;
      pData^.Prev := nil;
      pData^.Next := nil;
      lst.Add(pData);
      inc(FCount);
      DoAdd(pData);

      if FIDSeed > FIDSeed + 1 then
          RebuildIDSeedCounter // rebuild seed
      else
          inc(FIDSeed);
      DoAddDataNotifyProc(pData^.Data);
    end;
end;

function TUInt32HashPointerList.Insert(u32, InsertToBefore_: UInt32; Data_: Pointer; const Overwrite_: Boolean): PUInt32HashListPointerStruct;
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  InsertDest_, pData: PUInt32HashListPointerStruct;
begin
  InsertDest_ := u32Data[InsertToBefore_];
  if InsertDest_ = nil then
    begin
      Result := Add(u32, Data_, Overwrite_);
      Exit;
    end;

  New_Hash := MakeHashU32(u32);

  lst := GetListTable(New_Hash, True);
  if (lst.Count > 0) and (Overwrite_) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PUInt32HashListPointerStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (u32 = pData^.u32) then
            begin
              DoDelete(pData);
              if (FAutoFreeData) and (pData^.Data <> nil) and (pData^.Data <> Data_) then
                begin
                  DoDataFreeProc(pData^.Data);
                end;
              pData^.Data := Data_;
              Result := pData;

              DoInsertBefore(pData, InsertDest_);

              if (pData^.ID < FIDSeed - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDSeed;

                  if FIDSeed > FIDSeed + 1 then
                      RebuildIDSeedCounter // rebuild seed
                  else
                      inc(FIDSeed);
                end;
              DoAddDataNotifyProc(pData^.Data);

              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := New_Hash;
  pData^.u32 := u32;
  pData^.Data := Data_;
  pData^.ID := FIDSeed;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  inc(FCount);
  DoInsertBefore(pData, InsertDest_);

  if FIDSeed > FIDSeed + 1 then
      RebuildIDSeedCounter // rebuild seed
  else
      inc(FIDSeed);

  DoAddDataNotifyProc(pData^.Data);
end;

function TUInt32HashPointerList.Exists(u32: UInt32): Boolean;
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  pData: PUInt32HashListPointerStruct;
begin
  Result := False;
  if FCount = 0 then
      Exit;
  New_Hash := MakeHashU32(u32);
  lst := GetListTable(New_Hash, False);
  if lst <> nil then
    begin
      if lst.Count > 0 then
        for i := lst.Count - 1 downto 0 do
          begin
            pData := PUInt32HashListPointerStruct(lst[i]);
            if (New_Hash = pData^.qHash) and (u32 = pData^.u32) then
                Exit(True);
          end;
    end;
end;

procedure TUInt32HashPointerList.SetHashBlockCount(HashPoolSize_: Integer);
var
  i: Integer;
begin
  Clear;
  SetLength(FListBuffer, HashPoolSize_);
  for i := low(FListBuffer) to high(FListBuffer) do
      FListBuffer[i] := nil;
end;

function TUInt32HashPointerList.First: Pointer;
begin
  if FFirst <> nil then
      Result := FFirst^.Data
  else
      Result := nil;
end;

function TUInt32HashPointerList.Last: Pointer;
begin
  if FLast <> nil then
      Result := FLast^.Data
  else
      Result := nil;
end;

function TUInt32HashPointerList.GetNext(u32: UInt32): Pointer;
var
  p: PUInt32HashListPointerStruct;
begin
  Result := nil;
  p := Getu32Data(u32);
  if (p = nil) or (p = FLast) or (p^.Next = p) then
      Exit;
  Result := p^.Next^.Data;
end;

function TUInt32HashPointerList.GetPrev(u32: UInt32): Pointer;
var
  p: PUInt32HashListPointerStruct;
begin
  Result := nil;
  p := Getu32Data(u32);
  if (p = nil) or (p = FFirst) or (p^.Prev = p) then
      Exit;
  Result := p^.Prev^.Data;
end;

function TUInt32HashPointerList.ListBuffer: PListBuffer;
begin
  Result := @FListBuffer;
end;

procedure TUInt32HashPointerList.ProgressC(const OnProgress: TUInt32HashPointerListLoop_C);
var
  i: NativeInt;
  p: PUInt32HashListPointerStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.u32, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TUInt32HashPointerList.ProgressM(const OnProgress: TUInt32HashPointerListLoop_M);
var
  i: NativeInt;
  p: PUInt32HashListPointerStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.u32, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TUInt32HashPointerList.ProgressP(const OnProgress: TUInt32HashPointerListLoop_P);
var
  i: NativeInt;
  p: PUInt32HashListPointerStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.u32, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function TUInt32HashPointerList.ExistsPointer(pData: Pointer): Boolean;
var
  i: NativeInt;
  p: PUInt32HashListPointerStruct;
begin
  Result := False;
  if (FCount > 0) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          if p^.Data = pData then
            begin
              Result := True;
              Exit;
            end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TUInt32HashPointerList.PrintHashReport;
var
  i: NativeInt;
  L: TCore_List;
  Total: NativeInt;
  usaged, aMax, aMin: NativeInt;
  inited: Boolean;
begin
  inited := False;
  usaged := 0;
  aMax := 0;
  aMin := 0;
  Total := 0;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      L := FListBuffer[i];
      if L <> nil then
        begin
          inc(usaged);
          Total := Total + L.Count;
          if inited then
            begin
              if L.Count > aMax then
                  aMax := L.Count;
              if aMin > L.Count then
                  aMin := L.Count;
            end
          else
            begin
              aMax := L.Count;
              aMin := L.Count;
              inited := True;
            end;
        end;
    end;
  DoStatus(Format('usaged container:%d item total:%d Max:%d min:%d', [usaged, Total, aMax, aMin]));
end;

function TPointerHashNativeUIntList.GetListTable(hash: THash; AutoCreate: Boolean): TCore_List;
var
  i: Integer;
begin
  i := HashMod(hash, Length(FListBuffer));

  if (AutoCreate) and (FListBuffer[i] = nil) then
      FListBuffer[i] := TCore_List.Create;
  Result := FListBuffer[i];
end;

function TPointerHashNativeUIntList.GetNPtrData(NPtr: Pointer): PPointerHashListNativeUIntStruct;
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  pData: PPointerHashListNativeUIntStruct;
begin
  Result := nil;
  New_Hash := MakeHashP(NPtr);
  lst := GetListTable(New_Hash, False);
  if (lst <> nil) and (lst.Count > 0) then
    for i := lst.Count - 1 downto 0 do
      begin
        pData := PPointerHashListNativeUIntStruct(lst[i]);
        if (New_Hash = pData^.qHash) and (NPtr = pData^.NPtr) then
          begin
            Result := pData;
            if (FAccessOptimization) and (pData^.ID < FIDSeed - 1) then
              begin
                DoDelete(pData);
                if i < lst.Count - 1 then
                  begin
                    lst.Delete(i);
                    lst.Add(pData);
                  end;
                pData^.ID := FIDSeed;
                DoAdd(pData);

                if FIDSeed > FIDSeed + 1 then
                    RebuildIDSeedCounter // rebuild seed
                else
                    inc(FIDSeed);
              end;
            Exit;
          end;
      end;
end;

function TPointerHashNativeUIntList.GetNPtrVal(NPtr: Pointer): NativeUInt;
var
  p: PPointerHashListNativeUIntStruct;
begin
  p := GetNPtrData(NPtr);
  if p <> nil then
      Result := p^.Data
  else
      Result := NullValue;
end;

procedure TPointerHashNativeUIntList.RebuildIDSeedCounter;
var
  i: Integer;
  p: PPointerHashListNativeUIntStruct;
begin
  i := 0;
  p := FFirst;
  while i < FCount do
    begin
      p^.ID := i + 1;
      inc(i);
      p := p^.Next;
    end;

  FIDSeed := i + 1;
end;

procedure TPointerHashNativeUIntList.DoAdd(p: PPointerHashListNativeUIntStruct);
begin
  if (FFirst = nil) or (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
      p^.Prev := p;
      p^.Next := p;
    end
  else if FFirst = FLast then
    begin
      FLast := p;
      FFirst^.Prev := FLast;
      FFirst^.Next := FLast;
      FLast^.Next := FFirst;
      FLast^.Prev := FFirst;
    end
  else
    begin
      FFirst^.Prev := p;
      FLast^.Next := p;
      p^.Next := FFirst;
      p^.Prev := FLast;
      FLast := p;
    end;
end;

procedure TPointerHashNativeUIntList.DoInsertBefore(p, To_: PPointerHashListNativeUIntStruct);
var
  P_P: PPointerHashListNativeUIntStruct;
begin
  if p = To_ then
      Exit;
  if FFirst = To_ then
      FFirst := p;
  P_P := To_^.Prev;
  if P_P^.Next = To_ then
      P_P^.Next := p;
  if To_^.Next = To_ then
      To_^.Next := p;
  To_^.Prev := p;
  p^.Prev := P_P;
  p^.Next := To_;
end;

procedure TPointerHashNativeUIntList.DoDelete(p: PPointerHashListNativeUIntStruct);
var
  P_P, N_P: PPointerHashListNativeUIntStruct;
begin
  P_P := p^.Prev;
  N_P := p^.Next;

  if p = FFirst then
      FFirst := N_P;
  if p = FLast then
      FLast := P_P;

  if (FFirst = FLast) and (FLast = p) then
    begin
      FFirst := nil;
      FLast := nil;
    end
  else
    begin
      P_P^.Next := N_P;
      N_P^.Prev := P_P;
      p^.Prev := nil;
      p^.Next := nil;
    end;
end;

constructor TPointerHashNativeUIntList.Create;
begin
  CustomCreate(256);
end;

constructor TPointerHashNativeUIntList.CustomCreate(HashPoolSize_: Integer);
begin
  inherited Create;
  FCount := 0;
  FIDSeed := 0;
  FAccessOptimization := False;
  FFirst := nil;
  FLast := nil;
  FTotal := 0;
  FMinimizePtr := nil;
  FMaximumPtr := nil;
  SetLength(FListBuffer, 0);
  SetHashBlockCount(HashPoolSize_);
end;

destructor TPointerHashNativeUIntList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TPointerHashNativeUIntList.Clear;
var
  i: Integer;
  j: Integer;
  lst: TCore_List;
  pData: PPointerHashListNativeUIntStruct;
begin
  FCount := 0;
  FIDSeed := 0;
  FFirst := nil;
  FLast := nil;
  FTotal := 0;
  FMinimizePtr := nil;
  FMaximumPtr := nil;

  if Length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for j := lst.Count - 1 downto 0 do
                begin
                  pData := lst[j];
                  Dispose(pData);
                end;
            end;
          DisposeObject(lst);
          FListBuffer[i] := nil;
        end;
    end;
end;

procedure TPointerHashNativeUIntList.FastClear;
var
  i: Integer;
  j: Integer;
  lst: TCore_List;
  pData: PPointerHashListNativeUIntStruct;
begin
  FCount := 0;
  FIDSeed := 0;
  FFirst := nil;
  FLast := nil;
  FTotal := 0;
  FMinimizePtr := nil;
  FMaximumPtr := nil;

  if Length(FListBuffer) = 0 then
      Exit;

  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      if FListBuffer[i] <> nil then
        begin
          lst := FListBuffer[i];
          if lst.Count > 0 then
            begin
              for j := lst.Count - 1 downto 0 do
                begin
                  pData := lst[j];
                  Dispose(pData);
                end;
              lst.Clear;
            end;
        end;
    end;
end;

procedure TPointerHashNativeUIntList.GetListData(OutputList: TCore_List);
var
  i: Integer;
  p: PPointerHashListNativeUIntStruct;
begin
  OutputList.Clear;
  if FCount > 0 then
    begin
      OutputList.Count := FCount;
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          OutputList[i] := p;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function TPointerHashNativeUIntList.Delete(NPtr: Pointer): Boolean;
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  _ItemData: PPointerHashListNativeUIntStruct;
begin
  Result := False;
  if FCount = 0 then
      Exit;
  New_Hash := MakeHashP(NPtr);
  lst := GetListTable(New_Hash, False);
  if lst <> nil then
    begin
      i := 0;
      while i < lst.Count do
        begin
          _ItemData := lst[i];
          if (New_Hash = _ItemData^.qHash) and (NPtr = _ItemData^.NPtr) then
            begin
              dec(FTotal, _ItemData^.Data);
              DoDelete(_ItemData);
              Dispose(_ItemData);
              lst.Delete(i);
              dec(FCount);
              Result := True;
            end
          else
              inc(i);
        end;
    end;

  if FCount = 0 then
    begin
      FIDSeed := 1;
      FTotal := 0;
      FMinimizePtr := nil;
      FMaximumPtr := nil;
    end;
end;

function TPointerHashNativeUIntList.Add(NPtr: Pointer; Data_: NativeUInt; const Overwrite_: Boolean): PPointerHashListNativeUIntStruct;
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  pData: PPointerHashListNativeUIntStruct;
begin
  New_Hash := MakeHashP(NPtr);

  lst := GetListTable(New_Hash, True);
  if (lst.Count > 0) and (Overwrite_) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PPointerHashListNativeUIntStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (NPtr = pData^.NPtr) then
            begin
              dec(FTotal, pData^.Data);
              DoDelete(pData);
              pData^.Data := Data_;
              Result := pData;

              DoAdd(pData);

              if (pData^.ID < FIDSeed - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDSeed;

                  if FIDSeed > FIDSeed + 1 then
                      RebuildIDSeedCounter // rebuild seed
                  else
                      inc(FIDSeed);
                end;

              inc(FTotal, pData^.Data);
              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := New_Hash;
  pData^.NPtr := NPtr;
  pData^.Data := Data_;
  pData^.ID := FIDSeed;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  inc(FCount);
  DoAdd(pData);

  if FIDSeed > FIDSeed + 1 then
      RebuildIDSeedCounter // rebuild seed
  else
      inc(FIDSeed);

  inc(FTotal, pData^.Data);

  if (NativeUInt(NPtr) < NativeUInt(FMinimizePtr)) or (FMinimizePtr = nil) then
      FMinimizePtr := NPtr;
  if (NativeUInt(NPtr) > NativeUInt(FMaximumPtr)) or (FMaximumPtr = nil) then
      FMaximumPtr := NPtr;
end;

procedure TPointerHashNativeUIntList.SetValue(NPtr: Pointer; Data_: NativeUInt);
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  pData: PPointerHashListNativeUIntStruct;
  Done: Boolean;
begin
  New_Hash := MakeHashP(NPtr);

  lst := GetListTable(New_Hash, True);
  Done := False;
  if (lst.Count > 0) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PPointerHashListNativeUIntStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (NPtr = pData^.NPtr) then
            begin
              dec(FTotal, pData^.Data);
              pData^.Data := Data_;
              inc(FTotal, pData^.Data);
              Done := True;
            end;
        end;
    end;

  if not Done then
    begin
      new(pData);
      pData^.qHash := New_Hash;
      pData^.NPtr := NPtr;
      pData^.Data := Data_;
      pData^.ID := FIDSeed;
      pData^.Prev := nil;
      pData^.Next := nil;
      lst.Add(pData);
      inc(FCount);
      DoAdd(pData);

      if FIDSeed > FIDSeed + 1 then
          RebuildIDSeedCounter // rebuild seed
      else
          inc(FIDSeed);

      inc(FTotal, pData^.Data);

      if (NativeUInt(NPtr) < NativeUInt(FMinimizePtr)) or (FMinimizePtr = nil) then
          FMinimizePtr := NPtr;
      if (NativeUInt(NPtr) > NativeUInt(FMaximumPtr)) or (FMaximumPtr = nil) then
          FMaximumPtr := NPtr;
    end;
end;

function TPointerHashNativeUIntList.Insert(NPtr, InsertToBefore_: Pointer; Data_: NativeUInt; const Overwrite_: Boolean): PPointerHashListNativeUIntStruct;
var
  New_Hash: THash;
  lst: TCore_List;
  i: Integer;
  InsertDest_, pData: PPointerHashListNativeUIntStruct;
begin
  InsertDest_ := NPtrData[InsertToBefore_];
  if InsertDest_ = nil then
    begin
      Result := Add(NPtr, Data_, Overwrite_);
      Exit;
    end;

  New_Hash := MakeHashP(NPtr);

  lst := GetListTable(New_Hash, True);
  if (lst.Count > 0) and (Overwrite_) then
    begin
      for i := lst.Count - 1 downto 0 do
        begin
          pData := PPointerHashListNativeUIntStruct(lst[i]);
          if (New_Hash = pData^.qHash) and (NPtr = pData^.NPtr) then
            begin
              dec(FTotal, pData^.Data);
              DoDelete(pData);
              pData^.Data := Data_;
              Result := pData;

              DoInsertBefore(pData, InsertDest_);

              if (pData^.ID < FIDSeed - 1) then
                begin
                  if i < lst.Count - 1 then
                    begin
                      lst.Delete(i);
                      lst.Add(pData);
                    end;
                  pData^.ID := FIDSeed;

                  if FIDSeed > FIDSeed + 1 then
                      RebuildIDSeedCounter // rebuild seed
                  else
                      inc(FIDSeed);
                end;

              inc(FTotal, pData^.Data);
              Exit;
            end;
        end;
    end;

  new(pData);
  pData^.qHash := New_Hash;
  pData^.NPtr := NPtr;
  pData^.Data := Data_;
  pData^.ID := FIDSeed;
  pData^.Prev := nil;
  pData^.Next := nil;
  lst.Add(pData);
  Result := pData;
  inc(FCount);
  DoInsertBefore(pData, InsertDest_);

  if FIDSeed > FIDSeed + 1 then
      RebuildIDSeedCounter // rebuild seed
  else
      inc(FIDSeed);

  inc(FTotal, pData^.Data);

  if (NativeUInt(NPtr) < NativeUInt(FMinimizePtr)) or (FMinimizePtr = nil) then
      FMinimizePtr := NPtr;
  if (NativeUInt(NPtr) > NativeUInt(FMaximumPtr)) or (FMaximumPtr = nil) then
      FMaximumPtr := NPtr;
end;

function TPointerHashNativeUIntList.Exists(NPtr: Pointer): Boolean;
var
  New_Hash: THash;
  i: Integer;
  lst: TCore_List;
  pData: PPointerHashListNativeUIntStruct;
begin
  Result := False;
  if FCount = 0 then
      Exit;
  New_Hash := MakeHashP(NPtr);
  lst := GetListTable(New_Hash, False);
  if lst <> nil then
    begin
      if lst.Count > 0 then
        for i := lst.Count - 1 downto 0 do
          begin
            pData := PPointerHashListNativeUIntStruct(lst[i]);
            if (New_Hash = pData^.qHash) and (NPtr = pData^.NPtr) then
                Exit(True);
          end;
    end;
end;

procedure TPointerHashNativeUIntList.SetHashBlockCount(HashPoolSize_: Integer);
var
  i: Integer;
begin
  Clear;
  SetLength(FListBuffer, HashPoolSize_);
  for i := low(FListBuffer) to high(FListBuffer) do
      FListBuffer[i] := nil;
end;

function TPointerHashNativeUIntList.First: NativeUInt;
begin
  if FFirst <> nil then
      Result := FFirst^.Data
  else
      Result := NullValue;
end;

function TPointerHashNativeUIntList.Last: NativeUInt;
begin
  if FLast <> nil then
      Result := FLast^.Data
  else
      Result := NullValue;
end;

function TPointerHashNativeUIntList.GetNext(NPtr: Pointer): NativeUInt;
var
  p: PPointerHashListNativeUIntStruct;
begin
  Result := NullValue;
  p := GetNPtrData(NPtr);
  if (p = nil) or (p = FLast) or (p^.Next = p) then
      Exit;
  Result := p^.Next^.Data;
end;

function TPointerHashNativeUIntList.GetPrev(NPtr: Pointer): NativeUInt;
var
  p: PPointerHashListNativeUIntStruct;
begin
  Result := NullValue;
  p := GetNPtrData(NPtr);
  if (p = nil) or (p = FFirst) or (p^.Prev = p) then
      Exit;
  Result := p^.Prev^.Data;
end;

function TPointerHashNativeUIntList.ListBuffer: PListBuffer;
begin
  Result := @FListBuffer;
end;

procedure TPointerHashNativeUIntList.ProgressC(const OnProgress: TPointerHashNativeUIntListLoop_C);
var
  i: Integer;
  p: PPointerHashListNativeUIntStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.NPtr, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TPointerHashNativeUIntList.ProgressM(const OnProgress: TPointerHashNativeUIntListLoop_M);
var
  i: Integer;
  p: PPointerHashListNativeUIntStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.NPtr, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TPointerHashNativeUIntList.ProgressP(const OnProgress: TPointerHashNativeUIntListLoop_P);
var
  i: Integer;
  p: PPointerHashListNativeUIntStruct;
begin
  if (FCount > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          try
              OnProgress(p^.NPtr, p^.Data);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function TPointerHashNativeUIntList.ExistsNaviveUInt(Obj: NativeUInt): Boolean;
var
  i: Integer;
  p: PPointerHashListNativeUIntStruct;
begin
  Result := False;
  if (FCount > 0) then
    begin
      i := 0;
      p := FFirst;
      while i < FCount do
        begin
          if p^.Data = Obj then
            begin
              Result := True;
              Exit;
            end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TPointerHashNativeUIntList.PrintHashReport;
var
  i: NativeInt;
  L: TCore_List;
  t: NativeInt;
  usaged, aMax, aMin: NativeInt;
  inited: Boolean;
begin
  inited := False;
  usaged := 0;
  aMax := 0;
  aMin := 0;
  t := 0;
  for i := low(FListBuffer) to high(FListBuffer) do
    begin
      L := FListBuffer[i];
      if L <> nil then
        begin
          inc(usaged);
          t := t + L.Count;
          if inited then
            begin
              if L.Count > aMax then
                  aMax := L.Count;
              if aMin > L.Count then
                  aMin := L.Count;
            end
          else
            begin
              aMax := L.Count;
              aMin := L.Count;
              inited := True;
            end;
        end;
    end;
  DoStatus(Format('usaged container:%d item total:%d Max:%d min:%d', [usaged, t, aMax, aMin]));
end;

function THashObjectList.GetCount: NativeInt;
begin
  Result := FHashList.Count;
end;

function THashObjectList.GetIgnoreCase: Boolean;
begin
  Result := FHashList.IgnoreCase;
end;

procedure THashObjectList.SetIgnoreCase(const Value: Boolean);
begin
  FHashList.IgnoreCase := Value;
end;

function THashObjectList.GetKeyValue(const Name: SystemString): TCore_Object;
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.NameValue[Name];
  if pObjData <> nil then
      Result := pObjData^.Obj
  else
      Result := nil;
end;

procedure THashObjectList.SetKeyValue(const Name: SystemString; const Value: TCore_Object);
begin
  Add(Name, Value);
end;

function THashObjectList.GetOnChange(const Name: SystemString): THashObjectChangeEvent;
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.NameValue[Name];
  if pObjData <> nil then
      Result := pObjData^.OnChnage
  else
      Result := nil;
end;

procedure THashObjectList.SetOnChange(const Name: SystemString; const Value_: THashObjectChangeEvent);
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.NameValue[Name];
  if pObjData = nil then
    begin
      new(pObjData);
      pObjData^.OnChnage := Value_;
      pObjData^.Obj := nil;
      FHashList.Add(Name, pObjData, False);
    end
  else
      pObjData^.OnChnage := Value_;
end;

function THashObjectList.GetAccessOptimization: Boolean;
begin
  Result := FHashList.AccessOptimization;
end;

procedure THashObjectList.SetAccessOptimization(const Value: Boolean);
begin
  FHashList.AccessOptimization := Value;
end;

procedure THashObjectList.DefaultDataFreeProc(p: Pointer);
begin
  Dispose(PHashObjectListData(p));
end;

constructor THashObjectList.Create(AutoFreeData_: Boolean);
begin
  CustomCreate(AutoFreeData_, 64);
end;

constructor THashObjectList.CustomCreate(AutoFreeData_: Boolean; HashPoolSize_: Integer);
begin
  inherited Create;
  FHashList := THashList.CustomCreate(HashPoolSize_);
  FHashList.FAutoFreeData := True;

  FHashList.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FAutoFreeObject := AutoFreeData_;
  FIncremental := 0;
end;

destructor THashObjectList.Destroy;
begin
  Clear;
  DisposeObject(FHashList);
  inherited Destroy;
end;

procedure THashObjectList.Assign(sour: THashObjectList);
var
  i: Integer;
  p: PHashListData;
begin
  Clear;
  if sour.HashList.Count > 0 then
    begin
      i := 0;
      p := sour.HashList.FirstPtr;
      while i < sour.HashList.Count do
        begin
          FastAdd(p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.ProgressC(const OnProgress: THashObjectListLoop_C);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(@p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.ProgressM(const OnProgress: THashObjectListLoop_M);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(@p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.ProgressP(const OnProgress: THashObjectListLoop_P);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(@p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.Clear;
var
  lst: TCore_List;
  pObjData: PHashObjectListData;
  i: Integer;
begin
  if AutoFreeObject then
    begin
      lst := TCore_List.Create;
      FHashList.GetListData(lst);
      if lst.Count > 0 then
        for i := 0 to lst.Count - 1 do
          with PHashListData(lst[i])^ do
            begin
              pObjData := Data;
              if pObjData <> nil then
                if pObjData^.Obj <> nil then
                  begin
                    try
                        DisposeObject(pObjData^.Obj);
                    except
                    end;
                  end;
            end;
      DisposeObject(lst);
    end;
  FHashList.Clear;
  FIncremental := 0;
end;

procedure THashObjectList.GetNameList(OutputList: TCore_Strings);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.AddObject(p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.GetNameList(OutputList: TListString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.GetNameList(OutputList: TListPascalString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.GetListData(OutputList: TCore_Strings);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.AddObject(p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.GetListData(OutputList: TListString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.GetListData(OutputList: TListPascalString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName, PHashObjectListData(p^.Data)^.Obj);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.GetAsList(OutputList: TCore_ListForObj);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(PHashObjectListData(p^.Data)^.Obj);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function THashObjectList.GetObjAsName(Obj: TCore_Object): SystemString;
var
  i: Integer;
  p: PHashListData;
begin
  Result := '';
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          if PHashObjectListData(p^.Data)^.Obj = Obj then
            begin
              Result := p^.OriginName;
              Exit;
            end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashObjectList.Delete(const Name: SystemString);
var
  pObjData: PHashObjectListData;
begin
  if AutoFreeObject then
    begin
      pObjData := FHashList.NameValue[Name];
      if pObjData <> nil then
        begin
          if pObjData^.Obj <> nil then
            begin
              try
                DisposeObject(pObjData^.Obj);
                pObjData^.Obj := nil;
              except
              end;
            end;
        end;
    end;
  FHashList.Delete(Name);
end;

function THashObjectList.Add(const Name: SystemString; Obj_: TCore_Object): TCore_Object;
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.NameValue[Name];
  if pObjData <> nil then
    begin
      try
        if Assigned(pObjData^.OnChnage) then
            pObjData^.OnChnage(Self, Name, pObjData^.Obj, Obj_);
      except
      end;

      if (FAutoFreeObject) and (pObjData^.Obj <> nil) then
        begin
          try
            DisposeObject(pObjData^.Obj);
            pObjData^.Obj := nil;
          except
          end;
        end;
    end
  else
    begin
      new(pObjData);
      pObjData^.OnChnage := nil;
      FHashList.Add(Name, pObjData, False);
    end;

  pObjData^.Obj := Obj_;
  Result := Obj_;
end;

function THashObjectList.FastAdd(const Name: SystemString; Obj_: TCore_Object): TCore_Object;
var
  pObjData: PHashObjectListData;
begin
  new(pObjData);
  pObjData^.OnChnage := nil;
  FHashList.Add(Name, pObjData, False);

  pObjData^.Obj := Obj_;
  Result := Obj_;
end;

function THashObjectList.Find(const Name: SystemString): TCore_Object;
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.Find(Name);
  if pObjData <> nil then
      Result := pObjData^.Obj
  else
      Result := nil;
end;

function THashObjectList.Exists(const Name: SystemString): Boolean;
begin
  Result := FHashList.Exists(Name);
end;

function THashObjectList.ExistsObject(Obj: TCore_Object): Boolean;
var
  lst: TCore_List;
  i: Integer;
begin
  Result := False;
  lst := TCore_List.Create;
  FHashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            if PHashObjectListData(Data)^.Obj = Obj then
              begin
                Result := True;
                Break;
              end;
          end;
      end;
  DisposeObject(lst);
end;

procedure THashObjectList.CopyFrom(const Source: THashObjectList);
var
  lst: TCore_List;
  pObjData: PHashObjectListData;
  i: Integer;
begin
  lst := TCore_List.Create;
  Source.HashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          if Data <> nil then
            begin
              pObjData := Data;
              NameValue[OriginName] := pObjData^.Obj;
            end;
      end;
  DisposeObject(lst);
end;

function THashObjectList.ReName(_OLDName, _NewName: SystemString): Boolean;
var
  pObjData: PHashObjectListData;
begin
  pObjData := FHashList.NameValue[_OLDName];
  Result := (_OLDName <> _NewName) and (pObjData <> nil) and (FHashList.NameValue[_NewName] = nil);
  if Result then
    begin
      Add(_NewName, pObjData^.Obj);
      FHashList.Delete(_OLDName);
    end;
end;

function THashObjectList.MakeName: SystemString;
begin
  repeat
    inc(FIncremental);
    Result := IntToStr(FIncremental);
  until not Exists(Result);
end;

function THashObjectList.MakeRefName(RefrenceName: SystemString): SystemString;
begin
  Result := RefrenceName;
  if not Exists(Result) then
      Exit;

  repeat
    inc(FIncremental);
    Result := RefrenceName + IntToStr(FIncremental);
  until not Exists(Result);
end;

function THashStringList.GetCount: NativeInt;
begin
  Result := FHashList.Count;
end;

function THashStringList.GetIgnoreCase: Boolean;
begin
  Result := FHashList.IgnoreCase;
end;

procedure THashStringList.SetIgnoreCase(const Value: Boolean);
begin
  FHashList.IgnoreCase := Value;
end;

function THashStringList.GetKeyValue(const Name: SystemString): SystemString;
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
      Result := pVarData^.V
  else
      Result := Null;
end;

procedure THashStringList.SetKeyValue(const Name: SystemString; const Value: SystemString);
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.NameValue[Name];

  if pVarData = nil then
    begin
      new(pVarData);
      pVarData^.OnChnage := nil;
      FHashList.Add(Name, pVarData, False);
      if Assigned(FOnValueChangeNotify) then
          FOnValueChangeNotify(Self, Name, '', Value);
    end
  else
    begin
      if Assigned(pVarData^.OnChnage) then
        begin
          try
              pVarData^.OnChnage(Self, Name, pVarData^.V, Value);
          except
          end;
        end;
      if Assigned(FOnValueChangeNotify) then
          FOnValueChangeNotify(Self, Name, pVarData^.V, Value);
    end;
  pVarData^.V := Value;
end;

function THashStringList.GetOnChange(const Name: SystemString): THashStringChangeEvent;
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
      Result := pVarData^.OnChnage
  else
      Result := nil;
end;

procedure THashStringList.SetOnChange(const Name: SystemString; const Value_: THashStringChangeEvent);
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData = nil then
    begin
      new(pVarData);
      pVarData^.V := Null;
      pVarData^.OnChnage := Value_;
      FHashList.Add(Name, pVarData, False);
    end
  else
      pVarData^.OnChnage := Value_;
end;

function THashStringList.GetAccessOptimization: Boolean;
begin
  Result := FHashList.AccessOptimization;
end;

procedure THashStringList.SetAccessOptimization(const Value: Boolean);
begin
  FHashList.AccessOptimization := Value;
end;

procedure THashStringList.DefaultDataFreeProc(p: Pointer);
begin
  Dispose(PHashStringListData(p));
end;

constructor THashStringList.Create;
begin
  CustomCreate(64);
end;

constructor THashStringList.CustomCreate(HashPoolSize_: Integer);
begin
  inherited Create;
  FHashList := THashList.CustomCreate(HashPoolSize_);
  FHashList.FAutoFreeData := True;

  FHashList.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FAutoUpdateDefaultValue := False;
  FOnValueChangeNotify := nil;
end;

destructor THashStringList.Destroy;
begin
  DisposeObject(FHashList);
  inherited Destroy;
end;

procedure THashStringList.Assign(sour: THashStringList);
var
  i: Integer;
  p: PHashListData;
begin
  Clear;
  if sour.HashList.Count > 0 then
    begin
      i := 0;
      p := sour.HashList.FirstPtr;
      while i < sour.HashList.Count do
        begin
          FastAdd(p^.OriginName, PHashStringListData(p^.Data)^.V);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashStringList.MergeTo(dest: THashStringList);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          dest.Add(p^.OriginName, PHashStringListData(p^.Data)^.V);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashStringList.ProgressC(const OnProgress: THashStringListLoop_C);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(Self, @p^.OriginName, PHashStringListData(p^.Data)^.V);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashStringList.ProgressM(const OnProgress: THashStringListLoop_M);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(Self, @p^.OriginName, PHashStringListData(p^.Data)^.V);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashStringList.ProgressP(const OnProgress: THashStringListLoop_P);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(Self, @p^.OriginName, PHashStringListData(p^.Data)^.V);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function THashStringList.FirstName: SystemString;
begin
  if HashList.Count > 0 then
      Result := HashList.FirstPtr^.OriginName
  else
      Result := '';
end;

function THashStringList.LastName: SystemString;
begin
  if HashList.Count > 0 then
      Result := HashList.LastPtr^.OriginName
  else
      Result := '';
end;

function THashStringList.FirstData: PHashStringListData;
begin
  if HashList.Count > 0 then
      Result := HashList.FirstPtr^.Data
  else
      Result := nil;
end;

function THashStringList.LastData: PHashStringListData;
begin
  if HashList.Count > 0 then
      Result := HashList.LastPtr^.Data
  else
      Result := nil;
end;

procedure THashStringList.Clear;
begin
  FHashList.Clear;
end;

procedure THashStringList.GetNameList(OutputList: TCore_Strings);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashStringList.GetNameList(OutputList: TListString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashStringList.GetNameList(OutputList: TListPascalString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashStringList.Delete(const Name: SystemString);
begin
  FHashList.Delete(Name);
end;

function THashStringList.Add(const Name: SystemString; V: SystemString): SystemString;
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
    begin
      try
        if Assigned(pVarData^.OnChnage) then
            pVarData^.OnChnage(Self, Name, pVarData^.V, V);
      except
      end;
    end
  else
    begin
      new(pVarData);
      pVarData^.OnChnage := nil;
      FHashList.Add(Name, pVarData, True);
    end;

  pVarData^.V := V;
  Result := V;
end;

function THashStringList.FastAdd(const Name: SystemString; V: SystemString): SystemString;
var
  pVarData: PHashStringListData;
begin
  new(pVarData);
  pVarData^.OnChnage := nil;
  FHashList.Add(Name, pVarData, False);

  pVarData^.V := V;
  Result := V;
end;

function THashStringList.Find(const Name: SystemString): SystemString;
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.Find(Name);
  if pVarData <> nil then
      Result := pVarData^.V
  else
      Result := Null;
end;

function THashStringList.FindValue(const Value_: SystemString): SystemString;
var
  i: Integer;
  lst: TCore_List;
  pVarData: PHashStringListData;
begin
  Result := '';
  lst := TCore_List.Create;
  FHashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        pVarData := PHashListData(lst[i])^.Data;
        if umlSameVarValue(Value_, pVarData^.V) then
          begin
            Result := PHashListData(lst[i])^.OriginName;
            Break;
          end;
      end;
  DisposeObject(lst);
end;

function THashStringList.Exists(const Name: SystemString): Boolean;
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData = nil then
      Result := False
  else
      Result := not VarIsEmpty(pVarData^.V);
end;

procedure THashStringList.CopyFrom(const Source: THashStringList);
var
  lst: TCore_List;
  pVarData: PHashStringListData;
  i: Integer;
begin
  lst := TCore_List.Create;
  Source.HashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            pVarData := Data;
            NameValue[OriginName] := pVarData^.V;
          end;
      end;
  DisposeObject(lst);
end;

function THashStringList.IncValue(const Name: SystemString; V: SystemString): SystemString;
var
  pVarData: PHashStringListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
    begin
      if pVarData^.V <> '' then
          Result := pVarData^.V + ',' + V;

      try
        if Assigned(pVarData^.OnChnage) then
            pVarData^.OnChnage(Self, Name, pVarData^.V, Result);
      except
      end;

      pVarData^.V := Result;
    end
  else
    begin
      Result := V;

      new(pVarData);
      pVarData^.OnChnage := nil;
      pVarData^.V := Result;
      FHashList.Add(Name, pVarData, True);
    end;
end;

procedure THashStringList.IncValue(const vl: THashStringList);
var
  lst: TCore_List;
  i: Integer;
  p: PHashListData;
begin
  lst := TCore_List.Create;
  vl.FHashList.GetListData(lst);
  for i := 0 to lst.Count - 1 do
    begin
      p := PHashListData(lst[i]);
      IncValue(p^.OriginName, PHashStringListData(p^.Data)^.V);
    end;
  DisposeObject(lst);
end;

function THashStringList.GetDefaultValue(const Name: SystemString; Value_: SystemString): SystemString;
var
  pVarData: PHashStringListData;
begin
  try
    if Name = '' then
      begin
        Result := Value_;
        Exit;
      end;
    pVarData := FHashList.NameValue[Name];
    if pVarData <> nil then
      begin
        if (VarIsNull(pVarData^.V)) or (VarIsEmpty(pVarData^.V)) or ((VarIsStr(pVarData^.V)) and (VarToStr(pVarData^.V) = '')) then
          begin
            Result := Value_;
            if FAutoUpdateDefaultValue then
                SetKeyValue(Name, Value_);
          end
        else
          begin
            Result := pVarData^.V;
          end;
      end
    else
      begin
        Result := Value_;
        if FAutoUpdateDefaultValue then
            SetKeyValue(Name, Value_);
      end;
  except
      Result := Value_;
  end;
end;

procedure THashStringList.SetDefaultValue(const Name: SystemString; Value_: SystemString);
begin
  SetKeyValue(Name, Value_);
end;

function THashStringList.ProcessMacro(const Text_, HeadToken, TailToken: SystemString; var Output_: SystemString): Boolean;
var
  sour: U_String;
  h, t: U_String;
  bPos, ePos: Integer;
  KeyText: SystemString;
  i: Integer;
begin
  Output_ := '';
  sour.Text := Text_;
  h.Text := HeadToken;
  t.Text := TailToken;
  Result := True;

  i := 1;

  while i <= sour.L do
    begin
      if sour.ComparePos(i, h) then
        begin
          bPos := i;
          ePos := sour.GetPos(t, i + h.L);
          if ePos > 0 then
            begin
              KeyText := sour.Copy(bPos + h.L, ePos - (bPos + h.L)).Text;

              if Exists(KeyText) then
                begin
                  Output_ := Output_ + GetKeyValue(KeyText);
                  i := ePos + t.L;
                  Continue;
                end
              else
                begin
                  Result := False;
                end;
            end;
        end;

      Output_ := Output_ + sour[i];
      inc(i);
    end;
end;

function THashStringList.Replace(const Text_: SystemString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): SystemString;
var
  arry: TArrayBatch;
  i: Integer;
  p: PHashListData;
begin
  SetLength(arry, Count);
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          arry[i].sour := p^.OriginName;
          arry[i].dest := PHashStringListData(p^.Data)^.V;
          inc(i);
          p := p^.Next;
        end;
    end;
  umlSortBatch(arry);
  Result := umlBatchReplace(Text_, arry, OnlyWord, IgnoreCase, bPos, ePos, nil);
  SetLength(arry, 0);
end;

function THashStringList.UReplace(const Text_: USystemString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): USystemString;
var
  arry: TU_ArrayBatch;
  i: Integer;
  p: PHashListData;
begin
  SetLength(arry, Count);
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          arry[i].sour := p^.OriginName;
          arry[i].dest := PHashStringListData(p^.Data)^.V;
          inc(i);
          p := p^.Next;
        end;
    end;
  U_SortBatch(arry);
  Result := U_BatchReplace(Text_, arry, OnlyWord, IgnoreCase, bPos, ePos, nil);
  SetLength(arry, 0);
end;

procedure THashStringList.LoadFromStream(stream: TCore_Stream);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.LoadFromStream(stream);
  DisposeObject(VT);
end;

procedure THashStringList.SaveToStream(stream: TCore_Stream);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.SaveToStream(stream);
  DisposeObject(VT);
end;

procedure THashStringList.LoadFromFile(FileName: SystemString);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.LoadFromFile(FileName);
  DisposeObject(VT);
end;

procedure THashStringList.SaveToFile(FileName: SystemString);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.SaveToFile(FileName);
  DisposeObject(VT);
end;

procedure THashStringList.ExportAsStrings(Output_: TListPascalString);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.DataExport(Output_);
  DisposeObject(VT);
end;

procedure THashStringList.ExportAsStrings(Output_: TCore_Strings);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.DataExport(Output_);
  DisposeObject(VT);
end;

procedure THashStringList.ImportFromStrings(input: TListPascalString);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.DataImport(input);
  DisposeObject(VT);
end;

procedure THashStringList.ImportFromStrings(input: TCore_Strings);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.DataImport(input);
  DisposeObject(VT);
end;

function THashStringList.GetAsText: SystemString;
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.SaveToText(Result);
  DisposeObject(VT);
end;

procedure THashStringList.SetAsText(const Value: SystemString);
var
  VT: THashStringTextStream;
begin
  VT := THashStringTextStream.Create(Self);
  VT.LoadFromText(Value);
  DisposeObject(VT);
end;

function THashStringTextStream.GetKeyValue(Name_: SystemString): SystemString;
begin
  if FStringList <> nil then
      Result := FStringList[Name_]
  else
      Result := Null;
end;

procedure THashStringTextStream.SetKeyValue(Name_: SystemString; const Value: SystemString);
begin
  if FStringList <> nil then
      FStringList[Name_] := Value;
end;

constructor THashStringTextStream.Create(_VList: THashStringList);
begin
  inherited Create;
  FStringList := _VList;
end;

destructor THashStringTextStream.Destroy;
begin
  inherited Destroy;
end;

procedure THashStringTextStream.Clear;
begin
  if FStringList <> nil then
      FStringList.Clear;
end;

class function THashStringTextStream.VToStr(const V: SystemString): SystemString;
var
  b64: TPascalString;
begin
  if umlExistsChar(V, #10#13#9#8#0) then
    begin
      umlEncodeLineBASE64(V, b64);
      Result := '___base64:' + b64.Text;
    end
  else
      Result := V;
end;

class function THashStringTextStream.StrToV(const S: SystemString): SystemString;
var
  n, body: U_String;
  V: Variant;
begin
  n := umlTrimSpace(S);
  try
    if n.ComparePos(1, '___base64:') then
      begin
        n := umlDeleteFirstStr(n, ':').Text;
        umlDecodeLineBASE64(n, body);
        Result := body.Text;
      end
    else if n.ComparePos(1, 'exp') and umlMultipleMatch([
        'expression(*)', 'expression[*]', 'expression<*>', 'expression"*"', 'expression'#39'*'#39,
        'exp(*)', 'exp[*]', 'exp<*>', 'exp"*"', 'exp'#39'*'#39,
        'expr(*)', 'expr[*]', 'expr<*>', 'expr"*"', 'expr'#39'*'#39,
        'express(*)', 'express[*]', 'express<*>', 'express"*"', 'exp'#39'*'#39
        ], n) then
      begin
        body := umlDeleteFirstStr_Discontinuity(n, '([<"'#39);
        body.DeleteLast;
        V := EvaluateExpressionValue(False, body);
        if VarIsNull(V) then
            Result := n
        else
            Result := VarToStr(V);
      end
    else if n.ComparePos(1, 'e') and umlMultipleMatch(['e(*)', 'e[*]', 'e<*>', 'e"*"', 'e'#39'*'#39], n) then
      begin
        body := n;
        body := umlDeleteFirstStr_Discontinuity(n, '([<"'#39);
        body.DeleteLast;
        V := EvaluateExpressionValue(False, body);
        if VarIsNull(V) then
            Result := n
        else
            Result := VarToStr(V);
      end
    else
      begin
        Result := n.Text;
      end;
  except
      Result := n.Text;
  end;
end;

procedure THashStringTextStream.DataImport(TextList: TListPascalString);
var
  i: Integer;
  n: TPascalString;
  TextName, TextValue: TPascalString;
begin
  if FStringList = nil then
      Exit;
  if TextList.Count > 0 then
    for i := 0 to TextList.Count - 1 do
      begin
        n := TextList[i].TrimChar(#32);

        if ((n.Exists(':')) or (n.Exists('='))) and (not CharIn(n.First, [':', '='])) then
          begin
            TextName := umlGetFirstStr_Discontinuity(n, ':=');
            if TextName.L > 0 then
              begin
                TextValue := umlDeleteFirstStr_Discontinuity(n, ':=');
                FStringList[TextName.Text] := StrToV(TextValue.Text);
              end
            else
                FStringList[n.Text] := '';
          end
        else
          begin
            FStringList[n.Text] := '';
          end;
      end;
end;

procedure THashStringTextStream.DataImport(TextList: TCore_Strings);
var
  ns: TListPascalString;
begin
  ns := TListPascalString.Create;
  ns.Assign(TextList);
  DataImport(ns);
  DisposeObject(ns);
end;

procedure THashStringTextStream.DataExport(TextList: TListPascalString);
var
  i: Integer;
  vl: TCore_List;
  TextValue: SystemString;
begin
  if FStringList = nil then
      Exit;
  vl := TCore_List.Create;
  FStringList.HashList.GetListData(vl);
  if vl.Count > 0 then
    for i := 0 to vl.Count - 1 do
      begin
        TextValue := VToStr(PHashStringListData(PHashListData(vl[i])^.Data)^.V);

        if TextValue <> '' then
            TextList.Add((PHashListData(vl[i])^.OriginName + '=' + TextValue))
        else
            TextList.Add(PHashListData(vl[i])^.OriginName);
      end;
  DisposeObject(vl);
end;

procedure THashStringTextStream.DataExport(TextList: TCore_Strings);
var
  ns: TListPascalString;
begin
  ns := TListPascalString.Create;
  DataExport(ns);
  ns.AssignTo(TextList);
  DisposeObject(ns);
end;

procedure THashStringTextStream.LoadFromStream(stream: TCore_Stream);
var
  n: TListPascalString;
begin
  if FStringList = nil then
      Exit;
  n := TListPascalString.Create;
  n.LoadFromStream(stream);
  DataImport(n);
  DisposeObject(n);
end;

procedure THashStringTextStream.SaveToStream(stream: TCore_Stream);
var
  n: TListPascalString;
begin
  if FStringList = nil then
      Exit;
  n := TListPascalString.Create;
  DataExport(n);
  n.SaveToStream(stream);
  DisposeObject(n);
end;

procedure THashStringTextStream.LoadFromFile(FileName: SystemString);
var
  ns: TCore_Stream;
begin
  ns := TCore_FileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
      LoadFromStream(ns);
  finally
      DisposeObject(ns);
  end;
end;

procedure THashStringTextStream.SaveToFile(FileName: SystemString);
var
  ns: TCore_Stream;
begin
  ns := TCore_FileStream.Create(FileName, fmCreate);
  try
      SaveToStream(ns);
  finally
      DisposeObject(ns);
  end;
end;

procedure THashStringTextStream.LoadFromText(Text_: SystemString);
var
  n: TListPascalString;
begin
  if FStringList = nil then
      Exit;
  n := TListPascalString.Create;
  n.AsText := Text_;
  DataImport(n);
  DisposeObject(n);
end;

procedure THashStringTextStream.SaveToText(var Text_: SystemString);
var
  n: TListPascalString;
begin
  if FStringList = nil then
      Exit;
  n := TListPascalString.Create;
  DataExport(n);
  Text_ := n.AsText;
  DisposeObject(n);
end;

function THashStringTextStream.Text: SystemString;
begin
  SaveToText(Result);
end;

function THashVariantList.GetCount: NativeInt;
begin
  Result := FHashList.Count;
end;

function THashVariantList.GetIgnoreCase: Boolean;
begin
  Result := FHashList.IgnoreCase;
end;

procedure THashVariantList.SetIgnoreCase(const Value: Boolean);
begin
  FHashList.IgnoreCase := Value;
end;

function THashVariantList.GetKeyValue(const Name: SystemString): Variant;
var
  pVarData: PHashVariantListData;
begin
  if Name = '' then
    begin
      Result := Null;
      Exit;
    end;
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
      Result := pVarData^.V
  else
      Result := Null;
end;

procedure THashVariantList.SetKeyValue(const Name: SystemString; const Value: Variant);
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.NameValue[Name];

  if pVarData = nil then
    begin
      new(pVarData);
      pVarData^.OnChnage := nil;
      FHashList.Add(Name, pVarData, False);
      if Assigned(FOnValueChangeNotify) then
          FOnValueChangeNotify(Self, Name, Null, Value);
    end
  else
    begin
      if Assigned(pVarData^.OnChnage) then
        begin
          try
              pVarData^.OnChnage(Self, Name, pVarData^.V, Value);
          except
          end;
        end;
      if Assigned(FOnValueChangeNotify) then
          FOnValueChangeNotify(Self, Name, pVarData^.V, Value);
    end;
  pVarData^.V := Value;
end;

function THashVariantList.GetOnChange(const Name: SystemString): THashVariantChangeEvent;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
      Result := pVarData^.OnChnage
  else
      Result := nil;
end;

procedure THashVariantList.SetOnChange(const Name: SystemString; const Value_: THashVariantChangeEvent);
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData = nil then
    begin
      new(pVarData);
      pVarData^.V := Null;
      pVarData^.OnChnage := Value_;
      FHashList.Add(Name, pVarData, False);
    end
  else
      pVarData^.OnChnage := Value_;
end;

function THashVariantList.GetAccessOptimization: Boolean;
begin
  Result := FHashList.AccessOptimization;
end;

procedure THashVariantList.SetAccessOptimization(const Value: Boolean);
begin
  FHashList.AccessOptimization := Value;
end;

procedure THashVariantList.DefaultDataFreeProc(p: Pointer);
begin
  Dispose(PHashVariantListData(p));
end;

function THashVariantList.GetI64(const Name: SystemString): Int64;
var
  V: Variant;
begin
  V := GetDefaultValue(Name, 0);
  if VarIsOrdinal(V) then
      Result := V
  else
      Result := 0;
end;

procedure THashVariantList.SetI64(const Name: SystemString; const Value: Int64);
begin
  SetDefaultValue(Name, Value);
end;

function THashVariantList.GetI32(const Name: SystemString): Integer;
var
  V: Variant;
begin
  V := GetDefaultValue(Name, 0);
  if VarIsOrdinal(V) then
      Result := V
  else
      Result := 0;
end;

procedure THashVariantList.SetI32(const Name: SystemString; const Value: Integer);
begin
  SetDefaultValue(Name, Value);
end;

function THashVariantList.GetF(const Name: SystemString): Double;
var
  V: Variant;
begin
  V := GetDefaultValue(Name, 0);
  if VarIsFloat(V) then
      Result := V
  else
      Result := 0;
end;

procedure THashVariantList.SetF(const Name: SystemString; const Value: Double);
begin
  SetDefaultValue(Name, Value);
end;

function THashVariantList.GetS(const Name: SystemString): SystemString;
begin
  Result := VarToStr(GetDefaultValue(Name, ''));
end;

procedure THashVariantList.SetS(const Name, Value: SystemString);
begin
  SetDefaultValue(Name, Value);
end;

constructor THashVariantList.Create;
begin
  CustomCreate(64);
end;

constructor THashVariantList.CustomCreate(HashPoolSize_: Integer);
begin
  inherited Create;
  FHashList := THashList.CustomCreate(HashPoolSize_);
  FHashList.FAutoFreeData := True;

  FHashList.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}DefaultDataFreeProc;
  FAutoUpdateDefaultValue := False;
  FOnValueChangeNotify := nil;
end;

destructor THashVariantList.Destroy;
begin
  DisposeObject(FHashList);
  inherited Destroy;
end;

procedure THashVariantList.Assign(sour: THashVariantList);
var
  i: Integer;
  p: PHashListData;
begin
  Clear;
  if sour.HashList.Count > 0 then
    begin
      i := 0;
      p := sour.HashList.FirstPtr;
      while i < sour.HashList.Count do
        begin
          FastAdd(p^.OriginName, PHashVariantListData(p^.Data)^.V);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashVariantList.ProgressC(const OnProgress: THashVariantListLoop_C);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(Self, @p^.OriginName, PHashVariantListData(p^.Data)^.V);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashVariantList.ProgressM(const OnProgress: THashVariantListLoop_M);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(Self, @p^.OriginName, PHashVariantListData(p^.Data)^.V);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashVariantList.ProgressP(const OnProgress: THashVariantListLoop_P);
var
  i: Integer;
  p: PHashListData;
begin
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          try
              OnProgress(Self, @p^.OriginName, PHashVariantListData(p^.Data)^.V);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function THashVariantList.FirstName: SystemString;
begin
  if HashList.Count > 0 then
      Result := HashList.FirstPtr^.OriginName
  else
      Result := '';
end;

function THashVariantList.LastName: SystemString;
begin
  if HashList.Count > 0 then
      Result := HashList.LastPtr^.OriginName
  else
      Result := '';
end;

function THashVariantList.FirstData: PHashVariantListData;
begin
  if HashList.Count > 0 then
      Result := HashList.FirstPtr^.Data
  else
      Result := nil;
end;

function THashVariantList.LastData: PHashVariantListData;
begin
  if HashList.Count > 0 then
      Result := HashList.LastPtr^.Data
  else
      Result := nil;
end;

procedure THashVariantList.Clear;
begin
  FHashList.Clear;
end;

procedure THashVariantList.GetNameList(OutputList: TCore_Strings);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashVariantList.GetNameList(OutputList: TListString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashVariantList.GetNameList(OutputList: TListPascalString);
var
  i: Integer;
  p: PHashListData;
begin
  OutputList.Clear;
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          OutputList.Add(p^.OriginName);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure THashVariantList.Delete(const Name: SystemString);
begin
  FHashList.Delete(Name);
end;

function THashVariantList.Add(const Name: SystemString; V: Variant): Variant;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
    begin
      try
        if Assigned(pVarData^.OnChnage) then
            pVarData^.OnChnage(Self, Name, pVarData^.V, V);
      except
      end;
    end
  else
    begin
      new(pVarData);
      pVarData^.OnChnage := nil;
      FHashList.Add(Name, pVarData, True);
    end;

  pVarData^.V := V;
  Result := V;
end;

function THashVariantList.FastAdd(const Name: SystemString; V: Variant): Variant;
var
  pVarData: PHashVariantListData;
begin
  new(pVarData);
  pVarData^.OnChnage := nil;
  FHashList.Add(Name, pVarData, False);

  pVarData^.V := V;
  Result := V;
end;

function THashVariantList.Find(const Name: SystemString): Variant;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.Find(Name);
  if pVarData <> nil then
      Result := pVarData^.V
  else
      Result := Null;
end;

function THashVariantList.FindValue(const Value_: Variant): SystemString;
var
  i: Integer;
  lst: TCore_List;
  pVarData: PHashVariantListData;
begin
  Result := '';
  lst := TCore_List.Create;
  FHashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        pVarData := PHashListData(lst[i])^.Data;
        if umlSameVarValue(Value_, pVarData^.V) then
          begin
            Result := PHashListData(lst[i])^.OriginName;
            Break;
          end;
      end;
  DisposeObject(lst);
end;

function THashVariantList.Exists(const Name: SystemString): Boolean;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData = nil then
      Result := False
  else
      Result := not VarIsEmpty(pVarData^.V);
end;

procedure THashVariantList.CopyFrom(const Source: THashVariantList);
var
  lst: TCore_List;
  pVarData: PHashVariantListData;
  i: Integer;
begin
  lst := TCore_List.Create;
  Source.HashList.GetListData(lst);
  if lst.Count > 0 then
    for i := 0 to lst.Count - 1 do
      begin
        with PHashListData(lst[i])^ do
          begin
            pVarData := Data;
            NameValue[OriginName] := pVarData^.V;
          end;
      end;
  DisposeObject(lst);
end;

function THashVariantList.GetType(const Name: SystemString): Word;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.Find(Name);
  if pVarData = nil then
      Result := varEmpty
  else
      Result := VarType(pVarData^.V);
end;

function THashVariantList.IncValue(const Name: SystemString; V: Variant): Variant;
var
  pVarData: PHashVariantListData;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
    begin
      if VarIsStr(pVarData^.V) and VarIsStr(V) then
        begin
          if VarToStr(pVarData^.V) <> '' then
              Result := VarToStr(pVarData^.V) + ',' + VarToStr(V)
          else
              Result := VarToStr(pVarData^.V) + VarToStr(V);
        end
      else
        begin
          try
              Result := pVarData^.V + V;
          except
              Result := VarToStr(pVarData^.V) + VarToStr(V);
          end;
        end;

      try
        if Assigned(pVarData^.OnChnage) then
            pVarData^.OnChnage(Self, Name, pVarData^.V, Result);
      except
      end;

      pVarData^.V := Result;
    end
  else
    begin
      Result := V;

      new(pVarData);
      pVarData^.OnChnage := nil;
      pVarData^.V := Result;
      FHashList.Add(Name, pVarData, True);
    end;
end;

procedure THashVariantList.IncValue(const vl: THashVariantList);
var
  lst: TCore_List;
  i: Integer;
  p: PHashListData;
begin
  lst := TCore_List.Create;
  vl.FHashList.GetListData(lst);
  for i := 0 to lst.Count - 1 do
    begin
      p := PHashListData(lst[i]);
      IncValue(p^.OriginName, PHashVariantListData(p^.Data)^.V);
    end;
  DisposeObject(lst);
end;

function THashVariantList.SetMax(const Name: SystemString; V: Variant): Variant;
var
  pVarData: PHashVariantListData;
  r: Boolean;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
    begin
      try
          r := V > pVarData^.V;
      except
          r := True;
      end;

      if r then
        begin
          Result := V;
          try
            if Assigned(pVarData^.OnChnage) then
                pVarData^.OnChnage(Self, Name, pVarData^.V, Result);
          except
          end;

          pVarData^.V := Result;
        end;
    end
  else
    begin
      Result := V;

      new(pVarData);
      pVarData^.OnChnage := nil;
      pVarData^.V := Result;
      FHashList.Add(Name, pVarData, True);
    end;
end;

procedure THashVariantList.SetMax(const vl: THashVariantList);
var
  lst: TCore_List;
  i: Integer;
  p: PHashListData;
begin
  lst := TCore_List.Create;
  vl.FHashList.GetListData(lst);
  for i := 0 to lst.Count - 1 do
    begin
      p := PHashListData(lst[i]);
      SetMax(p^.OriginName, PHashVariantListData(p^.Data)^.V);
    end;
  DisposeObject(lst);
end;

function THashVariantList.SetMin(const Name: SystemString; V: Variant): Variant;
var
  pVarData: PHashVariantListData;
  r: Boolean;
begin
  pVarData := FHashList.NameValue[Name];
  if pVarData <> nil then
    begin
      try
          r := V < pVarData^.V;
      except
          r := True;
      end;

      if r then
        begin
          Result := V;
          try
            if Assigned(pVarData^.OnChnage) then
                pVarData^.OnChnage(Self, Name, pVarData^.V, Result);
          except
          end;

          pVarData^.V := Result;
        end;
    end
  else
    begin
      Result := V;

      new(pVarData);
      pVarData^.OnChnage := nil;
      pVarData^.V := Result;
      FHashList.Add(Name, pVarData, True);
    end;
end;

procedure THashVariantList.SetMin(const vl: THashVariantList);
var
  lst: TCore_List;
  i: Integer;
  p: PHashListData;
begin
  lst := TCore_List.Create;
  vl.FHashList.GetListData(lst);
  for i := 0 to lst.Count - 1 do
    begin
      p := PHashListData(lst[i]);
      SetMin(p^.OriginName, PHashVariantListData(p^.Data)^.V);
    end;
  DisposeObject(lst);
end;

function THashVariantList.GetDefaultValue(const Name: SystemString; Value_: Variant): Variant;
var
  pVarData: PHashVariantListData;
begin
  try
    if Name = '' then
      begin
        Result := Value_;
        Exit;
      end;
    pVarData := FHashList.NameValue[Name];
    if pVarData <> nil then
      begin
        if (VarIsNull(pVarData^.V)) or (VarIsEmpty(pVarData^.V)) or ((VarIsStr(pVarData^.V)) and (VarToStr(pVarData^.V) = '')) then
          begin
            Result := Value_;
            if FAutoUpdateDefaultValue then
                SetKeyValue(Name, Value_);
          end
        else
          begin
            Result := pVarData^.V;
          end;
      end
    else
      begin
        Result := Value_;
        if FAutoUpdateDefaultValue then
            SetKeyValue(Name, Value_);
      end;
  except
      Result := Value_;
  end;
end;

procedure THashVariantList.SetDefaultValue(const Name: SystemString; Value_: Variant);
begin
  SetKeyValue(Name, Value_);
end;

function THashVariantList.ProcessMacro(const Text_, HeadToken, TailToken: SystemString; var Output_: SystemString): Boolean;
var
  sour: U_String;
  h, t: U_String;
  bPos, ePos: Integer;
  KeyText: SystemString;
  i: Integer;
begin
  Output_ := '';
  sour.Text := Text_;
  h.Text := HeadToken;
  t.Text := TailToken;
  Result := True;

  i := 1;

  while i <= sour.L do
    begin
      if sour.ComparePos(i, h) then
        begin
          bPos := i;
          ePos := sour.GetPos(t, i + h.L);
          if ePos > 0 then
            begin
              KeyText := sour.Copy(bPos + h.L, ePos - (bPos + h.L)).Text;

              if Exists(KeyText) then
                begin
                  Output_ := Output_ + VarToStr(GetKeyValue(KeyText));
                  i := ePos + t.L;
                  Continue;
                end
              else
                begin
                  Result := False;
                end;
            end;
        end;

      Output_ := Output_ + sour[i];
      inc(i);
    end;
end;

function THashVariantList.Replace(const Text_: SystemString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): SystemString;
var
  arry: TArrayBatch;
  i: Integer;
  p: PHashListData;
begin
  SetLength(arry, Count);
  if HashList.Count > 0 then
    begin
      i := 0;
      p := HashList.FirstPtr;
      while i < HashList.Count do
        begin
          arry[i].sour := p^.OriginName;
          arry[i].dest := VarToStr(PHashVariantListData(p^.Data)^.V);
          inc(i);
          p := p^.Next;
        end;
    end;
  umlSortBatch(arry);
  Result := umlBatchReplace(Text_, arry, OnlyWord, IgnoreCase, bPos, ePos, nil);
  SetLength(arry, 0);
end;

procedure THashVariantList.LoadFromStream(stream: TCore_Stream);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.LoadFromStream(stream);
  DisposeObject(VT);
end;

procedure THashVariantList.SaveToStream(stream: TCore_Stream);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.SaveToStream(stream);
  DisposeObject(VT);
end;

procedure THashVariantList.LoadFromFile(FileName: SystemString);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.LoadFromFile(FileName);
  DisposeObject(VT);
end;

procedure THashVariantList.SaveToFile(FileName: SystemString);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.SaveToFile(FileName);
  DisposeObject(VT);
end;

procedure THashVariantList.ExportAsStrings(Output_: TListPascalString);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.DataExport(Output_);
  DisposeObject(VT);
end;

procedure THashVariantList.ExportAsStrings(Output_: TCore_Strings);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.DataExport(Output_);
  DisposeObject(VT);
end;

procedure THashVariantList.ImportFromStrings(input: TListPascalString);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.DataImport(input);
  DisposeObject(VT);
end;

procedure THashVariantList.ImportFromStrings(input: TCore_Strings);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.DataImport(input);
  DisposeObject(VT);
end;

function THashVariantList.GetAsText: SystemString;
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.SaveToText(Result);
  DisposeObject(VT);
end;

procedure THashVariantList.SetAsText(const Value: SystemString);
var
  VT: THashVariantTextStream;
begin
  VT := THashVariantTextStream.Create(Self);
  VT.LoadFromText(Value);
  DisposeObject(VT);
end;

function THashVariantTextStream.GetKeyValue(Name_: SystemString): Variant;
begin
  if FVariantList <> nil then
      Result := FVariantList[Name_]
  else
      Result := Null;
end;

procedure THashVariantTextStream.SetKeyValue(Name_: SystemString; const Value: Variant);
begin
  if FVariantList <> nil then
      FVariantList[Name_] := Value;
end;

constructor THashVariantTextStream.Create(_VList: THashVariantList);
begin
  inherited Create;
  FVariantList := _VList;
end;

destructor THashVariantTextStream.Destroy;
begin
  inherited Destroy;
end;

procedure THashVariantTextStream.Clear;
begin
  if FVariantList <> nil then
      FVariantList.Clear;
end;

class function THashVariantTextStream.VToStr(const V: Variant): SystemString;
var
  n, b64: U_String;
begin
  try
    case VarType(V) of
      varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord:
        begin
          Result := IntToStr(V);
        end;
      varInt64:
        begin
          Result := IntToStr(Int64(V));
        end;
      varUInt64:
        begin
{$IFDEF FPC}
          Result := IntToStr(UInt64(V));
{$ELSE}
          Result := UIntToStr(UInt64(V));
{$ENDIF}
        end;
      varSingle, varDouble, varCurrency, varDate:
        begin
          Result := FloatToStr(V);
        end;
      varOleStr, varString, varUString:
        begin
          n.Text := VarToStr(V);

          if umlExistsChar(n, #10#13#9#8#0) then
            begin
              umlEncodeLineBASE64(n, b64);
              Result := '___base64:' + b64.Text;
            end
          else
              Result := n.Text;
        end;
      varBoolean:
        begin
          Result := BoolToStr(V, True);
        end;
      else
        Result := VarToStr(V);
    end;
  except
    try
        Result := VarToStr(V);
    except
        Result := '';
    end;
  end;
end;

class function THashVariantTextStream.StrToV(const S: SystemString): Variant;
var
  n, body: U_String;
  V: Variant;
begin
  n := umlTrimSpace(S);
  try
    if n.ComparePos(1, '___base64:') then
      begin
        n := umlDeleteFirstStr(n, ':').Text;
        umlDecodeLineBASE64(n, body);
        Result := body.Text;
      end
    else if n.ComparePos(1, 'exp') and umlMultipleMatch([
        'expression(*)', 'expression[*]', 'expression<*>', 'expression"*"', 'expression'#39'*'#39,
        'exp(*)', 'exp[*]', 'exp<*>', 'exp"*"', 'exp'#39'*'#39,
        'expr(*)', 'expr[*]', 'expr<*>', 'expr"*"', 'expr'#39'*'#39,
        'express(*)', 'express[*]', 'express<*>', 'express"*"', 'exp'#39'*'#39
        ], n) then
      begin
        body := umlDeleteFirstStr_Discontinuity(n, '([<"'#39);
        body.DeleteLast;
        V := EvaluateExpressionValue(False, body);
        if VarIsNull(V) then
            Result := n
        else
            Result := VarToStr(V);
      end
    else if n.ComparePos(1, 'e') and umlMultipleMatch(['e(*)', 'e[*]', 'e<*>', 'e"*"', 'e'#39'*'#39], n) then
      begin
        body := n;
        body := umlDeleteFirstStr_Discontinuity(n, '([<"'#39);
        body.DeleteLast;
        V := EvaluateExpressionValue(False, body);
        if VarIsNull(V) then
            Result := n
        else
            Result := VarToStr(V);
      end
    else
      begin
        case umlGetNumTextType(n) of
          ntBool: Result := StrToBool(n.Text);
          ntInt: Result := StrToInt(n.Text);
          ntInt64: Result := StrToInt64(n.Text);
{$IFDEF FPC}
          ntUInt64: Result := StrToQWord(n.Text);
{$ELSE}
          ntUInt64: Result := StrToUInt64(n.Text);
{$ENDIF}
          ntWord: Result := StrToInt(n.Text);
          ntByte: Result := StrToInt(n.Text);
          ntSmallInt: Result := StrToInt(n.Text);
          ntShortInt: Result := StrToInt(n.Text);
          ntUInt: Result := StrToInt(n.Text);
          ntSingle: Result := StrToFloat(n.Text);
          ntDouble: Result := StrToFloat(n.Text);
          ntCurrency: Result := StrToFloat(n.Text);
          else Result := n.Text;
        end;
      end;
  except
      Result := n.Text;
  end;
end;

procedure THashVariantTextStream.DataImport(TextList: TListPascalString);
var
  i: Integer;
  n: TPascalString;
  TextName, TextValue: TPascalString;
begin
  if FVariantList = nil then
      Exit;
  if TextList.Count > 0 then
    for i := 0 to TextList.Count - 1 do
      begin
        n := TextList[i].TrimChar(#32);

        if ((n.Exists(':')) or (n.Exists('='))) and (not CharIn(n.First, [':', '='])) then
          begin
            TextName := umlGetFirstStr_Discontinuity(n, ':=');
            if TextName.L > 0 then
              begin
                TextValue := umlDeleteFirstStr_Discontinuity(n, ':=');
                FVariantList[TextName.Text] := StrToV(TextValue.Text);
              end
            else
                FVariantList[n.Text] := '';
          end
        else
          begin
            FVariantList[n.Text] := '';
          end;
      end;
end;

procedure THashVariantTextStream.DataImport(TextList: TCore_Strings);
var
  ns: TListPascalString;
begin
  ns := TListPascalString.Create;
  ns.Assign(TextList);
  DataImport(ns);
  DisposeObject(ns);
end;

procedure THashVariantTextStream.DataExport(TextList: TListPascalString);
var
  i: Integer;
  vl: TCore_List;
  TextValue: SystemString;
begin
  if FVariantList = nil then
      Exit;
  vl := TCore_List.Create;
  FVariantList.HashList.GetListData(vl);
  if vl.Count > 0 then
    for i := 0 to vl.Count - 1 do
      begin
        TextValue := VToStr(PHashVariantListData(PHashListData(vl[i])^.Data)^.V);

        if TextValue <> '' then
            TextList.Add((PHashListData(vl[i])^.OriginName + '=' + TextValue))
        else
            TextList.Add(PHashListData(vl[i])^.OriginName);
      end;
  DisposeObject(vl);
end;

procedure THashVariantTextStream.DataExport(TextList: TCore_Strings);
var
  ns: TListPascalString;
begin
  ns := TListPascalString.Create;
  DataExport(ns);
  ns.AssignTo(TextList);
  DisposeObject(ns);
end;

procedure THashVariantTextStream.LoadFromStream(stream: TCore_Stream);
var
  n: TListPascalString;
begin
  if FVariantList = nil then
      Exit;
  n := TListPascalString.Create;
  n.LoadFromStream(stream);
  DataImport(n);
  DisposeObject(n);
end;

procedure THashVariantTextStream.SaveToStream(stream: TCore_Stream);
var
  n: TListPascalString;
begin
  if FVariantList = nil then
      Exit;
  n := TListPascalString.Create;
  DataExport(n);
  n.SaveToStream(stream);
  DisposeObject(n);
end;

procedure THashVariantTextStream.LoadFromFile(FileName: SystemString);
var
  ns: TCore_Stream;
begin
  ns := TCore_FileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
      LoadFromStream(ns);
  finally
      DisposeObject(ns);
  end;
end;

procedure THashVariantTextStream.SaveToFile(FileName: SystemString);
var
  ns: TCore_Stream;
begin
  ns := TCore_FileStream.Create(FileName, fmCreate);
  try
      SaveToStream(ns);
  finally
      DisposeObject(ns);
  end;
end;

procedure THashVariantTextStream.LoadFromText(Text_: SystemString);
var
  n: TListPascalString;
begin
  if FVariantList = nil then
      Exit;
  n := TListPascalString.Create;
  n.AsText := Text_;
  DataImport(n);
  DisposeObject(n);
end;

procedure THashVariantTextStream.SaveToText(var Text_: SystemString);
var
  n: TListPascalString;
begin
  if FVariantList = nil then
      Exit;
  n := TListPascalString.Create;
  DataExport(n);
  Text_ := n.AsText;
  DisposeObject(n);
end;

function THashVariantTextStream.Text: SystemString;
begin
  SaveToText(Result);
end;

function THashVariantTextStream.GetValue(Name_: SystemString; V: Variant): Variant;
begin
  Result := NameValue[Name_];
  if VarIsNull(Result) then
    begin
      NameValue[Name_] := V;
      Result := V;
    end;
end;

function TListString.GetItems(idx: Integer): SystemString;
begin
  Result := PListStringData(FList[idx])^.Data;
end;

procedure TListString.SetItems(idx: Integer; Value: SystemString);
begin
  with PListStringData(FList[idx])^ do
    begin
      Data := Value;
      hash := MakeHashS(@Value);
    end;
end;

function TListString.GetObjects(idx: Integer): TCore_Object;
begin
  Result := PListStringData(FList[idx])^.Obj;
end;

procedure TListString.SetObjects(idx: Integer; Value: TCore_Object);
begin
  PListStringData(FList[idx])^.Obj := Value;
end;

constructor TListString.Create;
begin
  inherited Create;
  FList := TCore_List.Create;
end;

destructor TListString.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TListString.Add(Value: SystemString): Integer;
var
  p: PListStringData;
begin
  new(p);
  p^.Data := Value;
  p^.Obj := nil;
  p^.hash := MakeHashS(@Value);
  Result := FList.Add(p);
end;

function TListString.Add(Value: SystemString; Obj: TCore_Object): Integer;
var
  p: PListStringData;
begin
  new(p);
  p^.Data := Value;
  p^.Obj := Obj;
  p^.hash := MakeHashS(@Value);
  Result := FList.Add(p);
end;

function TListString.Delete(idx: Integer): Integer;
var
  p: PListStringData;
begin
  p := FList[idx];
  p^.Data := '';
  Dispose(p);
  FList.Delete(idx);
  Result := Count;
end;

function TListString.DeleteString(Value: SystemString): Integer;
var
  i: Integer;
  h: THash;
begin
  i := 0;
  h := MakeHashS(@Value);

  while i < Count do
    begin
      if (PListStringData(FList[i])^.hash = h) and (SameText(PListStringData(FList[i])^.Data, Value)) then
          Delete(i)
      else
          inc(i);
    end;
  Result := Count;
end;

procedure TListString.Clear;
var
  i: Integer;
  p: PListStringData;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := PListStringData(FList[i]);
      p^.Data := '';
      Dispose(p);
    end;
  FList.Clear;
end;

function TListString.Count: Integer;
begin
  Result := FList.Count;
end;

function TListString.ExistsValue(Value: SystemString): Integer;
var
  i: Integer;
  h: THash;
begin
  h := MakeHashS(@Value);

  Result := -1;

  for i := 0 to Count - 1 do
    if (PListStringData(FList[i])^.hash = h) and (SameText(PListStringData(FList[i])^.Data, Value)) then
      begin
        Result := i;
        Break;
      end;
end;

procedure TListString.Assign(SameObj: TListString);
var
  i: Integer;
  P1, P2: PListStringData;
begin
  Clear;
  for i := 0 to SameObj.Count - 1 do
    begin
      P2 := PListStringData(SameObj.FList[i]);
      new(P1);
      P1^ := P2^;
      FList.Add(P1);
    end;
end;

procedure TListString.LoadFromStream(stream: TCore_Stream);
var
  bp: Int64;
  r: TStreamReader;
begin
  Clear;
  bp := stream.Position;
{$IFDEF FPC}
  r := TStreamReader.Create(stream);
  while not r.Eof do
      Add(r.ReadLine);
{$ELSE FPC}
  r := TStreamReader.Create(stream, TEncoding.UTF8);
  try
    while not r.EndOfStream do
        Add(r.ReadLine);
  except
    Clear;
    DisposeObject(r);
    stream.Position := bp;
    r := TStreamReader.Create(stream, TEncoding.ANSI);
    while not r.EndOfStream do
        Add(r.ReadLine);
  end;
{$ENDIF FPC}
  DisposeObject(r);
end;

procedure TListString.SaveToStream(stream: TCore_Stream);
var
  i: Integer;
  n: TPascalString;
  b: TBytes;
begin
  for i := 0 to FList.Count - 1 do
    begin
      n.Text := PListStringData(FList[i])^.Data + #13#10;
      b := n.Bytes;
      stream.write(b[0], Length(b));
      n := '';
    end;
end;

procedure TListString.LoadFromFile(fn: SystemString);
var
  fs: TCore_FileStream;
begin
  fs := TCore_FileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TListString.SaveToFile(fn: SystemString);
var
  fs: TCore_FileStream;
begin
  fs := TCore_FileStream.Create(fn, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

function TListPascalString.GetText: SystemString;
var
  i: Integer;
begin
  Result := '';
  if Count > 0 then
    begin
      Result := Items[0];
      for i := 1 to Count - 1 do
          Result := Result + #13#10 + Items[i];
    end;
end;

procedure TListPascalString.SetText(const Value: SystemString);
var
  n: TPascalString;
  buff_: TBytes;
  m64: TMS64;
begin
  n.Text := Value;
  buff_ := n.Bytes;
  n := '';
  m64 := TMS64.Create;
  if Length(buff_) > 0 then
      m64.SetPointerWithProtectedMode(@buff_[0], Length(buff_));
  LoadFromStream(m64);
  DisposeObject(m64);
  SetLength(buff_, 0);
end;

function TListPascalString.GetItems(idx: Integer): TPascalString;
begin
  Result := FList[idx]^.Data;
end;

procedure TListPascalString.SetItems(idx: Integer; Value: TPascalString);
begin
  with FList[idx]^ do
    begin
      Data := Value;
      hash := MakeHashPas(@Value);
    end;
end;

function TListPascalString.GetItems_PPascalString(idx: Integer): PPascalString;
begin
  Result := @FList[idx]^.Data;
end;

function TListPascalString.GetObjects(idx: Integer): TCore_Object;
begin
  Result := FList[idx]^.Obj;
end;

procedure TListPascalString.SetObjects(idx: Integer; Value: TCore_Object);
begin
  FList[idx]^.Obj := Value;
end;

constructor TListPascalString.Create;
begin
  inherited Create;
  FList := TListPascalStringData_List.Create;
end;

destructor TListPascalString.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TListPascalString.Add(const Fmt: SystemString; const Args: array of const): Integer;
begin
  Result := Add(PFormat(Fmt, Args));
end;

function TListPascalString.Add(Value: SystemString): Integer;
var
  p: PListPascalStringData;
begin
  new(p);
  p^.Data.Text := Value;
  p^.Obj := nil;
  p^.hash := MakeHashPas(@p^.Data);
  Result := FList.Add(p);
end;

function TListPascalString.Add(Value: TPascalString): Integer;
var
  p: PListPascalStringData;
begin
  new(p);
  p^.Data := Value;
  p^.Obj := nil;
  p^.hash := MakeHashPas(@p^.Data);
  Result := FList.Add(p);
end;

function TListPascalString.Add(Value: TUPascalString): Integer;
var
  p: PListPascalStringData;
begin
  new(p);
  p^.Data.Text := Value.Text;
  p^.Obj := nil;
  p^.hash := MakeHashPas(@p^.Data);
  Result := FList.Add(p);
end;

function TListPascalString.Add(Value: SystemString; Obj: TCore_Object): Integer;
var
  p: PListPascalStringData;
begin
  new(p);
  p^.Data.Text := Value;
  p^.Obj := Obj;
  p^.hash := MakeHashPas(@p^.Data);
  Result := FList.Add(p);
end;

function TListPascalString.Add(Value: TPascalString; Obj: TCore_Object): Integer;
var
  p: PListPascalStringData;
begin
  new(p);
  p^.Data := Value;
  p^.Obj := Obj;
  p^.hash := MakeHashPas(@p^.Data);
  Result := FList.Add(p);
end;

function TListPascalString.Add(Value: TUPascalString; Obj: TCore_Object): Integer;
var
  p: PListPascalStringData;
begin
  new(p);
  p^.Data.Text := Value.Text;
  p^.Obj := Obj;
  p^.hash := MakeHashPas(@p^.Data);
  Result := FList.Add(p);
end;

function TListPascalString.Append(Value: SystemString): Integer;
begin
  Result := Add(Value);
end;

function TListPascalString.Delete(idx: Integer): Integer;
var
  p: PListPascalStringData;
begin
  p := FList[idx];
  p^.Data := '';
  Dispose(p);
  FList.Delete(idx);
  Result := Count;
end;

function TListPascalString.DeleteString(Value: TPascalString): Integer;
var
  i: Integer;
  h: THash;
begin
  i := 0;
  h := MakeHashPas(@Value);
  while i < FList.Count do
    begin
      if (FList[i]^.hash = h) and (FList[i]^.Data.Same(@Value)) then
          Delete(i)
      else
          inc(i);
    end;
  Result := Count;
end;

procedure TListPascalString.Clear;
var
  i: Integer;
  p: PListPascalStringData;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      p^.Data := '';
      Dispose(p);
    end;
  FList.Clear;
end;

function TListPascalString.Count: Integer;
begin
  Result := FList.Count;
end;

function TListPascalString.ExistsValue(Value: TPascalString): Integer;
var
  i: Integer;
  h: THash;
begin
  h := MakeHashPas(@Value);
  Result := -1;

  for i := 0 to FList.Count - 1 do
    if (FList[i]^.hash = h) and (FList[i]^.Data.Same(@Value)) then
      begin
        Result := i;
        Break;
      end;
end;

procedure TListPascalString.Exchange(const idx1, idx2: Integer);
var
  tmp: Pointer;
begin
  tmp := FList[idx1];
  FList[idx1] := FList[idx2];
  FList[idx2] := tmp;
end;

function TListPascalString.ReplaceSum(Pattern: TPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): Integer;
var
  i: Integer;
  p: PListPascalStringData;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    begin
      p := FList[i];
      inc(Result, umlReplaceSum(@p^.Data, Pattern, OnlyWord, IgnoreCase, bPos, ePos, nil));
    end;
end;

procedure TListPascalString.Assign(SameObj: TListPascalString);
var
  i: Integer;
  P1, P2: PListPascalStringData;
begin
  Clear;
  for i := 0 to SameObj.Count - 1 do
    begin
      P2 := SameObj.FList[i];
      new(P1);
      P1^ := P2^;
      FList.Add(P1);
    end;
end;

procedure TListPascalString.Assign(sour: TCore_Strings);
var
  i: Integer;
begin
  Clear;
  for i := 0 to sour.Count - 1 do
      Add(sour[i], sour.Objects[i]);
end;

procedure TListPascalString.AssignTo(dest: TCore_Strings);
var
  i: Integer;
begin
  dest.Clear;
  for i := 0 to Count - 1 do
      dest.AddObject(Items[i], Objects[i]);
end;

procedure TListPascalString.AssignTo(dest: TListPascalString);
begin
  dest.Assign(Self);
end;

procedure TListPascalString.AddStrings(sour: TListPascalString);
var
  i: Integer;
begin
  for i := 0 to sour.Count - 1 do
      Add(sour[i]);
end;

procedure TListPascalString.AddStrings(sour: TCore_Strings);
var
  i: Integer;
begin
  for i := 0 to sour.Count - 1 do
      Add(sour[i]);
end;

procedure TListPascalString.FillTo(var Output_: TArrayPascalString);
var
  i: Integer;
begin
  SetLength(Output_, Count);
  for i := 0 to Count - 1 do
      Output_[i] := Items[i];
end;

procedure TListPascalString.FillFrom(const InData: TArrayPascalString);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Length(InData) - 1 do
      Add(InData[i]);
end;

procedure TListPascalString.LoadFromStream(stream: TCore_Stream);
var
  bp: Int64;
  r: TStreamReader;
begin
  Clear;
  bp := stream.Position;
{$IFDEF FPC}
  r := TStreamReader.Create(stream);
  while not r.Eof do
      Add(r.ReadLine);
{$ELSE FPC}
  r := TStreamReader.Create(stream, TEncoding.UTF8);
  try
    while not r.EndOfStream do
        Add(r.ReadLine);
  except
    Clear;
    DisposeObject(r);
    stream.Position := bp;
    r := TStreamReader.Create(stream, TEncoding.ANSI);
    while not r.EndOfStream do
        Add(r.ReadLine);
  end;
{$ENDIF FPC}
  DisposeObject(r);
end;

procedure TListPascalString.SaveToStream(stream: TCore_Stream);
var
  i: Integer;
  n: TPascalString;
  b: TBytes;
begin
  for i := 0 to FList.Count - 1 do
    begin
      n := FList[i]^.Data.Text + #13#10;
      b := n.Bytes;
      stream.write(b[0], Length(b));
      n := '';
    end;
end;

procedure TListPascalString.LoadFromFile(fn: SystemString);
var
  fs: TCore_FileStream;
begin
  fs := TCore_FileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TListPascalString.SaveToFile(fn: SystemString);
var
  fs: TCore_FileStream;
begin
  fs := TCore_FileStream.Create(fn, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TOn_Backcall_.Init;
begin
  Obj_ := nil;
  On_C := nil;
  On_M := nil;
  On_P := nil;
end;

function TBackcall_Pool.GetVariantList: THashVariantList;
begin
  if FVariantList = nil then
      FVariantList := THashVariantList.Create;
  Result := FVariantList;
end;

function TBackcall_Pool.GetObjectList: THashObjectList;
begin
  if FObjectList = nil then
      FObjectList := THashObjectList.Create(False);
  Result := FObjectList;
end;

constructor TBackcall_Pool.Create;
begin
  inherited Create;
  FList := TBackcall_List_Decl.Create;
  FVariantList := nil;
  FObjectList := nil;
  FOwner := nil;
end;

destructor TBackcall_Pool.Destroy;
begin
  if FVariantList <> nil then
      DisposeObject(FVariantList);
  if FObjectList <> nil then
      DisposeObject(FObjectList);
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TBackcall_Pool.RegisterBackcallC(Obj_: TCore_Object; On_C_: TOn_Backcall_C);
var
  p: POn_Backcall_;
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    if FList[i]^.Obj_ = Obj_ then
        Exit;

  new(p);
  p^.Init;
  p^.Obj_ := Obj_;
  p^.On_C := On_C_;
  FList.Add(p);
end;

procedure TBackcall_Pool.RegisterBackcallM(Obj_: TCore_Object; On_M_: TOn_Backcall_M);
var
  p: POn_Backcall_;
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    if FList[i]^.Obj_ = Obj_ then
        Exit;

  new(p);
  p^.Init;
  p^.Obj_ := Obj_;
  p^.On_M := On_M_;
  FList.Add(p);
end;

procedure TBackcall_Pool.RegisterBackcallP(Obj_: TCore_Object; On_P_: TOn_Backcall_P);
var
  p: POn_Backcall_;
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    if FList[i]^.Obj_ = Obj_ then
        Exit;

  new(p);
  p^.Init;
  p^.Obj_ := Obj_;
  p^.On_P := On_P_;
  FList.Add(p);
end;

procedure TBackcall_Pool.UnRegisterBackcall(Obj_: TCore_Object);
var
  i: Integer;
begin
  i := 0;
  while i < FList.Count do
    begin
      if FList[i]^.Obj_ = Obj_ then
        begin
          Dispose(FList[i]);
          FList.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TBackcall_Pool.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
      Dispose(FList[i]);
  FList.Clear;
end;

procedure TBackcall_Pool.ExecuteBackcall(TriggerObject: TCore_Object; Param1, Param2, Param3: Variant);
var
  i: Integer;
  p: POn_Backcall_;
begin
  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if Assigned(p^.On_C) then
        begin
          try
              p^.On_C(Self, TriggerObject, Param1, Param2, Param3);
          except
          end;
        end;
      if Assigned(p^.On_M) then
        begin
          try
              p^.On_M(Self, TriggerObject, Param1, Param2, Param3);
          except
          end;
        end;
      if Assigned(p^.On_P) then
        begin
          try
              p^.On_P(Self, TriggerObject, Param1, Param2, Param3);
          except
          end;
        end;
      if (i >= 0) and (i < FList.Count) and (FList[i] = p) then
          inc(i);
    end;
end;

procedure TStringBigList.DoFree(var Data: SystemString);
begin
  Data := '';
end;

procedure TPascalStringBigList.DoFree(var Data: TPascalString);
begin
  Data := '';
end;

end.
