{ ****************************************************************************** }
{ * cloud 4.0 framework                                                        * }
{ ****************************************************************************** }
unit Z.Net.C4;

{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ELSE FPC}
  System.IOUtils,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine,
  Z.Geometry2D, Z.DFE, Z.Json,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.Expression, Z.OpCode,
  Z.Net, Z.Net.PhysicsIO,
  Z.Net.DoubleTunnelIO,
  Z.Net.DataStoreService,
  Z.Net.DoubleTunnelIO.VirtualAuth,
  Z.Net.DataStoreService.VirtualAuth,
  Z.Net.DoubleTunnelIO.NoAuth,
  Z.Net.DataStoreService.NoAuth;

type
  TC40_PhysicsService = class;
  TC40_PhysicsServicePool = class;
  TC40_PhysicsTunnel = class;
  TC40_PhysicsTunnelPool = class;
  TC40_Info = class;
  TC40_InfoList = class;
  TC40_Custom_Service = class;
  TC40_Custom_ServicePool = class;
  TC40_Custom_Client = class;
  TC40_Custom_ClientPool = class;
  TC40_Dispatch_Service = class;
  TC40_Dispatch_Client = class;
  TC40_Base_NoAuth_Service = class;
  TC40_Base_NoAuth_Client = class;
  TC40_Base_DataStoreNoAuth_Service = class;
  TC40_Base_DataStoreNoAuth_Client = class;
  TC40_Base_VirtualAuth_Service = class;
  TC40_Base_VirtualAuth_Client = class;
  TC40_Base_DataStoreVirtualAuth_Service = class;
  TC40_Base_DataStoreVirtualAuth_Client = class;

{$REGION 'PhysicsService'}
  TC40_DependNetworkString = U_StringArray;

  TC40_DependNetworkInfo = record
    Typ: U_String;
    Param: U_String;
  end;

  TC40_DependNetworkInfoArray = array of TC40_DependNetworkInfo;
  TC40_DependNetworkInfoList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_DependNetworkInfo>;

  IC40_PhysicsService_Event = interface
    procedure C40_PhysicsService_Build_Network(Sender: TC40_PhysicsService; Custom_Service_: TC40_Custom_Service);
    procedure C40_PhysicsService_Start(Sender: TC40_PhysicsService);
    procedure C40_PhysicsService_Stop(Sender: TC40_PhysicsService);
    procedure C40_PhysicsService_LinkSuccess(Sender: TC40_PhysicsService; Custom_Service_: TC40_Custom_Service; Trigger_: TCore_Object);
    procedure C40_PhysicsService_UserOut(Sender: TC40_PhysicsService; Custom_Service_: TC40_Custom_Service; Trigger_: TCore_Object);
  end;

  { automated physics service }
  TC40_PhysicsService = class(TCore_InterfacedObject)
  private
    FActivted: Boolean;
    FLastDeadConnectionCheckTime_: TTimeTick;
    procedure cmd_QueryInfo(Sender: TPeerIO; InData, OutData: TDFE);
  public
    ListeningAddr: U_String;
    PhysicsAddr: U_String;
    PhysicsPort: Word;
    PhysicsTunnel: TZNet_Server;
    AutoFreePhysicsTunnel: Boolean;
    DependNetworkServicePool: TC40_Custom_ServicePool;
    OnEvent: IC40_PhysicsService_Event;
    { api }
    constructor Create(ListeningAddr_, PhysicsAddr_: U_String; PhysicsPort_: Word; PhysicsTunnel_: TZNet_Server); overload;
    constructor Create(PhysicsAddr_: U_String; PhysicsPort_: Word; PhysicsTunnel_: TZNet_Server); overload;
    destructor Destroy; override;
    procedure Progress; virtual;
    function BuildDependNetwork(const Depend_: TC40_DependNetworkInfoArray): Boolean; overload; virtual;
    function BuildDependNetwork(const Depend_: TC40_DependNetworkString): Boolean; overload;
    function BuildDependNetwork(const Depend_: U_String): Boolean; overload;
    property Activted: Boolean read FActivted;
    procedure StartService; virtual;
    procedure StopService; virtual;
    { event }
    procedure DoLinkSuccess(Custom_Service_: TC40_Custom_Service; Trigger_: TCore_Object);
    procedure DoUserOut(Custom_Service_: TC40_Custom_Service; Trigger_: TCore_Object);
  end;

  TC40_PhysicsServicePool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_PhysicsService>;

  TC40_PhysicsServicePool = class(TC40_PhysicsServicePool_Decl)
  public
    procedure Progress;
    function ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
  end;
{$ENDREGION 'PhysicsService'}
{$REGION 'PhysicsTunnel'}

  TDCT40_OnQueryResultC = procedure(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
  TDCT40_OnQueryResultM = procedure(Sender: TC40_PhysicsTunnel; L: TC40_InfoList) of object;
{$IFDEF FPC}
  TDCT40_OnQueryResultP = procedure(Sender: TC40_PhysicsTunnel; L: TC40_InfoList) is nested;
{$ELSE FPC}
  TDCT40_OnQueryResultP = reference to procedure(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
{$ENDIF FPC}

  TDCT40_QueryResultData = class
  private
    procedure DoStreamParam(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
    procedure DoStreamFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
    procedure DoRun;
  public
    C40_PhysicsTunnel: TC40_PhysicsTunnel;
    L: TC40_InfoList;
    OnResultC: TDCT40_OnQueryResultC;
    OnResultM: TDCT40_OnQueryResultM;
    OnResultP: TDCT40_OnQueryResultP;
    constructor Create;
    destructor Destroy; override;
  end;

  TDCT40_QueryResultAndDependProcessor = class
  private
    procedure DCT40_OnCheckDepend(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
    procedure DCT40_OnAutoP2PVMConnectionDone(Sender: TZNet; P_IO: TPeerIO);
    procedure DCT40_OnBuildDependNetwork(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
    procedure DoRun(const state: Boolean);
  public
    C40_PhysicsTunnel: TC40_PhysicsTunnel;
    On_C: TOnState_C;
    On_M: TOnState_M;
    On_P: TOnState_P;
    constructor Create;
    destructor Destroy; override;
  end;

  IC40_PhysicsTunnel_Event = interface
    procedure C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    procedure C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
  end;

  { automated tunnel }
  TC40_PhysicsTunnel = class(TCore_InterfacedObject, IZNet_ClientInterface)
  private
    IsConnecting: Boolean;
    IsWaitBuildNetwor: Boolean;
    BuildNetworkIsDone: Boolean;
    OfflineTime: TTimeTick;
    procedure DoDelayConnect();
    procedure DoConnectOnResult(const state: Boolean);
    procedure DoConnectAndQuery(Param1: Pointer; Param2: TObject; const state: Boolean);
    procedure DoConnectAndCheckDepend(Param1: Pointer; Param2: TObject; const state: Boolean);
    procedure DoConnectAndBuildDependNetwork(Param1: Pointer; Param2: TObject; const state: Boolean);
  protected
    procedure ClientConnected(Sender: TZNet_Client); virtual;
    procedure ClientDisconnect(Sender: TZNet_Client); virtual;
  public
    PhysicsAddr: U_String;
    PhysicsPort: Word;
    PhysicsTunnel: TZNet_Client;
    DependNetworkInfoArray: TC40_DependNetworkInfoArray;
    DependNetworkClientPool: TC40_Custom_ClientPool;
    OnEvent: IC40_PhysicsTunnel_Event;
    { api }
    constructor Create(Addr_: U_String; Port_: Word);
    destructor Destroy; override;
    procedure Progress; virtual;
    function ResetDepend(const Depend_: TC40_DependNetworkInfoArray): Boolean; overload;
    function ResetDepend(const Depend_: TC40_DependNetworkString): Boolean; overload;
    function ResetDepend(const Depend_: U_String): Boolean; overload;
    function CheckDepend(): Boolean;
    function CheckDependC(OnResult: TOnState_C): Boolean;
    function CheckDependM(OnResult: TOnState_M): Boolean;
    function CheckDependP(OnResult: TOnState_P): Boolean;
    function BuildDependNetwork(): Boolean;
    function BuildDependNetworkC(OnResult: TOnState_C): Boolean;
    function BuildDependNetworkM(OnResult: TOnState_M): Boolean;
    function BuildDependNetworkP(OnResult: TOnState_P): Boolean;
    procedure QueryInfoC(OnResult: TDCT40_OnQueryResultC);
    procedure QueryInfoM(OnResult: TDCT40_OnQueryResultM);
    procedure QueryInfoP(OnResult: TDCT40_OnQueryResultP);
    function DependNetworkIsConnected: Boolean;
    { event }
    procedure DoClientConnected(Custom_Client_: TC40_Custom_Client);
  end;

  TC40_PhysicsTunnelPool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_PhysicsTunnel>;

  TC40_PhysicsTunnelPool = class(TC40_PhysicsTunnelPool_Decl)
  public
    { find addr }
    function ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
    function GetPhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word): TC40_PhysicsTunnel;
    { get or create from addr }
    function GetOrCreatePhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word): TC40_PhysicsTunnel; overload;
    function GetOrCreatePhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word;
      const Depend_: TC40_DependNetworkInfoArray; const OnEvent_: IC40_PhysicsTunnel_Event): TC40_PhysicsTunnel; overload;
    function GetOrCreatePhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word;
      const Depend_: U_String; const OnEvent_: IC40_PhysicsTunnel_Event): TC40_PhysicsTunnel; overload;
    { get or create from define }
    function GetOrCreatePhysicsTunnel(dispInfo: TC40_Info): TC40_PhysicsTunnel; overload;
    function GetOrCreatePhysicsTunnel(dispInfo: TC40_Info;
      const Depend_: TC40_DependNetworkInfoArray; const OnEvent_: IC40_PhysicsTunnel_Event): TC40_PhysicsTunnel; overload;
    function GetOrCreatePhysicsTunnel(dispInfo: TC40_Info;
      const Depend_: U_String; const OnEvent_: IC40_PhysicsTunnel_Event): TC40_PhysicsTunnel; overload;
    { fast service connection }
    procedure SearchServiceAndBuildConnection(PhysicsAddr: U_String; PhysicsPort: Word; FullConnection_: Boolean;
      const ServiceTyp: U_String; const OnEvent_: IC40_PhysicsTunnel_Event);
    { progress }
    procedure Progress;
  end;

  TTemp_SearchServiceBridge = class
  public
    Pool: TC40_PhysicsTunnelPool;
    FullConnection_: Boolean;
    ServiceTyp: U_String;
    OnEvent_: IC40_PhysicsTunnel_Event;
    procedure Do_SearchService_Event(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
  end;

{$ENDREGION 'PhysicsTunnel'}
{$REGION 'infoDefine'}

  TC40_Info = class
  private
    Ignored: Boolean;
    procedure MakeHash;
  public
    { share }
    OnlyInstance: Boolean;
    ServiceTyp: U_String;
    PhysicsAddr: U_String;
    PhysicsPort: Word;
    p2pVM_RecvTunnel_Addr: U_String;
    p2pVM_RecvTunnel_Port: Word;
    p2pVM_SendTunnel_Addr: U_String;
    p2pVM_SendTunnel_Port: Word;
    Workload, MaxWorkload: Integer;
    Hash: TMD5;

    { client translate }
    property p2pVM_ClientRecvTunnel_Addr: U_String read p2pVM_SendTunnel_Addr;
    property p2pVM_ClientRecvTunnel_Port: Word read p2pVM_SendTunnel_Port;
    property p2pVM_ClientSendTunnel_Addr: U_String read p2pVM_RecvTunnel_Addr;
    property p2pVM_ClientSendTunnel_Port: Word read p2pVM_RecvTunnel_Port;

    { api }
    constructor Create;
    destructor Destroy; override;
    procedure Assign(source: TC40_Info);
    function Clone: TC40_Info;
    procedure Load(stream: TCore_Stream);
    procedure Save(stream: TCore_Stream);
    function Same(Data_: TC40_Info): Boolean;
    function SameServiceTyp(Data_: TC40_Info): Boolean;
    function SamePhysicsAddr(PhysicsAddr_: U_String; PhysicsPort_: Word): Boolean; overload;
    function SamePhysicsAddr(Data_: TC40_Info): Boolean; overload;
    function SamePhysicsAddr(Data_: TC40_PhysicsTunnel): Boolean; overload;
    function SamePhysicsAddr(Data_: TC40_PhysicsService): Boolean; overload;
    function SameP2PVMAddr(Data_: TC40_Info): Boolean;
    function ReadyC40Client: Boolean;
    function GetOrCreateC40Client(PhysicsTunnel_: TC40_PhysicsTunnel; Param_: U_String): TC40_Custom_Client;
  end;

  TC40_InfoList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_Info>;

  TC40_Info_Array = array of TC40_Info;

  TC40_InfoList = class(TC40_InfoList_Decl)
  public
    AutoFree: Boolean;
    constructor Create(AutoFree_: Boolean);
    destructor Destroy; override;
    procedure Remove(obj: TC40_Info);
    procedure Delete(index: Integer);
    procedure Clear;
    class procedure SortWorkLoad(L_: TC40_InfoList);
    function GetInfoArray: TC40_Info_Array;
    function IsOnlyInstance(ServiceTyp: U_String): Boolean;
    function GetServiceTypNum(ServiceTyp: U_String): Integer;
    function SearchService(arry: TC40_DependNetworkInfoArray): TC40_Info_Array; overload;
    function SearchService(ServiceTyp: U_String): TC40_Info_Array; overload;
    function ExistsService(ServiceTyp: U_String): Boolean;
    function ExistsServiceAndPhysicsTunnel(ServiceTyp: U_String; PhysicsTunnel_: TC40_PhysicsTunnel): Boolean;
    function FindSame(Data_: TC40_Info): TC40_Info;
    function FindHash(Hash: TMD5): TC40_Info;
    function ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
    procedure RemovePhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word);
    function OverwriteInfo(Data_: TC40_Info): Boolean;
    function MergeAndUpdateWorkload(source: TC40_InfoList): Boolean;
    function MergeFromDF(D: TDFE): Boolean;
    procedure SaveToDF(D: TDFE);
  end;
{$ENDREGION 'infoDefine'}
{$REGION 'p2pVMCustomService'}

  TC40_Custom_Service = class(TCore_InterfacedObject)
  private
    FLastSafeCheckTime: TTimeTick;
  public
    Param: U_String;
    ParamList: THashStringList;
    SafeCheckTime: TTimeTick;
    ServiceInfo: TC40_Info;
    C40PhysicsService: TC40_PhysicsService;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); virtual;
    destructor Destroy; override;
    procedure SafeCheck; virtual;
    procedure Progress; virtual;
    procedure SetWorkload(Workload_, MaxWorkload_: Integer);
    procedure UpdateToGlobalDispatch;
    function GetHash: TMD5;
    property Hash: TMD5 read GetHash;
    { event }
    procedure DoLinkSuccess(Trigger_: TCore_Object);
    procedure DoUserOut(Trigger_: TCore_Object);
  end;

  TC40_Custom_Service_Class = class of TC40_Custom_Service;

  TC40_Custom_ServicePool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_Custom_Service>;
  TC40_Custom_Service_Array = array of TC40_Custom_Service;

  TC40_Custom_ServicePool = class(TC40_Custom_ServicePool_Decl)
  private
    FIPV6_Seed: Word;
  public
    constructor Create;
    procedure Progress;
    procedure MakeP2PVM_IPv6_Port(var ip6, port: U_String);
    function FindHash(hash_: TMD5): TC40_Custom_Service;
    function GetServiceFromHash(Hash: TMD5): TC40_Custom_Service;
    function ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
    function ExistsOnlyInstance(ServiceTyp: U_String): Boolean;
    function GetC40Array: TC40_Custom_Service_Array;
    function GetFromServiceTyp(ServiceTyp: U_String): TC40_Custom_Service_Array;
    function GetFromPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): TC40_Custom_Service_Array;
    function GetFromClass(Class_: TC40_Custom_Service_Class): TC40_Custom_Service_Array;
  end;
{$ENDREGION 'p2pVMCustomService'}
{$REGION 'p2pVMCustomClient'}

  TC40_Custom_Client = class(TCore_InterfacedObject)
  private
    FLastSafeCheckTime: TTimeTick;
  protected
    procedure DoNetworkOffline; virtual;
  public
    Param: U_String;
    ParamList: THashStringList;
    SafeCheckTime: TTimeTick;
    ClientInfo: TC40_Info;
    C40PhysicsTunnel: TC40_PhysicsTunnel;
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); virtual;
    destructor Destroy; override;
    procedure SafeCheck; virtual;
    procedure Progress; virtual;
    procedure Connect; virtual;
    function Connected: Boolean; virtual;
    procedure Disconnect; virtual;
    function GetHash: TMD5;
    property Hash: TMD5 read GetHash;
    { event }
    procedure DoClientConnected;
  end;

  TC40_Custom_Client_Class = class of TC40_Custom_Client;

  TC40_Custom_ClientPool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_Custom_Client>;

  TC40_Custom_Client_Array = array of TC40_Custom_Client;

  TC40_Custom_ClientPool_Wait_Data = record
    ServiceTyp_: U_String;
    Client_: TC40_Custom_Client;
  end;

  TC40_Custom_ClientPool_Wait_States = array of TC40_Custom_ClientPool_Wait_Data;

  TOn_C40_Custom_Client_EventC = procedure(States_: TC40_Custom_ClientPool_Wait_States);
  TOn_C40_Custom_Client_EventM = procedure(States_: TC40_Custom_ClientPool_Wait_States) of object;
{$IFDEF FPC}
  TOn_C40_Custom_Client_EventP = procedure(States_: TC40_Custom_ClientPool_Wait_States) is nested;
{$ELSE FPC}
  TOn_C40_Custom_Client_EventP = reference to procedure(States_: TC40_Custom_ClientPool_Wait_States);
{$ENDIF FPC}

  TC40_Custom_ClientPool_Wait = class
  private
    procedure DoRun;
  public
    States_: TC40_Custom_ClientPool_Wait_States;
    Pool_: TC40_Custom_ClientPool;
    On_C: TOn_C40_Custom_Client_EventC;
    On_M: TOn_C40_Custom_Client_EventM;
    On_P: TOn_C40_Custom_Client_EventP;
    constructor Create(dependNetwork_: U_String);
    destructor Destroy; override;
  end;

  TC40_Custom_ClientPool = class(TC40_Custom_ClientPool_Decl)
  private
  public
    procedure Progress;
    function FindHash(hash_: TMD5; isConnected: Boolean): TC40_Custom_Client; overload;
    function FindHash(hash_: TMD5): TC40_Custom_Client; overload;
    function ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
    function ExistsServiceInfo(info_: TC40_Info): Boolean;
    function ExistsServiceTyp(ServiceTyp: U_String): Boolean;
    function ExistsClass(Class_: TC40_Custom_Client_Class): TC40_Custom_Client;
    function ExistsConnectedClass(Class_: TC40_Custom_Client_Class): TC40_Custom_Client;
    function ExistsConnectedServiceTyp(ServiceTyp: U_String): TC40_Custom_Client;
    function ExistsConnectedServiceTypAndClass(ServiceTyp: U_String; Class_: TC40_Custom_Client_Class): TC40_Custom_Client;
    function FindPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
    function FindServiceInfo(info_: TC40_Info): Boolean;
    function FindServiceTyp(ServiceTyp: U_String): Boolean;
    function FindClass(Class_: TC40_Custom_Client_Class): TC40_Custom_Client;
    function FindConnectedClass(Class_: TC40_Custom_Client_Class): TC40_Custom_Client;
    function FindConnectedServiceTyp(ServiceTyp: U_String): TC40_Custom_Client;
    function FindConnectedServiceTypAndClass(ServiceTyp: U_String; Class_: TC40_Custom_Client_Class): TC40_Custom_Client;
    function GetClientFromHash(Hash: TMD5): TC40_Custom_Client;
    class procedure SortWorkLoad(L_: TC40_Custom_ClientPool);
    function GetC40Array: TC40_Custom_Client_Array;
    function SearchServiceTyp(ServiceTyp: U_String; isConnected: Boolean): TC40_Custom_Client_Array; overload;
    function SearchServiceTyp(ServiceTyp: U_String): TC40_Custom_Client_Array; overload;
    function SearchPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word; isConnected: Boolean): TC40_Custom_Client_Array; overload;
    function SearchPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): TC40_Custom_Client_Array; overload;
    function SearchClass(Class_: TC40_Custom_Client_Class; isConnected: Boolean): TC40_Custom_Client_Array; overload;
    function SearchClass(Class_: TC40_Custom_Client_Class): TC40_Custom_Client_Array; overload;
    procedure WaitConnectedDoneC(dependNetwork_: U_String; OnResult: TOn_C40_Custom_Client_EventC);
    procedure WaitConnectedDoneM(dependNetwork_: U_String; OnResult: TOn_C40_Custom_Client_EventM);
    procedure WaitConnectedDoneP(dependNetwork_: U_String; OnResult: TOn_C40_Custom_Client_EventP);
  end;

{$ENDREGION 'p2pVMCustomClient'}
{$REGION 'DispatchService'}

  TOnRemovePhysicsNetwork = class
  public
    PhysicsAddr: U_String;
    PhysicsPort: Word;
    constructor Create;
    procedure DoRun; virtual;
  end;

  TOnServiceInfoChange = procedure(Sender: TCore_Object; ServiceInfoList: TC40_InfoList) of object;

  // dispatch service
  TC40_Dispatch_Service = class(TC40_Custom_Service)
  private
    FOnServiceInfoChange: TOnServiceInfoChange;
    FWaiting_UpdateServerInfoToAllClient: Boolean;
    FWaiting_UpdateServerInfoToAllClient_TimeTick: TTimeTick;
    DelayCheck_Working: Boolean;
    procedure cmd_UpdateServiceInfo(Sender: TPeerIO; InData: TDFE);
    procedure cmd_UpdateServiceState(Sender: TPeerIO; InData: TDFE);
    procedure cmd_IgnoreChange(Sender: TPeerIO; InData: TDFE);
    procedure cmd_RequestUpdate(Sender: TPeerIO; InData: TDFE);
    procedure cmd_RemovePhysicsNetwork(Sender: TPeerIO; InData: TDFE);
    procedure Prepare_UpdateServerInfoToAllClient;
    procedure UpdateServerInfoToAllClient;

    procedure DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
    procedure DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
    procedure DoDelayCheckLocalServiceInfo;
  public
    Service: TDT_P2PVM_NoAuth_Custom_Service;
    ServiceInfoList: TC40_InfoList;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure IgnoreChangeToAllClient(Hash__: TMD5; Ignored: Boolean);
    procedure UpdateServiceStateToAllClient;
    { event }
    property OnServiceInfoChange: TOnServiceInfoChange read FOnServiceInfoChange write FOnServiceInfoChange;
  end;
{$ENDREGION 'DispatchService'}
{$REGION 'DispatchClient'}

  // dispatch client
  TC40_Dispatch_Client = class(TC40_Custom_Client)
  private
    FOnServiceInfoChange: TOnServiceInfoChange;
    DelayCheck_Working: Boolean;
    procedure cmd_UpdateServiceInfo(Sender: TPeerIO; InData: TDFE);
    procedure cmd_UpdateServiceState(Sender: TPeerIO; InData: TDFE);
    procedure cmd_IgnoreChange(Sender: TPeerIO; InData: TDFE);
    procedure cmd_RemovePhysicsNetwork(Sender: TPeerIO; InData: TDFE);
    procedure Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client);
    procedure DoDelayCheckLocalServiceInfo;
  public
    Client: TDT_P2PVM_NoAuth_Custom_Client;
    ServiceInfoList: TC40_InfoList;
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure PostLocalServiceInfo(forcePost_: Boolean);
    procedure RequestUpdate();
    procedure IgnoreChangeToService(Hash__: TMD5; Ignored: Boolean);
    procedure UpdateLocalServiceState;
    procedure RemovePhysicsNetwork(PhysicsAddr: U_String; PhysicsPort: Word);
    { event }
    property OnServiceInfoChange: TOnServiceInfoChange read FOnServiceInfoChange write FOnServiceInfoChange;
  end;
{$ENDREGION 'DispatchClient'}
{$REGION 'RegistedData'}

  TC40_RegistedData = record
    ServiceTyp: U_String;
    ServiceClass: TC40_Custom_Service_Class;
    ClientClass: TC40_Custom_Client_Class;
  end;

  PC40_RegistedData = ^TC40_RegistedData;

  TC40_RegistedDataList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PC40_RegistedData>;

  TC40_RegistedDataList = class(TC40_RegistedDataList_Decl)
  public
    destructor Destroy; override;
    procedure Clean;
    procedure Print;
  end;
{$ENDREGION 'RegistedData'}
{$REGION 'DTC40NoAuthModel'}

  TC40_Base_NoAuth_Service = class(TC40_Custom_Service)
  protected
    procedure DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
    procedure DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
  public
    Service: TDT_P2PVM_NoAuth_Custom_Service;
    DTNoAuthService: TDTService_NoAuth;
    property DTNoAuth: TDTService_NoAuth read DTNoAuthService;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
  end;

  TC40_Base_NoAuth_Client = class(TC40_Custom_Client)
  protected
    procedure Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client); virtual;
  public
    Client: TDT_P2PVM_NoAuth_Custom_Client;
    DTNoAuthClient: TDTClient_NoAuth;
    property DTNoAuth: TDTClient_NoAuth read DTNoAuthClient;
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
  end;

  TC40_Base_DataStoreNoAuth_Service = class(TC40_Custom_Service)
  protected
    procedure DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
    procedure DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
  public
    Service: TDT_P2PVM_NoAuth_Custom_Service;
    DTNoAuthService: TDataStoreService_NoAuth;
    property DTNoAuth: TDataStoreService_NoAuth read DTNoAuthService;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
  end;

  TC40_Base_DataStoreNoAuth_Client = class(TC40_Custom_Client)
  protected
    procedure Do_DT_P2PVM_DataStoreNoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client); virtual;
  public
    Client: TDT_P2PVM_NoAuth_Custom_Client;
    DTNoAuthClient: TDataStoreClient_NoAuth;
    property DTNoAuth: TDataStoreClient_NoAuth read DTNoAuthClient;
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
  end;

{$ENDREGION 'DTC40NoAuthModel'}
{$REGION 'DTC40VirtualAuthModel'}

  TC40_Base_VirtualAuth_Service = class(TC40_Custom_Service)
  protected
    procedure DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO); virtual;
    procedure DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO); virtual;
    procedure DoLinkSuccess_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); virtual;
    procedure DoUserOut_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); virtual;
  public
    Service: TDT_P2PVM_VirtualAuth_Custom_Service;
    DTVirtualAuthService: TDTService_VirtualAuth;
    property DTVirtualAuth: TDTService_VirtualAuth read DTVirtualAuthService;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
  end;

  TC40_Base_VirtualAuth_Client = class(TC40_Custom_Client)
  protected
    procedure Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_VirtualAuth_Custom_Client); virtual;
  public
    Client: TDT_P2PVM_VirtualAuth_Custom_Client;
    DTVirtualAuthClient: TDTClient_VirtualAuth;
    UserName, Password: U_String;
    NoDTLink: Boolean;
    property DTVirtualAuth: TDTClient_VirtualAuth read DTVirtualAuthClient;
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    function LoginIsSuccessed: Boolean;
  end;

  TC40_Base_DataStoreVirtualAuth_Service = class(TC40_Custom_Service)
  protected
    procedure DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO); virtual;
    procedure DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO); virtual;
    procedure DoLinkSuccess_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); virtual;
    procedure DoUserOut_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); virtual;
  public
    Service: TDT_P2PVM_VirtualAuth_Custom_Service;
    DTVirtualAuthService: TDataStoreService_VirtualAuth;
    property DTVirtualAuth: TDataStoreService_VirtualAuth read DTVirtualAuthService;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
  end;

  TC40_Base_DataStoreVirtualAuth_Client = class(TC40_Custom_Client)
  protected
    procedure Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_VirtualAuth_Custom_Client); virtual;
  public
    Client: TDT_P2PVM_VirtualAuth_Custom_Client;
    DTVirtualAuthClient: TDataStoreClient_VirtualAuth;
    UserName, Password: U_String;
    NoDTLink: Boolean;
    property DTVirtualAuth: TDataStoreClient_VirtualAuth read DTVirtualAuthClient;
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    function LoginIsSuccessed: Boolean;
  end;

{$ENDREGION 'DTC40VirtualAuthModel'}
{$REGION 'DTC40BuildInAuthModel'}

  TC40_Base_Service = class(TC40_Custom_Service)
  protected
    procedure DoLinkSuccess_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
    procedure DoUserOut_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
  public
    Service: TDT_P2PVM_Custom_Service;
    DTService: TDTService;
    property DT: TDTService read DTService;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
  end;

  TC40_Base_Client = class(TC40_Custom_Client)
  protected
    procedure Do_DT_P2PVM_Custom_Client_TunnelLink(Sender: TDT_P2PVM_Custom_Client); virtual;
  public
    Client: TDT_P2PVM_Custom_Client;
    DTClient: TDTClient;
    UserName, Password: U_String;
    NoDTLink: Boolean;
    property DT: TDTClient read DTClient;
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    function LoginIsSuccessed: Boolean;
  end;

  TC40_Base_DataStore_Service = class(TC40_Custom_Service)
  protected
    procedure DoLinkSuccess_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
    procedure DoUserOut_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
  public
    Service: TDT_P2PVM_Custom_Service;
    DTService: TDataStoreService;
    property DT: TDataStoreService read DTService;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
  end;

  TC40_Base_DataStore_Client = class(TC40_Custom_Client)
  protected
    procedure Do_DT_P2PVM_Custom_Client_TunnelLink(Sender: TDT_P2PVM_Custom_Client); virtual;
  public
    Client: TDT_P2PVM_Custom_Client;
    DTClient: TDataStoreClient;
    UserName, Password: U_String;
    NoDTLink: Boolean;
    property DT: TDataStoreClient read DTClient;
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    function LoginIsSuccessed: Boolean;
  end;

{$ENDREGION 'DTC40BuildInAuthModel'}


var
  { quiet mode, defualt is false }
  C40_QuietMode: Boolean;
  { physics service safeCheck time, default is 1 minute }
  C40_SafeCheckTime: TTimeTick;
  { C4 reconnection delay time, default is 5.0(float) seconds }
  C40_PhysicsReconnectionDelayTime: Double;
  { C4 Dispatch Service info update delay, default is 1 seconds }
  C40_UpdateServiceInfoDelayTime: TTimeTick;
  { physics service timeout, default is 5 minute }
  C40_PhysicsServiceTimeout: TTimeTick;
  { physics tunnel timeout, default is 5 minute }
  C40_PhysicsTunnelTimeout: TTimeTick;
  { kill dead physics connection timeout, default is 5 seconds }
  C40_KillDeadPhysicsConnectionTimeout: TTimeTick;
  { kill IDC fault timeout, default is 1 hour }
  C40_KillIDCFaultTimeout: TTimeTick;
  { root path, default is current Directory }
  C40_RootPath: U_String;
  { p2pVM default password }
  C40_Password: SystemString = 'DTC40@ZSERVER';
  { PhysicsTunnel interface }
  C40_PhysicsClientClass: TZNet_ClientClass;
  { automated matched }
  C40_Registed: TC40_RegistedDataList;
  { physics service pool }
  C40_PhysicsServicePool: TC40_PhysicsServicePool;
  { custom service pool }
  C40_ServicePool: TC40_Custom_ServicePool;
  { physics tunnel pool }
  C40_PhysicsTunnelPool: TC40_PhysicsTunnelPool;
  { custom client pool }
  C40_ClientPool: TC40_Custom_ClientPool;
  { default configure }
  C40_DefaultConfig: THashStringList;

function C40_Online_DP: TC40_Dispatch_Client; { System Online-DP }
procedure C40Progress;                        { C4 main progress }

{ quiet }
procedure C40SetQuietMode(QuietMode_: Boolean);

{ configure }
procedure C40WriteConfig(HS: THashStringList);
procedure C40ReadConfig(HS: THashStringList);
procedure C40ResetDefaultConfig;

{ free }
procedure C40Clean;
procedure C40Clean_Service;
procedure C40Clean_Client;

{ print state }
procedure C40PrintRegistation;

{ search physics }
function C40ExistsPhysicsNetwork(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;

{ Kill physics tunnel }
procedure C40RemovePhysics(PhysicsAddr: U_String; PhysicsPort: Word;
  Remove_P2PVM_Client_, Remove_Physics_Client_, RemoveP2PVM_Service_, Remove_Physcis_Service_: Boolean); overload;
procedure C40RemovePhysics(Tunnel_: TC40_PhysicsTunnel); overload;
procedure C40RemovePhysics(Service_: TC40_PhysicsService); overload;
procedure C40CheckAndKillDeadPhysicsTunnel();

{ register }
function RegisterC40(ServiceTyp: U_String; ServiceClass: TC40_Custom_Service_Class; ClientClass: TC40_Custom_Client_Class): Boolean;
function FindRegistedC40(ServiceTyp: U_String): PC40_RegistedData;
function GetRegisterServiceTypFromClass(ClientClass: TC40_Custom_Client_Class): U_String; overload;
function GetRegisterServiceTypFromClass(ServiceClass: TC40_Custom_Service_Class): U_String; overload;

{ misc }
function ExtractDependInfo(info: TC40_DependNetworkInfoList): TC40_DependNetworkInfoArray; overload;
function ExtractDependInfo(info: U_String): TC40_DependNetworkInfoArray; overload;
function ExtractDependInfo(arry: TC40_DependNetworkString): TC40_DependNetworkInfoArray; overload;
function ExtractDependInfoToL(info: U_String): TC40_DependNetworkInfoList; overload;
function ExtractDependInfoToL(arry: TC40_DependNetworkString): TC40_DependNetworkInfoList; overload;

implementation

var
  C40Progress_Working: Boolean = False;

function C40_Online_DP: TC40_Dispatch_Client;
var
  arry: TC40_Custom_Client_Array;
begin
  arry := C40_ClientPool.SearchClass(TC40_Dispatch_Client, True);
  if length(arry) > 0 then
      Result := arry[0] as TC40_Dispatch_Client
  else
      Result := nil;
  SetLength(arry, 0);
end;

procedure C40Progress;
begin
  if C40Progress_Working then
      exit;
  CheckThread;
  C40Progress_Working := True;
  try
    C40_PhysicsServicePool.Progress;
    C40_ServicePool.Progress;
    C40_PhysicsTunnelPool.Progress;
    C40_ClientPool.Progress;
    C40CheckAndKillDeadPhysicsTunnel();
  finally
      C40Progress_Working := False;
  end;
end;

procedure C40SetQuietMode(QuietMode_: Boolean);
  procedure Do_SetQuietMode(F: TZNet);
  var
    p2p_: TZNet_WithP2PVM_Client;
    i: Integer;
  begin
    F.QuietMode := QuietMode_;
    if F is TZNet_WithP2PVM_Client then
      begin
        p2p_ := TZNet_WithP2PVM_Client(F);
        for i := 0 to p2p_.ClonePool.Count - 1 do
            Do_SetQuietMode(p2p_.ClonePool[i]);
      end;
  end;

var
  i: Integer;
  cc: TC40_Custom_Client;
  cs: TC40_Custom_Service;
begin
  C40_QuietMode := QuietMode_;

  for i := 0 to C40_ClientPool.Count - 1 do
    begin
      cc := C40_ClientPool[i];
      if cc is TC40_Dispatch_Client then
        begin
          Do_SetQuietMode(TC40_Dispatch_Client(cc).Client.RecvTunnel);
          Do_SetQuietMode(TC40_Dispatch_Client(cc).Client.SendTunnel);
        end
      else if cc is TC40_Base_NoAuth_Client then
        begin
          Do_SetQuietMode(TC40_Base_NoAuth_Client(cc).Client.RecvTunnel);
          Do_SetQuietMode(TC40_Base_NoAuth_Client(cc).Client.SendTunnel);
        end
      else if cc is TC40_Base_DataStoreNoAuth_Client then
        begin
          Do_SetQuietMode(TC40_Base_DataStoreNoAuth_Client(cc).Client.RecvTunnel);
          Do_SetQuietMode(TC40_Base_DataStoreNoAuth_Client(cc).Client.SendTunnel);
        end
      else if cc is TC40_Base_VirtualAuth_Client then
        begin
          Do_SetQuietMode(TC40_Base_VirtualAuth_Client(cc).Client.RecvTunnel);
          Do_SetQuietMode(TC40_Base_VirtualAuth_Client(cc).Client.SendTunnel);
        end
      else if cc is TC40_Base_DataStoreVirtualAuth_Client then
        begin
          Do_SetQuietMode(TC40_Base_DataStoreVirtualAuth_Client(cc).Client.RecvTunnel);
          Do_SetQuietMode(TC40_Base_DataStoreVirtualAuth_Client(cc).Client.SendTunnel);
        end
      else if cc is TC40_Base_Client then
        begin
          Do_SetQuietMode(TC40_Base_Client(cc).Client.RecvTunnel);
          Do_SetQuietMode(TC40_Base_Client(cc).Client.SendTunnel);
        end
      else if cc is TC40_Base_DataStore_Client then
        begin
          Do_SetQuietMode(TC40_Base_DataStore_Client(cc).Client.RecvTunnel);
          Do_SetQuietMode(TC40_Base_DataStore_Client(cc).Client.SendTunnel);
        end
      else
          DoStatus('C40SetQuietMode no support: %s', [cc.ClassName]);
    end;

  for i := 0 to C40_ServicePool.Count - 1 do
    begin
      cs := C40_ServicePool[i];
      if cs is TC40_Dispatch_Service then
        begin
          Do_SetQuietMode(TC40_Dispatch_Service(cs).Service.RecvTunnel);
          Do_SetQuietMode(TC40_Dispatch_Service(cs).Service.SendTunnel);
        end
      else if cs is TC40_Base_NoAuth_Service then
        begin
          Do_SetQuietMode(TC40_Base_NoAuth_Service(cs).Service.RecvTunnel);
          Do_SetQuietMode(TC40_Base_NoAuth_Service(cs).Service.SendTunnel);
        end
      else if cs is TC40_Base_DataStoreNoAuth_Service then
        begin
          Do_SetQuietMode(TC40_Base_DataStoreNoAuth_Service(cs).Service.RecvTunnel);
          Do_SetQuietMode(TC40_Base_DataStoreNoAuth_Service(cs).Service.SendTunnel);
        end
      else if cs is TC40_Base_VirtualAuth_Service then
        begin
          Do_SetQuietMode(TC40_Base_VirtualAuth_Service(cs).Service.RecvTunnel);
          Do_SetQuietMode(TC40_Base_VirtualAuth_Service(cs).Service.SendTunnel);
        end
      else if cs is TC40_Base_DataStoreVirtualAuth_Service then
        begin
          Do_SetQuietMode(TC40_Base_DataStoreVirtualAuth_Service(cs).Service.RecvTunnel);
          Do_SetQuietMode(TC40_Base_DataStoreVirtualAuth_Service(cs).Service.SendTunnel);
        end
      else if cs is TC40_Base_Service then
        begin
          Do_SetQuietMode(TC40_Base_Service(cs).Service.RecvTunnel);
          Do_SetQuietMode(TC40_Base_Service(cs).Service.SendTunnel);
        end
      else if cs is TC40_Base_DataStore_Service then
        begin
          Do_SetQuietMode(TC40_Base_DataStore_Service(cs).Service.RecvTunnel);
          Do_SetQuietMode(TC40_Base_DataStore_Service(cs).Service.SendTunnel);
        end
      else
          DoStatus('C40SetQuietMode no support: %s', [cs.ClassName]);
    end;

  for i := 0 to C40_PhysicsTunnelPool.Count - 1 do
      Do_SetQuietMode(C40_PhysicsTunnelPool[i].PhysicsTunnel);

  for i := 0 to C40_PhysicsServicePool.Count - 1 do
      Do_SetQuietMode(C40_PhysicsServicePool[i].PhysicsTunnel);
end;

procedure C40WriteConfig(HS: THashStringList);
begin
  HS.SetDefaultValue('Quiet', umlBoolToStr(C40_QuietMode));
  HS.SetDefaultValue('SafeCheckTime', umlIntToStr(C40_SafeCheckTime));
  HS.SetDefaultValue('PhysicsReconnectionDelayTime', umlFloatToStr(C40_PhysicsReconnectionDelayTime));
  HS.SetDefaultValue('UpdateServiceInfoDelayTime', umlIntToStr(C40_UpdateServiceInfoDelayTime));
  HS.SetDefaultValue('PhysicsServiceTimeout', umlIntToStr(C40_PhysicsServiceTimeout));
  HS.SetDefaultValue('PhysicsTunnelTimeout', umlIntToStr(C40_PhysicsTunnelTimeout));
  HS.SetDefaultValue('KillIDCFaultTimeout', umlIntToStr(C40_KillIDCFaultTimeout));
end;

procedure C40ReadConfig(HS: THashStringList);
begin
  C40SetQuietMode(EStrToBool(HS.GetDefaultValue('Quiet', umlBoolToStr(C40_QuietMode))));
  C40_SafeCheckTime := EStrToInt(HS.GetDefaultValue('SafeCheckTime', umlIntToStr(C40_SafeCheckTime)));
  C40_PhysicsReconnectionDelayTime := EStrToDouble(HS.GetDefaultValue('PhysicsReconnectionDelayTime', umlFloatToStr(C40_PhysicsReconnectionDelayTime)));
  C40_UpdateServiceInfoDelayTime := EStrToInt(HS.GetDefaultValue('UpdateServiceInfoDelayTime', umlIntToStr(C40_UpdateServiceInfoDelayTime)));
  C40_PhysicsServiceTimeout := EStrToInt(HS.GetDefaultValue('PhysicsServiceTimeout', umlIntToStr(C40_PhysicsServiceTimeout)));
  C40_PhysicsTunnelTimeout := EStrToInt(HS.GetDefaultValue('PhysicsTunnelTimeout', umlIntToStr(C40_PhysicsTunnelTimeout)));
  C40_KillIDCFaultTimeout := EStrToInt(HS.GetDefaultValue('KillIDCFaultTimeout', umlIntToStr(C40_KillIDCFaultTimeout)));
end;

procedure C40ResetDefaultConfig;
begin
  C40ReadConfig(C40_DefaultConfig);
end;

procedure C40Clean;
var
  i: Integer;
begin
  for i := 0 to C40_PhysicsTunnelPool.Count - 1 do
      C40_PhysicsTunnelPool[i].PhysicsTunnel.Disconnect;
  for i := 0 to C40_PhysicsServicePool.Count - 1 do
      C40_PhysicsServicePool[i].StopService;

  while C40_ClientPool.Count > 0 do
      disposeObject(C40_ClientPool[0]);
  while C40_ServicePool.Count > 0 do
      disposeObject(C40_ServicePool[0]);
  C40_ServicePool.FIPV6_Seed := 1;
  while C40_PhysicsTunnelPool.Count > 0 do
      disposeObject(C40_PhysicsTunnelPool[0]);
  while C40_PhysicsServicePool.Count > 0 do
      disposeObject(C40_PhysicsServicePool[0]);
end;

procedure C40Clean_Service;
var
  i: Integer;
begin
  for i := 0 to C40_PhysicsServicePool.Count - 1 do
      C40_PhysicsServicePool[i].StopService;

  while C40_ServicePool.Count > 0 do
      disposeObject(C40_ServicePool[0]);
  C40_ServicePool.FIPV6_Seed := 1;
  while C40_PhysicsServicePool.Count > 0 do
      disposeObject(C40_PhysicsServicePool[0]);
end;

procedure C40Clean_Client;
var
  i: Integer;
begin
  for i := 0 to C40_PhysicsTunnelPool.Count - 1 do
      C40_PhysicsTunnelPool[i].PhysicsTunnel.Disconnect;

  while C40_ClientPool.Count > 0 do
      disposeObject(C40_ClientPool[0]);
  while C40_PhysicsTunnelPool.Count > 0 do
      disposeObject(C40_PhysicsTunnelPool[0]);
end;

procedure C40PrintRegistation;
begin
  C40_Registed.Print;
end;

function C40ExistsPhysicsNetwork(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
begin
  Result := True;
  if
    C40_PhysicsServicePool.ExistsPhysicsAddr(PhysicsAddr, PhysicsPort) or
    C40_ServicePool.ExistsPhysicsAddr(PhysicsAddr, PhysicsPort) or
    C40_PhysicsTunnelPool.ExistsPhysicsAddr(PhysicsAddr, PhysicsPort) or
    C40_ClientPool.ExistsPhysicsAddr(PhysicsAddr, PhysicsPort) then
      exit;
  Result := False;
end;

procedure C40RemovePhysics(PhysicsAddr: U_String; PhysicsPort: Word;
  Remove_P2PVM_Client_, Remove_Physics_Client_, RemoveP2PVM_Service_, Remove_Physcis_Service_: Boolean);
var
  i: Integer;
begin
  if Remove_P2PVM_Client_ then
    begin
      try
        { remove client }
        i := 0;
        while i < C40_ClientPool.Count do
          if PhysicsAddr.Same(@C40_ClientPool[i].ClientInfo.PhysicsAddr) and (PhysicsPort = C40_ClientPool[i].ClientInfo.PhysicsPort) then
            begin
              disposeObject(C40_ClientPool[i]);
              i := 0;
            end
          else
              inc(i);
      except
      end;
    end;

  { remove dispatch info }
  for i := 0 to C40_ClientPool.Count - 1 do
    if C40_ClientPool[i] is TC40_Dispatch_Client then
        TC40_Dispatch_Client(C40_ClientPool[i]).ServiceInfoList.RemovePhysicsAddr(PhysicsAddr, PhysicsPort);

  if Remove_Physics_Client_ then
    begin
      try
        { remove physics tunnel }
        i := 0;
        while i < C40_PhysicsTunnelPool.Count do
          begin
            if PhysicsAddr.Same(@C40_PhysicsTunnelPool[i].PhysicsAddr) and (PhysicsPort = C40_PhysicsTunnelPool[i].PhysicsPort) then
              begin
                disposeObject(C40_PhysicsTunnelPool[i]);
                i := 0;
              end
            else
                inc(i);
          end;
      except
      end;
    end;

  if RemoveP2PVM_Service_ then
    begin
      try
        { remove service }
        i := 0;
        while i < C40_ServicePool.Count do
          if PhysicsAddr.Same(@C40_ServicePool[i].ServiceInfo.PhysicsAddr) and (PhysicsPort = C40_ServicePool[i].ServiceInfo.PhysicsPort) then
            begin
              disposeObject(C40_ServicePool[i]);
              i := 0;
            end
          else
              inc(i);
      except
      end;
    end;

  { remove service info }
  for i := 0 to C40_ServicePool.Count - 1 do
    if C40_ServicePool[i] is TC40_Dispatch_Service then
        TC40_Dispatch_Service(C40_ServicePool[i]).ServiceInfoList.RemovePhysicsAddr(PhysicsAddr, PhysicsPort);

  if Remove_Physcis_Service_ then
    begin
      try
        { remove physics service }
        i := 0;
        while i < C40_PhysicsServicePool.Count do
          begin
            if PhysicsAddr.Same(@C40_PhysicsServicePool[i].PhysicsAddr) and (PhysicsPort = C40_PhysicsServicePool[i].PhysicsPort) then
              begin
                disposeObject(C40_PhysicsServicePool[i]);
                i := 0;
              end
            else
                inc(i);
          end;
      except
      end;
    end;
end;

procedure C40RemovePhysics(Tunnel_: TC40_PhysicsTunnel);
begin
  C40RemovePhysics(Tunnel_.PhysicsAddr, Tunnel_.PhysicsPort, True, True, False, False);
end;

procedure C40RemovePhysics(Service_: TC40_PhysicsService);
begin
  C40RemovePhysics(Service_.PhysicsAddr, Service_.PhysicsPort, True, True, True, True);
end;

procedure C40CheckAndKillDeadPhysicsTunnel();
var
  i: Integer;
  tmp: TC40_PhysicsTunnel;
begin
  i := 0;
  while i < C40_PhysicsTunnelPool.Count do
    begin
      tmp := C40_PhysicsTunnelPool[i];
      if (not tmp.PhysicsTunnel.RemoteInited) and (not tmp.BuildNetworkIsDone) and
        (tmp.OfflineTime > 0) and (GetTimeTick - tmp.OfflineTime > C40_KillDeadPhysicsConnectionTimeout) then
        begin
          C40RemovePhysics(tmp);
          i := 0;
        end
      else if (not tmp.PhysicsTunnel.RemoteInited) and (tmp.BuildNetworkIsDone) and
        (tmp.OfflineTime > 0) and (GetTimeTick - tmp.OfflineTime > C40_KillIDCFaultTimeout) then
        begin
          C40RemovePhysics(tmp);
          i := 0;
        end
      else
          inc(i);
    end;
end;

function RegisterC40(ServiceTyp: U_String; ServiceClass: TC40_Custom_Service_Class; ClientClass: TC40_Custom_Client_Class): Boolean;
var
  i: Integer;
  p: PC40_RegistedData;
begin
  Result := False;
  p := nil;
  for i := 0 to C40_Registed.Count - 1 do
    if ServiceTyp.Same(@C40_Registed[i]^.ServiceTyp) then
      begin
        p := C40_Registed[i];
        break;
      end;

  if p = nil then
    begin
      new(p);
      p^.ServiceTyp := ServiceTyp;
      p^.ServiceClass := nil;
      p^.ClientClass := nil;
      C40_Registed.Add(p);
    end;

  if ServiceClass <> nil then
      p^.ServiceClass := ServiceClass;
  if ClientClass <> nil then
      p^.ClientClass := ClientClass;
  Result := True;
end;

function FindRegistedC40(ServiceTyp: U_String): PC40_RegistedData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to C40_Registed.Count - 1 do
    if ServiceTyp.Same(@C40_Registed[i]^.ServiceTyp) then
      begin
        Result := C40_Registed[i];
        exit;
      end;
end;

function GetRegisterServiceTypFromClass(ClientClass: TC40_Custom_Client_Class): U_String;
var
  i: Integer;
  p: PC40_RegistedData;
begin
  Result := '';
  for i := 0 to C40_Registed.Count - 1 do
    begin
      p := C40_Registed[i];
      if p^.ClientClass.InheritsFrom(ClientClass) then
        begin
          if Result.L > 0 then
              Result.Append('|');
          Result.Append(p^.ServiceTyp);
        end;
    end;
end;

function GetRegisterServiceTypFromClass(ServiceClass: TC40_Custom_Service_Class): U_String;
var
  i: Integer;
  p: PC40_RegistedData;
begin
  Result := '';
  for i := 0 to C40_Registed.Count - 1 do
    begin
      p := C40_Registed[i];
      if p^.ServiceClass.InheritsFrom(ServiceClass) then
        begin
          if Result.L > 0 then
              Result.Append('|');
          Result.Append(p^.ServiceTyp);
        end;
    end;
end;

function ExtractDependInfo(info: TC40_DependNetworkInfoList): TC40_DependNetworkInfoArray;
var
  i: Integer;
begin
  SetLength(Result, info.Count);
  for i := 0 to info.Count - 1 do
      Result[i] := info[i];
end;

function ExtractDependInfo(info: U_String): TC40_DependNetworkInfoArray;
var
  tmp: TC40_DependNetworkString;
begin
  umlGetSplitArray(info, tmp, '|<>');
  Result := ExtractDependInfo(tmp);
end;

function ExtractDependInfo(arry: TC40_DependNetworkString): TC40_DependNetworkInfoArray;
var
  i: Integer;
  info_: TC40_DependNetworkInfo;
begin
  SetLength(Result, length(arry));
  for i := 0 to length(arry) - 1 do
    begin
      info_.Typ := umlTrimSpace(umlGetFirstStr(arry[i], '@'));
      info_.Param := umlTrimSpace(umlDeleteFirstStr(arry[i], '@'));
      Result[i] := info_;
    end;
end;

function ExtractDependInfoToL(info: U_String): TC40_DependNetworkInfoList;
var
  tmp: TC40_DependNetworkString;
begin
  umlGetSplitArray(info, tmp, '|<>');
  Result := ExtractDependInfoToL(tmp);
end;

function ExtractDependInfoToL(arry: TC40_DependNetworkString): TC40_DependNetworkInfoList;
var
  i: Integer;
  info_: TC40_DependNetworkInfo;
begin
  Result := TC40_DependNetworkInfoList.Create;
  for i := 0 to length(arry) - 1 do
    begin
      info_.Typ := umlTrimSpace(umlGetFirstStr(arry[i], '@'));
      info_.Param := umlTrimSpace(umlDeleteFirstStr(arry[i], '@'));
      Result.Add(info_);
    end;
end;

procedure TC40_PhysicsService.cmd_QueryInfo(Sender: TPeerIO; InData, OutData: TDFE);
var
  i, j: Integer;
  L: TC40_InfoList;
begin
  L := TC40_InfoList.Create(True);

  // search all service
  for i := 0 to C40_ServicePool.Count - 1 do
    if C40_ServicePool[i].C40PhysicsService.Activted then
      begin
        if L.FindSame(C40_ServicePool[i].ServiceInfo) = nil then
            L.Add(C40_ServicePool[i].ServiceInfo.Clone);
        // dispatch service
        if C40_ServicePool[i] is TC40_Dispatch_Service then
            L.MergeAndUpdateWorkload(TC40_Dispatch_Service(C40_ServicePool[i]).ServiceInfoList);
      end;

  // search all DP client
  for i := 0 to C40_ClientPool.Count - 1 do
    if C40_ClientPool[i] is TC40_Dispatch_Client then
        L.MergeAndUpdateWorkload(TC40_Dispatch_Client(C40_ClientPool[i]).ServiceInfoList);

  L.SaveToDF(OutData);
  disposeObject(L);
end;

constructor TC40_PhysicsService.Create(ListeningAddr_, PhysicsAddr_: U_String; PhysicsPort_: Word; PhysicsTunnel_: TZNet_Server);
begin
  inherited Create;
  FActivted := False;
  ListeningAddr := umlTrimSpace(ListeningAddr_);
  PhysicsAddr := umlTrimSpace(PhysicsAddr_);
  PhysicsPort := PhysicsPort_;
  PhysicsTunnel := PhysicsTunnel_;
  PhysicsTunnel.AutomatedP2PVMAuthToken := C40_Password;
  PhysicsTunnel.TimeOutKeepAlive := True;
  PhysicsTunnel.IdleTimeOut := C40_PhysicsServiceTimeout;
  PhysicsTunnel.RegisterStream('QueryInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_QueryInfo;
  PhysicsTunnel.PrintParams['QueryInfo'] := False;
  PhysicsTunnel.QuietMode := C40_QuietMode;
  AutoFreePhysicsTunnel := False;
  DependNetworkServicePool := TC40_Custom_ServicePool.Create;
  OnEvent := nil;
  C40_PhysicsServicePool.Add(Self);
  FLastDeadConnectionCheckTime_ := GetTimeTick;
end;

constructor TC40_PhysicsService.Create(PhysicsAddr_: U_String; PhysicsPort_: Word; PhysicsTunnel_: TZNet_Server);
begin
  Create(PhysicsAddr_, PhysicsAddr_, PhysicsPort_, PhysicsTunnel_);
end;

destructor TC40_PhysicsService.Destroy;
begin
  try
      StopService;
  except
  end;
  C40_PhysicsServicePool.Remove(Self);
  PhysicsTunnel.DeleteRegistedCMD('QueryInfo');
  disposeObject(DependNetworkServicePool);
  if AutoFreePhysicsTunnel then
      disposeObject(PhysicsTunnel);
  inherited Destroy;
end;

procedure TC40_PhysicsService.Progress;
var
  arry: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
begin
  if GetTimeTick - FLastDeadConnectionCheckTime_ > 1000 then
    begin
      PhysicsTunnel.GetIO_Array(arry);
      for ID_ in arry do
        begin
          IO_ := PhysicsTunnel.PeerIO[ID_];
          if (IO_ <> nil) and (not IO_.p2pVMTunnelReadyOk)
            and (GetTimeTick - IO_.IO_Create_TimeTick > C40_KillDeadPhysicsConnectionTimeout) then
              IO_.Disconnect;
        end;
      FLastDeadConnectionCheckTime_ := GetTimeTick;
    end;

  PhysicsTunnel.Progress;
end;

function TC40_PhysicsService.BuildDependNetwork(const Depend_: TC40_DependNetworkInfoArray): Boolean;
var
  i: Integer;
  p: PC40_RegistedData;
  tmp: TC40_Custom_Service;
begin
  Result := False;

  for i := 0 to length(Depend_) - 1 do
    begin
      p := FindRegistedC40(Depend_[i].Typ);
      if p = nil then
        begin
          PhysicsTunnel.Print('no found Registed service "%s"', [Depend_[i].Typ.Text]);
          exit;
        end;

      tmp := p^.ServiceClass.Create(Self, p^.ServiceTyp, Depend_[i].Param);
      PhysicsTunnel.Print('Build Depend service "%s" instance class "%s"', [tmp.ServiceInfo.ServiceTyp.Text, tmp.ClassName]);
      PhysicsTunnel.Print('service %s p2pVM Received tunnel ip %s port: %d', [tmp.ServiceInfo.ServiceTyp.Text, tmp.ServiceInfo.p2pVM_RecvTunnel_Addr.Text, tmp.ServiceInfo.p2pVM_RecvTunnel_Port]);
      PhysicsTunnel.Print('service %s p2pVM Send tunnel ip %s port: %d', [tmp.ServiceInfo.ServiceTyp.Text, tmp.ServiceInfo.p2pVM_SendTunnel_Addr.Text, tmp.ServiceInfo.p2pVM_SendTunnel_Port]);

      if Assigned(OnEvent) then
          OnEvent.C40_PhysicsService_Build_Network(Self, tmp);
    end;
  Result := True;
end;

function TC40_PhysicsService.BuildDependNetwork(const Depend_: TC40_DependNetworkString): Boolean;
begin
  Result := BuildDependNetwork(ExtractDependInfo(Depend_));
end;

function TC40_PhysicsService.BuildDependNetwork(const Depend_: U_String): Boolean;
begin
  Result := BuildDependNetwork(ExtractDependInfo(Depend_));
end;

procedure TC40_PhysicsService.StartService;
begin
  FActivted := PhysicsTunnel.StartService(ListeningAddr, PhysicsPort);
  if FActivted then
    begin
      PhysicsTunnel.Print('Physics Service Listening successed, internet addr: %s port: %d', [ListeningAddr.Text, PhysicsPort]);
      if Assigned(OnEvent) then
          OnEvent.C40_PhysicsService_Start(Self);
    end
  else
      PhysicsTunnel.Print('Physics Service Listening failed, internet addr: %s port: %d', [ListeningAddr.Text, PhysicsPort]);
end;

procedure TC40_PhysicsService.StopService;
begin
  if not FActivted then
      exit;
  PhysicsTunnel.StopService;
  PhysicsTunnel.Print('Physics Service Listening Stop.', []);
  if Assigned(OnEvent) then
      OnEvent.C40_PhysicsService_Stop(Self);
  FActivted := False;
end;

procedure TC40_PhysicsService.DoLinkSuccess(Custom_Service_: TC40_Custom_Service; Trigger_: TCore_Object);
begin
  if Assigned(OnEvent) then
      OnEvent.C40_PhysicsService_LinkSuccess(Self, Custom_Service_, Trigger_);
end;

procedure TC40_PhysicsService.DoUserOut(Custom_Service_: TC40_Custom_Service; Trigger_: TCore_Object);
begin
  if Assigned(OnEvent) then
      OnEvent.C40_PhysicsService_UserOut(Self, Custom_Service_, Trigger_);
end;

procedure TC40_PhysicsServicePool.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    begin
      try
          Items[i].Progress;
      except
      end;
    end;
end;

function TC40_PhysicsServicePool.ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if PhysicsAddr.Same(@Items[i].PhysicsAddr) and (PhysicsPort = Items[i].PhysicsPort) then
        exit;
  Result := False;
end;

procedure TDCT40_QueryResultData.DoStreamParam(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
begin
  L.MergeFromDF(Result_);
  DoRun;
end;

procedure TDCT40_QueryResultData.DoStreamFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
begin
  DoRun;
end;

procedure TDCT40_QueryResultData.DoRun;
begin
  try
    if Assigned(OnResultC) then
        OnResultC(C40_PhysicsTunnel, L);
    if Assigned(OnResultM) then
        OnResultM(C40_PhysicsTunnel, L);
    if Assigned(OnResultP) then
        OnResultP(C40_PhysicsTunnel, L);
  except
  end;
  Free;
end;

constructor TDCT40_QueryResultData.Create;
begin
  inherited Create;
  C40_PhysicsTunnel := nil;
  L := TC40_InfoList.Create(True);
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TDCT40_QueryResultData.Destroy;
begin
  disposeObject(L);
  inherited Destroy;
end;

procedure TDCT40_QueryResultAndDependProcessor.DCT40_OnCheckDepend(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
var
  i: Integer;
  state: Boolean;
begin
  state := True;
  for i := 0 to length(Sender.DependNetworkInfoArray) - 1 do
    begin
      if L.ExistsService(Sender.DependNetworkInfoArray[i].Typ) then
        begin
          Sender.PhysicsTunnel.Print('Check addr %s port:%d service "%s" passed.', [Sender.PhysicsAddr.Text, Sender.PhysicsPort, Sender.DependNetworkInfoArray[i].Typ.Text]);
        end
      else
        begin
          Sender.PhysicsTunnel.Print('failed! Check addr %s port:%d no found service "%s".', [Sender.PhysicsAddr.Text, Sender.PhysicsPort, Sender.DependNetworkInfoArray[i].Typ.Text]);
          state := False;
        end;
    end;
  DoRun(state);
end;

procedure TDCT40_QueryResultAndDependProcessor.DCT40_OnAutoP2PVMConnectionDone(Sender: TZNet; P_IO: TPeerIO);
var
  i: Integer;
begin
  Sender.AutomatedP2PVMClient := True;

  for i := 0 to C40_PhysicsTunnel.DependNetworkClientPool.Count - 1 do
    with C40_PhysicsTunnel.DependNetworkClientPool[i] do
      if not Connected then
          Connect;

  C40_PhysicsTunnel.IsWaitBuildNetwor := False;
  C40_PhysicsTunnel.BuildNetworkIsDone := True;
  C40_PhysicsTunnel.OfflineTime := 0;
  DoRun(True);
end;

procedure TDCT40_QueryResultAndDependProcessor.DCT40_OnBuildDependNetwork(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
var
  i, j: Integer;
  found_: Integer;
  tmp: TC40_Custom_Client;
begin
  found_ := 0;
  for i := 0 to length(Sender.DependNetworkInfoArray) - 1 do
    if L.ExistsService(Sender.DependNetworkInfoArray[i].Typ) then
        inc(found_);

  if found_ = 0 then
    begin
      DoRun(False);
      exit;
    end;

  for i := 0 to length(Sender.DependNetworkInfoArray) - 1 do
    for j := 0 to L.Count - 1 do
      begin
        if L[j].SamePhysicsAddr(Sender) and L[j].ServiceTyp.Same(@Sender.DependNetworkInfoArray[i].Typ) and
          (not Sender.DependNetworkClientPool.ExistsServiceInfo(L[j])) then
          begin
            tmp := L[j].GetOrCreateC40Client(Sender, Sender.DependNetworkInfoArray[i].Param);
            if tmp <> nil then
              begin
                Sender.PhysicsTunnel.Print('build "%s" network done.', [L[j].ServiceTyp.Text]);
                Sender.PhysicsTunnel.Print('"%s" network physics address "%s" physics port "%d" DCT40 Class:%s',
                  [L[j].ServiceTyp.Text, Sender.PhysicsAddr.Text, Sender.PhysicsPort, tmp.ClassName]);
                Sender.PhysicsTunnel.Print('"%s" network p2pVM Received Tunnel IPV6 "%s" Port:%d',
                  [L[j].ServiceTyp.Text, L[j].p2pVM_RecvTunnel_Addr.Text, L[j].PhysicsPort]);
                Sender.PhysicsTunnel.Print('"%s" network p2pVM Send Tunnel IPV6 "%s" Port:%d',
                  [L[j].ServiceTyp.Text, L[j].p2pVM_SendTunnel_Addr.Text, L[j].PhysicsPort]);

                if Assigned(C40_PhysicsTunnel.OnEvent) then
                    C40_PhysicsTunnel.OnEvent.C40_PhysicsTunnel_Build_Network(C40_PhysicsTunnel, tmp);
              end
            else
              begin
                Sender.PhysicsTunnel.Print('build "%s" network error.', [L[j].ServiceTyp.Text]);
              end;
          end;
      end;
  Sender.PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_M := {$IFDEF FPC}@{$ENDIF FPC}DCT40_OnAutoP2PVMConnectionDone;
  Sender.PhysicsTunnel.AutomatedP2PVM_Open(Sender.PhysicsTunnel.ClientIO);
end;

procedure TDCT40_QueryResultAndDependProcessor.DoRun(const state: Boolean);
begin
  if Assigned(On_C) then
      On_C(state);
  if Assigned(On_M) then
      On_M(state);
  if Assigned(On_P) then
      On_P(state);
  Free;
end;

constructor TDCT40_QueryResultAndDependProcessor.Create;
begin
  inherited Create;
  C40_PhysicsTunnel := nil;
  On_C := nil;
  On_M := nil;
  On_P := nil;
end;

destructor TDCT40_QueryResultAndDependProcessor.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_PhysicsTunnel.DoDelayConnect;
begin
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, {$IFDEF FPC}@{$ENDIF FPC}DoConnectOnResult);
end;

procedure TC40_PhysicsTunnel.DoConnectOnResult(const state: Boolean);
begin
  if not BuildNetworkIsDone then
    begin
      if state then
          PhysicsTunnel.Print('Physics Tunnel connection successed, internet addr: %s port: %d', [PhysicsAddr.Text, PhysicsPort])
      else
          PhysicsTunnel.Print('Physics Tunnel connection failed, internet addr: %s port: %d', [PhysicsAddr.Text, PhysicsPort]);
    end;
  IsConnecting := False;
end;

procedure TC40_PhysicsTunnel.DoConnectAndQuery(Param1: Pointer; Param2: TObject; const state: Boolean);
var
  tmp: TDCT40_QueryResultData;
begin
  DoConnectOnResult(state);
  tmp := TDCT40_QueryResultData(Param2);
  if state then
    begin
      PhysicsTunnel.SendStreamCmdM('QueryInfo', nil, nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParam, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailed);
    end
  else
    begin
      try
          tmp.DoRun;
      except
      end;
    end;
end;

procedure TC40_PhysicsTunnel.DoConnectAndCheckDepend(Param1: Pointer; Param2: TObject; const state: Boolean);
var
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  DoConnectOnResult(state);
  tmp := TDCT40_QueryResultAndDependProcessor(Param2);
  if state then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnCheckDepend);
    end
  else
    begin
      try
          tmp.DoRun(state);
      except
      end;
    end;
end;

procedure TC40_PhysicsTunnel.DoConnectAndBuildDependNetwork(Param1: Pointer; Param2: TObject; const state: Boolean);
var
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  DoConnectOnResult(state);
  tmp := TDCT40_QueryResultAndDependProcessor(Param2);
  if state then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
    end
  else
    begin
      try
          tmp.DoRun(state);
      except
      end;
    end;
end;

procedure TC40_PhysicsTunnel.ClientConnected(Sender: TZNet_Client);
begin
  try
    if Assigned(OnEvent) then
        OnEvent.C40_PhysicsTunnel_Connected(Self);
  except
  end;
end;

procedure TC40_PhysicsTunnel.ClientDisconnect(Sender: TZNet_Client);
var
  i: Integer;
begin
  try
    if Assigned(OnEvent) then
        OnEvent.C40_PhysicsTunnel_Disconnect(Self);
  except
  end;

  try
    for i := 0 to DependNetworkClientPool.Count - 1 do
        DependNetworkClientPool[i].DoNetworkOffline;
  except
  end;
end;

constructor TC40_PhysicsTunnel.Create(Addr_: U_String; Port_: Word);
var
  i: Integer;
begin
  inherited Create;
  IsConnecting := False;
  IsWaitBuildNetwor := False;
  BuildNetworkIsDone := False;
  OfflineTime := GetTimeTick;

  PhysicsAddr := umlTrimSpace(Addr_);
  PhysicsPort := Port_;
  PhysicsTunnel := C40_PhysicsClientClass.Create;
  PhysicsTunnel.AutomatedP2PVMAuthToken := C40_Password;
  PhysicsTunnel.TimeOutKeepAlive := True;
  PhysicsTunnel.IdleTimeOut := C40_PhysicsTunnelTimeout;
  PhysicsTunnel.SyncOnResult := False;
  PhysicsTunnel.SyncOnCompleteBuffer := True;
  PhysicsTunnel.SwitchDefaultPerformance;
  PhysicsTunnel.OnInterface := Self;
  PhysicsTunnel.PrintParams['QueryInfo'] := False;
  PhysicsTunnel.QuietMode := C40_QuietMode;

  SetLength(DependNetworkInfoArray, 0);
  DependNetworkClientPool := TC40_Custom_ClientPool.Create;
  OnEvent := nil;
  C40_PhysicsTunnelPool.Add(Self);
end;

destructor TC40_PhysicsTunnel.Destroy;
begin
  try
      PhysicsTunnel.Disconnect;
  except
  end;
  OnEvent := nil;
  C40_PhysicsTunnelPool.Remove(Self);
  PhysicsAddr := '';
  SetLength(DependNetworkInfoArray, 0);
  disposeObject(DependNetworkClientPool);
  disposeObject(PhysicsTunnel);
  inherited Destroy;
end;

procedure TC40_PhysicsTunnel.Progress;
begin
  PhysicsTunnel.Progress;

  { check state and reconnection }
  if BuildNetworkIsDone and (not IsConnecting) and (not PhysicsTunnel.RemoteInited) then
    begin
      IsConnecting := True;
      PhysicsTunnel.PostProgress.PostExecuteM_NP(C40_PhysicsReconnectionDelayTime, {$IFDEF FPC}@{$ENDIF FPC}DoDelayConnect);
    end;

  { check offline state }
  if (OfflineTime = 0) and (not PhysicsTunnel.RemoteInited) then
      OfflineTime := GetTimeTick;
end;

function TC40_PhysicsTunnel.ResetDepend(const Depend_: TC40_DependNetworkInfoArray): Boolean;
var
  i: Integer;
begin
  SetLength(DependNetworkInfoArray, length(Depend_));
  for i := 0 to length(Depend_) - 1 do
      DependNetworkInfoArray[i] := Depend_[i];

  Result := False;
  for i := 0 to length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
        exit;
  Result := True;
end;

function TC40_PhysicsTunnel.ResetDepend(const Depend_: TC40_DependNetworkString): Boolean;
begin
  Result := ResetDepend(ExtractDependInfo(Depend_));
end;

function TC40_PhysicsTunnel.ResetDepend(const Depend_: U_String): Boolean;
begin
  Result := ResetDepend(ExtractDependInfo(Depend_));
end;

function TC40_PhysicsTunnel.CheckDepend(): Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;

  Result := True;
  for i := 0 to length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.C40_PhysicsTunnel := Self;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnCheckDepend);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndCheckDepend);
end;

function TC40_PhysicsTunnel.CheckDependC(OnResult: TOnState_C): Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;

  Result := True;
  for i := 0 to length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        if Assigned(OnResult) then
            OnResult(False);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.C40_PhysicsTunnel := Self;
  tmp.On_C := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnCheckDepend);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndCheckDepend);
end;

function TC40_PhysicsTunnel.CheckDependM(OnResult: TOnState_M): Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;

  Result := True;
  for i := 0 to length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        if Assigned(OnResult) then
            OnResult(False);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.C40_PhysicsTunnel := Self;
  tmp.On_M := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnCheckDepend);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndCheckDepend);
end;

function TC40_PhysicsTunnel.CheckDependP(OnResult: TOnState_P): Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;

  Result := True;
  for i := 0 to length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        if Assigned(OnResult) then
            OnResult(False);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.C40_PhysicsTunnel := Self;
  tmp.On_P := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnCheckDepend);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndCheckDepend);
end;

function TC40_PhysicsTunnel.BuildDependNetwork: Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;
  if IsWaitBuildNetwor then
      exit;
  if BuildNetworkIsDone then
      exit;

  Result := True;
  for i := 0 to length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.C40_PhysicsTunnel := Self;
  IsWaitBuildNetwor := True;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndBuildDependNetwork);
end;

function TC40_PhysicsTunnel.BuildDependNetworkC(OnResult: TOnState_C): Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;
  if IsWaitBuildNetwor then
      exit;

  if BuildNetworkIsDone then
    begin
      IsWaitBuildNetwor := True;
      tmp := TDCT40_QueryResultAndDependProcessor.Create;
      tmp.C40_PhysicsTunnel := Self;
      tmp.On_C := OnResult;
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
      PhysicsTunnel.AutomatedP2PVM_Open();
      exit;
    end;

  Result := True;
  for i := 0 to length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        if Assigned(OnResult) then
            OnResult(False);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.C40_PhysicsTunnel := Self;
  tmp.On_C := OnResult;
  IsWaitBuildNetwor := True;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndBuildDependNetwork);
end;

function TC40_PhysicsTunnel.BuildDependNetworkM(OnResult: TOnState_M): Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;
  if IsWaitBuildNetwor then
      exit;

  if BuildNetworkIsDone then
    begin
      IsWaitBuildNetwor := True;
      tmp := TDCT40_QueryResultAndDependProcessor.Create;
      tmp.C40_PhysicsTunnel := Self;
      tmp.On_M := OnResult;
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
      PhysicsTunnel.AutomatedP2PVM_Open();
      exit;
    end;

  Result := True;
  for i := 0 to length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        if Assigned(OnResult) then
            OnResult(False);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.C40_PhysicsTunnel := Self;
  tmp.On_M := OnResult;
  IsWaitBuildNetwor := True;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndBuildDependNetwork);
end;

function TC40_PhysicsTunnel.BuildDependNetworkP(OnResult: TOnState_P): Boolean;
var
  i: Integer;
  tmp: TDCT40_QueryResultAndDependProcessor;
begin
  Result := False;
  if IsConnecting then
      exit;
  if IsWaitBuildNetwor then
      exit;

  if BuildNetworkIsDone then
    begin
      IsWaitBuildNetwor := True;
      tmp := TDCT40_QueryResultAndDependProcessor.Create;
      tmp.C40_PhysicsTunnel := Self;
      tmp.On_P := OnResult;
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
      PhysicsTunnel.AutomatedP2PVM_Open();
      exit;
    end;

  Result := True;
  for i := 0 to length(DependNetworkInfoArray) - 1 do
    if FindRegistedC40(DependNetworkInfoArray[i].Typ) = nil then
      begin
        PhysicsTunnel.Print('no registed "%s"', [DependNetworkInfoArray[i].Typ.Text]);
        if Assigned(OnResult) then
            OnResult(False);
        exit;
      end;

  tmp := TDCT40_QueryResultAndDependProcessor.Create;
  tmp.C40_PhysicsTunnel := Self;
  tmp.On_P := OnResult;
  IsWaitBuildNetwor := True;

  if PhysicsTunnel.RemoteInited then
    begin
      QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.DCT40_OnBuildDependNetwork);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndBuildDependNetwork);
end;

procedure TC40_PhysicsTunnel.QueryInfoC(OnResult: TDCT40_OnQueryResultC);
var
  tmp: TDCT40_QueryResultData;
begin
  tmp := TDCT40_QueryResultData.Create;
  tmp.C40_PhysicsTunnel := Self;
  tmp.OnResultC := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      PhysicsTunnel.SendStreamCmdM('QueryInfo', nil, nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParam, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailed);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndQuery);
end;

procedure TC40_PhysicsTunnel.QueryInfoM(OnResult: TDCT40_OnQueryResultM);
var
  tmp: TDCT40_QueryResultData;
begin
  tmp := TDCT40_QueryResultData.Create;
  tmp.C40_PhysicsTunnel := Self;
  tmp.OnResultM := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      PhysicsTunnel.SendStreamCmdM('QueryInfo', nil, nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParam, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailed);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndQuery);
end;

procedure TC40_PhysicsTunnel.QueryInfoP(OnResult: TDCT40_OnQueryResultP);
var
  tmp: TDCT40_QueryResultData;
begin
  tmp := TDCT40_QueryResultData.Create;
  tmp.C40_PhysicsTunnel := Self;
  tmp.OnResultP := OnResult;

  if PhysicsTunnel.RemoteInited then
    begin
      PhysicsTunnel.SendStreamCmdM('QueryInfo', nil, nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParam, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailed);
      exit;
    end;

  IsConnecting := True;
  PhysicsTunnel.AutomatedP2PVMService := False;
  PhysicsTunnel.AutomatedP2PVMClient := False;
  PhysicsTunnel.AsyncConnectM(PhysicsAddr, PhysicsPort, nil, tmp, {$IFDEF FPC}@{$ENDIF FPC}DoConnectAndQuery);
end;

function TC40_PhysicsTunnel.DependNetworkIsConnected: Boolean;
var
  i: Integer;
begin
  Result := False;
  if IsConnecting then
      exit;
  if not PhysicsTunnel.RemoteInited then
      exit;
  if not BuildNetworkIsDone then
      exit;
  for i := 0 to DependNetworkClientPool.Count - 1 do
    if not DependNetworkClientPool[i].Connected then
        exit;
  Result := True;
end;

procedure TC40_PhysicsTunnel.DoClientConnected(Custom_Client_: TC40_Custom_Client);
begin
  if Assigned(OnEvent) then
      OnEvent.C40_PhysicsTunnel_Client_Connected(Self, Custom_Client_);
end;

function TC40_PhysicsTunnelPool.ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if PhysicsAddr.Same(@Items[i].PhysicsAddr) and (PhysicsPort = Items[i].PhysicsPort) then
        exit;
  Result := False;
end;

function TC40_PhysicsTunnelPool.GetPhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word): TC40_PhysicsTunnel;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if PhysicsAddr.Same(@Items[i].PhysicsAddr) and (PhysicsPort = Items[i].PhysicsPort) then
      begin
        Result := Items[i];
        exit;
      end;
end;

function TC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word): TC40_PhysicsTunnel;
begin
  Result := GetPhysicsTunnel(PhysicsAddr, PhysicsPort);
  if Result = nil then
      Result := TC40_PhysicsTunnel.Create(PhysicsAddr, PhysicsPort);
end;

function TC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word;
  const Depend_: TC40_DependNetworkInfoArray; const OnEvent_: IC40_PhysicsTunnel_Event): TC40_PhysicsTunnel;
begin
  Result := GetPhysicsTunnel(PhysicsAddr, PhysicsPort);
  if (Result = nil) then
    begin
      Result := TC40_PhysicsTunnel.Create(PhysicsAddr, PhysicsPort);
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end
  else if (not Result.IsConnecting) and (not Result.BuildNetworkIsDone) then
    begin
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end;
end;

function TC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(PhysicsAddr: U_String; PhysicsPort: Word;
  const Depend_: U_String; const OnEvent_: IC40_PhysicsTunnel_Event): TC40_PhysicsTunnel;
begin
  Result := GetPhysicsTunnel(PhysicsAddr, PhysicsPort);
  if Result = nil then
    begin
      Result := TC40_PhysicsTunnel.Create(PhysicsAddr, PhysicsPort);
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end
  else if (not Result.IsConnecting) and (not Result.BuildNetworkIsDone) then
    begin
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end;
end;

function TC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(dispInfo: TC40_Info): TC40_PhysicsTunnel;
begin
  Result := GetPhysicsTunnel(dispInfo.PhysicsAddr, dispInfo.PhysicsPort);
  if Result = nil then
    begin
      Result := TC40_PhysicsTunnel.Create(dispInfo.PhysicsAddr, dispInfo.PhysicsPort);
    end;
end;

function TC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(dispInfo: TC40_Info;
  const Depend_: TC40_DependNetworkInfoArray; const OnEvent_: IC40_PhysicsTunnel_Event): TC40_PhysicsTunnel;
begin
  Result := GetPhysicsTunnel(dispInfo.PhysicsAddr, dispInfo.PhysicsPort);
  if Result = nil then
    begin
      Result := TC40_PhysicsTunnel.Create(dispInfo.PhysicsAddr, dispInfo.PhysicsPort);
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end
  else if (not Result.IsConnecting) and (not Result.BuildNetworkIsDone) then
    begin
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end;
end;

function TC40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(dispInfo: TC40_Info;
  const Depend_: U_String; const OnEvent_: IC40_PhysicsTunnel_Event): TC40_PhysicsTunnel;
begin
  Result := GetPhysicsTunnel(dispInfo.PhysicsAddr, dispInfo.PhysicsPort);
  if Result = nil then
    begin
      Result := TC40_PhysicsTunnel.Create(dispInfo.PhysicsAddr, dispInfo.PhysicsPort);
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end
  else if (not Result.IsConnecting) and (not Result.BuildNetworkIsDone) then
    begin
      Result.OnEvent := OnEvent_;
      Result.ResetDepend(Depend_);
      Result.BuildDependNetwork();
    end;
end;

procedure TC40_PhysicsTunnelPool.SearchServiceAndBuildConnection(PhysicsAddr: U_String; PhysicsPort: Word; FullConnection_: Boolean;
  const ServiceTyp: U_String; const OnEvent_: IC40_PhysicsTunnel_Event);
var
  tmp: TTemp_SearchServiceBridge;
  Tunnel_: TC40_PhysicsTunnel;
begin
  tmp := TTemp_SearchServiceBridge.Create;
  tmp.Pool := Self;
  tmp.FullConnection_ := FullConnection_;
  tmp.ServiceTyp := ServiceTyp;
  tmp.OnEvent_ := OnEvent_;
  Tunnel_ := GetOrCreatePhysicsTunnel(PhysicsAddr, PhysicsPort);
  Tunnel_.QueryInfoM({$IFDEF FPC}@{$ENDIF FPC}tmp.Do_SearchService_Event);
end;

procedure TTemp_SearchServiceBridge.Do_SearchService_Event(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
var
  arry: TC40_Info_Array;
  i: Integer;
begin
  arry := L.SearchService(ServiceTyp);
  if FullConnection_ then
    begin
      for i := low(arry) to high(arry) do
          Pool.GetOrCreatePhysicsTunnel(arry[i], ServiceTyp, OnEvent_);
    end
  else if length(arry) > 0 then
    begin
      Pool.GetOrCreatePhysicsTunnel(arry[0], ServiceTyp, OnEvent_);
    end;
  DelayFreeObj(1.0, Self);
end;

procedure TC40_PhysicsTunnelPool.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    begin
      try
          Items[i].Progress;
      except
      end;
    end;
end;

procedure TC40_Info.MakeHash;
var
  n: U_String;
  buff: TBytes;
begin
  n := umlTrimSpace(PhysicsAddr) + '_' + umlIntToStr(PhysicsPort) + '_' + umlTrimSpace(p2pVM_RecvTunnel_Addr) + '_' + umlTrimSpace(p2pVM_SendTunnel_Addr);
  n := n.LowerText;
  buff := n.Bytes;
  n := '';
  Hash := umlMD5(@buff[0], length(buff));
  SetLength(buff, 0);
end;

constructor TC40_Info.Create;
begin
  inherited Create;
  Ignored := False;
  // share
  OnlyInstance := False;
  ServiceTyp := '';
  PhysicsAddr := '';
  PhysicsPort := 0;
  p2pVM_RecvTunnel_Addr := '';
  p2pVM_RecvTunnel_Port := 0;
  p2pVM_SendTunnel_Addr := '';
  p2pVM_SendTunnel_Port := 0;
  Workload := 0;
  MaxWorkload := 0;
  Hash := NullMD5;
end;

destructor TC40_Info.Destroy;
begin
  ServiceTyp := '';
  PhysicsAddr := '';
  p2pVM_RecvTunnel_Addr := '';
  p2pVM_SendTunnel_Addr := '';
  inherited Destroy;
end;

procedure TC40_Info.Assign(source: TC40_Info);
begin
  Ignored := source.Ignored;
  OnlyInstance := source.OnlyInstance;
  ServiceTyp := source.ServiceTyp;
  PhysicsAddr := source.PhysicsAddr;
  PhysicsPort := source.PhysicsPort;
  p2pVM_RecvTunnel_Addr := source.p2pVM_RecvTunnel_Addr;
  p2pVM_RecvTunnel_Port := source.p2pVM_RecvTunnel_Port;
  p2pVM_SendTunnel_Addr := source.p2pVM_SendTunnel_Addr;
  p2pVM_SendTunnel_Port := source.p2pVM_SendTunnel_Port;
  Workload := source.Workload;
  MaxWorkload := source.MaxWorkload;
  Hash := source.Hash;
end;

function TC40_Info.Clone: TC40_Info;
begin
  Result := TC40_Info.Create;
  Result.Assign(Self);
end;

procedure TC40_Info.Load(stream: TCore_Stream);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.LoadFromStream(stream);

  OnlyInstance := D.R.ReadBool;
  ServiceTyp := D.R.ReadString;
  PhysicsAddr := D.R.ReadString;
  PhysicsPort := D.R.ReadWord;
  p2pVM_RecvTunnel_Addr := D.R.ReadString;
  p2pVM_RecvTunnel_Port := D.R.ReadWord;
  p2pVM_SendTunnel_Addr := D.R.ReadString;
  p2pVM_SendTunnel_Port := D.R.ReadWord;
  Workload := D.R.ReadInteger;
  MaxWorkload := D.R.ReadInteger;
  Hash := D.R.ReadMD5;

  disposeObject(D);
end;

procedure TC40_Info.Save(stream: TCore_Stream);
var
  D: TDFE;
begin
  D := TDFE.Create;

  D.WriteBool(OnlyInstance);
  D.WriteString(ServiceTyp);
  D.WriteString(PhysicsAddr);
  D.WriteWord(PhysicsPort);
  D.WriteString(p2pVM_RecvTunnel_Addr);
  D.WriteWord(p2pVM_RecvTunnel_Port);
  D.WriteString(p2pVM_SendTunnel_Addr);
  D.WriteWord(p2pVM_SendTunnel_Port);
  D.WriteInteger(Workload);
  D.WriteInteger(MaxWorkload);
  D.WriteMD5(Hash);

  D.FastEncodeTo(stream);
  disposeObject(D);
end;

function TC40_Info.Same(Data_: TC40_Info): Boolean;
begin
  Result := False;
  if not ServiceTyp.Same(@Data_.ServiceTyp) then
      exit;
  if not PhysicsAddr.Same(@Data_.PhysicsAddr) then
      exit;
  if PhysicsPort <> Data_.PhysicsPort then
      exit;
  if not p2pVM_RecvTunnel_Addr.Same(@Data_.p2pVM_RecvTunnel_Addr) then
      exit;
  if p2pVM_RecvTunnel_Port <> Data_.p2pVM_RecvTunnel_Port then
      exit;
  if not p2pVM_SendTunnel_Addr.Same(@Data_.p2pVM_SendTunnel_Addr) then
      exit;
  if p2pVM_SendTunnel_Port <> Data_.p2pVM_SendTunnel_Port then
      exit;
  Result := True;
end;

function TC40_Info.SameServiceTyp(Data_: TC40_Info): Boolean;
begin
  Result := ServiceTyp.Same(@Data_.ServiceTyp);
end;

function TC40_Info.SamePhysicsAddr(PhysicsAddr_: U_String; PhysicsPort_: Word): Boolean;
begin
  Result := False;
  if not PhysicsAddr.Same(@PhysicsAddr_) then
      exit;
  if PhysicsPort <> PhysicsPort_ then
      exit;
  Result := True;
end;

function TC40_Info.SamePhysicsAddr(Data_: TC40_Info): Boolean;
begin
  Result := False;
  if not PhysicsAddr.Same(@Data_.PhysicsAddr) then
      exit;
  if PhysicsPort <> Data_.PhysicsPort then
      exit;
  Result := True;
end;

function TC40_Info.SamePhysicsAddr(Data_: TC40_PhysicsTunnel): Boolean;
begin
  Result := False;
  if not PhysicsAddr.Same(@Data_.PhysicsAddr) then
      exit;
  if PhysicsPort <> Data_.PhysicsPort then
      exit;
  Result := True;
end;

function TC40_Info.SamePhysicsAddr(Data_: TC40_PhysicsService): Boolean;
begin
  Result := False;
  if not PhysicsAddr.Same(@Data_.PhysicsAddr) then
      exit;
  if PhysicsPort <> Data_.PhysicsPort then
      exit;
  Result := True;
end;

function TC40_Info.SameP2PVMAddr(Data_: TC40_Info): Boolean;
begin
  Result := False;
  if not p2pVM_RecvTunnel_Addr.Same(@Data_.p2pVM_RecvTunnel_Addr) then
      exit;
  if p2pVM_RecvTunnel_Port <> Data_.p2pVM_RecvTunnel_Port then
      exit;
  if not p2pVM_SendTunnel_Addr.Same(@Data_.p2pVM_SendTunnel_Addr) then
      exit;
  if p2pVM_SendTunnel_Port <> Data_.p2pVM_SendTunnel_Port then
      exit;
  Result := True;
end;

function TC40_Info.ReadyC40Client: Boolean;
var
  p: PC40_RegistedData;
begin
  p := FindRegistedC40(ServiceTyp);
  Result := (p <> nil) and (p^.ClientClass <> nil);
end;

function TC40_Info.GetOrCreateC40Client(PhysicsTunnel_: TC40_PhysicsTunnel; Param_: U_String): TC40_Custom_Client;
var
  p: PC40_RegistedData;
  i: Integer;
begin
  Result := nil;
  for i := 0 to PhysicsTunnel_.DependNetworkClientPool.Count - 1 do
    if Same(PhysicsTunnel_.DependNetworkClientPool[i].ClientInfo) then
      begin
        Result := C40_ClientPool[i];
        exit;
      end;

  p := FindRegistedC40(ServiceTyp);
  if p <> nil then
      Result := p^.ClientClass.Create(PhysicsTunnel_, Self, Param_);
end;

constructor TC40_InfoList.Create(AutoFree_: Boolean);
begin
  inherited Create;
  AutoFree := AutoFree_;
end;

destructor TC40_InfoList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TC40_InfoList.Remove(obj: TC40_Info);
begin
  if AutoFree then
      disposeObject(obj);
  inherited Remove(obj);
end;

procedure TC40_InfoList.Delete(index: Integer);
begin
  if AutoFree then
      disposeObject(Items[index]);
  inherited Delete(index);
end;

procedure TC40_InfoList.Clear;
var
  i: Integer;
begin
  if AutoFree then
    for i := 0 to Count - 1 do
        disposeObject(Items[i]);
  inherited Clear;
end;

class procedure TC40_InfoList.SortWorkLoad(L_: TC40_InfoList);
  function Compare_(Left, Right: TC40_Info): ShortInt;
  begin
    Result := CompareFloat(Left.Workload / Left.MaxWorkload, Right.Workload / Right.MaxWorkload);
    if Result = 0 then
        Result := CompareGeoInt(Right.MaxWorkload, Left.MaxWorkload);
  end;

  procedure fastSort_(arry_: TC40_InfoList; L, R: Integer);
  var
    i, j: Integer;
    p: TC40_Info;
  begin
    repeat
      i := L;
      j := R;
      p := arry_[(L + R) shr 1];
      repeat
        while Compare_(arry_[i], p) < 0 do
            inc(i);
        while Compare_(arry_[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
                arry_.Exchange(i, j);
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          fastSort_(arry_, L, j);
      L := i;
    until i >= R;
  end;

begin
  if L_.Count > 1 then
      fastSort_(L_, 0, L_.Count - 1);
end;

function TC40_InfoList.GetInfoArray: TC40_Info_Array;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Items[i];
end;

function TC40_InfoList.IsOnlyInstance(ServiceTyp: U_String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if umlMultipleMatch(True, ServiceTyp, Items[i].ServiceTyp) and Items[i].OnlyInstance then
      begin
        Result := True;
        exit;
      end;
end;

function TC40_InfoList.GetServiceTypNum(ServiceTyp: U_String): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if umlMultipleMatch(True, ServiceTyp, Items[i].ServiceTyp) then
        inc(Result);
end;

function TC40_InfoList.SearchService(arry: TC40_DependNetworkInfoArray): TC40_Info_Array;
var
  L: TC40_InfoList;
  i, j: Integer;
  found_: Boolean;
begin
  L := TC40_InfoList.Create(False);
  { filter }
  for i := 0 to Count - 1 do
    begin
      found_ := False;
      for j := Low(arry) to high(arry) do
        if arry[j].Typ.Same(@Items[i].ServiceTyp) then
          begin
            found_ := True;
            break;
          end;
      if found_ then
          L.Add(Items[i]);
    end;
  { sort }
  TC40_InfoList.SortWorkLoad(L);
  Result := L.GetInfoArray;
  disposeObject(L);
end;

function TC40_InfoList.SearchService(ServiceTyp: U_String): TC40_Info_Array;
var
  L: TC40_InfoList;
  i: Integer;
begin
  L := TC40_InfoList.Create(False);
  { filter }
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ServiceTyp) then
        L.Add(Items[i]);
  { sort }
  TC40_InfoList.SortWorkLoad(L);
  Result := L.GetInfoArray;
  disposeObject(L);
end;

function TC40_InfoList.ExistsService(ServiceTyp: U_String): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ServiceTyp) then
        exit;
  Result := False;
end;

function TC40_InfoList.ExistsServiceAndPhysicsTunnel(ServiceTyp: U_String; PhysicsTunnel_: TC40_PhysicsTunnel): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if umlMultipleMatch(True, ServiceTyp, Items[i].ServiceTyp) and (Items[i].SamePhysicsAddr(PhysicsTunnel_)) then
        exit;
  Result := False;
end;

function TC40_InfoList.FindSame(Data_: TC40_Info): TC40_Info;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Same(Data_) then
      begin
        Result := Items[i];
        exit;
      end;
end;

function TC40_InfoList.FindHash(Hash: TMD5): TC40_Info;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if umlCompareMD5(Hash, Items[i].Hash) then
      begin
        Result := Items[i];
        exit;
      end;
end;

function TC40_InfoList.ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if PhysicsAddr.Same(@Items[i].PhysicsAddr) and (PhysicsPort = Items[i].PhysicsPort) then
        exit;
  Result := False;
end;

procedure TC40_InfoList.RemovePhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word);
var
  i: Integer;
begin
  i := 0;
  while i < Count do
    if PhysicsAddr.Same(@Items[i].PhysicsAddr) and (PhysicsPort = Items[i].PhysicsPort) then
        Delete(i)
    else
        inc(i);
end;

function TC40_InfoList.OverwriteInfo(Data_: TC40_Info): Boolean;
var
  found_: TC40_Info;
begin
  Result := False;
  found_ := FindSame(Data_);
  if found_ <> nil then
    begin
      if found_ <> Data_ then
          found_.Assign(Data_);
    end
  else
    begin
      if AutoFree then
        begin
          Add(Data_.Clone);
          Result := True;
        end
      else
          DoStatus('autofree is false = memory leak.');
    end;
end;

function TC40_InfoList.MergeAndUpdateWorkload(source: TC40_InfoList): Boolean;
var
  i: Integer;
  found_: TC40_Info;
begin
  Result := False;
  for i := 0 to source.Count - 1 do
    begin
      found_ := FindSame(source[i]);
      if found_ = nil then
        begin
          if AutoFree then
              Add(source[i].Clone)
          else
              Add(source[i]);
          Result := True;
        end
      else if AutoFree then
        begin
          found_.Workload := umlMax(found_.Workload, source[i].Workload);
          found_.MaxWorkload := umlMax(found_.MaxWorkload, source[i].MaxWorkload);
        end;
    end;
end;

function TC40_InfoList.MergeFromDF(D: TDFE): Boolean;
var
  i: Integer;
  m64: TMS64;
  tmp, found_: TC40_Info;
  arry: TC40_Info_Array;
  ReadyNewInfo_: Boolean;
begin
  Result := False;
  while D.R.NotEnd do
    begin
      m64 := TMS64.Create;
      D.R.ReadStream(m64);
      m64.Position := 0;
      tmp := TC40_Info.Create;
      tmp.Load(m64);
      disposeObject(m64);
      found_ := FindSame(tmp);
      if found_ <> nil then
        begin
          disposeObject(tmp);
        end
      else
        begin
          ReadyNewInfo_ := True;
          if not AutoFree then
              DoStatus('autofree is false = memory leak.');
          if (tmp.OnlyInstance) then
            begin
              arry := SearchService(tmp.ServiceTyp);
              if length(arry) > 0 then
                begin
                  ReadyNewInfo_ := False;
                  DoStatus('"%s" is only instance.', [tmp.ServiceTyp.Text]);
                end;
            end;
          if ReadyNewInfo_ then
            begin
              Add(tmp);
              Result := True;
            end;
        end;
    end;
end;

procedure TC40_InfoList.SaveToDF(D: TDFE);
var
  i: Integer;
  m64: TMS64;
begin
  m64 := TMS64.Create;
  for i := 0 to Count - 1 do
    if not Items[i].Ignored then
      begin
        Items[i].Save(m64);
        D.WriteStream(m64);
        m64.Clear;
      end;
  disposeObject(m64);
end;

constructor TC40_Custom_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
var
  P2PVM_Recv_Name_, P2PVM_Recv_IP6_, P2PVM_Recv_Port_: U_String;
  P2PVM_Send_Name_, P2PVM_Send_IP6_, P2PVM_Send_Port_: U_String;
  tmp: TPascalStringList;
begin
  inherited Create;

  Param := Param_;
  C40PhysicsService := PhysicsService_;

  ParamList := THashStringList.Create;
  ParamList.AutoUpdateDefaultValue := True;
  try
    tmp := TPascalStringList.Create;
    umlSeparatorText(Param, tmp, ',;' + #13#10);
    ParamList.ImportFromStrings(tmp);
    disposeObject(tmp);
  except
  end;

  FLastSafeCheckTime := GetTimeTick;
  SafeCheckTime := EStrToInt64(ParamList.GetDefaultValue('SafeCheckTime', umlIntToStr(C40_SafeCheckTime)), C40_SafeCheckTime);

  P2PVM_Recv_Name_ := ServiceTyp + 'R';
  C40_ServicePool.MakeP2PVM_IPv6_Port(P2PVM_Recv_IP6_, P2PVM_Recv_Port_);
  P2PVM_Send_Name_ := ServiceTyp + 'S';
  C40_ServicePool.MakeP2PVM_IPv6_Port(P2PVM_Send_IP6_, P2PVM_Send_Port_);

  ServiceInfo := TC40_Info.Create;
  ServiceInfo.Ignored := EStrToBool(ParamList.GetDefaultValue('Ignored', if_(ServiceInfo.Ignored, 'True', 'False')), ServiceInfo.Ignored);
  ServiceInfo.OnlyInstance := EStrToBool(ParamList.GetDefaultValue('OnlyInstance', if_(ServiceInfo.OnlyInstance, 'True', 'False')), ServiceInfo.OnlyInstance);
  ServiceInfo.ServiceTyp := ServiceTyp;
  ServiceInfo.PhysicsAddr := C40PhysicsService.PhysicsAddr;
  ServiceInfo.PhysicsPort := C40PhysicsService.PhysicsPort;
  ServiceInfo.p2pVM_RecvTunnel_Addr := P2PVM_Recv_IP6_;
  ServiceInfo.p2pVM_RecvTunnel_Port := umlStrToInt(P2PVM_Recv_Port_);
  ServiceInfo.p2pVM_SendTunnel_Addr := P2PVM_Send_IP6_;
  ServiceInfo.p2pVM_SendTunnel_Port := umlStrToInt(P2PVM_Send_Port_);
  SetWorkload(0, 100);
  ServiceInfo.MakeHash;

  C40_ServicePool.Add(Self);
  C40PhysicsService.DependNetworkServicePool.Add(Self);
end;

destructor TC40_Custom_Service.Destroy;
begin
  C40PhysicsService.DependNetworkServicePool.Remove(Self);
  C40_ServicePool.Remove(Self);
  disposeObject(ServiceInfo);
  disposeObject(ParamList);
  inherited Destroy;
end;

procedure TC40_Custom_Service.SafeCheck;
begin

end;

procedure TC40_Custom_Service.Progress;
begin
  if GetTimeTick - FLastSafeCheckTime > SafeCheckTime then
    begin
      try
          SafeCheck;
      except
      end;
      FLastSafeCheckTime := GetTimeTick;
    end;
end;

procedure TC40_Custom_Service.SetWorkload(Workload_, MaxWorkload_: Integer);
begin
  ServiceInfo.Workload := Workload_;
  ServiceInfo.MaxWorkload := MaxWorkload_;
end;

procedure TC40_Custom_Service.UpdateToGlobalDispatch;
var
  i: Integer;
  dps: TC40_Dispatch_Service;
  dpc: TC40_Dispatch_Client;
begin
  for i := 0 to C40_ServicePool.Count - 1 do
    if C40_ServicePool[i] is TC40_Dispatch_Service then
      if C40_ServicePool[i] <> Self then
        begin
          dps := TC40_Dispatch_Service(C40_ServicePool[i]);
          if dps.ServiceInfoList.OverwriteInfo(ServiceInfo) then
              dps.Prepare_UpdateServerInfoToAllClient;
        end;

  for i := 0 to C40_ClientPool.Count - 1 do
    if C40_ClientPool[i] is TC40_Dispatch_Client then
      begin
        dpc := TC40_Dispatch_Client(C40_ClientPool[i]);
        if dpc.ServiceInfoList.OverwriteInfo(ServiceInfo) and dpc.Connected then
            dpc.PostLocalServiceInfo(True);
      end;
end;

function TC40_Custom_Service.GetHash: TMD5;
begin
  Result := ServiceInfo.Hash;
end;

procedure TC40_Custom_Service.DoLinkSuccess(Trigger_: TCore_Object);
begin
  C40PhysicsService.DoLinkSuccess(Self, Trigger_);
end;

procedure TC40_Custom_Service.DoUserOut(Trigger_: TCore_Object);
begin
  C40PhysicsService.DoUserOut(Self, Trigger_);
end;

constructor TC40_Custom_ServicePool.Create;
begin
  inherited Create;
  FIPV6_Seed := 1;
end;

procedure TC40_Custom_ServicePool.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    begin
      try
          Items[i].Progress;
      except
      end;
    end;
end;

procedure TC40_Custom_ServicePool.MakeP2PVM_IPv6_Port(var ip6, port: U_String);
var
  tmp: TIPV6;
  i: Integer;
begin
  for i := 0 to 7 do
      tmp[i] := FIPV6_Seed;
  port := umlIntToStr(FIPV6_Seed);
  inc(FIPV6_Seed);
  ip6 := IPV6ToStr(tmp);
end;

function TC40_Custom_ServicePool.FindHash(hash_: TMD5): TC40_Custom_Service;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if umlCompareMD5(hash_, Items[i].ServiceInfo.Hash) then
        exit(Items[i]);
end;

function TC40_Custom_ServicePool.GetServiceFromHash(Hash: TMD5): TC40_Custom_Service;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if umlCompareMD5(Hash, Items[i].ServiceInfo.Hash) then
        Result := Items[i];
end;

function TC40_Custom_ServicePool.ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if PhysicsAddr.Same(@Items[i].ServiceInfo.PhysicsAddr) and (PhysicsPort = Items[i].ServiceInfo.PhysicsPort) then
        exit;
  Result := False;
end;

function TC40_Custom_ServicePool.ExistsOnlyInstance(ServiceTyp: U_String): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if Items[i].ServiceInfo.OnlyInstance and ServiceTyp.Same(@Items[i].ServiceInfo.ServiceTyp) then
        exit;
  Result := False;
end;

function TC40_Custom_ServicePool.GetC40Array: TC40_Custom_Service_Array;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Items[i];
end;

function TC40_Custom_ServicePool.GetFromServiceTyp(ServiceTyp: U_String): TC40_Custom_Service_Array;
var
  L: TC40_Custom_ServicePool;
  i: Integer;
begin
  L := TC40_Custom_ServicePool.Create;
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ServiceInfo.ServiceTyp) then
        L.Add(Items[i]);
  Result := L.GetC40Array;
  disposeObject(L);
end;

function TC40_Custom_ServicePool.GetFromPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): TC40_Custom_Service_Array;
var
  L: TC40_Custom_ServicePool;
  i: Integer;
begin
  L := TC40_Custom_ServicePool.Create;
  for i := 0 to Count - 1 do
    if (PhysicsPort = Items[i].ServiceInfo.PhysicsPort) and PhysicsAddr.Same(@Items[i].ServiceInfo.PhysicsAddr) then
        L.Add(Items[i]);
  Result := L.GetC40Array;
  disposeObject(L);
end;

function TC40_Custom_ServicePool.GetFromClass(Class_: TC40_Custom_Service_Class): TC40_Custom_Service_Array;
var
  L: TC40_Custom_ServicePool;
  i: Integer;
begin
  L := TC40_Custom_ServicePool.Create;
  for i := 0 to Count - 1 do
    if Items[i].InheritsFrom(Class_) then
        L.Add(Items[i]);
  Result := L.GetC40Array;
  disposeObject(L);
end;

procedure TC40_Custom_Client.DoNetworkOffline;
begin

end;

constructor TC40_Custom_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
var
  tmp: TPascalStringList;
begin
  inherited Create;
  Param := Param_;
  ClientInfo := TC40_Info.Create;
  ClientInfo.Assign(source_);

  ParamList := THashStringList.Create;
  ParamList.AutoUpdateDefaultValue := True;
  try
    tmp := TPascalStringList.Create;
    umlSeparatorText(Param, tmp, ',;' + #13#10);
    ParamList.ImportFromStrings(tmp);
    disposeObject(tmp);
  except
  end;

  FLastSafeCheckTime := GetTimeTick;
  SafeCheckTime := EStrToInt64(ParamList.GetDefaultValue('SafeCheckTime', umlIntToStr(C40_SafeCheckTime)), C40_SafeCheckTime);

  if PhysicsTunnel_ = nil then
      C40PhysicsTunnel := C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(ClientInfo)
  else
      C40PhysicsTunnel := PhysicsTunnel_;
  C40PhysicsTunnel.DependNetworkClientPool.Add(Self);

  C40_ClientPool.Add(Self);
end;

destructor TC40_Custom_Client.Destroy;
begin
  C40_ClientPool.Remove(Self);
  C40PhysicsTunnel.DependNetworkClientPool.Remove(Self);
  disposeObject(ClientInfo);
  disposeObject(ParamList);
  inherited Destroy;
end;

procedure TC40_Custom_Client.SafeCheck;
begin

end;

procedure TC40_Custom_Client.Progress;
begin
  if GetTimeTick - FLastSafeCheckTime > SafeCheckTime then
    begin
      try
          SafeCheck;
      except
      end;
      FLastSafeCheckTime := GetTimeTick;
    end;
end;

procedure TC40_Custom_Client.Connect;
begin

end;

function TC40_Custom_Client.Connected: Boolean;
begin
  Result := False;
end;

procedure TC40_Custom_Client.Disconnect;
begin

end;

function TC40_Custom_Client.GetHash: TMD5;
begin
  Result := ClientInfo.Hash;
end;

procedure TC40_Custom_Client.DoClientConnected;
begin
  C40PhysicsTunnel.DoClientConnected(Self);
end;

procedure TC40_Custom_ClientPool_Wait.DoRun;
  function ExistsClientFromStatesDone(c_: TC40_Custom_Client): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := 0 to length(States_) - 1 do
      if States_[i].Client_ = c_ then
          exit;
    Result := False;
  end;

  function MatchServiceTypForPool(var d_: TC40_Custom_ClientPool_Wait_Data): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := 0 to Pool_.Count - 1 do
      begin
        if Pool_[i].Connected and d_.ServiceTyp_.Same(@Pool_[i].ClientInfo.ServiceTyp) and (not ExistsClientFromStatesDone(Pool_[i])) then
          begin
            d_.Client_ := Pool_[i];
            exit;
          end;
      end;
    Result := False;
  end;

  function IsAllDone: Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to length(States_) - 1 do
      if States_[i].Client_ = nil then
          exit;
    Result := True;
  end;

var
  i: Integer;
begin
  for i := 0 to length(States_) - 1 do
      MatchServiceTypForPool(States_[i]);

  if IsAllDone then
    begin
      try
        if Assigned(On_C) then
            On_C(States_);
        if Assigned(On_M) then
            On_M(States_);
        if Assigned(On_P) then
            On_P(States_);
      except
      end;
      DelayFreeObject(0.5, Self, nil);
    end
  else
      SystemPostProgress.PostExecuteM_NP(0.1, {$IFDEF FPC}@{$ENDIF FPC}DoRun);
end;

constructor TC40_Custom_ClientPool_Wait.Create(dependNetwork_: U_String);
var
  arry: TArrayPascalString;
  i: Integer;
begin
  inherited Create;
  umlGetSplitArray(dependNetwork_, arry, '|<>');
  SetLength(States_, length(arry));
  for i := 0 to length(arry) - 1 do
    begin
      States_[i].ServiceTyp_ := arry[i];
      States_[i].Client_ := nil;
    end;

  Pool_ := nil;
  On_C := nil;
  On_M := nil;
  On_P := nil;
end;

destructor TC40_Custom_ClientPool_Wait.Destroy;
begin
  SetLength(States_, 0);
  inherited Destroy;
end;

procedure TC40_Custom_ClientPool.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    begin
      try
          Items[i].Progress;
      except
      end;
    end;
end;

function TC40_Custom_ClientPool.FindHash(hash_: TMD5; isConnected: Boolean): TC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if umlCompareMD5(hash_, Items[i].ClientInfo.Hash) and ((not isConnected) or (isConnected and Items[i].Connected)) then
        exit(Items[i]);
end;

function TC40_Custom_ClientPool.FindHash(hash_: TMD5): TC40_Custom_Client;
begin
  Result := FindHash(hash_, False);
end;

function TC40_Custom_ClientPool.ExistsPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if PhysicsAddr.Same(@Items[i].ClientInfo.PhysicsAddr) and (PhysicsPort = Items[i].ClientInfo.PhysicsPort) then
        exit;
  Result := False;
end;

function TC40_Custom_ClientPool.ExistsServiceInfo(info_: TC40_Info): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if info_.Same(Items[i].ClientInfo) then
        exit;
  Result := False;
end;

function TC40_Custom_ClientPool.ExistsServiceTyp(ServiceTyp: U_String): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ClientInfo.ServiceTyp) then
        exit;
  Result := False;
end;

function TC40_Custom_ClientPool.ExistsClass(Class_: TC40_Custom_Client_Class): TC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].InheritsFrom(Class_) then
        exit(Items[i]);
end;

function TC40_Custom_ClientPool.ExistsConnectedClass(Class_: TC40_Custom_Client_Class): TC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].InheritsFrom(Class_) and Items[i].Connected then
        exit(Items[i]);
end;

function TC40_Custom_ClientPool.ExistsConnectedServiceTyp(ServiceTyp: U_String): TC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ClientInfo.ServiceTyp) and Items[i].Connected then
        exit(Items[i]);
end;

function TC40_Custom_ClientPool.ExistsConnectedServiceTypAndClass(ServiceTyp: U_String; Class_: TC40_Custom_Client_Class): TC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Connected and ServiceTyp.Same(@Items[i].ClientInfo.ServiceTyp) and Items[i].InheritsFrom(Class_) then
        exit(Items[i]);
end;

function TC40_Custom_ClientPool.FindPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if PhysicsAddr.Same(@Items[i].ClientInfo.PhysicsAddr) and (PhysicsPort = Items[i].ClientInfo.PhysicsPort) then
        exit;
  Result := False;
end;

function TC40_Custom_ClientPool.FindServiceInfo(info_: TC40_Info): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if info_.Same(Items[i].ClientInfo) then
        exit;
  Result := False;
end;

function TC40_Custom_ClientPool.FindServiceTyp(ServiceTyp: U_String): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ClientInfo.ServiceTyp) then
        exit;
  Result := False;
end;

function TC40_Custom_ClientPool.FindClass(Class_: TC40_Custom_Client_Class): TC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].InheritsFrom(Class_) then
        exit(Items[i]);
end;

function TC40_Custom_ClientPool.FindConnectedClass(Class_: TC40_Custom_Client_Class): TC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].InheritsFrom(Class_) and Items[i].Connected then
        exit(Items[i]);
end;

function TC40_Custom_ClientPool.FindConnectedServiceTyp(ServiceTyp: U_String): TC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ClientInfo.ServiceTyp) and Items[i].Connected then
        exit(Items[i]);
end;

function TC40_Custom_ClientPool.FindConnectedServiceTypAndClass(ServiceTyp: U_String; Class_: TC40_Custom_Client_Class): TC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Connected and ServiceTyp.Same(@Items[i].ClientInfo.ServiceTyp) and Items[i].InheritsFrom(Class_) then
        exit(Items[i]);
end;

function TC40_Custom_ClientPool.GetClientFromHash(Hash: TMD5): TC40_Custom_Client;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if umlCompareMD5(Hash, Items[i].ClientInfo.Hash) then
        Result := Items[i];
end;

class procedure TC40_Custom_ClientPool.SortWorkLoad(L_: TC40_Custom_ClientPool);
  function Compare_(Left, Right: TC40_Custom_Client): ShortInt;
  begin
    Result := CompareFloat(Left.ClientInfo.Workload / Left.ClientInfo.MaxWorkload, Right.ClientInfo.Workload / Right.ClientInfo.MaxWorkload);
    if Result = 0 then
        Result := CompareGeoInt(Right.ClientInfo.MaxWorkload, Left.ClientInfo.MaxWorkload);
  end;

  procedure fastSort_(arry_: TC40_Custom_ClientPool; L, R: Integer);
  var
    i, j: Integer;
    p: TC40_Custom_Client;
  begin
    repeat
      i := L;
      j := R;
      p := arry_[(L + R) shr 1];
      repeat
        while Compare_(arry_[i], p) < 0 do
            inc(i);
        while Compare_(arry_[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
                arry_.Exchange(i, j);
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          fastSort_(arry_, L, j);
      L := i;
    until i >= R;
  end;

begin
  if L_.Count > 1 then
      fastSort_(L_, 0, L_.Count - 1);
end;

function TC40_Custom_ClientPool.GetC40Array: TC40_Custom_Client_Array;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Items[i];
end;

function TC40_Custom_ClientPool.SearchServiceTyp(ServiceTyp: U_String; isConnected: Boolean): TC40_Custom_Client_Array;
var
  L: TC40_Custom_ClientPool;
  i: Integer;
begin
  L := TC40_Custom_ClientPool.Create;
  for i := 0 to Count - 1 do
    if ServiceTyp.Same(@Items[i].ClientInfo.ServiceTyp) then
      if (not isConnected) or (isConnected and Items[i].Connected) then
          L.Add(Items[i]);
  SortWorkLoad(L);
  Result := L.GetC40Array;
  disposeObject(L);
end;

function TC40_Custom_ClientPool.SearchServiceTyp(ServiceTyp: U_String): TC40_Custom_Client_Array;
begin
  Result := SearchServiceTyp(ServiceTyp, False);
end;

function TC40_Custom_ClientPool.SearchPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word; isConnected: Boolean): TC40_Custom_Client_Array;
var
  L: TC40_Custom_ClientPool;
  i: Integer;
begin
  L := TC40_Custom_ClientPool.Create;
  for i := 0 to Count - 1 do
    if (PhysicsPort = Items[i].ClientInfo.PhysicsPort) and PhysicsAddr.Same(@Items[i].ClientInfo.PhysicsAddr) then
      if (not isConnected) or (isConnected and Items[i].Connected) then
          L.Add(Items[i]);
  SortWorkLoad(L);
  Result := L.GetC40Array;
  disposeObject(L);
end;

function TC40_Custom_ClientPool.SearchPhysicsAddr(PhysicsAddr: U_String; PhysicsPort: Word): TC40_Custom_Client_Array;
begin
  Result := SearchPhysicsAddr(PhysicsAddr, PhysicsPort, False);
end;

function TC40_Custom_ClientPool.SearchClass(Class_: TC40_Custom_Client_Class; isConnected: Boolean): TC40_Custom_Client_Array;
var
  L: TC40_Custom_ClientPool;
  i: Integer;
begin
  L := TC40_Custom_ClientPool.Create;
  for i := 0 to Count - 1 do
    if Items[i].InheritsFrom(Class_) then
      if (not isConnected) or (isConnected and Items[i].Connected) then
          L.Add(Items[i]);
  SortWorkLoad(L);
  Result := L.GetC40Array;
  disposeObject(L);
end;

function TC40_Custom_ClientPool.SearchClass(Class_: TC40_Custom_Client_Class): TC40_Custom_Client_Array;
begin
  Result := SearchClass(Class_, False);
end;

procedure TC40_Custom_ClientPool.WaitConnectedDoneC(dependNetwork_: U_String; OnResult: TOn_C40_Custom_Client_EventC);
var
  tmp: TC40_Custom_ClientPool_Wait;
begin
  tmp := TC40_Custom_ClientPool_Wait.Create(dependNetwork_);
  tmp.Pool_ := Self;
  tmp.On_C := OnResult;
  SystemPostProgress.PostExecuteM_NP(0.1, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoRun);
end;

procedure TC40_Custom_ClientPool.WaitConnectedDoneM(dependNetwork_: U_String; OnResult: TOn_C40_Custom_Client_EventM);
var
  tmp: TC40_Custom_ClientPool_Wait;
begin
  tmp := TC40_Custom_ClientPool_Wait.Create(dependNetwork_);
  tmp.Pool_ := Self;
  tmp.On_M := OnResult;
  SystemPostProgress.PostExecuteM_NP(0.1, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoRun);
end;

procedure TC40_Custom_ClientPool.WaitConnectedDoneP(dependNetwork_: U_String; OnResult: TOn_C40_Custom_Client_EventP);
var
  tmp: TC40_Custom_ClientPool_Wait;
begin
  tmp := TC40_Custom_ClientPool_Wait.Create(dependNetwork_);
  tmp.Pool_ := Self;
  tmp.On_P := OnResult;
  SystemPostProgress.PostExecuteM_NP(0.1, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoRun);
end;

constructor TOnRemovePhysicsNetwork.Create;
begin
  PhysicsAddr := '';
  PhysicsPort := 0;
end;

procedure TOnRemovePhysicsNetwork.DoRun;
begin
  C40RemovePhysics(PhysicsAddr, PhysicsPort, True, True, True, True);
  DelayFreeObject(1.0, Self);
end;

procedure TC40_Dispatch_Service.cmd_UpdateServiceInfo(Sender: TPeerIO; InData: TDFE);
begin
  if ServiceInfoList.MergeFromDF(InData) then
    begin
      Prepare_UpdateServerInfoToAllClient;

      if Assigned(FOnServiceInfoChange) then
          FOnServiceInfoChange(Self, ServiceInfoList);
    end;
end;

procedure TC40_Dispatch_Service.cmd_UpdateServiceState(Sender: TPeerIO; InData: TDFE);
var
  D, ND: TDFE;
  Hash__: TMD5;
  Workload, MaxWorkload: Integer;
  info_: TC40_Info;
  i: Integer;
  S_IO: TPeerIO;
  arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
begin
  ND := TDFE.Create;
  D := TDFE.Create;
  while InData.R.NotEnd do
    begin
      InData.R.ReadDataFrame(D);
      Hash__ := D.R.ReadMD5;
      Workload := D.R.ReadInteger;
      MaxWorkload := D.R.ReadInteger;
      info_ := ServiceInfoList.FindHash(Hash__);
      if (info_ <> nil) then
        begin
          if (info_.Workload <> Workload) or (info_.MaxWorkload <> MaxWorkload) then
              ND.WriteDataFrame(D);
          info_.Workload := Workload;
          info_.MaxWorkload := MaxWorkload;
        end;
    end;
  disposeObject(D);

  for i := 0 to C40_ServicePool.Count - 1 do
    begin
      info_ := ServiceInfoList.FindSame(C40_ServicePool[i].ServiceInfo);
      if info_ <> nil then
          info_.Assign(C40_ServicePool[i].ServiceInfo);
    end;

  if ND.Count > 0 then
    begin
      S_IO := nil;
      if Service.DTService.GetUserDefineRecvTunnel(Sender).LinkOk then
          S_IO := Service.DTService.GetUserDefineRecvTunnel(Sender).SendTunnel.Owner;
      Service.SendTunnel.GetIO_Array(arry_);
      for ID_ in arry_ do
        begin
          IO_ := Service.SendTunnel[ID_];
          if (IO_ <> nil) and (IO_ <> S_IO) and TPeerClientUserDefineForSendTunnel_NoAuth(IO_.UserDefine).LinkOk then
              IO_.SendDirectStreamCmd('UpdateServiceState', ND);
        end;
    end;
  disposeObject(ND);
end;

procedure TC40_Dispatch_Service.cmd_IgnoreChange(Sender: TPeerIO; InData: TDFE);
var
  Hash__: TMD5;
  Ignored: Boolean;
  info_: TC40_Info;
begin
  Hash__ := InData.R.ReadMD5;
  Ignored := InData.R.ReadBool;
  info_ := ServiceInfoList.FindHash(Hash__);
  if (info_ <> nil) and (info_.Ignored <> Ignored) then
    begin
      info_.Ignored := Ignored;
      IgnoreChangeToAllClient(info_.Hash, info_.Ignored);
    end;
end;

procedure TC40_Dispatch_Service.cmd_RequestUpdate(Sender: TPeerIO; InData: TDFE);
begin
  Prepare_UpdateServerInfoToAllClient;
end;

procedure TC40_Dispatch_Service.cmd_RemovePhysicsNetwork(Sender: TPeerIO; InData: TDFE);
var
  tmp: TOnRemovePhysicsNetwork;
  arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  IODef_: TPeerClientUserDefineForRecvTunnel_NoAuth;
begin
  tmp := TOnRemovePhysicsNetwork.Create;
  tmp.PhysicsAddr := InData.R.ReadString;
  tmp.PhysicsPort := InData.R.ReadWord;
  SysPost.PostExecuteM_NP(2.0, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoRun);

  if C40ExistsPhysicsNetwork(tmp.PhysicsAddr, tmp.PhysicsPort) then
    begin
      Service.RecvTunnel.GetIO_Array(arry_);
      for ID_ in arry_ do
        begin
          IO_ := Service.RecvTunnel[ID_];
          if (IO_ <> nil) and (IO_ <> Sender) and TPeerClientUserDefineForRecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
            begin
              IODef_ := TPeerClientUserDefineForRecvTunnel_NoAuth(IO_.UserDefine);
              IODef_.SendTunnel.Owner.SendDirectStreamCmd('RemovePhysicsNetwork', InData);
            end;
        end;
    end;
end;

procedure TC40_Dispatch_Service.Prepare_UpdateServerInfoToAllClient;
begin
  FWaiting_UpdateServerInfoToAllClient := True;
  FWaiting_UpdateServerInfoToAllClient_TimeTick := GetTimeTick + C40_UpdateServiceInfoDelayTime;
end;

procedure TC40_Dispatch_Service.UpdateServerInfoToAllClient;
var
  D: TDFE;
  arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
begin
  D := TDFE.Create;
  ServiceInfoList.SaveToDF(D);
  Service.SendTunnel.GetIO_Array(arry_);
  for ID_ in arry_ do
    begin
      IO_ := Service.SendTunnel[ID_];
      if (IO_ <> nil) and TPeerClientUserDefineForSendTunnel_NoAuth(IO_.UserDefine).LinkOk then
          IO_.SendDirectStreamCmd('UpdateServiceInfo', D);
    end;
  disposeObject(D);
end;

procedure TC40_Dispatch_Service.DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TC40_Dispatch_Service.DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoUserOut(UserDefineIO);
end;

procedure TC40_Dispatch_Service.DoDelayCheckLocalServiceInfo;
var
  i: Integer;
  isChange_: Boolean;
  info_: TC40_Info;
begin
  DelayCheck_Working := False;
  isChange_ := False;
  for i := 0 to C40_ServicePool.Count - 1 do
    if C40_ServicePool[i].C40PhysicsService.Activted then
      begin
        info_ := ServiceInfoList.FindSame(C40_ServicePool[i].ServiceInfo);
        if info_ = nil then
          begin
            ServiceInfoList.Add(C40_ServicePool[i].ServiceInfo.Clone);
            isChange_ := True;
          end
        else
            info_.Assign(C40_ServicePool[i].ServiceInfo);
      end;
  if isChange_ then
    begin
      Prepare_UpdateServerInfoToAllClient;
    end
  else
    begin
      UpdateServiceStateToAllClient;
    end;
end;

constructor TC40_Dispatch_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
var
  i: Integer;
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  FOnServiceInfoChange := nil;
  FWaiting_UpdateServerInfoToAllClient := False;
  FWaiting_UpdateServerInfoToAllClient_TimeTick := 0;
  DelayCheck_Working := False;

  { custom p2pVM service }
  Service := TDT_P2PVM_NoAuth_Custom_Service.Create(TDTService_NoAuth, PhysicsService_.PhysicsTunnel,
    ServiceInfo.ServiceTyp + 'R', ServiceInfo.p2pVM_RecvTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_RecvTunnel_Port),
    ServiceInfo.ServiceTyp + 'S', ServiceInfo.p2pVM_SendTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_SendTunnel_Port)
    );
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.FileSystem := False;
  Service.DTService.PublicFileDirectory := umlCombinePath(C40_RootPath, ServiceInfo.ServiceTyp.Text);
  if not umlDirectoryExists(Service.DTService.PublicFileDirectory) then
      umlCreateDirectory(Service.DTService.PublicFileDirectory);

  Service.RecvTunnel.RegisterDirectStream('UpdateServiceInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_UpdateServiceInfo;
  Service.RecvTunnel.RegisterDirectStream('UpdateServiceState').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_UpdateServiceState;
  Service.RecvTunnel.RegisterDirectStream('IgnoreChange').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_IgnoreChange;
  Service.RecvTunnel.RegisterDirectStream('RequestUpdate').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RequestUpdate;
  Service.RecvTunnel.RegisterDirectStream('RemovePhysicsNetwork').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemovePhysicsNetwork;

  Service.RecvTunnel.PrintParams['UpdateServiceInfo'] := False;
  Service.RecvTunnel.PrintParams['UpdateServiceState'] := False;
  Service.RecvTunnel.PrintParams['IgnoreChange'] := False;
  Service.RecvTunnel.PrintParams['RequestUpdate'] := False;

  Service.SendTunnel.PrintParams['UpdateServiceInfo'] := False;
  Service.SendTunnel.PrintParams['UpdateServiceState'] := False;
  Service.SendTunnel.PrintParams['IgnoreChange'] := False;
  Service.SendTunnel.PrintParams['RequestUpdate'] := False;

  { register local service. }
  ServiceInfoList := TC40_InfoList.Create(True);
  for i := 0 to C40_ServicePool.Count - 1 do
    if C40_ServicePool[i].C40PhysicsService.Activted then
      if ServiceInfoList.FindSame(C40_ServicePool[i].ServiceInfo) = nil then
          ServiceInfoList.Add(C40_ServicePool[i].ServiceInfo.Clone);

  UpdateToGlobalDispatch;
end;

destructor TC40_Dispatch_Service.Destroy;
begin
  disposeObject(Service);
  disposeObject(ServiceInfoList);
  inherited Destroy;
end;

procedure TC40_Dispatch_Service.Progress;
begin
  inherited Progress;
  Service.Progress;

  if FWaiting_UpdateServerInfoToAllClient and (GetTimeTick > FWaiting_UpdateServerInfoToAllClient_TimeTick) then
    begin
      FWaiting_UpdateServerInfoToAllClient := False;
      FWaiting_UpdateServerInfoToAllClient_TimeTick := 0;
      UpdateServerInfoToAllClient;
    end;
  ServiceInfo.Workload := Service.DTService.RecvTunnel.Count + Service.DTService.SendTunnel.Count;

  if not DelayCheck_Working then
    begin
      DelayCheck_Working := True;
      C40PhysicsService.PhysicsTunnel.PostProgress.PostExecuteM_NP(2.0, {$IFDEF FPC}@{$ENDIF FPC}DoDelayCheckLocalServiceInfo);
    end;
end;

procedure TC40_Dispatch_Service.IgnoreChangeToAllClient(Hash__: TMD5; Ignored: Boolean);
var
  D: TDFE;
  arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
begin
  D := TDFE.Create;
  D.WriteMD5(Hash__);
  D.WriteBool(Ignored);
  Service.SendTunnel.GetIO_Array(arry_);
  for ID_ in arry_ do
    begin
      IO_ := Service.SendTunnel[ID_];
      if (IO_ <> nil) and TPeerClientUserDefineForSendTunnel_NoAuth(IO_.UserDefine).LinkOk then
          IO_.SendDirectStreamCmd('IgnoreChange', D);
    end;
  disposeObject(D);
end;

procedure TC40_Dispatch_Service.UpdateServiceStateToAllClient;
var
  i: Integer;
  D, tmp: TDFE;
  info_: TC40_Info;
  arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
begin
  D := TDFE.Create;
  for i := 0 to C40_ServicePool.Count - 1 do
    if C40_ServicePool[i].C40PhysicsService.Activted then
      begin
        info_ := C40_ServicePool[i].ServiceInfo;
        tmp := TDFE.Create;
        tmp.WriteMD5(info_.Hash);
        tmp.WriteInteger(info_.Workload);
        tmp.WriteInteger(info_.MaxWorkload);
        D.WriteDataFrame(tmp);
        disposeObject(tmp);
      end;

  Service.SendTunnel.GetIO_Array(arry_);
  for ID_ in arry_ do
    begin
      IO_ := Service.SendTunnel[ID_];
      if (IO_ <> nil) and TPeerClientUserDefineForSendTunnel_NoAuth(IO_.UserDefine).LinkOk then
          IO_.SendDirectStreamCmd('UpdateServiceState', D);
    end;
  disposeObject(D);
end;

procedure TC40_Dispatch_Client.cmd_UpdateServiceInfo(Sender: TPeerIO; InData: TDFE);
var
  i: Integer;
  arry_: TC40_Custom_Client_Array;
  cc: TC40_Custom_Client;
begin
  if ServiceInfoList.MergeFromDF(InData) then
    begin
      if Assigned(FOnServiceInfoChange) then
          FOnServiceInfoChange(Self, ServiceInfoList);

      { broadcast to all service }
      arry_ := C40_ClientPool.SearchClass(TC40_Dispatch_Client, True);
      for cc in arry_ do
        if (cc <> Self) then
            TC40_Dispatch_Client(cc).Client.SendTunnel.SendDirectStreamCmd('UpdateServiceInfo', InData);
    end;
end;

procedure TC40_Dispatch_Client.cmd_UpdateServiceState(Sender: TPeerIO; InData: TDFE);
var
  D: TDFE;
  Hash__: TMD5;
  Workload, MaxWorkload: Integer;
  info_: TC40_Info;
  i, j: Integer;
begin
  D := TDFE.Create;
  while InData.R.NotEnd do
    begin
      InData.R.ReadDataFrame(D);
      Hash__ := D.R.ReadMD5;
      Workload := D.R.ReadInteger;
      MaxWorkload := D.R.ReadInteger;
      info_ := ServiceInfoList.FindHash(Hash__);
      if (info_ <> nil) then
        begin
          info_.Workload := Workload;
          info_.MaxWorkload := MaxWorkload;
          // automated fixed info
          for j := 0 to C40_ClientPool.Count - 1 do
            if C40_ClientPool[j].ClientInfo.Same(info_) then
                C40_ClientPool[j].ClientInfo.Assign(info_);
        end;

      for i := 0 to C40_ServicePool.Count - 1 do
        if (C40_ServicePool[i] is TC40_Dispatch_Service) then
          begin
            info_ := TC40_Dispatch_Service(C40_ServicePool[i]).ServiceInfoList.FindHash(Hash__);
            if (info_ <> nil) then
              begin
                info_.Workload := Workload;
                info_.MaxWorkload := MaxWorkload;
              end;
          end;
    end;
  disposeObject(D);

  for i := 0 to C40_ServicePool.Count - 1 do
    begin
      info_ := ServiceInfoList.FindSame(C40_ServicePool[i].ServiceInfo);
      if info_ <> nil then
          info_.Assign(C40_ServicePool[i].ServiceInfo);
    end;
end;

procedure TC40_Dispatch_Client.cmd_IgnoreChange(Sender: TPeerIO; InData: TDFE);
var
  Hash__: TMD5;
  Ignored: Boolean;
  info_: TC40_Info;
  arry_: TC40_Custom_Client_Array;
  cc: TC40_Custom_Client;
  j: Integer;
begin
  Hash__ := InData.R.ReadMD5;
  Ignored := InData.R.ReadBool;
  info_ := ServiceInfoList.FindHash(Hash__);
  if (info_ <> nil) then
    begin
      info_.Ignored := Ignored;
      // automated fixed info error.
      for j := 0 to C40_ClientPool.Count - 1 do
        if C40_ClientPool[j].ClientInfo.Same(info_) then
            C40_ClientPool[j].ClientInfo.Assign(info_);
    end;

  { broadcast to all service }
  arry_ := C40_ClientPool.SearchClass(TC40_Dispatch_Client, True);
  for cc in arry_ do
    if (cc <> Self) then
        TC40_Dispatch_Client(cc).Client.SendTunnel.SendDirectStreamCmd('IgnoreChange', InData);
end;

procedure TC40_Dispatch_Client.cmd_RemovePhysicsNetwork(Sender: TPeerIO; InData: TDFE);
var
  tmp: TOnRemovePhysicsNetwork;
  arry_: TC40_Custom_Client_Array;
  cc: TC40_Custom_Client;
begin
  tmp := TOnRemovePhysicsNetwork.Create;
  tmp.PhysicsAddr := InData.R.ReadString;
  tmp.PhysicsPort := InData.R.ReadWord;
  SysPost.PostExecuteM_NP(2.0, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoRun);

  if C40ExistsPhysicsNetwork(tmp.PhysicsAddr, tmp.PhysicsPort) then
    begin
      { broadcast to all service }
      arry_ := C40_ClientPool.SearchClass(TC40_Dispatch_Client, True);
      for cc in arry_ do
        if (cc <> Self) then
            TC40_Dispatch_Client(cc).Client.SendTunnel.SendDirectStreamCmd('RemovePhysicsNetwork', InData);
    end;
end;

procedure TC40_Dispatch_Client.Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client);
begin
  PostLocalServiceInfo(True);
  RequestUpdate();
  DoClientConnected();
end;

procedure TC40_Dispatch_Client.DoDelayCheckLocalServiceInfo;
var
  i: Integer;
begin
  DelayCheck_Working := False;
  PostLocalServiceInfo(False);
  UpdateLocalServiceState;

  { check and build network }
  for i := 0 to ServiceInfoList.Count - 1 do
      C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(ServiceInfoList[i], C40PhysicsTunnel.DependNetworkInfoArray, C40PhysicsTunnel.OnEvent);
end;

constructor TC40_Dispatch_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
var
  i: Integer;
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  FOnServiceInfoChange := nil;
  DelayCheck_Working := False;

  { custom p2pVM client }
  Client := TDT_P2PVM_NoAuth_Custom_Client.Create(
    TDTClient_NoAuth, C40PhysicsTunnel.PhysicsTunnel,
    ClientInfo.ServiceTyp + 'R', ClientInfo.p2pVM_ClientRecvTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientRecvTunnel_Port),
    ClientInfo.ServiceTyp + 'S', ClientInfo.p2pVM_ClientSendTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientSendTunnel_Port)
    );
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink;

  Client.RecvTunnel.RegisterDirectStream('UpdateServiceInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_UpdateServiceInfo;
  Client.RecvTunnel.RegisterDirectStream('UpdateServiceState').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_UpdateServiceState;
  Client.RecvTunnel.RegisterDirectStream('IgnoreChange').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_IgnoreChange;
  Client.RecvTunnel.RegisterDirectStream('RemovePhysicsNetwork').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemovePhysicsNetwork;

  Client.RecvTunnel.PrintParams['UpdateServiceInfo'] := False;
  Client.RecvTunnel.PrintParams['UpdateServiceState'] := False;
  Client.RecvTunnel.PrintParams['IgnoreChange'] := False;
  Client.RecvTunnel.PrintParams['RequestUpdate'] := False;

  Client.SendTunnel.PrintParams['UpdateServiceInfo'] := False;
  Client.SendTunnel.PrintParams['UpdateServiceState'] := False;
  Client.SendTunnel.PrintParams['IgnoreChange'] := False;
  Client.SendTunnel.PrintParams['RequestUpdate'] := False;

  { register local service. }
  ServiceInfoList := TC40_InfoList.Create(True);
  for i := 0 to C40_ServicePool.Count - 1 do
    if C40_ServicePool[i].C40PhysicsService.Activted then
      if ServiceInfoList.FindSame(C40_ServicePool[i].ServiceInfo) = nil then
          ServiceInfoList.Add(C40_ServicePool[i].ServiceInfo.Clone);

  { check and build network }
  for i := 0 to ServiceInfoList.Count - 1 do
      C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(ServiceInfoList[i], C40PhysicsTunnel.DependNetworkInfoArray, C40PhysicsTunnel.OnEvent);
end;

destructor TC40_Dispatch_Client.Destroy;
begin
  disposeObject(Client);
  disposeObject(ServiceInfoList);
  inherited Destroy;
end;

procedure TC40_Dispatch_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
  if not DelayCheck_Working then
    begin
      DelayCheck_Working := True;
      C40PhysicsTunnel.PhysicsTunnel.PostProgress.PostExecuteM_NP(2.0, {$IFDEF FPC}@{$ENDIF FPC}DoDelayCheckLocalServiceInfo);
    end;
end;

procedure TC40_Dispatch_Client.Connect;
begin
  inherited Connect;
  Client.Connect();
end;

function TC40_Dispatch_Client.Connected: Boolean;
begin
  Result := Client.DTClient.LinkOk;
end;

procedure TC40_Dispatch_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

procedure TC40_Dispatch_Client.PostLocalServiceInfo(forcePost_: Boolean);
var
  i: Integer;
  isChange_: Boolean;
  info: TC40_Info;
  D: TDFE;
begin
  isChange_ := False;
  for i := 0 to C40_ServicePool.Count - 1 do
    if C40_ServicePool[i].C40PhysicsService.Activted then
      begin
        info := ServiceInfoList.FindSame(C40_ServicePool[i].ServiceInfo);
        if info = nil then
          begin
            ServiceInfoList.Add(C40_ServicePool[i].ServiceInfo.Clone);
            isChange_ := True;
          end
        else
            info.Assign(C40_ServicePool[i].ServiceInfo);
      end;

  if isChange_ or forcePost_ then
    begin
      D := TDFE.Create;
      ServiceInfoList.SaveToDF(D);
      Client.SendTunnel.SendDirectStreamCmd('UpdateServiceInfo', D);
      disposeObject(D);
    end;
end;

procedure TC40_Dispatch_Client.RequestUpdate;
begin
  Client.SendTunnel.SendDirectStreamCmd('RequestUpdate');
end;

procedure TC40_Dispatch_Client.IgnoreChangeToService(Hash__: TMD5; Ignored: Boolean);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteMD5(Hash__);
  D.WriteBool(Ignored);
  Client.SendTunnel.SendDirectStreamCmd('IgnoreChange', D);
  disposeObject(D);
end;

procedure TC40_Dispatch_Client.UpdateLocalServiceState;
var
  i: Integer;
  D, tmp: TDFE;
  info_: TC40_Info;
begin
  D := TDFE.Create;
  for i := 0 to C40_ServicePool.Count - 1 do
    if C40_ServicePool[i].C40PhysicsService.Activted then
      begin
        info_ := C40_ServicePool[i].ServiceInfo;
        tmp := TDFE.Create;
        tmp.WriteMD5(info_.Hash);
        tmp.WriteInteger(info_.Workload);
        tmp.WriteInteger(info_.MaxWorkload);
        D.WriteDataFrame(tmp);
        disposeObject(tmp);
      end;
  Client.SendTunnel.SendDirectStreamCmd('UpdateServiceState', D);
  disposeObject(D);
end;

procedure TC40_Dispatch_Client.RemovePhysicsNetwork(PhysicsAddr: U_String; PhysicsPort: Word);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(PhysicsAddr);
  D.WriteWord(PhysicsPort);
  Client.SendTunnel.SendDirectStreamCmd('RemovePhysicsNetwork', D);
  disposeObject(D);
end;

destructor TC40_RegistedDataList.Destroy;
begin
  Clean;
  inherited Destroy;
end;

procedure TC40_RegistedDataList.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    begin
      Items[i]^.ServiceTyp := '';
      Dispose(Items[i]);
    end;
  inherited Clear;
end;

procedure TC40_RegistedDataList.Print;
var
  i: Integer;
  p: PC40_RegistedData;
begin
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      DoStatus('Type "%s" Service "%s" Client "%s"', [p^.ServiceTyp.Text, p^.ServiceClass.ClassName, p^.ClientClass.ClassName]);
    end;
end;

procedure TC40_Base_NoAuth_Service.DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TC40_Base_NoAuth_Service.DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoUserOut(UserDefineIO);
end;

constructor TC40_Base_NoAuth_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_NoAuth_Custom_Service.Create(TDTService_NoAuth, PhysicsService_.PhysicsTunnel,
    ServiceInfo.ServiceTyp + 'R', ServiceInfo.p2pVM_RecvTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_RecvTunnel_Port),
    ServiceInfo.ServiceTyp + 'S', ServiceInfo.p2pVM_SendTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_SendTunnel_Port)
    );
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.FileSystem := EStrToBool(ParamList.GetDefaultValue('FileSystem', umlBoolToStr(Service.DTService.FileSystem)), Service.DTService.FileSystem);
  Service.DTService.PublicFileDirectory := umlCombinePath(C40_RootPath, ServiceInfo.ServiceTyp.Text);
  if not umlDirectoryExists(Service.DTService.PublicFileDirectory) then
      umlCreateDirectory(Service.DTService.PublicFileDirectory);
  DTNoAuthService := Service.DTService;
  UpdateToGlobalDispatch;
end;

destructor TC40_Base_NoAuth_Service.Destroy;
begin
  disposeObject(Service);
  inherited Destroy;
end;

procedure TC40_Base_NoAuth_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
  ServiceInfo.Workload := Service.DTService.RecvTunnel.Count + Service.DTService.SendTunnel.Count;
end;

procedure TC40_Base_NoAuth_Client.Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client);
begin
  DoClientConnected();
end;

constructor TC40_Base_NoAuth_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_NoAuth_Custom_Client.Create(
    TDTClient_NoAuth, C40PhysicsTunnel.PhysicsTunnel,
    ClientInfo.ServiceTyp + 'R', ClientInfo.p2pVM_ClientRecvTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientRecvTunnel_Port),
    ClientInfo.ServiceTyp + 'S', ClientInfo.p2pVM_ClientSendTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientSendTunnel_Port)
    );
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink;
  DTNoAuthClient := Client.DTClient;
end;

destructor TC40_Base_NoAuth_Client.Destroy;
begin
  disposeObject(Client);
  inherited Destroy;
end;

procedure TC40_Base_NoAuth_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TC40_Base_NoAuth_Client.Connect;
begin
  inherited Connect;
  Client.Connect();
end;

function TC40_Base_NoAuth_Client.Connected: Boolean;
begin
  Result := Client.DTClient.LinkOk;
end;

procedure TC40_Base_NoAuth_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

procedure TC40_Base_DataStoreNoAuth_Service.DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TC40_Base_DataStoreNoAuth_Service.DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoUserOut(UserDefineIO);
end;

constructor TC40_Base_DataStoreNoAuth_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_NoAuth_Custom_Service.Create(TDataStoreService_NoAuth, PhysicsService_.PhysicsTunnel,
    ServiceInfo.ServiceTyp + 'R', ServiceInfo.p2pVM_RecvTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_RecvTunnel_Port),
    ServiceInfo.ServiceTyp + 'S', ServiceInfo.p2pVM_SendTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_SendTunnel_Port)
    );
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.FileSystem := EStrToBool(ParamList.GetDefaultValue('FileSystem', umlBoolToStr(Service.DTService.FileSystem)), Service.DTService.FileSystem);
  Service.DTService.PublicFileDirectory := umlCombinePath(C40_RootPath, ServiceInfo.ServiceTyp.Text);
  if not umlDirectoryExists(Service.DTService.PublicFileDirectory) then
      umlCreateDirectory(Service.DTService.PublicFileDirectory);
  DTNoAuthService := Service.DTService as TDataStoreService_NoAuth;
  UpdateToGlobalDispatch;
end;

destructor TC40_Base_DataStoreNoAuth_Service.Destroy;
begin
  disposeObject(Service);
  inherited Destroy;
end;

procedure TC40_Base_DataStoreNoAuth_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
  ServiceInfo.Workload := Service.DTService.RecvTunnel.Count + Service.DTService.SendTunnel.Count;
end;

procedure TC40_Base_DataStoreNoAuth_Client.Do_DT_P2PVM_DataStoreNoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client);
begin
  DoClientConnected();
end;

constructor TC40_Base_DataStoreNoAuth_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_NoAuth_Custom_Client.Create(
    TDataStoreClient_NoAuth, C40PhysicsTunnel.PhysicsTunnel,
    ClientInfo.ServiceTyp + 'R', ClientInfo.p2pVM_ClientRecvTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientRecvTunnel_Port),
    ClientInfo.ServiceTyp + 'S', ClientInfo.p2pVM_ClientSendTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientSendTunnel_Port)
    );
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_DataStoreNoAuth_Custom_Client_TunnelLink;
  DTNoAuthClient := Client.DTClient as TDataStoreClient_NoAuth;
end;

destructor TC40_Base_DataStoreNoAuth_Client.Destroy;
begin
  disposeObject(Client);
  inherited Destroy;
end;

procedure TC40_Base_DataStoreNoAuth_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TC40_Base_DataStoreNoAuth_Client.Connect;
begin
  inherited Connect;
  Client.Connect();
end;

function TC40_Base_DataStoreNoAuth_Client.Connected: Boolean;
begin
  Result := Client.DTClient.LinkOk;
end;

procedure TC40_Base_DataStoreNoAuth_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

procedure TC40_Base_VirtualAuth_Service.DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO);
begin
  RegIO.Accept;
end;

procedure TC40_Base_VirtualAuth_Service.DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO);
begin
  AuthIO.Accept;
end;

procedure TC40_Base_VirtualAuth_Service.DoLinkSuccess_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TC40_Base_VirtualAuth_Service.DoUserOut_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  DoUserOut(UserDefineIO);
end;

constructor TC40_Base_VirtualAuth_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_VirtualAuth_Custom_Service.Create(TDTService_VirtualAuth, PhysicsService_.PhysicsTunnel,
    ServiceInfo.ServiceTyp + 'R', ServiceInfo.p2pVM_RecvTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_RecvTunnel_Port),
    ServiceInfo.ServiceTyp + 'S', ServiceInfo.p2pVM_SendTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_SendTunnel_Port)
    );
  Service.DTService.OnUserAuth := {$IFDEF FPC}@{$ENDIF FPC}DoUserAuth_Event;
  Service.DTService.OnUserReg := {$IFDEF FPC}@{$ENDIF FPC}DoUserReg_Event;
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.FileSystem := EStrToBool(ParamList.GetDefaultValue('FileSystem', umlBoolToStr(Service.DTService.FileSystem)), Service.DTService.FileSystem);
  Service.DTService.PublicFileDirectory := umlCombinePath(C40_RootPath, ServiceInfo.ServiceTyp.Text);
  if not umlDirectoryExists(Service.DTService.PublicFileDirectory) then
      umlCreateDirectory(Service.DTService.PublicFileDirectory);
  DTVirtualAuthService := Service.DTService;
  UpdateToGlobalDispatch;
end;

destructor TC40_Base_VirtualAuth_Service.Destroy;
begin
  disposeObject(Service);
  inherited Destroy;
end;

procedure TC40_Base_VirtualAuth_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
  ServiceInfo.Workload := Service.DTService.RecvTunnel.Count + Service.DTService.SendTunnel.Count;
end;

procedure TC40_Base_VirtualAuth_Client.Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_VirtualAuth_Custom_Client);
begin
  if Client.LoginIsSuccessed then
    begin
      UserName := Client.LastUser;
      Password := Client.LastPasswd;
      NoDTLink := False;
      Client.RegisterUserAndLogin := False;
    end;
  DoClientConnected();
end;

constructor TC40_Base_VirtualAuth_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_VirtualAuth_Custom_Client.Create(
    TDTClient_VirtualAuth, C40PhysicsTunnel.PhysicsTunnel,
    ClientInfo.ServiceTyp + 'R', ClientInfo.p2pVM_ClientRecvTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientRecvTunnel_Port),
    ClientInfo.ServiceTyp + 'S', ClientInfo.p2pVM_ClientSendTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientSendTunnel_Port)
    );
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink;
  DTVirtualAuthClient := Client.DTClient;
  UserName := ParamList.GetDefaultValue('UserName', '');
  Password := ParamList.GetDefaultValue('Password', '');
  Client.RegisterUserAndLogin := EStrToBool(ParamList.GetDefaultValue('RegUser', 'False'), False);
  NoDTLink := EStrToBool(ParamList.GetDefaultValue('NoDTLink', 'True'), True);
end;

destructor TC40_Base_VirtualAuth_Client.Destroy;
begin
  disposeObject(Client);
  inherited Destroy;
end;

procedure TC40_Base_VirtualAuth_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TC40_Base_VirtualAuth_Client.Connect;
begin
  inherited Connect;
  if not NoDTLink then
      Client.Connect(UserName, Password);
end;

function TC40_Base_VirtualAuth_Client.Connected: Boolean;
begin
  if NoDTLink then
      Result := Client.DTClient.RecvTunnel.RemoteInited and Client.DTClient.SendTunnel.RemoteInited
  else
      Result := Client.DTClient.LinkOk;
end;

procedure TC40_Base_VirtualAuth_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

function TC40_Base_VirtualAuth_Client.LoginIsSuccessed: Boolean;
begin
  Result := Client.LoginIsSuccessed;
end;

procedure TC40_Base_DataStoreVirtualAuth_Service.DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO);
begin
  RegIO.Accept;
end;

procedure TC40_Base_DataStoreVirtualAuth_Service.DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO);
begin
  AuthIO.Accept;
end;

procedure TC40_Base_DataStoreVirtualAuth_Service.DoLinkSuccess_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TC40_Base_DataStoreVirtualAuth_Service.DoUserOut_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  DoUserOut(UserDefineIO);
end;

constructor TC40_Base_DataStoreVirtualAuth_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_VirtualAuth_Custom_Service.Create(TDataStoreService_VirtualAuth, PhysicsService_.PhysicsTunnel,
    ServiceInfo.ServiceTyp + 'R', ServiceInfo.p2pVM_RecvTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_RecvTunnel_Port),
    ServiceInfo.ServiceTyp + 'S', ServiceInfo.p2pVM_SendTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_SendTunnel_Port)
    );
  Service.DTService.OnUserAuth := {$IFDEF FPC}@{$ENDIF FPC}DoUserAuth_Event;
  Service.DTService.OnUserReg := {$IFDEF FPC}@{$ENDIF FPC}DoUserReg_Event;
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.FileSystem := EStrToBool(ParamList.GetDefaultValue('FileSystem', umlBoolToStr(Service.DTService.FileSystem)), Service.DTService.FileSystem);
  Service.DTService.PublicFileDirectory := umlCombinePath(C40_RootPath, ServiceInfo.ServiceTyp.Text);
  if not umlDirectoryExists(Service.DTService.PublicFileDirectory) then
      umlCreateDirectory(Service.DTService.PublicFileDirectory);
  DTVirtualAuthService := Service.DTService as TDataStoreService_VirtualAuth;
  UpdateToGlobalDispatch;
end;

destructor TC40_Base_DataStoreVirtualAuth_Service.Destroy;
begin
  disposeObject(Service);
  inherited Destroy;
end;

procedure TC40_Base_DataStoreVirtualAuth_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
  ServiceInfo.Workload := Service.DTService.RecvTunnel.Count + Service.DTService.SendTunnel.Count;
end;

procedure TC40_Base_DataStoreVirtualAuth_Client.Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_VirtualAuth_Custom_Client);
begin
  if Client.LoginIsSuccessed then
    begin
      UserName := Client.LastUser;
      Password := Client.LastPasswd;
      NoDTLink := False;
      Client.RegisterUserAndLogin := False;
    end;
  DoClientConnected();
end;

constructor TC40_Base_DataStoreVirtualAuth_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_VirtualAuth_Custom_Client.Create(
    TDataStoreClient_VirtualAuth, C40PhysicsTunnel.PhysicsTunnel,
    ClientInfo.ServiceTyp + 'R', ClientInfo.p2pVM_ClientRecvTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientRecvTunnel_Port),
    ClientInfo.ServiceTyp + 'S', ClientInfo.p2pVM_ClientSendTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientSendTunnel_Port)
    );
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink;
  DTVirtualAuthClient := Client.DTClient as TDataStoreClient_VirtualAuth;
  UserName := ParamList.GetDefaultValue('UserName', '');
  Password := ParamList.GetDefaultValue('Password', '');
  Client.RegisterUserAndLogin := EStrToBool(ParamList.GetDefaultValue('RegUser', 'False'), False);
  NoDTLink := EStrToBool(ParamList.GetDefaultValue('NoDTLink', 'True'), True);
end;

destructor TC40_Base_DataStoreVirtualAuth_Client.Destroy;
begin
  disposeObject(Client);
  inherited Destroy;
end;

procedure TC40_Base_DataStoreVirtualAuth_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TC40_Base_DataStoreVirtualAuth_Client.Connect;
begin
  inherited Connect;
  if not NoDTLink then
      Client.Connect(UserName, Password);
end;

function TC40_Base_DataStoreVirtualAuth_Client.Connected: Boolean;
begin
  if NoDTLink then
      Result := Client.DTClient.RecvTunnel.RemoteInited and Client.DTClient.SendTunnel.RemoteInited
  else
      Result := Client.DTClient.LinkOk;
end;

procedure TC40_Base_DataStoreVirtualAuth_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

function TC40_Base_DataStoreVirtualAuth_Client.LoginIsSuccessed: Boolean;
begin
  Result := Client.LoginIsSuccessed;
end;

procedure TC40_Base_Service.DoLinkSuccess_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TC40_Base_Service.DoUserOut_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  DoUserOut(UserDefineIO);
end;

constructor TC40_Base_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_Custom_Service.Create(TDTService, PhysicsService_.PhysicsTunnel,
    ServiceInfo.ServiceTyp + 'R', ServiceInfo.p2pVM_RecvTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_RecvTunnel_Port),
    ServiceInfo.ServiceTyp + 'S', ServiceInfo.p2pVM_SendTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_SendTunnel_Port)
    );
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.AllowRegisterNewUser := True;
  Service.DTService.AllowSaveUserInfo := True;
  Service.DTService.FileSystem := EStrToBool(ParamList.GetDefaultValue('FileSystem', umlBoolToStr(Service.DTService.FileSystem)), Service.DTService.FileSystem);
  Service.DTService.PublicPath := umlCombinePath(C40_RootPath, ServiceInfo.ServiceTyp.Text);
  Service.DTService.RootPath := Service.DTService.PublicPath;
  if not umlDirectoryExists(Service.DTService.PublicPath) then
      umlCreateDirectory(Service.DTService.PublicPath);
  DTService := Service.DTService;
  UpdateToGlobalDispatch;
end;

destructor TC40_Base_Service.Destroy;
begin
  disposeObject(Service);
  inherited Destroy;
end;

procedure TC40_Base_Service.SafeCheck;
begin
  inherited SafeCheck;
  Service.DTService.SaveUserDB;
end;

procedure TC40_Base_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
  ServiceInfo.Workload := Service.DTService.RecvTunnel.Count + Service.DTService.SendTunnel.Count;
end;

procedure TC40_Base_Client.Do_DT_P2PVM_Custom_Client_TunnelLink(Sender: TDT_P2PVM_Custom_Client);
begin
  if Client.LoginIsSuccessed then
    begin
      UserName := Client.LastUser;
      Password := Client.LastPasswd;
      NoDTLink := False;
      Client.RegisterUserAndLogin := False;
    end;
  DoClientConnected();
end;

constructor TC40_Base_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_Custom_Client.Create(
    TDTClient, C40PhysicsTunnel.PhysicsTunnel,
    ClientInfo.ServiceTyp + 'R', ClientInfo.p2pVM_ClientRecvTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientRecvTunnel_Port),
    ClientInfo.ServiceTyp + 'S', ClientInfo.p2pVM_ClientSendTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientSendTunnel_Port)
    );
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_Custom_Client_TunnelLink;
  DTClient := Client.DTClient;
  UserName := ParamList.GetDefaultValue('UserName', '');
  Password := ParamList.GetDefaultValue('Password', '');
  Client.RegisterUserAndLogin := EStrToBool(ParamList.GetDefaultValue('RegUser', 'False'), False);
  NoDTLink := EStrToBool(ParamList.GetDefaultValue('NoDTLink', 'True'), True);
end;

destructor TC40_Base_Client.Destroy;
begin
  disposeObject(Client);
  inherited Destroy;
end;

procedure TC40_Base_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TC40_Base_Client.Connect;
begin
  inherited Connect;
  if not NoDTLink then
      Client.Connect(UserName, Password);
end;

function TC40_Base_Client.Connected: Boolean;
begin
  if NoDTLink then
      Result := Client.DTClient.RecvTunnel.RemoteInited and Client.DTClient.SendTunnel.RemoteInited
  else
      Result := Client.DTClient.LinkOk;
end;

procedure TC40_Base_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

function TC40_Base_Client.LoginIsSuccessed: Boolean;
begin
  Result := Client.LoginIsSuccessed;
end;

procedure TC40_Base_DataStore_Service.DoLinkSuccess_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TC40_Base_DataStore_Service.DoUserOut_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  DoUserOut(UserDefineIO);
end;

constructor TC40_Base_DataStore_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_Custom_Service.Create(TDataStoreService, PhysicsService_.PhysicsTunnel,
    ServiceInfo.ServiceTyp + 'R', ServiceInfo.p2pVM_RecvTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_RecvTunnel_Port),
    ServiceInfo.ServiceTyp + 'S', ServiceInfo.p2pVM_SendTunnel_Addr, umlIntToStr(ServiceInfo.p2pVM_SendTunnel_Port)
    );
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.AllowRegisterNewUser := True;
  Service.DTService.AllowSaveUserInfo := True;
  Service.DTService.FileSystem := EStrToBool(ParamList.GetDefaultValue('FileSystem', umlBoolToStr(Service.DTService.FileSystem)), Service.DTService.FileSystem);
  Service.DTService.PublicPath := umlCombinePath(C40_RootPath, ServiceInfo.ServiceTyp.Text);
  Service.DTService.RootPath := Service.DTService.PublicPath;
  if not umlDirectoryExists(Service.DTService.PublicPath) then
      umlCreateDirectory(Service.DTService.PublicPath);

  DTService := Service.DTService as TDataStoreService;
  UpdateToGlobalDispatch;
end;

destructor TC40_Base_DataStore_Service.Destroy;
begin
  disposeObject(Service);
  inherited Destroy;
end;

procedure TC40_Base_DataStore_Service.SafeCheck;
begin
  inherited SafeCheck;
  Service.DTService.SaveUserDB;
end;

procedure TC40_Base_DataStore_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
  ServiceInfo.Workload := Service.DTService.RecvTunnel.Count + Service.DTService.SendTunnel.Count;
end;

procedure TC40_Base_DataStore_Client.Do_DT_P2PVM_Custom_Client_TunnelLink(Sender: TDT_P2PVM_Custom_Client);
begin
  if Client.LoginIsSuccessed then
    begin
      UserName := Client.LastUser;
      Password := Client.LastPasswd;
      NoDTLink := False;
      Client.RegisterUserAndLogin := False;
    end;
  DoClientConnected();
end;

constructor TC40_Base_DataStore_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_Custom_Client.Create(
    TDataStoreClient, C40PhysicsTunnel.PhysicsTunnel,
    ClientInfo.ServiceTyp + 'R', ClientInfo.p2pVM_ClientRecvTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientRecvTunnel_Port),
    ClientInfo.ServiceTyp + 'S', ClientInfo.p2pVM_ClientSendTunnel_Addr, umlIntToStr(ClientInfo.p2pVM_ClientSendTunnel_Port)
    );
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_Custom_Client_TunnelLink;
  DTClient := Client.DTClient as TDataStoreClient;
  UserName := ParamList.GetDefaultValue('UserName', '');
  Password := ParamList.GetDefaultValue('Password', '');
  Client.RegisterUserAndLogin := EStrToBool(ParamList.GetDefaultValue('RegUser', 'False'), False);
  NoDTLink := EStrToBool(ParamList.GetDefaultValue('NoDTLink', 'True'), True);
end;

destructor TC40_Base_DataStore_Client.Destroy;
begin
  disposeObject(Client);
  inherited Destroy;
end;

procedure TC40_Base_DataStore_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TC40_Base_DataStore_Client.Connect;
begin
  inherited Connect;
  if not NoDTLink then
      Client.Connect(UserName, Password);
end;

function TC40_Base_DataStore_Client.Connected: Boolean;
begin
  if NoDTLink then
      Result := Client.DTClient.RecvTunnel.RemoteInited and Client.DTClient.SendTunnel.RemoteInited
  else
      Result := Client.DTClient.LinkOk;
end;

procedure TC40_Base_DataStore_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

function TC40_Base_DataStore_Client.LoginIsSuccessed: Boolean;
begin
  Result := Client.LoginIsSuccessed;
end;

initialization

// init
ProgressBackgroundProc := {$IFDEF FPC}@{$ENDIF FPC}C40Progress;

C40_QuietMode := False;
C40_SafeCheckTime := 1000 * 60;
C40_PhysicsReconnectionDelayTime := 5.0;
C40_UpdateServiceInfoDelayTime := 1000 * 1;
C40_PhysicsServiceTimeout := 1000 * 60 * 5;
C40_PhysicsTunnelTimeout := 1000 * 60 * 5;
C40_KillDeadPhysicsConnectionTimeout := 1000 * 5;
C40_KillIDCFaultTimeout := 1000 * 60 * 60;

{$IFDEF FPC}
C40_RootPath := umlCurrentPath;
{$ELSE FPC}
C40_RootPath := TPath.GetLibraryPath;
{$ENDIF FPC}

C40_PhysicsClientClass := Z.Net.PhysicsIO.TPhysicsClient;
C40_Registed := TC40_RegistedDataList.Create;
C40_PhysicsServicePool := TC40_PhysicsServicePool.Create;
C40_ServicePool := TC40_Custom_ServicePool.Create;
C40_PhysicsTunnelPool := TC40_PhysicsTunnelPool.Create;
C40_ClientPool := TC40_Custom_ClientPool.Create;

// build-in registration
RegisterC40('DP', TC40_Dispatch_Service, TC40_Dispatch_Client);
RegisterC40('NA', TC40_Base_NoAuth_Service, TC40_Base_NoAuth_Client);
RegisterC40('DNA', TC40_Base_DataStoreNoAuth_Service, TC40_Base_DataStoreNoAuth_Client);
RegisterC40('VA', TC40_Base_VirtualAuth_Service, TC40_Base_VirtualAuth_Client);
RegisterC40('DVA', TC40_Base_DataStoreVirtualAuth_Service, TC40_Base_DataStoreVirtualAuth_Client);
RegisterC40('D', TC40_Base_Service, TC40_Base_Client);
RegisterC40('DD', TC40_Base_DataStore_Service, TC40_Base_DataStore_Client);

// backup
C40_DefaultConfig := THashStringList.CustomCreate(8);
C40WriteConfig(C40_DefaultConfig);

finalization

C40Clean;

disposeObject(C40_PhysicsServicePool);
disposeObject(C40_ServicePool);
disposeObject(C40_PhysicsTunnelPool);
disposeObject(C40_ClientPool);
disposeObject(C40_Registed);
disposeObject(C40_DefaultConfig);

end.
