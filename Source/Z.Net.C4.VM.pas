{ ****************************************************************************** }
{ * cloud 4.0 framework-VM                                                     * }
{ ****************************************************************************** }
unit Z.Net.C4.VM;

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
  Z.Net.DataStoreService.NoAuth,
  Z.Net.C4;

type
  TC40_Custom_VM_Service = class;
  TC40_Custom_VM_Client = class;

  TC40_Custom_VM_Service = class(TCore_InterfacedObject)
  private
    FLastSafeCheckTime: TTimeTick;
  public
    Param: U_String;
    ParamList: THashStringList;
    SafeCheckTime: TTimeTick;
    constructor Create(Param_: U_String); virtual;
    destructor Destroy; override;
    procedure SafeCheck; virtual;
    procedure Progress; virtual;
    { event }
    procedure DoLinkSuccess(Trigger_: TCore_Object);
    procedure DoUserOut(Trigger_: TCore_Object);
  end;

  TOn_Client_Offline = procedure(Sender: TC40_Custom_VM_Client) of object;

  TC40_Custom_VM_Client = class(TCore_InterfacedObject)
  private
    FLastSafeCheckTime: TTimeTick;
  public
    Param: U_String;
    ParamList: THashStringList;
    SafeCheckTime: TTimeTick;
    On_Client_Offline: TOn_Client_Offline;
    constructor Create(Param_: U_String); virtual;
    destructor Destroy; override;
    procedure SafeCheck; virtual;
    procedure Progress; virtual;
    function Connected: Boolean; virtual;
    procedure Disconnect; virtual;
    { event }
    procedure DoNetworkOnline; virtual;  { trigger: connected }
    procedure DoNetworkOffline; virtual; { trigger: offline }
  end;

  TC40_NoAuth_VM_Service = class(TC40_Custom_VM_Service)
  protected
    procedure DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
    procedure DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
  public
    Service: TDT_P2PVM_NoAuth_Service;
    DTNoAuthService: TDTService_NoAuth;
    property DTNoAuth: TDTService_NoAuth read DTNoAuthService;
    class function Get_Service_Class: TDTService_NoAuthClass; virtual;
    constructor Create(Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure StartService(ListenAddr, ListenPort, Auth: SystemString);
    procedure StopService;
  end;

  TC40_NoAuth_VM_Client = class(TC40_Custom_VM_Client, IZNet_ClientInterface)
  protected
    procedure ClientConnected(Sender: TZNet_Client); virtual;
    procedure ClientDisconnect(Sender: TZNet_Client); virtual;
    procedure Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Client); virtual;
  public
    Client: TDT_P2PVM_NoAuth_Client;
    DTNoAuthClient: TDTClient_NoAuth;
    property DTNoAuth: TDTClient_NoAuth read DTNoAuthClient;
    class function Get_Client_Class: TDTClient_NoAuthClass; virtual;
    constructor Create(Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect(addr, Port, Auth: SystemString);
    function Connected: Boolean; override;
    procedure Disconnect; override;
  end;

  TC40_DataStore_NoAuth_VM_Service = class(TC40_NoAuth_VM_Service)
  public
    function Get_DT_DataStore_NoAuth: TDataStoreService_NoAuth;
    property DT_DataStore_NoAuth: TDataStoreService_NoAuth read Get_DT_DataStore_NoAuth;
    class function Get_Service_Class: TDTService_NoAuthClass; override;
  end;

  TC40_DataStore_NoAuth_VM_Client = class(TC40_NoAuth_VM_Client)
  public
    function Get_DT_DataStore_NoAuth: TDataStoreClient_NoAuth;
    property DT_DataStore_NoAuth: TDataStoreClient_NoAuth read Get_DT_DataStore_NoAuth;
    class function Get_Client_Class: TDTClient_NoAuthClass; override;
  end;

  TC40_VirtualAuth_VM_Service = class(TC40_Custom_VM_Service)
  protected
    procedure DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO); virtual;
    procedure DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO); virtual;
    procedure DoLinkSuccess_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); virtual;
    procedure DoUserOut_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); virtual;
  public
    Service: TDT_P2PVM_VirtualAuth_Service;
    DTVirtualAuthService: TDTService_VirtualAuth;
    property DTVirtualAuth: TDTService_VirtualAuth read DTVirtualAuthService;
    class function Get_Service_Class: TDTService_VirtualAuthClass; virtual;
    constructor Create(Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure StartService(ListenAddr, ListenPort, Auth: SystemString);
    procedure StopService;
  end;

  TC40_VirtualAuth_VM_Client = class(TC40_Custom_VM_Client, IZNet_ClientInterface)
  protected
    procedure ClientConnected(Sender: TZNet_Client); virtual;
    procedure ClientDisconnect(Sender: TZNet_Client); virtual;
    procedure Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_VirtualAuth_Client); virtual;
  public
    Client: TDT_P2PVM_VirtualAuth_Client;
    DTVirtualAuthClient: TDTClient_VirtualAuth;
    property DTVirtualAuth: TDTClient_VirtualAuth read DTVirtualAuthClient;
    class function Get_Client_Class: TDTClient_VirtualAuthClass; virtual;
    constructor Create(Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect(addr, Port, Auth, User, Passwd: SystemString);
    function Connected: Boolean; override;
    procedure Disconnect; override;
  end;

  TC40_DataStore_VirtualAuth_VM_Service = class(TC40_VirtualAuth_VM_Service)
  public
    function Get_DT_DataStore_VirtualAuth: TDataStoreService_VirtualAuth;
    property DT_DataStore_VirtualAuth: TDataStoreService_VirtualAuth read Get_DT_DataStore_VirtualAuth;
    class function Get_Service_Class: TDTService_VirtualAuthClass; override;
  end;

  TC40_DataStore_VirtualAuth_VM_Client = class(TC40_VirtualAuth_VM_Client)
  public
    function Get_DT_DataStore_VirtualAuth: TDataStoreClient_VirtualAuth;
    property DT_DataStore_VirtualAuth: TDataStoreClient_VirtualAuth read Get_DT_DataStore_VirtualAuth;
    class function Get_Client_Class: TDTClient_VirtualAuthClass; override;
  end;

  TC40_VM_Service = class(TC40_Custom_VM_Service)
  protected
    procedure DoLinkSuccess_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
    procedure DoUserOut_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
  public
    Service: TDT_P2PVM_Service;
    DTVirtualAuthService: TDTService;
    property DTVirtualAuth: TDTService read DTVirtualAuthService;
    class function Get_Service_Class: TDTServiceClass; virtual;
    constructor Create(Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure StartService(ListenAddr, ListenPort, Auth: SystemString);
    procedure StopService;
  end;

  TC40_VM_Client = class(TC40_Custom_VM_Client, IZNet_ClientInterface)
  protected
    procedure ClientConnected(Sender: TZNet_Client); virtual;
    procedure ClientDisconnect(Sender: TZNet_Client); virtual;
    procedure Do_DT_P2PVM_Custom_Client_TunnelLink(Sender: TDT_P2PVM_Client); virtual;
  public
    Client: TDT_P2PVM_Client;
    DTVirtualAuthClient: TDTClient;
    property DTVirtualAuth: TDTClient read DTVirtualAuthClient;
    class function Get_Client_Class: TDTClientClass; virtual;
    constructor Create(Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Connect(addr, Port, Auth, User, Passwd: SystemString);
    function Connected: Boolean; override;
    procedure Disconnect; override;
  end;

  TC40_DataStore_VM_Service = class(TC40_VM_Service)
  public
    function Get_DT_DataStore: TDataStoreService;
    property DT_DataStore: TDataStoreService read Get_DT_DataStore;
    class function Get_Service_Class: TDTServiceClass; override;
  end;

  TC40_DataStore_VM_Client = class(TC40_VM_Client)
  public
    function Get_DT_DataStore: TDataStoreClient;
    property DT_DataStore: TDataStoreClient read Get_DT_DataStore;
    class function Get_Client_Class: TDTClientClass; override;
  end;

implementation

constructor TC40_Custom_VM_Service.Create(Param_: U_String);
var
  tmp: TPascalStringList;
begin
  inherited Create;

  Param := Param_;

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
end;

destructor TC40_Custom_VM_Service.Destroy;
begin
  disposeObject(ParamList);
  inherited Destroy;
end;

procedure TC40_Custom_VM_Service.SafeCheck;
begin

end;

procedure TC40_Custom_VM_Service.Progress;
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

procedure TC40_Custom_VM_Service.DoLinkSuccess(Trigger_: TCore_Object);
begin

end;

procedure TC40_Custom_VM_Service.DoUserOut(Trigger_: TCore_Object);
begin

end;

constructor TC40_Custom_VM_Client.Create(Param_: U_String);
var
  tmp: TPascalStringList;
begin
  inherited Create;
  Param := Param_;

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
  On_Client_Offline := nil;
end;

destructor TC40_Custom_VM_Client.Destroy;
begin
  disposeObject(ParamList);
  inherited Destroy;
end;

procedure TC40_Custom_VM_Client.SafeCheck;
begin

end;

procedure TC40_Custom_VM_Client.Progress;
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

function TC40_Custom_VM_Client.Connected: Boolean;
begin
  Result := False;
end;

procedure TC40_Custom_VM_Client.Disconnect;
begin

end;

procedure TC40_Custom_VM_Client.DoNetworkOnline;
begin

end;

procedure TC40_Custom_VM_Client.DoNetworkOffline;
begin
  try
    if Assigned(On_Client_Offline) then
        On_Client_Offline(Self);
  except
  end;
end;

procedure TC40_NoAuth_VM_Service.DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TC40_NoAuth_VM_Service.DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoUserOut(UserDefineIO);
end;

class function TC40_NoAuth_VM_Service.Get_Service_Class: TDTService_NoAuthClass;
begin
  Result := TDTService_NoAuth;
end;

constructor TC40_NoAuth_VM_Service.Create(Param_: U_String);
begin
  inherited Create(Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_NoAuth_Service.Create(Get_Service_Class);

  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.FileSystem := EStrToBool(ParamList.GetDefaultValue('FileSystem', umlBoolToStr(Service.DTService.FileSystem)), Service.DTService.FileSystem);
  Service.DTService.PublicFileDirectory := umlCombinePath(C40_RootPath, ClassName);
  if not umlDirectoryExists(Service.DTService.PublicFileDirectory) then
      umlCreateDirectory(Service.DTService.PublicFileDirectory);
  DTNoAuthService := Service.DTService;
end;

destructor TC40_NoAuth_VM_Service.Destroy;
begin
  disposeObject(Service);
  inherited Destroy;
end;

procedure TC40_NoAuth_VM_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
end;

procedure TC40_NoAuth_VM_Service.StartService(ListenAddr, ListenPort, Auth: SystemString);
begin
  Service.StartService(ListenAddr, ListenPort, Auth);
end;

procedure TC40_NoAuth_VM_Service.StopService;
begin
  Service.StopService;
end;

procedure TC40_NoAuth_VM_Client.ClientConnected(Sender: TZNet_Client);
begin

end;

procedure TC40_NoAuth_VM_Client.ClientDisconnect(Sender: TZNet_Client);
begin
  DoNetworkOffline();
end;

procedure TC40_NoAuth_VM_Client.Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Client);
begin
  DoNetworkOnline();
end;

class function TC40_NoAuth_VM_Client.Get_Client_Class: TDTClient_NoAuthClass;
begin
  Result := TDTClient_NoAuth;
end;

constructor TC40_NoAuth_VM_Client.Create(Param_: U_String);
begin
  inherited Create(Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_NoAuth_Client.Create(Get_Client_Class);
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink;
  DTNoAuthClient := Client.DTClient;
  Client.PhysicsTunnel.OnInterface := Self;
end;

destructor TC40_NoAuth_VM_Client.Destroy;
begin

  inherited;
end;

procedure TC40_NoAuth_VM_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TC40_NoAuth_VM_Client.Connect(addr, Port, Auth: SystemString);
begin
  Client.Connect(addr, Port, Auth);
end;

function TC40_NoAuth_VM_Client.Connected: Boolean;
begin
  Result := Client.DTClient.LinkOk;
end;

procedure TC40_NoAuth_VM_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

function TC40_DataStore_NoAuth_VM_Service.Get_DT_DataStore_NoAuth: TDataStoreService_NoAuth;
begin
  Result := DTNoAuthService as TDataStoreService_NoAuth;
end;

class function TC40_DataStore_NoAuth_VM_Service.Get_Service_Class: TDTService_NoAuthClass;
begin
  Result := TDataStoreService_NoAuth;
end;

function TC40_DataStore_NoAuth_VM_Client.Get_DT_DataStore_NoAuth: TDataStoreClient_NoAuth;
begin
  Result := DTNoAuthClient as TDataStoreClient_NoAuth;
end;

class function TC40_DataStore_NoAuth_VM_Client.Get_Client_Class: TDTClient_NoAuthClass;
begin
  Result := TDataStoreClient_NoAuth;
end;

procedure TC40_VirtualAuth_VM_Service.DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO);
begin
  RegIO.Accept;
end;

procedure TC40_VirtualAuth_VM_Service.DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO);
begin
  AuthIO.Accept;
end;

procedure TC40_VirtualAuth_VM_Service.DoLinkSuccess_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TC40_VirtualAuth_VM_Service.DoUserOut_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  DoUserOut(UserDefineIO);
end;

class function TC40_VirtualAuth_VM_Service.Get_Service_Class: TDTService_VirtualAuthClass;
begin
  Result := TDTService_VirtualAuth;
end;

constructor TC40_VirtualAuth_VM_Service.Create(Param_: U_String);
begin
  inherited Create(Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_VirtualAuth_Service.Create(Get_Service_Class);

  Service.DTService.OnUserAuth := {$IFDEF FPC}@{$ENDIF FPC}DoUserAuth_Event;
  Service.DTService.OnUserReg := {$IFDEF FPC}@{$ENDIF FPC}DoUserReg_Event;
  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.FileSystem := EStrToBool(ParamList.GetDefaultValue('FileSystem', umlBoolToStr(Service.DTService.FileSystem)), Service.DTService.FileSystem);
  Service.DTService.PublicFileDirectory := umlCombinePath(C40_RootPath, ClassName);
  if not umlDirectoryExists(Service.DTService.PublicFileDirectory) then
      umlCreateDirectory(Service.DTService.PublicFileDirectory);
  DTVirtualAuthService := Service.DTService;
end;

destructor TC40_VirtualAuth_VM_Service.Destroy;
begin
  disposeObject(Service);
  inherited Destroy;
end;

procedure TC40_VirtualAuth_VM_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
end;

procedure TC40_VirtualAuth_VM_Service.StartService(ListenAddr, ListenPort, Auth: SystemString);
begin
  Service.StartService(ListenAddr, ListenPort, Auth);
end;

procedure TC40_VirtualAuth_VM_Service.StopService;
begin
  Service.StopService;
end;

procedure TC40_VirtualAuth_VM_Client.ClientConnected(Sender: TZNet_Client);
begin

end;

procedure TC40_VirtualAuth_VM_Client.ClientDisconnect(Sender: TZNet_Client);
begin
  DoNetworkOffline();
end;

procedure TC40_VirtualAuth_VM_Client.Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_VirtualAuth_Client);
begin
  DoNetworkOnline();
end;

class function TC40_VirtualAuth_VM_Client.Get_Client_Class: TDTClient_VirtualAuthClass;
begin
  Result := TDTClient_VirtualAuth;
end;

constructor TC40_VirtualAuth_VM_Client.Create(Param_: U_String);
begin
  inherited Create(Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_VirtualAuth_Client.Create(Get_Client_Class);
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink;
  DTVirtualAuthClient := Client.DTClient;
  Client.PhysicsTunnel.OnInterface := Self;
end;

destructor TC40_VirtualAuth_VM_Client.Destroy;
begin

  inherited;
end;

procedure TC40_VirtualAuth_VM_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TC40_VirtualAuth_VM_Client.Connect(addr, Port, Auth, User, Passwd: SystemString);
begin
  Client.Connect(addr, Port, Auth, User, Passwd);
end;

function TC40_VirtualAuth_VM_Client.Connected: Boolean;
begin
  Result := Client.DTClient.LinkOk;
end;

procedure TC40_VirtualAuth_VM_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

function TC40_DataStore_VirtualAuth_VM_Service.Get_DT_DataStore_VirtualAuth: TDataStoreService_VirtualAuth;
begin
  Result := DTVirtualAuthService as TDataStoreService_VirtualAuth;
end;

class function TC40_DataStore_VirtualAuth_VM_Service.Get_Service_Class: TDTService_VirtualAuthClass;
begin
  Result := TDataStoreService_VirtualAuth;
end;

function TC40_DataStore_VirtualAuth_VM_Client.Get_DT_DataStore_VirtualAuth: TDataStoreClient_VirtualAuth;
begin
  Result := DTVirtualAuthClient as TDataStoreClient_VirtualAuth;
end;

class function TC40_DataStore_VirtualAuth_VM_Client.Get_Client_Class: TDTClient_VirtualAuthClass;
begin
  Result := TDataStoreClient_VirtualAuth;
end;

procedure TC40_VM_Service.DoLinkSuccess_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  DoLinkSuccess(UserDefineIO);
end;

procedure TC40_VM_Service.DoUserOut_Event(Sender: TDTService; UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  DoUserOut(UserDefineIO);
end;

class function TC40_VM_Service.Get_Service_Class: TDTServiceClass;
begin
  Result := TDTService;
end;

constructor TC40_VM_Service.Create(Param_: U_String);
begin
  inherited Create(Param_);
  { custom p2pVM service }
  Service := TDT_P2PVM_Service.Create(Get_Service_Class);

  Service.DTService.OnLinkSuccess := {$IFDEF FPC}@{$ENDIF FPC}DoLinkSuccess_Event;
  Service.DTService.OnUserOut := {$IFDEF FPC}@{$ENDIF FPC}DoUserOut_Event;
  Service.DTService.FileSystem := EStrToBool(ParamList.GetDefaultValue('FileSystem', umlBoolToStr(Service.DTService.FileSystem)), Service.DTService.FileSystem);
  Service.DTService.RootPath := umlCombinePath(C40_RootPath, ClassName);
  Service.DTService.PublicPath := Service.DTService.RootPath;
  if not umlDirectoryExists(Service.DTService.RootPath) then
      umlCreateDirectory(Service.DTService.RootPath);
  DTVirtualAuthService := Service.DTService;
end;

destructor TC40_VM_Service.Destroy;
begin
  disposeObject(Service);
  inherited Destroy;
end;

procedure TC40_VM_Service.Progress;
begin
  inherited Progress;
  Service.Progress;
end;

procedure TC40_VM_Service.StartService(ListenAddr, ListenPort, Auth: SystemString);
begin
  Service.StartService(ListenAddr, ListenPort, Auth);
end;

procedure TC40_VM_Service.StopService;
begin
  Service.StopService;
end;

procedure TC40_VM_Client.ClientConnected(Sender: TZNet_Client);
begin

end;

procedure TC40_VM_Client.ClientDisconnect(Sender: TZNet_Client);
begin
  DoNetworkOffline();
end;

procedure TC40_VM_Client.Do_DT_P2PVM_Custom_Client_TunnelLink(Sender: TDT_P2PVM_Client);
begin
  DoNetworkOnline();
end;

class function TC40_VM_Client.Get_Client_Class: TDTClientClass;
begin
  Result := TDTClient;
end;

constructor TC40_VM_Client.Create(Param_: U_String);
begin
  inherited Create(Param_);
  { custom p2pVM client }
  Client := TDT_P2PVM_Client.Create(Get_Client_Class);
  Client.OnTunnelLink := {$IFDEF FPC}@{$ENDIF FPC}Do_DT_P2PVM_Custom_Client_TunnelLink;
  DTVirtualAuthClient := Client.DTClient;
  Client.PhysicsTunnel.OnInterface := Self;
end;

destructor TC40_VM_Client.Destroy;
begin

  inherited;
end;

procedure TC40_VM_Client.Progress;
begin
  inherited Progress;
  Client.Progress;
end;

procedure TC40_VM_Client.Connect(addr, Port, Auth, User, Passwd: SystemString);
begin
  Client.Connect(addr, Port, Auth, User, Passwd);
end;

function TC40_VM_Client.Connected: Boolean;
begin
  Result := Client.DTClient.LinkOk;
end;

procedure TC40_VM_Client.Disconnect;
begin
  inherited Disconnect;
  Client.Disconnect;
end;

function TC40_DataStore_VM_Service.Get_DT_DataStore: TDataStoreService;
begin
  Result := DTVirtualAuthService as TDataStoreService;
end;

class function TC40_DataStore_VM_Service.Get_Service_Class: TDTServiceClass;
begin
  Result := TDataStoreService;
end;

function TC40_DataStore_VM_Client.Get_DT_DataStore: TDataStoreClient;
begin
  Result := DTVirtualAuthClient as TDataStoreClient;
end;

class function TC40_DataStore_VM_Client.Get_Client_Class: TDTClientClass;
begin
  Result := TDataStoreClient;
end;

end.
