{ ****************************************************************************** }
{ * cloud 4.0 XNAT Cluster port mapping                                        * }
{ ****************************************************************************** }
unit Z.Net.C4_XNAT_Cluster_Port_Mapping;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses Variants,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib,
  Z.Geometry2D, Z.DFE, Z.ListEngine,
  Z.Parsing, Z.Expression, Z.OpCode,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth, Z.Net.C4,
  Z.Net.XNAT.Client, Z.Net.XNAT.Service, Z.Net.XNAT.Physics;

type
  TC40_CPM_Info = class
  public
    NoDistributed: Boolean;
    ListenAddr: U_String;
    ListenPort: Word;
    Mapping: U_String;
    TimeOut: TTimeTick;
    User_Data: U_String; // user define
    Test_Listening_Passed: Boolean;
    Activted: Boolean;
    constructor Create();
    destructor Destroy; override;
    procedure Encode(d: TDFE);
    procedure Decode(d: TDFE);
  end;

  TC40_CPM_Info_List_Decl = TGenericsList<TC40_CPM_Info>;

  TC40_CPM_Info_List = class(TC40_CPM_Info_List_Decl)
  public
    procedure Clean;
    function Find_Mapping(Mapping: U_String): TC40_CPM_Info;
  end;

  TC40_CPM_Service_Tool = class(TC40_Base_NoAuth_Service)
  protected
    procedure cmd_Get_CPM_Service(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Get_CPM_Mapping(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Add_CPM_Service_Listening(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Open_CPM_Service_Tunnel(Sender: TPeerIO; InData: TDFE);
  protected
    procedure CC_CPM_Service_Info(var OP_Param: TOpParam);
  public
    XNAT_Physics_Service: TXNATService;
    CPM_List: TC40_CPM_Info_List;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
  end;

  TC40_CPM_Client_Tool = class;

  TON_Get_CPM_MappingC = procedure(Sender: TC40_CPM_Client_Tool; L: TC40_CPM_Info_List);
  TON_Get_CPM_MappingM = procedure(Sender: TC40_CPM_Client_Tool; L: TC40_CPM_Info_List) of object;
{$IFDEF FPC}
  TON_Get_CPM_MappingP = procedure(Sender: TC40_CPM_Client_Tool; L: TC40_CPM_Info_List) is nested;
{$ELSE FPC}
  TON_Get_CPM_MappingP = reference to procedure(Sender: TC40_CPM_Client_Tool; L: TC40_CPM_Info_List);
{$ENDIF FPC}

  TON_Get_CPM_Mapping = class(TOnResult_Bridge)
  public
    Client: TC40_CPM_Client_Tool;
    OnResultC: TON_Get_CPM_MappingC;
    OnResultM: TON_Get_CPM_MappingM;
    OnResultP: TON_Get_CPM_MappingP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TON_End_CPM_Address_Mapping = class(TOnResult_Bridge)
  public
    Client: TC40_CPM_Client_Tool;
    procedure Do_First_Get_CPM_Mapping_And_OpenTunnel(Sender: TC40_CPM_Client_Tool; L: TC40_CPM_Info_List);
    procedure Do_Wait_OpenTunnel_Done(Sender: TXNATClient; State: Boolean);
    procedure Do_Delay_3_Second_Again_OpenTunnel;
    procedure Do_Delay_3_Second_Done;
    procedure Do_Last_Get_CPM_Mapping_Final_State(Sender: TC40_CPM_Client_Tool; L: TC40_CPM_Info_List);
  end;

  TC40_CPM_Client_Tool = class(TC40_Base_NoAuth_Client)
  protected
    procedure Do_Get_CPM_Service(Sender: TPeerIO; Result_: TDFE);
    procedure Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client); override;
  protected
    procedure CC_CPM_Client_Info(var OP_Param: TOpParam);
  public
    Remote_XNAT_ReadyOK: Boolean;
    Remote_XNAT_Host, Remote_XNAT_Port, Remote_XNAT_Auth: U_String;
    XNAT_Physics_Client: TXNATClient;
    CPM_Address_Mapping_Report: TPascalStringList; // local cmp address
    XNAT_Remote_CMP_Info_List: TC40_CPM_Info_List;
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;

    procedure Get_CPM_Mapping();
    procedure Get_CPM_MappingC(OnResult: TON_Get_CPM_MappingC);
    procedure Get_CPM_MappingM(OnResult: TON_Get_CPM_MappingM);
    procedure Get_CPM_MappingP(OnResult: TON_Get_CPM_MappingP);
    procedure Add_CPM_Service_Listening(NoDistributed: Boolean; ListenAddr: U_String; ListenPort: Word; Mapping: U_String; TimeOut: TTimeTick; User_Data: U_String); overload;
    // remote service listening 0.0.0.0 for all ipv4
    procedure Add_CPM_Service_Listening(NoDistributed: Boolean; ListenPort: Word; Mapping: U_String; TimeOut: TTimeTick; User_Data: U_String); overload;
    procedure Open_CPM_Service_Tunnel;
    procedure Begin_CPM_Address_Mapping;
    procedure Add_CPM_Address_Mapping(Mapping, Address, Port: U_String);
    procedure End_CPM_Address_Mapping;
  end;

  TC40_CPM_Client_Tool_List = TGenericsList<TC40_CPM_Client_Tool>;

implementation

constructor TC40_CPM_Info.Create;
begin
  inherited Create;
  NoDistributed := True;
  ListenAddr := '';
  ListenPort := 0;
  Mapping := '';
  TimeOut := 0;
  User_Data := '';
  Test_Listening_Passed := False;
  Activted := False;
end;

destructor TC40_CPM_Info.Destroy;
begin
  NoDistributed := True;
  ListenAddr := '';
  ListenPort := 0;
  Mapping := '';
  TimeOut := 0;
  User_Data := '';
  Test_Listening_Passed := False;
  Activted := False;
  inherited Destroy;
end;

procedure TC40_CPM_Info.Encode(d: TDFE);
begin
  d.WriteBool(NoDistributed);
  d.WriteString(ListenAddr);
  d.WriteWORD(ListenPort);
  d.WriteString(Mapping);
  d.WriteUInt64(TimeOut);
  d.WriteString(User_Data);
  d.WriteBool(Test_Listening_Passed);
  d.WriteBool(Activted);
end;

procedure TC40_CPM_Info.Decode(d: TDFE);
begin
  NoDistributed := d.R.ReadBool;
  ListenAddr := d.R.ReadString;
  ListenPort := d.R.ReadWord;
  Mapping := d.R.ReadString;
  TimeOut := d.R.ReadUInt64;
  User_Data := d.R.ReadString;
  Test_Listening_Passed := d.R.ReadBool;
  Activted := d.R.ReadBool;
end;

procedure TC40_CPM_Info_List.Clean;
var
  i: integer;
begin
  for i := 0 to count - 1 do
      DisposeObject(items[i]);
  inherited Clear;
end;

function TC40_CPM_Info_List.Find_Mapping(Mapping: U_String): TC40_CPM_Info;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to count - 1 do
    if Mapping.Same(@items[i].Mapping) then
        Exit(items[i]);
end;

procedure TC40_CPM_Service_Tool.cmd_Get_CPM_Service(Sender: TPeerIO; InData, OutData: TDFE);
begin
  OutData.WriteString(XNAT_Physics_Service.Host);
  OutData.WriteString(XNAT_Physics_Service.Port);
  OutData.WriteString(XNAT_Physics_Service.AuthToken);
end;

procedure TC40_CPM_Service_Tool.cmd_Get_CPM_Mapping(Sender: TPeerIO; InData, OutData: TDFE);
var
  i: integer;
  info: TC40_CPM_Info;
  tmp: TDFE;
begin
  for i := 0 to XNAT_Physics_Service.ShareListenList.count - 1 do
    begin
      info := XNAT_Physics_Service.ShareListenList[i].UserObject as TC40_CPM_Info;
      info.Test_Listening_Passed := XNAT_Physics_Service.ShareListenList[i].Test_Listening_Passed;
      info.Activted := XNAT_Physics_Service.ShareListenList[i].Activted;
    end;

  for i := 0 to CPM_List.count - 1 do
    begin
      tmp := TDFE.Create;
      CPM_List[i].Encode(tmp);
      OutData.WriteDataFrame(tmp);
      DisposeObject(tmp);
    end;
end;

procedure TC40_CPM_Service_Tool.cmd_Add_CPM_Service_Listening(Sender: TPeerIO; InData: TDFE);
var
  NoDistributed: Boolean;
  ListenAddr: U_String;
  ListenPort: Word;
  Mapping: U_String;
  TimeOut: TTimeTick;
  User_Data: U_String;
  info: TC40_CPM_Info;
  i: integer;
begin
  NoDistributed := InData.R.ReadBool;
  ListenAddr := InData.R.ReadString;
  ListenPort := InData.R.ReadWord;
  Mapping := InData.R.ReadString;
  TimeOut := InData.R.ReadUInt64;
  User_Data := InData.R.ReadString;

  info := nil;
  for i := CPM_List.count - 1 downto 0 do
    if Mapping.Same(@CPM_List[i].Mapping) then
        info := CPM_List[i];

  if info = nil then
    begin
      info := TC40_CPM_Info.Create;
      CPM_List.Add(info);
    end;

  info.NoDistributed := NoDistributed;
  info.ListenAddr := ListenAddr;
  info.ListenPort := ListenPort;
  info.Mapping := Mapping;
  info.TimeOut := TimeOut;
  info.User_Data := User_Data;
end;

procedure TC40_CPM_Service_Tool.cmd_Open_CPM_Service_Tunnel(Sender: TPeerIO; InData: TDFE);
var
  i: integer;
  info: TC40_CPM_Info;
  serv: TXServiceListen;
begin
  XNAT_Physics_Service.Reset;
  XNAT_Physics_Service.Quiet := C40_QuietMode;

  for i := 0 to CPM_List.count - 1 do
    begin
      info := CPM_List[i];
      if info.NoDistributed then
          serv := XNAT_Physics_Service.AddNoDistributedMapping('0.0.0.0', umlIntToStr(info.ListenPort), info.Mapping, info.TimeOut)
      else
          serv := XNAT_Physics_Service.AddMapping('0.0.0.0', umlIntToStr(info.ListenPort), info.Mapping, info.TimeOut);
      serv.UserObject := info;
    end;
  XNAT_Physics_Service.OpenTunnel;
  for i := 0 to XNAT_Physics_Service.ShareListenList.count - 1 do
    begin
      info := XNAT_Physics_Service.ShareListenList[i].UserObject as TC40_CPM_Info;
      info.Test_Listening_Passed := XNAT_Physics_Service.ShareListenList[i].Test_Listening_Passed;
      info.Activted := False;
    end;
end;

procedure TC40_CPM_Service_Tool.CC_CPM_Service_Info(var OP_Param: TOpParam);
var
  i: integer;
  info: TC40_CPM_Info;
begin
  for i := 0 to XNAT_Physics_Service.ShareListenList.count - 1 do
    begin
      info := XNAT_Physics_Service.ShareListenList[i].UserObject as TC40_CPM_Info;
      info.Test_Listening_Passed := XNAT_Physics_Service.ShareListenList[i].Test_Listening_Passed;
      info.Activted := XNAT_Physics_Service.ShareListenList[i].Activted;
    end;

  for i := 0 to CPM_List.count - 1 do
    begin
      info := CPM_List[i];
      DoStatus('NoDistributed:%s listening:%s, mapping:%s, timeout:%dms Listening:%s, activted:%s',
        [umlBoolToStr(info.NoDistributed).Text, Build_Host_URL(info.ListenAddr, info.ListenPort), info.Mapping.Text, info.TimeOut,
          umlBoolToStr(info.Test_Listening_Passed).Text, umlBoolToStr(info.Activted).Text
          ]);
    end;
  DoStatus('C4 XNAT Host:%s Port:%d', [PhysicsService.PhysicsAddr.Text, PhysicsService.PhysicsPort]);
  DoStatus('XNAT Host:%s Port:%s Auth:%s', [XNAT_Physics_Service.Host.Text, XNAT_Physics_Service.Port.Text, XNAT_Physics_Service.AuthToken.Text]);
end;

constructor TC40_CPM_Service_Tool.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  // cmd
  DTNoAuthService.RecvTunnel.RegisterStream('Get_CPM_Service').OnExecute := cmd_Get_CPM_Service;
  DTNoAuthService.RecvTunnel.RegisterStream('Get_CPM_Mapping').OnExecute := cmd_Get_CPM_Mapping;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Add_CPM_Service_Listening').OnExecute := cmd_Add_CPM_Service_Listening;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Open_CPM_Service_Tunnel').OnExecute := cmd_Open_CPM_Service_Tunnel;
  // init XNAT
  XNAT_Physics_Service := TXNATService.Create;
  XNAT_Physics_Service.Host := ParamList.GetDefaultValue('XNAT_Host', PhysicsService_.PhysicsAddr);
  XNAT_Physics_Service.Port := ParamList.GetDefaultValue('XNAT_Port', '9087');
  XNAT_Physics_Service.AuthToken := ParamList.GetDefaultValue('XNAT_Auth', C40_Password);
  XNAT_Physics_Service.MaxVMFragment := ParamList.GetDefaultValue('XNAT_MaxVMFragment', XNAT_Physics_Service.MaxVMFragment);
  XNAT_Physics_Service.ProtocolCompressed := EStrToBool(ParamList.GetDefaultValue('XNAT_Compressed', umlBoolToStr(XNAT_Physics_Service.ProtocolCompressed)));
  XNAT_Physics_Service.Quiet := C40_QuietMode;
  // XNAT define constainer
  CPM_List := TC40_CPM_Info_List.Create;
  // instance
  ServiceInfo.OnlyInstance := False;
  UpdateToGlobalDispatch;
  ParamList.SetDefaultValue('OnlyInstance', if_(ServiceInfo.OnlyInstance, 'True', 'False'));

  Register_ConsoleCommand('CPM_Service_Info', 'CPM_Service_Info(): service cluster port mapping info').OnEvent_M := CC_CPM_Service_Info;
  Register_ConsoleCommand('CPM_Serv_Info', 'CPM_Serv_Info(): service cluster port mapping info').OnEvent_M := CC_CPM_Service_Info;
end;

destructor TC40_CPM_Service_Tool.Destroy;
begin
  CPM_List.Clean;
  DisposeObject(CPM_List);
  DisposeObject(XNAT_Physics_Service);
  inherited Destroy;
end;

procedure TC40_CPM_Service_Tool.SafeCheck;
begin
  inherited SafeCheck;
end;

procedure TC40_CPM_Service_Tool.Progress;
begin
  inherited Progress;
  XNAT_Physics_Service.Progress;
end;

constructor TON_Get_CPM_Mapping.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Get_CPM_Mapping.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  tmp: TDFE;
  info: TC40_CPM_Info;
  i: integer;
  cli: TXClientMapping;
begin
  Client.XNAT_Remote_CMP_Info_List.Clean;
  while Result_.R.NotEnd do
    begin
      tmp := TDFE.Create;
      Result_.R.ReadDataFrame(tmp);
      info := TC40_CPM_Info.Create;
      info.Decode(tmp);
      DisposeObject(tmp);
      Client.XNAT_Remote_CMP_Info_List.Add(info);
    end;

  Client.CPM_Address_Mapping_Report.Clear;
  for i := 0 to Client.XNAT_Remote_CMP_Info_List.count - 1 do
    begin
      info := Client.XNAT_Remote_CMP_Info_List[i];
      cli := Client.XNAT_Physics_Client.HashMapping[info.Mapping];
      if cli <> nil then
        begin
          Client.CPM_Address_Mapping_Report.Add('mapping:"%s" Listening:%s -> %s -> to:%s to port:%s, test:%s activted:%s',
            [info.Mapping.Text, Client.Remote_XNAT_Host.Text, Build_Host_URL(info.ListenAddr, info.ListenPort), cli.Addr.Text, cli.Port.Text,
              if_(info.Test_Listening_Passed, 'ReadyOK', 'error!'),
              umlBoolToStr(info.Activted).Text]);
        end;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Client.XNAT_Remote_CMP_Info_List);
    if Assigned(OnResultM) then
        OnResultM(Client, Client.XNAT_Remote_CMP_Info_List);
    if Assigned(OnResultP) then
        OnResultP(Client, Client.XNAT_Remote_CMP_Info_List);
  except
  end;
  DelayFreeObject(1.0, Self);
end;

procedure TON_Get_CPM_Mapping.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  L: TC40_CPM_Info_List;
begin
  L := TC40_CPM_Info_List.Create;
  try
    if Assigned(OnResultC) then
        OnResultC(Client, L);
    if Assigned(OnResultM) then
        OnResultM(Client, L);
    if Assigned(OnResultP) then
        OnResultP(Client, L);
  except
  end;
  L.Clean;
  DelayFreeObject(1.0, Self, L);
end;

procedure TON_End_CPM_Address_Mapping.Do_First_Get_CPM_Mapping_And_OpenTunnel(Sender: TC40_CPM_Client_Tool; L: TC40_CPM_Info_List);
begin
  if not Client.Remote_XNAT_ReadyOK then
    begin
      DoStatus('XNAT no ready.');
      DelayFreeObj(1.0, Self);
      Exit;
    end;

  Client.XNAT_Physics_Client.Host := Client.Remote_XNAT_Host;
  Client.XNAT_Physics_Client.Port := Client.Remote_XNAT_Port;
  Client.XNAT_Physics_Client.AuthToken := Client.Remote_XNAT_Auth;
  Client.XNAT_Physics_Client.On_Open_Tunnel_Done := Do_Wait_OpenTunnel_Done;
  Client.XNAT_Physics_Client.OpenTunnel;
end;

procedure TON_End_CPM_Address_Mapping.Do_Wait_OpenTunnel_Done(Sender: TXNATClient; State: Boolean);
begin
  if State then
    begin
      Client.PhysicsTunnel.PhysicsTunnel.PostProgress.PostExecuteM_NP(3.0, Do_Delay_3_Second_Done);
    end
  else
    begin
      Client.PhysicsTunnel.PhysicsTunnel.PostProgress.PostExecuteM_NP(3.0, Do_Delay_3_Second_Again_OpenTunnel);
    end;
end;

procedure TON_End_CPM_Address_Mapping.Do_Delay_3_Second_Again_OpenTunnel;
begin
  Client.XNAT_Physics_Client.OpenTunnel;
end;

procedure TON_End_CPM_Address_Mapping.Do_Delay_3_Second_Done;
begin
  Client.Get_CPM_MappingM(Do_Last_Get_CPM_Mapping_Final_State);
end;

procedure TON_End_CPM_Address_Mapping.Do_Last_Get_CPM_Mapping_Final_State(Sender: TC40_CPM_Client_Tool; L: TC40_CPM_Info_List);
begin
  DelayFreeObj(1.0, Self);
end;

procedure TC40_CPM_Client_Tool.Do_Get_CPM_Service(Sender: TPeerIO; Result_: TDFE);
begin
  Remote_XNAT_ReadyOK := True;
  // Perhaps the remote CPM specifying the physical address is incorrect, forcing the use of an absolutely correct physical address
  Remote_XNAT_Host := PhysicsTunnel.PhysicsAddr;
  Result_.R.Next;
  // physics port
  Remote_XNAT_Port := Result_.R.ReadString;
  // p2pVM auth
  Remote_XNAT_Auth := Result_.R.ReadString;
end;

procedure TC40_CPM_Client_Tool.Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client);
begin
  inherited Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_CPM_Service', nil, Do_Get_CPM_Service);
end;

procedure TC40_CPM_Client_Tool.CC_CPM_Client_Info(var OP_Param: TOpParam);
var
  i: integer;
begin
  Get_CPM_Mapping();
  for i := 0 to CPM_Address_Mapping_Report.count - 1 do
      DoStatus(CPM_Address_Mapping_Report[i]);
end;

constructor TC40_CPM_Client_Tool.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  Remote_XNAT_ReadyOK := False;
  Remote_XNAT_Host := PhysicsTunnel_.PhysicsAddr;
  Remote_XNAT_Port := '';
  Remote_XNAT_Auth := '';
  XNAT_Physics_Client := TXNATClient.Create;
  XNAT_Physics_Client.Quiet := C40_QuietMode;
  CPM_Address_Mapping_Report := TPascalStringList.Create;
  XNAT_Remote_CMP_Info_List := TC40_CPM_Info_List.Create;

  Register_ConsoleCommand('CPM_Client_Info', 'CPM_Client_Info(): client cluster port mapping info').OnEvent_M := CC_CPM_Client_Info;
  Register_ConsoleCommand('CPM_Cli_Info', 'CPM_Cli_Info(): client cluster port mapping info').OnEvent_M := CC_CPM_Client_Info;
end;

destructor TC40_CPM_Client_Tool.Destroy;
begin
  DisposeObjectAndNil(XNAT_Physics_Client);
  DisposeObjectAndNil(CPM_Address_Mapping_Report);
  DisposeObjectAndNil(XNAT_Remote_CMP_Info_List);
  inherited Destroy;
end;

procedure TC40_CPM_Client_Tool.Progress;
begin
  XNAT_Physics_Client.Progress;
  inherited Progress;
end;

procedure TC40_CPM_Client_Tool.Get_CPM_Mapping();
var
  tmp: TON_Get_CPM_Mapping;
begin
  tmp := TON_Get_CPM_Mapping.Create;
  tmp.Client := Self;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_CPM_Mapping', TDFE.Create.DelayFree, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
end;

procedure TC40_CPM_Client_Tool.Get_CPM_MappingC(OnResult: TON_Get_CPM_MappingC);
var
  d: TDFE;
  tmp: TON_Get_CPM_Mapping;
begin
  d := TDFE.Create;
  tmp := TON_Get_CPM_Mapping.Create;
  tmp.Client := Self;
  tmp.OnResultC := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_CPM_Mapping', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_CPM_Client_Tool.Get_CPM_MappingM(OnResult: TON_Get_CPM_MappingM);
var
  d: TDFE;
  tmp: TON_Get_CPM_Mapping;
begin
  d := TDFE.Create;
  tmp := TON_Get_CPM_Mapping.Create;
  tmp.Client := Self;
  tmp.OnResultM := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_CPM_Mapping', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_CPM_Client_Tool.Get_CPM_MappingP(OnResult: TON_Get_CPM_MappingP);
var
  d: TDFE;
  tmp: TON_Get_CPM_Mapping;
begin
  d := TDFE.Create;
  tmp := TON_Get_CPM_Mapping.Create;
  tmp.Client := Self;
  tmp.OnResultP := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_CPM_Mapping', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_CPM_Client_Tool.Add_CPM_Service_Listening(NoDistributed: Boolean; ListenAddr: U_String; ListenPort: Word; Mapping: U_String; TimeOut: TTimeTick; User_Data: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteBool(NoDistributed);
  d.WriteString(ListenAddr);
  d.WriteWORD(ListenPort);
  d.WriteString(Mapping);
  d.WriteUInt64(TimeOut);
  d.WriteString(User_Data);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Add_CPM_Service_Listening', d);
  DisposeObject(d);
end;

procedure TC40_CPM_Client_Tool.Add_CPM_Service_Listening(NoDistributed: Boolean; ListenPort: Word; Mapping: U_String; TimeOut: TTimeTick; User_Data: U_String);
begin
  Add_CPM_Service_Listening(NoDistributed, '0.0.0.0', ListenPort, Mapping, TimeOut, User_Data);
end;

procedure TC40_CPM_Client_Tool.Open_CPM_Service_Tunnel;
begin
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Open_CPM_Service_Tunnel');
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_CPM_Service', nil, Do_Get_CPM_Service);
end;

procedure TC40_CPM_Client_Tool.Begin_CPM_Address_Mapping;
begin
  DisposeObjectAndNil(XNAT_Physics_Client);
  XNAT_Physics_Client := TXNATClient.Create;
  XNAT_Physics_Client.Quiet := C40_QuietMode;
end;

procedure TC40_CPM_Client_Tool.Add_CPM_Address_Mapping(Mapping, Address, Port: U_String);
begin
  XNAT_Physics_Client.AddMapping(Address, Port, Mapping, 100);
end;

procedure TC40_CPM_Client_Tool.End_CPM_Address_Mapping;
var
  tmp: TON_End_CPM_Address_Mapping;
begin
  tmp := TON_End_CPM_Address_Mapping.Create;
  tmp.Client := Self;
  Get_CPM_MappingM(tmp.Do_First_Get_CPM_Mapping_And_OpenTunnel);
end;

initialization

RegisterC40('CPM', TC40_CPM_Service_Tool, TC40_CPM_Client_Tool);

end.
