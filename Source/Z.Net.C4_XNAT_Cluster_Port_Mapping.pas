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
  end;

  TC40_CPM_Service_Tool = class(TC40_Base_NoAuth_Service)
  protected
    procedure cmd_Get_CPM_Service(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Get_CPM_Mapping(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Add_CPM_Service_Listening(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Open_CPM_Service_Tunnel(Sender: TPeerIO; InData: TDFE);
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
    procedure Do_Last_Get_CPM_Mapping_Final_State(Sender: TC40_CPM_Client_Tool; L: TC40_CPM_Info_List);
  end;

  TC40_CPM_Client_Tool = class(TC40_Base_NoAuth_Client)
  protected
    procedure Do_Get_CPM_Service(Sender: TPeerIO; Result_: TDFE);
    procedure Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client); override;
  public
    Remote_XNAT_ReadyOK: Boolean;
    Remote_XNAT_Host, Remote_XNAT_Port, Remote_XNAT_Auth: U_String;
    XNAT_Physics_Client: TXNATClient;
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;

    procedure Get_CPM_MappingC(OnResult: TON_Get_CPM_MappingC);
    procedure Get_CPM_MappingM(OnResult: TON_Get_CPM_MappingM);
    procedure Get_CPM_MappingP(OnResult: TON_Get_CPM_MappingP);
    procedure Add_CPM_Service_Listening(NoDistributed: Boolean; ListenPort: Word; Mapping: U_String; TimeOut: TTimeTick; User_Data: U_String);
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
  info: TC40_CPM_Info;
  i: integer;
begin
  info := TC40_CPM_Info.Create;
  info.NoDistributed := InData.R.ReadBool;
  info.ListenPort := InData.R.ReadWord;
  info.Mapping := InData.R.ReadString;
  info.TimeOut := InData.R.ReadUInt64;
  info.User_Data := InData.R.ReadString;

  for i := 0 to CPM_List.count - 1 do
    if (info.ListenPort = CPM_List[i].ListenPort) or (info.Mapping.Same(@CPM_List[i].Mapping)) then
      begin
        DisposeObject(info);
        exit;
      end;

  CPM_List.Add(info);
end;

procedure TC40_CPM_Service_Tool.cmd_Open_CPM_Service_Tunnel(Sender: TPeerIO; InData: TDFE);
var
  i: integer;
  info: TC40_CPM_Info;
  serv: TXServiceListen;
begin
  DisposeObjectAndNil(XNAT_Physics_Service);
  XNAT_Physics_Service := TXNATService.Create;
  XNAT_Physics_Service.Host := ParamList.GetDefaultValue('XNAT_Host', PhysicsService.PhysicsAddr);
  XNAT_Physics_Service.Port := ParamList.GetDefaultValue('XNAT_Port', '9087');
  XNAT_Physics_Service.AuthToken := ParamList.GetDefaultValue('XNAT_Auth', C40_Password);
  XNAT_Physics_Service.MaxVMFragment := ParamList.GetDefaultValue('XNAT_MaxVMFragment', XNAT_Physics_Service.MaxVMFragment);
  XNAT_Physics_Service.ProtocolCompressed := EStrToBool(ParamList.GetDefaultValue('XNAT_Compressed', umlBoolToStr(XNAT_Physics_Service.ProtocolCompressed)));
  XNAT_Physics_Service.Quiet := C40_QuietMode;

  for i := 0 to CPM_List.count - 1 do
    begin
      info := CPM_List[i];
      if info.NoDistributed then
          serv := XNAT_Physics_Service.AddNoDistributedMapping(XNAT_Physics_Service.Host, umlIntToStr(info.ListenPort), info.Mapping, info.TimeOut)
      else
          serv := XNAT_Physics_Service.AddMapping(XNAT_Physics_Service.Host, umlIntToStr(info.ListenPort), info.Mapping, info.TimeOut);
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
  L: TC40_CPM_Info_List;
  tmp: TDFE;
  info: TC40_CPM_Info;
begin
  L := TC40_CPM_Info_List.Create;
  while Result_.R.NotEnd do
    begin
      tmp := TDFE.Create;
      Result_.R.ReadDataFrame(tmp);
      info := TC40_CPM_Info.Create;
      info.Decode(tmp);
      DisposeObject(tmp);
      L.Add(info);
    end;
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
      exit;
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
      Client.Get_CPM_MappingM(Do_Last_Get_CPM_Mapping_Final_State);
    end
  else
    begin
      DelayFreeObj(1.0, Self);
    end;
end;

procedure TON_End_CPM_Address_Mapping.Do_Last_Get_CPM_Mapping_Final_State(Sender: TC40_CPM_Client_Tool; L: TC40_CPM_Info_List);
var
  i: integer;
  info: TC40_CPM_Info;
  cli: TXClientMapping;
begin
  for i := 0 to L.count - 1 do
    begin
      info := L[i];
      cli := Sender.XNAT_Physics_Client.HashMapping[info.Mapping];
      if cli <> nil then
        begin
          DoStatus('mapping:"%s" Listening:%s Listening Port:%d -> to:%s to port:%s, proxy state:%s',
            [info.Mapping.Text, Sender.PhysicsTunnel.PhysicsAddr.Text, info.ListenPort, cli.Addr.Text, cli.Port.Text,
              if_(info.Test_Listening_Passed, 'ReadyOK', 'error!')]);
        end;
    end;
  DelayFreeObj(1.0, Self);
end;

procedure TC40_CPM_Client_Tool.Do_Get_CPM_Service(Sender: TPeerIO; Result_: TDFE);
begin
  Remote_XNAT_ReadyOK := True;
  Remote_XNAT_Host := Result_.R.ReadString;
  Remote_XNAT_Port := Result_.R.ReadString;
  Remote_XNAT_Auth := Result_.R.ReadString;
end;

procedure TC40_CPM_Client_Tool.Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client);
begin
  inherited Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_CPM_Service', nil, Do_Get_CPM_Service);
end;

constructor TC40_CPM_Client_Tool.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  Remote_XNAT_ReadyOK := False;
  Remote_XNAT_Host := '';
  Remote_XNAT_Port := '';
  Remote_XNAT_Auth := '';
  XNAT_Physics_Client := TXNATClient.Create;
  XNAT_Physics_Client.Quiet := C40_QuietMode;
end;

destructor TC40_CPM_Client_Tool.Destroy;
begin
  DisposeObjectAndNil(XNAT_Physics_Client);
  inherited Destroy;
end;

procedure TC40_CPM_Client_Tool.Progress;
begin
  XNAT_Physics_Client.Progress;
  inherited Progress;
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

procedure TC40_CPM_Client_Tool.Add_CPM_Service_Listening(NoDistributed: Boolean; ListenPort: Word; Mapping: U_String; TimeOut: TTimeTick; User_Data: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteBool(NoDistributed);
  d.WriteWORD(ListenPort);
  d.WriteString(Mapping);
  d.WriteUInt64(TimeOut);
  d.WriteString(User_Data);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Add_CPM_Service_Listening', d);
  DisposeObject(d);
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
