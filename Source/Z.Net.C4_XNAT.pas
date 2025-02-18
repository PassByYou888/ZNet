(*
https://zpascal.net
https://github.com/PassByYou888/ZNet
https://github.com/PassByYou888/zRasterization
https://github.com/PassByYou888/ZSnappy
https://github.com/PassByYou888/Z-AI1.4
https://github.com/PassByYou888/ZAI_1.41
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
{ * cloud 4.0 XNAT Tool                                                        * }
{ ****************************************************************************** }
unit Z.Net.C4_XNAT;

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
  Z.Net.XNAT.Client, Z.Net.XNAT.MappingOnVirutalService, Z.Net.XNAT.Service, Z.Net.XNAT.Physics;

type
  TC40_XNAT_Mapping_Info = class(TCore_Object_Intermediate)
  public
    NoDistributed: Boolean;
    ListenPort: Word;
    Mapping: U_String;
    TimeOut: TTimeTick;
    IsOpen: Boolean;
    constructor Create();
    destructor Destroy; override;
    procedure Encode(d: TDFE);
    procedure Decode(d: TDFE);
  end;

  TC40_XNAT_Mapping_Info_List_Decl = TGenericsList<TC40_XNAT_Mapping_Info>;

  TC40_XNAT_Mapping_Info_List = class(TC40_XNAT_Mapping_Info_List_Decl)
  public
    procedure Clean;
  end;

  TC40_XNAT_Service_Tool = class(TC40_Base_NoAuth_Service)
  protected
    procedure cmd_Get_XNAT_Service(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Get_XNAT_Mapping(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Add_XNAT_Mapping(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Open_XNAT_Tunnel(Sender: TPeerIO; InData: TDFE);
  public
    XNAT_Physics_Service: TXNATService;
    XNATMappingList: TC40_XNAT_Mapping_Info_List;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
  end;

  TC40_XNAT_Client_Tool = class;

  TON_Get_XNAT_MappingC = procedure(Sender: TC40_XNAT_Client_Tool; L: TC40_XNAT_Mapping_Info_List);
  TON_Get_XNAT_MappingM = procedure(Sender: TC40_XNAT_Client_Tool; L: TC40_XNAT_Mapping_Info_List) of object;
{$IFDEF FPC}
  TON_Get_XNAT_MappingP = procedure(Sender: TC40_XNAT_Client_Tool; L: TC40_XNAT_Mapping_Info_List) is nested;
{$ELSE FPC}
  TON_Get_XNAT_MappingP = reference to procedure(Sender: TC40_XNAT_Client_Tool; L: TC40_XNAT_Mapping_Info_List);
{$ENDIF FPC}

  TON_Get_XNAT_Mapping = class(TOnResult_Bridge)
  public
    Client: TC40_XNAT_Client_Tool;
    OnResultC: TON_Get_XNAT_MappingC;
    OnResultM: TON_Get_XNAT_MappingM;
    OnResultP: TON_Get_XNAT_MappingP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TXNAT_C4_VS_Mapping = class(TXNAT_VS_Mapping)
  public
    Owner: TC40_XNAT_Client_Tool;
    constructor Create(Owner_: TC40_XNAT_Client_Tool);
    destructor Destroy; override;
  end;

  TON_Build_Physics_ServiceC = procedure(Sender: TC40_XNAT_Client_Tool; Service: TXNAT_MappingOnVirutalService);
  TON_Build_Physics_ServiceM = procedure(Sender: TC40_XNAT_Client_Tool; Service: TXNAT_MappingOnVirutalService) of object;
{$IFDEF FPC}
  TON_Build_Physics_ServiceP = procedure(Sender: TC40_XNAT_Client_Tool; Service: TXNAT_MappingOnVirutalService) is nested;
{$ELSE FPC}
  TON_Build_Physics_ServiceP = reference to procedure(Sender: TC40_XNAT_Client_Tool; Service: TXNAT_MappingOnVirutalService);
{$ENDIF FPC}

  TBuild_Physics_Service_Bridge = class(TCore_Object_Intermediate)
  public
    Client: TC40_XNAT_Client_Tool;
    Mapping: U_String;
    MaxWorkload: Cardinal;
    OnResultC: TON_Build_Physics_ServiceC;
    OnResultM: TON_Build_Physics_ServiceM;
    OnResultP: TON_Build_Physics_ServiceP;
    constructor Create();
    destructor Destroy; override;
    procedure Do_Get_XNAT_Mapping(Sender: TC40_XNAT_Client_Tool; L: TC40_XNAT_Mapping_Info_List);
  end;

  TC40_XNAT_Client_Tool = class(TC40_Base_NoAuth_Client)
  protected
    procedure Do_Get_XNAT_Service(Sender: TPeerIO; Result_: TDFE);
    procedure Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client); override;
  public
    Remote_XNAT_Host, Remote_XNAT_Port, Remote_XNAT_Auth: U_String;
    XNAT_VS_List: TXNAT_VS_Mapping_List_Decl;
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;

    procedure Get_XNAT_MappingC(OnResult: TON_Get_XNAT_MappingC);
    procedure Get_XNAT_MappingM(OnResult: TON_Get_XNAT_MappingM);
    procedure Get_XNAT_MappingP(OnResult: TON_Get_XNAT_MappingP);
    procedure Add_XNAT_Mapping(NoDistributed: Boolean; ListenPort: Word; Mapping: U_String; TimeOut: TTimeTick);
    procedure Open_XNAT_Tunnel;
    procedure Build_Physics_ServiceC(Mapping: U_String; MaxWorkload: Cardinal; OnResult: TON_Build_Physics_ServiceC);
    procedure Build_Physics_ServiceM(Mapping: U_String; MaxWorkload: Cardinal; OnResult: TON_Build_Physics_ServiceM);
    procedure Build_Physics_ServiceP(Mapping: U_String; MaxWorkload: Cardinal; OnResult: TON_Build_Physics_ServiceP);
  end;

  TC40_XNAT_Client_Tool_List = TGenericsList<TC40_XNAT_Client_Tool>;

implementation

constructor TC40_XNAT_Mapping_Info.Create;
begin
  inherited Create;
  NoDistributed := True;
  ListenPort := 0;
  Mapping := '';
  TimeOut := 0;
  IsOpen := False;
end;

destructor TC40_XNAT_Mapping_Info.Destroy;
begin
  NoDistributed := True;
  ListenPort := 0;
  Mapping := '';
  TimeOut := 0;
  IsOpen := False;
  inherited Destroy;
end;

procedure TC40_XNAT_Mapping_Info.Encode(d: TDFE);
begin
  d.WriteBool(NoDistributed);
  d.WriteWORD(ListenPort);
  d.WriteString(Mapping);
  d.WriteUInt64(TimeOut);
  d.WriteBool(IsOpen);
end;

procedure TC40_XNAT_Mapping_Info.Decode(d: TDFE);
begin
  NoDistributed := d.R.ReadBool;
  ListenPort := d.R.ReadWord;
  Mapping := d.R.ReadString;
  TimeOut := d.R.ReadUInt64;
  IsOpen := d.R.ReadBool;
end;

procedure TC40_XNAT_Mapping_Info_List.Clean;
var
  i: integer;
begin
  for i := 0 to count - 1 do
      DisposeObject(items[i]);
  inherited Clear;
end;

procedure TC40_XNAT_Service_Tool.cmd_Get_XNAT_Service(Sender: TPeerIO; InData, OutData: TDFE);
begin
  OutData.WriteString(XNAT_Physics_Service.Host);
  OutData.WriteString(XNAT_Physics_Service.Port);
  OutData.WriteString(XNAT_Physics_Service.AuthToken);
end;

procedure TC40_XNAT_Service_Tool.cmd_Get_XNAT_Mapping(Sender: TPeerIO; InData, OutData: TDFE);
var
  i: integer;
  tmp: TDFE;
begin
  for i := 0 to XNATMappingList.count - 1 do
    begin
      tmp := TDFE.Create;
      XNATMappingList[i].Encode(tmp);
      OutData.WriteDataFrame(tmp);
      DisposeObject(tmp);
    end;
end;

procedure TC40_XNAT_Service_Tool.cmd_Add_XNAT_Mapping(Sender: TPeerIO; InData: TDFE);
var
  info: TC40_XNAT_Mapping_Info;
  i: integer;
begin
  info := TC40_XNAT_Mapping_Info.Create;
  info.NoDistributed := InData.R.ReadBool;
  info.ListenPort := InData.R.ReadWord;
  info.Mapping := InData.R.ReadString;
  info.TimeOut := InData.R.ReadUInt64;
  info.IsOpen := False;

  for i := 0 to XNATMappingList.count - 1 do
    if (info.ListenPort = XNATMappingList[i].ListenPort) or (info.Mapping.Same(@XNATMappingList[i].Mapping)) then
      begin
        DisposeObject(info);
        exit;
      end;

  XNATMappingList.Add(info);
end;

procedure TC40_XNAT_Service_Tool.cmd_Open_XNAT_Tunnel(Sender: TPeerIO; InData: TDFE);
var
  i: integer;
  info: TC40_XNAT_Mapping_Info;
begin
  DisposeObjectAndNil(XNAT_Physics_Service);
  XNAT_Physics_Service := TXNATService.Create;
  XNAT_Physics_Service.Host := ParamList.GetDefaultValue('XNAT_Host', XNAT_Physics_Service.Host);
  XNAT_Physics_Service.Port := ParamList.GetDefaultValue('XNAT_Port', XNAT_Physics_Service.Port);
  XNAT_Physics_Service.AuthToken := ParamList.GetDefaultValue('XNAT_Auth', XNAT_Physics_Service.AuthToken);
  XNAT_Physics_Service.MaxVMFragment := ParamList.GetDefaultValue('XNAT_MaxVMFragment', XNAT_Physics_Service.MaxVMFragment);
  XNAT_Physics_Service.ProtocolCompressed := EStrToBool(ParamList.GetDefaultValue('XNAT_Compressed', umlBoolToStr(XNAT_Physics_Service.ProtocolCompressed)));
  for i := 0 to XNATMappingList.count - 1 do
    begin
      info := XNATMappingList[i];
      if info.NoDistributed then
          XNAT_Physics_Service.AddNoDistributedMapping(XNAT_Physics_Service.Host, umlIntToStr(info.ListenPort), info.Mapping, info.TimeOut)
      else
          XNAT_Physics_Service.AddMapping(XNAT_Physics_Service.Host, umlIntToStr(info.ListenPort), info.Mapping, info.TimeOut);
      info.IsOpen := True;
    end;
  XNAT_Physics_Service.OpenTunnel;
end;

constructor TC40_XNAT_Service_Tool.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  // cmd
  DTNoAuthService.RecvTunnel.RegisterStream('Get_XNAT_Service').OnExecute := cmd_Get_XNAT_Service;
  DTNoAuthService.RecvTunnel.RegisterStream('Get_XNAT_Mapping').OnExecute := cmd_Get_XNAT_Mapping;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Add_XNAT_Mapping').OnExecute := cmd_Add_XNAT_Mapping;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Open_XNAT_Tunnel').OnExecute := cmd_Open_XNAT_Tunnel;
  // init XNAT
  XNAT_Physics_Service := TXNATService.Create;
  XNAT_Physics_Service.Host := ParamList.GetDefaultValue('XNAT_Host', XNAT_Physics_Service.Host);
  XNAT_Physics_Service.Port := ParamList.GetDefaultValue('XNAT_Port', XNAT_Physics_Service.Port);
  XNAT_Physics_Service.AuthToken := ParamList.GetDefaultValue('XNAT_Auth', XNAT_Physics_Service.AuthToken);
  XNAT_Physics_Service.MaxVMFragment := ParamList.GetDefaultValue('XNAT_MaxVMFragment', XNAT_Physics_Service.MaxVMFragment);
  XNAT_Physics_Service.ProtocolCompressed := EStrToBool(ParamList.GetDefaultValue('XNAT_Compressed', umlBoolToStr(XNAT_Physics_Service.ProtocolCompressed)));
  // XNAT define constainer
  XNATMappingList := TC40_XNAT_Mapping_Info_List.Create;
  // instance
  ServiceInfo.OnlyInstance := False;
  UpdateToGlobalDispatch;
  ParamList.SetDefaultValue('OnlyInstance', if_(ServiceInfo.OnlyInstance, 'True', 'False'));
end;

destructor TC40_XNAT_Service_Tool.Destroy;
begin
  XNATMappingList.Clean;
  DisposeObject(XNATMappingList);
  DisposeObject(XNAT_Physics_Service);
  inherited Destroy;
end;

procedure TC40_XNAT_Service_Tool.SafeCheck;
begin
  inherited SafeCheck;
end;

procedure TC40_XNAT_Service_Tool.Progress;
begin
  inherited Progress;
  XNAT_Physics_Service.Progress;
end;

constructor TON_Get_XNAT_Mapping.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Get_XNAT_Mapping.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  L: TC40_XNAT_Mapping_Info_List;
  tmp: TDFE;
  info: TC40_XNAT_Mapping_Info;
begin
  L := TC40_XNAT_Mapping_Info_List.Create;
  while Result_.R.NotEnd do
    begin
      tmp := TDFE.Create;
      Result_.R.ReadDataFrame(tmp);
      info := TC40_XNAT_Mapping_Info.Create;
      info.Decode(tmp);
      DisposeObject(tmp);
      L.Add(info);
    end;
  try
    if Assigned(OnResultC) then
        OnResultC(Client, L)
    else if Assigned(OnResultM) then
        OnResultM(Client, L)
    else if Assigned(OnResultP) then
        OnResultP(Client, L);
  except
  end;
  L.Clean;
  DelayFreeObject(1.0, Self, L);
end;

procedure TON_Get_XNAT_Mapping.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  L: TC40_XNAT_Mapping_Info_List;
begin
  L := TC40_XNAT_Mapping_Info_List.Create;
  try
    if Assigned(OnResultC) then
        OnResultC(Client, L)
    else if Assigned(OnResultM) then
        OnResultM(Client, L)
    else if Assigned(OnResultP) then
        OnResultP(Client, L);
  except
  end;
  L.Clean;
  DelayFreeObject(1.0, Self, L);
end;

constructor TXNAT_C4_VS_Mapping.Create(Owner_: TC40_XNAT_Client_Tool);
begin
  inherited Create;
  Owner := Owner_;
  Owner.XNAT_VS_List.Add(Self);
end;

destructor TXNAT_C4_VS_Mapping.Destroy;
var
  i: integer;
begin
  i := 0;
  while i < Owner.XNAT_VS_List.count do
    if Owner.XNAT_VS_List[i] = Self then
        Owner.XNAT_VS_List.Delete(i)
    else
        inc(i);
  inherited Destroy;
end;

constructor TBuild_Physics_Service_Bridge.Create();
begin
  inherited Create();
  Client := nil;
  Mapping := '';
  MaxWorkload := 0;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TBuild_Physics_Service_Bridge.Destroy;
begin
  inherited Destroy;
end;

procedure TBuild_Physics_Service_Bridge.Do_Get_XNAT_Mapping(Sender: TC40_XNAT_Client_Tool; L: TC40_XNAT_Mapping_Info_List);
var
  i: integer;
  info: TC40_XNAT_Mapping_Info;
  VS_Mapping: TXNAT_C4_VS_Mapping;
  Service: TXNAT_MappingOnVirutalService;
begin
  info := nil;
  VS_Mapping := nil;
  Service := nil;
  for i := 0 to L.count - 1 do
    if Mapping.Same(@L[i].Mapping) then
      begin
        info := L[i];
        break;
      end;

  if info <> nil then
    begin
      VS_Mapping := TXNAT_C4_VS_Mapping.Create(Client);
      VS_Mapping.Host := Client.Remote_XNAT_Host;
      VS_Mapping.Port := Client.Remote_XNAT_Port;
      VS_Mapping.AuthToken := Client.Remote_XNAT_Auth;
      Service := VS_Mapping.AddMappingService(info.Mapping, MaxWorkload);
      Client.XNAT_VS_List.Add(VS_Mapping);
      VS_Mapping.OpenTunnel();
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Service)
    else if Assigned(OnResultM) then
        OnResultM(Client, Service)
    else if Assigned(OnResultP) then
        OnResultP(Client, Service);
  except
  end;

  DelayFreeObj(1.0, Self);
end;

procedure TC40_XNAT_Client_Tool.Do_Get_XNAT_Service(Sender: TPeerIO; Result_: TDFE);
begin
  Remote_XNAT_Host := Result_.R.ReadString;
  Remote_XNAT_Port := Result_.R.ReadString;
  Remote_XNAT_Auth := Result_.R.ReadString;
end;

procedure TC40_XNAT_Client_Tool.Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client);
begin
  inherited Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_XNAT_Service', nil, Do_Get_XNAT_Service);
end;

constructor TC40_XNAT_Client_Tool.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  Remote_XNAT_Host := '';
  Remote_XNAT_Port := '';
  Remote_XNAT_Auth := '';
  XNAT_VS_List := TXNAT_VS_Mapping_List_Decl.Create;
end;

destructor TC40_XNAT_Client_Tool.Destroy;
begin
  while XNAT_VS_List.count > 0 do
      DisposeObject(XNAT_VS_List[0]);
  DisposeObject(XNAT_VS_List);
  inherited Destroy;
end;

procedure TC40_XNAT_Client_Tool.Progress;
var
  i: integer;
begin
  for i := 0 to XNAT_VS_List.count - 1 do
      XNAT_VS_List[i].Progress;
  inherited Progress;
end;

procedure TC40_XNAT_Client_Tool.Get_XNAT_MappingC(OnResult: TON_Get_XNAT_MappingC);
var
  d: TDFE;
  tmp: TON_Get_XNAT_Mapping;
begin
  d := TDFE.Create;
  tmp := TON_Get_XNAT_Mapping.Create;
  tmp.Client := Self;
  tmp.OnResultC := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_XNAT_Mapping', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_XNAT_Client_Tool.Get_XNAT_MappingM(OnResult: TON_Get_XNAT_MappingM);
var
  d: TDFE;
  tmp: TON_Get_XNAT_Mapping;
begin
  d := TDFE.Create;
  tmp := TON_Get_XNAT_Mapping.Create;
  tmp.Client := Self;
  tmp.OnResultM := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_XNAT_Mapping', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_XNAT_Client_Tool.Get_XNAT_MappingP(OnResult: TON_Get_XNAT_MappingP);
var
  d: TDFE;
  tmp: TON_Get_XNAT_Mapping;
begin
  d := TDFE.Create;
  tmp := TON_Get_XNAT_Mapping.Create;
  tmp.Client := Self;
  tmp.OnResultP := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_XNAT_Mapping', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_XNAT_Client_Tool.Add_XNAT_Mapping(NoDistributed: Boolean; ListenPort: Word; Mapping: U_String; TimeOut: TTimeTick);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteBool(NoDistributed);
  d.WriteWORD(ListenPort);
  d.WriteString(Mapping);
  d.WriteUInt64(TimeOut);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Add_XNAT_Mapping', d);
  DisposeObject(d);
end;

procedure TC40_XNAT_Client_Tool.Open_XNAT_Tunnel;
begin
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Open_XNAT_Tunnel');
end;

procedure TC40_XNAT_Client_Tool.Build_Physics_ServiceC(Mapping: U_String; MaxWorkload: Cardinal; OnResult: TON_Build_Physics_ServiceC);
var
  tmp: TBuild_Physics_Service_Bridge;
begin
  tmp := TBuild_Physics_Service_Bridge.Create;
  tmp.Client := Self;
  tmp.Mapping := Mapping;
  tmp.MaxWorkload := MaxWorkload;
  tmp.OnResultC := OnResult;
  Get_XNAT_MappingM(tmp.Do_Get_XNAT_Mapping);
end;

procedure TC40_XNAT_Client_Tool.Build_Physics_ServiceM(Mapping: U_String; MaxWorkload: Cardinal; OnResult: TON_Build_Physics_ServiceM);
var
  tmp: TBuild_Physics_Service_Bridge;
begin
  tmp := TBuild_Physics_Service_Bridge.Create;
  tmp.Client := Self;
  tmp.Mapping := Mapping;
  tmp.MaxWorkload := MaxWorkload;
  tmp.OnResultM := OnResult;
  Get_XNAT_MappingM(tmp.Do_Get_XNAT_Mapping);
end;

procedure TC40_XNAT_Client_Tool.Build_Physics_ServiceP(Mapping: U_String; MaxWorkload: Cardinal; OnResult: TON_Build_Physics_ServiceP);
var
  tmp: TBuild_Physics_Service_Bridge;
begin
  tmp := TBuild_Physics_Service_Bridge.Create;
  tmp.Client := Self;
  tmp.Mapping := Mapping;
  tmp.MaxWorkload := MaxWorkload;
  tmp.OnResultP := OnResult;
  Get_XNAT_MappingM(tmp.Do_Get_XNAT_Mapping);
end;

initialization

RegisterC40('XNAT', TC40_XNAT_Service_Tool, TC40_XNAT_Client_Tool);

end.
 
