{ ****************************************************************************** }
{ * cloud 4.0 console application framework                                    * }
{ ****************************************************************************** }
unit Z.Net.C4_Console_APP;
{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status,
  Z.ListEngine, Z.HashList.Templet, Z.Expression, Z.OpCode, Z.Parsing, Z.DFE, Z.TextDataEngine,
  Z.Json, Z.Geometry2D, Z.Geometry3D, Z.Number,
  Z.MemoryStream,
  Z.Net,
  Z.ZDB.ObjectData_LIB, Z.ZDB, Z.ZDB.Engine, Z.ZDB.LocalManager,
  Z.ZDB.FileIndexPackage_LIB, Z.ZDB.FilePackage_LIB, Z.ZDB.ItemStream_LIB, Z.ZDB.HashField_LIB, Z.ZDB.HashItem_LIB,
  Z.ZDB2.Custom, Z.ZDB2, Z.ZDB2.DFE, Z.ZDB2.HS, Z.ZDB2.HV, Z.ZDB2.Json, Z.ZDB2.MS64, Z.ZDB2.NM, Z.ZDB2.TE, Z.ZDB2.FileEncoder,
  Z.Net.C4, Z.Net.C4_UserDB, Z.Net.C4_Var, Z.Net.C4_FS, Z.Net.C4_RandSeed, Z.Net.C4_Log_DB, Z.Net.C4_XNAT, Z.Net.C4_Alias,
  Z.Net.C4_FS2, Z.Net.C4_PascalRewrite_Client, Z.Net.C4_PascalRewrite_Service,
  Z.Net.C4_NetDisk_Admin_Tool,
  Z.Net.C4_TEKeyValue,
  Z.Net.PhysicsIO, Z.Net.C4_NetDisk_Client, Z.Net.C4_NetDisk_Directory,
  Z.Net.C4_NetDisk_Service;

type
  TC40_Console_Help = class
  private
    procedure UpdateServiceInfo; overload;
    procedure UpdateServiceInfo(phy_serv: TC40_PhysicsService); overload;
    procedure UpdateTunnelInfo; overload;
    procedure UpdateTunnelInfo(phy_tunnel: TC40_PhysicsTunnel); overload;
    function Do_Help(var OP_Param: TOpParam): Variant;
    function Do_Exit(var OP_Param: TOpParam): Variant;
    function Do_Service(var OP_Param: TOpParam): Variant;
    function Do_Tunnel(var OP_Param: TOpParam): Variant;
    function Do_Reg(var OP_Param: TOpParam): Variant;
    function Do_Cmd(var OP_Param: TOpParam): Variant;
    function Do_Custom_Console_Cmd(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
  public
    opRT: TOpCustomRunTime;
    HelpTextStyle: TTextStyle;
    IsExit: Boolean;
    constructor Create; virtual;
    destructor Destroy; override;
    function Run_HelpCmd(Expression: U_String): Boolean;
  end;

var
  C40AppParam: U_StringArray;
  C40AppParsingTextStyle: TTextStyle;
  On_C40_PhysicsTunnel_Event: IC40_PhysicsTunnel_Event;
  On_C40_PhysicsService_Event: IC40_PhysicsService_Event;

procedure C40_Init_AppParamFromSystemCmdLine;
function C40_Extract_CmdLine(): Boolean;

implementation

uses Variants;

procedure TC40_Console_Help.UpdateServiceInfo;
var
  i: Integer;
  phy_serv: TC40_PhysicsService;
begin
  for i := 0 to C40_PhysicsServicePool.Count - 1 do
    begin
      phy_serv := C40_PhysicsServicePool[i];
      DoStatus('service "%s" port:%d connection workload:%d send:%s receive:%s',
        [phy_serv.PhysicsAddr.Text, phy_serv.PhysicsPort, phy_serv.PhysicsTunnel.Count,
          umlSizeToStr(phy_serv.PhysicsTunnel.Statistics[stSendSize]).Text,
          umlSizeToStr(phy_serv.PhysicsTunnel.Statistics[stReceiveSize]).Text
          ]);
    end;
end;

procedure TC40_Console_Help.UpdateServiceInfo(phy_serv: TC40_PhysicsService);
var
  i, j: Integer;
  custom_serv: TC40_Custom_Service;
  s_recv_, s_send_: TZNet_WithP2PVM_Server;
begin
  DoStatus('Physics service: "%s" Unit: "%s"', [phy_serv.PhysicsTunnel.ClassName, phy_serv.PhysicsTunnel.UnitName + '.pas']);
  DoStatus('Physics service workload: %d', [phy_serv.PhysicsTunnel.Count]);
  DoStatus('Physics service receive:%s, send:%s ', [umlSizeToStr(phy_serv.PhysicsTunnel.Statistics[stReceiveSize]).Text, umlSizeToStr(phy_serv.PhysicsTunnel.Statistics[stSendSize]).Text]);
  DoStatus('Physcis Listening ip: "%s" Port: %d', [phy_serv.PhysicsAddr.Text, phy_serv.PhysicsPort]);
  DoStatus('Listening Successed: %s', [if_(phy_serv.Activted, 'Yes', 'Failed')]);
  for i := 0 to phy_serv.DependNetworkServicePool.Count - 1 do
    begin
      DoStatus('--------------------------------------------', []);
      custom_serv := phy_serv.DependNetworkServicePool[i];
      DoStatus('Type: %s', [custom_serv.ServiceInfo.ServiceTyp.Text]);
      DoStatus('workload: %d / %d', [custom_serv.ServiceInfo.Workload, custom_serv.ServiceInfo.MaxWorkload]);
      if custom_serv.Get_P2PVM_Service(s_recv_, s_send_) then
          DoStatus('receive:%s send:%s',
          [umlSizeToStr(s_recv_.Statistics[stReceiveSize]).Text, umlSizeToStr(s_recv_.Statistics[stSendSize]).Text]);
      DoStatus('Only Instance: %s', [if_(custom_serv.ServiceInfo.OnlyInstance, 'Yes', 'More Instance.')]);
      DoStatus('Hash: %s', [umlMD5ToStr(custom_serv.ServiceInfo.Hash).Text]);
      DoStatus('Alias or Hash: %s', [custom_serv.AliasOrHash.Text]);
      DoStatus('Class: "%s" Unit: "%s"', [custom_serv.ClassName, custom_serv.UnitName + '.pas']);
      DoStatus('Receive Tunnel IP: %s Port: %d',
        [custom_serv.ServiceInfo.p2pVM_RecvTunnel_Addr.Text, custom_serv.ServiceInfo.p2pVM_RecvTunnel_Port]);
      DoStatus('Send Tunnel IP: %s Port: %d',
        [custom_serv.ServiceInfo.p2pVM_SendTunnel_Addr.Text, custom_serv.ServiceInfo.p2pVM_SendTunnel_Port]);
      DoStatus('Workload: %d/%d', [custom_serv.ServiceInfo.Workload, custom_serv.ServiceInfo.MaxWorkload]);
      DoStatus('Parameter', []);
      DoStatus('{', []);
      DoStatus(#9 + umlReplace(custom_serv.ParamList.AsText, #13#10, #13#10#9, False, False));
      DoStatus('}', []);
    end;
  DoStatus('', []);
end;

procedure TC40_Console_Help.UpdateTunnelInfo;
var
  i: Integer;
  phy_tunnel: TC40_PhysicsTunnel;
begin
  for i := 0 to C40_PhysicsTunnelPool.Count - 1 do
    begin
      phy_tunnel := C40_PhysicsTunnelPool[i];
      DoStatus('tunnel "%s" port:%d send:%s receive:%s',
        [phy_tunnel.PhysicsAddr.Text, phy_tunnel.PhysicsPort,
          umlSizeToStr(phy_tunnel.PhysicsTunnel.Statistics[stSendSize]).Text,
          umlSizeToStr(phy_tunnel.PhysicsTunnel.Statistics[stReceiveSize]).Text
          ]);
    end;
end;

procedure TC40_Console_Help.UpdateTunnelInfo(phy_tunnel: TC40_PhysicsTunnel);
var
  i: Integer;
  custom_client: TC40_Custom_Client;
  c_recv_, c_send_: TZNet_WithP2PVM_Client;
begin
  DoStatus('Physics tunnel: "%s" Unit: "%s"', [phy_tunnel.PhysicsTunnel.ClassName, phy_tunnel.PhysicsTunnel.UnitName + '.pas']);
  DoStatus('Physcis ip: "%s" Port: %d', [phy_tunnel.PhysicsAddr.Text, phy_tunnel.PhysicsPort]);
  DoStatus('Physcis Connected: %s', [if_(phy_tunnel.PhysicsTunnel.Connected, 'Yes', 'Failed')]);
  DoStatus('Physics receive:%s, send:%s ', [umlSizeToStr(phy_tunnel.PhysicsTunnel.Statistics[stReceiveSize]).Text, umlSizeToStr(phy_tunnel.PhysicsTunnel.Statistics[stSendSize]).Text]);
  for i := 0 to phy_tunnel.DependNetworkClientPool.Count - 1 do
    begin
      DoStatus('--------------------------------------------', []);
      custom_client := phy_tunnel.DependNetworkClientPool[i];
      DoStatus('Type: %s', [custom_client.ClientInfo.ServiceTyp.Text]);
      DoStatus('Connected: %s', [if_(custom_client.Connected, 'Yes', 'Failed')]);
      if custom_client.Get_P2PVM_Tunnel(c_recv_, c_send_) then
          DoStatus('receive:%s send:%s',
          [umlSizeToStr(c_recv_.Statistics[stReceiveSize]).Text, umlSizeToStr(c_recv_.Statistics[stSendSize]).Text]);
      DoStatus('Only Instance: %s', [if_(custom_client.ClientInfo.OnlyInstance, 'Yes', 'More Instance.')]);
      DoStatus('Hash: %s', [umlMD5ToStr(custom_client.ClientInfo.Hash).Text]);
      DoStatus('Alias or Hash: %s', [custom_client.AliasOrHash.Text]);
      DoStatus('Class: "%s" Unit: "%s"', [custom_client.ClassName, custom_client.UnitName + '.pas']);
      DoStatus('Receive Tunnel IP: %s Port: %d',
        [custom_client.ClientInfo.p2pVM_RecvTunnel_Addr.Text, custom_client.ClientInfo.p2pVM_RecvTunnel_Port]);
      DoStatus('Send Tunnel IP: %s Port: %d',
        [custom_client.ClientInfo.p2pVM_SendTunnel_Addr.Text, custom_client.ClientInfo.p2pVM_SendTunnel_Port]);
      DoStatus('Workload: %d/%d', [custom_client.ClientInfo.Workload, custom_client.ClientInfo.MaxWorkload]);
      DoStatus('Parameter', []);
      DoStatus('{', []);
      DoStatus(#9 + umlReplace(custom_client.ParamList.AsText, #13#10, #13#10#9, False, False));
      DoStatus('}', []);
    end;
  DoStatus('', []);
end;

function TC40_Console_Help.Do_Help(var OP_Param: TOpParam): Variant;
var
  i: Integer;
  L: TPascalStringList;
begin
  L := opRT.GetAllProcDescription(True, '*');
  for i := 0 to L.Count - 1 do
      DoStatus(L[i]);
  Result := True;
end;

function TC40_Console_Help.Do_Exit(var OP_Param: TOpParam): Variant;
begin
  IsExit := True;
  Result := True;
end;

function TC40_Console_Help.Do_Service(var OP_Param: TOpParam): Variant;
var
  i: Integer;
  ip: U_String;
  port: word;
begin
  if length(OP_Param) = 1 then
    begin
      ip := VarToStr(OP_Param[0]);
      for i := 0 to C40_PhysicsServicePool.Count - 1 do
        begin
          if (umlMultipleMatch(ip, C40_PhysicsServicePool[i].ListeningAddr)
              or umlMultipleMatch(ip, C40_PhysicsServicePool[i].PhysicsAddr)) then
              UpdateServiceInfo(C40_PhysicsServicePool[i]);
        end;
    end
  else if length(OP_Param) = 2 then
    begin
      ip := VarToStr(OP_Param[0]);
      port := OP_Param[1];
      for i := 0 to C40_PhysicsServicePool.Count - 1 do
        begin
          if (umlMultipleMatch(ip, C40_PhysicsServicePool[i].ListeningAddr)
              or umlMultipleMatch(ip, C40_PhysicsServicePool[i].PhysicsAddr)) and (port = C40_PhysicsServicePool[i].PhysicsPort) then
              UpdateServiceInfo(C40_PhysicsServicePool[i]);
        end;
    end
  else
    begin
      UpdateServiceInfo();
    end;
  Result := True;
end;

function TC40_Console_Help.Do_Tunnel(var OP_Param: TOpParam): Variant;
var
  i: Integer;
  ip: U_String;
  port: word;
begin
  if length(OP_Param) = 1 then
    begin
      ip := VarToStr(OP_Param[0]);
      for i := 0 to C40_PhysicsTunnelPool.Count - 1 do
        begin
          if umlMultipleMatch(ip, C40_PhysicsTunnelPool[i].PhysicsAddr) then
              UpdateTunnelInfo(C40_PhysicsTunnelPool[i]);
        end;
    end
  else if length(OP_Param) = 2 then
    begin
      ip := VarToStr(OP_Param[0]);
      port := OP_Param[1];
      for i := 0 to C40_PhysicsTunnelPool.Count - 1 do
        begin
          if umlMultipleMatch(ip, C40_PhysicsTunnelPool[i].PhysicsAddr)
            and (port = C40_PhysicsTunnelPool[i].PhysicsPort) then
              UpdateTunnelInfo(C40_PhysicsTunnelPool[i]);
        end;
    end
  else
    begin
      UpdateTunnelInfo();
    end;
  Result := True;
end;

function TC40_Console_Help.Do_Reg(var OP_Param: TOpParam): Variant;
begin
  C40_Registed.Print;
  Result := True;
end;

function TC40_Console_Help.Do_Cmd(var OP_Param: TOpParam): Variant;
var
  i: Integer;
begin
  for i := low(C40AppParam) to high(C40AppParam) do
      DoStatus(C40AppParam[i]);
  Result := True;
end;

function TC40_Console_Help.Do_Custom_Console_Cmd(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
var
  tk: TTimeTick;
  LName: U_String;
  i: Integer;
  cc: TC4_Help_Console_Command;
  __repeat__: TC4_Help_Console_Command_Decl.TRepeat___;
  rData: TC4_Help_Console_Command_Data;
begin
  tk := GetTimeTick;
  LName := Sender.Trigger^.Name;
  for i := 0 to C40_ServicePool.Count - 1 do
    begin
      cc := C40_ServicePool[i].ConsoleCommand;
      if cc.Num > 0 then
        begin
          __repeat__ := cc.Repeat_;
          repeat
            rData := __repeat__.Queue^.Data;
            if LName.Same(rData.Cmd) then
              begin
                rData.DoExecute(OP_Param);
                DoStatus('execute %s from %s(%s)', [rData.Cmd, C40_ServicePool[i].ClassName, C40_ServicePool[i].ServiceInfo.ServiceTyp.Text]);
              end;
          until not __repeat__.Next;
        end;
    end;
  for i := 0 to C40_ClientPool.Count - 1 do
    begin
      cc := C40_ClientPool[i].ConsoleCommand;
      if cc.Num > 0 then
        begin
          __repeat__ := cc.Repeat_;
          repeat
            rData := __repeat__.Queue^.Data;
            if LName.Same(rData.Cmd) then
              begin
                rData.DoExecute(OP_Param);
                DoStatus('execute %s from %s(%s)', [rData.Cmd, C40_ClientPool[i].ClassName, C40_ClientPool[i].ClientInfo.ServiceTyp.Text]);
              end;
          until not __repeat__.Next;
        end;
    end;
  for i := 0 to C40_VM_Service_Pool.Count - 1 do
    begin
      cc := C40_VM_Service_Pool[i].ConsoleCommand;
      if cc.Num > 0 then
        begin
          __repeat__ := cc.Repeat_;
          repeat
            rData := __repeat__.Queue^.Data;
            if LName.Same(rData.Cmd) then
              begin
                rData.DoExecute(OP_Param);
                DoStatus('execute %s from %s', [rData.Cmd, C40_VM_Service_Pool[i].ClassName]);
              end;
          until not __repeat__.Next;
        end;
    end;
  for i := 0 to C40_VM_Client_Pool.Count - 1 do
    begin
      cc := C40_VM_Client_Pool[i].ConsoleCommand;
      if cc.Num > 0 then
        begin
          __repeat__ := cc.Repeat_;
          repeat
            rData := __repeat__.Queue^.Data;
            if LName.Same(rData.Cmd) then
              begin
                rData.DoExecute(OP_Param);
                DoStatus('execute %s from %s', [rData.Cmd, C40_VM_Client_Pool[i].ClassName]);
              end;
          until not __repeat__.Next;
        end;
    end;
  DoStatus('request reopen.');
  Result := GetTimeTick - tk;
end;

constructor TC40_Console_Help.Create;
var
  i: Integer;
  cc: TC4_Help_Console_Command;
  __repeat__: TC4_Help_Console_Command_Decl.TRepeat___;
  rData: TC4_Help_Console_Command_Data;
begin
  inherited Create;
  opRT := TOpCustomRunTime.Create;
  HelpTextStyle := C40AppParsingTextStyle;
  IsExit := False;

  opRT.RegOpM('Help', {$IFDEF FPC}@{$ENDIF FPC}Do_Help)^.Description := 'help info.';
  opRT.RegOpM('Exit', {$IFDEF FPC}@{$ENDIF FPC}Do_Exit)^.Description := 'safe close this console.';
  opRT.RegOpM('Close', {$IFDEF FPC}@{$ENDIF FPC}Do_Exit)^.Description := 'safe close this console.';
  opRT.RegOpM('service', {$IFDEF FPC}@{$ENDIF FPC}Do_Service)^.Description := 'local service report.';
  opRT.RegOpM('server', {$IFDEF FPC}@{$ENDIF FPC}Do_Service)^.Description := 'local service report.';
  opRT.RegOpM('serv', {$IFDEF FPC}@{$ENDIF FPC}Do_Service)^.Description := 'local service report.';
  opRT.RegOpM('tunnel', {$IFDEF FPC}@{$ENDIF FPC}Do_Tunnel)^.Description := 'tunnel report.';
  opRT.RegOpM('client', {$IFDEF FPC}@{$ENDIF FPC}Do_Tunnel)^.Description := 'tunnel report.';
  opRT.RegOpM('cli', {$IFDEF FPC}@{$ENDIF FPC}Do_Tunnel)^.Description := 'tunnel report.';
  opRT.RegOpM('RegInfo', {$IFDEF FPC}@{$ENDIF FPC}Do_Reg)^.Description := 'C4 registed info.';
  opRT.RegOpM('cmd', {$IFDEF FPC}@{$ENDIF FPC}Do_Cmd)^.Description := 'current command line.';

  for i := 0 to C40_ServicePool.Count - 1 do
    begin
      cc := C40_ServicePool[i].ConsoleCommand;
      if cc.Num > 0 then
        begin
          __repeat__ := cc.Repeat_;
          repeat
            rData := __repeat__.Queue^.Data;
            if not opRT.ProcList.Exists(rData.Cmd) then
                opRT.RegObjectOpM(rData.Cmd, {$IFDEF FPC}@{$ENDIF FPC}Do_Custom_Console_Cmd)^.Description := rData.Desc;
          until not __repeat__.Next;
        end;
    end;
  for i := 0 to C40_ClientPool.Count - 1 do
    begin
      cc := C40_ClientPool[i].ConsoleCommand;
      if cc.Num > 0 then
        begin
          __repeat__ := cc.Repeat_;
          repeat
            rData := __repeat__.Queue^.Data;
            if not opRT.ProcList.Exists(rData.Cmd) then
                opRT.RegObjectOpM(rData.Cmd, {$IFDEF FPC}@{$ENDIF FPC}Do_Custom_Console_Cmd)^.Description := rData.Desc;
          until not __repeat__.Next;
        end;
    end;
  for i := 0 to C40_VM_Service_Pool.Count - 1 do
    begin
      cc := C40_VM_Service_Pool[i].ConsoleCommand;
      if cc.Num > 0 then
        begin
          __repeat__ := cc.Repeat_;
          repeat
            rData := __repeat__.Queue^.Data;
            if not opRT.ProcList.Exists(rData.Cmd) then
                opRT.RegObjectOpM(rData.Cmd, {$IFDEF FPC}@{$ENDIF FPC}Do_Custom_Console_Cmd)^.Description := rData.Desc;
          until not __repeat__.Next;
        end;
    end;
  for i := 0 to C40_VM_Client_Pool.Count - 1 do
    begin
      cc := C40_VM_Client_Pool[i].ConsoleCommand;
      if cc.Num > 0 then
        begin
          __repeat__ := cc.Repeat_;
          repeat
            rData := __repeat__.Queue^.Data;
            if not opRT.ProcList.Exists(rData.Cmd) then
                opRT.RegObjectOpM(rData.Cmd, {$IFDEF FPC}@{$ENDIF FPC}Do_Custom_Console_Cmd)^.Description := rData.Desc;
          until not __repeat__.Next;
        end;
    end;
end;

destructor TC40_Console_Help.Destroy;
begin
  disposeObject(opRT);
  inherited Destroy;
end;

function TC40_Console_Help.Run_HelpCmd(Expression: U_String): Boolean;
var
  r: Variant;
  r_arry: TExpressionValueVector;
begin
  if IsSymbolVectorExpression(Expression, HelpTextStyle) then
    begin
      r_arry := EvaluateExpressionVector(False, False, nil, HelpTextStyle, Expression, opRT, nil);
      Result := not ExpressionValueVectorIsError(r_arry);
      DoStatus('%s result: %s', [Expression.Text, ExpressionValueVectorToStr(r_arry).Text]);
    end
  else
    begin
      r := EvaluateExpressionValue(False, HelpTextStyle, Expression, opRT);
      Result := not ExpressionValueIsError(r);
      DoStatus('%s result: %s', [Expression.Text, VarToStr(r)]);
    end;
end;

type
  TCmd_Net_Info_ = record
    listen_ip: string;
    ip: string;
    port: word;
    depend: string;
    isAuto, Min_Workload: Boolean;
  end;

  TCmd_Net_Info_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TCmd_Net_Info_>;

  TCommand_Script = class
  private
    function Do_Config(var OP_Param: TOpParam): Variant;
    function Do_AutoClient(var OP_Param: TOpParam): Variant;
    function Do_Client(var OP_Param: TOpParam): Variant;
    function Do_Service(var OP_Param: TOpParam): Variant;
    function Do_Sleep(var OP_Param: TOpParam): Variant;
  public
    opRT: TOpCustomRunTime;
    Config: THashStringList;
    ConfigIsUpdate: Boolean;
    Client_NetInfo_List: TCmd_Net_Info_List;
    Service_NetInfo_List: TCmd_Net_Info_List;
    constructor Create;
    destructor Destroy; override;
    procedure RegApi;
    procedure Parsing(Expression: U_String);
  end;

function TCommand_Script.Do_Config(var OP_Param: TOpParam): Variant;
begin
  if length(OP_Param) > 0 then
    begin
      Config.SetDefaultValue(opRT.Trigger^.Name, VarToStr(OP_Param[0]));
      Result := True;
      ConfigIsUpdate := True;
    end
  else
      Result := Config[opRT.Trigger^.Name];
end;

function TCommand_Script.Do_AutoClient(var OP_Param: TOpParam): Variant;
var
  net_info_: TCmd_Net_Info_;
begin
  net_info_.listen_ip := '';
  net_info_.ip := OP_Param[0];
  net_info_.port := OP_Param[1];
  net_info_.depend := OP_Param[2];
  net_info_.isAuto := True;
  if length(OP_Param) > 3 then
      net_info_.Min_Workload := OP_Param[3]
  else
      net_info_.Min_Workload := False;
  Client_NetInfo_List.Add(net_info_);
  Result := True;
end;

function TCommand_Script.Do_Client(var OP_Param: TOpParam): Variant;
var
  net_info_: TCmd_Net_Info_;
begin
  net_info_.listen_ip := '';
  net_info_.ip := OP_Param[0];
  net_info_.port := OP_Param[1];
  net_info_.depend := OP_Param[2];
  net_info_.isAuto := False;
  net_info_.Min_Workload := False;
  Client_NetInfo_List.Add(net_info_);
  Result := True;
end;

function TCommand_Script.Do_Service(var OP_Param: TOpParam): Variant;
var
  net_info_: TCmd_Net_Info_;
begin
  if length(OP_Param) > 3 then
    begin
      net_info_.listen_ip := OP_Param[0];
      net_info_.ip := OP_Param[1];
      net_info_.port := OP_Param[2];
      net_info_.depend := OP_Param[3];
      net_info_.isAuto := False;
      net_info_.Min_Workload := False;
      Service_NetInfo_List.Add(net_info_);
    end
  else if length(OP_Param) = 3 then
    begin
      net_info_.ip := OP_Param[0];
      if Z.Net.IsIPv4(net_info_.ip) then
          net_info_.listen_ip := '0.0.0.0'
      else if Z.Net.IsIPV6(net_info_.ip) then
          net_info_.listen_ip := '::'
      else
          net_info_.listen_ip := '0.0.0.0';

      net_info_.port := OP_Param[1];
      net_info_.depend := OP_Param[2];
      net_info_.isAuto := False;
      net_info_.Min_Workload := False;
      Service_NetInfo_List.Add(net_info_);
    end;
  Result := True;
end;

function TCommand_Script.Do_Sleep(var OP_Param: TOpParam): Variant;
begin
  TCompute.Sleep(OP_Param[0]);
  Result := True;
end;

constructor TCommand_Script.Create;
begin
  inherited Create;
  opRT := TOpCustomRunTime.Create;

  Config := THashStringList.Create;
  ConfigIsUpdate := False;

  Client_NetInfo_List := TCmd_Net_Info_List.Create;
  Service_NetInfo_List := TCmd_Net_Info_List.Create;
end;

destructor TCommand_Script.Destroy;
begin
  disposeObject(Client_NetInfo_List);
  disposeObject(Service_NetInfo_List);
  disposeObject(opRT);
  disposeObject(Config);
  inherited Destroy;
end;

procedure TCommand_Script.RegApi;
var
  L: TListPascalString;
  i: Integer;
begin
  L := TListPascalString.Create;
  Config.GetNameList(L);
  for i := 0 to L.Count - 1 do
    begin
      opRT.RegOpM(L[i], {$IFDEF FPC}@{$ENDIF FPC}Do_Config);
    end;
  disposeObject(L);

  opRT.RegOpM('Auto', {$IFDEF FPC}@{$ENDIF FPC}Do_AutoClient);
  opRT.RegOpM('AutoClient', {$IFDEF FPC}@{$ENDIF FPC}Do_AutoClient);
  opRT.RegOpM('AutoCli', {$IFDEF FPC}@{$ENDIF FPC}Do_AutoClient);
  opRT.RegOpM('AutoTunnel', {$IFDEF FPC}@{$ENDIF FPC}Do_AutoClient);
  opRT.RegOpM('AutoConnect', {$IFDEF FPC}@{$ENDIF FPC}Do_AutoClient);
  opRT.RegOpM('AutoConnection', {$IFDEF FPC}@{$ENDIF FPC}Do_AutoClient);
  opRT.RegOpM('AutoNet', {$IFDEF FPC}@{$ENDIF FPC}Do_AutoClient);
  opRT.RegOpM('AutoBuild', {$IFDEF FPC}@{$ENDIF FPC}Do_AutoClient);

  opRT.RegOpM('Client', {$IFDEF FPC}@{$ENDIF FPC}Do_Client);
  opRT.RegOpM('Cli', {$IFDEF FPC}@{$ENDIF FPC}Do_Client);
  opRT.RegOpM('Tunnel', {$IFDEF FPC}@{$ENDIF FPC}Do_Client);
  opRT.RegOpM('Connect', {$IFDEF FPC}@{$ENDIF FPC}Do_Client);
  opRT.RegOpM('Connection', {$IFDEF FPC}@{$ENDIF FPC}Do_Client);
  opRT.RegOpM('Net', {$IFDEF FPC}@{$ENDIF FPC}Do_Client);
  opRT.RegOpM('Build', {$IFDEF FPC}@{$ENDIF FPC}Do_Client);

  opRT.RegOpM('Service', {$IFDEF FPC}@{$ENDIF FPC}Do_Service);
  opRT.RegOpM('Serv', {$IFDEF FPC}@{$ENDIF FPC}Do_Service);
  opRT.RegOpM('Listen', {$IFDEF FPC}@{$ENDIF FPC}Do_Service);
  opRT.RegOpM('Listening', {$IFDEF FPC}@{$ENDIF FPC}Do_Service);

  opRT.RegOpM('Wait', {$IFDEF FPC}@{$ENDIF FPC}Do_Sleep);
  opRT.RegOpM('Sleep', {$IFDEF FPC}@{$ENDIF FPC}Do_Sleep);
end;

procedure TCommand_Script.Parsing(Expression: U_String);
begin
  EvaluateExpressionValue(False, C40AppParsingTextStyle, Expression, opRT);
end;

procedure C40_Init_AppParamFromSystemCmdLine;
var
  i: Integer;
begin
  SetLength(C40AppParam, ParamCount);
  for i := 1 to ParamCount do
      C40AppParam[i - 1] := ParamStr(i);
end;

function C40_Extract_CmdLine(): Boolean;
var
  error_: Boolean;
  IsInited_: Boolean;
  cmd_script_: TCommand_Script;
  i, j: Integer;
  net_info_: TCmd_Net_Info_;
  arry: TC40_DependNetworkInfoArray;
begin
  Result := False;
  if length(C40AppParam) = 0 then
      exit;
  error_ := False;
  IsInited_ := False;
  try
    cmd_script_ := TCommand_Script.Create;
    Z.Net.C4.C40WriteConfig(cmd_script_.Config);
    cmd_script_.Config.SetDefaultValue('Root', Z.Net.C4.C40_RootPath);
    cmd_script_.Config.SetDefaultValue('Password', Z.Net.C4.C40_Password);
    cmd_script_.RegApi;

    for i := low(C40AppParam) to high(C40AppParam) do
        cmd_script_.Parsing(C40AppParam[i]);

    if (not error_) and (cmd_script_.Client_NetInfo_List.Count > 0) then
      begin
        for i := 0 to cmd_script_.Client_NetInfo_List.Count - 1 do
          begin
            net_info_ := cmd_script_.Client_NetInfo_List[i];
            arry := ExtractDependInfo(net_info_.depend);
            for j := Low(arry) to high(arry) do
              if FindRegistedC40(arry[j].Typ) = nil then
                begin
                  DoStatus('no found %s', [arry[j].Typ.Text]);
                  error_ := True;
                end;
          end;
      end;

    if (not error_) and (cmd_script_.Service_NetInfo_List.Count > 0) then
      begin
        for i := 0 to cmd_script_.Service_NetInfo_List.Count - 1 do
          begin
            net_info_ := cmd_script_.Service_NetInfo_List[i];
            arry := ExtractDependInfo(net_info_.depend);
            for j := Low(arry) to high(arry) do
              if FindRegistedC40(arry[j].Typ) = nil then
                begin
                  DoStatus('no found %s', [arry[j].Typ.Text]);
                  error_ := True;
                end;
          end;
      end;

    if not error_ then
      begin
        if cmd_script_.ConfigIsUpdate then
          begin
            Z.Net.C4.C40ReadConfig(cmd_script_.Config);
            Z.Net.C4.C40_RootPath := cmd_script_.Config.GetDefaultValue('Root', Z.Net.C4.C40_RootPath);
            if not umlDirectoryExists(Z.Net.C4.C40_RootPath) then
                umlCreateDirectory(Z.Net.C4.C40_RootPath);
            Z.Net.C4.C40_Password := cmd_script_.Config.GetDefaultValue('Password', Z.Net.C4.C40_Password);
          end;

        if cmd_script_.Service_NetInfo_List.Count > 0 then
          begin
            for i := 0 to cmd_script_.Service_NetInfo_List.Count - 1 do
              begin
                net_info_ := cmd_script_.Service_NetInfo_List[i];

                with Z.Net.C4.TC40_PhysicsService.Create(
                  net_info_.listen_ip, net_info_.ip, net_info_.port, Z.Net.PhysicsIO.TPhysicsServer.Create) do
                  begin
                    AutoFreePhysicsTunnel := True;
                    BuildDependNetwork(net_info_.depend);
                    OnEvent := On_C40_PhysicsService_Event;
                    StartService;
                  end;

                IsInited_ := True;
              end;
          end;

        if cmd_script_.Client_NetInfo_List.Count > 0 then
          begin
            for i := 0 to cmd_script_.Client_NetInfo_List.Count - 1 do
              begin
                net_info_ := cmd_script_.Client_NetInfo_List[i];

                if net_info_.isAuto then
                    Z.Net.C4.C40_PhysicsTunnelPool.SearchServiceAndBuildConnection(
                    net_info_.ip, net_info_.port, not net_info_.Min_Workload, net_info_.depend, On_C40_PhysicsTunnel_Event)
                else
                    Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(
                    net_info_.ip, net_info_.port, net_info_.depend, On_C40_PhysicsTunnel_Event);

                IsInited_ := True;
              end;
          end;
      end;

    cmd_script_.Free;
  except
  end;
  Result := IsInited_;
end;

initialization

SetLength(C40AppParam, 0);
C40AppParsingTextStyle := TTextStyle.tsPascal;
On_C40_PhysicsTunnel_Event := nil;
On_C40_PhysicsService_Event := nil;

end.
