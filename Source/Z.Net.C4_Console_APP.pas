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
  Z.ListEngine, Z.GHashList, Z.Expression, Z.OpCode, Z.Parsing, Z.DFE, Z.TextDataEngine,
  Z.Json, Z.Geometry2D, Z.Geometry3D, Z.Number,
  Z.MemoryStream,
  Z.Net,
  Z.ZDB.ObjectData_LIB, Z.ZDB, Z.ZDB.Engine, Z.ZDB.LocalManager,
  Z.ZDB.FileIndexPackage_LIB, Z.ZDB.FilePackage_LIB, Z.ZDB.ItemStream_LIB, Z.ZDB.HashField_LIB, Z.ZDB.HashItem_LIB,
  Z.ZDB2.Custom, Z.ZDB2, Z.ZDB2.DFE, Z.ZDB2.HS, Z.ZDB2.HV, Z.ZDB2.Json, Z.ZDB2.MS64, Z.ZDB2.NM, Z.ZDB2.TE, Z.ZDB2.FileEncoder,
  Z.Net.C4, Z.Net.C4_UserDB, Z.Net.C4_Var, Z.Net.C4_FS, Z.Net.C4_RandSeed, Z.Net.C4_Log_DB, Z.Net.C4_XNAT, Z.Net.C4_Alias,
  Z.Net.C4_FS2, Z.Net.C4_PascalRewrite_Client, Z.Net.C4_PascalRewrite_Service,
  Z.ZDB2.MEM64, Z.ZDB2.ObjectDataManager,
  Z.Net.PhysicsIO;

var
  C40AppParam: U_StringArray;
  C40AppParsingTextStyle: TTextStyle;
  On_C40_PhysicsTunnel_Event: IC40_PhysicsTunnel_Event;
  On_C40_PhysicsService_Event: IC40_PhysicsService_Event;

procedure C40_Init_AppParamFromSystemCmdLine;
function C40_Extract_CmdLine(): Boolean;

implementation

uses Variants;

type
  TCmd_Net_Info_ = record
    listen_ip: string;
    ip: string;
    port: Word;
    depend: string;
  end;

  TCmd_Net_Info_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TCmd_Net_Info_>;

  TCommand_Script = class
  private
    function Do_Config(var OP_Param: TOpParam): Variant;
    function Do_Client(var OP_Param: TOpParam): Variant;
    function Do_Service(var OP_Param: TOpParam): Variant;
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

function TCommand_Script.Do_Client(var OP_Param: TOpParam): Variant;
var
  net_info_: TCmd_Net_Info_;
begin
  net_info_.listen_ip := '';
  net_info_.ip := OP_Param[0];
  net_info_.port := OP_Param[1];
  net_info_.depend := OP_Param[2];
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
      Service_NetInfo_List.Add(net_info_);
    end;
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

  opRT.RegOpM('Service', {$IFDEF FPC}@{$ENDIF FPC}Do_Service);
  opRT.RegOpM('Serv', {$IFDEF FPC}@{$ENDIF FPC}Do_Service);
  opRT.RegOpM('Listen', {$IFDEF FPC}@{$ENDIF FPC}Do_Service);
  opRT.RegOpM('Listening', {$IFDEF FPC}@{$ENDIF FPC}Do_Service);
  opRT.RegOpM('Client', {$IFDEF FPC}@{$ENDIF FPC}Do_Client);
  opRT.RegOpM('Cli', {$IFDEF FPC}@{$ENDIF FPC}Do_Client);
  opRT.RegOpM('Tunnel', {$IFDEF FPC}@{$ENDIF FPC}Do_Client);
  opRT.RegOpM('Connect', {$IFDEF FPC}@{$ENDIF FPC}Do_Client);
  opRT.RegOpM('Connection', {$IFDEF FPC}@{$ENDIF FPC}Do_Client);
  opRT.RegOpM('Net', {$IFDEF FPC}@{$ENDIF FPC}Do_Client);
  opRT.RegOpM('Build', {$IFDEF FPC}@{$ENDIF FPC}Do_Client);
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
