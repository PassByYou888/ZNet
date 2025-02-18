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
{ * cloud 4.0 console application framework                                    * }
{ ****************************************************************************** }
unit Z.Net.C4_Console_APP;

{$DEFINE FPC_DELPHI_MODE}
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
  Z.ZDB2, Z.ZDB2.DFE, Z.ZDB2.HS, Z.ZDB2.HV, Z.ZDB2.Json, Z.ZDB2.MS64, Z.ZDB2.NM, Z.ZDB2.TE, Z.ZDB2.FileEncoder,
  Z.Net.C4, Z.Net.C4_UserDB, Z.Net.C4_Var, Z.Net.C4_FS, Z.Net.C4_RandSeed, Z.Net.C4_Log_DB, Z.Net.C4_XNAT, Z.Net.C4_Alias,
  Z.Net.C4_FS2, Z.Net.C4_PascalRewrite_Client, Z.Net.C4_PascalRewrite_Service,
  Z.Net.C4_NetDisk_Admin_Tool,
  Z.Net.C4_TEKeyValue,
  Z.Net.PhysicsIO, Z.Net.C4_NetDisk_Client, Z.Net.C4_NetDisk_Directory,
  Z.Net.C4_NetDisk_Service;

var
  C40AppParam: U_StringArray;
  C40AppParsingTextStyle: TTextStyle;
  On_C40_PhysicsTunnel_Event_Console: IC40_PhysicsTunnel_Event;
  On_C40_PhysicsService_Event_Console: IC40_PhysicsService_Event;

procedure C40_Init_AppParamFromSystemCmdLine;
function C40_Extract_CmdLine(): Boolean; overload;
function C40_Extract_CmdLine(const Param_: U_StringArray): Boolean; overload;
function C40_Extract_CmdLine(const TextStyle_: TTextStyle; const Param_: U_StringArray): Boolean; overload;
procedure C40_Execute_Main_Loop;

implementation

uses Variants;

type
  TCmd_Net_Info_ = record
    listen_ip: string;
    ip: string;
    port: word;
    depend: string;
    isAuto, Min_Workload: Boolean;
    KeepAlive_Connected: Boolean;
    procedure Init;
  end;

  TCmd_Net_Info_List = TGenericsList<TCmd_Net_Info_>;

  TCommand_Script = class(TCore_Object_Intermediate)
  private
    function Do_Config(OpRunTime: TOpCustomRunTime; OP_RT_Data: POpRTData; var OP_Param: TOpParam): Variant;
    function Do_KeepAlive_Client(var OP_Param: TOpParam): Variant;
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
    procedure Execute(Expression: U_String);
  end;

procedure TCmd_Net_Info_.Init;
begin
  listen_ip := '';
  ip := '';
  port := 0;
  depend := '';
  isAuto := False;
  Min_Workload := False;
  KeepAlive_Connected := False;
end;

function TCommand_Script.Do_Config(OpRunTime: TOpCustomRunTime; OP_RT_Data: POpRTData; var OP_Param: TOpParam): Variant;
begin
  if length(OP_Param) > 0 then
    begin
      Config.SetDefaultValue(OP_RT_Data^.Name, VarToStr(OP_Param[0]));
      Result := True;
      ConfigIsUpdate := True;
    end
  else
      Result := Config[OP_RT_Data^.Name];
end;

function TCommand_Script.Do_KeepAlive_Client(var OP_Param: TOpParam): Variant;
var
  net_info_: TCmd_Net_Info_;
begin
  net_info_.Init;
  net_info_.listen_ip := '';
  net_info_.ip := OP_Param[0];
  net_info_.port := OP_Param[1];
  net_info_.depend := OP_Param[2];
  net_info_.isAuto := False;
  if length(OP_Param) > 3 then
      net_info_.Min_Workload := OP_Param[3]
  else
      net_info_.Min_Workload := False;
  net_info_.KeepAlive_Connected := True;
  Client_NetInfo_List.Add(net_info_);
  Result := True;
end;

function TCommand_Script.Do_AutoClient(var OP_Param: TOpParam): Variant;
var
  net_info_: TCmd_Net_Info_;
begin
  net_info_.Init;
  net_info_.listen_ip := '';
  net_info_.ip := OP_Param[0];
  net_info_.port := OP_Param[1];
  net_info_.depend := OP_Param[2];
  net_info_.isAuto := True;
  if length(OP_Param) > 3 then
      net_info_.Min_Workload := OP_Param[3]
  else
      net_info_.Min_Workload := False;
  net_info_.KeepAlive_Connected := False;
  Client_NetInfo_List.Add(net_info_);
  Result := True;
end;

function TCommand_Script.Do_Client(var OP_Param: TOpParam): Variant;
var
  net_info_: TCmd_Net_Info_;
begin
  net_info_.Init;
  net_info_.listen_ip := '';
  net_info_.ip := OP_Param[0];
  net_info_.port := OP_Param[1];
  net_info_.depend := OP_Param[2];
  net_info_.isAuto := False;
  net_info_.Min_Workload := False;
  net_info_.KeepAlive_Connected := False;
  Client_NetInfo_List.Add(net_info_);
  Result := True;
end;

function TCommand_Script.Do_Service(var OP_Param: TOpParam): Variant;
var
  net_info_: TCmd_Net_Info_;
begin
  net_info_.Init;
  if length(OP_Param) > 3 then
    begin
      net_info_.listen_ip := OP_Param[0];
      net_info_.ip := OP_Param[1];
      net_info_.port := OP_Param[2];
      net_info_.depend := OP_Param[3];
      net_info_.isAuto := False;
      net_info_.Min_Workload := False;
      net_info_.KeepAlive_Connected := False;
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
      net_info_.KeepAlive_Connected := False;
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
      opRT.Reg_RT_OpM(L[i], Do_Config)^.Category := 'C4 Param variant';
    end;
  disposeObject(L);

  opRT.Reg_Param_OpM('KeepAlive', Do_KeepAlive_Client)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('KeepAliveClient', Do_KeepAlive_Client)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('KeepAliveCli', Do_KeepAlive_Client)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('KeepAliveTunnel', Do_KeepAlive_Client)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('KeepAliveConnect', Do_KeepAlive_Client)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('KeepAliveConnection', Do_KeepAlive_Client)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('KeepAliveNet', Do_KeepAlive_Client)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('KeepAliveBuild', Do_KeepAlive_Client)^.Category := 'C4 Param Command';

  opRT.Reg_Param_OpM('Auto', Do_AutoClient)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('AutoClient', Do_AutoClient)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('AutoCli', Do_AutoClient)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('AutoTunnel', Do_AutoClient)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('AutoConnect', Do_AutoClient)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('AutoConnection', Do_AutoClient)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('AutoNet', Do_AutoClient)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('AutoBuild', Do_AutoClient)^.Category := 'C4 Param Command';

  opRT.Reg_Param_OpM('Client', Do_Client)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('Cli', Do_Client)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('Tunnel', Do_Client)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('Connect', Do_Client)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('Connection', Do_Client)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('Net', Do_Client)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('Build', Do_Client)^.Category := 'C4 Param Command';

  opRT.Reg_Param_OpM('Service', Do_Service)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('Server', Do_Service)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('Serv', Do_Service)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('Listen', Do_Service)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('Listening', Do_Service)^.Category := 'C4 Param Command';

  opRT.Reg_Param_OpM('Wait', Do_Sleep)^.Category := 'C4 Param Command';
  opRT.Reg_Param_OpM('Sleep', Do_Sleep)^.Category := 'C4 Param Command';
end;

procedure TCommand_Script.Execute(Expression: U_String);
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
  c4_opt: THashStringList;
  phy_: TC40_PhysicsTunnel;
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
      begin
        // ignore none c4 param
        if (not umlMultipleMatch([
              '-Task:*', '-TaskID:*', // Protected Param
              '-minimized', 'minimized', '-min', 'min', // Protected Param
              '-Max_Mem_Protected:*', '-Max_Memory:*', '-Memory:*', '-Mem:*', 'mem:*', 'memory:*', // Protected Param
              '-NUMA:*', 'NUMA:*', '-NODE:*', 'Node:*', // Protected Param
              '-D3D', '-D3D', '-D2D', '-GPU', '-SOFT', '-GrayTheme', '-DefaultTheme' // fmx app param
              ], C40AppParam[i])) and
          ((Ignore_Command_Line.Count <= 0) or (not umlMultipleMatch(Ignore_Command_Line, C40AppParam[i]))) then
            cmd_script_.Execute(C40AppParam[i]);
      end;

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
            IsInited_ := True;
            for i := 0 to cmd_script_.Service_NetInfo_List.Count - 1 do
              begin
                net_info_ := cmd_script_.Service_NetInfo_List[i];

                with Z.Net.C4.TC40_PhysicsService.Create(
                  net_info_.listen_ip, net_info_.ip, net_info_.port, Z.Net.PhysicsIO.TPhysicsServer.Create) do
                  begin
                    AutoFreePhysicsTunnel := True;
                    BuildDependNetwork(net_info_.depend);
                    OnEvent := On_C40_PhysicsService_Event_Console;
                    StartService;
                    IsInited_ := IsInited_ or Activted;
                  end;
              end;
          end;

        if cmd_script_.Client_NetInfo_List.Count > 0 then
          begin
            IsInited_ := True;
            for i := 0 to cmd_script_.Client_NetInfo_List.Count - 1 do
              begin
                net_info_ := cmd_script_.Client_NetInfo_List[i];

                if net_info_.KeepAlive_Connected then
                  begin
                    C40_PhysicsTunnelPool.Auto_Repair_First_BuildDependNetwork_Fault := True;
                  end;

                if net_info_.isAuto then
                    Z.Net.C4.C40_PhysicsTunnelPool.SearchServiceAndBuildConnection(
                    net_info_.ip, net_info_.port, not net_info_.Min_Workload, net_info_.depend, On_C40_PhysicsTunnel_Event_Console)
                else
                    Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(
                    net_info_.ip, net_info_.port, net_info_.depend, On_C40_PhysicsTunnel_Event_Console);
              end;
          end;
      end;

    cmd_script_.Free;

{$IFDEF DEBUG}
    if IsInited_ then
      begin
        c4_opt := THashStringList.Create;
        C40WriteConfig(c4_opt);
        DoStatus('');
        DoStatus('C40 Network Options');
        DoStatus(c4_opt.AsText);
        disposeObject(c4_opt);
        DoStatus('');
      end;
{$ENDIF DEBUG}
  except
  end;
  Result := IsInited_;
end;

function C40_Extract_CmdLine(const Param_: U_StringArray): Boolean;
begin
  C40AppParam := Param_;
  Result := C40_Extract_CmdLine();
end;

function C40_Extract_CmdLine(const TextStyle_: TTextStyle; const Param_: U_StringArray): Boolean;
begin
  C40AppParsingTextStyle := TextStyle_;
  C40AppParam := Param_;
  Result := C40_Extract_CmdLine();
end;

type
  TMain_Loop_Instance__ = class(TCore_Object_Intermediate)
  private
    exit_signal: Boolean;
    procedure Do_Check_On_Exit;
  public
    constructor Create;
    procedure Wait();
  end;

procedure TMain_Loop_Instance__.Do_Check_On_Exit;
var
  n: string;
  cH: TC40_Console_Help;
begin
  TCompute.Set_Thread_Info('C4 Console-help Thread');
  cH := nil;
  repeat
    TCompute.Sleep(100);
    Readln(n);
    n := umlTrimSpace(n);
    if cH = nil then
        cH := TC40_Console_Help.Create;
    if n <> '' then
        cH.Run_HelpCmd(n);
  until cH.IsExit;
  DisposeObjectAndNil(cH);
  exit_signal := True;
end;

constructor TMain_Loop_Instance__.Create;
begin
  inherited Create;
  exit_signal := False;
  TCompute.RunM_NP(Do_Check_On_Exit);
end;

procedure TMain_Loop_Instance__.Wait;
begin
  while not exit_signal do
      Z.Net.C4.C40Progress;
end;

procedure C40_Execute_Main_Loop;
begin
  with TMain_Loop_Instance__.Create do
    begin
      Wait;
      Free;
    end;
end;

initialization

SetLength(C40AppParam, 0);
C40AppParsingTextStyle := TTextStyle.tsPascal;

try
  On_C40_PhysicsTunnel_Event_Console := nil;
  On_C40_PhysicsService_Event_Console := nil;
except
end;

finalization

try
  On_C40_PhysicsTunnel_Event_Console := nil;
  On_C40_PhysicsService_Event_Console := nil;
except
end;

end.
 
