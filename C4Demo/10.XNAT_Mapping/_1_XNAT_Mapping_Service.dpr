program _1_XNAT_Mapping_Service;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_XNAT,
  Z.Status,
  Z.Net.C4_Console_APP;

var
  exit_signal: Boolean;

procedure Do_Check_On_Exit;
var
  n: string;
  cH: TC40_Console_Help;
begin
  cH := TC40_Console_Help.Create;
  repeat
    TCompute.Sleep(100);
    Readln(n);
    cH.Run_HelpCmd(n);
  until cH.IsExit;
  disposeObject(cH);
  exit_signal := True;
end;

const
  Internet_XNAT_Service_Addr_ = '127.0.0.1';
  Internet_XNAT_Service_Port_ = 8397;

begin
  RegisterC40('MY_XNAT_1', TC40_XNAT_Service_Tool, TC40_XNAT_Client_Tool);

  Z.Net.C4.C40_QuietMode := False;

  // 创建自动化XNAT配置服务，为SaaS网络提供内网穿透支持
  // 使用C4的XNAT配置服务时不要挂太多穿透，1-2个就够了，如果需要多穿，就多开几个配置服务
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_XNAT_Service_Addr_, Internet_XNAT_Service_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('MY_XNAT_1@XNAT_Host:127.0.0.1,XNAT_Port:9911');
      StartService;
    end;

  // 主循环
  StatusThreadID := False;
  exit_signal := False;
  TCompute.RunC_NP(@Do_Check_On_Exit);
  while not exit_signal do
      Z.Net.C4.C40Progress;

  Z.Net.C4.C40Clean;

end.
