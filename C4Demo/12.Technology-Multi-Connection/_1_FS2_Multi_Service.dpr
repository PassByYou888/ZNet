program _1_FS2_Multi_Service;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Windows,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_FS2;

var
  exit_signal: Boolean;

procedure Do_Check_On_Exit;
var
  n: string;
begin
  repeat
    TCompute.Sleep(100);
    Readln(n);
  until umlMultipleMatch(['exit', 'close'], n);
  exit_signal := True;
end;

const
  // 调度服务器端口公网地址,可以是ipv4,ipv6,dns
  // 公共地址,不能给127.0.0.1这类
  Internet_DP_Addr_ = '127.0.0.1';
  // 调度服务器端口
  Internet_DP_Port_ = 8387;

function GetMyUserDB_Service: TC40_FS2_Service;
var
  arry: TC40_Custom_Service_Array;
begin
  arry := C40_ServicePool.GetFromServiceTyp('FS2');
  if length(arry) > 0 then
      Result := arry[0] as TC40_FS2_Service
  else
      Result := nil;
end;

begin
  // 打开Log信息
  Z.Net.C4.C40_QuietMode := False;

  // 创建调度服务和文件系统服务
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_DP_Addr_, Internet_DP_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      // FS@SafeCheckTime=5000 是作为fs服务器的构建参数，SafeCheckTime表示安全检测，IO数据写入磁盘的时间间隔
      BuildDependNetwork('FS2@SafeCheckTime=5000');
      StartService;
    end;

  // 接通调度端
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'DP', nil);

  // 主循环
  // 信号循环模型，console里面输入exit关闭
  exit_signal := False;
  TCompute.RunC_NP(@Do_Check_On_Exit);
  while not exit_signal do
      Z.Net.C4.C40Progress;
  Z.Net.C4.C40Clean;

end.
