program _2_LOG_DB_Client;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.MemoryStream,
  Z.Notify,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_Log_DB, DateUtils,
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
  // 调度服务器端口公网地址,可以是ipv4,ipv6,dns
  // 公共地址,不能给127.0.0.1这类
  Internet_DP_Addr_ = '127.0.0.1';
  // 调度服务器端口
  Internet_DP_Port_ = 8387;

function Get_Log_DB_Client: TC40_Log_DB_Client;
begin
  Result := TC40_Log_DB_Client(C40_ClientPool.FindConnectedServiceTyp('Log'));
end;

begin
  Z.Net.C4.C40_QuietMode := False;
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'DP|Log', nil);

  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('Log', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    var
      i, j: integer;
    begin
      for j := 1 to 20 do
        for i := 1 to 10 do
          begin
            Get_Log_DB_Client.PostLog(PFormat('test_log_db_%d', [j]), PFormat('log %d', [i]), PFormat('log %d', [i * i]));
          end;

      Get_Log_DB_Client.GetLogDBP(procedure(Sender: TC40_Log_DB_Client; arry: U_StringArray)
        var
          i: integer;
        begin
          for i := 0 to length(arry) - 1 do
              doStatus(arry[i]);
        end);

      Get_Log_DB_Client.QueryLogP('test_log_db_1', IncHour(now, -1), IncHour(now, 1),
        procedure(Sender: TC40_Log_DB_Client; LogDB: SystemString; arry: TArrayLogData)
        var
          i: integer;
        begin
          for i := 0 to length(arry) - 1 do
              doStatus(arry[i].Log1);
          doStatus('query done.');
        end);
    end);

  // 主循环
  StatusThreadID := False;
  exit_signal := False;
  TCompute.RunC_NP(@Do_Check_On_Exit);
  while not exit_signal do
      Z.Net.C4.C40Progress;

  Z.Net.C4.C40Clean;

end.
