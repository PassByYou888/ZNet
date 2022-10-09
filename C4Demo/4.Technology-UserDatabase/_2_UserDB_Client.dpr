program _2_UserDB_Client;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_UserDB,
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

function GetMyUserDB_Client: TC40_UserDB_Client;
begin
  Result := TC40_UserDB_Client(C40_ClientPool.ExistsConnectedServiceTyp('userDB'));
end;

begin
  // 打开Log信息
  Z.Net.C4.C40_QuietMode := False;

  // 接通调度端和用户身份数据库服务
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'DP|UserDB', nil);

  // WaitConnectedDone可以同时检查多个依赖服务是否就绪
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('DP|UserDB', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    begin
      GetMyUserDB_Client.Usr_RegC('testUser', '123456', nil);

      // 给testUser永久增加一个可用与登录的别名，例如增加用户的电子邮件，手机号码
      GetMyUserDB_Client.Usr_NewIdentifierP('testUser', 'test@mail.com',
          procedure(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString)
        begin
          DoStatus(info_);
        end);

      // 使用别名远程验证用户身份，这里只是验证返回，便于VM服务器工作，userDB并不会做任何登录处理，登录处理在VM服务器干
      GetMyUserDB_Client.Usr_AuthP('test@mail.com', '123456',
        procedure(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString)
        begin
          DoStatus(info_);
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
