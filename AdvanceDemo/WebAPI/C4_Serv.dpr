program C4_Serv;

{$APPTYPE CONSOLE}

{$R *.res}

// web api是一道通用应用程序接口,在后端就是c4平台
// 在任何时候我都建议使用c4做基建平台,高稳定性,模组化,断线重连,线程信息化,服务器运行中调试

uses
  SysUtils,
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status, Z.HashList.Templet, Z.Expression, Z.OpCode, Z.Parsing,
  Z.DFE, Z.Net, Z.Net.PhysicsIO, Z.Net.C4, Z.Net.C4_Console_APP,
  C4_Demo_Service in 'C4_Demo_Service.pas';

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

begin
  StatusThreadID := False;

  // 以接近脚本的表达式来启动c4
  if C40_Extract_CmdLine(tsC, ['Service("0.0.0.0","127.0.0.1","9399","Demo")']) then
    begin
      DoStatus('命令行 "help" 打印服务器运行调试命令.');
      DoStatus('Set_Demo_Info命令可以直接修改webapi得到demo_info');
      exit_signal := False;
      TCompute.RunC_NP(@Do_Check_On_Exit);
      while not exit_signal do
          Z.Net.C4.C40Progress;
    end;
  Z.Net.C4.C40Clean;

end.
