program C4_LFService;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UPascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.ListEngine,
  Z.GHashList,
  Z.Expression,
  Z.OpCode,
  Z.Parsing,
  Z.DFE,
  Z.TextDataEngine,
  Z.MemoryStream,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_FS2,
  Z.Net.C4_Console_APP;

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

begin
  // 大规模文件传输服务含有文件数据库
  // console方式不要直接关闭
  // 本服务器可以直接copy代码到laz环境构建成IOT,linux系统，注意：在fpc-console应用是无法使用LCL的，必须是NoUI程序，C4可以支持No LCL应用环境
  Z.Net.C4_Console_APP.C40AppParsingTextStyle := TTextStyle.tsC;
  Z.Net.C4_Console_APP.C40AppParam := [
    Format('Service("0.0.0.0","%s",9188,"DP")', ['127.0.0.1']),
    Format('Service("0.0.0.0","%s",9189,"FS2")', ['127.0.0.1'])
    ];

  DoStatus('Prepare service.');

  if Z.Net.C4_Console_APP.C40_Extract_CmdLine then
    begin
      exit_signal := False;
      TCompute.RunC_NP(@Do_Check_On_Exit);
      while not exit_signal do
          Z.Net.C4.C40Progress;
    end;

  Z.Net.C4.C40Clean;

end.
