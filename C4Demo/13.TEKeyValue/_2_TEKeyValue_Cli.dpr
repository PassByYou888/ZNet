program _2_TEKeyValue_Cli;

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
  Z.Net.C4_TEKeyValue,
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

procedure Do_Test2(cli: TC40_TEKeyValue_Client; key_: U_String);
begin
  cli.GetTextValue_P('my_db', 'my_section', key_, '',
    procedure(sender: TC40_TEKeyValue_Client; Value_: U_String)
    begin
      DoStatus('%s = %s', [key_.Text, Value_.Text]);
    end);
end;

procedure Do_Test(cli: TC40_TEKeyValue_Client);
var
  m: Integer;
begin
  // 关闭log
  Z.Net.C4.C40SetQuietMode(True);

  // THashTextEngine内部有两种数据容器，分别是Variant和String，等同于INI配置文件分别有Variant/String
  // THashTextEngine在解析时会将文本根据需要自动翻译到目标容器，例如String容器相比Variant容器更节省cpu翻译开销，并且载入更快
  // 处于性能考虑，THashTextEngine不会自动处理不同数据容器，如果同时使用了两种数据容器，需要Rebuild一次

  // 在同一个Section中，要避免同时使用两种容器
  {
    SetValue('my_db', 'my_section', 'key1', variant)            // 赋予Variant值
    SetTextValue('my_db', 'my_section', 'key2', 'text')         // 赋予String值
  }
  // 在同一个Section中，如果要用两种容器，这样来干
  {
    Rebuild                                                     // 结构重构，等同于让不同的容器数据直接融合
    SetValue('my_db', 'my_section', 'key1', variant)            // 赋予Variant值
    Rebuild                                                     // 结构重构，等同于让不同的容器数据直接融合
    GetTextValue('my_db', 'my_section', 'key1')                 // 以String方式获取Variant值
    SetTextValue('my_db', 'my_section', 'key2', 'text')         // 赋予String值
    Rebuild                                                     // 结构重构，等同于让不同的容器数据直接融合
  }

  cli.Rebuild('my_db');
  for m := 1 to 10 do
      cli.SetValue('my_db', 'my_section', PFormat('Number_Key_%d', [m]), umlRandomRange(-1000, 1000));
  cli.Rebuild('my_db');
  for m := 1 to 10 do
      cli.SetTextValue('my_db', 'my_section', PFormat('ASCII_Key_%d', [m]), TPascalString.RandomString(10, [cAtoZ]));
  cli.Rebuild('my_db');

  cli.GetTextKey_P('my_db', 'my_section', procedure(sender: TC40_TEKeyValue_Client; arry: U_StringArray)
    var
      i: Integer;
    begin
      for i := low(arry) to high(arry) do
          Do_Test2(cli, arry[i]);
    end);
end;

procedure Do_Connection_Done(States: TC40_Custom_ClientPool_Wait_States);
var
  i: Integer;
  cli: TC40_TEKeyValue_Client;
begin
  DoStatus('ready.');
  for i := low(States) to high(States) do
    begin
      cli := TC40_TEKeyValue_Client(States[i].Client_);
      Do_Test(cli);
    end;
end;

begin
  StatusThreadID := false;

  // TEKeyValue服务等同于KeyValue数据库
  // TEKeyValue非常适合大规模存放数值和字符串参数
  // TEKeyValue服务支持数据融合，可以由两个端同时提交数据，它会自动融合这些数据
  Z.Net.C4_Console_APP.C40AppParsingTextStyle := tsC;
  Z.Net.C4_Console_APP.C40AppParam := ['Tunnel("127.0.0.1",9188,"TEKeyValue")'];

  Z.Net.C4.C40_ClientPool.WaitConnectedDoneC('TEKeyValue', Do_Connection_Done);

  if Z.Net.C4_Console_APP.C40_Extract_CmdLine then
    begin
      exit_signal := false;
      TCompute.RunC_NP(@Do_Check_On_Exit);
      while not exit_signal do
          Z.Net.C4.C40Progress;
    end;
  Z.Net.C4.C40Clean;

end.
