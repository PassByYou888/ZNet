program _1_TEKeyValue_Serv;

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
  Z.HashList.Templet,
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

begin
  StatusThreadID := False;
  // TEKeyValue服务等同于KeyValue数据库
  Z.Net.C4_Console_APP.C40AppParsingTextStyle := tsC;
  Z.Net.C4_Console_APP.C40AppParam := ['Service("0.0.0.0","127.0.0.1",9188,"TEKeyValue")'];

  if Z.Net.C4_Console_APP.C40_Extract_CmdLine then
    begin
      C40_Execute_Main_Loop;
    end;
  Z.Net.C4.C40Clean;

end.
