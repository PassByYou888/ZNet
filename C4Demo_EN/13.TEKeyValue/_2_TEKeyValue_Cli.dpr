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
  j, m: Integer;
begin
  {  Close log  }
  Z.Net.C4.C40SetQuietMode(True);

  {  There are two types of data containers inside THashTextEngine, namely Variant and String, which are equivalent to the INI configuration files having Variant/String respectively  }
  {  THashTextEngine will automatically translate text to the target container as needed during parsing, such as the String container, which saves CPU translation costs and loads faster compared to the Variant container  }
  {  For performance reasons, THashTextEngine will not automatically process different data containers. If two data containers are used at the same time, Rebuild is required once  }

  {  In the same section, avoid using two containers at the same time  }
  {  SetValue ('my_db ','my_section','key1 ', variant)//Assign a Variant value
SetTextValue ('my_db ','my_section', 'key2', 'text')//Assign a String value  }
  {  In the same section, if you want to use two types of containers, do this  }
  {  Rebuild//Structural reconstruction, equivalent to directly integrating data from different containers
SetValue ('my_db ','my_section','key1 ', variant)//Assign a Variant value
Rebuild//Structural reconstruction, equivalent to directly integrating data from different containers
GetTextValue ('my_db ','my_section','key1 ')//Get the Variant value as a String
SetTextValue ('my_db ','my_section', 'key2', 'text')//Assign a String value
Rebuild//Structural reconstruction, equivalent to directly integrating data from different containers  }

  cli.Rebuild('my_db');
  for m := 1 to 10 do
      cli.SetValue('my_db', 'my_section', PFormat('Number_Key_%d', [m]), umlRandomRange(-1000, 1000));
  cli.Rebuild('my_db');
  for m := 1 to 10 do
      cli.SetTextValue('my_db', 'my_section', PFormat('ASCII_Key_%d', [m]), TPascalString.RandomString(10, [cAtoZ]));
  cli.Rebuild('my_db');

  for j := 1 to 100 do
      cli.Rebuild(PFormat('my_db_%d', [j]));

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

  {  The TEKeyValue service is equivalent to the KeyValue database  }
  {  TEKeyValue is very suitable for storing numerical and string parameters on a large scale  }
  {  The TEKeyValue service supports data fusion, which can be submitted by both ends simultaneously. It will automatically fuse these data  }
  Z.Net.C4_Console_APP.C40AppParsingTextStyle := tsC;
  Z.Net.C4_Console_APP.C40AppParam := ['Tunnel("127.0.0.1",9188,"TEKeyValue")'];

  Z.Net.C4.C40_ClientPool.WaitConnectedDoneC('TEKeyValue', Do_Connection_Done);

  if Z.Net.C4_Console_APP.C40_Extract_CmdLine then
    begin
      C40_Execute_Main_Loop;
    end;
  Z.Net.C4.C40Clean;

end.
