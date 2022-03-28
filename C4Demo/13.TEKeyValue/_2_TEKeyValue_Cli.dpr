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
  j, m: Integer;
begin
  // �ر�log
  Z.Net.C4.C40SetQuietMode(True);

  // THashTextEngine�ڲ������������������ֱ���Variant��String����ͬ��INI�����ļ��ֱ���Variant/String
  // THashTextEngine�ڽ���ʱ�Ὣ�ı�������Ҫ�Զ����뵽Ŀ������������String�������Variant��������ʡcpu���뿪���������������
  // �������ܿ��ǣ�THashTextEngine�����Զ�����ͬ�������������ͬʱʹ��������������������ҪRebuildһ��

  // ��ͬһ��Section�У�Ҫ����ͬʱʹ����������
  {
    SetValue('my_db', 'my_section', 'key1', variant)            // ����Variantֵ
    SetTextValue('my_db', 'my_section', 'key2', 'text')         // ����Stringֵ
  }
  // ��ͬһ��Section�У����Ҫ��������������������
  {
    Rebuild                                                     // �ṹ�ع�����ͬ���ò�ͬ����������ֱ���ں�
    SetValue('my_db', 'my_section', 'key1', variant)            // ����Variantֵ
    Rebuild                                                     // �ṹ�ع�����ͬ���ò�ͬ����������ֱ���ں�
    GetTextValue('my_db', 'my_section', 'key1')                 // ��String��ʽ��ȡVariantֵ
    SetTextValue('my_db', 'my_section', 'key2', 'text')         // ����Stringֵ
    Rebuild                                                     // �ṹ�ع�����ͬ���ò�ͬ����������ֱ���ں�
  }

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

  // TEKeyValue�����ͬ��KeyValue���ݿ�
  // TEKeyValue�ǳ��ʺϴ��ģ�����ֵ���ַ�������
  // TEKeyValue����֧�������ںϣ�������������ͬʱ�ύ���ݣ������Զ��ں���Щ����
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
