program C4_Serv;

{$APPTYPE CONSOLE}

{$R *.res}

// web api��һ��ͨ��Ӧ�ó���ӿ�,�ں�˾���c4ƽ̨
// ���κ�ʱ���Ҷ�����ʹ��c4������ƽ̨,���ȶ���,ģ�黯,��������,�߳���Ϣ��,�����������е���

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

  // �Խӽ��ű��ı��ʽ������c4
  if C40_Extract_CmdLine(tsC, ['Service("0.0.0.0","127.0.0.1","9399","Demo")']) then
    begin
      DoStatus('������ "help" ��ӡ���������е�������.');
      DoStatus('Set_Demo_Info�������ֱ���޸�webapi�õ�demo_info');
      exit_signal := False;
      TCompute.RunC_NP(@Do_Check_On_Exit);
      while not exit_signal do
          Z.Net.C4.C40Progress;
    end;
  Z.Net.C4.C40Clean;

end.
