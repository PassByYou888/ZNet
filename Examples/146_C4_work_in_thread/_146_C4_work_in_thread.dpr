program _146_C4_work_in_thread;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.DFE, Z.Parsing, Z.Expression, Z.Opcode, Z.Status,
  Z.Net, Z.Net.C4, Z.Net.C4_Console_APP;

type
  TMY_Serv = class(TC40_Base_NoAuth_Service);
  TMY_Cli = class(TC40_Base_NoAuth_Client);

procedure Second_Main(); // ģ��˫���߳�,�����ǵ�2������Main���
var
  cli: TMY_Cli;
begin
  if C40_Extract_CmdLine(TTextStyle.tsC, // ����C4���,�������
    ['Service("0.0.0.0","127.0.0.1", 9000, "Serv000")', 'Client("127.0.0.1", 9000, "Serv000")']) then
    begin
      DoStatus('��2�����߳�ID: %d', [Core_Main_Thread_ID]); // Zϵ�Լ���װ�˵ײ���߳�ͬ������,Ҳ�����Լ����������ģ��
      TC40_Auto_Deploy<TMY_Cli>.Create(cli); // ������������readyok��ʾ��ʾc4�������
      C40_Execute_Main_Loop;
    end;
  C40Clean; // ZNet�ܶ��߳�û���κ�����,ͬʱҲ��֤��ZNet��ϵ�߳�ģ�Ͳ�����UI Synchronize����
end; // ִ�е�����ģ�����߳̽���,�ع�ԭʼ�߳�״̬

begin
  DoStatus('��1�����߳�ID: %d', [Core_Main_Thread_ID]); // primary main thread
  RegisterC40('Serv000', TMY_Serv, TMY_Cli); // ʹ��C4��Ҫ��ע��һ��, Serv000��ʾ��������ϵ
  // ��1�����߳���RTL, ��2�����߳���Zϵ
  // Zϵ˫���߳�ģ��,ȫ�����ͨ��,����֧��C4,SrossSocket,ICS8,ICS9,Indy,Synapse,DIOCP
  // ����ģ�ͷǳ��ʺ�DLL,OCX,ActiveX,����֧����,����ʵ������֧��һ��˫��,����,�ҽ�10��˫�߷���,ֻ��ҪLoadLibrary�ﵽ10��
  // ��DLL��,Begin_Simulator_Main_Thread��ָ��� Second_Main ��ͬ�� Exe �����
  // Systemϵ�ڱ�������MainThreadID��δ�漰,˫���߳�ģ��ֻ���Zϵȫ��
  // ˫���߳�ģ�Ͳ�֧��UI,������ CheckSynchronize,TThread.Synchronize �ĵط�,˫���߶���֧��
  // ���Ҫǿ��֧��,��������ģ��: TSoft_Synchronize_Tool
  Begin_Simulator_Main_Thread(Second_Main); // ʹ��˫���߼�����Ҫ�򿪱��������� Core_Thread_Soft_Synchronize
  while Simulator_Main_Thread_Activted do // ��ģ�����߽���,�����Ժ�,���Զ���ԭZϵ���̻߳���,˫����ģ��δ�޸��κ�RTL����
      TCompute.Sleep(1);
  Second_Main(); // ����ִ����ɺ�,���е�����,�Ѿ���ԭ�̻߳���,������һ��C4����,������ڵ�һ�����߿ռ�����

end.
