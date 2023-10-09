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

procedure Second_Main(); // 模拟双主线程,这里是第2个主线Main入口
var
  cli: TMY_Cli;
begin
  if C40_Extract_CmdLine(TTextStyle.tsC, // 极简C4框架,方便测试
    ['Service("0.0.0.0","127.0.0.1", 9000, "Serv000")', 'Client("127.0.0.1", 9000, "Serv000")']) then
    begin
      DoStatus('第2个主线程ID: %d', [Core_Main_Thread_ID]); // Z系自己封装了底层的线程同步机制,也定义自己的灵活主线模型
      TC40_Auto_Deploy<TMY_Cli>.Create(cli); // 看到依赖服务readyok提示表示c4网络就绪
      C40_Execute_Main_Loop;
    end;
  C40Clean; // ZNet跑多线程没有任何问题,同时也验证了ZNet体系线程模型不依赖UI Synchronize机制
end; // 执行到这里模拟主线程结束,回归原始线程状态

begin
  DoStatus('第1个主线程ID: %d', [Core_Main_Thread_ID]); // primary main thread
  RegisterC40('Serv000', TMY_Serv, TMY_Cli); // 使用C4需要先注册一下, Serv000表示服务器体系
  // 第1个主线程是RTL, 第2个主线程是Z系
  // Z系双主线程模型,全面测试通过,良好支持C4,SrossSocket,ICS8,ICS9,Indy,Synapse,DIOCP
  // 这种模型非常适合DLL,OCX,ActiveX,开分支服务,所有实例都能支持一个双线,例如,挂接10个双线服务,只需要LoadLibrary达到10次
  // 在DLL中,Begin_Simulator_Main_Thread所指向的 Second_Main 等同于 Exe 的入口
  // System系内变量包括MainThreadID均未涉及,双主线程模型只针对Z系全库
  // 双主线程模型不支持UI,凡是用 CheckSynchronize,TThread.Synchronize 的地方,双主线都不支持
  // 如果要强行支持,需接入程序模型: TSoft_Synchronize_Tool
  Begin_Simulator_Main_Thread(Second_Main); // 使用双主线技术需要打开编译器定义 Core_Thread_Soft_Synchronize
  while Simulator_Main_Thread_Activted do // 等模拟主线结束,结束以后,会自动还原Z系的线程环境,双主线模型未修改任何RTL变量
      TCompute.Sleep(1);
  Second_Main(); // 程序执行完成后,运行到这里,已经还原线程环境,再运行一次C4服务,这次是在第一个主线空间运行

end.
