program _167_Thread_Info_Demo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  Z.Core,
  Z.PascalStrings,
  Z.Status,
  Z.Net.C4_Console_APP;

{
  线程标签技术
  在超算程序中,线程种类可以多达几十种,注意这里不是几十条线程,是几十种线程,这些线程很有必要都打上标签,这样可以跟踪和监视线程的运行状态
  例如在运行算法的流程中,从启动到结束,线程会有计算消耗状态,运行时间,通过观察状态可以在服务器出现故障时定位出问题所在的位置
  本demo偏高阶,需要使用c4的命令行框架,
  使用方法,启动demo,命令行敲入,"hpc_thread_info"
  退出使用"exit"命令
  在Z系线程系统中必须是所有线程正常结束才能退出app
  提示:c4的运行时命令行工具可以支持console,vcl,fmx,lcl等等平台,兼容fpc
}

procedure th1;
begin
  TCompute.Set_Thread_Info('线程1,大约5秒后结束');
  TCompute.Sleep(1000 * 5);
end;

procedure th2;
begin
  TCompute.Set_Thread_Info('线程2,大约10秒后结束');
  TCompute.Sleep(1000 * 10);
end;

procedure th3;
begin
  TCompute.Set_Thread_Info('线程3,大约15秒后结束');
  TCompute.Sleep(1000 * 15);
end;

begin
  TCompute.RunC_NP(th1);
  TCompute.RunC_NP(th2);
  TCompute.RunC_NP(th3);
  Z.Net.C4_Console_APP.C40_Execute_Main_Loop;
end.
