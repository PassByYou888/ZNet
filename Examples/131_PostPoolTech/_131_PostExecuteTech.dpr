program _131_PostExecuteTech;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.Status,
  Z.Notify;

{
  PostExecute是一种定向抛出执行事件的容器方法,执行代码可以抛给任意线程,从而实现任务调度,数据调度这类功能
  本demo演示了两种不同的序列化定向发送代码给线程的执行机制
}

procedure do_Main_Demo_TThreadPost();
var
  activted: Boolean;
  // TThreadPost:在保证线程安全前提下,这是一种可以将代码安全发送至目标线程中按发送序列化高速执行,支持d/fpc
  // 在AI中的dnn-thread技术使用了TThreadPost
  // 在Z.ZDB2.Thread.Queue库中命令队列技术使用了TThreadPost
  th_1_post_tool: TThreadPost;
  TK: TTimeTick;
  isExit: Boolean; // TCompute线程的执行终止状态机,它是线程安全的原子变量
begin
  activted := True;
  th_1_post_tool := TThreadPost.Create(0);
  TCompute.RunP(nil, nil, procedure(ThSender: TCompute)
    begin
      DoStatus('TThreadPost:当前驱动线程ID:%d', [ThSender.ThreadID]);
      th_1_post_tool.ThreadID := ThSender.ThreadID;
      while activted do
        begin
          th_1_post_tool.Progress();
          TCompute.Sleep(1);
        end;
    end, nil, nil, @isExit);

  // 往线程发送执行代码
  th_1_post_tool.PostP1(procedure
    begin
      DoStatus('TThreadPost(1):执行线程ID:%d', [TCompute.CurrentThread.ThreadID]);
    end);
  th_1_post_tool.PostP1(procedure
    begin
      DoStatus('TThreadPost(2):执行线程ID:%d', [TCompute.CurrentThread.ThreadID]);
    end);
  th_1_post_tool.PostP1(procedure
    begin
      DoStatus('TThreadPost(3):执行线程ID:%d', [TCompute.CurrentThread.ThreadID]);
    end);

  TK := GetTimeTick;

  while GetTimeTick - TK < 2000 do
    begin
      th_1_post_tool.Progress();
      CheckThread(1);
    end;
  activted := False;

  while not isExit do
      TCompute.Sleep(1);

  DisposeObjectAndNil(th_1_post_tool);
end;

procedure do_Main_Demo_Progress_Post();
var
  activted: Boolean;
  // TN_Progress_Tool:保证线程安全前提下,支持高级物理时间转换器和数据结构的序列化执行,支持d/fpc
  // 在ZNet中TN_Progress_Tool作为延迟执行机制被大量使用
  th_2_progress_tool: TCadencer_N_Progress_Tool;
  TK: TTimeTick;
  isExit: Boolean; // TCompute线程的执行终止状态机,它是线程安全的原子变量
begin
  activted := True;
  th_2_progress_tool := TCadencer_N_Progress_Tool.Create;
  TCompute.RunP(nil, nil, procedure(ThSender: TCompute)
    begin
      DoStatus('TN_Progress_Tool:当前驱动线程ID:%d', [ThSender.ThreadID]);
      while activted do
        begin
          th_2_progress_tool.Progress();
          TCompute.Sleep(1);
        end;
    end, nil, nil, @isExit);

  // 往线程发送延迟1秒后的执行代码
  th_2_progress_tool.PostExecuteP_NP(1.0, procedure
    begin
      DoStatus('TN_Progress_Tool(1):执行线程ID:%d', [TCompute.CurrentThread.ThreadID]);
    end);
  th_2_progress_tool.PostExecuteP_NP(1.0, procedure
    begin
      DoStatus('TN_Progress_Tool(2):执行线程ID:%d', [TCompute.CurrentThread.ThreadID]);
    end);
  th_2_progress_tool.PostExecuteP_NP(1.0, procedure
    begin
      DoStatus('TN_Progress_Tool(3):执行线程ID:%d', [TCompute.CurrentThread.ThreadID]);
    end);

  TK := GetTimeTick;

  while GetTimeTick - TK < 2000 do
      CheckThread(1);
  activted := False;

  while not isExit do
      TCompute.Sleep(1);

  DisposeObjectAndNil(th_2_progress_tool);
end;

begin
  do_Main_Demo_TThreadPost();
  do_Main_Demo_Progress_Post();

end.
