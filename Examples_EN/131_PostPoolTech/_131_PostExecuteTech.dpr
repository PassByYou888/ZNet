program _131_PostExecuteTech;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.Status,
  Z.Notify;

{ PostExecute is a container method that directs the throwing of execution events. The execution code can be thrown to any thread, thereby achieving functions such as task scheduling and data scheduling
  This demo demonstrates two different execution mechanisms for serialization directed sending code to threads }

procedure do_Main_Demo_TThreadPost();
var
  activted: Boolean;
  { TThreadPost: Under the premise of ensuring thread safety, this is a method that can safely send code to the target thread and execute it at high speed according to send serialization, supporting d/fpc }
  { The dnn thread technology in AI uses TThreadPost }
  { The command queue technology in the Z.ZDB2.Thread.Queue library uses TThreadPost }
  th_1_post_tool: TThreadPost;
  TK: TTimeTick;
  isExit: Boolean; { The execution termination state machine of TCompute thread, which is a thread safe atomic variable }
begin
  activted := True;
  th_1_post_tool := TThreadPost.Create(0);
  TCompute.RunP(nil, nil, procedure(ThSender: TCompute)
    begin
      DoStatus('TThreadPost: Current driver thread ID:%d', [ThSender.ThreadID]);
      th_1_post_tool.ThreadID := ThSender.ThreadID;
      while activted do
        begin
          th_1_post_tool.Progress();
          TCompute.Sleep(1);
        end;
    end, nil, nil, @isExit);

  { Send execution code to the thread }
  th_1_post_tool.PostP1(procedure
    begin
      DoStatus('TThreadPost (1): Execution thread ID:%d', [TCompute.CurrentThread.ThreadID]);
    end);
  th_1_post_tool.PostP1(procedure
    begin
      DoStatus('TThreadPost (2): Execution thread ID:%d', [TCompute.CurrentThread.ThreadID]);
    end);
  th_1_post_tool.PostP1(procedure
    begin
      DoStatus('TThreadPost (3): Execution thread ID:%d', [TCompute.CurrentThread.ThreadID]);
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
  { TN_Progress_Tool: Supports advanced physical time converters and serialized execution of data structures while ensuring thread safety, and supports d/fpc }
  { TN in ZNet_Progress_Tool is widely used as a delayed execution mechanism }
  th_2_progress_tool: TCadencer_N_Progress_Tool;
  TK: TTimeTick;
  isExit: Boolean; { The execution termination state machine of TCompute thread, which is a thread safe atomic variable }
begin
  activted := True;
  th_2_progress_tool := TCadencer_N_Progress_Tool.Create;
  TCompute.RunP(nil, nil, procedure(ThSender: TCompute)
    begin
      DoStatus('TN_Progress_Tool: Current driver thread ID:%d', [ThSender.ThreadID]);
      while activted do
        begin
          th_2_progress_tool.Progress();
          TCompute.Sleep(1);
        end;
    end, nil, nil, @isExit);

  { Send execution code with a delay of 1 second to the thread }
  th_2_progress_tool.PostExecuteP_NP(1.0, procedure
    begin
      DoStatus('TN_Progress_Tool (1): Execution thread ID:%d', [TCompute.CurrentThread.ThreadID]);
    end);
  th_2_progress_tool.PostExecuteP_NP(1.0, procedure
    begin
      DoStatus('TN_Progress_Tool (2): Execution thread ID:%d', [TCompute.CurrentThread.ThreadID]);
    end);
  th_2_progress_tool.PostExecuteP_NP(1.0, procedure
    begin
      DoStatus('TN_Progress_Tool (3): Execution thread ID:%d', [TCompute.CurrentThread.ThreadID]);
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
