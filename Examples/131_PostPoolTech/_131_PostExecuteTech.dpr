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
  PostExecute��һ�ֶ����׳�ִ���¼�����������,ִ�д�������׸������߳�,�Ӷ�ʵ���������,���ݵ������๦��
  ��demo��ʾ�����ֲ�ͬ�����л������ʹ�����̵߳�ִ�л���
}

procedure do_Main_Demo_TThreadPost();
var
  activted: Boolean;
  // TThreadPost:�ڱ�֤�̰߳�ȫǰ����,����һ�ֿ��Խ����밲ȫ������Ŀ���߳��а��������л�����ִ��,֧��d/fpc
  // ��AI�е�dnn-thread����ʹ����TThreadPost
  // ��Z.ZDB2.Thread.Queue����������м���ʹ����TThreadPost
  th_1_post_tool: TThreadPost;
  TK: TTimeTick;
  isExit: Boolean; // TCompute�̵߳�ִ����ֹ״̬��,�����̰߳�ȫ��ԭ�ӱ���
begin
  activted := True;
  th_1_post_tool := TThreadPost.Create(0);
  TCompute.RunP(nil, nil, procedure(ThSender: TCompute)
    begin
      DoStatus('TThreadPost:��ǰ�����߳�ID:%d', [ThSender.ThreadID]);
      th_1_post_tool.ThreadID := ThSender.ThreadID;
      while activted do
        begin
          th_1_post_tool.Progress();
          TCompute.Sleep(1);
        end;
    end, nil, nil, @isExit);

  // ���̷߳���ִ�д���
  th_1_post_tool.PostP1(procedure
    begin
      DoStatus('TThreadPost(1):ִ���߳�ID:%d', [TCompute.CurrentThread.ThreadID]);
    end);
  th_1_post_tool.PostP1(procedure
    begin
      DoStatus('TThreadPost(2):ִ���߳�ID:%d', [TCompute.CurrentThread.ThreadID]);
    end);
  th_1_post_tool.PostP1(procedure
    begin
      DoStatus('TThreadPost(3):ִ���߳�ID:%d', [TCompute.CurrentThread.ThreadID]);
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
  // TN_Progress_Tool:��֤�̰߳�ȫǰ����,֧�ָ߼�����ʱ��ת���������ݽṹ�����л�ִ��,֧��d/fpc
  // ��ZNet��TN_Progress_Tool��Ϊ�ӳ�ִ�л��Ʊ�����ʹ��
  th_2_progress_tool: TCadencer_N_Progress_Tool;
  TK: TTimeTick;
  isExit: Boolean; // TCompute�̵߳�ִ����ֹ״̬��,�����̰߳�ȫ��ԭ�ӱ���
begin
  activted := True;
  th_2_progress_tool := TCadencer_N_Progress_Tool.Create;
  TCompute.RunP(nil, nil, procedure(ThSender: TCompute)
    begin
      DoStatus('TN_Progress_Tool:��ǰ�����߳�ID:%d', [ThSender.ThreadID]);
      while activted do
        begin
          th_2_progress_tool.Progress();
          TCompute.Sleep(1);
        end;
    end, nil, nil, @isExit);

  // ���̷߳����ӳ�1����ִ�д���
  th_2_progress_tool.PostExecuteP_NP(1.0, procedure
    begin
      DoStatus('TN_Progress_Tool(1):ִ���߳�ID:%d', [TCompute.CurrentThread.ThreadID]);
    end);
  th_2_progress_tool.PostExecuteP_NP(1.0, procedure
    begin
      DoStatus('TN_Progress_Tool(2):ִ���߳�ID:%d', [TCompute.CurrentThread.ThreadID]);
    end);
  th_2_progress_tool.PostExecuteP_NP(1.0, procedure
    begin
      DoStatus('TN_Progress_Tool(3):ִ���߳�ID:%d', [TCompute.CurrentThread.ThreadID]);
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
