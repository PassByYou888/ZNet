program _159_SequenceExpression;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Variants,
  Z.Core,
  Z.PascalStrings,
  Z.UPascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.Parsing,
  Z.Expression,
  Z.Expression.Sequence,
  Z.OpCode;

type
  // 表达式序列化是非线性流程
  // 表达式序列化可以用于配置和脚本,起到流程被脚本控制的作用
  // 表达式序列化简单直接,不绕弯子,想写类似make脚本,直接上就行了,如果需要if/for/var这类机制自己加
  TMy_RunTime = class(TExpression_Sequence_RunTime)
  public
    function Do_Download(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
    function Do_Process(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
    procedure Reg_RunTime; override;
  end;

  // 仿真下载api,5秒后下载完成,通过Do_End_And_Result返回结果
function TMy_RunTime.Do_Download(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
var
  f: string;
begin
  Do_Begin; // 非线性流程启动时要调用Do_Begin,否则会被视作线性流程
  Result := False;
  f := VarToStr(OP_Param[1]);
  DoStatus('%s 下载中,保存到 %s', [VarToStr(OP_Param[0]), f]);
  Owner.N_Progress.PostExecuteP_NP(5.0, procedure
    begin
      DoStatus('下载完成,目标文件 %s', [f]);
      Do_End_And_Result(True); // 非线性流程结束时要调用Do_End
    end);
end;

// 仿真处理api,开个线程,处理完成后,触发Do_End
function TMy_RunTime.Do_Process(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
begin
  Do_Begin; // 非线性流程启动时要调用Do_Begin,否则会被视作线性流程
  DoStatus('开始处理文件 %s,5秒后完成处理', [VarToStr(OP_Param[0])]);
  Result := True;
  TCompute.RunP_NP(procedure
    begin
      TCompute.Sleep(5000);
      Do_End; // 非线性流程结束时要调用Do_End
    end);
end;

procedure TMy_RunTime.Reg_RunTime;
begin
  RegObjectOpM('Download', 'Download(url), download url.', Do_Download).Category := 'Demo';
  RegObjectOpM('Process', 'Process(url), process file.', Do_Process).Category := 'Demo';
end;

const
  code =
    'Download("https://url.com/abc.jpg", "c:\\temp\\abc.jpg")' + #13#10 +
    'process("c:\\temp\\abc.jpg"),print("所有序列化脚本运行结束")';

begin
  with TExpression_Sequence.Create do
    begin
      Sequence_Class := TMy_RunTime;
      Extract_Code(tsC, code); // 这里是两个非线性流程,分别是下载,和处理,它们都是仿真的
      Run;
      Wait;
      Free;
    end;
  DoStatus('回车键退出');
  readln;

end.
