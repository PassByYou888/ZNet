unit _163_NonLinearExpressionStack_Demo_Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Parsing, Z.Expression, Z.OpCode, Z.Notify, Z.Status;

type
  T_163_NonLinearExpressionStack_Demo_Form = class(TForm)
    sysTimer: TTimer;
    Memo: TMemo;
    expEdit: TLabeledEdit;
    runButton: TButton;
    runInThreadButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sysTimerTimer(Sender: TObject);
    procedure runButtonClick(Sender: TObject);
    procedure runInThreadButtonClick(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
  end;

var
  _163_NonLinearExpressionStack_Demo_Form: T_163_NonLinearExpressionStack_Demo_Form;
  RT: TOpCustomRunTime;

implementation

{$R *.dfm}


procedure T_163_NonLinearExpressionStack_Demo_Form.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);
  RT := TOpCustomRunTime.Create;
  RT.Reg_Code_OpP('小明', '', function(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; OP_Code: TOpCode; var OP_Param: TOpParam): Variant
    begin
      Result := '';
      OP_Code.NonLinear.Do_Begin; // 启动非线性流程
      DoStatus('2秒后执行小明输入');
      SystemPostProgress.PostExecuteP_NP(2, procedure
        begin
          DoStatus('等待输入');
          OP_Code.NonLinear.Do_End( // 结束非线性流程
            InputBox('输入小明名字', '小明叫:', '王小明'));
        end);
    end);
  RT.Reg_Code_OpP('小明爸爸', '', function(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; OP_Code: TOpCode; var OP_Param: TOpParam): Variant
    begin
      Result := '';
      OP_Code.NonLinear.Do_Begin; // 启动非线性流程
      DoStatus('3秒后执行小明爸爸输入');
      SystemPostProgress.PostExecuteP_NP(3, procedure
        begin
          DoStatus('等待输入');
          OP_Code.NonLinear.Do_End( // 结束非线性流程
            InputBox('输入小明爸爸名字', '小明爸爸叫:', '老王'));
        end);
    end);
  RT.Reg_Code_OpP('小明妈妈', '', function(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; OP_Code: TOpCode; var OP_Param: TOpParam): Variant
    begin
      Result := '';
      OP_Code.NonLinear.Do_Begin; // 启动非线性流程
      DoStatus('4.5秒后执行小明妈妈输入');
      SystemPostProgress.PostExecuteP_NP(4.5, procedure
        begin
          DoStatus('等待输入');
          OP_Code.NonLinear.Do_End( // 结束非线性流程
            InputBox('输入小明妈妈名字', '小明妈妈叫:', '翠花'));
        end);
    end);
end;

procedure T_163_NonLinearExpressionStack_Demo_Form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RemoveDoStatusHook(self);
end;

procedure T_163_NonLinearExpressionStack_Demo_Form.sysTimerTimer(Sender: TObject);
begin
  CheckThread;
end;

procedure T_163_NonLinearExpressionStack_Demo_Form.runButtonClick(Sender: TObject);
begin
  System_NonLinear_Pool.Post_Execute_P(tsPascal, expEdit.Text, RT, procedure(Sender: TOpCode_NonLinear)
    begin
      DoStatus('执行结果:' + VarToStr(Sender.Result_));
    end);
end;

procedure T_163_NonLinearExpressionStack_Demo_Form.runInThreadButtonClick(Sender: TObject);
begin
  TCompute.RunP_NP(procedure
    var
      pool: TOpCode_NonLinear_Pool;
    begin
      pool := TOpCode_NonLinear_Pool.Create;
      pool.Post_Execute_P(tsPascal, expEdit.Text, RT, procedure(Sender: TOpCode_NonLinear)
        begin
          DoStatus('执行结果:' + VarToStr(Sender.Result_));
        end);

      // 模拟主循环
      while pool.Num + pool.Post___.Num > 0 do
          pool.Process;
      disposeObject(pool);
    end);
end;

procedure T_163_NonLinearExpressionStack_Demo_Form.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

end.
