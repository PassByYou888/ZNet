unit _162_SequenceExpression_APP_Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.ListEngine, Z.Geometry2D, Z.Notify,
  Z.MemoryStream, Z.DFE, Z.TextDataEngine, Z.Status,
  Z.Expression, Z.OpCode, Z.Parsing, Z.Expression.Sequence,
  Z.Net, Z.Net.PhysicsIO, Z.Net.C4;

type
  TOptions_Script = class;
  TProcess_Script = class;

  // 参数区域用于初始化事件参数
  // 运行区域在事件触发时运行,条件->动作
  TSubscribe_Script_RunTime = class(TExpression_Sequence_RunTime)
  public // 参数区域
    function op_Host(var OP_Param: TOpParam): Variant; // 全局c4地址
    function op_Port(var OP_Param: TOpParam): Variant; // 全局c4端口
    function op_Build_C4_NetWork(var OP_Param: TOpParam): Variant; // 构建C4网络
  public // 条件区域,以非线性流程做数据化计算,所有条件必须成立

    // 序列化脚本的代码从6代监控的subscribe模块拔出,这里没有条件函数,需要自己照范例实现
    // 例如,业务流程判断1,判断2...

  public // 动作区域

    // 序列化脚本的代码从6代监控的subscribe模块拔出,这里没有动作函数,需要自己照范例实现
    // 例如,http推送,运行shell exe,调用某个api...

  public // 公共区域
    function op_Delay(var OP_Param: TOpParam): Variant; // 延迟操作,非线性流程的延迟会让后面的命令处于等待状态,并且在延迟期间不会触发条件区域和动作区域
  public
    Options_Script: TOptions_Script;
    Process_Script: TProcess_Script;
    constructor Create(Owner_: TExpression_Sequence); override;
    destructor Destroy; override;
  end;

  TOptions_Script = class
  private
    procedure Do_Expression_Sequence_Create_RunTime(Sender: TExpression_Sequence; NewRunTime: TExpression_Sequence_RunTime);
    procedure Do_Expression_Sequence_Step(Sender: TExpression_Sequence; Current_Step: TExpression_Sequence_RunTime);
    procedure Do_Expression_Sequence_Done(Sender: TExpression_Sequence);
  public
    // 非线性脚本实例
    Inst_Options: TExpression_Sequence;
    // 非线性脚本
    Script_Options: TPascalStringList;
    // 启动参数
    C4Host, C4Port: U_String; // 数据中心服务器
    constructor Create;
    destructor Destroy; override;
    function Prepare(Style: TTextStyle; Code_: TCore_Strings): Boolean;
    function IsReady: Boolean;
    function Get_Run_Info: SystemString;
    procedure Run;
    procedure Progress;
  end;

  TProcess_Script = class
  private
    procedure Do_Expression_Sequence_Create_RunTime(Sender: TExpression_Sequence; NewRunTime: TExpression_Sequence_RunTime);
    procedure Do_Expression_Sequence_Step(Sender: TExpression_Sequence; Current_Step: TExpression_Sequence_RunTime);
    procedure Do_Expression_Sequence_Done(Sender: TExpression_Sequence);
  private
    // 计算用
    FIs_Running: Boolean;
    procedure Do_Run;
    procedure Do_Done;
  public
    Options_Script: TOptions_Script;
    // 非线性脚本实例
    Inst_Condition, Inst_Action: TExpression_Sequence;
    // 非线性脚本
    Script_Condition, Script_Action: TPascalStringList;
    constructor Create(Opt_: TOptions_Script);
    destructor Destroy; override;
    function Prepare(Style: TTextStyle; Condition_Code_, Action_Code_: TCore_Strings): Boolean;
    function IsReady: Boolean;
    function Get_Condition_Run_Info: SystemString;
    function Get_Action_Run_Info: SystemString;
    procedure Run;
    procedure Progress;
    property Is_Running: Boolean read FIs_Running;
  end;

  T_162_SequenceExpression_APP_Form = class(TForm)
    Readme_Memo: TMemo;
    SysTimer: TTimer;
    Opt_Memo: TMemo;
    Opt_Info_Label: TLabel;
    Process_Condtion_Memo: TMemo;
    Process_Condtion_Info_Label: TLabel;
    Log_Memo: TMemo;
    Log_Info_Label: TLabel;
    run_Button: TButton;
    Process_Action_Memo: TMemo;
    Process_Action_Info_Label: TLabel;
    procedure run_ButtonClick(Sender: TObject);
    procedure SysTimerTimer(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
    Options_Script: TOptions_Script;
    Process_Script: TProcess_Script;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run_Script;
  end;

var
  _162_SequenceExpression_APP_Form: T_162_SequenceExpression_APP_Form;

implementation

{$R *.dfm}


function TSubscribe_Script_RunTime.op_Host(var OP_Param: TOpParam): Variant;
begin
  Options_Script.C4Host := VarToStr(OP_Param[0]);
  Result := True;
end;

function TSubscribe_Script_RunTime.op_Port(var OP_Param: TOpParam): Variant;
begin
  Options_Script.C4Port := VarToStr(OP_Param[0]);
  Result := True;
end;

function TSubscribe_Script_RunTime.op_Build_C4_NetWork(var OP_Param: TOpParam): Variant;
begin
  Do_Begin;
  C40Clean_Client;
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Options_Script.C4Host, Options_Script.C4Port, 'NA', nil);
  TC40_Auto_Deployment_Client<TC40_Base_NoAuth_Client>.Create_P(procedure(var Sender: TC40_Base_NoAuth_Client)
    begin
      Do_End;
    end);
  Result := True;
end;

function TSubscribe_Script_RunTime.op_Delay(var OP_Param: TOpParam): Variant;
begin
  Do_Begin;
  Owner.Post_Progress.PostExecuteM_NP(OP_Param[0], Do_End);
  Result := True;
end;

constructor TSubscribe_Script_RunTime.Create(Owner_: TExpression_Sequence);
begin
  inherited Create(Owner_);
  Options_Script := nil;
  Process_Script := nil;

  // 系统参数区域
  RegOpM('Host', op_Host);
  RegOpM('Port', op_Port);
  RegOpM('Build_C4_NetWork', op_Build_C4_NetWork);
  // 通用函数区域
  RegOpM('Delay', op_Delay);
end;

destructor TSubscribe_Script_RunTime.Destroy;
begin
  inherited Destroy;
end;

procedure TOptions_Script.Do_Expression_Sequence_Create_RunTime(Sender: TExpression_Sequence; NewRunTime: TExpression_Sequence_RunTime);
begin
  TSubscribe_Script_RunTime(NewRunTime).Options_Script := self;
  TSubscribe_Script_RunTime(NewRunTime).Process_Script := nil;
end;

procedure TOptions_Script.Do_Expression_Sequence_Step(Sender: TExpression_Sequence; Current_Step: TExpression_Sequence_RunTime);
begin

end;

procedure TOptions_Script.Do_Expression_Sequence_Done(Sender: TExpression_Sequence);
begin

end;

constructor TOptions_Script.Create;
begin
  inherited Create;
  Inst_Options := TExpression_Sequence.Create;
  Inst_Options.Sequence_Class := TSubscribe_Script_RunTime;
  Inst_Options.On_Expression_Sequence_Create_RunTime := Do_Expression_Sequence_Create_RunTime;
  Inst_Options.On_Expression_Sequence_Step := Do_Expression_Sequence_Step;
  Inst_Options.On_Expression_Sequence_Done := Do_Expression_Sequence_Done;
  Script_Options := TPascalStringList.Create;
  // 启动参数
  C4Host := '127.0.0.1';
  C4Port := '8191';
end;

destructor TOptions_Script.Destroy;
begin
  DisposeObject(Inst_Options);
  DisposeObject(Script_Options);
  inherited Destroy;
end;

function TOptions_Script.Prepare(Style: TTextStyle; Code_: TCore_Strings): Boolean;
begin
  Inst_Options.Wait;
  Script_Options.Assign(Code_);
  Inst_Options.Clear;
  Inst_Options.Extract_Code(Style, Code_);
  Result := Inst_Options.Check_Syntax;
  if not Result then
    begin
      Inst_Options.Clear;
      Script_Options.Clear;
    end;
end;

function TOptions_Script.IsReady: Boolean;
begin
  Result := (Inst_Options.Num > 0) and (not Inst_Options.Is_Running);
end;

function TOptions_Script.Get_Run_Info: SystemString;
begin
  if Inst_Options.Is_Running then
    begin
      Result := PFormat('非线性 %s', [Inst_Options.Focus.Code_.Text]);
    end
  else
    begin
      if Inst_Options.RunNum <= 0 then
          Result := '挂起中'
      else
          Result := PFormat('已运行 %d 次, 目前在挂起状态', [Inst_Options.RunNum]);
    end;
end;

procedure TOptions_Script.Run;
begin
  Inst_Options.Run;
end;

procedure TOptions_Script.Progress;
begin
  Inst_Options.Progress;
end;

procedure TProcess_Script.Do_Expression_Sequence_Create_RunTime(Sender: TExpression_Sequence; NewRunTime: TExpression_Sequence_RunTime);
begin
  TSubscribe_Script_RunTime(NewRunTime).Options_Script := Options_Script;
  TSubscribe_Script_RunTime(NewRunTime).Process_Script := self;
end;

procedure TProcess_Script.Do_Expression_Sequence_Step(Sender: TExpression_Sequence; Current_Step: TExpression_Sequence_RunTime);
begin

end;

procedure TProcess_Script.Do_Expression_Sequence_Done(Sender: TExpression_Sequence);
var
  Condition_is_True: Boolean;
begin
  if Sender = Inst_Condition then
    begin
      // 检查条件返回状态
      // 如果返回全部为true,执行action
      Condition_is_True := True;
      if Inst_Condition.Num > 0 then
        with Inst_Condition.repeat_ do
          repeat
              Condition_is_True := Condition_is_True and Boolean(Queue^.Data.Code_Result);
          until not Next;

      if Condition_is_True then
        begin
          Inst_Action.Run;
        end
      else
        begin
          FIs_Running := False;
          Do_Done;
        end;
    end
  else if Sender = Inst_Action then
    begin
      FIs_Running := False;
      Do_Done;
    end;
end;

procedure TProcess_Script.Do_Run;
begin
end;

procedure TProcess_Script.Do_Done;
begin
end;

constructor TProcess_Script.Create(Opt_: TOptions_Script);
begin
  inherited Create;
  FIs_Running := False;
  Options_Script := Opt_;
  // 非线性脚本实例
  Inst_Condition := TExpression_Sequence.Create;
  Inst_Condition.Sequence_Class := TSubscribe_Script_RunTime;
  Inst_Condition.On_Expression_Sequence_Create_RunTime := Do_Expression_Sequence_Create_RunTime;
  Inst_Condition.On_Expression_Sequence_Step := Do_Expression_Sequence_Step;
  Inst_Condition.On_Expression_Sequence_Done := Do_Expression_Sequence_Done;
  Inst_Action := TExpression_Sequence.Create;
  Inst_Action.Sequence_Class := TSubscribe_Script_RunTime;
  Inst_Action.On_Expression_Sequence_Create_RunTime := Do_Expression_Sequence_Create_RunTime;
  Inst_Action.On_Expression_Sequence_Step := Do_Expression_Sequence_Step;
  Inst_Action.On_Expression_Sequence_Done := Do_Expression_Sequence_Done;
  // 非线性脚本
  Script_Condition := TPascalStringList.Create;
  Script_Action := TPascalStringList.Create;
end;

destructor TProcess_Script.Destroy;
begin
  DisposeObject(Inst_Condition);
  DisposeObject(Inst_Action);
  DisposeObject(Script_Condition);
  DisposeObject(Script_Action);
  inherited Destroy;
end;

function TProcess_Script.Prepare(Style: TTextStyle; Condition_Code_, Action_Code_: TCore_Strings): Boolean;
begin
  Inst_Condition.Wait;
  Inst_Action.Wait;

  Script_Condition.Assign(Condition_Code_);
  Script_Action.Assign(Action_Code_);

  Inst_Condition.Clear;
  Inst_Action.Clear;

  Inst_Condition.Extract_Code(Style, Condition_Code_);
  Inst_Action.Extract_Code(Style, Action_Code_);

  Result := Inst_Condition.Check_Syntax and Inst_Action.Check_Syntax;
  if not Result then
    begin
      Inst_Condition.Clear;
      Inst_Action.Clear;

      Script_Condition.Clear;
      Inst_Action.Clear;
    end;
end;

function TProcess_Script.IsReady: Boolean;
begin
  Result := (Inst_Condition.Num > 0) and (Inst_Action.Num > 0) and (not Inst_Condition.Is_Running) and (not Inst_Action.Is_Running) and (not FIs_Running);
end;

function TProcess_Script.Get_Condition_Run_Info: SystemString;
begin
  if Inst_Condition.Is_Running then
    begin
      Result := PFormat('已运行 %d 次,正在执行 %s', [Inst_Condition.RunNum, Inst_Condition.Focus.Code_.Text]);
    end
  else
    begin
      if Inst_Condition.RunNum <= 0 then
          Result := '挂起中'
      else
          Result := PFormat('已运行 %d 次, 目前在等待触发状态', [Inst_Condition.RunNum]);
    end;
end;

function TProcess_Script.Get_Action_Run_Info: SystemString;
begin
  if Inst_Action.Is_Running then
    begin
      Result := PFormat('已运行 %d 次,正在执行 %s', [Inst_Action.RunNum, Inst_Action.Focus.Code_.Text]);
    end
  else
    begin
      if Inst_Action.RunNum <= 0 then
          Result := '挂起中'
      else
          Result := PFormat('已运行 %d 次, 目前在等待触发状态', [Inst_Action.RunNum]);
    end;
end;

procedure TProcess_Script.Run;
begin
  FIs_Running := True;
  Do_Run;
  Inst_Condition.Run;
end;

procedure TProcess_Script.Progress;
begin
  if IsReady then
      Run;
  Inst_Condition.Progress;
  Inst_Action.Progress;
end;

procedure T_162_SequenceExpression_APP_Form.run_ButtonClick(Sender: TObject);
begin
  Run_Script;
end;

procedure T_162_SequenceExpression_APP_Form.SysTimerTimer(Sender: TObject);
begin
  C40Progress(0);
  Options_Script.Progress;
  Process_Script.Progress;

  if Options_Script.Inst_Options.Is_Running then
    begin
      Opt_Info_Label.Caption := Options_Script.Get_Run_Info;
    end;

  if Process_Script.Is_Running then
    begin
      Process_Condtion_Info_Label.Caption := Process_Script.Get_Condition_Run_Info;
      Process_Action_Info_Label.Caption := Process_Script.Get_Action_Run_Info;
    end;
end;

procedure T_162_SequenceExpression_APP_Form.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  if Log_Memo.Lines.Count > 1000 then
    begin
      Log_Memo.Lines.BeginUpdate;
      Log_Memo.Lines.Clear;
      Log_Memo.Lines.EndUpdate;
    end;
  Log_Memo.Lines.Add(Text_);
end;

constructor T_162_SequenceExpression_APP_Form.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options_Script := TOptions_Script.Create;
  Process_Script := TProcess_Script.Create(Options_Script);
  AddDoStatusHook(self, DoStatus_backcall);
  with Z.Net.C4.TC40_PhysicsService.Create('0.0.0.0', '', 8191, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('NA');
      StartService;
    end;
end;

destructor T_162_SequenceExpression_APP_Form.Destroy;
begin
  DisposeObject(Options_Script);
  DisposeObject(Process_Script);
  RemoveDoStatusHook(self);
  inherited Destroy;
end;

procedure T_162_SequenceExpression_APP_Form.Run_Script;
begin
  if Options_Script.Prepare(tsPascal, Opt_Memo.Lines) then
    begin
      Options_Script.Run;
      if Process_Script.Prepare(tsPascal, Process_Condtion_Memo.Lines, Process_Action_Memo.Lines) then
          Process_Script.Run;
    end;
end;

end.
