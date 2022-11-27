unit _129_ZDB2_Thread_App_DemoFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Cipher, Z.Status, Z.Notify, Z.MemoryStream,
  Z.ListEngine,
  Z.ZDB2, Z.ZDB2.Thread.Queue, Z.ZDB2.Thread;

type
  TCustom_Data = class(TZDB2_Th_Engine_Data)
  public
    MD5: TMD5;
    Size: NativeInt;
    constructor Create; override;
    destructor Destroy; override;
  end;

  T_129_ZDB2_Thread_App_DemoForm = class(TForm)
    Memo: TMemo;
    runButton: TButton;
    fpsTimer: TTimer;
    Data_Num_Label: TLabel;
    Remove_Num_Label: TLabel;
    Add_Num_Label: TLabel;
    Parallel_Load_Num_Label: TLabel;
    For_Num_Label: TLabel;
    Parallel_Load_CheckBox: TCheckBox;
    For_CheckBox: TCheckBox;
    Progress_CheckBox: TCheckBox;
    Progress_Num_Label: TLabel;
    Queue_Num_Label: TLabel;
    procedure fpsTimerTimer(Sender: TObject);
    procedure runButtonClick(Sender: TObject);
  private
    procedure backcall_DoStatus(Text_: SystemString; const ID: Integer); // 状态log
    procedure Do_Init_Sim_Mem_DB;                                        // 初始化db
    procedure Do_Sim_Add;                                                // 多线程追加数据模拟
    procedure Do_Sim_Remove;                                             // 多线程删除数据模拟
    procedure Do_Sim_Parallel_Load;                                      // 多线程物理遍历模拟
    procedure Do_Sim_For;                                                // 多线程实例遍历模拟
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  _129_ZDB2_Thread_App_DemoForm: T_129_ZDB2_Thread_App_DemoForm;
  Sim_Mem_DB: TZDB2_Th_Engine_Marshal;
  Task_Num: Integer;
  Add_Num, Remove_Num, Parallel_Load_Num, For_Num, Progress_Num: Int64;
  Sim_Running: Boolean;

implementation

{$R *.dfm}


constructor TCustom_Data.Create;
begin
  inherited Create;
  MD5 := Null_MD5;
  Size := 0;
end;

destructor TCustom_Data.Destroy;
begin
  try
      inherited Destroy;
  except
  end;
end;

procedure T_129_ZDB2_Thread_App_DemoForm.fpsTimerTimer(Sender: TObject);
var
  n: U_String;
begin
  CheckThread;
  Data_Num_Label.Caption := Format('仿真任务:%d 数据链回收器:%d 数据实例回收器:%d 数据条目总数:%d',
    [Task_Num, Sim_Mem_DB.Data_Link_Recycle_Tool.Num, Sim_Mem_DB.Instance_Recycle_Tool.Num, Sim_Mem_DB.Total]);
  Add_Num_Label.Caption := Format('添加数据次数:%d', [Add_Num]);
  Remove_Num_Label.Caption := Format('删除数据次数:%d', [Remove_Num]);
  Parallel_Load_Num_Label.Caption := Format('物理遍历次数:%d', [Parallel_Load_Num]);
  For_Num_Label.Caption := Format('实例遍历次数:%d', [For_Num]);
  Progress_Num_Label.Caption := Format('主循环次数:%d', [Progress_Num]);

  n := '';
  if (Task_Num > 0) and (Sim_Mem_DB.Engine_Pool.Num > 0) then
    with Sim_Mem_DB.Engine_Pool.Repeat_ do
      repeat
        if Queue^.Data.Engine <> nil then
            n.Append('数据库(%d) IO读写队列:%d 存储量:%s/%s' + #13#10,
            [I__ + 1,
              Queue^.Data.Engine.QueueNum,
              umlSizeToStr(Queue^.Data.Engine.CoreSpace_Size).Text,
              umlSizeToStr(Queue^.Data.Engine.CoreSpace_Physics_Size).Text]);
      until not Next;
  Queue_Num_Label.Caption := n;

  if Progress_CheckBox.Checked then
    begin
      if Sim_Mem_DB.Progress then
          AtomInc(Progress_Num);
    end;
end;

procedure T_129_ZDB2_Thread_App_DemoForm.runButtonClick(Sender: TObject);
begin
  if Sim_Running then
    begin
      Sim_Running := False;
      runButton.Enabled := False;
      TCompute.RunP_NP(procedure
        begin
          while Task_Num > 0 do
              TCompute.Sleep(100);
          TCompute.Sync(procedure
            begin
              Sim_Mem_DB.Check_Recycle_Pool;
              Sim_Mem_DB.Engine_Pool.Clear;
              Sim_Mem_DB.Check_Recycle_Pool;
              runButton.Enabled := True;
            end);
          Add_Num := 0;
          Remove_Num := 0;
          Parallel_Load_Num := 0;
          For_Num := 0;
          Progress_Num := 0;
        end);
    end
  else
    begin
      TCompute.RunP_NP(procedure
        var
          i: Integer;
        begin
          Task_Num := 0;
          Add_Num := 0;
          Remove_Num := 0;
          Parallel_Load_Num := 0;
          For_Num := 0;
          Progress_Num := 0;

          Sim_Mem_DB.Engine_Pool.Clear;
          Do_Init_Sim_Mem_DB;
          Sim_Running := True;
          for i := 1 to 10 do
            begin
              TCompute.RunP_NP(procedure
                var
                  r: Integer;
                begin
                  AtomInc(Task_Num);
                  while Sim_Running do
                    begin
                      r := TMT19937.Rand32;
                      case r mod 1000 of
                        0 .. 890: Do_Sim_Add;
                        891 .. 900: Do_Sim_Remove;
                        990 .. 998: Do_Sim_For;
                        999:
                          begin
                            if Parallel_Load_CheckBox.Checked then
                                Do_Sim_Parallel_Load
                            else
                                Do_Sim_For;
                          end;
                      end;
                    end;
                  AtomDec(Task_Num);
                end);
            end;
        end);
    end;
end;

procedure T_129_ZDB2_Thread_App_DemoForm.backcall_DoStatus(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

procedure T_129_ZDB2_Thread_App_DemoForm.Do_Init_Sim_Mem_DB;
var
  i: Integer;
  Eng_: TZDB2_Th_Engine;
begin
  for i := 1 to 5 do
    begin
      Eng_ := TZDB2_Th_Engine.Create(Sim_Mem_DB);
      Eng_.Mode := smBigData;
      Eng_.BlockSize := 1536;
      Eng_.Database_File := '';
      Eng_.OnlyRead := False;
      Eng_.Cipher_Security := TCipherSecurity.csNone;
    end;
  Sim_Mem_DB.Build(TCustom_Data);
end;

procedure T_129_ZDB2_Thread_App_DemoForm.Do_Sim_Add;
var
  m64: TMem64;
  inst: TCustom_Data;
begin
  m64 := TMem64.Create;
  m64.Size := umlRandomRange(1536, 16 * 1024);
  TMT19937.Rand32(MaxInt, m64.Memory, m64.Size div 4);
  inst := Sim_Mem_DB.Add_Data_To_Minimize_Size_Engine as TCustom_Data;
  inst.Lock;
  inst.MD5 := m64.ToMD5;
  inst.Size := m64.Size;
  inst.UnLock;
  inst.Async_Save_And_Free_Data(m64);
  AtomInc(Add_Num);
end;

procedure T_129_ZDB2_Thread_App_DemoForm.Do_Sim_Remove;
begin
  TCompute.Sleep(100);
  Sim_Mem_DB.For_P(True, 8, procedure(Sender: TZDB2_Th_Engine_Data; Index: Int64; var Aborted: Boolean)
    var
      inst: TCustom_Data;
    begin
      inst := Sender as TCustom_Data;
      if (index > 0) and (index mod 2 = 0) then
        begin
          Sender.Remove;
          AtomInc(Remove_Num);
        end;
    end);
end;

procedure T_129_ZDB2_Thread_App_DemoForm.Do_Sim_Parallel_Load;
begin
  Sim_Mem_DB.Parallel_Load_P(1,
    procedure(Sender: TZDB2_Th_Engine_Data; IO_: TMS64)
    var
      inst: TCustom_Data;
    begin
      inst := Sender as TCustom_Data;
      if not umlCompareMD5(inst.MD5, IO_.ToMD5) then
          DoStatus('md5 error.');
    end, nil);
  AtomInc(Parallel_Load_Num);
end;

procedure T_129_ZDB2_Thread_App_DemoForm.Do_Sim_For;
begin
  if not For_CheckBox.Checked then
      exit;
  Sim_Mem_DB.For_P(True, 8, procedure(Sender: TZDB2_Th_Engine_Data; Index: Int64; var Aborted: Boolean)
    var
      inst: TCustom_Data;
    begin
      inst := Sender as TCustom_Data;
    end);
  AtomInc(For_Num);
end;

constructor T_129_ZDB2_Thread_App_DemoForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AddDoStatusHook(self, backcall_DoStatus);
  Sim_Mem_DB := TZDB2_Th_Engine_Marshal.Create;
  Sim_Mem_DB.Current_Data_Class := TCustom_Data;
  Task_Num := 0;
  Add_Num := 0;
  Remove_Num := 0;
  Parallel_Load_Num := 0;
  For_Num := 0;
  Progress_Num := 0;
end;

destructor T_129_ZDB2_Thread_App_DemoForm.Destroy;
begin
  DisposeObjectAndNil(Sim_Mem_DB);
  RemoveDoStatusHook(self);
  inherited Destroy;
end;

end.
