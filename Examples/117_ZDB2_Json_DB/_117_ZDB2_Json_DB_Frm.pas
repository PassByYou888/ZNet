unit _117_ZDB2_Json_DB_Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ComCtrls,

  Z.Core, Z.PascalStrings, Z.MemoryStream, Z.UnicodeMixedLib, Z.Cipher, Z.Status,
  Z.Expression, Z.OpCode, Z.Parsing, Z.IOThread,
  Z.Json, Z.ZDB2, Z.ZDB2.Json;

type
  TTZDB2_Json_DB_Frm = class(TForm)
    fpsTimer: TTimer;
    MakeJsonButton: TButton;
    Memo: TMemo;
    MakeInfoLabel: TLabel;
    newbieQueryButton: TButton;
    expertQueryButton: TButton;
    thInfoLabel: TLabel;
    removeButton: TButton;
    ProgressBar: TProgressBar;
    modifyButton: TButton;
    extractSpaceButton: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure fpsTimerTimer(Sender: TObject);
    procedure MakeJsonButtonClick(Sender: TObject);
    procedure newbieQueryButtonClick(Sender: TObject);
    procedure expertQueryButtonClick(Sender: TObject);
    procedure removeButtonClick(Sender: TObject);
    procedure modifyButtonClick(Sender: TObject);
    procedure extractSpaceButtonClick(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
    Cipher_: TZDB2_Cipher;
    JDB: TZDB2_List_Json;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  // 使用IO线程池将数据批量输入，接下来，就让IO线程池负责条件分析
  // IO线程池的TExpert_Query_IO_Data不是链表，数据队列在x64可以上百亿份，不影响系统性能
  TExpert_Query_IO_Data = class(TIO_Thread_Data)
  public
    js_index: Integer;
    jsData: TMem64;
    procedure Process; override; // Process很适合做运算操作，以及繁琐的处理流程都适合这里干
  end;

var
  TZDB2_Json_DB_Frm: TTZDB2_Json_DB_Frm;

implementation

{$R *.dfm}


procedure TExpert_Query_IO_Data.Process;
var
  js: TZJ;
  i: Integer;
  arry: TZJArry;
  n: U_String;
begin
  js := TZJ.Create;
  js.LoadFromStream(jsData.Stream64); // 解码json
  arry := js.A['exp_math_arry'];
  for i := 0 to arry.Count - 1 do
    begin
      n := arry.S[i];
      // umlReplaceSum是计算统计，相当于find字符串，以word方式搜索99，至少出现4次
      if umlReplaceSum(@n, '99', true, true, 0, 0, nil) > 3 then
          DoStatus('found "%s" from index: %d expression evaluate: %s', [n.Text, js_index, VarToStr(EvaluateExpressionValue(False, n))]);
    end;
  DisposeObject(js);
end;

procedure TTZDB2_Json_DB_Frm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DeleteDoStatusHook(self);
end;

procedure TTZDB2_Json_DB_Frm.fpsTimerTimer(Sender: TObject);
begin
  thInfoLabel.Caption := TCompute.State;
  with JDB.CoreSpace.State^ do
      MakeInfoLabel.Caption := PFormat('数据条目: %d 物理空间（自动扩容） %s 自由空间 %s',
      [JDB.Count, umlSizeToStr(Physics).Text, umlSizeToStr(FreeSpace).Text]);

  CheckThread(1);
  JDB.Progress;
end;

procedure TTZDB2_Json_DB_Frm.MakeJsonButtonClick(Sender: TObject);
var
  i, j: NativeInt;
  js: TZDB2_Json;
  tk: TTimeTick;
begin
  tk := GetTimeTick;
  for i := 1 to 10 * 1024 do
    begin
      js := JDB.NewData;
      for j := 1 to 500 do
          js.Data.A['exp_math_arry'].Add(Format('%d + tan(%d) * arctan(%d) + %d + %d - %d - %d - %d',
          [umlRandomRange(-99, 99),
          umlRandomRange(-99, 99),
          umlRandomRange(-99, 99),
          umlRandomRange(-99, 99),
          umlRandomRange(-99, 99),
          umlRandomRange(-99, 99),
          umlRandomRange(-99, 99),
          umlRandomRange(-99, 99)]));
      js.Save;
      if GetTimeTick - tk > 100 then
        begin
          Application.ProcessMessages;
          tk := GetTimeTick;
        end;
    end;
end;

procedure TTZDB2_Json_DB_Frm.newbieQueryButtonClick(Sender: TObject);
var
  j: Integer;
  arry: TZJArry;
  n: U_String;
  tk: TTimeTick;
begin
  if JDB.Count <= 0 then
      exit;
  ProgressBar.Max := JDB.Count - 1;
  tk := GetTimeTick;

  if JDB.Count > 0 then
    with JDB.Repeat_ do
      repeat
        arry := Queue^.Data.Data.A['exp_math_arry'];
        for j := 0 to arry.Count - 1 do
          begin
            n := arry.S[j];
            // umlReplaceSum是计算统计，相当于find字符串，以word方式搜索99，至少出现4次
            if umlReplaceSum(@n, '99', true, true, 0, 0, nil) > 3 then
              begin
                DoStatus('found "%s" from index: %d expression evaluate: %s', [n.Text, I__, VarToStr(EvaluateExpressionValue(False, n))]);
              end;
          end;
        Queue^.Data.RecycleMemory;
        if GetTimeTick - tk > 1000 then
          begin
            Application.ProcessMessages;
            tk := GetTimeTick;
          end;
        ProgressBar.Position := I__;

      until not Next;

  DoStatus('query done.');
end;

procedure TTZDB2_Json_DB_Frm.expertQueryButtonClick(Sender: TObject);
var
  Queue__: TIO_Thread;
  query_data: TExpert_Query_IO_Data;
  running: Boolean;
  tk: TTimeTick;
begin
  if JDB.Count <= 0 then
      exit;
  // flush会确保修改的内容被写入物理空间
  JDB.Flush(False);

  // 创建IO线程池，线程数并不是越多越快，计算量大才能给大
  Queue__ := TIO_Thread.Create(8);

  ProgressBar.Max := JDB.Count - 1;

  // 开个等查完的线程
  running := true;
  TCompute.RunP_NP(procedure
    var
      io_data: TExpert_Query_IO_Data;
    begin
      while running or (Queue__.Count > 0) do
        begin
          // IOThread核心机制：enqueue是123，dequeue也会是123
          io_data := TExpert_Query_IO_Data(Queue__.Dequeue);
          if io_data <> nil then
            begin
              ProgressBar.Position := io_data.js_index;
              DisposeObject(io_data.jsData);
              DisposeObject(io_data);
            end
          else
              TCompute.Sleep(1);
        end;
      DisposeObject(Queue__);
      DoStatus('expert query done.');
    end);

  tk := GetTimeTick;

  if JDB.Count > 0 then
    with JDB.Repeat_ do
      repeat
        query_data := TExpert_Query_IO_Data.Create;
        query_data.js_index := I__;
        query_data.jsData := TMem64.Create;
        JDB.CoreSpace.ReadData(query_data.jsData, Queue^.Data.ID); // 从物理空间读出实体数据，这一步不会解码json
        // 把query_data编入处理队列
        // TExpert_Query_IO_Data会在处理完以后，恢复keep
        // enqueue线程安全
        Queue__.Enqueue(query_data);

        // 限制输入队列，防止过分膨胀
        while (Queue__.Count > 1000) or (GetTimeTick - tk > 100) do
          begin
            Application.ProcessMessages;
            tk := GetTimeTick;
          end;
      until not Next;

  running := False;
end;

procedure TTZDB2_Json_DB_Frm.removeButtonClick(Sender: TObject);
var
  j: Integer;
  arry: TZJArry;
  n: U_String;
  tk: TTimeTick;
  found_: Integer;
begin
  if JDB.Count <= 0 then
      exit;

  ProgressBar.Max := JDB.Count - 1;
  tk := GetTimeTick;

  if JDB.Count > 0 then
    with JDB.Repeat_ do
      repeat
        arry := Queue^.Data.Data.A['exp_math_arry'];
        found_ := 0;
        for j := 0 to arry.Count - 1 do
          begin
            n := arry.S[j];
            // umlReplaceSum是计算统计，相当于find字符串，以word方式搜索99，至少出现4次
            if umlReplaceSum(@n, '99', true, true, 0, 0, nil) > 3 then
                inc(found_);
          end;

        if found_ > 0 then
            JDB.Push_To_Recycle_Pool(Queue^.Data, true);

        if GetTimeTick - tk > 1000 then
          begin
            Application.ProcessMessages;
            tk := GetTimeTick;
          end;
        ProgressBar.Position := I__;
      until not Next;

  JDB.Free_Recycle_Pool;
  DoStatus('remove done.');
end;

procedure TTZDB2_Json_DB_Frm.modifyButtonClick(Sender: TObject);
var
  j: Integer;
  arry: TZJArry;
  n: U_String;
  tk: TTimeTick;
begin
  if JDB.Count <= 0 then
      exit;
  // 修改json内容直接干就完了，然后用flush处理一下会重构结构
  ProgressBar.Max := JDB.Count - 1;
  tk := GetTimeTick;

  if JDB.Count > 0 then
    with JDB.Repeat_ do
      repeat
        arry := Queue^.Data.Data.A['exp_math_arry'];
        j := 0;
        while j < arry.Count do
          begin
            n := arry.S[j];
            if umlReplaceSum(@n, '99', true, true, 0, 0, nil) > 0 then
                arry.Delete(j) // 修改Json内容时，会自动按时间撮存储到物理空间
            else
                inc(j);
          end;

        if GetTimeTick - tk > 1000 then
          begin
            Application.ProcessMessages;
            tk := GetTimeTick;
          end;
        ProgressBar.Position := I__;
      until not Next;

  DoStatus('flush.');
  JDB.Flush(False); // flush会确保修改的内容被写入物理空间
  DoStatus('modify done.');
end;

procedure TTZDB2_Json_DB_Frm.extractSpaceButtonClick(Sender: TObject);
var
  new_jdb_stream: TStream;
begin
  if JDB.Count <= 0 then
      exit;
  // zdb2的物理空间在大量删除回收以后会出现碎片，这里演示了如何回收碎片
  new_jdb_stream := TMS64.CustomCreate(100 * 1024 * 1024);

  // 第一步 把当前数据重构到新物理空间
  // extractTo会在内核层直接重构物理空间结构，新结构没有碎片，并且数据库也会变小，等同于优化
  // 一般来说，如果数据是不用更改的，大可在发行前用extractTo来操作一次
  JDB.ExtractTo(new_jdb_stream);

  // 第二步 把当前jdb释放掉
  DisposeObject(JDB);

  // 第三步 重新创建jdb
  JDB := TZDB2_List_Json.Create(TZDB2_Json, nil, 500, new_jdb_stream, False, 100 * 1024 * 1024, 200, Cipher_);
  JDB.AutoFreeStream := true;
  JDB.CoreSpace.Mode := smBigData;
  JDB.CoreSpace.MaxCacheMemory := 128 * 1024 * 1024;
end;

procedure TTZDB2_Json_DB_Frm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  if Memo.Lines.Count > 5000 then
      Memo.Lines.Clear;
  Memo.Lines.Add(Text_);
end;

constructor TTZDB2_Json_DB_Frm.Create(AOwner: TComponent);
var
  jdb_stream: TStream;
begin
  inherited;
  AddDoStatusHook(self, DoStatus_backcall);
  // 数据库加密接口
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csTwoFish, '123456', 1, true, true);
  // zdb2的json数据引擎
  jdb_stream := TMS64.CustomCreate(100 * 1024 * 1024); // 如果用文件存，这里给TFilestream
  JDB := TZDB2_List_Json.Create(TZDB2_Json, nil, 500, jdb_stream, False, 100 * 1024 * 1024, 200, Cipher_);
  JDB.IOHnd.Cache.UsedWriteCache := true;
  JDB.IOHnd.Cache.UsedReadCache := true;
  JDB.AutoFreeStream := true;
  // IO cache 模型，给smBigData以后不会cache读写，一般只要是内存stream，都不用开cache
  JDB.CoreSpace.Mode := smBigData;
  JDB.CoreSpace.MaxCacheMemory := 128 * 1024 * 1024; // 如果IO是文件，缓存可以给大，默认16M
end;

destructor TTZDB2_Json_DB_Frm.Destroy;
begin
  DisposeObject(JDB);
  DisposeObject(Cipher_);
  inherited;
end;

end.
