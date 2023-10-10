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

  // ʹ��IO�̳߳ؽ������������룬������������IO�̳߳ظ�����������
  // IO�̳߳ص�TExpert_Query_IO_Data�����������ݶ�����x64�����ϰ��ڷݣ���Ӱ��ϵͳ����
  TExpert_Query_IO_Data = class(TIO_Thread_Data)
  public
    js_index: Integer;
    jsData: TMem64;
    procedure Process; override; // Process���ʺ�������������Լ������Ĵ������̶��ʺ������
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
  js.LoadFromStream(jsData.Stream64); // ����json
  arry := js.A['exp_math_arry'];
  for i := 0 to arry.Count - 1 do
    begin
      n := arry.S[i];
      // umlReplaceSum�Ǽ���ͳ�ƣ��൱��find�ַ�������word��ʽ����99�����ٳ���4��
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
      MakeInfoLabel.Caption := PFormat('������Ŀ: %d ����ռ䣨�Զ����ݣ� %s ���ɿռ� %s',
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
            // umlReplaceSum�Ǽ���ͳ�ƣ��൱��find�ַ�������word��ʽ����99�����ٳ���4��
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
  // flush��ȷ���޸ĵ����ݱ�д������ռ�
  JDB.Flush(False);

  // ����IO�̳߳أ��߳���������Խ��Խ�죬����������ܸ���
  Queue__ := TIO_Thread.Create(8);

  ProgressBar.Max := JDB.Count - 1;

  // �����Ȳ�����߳�
  running := true;
  TCompute.RunP_NP(procedure
    var
      io_data: TExpert_Query_IO_Data;
    begin
      while running or (Queue__.Count > 0) do
        begin
          // IOThread���Ļ��ƣ�enqueue��123��dequeueҲ����123
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
        JDB.CoreSpace.ReadData(query_data.jsData, Queue^.Data.ID); // ������ռ����ʵ�����ݣ���һ���������json
        // ��query_data���봦�����
        // TExpert_Query_IO_Data���ڴ������Ժ󣬻ָ�keep
        // enqueue�̰߳�ȫ
        Queue__.Enqueue(query_data);

        // ����������У���ֹ��������
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
            // umlReplaceSum�Ǽ���ͳ�ƣ��൱��find�ַ�������word��ʽ����99�����ٳ���4��
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
  // �޸�json����ֱ�Ӹɾ����ˣ�Ȼ����flush����һ�»��ع��ṹ
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
                arry.Delete(j) // �޸�Json����ʱ�����Զ���ʱ���洢������ռ�
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
  JDB.Flush(False); // flush��ȷ���޸ĵ����ݱ�д������ռ�
  DoStatus('modify done.');
end;

procedure TTZDB2_Json_DB_Frm.extractSpaceButtonClick(Sender: TObject);
var
  new_jdb_stream: TStream;
begin
  if JDB.Count <= 0 then
      exit;
  // zdb2������ռ��ڴ���ɾ�������Ժ�������Ƭ��������ʾ����λ�����Ƭ
  new_jdb_stream := TMS64.CustomCreate(100 * 1024 * 1024);

  // ��һ�� �ѵ�ǰ�����ع���������ռ�
  // extractTo�����ں˲�ֱ���ع�����ռ�ṹ���½ṹû����Ƭ���������ݿ�Ҳ���С����ͬ���Ż�
  // һ����˵����������ǲ��ø��ĵģ�����ڷ���ǰ��extractTo������һ��
  JDB.ExtractTo(new_jdb_stream);

  // �ڶ��� �ѵ�ǰjdb�ͷŵ�
  DisposeObject(JDB);

  // ������ ���´���jdb
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
  // ���ݿ���ܽӿ�
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csTwoFish, '123456', 1, true, true);
  // zdb2��json��������
  jdb_stream := TMS64.CustomCreate(100 * 1024 * 1024); // ������ļ��棬�����TFilestream
  JDB := TZDB2_List_Json.Create(TZDB2_Json, nil, 500, jdb_stream, False, 100 * 1024 * 1024, 200, Cipher_);
  JDB.IOHnd.Cache.UsedWriteCache := true;
  JDB.IOHnd.Cache.UsedReadCache := true;
  JDB.AutoFreeStream := true;
  // IO cache ģ�ͣ���smBigData�Ժ󲻻�cache��д��һ��ֻҪ���ڴ�stream�������ÿ�cache
  JDB.CoreSpace.Mode := smBigData;
  JDB.CoreSpace.MaxCacheMemory := 128 * 1024 * 1024; // ���IO���ļ���������Ը���Ĭ��16M
end;

destructor TTZDB2_Json_DB_Frm.Destroy;
begin
  DisposeObject(JDB);
  DisposeObject(Cipher_);
  inherited;
end;

end.
