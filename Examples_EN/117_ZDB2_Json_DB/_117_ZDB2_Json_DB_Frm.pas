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

  {  Use the IO thread pool to input data in bulk. Next, let the IO thread pool be responsible for condition analysis  }
  {  TExpert for IO thread pool_Query_IO_Data is not a linked list, and the data queue can reach tens of billions on x64, which does not affect system performance  }
  TExpert_Query_IO_Data = class(TIO_Thread_Data)
  public
    js_index: Integer;
    jsData: TMem64;
    procedure Process; override; {  Process is very suitable for performing arithmetic operations, as well as tedious processing processes, which are suitable for this job  }
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
  js.LoadFromStream(jsData.Stream64); {  Decode JSON  }
  arry := js.A['exp_math_arry'];
  for i := 0 to arry.Count - 1 do
    begin
      n := arry.S[i];
      {  UmlReplaceSum is a computational statistic that is equivalent to finding a string. It searches for 99 in Word mode and appears at least 4 times  }
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
      MakeInfoLabel.Caption := PFormat('Data entry:%d physical space (automatic expansion)%s free space%s',
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
            {  UmlReplaceSum is a computational statistic that is equivalent to finding a string. It searches for 99 in Word mode and appears at least 4 times  }
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
  {  Flush ensures that the modified content is written to physical space  }
  JDB.Flush(False);

  {  Creating an IO thread pool does not necessarily mean that the faster the number of threads, the larger the computational workload, the more efficient it can be  }
  Queue__ := TIO_Thread.Create(8);

  ProgressBar.Max := JDB.Count - 1;

  {  Open a thread to wait for the check to be completed  }
  running := true;
  TCompute.RunP_NP(procedure
    var
      io_data: TExpert_Query_IO_Data;
    begin
      while running or (Queue__.Count > 0) do
        begin
          {  IOThread core mechanism: enqueue is 123, and dequeue will also be 123  }
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
        JDB.CoreSpace.ReadData(query_data.jsData, Queue^.Data.ID); {  Reading physical data from physical space does not decode JSON in this step  }
        {  Query_Data is queued for processing  }
        {  TExpert_Query_IO_Data will be restored to keep after processing  }
        {  Enqueue thread safety  }
        Queue__.Enqueue(query_data);

        {  Restrict input queues to prevent excessive bloating  }
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
            {  UmlReplaceSum is a computational statistic that is equivalent to finding a string. It searches for 99 in Word mode and appears at least 4 times  }
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
  {  Simply modify the JSON content and use flush to refactor the structure  }
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
                arry.Delete(j) {  When modifying Json content, it will automatically be stored in physical space according to time sequence  }
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
  JDB.Flush(False); {  Flush ensures that the modified content is written to physical space  }
  DoStatus('modify done.');
end;

procedure TTZDB2_Json_DB_Frm.extractSpaceButtonClick(Sender: TObject);
var
  new_jdb_stream: TStream;
begin
  if JDB.Count <= 0 then
      exit;
  {  After a large amount of deletion and recycling, the physical space of zdb2 may experience fragmentation. Here is a demonstration of how to recycle fragmentation  }
  new_jdb_stream := TMS64.CustomCreate(100 * 1024 * 1024);

  {  The first step is to reconstruct the current data into a new physical space  }
  {  ExtractTo will directly reconstruct the physical spatial structure at the kernel layer, with no fragmentation in the new structure and a smaller database, which is equivalent to optimization  }
  {  Generally speaking, if the data does not need to be changed, extractTo can be used to operate it once before release  }
  JDB.ExtractTo(new_jdb_stream);

  {  Step 2: Release the current jdb  }
  DisposeObject(JDB);

  {  Step 3: Recreate the jdb  }
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
  {  Database encryption interface  }
  Cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csTwoFish, '123456', 1, true, true);
  {  Json data engine for zdb  }
  jdb_stream := TMS64.CustomCreate(100 * 1024 * 1024); {  If stored in a file, this is for TFilestream  }
  JDB := TZDB2_List_Json.Create(TZDB2_Json, nil, 500, jdb_stream, False, 100 * 1024 * 1024, 200, Cipher_);
  JDB.IOHnd.Cache.UsedWriteCache := true;
  JDB.IOHnd.Cache.UsedReadCache := true;
  JDB.AutoFreeStream := true;
  {  The IO cache model does not cache reads and writes to smBigData in the future. Generally, as long as it is a memory stream, there is no need to open the cache  }
  JDB.CoreSpace.Mode := smBigData;
  JDB.CoreSpace.MaxCacheMemory := 128 * 1024 * 1024; {  If the IO is a file, the cache can be larger, with a default of 16M  }
end;

destructor TTZDB2_Json_DB_Frm.Destroy;
begin
  DisposeObject(JDB);
  DisposeObject(Cipher_);
  inherited;
end;

end.
