unit FMXBatchDataClientFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TabControl, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation,
  FMX.Layouts,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.ScrollBox, FMX.Memo,
  Z.Net.DataStoreService, Z.ZDB.Engine,
  Z.ZDB.LocalManager, Z.Net.Client.Indy,
  Z.Notify,
  Z.Net, Z.Core, Z.Status,
  Z.PascalStrings, Z.MemoryStream, Z.UnicodeMixedLib,
  Z.Net.DataStoreService.VirtualAuth,
  Z.Net.DoubleTunnelIO.VirtualAuth, FMX.Memo.Types;

type
  TMyDataStoreClient = class(TDataStoreClient_VirtualAuth)
  protected
    procedure ClientDisconnect(Sender: TZNet_Client); override;

  end;

  TFMXBatchDataClientForm = class(TForm)
    TabControl: TTabControl;
    LoginTabItem: TTabItem;
    Layout1: TLayout;
    Layout2: TLayout;
    Label1: TLabel;
    UserIDEdit: TEdit;
    Layout3: TLayout;
    Label2: TLabel;
    PasswdEdit: TEdit;
    LoginBtn: TButton;
    Layout4: TLayout;
    Label3: TLabel;
    ServerEdit: TEdit;
    Timer1: TTimer;
    StatusMemo: TMemo;
    OfflineTabItem: TTabItem;
    Layout5: TLayout;
    DisconnectButton: TButton;
    DBOperationDataTabItem: TTabItem;
    Gen10JsonButton: TButton;
    DisconnectCheckTimer: TTimer;
    Layout6: TLayout;
    Label4: TLabel;
    JsonDestDBEdit: TEdit;
    Gen100kJsonButton: TButton;
    ResultTabItem: TTabItem;
    ResultMemo: TMemo;
    QueryJsonButton: TButton;
    Layout7: TLayout;
    Label5: TLabel;
    JsonKeyEdit: TEdit;
    Layout8: TLayout;
    Label6: TLabel;
    JsonValueEdit: TEdit;
    ResetJsonDBButton: TButton;
    AnalysisJsonButton: TButton;
    Layout9: TLayout;
    Label7: TLabel;
    AnalysisDestDBEdit: TEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoginBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure DisconnectCheckTimerTimer(Sender: TObject);
    procedure Gen10JsonButtonClick(Sender: TObject);
    procedure Gen100kJsonButtonClick(Sender: TObject);
    procedure QueryJsonButtonClick(Sender: TObject);
    procedure ResetJsonDBButtonClick(Sender: TObject);
    procedure AnalysisJsonButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    RecvTunnel, SendTunnel: TZNet_Client;
    DBClient: TMyDataStoreClient;
    procedure DoStatusNear(AText: string; const ID: Integer);
  end;

var
  FMXBatchDataClientForm: TFMXBatchDataClientForm;

implementation

{$R *.fmx}


procedure TMyDataStoreClient.ClientDisconnect(Sender: TZNet_Client);
begin
  FMXBatchDataClientForm.TabControl.ActiveTab := FMXBatchDataClientForm.LoginTabItem;
  inherited;
end;

procedure TFMXBatchDataClientForm.AnalysisJsonButtonClick(Sender: TObject);
var
  vl: TDBEngineVL; {  Tdbenginevl is a key value data structure prototype  }
begin
  vl := TDBEngineVL.Create;
  vl['Key'] := 'RandomValue';
  vl['Value'] := 1; {  The value we need to calculate is 1  }

  {  Statistics and analysis are triggered using the MyCustomAnalysis filter registered on the server side  }
  {  When statistics and analysis are conducted on the server side, deep matching, image similarity, text similarity, voice similarity, and so on can be performed. They will all work on a parallel platform, and of course, you also need corresponding algorithm module support  }
  {  Statistics and analysis can be executed statically on the server (without fragmented buffer feedback, which is very effective for statistical analysis of large databases)  }
  {  After the server quietly executes statistics and analysis, we perform step to step operations through events. ZDB is fully compatible and supports anonymous function mechanisms  }
  {  Does it feel like a single machine?  }
  DBClient.QueryDBP(
    'MyCustomAnalysis',      {  Mycustomanalysis is registered and implemented on the server  }
    False,                   {  Whether the buffer fragments are synchronized to the client or not, because our statistics pursue the results. There is no need to synchronize here. Let the server do it. We only need to specify what to do after the statistics are completed in the completion event  }
    True,                    {  Will the query results be written to the Output database? This output is equivalent to selecting to a view, but the output will copy  }
    False,                   {  The output data is an in memory database. If it is false, the queried output will be stored as a physical file  }
    False,                   {  Reverse query, starting from the end  }
    JsonDestDBEdit.Text,     {  Database name of the query  }
    AnalysisDestDBEdit.Text, {  Statistical Output Name  }
    1.0,                     {  Fragment buffer time. Because the query is too frequent, the bottom layer of ZDB will cache and compress the query results within this time, and then send them. 0 is immediate feedback  }
    0,                       {  The maximum waiting query time, 0 is infinite  }
    0,                       {  Maximum number of feedback entries for matching queries  }
    vl,                      {  KeyValue parameter sent to mycustomquery  }
    nil,
      procedure(dbN, outN, pipeN: string; TotalResult: Int64)
    begin
      {  This event is triggered when the server query is completed  }
      DoStatus('Statistics of%s completed. There are%d outputs in the database%s', [dbN, TotalResult, outN]);
      ResultMemo.BeginUpdate;
      ResultMemo.Lines.Clear;
      {  After the statistics are completed, a permanent file database will be output  }
      {  During the change event, we can repeatedly make statistics on the change database and query again to get the results we need  }
      {  However, there is no need to make multiple queries here. The statistical results are directly downloaded to the local and displayed  }
      DBClient.DownloadDBP(False, outN,
          procedure(dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64)
        var
          ns: TStringList;
        begin
          {  After the server queries the results, here is the data feedback  }
          {  The data of this event is temporary and will be killed after the call. If you want to temporarily store the query result data, you must do it here  }
          ns := TStringList.Create;
          ns.LoadFromStream(DataSour);
          ResultMemo.Lines.AddStrings(ns);
          DisposeObject(ns);
        end,
        procedure(dbN, outN, pipeN: string; TotalResult: Int64)
        begin
          {  Because the download has completed the statistical results, the server no longer needs this statistical database. Now, we will delete it  }
          {  Note: If the statistical database is being accessed by a certain pipeline, the deletion here will result in an error  }
          {  To avoid this issue, we only need to ensure that the file database for statistical output is unique when conducting statistics  }
          DBClient.CloseDB(dbN, True);

          DoStatus('Statistics result%s download completed,%d in total', [dbN, TotalResult]);
          ResultMemo.EndUpdate;
          TabControl.ActiveTab := ResultTabItem;
        end);
    end);

  DisposeObject(vl);
end;

procedure TFMXBatchDataClientForm.Button1Click(Sender: TObject);
var
  i: Integer;
  j: TDBEngineJson;
begin
  TabControl.Enabled := False;
  {  The first parameter of InitDB is the memory database, and if we set it to false, we will create a file database  }
  DBClient.InitDB(False, JsonDestDBEdit.Text);

  {  Generate 100 JSON objects to the entity file library  }
  {  Value counts from 11. Note that we use the string value here  }
  for i := 10 + 1 to 100 + 10 do
    begin
      j := TDBEngineJson.Create;
      j.S['myKey'] := IntToStr(i);
      {  Randomvalue is used to demonstrate statistics and analysis capabilities  }
      j.i['RandomValue'] := umlRandomRange(1, 10);
      DBClient.FastPostCompleteBuffer(JsonDestDBEdit.Text, j);
      DisposeObject(j);
    end;
  TabControl.Enabled := True;
end;

procedure TFMXBatchDataClientForm.DisconnectButtonClick(Sender: TObject);
begin
  DBClient.Disconnect;
end;

procedure TFMXBatchDataClientForm.DisconnectCheckTimerTimer(
Sender: TObject);
begin
  {  Due to cross platform problems, Indy does not support disconnection events at the bottom of IOS and Android platforms  }
  {  The disconnection status must be checked manually  }
  {  After the connection is successful, we activate a timer to cycle through the disconnection check  }
  if not DBClient.Connected then
    begin
      DBClient.RecvTunnel.TriggerDoDisconnect;
      DisconnectCheckTimer.Enabled := False;
    end;
end;

procedure TFMXBatchDataClientForm.DoStatusNear(AText: string;
const ID: Integer);
begin
  StatusMemo.Lines.Add(AText);
  StatusMemo.GoToTextEnd;
end;

procedure TFMXBatchDataClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  RecvTunnel := TZNet_Client_Indy.Create;
  SendTunnel := TZNet_Client_Indy.Create;
  DBClient := TMyDataStoreClient.Create(RecvTunnel, SendTunnel);
  DBClient.RegisterCommand;

  RecvTunnel.QuietMode := True;
  SendTunnel.QuietMode := True;
end;

procedure TFMXBatchDataClientForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
  DisposeObject([DBClient, RecvTunnel, SendTunnel]);
end;

procedure TFMXBatchDataClientForm.Gen10JsonButtonClick(Sender: TObject);
var
  i: Integer;
  j: TDBEngineJson;
begin
  {  The first parameter of InitDB is the memory database, and if we set it to false, we will create a file database  }
  DBClient.InitDB(False, JsonDestDBEdit.Text);

  {  Generate 10 JSON objects to the entity file library  }
  for i := 1 to 10 do
    begin
      j := TDBEngineJson.Create;
      j.S['myKey'] := IntToStr(i);
      {  Randomvalue is used to demonstrate statistics and analysis capabilities  }
      j.i['RandomValue'] := umlRandomRange(1, 10);
      DBClient.PostAssembleStream(JsonDestDBEdit.Text, j);
      DisposeObject(j);
    end;
end;

procedure TFMXBatchDataClientForm.Gen100kJsonButtonClick(Sender: TObject);
begin
  TabControl.Enabled := False;
  {  The first parameter of InitDB is the memory database, and if we set it to false, we will create a file database  }
  DBClient.InitDB(False, JsonDestDBEdit.Text);

  {  The new version of ZNet can support thread sending, which can avoid stuttering and ensure smooth sending  }
  TCompute.RunP_NP(procedure
    var
      i: Integer;
      j: TDBEngineJson;
    begin
      {  Generate 100000 JSON objects to the entity file library  }
      {  Value counts from 11. Note that we use the string value here  }
      for i := 10 + 1 to 100000 + 10 do
        begin
          j := TDBEngineJson.Create;
          j.S['myKey'] := IntToStr(i);
          {  Randomvalue is used to demonstrate statistics and analysis capabilities  }
          j.i['RandomValue'] := umlRandomRange(1, 10);
          DBClient.FastPostCompleteBuffer(JsonDestDBEdit.Text, j);
          DisposeObject(j);
        end;
      TCompute.PostP1(procedure
        begin
          TabControl.Enabled := True;
        end);
    end);
end;

procedure TFMXBatchDataClientForm.LoginBtnClick(Sender: TObject);
begin
  SendTunnel.AsyncConnectP(ServerEdit.Text, 10099, procedure(const sState: Boolean)
    begin
      if sState then
          RecvTunnel.AsyncConnectP(ServerEdit.Text, 10098, procedure(const rState: Boolean)
          begin
            if rState then
                DBClient.UserLoginP(UserIDEdit.Text, PasswdEdit.Text,
                  procedure(const State: Boolean)
                begin
                  if State then
                    begin
                      DoStatus('Login succeeded');
                      DBClient.TunnelLinkP(
                          procedure(const State: Boolean)
                        begin
                          if State then
                            begin
                              DoStatus('Dual channel link successful');
                              TabControl.ActiveTab := DBOperationDataTabItem;

                              {  Due to cross platform problems, Indy does not support disconnection events at the bottom of IOS and Android platforms  }
                              {  The disconnection status must be checked manually  }
                              {  After the connection is successful, we activate a timer to cycle through the disconnection check  }
                              DisconnectCheckTimer.Enabled := True;
                              DBClient.ProgressEngine.PostExecuteP(1, procedure(Sender: TNPostExecute)
                                begin
                                  while not DBClient.DataCipherKeyFinished do
                                      DBClient.Progress;
                                end)
                            end;
                        end);
                    end;
                end);
          end);
    end);
end;

procedure TFMXBatchDataClientForm.QueryJsonButtonClick(Sender: TObject);
var
  vl: TDBEngineVL; {  Tdbenginevl is a key value data structure prototype  }
begin
  vl := TDBEngineVL.Create;
  vl['Key'] := JsonKeyEdit.Text;
  vl['Value'] := JsonValueEdit.Text;

  ResultMemo.BeginUpdate;
  ResultMemo.Lines.Clear;

  DBClient.QueryDBP(
  'MyCustomQuery',     {  Registration and implementation of mycustomquery in server  }
  True,                {  Whether the buffer fragments are synchronized to the client  }
  False,               {  Will the query results be written to the Output database? This output is equivalent to selecting to a view, but the output will copy  }
  True,                {  The output data is an in memory database. If it is false, the queried output will be stored as a physical file  }
  False,               {  Reverse query, starting from the end  }
  JsonDestDBEdit.Text, {  Database name of the query  }
  '',                  {  The output name of the query, because we do not write output, but also temporary memory, can be ignored here  }
  1.0,                 {  Fragment buffer time. Because the query is too frequent, the bottom layer of ZDB will cache and compress the query results within this time, and then send them. 0 is immediate feedback  }
  0,                   {  The maximum waiting query time, 0 is infinite  }
  0,                   {  Maximum number of feedback entries for matching queries  }
  vl,                  {  KeyValue parameter sent to mycustomquery  }
    procedure(dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64)
    var
      ns: TStringList;
    begin
      {  After the server queries the results, here is the data feedback  }
      {  The data of this event is temporary and will be killed after the call. If you want to temporarily store the query result data, you must do it here  }
      ns := TStringList.Create;
      ns.LoadFromStream(DataSour);
      ResultMemo.Lines.AddStrings(ns);
      DisposeObject(ns);
    end,
      procedure(dbN, outN, pipeN: string; TotalResult: Int64)
    begin
      {  This event is triggered when the server query is completed  }
      ResultMemo.EndUpdate;
      TabControl.ActiveTab := ResultTabItem;
      DoStatus('Query%s completed. A total of%d matches were found', [dbN, TotalResult]);
    end);

  DisposeObject(vl);
end;

procedure TFMXBatchDataClientForm.ResetJsonDBButtonClick(Sender: TObject);
begin
  DBClient.ResetData(JsonDestDBEdit.Text);
  DBClient.ResetData(AnalysisDestDBEdit.Text);
end;

procedure TFMXBatchDataClientForm.Timer1Timer(Sender: TObject);
begin
  DBClient.Progress;
  CheckThread;
end;

end.
