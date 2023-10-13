unit ZDBBatchDataServiceFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.Status,
  Z.ZDB.Engine, Z.ZDB.LocalManager,
  Z.Net.Server.CrossSocket,
  Z.Net.Server.ICS,
  Z.Net.Server.ICSCustomSocket,
  Z.Net.Server.Indy, Z.MemoryStream, Z.DFE,
  Z.Delphi.JsonDataObjects, Z.Net,
  Z.Net.DataStoreService, Z.Core,
  Z.Net.DataStoreService.NoAuth,
  Z.Net.DoubleTunnelIO,
  Z.Net.DataStoreService.Common, Z.UnicodeMixedLib,
  Z.Net.DataStoreService.VirtualAuth,
  Z.Net.DoubleTunnelIO.VirtualAuth;

type
  TMyDataStoreService = class(TDataStoreService_VirtualAuth)
  protected
    procedure UserAuth(Sender: TVirtualAuthIO); override;
    procedure UserLoginSuccess(UserDefineIO: TService_RecvTunnel_UserDefine_VirtualAuth); override;
    procedure UserLinkSuccess(UserDefineIO: TService_RecvTunnel_UserDefine_VirtualAuth); override;
    procedure UserOut(UserDefineIO: TService_RecvTunnel_UserDefine_VirtualAuth); override;
  end;

  TZDBBatchDataServiceForm = class(TForm)
    StatusMemo: TMemo;
    WatchMemo: TMemo;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
    procedure MyCustomJsonQuery(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
    procedure MyCustomJsonAnalysisQuery(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
  public
    { Public declarations }
    RecvTunnel, SendTunnel: TZNet_Server;
    DBService: TMyDataStoreService;
    procedure DoStatusNear(AText: string; const ID: Integer);
  end;

var
  ZDBBatchDataServiceForm: TZDBBatchDataServiceForm;

implementation

{$R *.dfm}


procedure TMyDataStoreService.UserLinkSuccess(UserDefineIO: TService_RecvTunnel_UserDefine_VirtualAuth);
begin
  DoStatus('User%s successfully established interactive link', [UserDefineIO.UserID]);
  inherited;
end;

procedure TMyDataStoreService.UserAuth(Sender: TVirtualAuthIO);
begin
  Sender.Accept;
end;

procedure TMyDataStoreService.UserLoginSuccess(UserDefineIO: TService_RecvTunnel_UserDefine_VirtualAuth);
begin
  DoStatus('User%s logged in successfully', [UserDefineIO.UserID]);
  inherited;
end;

procedure TMyDataStoreService.UserOut(UserDefineIO: TService_RecvTunnel_UserDefine_VirtualAuth);
begin
  DoStatus('User%s is offline', [UserDefineIO.UserID]);
  inherited;
end;

procedure TZDBBatchDataServiceForm.DoStatusNear(AText: string; const ID: Integer);
begin
  StatusMemo.Lines.Add(AText);
end;

procedure TZDBBatchDataServiceForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RecvTunnel.StopService;
  SendTunnel.StopService;
end;

procedure TZDBBatchDataServiceForm.MyCustomJsonQuery(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
var
  key: string;
  value: string;
  j: TDBEngineJson;
begin
  {  Query is a high throughput trigger frequency that generates millions of events per second when called  }
  {  When writing query, you must pay attention to optimizing the program and compare the conditions with the least code  }
  {  Try to avoid creating entity objects in the query, as frequent object creation can drag query efficiency too much  }
  if not qState.IsJson then
      exit;

  {  Get a JSON object from the database  }
  j := qState.Eng.GetJson(qState);

  {  Sacrificing performance for security checks  }
  if not dPipe.Values.Exists('Key') then
      exit;
  if not dPipe.Values.Exists('Value') then
      exit;

  {  The values in dPipe come from the query parameters sent by the client, not the database  }
  key := dPipe.Values.GetDefaultValue('Key', '');

  {  The values in dPipe come from the query parameters sent by the client, not the database  }
  value := dPipe.Values.GetDefaultValue('Value', '');

  Allowed := umlMultipleMatch(value, j.S[key]);
end;

procedure TZDBBatchDataServiceForm.MyCustomJsonAnalysisQuery(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
var
  key: string;
  value: Integer;
  j: TDBEngineJson;
begin
  {  Query is a high throughput trigger frequency that generates millions of events per second when called  }
  {  When writing query, you must pay attention to optimizing the program and compare the conditions with the least code  }
  {  Try to avoid creating entity objects in the query, as frequent object creation can drag query efficiency too much  }
  if not qState.IsJson then
      exit;

  {  Get a JSON object from the database  }
  j := qState.Eng.GetJson(qState);

  {  Sacrificing performance for security checks  }
  if not dPipe.Values.Exists('Key') then
      exit;
  if not dPipe.Values.Exists('Value') then
      exit;

  {  The values in dPipe come from the query parameters sent by the client, not the database  }
  key := dPipe.Values.GetDefaultValue('Key', '');

  {  The values in dPipe come from the query parameters sent by the client, not the database  }
  value := dPipe.Values.GetDefaultValue('Value', 0);

  Allowed := value = j.I[key];
end;

procedure TZDBBatchDataServiceForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  RecvTunnel := TZNet_Server_CrossSocket.Create;

  SendTunnel := TZNet_Server_CrossSocket.Create;

  DBService := TMyDataStoreService.Create(RecvTunnel, SendTunnel);
  DBService.RegisterCommand;
  DBService.ZDBLocal.LoadDB(False);

  DBService.RegisterQuery_C('MyCustomQuery').OnPipelineQuery := MyCustomJsonQuery;
  DBService.RegisterQuery_C('MyCustomAnalysis').OnPipelineQuery := MyCustomJsonAnalysisQuery;

  RecvTunnel.StartService('', 10099);
  SendTunnel.StartService('', 10098);

  RecvTunnel.QuietMode := True;
  SendTunnel.QuietMode := True;

  DBService.SwitchAsMaxSecurity;
end;

procedure TZDBBatchDataServiceForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
  DisposeObject([DBService, RecvTunnel, SendTunnel]);
end;

procedure TZDBBatchDataServiceForm.Timer1Timer(Sender: TObject);
begin
  DBService.Progress;
  Check_Soft_Thread_Synchronize(10);
end;

procedure TZDBBatchDataServiceForm.Timer2Timer(Sender: TObject);
var
  I: Integer;
  lst: TCore_ListForObj;
  db: TZDBLMStore;
  pl: TZDBPipeline;
begin
  lst := TCore_ListForObj.Create;
  DBService.ZDBLocal.GetDBList(lst);

  WatchMemo.Lines.BeginUpdate;
  WatchMemo.Lines.Clear;

  I := Round(DBService.PostCounterOfPerSec);

  WatchMemo.Lines.Add(Format('An average of%d addition, deletion and modification operations are received per second', [I]));

  WatchMemo.Lines.Add('Active database');
  for I := 0 to lst.Count - 1 do
    begin
      db := TZDBLMStore(lst[I]);
      WatchMemo.Lines.Add(Format('Library%s entry:%d size:%s cache%s', [db.name, db.Count, umlSizeToStr(db.DBEngine.Size).Text, db.CacheAnnealingState]));
    end;

  lst.Clear;
  WatchMemo.Lines.Add('Working query pipeline');
  DBService.ZDBLocal.GetPipeList(lst);
  for I := 0 to lst.Count - 1 do
    begin
      pl := TZDBPipeline(lst[I]);
      WatchMemo.Lines.Add(Format('Pipeline%s crawled%d times per second', [pl.PipelineName, Round(pl.QueryCounterOfPerSec)]));
    end;

  DisposeObject(lst);
  WatchMemo.Lines.EndUpdate;
end;

end.
