program DatastoreServ;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Delphi.JsonDataObjects,
  Z.Core,
  Z.PascalStrings,
  Z.Status,
  Z.UnicodeMixedLib,
  Z.DFE,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.DoubleTunnelIO.NoAuth,
  Z.ZDB.LocalManager,
  Z.ZDB.Engine,
  Z.ZDB,
  Z.ZDB.ObjectData_LIB,
  Z.Net.DataStoreService.NoAuth;

const
  IsQuiet: Boolean = False;

type
  TMyDataStoreService = class(TDataStoreService_NoAuth)
  public
    constructor Create(RecvTunnel_, SendTunnel_: TZNet_Server); override;
    destructor Destroy; override;
    procedure cmd_MyJsonQuery(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
  end;

constructor TMyDataStoreService.Create(RecvTunnel_, SendTunnel_: TZNet_Server);
begin
  inherited Create(RecvTunnel_, SendTunnel_);
  RegisterQuery_C('MyJsonQuery').OnPipelineQuery := cmd_MyJsonQuery;
end;

destructor TMyDataStoreService.Destroy;
begin
  inherited Destroy;
end;

procedure TMyDataStoreService.cmd_MyJsonQuery(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
var
  js: TJsonObject;
begin
  Allowed := False;
  if qState.ID <> 1 then
      exit;
  // ZDB�Դ�����json�Ļ����ڱ��������json��ʵ��cache���Ӷ������˷���load���ﵽ���ٲ�ѯ����������
  // ������ʾ����������ʹ��stream��ÿ�α�����ҪLoad��Ȼ���ж�������������������ʾZDB��ԭ��
  js := TJsonObject.Create;
  try
    Allowed := True;
    js.LoadFromStream(qState.Cache, TEncoding.UTF8);
    if dPipe.Values.Exists('Name') then
        Allowed := Allowed and umlSearchMatch(dPipe.Values.S['Name'], js.S['Name']);
    if dPipe.Values.Exists('Phone') then
        Allowed := Allowed and umlSearchMatch(dPipe.Values.S['Phone'], js.S['Phone']);
    if dPipe.Values.Exists('Comment') then
        Allowed := Allowed and umlSearchMatch(dPipe.Values.S['Comment'], js.S['Comment']);
  except
      Allowed := False;
  end;
  js.Free;
end;

var
  DatabasePhyServ: TPhysicsServer;
  DatabaseRecvTunnel, DatabaseSendTunnel: TZNet_WithP2PVM_Server;
  DatabaseService: TMyDataStoreService;

procedure InitServ;
begin
  DatabaseRecvTunnel := TZNet_WithP2PVM_Server.Create;
  DatabaseRecvTunnel.QuietMode := IsQuiet;

  DatabaseSendTunnel := TZNet_WithP2PVM_Server.Create;
  DatabaseSendTunnel.QuietMode := IsQuiet;
  // BigStreamMemorySwapSpace����BigStream���͵�����ΪTMemoryStream or TMemoryStream64���������ļ�ת�棬�Ӷ��ﵽ�ͷ��ڴ������
  // ��ѡ����Ҫ����Ӧ��ͻȻ������������Ŀ������������ǧ�Ƶ�BigStream��������
  DatabaseSendTunnel.BigStreamMemorySwapSpace := True;
  // BigStreamSwapSpaceTriggerSize�������ļ�ת����������������TMemoryStream���ڴ�ռ�ÿռ����ﵽ1024*1024�Ż������ļ�ת��
  DatabaseSendTunnel.BigStreamSwapSpaceTriggerSize := 1024 * 1024;

  DatabaseService := TMyDataStoreService.Create(DatabaseRecvTunnel, DatabaseSendTunnel);
  DatabaseService.ZDBLocal.RootPath := umlCombinePath(ZDBLocalManager_SystemRootPath, 'Database');
  umlCreateDirectory(DatabaseService.ZDBLocal.RootPath);
  DatabaseService.RegisterCommand;
  DoStatus('ZDB V1.0 Root directory %s', [DatabaseService.ZDBLocal.RootPath]);

  DatabasePhyServ := TPhysicsServer.Create;
  DatabasePhyServ.QuietMode := IsQuiet;
  DatabasePhyServ.AutomatedP2PVMServiceBind.AddService(DatabaseRecvTunnel, '::', 1);
  DatabasePhyServ.AutomatedP2PVMServiceBind.AddService(DatabaseSendTunnel, '::', 2);
  DatabasePhyServ.AutomatedP2PVMAuthToken := 'datastore';
  if DatabasePhyServ.StartService('', 9238) then
      DoStatus('database service listening port 9238 successed.')
  else
      DoStatus('database service open port 9238 failed.');
end;

procedure DoMainLoop;
begin
  while True do
    begin
      DatabaseService.Progress;
      DatabasePhyServ.Progress;
      CheckThreadSynchronize(10);
    end;
end;

begin
  InitServ();
  DoMainLoop();

end.
