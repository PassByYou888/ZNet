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
  vl: TDBEngineVL; // TDBEngineVL�Ǹ�key-value���ݽṹԭ��
begin
  vl := TDBEngineVL.Create;
  vl['Key'] := 'RandomValue';
  vl['Value'] := 1; // ����Ҫ����ͳ�Ƶ�ֵΪ1

  // ͳ�ƺͷ���ʹ�÷�������ע��� MyCustomAnalysis ���������д���
  // ͳ�ƺͷ����ڷ������˽���ʱ�����Խ������ƥ�䣬ͼ�������ԣ��ı������ԣ����������Եȵȣ����Ƕ����ڲ��л�ƽ̨�й�������Ȼ���㻹��Ҫ��Ӧ���㷨ģ��֧��
  // ͳ�ƺͷ��������ڷ�������ִ̬�У�����Ƭ���������������ͳ�ƴ������ݿ⣬���ú����ԣ�
  // ��������������ִ�����ͳ�ƺͷ���������ͨ���¼�������step to step�Ĳ�����ZDB��ȫ���ݺ�֧��������������
  // �ǲ��Ǹо��͵���һ����
  DBClient.QueryDBP(
    'MyCustomAnalysis',      // MyCustomAnalysis �ڷ�����ע���ʵ��
    False,                   // ������Ƭ�Ƿ�ͬ�����ͻ��ˣ���Ϊ���ǵ�ͳ��׷����ǽ�������ﲻ��Ҫͬ�����÷�����ȥ�ɣ�����ֻ��Ҫ������¼���ָ��ͳ����ɺ��ʲô��
    True,                    // �Ƿ񽫲�ѯ���д�뵽Output���ݿ⣬���Output�൱����select����ͼ������Output��Copy
    False,                   // output����Ϊ�ڴ����ݿ⣬�����False����ѯ��output����һ��ʵ���ļ����д洢
    False,                   // �Ƿ����ѯ�������ʼ��
    JsonDestDBEdit.Text,     // ��ѯ�����ݿ�����
    AnalysisDestDBEdit.Text, // ͳ�Ƶ�Output����
    1.0,                     // ��Ƭ����ʱ��,��Ϊ��ѯ����Ƶ�ʣ�ZDB�ײ���ڸ�ʱ���ڶԲ�ѯ������л����ѹ����Ȼ���ٷ��͹���,0�Ǽ�ʱ����
    0,                       // ���ȴ��Ĳ�ѯʱ�䣬0������
    0,                       // ���ƥ���ѯ�ķ�����Ŀ��
    vl,                      // ���͸�MyCustomQuery�õ�KeyValue����
    nil,
    procedure(dbN, outN, pipeN: string; TotalResult: Int64)
    begin
      // ��������ѯ���ʱ������������¼�
      DoStatus('ͳ�� %s ��� �ܹ������ %d �������ݿ�%s��', [dbN, TotalResult, outN]);
      ResultMemo.BeginUpdate;
      ResultMemo.Lines.Clear;
      // ͳ����ɺ�����һ���������ļ����ݿ�
      // �����ڸ��¼��п��Է����Ը����ݿ�����ٴ�ͳ�ƣ��ٴβ�ѯ���Եõ�������Ҫ�Ľ��
      // �������ﲻ����β�ѯ�ˣ�ֱ�ӽ�ͳ�ƽ�����ص����ز�����ʾ
      DBClient.DownloadDBP(False, outN,
        procedure(dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64)
        var
          ns: TStringList;
        begin
          // ��������ѯ����������������ݷ���
          // ���¼�����������ʱ�ģ����ý��������ɵ������Ҫ�ݴ��ѯ������ݣ�������������
          ns := TStringList.Create;
          ns.LoadFromStream(DataSour);
          ResultMemo.Lines.AddStrings(ns);
          DisposeObject(ns);
        end,
        procedure(dbN, outN, pipeN: string; TotalResult: Int64)
        begin
          // ��Ϊ���������ͳ�ƽ����������������Ҫ���ͳ�����ݿ��ˣ����ڣ����ǽ�ɾ����
          // ע�⣺���ͳ�ƿ����ڱ�ĳ�����߷��ʣ������ɾ���ͻ����
          // Ҫ��������⣬������ͳ��ʱ��ֻ��Ҫȷ��ͳ��������ļ����ݿ���Ψһ��
          DBClient.CloseDB(dbN, True);

          DoStatus('ͳ�ƽ�� %s ������� �ܹ� %d ��', [dbN, TotalResult]);
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
  // InitDB�ĵ�һ���������ڴ����ݿ⣬�������ó�false�Ǵ���һ���ļ����ݿ�
  DBClient.InitDB(False, JsonDestDBEdit.Text);

  // ����100��json����ʵ���ļ���
  // value��11��ʼ������ע�⣬����value����ʹ���ַ���
  for i := 10 + 1 to 100 + 10 do
    begin
      j := TDBEngineJson.Create;
      j.S['myKey'] := IntToStr(i);
      // randomValue������ʾͳ�ƺͷ�������
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
  // ��Ϊ��ƽ̨�����⣬indy��ios�Ͱ�׿ƽ̨�ײ㶼��֧�ֶ����¼�
  // �����ֶ�������״̬
  // �����ӳɹ������Ǽ���һ����ʱ����ѭ��������
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
  // InitDB�ĵ�һ���������ڴ����ݿ⣬�������ó�false�Ǵ���һ���ļ����ݿ�
  DBClient.InitDB(False, JsonDestDBEdit.Text);

  // ����10��json����ʵ���ļ���
  for i := 1 to 10 do
    begin
      j := TDBEngineJson.Create;
      j.S['myKey'] := IntToStr(i);
      // randomValue������ʾͳ�ƺͷ�������
      j.i['RandomValue'] := umlRandomRange(1, 10);
      DBClient.PostAssembleStream(JsonDestDBEdit.Text, j);
      DisposeObject(j);
    end;
end;

procedure TFMXBatchDataClientForm.Gen100kJsonButtonClick(Sender: TObject);
var
  i: Integer;
  j: TDBEngineJson;
begin
  TabControl.Enabled := False;
  // InitDB�ĵ�һ���������ڴ����ݿ⣬�������ó�false�Ǵ���һ���ļ����ݿ�
  DBClient.InitDB(False, JsonDestDBEdit.Text);

  // ����100000��json����ʵ���ļ���
  // value��11��ʼ������ע�⣬����value����ʹ���ַ���
  for i := 10 + 1 to 100000 + 10 do
    begin
      j := TDBEngineJson.Create;
      j.S['myKey'] := IntToStr(i);
      // randomValue������ʾͳ�ƺͷ�������
      j.i['RandomValue'] := umlRandomRange(1, 10);
      DBClient.FastPostCompleteBuffer(JsonDestDBEdit.Text, j);
      DisposeObject(j);
    end;
  TabControl.Enabled := True;
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
                      DoStatus('��¼�ɹ�');
                      DBClient.TunnelLinkP(
                        procedure(const State: Boolean)
                        begin
                          if State then
                            begin
                              DoStatus('˫ͨ�����ӳɹ�');
                              TabControl.ActiveTab := DBOperationDataTabItem;

                              // ��Ϊ��ƽ̨�����⣬indy��ios�Ͱ�׿ƽ̨�ײ㶼��֧�ֶ����¼�
                              // �����ֶ�������״̬
                              // �����ӳɹ������Ǽ���һ����ʱ����ѭ��������
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
  vl: TDBEngineVL; // TDBEngineVL�Ǹ�key-value���ݽṹԭ��
begin
  vl := TDBEngineVL.Create;
  vl['Key'] := JsonKeyEdit.Text;
  vl['Value'] := JsonValueEdit.Text;

  ResultMemo.BeginUpdate;
  ResultMemo.Lines.Clear;

  DBClient.QueryDBP(
    'MyCustomQuery',   // MyCustomQuery�ڷ�����ע���ʵ��
  True,                // ������Ƭ�Ƿ�ͬ�����ͻ���
  False,               // �Ƿ񽫲�ѯ���д�뵽Output���ݿ⣬���Output�൱����select����ͼ������Output��Copy
  True,                // output����Ϊ�ڴ����ݿ⣬�����False����ѯ��output����һ��ʵ���ļ����д洢
  False,               // �Ƿ����ѯ�������ʼ��
  JsonDestDBEdit.Text, // ��ѯ�����ݿ�����
  '',                  // ��ѯ��Output���ƣ���Ϊ���ǲ�д��Output��������ʱ�ڴ棬������Ժ��Ե�
  1.0,                 // ��Ƭ����ʱ��,��Ϊ��ѯ����Ƶ�ʣ�ZDB�ײ���ڸ�ʱ���ڶԲ�ѯ������л����ѹ����Ȼ���ٷ��͹���,0�Ǽ�ʱ����
  0,                   // ���ȴ��Ĳ�ѯʱ�䣬0������
  0,                   // ���ƥ���ѯ�ķ�����Ŀ��
  vl,                  // ���͸�MyCustomQuery�õ�KeyValue����
    procedure(dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64)
    var
      ns: TStringList;
    begin
      // ��������ѯ����������������ݷ���
      // ���¼�����������ʱ�ģ����ý��������ɵ������Ҫ�ݴ��ѯ������ݣ�������������
      ns := TStringList.Create;
      ns.LoadFromStream(DataSour);
      ResultMemo.Lines.AddStrings(ns);
      DisposeObject(ns);
    end,
    procedure(dbN, outN, pipeN: string; TotalResult: Int64)
    begin
      // ��������ѯ���ʱ������������¼�
      ResultMemo.EndUpdate;
      TabControl.ActiveTab := ResultTabItem;
      DoStatus('��ѯ %s ��� �ܹ��ҵ�ƥ�� %d ��', [dbN, TotalResult]);
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
end;

end.
