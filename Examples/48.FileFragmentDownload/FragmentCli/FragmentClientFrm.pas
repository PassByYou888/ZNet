unit FragmentClientFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  Z.Core, Z.PascalStrings, Z.UnicodeMixedLib, Z.MemoryStream,
  Z.Status,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.DoubleTunnelIO.NoAuth, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Mask;

type
  // ���������Ա�׼���ݽṹ�������,˼·�������

  // TaskData��ÿ����Ƭ��Ԥ�����ز���,����Զ���ļ���,��Ƭ����ʼλ��,����λ��,��Ƭmd5��֤��,״̬���ȵ�
  TTaskData = record
    FileName: SystemString;
    StartPos, EndPos: Int64;
    Data: TMemoryStream64;
    MD5: TMD5;
    Done: Boolean;
    procedure Init;
    procedure Free;
  end;

  PTaskData = ^TTaskData;

  // ����TaskData����
  TDownloadTask_ = TGenericsList<PTaskData>;

  // Ĭ�Ϸ����������Զ��ͷ�,������Ҫ��һ���Զ��ͷŴ���
  // �÷�������״̬���ͷ��ǳ��ڼ���fpc�ķ��ͻ���,fpc������û���ͷ�ʱ�Ĵ����¼�
  TDownloadTask = class(TDownloadTask_)
  public
    destructor Destroy; override;
    procedure Remove(p: PTaskData);
    procedure Delete(index: Integer);
    procedure Clear;
    function Done: Boolean;
  end;

  // ��session���������������
  // ����Ķ�����ʹ����p2pVM
  // �����p2pVM������ʽΪ������Ƶ�automatedP2PVM
  TP2PVM_Session = class
  private
    // ����ʹ����AutomatedP2PVM֧�ּ���,����ϸ����ο����demo
    // logic��ͷ�Ķ�����������,˫ͨ����Щ����
    logic_recv, logic_send: TZNet_WithP2PVM_Client;
    logic: TZNet_DoubleTunnelClient_NoAuth;
    // phyCli����������
    phyCli: TPhysicsClient;
    // �ļ������ؼ���
    total: Int64;
    // ��Ƭ�����������ʱ�ݴ����
    queue: TDownloadTask_;
    procedure autoP2PVM_Done(Sender: TZNet; P_IO: TPeerIO);
    procedure Download_Backcall(const UserData: Pointer; const UserObject: TCore_Object;
      const FileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5);
  public
    // ��create���캯��ʵ����automatedP2PVM
    constructor Create;
    destructor Destroy; override;
    procedure Progress;
    procedure Connect(Host: SystemString);
    procedure Download(p: PTaskData);
  end;

  TSessionList_ = TGenericsList<TP2PVM_Session>;

  // Ĭ�Ϸ����������Զ��ͷ�,������Ҫ��һ���Զ��ͷŴ���
  // �÷�������״̬���ͷ��ǳ��ڼ���fpc�ķ��ͻ���,fpc������û���ͷ�ʱ�Ĵ����¼�
  TSessionList = class(TSessionList_)
  private
    id: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Remove(obj: TP2PVM_Session);
    procedure Delete(index: Integer);
    procedure Clear;
    procedure Progress;
    function Pick_NextSession: TP2PVM_Session;
  end;

  TFragmentClientForm = class(TForm)
    Timer1: TTimer;
    Memo: TMemo;
    HostEdit: TLabeledEdit;
    connButton: TButton;
    downloadButton: TButton;
    checkDownTimer: TTimer;
    stateMemo: TMemo;
    procedure checkDownTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure downloadButtonClick(Sender: TObject);
    procedure connButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure DoStatus_Backcall(Text_: SystemString; const id: Integer);
  public
    Session: TSessionList;
    DownloadTask: TDownloadTask;
    targetStream: TStream64;
  end;

var
  FragmentClientForm: TFragmentClientForm;

const
  demoFile = 'FragmentServiceDemo.exe'; // ��Ҫ���ص�Զ���ļ���
  bloackSize = 8192;                    // �ֿ����ش�С

implementation

{$R *.dfm}


procedure TTaskData.Init;
begin
  FileName := '';
  StartPos := 0;
  EndPos := 0;
  Data := TMemoryStream64.Create;
  MD5 := NULLMD5;
  Done := False;
end;

procedure TTaskData.Free;
begin
  FileName := '';
  DisposeObjectAndNIl(Data);
end;

destructor TDownloadTask.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TDownloadTask.Remove(p: PTaskData);
begin
  Dispose(p);
  inherited Remove(p);
end;

procedure TDownloadTask.Delete(index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      Dispose(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TDownloadTask.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Dispose(Items[i]);
  inherited Clear;
end;

function TDownloadTask.Done: Boolean;
var
  i: Integer;
begin
  Result := Count > 0;
  for i := 0 to Count - 1 do
      Result := Result and Items[i]^.Done;
end;

procedure TP2PVM_Session.autoP2PVM_Done(Sender: TZNet; P_IO: TPeerIO);
begin
  // ���¼��������е�p2pVM�������ʱ����

  // ����˫ͨ��
  logic.TunnelLinkP(procedure(const lState: Boolean)
    begin
      if lState then
          DoStatus('˫ͨ���������.');
    end);
end;

procedure TP2PVM_Session.Download_Backcall(const UserData: Pointer; const UserObject: TCore_Object;
const FileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5);
var
  p: PTaskData;
begin
  p := UserData;
  p^.Data.Clear;
  p^.Data.WritePtr(DataPtr, DataSize);
  p^.Data.Position := 0;
  p^.Done := True;
  p^.MD5 := MD5;

  Inc(total, DataSize);

  // �����÷��������ļ�ȫ���ݴ浽�ڴ�
  // ����Ļ���Ϊ,���һ����Ƭ���Ժ�,������������һ����Ƭ��
  if queue.Count > 0 then
    begin
      p := queue[0];
      queue.Delete(0);
      // GetFileFragmentDataMΪԭ�ӹ���,ʵ����Զ���ļ���������Ƭ����
      logic.GetFileFragmentDataM(p^.FileName, p^.StartPos, p^.EndPos, p, nil, Download_Backcall);
    end;
end;

constructor TP2PVM_Session.Create;
begin
  inherited Create;
  // ��һ���ֲο�AutomatedP2PVM���demo
  logic_recv := TZNet_WithP2PVM_Client.Create;
  logic_send := TZNet_WithP2PVM_Client.Create;
  logic_recv.QuietMode := True;
  logic_send.QuietMode := True;
  logic := TZNet_DoubleTunnelClient_NoAuth.Create(logic_recv, logic_send);
  logic.RegisterCommand;

  phyCli := TPhysicsClient.Create;
  phyCli.QuietMode := True;
  phyCli.AutomatedP2PVMClientBind.AddClient(logic_recv, '::', 98);
  phyCli.AutomatedP2PVMClientBind.AddClient(logic_send, '::', 99);
  phyCli.AutomatedP2PVMClient := True;
  phyCli.AutomatedP2PVMAuthToken := '123456';

  // AutomatedP2PVM �����ӳɹ���,��ʹ��ʹ��p2pVM����,���¼��������е�p2pVM�������ʱ����
  phyCli.OnAutomatedP2PVMClientConnectionDone_M := autoP2PVM_Done;

  total := 0;
  queue := TDownloadTask_.Create;
end;

destructor TP2PVM_Session.Destroy;
begin
  DisposeObject([logic, logic_recv, logic_send, phyCli]);
  DisposeObject(queue);
  inherited Destroy;
end;

procedure TP2PVM_Session.Progress;
begin
  phyCli.Progress;
  logic.Progress;
end;

procedure TP2PVM_Session.Connect(Host: SystemString);
begin
  phyCli.AsyncConnectP(Host, 9799, procedure(const cState: Boolean)
    begin
    end);
end;

procedure TP2PVM_Session.Download(p: PTaskData);
begin
  if not phyCli.Connected then
      exit;

  // �����÷��������ļ�ȫ���ݴ浽�ڴ�
  // ����Ļ���Ϊ,���һ����Ƭ���Ժ�,������������һ����Ƭ��
  if logic_send.QueueCmdCount = 0 then
    // GetFileFragmentDataMΪԭ�ӹ���,ʵ����Զ���ļ���������Ƭ����
      logic.GetFileFragmentDataM(p^.FileName, p^.StartPos, p^.EndPos, p, nil, Download_Backcall)
  else
      queue.Add(p);
end;

constructor TSessionList.Create;
begin
  inherited Create;
  id := 0;
end;

destructor TSessionList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSessionList.Remove(obj: TP2PVM_Session);
begin
  DisposeObject(obj);
  inherited Remove(obj);
end;

procedure TSessionList.Delete(index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TSessionList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(Items[i]);
  inherited Clear;
end;

procedure TSessionList.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
end;

function TSessionList.Pick_NextSession: TP2PVM_Session;
begin
  if Count = 0 then
      exit(nil);

  if id >= Count then
      id := 0;

  Result := Items[id];
  Inc(id);
end;

procedure TFragmentClientForm.checkDownTimerTimer(Sender: TObject);
var
  i: Integer;
begin
  if (DownloadTask.Count > 0) and (DownloadTask.Done) then
    begin
      DoStatus('�����Ƭ��������.');

      // �ϲ���Ƭ
      for i := 0 to DownloadTask.Count - 1 do
        begin
          targetStream.Position := DownloadTask[i]^.StartPos;
          targetStream.CopyFrom(DownloadTask[i]^.Data, DownloadTask[i]^.Data.Size);
        end;

      DoStatus(demoFile + ' �����ļ�md5: ' + umlStreamMD5String(targetStream));
      DownloadTask.Clear;
    end;

  stateMemo.Lines.BeginUpdate;
  stateMemo.Lines.Clear;
  for i := 0 to Session.Count - 1 do
      stateMemo.Lines.Add(Format('����%d ����:%d ��������:%d ��������:%d', [i + 1, Session[i].total,
      Session[i].phyCli.Statistics[stReceiveSize], Session[i].phyCli.Statistics[stSendSize]]));
  stateMemo.Lines.EndUpdate;
end;

procedure TFragmentClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatus_Backcall);
  Session := TSessionList.Create;
  DownloadTask := TDownloadTask.Create;
  targetStream := TStream64.Create;
end;

procedure TFragmentClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(Session);
  DisposeObject(DownloadTask);
  DisposeObject(targetStream);
end;

procedure TFragmentClientForm.downloadButtonClick(Sender: TObject);
begin
  if Session.Count = 0 then
      exit;

  // 1,�Ȼ�ȡԶ���ļ���С
  Session.First.logic.GetFileInfoP(demoFile, nil, nil,
    procedure(const UserData: Pointer; const UserObject: TCore_Object;
      const FileName: SystemString; const Existed: Boolean; const fSiz: Int64)
    var
      i: Integer;
      step: Int64;
      p: PTaskData;
    begin
      if not Existed then
          exit;

      targetStream.Size := fSiz;

      // 2,����������������
      DownloadTask.Clear;
      step := 0;
      while step + bloackSize < fSiz do
        begin
          new(p);
          p^.Init;
          p^.FileName := FileName;
          p^.StartPos := step;
          p^.EndPos := step + bloackSize;
          DownloadTask.Add(p);
          Inc(step, bloackSize);
        end;
      if step < fSiz then
        begin
          new(p);
          p^.Init;
          p^.FileName := FileName;
          p^.StartPos := step;
          p^.EndPos := fSiz;
          DownloadTask.Add(p);
        end;

      // 3,����Զ���ļ���md5������֤������ȷ��
      Session.First.logic.GetFileMD5P(FileName, 0, fSiz, nil, nil,
        procedure(const UserData: Pointer; const UserObject: TCore_Object;
          const FileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5)
        begin
          DoStatus(FileName + ' Զ���ļ�md5: ' + umlMD5ToStr(MD5));
        end);

      // 4,��������ӵ����ض���
      for i := 0 to DownloadTask.Count - 1 do
          Session.Pick_NextSession.Download(DownloadTask[i]);
    end);
end;

procedure TFragmentClientForm.connButtonClick(Sender: TObject);
var
  i: Integer;
  p: TP2PVM_Session;
begin
  for i := 0 to 10 - 1 do
    begin
      p := TP2PVM_Session.Create;
      p.Connect(HostEdit.Text);
      Session.Add(p);
    end;
end;

procedure TFragmentClientForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  DeleteDoStatusHook(Self);
end;

procedure TFragmentClientForm.Timer1Timer(Sender: TObject);
begin
  CheckThreadSynchronize(10);
  Session.Progress;
end;

procedure TFragmentClientForm.DoStatus_Backcall(Text_: SystemString; const id: Integer);
begin
  Memo.Lines.Add(Text_);
end;

end.
