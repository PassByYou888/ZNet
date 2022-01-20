program EZLinuxServ;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  System.Classes,
  Z.Net,
  Z.Net.Server.Indy,
  Z.Net.Server.CrossSocket,
  Z.Status, Z.Core,
  Z.DFE, Z.UnicodeMixedLib, Z.MemoryStream;

(*
  ����ʵ�ʲ��ԣ�Linux��indy��ϵͳԭ����������ס�̣߳������ٽ���ģ��ԭ�������ڴ����ϼ��ɽ������
  ����ʵ�ʲ��ԣ�Linux��crossSocket�ĸ߲��������������޸�, ����ϸ�ںͻ�������������CommunicationFramework_Server_CrossSocket.pas��д��

  EzLinuxServ����ʹ��delphi xe10.1.2����д
  �����delphi xe10.2.2�����ϰ汾�����������ƽ̨��������Ҳ���linux�����½�һ��console���̣������븴�ƹ�ȥ����
*)

type
  TMySpecialDefine = class(TPeerClientUserSpecial)
  public
    tempStream: TMemoryStream64;
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TMyServer = class(TZNet_Server_CrossSocket)
  private
    procedure cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
    procedure cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
    procedure cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);

    procedure cmd_TestMiniStream(Sender: TPeerClient; InData: TDataFrameEngine);

    procedure cmd_Test128MBigStream(Sender: TPeerClient; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64);

    procedure cmd_TestCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
  public
  end;

constructor TMySpecialDefine.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  tempStream := TMemoryStream64.Create;
  DoStatus('%s connected', [Owner.PeerIP]);
end;

destructor TMySpecialDefine.Destroy;
begin
  DoStatus('%s disconnect', [Owner.PeerIP]);
  DisposeObject(tempStream);
  inherited Destroy;
end;

procedure TMyServer.cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
begin
  DoStatus('client: %s', [InData]);
end;

procedure TMyServer.cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
begin
  DoStatus('client: %s', [InData.Reader.ReadString]);
end;

procedure TMyServer.cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  OutData.WriteString('result 654321');
end;

procedure TMyServer.cmd_TestMiniStream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  InData.Reader.ReadStream(ms);

  DoStatus(umlMD5Char(ms.Memory, ms.Size).Text);

  DisposeObject(ms);
end;

procedure TMyServer.cmd_Test128MBigStream(Sender: TPeerClient; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  tempStream: TMemoryStream64;
begin
  tempStream := TMySpecialDefine(Sender.UserSpecial).tempStream;
  tempStream.CopyFrom(InData, InData.Size);

  // bigstream complete
  if tempStream.Size = BigStreamTotal then
    begin
      Sender.Print('bigsteram finish');
      Sender.Print('bigsteram md5:' + umlMD5Char(tempStream.Memory, tempStream.Size).Text);
      tempStream.Clear;
    end;
end;

procedure TMyServer.cmd_TestCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
begin
  Sender.Print('Complete buffer md5: %s', [umlMD5String(InData, DataSize).Text]);
end;

// ��ѭ��
var
  server: TMyServer;

begin
  server := TMyServer.Create;
  server.PeerClientUserSpecialClass := TMySpecialDefine;

  // �������completeBuffer������ֻ���ڲ��ԣ��������з�����������һ���4M�Ϳ�����
  server.MaxCompleteBufferSize := 128 * 1024 * 1024;

  server.RegisterDirectConsole('helloWorld_Console').OnExecute := server.cmd_helloWorld_Console;
  server.RegisterDirectStream('helloWorld_Stream').OnExecute := server.cmd_helloWorld_Stream;
  server.RegisterStream('helloWorld_Stream_Result').OnExecute := server.cmd_helloWorld_Stream_Result;
  server.RegisterDirectStream('TestMiniStream').OnExecute := server.cmd_TestMiniStream;
  server.RegisterBigStream('Test128MBigStream').OnExecute := server.cmd_Test128MBigStream;
  // ע��Completebufferָ��
  server.RegisterCompleteBuffer('TestCompleteBuffer').OnExecute := server.cmd_TestCompleteBuffer;

  if server.StartService('0.0.0.0', 9818) then
      DoStatus('start service success')
  else
      DoStatus('start service failed!');

  // ������ѭ��
  while true do
    begin
      server.Progress;

      // �ӳ�ͬ�����
      try
          Z.Core.CheckThreadSynchronize(10);
      except
      end;
    end;

end.
