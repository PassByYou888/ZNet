program VirtualAuth_Server;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core, Z.ListEngine, Z.UnicodeMixedLib, Z.Status,
  Z.DFE, Z.MemoryStream, Z.PascalStrings, Z.Cipher, Z.Notify, Z.Cadencer,
  Z.Net,
  Z.Net.DoubleTunnelIO.VirtualAuth,
  Z.Net.Server.CrossSocket;

type
  TMyService = class(TZNet_DoubleTunnelService_VirtualAuth)
  protected
    procedure UserAuth(Sender: TVirtualAuthIO); override;
  end;

procedure OtherServerReponse(Sender: TNPostExecute);
var
  AuthIO: TVirtualAuthIO;
begin
  AuthIO := TVirtualAuthIO(Sender.Data1);
  // �ڷ��������������Ĺ����У����ǵȴ���֤���û������Ѿ����ߣ����������Ҫ�ж�һ��
  if not AuthIO.Online then
    begin
      AuthIO.Bye; // TVirtualAuthIO�е�bye��ͬ��Free��������ǲ�Bye��������ڴ�й©
      exit;
    end;

  // TVirtualAuthIO�е�Accept��Reject����ֻ�ܱ�����һ�Σ���ɺ����ᱻ�Զ��ͷ�
  if SameText(AuthIO.UserID, 'Test') and SameText(AuthIO.Passwd, 'Test') then
      AuthIO.Accept // �����û���¼
  else
      AuthIO.Reject; // �ܾ��û���¼
end;

procedure TMyService.UserAuth(Sender: TVirtualAuthIO); // TVirtualAuthIO�����ֹ���ģʽ
begin
  inherited UserAuth(Sender);

  // ��һ���ǣ�������֤��ֱ��������д��ʵ��
  (*
    if SameText(Sender.UserID, 'Test') and SameText(Sender.Passwd, 'Test') then
    Sender.Accept // �����û���¼
    else
    Sender.Reject; // �ܾ��û���¼
  *)
  // �ڶ��ֹ���ģʽ���ӳ���֤���ڵڶ���ģʽ�У�����ʲô��������������TVirtualAuthIO��ʵ��������Զ��֤��ɣ������ٷ������ͻ���
  with ProgressEngine.PostExecuteC(3.0, OtherServerReponse) do
      Data1 := Sender;
end;

procedure RunServer;
var
  RecvTunnel, SendTunnel: TZNet_Server_CrossSocket;
  Service: TMyService;
begin
  RecvTunnel := TZNet_Server_CrossSocket.Create;
  SendTunnel := TZNet_Server_CrossSocket.Create;
  SendTunnel.StartService('0.0.0.0', 9816);
  RecvTunnel.StartService('0.0.0.0', 9815);
  if SendTunnel.StartedService and RecvTunnel.StartedService then
    begin
      Service := TMyService.Create(RecvTunnel, SendTunnel);
      Service.RegisterCommand;

      while True do
        begin
          TCore_Thread.Sleep(1);
          Service.Progress;
          try
              Z.Core.CheckThreadSynchronize;
          except
          end;
        end;
    end
  else
    begin
      DisposeObject([RecvTunnel, SendTunnel]);
    end;
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
      RunServer;
  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
