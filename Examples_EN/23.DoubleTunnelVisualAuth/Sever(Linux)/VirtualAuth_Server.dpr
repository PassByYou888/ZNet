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
  {  During the process of accessing other servers, the user we are waiting for verification may have been disconnected, so we need to make a judgment  }
  if not AuthIO.Online then
    begin
      AuthIO.Bye; {  Byte in TVirtualAuthIO is equivalent to Free. If we do not Byte, it will cause memory leakage  }
      exit;
    end;

  {  The Accept and Reject methods in TVirtualAuthIO can only be called once and will be automatically released upon completion  }
  if SameText(AuthIO.UserID, 'Test') and SameText(AuthIO.Passwd, 'Test') then
      AuthIO.Accept {  Accept user login  }
  else
      AuthIO.Reject; {  Deny user login  }
end;

procedure TMyService.UserAuth(Sender: TVirtualAuthIO); {  TVirtualAuthIO has two working modes  }
begin
  inherited UserAuth(Sender);

  {  The first method is to immediately authenticate and directly write the implementation below  }
  {  if SameText(Sender.UserID, 'Test') and SameText(Sender.Passwd, 'Test') then
Sender. Accept / / accept user login
else
Sender.Reject; //  Deny user login  }
  {  The second working mode is delayed authentication. In the second mode, we don't need to do anything and save the instance of TVirtualAuthIO. Once the remote authentication is completed, we will provide feedback to the client  }
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
