unit DCliFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.Net,
  Z.Status, Z.Core,
  Z.Net.Client.CrossSocket,
  Z.Net.Client.ICS, Z.Net.Client.Indy,
  Z.Cadencer, Z.DFE, Z.Net.DoubleTunnelIO;

type
  TAuthDoubleTunnelClientForm = class(TForm)
    Memo1: TMemo;
    ConnectButton: TButton;
    HostEdit: TLabeledEdit;
    Timer1: TTimer;
    HelloWorldBtn: TButton;
    UserEdit: TLabeledEdit;
    PasswdEdit: TLabeledEdit;
    RegUserButton: TButton;
    AsyncConnectButton: TButton;
    TimeLabel: TLabel;
    fixedTimeButton: TButton;
    connectTunnelButton: TButton;
    Button1: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure HelloWorldBtnClick(Sender: TObject);
    procedure AsyncConnectButtonClick(Sender: TObject);
    procedure fixedTimeButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RegUserButtonClick(Sender: TObject);
    procedure connectTunnelButtonClick(Sender: TObject);
  private
    procedure DoStatusNear(AText: string; const ID: Integer);
    procedure cmd_ChangeCaption(Sender: TPeerClient; InData: TDFE);
    procedure cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDFE);
  public
    {  VM tunnel  }
    {  The VM tunnel can work autonomously while also carrying the entire protocol stack  }
    {  Here, we will bind recvtunnel + sendtunnel in vmtunnel at the same time, and use only one link to implement dual channel service  }
    {  VM tunnel can be any socket, bedstead, Indy, ICs, crosssocket, etc. all support VM tunnel  }
    VMTunnel: TZNet_Client;

    {  ZS Normal Communication Framework  }
    RecvTunnel: TZNet_WithP2PVM_Client;
    SendTunnel: TZNet_WithP2PVM_Client;
    client: TZNet_DoubleTunnelClient;
  end;

var
  AuthDoubleTunnelClientForm: TAuthDoubleTunnelClientForm;

implementation

{$R *.dfm}


procedure TAuthDoubleTunnelClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TAuthDoubleTunnelClientForm.fixedTimeButtonClick(Sender: TObject);
begin
  {  High speed synchronization without progress trigger  }
  {  This is done to minimize the delay rate of time  }
  client.SendTunnel.SyncOnResult := True;
  client.SyncCadencer;
  client.SendTunnel.WaitP(1000, procedure(const cState: Boolean)
    begin
      {  Because after synconresult is opened, nested deadlocks will occur in anonymous functions  }
      {  We now turn it off to ensure nested execution of anonymous functions  }
      client.SendTunnel.SyncOnResult := False;
    end);
end;

procedure TAuthDoubleTunnelClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  {  VM tunnel  }
  {  The VM tunnel can work autonomously while also carrying the entire protocol stack  }
  {  Here, we will bind recvtunnel + sendtunnel in vmtunnel at the same time, and use only one link to implement dual channel service  }
  VMTunnel := TZNet_Client_CrossSocket.Create;

  {  ZS Normal Communication Framework  }
  RecvTunnel := TZNet_WithP2PVM_Client.Create;
  SendTunnel := TZNet_WithP2PVM_Client.Create;
  client := TZNet_DoubleTunnelClient.Create(RecvTunnel, SendTunnel);

  client.RegisterCommand;

  {  Registering communication instructions that can be initiated by the server  }
  client.RecvTunnel.RegisterDirectStream('ChangeCaption').OnExecute := cmd_ChangeCaption;
  client.RecvTunnel.RegisterStream('GetClientValue').OnExecute := cmd_GetClientValue;
end;

procedure TAuthDoubleTunnelClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TAuthDoubleTunnelClientForm.HelloWorldBtnClick(Sender: TObject);
var
  SendDe, ResultDE: TDFE;
begin
  {  Send a console form hello world instruction to the server  }
  client.SendTunnel.SendDirectConsoleCmd('helloWorld_Console', '');

  {  Send a hello world instruction in the form of stream to the server  }
  SendDe := TDFE.Create;
  SendDe.WriteString('directstream 123456');
  client.SendTunnel.SendDirectStreamCmd('helloWorld_Stream', SendDe);
  DisposeObject([SendDe]);

  {  Asynchronous sending and receiving Stream instructions, feedback triggered by proc callback  }
  SendDe := TDFE.Create;
  SendDe.WriteString('123456');
  client.SendTunnel.SendStreamCmdP('helloWorld_Stream_Result', SendDe,
    procedure(Sender: TPeerClient; ResultData: TDFE)
    begin
      if ResultData.Count > 0 then
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
    end);
  DisposeObject([SendDe]);

  {  Blocking sending and receiving Stream instructions  }
  SendDe := TDFE.Create;
  ResultDE := TDFE.Create;
  SendDe.WriteString('123456');
  client.SendTunnel.WaitSendStreamCmd('helloWorld_Stream_Result', SendDe, ResultDE, 5000);
  if ResultDE.Count > 0 then
      DoStatus('server response:%s', [ResultDE.Reader.ReadString]);
  DisposeObject([SendDe, ResultDE]);
end;

procedure TAuthDoubleTunnelClientForm.RegUserButtonClick(Sender: TObject);
begin
  SendTunnel.Connect('::', 2);
  RecvTunnel.Connect('::', 1);

  client.RegisterUserP(UserEdit.Text, PasswdEdit.Text, procedure(const rState: Boolean)
    begin
      client.Disconnect;
    end);
end;

procedure TAuthDoubleTunnelClientForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  VMTunnel.Progress;
  client.Progress;
  TimeLabel.Caption := Format('sync time:%f', [client.CadencerEngine.UpdateCurrentTime]);
end;

procedure TAuthDoubleTunnelClientForm.cmd_ChangeCaption(Sender: TPeerClient; InData: TDFE);
begin
  Caption := InData.Reader.ReadString;
end;

procedure TAuthDoubleTunnelClientForm.cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDFE);
begin
  OutData.WriteString('getclientvalue:abc');
end;

procedure TAuthDoubleTunnelClientForm.ConnectButtonClick(Sender: TObject);
begin
  client.Disconnect;

  SendTunnel.Connect('::', 2);
  RecvTunnel.Connect('::', 1);

  {  Check if both channels have been successfully linked and ensure that initialization work such as symmetric encryption has been completed  }
  while (not client.RemoteInited) and (client.Connected) do
    begin
      TThread.Sleep(10);
      client.Progress;
    end;

  if client.Connected then
    begin
      {  Nested anonymous function support  }
      client.UserLoginP(UserEdit.Text, PasswdEdit.Text,
        procedure(const State: Boolean)
        begin
          if State then
              client.TunnelLinkP(
              procedure(const State: Boolean)
              begin
                DoStatus('double tunnel link success!');
              end)
        end);
    end;
end;

procedure TAuthDoubleTunnelClientForm.connectTunnelButtonClick(Sender: TObject);
begin
  VMTunnel.AsyncConnectP(HostEdit.Text, 9899, procedure(const cState: Boolean)
    begin
      if cState then
          VMTunnel.ClientIO.BuildP2PAuthTokenP(procedure
          begin
            VMTunnel.ClientIO.OpenP2PVMTunnelP(True, '', procedure(const VMauthState: Boolean)
              begin
                if VMauthState then
                  begin
                    {  Bind the client framework to the tunnel  }
                    {  There are two clients here. We bind them  }
                    VMTunnel.ClientIO.p2pVMTunnel.InstallLogicFramework(SendTunnel);
                    VMTunnel.ClientIO.p2pVMTunnel.InstallLogicFramework(RecvTunnel);
                  end;
              end);
          end);
    end);
end;

procedure TAuthDoubleTunnelClientForm.AsyncConnectButtonClick(Sender: TObject);
begin
  {  Asynchronous dual channel link  }
  client.AsyncConnectP('::', 1, 2,
    procedure(const cState: Boolean)
    begin
      if cState then
        begin
          DoStatus('connected success!');
          {  Nested anonymous function support  }
          client.UserLoginP(UserEdit.Text, PasswdEdit.Text,
            procedure(const lState: Boolean)
            begin
              if lState then
                begin
                  DoStatus('login successed!');
                  client.TunnelLinkP(
                    procedure(const tState: Boolean)
                    begin
                      if tState then
                          DoStatus('double tunnel link success!')
                      else
                          DoStatus('double tunnel link failed!');
                    end)
                end
              else
                begin
                  DoStatus('login failed!');
                end;
            end);
        end
      else
        begin
          DoStatus('connected failed!');
        end;
    end);

end;

procedure TAuthDoubleTunnelClientForm.Button1Click(Sender: TObject);
begin
  VMTunnel.Disconnect;
end;

end.
