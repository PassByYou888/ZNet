unit FMXAuthDoubleTunnelCliFrm;


interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,

  Z.Net.Client.Indy, Z.DFE,
  Z.Net, Z.Core, Z.Status,
  Z.Net.DoubleTunnelIO, FMX.Memo.Types;

type
  TFMXAuthDoubleClientForm = class(TForm)
    Memo1: TMemo;
    connectButton: TButton;
    HostEdit: TEdit;
    HelloWorldBtn: TButton;
    Timer1: TTimer;
    UserEdit: TEdit;
    PasswdEdit: TEdit;
    RegUserButton: TButton;
    AsyncButton: TButton;
    timeLabel: TLabel;
    FixedTimeButton: TButton;
    connectTunnelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure HelloWorldBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure RegUserButtonClick(Sender: TObject);
    procedure AsyncButtonClick(Sender: TObject);
    procedure FixedTimeButtonClick(Sender: TObject);
    procedure connectTunnelButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);

    procedure cmd_ChangeCaption(Sender: TPeerClient; InData: TDFE);
    procedure cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDFE);
  public
    { Public declarations }
    // vm隧道
    // vm隧道可以在自生正常工作中，同时带起整个协议栈的工作
    // 我们在这里会将RecvTunnel+SendTunnel同时绑定在VMTunnel中，只用一个链接实现双通道服务
    // vm隧道可以是任何socket床架，indy，ics，crossSocket，等等均支持vm隧道
    VMTunnel: TZNet_Client_Indy;

    // zs正常的通讯框架
    RecvTunnel: TZNet_WithP2PVM_Client;
    SendTunnel: TZNet_WithP2PVM_Client;
    client: TZNet_DoubleTunnelClient;
  end;

var
  FMXAuthDoubleClientForm: TFMXAuthDoubleClientForm;

implementation

{$R *.fmx}

{ TFMXClientForm }

procedure TFMXAuthDoubleClientForm.cmd_ChangeCaption(Sender: TPeerClient; InData: TDFE);
begin
  DoStatus('Change Caption:%s', [InData.Reader.ReadString]);
end;

procedure TFMXAuthDoubleClientForm.cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDFE);
begin
  OutData.WriteString('getclientvalue:abc');
end;

procedure TFMXAuthDoubleClientForm.connectButtonClick(Sender: TObject);
begin
  SendTunnel.Connect('::', 2);
  RecvTunnel.Connect('::', 1);

  // 检查双通道是否都已经成功链接，确保完成了对称加密等等初始化工作
  while (not client.RemoteInited) and (client.Connected) do
    begin
      TThread.Sleep(10);
      client.Progress;
    end;

  if client.Connected then
    begin
      // 嵌套式匿名函数支持
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

procedure TFMXAuthDoubleClientForm.connectTunnelButtonClick(
  Sender: TObject);
begin
  if not VMTunnel.Connect(HostEdit.Text, 9899) then
      exit;

  VMTunnel.ClientIO.BuildP2PAuthTokenP(procedure
    begin
      VMTunnel.ClientIO.OpenP2PVMTunnelP(True, '', procedure(const VMauthState: Boolean)
        begin
          // 如果VM隧道握手成功
          if VMauthState then
            begin
              // 将客户端框架绑定到隧道中
              // 这里有两个客户端，我们都绑定进去
              VMTunnel.ClientIO.p2pVMTunnel.InstallLogicFramework(SendTunnel);
              VMTunnel.ClientIO.p2pVMTunnel.InstallLogicFramework(RecvTunnel);
            end
        end);
    end);
end;

procedure TFMXAuthDoubleClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TFMXAuthDoubleClientForm.FixedTimeButtonClick(Sender: TObject);
begin
  // 高速同步，不经过Progress触发
  // 这样干是将时间的延迟率降低到最小
  client.SendTunnel.SyncOnResult := True;
  client.SyncCadencer;
  client.SendTunnel.WaitP(1000, procedure(const cState: Boolean)
    begin
      // 因为打开了SyncOnResult后，匿名函数会出现嵌套死锁
      // 我们现在关闭它，以保证匿名函数的嵌套执行
      client.SendTunnel.SyncOnResult := False;
    end);
end;

procedure TFMXAuthDoubleClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  // vm隧道
  // vm隧道可以在自生正常工作中，同时带起整个协议栈的工作
  // 我们在这里会将RecvTunnel+SendTunnel同时绑定在VMTunnel中，只用一个链接实现双通道服务
  VMTunnel := TZNet_Client_Indy.Create;

  // zs正常的通讯框架
  RecvTunnel := TZNet_WithP2PVM_Client.Create;
  SendTunnel := TZNet_WithP2PVM_Client.Create;
  client := TZNet_DoubleTunnelClient.Create(RecvTunnel, SendTunnel);

  client.RegisterCommand;

  // 注册可以由服务器发起的通讯指令
  client.RecvTunnel.RegisterDirectStream('ChangeCaption').OnExecute := cmd_ChangeCaption;
  client.RecvTunnel.RegisterStream('GetClientValue').OnExecute := cmd_GetClientValue;
end;

procedure TFMXAuthDoubleClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TFMXAuthDoubleClientForm.HelloWorldBtnClick(Sender: TObject);
var
  SendDe, ResultDE: TDFE;
begin
  // 往服务器发送一条console形式的hello world指令
  client.SendTunnel.SendDirectConsoleCmd('helloWorld_Console', '');

  // 往服务器发送一条stream形式的hello world指令
  SendDe := TDFE.Create;
  SendDe.WriteString('directstream 123456');
  client.SendTunnel.SendDirectStreamCmd('helloWorld_Stream', SendDe);
  DisposeObject([SendDe]);

  // 异步方式发送，并且接收Stream指令，反馈以proc回调触发
  SendDe := TDFE.Create;
  SendDe.WriteString('123456');
  client.SendTunnel.SendStreamCmdP('helloWorld_Stream_Result', SendDe,
    procedure(Sender: TPeerClient; ResultData: TDFE)
    begin
      if ResultData.Count > 0 then
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
    end);
  DisposeObject([SendDe]);

  // 阻塞方式发送，并且接收Stream指令
  SendDe := TDFE.Create;
  ResultDE := TDFE.Create;
  SendDe.WriteString('123456');
  client.SendTunnel.WaitSendStreamCmd('helloWorld_Stream_Result', SendDe, ResultDE, 5000);
  if ResultDE.Count > 0 then
      DoStatus('server response:%s', [ResultDE.Reader.ReadString]);
  DisposeObject([SendDe, ResultDE]);
end;

procedure TFMXAuthDoubleClientForm.RegUserButtonClick(Sender: TObject);
begin
  SendTunnel.Connect('::', 2);
  RecvTunnel.Connect('::', 1);

  client.RegisterUserP(UserEdit.Text, PasswdEdit.Text, procedure(const rState: Boolean)
    begin
      client.Disconnect;
    end);
end;

procedure TFMXAuthDoubleClientForm.AsyncButtonClick(Sender: TObject);
begin
  // 异步式双通道链接
  client.AsyncConnectP('::', 1, 2,
    procedure(const cState: Boolean)
    begin
      if cState then
        begin
          DoStatus('connected success!');
          // 嵌套式匿名函数支持
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

procedure TFMXAuthDoubleClientForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  VMTunnel.Progress;
  client.Progress;
  timeLabel.Text := Format('sync time:%f', [client.CadencerEngine.UpdateCurrentTime]);
end;

end.
