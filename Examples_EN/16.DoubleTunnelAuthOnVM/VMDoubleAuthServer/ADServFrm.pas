unit ADServFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.PascalStrings,
  Z.Net,
  Z.Net.Server.ICS,
  Z.Net.Server.Indy,
  Z.Net.Server.CrossSocket, Z.Status, Z.Core,
  Z.DFE, Z.Net.DoubleTunnelIO;

type
  TAuthDoubleServerForm = class;

  TMyVM_Tunnel = class(TZNet_Server_CrossSocket)
  public
    procedure p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean); override;
    {  When the VM tunnel was first created  }
    procedure p2pVMTunnelOpenBefore(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM); override;
    {  The tunnel has successfully shaken hands  }
    procedure p2pVMTunnelOpen(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM); override;
    {  Triggered by a delay of one second after a successful tunnel handshake  }
    procedure p2pVMTunnelOpenAfter(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM); override;
    {  The tunnel is closed. This event is triggered when a remote request or disconnection occurs  }
    procedure p2pVMTunnelClose(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM); override;
  end;

  TMyService = class(TZNet_DoubleTunnelService)
  private
    f: TAuthDoubleServerForm;
  protected
    procedure UserRegistedSuccess(UserID: string); override;
    procedure UserLinkSuccess(UserDefineIO: TService_RecvTunnel_UserDefine); override;
    procedure UserOut(UserDefineIO: TService_RecvTunnel_UserDefine); override;
  protected
    // reg cmd
    procedure cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
    procedure cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
    procedure cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
  public
    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TAuthDoubleServerForm = class(TForm)
    Memo1: TMemo;
    StartServiceButton: TButton;
    Timer1: TTimer;
    ChangeCaptionButton: TButton;
    GetClientValueButton: TButton;
    TimeLabel: TLabel;
    procedure StartServiceButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ChangeCaptionButtonClick(Sender: TObject);
    procedure GetClientValueButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }

    {  VM tunnel  }
    {  The VM tunnel can work normally while also carrying the entire protocol stack  }
    {  Here, we will bind recvtunnel + sendtunnel in vmtunnel at the same time, and use only one link to implement dual channel service  }
    VMTunnel: TMyVM_Tunnel;

    {  ZS Normal Communication Framework  }
    RecvTunnel: TZNet_WithP2PVM_Server;
    SendTunnel: TZNet_WithP2PVM_Server;
    Service: TMyService;
  end;

var
  AuthDoubleServerForm: TAuthDoubleServerForm;

implementation

{$R *.dfm}

procedure TMyVM_Tunnel.p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TMyVM_Tunnel.p2pVMTunnelClose(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM);
begin
  {  When this event is triggered, the vm tunnel has been released  }

  {  Restore binding  }
  Sender.p2pVM.UnInstallLogicFramework(AuthDoubleServerForm.RecvTunnel);
  Sender.p2pVM.UnInstallLogicFramework(AuthDoubleServerForm.SendTunnel);
  inherited p2pVMTunnelClose(Sender, p2pVMTunnel);
end;

procedure TMyVM_Tunnel.p2pVMTunnelOpenBefore(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM);
begin
  inherited;
  {  When this event is triggered, the VM tunnel has been established, but there is no handshake  }

  {  If the bound channel is a server type, it can be one to many  }
  {  One to many is the same server, which can be bound to multiple VM tunnels to realize VM virtual tunnel service  }
  {  The VM virtual tunnel service does not limit the number of links. A virtual tunnel can carry more than 1 million links  }

  {  Once the tunnel is established successfully, vmtunnel can also send and receive commands normally. The tunnel binding of recvtunnel + sendtunnel will not affect vmtunnel  }
  {  Once the tunnel is established successfully, the vmtunnel protocol will change. It is not a special case. Do not cancel the tunnel easily  }

  {  Bind the receive channel to the VM tunnel  }
  Sender.p2pVM.InstallLogicFramework(AuthDoubleServerForm.RecvTunnel);
  {  Bind the sending channel to the VM tunnel  }
  Sender.p2pVM.InstallLogicFramework(AuthDoubleServerForm.SendTunnel);
end;

procedure TMyVM_Tunnel.p2pVMTunnelOpen(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM);
begin
  inherited;
  {  When this event is triggered, the VM has successfully handshake  }
end;

procedure TMyVM_Tunnel.p2pVMTunnelOpenAfter(Sender: TPeerClient; p2pVMTunnel: TZNet_P2PVM);
begin
  inherited;
  {  When this event is triggered, the VM has successfully handshake and has passed for 1 second  }
end;

procedure TMyService.UserLinkSuccess(UserDefineIO: TService_RecvTunnel_UserDefine);
begin
  inherited UserLinkSuccess(UserDefineIO);
  DoStatus('user link success!');
end;

procedure TMyService.UserOut(UserDefineIO: TService_RecvTunnel_UserDefine);
begin
  inherited UserOut(UserDefineIO);
  DoStatus('user out!');
end;

procedure TMyService.UserRegistedSuccess(UserID: string);
begin
  inherited UserRegistedSuccess(UserID);
end;

procedure TMyService.cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
var
  UserIO: TService_RecvTunnel_UserDefine;
begin
  UserIO := GetUserDefineRecvTunnel(Sender);

  {  User not logged in successfully  }
  if not UserIO.LoginSuccessed then
      exit;
  {  Channel not merged  }
  if not UserIO.LinkOK then
      exit;

  DoStatus('client: %s', [InData]);
end;

procedure TMyService.cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  UserIO: TService_RecvTunnel_UserDefine;
begin
  UserIO := GetUserDefineRecvTunnel(Sender);

  {  User not logged in successfully  }
  if not UserIO.LoginSuccessed then
      exit;
  {  Channel not merged  }
  if not UserIO.LinkOK then
      exit;

  DoStatus('client: %s', [InData.Reader.ReadString]);
end;

procedure TMyService.cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserIO: TService_RecvTunnel_UserDefine;
begin
  UserIO := GetUserDefineRecvTunnel(Sender);

  {  User not logged in successfully  }
  if not UserIO.LoginSuccessed then
      exit;
  {  Channel not merged  }
  if not UserIO.LinkOK then
      exit;

  OutData.WriteString('result 654321');
end;

procedure TMyService.RegisterCommand;
begin
  inherited RegisterCommand;
  RecvTunnel.RegisterDirectConsole('helloWorld_Console').OnExecute := cmd_helloWorld_Console;
  RecvTunnel.RegisterDirectStream('helloWorld_Stream').OnExecute := cmd_helloWorld_Stream;
  RecvTunnel.RegisterStream('helloWorld_Stream_Result').OnExecute := cmd_helloWorld_Stream_Result;
end;

procedure TMyService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  RecvTunnel.UnRegisted('helloWorld_Console');
  RecvTunnel.UnRegisted('helloWorld_Stream');
  RecvTunnel.UnRegisted('helloWorld_Stream_Result');
end;

procedure TAuthDoubleServerForm.ChangeCaptionButtonClick(Sender: TObject);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString('change caption as hello World,from server!');
  {  The broadcast method does not distinguish whether the client has logged in or whether the dual channel has been successfully established  }
  SendTunnel.BroadcastDirectStreamCmd('ChangeCaption', de);
  disposeObject(de);
end;

procedure TAuthDoubleServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TAuthDoubleServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  VMTunnel := TMyVM_Tunnel.Create;

  RecvTunnel := TZNet_WithP2PVM_Server.Create;
  SendTunnel := TZNet_WithP2PVM_Server.Create;
  Service := TMyService.Create(RecvTunnel, SendTunnel);

  {  By default, TMyService does not save user information to UserDB, and it generates many useless directories every time it exits the server  }
  {  When we open allowsaveuserinfo, all user information will be permanently recorded  }
  {  Note: In the future, when we need to maintain the user database, we can only do it through programming. Managing files directly is anti human  }
  Service.AllowSaveUserInfo := True;

  {  When the dual channel server with authentication is started, the user database must be read manually. This step will use a lot of swap memory. If the size of userdb is 300m, 2G memory overhead is required for reading  }
  {  After the user logs in, in order to speed up the retrieval of user information, all user information is stored in memory. If the UserDB size is 300M, it will require approximately 1GB of memory overhead to run  }
  {  If there are too many users, such as over 100000, then the memory of the x86 platform is not enough, and you need x64  }
  {  LoadUserDB uses a high-speed hash table internally for searching, which reads very quickly but consumes a lot of memory  }
  Service.LoadUserDB;

  Service.f := self;
  Service.AllowRegisterNewUser := True;

  StartServiceButtonClick(nil);
end;

procedure TAuthDoubleServerForm.FormDestroy(Sender: TObject);
begin
  disposeObject([VMTunnel, RecvTunnel, SendTunnel, Service]);
  DeleteDoStatusHook(self);
end;

procedure TAuthDoubleServerForm.GetClientValueButtonClick(Sender: TObject);
begin
  SendTunnel.ProgressPeerIOP(procedure(PeerClient: TPeerClient)
    var
      c: TPeerClient;
      de: TDataFrameEngine;
    begin
      c := PeerClient;
      {  If the client fails to log in successfully  }
      if TService_SendTunnel_UserDefine(c.UserDefine).RecvTunnel = nil then
          exit;
      {  As listed above, if the client is not logged in  }
      if not TService_SendTunnel_UserDefine(c.UserDefine).RecvTunnel.LinkOK then
          exit;

      de := TDataFrameEngine.Create;
      de.WriteString('change caption as hello World,from server!');
      c.SendStreamCmdP('GetClientValue', de,
        procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
        begin
          if ResultData.Count > 0 then
              DoStatus('getClientValue [%s] response:%s', [c.GetPeerIP, ResultData.Reader.ReadString]);
        end);
      disposeObject(de);
    end);
end;

procedure TAuthDoubleServerForm.StartServiceButtonClick(Sender: TObject);
begin
  {  Based on the official crosssocket documentation, if the host interface is empty, bind all IPv6 + IPv4 IP addresses  }
  {  If the Host interface is 0.0.0.0, bind all IPV4 addresses,:: bind all IPV6 addresses  }
  if VMTunnel.StartService('', 9899) then
      DoStatus('VM tunnel port 9899 listening succeeded')
  else
      DoStatus('VM tunnel port 9899 failed to listen, system occupied');

  {  VM only supports address listening in ipv6 format, and VM listening does not affect the operating system. It is done in VM tunnels  }
  {  VM's tunnel service listening can listen to multiple at once  }
  {  We listen on ports 1 and 2 in the VM  }
  SendTunnel.StartService('::', 1);
  SendTunnel.StartService('::', 11);
  SendTunnel.StartService('::', 111);
  SendTunnel.StartService('::', 1111);
  SendTunnel.StartService('::33:ff', 1111);

  RecvTunnel.StartService('::', 2);
  RecvTunnel.StartService('::12:3e:87ef', 2);
  RecvTunnel.StartService('::4:ff08:e302', 2);

  Service.UnRegisterCommand;
  Service.RegisterCommand;
end;

procedure TAuthDoubleServerForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  VMTunnel.Progress;
  Service.Progress;
  TimeLabel.Caption := Format('sync time:%f', [Service.CadencerEngine.UpdateCurrentTime]);
end;

end.
