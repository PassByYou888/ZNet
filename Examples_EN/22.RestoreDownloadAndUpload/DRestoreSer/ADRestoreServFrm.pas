unit ADRestoreServFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.Net,
  Z.Net.Server.CrossSocket, Z.Status, Z.Core,
  Z.DFE, Z.Net.DoubleTunnelIO, Z.Net.DoubleTunnelIO.VirtualAuth;

type
  TAuthDoubleServerForm = class;

  TMyService = class(TZNet_DoubleTunnelService)
  private
    f: TAuthDoubleServerForm;
  protected
    procedure UserRegistedSuccess(UserID: string); override;
    procedure UserLinkSuccess(UserDefineIO: TService_RecvTunnel_UserDefine); override;
    procedure UserOut(UserDefineIO: TService_RecvTunnel_UserDefine); override;
  public
    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TAuthDoubleServerForm = class(TForm)
    Memo1: TMemo;
    StartServiceButton: TButton;
    Timer1: TTimer;
    procedure StartServiceButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    RecvTunnel: TZNet_Server_CrossSocket;
    SendTunnel: TZNet_Server_CrossSocket;
    Service   : TMyService;
  end;

var
  AuthDoubleServerForm: TAuthDoubleServerForm;

implementation

{$R *.dfm}


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

procedure TMyService.RegisterCommand;
begin
  inherited RegisterCommand;
end;

procedure TMyService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
end;

procedure TAuthDoubleServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TAuthDoubleServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  RecvTunnel := TZNet_Server_CrossSocket.Create;
  SendTunnel := TZNet_Server_CrossSocket.Create;
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
end;

procedure TAuthDoubleServerForm.FormDestroy(Sender: TObject);
begin
  disposeObject([RecvTunnel, SendTunnel, Service]);
  DeleteDoStatusHook(self);
end;

procedure TAuthDoubleServerForm.StartServiceButtonClick(Sender: TObject);
begin
  {  Based on the official ICS document, the binding Host interface cannot be empty, and IPV4 or IPV6 needs to be specified  }
  if SendTunnel.StartService('0.0.0.0', 9816) then
      DoStatus('listen send service success')
  else
      DoStatus('listen send service failed!');
  SendTunnel.IDCounter := 100;

  if RecvTunnel.StartService('0.0.0.0', 9815) then
      DoStatus('listen Recv service success')
  else
      DoStatus('listen Recv service failed!');

  Service.UnRegisterCommand;
  Service.RegisterCommand;
end;

procedure TAuthDoubleServerForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  Service.Progress;
end;

end.
