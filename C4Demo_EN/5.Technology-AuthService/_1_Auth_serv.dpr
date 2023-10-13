program _1_Auth_serv;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Net,
  Z.Net.DoubleTunnelIO.VirtualAuth,
  Z.Status,
  Z.Net.PhysicsIO,
  Z.Json,
  Z.Net.C4,
  Z.Net.C4_Console_APP;

const
  {  The public network address of the dispatching server port, which can be IPv4, IPv6 or DNS  }
  {  Public address, cannot be given to a type of 127.0.0.1  }
  Internet_DP_Addr_ = '127.0.0.1';
  {  Scheduling Server Port  }
  Internet_DP_Port_ = 8387;

type
  {  Minimal authentication service  }
  TMyVA_Service = class(TC40_Base_VirtualAuth_Service)
  protected
    procedure DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO); override;
    procedure DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO); override;
  public
    {  Simply open a JSON memory database, and the user stores the user authentication password  }
    UserJson: TZJ;
    UserJsonFileName: U_String;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
  end;

procedure TMyVA_Service.DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO);
begin
  if UserJson.IndexOf(RegIO.UserID) < 0 then
    begin
      UserJson.S[RegIO.UserID] := RegIO.Passwd;
      RegIO.Accept;
      DoStatus('Successfully registered user '#39'%s'#39, [RegIO.UserID]);
      UserJson.SaveToFile(UserJsonFileName);
    end
  else
    begin
      DoStatus('Duplicate registered user name '#39'%s'#39, [RegIO.UserID]);
      RegIO.Reject;
    end;
end;

procedure TMyVA_Service.DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO);
begin
  if (UserJson.IndexOf(AuthIO.UserID) >= 0) and umlSameText(UserJson.S[AuthIO.UserID], AuthIO.Passwd) then
    begin
      AuthIO.Accept;
      DoStatus('User authentication successful '#39'%s'#39, [AuthIO.UserID]);
    end
  else
    begin
      AuthIO.Reject;
      DoStatus('User authentication failed '#39'%s'#39, [AuthIO.UserID]);
    end;
end;

constructor TMyVA_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  UserJson := TZJ.Create;
  UserJsonFileName := umlCombineFileName(DTVirtualAuthService.PublicFileDirectory, 'user.json');
  if umlFileExists(UserJsonFileName) then
      UserJson.LoadFromFile(UserJsonFileName);
end;

destructor TMyVA_Service.Destroy;
begin
  DisposeObject(UserJson);
  inherited Destroy;
end;

begin
  {  In one sentence, summarize the automatic authentication network. After passing the first authentication, start the automatic network  }

  RegisterC40('MyVA', TMyVA_Service, TC40_Base_VirtualAuth_Client);
  {  Open Log Information  }
  Z.Net.C4.C40_QuietMode := False;

  {  VirtualAuth dual channel service with authentication mechanism in C4 network  }
  {  VirtualAuth can work in non validation environments  }
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_DP_Addr_, Internet_DP_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('MyVA');
      StartService;
    end;

  {  Main loop  }
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
