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
  // 调度服务器端口公网地址,可以是ipv4,ipv6,dns
  // 公共地址,不能给127.0.0.1这类
  Internet_DP_Addr_ = '127.0.0.1';
  // 调度服务器端口
  Internet_DP_Port_ = 8387;

type
  // 极简验证服务
  TMyVA_Service = class(TC40_Base_VirtualAuth_Service)
  protected
    procedure DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO); override;
    procedure DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO); override;
  public
    // 简单开个json内存数据库，用户存放用户验证密码
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
      DoStatus('注册用户成功 "%s"', [RegIO.UserID]);
      UserJson.SaveToFile(UserJsonFileName);
    end
  else
    begin
      DoStatus('注册用户名重复 "%s"', [RegIO.UserID]);
      RegIO.Reject;
    end;
end;

procedure TMyVA_Service.DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO);
begin
  if (UserJson.IndexOf(AuthIO.UserID) >= 0) and umlSameText(UserJson.S[AuthIO.UserID], AuthIO.Passwd) then
    begin
      AuthIO.Accept;
      DoStatus('用户身份验证成功 "%s"', [AuthIO.UserID]);
    end
  else
    begin
      AuthIO.Reject;
      DoStatus('用户身份验证失败 "%s"', [AuthIO.UserID]);
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
  // 一句话总结自动化验证网络，通过首次身份验证后，开启自动化网络

  RegisterC40('MyVA', TMyVA_Service, TC40_Base_VirtualAuth_Client);
  // 打开Log信息
  Z.Net.C4.C40_QuietMode := False;

  // VirtualAuth在C4网络中带有身份验证机制的双通道服务
  // VirtualAuth可以工作在非验证环境
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_DP_Addr_, Internet_DP_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('MyVA');
      StartService;
    end;

  // 主循环
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
