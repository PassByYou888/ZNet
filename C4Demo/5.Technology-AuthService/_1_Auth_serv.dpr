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
  // ���ȷ������˿ڹ�����ַ,������ipv4,ipv6,dns
  // ������ַ,���ܸ�127.0.0.1����
  Internet_DP_Addr_ = '127.0.0.1';
  // ���ȷ������˿�
  Internet_DP_Port_ = 8387;

type
  // ������֤����
  TMyVA_Service = class(TC40_Base_VirtualAuth_Service)
  protected
    procedure DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO); override;
    procedure DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO); override;
  public
    // �򵥿���json�ڴ����ݿ⣬�û�����û���֤����
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
      DoStatus('ע���û��ɹ� "%s"', [RegIO.UserID]);
      UserJson.SaveToFile(UserJsonFileName);
    end
  else
    begin
      DoStatus('ע���û����ظ� "%s"', [RegIO.UserID]);
      RegIO.Reject;
    end;
end;

procedure TMyVA_Service.DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO);
begin
  if (UserJson.IndexOf(AuthIO.UserID) >= 0) and umlSameText(UserJson.S[AuthIO.UserID], AuthIO.Passwd) then
    begin
      AuthIO.Accept;
      DoStatus('�û������֤�ɹ� "%s"', [AuthIO.UserID]);
    end
  else
    begin
      AuthIO.Reject;
      DoStatus('�û������֤ʧ�� "%s"', [AuthIO.UserID]);
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
  // һ�仰�ܽ��Զ�����֤���磬ͨ���״������֤�󣬿����Զ�������

  RegisterC40('MyVA', TMyVA_Service, TC40_Base_VirtualAuth_Client);
  // ��Log��Ϣ
  Z.Net.C4.C40_QuietMode := False;

  // VirtualAuth��C4�����д��������֤���Ƶ�˫ͨ������
  // VirtualAuth���Թ����ڷ���֤����
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_DP_Addr_, Internet_DP_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('MyVA');
      StartService;
    end;

  // ��ѭ��
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
