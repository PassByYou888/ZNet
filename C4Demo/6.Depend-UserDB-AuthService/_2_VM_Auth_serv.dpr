program _2_VM_Auth_serv;

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
  Z.Notify,
  Z.Net.PhysicsIO,
  Z.Net.C4, Z.Net.C4_UserDB,
  Z.Net.C4_Console_APP;

const
  // ���ȷ������˿ڹ�����ַ,������ipv4,ipv6,dns
  // ������ַ,���ܸ�127.0.0.1����
  Internet_DP_Addr_ = '127.0.0.1';
  // ���ȷ������˿�
  Internet_DP_Port_ = 8387;

  // ���ط�����������ַ
  Internet_LocalService_Addr_ = '127.0.0.1';
  Internet_LocalService_Port_ = 8386;

function Get_UserDB_Client: TC40_UserDB_Client;
begin
  Result := TC40_UserDB_Client(C40_ClientPool.ExistsConnectedServiceTyp('UserDB'));
end;

type
  // VM����
  TMyVA_Service = class(TC40_Base_VirtualAuth_Service)
  protected
    procedure DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO); override;
    procedure DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO); override;
  public
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
  end;

type
  TTemp_Reg_Class = class
  public
    RegIO: TVirtualRegIO;
    procedure Do_Usr_Reg(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
  end;

procedure TTemp_Reg_Class.Do_Usr_Reg(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
begin
  if State_ then
      RegIO.Accept
  else
      RegIO.Reject;
  DelayFreeObj(1.0, self);
end;

procedure TMyVA_Service.DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO);
var
  tmp: TTemp_Reg_Class;
begin
  if Get_UserDB_Client = nil then
    begin
      RegIO.Reject;
      exit;
    end;
  // �������temp����Ϊ�¼����壬���¼�ָ��userdb�ķ���
  tmp := TTemp_Reg_Class.Create;
  tmp.RegIO := RegIO;
  Get_UserDB_Client.Usr_RegM(RegIO.UserID, RegIO.Passwd, tmp.Do_Usr_Reg);
end;

type
  TTemp_Auth_Class = class
  public
    AuthIO: TVirtualAuthIO;
    procedure Do_Usr_Auth(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
  end;

procedure TTemp_Auth_Class.Do_Usr_Auth(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
begin
  if State_ then
      AuthIO.Accept
  else
      AuthIO.Reject;
  DelayFreeObj(1.0, self);
end;

procedure TMyVA_Service.DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO);
var
  tmp: TTemp_Auth_Class;
begin
  if Get_UserDB_Client = nil then
    begin
      AuthIO.Reject;
      exit;
    end;
  // �������temp����Ϊ�¼����壬���¼�ָ��userdb�ķ���
  tmp := TTemp_Auth_Class.Create;
  tmp.AuthIO := AuthIO;
  Get_UserDB_Client.Usr_AuthM(AuthIO.UserID, AuthIO.Passwd, tmp.Do_Usr_Auth);
end;

constructor TMyVA_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
end;

destructor TMyVA_Service.Destroy;
begin
  inherited Destroy;
end;

begin
  // �����������Է���userDB�����û�ע�����֤ʱ��ͨ������userDBʵ�֣����������ɶ࿪���൱��VM�������߸���

  // ע��MyVA
  RegisterC40('MyVA', TMyVA_Service, TC40_Base_VirtualAuth_Client);

  // ��Log��Ϣ
  Z.Net.C4.C40_QuietMode := False;
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'DP|UserDB', nil);
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_LocalService_Addr_, Internet_LocalService_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('MyVA');
      StartService;
    end;

  // ��ѭ��
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
