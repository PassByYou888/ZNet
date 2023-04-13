// 自动化依赖检测可以有效降低部署复杂性
// 自动化依赖是通过侦测已连接就绪的客户端类别来实现
unit C4_Auto_Deployment_IMP_Serv;

interface

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
  Z.Net.C4, Z.Net.C4_UserDB, Z.Net.C4_Log_DB;

type
  TAuto_Deployment_Service = class;

  TTemp_Reg_Class = class // 来自UserDB服务器的事件桥
  public
    Service: TAuto_Deployment_Service;
    RegIO: TVirtualRegIO;
    procedure Do_Usr_Reg(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
  end;

  TTemp_Auth_Class = class // 来自UserDB服务器的事件桥
  public
    Service: TAuto_Deployment_Service;
    AuthIO: TVirtualAuthIO;
    procedure Do_Usr_Auth(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
  end;

  TAuto_Deployment_Service = class(TC40_Base_VirtualAuth_Service)
  protected
    procedure DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO); override;
    procedure DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO); override;
  protected
    // 自动化检测并启用c4的依赖部署系统
    procedure Do_C40_Deployment_Ready(States: TC40_Custom_ClientPool_Wait_States);
  public
    Log_Client: TC40_Log_DB_Client;
    UserDB_Client: TC40_UserDB_Client;

    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
  end;

implementation

procedure TTemp_Reg_Class.Do_Usr_Reg(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
begin
  if State_ then
      RegIO.Accept
  else
      RegIO.Reject;
  DelayFreeObj(1.0, self);
  if Service.Log_Client <> nil then
      Service.Log_Client.PostLog('User_' + MakeNowDateStr, Format('User Register "%s" = %s', [RegIO.UserID, umlBoolToStr(State_).Text]), info_);
end;

procedure TTemp_Auth_Class.Do_Usr_Auth(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
begin
  if State_ then
      AuthIO.Accept
  else
      AuthIO.Reject;
  DelayFreeObj(1.0, self);
  if Service.Log_Client <> nil then
      Service.Log_Client.PostLog('User_' + MakeNowDateStr, Format('User Auth "%s" = %s', [AuthIO.UserID, umlBoolToStr(State_).Text]), info_);
end;

procedure TAuto_Deployment_Service.DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO);
var
  tmp: TTemp_Reg_Class;
begin
  if UserDB_Client = nil then
    begin
      RegIO.Reject;
      exit;
    end;
  tmp := TTemp_Reg_Class.Create;
  tmp.Service := self;
  tmp.RegIO := RegIO;
  UserDB_Client.Usr_RegM(RegIO.UserID, RegIO.Passwd, tmp.Do_Usr_Reg);
end;

procedure TAuto_Deployment_Service.DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO);
var
  tmp: TTemp_Auth_Class;
begin
  if UserDB_Client = nil then
    begin
      AuthIO.Reject;
      exit;
    end;
  tmp := TTemp_Auth_Class.Create;
  tmp.Service := self;
  tmp.AuthIO := AuthIO;
  UserDB_Client.Usr_AuthM(AuthIO.UserID, AuthIO.Passwd, tmp.Do_Usr_Auth);
end;

procedure TAuto_Deployment_Service.Do_C40_Deployment_Ready(States: TC40_Custom_ClientPool_Wait_States);
var
  i: Integer;
  cc: TC40_Custom_Client;
begin
  Log_Client := nil;
  UserDB_Client := nil;
  for i := 0 to Z.Net.C4.C40_ClientPool.Count - 1 do
    begin
      cc := Z.Net.C4.C40_ClientPool[i];
      if cc is TC40_Log_DB_Client then
          Log_Client := cc as TC40_Log_DB_Client
      else if cc is TC40_UserDB_Client then
          UserDB_Client := cc as TC40_UserDB_Client;
    end;
  DoStatus('依赖系统准备就绪.');
end;

constructor TAuto_Deployment_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited;
  Log_Client := nil;
  UserDB_Client := nil;
  // 等依赖服务事件
  C40_ClientPool.WaitConnectedDoneM('UserDB|Log', {$IFDEF FPC}@{$ENDIF FPC}Do_C40_Deployment_Ready);
end;

destructor TAuto_Deployment_Service.Destroy;
begin
  inherited;
end;

initialization

RegisterC40('Auto_Deployment_Demo', TAuto_Deployment_Service, nil);

end.
