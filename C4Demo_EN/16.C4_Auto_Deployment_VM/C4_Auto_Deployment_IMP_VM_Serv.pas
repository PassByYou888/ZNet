{  Automated dependency detection can effectively reduce deployment complexity  }
{  Automation dependency is achieved by detecting client classes that are already connected and ready  }
unit C4_Auto_Deployment_IMP_VM_Serv;

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
  Z.Net.C4, Z.Net.C4_UserDB, Z.Net.C4_Log_DB, Z.Net.C4.VM;

type
  TAuto_Deployment_Service = class;

  TTemp_Reg_Class = class {  Event bridge from UserDB server  }
  public
    Service: TAuto_Deployment_Service;
    RegIO: TVirtualRegIO;
    procedure Do_Usr_Reg(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
  end;

  TTemp_Auth_Class = class {  Event bridge from UserDB server  }
  public
    Service: TAuto_Deployment_Service;
    AuthIO: TVirtualAuthIO;
    procedure Do_Usr_Auth(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
  end;

  TAuto_Deployment_Service = class(TC40_VirtualAuth_VM_Service) {  VM mode server is different from C4 here  }
  protected
    procedure DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO); override;
    procedure DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO); override;
  protected
  public
    Log_Client: TC40_Log_DB_Client;
    UserDB_Client: TC40_UserDB_Client;

    constructor Create(Param_: U_String); override;
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

constructor TAuto_Deployment_Service.Create(Param_: U_String);
begin
  inherited;
  Log_Client := nil;
  UserDB_Client := nil;
  {  Automatic dependency deployment  }
  TC40_Auto_Deployment_Client<TC40_Log_DB_Client>.Create('Log', Log_Client);
  TC40_Auto_Deployment_Client<TC40_UserDB_Client>.Create('UserDB', UserDB_Client);
end;

destructor TAuto_Deployment_Service.Destroy;
begin
  inherited;
end;

end.
