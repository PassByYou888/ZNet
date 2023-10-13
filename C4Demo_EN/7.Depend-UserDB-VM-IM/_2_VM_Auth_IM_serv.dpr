program _2_VM_Auth_IM_serv;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.ListEngine,
  Z.Net,
  Z.Net.DoubleTunnelIO.VirtualAuth,
  Z.DFE,
  Z.Status,
  Z.Notify,
  Z.Net.PhysicsIO, Z.Json,
  Z.Net.C4, Z.Net.C4_UserDB,
  Z.Net.C4_Console_APP;

const
  {  The public network address of the dispatching server port, which can be IPv4, IPv6 or DNS  }
  {  Public address, cannot be given to a type of 127.0.0.1  }
  Internet_DP_Addr_ = '127.0.0.1';
  {  Scheduling Server Port  }
  Internet_DP_Port_ = 8387;

  {  Local server public network address  }
  Internet_LocalService_Addr_ = '127.0.0.1';
  Internet_LocalService_Port_ = 8386;

function Get_UserDB_Client: TC40_UserDB_Client;
begin
  Result := TC40_UserDB_Client(C40_ClientPool.ExistsConnectedServiceTyp('UserDB'));
end;

type
  TMyVA_Service = class(TC40_Base_VirtualAuth_Service)
  private type
    TMyVA_RecvIO_Define = class(TService_RecvTunnel_UserDefine_VirtualAuth)
    public
      UserPrimaryIdentifier: U_String;
      MyCustomData: TZJ;
      MyCustomData_MD5: TMD5;
      constructor Create(Owner_: TPeerIO); override;
      destructor Destroy; override;
    end;

    TMyVA_RecvIO_Define_List = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericsList<TMyVA_RecvIO_Define>;

    TTemp_Reg_Class = class
    public
      RegIO: TVirtualRegIO;
      procedure Do_Usr_Reg(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
    end;

    TTemp_Auth_Class = class
    public
      AuthIO: TVirtualAuthIO;
      procedure Do_Usr_Get(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString; Json_: TZJ);
      procedure Do_Usr_GetPrimaryIdentifier(Sender: TC40_UserDB_Client; State_: Boolean; info_, PrimaryIdentifier_: SystemString);
      procedure Do_Usr_Auth(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
    end;

    TTemp_NewIdentifier_Class = class(TCustom_Event_Bridge)
    public
      procedure Do_Usr_NewIdentifier(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
    end;

    TTemp_GetAlias_Class = class(TCustom_Event_Bridge)
    public
      procedure Do_Usr_Get(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString; Json_: TZJ);
    end;

    TTemp_GetMyFriend_Class = class(TCustom_Event_Bridge)
    public
      procedure Do_Usr_GetFriends(Sender: TC40_UserDB_Client; FriendArry: U_StringArray);
    end;
  protected
    procedure DoUserReg_Event(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO); override;
    procedure DoUserAuth_Event(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO); override;
    procedure DoUserOut_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TService_RecvTunnel_UserDefine_VirtualAuth); override;
  private
    procedure cmd_NewLoginName(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_NewAlias(Sender: TPeerIO; InData: TDFE);
    procedure cmd_GetAlias(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Msg(Sender: TPeerIO; InData: TDFE);
    procedure cmd_RequestFriend(Sender: TPeerIO; InData: TDFE);
    procedure cmd_ReponseFriend(Sender: TPeerIO; InData: TDFE);
    procedure cmd_RemoveFriend(Sender: TPeerIO; InData: TDFE);
    procedure cmd_GetMyFriend(Sender: TPeerIO; InData, OutData: TDFE);
  public
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    function Search_IO_Def_From_UserPrimaryIdentifier(UserPrimaryIdentifier: U_String): TMyVA_RecvIO_Define_List;
  end;

constructor TMyVA_Service.TMyVA_RecvIO_Define.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  UserPrimaryIdentifier := '';
  MyCustomData := TZJ.Create;
end;

destructor TMyVA_Service.TMyVA_RecvIO_Define.Destroy;
begin
  inherited Destroy;
  DisposeObject(MyCustomData);
end;

procedure TMyVA_Service.TTemp_Reg_Class.Do_Usr_Reg(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
begin
  if State_ then
      RegIO.Accept
  else
      RegIO.Reject;
  DelayFreeObj(1.0, Self);
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
  {  Create a temp class as an event springboard and point the event to the return of userdb  }
  tmp := TTemp_Reg_Class.Create;
  tmp.RegIO := RegIO;
  Get_UserDB_Client.Usr_RegM(RegIO.UserID, RegIO.Passwd, tmp.Do_Usr_Reg);
end;

procedure TMyVA_Service.TTemp_Auth_Class.Do_Usr_Get(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString; Json_: TZJ);
var
  IO_Def: TMyVA_RecvIO_Define;
begin
  if AuthIO.Online then
    begin
      {  Complete downloading custom information for user registration  }
      IO_Def := TMyVA_RecvIO_Define(AuthIO.UserDefineIO);
      IO_Def.MyCustomData.Assign(Json_);
      IO_Def.MyCustomData_MD5 := IO_Def.MyCustomData.MD5;

      {  Through usr_Open starts the online status of the user on the userdb server. The im subsystem must be opened to work  }
      Sender.Usr_Open(AuthIO.UserID);

      {  Finally, let the C end pass the verification  }
      AuthIO.Accept;
    end
  else
    begin
    end;
  DelayFreeObj(1.0, Self);
end;

procedure TMyVA_Service.TTemp_Auth_Class.Do_Usr_GetPrimaryIdentifier(Sender: TC40_UserDB_Client; State_: Boolean; info_, PrimaryIdentifier_: SystemString);
begin
  if State_ and AuthIO.Online and (Get_UserDB_Client <> nil) then
    begin
      TMyVA_RecvIO_Define(AuthIO.UserDefineIO).UserPrimaryIdentifier := PrimaryIdentifier_;
      {  Download customized information for user registration  }
      Sender.Usr_GetM(PrimaryIdentifier_, 'Custom', Do_Usr_Get);
    end
  else
    begin
      AuthIO.Reject;
      DelayFreeObj(1.0, Self);
    end;
end;

procedure TMyVA_Service.TTemp_Auth_Class.Do_Usr_Auth(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
begin
  if State_ and AuthIO.Online and (Get_UserDB_Client <> nil) then
    begin
      {  After the first step of authentication is passed, the user identity master identifier is obtained from the userdb service  }
      Get_UserDB_Client.Usr_GetPrimaryIdentifierM(AuthIO.UserID, Do_Usr_GetPrimaryIdentifier);
    end
  else
    begin
      AuthIO.Reject;
      DelayFreeObj(1.0, Self);
    end;
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
  {  Create a temp class as an event springboard and point the event to the return of userdb  }
  tmp := TTemp_Auth_Class.Create;
  tmp.AuthIO := AuthIO;
  Get_UserDB_Client.Usr_AuthM(AuthIO.UserID, AuthIO.Passwd, tmp.Do_Usr_Auth);
end;

procedure TMyVA_Service.DoUserOut_Event(Sender: TDTService_VirtualAuth; UserDefineIO: TService_RecvTunnel_UserDefine_VirtualAuth);
var
  IO_Def: TMyVA_RecvIO_Define;
  L: TMyVA_RecvIO_Define_List;
begin
  if Get_UserDB_Client = nil then
      exit;
  IO_Def := UserDefineIO as TMyVA_RecvIO_Define;
  if IO_Def.LoginSuccessed then
    begin
      {  If the mycustom data is found to be changed, it is passed to the userdb service  }
      if not umlCompareMD5(IO_Def.MyCustomData_MD5, IO_Def.MyCustomData.MD5) then
          Get_UserDB_Client.Usr_Set(IO_Def.UserPrimaryIdentifier, 'Custom', IO_Def.MyCustomData);

      {  When the C-end user disconnects (referring to the disconnection of multiple logins on one account), tell the userDB service not to give me any more messages related to him  }
      L := Search_IO_Def_From_UserPrimaryIdentifier(IO_Def.UserPrimaryIdentifier);
      if (L.Count = 1) and (L[0] = IO_Def) and (Get_UserDB_Client <> nil) then
          Get_UserDB_Client.Usr_Close(IO_Def.UserPrimaryIdentifier);
      L.Free;
    end;
end;

procedure TMyVA_Service.TTemp_NewIdentifier_Class.Do_Usr_NewIdentifier(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
begin
  if CheckIO then
    begin
      IO.OutDFE.WriteBool(State_);
      IO.OutDFE.WriteString(info_);
      IO.ContinueResultSend;
    end;
  DelayFreeObj(1.0, Self);
end;

procedure TMyVA_Service.cmd_NewLoginName(Sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TMyVA_RecvIO_Define;
  tmp: TTemp_NewIdentifier_Class;
begin
  if Get_UserDB_Client = nil then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('UserDB service is offline.');
      exit;
    end;
  IO_Def := Sender.UserDefine as TMyVA_RecvIO_Define;
  if not IO_Def.LoginSuccessed then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('login failed!');
      exit;
    end;
  tmp := TTemp_NewIdentifier_Class.Create(Sender);
  Get_UserDB_Client.Usr_NewIdentifierM(IO_Def.UserPrimaryIdentifier, InData.R.ReadString, tmp.Do_Usr_NewIdentifier);
  Sender.PauseResultSend;
end;

procedure TMyVA_Service.cmd_NewAlias(Sender: TPeerIO; InData: TDFE);
var
  IO_Def: TMyVA_RecvIO_Define;
begin
  if Get_UserDB_Client = nil then
      exit;
  IO_Def := Sender.UserDefine as TMyVA_RecvIO_Define;
  if not IO_Def.LoginSuccessed then
      exit;
  IO_Def.MyCustomData.S['Alias'] := InData.R.ReadString;
  IO_Def.MyCustomData_MD5 := IO_Def.MyCustomData.MD5;
  Get_UserDB_Client.Usr_Set(IO_Def.UserPrimaryIdentifier, 'Custom', IO_Def.MyCustomData);
end;

procedure TMyVA_Service.TTemp_GetAlias_Class.Do_Usr_Get(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString; Json_: TZJ);
begin
  if CheckIO then
    begin
      if Json_.IndexOf('Alias') >= 0 then
          IO.OutDFE.WriteString(Json_.S['Alias']);
      IO.ContinueResultSend;
    end;
  DelayFreeObj(1.0, Self);
end;

procedure TMyVA_Service.cmd_GetAlias(Sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TMyVA_RecvIO_Define;
  tmp: TTemp_GetAlias_Class;
  usr_Name: U_String;
begin
  if Get_UserDB_Client = nil then
      exit;
  IO_Def := Sender.UserDefine as TMyVA_RecvIO_Define;
  if not IO_Def.LoginSuccessed then
      exit;
  usr_Name := InData.R.ReadString;
  if usr_Name.L = 0 then
      exit;
  tmp := TTemp_GetAlias_Class.Create(Sender);
  Get_UserDB_Client.Usr_GetM(usr_Name, 'Custom', tmp.Do_Usr_Get);
  Sender.PauseResultSend;
end;

procedure TMyVA_Service.cmd_Msg(Sender: TPeerIO; InData: TDFE);
var
  IO_Def: TMyVA_RecvIO_Define;
  ToUserName_, msg_: U_String;
begin
  if Get_UserDB_Client = nil then
      exit;
  IO_Def := Sender.UserDefine as TMyVA_RecvIO_Define;
  if not IO_Def.LoginSuccessed then
      exit;

  ToUserName_ := InData.R.ReadString;
  msg_ := InData.R.ReadString;
  Get_UserDB_Client.Usr_Msg(IO_Def.UserPrimaryIdentifier, ToUserName_, msg_);
end;

procedure TMyVA_Service.cmd_RequestFriend(Sender: TPeerIO; InData: TDFE);
var
  IO_Def: TMyVA_RecvIO_Define;
  ToUserName_, msg_: U_String;
begin
  if Get_UserDB_Client = nil then
      exit;
  IO_Def := Sender.UserDefine as TMyVA_RecvIO_Define;
  if not IO_Def.LoginSuccessed then
      exit;
  ToUserName_ := InData.R.ReadString;
  msg_ := InData.R.ReadString;
  Get_UserDB_Client.Usr_RequestAddFriend(IO_Def.UserPrimaryIdentifier, ToUserName_, msg_);
end;

procedure TMyVA_Service.cmd_ReponseFriend(Sender: TPeerIO; InData: TDFE);
var
  IO_Def: TMyVA_RecvIO_Define;
  ToUserName_, msg_: U_String;
  Accept_: Boolean;
begin
  if Get_UserDB_Client = nil then
      exit;
  IO_Def := Sender.UserDefine as TMyVA_RecvIO_Define;
  if not IO_Def.LoginSuccessed then
      exit;
  ToUserName_ := InData.R.ReadString;
  msg_ := InData.R.ReadString;
  Accept_ := InData.R.ReadBool;
  Get_UserDB_Client.Usr_ReponseAddFriend(IO_Def.UserPrimaryIdentifier, ToUserName_, msg_, Accept_);
end;

procedure TMyVA_Service.cmd_RemoveFriend(Sender: TPeerIO; InData: TDFE);
var
  IO_Def: TMyVA_RecvIO_Define;
  ToUserName_: U_String;
begin
  if Get_UserDB_Client = nil then
      exit;
  IO_Def := Sender.UserDefine as TMyVA_RecvIO_Define;
  if not IO_Def.LoginSuccessed then
      exit;
  ToUserName_ := InData.R.ReadString;
  Get_UserDB_Client.Usr_RemoveFriend(IO_Def.UserPrimaryIdentifier, ToUserName_);
end;

procedure TMyVA_Service.TTemp_GetMyFriend_Class.Do_Usr_GetFriends(Sender: TC40_UserDB_Client; FriendArry: U_StringArray);
var
  i: integer;
begin
  if CheckIO then
    begin
      for i := 0 to length(FriendArry) - 1 do
          IO.OutDFE.WriteString(FriendArry[i]);
      IO.ContinueResultSend;
    end;
  DelayFreeObj(1.0, Self);
end;

procedure TMyVA_Service.cmd_GetMyFriend(Sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TMyVA_RecvIO_Define;
begin
  if Get_UserDB_Client = nil then
      exit;
  IO_Def := Sender.UserDefine as TMyVA_RecvIO_Define;
  if not IO_Def.LoginSuccessed then
      exit;
  Get_UserDB_Client.Usr_GetFriendsM(IO_Def.UserPrimaryIdentifier, TTemp_GetMyFriend_Class.Create(Sender).Do_Usr_GetFriends);
  Sender.PauseResultSend;
end;

constructor TMyVA_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  {  Specify custom IO  }
  DTVirtualAuthService.RecvTunnel.UserDefineClass := TMyVA_RecvIO_Define;
  {  Registration command  }
  DTVirtualAuthService.RecvTunnel.RegisterStream('NewLoginName').OnExecute := cmd_NewLoginName;
  DTVirtualAuthService.RecvTunnel.RegisterDirectStream('NewAlias').OnExecute := cmd_NewAlias;
  DTVirtualAuthService.RecvTunnel.RegisterStream('GetAlias').OnExecute := cmd_GetAlias;
  DTVirtualAuthService.RecvTunnel.RegisterDirectStream('Msg').OnExecute := cmd_Msg;
  DTVirtualAuthService.RecvTunnel.RegisterDirectStream('RequestFriend').OnExecute := cmd_RequestFriend;
  DTVirtualAuthService.RecvTunnel.RegisterDirectStream('ReponseFriend').OnExecute := cmd_ReponseFriend;
  DTVirtualAuthService.RecvTunnel.RegisterDirectStream('RemoveFriend').OnExecute := cmd_RemoveFriend;
  DTVirtualAuthService.RecvTunnel.RegisterStream('GetMyFriend').OnExecute := cmd_GetMyFriend;
end;

destructor TMyVA_Service.Destroy;
begin
  inherited Destroy;
end;

function TMyVA_Service.Search_IO_Def_From_UserPrimaryIdentifier(UserPrimaryIdentifier: U_String): TMyVA_RecvIO_Define_List;
var
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_Def: TMyVA_Service.TMyVA_RecvIO_Define;
begin
  Result := TMyVA_RecvIO_Define_List.Create;
  DTVirtualAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_Def := DTVirtualAuthService.RecvTunnel[ID_].UserDefine as TMyVA_Service.TMyVA_RecvIO_Define;
      if IO_Def.LinkOk and IO_Def.LoginSuccessed and UserPrimaryIdentifier.Same(@IO_Def.UserPrimaryIdentifier) then
          Result.Add(IO_Def);
    end;
end;

type
  TMyClientIntf = class(TCore_InterfacedObject, IC40_PhysicsTunnel_Event, I_ON_C40_UserDB_Client_Notify)
  private
    {  SaaS network status event interface  }
    procedure C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    procedure C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    {  Im event interface  }
    procedure Do_User_Msg(Sender: TC40_UserDB_Client; FromUserName_, ToUserName_, msg_: U_String);
    procedure Do_User_Open(Sender: TC40_UserDB_Client; UserName_, ToUserName_: U_String);
    procedure Do_User_Close(Sender: TC40_UserDB_Client; UserName_, ToUserName_: U_String);
    procedure Do_User_Request_Friend(Sender: TC40_UserDB_Client; FromUserName_, DestFriendUserName_, msg_: U_String);
    procedure Do_User_Kick(sender: TC40_UserDB_Client; UserName_: U_String);
  public
  end;

  {  SaaS network status event interface  }
procedure TMyClientIntf.C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
begin
end;

procedure TMyClientIntf.C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
begin
end;

procedure TMyClientIntf.C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
  if Custom_Client_ is TC40_UserDB_Client then
      TC40_UserDB_Client(Custom_Client_).ON_C40_UserDB_Client_Notify := Self;
end;

procedure TMyClientIntf.C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
end;

procedure TMyClientIntf.Do_User_Msg(Sender: TC40_UserDB_Client; FromUserName_, ToUserName_, msg_: U_String);
var
  arry: TC40_Custom_Service_Array;
  i, j: integer;
  S: TMyVA_Service;
  L: TMyVA_Service.TMyVA_RecvIO_Define_List;
begin
  {  First find the server from the SaaS network  }
  arry := Z.Net.C4.C40_ServicePool.GetFromServiceTyp('MyVA');
  for i := 0 to length(arry) - 1 do
    begin
      S := TMyVA_Service(arry[i]);
      {  Search for user identifiers, allowing users to log in multiple times with one account and broadcast messages simultaneously, similar to the multi device login mechanism of WeChat QQ  }
      L := S.Search_IO_Def_From_UserPrimaryIdentifier(ToUserName_);
      for j := 0 to L.Count - 1 do
          L[j].SendTunnel.Owner.SendDirectConsoleCmd('userMsg', Format('%S said to you:%s', [FromUserName_.Text, msg_.Text]));
      L.Free;
    end;
end;

procedure TMyClientIntf.Do_User_Open(Sender: TC40_UserDB_Client; UserName_, ToUserName_: U_String);
var
  arry: TC40_Custom_Service_Array;
  i, j: integer;
  S: TMyVA_Service;
  L: TMyVA_Service.TMyVA_RecvIO_Define_List;
begin
  {  First find the server from the SaaS network  }
  arry := Z.Net.C4.C40_ServicePool.GetFromServiceTyp('MyVA');
  for i := 0 to length(arry) - 1 do
    begin
      S := TMyVA_Service(arry[i]);
      {  Search for user identifiers, allowing users to log in multiple times with one account and broadcast messages simultaneously, similar to the multi device login mechanism of WeChat QQ  }
      L := S.Search_IO_Def_From_UserPrimaryIdentifier(ToUserName_);
      for j := 0 to L.Count - 1 do
          L[j].SendTunnel.Owner.SendDirectConsoleCmd('userOnline', Format('%S online', [UserName_.Text]));
      L.Free;
    end;
end;

procedure TMyClientIntf.Do_User_Close(Sender: TC40_UserDB_Client; UserName_, ToUserName_: U_String);
var
  arry: TC40_Custom_Service_Array;
  i, j: integer;
  S: TMyVA_Service;
  L: TMyVA_Service.TMyVA_RecvIO_Define_List;
begin
  {  First find the server from the SaaS network  }
  arry := Z.Net.C4.C40_ServicePool.GetFromServiceTyp('MyVA');
  for i := 0 to length(arry) - 1 do
    begin
      S := TMyVA_Service(arry[i]);
      {  Search for user identifiers, allowing users to log in multiple times with one account and broadcast messages simultaneously, similar to the multi device login mechanism of WeChat QQ  }
      L := S.Search_IO_Def_From_UserPrimaryIdentifier(ToUserName_);
      for j := 0 to L.Count - 1 do
          L[j].SendTunnel.Owner.SendDirectConsoleCmd('userOffline', Format('%S offline', [UserName_.Text]));
      L.Free;
    end;
end;

procedure TMyClientIntf.Do_User_Request_Friend(Sender: TC40_UserDB_Client; FromUserName_, DestFriendUserName_, msg_: U_String);
var
  arry: TC40_Custom_Service_Array;
  i, j: integer;
  S: TMyVA_Service;
  L: TMyVA_Service.TMyVA_RecvIO_Define_List;
begin
  {  First find the server from the SaaS network  }
  arry := Z.Net.C4.C40_ServicePool.GetFromServiceTyp('MyVA');
  for i := 0 to length(arry) - 1 do
    begin
      S := TMyVA_Service(arry[i]);
      {  Search for user identifiers, allowing users to log in multiple times with one account and broadcast messages simultaneously, similar to the multi device login mechanism of WeChat QQ  }
      L := S.Search_IO_Def_From_UserPrimaryIdentifier(DestFriendUserName_);
      for j := 0 to L.Count - 1 do
          L[j].SendTunnel.Owner.SendDirectConsoleCmd('userRequestFriend', Format('%S requests to add you as a friend', [FromUserName_.Text, msg_.Text]));
      L.Free;
    end;
end;

procedure TMyClientIntf.Do_User_Kick(sender: TC40_UserDB_Client; UserName_: U_String);
var
  arry: TC40_Custom_Service_Array;
  i, j: integer;
  S: TMyVA_Service;
  L: TMyVA_Service.TMyVA_RecvIO_Define_List;
begin
  arry := Z.Net.C4.C40_ServicePool.GetFromServiceTyp('MyVA');
  for i := 0 to length(arry) - 1 do
    begin
      S := TMyVA_Service(arry[i]);
      L := S.Search_IO_Def_From_UserPrimaryIdentifier(UserName_);
      for j := 0 to L.Count - 1 do
          L[j].Owner.p2pVM.Owner_IO.DelayClose;
      L.Free;
    end;
end;

begin
  {  Register myva  }
  RegisterC40('MyVA', TMyVA_Service, nil);

  {  Open Log Information  }
  Z.Net.C4.C40_QuietMode := False;
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'DP|UserDB', TMyClientIntf.Create);
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_LocalService_Addr_, Internet_LocalService_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('MyVA');
      StartService;
    end;

  {  Main loop  }
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
