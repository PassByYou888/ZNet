{ ****************************************************************************** }
{ * cloud 4.0 User Database VM support                                         * }
{ ****************************************************************************** }
unit Z.Net.C4_VM_UserDB;

{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib,
  Z.Geometry2D, Z.DFE, Z.Expression, Z.OpCode, Z.ListEngine,
  Z.Json, Z.HashList.Templet, Z.ZDB2, Z.ZDB2.Json, Z.Cipher,
  Z.Notify, Z.MemoryStream,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth, Z.Net.C4, Z.Net.C4.VM;

type
  TC40_UserDB_VM_Service = class;

  TC40_UserDB_VM_Json_Hash_Pool = {$IFDEF FPC}specialize {$ENDIF FPC}TGeneric_String_Object_Hash<TZDB2_Json>;

  TC40_UserDB_VM_Service_SendTunnel_NoAuth = class(TPeerClientUserDefineForSendTunnel_NoAuth)
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TC40_UserDB_VM_Service_RecvTunnel_NoAuth = class(TPeerClientUserDefineForRecvTunnel_NoAuth)
  public
    OpenUserIdentifier: TC40_UserDB_VM_Json_Hash_Pool;
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
    procedure SendUser_Msg(FromUserName_, ToUserName_, Msg_: U_String);
    procedure SendUser_Open(UserName_, ToUserName_: U_String);
    procedure SendUser_Close(UserName_, ToUserName_: U_String);
    procedure SendUser_Request_Friend(FromUserName_, DestFriendUserName_, Msg_: U_String);
    procedure SendUser_Kick(UserName_: U_String);
  end;

  TC40_UserDB_VM_Service_RecvTunnel_NoAuth_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_UserDB_VM_Service_RecvTunnel_NoAuth>;

  TC40_UserDB_VM_Service = class(TC40_NoAuth_VM_Service)
  protected
    // IM
    procedure cmd_Usr_Open(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Close(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_IsOpen(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Msg(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_GetFriends(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_RemoveFriend(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_RequestAddFriend(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_ReponseAddFriend(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_OnlineNum(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_OnlineList(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Kick(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Enabled(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Disable(Sender: TPeerIO; InData: TDFE);
    // db
    procedure cmd_Usr_Reg(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Exists(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Auth(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_ChangePassword(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_ResetPassword(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_NewIdentifier(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_GetPrimaryIdentifier(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Get(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Set(Sender: TPeerIO; InData: TDFE);
    // admin
    procedure cmd_Usr_Search(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Usr_Upload(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Remove(Sender: TPeerIO; InData: TDFE);
  protected
    // console command
    procedure CC_Compress_And_Reload(var OP_Param: TOpParam);
  public
    C40_UserDB_VM_FileName: U_String;
    UserIdentifierHash: TC40_UserDB_VM_Json_Hash_Pool;
    ZDB2RecycleMemoryTimeOut: TTimeTick;
    ZDB2DeltaSpace: Int64;
    ZDB2BlockSize: Word;
    ZDB2EnabledCipher: Boolean;
    ZDB2CipherName: U_String;
    ZDB2Password: U_String;
    ZDB2Cipher: TZDB2_Cipher;
    Max_Reg_Identifier_Size: Integer;
    JsonDatabase: TZDB2_List_Json;
    constructor Create(Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
    procedure SendMsg(FromUserName_, ToUserName_, Msg_: U_String);
  end;

  TC40_UserDB_VM_Client = class;

  I_ON_C40_UserDB_VM_Client_Notify = interface
    procedure Do_User_Msg(Sender: TC40_UserDB_VM_Client; FromUserName_, ToUserName_, Msg_: U_String);
    procedure Do_User_Open(Sender: TC40_UserDB_VM_Client; UserName_, ToUserName_: U_String);
    procedure Do_User_Close(Sender: TC40_UserDB_VM_Client; UserName_, ToUserName_: U_String);
    procedure Do_User_Request_Friend(Sender: TC40_UserDB_VM_Client; FromUserName_, DestFriendUserName_, Msg_: U_String);
    procedure Do_User_Kick(Sender: TC40_UserDB_VM_Client; UserName_: U_String);
  end;

{$REGION 'bridge_define'}

  TC40_UserDB_VM_On_IsOpenC = procedure(Sender: TC40_UserDB_VM_Client; State_: TArrayBool);
  TC40_UserDB_VM_On_IsOpenM = procedure(Sender: TC40_UserDB_VM_Client; State_: TArrayBool) of object;
{$IFDEF FPC}
  TC40_UserDB_VM_On_IsOpenP = procedure(Sender: TC40_UserDB_VM_Client; State_: TArrayBool) is nested;
{$ELSE FPC}
  TC40_UserDB_VM_On_IsOpenP = reference to procedure(Sender: TC40_UserDB_VM_Client; State_: TArrayBool);
{$ENDIF FPC}

  TC40_UserDB_VM_On_IsOpen = class(TOnResultBridge)
  public
    Client: TC40_UserDB_VM_Client;
    OnResultC: TC40_UserDB_VM_On_IsOpenC;
    OnResultM: TC40_UserDB_VM_On_IsOpenM;
    OnResultP: TC40_UserDB_VM_On_IsOpenP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_UserDB_VM_On_GetFriendsC = procedure(Sender: TC40_UserDB_VM_Client; FriendArry: U_StringArray);
  TC40_UserDB_VM_On_GetFriendsM = procedure(Sender: TC40_UserDB_VM_Client; FriendArry: U_StringArray) of object;
{$IFDEF FPC}
  TC40_UserDB_VM_On_GetFriendsP = procedure(Sender: TC40_UserDB_VM_Client; FriendArry: U_StringArray) is nested;
{$ELSE FPC}
  TC40_UserDB_VM_On_GetFriendsP = reference to procedure(Sender: TC40_UserDB_VM_Client; FriendArry: U_StringArray);
{$ENDIF FPC}

  TC40_UserDB_VM_On_GetFriends = class(TOnResultBridge)
  public
    Client: TC40_UserDB_VM_Client;
    OnResultC: TC40_UserDB_VM_On_GetFriendsC;
    OnResultM: TC40_UserDB_VM_On_GetFriendsM;
    OnResultP: TC40_UserDB_VM_On_GetFriendsP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_UserDB_VM_On_OnlineNumC = procedure(Sender: TC40_UserDB_VM_Client; Online_Num, User_Num: Integer);
  TC40_UserDB_VM_On_OnlineNumM = procedure(Sender: TC40_UserDB_VM_Client; Online_Num, User_Num: Integer) of object;
{$IFDEF FPC}
  TC40_UserDB_VM_On_OnlineNumP = procedure(Sender: TC40_UserDB_VM_Client; Online_Num, User_Num: Integer) is nested;
{$ELSE FPC}
  TC40_UserDB_VM_On_OnlineNumP = reference to procedure(Sender: TC40_UserDB_VM_Client; Online_Num, User_Num: Integer);
{$ENDIF FPC}

  TC40_UserDB_VM_On_OnlineNum = class(TOnResultBridge)
  public
    Client: TC40_UserDB_VM_Client;
    OnResultC: TC40_UserDB_VM_On_OnlineNumC;
    OnResultM: TC40_UserDB_VM_On_OnlineNumM;
    OnResultP: TC40_UserDB_VM_On_OnlineNumP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_UserDB_VM_On_OnlineListC = procedure(Sender: TC40_UserDB_VM_Client; arry: U_StringArray);
  TC40_UserDB_VM_On_OnlineListM = procedure(Sender: TC40_UserDB_VM_Client; arry: U_StringArray) of object;
{$IFDEF FPC}
  TC40_UserDB_VM_On_OnlineListP = procedure(Sender: TC40_UserDB_VM_Client; arry: U_StringArray) is nested;
{$ELSE FPC}
  TC40_UserDB_VM_On_OnlineListP = reference to procedure(Sender: TC40_UserDB_VM_Client; arry: U_StringArray);
{$ENDIF FPC}

  TC40_UserDB_VM_On_OnlineList = class(TOnResultBridge)
  public
    Client: TC40_UserDB_VM_Client;
    OnResultC: TC40_UserDB_VM_On_OnlineListC;
    OnResultM: TC40_UserDB_VM_On_OnlineListM;
    OnResultP: TC40_UserDB_VM_On_OnlineListP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_UserDB_VM_On_RegC = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString);
  TC40_UserDB_VM_On_RegM = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString) of object;
{$IFDEF FPC}
  TC40_UserDB_VM_On_RegP = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString) is nested;
{$ELSE FPC}
  TC40_UserDB_VM_On_RegP = reference to procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString);
{$ENDIF FPC}

  TC40_UserDB_VM_On_Reg = class(TOnResultBridge)
  public
    Client: TC40_UserDB_VM_Client;
    OnResultC: TC40_UserDB_VM_On_RegC;
    OnResultM: TC40_UserDB_VM_On_RegM;
    OnResultP: TC40_UserDB_VM_On_RegP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_UserDB_VM_On_ExistsC = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean);
  TC40_UserDB_VM_On_ExistsM = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean) of object;
{$IFDEF FPC}
  TC40_UserDB_VM_On_ExistsP = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean) is nested;
{$ELSE FPC}
  TC40_UserDB_VM_On_ExistsP = reference to procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean);
{$ENDIF FPC}

  TC40_UserDB_VM_On_Exists = class(TOnResultBridge)
  public
    Client: TC40_UserDB_VM_Client;
    OnResultC: TC40_UserDB_VM_On_ExistsC;
    OnResultM: TC40_UserDB_VM_On_ExistsM;
    OnResultP: TC40_UserDB_VM_On_ExistsP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_UserDB_VM_On_AuthC = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString);
  TC40_UserDB_VM_On_AuthM = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString) of object;
{$IFDEF FPC}
  TC40_UserDB_VM_On_AuthP = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString) is nested;
{$ELSE FPC}
  TC40_UserDB_VM_On_AuthP = reference to procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString);
{$ENDIF FPC}

  TC40_UserDB_VM_On_Auth = class(TOnResultBridge)
  public
    Client: TC40_UserDB_VM_Client;
    OnResultC: TC40_UserDB_VM_On_AuthC;
    OnResultM: TC40_UserDB_VM_On_AuthM;
    OnResultP: TC40_UserDB_VM_On_AuthP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_UserDB_VM_On_ChangePasswordC = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString);
  TC40_UserDB_VM_On_ChangePasswordM = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString) of object;
{$IFDEF FPC}
  TC40_UserDB_VM_On_ChangePasswordP = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString) is nested;
{$ELSE FPC}
  TC40_UserDB_VM_On_ChangePasswordP = reference to procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString);
{$ENDIF FPC}

  TC40_UserDB_VM_On_ChangePassword = class(TOnResultBridge)
  public
    Client: TC40_UserDB_VM_Client;
    OnResultC: TC40_UserDB_VM_On_ChangePasswordC;
    OnResultM: TC40_UserDB_VM_On_ChangePasswordM;
    OnResultP: TC40_UserDB_VM_On_ChangePasswordP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_UserDB_VM_On_NewIdentifierC = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString);
  TC40_UserDB_VM_On_NewIdentifierM = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString) of object;
{$IFDEF FPC}
  TC40_UserDB_VM_On_NewIdentifierP = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString) is nested;
{$ELSE FPC}
  TC40_UserDB_VM_On_NewIdentifierP = reference to procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString);
{$ENDIF FPC}

  TC40_UserDB_VM_On_NewIdentifier = class(TOnResultBridge)
  public
    Client: TC40_UserDB_VM_Client;
    OnResultC: TC40_UserDB_VM_On_NewIdentifierC;
    OnResultM: TC40_UserDB_VM_On_NewIdentifierM;
    OnResultP: TC40_UserDB_VM_On_NewIdentifierP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_UserDB_VM_On_GetC = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString; Json_: TZJ);
  TC40_UserDB_VM_On_GetM = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString; Json_: TZJ) of object;
{$IFDEF FPC}
  TC40_UserDB_VM_On_GetP = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString; Json_: TZJ) is nested;
{$ELSE FPC}
  TC40_UserDB_VM_On_GetP = reference to procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_: SystemString; Json_: TZJ);
{$ENDIF FPC}

  TC40_UserDB_VM_On_Get = class(TOnResultBridge)
  public
    Client: TC40_UserDB_VM_Client;
    OnResultC: TC40_UserDB_VM_On_GetC;
    OnResultM: TC40_UserDB_VM_On_GetM;
    OnResultP: TC40_UserDB_VM_On_GetP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_UserDB_VM_On_GetPrimaryIdentifierC = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_, PrimaryIdentifier_: SystemString);
  TC40_UserDB_VM_On_GetPrimaryIdentifierM = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_, PrimaryIdentifier_: SystemString) of object;
{$IFDEF FPC}
  TC40_UserDB_VM_On_GetPrimaryIdentifierP = procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_, PrimaryIdentifier_: SystemString) is nested;
{$ELSE FPC}
  TC40_UserDB_VM_On_GetPrimaryIdentifierP = reference to procedure(Sender: TC40_UserDB_VM_Client; State_: Boolean; info_, PrimaryIdentifier_: SystemString);
{$ENDIF FPC}

  TC40_UserDB_VM_On_GetPrimaryIdentifier = class(TOnResultBridge)
  public
    Client: TC40_UserDB_VM_Client;
    OnResultC: TC40_UserDB_VM_On_GetPrimaryIdentifierC;
    OnResultM: TC40_UserDB_VM_On_GetPrimaryIdentifierM;
    OnResultP: TC40_UserDB_VM_On_GetPrimaryIdentifierP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

{$ENDREGION 'bridge_define'}

  TC40_UserDB_VM_Client = class(TC40_NoAuth_VM_Client)
  private
    procedure cmd_Usr_Msg(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Open(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Close(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Request_Friend(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Usr_Kick(Sender: TPeerIO; InData: TDFE);
  public
    ON_C40_UserDB_VM_Client_Notify: I_ON_C40_UserDB_VM_Client_Notify;
    constructor Create(Param_: U_String); override;
    destructor Destroy; override;

    // IM
    procedure Usr_Open(UserName_: U_String);
    procedure Usr_Close(UserName_: U_String);
    // is open for one
    procedure Usr_IsOpen_Bridge(UserName_: U_String; Bridge_IO_: TPeerIO); overload;
    procedure Usr_IsOpenC(UserName_: U_String; OnResult: TC40_UserDB_VM_On_IsOpenC); overload;
    procedure Usr_IsOpenM(UserName_: U_String; OnResult: TC40_UserDB_VM_On_IsOpenM); overload;
    procedure Usr_IsOpenP(UserName_: U_String; OnResult: TC40_UserDB_VM_On_IsOpenP); overload;
    // is open for multi
    procedure Usr_IsOpen_Bridge(UserName_: U_StringArray; Bridge_IO_: TPeerIO); overload;
    procedure Usr_IsOpenC(UserName_: U_StringArray; OnResult: TC40_UserDB_VM_On_IsOpenC); overload;
    procedure Usr_IsOpenM(UserName_: U_StringArray; OnResult: TC40_UserDB_VM_On_IsOpenM); overload;
    procedure Usr_IsOpenP(UserName_: U_StringArray; OnResult: TC40_UserDB_VM_On_IsOpenP); overload;
    // send msg
    procedure Usr_Msg(FromUserName_, ToUserName_, Msg_: U_String);
    // get friends
    procedure Usr_GetFriends_Bridge(UserName_: U_String; Bridge_IO_: TPeerIO);
    procedure Usr_GetFriendsC(UserName_: U_String; OnResult: TC40_UserDB_VM_On_GetFriendsC);
    procedure Usr_GetFriendsM(UserName_: U_String; OnResult: TC40_UserDB_VM_On_GetFriendsM);
    procedure Usr_GetFriendsP(UserName_: U_String; OnResult: TC40_UserDB_VM_On_GetFriendsP);
    // remove friend
    procedure Usr_RemoveFriend(UserName_, DestFriendUserName_: U_String);
    // request add friend
    procedure Usr_RequestAddFriend(FromUserName_, DestFriendUserName_, Msg_: U_String);
    // reponse add friend
    procedure Usr_ReponseAddFriend(FromUserName_, DestFriendUserName_, Msg_: U_String; Accept_: Boolean);
    // get online num
    procedure Usr_OnlineNum_Bridge(Bridge_IO_: TPeerIO);
    procedure Usr_OnlineNumC(OnResult: TC40_UserDB_VM_On_OnlineNumC);
    procedure Usr_OnlineNumM(OnResult: TC40_UserDB_VM_On_OnlineNumM);
    procedure Usr_OnlineNumP(OnResult: TC40_UserDB_VM_On_OnlineNumP);
    // get online list
    procedure Usr_OnlineList_Bridge(Max_Num: Integer; Bridge_IO_: TPeerIO);
    procedure Usr_OnlineListC(Max_Num: Integer; OnResult: TC40_UserDB_VM_On_OnlineListC);
    procedure Usr_OnlineListM(Max_Num: Integer; OnResult: TC40_UserDB_VM_On_OnlineListM);
    procedure Usr_OnlineListP(Max_Num: Integer; OnResult: TC40_UserDB_VM_On_OnlineListP);
    // kick user
    procedure Usr_Kick(UserName_: U_String);
    // enabled user
    procedure Usr_Enabled(UserName_: U_String);
    // disable usere
    procedure Usr_Disable(UserName_: U_String);
    // user registration
    procedure Usr_Reg_Bridge(UserName_, passwd_: U_String; Bridge_IO_: TPeerIO);
    procedure Usr_RegC(UserName_, passwd_: U_String; OnResult: TC40_UserDB_VM_On_RegC);
    procedure Usr_RegM(UserName_, passwd_: U_String; OnResult: TC40_UserDB_VM_On_RegM);
    procedure Usr_RegP(UserName_, passwd_: U_String; OnResult: TC40_UserDB_VM_On_RegP);
    // find user
    procedure Usr_Exists_Bridge(UserName_: U_String; Bridge_IO_: TPeerIO);
    procedure Usr_ExistsC(UserName_: U_String; OnResult: TC40_UserDB_VM_On_ExistsC);
    procedure Usr_ExistsM(UserName_: U_String; OnResult: TC40_UserDB_VM_On_ExistsM);
    procedure Usr_ExistsP(UserName_: U_String; OnResult: TC40_UserDB_VM_On_ExistsP);
    // auth: Quantum Cryptography Password
    procedure Usr_Auth_Bridge(UserName_, passwd_: U_String; Bridge_IO_: TPeerIO);
    procedure Usr_AuthC(UserName_, passwd_: U_String; OnResult: TC40_UserDB_VM_On_AuthC);
    procedure Usr_AuthM(UserName_, passwd_: U_String; OnResult: TC40_UserDB_VM_On_AuthM);
    procedure Usr_AuthP(UserName_, passwd_: U_String; OnResult: TC40_UserDB_VM_On_AuthP);
    // change password
    procedure Usr_ChangePassword_Bridge(UserName_, passwd_, NewPasswd_: U_String; Bridge_IO_: TPeerIO);
    procedure Usr_ChangePasswordC(UserName_, passwd_, NewPasswd_: U_String; OnResult: TC40_UserDB_VM_On_ChangePasswordC);
    procedure Usr_ChangePasswordM(UserName_, passwd_, NewPasswd_: U_String; OnResult: TC40_UserDB_VM_On_ChangePasswordM);
    procedure Usr_ChangePasswordP(UserName_, passwd_, NewPasswd_: U_String; OnResult: TC40_UserDB_VM_On_ChangePasswordP);
    // reset password
    procedure Usr_ResetPassword(UserName_, NewPasswd_: U_String);
    // new user Identifier
    procedure Usr_NewIdentifier_Bridge(UserName_, NewIdentifier_: U_String; Bridge_IO_: TPeerIO);
    procedure Usr_NewIdentifierC(UserName_, NewIdentifier_: U_String; OnResult: TC40_UserDB_VM_On_NewIdentifierC);
    procedure Usr_NewIdentifierM(UserName_, NewIdentifier_: U_String; OnResult: TC40_UserDB_VM_On_NewIdentifierM);
    procedure Usr_NewIdentifierP(UserName_, NewIdentifier_: U_String; OnResult: TC40_UserDB_VM_On_NewIdentifierP);
    // get identifier
    procedure Usr_GetPrimaryIdentifier_Bridge(UserName_: U_String; Bridge_IO_: TPeerIO);
    procedure Usr_GetPrimaryIdentifierC(UserName_: U_String; OnResult: TC40_UserDB_VM_On_GetPrimaryIdentifierC);
    procedure Usr_GetPrimaryIdentifierM(UserName_: U_String; OnResult: TC40_UserDB_VM_On_GetPrimaryIdentifierM);
    procedure Usr_GetPrimaryIdentifierP(UserName_: U_String; OnResult: TC40_UserDB_VM_On_GetPrimaryIdentifierP);
    // get json object
    procedure Usr_Get_Bridge(UserName_, ObjName_: U_String; Bridge_IO_: TPeerIO);
    procedure Usr_GetC(UserName_, ObjName_: U_String; OnResult: TC40_UserDB_VM_On_GetC);
    procedure Usr_GetM(UserName_, ObjName_: U_String; OnResult: TC40_UserDB_VM_On_GetM);
    procedure Usr_GetP(UserName_, ObjName_: U_String; OnResult: TC40_UserDB_VM_On_GetP);
    // set json object
    procedure Usr_Set(UserName_, ObjName_: U_String; Json_: TZJ);
    // admin
    procedure Usr_SearchM(text_: U_String; maxNum_: Integer; OnResult: TOnStream_M);
    procedure Usr_SearchP(text_: U_String; maxNum_: Integer; OnResult: TOnStream_P);
    procedure Usr_Upload(Json: TZJ); overload;
    procedure Usr_Upload(L_: TZJL); overload;
    procedure Usr_Remove(UserName_: U_String); overload;
    procedure Usr_Remove(L_: TPascalStringList); overload;
  end;

  TC40_UserDB_VM_Client_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_UserDB_VM_Client>;

implementation

constructor TC40_UserDB_VM_Service_SendTunnel_NoAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
end;

destructor TC40_UserDB_VM_Service_SendTunnel_NoAuth.Destroy;
begin
  inherited Destroy;
end;

constructor TC40_UserDB_VM_Service_RecvTunnel_NoAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  OpenUserIdentifier := TC40_UserDB_VM_Json_Hash_Pool.Create(False, 1024, nil);
end;

destructor TC40_UserDB_VM_Service_RecvTunnel_NoAuth.Destroy;
begin
  DisposeObject(OpenUserIdentifier);
  inherited Destroy;
end;

procedure TC40_UserDB_VM_Service_RecvTunnel_NoAuth.SendUser_Msg(FromUserName_, ToUserName_, Msg_: U_String);
var
  D: TDFE;
begin
  if not LinkOk then
      exit;
  D := TDFE.Create;
  D.WriteString(FromUserName_);
  D.WriteString(ToUserName_);
  D.WriteString(Msg_);
  SendTunnel.Owner.SendDirectStreamCmd('Usr_Msg', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Service_RecvTunnel_NoAuth.SendUser_Open(UserName_, ToUserName_: U_String);
var
  D: TDFE;
begin
  if not LinkOk then
      exit;
  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(ToUserName_);
  SendTunnel.Owner.SendDirectStreamCmd('Usr_Open', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Service_RecvTunnel_NoAuth.SendUser_Close(UserName_, ToUserName_: U_String);
var
  D: TDFE;
begin
  if not LinkOk then
      exit;
  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(ToUserName_);
  SendTunnel.Owner.SendDirectStreamCmd('Usr_Close', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Service_RecvTunnel_NoAuth.SendUser_Request_Friend(FromUserName_, DestFriendUserName_, Msg_: U_String);
var
  D: TDFE;
begin
  if not LinkOk then
      exit;
  D := TDFE.Create;
  D.WriteString(FromUserName_);
  D.WriteString(DestFriendUserName_);
  D.WriteString(Msg_);
  SendTunnel.Owner.SendDirectStreamCmd('Usr_Request_Friend', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Service_RecvTunnel_NoAuth.SendUser_Kick(UserName_: U_String);
var
  D: TDFE;
begin
  if not LinkOk then
      exit;
  D := TDFE.Create;
  D.WriteString(UserName_);
  SendTunnel.Owner.SendDirectStreamCmd('Usr_Kick', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_Open(Sender: TPeerIO; InData: TDFE);
var
  Recv_IO_Def: TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TC40_UserDB_VM_Service_SendTunnel_NoAuth;
  UserName_: U_String;
  Json: TZDB2_Json;
  identifier_arry: TZJArry;
  msg_arry: TZJArry;
  friend_arry: TZJArry;
  request_friend_arry: TZJArry;
  i: Integer;
  FromUserName_, Msg_: U_String;
begin
  Recv_IO_Def := Sender.IODefine as TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  if not Recv_IO_Def.LinkOk then
      exit;
  Send_IO_Def := Recv_IO_Def.SendTunnel as TC40_UserDB_VM_Service_SendTunnel_NoAuth;
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  UserName_ := UserIdentifierHash[UserName_].Data.S['PrimaryIdentifier'];
  Json := UserIdentifierHash[UserName_];

  identifier_arry := Json.Data.A['Identifier'];
  for i := 0 to identifier_arry.Count - 1 do
      Recv_IO_Def.OpenUserIdentifier.Add(identifier_arry.S[i], Json);

  msg_arry := Json.Data.A['Msg'];
  for i := 0 to msg_arry.Count - 1 do
    begin
      FromUserName_ := umlGetFirstStr_Discontinuity(msg_arry.S[i], '|');
      Msg_ := umlDeleteFirstStr_Discontinuity(msg_arry.S[i], '|');
      Recv_IO_Def.SendUser_Msg(FromUserName_, UserName_, Msg_);
    end;
  msg_arry.Clear;

  friend_arry := Json.Data.A['friend'];
  for i := 0 to friend_arry.Count - 1 do
      Recv_IO_Def.SendUser_Open(UserName_, friend_arry.S[i]);

  request_friend_arry := Json.Data.A['request_friend'];
  for i := 0 to request_friend_arry.Count - 1 do
    begin
      FromUserName_ := umlGetFirstStr_Discontinuity(request_friend_arry.S[i], '|');
      Msg_ := umlDeleteFirstStr_Discontinuity(request_friend_arry.S[i], '|');
      Recv_IO_Def.SendUser_Request_Friend(FromUserName_, UserName_, Msg_);
    end;
  request_friend_arry.Clear;
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_Close(Sender: TPeerIO; InData: TDFE);
var
  Recv_IO_Def: TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TC40_UserDB_VM_Service_SendTunnel_NoAuth;
  UserName_: U_String;
  Json: TZDB2_Json;
  identifier_arry: TZJArry;
  friend_arry: TZJArry;
  i: Integer;
begin
  Recv_IO_Def := Sender.IODefine as TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  if not Recv_IO_Def.LinkOk then
      exit;
  Send_IO_Def := Recv_IO_Def.SendTunnel as TC40_UserDB_VM_Service_SendTunnel_NoAuth;
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  UserName_ := UserIdentifierHash[UserName_].Data.S['PrimaryIdentifier'];
  Json := UserIdentifierHash[UserName_];

  identifier_arry := Json.Data.A['Identifier'];
  for i := 0 to identifier_arry.Count - 1 do
      Recv_IO_Def.OpenUserIdentifier.Delete(identifier_arry.S[i]);

  friend_arry := Json.Data.A['friend'];
  for i := 0 to friend_arry.Count - 1 do
      Recv_IO_Def.SendUser_Close(UserName_, friend_arry.S[i]);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_IsOpen(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_: U_String;
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  L: TC40_UserDB_VM_Service_RecvTunnel_NoAuth_List;
  i: Integer;
  found_: Boolean;
begin
  L := TC40_UserDB_VM_Service_RecvTunnel_NoAuth_List.Create;
  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if (IO_ <> nil) and TC40_UserDB_VM_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
        begin
          Recv_IO_Def := IO_.IODefine as TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
          L.Add(Recv_IO_Def);
        end;
    end;
  SetLength(Arry_, 0);

  while InData.R.NotEnd do
    begin
      UserName_ := InData.R.ReadString;
      found_ := False;
      if UserIdentifierHash.Exists(UserName_) then
        begin
          UserName_ := UserIdentifierHash[UserName_].Data.S['PrimaryIdentifier'];
          for i := 0 to L.Count - 1 do
            if L[i].OpenUserIdentifier.Exists(UserName_) then
              begin
                found_ := True;
                break;
              end;
        end;
      OutData.WriteBool(found_);
    end;

  DisposeObject(L);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_Msg(Sender: TPeerIO; InData: TDFE);
var
  FromUserName_, ToUserName_, Msg_: U_String;
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TC40_UserDB_VM_Service_SendTunnel_NoAuth;
  sendSuccessed_: Boolean;
  Json: TZDB2_Json;
  arry: TZJArry;
begin
  FromUserName_ := InData.R.ReadString;
  ToUserName_ := InData.R.ReadString;
  Msg_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(FromUserName_) then
      exit;
  if not UserIdentifierHash.Exists(ToUserName_) then
      exit;

  FromUserName_ := UserIdentifierHash[FromUserName_].Data.S['PrimaryIdentifier'];
  ToUserName_ := UserIdentifierHash[ToUserName_].Data.S['PrimaryIdentifier'];

  sendSuccessed_ := False;
  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if (IO_ <> nil) and TC40_UserDB_VM_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
        begin
          Recv_IO_Def := IO_.IODefine as TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
          Send_IO_Def := Recv_IO_Def.SendTunnel as TC40_UserDB_VM_Service_SendTunnel_NoAuth;
          if Recv_IO_Def.OpenUserIdentifier.Exists(ToUserName_) then
            begin
              Recv_IO_Def.SendUser_Msg(FromUserName_, ToUserName_, Msg_);
              sendSuccessed_ := True;
            end;
        end;
    end;
  SetLength(Arry_, 0);

  if not sendSuccessed_ then
    begin
      Json := UserIdentifierHash[ToUserName_];
      arry := Json.Data.A['Msg'];
      arry.Add(PFormat('%s|%s', [FromUserName_.Text, Msg_.Text]));
    end;
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_GetFriends(Sender: TPeerIO; InData, OutData: TDFE);
var
  Recv_IO_Def: TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TC40_UserDB_VM_Service_SendTunnel_NoAuth;
  UserName_: U_String;
  Json: TZDB2_Json;
  friend_arry: TZJArry;
  i: Integer;
begin
  Recv_IO_Def := Sender.IODefine as TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  if not Recv_IO_Def.LinkOk then
      exit;
  Send_IO_Def := Recv_IO_Def.SendTunnel as TC40_UserDB_VM_Service_SendTunnel_NoAuth;
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  Json := UserIdentifierHash[UserName_];

  friend_arry := Json.Data.A['friend'];
  for i := 0 to friend_arry.Count - 1 do
      OutData.WriteString(friend_arry.S[i]);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_RemoveFriend(Sender: TPeerIO; InData: TDFE);
var
  Recv_IO_Def: TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TC40_UserDB_VM_Service_SendTunnel_NoAuth;
  UserName_, DestFriendUserName_: U_String;
  Json: TZDB2_Json;
  friend_arry: TZJArry;
  i: Integer;
begin
  Recv_IO_Def := Sender.IODefine as TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  if not Recv_IO_Def.LinkOk then
      exit;
  Send_IO_Def := Recv_IO_Def.SendTunnel as TC40_UserDB_VM_Service_SendTunnel_NoAuth;
  UserName_ := InData.R.ReadString;
  DestFriendUserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  if not UserIdentifierHash.Exists(DestFriendUserName_) then
      exit;
  DestFriendUserName_ := UserIdentifierHash[DestFriendUserName_].Data.S['PrimaryIdentifier'];

  Json := UserIdentifierHash[UserName_];
  friend_arry := Json.Data.A['friend'];
  i := 0;
  while i < friend_arry.Count do
    if DestFriendUserName_.Same(friend_arry.S[i]) then
        friend_arry.Delete(i)
    else
        inc(i);

  Json := UserIdentifierHash[DestFriendUserName_];
  friend_arry := Json.Data.A['friend'];
  i := 0;
  while i < friend_arry.Count do
    if UserName_.Same(friend_arry.S[i]) then
        friend_arry.Delete(i)
    else
        inc(i);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_RequestAddFriend(Sender: TPeerIO; InData: TDFE);
var
  FromUserName_, DestFriendUserName_, Msg_: U_String;

  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TC40_UserDB_VM_Service_SendTunnel_NoAuth;
  sendSuccessed_: Boolean;

  Json: TZDB2_Json;
  request_friend_arry: TZJArry;
  i: Integer;
begin
  FromUserName_ := InData.R.ReadString;
  DestFriendUserName_ := InData.R.ReadString;
  Msg_ := InData.R.ReadString;

  if not UserIdentifierHash.Exists(FromUserName_) then
      exit;
  if not UserIdentifierHash.Exists(DestFriendUserName_) then
      exit;
  FromUserName_ := UserIdentifierHash[FromUserName_].Data.S['PrimaryIdentifier'];
  DestFriendUserName_ := UserIdentifierHash[DestFriendUserName_].Data.S['PrimaryIdentifier'];

  sendSuccessed_ := False;
  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if (IO_ <> nil) and TC40_UserDB_VM_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
        begin
          Recv_IO_Def := IO_.IODefine as TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
          Send_IO_Def := Recv_IO_Def.SendTunnel as TC40_UserDB_VM_Service_SendTunnel_NoAuth;
          if Recv_IO_Def.OpenUserIdentifier.Exists(DestFriendUserName_) then
            begin
              Recv_IO_Def.SendUser_Request_Friend(FromUserName_, DestFriendUserName_, Msg_);
              sendSuccessed_ := True;
            end;
        end;
    end;
  SetLength(Arry_, 0);

  if not sendSuccessed_ then
    begin
      Json := UserIdentifierHash[DestFriendUserName_];
      request_friend_arry := Json.Data.A['request_friend'];
      request_friend_arry.Add(PFormat('%s|%s', [FromUserName_.Text, Msg_.Text]));
    end;
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_ReponseAddFriend(Sender: TPeerIO; InData: TDFE);
var
  Recv_IO_Def: TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TC40_UserDB_VM_Service_SendTunnel_NoAuth;

  FromUserName_, DestFriendUserName_, Msg_: U_String;
  Accept_: Boolean;
  Json: TZDB2_Json;
  friend_arry: TZJArry;
  i: Integer;
  found_: Boolean;
begin
  Recv_IO_Def := Sender.IODefine as TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  if not Recv_IO_Def.LinkOk then
      exit;
  Send_IO_Def := Recv_IO_Def.SendTunnel as TC40_UserDB_VM_Service_SendTunnel_NoAuth;

  FromUserName_ := InData.R.ReadString;
  DestFriendUserName_ := InData.R.ReadString;
  Msg_ := InData.R.ReadString;
  Accept_ := InData.R.ReadBool;

  if not UserIdentifierHash.Exists(FromUserName_) then
      exit;
  if not UserIdentifierHash.Exists(DestFriendUserName_) then
      exit;

  FromUserName_ := UserIdentifierHash[FromUserName_].Data.S['PrimaryIdentifier'];
  DestFriendUserName_ := UserIdentifierHash[DestFriendUserName_].Data.S['PrimaryIdentifier'];

  if Accept_ then
    begin
      Json := UserIdentifierHash[FromUserName_];
      friend_arry := Json.Data.A['friend'];
      found_ := False;
      for i := 0 to friend_arry.Count - 1 do
        if DestFriendUserName_.Same(friend_arry.S[i]) then
          begin
            found_ := True;
            break;
          end;
      if not found_ then
          friend_arry.Add(DestFriendUserName_);

      Json := UserIdentifierHash[DestFriendUserName_];
      friend_arry := Json.Data.A['friend'];
      found_ := False;
      for i := 0 to friend_arry.Count - 1 do
        if FromUserName_.Same(friend_arry.S[i]) then
          begin
            found_ := True;
            break;
          end;
      if not found_ then
          friend_arry.Add(FromUserName_);
    end
  else
    begin
      Json := UserIdentifierHash[FromUserName_];
      friend_arry := Json.Data.A['friend'];
      i := 0;
      while i < friend_arry.Count do
        if DestFriendUserName_.Same(friend_arry.S[i]) then
            friend_arry.Delete(i)
        else
            inc(i);

      Json := UserIdentifierHash[DestFriendUserName_];
      friend_arry := Json.Data.A['friend'];
      i := 0;
      while i < friend_arry.Count do
        if FromUserName_.Same(friend_arry.S[i]) then
            friend_arry.Delete(i)
        else
            inc(i);
    end;
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_OnlineNum(Sender: TPeerIO; InData, OutData: TDFE);
var
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  num: Integer;
begin
  num := 0;
  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if (IO_ <> nil) and TC40_UserDB_VM_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
        begin
          Recv_IO_Def := IO_.IODefine as TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
          inc(num, Recv_IO_Def.OpenUserIdentifier.Count);
        end;
    end;
  SetLength(Arry_, 0);

  OutData.WriteInteger(num);
  OutData.WriteInteger(UserIdentifierHash.Count);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_OnlineList(Sender: TPeerIO; InData, OutData: TDFE);
var
  Max_Num: Integer;
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
{$IFDEF FPC}
  procedure do_fpc_progress(const Name_: PSystemString; Obj_: TZDB2_Json);
  begin
    if OutData.Count < Max_Num then
        OutData.WriteString(Obj_.Data.S['PrimaryIdentifier']);
  end;
{$ENDIF FPC}


begin
  Max_Num := InData.R.ReadInteger;

  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if (IO_ <> nil) and TC40_UserDB_VM_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
        begin
          Recv_IO_Def := IO_.IODefine as TC40_UserDB_VM_Service_RecvTunnel_NoAuth;

          if OutData.Count < Max_Num then
            begin
{$IFDEF FPC}
              Recv_IO_Def.OpenUserIdentifier.ProgressP(@do_fpc_progress);
{$ELSE FPC}
              Recv_IO_Def.OpenUserIdentifier.ProgressP(procedure(const Name_: PSystemString; Obj_: TZDB2_Json)
                begin
                  if OutData.Count < Max_Num then
                      OutData.WriteString(Obj_.Data.S['PrimaryIdentifier']);
                end);
{$ENDIF FPC}
            end;
        end;
    end;
  SetLength(Arry_, 0);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_Kick(Sender: TPeerIO; InData: TDFE);
var
  UserName_: U_String;
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TC40_UserDB_VM_Service_SendTunnel_NoAuth;
begin
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  UserName_ := UserIdentifierHash[UserName_].Data.S['PrimaryIdentifier'];

  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if (IO_ <> nil) and TC40_UserDB_VM_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
        begin
          Recv_IO_Def := IO_.IODefine as TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
          Send_IO_Def := Recv_IO_Def.SendTunnel as TC40_UserDB_VM_Service_SendTunnel_NoAuth;
          if Recv_IO_Def.OpenUserIdentifier.Exists(UserName_) then
            begin
              Recv_IO_Def.OpenUserIdentifier.Delete(UserName_);
              Recv_IO_Def.SendUser_Kick(UserName_);
            end;
        end;
    end;
  SetLength(Arry_, 0);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_Enabled(Sender: TPeerIO; InData: TDFE);
var
  UserName_: U_String;
  Json: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  Json := UserIdentifierHash[UserName_];
  Json.Data.B['Enabled'] := True;
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_Disable(Sender: TPeerIO; InData: TDFE);
var
  UserName_: U_String;
  Json: TZDB2_Json;
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TC40_UserDB_VM_Service_SendTunnel_NoAuth;
begin
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  Json := UserIdentifierHash[UserName_];
  Json.Data.B['Enabled'] := False;

  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if (IO_ <> nil) and TC40_UserDB_VM_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
        begin
          Recv_IO_Def := IO_.IODefine as TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
          Send_IO_Def := Recv_IO_Def.SendTunnel as TC40_UserDB_VM_Service_SendTunnel_NoAuth;
          if Recv_IO_Def.OpenUserIdentifier.Exists(UserName_) then
            begin
              Recv_IO_Def.OpenUserIdentifier.Delete(UserName_);
              Recv_IO_Def.SendUser_Kick(UserName_);
            end;
        end;
    end;
  SetLength(Arry_, 0);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_Reg(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_, passwd_: U_String;
  Json: TZDB2_Json;
  arry: TZJArry;
  i: Integer;
begin
  UserName_ := InData.R.ReadString;
  passwd_ := InData.R.ReadString;
  if (length(UserName_.Bytes) < 4) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('User name "%s" is too short', [UserName_.Text]);
      exit;
    end;

  if UserName_.Exists([':', '/', '\', '?', '*', '"', '|', ',']) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('User name "%s" illegal symbols', [UserName_.Text]);
      exit;
    end;

  if (length(UserName_.Bytes) > Max_Reg_Identifier_Size) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('User name "%s" is too long', [UserName_.Text]);
      exit;
    end;

  if (length(passwd_.Bytes) < 6) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('password is too short');
      exit;
    end;

  if UserIdentifierHash.Exists(UserName_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('repeat user "%s"', [UserName_.Text]);
      exit;
    end;

  Json := JsonDatabase.NewData;
  arry := Json.Data.A['Identifier'];
  arry.Add(UserName_);
  Json.Data.S['PrimaryIdentifier'] := UserName_;
  Json.Data.S['Password'] := GenerateQuantumCryptographyPassword(passwd_.LowerText);
  Json.Data.D['RegTime'] := umlNow;
  Json.Data.D['LastAuth'] := umlNow;
  Json.Data.B['Enabled'] := True;
  for i := 0 to arry.Count - 1 do
      UserIdentifierHash.Add(arry.S[i], Json);
  OutData.WriteBool(True);
  OutData.WriteString('user "%s" registration done.', [UserName_.Text]);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_Exists(Sender: TPeerIO; InData, OutData: TDFE);
begin
  OutData.WriteBool(UserIdentifierHash.Exists(InData.R.ReadString));
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_Auth(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_, passwd_: U_String;
  Json: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;
  passwd_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found user "%s"', [UserName_.Text]);
      exit;
    end;

  Json := UserIdentifierHash[UserName_];
  if (Json.Data.IndexOf('Enabled') >= 0) and (not Json.Data.B['Enabled']) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('account "%s" is Disable.', [UserName_.Text]);
      exit;
    end;

  if CompareQuantumCryptographyPassword(passwd_.LowerText, Json.Data.S['Password']) then
    begin
      Json.Data.D['LastAuth'] := umlNow;
      OutData.WriteBool(True);
      OutData.WriteString('user "%s" auth successed.', [UserName_.Text]);
    end
  else
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth password from user "%s"', [UserName_.Text]);
    end;
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_ChangePassword(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_, passwd_, NewPasswd_: U_String;
  Json: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;
  passwd_ := InData.R.ReadString;
  NewPasswd_ := InData.R.ReadString;

  if (length(NewPasswd_.Bytes) < 6) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('new password is too short');
      exit;
    end;

  if not UserIdentifierHash.Exists(UserName_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found user "%s"', [UserName_.Text]);
      exit;
    end;

  Json := UserIdentifierHash[UserName_];

  if CompareQuantumCryptographyPassword(passwd_.LowerText, Json.Data.S['Password']) then
    begin
      Json.Data.S['Password'] := GenerateQuantumCryptographyPassword(NewPasswd_.LowerText);
      OutData.WriteBool(True);
      OutData.WriteString('"%s" change password successed.', [UserName_.Text]);
    end
  else
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no match password from user "%s"', [UserName_.Text]);
    end;
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_ResetPassword(Sender: TPeerIO; InData: TDFE);
var
  UserName_, NewPasswd_: U_String;
  Json: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;
  NewPasswd_ := InData.R.ReadString;
  if (length(NewPasswd_.Bytes) < 6) then
      exit;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  Json := UserIdentifierHash[UserName_];
  Json.Data.S['Password'] := GenerateQuantumCryptographyPassword(NewPasswd_.LowerText);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_NewIdentifier(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_, NewIdentifier_: U_String;
  Json: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;
  NewIdentifier_ := InData.R.ReadString;

  if (length(NewIdentifier_.Bytes) < 4) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('New Identifier is too short');
      exit;
    end;

  if NewIdentifier_.Exists([':', '/', '\', '?', '*', '"', '|', ',']) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('New Identifier "%s" illegal symbols', [NewIdentifier_.Text]);
      exit;
    end;

  if UserIdentifierHash.Exists(NewIdentifier_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('found user "%s"', [NewIdentifier_.Text]);
      exit;
    end;

  if not UserIdentifierHash.Exists(UserName_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found user "%s"', [UserName_.Text]);
      exit;
    end;

  Json := UserIdentifierHash[UserName_];
  Json.Data.A['Identifier'].Add(NewIdentifier_);
  UserIdentifierHash.Add(NewIdentifier_, Json);
  OutData.WriteBool(True);
  OutData.WriteString('new Identifier "%s" for user "%s"', [NewIdentifier_.Text, UserName_.Text]);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_GetPrimaryIdentifier(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_: U_String;
  Json: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;

  if not UserIdentifierHash.Exists(UserName_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found user "%s"', [UserName_.Text]);
      exit;
    end;

  Json := UserIdentifierHash[UserName_];
  OutData.WriteBool(True);
  OutData.WriteString('primary Identifier "%s"', [Json.Data.S['PrimaryIdentifier']]);
  OutData.WriteString(Json.Data.S['PrimaryIdentifier']);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_Get(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserName_, ObjName_: U_String;
  Json: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found user "%s"', [UserName_.Text]);
      exit;
    end;
  ObjName_ := InData.R.ReadString;
  Json := UserIdentifierHash[UserName_];
  OutData.WriteBool(True);
  OutData.WriteString('get user "%s" json object %s', [UserName_.Text, ObjName_.Text]);
  OutData.WriteJson(Json.Data.O[ObjName_]);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_Set(Sender: TPeerIO; InData: TDFE);
var
  UserName_, ObjName_: U_String;
  Json: TZDB2_Json;
begin
  UserName_ := InData.R.ReadString;
  if not UserIdentifierHash.Exists(UserName_) then
      exit;
  ObjName_ := InData.R.ReadString;
  Json := UserIdentifierHash[UserName_];
  InData.R.ReadJson(Json.Data.O[ObjName_]);
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_Search(Sender: TPeerIO; InData, OutData: TDFE);
var
  SearchText: U_String;
  MaxNum: Integer;
  Json: TZDB2_Json;
  n: U_String;
begin
  SearchText := InData.R.ReadString;
  MaxNum := InData.R.ReadInteger;
  if JsonDatabase.Count > 0 then
    with JsonDatabase.Invert_Repeat_ do
      repeat
        Json := Queue^.Data;
        if Json.Data_Direct = nil then
          begin
            n := Json.Data.ToJSONString(False);
            Json.RecycleMemory;
          end
        else
          begin
            n := Json.Data.ToJSONString(False);
          end;
        if (SearchText.L = 0) or (n.GetPos(@SearchText) > 0) then
          begin
            OutData.WriteString(n);
            if (MaxNum > 0) and (OutData.Count > MaxNum) then
                break;
          end;
      until not Prev;
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_Upload(Sender: TPeerIO; InData: TDFE);
var
  tmp: TZJ;
  arry: TZJArry;
  i: Integer;
  Json: TZDB2_Json;
  found_: Boolean;
begin
  while InData.R.NotEnd do
    begin
      tmp := TZJ.Create;
      try
        InData.R.ReadJson(tmp);

        found_ := (tmp.IndexOf('PrimaryIdentifier') >= 0) and UserIdentifierHash.Exists(tmp.S['PrimaryIdentifier']);
        if (not found_) and (tmp.IndexOf('Identifier') >= 0) then
          begin
            arry := tmp.A['Identifier'];
            for i := 0 to arry.Count - 1 do
              if UserIdentifierHash.Exists(arry.S[i]) then
                begin
                  found_ := True;
                  break;
                end;
          end;

        if not found_ then
          begin
            Json := JsonDatabase.NewData;
            Json.Data.SwapInstance(tmp);
            arry := Json.Data.A['Identifier'];
            for i := 0 to arry.Count - 1 do
              begin
                UserIdentifierHash.Add(arry.S[i], Json);
                if Json.Data.IndexOf('PrimaryIdentifier') < 0 then
                    Json.Data.S['PrimaryIdentifier'] := arry.S[i];
                if not C40_QuietMode then
                    DoStatus('user "%s" import done.', [arry.S[i]]);
              end;
          end;
      except
      end;
      DisposeObject(tmp);
    end;
end;

procedure TC40_UserDB_VM_Service.cmd_Usr_Remove(Sender: TPeerIO; InData: TDFE);
var
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  UserName_: U_String;
  Json_: TZDB2_Json;
  identifier_arry: TZJArry;
  i: Integer;
begin
  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  while InData.R.NotEnd do
    begin
      UserName_ := InData.R.ReadString;
      if UserIdentifierHash.Exists(UserName_) then
        begin
          UserName_ := UserIdentifierHash[UserName_].Data.S['PrimaryIdentifier'];
          Json_ := UserIdentifierHash[UserName_];
          identifier_arry := Json_.Data.A['Identifier'];
          for i := 0 to identifier_arry.Count - 1 do
            begin
              for ID_ in Arry_ do
                begin
                  IO_ := DTNoAuthService.RecvTunnel[ID_];
                  if (IO_ <> nil) and TC40_UserDB_VM_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
                    begin
                      Recv_IO_Def := IO_.IODefine as TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
                      Recv_IO_Def.SendUser_Kick(UserName_);
                      Recv_IO_Def.OpenUserIdentifier.Delete(identifier_arry.S[i]);
                    end;
                end;
              UserIdentifierHash.Delete(identifier_arry.S[i]);
            end;
          JsonDatabase.Remove(Json_, True);
        end;
    end;
end;

procedure TC40_UserDB_VM_Service.CC_Compress_And_Reload(var OP_Param: TOpParam);
var
  New_F: U_String;
  FS: TCore_FileStream;
begin
  New_F := Get_New_ZDB2_Extract_FileName(C40_UserDB_VM_FileName);
  FS := TCore_FileStream.Create(New_F, fmCreate);
  JsonDatabase.ExtractTo(FS);
  DisposeObject(FS);
end;

constructor TC40_UserDB_VM_Service.Create(Param_: U_String);
var
  FS: TCore_FileStream;
  i, j: Integer;
  Json: TZDB2_Json;
  identifier_arry: TZJArry;
begin
  inherited Create(Param_);
  DTNoAuthService.RecvTunnel.UserDefineClass := TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  DTNoAuthService.SendTunnel.UserDefineClass := TC40_UserDB_VM_Service_SendTunnel_NoAuth;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Open').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Open;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Close').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Close;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_IsOpen').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_IsOpen;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Msg').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Msg;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_GetFriends').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_GetFriends;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_RemoveFriend').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_RemoveFriend;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_RequestAddFriend').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_RequestAddFriend;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_ReponseAddFriend').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_ReponseAddFriend;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_OnlineNum').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_OnlineNum;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_OnlineList').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_OnlineList;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Kick').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Kick;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Enabled').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Enabled;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Disable').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Disable;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Reg').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Reg;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Exists').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Exists;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Auth').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Auth;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_ChangePassword').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_ChangePassword;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_ResetPassword').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_ResetPassword;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_NewIdentifier').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_NewIdentifier;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_GetPrimaryIdentifier').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_GetPrimaryIdentifier;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Get').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Get;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Set').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Set;
  DTNoAuthService.RecvTunnel.RegisterStream('Usr_Search').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Search;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Upload').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Upload;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Usr_Remove').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Remove;

  C40_UserDB_VM_FileName := umlCombineFileName(DTNoAuthService.PublicFileDirectory, Get_DB_FileName_Config(PFormat('DTC40_%s.Space', ['UserDB_VM'])));
  Check_And_Replace_ZDB2_Extract_FileName(C40_UserDB_VM_FileName);

  UserIdentifierHash := TC40_UserDB_VM_Json_Hash_Pool.Create(False,
    EStrToInt64(ParamList.GetDefaultValue('Identifier_HashPool', '4*1024*1024'), 4 * 1024 * 1024),
    nil);
  UserIdentifierHash.AccessOptimization := True;
  UserIdentifierHash.IgnoreCase := True;

  if EStrToBool(ParamList.GetDefaultValue('ForeverSave', 'True'), True) and umlFileExists(C40_UserDB_VM_FileName) then
      FS := TCore_FileStream.Create(C40_UserDB_VM_FileName, fmOpenReadWrite)
  else
      FS := TCore_FileStream.Create(C40_UserDB_VM_FileName, fmCreate);

  ZDB2RecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('RecycleMemory', '60*1000'), 60 * 1000);
  ZDB2DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('DeltaSpace', '16*1024*1024'), 16 * 1024 * 1024);
  ZDB2BlockSize := EStrToInt(ParamList.GetDefaultValue('BlockSize', '1024'), 1024);
  ZDB2EnabledCipher := EStrToBool(ParamList.GetDefaultValue('EnabledCipher', 'False'), False);
  ZDB2CipherName := ParamList.GetDefaultValue('Cipher', TCipher.CCipherSecurityName[TCipherSecurity.csNone]);
  ZDB2Password := ParamList.GetDefaultValue('Password', Z.Net.C4.C40_Password);
  Max_Reg_Identifier_Size := EStrToInt(ParamList.GetDefaultValue('Max_Reg_Identifier_Size', '30'), 30);

  if ZDB2EnabledCipher then
      ZDB2Cipher := TZDB2_Cipher.Create(ZDB2CipherName, ZDB2Password, 1, True, True)
  else
      ZDB2Cipher := nil;

  JsonDatabase := TZDB2_List_Json.Create(TZDB2_Json, nil, ZDB2RecycleMemoryTimeOut, FS, False, ZDB2DeltaSpace, ZDB2BlockSize, ZDB2Cipher);
  JsonDatabase.AutoFreeStream := True;

  if not C40_QuietMode then
      DoStatus('extract user Database.');
  if JsonDatabase.Count > 0 then
    with JsonDatabase.Repeat_ do
      repeat
        Json := Queue^.Data;
        identifier_arry := Json.Data.A['Identifier'];
        for i := 0 to identifier_arry.Count - 1 do
          begin
            if UserIdentifierHash.Exists(identifier_arry.S[i]) then
              begin
                if not C40_QuietMode then
                    DoStatus('repeat user %s', [identifier_arry.S[i]]);
                UserIdentifierHash.Add(identifier_arry.S[i], Json);
              end
            else
                UserIdentifierHash.FastAdd(identifier_arry.S[i], Json);
          end;
        Json.RecycleMemory;
      until not Next;
  JsonDatabase.Flush;
  if not C40_QuietMode then
      DoStatus('extract user Database done.');

  Register_ConsoleCommand('Compress_And_Reload', 'Compress and reload.').OnEvent_M := {$IFDEF FPC}@{$ENDIF FPC}CC_Compress_And_Reload;
end;

destructor TC40_UserDB_VM_Service.Destroy;
begin
  DisposeObject(UserIdentifierHash);
  DisposeObject(JsonDatabase);
  DisposeObject(ZDB2Cipher);
  inherited Destroy;
end;

procedure TC40_UserDB_VM_Service.SafeCheck;
begin
  inherited SafeCheck;
  JsonDatabase.Flush;
end;

procedure TC40_UserDB_VM_Service.Progress;
begin
  inherited Progress;
  JsonDatabase.Progress;
end;

procedure TC40_UserDB_VM_Service.SendMsg(FromUserName_, ToUserName_, Msg_: U_String);
var
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  Recv_IO_Def: TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
  Send_IO_Def: TC40_UserDB_VM_Service_SendTunnel_NoAuth;
  sendSuccessed_: Boolean;
  Json: TZDB2_Json;
  arry: TZJArry;
begin
  if not UserIdentifierHash.Exists(FromUserName_) then
      exit;
  if not UserIdentifierHash.Exists(ToUserName_) then
      exit;

  sendSuccessed_ := False;
  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if (IO_ <> nil) and TC40_UserDB_VM_Service_RecvTunnel_NoAuth(IO_.UserDefine).LinkOk then
        begin
          Recv_IO_Def := IO_.IODefine as TC40_UserDB_VM_Service_RecvTunnel_NoAuth;
          Send_IO_Def := Recv_IO_Def.SendTunnel as TC40_UserDB_VM_Service_SendTunnel_NoAuth;
          if Recv_IO_Def.OpenUserIdentifier.Exists(ToUserName_) then
            begin
              Recv_IO_Def.SendUser_Msg(FromUserName_, ToUserName_, Msg_);
              sendSuccessed_ := True;
            end;
        end;
    end;
  SetLength(Arry_, 0);

  if not sendSuccessed_ then
    begin
      Json := UserIdentifierHash[ToUserName_];
      arry := Json.Data.A['Msg'];
      arry.Add(PFormat('%s|%s', [FromUserName_.Text, Msg_.Text]));
    end;
end;

constructor TC40_UserDB_VM_On_IsOpen.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_UserDB_VM_On_IsOpen.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: TArrayBool;
  i: Integer;
begin
  SetLength(State_, Result_.Count);
  for i := 0 to Result_.Count - 1 do
      State_[i] := Result_.ReadBool(i);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TC40_UserDB_VM_On_IsOpen.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: TArrayBool;
  i: Integer;
begin
  SetLength(State_, SendData.Count);
  for i := 0 to SendData.Count - 1 do
      State_[i] := False;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_UserDB_VM_On_GetFriends.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_UserDB_VM_On_GetFriends.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  FriendArry: U_StringArray;
  i: Integer;
begin
  SetLength(FriendArry, Result_.Count);
  for i := 0 to Result_.Count - 1 do
      FriendArry[i] := Result_.ReadString(i);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, FriendArry);
    if Assigned(OnResultM) then
        OnResultM(Client, FriendArry);
    if Assigned(OnResultP) then
        OnResultP(Client, FriendArry);
  except
  end;
  SetLength(FriendArry, 0);
  DelayFreeObject(1.0, self);
end;

procedure TC40_UserDB_VM_On_GetFriends.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  FriendArry: U_StringArray;
begin
  SetLength(FriendArry, 0);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, FriendArry);
    if Assigned(OnResultM) then
        OnResultM(Client, FriendArry);
    if Assigned(OnResultP) then
        OnResultP(Client, FriendArry);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_UserDB_VM_On_OnlineNum.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_UserDB_VM_On_OnlineNum.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  Online_Num, User_Num: Integer;
  i: Integer;
begin
  Online_Num := Result_.R.ReadInteger;
  User_Num := Result_.R.ReadInteger;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Online_Num, User_Num);
    if Assigned(OnResultM) then
        OnResultM(Client, Online_Num, User_Num);
    if Assigned(OnResultP) then
        OnResultP(Client, Online_Num, User_Num);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TC40_UserDB_VM_On_OnlineNum.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  Online_Num, User_Num: Integer;
  i: Integer;
begin
  Online_Num := 0;
  User_Num := 0;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Online_Num, User_Num);
    if Assigned(OnResultM) then
        OnResultM(Client, Online_Num, User_Num);
    if Assigned(OnResultP) then
        OnResultP(Client, Online_Num, User_Num);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_UserDB_VM_On_OnlineList.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_UserDB_VM_On_OnlineList.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  arry: U_StringArray;
  i: Integer;
begin
  SetLength(arry, Result_.Count);
  for i := 0 to Result_.Count - 1 do
      arry[i] := Result_.ReadString(i);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  SetLength(arry, 0);
  DelayFreeObject(1.0, self);
end;

procedure TC40_UserDB_VM_On_OnlineList.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  arry: U_StringArray;
begin
  SetLength(arry, 0);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_UserDB_VM_On_Reg.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_UserDB_VM_On_Reg.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TC40_UserDB_VM_On_Reg.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_UserDB_VM_On_Exists.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_UserDB_VM_On_Exists.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
begin
  State_ := False;
  if Result_.Count > 0 then
    begin
      State_ := Result_.R.ReadBool;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TC40_UserDB_VM_On_Exists.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
begin
  State_ := False;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_UserDB_VM_On_Auth.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_UserDB_VM_On_Auth.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TC40_UserDB_VM_On_Auth.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_UserDB_VM_On_ChangePassword.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_UserDB_VM_On_ChangePassword.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TC40_UserDB_VM_On_ChangePassword.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_UserDB_VM_On_NewIdentifier.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_UserDB_VM_On_NewIdentifier.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TC40_UserDB_VM_On_NewIdentifier.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_UserDB_VM_On_Get.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_UserDB_VM_On_Get.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
  Json: TZJ;
begin
  State_ := False;
  info_ := 'error.';
  Json := TZJ.Create;
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
      if State_ then
          Result_.R.ReadJson(Json);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_, Json);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_, Json);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_, Json);
  except
  end;
  DelayFreeObject(1.0, self, Json);
end;

procedure TC40_UserDB_VM_On_Get.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_: SystemString;
  Json: TZJ;
begin
  State_ := False;
  info_ := 'error.';
  Json := TZJ.Create;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_, Json);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_, Json);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_, Json);
  except
  end;
  DelayFreeObject(1.0, self, Json);
end;

constructor TC40_UserDB_VM_On_GetPrimaryIdentifier.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_UserDB_VM_On_GetPrimaryIdentifier.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_, PrimaryIdentifier_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  PrimaryIdentifier_ := '';
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
      if State_ then
          PrimaryIdentifier_ := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_, PrimaryIdentifier_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_, PrimaryIdentifier_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_, PrimaryIdentifier_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TC40_UserDB_VM_On_GetPrimaryIdentifier.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_, PrimaryIdentifier_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  PrimaryIdentifier_ := '';

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_, PrimaryIdentifier_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_, PrimaryIdentifier_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_, PrimaryIdentifier_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TC40_UserDB_VM_Client.cmd_Usr_Msg(Sender: TPeerIO; InData: TDFE);
var
  FromUserName_, ToUserName_, Msg_: U_String;
begin
  FromUserName_ := InData.R.ReadString;
  ToUserName_ := InData.R.ReadString;
  Msg_ := InData.R.ReadString;
  try
    if Assigned(ON_C40_UserDB_VM_Client_Notify) then
        ON_C40_UserDB_VM_Client_Notify.Do_User_Msg(self, FromUserName_, ToUserName_, Msg_);
  except
  end;
end;

procedure TC40_UserDB_VM_Client.cmd_Usr_Open(Sender: TPeerIO; InData: TDFE);
var
  UserName_, ToUserName_: U_String;
begin
  UserName_ := InData.R.ReadString;
  ToUserName_ := InData.R.ReadString;
  try
    if Assigned(ON_C40_UserDB_VM_Client_Notify) then
        ON_C40_UserDB_VM_Client_Notify.Do_User_Open(self, UserName_, ToUserName_);
  except
  end;
end;

procedure TC40_UserDB_VM_Client.cmd_Usr_Close(Sender: TPeerIO; InData: TDFE);
var
  UserName_, ToUserName_: U_String;
begin
  UserName_ := InData.R.ReadString;
  ToUserName_ := InData.R.ReadString;
  try
    if Assigned(ON_C40_UserDB_VM_Client_Notify) then
        ON_C40_UserDB_VM_Client_Notify.Do_User_Close(self, UserName_, ToUserName_);
  except
  end;
end;

procedure TC40_UserDB_VM_Client.cmd_Usr_Request_Friend(Sender: TPeerIO; InData: TDFE);
var
  FromUserName_, DestFriendUserName_, Msg_: U_String;
begin
  FromUserName_ := InData.R.ReadString;
  DestFriendUserName_ := InData.R.ReadString;
  Msg_ := InData.R.ReadString;
  try
    if Assigned(ON_C40_UserDB_VM_Client_Notify) then
        ON_C40_UserDB_VM_Client_Notify.Do_User_Request_Friend(self, FromUserName_, DestFriendUserName_, Msg_);
  except
  end;
end;

procedure TC40_UserDB_VM_Client.cmd_Usr_Kick(Sender: TPeerIO; InData: TDFE);
var
  UserName_: U_String;
begin
  UserName_ := InData.R.ReadString;
  try
    if Assigned(ON_C40_UserDB_VM_Client_Notify) then
        ON_C40_UserDB_VM_Client_Notify.Do_User_Kick(self, UserName_);
  except
  end;
end;

constructor TC40_UserDB_VM_Client.Create(Param_: U_String);
begin
  inherited Create(Param_);
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('Usr_Msg').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Msg;
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('Usr_Open').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Open;
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('Usr_Close').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Close;
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('Usr_Request_Friend').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Request_Friend;
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('Usr_Kick').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Usr_Kick;
  ON_C40_UserDB_VM_Client_Notify := nil;
end;

destructor TC40_UserDB_VM_Client.Destroy;
begin
  ON_C40_UserDB_VM_Client_Notify := nil;
  inherited Destroy;
end;

procedure TC40_UserDB_VM_Client.Usr_Open(UserName_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Open', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_Close(UserName_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Close', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_IsOpen_Bridge(UserName_: U_String; Bridge_IO_: TPeerIO);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_IsOpen', D, {$IFDEF FPC}@{$ENDIF FPC}TStreamEventBridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_IsOpenC(UserName_: U_String; OnResult: TC40_UserDB_VM_On_IsOpenC);
var
  tmp: TC40_UserDB_VM_On_IsOpen;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_IsOpen.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_IsOpen', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_IsOpenM(UserName_: U_String; OnResult: TC40_UserDB_VM_On_IsOpenM);
var
  tmp: TC40_UserDB_VM_On_IsOpen;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_IsOpen.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_IsOpen', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_IsOpenP(UserName_: U_String; OnResult: TC40_UserDB_VM_On_IsOpenP);
var
  tmp: TC40_UserDB_VM_On_IsOpen;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_IsOpen.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_IsOpen', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_IsOpen_Bridge(UserName_: U_StringArray; Bridge_IO_: TPeerIO);
var
  D: TDFE;
  i: Integer;
begin
  D := TDFE.Create;
  for i := 0 to length(UserName_) - 1 do
      D.WriteString(UserName_[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_IsOpen', D, {$IFDEF FPC}@{$ENDIF FPC}TStreamEventBridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_IsOpenC(UserName_: U_StringArray; OnResult: TC40_UserDB_VM_On_IsOpenC);
var
  tmp: TC40_UserDB_VM_On_IsOpen;
  D: TDFE;
  i: Integer;
begin
  tmp := TC40_UserDB_VM_On_IsOpen.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  for i := 0 to length(UserName_) - 1 do
      D.WriteString(UserName_[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_IsOpen', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_IsOpenM(UserName_: U_StringArray; OnResult: TC40_UserDB_VM_On_IsOpenM);
var
  tmp: TC40_UserDB_VM_On_IsOpen;
  D: TDFE;
  i: Integer;
begin
  tmp := TC40_UserDB_VM_On_IsOpen.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  for i := 0 to length(UserName_) - 1 do
      D.WriteString(UserName_[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_IsOpen', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_IsOpenP(UserName_: U_StringArray; OnResult: TC40_UserDB_VM_On_IsOpenP);
var
  tmp: TC40_UserDB_VM_On_IsOpen;
  D: TDFE;
  i: Integer;
begin
  tmp := TC40_UserDB_VM_On_IsOpen.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  for i := 0 to length(UserName_) - 1 do
      D.WriteString(UserName_[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_IsOpen', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_Msg(FromUserName_, ToUserName_, Msg_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(FromUserName_);
  D.WriteString(ToUserName_);
  D.WriteString(Msg_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Msg', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_GetFriends_Bridge(UserName_: U_String; Bridge_IO_: TPeerIO);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_GetFriends', D, {$IFDEF FPC}@{$ENDIF FPC}TStreamEventBridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_GetFriendsC(UserName_: U_String; OnResult: TC40_UserDB_VM_On_GetFriendsC);
var
  tmp: TC40_UserDB_VM_On_GetFriends;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_GetFriends.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_GetFriends', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_GetFriendsM(UserName_: U_String; OnResult: TC40_UserDB_VM_On_GetFriendsM);
var
  tmp: TC40_UserDB_VM_On_GetFriends;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_GetFriends.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_GetFriends', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_GetFriendsP(UserName_: U_String; OnResult: TC40_UserDB_VM_On_GetFriendsP);
var
  tmp: TC40_UserDB_VM_On_GetFriends;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_GetFriends.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_GetFriends', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_RemoveFriend(UserName_, DestFriendUserName_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(DestFriendUserName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_RemoveFriend', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_RequestAddFriend(FromUserName_, DestFriendUserName_, Msg_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(FromUserName_);
  D.WriteString(DestFriendUserName_);
  D.WriteString(Msg_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_RequestAddFriend', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_ReponseAddFriend(FromUserName_, DestFriendUserName_, Msg_: U_String; Accept_: Boolean);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(FromUserName_);
  D.WriteString(DestFriendUserName_);
  D.WriteString(Msg_);
  D.WriteBool(Accept_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_ReponseAddFriend', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_OnlineNum_Bridge(Bridge_IO_: TPeerIO);
var
  D: TDFE;
begin
  D := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_OnlineNum', D, {$IFDEF FPC}@{$ENDIF FPC}TStreamEventBridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_OnlineNumC(OnResult: TC40_UserDB_VM_On_OnlineNumC);
var
  tmp: TC40_UserDB_VM_On_OnlineNum;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_OnlineNum.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_OnlineNum', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_OnlineNumM(OnResult: TC40_UserDB_VM_On_OnlineNumM);
var
  tmp: TC40_UserDB_VM_On_OnlineNum;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_OnlineNum.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_OnlineNum', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_OnlineNumP(OnResult: TC40_UserDB_VM_On_OnlineNumP);
var
  tmp: TC40_UserDB_VM_On_OnlineNum;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_OnlineNum.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_OnlineNum', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_OnlineList_Bridge(Max_Num: Integer; Bridge_IO_: TPeerIO);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteInteger(Max_Num);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_OnlineList', D, {$IFDEF FPC}@{$ENDIF FPC}TStreamEventBridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_OnlineListC(Max_Num: Integer; OnResult: TC40_UserDB_VM_On_OnlineListC);
var
  tmp: TC40_UserDB_VM_On_OnlineList;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_OnlineList.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteInteger(Max_Num);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_OnlineList', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_OnlineListM(Max_Num: Integer; OnResult: TC40_UserDB_VM_On_OnlineListM);
var
  tmp: TC40_UserDB_VM_On_OnlineList;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_OnlineList.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteInteger(Max_Num);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_OnlineList', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_OnlineListP(Max_Num: Integer; OnResult: TC40_UserDB_VM_On_OnlineListP);
var
  tmp: TC40_UserDB_VM_On_OnlineList;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_OnlineList.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteInteger(Max_Num);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_OnlineList', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_Kick(UserName_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Kick', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_Enabled(UserName_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Enabled', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_Disable(UserName_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Disable', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_Reg_Bridge(UserName_, passwd_: U_String; Bridge_IO_: TPeerIO);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Reg', D, {$IFDEF FPC}@{$ENDIF FPC}TStreamEventBridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_RegC(UserName_, passwd_: U_String; OnResult: TC40_UserDB_VM_On_RegC);
var
  tmp: TC40_UserDB_VM_On_Reg;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_Reg.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Reg', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_RegM(UserName_, passwd_: U_String; OnResult: TC40_UserDB_VM_On_RegM);
var
  tmp: TC40_UserDB_VM_On_Reg;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_Reg.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Reg', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_RegP(UserName_, passwd_: U_String; OnResult: TC40_UserDB_VM_On_RegP);
var
  tmp: TC40_UserDB_VM_On_Reg;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_Reg.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Reg', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_Exists_Bridge(UserName_: U_String; Bridge_IO_: TPeerIO);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Exists', D, {$IFDEF FPC}@{$ENDIF FPC}TStreamEventBridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_ExistsC(UserName_: U_String; OnResult: TC40_UserDB_VM_On_ExistsC);
var
  tmp: TC40_UserDB_VM_On_Exists;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_Exists.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Exists', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_ExistsM(UserName_: U_String; OnResult: TC40_UserDB_VM_On_ExistsM);
var
  tmp: TC40_UserDB_VM_On_Exists;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_Exists.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Exists', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_ExistsP(UserName_: U_String; OnResult: TC40_UserDB_VM_On_ExistsP);
var
  tmp: TC40_UserDB_VM_On_Exists;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_Exists.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Exists', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_Auth_Bridge(UserName_, passwd_: U_String; Bridge_IO_: TPeerIO);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Auth', D, {$IFDEF FPC}@{$ENDIF FPC}TStreamEventBridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_AuthC(UserName_, passwd_: U_String; OnResult: TC40_UserDB_VM_On_AuthC);
var
  tmp: TC40_UserDB_VM_On_Auth;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_Auth.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Auth', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_AuthM(UserName_, passwd_: U_String; OnResult: TC40_UserDB_VM_On_AuthM);
var
  tmp: TC40_UserDB_VM_On_Auth;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_Auth.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Auth', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_AuthP(UserName_, passwd_: U_String; OnResult: TC40_UserDB_VM_On_AuthP);
var
  tmp: TC40_UserDB_VM_On_Auth;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_Auth.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Auth', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_ChangePassword_Bridge(UserName_, passwd_, NewPasswd_: U_String; Bridge_IO_: TPeerIO);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  D.WriteString(NewPasswd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_ChangePassword', D, {$IFDEF FPC}@{$ENDIF FPC}TStreamEventBridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_ChangePasswordC(UserName_, passwd_, NewPasswd_: U_String; OnResult: TC40_UserDB_VM_On_ChangePasswordC);
var
  tmp: TC40_UserDB_VM_On_ChangePassword;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_ChangePassword.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  D.WriteString(NewPasswd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_ChangePassword', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_ChangePasswordM(UserName_, passwd_, NewPasswd_: U_String; OnResult: TC40_UserDB_VM_On_ChangePasswordM);
var
  tmp: TC40_UserDB_VM_On_ChangePassword;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_ChangePassword.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  D.WriteString(NewPasswd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_ChangePassword', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_ChangePasswordP(UserName_, passwd_, NewPasswd_: U_String; OnResult: TC40_UserDB_VM_On_ChangePasswordP);
var
  tmp: TC40_UserDB_VM_On_ChangePassword;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_ChangePassword.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(passwd_);
  D.WriteString(NewPasswd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_ChangePassword', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_ResetPassword(UserName_, NewPasswd_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(NewPasswd_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_ResetPassword', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_NewIdentifier_Bridge(UserName_, NewIdentifier_: U_String; Bridge_IO_: TPeerIO);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(NewIdentifier_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_NewIdentifier', D, {$IFDEF FPC}@{$ENDIF FPC}TStreamEventBridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_NewIdentifierC(UserName_, NewIdentifier_: U_String; OnResult: TC40_UserDB_VM_On_NewIdentifierC);
var
  tmp: TC40_UserDB_VM_On_NewIdentifier;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_NewIdentifier.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(NewIdentifier_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_NewIdentifier', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_NewIdentifierM(UserName_, NewIdentifier_: U_String; OnResult: TC40_UserDB_VM_On_NewIdentifierM);
var
  tmp: TC40_UserDB_VM_On_NewIdentifier;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_NewIdentifier.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(NewIdentifier_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_NewIdentifier', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_NewIdentifierP(UserName_, NewIdentifier_: U_String; OnResult: TC40_UserDB_VM_On_NewIdentifierP);
var
  tmp: TC40_UserDB_VM_On_NewIdentifier;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_NewIdentifier.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(NewIdentifier_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_NewIdentifier', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_GetPrimaryIdentifier_Bridge(UserName_: U_String; Bridge_IO_: TPeerIO);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_GetPrimaryIdentifier', D, {$IFDEF FPC}@{$ENDIF FPC}TStreamEventBridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_GetPrimaryIdentifierC(UserName_: U_String; OnResult: TC40_UserDB_VM_On_GetPrimaryIdentifierC);
var
  tmp: TC40_UserDB_VM_On_GetPrimaryIdentifier;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_GetPrimaryIdentifier.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_GetPrimaryIdentifier', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_GetPrimaryIdentifierM(UserName_: U_String; OnResult: TC40_UserDB_VM_On_GetPrimaryIdentifierM);
var
  tmp: TC40_UserDB_VM_On_GetPrimaryIdentifier;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_GetPrimaryIdentifier.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_GetPrimaryIdentifier', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_GetPrimaryIdentifierP(UserName_: U_String; OnResult: TC40_UserDB_VM_On_GetPrimaryIdentifierP);
var
  tmp: TC40_UserDB_VM_On_GetPrimaryIdentifier;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_GetPrimaryIdentifier.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_GetPrimaryIdentifier', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_Get_Bridge(UserName_, ObjName_: U_String; Bridge_IO_: TPeerIO);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(ObjName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Get', D, {$IFDEF FPC}@{$ENDIF FPC}TStreamEventBridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_GetC(UserName_, ObjName_: U_String; OnResult: TC40_UserDB_VM_On_GetC);
var
  tmp: TC40_UserDB_VM_On_Get;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_Get.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(ObjName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Get', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_GetM(UserName_, ObjName_: U_String; OnResult: TC40_UserDB_VM_On_GetM);
var
  tmp: TC40_UserDB_VM_On_Get;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_Get.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(ObjName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Get', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_GetP(UserName_, ObjName_: U_String; OnResult: TC40_UserDB_VM_On_GetP);
var
  tmp: TC40_UserDB_VM_On_Get;
  D: TDFE;
begin
  tmp := TC40_UserDB_VM_On_Get.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(ObjName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Get', D, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_Set(UserName_, ObjName_: U_String; Json_: TZJ);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  D.WriteString(ObjName_);
  D.WriteJson(Json_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Set', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_SearchM(text_: U_String; maxNum_: Integer; OnResult: TOnStream_M);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(text_);
  D.WriteInteger(maxNum_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Usr_Search', D, OnResult);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_SearchP(text_: U_String; maxNum_: Integer; OnResult: TOnStream_P);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(text_);
  D.WriteInteger(maxNum_);
  DTNoAuthClient.SendTunnel.SendStreamCmdP('Usr_Search', D, OnResult);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_Upload(Json: TZJ);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteJson(Json);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Upload', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_Upload(L_: TZJL);
var
  D: TDFE;
  i: Integer;
begin
  D := TDFE.Create;
  for i := 0 to L_.Count - 1 do
      D.WriteJson(L_[i]);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Upload', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_Remove(UserName_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(UserName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Remove', D);
  DisposeObject(D);
end;

procedure TC40_UserDB_VM_Client.Usr_Remove(L_: TPascalStringList);
var
  D: TDFE;
  i: Integer;
begin
  D := TDFE.Create;
  for i := 0 to L_.Count - 1 do
      D.WriteString(L_[i]);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Usr_Remove', D);
  DisposeObject(D);
end;

end.
