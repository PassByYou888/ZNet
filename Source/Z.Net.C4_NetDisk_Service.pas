{ ****************************************************************************** }
{ * cloud 4.0 network disk VM Service                                          * }
{ ****************************************************************************** }
unit Z.Net.C4_NetDisk_Service;

{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine,
  Z.Geometry2D, Z.DFE, Z.Json, Z.Expression,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.ZDB2,
  Z.GHashList,
  Z.LinearAction,
  Z.Net, Z.Net.PhysicsIO,
  Z.Net.DoubleTunnelIO.NoAuth,
  Z.Net.C4_NetDisk_Directory,
  Z.Net.C4_UserDB,
  Z.Net.C4_FS2,
  Z.Net.C4_Log_DB,
  Z.Net.C4_TEKeyValue,
  Z.Net.C4;

type
{$REGION 'define_bridge'}
  TC40_NetDisk_Service = class;

  TC40_NetDisk_Service_SendTunnel_NoAuth = class(TPeerClientUserDefineForSendTunnel_NoAuth)
  public
    VM_Service: TC40_NetDisk_Service;
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TC40_NetDisk_Service_RecvTunnel_NoAuth = class(TPeerClientUserDefineForRecvTunnel_NoAuth)
  private
    NetDisk_File_Name: U_String;
    NetDisk_File_Field, NetDisk_File_Item: U_String;
    NetDisk_File_Frag: TDirectory_MD5_Data_Frag_Struct_List;
    NetDisk_File_Client: TC40_FS2_Client;
  public
    VM_Service: TC40_NetDisk_Service;
    AuthDone: Boolean;
    UserName: U_String;
    PrimaryIdentifier: U_String;
    UserJson: TZJ;
    UserJson_MD5: TMD5;
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
    procedure Reinit;
  end;

  TC40_NetDisk_Service_RecvIO_Define_List = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericsList<TC40_NetDisk_Service_RecvTunnel_NoAuth>;

  TC40_NetDisk_Service_Auth_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    IO_Def_: TC40_NetDisk_Service_RecvTunnel_NoAuth;
    procedure Do_ExistsDB(sender: TC40_NetDisk_Directory_Client; Successed: Boolean);
    procedure Do_Usr_GetDetail(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString; Json_: TZJ);
    procedure Do_Usr_GetPrimaryIdentifier(sender: TC40_UserDB_Client; State_: Boolean; info_, PrimaryIdentifier_: SystemString);
    procedure Do_Usr_Auth(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
    procedure Do_Usr_Exists(sender: TC40_UserDB_Client; State_: Boolean);
  end;

  TC40_NetDisk_Service_Reg_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    userName_, Passwd_: U_String;
    procedure Do_NewDB(sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString);
    procedure Do_Usr_Reg(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
  end;

  TC40_NetDisk_Service_NewIdentifier_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    procedure Do_Usr_NewIdentifier(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
  end;

  TC40_NetDisk_Service_GetAlias_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    procedure Do_Usr_Get(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString; Json_: TZJ);
  end;

  TC40_NetDisk_Service_GetMyFriends_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    procedure Do_Usr_GetFriends(sender: TC40_UserDB_Client; FriendArry: U_StringArray);
  end;

  TC40_NetDisk_Service_GetOnlineNum_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    procedure Do_Usr_OnlineNum(sender: TC40_UserDB_Client; Online_Num, User_Num: Integer);
  end;

  TC40_NetDisk_Service_GetOnlineList_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    procedure Do_Usr_OnlineList(sender: TC40_UserDB_Client; arry: U_StringArray);
  end;

  TC40_NetDisk_Service_SearchMultiMD5_FS_Service_Bridge = class(TCustomEventBridge)
  private type
    TPair = class
    public
      FS2: TC40_FS2_Client;
      FoundNum: Integer;
      IsDone: Boolean;
    end;

    TPair_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TPair>;
  public
    VM_Service: TC40_NetDisk_Service;
    FS_Pair: TPair_List;
    procedure Sort_FS_Pair;
    procedure Do_FS2_SearchMultiMD5(sender: TC40_FS2_Client; L: TFS2_SearchMultiMD5_State_List);
  end;

  TC40_NetDisk_Service_CheckAndCopy_NetDisk_File_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    file_MD5: TMD5;
    file_Name: U_String;
    file_Field: U_String;
    file_item: U_String;
    file_Size: Int64;
    file_time: Double;
    procedure Do_PutItemMD5(sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString);
  end;

  TC40_NetDisk_Service_CheckAndCopy_NetDisk_File_Frag_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    frag_md5_: TMD5;
    frag_pos_: Int64;
    frag_size_: Int64;
    procedure Do_CheckMD5AndFastCopy(sender: TC40_FS2_Client; State_: Boolean);
  end;

  TC40_NetDisk_Service_Post_NetDisk_File_Frag_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    p: PDirectory_MD5_Data_Frag_Struct;
    Event_: UInt64;
    procedure Do_FS2_PostFile_Done(sender: TC40_FS2_Client; info_: U_String);
  end;

  TC40_NetDisk_Service_EndPost_NetDisk_File_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    procedure Do_PutItemFrag(sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString);
  end;

  TC40_NetDisk_Service_Get_NetDisk_File_Frag_Info_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    procedure Do_GetItemFrag(sender: TC40_NetDisk_Directory_Client; Successed: Boolean; L: TDirectory_MD5_Data_Frag_Struct_List);
  end;

  TC40_NetDisk_Service_Get_NetDisk_File_Frag_MD5_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    alias_or_hash_: U_String;
    FS_File: U_String;
    procedure Do_FS2_GetFileMD5(sender: TC40_FS2_Client; State_: Boolean; info_: SystemString; MD5_: TMD5);
  end;

  TC40_NetDisk_Service_Get_NetDisk_File_Frag_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    alias_or_hash_: U_String;
    FS_File: U_String;
    Pos_: Int64;
    Event_: UInt64;
    procedure Do_FS2_GetFile_Done(sender: TC40_FS2_Client; Stream: TMS64; info_: U_String; Successed: Boolean);
  end;

  TC40_NetDisk_Service_Get_NetDisk_File_List_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    DB_Field: U_String;
    procedure Do_GetItemList(sender: TC40_NetDisk_Directory_Client; Field_Path: U_String; arry: TItemList_Data_Array);
  end;

  TC40_NetDisk_Service_Get_NetDisk_SpaceInfo_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    procedure Do_SpaceInfo(sender: TC40_NetDisk_Directory_Client; Field_Num, Item_Num, ItemSpace: Int64);
  end;

  TC40_NetDisk_Service_Build_Share_Disk_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    Share_Directory_DB_Name: U_String;
    procedure Do_NewDB(sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString);
  end;

  TC40_NetDisk_Service_Get_Share_Disk_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    procedure Do_GetKey(sender: TC40_TEKeyValue_Client; arry: U_StringArray);
  end;

  TC40_NetDisk_Service_Get_Share_Disk_File_List_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    Share_Directory_DB_Name: U_String;
    DB_Field: U_String;
    procedure Do_GetItemList(sender: TC40_NetDisk_Directory_Client; Field_Path: U_String; arry: TItemList_Data_Array);
  end;

  TC40_NetDisk_Service_Get_Share_Disk_File_Frag_Info_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    procedure Do_GetItemFrag(sender: TC40_NetDisk_Directory_Client; Successed: Boolean; L: TDirectory_MD5_Data_Frag_Struct_List);
  end;

  TC40_NetDisk_Service_Search_NetDisk_File_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    DB_Field, DB_Search: U_String;
    procedure Do_SearchItem(sender: TC40_NetDisk_Directory_Client; SearchResult: TON_SearchItem_Data_array);
  end;

  TC40_NetDisk_Service_Search_Share_NetDisk_File_Bridge = class(TCustomEventBridge)
  public
    VM_Service: TC40_NetDisk_Service;
    Share_Directory_DB_Name: U_String;
    DB_Field, DB_Search: U_String;
    procedure Do_SearchItem(sender: TC40_NetDisk_Directory_Client; SearchResult: TON_SearchItem_Data_array);
  end;

{$ENDREGION 'define_bridge'}

  TC40_NetDisk_Service_PrimaryIdentifier_Pool = {$IFDEF FPC}specialize {$ENDIF FPC}TGeneric_String_Object_Hash<TC40_NetDisk_Service_RecvIO_Define_List>;

  TC40_NetDisk_Service = class(TC40_Base_NoAuth_Service, I_ON_C40_UserDB_Client_Notify, I_ON_C40_NetDisk_Directory_Client_Interface)
  protected
    FPrimaryIdentifier_Pool: TC40_NetDisk_Service_PrimaryIdentifier_Pool;
    procedure Add_PrimaryIdentifier(RecvIO_Define_: TC40_NetDisk_Service_RecvTunnel_NoAuth);
    procedure Remove_PrimaryIdentifier(RecvIO_Define_: TC40_NetDisk_Service_RecvTunnel_NoAuth);
    function Check_PrimaryIdentifier(RecvIO_Define_: TC40_NetDisk_Service_RecvTunnel_NoAuth): Boolean;
  protected
    procedure DoLinkSuccess_Event(sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
    procedure DoUserOut_Event(sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
  protected
    // user db interface
    FUserDB_Client: TC40_UserDB_Client;
    function Get_UserDB_Client: TC40_UserDB_Client;
    procedure Set_UserDB_Client(const Value: TC40_UserDB_Client);
    // user db event
    procedure Do_User_Msg(sender: TC40_UserDB_Client; FromUserName_, ToUserName_, Msg_: U_String);
    procedure Do_User_Open(sender: TC40_UserDB_Client; userName_, ToUserName_: U_String);
    procedure Do_User_Close(sender: TC40_UserDB_Client; userName_, ToUserName_: U_String);
    procedure Do_User_Request_Friend(sender: TC40_UserDB_Client; FromUserName_, DestFriendUserName_, Msg_: U_String);
    procedure Do_User_Kick(sender: TC40_UserDB_Client; userName_: U_String);
  protected
    // directory interface
    FDirectory_Client: TC40_NetDisk_Directory_Client;
    function Get_Directory_Client: TC40_NetDisk_Directory_Client;
    procedure Set_Directory_Client(const Value: TC40_NetDisk_Directory_Client);
    procedure Do_Remove_Directory_MD5(arry: U_StringArray);
    procedure Do_Remove_Directory_Invalid_Frag(arry: U_StringArray);
  protected
    // log interface
    FLog_Client: TC40_Log_DB_Client;
    function Get_Log_Client: TC40_Log_DB_Client;
    procedure Set_Log_Client(const Value: TC40_Log_DB_Client);
  protected
    // TEKeyValue Interface
    FTEKeyValue_Client: TC40_TEKeyValue_Client;
    function Get_TEKeyValue_Client: TC40_TEKeyValue_Client;
    procedure Set_TEKeyValue_Client(const Value: TC40_TEKeyValue_Client);
  protected
    // FS2.0 interface
    FFS2_Client_Pool: TC40_FS2_Client_List;
  protected
    // user
    procedure cmd_Auth(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Reg(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_NewLoginName(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_NewAlias(sender: TPeerIO; InData: TDFE);
    procedure cmd_GetAlias(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Msg(sender: TPeerIO; InData: TDFE);
    procedure cmd_RequestFriend(sender: TPeerIO; InData: TDFE);
    procedure cmd_ReponseFriend(sender: TPeerIO; InData: TDFE);
    procedure cmd_RemoveFriend(sender: TPeerIO; InData: TDFE);
    procedure cmd_GetMyFriends(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_GetOnlineNum(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_GetOnlineList(sender: TPeerIO; InData, OutData: TDFE);
    // netdisk
    procedure cmd_Get_NetDisk_Config(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Get_FS_Service(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_SearchMultiMD5_FS_Service(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_CheckAndCopy_NetDisk_File(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_BeginPost_NetDisk_File(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_CheckAndCopy_NetDisk_File_Frag(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Post_NetDisk_File_Frag(sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure cmd_EndPost_NetDisk_File(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Get_NetDisk_File_Frag_Info(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Get_NetDisk_File_Frag_MD5(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Get_NetDisk_File_Frag(sender: TPeerIO; InData: TDFE);
    procedure cmd_Get_NetDisk_File_List(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Get_NetDisk_SpaceInfo(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Remove_Item(sender: TPeerIO; InData: TDFE);
    procedure cmd_Remove_Field(sender: TPeerIO; InData: TDFE);
    procedure cmd_Copy_Item(sender: TPeerIO; InData: TDFE);
    procedure cmd_Copy_Field(sender: TPeerIO; InData: TDFE);
    procedure cmd_CreateField(sender: TPeerIO; InData: TDFE);
    procedure cmd_RenameField(sender: TPeerIO; InData: TDFE);
    procedure cmd_RenameItem(sender: TPeerIO; InData: TDFE);
    procedure cmd_Build_Share_Disk(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Get_Share_Disk(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Remove_Share_Disk(sender: TPeerIO; InData: TDFE);
    procedure cmd_Get_Share_Disk_File_List(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Get_Share_Disk_File_Frag_Info(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Search_NetDisk_File(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Search_Share_NetDisk_File(sender: TPeerIO; InData, OutData: TDFE);
    // admin
    procedure cmd_Auth_Admin(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Close_Auth_Admin(sender: TPeerIO; InData: TDFE);
  public
    // deployment
    property UserDB_Client: TC40_UserDB_Client read Get_UserDB_Client write Set_UserDB_Client;
    property Directory_Client: TC40_NetDisk_Directory_Client read Get_Directory_Client write Set_Directory_Client;
    property TEKeyValue_Client: TC40_TEKeyValue_Client read Get_TEKeyValue_Client write Set_TEKeyValue_Client;
    property FS2_Client_Pool: TC40_FS2_Client_List read FFS2_Client_Pool;
    property Log_Client: TC40_Log_DB_Client read Get_Log_Client write Set_Log_Client;
    // automated config.
    procedure Automated_Config_NetDisk_Service_Relevance;
    function Check_NetDisk_Service_Relevance(Status_: Boolean): Boolean; overload;
    function Check_NetDisk_Service_Relevance(): Boolean; overload;
  public
    File_Chunk_Size: Int64;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;

    function Search_IO_Def_From_UserPrimaryIdentifier(PrimaryIdentifier_: U_String): TC40_NetDisk_Service_RecvIO_Define_List;
    procedure PostLog(info: SystemString); overload;
    procedure PostLog(info1, info2: SystemString); overload;
    procedure PostLog(const v: SystemString; const Args: array of const); overload;
    procedure PostLog(const v: SystemString; const Args: array of const; info2: SystemString); overload;
    class function PrimaryIdentifierToDirectory(PrimaryIdentifier_: U_String): U_String;
    class function ShareToDirectory(PrimaryIdentifier_: U_String; MD5_: TMD5): U_String;
    class function IsMyShareDirectory(PrimaryIdentifier_, Share_Directory_DB_Name: U_String): Boolean;
    class function IsShareDirectory(Share_Directory_DB_Name: U_String): Boolean;
    // custom user Detail
    class procedure Get_User_Reg_Detail(Json_: TZJ); virtual;
  end;

implementation

constructor TC40_NetDisk_Service_SendTunnel_NoAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  VM_Service := nil;
end;

destructor TC40_NetDisk_Service_SendTunnel_NoAuth.Destroy;
begin
  inherited Destroy;
end;

constructor TC40_NetDisk_Service_RecvTunnel_NoAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  VM_Service := nil;
  AuthDone := False;
  UserName := '';
  PrimaryIdentifier := '';
  UserJson := TZJ.Create;
  UserJson_MD5 := UserJson.MD5;
  NetDisk_File_Name := '';
  NetDisk_File_Field := '';
  NetDisk_File_Item := '';
  NetDisk_File_Frag := TDirectory_MD5_Data_Frag_Struct_List.Create;
  NetDisk_File_Client := nil;
end;

destructor TC40_NetDisk_Service_RecvTunnel_NoAuth.Destroy;
begin
  inherited Destroy;
  DisposeObject(UserJson);
  DisposeObjectAndNil(NetDisk_File_Frag);
end;

procedure TC40_NetDisk_Service_RecvTunnel_NoAuth.Reinit;
begin
  DisposeObject(UserJson);
  DisposeObjectAndNil(NetDisk_File_Frag);

  AuthDone := False;
  UserName := '';
  PrimaryIdentifier := '';
  UserJson := TZJ.Create;
  UserJson_MD5 := UserJson.MD5;
  NetDisk_File_Name := '';
  NetDisk_File_Field := '';
  NetDisk_File_Item := '';
  NetDisk_File_Frag := TDirectory_MD5_Data_Frag_Struct_List.Create;
  NetDisk_File_Client := nil;
end;

procedure TC40_NetDisk_Service_Auth_Bridge.Do_ExistsDB(sender: TC40_NetDisk_Directory_Client; Successed: Boolean);
begin
  if CheckIO then
    begin
      if Successed then
        begin
          IO.OutDataFrame.WriteBool(True);
          IO.OutDataFrame.WriteString('auth and check directory DB successed.');
          IO.OutDataFrame.WriteString(IO_Def_.PrimaryIdentifier);
          IO.ContinueResultSend;
          VM_Service.PostLog('%s auth and check directory DB successed.', [IO_Def_.PrimaryIdentifier.Text]);
          IO_Def_.AuthDone := True;
          VM_Service.Add_PrimaryIdentifier(IO_Def_);
          if (VM_Service.UserDB_Client <> nil) and (VM_Service.UserDB_Client.Connected) then
              VM_Service.UserDB_Client.Usr_Open(IO_Def_.PrimaryIdentifier);
        end
      else
        begin
          IO.OutDataFrame.WriteBool(False);
          IO.OutDataFrame.WriteString('no found directory DB');
          IO.ContinueResultSend;
          VM_Service.PostLog('%s auth failed. no found directory %s.',
            [IO_Def_.PrimaryIdentifier.Text, VM_Service.PrimaryIdentifierToDirectory(IO_Def_.PrimaryIdentifier).Text]);
        end;
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Auth_Bridge.Do_Usr_GetDetail(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString; Json_: TZJ);
begin
  if CheckIO then
    begin
      if State_ then
        begin
          IO_Def_.UserJson.Assign(Json_);
          IO_Def_.UserJson_MD5 := IO_Def_.UserJson.MD5;
          if (VM_Service.Directory_Client <> nil) and (VM_Service.Directory_Client.Connected) then
            begin
              VM_Service.Directory_Client.ExistsDB_M(
                VM_Service.PrimaryIdentifierToDirectory(IO_Def_.PrimaryIdentifier), {$IFDEF FPC}@{$ENDIF FPC}Do_ExistsDB);
              exit;
            end;
          IO.OutDataFrame.WriteBool(False);
          IO.OutDataFrame.WriteString('directory is offline.');
          IO.ContinueResultSend;
          VM_Service.PostLog('%s auth failed. directory is offline.', [IO_Def_.PrimaryIdentifier.Text], info_);
        end
      else
        begin
          IO.OutDataFrame.WriteBool(False);
          IO.OutDataFrame.WriteString(info_);
          IO.ContinueResultSend;
          VM_Service.PostLog('%s Usr_GetDetail failed.', [IO_Def_.PrimaryIdentifier.Text], info_);
        end;
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Auth_Bridge.Do_Usr_GetPrimaryIdentifier(sender: TC40_UserDB_Client; State_: Boolean; info_, PrimaryIdentifier_: SystemString);
begin
  if CheckIO then
    begin
      if State_ and (VM_Service.UserDB_Client <> nil) and (VM_Service.UserDB_Client.Connected) then
        begin
          IO_Def_.PrimaryIdentifier := PrimaryIdentifier_;
          VM_Service.UserDB_Client.Usr_GetM(IO_Def_.UserName, 'Detail', {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_GetDetail);
          exit;
        end;
      IO.OutDataFrame.WriteBool(False);
      IO.OutDataFrame.WriteString('PrimaryIdentifier error.');
      IO.ContinueResultSend;
      VM_Service.PostLog('%s auth failed.', [IO_Def_.PrimaryIdentifier.Text], 'PrimaryIdentifier error.');
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Auth_Bridge.Do_Usr_Auth(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
begin
  if CheckIO then
    begin
      if (State_) and (VM_Service.UserDB_Client <> nil) and (VM_Service.UserDB_Client.Connected) then
        begin
          VM_Service.UserDB_Client.Usr_GetPrimaryIdentifierM(IO_Def_.UserName, {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_GetPrimaryIdentifier);
          exit;
        end;
      IO.OutDataFrame.WriteBool(False);
      IO.OutDataFrame.WriteString(info_);
      IO.ContinueResultSend;
      VM_Service.PostLog('%s auth failed.', [IO_Def_.PrimaryIdentifier.Text], info_);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Auth_Bridge.Do_Usr_Exists(sender: TC40_UserDB_Client; State_: Boolean);
begin
  if CheckIO then
    begin
      if (State_) and (VM_Service.UserDB_Client <> nil) and (VM_Service.UserDB_Client.Connected) then
        begin
          VM_Service.UserDB_Client.Usr_GetPrimaryIdentifierM(IO_Def_.UserName, {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_GetPrimaryIdentifier);
          exit;
        end;
      IO.OutDataFrame.WriteBool(False);
      IO.OutDataFrame.WriteString('no exists user');
      IO.ContinueResultSend;
      VM_Service.PostLog('%s no exists.', [IO_Def_.PrimaryIdentifier.Text]);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Reg_Bridge.Do_NewDB(sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString);
begin
  IO.OutDataFrame.WriteBool(Successed);
  IO.OutDataFrame.WriteString(info);
  IO.ContinueResultSend;
  DelayFreeObj(1.0, self);
  if Successed then
      VM_Service.PostLog('%s create Directory DB successed.', [userName_.Text], info)
  else
      VM_Service.PostLog('%s create Directory DB failed.', [userName_.Text], info)
end;

procedure TC40_NetDisk_Service_Reg_Bridge.Do_Usr_Reg(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
var
  j_: TZJ;
begin
  if CheckIO then
    begin
      if State_ and (VM_Service.Directory_Client <> nil) and (VM_Service.Directory_Client.Connected) then
        begin
          // user alias
          j_ := TZJ.Create;
          j_.S['Alias'] := userName_;
          TC40_NetDisk_Service.Get_User_Reg_Detail(j_);
          VM_Service.UserDB_Client.Usr_Set(userName_, 'Detail', j_);
          DisposeObject(j_);
          // create directory db
          VM_Service.Directory_Client.NewDB_M(VM_Service.PrimaryIdentifierToDirectory(userName_), {$IFDEF FPC}@{$ENDIF FPC}Do_NewDB);
          VM_Service.PostLog('%s reg successed.', [userName_.Text], info_);
          exit;
        end;
      IO.OutDataFrame.WriteBool(False);
      IO.OutDataFrame.WriteString(info_);
      IO.ContinueResultSend;
      VM_Service.PostLog('%s reg failed.', [userName_.Text], info_);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_NewIdentifier_Bridge.Do_Usr_NewIdentifier(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      IO.OutDFE.WriteBool(State_);
      IO.OutDFE.WriteString(info_);
      IO.ContinueResultSend;
      VM_Service.PostLog('%s new Identifier: %s', [IO_Def.PrimaryIdentifier.Text], info_);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_GetAlias_Bridge.Do_Usr_Get(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString; Json_: TZJ);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      if Json_.IndexOf('Alias') >= 0 then
          IO.OutDFE.WriteString(Json_.S['Alias']);
      IO.ContinueResultSend;
      VM_Service.PostLog('%s GetAlias: %s', [IO_Def.PrimaryIdentifier.Text], info_);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_GetMyFriends_Bridge.Do_Usr_GetFriends(sender: TC40_UserDB_Client; FriendArry: U_StringArray);
var
  i: Integer;
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      for i := 0 to length(FriendArry) - 1 do
          IO.OutDFE.WriteString(FriendArry[i]);
      IO.ContinueResultSend;
      VM_Service.PostLog('%s GetFriends', [IO_Def.PrimaryIdentifier.Text]);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_GetOnlineNum_Bridge.Do_Usr_OnlineNum(sender: TC40_UserDB_Client; Online_Num, User_Num: Integer);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      IO.OutDFE.WriteInteger(Online_Num);
      IO.OutDFE.WriteInteger(User_Num);
      IO.ContinueResultSend;
      VM_Service.PostLog('%s Get Online Num.', [IO_Def.PrimaryIdentifier.Text], PFormat('online:%d user:%d', [Online_Num, User_Num]));
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_GetOnlineList_Bridge.Do_Usr_OnlineList(sender: TC40_UserDB_Client; arry: U_StringArray);
var
  i: Integer;
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      for i := low(arry) to high(arry) do
          IO.OutDFE.WriteString(arry[i]);
      VM_Service.PostLog('%s Get Online List.', [IO_Def.PrimaryIdentifier.Text]);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_SearchMultiMD5_FS_Service_Bridge.Sort_FS_Pair;
  function Compare_(Left, Right: TPair): ShortInt;
  begin
    Result := CompareInt64(Left.FoundNum, Right.FoundNum);
    if Result = 0 then
        Result := CompareInt64(Left.FS2.Remote_FS_DB_Size, Right.FS2.Remote_FS_DB_Size);
  end;

  procedure fastSort_(L, R: Integer);
  var
    i, j: TGeoInt;
    p: TPair;
  begin
    repeat
      i := L;
      j := R;
      p := FS_Pair[(L + R) shr 1];
      repeat
        while Compare_(FS_Pair[i], p) < 0 do
            inc(i);
        while Compare_(FS_Pair[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
                FS_Pair.Exchange(i, j);
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          fastSort_(L, j);
      L := i;
    until i >= R;
  end;

begin
  if FS_Pair.count > 1 then
      fastSort_(0, FS_Pair.count - 1);
end;

procedure TC40_NetDisk_Service_SearchMultiMD5_FS_Service_Bridge.Do_FS2_SearchMultiMD5(sender: TC40_FS2_Client; L: TFS2_SearchMultiMD5_State_List);
var
  num: Integer;
  i: Integer;
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
begin
  num := 0;
  for i := 0 to L.count - 1 do
    if L[i].IsFound then
        inc(num);

  for i := 0 to FS_Pair.count - 1 do
    if FS_Pair[i].FS2 = sender then
      begin
        FS_Pair[i].FoundNum := num;
        FS_Pair[i].IsDone := True;
      end;

  for i := 0 to FS_Pair.count - 1 do
    if not FS_Pair[i].IsDone then
        exit;

  if CheckIO then
    begin
      Sort_FS_Pair;
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      IO.OutDataFrame.WriteBool(True);
      IO.OutDataFrame.WriteString(FS_Pair.First.FS2.AliasOrHash);
      IO.ContinueResultSend;
      VM_Service.PostLog('%s SearchMultiMD5.', [IO_Def.PrimaryIdentifier.Text], FS_Pair.First.FS2.AliasOrHash);
    end;

  for i := 0 to FS_Pair.count - 1 do
      DisposeObject(FS_Pair[i]);
  DisposeObject(FS_Pair);

  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_CheckAndCopy_NetDisk_File_Bridge.Do_PutItemMD5(sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      IO.OutDataFrame.WriteBool(Successed);
      IO.OutDataFrame.WriteString(info);
      IO.ContinueResultSend;
      VM_Service.PostLog('%s CheckAndCopy_NetDisk_File.', [IO_Def.PrimaryIdentifier.Text], info);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_CheckAndCopy_NetDisk_File_Frag_Bridge.Do_CheckMD5AndFastCopy(sender: TC40_FS2_Client; State_: Boolean);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  p: PDirectory_MD5_Data_Frag_Struct;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      IO.OutDataFrame.WriteBool(State_);

      if State_ then
        begin
          new(p);
          p^.FS_AliasOrHash := sender.AliasOrHash;
          p^.FS_File := umlMD5ToStr(frag_md5_);
          p^.Pos_ := frag_pos_;
          p^.Size_ := frag_size_;
          IO_Def.NetDisk_File_Frag.Add(p);
          IO.OutDataFrame.WriteString('copy frag "%s" successed.', [p^.FS_File.Text]);
          VM_Service.PostLog('%s CheckAndCopy_NetDisk_File_Frag successed.', [IO_Def.PrimaryIdentifier.Text], PFormat('alias:%s file:%s', [sender.AliasOrHash.Text, umlMD5ToStr(frag_md5_).Text]));
        end
      else
        begin
          IO.OutDataFrame.WriteString('no found frag "%s"', [umlMD5ToStr(frag_md5_).Text]);
          VM_Service.PostLog('%s CheckAndCopy_NetDisk_File_Frag failed.', [IO_Def.PrimaryIdentifier.Text], PFormat('alias:%s file:%s', [sender.AliasOrHash.Text, umlMD5ToStr(frag_md5_).Text]));
        end;

      IO.ContinueResultSend;
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Post_NetDisk_File_Frag_Bridge.Do_FS2_PostFile_Done(sender: TC40_FS2_Client; info_: U_String);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  d: TDFE;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      IO_Def.NetDisk_File_Frag.Add(p);
      d := TDFE.Create;
      d.WriteBool(True);
      d.WritePointer(Event_);
      IO_Def.SendTunnel.Owner.SendDirectStreamCmd('Done_PostFile_Frag', d);
      DisposeObject(d);
      VM_Service.PostLog('%s Post_NetDisk_File_Frag.', [IO_Def.PrimaryIdentifier.Text], info_);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_EndPost_NetDisk_File_Bridge.Do_PutItemFrag(sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      IO_Def.NetDisk_File_Name := '';
      IO_Def.NetDisk_File_Field := '';
      IO_Def.NetDisk_File_Item := '';
      IO_Def.NetDisk_File_Frag.Clean;
      IO_Def.NetDisk_File_Client := nil;
      IO.OutDataFrame.WriteBool(Successed);
      IO.OutDataFrame.WriteString(info);
      IO.ContinueResultSend;
      VM_Service.PostLog('%s EndPost_NetDisk_File.', [IO_Def.PrimaryIdentifier.Text], info);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Get_NetDisk_File_Frag_Info_Bridge.Do_GetItemFrag(sender: TC40_NetDisk_Directory_Client; Successed: Boolean; L: TDirectory_MD5_Data_Frag_Struct_List);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  i: Integer;
  test_passed: Integer;
  d: TDFE;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;

      test_passed := 0;
      for i := 0 to L.count - 1 do
        if Z.Net.C4.C40_ClientPool.FindAliasOrHash(L[i]^.FS_AliasOrHash, True) <> nil then
            inc(test_passed);

      if test_passed >= L.count then
        begin
          IO.OutDataFrame.WriteBool(Successed);
          d := TDFE.Create;
          L.Encode(d);
          IO.OutDataFrame.WriteDataFrame(d);
          IO.ContinueResultSend;
          DisposeObject(d);
          VM_Service.PostLog('%s Get_NetDisk_File_Frag_Info.', [IO_Def.PrimaryIdentifier.Text]);
        end
      else
        begin
          IO.OutDataFrame.WriteBool(False);
          IO.OutDataFrame.WriteString('Get_NetDisk_File_Frag_Info error: no found FS2 AliasOrHash.');
          IO.ContinueResultSend;
        end;
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Get_NetDisk_File_Frag_MD5_Bridge.Do_FS2_GetFileMD5(sender: TC40_FS2_Client; State_: Boolean; info_: SystemString; MD5_: TMD5);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      IO.OutDataFrame.WriteBool(State_);
      IO.OutDataFrame.WriteString(info_);
      IO.OutDataFrame.WriteMD5(MD5_);
      IO.ContinueResultSend;
      VM_Service.PostLog('%s Get_NetDisk_File_Frag_MD5.', [IO_Def.PrimaryIdentifier.Text], info_);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Get_NetDisk_File_Frag_Bridge.Do_FS2_GetFile_Done(sender: TC40_FS2_Client; Stream: TMS64; info_: U_String; Successed: Boolean);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  m64: TMS64;
  d: TDFE;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      if Successed then
        begin
          m64 := TMS64.Create;
          m64.Size := Stream.Size + 16;
          m64.Position := 0;
          m64.WriteInt64(Pos_);
          m64.WriteUInt64(Event_);
          m64.WritePtr(Stream.Memory, Stream.Size);
          IO_Def.SendTunnel.Owner.SendCompleteBuffer('Done_Get_File_Frag', m64, True);
          VM_Service.PostLog('%s Get_NetDisk_File_Frag.', [IO_Def.PrimaryIdentifier.Text], PFormat('pos:%d', [Pos_]));
        end
      else
        begin
          d := TDFE.Create;
          d.WriteInt64(Pos_);
          d.WritePointer(Event_);
          IO_Def.SendTunnel.Owner.SendDirectStreamCmd('Get_File_Error', d);
          DisposeObject(d);
          VM_Service.PostLog('%s Get_NetDisk_File_Frag failed.', [IO_Def.PrimaryIdentifier.Text], PFormat('pos:%d', [Pos_]));
        end;
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Get_NetDisk_File_List_Bridge.Do_GetItemList(sender: TC40_NetDisk_Directory_Client; Field_Path: U_String; arry: TItemList_Data_Array);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  i: Integer;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      IO.OutDataFrame.WriteBool(True);
      IO.OutDataFrame.WriteString(Field_Path);
      for i := Low(arry) to high(arry) do
        begin
          IO.OutDataFrame.WriteString(arry[i].Name);
          IO.OutDataFrame.WriteInt64(arry[i].num);
          IO.OutDataFrame.WriteDouble(arry[i].Time_);
        end;
      IO.ContinueResultSend;
      VM_Service.PostLog('%s Get_NetDisk_File_List.', [IO_Def.PrimaryIdentifier.Text], DB_Field);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Get_NetDisk_SpaceInfo_Bridge.Do_SpaceInfo(sender: TC40_NetDisk_Directory_Client; Field_Num, Item_Num, ItemSpace: Int64);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      IO.OutDataFrame.WriteBool(True);
      IO.OutDataFrame.WriteString('successed.');
      IO.OutDataFrame.WriteInt64(Field_Num);
      IO.OutDataFrame.WriteInt64(Item_Num);
      IO.OutDataFrame.WriteInt64(ItemSpace);
      IO.ContinueResultSend;
      VM_Service.PostLog('%s Get_NetDisk_SpaceInfo.', [IO_Def.PrimaryIdentifier.Text],
        PFormat('filed:%d item:%d space:%d', [Field_Num, Item_Num, ItemSpace]));
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Build_Share_Disk_Bridge.Do_NewDB(sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  Time_: Double;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      if Successed then
        begin
          IO.OutDataFrame.WriteBool(True);
          IO.OutDataFrame.WriteString('done.');
          IO.OutDataFrame.WriteString(Share_Directory_DB_Name);
          IO.ContinueResultSend;

          VM_Service.PostLog('%s Build_Share_Disk %s',
            [IO_Def.PrimaryIdentifier.Text, Share_Directory_DB_Name.Text]);
          if (VM_Service.TEKeyValue_Client <> nil) and (VM_Service.FTEKeyValue_Client.Connected) then
            begin
              VM_Service.TEKeyValue_Client.SetTextValue(IO_Def.PrimaryIdentifier, 'Share', Share_Directory_DB_Name, umlDateTimeToStr(umlNow));
              VM_Service.TEKeyValue_Client.Rebuild(IO_Def.PrimaryIdentifier);
              VM_Service.PostLog('%s Build_Share_Disk %s, and Update KeyValue',
                [IO_Def.PrimaryIdentifier.Text, Share_Directory_DB_Name.Text]);
            end
          else
            begin
              VM_Service.PostLog('%s Build_Share_Disk %s. KeyValue is offline.',
                [IO_Def.PrimaryIdentifier.Text, Share_Directory_DB_Name.Text]);
            end;
        end
      else
        begin
          Time_ := umlNow;
          Share_Directory_DB_Name := VM_Service.ShareToDirectory(IO_Def.PrimaryIdentifier, umlMD5(@Time_, 8));
          sender.NewDB_M(Share_Directory_DB_Name, {$IFDEF FPC}@{$ENDIF FPC}Do_NewDB);
          exit;
        end;
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Get_Share_Disk_Bridge.Do_GetKey(sender: TC40_TEKeyValue_Client; arry: U_StringArray);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  i: Integer;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      IO.OutDataFrame.WriteBool(True);
      IO.OutDataFrame.WriteString('Done.');
      for i := low(arry) to high(arry) do
          IO.OutDataFrame.WriteString(arry[i]);
      IO.ContinueResultSend;
      VM_Service.PostLog('%s Get_Share_Disk.', [IO_Def.PrimaryIdentifier.Text]);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Get_Share_Disk_File_List_Bridge.Do_GetItemList(sender: TC40_NetDisk_Directory_Client; Field_Path: U_String; arry: TItemList_Data_Array);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  i: Integer;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      IO.OutDataFrame.WriteBool(True);
      IO.OutDataFrame.WriteString(Field_Path);
      for i := Low(arry) to high(arry) do
        begin
          IO.OutDataFrame.WriteString(arry[i].Name);
          IO.OutDataFrame.WriteInt64(arry[i].num);
          IO.OutDataFrame.WriteDouble(arry[i].Time_);
        end;
      IO.ContinueResultSend;
      VM_Service.PostLog('%s Get_Share_Disk_File_List.', [IO_Def.PrimaryIdentifier.Text],
        PFormat('%s@%s', [Share_Directory_DB_Name.Text, DB_Field.Text]));
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Get_Share_Disk_File_Frag_Info_Bridge.Do_GetItemFrag(sender: TC40_NetDisk_Directory_Client; Successed: Boolean; L: TDirectory_MD5_Data_Frag_Struct_List);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  i: Integer;
  test_passed: Integer;
  d: TDFE;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;

      test_passed := 0;
      for i := 0 to L.count - 1 do
        if Z.Net.C4.C40_ClientPool.FindAliasOrHash(L[i]^.FS_AliasOrHash, True) <> nil then
            inc(test_passed);

      if test_passed >= L.count then
        begin
          IO.OutDataFrame.WriteBool(Successed);
          d := TDFE.Create;
          L.Encode(d);
          IO.OutDataFrame.WriteDataFrame(d);
          IO.ContinueResultSend;
          DisposeObject(d);
          VM_Service.PostLog('%s Get_Share_Disk_File_Frag_Info.', [IO_Def.PrimaryIdentifier.Text]);
        end
      else
        begin
          IO.OutDataFrame.WriteBool(False);
          IO.OutDataFrame.WriteString('Get_Share_Disk_File_Frag_Info error: no found FS2 AliasOrHash.');
          IO.ContinueResultSend;
        end;
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Search_NetDisk_File_Bridge.Do_SearchItem(sender: TC40_NetDisk_Directory_Client; SearchResult: TON_SearchItem_Data_array);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  i: Integer;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      IO.OutDataFrame.WriteBool(True);
      for i := Low(SearchResult) to high(SearchResult) do
        begin
          IO.OutDataFrame.WriteString(SearchResult[i].Current_Field);
          IO.OutDataFrame.WriteString(SearchResult[i].FieldOrItem);
          IO.OutDataFrame.WriteInt64(SearchResult[i].num);
          IO.OutDataFrame.WriteDouble(SearchResult[i].ModificationTime);
        end;
      IO.ContinueResultSend;
      VM_Service.PostLog('%s Search_NetDisk_File.', [IO_Def.PrimaryIdentifier.Text], DB_Field);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service_Search_Share_NetDisk_File_Bridge.Do_SearchItem(sender: TC40_NetDisk_Directory_Client; SearchResult: TON_SearchItem_Data_array);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  i: Integer;
begin
  if CheckIO then
    begin
      IO_Def := IO.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      IO.OutDataFrame.WriteBool(True);
      for i := Low(SearchResult) to high(SearchResult) do
        begin
          IO.OutDataFrame.WriteString(SearchResult[i].Current_Field);
          IO.OutDataFrame.WriteString(SearchResult[i].FieldOrItem);
          IO.OutDataFrame.WriteInt64(SearchResult[i].num);
          IO.OutDataFrame.WriteDouble(SearchResult[i].ModificationTime);
        end;
      IO.ContinueResultSend;
      VM_Service.PostLog('%s Search_Share_NetDisk_File.', [IO_Def.PrimaryIdentifier.Text], DB_Field);
    end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_Service.Add_PrimaryIdentifier(RecvIO_Define_: TC40_NetDisk_Service_RecvTunnel_NoAuth);
var
  L: TC40_NetDisk_Service_RecvIO_Define_List;
begin
  if not RecvIO_Define_.AuthDone then
      exit;
  L := FPrimaryIdentifier_Pool[RecvIO_Define_.PrimaryIdentifier];
  if L = nil then
    begin
      L := TC40_NetDisk_Service_RecvIO_Define_List.Create;
      FPrimaryIdentifier_Pool.FastAdd(RecvIO_Define_.PrimaryIdentifier, L);
    end
  else
      L.Add(RecvIO_Define_);
end;

procedure TC40_NetDisk_Service.Remove_PrimaryIdentifier(RecvIO_Define_: TC40_NetDisk_Service_RecvTunnel_NoAuth);
var
  L: TC40_NetDisk_Service_RecvIO_Define_List;
begin
  if not RecvIO_Define_.AuthDone then
      exit;
  L := FPrimaryIdentifier_Pool[RecvIO_Define_.PrimaryIdentifier];
  if L <> nil then
    begin
      L.Remove(RecvIO_Define_);
      if L.count <= 0 then
          FPrimaryIdentifier_Pool.Delete(RecvIO_Define_.PrimaryIdentifier);
    end;
end;

function TC40_NetDisk_Service.Check_PrimaryIdentifier(RecvIO_Define_: TC40_NetDisk_Service_RecvTunnel_NoAuth): Boolean;
var
  L: TC40_NetDisk_Service_RecvIO_Define_List;
begin
  Result := False;
  if not RecvIO_Define_.AuthDone then
      exit;
  L := FPrimaryIdentifier_Pool[RecvIO_Define_.PrimaryIdentifier];
  if L = nil then
      exit;
  Result := L.IndexOf(RecvIO_Define_) >= 0;
end;

procedure TC40_NetDisk_Service.DoLinkSuccess_Event(sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
begin
  inherited DoLinkSuccess_Event(sender, UserDefineIO);
  IO_Def := UserDefineIO as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  IO_Def.VM_Service := self;
  TC40_NetDisk_Service_SendTunnel_NoAuth(IO_Def.SendTunnel).VM_Service := self;
end;

procedure TC40_NetDisk_Service.DoUserOut_Event(sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
begin
  IO_Def := UserDefineIO as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if (IO_Def.AuthDone) then
    begin
      Remove_PrimaryIdentifier(IO_Def);
      if (UserDB_Client <> nil) and (UserDB_Client.Connected) then
        begin
          if not umlCompareMD5(IO_Def.UserJson_MD5, IO_Def.UserJson.MD5) then
              UserDB_Client.Usr_Set(IO_Def.PrimaryIdentifier, 'Detail', IO_Def.UserJson);

          if not Check_PrimaryIdentifier(IO_Def) then
              UserDB_Client.Usr_Close(IO_Def.PrimaryIdentifier);
        end;
      PostLog('%s UserOut_Event.', [IO_Def.PrimaryIdentifier.Text]);
    end;
  inherited DoUserOut_Event(sender, UserDefineIO);
end;

function TC40_NetDisk_Service.Get_UserDB_Client: TC40_UserDB_Client;
begin
  Result := FUserDB_Client;
end;

procedure TC40_NetDisk_Service.Set_UserDB_Client(const Value: TC40_UserDB_Client);
begin
  if FUserDB_Client <> nil then
      FUserDB_Client.ON_C40_UserDB_Client_Notify := nil;
  FUserDB_Client := Value;
  if FUserDB_Client <> nil then
      FUserDB_Client.ON_C40_UserDB_Client_Notify := self;
end;

procedure TC40_NetDisk_Service.Do_User_Msg(sender: TC40_UserDB_Client; FromUserName_, ToUserName_, Msg_: U_String);
var
  d: TDFE;
  L: TC40_NetDisk_Service_RecvIO_Define_List;
  i: Integer;
begin
  d := TDFE.Create;
  d.WriteString(FromUserName_);
  d.WriteString(ToUserName_);
  d.WriteString(Msg_);
  L := Search_IO_Def_From_UserPrimaryIdentifier(ToUserName_);
  for i := 0 to L.count - 1 do
      L[i].SendTunnel.Owner.SendDirectStreamCmd('userMsg', d);
  L.Free;
  DisposeObject(d);

  PostLog('%s User_Msg to %s', [FromUserName_.Text, ToUserName_.Text], Msg_);
end;

procedure TC40_NetDisk_Service.Do_User_Open(sender: TC40_UserDB_Client; userName_, ToUserName_: U_String);
var
  d: TDFE;
  L: TC40_NetDisk_Service_RecvIO_Define_List;
  i: Integer;
begin
  d := TDFE.Create;
  d.WriteString(userName_);
  d.WriteString(ToUserName_);
  L := Search_IO_Def_From_UserPrimaryIdentifier(ToUserName_);
  for i := 0 to L.count - 1 do
      L[i].SendTunnel.Owner.SendDirectStreamCmd('userOnline', d);
  L.Free;
  DisposeObject(d);

  PostLog('%s User_Open to %s', [userName_.Text, ToUserName_.Text]);
end;

procedure TC40_NetDisk_Service.Do_User_Close(sender: TC40_UserDB_Client; userName_, ToUserName_: U_String);
var
  d: TDFE;
  L: TC40_NetDisk_Service_RecvIO_Define_List;
  i: Integer;
begin
  d := TDFE.Create;
  d.WriteString(userName_);
  d.WriteString(ToUserName_);
  L := Search_IO_Def_From_UserPrimaryIdentifier(ToUserName_);
  for i := 0 to L.count - 1 do
      L[i].SendTunnel.Owner.SendDirectStreamCmd('userOffline', d);
  L.Free;
  DisposeObject(d);

  PostLog('%s User_Close to %s', [userName_.Text, ToUserName_.Text]);
end;

procedure TC40_NetDisk_Service.Do_User_Request_Friend(sender: TC40_UserDB_Client; FromUserName_, DestFriendUserName_, Msg_: U_String);
var
  d: TDFE;
  L: TC40_NetDisk_Service_RecvIO_Define_List;
  i: Integer;
begin
  d := TDFE.Create;
  d.WriteString(FromUserName_);
  d.WriteString(DestFriendUserName_);
  d.WriteString(Msg_);
  L := Search_IO_Def_From_UserPrimaryIdentifier(DestFriendUserName_);
  for i := 0 to L.count - 1 do
      L[i].SendTunnel.Owner.SendDirectStreamCmd('userRequestFriend', d);
  L.Free;
  DisposeObject(d);

  PostLog('%s User_Request_Friend to %s', [FromUserName_.Text, DestFriendUserName_.Text], Msg_);
end;

procedure TC40_NetDisk_Service.Do_User_Kick(sender: TC40_UserDB_Client; userName_: U_String);
var
  L: TC40_NetDisk_Service_RecvIO_Define_List;
  i: Integer;
  p2p_IO: TP2PVM_PeerIO;
begin
  L := Search_IO_Def_From_UserPrimaryIdentifier(userName_);
  for i := 0 to L.count - 1 do
    if L[i].Owner is TP2PVM_PeerIO then
      begin
        p2p_IO := L[i].Owner as TP2PVM_PeerIO;
        if p2p_IO.LinkVM <> nil then
          if p2p_IO.LinkVM.Owner_IO <> nil then
              p2p_IO.LinkVM.Owner_IO.Disconnect;
      end;
  L.Free;

  PostLog('%s User_Kick', [userName_.Text]);
end;

function TC40_NetDisk_Service.Get_Directory_Client: TC40_NetDisk_Directory_Client;
begin
  Result := FDirectory_Client;
end;

procedure TC40_NetDisk_Service.Set_Directory_Client(const Value: TC40_NetDisk_Directory_Client);
begin
  if FDirectory_Client <> nil then
      FDirectory_Client.ON_C40_NetDisk_Directory_Client_Interface := nil;
  FDirectory_Client := Value;
  if FDirectory_Client <> nil then
      FDirectory_Client.ON_C40_NetDisk_Directory_Client_Interface := self;
end;

procedure TC40_NetDisk_Service.Do_Remove_Directory_MD5(arry: U_StringArray);
begin

end;

procedure TC40_NetDisk_Service.Do_Remove_Directory_Invalid_Frag(arry: U_StringArray);
var
  i: Integer;
begin
  for i := 0 to FFS2_Client_Pool.count - 1 do
      FFS2_Client_Pool[i].RemoveCache(arry);
end;

function TC40_NetDisk_Service.Get_Log_Client: TC40_Log_DB_Client;
begin
  Result := FLog_Client;
end;

procedure TC40_NetDisk_Service.Set_Log_Client(const Value: TC40_Log_DB_Client);
begin
  FLog_Client := Value;
end;

function TC40_NetDisk_Service.Get_TEKeyValue_Client: TC40_TEKeyValue_Client;
begin
  Result := FTEKeyValue_Client;
end;

procedure TC40_NetDisk_Service.Set_TEKeyValue_Client(const Value: TC40_TEKeyValue_Client);
begin
  FTEKeyValue_Client := Value;
end;

procedure TC40_NetDisk_Service.cmd_Auth(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def_: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  userName_, Passwd_: U_String;
  tmp: TC40_NetDisk_Service_Auth_Bridge;
begin
  IO_Def_ := DTNoAuthService.GetUserDefineRecvTunnel(sender) as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if IO_Def_.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('repeat auth.');
      exit;
    end;
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('UserDB service is offline');
      exit;
    end;
  if (Directory_Client = nil) or (not Directory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('directory service is offline');
      exit;
    end;

  userName_ := InData.R.ReadString;
  Passwd_ := InData.R.ReadString;
  IO_Def_.UserName := userName_;

  tmp := TC40_NetDisk_Service_Auth_Bridge.Create(sender);
  tmp.VM_Service := self;
  tmp.IO_Def_ := IO_Def_;
  UserDB_Client.Usr_AuthM(userName_, Passwd_, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Usr_Auth);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Reg(sender: TPeerIO; InData, OutData: TDFE);
var
  userName_, Passwd_: U_String;
  tmp: TC40_NetDisk_Service_Reg_Bridge;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('UserDB service is offline');
      exit;
    end;
  if (Directory_Client = nil) or (not Directory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('directory service is offline');
      exit;
    end;
  userName_ := InData.R.ReadString;
  Passwd_ := InData.R.ReadString;
  tmp := TC40_NetDisk_Service_Reg_Bridge.Create(sender);
  tmp.VM_Service := self;
  tmp.userName_ := userName_;
  tmp.Passwd_ := Passwd_;
  UserDB_Client.Usr_RegM(userName_, Passwd_, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Usr_Reg);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_NewLoginName(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_NewIdentifier_Bridge;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('UserDB service is offline.');
      exit;
    end;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('login failed!');
      exit;
    end;
  tmp := TC40_NetDisk_Service_NewIdentifier_Bridge.Create(sender);
  tmp.VM_Service := self;
  UserDB_Client.Usr_NewIdentifierM(IO_Def.PrimaryIdentifier, InData.R.ReadString, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Usr_NewIdentifier);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_NewAlias(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;
  IO_Def.UserJson.S['Alias'] := InData.R.ReadString;
  IO_Def.UserJson_MD5 := IO_Def.UserJson.MD5;
  UserDB_Client.Usr_Set(IO_Def.PrimaryIdentifier, 'Detail', IO_Def.UserJson);
end;

procedure TC40_NetDisk_Service.cmd_GetAlias(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_GetAlias_Bridge;
  usr_Name: U_String;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;
  usr_Name := InData.R.ReadString;
  if usr_Name.L = 0 then
      exit;
  tmp := TC40_NetDisk_Service_GetAlias_Bridge.Create(sender);
  tmp.VM_Service := self;
  UserDB_Client.Usr_GetM(usr_Name, 'Detail', {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Usr_Get);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Msg(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  ToUserName_, Msg_: U_String;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;

  ToUserName_ := InData.R.ReadString;
  Msg_ := InData.R.ReadString;
  UserDB_Client.Usr_Msg(IO_Def.PrimaryIdentifier, ToUserName_, Msg_);
end;

procedure TC40_NetDisk_Service.cmd_RequestFriend(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  ToUserName_, Msg_: U_String;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;
  ToUserName_ := InData.R.ReadString;
  Msg_ := InData.R.ReadString;
  UserDB_Client.Usr_RequestAddFriend(IO_Def.PrimaryIdentifier, ToUserName_, Msg_);
end;

procedure TC40_NetDisk_Service.cmd_ReponseFriend(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  ToUserName_, Msg_: U_String;
  Accept_: Boolean;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;
  ToUserName_ := InData.R.ReadString;
  Msg_ := InData.R.ReadString;
  Accept_ := InData.R.ReadBool;
  UserDB_Client.Usr_ReponseAddFriend(IO_Def.PrimaryIdentifier, ToUserName_, Msg_, Accept_);
end;

procedure TC40_NetDisk_Service.cmd_RemoveFriend(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  ToUserName_: U_String;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;
  ToUserName_ := InData.R.ReadString;
  UserDB_Client.Usr_RemoveFriend(IO_Def.PrimaryIdentifier, ToUserName_);
end;

procedure TC40_NetDisk_Service.cmd_GetMyFriends(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_GetMyFriends_Bridge;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;
  tmp := TC40_NetDisk_Service_GetMyFriends_Bridge.Create(sender);
  tmp.VM_Service := self;
  UserDB_Client.Usr_GetFriendsM(IO_Def.PrimaryIdentifier, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Usr_GetFriends);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_GetOnlineNum(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_GetOnlineNum_Bridge;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;
  tmp := TC40_NetDisk_Service_GetOnlineNum_Bridge.Create(sender);
  tmp.VM_Service := self;
  UserDB_Client.Usr_OnlineNumM({$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Usr_OnlineNum);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_GetOnlineList(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_GetOnlineList_Bridge;
  Max_Num: Integer;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;
  tmp := TC40_NetDisk_Service_GetOnlineList_Bridge.Create(sender);
  tmp.VM_Service := self;
  Max_Num := InData.R.ReadInteger;
  UserDB_Client.Usr_OnlineListM(Max_Num, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Usr_OnlineList);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Get_NetDisk_Config(sender: TPeerIO; InData, OutData: TDFE);
begin
  OutData.WriteInt64(File_Chunk_Size);
end;

procedure TC40_NetDisk_Service.cmd_Get_FS_Service(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  i: Integer;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;

  OutData.WriteBool(True);
  for i := 0 to FFS2_Client_Pool.count - 1 do
    if FFS2_Client_Pool[i].Connected then
      begin
        OutData.WriteString(FFS2_Client_Pool[i].AliasOrHash);
        OutData.WriteInt64(FFS2_Client_Pool[i].Remote_FS_DB_Size);
        OutData.WriteCardinal(FFS2_Client_Pool[i].MaxFileSize);
      end;
  PostLog('%s Get_FS_Service', [IO_Def.PrimaryIdentifier.Text]);
end;

procedure TC40_NetDisk_Service.cmd_SearchMultiMD5_FS_Service(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_SearchMultiMD5_FS_Service_Bridge;
  tmp_Pair: TC40_NetDisk_Service_SearchMultiMD5_FS_Service_Bridge.TPair;
  md5_arry: TArrayMD5;
  i: Integer;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;

  // build pair struct
  tmp := TC40_NetDisk_Service_SearchMultiMD5_FS_Service_Bridge.Create(sender);
  tmp.VM_Service := self;
  tmp.FS_Pair := TC40_NetDisk_Service_SearchMultiMD5_FS_Service_Bridge.TPair_List.Create;
  for i := 0 to FFS2_Client_Pool.count - 1 do
    if FFS2_Client_Pool[i].Connected then
      begin
        tmp_Pair := TC40_NetDisk_Service_SearchMultiMD5_FS_Service_Bridge.TPair.Create;
        tmp_Pair.FS2 := FFS2_Client_Pool[i];
        tmp_Pair.FoundNum := 0;
        tmp_Pair.IsDone := False;
        tmp.FS_Pair.Add(tmp_Pair);
      end;

  // prepare md5
  SetLength(md5_arry, InData.count);
  for i := 0 to InData.count - 1 do
      md5_arry[i] := InData.ReadMD5(i);

  // search all FS
  for i := 0 to tmp.FS_Pair.count - 1 do
      tmp.FS_Pair[i].FS2.FS2_SearchMultiMD5M(md5_arry, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_FS2_SearchMultiMD5);

  // pause result
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_CheckAndCopy_NetDisk_File(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_CheckAndCopy_NetDisk_File_Bridge;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;
  tmp := TC40_NetDisk_Service_CheckAndCopy_NetDisk_File_Bridge.Create(sender);
  tmp.VM_Service := self;
  tmp.file_MD5 := InData.R.ReadMD5;
  tmp.file_Name := InData.R.ReadString;
  tmp.file_Field := umlGetUnixFilePath(tmp.file_Name);
  tmp.file_item := umlGetUnixFileName(tmp.file_Name);
  tmp.file_time := InData.R.ReadDouble;
  tmp.file_Size := InData.R.ReadInt64;
  FDirectory_Client.PutItemMD5_M(
    PrimaryIdentifierToDirectory(IO_Def.PrimaryIdentifier),
    tmp.file_Field,
    tmp.file_item,
    umlMD5ToStr(tmp.file_MD5), {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_PutItemMD5);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_BeginPost_NetDisk_File(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  i: Integer;
  alias_or_hash_: U_String;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;

  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;

  // search service
  alias_or_hash_ := InData.R.ReadString; // 1, service alias or hash
  IO_Def.NetDisk_File_Client := nil;
  for i := 0 to FFS2_Client_Pool.count - 1 do
    if alias_or_hash_.Same(FFS2_Client_Pool[i].AliasOrHash) then
        IO_Def.NetDisk_File_Client := FFS2_Client_Pool[i];
  if IO_Def.NetDisk_File_Client = nil then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found FS service');
      exit;
    end;
  if not IO_Def.NetDisk_File_Client.Connected then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS service is offline');
      exit;
    end;

  // frag clean
  IO_Def.NetDisk_File_Frag.Clean;
  // init file info
  IO_Def.NetDisk_File_Frag.MD5 := InData.R.ReadMD5; // 2, file md5
  IO_Def.NetDisk_File_Name := InData.R.ReadString;  // 3, remote full file name
  IO_Def.NetDisk_File_Field := umlGetUnixFilePath(IO_Def.NetDisk_File_Name);
  IO_Def.NetDisk_File_Item := umlGetUnixFileName(IO_Def.NetDisk_File_Name);
  IO_Def.NetDisk_File_Frag.Time_ := InData.R.ReadDouble; // 4, remote file time
  IO_Def.NetDisk_File_Frag.Size := InData.R.ReadInt64;   // 5, file size

  OutData.WriteBool(True);
  OutData.WriteString('prepare net-disk file.');
end;

procedure TC40_NetDisk_Service.cmd_CheckAndCopy_NetDisk_File_Frag(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  i: Integer;
  alias_or_hash_: U_String;
  tmp: TC40_NetDisk_Service_CheckAndCopy_NetDisk_File_Frag_Bridge;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;

  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;

  // search service
  alias_or_hash_ := InData.R.ReadString; // 1, service alias or hash
  IO_Def.NetDisk_File_Client := nil;
  for i := 0 to FFS2_Client_Pool.count - 1 do
    if alias_or_hash_.Same(FFS2_Client_Pool[i].AliasOrHash) then
        IO_Def.NetDisk_File_Client := FFS2_Client_Pool[i];
  if IO_Def.NetDisk_File_Client = nil then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found FS service');
      exit;
    end;
  if not IO_Def.NetDisk_File_Client.Connected then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS service is offline');
      exit;
    end;
  tmp := TC40_NetDisk_Service_CheckAndCopy_NetDisk_File_Frag_Bridge.Create(sender);
  tmp.VM_Service := self;
  tmp.frag_md5_ := InData.R.ReadMD5;    // 2, file md5
  tmp.frag_pos_ := InData.R.ReadInt64;  // 3, fragment pos
  tmp.frag_size_ := InData.R.ReadInt64; // 4, file size
  IO_Def.NetDisk_File_Client.FS2_CheckMD5AndFastCopyM(
    umlMD5ToStr(tmp.frag_md5_), tmp.frag_md5_, tmp.frag_size_, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_CheckMD5AndFastCopy);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Post_NetDisk_File_Frag(sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  Pos_: Int64;
  Event_: UInt64;
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  d: TDFE;
  p: PDirectory_MD5_Data_Frag_Struct;
  m64: TMS64;
  tmp: TC40_NetDisk_Service_Post_NetDisk_File_Frag_Bridge;
begin
  Pos_ := PInt64(InData)^;                  // 0-7, pos
  Event_ := PUInt64(GetOffset(InData, 8))^; // 8-15, event backcall data

  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;
  if (IO_Def.NetDisk_File_Client = nil) or (not IO_Def.NetDisk_File_Client.Connected) then
    begin
      d := TDFE.Create;
      d.WriteBool(False);
      d.WritePointer(Event_);
      IO_Def.SendTunnel.Owner.SendDirectStreamCmd('Done_PostFile_Frag', d);
      DisposeObject(d);
      exit;
    end;

  // make data clone
  m64 := TMS64.Create;
  m64.WritePtr(GetOffset(InData, 16), DataSize - 16);

  // generate fragment
  new(p);
  p^.FS_AliasOrHash := IO_Def.NetDisk_File_Client.AliasOrHash;
  p^.FS_File := umlMD5ToStr(m64.ToMD5);
  p^.Pos_ := Pos_;
  p^.Size_ := m64.Size;

  // post frag
  tmp := TC40_NetDisk_Service_Post_NetDisk_File_Frag_Bridge.Create(sender);
  tmp.VM_Service := self;
  tmp.p := p;
  tmp.Event_ := Event_;
  IO_Def.NetDisk_File_Client.FS2_PostFile_M(True, p^.FS_File, m64, True, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_FS2_PostFile_Done);
end;

procedure TC40_NetDisk_Service.cmd_EndPost_NetDisk_File(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_EndPost_NetDisk_File_Bridge;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;

  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;

  tmp := TC40_NetDisk_Service_EndPost_NetDisk_File_Bridge.Create(sender);
  tmp.VM_Service := self;
  IO_Def.NetDisk_File_Frag.SortPos;
  FDirectory_Client.PutItemFrag_M(
    PrimaryIdentifierToDirectory(IO_Def.PrimaryIdentifier),
    IO_Def.NetDisk_File_Field,
    IO_Def.NetDisk_File_Item,
    IO_Def.NetDisk_File_Frag, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_PutItemFrag);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Get_NetDisk_File_Frag_Info(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_Get_NetDisk_File_Frag_Info_Bridge;
  DB_Field, DB_Item: U_String;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;
  tmp := TC40_NetDisk_Service_Get_NetDisk_File_Frag_Info_Bridge.Create(sender);
  tmp.VM_Service := self;
  DB_Field := InData.R.ReadString;
  DB_Item := InData.R.ReadString;
  FDirectory_Client.GetItemFrag_M(
    PrimaryIdentifierToDirectory(IO_Def.PrimaryIdentifier),
    DB_Field,
    DB_Item, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_GetItemFrag);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Get_NetDisk_File_Frag_MD5(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_Get_NetDisk_File_Frag_MD5_Bridge;
  fs_: TC40_FS2_Client;
  i: Integer;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;

  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;

  tmp := TC40_NetDisk_Service_Get_NetDisk_File_Frag_MD5_Bridge.Create(sender);
  tmp.VM_Service := self;
  tmp.alias_or_hash_ := InData.R.ReadString; // 1, service alias or hash
  tmp.FS_File := InData.R.ReadString;        // 2, file name

  // search service
  fs_ := nil;
  for i := 0 to FFS2_Client_Pool.count - 1 do
    if tmp.alias_or_hash_.Same(FFS2_Client_Pool[i].AliasOrHash) then
        fs_ := FFS2_Client_Pool[i];
  if (fs_ = nil) or (not fs_.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found FS Service hash.');
      DisposeObject(tmp);
      exit;
    end;
  fs_.FS2_GetFileMD5M(tmp.FS_File, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_FS2_GetFileMD5);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Get_NetDisk_File_Frag(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_Get_NetDisk_File_Frag_Bridge;
  fs_: TC40_FS2_Client;
  i: Integer;
  d: TDFE;
begin
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;

  tmp := TC40_NetDisk_Service_Get_NetDisk_File_Frag_Bridge.Create(sender);
  tmp.VM_Service := self;
  tmp.alias_or_hash_ := InData.R.ReadString; // 1, service alias or hash
  tmp.FS_File := InData.R.ReadString;        // 2, file name
  tmp.Pos_ := InData.R.ReadInt64;            // 3, pos
  tmp.Event_ := InData.R.ReadPointer;        // 4, event

  // search service
  fs_ := nil;
  for i := 0 to FFS2_Client_Pool.count - 1 do
    if tmp.alias_or_hash_.Same(FFS2_Client_Pool[i].AliasOrHash) then
        fs_ := FFS2_Client_Pool[i];
  if (fs_ = nil) or (not fs_.Connected) then
    begin
      d := TDFE.Create;
      d.WriteInt64(tmp.Pos_);
      d.WritePointer(tmp.Event_);
      IO_Def.SendTunnel.Owner.SendDirectStreamCmd('Get_File_Error', d);
      DisposeObject(d);
      DisposeObject(tmp);
      exit;
    end;

  fs_.FS2_GetFile_M(True, tmp.FS_File, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_FS2_GetFile_Done);
end;

procedure TC40_NetDisk_Service.cmd_Get_NetDisk_File_List(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_Get_NetDisk_File_List_Bridge;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;

  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;

  tmp := TC40_NetDisk_Service_Get_NetDisk_File_List_Bridge.Create(sender);
  tmp.VM_Service := self;
  tmp.DB_Field := InData.R.ReadString;
  FDirectory_Client.GetItemList_M(PrimaryIdentifierToDirectory(IO_Def.PrimaryIdentifier), tmp.DB_Field, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_GetItemList);

  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Get_NetDisk_SpaceInfo(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_Get_NetDisk_SpaceInfo_Bridge;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;

  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;

  tmp := TC40_NetDisk_Service_Get_NetDisk_SpaceInfo_Bridge.Create(sender);
  tmp.VM_Service := self;
  FDirectory_Client.SpaceInfo_M(PrimaryIdentifierToDirectory(IO_Def.PrimaryIdentifier), {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_SpaceInfo);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Remove_Item(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  DB_Field: U_String;
  DB_Remove_Item_: U_String;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;

  DB_Field := InData.R.ReadString;
  DB_Remove_Item_ := InData.R.ReadString;

  FDirectory_Client.RemoveItem(PrimaryIdentifierToDirectory(IO_Def.PrimaryIdentifier), DB_Field, DB_Remove_Item_);
  PostLog('%s Remove Item %s',
    [IO_Def.PrimaryIdentifier.Text, umlCombineUnixFileName(DB_Field, DB_Remove_Item_).Text]);
end;

procedure TC40_NetDisk_Service.cmd_Remove_Field(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  DB_Field: U_String;
  DB_Remove_Field_: U_String;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;

  DB_Field := InData.R.ReadString;
  DB_Remove_Field_ := InData.R.ReadString;

  FDirectory_Client.RemoveField(PrimaryIdentifierToDirectory(IO_Def.PrimaryIdentifier), DB_Field, DB_Remove_Field_);
  PostLog('%s Remove Field %s',
    [IO_Def.PrimaryIdentifier.Text, umlCombineUnixPath(DB_Field, DB_Remove_Field_).Text]);
end;

procedure TC40_NetDisk_Service.cmd_Copy_Item(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  arry: TCopyItem_Info_Array;
  i: Integer;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;

  i := 0;
  SetLength(arry, InData.count div 5);
  while InData.R.NotEnd do
    begin
      arry[i].Sour_DB_Name := InData.R.ReadString;
      if arry[i].Sour_DB_Name = '' then
          arry[i].Sour_DB_Name := PrimaryIdentifierToDirectory(IO_Def.PrimaryIdentifier);
      arry[i].Sour_DB_Field := InData.R.ReadString;
      arry[i].Sour_DB_Item := InData.R.ReadString;

      arry[i].Dest_DB_Name := InData.R.ReadString;
      if arry[i].Dest_DB_Name = '' then
          arry[i].Dest_DB_Name := PrimaryIdentifierToDirectory(IO_Def.PrimaryIdentifier);
      arry[i].Dest_DB_Field := InData.R.ReadString;

      PostLog('%s CopyItem %s@%s -> %s@%s',
        [IO_Def.PrimaryIdentifier.Text,
        arry[i].Sour_DB_Name, umlCombineUnixPath(arry[i].Sour_DB_Field, arry[i].Sour_DB_Item).Text,
        arry[i].Dest_DB_Name, arry[i].Dest_DB_Field]);

      inc(i);
    end;

  FDirectory_Client.CopyItem(arry);
end;

procedure TC40_NetDisk_Service.cmd_Copy_Field(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  arry: TCopyField_Info_Array;
  i: Integer;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;

  i := 0;
  SetLength(arry, InData.count div 4);
  while InData.R.NotEnd do
    begin
      arry[i].Sour_DB_Name := InData.R.ReadString;
      if arry[i].Sour_DB_Name = '' then
          arry[i].Sour_DB_Name := PrimaryIdentifierToDirectory(IO_Def.PrimaryIdentifier);
      arry[i].Sour_DB_Field := InData.R.ReadString;

      arry[i].Dest_DB_Name := InData.R.ReadString;
      if arry[i].Dest_DB_Name = '' then
          arry[i].Dest_DB_Name := PrimaryIdentifierToDirectory(IO_Def.PrimaryIdentifier);
      arry[i].Dest_DB_Field := InData.R.ReadString;

      PostLog('%s CopyField %s@%s -> %s@%s',
        [IO_Def.PrimaryIdentifier.Text, arry[i].Sour_DB_Name, arry[i].Sour_DB_Field, arry[i].Dest_DB_Name, arry[i].Dest_DB_Field]);

      inc(i);
    end;

  FDirectory_Client.CopyField(arry);
end;

procedure TC40_NetDisk_Service.cmd_CreateField(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  DB_Field: U_String;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;

  DB_Field := InData.R.ReadString;
  FDirectory_Client.NewField(IO_Def.PrimaryIdentifier, DB_Field);
end;

procedure TC40_NetDisk_Service.cmd_RenameField(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  DB_Field: U_String;
  New_Field_Name: U_String;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;

  DB_Field := InData.R.ReadString;
  New_Field_Name := InData.R.ReadString;

  FDirectory_Client.RenameField(IO_Def.PrimaryIdentifier, DB_Field, New_Field_Name);
end;

procedure TC40_NetDisk_Service.cmd_RenameItem(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  DB_Field: U_String;
  Old_Item_Name: U_String;
  New_Item_Name: U_String;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;

  DB_Field := InData.R.ReadString;
  Old_Item_Name := InData.R.ReadString;
  New_Item_Name := InData.R.ReadString;

  FDirectory_Client.RenameItem(IO_Def.PrimaryIdentifier, DB_Field, Old_Item_Name, New_Item_Name);
end;

procedure TC40_NetDisk_Service.cmd_Build_Share_Disk(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_Build_Share_Disk_Bridge;
  Time_: TDateTime;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;
  if (FTEKeyValue_Client = nil) or (not FTEKeyValue_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FTEKeyValue is offline.');
      exit;
    end;

  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;

  tmp := TC40_NetDisk_Service_Build_Share_Disk_Bridge.Create(sender);
  tmp.VM_Service := self;
  Time_ := umlNow;
  tmp.Share_Directory_DB_Name := ShareToDirectory(IO_Def.PrimaryIdentifier, umlMD5(@Time_, 8));
  FDirectory_Client.NewDB_M(tmp.Share_Directory_DB_Name, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_NewDB);

  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Get_Share_Disk(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_Get_Share_Disk_Bridge;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;
  if (FTEKeyValue_Client = nil) or (not FTEKeyValue_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FTEKeyValue is offline.');
      exit;
    end;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;

  tmp := TC40_NetDisk_Service_Get_Share_Disk_Bridge.Create(sender);
  tmp.VM_Service := self;
  TEKeyValue_Client.GetKey_M(IO_Def.PrimaryIdentifier, 'Share', {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_GetKey);

  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Remove_Share_Disk(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  Share_Directory_DB_Name: U_String;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
      exit;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
      exit;
  if (FTEKeyValue_Client = nil) or (not FTEKeyValue_Client.Connected) then
      exit;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
      exit;

  Share_Directory_DB_Name := InData.R.ReadString;

  if IsMyShareDirectory(IO_Def.PrimaryIdentifier, Share_Directory_DB_Name) then
    begin
      TEKeyValue_Client.RemoveKey(IO_Def.PrimaryIdentifier, 'Share', Share_Directory_DB_Name);
      TEKeyValue_Client.Rebuild(IO_Def.PrimaryIdentifier);
      FDirectory_Client.RemoveDB(Share_Directory_DB_Name);
      PostLog('%s Remove_Share_Disk %s.', [IO_Def.PrimaryIdentifier.Text, Share_Directory_DB_Name.Text]);
    end;
end;

procedure TC40_NetDisk_Service.cmd_Get_Share_Disk_File_List(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_Get_Share_Disk_File_List_Bridge;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;

  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;

  tmp := TC40_NetDisk_Service_Get_Share_Disk_File_List_Bridge.Create(sender);
  tmp.VM_Service := self;
  tmp.Share_Directory_DB_Name := InData.R.ReadString;
  tmp.DB_Field := InData.R.ReadString;
  if not IsShareDirectory(tmp.Share_Directory_DB_Name) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('%s not is share.', [tmp.Share_Directory_DB_Name.Text]);
      tmp.Free;
      exit;
    end;
  FDirectory_Client.GetItemList_M(tmp.Share_Directory_DB_Name, tmp.DB_Field, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_GetItemList);

  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Get_Share_Disk_File_Frag_Info(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_Get_Share_Disk_File_Frag_Info_Bridge;
  Share_Directory_DB_Name, DB_Field, DB_Item: U_String;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;
  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;

  Share_Directory_DB_Name := InData.R.ReadString;
  DB_Field := InData.R.ReadString;
  DB_Item := InData.R.ReadString;
  if not IsShareDirectory(Share_Directory_DB_Name) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('%s not is share.', [Share_Directory_DB_Name.Text]);
      exit;
    end;

  tmp := TC40_NetDisk_Service_Get_Share_Disk_File_Frag_Info_Bridge.Create(sender);
  tmp.VM_Service := self;
  FDirectory_Client.GetItemFrag_M(
    Share_Directory_DB_Name,
    DB_Field,
    DB_Item, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_GetItemFrag);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Search_NetDisk_File(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_Search_NetDisk_File_Bridge;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;

  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;

  tmp := TC40_NetDisk_Service_Search_NetDisk_File_Bridge.Create(sender);
  tmp.VM_Service := self;
  tmp.DB_Field := InData.R.ReadString;
  tmp.DB_Search := InData.R.ReadString;
  FDirectory_Client.SearchItem_M(PrimaryIdentifierToDirectory(IO_Def.PrimaryIdentifier), tmp.DB_Field, tmp.DB_Search,
{$IFDEF FPC}@{$ENDIF FPC}tmp.Do_SearchItem);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Search_Share_NetDisk_File(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  tmp: TC40_NetDisk_Service_Search_Share_NetDisk_File_Bridge;
begin
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('user db is offline.');
      exit;
    end;
  if FFS2_Client_Pool.count = 0 then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('FS is offline.');
      exit;
    end;
  if (FDirectory_Client = nil) or (not FDirectory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('Directory is offline.');
      exit;
    end;

  IO_Def := sender.UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if not IO_Def.AuthDone then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no auth.');
      exit;
    end;

  tmp := TC40_NetDisk_Service_Search_Share_NetDisk_File_Bridge.Create(sender);
  tmp.VM_Service := self;
  tmp.Share_Directory_DB_Name := InData.R.ReadString;
  tmp.DB_Field := InData.R.ReadString;
  tmp.DB_Search := InData.R.ReadString;

  if not IsShareDirectory(tmp.Share_Directory_DB_Name) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('%s not is share.', [tmp.Share_Directory_DB_Name.Text]);
      tmp.Free;
      exit;
    end;

  FDirectory_Client.SearchItem_M(tmp.Share_Directory_DB_Name, tmp.DB_Field, tmp.DB_Search,
{$IFDEF FPC}@{$ENDIF FPC}tmp.Do_SearchItem);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Auth_Admin(sender: TPeerIO; InData, OutData: TDFE);
var
  IO_Def_: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  userName_: U_String;
  tmp: TC40_NetDisk_Service_Auth_Bridge;
begin
  IO_Def_ := DTNoAuthService.GetUserDefineRecvTunnel(sender) as TC40_NetDisk_Service_RecvTunnel_NoAuth;
  if IO_Def_.AuthDone then
    begin
      Remove_PrimaryIdentifier(IO_Def_);
      if (UserDB_Client <> nil) and (UserDB_Client.Connected) then
        begin
          if not umlCompareMD5(IO_Def_.UserJson_MD5, IO_Def_.UserJson.MD5) then
              UserDB_Client.Usr_Set(IO_Def_.PrimaryIdentifier, 'Detail', IO_Def_.UserJson);

          if not Check_PrimaryIdentifier(IO_Def_) then
              UserDB_Client.Usr_Close(IO_Def_.PrimaryIdentifier);
        end;
      IO_Def_.Reinit;
      PostLog('%s UserOut_Event.', [IO_Def_.PrimaryIdentifier.Text]);
    end;
  if (UserDB_Client = nil) or (not UserDB_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('UserDB service is offline');
      exit;
    end;
  if (Directory_Client = nil) or (not Directory_Client.Connected) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('directory service is offline');
      exit;
    end;

  userName_ := InData.R.ReadString;
  IO_Def_.UserName := userName_;

  tmp := TC40_NetDisk_Service_Auth_Bridge.Create(sender);
  tmp.VM_Service := self;
  tmp.IO_Def_ := IO_Def_;
  UserDB_Client.Usr_ExistsM(userName_, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Usr_Exists);
  sender.PauseResultSend;
end;

procedure TC40_NetDisk_Service.cmd_Close_Auth_Admin(sender: TPeerIO; InData: TDFE);
var
  IO_Def_: TC40_NetDisk_Service_RecvTunnel_NoAuth;
  userName_: U_String;
begin
  userName_ := InData.R.ReadString;
  IO_Def_ := DTNoAuthService.GetUserDefineRecvTunnel(sender) as TC40_NetDisk_Service_RecvTunnel_NoAuth;

  if (IO_Def_.AuthDone) then
    begin
      Remove_PrimaryIdentifier(IO_Def_);
      if (UserDB_Client <> nil) and (UserDB_Client.Connected) then
        begin
          if not umlCompareMD5(IO_Def_.UserJson_MD5, IO_Def_.UserJson.MD5) then
              UserDB_Client.Usr_Set(IO_Def_.PrimaryIdentifier, 'Detail', IO_Def_.UserJson);

          if not Check_PrimaryIdentifier(IO_Def_) then
              UserDB_Client.Usr_Close(IO_Def_.PrimaryIdentifier);
        end;
      IO_Def_.Reinit;
      PostLog('%s UserOut_Event.', [IO_Def_.PrimaryIdentifier.Text]);
    end;
end;

procedure TC40_NetDisk_Service.Automated_Config_NetDisk_Service_Relevance;
var
  i: Integer;
  cc: TC40_Custom_Client;
begin
  UserDB_Client := nil;
  Directory_Client := nil;
  TEKeyValue_Client := nil;
  Log_Client := nil;
  FS2_Client_Pool.Clear;
  for i := 0 to Z.Net.C4.C40_ClientPool.count - 1 do
    begin
      cc := Z.Net.C4.C40_ClientPool[i];
      if cc is TC40_UserDB_Client then
          UserDB_Client := cc as TC40_UserDB_Client
      else if cc is TC40_NetDisk_Directory_Client then
          Directory_Client := cc as TC40_NetDisk_Directory_Client
      else if cc is TC40_TEKeyValue_Client then
          TEKeyValue_Client := cc as TC40_TEKeyValue_Client
      else if cc is TC40_Log_DB_Client then
          Log_Client := cc as TC40_Log_DB_Client
      else if cc is TC40_FS2_Client then
          FS2_Client_Pool.Add(cc as TC40_FS2_Client);
    end;
end;

function TC40_NetDisk_Service.Check_NetDisk_Service_Relevance(Status_: Boolean): Boolean;
var
  i: Integer;
begin
  Result := True;
  if (UserDB_Client <> nil) and (UserDB_Client.Connected) then
    begin
      if Status_ then
          DoStatus('NetDisk Check:UserDB_Client passed.');
    end
  else
    begin
      if Status_ then
          DoStatus('NetDisk Check:UserDB_Client error!');
      Result := Result and False;
    end;

  if (Directory_Client <> nil) and (Directory_Client.Connected) then
    begin
      if Status_ then
          DoStatus('NetDisk Check:Directory_Client passed.');
    end
  else
    begin
      if Status_ then
          DoStatus('NetDisk Check:Directory_Client error!');
      Result := Result and False;
    end;

  if (TEKeyValue_Client <> nil) and (TEKeyValue_Client.Connected) then
    begin
      if Status_ then
          DoStatus('NetDisk Check:TEKeyValue_Client passed.');
    end
  else
    begin
      if Status_ then
          DoStatus('NetDisk Check:TEKeyValue_Client error!');
      Result := Result and False;
    end;

  if (Log_Client <> nil) and (Log_Client.Connected) then
    begin
      if Status_ then
          DoStatus('NetDisk Check:Log_Client passed.');
    end
  else
    begin
      if Status_ then
          DoStatus('NetDisk Check:Log_Client error!');
      Result := Result and False;
    end;

  Result := Result and (FS2_Client_Pool.count > 0);
  for i := 0 to FS2_Client_Pool.count - 1 do
    begin
      if FS2_Client_Pool[i].Connected then
        begin
          if Status_ then
              DoStatus('NetDisk Check:FS2_Client: %s passed.', [FS2_Client_Pool[i].ClientInfo.ServiceTyp.Text]);
        end
      else
        begin
          if Status_ then
              DoStatus('NetDisk Check:FS2_Client: %s error.', [FS2_Client_Pool[i].ClientInfo.ServiceTyp.Text]);
          Result := Result and False;
        end;
    end;
end;

function TC40_NetDisk_Service.Check_NetDisk_Service_Relevance: Boolean;
begin
  Result := Check_NetDisk_Service_Relevance(True);
end;

constructor TC40_NetDisk_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
var
  fs: TCore_Stream;
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);

  FPrimaryIdentifier_Pool := TC40_NetDisk_Service_PrimaryIdentifier_Pool.Create(True, $FFFF, nil);

  // max complete buffer 10M
  DTNoAuthService.RecvTunnel.MaxCompleteBufferSize := EStrToInt64(ParamList.GetDefaultValue('MaxBuffer', '10*1024*1024'), 10 * 1024 * 1024);
  DTNoAuthService.RecvTunnel.CompleteBufferCompressed := False;

  // is only instance
  ServiceInfo.OnlyInstance := False;
  UpdateToGlobalDispatch;
  ParamList.SetDefaultValue('OnlyInstance', if_(ServiceInfo.OnlyInstance, 'True', 'False'));

  // custom
  DTNoAuth.SendTunnel.PeerClientUserDefineClass := TC40_NetDisk_Service_SendTunnel_NoAuth;
  DTNoAuth.RecvTunnel.PeerClientUserDefineClass := TC40_NetDisk_Service_RecvTunnel_NoAuth;

  // define
  File_Chunk_Size := EStrToInt(ParamList.GetDefaultValue('File_Chunk_Size', '500*1024'), 500 * 1024);

  // IM
  DTNoAuth.RecvTunnel.RegisterStream('Auth').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Auth;
  DTNoAuth.RecvTunnel.RegisterStream('Reg').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Reg;
  DTNoAuth.RecvTunnel.RegisterStream('NewLoginName').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_NewLoginName;
  DTNoAuth.RecvTunnel.RegisterDirectStream('NewAlias').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_NewAlias;
  DTNoAuth.RecvTunnel.RegisterStream('GetAlias').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetAlias;
  DTNoAuth.RecvTunnel.RegisterDirectStream('Msg').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Msg;
  DTNoAuth.RecvTunnel.RegisterDirectStream('RequestFriend').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RequestFriend;
  DTNoAuth.RecvTunnel.RegisterDirectStream('ReponseFriend').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_ReponseFriend;
  DTNoAuth.RecvTunnel.RegisterDirectStream('RemoveFriend').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemoveFriend;
  DTNoAuth.RecvTunnel.RegisterStream('GetMyFriends').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetMyFriends;
  DTNoAuth.RecvTunnel.RegisterStream('GetOnlineNum').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetOnlineNum;
  DTNoAuth.RecvTunnel.RegisterStream('GetOnlineList').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetOnlineList;

  // network VM
  DTNoAuth.RecvTunnel.RegisterStream('Get_NetDisk_Config').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Get_NetDisk_Config;
  DTNoAuth.RecvTunnel.RegisterStream('Get_FS_Service').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Get_FS_Service;
  DTNoAuth.RecvTunnel.RegisterStream('SearchMultiMD5_FS_Service').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_SearchMultiMD5_FS_Service;
  DTNoAuth.RecvTunnel.RegisterStream('CheckAndCopy_NetDisk_File').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_CheckAndCopy_NetDisk_File;
  DTNoAuth.RecvTunnel.RegisterStream('BeginPost_NetDisk_File').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_BeginPost_NetDisk_File;
  DTNoAuth.RecvTunnel.RegisterStream('CheckAndCopy_NetDisk_File_Frag').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_CheckAndCopy_NetDisk_File_Frag;
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer('Post_NetDisk_File_Frag').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Post_NetDisk_File_Frag;
  DTNoAuth.RecvTunnel.RegisterStream('EndPost_NetDisk_File').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_EndPost_NetDisk_File;
  DTNoAuth.RecvTunnel.RegisterStream('Get_NetDisk_File_Frag_Info').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Get_NetDisk_File_Frag_Info;
  DTNoAuth.RecvTunnel.RegisterStream('Get_NetDisk_File_Frag_MD5').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Get_NetDisk_File_Frag_MD5;
  DTNoAuth.RecvTunnel.RegisterDirectStream('Get_NetDisk_File_Frag').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Get_NetDisk_File_Frag;
  DTNoAuth.RecvTunnel.RegisterStream('Get_NetDisk_File_List').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Get_NetDisk_File_List;
  DTNoAuth.RecvTunnel.RegisterStream('Get_NetDisk_SpaceInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Get_NetDisk_SpaceInfo;
  DTNoAuth.RecvTunnel.RegisterDirectStream('Remove_Item').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Remove_Item;
  DTNoAuth.RecvTunnel.RegisterDirectStream('Remove_Field').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Remove_Field;
  DTNoAuth.RecvTunnel.RegisterDirectStream('Copy_Item').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Copy_Item;
  DTNoAuth.RecvTunnel.RegisterDirectStream('Copy_Field').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Copy_Field;
  DTNoAuth.RecvTunnel.RegisterDirectStream('CreateField').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_CreateField;
  DTNoAuth.RecvTunnel.RegisterDirectStream('RenameField').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RenameField;
  DTNoAuth.RecvTunnel.RegisterDirectStream('RenameItem').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RenameItem;
  DTNoAuth.RecvTunnel.RegisterStream('Build_Share_Disk').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Build_Share_Disk;
  DTNoAuth.RecvTunnel.RegisterStream('Get_Share_Disk').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Get_Share_Disk;
  DTNoAuth.RecvTunnel.RegisterDirectStream('Remove_Share_Disk').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Remove_Share_Disk;
  DTNoAuth.RecvTunnel.RegisterStream('Get_Share_Disk_File_List').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Get_Share_Disk_File_List;
  DTNoAuth.RecvTunnel.RegisterStream('Get_Share_Disk_File_Frag_Info').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Get_Share_Disk_File_Frag_Info;
  DTNoAuth.RecvTunnel.RegisterStream('Search_NetDisk_File').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Search_NetDisk_File;
  DTNoAuth.RecvTunnel.RegisterStream('Search_Share_NetDisk_File').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Search_Share_NetDisk_File;

  // admin
  DTNoAuth.RecvTunnel.RegisterStream('Auth_Admin').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Auth_Admin;
  DTNoAuth.RecvTunnel.RegisterDirectStream('Close_Auth_Admin').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Close_Auth_Admin;

  FUserDB_Client := nil;
  FDirectory_Client := nil;
  FTEKeyValue_Client := nil;
  FLog_Client := nil;
  FFS2_Client_Pool := TC40_FS2_Client_List.Create;
end;

destructor TC40_NetDisk_Service.Destroy;
begin
  DisposeObject(FPrimaryIdentifier_Pool);
  DisposeObject(FFS2_Client_Pool);
  inherited Destroy;
end;

procedure TC40_NetDisk_Service.SafeCheck;
begin
  inherited SafeCheck;
  if not Check_NetDisk_Service_Relevance(False) then
    begin
      Automated_Config_NetDisk_Service_Relevance();
      Check_NetDisk_Service_Relevance();
    end;
end;

procedure TC40_NetDisk_Service.Progress;
begin
  inherited Progress;
end;

function TC40_NetDisk_Service.Search_IO_Def_From_UserPrimaryIdentifier(PrimaryIdentifier_: U_String): TC40_NetDisk_Service_RecvIO_Define_List;
var
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_Def: TC40_NetDisk_Service_RecvTunnel_NoAuth;
begin
  Result := TC40_NetDisk_Service_RecvIO_Define_List.Create;
  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_Def := DTNoAuthService.RecvTunnel[ID_].UserDefine as TC40_NetDisk_Service_RecvTunnel_NoAuth;
      if IO_Def.LinkOk and IO_Def.AuthDone and PrimaryIdentifier_.Same(@IO_Def.PrimaryIdentifier) then
          Result.Add(IO_Def);
    end;
end;

procedure TC40_NetDisk_Service.PostLog(info: SystemString);
begin
  if (Log_Client = nil) or (not Log_Client.Connected) then
      exit;
  Log_Client.PostLog('NetDisk_VM_' + AliasOrHash + '_' + MakeNowDateStr, info, '');
end;

procedure TC40_NetDisk_Service.PostLog(info1, info2: SystemString);
begin
  if (Log_Client = nil) or (not Log_Client.Connected) then
      exit;
  Log_Client.PostLog('NetDisk_VM_' + AliasOrHash + '_' + MakeNowDateStr, info1, info2);
end;

procedure TC40_NetDisk_Service.PostLog(const v: SystemString; const Args: array of const);
begin
  PostLog(PFormat(v, Args));
end;

procedure TC40_NetDisk_Service.PostLog(const v: SystemString; const Args: array of const; info2: SystemString);
begin
  PostLog(PFormat(v, Args), info2);
end;

class function TC40_NetDisk_Service.PrimaryIdentifierToDirectory(PrimaryIdentifier_: U_String): U_String;
begin
  Result := PrimaryIdentifier_;
end;

class function TC40_NetDisk_Service.ShareToDirectory(PrimaryIdentifier_: U_String; MD5_: TMD5): U_String;
begin
  Result := 'S:' + PrimaryIdentifier_ + '_' + umlMD5ToStr(MD5_);
end;

class function TC40_NetDisk_Service.IsMyShareDirectory(PrimaryIdentifier_, Share_Directory_DB_Name: U_String): Boolean;
begin
  Result := umlMultipleMatch('S:' + PrimaryIdentifier_ + '_*', Share_Directory_DB_Name);
end;

class function TC40_NetDisk_Service.IsShareDirectory(Share_Directory_DB_Name: U_String): Boolean;
begin
  Result := umlMultipleMatch('S:*', Share_Directory_DB_Name);
end;

class procedure TC40_NetDisk_Service.Get_User_Reg_Detail(Json_: TZJ);
begin
  Json_.S['Age'] := 'unknow';
  Json_.S['Sex'] := 'unknow';
  Json_.S['introduction'] := 'This guy is too lazy to leave anything';
  Json_.I64['MaxSpace'] := 1024 * 1024 * 1024;
end;

initialization

RegisterC40('NetDisk_C4', TC40_NetDisk_Service, nil);

end.
