{ ****************************************************************************** }
{ * cloud 4.0 network disk VM Client                                           * }
{ ****************************************************************************** }
unit Z.Net.C4_NetDisk_VM_Client;

{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine,
  Z.Geometry2D, Z.DFE, Z.Json, Z.Expression,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.GHashList,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth,
  Z.Net.C4_NetDisk_Directory,
  Z.Net.C4, Z.Net.C4.VM;

type
  TC40_NetDisk_VM_Client = class;
  TC40_NetDisk_VM_Client_Class = class of TC40_NetDisk_VM_Client;

{$REGION 'event'}
  TC40_NetDisk_VM_Client_On_Usr_AuthC = procedure(sender: TC40_NetDisk_VM_Client; State_: Boolean; info_: SystemString);
  TC40_NetDisk_VM_Client_On_Usr_AuthM = procedure(sender: TC40_NetDisk_VM_Client; State_: Boolean; info_: SystemString) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_AuthP = procedure(sender: TC40_NetDisk_VM_Client; State_: Boolean; info_: SystemString) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_AuthP = reference to procedure(sender: TC40_NetDisk_VM_Client; State_: Boolean; info_: SystemString);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_Auth = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_AuthC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_AuthM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_AuthP;
    constructor Create;
    procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_RegC = procedure(sender: TC40_NetDisk_VM_Client; State_: Boolean; info_: SystemString);
  TC40_NetDisk_VM_Client_On_Usr_RegM = procedure(sender: TC40_NetDisk_VM_Client; State_: Boolean; info_: SystemString) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_RegP = procedure(sender: TC40_NetDisk_VM_Client; State_: Boolean; info_: SystemString) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_RegP = reference to procedure(sender: TC40_NetDisk_VM_Client; State_: Boolean; info_: SystemString);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_Reg = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_RegC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_RegM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_RegP;
    constructor Create;
    procedure DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_NewLoginNameC = procedure(sender: TC40_NetDisk_VM_Client; State_: Boolean; info_: SystemString);
  TC40_NetDisk_VM_Client_On_Usr_NewLoginNameM = procedure(sender: TC40_NetDisk_VM_Client; State_: Boolean; info_: SystemString) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_NewLoginNameP = procedure(sender: TC40_NetDisk_VM_Client; State_: Boolean; info_: SystemString) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_NewLoginNameP = reference to procedure(sender: TC40_NetDisk_VM_Client; State_: Boolean; info_: SystemString);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_NewLoginName = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_NewLoginNameC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_NewLoginNameM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_NewLoginNameP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_GetAliasC = procedure(sender: TC40_NetDisk_VM_Client; Alias_: SystemString);
  TC40_NetDisk_VM_Client_On_Usr_GetAliasM = procedure(sender: TC40_NetDisk_VM_Client; Alias_: SystemString) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_GetAliasP = procedure(sender: TC40_NetDisk_VM_Client; Alias_: SystemString) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_GetAliasP = reference to procedure(sender: TC40_NetDisk_VM_Client; Alias_: SystemString);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_GetAlias = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_GetAliasC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_GetAliasM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_GetAliasP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_GetMyFriendsC = procedure(sender: TC40_NetDisk_VM_Client; FriendArry: U_StringArray);
  TC40_NetDisk_VM_Client_On_Usr_GetMyFriendsM = procedure(sender: TC40_NetDisk_VM_Client; FriendArry: U_StringArray) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_GetMyFriendsP = procedure(sender: TC40_NetDisk_VM_Client; FriendArry: U_StringArray) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_GetMyFriendsP = reference to procedure(sender: TC40_NetDisk_VM_Client; FriendArry: U_StringArray);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_GetMyFriends = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_GetMyFriendsC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_GetMyFriendsM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_GetMyFriendsP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_GetOnlineNumC = procedure(sender: TC40_NetDisk_VM_Client; Online_Num, User_Num: Integer);
  TC40_NetDisk_VM_Client_On_Usr_GetOnlineNumM = procedure(sender: TC40_NetDisk_VM_Client; Online_Num, User_Num: Integer) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_GetOnlineNumP = procedure(sender: TC40_NetDisk_VM_Client; Online_Num, User_Num: Integer) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_GetOnlineNumP = reference to procedure(sender: TC40_NetDisk_VM_Client; Online_Num, User_Num: Integer);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_GetOnlineNum = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_GetOnlineNumC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_GetOnlineNumM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_GetOnlineNumP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_GetOnlineListC = procedure(sender: TC40_NetDisk_VM_Client; arry: U_StringArray);
  TC40_NetDisk_VM_Client_On_Usr_GetOnlineListM = procedure(sender: TC40_NetDisk_VM_Client; arry: U_StringArray) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_GetOnlineListP = procedure(sender: TC40_NetDisk_VM_Client; arry: U_StringArray) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_GetOnlineListP = reference to procedure(sender: TC40_NetDisk_VM_Client; arry: U_StringArray);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_GetOnlineList = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_GetOnlineListC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_GetOnlineListM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_GetOnlineListP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_FS_Service_Info = record
    AliasOrHash: U_String;
    Remote_FS_DB_Size: Int64;
    MaxFileSize: Int64;
  end;

  TC40_NetDisk_VM_Client_On_Usr_FS_Service_Info_Array = array of TC40_NetDisk_VM_Client_On_Usr_FS_Service_Info;

  TC40_NetDisk_VM_Client_On_Usr_Get_FS_ServiceC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; arry: TC40_NetDisk_VM_Client_On_Usr_FS_Service_Info_Array);
  TC40_NetDisk_VM_Client_On_Usr_Get_FS_ServiceM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; arry: TC40_NetDisk_VM_Client_On_Usr_FS_Service_Info_Array) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_FS_ServiceP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; arry: TC40_NetDisk_VM_Client_On_Usr_FS_Service_Info_Array) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_FS_ServiceP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; arry: TC40_NetDisk_VM_Client_On_Usr_FS_Service_Info_Array);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_Get_FS_Service = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_Get_FS_ServiceC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_Get_FS_ServiceM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_Get_FS_ServiceP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_ServiceC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
  TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_ServiceM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_ServiceP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_ServiceP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_Service = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_ServiceC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_ServiceM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_ServiceP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_FileC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
  TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_FileM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_FileP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_FileP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_FileC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_FileM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_FileP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_FileC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
  TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_FileM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_FileP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_FileP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_File = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_FileC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_FileM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_FileP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_FragC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
  TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_FragM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_FragP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_FragP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_Frag = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_FragC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_FragM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_FragP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_FileC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
  TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_FileM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_FileP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_FileP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_File = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_FileC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_FileM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_FileP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_InfoC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; L: TDirectory_MD5_Data_Frag_Struct_List);
  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_InfoM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; L: TDirectory_MD5_Data_Frag_Struct_List) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_InfoP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; L: TDirectory_MD5_Data_Frag_Struct_List) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_InfoP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; L: TDirectory_MD5_Data_Frag_Struct_List);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_Info = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_InfoC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_InfoM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_InfoP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5C = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; MD5: TMD5);
  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5M = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; MD5: TMD5) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5P = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; MD5: TMD5) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5P = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; MD5: TMD5);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5 = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5C;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5M;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5P;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List_Data = record
    Name: U_String;
    Num: Int64;
    Time_: TDateTime;
  end;

  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List_Data_array = array of TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List_Data;

  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_ListC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List_Data_array);
  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_ListM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List_Data_array) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_ListP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List_Data_array) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_ListP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List_Data_array);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_ListC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_ListM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_ListP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfoC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; Field_Num, Item_Num, ItemSpace: Int64);
  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfoM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; Field_Num, Item_Num, ItemSpace: Int64) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfoP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; Field_Num, Item_Num, ItemSpace: Int64) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfoP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; Field_Num, Item_Num, ItemSpace: Int64);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfo = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfoC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfoM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfoP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_Build_Share_DiskC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info, Share_Directory_DB_Name: U_String);
  TC40_NetDisk_VM_Client_On_Usr_Build_Share_DiskM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info, Share_Directory_DB_Name: U_String) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Build_Share_DiskP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info, Share_Directory_DB_Name: U_String) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_Build_Share_DiskP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info, Share_Directory_DB_Name: U_String);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_Build_Share_Disk = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_Build_Share_DiskC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_Build_Share_DiskM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_Build_Share_DiskP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_Get_Share_DiskC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: U_StringArray);
  TC40_NetDisk_VM_Client_On_Usr_Get_Share_DiskM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: U_StringArray) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_Share_DiskP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: U_StringArray) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_Share_DiskP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: U_StringArray);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_Get_Share_DiskC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_Get_Share_DiskM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_Get_Share_DiskP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_ListC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List_Data_array);
  TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_ListM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List_Data_array) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_ListP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List_Data_array) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_ListP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List_Data_array);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_List = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_ListC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_ListM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_ListP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_InfoC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; L: TDirectory_MD5_Data_Frag_Struct_List);
  TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_InfoM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; L: TDirectory_MD5_Data_Frag_Struct_List) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_InfoP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; L: TDirectory_MD5_Data_Frag_Struct_List) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_InfoP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; L: TDirectory_MD5_Data_Frag_Struct_List);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_Info = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_InfoC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_InfoM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_InfoP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File_Data = record
    Current_Field, FieldOrItem: SystemString;
    Num: Int64;
    ModificationTime: TDateTime;
  end;

  TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File_Data_array = array of TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File_Data;

  TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File_Data_array);
  TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File_Data_array) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File_Data_array) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File_Data_array);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldC = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: U_StringArray);
  TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldM = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: U_StringArray) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldP = procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: U_StringArray) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldP = reference to procedure(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; arry: U_StringArray);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
  end;

  TC40_NetDisk_VM_Client_On_Usr_Auto_Post_FileC = procedure(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object; stream: TCore_Stream; Successed: Boolean; info: U_String);
  TC40_NetDisk_VM_Client_On_Usr_Auto_Post_FileM = procedure(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object; stream: TCore_Stream; Successed: Boolean; info: U_String) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Auto_Post_FileP = procedure(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object; stream: TCore_Stream; Successed: Boolean; info: U_String) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_Auto_Post_FileP = reference to procedure(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object; stream: TCore_Stream; Successed: Boolean; info: U_String);
{$ENDIF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File = class;
  PON_Usr_Auto_Post_File = ^TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File;
  TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File_Pool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File>;

  TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File_Ptr_ = record
    Instance_: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File;
  end;

  PON_Usr_Auto_Post_File_Ptr = ^TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File_Ptr_;

  TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File = class
  private
    Current_FS2_AliasOrHash: U_String;
    Current_Stream_Chunk_Pos: Int64;
    Current_Stream_Chunk_Siz: Int64;
    Current_Stream_Chunk_MD5: TMD5;
    Current_Stream_Chunk: TMem64;
    IsBusy, IsExit: Boolean;
    Ptr_: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File_Ptr_;
  public
    Client: TC40_NetDisk_VM_Client;
    Chunk_Size: Int64;
    MD5_Buff: TMD5_Pool;
    MD5: TMD5;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_FileC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_FileM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_FileP;
    DB_Field, DB_Item: U_String;
    stream: TCore_Stream;
    Done_Free_Stream: Boolean;
    FileTime_: TDateTime;
    State_Completed, State_Total: Int64;
    UserData: TCore_Object;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Result(Successed: Boolean; info: U_String);             // step done.
    procedure Do_Done_And_DelayFree(Successed: Boolean; info: U_String); // step done.
    procedure Compute_Stream_MD5;
    procedure Do_Compute_Stream_MD5; // step 1
    procedure Do_Done_Compute_Stream_MD5;
    procedure Do_Compute_Stream_MD5_Error;                                                                           // step done.
    procedure Do_CheckAndCopy_NetDisk_File(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);      // step 2
    procedure Do_SearchMultiMD5_FS_Service(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);      // step 3
    procedure Do_BeginPost_NetDisk_File(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);         // step 4
    procedure Do_CheckAndCopy_NetDisk_File_Frag(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String); // loop 5
    procedure Do_Done_PostFile_Frag(Successed: Boolean);                                                             // loop 5
    procedure Do_EndPost_NetDisk_File(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);           // step done.
  end;

  TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileC = procedure(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object; stream: TCore_Stream; Successed: Boolean; info: U_String);
  TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileM = procedure(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object; stream: TCore_Stream; Successed: Boolean; info: U_String) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileP = procedure(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object; stream: TCore_Stream; Successed: Boolean; info: U_String) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileP = reference to procedure(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object; stream: TCore_Stream; Successed: Boolean; info: U_String);
{$ENDIF FPC}
  TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File = class;
  PON_Usr_Auto_Get_File = ^TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File;
  TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File_Pool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File>;

  TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File_Ptr_ = record
    Instance_: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File;
  end;

  PON_Usr_Auto_Get_File_Ptr = ^TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File_Ptr_;

  TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File = class
  private
    Current_Remote_Frag_Index: Integer;
    Current_Local_MD5_Chunk: array of TMD5;
    IsBusy, IsExit: Boolean;
    Ptr_: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File_Ptr_;
  public
    Client: TC40_NetDisk_VM_Client;
    OnResultC: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileC;
    OnResultM: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileM;
    OnResultP: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileP;
    Remote_Frag_List: TDirectory_MD5_Data_Frag_Struct_List;
    stream: TCore_Stream;
    Done_Free_Stream: Boolean;
    State_Completed, State_Total: Int64;
    UserData: TCore_Object;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Result(Successed: Boolean; info: U_String);
    procedure Do_Done_And_DelayFree(Successed: Boolean; info: U_String);
    procedure Do_Usr_Get_NetDisk_File_Frag_Info(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; L: TDirectory_MD5_Data_Frag_Struct_List); // step 1
    procedure Do_Compute_Local_Frag;                                                                                                                          // step 2
    procedure Do_File_Same;
    procedure Do_File_Downloaded;                                                                                          // step done.
    procedure Do_Download_Frag;                                                                                            // step 3
    procedure Do_Get_NetDisk_File_Frag_MD5(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; MD5: TMD5); // loop 4
    procedure Do_Download_Frag_Done(m64: TMS64);                                                                           // loop 5
    procedure Do_Download_Frag_Error();
  end;

  TC40_NetDisk_VM_Client_OnEvent = procedure(sender: TC40_NetDisk_VM_Client) of object;

  TC40_NetDisk_VM_Client_Clone_Bridge_Event_C = procedure(sender: TC40_NetDisk_VM_Client; New_Instance: TC40_NetDisk_VM_Client);
  TC40_NetDisk_VM_Client_Clone_Bridge_Event_M = procedure(sender: TC40_NetDisk_VM_Client; New_Instance: TC40_NetDisk_VM_Client) of object;
{$IFDEF FPC}
  TC40_NetDisk_VM_Client_Clone_Bridge_Event_P = procedure(sender: TC40_NetDisk_VM_Client; New_Instance: TC40_NetDisk_VM_Client) is nested;
{$ELSE FPC}
  TC40_NetDisk_VM_Client_Clone_Bridge_Event_P = reference to procedure(sender: TC40_NetDisk_VM_Client; New_Instance: TC40_NetDisk_VM_Client);
{$ENDIF FPC}

  TC40_NetDisk_VM_Client_Clone_Bridge = class
  private
    procedure Do_BuildDependNetwork_Done(const state: Boolean);
    procedure Do_AuthDone(sender: TC40_NetDisk_VM_Client);
  public
    Source: TC40_NetDisk_VM_Client;
    OnEvent_C: TC40_NetDisk_VM_Client_Clone_Bridge_Event_C;
    OnEvent_M: TC40_NetDisk_VM_Client_Clone_Bridge_Event_M;
    OnEvent_P: TC40_NetDisk_VM_Client_Clone_Bridge_Event_P;
    New_Instance: TC40_NetDisk_VM_Client;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Event(Source_, New_Intance_: TC40_NetDisk_VM_Client);
  end;

{$ENDREGION 'event'}

  I_C40_NetDisk_VM_Client_Event = interface
    procedure Do_UserMsg(sender: TC40_NetDisk_VM_Client; FromUserName_, ToUserName_, msg_: U_String);
    procedure Do_UserOnline(sender: TC40_NetDisk_VM_Client; userName_, ToUserName_: U_String);
    procedure Do_UserOffline(sender: TC40_NetDisk_VM_Client; userName_, ToUserName_: U_String);
    procedure Do_UserRequestFriend(sender: TC40_NetDisk_VM_Client; FromUserName_, DestFriendUserName_, msg_: U_String);
    procedure Do_Auto_Begin_MD5(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object; stream: TCore_Stream);
    procedure Do_Auto_End_MD5(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object; stream: TCore_Stream);
    procedure Do_Auto_Post_State(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object; stream: TCore_Stream; Completed, Total: Int64);
    procedure Do_Auto_Get_State(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object; stream: TCore_Stream; Completed, Total: Int64);
  end;

  TC40_NetDisk_VM_Client = class(TC40_NoAuth_VM_Client)
  private
    // IM Event
    procedure cmd_userMsg(sender: TPeerIO; InData: TDFE);
    procedure cmd_userOnline(sender: TPeerIO; InData: TDFE);
    procedure cmd_userOffline(sender: TPeerIO; InData: TDFE);
    procedure cmd_userRequestFriend(sender: TPeerIO; InData: TDFE);
  private
    // netdisk vm event
    FAuto_Post_File_Pool: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File_Pool;
    FAuto_Get_File_Pool: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File_Pool;
    procedure cmd_Done_PostFile_Frag(sender: TPeerIO; InData: TDFE);
    procedure cmd_Done_Get_File_Frag(sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure cmd_Get_File_Error(sender: TPeerIO; InData: TDFE);
  protected
    procedure DoNetworkOffline; override; // trigger: offline
  protected
    FFile_Chunk_Size: Int64;
    procedure Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(sender: TDT_P2PVM_NoAuth_Client); override;
    procedure Do_Get_NetDisk_Config(sender: TPeerIO; Result_: TDataFrameEngine);
    procedure Do_Reconnect_Usr_Auth(sender: TC40_NetDisk_VM_Client; State_: Boolean; info_: SystemString);
  public
    OnEvent: I_C40_NetDisk_VM_Client_Event;
    Last_UserName, Last_Passwd, Last_PrimaryIdentifier: U_String;
    Auth_Done: Boolean;
    On_Auth_Done: TC40_NetDisk_VM_Client_OnEvent;
    All_Ready_Is_Done: Boolean;
    constructor Create(Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure Do_Auth_Done; virtual;
    procedure Disconnect; override;
    procedure Clone_C(OnResult: TC40_NetDisk_VM_Client_Clone_Bridge_Event_C);
    procedure Clone_M(OnResult: TC40_NetDisk_VM_Client_Clone_Bridge_Event_M);
    procedure Clone_P(OnResult: TC40_NetDisk_VM_Client_Clone_Bridge_Event_P);

    // User auth
    procedure AuthC(userName_, Passwd_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_AuthC);
    procedure AuthM(userName_, Passwd_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_AuthM);
    procedure AuthP(userName_, Passwd_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_AuthP);
    // User reg
    procedure RegC(userName_, Passwd_: U_String; Key_Value_Data_: THashStringList; OnResult: TC40_NetDisk_VM_Client_On_Usr_RegC);
    procedure RegM(userName_, Passwd_: U_String; Key_Value_Data_: THashStringList; OnResult: TC40_NetDisk_VM_Client_On_Usr_RegM);
    procedure RegP(userName_, Passwd_: U_String; Key_Value_Data_: THashStringList; OnResult: TC40_NetDisk_VM_Client_On_Usr_RegP);
    // new user login name
    procedure NewLoginName_C(NewLogin_Name_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_NewLoginNameC);
    procedure NewLoginName_M(NewLogin_Name_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_NewLoginNameM);
    procedure NewLoginName_P(NewLogin_Name_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_NewLoginNameP);
    // new alias
    procedure NewAlias(NewAlias_Name_: U_String);
    // get alias
    procedure GetAlias_C(User_Name_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_GetAliasC);
    procedure GetAlias_M(User_Name_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_GetAliasM);
    procedure GetAlias_P(User_Name_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_GetAliasP);
    // send msg
    procedure Msg(ToUserName_, msg_: U_String);
    // reqeust add friend
    procedure RequestFriend(ToUserName_, msg_: U_String);
    // reponse add friend
    procedure ReponseFriend(ToUserName_, msg_: U_String; Accept_: Boolean);
    // remove friend
    procedure RemoveFriend(ToUserName_: U_String);
    // get my friend
    procedure GetMyFriends_C(OnResult: TC40_NetDisk_VM_Client_On_Usr_GetMyFriendsC);
    procedure GetMyFriends_M(OnResult: TC40_NetDisk_VM_Client_On_Usr_GetMyFriendsM);
    procedure GetMyFriends_P(OnResult: TC40_NetDisk_VM_Client_On_Usr_GetMyFriendsP);
    // get online num
    procedure GetOnlineNum_C(OnResult: TC40_NetDisk_VM_Client_On_Usr_GetOnlineNumC);
    procedure GetOnlineNum_M(OnResult: TC40_NetDisk_VM_Client_On_Usr_GetOnlineNumM);
    procedure GetOnlineNum_P(OnResult: TC40_NetDisk_VM_Client_On_Usr_GetOnlineNumP);
    // get online line
    procedure GetOnlineList_C(Max_Num: Integer; OnResult: TC40_NetDisk_VM_Client_On_Usr_GetOnlineListC);
    procedure GetOnlineList_M(Max_Num: Integer; OnResult: TC40_NetDisk_VM_Client_On_Usr_GetOnlineListM);
    procedure GetOnlineList_P(Max_Num: Integer; OnResult: TC40_NetDisk_VM_Client_On_Usr_GetOnlineListP);
    // get fs service info
    procedure Get_FS_Service_C(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_FS_ServiceC);
    procedure Get_FS_Service_M(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_FS_ServiceM);
    procedure Get_FS_Service_P(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_FS_ServiceP);
    // auto search block from FS system
    procedure SearchMultiMD5_FS_Service_C(md5_arry: TArrayMD5; OnResult: TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_ServiceC);
    procedure SearchMultiMD5_FS_Service_M(md5_arry: TArrayMD5; OnResult: TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_ServiceM);
    procedure SearchMultiMD5_FS_Service_P(md5_arry: TArrayMD5; OnResult: TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_ServiceP);
    // check and copy file for directory service
    procedure CheckAndCopy_NetDisk_File_C(file_MD5: TMD5; file_Name: U_String; file_time: Double; file_Size: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_FileC);
    procedure CheckAndCopy_NetDisk_File_M(file_MD5: TMD5; file_Name: U_String; file_time: Double; file_Size: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_FileM);
    procedure CheckAndCopy_NetDisk_File_P(file_MD5: TMD5; file_Name: U_String; file_time: Double; file_Size: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_FileP);
    // begin post
    procedure BeginPost_NetDisk_File_C(alias_or_hash_: U_String; file_MD5: TMD5; file_Name: U_String; file_time: Double; file_Size: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_FileC);
    procedure BeginPost_NetDisk_File_M(alias_or_hash_: U_String; file_MD5: TMD5; file_Name: U_String; file_time: Double; file_Size: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_FileM);
    procedure BeginPost_NetDisk_File_P(alias_or_hash_: U_String; file_MD5: TMD5; file_Name: U_String; file_time: Double; file_Size: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_FileP);
    // check and copy fragment for fs service
    procedure CheckAndCopy_NetDisk_File_Frag_C(alias_or_hash_: U_String; frag_md5_: TMD5; frag_pos_, frag_size_: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_FragC);
    procedure CheckAndCopy_NetDisk_File_Frag_M(alias_or_hash_: U_String; frag_md5_: TMD5; frag_pos_, frag_size_: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_FragM);
    procedure CheckAndCopy_NetDisk_File_Frag_P(alias_or_hash_: U_String; frag_md5_: TMD5; frag_pos_, frag_size_: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_FragP);
    // post fragment for fs service
    procedure Post_NetDisk_File_Frag(Pos_: Int64; Event_, buff: Pointer; buff_size: Int64);
    // end post
    procedure EndPost_NetDisk_File_C(OnResult: TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_FileC);
    procedure EndPost_NetDisk_File_M(OnResult: TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_FileM);
    procedure EndPost_NetDisk_File_P(OnResult: TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_FileP);
    // get file fragmeng info from directory service
    procedure Get_NetDisk_File_Frag_Info_C(DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_InfoC);
    procedure Get_NetDisk_File_Frag_Info_M(DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_InfoM);
    procedure Get_NetDisk_File_Frag_Info_P(DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_InfoP);
    // get file fragment md5 from directory service
    procedure Get_NetDisk_File_Frag_MD5_C(alias_or_hash_, FS_File: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5C);
    procedure Get_NetDisk_File_Frag_MD5_M(alias_or_hash_, FS_File: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5M);
    procedure Get_NetDisk_File_Frag_MD5_P(alias_or_hash_, FS_File: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5P);
    // download fragment from fs service
    procedure Get_NetDisk_File_Frag(alias_or_hash_: U_String; FS_File: U_String; Pos_: Int64; Event_: Pointer);
    // my netdisk file list
    procedure Get_NetDisk_File_List_C(DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_ListC);
    procedure Get_NetDisk_File_List_M(DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_ListM);
    procedure Get_NetDisk_File_List_P(DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_ListP);
    // my netdisk space
    procedure Get_NetDisk_SpaceInfo_C(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfoC);
    procedure Get_NetDisk_SpaceInfo_M(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfoM);
    procedure Get_NetDisk_SpaceInfo_P(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfoP);
    // remove item
    procedure Remove_Item(DB_Field, DB_Remove_Item_: U_String);
    // remove field
    procedure Remove_Field(DB_Field, DB_Remove_Field_: U_String);
    // copy from netdisk
    procedure Copy_Item(arry: TCopyItem_Info_Array);
    procedure Copy_Field(arry: TCopyField_Info_Array);
    // create field
    procedure CreateField(DB_Field: U_String);
    // rename
    procedure RenameField(DB_Field, New_Field_Name: U_String);
    procedure RenameItem(DB_Field, Old_Item_Name, New_Item_Name: U_String);
    // build my share disk
    procedure Build_Share_Disk_C(OnResult: TC40_NetDisk_VM_Client_On_Usr_Build_Share_DiskC);
    procedure Build_Share_Disk_M(OnResult: TC40_NetDisk_VM_Client_On_Usr_Build_Share_DiskM);
    procedure Build_Share_Disk_P(OnResult: TC40_NetDisk_VM_Client_On_Usr_Build_Share_DiskP);
    // list my share disk
    procedure Get_Share_Disk_C(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_DiskC);
    procedure Get_Share_Disk_M(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_DiskM);
    procedure Get_Share_Disk_P(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_DiskP);
    // remove my share disk
    procedure Remove_Share_Disk(Share_Directory_DB_Name: U_String);
    // share netedisk file list
    procedure Get_Share_Disk_File_List_C(Share_Directory_DB_Name, DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_ListC);
    procedure Get_Share_Disk_File_List_M(Share_Directory_DB_Name, DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_ListM);
    procedure Get_Share_Disk_File_List_P(Share_Directory_DB_Name, DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_ListP);
    // share netedisk filefrag
    procedure Get_Share_Disk_File_Frag_Info_C(Share_Directory_DB_Name, DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_InfoC);
    procedure Get_Share_Disk_File_Frag_Info_M(Share_Directory_DB_Name, DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_InfoM);
    procedure Get_Share_Disk_File_Frag_Info_P(Share_Directory_DB_Name, DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_InfoP);
    // file search in my netdisk
    procedure Search_NetDisk_File_C(DB_Field, DB_Search: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileC);
    procedure Search_NetDisk_File_M(DB_Field, DB_Search: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileM);
    procedure Search_NetDisk_File_P(DB_Field, DB_Search: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileP);
    // file search in share netdisk
    procedure Search_Share_NetDisk_File_C(Share_Directory_DB_Name, DB_Field, DB_Search: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileC);
    procedure Search_Share_NetDisk_File_M(Share_Directory_DB_Name, DB_Field, DB_Search: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileM);
    procedure Search_Share_NetDisk_File_P(Share_Directory_DB_Name, DB_Field, DB_Search: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileP);
    // Field search in my netdisk
    procedure Search_NetDisk_Field_C(DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldC);
    procedure Search_NetDisk_Field_M(DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldM);
    procedure Search_NetDisk_Field_P(DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldP);
    // Field search in share netdisk
    procedure Search_Share_NetDisk_Field_C(Share_Directory_DB_Name, DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldC);
    procedure Search_Share_NetDisk_Field_M(Share_Directory_DB_Name, DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldM);
    procedure Search_Share_NetDisk_Field_P(Share_Directory_DB_Name, DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldP);
    // admin auth
    procedure Auth_AdminC(userName_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_AuthC);
    procedure Auth_AdminM(userName_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_AuthM);
    procedure Auth_AdminP(userName_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_AuthP);
    // close admin auth
    procedure Close_Auth_Admin(userName_: U_String);
    // *****************************************************************************************************************
    // Automated upload
    // *****************************************************************************************************************
    procedure Auto_Post_File_C(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; FileTime_: TDateTime; DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_FileC);
    procedure Auto_Post_File_M(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; FileTime_: TDateTime; DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_FileM);
    procedure Auto_Post_File_P(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; FileTime_: TDateTime; DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_FileP);
    // *****************************************************************************************************************
    // Automated download from my netdisk
    // *****************************************************************************************************************
    procedure Auto_Get_File_C(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileC);
    procedure Auto_Get_File_M(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileM);
    procedure Auto_Get_File_P(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileP);
    // *****************************************************************************************************************
    // Automated download from share netdisk
    // *****************************************************************************************************************
    procedure Auto_Get_File_From_Share_Disk_C(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; Share_Directory_DB_Name, DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileC);
    procedure Auto_Get_File_From_Share_Disk_M(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; Share_Directory_DB_Name, DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileM);
    procedure Auto_Get_File_From_Share_Disk_P(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; Share_Directory_DB_Name, DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileP);
  end;

  TC40_NetDisk_VM_Client_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_NetDisk_VM_Client>;

implementation


constructor TC40_NetDisk_VM_Client_On_Usr_Auth.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auth.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  if Result_.Count > 0 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
    end;

  if State_ then
    begin
      SendData.R.Index := 0;
      Client.Last_UserName := SendData.R.ReadString;
      Client.Last_Passwd := SendData.R.ReadString;
      if Result_.Count >= 3 then
          Client.Last_PrimaryIdentifier := Result_.R.ReadString;
      Client.Auth_Done := True;
      Client.DTNoAuthClient.ProgressEngine.PostExecuteM_NP(0, {$IFDEF FPC}@{$ENDIF FPC}Client.Do_Auth_Done);
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

procedure TC40_NetDisk_VM_Client_On_Usr_Auth.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
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

constructor TC40_NetDisk_VM_Client_On_Usr_Reg.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Reg.DoStreamParamEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := False;
  info_ := 'error.';
  if Result_.Count > 0 then
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

procedure TC40_NetDisk_VM_Client_On_Usr_Reg.DoStreamFailedEvent(sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
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

constructor TC40_NetDisk_VM_Client_On_Usr_NewLoginName.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_NewLoginName.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  State_: Boolean;
  info_: SystemString;
begin
  if Result_.Count > 0 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
    end
  else
    begin
      State_ := False;
      info_ := 'error';
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

constructor TC40_NetDisk_VM_Client_On_Usr_GetAlias.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_GetAlias.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Alias_: SystemString;
begin
  if Result_.R.NotEnd then
      Alias_ := Result_.R.ReadString
  else
      Alias_ := '';

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Alias_);
    if Assigned(OnResultM) then
        OnResultM(Client, Alias_);
    if Assigned(OnResultP) then
        OnResultP(Client, Alias_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_GetMyFriends.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_GetMyFriends.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  i: Integer;
  FriendArry: U_StringArray;
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
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_GetOnlineNum.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_GetOnlineNum.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  i: Integer;
  Online_Num, User_Num: Integer;
begin
  if Result_.Count > 0 then
    begin
      Online_Num := Result_.R.ReadInteger;
      User_Num := Result_.R.ReadInteger;
    end
  else
    begin
      Online_Num := 0;
      User_Num := 0;
    end;

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

constructor TC40_NetDisk_VM_Client_On_Usr_GetOnlineList.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_GetOnlineList.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
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

constructor TC40_NetDisk_VM_Client_On_Usr_Get_FS_Service.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Get_FS_Service.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  arry: TC40_NetDisk_VM_Client_On_Usr_FS_Service_Info_Array;
  i: Integer;
begin
  Successed := Result_.R.ReadBool;
  if Successed then
    begin
      SetLength(arry, (Result_.Count - 1) div 3);
      i := 0;
      while Result_.R.NotEnd do
        begin
          arry[i].AliasOrHash := Result_.R.ReadString;
          arry[i].Remote_FS_DB_Size := Result_.R.ReadInt64;
          arry[i].MaxFileSize := Result_.R.ReadInt64;
        end;
    end
  else
    begin
      SetLength(arry, 0);
      if not Client.Client.QuietMode then
          DoStatus(Result_.R.ReadString); // print error info.
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, arry);
  except
  end;
  for i := low(arry) to high(arry) do
      arry[i].AliasOrHash := '';
  SetLength(arry, 0);
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_Service.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_Service.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  info: U_String;
begin
  Successed := Result_.R.ReadBool;
  info := Result_.R.ReadString;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  info: U_String;
begin
  Successed := Result_.R.ReadBool;
  info := Result_.R.ReadString;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_File.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_File.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  info: U_String;
begin
  Successed := Result_.R.ReadBool;
  info := Result_.R.ReadString;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_Frag.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_Frag.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  info: U_String;
begin
  Successed := Result_.R.ReadBool;
  info := Result_.R.ReadString;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_File.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_File.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  info: U_String;
begin
  Successed := Result_.R.ReadBool;
  info := Result_.R.ReadString;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_Info.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_Info.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  L: TDirectory_MD5_Data_Frag_Struct_List;
  d: TDFE;
  info: U_String;
begin
  Successed := Result_.R.ReadBool;
  L := TDirectory_MD5_Data_Frag_Struct_List.Create;
  info := '';
  if Successed then
    begin
      d := TDFE.Create;
      Result_.R.ReadDataFrame(d);
      L.Decode(d);
    end
  else
    begin
      info := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info, L);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info, L);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info, L);
  except
  end;
  DelayFreeObject(1.0, self, L);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  info: U_String;
  MD5: TMD5;
begin
  Successed := Result_.R.ReadBool;
  info := Result_.R.ReadString;
  MD5 := NullMD5;
  if Successed then
      MD5 := Result_.R.ReadMD5;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info, MD5);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info, MD5);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info, MD5);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  info: U_String;
  arry: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List_Data_array;
  i: Integer;
begin
  Successed := Result_.R.ReadBool;
  info := '';
  SetLength(arry, 0);
  if Successed then
    begin
      info := Result_.R.ReadString;
      SetLength(arry, (Result_.Count - 2) div 3);
      i := 0;
      while Result_.R.NotEnd do
        begin
          arry[i].Name := Result_.R.ReadString;
          arry[i].Num := Result_.R.ReadInt64;
          arry[i].Time_ := Result_.R.ReadDouble;
          inc(i);
        end;
    end
  else
    begin
      info := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info, arry);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfo.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfo.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  info: U_String;
  Field_Num, Item_Num, ItemSpace: Int64;
begin
  Successed := Result_.R.ReadBool;
  info := Result_.R.ReadString;
  Field_Num := 0;
  Item_Num := 0;
  ItemSpace := 0;

  if Successed then
    begin
      Field_Num := Result_.R.ReadInt64;
      Item_Num := Result_.R.ReadInt64;
      ItemSpace := Result_.R.ReadInt64;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info, Field_Num, Item_Num, ItemSpace);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info, Field_Num, Item_Num, ItemSpace);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info, Field_Num, Item_Num, ItemSpace);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_Build_Share_Disk.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Build_Share_Disk.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  info, Share_Directory_DB_Name: U_String;
begin
  Successed := Result_.R.ReadBool;
  info := Result_.R.ReadString;
  Share_Directory_DB_Name := '';

  if Successed then
    begin
      Share_Directory_DB_Name := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info, Share_Directory_DB_Name);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info, Share_Directory_DB_Name);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info, Share_Directory_DB_Name);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  info: U_String;
  arry: U_StringArray;
  i: Integer;
begin
  Successed := Result_.R.ReadBool;
  info := Result_.R.ReadString;
  SetLength(arry, 0);

  if Successed then
    begin
      SetLength(arry, Result_.Count - 2);
      i := 0;
      while Result_.R.NotEnd do
        begin
          arry[i] := Result_.R.ReadString;
          inc(i);
        end;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info, arry);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_List.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_List.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  info: U_String;
  arry: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List_Data_array;
  i: Integer;
begin
  Successed := Result_.R.ReadBool;
  info := '';
  SetLength(arry, 0);
  if Successed then
    begin
      info := Result_.R.ReadString;
      SetLength(arry, (Result_.Count - 2) div 3);
      i := 0;
      while Result_.R.NotEnd do
        begin
          arry[i].Name := Result_.R.ReadString;
          arry[i].Num := Result_.R.ReadInt64;
          arry[i].Time_ := Result_.R.ReadDouble;
          inc(i);
        end;
    end
  else
    begin
      info := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info, arry);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_Info.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_Info.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  L: TDirectory_MD5_Data_Frag_Struct_List;
  d: TDFE;
  info: U_String;
begin
  Successed := Result_.R.ReadBool;
  L := TDirectory_MD5_Data_Frag_Struct_List.Create;
  info := '';
  if Successed then
    begin
      d := TDFE.Create;
      Result_.R.ReadDataFrame(d);
      L.Decode(d);
    end
  else
    begin
      info := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info, L);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info, L);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info, L);
  except
  end;
  DelayFreeObject(1.0, self, L);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  info: U_String;
  arry: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File_Data_array;
  i: Integer;
begin
  Successed := Result_.R.ReadBool;
  info := '';
  SetLength(arry, 0);
  if Successed then
    begin
      info := 'successed.';
      SetLength(arry, (Result_.Count - 1) shr 2);
      i := 0;
      while Result_.R.NotEnd do
        begin
          arry[i].Current_Field := Result_.R.ReadString;
          arry[i].FieldOrItem := Result_.R.ReadString;
          arry[i].Num := Result_.R.ReadInt64;
          arry[i].ModificationTime := Result_.R.ReadDouble;
          inc(i);
        end;
    end
  else
    begin
      info := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info, arry);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Successed: Boolean;
  info: U_String;
  arry: U_StringArray;
  i: Integer;
begin
  Successed := Result_.R.ReadBool;
  info := '';
  SetLength(arry, 0);
  if Successed then
    begin
      info := 'successed.';
      SetLength(arry, (Result_.Count - 1));
      i := 0;
      while Result_.R.NotEnd do
        begin
          arry[i] := Result_.R.ReadString;
          inc(i);
        end;
    end
  else
    begin
      info := Result_.R.ReadString;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info, arry);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Create;
begin
  inherited Create;
  Current_FS2_AliasOrHash := '';
  Current_Stream_Chunk_Pos := 0;
  Current_Stream_Chunk_Siz := 0;
  Current_Stream_Chunk_MD5 := NullMD5;
  Current_Stream_Chunk := TMem64.CustomCreate(1024 * 1024);
  IsBusy := False;
  IsExit := False;
  Ptr_.Instance_ := self;
  Client := nil;
  Chunk_Size := 1024 * 1024;
  MD5_Buff := TMD5_Pool.Create;
  MD5 := NullMD5;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
  DB_Field := '';
  DB_Item := '';
  stream := nil;
  Done_Free_Stream := False;
  FileTime_ := 0;
  State_Completed := 0;
  State_Total := 0;
  UserData := nil;
end;

destructor TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Destroy;
begin
  DisposeObject(Current_Stream_Chunk);
  DisposeObject(MD5_Buff);
  if Done_Free_Stream then
      DisposeObjectAndNil(stream);
  inherited Destroy;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Do_Result(Successed: Boolean; info: U_String);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, UserData, stream, Successed, info);
    if Assigned(OnResultM) then
        OnResultM(Client, UserData, stream, Successed, info);
    if Assigned(OnResultP) then
        OnResultP(Client, UserData, stream, Successed, info);
  except
  end;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Do_Done_And_DelayFree(Successed: Boolean; info: U_String);
var
  i: Integer;
begin
  Do_Result(Successed, info);

  // remove for pool
  i := 0;
  while i < Client.FAuto_Post_File_Pool.Count do
    if Client.FAuto_Post_File_Pool[i] = self then
        Client.FAuto_Post_File_Pool.Delete(i)
    else
        inc(i);

  // free self
  DelayFreeObject(0.1, self);
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Compute_Stream_MD5;
begin
  Client.FAuto_Post_File_Pool.Add(self);
  TCompute.RunM_NP({$IFDEF FPC}@{$ENDIF FPC}Do_Compute_Stream_MD5);
  IsBusy := True;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Do_Compute_Stream_MD5;
var
  tmp: TMS64;
  Siz: Int64;
  md5_cont: TMD5Context;
begin
  try
    if Assigned(Client.OnEvent) then
        Client.OnEvent.Do_Auto_Begin_MD5(Client, UserData, stream);
    tmp := TMS64.CustomCreate(Chunk_Size);
    Siz := stream.Size;
    stream.Position := 0;
    THashMD5.InitMD5(md5_cont);
    while Siz > 0 do
      begin
        if Siz > Chunk_Size then
          begin
            tmp.Clear;
            tmp.CopyFrom(stream, Chunk_Size);
            MD5_Buff.Add(tmp.ToMD5);
            THashMD5.UpdateMD5(md5_cont, tmp.Memory^, Chunk_Size);
            dec(Siz, Chunk_Size);
          end
        else if Siz > 0 then
          begin
            tmp.Clear;
            tmp.CopyFrom(stream, Siz);
            MD5_Buff.Add(tmp.ToMD5);
            THashMD5.UpdateMD5(md5_cont, tmp.Memory^, Siz);
            THashMD5.FinalizeMD5(md5_cont, MD5);
            Siz := 0;
          end;
        if IsExit then
            break;
      end;
    DisposeObject(tmp);
    if Assigned(Client.OnEvent) then
        Client.OnEvent.Do_Auto_End_MD5(Client, UserData, stream);
    SysProgress.PostM1({$IFDEF FPC}@{$ENDIF FPC}Do_Done_Compute_Stream_MD5);
  except
      SysProgress.PostM1({$IFDEF FPC}@{$ENDIF FPC}Do_Compute_Stream_MD5_Error);
  end;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Do_Done_Compute_Stream_MD5;
begin
  IsBusy := False;
  if IsExit then
      exit;

  if not Client.Connected then
    begin
      Do_Done_And_DelayFree(False, 'no connection.');
      exit;
    end;
  Client.CheckAndCopy_NetDisk_File_M(MD5, umlCombineUnixFileName(DB_Field, DB_Item), FileTime_, stream.Size, {$IFDEF FPC}@{$ENDIF FPC}Do_CheckAndCopy_NetDisk_File);
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Do_Compute_Stream_MD5_Error;
begin
  Do_Done_And_DelayFree(False, 'compute MD5 error.');
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Do_CheckAndCopy_NetDisk_File(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
var
  arry: TArrayMD5;
  i: Integer;
begin
  State_Total := stream.Size;
  State_Completed := 0;
  if Successed then
    begin
      State_Completed := State_Total;
      if Assigned(Client.OnEvent) then
          Client.OnEvent.Do_Auto_Post_State(Client, UserData, stream, State_Completed, State_Total);
      Do_Done_And_DelayFree(Successed, info);
      exit;
    end
  else
    begin
      if Assigned(Client.OnEvent) then
          Client.OnEvent.Do_Auto_Post_State(Client, UserData, stream, State_Completed, State_Total);
    end;
  SetLength(arry, umlMin(100, MD5_Buff.Count));
  for i := low(arry) to high(arry) do
      arry[i] := MD5_Buff[i];
  Client.SearchMultiMD5_FS_Service_M(arry, {$IFDEF FPC}@{$ENDIF FPC}Do_SearchMultiMD5_FS_Service);
  SetLength(arry, 0);
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Do_SearchMultiMD5_FS_Service(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
begin
  if not Successed then
    begin
      Do_Done_And_DelayFree(Successed, info);
      exit;
    end;
  Current_FS2_AliasOrHash := info;
  Client.BeginPost_NetDisk_File_M(Current_FS2_AliasOrHash,
    MD5, umlCombineUnixFileName(DB_Field, DB_Item), FileTime_, stream.Size, {$IFDEF FPC}@{$ENDIF FPC}Do_BeginPost_NetDisk_File);
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Do_BeginPost_NetDisk_File(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
begin
  if not Successed then
    begin
      Do_Done_And_DelayFree(Successed, info);
      exit;
    end;

  Current_Stream_Chunk_Pos := 0;
  Current_Stream_Chunk_Siz := umlMin(Chunk_Size, stream.Size - Current_Stream_Chunk_Pos);
  Current_Stream_Chunk.Clear;
  stream.Position := Current_Stream_Chunk_Pos;
  Current_Stream_Chunk.CopyFrom(stream, Current_Stream_Chunk_Siz);
  Current_Stream_Chunk_MD5 := Current_Stream_Chunk.ToMD5;

  Client.CheckAndCopy_NetDisk_File_Frag_M(Current_FS2_AliasOrHash,
    Current_Stream_Chunk_MD5, Current_Stream_Chunk_Pos, Current_Stream_Chunk_Siz, {$IFDEF FPC}@{$ENDIF FPC}Do_CheckAndCopy_NetDisk_File_Frag);
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Do_CheckAndCopy_NetDisk_File_Frag(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
begin
  if Successed then
    begin
      State_Completed := Current_Stream_Chunk_Pos;
      if Assigned(Client.OnEvent) then
          Client.OnEvent.Do_Auto_Post_State(Client, UserData, stream, State_Completed, State_Total);

      if Current_Stream_Chunk_Pos + Current_Stream_Chunk_Siz >= stream.Size then
        begin
          // successed
          Client.EndPost_NetDisk_File_M({$IFDEF FPC}@{$ENDIF FPC}Do_EndPost_NetDisk_File);
          exit;
        end;

      // to next chunk
      inc(Current_Stream_Chunk_Pos, Current_Stream_Chunk_Siz);
      Current_Stream_Chunk_Siz := umlMin(Chunk_Size, stream.Size - Current_Stream_Chunk_Pos);
      Current_Stream_Chunk.Clear;
      stream.Position := Current_Stream_Chunk_Pos;
      Current_Stream_Chunk.CopyFrom(stream, Current_Stream_Chunk_Siz);
      Current_Stream_Chunk_MD5 := Current_Stream_Chunk.ToMD5;

      Client.CheckAndCopy_NetDisk_File_Frag_M(Current_FS2_AliasOrHash,
        Current_Stream_Chunk_MD5, Current_Stream_Chunk_Pos, Current_Stream_Chunk_Siz, {$IFDEF FPC}@{$ENDIF FPC}Do_CheckAndCopy_NetDisk_File_Frag);
    end
  else
    begin
      Client.Post_NetDisk_File_Frag(Current_Stream_Chunk_Pos, @Ptr_, Current_Stream_Chunk.Memory, Current_Stream_Chunk.Size);
    end;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Do_Done_PostFile_Frag(Successed: Boolean);
begin
  if not Successed then
    begin
      Do_Done_And_DelayFree(Successed, 'Post_NetDisk_File_Frag error.');
      exit;
    end;

  State_Completed := Current_Stream_Chunk_Pos;
  if Assigned(Client.OnEvent) then
      Client.OnEvent.Do_Auto_Post_State(Client, UserData, stream, State_Completed, State_Total);

  if Current_Stream_Chunk_Pos + Current_Stream_Chunk_Siz >= stream.Size then
    begin
      // successed
      Client.EndPost_NetDisk_File_M({$IFDEF FPC}@{$ENDIF FPC}Do_EndPost_NetDisk_File);
      exit;
    end;

  // to next chunk
  inc(Current_Stream_Chunk_Pos, Current_Stream_Chunk_Siz);
  Current_Stream_Chunk_Siz := umlMin(Chunk_Size, stream.Size - Current_Stream_Chunk_Pos);
  Current_Stream_Chunk.Clear;
  stream.Position := Current_Stream_Chunk_Pos;
  Current_Stream_Chunk.CopyFrom(stream, Current_Stream_Chunk_Siz);
  Current_Stream_Chunk_MD5 := Current_Stream_Chunk.ToMD5;

  // recheck frag
  Client.CheckAndCopy_NetDisk_File_Frag_M(Current_FS2_AliasOrHash,
    Current_Stream_Chunk_MD5, Current_Stream_Chunk_Pos, Current_Stream_Chunk_Siz, {$IFDEF FPC}@{$ENDIF FPC}Do_CheckAndCopy_NetDisk_File_Frag);
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Do_EndPost_NetDisk_File(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String);
begin
  State_Completed := State_Total;
  if Assigned(Client.OnEvent) then
      Client.OnEvent.Do_Auto_Post_State(Client, UserData, stream, State_Completed, State_Total);
  Do_Done_And_DelayFree(Successed, info);
end;

constructor TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Create;
begin
  inherited Create;
  Current_Remote_Frag_Index := 0;
  SetLength(Current_Local_MD5_Chunk, 0);
  IsBusy := False;
  IsExit := False;
  Ptr_.Instance_ := self;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
  Remote_Frag_List := TDirectory_MD5_Data_Frag_Struct_List.Create;
  stream := nil;
  Done_Free_Stream := False;
  State_Completed := 0;
  State_Total := 0;
  UserData := nil;
end;

destructor TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Destroy;
begin
  DisposeObject(Remote_Frag_List);
  if Done_Free_Stream then
      DisposeObjectAndNil(stream);
  inherited Destroy;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Do_Result(Successed: Boolean; info: U_String);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, UserData, stream, Successed, info);
    if Assigned(OnResultM) then
        OnResultM(Client, UserData, stream, Successed, info);
    if Assigned(OnResultP) then
        OnResultP(Client, UserData, stream, Successed, info);
  except
  end;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Do_Done_And_DelayFree(Successed: Boolean; info: U_String);
var
  i: Integer;
begin
  Do_Result(Successed, info);

  // remove for pool
  i := 0;
  while i < Client.FAuto_Get_File_Pool.Count do
    if Client.FAuto_Get_File_Pool[i] = self then
        Client.FAuto_Get_File_Pool.Delete(i)
    else
        inc(i);

  // free self
  DelayFreeObject(0.1, self);
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Do_Usr_Get_NetDisk_File_Frag_Info(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; L: TDirectory_MD5_Data_Frag_Struct_List);
var
  i: Integer;
  p: PDirectory_MD5_Data_Frag_Struct;
begin
  if not Successed then
    begin
      Do_Done_And_DelayFree(Successed, info);
      exit;
    end;
  for i := 0 to L.Count - 1 do
    begin
      new(p);
      p^ := L[i]^;
      Remote_Frag_List.Add(p);
    end;
  Remote_Frag_List.MD5 := L.MD5;
  Remote_Frag_List.Size := L.Size;
  Remote_Frag_List.Time_ := L.Time_;
  State_Total := Remote_Frag_List.Size;
  State_Completed := 0;

  TCompute.RunM_NP({$IFDEF FPC}@{$ENDIF FPC}Do_Compute_Local_Frag);
  IsBusy := True;
  Client.FAuto_Get_File_Pool.Add(self);
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Do_Compute_Local_Frag;
var
  i: Integer;
  p: PDirectory_MD5_Data_Frag_Struct;
begin
  Remote_Frag_List.SortPos;
  State_Completed := 0;

  if Assigned(Client.OnEvent) then
      Client.OnEvent.Do_Auto_Begin_MD5(Client, UserData, stream);
  try
    stream.Size := Remote_Frag_List.Size;
    SetLength(Current_Local_MD5_Chunk, Remote_Frag_List.Count);
    for i := 0 to Remote_Frag_List.Count - 1 do
        Current_Local_MD5_Chunk[i] := Null_Buff_MD5;

    // compare stream md5
    if umlCompareMD5(umlStreamMD5(stream), Remote_Frag_List.MD5) or (Remote_Frag_List.Count = 0) then
      begin
        if Assigned(Client.OnEvent) then
            Client.OnEvent.Do_Auto_End_MD5(Client, UserData, stream);
        SysProgress.PostM1({$IFDEF FPC}@{$ENDIF FPC}Do_File_Same);
        exit;
      end;

    // compute local fragment
    i := 0;
    for i := 0 to Remote_Frag_List.Count - 1 do
      begin
        p := Remote_Frag_List[i];
        try
            Current_Local_MD5_Chunk[i] := umlStreamMD5(stream, p^.Pos_, p^.Pos_ + p^.Size_);
        except
            Current_Local_MD5_Chunk[i] := Null_Buff_MD5;
        end;
      end;
    Current_Remote_Frag_Index := 0;
    if Assigned(Client.OnEvent) then
        Client.OnEvent.Do_Auto_End_MD5(Client, UserData, stream);
    SysProgress.PostM1({$IFDEF FPC}@{$ENDIF FPC}Do_Download_Frag);
  except
      SysProgress.PostM1({$IFDEF FPC}@{$ENDIF FPC}Do_Download_Frag_Error);
  end;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Do_File_Same;
begin
  IsBusy := False;
  if IsExit then
      exit;
  State_Completed := State_Total;
  if Assigned(Client.OnEvent) then
      Client.OnEvent.Do_Auto_Get_State(Client, UserData, stream, State_Completed, State_Total);
  Do_Done_And_DelayFree(True, 'done.');
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Do_File_Downloaded;
begin
  State_Completed := State_Total;
  if Assigned(Client.OnEvent) then
      Client.OnEvent.Do_Auto_Get_State(Client, UserData, stream, State_Completed, State_Total);
  Do_Done_And_DelayFree(True, 'done.');
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Do_Download_Frag;
begin
  IsBusy := False;
  if IsExit then
      exit;
  Client.Get_NetDisk_File_Frag_MD5_M(
    Remote_Frag_List[Current_Remote_Frag_Index]^.FS_AliasOrHash,
    Remote_Frag_List[Current_Remote_Frag_Index]^.FS_File,
{$IFDEF FPC}@{$ENDIF FPC}Do_Get_NetDisk_File_Frag_MD5);
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Do_Get_NetDisk_File_Frag_MD5(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String; MD5: TMD5);
begin
  if Successed and umlCompareMD5(MD5, Current_Local_MD5_Chunk[Current_Remote_Frag_Index]) then
    begin
      if not Client.Client.QuietMode then
          DoStatus('skip chunk position:%d size:%d md5:%s',
          [Remote_Frag_List[Current_Remote_Frag_Index]^.Pos_, Remote_Frag_List[Current_Remote_Frag_Index]^.Size_, umlMD5ToStr(MD5).Text]);

      State_Completed := State_Completed + Remote_Frag_List[Current_Remote_Frag_Index]^.Size_;
      if Assigned(Client.OnEvent) then
          Client.OnEvent.Do_Auto_Get_State(Client, UserData, stream, State_Completed, State_Total);

      // do next
      if Current_Remote_Frag_Index + 1 < Remote_Frag_List.Count then
        begin
          inc(Current_Remote_Frag_Index);
          Do_Download_Frag();
        end
      else
        begin
          Do_File_Downloaded();
        end;
    end
  else
    begin
      // begin download
      Client.Get_NetDisk_File_Frag(
        Remote_Frag_List[Current_Remote_Frag_Index]^.FS_AliasOrHash,
        Remote_Frag_List[Current_Remote_Frag_Index]^.FS_File,
        Remote_Frag_List[Current_Remote_Frag_Index]^.Pos_,
        @Ptr_);
    end;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Do_Download_Frag_Done(m64: TMS64);
begin
  m64.Position := 0;
  stream.Position := Remote_Frag_List[Current_Remote_Frag_Index]^.Pos_;
  stream.CopyFrom(m64, m64.Size);

  State_Completed := State_Completed + m64.Size;
  if Assigned(Client.OnEvent) then
      Client.OnEvent.Do_Auto_Get_State(Client, UserData, stream, State_Completed, State_Total);

  // do next
  if Current_Remote_Frag_Index + 1 < Remote_Frag_List.Count then
    begin
      inc(Current_Remote_Frag_Index);
      Do_Download_Frag();
    end
  else
    begin
      Do_File_Downloaded();
    end;
end;

procedure TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Do_Download_Frag_Error;
begin
  Do_Done_And_DelayFree(False, 'frag error.');
end;

procedure TC40_NetDisk_VM_Client_Clone_Bridge.Do_BuildDependNetwork_Done(const state: Boolean);
begin
  if not state then
    begin
      Do_Event(nil, nil);
      DisposeObject(New_Instance);
    end;
end;

procedure TC40_NetDisk_VM_Client_Clone_Bridge.Do_AuthDone(sender: TC40_NetDisk_VM_Client);
begin
  Do_Event(Source, sender);
  New_Instance.On_Auth_Done := nil;
end;

constructor TC40_NetDisk_VM_Client_Clone_Bridge.Create;
begin
  inherited Create;
  Source := nil;
  OnEvent_C := nil;
  OnEvent_M := nil;
  OnEvent_P := nil;
  New_Instance := nil;
end;

destructor TC40_NetDisk_VM_Client_Clone_Bridge.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_NetDisk_VM_Client_Clone_Bridge.Do_Event(Source_, New_Intance_: TC40_NetDisk_VM_Client);
begin
  if Assigned(OnEvent_C) then
      OnEvent_C(Source_, New_Intance_);
  if Assigned(OnEvent_M) then
      OnEvent_M(Source_, New_Intance_);
  if Assigned(OnEvent_P) then
      OnEvent_P(Source_, New_Intance_);
  DelayFreeObj(1.0, self);
end;

procedure TC40_NetDisk_VM_Client.cmd_userMsg(sender: TPeerIO; InData: TDFE);
var
  FromUserName_, ToUserName_, msg_: U_String;
begin
  FromUserName_ := InData.R.ReadString;
  ToUserName_ := InData.R.ReadString;
  msg_ := InData.R.ReadString;
  if Assigned(OnEvent) then
      OnEvent.Do_UserMsg(self, FromUserName_, ToUserName_, msg_);
end;

procedure TC40_NetDisk_VM_Client.cmd_userOnline(sender: TPeerIO; InData: TDFE);
var
  userName_, ToUserName_: U_String;
begin
  userName_ := InData.R.ReadString;
  ToUserName_ := InData.R.ReadString;
  if Assigned(OnEvent) then
      OnEvent.Do_UserOnline(self, userName_, ToUserName_);
end;

procedure TC40_NetDisk_VM_Client.cmd_userOffline(sender: TPeerIO; InData: TDFE);
var
  userName_, ToUserName_: U_String;
begin
  userName_ := InData.R.ReadString;
  ToUserName_ := InData.R.ReadString;
  if Assigned(OnEvent) then
      OnEvent.Do_UserOffline(self, userName_, ToUserName_);
end;

procedure TC40_NetDisk_VM_Client.cmd_userRequestFriend(sender: TPeerIO; InData: TDFE);
var
  FromUserName_, DestFriendUserName_, msg_: U_String;
begin
  FromUserName_ := InData.R.ReadString;
  DestFriendUserName_ := InData.R.ReadString;
  msg_ := InData.R.ReadString;
  if Assigned(OnEvent) then
      OnEvent.Do_UserRequestFriend(self, FromUserName_, DestFriendUserName_, msg_);
end;

procedure TC40_NetDisk_VM_Client.cmd_Done_PostFile_Frag(sender: TPeerIO; InData: TDFE);
var
  Successed: Boolean;
  Event_: Pointer;
begin
  Successed := InData.R.ReadBool;
  Event_ := Pointer(InData.R.ReadPointer);

  if Event_ <> nil then
    begin
      PON_Usr_Auto_Post_File_Ptr(Event_)^.Instance_.Do_Done_PostFile_Frag(Successed);
    end;
end;

procedure TC40_NetDisk_VM_Client.cmd_Done_Get_File_Frag(sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  Pos_: Int64;
  Event_: Pointer;
  m64: TMS64;
begin
  Pos_ := PInt64(GetOffset(InData, 0))^;
  Event_ := Pointer(PUInt64(GetOffset(InData, 8))^);
  m64 := TMS64.Create;
  m64.Mapping(GetOffset(InData, 16), DataSize - 16);

  if Event_ <> nil then
    begin
      PON_Usr_Auto_Get_File_Ptr(Event_)^.Instance_.Do_Download_Frag_Done(m64);
    end;
  DisposeObject(m64);
end;

procedure TC40_NetDisk_VM_Client.cmd_Get_File_Error(sender: TPeerIO; InData: TDFE);
var
  Pos_: Int64;
  Event_: Pointer;
begin
  Pos_ := InData.R.ReadInt64;
  Event_ := Pointer(InData.R.ReadPointer);

  if Event_ <> nil then
    begin
      PON_Usr_Auto_Get_File_Ptr(Event_)^.Instance_.Do_Download_Frag_Error();
    end;
end;

procedure TC40_NetDisk_VM_Client.DoNetworkOffline;
var
  i: Integer;
begin
  inherited DoNetworkOffline;
  for i := 0 to FAuto_Post_File_Pool.Count - 1 do
    begin
      FAuto_Post_File_Pool[i].IsExit := True;
      while FAuto_Post_File_Pool[i].IsBusy do
          CheckThread(1);
      FAuto_Post_File_Pool[i].Do_Result(False, 'offline.');
      DisposeObject(FAuto_Post_File_Pool[i]);
    end;
  FAuto_Post_File_Pool.Clear;

  for i := 0 to FAuto_Get_File_Pool.Count - 1 do
    begin
      FAuto_Get_File_Pool[i].IsExit := True;
      while FAuto_Get_File_Pool[i].IsBusy do
          CheckThread(1);
      FAuto_Get_File_Pool[i].Do_Result(False, 'offline.');
      DisposeObject(FAuto_Get_File_Pool[i]);
    end;
  FAuto_Get_File_Pool.Clear;
  All_Ready_Is_Done := False;
end;

procedure TC40_NetDisk_VM_Client.Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(sender: TDT_P2PVM_NoAuth_Client);
var
  d: TDFE;
begin
  inherited Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(sender);
  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_NetDisk_Config', d, {$IFDEF FPC}@{$ENDIF FPC}Do_Get_NetDisk_Config);
  DisposeObject(d);
  if Auth_Done then
    begin
      AuthM(Last_UserName, Last_Passwd, {$IFDEF FPC}@{$ENDIF FPC}Do_Reconnect_Usr_Auth);
    end;
end;

procedure TC40_NetDisk_VM_Client.Do_Get_NetDisk_Config(sender: TPeerIO; Result_: TDataFrameEngine);
begin
  FFile_Chunk_Size := Result_.R.ReadInt64;
  if not Client.QuietMode then
      DoStatus('Chunk: %d', [FFile_Chunk_Size]);
end;

procedure TC40_NetDisk_VM_Client.Do_Reconnect_Usr_Auth(sender: TC40_NetDisk_VM_Client; State_: Boolean; info_: SystemString);
begin
  Auth_Done := State_;
  if not Client.QuietMode then
      DoStatus(info_);
  if not State_ then
    begin
      Client.PhysicsTunnel.DelayCloseIO(1);
    end;
end;

constructor TC40_NetDisk_VM_Client.Create(Param_: U_String);
begin
  inherited Create(Param_);
  // IM
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('userMsg').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_userMsg;
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('userOnline').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_userOnline;
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('userOffline').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_userOffline;
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('userRequestFriend').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_userRequestFriend;
  // network vm
  FAuto_Post_File_Pool := TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File_Pool.Create;
  FAuto_Get_File_Pool := TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File_Pool.Create;
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('Done_PostFile_Frag').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Done_PostFile_Frag;
  DTNoAuthClient.RecvTunnel.RegisterCompleteBuffer('Done_Get_File_Frag').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Done_Get_File_Frag;
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('Get_File_Error').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Get_File_Error;

  FFile_Chunk_Size := 1024;
  OnEvent := nil;
  Last_UserName := '';
  Last_Passwd := '';
  Last_PrimaryIdentifier := '';
  Auth_Done := False;
  On_Auth_Done := nil;
  All_Ready_Is_Done := False;
end;

destructor TC40_NetDisk_VM_Client.Destroy;
var
  i: Integer;
begin
  // free Post event
  for i := 0 to FAuto_Post_File_Pool.Count - 1 do
    begin
      FAuto_Post_File_Pool[i].IsExit := True;
      while FAuto_Post_File_Pool[i].IsBusy do
          CheckThread(1);
      FAuto_Post_File_Pool[i].Do_Result(False, 'offline.');
      DisposeObject(FAuto_Post_File_Pool[i]);
    end;
  DisposeObject(FAuto_Post_File_Pool);

  // free Get event
  for i := 0 to FAuto_Get_File_Pool.Count - 1 do
    begin
      FAuto_Get_File_Pool[i].IsExit := True;
      while FAuto_Get_File_Pool[i].IsBusy do
          CheckThread(1);
      FAuto_Get_File_Pool[i].Do_Result(False, 'offline.');
      DisposeObject(FAuto_Get_File_Pool[i]);
    end;
  DisposeObject(FAuto_Get_File_Pool);

  OnEvent := nil;
  inherited Destroy;
end;

procedure TC40_NetDisk_VM_Client.Progress;
begin
  inherited Progress;
end;

procedure TC40_NetDisk_VM_Client.Do_Auth_Done;
begin
  All_Ready_Is_Done := True;
  if Assigned(On_Auth_Done) then
      On_Auth_Done(self);
end;

procedure TC40_NetDisk_VM_Client.Disconnect;
begin
  Auth_Done := False;
  All_Ready_Is_Done := False;
  inherited Disconnect;
end;

procedure TC40_NetDisk_VM_Client.Clone_C(OnResult: TC40_NetDisk_VM_Client_Clone_Bridge_Event_C);
var
  tmp: TC40_NetDisk_VM_Client_Clone_Bridge;
begin
  tmp := TC40_NetDisk_VM_Client_Clone_Bridge.Create;
  tmp.Source := self;
  tmp.OnEvent_C := OnResult;
  if not Auth_Done then
    begin
      tmp.Do_Event(nil, nil);
      exit;
    end;

  tmp.New_Instance := TC40_NetDisk_VM_Client_Class(ClassType).Create(param);
  tmp.New_Instance.Client.QuietMode := Client.QuietMode;
  tmp.New_Instance.Last_UserName := Last_UserName;
  tmp.New_Instance.Last_Passwd := Last_Passwd;
  tmp.New_Instance.Last_PrimaryIdentifier := Last_PrimaryIdentifier;
  tmp.New_Instance.Auth_Done := True;
  tmp.New_Instance.On_Auth_Done := {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_AuthDone;
  tmp.New_Instance.Connect_M(Client.LastAddr, Client.LastPort, Client.LastAuth, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_BuildDependNetwork_Done);
end;

procedure TC40_NetDisk_VM_Client.Clone_M(OnResult: TC40_NetDisk_VM_Client_Clone_Bridge_Event_M);
var
  tmp: TC40_NetDisk_VM_Client_Clone_Bridge;
begin
  tmp := TC40_NetDisk_VM_Client_Clone_Bridge.Create;
  tmp.Source := self;
  tmp.OnEvent_M := OnResult;
  if not Auth_Done then
    begin
      tmp.Do_Event(nil, nil);
      exit;
    end;

  tmp.New_Instance := TC40_NetDisk_VM_Client_Class(ClassType).Create(param);
  tmp.New_Instance.Client.QuietMode := Client.QuietMode;
  tmp.New_Instance.Last_UserName := Last_UserName;
  tmp.New_Instance.Last_Passwd := Last_Passwd;
  tmp.New_Instance.Last_PrimaryIdentifier := Last_PrimaryIdentifier;
  tmp.New_Instance.Auth_Done := True;
  tmp.New_Instance.On_Auth_Done := {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_AuthDone;
  tmp.New_Instance.Connect_M(Client.LastAddr, Client.LastPort, Client.LastAuth, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_BuildDependNetwork_Done);
end;

procedure TC40_NetDisk_VM_Client.Clone_P(OnResult: TC40_NetDisk_VM_Client_Clone_Bridge_Event_P);
var
  tmp: TC40_NetDisk_VM_Client_Clone_Bridge;
begin
  tmp := TC40_NetDisk_VM_Client_Clone_Bridge.Create;
  tmp.Source := self;
  tmp.OnEvent_P := OnResult;
  if not Auth_Done then
    begin
      tmp.Do_Event(nil, nil);
      exit;
    end;

  tmp.New_Instance := TC40_NetDisk_VM_Client_Class(ClassType).Create(param);
  tmp.New_Instance.Client.QuietMode := Client.QuietMode;
  tmp.New_Instance.Last_UserName := Last_UserName;
  tmp.New_Instance.Last_Passwd := Last_Passwd;
  tmp.New_Instance.Last_PrimaryIdentifier := Last_PrimaryIdentifier;
  tmp.New_Instance.Auth_Done := True;
  tmp.New_Instance.On_Auth_Done := {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_AuthDone;
  tmp.New_Instance.Connect_M(Client.LastAddr, Client.LastPort, Client.LastAuth, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_BuildDependNetwork_Done);
end;

procedure TC40_NetDisk_VM_Client.AuthC(userName_, Passwd_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_AuthC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auth;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auth.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  d.WriteString(userName_);
  d.WriteString(Passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Auth', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.AuthM(userName_, Passwd_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_AuthM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auth;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auth.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  d.WriteString(userName_);
  d.WriteString(Passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Auth', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.AuthP(userName_, Passwd_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_AuthP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auth;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auth.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  d.WriteString(userName_);
  d.WriteString(Passwd_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Auth', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.RegC(userName_, Passwd_: U_String; Key_Value_Data_: THashStringList; OnResult: TC40_NetDisk_VM_Client_On_Usr_RegC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Reg;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Reg.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  d.WriteString(userName_);
  d.WriteString(Passwd_);
  if Key_Value_Data_ <> nil then
      d.WriteHashStringList(Key_Value_Data_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Reg', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.RegM(userName_, Passwd_: U_String; Key_Value_Data_: THashStringList; OnResult: TC40_NetDisk_VM_Client_On_Usr_RegM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Reg;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Reg.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  d.WriteString(userName_);
  d.WriteString(Passwd_);
  if Key_Value_Data_ <> nil then
      d.WriteHashStringList(Key_Value_Data_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Reg', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.RegP(userName_, Passwd_: U_String; Key_Value_Data_: THashStringList; OnResult: TC40_NetDisk_VM_Client_On_Usr_RegP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Reg;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Reg.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  d.WriteString(userName_);
  d.WriteString(Passwd_);
  if Key_Value_Data_ <> nil then
      d.WriteHashStringList(Key_Value_Data_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Reg', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.NewLoginName_C(NewLogin_Name_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_NewLoginNameC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_NewLoginName;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_NewLoginName.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(NewLogin_Name_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NewLoginName', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.NewLoginName_M(NewLogin_Name_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_NewLoginNameM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_NewLoginName;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_NewLoginName.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(NewLogin_Name_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NewLoginName', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.NewLoginName_P(NewLogin_Name_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_NewLoginNameP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_NewLoginName;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_NewLoginName.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(NewLogin_Name_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NewLoginName', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.NewAlias(NewAlias_Name_: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(NewAlias_Name_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('NewAlias', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.GetAlias_C(User_Name_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_GetAliasC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_GetAlias;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_GetAlias.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(User_Name_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetAlias', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.GetAlias_M(User_Name_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_GetAliasM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_GetAlias;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_GetAlias.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(User_Name_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetAlias', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.GetAlias_P(User_Name_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_GetAliasP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_GetAlias;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_GetAlias.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(User_Name_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetAlias', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Msg(ToUserName_, msg_: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(ToUserName_);
  d.WriteString(msg_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Msg', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.RequestFriend(ToUserName_, msg_: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(ToUserName_);
  d.WriteString(msg_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RequestFriend', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.ReponseFriend(ToUserName_, msg_: U_String; Accept_: Boolean);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(ToUserName_);
  d.WriteString(msg_);
  d.WriteBool(Accept_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('ReponseFriend', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.RemoveFriend(ToUserName_: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(ToUserName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveFriend', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.GetMyFriends_C(OnResult: TC40_NetDisk_VM_Client_On_Usr_GetMyFriendsC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_GetMyFriends;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_GetMyFriends.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetMyFriends', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.GetMyFriends_M(OnResult: TC40_NetDisk_VM_Client_On_Usr_GetMyFriendsM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_GetMyFriends;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_GetMyFriends.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetMyFriends', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.GetMyFriends_P(OnResult: TC40_NetDisk_VM_Client_On_Usr_GetMyFriendsP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_GetMyFriends;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_GetMyFriends.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetMyFriends', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.GetOnlineNum_C(OnResult: TC40_NetDisk_VM_Client_On_Usr_GetOnlineNumC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_GetOnlineNum;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_GetOnlineNum.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetOnlineNum', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.GetOnlineNum_M(OnResult: TC40_NetDisk_VM_Client_On_Usr_GetOnlineNumM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_GetOnlineNum;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_GetOnlineNum.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetOnlineNum', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.GetOnlineNum_P(OnResult: TC40_NetDisk_VM_Client_On_Usr_GetOnlineNumP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_GetOnlineNum;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_GetOnlineNum.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetOnlineNum', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.GetOnlineList_C(Max_Num: Integer; OnResult: TC40_NetDisk_VM_Client_On_Usr_GetOnlineListC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_GetOnlineList;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_GetOnlineList.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteInteger(Max_Num);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetOnlineList', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.GetOnlineList_M(Max_Num: Integer; OnResult: TC40_NetDisk_VM_Client_On_Usr_GetOnlineListM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_GetOnlineList;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_GetOnlineList.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteInteger(Max_Num);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetOnlineList', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.GetOnlineList_P(Max_Num: Integer; OnResult: TC40_NetDisk_VM_Client_On_Usr_GetOnlineListP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_GetOnlineList;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_GetOnlineList.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteInteger(Max_Num);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetOnlineList', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_FS_Service_C(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_FS_ServiceC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_FS_Service;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_FS_Service.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_FS_Service', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_FS_Service_M(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_FS_ServiceM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_FS_Service;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_FS_Service.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_FS_Service', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_FS_Service_P(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_FS_ServiceP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_FS_Service;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_FS_Service.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_FS_Service', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.SearchMultiMD5_FS_Service_C(md5_arry: TArrayMD5; OnResult: TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_ServiceC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_Service;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_Service.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  for i := low(md5_arry) to high(md5_arry) do
      d.WriteMD5(md5_arry[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('SearchMultiMD5_FS_Service', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.SearchMultiMD5_FS_Service_M(md5_arry: TArrayMD5; OnResult: TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_ServiceM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_Service;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_Service.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  for i := low(md5_arry) to high(md5_arry) do
      d.WriteMD5(md5_arry[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('SearchMultiMD5_FS_Service', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.SearchMultiMD5_FS_Service_P(md5_arry: TArrayMD5; OnResult: TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_ServiceP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_Service;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_SearchMultiMD5_FS_Service.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  for i := low(md5_arry) to high(md5_arry) do
      d.WriteMD5(md5_arry[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('SearchMultiMD5_FS_Service', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.CheckAndCopy_NetDisk_File_C(file_MD5: TMD5; file_Name: U_String; file_time: Double; file_Size: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_FileC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteMD5(file_MD5);
  d.WriteString(file_Name);
  d.WriteDouble(file_time);
  d.WriteInt64(file_Size);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('CheckAndCopy_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.CheckAndCopy_NetDisk_File_M(file_MD5: TMD5; file_Name: U_String; file_time: Double; file_Size: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_FileM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteMD5(file_MD5);
  d.WriteString(file_Name);
  d.WriteDouble(file_time);
  d.WriteInt64(file_Size);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('CheckAndCopy_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.CheckAndCopy_NetDisk_File_P(file_MD5: TMD5; file_Name: U_String; file_time: Double; file_Size: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_FileP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteMD5(file_MD5);
  d.WriteString(file_Name);
  d.WriteDouble(file_time);
  d.WriteInt64(file_Size);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('CheckAndCopy_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.BeginPost_NetDisk_File_C(alias_or_hash_: U_String; file_MD5: TMD5; file_Name: U_String; file_time: Double; file_Size: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_FileC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(alias_or_hash_);
  d.WriteMD5(file_MD5);
  d.WriteString(file_Name);
  d.WriteDouble(file_time);
  d.WriteInt64(file_Size);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('BeginPost_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.BeginPost_NetDisk_File_M(alias_or_hash_: U_String; file_MD5: TMD5; file_Name: U_String; file_time: Double; file_Size: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_FileM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(alias_or_hash_);
  d.WriteMD5(file_MD5);
  d.WriteString(file_Name);
  d.WriteDouble(file_time);
  d.WriteInt64(file_Size);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('BeginPost_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.BeginPost_NetDisk_File_P(alias_or_hash_: U_String; file_MD5: TMD5; file_Name: U_String; file_time: Double; file_Size: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_FileP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_BeginPost_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(alias_or_hash_);
  d.WriteMD5(file_MD5);
  d.WriteString(file_Name);
  d.WriteDouble(file_time);
  d.WriteInt64(file_Size);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('BeginPost_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.CheckAndCopy_NetDisk_File_Frag_C(alias_or_hash_: U_String; frag_md5_: TMD5; frag_pos_, frag_size_: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_FragC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_Frag;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_Frag.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(alias_or_hash_);
  d.WriteMD5(frag_md5_);
  d.WriteInt64(frag_pos_);
  d.WriteInt64(frag_size_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('CheckAndCopy_NetDisk_File_Frag', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.CheckAndCopy_NetDisk_File_Frag_M(alias_or_hash_: U_String; frag_md5_: TMD5; frag_pos_, frag_size_: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_FragM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_Frag;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_Frag.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(alias_or_hash_);
  d.WriteMD5(frag_md5_);
  d.WriteInt64(frag_pos_);
  d.WriteInt64(frag_size_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('CheckAndCopy_NetDisk_File_Frag', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.CheckAndCopy_NetDisk_File_Frag_P(alias_or_hash_: U_String; frag_md5_: TMD5; frag_pos_, frag_size_: Int64; OnResult: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_FragP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_Frag;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_CheckAndCopy_NetDisk_File_Frag.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(alias_or_hash_);
  d.WriteMD5(frag_md5_);
  d.WriteInt64(frag_pos_);
  d.WriteInt64(frag_size_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('CheckAndCopy_NetDisk_File_Frag', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Post_NetDisk_File_Frag(Pos_: Int64; Event_, buff: Pointer; buff_size: Int64);
var
  tmp: TMem64;
begin
  tmp := TMem64.Create;
  tmp.Size := buff_size + 16;
  tmp.Position := 0;
  tmp.WriteInt64(Pos_);
  tmp.WriteUInt64(UInt64(Event_));
  tmp.WritePtr(buff, buff_size);
  DTNoAuthClient.SendTunnel.SendCompleteBuffer('Post_NetDisk_File_Frag', tmp, True);
end;

procedure TC40_NetDisk_VM_Client.EndPost_NetDisk_File_C(OnResult: TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_FileC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('EndPost_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.EndPost_NetDisk_File_M(OnResult: TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_FileM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('EndPost_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.EndPost_NetDisk_File_P(OnResult: TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_FileP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_EndPost_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('EndPost_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_NetDisk_File_Frag_Info_C(DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_InfoC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_Info;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_Info.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_NetDisk_File_Frag_Info', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_NetDisk_File_Frag_Info_M(DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_InfoM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_Info;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_Info.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_NetDisk_File_Frag_Info', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_NetDisk_File_Frag_Info_P(DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_InfoP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_Info;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_Info.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_NetDisk_File_Frag_Info', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_NetDisk_File_Frag_MD5_C(alias_or_hash_, FS_File: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5C);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(alias_or_hash_);
  d.WriteString(FS_File);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_NetDisk_File_Frag_MD5', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_NetDisk_File_Frag_MD5_M(alias_or_hash_, FS_File: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5M);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(alias_or_hash_);
  d.WriteString(FS_File);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_NetDisk_File_Frag_MD5', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_NetDisk_File_Frag_MD5_P(alias_or_hash_, FS_File: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5P);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_Frag_MD5.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(alias_or_hash_);
  d.WriteString(FS_File);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_NetDisk_File_Frag_MD5', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_NetDisk_File_Frag(alias_or_hash_, FS_File: U_String; Pos_: Int64; Event_: Pointer);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(alias_or_hash_);
  d.WriteString(FS_File);
  d.WriteInt64(Pos_);
  d.WritePointer(Event_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Get_NetDisk_File_Frag', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_NetDisk_File_List_C(DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_ListC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_NetDisk_File_List', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_NetDisk_File_List_M(DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_ListM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_NetDisk_File_List', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_NetDisk_File_List_P(DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_ListP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_File_List.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_NetDisk_File_List', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_NetDisk_SpaceInfo_C(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfoC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfo;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfo.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_NetDisk_SpaceInfo', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_NetDisk_SpaceInfo_M(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfoM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfo;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfo.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_NetDisk_SpaceInfo', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_NetDisk_SpaceInfo_P(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfoP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfo;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_NetDisk_SpaceInfo.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_NetDisk_SpaceInfo', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Remove_Item(DB_Field, DB_Remove_Item_: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(DB_Field);
  d.WriteString(DB_Remove_Item_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Remove_Item', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Remove_Field(DB_Field, DB_Remove_Field_: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(DB_Field);
  d.WriteString(DB_Remove_Field_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Remove_Field', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Copy_Item(arry: TCopyItem_Info_Array);
var
  d: TDFE;
  i: Integer;
begin
  d := TDFE.Create;
  for i := low(arry) to high(arry) do
    begin
      d.WriteString(arry[i].Sour_DB_Name);
      d.WriteString(arry[i].Sour_DB_Field);
      d.WriteString(arry[i].Sour_DB_Item);
      d.WriteString(arry[i].Dest_DB_Name);
      d.WriteString(arry[i].Dest_DB_Field);
    end;

  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Copy_Item', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Copy_Field(arry: TCopyField_Info_Array);
var
  d: TDFE;
  i: Integer;
begin
  d := TDFE.Create;
  for i := low(arry) to high(arry) do
    begin
      d.WriteString(arry[i].Sour_DB_Name);
      d.WriteString(arry[i].Sour_DB_Field);
      d.WriteString(arry[i].Dest_DB_Name);
      d.WriteString(arry[i].Dest_DB_Field);
    end;

  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Copy_Field', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.CreateField(DB_Field: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('CreateField', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.RenameField(DB_Field, New_Field_Name: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(DB_Field);
  d.WriteString(New_Field_Name);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RenameField', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.RenameItem(DB_Field, Old_Item_Name, New_Item_Name: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(DB_Field);
  d.WriteString(Old_Item_Name);
  d.WriteString(New_Item_Name);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RenameItem', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Build_Share_Disk_C(OnResult: TC40_NetDisk_VM_Client_On_Usr_Build_Share_DiskC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Build_Share_Disk;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Build_Share_Disk.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Build_Share_Disk', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Build_Share_Disk_M(OnResult: TC40_NetDisk_VM_Client_On_Usr_Build_Share_DiskM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Build_Share_Disk;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Build_Share_Disk.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Build_Share_Disk', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Build_Share_Disk_P(OnResult: TC40_NetDisk_VM_Client_On_Usr_Build_Share_DiskP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Build_Share_Disk;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Build_Share_Disk.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Build_Share_Disk', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_Share_Disk_C(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_DiskC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_Share_Disk', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_Share_Disk_M(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_DiskM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_Share_Disk', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_Share_Disk_P(OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_DiskP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_Share_Disk', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Remove_Share_Disk(Share_Directory_DB_Name: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(Share_Directory_DB_Name);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Remove_Share_Disk', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_Share_Disk_File_List_C(Share_Directory_DB_Name, DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_ListC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_List;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_List.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(Share_Directory_DB_Name);
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_Share_Disk_File_List', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_Share_Disk_File_List_M(Share_Directory_DB_Name, DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_ListM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_List;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_List.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(Share_Directory_DB_Name);
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_Share_Disk_File_List', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_Share_Disk_File_List_P(Share_Directory_DB_Name, DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_ListP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_List;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_List.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(Share_Directory_DB_Name);
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_Share_Disk_File_List', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_Share_Disk_File_Frag_Info_C(Share_Directory_DB_Name, DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_InfoC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_Info;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_Info.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(Share_Directory_DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_Share_Disk_File_Frag_Info', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_Share_Disk_File_Frag_Info_M(Share_Directory_DB_Name, DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_InfoM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_Info;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_Info.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(Share_Directory_DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_Share_Disk_File_Frag_Info', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Get_Share_Disk_File_Frag_Info_P(Share_Directory_DB_Name, DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_InfoP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_Info;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Get_Share_Disk_File_Frag_Info.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(Share_Directory_DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_Share_Disk_File_Frag_Info', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Search_NetDisk_File_C(DB_Field, DB_Search: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Field);
  d.WriteString(DB_Search);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Search_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Search_NetDisk_File_M(DB_Field, DB_Search: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Field);
  d.WriteString(DB_Search);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Search_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Search_NetDisk_File_P(DB_Field, DB_Search: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Field);
  d.WriteString(DB_Search);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Search_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Search_Share_NetDisk_File_C(Share_Directory_DB_Name, DB_Field, DB_Search: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(Share_Directory_DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Search);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Search_Share_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Search_Share_NetDisk_File_M(Share_Directory_DB_Name, DB_Field, DB_Search: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(Share_Directory_DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Search);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Search_Share_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Search_Share_NetDisk_File_P(Share_Directory_DB_Name, DB_Field, DB_Search: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FileP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(Share_Directory_DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Search);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Search_Share_NetDisk_File', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Search_NetDisk_Field_C(DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Search_NetDisk_Field', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Search_NetDisk_Field_M(DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Search_NetDisk_Field', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Search_NetDisk_Field_P(DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Search_NetDisk_Field', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Search_Share_NetDisk_Field_C(Share_Directory_DB_Name, DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(Share_Directory_DB_Name);
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Search_Share_NetDisk_Field', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Search_Share_NetDisk_Field_M(Share_Directory_DB_Name, DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(Share_Directory_DB_Name);
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Search_Share_NetDisk_Field', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Search_Share_NetDisk_Field_P(Share_Directory_DB_Name, DB_Field: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_FieldP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_Field.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(Share_Directory_DB_Name);
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Search_Share_NetDisk_Field', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Auth_AdminC(userName_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_AuthC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auth;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auth.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  d.WriteString(userName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Auth_Admin', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Auth_AdminM(userName_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_AuthM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auth;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auth.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  d.WriteString(userName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Auth_Admin', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Auth_AdminP(userName_: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_AuthP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auth;
  d: TDFE;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auth.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  d.WriteString(userName_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('Auth_Admin', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Close_Auth_Admin(userName_: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(userName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Close_Auth_Admin', d);
  DisposeObject(d);
end;

procedure TC40_NetDisk_VM_Client.Auto_Post_File_C(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; FileTime_: TDateTime; DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_FileC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Create;
  tmp.Client := self;
  tmp.Chunk_Size := FFile_Chunk_Size;
  tmp.OnResultC := OnResult;
  tmp.DB_Field := DB_Field;
  tmp.DB_Item := DB_Item;
  tmp.stream := stream;
  tmp.Done_Free_Stream := Done_Free_Stream;
  tmp.FileTime_ := FileTime_;
  tmp.UserData := UserData;
  tmp.Compute_Stream_MD5;
end;

procedure TC40_NetDisk_VM_Client.Auto_Post_File_M(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; FileTime_: TDateTime; DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_FileM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Create;
  tmp.Client := self;
  tmp.Chunk_Size := FFile_Chunk_Size;
  tmp.OnResultM := OnResult;
  tmp.DB_Field := DB_Field;
  tmp.DB_Item := DB_Item;
  tmp.stream := stream;
  tmp.Done_Free_Stream := Done_Free_Stream;
  tmp.FileTime_ := FileTime_;
  tmp.UserData := UserData;
  tmp.Compute_Stream_MD5;
end;

procedure TC40_NetDisk_VM_Client.Auto_Post_File_P(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; FileTime_: TDateTime; DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_FileP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auto_Post_File.Create;
  tmp.Client := self;
  tmp.Chunk_Size := FFile_Chunk_Size;
  tmp.OnResultP := OnResult;
  tmp.DB_Field := DB_Field;
  tmp.DB_Item := DB_Item;
  tmp.stream := stream;
  tmp.Done_Free_Stream := Done_Free_Stream;
  tmp.FileTime_ := FileTime_;
  tmp.UserData := UserData;
  tmp.Compute_Stream_MD5;
end;

procedure TC40_NetDisk_VM_Client.Auto_Get_File_C(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  tmp.stream := stream;
  tmp.Done_Free_Stream := Done_Free_Stream;
  tmp.UserData := UserData;
  Get_NetDisk_File_Frag_Info_M(DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Usr_Get_NetDisk_File_Frag_Info);
end;

procedure TC40_NetDisk_VM_Client.Auto_Get_File_M(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  tmp.stream := stream;
  tmp.Done_Free_Stream := Done_Free_Stream;
  tmp.UserData := UserData;
  Get_NetDisk_File_Frag_Info_M(DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Usr_Get_NetDisk_File_Frag_Info);
end;

procedure TC40_NetDisk_VM_Client.Auto_Get_File_P(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  tmp.stream := stream;
  tmp.Done_Free_Stream := Done_Free_Stream;
  tmp.UserData := UserData;
  Get_NetDisk_File_Frag_Info_M(DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Usr_Get_NetDisk_File_Frag_Info);
end;

procedure TC40_NetDisk_VM_Client.Auto_Get_File_From_Share_Disk_C(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; Share_Directory_DB_Name, DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileC);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  tmp.stream := stream;
  tmp.Done_Free_Stream := Done_Free_Stream;
  tmp.UserData := UserData;
  Get_Share_Disk_File_Frag_Info_M(Share_Directory_DB_Name, DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Usr_Get_NetDisk_File_Frag_Info);
end;

procedure TC40_NetDisk_VM_Client.Auto_Get_File_From_Share_Disk_M(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; Share_Directory_DB_Name, DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileM);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  tmp.stream := stream;
  tmp.Done_Free_Stream := Done_Free_Stream;
  tmp.UserData := UserData;
  Get_Share_Disk_File_Frag_Info_M(Share_Directory_DB_Name, DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Usr_Get_NetDisk_File_Frag_Info);
end;

procedure TC40_NetDisk_VM_Client.Auto_Get_File_From_Share_Disk_P(UserData: TCore_Object; stream: TCore_Stream; Done_Free_Stream: Boolean; Share_Directory_DB_Name, DB_Field, DB_Item: U_String; OnResult: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_FileP);
var
  tmp: TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File;
begin
  tmp := TC40_NetDisk_VM_Client_On_Usr_Auto_Get_File.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  tmp.stream := stream;
  tmp.Done_Free_Stream := Done_Free_Stream;
  tmp.UserData := UserData;
  Get_Share_Disk_File_Frag_Info_M(Share_Directory_DB_Name, DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Usr_Get_NetDisk_File_Frag_Info);
end;

end.
