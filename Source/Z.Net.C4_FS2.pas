{ ****************************************************************************** }
{ * cloud 4.0 File System 2.0                                                  * }
{ ****************************************************************************** }
unit Z.Net.C4_FS2;

{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ELSE FPC}
  System.IOUtils,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine,
  Z.Geometry2D, Z.DFE, Z.Json, Z.Expression, Z.OpCode,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.ZDB2, Z.ZDB2.MS64, Z.HashList.Templet,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth, Z.Net.C4;

type
  TC40_FS2_Service = class;
  TC40_FS2_Service_File_Data = class;

  TC40_FS2_Service_ZDB2_MS64 = class(TZDB2_MS64)
  public
    Service_File_Data: TC40_FS2_Service_File_Data;
    constructor Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer); override;
  end;

  TC40_FS2_Service_File_Data = class
  public
    Owner: TC40_FS2_Service;
    Stream: TC40_FS2_Service_ZDB2_MS64;
    FileName: U_String;
    FileTime: TDateTime;
    FileRef: Integer;
    FileMD5: TMD5;
    FileSize: Int64;
    constructor Create(Owner_: TC40_FS2_Service; Stream_: TZDB2_MS64);
    destructor Destroy; override;
  end;

  TC40_FS2_Service_File_Data_Pool = {$IFDEF FPC}specialize {$ENDIF FPC}TGeneric_String_Object_Hash<TC40_FS2_Service_File_Data>;
  TC40_FS2_Service_MD5_Data_Pool = {$IFDEF FPC}specialize {$ENDIF FPC}TGeneric_String_Object_Hash<TPascalStringList>;

  TC40_FS2_Service = class(TC40_Base_NoAuth_Service)
  protected
    procedure DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TService_RecvTunnel_UserDefine_NoAuth); override;
    // command
    procedure cmd_FS2_CheckMD5AndFastCopy(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_FS2_PostFile(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure cmd_FS2_GetFile(Sender: TPeerIO; InData: TDFE);
    procedure cmd_FS2_GetFileMD5(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_FS2_SearchMultiMD5(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_FS2_RemoveFile(Sender: TPeerIO; InData: TDFE);
    procedure cmd_FS2_UpdateFileTime(Sender: TPeerIO; InData: TDFE);
    procedure cmd_FS2_UpdateFileRef(Sender: TPeerIO; InData: TDFE);
    procedure cmd_FS2_IncFileRef(Sender: TPeerIO; InData: TDFE);
    procedure cmd_FS2_GetMD5Files(Sender: TPeerIO; InData, OutData: TDFE);
    // admin
    procedure cmd_FS2_Size(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_FS2_Search(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_FS2_PoolFrag(Sender: TPeerIO; InData, OutData: TDFE);
  protected
    // console command
    procedure CC_Compress_And_Reload(var OP_Param: TOpParam);
  private
    FileDatabaseIsUpdate: Boolean;
    FLast_FS_Update_TimeTick: TTimeTick;
  public
    // ZDB2 Core
    ZDB2RecycleMemoryTimeOut: TTimeTick;
    ZDB2DeltaSpace: Int64;
    ZDB2BlockSize: Word;
    ZDB2EnabledCipher: Boolean;
    ZDB2CipherName: U_String;
    ZDB2Password: U_String;
    ZDB2Cipher: TZDB2_Cipher;
    C40_FS2_FileName: U_String;
    FileHashPool: TC40_FS2_Service_File_Data_Pool;
    MD5Pool: TC40_FS2_Service_MD5_Data_Pool;
    FileDatabase: TZDB2_List_MS64;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
    procedure Update_All_Client_FS_State;
  end;

  TC40_FS2_Client = class;

  TFS2_Client_CacheData = class
    Owner: TC40_FS2_Client;
    Stream: TZDB2_MS64;
    LastAccess: TTimeTick;
    constructor Create(Owner_: TC40_FS2_Client);
    destructor Destroy; override;
  end;

  TFS2_Client_CacheHashPool = {$IFDEF FPC}specialize {$ENDIF FPC}TGeneric_String_Object_Hash<TFS2_Client_CacheData>;

{$REGION 'bridge_define'}
  TP2PVM_Recycle_Pool = {$IFDEF FPC}specialize {$ENDIF FPC}TOrderStruct<TZNet_WithP2PVM_Client>;

  TC40_FS2_Client_CheckMD5AndFastCopyC = procedure(Sender: TC40_FS2_Client; State_: Boolean);
  TC40_FS2_Client_CheckMD5AndFastCopyM = procedure(Sender: TC40_FS2_Client; State_: Boolean) of object;
{$IFDEF FPC}
  TC40_FS2_Client_CheckMD5AndFastCopyP = procedure(Sender: TC40_FS2_Client; State_: Boolean) is nested;
{$ELSE FPC}
  TC40_FS2_Client_CheckMD5AndFastCopyP = reference to procedure(Sender: TC40_FS2_Client; State_: Boolean);
{$ENDIF FPC}

  TC40_FS2_Client_CheckMD5AndFastCopy = class(TOnResultBridge)
  public
    Client: TC40_FS2_Client;
    OnResultC: TC40_FS2_Client_CheckMD5AndFastCopyC;
    OnResultM: TC40_FS2_Client_CheckMD5AndFastCopyM;
    OnResultP: TC40_FS2_Client_CheckMD5AndFastCopyP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_FS2_Client_PostFile_DoneC = procedure(Sender: TC40_FS2_Client; info_: U_String);
  TC40_FS2_Client_PostFile_DoneM = procedure(Sender: TC40_FS2_Client; info_: U_String) of object;
{$IFDEF FPC}
  TC40_FS2_Client_PostFile_DoneP = procedure(Sender: TC40_FS2_Client; info_: U_String) is nested;
{$ELSE FPC}
  TC40_FS2_Client_PostFile_DoneP = reference to procedure(Sender: TC40_FS2_Client; info_: U_String);
{$ENDIF FPC}

  TC40_FS2_Client_Post_File_Tunnel = class
  public
    p2pClient: TZNet_WithP2PVM_Client;
    Client: TC40_FS2_Client;
    File_Name: U_String;
    Stream: TMS64;
    OnResultC: TC40_FS2_Client_PostFile_DoneC;
    OnResultM: TC40_FS2_Client_PostFile_DoneM;
    OnResultP: TC40_FS2_Client_PostFile_DoneP;

    constructor Create;
    destructor Destroy; override;
    procedure DoP2PVM_CloneConnectAndPostFile(Sender: TZNet_WithP2PVM_Client);
    procedure cmd_PostDone(Sender: TPeerIO; InData: SystemString);
  end;

  TC40_FS2_Client_Post_File_Cache = class
  public
    Client: TC40_FS2_Client;
    File_Name: U_String;
    Stream: TCore_Stream;
    doneFree: Boolean;
    OnResultC: TC40_FS2_Client_PostFile_DoneC;
    OnResultM: TC40_FS2_Client_PostFile_DoneM;
    OnResultP: TC40_FS2_Client_PostFile_DoneP;
    constructor Create;
    procedure Do_CheckMD5AndFastCopy(Sender: TC40_FS2_Client; State_: Boolean);
  end;

  TC40_FS2_Client_GetFile_DoneC = procedure(Sender: TC40_FS2_Client; Stream: TMS64; info_: U_String; Successed: Boolean);
  TC40_FS2_Client_GetFile_DoneM = procedure(Sender: TC40_FS2_Client; Stream: TMS64; info_: U_String; Successed: Boolean) of object;
{$IFDEF FPC}
  TC40_FS2_Client_GetFile_DoneP = procedure(Sender: TC40_FS2_Client; Stream: TMS64; info_: U_String; Successed: Boolean) is nested;
{$ELSE FPC}
  TC40_FS2_Client_GetFile_DoneP = reference to procedure(Sender: TC40_FS2_Client; Stream: TMS64; info_: U_String; Successed: Boolean);
{$ENDIF FPC}

  TC40_FS2_Client_Get_File_Tunnel = class
  public
    p2pClient: TZNet_WithP2PVM_Client;
    Client: TC40_FS2_Client;
    File_Name: U_String;
    OnResultC: TC40_FS2_Client_GetFile_DoneC;
    OnResultM: TC40_FS2_Client_GetFile_DoneM;
    OnResultP: TC40_FS2_Client_GetFile_DoneP;

    constructor Create;
    destructor Destroy; override;
    procedure cmd_Save(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure cmd_Error(Sender: TPeerIO; InData: SystemString);
    procedure DoP2PVM_CloneConnectAndGetFile(Sender: TZNet_WithP2PVM_Client);
  end;

  TC40_FS2_Client_GetFileMD5C = procedure(Sender: TC40_FS2_Client; State_: Boolean; info_: SystemString; MD5_: TMD5);
  TC40_FS2_Client_GetFileMD5M = procedure(Sender: TC40_FS2_Client; State_: Boolean; info_: SystemString; MD5_: TMD5) of object;
{$IFDEF FPC}
  TC40_FS2_Client_GetFileMD5P = procedure(Sender: TC40_FS2_Client; State_: Boolean; info_: SystemString; MD5_: TMD5) is nested;
{$ELSE FPC}
  TC40_FS2_Client_GetFileMD5P = reference to procedure(Sender: TC40_FS2_Client; State_: Boolean; info_: SystemString; MD5_: TMD5);
{$ENDIF FPC}

  TC40_FS2_Client_GetFileMD5 = class(TOnResultBridge)
  public
    Client: TC40_FS2_Client;
    OnResultC: TC40_FS2_Client_GetFileMD5C;
    OnResultM: TC40_FS2_Client_GetFileMD5M;
    OnResultP: TC40_FS2_Client_GetFileMD5P;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TFS2_SearchMultiMD5_State = record
    MD5: TMD5;
    IsFound: Boolean;
  end;

  TFS2_SearchMultiMD5_State_Array = array of TFS2_SearchMultiMD5_State;

  TC40_FS2_Client_SearchMultiMD5C = procedure(Sender: TC40_FS2_Client; arry: TFS2_SearchMultiMD5_State_Array);
  TC40_FS2_Client_SearchMultiMD5M = procedure(Sender: TC40_FS2_Client; arry: TFS2_SearchMultiMD5_State_Array) of object;
{$IFDEF FPC}
  TC40_FS2_Client_SearchMultiMD5P = procedure(Sender: TC40_FS2_Client; arry: TFS2_SearchMultiMD5_State_Array) is nested;
{$ELSE FPC}
  TC40_FS2_Client_SearchMultiMD5P = reference to procedure(Sender: TC40_FS2_Client; arry: TFS2_SearchMultiMD5_State_Array);
{$ENDIF FPC}

  TC40_FS2_Client_SearchMultiMD5 = class(TOnResultBridge)
  public
    Client: TC40_FS2_Client;
    OnResultC: TC40_FS2_Client_SearchMultiMD5C;
    OnResultM: TC40_FS2_Client_SearchMultiMD5M;
    OnResultP: TC40_FS2_Client_SearchMultiMD5P;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_FS2_Client_GetMD5FilesC = procedure(Sender: TC40_FS2_Client; Files_: U_StringArray);
  TC40_FS2_Client_GetMD5FilesM = procedure(Sender: TC40_FS2_Client; Files_: U_StringArray) of object;
{$IFDEF FPC}
  TC40_FS2_Client_GetMD5FilesP = procedure(Sender: TC40_FS2_Client; Files_: U_StringArray) is nested;
{$ELSE FPC}
  TC40_FS2_Client_GetMD5FilesP = reference to procedure(Sender: TC40_FS2_Client; Files_: U_StringArray);
{$ENDIF FPC}

  TC40_FS2_Client_GetMD5Files = class(TOnResultBridge)
  public
    Client: TC40_FS2_Client;
    OnResultC: TC40_FS2_Client_GetMD5FilesC;
    OnResultM: TC40_FS2_Client_GetMD5FilesM;
    OnResultP: TC40_FS2_Client_GetMD5FilesP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_FS2_Client_GetFileMD5_Cache = class
  public
    Client: TC40_FS2_Client;
    File_Name: U_String;
    OnResultC: TC40_FS2_Client_GetFile_DoneC;
    OnResultM: TC40_FS2_Client_GetFile_DoneM;
    OnResultP: TC40_FS2_Client_GetFile_DoneP;

    constructor Create;
    destructor Destroy; override;
    procedure Do_FS2_GetFileMD5(Sender: TC40_FS2_Client; State_: Boolean; info_: SystemString; MD5_: TMD5);
  end;

  TFS2_FileSizeInfo = record
    FileName: SystemString;
    Size: Int64;
  end;

  TFS2_FileSizeInfo_Array = array of TFS2_FileSizeInfo;

  TC40_FS2_Client_SizeC = procedure(Sender: TC40_FS2_Client; arry: TFS2_FileSizeInfo_Array);
  TC40_FS2_Client_SizeM = procedure(Sender: TC40_FS2_Client; arry: TFS2_FileSizeInfo_Array) of object;
{$IFDEF FPC}
  TC40_FS2_Client_SizeP = procedure(Sender: TC40_FS2_Client; arry: TFS2_FileSizeInfo_Array) is nested;
{$ELSE FPC}
  TC40_FS2_Client_SizeP = reference to procedure(Sender: TC40_FS2_Client; arry: TFS2_FileSizeInfo_Array);
{$ENDIF FPC}

  TC40_FS2_Client_Size = class(TOnResultBridge)
  public
    Client: TC40_FS2_Client;
    OnResultC: TC40_FS2_Client_SizeC;
    OnResultM: TC40_FS2_Client_SizeM;
    OnResultP: TC40_FS2_Client_SizeP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TFS2_FileInfo = record
    FileName: SystemString;
    FileTime: TDateTime;
    FileRef: Integer;
    Size: Int64;
    MD5: TMD5;
  end;

  TFS2_FileInfo_Array = array of TFS2_FileInfo;

  TC40_FS2_Client_SearchC = procedure(Sender: TC40_FS2_Client; arry: TFS2_FileInfo_Array);
  TC40_FS2_Client_SearchM = procedure(Sender: TC40_FS2_Client; arry: TFS2_FileInfo_Array) of object;
{$IFDEF FPC}
  TC40_FS2_Client_SearchP = procedure(Sender: TC40_FS2_Client; arry: TFS2_FileInfo_Array) is nested;
{$ELSE FPC}
  TC40_FS2_Client_SearchP = reference to procedure(Sender: TC40_FS2_Client; arry: TFS2_FileInfo_Array);
{$ENDIF FPC}

  TC40_FS2_Client_Search = class(TOnResultBridge)
  public
    Client: TC40_FS2_Client;
    OnResultC: TC40_FS2_Client_SearchC;
    OnResultM: TC40_FS2_Client_SearchM;
    OnResultP: TC40_FS2_Client_SearchP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TFS2_PoolFragInfo = record
    FileName: SystemString;
    MD5: TMD5;
    FileTime: TDateTime;
  end;

  TFS2_PoolFragInfo_Array = array of TFS2_PoolFragInfo;

  TC40_FS2_Client_PoolFragC = procedure(Sender: TC40_FS2_Client; arry: TFS2_PoolFragInfo_Array);
  TC40_FS2_Client_PoolFragM = procedure(Sender: TC40_FS2_Client; arry: TFS2_PoolFragInfo_Array) of object;
{$IFDEF FPC}
  TC40_FS2_Client_PoolFragP = procedure(Sender: TC40_FS2_Client; arry: TFS2_PoolFragInfo_Array) is nested;
{$ELSE FPC}
  TC40_FS2_Client_PoolFragP = reference to procedure(Sender: TC40_FS2_Client; arry: TFS2_PoolFragInfo_Array);
{$ENDIF FPC}

  TC40_FS2_Client_PoolFrag = class(TOnResultBridge)
  public
    Client: TC40_FS2_Client;
    OnResultC: TC40_FS2_Client_PoolFragC;
    OnResultM: TC40_FS2_Client_PoolFragM;
    OnResultP: TC40_FS2_Client_PoolFragP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

{$ENDREGION 'bridge_define'}

  TC40_FS2_Client = class(TC40_Base_NoAuth_Client)
  protected
    FMaxFileSize: Cardinal;
    FRemote_FS_DB_Size, FRemote_FS_Num: Int64;
    FRemoveCacheList: TPascalStringList;
    procedure Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client); override;
    procedure Do_Progress_FileCachePool(const Name_: PSystemString; Obj_: TFS2_Client_CacheData);
    procedure cmd_FS_State(Sender: TPeerIO; InData: TDFE);
  public
    C40_FS2_Cache_FileName: U_String;
    ZDB2RecycleMemoryTimeOut: TTimeTick;
    ZDB2DeltaSpace: Int64;
    ZDB2BlockSize: Word;
    ZDB2EnabledCipher: Boolean;
    ZDB2CipherName: U_String;
    ZDB2Password: U_String;
    ZDB2Cipher: TZDB2_Cipher;
    Cache_File_Life: Int64;
    FileCacheHashPool: TFS2_Client_CacheHashPool;
    Cache_Database: TZDB2_List_MS64;
    P2PVM_Recycle_Pool: TP2PVM_Recycle_Pool;
    property MaxFileSize: Cardinal read FMaxFileSize;
    property Remote_FS_DB_Size: Int64 read FRemote_FS_DB_Size;
    property Remote_FS_Num: Int64 read FRemote_FS_Num;

    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
    procedure RemoveCache(File_Name: U_String); overload;
    procedure RemoveCache(arry: U_StringArray); overload;

    // fast copy
    procedure FS2_CheckMD5AndFastCopyC(File_Name: U_String; Stream_MD5_: TMD5; Stream_Size_: Int64; OnResult: TC40_FS2_Client_CheckMD5AndFastCopyC); overload;
    procedure FS2_CheckMD5AndFastCopyM(File_Name: U_String; Stream_MD5_: TMD5; Stream_Size_: Int64; OnResult: TC40_FS2_Client_CheckMD5AndFastCopyM); overload;
    procedure FS2_CheckMD5AndFastCopyP(File_Name: U_String; Stream_MD5_: TMD5; Stream_Size_: Int64; OnResult: TC40_FS2_Client_CheckMD5AndFastCopyP); overload;
    procedure FS2_CheckMD5AndFastCopyC(File_Name: U_String; Stream: TCore_Stream; OnResult: TC40_FS2_Client_CheckMD5AndFastCopyC); overload;
    procedure FS2_CheckMD5AndFastCopyM(File_Name: U_String; Stream: TCore_Stream; OnResult: TC40_FS2_Client_CheckMD5AndFastCopyM); overload;
    procedure FS2_CheckMD5AndFastCopyP(File_Name: U_String; Stream: TCore_Stream; OnResult: TC40_FS2_Client_CheckMD5AndFastCopyP); overload;
    // upload
    procedure FS2_PostFile(UsedCache_: Boolean; File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean);
    procedure FS2_PostFile_C(UsedCache_: Boolean; File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean; OnResult: TC40_FS2_Client_PostFile_DoneC);
    procedure FS2_PostFile_M(UsedCache_: Boolean; File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean; OnResult: TC40_FS2_Client_PostFile_DoneM);
    procedure FS2_PostFile_P(UsedCache_: Boolean; File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean; OnResult: TC40_FS2_Client_PostFile_DoneP);
    // download
    procedure FS2_GetFile_C(UsedCache_: Boolean; File_Name: U_String; OnResult: TC40_FS2_Client_GetFile_DoneC);
    procedure FS2_GetFile_M(UsedCache_: Boolean; File_Name: U_String; OnResult: TC40_FS2_Client_GetFile_DoneM);
    procedure FS2_GetFile_P(UsedCache_: Boolean; File_Name: U_String; OnResult: TC40_FS2_Client_GetFile_DoneP);
    // md5
    procedure FS2_GetFileMD5C(File_Name: U_String; OnResult: TC40_FS2_Client_GetFileMD5C);
    procedure FS2_GetFileMD5M(File_Name: U_String; OnResult: TC40_FS2_Client_GetFileMD5M);
    procedure FS2_GetFileMD5P(File_Name: U_String; OnResult: TC40_FS2_Client_GetFileMD5P);
    // multi md5
    procedure FS2_SearchMultiMD5C(arry: TArrayMD5; OnResult: TC40_FS2_Client_SearchMultiMD5C);
    procedure FS2_SearchMultiMD5M(arry: TArrayMD5; OnResult: TC40_FS2_Client_SearchMultiMD5M);
    procedure FS2_SearchMultiMD5P(arry: TArrayMD5; OnResult: TC40_FS2_Client_SearchMultiMD5P);
    // query md5 as files
    procedure FS2_GetMD5FilesC(MD5_: TMD5; OnResult: TC40_FS2_Client_GetMD5FilesC);
    procedure FS2_GetMD5FilesM(MD5_: TMD5; OnResult: TC40_FS2_Client_GetMD5FilesM);
    procedure FS2_GetMD5FilesP(MD5_: TMD5; OnResult: TC40_FS2_Client_GetMD5FilesP);
    // remove
    procedure FS2_RemoveFile(File_Name: U_String); overload;
    procedure FS2_RemoveFile(arry: U_StringArray); overload;
    // update time
    procedure FS2_UpdateFileTime(File_Name: U_String; nTime: TDateTime);
    // ref
    procedure FS2_UpdateFileRef(File_Name: U_String; ref_: Integer);
    procedure FS2_IncFileRef(File_Name: U_String; inc_ref_: Integer);
    // admin
    procedure FS2_SizeC(FileNames: U_StringArray; OnResult: TC40_FS2_Client_SizeC);
    procedure FS2_SizeM(FileNames: U_StringArray; OnResult: TC40_FS2_Client_SizeM);
    procedure FS2_SizeP(FileNames: U_StringArray; OnResult: TC40_FS2_Client_SizeP);
    procedure FS2_SearchC(filter: U_String; MaxNum: Integer; OnResult: TC40_FS2_Client_SearchC);
    procedure FS2_SearchM(filter: U_String; MaxNum: Integer; OnResult: TC40_FS2_Client_SearchM);
    procedure FS2_SearchP(filter: U_String; MaxNum: Integer; OnResult: TC40_FS2_Client_SearchP);
    procedure FS2_PoolFragC(OnResult: TC40_FS2_Client_PoolFragC);
    procedure FS2_PoolFragM(OnResult: TC40_FS2_Client_PoolFragM);
    procedure FS2_PoolFragP(OnResult: TC40_FS2_Client_PoolFragP);
  end;

  TC40_FS2_Client_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_FS2_Client>;

implementation

constructor TC40_FS2_Service_ZDB2_MS64.Create(CoreSpace_: TZDB2_Core_Space; ID_: Integer);
begin
  inherited Create(CoreSpace_, ID_);
  Service_File_Data := nil;
end;

constructor TC40_FS2_Service_File_Data.Create(Owner_: TC40_FS2_Service; Stream_: TZDB2_MS64);
begin
  inherited Create;
  Owner := Owner_;
  if Stream_ is TC40_FS2_Service_ZDB2_MS64 then
      Stream := Stream_ as TC40_FS2_Service_ZDB2_MS64
  else
      RaiseInfo('error.');
  Stream.Service_File_Data := Self;
  FileName := '';
  FileTime := umlNow;
  FileRef := 0;
  FileMD5 := NullMD5;
  FileSize := 0;
end;

destructor TC40_FS2_Service_File_Data.Destroy;
begin
  FileName := '';
  if (Owner.FileDatabase <> nil) and (Stream <> nil) then
      Owner.FileDatabase.Remove(Stream, True);
  inherited Destroy;
end;

procedure TC40_FS2_Service.DoLinkSuccess_Event(Sender: TDTService_NoAuth; UserDefineIO: TService_RecvTunnel_UserDefine_NoAuth);
var
  tmp_: TDFE;
begin
  inherited DoLinkSuccess_Event(Sender, UserDefineIO);
  tmp_ := TDFE.Create;
  tmp_.WriteInt64(FileDatabase.CoreSpace.State^.Physics);
  tmp_.WriteInt64(FileDatabase.Count);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('FS_State', tmp_);
  disposeObject(tmp_);
end;

procedure TC40_FS2_Service.cmd_FS2_CheckMD5AndFastCopy(Sender: TPeerIO; InData, OutData: TDFE);
var
  fileName_: U_String;
  fileTime_: Double;
  fileRef_: Integer;
  FileMD5_: TMD5;
  fileSize_: Int64;
  FL_: TPascalStringList;
  fd: TC40_FS2_Service_File_Data;
  i: Integer;
  newFD: TC40_FS2_Service_File_Data;
begin
  fileName_ := InData.R.ReadString;
  fileTime_ := InData.R.ReadDouble;
  fileRef_ := InData.R.ReadInteger;
  FileMD5_ := InData.R.ReadMD5;
  fileSize_ := InData.R.ReadInt64;

  FL_ := MD5Pool[umlMD5ToStr(FileMD5_)];
  if FL_ <> nil then
    begin
      // check
      for i := 0 to FL_.Count - 1 do
        if FL_[i].Same(@fileName_) then
          begin
            OutData.WriteBool(True);
            exit;
          end;

      // fast copy
      for i := 0 to FL_.Count - 1 do
        begin
          fd := FileHashPool[FL_[i]];
          if (fd <> nil) and (fd.FileSize = fileSize_) then
            begin
              fd.Stream.Data.Position := 0;
              fd.Stream.Data.ReadString;
              fd.Stream.Data.ReadDouble;
              fd.Stream.Data.ReadInt32;

              newFD := TC40_FS2_Service_File_Data.Create(Self, FileDatabase.NewData);
              newFD.FileName := fileName_;
              newFD.FileTime := fileTime_;
              newFD.FileRef := fileRef_;
              newFD.FileMD5 := FileMD5_;
              newFD.FileSize := fileSize_;
              newFD.Stream.Data.WriteString(fileName_);
              newFD.Stream.Data.WriteDouble(fileTime_);
              newFD.Stream.Data.WriteInt32(fileRef_);
              newFD.Stream.Data.WritePtr(fd.Stream.Data.PosAsPtr, fd.Stream.Data.Size - fd.Stream.Data.Position);
              newFD.Stream.Save;
              FileHashPool.Add(newFD.FileName, newFD);
              OutData.WriteBool(True);
              FileDatabaseIsUpdate := True;
              exit;
            end;
        end;
    end;
  OutData.WriteBool(False);
end;

procedure TC40_FS2_Service.cmd_FS2_PostFile(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  fd: TC40_FS2_Service_File_Data;
  FL_: TPascalStringList;
begin
  fd := TC40_FS2_Service_File_Data.Create(Self, FileDatabase.NewData);
  fd.Stream.Data.WritePtr(InData, DataSize);
  fd.Stream.Data.Position := 0;
  fd.FileName := fd.Stream.Data.ReadString;
  fd.FileTime := fd.Stream.Data.ReadDouble;
  fd.FileRef := fd.Stream.Data.ReadInt32;
  fd.FileMD5 := umlMD5(fd.Stream.Data.PosAsPtr, fd.Stream.Data.Size - fd.Stream.Data.Position);
  fd.FileSize := fd.Stream.Data.Size - fd.Stream.Data.Position;
  fd.Stream.Save;
  FileHashPool.Add(fd.FileName, fd);

  FL_ := MD5Pool[umlMD5ToStr(fd.FileMD5)];
  if FL_ <> nil then
    begin
      if FL_.ExistsValue(fd.FileName) < 0 then
          FL_.Add(fd.FileName);
    end
  else
    begin
      FL_ := TPascalStringList.Create;
      FL_.Add(fd.FileName);
      MD5Pool.Add(umlMD5ToStr(fd.FileMD5), FL_);
    end;

  Sender.SendDirectConsoleCmd('PostDone', fd.FileName);
  FileDatabaseIsUpdate := True;
end;

procedure TC40_FS2_Service.cmd_FS2_GetFile(Sender: TPeerIO; InData: TDFE);
var
  File_Name: U_String;
  IO_ID: Cardinal;
  IO_: TPeerIO;
  fd: TC40_FS2_Service_File_Data;
begin
  File_Name := InData.R.ReadString;
  IO_ID := InData.R.ReadCardinal;
  IO_ := DTNoAuthService.RecvTunnel[IO_ID];
  if IO_ = nil then
      exit;
  fd := FileHashPool[File_Name];
  if fd = nil then
      IO_.SendDirectConsoleCmd('Error', PFormat('no found "%s"', [File_Name.Text]))
  else
    begin
      IO_.SendCompleteBuffer('Save', fd.Stream.Data.Clone, True);
      fd.Stream.Save;
    end;
end;

procedure TC40_FS2_Service.cmd_FS2_GetFileMD5(Sender: TPeerIO; InData, OutData: TDFE);
var
  File_Name: U_String;
  fd: TC40_FS2_Service_File_Data;
begin
  File_Name := InData.R.ReadString;
  fd := FileHashPool[File_Name];
  if fd = nil then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found "%s"', [File_Name.Text]);
      OutData.WriteMD5(NullMD5);
    end
  else
    begin
      OutData.WriteBool(True);
      OutData.WriteString(fd.FileName);
      OutData.WriteMD5(fd.FileMD5);
    end;
end;

procedure TC40_FS2_Service.cmd_FS2_SearchMultiMD5(Sender: TPeerIO; InData, OutData: TDFE);
var
  MD5_: TMD5;
  FL_: TPascalStringList;
begin
  while InData.R.NotEnd do
    begin
      MD5_ := InData.R.ReadMD5;
      FL_ := MD5Pool[umlMD5ToStr(MD5_)];
      OutData.WriteMD5(MD5_);
      OutData.WriteBool((FL_ <> nil) and (FL_.Count > 0));
    end;
end;

procedure TC40_FS2_Service.cmd_FS2_RemoveFile(Sender: TPeerIO; InData: TDFE);
var
  fn: U_String;
  fd: TC40_FS2_Service_File_Data;
  FL_: TPascalStringList;
begin
  while InData.R.NotEnd do
    begin
      fn := InData.R.ReadString;
      fd := FileHashPool[fn];
      if fd <> nil then
        begin
          FL_ := MD5Pool[umlMD5ToStr(fd.FileMD5)];
          if FL_ <> nil then
            begin
              FL_.DeleteString(fn);
              if FL_.Count = 0 then
                  MD5Pool.Delete(umlMD5ToStr(fd.FileMD5));
            end;

          fd.Stream.Remove;
          fd.Stream := nil;
          FileHashPool.Delete(fn);
          FileDatabaseIsUpdate := True;
        end;
    end;
end;

procedure TC40_FS2_Service.cmd_FS2_UpdateFileTime(Sender: TPeerIO; InData: TDFE);
var
  fn: U_String;
  nTime: Double;
  fd: TC40_FS2_Service_File_Data;
  buff: array of byte;
  read_size: Word;
  tmp2: NativeUInt;
begin
  fn := InData.R.ReadString;
  nTime := InData.R.ReadDouble;
  fd := FileHashPool[fn];
  if fd = nil then
      exit;

  fd.FileTime := nTime;

  if fd.Stream.Data_Direct <> nil then
    begin
      // modify cache
      PDouble(GetPtr(fd.Stream.Data_Direct.Memory, PCardinal(fd.Stream.Data_Direct.Memory)^ + 4))^ := fd.FileTime;
    end
  else
    begin
      SetLength(buff, $FFFF);
      read_size := fd.Stream.CoreSpace.Block_IO_Read(@buff[0], fd.Stream.ID);
      if read_size > 0 then
        begin
          tmp2 := PCardinal(@buff[0])^ + 4;
          if tmp2 + 8 <= read_size then
            begin
              // io optimized overwrite
              PDouble(GetPtr(@buff[0], tmp2))^ := fd.FileTime;
              fd.Stream.CoreSpace.Block_IO_Write(@buff[0], fd.Stream.ID);
            end
          else
            begin
              // overwrite
              PDouble(GetPtr(fd.Stream.Data.Memory, PCardinal(fd.Stream.Data.Memory)^ + 4))^ := fd.FileTime;
            end;
        end;
      SetLength(buff, 0);
    end;
end;

procedure TC40_FS2_Service.cmd_FS2_UpdateFileRef(Sender: TPeerIO; InData: TDFE);
var
  fn: U_String;
  ref_: Integer;
  fd: TC40_FS2_Service_File_Data;
  buff: array of byte;
  read_size: Word;
  tmp2: NativeUInt;
begin
  fn := InData.R.ReadString;
  ref_ := InData.R.ReadInteger;
  fd := FileHashPool[fn];
  if fd = nil then
      exit;

  fd.FileRef := ref_;

  if fd.Stream.Data_Direct <> nil then
    begin
      // modify cache
      PInteger(GetPtr(fd.Stream.Data_Direct.Memory, PCardinal(fd.Stream.Data_Direct.Memory)^ + 4 + 8))^ := fd.FileRef;
    end
  else
    begin
      SetLength(buff, $FFFF);
      read_size := fd.Stream.CoreSpace.Block_IO_Read(@buff[0], fd.Stream.ID);
      if read_size > 0 then
        begin
          tmp2 := PCardinal(@buff[0])^ + 4 + 8;
          if tmp2 + 4 <= read_size then
            begin
              // io optimized overwrite
              PInteger(GetPtr(@buff[0], tmp2))^ := fd.FileRef;
              fd.Stream.CoreSpace.Block_IO_Write(@buff[0], fd.Stream.ID);
            end
          else
            begin
              // overwrite
              PInteger(GetPtr(fd.Stream.Data.Memory, PCardinal(fd.Stream.Data.Memory)^ + 4 + 8))^ := fd.FileRef;
            end;
        end;
      SetLength(buff, 0);
    end;
end;

procedure TC40_FS2_Service.cmd_FS2_IncFileRef(Sender: TPeerIO; InData: TDFE);
var
  fn: U_String;
  inc_ref_: Integer;
  fd: TC40_FS2_Service_File_Data;
  buff: array of byte;
  read_size: Word;
  tmp2: NativeUInt;
begin
  fn := InData.R.ReadString;
  inc_ref_ := InData.R.ReadInteger;
  fd := FileHashPool[fn];
  if fd = nil then
      exit;

  fd.FileRef := fd.FileRef + inc_ref_;

  if fd.Stream.Data_Direct <> nil then
    begin
      // modify cache
      PInteger(GetPtr(fd.Stream.Data_Direct.Memory, PCardinal(fd.Stream.Data_Direct.Memory)^ + 4 + 8))^ := fd.FileRef;
    end
  else
    begin
      SetLength(buff, $FFFF);
      read_size := fd.Stream.CoreSpace.Block_IO_Read(@buff[0], fd.Stream.ID);
      if read_size > 0 then
        begin
          tmp2 := PCardinal(@buff[0])^ + 4 + 8;
          if tmp2 + 4 <= read_size then
            begin
              // io optimized overwrite
              PInteger(GetPtr(@buff[0], tmp2))^ := fd.FileRef;
              fd.Stream.CoreSpace.Block_IO_Write(@buff[0], fd.Stream.ID);
            end
          else
            begin
              // overwrite
              PInteger(GetPtr(fd.Stream.Data.Memory, PCardinal(fd.Stream.Data.Memory)^ + 4 + 8))^ := fd.FileRef;
            end;
        end;
      SetLength(buff, 0);
    end;
end;

procedure TC40_FS2_Service.cmd_FS2_GetMD5Files(Sender: TPeerIO; InData, OutData: TDFE);
var
  MD5_: TMD5;
  FL_: TPascalStringList;
  i: Integer;
begin
  MD5_ := InData.R.ReadMD5;
  FL_ := MD5Pool[umlMD5ToStr(MD5_)];
  if FL_ <> nil then
    for i := 0 to FL_.Count - 1 do
        OutData.WriteString(FL_[i]);
end;

procedure TC40_FS2_Service.cmd_FS2_Size(Sender: TPeerIO; InData, OutData: TDFE);
var
  fd: TC40_FS2_Service_File_Data;
begin
  while InData.R.NotEnd do
    begin
      fd := FileHashPool[InData.R.ReadString];
      if fd = nil then
          OutData.WriteInt64(-1)
      else
          OutData.WriteInt64(fd.FileSize);
    end;
end;

procedure TC40_FS2_Service.cmd_FS2_Search(Sender: TPeerIO; InData, OutData: TDFE);
var
  filter_: U_String;
  MaxNum: Integer;
{$IFDEF FPC}
  procedure fpc_progress_(const Name_: PSystemString; Obj_: TC40_FS2_Service_File_Data);
  begin
    if (OutData.Count div 5) > MaxNum then
        exit;
    if umlSearchMatch(filter_, Name_^) then
      begin
        OutData.WriteString(Obj_.FileName);
        OutData.WriteDouble(Obj_.FileTime);
        OutData.WriteInteger(Obj_.FileRef);
        OutData.WriteInt64(Obj_.FileSize);
        OutData.WriteMD5(Obj_.FileMD5);
      end;
  end;
{$ENDIF FPC}


begin
  filter_ := InData.R.ReadString;
  MaxNum := InData.R.ReadInteger;

{$IFDEF FPC}
  FileHashPool.ProgressP(@fpc_progress_);
{$ELSE FPC}
  FileHashPool.ProgressP(procedure(const Name_: PSystemString; Obj_: TC40_FS2_Service_File_Data)
    begin
      if (OutData.Count div 5) > MaxNum then
          exit;
      if umlSearchMatch(filter_, Name_^) then
        begin
          OutData.WriteString(Obj_.FileName);
          OutData.WriteDouble(Obj_.FileTime);
          OutData.WriteInteger(Obj_.FileRef);
          OutData.WriteInt64(Obj_.FileSize);
          OutData.WriteMD5(Obj_.FileMD5);
        end;
    end);
{$ENDIF FPC}

end;

procedure TC40_FS2_Service.cmd_FS2_PoolFrag(Sender: TPeerIO; InData, OutData: TDFE);
var
  fd: TC40_FS2_Service_File_Data;
begin
  if FileDatabase.Count > 0 then
    with FileDatabase.Invert_Repeat_ do
      repeat
        fd := TC40_FS2_Service_ZDB2_MS64(Queue^.Data).Service_File_Data;
        if fd <> nil then
          begin
            OutData.WriteString(fd.FileName);
            OutData.WriteMD5(fd.FileMD5);
            OutData.WriteDouble(fd.FileTime);
          end;
      until not Prev;
end;

procedure TC40_FS2_Service.CC_Compress_And_Reload(var OP_Param: TOpParam);
var
  New_F: U_String;
  FS: TCore_FileStream;
begin
  New_F := Get_New_ZDB2_Extract_FileName(C40_FS2_FileName);
  FS := TCore_FileStream.Create(New_F, fmCreate);
  FileDatabase.ExtractTo(FS);
  disposeObject(FS);
end;

constructor TC40_FS2_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
var
  FS: TCore_Stream;
  fd: TC40_FS2_Service_File_Data;
  FL_: TPascalStringList;
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  // max complete buffer 10M
  DTNoAuthService.RecvTunnel.MaxCompleteBufferSize := EStrToInt64(ParamList.GetDefaultValue('MaxBuffer', '10*1024*1024'), 10 * 1024 * 1024);
  DTNoAuthService.RecvTunnel.CompleteBufferCompressed := False;

  DTNoAuthService.RecvTunnel.RegisterStream('FS2_CheckMD5AndFastCopy').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_CheckMD5AndFastCopy;
  DTNoAuthService.RecvTunnel.RegisterCompleteBuffer('FS2_PostFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_PostFile;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('FS2_GetFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_GetFile;
  DTNoAuthService.RecvTunnel.RegisterStream('FS2_GetFileMD5').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_GetFileMD5;
  DTNoAuthService.RecvTunnel.RegisterStream('FS2_SearchMultiMD5').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_SearchMultiMD5;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('FS2_RemoveFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_RemoveFile;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('FS2_UpdateFileTime').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_UpdateFileTime;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('FS2_UpdateFileRef').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_UpdateFileRef;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('FS2_IncFileRef').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_IncFileRef;
  DTNoAuthService.RecvTunnel.RegisterStream('FS2_GetMD5Files').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_GetMD5Files;
  DTNoAuthService.RecvTunnel.RegisterStream('FS2_Size').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_Size;
  DTNoAuthService.RecvTunnel.RegisterStream('FS2_Search').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_Search;
  DTNoAuthService.RecvTunnel.RegisterStream('FS2_PoolFrag').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_PoolFrag;
  // instance
  ServiceInfo.OnlyInstance := True;
  UpdateToGlobalDispatch;
  ParamList.SetDefaultValue('OnlyInstance', if_(ServiceInfo.OnlyInstance, 'True', 'False'));

  ZDB2RecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('RecycleMemory', '5*1000'), 5 * 1000);
  ZDB2DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('DeltaSpace', '500*1024*1024'), 500 * 1024 * 1024);
  ZDB2BlockSize := EStrToInt(ParamList.GetDefaultValue('BlockSize', '16384'), 16384);
  ZDB2EnabledCipher := EStrToBool(ParamList.GetDefaultValue('EnabledCipher', 'False'), False);
  ZDB2CipherName := ParamList.GetDefaultValue('Cipher', TCipher.CCipherSecurityName[TCipherSecurity.csNone]);
  ZDB2Password := ParamList.GetDefaultValue('Password', Z.Net.C4.C40_Password);

  if ZDB2EnabledCipher then
      ZDB2Cipher := TZDB2_Cipher.Create(ZDB2CipherName, ZDB2Password, 1, True, True)
  else
      ZDB2Cipher := nil;
  C40_FS2_FileName := umlCombineFileName(DTNoAuthService.PublicFileDirectory, Get_DB_FileName_Config(PFormat('DTC40_%s.Space2', [ServiceInfo.ServiceTyp.Text])));
  Check_And_Replace_ZDB2_Extract_FileName(C40_FS2_FileName);

  FileHashPool := TC40_FS2_Service_File_Data_Pool.Create(True,
    EStrToInt64(ParamList.GetDefaultValue('File_HashPool', '4*1024*1024'), 4 * 1024 * 1024),
    nil);
  FileHashPool.IgnoreCase := True;

  MD5Pool := TC40_FS2_Service_MD5_Data_Pool.Create(True,
    EStrToInt64(ParamList.GetDefaultValue('MD5_HashPool', '4*1024*1024'), 4 * 1024 * 1024),
    nil);

  DoStatus('extract %s', [C40_FS2_FileName.Text]);

  if EStrToBool(ParamList.GetDefaultValue('ForeverSave', 'True'), True) and umlFileExists(C40_FS2_FileName) then
      FS := TCore_FileStream.Create(C40_FS2_FileName, fmOpenReadWrite)
  else
      FS := TCore_FileStream.Create(C40_FS2_FileName, fmCreate);

  FileDatabase := TZDB2_List_MS64.Create(
  TC40_FS2_Service_ZDB2_MS64,
    nil,
    ZDB2RecycleMemoryTimeOut,
    FS,
    False,
    ZDB2DeltaSpace,
    ZDB2BlockSize,
    ZDB2Cipher);
  FileDatabase.AutoFreeStream := True;

  if FileDatabase.Count > 0 then
    with FileDatabase.Repeat_ do
      repeat
        fd := TC40_FS2_Service_File_Data.Create(Self, Queue^.Data);
        fd.Stream.Data.Position := 0;
        fd.FileName := fd.Stream.Data.ReadString;
        fd.FileTime := fd.Stream.Data.ReadDouble;
        fd.FileRef := fd.Stream.Data.ReadInt32;
        fd.FileMD5 := umlMD5(fd.Stream.Data.PosAsPtr, fd.Stream.Data.Size - fd.Stream.Data.Position);
        fd.FileSize := fd.Stream.Data.Size - fd.Stream.Data.Position;
        fd.Stream.RecycleMemory;
        FileHashPool.Add(fd.FileName, fd);

        FL_ := MD5Pool[umlMD5ToStr(fd.FileMD5)];
        if FL_ <> nil then
          begin
            if FL_.ExistsValue(fd.FileName) < 0 then
                FL_.Add(fd.FileName);
          end
        else
          begin
            FL_ := TPascalStringList.Create;
            FL_.Add(fd.FileName);
            MD5Pool.Add(umlMD5ToStr(fd.FileMD5), FL_);
          end;
      until not Next;

  FLast_FS_Update_TimeTick := GetTimeTick();
  FileDatabaseIsUpdate := False;

  Register_ConsoleCommand('Compress_And_Reload', 'Compress file system and reload.').OnEvent_M := {$IFDEF FPC}@{$ENDIF FPC}CC_Compress_And_Reload;
end;

destructor TC40_FS2_Service.Destroy;
{$IFDEF FPC}
  procedure fpc_progress_(const Name_: PSystemString; Obj_: TC40_FS2_Service_File_Data);
  begin
    Obj_.Stream := nil;
  end;
{$ENDIF FPC}


begin
{$IFDEF FPC}
  FileHashPool.ProgressP(@fpc_progress_);
{$ELSE FPC}
  FileHashPool.ProgressP(procedure(const Name_: PSystemString; Obj_: TC40_FS2_Service_File_Data)
    begin
      Obj_.Stream := nil;
    end);
{$ENDIF FPC}
  DisposeObjectAndNil(FileHashPool);
  DisposeObjectAndNil(MD5Pool);
  DisposeObjectAndNil(FileDatabase);
  DisposeObjectAndNil(ZDB2Cipher);
  inherited Destroy;
end;

procedure TC40_FS2_Service.SafeCheck;
begin
  inherited SafeCheck;
  FileDatabase.Flush;
end;

procedure TC40_FS2_Service.Progress;
begin
  inherited Progress;
  FileDatabase.Progress;

  if GetTimeTick() - FLast_FS_Update_TimeTick > 1000 then
    begin
      if FileDatabaseIsUpdate then
        begin
          Update_All_Client_FS_State;
          FileDatabaseIsUpdate := False;
        end;
      FLast_FS_Update_TimeTick := GetTimeTick();
    end;
end;

procedure TC40_FS2_Service.Update_All_Client_FS_State;
var
  arry: TIO_Array;
  ID_: Cardinal;
  IO_, IO_S: TPeerIO;
  tmp_: TDFE;
begin
  tmp_ := TDFE.Create;
  tmp_.WriteInt64(FileDatabase.CoreSpace.State^.Physics);
  tmp_.WriteInt64(FileDatabase.Count);

  DTNoAuth.RecvTunnel.GetIO_Array(arry);
  for ID_ in arry do
    begin
      IO_ := DTNoAuth.RecvTunnel.PeerIO[ID_];
      if (IO_ <> nil) and DTNoAuth.GetUserDefineRecvTunnel(IO_).LinkOk then
        begin
          IO_S := DTNoAuth.GetUserDefineRecvTunnel(IO_).SendTunnel.Owner;
          IO_S.SendDirectStreamCmd('FS_State', tmp_);
        end;
    end;

  disposeObject(tmp_);
end;

constructor TFS2_Client_CacheData.Create(Owner_: TC40_FS2_Client);
begin
  inherited Create;
  Owner := Owner_;
  Stream := Owner.Cache_Database.NewData;
  LastAccess := GetTimeTick();
end;

destructor TFS2_Client_CacheData.Destroy;
begin
  Owner.Cache_Database.Remove(Stream, True);
  inherited Destroy;
end;

constructor TC40_FS2_Client_CheckMD5AndFastCopy.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_FS2_Client_CheckMD5AndFastCopy.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
begin
  State_ := Result_.R.ReadBool;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_);
  except
  end;
  DelayFreeObject(1.0, Self);
end;

procedure TC40_FS2_Client_CheckMD5AndFastCopy.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
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
  DelayFreeObject(1.0, Self);
end;

constructor TC40_FS2_Client_Post_File_Tunnel.Create;
begin
  inherited Create;
  p2pClient := nil;
  Client := nil;
  File_Name := '';
  Stream := TMS64.Create;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TC40_FS2_Client_Post_File_Tunnel.Destroy;
begin
  disposeObject(Stream);
  inherited Destroy;
end;

procedure TC40_FS2_Client_Post_File_Tunnel.DoP2PVM_CloneConnectAndPostFile(Sender: TZNet_WithP2PVM_Client);
var
  tmp_File_Name_: U_String;
  tmp_Time_: TDateTime;
  Cache: TFS2_Client_CacheData;
begin
  if Sender = nil then
      exit;
  Stream.Position := 0;
  tmp_File_Name_ := Stream.ReadString;
  tmp_Time_ := Stream.ReadDouble;
  Cache := TFS2_Client_CacheData.Create(Client);
  Client.FileCacheHashPool.Add(tmp_File_Name_, Cache);
  Sender.Print('build cache "%s"', [File_Name.Text]);
  Cache.Stream.Data.WritePtr(Stream.PosAsPtr, Stream.Size - Stream.Position);
  Cache.Stream.Save;
  Cache.LastAccess := GetTimeTick();
  Sender.Print('update cache "%s"', [File_Name.Text]);

  p2pClient := Sender;
  p2pClient.RegisterDirectConsole('PostDone').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_PostDone;
  Sender.SendCompleteBuffer('FS2_PostFile', Stream.Memory, Stream.Size, True);
  Stream.DiscardMemory;
end;

procedure TC40_FS2_Client_Post_File_Tunnel.cmd_PostDone(Sender: TPeerIO; InData: SystemString);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, InData);
    if Assigned(OnResultM) then
        OnResultM(Client, InData);
    if Assigned(OnResultP) then
        OnResultP(Client, InData);
  except
  end;
  p2pClient.UnRegisted('PostDone');
  Client.P2PVM_Recycle_Pool.Push(p2pClient);
end;

constructor TC40_FS2_Client_Post_File_Cache.Create;
begin
  inherited Create;
  Client := nil;
  File_Name := '';
  Stream := nil;
  doneFree := False;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_FS2_Client_Post_File_Cache.Do_CheckMD5AndFastCopy(Sender: TC40_FS2_Client; State_: Boolean);
var
  tmp: TC40_FS2_Client_Post_File_Tunnel;
begin
  if State_ then
    begin
      try
        if Assigned(OnResultC) then
            OnResultC(Client, File_Name);
        if Assigned(OnResultM) then
            OnResultM(Client, File_Name);
        if Assigned(OnResultP) then
            OnResultP(Client, File_Name);
      except
      end;
      if doneFree then
          disposeObject(Stream);
    end
  else
    begin
      tmp := TC40_FS2_Client_Post_File_Tunnel.Create;
      tmp.Client := Client;
      tmp.File_Name := File_Name;
      tmp.OnResultC := OnResultC;
      tmp.OnResultM := OnResultM;
      tmp.OnResultP := OnResultP;
      tmp.Stream.WriteString(File_Name); // name
      tmp.Stream.WriteDouble(umlNow); // file time
      tmp.Stream.WriteInt32(0); // ref
      Stream.Position := 0;
      tmp.Stream.CopyFrom(Stream, Stream.Size);

      if Client.P2PVM_Recycle_Pool.Num > 0 then
        begin
          tmp.DoP2PVM_CloneConnectAndPostFile(Client.P2PVM_Recycle_Pool.First^.Data);
          Client.P2PVM_Recycle_Pool.Next;
        end
      else
          Client.Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
      if doneFree then
          disposeObject(Stream);
    end;
  File_Name := '';
  DelayFreeObject(1.0, Self);
end;

constructor TC40_FS2_Client_Get_File_Tunnel.Create;
begin
  inherited Create;
  p2pClient := nil;
  Client := nil;
  File_Name := '';
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TC40_FS2_Client_Get_File_Tunnel.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_FS2_Client_Get_File_Tunnel.cmd_Save(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  tmp1, tmp2: TMS64;
  tmp_File_Name_: U_String;
  tmp_Time_: TDateTime;
  tmp_ref: Integer;
  Cache: TFS2_Client_CacheData;
begin
  tmp1 := TMS64.Create;
  tmp1.Mapping(InData, DataSize);
  tmp_File_Name_ := tmp1.ReadString;
  tmp_Time_ := tmp1.ReadDouble;
  tmp_ref := tmp1.ReadInt32;
  tmp2 := TMS64.Create;
  tmp2.Mapping(tmp1.PosAsPtr, tmp1.Size - tmp1.Position);

  Cache := TFS2_Client_CacheData.Create(Client);
  Client.FileCacheHashPool.Add(tmp_File_Name_, Cache);
  Sender.Print('build cache "%s"', [tmp_File_Name_.Text]);
  Cache.Stream.Data.LoadFromStream(tmp2);
  Cache.Stream.Save;
  Cache.LastAccess := GetTimeTick();
  Sender.Print('update cache "%s"', [tmp_File_Name_.Text]);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, tmp2, tmp_File_Name_, True);
    if Assigned(OnResultM) then
        OnResultM(Client, tmp2, tmp_File_Name_, True);
    if Assigned(OnResultP) then
        OnResultP(Client, tmp2, tmp_File_Name_, True);
  except
  end;

  disposeObject(tmp1);
  disposeObject(tmp2);

  p2pClient.UnRegisted('Save');
  p2pClient.UnRegisted('Error');
  Client.P2PVM_Recycle_Pool.Push(p2pClient);
end;

procedure TC40_FS2_Client_Get_File_Tunnel.cmd_Error(Sender: TPeerIO; InData: SystemString);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, nil, InData, False);
    if Assigned(OnResultM) then
        OnResultM(Client, nil, InData, False);
    if Assigned(OnResultP) then
        OnResultP(Client, nil, InData, False);
  except
  end;

  p2pClient.UnRegisted('Save');
  p2pClient.UnRegisted('Error');
  Client.P2PVM_Recycle_Pool.Push(p2pClient);
end;

procedure TC40_FS2_Client_Get_File_Tunnel.DoP2PVM_CloneConnectAndGetFile(Sender: TZNet_WithP2PVM_Client);
var
  d: TDFE;
begin
  if Sender = nil then
      exit;
  p2pClient := Sender;
  Sender.RegisterCompleteBuffer('Save').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Save;
  Sender.RegisterDirectConsole('Error').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Error;
  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteCardinal(Sender.ClientIO.ID);
  Client.DTNoAuthClient.SendTunnel.SendDirectStreamCmd('FS2_GetFile', d);
  disposeObject(d);
end;

constructor TC40_FS2_Client_GetFileMD5.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_FS2_Client_GetFileMD5.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
  MD5: TMD5;
begin
  State_ := False;
  info_ := 'error';
  MD5 := NullMD5;
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
      MD5 := Result_.R.ReadMD5;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_, MD5);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_, MD5);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_, MD5);
  except
  end;
  DelayFreeObject(1.0, Self);
end;

procedure TC40_FS2_Client_GetFileMD5.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_: SystemString;
  MD5: TMD5;
begin
  State_ := False;
  info_ := 'error.';
  MD5 := NullMD5;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_, MD5);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_, MD5);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_, MD5);
  except
  end;
  DelayFreeObject(1.0, Self);
end;

constructor TC40_FS2_Client_SearchMultiMD5.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_FS2_Client_SearchMultiMD5.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  arry: TFS2_SearchMultiMD5_State_Array;
  i: Integer;
begin
  SetLength(arry, Result_.Count shr 1);
  i := 0;
  while Result_.R.NotEnd do
    begin
      arry[i].MD5 := Result_.R.ReadMD5;
      arry[i].IsFound := Result_.R.ReadBool;
      inc(i);
    end;

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
  DelayFreeObject(1.0, Self);
end;

procedure TC40_FS2_Client_SearchMultiMD5.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  arry: TFS2_SearchMultiMD5_State_Array;
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
  DelayFreeObject(1.0, Self);
end;

constructor TC40_FS2_Client_GetMD5Files.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_FS2_Client_GetMD5Files.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  i: Integer;
  Files_: U_StringArray;
begin
  SetLength(Files_, Result_.Count);
  for i := 0 to Result_.Count - 1 do
      Files_[i] := Result_.ReadString(i);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Files_);
    if Assigned(OnResultM) then
        OnResultM(Client, Files_);
    if Assigned(OnResultP) then
        OnResultP(Client, Files_);
  except
  end;
  SetLength(Files_, 0);
  DelayFreeObject(1.0, Self);
end;

procedure TC40_FS2_Client_GetMD5Files.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  Files_: U_StringArray;
begin
  SetLength(Files_, 0);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Files_);
    if Assigned(OnResultM) then
        OnResultM(Client, Files_);
    if Assigned(OnResultP) then
        OnResultP(Client, Files_);
  except
  end;
  SetLength(Files_, 0);
  DelayFreeObject(1.0, Self);
end;

constructor TC40_FS2_Client_GetFileMD5_Cache.Create;
begin
  inherited Create;
  Client := nil;
  File_Name := '';
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TC40_FS2_Client_GetFileMD5_Cache.Destroy;
begin
  File_Name := '';
  inherited Destroy;
end;

procedure TC40_FS2_Client_GetFileMD5_Cache.Do_FS2_GetFileMD5(Sender: TC40_FS2_Client; State_: Boolean; info_: SystemString; MD5_: TMD5);
var
  Cache: TFS2_Client_CacheData;
  tmp: TC40_FS2_Client_Get_File_Tunnel;
begin
  if not State_ then
    begin
      try
        if Assigned(OnResultC) then
            OnResultC(Client, nil, File_Name, False);
        if Assigned(OnResultM) then
            OnResultM(Client, nil, File_Name, False);
        if Assigned(OnResultP) then
            OnResultP(Client, nil, File_Name, False);
      except
      end;
      Client.DTNoAuth.ProgressEngine.PostDelayFreeObject(1.0, Self, nil);
      exit;
    end;

  Cache := Client.FileCacheHashPool[File_Name];
  if (Cache <> nil) then
    begin
      if umlMD5Compare(umlStreamMD5(Cache.Stream.Data), MD5_) then
        begin
          Cache.LastAccess := GetTimeTick();
          Sender.DTNoAuth.RecvTunnel.Print('get file "%s" from cache', [File_Name.Text]);
          try
            Cache.Stream.Data.Position := 0;
            if Assigned(OnResultC) then
                OnResultC(Client, Cache.Stream.Data, File_Name, True);
            if Assigned(OnResultM) then
                OnResultM(Client, Cache.Stream.Data, File_Name, True);
            if Assigned(OnResultP) then
                OnResultP(Client, Cache.Stream.Data, File_Name, True);
          except
          end;
          Client.DTNoAuth.ProgressEngine.PostDelayFreeObject(1.0, Self, nil);
          exit;
        end;
    end;

  tmp := TC40_FS2_Client_Get_File_Tunnel.Create;
  tmp.Client := Client;
  tmp.File_Name := File_Name;
  tmp.OnResultC := OnResultC;
  tmp.OnResultM := OnResultM;
  tmp.OnResultP := OnResultP;

  if Client.P2PVM_Recycle_Pool.Num > 0 then
    begin
      tmp.DoP2PVM_CloneConnectAndGetFile(Client.P2PVM_Recycle_Pool.First^.Data);
      Client.P2PVM_Recycle_Pool.Next;
    end
  else
      Client.Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);

  Client.DTNoAuth.ProgressEngine.PostDelayFreeObject(1.0, Self, nil);
end;

constructor TC40_FS2_Client_Size.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_FS2_Client_Size.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  arry: TFS2_FileSizeInfo_Array;
  i: Integer;
begin
  SendData.R.Index := 0;
  SetLength(arry, Result_.Count);
  i := 0;
  while Result_.R.NotEnd do
    begin
      arry[i].FileName := SendData.R.ReadString;
      arry[i].Size := Result_.R.ReadInt64;
      inc(i);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  DelayFreeObject(1.0, Self);
end;

procedure TC40_FS2_Client_Size.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  arry: TFS2_FileSizeInfo_Array;
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
  DelayFreeObject(1.0, Self);
end;

constructor TC40_FS2_Client_Search.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_FS2_Client_Search.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  arry: TFS2_FileInfo_Array;
  i: Integer;
begin
  SetLength(arry, Result_.Count div 5);
  i := 0;
  while Result_.R.NotEnd do
    begin
      arry[i].FileName := Result_.R.ReadString;
      arry[i].FileTime := Result_.R.ReadDouble;
      arry[i].FileRef := Result_.R.ReadInteger;
      arry[i].Size := Result_.R.ReadInt64;
      arry[i].MD5 := Result_.R.ReadMD5;
      inc(i);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  DelayFreeObject(1.0, Self);
end;

procedure TC40_FS2_Client_Search.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  arry: TFS2_FileInfo_Array;
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
  DelayFreeObject(1.0, Self);
end;

constructor TC40_FS2_Client_PoolFrag.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_FS2_Client_PoolFrag.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  arry: TFS2_PoolFragInfo_Array;
  i: Integer;
begin
  SetLength(arry, Result_.Count div 3);
  i := 0;
  while Result_.R.NotEnd do
    begin
      arry[i].FileName := Result_.R.ReadString;
      arry[i].MD5 := Result_.R.ReadMD5;
      arry[i].FileTime := Result_.R.ReadDouble;
      inc(i);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  DelayFreeObject(1.0, Self);
end;

procedure TC40_FS2_Client_PoolFrag.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  arry: TFS2_PoolFragInfo_Array;
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
  DelayFreeObject(1.0, Self);
end;

procedure TC40_FS2_Client.Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client);
begin
  inherited Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender);
  FMaxFileSize := DTNoAuth.SendTunnel.ServerState^.MaxCompleteBufferSize;
end;

procedure TC40_FS2_Client.Do_Progress_FileCachePool(const Name_: PSystemString; Obj_: TFS2_Client_CacheData);
begin
  if GetTimeTick() - Obj_.LastAccess > Cache_File_Life then
      FRemoveCacheList.Add(Name_^);
end;

procedure TC40_FS2_Client.cmd_FS_State(Sender: TPeerIO; InData: TDFE);
begin
  FRemote_FS_DB_Size := InData.R.ReadInt64;
  FRemote_FS_Num := InData.R.ReadInt64;
end;

constructor TC40_FS2_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
var
  i: Integer;
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('FS_State').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS_State;
  FRemote_FS_DB_Size := 0;
  FRemote_FS_Num := 0;

  C40_FS2_Cache_FileName := umlCombineFileName({$IFDEF FPC}C40_RootPath{$ELSE FPC}TPath.GetTempPath{$ENDIF FPC},
    PFormat('DTC40_FS2_Cache_%s_%s.tmp', [source_.ServiceTyp.Text, umlMD5ToStr(source_.Hash).Text]));
  i := 1;
  while umlFileExists(C40_FS2_Cache_FileName) do
    begin
      C40_FS2_Cache_FileName := umlCombineFileName({$IFDEF FPC}C40_RootPath{$ELSE FPC}TPath.GetTempPath{$ENDIF FPC},
        PFormat('DTC40_FS2_Cache_%s_%s(%d).tmp', [source_.ServiceTyp.Text, umlMD5ToStr(source_.Hash).Text, i]));
      inc(i);
    end;

  ZDB2RecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('RecycleMemory', '5*1000'), 5 * 1000);
  ZDB2DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('DeltaSpace', '128*1024*1024'), 128 * 1024 * 1024);
  ZDB2BlockSize := EStrToInt(ParamList.GetDefaultValue('BlockSize', '1536'), 1536);
  ZDB2EnabledCipher := EStrToBool(ParamList.GetDefaultValue('EnabledCipher', 'False'), False);
  ZDB2CipherName := ParamList.GetDefaultValue('Cipher', TCipher.CCipherSecurityName[TCipherSecurity.csNone]);
  ZDB2Password := ParamList.GetDefaultValue('Password', Z.Net.C4.C40_Password);
  Cache_File_Life := EStrToInt64(ParamList.GetDefaultValue('CacheLife', '60*60*1000'), 60 * 60 * 1000);

  if ZDB2EnabledCipher then
      ZDB2Cipher := TZDB2_Cipher.Create(ZDB2CipherName, ZDB2Password, 1, True, True)
  else
      ZDB2Cipher := nil;

  FileCacheHashPool := TFS2_Client_CacheHashPool.Create(True,
    EStrToInt64(ParamList.GetDefaultValue('File_HashPool', '4*1024*1024'), 4 * 1024 * 1024),
    nil);

  Cache_Database := TZDB2_List_MS64.Create(
  TZDB2_MS64,
    nil,
    ZDB2RecycleMemoryTimeOut,
    TCore_FileStream.Create(C40_FS2_Cache_FileName, fmCreate),
    False,
    ZDB2DeltaSpace,
    ZDB2BlockSize,
    ZDB2Cipher);
  Cache_Database.CoreSpace.Mode := smBigData;
  Cache_Database.AutoFreeStream := True;

  FMaxFileSize := 0;
  FRemoveCacheList := TPascalStringList.Create;

  P2PVM_Recycle_Pool := TP2PVM_Recycle_Pool.Create;
end;

destructor TC40_FS2_Client.Destroy;
begin
  disposeObject(P2PVM_Recycle_Pool);
  disposeObject(FileCacheHashPool);
  disposeObject(Cache_Database);
  umlDeleteFile(C40_FS2_Cache_FileName);
  disposeObject(FRemoveCacheList);
  DisposeObjectAndNil(ZDB2Cipher);
  inherited Destroy;
end;

procedure TC40_FS2_Client.SafeCheck;
var
  i: Integer;
begin
  inherited SafeCheck;
  FRemoveCacheList.Clear;
  FileCacheHashPool.ProgressM({$IFDEF FPC}@{$ENDIF FPC}Do_Progress_FileCachePool);
  if FRemoveCacheList.Count > 0 then
    begin
      for i := 0 to FRemoveCacheList.Count - 1 do
          FileCacheHashPool.Delete(FRemoveCacheList[i]);
      FRemoveCacheList.Clear;
    end;
  Cache_Database.Flush;
end;

procedure TC40_FS2_Client.Progress;
begin
  inherited Progress;
  Cache_Database.Progress;
end;

procedure TC40_FS2_Client.RemoveCache(File_Name: U_String);
begin
  FileCacheHashPool.Delete(File_Name);
end;

procedure TC40_FS2_Client.RemoveCache(arry: U_StringArray);
var
  i: Integer;
begin
  for i := low(arry) to high(arry) do
      FileCacheHashPool.Delete(arry[i]);
end;

procedure TC40_FS2_Client.FS2_CheckMD5AndFastCopyC(File_Name: U_String; Stream_MD5_: TMD5; Stream_Size_: Int64; OnResult: TC40_FS2_Client_CheckMD5AndFastCopyC);
var
  tmp: TC40_FS2_Client_CheckMD5AndFastCopy;
  d: TDFE;
begin
  tmp := TC40_FS2_Client_CheckMD5AndFastCopy.Create;
  tmp.Client := Self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteDouble(umlNow);
  d.WriteInteger(0);
  d.WriteMD5(Stream_MD5_);
  d.WriteInt64(Stream_Size_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_CheckMD5AndFastCopy', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_CheckMD5AndFastCopyM(File_Name: U_String; Stream_MD5_: TMD5; Stream_Size_: Int64; OnResult: TC40_FS2_Client_CheckMD5AndFastCopyM);
var
  tmp: TC40_FS2_Client_CheckMD5AndFastCopy;
  d: TDFE;
begin
  tmp := TC40_FS2_Client_CheckMD5AndFastCopy.Create;
  tmp.Client := Self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteDouble(umlNow);
  d.WriteInteger(0);
  d.WriteMD5(Stream_MD5_);
  d.WriteInt64(Stream_Size_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_CheckMD5AndFastCopy', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_CheckMD5AndFastCopyP(File_Name: U_String; Stream_MD5_: TMD5; Stream_Size_: Int64; OnResult: TC40_FS2_Client_CheckMD5AndFastCopyP);
var
  tmp: TC40_FS2_Client_CheckMD5AndFastCopy;
  d: TDFE;
begin
  tmp := TC40_FS2_Client_CheckMD5AndFastCopy.Create;
  tmp.Client := Self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteDouble(umlNow);
  d.WriteInteger(0);
  d.WriteMD5(Stream_MD5_);
  d.WriteInt64(Stream_Size_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_CheckMD5AndFastCopy', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_CheckMD5AndFastCopyC(File_Name: U_String; Stream: TCore_Stream; OnResult: TC40_FS2_Client_CheckMD5AndFastCopyC);
var
  tmp: TC40_FS2_Client_CheckMD5AndFastCopy;
  d: TDFE;
begin
  if Stream.Size = 0 then
      exit;
  tmp := TC40_FS2_Client_CheckMD5AndFastCopy.Create;
  tmp.Client := Self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteDouble(umlNow);
  d.WriteInteger(0);
  d.WriteMD5(umlStreamMD5(Stream));
  d.WriteInt64(Stream.Size);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_CheckMD5AndFastCopy', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_CheckMD5AndFastCopyM(File_Name: U_String; Stream: TCore_Stream; OnResult: TC40_FS2_Client_CheckMD5AndFastCopyM);
var
  tmp: TC40_FS2_Client_CheckMD5AndFastCopy;
  d: TDFE;
begin
  if Stream.Size = 0 then
      exit;
  tmp := TC40_FS2_Client_CheckMD5AndFastCopy.Create;
  tmp.Client := Self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteDouble(umlNow);
  d.WriteInteger(0);
  d.WriteMD5(umlStreamMD5(Stream));
  d.WriteInt64(Stream.Size);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_CheckMD5AndFastCopy', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_CheckMD5AndFastCopyP(File_Name: U_String; Stream: TCore_Stream; OnResult: TC40_FS2_Client_CheckMD5AndFastCopyP);
var
  tmp: TC40_FS2_Client_CheckMD5AndFastCopy;
  d: TDFE;
begin
  if Stream.Size = 0 then
      exit;
  tmp := TC40_FS2_Client_CheckMD5AndFastCopy.Create;
  tmp.Client := Self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteDouble(umlNow);
  d.WriteInteger(0);
  d.WriteMD5(umlStreamMD5(Stream));
  d.WriteInt64(Stream.Size);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_CheckMD5AndFastCopy', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_PostFile(UsedCache_: Boolean; File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean);
var
  cache_: TC40_FS2_Client_Post_File_Cache;
  tmp: TC40_FS2_Client_Post_File_Tunnel;
begin
  if Stream.Size = 0 then
    begin
      if doneFree then
          disposeObject(Stream);
      exit;
    end;
  if UsedCache_ then
    begin
      cache_ := TC40_FS2_Client_Post_File_Cache.Create;
      cache_.Client := Self;
      cache_.File_Name := File_Name;
      cache_.Stream := Stream;
      cache_.doneFree := doneFree;
      FS2_CheckMD5AndFastCopyM(File_Name, Stream, {$IFDEF FPC}@{$ENDIF FPC}cache_.Do_CheckMD5AndFastCopy);
    end
  else
    begin
      tmp := TC40_FS2_Client_Post_File_Tunnel.Create;
      tmp.Client := Self;
      tmp.File_Name := File_Name;
      tmp.Stream.WriteString(File_Name); // name
      tmp.Stream.WriteDouble(umlNow); // file time
      tmp.Stream.WriteInt32(0); // ref
      Stream.Position := 0;
      tmp.Stream.CopyFrom(Stream, Stream.Size);
      if P2PVM_Recycle_Pool.Num > 0 then
        begin
          tmp.DoP2PVM_CloneConnectAndPostFile(P2PVM_Recycle_Pool.First^.Data);
          P2PVM_Recycle_Pool.Next;
        end
      else
          Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
      if doneFree then
          disposeObject(Stream);
    end;
end;

procedure TC40_FS2_Client.FS2_PostFile_C(UsedCache_: Boolean; File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean; OnResult: TC40_FS2_Client_PostFile_DoneC);
var
  cache_: TC40_FS2_Client_Post_File_Cache;
  tmp: TC40_FS2_Client_Post_File_Tunnel;
begin
  if Stream.Size = 0 then
    begin
      if doneFree then
          disposeObject(Stream);
      exit;
    end;
  if UsedCache_ then
    begin
      cache_ := TC40_FS2_Client_Post_File_Cache.Create;
      cache_.Client := Self;
      cache_.File_Name := File_Name;
      cache_.Stream := Stream;
      cache_.doneFree := doneFree;
      cache_.OnResultC := OnResult;
      FS2_CheckMD5AndFastCopyM(File_Name, Stream, {$IFDEF FPC}@{$ENDIF FPC}cache_.Do_CheckMD5AndFastCopy);
    end
  else
    begin
      tmp := TC40_FS2_Client_Post_File_Tunnel.Create;
      tmp.Client := Self;
      tmp.File_Name := File_Name;
      tmp.OnResultC := OnResult;
      tmp.Stream.WriteString(File_Name); // name
      tmp.Stream.WriteDouble(umlNow); // file time
      tmp.Stream.WriteInt32(0); // ref
      Stream.Position := 0;
      tmp.Stream.CopyFrom(Stream, Stream.Size);
      if P2PVM_Recycle_Pool.Num > 0 then
        begin
          tmp.DoP2PVM_CloneConnectAndPostFile(P2PVM_Recycle_Pool.First^.Data);
          P2PVM_Recycle_Pool.Next;
        end
      else
          Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
      if doneFree then
          disposeObject(Stream);
    end;
end;

procedure TC40_FS2_Client.FS2_PostFile_M(UsedCache_: Boolean; File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean; OnResult: TC40_FS2_Client_PostFile_DoneM);
var
  cache_: TC40_FS2_Client_Post_File_Cache;
  tmp: TC40_FS2_Client_Post_File_Tunnel;
begin
  if Stream.Size = 0 then
    begin
      if doneFree then
          disposeObject(Stream);
      exit;
    end;
  if UsedCache_ then
    begin
      cache_ := TC40_FS2_Client_Post_File_Cache.Create;
      cache_.Client := Self;
      cache_.File_Name := File_Name;
      cache_.Stream := Stream;
      cache_.doneFree := doneFree;
      cache_.OnResultM := OnResult;
      FS2_CheckMD5AndFastCopyM(File_Name, Stream, {$IFDEF FPC}@{$ENDIF FPC}cache_.Do_CheckMD5AndFastCopy);
    end
  else
    begin
      tmp := TC40_FS2_Client_Post_File_Tunnel.Create;
      tmp.Client := Self;
      tmp.File_Name := File_Name;
      tmp.OnResultM := OnResult;
      tmp.Stream.WriteString(File_Name); // name
      tmp.Stream.WriteDouble(umlNow); // file time
      tmp.Stream.WriteInt32(0); // ref
      Stream.Position := 0;
      tmp.Stream.CopyFrom(Stream, Stream.Size);
      if P2PVM_Recycle_Pool.Num > 0 then
        begin
          tmp.DoP2PVM_CloneConnectAndPostFile(P2PVM_Recycle_Pool.First^.Data);
          P2PVM_Recycle_Pool.Next;
        end
      else
          Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
      if doneFree then
          disposeObject(Stream);
    end;
end;

procedure TC40_FS2_Client.FS2_PostFile_P(UsedCache_: Boolean; File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean; OnResult: TC40_FS2_Client_PostFile_DoneP);
var
  cache_: TC40_FS2_Client_Post_File_Cache;
  tmp: TC40_FS2_Client_Post_File_Tunnel;
begin
  if Stream.Size = 0 then
    begin
      if doneFree then
          disposeObject(Stream);
      exit;
    end;
  if UsedCache_ then
    begin
      cache_ := TC40_FS2_Client_Post_File_Cache.Create;
      cache_.Client := Self;
      cache_.File_Name := File_Name;
      cache_.Stream := Stream;
      cache_.doneFree := doneFree;
      cache_.OnResultP := OnResult;
      FS2_CheckMD5AndFastCopyM(File_Name, Stream, {$IFDEF FPC}@{$ENDIF FPC}cache_.Do_CheckMD5AndFastCopy);
    end
  else
    begin
      tmp := TC40_FS2_Client_Post_File_Tunnel.Create;
      tmp.Client := Self;
      tmp.File_Name := File_Name;
      tmp.OnResultP := OnResult;
      tmp.Stream.WriteString(File_Name); // name
      tmp.Stream.WriteDouble(umlNow); // file time
      tmp.Stream.WriteInt32(0); // ref
      Stream.Position := 0;
      tmp.Stream.CopyFrom(Stream, Stream.Size);
      if P2PVM_Recycle_Pool.Num > 0 then
        begin
          tmp.DoP2PVM_CloneConnectAndPostFile(P2PVM_Recycle_Pool.First^.Data);
          P2PVM_Recycle_Pool.Next;
        end
      else
          Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
      if doneFree then
          disposeObject(Stream);
    end;
end;

procedure TC40_FS2_Client.FS2_GetFile_C(UsedCache_: Boolean; File_Name: U_String; OnResult: TC40_FS2_Client_GetFile_DoneC);
var
  tmp_cache_: TC40_FS2_Client_GetFileMD5_Cache;
  tmp: TC40_FS2_Client_Get_File_Tunnel;
begin
  if UsedCache_ and FileCacheHashPool.Exists(File_Name) then
    begin
      tmp_cache_ := TC40_FS2_Client_GetFileMD5_Cache.Create;
      tmp_cache_.Client := Self;
      tmp_cache_.File_Name := File_Name;
      tmp_cache_.OnResultC := OnResult;
      FS2_GetFileMD5M(File_Name, {$IFDEF FPC}@{$ENDIF FPC}tmp_cache_.Do_FS2_GetFileMD5);
    end
  else
    begin
      tmp := TC40_FS2_Client_Get_File_Tunnel.Create;
      tmp.Client := Self;
      tmp.File_Name := File_Name;
      tmp.OnResultC := OnResult;
      if P2PVM_Recycle_Pool.Num > 0 then
        begin
          tmp.DoP2PVM_CloneConnectAndGetFile(P2PVM_Recycle_Pool.First^.Data);
          P2PVM_Recycle_Pool.Next;
        end
      else
          Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);
    end;
end;

procedure TC40_FS2_Client.FS2_GetFile_M(UsedCache_: Boolean; File_Name: U_String; OnResult: TC40_FS2_Client_GetFile_DoneM);
var
  tmp_cache_: TC40_FS2_Client_GetFileMD5_Cache;
  tmp: TC40_FS2_Client_Get_File_Tunnel;
begin
  if UsedCache_ and FileCacheHashPool.Exists(File_Name) then
    begin
      tmp_cache_ := TC40_FS2_Client_GetFileMD5_Cache.Create;
      tmp_cache_.Client := Self;
      tmp_cache_.File_Name := File_Name;
      tmp_cache_.OnResultM := OnResult;
      FS2_GetFileMD5M(File_Name, {$IFDEF FPC}@{$ENDIF FPC}tmp_cache_.Do_FS2_GetFileMD5);
    end
  else
    begin
      tmp := TC40_FS2_Client_Get_File_Tunnel.Create;
      tmp.Client := Self;
      tmp.File_Name := File_Name;
      tmp.OnResultM := OnResult;
      if P2PVM_Recycle_Pool.Num > 0 then
        begin
          tmp.DoP2PVM_CloneConnectAndGetFile(P2PVM_Recycle_Pool.First^.Data);
          P2PVM_Recycle_Pool.Next;
        end
      else
          Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);
    end;
end;

procedure TC40_FS2_Client.FS2_GetFile_P(UsedCache_: Boolean; File_Name: U_String; OnResult: TC40_FS2_Client_GetFile_DoneP);
var
  tmp_cache_: TC40_FS2_Client_GetFileMD5_Cache;
  tmp: TC40_FS2_Client_Get_File_Tunnel;
begin
  if UsedCache_ and FileCacheHashPool.Exists(File_Name) then
    begin
      tmp_cache_ := TC40_FS2_Client_GetFileMD5_Cache.Create;
      tmp_cache_.Client := Self;
      tmp_cache_.File_Name := File_Name;
      tmp_cache_.OnResultP := OnResult;
      FS2_GetFileMD5M(File_Name, {$IFDEF FPC}@{$ENDIF FPC}tmp_cache_.Do_FS2_GetFileMD5);
    end
  else
    begin
      tmp := TC40_FS2_Client_Get_File_Tunnel.Create;
      tmp.Client := Self;
      tmp.File_Name := File_Name;
      tmp.OnResultP := OnResult;
      if P2PVM_Recycle_Pool.Num > 0 then
        begin
          tmp.DoP2PVM_CloneConnectAndGetFile(P2PVM_Recycle_Pool.First^.Data);
          P2PVM_Recycle_Pool.Next;
        end
      else
          Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);
    end;
end;

procedure TC40_FS2_Client.FS2_GetFileMD5C(File_Name: U_String; OnResult: TC40_FS2_Client_GetFileMD5C);
var
  tmp: TC40_FS2_Client_GetFileMD5;
  d: TDFE;
begin
  tmp := TC40_FS2_Client_GetFileMD5.Create;
  tmp.Client := Self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  d.WriteString(File_Name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_GetFileMD5', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_GetFileMD5M(File_Name: U_String; OnResult: TC40_FS2_Client_GetFileMD5M);
var
  tmp: TC40_FS2_Client_GetFileMD5;
  d: TDFE;
begin
  tmp := TC40_FS2_Client_GetFileMD5.Create;
  tmp.Client := Self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  d.WriteString(File_Name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_GetFileMD5', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_GetFileMD5P(File_Name: U_String; OnResult: TC40_FS2_Client_GetFileMD5P);
var
  tmp: TC40_FS2_Client_GetFileMD5;
  d: TDFE;
begin
  tmp := TC40_FS2_Client_GetFileMD5.Create;
  tmp.Client := Self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  d.WriteString(File_Name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_GetFileMD5', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SearchMultiMD5C(arry: TArrayMD5; OnResult: TC40_FS2_Client_SearchMultiMD5C);
var
  tmp: TC40_FS2_Client_SearchMultiMD5;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_FS2_Client_SearchMultiMD5.Create;
  tmp.Client := Self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  for i := low(arry) to high(arry) do
      d.WriteMD5(arry[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_SearchMultiMD5', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SearchMultiMD5M(arry: TArrayMD5; OnResult: TC40_FS2_Client_SearchMultiMD5M);
var
  tmp: TC40_FS2_Client_SearchMultiMD5;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_FS2_Client_SearchMultiMD5.Create;
  tmp.Client := Self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  for i := low(arry) to high(arry) do
      d.WriteMD5(arry[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_SearchMultiMD5', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SearchMultiMD5P(arry: TArrayMD5; OnResult: TC40_FS2_Client_SearchMultiMD5P);
var
  tmp: TC40_FS2_Client_SearchMultiMD5;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_FS2_Client_SearchMultiMD5.Create;
  tmp.Client := Self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  for i := low(arry) to high(arry) do
      d.WriteMD5(arry[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_SearchMultiMD5', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_GetMD5FilesC(MD5_: TMD5; OnResult: TC40_FS2_Client_GetMD5FilesC);
var
  tmp: TC40_FS2_Client_GetMD5Files;
  d: TDFE;
begin
  tmp := TC40_FS2_Client_GetMD5Files.Create;
  tmp.Client := Self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  d.WriteMD5(MD5_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_GetMD5Files', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_GetMD5FilesM(MD5_: TMD5; OnResult: TC40_FS2_Client_GetMD5FilesM);
var
  tmp: TC40_FS2_Client_GetMD5Files;
  d: TDFE;
begin
  tmp := TC40_FS2_Client_GetMD5Files.Create;
  tmp.Client := Self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  d.WriteMD5(MD5_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_GetMD5Files', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_GetMD5FilesP(MD5_: TMD5; OnResult: TC40_FS2_Client_GetMD5FilesP);
var
  tmp: TC40_FS2_Client_GetMD5Files;
  d: TDFE;
begin
  tmp := TC40_FS2_Client_GetMD5Files.Create;
  tmp.Client := Self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  d.WriteMD5(MD5_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_GetMD5Files', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_RemoveFile(File_Name: U_String);
var
  d: TDFE;
begin
  FileCacheHashPool.Delete(File_Name);
  d := TDFE.Create;
  d.WriteString(File_Name);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('FS2_RemoveFile', d);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_RemoveFile(arry: U_StringArray);
var
  d: TDFE;
  i: Integer;
begin
  d := TDFE.Create;
  for i := low(arry) to high(arry) do
    begin
      FileCacheHashPool.Delete(arry[i]);
      d.WriteString(arry[i]);
    end;
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('FS2_RemoveFile', d);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_UpdateFileTime(File_Name: U_String; nTime: TDateTime);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteDouble(nTime);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('FS2_UpdateFileTime', d);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_UpdateFileRef(File_Name: U_String; ref_: Integer);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteInteger(ref_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('FS2_UpdateFileRef', d);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_IncFileRef(File_Name: U_String; inc_ref_: Integer);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteInteger(inc_ref_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('FS2_IncFileRef', d);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SizeC(FileNames: U_StringArray; OnResult: TC40_FS2_Client_SizeC);
var
  tmp: TC40_FS2_Client_Size;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_FS2_Client_Size.Create;
  tmp.Client := Self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  for i := 0 to length(FileNames) - 1 do
      d.WriteString(FileNames[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_Size', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SizeM(FileNames: U_StringArray; OnResult: TC40_FS2_Client_SizeM);
var
  tmp: TC40_FS2_Client_Size;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_FS2_Client_Size.Create;
  tmp.Client := Self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  for i := 0 to length(FileNames) - 1 do
      d.WriteString(FileNames[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_Size', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SizeP(FileNames: U_StringArray; OnResult: TC40_FS2_Client_SizeP);
var
  tmp: TC40_FS2_Client_Size;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_FS2_Client_Size.Create;
  tmp.Client := Self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  for i := 0 to length(FileNames) - 1 do
      d.WriteString(FileNames[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_Size', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SearchC(filter: U_String; MaxNum: Integer; OnResult: TC40_FS2_Client_SearchC);
var
  tmp: TC40_FS2_Client_Search;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_FS2_Client_Search.Create;
  tmp.Client := Self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  d.WriteString(filter);
  d.WriteInteger(MaxNum);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_Search', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SearchM(filter: U_String; MaxNum: Integer; OnResult: TC40_FS2_Client_SearchM);
var
  tmp: TC40_FS2_Client_Search;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_FS2_Client_Search.Create;
  tmp.Client := Self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  d.WriteString(filter);
  d.WriteInteger(MaxNum);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_Search', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SearchP(filter: U_String; MaxNum: Integer; OnResult: TC40_FS2_Client_SearchP);
var
  tmp: TC40_FS2_Client_Search;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_FS2_Client_Search.Create;
  tmp.Client := Self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  d.WriteString(filter);
  d.WriteInteger(MaxNum);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_Search', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_PoolFragC(OnResult: TC40_FS2_Client_PoolFragC);
var
  tmp: TC40_FS2_Client_PoolFrag;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_FS2_Client_PoolFrag.Create;
  tmp.Client := Self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_PoolFrag', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_PoolFragM(OnResult: TC40_FS2_Client_PoolFragM);
var
  tmp: TC40_FS2_Client_PoolFrag;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_FS2_Client_PoolFrag.Create;
  tmp.Client := Self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_PoolFrag', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

procedure TC40_FS2_Client.FS2_PoolFragP(OnResult: TC40_FS2_Client_PoolFragP);
var
  tmp: TC40_FS2_Client_PoolFrag;
  d: TDFE;
  i: Integer;
begin
  tmp := TC40_FS2_Client_PoolFrag.Create;
  tmp.Client := Self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_PoolFrag', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  disposeObject(d);
end;

initialization

RegisterC40('FS2', TC40_FS2_Service, TC40_FS2_Client);

end.
