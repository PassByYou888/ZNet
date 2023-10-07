{ ****************************************************************************** }
{ * double tunnel IO framework(Auth)                                           * }
{ ****************************************************************************** }

unit Z.Net.DoubleTunnelIO;

{$DEFINE FPC_DELPHI_MODE}
{$I ..\Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core,
  Z.ListEngine, Z.UnicodeMixedLib,
  Z.DFE, Z.MemoryStream, Z.Net, Z.Net.PhysicsIO,
  Z.TextDataEngine,
  Z.Status, Z.Cadencer, Z.Notify, Z.ZDB.FilePackage_LIB,
  Z.ZDB, Z.Cipher, Z.PascalStrings, Z.UPascalStrings;

type
  TDTService = class;
  TService_RecvTunnel_UserDefine = class;
  TDTServiceClass = class of TDTService;

  TService_SendTunnel_UserDefine = class(TPeer_IO_User_Define)
  public
    RecvTunnel: TService_RecvTunnel_UserDefine;
    RecvTunnelID: Cardinal;
    DoubleTunnelService: TDTService;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
    function LinkOk: Boolean;
    property BindOk: Boolean read LinkOk;
  end;

  TService_RecvTunnel_UserDefine = class(TPeer_IO_User_Define)
  public
    SendTunnel: TService_SendTunnel_UserDefine;
    SendTunnelID: Cardinal;
    UserFlag, UserID: SystemString;
    UserPath: SystemString;
    UserConfigFile: THashTextEngine;
    DoubleTunnelService: TDTService;
    UserDBIntf: THashVariantList;
    LoginSuccessed: Boolean;
    FCurrentFileStream: TCore_Stream;
    FCurrentReceiveFileName: SystemString;

    WaitLink: Boolean;
    WaitLinkSendID: Cardinal;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;

    function MakeFilePath(fn: SystemString): SystemString;
    function GetUserID: SystemString;

    procedure SaveConfigFile; virtual;

    function LinkOk: Boolean;
    property BindOk: Boolean read LinkOk;
    property CurrentFileStream: TCore_Stream read FCurrentFileStream write FCurrentFileStream;
    property CurrentReceiveFileName: SystemString read FCurrentReceiveFileName write FCurrentReceiveFileName;
  end;

  TPeerClientUserDefineForRecvTunnel_List = TGenericsList<TService_RecvTunnel_UserDefine>;

  TOnLinkSuccess = procedure(Sender: TDTService; UserDefineIO: TService_RecvTunnel_UserDefine) of object;
  TOnUserOut = procedure(Sender: TDTService; UserDefineIO: TService_RecvTunnel_UserDefine) of object;

  TDTService = class(TCore_InterfacedObject)
  protected
    FRecvTunnel, FSendTunnel: TZNet_Server;
    FFileSystem: Boolean;
    FRootPath, FPublicPath: SystemString;
    FUserDB: THashTextEngine;
    FAllowRegisterNewUser: Boolean;
    FAllowSaveUserInfo: Boolean;
    FCadencerEngine: TCadencer;
    FProgressEngine: TN_Progress_Tool;
    { event }
    FOnLinkSuccess: TOnLinkSuccess;
    FOnUserOut: TOnUserOut;
  protected
    { virtual event }
    procedure UserRegistedSuccess(UserID: SystemString); virtual;
    procedure UserLoginSuccess(UserDefineIO: TService_RecvTunnel_UserDefine); virtual;
    procedure UserLinkSuccess(UserDefineIO: TService_RecvTunnel_UserDefine); virtual;
    procedure UserCreateDirectorySuccess(UserDefineIO: TService_RecvTunnel_UserDefine; dn: SystemString); virtual;
    procedure UserPostFileSuccess(UserDefineIO: TService_RecvTunnel_UserDefine; fn: SystemString); virtual;
    procedure UserOut(UserDefineIO: TService_RecvTunnel_UserDefine); virtual;
  protected
    { registed server command }
    procedure Command_UserLogin(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_RegisterUser(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_TunnelLink(Sender: TPeerIO; InData, OutData: TDFE); virtual;

    procedure Command_ChangePasswd(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_CustomNewUser(Sender: TPeerIO; InData, OutData: TDFE); virtual;

    procedure Command_ProcessStoreQueueCMD(Sender: TPeerIO; InData: TDFE); virtual;

    procedure Command_GetPublicFileList(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetPrivateFileList(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetPrivateDirectoryList(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_CreatePrivateDirectory(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetPublicFileInfo(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetPrivateFileInfo(Sender: TPeerIO; InData, OutData: TDFE); virtual;

    procedure Do_Th_Command_GetPublicFileMD5(ThSender: THPC_Stream; ThInData, ThOutData: TDFE);
    procedure Command_GetPublicFileMD5(Sender: TPeerIO; InData, OutData: TDFE); virtual;

    procedure Do_Th_Command_GetPrivateFileMD5(ThSender: THPC_Stream; ThInData, ThOutData: TDFE);
    procedure Command_GetPrivateFileMD5(Sender: TPeerIO; InData, OutData: TDFE); virtual;

    procedure Command_GetPublicFile(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetPrivateFile(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetUserPrivateFile(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetPublicFileAs(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetPrivateFileAs(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetUserPrivateFileAs(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_PostPublicFileInfo(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_PostPrivateFileInfo(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_PostFile(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_PostFileOver(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_GetPublicFileFragmentData(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetPrivateFileFragmentData(Sender: TPeerIO; InData, OutData: TDFE); virtual;

    procedure Command_GetCurrentCadencer(Sender: TPeerIO; InData, OutData: TDFE); virtual;

    procedure Command_NewBatchStream(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_PostBatchStream(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_ClearBatchStream(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDFE); virtual;
  public
    constructor Create(RecvTunnel_, SendTunnel_: TZNet_Server); virtual;
    destructor Destroy; override;

    procedure SwitchAsMaxPerformance;
    procedure SwitchAsMaxSecurity;
    procedure SwitchAsDefaultPerformance;

    procedure Progress; virtual;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); virtual;

    {
      load USERDB
      load need execute in rootpath set completed after
    }
    procedure LoadUserDB;

    procedure SaveUserDB;

    function RegUser(UsrID, UsrPasswd: SystemString; UserConfigFile_: THashTextEngine): Boolean;
    function ExistsUser(UsrID: SystemString): Boolean;
    function GetUserPath(UsrID: SystemString): SystemString;
    function GetUserFile(UsrID, UserFileName_: SystemString): SystemString;
    function GetUserDefineIO(UsrID: SystemString): TService_RecvTunnel_UserDefine;
    function UserOnline(UsrID: SystemString): Boolean;

    function PackUserAsFile(UsrID, packageFile: SystemString): Boolean;
    function PackUserAsStream(UsrID: SystemString; packageStream: TCore_Stream): Boolean;
    function UnPackFileAsUser(packageFile: SystemString): Boolean;
    function UnPackStreamAsUser(packageStream: TCore_Stream): Boolean;

    {
      only work in direct command
      if user online immediate execution
      if user offline store to notify queue
    }
    procedure PostStoreQueueCMD(ToUserID: SystemString; Cmd: SystemString; InData: TDFE);

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;

    function MakeUserFlag: SystemString;
    function GetUserDefineRecvTunnel(RecvCli: TPeerIO): TService_RecvTunnel_UserDefine;

    function TotalLinkCount: Integer;

    procedure PostBatchStream(cli: TPeerIO; stream: TCore_Stream; doneFreeStream: Boolean); overload;
    procedure PostBatchStreamC(cli: TPeerIO; stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_C); overload;
    procedure PostBatchStreamM(cli: TPeerIO; stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_M); overload;
    procedure PostBatchStreamP(cli: TPeerIO; stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_P); overload;
    procedure ClearBatchStream(cli: TPeerIO);
    procedure GetBatchStreamStateM(cli: TPeerIO; OnResult: TOnStream_M); overload;
    procedure GetBatchStreamStateM(cli: TPeerIO; Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_M); overload;
    procedure GetBatchStreamStateP(cli: TPeerIO; OnResult: TOnStream_P); overload;
    procedure GetBatchStreamStateP(cli: TPeerIO; Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_P); overload;

    property AllowRegisterNewUser: Boolean read FAllowRegisterNewUser write FAllowRegisterNewUser;
    property AllowSaveUserInfo: Boolean read FAllowSaveUserInfo write FAllowSaveUserInfo;

    property FileSystem: Boolean read FFileSystem write FFileSystem;
    { private store space }
    property RootPath: SystemString read FRootPath write FRootPath;
    { public store space }
    property PublicPath: SystemString read FPublicPath write FPublicPath;

    property CadencerEngine: TCadencer read FCadencerEngine;

    property ProgressEngine: TN_Progress_Tool read FProgressEngine;
    property ProgressPost: TN_Progress_Tool read FProgressEngine;
    property PostProgress: TN_Progress_Tool read FProgressEngine;
    property PostRun: TN_Progress_Tool read FProgressEngine;
    property PostExecute: TN_Progress_Tool read FProgressEngine;

    property RecvTunnel: TZNet_Server read FRecvTunnel;
    property SendTunnel: TZNet_Server read FSendTunnel;

    property OnLinkSuccess: TOnLinkSuccess read FOnLinkSuccess write FOnLinkSuccess;
    property OnUserOut: TOnUserOut read FOnUserOut write FOnUserOut;
  end;

  TDTClient = class;
  TClient_SendTunnel = class;
  TDTClientClass = class of TDTClient;

  TClient_RecvTunnel = class(TPeer_IO_User_Define)
  public
    Client: TDTClient;
    SendTunnel: TClient_SendTunnel;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TClient_SendTunnel = class(TPeer_IO_User_Define)
  public
    Client: TDTClient;
    RecvTunnel: TClient_RecvTunnel;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TGetFileInfo_C = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
  TGetFileInfo_M = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64) of object;
  TFileMD5_C = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5);
  TFileMD5_M = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5) of object;
  TFileComplete_C = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    stream: TCore_Stream; const fileName: SystemString);
  TFileComplete_M = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    stream: TCore_Stream; const fileName: SystemString) of object;
  TFileFragmentData_C = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5);
  TFileFragmentData_M = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5) of object;

{$IFDEF FPC}
  TGetFileInfo_P = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64) is nested;
  TFileMD5_P = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5) is nested;
  TFileComplete_P = procedure(const UserData: Pointer; const UserObject: TCore_Object; stream:
      TCore_Stream; const fileName: SystemString) is nested;
  TFileFragmentData_P = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5) is nested;
{$ELSE FPC}
  TGetFileInfo_P = reference to procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
  TFileMD5_P = reference to procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5);
  TFileComplete_P = reference to procedure(const UserData: Pointer; const UserObject: TCore_Object; stream:
      TCore_Stream; const fileName: SystemString);
  TFileFragmentData_P = reference to procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5);
{$ENDIF FPC}

  TDTClient = class(TCore_InterfacedObject, IZNet_ClientInterface)
  protected
    FSendTunnel, FRecvTunnel: TZNet_Client;
    FFileSystem: Boolean;
    FCurrentStream: TCore_Stream;
    FCurrentReceiveStreamFileName: SystemString;
    FAutoFreeTunnel: Boolean;
    FLinkOk: Boolean;
    FWaitCommandTimeout: Cardinal;

    FRecvFileing: Boolean;
    FRecvFileOfBatching: Boolean;
    FRecvFileName: SystemString;

    FCadencerEngine: TCadencer;
    FLastCadencerTime: Double;
    FServerDelay: Double;

    FProgressEngine: TN_Progress_Tool;
  public
    { registed client command }
    procedure Command_FileInfo(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_PostFile(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_PostFileOver(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_PostFileFragmentData(Sender: TPeerIO; InData: PByte; DataSize: NativeInt); virtual;

    procedure GetPublicFileInfo_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE); virtual;
    procedure GetPrivateFileInfo_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE); virtual;

    procedure GetPublicFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE); virtual;
    procedure GetPrivateFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE); virtual;

    procedure GetPublicFile_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE); virtual;
    procedure GetPrivateFile_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE); virtual;

    { Downloading file fragment data from the server asynchronously and triggering notifications when completed }
    procedure GetFileFragmentData_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE); virtual;

    { GetCurrentCadencer result proc }
    procedure GetCurrentCadencer_StreamResult(Sender: TPeerIO; Result_: TDFE); virtual;

    { batch stream suppport }
    procedure Command_NewBatchStream(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_PostBatchStream(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_ClearBatchStream(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDFE); virtual;
  protected
    { client notify interface }
    procedure ClientConnected(Sender: TZNet_Client); virtual;
    procedure ClientDisconnect(Sender: TZNet_Client); virtual;
  protected
    { async connect support }
    FAsyncConnectAddr: SystemString;
    FAsyncConnRecvPort, FAsyncConnSendPort: Word;
    FAsyncOnResult_C: TOnState_C;
    FAsyncOnResult_M: TOnState_M;
    FAsyncOnResult_P: TOnState_P;
    procedure AsyncSendConnectResult(const cState: Boolean);
    procedure AsyncRecvConnectResult(const cState: Boolean);
    procedure UserLogin_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
    procedure UserLogin_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
    procedure RegisterUser_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
    procedure RegisterUser_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
    procedure TunnelLink_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
    procedure TunnelLink_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
  public
    constructor Create(RecvTunnel_, SendTunnel_: TZNet_Client); virtual;
    destructor Destroy; override;

    // free recveive+send tunnel from destroy, default is false
    property AutoFreeTunnel: Boolean read FAutoFreeTunnel write FAutoFreeTunnel;

    function Connected: Boolean; virtual;

    function IOBusy: Boolean;

    procedure SwitchAsMaxPerformance;
    procedure SwitchAsMaxSecurity;
    procedure SwitchAsDefaultPerformance;

    procedure Progress; virtual;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); virtual;

    { sync connect }
    function Connect(addr: SystemString; const RecvPort, SendPort: Word): Boolean; virtual;

    { async connection }
    procedure AsyncConnectC(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TOnState_C); overload; virtual;
    procedure AsyncConnectM(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TOnState_M); overload; virtual;
    procedure AsyncConnectP(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TOnState_P); overload; virtual;
    { parameter async connection }
    procedure AsyncConnectC(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TOnParamState_C); overload;
    procedure AsyncConnectM(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TOnParamState_M); overload;
    procedure AsyncConnectP(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TOnParamState_P); overload;
    procedure Disconnect; virtual;

    { sync mode }
    function UserLogin(UserID, passwd: SystemString): Boolean; virtual;
    function RegisterUser(UserID, passwd: SystemString): Boolean; virtual;
    function TunnelLink: Boolean; virtual;

    { async mode }
    procedure UserLoginC(UserID, passwd: SystemString; On_C: TOnState_C); virtual;
    procedure UserLoginM(UserID, passwd: SystemString; On_M: TOnState_M); virtual;
    procedure UserLoginP(UserID, passwd: SystemString; On_P: TOnState_P); virtual;
    procedure RegisterUserC(UserID, passwd: SystemString; On_C: TOnState_C); virtual;
    procedure RegisterUserM(UserID, passwd: SystemString; On_M: TOnState_M); virtual;
    procedure RegisterUserP(UserID, passwd: SystemString; On_P: TOnState_P); virtual;
    procedure TunnelLinkC(On_C: TOnState_C); virtual;
    procedure TunnelLinkM(On_M: TOnState_M); virtual;
    procedure TunnelLinkP(On_P: TOnState_P); virtual;
    procedure SyncCadencer; virtual;

    { sync user op }
    function ChangePassword(oldPasswd, newPasswd: SystemString): Boolean;
    function CustomNewUser(UsrID, UsrPasswd: SystemString; UserConfigFile_: THashTextEngine): Boolean;
    procedure ProcessStoreQueueCMD;
    procedure GetPublicFileList(Filter: SystemString; lst: TCore_Strings);
    procedure GetPrivateFileList(Filter, RemoteDirectory: SystemString; lst: TCore_Strings); overload;
    procedure GetPrivateFileList(Filter: SystemString; lst: TCore_Strings); overload;
    procedure GetPrivateDirectoryList(Filter, RemoteDirectory: SystemString; lst: TCore_Strings); overload;
    procedure GetPrivateDirectoryList(Filter: SystemString; lst: TCore_Strings); overload;
    function CreatePrivateDirectory(RemoteDirectory: SystemString): Boolean;

    { remote file information }
    procedure GetPublicFileInfoC(fileName: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_C); overload;
    procedure GetPublicFileInfoM(fileName: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_M); overload;
    procedure GetPublicFileInfoP(fileName: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_P); overload;
    procedure GetPrivateFileInfoC(fileName, RemoteDirectory: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_C); overload;
    procedure GetPrivateFileInfoM(fileName, RemoteDirectory: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_M); overload;
    procedure GetPrivateFileInfoP(fileName, RemoteDirectory: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_P); overload;

    { remote md5 support with public store space }
    procedure GetPublicFileMD5C(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_C); overload;
    procedure GetPublicFileMD5M(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_M); overload;
    procedure GetPublicFileMD5P(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_P); overload;

    { remote md5 support with private store space }
    procedure GetPrivateFileMD5C(fileName, RemoteDirectory: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_C); overload;
    procedure GetPrivateFileMD5M(fileName, RemoteDirectory: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_M); overload;
    procedure GetPrivateFileMD5P(fileName, RemoteDirectory: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_P); overload;

    { normal download with public store space }
    function GetPublicFile(fileName, saveToPath: SystemString): Boolean; overload;
    procedure GetPublicFileC(fileName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C); overload;
    procedure GetPublicFileM(fileName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M); overload;
    procedure GetPublicFileP(fileName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P); overload;

    { restore download with public store space }
    function GetPublicFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean; overload;
    procedure GetPublicFileC(fileName: SystemString; StartPos: Int64; saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C); overload;
    procedure GetPublicFileM(fileName: SystemString; StartPos: Int64; saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M); overload;
    procedure GetPublicFileP(fileName: SystemString; StartPos: Int64; saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P); overload;
    procedure GetPublicFileAsC(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C); overload;
    procedure GetPublicFileAsM(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M); overload;
    procedure GetPublicFileAsP(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P); overload;

    { normal download with private store space }
    function GetPrivateFile(fileName, RemoteDirectory, saveToPath: SystemString): Boolean; overload;
    function GetPrivateFile(fileName, saveToPath: SystemString): Boolean; overload;
    procedure GetPrivateFileC(fileName, RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C); overload;
    procedure GetPrivateFileM(fileName, RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M); overload;
    procedure GetPrivateFileP(fileName, RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P); overload;

    { restore download with private store space }
    function GetPrivateFile(fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString): Boolean; overload;
    function GetPrivateFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean; overload;
    procedure GetPrivateFileC(fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C); overload;
    procedure GetPrivateFileM(fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M); overload;
    procedure GetPrivateFileP(fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P); overload;
    procedure GetPrivateFileAsC(fileName, saveFileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C); overload;
    procedure GetPrivateFileAsM(fileName, saveFileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M); overload;
    procedure GetPrivateFileAsP(fileName, saveFileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P); overload;

    { normal download with custom user }
    function GetUserPrivateFile(UserID, fileName, RemoteDirectory, saveToPath: SystemString): Boolean; overload;
    function GetUserPrivateFile(UserID, fileName, saveToPath: SystemString): Boolean; overload;
    procedure GetUserPrivateFileC(UserID, fileName, RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C); overload;
    procedure GetUserPrivateFileM(UserID, fileName, RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M); overload;
    procedure GetUserPrivateFileP(UserID, fileName, RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P); overload;

    { restore download with custom user }
    function GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString): Boolean; overload;
    function GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean; overload;
    procedure GetUserPrivateFileC(UserID, fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C); overload;
    procedure GetUserPrivateFileM(UserID, fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M); overload;
    procedure GetUserPrivateFileP(UserID, fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P); overload;
    procedure GetUserPrivateFileAsC(UserID, fileName, saveFileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C); overload;
    procedure GetUserPrivateFileAsM(UserID, fileName, saveFileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M); overload;
    procedure GetUserPrivateFileAsP(UserID, fileName, saveFileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P); overload;

    { public file fragment }
    procedure GetPublicFileFragmentDataC(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_C: TFileFragmentData_C); overload;
    procedure GetPublicFileFragmentDataM(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_M: TFileFragmentData_M); overload;
    procedure GetPublicFileFragmentDataP(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_P: TFileFragmentData_P); overload;

    { Private file fragment }
    procedure GetPrivateFileFragmentDataC(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_C: TFileFragmentData_C); overload;
    procedure GetPrivateFileFragmentDataM(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_M: TFileFragmentData_M); overload;
    procedure GetPrivateFileFragmentDataP(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_P: TFileFragmentData_P); overload;

    { automated download and verify }
    procedure AutomatedDownloadPublicFileC(remoteFile, localFile: U_String; OnDownloadDone: TFileComplete_C);
    procedure AutomatedDownloadPublicFileM(remoteFile, localFile: U_String; OnDownloadDone: TFileComplete_M);
    procedure AutomatedDownloadPublicFileP(remoteFile, localFile: U_String; OnDownloadDone: TFileComplete_P);
    procedure AutomatedDownloadPrivateFileC(remoteFile, RemoteDirectory, localFile: U_String; OnDownloadDone: TFileComplete_C);
    procedure AutomatedDownloadPrivateFileM(remoteFile, RemoteDirectory, localFile: U_String; OnDownloadDone: TFileComplete_M);
    procedure AutomatedDownloadPrivateFileP(remoteFile, RemoteDirectory, localFile: U_String; OnDownloadDone: TFileComplete_P);

    { upload file to public store space }
    procedure PostFileToPublic(fileName: SystemString); overload;
    { restore upload file to public store space }
    procedure PostFileToPublic(fileName: SystemString; StartPos: Int64); overload;
    { upload file to private store space }
    procedure PostFileToPrivate(fileName, RemoteDirectory: SystemString); overload;
    procedure PostFileToPrivate(fileName: SystemString); overload;
    { restore upload file to private store space }
    procedure PostFileToPrivate(fileName, RemoteDirectory: SystemString; StartPos: Int64); overload;
    procedure PostFileToPrivate(fileName: SystemString; StartPos: Int64); overload;
    { upload stream to private store space }
    procedure PostStreamToPrivate(RemoteFileName, RemoteDirectory: SystemString; stream: TCore_Stream; doneFreeStream: Boolean); overload;
    { restore upload stream to private store space }
    procedure PostStreamToPrivate(RemoteFileName, RemoteDirectory: SystemString; stream: TCore_Stream; StartPos: Int64; doneFreeStream: Boolean); overload;

    { automated Upload and verify }
    procedure AutomatedUploadFileToPublic(localFile: U_String);
    procedure AutomatedUploadFileToPrivate(localFile, RemoteDirectory: U_String);

    { batch stream suppport }
    procedure PostBatchStream(stream: TCore_Stream; doneFreeStream: Boolean); overload;
    procedure PostBatchStreamC(stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_C); overload;
    procedure PostBatchStreamM(stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_M); overload;
    procedure PostBatchStreamP(stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_P); overload;
    procedure ClearBatchStream;
    procedure GetBatchStreamStateM(OnResult: TOnStream_M); overload;
    procedure GetBatchStreamStateM(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_M); overload;
    procedure GetBatchStreamStateP(OnResult: TOnStream_P); overload;
    procedure GetBatchStreamStateP(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_P); overload;
    function GetBatchStreamState(Result_: TDFE; TimeOut_: TTimeTick): Boolean; overload;

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;

    property LinkOk: Boolean read FLinkOk;
    property BindOk: Boolean read FLinkOk;
    property WaitCommandTimeout: Cardinal read FWaitCommandTimeout write FWaitCommandTimeout;
    property RecvFileing: Boolean read FRecvFileing;
    property RecvFileOfBatching: Boolean read FRecvFileOfBatching;
    property RecvFileName: SystemString read FRecvFileName;

    property CadencerEngine: TCadencer read FCadencerEngine;
    property ServerDelay: Double read FServerDelay;

    property ProgressEngine: TN_Progress_Tool read FProgressEngine;
    property ProgressPost: TN_Progress_Tool read FProgressEngine;
    property PostProgress: TN_Progress_Tool read FProgressEngine;
    property PostRun: TN_Progress_Tool read FProgressEngine;
    property PostExecute: TN_Progress_Tool read FProgressEngine;

    property RecvTunnel: TZNet_Client read FRecvTunnel;
    property SendTunnel: TZNet_Client read FSendTunnel;

    function RemoteInited: Boolean;
  end;

  TDT_P2PVM_OnState = record
    On_C: TOnState_C;
    On_M: TOnState_M;
    On_P: TOnState_P;
    procedure Init;
  end;

  PDT_P2PVM_OnState = ^TDT_P2PVM_OnState;

  TDT_P2PVM_Service = class(TCore_Object)
  private
    function GetQuietMode: Boolean;
    procedure SetQuietMode(const Value: Boolean);
  public
    RecvTunnel, SendTunnel: TZNet_WithP2PVM_Server;
    DTService: TDTService;
    PhysicsTunnel: TPhysicsServer;

    constructor Create(ServiceClass_: TDTServiceClass);
    destructor Destroy; override;
    procedure Progress; virtual;
    function StartService(ListenAddr, ListenPort, Auth: SystemString): Boolean;
    procedure StopService;
    property QuietMode: Boolean read GetQuietMode write SetQuietMode;
  end;

  TDT_P2PVM_Client = class;
  TDT_P2PVM_ServicePool = TGenericsList<TDT_P2PVM_Service>;
  TOn_DT_P2PVM_Client_TunnelLink = procedure(Sender: TDT_P2PVM_Client) of object;

  TDT_P2PVM_Client = class(TCore_Object)
  private
    OnConnectResultState: TDT_P2PVM_OnState;
    Connecting: Boolean;
    Reconnection: Boolean;
    procedure DoConnectionResult(const state: Boolean);
    procedure DoAutomatedP2PVMClientConnectionDone(Sender: TZNet; P_IO: TPeerIO);
    procedure DoRegisterResult(const state: Boolean);
    procedure DoLoginResult(const state: Boolean);
    procedure DoTunnelLinkResult(const state: Boolean);

    function GetQuietMode: Boolean;
    procedure SetQuietMode(const Value: Boolean);
  public
    RecvTunnel, SendTunnel: TZNet_WithP2PVM_Client;
    DTClient: TDTClient;
    PhysicsTunnel: TPhysicsClient;
    LastAddr, LastPort, LastAuth: SystemString;
    LastUser, LastPasswd: SystemString;
    RegisterUserAndLogin: Boolean;
    AutomatedConnection: Boolean;
    OnTunnelLink: TOn_DT_P2PVM_Client_TunnelLink;

    constructor Create(ClientClass_: TDTClientClass);
    destructor Destroy; override;
    procedure Progress; virtual;
    procedure Connect(addr, Port, Auth, User, passwd: SystemString);
    procedure Connect_C(addr, Port, Auth, User, passwd: SystemString; OnResult: TOnState_C);
    procedure Connect_M(addr, Port, Auth, User, passwd: SystemString; OnResult: TOnState_M);
    procedure Connect_P(addr, Port, Auth, User, passwd: SystemString; OnResult: TOnState_P);
    procedure Disconnect;
    property QuietMode: Boolean read GetQuietMode write SetQuietMode;
  end;

  TDT_P2PVM_ClientPool = TGenericsList<TDT_P2PVM_Client>;

  TDT_P2PVM_Custom_Service = class;
  TDT_P2PVM_Custom_Service_Class = class of TDT_P2PVM_Custom_Service;

  TDT_P2PVM_Custom_Service = class(TCore_InterfacedObject)
  private
    function GetQuietMode: Boolean;
    procedure SetQuietMode(const Value: Boolean);
  public
    // bind
    Bind_PhysicsTunnel: TZNet_Server;
    Bind_P2PVM_Recv_IP6: SystemString;
    Bind_P2PVM_Recv_Port: Word;
    Bind_P2PVM_Send_IP6: SystemString;
    Bind_P2PVM_Send_Port: Word;
    // local
    RecvTunnel, SendTunnel: TZNet_WithP2PVM_Server;
    DTService: TDTService;

    constructor Create(ServiceClass_: TDTServiceClass; PhysicsTunnel_: TZNet_Server;
      P2PVM_Recv_Name_, P2PVM_Recv_IP6_, P2PVM_Recv_Port_,
      P2PVM_Send_Name_, P2PVM_Send_IP6_, P2PVM_Send_Port_: SystemString); virtual;
    destructor Destroy; override;
    procedure Progress; virtual;
    procedure StartService(); virtual;
    procedure StopService(); virtual;
    property QuietMode: Boolean read GetQuietMode write SetQuietMode;
  end;

  TDT_P2PVM_Custom_ServicePool = TGenericsList<TDT_P2PVM_Custom_Service>;

  TDT_P2PVM_Custom_Client = class;
  TDT_P2PVM_Custom_Client_Class = class of TDT_P2PVM_Custom_Client;
  TOn_DT_P2PVM_Custom_Client_TunnelLink = procedure(Sender: TDT_P2PVM_Custom_Client) of object;

  TDT_P2PVM_Custom_Client_Clone_Pool_ = TBigList<TDT_P2PVM_Custom_Client>;

  TDT_P2PVM_Custom_Client_Clone_Pool = class(TDT_P2PVM_Custom_Client_Clone_Pool_)
  public
    procedure DoFree(var Data: TDT_P2PVM_Custom_Client); override;
  end;

  TDT_P2PVM_Custom_Client = class(TCore_InterfacedObject)
  private
    OnConnectResultState: TDT_P2PVM_OnState;
    Connecting: Boolean;
    Reconnection: Boolean;
    procedure DoRegisterResult(const state: Boolean);
    procedure DoLoginResult(const state: Boolean);
    function GetQuietMode: Boolean;
    procedure SetQuietMode(const Value: Boolean);
  private
    // clone Technology
    Parent_Client: TDT_P2PVM_Custom_Client;
    Clone_Instance_Ptr: TDT_P2PVM_Custom_Client_Clone_Pool_.PQueueStruct;
    procedure Do_Recv_Connect_State(const state: Boolean);
    procedure Do_Send_Connect_State(const state: Boolean);
  public
    Clone_Pool: TDT_P2PVM_Custom_Client_Clone_Pool;
    // bind
    Bind_PhysicsTunnel: TZNet_Client;
    Bind_P2PVM_Recv_IP6: SystemString;
    Bind_P2PVM_Recv_Port: Word;
    Bind_P2PVM_Send_IP6: SystemString;
    Bind_P2PVM_Send_Port: Word;
    // local
    ClientClass: TDTClientClass;
    RecvTunnel, SendTunnel: TZNet_WithP2PVM_Client;
    DTClient: TDTClient;
    LastUser, LastPasswd: SystemString;
    RegisterUserAndLogin: Boolean;
    AutomatedConnection: Boolean;
    OnTunnelLink: TOn_DT_P2PVM_Custom_Client_TunnelLink;

    constructor Create(ClientClass_: TDTClientClass; PhysicsTunnel_: TZNet_Client;
      P2PVM_Recv_Name_, P2PVM_Recv_IP6_, P2PVM_Recv_Port_, P2PVM_Send_Name_, P2PVM_Send_IP6_, P2PVM_Send_Port_: SystemString); virtual;
    constructor Create_Clone(Parent_Client_: TDT_P2PVM_Custom_Client); virtual;
    destructor Destroy; override;
    procedure Progress;
    function LoginIsSuccessed: Boolean;
    procedure DoTunnelLinkResult(const state: Boolean);
    procedure Connect(User, passwd: SystemString);
    procedure Connect_C(User, passwd: SystemString; OnResult: TOnState_C);
    procedure Connect_M(User, passwd: SystemString; OnResult: TOnState_M);
    procedure Connect_P(User, passwd: SystemString; OnResult: TOnState_P);
    procedure Disconnect;
    property QuietMode: Boolean read GetQuietMode write SetQuietMode;
  end;

  TDT_P2PVM_Custom_ClientPool = TGenericsList<TDT_P2PVM_Custom_Client>;

  PGetFileInfoStruct = ^TGetFileInfoStruct;

  TGetFileInfoStruct = record
    UserData: Pointer;
    UserObject: TCore_Object;
    fileName: SystemString;
    OnComplete_C: TGetFileInfo_C;
    OnComplete_M: TGetFileInfo_M;
    OnComplete_P: TGetFileInfo_P;
    procedure Init;
  end;

  PFileMD5Struct = ^TFileMD5Struct;

  TFileMD5Struct = record
    UserData: Pointer;
    UserObject: TCore_Object;
    fileName: SystemString;
    StartPos, EndPos: Int64;
    OnComplete_C: TFileMD5_C;
    OnComplete_M: TFileMD5_M;
    OnComplete_P: TFileMD5_P;
    procedure Init;
  end;

  PRemoteFileBackcall = ^TRemoteFileBackcall;

  TRemoteFileBackcall = record
    UserData: Pointer;
    UserObject: TCore_Object;
    OnComplete_C: TFileComplete_C;
    OnComplete_M: TFileComplete_M;
    OnComplete_P: TFileComplete_P;
    procedure Init;
  end;

  PFileFragmentDataBackcall = ^TFileFragmentDataBackcall;

  TFileFragmentDataBackcall = record
    UserData: Pointer;
    UserObject: TCore_Object;
    fileName: SystemString;
    StartPos, EndPos: Int64;
    OnComplete_C: TFileFragmentData_C;
    OnComplete_M: TFileFragmentData_M;
    OnComplete_P: TFileFragmentData_P;
  end;

  TAutomatedDownloadPublicFile_Struct = class
  private
    remoteFile, localFile: SystemString;
    OnDownloadDoneC: TFileComplete_C;
    OnDownloadDoneM: TFileComplete_M;
    OnDownloadDoneP: TFileComplete_P;
    Client: TDTClient;
    r_fileName: SystemString;
    r_fileExisted: Boolean;
    r_fileSize: Int64;
    r_fileMD5: Z.Core.TMD5;
    l_fileMD5: Z.Core.TMD5;
    procedure DoComplete(const UserData: Pointer; const UserObject: TCore_Object; stream: TCore_Stream; const fileName: SystemString);
    procedure DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCore_Object; const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
    procedure Do_Th_ComputeLFileMD5();
    procedure Done_ComputeLFileMD5();
    procedure DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCore_Object; const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: Z.Core.TMD5);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAutomatedDownloadPrivateFile_Struct = class
  private
    remoteFile, RemoteDirectory, localFile: SystemString;
    OnDownloadDoneC: TFileComplete_C;
    OnDownloadDoneM: TFileComplete_M;
    OnDownloadDoneP: TFileComplete_P;
    Client: TDTClient;
    r_fileName: SystemString;
    r_fileExisted: Boolean;
    r_fileSize: Int64;
    r_fileMD5: Z.Core.TMD5;
    l_fileMD5: Z.Core.TMD5;
    procedure DoComplete(const UserData: Pointer; const UserObject: TCore_Object; stream: TCore_Stream; const fileName: SystemString);
    procedure DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCore_Object; const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
    procedure Do_Th_ComputeLFileMD5();
    procedure Done_ComputeLFileMD5();
    procedure DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCore_Object; const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: Z.Core.TMD5);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAutomatedUploadPublicFile_Struct = class
  private
    localFile: SystemString;
    Client: TDTClient;
    r_fileName: SystemString;
    r_fileExisted: Boolean;
    r_fileSize: Int64;
    r_fileMD5: Z.Core.TMD5;
    l_file_StartPos, l_file_EndPos: Int64;
    l_fileMD5: Z.Core.TMD5;
    procedure DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCore_Object; const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
    procedure Do_Th_ComputeLFileMD5();
    procedure Done_ComputeLFileMD5();
    procedure DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCore_Object; const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: Z.Core.TMD5);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAutomatedUploadPrivateFile_Struct = class
  private
    localFile, RemoteDirectory: SystemString;
    Client: TDTClient;
    r_fileName: SystemString;
    r_fileExisted: Boolean;
    r_fileSize: Int64;
    r_fileMD5: Z.Core.TMD5;
    l_file_StartPos, l_file_EndPos: Int64;
    l_fileMD5: Z.Core.TMD5;
    procedure DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCore_Object; const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
    procedure Do_Th_ComputeLFileMD5();
    procedure Done_ComputeLFileMD5();
    procedure DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCore_Object; const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: Z.Core.TMD5);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TZNet_DoubleTunnelService = TDTService;
  TZNet_DoubleTunnelClient = TDTClient;

implementation

uses SysUtils;

procedure TGetFileInfoStruct.Init;
begin
  UserData := nil;
  UserObject := nil;
  fileName := '';
  OnComplete_C := nil;
  OnComplete_M := nil;
  OnComplete_P := nil;
end;

procedure TFileMD5Struct.Init;
begin
  UserData := nil;
  UserObject := nil;
  fileName := '';
  StartPos := 0;
  EndPos := 0;
  OnComplete_C := nil;
  OnComplete_M := nil;
  OnComplete_P := nil;
end;

procedure TRemoteFileBackcall.Init;
begin
  UserData := nil;
  UserObject := nil;
  OnComplete_C := nil;
  OnComplete_M := nil;
  OnComplete_P := nil;
end;

procedure TAutomatedDownloadPublicFile_Struct.DoComplete(const UserData: Pointer; const UserObject: TCore_Object; stream: TCore_Stream; const fileName: SystemString);
begin
  try
    if Assigned(OnDownloadDoneC) then
        OnDownloadDoneC(UserData, UserObject, stream, fileName);
    if Assigned(OnDownloadDoneM) then
        OnDownloadDoneM(UserData, UserObject, stream, fileName);
    if Assigned(OnDownloadDoneP) then
        OnDownloadDoneP(UserData, UserObject, stream, fileName);
  except
  end;
  DelayFreeObj(1.0, Self);
end;

procedure TAutomatedDownloadPublicFile_Struct.DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCore_Object;
  const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
begin
  if Existed then
    begin
      r_fileName := fileName;
      r_fileSize := fSiz;
      if not umlFileExists(localFile) then
          Client.GetPublicFileAsM(remoteFile, umlGetFileName(localFile), 0, umlGetFilePath(localFile), nil, nil, DoComplete)
      else if fSiz >= umlGetFileSize(localFile) then
        begin
          umlCacheFileMD5(localFile);
          Client.GetPublicFileMD5M(umlGetFileName(remoteFile), 0, umlGetFileSize(localFile), nil, nil, DoResult_GetFileMD5);
        end
      else
          Client.GetPublicFileAsM(remoteFile, umlGetFileName(localFile), 0, umlGetFilePath(localFile), nil, nil, DoComplete);
    end
  else
    begin
      DoStatus('no found remote file: "%s" ', [remoteFile]);
      DelayFreeObj(1.0, Self);
    end;
end;

procedure TAutomatedDownloadPublicFile_Struct.Do_Th_ComputeLFileMD5;
begin
  DoStatus('compute md5 from local "%s"', [localFile]);
  l_fileMD5 := umlFileMD5(localFile);
  TCompute.PostM1(Done_ComputeLFileMD5);
end;

procedure TAutomatedDownloadPublicFile_Struct.Done_ComputeLFileMD5;
begin
  if umlMD5Compare(l_fileMD5, r_fileMD5) then
    begin
      if r_fileSize = umlGetFileSize(localFile) then
          DoComplete(nil, nil, nil, localFile)
      else
          Client.GetPublicFileAsM(r_fileName, umlGetFileName(localFile), umlGetFileSize(localFile), umlGetFilePath(localFile), nil, nil, DoComplete);
    end
  else
      Client.GetPublicFileAsM(r_fileName, umlGetFileName(localFile), 0, umlGetFilePath(localFile), nil, nil, DoComplete);
end;

procedure TAutomatedDownloadPublicFile_Struct.DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCore_Object;
  const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: Z.Core.TMD5);
begin
  r_fileMD5 := MD5;
  TCompute.RunM_NP(Do_Th_ComputeLFileMD5);
end;

constructor TAutomatedDownloadPublicFile_Struct.Create;
begin
  inherited Create;
  remoteFile := '';
  localFile := '';
  OnDownloadDoneC := nil;
  OnDownloadDoneM := nil;
  OnDownloadDoneP := nil;
  Client := nil;
  r_fileName := '';
  r_fileExisted := False;
  r_fileSize := -1;
  r_fileMD5 := NullMD5;
  l_fileMD5 := NullMD5;
end;

destructor TAutomatedDownloadPublicFile_Struct.Destroy;
begin
  remoteFile := '';
  localFile := '';
  r_fileName := '';
  inherited Destroy;
end;

procedure TAutomatedDownloadPrivateFile_Struct.DoComplete(const UserData: Pointer; const UserObject: TCore_Object; stream: TCore_Stream; const fileName: SystemString);
begin
  try
    if Assigned(OnDownloadDoneC) then
        OnDownloadDoneC(UserData, UserObject, stream, fileName);
    if Assigned(OnDownloadDoneM) then
        OnDownloadDoneM(UserData, UserObject, stream, fileName);
    if Assigned(OnDownloadDoneP) then
        OnDownloadDoneP(UserData, UserObject, stream, fileName);
  except
  end;
  DelayFreeObj(1.0, Self);
end;

procedure TAutomatedDownloadPrivateFile_Struct.DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCore_Object;
  const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
begin
  if Existed then
    begin
      r_fileName := fileName;
      r_fileSize := fSiz;
      if not umlFileExists(localFile) then
          Client.GetPrivateFileAsM(remoteFile, umlGetFileName(localFile), 0, RemoteDirectory, umlGetFilePath(localFile), nil, nil, DoComplete)
      else if fSiz >= umlGetFileSize(localFile) then
          Client.GetPrivateFileMD5M(umlGetFileName(remoteFile), RemoteDirectory, 0, umlGetFileSize(localFile), nil, nil, DoResult_GetFileMD5)
      else
          Client.GetPrivateFileAsM(remoteFile, umlGetFileName(localFile), 0, RemoteDirectory, umlGetFilePath(localFile), nil, nil, DoComplete);
    end
  else
    begin
      DoStatus('no found remote file: "%s" ', [remoteFile]);
      DelayFreeObj(1.0, Self);
    end;
end;

procedure TAutomatedDownloadPrivateFile_Struct.Do_Th_ComputeLFileMD5;
begin
  DoStatus('compute md5 from local "%s"', [localFile]);
  l_fileMD5 := umlFileMD5(localFile);
  TCompute.PostM1(Done_ComputeLFileMD5);
end;

procedure TAutomatedDownloadPrivateFile_Struct.Done_ComputeLFileMD5;
begin
  if umlMD5Compare(l_fileMD5, r_fileMD5) then
    begin
      if r_fileSize = umlGetFileSize(localFile) then
          DoComplete(nil, nil, nil, localFile)
      else
          Client.GetPrivateFileAsM(r_fileName, umlGetFileName(localFile), umlGetFileSize(localFile), RemoteDirectory, umlGetFilePath(localFile), nil, nil, DoComplete);
    end
  else
      Client.GetPrivateFileAsM(r_fileName, umlGetFileName(localFile), 0, RemoteDirectory, umlGetFilePath(localFile), nil, nil, DoComplete);
end;

procedure TAutomatedDownloadPrivateFile_Struct.DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCore_Object;
  const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: Z.Core.TMD5);
begin
  r_fileMD5 := MD5;
  TCompute.RunM_NP(Do_Th_ComputeLFileMD5);
end;

constructor TAutomatedDownloadPrivateFile_Struct.Create;
begin
  inherited Create;
  remoteFile := '';
  RemoteDirectory := '';
  localFile := '';
  OnDownloadDoneC := nil;
  OnDownloadDoneM := nil;
  OnDownloadDoneP := nil;
  Client := nil;
  r_fileName := '';
  r_fileExisted := False;
  r_fileSize := -1;
  r_fileMD5 := NullMD5;
  l_fileMD5 := NullMD5;
end;

destructor TAutomatedDownloadPrivateFile_Struct.Destroy;
begin
  remoteFile := '';
  RemoteDirectory := '';
  localFile := '';
  r_fileName := '';
  inherited Destroy;
end;

procedure TAutomatedUploadPublicFile_Struct.DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCore_Object;
  const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
begin
  r_fileExisted := Existed;

  if Existed then
    begin
      r_fileName := fileName;
      r_fileSize := fSiz;
      if r_fileSize <= umlGetFileSize(localFile) then
          Client.GetPublicFileMD5M(umlGetFileName(localFile), 0, r_fileSize, nil, nil, DoResult_GetFileMD5)
      else
        begin
          Client.PostFileToPublic(localFile);
          DelayFreeObj(1.0, Self);
        end;
    end
  else
    begin
      Client.PostFileToPublic(localFile);
      DelayFreeObj(1.0, Self);
    end;
end;

procedure TAutomatedUploadPublicFile_Struct.Do_Th_ComputeLFileMD5;
begin
  DoStatus('compute md5 from local "%s"', [localFile]);
  l_fileMD5 := umlFileMD5(localFile, l_file_StartPos, l_file_EndPos);
  TCompute.PostM1(Done_ComputeLFileMD5);
end;

procedure TAutomatedUploadPublicFile_Struct.Done_ComputeLFileMD5;
begin
  if umlMD5Compare(r_fileMD5, l_fileMD5) then
    begin
      if umlGetFileSize(localFile) > r_fileSize then
          Client.PostFileToPublic(localFile, r_fileSize);
    end
  else
      Client.PostFileToPublic(localFile);
  DelayFreeObj(1.0, Self);
end;

procedure TAutomatedUploadPublicFile_Struct.DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCore_Object;
  const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: Z.Core.TMD5);
begin
  r_fileMD5 := MD5;
  l_file_StartPos := StartPos;
  l_file_EndPos := EndPos;
  TCompute.RunM_NP(Do_Th_ComputeLFileMD5);
end;

constructor TAutomatedUploadPublicFile_Struct.Create;
begin
  inherited Create;
  localFile := '';
  Client := nil;
  r_fileName := '';
  r_fileExisted := False;
  r_fileSize := -1;
  r_fileMD5 := NullMD5;

  l_file_StartPos := 0;
  l_file_EndPos := 0;
  l_fileMD5 := NullMD5;
end;

destructor TAutomatedUploadPublicFile_Struct.Destroy;
begin
  localFile := '';
  r_fileName := '';
  inherited Destroy;
end;

procedure TAutomatedUploadPrivateFile_Struct.DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCore_Object;
  const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
begin
  r_fileExisted := Existed;

  if Existed then
    begin
      r_fileName := fileName;
      r_fileSize := fSiz;
      if r_fileSize <= umlGetFileSize(localFile) then
          Client.GetPrivateFileMD5M(umlGetFileName(localFile), RemoteDirectory, 0, r_fileSize, nil, nil, DoResult_GetFileMD5)
      else
        begin
          Client.PostFileToPrivate(localFile, RemoteDirectory);
          DelayFreeObj(1.0, Self);
        end;
    end
  else
    begin
      Client.PostFileToPrivate(localFile, RemoteDirectory);
      DelayFreeObj(1.0, Self);
    end;
end;

procedure TAutomatedUploadPrivateFile_Struct.Do_Th_ComputeLFileMD5;
begin
  DoStatus('compute md5 from local "%s"', [localFile]);
  l_fileMD5 := umlFileMD5(localFile, l_file_StartPos, l_file_EndPos);
  TCompute.PostM1(Done_ComputeLFileMD5);
end;

procedure TAutomatedUploadPrivateFile_Struct.Done_ComputeLFileMD5;
begin
  if umlMD5Compare(r_fileMD5, l_fileMD5) then
    begin
      if umlGetFileSize(localFile) > r_fileSize then
          Client.PostFileToPrivate(localFile, RemoteDirectory, r_fileSize);
    end
  else
      Client.PostFileToPrivate(localFile, RemoteDirectory);
  DelayFreeObj(1.0, Self);
end;

procedure TAutomatedUploadPrivateFile_Struct.DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCore_Object;
  const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: Z.Core.TMD5);
begin
  r_fileMD5 := MD5;
  l_file_StartPos := StartPos;
  l_file_EndPos := EndPos;
  TCompute.RunM_NP(Do_Th_ComputeLFileMD5);
end;

constructor TAutomatedUploadPrivateFile_Struct.Create;
begin
  inherited Create;
  localFile := '';
  RemoteDirectory := '';
  Client := nil;
  r_fileName := '';
  r_fileExisted := False;
  r_fileSize := -1;
  r_fileMD5 := NullMD5;

  l_file_StartPos := 0;
  l_file_EndPos := 0;
  l_fileMD5 := NullMD5;
end;

destructor TAutomatedUploadPrivateFile_Struct.Destroy;
begin
  localFile := '';
  RemoteDirectory := '';
  r_fileName := '';
  inherited Destroy;
end;

constructor TService_SendTunnel_UserDefine.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  RecvTunnel := nil;
  RecvTunnelID := 0;
  DoubleTunnelService := nil;
end;

destructor TService_SendTunnel_UserDefine.Destroy;
begin
  if (DoubleTunnelService <> nil) and (RecvTunnelID > 0) and (RecvTunnel <> nil) then
    begin
      if DoubleTunnelService.FRecvTunnel.Exists(RecvTunnelID) then
          DoubleTunnelService.FRecvTunnel.PeerIO[RecvTunnelID].Disconnect;
    end;
  inherited Destroy;
end;

function TService_SendTunnel_UserDefine.LinkOk: Boolean;
begin
  Result := DoubleTunnelService <> nil;
end;

constructor TService_RecvTunnel_UserDefine.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  SendTunnel := nil;
  SendTunnelID := 0;
  UserFlag := '';
  UserID := '';
  UserPath := '';
  UserConfigFile := THashTextEngine.Create;
  DoubleTunnelService := nil;
  UserDBIntf := nil;
  LoginSuccessed := False;
  WaitLink := True;
  WaitLinkSendID := 0;
  FCurrentFileStream := nil;
  FCurrentReceiveFileName := '';
end;

destructor TService_RecvTunnel_UserDefine.Destroy;
begin
  if LoginSuccessed then
    begin
      if (DoubleTunnelService <> nil) then
        begin
          SaveConfigFile;
          DoubleTunnelService.UserOut(Self);
        end;

      if (DoubleTunnelService <> nil) and (SendTunnelID > 0) and (SendTunnel <> nil) then
        begin
          if DoubleTunnelService.FSendTunnel.Exists(SendTunnelID) then
              DoubleTunnelService.FSendTunnel.PeerIO[SendTunnelID].Disconnect;
        end;
    end;

  try
      DisposeObject(UserConfigFile);
  except
  end;

  if FCurrentFileStream <> nil then
    begin
      DisposeObject(FCurrentFileStream);
      FCurrentFileStream := nil;
    end;

  inherited Destroy;
end;

function TService_RecvTunnel_UserDefine.MakeFilePath(fn: SystemString): SystemString;
begin
  Result := umlCombineFileName(UserPath, fn);
end;

function TService_RecvTunnel_UserDefine.GetUserID: SystemString;
begin
  Result := UserConfigFile.GetDefaultValue('UserInfo', 'UserID', '');
end;

procedure TService_RecvTunnel_UserDefine.SaveConfigFile;
begin
  UserConfigFile.SaveToFile(MakeFilePath('User.Config'));
end;

function TService_RecvTunnel_UserDefine.LinkOk: Boolean;
begin
  Result := SendTunnel <> nil;
end;

procedure TDTService.UserRegistedSuccess(UserID: SystemString);
begin
end;

procedure TDTService.UserLoginSuccess(UserDefineIO: TService_RecvTunnel_UserDefine);
begin
end;

procedure TDTService.UserLinkSuccess(UserDefineIO: TService_RecvTunnel_UserDefine);
begin
  try
    if Assigned(FOnLinkSuccess) then
        FOnLinkSuccess(Self, UserDefineIO);
  except
  end;
end;

procedure TDTService.UserCreateDirectorySuccess(UserDefineIO: TService_RecvTunnel_UserDefine; dn: SystemString);
begin
end;

procedure TDTService.UserPostFileSuccess(UserDefineIO: TService_RecvTunnel_UserDefine; fn: SystemString);
begin
end;

procedure TDTService.UserOut(UserDefineIO: TService_RecvTunnel_UserDefine);
begin
  try
    if Assigned(FOnUserOut) then
        FOnUserOut(Self, UserDefineIO);
  except
  end;
end;

procedure TDTService.Command_UserLogin(Sender: TPeerIO; InData, OutData: TDFE);
var
  SendTunnelID: Cardinal;
  UserID, UserPasswd: SystemString;
  UserDefineIO: TService_RecvTunnel_UserDefine;
begin
  SendTunnelID := InData.Reader.ReadCardinal;
  UserID := InData.Reader.ReadString;
  UserPasswd := InData.Reader.ReadString;

  UserDefineIO := GetUserDefineRecvTunnel(Sender);

  if not FSendTunnel.Exists(SendTunnelID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('send tunnel Illegal:%d', [SendTunnelID]));
      Exit;
    end;

  if not FUserDB.Exists(UserID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('user name Invalid:%s', [UserID]));
      Exit;
    end;

  try
    if not CompareQuantumCryptographyPassword(UserPasswd, SystemString(FUserDB.GetDefaultValue(UserID, 'password', ''))) then
      begin
        OutData.WriteBool(False);
        OutData.WriteString(PFormat('password error', []));
        Exit;
      end;
  except
    OutData.WriteBool(False);
    OutData.WriteString(PFormat('password error', []));
    Exit;
  end;

  UserDefineIO.UserDBIntf := FUserDB.VariantList[UserID];
  UserDefineIO.UserDBIntf['LastLoginTime'] := DateTimeToStr(Now);
  UserDefineIO.UserFlag := UserDefineIO.UserDBIntf['UserFlag'];
  UserDefineIO.UserID := UserID;
  UserDefineIO.UserPath := umlCombinePath(FRootPath, UserDefineIO.UserFlag);
  UserDefineIO.DoubleTunnelService := Self;
  UserDefineIO.LoginSuccessed := True;

  if umlFileExists(UserDefineIO.MakeFilePath('User.Config')) then
      UserDefineIO.UserConfigFile.LoadFromFile(UserDefineIO.MakeFilePath('User.Config'));

  UserDefineIO.UserConfigFile.Hit['UserInfo', 'UserID'] := UserID;
  UserDefineIO.UserConfigFile.Hit['UserInfo', 'Password'] := SystemString(FUserDB.GetDefaultValue(UserID, 'password', ''));

  UserDefineIO.WaitLink := True;
  UserDefineIO.WaitLinkSendID := SendTunnelID;

  OutData.WriteBool(True);
  OutData.WriteString(PFormat('success Login:%s', [UserID]));
  OutData.WriteString(UserDefineIO.UserFlag);

  UserLoginSuccess(UserDefineIO);
end;

procedure TDTService.Command_RegisterUser(Sender: TPeerIO; InData, OutData: TDFE);
var
  SendTunnelID: Cardinal;
  UserID, UserPasswd: SystemString;
  UserDefineIO: TService_RecvTunnel_UserDefine;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);

  if not FAllowRegisterNewUser then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('disable user register in server', []));
      Exit;
    end;

  SendTunnelID := InData.Reader.ReadCardinal;
  UserID := InData.Reader.ReadString;
  UserPasswd := InData.Reader.ReadString;

  if not FSendTunnel.Exists(SendTunnelID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('send tunnel Illegal:%d', [SendTunnelID]));
      Exit;
    end;

  if umlExistsChar(UserID, '[]:'#13#10#9#8#0) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('user name Illegal:%s', [UserID]));
      Exit;
    end;

  if FUserDB.Exists(UserID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('user already registed:%s', [UserID]));
      Exit;
    end;

  UserDefineIO.UserFlag := MakeUserFlag;
  UserDefineIO.UserID := UserID;
  UserDefineIO.UserPath := umlCombinePath(FRootPath, UserDefineIO.UserFlag);
  umlCreateDirectory(UserDefineIO.UserPath);
  UserDefineIO.UserDBIntf := FUserDB.VariantList[UserID];
  UserDefineIO.UserDBIntf['UserFlag'] := UserDefineIO.UserFlag;
  UserDefineIO.UserDBIntf['password'] := GenerateQuantumCryptographyPassword(UserPasswd).Text;
  UserDefineIO.UserDBIntf['RegTime'] := DateTimeToStr(Now);
  UserDefineIO.DoubleTunnelService := Self;
  UserDefineIO.LoginSuccessed := True;

  UserDefineIO.UserConfigFile.Hit['UserInfo', 'UserID'] := UserID;
  UserDefineIO.UserConfigFile.Hit['UserInfo', 'Password'] := UserDefineIO.UserDBIntf['password'];
  UserDefineIO.SaveConfigFile;

  if FAllowSaveUserInfo then
      SaveUserDB;

  UserDefineIO.WaitLink := True;
  UserDefineIO.WaitLinkSendID := SendTunnelID;

  OutData.WriteBool(True);
  OutData.WriteString(PFormat('success registed:%s', [UserID]));
  OutData.WriteString(UserDefineIO.UserFlag);

  UserDefineIO.SaveConfigFile;
  UserRegistedSuccess(UserID);
end;

procedure TDTService.Command_TunnelLink(Sender: TPeerIO; InData, OutData: TDFE);
var
  RecvID, SendID: Cardinal;
  UserDefineIO: TService_RecvTunnel_UserDefine;
begin
  RecvID := InData.Reader.ReadCardinal;
  SendID := InData.Reader.ReadCardinal;

  UserDefineIO := GetUserDefineRecvTunnel(Sender);

  if not UserDefineIO.LoginSuccessed then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('need login or register', []));
      OutData.WriteBool(FFileSystem);
      Exit;
    end;

  if not FSendTunnel.Exists(SendID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('send tunnel Illegal:%d', [SendID]));
      OutData.WriteBool(FFileSystem);
      Exit;
    end;

  if not FRecvTunnel.Exists(RecvID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('received tunnel Illegal:%d', [RecvID]));
      OutData.WriteBool(FFileSystem);
      Exit;
    end;

  if Sender.ID <> RecvID then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('received tunnel Illegal:%d-%d', [Sender.ID, RecvID]));
      OutData.WriteBool(FFileSystem);
      Exit;
    end;

  UserDefineIO.SendTunnel := FSendTunnel.PeerIO[SendID].UserDefine as TService_SendTunnel_UserDefine;
  UserDefineIO.SendTunnelID := SendID;
  UserDefineIO.SendTunnel.RecvTunnel := UserDefineIO;
  UserDefineIO.SendTunnel.RecvTunnelID := RecvID;
  UserDefineIO.SendTunnel.DoubleTunnelService := Self;

  OutData.WriteBool(True);
  OutData.WriteString(PFormat('tunnel link success! received:%d <-> send:%d', [RecvID, SendID]));
  OutData.WriteBool(FFileSystem);

  UserLinkSuccess(UserDefineIO);
end;

procedure TDTService.Command_ChangePasswd(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  oldPasswd, newPasswd: SystemString;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  oldPasswd := InData.Reader.ReadString;
  newPasswd := InData.Reader.ReadString;

  try
    if not CompareQuantumCryptographyPassword(oldPasswd, SystemString(FUserDB.GetDefaultValue(UserDefineIO.UserID, 'password', ''))) then
      begin
        OutData.WriteBool(False);
        OutData.WriteString(PFormat('password error', []));
        Exit;
      end;
  except
    OutData.WriteBool(False);
    OutData.WriteString(PFormat('password error', []));
    Exit;
  end;

  UserDefineIO.UserDBIntf['password'] := GenerateQuantumCryptographyPassword(newPasswd).Text;
  if FAllowSaveUserInfo then
      SaveUserDB;

  OutData.WriteBool(True);
  OutData.WriteString(PFormat('password change success', []));
end;

procedure TDTService.Command_CustomNewUser(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  UserID, passwd: SystemString;
  UserConfig: THashTextEngine;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  UserID := InData.Reader.ReadString;
  passwd := InData.Reader.ReadString;
  UserConfig := THashTextEngine.Create;
  InData.Reader.ReadSectionText(UserConfig);

  OutData.WriteBool(RegUser(UserID, passwd, UserConfig));

  DisposeObject(UserConfig);
end;

procedure TDTService.Command_ProcessStoreQueueCMD(Sender: TPeerIO; InData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  fs: U_StringArray;
  fn: SystemString;
  Cmd: SystemString;
  de, de2: TDFE;
  stream: TCore_FileStream;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  fs := umlGet_File_Full_Array(UserDefineIO.UserPath);

  for fn in fs do
    begin
      if umlMultipleMatch(True, '*.queue', fn) then
        begin
          try
            de := TDFE.Create;
            stream := TCore_FileStream.Create(fn, fmOpenRead or fmShareDenyNone);
            de.DecodeFrom(stream);
            DisposeObject(stream);
            Cmd := de.Reader.ReadString;
            de2 := TDFE.Create;
            de.Reader.ReadDataFrame(de2);
            FSendTunnel.SendDirectStreamCmd(UserDefineIO.SendTunnel.Owner, Cmd, de2);
            DisposeObject(de2);
            DisposeObject(de);
          except
          end;
          umlDeleteFile(fn);
        end;
    end;
end;

procedure TDTService.Command_GetPublicFileList(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  Filter: SystemString;
  fs: U_StringArray;
  i: Integer;
  n: SystemString;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  Filter := InData.Reader.ReadString;

  fs := umlGet_File_Full_Array(FPublicPath);
  for i := low(fs) to high(fs) do
    begin
      n := umlGetFileName(fs[i]);
      if umlMultipleMatch(Filter, n) then
          OutData.WriteString(n);
    end;
end;

procedure TDTService.Command_GetPrivateFileList(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  Filter, dn: SystemString;
  fs: U_StringArray;
  i: Integer;
  n: SystemString;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  Filter := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;

  fs := umlGet_File_Full_Array(umlCombinePath(UserDefineIO.UserPath, dn));
  for i := low(fs) to high(fs) do
    begin
      n := umlGetFileName(fs[i]);
      if umlMultipleMatch(Filter, n) then
          OutData.WriteString(n);
    end;
end;

procedure TDTService.Command_GetPrivateDirectoryList(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  Filter, dn: SystemString;
  fs: U_StringArray;
  i: Integer;
  n: SystemString;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  Filter := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;

  fs := umlGet_Path_Full_Array(umlCombinePath(UserDefineIO.UserPath, dn));
  for i := low(fs) to high(fs) do
    begin
      n := umlGetFileName(fs[i]);
      if umlMultipleMatch(Filter, n) then
          OutData.WriteString(n);
    end;
end;

procedure TDTService.Command_CreatePrivateDirectory(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  dn, fulldn: SystemString;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  dn := InData.Reader.ReadString;

  fulldn := umlCombinePath(UserDefineIO.UserPath, dn);

  try
    if not umlDirectoryExists(fulldn) then
      begin
        umlCreateDirectory(fulldn);
        OutData.WriteBool(umlDirectoryExists(fulldn));
        UserCreateDirectorySuccess(UserDefineIO, fulldn);
      end
    else
      begin
        OutData.WriteBool(True);
      end;
  except
      OutData.WriteBool(False);
  end;
end;

procedure TDTService.Command_GetPublicFileInfo(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  fullfn, fileName: SystemString;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;
  fileName := InData.Reader.ReadString;

  fullfn := umlCombineFileName(FPublicPath, fileName);
  if umlFileExists(fullfn) then
    begin
      OutData.WriteBool(True);
      OutData.WriteInt64(umlGetFileSize(fullfn));
    end
  else
    begin
      OutData.WriteBool(False);
      OutData.WriteInt64(0);
    end;
end;

procedure TDTService.Command_GetPrivateFileInfo(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  fullfn, fileName, dn: SystemString;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;
  fileName := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;

  fullfn := umlCombineFileName(umlCombinePath(UserDefineIO.UserPath, dn), fileName);
  if umlFileExists(fullfn) then
    begin
      OutData.WriteBool(True);
      OutData.WriteInt64(umlGetFileSize(fullfn));
    end
  else
    begin
      OutData.WriteBool(False);
      OutData.WriteInt64(0);
    end;
end;

procedure TDTService.Do_Th_Command_GetPublicFileMD5(ThSender: THPC_Stream; ThInData, ThOutData: TDFE);
var
  fullfn, fileName: SystemString;
  StartPos, EndPos: Int64;
  fs: TCore_FileStream;
  MD5: TMD5;
begin
  fileName := ThInData.Reader.ReadString;
  StartPos := ThInData.Reader.ReadInt64;
  EndPos := ThInData.Reader.ReadInt64;

  fullfn := umlCombineFileName(FPublicPath, fileName);
  if not umlFileExists(fullfn) then
    begin
      ThOutData.WriteBool(False);
      Exit;
    end;

  try
      fs := TCore_FileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
    ThOutData.WriteBool(False);
    Exit;
  end;

  if (EndPos > fs.Size) then
      EndPos := fs.Size;

  if ((EndPos = StartPos) or (EndPos = 0)) or ((StartPos = 0) and (EndPos = fs.Size)) then
      MD5 := umlFileMD5(fullfn)
  else
      MD5 := umlStreamMD5(fs, StartPos, EndPos);

  ThOutData.WriteBool(True);
  ThOutData.WriteMD5(MD5);
  DisposeObject(fs);
end;

procedure TDTService.Command_GetPublicFileMD5(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;
  RunHPC_StreamM(Sender, nil, nil, InData, OutData, Do_Th_Command_GetPublicFileMD5);
end;

procedure TDTService.Do_Th_Command_GetPrivateFileMD5(ThSender: THPC_Stream; ThInData, ThOutData: TDFE);
var
  fullfn, fileName, dn: SystemString;
  StartPos, EndPos: Int64;
  fs: TCore_FileStream;
  MD5: TMD5;

{$IFDEF FPC}
  procedure do_fpc_sync();
  var
    UserDefineIO: TService_RecvTunnel_UserDefine;
  begin
    UserDefineIO := GetUserDefineRecvTunnel(ThSender.IO);
    fullfn := umlCombineFileName(umlCombinePath(UserDefineIO.UserPath, dn), fileName);
  end;
{$ENDIF FPC}


begin
  fullfn := '';
  fileName := ThInData.Reader.ReadString;
  dn := ThInData.Reader.ReadString;
  StartPos := ThInData.Reader.ReadInt64;
  EndPos := ThInData.Reader.ReadInt64;

{$IFDEF FPC}
  TCompute.Sync(do_fpc_sync);
{$ELSE FPC}
  TCompute.Sync(procedure
    var
      UserDefineIO: TService_RecvTunnel_UserDefine;
    begin
      if not ThSender.IsOnline then
          Exit;
      UserDefineIO := GetUserDefineRecvTunnel(ThSender.IO);
      if UserDefineIO <> nil then
          fullfn := umlCombineFileName(umlCombinePath(UserDefineIO.UserPath, dn), fileName);
    end);
{$ENDIF FPC}
  if not umlFileExists(fullfn) then
    begin
      ThOutData.WriteBool(False);
      Exit;
    end;

  try
      fs := TCore_FileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
    ThOutData.WriteBool(False);
    Exit;
  end;

  if (EndPos > fs.Size) then
      EndPos := fs.Size;

  if ((EndPos = StartPos) or (EndPos = 0)) or ((StartPos = 0) and (EndPos = fs.Size)) then
      MD5 := umlFileMD5(fullfn)
  else
      MD5 := umlStreamMD5(fs, StartPos, EndPos);

  ThOutData.WriteBool(True);
  ThOutData.WriteMD5(MD5);
  DisposeObject(fs);
end;

procedure TDTService.Command_GetPrivateFileMD5(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;
  RunHPC_StreamM(Sender, nil, nil, InData, OutData, Do_Th_Command_GetPrivateFileMD5);
end;

procedure TDTService.Command_GetPublicFile(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  fullfn, fileName, remoteinfo: SystemString;
  StartPos: Int64;
  RemoteBackcallAddr: UInt64;
  sendDE: TDFE;
  fs: TCore_FileStream;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  fileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(FPublicPath, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('filename invailed %s', [fileName]));
      Exit;
    end;

  try
      fs := TCore_FileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
      Exit;
  end;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_FileInfo, sendDE);
  DisposeObject(sendDE);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDFE.Create;
  sendDE.WritePointer(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);

  OutData.WriteBool(True);
  OutData.WriteString(PFormat('post %s to send tunnel', [fileName]));
end;

procedure TDTService.Command_GetPrivateFile(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  fullfn, fileName, dn, remoteinfo: SystemString;
  StartPos: Int64;
  RemoteBackcallAddr: UInt64;
  sendDE: TDFE;
  fs: TCore_FileStream;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  fileName := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(umlCombinePath(UserDefineIO.UserPath, dn), fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('filename invailed %s', [fileName]));
      Exit;
    end;

  try
      fs := TCore_FileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
      Exit;
  end;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_FileInfo, sendDE);
  DisposeObject(sendDE);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDFE.Create;
  sendDE.WritePointer(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);

  OutData.WriteBool(True);
  OutData.WriteString(PFormat('post %s to send tunnel', [fileName]));
end;

procedure TDTService.Command_GetUserPrivateFile(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  fullfn, UserID, fileName, dn, remoteinfo: SystemString;
  StartPos: Int64;
  RemoteBackcallAddr: UInt64;
  sendDE: TDFE;
  fs: TCore_FileStream;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  UserID := InData.Reader.ReadString;
  fileName := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  if not ExistsUser(UserID) then
      Exit;

  fullfn := umlCombineFileName(umlCombinePath(GetUserPath(UserID), dn), fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('filename invailed %s', [fileName]));
      Exit;
    end;

  try
      fs := TCore_FileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
      Exit;
  end;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_FileInfo, sendDE);
  DisposeObject(sendDE);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDFE.Create;
  sendDE.WritePointer(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);

  OutData.WriteBool(True);
  OutData.WriteString(PFormat('post %s to send tunnel', [fileName]));
end;

procedure TDTService.Command_GetPublicFileAs(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  fullfn, fileName, saveFileName, remoteinfo: SystemString;
  StartPos: Int64;
  RemoteBackcallAddr: UInt64;
  sendDE: TDFE;
  fs: TCore_FileStream;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  fileName := InData.Reader.ReadString;
  saveFileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(FPublicPath, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('filename invailed %s', [fileName]));
      Exit;
    end;

  try
      fs := TCore_FileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
      Exit;
  end;

  sendDE := TDFE.Create;
  sendDE.WriteString(saveFileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_FileInfo, sendDE);
  DisposeObject(sendDE);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDFE.Create;
  sendDE.WritePointer(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);

  OutData.WriteBool(True);
  OutData.WriteString(PFormat('post %s to send tunnel', [fileName]));
end;

procedure TDTService.Command_GetPrivateFileAs(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  fullfn, fileName, saveFileName, dn, remoteinfo: SystemString;
  StartPos: Int64;
  RemoteBackcallAddr: UInt64;
  sendDE: TDFE;
  fs: TCore_FileStream;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  fileName := InData.Reader.ReadString;
  saveFileName := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(umlCombinePath(UserDefineIO.UserPath, dn), fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('filename invailed %s', [fileName]));
      Exit;
    end;

  try
      fs := TCore_FileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
      Exit;
  end;

  sendDE := TDFE.Create;
  sendDE.WriteString(saveFileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_FileInfo, sendDE);
  DisposeObject(sendDE);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDFE.Create;
  sendDE.WritePointer(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);

  OutData.WriteBool(True);
  OutData.WriteString(PFormat('post %s to send tunnel', [fileName]));
end;

procedure TDTService.Command_GetUserPrivateFileAs(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  fullfn, UserID, fileName, saveFileName, dn, remoteinfo: SystemString;
  StartPos: Int64;
  RemoteBackcallAddr: UInt64;
  sendDE: TDFE;
  fs: TCore_FileStream;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  UserID := InData.Reader.ReadString;
  fileName := InData.Reader.ReadString;
  saveFileName := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  if not ExistsUser(UserID) then
      Exit;

  fullfn := umlCombineFileName(umlCombinePath(GetUserPath(UserID), dn), fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('filename invailed %s', [fileName]));
      Exit;
    end;

  try
      fs := TCore_FileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
      Exit;
  end;

  sendDE := TDFE.Create;
  sendDE.WriteString(saveFileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_FileInfo, sendDE);
  DisposeObject(sendDE);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDFE.Create;
  sendDE.WritePointer(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);

  OutData.WriteBool(True);
  OutData.WriteString(PFormat('post %s to send tunnel', [fileName]));
end;

procedure TDTService.Command_PostPublicFileInfo(Sender: TPeerIO; InData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  fn: SystemString;
  StartPos: Int64;
  FSize: Int64;
  fullfn: SystemString;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
    begin
      Sender.DelayClose();
      Exit;
    end;
  if UserDefineIO.SendTunnel = nil then
    begin
      Sender.DelayClose();
      Exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      DisposeObject(UserDefineIO.FCurrentFileStream);
      UserDefineIO.FCurrentFileStream := nil;
    end;

  fn := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  FSize := InData.Reader.ReadInt64;

  fullfn := umlCombineFileName(FPublicPath, fn);
  UserDefineIO.FCurrentReceiveFileName := fullfn;
  try
    if (StartPos > 0) and (umlFileExists(fullfn)) then
      begin
        UserDefineIO.FCurrentFileStream := TCore_FileStream.Create(fullfn, fmOpenReadWrite);
        if StartPos <= UserDefineIO.FCurrentFileStream.Size then
            UserDefineIO.FCurrentFileStream.Position := StartPos
        else
            UserDefineIO.FCurrentFileStream.Position := UserDefineIO.FCurrentFileStream.Size;
        Sender.Print(PFormat('preprocess user:%s restore post to public: %s', [UserDefineIO.UserID, fullfn]));
      end
    else
      begin
        UserDefineIO.FCurrentFileStream := TCore_FileStream.Create(fullfn, fmCreate);
        Sender.Print(PFormat('preprocess user:%s normal post to public: %s', [UserDefineIO.UserID, fullfn]));
      end;
  except
    Sender.Print(PFormat('public file failed! user:%s post to public: %s', [UserDefineIO.UserID, fullfn]));
    UserDefineIO.FCurrentFileStream := nil;
  end;
end;

procedure TDTService.Command_PostPrivateFileInfo(Sender: TPeerIO; InData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  fn, dn: SystemString;
  StartPos: Int64;
  FSize: Int64;
  fullfn: SystemString;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
    begin
      Sender.DelayClose();
      Exit;
    end;
  if UserDefineIO.SendTunnel = nil then
    begin
      Sender.DelayClose();
      Exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      DisposeObject(UserDefineIO.FCurrentFileStream);
      UserDefineIO.FCurrentFileStream := nil;
    end;

  fn := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  FSize := InData.Reader.ReadInt64;

  if not umlDirectoryExists(umlCombinePath(UserDefineIO.UserPath, dn)) then
    begin
      umlCreateDirectory(umlCombinePath(UserDefineIO.UserPath, dn));
      Exit;
    end;

  fullfn := umlCombineFileName(umlCombinePath(UserDefineIO.UserPath, dn), fn);
  UserDefineIO.FCurrentReceiveFileName := fullfn;

  try
    if (StartPos > 0) and (umlFileExists(fullfn)) then
      begin
        UserDefineIO.FCurrentFileStream := TCore_FileStream.Create(fullfn, fmOpenReadWrite);
        if StartPos <= UserDefineIO.FCurrentFileStream.Size then
            UserDefineIO.FCurrentFileStream.Position := StartPos
        else
            UserDefineIO.FCurrentFileStream.Position := UserDefineIO.FCurrentFileStream.Size;
        Sender.Print(PFormat('preprocess user:%s restore post to private: %s', [UserDefineIO.UserID, fullfn]));
      end
    else
      begin
        UserDefineIO.FCurrentFileStream := TCore_FileStream.Create(fullfn, fmCreate);
        Sender.Print(PFormat('preprocess user:%s normal post to private: %s', [UserDefineIO.UserID, fullfn]));
      end;
  except
    Sender.Print(PFormat('create private file failed! user:%s post to private: %s', [UserDefineIO.UserID, fullfn]));
    UserDefineIO.FCurrentFileStream := nil;
  end;
end;

procedure TDTService.Command_PostFile(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
    begin
      Sender.DelayClose();
      Exit;
    end;
  if UserDefineIO.SendTunnel = nil then
    begin
      Sender.DelayClose();
      Exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      InData.Position := 0;
      if InData.Size > 0 then
          UserDefineIO.FCurrentFileStream.CopyFrom(InData, InData.Size);
    end;
end;

procedure TDTService.Command_PostFileOver(Sender: TPeerIO; InData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  fn: SystemString;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
    begin
      Sender.DelayClose();
      Exit;
    end;
  if UserDefineIO.SendTunnel = nil then
    begin
      Sender.DelayClose();
      Exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      fn := UserDefineIO.FCurrentReceiveFileName;
      DisposeObject(UserDefineIO.FCurrentFileStream);
      UserDefineIO.FCurrentFileStream := nil;

      Sender.Print(PFormat('Received File Completed:%s', [fn]));
      UserPostFileSuccess(UserDefineIO, fn);
    end;
end;

procedure TDTService.Command_GetPublicFileFragmentData(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  fullfn, fileName: SystemString;
  StartPos, EndPos, siz, fp: Int64;
  RemoteBackcallAddr: UInt64;
  fs: TCore_FileStream;
  mem_: TMS64;
  MD5: TMD5;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      Exit;

  fileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  EndPos := InData.Reader.ReadInt64;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(FPublicPath, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      Exit;
    end;

  try
      fs := TCore_FileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
    OutData.WriteBool(False);
    Exit;
  end;

  if EndPos < StartPos then
      Swap(EndPos, StartPos);

  if (EndPos > fs.Size) then
      EndPos := fs.Size;

  siz := EndPos - StartPos;
  if siz <= 0 then
    begin
      OutData.WriteBool(False);
      DisposeObject(fs);
      Exit;
    end;

  fs.Position := StartPos;
  mem_ := TMS64.Create;
  mem_.WriteUInt64(RemoteBackcallAddr);
  mem_.WriteInt64(StartPos);
  mem_.WriteInt64(EndPos);
  mem_.WriteInt64(siz);
  fp := mem_.Position;
  mem_.CopyFrom(fs, siz);
  MD5 := umlStreamMD5(mem_, fp, mem_.Size);
  mem_.WriteMD5(MD5);

  DisposeObject(fs);
  UserDefineIO.SendTunnel.Owner.SendCompleteBuffer(C_PostFileFragmentData, mem_.Memory, mem_.Size, True);
  mem_.DiscardMemory;
  DisposeObject(mem_);

  OutData.WriteBool(True);
end;

procedure TDTService.Command_GetPrivateFileFragmentData(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  fullfn, fileName: SystemString;
  StartPos, EndPos, siz, fp: Int64;
  RemoteBackcallAddr: UInt64;
  fs: TCore_FileStream;
  mem_: TMS64;
  MD5: TMD5;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      Exit;

  fileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  EndPos := InData.Reader.ReadInt64;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(UserDefineIO.UserPath, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      Exit;
    end;

  try
      fs := TCore_FileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
    OutData.WriteBool(False);
    Exit;
  end;

  if EndPos < StartPos then
      Swap(EndPos, StartPos);

  if (EndPos > fs.Size) then
      EndPos := fs.Size;

  siz := EndPos - StartPos;
  if siz <= 0 then
    begin
      OutData.WriteBool(False);
      DisposeObject(fs);
      Exit;
    end;

  fs.Position := StartPos;
  mem_ := TMS64.Create;
  mem_.WriteUInt64(RemoteBackcallAddr);
  mem_.WriteInt64(StartPos);
  mem_.WriteInt64(EndPos);
  mem_.WriteInt64(siz);
  fp := mem_.Position;
  mem_.CopyFrom(fs, siz);
  MD5 := umlStreamMD5(mem_, fp, mem_.Size);
  mem_.WriteMD5(MD5);

  DisposeObject(fs);
  UserDefineIO.SendTunnel.Owner.SendCompleteBuffer(C_PostFileFragmentData, mem_.Memory, mem_.Size, True);
  mem_.DiscardMemory;
  DisposeObject(mem_);

  OutData.WriteBool(True);
end;

procedure TDTService.Command_GetCurrentCadencer(Sender: TPeerIO; InData, OutData: TDFE);
begin
  FCadencerEngine.Progress;
  OutData.WriteDouble(FCadencerEngine.CurrentTime);
end;

procedure TDTService.Command_NewBatchStream(Sender: TPeerIO; InData: TDFE);
var
  RT: TService_RecvTunnel_UserDefine;
  p: PBigStreamBatchPostData;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      Exit;
  p := RT.BigStreamBatchList.NewPostData;
  p^.RemoteMD5 := InData.Reader.ReadMD5;
  p^.CompletedBackcallPtr := InData.Reader.ReadPointer;
end;

procedure TDTService.Command_PostBatchStream(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  RT: TService_RecvTunnel_UserDefine;
  p: PBigStreamBatchPostData;
  de: TDFE;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      Exit;

  if Sender.UserDefine.BigStreamBatchList.Count > 0 then
    begin
      p := RT.BigStreamBatchList.Last;
      p^.Source.Position := p^.Source.Size;
      p^.Source.CopyFrom(InData, InData.Size);
      if (p^.Source.Size >= BigStreamTotal) then
        begin
          p^.Source.Position := 0;
          p^.SourceMD5 := umlStreamMD5(p^.Source);

          if p^.CompletedBackcallPtr <> 0 then
            begin
              de := TDFE.Create;
              de.WriteMD5(p^.RemoteMD5);
              de.WriteMD5(p^.SourceMD5);
              de.WritePointer(p^.CompletedBackcallPtr);
              RT.SendTunnel.Owner.SendDirectStreamCmd(C_PostBatchStreamDone, de);
              DisposeObject(de);
            end;
        end;
    end;
end;

procedure TDTService.Command_ClearBatchStream(Sender: TPeerIO; InData: TDFE);
var
  RT: TService_RecvTunnel_UserDefine;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      Exit;
  RT.BigStreamBatchList.Clear;
end;

procedure TDTService.Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDFE);
var
  RT: TService_RecvTunnel_UserDefine;
  rMD5, sMD5: TMD5;
  backCallVal: UInt64;
  backCallValPtr: POnStateStruct;
  MD5Verify: Boolean;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      Exit;

  rMD5 := InData.Reader.ReadMD5;
  sMD5 := InData.Reader.ReadMD5;
  backCallVal := InData.Reader.ReadPointer;

  backCallValPtr := POnStateStruct(Pointer(backCallVal));
  MD5Verify := umlMD5Compare(rMD5, sMD5);

  if backCallValPtr = nil then
      Exit;

  try
    if Assigned(backCallValPtr^.On_C) then
        backCallValPtr^.On_C(MD5Verify);
  except
  end;

  try
    if Assigned(backCallValPtr^.On_M) then
        backCallValPtr^.On_M(MD5Verify);
  except
  end;

  try
    if Assigned(backCallValPtr^.On_P) then
        backCallValPtr^.On_P(MD5Verify);
  except
  end;

  try
      Dispose(backCallValPtr);
  except
  end;
end;

procedure TDTService.Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDFE);
var
  RT: TService_RecvTunnel_UserDefine;
  i: Integer;
  p: PBigStreamBatchPostData;

  de: TDFE;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      Exit;

  for i := 0 to RT.BigStreamBatchList.Count - 1 do
    begin
      p := RT.BigStreamBatchList[i];
      de := TDFE.Create;
      p^.Encode(de);
      OutData.WriteDataFrame(de);
      DisposeObject(de);
    end;
end;

constructor TDTService.Create(RecvTunnel_, SendTunnel_: TZNet_Server);
begin
  inherited Create;
  FRecvTunnel := RecvTunnel_;
  FRecvTunnel.PeerClientUserDefineClass := TService_RecvTunnel_UserDefine;
  FSendTunnel := SendTunnel_;
  FSendTunnel.PeerClientUserDefineClass := TService_SendTunnel_UserDefine;

  FRecvTunnel.DoubleChannelFramework := Self;
  FSendTunnel.DoubleChannelFramework := Self;

  FFileSystem := {$IFDEF DoubleIOFileSystem}True{$ELSE DoubleIOFileSystem}False{$ENDIF DoubleIOFileSystem};
  FRootPath := umlCurrentPath;
  FPublicPath := FRootPath;
  FUserDB := THashTextEngine.Create(20 * 10000);
  FAllowRegisterNewUser := False;
  FAllowSaveUserInfo := False;

  FCadencerEngine := TCadencer.Create;
  FCadencerEngine.OnProgress := CadencerProgress;
  FProgressEngine := TN_Progress_Tool.Create;

  SwitchAsDefaultPerformance;

  FOnLinkSuccess := nil;
  FOnUserOut := nil;
end;

destructor TDTService.Destroy;
begin
  DisposeObject(FUserDB);
  DisposeObject(FCadencerEngine);
  DisposeObject(FProgressEngine);
  inherited Destroy;
end;

procedure TDTService.SwitchAsMaxPerformance;
begin
  FRecvTunnel.SwitchMaxPerformance;
  FSendTunnel.SwitchMaxPerformance;
end;

procedure TDTService.SwitchAsMaxSecurity;
begin
  FRecvTunnel.SwitchMaxSecurity;
  FSendTunnel.SwitchMaxSecurity;
end;

procedure TDTService.SwitchAsDefaultPerformance;
begin
  FRecvTunnel.SwitchDefaultPerformance;
  FSendTunnel.SwitchDefaultPerformance;
end;

procedure TDTService.Progress;
begin
  FCadencerEngine.Progress;
  FRecvTunnel.Progress;
  FSendTunnel.Progress;
end;

procedure TDTService.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  FProgressEngine.Progress(deltaTime);
end;

procedure TDTService.LoadUserDB;
var
  IO_Array: TIO_Array;
  pcid: Cardinal;
  cli: TService_RecvTunnel_UserDefine;
begin
  if umlFileExists(umlCombineFileName(FRootPath, C_UserDB)) then
    begin
      FUserDB.LoadFromFile(umlCombineFileName(FRootPath, C_UserDB));

      FRecvTunnel.GetIO_Array(IO_Array);
      for pcid in IO_Array do
        begin
          cli := GetUserDefineRecvTunnel(FRecvTunnel.PeerIO[pcid]);
          if (cli <> nil) and (cli.LoginSuccessed) then
              cli.UserDBIntf := FUserDB.VariantList[cli.UserID];
        end;
    end;
end;

procedure TDTService.SaveUserDB;
var
  IO_Array: TIO_Array;
  pcid: Cardinal;
  cli: TService_RecvTunnel_UserDefine;
begin
  FUserDB.SaveToFile(umlCombineFileName(FRootPath, C_UserDB));

  FRecvTunnel.GetIO_Array(IO_Array);
  for pcid in IO_Array do
    begin
      cli := GetUserDefineRecvTunnel(FRecvTunnel.PeerIO[pcid]);
      if (cli <> nil) and (cli.LoginSuccessed) then
          cli.UserDBIntf := FUserDB.VariantList[cli.UserID];
    end;
end;

function TDTService.RegUser(UsrID, UsrPasswd: SystemString; UserConfigFile_: THashTextEngine): Boolean;
var
  UserFlag_, UserPath_: SystemString;
  UserDBIntf_: THashVariantList;
  te: THashTextEngine;
begin
  Result := False;
  if umlExistsChar(UsrID, '[]:'#13#10#9#8#0) then
      Exit;

  if FUserDB.Exists(UsrID) then
      Exit;

  UserFlag_ := MakeUserFlag;

  UserPath_ := umlCombinePath(FRootPath, UserFlag_);
  umlCreateDirectory(UserPath_);

  UserDBIntf_ := FUserDB.VariantList[UsrID];
  UserDBIntf_['UserFlag'] := UserFlag_;
  UserDBIntf_['password'] := GenerateQuantumCryptographyPassword(UsrPasswd).Text;

  if UserConfigFile_ <> nil then
    begin
      UserConfigFile_.Hit['UserInfo', 'UserID'] := UsrID;
      UserConfigFile_.Hit['UserInfo', 'Password'] := UserDBIntf_['password'];
      UserConfigFile_.SaveToFile(umlCombineFileName(UserPath_, 'User.Config'));
    end
  else
    begin
      te := THashTextEngine.Create;
      te.Hit['UserInfo', 'UserID'] := UsrID;
      te.Hit['UserInfo', 'Password'] := UserDBIntf_['password'];
      te.SaveToFile(umlCombineFileName(UserPath_, 'User.Config'));
      DisposeObject(te);
    end;

  if FAllowSaveUserInfo then
      SaveUserDB;

  UserRegistedSuccess(UsrID);
  Result := True;
end;

function TDTService.ExistsUser(UsrID: SystemString): Boolean;
begin
  Result := FUserDB.Exists(UsrID);
end;

function TDTService.GetUserPath(UsrID: SystemString): SystemString;
var
  UserFlag_: SystemString;
  UserDBIntf_: THashVariantList;
begin
  Result := '';
  if not ExistsUser(UsrID) then
      Exit;

  UserDBIntf_ := FUserDB.VariantList[UsrID];
  UserFlag_ := UserDBIntf_.GetDefaultValue('UserFlag', '');
  Result := umlCombinePath(FRootPath, UserFlag_);
end;

function TDTService.GetUserFile(UsrID, UserFileName_: SystemString): SystemString;
begin
  Result := umlCombineFileName(GetUserPath(UsrID), UserFileName_);
end;

function TDTService.GetUserDefineIO(UsrID: SystemString): TService_RecvTunnel_UserDefine;
var
  R_: TService_RecvTunnel_UserDefine;
{$IFDEF FPC}
  procedure do_fpc_progress(P_IO: TPeerIO);
  begin
    if TService_RecvTunnel_UserDefine(P_IO.UserDefine).LinkOk and
      SameText(TService_RecvTunnel_UserDefine(P_IO.UserDefine).UserID, UsrID) then
        R_ := TService_RecvTunnel_UserDefine(P_IO.UserDefine);
  end;
{$ENDIF FPC}


begin
  R_ := nil;

{$IFDEF FPC}
  FRecvTunnel.ProgressPeerIOP(do_fpc_progress);
{$ELSE FPC}
  FRecvTunnel.ProgressPeerIOP(procedure(P_IO: TPeerIO)
    begin
      if TService_RecvTunnel_UserDefine(P_IO.UserDefine).LinkOk and
        SameText(TService_RecvTunnel_UserDefine(P_IO.UserDefine).UserID, UsrID) then
          R_ := TService_RecvTunnel_UserDefine(P_IO.UserDefine);
    end);
{$ENDIF FPC}
  Result := R_;
end;

function TDTService.UserOnline(UsrID: SystemString): Boolean;
begin
  Result := GetUserDefineIO(UsrID) <> nil;
end;

function TDTService.PackUserAsFile(UsrID, packageFile: SystemString): Boolean;
var
  cli: TService_RecvTunnel_UserDefine;
begin
  Result := False;
  if not ExistsUser(UsrID) then
      Exit;
  cli := GetUserDefineIO(UsrID);
  if cli <> nil then
      cli.SaveConfigFile;

  BatchImportPathToDBFile(GetUserPath(UsrID), '*', packageFile);

  Result := True;
end;

function TDTService.PackUserAsStream(UsrID: SystemString; packageStream: TCore_Stream): Boolean;
var
  cli: TService_RecvTunnel_UserDefine;
begin
  Result := False;
  if not ExistsUser(UsrID) then
      Exit;
  cli := GetUserDefineIO(UsrID);
  if cli <> nil then
      cli.SaveConfigFile;

  BatchImportPathToDBStream(GetUserPath(UsrID), '*', packageStream);

  Result := True;
end;

function TDTService.UnPackFileAsUser(packageFile: SystemString): Boolean;
var
  fs: TCore_FileStream;
begin
  fs := TCore_FileStream.Create(packageFile, fmOpenRead or fmShareDenyNone);
  Result := UnPackStreamAsUser(fs);
  DisposeObject(fs);
end;

function TDTService.UnPackStreamAsUser(packageStream: TCore_Stream): Boolean;
var
  dbEng: TObjectDataManager;
  M: TMS64;
  te: THashTextEngine;

  UsrID, UsrPasswd: SystemString;
  UserDBIntf_: THashVariantList;
begin
  packageStream.Position := 0;
  dbEng := TObjectDataManager.CreateAsStream(packageStream, '', ObjectDataMarshal.ID, True, False, False);
  M := TMS64.Create;
  te := THashTextEngine.Create;

  try
    Result := ExtractFileInDB(dbEng, '/', 'User.Config', M);
    if Result then
      begin
        M.Position := 0;
        te.LoadFromStream(M);

        UsrID := te.GetDefaultValue('UserInfo', 'UserID', '');
        UsrPasswd := te.GetDefaultValue('UserInfo', 'Password', '');

        if not UserOnline(UsrID) then
          begin
            if not ExistsUser(UsrID) then
              begin
                DoStatus('Register new user "%s" From Pack Stream', [UsrID]);
                RegUser(UsrID, UsrPasswd, nil);
                ExtractDBToPath(dbEng, GetUserPath(UsrID));
                Result := True;
              end
            else
              begin
                DoStatus('update user "%s" From Pack Stream', [UsrID]);
                ExtractDBToPath(dbEng, GetUserPath(UsrID));

                UserDBIntf_ := FUserDB.VariantList[UsrID];
                UserDBIntf_['password'] := UsrPasswd;

                SaveUserDB;
                Result := True;
              end;
          end
        else
            DoStatus('un pack error, User is Online:%s', [UsrID]);
      end
    else
        DoStatus('unpack error,no exists file user.config in pack Stream', []);
  except
      Result := False;
  end;

  DisposeObject(te);
  DisposeObject(M);
  DisposeObject(dbEng);
end;

procedure TDTService.PostStoreQueueCMD(ToUserID: SystemString; Cmd: SystemString; InData: TDFE);
var
  UserDefineIO: TService_RecvTunnel_UserDefine;
  d: Double;
  p: PInt64;
  UserPath: SystemString;
  fn: SystemString;
  de: TDFE;
  fs: TCore_FileStream;
begin
  UserDefineIO := GetUserDefineIO(ToUserID);
  if UserDefineIO <> nil then
    begin
      FSendTunnel.SendDirectStreamCmd(UserDefineIO.SendTunnel.Owner, Cmd, InData);
    end
  else if ExistsUser(ToUserID) then
    begin
      UserPath := GetUserPath(ToUserID);

      repeat
        TCore_Thread.Sleep(1);
        d := Now;
        p := @d;
        fn := umlCombineFileName(UserPath, IntToHex(p^, 16) + '.queue');
      until not umlFileExists(fn);

      de := TDFE.Create;
      de.WriteString(Cmd);
      de.WriteDataFrame(InData);
      fs := TCore_FileStream.Create(fn, fmCreate);
      de.EncodeAsZLib(fs);
      DisposeObject(de);
      DisposeObject(fs);
    end;
end;

procedure TDTService.RegisterCommand;
begin
  FRecvTunnel.RegisterStream(C_UserLogin).OnExecute := Command_UserLogin;
  FRecvTunnel.RegisterStream(C_RegisterUser).OnExecute := Command_RegisterUser;
  FRecvTunnel.RegisterStream(C_TunnelLink).OnExecute := Command_TunnelLink;
  FRecvTunnel.RegisterStream(C_ChangePasswd).OnExecute := Command_ChangePasswd;
  FRecvTunnel.RegisterStream(C_CustomNewUser).OnExecute := Command_CustomNewUser;
  FRecvTunnel.RegisterDirectStream(C_ProcessStoreQueueCMD).OnExecute := Command_ProcessStoreQueueCMD;
  FRecvTunnel.RegisterStream(C_GetPublicFileList).OnExecute := Command_GetPublicFileList;
  FRecvTunnel.RegisterStream(C_GetPrivateFileList).OnExecute := Command_GetPrivateFileList;
  FRecvTunnel.RegisterStream(C_GetPrivateDirectoryList).OnExecute := Command_GetPrivateDirectoryList;
  FRecvTunnel.RegisterStream(C_CreatePrivateDirectory).OnExecute := Command_CreatePrivateDirectory;
  FRecvTunnel.RegisterStream(C_GetPublicFileInfo).OnExecute := Command_GetPublicFileInfo;
  FRecvTunnel.RegisterStream(C_GetPrivateFileInfo).OnExecute := Command_GetPrivateFileInfo;
  FRecvTunnel.RegisterStream(C_GetPublicFileMD5).OnExecute := Command_GetPublicFileMD5;
  FRecvTunnel.RegisterStream(C_GetPrivateFileMD5).OnExecute := Command_GetPrivateFileMD5;

  FRecvTunnel.RegisterStream(C_GetPublicFile).OnExecute := Command_GetPublicFile;
  FRecvTunnel.RegisterStream(C_GetPrivateFile).OnExecute := Command_GetPrivateFile;
  FRecvTunnel.RegisterStream(C_GetUserPrivateFile).OnExecute := Command_GetUserPrivateFile;

  FRecvTunnel.RegisterStream(C_GetPublicFileAs).OnExecute := Command_GetPublicFileAs;
  FRecvTunnel.RegisterStream(C_GetPrivateFileAs).OnExecute := Command_GetPrivateFileAs;
  FRecvTunnel.RegisterStream(C_GetUserPrivateFileAs).OnExecute := Command_GetUserPrivateFileAs;

  FRecvTunnel.RegisterDirectStream(C_PostPublicFileInfo).OnExecute := Command_PostPublicFileInfo;
  FRecvTunnel.RegisterDirectStream(C_PostPrivateFileInfo).OnExecute := Command_PostPrivateFileInfo;
  FRecvTunnel.RegisterBigStream(C_PostFile).OnExecute := Command_PostFile;
  FRecvTunnel.RegisterDirectStream(C_PostFileOver).OnExecute := Command_PostFileOver;

  FRecvTunnel.RegisterStream(C_GetPublicFileFragmentData).OnExecute := Command_GetPublicFileFragmentData;
  FRecvTunnel.RegisterStream(C_GetPrivateFileFragmentData).OnExecute := Command_GetPrivateFileFragmentData;

  FRecvTunnel.RegisterStream(C_GetCurrentCadencer).OnExecute := Command_GetCurrentCadencer;
  FRecvTunnel.RegisterDirectStream(C_NewBatchStream).OnExecute := Command_NewBatchStream;
  FRecvTunnel.RegisterBigStream(C_PostBatchStream).OnExecute := Command_PostBatchStream;
  FRecvTunnel.RegisterDirectStream(C_ClearBatchStream).OnExecute := Command_ClearBatchStream;
  FRecvTunnel.RegisterDirectStream(C_PostBatchStreamDone).OnExecute := Command_PostBatchStreamDone;
  FRecvTunnel.RegisterStream(C_GetBatchStreamState).OnExecute := Command_GetBatchStreamState;
end;

procedure TDTService.UnRegisterCommand;
begin
  FRecvTunnel.DeleteRegistedCMD(C_UserLogin);
  FRecvTunnel.DeleteRegistedCMD(C_RegisterUser);
  FRecvTunnel.DeleteRegistedCMD(C_TunnelLink);

  FRecvTunnel.DeleteRegistedCMD(C_ChangePasswd);
  FRecvTunnel.DeleteRegistedCMD(C_CustomNewUser);

  FRecvTunnel.DeleteRegistedCMD(C_ProcessStoreQueueCMD);

  FRecvTunnel.DeleteRegistedCMD(C_GetPublicFileList);
  FRecvTunnel.DeleteRegistedCMD(C_GetPrivateFileList);
  FRecvTunnel.DeleteRegistedCMD(C_GetPrivateDirectoryList);

  FRecvTunnel.DeleteRegistedCMD(C_GetUserPrivateFileList);
  FRecvTunnel.DeleteRegistedCMD(C_GetUserPrivateDirectoryList);

  FRecvTunnel.DeleteRegistedCMD(C_CreatePrivateDirectory);

  FRecvTunnel.DeleteRegistedCMD(C_GetPublicFileInfo);
  FRecvTunnel.DeleteRegistedCMD(C_GetPrivateFileInfo);

  FRecvTunnel.DeleteRegistedCMD(C_GetPublicFileMD5);
  FRecvTunnel.DeleteRegistedCMD(C_GetPrivateFileMD5);

  FRecvTunnel.DeleteRegistedCMD(C_GetPublicFile);
  FRecvTunnel.DeleteRegistedCMD(C_GetPrivateFile);
  FRecvTunnel.DeleteRegistedCMD(C_GetUserPrivateFile);

  FRecvTunnel.DeleteRegistedCMD(C_GetPublicFileAs);
  FRecvTunnel.DeleteRegistedCMD(C_GetPrivateFileAs);
  FRecvTunnel.DeleteRegistedCMD(C_GetUserPrivateFileAs);

  FRecvTunnel.DeleteRegistedCMD(C_PostPublicFileInfo);
  FRecvTunnel.DeleteRegistedCMD(C_PostPrivateFileInfo);
  FRecvTunnel.DeleteRegistedCMD(C_PostFile);
  FRecvTunnel.DeleteRegistedCMD(C_PostFileOver);

  FRecvTunnel.DeleteRegistedCMD(C_GetPublicFileFragmentData);
  FRecvTunnel.DeleteRegistedCMD(C_GetPrivateFileFragmentData);

  FRecvTunnel.DeleteRegistedCMD(C_GetCurrentCadencer);

  FRecvTunnel.DeleteRegistedCMD(C_NewBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_PostBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_ClearBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_PostBatchStreamDone);
  FRecvTunnel.DeleteRegistedCMD(C_GetBatchStreamState);
end;

function TDTService.MakeUserFlag: SystemString;
var
  d: Double;
  p: PInt64;
begin
  repeat
    TCore_Thread.Sleep(1);
    d := Now;
    p := @d;
    Result := IntToHex(p^, 16);
  until not umlDirectoryExists(umlCombinePath(FRootPath, Result));
end;

function TDTService.GetUserDefineRecvTunnel(RecvCli: TPeerIO): TService_RecvTunnel_UserDefine;
begin
  if RecvCli = nil then
      Exit(nil);
  Result := RecvCli.UserDefine as TService_RecvTunnel_UserDefine;
end;

function TDTService.TotalLinkCount: Integer;
var
  R_: Integer;
{$IFDEF FPC}
  procedure do_fpc_progress(P_IO: TPeerIO);
  begin
    if TService_RecvTunnel_UserDefine(P_IO.UserDefine).LinkOk then
        inc(R_);
  end;
{$ENDIF FPC}


begin
  R_ := 0;

{$IFDEF FPC}
  FRecvTunnel.ProgressPeerIOP(do_fpc_progress);
{$ELSE FPC}
  FRecvTunnel.ProgressPeerIOP(procedure(P_IO: TPeerIO)
    begin
      if TService_RecvTunnel_UserDefine(P_IO.UserDefine).LinkOk then
          inc(R_);
    end);
{$ENDIF FPC}
  Result := R_;
end;

procedure TDTService.PostBatchStream(cli: TPeerIO; stream: TCore_Stream; doneFreeStream: Boolean);
var
  de: TDFE;
begin
  de := TDFE.Create;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(0);
  cli.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  cli.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TDTService.PostBatchStreamC(cli: TPeerIO; stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_C);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.Init;
      p^.On_C := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  cli.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  cli.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TDTService.PostBatchStreamM(cli: TPeerIO; stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_M);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.Init;
      p^.On_M := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  cli.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  cli.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TDTService.PostBatchStreamP(cli: TPeerIO; stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_P);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.Init;
      p^.On_P := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  cli.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  cli.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TDTService.ClearBatchStream(cli: TPeerIO);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;
  cli.SendDirectStreamCmd(C_ClearBatchStream, de);
  DisposeObject(de);
end;

procedure TDTService.GetBatchStreamStateM(cli: TPeerIO; OnResult: TOnStream_M);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;
  cli.SendStreamCmdM(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

procedure TDTService.GetBatchStreamStateM(cli: TPeerIO; Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_M);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;
  cli.SendStreamCmdM(C_GetBatchStreamState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDTService.GetBatchStreamStateP(cli: TPeerIO; OnResult: TOnStream_P);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;
  cli.SendStreamCmdP(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

procedure TDTService.GetBatchStreamStateP(cli: TPeerIO; Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_P);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;
  cli.SendStreamCmdP(C_GetBatchStreamState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

constructor TClient_RecvTunnel.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  Client := nil;
  SendTunnel := nil;
end;

destructor TClient_RecvTunnel.Destroy;
begin
  if Client <> nil then
    begin
      if Client.FCurrentStream <> nil then
        begin
          DisposeObject(Client.FCurrentStream);
          Client.FCurrentStream := nil;
        end;
      Client.FLinkOk := False;
    end;
  inherited Destroy;
end;

constructor TClient_SendTunnel.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  Client := nil;
  RecvTunnel := nil;
end;

destructor TClient_SendTunnel.Destroy;
begin
  if Client <> nil then
      Client.FLinkOk := False;
  inherited Destroy;
end;

procedure TDTClient.Command_FileInfo(Sender: TPeerIO; InData: TDFE);
var
  fn: SystemString;
  StartPos: Int64;
  FSize: Int64;
  remoteinfo: SystemString;
  fullfn: SystemString;
begin
  if FCurrentStream <> nil then
    begin
      DisposeObject(FCurrentStream);
      FCurrentStream := nil;
    end;

  fn := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  FSize := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;

  if not umlDirectoryExists(remoteinfo) then
      umlCreateDirectory(remoteinfo);

  fullfn := umlCombineFileName(remoteinfo, fn);
  FCurrentReceiveStreamFileName := fullfn;
  try
    if StartPos > 0 then
      begin
        FCurrentStream := TCore_FileStream.Create(fullfn, fmOpenReadWrite);
        FCurrentStream.Position := StartPos;
      end
    else
        FCurrentStream := TCore_FileStream.Create(fullfn, fmCreate);
  except
      FRecvTunnel.ClientIO.DelayClose();
  end;
end;

procedure TDTClient.Command_PostFile(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin
  if FCurrentStream <> nil then
    begin
      InData.Position := 0;
      if InData.Size > 0 then
          FCurrentStream.CopyFrom(InData, InData.Size);
    end;
end;

procedure TDTClient.Command_PostFileOver(Sender: TPeerIO; InData: TDFE);
var
  RemoteBackcallAddr: UInt64;
  p: PRemoteFileBackcall;
  fn: SystemString;
begin
  RemoteBackcallAddr := InData.Reader.ReadPointer;
  p := Pointer(RemoteBackcallAddr);
  fn := FCurrentReceiveStreamFileName;

  if FCurrentStream <> nil then
    begin
      Sender.Print(PFormat('Receive %s ok', [umlGetFileName(fn).Text]));

      try
        if p <> nil then
          begin
            if Assigned(p^.OnComplete_C) then
              begin
                FCurrentStream.Position := 0;
                p^.OnComplete_C(p^.UserData, p^.UserObject, FCurrentStream, fn);
              end;
            if Assigned(p^.OnComplete_M) then
              begin
                FCurrentStream.Position := 0;
                p^.OnComplete_M(p^.UserData, p^.UserObject, FCurrentStream, fn);
              end;
            if Assigned(p^.OnComplete_P) then
              begin
                FCurrentStream.Position := 0;
                p^.OnComplete_P(p^.UserData, p^.UserObject, FCurrentStream, fn);
              end;
            Dispose(p);
          end;
      except
      end;

      DisposeObject(FCurrentStream);
      FCurrentStream := nil;
    end;

  FRecvFileing := False;
end;

procedure TDTClient.Command_PostFileFragmentData(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  mem_: TMS64;
  StartPos, EndPos, siz: Int64;
  RemoteBackcallAddr: UInt64;
  p: PFileFragmentDataBackcall;
  fp: Pointer;
  MD5: TMD5;
begin
  mem_ := TMS64.Create;
  mem_.SetPointerWithProtectedMode(InData, DataSize);
  RemoteBackcallAddr := mem_.ReadUInt64;
  StartPos := mem_.ReadInt64;
  EndPos := mem_.ReadInt64;
  siz := mem_.ReadInt64;
  fp := mem_.PositionAsPtr;
  mem_.Position := mem_.Position + siz;
  MD5 := mem_.ReadMD5;
  DisposeObject(mem_);

  p := Pointer(RemoteBackcallAddr);
  if p <> nil then
    begin
      try
        if Assigned(p^.OnComplete_C) then
            p^.OnComplete_C(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, fp, siz, MD5);
        if Assigned(p^.OnComplete_M) then
            p^.OnComplete_M(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, fp, siz, MD5);
        if Assigned(p^.OnComplete_P) then
            p^.OnComplete_P(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, fp, siz, MD5);
      except
      end;
      p^.fileName := '';
      Dispose(p);
    end;
end;

procedure TDTClient.GetPublicFileInfo_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE);
var
  p: PGetFileInfoStruct;
  Existed: Boolean;
  fSiz: Int64;
begin
  p := PGetFileInfoStruct(Param1);
  Existed := Result_.Reader.ReadBool;
  fSiz := Result_.Reader.ReadInt64;
  if p <> nil then
    begin
      if Assigned(p^.OnComplete_C) then
          p^.OnComplete_C(p^.UserData, p^.UserObject, p^.fileName, Existed, fSiz);
      if Assigned(p^.OnComplete_M) then
          p^.OnComplete_M(p^.UserData, p^.UserObject, p^.fileName, Existed, fSiz);
      if Assigned(p^.OnComplete_P) then
          p^.OnComplete_P(p^.UserData, p^.UserObject, p^.fileName, Existed, fSiz);
      p^.fileName := '';
      Dispose(p);
    end;
end;

procedure TDTClient.GetPrivateFileInfo_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE);
var
  p: PGetFileInfoStruct;
  Existed: Boolean;
  fSiz: Int64;
begin
  p := PGetFileInfoStruct(Param1);
  Existed := Result_.Reader.ReadBool;
  fSiz := Result_.Reader.ReadInt64;
  if p <> nil then
    begin
      if Assigned(p^.OnComplete_C) then
          p^.OnComplete_C(p^.UserData, p^.UserObject, p^.fileName, Existed, fSiz);
      if Assigned(p^.OnComplete_M) then
          p^.OnComplete_M(p^.UserData, p^.UserObject, p^.fileName, Existed, fSiz);
      if Assigned(p^.OnComplete_P) then
          p^.OnComplete_P(p^.UserData, p^.UserObject, p^.fileName, Existed, fSiz);
      p^.fileName := '';
      Dispose(p);
    end;
end;

procedure TDTClient.GetPublicFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE);
var
  p: PFileMD5Struct;
  successed: Boolean;
  MD5: TMD5;
begin
  p := PFileMD5Struct(Param1);
  successed := Result_.Reader.ReadBool;
  if successed then
      MD5 := Result_.Reader.ReadMD5
  else
      MD5 := NullMD5;
  if p <> nil then
    begin
      if Assigned(p^.OnComplete_C) then
          p^.OnComplete_C(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, MD5);
      if Assigned(p^.OnComplete_M) then
          p^.OnComplete_M(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, MD5);
      if Assigned(p^.OnComplete_P) then
          p^.OnComplete_P(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, MD5);
      p^.fileName := '';
      Dispose(p);
    end;
end;

procedure TDTClient.GetPrivateFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE);
var
  p: PFileMD5Struct;
  successed: Boolean;
  MD5: TMD5;
begin
  p := PFileMD5Struct(Param1);
  successed := Result_.Reader.ReadBool;
  if successed then
      MD5 := Result_.Reader.ReadMD5
  else
      MD5 := NullMD5;
  if p <> nil then
    begin
      if Assigned(p^.OnComplete_C) then
          p^.OnComplete_C(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, MD5);
      if Assigned(p^.OnComplete_M) then
          p^.OnComplete_M(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, MD5);
      if Assigned(p^.OnComplete_P) then
          p^.OnComplete_P(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, MD5);
      p^.fileName := '';
      Dispose(p);
    end;
end;

procedure TDTClient.GetPublicFile_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE);
var
  p: PRemoteFileBackcall;
begin
  if Result_.Count > 0 then
    begin
      if Result_.Reader.ReadBool then
        begin
          FRecvFileing := True;
          FRecvFileName := InData.ReadString(0);
          Exit;
        end;
      Sender.Print('GetPublicFile failed:%s', [Result_.Reader.ReadString]);
    end;

  p := Param1;
  Dispose(p);
end;

procedure TDTClient.GetPrivateFile_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE);
var
  p: PRemoteFileBackcall;
begin
  if Result_.Count > 0 then
    begin
      if Result_.Reader.ReadBool then
        begin
          FRecvFileing := True;
          FRecvFileName := InData.ReadString(0);
          Exit;
        end;
      Sender.Print('GetPrivateFile failed:%s', [Result_.Reader.ReadString]);
    end;

  p := Param1;
  Dispose(p);
end;

procedure TDTClient.GetFileFragmentData_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE);
var
  p: PFileFragmentDataBackcall;
begin
  if Result_.Count > 0 then
    begin
      if Result_.Reader.ReadBool then
          Exit;
    end;

  p := Param1;
  Dispose(p);
end;

procedure TDTClient.GetCurrentCadencer_StreamResult(Sender: TPeerIO; Result_: TDFE);
var
  servTime: Double;
begin
  servTime := Result_.Reader.ReadDouble;

  FCadencerEngine.Progress;
  FServerDelay := FCadencerEngine.CurrentTime - FLastCadencerTime;

  FCadencerEngine.CurrentTime := servTime + FServerDelay;
  FCadencerEngine.Progress;
end;

procedure TDTClient.Command_NewBatchStream(Sender: TPeerIO; InData: TDFE);
var
  RT: TClient_RecvTunnel;
  p: PBigStreamBatchPostData;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClient_RecvTunnel;
  p := RT.BigStreamBatchList.NewPostData;
  p^.RemoteMD5 := InData.Reader.ReadMD5;
  p^.CompletedBackcallPtr := InData.Reader.ReadPointer;
end;

procedure TDTClient.Command_PostBatchStream(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  RT: TClient_RecvTunnel;
  p: PBigStreamBatchPostData;
  de: TDFE;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClient_RecvTunnel;

  if Sender.UserDefine.BigStreamBatchList.Count > 0 then
    begin
      p := RT.BigStreamBatchList.Last;
      p^.Source.Position := p^.Source.Size;
      p^.Source.CopyFrom(InData, InData.Size);
      if (p^.Source.Size >= BigStreamTotal) then
        begin
          p^.Source.Position := 0;
          p^.SourceMD5 := umlStreamMD5(p^.Source);

          if p^.CompletedBackcallPtr <> 0 then
            begin
              de := TDFE.Create;
              de.WriteMD5(p^.RemoteMD5);
              de.WriteMD5(p^.SourceMD5);
              de.WritePointer(p^.CompletedBackcallPtr);
              SendTunnel.SendDirectStreamCmd(C_PostBatchStreamDone, de);
              DisposeObject(de);
            end;
        end;
    end;
end;

procedure TDTClient.Command_ClearBatchStream(Sender: TPeerIO; InData: TDFE);
var
  RT: TClient_RecvTunnel;
  p: PBigStreamBatchPostData;
  de: TDFE;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClient_RecvTunnel;
  RT.BigStreamBatchList.Clear;
end;

procedure TDTClient.Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDFE);
var
  RT: TClient_RecvTunnel;
  rMD5, sMD5: TMD5;
  backCallVal: UInt64;
  backCallValPtr: POnStateStruct;
  MD5Verify: Boolean;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClient_RecvTunnel;

  rMD5 := InData.Reader.ReadMD5;
  sMD5 := InData.Reader.ReadMD5;
  backCallVal := InData.Reader.ReadPointer;

  backCallValPtr := POnStateStruct(Pointer(backCallVal));
  MD5Verify := umlMD5Compare(rMD5, sMD5);

  if backCallValPtr = nil then
      Exit;

  try
    if Assigned(backCallValPtr^.On_C) then
        backCallValPtr^.On_C(MD5Verify);
  except
  end;

  try
    if Assigned(backCallValPtr^.On_M) then
        backCallValPtr^.On_M(MD5Verify);
  except
  end;

  try
    if Assigned(backCallValPtr^.On_P) then
        backCallValPtr^.On_P(MD5Verify);
  except
  end;

  try
      Dispose(backCallValPtr);
  except
  end;
end;

procedure TDTClient.Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDFE);
var
  RT: TClient_RecvTunnel;
  i: Integer;
  p: PBigStreamBatchPostData;

  de: TDFE;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClient_RecvTunnel;

  for i := 0 to RT.BigStreamBatchList.Count - 1 do
    begin
      p := RT.BigStreamBatchList[i];
      de := TDFE.Create;
      p^.Encode(de);
      OutData.WriteDataFrame(de);
      DisposeObject(de);
    end;
end;

procedure TDTClient.ClientConnected(Sender: TZNet_Client);
begin
end;

procedure TDTClient.ClientDisconnect(Sender: TZNet_Client);
begin
end;

procedure TDTClient.AsyncSendConnectResult(const cState: Boolean);
begin
  if not cState then
    begin
      try
        if Assigned(FAsyncOnResult_C) then
            FAsyncOnResult_C(False);
        if Assigned(FAsyncOnResult_M) then
            FAsyncOnResult_M(False);
        if Assigned(FAsyncOnResult_P) then
            FAsyncOnResult_P(False);
      except
      end;
      FAsyncConnectAddr := '';
      FAsyncConnRecvPort := 0;
      FAsyncConnSendPort := 0;
      FAsyncOnResult_C := nil;
      FAsyncOnResult_M := nil;
      FAsyncOnResult_P := nil;
      Exit;
    end;

  RecvTunnel.AsyncConnectM(FAsyncConnectAddr, FAsyncConnRecvPort, AsyncRecvConnectResult);
end;

procedure TDTClient.AsyncRecvConnectResult(const cState: Boolean);
begin
  if not cState then
      SendTunnel.Disconnect;

  try
    if Assigned(FAsyncOnResult_C) then
        FAsyncOnResult_C(cState);
    if Assigned(FAsyncOnResult_M) then
        FAsyncOnResult_M(cState);
    if Assigned(FAsyncOnResult_P) then
        FAsyncOnResult_P(cState);
  except
  end;

  FAsyncConnectAddr := '';
  FAsyncConnRecvPort := 0;
  FAsyncConnSendPort := 0;
  FAsyncOnResult_C := nil;
  FAsyncOnResult_M := nil;
  FAsyncOnResult_P := nil;
end;

procedure TDTClient.UserLogin_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  r: Boolean;
  p: POnStateStruct;
begin
  p := Param1;
  r := False;
  if Result_.Count > 0 then
    begin
      r := Result_.ReadBool(0);
      FSendTunnel.ClientIO.Print(Result_.ReadString(1));
    end;

  if Assigned(p^.On_C) then
      p^.On_C(r);
  if Assigned(p^.On_M) then
      p^.On_M(r);
  if Assigned(p^.On_P) then
      p^.On_P(r);

  Dispose(p);
end;

procedure TDTClient.UserLogin_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  p: POnStateStruct;
begin
  p := Param1;
  if Assigned(p^.On_C) then
      p^.On_C(False);
  if Assigned(p^.On_M) then
      p^.On_M(False);
  if Assigned(p^.On_P) then
      p^.On_P(False);

  Dispose(p);
end;

procedure TDTClient.RegisterUser_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  r: Boolean;
  p: POnStateStruct;
begin
  p := Param1;
  r := False;
  if Result_.Count > 0 then
    begin
      r := Result_.ReadBool(0);
      FSendTunnel.ClientIO.Print(Result_.ReadString(1));
    end;

  if Assigned(p^.On_C) then
      p^.On_C(r);
  if Assigned(p^.On_M) then
      p^.On_M(r);
  if Assigned(p^.On_P) then
      p^.On_P(r);

  Dispose(p);
end;

procedure TDTClient.RegisterUser_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  p: POnStateStruct;
begin
  p := Param1;
  if Assigned(p^.On_C) then
      p^.On_C(False);
  if Assigned(p^.On_M) then
      p^.On_M(False);
  if Assigned(p^.On_P) then
      p^.On_P(False);

  Dispose(p);
end;

procedure TDTClient.TunnelLink_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  r: Boolean;
  p: POnStateStruct;
begin
  p := Param1;
  r := False;
  if Result_.Count > 0 then
    begin
      r := Result_.ReadBool(0);
      FSendTunnel.ClientIO.Print(Result_.ReadString(1));

      if r then
        begin
          if Result_.Count >= 2 then
              FFileSystem := Result_.ReadBool(2)
          else
              FFileSystem := True;
          TClient_SendTunnel(FSendTunnel.ClientIO.UserDefine).Client := Self;
          TClient_SendTunnel(FSendTunnel.ClientIO.UserDefine).RecvTunnel := TClient_RecvTunnel(FRecvTunnel.ClientIO.UserDefine);

          TClient_RecvTunnel(FRecvTunnel.ClientIO.UserDefine).Client := Self;
          TClient_RecvTunnel(FRecvTunnel.ClientIO.UserDefine).SendTunnel := TClient_SendTunnel(FSendTunnel.ClientIO.UserDefine);

          FLinkOk := True;
        end;
    end;

  if Assigned(p^.On_C) then
      p^.On_C(r);
  if Assigned(p^.On_M) then
      p^.On_M(r);
  if Assigned(p^.On_P) then
      p^.On_P(r);

  Dispose(p);
end;

procedure TDTClient.TunnelLink_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  p: POnStateStruct;
begin
  p := Param1;
  if Assigned(p^.On_C) then
      p^.On_C(False);
  if Assigned(p^.On_M) then
      p^.On_M(False);
  if Assigned(p^.On_P) then
      p^.On_P(False);

  Dispose(p);
end;

constructor TDTClient.Create(RecvTunnel_, SendTunnel_: TZNet_Client);
begin
  inherited Create;
  FRecvTunnel := RecvTunnel_;
  FRecvTunnel.NotyifyInterface := Self;
  FRecvTunnel.PeerClientUserDefineClass := TClient_RecvTunnel;

  FSendTunnel := SendTunnel_;
  FSendTunnel.NotyifyInterface := Self;
  FSendTunnel.PeerClientUserDefineClass := TClient_SendTunnel;

  FRecvTunnel.DoubleChannelFramework := Self;
  FSendTunnel.DoubleChannelFramework := Self;

  FFileSystem := False;
  FCurrentStream := nil;
  FCurrentReceiveStreamFileName := '';

  FAutoFreeTunnel := False;

  FLinkOk := False;
  FWaitCommandTimeout := 8000;

  FRecvFileing := False;
  FRecvFileOfBatching := False;
  FRecvFileName := '';

  FCadencerEngine := TCadencer.Create;
  FCadencerEngine.OnProgress := CadencerProgress;
  FLastCadencerTime := 0;
  FServerDelay := 0;

  FProgressEngine := TN_Progress_Tool.Create;

  FAsyncConnectAddr := '';
  FAsyncConnRecvPort := 0;
  FAsyncConnSendPort := 0;
  FAsyncOnResult_C := nil;
  FAsyncOnResult_M := nil;
  FAsyncOnResult_P := nil;

  SwitchAsDefaultPerformance;
end;

destructor TDTClient.Destroy;
begin
  FRecvTunnel.NotyifyInterface := nil;
  FSendTunnel.NotyifyInterface := nil;

  if FCurrentStream <> nil then
    begin
      DisposeObject(FCurrentStream);
      FCurrentStream := nil;
    end;

  if FAutoFreeTunnel then
    begin
      DisposeObjectAndNil(FRecvTunnel);
      DisposeObjectAndNil(FSendTunnel);
    end;

  DisposeObject(FCadencerEngine);
  DisposeObject(FProgressEngine);
  inherited Destroy;
end;

function TDTClient.Connected: Boolean;
begin
  try
      Result := FSendTunnel.Connected and FRecvTunnel.Connected;
  except
      Result := False;
  end;
end;

function TDTClient.IOBusy: Boolean;
begin
  try
      Result := FSendTunnel.IOBusy and FRecvTunnel.IOBusy;
  except
      Result := True;
  end;
end;

procedure TDTClient.SwitchAsMaxPerformance;
begin
  FRecvTunnel.SwitchMaxPerformance;
  FSendTunnel.SwitchMaxPerformance;
end;

procedure TDTClient.SwitchAsMaxSecurity;
begin
  FRecvTunnel.SwitchMaxSecurity;
  FSendTunnel.SwitchMaxSecurity;
end;

procedure TDTClient.SwitchAsDefaultPerformance;
begin
  FRecvTunnel.SwitchDefaultPerformance;
  FSendTunnel.SwitchDefaultPerformance;
end;

procedure TDTClient.Progress;
var
  p2pVMDone: Boolean;
begin
  FCadencerEngine.Progress;

  try
    p2pVMDone := False;

    if (not p2pVMDone) and (FRecvTunnel is TZNet_WithP2PVM_Client) then
      if FRecvTunnel.ClientIO <> nil then
        begin
          FRecvTunnel.ProgressWaitSend(FRecvTunnel.ClientIO);
          p2pVMDone := True;
        end;
    FRecvTunnel.Progress;

    if (not p2pVMDone) and (FSendTunnel is TZNet_WithP2PVM_Client) then
      if FSendTunnel.ClientIO <> nil then
        begin
          FSendTunnel.ProgressWaitSend(FSendTunnel.ClientIO);
          p2pVMDone := True;
        end;
    FSendTunnel.Progress;

    if not Connected then
        FLinkOk := False;
  except
  end;
end;

procedure TDTClient.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  FProgressEngine.Progress(deltaTime);
end;

function TDTClient.Connect(addr: SystemString; const RecvPort, SendPort: Word): Boolean;
var
  t: Cardinal;
begin
  Result := False;
  Disconnect;

  if not FSendTunnel.Connect(addr, SendPort) then
    begin
      DoStatus('connect %s failed!', [addr]);
      Exit;
    end;
  if not FRecvTunnel.Connect(addr, RecvPort) then
    begin
      DoStatus('connect %s failed!', [addr]);
      Exit;
    end;

  t := GetTimeTick + 10000;
  while not RemoteInited do
    begin
      if TCore_Thread.GetTickCount > t then
          Break;
      if not Connected then
          Break;
      Progress;
    end;

  Result := Connected;
end;

procedure TDTClient.AsyncConnectC(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TOnState_C);
begin
  Disconnect;
  FAsyncConnectAddr := addr;
  FAsyncConnRecvPort := RecvPort;
  FAsyncConnSendPort := SendPort;
  FAsyncOnResult_C := OnResult;
  FAsyncOnResult_M := nil;
  FAsyncOnResult_P := nil;
  SendTunnel.AsyncConnectM(FAsyncConnectAddr, FAsyncConnSendPort, AsyncSendConnectResult);
end;

procedure TDTClient.AsyncConnectM(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TOnState_M);
begin
  Disconnect;
  FAsyncConnectAddr := addr;
  FAsyncConnRecvPort := RecvPort;
  FAsyncConnSendPort := SendPort;
  FAsyncOnResult_C := nil;
  FAsyncOnResult_M := OnResult;
  FAsyncOnResult_P := nil;
  SendTunnel.AsyncConnectM(FAsyncConnectAddr, FAsyncConnSendPort, AsyncSendConnectResult);
end;

procedure TDTClient.AsyncConnectP(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TOnState_P);
begin
  Disconnect;
  FAsyncConnectAddr := addr;
  FAsyncConnRecvPort := RecvPort;
  FAsyncConnSendPort := SendPort;
  FAsyncOnResult_C := nil;
  FAsyncOnResult_M := nil;
  FAsyncOnResult_P := OnResult;

  SendTunnel.AsyncConnectM(FAsyncConnectAddr, FAsyncConnSendPort, AsyncSendConnectResult);
end;

procedure TDTClient.AsyncConnectC(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TOnParamState_C);
var
  ParamBridge: TState_Param_Bridge;
begin
  ParamBridge := TState_Param_Bridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyC := OnResult;
  AsyncConnectM(addr, RecvPort, SendPort, ParamBridge.DoStateResult);
end;

procedure TDTClient.AsyncConnectM(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TOnParamState_M);
var
  ParamBridge: TState_Param_Bridge;
begin
  ParamBridge := TState_Param_Bridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyM := OnResult;
  AsyncConnectM(addr, RecvPort, SendPort, ParamBridge.DoStateResult);
end;

procedure TDTClient.AsyncConnectP(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TOnParamState_P);
var
  ParamBridge: TState_Param_Bridge;
begin
  ParamBridge := TState_Param_Bridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyP := OnResult;
  AsyncConnectM(addr, RecvPort, SendPort, ParamBridge.DoStateResult);
end;

procedure TDTClient.Disconnect;
begin
  if FSendTunnel.ClientIO <> nil then
      FSendTunnel.Disconnect;

  if FRecvTunnel.ClientIO <> nil then
      FRecvTunnel.Disconnect;

  FAsyncConnectAddr := '';
  FAsyncConnRecvPort := 0;
  FAsyncConnSendPort := 0;
  FAsyncOnResult_C := nil;
  FAsyncOnResult_M := nil;
  FAsyncOnResult_P := nil;
end;

function TDTClient.UserLogin(UserID, passwd: SystemString): Boolean;
var
  sendDE, resDE: TDFE;
begin
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);
  FSendTunnel.WaitSendStreamCmd(C_UserLogin, sendDE, resDE, FWaitCommandTimeout * 2);

  if resDE.Count > 0 then
    begin
      Result := resDE.ReadBool(0);
      FSendTunnel.ClientIO.Print(resDE.ReadString(1));
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TDTClient.RegisterUser(UserID, passwd: SystemString): Boolean;
var
  sendDE, resDE: TDFE;
begin
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);
  FSendTunnel.WaitSendStreamCmd(C_RegisterUser, sendDE, resDE, FWaitCommandTimeout * 2);

  if resDE.Count > 0 then
    begin
      Result := resDE.ReadBool(0);
      FSendTunnel.ClientIO.Print(resDE.ReadString(1));
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TDTClient.TunnelLink: Boolean;
var
  sendDE, resDE: TDFE;
begin
  if FLinkOk then
      Exit(True);
  Result := False;
  FLinkOk := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  SyncCadencer;

  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteCardinal(FSendTunnel.RemoteID);
  sendDE.WriteCardinal(FRecvTunnel.RemoteID);

  FSendTunnel.WaitSendStreamCmd(C_TunnelLink, sendDE, resDE, FWaitCommandTimeout * 2);

  if resDE.Count > 0 then
    begin
      Result := resDE.ReadBool(0);
      FSendTunnel.ClientIO.Print(resDE.ReadString(1));

      if Result then
        begin
          if resDE.Count >= 2 then
              FFileSystem := resDE.ReadBool(2)
          else
              FFileSystem := True;
          TClient_SendTunnel(FSendTunnel.ClientIO.UserDefine).Client := Self;
          TClient_SendTunnel(FSendTunnel.ClientIO.UserDefine).RecvTunnel := TClient_RecvTunnel(FRecvTunnel.ClientIO.UserDefine);

          TClient_RecvTunnel(FRecvTunnel.ClientIO.UserDefine).Client := Self;
          TClient_RecvTunnel(FRecvTunnel.ClientIO.UserDefine).SendTunnel := TClient_SendTunnel(FSendTunnel.ClientIO.UserDefine);
          FLinkOk := True;
        end;
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TDTClient.UserLoginC(UserID, passwd: SystemString; On_C: TOnState_C);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);

  new(p);
  p^.Init;
  p^.On_C := On_C;
  FSendTunnel.SendStreamCmdM(C_UserLogin, sendDE, p, nil, UserLogin_OnResult, UserLogin_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient.UserLoginM(UserID, passwd: SystemString; On_M: TOnState_M);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);

  new(p);
  p^.Init;
  p^.On_M := On_M;
  FSendTunnel.SendStreamCmdM(C_UserLogin, sendDE, p, nil, UserLogin_OnResult, UserLogin_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient.UserLoginP(UserID, passwd: SystemString; On_P: TOnState_P);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);

  new(p);
  p^.Init;
  p^.On_P := On_P;
  FSendTunnel.SendStreamCmdM(C_UserLogin, sendDE, p, nil, UserLogin_OnResult, UserLogin_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient.RegisterUserC(UserID, passwd: SystemString; On_C: TOnState_C);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);

  new(p);
  p^.Init;
  p^.On_C := On_C;
  FSendTunnel.SendStreamCmdM(C_RegisterUser, sendDE, p, nil, RegisterUser_OnResult, RegisterUser_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient.RegisterUserM(UserID, passwd: SystemString; On_M: TOnState_M);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);

  new(p);
  p^.Init;
  p^.On_M := On_M;
  FSendTunnel.SendStreamCmdM(C_RegisterUser, sendDE, p, nil, RegisterUser_OnResult, RegisterUser_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient.RegisterUserP(UserID, passwd: SystemString; On_P: TOnState_P);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);

  new(p);
  p^.Init;
  p^.On_P := On_P;
  FSendTunnel.SendStreamCmdM(C_RegisterUser, sendDE, p, nil, RegisterUser_OnResult, RegisterUser_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient.TunnelLinkC(On_C: TOnState_C);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if FLinkOk then
      Exit;

  FLinkOk := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  SyncCadencer;

  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FSendTunnel.RemoteID);
  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  new(p);
  p^.Init;
  p^.On_C := On_C;
  FSendTunnel.SendStreamCmdM(C_TunnelLink, sendDE, p, nil, TunnelLink_OnResult, TunnelLink_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient.TunnelLinkM(On_M: TOnState_M);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if FLinkOk then
      Exit;

  FLinkOk := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  SyncCadencer;

  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FSendTunnel.RemoteID);
  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  new(p);
  p^.Init;
  p^.On_M := On_M;
  FSendTunnel.SendStreamCmdM(C_TunnelLink, sendDE, p, nil, TunnelLink_OnResult, TunnelLink_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient.TunnelLinkP(On_P: TOnState_P);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if FLinkOk then
      Exit;

  FLinkOk := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  SyncCadencer;

  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FSendTunnel.RemoteID);
  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  new(p);
  p^.Init;
  p^.On_P := On_P;
  FSendTunnel.SendStreamCmdM(C_TunnelLink, sendDE, p, nil, TunnelLink_OnResult, TunnelLink_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient.SyncCadencer;
var
  sendDE: TDFE;
begin
  sendDE := TDFE.Create;

  FCadencerEngine.Progress;
  FLastCadencerTime := FCadencerEngine.CurrentTime;
  FServerDelay := 0;
  sendDE.WriteDouble(FLastCadencerTime);
  FSendTunnel.SendStreamCmdM(C_GetCurrentCadencer, sendDE, GetCurrentCadencer_StreamResult);
  DisposeObject(sendDE);
end;

function TDTClient.ChangePassword(oldPasswd, newPasswd: SystemString): Boolean;
var
  sendDE, resDE: TDFE;
begin
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteString(oldPasswd);
  sendDE.WriteString(newPasswd);

  FSendTunnel.WaitSendStreamCmd(C_ChangePasswd, sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count = 2 then
      Result := resDE.ReadBool(0);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TDTClient.CustomNewUser(UsrID, UsrPasswd: SystemString; UserConfigFile_: THashTextEngine): Boolean;
var
  sendDE, resDE: TDFE;
begin
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteString(UsrID);
  sendDE.WriteString(UsrPasswd);
  sendDE.WriteSectionText(UserConfigFile_);

  FSendTunnel.WaitSendStreamCmd(C_CustomNewUser, sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
      Result := resDE.ReadBool(0);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TDTClient.ProcessStoreQueueCMD;
var
  sendDE: TDFE;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;

  FSendTunnel.SendDirectStreamCmd(C_ProcessStoreQueueCMD, sendDE);

  DisposeObject(sendDE);
end;

procedure TDTClient.GetPublicFileList(Filter: SystemString; lst: TCore_Strings);
var
  sendDE, resDE: TDFE;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteString(Filter);

  FSendTunnel.WaitSendStreamCmd(C_GetPublicFileList, sendDE, resDE, FWaitCommandTimeout);

  while not resDE.Reader.IsEnd do
      lst.Add(resDE.Reader.ReadString);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TDTClient.GetPrivateFileList(Filter, RemoteDirectory: SystemString; lst: TCore_Strings);
var
  sendDE, resDE: TDFE;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteString(Filter);
  sendDE.WriteString(RemoteDirectory);

  FSendTunnel.WaitSendStreamCmd(C_GetPrivateFileList, sendDE, resDE, FWaitCommandTimeout);

  while not resDE.Reader.IsEnd do
      lst.Add(resDE.Reader.ReadString);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TDTClient.GetPrivateFileList(Filter: SystemString; lst: TCore_Strings);
begin
  GetPrivateFileList(Filter, '', lst);
end;

procedure TDTClient.GetPrivateDirectoryList(Filter, RemoteDirectory: SystemString; lst: TCore_Strings);
var
  sendDE, resDE: TDFE;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteString(Filter);
  sendDE.WriteString(RemoteDirectory);

  FSendTunnel.WaitSendStreamCmd(C_GetPrivateDirectoryList, sendDE, resDE, FWaitCommandTimeout);

  while not resDE.Reader.IsEnd do
      lst.Add(resDE.Reader.ReadString);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TDTClient.GetPrivateDirectoryList(Filter: SystemString; lst: TCore_Strings);
begin
  GetPrivateDirectoryList(Filter, '', lst);
end;

function TDTClient.CreatePrivateDirectory(RemoteDirectory: SystemString): Boolean;
var
  sendDE, resDE: TDFE;
begin
  Result := False;
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteString(RemoteDirectory);

  FSendTunnel.WaitSendStreamCmd(C_CreatePrivateDirectory, sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.Reader.ReadBool;
      FSendTunnel.ClientIO.Print(resDE.Reader.ReadString);
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

{ remote file exists }
procedure TDTClient.GetPublicFileInfoC(fileName: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_C);
var
  sendDE: TDFE;
  p: PGetFileInfoStruct;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnComplete_C := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileInfo, sendDE, p, nil, GetPublicFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPublicFileInfoM(fileName: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_M);
var
  sendDE: TDFE;
  p: PGetFileInfoStruct;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnComplete_M := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileInfo, sendDE, p, nil, GetPublicFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPublicFileInfoP(fileName: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_P);
var
  sendDE: TDFE;
  p: PGetFileInfoStruct;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnComplete_P := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileInfo, sendDE, p, nil, GetPublicFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPrivateFileInfoC(fileName, RemoteDirectory: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_C);
var
  sendDE: TDFE;
  p: PGetFileInfoStruct;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteString(RemoteDirectory);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnComplete_C := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileInfo, sendDE, p, nil, GetPrivateFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPrivateFileInfoM(fileName, RemoteDirectory: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_M);
var
  sendDE: TDFE;
  p: PGetFileInfoStruct;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteString(RemoteDirectory);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnComplete_M := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileInfo, sendDE, p, nil, GetPrivateFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPrivateFileInfoP(fileName, RemoteDirectory: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_P);
var
  sendDE: TDFE;
  p: PGetFileInfoStruct;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteString(RemoteDirectory);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnComplete_P := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileInfo, sendDE, p, nil, GetPrivateFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

{ remote md5 support with public store space }
procedure TDTClient.GetPublicFileMD5C(fileName: SystemString; const StartPos, EndPos: Int64;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_C);
var
  sendDE: TDFE;
  p: PFileMD5Struct;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_C := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileMD5, sendDE, p, nil, GetPublicFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPublicFileMD5M(fileName: SystemString; const StartPos, EndPos: Int64;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_M);
var
  sendDE: TDFE;
  p: PFileMD5Struct;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_M := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileMD5, sendDE, p, nil, GetPublicFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPublicFileMD5P(fileName: SystemString; const StartPos, EndPos: Int64;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_P);
var
  sendDE: TDFE;
  p: PFileMD5Struct;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_P := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileMD5, sendDE, p, nil, GetPublicFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

{ remote md5 support with private store space }
procedure TDTClient.GetPrivateFileMD5C(fileName, RemoteDirectory: SystemString; const StartPos, EndPos: Int64;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_C);
var
  sendDE: TDFE;
  p: PFileMD5Struct;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_C := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileMD5, sendDE, p, nil, GetPrivateFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPrivateFileMD5M(fileName, RemoteDirectory: SystemString; const StartPos, EndPos: Int64;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_M);
var
  sendDE: TDFE;
  p: PFileMD5Struct;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_M := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileMD5, sendDE, p, nil, GetPrivateFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPrivateFileMD5P(fileName, RemoteDirectory: SystemString; const StartPos, EndPos: Int64;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_P);
var
  sendDE: TDFE;
  p: PFileMD5Struct;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_P := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileMD5, sendDE, p, nil, GetPrivateFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

function TDTClient.GetPublicFile(fileName, saveToPath: SystemString): Boolean;
begin
  Result := GetPublicFile(fileName, 0, saveToPath);
end;

procedure TDTClient.GetPublicFileC(fileName, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C);
begin
  GetPublicFileC(fileName, 0, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TDTClient.GetPublicFileM(fileName, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M);
begin
  GetPublicFileM(fileName, 0, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TDTClient.GetPublicFileP(fileName, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P);
begin
  GetPublicFileP(fileName, 0, saveToPath, UserData, UserObject, OnComplete);
end;

{ restore download from public }
function TDTClient.GetPublicFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean;
var
  sendDE, resDE: TDFE;
begin
  Result := False;
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  sendDE.WritePointer(0);

  FSendTunnel.WaitSendStreamCmd(C_GetPublicFile, sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.Reader.ReadBool;
      FSendTunnel.ClientIO.Print(resDE.Reader.ReadString);

      if Result then
        begin
          FRecvFileing := True;
          FRecvFileName := fileName;
        end;
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TDTClient.GetPublicFileC(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_C := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFile, sendDE, p, nil, GetPublicFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPublicFileM(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_M := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFile, sendDE, p, nil, GetPublicFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPublicFileP(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_P := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFile, sendDE, p, nil, GetPublicFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPublicFileAsC(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_C := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFile, sendDE, p, nil, GetPublicFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPublicFileAsM(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_M := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFile, sendDE, p, nil, GetPublicFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPublicFileAsP(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_P := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFile, sendDE, p, nil, GetPublicFile_StreamParamResult);
  DisposeObject(sendDE);
end;

function TDTClient.GetPrivateFile(fileName, RemoteDirectory, saveToPath: SystemString): Boolean;
begin
  Result := GetPrivateFile(fileName, 0, RemoteDirectory, saveToPath);
end;

function TDTClient.GetPrivateFile(fileName, saveToPath: SystemString): Boolean;
begin
  Result := GetPrivateFile(fileName, '', saveToPath);
end;

procedure TDTClient.GetPrivateFileC(fileName, RemoteDirectory, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C);
begin
  GetPrivateFileC(fileName, 0, RemoteDirectory, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TDTClient.GetPrivateFileM(fileName, RemoteDirectory, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M);
begin
  GetPrivateFileM(fileName, 0, RemoteDirectory, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TDTClient.GetPrivateFileP(fileName, RemoteDirectory, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P);
begin
  GetPrivateFileP(fileName, 0, RemoteDirectory, saveToPath, UserData, UserObject, OnComplete);
end;

{ restore download from user }
function TDTClient.GetPrivateFile(fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString): Boolean;
var
  sendDE, resDE: TDFE;
begin
  Result := False;
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  sendDE.WritePointer(0);

  FSendTunnel.WaitSendStreamCmd(C_GetPrivateFile, sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.Reader.ReadBool;
      FSendTunnel.ClientIO.Print(resDE.Reader.ReadString);
      if Result then
        begin
          FRecvFileing := True;
          FRecvFileName := fileName;
        end;
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TDTClient.GetPrivateFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean;
begin
  Result := GetPrivateFile(fileName, StartPos, '', saveToPath);
end;

procedure TDTClient.GetPrivateFileC(fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_C := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFile, sendDE, p, nil, GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPrivateFileM(fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_M := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFile, sendDE, p, nil, GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPrivateFileP(fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_P := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFile, sendDE, p, nil, GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPrivateFileAsC(fileName, saveFileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_C := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFile, sendDE, p, nil, GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPrivateFileAsM(fileName, saveFileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_M := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFile, sendDE, p, nil, GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPrivateFileAsP(fileName, saveFileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_P := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFile, sendDE, p, nil, GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

function TDTClient.GetUserPrivateFile(UserID, fileName, RemoteDirectory, saveToPath: SystemString): Boolean;
begin
  Result := GetUserPrivateFile(UserID, fileName, 0, RemoteDirectory, saveToPath);
end;

function TDTClient.GetUserPrivateFile(UserID, fileName, saveToPath: SystemString): Boolean;
begin
  Result := GetUserPrivateFile(UserID, fileName, '', saveToPath);
end;

procedure TDTClient.GetUserPrivateFileC(UserID, fileName, RemoteDirectory, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C);
begin
  GetUserPrivateFileC(UserID, fileName, 0, RemoteDirectory, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TDTClient.GetUserPrivateFileM(UserID, fileName, RemoteDirectory, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M);
begin
  GetUserPrivateFileM(UserID, fileName, 0, RemoteDirectory, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TDTClient.GetUserPrivateFileP(UserID, fileName, RemoteDirectory, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P);
begin
  GetUserPrivateFileP(UserID, fileName, 0, RemoteDirectory, saveToPath, UserData, UserObject, OnComplete);
end;

{ restore download with custom user }
function TDTClient.GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString): Boolean;
var
  sendDE, resDE: TDFE;
begin
  Result := False;
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(fileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  sendDE.WritePointer(0);

  FSendTunnel.WaitSendStreamCmd(C_GetUserPrivateFile, sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.Reader.ReadBool;
      FSendTunnel.ClientIO.Print(resDE.Reader.ReadString);
      if Result then
        begin
          FRecvFileing := True;
          FRecvFileName := fileName;
        end;
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TDTClient.GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean;
begin
  Result := GetUserPrivateFile(UserID, fileName, StartPos, '', saveToPath);
end;

procedure TDTClient.GetUserPrivateFileC(UserID, fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(fileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_C := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetUserPrivateFile, sendDE, p, nil, GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetUserPrivateFileM(UserID, fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(fileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_M := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetUserPrivateFile, sendDE, p, nil, GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetUserPrivateFileP(UserID, fileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(fileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_P := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetUserPrivateFile, sendDE, p, nil, GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetUserPrivateFileAsC(UserID, fileName, saveFileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_C);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_C := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetUserPrivateFile, sendDE, p, nil, GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetUserPrivateFileAsM(UserID, fileName, saveFileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_M);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_M := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetUserPrivateFile, sendDE, p, nil, GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetUserPrivateFileAsP(UserID, fileName, saveFileName: SystemString; StartPos: Int64; RemoteDirectory, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileComplete_P);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_P := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetUserPrivateFile, sendDE, p, nil, GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPublicFileFragmentDataC(fileName: SystemString; StartPos, EndPos: Int64;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_C: TFileFragmentData_C);
var
  sendDE: TDFE;
  p: PFileFragmentDataBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_C := OnComplete_C;
  p^.OnComplete_M := nil;
  p^.OnComplete_P := nil;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileFragmentData, sendDE, p, nil, GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPublicFileFragmentDataM(fileName: SystemString; StartPos, EndPos: Int64;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_M: TFileFragmentData_M);
var
  sendDE: TDFE;
  p: PFileFragmentDataBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_C := nil;
  p^.OnComplete_M := OnComplete_M;
  p^.OnComplete_P := nil;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileFragmentData, sendDE, p, nil, GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPublicFileFragmentDataP(fileName: SystemString; StartPos, EndPos: Int64;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_P: TFileFragmentData_P);
var
  sendDE: TDFE;
  p: PFileFragmentDataBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_C := nil;
  p^.OnComplete_M := nil;
  p^.OnComplete_P := OnComplete_P;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileFragmentData, sendDE, p, nil, GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPrivateFileFragmentDataC(fileName: SystemString; StartPos, EndPos: Int64;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_C: TFileFragmentData_C);
var
  sendDE: TDFE;
  p: PFileFragmentDataBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_C := OnComplete_C;
  p^.OnComplete_M := nil;
  p^.OnComplete_P := nil;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileFragmentData, sendDE, p, nil, GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPrivateFileFragmentDataM(fileName: SystemString; StartPos, EndPos: Int64;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_M: TFileFragmentData_M);
var
  sendDE: TDFE;
  p: PFileFragmentDataBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_C := nil;
  p^.OnComplete_M := OnComplete_M;
  p^.OnComplete_P := nil;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileFragmentData, sendDE, p, nil, GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.GetPrivateFileFragmentDataP(fileName: SystemString; StartPos, EndPos: Int64;
const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_P: TFileFragmentData_P);
var
  sendDE: TDFE;
  p: PFileFragmentDataBackcall;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_C := nil;
  p^.OnComplete_M := nil;
  p^.OnComplete_P := OnComplete_P;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileFragmentData, sendDE, p, nil, GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient.AutomatedDownloadPublicFileC(remoteFile, localFile: U_String; OnDownloadDone: TFileComplete_C);
var
  tmp: TAutomatedDownloadPublicFile_Struct;
begin
  if not FFileSystem then
      Exit;
  tmp := TAutomatedDownloadPublicFile_Struct.Create;
  tmp.remoteFile := remoteFile;
  tmp.localFile := localFile;
  tmp.OnDownloadDoneC := OnDownloadDone;
  tmp.Client := Self;

  GetPublicFileInfoM(umlGetFileName(remoteFile), nil, nil, tmp.DoResult_GetFileInfo);
end;

procedure TDTClient.AutomatedDownloadPublicFileM(remoteFile, localFile: U_String; OnDownloadDone: TFileComplete_M);
var
  tmp: TAutomatedDownloadPublicFile_Struct;
begin
  if not FFileSystem then
      Exit;
  tmp := TAutomatedDownloadPublicFile_Struct.Create;
  tmp.remoteFile := remoteFile;
  tmp.localFile := localFile;
  tmp.OnDownloadDoneM := OnDownloadDone;
  tmp.Client := Self;

  GetPublicFileInfoM(umlGetFileName(remoteFile), nil, nil, tmp.DoResult_GetFileInfo);
end;

procedure TDTClient.AutomatedDownloadPublicFileP(remoteFile, localFile: U_String; OnDownloadDone: TFileComplete_P);
var
  tmp: TAutomatedDownloadPublicFile_Struct;
begin
  if not FFileSystem then
      Exit;
  tmp := TAutomatedDownloadPublicFile_Struct.Create;
  tmp.remoteFile := remoteFile;
  tmp.localFile := localFile;
  tmp.OnDownloadDoneP := OnDownloadDone;
  tmp.Client := Self;

  GetPublicFileInfoM(umlGetFileName(remoteFile), nil, nil, tmp.DoResult_GetFileInfo);
end;

procedure TDTClient.AutomatedDownloadPrivateFileC(remoteFile, RemoteDirectory, localFile: U_String; OnDownloadDone: TFileComplete_C);
var
  tmp: TAutomatedDownloadPrivateFile_Struct;
begin
  if not FFileSystem then
      Exit;
  tmp := TAutomatedDownloadPrivateFile_Struct.Create;
  tmp.remoteFile := remoteFile;
  tmp.RemoteDirectory := RemoteDirectory;
  tmp.localFile := localFile;
  tmp.OnDownloadDoneC := OnDownloadDone;
  tmp.Client := Self;

  GetPrivateFileInfoM(umlGetFileName(remoteFile), RemoteDirectory, nil, nil, tmp.DoResult_GetFileInfo);
end;

procedure TDTClient.AutomatedDownloadPrivateFileM(remoteFile, RemoteDirectory, localFile: U_String; OnDownloadDone: TFileComplete_M);
var
  tmp: TAutomatedDownloadPrivateFile_Struct;
begin
  if not FFileSystem then
      Exit;
  tmp := TAutomatedDownloadPrivateFile_Struct.Create;
  tmp.remoteFile := remoteFile;
  tmp.RemoteDirectory := RemoteDirectory;
  tmp.localFile := localFile;
  tmp.OnDownloadDoneM := OnDownloadDone;
  tmp.Client := Self;

  GetPrivateFileInfoM(umlGetFileName(remoteFile), RemoteDirectory, nil, nil, tmp.DoResult_GetFileInfo);
end;

procedure TDTClient.AutomatedDownloadPrivateFileP(remoteFile, RemoteDirectory, localFile: U_String; OnDownloadDone: TFileComplete_P);
var
  tmp: TAutomatedDownloadPrivateFile_Struct;
begin
  if not FFileSystem then
      Exit;
  tmp := TAutomatedDownloadPrivateFile_Struct.Create;
  tmp.remoteFile := remoteFile;
  tmp.RemoteDirectory := RemoteDirectory;
  tmp.localFile := localFile;
  tmp.OnDownloadDoneP := OnDownloadDone;
  tmp.Client := Self;

  GetPrivateFileInfoM(umlGetFileName(remoteFile), RemoteDirectory, nil, nil, tmp.DoResult_GetFileInfo);
end;

procedure TDTClient.PostFileToPublic(fileName: SystemString);
begin
  PostFileToPublic(fileName, 0);
end;

procedure TDTClient.PostFileToPublic(fileName: SystemString; StartPos: Int64);
var
  sendDE: TDFE;
  fs: TCore_FileStream;
begin
  if not FFileSystem then
      Exit;
  if not umlFileExists(fileName) then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  fs := TCore_FileStream.Create(fileName, fmOpenRead or fmShareDenyNone);

  sendDE := TDFE.Create;
  sendDE.WriteString(umlGetFileName(fileName));
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  FSendTunnel.SendDirectStreamCmd(C_PostPublicFileInfo, sendDE);
  DisposeObject(sendDE);

  FSendTunnel.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDFE.Create;
  FSendTunnel.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);
end;

procedure TDTClient.PostFileToPrivate(fileName, RemoteDirectory: SystemString);
begin
  PostFileToPrivate(fileName, RemoteDirectory, 0);
end;

procedure TDTClient.PostFileToPrivate(fileName: SystemString);
begin
  PostFileToPrivate(fileName, '');
end;

procedure TDTClient.PostFileToPrivate(fileName, RemoteDirectory: SystemString; StartPos: Int64);
var
  sendDE: TDFE;
  fs: TCore_FileStream;
begin
  if not FFileSystem then
      Exit;
  if not umlFileExists(fileName) then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  fs := TCore_FileStream.Create(fileName, fmOpenRead or fmShareDenyNone);

  sendDE := TDFE.Create;
  sendDE.WriteString(umlGetFileName(fileName));
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  FSendTunnel.SendDirectStreamCmd(C_PostPrivateFileInfo, sendDE);
  DisposeObject(sendDE);

  FSendTunnel.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDFE.Create;
  FSendTunnel.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);
end;

procedure TDTClient.PostFileToPrivate(fileName: SystemString; StartPos: Int64);
begin
  PostFileToPrivate(fileName, '', StartPos);
end;

procedure TDTClient.PostStreamToPrivate(RemoteFileName, RemoteDirectory: SystemString; stream: TCore_Stream; doneFreeStream: Boolean);
begin
  PostStreamToPrivate(RemoteFileName, RemoteDirectory, stream, 0, doneFreeStream);
end;

procedure TDTClient.PostStreamToPrivate(RemoteFileName, RemoteDirectory: SystemString; stream: TCore_Stream; StartPos: Int64; doneFreeStream: Boolean);
var
  sendDE: TDFE;
begin
  if not FFileSystem then
    begin
      if doneFreeStream then
          DisposeObject(stream);
      Exit;
    end;
  if not FSendTunnel.Connected then
    begin
      if doneFreeStream then
          DisposeObject(stream);
      Exit;
    end;
  if not FRecvTunnel.Connected then
    begin
      if doneFreeStream then
          DisposeObject(stream);
      Exit;
    end;

  sendDE := TDFE.Create;
  sendDE.WriteString(RemoteFileName);
  sendDE.WriteString(RemoteDirectory);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(stream.Size);
  FSendTunnel.SendDirectStreamCmd(C_PostPrivateFileInfo, sendDE);
  DisposeObject(sendDE);

  FSendTunnel.SendBigStream(C_PostFile, stream, StartPos, doneFreeStream);

  sendDE := TDFE.Create;
  FSendTunnel.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);
end;

procedure TDTClient.AutomatedUploadFileToPublic(localFile: U_String);
var
  tmp: TAutomatedUploadPublicFile_Struct;
begin
  if not FFileSystem then
      Exit;
  tmp := TAutomatedUploadPublicFile_Struct.Create;
  tmp.localFile := localFile;
  tmp.Client := Self;

  GetPublicFileInfoM(umlGetFileName(localFile), nil, nil, tmp.DoResult_GetFileInfo);
end;

procedure TDTClient.AutomatedUploadFileToPrivate(localFile, RemoteDirectory: U_String);
var
  tmp: TAutomatedUploadPrivateFile_Struct;
begin
  if not FFileSystem then
      Exit;
  tmp := TAutomatedUploadPrivateFile_Struct.Create;
  tmp.localFile := localFile;
  tmp.RemoteDirectory := RemoteDirectory;
  tmp.Client := Self;

  GetPrivateFileInfoM(umlGetFileName(localFile), RemoteDirectory, nil, nil, tmp.DoResult_GetFileInfo);
end;

procedure TDTClient.PostBatchStream(stream: TCore_Stream; doneFreeStream: Boolean);
var
  de: TDFE;
begin
  de := TDFE.Create;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(0);
  SendTunnel.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  SendTunnel.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TDTClient.PostBatchStreamC(stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_C);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.Init;
      p^.On_C := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  SendTunnel.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  SendTunnel.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TDTClient.PostBatchStreamM(stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_M);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.Init;
      p^.On_M := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  SendTunnel.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  SendTunnel.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TDTClient.PostBatchStreamP(stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_P);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.Init;
      p^.On_P := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  SendTunnel.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  SendTunnel.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TDTClient.ClearBatchStream;
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;
  SendTunnel.SendDirectStreamCmd(C_ClearBatchStream, de);
  DisposeObject(de);
end;

procedure TDTClient.GetBatchStreamStateM(OnResult: TOnStream_M);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdM(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

procedure TDTClient.GetBatchStreamStateM(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_M);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdM(C_GetBatchStreamState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDTClient.GetBatchStreamStateP(OnResult: TOnStream_P);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdP(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

procedure TDTClient.GetBatchStreamStateP(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_P);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdP(C_GetBatchStreamState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

function TDTClient.GetBatchStreamState(Result_: TDFE; TimeOut_: TTimeTick): Boolean;
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.WaitSendStreamCmd(C_GetBatchStreamState, de, Result_, TimeOut_);
  Result := Result_.Count > 0;
  DisposeObject(de);
end;

procedure TDTClient.RegisterCommand;
begin
  FRecvTunnel.RegisterDirectStream(C_FileInfo).OnExecute := Command_FileInfo;
  FRecvTunnel.RegisterBigStream(C_PostFile).OnExecute := Command_PostFile;
  FRecvTunnel.RegisterDirectStream(C_PostFileOver).OnExecute := Command_PostFileOver;
  FRecvTunnel.RegisterCompleteBuffer(C_PostFileFragmentData).OnExecute := Command_PostFileFragmentData;

  FRecvTunnel.RegisterDirectStream(C_NewBatchStream).OnExecute := Command_NewBatchStream;
  FRecvTunnel.RegisterBigStream(C_PostBatchStream).OnExecute := Command_PostBatchStream;
  FRecvTunnel.RegisterDirectStream(C_ClearBatchStream).OnExecute := Command_ClearBatchStream;
  FRecvTunnel.RegisterDirectStream(C_PostBatchStreamDone).OnExecute := Command_PostBatchStreamDone;
  FRecvTunnel.RegisterStream(C_GetBatchStreamState).OnExecute := Command_GetBatchStreamState;
end;

procedure TDTClient.UnRegisterCommand;
begin
  FRecvTunnel.DeleteRegistedCMD(C_FileInfo);
  FRecvTunnel.DeleteRegistedCMD(C_PostFile);
  FRecvTunnel.DeleteRegistedCMD(C_PostFileOver);
  FRecvTunnel.DeleteRegistedCMD(C_PostFileFragmentData);

  FRecvTunnel.DeleteRegistedCMD(C_NewBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_PostBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_ClearBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_PostBatchStreamDone);
  FRecvTunnel.DeleteRegistedCMD(C_GetBatchStreamState);
end;

function TDTClient.RemoteInited: Boolean;
begin
  Result := FSendTunnel.RemoteInited and FRecvTunnel.RemoteInited;
end;

procedure TDT_P2PVM_OnState.Init;
begin
  On_C := nil;
  On_M := nil;
  On_P := nil;
end;

function TDT_P2PVM_Service.GetQuietMode: Boolean;
begin
  Result := RecvTunnel.QuietMode and SendTunnel.QuietMode and PhysicsTunnel.QuietMode;
end;

procedure TDT_P2PVM_Service.SetQuietMode(const Value: Boolean);
begin
  RecvTunnel.QuietMode := Value;
  SendTunnel.QuietMode := Value;
  PhysicsTunnel.QuietMode := Value;
end;

constructor TDT_P2PVM_Service.Create(ServiceClass_: TDTServiceClass);
begin
  inherited Create;
  RecvTunnel := TZNet_WithP2PVM_Server.Create;
  RecvTunnel.QuietMode := True;

  SendTunnel := TZNet_WithP2PVM_Server.Create;
  SendTunnel.QuietMode := True;

  DTService := ServiceClass_.Create(RecvTunnel, SendTunnel);
  DTService.RegisterCommand;
  DTService.SwitchAsDefaultPerformance;

  PhysicsTunnel := TPhysicsServer.Create;
  PhysicsTunnel.QuietMode := True;
  PhysicsTunnel.AutomatedP2PVMBindService.AddService(RecvTunnel);
  PhysicsTunnel.AutomatedP2PVMBindService.AddService(SendTunnel);
  PhysicsTunnel.AutomatedP2PVMService := True;

  RecvTunnel.PrefixName := 'DT';
  RecvTunnel.Name := 'R';
  SendTunnel.PrefixName := 'DT';
  SendTunnel.Name := 'S';
  PhysicsTunnel.PrefixName := 'Physics';
  PhysicsTunnel.Name := 'p2pVM';
end;

destructor TDT_P2PVM_Service.Destroy;
begin
  StopService;
  DisposeObject(RecvTunnel);
  DisposeObject(SendTunnel);
  DisposeObject(DTService);
  DisposeObject(PhysicsTunnel);
  inherited Destroy;
end;

procedure TDT_P2PVM_Service.Progress;
begin
  DTService.Progress;
  PhysicsTunnel.Progress;
end;

function TDT_P2PVM_Service.StartService(ListenAddr, ListenPort, Auth: SystemString): Boolean;
begin
  StopService;
  RecvTunnel.StartService('::', 1);
  SendTunnel.StartService('::', 2);
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  Result := PhysicsTunnel.StartService(ListenAddr, umlStrToInt(ListenPort));
  if Result then
      DoStatus('listening %s:%s ok.', [TranslateBindAddr(ListenAddr), ListenPort])
  else
      DoStatus('listening %s:%s failed!', [TranslateBindAddr(ListenAddr), ListenPort]);
end;

procedure TDT_P2PVM_Service.StopService;
begin
  PhysicsTunnel.StopService;
  RecvTunnel.StopService;
  SendTunnel.StopService;
end;

procedure TDT_P2PVM_Client.DoConnectionResult(const state: Boolean);
begin
  if not state then
    begin
      Connecting := False;

      if Assigned(OnConnectResultState.On_C) then
          OnConnectResultState.On_C(state);
      if Assigned(OnConnectResultState.On_M) then
          OnConnectResultState.On_M(state);
      if Assigned(OnConnectResultState.On_P) then
          OnConnectResultState.On_P(state);
      OnConnectResultState.Init;
    end;

  PhysicsTunnel.PrintParam('DT Physics Connect %s', umlBoolToStr(state));
end;

procedure TDT_P2PVM_Client.DoAutomatedP2PVMClientConnectionDone(Sender: TZNet; P_IO: TPeerIO);
begin
  PhysicsTunnel.Print('DT p2pVM done.');
  if RegisterUserAndLogin then
    begin
      DTClient.RegisterUserM(LastUser, LastPasswd, DoLoginResult);
    end
  else
    begin
      DTClient.UserLoginM(LastUser, LastPasswd, DoLoginResult);
    end;
end;

procedure TDT_P2PVM_Client.DoRegisterResult(const state: Boolean);
begin
  if not state then
    begin
      DTClient.UserLoginM(LastUser, LastPasswd, DoLoginResult);
    end
  else
    begin
      DTClient.TunnelLinkM(DoTunnelLinkResult);
    end;
end;

procedure TDT_P2PVM_Client.DoLoginResult(const state: Boolean);
begin
  if not state then
    begin
      Connecting := False;

      if Assigned(OnConnectResultState.On_C) then
          OnConnectResultState.On_C(state);
      if Assigned(OnConnectResultState.On_M) then
          OnConnectResultState.On_M(state);
      if Assigned(OnConnectResultState.On_P) then
          OnConnectResultState.On_P(state);
      OnConnectResultState.Init;
      Exit;
    end;

  DTClient.TunnelLinkM(DoTunnelLinkResult);
end;

procedure TDT_P2PVM_Client.DoTunnelLinkResult(const state: Boolean);
begin
  if Assigned(OnConnectResultState.On_C) then
      OnConnectResultState.On_C(state);
  if Assigned(OnConnectResultState.On_M) then
      OnConnectResultState.On_M(state);
  if Assigned(OnConnectResultState.On_P) then
      OnConnectResultState.On_P(state);
  OnConnectResultState.Init;
  Connecting := False;

  if state then
    begin
      RegisterUserAndLogin := False;
      if AutomatedConnection then
          Reconnection := True;
      if Assigned(OnTunnelLink) then
          OnTunnelLink(Self);
    end;
end;

function TDT_P2PVM_Client.GetQuietMode: Boolean;
begin
  Result := RecvTunnel.QuietMode and SendTunnel.QuietMode and PhysicsTunnel.QuietMode;
end;

procedure TDT_P2PVM_Client.SetQuietMode(const Value: Boolean);
begin
  RecvTunnel.QuietMode := Value;
  SendTunnel.QuietMode := Value;
  PhysicsTunnel.QuietMode := Value;
end;

constructor TDT_P2PVM_Client.Create(ClientClass_: TDTClientClass);
begin
  inherited Create;
  OnConnectResultState.Init;
  Connecting := False;
  Reconnection := False;

  RecvTunnel := TZNet_WithP2PVM_Client.Create;
  RecvTunnel.QuietMode := True;

  SendTunnel := TZNet_WithP2PVM_Client.Create;
  SendTunnel.QuietMode := True;

  DTClient := ClientClass_.Create(RecvTunnel, SendTunnel);
  DTClient.RegisterCommand;
  DTClient.SwitchAsDefaultPerformance;

  PhysicsTunnel := TPhysicsClient.Create;
  PhysicsTunnel.QuietMode := True;
  PhysicsTunnel.AutomatedP2PVMBindClient.AddClient(SendTunnel, '::', 1);
  PhysicsTunnel.AutomatedP2PVMBindClient.AddClient(RecvTunnel, '::', 2);
  PhysicsTunnel.AutomatedP2PVMClient := True;
  PhysicsTunnel.AutomatedP2PVMClientDelayBoot := 0;

  LastAddr := '';
  LastPort := '';
  LastAuth := '';
  LastUser := '';
  LastPasswd := '';

  RegisterUserAndLogin := False;
  AutomatedConnection := True;
  OnTunnelLink := nil;

  RecvTunnel.PrefixName := 'DT';
  RecvTunnel.Name := 'R';
  SendTunnel.PrefixName := 'DT';
  SendTunnel.Name := 'S';
  PhysicsTunnel.PrefixName := 'Physics';
  PhysicsTunnel.Name := 'p2pVM';
end;

destructor TDT_P2PVM_Client.Destroy;
begin
  Disconnect;
  DisposeObject(RecvTunnel);
  DisposeObject(SendTunnel);
  DisposeObject(DTClient);
  DisposeObject(PhysicsTunnel);
  inherited Destroy;
end;

procedure TDT_P2PVM_Client.Progress;
begin
  DTClient.Progress;
  PhysicsTunnel.Progress;

  if (AutomatedConnection) and ((not PhysicsTunnel.Connected) or (not DTClient.LinkOk)) and (not Connecting) and (Reconnection) then
      Connect(LastAddr, LastPort, LastAuth, LastUser, LastPasswd);
end;

procedure TDT_P2PVM_Client.Connect(addr, Port, Auth, User, passwd: SystemString);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Reconnection then
    begin
      LastAddr := addr;
      LastPort := Port;
      LastAuth := Auth;
    end;
  LastUser := User;
  LastPasswd := passwd;
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  OnConnectResultState.Init;
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_M := DoAutomatedP2PVMClientConnectionDone;
  PhysicsTunnel.AsyncConnectM(addr, umlStrToInt(Port), DoConnectionResult);
end;

procedure TDT_P2PVM_Client.Connect_C(addr, Port, Auth, User, passwd: SystemString; OnResult: TOnState_C);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Reconnection then
    begin
      LastAddr := addr;
      LastPort := Port;
      LastAuth := Auth;
    end;
  LastUser := User;
  LastPasswd := passwd;
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  OnConnectResultState.Init;
  OnConnectResultState.On_C := OnResult;
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_M := DoAutomatedP2PVMClientConnectionDone;
  PhysicsTunnel.AsyncConnectM(addr, umlStrToInt(Port), DoConnectionResult);
end;

procedure TDT_P2PVM_Client.Connect_M(addr, Port, Auth, User, passwd: SystemString; OnResult: TOnState_M);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Reconnection then
    begin
      LastAddr := addr;
      LastPort := Port;
      LastAuth := Auth;
    end;
  LastUser := User;
  LastPasswd := passwd;
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  OnConnectResultState.Init;
  OnConnectResultState.On_M := OnResult;
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_M := DoAutomatedP2PVMClientConnectionDone;
  PhysicsTunnel.AsyncConnectM(addr, umlStrToInt(Port), DoConnectionResult);
end;

procedure TDT_P2PVM_Client.Connect_P(addr, Port, Auth, User, passwd: SystemString; OnResult: TOnState_P);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Reconnection then
    begin
      LastAddr := addr;
      LastPort := Port;
      LastAuth := Auth;
    end;
  LastUser := User;
  LastPasswd := passwd;
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  OnConnectResultState.Init;
  OnConnectResultState.On_P := OnResult;
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_M := DoAutomatedP2PVMClientConnectionDone;
  PhysicsTunnel.AsyncConnectM(addr, umlStrToInt(Port), DoConnectionResult);
end;

procedure TDT_P2PVM_Client.Disconnect;
begin
  Connecting := False;
  Reconnection := False;
  LastAddr := '';
  LastPort := '';
  LastAuth := '';
  PhysicsTunnel.Disconnect;
end;

function TDT_P2PVM_Custom_Service.GetQuietMode: Boolean;
begin
  Result := RecvTunnel.QuietMode and SendTunnel.QuietMode;
end;

procedure TDT_P2PVM_Custom_Service.SetQuietMode(const Value: Boolean);
begin
  RecvTunnel.QuietMode := Value;
  SendTunnel.QuietMode := Value;
end;

constructor TDT_P2PVM_Custom_Service.Create(ServiceClass_: TDTServiceClass; PhysicsTunnel_: TZNet_Server;
P2PVM_Recv_Name_, P2PVM_Recv_IP6_, P2PVM_Recv_Port_,
  P2PVM_Send_Name_, P2PVM_Send_IP6_, P2PVM_Send_Port_: SystemString);
begin
  inherited Create;

  Bind_PhysicsTunnel := PhysicsTunnel_;
  Bind_P2PVM_Recv_IP6 := P2PVM_Recv_IP6_;
  Bind_P2PVM_Recv_Port := umlStrToInt(P2PVM_Recv_Port_);
  Bind_P2PVM_Send_IP6 := P2PVM_Send_IP6_;
  Bind_P2PVM_Send_Port := umlStrToInt(P2PVM_Send_Port_);

  RecvTunnel := TZNet_WithP2PVM_Server.Create;
  RecvTunnel.QuietMode := PhysicsTunnel_.QuietMode;
  RecvTunnel.PrefixName := 'DT';
  RecvTunnel.Name := P2PVM_Recv_Name_;

  SendTunnel := TZNet_WithP2PVM_Server.Create;
  SendTunnel.QuietMode := PhysicsTunnel_.QuietMode;
  SendTunnel.PrefixName := 'DT';
  SendTunnel.Name := P2PVM_Send_Name_;

  DTService := ServiceClass_.Create(RecvTunnel, SendTunnel);
  DTService.RegisterCommand;
  DTService.SwitchAsDefaultPerformance;

  Bind_PhysicsTunnel.AutomatedP2PVMServiceBind.AddService(RecvTunnel);
  Bind_PhysicsTunnel.AutomatedP2PVMServiceBind.AddService(SendTunnel);
  Bind_PhysicsTunnel.AutomatedP2PVMService := True;
  StartService();
end;

destructor TDT_P2PVM_Custom_Service.Destroy;
begin
  StopService;
  Bind_PhysicsTunnel.AutomatedP2PVMServiceBind.RemoveService(RecvTunnel);
  Bind_PhysicsTunnel.AutomatedP2PVMServiceBind.RemoveService(SendTunnel);
  DisposeObject(RecvTunnel);
  DisposeObject(SendTunnel);
  DisposeObject(DTService);
  inherited Destroy;
end;

procedure TDT_P2PVM_Custom_Service.Progress;
begin
  Bind_PhysicsTunnel.Progress;
  DTService.Progress;
end;

procedure TDT_P2PVM_Custom_Service.StartService;
begin
  RecvTunnel.StartService(Bind_P2PVM_Recv_IP6, Bind_P2PVM_Recv_Port);
  SendTunnel.StartService(Bind_P2PVM_Send_IP6, Bind_P2PVM_Send_Port);
end;

procedure TDT_P2PVM_Custom_Service.StopService;
begin
  RecvTunnel.StopService;
  RecvTunnel.StopService;
end;

procedure TDT_P2PVM_Custom_Client_Clone_Pool.DoFree(var Data: TDT_P2PVM_Custom_Client);
begin
  if Data <> nil then
    begin
      Data.Clone_Instance_Ptr := nil;
      DisposeObjectAndNil(Data);
    end;
end;

procedure TDT_P2PVM_Custom_Client.DoRegisterResult(const state: Boolean);
begin
  if not state then
    begin
      DTClient.UserLoginM(LastUser, LastPasswd, DoLoginResult);
    end
  else
    begin
      DTClient.TunnelLinkM(DoTunnelLinkResult);
    end;
end;

procedure TDT_P2PVM_Custom_Client.DoLoginResult(const state: Boolean);
begin
  if not state then
    begin
      if Assigned(OnConnectResultState.On_C) then
          OnConnectResultState.On_C(state);
      if Assigned(OnConnectResultState.On_M) then
          OnConnectResultState.On_M(state);
      if Assigned(OnConnectResultState.On_P) then
          OnConnectResultState.On_P(state);
      OnConnectResultState.Init;
      Connecting := False;
      Exit;
    end;

  DTClient.TunnelLinkM(DoTunnelLinkResult);
end;

function TDT_P2PVM_Custom_Client.GetQuietMode: Boolean;
begin
  Result := RecvTunnel.QuietMode and SendTunnel.QuietMode;
end;

procedure TDT_P2PVM_Custom_Client.SetQuietMode(const Value: Boolean);
begin
  RecvTunnel.QuietMode := Value;
  SendTunnel.QuietMode := Value;
end;

procedure TDT_P2PVM_Custom_Client.Do_Recv_Connect_State(const state: Boolean);
begin
  if not state then
      Exit;
  if SendTunnel.RemoteInited then
      Connecting := False;
end;

procedure TDT_P2PVM_Custom_Client.Do_Send_Connect_State(const state: Boolean);
begin
  if not state then
      Exit;
  if RecvTunnel.RemoteInited then
      Connecting := False;
end;

constructor TDT_P2PVM_Custom_Client.Create(ClientClass_: TDTClientClass; PhysicsTunnel_: TZNet_Client;
P2PVM_Recv_Name_, P2PVM_Recv_IP6_, P2PVM_Recv_Port_, P2PVM_Send_Name_, P2PVM_Send_IP6_, P2PVM_Send_Port_: SystemString);
begin
  inherited Create;
  // internal
  OnConnectResultState.Init;
  Connecting := False;
  Reconnection := False;

  // clone Technology
  Parent_Client := nil;
  Clone_Instance_Ptr := nil;
  Clone_Pool := TDT_P2PVM_Custom_Client_Clone_Pool.Create;
  // bind
  Bind_PhysicsTunnel := PhysicsTunnel_;
  Bind_P2PVM_Recv_IP6 := P2PVM_Recv_IP6_;
  Bind_P2PVM_Recv_Port := umlStrToInt(P2PVM_Recv_Port_);
  Bind_P2PVM_Send_IP6 := P2PVM_Send_IP6_;
  Bind_P2PVM_Send_Port := umlStrToInt(P2PVM_Send_Port_);

  // local
  RecvTunnel := TZNet_WithP2PVM_Client.Create;
  RecvTunnel.QuietMode := PhysicsTunnel_.QuietMode;
  RecvTunnel.PrefixName := 'DT';
  RecvTunnel.Name := P2PVM_Recv_Name_;
  SendTunnel := TZNet_WithP2PVM_Client.Create;
  SendTunnel.QuietMode := PhysicsTunnel_.QuietMode;
  SendTunnel.PrefixName := 'DT';
  SendTunnel.Name := P2PVM_Send_Name_;
  // local DT
  ClientClass := ClientClass_;
  DTClient := ClientClass.Create(RecvTunnel, SendTunnel);
  DTClient.RegisterCommand;
  DTClient.SwitchAsDefaultPerformance;

  LastUser := '';
  LastPasswd := '';
  RegisterUserAndLogin := False;
  AutomatedConnection := True;
  OnTunnelLink := nil;

  // automated p2pVM
  Bind_PhysicsTunnel.AutomatedP2PVMBindClient.AddClient(RecvTunnel, Bind_P2PVM_Recv_IP6, Bind_P2PVM_Recv_Port);
  Bind_PhysicsTunnel.AutomatedP2PVMBindClient.AddClient(SendTunnel, Bind_P2PVM_Send_IP6, Bind_P2PVM_Send_Port);
  Bind_PhysicsTunnel.AutomatedP2PVMClient := True;
  Bind_PhysicsTunnel.AutomatedP2PVMClientDelayBoot := 0;
end;

constructor TDT_P2PVM_Custom_Client.Create_Clone(Parent_Client_: TDT_P2PVM_Custom_Client);
begin
  inherited Create;

  if not Parent_Client_.AutomatedConnection then
      RaiseInfo('Host not established');
  if Parent_Client_.LastUser = '' then
      RaiseInfo('Host loss user info');
  if Parent_Client_.LastPasswd = '' then
      RaiseInfo('Host loss password info');
  if not Parent_Client_.DTClient.LinkOk then
      RaiseInfo('Host is Offline.');

  // internal
  OnConnectResultState.Init;
  Connecting := True;
  Reconnection := True;

  // clone Technology
  Parent_Client := Parent_Client_;
  Clone_Instance_Ptr := Parent_Client_.Clone_Pool.Add(Self);
  Clone_Pool := TDT_P2PVM_Custom_Client_Clone_Pool.Create;

  // bind
  Bind_PhysicsTunnel := Parent_Client_.Bind_PhysicsTunnel;
  Bind_P2PVM_Recv_IP6 := Parent_Client_.Bind_P2PVM_Recv_IP6;
  Bind_P2PVM_Recv_Port := Parent_Client_.Bind_P2PVM_Recv_Port;
  Bind_P2PVM_Send_IP6 := Parent_Client_.Bind_P2PVM_Send_IP6;
  Bind_P2PVM_Send_Port := Parent_Client_.Bind_P2PVM_Send_Port;

  // local
  RecvTunnel := TZNet_WithP2PVM_Client.Create;
  RecvTunnel.QuietMode := Bind_PhysicsTunnel.QuietMode;
  RecvTunnel.PrefixName := 'DT';
  RecvTunnel.Name := Parent_Client_.RecvTunnel.Name;
  SendTunnel := TZNet_WithP2PVM_Client.Create;
  SendTunnel.QuietMode := Bind_PhysicsTunnel.QuietMode;
  SendTunnel.PrefixName := 'DT';
  SendTunnel.Name := Parent_Client_.SendTunnel.Name;
  // local DT
  ClientClass := Parent_Client_.ClientClass;
  DTClient := ClientClass.Create(RecvTunnel, SendTunnel);
  DTClient.RegisterCommand;
  DTClient.SwitchAsDefaultPerformance;

  LastUser := Parent_Client_.LastUser;
  LastPasswd := Parent_Client_.LastPasswd;
  RegisterUserAndLogin := False;
  AutomatedConnection := True;
  OnTunnelLink := nil;

  // automated p2pVM
  Bind_PhysicsTunnel.AutomatedP2PVMBindClient.AddClient(RecvTunnel, Bind_P2PVM_Recv_IP6, Bind_P2PVM_Recv_Port);
  Bind_PhysicsTunnel.AutomatedP2PVMBindClient.AddClient(SendTunnel, Bind_P2PVM_Send_IP6, Bind_P2PVM_Send_Port);
  Bind_PhysicsTunnel.AutomatedP2PVMClient := True;
  Bind_PhysicsTunnel.AutomatedP2PVMClientDelayBoot := 0;

  Bind_PhysicsTunnel.P2PVM.InstallLogicFramework(RecvTunnel);
  Bind_PhysicsTunnel.P2PVM.InstallLogicFramework(SendTunnel);

  if Parent_Client_.RecvTunnel.RemoteInited then
      RecvTunnel.AsyncConnectM(Bind_P2PVM_Recv_IP6, Bind_P2PVM_Recv_Port, Do_Recv_Connect_State);
  if Parent_Client_.SendTunnel.RemoteInited then
      SendTunnel.AsyncConnectM(Bind_P2PVM_Send_IP6, Bind_P2PVM_Send_Port, Do_Send_Connect_State);
end;

destructor TDT_P2PVM_Custom_Client.Destroy;
begin
  if (Parent_Client <> nil) and (Clone_Instance_Ptr <> nil) then
    begin
      Clone_Instance_Ptr^.Data := nil;
      Parent_Client.Clone_Pool.Remove_P(Clone_Instance_Ptr);
    end;
  Clone_Pool.Clear;

  if Bind_PhysicsTunnel <> nil then
    begin
      Bind_PhysicsTunnel.AutomatedP2PVMBindClient.RemoveClient(RecvTunnel);
      Bind_PhysicsTunnel.AutomatedP2PVMBindClient.RemoveClient(SendTunnel);
    end;

  Disconnect;
  DisposeObjectAndNil(DTClient);
  DisposeObjectAndNil(RecvTunnel);
  DisposeObjectAndNil(SendTunnel);
  DisposeObjectAndNil(Clone_Pool);
  inherited Destroy;
end;

procedure TDT_P2PVM_Custom_Client.Progress;
begin
  Bind_PhysicsTunnel.Progress;
  DTClient.Progress;
  if (AutomatedConnection) and (Bind_PhysicsTunnel.RemoteInited) and (Bind_PhysicsTunnel.AutomatedP2PVMClientConnectionDone(Bind_PhysicsTunnel.ClientIO))
    and (not Connecting) and (Reconnection) and (not DTClient.LinkOk) then
      Connect(LastUser, LastPasswd);

  if Clone_Pool.Num > 0 then
    with Clone_Pool.Invert_Repeat_ do
      repeat
          queue^.Data.Progress;
      until not Prev;
end;

function TDT_P2PVM_Custom_Client.LoginIsSuccessed: Boolean;
begin
  Result := DTClient.LinkOk;
end;

procedure TDT_P2PVM_Custom_Client.DoTunnelLinkResult(const state: Boolean);
begin
  if Assigned(OnConnectResultState.On_C) then
      OnConnectResultState.On_C(state);
  if Assigned(OnConnectResultState.On_M) then
      OnConnectResultState.On_M(state);
  if Assigned(OnConnectResultState.On_P) then
      OnConnectResultState.On_P(state);
  OnConnectResultState.Init;
  Connecting := False;

  if state then
    begin
      RegisterUserAndLogin := False;

      if AutomatedConnection then
          Reconnection := True;

      if Assigned(OnTunnelLink) then
          OnTunnelLink(Self);
    end;
end;

procedure TDT_P2PVM_Custom_Client.Connect(User, passwd: SystemString);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Bind_PhysicsTunnel.RemoteInited then
    begin
      Connecting := False;
      Exit;
    end;
  LastUser := User;
  LastPasswd := passwd;
  OnConnectResultState.Init;
  if RegisterUserAndLogin then
      DTClient.RegisterUserM(LastUser, LastPasswd, DoRegisterResult)
  else
      DTClient.UserLoginM(LastUser, LastPasswd, DoLoginResult);
end;

procedure TDT_P2PVM_Custom_Client.Connect_C(User, passwd: SystemString; OnResult: TOnState_C);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Bind_PhysicsTunnel.RemoteInited then
    begin
      Connecting := False;
      OnResult(False);
      Exit;
    end;
  LastUser := User;
  LastPasswd := passwd;
  OnConnectResultState.Init;
  OnConnectResultState.On_C := OnResult;
  if RegisterUserAndLogin then
      DTClient.RegisterUserM(LastUser, LastPasswd, DoRegisterResult)
  else
      DTClient.UserLoginM(LastUser, LastPasswd, DoLoginResult);
end;

procedure TDT_P2PVM_Custom_Client.Connect_M(User, passwd: SystemString; OnResult: TOnState_M);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Bind_PhysicsTunnel.RemoteInited then
    begin
      Connecting := False;
      OnResult(False);
      Exit;
    end;
  LastUser := User;
  LastPasswd := passwd;
  OnConnectResultState.Init;
  OnConnectResultState.On_M := OnResult;
  if RegisterUserAndLogin then
      DTClient.RegisterUserM(LastUser, LastPasswd, DoRegisterResult)
  else
      DTClient.UserLoginM(LastUser, LastPasswd, DoLoginResult);
end;

procedure TDT_P2PVM_Custom_Client.Connect_P(User, passwd: SystemString; OnResult: TOnState_P);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Bind_PhysicsTunnel.RemoteInited then
    begin
      Connecting := False;
      OnResult(False);
      Exit;
    end;
  LastUser := User;
  LastPasswd := passwd;
  OnConnectResultState.Init;
  OnConnectResultState.On_P := OnResult;
  if RegisterUserAndLogin then
      DTClient.RegisterUserM(LastUser, LastPasswd, DoRegisterResult)
  else
      DTClient.UserLoginM(LastUser, LastPasswd, DoLoginResult);
end;

procedure TDT_P2PVM_Custom_Client.Disconnect;
begin
  Connecting := False;
  Reconnection := False;
  DTClient.Disconnect;

  if Clone_Pool.Num > 0 then
    with Clone_Pool.Invert_Repeat_ do
      repeat
          queue^.Data.Disconnect;
      until not Prev;
end;

end.

