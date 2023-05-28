{ ****************************************************************************** }
{ * double tunnel IO framework(Virtual Auth)                                   * }
{ ****************************************************************************** }

unit Z.Net.DoubleTunnelIO.VirtualAuth;

{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core,
  Z.ListEngine, Z.UnicodeMixedLib,
  Z.DFE, Z.MemoryStream, Z.Net, Z.Net.PhysicsIO,
  Z.TextDataEngine,
  Z.Status, Z.Cadencer, Z.Notify, Z.PascalStrings, Z.UPascalStrings, Z.Cipher;

type
  TDTService_VirtualAuth = class;
  TPeerClientUserDefineForRecvTunnel_VirtualAuth = class;
  TDTService_VirtualAuthClass = class of TDTService_VirtualAuth;

  TVirtualAuthIO = class(TCore_Object)
  private
    RecvIO_ID, SendIO_ID: Cardinal;
    AuthResult: TDFE;
    Done: Boolean;
  public
    Owner: TDTService_VirtualAuth;
    UserID, Passwd: SystemString;
    function Online: Boolean;
    function UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
    procedure Accept;
    procedure Reject;
    procedure Bye;
  end;

  TVirtualRegIO = class(TCore_Object)
  private
    RecvIO_ID, SendIO_ID: Cardinal;
    RegResult: TDFE;
    Done: Boolean;
  public
    Owner: TDTService_VirtualAuth;
    UserID, Passwd: SystemString;
    function Online: Boolean;
    function UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
    procedure Accept;
    procedure Reject;
    procedure Bye;
  end;

  TPeerClientUserDefineForSendTunnel_VirtualAuth = class(TPeerIOUserDefine)
  public
    RecvTunnel: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
    RecvTunnelID: Cardinal;
    DoubleTunnelService: TDTService_VirtualAuth;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;

    function LinkOk: Boolean;
    property BindOk: Boolean read LinkOk;
  end;

  TPeerClientUserDefineForRecvTunnel_VirtualAuth = class(TPeerIOUserDefine)
  private
    FCurrentFileStream: TCore_Stream;
    FCurrentReceiveFileName: SystemString;
  public
    SendTunnel: TPeerClientUserDefineForSendTunnel_VirtualAuth;
    SendTunnelID: Cardinal;
    DoubleTunnelService: TDTService_VirtualAuth;
    UserID, Passwd: SystemString;
    LoginSuccessed: Boolean;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;

    function LinkOk: Boolean;
    property BindOk: Boolean read LinkOk;
    property CurrentFileStream: TCore_Stream read FCurrentFileStream write FCurrentFileStream;
    property CurrentReceiveFileName: SystemString read FCurrentReceiveFileName write FCurrentReceiveFileName;
  end;

  TVirtualAuth_OnAuth = procedure(Sender: TDTService_VirtualAuth; AuthIO: TVirtualAuthIO) of object;
  TVirtualAuth_OnReg = procedure(Sender: TDTService_VirtualAuth; RegIO: TVirtualRegIO) of object;
  TVirtualAuth_OnLinkSuccess = procedure(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth) of object;
  TVirtualAuth_OnUserOut = procedure(Sender: TDTService_VirtualAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth) of object;

  TDTService_VirtualAuth = class(TCore_InterfacedObject)
  protected
    FRecvTunnel, FSendTunnel: TZNet_Server;
    FCadencerEngine: TCadencer;
    FProgressEngine: TN_Progress_Tool;
    FFileSystem: Boolean;
    FFileShareDirectory: SystemString;
    { event }
    FOnUserAuth: TVirtualAuth_OnAuth;
    FOnUserReg: TVirtualAuth_OnReg;
    FOnLinkSuccess: TVirtualAuth_OnLinkSuccess;
    FOnUserOut: TVirtualAuth_OnUserOut;
  protected
    { virtual event }
    procedure UserAuth(Sender: TVirtualAuthIO); virtual;
    procedure UserReg(Sender: TVirtualRegIO); virtual;
    procedure UserLoginSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); virtual;
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); virtual;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); virtual;
    procedure UserPostFileSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth; fn: SystemString); virtual;
  protected
    { registed server command }
    procedure Command_UserLogin(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_RegisterUser(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_TunnelLink(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetCurrentCadencer(Sender: TPeerIO; InData, OutData: TDFE); virtual;

    procedure Command_GetFileTime(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetFileInfo(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Do_Th_Command_GetFileMD5(ThSender: THPC_Stream; ThInData, ThOutData: TDFE);
    procedure Command_GetFileMD5(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetFile(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetFileAs(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_PostFileInfo(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_PostFile(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_PostFileOver(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_GetFileFragmentData(Sender: TPeerIO; InData, OutData: TDFE); virtual;

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

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;

    function GetUserDefineRecvTunnel(RecvCli: TPeerIO): TPeerClientUserDefineForRecvTunnel_VirtualAuth;

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

    property CadencerEngine: TCadencer read FCadencerEngine;

    property ProgressEngine: TN_Progress_Tool read FProgressEngine;
    property ProgressPost: TN_Progress_Tool read FProgressEngine;
    property PostProgress: TN_Progress_Tool read FProgressEngine;
    property PostRun: TN_Progress_Tool read FProgressEngine;
    property PostExecute: TN_Progress_Tool read FProgressEngine;

    property FileSystem: Boolean read FFileSystem write FFileSystem;
    property FileReceiveDirectory: SystemString read FFileShareDirectory write FFileShareDirectory;
    property PublicFileDirectory: SystemString read FFileShareDirectory write FFileShareDirectory;
    property FileShareDirectory: SystemString read FFileShareDirectory write FFileShareDirectory;

    property RecvTunnel: TZNet_Server read FRecvTunnel;
    property SendTunnel: TZNet_Server read FSendTunnel;

    property OnUserAuth: TVirtualAuth_OnAuth read FOnUserAuth write FOnUserAuth;
    property OnUserReg: TVirtualAuth_OnReg read FOnUserReg write FOnUserReg;
    property OnLinkSuccess: TVirtualAuth_OnLinkSuccess read FOnLinkSuccess write FOnLinkSuccess;
    property OnUserOut: TVirtualAuth_OnUserOut read FOnUserOut write FOnUserOut;
  end;

  TDTClient_VirtualAuth = class;
  TClientUserDefineForSendTunnel_VirtualAuth = class;
  TDTClient_VirtualAuthClass = class of TDTClient_VirtualAuth;

  TClientUserDefineForRecvTunnel_VirtualAuth = class(TPeerIOUserDefine)
  public
    Client: TDTClient_VirtualAuth;
    SendTunnel: TClientUserDefineForSendTunnel_VirtualAuth;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TClientUserDefineForSendTunnel_VirtualAuth = class(TPeerIOUserDefine)
  public
    Client: TDTClient_VirtualAuth;
    RecvTunnel: TClientUserDefineForRecvTunnel_VirtualAuth;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TGetFileInfo_C_VirtualAuth = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
  TGetFileInfo_M_VirtualAuth = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64) of object;
  TFileMD5_C_VirtualAuth = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5);
  TFileMD5_M_VirtualAuth = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5) of object;
  TFileComplete_C_VirtualAuth = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    stream: TCore_Stream; const fileName: SystemString);
  TFileComplete_M_VirtualAuth = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    stream: TCore_Stream; const fileName: SystemString) of object;
  TFileFragmentData_C_VirtualAuth = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5);
  TFileFragmentData_M_VirtualAuth = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5) of object;

{$IFDEF FPC}
  TGetFileInfo_P_VirtualAuth = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64) is nested;
  TFileMD5_P_VirtualAuth = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5) is nested;
  TFileComplete_P_VirtualAuth = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    stream: TCore_Stream; const fileName: SystemString) is nested;
  TFileFragmentData_P_VirtualAuth = procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5) is nested;
{$ELSE FPC}
  TGetFileInfo_P_VirtualAuth = reference to procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
  TFileMD5_P_VirtualAuth = reference to procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5);
  TFileComplete_P_VirtualAuth = reference to procedure(const UserData: Pointer; const UserObject: TCore_Object;
    stream: TCore_Stream; const fileName: SystemString);
  TFileFragmentData_P_VirtualAuth = reference to procedure(const UserData: Pointer; const UserObject: TCore_Object;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5);
{$ENDIF FPC}

  TDTClient_VirtualAuth = class(TCore_InterfacedObject, IZNet_ClientInterface)
  protected
    FSendTunnel, FRecvTunnel: TZNet_Client;
    FFileSystem: Boolean;
    FAutoFreeTunnel: Boolean;
    FLinkOk: Boolean;
    FWaitCommandTimeout: Cardinal;

    FCurrentStream: TCore_Stream;
    FCurrentReceiveStreamFileName: SystemString;

    FCadencerEngine: TCadencer;
    FProgressEngine: TN_Progress_Tool;

    FLastCadencerTime: Double;
    FServerDelay: Double;
  protected
    { client notify interface }
    procedure ClientConnected(Sender: TZNet_Client); virtual;
    procedure ClientDisconnect(Sender: TZNet_Client); virtual;
  public
    { registed client command }
    procedure Command_FileInfo(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_PostFile(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_PostFileOver(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_PostFileFragmentData(Sender: TPeerIO; InData: PByte; DataSize: NativeInt); virtual;

    procedure GetCurrentCadencer_StreamResult(Sender: TPeerIO; Result_: TDFE); virtual;

    procedure GetFileInfo_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE); virtual;
    procedure GetFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE); virtual;

    { Downloading files from the server asynchronously and triggering notifications when completed }
    procedure GetFile_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE); virtual;

    { Downloading file fragment data from the server asynchronously and triggering notifications when completed }
    procedure GetFileFragmentData_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE); virtual;

    { batch stream suppport }
    procedure Command_NewBatchStream(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_PostBatchStream(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_ClearBatchStream(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDFE); virtual;
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
    function Connect(addr: SystemString; const RecvPort, SendPort: Word): Boolean; overload; virtual;

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
    function UserLogin(UserID, Passwd: SystemString): Boolean; virtual;
    function RegisterUser(UserID, Passwd: SystemString): Boolean; virtual;
    function TunnelLink: Boolean; virtual;

    { async user login }
    procedure UserLoginC(UserID, Passwd: SystemString; On_C: TOnState_C); virtual;
    procedure UserLoginM(UserID, Passwd: SystemString; On_M: TOnState_M); virtual;
    procedure UserLoginP(UserID, Passwd: SystemString; On_P: TOnState_P); virtual;

    { async user registration }
    procedure RegisterUserC(UserID, Passwd: SystemString; On_C: TOnState_C); virtual;
    procedure RegisterUserM(UserID, Passwd: SystemString; On_M: TOnState_M); virtual;
    procedure RegisterUserP(UserID, Passwd: SystemString; On_P: TOnState_P); virtual;

    { async tunnel link }
    procedure TunnelLinkC(On_C: TOnState_C); virtual;
    procedure TunnelLinkM(On_M: TOnState_M); virtual;
    procedure TunnelLinkP(On_P: TOnState_P); virtual;

    { async mode SyncCadencer }
    procedure SyncCadencer; virtual;

    { remote file time }
    procedure GetFileTimeM(RemoteFilename: SystemString; On_CResult: TOnStream_M); overload;
    procedure GetFileTimeP(RemoteFilename: SystemString; On_CResult: TOnStream_P); overload;
    { remote file information }
    procedure GetFileInfoC(fileName: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_C_VirtualAuth); overload;
    procedure GetFileInfoM(fileName: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_M_VirtualAuth); overload;
    procedure GetFileInfoP(fileName: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_P_VirtualAuth); overload;
    { remote md5 support with public store space }
    procedure GetFileMD5C(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_C_VirtualAuth); overload;
    procedure GetFileMD5M(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_M_VirtualAuth); overload;
    procedure GetFileMD5P(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_P_VirtualAuth); overload;

    { normal download }
    procedure GetFileC(fileName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_C: TFileComplete_C_VirtualAuth); overload;
    procedure GetFileM(fileName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_M: TFileComplete_M_VirtualAuth); overload;
    procedure GetFileP(fileName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_P: TFileComplete_P_VirtualAuth); overload;
    { Synchronously waiting to download files from the server to complete }
    function GetFile(fileName, saveToPath: SystemString): Boolean; overload;
    { restore download }
    procedure GetFileC(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_C: TFileComplete_C_VirtualAuth); overload;
    procedure GetFileM(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_M: TFileComplete_M_VirtualAuth); overload;
    procedure GetFileP(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_P: TFileComplete_P_VirtualAuth); overload;
    procedure GetFileAsC(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_C: TFileComplete_C_VirtualAuth); overload;
    procedure GetFileAsM(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_M: TFileComplete_M_VirtualAuth); overload;
    procedure GetFileAsP(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_P: TFileComplete_P_VirtualAuth); overload;
    { Synchronously waiting to restore download files from the server to complete }
    function GetFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean; overload;

    { file fragment }
    procedure GetFileFragmentDataC(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_C: TFileFragmentData_C_VirtualAuth); overload;
    procedure GetFileFragmentDataM(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_M: TFileFragmentData_M_VirtualAuth); overload;
    procedure GetFileFragmentDataP(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_P: TFileFragmentData_P_VirtualAuth); overload;

    { automated download and verify }
    procedure AutomatedDownloadFileC(remoteFile, localFile: U_String; OnDownloadDone: TFileComplete_C_VirtualAuth);
    procedure AutomatedDownloadFileM(remoteFile, localFile: U_String; OnDownloadDone: TFileComplete_M_VirtualAuth);
    procedure AutomatedDownloadFileP(remoteFile, localFile: U_String; OnDownloadDone: TFileComplete_P_VirtualAuth);

    { Uploading local files asynchronously }
    procedure PostFile(fileName: SystemString); overload;
    { restore Uploading local files asynchronously }
    procedure PostFile(fileName: SystemString; StartPos: Int64); overload;
    { Upload an Stream asynchronously and automatically release Stream after completion }
    procedure PostFile(fn: SystemString; stream: TCore_Stream; doneFreeStream: Boolean); overload;
    { restore Upload an Stream asynchronously and automatically release Stream after completion }
    procedure PostFile(fn: SystemString; stream: TCore_Stream; StartPos: Int64; doneFreeStream: Boolean); overload;

    { automated Upload and verify }
    procedure AutomatedUploadFile(localFile: U_String);

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

    property CadencerEngine: TCadencer read FCadencerEngine;

    property ProgressEngine: TN_Progress_Tool read FProgressEngine;
    property ProgressPost: TN_Progress_Tool read FProgressEngine;
    property PostProgress: TN_Progress_Tool read FProgressEngine;
    property PostRun: TN_Progress_Tool read FProgressEngine;
    property PostExecute: TN_Progress_Tool read FProgressEngine;

    property ServerDelay: Double read FServerDelay;

    function RemoteInited: Boolean;

    property RecvTunnel: TZNet_Client read FRecvTunnel;
    property SendTunnel: TZNet_Client read FSendTunnel;
  end;

  TDT_P2PVM_VirtualAuth_OnState = record
    On_C: TOnState_C;
    On_M: TOnState_M;
    On_P: TOnState_P;
    procedure Init;
  end;

  PDT_P2PVM_VirtualAuth_OnState = ^TDT_P2PVM_VirtualAuth_OnState;

  TDT_P2PVM_VirtualAuth_Service = class(TCore_Object)
  private
    function GetQuietMode: Boolean;
    procedure SetQuietMode(const Value: Boolean);
  public
    RecvTunnel, SendTunnel: TZNet_WithP2PVM_Server;
    DTService: TDTService_VirtualAuth;
    PhysicsTunnel: TPhysicsServer;

    constructor Create(ServiceClass_: TDTService_VirtualAuthClass);
    destructor Destroy; override;
    procedure Progress; virtual;
    procedure StartService(ListenAddr, ListenPort, Auth: SystemString);
    procedure StopService;
    property QuietMode: Boolean read GetQuietMode write SetQuietMode;
  end;

  TDT_P2PVM_VirtualAuth_Client = class;
  TDT_P2PVM_VirtualAuth_ServicePool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDT_P2PVM_VirtualAuth_Service>;
  TOn_DT_P2PVM_VirtualAuth_Client_TunnelLink = procedure(Sender: TDT_P2PVM_VirtualAuth_Client) of object;

  TDT_P2PVM_VirtualAuth_Client = class(TCore_Object)
  private
    OnConnectResultState: TDT_P2PVM_VirtualAuth_OnState;
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
    DTClient: TDTClient_VirtualAuth;
    PhysicsTunnel: TPhysicsClient;
    LastAddr, LastPort, LastAuth: SystemString;
    LastUser, LastPasswd: SystemString;
    RegisterUserAndLogin: Boolean;
    AutomatedConnection: Boolean;
    OnTunnelLink: TOn_DT_P2PVM_VirtualAuth_Client_TunnelLink;

    constructor Create(ClientClass_: TDTClient_VirtualAuthClass);
    destructor Destroy; override;
    procedure Progress; virtual;
    procedure Connect(addr, Port, Auth, User, Passwd: SystemString);
    procedure Connect_C(addr, Port, Auth, User, Passwd: SystemString; OnResult: TOnState_C);
    procedure Connect_M(addr, Port, Auth, User, Passwd: SystemString; OnResult: TOnState_M);
    procedure Connect_P(addr, Port, Auth, User, Passwd: SystemString; OnResult: TOnState_P);
    procedure Disconnect;
    property QuietMode: Boolean read GetQuietMode write SetQuietMode;
  end;

  TDT_P2PVM_VirtualAuth_ClientPool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDT_P2PVM_VirtualAuth_Client>;

  TDT_P2PVM_VirtualAuth_Custom_Service = class;
  TDT_P2PVM_VirtualAuth_Custom_Service_Class = class of TDT_P2PVM_VirtualAuth_Custom_Service;

  TDT_P2PVM_VirtualAuth_Custom_Service = class(TCore_InterfacedObject)
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
    DTService: TDTService_VirtualAuth;

    constructor Create(ServiceClass_: TDTService_VirtualAuthClass; PhysicsTunnel_: TZNet_Server;
      P2PVM_Recv_Name_, P2PVM_Recv_IP6_, P2PVM_Recv_Port_,
      P2PVM_Send_Name_, P2PVM_Send_IP6_, P2PVM_Send_Port_: SystemString); virtual;
    destructor Destroy; override;
    procedure Progress; virtual;
    procedure StartService(); virtual;
    procedure StopService(); virtual;
    property QuietMode: Boolean read GetQuietMode write SetQuietMode;
  end;

  TDT_P2PVM_VirtualAuth_Custom_ServicePool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDT_P2PVM_VirtualAuth_Custom_Service>;

  TDT_P2PVM_VirtualAuth_Custom_Client = class;
  TDT_P2PVM_VirtualAuth_Custom_Client_Class = class of TDT_P2PVM_VirtualAuth_Custom_Client;
  TOn_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink = procedure(Sender: TDT_P2PVM_VirtualAuth_Custom_Client) of object;

  TDT_P2PVM_VirtualAuth_Custom_Client_Clone_Pool_ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<TDT_P2PVM_VirtualAuth_Custom_Client>;

  TDT_P2PVM_VirtualAuth_Custom_Client_Clone_Pool = class(TDT_P2PVM_VirtualAuth_Custom_Client_Clone_Pool_)
  public
    procedure DoFree(var Data: TDT_P2PVM_VirtualAuth_Custom_Client); override;
  end;

  TDT_P2PVM_VirtualAuth_Custom_Client = class(TCore_InterfacedObject)
  private
    OnConnectResultState: TDT_P2PVM_VirtualAuth_OnState;
    Connecting: Boolean;
    Reconnection: Boolean;
    procedure DoRegisterResult(const state: Boolean);
    procedure DoLoginResult(const state: Boolean);
    function GetQuietMode: Boolean;
    procedure SetQuietMode(const Value: Boolean);
  private
    // clone Technology
    Parent_Client: TDT_P2PVM_VirtualAuth_Custom_Client;
    Clone_Instance_Ptr: TDT_P2PVM_VirtualAuth_Custom_Client_Clone_Pool_.PQueueStruct;
    procedure Do_Recv_Connect_State(const state: Boolean);
    procedure Do_Send_Connect_State(const state: Boolean);
  public
    Clone_Pool: TDT_P2PVM_VirtualAuth_Custom_Client_Clone_Pool;
    // bind
    Bind_PhysicsTunnel: TZNet_Client;
    Bind_P2PVM_Recv_IP6: SystemString;
    Bind_P2PVM_Recv_Port: Word;
    Bind_P2PVM_Send_IP6: SystemString;
    Bind_P2PVM_Send_Port: Word;
    // local
    ClientClass: TDTClient_VirtualAuthClass;
    RecvTunnel, SendTunnel: TZNet_WithP2PVM_Client;
    DTClient: TDTClient_VirtualAuth;
    LastUser, LastPasswd: SystemString;
    RegisterUserAndLogin: Boolean;
    AutomatedConnection: Boolean;
    OnTunnelLink: TOn_DT_P2PVM_VirtualAuth_Custom_Client_TunnelLink;

    constructor Create(ClientClass_: TDTClient_VirtualAuthClass; PhysicsTunnel_: TZNet_Client;
      P2PVM_Recv_Name_, P2PVM_Recv_IP6_, P2PVM_Recv_Port_,
      P2PVM_Send_Name_, P2PVM_Send_IP6_, P2PVM_Send_Port_: SystemString); virtual;
    constructor Create_Clone(Parent_Client_: TDT_P2PVM_VirtualAuth_Custom_Client); virtual;
    destructor Destroy; override;
    procedure Progress;
    function LoginIsSuccessed: Boolean;
    procedure DoTunnelLinkResult(const state: Boolean);
    procedure Connect(User, Passwd: SystemString); overload;
    procedure Connect(); overload;
    procedure Connect_C(User, Passwd: SystemString; OnResult: TOnState_C); overload;
    procedure Connect_C(OnResult: TOnState_C); overload;
    procedure Connect_M(User, Passwd: SystemString; OnResult: TOnState_M); overload;
    procedure Connect_M(OnResult: TOnState_M); overload;
    procedure Connect_P(User, Passwd: SystemString; OnResult: TOnState_P); overload;
    procedure Connect_P(OnResult: TOnState_P); overload;
    procedure Disconnect;
    property QuietMode: Boolean read GetQuietMode write SetQuietMode;
  end;

  TDT_P2PVM_VirtualAuth_Custom_ClientPool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDT_P2PVM_VirtualAuth_Custom_Client>;

  PGetFileInfoStruct_VirtualAuth = ^TGetFileInfoStruct_VirtualAuth;

  TGetFileInfoStruct_VirtualAuth = record
    UserData: Pointer;
    UserObject: TCore_Object;
    fileName: SystemString;
    OnComplete_C: TGetFileInfo_C_VirtualAuth;
    OnComplete_M: TGetFileInfo_M_VirtualAuth;
    OnComplete_P: TGetFileInfo_P_VirtualAuth;
  end;

  PFileMD5Struct_VirtualAuth = ^TFileMD5Struct_VirtualAuth;

  TFileMD5Struct_VirtualAuth = record
    UserData: Pointer;
    UserObject: TCore_Object;
    fileName: SystemString;
    StartPos, EndPos: Int64;
    OnComplete_C: TFileMD5_C_VirtualAuth;
    OnComplete_M: TFileMD5_M_VirtualAuth;
    OnComplete_P: TFileMD5_P_VirtualAuth;
  end;

  PRemoteFileBackcall_VirtualAuth = ^TRemoteFileBackcall_VirtualAuth;

  TRemoteFileBackcall_VirtualAuth = record
    UserData: Pointer;
    UserObject: TCore_Object;
    OnComplete_C: TFileComplete_C_VirtualAuth;
    OnComplete_M: TFileComplete_M_VirtualAuth;
    OnComplete_P: TFileComplete_P_VirtualAuth;
  end;

  PFileFragmentDataBackcall_VirtualAuth = ^TFileFragmentDataBackcall_VirtualAuth;

  TFileFragmentDataBackcall_VirtualAuth = record
    UserData: Pointer;
    UserObject: TCore_Object;
    fileName: SystemString;
    StartPos, EndPos: Int64;
    OnComplete_C: TFileFragmentData_C_VirtualAuth;
    OnComplete_M: TFileFragmentData_M_VirtualAuth;
    OnComplete_P: TFileFragmentData_P_VirtualAuth;
  end;

  TAutomatedDownloadFile_Struct_VirtualAuth = class
  private
    remoteFile, localFile: SystemString;
    OnDownloadDoneC: TFileComplete_C_VirtualAuth;
    OnDownloadDoneM: TFileComplete_M_VirtualAuth;
    OnDownloadDoneP: TFileComplete_P_VirtualAuth;
    Client: TDTClient_VirtualAuth;
    r_fileName: SystemString;
    r_fileExisted: Boolean;
    r_fileSize: Int64;
    r_fileMD5: Z.UnicodeMixedLib.TMD5;
    l_fileMD5: Z.UnicodeMixedLib.TMD5;
    procedure DoComplete(const UserData: Pointer; const UserObject: TCore_Object; stream: TCore_Stream; const fileName: SystemString);
    procedure DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCore_Object; const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
    procedure Do_Th_ComputeLFileMD5();
    procedure Done_ComputeLFileMD5();
    procedure DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCore_Object; const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: Z.UnicodeMixedLib.TMD5);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAutomatedUploadFile_Struct_VirtualAuth = class
  private
    localFile: SystemString;
    Client: TDTClient_VirtualAuth;
    r_fileName: SystemString;
    r_fileExisted: Boolean;
    r_fileSize: Int64;
    r_fileMD5: Z.UnicodeMixedLib.TMD5;
    l_file_StartPos, l_file_EndPos: Int64;
    l_fileMD5: Z.UnicodeMixedLib.TMD5;
    procedure DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCore_Object; const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
    procedure Do_Th_ComputeLFileMD5();
    procedure Done_ComputeLFileMD5();
    procedure DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCore_Object; const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: Z.UnicodeMixedLib.TMD5);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TZNet_DoubleTunnelService_VirtualAuth = TDTService_VirtualAuth;
  TZNet_DoubleTunnelClient_VirtualAuth = TDTClient_VirtualAuth;

implementation

uses SysUtils;

procedure TAutomatedDownloadFile_Struct_VirtualAuth.DoComplete(const UserData: Pointer; const UserObject: TCore_Object; stream: TCore_Stream; const fileName: SystemString);
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

procedure TAutomatedDownloadFile_Struct_VirtualAuth.DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCore_Object;
  const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
begin
  if Existed then
    begin
      r_fileName := fileName;
      r_fileSize := fSiz;
      if not umlFileExists(localFile) then
          Client.GetFileAsM(remoteFile, umlGetFileName(localFile), 0, umlGetFilePath(localFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}DoComplete)
      else if fSiz >= umlGetFileSize(localFile) then
        begin
          umlCacheFileMD5(localFile);
          Client.GetFileMD5M(umlGetFileName(remoteFile), 0, umlGetFileSize(localFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}DoResult_GetFileMD5);
        end
      else
          Client.GetFileAsM(remoteFile, umlGetFileName(localFile), 0, umlGetFilePath(localFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}DoComplete);
    end
  else
    begin
      DoStatus('no found remote file: "%s" ', [remoteFile]);
      DelayFreeObj(1.0, Self);
    end;
end;

procedure TAutomatedDownloadFile_Struct_VirtualAuth.Do_Th_ComputeLFileMD5;
begin
  DoStatus('compute md5 from local "%s"', [localFile]);
  l_fileMD5 := umlFileMD5(localFile);
  TCompute.PostM1({$IFDEF FPC}@{$ENDIF FPC}Done_ComputeLFileMD5);
end;

procedure TAutomatedDownloadFile_Struct_VirtualAuth.Done_ComputeLFileMD5;
begin
  if umlMD5Compare(l_fileMD5, r_fileMD5) then
    begin
      if r_fileSize = umlGetFileSize(localFile) then
          DoComplete(nil, nil, nil, localFile)
      else
          Client.GetFileAsM(r_fileName, umlGetFileName(localFile), umlGetFileSize(localFile), umlGetFilePath(localFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}DoComplete);
    end
  else
      Client.GetFileAsM(r_fileName, umlGetFileName(localFile), 0, umlGetFilePath(localFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}DoComplete);
end;

procedure TAutomatedDownloadFile_Struct_VirtualAuth.DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCore_Object;
  const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: Z.UnicodeMixedLib.TMD5);
begin
  r_fileMD5 := MD5;
  TCompute.RunM_NP({$IFDEF FPC}@{$ENDIF FPC}Do_Th_ComputeLFileMD5);
end;

constructor TAutomatedDownloadFile_Struct_VirtualAuth.Create;
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

destructor TAutomatedDownloadFile_Struct_VirtualAuth.Destroy;
begin
  remoteFile := '';
  localFile := '';
  r_fileName := '';
  inherited Destroy;
end;

procedure TAutomatedUploadFile_Struct_VirtualAuth.DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCore_Object;
  const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
begin
  r_fileExisted := Existed;

  if Existed then
    begin
      r_fileName := fileName;
      r_fileSize := fSiz;
      if r_fileSize <= umlGetFileSize(localFile) then
          Client.GetFileMD5M(umlGetFileName(localFile), 0, r_fileSize, nil, nil, {$IFDEF FPC}@{$ENDIF FPC}DoResult_GetFileMD5)
      else
        begin
          Client.PostFile(localFile);
          DelayFreeObj(1.0, Self);
        end;
    end
  else
    begin
      Client.PostFile(localFile);
      DelayFreeObj(1.0, Self);
    end;
end;

procedure TAutomatedUploadFile_Struct_VirtualAuth.Do_Th_ComputeLFileMD5;
begin
  DoStatus('compute md5 from local "%s"', [localFile]);
  l_fileMD5 := umlFileMD5(localFile, l_file_StartPos, l_file_EndPos);
  TCompute.PostM1({$IFDEF FPC}@{$ENDIF FPC}Done_ComputeLFileMD5);
end;

procedure TAutomatedUploadFile_Struct_VirtualAuth.Done_ComputeLFileMD5;
begin
  if umlMD5Compare(r_fileMD5, l_fileMD5) then
    begin
      if umlGetFileSize(localFile) > r_fileSize then
          Client.PostFile(localFile, r_fileSize);
    end
  else
      Client.PostFile(localFile);
  DelayFreeObj(1.0, Self);
end;

procedure TAutomatedUploadFile_Struct_VirtualAuth.DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCore_Object;
  const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: Z.UnicodeMixedLib.TMD5);
begin
  r_fileMD5 := MD5;
  l_file_StartPos := StartPos;
  l_file_EndPos := EndPos;
  TCompute.RunM_NP({$IFDEF FPC}@{$ENDIF FPC}Do_Th_ComputeLFileMD5);
end;

constructor TAutomatedUploadFile_Struct_VirtualAuth.Create;
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

destructor TAutomatedUploadFile_Struct_VirtualAuth.Destroy;
begin
  localFile := '';
  r_fileName := '';
  inherited Destroy;
end;

function TVirtualAuthIO.Online: Boolean;
begin
  Result := Owner.RecvTunnel.Exists(RecvIO_ID);
end;

function TVirtualAuthIO.UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
begin
  Result := nil;
  if not Online then
      exit;
  Result := Owner.RecvTunnel[RecvIO_ID].UserDefine as TPeerClientUserDefineForRecvTunnel_VirtualAuth;
end;

procedure TVirtualAuthIO.Accept;
var
  IO: TPeerIO;
  n: SystemString;
begin
  if AuthResult <> nil then
    begin
      UserDefineIO.UserID := UserID;
      UserDefineIO.Passwd := Passwd;
      UserDefineIO.LoginSuccessed := True;
      AuthResult.WriteBool(True);
      AuthResult.WriteString(PFormat('success Login:%s', [UserID]));
      Owner.UserLoginSuccess(UserDefineIO);
      Done := True;
      exit;
    end
  else if Online then
    begin
      IO := Owner.RecvTunnel.PeerIO[RecvIO_ID];
      if (IO.ResultSendIsPaused) then
        begin
          UserDefineIO.UserID := UserID;
          UserDefineIO.Passwd := Passwd;
          UserDefineIO.LoginSuccessed := True;
          IO.OutDataFrame.WriteBool(True);
          IO.OutDataFrame.WriteString(PFormat('success Login:%s', [UserID]));
          IO.ContinueResultSend;
          Owner.UserLoginSuccess(UserDefineIO);
        end;
    end;
  DelayFreeObj(1.0, Self);
end;

procedure TVirtualAuthIO.Reject;
var
  IO: TPeerIO;
  r_IO, s_IO: TPeerIO;
begin
  if AuthResult <> nil then
    begin
      UserDefineIO.UserID := UserID;
      UserDefineIO.Passwd := Passwd;
      UserDefineIO.LoginSuccessed := False;
      AuthResult.WriteBool(False);
      AuthResult.WriteString(PFormat('Reject user:%s', [UserID]));
      Done := True;
      exit;
    end
  else if Online then
    begin
      IO := Owner.RecvTunnel.PeerIO[RecvIO_ID];
      if (IO.ResultSendIsPaused) then
        begin
          UserDefineIO.UserID := UserID;
          UserDefineIO.Passwd := Passwd;
          UserDefineIO.LoginSuccessed := False;
          IO.OutDataFrame.WriteBool(False);
          IO.OutDataFrame.WriteString(PFormat('Reject user:%s', [UserID]));
          IO.ContinueResultSend;
        end;
    end;

  DelayFreeObj(1.0, Self);
end;

procedure TVirtualAuthIO.Bye;
var
  r_IO, s_IO: TPeerIO;
begin
  r_IO := Owner.RecvTunnel.PeerIO[RecvIO_ID];
  if r_IO <> nil then
      r_IO.delayClose(1.0);

  DelayFreeObj(1.0, Self);
end;

function TVirtualRegIO.Online: Boolean;
begin
  Result := Owner.RecvTunnel.Exists(RecvIO_ID);
end;

function TVirtualRegIO.UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
begin
  Result := nil;
  if not Online then
      exit;
  Result := Owner.RecvTunnel[RecvIO_ID].UserDefine as TPeerClientUserDefineForRecvTunnel_VirtualAuth;
end;

procedure TVirtualRegIO.Accept;
var
  IO: TPeerIO;
  n: SystemString;
begin
  if RegResult <> nil then
    begin
      RegResult.WriteBool(True);
      RegResult.WriteString(PFormat('success Reg:%s', [UserID]));
      Done := True;
      exit;
    end
  else if Online then
    begin
      IO := Owner.RecvTunnel.PeerIO[RecvIO_ID];
      if (IO.ResultSendIsPaused) then
        begin
          IO.OutDataFrame.WriteBool(True);
          IO.OutDataFrame.WriteString(PFormat('success Reg:%s', [UserID]));
          IO.ContinueResultSend;
        end;
    end;
  DelayFreeObj(1.0, Self);
end;

procedure TVirtualRegIO.Reject;
var
  IO: TPeerIO;
  r_IO, s_IO: TPeerIO;
begin
  if RegResult <> nil then
    begin
      RegResult.WriteBool(False);
      RegResult.WriteString(PFormat('Reject Reg:%s', [UserID]));
      Done := True;
      exit;
    end
  else if Online then
    begin
      IO := Owner.RecvTunnel.PeerIO[RecvIO_ID];
      if (IO.ResultSendIsPaused) then
        begin
          IO.OutDataFrame.WriteBool(False);
          IO.OutDataFrame.WriteString(PFormat('Reject Reg:%s', [UserID]));
          IO.ContinueResultSend;
        end;
    end;

  DelayFreeObj(1.0, Self);
end;

procedure TVirtualRegIO.Bye;
var
  r_IO, s_IO: TPeerIO;
begin
  r_IO := Owner.RecvTunnel.PeerIO[RecvIO_ID];
  if r_IO <> nil then
      r_IO.delayClose(1.0);

  DelayFreeObj(1.0, Self);
end;

constructor TPeerClientUserDefineForSendTunnel_VirtualAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  RecvTunnel := nil;
  RecvTunnelID := 0;
  DoubleTunnelService := nil;
end;

destructor TPeerClientUserDefineForSendTunnel_VirtualAuth.Destroy;
begin
  if (DoubleTunnelService <> nil) and (RecvTunnelID > 0) and (RecvTunnel <> nil) then
    begin
      if DoubleTunnelService.FRecvTunnel.Exists(RecvTunnelID) then
          DoubleTunnelService.FRecvTunnel.PeerIO[RecvTunnelID].Disconnect;
    end;
  inherited Destroy;
end;

function TPeerClientUserDefineForSendTunnel_VirtualAuth.LinkOk: Boolean;
begin
  Result := DoubleTunnelService <> nil;
end;

constructor TPeerClientUserDefineForRecvTunnel_VirtualAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  SendTunnel := nil;
  SendTunnelID := 0;
  DoubleTunnelService := nil;
  FCurrentFileStream := nil;
  FCurrentReceiveFileName := '';
  UserID := '';
  Passwd := '';
  LoginSuccessed := False;
end;

destructor TPeerClientUserDefineForRecvTunnel_VirtualAuth.Destroy;
begin
  if DoubleTunnelService <> nil then
    begin
      DoubleTunnelService.UserOut(Self);

      if (DoubleTunnelService <> nil) and (SendTunnelID > 0) and (SendTunnel <> nil) then
        begin
          if DoubleTunnelService.FSendTunnel.Exists(SendTunnelID) then
              DoubleTunnelService.FSendTunnel.PeerIO[SendTunnelID].Disconnect;
        end;
      DoubleTunnelService := nil;
    end;

  if FCurrentFileStream <> nil then
      DisposeObject(FCurrentFileStream);
  FCurrentFileStream := nil;
  inherited Destroy;
end;

function TPeerClientUserDefineForRecvTunnel_VirtualAuth.LinkOk: Boolean;
begin
  Result := DoubleTunnelService <> nil;
end;

procedure TDTService_VirtualAuth.UserAuth(Sender: TVirtualAuthIO);
begin
  try
    if Assigned(FOnUserAuth) then
        FOnUserAuth(Self, Sender);
  except
  end;
end;

procedure TDTService_VirtualAuth.UserReg(Sender: TVirtualRegIO);
begin
  try
    if Assigned(FOnUserReg) then
        FOnUserReg(Self, Sender);
  except
  end;
end;

procedure TDTService_VirtualAuth.UserLoginSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
end;

procedure TDTService_VirtualAuth.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  try
    if Assigned(FOnLinkSuccess) then
        FOnLinkSuccess(Self, UserDefineIO);
  except
  end;
end;

procedure TDTService_VirtualAuth.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  try
    if Assigned(FOnUserOut) then
        FOnUserOut(Self, UserDefineIO);
  except
  end;
end;

procedure TDTService_VirtualAuth.UserPostFileSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth; fn: SystemString);
begin
end;

procedure TDTService_VirtualAuth.Command_UserLogin(Sender: TPeerIO; InData, OutData: TDFE);
var
  SendTunnelID: Cardinal;
  UserID, UserPasswd: SystemString;
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
  AuthIO: TVirtualAuthIO;
begin
  SendTunnelID := InData.Reader.ReadCardinal;
  UserID := InData.Reader.ReadString;
  UserPasswd := InData.Reader.ReadString;

  UserDefineIO := GetUserDefineRecvTunnel(Sender);

  if not FSendTunnel.Exists(SendTunnelID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('send tunnel Illegal:%d', [SendTunnelID]));
      exit;
    end;

  AuthIO := TVirtualAuthIO.Create;
  AuthIO.Owner := Self;
  AuthIO.RecvIO_ID := Sender.ID;
  AuthIO.SendIO_ID := SendTunnelID;
  AuthIO.AuthResult := OutData;
  AuthIO.Done := False;
  AuthIO.UserID := UserID;
  AuthIO.Passwd := UserPasswd;

  try
      UserAuth(AuthIO);
  except
  end;

  if AuthIO.Done then
    begin
      DisposeObject(AuthIO);
    end
  else
    begin
      AuthIO.AuthResult := nil;
      Sender.PauseResultSend;
    end;
end;

procedure TDTService_VirtualAuth.Command_RegisterUser(Sender: TPeerIO; InData, OutData: TDFE);
var
  SendTunnelID: Cardinal;
  UserID, UserPasswd: SystemString;
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
  RegIO: TVirtualRegIO;
begin
  SendTunnelID := InData.Reader.ReadCardinal;
  UserID := InData.Reader.ReadString;
  UserPasswd := InData.Reader.ReadString;

  UserDefineIO := GetUserDefineRecvTunnel(Sender);

  if not FSendTunnel.Exists(SendTunnelID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('send tunnel Illegal:%d', [SendTunnelID]));
      exit;
    end;

  RegIO := TVirtualRegIO.Create;
  RegIO.Owner := Self;
  RegIO.RecvIO_ID := Sender.ID;
  RegIO.SendIO_ID := SendTunnelID;
  RegIO.RegResult := OutData;
  RegIO.Done := False;
  RegIO.UserID := UserID;
  RegIO.Passwd := UserPasswd;

  try
      UserReg(RegIO);
  except
  end;

  if RegIO.Done then
    begin
      DisposeObject(RegIO);
    end
  else
    begin
      RegIO.RegResult := nil;
      Sender.PauseResultSend;
    end;
end;

procedure TDTService_VirtualAuth.Command_TunnelLink(Sender: TPeerIO; InData, OutData: TDFE);
var
  RecvID, SendID: Cardinal;
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
begin
  RecvID := InData.Reader.ReadCardinal;
  SendID := InData.Reader.ReadCardinal;

  UserDefineIO := GetUserDefineRecvTunnel(Sender);

  if not FSendTunnel.Exists(SendID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('send tunnel Illegal:%d', [SendID]));
      OutData.WriteBool(FFileSystem);
      exit;
    end;

  if not FRecvTunnel.Exists(RecvID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('received tunnel Illegal:%d', [RecvID]));
      OutData.WriteBool(FFileSystem);
      exit;
    end;

  if Sender.ID <> RecvID then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('received tunnel Illegal:%d-%d', [Sender.ID, RecvID]));
      OutData.WriteBool(FFileSystem);
      exit;
    end;

  UserDefineIO.SendTunnel := FSendTunnel.PeerIO[SendID].UserDefine as TPeerClientUserDefineForSendTunnel_VirtualAuth;
  UserDefineIO.SendTunnelID := SendID;
  UserDefineIO.DoubleTunnelService := Self;

  UserDefineIO.SendTunnel.RecvTunnel := UserDefineIO;
  UserDefineIO.SendTunnel.RecvTunnelID := RecvID;
  UserDefineIO.SendTunnel.DoubleTunnelService := Self;

  OutData.WriteBool(True);
  OutData.WriteString(PFormat('tunnel link success! received:%d <-> send:%d', [RecvID, SendID]));
  OutData.WriteBool(FFileSystem);

  UserLinkSuccess(UserDefineIO);
end;

procedure TDTService_VirtualAuth.Command_GetCurrentCadencer(Sender: TPeerIO; InData, OutData: TDFE);
begin
  FCadencerEngine.Progress;
  OutData.WriteDouble(FCadencerEngine.CurrentTime);
end;

procedure TDTService_VirtualAuth.Command_GetFileTime(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
  fullfn, fileName: SystemString;
begin
  if not FFileSystem then
      exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      exit;

  fileName := InData.Reader.ReadString;
  fullfn := umlCombineFileName(FFileShareDirectory, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      exit;
    end;
  OutData.WriteBool(True);
  OutData.WriteString(fileName);
  OutData.WriteDouble(umlGetFileTime(fullfn));
end;

procedure TDTService_VirtualAuth.Command_GetFileInfo(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
  fullfn, fileName: SystemString;
begin
  if not FFileSystem then
      exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      exit;

  fileName := InData.Reader.ReadString;

  fullfn := umlCombineFileName(FFileShareDirectory, fileName);
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

procedure TDTService_VirtualAuth.Do_Th_Command_GetFileMD5(ThSender: THPC_Stream; ThInData, ThOutData: TDFE);
var
  fullfn, fileName: SystemString;
  StartPos, EndPos: Int64;
  fs: TCore_FileStream;
  MD5: TMD5;
begin
  fileName := ThInData.Reader.ReadString;
  StartPos := ThInData.Reader.ReadInt64;
  EndPos := ThInData.Reader.ReadInt64;

  fullfn := umlCombineFileName(FFileShareDirectory, fileName);
  if not umlFileExists(fullfn) then
    begin
      ThOutData.WriteBool(False);
      exit;
    end;

  try
      fs := TCore_FileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
    ThOutData.WriteBool(False);
    exit;
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

procedure TDTService_VirtualAuth.Command_GetFileMD5(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      exit;

  RunHPC_StreamM(Sender, nil, nil, InData, OutData, {$IFDEF FPC}@{$ENDIF FPC}Do_Th_Command_GetFileMD5);
end;

procedure TDTService_VirtualAuth.Command_GetFile(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
  fullfn, fileName, remoteinfo: SystemString;
  StartPos: Int64;
  RemoteBackcallAddr: UInt64;
  sendDE: TDFE;
  fs: TCore_FileStream;
begin
  if not FFileSystem then
      exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      exit;

  fileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(FFileShareDirectory, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('filename invailed %s', [fileName]));
      exit;
    end;

  try
      fs := TCore_FileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
      exit;
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

procedure TDTService_VirtualAuth.Command_GetFileAs(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
  fullfn, fileName, saveFileName, remoteinfo: SystemString;
  StartPos: Int64;
  RemoteBackcallAddr: UInt64;
  sendDE: TDFE;
  fs: TCore_FileStream;
begin
  if not FFileSystem then
      exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      exit;

  fileName := InData.Reader.ReadString;
  saveFileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(FFileShareDirectory, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('filename invailed %s', [fileName]));
      exit;
    end;

  try
      fs := TCore_FileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
      exit;
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

procedure TDTService_VirtualAuth.Command_PostFileInfo(Sender: TPeerIO; InData: TDFE);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
  fn: SystemString;
  StartPos: Int64;
  FSize: Int64;
  fullfn: SystemString;
begin
  if not FFileSystem then
      exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
    begin
      Sender.delayClose();
      exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      DisposeObject(UserDefineIO.FCurrentFileStream);
      UserDefineIO.FCurrentFileStream := nil;
    end;

  fn := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  FSize := InData.Reader.ReadInt64;

  fullfn := umlCombineFileName(FFileShareDirectory, fn);
  UserDefineIO.FCurrentReceiveFileName := fullfn;
  try
    if (StartPos > 0) and (umlFileExists(fullfn)) then
      begin
        UserDefineIO.FCurrentFileStream := TCore_FileStream.Create(fullfn, fmOpenReadWrite);
        if StartPos <= UserDefineIO.FCurrentFileStream.Size then
            UserDefineIO.FCurrentFileStream.Position := StartPos
        else
            UserDefineIO.FCurrentFileStream.Position := UserDefineIO.FCurrentFileStream.Size;
        Sender.Print(PFormat('restore post to public: %s', [fullfn]));
      end
    else
      begin
        UserDefineIO.FCurrentFileStream := TCore_FileStream.Create(fullfn, fmCreate);
        Sender.Print(PFormat('normal post to public: %s', [fullfn]));
      end;
  except
    Sender.Print('post file failed: %s', [fullfn]);
    UserDefineIO.FCurrentFileStream := nil;
  end;
end;

procedure TDTService_VirtualAuth.Command_PostFile(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
    begin
      Sender.delayClose();
      exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      InData.Position := 0;
      if InData.Size > 0 then
          UserDefineIO.FCurrentFileStream.CopyFrom(InData, InData.Size);
    end;
end;

procedure TDTService_VirtualAuth.Command_PostFileOver(Sender: TPeerIO; InData: TDFE);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
  fn: SystemString;
begin
  if not FFileSystem then
      exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
    begin
      Sender.delayClose();
      exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      fn := UserDefineIO.FCurrentReceiveFileName;
      DisposeObject(UserDefineIO.FCurrentFileStream);
      UserDefineIO.FCurrentFileStream := nil;

      Sender.Print('Received File Completed:%s', [fn]);
      UserPostFileSuccess(UserDefineIO, fn);
    end;
end;

procedure TDTService_VirtualAuth.Command_GetFileFragmentData(Sender: TPeerIO; InData, OutData: TDFE);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
  fullfn, fileName: SystemString;
  StartPos, EndPos, siz, fp: Int64;
  RemoteBackcallAddr: UInt64;
  fs: TCore_FileStream;
  mem_: TMS64;
  MD5: TMD5;
begin
  if not FFileSystem then
      exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      exit;

  fileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  EndPos := InData.Reader.ReadInt64;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(FFileShareDirectory, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      exit;
    end;

  try
      fs := TCore_FileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
    OutData.WriteBool(False);
    exit;
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
      exit;
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

procedure TDTService_VirtualAuth.Command_NewBatchStream(Sender: TPeerIO; InData: TDFE);
var
  RT: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
  p: PBigStreamBatchPostData;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      exit;
  p := RT.BigStreamBatchList.NewPostData;
  p^.RemoteMD5 := InData.Reader.ReadMD5;
  p^.CompletedBackcallPtr := InData.Reader.ReadPointer;
end;

procedure TDTService_VirtualAuth.Command_PostBatchStream(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  RT: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
  p: PBigStreamBatchPostData;
  de: TDFE;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      exit;

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

procedure TDTService_VirtualAuth.Command_ClearBatchStream(Sender: TPeerIO; InData: TDFE);
var
  RT: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      exit;
  RT.BigStreamBatchList.Clear;
end;

procedure TDTService_VirtualAuth.Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDFE);
var
  RT: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
  rMD5, sMD5: TMD5;
  backCallVal: UInt64;
  backCallValPtr: POnStateStruct;
  MD5Verify: Boolean;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      exit;

  rMD5 := InData.Reader.ReadMD5;
  sMD5 := InData.Reader.ReadMD5;
  backCallVal := InData.Reader.ReadPointer;

  backCallValPtr := POnStateStruct(Pointer(backCallVal));
  MD5Verify := umlMD5Compare(rMD5, sMD5);

  if backCallValPtr = nil then
      exit;

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

procedure TDTService_VirtualAuth.Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDFE);
var
  RT: TPeerClientUserDefineForRecvTunnel_VirtualAuth;
  i: Integer;
  p: PBigStreamBatchPostData;

  de: TDFE;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      exit;

  for i := 0 to RT.BigStreamBatchList.Count - 1 do
    begin
      p := RT.BigStreamBatchList[i];
      de := TDFE.Create;
      p^.Encode(de);
      OutData.WriteDataFrame(de);
      DisposeObject(de);
    end;
end;

constructor TDTService_VirtualAuth.Create(RecvTunnel_, SendTunnel_: TZNet_Server);
begin
  inherited Create;
  FRecvTunnel := RecvTunnel_;
  FRecvTunnel.PeerClientUserDefineClass := TPeerClientUserDefineForRecvTunnel_VirtualAuth;
  FSendTunnel := SendTunnel_;
  FSendTunnel.PeerClientUserDefineClass := TPeerClientUserDefineForSendTunnel_VirtualAuth;

  FRecvTunnel.DoubleChannelFramework := Self;
  FSendTunnel.DoubleChannelFramework := Self;

  FCadencerEngine := TCadencer.Create;
  FCadencerEngine.OnProgress := {$IFDEF FPC}@{$ENDIF FPC}CadencerProgress;
  FProgressEngine := TN_Progress_Tool.Create;

  FFileSystem := {$IFDEF DoubleIOFileSystem}True{$ELSE DoubleIOFileSystem}False{$ENDIF DoubleIOFileSystem};
  FFileShareDirectory := umlCurrentPath;

  if not umlDirectoryExists(FFileShareDirectory) then
      umlCreateDirectory(FFileShareDirectory);

  SwitchAsDefaultPerformance;

  FOnUserAuth := nil;
  FOnUserReg := nil;
  FOnLinkSuccess := nil;
  FOnUserOut := nil;
end;

destructor TDTService_VirtualAuth.Destroy;
begin
  DisposeObject(FCadencerEngine);
  DisposeObject(FProgressEngine);
  inherited Destroy;
end;

procedure TDTService_VirtualAuth.SwitchAsMaxPerformance;
begin
  FRecvTunnel.SwitchMaxPerformance;
  FSendTunnel.SwitchMaxPerformance;
end;

procedure TDTService_VirtualAuth.SwitchAsMaxSecurity;
begin
  FRecvTunnel.SwitchMaxSecurity;
  FSendTunnel.SwitchMaxSecurity;
end;

procedure TDTService_VirtualAuth.SwitchAsDefaultPerformance;
begin
  FRecvTunnel.SwitchDefaultPerformance;
  FSendTunnel.SwitchDefaultPerformance;
end;

procedure TDTService_VirtualAuth.Progress;
begin
  FCadencerEngine.Progress;
  FRecvTunnel.Progress;
  FSendTunnel.Progress;
end;

procedure TDTService_VirtualAuth.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  FProgressEngine.Progress(deltaTime);
end;

procedure TDTService_VirtualAuth.RegisterCommand;
begin
  FRecvTunnel.RegisterStream(C_UserLogin).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_UserLogin;
  FRecvTunnel.RegisterStream(C_RegisterUser).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_RegisterUser;
  FRecvTunnel.RegisterStream(C_TunnelLink).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_TunnelLink;
  FRecvTunnel.RegisterStream(C_GetCurrentCadencer).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetCurrentCadencer;

  FRecvTunnel.RegisterStream(C_GetFileTime).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetFileTime;
  FRecvTunnel.RegisterStream(C_GetFileInfo).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetFileInfo;
  FRecvTunnel.RegisterStream(C_GetFileMD5).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetFileMD5;
  FRecvTunnel.RegisterStream(C_GetFile).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetFile;
  FRecvTunnel.RegisterStream(C_GetFileAs).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetFileAs;
  FRecvTunnel.RegisterDirectStream(C_PostFileInfo).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFileInfo;
  FRecvTunnel.RegisterBigStream(C_PostFile).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFile;
  FRecvTunnel.RegisterDirectStream(C_PostFileOver).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFileOver;
  FRecvTunnel.RegisterStream(C_GetFileFragmentData).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetFileFragmentData;

  FRecvTunnel.RegisterDirectStream(C_NewBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_NewBatchStream;
  FRecvTunnel.RegisterBigStream(C_PostBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostBatchStream;
  FRecvTunnel.RegisterDirectStream(C_ClearBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ClearBatchStream;
  FRecvTunnel.RegisterDirectStream(C_PostBatchStreamDone).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostBatchStreamDone;
  FRecvTunnel.RegisterStream(C_GetBatchStreamState).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetBatchStreamState;
end;

procedure TDTService_VirtualAuth.UnRegisterCommand;
begin
  FRecvTunnel.DeleteRegistedCMD(C_UserLogin);
  FRecvTunnel.DeleteRegistedCMD(C_RegisterUser);
  FRecvTunnel.DeleteRegistedCMD(C_TunnelLink);
  FRecvTunnel.DeleteRegistedCMD(C_GetCurrentCadencer);

  FRecvTunnel.DeleteRegistedCMD(C_GetFileTime);
  FRecvTunnel.DeleteRegistedCMD(C_GetFileInfo);
  FRecvTunnel.DeleteRegistedCMD(C_GetFileMD5);
  FRecvTunnel.DeleteRegistedCMD(C_GetFile);
  FRecvTunnel.DeleteRegistedCMD(C_GetFileAs);
  FRecvTunnel.DeleteRegistedCMD(C_PostFileInfo);
  FRecvTunnel.DeleteRegistedCMD(C_PostFile);
  FRecvTunnel.DeleteRegistedCMD(C_PostFileOver);
  FRecvTunnel.DeleteRegistedCMD(C_GetFileFragmentData);

  FRecvTunnel.DeleteRegistedCMD(C_NewBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_PostBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_ClearBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_PostBatchStreamDone);
  FRecvTunnel.DeleteRegistedCMD(C_GetBatchStreamState);
end;

function TDTService_VirtualAuth.GetUserDefineRecvTunnel(RecvCli: TPeerIO): TPeerClientUserDefineForRecvTunnel_VirtualAuth;
begin
  Result := RecvCli.UserDefine as TPeerClientUserDefineForRecvTunnel_VirtualAuth;
end;

function TDTService_VirtualAuth.TotalLinkCount: Integer;
begin
  Result := RecvTunnel.Count;
end;

procedure TDTService_VirtualAuth.PostBatchStream(cli: TPeerIO; stream: TCore_Stream; doneFreeStream: Boolean);
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

procedure TDTService_VirtualAuth.PostBatchStreamC(cli: TPeerIO; stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_C);
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

procedure TDTService_VirtualAuth.PostBatchStreamM(cli: TPeerIO; stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_M);
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

procedure TDTService_VirtualAuth.PostBatchStreamP(cli: TPeerIO; stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_P);
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

procedure TDTService_VirtualAuth.ClearBatchStream(cli: TPeerIO);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;
  cli.SendDirectStreamCmd(C_ClearBatchStream, de);
  DisposeObject(de);
end;

procedure TDTService_VirtualAuth.GetBatchStreamStateM(cli: TPeerIO; OnResult: TOnStream_M);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;
  cli.SendStreamCmdM(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

procedure TDTService_VirtualAuth.GetBatchStreamStateM(cli: TPeerIO; Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_M);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;
  cli.SendStreamCmdM(C_GetBatchStreamState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDTService_VirtualAuth.GetBatchStreamStateP(cli: TPeerIO; OnResult: TOnStream_P);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;
  cli.SendStreamCmdP(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

procedure TDTService_VirtualAuth.GetBatchStreamStateP(cli: TPeerIO; Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_P);
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;
  cli.SendStreamCmdP(C_GetBatchStreamState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

constructor TClientUserDefineForRecvTunnel_VirtualAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  Client := nil;
  SendTunnel := nil;
end;

destructor TClientUserDefineForRecvTunnel_VirtualAuth.Destroy;
begin
  if Client <> nil then
      Client.FLinkOk := False;
  inherited Destroy;
end;

constructor TClientUserDefineForSendTunnel_VirtualAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  Client := nil;
  RecvTunnel := nil;
end;

destructor TClientUserDefineForSendTunnel_VirtualAuth.Destroy;
begin
  if Client <> nil then
      Client.FLinkOk := False;
  inherited Destroy;
end;

{ client notify interface }
procedure TDTClient_VirtualAuth.ClientConnected(Sender: TZNet_Client);
begin
end;

procedure TDTClient_VirtualAuth.ClientDisconnect(Sender: TZNet_Client);
begin
end;

procedure TDTClient_VirtualAuth.Command_FileInfo(Sender: TPeerIO; InData: TDFE);
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
    Sender.Print('post file failed: %s', [fullfn]);
    { FRecvTunnel.ClientIO.Disconnect; }
    FCurrentStream := nil;
  end;
end;

procedure TDTClient_VirtualAuth.Command_PostFile(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin
  if FCurrentStream <> nil then
    begin
      InData.Position := 0;
      if InData.Size > 0 then
        begin
          FCurrentStream.Position := FCurrentStream.Size;
          FCurrentStream.CopyFrom(InData, InData.Size);
        end;
    end;
end;

procedure TDTClient_VirtualAuth.Command_PostFileOver(Sender: TPeerIO; InData: TDFE);
var
  RemoteBackcallAddr: UInt64;
  p: PRemoteFileBackcall_VirtualAuth;
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
end;

procedure TDTClient_VirtualAuth.Command_PostFileFragmentData(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  mem_: TMS64;
  StartPos, EndPos, siz: Int64;
  RemoteBackcallAddr: UInt64;
  p: PFileFragmentDataBackcall_VirtualAuth;
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

procedure TDTClient_VirtualAuth.GetCurrentCadencer_StreamResult(Sender: TPeerIO; Result_: TDFE);
var
  servTime: Double;
begin
  servTime := Result_.Reader.ReadDouble;

  FCadencerEngine.Progress;
  FServerDelay := FCadencerEngine.CurrentTime - FLastCadencerTime;

  FCadencerEngine.CurrentTime := servTime + FServerDelay;
  FCadencerEngine.Progress;
end;

procedure TDTClient_VirtualAuth.GetFileInfo_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE);
var
  p: PGetFileInfoStruct_VirtualAuth;
  Existed: Boolean;
  fSiz: Int64;
begin
  p := PGetFileInfoStruct_VirtualAuth(Param1);
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

procedure TDTClient_VirtualAuth.GetFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE);
var
  p: PFileMD5Struct_VirtualAuth;
  successed: Boolean;
  MD5: TMD5;
begin
  p := PFileMD5Struct_VirtualAuth(Param1);
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

procedure TDTClient_VirtualAuth.GetFile_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE);
var
  p: PRemoteFileBackcall_VirtualAuth;
begin
  if Result_.Count > 0 then
    begin
      if Result_.Reader.ReadBool then
          exit;
      Sender.Print('get file failed:%s', [Result_.Reader.ReadString]);
    end;

  p := Param1;
  Dispose(p);
end;

procedure TDTClient_VirtualAuth.GetFileFragmentData_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDFE);
var
  p: PFileFragmentDataBackcall_VirtualAuth;
begin
  if Result_.Count > 0 then
    begin
      if Result_.Reader.ReadBool then
          exit;
    end;

  p := Param1;
  Dispose(p);
end;

procedure TDTClient_VirtualAuth.Command_NewBatchStream(Sender: TPeerIO; InData: TDFE);
var
  RT: TClientUserDefineForRecvTunnel_VirtualAuth;
  p: PBigStreamBatchPostData;
begin
  if not LinkOk then
      exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel_VirtualAuth;
  p := RT.BigStreamBatchList.NewPostData;
  p^.RemoteMD5 := InData.Reader.ReadMD5;
  p^.CompletedBackcallPtr := InData.Reader.ReadPointer;
end;

procedure TDTClient_VirtualAuth.Command_PostBatchStream(Sender: TPeerIO; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  RT: TClientUserDefineForRecvTunnel_VirtualAuth;
  p: PBigStreamBatchPostData;
  de: TDFE;
begin
  if not LinkOk then
      exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel_VirtualAuth;

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

procedure TDTClient_VirtualAuth.Command_ClearBatchStream(Sender: TPeerIO; InData: TDFE);
var
  RT: TClientUserDefineForRecvTunnel_VirtualAuth;
  p: PBigStreamBatchPostData;
  de: TDFE;
begin
  if not LinkOk then
      exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel_VirtualAuth;
  RT.BigStreamBatchList.Clear;
end;

procedure TDTClient_VirtualAuth.Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDFE);
var
  RT: TClientUserDefineForRecvTunnel_VirtualAuth;
  rMD5, sMD5: TMD5;
  backCallVal: UInt64;
  backCallValPtr: POnStateStruct;
  MD5Verify: Boolean;
begin
  if not LinkOk then
      exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel_VirtualAuth;

  rMD5 := InData.Reader.ReadMD5;
  sMD5 := InData.Reader.ReadMD5;
  backCallVal := InData.Reader.ReadPointer;

  backCallValPtr := POnStateStruct(Pointer(backCallVal));
  MD5Verify := umlMD5Compare(rMD5, sMD5);

  if backCallValPtr = nil then
      exit;

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

procedure TDTClient_VirtualAuth.Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDFE);
var
  RT: TClientUserDefineForRecvTunnel_VirtualAuth;
  i: Integer;
  p: PBigStreamBatchPostData;

  de: TDFE;
begin
  if not LinkOk then
      exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel_VirtualAuth;

  for i := 0 to RT.BigStreamBatchList.Count - 1 do
    begin
      p := RT.BigStreamBatchList[i];
      de := TDFE.Create;
      p^.Encode(de);
      OutData.WriteDataFrame(de);
      DisposeObject(de);
    end;
end;

procedure TDTClient_VirtualAuth.AsyncSendConnectResult(const cState: Boolean);
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
      exit;
    end;

  RecvTunnel.AsyncConnectM(FAsyncConnectAddr, FAsyncConnRecvPort, {$IFDEF FPC}@{$ENDIF FPC}AsyncRecvConnectResult);
end;

procedure TDTClient_VirtualAuth.AsyncRecvConnectResult(const cState: Boolean);
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

procedure TDTClient_VirtualAuth.UserLogin_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
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

procedure TDTClient_VirtualAuth.UserLogin_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
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

procedure TDTClient_VirtualAuth.RegisterUser_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
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

procedure TDTClient_VirtualAuth.RegisterUser_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
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

procedure TDTClient_VirtualAuth.TunnelLink_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
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
          TClientUserDefineForSendTunnel_VirtualAuth(FSendTunnel.ClientIO.UserDefine).Client := Self;
          TClientUserDefineForSendTunnel_VirtualAuth(FSendTunnel.ClientIO.UserDefine).RecvTunnel := TClientUserDefineForRecvTunnel_VirtualAuth(FRecvTunnel.ClientIO.UserDefine);

          TClientUserDefineForRecvTunnel_VirtualAuth(FRecvTunnel.ClientIO.UserDefine).Client := Self;
          TClientUserDefineForRecvTunnel_VirtualAuth(FRecvTunnel.ClientIO.UserDefine).SendTunnel := TClientUserDefineForSendTunnel_VirtualAuth(FSendTunnel.ClientIO.UserDefine);

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

procedure TDTClient_VirtualAuth.TunnelLink_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
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

constructor TDTClient_VirtualAuth.Create(RecvTunnel_, SendTunnel_: TZNet_Client);
begin
  inherited Create;
  FRecvTunnel := RecvTunnel_;
  FRecvTunnel.NotyifyInterface := Self;
  FRecvTunnel.PeerClientUserDefineClass := TClientUserDefineForRecvTunnel_VirtualAuth;

  FSendTunnel := SendTunnel_;
  FSendTunnel.NotyifyInterface := Self;
  FSendTunnel.PeerClientUserDefineClass := TClientUserDefineForSendTunnel_VirtualAuth;

  FRecvTunnel.DoubleChannelFramework := Self;
  FSendTunnel.DoubleChannelFramework := Self;

  FFileSystem := False;
  FAutoFreeTunnel := False;

  FLinkOk := False;
  FWaitCommandTimeout := 5000;

  FCurrentStream := nil;
  FCurrentReceiveStreamFileName := '';

  FCadencerEngine := TCadencer.Create;
  FCadencerEngine.OnProgress := {$IFDEF FPC}@{$ENDIF FPC}CadencerProgress;
  FProgressEngine := TN_Progress_Tool.Create;

  FLastCadencerTime := 0;
  FServerDelay := 0;

  FAsyncConnectAddr := '';
  FAsyncConnRecvPort := 0;
  FAsyncConnSendPort := 0;
  FAsyncOnResult_C := nil;
  FAsyncOnResult_M := nil;
  FAsyncOnResult_P := nil;

  SwitchAsDefaultPerformance;
end;

destructor TDTClient_VirtualAuth.Destroy;
begin
  FRecvTunnel.NotyifyInterface := nil;
  FSendTunnel.NotyifyInterface := nil;
  if FAutoFreeTunnel then
    begin
      DisposeObjectAndNil(FRecvTunnel);
      DisposeObjectAndNil(FSendTunnel);
    end;
  DisposeObject([FCadencerEngine, FProgressEngine]);

  inherited Destroy;
end;

function TDTClient_VirtualAuth.Connected: Boolean;
begin
  try
      Result := FSendTunnel.Connected and FRecvTunnel.Connected;
  except
      Result := False;
  end;
end;

function TDTClient_VirtualAuth.IOBusy: Boolean;
begin
  try
      Result := FSendTunnel.IOBusy and FRecvTunnel.IOBusy;
  except
      Result := True;
  end;
end;

procedure TDTClient_VirtualAuth.SwitchAsMaxPerformance;
begin
  FRecvTunnel.SwitchMaxPerformance;
  FSendTunnel.SwitchMaxPerformance;
end;

procedure TDTClient_VirtualAuth.SwitchAsMaxSecurity;
begin
  FRecvTunnel.SwitchMaxSecurity;
  FSendTunnel.SwitchMaxSecurity;
end;

procedure TDTClient_VirtualAuth.SwitchAsDefaultPerformance;
begin
  FRecvTunnel.SwitchDefaultPerformance;
  FSendTunnel.SwitchDefaultPerformance;
end;

procedure TDTClient_VirtualAuth.Progress;
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

procedure TDTClient_VirtualAuth.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  FProgressEngine.Progress(deltaTime);
end;

function TDTClient_VirtualAuth.Connect(addr: SystemString; const RecvPort, SendPort: Word): Boolean;
var
  t: Cardinal;
begin
  Result := False;
  Disconnect;

  if not FSendTunnel.Connect(addr, SendPort) then
    begin
      DoStatus('connect %s failed!', [addr]);
      exit;
    end;
  if not FRecvTunnel.Connect(addr, RecvPort) then
    begin
      DoStatus('connect %s failed!', [addr]);
      exit;
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

procedure TDTClient_VirtualAuth.AsyncConnectC(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TOnState_C);
begin
  Disconnect;
  FAsyncConnectAddr := addr;
  FAsyncConnRecvPort := RecvPort;
  FAsyncConnSendPort := SendPort;
  FAsyncOnResult_C := OnResult;
  FAsyncOnResult_M := nil;
  FAsyncOnResult_P := nil;
  SendTunnel.AsyncConnectM(FAsyncConnectAddr, FAsyncConnSendPort, {$IFDEF FPC}@{$ENDIF FPC}AsyncSendConnectResult);
end;

procedure TDTClient_VirtualAuth.AsyncConnectM(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TOnState_M);
begin
  Disconnect;
  FAsyncConnectAddr := addr;
  FAsyncConnRecvPort := RecvPort;
  FAsyncConnSendPort := SendPort;
  FAsyncOnResult_C := nil;
  FAsyncOnResult_M := OnResult;
  FAsyncOnResult_P := nil;
  SendTunnel.AsyncConnectM(FAsyncConnectAddr, FAsyncConnSendPort, {$IFDEF FPC}@{$ENDIF FPC}AsyncSendConnectResult);
end;

procedure TDTClient_VirtualAuth.AsyncConnectP(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TOnState_P);
begin
  Disconnect;
  FAsyncConnectAddr := addr;
  FAsyncConnRecvPort := RecvPort;
  FAsyncConnSendPort := SendPort;
  FAsyncOnResult_C := nil;
  FAsyncOnResult_M := nil;
  FAsyncOnResult_P := OnResult;

  SendTunnel.AsyncConnectM(FAsyncConnectAddr, FAsyncConnSendPort, {$IFDEF FPC}@{$ENDIF FPC}AsyncSendConnectResult);
end;

procedure TDTClient_VirtualAuth.AsyncConnectC(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TOnParamState_C);
var
  ParamBridge: TStateParamBridge;
begin
  ParamBridge := TStateParamBridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyC := OnResult;
  AsyncConnectM(addr, RecvPort, SendPort, {$IFDEF FPC}@{$ENDIF FPC}ParamBridge.DoStateResult);
end;

procedure TDTClient_VirtualAuth.AsyncConnectM(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TOnParamState_M);
var
  ParamBridge: TStateParamBridge;
begin
  ParamBridge := TStateParamBridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyM := OnResult;
  AsyncConnectM(addr, RecvPort, SendPort, {$IFDEF FPC}@{$ENDIF FPC}ParamBridge.DoStateResult);
end;

procedure TDTClient_VirtualAuth.AsyncConnectP(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TOnParamState_P);
var
  ParamBridge: TStateParamBridge;
begin
  ParamBridge := TStateParamBridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyP := OnResult;
  AsyncConnectM(addr, RecvPort, SendPort, {$IFDEF FPC}@{$ENDIF FPC}ParamBridge.DoStateResult);
end;

procedure TDTClient_VirtualAuth.Disconnect;
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

function TDTClient_VirtualAuth.UserLogin(UserID, Passwd: SystemString): Boolean;
var
  sendDE, resDE: TDFE;
begin
  Result := False;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;
  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(Passwd);
  FSendTunnel.WaitSendStreamCmd(C_UserLogin, sendDE, resDE, FWaitCommandTimeout * 2);

  if resDE.Count > 0 then
    begin
      Result := resDE.ReadBool(0);
      FSendTunnel.ClientIO.Print(resDE.ReadString(1));
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TDTClient_VirtualAuth.RegisterUser(UserID, Passwd: SystemString): Boolean;
var
  sendDE, resDE: TDFE;
begin
  Result := False;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;
  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(Passwd);
  FSendTunnel.WaitSendStreamCmd(C_RegisterUser, sendDE, resDE, FWaitCommandTimeout * 2);

  if resDE.Count > 0 then
    begin
      Result := resDE.ReadBool(0);
      FSendTunnel.ClientIO.Print(resDE.ReadString(1));
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TDTClient_VirtualAuth.TunnelLink: Boolean;
var
  sendDE, resDE: TDFE;
begin
  if FLinkOk then
      exit(True);
  FLinkOk := False;
  Result := False;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  SyncCadencer;

  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteCardinal(FSendTunnel.RemoteID);
  sendDE.WriteCardinal(FRecvTunnel.RemoteID);

  FSendTunnel.WaitSendStreamCmd(C_TunnelLink, sendDE, resDE, FWaitCommandTimeout);

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
          TClientUserDefineForSendTunnel_VirtualAuth(FSendTunnel.ClientIO.UserDefine).Client := Self;
          TClientUserDefineForSendTunnel_VirtualAuth(FSendTunnel.ClientIO.UserDefine).RecvTunnel := TClientUserDefineForRecvTunnel_VirtualAuth(FRecvTunnel.ClientIO.UserDefine);

          TClientUserDefineForRecvTunnel_VirtualAuth(FRecvTunnel.ClientIO.UserDefine).Client := Self;
          TClientUserDefineForRecvTunnel_VirtualAuth(FRecvTunnel.ClientIO.UserDefine).SendTunnel := TClientUserDefineForSendTunnel_VirtualAuth(FSendTunnel.ClientIO.UserDefine);

          FLinkOk := True;
        end;
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TDTClient_VirtualAuth.UserLoginC(UserID, Passwd: SystemString; On_C: TOnState_C);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;
  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(Passwd);

  new(p);
  p^.Init;
  p^.On_C := On_C;
  FSendTunnel.SendStreamCmdM(C_UserLogin, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}UserLogin_OnResult, {$IFDEF FPC}@{$ENDIF FPC}UserLogin_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.UserLoginM(UserID, Passwd: SystemString; On_M: TOnState_M);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;
  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(Passwd);

  new(p);
  p^.Init;
  p^.On_M := On_M;
  FSendTunnel.SendStreamCmdM(C_UserLogin, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}UserLogin_OnResult, {$IFDEF FPC}@{$ENDIF FPC}UserLogin_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.UserLoginP(UserID, Passwd: SystemString; On_P: TOnState_P);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;
  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(Passwd);

  new(p);
  p^.Init;
  p^.On_P := On_P;
  FSendTunnel.SendStreamCmdM(C_UserLogin, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}UserLogin_OnResult, {$IFDEF FPC}@{$ENDIF FPC}UserLogin_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.RegisterUserC(UserID, Passwd: SystemString; On_C: TOnState_C);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;
  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(Passwd);

  new(p);
  p^.Init;
  p^.On_C := On_C;
  FSendTunnel.SendStreamCmdM(C_RegisterUser, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}RegisterUser_OnResult, {$IFDEF FPC}@{$ENDIF FPC}RegisterUser_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.RegisterUserM(UserID, Passwd: SystemString; On_M: TOnState_M);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;
  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(Passwd);

  new(p);
  p^.Init;
  p^.On_M := On_M;
  FSendTunnel.SendStreamCmdM(C_RegisterUser, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}RegisterUser_OnResult, {$IFDEF FPC}@{$ENDIF FPC}RegisterUser_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.RegisterUserP(UserID, Passwd: SystemString; On_P: TOnState_P);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;
  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(Passwd);

  new(p);
  p^.Init;
  p^.On_P := On_P;
  FSendTunnel.SendStreamCmdM(C_RegisterUser, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}RegisterUser_OnResult, {$IFDEF FPC}@{$ENDIF FPC}RegisterUser_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.TunnelLinkC(On_C: TOnState_C);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if FLinkOk then
      exit;

  FLinkOk := False;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  SyncCadencer;

  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FSendTunnel.RemoteID);
  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  new(p);
  p^.Init;
  p^.On_C := On_C;
  FSendTunnel.SendStreamCmdM(C_TunnelLink, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}TunnelLink_OnResult, {$IFDEF FPC}@{$ENDIF FPC}TunnelLink_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.TunnelLinkM(On_M: TOnState_M);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if FLinkOk then
      exit;

  FLinkOk := False;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  SyncCadencer;

  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FSendTunnel.RemoteID);
  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  new(p);
  p^.Init;
  p^.On_M := On_M;
  FSendTunnel.SendStreamCmdM(C_TunnelLink, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}TunnelLink_OnResult, {$IFDEF FPC}@{$ENDIF FPC}TunnelLink_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.TunnelLinkP(On_P: TOnState_P);
var
  sendDE: TDFE;
  p: POnStateStruct;
begin
  if FLinkOk then
      exit;

  FLinkOk := False;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  SyncCadencer;

  sendDE := TDFE.Create;

  sendDE.WriteCardinal(FSendTunnel.RemoteID);
  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  new(p);
  p^.Init;
  p^.On_P := On_P;
  FSendTunnel.SendStreamCmdM(C_TunnelLink, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}TunnelLink_OnResult, {$IFDEF FPC}@{$ENDIF FPC}TunnelLink_OnFailed);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.SyncCadencer;
var
  sendDE: TDFE;
begin
  sendDE := TDFE.Create;

  FCadencerEngine.Progress;
  FLastCadencerTime := FCadencerEngine.CurrentTime;
  FServerDelay := 0;
  sendDE.WriteDouble(FLastCadencerTime);
  FSendTunnel.SendStreamCmdM(C_GetCurrentCadencer, sendDE, {$IFDEF FPC}@{$ENDIF FPC}GetCurrentCadencer_StreamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.GetFileTimeM(RemoteFilename: SystemString; On_CResult: TOnStream_M);
var
  sendDE: TDFE;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(RemoteFilename);
  FSendTunnel.SendStreamCmdM(C_GetFileTime, sendDE, On_CResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.GetFileTimeP(RemoteFilename: SystemString; On_CResult: TOnStream_P);
var
  sendDE: TDFE;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(RemoteFilename);
  FSendTunnel.SendStreamCmdP(C_GetFileTime, sendDE, On_CResult);
  DisposeObject(sendDE);
end;

{ remote file exists }
procedure TDTClient_VirtualAuth.GetFileInfoC(fileName: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_C_VirtualAuth);
var
  sendDE: TDFE;
  p: PGetFileInfoStruct_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnComplete_C := OnComplete;
  p^.OnComplete_M := nil;
  p^.OnComplete_P := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileInfo, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.GetFileInfoM(fileName: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_M_VirtualAuth);
var
  sendDE: TDFE;
  p: PGetFileInfoStruct_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnComplete_C := nil;
  p^.OnComplete_M := OnComplete;
  p^.OnComplete_P := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileInfo, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.GetFileInfoP(fileName: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TGetFileInfo_P_VirtualAuth);
var
  sendDE: TDFE;
  p: PGetFileInfoStruct_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnComplete_C := nil;
  p^.OnComplete_M := nil;
  p^.OnComplete_P := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileInfo, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

{ remote md5 support with public store space }
procedure TDTClient_VirtualAuth.GetFileMD5C(fileName: SystemString; const StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_C_VirtualAuth);
var
  sendDE: TDFE;
  p: PFileMD5Struct_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_C := OnComplete;
  p^.OnComplete_M := nil;
  p^.OnComplete_P := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileMD5, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.GetFileMD5M(fileName: SystemString; const StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_M_VirtualAuth);
var
  sendDE: TDFE;
  p: PFileMD5Struct_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_C := nil;
  p^.OnComplete_M := OnComplete;
  p^.OnComplete_P := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileMD5, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.GetFileMD5P(fileName: SystemString; const StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCore_Object; const OnComplete: TFileMD5_P_VirtualAuth);
var
  sendDE: TDFE;
  p: PFileMD5Struct_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnComplete_C := nil;
  p^.OnComplete_M := nil;
  p^.OnComplete_P := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileMD5, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.GetFileC(fileName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_C: TFileComplete_C_VirtualAuth);
begin
  GetFileC(fileName, 0, saveToPath, UserData, UserObject, OnComplete_C);
end;

procedure TDTClient_VirtualAuth.GetFileM(fileName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_M: TFileComplete_M_VirtualAuth);
begin
  GetFileM(fileName, 0, saveToPath, UserData, UserObject, OnComplete_M);
end;

procedure TDTClient_VirtualAuth.GetFileP(fileName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_P: TFileComplete_P_VirtualAuth);
begin
  GetFileP(fileName, 0, saveToPath, UserData, UserObject, OnComplete_P);
end;

function TDTClient_VirtualAuth.GetFile(fileName, saveToPath: SystemString): Boolean;
begin
  Result := GetFile(fileName, 0, saveToPath);
end;

{ restore download }
procedure TDTClient_VirtualAuth.GetFileC(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_C: TFileComplete_C_VirtualAuth);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_C := OnComplete_C;
  p^.OnComplete_M := nil;
  p^.OnComplete_P := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.GetFileM(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_M: TFileComplete_M_VirtualAuth);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_C := nil;
  p^.OnComplete_M := OnComplete_M;
  p^.OnComplete_P := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.GetFileP(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_P: TFileComplete_P_VirtualAuth);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_C := nil;
  p^.OnComplete_M := nil;
  p^.OnComplete_P := OnComplete_P;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.GetFileAsC(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_C: TFileComplete_C_VirtualAuth);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_C := OnComplete_C;
  p^.OnComplete_M := nil;
  p^.OnComplete_P := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileAs, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.GetFileAsM(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_M: TFileComplete_M_VirtualAuth);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_C := nil;
  p^.OnComplete_M := OnComplete_M;
  p^.OnComplete_P := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileAs, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.GetFileAsP(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_P: TFileComplete_P_VirtualAuth);
var
  sendDE: TDFE;
  p: PRemoteFileBackcall_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete_C := nil;
  p^.OnComplete_M := nil;
  p^.OnComplete_P := OnComplete_P;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileAs, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFile_StreamParamResult);
  DisposeObject(sendDE);
end;

{ Synchronously waiting to download files from the server to complete }
function TDTClient_VirtualAuth.GetFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean;
var
  sendDE, resDE: TDFE;
begin
  Result := False;
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  sendDE := TDFE.Create;
  resDE := TDFE.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  sendDE.WritePointer(0);

  FSendTunnel.WaitSendStreamCmd(C_GetFile, sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.Reader.ReadBool;
      FSendTunnel.ClientIO.Print(resDE.Reader.ReadString);
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TDTClient_VirtualAuth.GetFileFragmentDataC(fileName: SystemString; StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_C: TFileFragmentData_C_VirtualAuth);
var
  sendDE: TDFE;
  p: PFileFragmentDataBackcall_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

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

  FSendTunnel.SendStreamCmdM(C_GetFileFragmentData, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.GetFileFragmentDataM(fileName: SystemString; StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_M: TFileFragmentData_M_VirtualAuth);
var
  sendDE: TDFE;
  p: PFileFragmentDataBackcall_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

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

  FSendTunnel.SendStreamCmdM(C_GetFileFragmentData, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.GetFileFragmentDataP(fileName: SystemString; StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCore_Object; const OnComplete_P: TFileFragmentData_P_VirtualAuth);
var
  sendDE: TDFE;
  p: PFileFragmentDataBackcall_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

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

  FSendTunnel.SendStreamCmdM(C_GetFileFragmentData, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.AutomatedDownloadFileC(remoteFile, localFile: U_String; OnDownloadDone: TFileComplete_C_VirtualAuth);
var
  tmp: TAutomatedDownloadFile_Struct_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  tmp := TAutomatedDownloadFile_Struct_VirtualAuth.Create;
  tmp.remoteFile := remoteFile;
  tmp.localFile := localFile;
  tmp.OnDownloadDoneC := OnDownloadDone;
  tmp.Client := Self;

  GetFileInfoM(umlGetFileName(remoteFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoResult_GetFileInfo);
end;

procedure TDTClient_VirtualAuth.AutomatedDownloadFileM(remoteFile, localFile: U_String; OnDownloadDone: TFileComplete_M_VirtualAuth);
var
  tmp: TAutomatedDownloadFile_Struct_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  tmp := TAutomatedDownloadFile_Struct_VirtualAuth.Create;
  tmp.remoteFile := remoteFile;
  tmp.localFile := localFile;
  tmp.OnDownloadDoneM := OnDownloadDone;
  tmp.Client := Self;

  GetFileInfoM(umlGetFileName(remoteFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoResult_GetFileInfo);
end;

procedure TDTClient_VirtualAuth.AutomatedDownloadFileP(remoteFile, localFile: U_String; OnDownloadDone: TFileComplete_P_VirtualAuth);
var
  tmp: TAutomatedDownloadFile_Struct_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  tmp := TAutomatedDownloadFile_Struct_VirtualAuth.Create;
  tmp.remoteFile := remoteFile;
  tmp.localFile := localFile;
  tmp.OnDownloadDoneP := OnDownloadDone;
  tmp.Client := Self;

  GetFileInfoM(umlGetFileName(remoteFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoResult_GetFileInfo);
end;

procedure TDTClient_VirtualAuth.PostFile(fileName: SystemString);
var
  sendDE: TDFE;
  fs: TCore_FileStream;
begin
  if not FFileSystem then
      exit;
  if not umlFileExists(fileName) then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  fs := TCore_FileStream.Create(fileName, fmOpenRead or fmShareDenyNone);

  sendDE := TDFE.Create;
  sendDE.WriteString(umlGetFileName(fileName));
  sendDE.WriteInt64(0);
  sendDE.WriteInt64(fs.Size);
  FSendTunnel.SendDirectStreamCmd(C_PostFileInfo, sendDE);
  DisposeObject(sendDE);

  fs.Position := 0;
  FSendTunnel.SendBigStream(C_PostFile, fs, True);

  sendDE := TDFE.Create;
  FSendTunnel.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.PostFile(fileName: SystemString; StartPos: Int64);
var
  sendDE: TDFE;
  fs: TCore_FileStream;
begin
  if not FFileSystem then
      exit;
  if not umlFileExists(fileName) then
      exit;
  if not FSendTunnel.Connected then
      exit;
  if not FRecvTunnel.Connected then
      exit;

  fs := TCore_FileStream.Create(fileName, fmOpenRead or fmShareDenyNone);

  sendDE := TDFE.Create;
  sendDE.WriteString(umlGetFileName(fileName));
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  FSendTunnel.SendDirectStreamCmd(C_PostFileInfo, sendDE);
  DisposeObject(sendDE);

  fs.Position := 0;
  FSendTunnel.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDFE.Create;
  FSendTunnel.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.PostFile(fn: SystemString; stream: TCore_Stream; doneFreeStream: Boolean);
var
  sendDE: TDFE;
begin
  if (not FSendTunnel.Connected) or (not FRecvTunnel.Connected) or (not FFileSystem) then
    begin
      if doneFreeStream then
          DisposeObject(stream);
      exit;
    end;

  sendDE := TDFE.Create;
  sendDE.WriteString(umlGetFileName(fn));
  sendDE.WriteInt64(0);
  sendDE.WriteInt64(stream.Size);
  FSendTunnel.SendDirectStreamCmd(C_PostFileInfo, sendDE);
  DisposeObject(sendDE);

  stream.Position := 0;
  FSendTunnel.SendBigStream(C_PostFile, stream, doneFreeStream);

  sendDE := TDFE.Create;
  FSendTunnel.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.PostFile(fn: SystemString; stream: TCore_Stream; StartPos: Int64; doneFreeStream: Boolean);
var
  sendDE: TDFE;
begin
  if (not FSendTunnel.Connected) or (not FRecvTunnel.Connected) or (not FFileSystem) then
    begin
      if doneFreeStream then
          DisposeObject(stream);
      exit;
    end;

  sendDE := TDFE.Create;
  sendDE.WriteString(umlGetFileName(fn));
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(stream.Size);
  FSendTunnel.SendDirectStreamCmd(C_PostFileInfo, sendDE);
  DisposeObject(sendDE);

  stream.Position := 0;
  FSendTunnel.SendBigStream(C_PostFile, stream, StartPos, doneFreeStream);

  sendDE := TDFE.Create;
  FSendTunnel.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);
end;

procedure TDTClient_VirtualAuth.AutomatedUploadFile(localFile: U_String);
var
  tmp: TAutomatedUploadFile_Struct_VirtualAuth;
begin
  if not FFileSystem then
      exit;
  tmp := TAutomatedUploadFile_Struct_VirtualAuth.Create;
  tmp.localFile := localFile;
  tmp.Client := Self;

  GetFileInfoM(umlGetFileName(localFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoResult_GetFileInfo);
end;

procedure TDTClient_VirtualAuth.PostBatchStream(stream: TCore_Stream; doneFreeStream: Boolean);
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

procedure TDTClient_VirtualAuth.PostBatchStreamC(stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_C);
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

procedure TDTClient_VirtualAuth.PostBatchStreamM(stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_M);
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

procedure TDTClient_VirtualAuth.PostBatchStreamP(stream: TCore_Stream; doneFreeStream: Boolean; OnCompletedBackcall: TOnState_P);
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

procedure TDTClient_VirtualAuth.ClearBatchStream;
var
  de: TDFE;
  p: POnStateStruct;
begin
  de := TDFE.Create;
  SendTunnel.SendDirectStreamCmd(C_ClearBatchStream, de);
  DisposeObject(de);
end;

procedure TDTClient_VirtualAuth.GetBatchStreamStateM(OnResult: TOnStream_M);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdM(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

procedure TDTClient_VirtualAuth.GetBatchStreamStateM(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_M);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdM(C_GetBatchStreamState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDTClient_VirtualAuth.GetBatchStreamStateP(OnResult: TOnStream_P);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdP(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

procedure TDTClient_VirtualAuth.GetBatchStreamStateP(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_P);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdP(C_GetBatchStreamState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

function TDTClient_VirtualAuth.GetBatchStreamState(Result_: TDFE; TimeOut_: TTimeTick): Boolean;
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.WaitSendStreamCmd(C_GetBatchStreamState, de, Result_, TimeOut_);
  Result := Result_.Count > 0;
  DisposeObject(de);
end;

procedure TDTClient_VirtualAuth.RegisterCommand;
begin
  FRecvTunnel.RegisterDirectStream(C_FileInfo).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_FileInfo;
  FRecvTunnel.RegisterBigStream(C_PostFile).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFile;
  FRecvTunnel.RegisterDirectStream(C_PostFileOver).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFileOver;
  FRecvTunnel.RegisterCompleteBuffer(C_PostFileFragmentData).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFileFragmentData;

  FRecvTunnel.RegisterDirectStream(C_NewBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_NewBatchStream;
  FRecvTunnel.RegisterBigStream(C_PostBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostBatchStream;
  FRecvTunnel.RegisterDirectStream(C_ClearBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ClearBatchStream;
  FRecvTunnel.RegisterDirectStream(C_PostBatchStreamDone).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostBatchStreamDone;
  FRecvTunnel.RegisterStream(C_GetBatchStreamState).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetBatchStreamState;
end;

procedure TDTClient_VirtualAuth.UnRegisterCommand;
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

function TDTClient_VirtualAuth.RemoteInited: Boolean;
begin
  Result := FSendTunnel.RemoteInited and FRecvTunnel.RemoteInited;
end;

procedure TDT_P2PVM_VirtualAuth_OnState.Init;
begin
  On_C := nil;
  On_M := nil;
  On_P := nil;
end;

function TDT_P2PVM_VirtualAuth_Service.GetQuietMode: Boolean;
begin
  Result := RecvTunnel.QuietMode and SendTunnel.QuietMode and PhysicsTunnel.QuietMode;
end;

procedure TDT_P2PVM_VirtualAuth_Service.SetQuietMode(const Value: Boolean);
begin
  RecvTunnel.QuietMode := Value;
  SendTunnel.QuietMode := Value;
  PhysicsTunnel.QuietMode := Value;
end;

constructor TDT_P2PVM_VirtualAuth_Service.Create(ServiceClass_: TDTService_VirtualAuthClass);
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

  RecvTunnel.PrefixName := 'VA';
  RecvTunnel.Name := 'R';
  SendTunnel.PrefixName := 'VA';
  SendTunnel.Name := 'S';
  PhysicsTunnel.PrefixName := 'Physics';
  PhysicsTunnel.Name := 'p2pVM';
end;

destructor TDT_P2PVM_VirtualAuth_Service.Destroy;
begin
  StopService;
  DisposeObject(RecvTunnel);
  DisposeObject(SendTunnel);
  DisposeObject(DTService);
  DisposeObject(PhysicsTunnel);
  inherited Destroy;
end;

procedure TDT_P2PVM_VirtualAuth_Service.Progress;
begin
  DTService.Progress;
  PhysicsTunnel.Progress;
end;

procedure TDT_P2PVM_VirtualAuth_Service.StartService(ListenAddr, ListenPort, Auth: SystemString);
begin
  StopService;
  RecvTunnel.StartService('::', 1);
  SendTunnel.StartService('::', 2);
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  if PhysicsTunnel.StartService(ListenAddr, umlStrToInt(ListenPort)) then
      DoStatus('listening %s:%s ok.', [TranslateBindAddr(ListenAddr), ListenPort])
  else
      DoStatus('listening %s:%s failed!', [TranslateBindAddr(ListenAddr), ListenPort]);
end;

procedure TDT_P2PVM_VirtualAuth_Service.StopService;
begin
  PhysicsTunnel.StopService;
  RecvTunnel.StopService;
  SendTunnel.StopService;
end;

procedure TDT_P2PVM_VirtualAuth_Client.DoConnectionResult(const state: Boolean);
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

procedure TDT_P2PVM_VirtualAuth_Client.DoAutomatedP2PVMClientConnectionDone(Sender: TZNet; P_IO: TPeerIO);
begin
  PhysicsTunnel.Print('DT p2pVM done.');
  if (LastUser = '') or (LastPasswd = '') then
    begin
      DTClient.TunnelLinkM({$IFDEF FPC}@{$ENDIF FPC}DoTunnelLinkResult);
    end
  else if RegisterUserAndLogin then
    begin
      DTClient.RegisterUserM(LastUser, LastPasswd, {$IFDEF FPC}@{$ENDIF FPC}DoLoginResult);
    end
  else
    begin
      DTClient.UserLoginM(LastUser, LastPasswd, {$IFDEF FPC}@{$ENDIF FPC}DoLoginResult);
    end;
end;

procedure TDT_P2PVM_VirtualAuth_Client.DoRegisterResult(const state: Boolean);
begin
  DTClient.UserLoginM(LastUser, LastPasswd, {$IFDEF FPC}@{$ENDIF FPC}DoLoginResult);
end;

procedure TDT_P2PVM_VirtualAuth_Client.DoLoginResult(const state: Boolean);
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
      exit;
    end;

  DTClient.TunnelLinkM({$IFDEF FPC}@{$ENDIF FPC}DoTunnelLinkResult);
end;

procedure TDT_P2PVM_VirtualAuth_Client.DoTunnelLinkResult(const state: Boolean);
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

function TDT_P2PVM_VirtualAuth_Client.GetQuietMode: Boolean;
begin
  Result := RecvTunnel.QuietMode and SendTunnel.QuietMode and PhysicsTunnel.QuietMode;
end;

procedure TDT_P2PVM_VirtualAuth_Client.SetQuietMode(const Value: Boolean);
begin
  RecvTunnel.QuietMode := Value;
  SendTunnel.QuietMode := Value;
  PhysicsTunnel.QuietMode := Value;
end;

constructor TDT_P2PVM_VirtualAuth_Client.Create(ClientClass_: TDTClient_VirtualAuthClass);
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

  RecvTunnel.PrefixName := 'VA';
  RecvTunnel.Name := 'R';
  SendTunnel.PrefixName := 'VA';
  SendTunnel.Name := 'S';
  PhysicsTunnel.PrefixName := 'Physics';
  PhysicsTunnel.Name := 'p2pVM';
end;

destructor TDT_P2PVM_VirtualAuth_Client.Destroy;
begin
  Disconnect;
  DisposeObject(RecvTunnel);
  DisposeObject(SendTunnel);
  DisposeObject(DTClient);
  DisposeObject(PhysicsTunnel);
  inherited Destroy;
end;

procedure TDT_P2PVM_VirtualAuth_Client.Progress;
begin
  DTClient.Progress;
  PhysicsTunnel.Progress;

  if (AutomatedConnection) and ((not PhysicsTunnel.Connected) or (not DTClient.LinkOk)) and (not Connecting) and (Reconnection) then
      Connect(LastAddr, LastPort, LastAuth, LastUser, LastPasswd);
end;

procedure TDT_P2PVM_VirtualAuth_Client.Connect(addr, Port, Auth, User, Passwd: SystemString);
begin
  if Connecting then
      exit;
  Connecting := True;
  if not Reconnection then
    begin
      LastAddr := addr;
      LastPort := Port;
      LastAuth := Auth;
    end;
  LastUser := User;
  LastPasswd := Passwd;
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  OnConnectResultState.Init;
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_M := {$IFDEF FPC}@{$ENDIF FPC}DoAutomatedP2PVMClientConnectionDone;
  PhysicsTunnel.AsyncConnectM(addr, umlStrToInt(Port), {$IFDEF FPC}@{$ENDIF FPC}DoConnectionResult);
end;

procedure TDT_P2PVM_VirtualAuth_Client.Connect_C(addr, Port, Auth, User, Passwd: SystemString; OnResult: TOnState_C);
begin
  if Connecting then
      exit;
  Connecting := True;
  if not Reconnection then
    begin
      LastAddr := addr;
      LastPort := Port;
      LastAuth := Auth;
    end;
  LastUser := User;
  LastPasswd := Passwd;
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  OnConnectResultState.Init;
  OnConnectResultState.On_C := OnResult;
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_M := {$IFDEF FPC}@{$ENDIF FPC}DoAutomatedP2PVMClientConnectionDone;
  PhysicsTunnel.AsyncConnectM(addr, umlStrToInt(Port), {$IFDEF FPC}@{$ENDIF FPC}DoConnectionResult);
end;

procedure TDT_P2PVM_VirtualAuth_Client.Connect_M(addr, Port, Auth, User, Passwd: SystemString; OnResult: TOnState_M);
begin
  if Connecting then
      exit;
  Connecting := True;
  if not Reconnection then
    begin
      LastAddr := addr;
      LastPort := Port;
      LastAuth := Auth;
    end;
  LastUser := User;
  LastPasswd := Passwd;
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  OnConnectResultState.Init;
  OnConnectResultState.On_M := OnResult;
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_M := {$IFDEF FPC}@{$ENDIF FPC}DoAutomatedP2PVMClientConnectionDone;
  PhysicsTunnel.AsyncConnectM(addr, umlStrToInt(Port), {$IFDEF FPC}@{$ENDIF FPC}DoConnectionResult);
end;

procedure TDT_P2PVM_VirtualAuth_Client.Connect_P(addr, Port, Auth, User, Passwd: SystemString; OnResult: TOnState_P);
begin
  if Connecting then
      exit;
  Connecting := True;
  if not Reconnection then
    begin
      LastAddr := addr;
      LastPort := Port;
      LastAuth := Auth;
    end;
  LastUser := User;
  LastPasswd := Passwd;
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  OnConnectResultState.Init;
  OnConnectResultState.On_P := OnResult;
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_M := {$IFDEF FPC}@{$ENDIF FPC}DoAutomatedP2PVMClientConnectionDone;
  PhysicsTunnel.AsyncConnectM(addr, umlStrToInt(Port), {$IFDEF FPC}@{$ENDIF FPC}DoConnectionResult);
end;

procedure TDT_P2PVM_VirtualAuth_Client.Disconnect;
begin
  Connecting := False;
  Reconnection := False;
  LastAddr := '';
  LastPort := '';
  LastAuth := '';
  PhysicsTunnel.Disconnect;
end;

function TDT_P2PVM_VirtualAuth_Custom_Service.GetQuietMode: Boolean;
begin
  Result := RecvTunnel.QuietMode and SendTunnel.QuietMode;
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Service.SetQuietMode(const Value: Boolean);
begin
  RecvTunnel.QuietMode := Value;
  SendTunnel.QuietMode := Value;
end;

constructor TDT_P2PVM_VirtualAuth_Custom_Service.Create(ServiceClass_: TDTService_VirtualAuthClass; PhysicsTunnel_: TZNet_Server;
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
  RecvTunnel.PrefixName := 'VA';
  RecvTunnel.Name := P2PVM_Recv_Name_;

  SendTunnel := TZNet_WithP2PVM_Server.Create;
  SendTunnel.QuietMode := PhysicsTunnel_.QuietMode;
  SendTunnel.PrefixName := 'VA';
  SendTunnel.Name := P2PVM_Send_Name_;

  DTService := ServiceClass_.Create(RecvTunnel, SendTunnel);
  DTService.RegisterCommand;
  DTService.SwitchAsDefaultPerformance;

  Bind_PhysicsTunnel.AutomatedP2PVMServiceBind.AddService(RecvTunnel);
  Bind_PhysicsTunnel.AutomatedP2PVMServiceBind.AddService(SendTunnel);
  Bind_PhysicsTunnel.AutomatedP2PVMService := True;
  StartService();
end;

destructor TDT_P2PVM_VirtualAuth_Custom_Service.Destroy;
begin
  StopService;
  Bind_PhysicsTunnel.AutomatedP2PVMServiceBind.RemoveService(RecvTunnel);
  Bind_PhysicsTunnel.AutomatedP2PVMServiceBind.RemoveService(SendTunnel);
  DisposeObject(RecvTunnel);
  DisposeObject(SendTunnel);
  DisposeObject(DTService);
  inherited Destroy;
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Service.Progress;
begin
  Bind_PhysicsTunnel.Progress;
  DTService.Progress;
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Service.StartService;
begin
  RecvTunnel.StartService(Bind_P2PVM_Recv_IP6, Bind_P2PVM_Recv_Port);
  SendTunnel.StartService(Bind_P2PVM_Send_IP6, Bind_P2PVM_Send_Port);
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Service.StopService;
begin
  RecvTunnel.StopService;
  RecvTunnel.StopService;
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client_Clone_Pool.DoFree(var Data: TDT_P2PVM_VirtualAuth_Custom_Client);
begin
  if Data <> nil then
    begin
      Data.Clone_Instance_Ptr := nil;
      DisposeObjectAndNil(Data);
    end;
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.DoRegisterResult(const state: Boolean);
begin
  DTClient.UserLoginM(LastUser, LastPasswd, {$IFDEF FPC}@{$ENDIF FPC}DoLoginResult);
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.DoLoginResult(const state: Boolean);
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
      exit;
    end;

  DTClient.TunnelLinkM({$IFDEF FPC}@{$ENDIF FPC}DoTunnelLinkResult);
end;

function TDT_P2PVM_VirtualAuth_Custom_Client.GetQuietMode: Boolean;
begin
  Result := RecvTunnel.QuietMode and SendTunnel.QuietMode;
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.SetQuietMode(const Value: Boolean);
begin
  RecvTunnel.QuietMode := Value;
  SendTunnel.QuietMode := Value;
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.Do_Recv_Connect_State(const state: Boolean);
begin
  if not state then
      exit;
  if SendTunnel.RemoteInited then
      Connecting := False;
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.Do_Send_Connect_State(const state: Boolean);
begin
  if not state then
      exit;
  if RecvTunnel.RemoteInited then
      Connecting := False;
end;

constructor TDT_P2PVM_VirtualAuth_Custom_Client.Create(ClientClass_: TDTClient_VirtualAuthClass; PhysicsTunnel_: TZNet_Client;
  P2PVM_Recv_Name_, P2PVM_Recv_IP6_, P2PVM_Recv_Port_,
  P2PVM_Send_Name_, P2PVM_Send_IP6_, P2PVM_Send_Port_: SystemString);
begin
  inherited Create;
  // internal
  OnConnectResultState.Init;
  Connecting := False;
  Reconnection := False;

  // clone Technology
  Parent_Client := nil;
  Clone_Instance_Ptr := nil;
  Clone_Pool := TDT_P2PVM_VirtualAuth_Custom_Client_Clone_Pool.Create;

  // bind
  Bind_PhysicsTunnel := PhysicsTunnel_;
  Bind_P2PVM_Recv_IP6 := P2PVM_Recv_IP6_;
  Bind_P2PVM_Recv_Port := umlStrToInt(P2PVM_Recv_Port_);
  Bind_P2PVM_Send_IP6 := P2PVM_Send_IP6_;
  Bind_P2PVM_Send_Port := umlStrToInt(P2PVM_Send_Port_);

  // local
  RecvTunnel := TZNet_WithP2PVM_Client.Create;
  RecvTunnel.QuietMode := PhysicsTunnel_.QuietMode;
  RecvTunnel.PrefixName := 'VA';
  RecvTunnel.Name := P2PVM_Recv_Name_;
  SendTunnel := TZNet_WithP2PVM_Client.Create;
  SendTunnel.QuietMode := PhysicsTunnel_.QuietMode;
  SendTunnel.PrefixName := 'VA';
  SendTunnel.Name := P2PVM_Send_Name_;

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

constructor TDT_P2PVM_VirtualAuth_Custom_Client.Create_Clone(Parent_Client_: TDT_P2PVM_VirtualAuth_Custom_Client);
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
  Clone_Pool := TDT_P2PVM_VirtualAuth_Custom_Client_Clone_Pool.Create;

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
      RecvTunnel.AsyncConnectM(Bind_P2PVM_Recv_IP6, Bind_P2PVM_Recv_Port, {$IFDEF FPC}@{$ENDIF FPC}Do_Recv_Connect_State);
  if Parent_Client_.SendTunnel.RemoteInited then
      SendTunnel.AsyncConnectM(Bind_P2PVM_Send_IP6, Bind_P2PVM_Send_Port, {$IFDEF FPC}@{$ENDIF FPC}Do_Send_Connect_State);
end;

destructor TDT_P2PVM_VirtualAuth_Custom_Client.Destroy;
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
  DisposeObject(RecvTunnel);
  DisposeObject(SendTunnel);
  DisposeObject(DTClient);
  inherited Destroy;
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.Progress;
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

function TDT_P2PVM_VirtualAuth_Custom_Client.LoginIsSuccessed: Boolean;
begin
  Result := False;
  if (LastUser = '') or (LastPasswd = '') then
      exit;
  Result := DTClient.LinkOk;
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.DoTunnelLinkResult(const state: Boolean);
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

procedure TDT_P2PVM_VirtualAuth_Custom_Client.Connect(User, Passwd: SystemString);
begin
  if Connecting then
      exit;
  Connecting := True;
  if not Bind_PhysicsTunnel.RemoteInited then
    begin
      Connecting := False;
      exit;
    end;
  LastUser := User;
  LastPasswd := Passwd;
  OnConnectResultState.Init;

  if (LastUser = '') or (LastPasswd = '') then
      DTClient.TunnelLinkM({$IFDEF FPC}@{$ENDIF FPC}DoTunnelLinkResult)
  else if RegisterUserAndLogin then
      DTClient.RegisterUserM(LastUser, LastPasswd, {$IFDEF FPC}@{$ENDIF FPC}DoRegisterResult)
  else
      DTClient.UserLoginM(LastUser, LastPasswd, {$IFDEF FPC}@{$ENDIF FPC}DoLoginResult);
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.Connect;
begin
  Connect('', '');
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.Connect_C(User, Passwd: SystemString; OnResult: TOnState_C);
begin
  if Connecting then
      exit;
  Connecting := True;
  if not Bind_PhysicsTunnel.RemoteInited then
    begin
      Connecting := False;
      OnResult(False);
      exit;
    end;
  LastUser := User;
  LastPasswd := Passwd;
  OnConnectResultState.Init;
  OnConnectResultState.On_C := OnResult;
  if (LastUser = '') or (LastPasswd = '') then
      DTClient.TunnelLinkM({$IFDEF FPC}@{$ENDIF FPC}DoTunnelLinkResult)
  else if RegisterUserAndLogin then
      DTClient.RegisterUserM(LastUser, LastPasswd, {$IFDEF FPC}@{$ENDIF FPC}DoRegisterResult)
  else
      DTClient.UserLoginM(LastUser, LastPasswd, {$IFDEF FPC}@{$ENDIF FPC}DoLoginResult);
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.Connect_C(OnResult: TOnState_C);
begin
  Connect_C('', '', OnResult);
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.Connect_M(User, Passwd: SystemString; OnResult: TOnState_M);
begin
  if Connecting then
      exit;
  Connecting := True;
  if not Bind_PhysicsTunnel.RemoteInited then
    begin
      Connecting := False;
      OnResult(False);
      exit;
    end;
  LastUser := User;
  LastPasswd := Passwd;
  OnConnectResultState.Init;
  OnConnectResultState.On_M := OnResult;
  if (LastUser = '') or (LastPasswd = '') then
      DTClient.TunnelLinkM({$IFDEF FPC}@{$ENDIF FPC}DoTunnelLinkResult)
  else if RegisterUserAndLogin then
      DTClient.RegisterUserM(LastUser, LastPasswd, {$IFDEF FPC}@{$ENDIF FPC}DoRegisterResult)
  else
      DTClient.UserLoginM(LastUser, LastPasswd, {$IFDEF FPC}@{$ENDIF FPC}DoLoginResult);
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.Connect_M(OnResult: TOnState_M);
begin
  Connect_M('', '', OnResult);
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.Connect_P(User, Passwd: SystemString; OnResult: TOnState_P);
begin
  if Connecting then
      exit;
  Connecting := True;
  if not Bind_PhysicsTunnel.RemoteInited then
    begin
      Connecting := False;
      OnResult(False);
      exit;
    end;
  LastUser := User;
  LastPasswd := Passwd;
  OnConnectResultState.Init;
  OnConnectResultState.On_P := OnResult;
  if (LastUser = '') or (LastPasswd = '') then
      DTClient.TunnelLinkM({$IFDEF FPC}@{$ENDIF FPC}DoTunnelLinkResult)
  else if RegisterUserAndLogin then
      DTClient.RegisterUserM(LastUser, LastPasswd, {$IFDEF FPC}@{$ENDIF FPC}DoRegisterResult)
  else
      DTClient.UserLoginM(LastUser, LastPasswd, {$IFDEF FPC}@{$ENDIF FPC}DoLoginResult);
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.Connect_P(OnResult: TOnState_P);
begin
  Connect_P('', '', OnResult);
end;

procedure TDT_P2PVM_VirtualAuth_Custom_Client.Disconnect;
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
