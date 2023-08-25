{ ****************************************************************************** }
{ * DataStore Service                                                          * }
{ ****************************************************************************** }

unit Z.Net.DataStoreService;

{$I Z.Define.inc}

interface



uses Z.Core, Z.ListEngine, Z.UnicodeMixedLib, Z.DFE, Z.MemoryStream, Z.Net, Z.TextDataEngine,
  Z.Status, Z.Cadencer, Z.Notify, Z.PascalStrings, Z.UPascalStrings, Z.Cipher, Z.ZDB.Engine, Z.ZDB.ItemStream_LIB, Z.Compress,
  SysUtils, Z.Json,
  Z.Net.DoubleTunnelIO, Z.Net.DataStoreService.Common, Z.ZDB.LocalManager;

type
  TDataStoreService = class;
  TDataStoreService_SendTunnel_UserDefine = class;

  TDataStoreService_RecvTunnel_UserDefine = class(TService_RecvTunnel_UserDefine)
  private
    FPostPerformaceCounter: Integer;
    FLastPostPerformaceTime: TTimeTick;
    FPostCounterOfPerSec: Double;
  private
    { data security }
    FDataStoreCipherSecurity: TCipherSecurity;
    FDataStoreCipherKey: TCipherKeyBuffer;
    FCipherInstance: TCipher_Base;
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;

    procedure Progress; override;

    function SendTunnelDefine: TDataStoreService_SendTunnel_UserDefine;
    property PostCounterOfPerSec: Double read FPostCounterOfPerSec;

    { data security }
    procedure EncryptBuffer(sour: Pointer; Size: NativeInt; Encrypt: Boolean);
  end;

  TDataStoreService_SendTunnel_UserDefine = class(TService_SendTunnel_UserDefine)
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;

    function RecvTunnelDefine: TDataStoreService_RecvTunnel_UserDefine;
  end;

  TDataStoreService = class(TZNet_DoubleTunnelService, IZDBLocalManagerNotify)
  private
    FZDBLocal: TZDBLocalManager;
    FQuery_CPool: THashObjectList;
    FPerQueryPipelineDelayFreeTime: Double;
  protected
    { interface from IZDBLocalManagerNotify }
    procedure CreateQuery(pipe: TZDBPipeline); virtual;
    procedure QueryFragmentData(pipe: TZDBPipeline; FragmentSource: TMS64); virtual;
    procedure QueryDone(pipe: TZDBPipeline); virtual;
    procedure StorePosTransform(const Data: Pointer; const TransformBuff: PZDBStorePosTransformArray);
    procedure OpenDB(ActiveDB: TZDBLMStore); virtual;
    procedure CreateDB(ActiveDB: TZDBLMStore); virtual;
    procedure CloseDB(ActiveDB: TZDBLMStore); virtual;
    procedure InsertData(Sender: TZDBLMStore; InsertPos: Int64; buff: TCore_Stream; ID: Cardinal; CompletePos: Int64); virtual;
    procedure AddData(Sender: TZDBLMStore; buff: TCore_Stream; ID: Cardinal; CompletePos: Int64); virtual;
    procedure ModifyData(Sender: TZDBLMStore; const StorePos: Int64; buff: TCore_Stream); virtual;
    procedure DeleteData(Sender: TZDBLMStore; const StorePos: Int64); virtual;
  protected
    procedure DownloadQueryFilterMethod(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
    procedure DownloadQueryWithIDFilterMethod(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);

    procedure UserOut(UserDefineIO: TService_RecvTunnel_UserDefine); override;
    procedure UserLinkSuccess(UserDefineIO: TService_RecvTunnel_UserDefine); override;

    procedure Command_InitDB(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_CloseDB(Sender: TPeerIO; InData: TDFE); virtual;

    procedure Command_CopyDB(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_CompressDB(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_ReplaceDB(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_ResetData(Sender: TPeerIO; InData: TDFE); virtual;

    procedure Command_QueryDB(Sender: TPeerIO; InData: TDFE); virtual;

    procedure Command_DownloadDB(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_DownloadDBWithID(Sender: TPeerIO; InData: TDFE); virtual;

    procedure Command_RequestDownloadAssembleStream(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_RequestFastDownloadAssembleStream(Sender: TPeerIO; InData: TDFE); virtual;

    procedure Command_FastPostCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure Command_FastInsertCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure Command_FastModifyCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);

    procedure Command_CompletedPostAssembleStream(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_CompletedInsertAssembleStream(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_CompletedModifyAssembleStream(Sender: TPeerIO; InData: TDFE); virtual;

    procedure Command_DeleteData(Sender: TPeerIO; InData: TDFE); virtual;

    procedure Command_GetDBList(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetQueryList(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_GetQueryState(Sender: TPeerIO; InData, OutData: TDFE); virtual;
    procedure Command_QueryStop(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_QueryPause(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_QueryPlay(Sender: TPeerIO; InData: TDFE); virtual;

    { send client command }
    procedure Send_CompletedFragmentBigStream(pipe: TTDataStoreService_DBPipeline);
    procedure Send_CompletedQuery(pipe: TTDataStoreService_DBPipeline);
    procedure Send_CompletedDownloadAssemble(SendCli_: TPeerIO; dataBaseName_: SystemString; dStorePos: Int64; BackcallPtr: UInt64);
    procedure Send_CompletedFastDownloadAssemble(SendCli_: TPeerIO; dataBaseName_: SystemString; dStorePos: Int64; BackcallPtr: UInt64);
    procedure Send_CompletedStorePosTransform(SendCli_: TPeerIO; const BackcallPtr: UInt64; const TransformBuff: PZDBStorePosTransformArray);
  public
    constructor Create(RecvTunnel_, SendTunnel_: TZNet_Server); override;
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Progress; override;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); override;

    function GetDataStoreUserDefine(RecvCli: TPeerIO): TDataStoreService_RecvTunnel_UserDefine;

    function RegisterQuery_C(QuerierName_: SystemString): TTDataStoreService_Query_C;
    procedure UnRegisterQuery_C(QuerierName_: SystemString);
    function GetRegistedQuery_C(QuerierName_: SystemString): TTDataStoreService_Query_C;

    function PostCounterOfPerSec: Double;

    property ZDBLocal: TZDBLocalManager read FZDBLocal;
    property Query_CPool: THashObjectList read FQuery_CPool;
    property PerQueryPipelineDelayFreeTime: Double read FPerQueryPipelineDelayFreeTime write FPerQueryPipelineDelayFreeTime;
  end;

  TDataStoreClient = class(TZNet_DoubleTunnelClient)
  private
    FDataStoreCipherSecurity: TCipherSecurity;
    FDataStoreCipherKey: TCipherKeyBuffer;
    FCipherInstance: TCipher_Base;
    procedure EncryptBuffer(sour: Pointer; Size: NativeInt; Encrypt: Boolean);
    procedure Command_DataStoreSecurity(Sender: TPeerIO; InData: TDFE);
  private
    procedure Command_CompletedFragmentBigStream(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_CompletedQuery(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_CompletedDownloadAssemble(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_CompletedFastDownloadAssemble(Sender: TPeerIO; InData: TDFE); virtual;
    procedure Command_CompletedStorePosTransform(Sender: TPeerIO; InData: TDFE); virtual;
  public
    constructor Create(RecvTunnel_, SendTunnel_: TZNet_Client); override;
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Progress; override;

    function DataCipherKeyFinished: Boolean;

    procedure InitDB(InMem: Boolean; dataBaseName_: SystemString); virtual;
    procedure CloseDB(dataBaseName_: SystemString; CloseAndDeleted: Boolean); virtual;

    procedure CopyDB(dataBaseName_, CopyDestDatabaseName_: SystemString); overload;
    procedure CopyDB(dataBaseName_, CopyDestDatabaseName_: SystemString; const BackcallPtr: PStorePosTransformNotify); overload;
    procedure CopyDB_C(dataBaseName_, CopyDestDatabaseName_: SystemString; const OnDone_C: TStorePosTransformNotify_C); overload;
    procedure CopyDB_M(dataBaseName_, CopyDestDatabaseName_: SystemString; const OnDone_M: TStorePosTransformNotify_M); overload;
    procedure CopyDB_P(dataBaseName_, CopyDestDatabaseName_: SystemString; const OnDone_P: TStorePosTransformNotify_P); overload;

    procedure CompressDB(dataBaseName_: SystemString); overload;
    procedure CompressDB(dataBaseName_: SystemString; const BackcallPtr: PStorePosTransformNotify); overload;
    procedure CompressDB_C(dataBaseName_: SystemString; const OnDone_C: TStorePosTransformNotify_C); overload;
    procedure CompressDB_M(dataBaseName_: SystemString; const OnDone_M: TStorePosTransformNotify_M); overload;
    procedure CompressDB_P(dataBaseName_: SystemString; const OnDone_P: TStorePosTransformNotify_P); overload;

    procedure ReplaceDB(dataBaseName_, replaceN: SystemString); virtual;
    procedure ResetData(dataBaseName_: SystemString); virtual;

    procedure QuietQueryDB(RegistedQuerier_: SystemString; ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString; MaxWait: Double; MaxQueryResult: Int64); virtual;

    procedure QueryDB(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
      fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64; BackcallPtr: PDataStoreClientQueryNotify; RemoteParams: THashVariantList); overload; virtual;

    procedure QueryDBC(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
      fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
      RemoteParams: THashVariantList; { service ref remote parameter }
      OnQuery_C: TFillQueryData_C; OnDone_C: TQueryDoneNotify_C); overload;

    procedure QueryDBC(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
      fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
      RemoteParams: THashVariantList;                                           { service ref remote parameter }
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnQuery_C: TUserFillQueryData_C; OnDone_C: TUserQueryDoneNotify_C); overload;

    procedure QueryDBM(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
      fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
      RemoteParams: THashVariantList; { service ref remote parameter }
      OnQuery_M: TFillQueryData_M; OnDone_M: TQueryDoneNotify_M); overload;

    procedure QueryDBM(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
      fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
      RemoteParams: THashVariantList;                                           { service ref remote parameter }
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnQuery_M: TUserFillQueryData_M; OnDone_M: TUserQueryDoneNotify_M); overload;

    procedure QueryDBP(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
      fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
      RemoteParams: THashVariantList; { service ref remote parameter }
      OnQuery_P: TFillQueryData_P; OnDone_P: TQueryDoneNotify_P); overload;

    procedure QueryDBP(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
      fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
      RemoteParams: THashVariantList;                                           { service ref remote parameter }
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnQuery_P: TUserFillQueryData_P; OnDone_P: TUserQueryDoneNotify_P); overload;

    procedure QueryDBC(RegistedQuerier_: SystemString; dataBaseName_: SystemString; RemoteParams: THashVariantList; OnQuery_C: TFillQueryData_C; OnDone_C: TQueryDoneNotify_C); overload;
    procedure QueryDBM(RegistedQuerier_: SystemString; dataBaseName_: SystemString; RemoteParams: THashVariantList; OnQuery_M: TFillQueryData_M; OnDone_M: TQueryDoneNotify_M); overload;
    procedure QueryDBP(RegistedQuerier_: SystemString; dataBaseName_: SystemString; RemoteParams: THashVariantList; OnQuery_P: TFillQueryData_P; OnDone_P: TQueryDoneNotify_P); overload;

    procedure DownloadDB(ReverseQuery: Boolean; dataBaseName_: SystemString; BackcallPtr: PDataStoreClientQueryNotify); overload; virtual;
    procedure DownloadDBC(ReverseQuery: Boolean; dataBaseName_: SystemString; OnQuery_C: TFillQueryData_C; OnDone_C: TQueryDoneNotify_C); overload;
    procedure DownloadDBM(ReverseQuery: Boolean; dataBaseName_: SystemString; OnQuery_M: TFillQueryData_M; OnDone_M: TQueryDoneNotify_M); overload;
    procedure DownloadDBP(ReverseQuery: Boolean; dataBaseName_: SystemString; OnQuery_P: TFillQueryData_P; OnDone_P: TQueryDoneNotify_P); overload;

    procedure DownloadDBWithID(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; BackcallPtr: PDataStoreClientQueryNotify); overload; virtual;
    procedure DownloadDBWithIDC(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; OnQuery_C: TFillQueryData_C; OnDone_C: TQueryDoneNotify_C); overload;
    procedure DownloadDBWithIDM(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; OnQuery_M: TFillQueryData_M; OnDone_M: TQueryDoneNotify_M); overload;
    procedure DownloadDBWithIDP(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; OnQuery_P: TFillQueryData_P; OnDone_P: TQueryDoneNotify_P); overload;

    procedure BeginAssembleStream; virtual;

    procedure RequestDownloadAssembleStream(dataBaseName_: SystemString; StorePos: Int64; BackcallPtr: PDataStoreClientDownloadNotify); virtual;
    procedure DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; OnDone_C: TDownloadDoneNotify_C); overload;
    procedure DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; OnDone_M: TDownloadDoneNotify_M); overload;
    procedure DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; OnDone_P: TDownloadDoneNotify_P); overload;
    procedure DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDone_C: TDownloadDoneNotify_C); overload;
    procedure DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDone_M: TDownloadDoneNotify_M); overload;
    procedure DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDone_P: TDownloadDoneNotify_P); overload;

    procedure DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64;
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnDone_C: TUserDownloadDoneNotify_C); overload;
    procedure DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64;
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnDone_M: TUserDownloadDoneNotify_M); overload;
    procedure DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64;
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnDone_P: TUserDownloadDoneNotify_P); overload;

    procedure DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnDone_C: TUserDownloadDoneNotify_C); overload;
    procedure DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnDone_M: TUserDownloadDoneNotify_M); overload;
    procedure DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnDone_P: TUserDownloadDoneNotify_P); overload;

    procedure RequestFastDownloadAssembleStream(dataBaseName_: SystemString; StorePos: Int64; BackcallPtr: PDataStoreClientDownloadNotify); virtual;
    procedure FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; OnDone_C: TDownloadDoneNotify_C); overload;
    procedure FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; OnDone_M: TDownloadDoneNotify_M); overload;
    procedure FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; OnDone_P: TDownloadDoneNotify_P); overload;

    procedure FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDone_C: TDownloadDoneNotify_C); overload;
    procedure FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDone_M: TDownloadDoneNotify_M); overload;
    procedure FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDone_P: TDownloadDoneNotify_P); overload;

    procedure FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64;
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnDone_C: TUserDownloadDoneNotify_C); overload;
    procedure FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64;
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnDone_M: TUserDownloadDoneNotify_M); overload;
    procedure FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64;
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnDone_P: TUserDownloadDoneNotify_P); overload;

    procedure FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnDone_C: TUserDownloadDoneNotify_C); overload;
    procedure FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnDone_M: TUserDownloadDoneNotify_M); overload;
    procedure FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
      UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
      OnDone_P: TUserDownloadDoneNotify_P); overload;

    { Security post support }
    procedure PostAssembleStream(dataBaseName_: SystemString; stream: TMS64; dID: Cardinal; DoneTimeFree: Boolean); overload; virtual;
    procedure PostAssembleStreamCopy(dataBaseName_: SystemString; stream: TCore_Stream; dID: Cardinal);
    procedure PostAssembleStream(dataBaseName_: SystemString; DataSource: TDFE); overload;
    procedure PostAssembleStream(dataBaseName_: SystemString; DataSource: THashVariantList); overload;
    procedure PostAssembleStream(dataBaseName_: SystemString; DataSource: THashStringList); overload;
    procedure PostAssembleStream(dataBaseName_: SystemString; DataSource: TSectionTextData); overload;
    procedure PostAssembleStream(dataBaseName_: SystemString; DataSource: TZ_JsonObject); overload; virtual;
    procedure PostAssembleStream(dataBaseName_: SystemString; DataSource: TPascalString); overload;

    { Security insert support }
    procedure InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64; dID: Cardinal; DoneTimeFree: Boolean); overload; virtual;
    procedure InsertAssembleStreamCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCore_Stream; dID: Cardinal);
    procedure InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDFE); overload;
    procedure InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList); overload;
    procedure InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList); overload;
    procedure InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData); overload;
    procedure InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TZ_JsonObject); overload;
    procedure InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString); overload;

    { Security modify support }
    procedure ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64; DoneTimeFree: Boolean); overload; virtual;
    procedure ModifyAssembleStreamCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCore_Stream);
    procedure ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDFE); overload;
    procedure ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList); overload;
    procedure ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList); overload;
    procedure ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData); overload;
    procedure ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TZ_JsonObject); overload;
    procedure ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString); overload;

    procedure GetPostAssembleStreamStateM(OnResult: TOnStream_M); overload;
    procedure GetPostAssembleStreamStateM(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_M); overload;
    procedure GetPostAssembleStreamStateP(OnResult: TOnStream_P); overload;
    procedure GetPostAssembleStreamStateP(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_P); overload;

    procedure EndAssembleStream; virtual;

    procedure DeleteData(dataBaseName_: SystemString; dStorePos: Int64); virtual;

    { fast post support }
    procedure FastPostCompleteBuffer(dataBaseName_: SystemString; stream: TMS64; dID: Cardinal; DoneTimeFree: Boolean); overload; virtual;
    procedure FastPostCompleteBufferCopy(dataBaseName_: SystemString; stream: TCore_Stream; dID: Cardinal);
    procedure FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TDFE); overload;
    procedure FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: THashVariantList); overload;
    procedure FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: THashStringList); overload;
    procedure FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TSectionTextData); overload;
    procedure FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TZ_JsonObject); overload; virtual;
    procedure FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TPascalString); overload;

    { fast insert support }
    procedure FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64; dID: Cardinal; DoneTimeFree: Boolean); overload; virtual;
    procedure FastInsertCompleteBufferCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCore_Stream; dID: Cardinal);
    procedure FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDFE); overload;
    procedure FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList); overload;
    procedure FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList); overload;
    procedure FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData); overload;
    procedure FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TZ_JsonObject); overload;
    procedure FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString); overload;

    { fast modify support }
    procedure FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64; dID: Cardinal; DoneTimeFree: Boolean); overload; virtual;
    procedure FastModifyCompleteBufferCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCore_Stream; dID: Cardinal);
    procedure FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDFE); overload;
    procedure FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList); overload;
    procedure FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList); overload;
    procedure FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData); overload;
    procedure FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TZ_JsonObject); overload;
    procedure FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString); overload;

    procedure QueryStop(PipeName_: SystemString); virtual;
    procedure QueryPause(PipeName_: SystemString); virtual;
    procedure QueryPlay(PipeName_: SystemString); virtual;

    procedure GetDBListM(OnResult: TOnStream_M); overload;
    procedure GetDBListM(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_M); overload;
    procedure GetQueryListM(OnResult: TOnStream_M); overload;
    procedure GetQueryListM(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_M); overload;
    procedure GetQueryStateM(PipeName_: SystemString; OnResult: TOnStream_M); overload;
    procedure GetQueryStateM(PipeName_: SystemString; Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_M); overload;

    procedure GetDBListP(OnResult: TOnStream_P); overload;
    procedure GetDBListP(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_P); overload;
    procedure GetQueryListP(OnResult: TOnStream_P); overload;
    procedure GetQueryListP(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_P); overload;
    procedure GetQueryStateP(PipeName_: SystemString; OnResult: TOnStream_P); overload;
    procedure GetQueryStateP(PipeName_: SystemString; Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_P); overload;
  end;

implementation

type
  POnStorePosTransformTrigger = ^TOnStorePosTransformTrigger;

  TOnStorePosTransformTrigger = record
    Client_SendTunnel_ID: Cardinal;
    BackcallPtr: UInt64;
  end;

constructor TDataStoreService_RecvTunnel_UserDefine.Create(Owner_: TPeerIO);
type
  TCipherDef = array [0 .. 4] of TCipherSecurity;
const
  c: TCipherDef = (csRC6, csSerpent, csMars, csRijndael, csTwoFish);
var
  kref: TInt64;
begin
  inherited Create(Owner_);
  FPostPerformaceCounter := 0;
  FLastPostPerformaceTime := GetTimeTick;
  FPostCounterOfPerSec := 0;

  FDataStoreCipherSecurity := c[umlRandomRange(0, 4)];

  { generate random key }
  TMISC.GenerateRandomKey(kref, C_Int64_Size);
  TCipher.GenerateKey(FDataStoreCipherSecurity, @kref, C_Int64_Size, FDataStoreCipherKey);
  FCipherInstance := CreateCipherClass(FDataStoreCipherSecurity, FDataStoreCipherKey);
  FCipherInstance.Level := 1;
  FCipherInstance.CBC := True;
  FCipherInstance.ProcessTail := True;
end;

destructor TDataStoreService_RecvTunnel_UserDefine.Destroy;
begin
  DisposeObjectAndNil(FCipherInstance);
  inherited Destroy;
end;

procedure TDataStoreService_RecvTunnel_UserDefine.Progress;
var
  lastTime: TTimeTick;
begin
  lastTime := GetTimeTick;

  inherited Progress;

  if lastTime - FLastPostPerformaceTime > 1000 then
    begin
      try
        if FPostPerformaceCounter > 0 then
            FPostCounterOfPerSec := FPostPerformaceCounter / ((lastTime - FLastPostPerformaceTime) * 0.001)
        else
            FPostCounterOfPerSec := 0;
      except
          FPostCounterOfPerSec := 0;
      end;
      FLastPostPerformaceTime := lastTime;
      FPostPerformaceCounter := 0;
    end;
end;

function TDataStoreService_RecvTunnel_UserDefine.SendTunnelDefine: TDataStoreService_SendTunnel_UserDefine;
begin
  Result := SendTunnel as TDataStoreService_SendTunnel_UserDefine;
end;

procedure TDataStoreService_RecvTunnel_UserDefine.EncryptBuffer(sour: Pointer; Size: NativeInt; Encrypt: Boolean);
begin
  if FCipherInstance = nil then
      exit;
  if Encrypt then
      FCipherInstance.Encrypt(sour, Size)
  else
      FCipherInstance.Decrypt(sour, Size);
end;

constructor TDataStoreService_SendTunnel_UserDefine.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
end;

destructor TDataStoreService_SendTunnel_UserDefine.Destroy;
begin
  inherited Destroy;
end;

function TDataStoreService_SendTunnel_UserDefine.RecvTunnelDefine: TDataStoreService_RecvTunnel_UserDefine;
begin
  Result := RecvTunnel as TDataStoreService_RecvTunnel_UserDefine;
end;

procedure TDataStoreService.CreateQuery(pipe: TZDBPipeline);
var
  pl: TTDataStoreService_DBPipeline;
begin
  pl := TTDataStoreService_DBPipeline(pipe);
end;

procedure TDataStoreService.QueryFragmentData(pipe: TZDBPipeline; FragmentSource: TMS64);
var
  pl: TTDataStoreService_DBPipeline;
  DestStream: TMS64;
begin
  pl := TTDataStoreService_DBPipeline(pipe);
  if not pl.SyncToClient then
      exit;

  if not SendTunnel.Exists(pl.SendTunnel) then
      exit;

  DestStream := TMS64.Create;
  DestStream.SwapInstance(FragmentSource);

  TDataStoreService_RecvTunnel_UserDefine(pl.RecvTunnel).EncryptBuffer(DestStream.Memory, DestStream.Size, True);

  ClearBatchStream(pl.SendTunnel.Owner);
  PostBatchStream(pl.SendTunnel.Owner, DestStream, True);
  Send_CompletedFragmentBigStream(pl);
  ClearBatchStream(pl.SendTunnel.Owner);
end;

procedure TDataStoreService.QueryDone(pipe: TZDBPipeline);
var
  pl: TTDataStoreService_DBPipeline;
begin
  pl := TTDataStoreService_DBPipeline(pipe);

  if not FSendTunnel.Exists(pl.SendTunnel) then
      exit;

  Send_CompletedQuery(pl);
end;

procedure TDataStoreService.StorePosTransform(const Data: Pointer; const TransformBuff: PZDBStorePosTransformArray);
var
  p: POnStorePosTransformTrigger;
  de: TDFE;
begin
  if Data = nil then
      exit;
  p := POnStorePosTransformTrigger(Data);
  if (p^.BackcallPtr <> 0) and (FSendTunnel.Exists(p^.Client_SendTunnel_ID)) then
      Send_CompletedStorePosTransform(SendTunnel.PeerIO[p^.Client_SendTunnel_ID], p^.BackcallPtr, TransformBuff);
  Dispose(p);
end;

procedure TDataStoreService.OpenDB(ActiveDB: TZDBLMStore);
begin
end;

procedure TDataStoreService.CreateDB(ActiveDB: TZDBLMStore);
begin
end;

procedure TDataStoreService.CloseDB(ActiveDB: TZDBLMStore);
begin
end;

procedure TDataStoreService.InsertData(Sender: TZDBLMStore; InsertPos: Int64; buff: TCore_Stream; ID: Cardinal; CompletePos: Int64);
begin
end;

procedure TDataStoreService.AddData(Sender: TZDBLMStore; buff: TCore_Stream; ID: Cardinal; CompletePos: Int64);
begin
end;

procedure TDataStoreService.ModifyData(Sender: TZDBLMStore; const StorePos: Int64; buff: TCore_Stream);
begin
end;

procedure TDataStoreService.DeleteData(Sender: TZDBLMStore; const StorePos: Int64);
begin
end;

procedure TDataStoreService.DownloadQueryFilterMethod(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TDataStoreService.DownloadQueryWithIDFilterMethod(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
begin
  try
      Allowed := qState.ID = dPipe.UserVariant;
  except
      Allowed := False;
  end;
end;

procedure TDataStoreService.UserOut(UserDefineIO: TService_RecvTunnel_UserDefine);
var
  i: Integer;
  pl: TTDataStoreService_DBPipeline;
begin
  for i := 0 to FZDBLocal.QueryPipelineList.Count - 1 do
    begin
      pl := TTDataStoreService_DBPipeline(FZDBLocal.QueryPipelineList[i]);
      if pl.RecvTunnel = UserDefineIO.Owner.UserDefine then
          pl.stop;
    end;
  inherited UserOut(UserDefineIO);
end;

procedure TDataStoreService.UserLinkSuccess(UserDefineIO: TService_RecvTunnel_UserDefine);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  de: TDFE;
  arr: TDFArrayByte;
begin
  RT := UserDefineIO as TDataStoreService_RecvTunnel_UserDefine;
  de := TDFE.Create;
  de.WriteByte(Byte(RT.FDataStoreCipherSecurity));
  arr := de.WriteArrayByte;
  arr.AddPtrBuff(@RT.FDataStoreCipherKey[0], length(RT.FDataStoreCipherKey));
  RT.SendTunnel.Owner.SendDirectStreamCmd(C_DataStoreSecurity, de);
  DisposeObject(de);
  inherited UserLinkSuccess(UserDefineIO);
end;

procedure TDataStoreService.Command_InitDB(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  InMem: Boolean;
  dataBaseName_: SystemString;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  InMem := InData.Reader.ReadBool;
  dataBaseName_ := InData.Reader.ReadString;
  if InMem then
      FZDBLocal.InitMemoryDB(dataBaseName_)
  else
      FZDBLocal.InitDB(dataBaseName_, False);
end;

procedure TDataStoreService.Command_CloseDB(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  dataBaseName_: SystemString;
  CloseAndDeleted: Boolean;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  CloseAndDeleted := InData.Reader.ReadBool;

  if CloseAndDeleted then
      FZDBLocal.CloseAndDeleteDB(dataBaseName_)
  else
      FZDBLocal.CloseDB(dataBaseName_);
end;

procedure TDataStoreService.Command_CopyDB(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  dataBaseName_, copy2N: SystemString;
  BackcallPtr: UInt64;
  p: POnStorePosTransformTrigger;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  copy2N := InData.Reader.ReadString;
  BackcallPtr := InData.Reader.ReadPointer;

  new(p);
  p^.Client_SendTunnel_ID := RT.SendTunnelID;
  p^.BackcallPtr := BackcallPtr;
  FZDBLocal.CopyDB(dataBaseName_, copy2N, p, {$IFDEF FPC}@{$ENDIF FPC}StorePosTransform);
end;

procedure TDataStoreService.Command_CompressDB(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  dataBaseName_: SystemString;
  BackcallPtr: UInt64;
  p: POnStorePosTransformTrigger;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  BackcallPtr := InData.Reader.ReadPointer;

  new(p);
  p^.Client_SendTunnel_ID := RT.SendTunnelID;
  p^.BackcallPtr := BackcallPtr;
  FZDBLocal.CompressDB(dataBaseName_, p, {$IFDEF FPC}@{$ENDIF FPC}StorePosTransform);
end;

procedure TDataStoreService.Command_ReplaceDB(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  dataBaseName_, replaceN: SystemString;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  replaceN := InData.Reader.ReadString;
  FZDBLocal.ReplaceDB(dataBaseName_, replaceN);
end;

procedure TDataStoreService.Command_ResetData(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  dataBaseName_: SystemString;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  FZDBLocal.ResetData(dataBaseName_);
end;

procedure TDataStoreService.Command_QueryDB(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  RegedQueryName: SystemString;
  SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean;
  dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double;
  MaxQueryResult: Int64;

  AutoDestoryOutputDB: Boolean;
  DelayDestoryTime: Double;
  pl: TTDataStoreService_DBPipeline;
  qc: TTDataStoreService_Query_C;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  RegedQueryName := InData.Reader.ReadString;
  SyncToClient := InData.Reader.ReadBool;
  WriteResultToOutputDB := InData.Reader.ReadBool;
  InMem := InData.Reader.ReadBool;
  ReverseQuery := InData.Reader.ReadBool;
  dataBaseName_ := InData.Reader.ReadString;
  OutputDatabaseName_ := InData.Reader.ReadString;
  fragmentReponseTime := InData.Reader.ReadDouble;
  MaxWait := InData.Reader.ReadDouble;
  MaxQueryResult := InData.Reader.ReadInt64;

  if not FZDBLocal.ExistsDB(dataBaseName_) then
      exit;

  qc := TTDataStoreService_Query_C(FQuery_CPool[RegedQueryName]);

  if InMem then
      AutoDestoryOutputDB := True
  else
      AutoDestoryOutputDB := False;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.QueryDB(WriteResultToOutputDB, InMem, ReverseQuery, dataBaseName_, OutputDatabaseName_,
    AutoDestoryOutputDB, FPerQueryPipelineDelayFreeTime, fragmentReponseTime, MaxWait, 0, MaxQueryResult));
  pl.SendTunnel := RT.SendTunnelDefine;
  pl.RecvTunnel := RT;
  pl.BackcallPtr := InData.Reader.ReadPointer;
  pl.SyncToClient := SyncToClient;
  pl.RegistedQuery := RegedQueryName;
  pl.WriteFragmentBuffer := pl.SyncToClient;

  if InData.Reader.NotEnd then
      InData.Reader.ReadVariantList(pl.values);

  if qc <> nil then
    begin
      pl.OnDataFilter_M := qc.OnPipelineQuery;
      pl.OnDataDone_M := qc.OnPipelineQueryDone;
    end
  else
    begin
      pl.OnDataFilter_M := {$IFDEF FPC}@{$ENDIF FPC}DownloadQueryFilterMethod;
    end;
  ClearBatchStream(RT.SendTunnelDefine.Owner);
end;

procedure TDataStoreService.Command_DownloadDB(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  ReverseQuery: Boolean;
  dataBaseName_: SystemString;
  pl: TTDataStoreService_DBPipeline;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  ReverseQuery := InData.Reader.ReadBool;
  dataBaseName_ := InData.Reader.ReadString;

  if not FZDBLocal.ExistsDB(dataBaseName_) then
      exit;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.QueryDB(False, True, ReverseQuery, dataBaseName_, '', True, FPerQueryPipelineDelayFreeTime, 0.5, 0, 0, 0));
  pl.SendTunnel := RT.SendTunnelDefine;
  pl.RecvTunnel := RT;
  pl.BackcallPtr := InData.Reader.ReadPointer;
  pl.SyncToClient := True;
  pl.WriteFragmentBuffer := pl.SyncToClient;

  pl.OnDataFilter_M := {$IFDEF FPC}@{$ENDIF FPC}DownloadQueryFilterMethod;
  ClearBatchStream(RT.SendTunnelDefine.Owner);
end;

procedure TDataStoreService.Command_DownloadDBWithID(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  ReverseQuery: Boolean;
  dataBaseName_: SystemString;
  downloadWithID: Cardinal;
  pl: TTDataStoreService_DBPipeline;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  ReverseQuery := InData.Reader.ReadBool;
  dataBaseName_ := InData.Reader.ReadString;
  downloadWithID := InData.Reader.ReadCardinal;

  if not FZDBLocal.ExistsDB(dataBaseName_) then
      exit;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.QueryDB(False, True, ReverseQuery, dataBaseName_, '', True, FPerQueryPipelineDelayFreeTime, 0.5, 0, 0, 0));
  pl.SendTunnel := RT.SendTunnelDefine;
  pl.RecvTunnel := RT;
  pl.BackcallPtr := InData.Reader.ReadPointer;
  pl.SyncToClient := True;
  pl.WriteFragmentBuffer := pl.SyncToClient;

  { user download with ID }
  pl.UserVariant := downloadWithID;

  pl.OnDataFilter_M := {$IFDEF FPC}@{$ENDIF FPC}DownloadQueryWithIDFilterMethod;
  ClearBatchStream(RT.SendTunnelDefine.Owner);
end;

procedure TDataStoreService.Command_RequestDownloadAssembleStream(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  dataBaseName_: SystemString;
  StorePos: Int64;
  BackcallPtr: UInt64;
  M: TMS64;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  StorePos := InData.Reader.ReadInt64;
  BackcallPtr := InData.Reader.ReadPointer;

  M := TMS64.Create;
  if not FZDBLocal.ReadDBItemToZDBFragment(dataBaseName_, StorePos, M) then
    begin
      Sender.PrintParam('get Data Assemble Stream error: %s', dataBaseName_);
      DisposeObject(M);
      exit;
    end;

  RT.EncryptBuffer(M.Memory, M.Size, True);

  ClearBatchStream(RT.SendTunnelDefine.Owner);
  PostBatchStream(RT.SendTunnelDefine.Owner, M, True);
  Send_CompletedDownloadAssemble(RT.SendTunnelDefine.Owner, dataBaseName_, StorePos, BackcallPtr);
  ClearBatchStream(RT.SendTunnelDefine.Owner);
end;

procedure TDataStoreService.Command_RequestFastDownloadAssembleStream(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  dataBaseName_: SystemString;
  StorePos: Int64;
  BackcallPtr: UInt64;
  M: TMS64;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  StorePos := InData.Reader.ReadInt64;
  BackcallPtr := InData.Reader.ReadPointer;

  M := TMS64.Create;
  if not FZDBLocal.ReadDBItemToZDBFragment(dataBaseName_, StorePos, M) then
    begin
      Sender.PrintParam('get Data Assemble Stream error: %s', dataBaseName_);
      DisposeObject(M);
      exit;
    end;

  ClearBatchStream(RT.SendTunnelDefine.Owner);
  PostBatchStream(RT.SendTunnelDefine.Owner, M, True);
  Send_CompletedFastDownloadAssemble(RT.SendTunnelDefine.Owner, dataBaseName_, StorePos, BackcallPtr);
  ClearBatchStream(RT.SendTunnelDefine.Owner);
end;

procedure TDataStoreService.Command_FastPostCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  dataBaseName_: TPascalString;
  itmID: Cardinal;
  StorePos: Int64;
  output: Pointer;
  outputSiz: nativeUInt;
  m64: TMS64;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;
  inc(RT.FPostPerformaceCounter);

  DecodeZDBBuff(InData, DataSize, dataBaseName_, itmID, StorePos, output, outputSiz);
  m64 := TMS64.Create;
  m64.SetPointerWithProtectedMode(output, outputSiz);
  FZDBLocal.PostData(dataBaseName_, m64, itmID);
  DisposeObject(m64);
end;

procedure TDataStoreService.Command_FastInsertCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  dataBaseName_: TPascalString;
  itmID: Cardinal;
  StorePos: Int64;
  output: Pointer;
  outputSiz: nativeUInt;
  m64: TMS64;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;
  inc(RT.FPostPerformaceCounter);

  DecodeZDBBuff(InData, DataSize, dataBaseName_, itmID, StorePos, output, outputSiz);
  m64 := TMS64.Create;
  m64.SetPointerWithProtectedMode(output, outputSiz);
  FZDBLocal.InsertData(dataBaseName_, StorePos, m64, itmID);
  DisposeObject(m64);
end;

procedure TDataStoreService.Command_FastModifyCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  dataBaseName_: TPascalString;
  itmID: Cardinal;
  StorePos: Int64;
  output: Pointer;
  outputSiz: nativeUInt;
  m64: TMS64;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;
  inc(RT.FPostPerformaceCounter);

  DecodeZDBBuff(InData, DataSize, dataBaseName_, itmID, StorePos, output, outputSiz);
  m64 := TMS64.Create;
  m64.SetPointerWithProtectedMode(output, outputSiz);
  FZDBLocal.SetData(dataBaseName_, StorePos, m64);
  DisposeObject(m64);
end;

procedure TDataStoreService.Command_CompletedPostAssembleStream(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  dataBaseName_: SystemString;
  dID: Cardinal;
  p: PBigStreamBatchPostData;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  if RT.BigStreamBatchList.Count <= 0 then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  dID := InData.Reader.ReadCardinal;

  p := RT.BigStreamBatchList.Last;
  RT.EncryptBuffer(p^.Source.Memory, p^.Source.Size, False);
  p^.DBStorePos := FZDBLocal.PostData(dataBaseName_, p^.Source, dID);
  inc(RT.FPostPerformaceCounter);
end;

procedure TDataStoreService.Command_CompletedInsertAssembleStream(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  dataBaseName_: SystemString;
  dStorePos: Int64;
  dID: Cardinal;
  p: PBigStreamBatchPostData;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  if RT.BigStreamBatchList.Count <= 0 then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  dStorePos := InData.Reader.ReadInt64;
  dID := InData.Reader.ReadCardinal;

  p := RT.BigStreamBatchList.Last;
  RT.EncryptBuffer(p^.Source.Memory, p^.Source.Size, False);
  p^.DBStorePos := FZDBLocal.InsertData(dataBaseName_, dStorePos, p^.Source, dID);
  inc(RT.FPostPerformaceCounter);
end;

procedure TDataStoreService.Command_CompletedModifyAssembleStream(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  dataBaseName_: SystemString;
  dStorePos: Int64;
  p: PBigStreamBatchPostData;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  if RT.BigStreamBatchList.Count <= 0 then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  dStorePos := InData.Reader.ReadInt64;

  p := RT.BigStreamBatchList.Last;
  RT.EncryptBuffer(p^.Source.Memory, p^.Source.Size, False);

  if FZDBLocal.SetData(dataBaseName_, dStorePos, p^.Source) then
    begin
      p^.DBStorePos := dStorePos;
    end
  else
    begin
    end;
  inc(RT.FPostPerformaceCounter);
end;

procedure TDataStoreService.Command_DeleteData(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  dataBaseName_: SystemString;
  dStorePos: Int64;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  dStorePos := InData.Reader.ReadInt64;
  FZDBLocal.DeleteData(dataBaseName_, dStorePos);
  inc(RT.FPostPerformaceCounter);
end;

procedure TDataStoreService.Command_GetDBList(Sender: TPeerIO; InData, OutData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  lst: TCore_ListForObj;
  i: Integer;
  Database_: TZDBLMStore;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  lst := TCore_ListForObj.Create;
  FZDBLocal.GetDBList(lst);
  for i := 0 to lst.Count - 1 do
    begin
      Database_ := TZDBLMStore(lst[i]);
      OutData.WriteString(Database_.Name);
    end;
  DisposeObject(lst);
end;

procedure TDataStoreService.Command_GetQueryList(Sender: TPeerIO; InData, OutData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  i: Integer;
  pl: TTDataStoreService_DBPipeline;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;
  for i := 0 to FZDBLocal.QueryPipelineList.Count - 1 do
    begin
      pl := TTDataStoreService_DBPipeline(FZDBLocal.QueryPipelineList[i]);
      if (pl.RecvTunnel <> nil) and (pl.RecvTunnel.Owner = Sender) and
        (pl.Activted) and (pl.SourceDB <> nil) and (pl.OutputDB <> nil) then
          OutData.WriteString(pl.PipelineName);
    end;
end;

procedure TDataStoreService.Command_GetQueryState(Sender: TPeerIO; InData, OutData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  PipeName_: SystemString;
  pl: TTDataStoreService_DBPipeline;
  ps: TPipeState;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  PipeName_ := InData.Reader.ReadString;
  if not FZDBLocal.ExistsPipeline(PipeName_) then
      exit;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.PipelineN[PipeName_]);
  if pl = nil then
      exit;

  if not pl.Activted then
      exit;
  if pl.SourceDB = nil then
      exit;
  if pl.OutputDB = nil then
      exit;

  ps.Init;
  ps.WriteOutputDB := (pl.WriteResultToOutputDB);
  ps.Activted := (pl.Activted);
  ps.SyncToClient := (pl.SyncToClient);
  ps.MemoryMode := (pl.OutputDB.IsMemoryMode);
  ps.Paused := (pl.Paused);
  ps.DBCounter := (pl.SourceDB.Count);
  ps.QueryCounter := (pl.QueryCounter);
  ps.QueryResultCounter := (pl.QueryResultCounter);
  ps.MaxQueryCompare := (pl.MaxQueryCompare);
  ps.MaxQueryResult := (pl.MaxQueryResult);
  ps.QueryPerformanceOfPerSec := (pl.QueryCounterOfPerSec);
  ps.ConsumTime := (pl.QueryConsumTime);
  ps.MaxWaitTime := (pl.MaxWaitTime);
  ps.SourceDB := (pl.SourceDBName);
  ps.OutputDB := (pl.OutputDBName);
  ps.PipelineName := (pl.PipelineName);
  ps.RegistedQuery := (pl.RegistedQuery);
  ps.Encode(OutData);
  ps.Init;
end;

procedure TDataStoreService.Command_QueryStop(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  PipeName_: SystemString;
  pl: TTDataStoreService_DBPipeline;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  PipeName_ := InData.Reader.ReadString;
  if not FZDBLocal.ExistsPipeline(PipeName_) then
      exit;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.PipelineN[PipeName_]);
  if pl <> nil then
      pl.stop;
end;

procedure TDataStoreService.Command_QueryPause(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  PipeName_: SystemString;
  pl: TTDataStoreService_DBPipeline;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  PipeName_ := InData.Reader.ReadString;
  if not FZDBLocal.ExistsPipeline(PipeName_) then
      exit;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.PipelineN[PipeName_]);
  if pl <> nil then
      pl.Pause;
end;

procedure TDataStoreService.Command_QueryPlay(Sender: TPeerIO; InData: TDFE);
var
  RT: TDataStoreService_RecvTunnel_UserDefine;
  PipeName_: SystemString;
  pl: TTDataStoreService_DBPipeline;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  PipeName_ := InData.Reader.ReadString;
  if not FZDBLocal.ExistsPipeline(PipeName_) then
      exit;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.PipelineN[PipeName_]);
  if pl <> nil then
      pl.Play;
end;

procedure TDataStoreService.Send_CompletedFragmentBigStream(pipe: TTDataStoreService_DBPipeline);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(pipe.SourceDBName);
  de.WriteString(pipe.OutputDBName);
  de.WriteString(pipe.PipelineName);
  de.WritePointer(pipe.BackcallPtr);
  pipe.SendTunnel.Owner.SendDirectStreamCmd(C_CompletedFragmentBigStream, de);
  DisposeObject(de);
end;

procedure TDataStoreService.Send_CompletedQuery(pipe: TTDataStoreService_DBPipeline);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(pipe.SourceDBName);
  de.WriteString(pipe.OutputDBName);
  de.WriteString(pipe.PipelineName);
  de.WritePointer(pipe.BackcallPtr);
  de.WriteInt64(pipe.QueryResultCounter);
  pipe.SendTunnel.Owner.SendDirectStreamCmd(C_CompletedQuery, de);
  DisposeObject(de);
  ClearBatchStream(pipe.SendTunnel.Owner);
end;

procedure TDataStoreService.Send_CompletedDownloadAssemble(SendCli_: TPeerIO; dataBaseName_: SystemString; dStorePos: Int64; BackcallPtr: UInt64);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(dataBaseName_);
  de.WriteInt64(dStorePos);
  de.WritePointer(BackcallPtr);
  SendCli_.SendDirectStreamCmd(C_CompletedDownloadAssemble, de);
  DisposeObject(de);
  ClearBatchStream(SendCli_);
end;

procedure TDataStoreService.Send_CompletedFastDownloadAssemble(SendCli_: TPeerIO; dataBaseName_: SystemString; dStorePos: Int64; BackcallPtr: UInt64);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(dataBaseName_);
  de.WriteInt64(dStorePos);
  de.WritePointer(BackcallPtr);
  SendCli_.SendDirectStreamCmd(C_CompletedFastDownloadAssemble, de);
  DisposeObject(de);
  ClearBatchStream(SendCli_);
end;

procedure TDataStoreService.Send_CompletedStorePosTransform(SendCli_: TPeerIO; const BackcallPtr: UInt64; const TransformBuff: PZDBStorePosTransformArray);
var
  de: TDFE;
  i: Integer;
  arr: TDFArrayInt64;
begin
  de := TDFE.Create;
  de.WritePointer(BackcallPtr);

  arr := de.WriteArrayInt64;
  for i := 0 to length(TransformBuff^) - 1 do
      arr.Add(TransformBuff^[i].OriginPos);

  arr := de.WriteArrayInt64;
  for i := 0 to length(TransformBuff^) - 1 do
      arr.Add(TransformBuff^[i].NewPos);

  SendCli_.SendDirectStreamCmd(C_CompletedStorePosTransform, de);
  DisposeObject(de);
end;

constructor TDataStoreService.Create(RecvTunnel_, SendTunnel_: TZNet_Server);
begin
  inherited Create(RecvTunnel_, SendTunnel_);
  FRecvTunnel.PeerClientUserDefineClass := TDataStoreService_RecvTunnel_UserDefine;
  FSendTunnel.PeerClientUserDefineClass := TDataStoreService_SendTunnel_UserDefine;

  FZDBLocal := TZDBLocalManager.Create;
  FZDBLocal.PipelineClass := TTDataStoreService_DBPipeline;
  FZDBLocal.NotifyIntf := Self;

  FQuery_CPool := THashObjectList.Create(True);

  FPerQueryPipelineDelayFreeTime := 3.0;
end;

destructor TDataStoreService.Destroy;
begin
  DisposeObject([FZDBLocal, FQuery_CPool]);
  inherited Destroy;
end;

procedure TDataStoreService.RegisterCommand;
begin
  inherited RegisterCommand;

  FRecvTunnel.RegisterDirectStream(C_InitDB).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_InitDB;
  FRecvTunnel.RegisterDirectStream(C_CloseDB).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CloseDB;

  FRecvTunnel.RegisterDirectStream(C_CopyDB).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CopyDB;
  FRecvTunnel.RegisterDirectStream(C_CompressDB).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompressDB;
  FRecvTunnel.RegisterDirectStream(C_ReplaceDB).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ReplaceDB;
  FRecvTunnel.RegisterDirectStream(C_ResetData).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ResetData;

  FRecvTunnel.RegisterDirectStream(C_QueryDB).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_QueryDB;
  FRecvTunnel.RegisterDirectStream(C_DownloadDB).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_DownloadDB;
  FRecvTunnel.RegisterDirectStream(C_DownloadDBWithID).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_DownloadDBWithID;
  FRecvTunnel.RegisterDirectStream(C_RequestDownloadAssembleStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_RequestDownloadAssembleStream;
  FRecvTunnel.RegisterDirectStream(C_RequestFastDownloadAssembleStrea).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_RequestFastDownloadAssembleStream;

  FRecvTunnel.RegisterCompleteBuffer(C_FastPostCompleteBuffer).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_FastPostCompleteBuffer;
  FRecvTunnel.RegisterCompleteBuffer(C_FastInsertCompleteBuffer).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_FastInsertCompleteBuffer;
  FRecvTunnel.RegisterCompleteBuffer(C_FastModifyCompleteBuffer).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_FastModifyCompleteBuffer;

  FRecvTunnel.RegisterDirectStream(C_CompletedPostAssembleStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedPostAssembleStream;
  FRecvTunnel.RegisterDirectStream(C_CompletedInsertAssembleStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedInsertAssembleStream;
  FRecvTunnel.RegisterDirectStream(C_CompletedModifyAssembleStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedModifyAssembleStream;
  FRecvTunnel.RegisterDirectStream(C_DeleteData).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_DeleteData;

  FRecvTunnel.RegisterStream(C_GetDBList).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetDBList;
  FRecvTunnel.RegisterStream(C_GetQueryList).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetQueryList;
  FRecvTunnel.RegisterStream(C_GetQueryState).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetQueryState;
  FRecvTunnel.RegisterDirectStream(C_QueryStop).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_QueryStop;
  FRecvTunnel.RegisterDirectStream(C_QueryPause).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_QueryPause;
  FRecvTunnel.RegisterDirectStream(C_QueryPlay).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_QueryPlay;
end;

procedure TDataStoreService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD(C_InitDB);
  FRecvTunnel.DeleteRegistedCMD(C_CloseDB);

  FRecvTunnel.DeleteRegistedCMD(C_CopyDB);
  FRecvTunnel.DeleteRegistedCMD(C_CompressDB);
  FRecvTunnel.DeleteRegistedCMD(C_ReplaceDB);
  FRecvTunnel.DeleteRegistedCMD(C_ResetData);

  FRecvTunnel.DeleteRegistedCMD(C_QueryDB);
  FRecvTunnel.DeleteRegistedCMD(C_DownloadDB);
  FRecvTunnel.DeleteRegistedCMD(C_RequestDownloadAssembleStream);

  FRecvTunnel.DeleteRegistedCMD(C_FastPostCompleteBuffer);
  FRecvTunnel.DeleteRegistedCMD(C_FastInsertCompleteBuffer);
  FRecvTunnel.DeleteRegistedCMD(C_FastModifyCompleteBuffer);

  FRecvTunnel.DeleteRegistedCMD(C_CompletedPostAssembleStream);
  FRecvTunnel.DeleteRegistedCMD(C_CompletedInsertAssembleStream);
  FRecvTunnel.DeleteRegistedCMD(C_CompletedModifyAssembleStream);
  FRecvTunnel.DeleteRegistedCMD(C_DeleteData);

  FRecvTunnel.DeleteRegistedCMD(C_GetDBList);
  FRecvTunnel.DeleteRegistedCMD(C_GetQueryList);
  FRecvTunnel.DeleteRegistedCMD(C_GetQueryState);
  FRecvTunnel.DeleteRegistedCMD(C_QueryStop);
  FRecvTunnel.DeleteRegistedCMD(C_QueryPause);
  FRecvTunnel.DeleteRegistedCMD(C_QueryPlay);
end;

procedure TDataStoreService.Progress;
begin
  inherited Progress;
  FZDBLocal.Progress;
end;

procedure TDataStoreService.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  inherited CadencerProgress(Sender, deltaTime, newTime);
end;

function TDataStoreService.GetDataStoreUserDefine(RecvCli: TPeerIO): TDataStoreService_RecvTunnel_UserDefine;
begin
  Result := RecvCli.UserDefine as TDataStoreService_RecvTunnel_UserDefine;
end;

function TDataStoreService.RegisterQuery_C(QuerierName_: SystemString): TTDataStoreService_Query_C;
begin
  if FQuery_CPool.Exists(QuerierName_) then
      RaiseInfo('Query call already registed:%s', [QuerierName_]);

  Result := TTDataStoreService_Query_C.Create;
  FQuery_CPool[QuerierName_] := Result;
  DoStatus('Query Register info: "%s"', [QuerierName_]);
end;

procedure TDataStoreService.UnRegisterQuery_C(QuerierName_: SystemString);
begin
  if not FQuery_CPool.Exists(QuerierName_) then
      RaiseInfo('Query call not registed:%s', [QuerierName_]);

  FQuery_CPool.Delete(QuerierName_);
  DoStatus('Query UnRegister: "%s"', [QuerierName_]);
end;

function TDataStoreService.GetRegistedQuery_C(QuerierName_: SystemString): TTDataStoreService_Query_C;
begin
  Result := TTDataStoreService_Query_C(FQuery_CPool[QuerierName_]);
end;

function TDataStoreService.PostCounterOfPerSec: Double;
var
  IO_Array: TIO_Array;
  pcid: Cardinal;
  RT: TDataStoreService_RecvTunnel_UserDefine;
begin
  Result := 0;
  FRecvTunnel.GetIO_Array(IO_Array);
  for pcid in IO_Array do
    begin
      RT := GetDataStoreUserDefine(FRecvTunnel.PeerIO[pcid]);
      Result := Result + RT.FPostCounterOfPerSec;
    end;
end;

procedure TDataStoreClient.EncryptBuffer(sour: Pointer; Size: NativeInt; Encrypt: Boolean);
begin
  if FCipherInstance = nil then
      exit;
  if Encrypt then
      FCipherInstance.Encrypt(sour, Size)
  else
      FCipherInstance.Decrypt(sour, Size);
end;

procedure TDataStoreClient.Command_DataStoreSecurity(Sender: TPeerIO; InData: TDFE);
var
  arr: TDFArrayByte;
begin
  FDataStoreCipherSecurity := TCipherSecurity(InData.Reader.ReadByte);
  arr := InData.Reader.ReadArrayByte;
  SetLength(FDataStoreCipherKey, arr.Count);
  arr.GetBuff(@FDataStoreCipherKey[0]);

  DisposeObjectAndNil(FCipherInstance);
  FCipherInstance := CreateCipherClass(FDataStoreCipherSecurity, FDataStoreCipherKey);
  FCipherInstance.Level := 1;
  FCipherInstance.CBC := True;
  FCipherInstance.ProcessTail := True;
end;

procedure TDataStoreClient.Command_CompletedFragmentBigStream(Sender: TPeerIO; InData: TDFE);
var
  dataBaseName_, OutputDatabaseName_, PipeName_: SystemString;
  BackcallPtr: PDataStoreClientQueryNotify;
  M: TMS64;
begin
  dataBaseName_ := InData.Reader.ReadString;
  OutputDatabaseName_ := InData.Reader.ReadString;
  PipeName_ := InData.Reader.ReadString;
  BackcallPtr := PDataStoreClientQueryNotify(InData.Reader.ReadPointer);

  M := TMS64.Create;

  if Sender.UserDefine.BigStreamBatchList.Count > 0 then
    begin
      Sender.UserDefine.BigStreamBatchList.Last^.Source.Position := 0;
      EncryptBuffer(Sender.UserDefine.BigStreamBatchList.Last^.Source.Memory, Sender.UserDefine.BigStreamBatchList.Last^.Source.Size, False);
      Sender.UserDefine.BigStreamBatchList.Last^.Source.Position := 0;
      M.SwapInstance(Sender.UserDefine.BigStreamBatchList.Last^.Source);
      Sender.UserDefine.BigStreamBatchList.DeleteLast;
    end;

  if (BackcallPtr <> nil) and (M.Size > 0) then
    begin
      try
        M.Position := 0;
        if Assigned(BackcallPtr^.OnUserQuery_C) then
          begin
            FillFragmentSourceC(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, PipeName_, M, BackcallPtr^.OnUserQuery_C);
            M.Position := 0;
          end;
        if Assigned(BackcallPtr^.OnUserQuery_M) then
          begin
            FillFragmentSourceM(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, PipeName_, M, BackcallPtr^.OnUserQuery_M);
            M.Position := 0;
          end;
        if Assigned(BackcallPtr^.OnUserQuery_P) then
          begin
            FillFragmentSourceP(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, PipeName_, M, BackcallPtr^.OnUserQuery_P);
            M.Position := 0;
          end;
        if Assigned(BackcallPtr^.OnQuery_C) then
          begin
            FillFragmentSourceC(dataBaseName_, PipeName_, M, BackcallPtr^.OnQuery_C);
            M.Position := 0;
          end;
        if Assigned(BackcallPtr^.OnQuery_M) then
          begin
            FillFragmentSourceM(dataBaseName_, PipeName_, M, BackcallPtr^.OnQuery_M);
            M.Position := 0;
          end;
        if Assigned(BackcallPtr^.OnQuery_P) then
          begin
            FillFragmentSourceP(dataBaseName_, PipeName_, M, BackcallPtr^.OnQuery_P);
            M.Position := 0;
          end;
      except
      end;
    end;

  DisposeObject(M);
end;

procedure TDataStoreClient.Command_CompletedQuery(Sender: TPeerIO; InData: TDFE);
var
  dataBaseName_, OutputDatabaseName_, PipeName_: SystemString;
  BackcallPtr: PDataStoreClientQueryNotify;
  TotalResultCount: Int64;
begin
  dataBaseName_ := InData.Reader.ReadString;
  OutputDatabaseName_ := InData.Reader.ReadString;
  PipeName_ := InData.Reader.ReadString;
  BackcallPtr := PDataStoreClientQueryNotify(InData.Reader.ReadPointer);
  TotalResultCount := InData.Reader.ReadInt64;

  if BackcallPtr <> nil then
    begin
      try
        if Assigned(BackcallPtr^.OnUserDone_C) then
            BackcallPtr^.OnUserDone_C(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, OutputDatabaseName_, PipeName_, TotalResultCount);
        if Assigned(BackcallPtr^.OnUserDone_M) then
            BackcallPtr^.OnUserDone_M(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, OutputDatabaseName_, PipeName_, TotalResultCount);
        if Assigned(BackcallPtr^.OnUserDone_P) then
            BackcallPtr^.OnUserDone_P(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, OutputDatabaseName_, PipeName_, TotalResultCount);

        if Assigned(BackcallPtr^.OnDone_C) then
            BackcallPtr^.OnDone_C(dataBaseName_, OutputDatabaseName_, PipeName_, TotalResultCount);
        if Assigned(BackcallPtr^.OnDone_M) then
            BackcallPtr^.OnDone_M(dataBaseName_, OutputDatabaseName_, PipeName_, TotalResultCount);
        if Assigned(BackcallPtr^.OnDone_P) then
            BackcallPtr^.OnDone_P(dataBaseName_, OutputDatabaseName_, PipeName_, TotalResultCount);
      except
      end;
      Dispose(BackcallPtr);
    end;
  Sender.UserDefine.BigStreamBatchList.Clear;
end;

procedure TDataStoreClient.Command_CompletedDownloadAssemble(Sender: TPeerIO; InData: TDFE);
var
  dataBaseName_: SystemString;
  dStorePos: Int64;
  BackcallPtr: PDataStoreClientDownloadNotify;
  M, tmp: TMS64;
begin
  dataBaseName_ := InData.Reader.ReadString;
  dStorePos := InData.Reader.ReadInt64;
  BackcallPtr := PDataStoreClientDownloadNotify(InData.Reader.ReadPointer);

  if Sender.UserDefine.BigStreamBatchList.Count > 0 then
      M := Sender.UserDefine.BigStreamBatchList.Last^.Source
  else
      M := nil;

  if M <> nil then
    begin
      if BackcallPtr <> nil then
        begin
          EncryptBuffer(M.Memory, M.Size, False);

          if BackcallPtr^.AutoDecodeZDBStream then
              tmp := DecodeZDBFragment(M)
          else
              tmp := M;

          try
            tmp.Position := 0;
            if Assigned(BackcallPtr^.OnUserDone_C) then
              begin
                BackcallPtr^.OnUserDone_C(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnUserDone_M) then
              begin
                BackcallPtr^.OnUserDone_M(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnUserDone_P) then
              begin
                BackcallPtr^.OnUserDone_P(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;

            if Assigned(BackcallPtr^.OnDone_C) then
              begin
                BackcallPtr^.OnDone_C(dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnDone_M) then
              begin
                BackcallPtr^.OnDone_M(dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnDone_P) then
              begin
                BackcallPtr^.OnDone_P(dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
          except
          end;

          if BackcallPtr^.AutoDecodeZDBStream then
              DisposeObject(tmp);
          Dispose(BackcallPtr);
        end;
      Sender.UserDefine.BigStreamBatchList.DeleteLast;
    end;
end;

procedure TDataStoreClient.Command_CompletedFastDownloadAssemble(Sender: TPeerIO; InData: TDFE);
var
  dataBaseName_: SystemString;
  dStorePos: Int64;
  BackcallPtr: PDataStoreClientDownloadNotify;
  M, tmp: TMS64;
begin
  dataBaseName_ := InData.Reader.ReadString;
  dStorePos := InData.Reader.ReadInt64;
  BackcallPtr := PDataStoreClientDownloadNotify(InData.Reader.ReadPointer);

  if Sender.UserDefine.BigStreamBatchList.Count > 0 then
      M := Sender.UserDefine.BigStreamBatchList.Last^.Source
  else
      M := nil;

  if M <> nil then
    begin
      if BackcallPtr <> nil then
        begin
          if BackcallPtr^.AutoDecodeZDBStream then
              tmp := DecodeZDBFragment(M)
          else
              tmp := M;

          try
            tmp.Position := 0;
            if Assigned(BackcallPtr^.OnUserDone_C) then
              begin
                BackcallPtr^.OnUserDone_C(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnUserDone_M) then
              begin
                BackcallPtr^.OnUserDone_M(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnUserDone_P) then
              begin
                BackcallPtr^.OnUserDone_P(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;

            if Assigned(BackcallPtr^.OnDone_C) then
              begin
                BackcallPtr^.OnDone_C(dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnDone_M) then
              begin
                BackcallPtr^.OnDone_M(dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnDone_P) then
              begin
                BackcallPtr^.OnDone_P(dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
          except
          end;

          if BackcallPtr^.AutoDecodeZDBStream then
              DisposeObject(tmp);
          Dispose(BackcallPtr);
        end;
      Sender.UserDefine.BigStreamBatchList.DeleteLast;
    end;
end;

procedure TDataStoreClient.Command_CompletedStorePosTransform(Sender: TPeerIO; InData: TDFE);
var
  BackcallPtr: PStorePosTransformNotify;
  arr: TDFArrayInt64;
  i: Integer;
  TransformBuff: TZDBStorePosTransformArray;
begin
  BackcallPtr := PStorePosTransformNotify(InData.Reader.ReadPointer);

  arr := InData.Reader.ReadArrayInt64;
  SetLength(TransformBuff, arr.Count);
  for i := 0 to arr.Count - 1 do
      TransformBuff[i].OriginPos := arr[i];

  arr := InData.Reader.ReadArrayInt64;
  for i := 0 to arr.Count - 1 do
      TransformBuff[i].NewPos := arr[i];

  if BackcallPtr <> nil then
    begin
      if Assigned(BackcallPtr^.OnDone_C) then
          BackcallPtr^.OnDone_C(@TransformBuff);
      if Assigned(BackcallPtr^.OnDone_M) then
          BackcallPtr^.OnDone_M(@TransformBuff);
      if Assigned(BackcallPtr^.OnDone_P) then
          BackcallPtr^.OnDone_P(@TransformBuff);
    end;

  SetLength(TransformBuff, 0);
  Dispose(BackcallPtr);
end;

constructor TDataStoreClient.Create(RecvTunnel_, SendTunnel_: TZNet_Client);
begin
  inherited Create(RecvTunnel_, SendTunnel_);
  FDataStoreCipherSecurity := TCipherSecurity.csNone;
  SetLength(FDataStoreCipherKey, 0);
  FCipherInstance := nil;
end;

destructor TDataStoreClient.Destroy;
begin
  DisposeObjectAndNil(FCipherInstance);
  inherited Destroy;
end;

procedure TDataStoreClient.RegisterCommand;
begin
  inherited RegisterCommand;
  FRecvTunnel.RegisterDirectStream(C_DataStoreSecurity).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_DataStoreSecurity;
  FRecvTunnel.RegisterDirectStream(C_CompletedFragmentBigStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedFragmentBigStream;
  FRecvTunnel.RegisterDirectStream(C_CompletedQuery).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedQuery;
  FRecvTunnel.RegisterDirectStream(C_CompletedDownloadAssemble).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedDownloadAssemble;
  FRecvTunnel.RegisterDirectStream(C_CompletedFastDownloadAssemble).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedFastDownloadAssemble;
  FRecvTunnel.RegisterDirectStream(C_CompletedStorePosTransform).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedStorePosTransform;
end;

procedure TDataStoreClient.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD(C_CompletedFragmentBigStream);
  FRecvTunnel.DeleteRegistedCMD(C_CompletedQuery);
  FRecvTunnel.DeleteRegistedCMD(C_CompletedDownloadAssemble);
  FRecvTunnel.DeleteRegistedCMD(C_CompletedFastDownloadAssemble);
  FRecvTunnel.DeleteRegistedCMD(C_CompletedStorePosTransform);
end;

procedure TDataStoreClient.Progress;
begin
  inherited Progress;
end;

function TDataStoreClient.DataCipherKeyFinished: Boolean;
begin
  Result := (length(FDataStoreCipherKey) > 0) or (Self.FDataStoreCipherSecurity <> csNone);
end;

procedure TDataStoreClient.InitDB(InMem: Boolean; dataBaseName_: SystemString);
var
  de: TDFE;
begin
  de := TDFE.Create;

  de.WriteBool(InMem);
  de.WriteString(dataBaseName_);

  SendTunnel.SendDirectStreamCmd(C_InitDB, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.CloseDB(dataBaseName_: SystemString; CloseAndDeleted: Boolean);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(dataBaseName_);
  de.WriteBool(CloseAndDeleted);
  SendTunnel.SendDirectStreamCmd(C_CloseDB, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.CopyDB(dataBaseName_, CopyDestDatabaseName_: SystemString);
begin
  CopyDB(dataBaseName_, CopyDestDatabaseName_, nil);
end;

procedure TDataStoreClient.CopyDB(dataBaseName_, CopyDestDatabaseName_: SystemString; const BackcallPtr: PStorePosTransformNotify);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(dataBaseName_);
  de.WriteString(CopyDestDatabaseName_);
  de.WritePointer(BackcallPtr);
  SendTunnel.SendDirectStreamCmd(C_CopyDB, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.CopyDB_C(dataBaseName_, CopyDestDatabaseName_: SystemString; const OnDone_C: TStorePosTransformNotify_C);
var
  p: PStorePosTransformNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_C := OnDone_C;
  CopyDB(dataBaseName_, CopyDestDatabaseName_, p);
end;

procedure TDataStoreClient.CopyDB_M(dataBaseName_, CopyDestDatabaseName_: SystemString; const OnDone_M: TStorePosTransformNotify_M);
var
  p: PStorePosTransformNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_M := OnDone_M;
  CopyDB(dataBaseName_, CopyDestDatabaseName_, p);
end;

procedure TDataStoreClient.CopyDB_P(dataBaseName_, CopyDestDatabaseName_: SystemString; const OnDone_P: TStorePosTransformNotify_P);
var
  p: PStorePosTransformNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_P := OnDone_P;
  CopyDB(dataBaseName_, CopyDestDatabaseName_, p);
end;

procedure TDataStoreClient.CompressDB(dataBaseName_: SystemString);
begin
  CompressDB(dataBaseName_, nil);
end;

procedure TDataStoreClient.CompressDB(dataBaseName_: SystemString; const BackcallPtr: PStorePosTransformNotify);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(dataBaseName_);
  de.WritePointer(BackcallPtr);
  SendTunnel.SendDirectStreamCmd(C_CompressDB, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.CompressDB_C(dataBaseName_: SystemString; const OnDone_C: TStorePosTransformNotify_C);
var
  p: PStorePosTransformNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_C := OnDone_C;
  CompressDB(dataBaseName_, p);
end;

procedure TDataStoreClient.CompressDB_M(dataBaseName_: SystemString; const OnDone_M: TStorePosTransformNotify_M);
var
  p: PStorePosTransformNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_M := OnDone_M;
  CompressDB(dataBaseName_, p);
end;

procedure TDataStoreClient.CompressDB_P(dataBaseName_: SystemString; const OnDone_P: TStorePosTransformNotify_P);
var
  p: PStorePosTransformNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_P := OnDone_P;
  CompressDB(dataBaseName_, p);
end;

procedure TDataStoreClient.ReplaceDB(dataBaseName_, replaceN: SystemString);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(dataBaseName_);
  de.WriteString(replaceN);
  SendTunnel.SendDirectStreamCmd(C_ReplaceDB, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.ResetData(dataBaseName_: SystemString);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(dataBaseName_);
  SendTunnel.SendDirectStreamCmd(C_ResetData, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.QuietQueryDB(RegistedQuerier_: SystemString; ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString; MaxWait: Double; MaxQueryResult: Int64);
var
  de: TDFE;
begin
  de := TDFE.Create;

  de.WriteString(RegistedQuerier_);
  de.WriteBool(False); { sync to client }
  de.WriteBool(True);  { write output Database_ }
  de.WriteBool(False); { in memory }
  de.WriteBool(ReverseQuery);
  de.WriteString(dataBaseName_);
  de.WriteString(OutputDatabaseName_);
  de.WriteDouble(0.1); { fragmentReponseTime }
  de.WriteDouble(MaxWait);
  de.WriteInt64(MaxQueryResult);
  de.WritePointer(0); { backcall address }

  SendTunnel.SendDirectStreamCmd(C_QueryDB, de);

  DisposeObject(de);
end;

procedure TDataStoreClient.QueryDB(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64; BackcallPtr: PDataStoreClientQueryNotify; RemoteParams: THashVariantList);
var
  de: TDFE;
begin
  de := TDFE.Create;

  de.WriteString(RegistedQuerier_);
  de.WriteBool(SyncToClient); { sync to client }
  de.WriteBool(WriteResultToOutputDB);
  de.WriteBool(InMem);
  de.WriteBool(ReverseQuery);
  de.WriteString(dataBaseName_);
  de.WriteString(OutputDatabaseName_);
  de.WriteDouble(fragmentReponseTime);
  de.WriteDouble(MaxWait);
  de.WriteInt64(MaxQueryResult);
  de.WritePointer(BackcallPtr);
  if RemoteParams <> nil then
      de.WriteVariantList(RemoteParams);

  SendTunnel.SendDirectStreamCmd(C_QueryDB, de);

  DisposeObject(de);
end;

procedure TDataStoreClient.QueryDBC(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
  RemoteParams: THashVariantList; OnQuery_C: TFillQueryData_C; OnDone_C: TQueryDoneNotify_C);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQuery_C := OnQuery_C;
  p^.OnDone_C := OnDone_C;
  QueryDB(RegistedQuerier_, SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery, dataBaseName_, OutputDatabaseName_, fragmentReponseTime, MaxWait, MaxQueryResult, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBC(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
  RemoteParams: THashVariantList;                                           { service ref remote parameter }
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnQuery_C: TUserFillQueryData_C; OnDone_C: TUserQueryDoneNotify_C);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserQuery_C := OnQuery_C;
  p^.OnUserDone_C := OnDone_C;
  QueryDB(RegistedQuerier_, SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery, dataBaseName_, OutputDatabaseName_, fragmentReponseTime, MaxWait, MaxQueryResult, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBM(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
  RemoteParams: THashVariantList; OnQuery_M: TFillQueryData_M; OnDone_M: TQueryDoneNotify_M);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQuery_M := OnQuery_M;
  p^.OnDone_M := OnDone_M;
  QueryDB(RegistedQuerier_, SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery, dataBaseName_, OutputDatabaseName_, fragmentReponseTime, MaxWait, MaxQueryResult, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBM(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
  RemoteParams: THashVariantList;                                           { service ref remote parameter }
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnQuery_M: TUserFillQueryData_M; OnDone_M: TUserQueryDoneNotify_M);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserQuery_M := OnQuery_M;
  p^.OnUserDone_M := OnDone_M;
  QueryDB(RegistedQuerier_, SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery, dataBaseName_, OutputDatabaseName_, fragmentReponseTime, MaxWait, MaxQueryResult, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBP(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
  RemoteParams: THashVariantList; OnQuery_P: TFillQueryData_P; OnDone_P: TQueryDoneNotify_P);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQuery_P := OnQuery_P;
  p^.OnDone_P := OnDone_P;
  QueryDB(RegistedQuerier_, SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery, dataBaseName_, OutputDatabaseName_, fragmentReponseTime, MaxWait, MaxQueryResult, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBP(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
  RemoteParams: THashVariantList;                                           { service ref remote parameter }
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnQuery_P: TUserFillQueryData_P; OnDone_P: TUserQueryDoneNotify_P);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserQuery_P := OnQuery_P;
  p^.OnUserDone_P := OnDone_P;
  QueryDB(RegistedQuerier_, SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery, dataBaseName_, OutputDatabaseName_, fragmentReponseTime, MaxWait, MaxQueryResult, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBC(RegistedQuerier_: SystemString; dataBaseName_: SystemString; RemoteParams: THashVariantList; OnQuery_C: TFillQueryData_C; OnDone_C: TQueryDoneNotify_C);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQuery_C := OnQuery_C;
  p^.OnDone_C := OnDone_C;
  QueryDB(RegistedQuerier_, True, False, True, False, dataBaseName_, 'Memory', 0.5, 0, 0, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBM(RegistedQuerier_: SystemString; dataBaseName_: SystemString; RemoteParams: THashVariantList; OnQuery_M: TFillQueryData_M; OnDone_M: TQueryDoneNotify_M);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQuery_M := OnQuery_M;
  p^.OnDone_M := OnDone_M;
  QueryDB(RegistedQuerier_, True, False, True, False, dataBaseName_, 'Memory', 0.5, 0, 0, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBP(RegistedQuerier_: SystemString; dataBaseName_: SystemString; RemoteParams: THashVariantList; OnQuery_P: TFillQueryData_P; OnDone_P: TQueryDoneNotify_P);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQuery_P := OnQuery_P;
  p^.OnDone_P := OnDone_P;
  QueryDB(RegistedQuerier_, True, False, True, False, dataBaseName_, 'Memory', 0.5, 0, 0, p, RemoteParams);
end;

procedure TDataStoreClient.DownloadDB(ReverseQuery: Boolean; dataBaseName_: SystemString; BackcallPtr: PDataStoreClientQueryNotify);
var
  de: TDFE;
begin
  de := TDFE.Create;

  de.WriteBool(ReverseQuery);
  de.WriteString(dataBaseName_);
  de.WritePointer(BackcallPtr);

  SendTunnel.SendDirectStreamCmd(C_DownloadDB, de);

  DisposeObject(de);
end;

procedure TDataStoreClient.DownloadDBC(ReverseQuery: Boolean; dataBaseName_: SystemString; OnQuery_C: TFillQueryData_C; OnDone_C: TQueryDoneNotify_C);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQuery_C := OnQuery_C;
  p^.OnDone_C := OnDone_C;
  DownloadDB(ReverseQuery, dataBaseName_, p);
end;

procedure TDataStoreClient.DownloadDBM(ReverseQuery: Boolean; dataBaseName_: SystemString; OnQuery_M: TFillQueryData_M; OnDone_M: TQueryDoneNotify_M);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQuery_M := OnQuery_M;
  p^.OnDone_M := OnDone_M;
  DownloadDB(ReverseQuery, dataBaseName_, p);
end;

procedure TDataStoreClient.DownloadDBP(ReverseQuery: Boolean; dataBaseName_: SystemString; OnQuery_P: TFillQueryData_P; OnDone_P: TQueryDoneNotify_P);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQuery_P := OnQuery_P;
  p^.OnDone_P := OnDone_P;
  DownloadDB(ReverseQuery, dataBaseName_, p);
end;

procedure TDataStoreClient.DownloadDBWithID(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; BackcallPtr: PDataStoreClientQueryNotify);
var
  de: TDFE;
begin
  de := TDFE.Create;

  de.WriteBool(ReverseQuery);
  de.WriteString(dataBaseName_);
  de.WriteCardinal(db_ID);
  de.WritePointer(BackcallPtr);

  SendTunnel.SendDirectStreamCmd(C_DownloadDBWithID, de);

  DisposeObject(de);
end;

procedure TDataStoreClient.DownloadDBWithIDC(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; OnQuery_C: TFillQueryData_C; OnDone_C: TQueryDoneNotify_C);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQuery_C := OnQuery_C;
  p^.OnDone_C := OnDone_C;
  DownloadDBWithID(ReverseQuery, dataBaseName_, db_ID, p);
end;

procedure TDataStoreClient.DownloadDBWithIDM(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; OnQuery_M: TFillQueryData_M; OnDone_M: TQueryDoneNotify_M);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQuery_M := OnQuery_M;
  p^.OnDone_M := OnDone_M;
  DownloadDBWithID(ReverseQuery, dataBaseName_, db_ID, p);
end;

procedure TDataStoreClient.DownloadDBWithIDP(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; OnQuery_P: TFillQueryData_P; OnDone_P: TQueryDoneNotify_P);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQuery_P := OnQuery_P;
  p^.OnDone_P := OnDone_P;
  DownloadDBWithID(ReverseQuery, dataBaseName_, db_ID, p);
end;

procedure TDataStoreClient.BeginAssembleStream;
begin
  ClearBatchStream;
end;

procedure TDataStoreClient.RequestDownloadAssembleStream(dataBaseName_: SystemString; StorePos: Int64; BackcallPtr: PDataStoreClientDownloadNotify);
var
  de: TDFE;
begin
  de := TDFE.Create;

  de.WriteString(dataBaseName_);
  de.WriteInt64(StorePos);
  de.WritePointer(BackcallPtr);

  SendTunnel.SendDirectStreamCmd(C_RequestDownloadAssembleStream, de);

  DisposeObject(de);
end;

procedure TDataStoreClient.DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; OnDone_C: TDownloadDoneNotify_C);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_C := OnDone_C;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; OnDone_M: TDownloadDoneNotify_M);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_M := OnDone_M;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; OnDone_P: TDownloadDoneNotify_P);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_P := OnDone_P;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDone_C: TDownloadDoneNotify_C);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_C := OnDone_C;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDone_M: TDownloadDoneNotify_M);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_M := OnDone_M;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDone_P: TDownloadDoneNotify_P);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_P := OnDone_P;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64;
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnDone_C: TUserDownloadDoneNotify_C);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDone_C := OnDone_C;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64;
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnDone_M: TUserDownloadDoneNotify_M);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDone_M := OnDone_M;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64;
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnDone_P: TUserDownloadDoneNotify_P);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDone_P := OnDone_P;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnDone_C: TUserDownloadDoneNotify_C);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDone_C := OnDone_C;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnDone_M: TUserDownloadDoneNotify_M);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDone_M := OnDone_M;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnDone_P: TUserDownloadDoneNotify_P);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDone_P := OnDone_P;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.RequestFastDownloadAssembleStream(dataBaseName_: SystemString; StorePos: Int64; BackcallPtr: PDataStoreClientDownloadNotify);
var
  de: TDFE;
begin
  de := TDFE.Create;

  de.WriteString(dataBaseName_);
  de.WriteInt64(StorePos);
  de.WritePointer(BackcallPtr);

  SendTunnel.SendDirectStreamCmd(C_RequestFastDownloadAssembleStrea, de);

  DisposeObject(de);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; OnDone_C: TDownloadDoneNotify_C);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_C := OnDone_C;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; OnDone_M: TDownloadDoneNotify_M);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_M := OnDone_M;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; OnDone_P: TDownloadDoneNotify_P);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_P := OnDone_P;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDone_C: TDownloadDoneNotify_C);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_C := OnDone_C;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDone_M: TDownloadDoneNotify_M);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_M := OnDone_M;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDone_P: TDownloadDoneNotify_P);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDone_P := OnDone_P;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64;
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnDone_C: TUserDownloadDoneNotify_C);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDone_C := OnDone_C;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64;
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnDone_M: TUserDownloadDoneNotify_M);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDone_M := OnDone_M;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64;
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnDone_P: TUserDownloadDoneNotify_P);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDone_P := OnDone_P;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnDone_C: TUserDownloadDoneNotify_C);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDone_C := OnDone_C;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnDone_M: TUserDownloadDoneNotify_M);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDone_M := OnDone_M;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
  UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant; { local event parameter }
  OnDone_P: TUserDownloadDoneNotify_P);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDone_P := OnDone_P;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.PostAssembleStream(dataBaseName_: SystemString; stream: TMS64; dID: Cardinal; DoneTimeFree: Boolean);
var
  de: TDFE;
begin
  if stream.Size = 0 then
    begin
      SendTunnel.Error('PostAssembleStream Error: stream size is 0');
      exit;
    end;
  EncryptBuffer(stream.Memory, stream.Size, True);
  PostBatchStream(stream, DoneTimeFree);

  de := TDFE.Create;
  de.WriteString(dataBaseName_);
  de.WriteCardinal(dID);
  SendTunnel.SendDirectStreamCmd(C_CompletedPostAssembleStream, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.PostAssembleStreamCopy(dataBaseName_: SystemString; stream: TCore_Stream; dID: Cardinal);
var
  M: TMS64;
begin
  M := TMS64.Create;
  stream.Position := 0;
  M.CopyFrom(stream, stream.Size);
  M.Position := 0;
  PostAssembleStream(dataBaseName_, M, dID, True);
end;

procedure TDataStoreClient.PostAssembleStream(dataBaseName_: SystemString; DataSource: TDFE);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.EncodeTo(M, True);
  PostAssembleStream(dataBaseName_, M, c_DF, True);
end;

procedure TDataStoreClient.PostAssembleStream(dataBaseName_: SystemString; DataSource: THashVariantList);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  PostAssembleStream(dataBaseName_, M, c_VL, True);
end;

procedure TDataStoreClient.PostAssembleStream(dataBaseName_: SystemString; DataSource: THashStringList);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  PostAssembleStream(dataBaseName_, M, c_VT, True);
end;

procedure TDataStoreClient.PostAssembleStream(dataBaseName_: SystemString; DataSource: TSectionTextData);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  PostAssembleStream(dataBaseName_, M, c_TE, True);
end;

procedure TDataStoreClient.PostAssembleStream(dataBaseName_: SystemString; DataSource: TZ_JsonObject);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  PostAssembleStream(dataBaseName_, M, c_Json, True);
end;

procedure TDataStoreClient.PostAssembleStream(dataBaseName_: SystemString; DataSource: TPascalString);
var
  M: TMS64;
begin
  M := TMS64.Create;
  TDBEnginePascalString.SavePascalStringToStream(@DataSource, M);
  PostAssembleStream(dataBaseName_, M, c_PascalString, True);
end;

procedure TDataStoreClient.InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64; dID: Cardinal; DoneTimeFree: Boolean);
var
  de: TDFE;
begin
  if stream.Size = 0 then
    begin
      SendTunnel.Error('InsertAssembleStream Error: stream size is 0');
      exit;
    end;
  EncryptBuffer(stream.Memory, stream.Size, True);
  PostBatchStream(stream, DoneTimeFree);

  de := TDFE.Create;
  de.WriteString(dataBaseName_);
  de.WriteInt64(dStorePos);
  de.WriteCardinal(dID);
  SendTunnel.SendDirectStreamCmd(C_CompletedInsertAssembleStream, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.InsertAssembleStreamCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCore_Stream; dID: Cardinal);
var
  M: TMS64;
begin
  M := TMS64.Create;
  stream.Position := 0;
  M.CopyFrom(stream, stream.Size);
  M.Position := 0;
  InsertAssembleStream(dataBaseName_, dStorePos, M, dID, True);
end;

procedure TDataStoreClient.InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDFE);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.EncodeTo(M, True);
  InsertAssembleStream(dataBaseName_, dStorePos, M, c_DF, True);
end;

procedure TDataStoreClient.InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  InsertAssembleStream(dataBaseName_, dStorePos, M, c_VL, True);
end;

procedure TDataStoreClient.InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  InsertAssembleStream(dataBaseName_, dStorePos, M, c_VT, True);
end;

procedure TDataStoreClient.InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  InsertAssembleStream(dataBaseName_, dStorePos, M, c_TE, True);
end;

procedure TDataStoreClient.InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TZ_JsonObject);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M, False);
  InsertAssembleStream(dataBaseName_, dStorePos, M, c_Json, True);
end;

procedure TDataStoreClient.InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString);
var
  M: TMS64;
begin
  M := TMS64.Create;
  TDBEnginePascalString.SavePascalStringToStream(@DataSource, M);
  InsertAssembleStream(dataBaseName_, dStorePos, M, c_PascalString, True);
end;

procedure TDataStoreClient.ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64; DoneTimeFree: Boolean);
var
  de: TDFE;
begin
  if stream.Size = 0 then
    begin
      SendTunnel.Error('ModifyAssembleStream Error: stream size is 0');
      exit;
    end;
  EncryptBuffer(stream.Memory, stream.Size, True);
  PostBatchStream(stream, DoneTimeFree);

  de := TDFE.Create;
  de.WriteString(dataBaseName_);
  de.WriteInt64(dStorePos);
  SendTunnel.SendDirectStreamCmd(C_CompletedModifyAssembleStream, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.ModifyAssembleStreamCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCore_Stream);
var
  M: TMS64;
begin
  M := TMS64.Create;
  stream.Position := 0;
  M.CopyFrom(stream, stream.Size);
  M.Position := 0;
  ModifyAssembleStream(dataBaseName_, dStorePos, M, True);
end;

procedure TDataStoreClient.ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDFE);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.EncodeTo(M, True);
  ModifyAssembleStream(dataBaseName_, dStorePos, M, True);
end;

procedure TDataStoreClient.ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  ModifyAssembleStream(dataBaseName_, dStorePos, M, True);
end;

procedure TDataStoreClient.ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  ModifyAssembleStream(dataBaseName_, dStorePos, M, True);
end;

procedure TDataStoreClient.ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  ModifyAssembleStream(dataBaseName_, dStorePos, M, True);
end;

procedure TDataStoreClient.ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TZ_JsonObject);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M, False);
  ModifyAssembleStream(dataBaseName_, dStorePos, M, True);
end;

procedure TDataStoreClient.ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString);
var
  M: TMS64;
begin
  M := TMS64.Create;
  TDBEnginePascalString.SavePascalStringToStream(@DataSource, M);
  ModifyAssembleStream(dataBaseName_, dStorePos, M, True);
end;

procedure TDataStoreClient.GetPostAssembleStreamStateM(OnResult: TOnStream_M);
begin
  GetBatchStreamStateM(OnResult);
end;

procedure TDataStoreClient.GetPostAssembleStreamStateM(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_M);
begin
  GetBatchStreamStateM(Param1, Param2, OnResult);
end;

procedure TDataStoreClient.GetPostAssembleStreamStateP(OnResult: TOnStream_P);
begin
  GetBatchStreamStateP(OnResult);
end;

procedure TDataStoreClient.GetPostAssembleStreamStateP(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_P);
begin
  GetBatchStreamStateP(Param1, Param2, OnResult);
end;

procedure TDataStoreClient.EndAssembleStream;
begin
  ClearBatchStream;
end;

procedure TDataStoreClient.DeleteData(dataBaseName_: SystemString; dStorePos: Int64);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(dataBaseName_);
  de.WriteInt64(dStorePos);
  SendTunnel.SendDirectStreamCmd(C_DeleteData, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.FastPostCompleteBuffer(dataBaseName_: SystemString; stream: TMS64; dID: Cardinal; DoneTimeFree: Boolean);
var
  p: Pointer;
  siz: nativeUInt;
begin
  if stream.Size = 0 then
    begin
      SendTunnel.Error('FastPostCompleteBuffer Error: stream size is 0');
      exit;
    end;
  p := EncodeZDBBuff(dataBaseName_, dID, 0, stream.Memory, stream.Size, siz);
  SendTunnel.SendCompleteBuffer(C_FastPostCompleteBuffer, p, siz, True);

  if DoneTimeFree then
      DisposeObject(stream);
end;

procedure TDataStoreClient.FastPostCompleteBufferCopy(dataBaseName_: SystemString; stream: TCore_Stream; dID: Cardinal);
var
  M: TMS64;
begin
  M := TMS64.Create;
  stream.Position := 0;
  M.CopyFrom(stream, stream.Size);
  M.Position := 0;
  FastPostCompleteBuffer(dataBaseName_, M, dID, True);
end;

procedure TDataStoreClient.FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TDFE);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.EncodeTo(M, True);
  FastPostCompleteBuffer(dataBaseName_, M, c_DF, True);
end;

procedure TDataStoreClient.FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: THashVariantList);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  FastPostCompleteBuffer(dataBaseName_, M, c_VL, True);
end;

procedure TDataStoreClient.FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: THashStringList);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  FastPostCompleteBuffer(dataBaseName_, M, c_VT, True);
end;

procedure TDataStoreClient.FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TSectionTextData);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  FastPostCompleteBuffer(dataBaseName_, M, c_TE, True);
end;

procedure TDataStoreClient.FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TZ_JsonObject);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  FastPostCompleteBuffer(dataBaseName_, M, c_Json, True);
end;

procedure TDataStoreClient.FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TPascalString);
var
  M: TMS64;
begin
  M := TMS64.Create;
  TDBEnginePascalString.SavePascalStringToStream(@DataSource, M);
  FastPostCompleteBuffer(dataBaseName_, M, c_PascalString, True);
end;

procedure TDataStoreClient.FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64; dID: Cardinal; DoneTimeFree: Boolean);
var
  p: Pointer;
  siz: nativeUInt;
begin
  if stream.Size = 0 then
    begin
      SendTunnel.Error('FastInsertCompleteBuffer Error: stream size is 0');
      exit;
    end;
  p := EncodeZDBBuff(dataBaseName_, dID, dStorePos, stream.Memory, stream.Size, siz);
  SendTunnel.SendCompleteBuffer(C_FastInsertCompleteBuffer, p, siz, True);

  if DoneTimeFree then
      DisposeObject(stream);
end;

procedure TDataStoreClient.FastInsertCompleteBufferCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCore_Stream; dID: Cardinal);
var
  M: TMS64;
begin
  M := TMS64.Create;
  stream.Position := 0;
  M.CopyFrom(stream, stream.Size);
  M.Position := 0;
  FastInsertCompleteBuffer(dataBaseName_, dStorePos, M, dID, True);
end;

procedure TDataStoreClient.FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDFE);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.EncodeTo(M, True);
  FastInsertCompleteBuffer(dataBaseName_, dStorePos, M, c_DF, True);
end;

procedure TDataStoreClient.FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  FastInsertCompleteBuffer(dataBaseName_, dStorePos, M, c_VL, True);
end;

procedure TDataStoreClient.FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  FastInsertCompleteBuffer(dataBaseName_, dStorePos, M, c_VT, True);
end;

procedure TDataStoreClient.FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  FastInsertCompleteBuffer(dataBaseName_, dStorePos, M, c_TE, True);
end;

procedure TDataStoreClient.FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TZ_JsonObject);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M, False);
  FastInsertCompleteBuffer(dataBaseName_, dStorePos, M, c_Json, True);
end;

procedure TDataStoreClient.FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString);
var
  M: TMS64;
begin
  M := TMS64.Create;
  TDBEnginePascalString.SavePascalStringToStream(@DataSource, M);
  FastInsertCompleteBuffer(dataBaseName_, dStorePos, M, c_PascalString, True);
end;

procedure TDataStoreClient.FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64; dID: Cardinal; DoneTimeFree: Boolean);
var
  p: Pointer;
  siz: nativeUInt;
begin
  if stream.Size = 0 then
    begin
      SendTunnel.Error('FastModifyCompleteBuffer Error: stream size is 0');
      exit;
    end;
  p := EncodeZDBBuff(dataBaseName_, dID, dStorePos, stream.Memory, stream.Size, siz);
  SendTunnel.SendCompleteBuffer(C_FastModifyCompleteBuffer, p, siz, True);

  if DoneTimeFree then
      DisposeObject(stream);
end;

procedure TDataStoreClient.FastModifyCompleteBufferCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCore_Stream; dID: Cardinal);
var
  M: TMS64;
begin
  M := TMS64.Create;
  stream.Position := 0;
  M.CopyFrom(stream, stream.Size);
  M.Position := 0;
  FastModifyCompleteBuffer(dataBaseName_, dStorePos, M, dID, True);
end;

procedure TDataStoreClient.FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDFE);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.EncodeTo(M, True);
  FastModifyCompleteBuffer(dataBaseName_, dStorePos, M, c_DF, True);
end;

procedure TDataStoreClient.FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  FastModifyCompleteBuffer(dataBaseName_, dStorePos, M, c_VL, True);
end;

procedure TDataStoreClient.FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  FastModifyCompleteBuffer(dataBaseName_, dStorePos, M, c_VT, True);
end;

procedure TDataStoreClient.FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M);
  FastModifyCompleteBuffer(dataBaseName_, dStorePos, M, c_TE, True);
end;

procedure TDataStoreClient.FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TZ_JsonObject);
var
  M: TMS64;
begin
  M := TMS64.Create;
  DataSource.SaveToStream(M, False);
  FastModifyCompleteBuffer(dataBaseName_, dStorePos, M, c_Json, True);
end;

procedure TDataStoreClient.FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString);
var
  M: TMS64;
begin
  M := TMS64.Create;
  TDBEnginePascalString.SavePascalStringToStream(@DataSource, M);
  FastModifyCompleteBuffer(dataBaseName_, dStorePos, M, c_PascalString, True);
end;

procedure TDataStoreClient.QueryStop(PipeName_: SystemString);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(PipeName_);
  SendTunnel.SendDirectStreamCmd(C_QueryStop, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.QueryPause(PipeName_: SystemString);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(PipeName_);
  SendTunnel.SendDirectStreamCmd(C_QueryPause, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.QueryPlay(PipeName_: SystemString);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(PipeName_);
  SendTunnel.SendDirectStreamCmd(C_QueryPlay, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetDBListM(OnResult: TOnStream_M);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdM(C_GetDBList, de, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetDBListM(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_M);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdM(C_GetDBList, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryListM(OnResult: TOnStream_M);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdM(C_GetQueryList, de, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryListM(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_M);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdM(C_GetQueryList, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryStateM(PipeName_: SystemString; OnResult: TOnStream_M);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(PipeName_);
  SendTunnel.SendStreamCmdM(C_GetQueryState, de, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryStateM(PipeName_: SystemString; Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_M);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(PipeName_);
  SendTunnel.SendStreamCmdM(C_GetQueryState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetDBListP(OnResult: TOnStream_P);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdP(C_GetDBList, de, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetDBListP(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_P);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdP(C_GetDBList, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryListP(OnResult: TOnStream_P);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdP(C_GetQueryList, de, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryListP(Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_P);
var
  de: TDFE;
begin
  de := TDFE.Create;
  SendTunnel.SendStreamCmdP(C_GetQueryList, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryStateP(PipeName_: SystemString; OnResult: TOnStream_P);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(PipeName_);
  SendTunnel.SendStreamCmdP(C_GetQueryState, de, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryStateP(PipeName_: SystemString; Param1: Pointer; Param2: TObject; OnResult: TOnStreamParam_P);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteString(PipeName_);
  SendTunnel.SendStreamCmdP(C_GetQueryState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

end.
