{ ****************************************************************************** }
{ * DataStore Service                                                          * }
{ ****************************************************************************** }
unit Z.Net.DataStoreService.Common;

{$DEFINE FPC_DELPHI_MODE}
{$I ..\Z.Define.inc}

interface

uses Variants, Z.Core, Z.Net, Z.PascalStrings, Z.UPascalStrings, Z.ZDB.Engine, Z.ZDB.LocalManager, Z.MemoryStream,
  Z.DFE;

type
  TTDataStoreService_DBPipeline = class(TZDBPipeline)
  public
    SendTunnel: TPeerClientUserDefine;
    RecvTunnel: TPeerClientUserDefine;
    BackcallPtr: UInt64;
    SyncToClient: Boolean;
    RegistedQuery: SystemString;

    constructor Create(InMem: Boolean; Owner_: TZDBLocalManager; sourDBName, APipelineN, OutDBName: SystemString); override;
    destructor Destroy; override;

    procedure Progress(deltaTime: Double); override;
  end;

  TTDataStoreService_Query_C = class(TCore_Object)
  private
  public
    OnPipelineQuery: TZDBPipelineFilter_M;
    OnPipelineQueryDone: TZDBPipelineDone_M;
    constructor Create;
  end;

  TUserQueryDoneNotify_C = procedure(UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant;
    dataBaseName_, outN, pipeN: SystemString; TotalResult: Int64);
  TUserQueryDoneNotify_M = procedure(UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant;
    dataBaseName_, outN, pipeN: SystemString; TotalResult: Int64) of object;
  TQueryDoneNotify_C = procedure(dataBaseName_, outN, pipeN: SystemString; TotalResult: Int64);
  TQueryDoneNotify_M = procedure(dataBaseName_, outN, pipeN: SystemString; TotalResult: Int64) of object;

{$IFDEF FPC}
  TUserQueryDoneNotify_P = procedure(UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant;
    dataBaseName_, outN, pipeN: SystemString; TotalResult: Int64) is nested;
  TQueryDoneNotify_P = procedure(dataBaseName_, outN, pipeN: SystemString; TotalResult: Int64) is nested;
{$ELSE FPC}
  TUserQueryDoneNotify_P = reference to procedure(UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant;
    dataBaseName_, outN, pipeN: SystemString; TotalResult: Int64);
  TQueryDoneNotify_P = reference to procedure(dataBaseName_, outN, pipeN: SystemString; TotalResult: Int64);
{$ENDIF FPC}
  PDataStoreClientQueryNotify = ^TDataStoreClientQueryNotify;

  TDataStoreClientQueryNotify = record
    UserPointer: Pointer;
    UserObject: TCore_Object;
    UserVariant: Variant;
    OnQuery_C: TFillQueryData_C;
    OnQuery_M: TFillQueryData_M;
    OnQuery_P: TFillQueryData_P;
    OnUserQuery_C: TUserFillQueryData_C;
    OnUserQuery_M: TUserFillQueryData_M;
    OnUserQuery_P: TUserFillQueryData_P;
    OnDone_C: TQueryDoneNotify_C;
    OnDone_M: TQueryDoneNotify_M;
    OnDone_P: TQueryDoneNotify_P;
    OnUserDone_C: TUserQueryDoneNotify_C;
    OnUserDone_M: TUserQueryDoneNotify_M;
    OnUserDone_P: TUserQueryDoneNotify_P;
    procedure Init;
  end;

  TUserDownloadDoneNotify_C = procedure(UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant;
    dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64);
  TUserDownloadDoneNotify_M = procedure(UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant;
    dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64) of object;
  TDownloadDoneNotify_C = procedure(dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64);
  TDownloadDoneNotify_M = procedure(dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64) of object;

{$IFDEF FPC}
  TUserDownloadDoneNotify_P = procedure(UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant;
    dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64) is nested;
  TDownloadDoneNotify_P = procedure(dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64) is nested;
{$ELSE FPC}
  TUserDownloadDoneNotify_P = reference to procedure(UserPointer: Pointer; UserObject: TCore_Object; UserVariant: Variant;
    dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64);
  TDownloadDoneNotify_P = reference to procedure(dataBaseName_: SystemString; dStorePos: Int64; stream: TMS64);
{$ENDIF FPC}
  PDataStoreClientDownloadNotify = ^TDataStoreClientDownloadNotify;

  TDataStoreClientDownloadNotify = record
    UserPointer: Pointer;
    UserObject: TCore_Object;
    UserVariant: Variant;
    AutoDecodeZDBStream: Boolean;
    OnUserDone_C: TUserDownloadDoneNotify_C;
    OnUserDone_M: TUserDownloadDoneNotify_M;
    OnUserDone_P: TUserDownloadDoneNotify_P;
    OnDone_C: TDownloadDoneNotify_C;
    OnDone_M: TDownloadDoneNotify_M;
    OnDone_P: TDownloadDoneNotify_P;
    procedure Init;
  end;

  { client storePos transform }
  TStorePosTransformNotify_C = procedure(const TransformBuff: PZDBStorePosTransformArray);
  TStorePosTransformNotify_M = procedure(const TransformBuff: PZDBStorePosTransformArray) of object;
{$IFDEF FPC}
  TStorePosTransformNotify_P = procedure(const TransformBuff: PZDBStorePosTransformArray) is nested;
{$ELSE FPC}
  TStorePosTransformNotify_P = reference to procedure(const TransformBuff: PZDBStorePosTransformArray);
{$ENDIF FPC}
  PStorePosTransformNotify = ^TStorePosTransformNotify;

  TStorePosTransformNotify = record
    OnDone_C: TStorePosTransformNotify_C;
    OnDone_M: TStorePosTransformNotify_M;
    OnDone_P: TStorePosTransformNotify_P;
    procedure Init;
  end;

  TPipeState = record
    WriteOutputDB, Activted, SyncToClient, MemoryMode, Paused: Boolean;
    DBCounter, QueryCounter, QueryResultCounter, MaxQueryCompare, MaxQueryResult: Int64;
    QueryPerformanceOfPerSec, ConsumTime, MaxWaitTime: Double;
    SourceDB, OutputDB, PipelineName, RegistedQuery: SystemString;

    procedure Init;
    procedure Encode(d: TDFE);
    procedure Decode(d: TDFE);
  end;

  TPipeInfo = TPipeState;
  PPipeInfo = ^TPipeInfo;

implementation

constructor TTDataStoreService_DBPipeline.Create(InMem: Boolean; Owner_: TZDBLocalManager; sourDBName, APipelineN, OutDBName: SystemString);
begin
  inherited Create(InMem, Owner_, sourDBName, APipelineN, OutDBName);
  SendTunnel := nil;
  RecvTunnel := nil;
  BackcallPtr := 0;
  SyncToClient := False;
  RegistedQuery := '';
end;

destructor TTDataStoreService_DBPipeline.Destroy;
begin
  inherited Destroy;
end;

procedure TTDataStoreService_DBPipeline.Progress(deltaTime: Double);
begin
  inherited Progress(deltaTime);
end;

constructor TTDataStoreService_Query_C.Create;
begin
  inherited Create;
  OnPipelineQuery := nil;
  OnPipelineQueryDone := nil;
end;

procedure TDataStoreClientQueryNotify.Init;
begin
  UserPointer := nil;
  UserObject := nil;
  UserVariant := Null;

  OnQuery_C := nil;
  OnQuery_M := nil;
  OnQuery_P := nil;

  OnUserQuery_C := nil;
  OnUserQuery_M := nil;
  OnUserQuery_P := nil;

  OnDone_C := nil;
  OnDone_M := nil;
  OnDone_P := nil;

  OnUserDone_C := nil;
  OnUserDone_M := nil;
  OnUserDone_P := nil;
end;

procedure TStorePosTransformNotify.Init;
begin
  OnDone_C := nil;
  OnDone_M := nil;
  OnDone_P := nil;
end;

procedure TDataStoreClientDownloadNotify.Init;
begin
  UserPointer := nil;
  UserObject := nil;
  UserVariant := Null;
  AutoDecodeZDBStream := False;

  OnDone_C := nil;
  OnDone_M := nil;
  OnDone_P := nil;

  OnUserDone_C := nil;
  OnUserDone_M := nil;
  OnUserDone_P := nil;
end;

procedure TPipeState.Init;
begin
  WriteOutputDB := False;
  Activted := False;
  SyncToClient := False;
  MemoryMode := False;
  Paused := False;
  DBCounter := 0;
  QueryCounter := 0;
  QueryResultCounter := 0;
  MaxQueryCompare := 0;
  MaxQueryResult := 0;
  QueryPerformanceOfPerSec := 0;
  ConsumTime := 0;
  MaxWaitTime := 0;
  SourceDB := '';
  OutputDB := '';
  PipelineName := '';
  RegistedQuery := '';
end;

procedure TPipeState.Encode(d: TDFE);
begin
  d.WriteBool(WriteOutputDB);
  d.WriteBool(Activted);
  d.WriteBool(SyncToClient);
  d.WriteBool(MemoryMode);
  d.WriteBool(Paused);
  d.WriteInt64(DBCounter);
  d.WriteInt64(QueryCounter);
  d.WriteInt64(QueryResultCounter);
  d.WriteInt64(MaxQueryCompare);
  d.WriteInt64(MaxQueryResult);
  d.WriteDouble(QueryPerformanceOfPerSec);
  d.WriteDouble(ConsumTime);
  d.WriteDouble(MaxWaitTime);
  d.WriteString(SourceDB);
  d.WriteString(OutputDB);
  d.WriteString(PipelineName);
  d.WriteString(RegistedQuery);
end;

procedure TPipeState.Decode(d: TDFE);
begin
  Init;
  WriteOutputDB := d.Reader.ReadBool;
  Activted := d.Reader.ReadBool;
  SyncToClient := d.Reader.ReadBool;
  MemoryMode := d.Reader.ReadBool;
  Paused := d.Reader.ReadBool;
  DBCounter := d.Reader.ReadInt64;
  QueryCounter := d.Reader.ReadInt64;
  QueryResultCounter := d.Reader.ReadInt64;
  MaxQueryCompare := d.Reader.ReadInt64;
  MaxQueryResult := d.Reader.ReadInt64;
  QueryPerformanceOfPerSec := d.Reader.ReadDouble;
  ConsumTime := d.Reader.ReadDouble;
  MaxWaitTime := d.Reader.ReadDouble;
  SourceDB := d.Reader.ReadString;
  OutputDB := d.Reader.ReadString;
  PipelineName := d.Reader.ReadString;
  RegistedQuery := d.Reader.ReadString;
end;

initialization

end.
