{ ****************************************************************************** }
{ * cloud 4.0 Unit Rewrite Tool                                                * }
{ ****************************************************************************** }
unit Z.Net.C4_PascalRewrite_Service;

{$I Z.Define.inc}

interface

uses Variants, SysUtils,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib,
  Z.Geometry2D, Z.DFE, Z.ListEngine,
  Z.Parsing, Z.Pascal_Code_Tool, Z.Expression, Z.OpCode,
  Z.Json, Z.GHashList,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth, Z.Net.C4,
  Z.ZDB2, Z.ZDB2.FileEncoder,
  Z.Pascal_Rewrite_Model_Data;

type
  TUnitRewriteService_IO_Define_ = class(TPeerClientUserDefineForRecvTunnel_NoAuth)
  public
    UnitRewriteProcessor: TUProcessorDataList;
    SymbolRewriteProcessor: TUProcessorDataList;
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TC40_Pascal_Rewrite_Service = class(TC40_Base_NoAuth_Service)
  protected
    procedure cmd_SetDefaultModel(Sender: TPeerIO; InData: TDFE);
    procedure cmd_UpdateModel(Sender: TPeerIO; InData: TDFE);

    procedure Do_Sync_Rewrite_Status(Sender: TNPostExecute);
    procedure Do_RewritePascal_HPC(ThSender: THPC_Stream; ThInData, ThOutData: TDFE);
    procedure cmd_RewritePascal(Sender: TPeerIO; InData, OutData: TDFE);
  public
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
    function Get_UnitRewriteService_IO_Define_(IO_: TPeerIO): TUnitRewriteService_IO_Define_;
  end;

implementation

constructor TUnitRewriteService_IO_Define_.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  UnitRewriteProcessor := TUProcessorDataList.Create;
  SymbolRewriteProcessor := TUProcessorDataList.Create;
end;

destructor TUnitRewriteService_IO_Define_.Destroy;
begin
  DisposeObject(UnitRewriteProcessor);
  DisposeObject(SymbolRewriteProcessor);
  inherited Destroy;
end;

procedure TC40_Pascal_Rewrite_Service.cmd_SetDefaultModel(Sender: TPeerIO; InData: TDFE);
var
  IO_Def: TUnitRewriteService_IO_Define_;
  dec: TZDB2_File_Decoder;
  fi: TZDB2_FI;
  stream_, m64: TMS64;
begin
  IO_Def := Get_UnitRewriteService_IO_Define_(Sender);
  try
    stream_ := TMS64.Create;
    Get_PassByYou888UpLevelModel_Stream(stream_);

    if TZDB2_File_Decoder.Check(stream_) then
      begin
        dec := TZDB2_File_Decoder.Create(stream_, 1);

        fi := dec.Files.FindFile('Unit');
        if fi <> nil then
          begin
            m64 := TMS64.Create;
            dec.DecodeToStream(fi, m64);
            m64.Position := 0;
            IO_Def.UnitRewriteProcessor.LoadFromStream(m64);
            DisposeObject(m64);
            if IO_Def.LinkOk then
                IO_Def.SendTunnel.Owner.SendDirectConsoleCmd('Rewrite_Status', 'Open Unit Rewrite Model.');
          end;

        fi := dec.Files.FindFile('Pattern');
        if fi <> nil then
          begin
            m64 := TMS64.Create;
            dec.DecodeToStream(fi, m64);
            m64.Position := 0;
            IO_Def.SymbolRewriteProcessor.LoadFromStream(m64);
            DisposeObject(m64);
            if IO_Def.LinkOk then
                IO_Def.SendTunnel.Owner.SendDirectConsoleCmd('Rewrite_Status', 'Open Symbol Rewrite Model.');
          end;

        DisposeObject(dec);
      end;
    DisposeObject(stream_);
  except
  end;
end;

procedure TC40_Pascal_Rewrite_Service.cmd_UpdateModel(Sender: TPeerIO; InData: TDFE);
var
  IO_Def: TUnitRewriteService_IO_Define_;
  m64: TMS64;
begin
  IO_Def := Get_UnitRewriteService_IO_Define_(Sender);
  m64 := TMS64.Create;
  try
    InData.R.ReadStream(m64);
    m64.Position := 0;
    IO_Def.UnitRewriteProcessor.LoadFromStream(m64);
    DisposeObject(m64);

    InData.R.ReadStream(m64);
    m64.Position := 0;
    IO_Def.SymbolRewriteProcessor.LoadFromStream(m64);
    DisposeObject(m64);
  except
  end;
end;

procedure TC40_Pascal_Rewrite_Service.Do_Sync_Rewrite_Status(Sender: TNPostExecute);
var
  id: Cardinal;
  info: SystemString;
  IO_Def: TUnitRewriteService_IO_Define_;
begin
  id := Sender.Data3;
  info := Sender.Data4;
  IO_Def := Get_UnitRewriteService_IO_Define_(DTNoAuth.RecvTunnel[id]);
  if IO_Def = nil then
      exit;
  if not IO_Def.LinkOk then
      exit;
  IO_Def.SendTunnel.Owner.SendDirectConsoleCmd('Rewrite_Status', info);
end;

procedure TC40_Pascal_Rewrite_Service.Do_RewritePascal_HPC(ThSender: THPC_Stream; ThInData, ThOutData: TDFE);
var
  IO_Def: TUnitRewriteService_IO_Define_;
  uHash, symHash: THashStringList;
  Code: TCore_StringList;
  fn: U_String;
  Current_Status: TPascalStringList;
{$IFDEF FPC}
  procedure Do_Sync_IO_Def;
  begin
    IO_Def := Get_UnitRewriteService_IO_Define_(ThSender.Framework.IOPool[ThSender.WorkID] as TPeerIO);
    if IO_Def = nil then
        exit;
    uHash := THashStringList.CustomCreate($FFFF);
    IO_Def.UnitRewriteProcessor.Build_uHash(uHash);
    symHash := THashStringList.CustomCreate($FFFF);
    IO_Def.SymbolRewriteProcessor.Build_uHash(symHash);
  end;

  procedure fpc_rewrite_status(const Fmt: SystemString; const Args: array of const);
  begin
    Current_Status.Add(TimeToStr(Now) + ' ' + PFormat(Fmt, Args));
    with DTNoAuth.PostProgress.PostExecuteM(0, @Do_Sync_Rewrite_Status) do
      begin
        Data3 := ThSender.id;
        Data4 := PFormat(Fmt, Args);
      end;
  end;
{$ENDIF FPC}


begin
  IO_Def := nil;
{$IFDEF FPC}
  TCompute.Sync(@Do_Sync_IO_Def);
{$ELSE FPC}
  TCompute.Sync(procedure
    begin
      IO_Def := Get_UnitRewriteService_IO_Define_(ThSender.Framework.IOPool[ThSender.WorkID] as TPeerIO);
      if IO_Def = nil then
          exit;
      uHash := THashStringList.CustomCreate($FFFF);
      IO_Def.UnitRewriteProcessor.Build_uHash(uHash);
      symHash := THashStringList.CustomCreate($FFFF);
      IO_Def.SymbolRewriteProcessor.Build_uHash(symHash);
      Current_Status := TPascalStringList.Create;
    end);
{$ENDIF FPC}
  if IO_Def = nil then
      exit;
  while ThInData.R.NotEnd do
    begin
      Code := TCore_StringList.Create;
      try
        fn := ThInData.R.ReadString;
        ThInData.R.ReadStrings(Code);
{$IFDEF FPC}
        RewritePascal_Process_Code(Code, uHash, symHash, @fpc_rewrite_status);
{$ELSE FPC}
        RewritePascal_Process_Code(Code, uHash, symHash, procedure(const Fmt: SystemString; const Args: array of const)
          begin
            Current_Status.Add(TimeToStr(Now) + ' ' + PFormat(Fmt, Args));
            with DTNoAuth.PostProgress.PostExecuteM(0, Do_Sync_Rewrite_Status) do
              begin
                Data3 := ThSender.id;
                Data4 := PFormat(Fmt, Args);
              end;
          end);
{$ENDIF FPC}
        Current_Status.Add(TimeToStr(Now) + ' ' + PFormat('%s rewrite done.', [umlGetFileName(fn).Text]));
        with DTNoAuth.PostProgress.PostExecuteM(0, {$IFDEF FPC}@{$ENDIF FPC}Do_Sync_Rewrite_Status) do
          begin
            Data3 := ThSender.id;
            Data4 := PFormat('%s rewrite done.', [umlGetFileName(fn).Text]);
          end;
        ThOutData.WriteString(fn);
        ThOutData.WritePascalStrings(Current_Status);
        ThOutData.WriteStrings(Code);
        Current_Status.Clear;
      except
      end;
      DisposeObject(Code);
    end;
  DisposeObject(uHash);
  DisposeObject(symHash);
  DisposeObject(Current_Status);
end;

procedure TC40_Pascal_Rewrite_Service.cmd_RewritePascal(Sender: TPeerIO; InData, OutData: TDFE);
begin
  RunHPC_StreamM(Sender, nil, nil, InData, OutData, {$IFDEF FPC}@{$ENDIF FPC}Do_RewritePascal_HPC);
end;

constructor TC40_Pascal_Rewrite_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  DTNoAuth.RecvTunnel.PeerClientUserDefineClass := TUnitRewriteService_IO_Define_;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('SetDefaultModel').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_SetDefaultModel;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('UpdateModel').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_UpdateModel;
  DTNoAuthService.RecvTunnel.RegisterStream('RewritePascal').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RewritePascal;
  ServiceInfo.OnlyInstance := False;
  UpdateToGlobalDispatch;
end;

destructor TC40_Pascal_Rewrite_Service.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_Pascal_Rewrite_Service.SafeCheck;
begin
  inherited SafeCheck;
end;

procedure TC40_Pascal_Rewrite_Service.Progress;
begin
  inherited Progress;
end;

function TC40_Pascal_Rewrite_Service.Get_UnitRewriteService_IO_Define_(IO_: TPeerIO): TUnitRewriteService_IO_Define_;
begin
  if IO_ <> nil then
      Result := TUnitRewriteService_IO_Define_(IO_.IODefine)
  else
      Result := nil;
end;

initialization

RegisterC40('Pascal_Rewrite', TC40_Pascal_Rewrite_Service, nil);

end.
