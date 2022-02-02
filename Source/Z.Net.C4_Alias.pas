{ ****************************************************************************** }
{ * cloud 4.0 alias service                                                    * }
{ ****************************************************************************** }
unit Z.Net.C4_Alias;

{$I Z.Define.inc}

interface

uses Variants,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib,
  Z.Geometry2D, Z.DFE, Z.ListEngine,
  Z.Parsing, Z.Expression, Z.OpCode,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth, Z.Net.C4,
  Z.ZDB2.HS, Z.ZDB2;

type
  TC40_Alias_Service = class(TC40_Base_NoAuth_Service)
  protected
    // command
    procedure cmd_SetAlias(Sender: TPeerIO; InData: TDFE);
    procedure cmd_GetAlias(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_RemoveAlias(Sender: TPeerIO; InData: TDFE);
    // admin
    procedure cmd_SearchAlias(Sender: TPeerIO; InData, OutData: TDFE);
  public
    ZDB2RecycleMemoryTimeOut: TTimeTick;
    ZDB2DeltaSpace: Int64;
    ZDB2BlockSize: Word;
    ZDB2EnabledCipher: Boolean;
    ZDB2CipherName: U_String;
    ZDB2Password: U_String;
    ZDB2Cipher: TZDB2_Cipher;
    C40_Alias_DB_FileName: U_String;
    Alias_DB: TZDB2_List_HashString;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
  end;

  TC40_Alias_Client = class;

  TON_GetAliasC = procedure(Sender: TC40_Alias_Client; NameKey_: THashStringList);
  TON_GetAliasM = procedure(Sender: TC40_Alias_Client; NameKey_: THashStringList) of object;
{$IFDEF FPC}
  TON_GetAliasP = procedure(Sender: TC40_Alias_Client; NameKey_: THashStringList) is nested;
{$ELSE FPC}
  TON_GetAliasP = reference to procedure(Sender: TC40_Alias_Client; NameKey_: THashStringList);
{$ENDIF FPC}

  TON_Temp_GetAliasC = class(TOnResultBridge)
  public
    Client: TC40_Alias_Client;
    OnResultC: TON_GetAliasC;
    OnResultM: TON_GetAliasM;
    OnResultP: TON_GetAliasP;
    constructor Create; override;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_Alias_Client = class(TC40_Base_NoAuth_Client)
  public
    constructor Create(source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure SetAlias(Name_, Hash_: U_String); overload;
    procedure SetAlias(Name_: U_String; Hash_: TMD5); overload;
    procedure GetAlias_C(Names_: U_ArrayString; OnResult: TON_GetAliasC);
    procedure GetAlias_M(Names_: U_ArrayString; OnResult: TON_GetAliasM);
    procedure GetAlias_P(Names_: U_ArrayString; OnResult: TON_GetAliasP);
    procedure RemoveAlias(Name_: U_String);
    procedure SearchAlias_C(Filter_: U_String; OnResult: TON_GetAliasC);
    procedure SearchAlias_M(Filter_: U_String; OnResult: TON_GetAliasM);
    procedure SearchAlias_P(Filter_: U_String; OnResult: TON_GetAliasP);
  end;

implementation

procedure TC40_Alias_Service.cmd_SetAlias(Sender: TPeerIO; InData: TDFE);
var
  i: Integer;
  N_, Hash_: U_String;
  HS: TZDB2_HashString;
begin
  for i := 0 to Alias_DB.Count - 1 do
    begin
      HS := Alias_DB[i];
      if N_.Same(HS.Data.GetDefaultValue('Alias', '')) then
          HS.Remove;
    end;

  N_ := InData.R.ReadString;
  Hash_ := InData.R.ReadString;
  HS := Alias_DB.NewData();
  HS.Data['Alias'] := N_;
  HS.Data['Name'] := Hash_;
  Alias_DB.Flush(False);
end;

procedure TC40_Alias_Service.cmd_GetAlias(Sender: TPeerIO; InData, OutData: TDFE);
  function found_(N_: U_String): TZDB2_HashString;
  var
    i: Integer;
    HS: TZDB2_HashString;
  begin
    Result := nil;
    for i := 0 to Alias_DB.Count - 1 do
      begin
        HS := Alias_DB[i];
        if N_.Same(HS.Data.GetDefaultValue('Alias', '')) then
          begin
            Result := HS;
            exit;
          end;
      end;
  end;

var
  tmp: TZDB2_HashString;
begin
  while InData.R.NotEnd do
    begin
      tmp := found_(InData.R.ReadString);
      if tmp <> nil then
        begin
          OutData.WriteString(tmp.Data.GetDefaultValue('Alias', ''));
          OutData.WriteString(tmp.Data.GetDefaultValue('Name', ''));
        end;
    end;
end;

procedure TC40_Alias_Service.cmd_RemoveAlias(Sender: TPeerIO; InData: TDFE);
var
  i: Integer;
  N_: U_String;
  HS: TZDB2_HashString;
begin
  N_ := InData.R.ReadString;
  for i := 0 to Alias_DB.Count - 1 do
    begin
      HS := Alias_DB[i];
      if umlMultipleMatch(N_, HS.Data.GetDefaultValue('Alias', '')) then
          HS.Remove;
    end;
  Alias_DB.Flush(False);
end;

procedure TC40_Alias_Service.cmd_SearchAlias(Sender: TPeerIO; InData, OutData: TDFE);
var
  i: Integer;
  N_: U_String;
  HS: TZDB2_HashString;
begin
  N_ := InData.R.ReadString;
  for i := 0 to Alias_DB.Count - 1 do
    begin
      HS := Alias_DB[i];
      if umlSearchMatch(N_, HS.Data.GetDefaultValue('Alias', '')) then
        begin
          OutData.WriteString(HS.Data.GetDefaultValue('Alias', ''));
          OutData.WriteString(HS.Data.GetDefaultValue('Name', ''));
        end;
    end;
end;

constructor TC40_Alias_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
var
  fs: TCore_Stream;
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);

  // cmd
  DTNoAuthService.RecvTunnel.RegisterDirectStream('SetAlias').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_SetAlias;
  DTNoAuthService.RecvTunnel.RegisterStream('GetAlias').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetAlias;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('RemoveAlias').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemoveAlias;
  DTNoAuthService.RecvTunnel.RegisterStream('SearchAlias').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_SearchAlias;

  // init DB
  ZDB2RecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('RecycleMemory', '5*1000'), 5 * 1000);
  ZDB2DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('DeltaSpace', '1*1024*1024'), 1 * 1024 * 1024);
  ZDB2BlockSize := EStrToInt(ParamList.GetDefaultValue('BlockSize', '200'), 200);
  ZDB2EnabledCipher := EStrToBool(ParamList.GetDefaultValue('EnabledCipher', 'True'), True);
  ZDB2CipherName := ParamList.GetDefaultValue('Cipher', TCipher.CCipherSecurityName[TCipherSecurity.csRijndael]);
  ZDB2Password := ParamList.GetDefaultValue('Password', Z.Net.C4.C40_Password);

  if ZDB2EnabledCipher then
      ZDB2Cipher := TZDB2_Cipher.Create(ZDB2CipherName, ZDB2Password, 1, True, True)
  else
      ZDB2Cipher := nil;
  C40_Alias_DB_FileName := umlCombineFileName(DTNoAuthService.PublicFileDirectory, PFormat('DTC40_%s.Space', [ServiceInfo.ServiceTyp.Text]));

  if umlFileExists(C40_Alias_DB_FileName) then
      fs := TCore_FileStream.Create(C40_Alias_DB_FileName, fmOpenReadWrite)
  else
      fs := TCore_FileStream.Create(C40_Alias_DB_FileName, fmCreate);

  Alias_DB := TZDB2_List_HashString.Create(
    TZDB2_HashString,
    nil,
    ZDB2RecycleMemoryTimeOut,
    fs,
    False,
    ZDB2DeltaSpace,
    ZDB2BlockSize,
    ZDB2Cipher);
  Alias_DB.AutoFreeStream := True;

  // only instance
  ServiceInfo.OnlyInstance := True;
  UpdateToGlobalDispatch;
end;

destructor TC40_Alias_Service.Destroy;
begin
  Alias_DB.Flush;
  DisposeObjectAndNil(Alias_DB);
  inherited Destroy;
end;

procedure TC40_Alias_Service.SafeCheck;
begin
  inherited SafeCheck;
  Alias_DB.Flush;
end;

procedure TC40_Alias_Service.Progress;
begin
  inherited Progress;
  Alias_DB.Progress;
end;

constructor TON_Temp_GetAliasC.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Temp_GetAliasC.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  NameKey_: THashStringList;
  N_, Hash_: U_String;
begin
  NameKey_ := THashStringList.Create;
  while Result_.R.NotEnd do
    begin
      N_ := Result_.R.ReadString;
      Hash_ := Result_.R.ReadString;
      NameKey_.Add(N_, Hash_);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, NameKey_);
    if Assigned(OnResultM) then
        OnResultM(Client, NameKey_);
    if Assigned(OnResultP) then
        OnResultP(Client, NameKey_);
  except
  end;
  DelayFreeObject(1.0, self, NameKey_);
end;

procedure TON_Temp_GetAliasC.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  NameKey_: THashStringList;
begin
  NameKey_ := THashStringList.Create;
  try
    if Assigned(OnResultC) then
        OnResultC(Client, NameKey_);
    if Assigned(OnResultM) then
        OnResultM(Client, NameKey_);
    if Assigned(OnResultP) then
        OnResultP(Client, NameKey_);
  except
  end;
  DelayFreeObject(1.0, self, NameKey_);
end;

constructor TC40_Alias_Client.Create(source_: TC40_Info; Param_: U_String);
begin
  inherited Create(source_, Param_);
end;

destructor TC40_Alias_Client.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_Alias_Client.Progress;
begin
  inherited Progress;
end;

procedure TC40_Alias_Client.SetAlias(Name_, Hash_: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(Name_);
  d.WriteString(Hash_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('SetAlias', d);
  DisposeObject(d);
end;

procedure TC40_Alias_Client.SetAlias(Name_: U_String; Hash_: TMD5);
begin
  SetAlias(Name_, umlMD5ToStr(Hash_));
end;

procedure TC40_Alias_Client.GetAlias_C(Names_: U_ArrayString; OnResult: TON_GetAliasC);
var
  tmp: TON_Temp_GetAliasC;
  d: TDFE;
  i: Integer;
begin
  tmp := TON_Temp_GetAliasC.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  for i := low(Names_) to high(Names_) do
      d.WriteString(Names_[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetAlias', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Alias_Client.GetAlias_M(Names_: U_ArrayString; OnResult: TON_GetAliasM);
var
  tmp: TON_Temp_GetAliasC;
  d: TDFE;
  i: Integer;
begin
  tmp := TON_Temp_GetAliasC.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  for i := low(Names_) to high(Names_) do
      d.WriteString(Names_[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetAlias', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Alias_Client.GetAlias_P(Names_: U_ArrayString; OnResult: TON_GetAliasP);
var
  tmp: TON_Temp_GetAliasC;
  d: TDFE;
  i: Integer;
begin
  tmp := TON_Temp_GetAliasC.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  for i := low(Names_) to high(Names_) do
      d.WriteString(Names_[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetAlias', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Alias_Client.RemoveAlias(Name_: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(Name_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveAlias', d);
  DisposeObject(d);
end;

procedure TC40_Alias_Client.SearchAlias_C(Filter_: U_String; OnResult: TON_GetAliasC);
var
  tmp: TON_Temp_GetAliasC;
  d: TDFE;
begin
  tmp := TON_Temp_GetAliasC.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  d.WriteString(Filter_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('SearchAlias', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Alias_Client.SearchAlias_M(Filter_: U_String; OnResult: TON_GetAliasM);
var
  tmp: TON_Temp_GetAliasC;
  d: TDFE;
begin
  tmp := TON_Temp_GetAliasC.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  d.WriteString(Filter_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('SearchAlias', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Alias_Client.SearchAlias_P(Filter_: U_String; OnResult: TON_GetAliasP);
var
  tmp: TON_Temp_GetAliasC;
  d: TDFE;
begin
  tmp := TON_Temp_GetAliasC.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  d.WriteString(Filter_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('SearchAlias', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

initialization

RegisterC40('Alias', TC40_Alias_Service, TC40_Alias_Client);

end.
