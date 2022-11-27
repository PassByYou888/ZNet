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

  TON_Temp_GetAlias = class(TOnResultBridge)
  public
    Client: TC40_Alias_Client;
    OnResultC: TON_GetAliasC;
    OnResultM: TON_GetAliasM;
    OnResultP: TON_GetAliasP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_Alias_Client = class(TC40_Base_NoAuth_Client)
  public
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure SetAlias(alias_, Name_: U_String); overload;
    procedure SetAlias(alias_: U_String; Hash_: TMD5); overload;
    procedure GetAlias_C(arry: U_ArrayString; OnResult: TON_GetAliasC);
    procedure GetAlias_M(arry: U_ArrayString; OnResult: TON_GetAliasM);
    procedure GetAlias_P(arry: U_ArrayString; OnResult: TON_GetAliasP);
    procedure RemoveAlias(alias_: U_String);
    procedure SearchAlias_C(Filter_: U_String; OnResult: TON_GetAliasC);
    procedure SearchAlias_M(Filter_: U_String; OnResult: TON_GetAliasM);
    procedure SearchAlias_P(Filter_: U_String; OnResult: TON_GetAliasP);
  end;

  TC40_Alias_Client_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_Alias_Client>;

implementation

procedure TC40_Alias_Service.cmd_SetAlias(Sender: TPeerIO; InData: TDFE);
var
  alias_, Name_: U_String;
  HS: TZDB2_HashString;
begin
  alias_ := InData.R.ReadString;
  Name_ := InData.R.ReadString;

  if Alias_DB.Count > 0 then
    with Alias_DB.Repeat_ do
      repeat
        HS := Queue^.Data;
        if alias_.Same(HS.Data.GetDefaultValue('Alias', '')) then
            Alias_DB.Push_To_Recycle_Pool(HS, true);
      until not Next;
  Alias_DB.Free_Recycle_Pool;

  HS := Alias_DB.NewData();
  HS.Data['Alias'] := alias_;
  HS.Data['Name'] := Name_;
end;

procedure TC40_Alias_Service.cmd_GetAlias(Sender: TPeerIO; InData, OutData: TDFE);
  function Do_found(alias_: U_String): TZDB2_HashString;
  var
    HS: TZDB2_HashString;
  begin
    if Alias_DB.Count > 0 then
      with Alias_DB.Repeat_ do
        repeat
          HS := Queue^.Data;
          if alias_.Same(HS.Data.GetDefaultValue('Alias', '')) then
              Exit(HS);
        until not Next;
    Result := nil;
  end;

var
  tmp: TZDB2_HashString;
begin
  while InData.R.NotEnd do
    begin
      tmp := Do_found(InData.R.ReadString);
      if tmp <> nil then
        begin
          OutData.WriteString(tmp.Data.GetDefaultValue('Alias', ''));
          OutData.WriteString(tmp.Data.GetDefaultValue('Name', ''));
        end;
    end;
end;

procedure TC40_Alias_Service.cmd_RemoveAlias(Sender: TPeerIO; InData: TDFE);
var
  alias_: U_String;
  HS: TZDB2_HashString;
begin
  alias_ := InData.R.ReadString;

  if Alias_DB.Count > 0 then
    with Alias_DB.Repeat_ do
      repeat
        HS := Queue^.Data;
        if umlMultipleMatch(alias_, HS.Data.GetDefaultValue('Alias', '')) then
            Alias_DB.Push_To_Recycle_Pool(HS, true);
      until not Next;
  Alias_DB.Free_Recycle_Pool;
end;

procedure TC40_Alias_Service.cmd_SearchAlias(Sender: TPeerIO; InData, OutData: TDFE);
var
  Filter_: U_String;
  HS: TZDB2_HashString;
begin
  Filter_ := InData.R.ReadString;

  if Alias_DB.Count > 0 then
    with Alias_DB.Repeat_ do
      repeat
        HS := Queue^.Data;
        if umlSearchMatch(Filter_, HS.Data.GetDefaultValue('Alias', '')) then
          begin
            OutData.WriteString(HS.Data.GetDefaultValue('Alias', ''));
            OutData.WriteString(HS.Data.GetDefaultValue('Name', ''));
          end;
      until not Next;
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
  ZDB2EnabledCipher := EStrToBool(ParamList.GetDefaultValue('EnabledCipher', 'False'), False);
  ZDB2CipherName := ParamList.GetDefaultValue('Cipher', TCipher.CCipherSecurityName[TCipherSecurity.csNone]);
  ZDB2Password := ParamList.GetDefaultValue('Password', Z.Net.C4.C40_Password);

  if ZDB2EnabledCipher then
      ZDB2Cipher := TZDB2_Cipher.Create(ZDB2CipherName, ZDB2Password, 1, true, true)
  else
      ZDB2Cipher := nil;
  C40_Alias_DB_FileName := umlCombineFileName(DTNoAuthService.PublicFileDirectory, Get_DB_FileName_Config(PFormat('DTC40_%s.Space', [ServiceInfo.ServiceTyp.Text])));

  if EStrToBool(ParamList.GetDefaultValue('ForeverSave', 'True'), true) and umlFileExists(C40_Alias_DB_FileName) then
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
  Alias_DB.AutoFreeStream := true;

  // only instance
  ServiceInfo.OnlyInstance := true;
  UpdateToGlobalDispatch;
  ParamList.SetDefaultValue('OnlyInstance', if_(ServiceInfo.OnlyInstance, 'True', 'False'));
end;

destructor TC40_Alias_Service.Destroy;
begin
  Alias_DB.Flush;
  DisposeObjectAndNil(Alias_DB);
  DisposeObjectAndNil(ZDB2Cipher);
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

constructor TON_Temp_GetAlias.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Temp_GetAlias.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
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

procedure TON_Temp_GetAlias.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
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

constructor TC40_Alias_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
end;

destructor TC40_Alias_Client.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_Alias_Client.Progress;
begin
  inherited Progress;
end;

procedure TC40_Alias_Client.SetAlias(alias_, Name_: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(alias_);
  d.WriteString(Name_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('SetAlias', d);
  DisposeObject(d);
end;

procedure TC40_Alias_Client.SetAlias(alias_: U_String; Hash_: TMD5);
begin
  SetAlias(alias_, umlMD5ToStr(Hash_));
end;

procedure TC40_Alias_Client.GetAlias_C(arry: U_ArrayString; OnResult: TON_GetAliasC);
var
  tmp: TON_Temp_GetAlias;
  d: TDFE;
  i: Integer;
begin
  tmp := TON_Temp_GetAlias.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  for i := low(arry) to high(arry) do
      d.WriteString(arry[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetAlias', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Alias_Client.GetAlias_M(arry: U_ArrayString; OnResult: TON_GetAliasM);
var
  tmp: TON_Temp_GetAlias;
  d: TDFE;
  i: Integer;
begin
  tmp := TON_Temp_GetAlias.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  for i := low(arry) to high(arry) do
      d.WriteString(arry[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetAlias', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Alias_Client.GetAlias_P(arry: U_ArrayString; OnResult: TON_GetAliasP);
var
  tmp: TON_Temp_GetAlias;
  d: TDFE;
  i: Integer;
begin
  tmp := TON_Temp_GetAlias.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  for i := low(arry) to high(arry) do
      d.WriteString(arry[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetAlias', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Alias_Client.RemoveAlias(alias_: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(alias_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveAlias', d);
  DisposeObject(d);
end;

procedure TC40_Alias_Client.SearchAlias_C(Filter_: U_String; OnResult: TON_GetAliasC);
var
  tmp: TON_Temp_GetAlias;
  d: TDFE;
begin
  tmp := TON_Temp_GetAlias.Create;
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
  tmp: TON_Temp_GetAlias;
  d: TDFE;
begin
  tmp := TON_Temp_GetAlias.Create;
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
  tmp: TON_Temp_GetAlias;
  d: TDFE;
begin
  tmp := TON_Temp_GetAlias.Create;
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
