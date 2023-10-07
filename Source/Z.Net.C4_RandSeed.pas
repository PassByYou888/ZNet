{ ****************************************************************************** }
{ * cloud 4.0 global network random Seed                                       * }
{ ****************************************************************************** }
unit Z.Net.C4_RandSeed;

{$DEFINE FPC_DELPHI_MODE}
{$I ..\Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine,
  Z.Geometry2D, Z.DFE, Z.Json, Z.Expression,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.FragmentBuffer, // solve for discontinuous space
  Z.ZDB2, Z.HashList.Templet,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth, Z.Net.C4;

type
  TC40_RandSeed_Client = class;
  TC40_RandSeed_Seed_Pool = TGeneric_String_Object_Hash<TUInt32HashPointerList>;

  TC40_RandSeed_Service = class(TC40_Base_NoAuth_Service)
  protected
  private
    procedure cmd_MakeSeed(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_RemoveSeed(sender: TPeerIO; InData: TDFE);
  public
    BigSeedPool: TC40_RandSeed_Seed_Pool;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    function GetSeedGroup(Name_: U_String): TUInt32HashPointerList;
  end;

  TC40_RandSeed_Client_On_MakeSeedC = procedure(sender: TC40_RandSeed_Client; Seed_: UInt32);
  TC40_RandSeed_Client_On_MakeSeedM = procedure(sender: TC40_RandSeed_Client; Seed_: UInt32) of object;
{$IFDEF FPC}
  TC40_RandSeed_Client_On_MakeSeedP = procedure(sender: TC40_RandSeed_Client; Seed_: UInt32) is nested;
{$ELSE FPC}
  TC40_RandSeed_Client_On_MakeSeedP = reference to procedure(sender: TC40_RandSeed_Client; Seed_: UInt32);
{$ENDIF FPC}

  TC40_RandSeed_Client_On_MakeSeed = class(TOnResult_Bridge)
  public
    Client: TC40_RandSeed_Client;
    OnResultC: TC40_RandSeed_Client_On_MakeSeedC;
    OnResultM: TC40_RandSeed_Client_On_MakeSeedM;
    OnResultP: TC40_RandSeed_Client_On_MakeSeedP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDFE); override;
  end;

  TC40_RandSeed_Client = class(TC40_Base_NoAuth_Client)
  public
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;

    procedure MakeSeed_Bridge(Group_: U_String; Min_, Max_: UInt32; Bridge_IO_: TPeerIO);
    procedure MakeSeed_C(Group_: U_String; Min_, Max_: UInt32; OnResult: TC40_RandSeed_Client_On_MakeSeedC);
    procedure MakeSeed_M(Group_: U_String; Min_, Max_: UInt32; OnResult: TC40_RandSeed_Client_On_MakeSeedM);
    procedure MakeSeed_P(Group_: U_String; Min_, Max_: UInt32; OnResult: TC40_RandSeed_Client_On_MakeSeedP);
    procedure RemoveSeed(Group_: U_String; Seed_: UInt32);
  end;

  TC40_RandSeed_Client_List = TGenericsList<TC40_RandSeed_Client>;

implementation


procedure TC40_RandSeed_Service.cmd_MakeSeed(sender: TPeerIO; InData, OutData: TDFE);
var
  group_Name_: U_String;
  Group_: TUInt32HashPointerList;
  Min_: UInt32;
  Max_: UInt32;
  tmp: UInt32;
begin
  group_Name_ := InData.R.ReadString;
  Group_ := GetSeedGroup(group_Name_);
  Min_ := InData.R.ReadCardinal;
  Max_ := InData.R.ReadCardinal;

  repeat
      tmp := umlRandomRange(Min_, Max_);
  until not Group_.Exists(tmp);
  Group_.Add(tmp, nil, False);

  OutData.WriteCardinal(tmp);

  sender.Print('make Seed:%d for "%s" total:%d', [tmp, group_Name_.Text, Group_.Count]);
end;

procedure TC40_RandSeed_Service.cmd_RemoveSeed(sender: TPeerIO; InData: TDFE);
var
  group_Name_: U_String;
  Group_: TUInt32HashPointerList;
  tmp: UInt32;
begin
  group_Name_ := InData.R.ReadString;
  tmp := InData.R.ReadCardinal;
  Group_ := GetSeedGroup(group_Name_);
  Group_.Delete(tmp);
  sender.Print('remove Seed:%d for "%s" total:%d', [tmp, group_Name_.Text, Group_.Count]);
end;

constructor TC40_RandSeed_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  DTNoAuthService.RecvTunnel.RegisterStream('MakeSeed').OnExecute := cmd_MakeSeed;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('RemoveSeed').OnExecute := cmd_RemoveSeed;
  // is only instance
  ServiceInfo.OnlyInstance := True;
  UpdateToGlobalDispatch;
  ParamList.SetDefaultValue('OnlyInstance', if_(ServiceInfo.OnlyInstance, 'True', 'False'));

  BigSeedPool := TC40_RandSeed_Seed_Pool.Create(True,
    EStrToInt64(ParamList.GetDefaultValue('Seed_HashPool', '4*1024*1024'), 4 * 1024 * 1024),
    nil);
end;

destructor TC40_RandSeed_Service.Destroy;
begin
  DisposeObjectAndNil(BigSeedPool);
  inherited Destroy;
end;

procedure TC40_RandSeed_Service.SafeCheck;
begin
  inherited SafeCheck;
end;

function TC40_RandSeed_Service.GetSeedGroup(Name_: U_String): TUInt32HashPointerList;
begin
  Result := BigSeedPool[Name_];
  if Result = nil then
    begin
      Result := TUInt32HashPointerList.CustomCreate(1024 * 1024);
      Result.AutoFreeData := False;
      Result.AccessOptimization := True;
      BigSeedPool.FastAdd(Name_, Result);
    end;
end;

constructor TC40_RandSeed_Client_On_MakeSeed.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_RandSeed_Client_On_MakeSeed.DoStreamEvent(sender: TPeerIO; Result_: TDFE);
var
  Seed_: UInt32;
begin
  Seed_ := Result_.R.ReadCardinal;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Seed_);
    if Assigned(OnResultM) then
        OnResultM(Client, Seed_);
    if Assigned(OnResultP) then
        OnResultP(Client, Seed_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_RandSeed_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
end;

destructor TC40_RandSeed_Client.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_RandSeed_Client.MakeSeed_Bridge(Group_: U_String; Min_, Max_: UInt32; Bridge_IO_: TPeerIO);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(Group_);
  D.WriteCardinal(Min_);
  D.WriteCardinal(Max_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('MakeSeed', D, TStream_Event_Bridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_RandSeed_Client.MakeSeed_C(Group_: U_String; Min_, Max_: UInt32; OnResult: TC40_RandSeed_Client_On_MakeSeedC);
var
  tmp: TC40_RandSeed_Client_On_MakeSeed;
  D: TDFE;
begin
  tmp := TC40_RandSeed_Client_On_MakeSeed.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(Group_);
  D.WriteCardinal(Min_);
  D.WriteCardinal(Max_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('MakeSeed', D, tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_RandSeed_Client.MakeSeed_M(Group_: U_String; Min_, Max_: UInt32; OnResult: TC40_RandSeed_Client_On_MakeSeedM);
var
  tmp: TC40_RandSeed_Client_On_MakeSeed;
  D: TDFE;
begin
  tmp := TC40_RandSeed_Client_On_MakeSeed.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(Group_);
  D.WriteCardinal(Min_);
  D.WriteCardinal(Max_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('MakeSeed', D, tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_RandSeed_Client.MakeSeed_P(Group_: U_String; Min_, Max_: UInt32; OnResult: TC40_RandSeed_Client_On_MakeSeedP);
var
  tmp: TC40_RandSeed_Client_On_MakeSeed;
  D: TDFE;
begin
  tmp := TC40_RandSeed_Client_On_MakeSeed.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(Group_);
  D.WriteCardinal(Min_);
  D.WriteCardinal(Max_);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('MakeSeed', D, tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure TC40_RandSeed_Client.RemoveSeed(Group_: U_String; Seed_: UInt32);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(Group_);
  D.WriteCardinal(Seed_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveSeed', D);
  DisposeObject(D);
end;

initialization

RegisterC40('RandSeed', TC40_RandSeed_Service, TC40_RandSeed_Client);

end.
