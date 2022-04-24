{ ****************************************************************************** }
{ * cloud 4.0 network disk Administrator tool                                  * }
{ ****************************************************************************** }
unit Z.Net.C4_NetDisk_Admin_Tool;

{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine,
  Z.Geometry2D, Z.DFE, Z.Json, Z.Expression,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.ZDB2,
  Z.GHashList,
  Z.LinearAction,
  Z.Net, Z.Net.PhysicsIO,
  Z.Net.DoubleTunnelIO.NoAuth,
  Z.Net.C4_NetDisk_Service,
  Z.Net.C4_NetDisk_Directory,
  Z.Net.C4_UserDB,
  Z.Net.C4_FS2,
  Z.Net.C4_Log_DB,
  Z.Net.C4_TEKeyValue,
  Z.Net.C4;

type
  TC40_NetDisk_Admin_Tool_Service = class;

  TSearch_And_Remove_InvalidFrag_Bridge = class(TProgress_Bridge)
  public
    Admin_Service: TC40_NetDisk_Admin_Tool_Service;
    FS2_Cli: TC40_FS2_Client;
    constructor Create(Framework_: TZNet); override;
    destructor Destroy; override;
    procedure Do_SearchInvalidFrag(Sender: TC40_NetDisk_Directory_Client; SearchResult: U_StringArray);
  end;

  TCheck_And_Recycle_Fragment_For_FS2_Bridge = class(TProgress_Bridge)
  public
    Admin_Service: TC40_NetDisk_Admin_Tool_Service;
    constructor Create(Framework_: TZNet); override;
    destructor Destroy; override;
    procedure Do_PoolFrag(Sender: TC40_FS2_Client; arry: TFS2_PoolFragInfo_Array);
  end;

  TC40_NetDisk_Admin_Tool_Service = class(TC40_Base_NoAuth_Service, I_ON_C40_NetDisk_Directory_Client_Interface)
  protected
    // user db interface
    FUserDB_Client: TC40_NetDisk_UserDB_Client;
    function Get_UserDB_Client: TC40_NetDisk_UserDB_Client;
    procedure Set_UserDB_Client(const Value: TC40_NetDisk_UserDB_Client);
  protected
    // directory interface
    FDirectory_Client: TC40_NetDisk_Directory_Client;
    function Get_Directory_Client: TC40_NetDisk_Directory_Client;
    procedure Set_Directory_Client(const Value: TC40_NetDisk_Directory_Client);
  protected
    // log interface
    FLog_Client: TC40_NetDisk_Log_DB_Client;
    function Get_Log_Client: TC40_NetDisk_Log_DB_Client;
    procedure Set_Log_Client(const Value: TC40_NetDisk_Log_DB_Client);
  protected
    // TEKeyValue Interface
    FTEKeyValue_Client: TC40_NetDisk_TEKeyValue_Client;
    function Get_TEKeyValue_Client: TC40_NetDisk_TEKeyValue_Client;
    procedure Set_TEKeyValue_Client(const Value: TC40_NetDisk_TEKeyValue_Client);
    procedure Do_Remove_Directory_MD5(arry: U_StringArray);
    procedure Do_Remove_Directory_Invalid_Frag(arry: U_StringArray);
  protected
    // FS2.0 interface
    FFS2_Client_Pool: TC40_NetDisk_FS2_Client_List;
  public
    // deployment
    property UserDB_Client: TC40_NetDisk_UserDB_Client read Get_UserDB_Client write Set_UserDB_Client;
    property Directory_Client: TC40_NetDisk_Directory_Client read Get_Directory_Client write Set_Directory_Client;
    property Log_Client: TC40_NetDisk_Log_DB_Client read Get_Log_Client write Set_Log_Client;
    property TEKeyValue_Client: TC40_NetDisk_TEKeyValue_Client read Get_TEKeyValue_Client write Set_TEKeyValue_Client;
    property FS2_Client_Pool: TC40_NetDisk_FS2_Client_List read FFS2_Client_Pool;
    // automated config.
    procedure Automated_Config_NetDisk_Admin_Relevance;
    function Check_NetDisk_Admin_Relevance(Status_: Boolean): Boolean; overload;
    function Check_NetDisk_Admin_Relevance(): Boolean; overload;
  protected
    procedure cmd_Enabled_Automated_Admin_Program(Sender: TPeerIO; InData: TDFE);
    procedure cmd_Check_And_Recycle_Fragment_For_FS2(Sender: TPeerIO; InData: TDFE);
  public
    Enabled_Automated_Admin_Program: Boolean;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
    procedure PostLog(info: SystemString); overload;
    procedure PostLog(info1, info2: SystemString); overload;
    procedure PostLog(const v: SystemString; const Args: array of const); overload;
    procedure PostLog(const v: SystemString; const Args: array of const; info2: SystemString); overload;

    procedure Check_And_Recycle_Fragment_For_FS2(FS2_Cli: TC40_FS2_Client); overload;
    procedure Check_And_Recycle_Fragment_For_FS2(); overload;
  end;

{$REGION 'Define_And_Bridge'}
{$ENDREGION 'Define_And_Bridge'}

  TC40_NetDisk_Admin_Tool_Client = class(TC40_Base_NoAuth_Client)
  public
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;

    procedure Enabled_Automated_Admin_Program(value_: Boolean);
    procedure Check_And_Recycle_Fragment_For_FS2;
  end;

  TC40_NetDisk_Admin_Tool_Client_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_NetDisk_Admin_Tool_Client>;

implementation

uses DateUtils;

constructor TSearch_And_Remove_InvalidFrag_Bridge.Create(Framework_: TZNet);
begin
  inherited Create(Framework_);
  Admin_Service := nil;
  FS2_Cli := nil;
end;

destructor TSearch_And_Remove_InvalidFrag_Bridge.Destroy;
begin
  inherited Destroy;
end;

procedure TSearch_And_Remove_InvalidFrag_Bridge.Do_SearchInvalidFrag(Sender: TC40_NetDisk_Directory_Client; SearchResult: U_StringArray);
var
  i: Integer;
begin
  if (FS2_Cli <> nil) and (FS2_Cli.Connected) then
    begin
      FS2_Cli.FS2_RemoveFile(SearchResult);
      for i := low(SearchResult) to high(SearchResult) do
          Admin_Service.PostLog('%s - search and remove Invalid Frag "%s"', [Admin_Service.ServiceInfo.ServiceTyp.Text, SearchResult[i]]);
    end;
  DelayFreeObj(1.0, self);
end;

constructor TCheck_And_Recycle_Fragment_For_FS2_Bridge.Create(Framework_: TZNet);
begin
  inherited Create(Framework_);
  Admin_Service := nil;
end;

destructor TCheck_And_Recycle_Fragment_For_FS2_Bridge.Destroy;
begin
  inherited Destroy;
end;

procedure TCheck_And_Recycle_Fragment_For_FS2_Bridge.Do_PoolFrag(Sender: TC40_FS2_Client; arry: TFS2_PoolFragInfo_Array);
var
  L: TPascalStringList;
  time_: TDateTime;
  i: Integer;
  new_arry: U_StringArray;
  tmp: TSearch_And_Remove_InvalidFrag_Bridge;
begin
  if (Admin_Service.FDirectory_Client <> nil) and (Admin_Service.FDirectory_Client.Connected) then
    begin
      L := TPascalStringList.Create;
      time_ := IncMinute(umlNow, -60);
      for i := low(arry) to high(arry) do
        begin
          try
            if CompareDateTime(arry[i].FileTime, time_) <= 0 then
                L.Add(arry[i].FileName);
          except
          end;
        end;

      if L.Count > 0 then
        begin
          L.FillToArry(new_arry);
          tmp := TSearch_And_Remove_InvalidFrag_Bridge.Create(Framework);
          tmp.Admin_Service := Admin_Service;
          tmp.FS2_Cli := Sender;
          Admin_Service.FDirectory_Client.SearchInvalidFrag_M(new_arry, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_SearchInvalidFrag);
          SetLength(new_arry, 0);
        end;
      disposeObject(L);
    end;
  DelayFreeObj(1.0, self);
end;

function TC40_NetDisk_Admin_Tool_Service.Get_UserDB_Client: TC40_NetDisk_UserDB_Client;
begin
  Result := FUserDB_Client;
end;

procedure TC40_NetDisk_Admin_Tool_Service.Set_UserDB_Client(const Value: TC40_NetDisk_UserDB_Client);
begin
  FUserDB_Client := Value;
end;

function TC40_NetDisk_Admin_Tool_Service.Get_Directory_Client: TC40_NetDisk_Directory_Client;
begin
  Result := FDirectory_Client;
end;

procedure TC40_NetDisk_Admin_Tool_Service.Set_Directory_Client(const Value: TC40_NetDisk_Directory_Client);
begin
  if FDirectory_Client <> nil then
      FDirectory_Client.ON_C40_NetDisk_Directory_Client_Interface := nil;
  FDirectory_Client := Value;
  if FDirectory_Client <> nil then
      FDirectory_Client.ON_C40_NetDisk_Directory_Client_Interface := self;
end;

function TC40_NetDisk_Admin_Tool_Service.Get_Log_Client: TC40_NetDisk_Log_DB_Client;
begin
  Result := FLog_Client;
end;

procedure TC40_NetDisk_Admin_Tool_Service.Set_Log_Client(const Value: TC40_NetDisk_Log_DB_Client);
begin
  FLog_Client := Value;
end;

function TC40_NetDisk_Admin_Tool_Service.Get_TEKeyValue_Client: TC40_NetDisk_TEKeyValue_Client;
begin
  Result := FTEKeyValue_Client;
end;

procedure TC40_NetDisk_Admin_Tool_Service.Set_TEKeyValue_Client(const Value: TC40_NetDisk_TEKeyValue_Client);
begin
  FTEKeyValue_Client := Value;
end;

procedure TC40_NetDisk_Admin_Tool_Service.Do_Remove_Directory_MD5(arry: U_StringArray);
var
  i: Integer;
begin
  for i := low(arry) to high(arry) do
      PostLog('%s - remove file "%s" from directory service.', [ServiceInfo.ServiceTyp.Text, arry[i]]);
end;

procedure TC40_NetDisk_Admin_Tool_Service.Do_Remove_Directory_Invalid_Frag(arry: U_StringArray);
var
  i: Integer;
begin
  if Enabled_Automated_Admin_Program then
    for i := 0 to FFS2_Client_Pool.Count - 1 do
        FFS2_Client_Pool[i].FS2_RemoveFile(arry);
  for i := low(arry) to high(arry) do
      PostLog('%s - remove cache and Invalid Frag "%s"', [ServiceInfo.ServiceTyp.Text, arry[i]]);
end;

procedure TC40_NetDisk_Admin_Tool_Service.Automated_Config_NetDisk_Admin_Relevance;
var
  i: Integer;
  cc: TC40_Custom_Client;
begin
  UserDB_Client := nil;
  Directory_Client := nil;
  TEKeyValue_Client := nil;
  FS2_Client_Pool.Clear;
  for i := 0 to Z.Net.C4.C40_ClientPool.Count - 1 do
    begin
      cc := Z.Net.C4.C40_ClientPool[i];
      if cc is TC40_NetDisk_UserDB_Client then
          UserDB_Client := cc as TC40_NetDisk_UserDB_Client
      else if cc is TC40_NetDisk_Directory_Client then
          Directory_Client := cc as TC40_NetDisk_Directory_Client
      else if cc is TC40_NetDisk_Log_DB_Client then
          Log_Client := cc as TC40_NetDisk_Log_DB_Client
      else if cc is TC40_NetDisk_TEKeyValue_Client then
          TEKeyValue_Client := cc as TC40_NetDisk_TEKeyValue_Client
      else if cc is TC40_NetDisk_FS2_Client then
          FS2_Client_Pool.Add(cc as TC40_NetDisk_FS2_Client);
    end;
end;

function TC40_NetDisk_Admin_Tool_Service.Check_NetDisk_Admin_Relevance(Status_: Boolean): Boolean;
var
  i: Integer;
begin
  Result := True;
  if (UserDB_Client <> nil) and (UserDB_Client.Connected) then
    begin
      if Status_ then
          DoStatus('Admin: Check UserDB_Client passed.');
    end
  else
    begin
      if Status_ then
          DoStatus('Admin: Check UserDB_Client error!');
      Result := Result and False;
    end;

  if (Directory_Client <> nil) and (Directory_Client.Connected) then
    begin
      if Status_ then
          DoStatus('Admin: Check Directory_Client passed.');
    end
  else
    begin
      if Status_ then
          DoStatus('Admin: Check Directory_Client error!');
      Result := Result and False;
    end;

  if (Log_Client <> nil) and (Log_Client.Connected) then
    begin
      if Status_ then
          DoStatus('Admin: Check Log_Client passed.');
    end
  else
    begin
      if Status_ then
          DoStatus('Admin: Check Log_Client error!');
      Result := Result and False;
    end;

  if (TEKeyValue_Client <> nil) and (TEKeyValue_Client.Connected) then
    begin
      if Status_ then
          DoStatus('Admin: Check TEKeyValue_Client passed.');
    end
  else
    begin
      if Status_ then
          DoStatus('Admin: Check TEKeyValue_Client error!');
      Result := Result and False;
    end;

  Result := Result and (FS2_Client_Pool.Count > 0);
  for i := 0 to FS2_Client_Pool.Count - 1 do
    begin
      if FS2_Client_Pool[i].Connected then
        begin
          if Status_ then
              DoStatus('Admin: Check FS2_Client: %s passed.', [FS2_Client_Pool[i].ClientInfo.ServiceTyp.Text]);
        end
      else
        begin
          if Status_ then
              DoStatus('Admin: Check FS2_Client: %s error.', [FS2_Client_Pool[i].ClientInfo.ServiceTyp.Text]);
          Result := Result and False;
        end;
    end;
end;

function TC40_NetDisk_Admin_Tool_Service.Check_NetDisk_Admin_Relevance: Boolean;
begin
  Result := Check_NetDisk_Admin_Relevance(not C40_QuietMode);
end;

procedure TC40_NetDisk_Admin_Tool_Service.cmd_Enabled_Automated_Admin_Program(Sender: TPeerIO; InData: TDFE);
begin
  Enabled_Automated_Admin_Program := InData.R.ReadBool;
end;

procedure TC40_NetDisk_Admin_Tool_Service.cmd_Check_And_Recycle_Fragment_For_FS2(Sender: TPeerIO; InData: TDFE);
begin
  Check_And_Recycle_Fragment_For_FS2;
end;

constructor TC40_NetDisk_Admin_Tool_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);

  // is only instance
  ServiceInfo.OnlyInstance := False;
  UpdateToGlobalDispatch;
  ParamList.SetDefaultValue('OnlyInstance', if_(ServiceInfo.OnlyInstance, 'True', 'False'));

  DTNoAuthService.RecvTunnel.RegisterDirectStream('Enabled_Automated_Admin_Program').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Enabled_Automated_Admin_Program;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Check_And_Recycle_Fragment_For_FS2').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Check_And_Recycle_Fragment_For_FS2;

  Enabled_Automated_Admin_Program := EStrToBool(ParamList.GetDefaultValue('Enabled_Automated_Admin_Program', 'False'), False);

  FUserDB_Client := nil;
  FDirectory_Client := nil;
  FTEKeyValue_Client := nil;
  FLog_Client := nil;
  FFS2_Client_Pool := TC40_NetDisk_FS2_Client_List.Create;
end;

destructor TC40_NetDisk_Admin_Tool_Service.Destroy;
begin
  disposeObject(FFS2_Client_Pool);
  inherited Destroy;
end;

procedure TC40_NetDisk_Admin_Tool_Service.SafeCheck;
begin
  inherited SafeCheck;
  if not Check_NetDisk_Admin_Relevance(False) then
    begin
      Automated_Config_NetDisk_Admin_Relevance();
      Check_NetDisk_Admin_Relevance();
    end
  else if Enabled_Automated_Admin_Program then
      Check_And_Recycle_Fragment_For_FS2;
end;

procedure TC40_NetDisk_Admin_Tool_Service.Progress;
begin
  inherited Progress;
end;

procedure TC40_NetDisk_Admin_Tool_Service.PostLog(info: SystemString);
begin
  if (Log_Client = nil) or (not Log_Client.Connected) then
      exit;
  Log_Client.PostLog('NetDisk_VM_' + AliasOrHash + '_' + MakeNowDateStr, info, '');
end;

procedure TC40_NetDisk_Admin_Tool_Service.PostLog(info1, info2: SystemString);
begin
  if (Log_Client = nil) or (not Log_Client.Connected) then
      exit;
  Log_Client.PostLog('NetDisk_VM_' + AliasOrHash + '_' + MakeNowDateStr, info1, info2);
end;

procedure TC40_NetDisk_Admin_Tool_Service.PostLog(const v: SystemString; const Args: array of const);
begin
  PostLog(PFormat(v, Args));
end;

procedure TC40_NetDisk_Admin_Tool_Service.PostLog(const v: SystemString; const Args: array of const; info2: SystemString);
begin
  PostLog(PFormat(v, Args), info2);
end;

procedure TC40_NetDisk_Admin_Tool_Service.Check_And_Recycle_Fragment_For_FS2(FS2_Cli: TC40_FS2_Client);
var
  tmp: TCheck_And_Recycle_Fragment_For_FS2_Bridge;
begin
  if not FS2_Cli.Connected then
      exit;
  tmp := TCheck_And_Recycle_Fragment_For_FS2_Bridge.Create(FS2_Cli.Client.SendTunnel);
  tmp.Admin_Service := self;
  FS2_Cli.FS2_PoolFragM({$IFDEF FPC}@{$ENDIF FPC}tmp.Do_PoolFrag);
end;

procedure TC40_NetDisk_Admin_Tool_Service.Check_And_Recycle_Fragment_For_FS2;
var
  i: Integer;
begin
  for i := 0 to FS2_Client_Pool.Count - 1 do
      Check_And_Recycle_Fragment_For_FS2(FS2_Client_Pool[i]);
end;

constructor TC40_NetDisk_Admin_Tool_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
end;

destructor TC40_NetDisk_Admin_Tool_Client.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_NetDisk_Admin_Tool_Client.Progress;
begin
  inherited Progress;
end;

procedure TC40_NetDisk_Admin_Tool_Client.Enabled_Automated_Admin_Program(value_: Boolean);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteBool(value_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Enabled_Automated_Admin_Program', d);
  disposeObject(d);
end;

procedure TC40_NetDisk_Admin_Tool_Client.Check_And_Recycle_Fragment_For_FS2;
begin
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Check_And_Recycle_Fragment_For_FS2');
end;

initialization

RegisterC40('NetDisk_Admin', TC40_NetDisk_Admin_Tool_Service, TC40_NetDisk_Admin_Tool_Client);

end.
