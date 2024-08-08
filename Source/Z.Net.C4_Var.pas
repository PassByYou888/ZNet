{ ****************************************************************************** }
{ * cloud 4.0 Network Variant                                                  * }
{ ****************************************************************************** }
unit Z.Net.C4_Var;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses Variants,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib,
  Z.Geometry2D, Z.DFE, Z.ListEngine,
  Z.Parsing, Z.Expression, Z.OpCode,
  Z.Json, Z.HashList.Templet, Z.Number,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.FragmentBuffer, // solve for discontinuous space
  Z.ZDB.ObjectData_LIB, Z.ZDB, Z.ZDB.ItemStream_LIB,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth, Z.Net.C4;

type
  TC40_Var_Service = class;
  TC40_Var_Client = class;

{$REGION 'Service Define'}

  TC40_Var_Service_NM_Pool = class(TNumberModulePool)
  public
    Name: U_String;
    Service: TC40_Var_Service;
    Client: TC40_Var_Client;
    Service_Send_IO_ID_List: TIO_ID_List;
    IsTemp, IsFreeing: Boolean;
    LifeTime, OverTime: TTimeTick;

    constructor Create; override;
    destructor Destroy; override;
    procedure DoNMChange(Sender: TNumberModule; OLD_, New_: Variant); override;
  end;

  TOn_C40_Var_Service_NM_Change = procedure(Sender: TC40_Var_Service; NMPool_: TC40_Var_Service_NM_Pool; NM: TNumberModule) of object;
  TOn_C40_Var_Service_NMPool_Event = procedure(Sender: TC40_Var_Service; NMPool_: TC40_Var_Service_NM_Pool) of object;

  TVAR_Service_NMBigPool = TGeneric_String_Object_Hash<TC40_Var_Service_NM_Pool>;
  TC40_Var_NumberModulePool_List = TGenericsList<TC40_Var_Service_NM_Pool>;

  TC40_Var_Service_IO_Define = class(TService_RecvTunnel_UserDefine_NoAuth)
  public
    NM_List: TC40_Var_NumberModulePool_List;
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;
{$ENDREGION 'Service Define'}

  TC40_Var_Service = class(TC40_Base_NoAuth_Service)
  protected
    IsLoading: Boolean;
    procedure DoLoading();
    procedure SaveNMBigPoolAsOX(DB_: TObjectDataManagerOfCache);
    function OP_DoSetSysNM(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; var OP_Param: TOpParam): Variant;
    function OP_DoGetSysNM(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; var OP_Param: TOpParam): Variant;
    procedure DoNMCreateOpRunTime(Sender: TNumberModulePool; OP_: TOpCustomRunTime);
    procedure DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TService_RecvTunnel_UserDefine_NoAuth); override;
  protected
    procedure cmd_NM_Init(Sender: TPeerIO; InData: TDFE);
    procedure cmd_NM_InitAsTemp(Sender: TPeerIO; InData: TDFE);
    procedure cmd_NM_Remove(Sender: TPeerIO; InData: TDFE);
    procedure cmd_NM_RemoveKey(Sender: TPeerIO; InData: TDFE);
    procedure cmd_NM_Get(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_NM_GetValue(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_NM_Open(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_NM_Close(Sender: TPeerIO; InData: TDFE);
    procedure cmd_NM_CloseAll(Sender: TPeerIO; InData: TDFE);
    procedure cmd_NM_Change(Sender: TPeerIO; InData: TDFE);
    procedure cmd_NM_Keep(Sender: TPeerIO; InData: TDFE);
    procedure cmd_NM_Script(Sender: TPeerIO; InData, OutData: TDFE);
    // admin
    procedure cmd_NM_Save(Sender: TPeerIO; InData: TDFE);
    procedure cmd_NM_Search(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_NM_SearchAndRunScript(Sender: TPeerIO; InData: TDFE);
  protected
    ProgressTempNMList: TC40_Var_NumberModulePool_List;
    procedure Progress_NMPool(const Name: PSystemString; Obj: TC40_Var_Service_NM_Pool);
  public
    C40_Var_FileName: U_String;
    NMBigPool: TVAR_Service_NMBigPool;
    OnChange: TOn_C40_Var_Service_NM_Change;
    OnRemove: TOn_C40_Var_Service_NMPool_Event;
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
    procedure SaveData;
    function GetNM(Name_: U_String): TC40_Var_Service_NM_Pool;
    procedure DoRemoveNumberModulePool(NM: TC40_Var_Service_NM_Pool);
    procedure PrintError(v: SystemString); overload;
    procedure PrintError(v: SystemString; const Args: array of const); overload;
  end;

{$REGION 'Client Define'}

  TON_C40_Var_Client_NM_Change = procedure(Sender: TC40_Var_Client; NMPool_: TC40_Var_Service_NM_Pool; NM: TNumberModule) of object;
  TON_C40_Var_Client_NM_Remove_Event = procedure(Sender: TC40_Var_Client; NMName: U_String) of object;

  TC40_Var_Client_NM_GetC = procedure(Sender: TC40_Var_Client; L: TC40_Var_NumberModulePool_List);
  TC40_Var_Client_NM_GetM = procedure(Sender: TC40_Var_Client; L: TC40_Var_NumberModulePool_List) of object;
{$IFDEF FPC}
  TC40_Var_Client_NM_GetP = procedure(Sender: TC40_Var_Client; L: TC40_Var_NumberModulePool_List) is nested;
{$ELSE FPC}
  TC40_Var_Client_NM_GetP = reference to procedure(Sender: TC40_Var_Client; L: TC40_Var_NumberModulePool_List);
{$ENDIF FPC}

  TC40_Var_Client_NM_Get = class(TOnResult_Bridge)
  public
    Client: TC40_Var_Client;
    OnResultC: TC40_Var_Client_NM_GetC;
    OnResultM: TC40_Var_Client_NM_GetM;
    OnResultP: TC40_Var_Client_NM_GetP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_Var_Client_NM_GetValueC = procedure(Sender: TC40_Var_Client; NM: TNumberModule);
  TC40_Var_Client_NM_GetValueM = procedure(Sender: TC40_Var_Client; NM: TNumberModule) of object;
{$IFDEF FPC}
  TC40_Var_Client_NM_GetValueP = procedure(Sender: TC40_Var_Client; NM: TNumberModule) is nested;
{$ELSE FPC}
  TC40_Var_Client_NM_GetValueP = reference to procedure(Sender: TC40_Var_Client; NM: TNumberModule);
{$ENDIF FPC}

  TC40_Var_Client_NM_GetValue = class(TOnResult_Bridge)
  public
    Client: TC40_Var_Client;
    NM_Name: U_String;
    OnResultC: TC40_Var_Client_NM_GetValueC;
    OnResultM: TC40_Var_Client_NM_GetValueM;
    OnResultP: TC40_Var_Client_NM_GetValueP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_Var_Client_NM_OpenC = procedure(Sender: TC40_Var_Client; NMPool_: TC40_Var_Service_NM_Pool);
  TC40_Var_Client_NM_OpenM = procedure(Sender: TC40_Var_Client; NMPool_: TC40_Var_Service_NM_Pool) of object;
{$IFDEF FPC}
  TC40_Var_Client_NM_OpenP = procedure(Sender: TC40_Var_Client; NMPool_: TC40_Var_Service_NM_Pool) is nested;
{$ELSE FPC}
  TC40_Var_Client_NM_OpenP = reference to procedure(Sender: TC40_Var_Client; NMPool_: TC40_Var_Service_NM_Pool);
{$ENDIF FPC}

  TC40_Var_Client_NM_Open = class(TOnResult_Bridge)
  public
    Client: TC40_Var_Client;
    OnResultC: TC40_Var_Client_NM_OpenC;
    OnResultM: TC40_Var_Client_NM_OpenM;
    OnResultP: TC40_Var_Client_NM_OpenP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_Var_Client_NM_ScriptC = procedure(Sender: TC40_Var_Client; Result_: TExpressionValueVector);
  TC40_Var_Client_NM_ScriptM = procedure(Sender: TC40_Var_Client; Result_: TExpressionValueVector) of object;
{$IFDEF FPC}
  TC40_Var_Client_NM_ScriptP = procedure(Sender: TC40_Var_Client; Result_: TExpressionValueVector) is nested;
{$ELSE FPC}
  TC40_Var_Client_NM_ScriptP = reference to procedure(Sender: TC40_Var_Client; Result_: TExpressionValueVector);
{$ENDIF FPC}

  TC40_Var_Client_NM_Script = class(TOnResult_Bridge)
  public
    Client: TC40_Var_Client;
    OnResultC: TC40_Var_Client_NM_ScriptC;
    OnResultM: TC40_Var_Client_NM_ScriptM;
    OnResultP: TC40_Var_Client_NM_ScriptP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;

  TC40_Var_Client_NM_SearchC = procedure(Sender: TC40_Var_Client; NMPool_: TC40_Var_NumberModulePool_List);
  TC40_Var_Client_NM_SearchM = procedure(Sender: TC40_Var_Client; NMPool_: TC40_Var_NumberModulePool_List) of object;
{$IFDEF FPC}
  TC40_Var_Client_NM_SearchP = procedure(Sender: TC40_Var_Client; NMPool_: TC40_Var_NumberModulePool_List) is nested;
{$ELSE FPC}
  TC40_Var_Client_NM_SearchP = reference to procedure(Sender: TC40_Var_Client; NMPool_: TC40_Var_NumberModulePool_List);
{$ENDIF FPC}

  TC40_Var_Client_NM_Search = class(TOnResult_Bridge)
  public
    Client: TC40_Var_Client;
    OnResultC: TC40_Var_Client_NM_SearchC;
    OnResultM: TC40_Var_Client_NM_SearchM;
    OnResultP: TC40_Var_Client_NM_SearchP;
    constructor Create;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
  end;
{$ENDREGION 'Client Define'}

  TC40_Var_Client = class(TC40_Base_NoAuth_Client)
  protected
    procedure cmd_NM_Change(Sender: TPeerIO; InData: TDFE);
    procedure cmd_NM_Remove(Sender: TPeerIO; InData: SystemString);
  public
    NMBigPool: TVAR_Service_NMBigPool;
    OnChange: TON_C40_Var_Client_NM_Change;
    OnRemove: TON_C40_Var_Client_NM_Remove_Event;
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;
    function GetNM(Name_: U_String): TC40_Var_Service_NM_Pool;

    // api
    procedure NM_Init(Name_: U_String; Open_: Boolean; NMPool_: TNumberModulePool);
    procedure NM_InitAsTemp(Name_: U_String; TimeOut_: TTimeTick; Open_: Boolean; NMPool_: TNumberModulePool);

    procedure NM_Remove(Name_: U_String; RemoveLocal: Boolean);
    procedure NM_RemoveKey(Name_, KeyName_: U_String; RemoveLocal: Boolean);

    procedure NM_Get_Bridge(arry: U_StringArray; Bridge_IO_: TPeerIO);
    procedure NM_GetC(arry: U_StringArray; OnResult: TC40_Var_Client_NM_GetC);
    procedure NM_GetM(arry: U_StringArray; OnResult: TC40_Var_Client_NM_GetM);
    procedure NM_GetP(arry: U_StringArray; OnResult: TC40_Var_Client_NM_GetP);

    procedure NM_GetValue_Bridge(NMName_: U_String; ValueNames_: U_StringArray; Bridge_IO_: TPeerIO);
    procedure NM_GetValueC(NMName_: U_String; ValueNames_: U_StringArray; OnResult: TC40_Var_Client_NM_GetValueC);
    procedure NM_GetValueM(NMName_: U_String; ValueNames_: U_StringArray; OnResult: TC40_Var_Client_NM_GetValueM);
    procedure NM_GetValueP(NMName_: U_String; ValueNames_: U_StringArray; OnResult: TC40_Var_Client_NM_GetValueP);

    procedure NM_Open_Bridge(arry: U_StringArray; Bridge_IO_: TPeerIO);
    procedure NM_OpenC(arry: U_StringArray; OnResult: TC40_Var_Client_NM_OpenC);
    procedure NM_OpenM(arry: U_StringArray; OnResult: TC40_Var_Client_NM_OpenM);
    procedure NM_OpenP(arry: U_StringArray; OnResult: TC40_Var_Client_NM_OpenP);

    procedure NM_Close(arry: U_StringArray; RemoveLocal: Boolean);
    procedure NM_CloseAll(RemoveLocal: Boolean);

    procedure NM_Change(NMName_, ValueName_: U_String; Variant_: Variant);

    procedure NM_Keep(NMName_: U_String);

    procedure NM_Script_Bridge(NMName_: U_String; ExpressionTexts_: U_StringArray; Bridge_IO_: TPeerIO);
    procedure NM_ScriptC(NMName_: U_String; ExpressionTexts_: U_StringArray; OnResult: TC40_Var_Client_NM_ScriptC);
    procedure NM_ScriptM(NMName_: U_String; ExpressionTexts_: U_StringArray; OnResult: TC40_Var_Client_NM_ScriptM);
    procedure NM_ScriptP(NMName_: U_String; ExpressionTexts_: U_StringArray; OnResult: TC40_Var_Client_NM_ScriptP);
    // admin
    procedure NM_Save();

    procedure NM_Search_Bridge(filter: U_String; MaxNum: Integer; AutoOpen: Boolean; Bridge_IO_: TPeerIO);
    procedure NM_SearchC(filter: U_String; MaxNum: Integer; AutoOpen: Boolean; OnResult: TC40_Var_Client_NM_SearchC);
    procedure NM_SearchM(filter: U_String; MaxNum: Integer; AutoOpen: Boolean; OnResult: TC40_Var_Client_NM_SearchM);
    procedure NM_SearchP(filter: U_String; MaxNum: Integer; AutoOpen: Boolean; OnResult: TC40_Var_Client_NM_SearchP);

    procedure NM_SearchAndRunScript(filter: U_String; ExpressionTexts_: U_StringArray);
  end;

  TC40_Var_Client_List = TGenericsList<TC40_Var_Client>;

implementation

constructor TC40_Var_Service_NM_Pool.Create;
begin
  inherited Create;
  Name := '';
  Service := nil;
  Service_Send_IO_ID_List := TIO_ID_List.Create;
  IsTemp := False;
  IsFreeing := False;
  LifeTime := 0;
  OverTime := 0;
end;

destructor TC40_Var_Service_NM_Pool.Destroy;
begin
  DisposeObject(Service_Send_IO_ID_List);
  inherited Destroy;
end;

procedure TC40_Var_Service_NM_Pool.DoNMChange(Sender: TNumberModule; OLD_, New_: Variant);
var
  d: TDFE;
  i: Integer;
begin
  inherited DoNMChange(Sender, OLD_, New_);
  if (Service <> nil) and (Service_Send_IO_ID_List.Count > 0) then
    begin
      d := TDFE.Create;
      d.WriteString(Name);
      d.WriteString(Sender.Name);
      d.WriteNM(Sender);
      for i := 0 to Service_Send_IO_ID_List.Count - 1 do
          Service.DTNoAuthService.SendTunnel.SendDirectStreamCmd(Service_Send_IO_ID_List[i], 'NM_Change', d);
      DisposeObject(d);
      try
        if Assigned(Service.OnChange) then
            Service.OnChange(Service, self, Sender);
      except
      end;
    end;
  if Client <> nil then
    begin
      try
        if Assigned(Client.OnChange) then
            Client.OnChange(Client, self, Sender);
      except
      end;
    end;
end;

constructor TC40_Var_Service_IO_Define.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  NM_List := TC40_Var_NumberModulePool_List.Create;
end;

destructor TC40_Var_Service_IO_Define.Destroy;
begin
  DisposeObject(NM_List);
  inherited Destroy;
end;

procedure TC40_Var_Service.DoLoading;
var
  tmp: TObjectDataManager;
  sr: TItemSearch;
  s_: TItemStream;
  NMPool_: TC40_Var_Service_NM_Pool;
begin
  IsLoading := True;

  NMBigPool.Clear;

  // run
  try
    if umlFileExists(C40_Var_FileName) then
      begin
        tmp := TObjectDataManager.Open(C40_Var_FileName, 0, True);
        if tmp.ItemFastFindFirst(tmp.RootField, '', sr) then
          begin
            repeat
              s_ := TItemStream.Create(tmp, sr.HeaderPOS);
              NMPool_ := GetNM(StreamReadString(s_));
              NMPool_.LoadFromStream(s_);
              DisposeObject(s_);
            until not tmp.ItemFastFindNext(sr);
          end;
        DisposeObject(tmp);
      end;
    if not C40_QuietMode then
        DoStatus('extract variant Database done.');
  except
  end;

  // done
  IsLoading := False;
end;

procedure TC40_Var_Service.SaveNMBigPoolAsOX(DB_: TObjectDataManagerOfCache);
{$IFDEF FPC}
  procedure fpc_Progress_(const Name: PSystemString; Obj: TC40_Var_Service_NM_Pool);
  var
    itmHnd: TItemHandle;
    s_: TItemStream;
  begin
    if Obj.IsTemp then
        exit;
    DB_.ItemFastCreate(DB_.RootField, Obj.Name, '', itmHnd);
    s_ := TItemStream.Create(DB_, itmHnd);
    StreamWriteString(s_, Obj.Name);
    Obj.SaveToStream(s_);
    DisposeObject(s_);
  end;
{$ENDIF FPC}


begin
{$IFDEF FPC}
  NMBigPool.ProgressP(fpc_Progress_);
{$ELSE FPC}
  NMBigPool.ProgressP(
      procedure(const Name: PSystemString; Obj: TC40_Var_Service_NM_Pool)
    var
      itmHnd: TItemHandle;
      s_: TItemStream;
    begin
      if Obj.IsTemp then
          exit;
      DB_.ItemFastCreate(DB_.RootField, Obj.Name, '', itmHnd);
      s_ := TItemStream.Create(DB_, itmHnd);
      StreamWriteString(s_, Obj.Name);
      Obj.SaveToStream(s_);
      DisposeObject(s_);
    end);
{$ENDIF FPC}
end;

function TC40_Var_Service.OP_DoSetSysNM(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; var OP_Param: TOpParam): Variant;
var
  NN_Name_, NM_Key_: SystemString;
  NMPool_: TC40_Var_Service_NM_Pool;
begin
  NN_Name_ := VarToStr(OP_Param[0]);
  NM_Key_ := VarToStr(OP_Param[1]);
  NMPool_ := GetNM(NN_Name_);
  if NMPool_.Exists(NM_Key_) then
      NMPool_[NM_Key_].AsValue := OP_Param[2]
  else
      NMPool_[NM_Key_].Origin := OP_Param[2];
  Result := OP_Param[2];
end;

function TC40_Var_Service.OP_DoGetSysNM(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; var OP_Param: TOpParam): Variant;
var
  NN_Name_, NM_Key_: SystemString;
  NMPool_: TC40_Var_Service_NM_Pool;
begin
  NN_Name_ := VarToStr(OP_Param[0]);
  NM_Key_ := VarToStr(OP_Param[1]);
  NMPool_ := NMBigPool[NN_Name_];
  if NMPool_ = nil then
    begin
      Result := OP_Param[2];
      exit;
    end;
  if not NMPool_.Exists(NM_Key_) then
    begin
      Result := OP_Param[2];
      exit;
    end;
  Result := NMPool_[NM_Key_].AsValue;
end;

procedure TC40_Var_Service.DoNMCreateOpRunTime(Sender: TNumberModulePool; OP_: TOpCustomRunTime);
begin
  OP_.Reg_RT_OpM('SetSys', '', OP_DoSetSysNM);
  OP_.Reg_RT_OpM('GetSys', '', OP_DoGetSysNM);
end;

procedure TC40_Var_Service.DoUserOut_Event(Sender: TDTService_NoAuth; UserDefineIO: TService_RecvTunnel_UserDefine_NoAuth);
var
  IO_Def_: TC40_Var_Service_IO_Define;
  i: Integer;
begin
  inherited DoUserOut_Event(Sender, UserDefineIO);
  IO_Def_ := UserDefineIO as TC40_Var_Service_IO_Define;
  for i := 0 to IO_Def_.NM_List.Count - 1 do
      IO_Def_.NM_List[i].Service_Send_IO_ID_List.Remove(IO_Def_.SendTunnelID);
end;

procedure TC40_Var_Service.cmd_NM_Init(Sender: TPeerIO; InData: TDFE);
var
  IODef_: TC40_Var_Service_IO_Define;
  NM_Name: U_String;
  Open_: Boolean;
  NM: TC40_Var_Service_NM_Pool;
begin
  IODef_ := DTNoAuthService.GetUserDefineRecvTunnel(Sender) as TC40_Var_Service_IO_Define;
  if not IODef_.LinkOk then
    begin
      PrintError('no link');
      exit;
    end;
  NM_Name := InData.R.ReadString;
  // open change
  NM := GetNM(NM_Name);
  Open_ := InData.R.ReadBool;
  if Open_ then
    begin
      if IODef_.NM_List.IndexOf(NM) < 0 then
          IODef_.NM_List.Add(NM);
      if NM.Service_Send_IO_ID_List.IndexOf(IODef_.SendTunnelID) < 0 then
          NM.Service_Send_IO_ID_List.Add(IODef_.SendTunnelID);
    end;
  // load NM
  InData.R.ReadNMPool(NM);
end;

procedure TC40_Var_Service.cmd_NM_InitAsTemp(Sender: TPeerIO; InData: TDFE);
var
  IODef_: TC40_Var_Service_IO_Define;
  NM_Name: U_String;
  NM_TimeOut: TTimeTick;
  Open_: Boolean;
  NM: TC40_Var_Service_NM_Pool;
begin
  IODef_ := DTNoAuthService.GetUserDefineRecvTunnel(Sender) as TC40_Var_Service_IO_Define;
  if not IODef_.LinkOk then
    begin
      PrintError('no link');
      exit;
    end;
  NM_Name := InData.R.ReadString;
  NM_TimeOut := InData.R.ReadUInt64;
  // init temp NM
  NM := GetNM(NM_Name);
  NM.IsTemp := True;
  NM.LifeTime := NM_TimeOut;
  NM.OverTime := GetTimeTick + NM.LifeTime;
  // open change
  Open_ := InData.R.ReadBool;
  if Open_ then
    begin
      if IODef_.NM_List.IndexOf(NM) < 0 then
          IODef_.NM_List.Add(NM);
      if NM.Service_Send_IO_ID_List.IndexOf(IODef_.SendTunnelID) < 0 then
          NM.Service_Send_IO_ID_List.Add(IODef_.SendTunnelID);
    end;
  // load NM
  InData.R.ReadNMPool(NM);
end;

procedure TC40_Var_Service.cmd_NM_Remove(Sender: TPeerIO; InData: TDFE);
var
  IODef_: TC40_Var_Service_IO_Define;
  NM_Name: U_String;
begin
  IODef_ := DTNoAuthService.GetUserDefineRecvTunnel(Sender) as TC40_Var_Service_IO_Define;
  if not IODef_.LinkOk then
    begin
      PrintError('no link');
      exit;
    end;
  NM_Name := InData.R.ReadString;
  if not NMBigPool.Exists(NM_Name) then
    begin
      PrintError('no exists number module "%s"', [NM_Name.Text]);
      exit;
    end;
  try
    if Assigned(OnRemove) then
        OnRemove(self, GetNM(NM_Name));
  except
  end;
  DoRemoveNumberModulePool(GetNM(NM_Name));
  NMBigPool.Delete(NM_Name);
end;

procedure TC40_Var_Service.cmd_NM_RemoveKey(Sender: TPeerIO; InData: TDFE);
var
  IODef_: TC40_Var_Service_IO_Define;
  NM_Name, Key_Name: U_String;
  NM: TC40_Var_Service_NM_Pool;
begin
  IODef_ := DTNoAuthService.GetUserDefineRecvTunnel(Sender) as TC40_Var_Service_IO_Define;
  if not IODef_.LinkOk then
    begin
      PrintError('no link');
      exit;
    end;
  NM_Name := InData.R.ReadString;
  if not NMBigPool.Exists(NM_Name) then
    begin
      PrintError('no exists number module "%s"', [NM_Name.Text]);
      exit;
    end;
  Key_Name := InData.R.ReadString;
  NM := GetNM(NM_Name);
  if not NM.Exists(Key_Name) then
    begin
      PrintError('no exists key from number module "%s->%s"', [NM_Name.Text, Key_Name.Text]);
      exit;
    end;
  NM.Delete(Key_Name);
end;

procedure TC40_Var_Service.cmd_NM_Get(Sender: TPeerIO; InData, OutData: TDFE);
var
  IODef_: TC40_Var_Service_IO_Define;
  NM_Name: U_String;
  NM: TC40_Var_Service_NM_Pool;
begin
  IODef_ := DTNoAuthService.GetUserDefineRecvTunnel(Sender) as TC40_Var_Service_IO_Define;
  if not IODef_.LinkOk then
    begin
      PrintError('no link');
      exit;
    end;
  while InData.R.NotEnd do
    begin
      NM_Name := InData.R.ReadString;
      if NMBigPool.Exists(NM_Name) then
        begin
          NM := GetNM(NM_Name);
          OutData.WriteString(NM_Name);
          OutData.WriteNMPool(NM);
        end
      else
        begin
          PrintError('no exists number module "%s"', [NM_Name.Text]);
        end;
    end;
end;

procedure TC40_Var_Service.cmd_NM_GetValue(Sender: TPeerIO; InData, OutData: TDFE);
var
  IODef_: TC40_Var_Service_IO_Define;
  NM_Name: U_String;
  NM: TC40_Var_Service_NM_Pool;
  VName_: U_String;
begin
  IODef_ := DTNoAuthService.GetUserDefineRecvTunnel(Sender) as TC40_Var_Service_IO_Define;
  if not IODef_.LinkOk then
    begin
      PrintError('no link');
      exit;
    end;
  NM_Name := InData.R.ReadString;
  if not NMBigPool.Exists(NM_Name) then
    begin
      PrintError('no exists number module "%s"', [NM_Name.Text]);
      exit;
    end;
  NM := GetNM(NM_Name);
  while InData.R.NotEnd do
    begin
      VName_ := InData.R.ReadString;
      if NM.Exists(VName_) then
        begin
          OutData.WriteString(VName_);
          OutData.WriteNM(NM[VName_]);
        end
      else
        begin
          PrintError('no exists number module "%s" Name "%s"', [NM_Name.Text, VName_.Text]);
        end;
    end;
end;

procedure TC40_Var_Service.cmd_NM_Open(Sender: TPeerIO; InData, OutData: TDFE);
var
  IODef_: TC40_Var_Service_IO_Define;
  NM_Name: U_String;
  NM: TC40_Var_Service_NM_Pool;
begin
  IODef_ := DTNoAuthService.GetUserDefineRecvTunnel(Sender) as TC40_Var_Service_IO_Define;
  if not IODef_.LinkOk then
    begin
      PrintError('no link');
      exit;
    end;
  while InData.R.NotEnd do
    begin
      NM_Name := InData.R.ReadString;
      if NMBigPool.Exists(NM_Name) then
        begin
          NM := GetNM(NM_Name);
          OutData.WriteString(NM_Name);
          OutData.WriteNMPool(NM);
          if IODef_.NM_List.IndexOf(NM) < 0 then
              IODef_.NM_List.Add(NM);
          if NM.Service_Send_IO_ID_List.IndexOf(IODef_.SendTunnelID) < 0 then
              NM.Service_Send_IO_ID_List.Add(IODef_.SendTunnelID);
        end
      else
        begin
          PrintError('no exists number module "%s"', [NM_Name.Text]);
        end;
    end;
end;

procedure TC40_Var_Service.cmd_NM_Close(Sender: TPeerIO; InData: TDFE);
var
  IODef_: TC40_Var_Service_IO_Define;
  NM_Name: U_String;
  NM: TC40_Var_Service_NM_Pool;
begin
  IODef_ := DTNoAuthService.GetUserDefineRecvTunnel(Sender) as TC40_Var_Service_IO_Define;
  if not IODef_.LinkOk then
    begin
      PrintError('no link');
      exit;
    end;
  while InData.R.NotEnd do
    begin
      NM_Name := InData.R.ReadString;
      if NMBigPool.Exists(NM_Name) then
        begin
          NM := GetNM(NM_Name);
          NM.Service_Send_IO_ID_List.Remove(IODef_.SendTunnelID);
          IODef_.NM_List.Remove(NM);
        end
      else
        begin
          PrintError('no exists number module "%s"', [NM_Name.Text]);
        end;
    end;
end;

procedure TC40_Var_Service.cmd_NM_CloseAll(Sender: TPeerIO; InData: TDFE);
var
  IODef_: TC40_Var_Service_IO_Define;
  i: Integer;
begin
  IODef_ := DTNoAuthService.GetUserDefineRecvTunnel(Sender) as TC40_Var_Service_IO_Define;
  if not IODef_.LinkOk then
    begin
      PrintError('no link');
      exit;
    end;
  for i := 0 to IODef_.NM_List.Count - 1 do
      IODef_.NM_List[i].Service_Send_IO_ID_List.Remove(IODef_.SendTunnelID);
  IODef_.NM_List.Clear;
end;

procedure TC40_Var_Service.cmd_NM_Change(Sender: TPeerIO; InData: TDFE);
var
  IODef_: TC40_Var_Service_IO_Define;
  NM_Name: U_String;
  NM: TC40_Var_Service_NM_Pool;
  VName_: U_String;
  v: Variant;
begin
  IODef_ := DTNoAuthService.GetUserDefineRecvTunnel(Sender) as TC40_Var_Service_IO_Define;
  if not IODef_.LinkOk then
    begin
      PrintError('no link');
      exit;
    end;
  NM_Name := InData.R.ReadString;
  if not NMBigPool.Exists(NM_Name) then
    begin
      PrintError('no exists number module "%s"', [NM_Name.Text]);
      exit;
    end;
  NM := GetNM(NM_Name);
  VName_ := InData.R.ReadString;
  v := InData.R.ReadVariant;
  if NM.Exists(VName_) then
      NM[VName_].Value := v
  else
      NM[VName_].Origin := v;
  if NM.IsTemp then
      NM.OverTime := GetTimeTick + NM.LifeTime;
end;

procedure TC40_Var_Service.cmd_NM_Keep(Sender: TPeerIO; InData: TDFE);
var
  IODef_: TC40_Var_Service_IO_Define;
  NM_Name: U_String;
  NM: TC40_Var_Service_NM_Pool;
begin
  IODef_ := DTNoAuthService.GetUserDefineRecvTunnel(Sender) as TC40_Var_Service_IO_Define;
  if not IODef_.LinkOk then
    begin
      PrintError('no link');
      exit;
    end;
  NM_Name := InData.R.ReadString;
  if not NMBigPool.Exists(NM_Name) then
    begin
      PrintError('no exists number module "%s"', [NM_Name.Text]);
      exit;
    end;
  NM := GetNM(NM_Name);
  if NM.IsTemp then
      NM.OverTime := GetTimeTick + NM.LifeTime;
end;

procedure TC40_Var_Service.cmd_NM_Script(Sender: TPeerIO; InData, OutData: TDFE);
var
  IODef_: TC40_Var_Service_IO_Define;
  NM_Name: U_String;
  found_: Boolean;
  NM: TC40_Var_Service_NM_Pool;
  Exp_: U_String;
  Vec_: TExpressionValueVector;
  i: Integer;
begin
  IODef_ := DTNoAuthService.GetUserDefineRecvTunnel(Sender) as TC40_Var_Service_IO_Define;
  if not IODef_.LinkOk then
    begin
      PrintError('no link');
      exit;
    end;
  NM_Name := InData.R.ReadString;
  found_ := NMBigPool.Exists(NM_Name);
  NM := GetNM(NM_Name);
  if not found_ then
    begin
      NM.IsTemp := True;
      NM.LifeTime := 60 * 1000;
      NM.OverTime := GetTimeTick + NM.LifeTime;
      // open change
      if IODef_.NM_List.IndexOf(NM) < 0 then
          IODef_.NM_List.Add(NM);
      if NM.Service_Send_IO_ID_List.IndexOf(IODef_.SendTunnelID) < 0 then
          NM.Service_Send_IO_ID_List.Add(IODef_.SendTunnelID);
    end;

  try
    while InData.R.NotEnd do
      begin
        Exp_ := InData.R.ReadString;
        if Exp_.L > 0 then
          begin
            if NM.IsVectorScript(Exp_) then
              begin
                Vec_ := NM.RunVectorScript(Exp_);
                for i := 0 to length(Vec_) - 1 do
                    OutData.WriteVariant(Vec_[i]);
                SetLength(Vec_, 0);
              end
            else
                OutData.WriteVariant(NM.RunScript(Exp_));
          end;
      end;
  except
  end;
  if NM.IsTemp then
      NM.OverTime := GetTimeTick + NM.LifeTime;
end;

procedure TC40_Var_Service.cmd_NM_Save(Sender: TPeerIO; InData: TDFE);
begin
  SaveData;
end;

procedure TC40_Var_Service.cmd_NM_Search(Sender: TPeerIO; InData, OutData: TDFE);
var
  IODef_: TC40_Var_Service_IO_Define;
  filter_: U_String;
  MaxNum: Integer;
  AutoOpen: Boolean;
{$IFDEF FPC}
  procedure fpc_Progress_(const Name: PSystemString; Obj: TC40_Var_Service_NM_Pool);
  begin
    if OutData.Count shr 1 >= MaxNum then
        exit;
    if not umlSearchMatch(filter_, Obj.Name) then
        exit;
    OutData.WriteString(Obj.Name);
    OutData.WriteNMPool(Obj);
    if AutoOpen then
      begin
        if IODef_.NM_List.IndexOf(Obj) < 0 then
            IODef_.NM_List.Add(Obj);
        if Obj.Service_Send_IO_ID_List.IndexOf(IODef_.SendTunnelID) < 0 then
            Obj.Service_Send_IO_ID_List.Add(IODef_.SendTunnelID);
      end;
  end;
{$ENDIF FPC}


begin
  IODef_ := DTNoAuthService.GetUserDefineRecvTunnel(Sender) as TC40_Var_Service_IO_Define;
  filter_ := InData.R.ReadString;
  MaxNum := InData.R.ReadInteger;
  AutoOpen := InData.R.ReadBool;
{$IFDEF FPC}
  NMBigPool.ProgressP(fpc_Progress_);
{$ELSE FPC}
  NMBigPool.ProgressP(procedure(const Name: PSystemString; Obj: TC40_Var_Service_NM_Pool)
    begin
      if OutData.Count shr 1 >= MaxNum then
          exit;
      if not umlSearchMatch(filter_, Obj.Name) then
          exit;
      OutData.WriteString(Obj.Name);
      OutData.WriteNMPool(Obj);
      if AutoOpen then
        begin
          if IODef_.NM_List.IndexOf(Obj) < 0 then
              IODef_.NM_List.Add(Obj);
          if Obj.Service_Send_IO_ID_List.IndexOf(IODef_.SendTunnelID) < 0 then
              Obj.Service_Send_IO_ID_List.Add(IODef_.SendTunnelID);
        end;
    end);
{$ENDIF FPC}
end;

procedure TC40_Var_Service.cmd_NM_SearchAndRunScript(Sender: TPeerIO; InData: TDFE);
var
  IODef_: TC40_Var_Service_IO_Define;
  filter_: U_String;
  Exp_Arry: U_StringArray;

{$IFDEF FPC}
  procedure fpc_Progress_(const Name: PSystemString; Obj: TC40_Var_Service_NM_Pool);
  var
    i: Integer;
    Exp_: U_String;
  begin
    if not umlSearchMatch(filter_, Obj.Name) then
        exit;
    try
      for i := 0 to length(Exp_Arry) - 1 do
        begin
          Exp_ := Exp_Arry[i];
          if Exp_.L > 0 then
            begin
              if Obj.IsVectorScript(Exp_) then
                  Obj.RunVectorScript(Exp_)
              else
                  Obj.RunScript(Exp_);
            end;
        end;
    except
    end;
  end;
{$ENDIF FPC}
  procedure Do_Read_Exp_Arry;
  var
    i: Integer;
  begin
    SetLength(Exp_Arry, InData.Count - 2);
    for i := 1 to InData.Count - 1 do
        Exp_Arry[i - 1] := umlTrimSpace(InData.R.ReadString);
  end;

begin
  IODef_ := DTNoAuthService.GetUserDefineRecvTunnel(Sender) as TC40_Var_Service_IO_Define;
  filter_ := InData.R.ReadString;
  Do_Read_Exp_Arry;
{$IFDEF FPC}
  NMBigPool.ProgressP(fpc_Progress_);
{$ELSE FPC}
  NMBigPool.ProgressP(procedure(const Name: PSystemString; Obj: TC40_Var_Service_NM_Pool)
    var
      i: Integer;
      Exp_: U_String;
    begin
      if not umlSearchMatch(filter_, Obj.Name) then
          exit;
      try
        for i := 0 to length(Exp_Arry) - 1 do
          begin
            Exp_ := Exp_Arry[i];
            if Exp_.L > 0 then
              begin
                if Obj.IsVectorScript(Exp_) then
                    Obj.RunVectorScript(Exp_)
                else
                    Obj.RunScript(Exp_);
              end;
          end;
      except
      end;
    end);
{$ENDIF FPC}
  SetLength(Exp_Arry, 0);
end;

procedure TC40_Var_Service.Progress_NMPool(const Name: PSystemString; Obj: TC40_Var_Service_NM_Pool);
begin
  if (Obj.IsTemp) and (not Obj.IsFreeing) and (Obj.OverTime < GetTimeTick) then
    begin
      Obj.IsFreeing := True;
      ProgressTempNMList.Add(Obj);
    end;
end;

constructor TC40_Var_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  DTNoAuthService.RecvTunnel.RegisterDirectStream('NM_Init').OnExecute := cmd_NM_Init;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('NM_InitAsTemp').OnExecute := cmd_NM_InitAsTemp;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('NM_Remove').OnExecute := cmd_NM_Remove;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('NM_RemoveKey').OnExecute := cmd_NM_RemoveKey;
  DTNoAuthService.RecvTunnel.RegisterStream('NM_Get').OnExecute := cmd_NM_Get;
  DTNoAuthService.RecvTunnel.RegisterStream('NM_GetValue').OnExecute := cmd_NM_GetValue;
  DTNoAuthService.RecvTunnel.RegisterStream('NM_Open').OnExecute := cmd_NM_Open;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('NM_Close').OnExecute := cmd_NM_Close;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('NM_CloseAll').OnExecute := cmd_NM_CloseAll;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('NM_Change').OnExecute := cmd_NM_Change;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('NM_Keep').OnExecute := cmd_NM_Keep;
  DTNoAuthService.RecvTunnel.RegisterStream('NM_Script').OnExecute := cmd_NM_Script;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('NM_Save').OnExecute := cmd_NM_Save;
  DTNoAuthService.RecvTunnel.RegisterStream('NM_Search').OnExecute := cmd_NM_Search;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('NM_SearchAndRunScript').OnExecute := cmd_NM_SearchAndRunScript;
  DTNoAuthService.RecvTunnel.PeerIOUserDefineClass := TC40_Var_Service_IO_Define;
  // is only instance
  ServiceInfo.OnlyInstance := True;
  UpdateToGlobalDispatch;
  ParamList.SetDefaultValue('OnlyInstance', if_(ServiceInfo.OnlyInstance, 'True', 'False'));

  ProgressTempNMList := TC40_Var_NumberModulePool_List.Create;
  C40_Var_FileName := umlCombineFileName(DTNoAuthService.PublicFileDirectory, Get_DB_FileName_Config(PFormat('DTC40_%s.OX', [ServiceInfo.ServiceTyp.Text])));

  NMBigPool := TVAR_Service_NMBigPool.Create(True,
    EStrToInt64(ParamList.GetDefaultValue('NM_HashPool', '1024*1024'), 1024 * 1024),
    nil);
  NMBigPool.AccessOptimization := True;
  NMBigPool.IgnoreCase := True;
  OnChange := nil;
  OnRemove := nil;

  if EStrToBool(ParamList.GetDefaultValue('ForeverSave', 'True'), True) and umlFileExists(C40_Var_FileName) then
      DoLoading();
end;

destructor TC40_Var_Service.Destroy;
begin
  SaveData;
  DisposeObject(ProgressTempNMList);
  DisposeObject(NMBigPool);
  inherited Destroy;
end;

procedure TC40_Var_Service.SafeCheck;
begin
  inherited SafeCheck;
end;

procedure TC40_Var_Service.Progress;
var
  i: Integer;
begin
  inherited Progress;

  ProgressTempNMList.Clear;
  NMBigPool.ProgressM(Progress_NMPool);
  try
    for i := 0 to ProgressTempNMList.Count - 1 do
      begin
        try
          if Assigned(OnRemove) then
              OnRemove(self, ProgressTempNMList[i]);
        except
        end;
        DoRemoveNumberModulePool(ProgressTempNMList[i]);
        NMBigPool.Delete(ProgressTempNMList[i].Name);
      end;
  except
  end;
  ProgressTempNMList.Clear;
end;

procedure TC40_Var_Service.SaveData;
var
  tmp: TObjectDataManagerOfCache;
begin
  tmp := TObjectDataManagerOfCache.CreateNew(C40_Var_FileName, 0);
  try
      SaveNMBigPoolAsOX(tmp);
  except
  end;
  DisposeObject(tmp);
end;

function TC40_Var_Service.GetNM(Name_: U_String): TC40_Var_Service_NM_Pool;
begin
  Result := NMBigPool[Name_];
  if Result = nil then
    begin
      Result := TC40_Var_Service_NM_Pool.Create;
      Result.Name := Name_;
      Result.Service := self;
      Result.OnNMCreateOpRunTime := DoNMCreateOpRunTime;
      NMBigPool.FastAdd(Name_, Result);
    end;
end;

procedure TC40_Var_Service.DoRemoveNumberModulePool(NM: TC40_Var_Service_NM_Pool);
var
  i: Integer;
  arry: TIO_Array;
  ID_: Cardinal;
  IO_: TPeerIO;
  IODef_: TC40_Var_Service_IO_Define;
begin
  for i := 0 to NM.Service_Send_IO_ID_List.Count - 1 do
      DTNoAuthService.SendTunnel.SendDirectConsoleCmd(NM.Service_Send_IO_ID_List[i], 'NM_Remove', NM.Name);

  DTNoAuthService.RecvTunnel.GetIO_Array(arry);
  for ID_ in arry do
    begin
      IO_ := DTNoAuthService.RecvTunnel[ID_];
      if IO_ <> nil then
        begin
          IODef_ := IO_.IODefine as TC40_Var_Service_IO_Define;
          IODef_.NM_List.Remove(NM);
        end;
    end;
  if not C40_QuietMode then
      DoStatus('remove NM "%s"', [NM.Name.Text]);
end;

procedure TC40_Var_Service.PrintError(v: SystemString);
begin
  C40PhysicsService.PhysicsTunnel.PrintError(v);
end;

procedure TC40_Var_Service.PrintError(v: SystemString; const Args: array of const);
begin
  PrintError(PFormat(v, Args));
end;

constructor TC40_Var_Client_NM_Get.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_Var_Client_NM_Get.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  L: TC40_Var_NumberModulePool_List;
  NM_Pool_: TC40_Var_Service_NM_Pool;
begin
  L := TC40_Var_NumberModulePool_List.Create;
  while Result_.R.NotEnd do
    begin
      NM_Pool_ := Client.GetNM(Result_.R.ReadString);
      Result_.R.ReadNMPool(NM_Pool_);
      L.Add(NM_Pool_);
    end;
  try
    if Assigned(OnResultC) then
        OnResultC(Client, L)
    else if Assigned(OnResultM) then
        OnResultM(Client, L)
    else if Assigned(OnResultP) then
        OnResultP(Client, L);
  except
  end;
  DelayFreeObject(1.0, self, L);
end;

procedure TC40_Var_Client_NM_Get.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  L: TC40_Var_NumberModulePool_List;
begin
  L := TC40_Var_NumberModulePool_List.Create;
  try
    if Assigned(OnResultC) then
        OnResultC(Client, L)
    else if Assigned(OnResultM) then
        OnResultM(Client, L)
    else if Assigned(OnResultP) then
        OnResultP(Client, L);
  except
  end;
  DelayFreeObject(1.0, self, L);
end;

constructor TC40_Var_Client_NM_GetValue.Create;
begin
  inherited Create;
  Client := nil;
  NM_Name := '';
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_Var_Client_NM_GetValue.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  NM_Pool_: TC40_Var_Service_NM_Pool;
  NM_: TNumberModule;
begin
  NM_Pool_ := Client.GetNM(NM_Name);
  while Result_.R.NotEnd do
    begin
      NM_ := NM_Pool_[Result_.R.ReadString];
      Result_.R.ReadNM(NM_);

      try
        if Assigned(OnResultC) then
            OnResultC(Client, NM_)
        else if Assigned(OnResultM) then
            OnResultM(Client, NM_)
        else if Assigned(OnResultP) then
            OnResultP(Client, NM_);
      except
      end;
    end;
  DelayFreeObject(1.0, self);
end;

procedure TC40_Var_Client_NM_GetValue.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, nil)
    else if Assigned(OnResultM) then
        OnResultM(Client, nil)
    else if Assigned(OnResultP) then
        OnResultP(Client, nil);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_Var_Client_NM_Open.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_Var_Client_NM_Open.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  NM_Pool_: TC40_Var_Service_NM_Pool;
begin
  while Result_.R.NotEnd do
    begin
      NM_Pool_ := Client.GetNM(Result_.R.ReadString);
      Result_.R.ReadNMPool(NM_Pool_);

      try
        if Assigned(OnResultC) then
            OnResultC(Client, NM_Pool_)
        else if Assigned(OnResultM) then
            OnResultM(Client, NM_Pool_)
        else if Assigned(OnResultP) then
            OnResultP(Client, NM_Pool_);
      except
      end;
    end;
  DelayFreeObject(1.0, self);
end;

procedure TC40_Var_Client_NM_Open.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, nil)
    else if Assigned(OnResultM) then
        OnResultM(Client, nil)
    else if Assigned(OnResultP) then
        OnResultP(Client, nil);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_Var_Client_NM_Script.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_Var_Client_NM_Script.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  tmp: TExpressionValueVector;
  i: Integer;
begin
  SetLength(tmp, Result_.Count);
  for i := 0 to Result_.Count - 1 do
      tmp[i] := Result_.ReadVariant(i);
  try
    if Assigned(OnResultC) then
        OnResultC(Client, tmp)
    else if Assigned(OnResultM) then
        OnResultM(Client, tmp)
    else if Assigned(OnResultP) then
        OnResultP(Client, tmp);
  except
  end;
  SetLength(tmp, 0);
  DelayFreeObject(1.0, self);
end;

procedure TC40_Var_Client_NM_Script.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  tmp: TExpressionValueVector;
begin
  SetLength(tmp, 0);
  try
    if Assigned(OnResultC) then
        OnResultC(Client, tmp)
    else if Assigned(OnResultM) then
        OnResultM(Client, tmp)
    else if Assigned(OnResultP) then
        OnResultP(Client, tmp);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_Var_Client_NM_Search.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_Var_Client_NM_Search.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  tmp: TC40_Var_Service_NM_Pool;
  L: TC40_Var_NumberModulePool_List;
begin
  L := TC40_Var_NumberModulePool_List.Create;
  while Result_.R.NotEnd do
    begin
      tmp := Client.GetNM(Result_.R.ReadString);
      Result_.R.ReadNMPool(tmp);
      L.Add(tmp);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, L)
    else if Assigned(OnResultM) then
        OnResultM(Client, L)
    else if Assigned(OnResultP) then
        OnResultP(Client, L);
  except
  end;

  DelayFreeObject(1.0, self, L);
end;

procedure TC40_Var_Client_NM_Search.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  L: TC40_Var_NumberModulePool_List;
begin
  L := TC40_Var_NumberModulePool_List.Create;
  try
    if Assigned(OnResultC) then
        OnResultC(Client, nil)
    else if Assigned(OnResultM) then
        OnResultM(Client, nil)
    else if Assigned(OnResultP) then
        OnResultP(Client, nil);
  except
  end;
  DelayFreeObject(1.0, self, L);
end;

procedure TC40_Var_Client.cmd_NM_Change(Sender: TPeerIO; InData: TDFE);
var
  NMPoolName_, ValueName_: U_String;
  NMPool_: TC40_Var_Service_NM_Pool;
  NM_: TNumberModule;
begin
  NMPoolName_ := InData.R.ReadString;
  ValueName_ := InData.R.ReadString;
  NMPool_ := GetNM(NMPoolName_);
  NM_ := NMPool_[ValueName_];
  InData.R.ReadNM(NM_);
  NM_.DoChange;
end;

procedure TC40_Var_Client.cmd_NM_Remove(Sender: TPeerIO; InData: SystemString);
begin
  if Assigned(OnRemove) then
      OnRemove(self, InData);
  NMBigPool.Delete(InData);
end;

constructor TC40_Var_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('NM_Change').OnExecute := cmd_NM_Change;
  DTNoAuthClient.RecvTunnel.RegisterDirectConsole('NM_Remove').OnExecute := cmd_NM_Remove;
  NMBigPool := TVAR_Service_NMBigPool.Create(True, 1024, nil);
  NMBigPool.AccessOptimization := True;
  NMBigPool.IgnoreCase := True;
  OnChange := nil;
  OnRemove := nil;
end;

destructor TC40_Var_Client.Destroy;
begin
  DisposeObject(NMBigPool);
  inherited Destroy;
end;

procedure TC40_Var_Client.Progress;
begin
  inherited Progress;
end;

function TC40_Var_Client.GetNM(Name_: U_String): TC40_Var_Service_NM_Pool;
begin
  Result := NMBigPool[Name_];
  if Result = nil then
    begin
      Result := TC40_Var_Service_NM_Pool.Create;
      Result.Name := Name_;
      Result.Client := self;
      NMBigPool.FastAdd(Name_, Result);
    end;
end;

procedure TC40_Var_Client.NM_Init(Name_: U_String; Open_: Boolean; NMPool_: TNumberModulePool);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(Name_);
  d.WriteBool(Open_);
  d.WriteNMPool(NMPool_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('NM_Init', d);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_InitAsTemp(Name_: U_String; TimeOut_: TTimeTick; Open_: Boolean; NMPool_: TNumberModulePool);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(Name_);
  d.WriteUInt64(TimeOut_);
  d.WriteBool(Open_);
  d.WriteNMPool(NMPool_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('NM_InitAsTemp', d);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_Remove(Name_: U_String; RemoveLocal: Boolean);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(Name_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('NM_Remove', d);
  DisposeObject(d);

  if RemoveLocal and Assigned(OnRemove) then
    if NMBigPool.Exists(Name_) then
        OnRemove(self, Name_);
  NMBigPool.Delete(Name_);
end;

procedure TC40_Var_Client.NM_RemoveKey(Name_, KeyName_: U_String; RemoveLocal: Boolean);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(Name_);
  d.WriteString(KeyName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('NM_RemoveKey', d);
  DisposeObject(d);
  if RemoveLocal and NMBigPool.Exists(Name_) then
      NMBigPool[Name_].Delete(KeyName_);
end;

procedure TC40_Var_Client.NM_Get_Bridge(arry: U_StringArray; Bridge_IO_: TPeerIO);
var
  d: TDFE;
  i: Integer;
begin
  d := TDFE.Create;
  for i := 0 to length(arry) - 1 do
      d.WriteString(arry[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Get', d, TStream_Event_Bridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_GetC(arry: U_StringArray; OnResult: TC40_Var_Client_NM_GetC);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_Get;
begin
  d := TDFE.Create;
  for i := 0 to length(arry) - 1 do
      d.WriteString(arry[i]);
  tmp := TC40_Var_Client_NM_Get.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Get', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_GetM(arry: U_StringArray; OnResult: TC40_Var_Client_NM_GetM);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_Get;
begin
  d := TDFE.Create;
  for i := 0 to length(arry) - 1 do
      d.WriteString(arry[i]);
  tmp := TC40_Var_Client_NM_Get.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Get', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_GetP(arry: U_StringArray; OnResult: TC40_Var_Client_NM_GetP);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_Get;
begin
  d := TDFE.Create;
  for i := 0 to length(arry) - 1 do
      d.WriteString(arry[i]);
  tmp := TC40_Var_Client_NM_Get.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Get', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_GetValue_Bridge(NMName_: U_String; ValueNames_: U_StringArray; Bridge_IO_: TPeerIO);
var
  d: TDFE;
  i: Integer;
begin
  d := TDFE.Create;
  d.WriteString(NMName_);
  for i := 0 to length(ValueNames_) - 1 do
      d.WriteString(ValueNames_[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_GetValue', d, TStream_Event_Bridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_GetValueC(NMName_: U_String; ValueNames_: U_StringArray; OnResult: TC40_Var_Client_NM_GetValueC);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_GetValue;
begin
  d := TDFE.Create;
  d.WriteString(NMName_);
  for i := 0 to length(ValueNames_) - 1 do
      d.WriteString(ValueNames_[i]);
  tmp := TC40_Var_Client_NM_GetValue.Create;
  tmp.Client := self;
  tmp.NM_Name := NMName_;
  tmp.OnResultC := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_GetValue', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_GetValueM(NMName_: U_String; ValueNames_: U_StringArray; OnResult: TC40_Var_Client_NM_GetValueM);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_GetValue;
begin
  d := TDFE.Create;
  d.WriteString(NMName_);
  for i := 0 to length(ValueNames_) - 1 do
      d.WriteString(ValueNames_[i]);
  tmp := TC40_Var_Client_NM_GetValue.Create;
  tmp.Client := self;
  tmp.NM_Name := NMName_;
  tmp.OnResultM := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_GetValue', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_GetValueP(NMName_: U_String; ValueNames_: U_StringArray; OnResult: TC40_Var_Client_NM_GetValueP);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_GetValue;
begin
  d := TDFE.Create;
  d.WriteString(NMName_);
  for i := 0 to length(ValueNames_) - 1 do
      d.WriteString(ValueNames_[i]);
  tmp := TC40_Var_Client_NM_GetValue.Create;
  tmp.Client := self;
  tmp.NM_Name := NMName_;
  tmp.OnResultP := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_GetValue', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_Open_Bridge(arry: U_StringArray; Bridge_IO_: TPeerIO);
var
  d: TDFE;
  i: Integer;
begin
  d := TDFE.Create;
  for i := 0 to length(arry) - 1 do
      d.WriteString(arry[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Open', d, TStream_Event_Bridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_OpenC(arry: U_StringArray; OnResult: TC40_Var_Client_NM_OpenC);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_Open;
begin
  d := TDFE.Create;
  for i := 0 to length(arry) - 1 do
      d.WriteString(arry[i]);
  tmp := TC40_Var_Client_NM_Open.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Open', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_OpenM(arry: U_StringArray; OnResult: TC40_Var_Client_NM_OpenM);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_Open;
begin
  d := TDFE.Create;
  for i := 0 to length(arry) - 1 do
      d.WriteString(arry[i]);
  tmp := TC40_Var_Client_NM_Open.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Open', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_OpenP(arry: U_StringArray; OnResult: TC40_Var_Client_NM_OpenP);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_Open;
begin
  d := TDFE.Create;
  for i := 0 to length(arry) - 1 do
      d.WriteString(arry[i]);
  tmp := TC40_Var_Client_NM_Open.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Open', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_Close(arry: U_StringArray; RemoveLocal: Boolean);
var
  d: TDFE;
  i: Integer;
begin
  d := TDFE.Create;
  for i := 0 to length(arry) - 1 do
      d.WriteString(arry[i]);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('NM_Close', d);
  DisposeObject(d);

  if RemoveLocal then
    for i := 0 to length(arry) - 1 do
      begin
        if Assigned(OnRemove) then
          if NMBigPool.Exists(arry[i]) then
              OnRemove(self, arry[i]);
        NMBigPool.Delete(arry[i])
      end;
end;

procedure TC40_Var_Client.NM_CloseAll(RemoveLocal: Boolean);
begin
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('NM_CloseAll');
  if RemoveLocal then
      NMBigPool.Clear;
end;

procedure TC40_Var_Client.NM_Change(NMName_, ValueName_: U_String; Variant_: Variant);
var
  d: TDFE;
  NM: TC40_Var_Service_NM_Pool;
begin
  d := TDFE.Create;
  d.WriteString(NMName_);
  d.WriteString(ValueName_);
  d.WriteVariant(Variant_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('NM_Change', d);
  DisposeObject(d);
  NM := GetNM(NMName_);
  NM[ValueName_].AsValue := Variant_;
end;

procedure TC40_Var_Client.NM_Keep(NMName_: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(NMName_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('NM_Keep', d);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_Script_Bridge(NMName_: U_String; ExpressionTexts_: U_StringArray; Bridge_IO_: TPeerIO);
var
  d: TDFE;
  i: Integer;
begin
  d := TDFE.Create;
  d.WriteString(NMName_);
  for i := 0 to length(ExpressionTexts_) - 1 do
      d.WriteString(ExpressionTexts_[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Script', d, TStream_Event_Bridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_ScriptC(NMName_: U_String; ExpressionTexts_: U_StringArray; OnResult: TC40_Var_Client_NM_ScriptC);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_Script;
begin
  d := TDFE.Create;
  d.WriteString(NMName_);
  for i := 0 to length(ExpressionTexts_) - 1 do
      d.WriteString(ExpressionTexts_[i]);
  tmp := TC40_Var_Client_NM_Script.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Script', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_ScriptM(NMName_: U_String; ExpressionTexts_: U_StringArray; OnResult: TC40_Var_Client_NM_ScriptM);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_Script;
begin
  d := TDFE.Create;
  d.WriteString(NMName_);
  for i := 0 to length(ExpressionTexts_) - 1 do
      d.WriteString(ExpressionTexts_[i]);
  tmp := TC40_Var_Client_NM_Script.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Script', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_ScriptP(NMName_: U_String; ExpressionTexts_: U_StringArray; OnResult: TC40_Var_Client_NM_ScriptP);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_Script;
begin
  d := TDFE.Create;
  d.WriteString(NMName_);
  for i := 0 to length(ExpressionTexts_) - 1 do
      d.WriteString(ExpressionTexts_[i]);
  tmp := TC40_Var_Client_NM_Script.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Script', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_Save;
begin
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('NM_Save');
end;

procedure TC40_Var_Client.NM_Search_Bridge(filter: U_String; MaxNum: Integer; AutoOpen: Boolean; Bridge_IO_: TPeerIO);
var
  d: TDFE;
  i: Integer;
begin
  d := TDFE.Create;
  d.WriteString(filter);
  d.WriteInteger(MaxNum);
  d.WriteBool(AutoOpen);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Search', d, TStream_Event_Bridge.Create(Bridge_IO_).DoStreamEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_SearchC(filter: U_String; MaxNum: Integer; AutoOpen: Boolean; OnResult: TC40_Var_Client_NM_SearchC);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_Search;
begin
  d := TDFE.Create;
  d.WriteString(filter);
  d.WriteInteger(MaxNum);
  d.WriteBool(AutoOpen);
  tmp := TC40_Var_Client_NM_Search.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Search', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_SearchM(filter: U_String; MaxNum: Integer; AutoOpen: Boolean; OnResult: TC40_Var_Client_NM_SearchM);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_Search;
begin
  d := TDFE.Create;
  d.WriteString(filter);
  d.WriteInteger(MaxNum);
  d.WriteBool(AutoOpen);
  tmp := TC40_Var_Client_NM_Search.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Search', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_SearchP(filter: U_String; MaxNum: Integer; AutoOpen: Boolean; OnResult: TC40_Var_Client_NM_SearchP);
var
  d: TDFE;
  i: Integer;
  tmp: TC40_Var_Client_NM_Search;
begin
  d := TDFE.Create;
  d.WriteString(filter);
  d.WriteInteger(MaxNum);
  d.WriteBool(AutoOpen);
  tmp := TC40_Var_Client_NM_Search.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NM_Search', d, nil, nil,
    tmp.DoStreamParamEvent, tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Var_Client.NM_SearchAndRunScript(filter: U_String; ExpressionTexts_: U_StringArray);
var
  d: TDFE;
  i: Integer;
begin
  d := TDFE.Create;
  d.WriteString(filter);
  for i := 0 to length(ExpressionTexts_) - 1 do
      d.WriteString(ExpressionTexts_[i]);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('NM_SearchAndRunScript', d);
  DisposeObject(d);
end;

initialization

RegisterC40('Var', TC40_Var_Service, TC40_Var_Client);

end.
