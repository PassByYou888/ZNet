{ ****************************************************************************** }
{ * cloud 4.0 log database                                                     * }
{ ****************************************************************************** }
unit Z.Net.C4_Log_DB;

{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine,
  Z.Geometry2D, Z.DFE, Z.Json, Z.Expression,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.ZDB2, Z.ZDB2.HS, Z.HashList.Templet,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth, Z.Net.C4;

type
  TC40_Log_DB_Service = class;

  TC40_Log_DB_ZDB2_HashString = class(TZDB2_List_HashString)
  protected
    Name: U_String;
    LastActivtTime: TTimeTick;
    LastPostTime: TDateTime;
    FileName: U_String;
  end;

  TLog_DB_Pool = {$IFDEF FPC}specialize {$ENDIF FPC}TGeneric_String_Object_Hash<TC40_Log_DB_ZDB2_HashString>;
  TLog_DB_List = {$IFDEF FPC}specialize {$ENDIF FPC}TBigList<TC40_Log_DB_ZDB2_HashString>;

  TC40_Log_DB_Service_RecvTunnel_NoAuth = class(TService_RecvTunnel_UserDefine_NoAuth)
  public
    Log_DB_Service: TC40_Log_DB_Service;
    Sync_Log: Boolean;
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TC40_Log_DB_Service = class(TC40_Base_NoAuth_Service)
  protected
    procedure DoLinkSuccess_Event(sender: TDTService_NoAuth; UserDefineIO: TService_RecvTunnel_UserDefine_NoAuth); override;
    procedure DoUserOut_Event(sender: TDTService_NoAuth; UserDefineIO: TService_RecvTunnel_UserDefine_NoAuth); override;
  private
    procedure cmd_PostLog(sender: TPeerIO; InData: TDFE);
    procedure cmd_QueryLog(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_QueryAndRemoveLog(sender: TPeerIO; InData: TDFE);
    procedure cmd_RemoveLog(sender: TPeerIO; InData: TDFE);
    procedure cmd_GetLogDB(sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_CloseDB(sender: TPeerIO; InData: TDFE);
    procedure cmd_RemoveDB(sender: TPeerIO; InData: TDFE);
    procedure cmd_Enabled_LogMonitor(sender: TPeerIO; InData: TDFE);
  private
    WaitFreeList: TLog_DB_List;
    procedure Do_Create_ZDB2_HashString(sender: TZDB2_List_HashString; Obj: TZDB2_HashString);
    procedure Do_DB_Pool_SafeCheck(const Name_: PSystemString; Obj_: TC40_Log_DB_ZDB2_HashString);
    procedure Do_DB_Pool_Progress(const Name_: PSystemString; Obj_: TC40_Log_DB_ZDB2_HashString);
  public
    C40_DB_Directory: U_String;
    DB_Pool: TLog_DB_Pool;
    LogDBRecycleMemoryTimeOut: TTimeTick;
    ZDB2RecycleMemoryTimeOut: TTimeTick;
    ZDB2DeltaSpace: Int64;
    ZDB2BlockSize: Word;
    ZDB2EnabledCipher: Boolean;
    ZDB2CipherName: U_String;
    ZDB2Password: U_String;
    ZDB2Cipher: TZDB2_Cipher;

    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;

    function GetDB(const LogDB: SystemString): TC40_Log_DB_ZDB2_HashString;
    procedure PostLog(const LogDB, Log1_, Log2_: SystemString);
  end;

{$REGION 'Client_Define_And_Bridge'}

  TLogData__ = record
  private
    FLogDB_Index: Integer;
  public
    LogDB: SystemString;
    LogTime: TDateTime;
    Log1, Log2: SystemString;
    Index: Integer;
    property LogDB_Index: Integer read FLogDB_Index;
  end;

  PLogData__ = ^TLogData__;

  TArrayLogData = array of TLogData__;

  TLogData_List_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PLogData__>;

  TLogData_List = class(TLogData_List_Decl)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Remove(p: PLogData__);
    procedure Delete(Index: Integer);
    procedure Clear;
    procedure AddP(LData: TLogData__);
    procedure AddArry(arry: TArrayLogData);
    procedure SortByTime();
  end;

  TC40_Log_DB_Client = class;

  TON_QueryLogC = procedure(sender: TC40_Log_DB_Client; LogDB: SystemString; arry: TArrayLogData);
  TON_QueryLogM = procedure(sender: TC40_Log_DB_Client; LogDB: SystemString; arry: TArrayLogData) of object;
{$IFDEF FPC}
  TON_QueryLogP = procedure(sender: TC40_Log_DB_Client; LogDB: SystemString; arry: TArrayLogData) is nested;
{$ELSE FPC}
  TON_QueryLogP = reference to procedure(sender: TC40_Log_DB_Client; LogDB: SystemString; arry: TArrayLogData);
{$ENDIF FPC}

  TON_QueryLog = class(TOnResultBridge)
  public
    Client: TC40_Log_DB_Client;
    LogDB: SystemString;
    OnResultC: TON_QueryLogC;
    OnResultM: TON_QueryLogM;
    OnResultP: TON_QueryLogP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDFE); override;
  end;

  TON_GetLogDBC = procedure(sender: TC40_Log_DB_Client; arry: U_StringArray);
  TON_GetLogDBM = procedure(sender: TC40_Log_DB_Client; arry: U_StringArray) of object;
{$IFDEF FPC}
  TON_GetLogDBP = procedure(sender: TC40_Log_DB_Client; arry: U_StringArray) is nested;
{$ELSE FPC}
  TON_GetLogDBP = reference to procedure(sender: TC40_Log_DB_Client; arry: U_StringArray);
{$ENDIF FPC}

  TON_GetLogDB = class(TOnResultBridge)
  public
    Client: TC40_Log_DB_Client;
    OnResultC: TON_GetLogDBC;
    OnResultM: TON_GetLogDBM;
    OnResultP: TON_GetLogDBP;
    constructor Create;
    procedure DoStreamEvent(sender: TPeerIO; Result_: TDFE); override;
  end;
{$ENDREGION 'Client_Define_And_Bridge'}

  I_ON_C40_Log_DB_Client_Interface = interface
    procedure Do_Sync_Log(LogDB, Log1_, Log2_: SystemString);
  end;

  TC40_Log_DB_Client = class(TC40_Base_NoAuth_Client)
  private
    procedure cmd_Log(sender: TPeerIO; InData: TDFE);
  public
    ON_C40_Log_DB_Client_Interface: I_ON_C40_Log_DB_Client_Interface;
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    // post log
    procedure PostLog(LogDB, Log1_, Log2_: SystemString); overload;
    procedure PostLog(LogDB, Log_: SystemString); overload;
    // query from time+filter
    procedure QueryLog_Bridge(LogDB: SystemString; bTime, eTime: TDateTime; filter1, filter2: U_String; Bridge_IO_: TPeerIO); overload;
    procedure QueryLogC(LogDB: SystemString; bTime, eTime: TDateTime; filter1, filter2: U_String; OnResult: TON_QueryLogC); overload;
    procedure QueryLogM(LogDB: SystemString; bTime, eTime: TDateTime; filter1, filter2: U_String; OnResult: TON_QueryLogM); overload;
    procedure QueryLogP(LogDB: SystemString; bTime, eTime: TDateTime; filter1, filter2: U_String; OnResult: TON_QueryLogP); overload;
    // query from time
    procedure QueryLog_Bridge(LogDB: SystemString; bTime, eTime: TDateTime; Bridge_IO_: TPeerIO); overload;
    procedure QueryLogC(LogDB: SystemString; bTime, eTime: TDateTime; OnResult: TON_QueryLogC); overload;
    procedure QueryLogM(LogDB: SystemString; bTime, eTime: TDateTime; OnResult: TON_QueryLogM); overload;
    procedure QueryLogP(LogDB: SystemString; bTime, eTime: TDateTime; OnResult: TON_QueryLogP); overload;
    // query and remove
    procedure QueryAndRemoveLog(LogDB: SystemString; bTime, eTime: TDateTime; filter1, filter2: U_String); overload;
    procedure QueryAndRemoveLog(LogDB: SystemString; bTime, eTime: TDateTime); overload;
    // remove
    procedure RemoveLog(LogDB: SystemString; arry_index: array of Integer);
    // get db list
    procedure GetLogDB_Bridge(Bridge_IO_: TPeerIO);
    procedure GetLogDBC(OnResult: TON_GetLogDBC);
    procedure GetLogDBM(OnResult: TON_GetLogDBM);
    procedure GetLogDBP(OnResult: TON_GetLogDBP);
    // close log db
    procedure CloseDB(LogDB: SystemString);
    // remove log db
    procedure RemoveDB(LogDB: SystemString);
    // log monitor
    procedure Enabled_LogMonitor(Sync_Log: Boolean);
  end;

  TC40_Log_DB_Client_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TC40_Log_DB_Client>;

function MakeNowDateStr(): SystemString;

implementation

uses SysUtils, DateUtils, Math;

function MakeNowDateStr(): SystemString;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Date, Year, Month, Day);
  Result := Format('%d_%d_%d', [Year, Month, Day]);
end;

procedure SortLog(var L_: TArrayLogData);
  function Compare_(var Left, Right: TLogData__): ShortInt;
  begin
    Result := CompareDateTime(Left.LogTime, Right.LogTime);
  end;

  procedure fastSort_(var Arry_: TArrayLogData; L, R: Integer);
  var
    i, j: Integer;
    p: ^TLogData__;
    tmp: TLogData__;
  begin
    repeat
      i := L;
      j := R;
      p := @Arry_[(L + R) shr 1];
      repeat
        while Compare_(Arry_[i], p^) < 0 do
            inc(i);
        while Compare_(Arry_[j], p^) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                tmp := Arry_[i];
                Arry_[i] := Arry_[j];
                Arry_[j] := tmp;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          fastSort_(Arry_, L, j);
      L := i;
    until i >= R;
  end;

begin
  if length(L_) > 1 then
      fastSort_(L_, 0, length(L_) - 1);
end;

constructor TC40_Log_DB_Service_RecvTunnel_NoAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  Log_DB_Service := nil;
  Sync_Log := False;
end;

destructor TC40_Log_DB_Service_RecvTunnel_NoAuth.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_Log_DB_Service.DoLinkSuccess_Event(sender: TDTService_NoAuth; UserDefineIO: TService_RecvTunnel_UserDefine_NoAuth);
var
  IO_Def: TC40_Log_DB_Service_RecvTunnel_NoAuth;
begin
  inherited DoLinkSuccess_Event(sender, UserDefineIO);
  IO_Def := TC40_Log_DB_Service_RecvTunnel_NoAuth(UserDefineIO);
  IO_Def.Log_DB_Service := self;
end;

procedure TC40_Log_DB_Service.DoUserOut_Event(sender: TDTService_NoAuth; UserDefineIO: TService_RecvTunnel_UserDefine_NoAuth);
begin
  inherited DoUserOut_Event(sender, UserDefineIO);
end;

procedure TC40_Log_DB_Service.cmd_PostLog(sender: TPeerIO; InData: TDFE);
var
  LogDB, Log1_, Log2_: SystemString;
  Arry_: TIO_Array;
  ID_: Cardinal;
  IO_Def: TC40_Log_DB_Service_RecvTunnel_NoAuth;
begin
  LogDB := InData.R.ReadString;
  Log1_ := InData.R.ReadString;
  Log2_ := InData.R.ReadString;
  PostLog(LogDB, Log1_, Log2_);

  DTNoAuthService.RecvTunnel.GetIO_Array(Arry_);
  for ID_ in Arry_ do
    begin
      IO_Def := DTNoAuthService.RecvTunnel[ID_].UserDefine as TC40_Log_DB_Service_RecvTunnel_NoAuth;
      if IO_Def.LinkOk and IO_Def.Sync_Log then
          IO_Def.SendTunnel.Owner.SendDirectStreamCmd('Log', InData);
    end;
end;

procedure TC40_Log_DB_Service.cmd_QueryLog(sender: TPeerIO; InData, OutData: TDFE);
var
  LogDB: SystemString;
  bTime, eTime: TDateTime;
  filter1, filter2: U_String;
  db_: TC40_Log_DB_ZDB2_HashString;
  d_: TDateTime;
  dt: SystemString;
  n1, n2: SystemString;
  Data_is_Null_: Boolean;
begin
  LogDB := InData.R.ReadString;
  bTime := InData.R.ReadDouble;
  eTime := InData.R.ReadDouble;
  filter1 := InData.R.ReadString;
  filter2 := InData.R.ReadString;

  db_ := GetDB(LogDB);
  if db_ = nil then
      exit;

  dt := umlDateTimeToStr(umlNow);

  if db_.Count > 0 then
    with db_.Repeat_ do
      repeat
        try
          Data_is_Null_ := Queue^.Data.Data_Direct = nil;
          d_ := umlStrToDateTime(Queue^.Data.Data.GetDefaultValue('Time', dt));
          if DateTimeInRange(d_, bTime, eTime) then
            begin
              n1 := Queue^.Data.Data.GetDefaultValue('Log1', '');
              n2 := Queue^.Data.Data.GetDefaultValue('Log2', '');
              if umlSearchMatch(filter1, n1) and umlSearchMatch(filter2, n2) then
                begin
                  OutData.WriteDouble(d_);
                  OutData.WriteString(n1);
                  OutData.WriteString(n2);
                  OutData.WriteInteger(I__);
                end;
            end;
          if Data_is_Null_ then
              Queue^.Data.RecycleMemory;
        except
        end;
      until not Next;
end;

procedure TC40_Log_DB_Service.cmd_QueryAndRemoveLog(sender: TPeerIO; InData: TDFE);
var
  LogDB: SystemString;
  bTime, eTime: TDateTime;
  filter1, filter2: U_String;
  db_: TC40_Log_DB_ZDB2_HashString;
  i: Integer;
  d_: TDateTime;
  dt: SystemString;
  n1, n2: SystemString;
  Data_is_Null_: Boolean;
begin
  LogDB := InData.R.ReadString;
  bTime := InData.R.ReadDouble;
  eTime := InData.R.ReadDouble;
  filter1 := InData.R.ReadString;
  filter2 := InData.R.ReadString;

  db_ := GetDB(LogDB);
  if db_ = nil then
      exit;

  dt := umlDateTimeToStr(umlNow);

  if db_.Count > 0 then
    with db_.Repeat_ do
      repeat
        try
          Data_is_Null_ := Queue^.Data.Data_Direct = nil;
          d_ := umlStrToDateTime(Queue^.Data.Data.GetDefaultValue('Time', dt));
          if DateTimeInRange(d_, bTime, eTime) then
            begin
              n1 := Queue^.Data.Data.GetDefaultValue('Log1', '');
              n2 := Queue^.Data.Data.GetDefaultValue('Log2', '');
              if umlSearchMatch(filter1, n1) and umlSearchMatch(filter2, n2) then
                  db_.Push_To_Recycle_Pool(Queue^.Data, True);
            end;
          if Data_is_Null_ then
              Queue^.Data.RecycleMemory;
        except
        end;
      until not Next;
  db_.Free_Recycle_Pool;
end;

procedure TC40_Log_DB_Service.cmd_RemoveLog(sender: TPeerIO; InData: TDFE);
var
  LogDB: SystemString;
  arry: TDFArrayInteger;
  db_: TC40_Log_DB_ZDB2_HashString;
  i: Integer;
begin
  LogDB := InData.R.ReadString;
  arry := InData.R.ReadArrayInteger;

  db_ := GetDB(LogDB);
  if db_ = nil then
      exit;

  if db_.Count > 0 then
    with db_.Repeat_ do
      repeat
        for i := 0 to arry.Count - 1 do
          if arry[i] = I__ then
              db_.Push_To_Recycle_Pool(Queue^.Data, True);
      until not Next;
  db_.Free_Recycle_Pool;
end;

procedure TC40_Log_DB_Service.cmd_GetLogDB(sender: TPeerIO; InData, OutData: TDFE);
var
  fArry: U_StringArray;
  fn: U_SystemString;
begin
  fArry := umlGet_File_Full_Array(C40_DB_Directory);
  for fn in fArry do
    if umlMultipleMatch(True, '*.Log_ZDB2', fn) then
        OutData.WriteString(umlChangeFileExt(umlGetFileName(fn), ''));
  SetLength(fArry, 0);
end;

procedure TC40_Log_DB_Service.cmd_CloseDB(sender: TPeerIO; InData: TDFE);
var
  LogDB: SystemString;
begin
  LogDB := InData.R.ReadString;
  DB_Pool.Delete(LogDB);
end;

procedure TC40_Log_DB_Service.cmd_RemoveDB(sender: TPeerIO; InData: TDFE);
var
  LogDB: SystemString;
  fn: U_String;
begin
  LogDB := InData.R.ReadString;
  DB_Pool.Delete(LogDB);
  fn := umlCombineFileName(C40_DB_Directory, umlConverStrToFileName(LogDB).Text + '.Log_ZDB2');
  if umlFileExists(fn) then
      umlDeleteFile(fn);
end;

procedure TC40_Log_DB_Service.cmd_Enabled_LogMonitor(sender: TPeerIO; InData: TDFE);
var
  IO_Def: TC40_Log_DB_Service_RecvTunnel_NoAuth;
begin
  IO_Def := DTNoAuthService.GetUserDefineRecvTunnel(sender) as TC40_Log_DB_Service_RecvTunnel_NoAuth;
  if not IO_Def.LinkOk then
      exit;
  IO_Def.Sync_Log := InData.R.ReadBool;
end;

procedure TC40_Log_DB_Service.Do_Create_ZDB2_HashString(sender: TZDB2_List_HashString; Obj: TZDB2_HashString);
begin

end;

procedure TC40_Log_DB_Service.Do_DB_Pool_SafeCheck(const Name_: PSystemString; Obj_: TC40_Log_DB_ZDB2_HashString);
begin
  Obj_.Flush;
end;

procedure TC40_Log_DB_Service.Do_DB_Pool_Progress(const Name_: PSystemString; Obj_: TC40_Log_DB_ZDB2_HashString);
begin
  Obj_.Progress;
  if GetTimeTick - Obj_.LastActivtTime > LogDBRecycleMemoryTimeOut then
      WaitFreeList.Add(Obj_);
end;

constructor TC40_Log_DB_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  // custom
  DTNoAuth.RecvTunnel.PeerClientUserDefineClass := TC40_Log_DB_Service_RecvTunnel_NoAuth;

  DTNoAuthService.RecvTunnel.SendDataCompressed := True;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('PostLog').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_PostLog;
  DTNoAuthService.RecvTunnel.RegisterStream('QueryLog').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_QueryLog;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('QueryAndRemoveLog').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_QueryAndRemoveLog;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('RemoveLog').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemoveLog;
  DTNoAuthService.RecvTunnel.RegisterStream('GetLogDB').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetLogDB;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('CloseDB').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_CloseDB;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('RemoveDB').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemoveDB;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('Enabled_LogMonitor').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Enabled_LogMonitor;
  Service.QuietMode := True;
  // is only instance
  ServiceInfo.OnlyInstance := True;
  UpdateToGlobalDispatch;
  ParamList.SetDefaultValue('OnlyInstance', if_(ServiceInfo.OnlyInstance, 'True', 'False'));

  WaitFreeList := TLog_DB_List.Create;
  C40_DB_Directory := umlCombinePath(DTNoAuthService.PublicFileDirectory, Get_DB_FileName_Config(PFormat('DTC40_%s', [ServiceInfo.ServiceTyp.Text])));
  umlCreateDirectory(C40_DB_Directory);

  DB_Pool := TLog_DB_Pool.Create(True,
    EStrToInt64(ParamList.GetDefaultValue('Log_DB_HashPool', '1024*1024'), 1024 * 1024),
    nil);

  LogDBRecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('LogDBRecycleMemory', '60*1000'), 60 * 1000);
  ZDB2RecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('RecycleMemory', '5*1000'), 5 * 1000);
  ZDB2DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('DeltaSpace', '1*1024*1024'), 1 * 1024 * 1024);
  ZDB2BlockSize := EStrToInt(ParamList.GetDefaultValue('BlockSize', '100'), 100);
  ZDB2EnabledCipher := EStrToBool(ParamList.GetDefaultValue('EnabledCipher', 'False'), False);
  ZDB2CipherName := ParamList.GetDefaultValue('Cipher', TCipher.CCipherSecurityName[TCipherSecurity.csNone]);
  ZDB2Password := ParamList.GetDefaultValue('Password', Z.Net.C4.C40_Password);
  if ZDB2EnabledCipher then
      ZDB2Cipher := TZDB2_Cipher.Create(ZDB2CipherName, ZDB2Password, 1, True, True)
  else
      ZDB2Cipher := nil;
end;

destructor TC40_Log_DB_Service.Destroy;
begin
  disposeObject(WaitFreeList);
  disposeObject(DB_Pool);
  DisposeObjectAndNil(ZDB2Cipher);
  inherited Destroy;
end;

procedure TC40_Log_DB_Service.SafeCheck;
begin
  inherited SafeCheck;
  DB_Pool.ProgressM({$IFDEF FPC}@{$ENDIF FPC}Do_DB_Pool_SafeCheck);
end;

procedure TC40_Log_DB_Service.Progress;
begin
  DB_Pool.ProgressM({$IFDEF FPC}@{$ENDIF FPC}Do_DB_Pool_Progress);

  while WaitFreeList.Count > 0 do
    begin
      try
        if not C40_QuietMode then
            DoStatus('recycle Memory, Log Database: %s', [WaitFreeList.First^.Data.Name.Text]);
        DB_Pool.Delete(WaitFreeList.First^.Data.Name);
      except
      end;
      WaitFreeList.Next;
    end;

  inherited Progress;
end;

function TC40_Log_DB_Service.GetDB(const LogDB: SystemString): TC40_Log_DB_ZDB2_HashString;
var
  fn: U_String;
  fs: TCore_FileStream;
  LogDB_: U_String;
begin
  LogDB_ := umlConverStrToFileName(LogDB);

  Result := DB_Pool[LogDB_];
  if Result = nil then
    begin
      fn := umlCombineFileName(C40_DB_Directory, LogDB_.Text + '.Log_ZDB2');
      try
        if EStrToBool(ParamList.GetDefaultValue('ForeverSave', 'True'), True) and umlFileExists(fn) then
            fs := TCore_FileStream.Create(fn, fmOpenReadWrite)
        else
            fs := TCore_FileStream.Create(fn, fmCreate);
        Result := TC40_Log_DB_ZDB2_HashString.Create(
          TZDB2_HashString,
{$IFDEF FPC}@{$ENDIF FPC}Do_Create_ZDB2_HashString,
          ZDB2RecycleMemoryTimeOut,
          fs,
          False,
          ZDB2DeltaSpace,
          ZDB2BlockSize,
          ZDB2Cipher);
        Result.CoreSpace.MaxCacheMemory := 1024 * 1024;
        Result.AutoFreeStream := True;
        Result.Name := LogDB_;
        Result.LastActivtTime := GetTimeTick;
        Result.LastPostTime := umlNow;
        Result.FileName := fn;
        DB_Pool.FastAdd(LogDB_, Result);
      except
          Result := nil;
      end;
    end
  else if Result <> nil then
      Result.LastActivtTime := GetTimeTick;
end;

procedure TC40_Log_DB_Service.PostLog(const LogDB, Log1_, Log2_: SystemString);
var
  db_: TC40_Log_DB_ZDB2_HashString;
  hs_: TZDB2_HashString;
  t_: TDateTime;
begin
  db_ := GetDB(LogDB);
  if db_ = nil then
      exit;
  hs_ := db_.NewData;

  t_ := umlNow;
  db_.LastPostTime := t_;

  hs_.Data['Time'] := umlDateTimeToStr(t_);
  if Log1_ <> '' then
      hs_.Data['Log1'] := Log1_;
  if Log2_ <> '' then
      hs_.Data['Log2'] := Log2_;
end;

constructor TLogData_List.Create;
begin
  inherited Create;
end;

destructor TLogData_List.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLogData_List.Remove(p: PLogData__);
begin
  Dispose(p);
  inherited Remove(p);
end;

procedure TLogData_List.Delete(Index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      Dispose(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TLogData_List.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Dispose(Items[i]);
  inherited Clear;
end;

procedure TLogData_List.AddP(LData: TLogData__);
var
  p: PLogData__;
begin
  new(p);
  p^ := LData;
  p^.Index := inherited Add(p);
end;

procedure TLogData_List.AddArry(arry: TArrayLogData);
var
  i: Integer;
  p: PLogData__;
begin
  for i := low(arry) to high(arry) do
    begin
      new(p);
      p^ := arry[i];
      p^.Index := inherited Add(p);
    end;
end;

procedure TLogData_List.SortByTime;
  function Compare_(L, R: PLogData__): ShortInt;
  begin
    Result := CompareDateTime(L^.LogTime, R^.LogTime);
    if Result = 0 then
        Result := CompareValue(L^.Index, R^.Index);
  end;

  procedure fastSort_(L, R: Integer);
  var
    i, j: Integer;
    p: PLogData__;
  begin
    repeat
      i := L;
      j := R;
      p := Items[(L + R) shr 1];
      repeat
        while Compare_(Items[i], p) < 0 do
            inc(i);
        while Compare_(Items[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
                Exchange(i, j);
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          fastSort_(L, j);
      L := i;
    until i >= R;
  end;

begin
  if Count > 1 then
      fastSort_(0, Count - 1);
end;

constructor TON_QueryLog.Create;
begin
  inherited Create;
  Client := nil;
  LogDB := '';
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_QueryLog.DoStreamEvent(sender: TPeerIO; Result_: TDFE);
var
  arry: TArrayLogData;
  i: Integer;
begin
  SetLength(arry, Result_.Count shr 2);
  i := 0;
  while Result_.R.NotEnd do
    begin
      arry[i].LogDB := LogDB;
      arry[i].LogTime := Result_.R.ReadDouble;
      arry[i].Log1 := Result_.R.ReadString;
      arry[i].Log2 := Result_.R.ReadString;
      arry[i].FLogDB_Index := Result_.R.ReadInteger;
      arry[i].Index := arry[i].FLogDB_Index;
      inc(i);
    end;

  SortLog(arry);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, LogDB, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, LogDB, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, LogDB, arry);
  except
  end;
  SetLength(arry, 0);
  DelayFreeObject(1.0, self);
end;

constructor TON_GetLogDB.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_GetLogDB.DoStreamEvent(sender: TPeerIO; Result_: TDFE);
var
  arry: U_StringArray;
  i: Integer;
begin
  SetLength(arry, Result_.Count);
  for i := 0 to Result_.Count - 1 do
      arry[i] := Result_.ReadString(i);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  SetLength(arry, 0);
  DelayFreeObject(1.0, self);
end;

procedure TC40_Log_DB_Client.cmd_Log(sender: TPeerIO; InData: TDFE);
var
  LogDB, Log1_, Log2_: SystemString;
begin
  LogDB := InData.R.ReadString;
  Log1_ := InData.R.ReadString;
  Log2_ := InData.R.ReadString;
  if Assigned(ON_C40_Log_DB_Client_Interface) then
      ON_C40_Log_DB_Client_Interface.Do_Sync_Log(LogDB, Log1_, Log2_);
end;

constructor TC40_Log_DB_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  Client.QuietMode := True;
  DTNoAuthClient.RecvTunnel.RegisterDirectStream('Log').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Log;
  ON_C40_Log_DB_Client_Interface := nil;
end;

destructor TC40_Log_DB_Client.Destroy;
begin
  ON_C40_Log_DB_Client_Interface := nil;
  inherited Destroy;
end;

procedure TC40_Log_DB_Client.PostLog(LogDB, Log1_, Log2_: SystemString);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(LogDB);
  d.WriteString(Log1_);
  d.WriteString(Log2_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('PostLog', d);
  disposeObject(d);
  if not C40_QuietMode then
    if Log2_ <> '' then
        DoStatus('Log1:%s Log2:%s -> %s', [Log1_, Log2_, LogDB])
    else
        DoStatus('Log:%s -> %s', [Log1_, LogDB]);
end;

procedure TC40_Log_DB_Client.PostLog(LogDB, Log_: SystemString);
begin
  PostLog(LogDB, Log_, '');
end;

procedure TC40_Log_DB_Client.QueryLog_Bridge(LogDB: SystemString; bTime, eTime: TDateTime; filter1, filter2: U_String; Bridge_IO_: TPeerIO);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(LogDB);
  d.WriteDouble(bTime);
  d.WriteDouble(eTime);
  d.WriteString(filter1);
  d.WriteString(filter2);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('QueryLog', d, {$IFDEF FPC}@{$ENDIF FPC}TStreamEventBridge.Create(Bridge_IO_).DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_Log_DB_Client.QueryLogC(LogDB: SystemString; bTime, eTime: TDateTime; filter1, filter2: U_String; OnResult: TON_QueryLogC);
var
  tmp: TON_QueryLog;
  d: TDFE;
begin
  tmp := TON_QueryLog.Create;
  tmp.Client := self;
  tmp.LogDB := LogDB;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(LogDB);
  d.WriteDouble(bTime);
  d.WriteDouble(eTime);
  d.WriteString(filter1);
  d.WriteString(filter2);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('QueryLog', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_Log_DB_Client.QueryLogM(LogDB: SystemString; bTime, eTime: TDateTime; filter1, filter2: U_String; OnResult: TON_QueryLogM);
var
  tmp: TON_QueryLog;
  d: TDFE;
begin
  tmp := TON_QueryLog.Create;
  tmp.Client := self;
  tmp.LogDB := LogDB;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(LogDB);
  d.WriteDouble(bTime);
  d.WriteDouble(eTime);
  d.WriteString(filter1);
  d.WriteString(filter2);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('QueryLog', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_Log_DB_Client.QueryLogP(LogDB: SystemString; bTime, eTime: TDateTime; filter1, filter2: U_String; OnResult: TON_QueryLogP);
var
  tmp: TON_QueryLog;
  d: TDFE;
begin
  tmp := TON_QueryLog.Create;
  tmp.Client := self;
  tmp.LogDB := LogDB;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(LogDB);
  d.WriteDouble(bTime);
  d.WriteDouble(eTime);
  d.WriteString(filter1);
  d.WriteString(filter2);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('QueryLog', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_Log_DB_Client.QueryLog_Bridge(LogDB: SystemString; bTime, eTime: TDateTime; Bridge_IO_: TPeerIO);
begin
  QueryLog_Bridge(LogDB, bTime, eTime, '', '', Bridge_IO_);
end;

procedure TC40_Log_DB_Client.QueryLogC(LogDB: SystemString; bTime, eTime: TDateTime; OnResult: TON_QueryLogC);
begin
  QueryLogC(LogDB, bTime, eTime, '', '', OnResult);
end;

procedure TC40_Log_DB_Client.QueryLogM(LogDB: SystemString; bTime, eTime: TDateTime; OnResult: TON_QueryLogM);
begin
  QueryLogM(LogDB, bTime, eTime, '', '', OnResult);
end;

procedure TC40_Log_DB_Client.QueryLogP(LogDB: SystemString; bTime, eTime: TDateTime; OnResult: TON_QueryLogP);
begin
  QueryLogP(LogDB, bTime, eTime, '', '', OnResult);
end;

procedure TC40_Log_DB_Client.QueryAndRemoveLog(LogDB: SystemString; bTime, eTime: TDateTime; filter1, filter2: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(LogDB);
  d.WriteDouble(bTime);
  d.WriteDouble(eTime);
  d.WriteString(filter1);
  d.WriteString(filter2);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('QueryAndRemoveLog', d);
  disposeObject(d);
end;

procedure TC40_Log_DB_Client.QueryAndRemoveLog(LogDB: SystemString; bTime, eTime: TDateTime);
begin
  QueryAndRemoveLog(LogDB, bTime, eTime, '', '');
end;

procedure TC40_Log_DB_Client.RemoveLog(LogDB: SystemString; arry_index: array of Integer);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(LogDB);
  d.WriteArrayInteger.WriteArray(arry_index);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveLog', d);
  disposeObject(d);
end;

procedure TC40_Log_DB_Client.GetLogDB_Bridge(Bridge_IO_: TPeerIO);
var
  d: TDFE;
begin
  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetLogDB', d, {$IFDEF FPC}@{$ENDIF FPC}TStreamEventBridge.Create(Bridge_IO_).DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_Log_DB_Client.GetLogDBC(OnResult: TON_GetLogDBC);
var
  tmp: TON_GetLogDB;
  d: TDFE;
begin
  tmp := TON_GetLogDB.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetLogDB', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_Log_DB_Client.GetLogDBM(OnResult: TON_GetLogDBM);
var
  tmp: TON_GetLogDB;
  d: TDFE;
begin
  tmp := TON_GetLogDB.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetLogDB', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_Log_DB_Client.GetLogDBP(OnResult: TON_GetLogDBP);
var
  tmp: TON_GetLogDB;
  d: TDFE;
begin
  tmp := TON_GetLogDB.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetLogDB', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_Log_DB_Client.CloseDB(LogDB: SystemString);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(LogDB);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('CloseDB', d);
  disposeObject(d);
end;

procedure TC40_Log_DB_Client.RemoveDB(LogDB: SystemString);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(LogDB);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveDB', d);
  disposeObject(d);
end;

procedure TC40_Log_DB_Client.Enabled_LogMonitor(Sync_Log: Boolean);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteBool(Sync_Log);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('Enabled_LogMonitor', d);
  disposeObject(d);
end;

initialization

RegisterC40('Log', TC40_Log_DB_Service, TC40_Log_DB_Client);

end.
