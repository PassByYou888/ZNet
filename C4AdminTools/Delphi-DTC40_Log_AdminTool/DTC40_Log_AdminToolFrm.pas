unit DTC40_Log_AdminToolFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.Menus, System.Actions, Vcl.ActnList,

  Vcl.FileCtrl,
  System.IOUtils, System.DateUtils, System.TypInfo,

  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status,
  Z.ListEngine, Z.HashList.Templet, Z.Expression, Z.OpCode, Z.Parsing, Z.DFE, Z.TextDataEngine,
  Z.Json, Z.Geometry2D, Z.Geometry3D, Z.Number,
  Z.MemoryStream, Z.Cipher, Z.Notify, Z.IOThread,
  Z.Net,
  Z.Net.DoubleTunnelIO,
  Z.Net.DoubleTunnelIO.NoAuth,
  Z.Net.DoubleTunnelIO.VirtualAuth,
  Z.Net.DataStoreService,
  Z.Net.DataStoreService.NoAuth,
  Z.Net.DataStoreService.VirtualAuth,
  Z.Net.DataStoreService.Common,
  Z.ZDB.ObjectData_LIB, Z.ZDB, Z.ZDB.Engine, Z.ZDB.LocalManager,
  Z.ZDB.FileIndexPackage_LIB, Z.ZDB.FilePackage_LIB, Z.ZDB.ItemStream_LIB, Z.ZDB.HashField_LIB, Z.ZDB.HashItem_LIB,
  Z.ZDB2.Custom, Z.ZDB2, Z.ZDB2.DFE, Z.ZDB2.HS, Z.ZDB2.HV, Z.ZDB2.Json, Z.ZDB2.MS64, Z.ZDB2.NM, Z.ZDB2.TE, Z.ZDB2.FileEncoder,
  Z.Net.C4, Z.Net.C4_UserDB, Z.Net.C4_Var, Z.Net.C4_FS, Z.Net.C4_RandSeed, Z.Net.C4_Log_DB,
  Z.Net.C4_NetDisk_Service, Z.Net.C4_NetDisk_Client,
  Z.Net.PhysicsIO;

type
  TTemp_Search = class;

  TDTC40_Log_AdminToolForm = class(TForm, IC40_PhysicsTunnel_Event, I_ON_C40_Log_DB_Client_Interface)
    logMemo: TMemo;
    botSplitter: TSplitter;
    TopBarPanel: TPanel;
    JoinHostEdit: TLabeledEdit;
    JoinPortEdit: TLabeledEdit;
    DependEdit: TLabeledEdit;
    BuildDependNetButton: TButton;
    resetDependButton: TButton;
    serviceComboBox: TComboBox;
    queryButton: TButton;
    DTC4PasswdEdit: TLabeledEdit;
    netTimer: TTimer;
    cliPanel: TPanel;
    ActionList_: TActionList;
    MainMenu_: TMainMenu;
    File1: TMenuItem;
    PopupMenu_: TPopupMenu;
    lpLSplitter: TSplitter;
    leftPanel: TPanel;
    logQueryToolBarPanel: TPanel;
    filter1Edit: TLabeledEdit;
    SearchLogButton: TButton;
    filter2Edit: TLabeledEdit;
    rCliPanel: TPanel;
    QueryMemo: TMemo;
    TimeRangeComboBox: TComboBox;
    Label1: TLabel;
    LogDBListView: TListView;
    logDBToolBarPanel: TPanel;
    LogDBFilterEdit: TLabeledEdit;
    searchLogDBButton: TButton;
    checkAllButton: TButton;
    uncheckAllButton: TButton;
    Action_RemoveLogDB: TAction;
    RemoveofLogDB1: TMenuItem;
    RemoveofLogDB2: TMenuItem;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure netTimerTimer(Sender: TObject);
    procedure queryButtonClick(Sender: TObject);
    procedure DTC4PasswdEditChange(Sender: TObject);
    procedure BuildDependNetButtonClick(Sender: TObject);
    procedure resetDependButtonClick(Sender: TObject);
    procedure LogDBListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
    procedure SearchLogButtonClick(Sender: TObject);
    procedure checkAllButtonClick(Sender: TObject);
    procedure uncheckAllButtonClick(Sender: TObject);
    procedure Action_RemoveLogDBExecute(Sender: TObject);
    procedure searchLogDBButtonClick(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
    procedure ReadConfig;
    procedure WriteConfig;
    procedure Do_QueryResult(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
    procedure DoConnected;
    procedure DoDisconnect;
    procedure Do_GetLog(Sender: TC40_Log_DB_Client; arry: U_StringArray);
    procedure RefreshLogDB;
    procedure DoSyncUpdateLogList;
    procedure DoSearchSortTh;
    procedure SearchDone(Sender: TObject);
    procedure SearchLog;
  private
    // IDTC40_PhysicsTunnel_Event
    procedure C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    procedure C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    // I_ON_C40_Log_DB_Client_Interface Event
    procedure Do_Sync_Log(LogDB, Log1_, Log2_: SystemString);
  public
    ValidService: TC40_InfoList;
    CurrentClient: TC40_Log_DB_Client;
    SearchReturn: TTemp_Search;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TTemp_Search = class
  public
    L: TLogData_List;
    Done_Log_DB: THashList;
    TotalQueryNum: Integer;
    OnDone: TNotifyEvent;
    constructor Create();
    destructor Destroy; override;
    procedure Do_QueryLog(Sender: TC40_Log_DB_Client; LogDB: SystemString; arry: TArrayLogData);
  end;

  TLog_Item = class(TListItem)
  public
    LogDB: U_String;
  end;

var
  DTC40_Log_AdminToolForm: TDTC40_Log_AdminToolForm;

implementation

{$R *.dfm}


constructor TTemp_Search.Create;
begin
  inherited Create;
  L := TLogData_List.Create;
  Done_Log_DB := THashList.CustomCreate(1024);
  Done_Log_DB.AutoFreeData := False;
  TotalQueryNum := 0;
  OnDone := nil;
end;

destructor TTemp_Search.Destroy;
begin
  DisposeObject(L);
  DisposeObject(Done_Log_DB);
  inherited Destroy;
end;

procedure TTemp_Search.Do_QueryLog(Sender: TC40_Log_DB_Client; LogDB: SystemString; arry: TArrayLogData);
begin
  L.AddArry(arry);
  Done_Log_DB.Add(LogDB, nil);
  DoStatus('done query "%s", found log %d', [LogDB, length(arry)]);
  if Done_Log_DB.Count >= TotalQueryNum then
    if Assigned(OnDone) then
        OnDone(self);
end;

procedure TDTC40_Log_AdminToolForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteConfig;
  CloseAction := caFree;
end;

procedure TDTC40_Log_AdminToolForm.netTimerTimer(Sender: TObject);
begin
  C40Progress;
end;

procedure TDTC40_Log_AdminToolForm.queryButtonClick(Sender: TObject);
var
  tunnel_: TC40_PhysicsTunnel;
begin
  tunnel_ := C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(JoinHostEdit.Text, EStrToInt(JoinPortEdit.Text, 0));
  tunnel_.QueryInfoM(Do_QueryResult);
end;

procedure TDTC40_Log_AdminToolForm.DTC4PasswdEditChange(Sender: TObject);
begin
  Z.Net.C4.C40_Password := DTC4PasswdEdit.Text;
end;

procedure TDTC40_Log_AdminToolForm.BuildDependNetButtonClick(Sender: TObject);
var
  info: TC40_Info;
begin
  if serviceComboBox.ItemIndex < 0 then
      exit;
  info := TC40_Info(serviceComboBox.Items.Objects[serviceComboBox.ItemIndex]);
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(info, info.ServiceTyp, self);
end;

procedure TDTC40_Log_AdminToolForm.resetDependButtonClick(Sender: TObject);
begin
  C40Clean;
end;

procedure TDTC40_Log_AdminToolForm.LogDBListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
begin
  ItemClass := TLog_Item;
end;

procedure TDTC40_Log_AdminToolForm.SearchLogButtonClick(Sender: TObject);
begin
  SearchLog;
end;

procedure TDTC40_Log_AdminToolForm.checkAllButtonClick(Sender: TObject);
var
  i: Integer;
  itm: TLog_Item;
begin
  for i := 0 to LogDBListView.Items.Count - 1 do
    begin
      itm := LogDBListView.Items[i] as TLog_Item;
      itm.Checked := True;
    end;
end;

procedure TDTC40_Log_AdminToolForm.uncheckAllButtonClick(Sender: TObject);
var
  i: Integer;
  itm: TLog_Item;
begin
  for i := 0 to LogDBListView.Items.Count - 1 do
    begin
      itm := LogDBListView.Items[i] as TLog_Item;
      itm.Checked := False;
    end;
end;

procedure TDTC40_Log_AdminToolForm.Action_RemoveLogDBExecute(Sender: TObject);
var
  i: Integer;
  itm: TLog_Item;
begin
  if CurrentClient = nil then
      exit;
  if MessageDlg('remove?', mtWarning, [mbYes, mbNo], 0) <> mrYes then
      exit;
  for i := 0 to LogDBListView.Items.Count - 1 do
    begin
      itm := LogDBListView.Items[i] as TLog_Item;
      if itm.Selected then
          CurrentClient.RemoveDB(itm.LogDB);
    end;
end;

procedure TDTC40_Log_AdminToolForm.searchLogDBButtonClick(Sender: TObject);
begin
  RefreshLogDB;
end;

procedure TDTC40_Log_AdminToolForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  if logMemo.Lines.Count > 2000 then
      logMemo.Clear;
  logMemo.Lines.Add(DateTimeToStr(now) + ' ' + Text_);
end;

procedure TDTC40_Log_AdminToolForm.ReadConfig;
var
  fn: U_String;
  TE: THashTextEngine;
begin
  fn := umlChangeFileExt(Application.ExeName, '.conf');
  if not umlFileExists(fn) then
      exit;
  TE := THashTextEngine.Create;
  TE.LoadFromFile(fn);
  JoinHostEdit.Text := TE.GetDefaultValue('Main', JoinHostEdit.Name, JoinHostEdit.Text);
  JoinPortEdit.Text := TE.GetDefaultValue('Main', JoinPortEdit.Name, JoinPortEdit.Text);
  DisposeObject(TE);
end;

procedure TDTC40_Log_AdminToolForm.WriteConfig;
var
  fn: U_String;
  TE: THashTextEngine;
begin
  fn := umlChangeFileExt(Application.ExeName, '.conf');

  TE := THashTextEngine.Create;

  TE.SetDefaultValue('Main', JoinHostEdit.Name, JoinHostEdit.Text);
  TE.SetDefaultValue('Main', JoinPortEdit.Name, JoinPortEdit.Text);

  TE.SaveToFile(fn);
  DisposeObject(TE);
end;

procedure TDTC40_Log_AdminToolForm.Do_QueryResult(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
var
  arry: TC40_Info_Array;
  i: Integer;
begin
  ValidService.Clear;
  arry := L.SearchService(ExtractDependInfo(DependEdit.Text));
  for i := low(arry) to high(arry) do
      ValidService.Add(arry[i].Clone);

  serviceComboBox.Clear;
  for i := 0 to ValidService.Count - 1 do
      serviceComboBox.AddItem(Format('"%s" host "%s" port %d', [ValidService[i].ServiceTyp.Text, ValidService[i].PhysicsAddr.Text, ValidService[i].PhysicsPort]), ValidService[i]);

  if serviceComboBox.Items.Count > 0 then
      serviceComboBox.ItemIndex := 0;
end;

procedure TDTC40_Log_AdminToolForm.Do_Sync_Log(LogDB, Log1_, Log2_: SystemString);
begin
  DoStatus('sync log %s log1:%s log2:%s', [LogDB, Log1_, Log2_]);
end;

procedure TDTC40_Log_AdminToolForm.DoConnected;
begin
  if CurrentClient <> nil then
    begin
      CurrentClient.ON_C40_Log_DB_Client_Interface := self;
      CurrentClient.Enabled_LogMonitor(True);
    end;
  RefreshLogDB;
end;

procedure TDTC40_Log_AdminToolForm.DoDisconnect;
begin
  SysPost.PostExecuteP_NP(1.0, procedure
    begin
      serviceComboBox.Clear;
      LogDBListView.Clear;
      QueryMemo.Lines.Clear;
    end);
end;

procedure TDTC40_Log_AdminToolForm.Do_GetLog(Sender: TC40_Log_DB_Client; arry: U_StringArray);
var
  itm: TLog_Item;
  i: Integer;
  n: SystemString;
begin
  LogDBListView.Items.BeginUpdate;
  LogDBListView.Items.Clear;
  for i := Low(arry) to high(arry) do
    begin
      n := arry[i];
      itm := LogDBListView.Items.Add as TLog_Item;
      itm.LogDB := n;
      itm.Caption := IntToStr(i + 1) + ' - ' + n;
      itm.SubItems.Add('idle');
    end;
  LogDBListView.Items.EndUpdate;
  LogDBListView.Height := LogDBListView.Height - 1;

  for i := 0 to LogDBListView.Items.Count - 1 do
    begin
      itm := LogDBListView.Items[i] as TLog_Item;
      if umlSearchMatch(LogDBFilterEdit.Text, itm.LogDB) then
          LogDBListView.Items[i].Checked := True;
    end;
end;

procedure TDTC40_Log_AdminToolForm.RefreshLogDB;
begin
  if CurrentClient = nil then
      exit;
  CurrentClient.GetLogDBM(Do_GetLog);
end;

procedure TDTC40_Log_AdminToolForm.DoSyncUpdateLogList;
var
  i: Integer;
  p: PLogData__;
begin
  QueryMemo.Lines.Clear;
  QueryMemo.Lines.BeginUpdate;
  for i := 0 to SearchReturn.L.Count - 1 do
    begin
      p := SearchReturn.L[i];
      QueryMemo.Lines.Add(Format('%d - %s DB: "%s" %s %s',
        [i + 1,
        DateTimeToStr(p^.LogTime),
        p^.LogDB,
        if_(p^.Log1 <> '', 'Log1: "' + p^.Log1 + '"', ''),
        if_(p^.Log2 <> '', 'Log2: "' + p^.Log2 + '"', '')]));
    end;
  QueryMemo.Lines.EndUpdate;

  SearchReturn.L.Clear;
  SearchReturn.Done_Log_DB.Clear;
  SearchReturn.TotalQueryNum := 0;
  SearchReturn.OnDone := nil;
end;

procedure TDTC40_Log_AdminToolForm.DoSearchSortTh;
begin
  SearchReturn.L.SortByTime;
  TCompute.SyncM(DoSyncUpdateLogList);
end;

procedure TDTC40_Log_AdminToolForm.SearchDone(Sender: TObject);
begin
  TCompute.RunM_NP(DoSearchSortTh);
end;

procedure TDTC40_Log_AdminToolForm.SearchLog;
var
  itm: TLog_Item;
  i: Integer;
  bTime, eTime: TDateTime;
begin
  if CurrentClient = nil then
      exit;

  eTime := now;
  case TimeRangeComboBox.ItemIndex of
    0: bTime := IncSecond(eTime, -60); // Last 60 seconds
    1: bTime := IncMinute(eTime, -10); // Last 10 minutes
    2: bTime := IncHour(eTime, -10);   // Last 1 hour
    3: bTime := Today;                 // ToDay
    4: bTime := IncDay(eTime, -3);     // Last 3 days
    5: bTime := IncDay(eTime, -7);     // Last week
    6: bTime := IncDay(eTime, -30);    // Last month
    7: bTime := IncDay(eTime, -90);    // Last 3 month
    8: bTime := IncDay(eTime, -365);   // Last 3 month
  end;

  SearchReturn.L.Clear;
  SearchReturn.Done_Log_DB.Clear;
  SearchReturn.TotalQueryNum := 0;
  SearchReturn.OnDone := SearchDone;
  for i := 0 to LogDBListView.Items.Count - 1 do
    begin
      itm := LogDBListView.Items[i] as TLog_Item;
      if itm.Checked then
        begin
          inc(SearchReturn.TotalQueryNum);
          CurrentClient.QueryLogM(itm.LogDB, bTime, eTime, filter1Edit.Text, filter2Edit.Text, SearchReturn.Do_QueryLog);
        end;
    end;
end;

procedure TDTC40_Log_AdminToolForm.C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
begin

end;

procedure TDTC40_Log_AdminToolForm.C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
begin
  if Sender.DependNetworkClientPool.IndexOf(CurrentClient) >= 0 then
    begin
      DoDisconnect;
      ValidService.Clear;
      CurrentClient := nil;
    end;
end;

procedure TDTC40_Log_AdminToolForm.C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin

end;

procedure TDTC40_Log_AdminToolForm.C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
var
  info: TC40_Info;
begin
  if serviceComboBox.ItemIndex < 0 then
      exit;
  info := TC40_Info(serviceComboBox.Items.Objects[serviceComboBox.ItemIndex]);
  if info.Same(Custom_Client_.ClientInfo) and (Custom_Client_ is TC40_Log_DB_Client) then
    begin
      CurrentClient := TC40_Log_DB_Client(Custom_Client_);
      SysPost.PostExecuteM_NP(0.5, DoConnected);
    end;
end;

constructor TDTC40_Log_AdminToolForm.Create(AOwner: TComponent);
var
  i: Integer;
  p: PC40_RegistedData;
  depend_: U_String;
begin
  inherited Create(AOwner);
  C40_QuietMode := False;
  AddDoStatusHook(self, DoStatus_backcall);

  DTC4PasswdEdit.Text := Z.Net.C4.C40_Password;
  ReadConfig;
  ValidService := TC40_InfoList.Create(True);
  CurrentClient := nil;

  depend_ := '';
  for i := 0 to C40_Registed.Count - 1 do
    begin
      p := C40_Registed[i];
      if p^.ClientClass.InheritsFrom(TC40_Log_DB_Client) then
        begin
          if depend_.L > 0 then
              depend_.Append('|');
          depend_.Append(p^.ServiceTyp);
        end;
    end;
  DependEdit.Text := depend_;

  SearchReturn := TTemp_Search.Create;
end;

destructor TDTC40_Log_AdminToolForm.Destroy;
begin
  C40Clean;
  RemoveDoStatusHook(self);
  DisposeObject(SearchReturn);
  inherited Destroy;
end;

end.
