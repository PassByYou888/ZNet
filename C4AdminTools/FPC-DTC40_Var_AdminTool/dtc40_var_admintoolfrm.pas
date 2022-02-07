unit DTC40_Var_AdminToolFrm;

{$mode objFPC}{$H+}
{$MODESWITCH AdvancedRecords}
{$MODESWITCH NestedProcVars}
{$MODESWITCH NESTEDCOMMENTS}
{$NOTES OFF}
{$STACKFRAMES OFF}
{$COPERATORS OFF}
{$GOTO ON}
{$INLINE ON}
{$MACRO ON}
{$HINTS ON}
{$IEEEERRORS ON}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  ActnList, Menus,
  Variants, DateUtils, TypInfo,

  LCLType,

  {$IFDEF FPC}
  Z.FPC.GenericList,
  {$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status,
  Z.ListEngine, Z.GHashList, Z.Expression, Z.OpCode, Z.Parsing, Z.DFE, Z.TextDataEngine,
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
  Z.Net.PhysicsIO;

type
  TDTC40_Var_AdminToolForm = class(TForm, IC40_PhysicsTunnel_Event)
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
    NM_PopupMenu_: TPopupMenu;
    leftPanel: TPanel;
    listToolBarPanel: TPanel;
    SearchEdit: TLabeledEdit;
    SearchButton: TButton;
    NumEdit: TLabeledEdit;
    NMListView: TListView;
    lpLSplitter: TSplitter;
    rCliPanel: TPanel;
    VarListView: TListView;
    ScriptEdit: TLabeledEdit;
    RunScriptButton: TButton;
    Action_NewNM: TAction;
    NewNumberModule1: TMenuItem;
    NewNumberModule2: TMenuItem;
    Action_RemoveNM: TAction;
    RemoveNumberModule1: TMenuItem;
    RemoveNumberModule2: TMenuItem;
    Action_RemoveNMKey: TAction;
    Var_PopupMenu_: TPopupMenu;
    RemoveKeyValue1: TMenuItem;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure netTimerTimer(Sender: TObject);
    procedure queryButtonClick(Sender: TObject);
    procedure DTC4PasswdEditChange(Sender: TObject);
    procedure BuildDependNetButtonClick(Sender: TObject);
    procedure resetDependButtonClick(Sender: TObject);
    procedure NMListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
    procedure SearchButtonClick(Sender: TObject);
    procedure NMListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure RunScriptButtonClick(Sender: TObject);
    procedure VarListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
    procedure Action_NewNMExecute(Sender: TObject);
    procedure Action_RemoveNMExecute(Sender: TObject);
    procedure Action_RemoveNMKeyExecute(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
    procedure ReadConfig;
    procedure WriteConfig;
    procedure Do_QueryResult(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
    procedure DoConnected;
    procedure DoDisconnect;
    procedure Do_NM_Search(Sender: TC40_Var_Client; NMPool_: TC40_Var_NumberModulePool_List);
    procedure SearchNM(filter: U_String; MaxNum: Integer);
    procedure Do_NM_Script(Sender: TC40_Var_Client; Result_: TExpressionValueVector);
    procedure RunScript(Exp: U_String);
  private
    // IDTC40_PhysicsTunnel_Event
    procedure C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    procedure C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    // NM Event
    procedure Do_DTC40_Var_NM_Change(Sender: TC40_Var_Client; NMPool_: TC40_VarService_NM_Pool; NM: TNumberModule);
    procedure Do_DTC40_Var_Client_NM_Remove(Sender: TC40_Var_Client; NMName: U_String);
  private
    FCurrentNM: TC40_VarService_NM_Pool;
    procedure SetCurrentNM(const Value: TC40_VarService_NM_Pool);
  public
    ValidService: TC40_InfoList;
    CurrentClient: TC40_Var_Client;
    property CurrentNM: TC40_VarService_NM_Pool read FCurrentNM write SetCurrentNM;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TNM_Item = class(TListItem)
  public
    NM: TC40_VarService_NM_Pool;
  end;

  TNumber_Item = class(TListItem)
  public
    Number: TNumberModule;
  end;

var
  DTC40_Var_AdminToolForm: TDTC40_Var_AdminToolForm;

implementation

uses dtc40_var_admintoolnewnmfrm;

{$R *.lfm}

procedure TDTC40_Var_AdminToolForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteConfig;
  CloseAction := caFree;
end;

procedure TDTC40_Var_AdminToolForm.netTimerTimer(Sender: TObject);
begin
  C40Progress;
end;

procedure TDTC40_Var_AdminToolForm.queryButtonClick(Sender: TObject);
var
  tunnel_: TC40_PhysicsTunnel;
begin
  tunnel_ := C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(JoinHostEdit.Text, EStrToInt(JoinPortEdit.Text, 0));
  tunnel_.QueryInfoM(@Do_QueryResult);
end;

procedure TDTC40_Var_AdminToolForm.DTC4PasswdEditChange(Sender: TObject);
begin
  Z.Net.C4.C40_Password := DTC4PasswdEdit.Text;
end;

procedure TDTC40_Var_AdminToolForm.BuildDependNetButtonClick(Sender: TObject);
var
  info: TC40_Info;
begin
  if serviceComboBox.ItemIndex < 0 then
      exit;
  info := TC40_Info(serviceComboBox.Items.Objects[serviceComboBox.ItemIndex]);
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(info, info.ServiceTyp, self);
end;

procedure TDTC40_Var_AdminToolForm.resetDependButtonClick(Sender: TObject);
begin
  C40Clean;
end;

procedure TDTC40_Var_AdminToolForm.NMListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
begin
  ItemClass := TNM_Item;
end;

procedure TDTC40_Var_AdminToolForm.SearchButtonClick(Sender: TObject);
begin
  SearchNM(SearchEdit.Text, EStrToInt(NumEdit.Text, 100));
end;

procedure TDTC40_Var_AdminToolForm.NMListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected then
      CurrentNM := TNM_Item(Item).NM
  else
      CurrentNM := nil;
end;

procedure TDTC40_Var_AdminToolForm.RunScriptButtonClick(Sender: TObject);
begin
  RunScript(ScriptEdit.Text);
end;

procedure TDTC40_Var_AdminToolForm.VarListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
begin
  ItemClass := TNumber_Item;
end;

procedure TDTC40_Var_AdminToolForm.Action_NewNMExecute(Sender: TObject);
begin
  if CurrentClient = nil then
      exit;
  DTC40_Var_AdminToolNewNMForm.Show;
end;

procedure TDTC40_Var_AdminToolForm.Action_RemoveNMExecute(Sender: TObject);
var
  i: Integer;
  itm: TNM_Item;
begin
  if CurrentClient = nil then
      exit;
  i := 0;
  while i < NMListView.Items.Count do
    begin
      itm := NMListView.Items[i] as TNM_Item;
      if itm.Selected then
        begin
          if CurrentNM = itm.NM then
              CurrentNM := nil;
          CurrentClient.NM_Remove(itm.NM.Name, False);
          NMListView.Items.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TDTC40_Var_AdminToolForm.Action_RemoveNMKeyExecute(Sender: TObject);
var
  i: Integer;
  itm: TNumber_Item;
begin
  if CurrentClient = nil then
      exit;
  i := 0;
  while i < VarListView.Items.Count do
    begin
      itm := VarListView.Items[i] as TNumber_Item;
      if itm.Selected then
        begin
          CurrentClient.NM_RemoveKey(CurrentNM.Name, itm.Number.Name, False);
          VarListView.Items.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TDTC40_Var_AdminToolForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  if logMemo.Lines.Count > 2000 then
      logMemo.Clear;
  logMemo.Lines.Add(DateTimeToStr(now) + ' ' + Text_);
end;

procedure TDTC40_Var_AdminToolForm.ReadConfig;
var
  fn: U_String;
  te: THashTextEngine;
begin
  fn := umlChangeFileExt(Application.ExeName, '.conf');
  if not umlFileExists(fn) then
      exit;
  te := THashTextEngine.Create;
  te.LoadFromFile(fn);
  JoinHostEdit.Text := te.GetDefaultValue('Main', JoinHostEdit.Name, JoinHostEdit.Text);
  JoinPortEdit.Text := te.GetDefaultValue('Main', JoinPortEdit.Name, JoinPortEdit.Text);
  DisposeObject(te);
end;

procedure TDTC40_Var_AdminToolForm.WriteConfig;
var
  fn: U_String;
  te: THashTextEngine;
begin
  fn := umlChangeFileExt(Application.ExeName, '.conf');

  te := THashTextEngine.Create;

  te.SetDefaultValue('Main', JoinHostEdit.Name, JoinHostEdit.Text);
  te.SetDefaultValue('Main', JoinPortEdit.Name, JoinPortEdit.Text);

  te.SaveToFile(fn);
  DisposeObject(te);
end;

procedure TDTC40_Var_AdminToolForm.Do_QueryResult(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
var
  arry: TC40_Info_Array;
  i: Integer;
begin
  arry := L.SearchService(ExtractDependInfo(DependEdit.Text));
  for i := low(arry) to high(arry) do
      ValidService.Add(arry[i].Clone);

  serviceComboBox.Clear;
  for i := 0 to ValidService.Count - 1 do
      serviceComboBox.AddItem(Format('"%s" host "%s" port %d', [ValidService[i].ServiceTyp.Text, ValidService[i].PhysicsAddr.Text, ValidService[i].PhysicsPort]), ValidService[i]);

  if serviceComboBox.Items.Count > 0 then
      serviceComboBox.ItemIndex := 0;
end;

procedure TDTC40_Var_AdminToolForm.DoConnected;
begin
  SearchButtonClick(SearchButton);
end;

procedure TDTC40_Var_AdminToolForm.DoDisconnect;
begin
      serviceComboBox.Clear;
      NMListView.Items.Clear;
end;

procedure TDTC40_Var_AdminToolForm.Do_NM_Search(Sender: TC40_Var_Client; NMPool_: TC40_Var_NumberModulePool_List);
var
  i: Integer;
  itm: TNM_Item;
begin
  NMListView.Items.BeginUpdate;
  NMListView.Items.Clear;
  for i := 0 to NMPool_.Count - 1 do
    begin
      itm := NMListView.Items.Add as TNM_Item;
      itm.NM := NMPool_[i];
      itm.Caption := itm.NM.Name;
    end;
  NMListView.Items.EndUpdate;
end;

procedure TDTC40_Var_AdminToolForm.SearchNM(filter: U_String; MaxNum: Integer);
begin
  if CurrentClient = nil then
      exit;
  CurrentNM := nil;
  NMListView.Clear;
  CurrentClient.NM_CloseAll(True);
  CurrentClient.NM_SearchM(filter, MaxNum, True, @Do_NM_Search);
end;

procedure TDTC40_Var_AdminToolForm.Do_NM_Script(Sender: TC40_Var_Client; Result_: TExpressionValueVector);
begin
  DoStatusE(Result_);
end;

procedure TDTC40_Var_AdminToolForm.RunScript(Exp: U_String);
var
  i: Integer;
  itm: TNM_Item;
begin
  if CurrentClient = nil then
      exit;
  if NMListView.SelCount = 0 then
      exit;
  if NMListView.SelCount = 1 then
    begin
      itm := NMListView.Selected as TNM_Item;
      CurrentClient.NM_ScriptM(itm.NM.Name, [Exp.Text], @Do_NM_Script);
      exit;
    end;
  for i := 0 to NMListView.Items.Count - 1 do
    begin
      itm := NMListView.Items[i] as TNM_Item;
      if itm.Selected then
          CurrentClient.NM_ScriptM(itm.NM.Name, [Exp.Text], @Do_NM_Script);
    end;
end;

procedure TDTC40_Var_AdminToolForm.C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
begin

end;

procedure TDTC40_Var_AdminToolForm.C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
begin
  if Sender.DependNetworkClientPool.IndexOf(CurrentClient) >= 0 then
    begin
      DoDisconnect;
      ValidService.Clear;
      CurrentClient := nil;
      CurrentNM := nil;
    end;
end;

procedure TDTC40_Var_AdminToolForm.C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin

end;

procedure TDTC40_Var_AdminToolForm.C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
var
  info: TC40_Info;
begin
  if serviceComboBox.ItemIndex < 0 then
      exit;
  info := TC40_Info(serviceComboBox.Items.Objects[serviceComboBox.ItemIndex]);
  if info.Same(Custom_Client_.ClientInfo) and (Custom_Client_ is TC40_Var_Client) then
    begin
      CurrentClient := TC40_Var_Client(Custom_Client_);
      CurrentClient.OnChange := @Do_DTC40_Var_NM_Change;
      CurrentClient.OnRemove := @Do_DTC40_Var_Client_NM_Remove;
      SysPost.PostExecuteM_NP(0.5, @DoConnected);
    end;
end;

procedure TDTC40_Var_AdminToolForm.Do_DTC40_Var_NM_Change(Sender: TC40_Var_Client; NMPool_: TC40_VarService_NM_Pool; NM: TNumberModule);
var
  i: Integer;
  itm: TNumber_Item;
  found_: Boolean;
begin
  if CurrentNM <> NMPool_ then
      exit;
  found_ := False;
  for i := 0 to VarListView.Items.Count - 1 do
    begin
      itm := VarListView.Items[i] as TNumber_Item;
      if itm.Number = NM then
        begin
          itm.SubItems[0] := NM.CurrentAsString;
          found_ := True;
        end;
    end;
  if not found_ then
    begin
      itm := VarListView.Items.Add as TNumber_Item;
      itm.Number := NM;
      itm.Caption := NM.Name;
      itm.SubItems.Add(NM.CurrentAsString);
    end;
end;

procedure TDTC40_Var_AdminToolForm.Do_DTC40_Var_Client_NM_Remove(Sender: TC40_Var_Client; NMName: U_String);
var
  i: Integer;
  itm: TNM_Item;
begin
  if (CurrentNM <> nil) and (NMName.Same(CurrentNM.Name)) then
      VarListView.Clear;

  i := 0;
  while i < NMListView.Items.Count do
    begin
      itm := NMListView.Items[i] as TNM_Item;
      if itm.NM.Name.Same(NMName) then
          NMListView.Items.Delete(i)
      else
          inc(i);
    end;
end;

procedure TDTC40_Var_AdminToolForm.SetCurrentNM(const Value: TC40_VarService_NM_Pool);
  procedure fpc_progress_(const Name_: PSystemString; Obj_: TNumberModule);
      var
        itm: TNumber_Item;
      begin
        itm := VarListView.Items.Add as TNumber_Item;
        itm.Number := Obj_;
        itm.Caption := Obj_.Name;
        itm.SubItems.Add(Obj_.CurrentAsString);
      end;
begin
  VarListView.Clear;
  FCurrentNM := Value;
  if FCurrentNM = nil then
      exit;
  VarListView.Items.BeginUpdate;
  FCurrentNM.List.ProgressP(@fpc_progress_);
  VarListView.Items.EndUpdate;
end;

constructor TDTC40_Var_AdminToolForm.Create(AOwner: TComponent);
var
  i: Integer;
  p: PC40_RegistedData;
  depend_: U_String;
begin
  inherited Create(AOwner);
  C40_QuietMode := False;
  AddDoStatusHook(self, @DoStatus_backcall);

  DTC4PasswdEdit.Text := Z.Net.C4.C40_Password;
  ReadConfig;
  ValidService := TC40_InfoList.Create(True);
  CurrentClient := nil;
  CurrentNM := nil;

  depend_ := '';
  for i := 0 to C40_Registed.Count - 1 do
    begin
      p := C40_Registed[i];
      if p^.ClientClass.InheritsFrom(TC40_Var_Client) then
        begin
          if depend_.L > 0 then
              depend_.Append('|');
          depend_.Append(p^.ServiceTyp);
        end;
    end;
  DependEdit.Text := depend_.Text;
end;

destructor TDTC40_Var_AdminToolForm.Destroy;
begin
  C40Clean;
  RemoveDoStatusHook(self);
  inherited Destroy;
end;

end.
