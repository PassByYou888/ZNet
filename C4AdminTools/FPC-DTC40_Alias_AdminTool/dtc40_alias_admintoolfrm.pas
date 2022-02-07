unit dtc40_alias_admintoolfrm;

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
  Z.Net.C4, Z.Net.C4_UserDB, Z.Net.C4_Var, Z.Net.C4_FS, Z.Net.C4_RandSeed, Z.Net.C4_Log_DB, Z.Net.C4_Alias,
  Z.Net.PhysicsIO;

type
  TDTC40_Alias_AdminToolForm = class(TForm, IC40_PhysicsTunnel_Event)
    netTimer: TTimer;
    logMemo: TMemo;
    TopBarPanel: TPanel;
    JoinHostEdit: TLabeledEdit;
    JoinPortEdit: TLabeledEdit;
    DependEdit: TLabeledEdit;
    BuildDependNetButton: TButton;
    resetDependButton: TButton;
    serviceComboBox: TComboBox;
    queryButton: TButton;
    DTC4PasswdEdit: TLabeledEdit;
    _B_Splitter: TSplitter;
    cliPanel: TPanel;
    logDBToolBarPanel: TPanel;
    AliasFilterEdit: TLabeledEdit;
    searchAliasButton: TButton;
    AliasListView: TListView;
    removeAliasButton: TButton;
    NewAliasButton: TButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure netTimerTimer(Sender: TObject);
    procedure BuildDependNetButtonClick(Sender: TObject);
    procedure queryButtonClick(Sender: TObject);
    procedure resetDependButtonClick(Sender: TObject);
    procedure searchAliasButtonClick(Sender: TObject);
    procedure removeAliasButtonClick(Sender: TObject);
    procedure NewAliasButtonClick(Sender: TObject);
    procedure AliasListViewKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure AliasListViewDblClick(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: integer);
    procedure ReadConfig;
    procedure WriteConfig;
    procedure Do_QueryResult(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
    procedure ClearAliasList;
    procedure Do_GetAlias_Result(Sender: TC40_Alias_Client; NameKey_: THashStringList);
    procedure DoSearchAlias(filter: U_String);
  private
    procedure C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    procedure C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
  public
    ValidService: TC40_InfoList;
    MyClient: TC40_Alias_Client;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  DTC40_Alias_AdminToolForm: TDTC40_Alias_AdminToolForm;

implementation

uses newaliasfrm;

{$R *.lfm}


procedure TDTC40_Alias_AdminToolForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteConfig;
  CloseAction := caFree;
end;

procedure TDTC40_Alias_AdminToolForm.netTimerTimer(Sender: TObject);
begin
  Z.Net.C4.C40Progress;
end;

procedure TDTC40_Alias_AdminToolForm.BuildDependNetButtonClick(Sender: TObject);
var
  info: TC40_Info;
begin
  if serviceComboBox.ItemIndex < 0 then
    exit;
  info := TC40_Info(serviceComboBox.Items.Objects[serviceComboBox.ItemIndex]);
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(info, info.ServiceTyp, self);
end;

procedure TDTC40_Alias_AdminToolForm.queryButtonClick(Sender: TObject);
var
  tunnel_: TC40_PhysicsTunnel;
begin
  tunnel_ := C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(JoinHostEdit.Text, EStrToInt(JoinPortEdit.Text, 0));
  tunnel_.QueryInfoM(@Do_QueryResult);
end;

procedure TDTC40_Alias_AdminToolForm.resetDependButtonClick(Sender: TObject);
begin
  C40Clean_Client;
end;

procedure TDTC40_Alias_AdminToolForm.searchAliasButtonClick(Sender: TObject);
begin
  DoSearchAlias(AliasFilterEdit.Text);
end;

procedure TDTC40_Alias_AdminToolForm.removeAliasButtonClick(Sender: TObject);
var
  i: integer;
begin
  if MyClient = nil then
    exit;
  for i := 0 to AliasListView.Items.Count - 1 do
    if AliasListView.Items[i].Selected then
      MyClient.RemoveAlias(AliasListView.Items[i].Caption);
  searchAliasButtonClick(searchAliasButton);
end;

procedure TDTC40_Alias_AdminToolForm.NewAliasButtonClick(Sender: TObject);
begin
  if MyClient = nil then
    exit;
  if NewAliasForm.ShowModal <> mrOk then
    exit;
  MyClient.SetAlias(NewAliasForm.AliasEdit.Text, NewAliasForm.NameEdit.Text);
  searchAliasButtonClick(searchAliasButton);
end;

procedure TDTC40_Alias_AdminToolForm.AliasListViewKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    removeAliasButtonClick(removeAliasButton);
end;

procedure TDTC40_Alias_AdminToolForm.AliasListViewDblClick(Sender: TObject);
begin
  if MyClient = nil then
    exit;
  if AliasListView.Selected = nil then
    exit;

  NewAliasForm.AliasEdit.Text := AliasListView.Selected.Caption;
  NewAliasForm.NameEdit.Text := AliasListView.Selected.SubItems[0];
  if NewAliasForm.ShowModal <> mrOk then
    exit;
  MyClient.SetAlias(NewAliasForm.AliasEdit.Text, NewAliasForm.NameEdit.Text);
  searchAliasButtonClick(searchAliasButton);
end;

procedure TDTC40_Alias_AdminToolForm.DoStatus_backcall(Text_: SystemString; const ID: integer);
begin
  if logMemo.Lines.Count > 2000 then
    logMemo.Clear;
  logMemo.Lines.Add(DateTimeToStr(now) + ' ' + Text_);
end;

procedure TDTC40_Alias_AdminToolForm.ReadConfig;
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

procedure TDTC40_Alias_AdminToolForm.WriteConfig;
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

procedure TDTC40_Alias_AdminToolForm.Do_QueryResult(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
var
  arry: TC40_Info_Array;
  i: integer;
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

procedure TDTC40_Alias_AdminToolForm.ClearAliasList;
begin
  AliasListView.Clear;
end;

procedure TDTC40_Alias_AdminToolForm.Do_GetAlias_Result(Sender: TC40_Alias_Client; NameKey_: THashStringList);

  procedure fpc_progress(Sender: THashStringList; Name_: PSystemString; const V: SystemString);
  var
    itm: TListItem;
  begin
    itm := AliasListView.Items.Add;
    itm.Caption := Name_^;
    itm.SubItems.Add(V);
    itm.ImageIndex := -1;
    itm.StateIndex := -1;
  end;

begin
  AliasListView.Items.BeginUpdate;
  NameKey_.ProgressP(@fpc_progress);
  AliasListView.Items.EndUpdate;
end;

procedure TDTC40_Alias_AdminToolForm.DoSearchAlias(filter: U_String);
begin
  ClearAliasList;
  if MyClient = nil then
    exit;
  MyClient.SearchAlias_M(filter, @Do_GetAlias_Result);
end;

procedure TDTC40_Alias_AdminToolForm.C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
begin
  DoStatus('connect to "%s" port %d ok.', [Sender.PhysicsAddr.Text, Sender.PhysicsPort]);
end;

procedure TDTC40_Alias_AdminToolForm.C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
begin
  serviceComboBox.Clear;
  ValidService.Clear;
  MyClient := nil;
  cliPanel.Enabled := False;
  ClearAliasList;
end;

procedure TDTC40_Alias_AdminToolForm.C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
  DoStatus('Build network: %s classes: %s', [Custom_Client_.ClientInfo.ServiceTyp.Text, Custom_Client_.ClassName]);
end;

procedure TDTC40_Alias_AdminToolForm.C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
  if Custom_Client_ is TC40_Alias_Client then
  begin
    MyClient := Custom_Client_ as TC40_Alias_Client;
    cliPanel.Enabled := True;
    searchAliasButtonClick(searchAliasButton);
  end;
end;

constructor TDTC40_Alias_AdminToolForm.Create(AOwner: TComponent);
var
  i: integer;
  p: PC40_RegistedData;
  depend_: U_String;
begin
  inherited Create(AOwner);
  AddDoStatusHook(self, @DoStatus_backcall);
  Z.Net.C4.C40_QuietMode := False;
  ValidService := TC40_InfoList.Create(True);

  ReadConfig;

  DTC4PasswdEdit.Text := Z.Net.C4.C40_Password;

  depend_ := '';
  for i := 0 to C40_Registed.Count - 1 do
  begin
    p := C40_Registed[i];
    if p^.ClientClass.InheritsFrom(TC40_Alias_Client) then
    begin
      if depend_.L > 0 then
        depend_.Append('|');
      depend_.Append(p^.ServiceTyp);
    end;
  end;
  DependEdit.Text := depend_.Text;

  C40_PhysicsTunnel_Disconnect(nil);
end;

destructor TDTC40_Alias_AdminToolForm.Destroy;
begin
  C40Clean;
  RemoveDoStatusHook(self);
  inherited Destroy;
end;

end.
