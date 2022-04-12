unit dtc40_tekeyvalue_admintoolfrm;

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
  Z.Net.C4_TEKeyValue,
  Z.Net.PhysicsIO;

type
  TDTC40_TEKeyValue_AdminTool_Form = class(TForm, IC40_PhysicsTunnel_Event)
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
    lpLSplitter: TSplitter;
    leftPanel: TPanel;
    TE_ListView: TListView;
    TE_L_ToolBarPanel: TPanel;
    search_TE_Button: TButton;
    rCliPanel: TPanel;
    TE_Memo: TMemo;
    SearchEdit: TLabeledEdit;
    NumEdit: TLabeledEdit;
    Panel1: TPanel;
    UpdateMemoTo_TE_Button: TButton;
    TE_Name_Edit: TLabeledEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure netTimerTimer(Sender: TObject);
    procedure BuildDependNetButtonClick(Sender: TObject);
    procedure queryButtonClick(Sender: TObject);
    procedure resetDependButtonClick(Sender: TObject);
    procedure search_TE_ButtonClick(Sender: TObject);
    procedure TE_ListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
    procedure TE_ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure UpdateMemoTo_TE_ButtonClick(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
    procedure ReadConfig;
    procedure WriteConfig;
    procedure Do_QueryResult(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
    procedure Clear_TE_List;
    procedure Clear_TE_Text;
    procedure Do_SearchTE(Sender: TC40_TEKeyValue_Client; arry: TC40_TEKeyValue_Client_SearchTE_Result_Array);
  private
    procedure C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    procedure C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
  public
    ValidService: TC40_InfoList;
    TEKeyValue_Client: TC40_TEKeyValue_Client;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  DTC40_TEKeyValue_AdminTool_Form: TDTC40_TEKeyValue_AdminTool_Form;

implementation

{$R *.lfm}


type
  T_TE_Item = class(TListItem)
  public
    TE: THashTextEngine;
    constructor Create(AOwner: TListItems); override;
    destructor Destroy; override;
  end;

constructor T_TE_Item.Create(AOwner: TListItems);
begin
  inherited;
  TE := THashTextEngine.Create;
end;

destructor T_TE_Item.Destroy;
begin
  DisposeObject(TE);
  inherited;
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteConfig;
  CloseAction := caFree;
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.netTimerTimer(Sender: TObject);
begin
  Z.Net.C4.C40Progress;
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.BuildDependNetButtonClick(Sender: TObject);
var
  info: TC40_Info;
begin
  if serviceComboBox.ItemIndex < 0 then
      exit;
  info := TC40_Info(serviceComboBox.Items.Objects[serviceComboBox.ItemIndex]);
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(info, info.ServiceTyp, self);
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.queryButtonClick(Sender: TObject);
var
  tunnel_: TC40_PhysicsTunnel;
begin
  tunnel_ := C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(JoinHostEdit.Text, EStrToInt(JoinPortEdit.Text, 0));
  tunnel_.QueryInfoM(@Do_QueryResult);
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.resetDependButtonClick(Sender: TObject);
begin
  C40Clean_Client;
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.search_TE_ButtonClick(Sender: TObject);
begin
  if TEKeyValue_Client = nil then
      exit;
  TEKeyValue_Client.SearchTE_M(SearchEdit.Text, EStrToInt(NumEdit.Text), @Do_SearchTE);
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.TE_ListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
begin
  ItemClass := T_TE_Item;
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.TE_ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected then
    begin
      TE_Name_Edit.Text := T_TE_Item(Item).Caption;
      TE_Memo.Text := T_TE_Item(Item).TE.AsText;
    end
  else
      Clear_TE_Text;
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.UpdateMemoTo_TE_ButtonClick(Sender: TObject);
var
  TE: THashTextEngine;
begin
  if TEKeyValue_Client = nil then
      exit;
  TE := THashTextEngine.Create;
  TE.AsText := TE_Memo.Text;
  TEKeyValue_Client.SetTE(TE_Name_Edit.Text, TE);
  DisposeObject(TE);
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  if logMemo.Lines.Count > 2000 then
      logMemo.Clear;
  logMemo.Lines.Add(DateTimeToStr(now) + ' ' + Text_);
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.ReadConfig;
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

procedure TDTC40_TEKeyValue_AdminTool_Form.WriteConfig;
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

procedure TDTC40_TEKeyValue_AdminTool_Form.Do_QueryResult(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
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

procedure TDTC40_TEKeyValue_AdminTool_Form.Clear_TE_List;
begin
  TE_ListView.Clear;
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.Clear_TE_Text;
begin
  TE_Memo.Clear;
  TE_Name_Edit.Text := '';
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.Do_SearchTE(Sender: TC40_TEKeyValue_Client; arry: TC40_TEKeyValue_Client_SearchTE_Result_Array);
var
  i: Integer;
begin
  Clear_TE_List;
  TE_ListView.Items.BeginUpdate;
  for i := low(arry) to high(arry) do
    begin
      with T_TE_Item(TE_ListView.Items.Add) do
        begin
          Caption := arry[i].Name;
          TE.SwapInstance(arry[i].Instance_);
          ImageIndex := -1;
          StateIndex := -1;
        end;
    end;
  TE_ListView.Items.EndUpdate;
  TE_ListView.Width := TE_ListView.Width + 1;
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
begin
  DoStatus('connect to "%s" port %d ok.', [Sender.PhysicsAddr.Text, Sender.PhysicsPort]);
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
begin
  serviceComboBox.Clear;
  ValidService.Clear;
  TEKeyValue_Client := nil;
  Clear_TE_List;
  Clear_TE_Text;
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
  DoStatus('Build network: %s classes: %s', [Custom_Client_.ClientInfo.ServiceTyp.Text, Custom_Client_.ClassName]);
end;

procedure TDTC40_TEKeyValue_AdminTool_Form.C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
  if Custom_Client_ is TC40_TEKeyValue_Client then
    begin
      TEKeyValue_Client := Custom_Client_ as TC40_TEKeyValue_Client;
      Clear_TE_List;
      Clear_TE_Text;
      search_TE_ButtonClick(search_TE_Button);
    end;
end;

constructor TDTC40_TEKeyValue_AdminTool_Form.Create(AOwner: TComponent);
var
  i: Integer;
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
      if p^.ClientClass.InheritsFrom(TC40_TEKeyValue_Client) then
        begin
          if depend_.L > 0 then
              depend_.Append('|');
          depend_.Append(p^.ServiceTyp);
        end;
    end;
  DependEdit.Text := depend_.Text;

  C40_PhysicsTunnel_Disconnect(nil);
end;

destructor TDTC40_TEKeyValue_AdminTool_Form.Destroy;
begin
  C40Clean;
  RemoveDoStatusHook(self);
  inherited Destroy;
end;

end.
