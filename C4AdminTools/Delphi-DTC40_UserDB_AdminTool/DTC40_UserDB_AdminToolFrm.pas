unit DTC40_UserDB_AdminToolFrm;

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
  Z.Net.C4_NetDisk_Client, Z.Net.C4_NetDisk_Service,
  Z.Net.PhysicsIO, Z.MediaCenter;

type
  TDTC40_UserDB_AdminToolForm = class(TForm, IC40_PhysicsTunnel_Event)
    TopBarPanel: TPanel;
    logMemo: TMemo;
    botSplitter: TSplitter;
    JoinHostEdit: TLabeledEdit;
    JoinPortEdit: TLabeledEdit;
    DependEdit: TLabeledEdit;
    BuildDependNetButton: TButton;
    resetDependButton: TButton;
    cliPanel: TPanel;
    leftPanel: TPanel;
    lpLSplitter: TSplitter;
    listToolBarPanel: TPanel;
    UserListView: TListView;
    serviceComboBox: TComboBox;
    queryButton: TButton;
    SearchEdit: TLabeledEdit;
    SearchButton: TButton;
    jsonMemo: TMemo;
    DTC4PasswdEdit: TLabeledEdit;
    netTimer: TTimer;
    Action_List: TActionList;
    Action_downloadtoDir: TAction;
    uploadJson_OpenDialog: TOpenDialog;
    Action_UploadJson: TAction;
    Action_LargeScaleRegistrationTool: TAction;
    NumEdit: TLabeledEdit;
    Action_Kick: TAction;
    Action_Enabled: TAction;
    Action_Disable: TAction;
    Action_Remove: TAction;
    PopupMenu_: TPopupMenu;
    Downloadselectedtodirectory1: TMenuItem;
    UploadjsontoUserDB1: TMenuItem;
    Kick1: TMenuItem;
    Enabled1: TMenuItem;
    Disable1: TMenuItem;
    Remove1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    MainMenu_: TMainMenu;
    File1: TMenuItem;
    Kick2: TMenuItem;
    Disable2: TMenuItem;
    Enabled2: TMenuItem;
    Remove2: TMenuItem;
    N3: TMenuItem;
    Downloadselectedtodirectory2: TMenuItem;
    UploadjsontoUserDB2: TMenuItem;
    N4: TMenuItem;
    LargeScaleRegistrationTool2: TMenuItem;
    Action_UserDB_State: TAction;
    UserDBServiceState1: TMenuItem;
    UserDBServiceState2: TMenuItem;
    Action_exit: TAction;
    Exit1: TMenuItem;
    procedure Action_downloadtoDirExecute(Sender: TObject);
    procedure Action_KickExecute(Sender: TObject);
    procedure Action_EnabledExecute(Sender: TObject);
    procedure Action_DisableExecute(Sender: TObject);
    procedure Action_RemoveExecute(Sender: TObject);
    procedure Action_UploadJsonExecute(Sender: TObject);
    procedure Action_UserDB_StateExecute(Sender: TObject);
    procedure Action_LargeScaleRegistrationToolExecute(Sender: TObject);
    procedure Action_exitExecute(Sender: TObject);
    procedure BuildDependNetButtonClick(Sender: TObject);
    procedure netTimerTimer(Sender: TObject);
    procedure queryButtonClick(Sender: TObject);
    procedure DTC4PasswdEditChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SearchButtonClick(Sender: TObject);
    procedure resetDependButtonClick(Sender: TObject);
    procedure SearchEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UserListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
    procedure UserListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
    procedure ReadConfig;
    procedure WriteConfig;
    procedure Do_QueryResult(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
    procedure DoConnected;
    procedure DoDisconnect;
    procedure Do_Usr_IsOpen(Sender: TC40_UserDB_Client; State_: TArrayBool);
    procedure Do_Usr_Serarch(Sender: TPeerIO; Result_: TDFE);
    procedure RefreshUserList(Text_: U_String; maxNum_: Integer);
    procedure Do_Usr_OnlineNum(Sender: TC40_UserDB_Client; Online_Num, User_Num: Integer);
  private
    // IDTC40_PhysicsTunnel_Event
    procedure C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    procedure C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
  public
    ValidService: TC40_InfoList;
    CurrentClient: TC40_UserDB_Client;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  DTC40_UserDB_AdminToolForm: TDTC40_UserDB_AdminToolForm;

implementation

{$R *.dfm}


uses DTC40_UserDB_AdminLargeScaleRegFrm;

type
  TUsr_Item = class(TListItem)
  public
    json: TZJ;
    PrimaryIdentifier: U_String;
    Ready: Boolean;
    constructor Create(AOwner: TListItems); override;
    destructor Destroy; override;
  end;

constructor TUsr_Item.Create(AOwner: TListItems);
begin
  inherited;
  json := TZJ.Create;
  Ready := False;
end;

destructor TUsr_Item.Destroy;
begin
  DisposeObject(json);
  inherited;
end;

procedure TDTC40_UserDB_AdminToolForm.Action_downloadtoDirExecute(Sender: TObject);
var
  i: Integer;
  dir: string;
  itm: TUsr_Item;
begin
  if UserListView.SelCount <= 0 then
      exit;
  if CurrentClient = nil then
      exit;

  dir := umlCurrentDirectory;
  if not SelectDirectory('downlaod to...', '', dir) then
      exit;

  for i := 0 to UserListView.Items.Count - 1 do
    begin
      itm := UserListView.Items[i] as TUsr_Item;
      if itm.Selected then
          itm.json.SaveToFile(umlCombineFileName(dir, itm.PrimaryIdentifier + '.json'));
    end;
end;

procedure TDTC40_UserDB_AdminToolForm.Action_KickExecute(Sender: TObject);
var
  i: Integer;
  itm: TUsr_Item;
begin
  if CurrentClient = nil then
      exit;

  for i := 0 to UserListView.Items.Count - 1 do
    begin
      itm := UserListView.Items[i] as TUsr_Item;
      if itm.Selected then
          CurrentClient.Usr_Kick(itm.PrimaryIdentifier);
    end;
end;

procedure TDTC40_UserDB_AdminToolForm.Action_EnabledExecute(Sender: TObject);
var
  i: Integer;
  itm: TUsr_Item;
begin
  if CurrentClient = nil then
      exit;

  for i := 0 to UserListView.Items.Count - 1 do
    begin
      itm := UserListView.Items[i] as TUsr_Item;
      if itm.Selected then
          CurrentClient.Usr_Enabled(itm.PrimaryIdentifier);
    end;
end;

procedure TDTC40_UserDB_AdminToolForm.Action_DisableExecute(Sender: TObject);
var
  i: Integer;
  itm: TUsr_Item;
begin
  if CurrentClient = nil then
      exit;

  for i := 0 to UserListView.Items.Count - 1 do
    begin
      itm := UserListView.Items[i] as TUsr_Item;
      if itm.Selected then
          CurrentClient.Usr_Disable(itm.PrimaryIdentifier);
    end;
end;

procedure TDTC40_UserDB_AdminToolForm.Action_RemoveExecute(Sender: TObject);
var
  i: Integer;
  itm: TUsr_Item;
begin
  if CurrentClient = nil then
      exit;

  if MessageDlg('remove?', mtWarning, [mbYes, mbNo], 0) <> mrYes then
      exit;
  for i := 0 to UserListView.Items.Count - 1 do
    begin
      itm := UserListView.Items[i] as TUsr_Item;
      if itm.Selected then
          CurrentClient.Usr_Remove(itm.PrimaryIdentifier);
    end;
end;

procedure TDTC40_UserDB_AdminToolForm.Action_UploadJsonExecute(Sender: TObject);
var
  i: Integer;
  L: TZJL;
begin
  if CurrentClient = nil then
      exit;
  if not uploadJson_OpenDialog.Execute then
      exit;

  L := TZJL.Create(True);
  for i := 0 to uploadJson_OpenDialog.Files.Count - 1 do
      L.AddFromFile(uploadJson_OpenDialog.Files[i]);
  CurrentClient.Usr_Upload(L);
  DisposeObject(L);
end;

procedure TDTC40_UserDB_AdminToolForm.Action_UserDB_StateExecute(Sender: TObject);
begin
  if CurrentClient = nil then
      exit;
  CurrentClient.Usr_OnlineNumM(Do_Usr_OnlineNum);
end;

procedure TDTC40_UserDB_AdminToolForm.Action_LargeScaleRegistrationToolExecute(Sender: TObject);
begin
  DTC40_UserDB_AdminLargeScaleRegForm.Show;
  DTC40_UserDB_AdminLargeScaleRegForm.RefreshCorpus;
end;

procedure TDTC40_UserDB_AdminToolForm.Action_exitExecute(Sender: TObject);
begin
  Close;
end;

procedure TDTC40_UserDB_AdminToolForm.BuildDependNetButtonClick(Sender: TObject);
var
  info: TC40_Info;
begin
  if serviceComboBox.ItemIndex < 0 then
      exit;
  info := TC40_Info(serviceComboBox.Items.Objects[serviceComboBox.ItemIndex]);
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(info, info.ServiceTyp, self);
end;

procedure TDTC40_UserDB_AdminToolForm.netTimerTimer(Sender: TObject);
begin
  C40Progress;
end;

procedure TDTC40_UserDB_AdminToolForm.queryButtonClick(Sender: TObject);
var
  tunnel_: TC40_PhysicsTunnel;
begin
  tunnel_ := C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(JoinHostEdit.Text, EStrToInt(JoinPortEdit.Text, 0));
  tunnel_.QueryInfoM(Do_QueryResult);
end;

procedure TDTC40_UserDB_AdminToolForm.DTC4PasswdEditChange(Sender: TObject);
begin
  Z.Net.C4.C40_Password := DTC4PasswdEdit.Text;
end;

procedure TDTC40_UserDB_AdminToolForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteConfig;
  CloseAction := caFree;
end;

procedure TDTC40_UserDB_AdminToolForm.SearchButtonClick(Sender: TObject);
begin
  RefreshUserList(SearchEdit.Text, EStrToInt(NumEdit.Text, 1000));
end;

procedure TDTC40_UserDB_AdminToolForm.resetDependButtonClick(Sender: TObject);
begin
  C40Clean;
end;

procedure TDTC40_UserDB_AdminToolForm.SearchEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
      SearchButtonClick(SearchButton);
end;

procedure TDTC40_UserDB_AdminToolForm.UserListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
begin
  ItemClass := TUsr_Item;
end;

procedure TDTC40_UserDB_AdminToolForm.UserListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  m64: TMS64;
begin
  if Selected then
    begin
      m64 := TMS64.Create;
      TUsr_Item(Item).json.SaveToStream(m64, True);
      m64.Position := 0;
      jsonMemo.Lines.LoadFromStream(m64, TEncoding.UTF8);
      m64.Free;
    end
  else
      jsonMemo.Clear;
end;

procedure TDTC40_UserDB_AdminToolForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  if logMemo.Lines.Count > 2000 then
      logMemo.Clear;
  logMemo.Lines.Add(DateTimeToStr(now) + ' ' + Text_);
end;

procedure TDTC40_UserDB_AdminToolForm.ReadConfig;
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

procedure TDTC40_UserDB_AdminToolForm.WriteConfig;
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

procedure TDTC40_UserDB_AdminToolForm.Do_QueryResult(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
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

procedure TDTC40_UserDB_AdminToolForm.DoConnected;
begin
  SearchButtonClick(SearchButton);
end;

procedure TDTC40_UserDB_AdminToolForm.DoDisconnect;
begin
  SysPost.PostExecuteP_NP(1.0, procedure
    begin
      serviceComboBox.Clear;
      UserListView.Items.Clear;
    end);
end;

procedure TDTC40_UserDB_AdminToolForm.Do_Usr_IsOpen(Sender: TC40_UserDB_Client; State_: TArrayBool);
var
  itm: TUsr_Item;
  i: Integer;
begin
  for i := 0 to UserListView.Items.Count - 1 do
    begin
      itm := UserListView.Items[i] as TUsr_Item;
      itm.SubItems[2] := umlBoolToStr(State_[i]);
      itm.Ready := True;
    end;
end;

procedure TDTC40_UserDB_AdminToolForm.Do_Usr_Serarch(Sender: TPeerIO; Result_: TDFE);
var
  itm: TUsr_Item;
  i: Integer;
  arry: U_StringArray;
begin
  if CurrentClient = nil then
      exit;
  UserListView.Items.BeginUpdate;
  UserListView.Items.Clear;
  while Result_.R.NotEnd do
    begin
      itm := UserListView.Items.Add as TUsr_Item;
      itm.json.ParseText(Result_.R.ReadString);
      itm.PrimaryIdentifier := itm.json.S['PrimaryIdentifier'];
      itm.Caption := itm.PrimaryIdentifier;
      itm.SubItems.Add(DateTimeToStr(itm.json.D['LastAuth']));
      itm.SubItems.Add(umlBoolToStr(itm.json.B['Enabled']));
      itm.SubItems.Add('...');
    end;
  UserListView.Items.EndUpdate;
  UserListView.Height := UserListView.Height - 1;
  SetLength(arry, UserListView.Items.Count);
  for i := 0 to UserListView.Items.Count - 1 do
    begin
      itm := UserListView.Items[i] as TUsr_Item;
      arry[i] := itm.PrimaryIdentifier;
    end;
  CurrentClient.Usr_IsOpenM(arry, Do_Usr_IsOpen);
end;

procedure TDTC40_UserDB_AdminToolForm.RefreshUserList(Text_: U_String; maxNum_: Integer);
var
  i: Integer;
  itm: TUsr_Item;
begin
  if CurrentClient = nil then
      exit;

  for i := 0 to UserListView.Items.Count - 1 do
    begin
      itm := UserListView.Items[i] as TUsr_Item;
      if not itm.Ready then
        begin
          DoStatus('busy.');
          exit;
        end;
    end;

  CurrentClient.Usr_SearchM(Text_, maxNum_, Do_Usr_Serarch);
end;

procedure TDTC40_UserDB_AdminToolForm.Do_Usr_OnlineNum(Sender: TC40_UserDB_Client; Online_Num, User_Num: Integer);
begin
  ShowMessage(Format('online:%d registated user:%d', [Online_Num, User_Num]));
end;

procedure TDTC40_UserDB_AdminToolForm.C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
begin

end;

procedure TDTC40_UserDB_AdminToolForm.C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
begin
  if Sender.DependNetworkClientPool.IndexOf(CurrentClient) >= 0 then
    begin
      DoDisconnect;
      ValidService.Clear;
      CurrentClient := nil;
    end;
end;

procedure TDTC40_UserDB_AdminToolForm.C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin

end;

procedure TDTC40_UserDB_AdminToolForm.C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
var
  info: TC40_Info;
begin
  if serviceComboBox.ItemIndex < 0 then
      exit;
  info := TC40_Info(serviceComboBox.Items.Objects[serviceComboBox.ItemIndex]);
  if info.Same(Custom_Client_.ClientInfo) and (Custom_Client_ is TC40_UserDB_Client) then
    begin
      CurrentClient := TC40_UserDB_Client(Custom_Client_);
      DoConnected;
    end;
end;

constructor TDTC40_UserDB_AdminToolForm.Create(AOwner: TComponent);
var
  i: Integer;
  p: PC40_RegistedData;
  depend_: U_String;
begin
  inherited Create(AOwner);
  C40_QuietMode := False;
  AddDoStatusHook(self, DoStatus_backcall);
  InitGlobalMedia([gmtDict]);

  DTC4PasswdEdit.Text := Z.Net.C4.C40_Password;
  ReadConfig;
  ValidService := TC40_InfoList.Create(True);
  CurrentClient := nil;

  depend_ := '';
  for i := 0 to C40_Registed.Count - 1 do
    begin
      p := C40_Registed[i];
      if p^.ClientClass.InheritsFrom(TC40_UserDB_Client) then
        begin
          if depend_.L > 0 then
              depend_.Append('|');
          depend_.Append(p^.ServiceTyp);
        end;
    end;
  DependEdit.Text := depend_;
end;

destructor TDTC40_UserDB_AdminToolForm.Destroy;
begin
  C40Clean;
  RemoveDoStatusHook(self);
  inherited Destroy;
end;

end.
