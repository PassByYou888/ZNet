unit _3_User_LoginFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,

  Vcl.FileCtrl,
  System.IOUtils, System.DateUtils, System.TypInfo,

  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status,
  Z.ListEngine, Z.GHashList, Z.Expression, Z.OpCode, Z.Parsing, Z.DFE, Z.TextDataEngine,
  Z.Json, Z.Geometry2D, Z.Geometry3D, Z.Number,
  Z.MemoryStream, Z.Cipher, Z.Notify, Z.IOThread,
  Z.Net,
  Z.Net.C4, Z.Net.C4_UserDB, Z.Net.C4_Var, Z.Net.C4_FS, Z.Net.C4_RandSeed, Z.Net.C4_Log_DB, Z.Net.C4_XNAT,
  Z.Net.C4_PascalRewrite_Client,
  Z.Net.PhysicsIO,
  MyCustomService;

type
  Tuser_login_Form = class(TForm, IC40_PhysicsTunnel_Event)
    TopBarPanel: TPanel;
    JoinHostEdit: TLabeledEdit;
    JoinPortEdit: TLabeledEdit;
    BuildDependNetButton: TButton;
    resetDependButton: TButton;
    serviceComboBox: TComboBox;
    queryButton: TButton;
    netTimer: TTimer;
    logMemo: TMemo;
    _B_Splitter: TSplitter;
    cliPanel: TPanel;
    userEdit: TLabeledEdit;
    passwdEdit: TLabeledEdit;
    reguserButton: TButton;
    loginUserButton: TButton;
    discButton: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure netTimerTimer(Sender: TObject);
    procedure queryButtonClick(Sender: TObject);
    procedure BuildDependNetButtonClick(Sender: TObject);
    procedure loginUserButtonClick(Sender: TObject);
    procedure reguserButtonClick(Sender: TObject);
    procedure resetDependButtonClick(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
    procedure ReadConfig;
    procedure WriteConfig;
    procedure Do_QueryResult(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
  private
    procedure C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    procedure C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
  public
    ValidService: TC40_InfoList;
    MyClient: TMyCustom_Client;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  user_login_Form: Tuser_login_Form;

implementation

{$R *.dfm}


procedure Tuser_login_Form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  WriteConfig;
end;

procedure Tuser_login_Form.netTimerTimer(Sender: TObject);
begin
  Z.Net.C4.C40Progress;
end;

procedure Tuser_login_Form.queryButtonClick(Sender: TObject);
var
  tunnel_: TC40_PhysicsTunnel;
begin
  tunnel_ := C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(JoinHostEdit.Text, EStrToInt(JoinPortEdit.Text, 0));
  tunnel_.QueryInfoM(Do_QueryResult);
end;

procedure Tuser_login_Form.BuildDependNetButtonClick(Sender: TObject);
var
  info: TC40_Info;
begin
  if serviceComboBox.ItemIndex < 0 then
      exit;
  info := TC40_Info(serviceComboBox.Items.Objects[serviceComboBox.ItemIndex]);
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(info, info.ServiceTyp, self);
end;

procedure Tuser_login_Form.loginUserButtonClick(Sender: TObject);
begin
  if MyClient = nil then
      exit;
  if MyClient.DTVirtualAuth.LinkOk then
      exit;
  MyClient.Client.RegisterUserAndLogin := False;
  MyClient.Client.Connect_P(userEdit.Text, passwdEdit.Text, procedure(const State: Boolean)
    begin
      if State then
          DoStatus('login successed')
      else
          DoStatus('login failed.');
    end);
end;

procedure Tuser_login_Form.reguserButtonClick(Sender: TObject);
begin
  if MyClient = nil then
      exit;
  if MyClient.DTVirtualAuth.LinkOk then
      exit;
  MyClient.Client.RegisterUserAndLogin := True;
  MyClient.Client.Connect_P(userEdit.Text, passwdEdit.Text, procedure(const State: Boolean)
    begin
      if State then
          DoStatus('reg successed')
      else
          DoStatus('reg failed.');
    end);
end;

procedure Tuser_login_Form.resetDependButtonClick(Sender: TObject);
begin
  C40Clean_Client;
end;

procedure Tuser_login_Form.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  if logMemo.Lines.Count > 2000 then
      logMemo.Clear;
  logMemo.Lines.Add(DateTimeToStr(now) + ' ' + Text_);
end;

procedure Tuser_login_Form.ReadConfig;
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

procedure Tuser_login_Form.WriteConfig;
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

procedure Tuser_login_Form.Do_QueryResult(Sender: TC40_PhysicsTunnel; L: TC40_InfoList);
var
  arry: TC40_Info_Array;
  i: Integer;
begin
  ValidService.Clear;
  arry := L.SearchService(ExtractDependInfo(GetRegisterServiceTypFromClass(TMyCustom_Client)));
  for i := low(arry) to high(arry) do
      ValidService.Add(arry[i].Clone);

  serviceComboBox.Clear;
  for i := 0 to ValidService.Count - 1 do
      serviceComboBox.AddItem(Format('"%s" host "%s" port %d', [ValidService[i].ServiceTyp.Text, ValidService[i].PhysicsAddr.Text, ValidService[i].PhysicsPort]), ValidService[i]);

  if serviceComboBox.Items.Count > 0 then
      serviceComboBox.ItemIndex := 0;
end;

procedure Tuser_login_Form.C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
begin
  DoStatus('connect to "%s" port %d ok.', [Sender.PhysicsAddr.Text, Sender.PhysicsPort]);
end;

procedure Tuser_login_Form.C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
begin
  serviceComboBox.Clear;
  ValidService.Clear;
  MyClient := nil;
  cliPanel.Visible := False;
end;

procedure Tuser_login_Form.C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
  DoStatus('Build network: %s classes: %s', [Custom_Client_.ClientInfo.ServiceTyp.Text, Custom_Client_.ClassName]);
  if Custom_Client_ is TMyCustom_Client then
    begin
      MyClient := Custom_Client_ as TMyCustom_Client;
      cliPanel.Visible := True;
    end;
end;

procedure Tuser_login_Form.C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
end;

constructor Tuser_login_Form.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AddDoStatusHook(self, DoStatus_backcall);
  Z.Net.C4.C40_QuietMode := False;

  ReadConfig;

  ValidService := TC40_InfoList.Create(True);
  C40_PhysicsTunnel_Disconnect(nil);
end;

destructor Tuser_login_Form.Destroy;
begin
  C40Clean;
  RemoveDoStatusHook(self);
  inherited Destroy;
end;

end.
