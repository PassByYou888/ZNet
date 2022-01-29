unit C40AppTempletFrm;

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
  Z.Net.C4, Z.Net.C4_UserDB, Z.Net.C4_Var, Z.Net.C4_FS, Z.Net.C4_RandSeed, Z.Net.C4_Log_DB, Z.Net.C4_XNAT, Z.Net.C4_Alias,
  Z.Net.C4_FS2,
  Z.Net.PhysicsIO;

type
  TC40AppTempletForm = class(TForm)
    logMemo: TMemo;
    botSplitter: TSplitter;
    PGControl: TPageControl;
    BuildNetworkTabSheet: TTabSheet;
    netTimer: TTimer;
    OptTabSheet: TTabSheet;
    DependNetToolPanel: TPanel;
    JoinHostEdit: TLabeledEdit;
    JoinPortEdit: TLabeledEdit;
    BuildDependNetButton: TButton;
    resetDependButton: TButton;
    DependEdit: TLabeledEdit;
    DependNetListView: TListView;
    DependPanel: TPanel;
    servicePanel: TPanel;
    net_Top_Splitter: TSplitter;
    ServiceToolPanel: TPanel;
    ServListeningIPEdit: TLabeledEdit;
    ServPortEdit: TLabeledEdit;
    ServiceDependEdit: TLabeledEdit;
    ServBuildNetButton: TButton;
    ServiceListView: TListView;
    LocalServiceStates_TabSheet: TTabSheet;
    QuietCheckBox: TCheckBox;
    SafeCheckTimerEdit: TLabeledEdit;
    PhysicsReconnectionDelayEdit: TLabeledEdit;
    UpdateServiceInfoTimerEdit: TLabeledEdit;
    PhysicsServiceTimeoutEdit: TLabeledEdit;
    PhysicsTunnelTimeoutEdit: TLabeledEdit;
    KillIDCFaultTimeoutEdit: TLabeledEdit;
    RootDirectoryEdit: TLabeledEdit;
    SelRootDirButton: TButton;
    passwdEdit: TLabeledEdit;
    ApplyOptButton: TButton;
    ResetOptButton: TButton;
    ServiceResetButton: TButton;
    ServiceInfoMemo: TMemo;
    TunnelStatesTabSheet: TTabSheet;
    TunnelInfoMemo: TMemo;
    ServInfoPhyAddrListBox: TListBox;
    localserinfoLSplitter: TSplitter;
    TunnelInfoPhyAddrListBox: TListBox;
    tunnel_infoLSplitter: TSplitter;
    UpdateStateTimer: TTimer;
    SaaS_Network_States_TabSheet: TTabSheet;
    SaaS_Info_TreeView: TTreeView;
    cmd_tool_TabSheet: TTabSheet;
    cmdLineParamEdit: TLabeledEdit;
    GenerateCmdLineButton: TButton;
    cmdLineTitleEdit: TLabeledEdit;
    cmdLineAppTitleEdit: TLabeledEdit;
    cmdLineDisableUICheckBox: TCheckBox;
    ArryParamMemo: TMemo;
    codeParamEdit: TLabeledEdit;
    ArryParamLabel: TLabel;
    Pas_RadioButton: TRadioButton;
    c_RadioButton: TRadioButton;
    ServIPEdit: TLabeledEdit;
    procedure netTimerTimer(Sender: TObject);
    procedure UpdateStateTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure DependEditChange(Sender: TObject);
    procedure DependEditExit(Sender: TObject);
    procedure DependNetListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure BuildDependNetButtonClick(Sender: TObject);
    procedure resetDependButtonClick(Sender: TObject);
    procedure ServiceDependEditChange(Sender: TObject);
    procedure ServiceDependEditExit(Sender: TObject);
    procedure ServiceListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ServBuildNetButtonClick(Sender: TObject);
    procedure ServiceResetButtonClick(Sender: TObject);
    procedure SelRootDirButtonClick(Sender: TObject);
    procedure ApplyOptButtonClick(Sender: TObject);
    procedure ResetOptButtonClick(Sender: TObject);
    procedure ServInfoPhyAddrListBoxClick(Sender: TObject);
    procedure TunnelInfoPhyAddrListBoxClick(Sender: TObject);
    procedure SaaS_Info_TreeViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GenerateCmdLineButtonClick(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
    procedure ReadConfig;
    procedure WriteConfig;
    function RebuildDependInfo(sour: U_String): U_String;
    function RebuildServiceInfo(sour: U_String): U_String;
    procedure RefreshDependReg(info: U_String);
    procedure RefreshServiceReg(info: U_String);
    procedure ReloadOpt;
    procedure ApplyOpt;
    procedure UpdateServiceInfo; overload;
    procedure UpdateServiceInfo(phy_serv: TC40_PhysicsService; dest: TStrings); overload;
    procedure UpdateTunnelInfo; overload;
    procedure UpdateTunnelInfo(phy_tunnel: TC40_PhysicsTunnel; dest: TStrings); overload;
    procedure UpdateSaaSInfo;
    class function GetPathTreeNode(Text_, Split_: U_String; TreeView_: TTreeView; RootNode_: TTreeNode): TTreeNode;
  public
    IsCommandLineWorkEnvir: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExtractAndProcessCmdLine(param_: U_StringArray): Boolean;
  end;

var
  C40AppTempletForm: TC40AppTempletForm;
  C40AppParam: U_StringArray = [];
  C40AppParsingTextStyle: TTextStyle = TTextStyle.tsPascal;
  On_C40_PhysicsTunnel_Event: IC40_PhysicsTunnel_Event = nil;
  On_C40_PhysicsService_Event: IC40_PhysicsService_Event = nil;

procedure InitC40AppParamFromSystemCmdLine;

implementation

{$R *.dfm}


procedure InitC40AppParamFromSystemCmdLine;
var
  i: Integer;
begin
  SetLength(C40AppParam, ParamCount);
  for i := 1 to ParamCount do
      C40AppParam[i - 1] := ParamStr(i);
end;

type
  TCmd_Net_Info_ = record
    listen_ip: string;
    ip: string;
    port: Word;
    depend: string;
  end;

  TCmd_Net_Info_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TCmd_Net_Info_>;

  TCommand_Script = class
  private
    function Do_Config(var OP_Param: TOpParam): Variant;
    function Do_Client(var OP_Param: TOpParam): Variant;
    function Do_Service(var OP_Param: TOpParam): Variant;
  public
    opRT: TOpCustomRunTime;
    Config: THashStringList;
    ConfigIsUpdate: Boolean;
    Client_NetInfo_List: TCmd_Net_Info_List;
    Service_NetInfo_List: TCmd_Net_Info_List;
    constructor Create;
    destructor Destroy; override;
    procedure RegApi;
    procedure Parsing(Expression: U_String);
  end;

function TCommand_Script.Do_Config(var OP_Param: TOpParam): Variant;
begin
  if length(OP_Param) > 0 then
    begin
      Config.SetDefaultValue(opRT.Trigger^.Name, VarToStr(OP_Param[0]));
      Result := True;
      ConfigIsUpdate := True;
    end
  else
      Result := Config[opRT.Trigger^.Name];
end;

function TCommand_Script.Do_Client(var OP_Param: TOpParam): Variant;
var
  net_info_: TCmd_Net_Info_;
begin
  net_info_.listen_ip := '';
  net_info_.ip := OP_Param[0];
  net_info_.port := OP_Param[1];
  net_info_.depend := OP_Param[2];
  Client_NetInfo_List.Add(net_info_);
  Result := True;
end;

function TCommand_Script.Do_Service(var OP_Param: TOpParam): Variant;
var
  net_info_: TCmd_Net_Info_;
begin
  if length(OP_Param) > 3 then
    begin
      net_info_.listen_ip := OP_Param[0];
      net_info_.ip := OP_Param[1];
      net_info_.port := OP_Param[2];
      net_info_.depend := OP_Param[3];
      Service_NetInfo_List.Add(net_info_);
    end
  else if length(OP_Param) = 3 then
    begin
      net_info_.ip := OP_Param[0];
      if Z.Net.IsIPv4(net_info_.ip) then
          net_info_.listen_ip := '0.0.0.0'
      else if Z.Net.IsIPV6(net_info_.ip) then
          net_info_.listen_ip := '::'
      else
          net_info_.listen_ip := '0.0.0.0';

      net_info_.port := OP_Param[1];
      net_info_.depend := OP_Param[2];
      Service_NetInfo_List.Add(net_info_);
    end;
  Result := True;
end;

constructor TCommand_Script.Create;
begin
  inherited Create;
  opRT := TOpCustomRunTime.Create;

  Config := THashStringList.Create;
  ConfigIsUpdate := False;

  Client_NetInfo_List := TCmd_Net_Info_List.Create;
  Service_NetInfo_List := TCmd_Net_Info_List.Create;
end;

destructor TCommand_Script.Destroy;
begin
  disposeObject(Client_NetInfo_List);
  disposeObject(Service_NetInfo_List);
  disposeObject(opRT);
  disposeObject(Config);
  inherited Destroy;
end;

procedure TCommand_Script.RegApi;
var
  L: TListPascalString;
  i: Integer;
begin
  L := TListPascalString.Create;
  Config.GetNameList(L);
  for i := 0 to L.Count - 1 do
    begin
      opRT.RegOpM(L[i], Do_Config);
    end;
  disposeObject(L);

  opRT.RegOpM('Service', Do_Service);
  opRT.RegOpM('Serv', Do_Service);
  opRT.RegOpM('Listen', Do_Service);
  opRT.RegOpM('Listening', Do_Service);
  opRT.RegOpM('Client', Do_Client);
  opRT.RegOpM('Cli', Do_Client);
  opRT.RegOpM('Tunnel', Do_Client);
  opRT.RegOpM('Connect', Do_Client);
  opRT.RegOpM('Connection', Do_Client);
  opRT.RegOpM('Net', Do_Client);
  opRT.RegOpM('Build', Do_Client);
end;

procedure TCommand_Script.Parsing(Expression: U_String);
begin
  EvaluateExpressionValue(False, C40AppParsingTextStyle, Expression, opRT);
end;

procedure TC40AppTempletForm.netTimerTimer(Sender: TObject);
begin
  C40Progress;
end;

procedure TC40AppTempletForm.UpdateStateTimerTimer(Sender: TObject);
begin
  if WindowState = wsMinimized then
      exit;
  UpdateServiceInfo;
  UpdateTunnelInfo;
  UpdateSaaSInfo;
end;

procedure TC40AppTempletForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteConfig;
  CloseAction := caFree;
end;

procedure TC40AppTempletForm.DependEditChange(Sender: TObject);
var
  i, j: Integer;
  p: PC40_RegistedData;
  arry: TC40_DependNetworkInfoArray;
  found_: Boolean;
begin
  DependNetListView.OnChange := nil;
  arry := ExtractDependInfo(DependEdit.Text);
  for i := 0 to DependNetListView.Items.Count - 1 do
    begin
      p := DependNetListView.Items[i].Data;
      found_ := False;
      for j := Low(arry) to high(arry) do
        if arry[j].Typ.Same(@p^.ServiceTyp) then
          begin
            found_ := True;
            break;
          end;
      DependNetListView.Items[i].Checked := found_;
    end;
  DependNetListView.OnChange := DependNetListViewChange;
end;

procedure TC40AppTempletForm.DependEditExit(Sender: TObject);
begin
  DependEdit.OnChange := nil;
  DependEdit.Text := RebuildDependInfo(DependEdit.Text);
  DependEdit.OnChange := DependEditChange;
end;

procedure TC40AppTempletForm.DependNetListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  DependEdit.OnChange := nil;
  DependEdit.Text := RebuildDependInfo(DependEdit.Text);
  DependEdit.OnChange := DependEditChange;
end;

procedure TC40AppTempletForm.BuildDependNetButtonClick(Sender: TObject);
begin
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(JoinHostEdit.Text, EStrToInt(JoinPortEdit.Text, 0), DependEdit.Text, On_C40_PhysicsTunnel_Event);
end;

procedure TC40AppTempletForm.resetDependButtonClick(Sender: TObject);
begin
  C40Clean_Client;
end;

procedure TC40AppTempletForm.ServiceDependEditChange(Sender: TObject);
var
  i, j: Integer;
  p: PC40_RegistedData;
  arry: TC40_DependNetworkInfoArray;
  found_: Boolean;
begin
  ServiceListView.OnChange := nil;
  arry := ExtractDependInfo(ServiceDependEdit.Text);
  for i := 0 to ServiceListView.Items.Count - 1 do
    begin
      p := ServiceListView.Items[i].Data;
      found_ := False;
      for j := Low(arry) to high(arry) do
        if arry[j].Typ.Same(@p^.ServiceTyp) then
          begin
            found_ := True;
            break;
          end;
      ServiceListView.Items[i].Checked := found_;
    end;
  ServiceListView.OnChange := ServiceListViewChange;
end;

procedure TC40AppTempletForm.ServiceDependEditExit(Sender: TObject);
begin
  ServiceDependEdit.OnChange := nil;
  ServiceDependEdit.Text := RebuildServiceInfo(ServiceDependEdit.Text);
  ServiceDependEdit.OnChange := ServiceDependEditChange;
end;

procedure TC40AppTempletForm.ServiceListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  ServiceDependEdit.OnChange := nil;
  ServiceDependEdit.Text := RebuildServiceInfo(ServiceDependEdit.Text);
  ServiceDependEdit.OnChange := ServiceDependEditChange;
end;

procedure TC40AppTempletForm.ServBuildNetButtonClick(Sender: TObject);
begin
  with Z.Net.C4.TC40_PhysicsService.Create(ServListeningIPEdit.Text,
    ServIPEdit.Text, EStrToInt(ServPortEdit.Text, 0), Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork(ServiceDependEdit.Text);
      OnEvent := On_C40_PhysicsService_Event;
      StartService;
    end;
end;

procedure TC40AppTempletForm.ServiceResetButtonClick(Sender: TObject);
begin
  Z.Net.C4.C40Clean_Service;
end;

procedure TC40AppTempletForm.SelRootDirButtonClick(Sender: TObject);
var
  dir_: String;
begin
  dir_ := RootDirectoryEdit.Text;
  if SelectDirectory('Select C40 Root Directory.', '', dir_, [sdNewFolder, sdNewUI, sdValidateDir]) then
      RootDirectoryEdit.Text := dir_;
end;

procedure TC40AppTempletForm.ApplyOptButtonClick(Sender: TObject);
begin
  ApplyOpt;
end;

procedure TC40AppTempletForm.ResetOptButtonClick(Sender: TObject);
begin
  C40ResetDefaultConfig;
  ReloadOpt;
end;

procedure TC40AppTempletForm.ServInfoPhyAddrListBoxClick(Sender: TObject);
var
  i: Integer;
begin
  ServiceInfoMemo.Lines.BeginUpdate;
  for i := 0 to ServInfoPhyAddrListBox.Items.Count - 1 do
    if ServInfoPhyAddrListBox.Selected[i] then
      begin
        ServiceInfoMemo.Clear;
        UpdateServiceInfo(ServInfoPhyAddrListBox.Items.Objects[i] as TC40_PhysicsService, ServiceInfoMemo.Lines);
      end;
  ServiceInfoMemo.Lines.EndUpdate;
end;

procedure TC40AppTempletForm.TunnelInfoPhyAddrListBoxClick(Sender: TObject);
var
  i: Integer;
begin
  TunnelInfoMemo.Lines.BeginUpdate;
  for i := 0 to TunnelInfoPhyAddrListBox.Items.Count - 1 do
    if TunnelInfoPhyAddrListBox.Selected[i] then
      begin
        TunnelInfoMemo.Clear;
        UpdateTunnelInfo(TunnelInfoPhyAddrListBox.Items.Objects[i] as TC40_PhysicsTunnel, TunnelInfoMemo.Lines);
      end;
  TunnelInfoMemo.Lines.EndUpdate;
end;

procedure TC40AppTempletForm.SaaS_Info_TreeViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F5 then
      SaaS_Info_TreeView.Items.Clear;
end;

procedure TC40AppTempletForm.GenerateCmdLineButtonClick(Sender: TObject);
  function conv_(sour: SystemString): SystemString;
  begin
    if Pas_RadioButton.Checked then
        Result := TTextParsing.TranslateTextToPascalDecl(sour)
    else if c_RadioButton.Checked then
        Result := TTextParsing.TranslateTextToC_Decl(sour)
    else
        Result := sour;
  end;

var
  HS: THashStringList;
  param: TPascalStringList;
  final_param: U_String;
  i: Integer;
begin
  HS := THashStringList.Create;
  Z.Net.C4.C40WriteConfig(HS);

  param := TPascalStringList.Create;

  param.Add(Format('Title(%s)', [conv_(cmdLineTitleEdit.Text)]));
  param.Add(Format('AppTitle(%s)', [conv_(cmdLineAppTitleEdit.Text)]));
  param.Add(Format('DisableUI(%s)', [conv_(umlBoolToStr(cmdLineDisableUICheckBox.Checked))]));
  param.Add(Format('Timer(%s)', [conv_(umlIntToStr(netTimer.Interval))]));
  param.Add(Format('Password(%s)', [conv_(Z.Net.C4.C40_Password)]));

  HS.ProgressP(procedure(Sender: THashStringList; Name_: PSystemString; const V: SystemString)
    begin
      if C40_DefaultConfig.GetDefaultValue(Name_^, V) <> V then
          param.Add(Format('%s(%s)', [Name_^, V]));
    end);

  if (ServIPEdit.Text <> '') and (ServPortEdit.Text <> '') and (ServiceDependEdit.Text <> '') then
      param.Add(Format('Service(%s,%s,%s,%s)', [conv_(ServListeningIPEdit.Text), conv_(ServIPEdit.Text), conv_(ServPortEdit.Text), conv_(ServiceDependEdit.Text)]));

  if (JoinHostEdit.Text <> '') and (JoinPortEdit.Text <> '') and (DependEdit.Text <> '') then
      param.Add(Format('Tunnel(%s,%s,%s)', [conv_(JoinHostEdit.Text), conv_(JoinPortEdit.Text), conv_(DependEdit.Text)]));

  final_param := '';
  ArryParamMemo.Clear;
  for i := 0 to param.Count - 1 do
    begin
      if i > 0 then
          final_param.Append(',');
      final_param.Append(param[i]);
      ArryParamMemo.Lines.Add(conv_(param[i]) + if_(i < param.Count - 1, ',', ''));
    end;
  cmdLineParamEdit.Text := '"' + final_param + '"';
  codeParamEdit.Text := conv_(final_param);

  disposeObject(HS);
  disposeObject(param);
end;

procedure TC40AppTempletForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  if logMemo.Lines.Count > 2000 then
      logMemo.Clear;
  logMemo.Lines.Add(DateTimeToStr(now) + ' ' + Text_);
end;

procedure TC40AppTempletForm.ReadConfig;
var
  fn: U_String;
  TE: THashTextEngine;
begin
  if IsCommandLineWorkEnvir then
      exit;
  fn := umlChangeFileExt(Application.ExeName, '.conf');
  if not umlFileExists(fn) then
      exit;
  TE := THashTextEngine.Create;
  TE.LoadFromFile(fn);
  Z.Net.C4.C40ReadConfig(TE.HStringList['Sys']);
  ReloadOpt;

  JoinHostEdit.Text := TE.GetDefaultValue('Main', JoinHostEdit.Name, JoinHostEdit.Text);
  JoinPortEdit.Text := TE.GetDefaultValue('Main', JoinPortEdit.Name, JoinPortEdit.Text);
  DependEdit.Text := TE.GetDefaultValue('Main', DependEdit.Name, DependEdit.Text);
  DependEditExit(DependEdit);

  ServListeningIPEdit.Text := TE.GetDefaultValue('Main', ServListeningIPEdit.Name, ServListeningIPEdit.Text);
  ServIPEdit.Text := TE.GetDefaultValue('Main', ServIPEdit.Name, ServIPEdit.Text);
  ServPortEdit.Text := TE.GetDefaultValue('Main', ServPortEdit.Name, ServPortEdit.Text);
  ServiceDependEdit.Text := TE.GetDefaultValue('Main', ServiceDependEdit.Name, ServiceDependEdit.Text);
  ServiceDependEditExit(ServiceDependEdit);
  disposeObject(TE);
end;

procedure TC40AppTempletForm.WriteConfig;
var
  fn: U_String;
  TE: THashTextEngine;
begin
  if IsCommandLineWorkEnvir then
      exit;
  fn := umlChangeFileExt(Application.ExeName, '.conf');

  TE := THashTextEngine.Create;
  ApplyOpt;
  Z.Net.C4.C40WriteConfig(TE.HStringList['Sys']);

  TE.SetDefaultValue('Main', JoinHostEdit.Name, JoinHostEdit.Text);
  TE.SetDefaultValue('Main', JoinPortEdit.Name, JoinPortEdit.Text);
  TE.SetDefaultValue('Main', DependEdit.Name, DependEdit.Text);

  TE.SetDefaultValue('Main', ServListeningIPEdit.Name, ServListeningIPEdit.Text);
  TE.SetDefaultValue('Main', ServIPEdit.Name, ServIPEdit.Text);
  TE.SetDefaultValue('Main', ServPortEdit.Name, ServPortEdit.Text);
  TE.SetDefaultValue('Main', ServiceDependEdit.Name, ServiceDependEdit.Text);

  TE.SaveToFile(fn);
  disposeObject(TE);
end;

function TC40AppTempletForm.RebuildDependInfo(sour: U_String): U_String;
var
  sourNet, destNet: TC40_DependNetworkInfoList;
  i, j: Integer;
  info: TC40_DependNetworkInfo;
  p: PC40_RegistedData;
begin
  Result := '';
  sourNet := ExtractDependInfoToL(sour);
  destNet := TC40_DependNetworkInfoList.Create;

  for i := 0 to DependNetListView.Items.Count - 1 do
    if DependNetListView.Items[i].Checked then
      begin
        p := DependNetListView.Items[i].Data;
        info.Typ := p^.ServiceTyp;
        for j := 0 to sourNet.Count - 1 do
          if sourNet[j].Typ.Same(@info.Typ) then
            begin
              info.param := sourNet[j].param;
              break;
            end;
        destNet.Add(info);
      end;

  for i := 0 to destNet.Count - 1 do
    begin
      if i > 0 then
          Result.Append('|');
      info := destNet[i];
      Result.Append(info.Typ);
      if info.param.L > 0 then
          Result.Append('@' + info.param);
    end;

  disposeObject(sourNet);
  disposeObject(destNet);
end;

function TC40AppTempletForm.RebuildServiceInfo(sour: U_String): U_String;
var
  sourNet, destNet: TC40_DependNetworkInfoList;
  i, j: Integer;
  info: TC40_DependNetworkInfo;
  p: PC40_RegistedData;
begin
  Result := '';
  sourNet := ExtractDependInfoToL(sour);
  destNet := TC40_DependNetworkInfoList.Create;

  for i := 0 to ServiceListView.Items.Count - 1 do
    if ServiceListView.Items[i].Checked then
      begin
        p := ServiceListView.Items[i].Data;
        info.Typ := p^.ServiceTyp;
        for j := 0 to sourNet.Count - 1 do
          if sourNet[j].Typ.Same(@info.Typ) then
            begin
              info.param := sourNet[j].param;
              break;
            end;
        destNet.Add(info);
      end;

  for i := 0 to destNet.Count - 1 do
    begin
      if i > 0 then
          Result.Append('|');
      info := destNet[i];
      Result.Append(info.Typ);
      if info.param.L > 0 then
          Result.Append('@' + info.param);
    end;

  disposeObject(sourNet);
  disposeObject(destNet);
end;

procedure TC40AppTempletForm.RefreshDependReg(info: U_String);
var
  i, j: Integer;
  p: PC40_RegistedData;
  arry: TC40_DependNetworkInfoArray;
begin
  DependNetListView.Items.BeginUpdate;
  DependNetListView.Items.Clear;
  for i := 0 to Z.Net.C4.C40_Registed.Count - 1 do
    begin
      p := Z.Net.C4.C40_Registed[i];
      if p^.ClientClass <> nil then
        with DependNetListView.Items.Add do
          begin
            Caption := p^.ServiceTyp;
            SubItems.Add(p^.ClientClass.ClassName);
            SubItems.Add(p^.ClientClass.UnitName + '.pas');
            Data := p;
          end;
    end;
  DependNetListView.Items.EndUpdate;

  arry := ExtractDependInfo(info);
  for i := 0 to DependNetListView.Items.Count - 1 do
    begin
      p := DependNetListView.Items[i].Data;
      for j := Low(arry) to high(arry) do
        if arry[j].Typ.Same(@p^.ServiceTyp) then
          begin
            DependNetListView.Items[i].Checked := True;
            break;
          end;
    end;
end;

procedure TC40AppTempletForm.RefreshServiceReg(info: U_String);
var
  i, j: Integer;
  p: PC40_RegistedData;
  arry: TC40_DependNetworkInfoArray;
begin
  ServiceListView.Items.BeginUpdate;
  ServiceListView.Items.Clear;
  for i := 0 to Z.Net.C4.C40_Registed.Count - 1 do
    begin
      p := Z.Net.C4.C40_Registed[i];
      if p^.ServiceClass <> nil then
        with ServiceListView.Items.Add do
          begin
            Caption := p^.ServiceTyp;
            SubItems.Add(p^.ServiceClass.ClassName);
            SubItems.Add(p^.ServiceClass.UnitName + '.pas');
            Data := p;
          end;
    end;
  ServiceListView.Items.EndUpdate;

  arry := ExtractDependInfo(info);
  for i := 0 to ServiceListView.Items.Count - 1 do
    begin
      p := ServiceListView.Items[i].Data;
      for j := Low(arry) to high(arry) do
        if arry[j].Typ.Same(@p^.ServiceTyp) then
          begin
            ServiceListView.Items[i].Checked := True;
            break;
          end;
    end;
end;

procedure TC40AppTempletForm.ReloadOpt;
begin
  QuietCheckBox.Checked := Z.Net.C4.C40_QuietMode;
  SafeCheckTimerEdit.Text := umlIntToStr(Z.Net.C4.C40_SafeCheckTime);
  PhysicsReconnectionDelayEdit.Text := umlShortFloatToStr(Z.Net.C4.C40_PhysicsReconnectionDelayTime);
  UpdateServiceInfoTimerEdit.Text := umlIntToStr(Z.Net.C4.C40_UpdateServiceInfoDelayTime);
  PhysicsServiceTimeoutEdit.Text := umlIntToStr(Z.Net.C4.C40_PhysicsServiceTimeout);
  PhysicsTunnelTimeoutEdit.Text := umlIntToStr(Z.Net.C4.C40_PhysicsTunnelTimeout);
  KillIDCFaultTimeoutEdit.Text := umlIntToStr(Z.Net.C4.C40_KillIDCFaultTimeout);
  RootDirectoryEdit.Text := Z.Net.C4.C40_RootPath;
  passwdEdit.Text := Z.Net.C4.C40_Password;
end;

procedure TC40AppTempletForm.ApplyOpt;
begin
  Z.Net.C4.C40SetQuietMode(QuietCheckBox.Checked);
  Z.Net.C4.C40_SafeCheckTime := EStrToInt(SafeCheckTimerEdit.Text, Z.Net.C4.C40_SafeCheckTime);
  Z.Net.C4.C40_PhysicsReconnectionDelayTime := EStrToDouble(PhysicsReconnectionDelayEdit.Text, Z.Net.C4.C40_PhysicsReconnectionDelayTime);
  Z.Net.C4.C40_UpdateServiceInfoDelayTime := EStrToInt(UpdateServiceInfoTimerEdit.Text, Z.Net.C4.C40_UpdateServiceInfoDelayTime);
  Z.Net.C4.C40_PhysicsServiceTimeout := EStrToInt(PhysicsServiceTimeoutEdit.Text, Z.Net.C4.C40_PhysicsServiceTimeout);
  Z.Net.C4.C40_PhysicsTunnelTimeout := EStrToInt(PhysicsTunnelTimeoutEdit.Text, Z.Net.C4.C40_PhysicsTunnelTimeout);
  Z.Net.C4.C40_KillIDCFaultTimeout := EStrToInt(KillIDCFaultTimeoutEdit.Text, Z.Net.C4.C40_KillIDCFaultTimeout);
  Z.Net.C4.C40_RootPath := RootDirectoryEdit.Text;
  Z.Net.C4.C40_Password := passwdEdit.Text;
end;

procedure TC40AppTempletForm.UpdateServiceInfo;
var
  i: Integer;
  phy_serv: TC40_PhysicsService;
begin
  for i := 0 to C40_PhysicsServicePool.Count - 1 do
    begin
      phy_serv := C40_PhysicsServicePool[i];
      if ServInfoPhyAddrListBox.Items.IndexOfObject(phy_serv) < 0 then
          ServInfoPhyAddrListBox.Items.AddObject(Format('service "%s" port:%d', [phy_serv.PhysicsAddr.Text, phy_serv.PhysicsPort]), phy_serv);
    end;

  i := 0;
  while i < ServInfoPhyAddrListBox.Items.Count do
    if C40_PhysicsServicePool.IndexOf(TC40_PhysicsService(ServInfoPhyAddrListBox.Items.Objects[i])) < 0 then
        ServInfoPhyAddrListBox.Items.Delete(i)
    else
        inc(i);
end;

procedure TC40AppTempletForm.UpdateServiceInfo(phy_serv: TC40_PhysicsService; dest: TStrings);
var
  i: Integer;
  custom_serv: TC40_Custom_Service;
begin
  dest.Add(Format('Physics service: "%s" Unit: "%s"', [phy_serv.PhysicsTunnel.ClassName, phy_serv.PhysicsTunnel.UnitName + '.pas']));
  dest.Add(Format('Physics service workload: %d', [phy_serv.PhysicsTunnel.Count]));
  dest.Add(Format('Physcis Listening ip: "%s" Port: %d', [phy_serv.PhysicsAddr.Text, phy_serv.PhysicsPort]));
  dest.Add(Format('Listening Successed: %s', [if_(phy_serv.Activted, 'Yes', 'Failed')]));
  for i := 0 to phy_serv.DependNetworkServicePool.Count - 1 do
    begin
      dest.Add(Format('--------------------------------------------', []));
      custom_serv := phy_serv.DependNetworkServicePool[i];
      dest.Add(Format('Type: %s', [custom_serv.ServiceInfo.ServiceTyp.Text]));
      dest.Add(Format('workload: %d / %d', [custom_serv.ServiceInfo.Workload, custom_serv.ServiceInfo.MaxWorkload]));
      dest.Add(Format('Only Instance: %s', [if_(custom_serv.ServiceInfo.OnlyInstance, 'Yes', 'More Instance.')]));
      dest.Add(Format('Hash: %s', [umlMD5ToStr(custom_serv.ServiceInfo.Hash).Text]));
      dest.Add(Format('Class: "%s" Unit: "%s"', [custom_serv.ClassName, custom_serv.UnitName + '.pas']));
      dest.Add(Format('Receive Tunnel IP: %s Port: %d',
        [custom_serv.ServiceInfo.p2pVM_RecvTunnel_Addr.Text, custom_serv.ServiceInfo.p2pVM_RecvTunnel_Port]));
      dest.Add(Format('Send Tunnel IP: %s Port: %d',
        [custom_serv.ServiceInfo.p2pVM_SendTunnel_Addr.Text, custom_serv.ServiceInfo.p2pVM_SendTunnel_Port]));
      dest.Add(Format('Workload: %d/%d', [custom_serv.ServiceInfo.Workload, custom_serv.ServiceInfo.MaxWorkload]));
      dest.Add(Format('Parameter', []));
      dest.Add(Format('{', []));
      dest.Add(custom_serv.ParamList.AsText);
      dest.Add(Format('}', []));
    end;
  dest.Add(Format('', []));
end;

procedure TC40AppTempletForm.UpdateTunnelInfo;
var
  i: Integer;
  phy_tunnel: TC40_PhysicsTunnel;
begin
  for i := 0 to C40_PhysicsTunnelPool.Count - 1 do
    begin
      phy_tunnel := C40_PhysicsTunnelPool[i];
      if TunnelInfoPhyAddrListBox.Items.IndexOfObject(phy_tunnel) < 0 then
          TunnelInfoPhyAddrListBox.Items.AddObject(Format('tunnel "%s" port:%d', [phy_tunnel.PhysicsAddr.Text, phy_tunnel.PhysicsPort]), phy_tunnel);
    end;

  i := 0;
  while i < TunnelInfoPhyAddrListBox.Items.Count do
    if C40_PhysicsTunnelPool.IndexOf(TC40_PhysicsTunnel(TunnelInfoPhyAddrListBox.Items.Objects[i])) < 0 then
        TunnelInfoPhyAddrListBox.Items.Delete(i)
    else
        inc(i);
end;

procedure TC40AppTempletForm.UpdateTunnelInfo(phy_tunnel: TC40_PhysicsTunnel; dest: TStrings);
var
  i: Integer;
  custom_client: TC40_Custom_Client;
begin
  dest.Add(Format('Physics tunnel: "%s" Unit: "%s"', [phy_tunnel.PhysicsTunnel.ClassName, phy_tunnel.PhysicsTunnel.UnitName + '.pas']));
  dest.Add(Format('Physcis ip: "%s" Port: %d', [phy_tunnel.PhysicsAddr.Text, phy_tunnel.PhysicsPort]));
  dest.Add(Format('Physcis Connected: %s', [if_(phy_tunnel.PhysicsTunnel.Connected, 'Yes', 'Failed')]));
  for i := 0 to phy_tunnel.DependNetworkClientPool.Count - 1 do
    begin
      dest.Add(Format('--------------------------------------------', []));
      custom_client := phy_tunnel.DependNetworkClientPool[i];
      dest.Add(Format('Type: %s', [custom_client.ClientInfo.ServiceTyp.Text]));
      dest.Add(Format('Connected: %s', [if_(custom_client.Connected, 'Yes', 'Failed')]));
      dest.Add(Format('Only Instance: %s', [if_(custom_client.ClientInfo.OnlyInstance, 'Yes', 'More Instance.')]));
      dest.Add(Format('Hash: %s', [umlMD5ToStr(custom_client.ClientInfo.Hash).Text]));
      dest.Add(Format('Class: "%s" Unit: "%s"', [custom_client.ClassName, custom_client.UnitName + '.pas']));
      dest.Add(Format('Receive Tunnel IP: %s Port: %d',
        [custom_client.ClientInfo.p2pVM_RecvTunnel_Addr.Text, custom_client.ClientInfo.p2pVM_RecvTunnel_Port]));
      dest.Add(Format('Send Tunnel IP: %s Port: %d',
        [custom_client.ClientInfo.p2pVM_SendTunnel_Addr.Text, custom_client.ClientInfo.p2pVM_SendTunnel_Port]));
      dest.Add(Format('Workload: %d/%d', [custom_client.ClientInfo.Workload, custom_client.ClientInfo.MaxWorkload]));
      dest.Add(Format('Parameter', []));
      dest.Add(Format('{', []));
      dest.Add(custom_client.ParamList.AsText);
      dest.Add(Format('}', []));
    end;
  dest.Add(Format('', []));
end;

procedure TC40AppTempletForm.UpdateSaaSInfo;
  procedure Do_Update_Statistics(node_: TTreeNode; F: TZNet);
  var
    st: TStatisticsType;
    n: string;
  begin
    for st := low(TStatisticsType) to high(TStatisticsType) do
      begin
        n := GetEnumName(TypeInfo(TStatisticsType), Ord(st));
        GetPathTreeNode(Format('%s:*@%s: %d', [n, n, F.Statistics[st]]), '|', SaaS_Info_TreeView, node_);
      end;
  end;

var
  i, j: Integer;
  phy_tunnel: TC40_PhysicsTunnel;
  dpc_arry: TC40_Custom_Client_Array;
  phy_serv: TC40_PhysicsService;
  dps_arry: TC40_Custom_Service_Array;
  L: TC40_InfoList;
  nd1, nd2, nd3: TTreeNode;
  cs: TC40_Custom_Service;
  cc: TC40_Custom_Client;
begin
  L := TC40_InfoList.Create(True);
  for i := 0 to C40_PhysicsTunnelPool.Count - 1 do
    begin
      phy_tunnel := C40_PhysicsTunnelPool[i];
      dpc_arry := phy_tunnel.DependNetworkClientPool.SearchClass(Z.Net.C4.TC40_Dispatch_Client, True);
      for j := 0 to length(dpc_arry) - 1 do
          L.MergeAndUpdateWorkload(Z.Net.C4.TC40_Dispatch_Client(dpc_arry[j]).ServiceInfoList);
    end;
  for i := 0 to C40_PhysicsServicePool.Count - 1 do
    begin
      phy_serv := C40_PhysicsServicePool[i];
      dps_arry := phy_serv.DependNetworkServicePool.GetFromClass(Z.Net.C4.TC40_Dispatch_Service);
      for j := 0 to length(dps_arry) - 1 do
          L.MergeAndUpdateWorkload(Z.Net.C4.TC40_Dispatch_Service(dps_arry[j]).ServiceInfoList);
    end;
  for i := 0 to L.Count - 1 do
    begin
      nd1 := GetPathTreeNode(Format('Network Nodes|host: %s port: %d', [L[i].PhysicsAddr.Text, L[i].PhysicsPort]), '|', SaaS_Info_TreeView, nil);
      nd2 := GetPathTreeNode(Format('Type: %s', [L[i].ServiceTyp.Text]), '|', SaaS_Info_TreeView, nd1);
      GetPathTreeNode(Format('hash:*@hash: %s', [umlMD5ToStr(L[i].Hash).Text]), '|', SaaS_Info_TreeView, nd2);
      GetPathTreeNode(Format('workload:*@workload: %d / %d', [L[i].Workload, L[i].MaxWorkload]), '|', SaaS_Info_TreeView, nd2);
    end;
  disposeObject(L);

  for i := 0 to C40_ServicePool.Count - 1 do
    begin
      cs := C40_ServicePool[i];
      nd1 := GetPathTreeNode(Format('Network Nodes|host: %s port: %d', [cs.ServiceInfo.PhysicsAddr.Text, cs.ServiceInfo.PhysicsPort]), '|', SaaS_Info_TreeView, nil);
      nd2 := GetPathTreeNode(Format('Type: %s', [cs.ServiceInfo.ServiceTyp.Text]), '|', SaaS_Info_TreeView, nd1);
      nd3 := GetPathTreeNode(Format('local service is running, class: %s unit: %s', [cs.ClassName, cs.UnitName + '.pas']), '|', SaaS_Info_TreeView, nd2);
    end;

  for i := 0 to C40_ClientPool.Count - 1 do
    begin
      cc := C40_ClientPool[i];
      nd1 := GetPathTreeNode(Format('Network Nodes|host: %s port: %d', [cc.ClientInfo.PhysicsAddr.Text, cc.ClientInfo.PhysicsPort]), '|', SaaS_Info_TreeView, nil);
      nd2 := GetPathTreeNode(Format('Type: %s', [cc.ClientInfo.ServiceTyp.Text]), '|', SaaS_Info_TreeView, nd1);
      nd3 := GetPathTreeNode(Format('local client is running, class: %s unit: %s', [cc.ClassName, cc.UnitName + '.pas']), '|', SaaS_Info_TreeView, nd2);
    end;
end;

class function TC40AppTempletForm.GetPathTreeNode(Text_, Split_: U_String; TreeView_: TTreeView; RootNode_: TTreeNode): TTreeNode;
var
  i: Integer;
  prefix_, match_, value_: U_String;
begin
  prefix_ := umlGetFirstStr(Text_, Split_);
  if prefix_.Exists('@') then
    begin
      match_ := umlGetFirstStr(prefix_, '@');
      value_ := umlDeleteFirstStr(prefix_, '@');
    end
  else
    begin
      match_ := prefix_;
      value_ := prefix_;
    end;

  if Text_ = '' then
      Result := RootNode_
  else if RootNode_ = nil then
    begin
      if TreeView_.Items.Count > 0 then
        begin
          for i := 0 to TreeView_.Items.Count - 1 do
            begin
              if (TreeView_.Items[i].Parent = RootNode_) and umlMultipleMatch(True, match_, TreeView_.Items[i].Text) then
                begin
                  TreeView_.Items[i].Text := value_;
                  Result := GetPathTreeNode(umlDeleteFirstStr(Text_, Split_), Split_, TreeView_, TreeView_.Items[i]);
                  exit;
                end;
            end;
        end;
      Result := TreeView_.Items.AddChild(RootNode_, value_);
      with Result do
        begin
          ImageIndex := -1;
          StateIndex := -1;
          SelectedIndex := -1;
          Data := nil;
        end;
      Result := GetPathTreeNode(umlDeleteFirstStr(Text_, Split_), Split_, TreeView_, Result);
    end
  else
    begin
      if (RootNode_.Count > 0) then
        begin
          for i := 0 to RootNode_.Count - 1 do
            begin
              if (RootNode_.Item[i].Parent = RootNode_) and umlMultipleMatch(True, match_, RootNode_.Item[i].Text) then
                begin
                  RootNode_.Item[i].Text := value_;
                  Result := GetPathTreeNode(umlDeleteFirstStr(Text_, Split_), Split_, TreeView_, RootNode_.Item[i]);
                  exit;
                end;
            end;
        end;
      Result := TreeView_.Items.AddChild(RootNode_, value_);
      with Result do
        begin
          ImageIndex := -1;
          StateIndex := -1;
          SelectedIndex := -1;
          Data := nil;
        end;
      Result := GetPathTreeNode(umlDeleteFirstStr(Text_, Split_), Split_, TreeView_, Result);
    end;
end;

constructor TC40AppTempletForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsCommandLineWorkEnvir := False;
  AddDoStatusHook(self, DoStatus_backcall);
  PGControl.ActivePageIndex := 0;

  RefreshDependReg('DP');
  RefreshServiceReg('DP');

  ExtractAndProcessCmdLine(C40AppParam);
  ReloadOpt;
  ReadConfig;

  cmdLineTitleEdit.Text := Caption;
  cmdLineAppTitleEdit.Text := Application.Title;

  SysProgress.PostP1(procedure
    begin
      DependNetListView.Height := DependNetListView.Height - 1;
      ServiceListView.Height := ServiceListView.Height - 1;
    end);
end;

destructor TC40AppTempletForm.Destroy;
begin
  C40Clean;
  RemoveDoStatusHook(self);
  inherited Destroy;
end;

function TC40AppTempletForm.ExtractAndProcessCmdLine(param_: U_StringArray): Boolean;
  procedure DoDisableAllComp(comp: TComponent);
  var
    i: Integer;
  begin
    if (comp is TWinControl) and (TWinControl(comp).Parent = cmd_tool_TabSheet) then
        exit;
    if (comp is TCustomEdit) then
      begin
        if not(comp is TCustomMemo) then
          begin
            TEdit(comp).Color := clBtnface;
            TEdit(comp).Enabled := False;
          end;
      end
    else if (comp is TCustomComboBox) then
      begin
        TComboBox(comp).Color := clBtnface;
        TComboBox(comp).Enabled := False;
      end
    else if (comp is TCustomListView) then
      begin
        TListView(comp).Color := clBtnface;
        TListView(comp).Enabled := False;
      end
    else if (comp is TCustomCheckBox) then
      begin
        TCheckBox(comp).Font.Color := clBtnface;
        TCheckBox(comp).Enabled := False;
      end
    else if (comp is TCustomButton) then
      begin
        TButton(comp).Font.Color := clBtnface;
        TButton(comp).Enabled := False;
      end;

    for i := 0 to comp.ComponentCount - 1 do
        DoDisableAllComp(comp.Components[i]);
  end;

var
  error_: Boolean;
  IsInited_: Boolean;
  cs: TCommand_Script;
  i, j: Integer;
  net_info_: TCmd_Net_Info_;
  arry: TC40_DependNetworkInfoArray;
  DisableUI: Boolean;
begin
  if length(param_) = 0 then
      exit;
  error_ := False;
  IsInited_ := False;
  DisableUI := False;
  try
    cs := TCommand_Script.Create;
    Z.Net.C4.C40WriteConfig(cs.Config);
    cs.Config.SetDefaultValue('Root', Z.Net.C4.C40_RootPath);
    cs.Config.SetDefaultValue('Password', Z.Net.C4.C40_Password);
    cs.Config.SetDefaultValue('Title', Caption);
    cs.Config.SetDefaultValue('AppTitle', Application.Title);
    cs.Config.SetDefaultValue('DisableUI', umlBoolToStr(DisableUI));
    cs.Config.SetDefaultValue('Timer', umlIntToStr(netTimer.Interval));
    cs.RegApi;

    for i := low(param_) to high(param_) do
        cs.Parsing(param_[i]);

    if (not error_) and (cs.Client_NetInfo_List.Count > 0) then
      begin
        for i := 0 to cs.Client_NetInfo_List.Count - 1 do
          begin
            net_info_ := cs.Client_NetInfo_List[i];
            arry := ExtractDependInfo(net_info_.depend);
            for j := Low(arry) to high(arry) do
              if FindRegistedC40(arry[j].Typ) = nil then
                begin
                  DoStatus('no found %s', [arry[j].Typ.Text]);
                  error_ := True;
                end;
          end;
      end;

    if (not error_) and (cs.Service_NetInfo_List.Count > 0) then
      begin
        for i := 0 to cs.Service_NetInfo_List.Count - 1 do
          begin
            net_info_ := cs.Service_NetInfo_List[i];
            arry := ExtractDependInfo(net_info_.depend);
            for j := Low(arry) to high(arry) do
              if FindRegistedC40(arry[j].Typ) = nil then
                begin
                  DoStatus('no found %s', [arry[j].Typ.Text]);
                  error_ := True;
                end;
          end;
      end;

    if not error_ then
      begin
        if cs.ConfigIsUpdate then
          begin
            Z.Net.C4.C40ReadConfig(cs.Config);
            Z.Net.C4.C40_RootPath := cs.Config.GetDefaultValue('Root', Z.Net.C4.C40_RootPath);
            if not umlDirectoryExists(Z.Net.C4.C40_RootPath) then
                umlCreateDirectory(Z.Net.C4.C40_RootPath);
            Z.Net.C4.C40_Password := cs.Config.GetDefaultValue('Password', Z.Net.C4.C40_Password);
            C40AppTempletForm.Caption := cs.Config.GetDefaultValue('Title', C40AppTempletForm.Caption);
            Application.Title := cs.Config.GetDefaultValue('AppTitle', Application.Title);
            DisableUI := EStrToBool(cs.Config.GetDefaultValue('DisableUI', umlBoolToStr(DisableUI)));
            netTimer.Interval := EStrToInt(cs.Config.GetDefaultValue('Timer', umlIntToStr(netTimer.Interval)));
          end;

        if DisableUI then
            DoDisableAllComp(self);

        if cs.Service_NetInfo_List.Count > 0 then
          begin
            for i := 0 to cs.Service_NetInfo_List.Count - 1 do
              begin
                net_info_ := cs.Service_NetInfo_List[i];

                ServListeningIPEdit.Text := net_info_.listen_ip;
                ServIPEdit.Text := net_info_.ip;
                ServPortEdit.Text := umlIntToStr(net_info_.port);
                ServiceDependEdit.Text := net_info_.depend;
                ServiceDependEdit.OnChange := nil;
                ServiceDependEdit.Text := RebuildServiceInfo(ServiceDependEdit.Text);
                ServiceDependEdit.OnChange := ServiceDependEditChange;
                ServBuildNetButtonClick(ServBuildNetButton);
                IsInited_ := True;
              end;
          end;

        if cs.Client_NetInfo_List.Count > 0 then
          begin
            for i := 0 to cs.Client_NetInfo_List.Count - 1 do
              begin
                net_info_ := cs.Client_NetInfo_List[i];
                JoinHostEdit.Text := net_info_.ip;
                JoinPortEdit.Text := umlIntToStr(net_info_.port);
                DependEdit.Text := net_info_.depend;
                DependEdit.OnChange := nil;
                DependEdit.Text := RebuildDependInfo(DependEdit.Text);
                DependEdit.OnChange := DependEditChange;
                BuildDependNetButtonClick(BuildDependNetButton);
                IsInited_ := True;
              end;
          end;

        if IsInited_ then
          begin
            IsCommandLineWorkEnvir := True;
          end;
      end;

    cs.Free;
  except
  end;
  Result := IsInited_;
end;

initialization

SetLength(C40AppParam, 0);
On_C40_PhysicsTunnel_Event := nil;
On_C40_PhysicsService_Event := nil;

end.
