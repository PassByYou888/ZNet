unit C4_VAR_Tech_Demo_Cli_Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,

  Z.Core, Z.PascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine,
  Z.Geometry2D, Z.DFE, Z.Json, Z.Expression,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.Number,
  Z.Net, Z.Net.PhysicsIO, Z.Net.C4, Z.Net.C4_Var, Z.Net.C4_FS, Z.Net.C4_UserDB;

type
  TC4_VAR_Tech_Demo_Cli_Form = class(TForm, IC40_PhysicsTunnel_Event)
    topPanel: TPanel;
    AddrEdit: TLabeledEdit;
    PortEdit: TLabeledEdit;
    dependEdit: TLabeledEdit;
    GoNetworkButton: TButton;
    netTimer: TTimer;
    Memo: TMemo;
    botSplitter: TSplitter;
    cliPanel: TPanel;
    LPanel: TPanel;
    LSplitter: TSplitter;
    RPanel: TPanel;
    TreeView: TTreeView;
    UpdateStateTimer: TTimer;
    ScriptMemo: TMemo;
    LTPanel: TPanel;
    InitLocalNMFromScriptButton: TButton;
    InitLocalNMButton: TButton;
    NMEdit: TLabeledEdit;
    SyncNMToRemoteButton: TButton;
    removeNMButton: TButton;
    SyncAsTempForRemoteButton: TButton;
    runScriptFromRemoteButton: TButton;
    openNMButton: TButton;
    closeNMButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure netTimerTimer(Sender: TObject);
    procedure UpdateStateTimerTimer(Sender: TObject);
    procedure GoNetworkButtonClick(Sender: TObject);
    procedure InitLocalNMFromScriptButtonClick(Sender: TObject);
    procedure InitLocalNMButtonClick(Sender: TObject);
    procedure SyncAsTempForRemoteButtonClick(Sender: TObject);
    procedure openNMButtonClick(Sender: TObject);
    procedure closeNMButtonClick(Sender: TObject);
    procedure removeNMButtonClick(Sender: TObject);
    procedure runScriptFromRemoteButtonClick(Sender: TObject);
    procedure SyncNMToRemoteButtonClick(Sender: TObject);
  private
    procedure C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    procedure C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);

    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
    VarClient: TC40_Var_Client;
    procedure UpdateVarStates;
  end;

function GetPathTreeNode(Value_, Split_: string; Tree_: TTreeView; RootNode_: TTreeNode): TTreeNode;

var
  C4_VAR_Tech_Demo_Cli_Form: TC4_VAR_Tech_Demo_Cli_Form;

implementation

{$R *.dfm}


function GetPathTreeNode(Value_, Split_: string; Tree_: TTreeView; RootNode_: TTreeNode): TTreeNode;
var
  i: Integer;
  Postfix_: string;
begin
  Postfix_ := umlGetFirstStr(Value_, Split_);
  if Value_ = '' then
      Result := RootNode_
  else if RootNode_ = nil then
    begin
      if Tree_.Items.Count > 0 then
        begin
          for i := 0 to Tree_.Items.Count - 1 do
            begin
              if (Tree_.Items[i].Parent = RootNode_) and (umlMultipleMatch(True, Postfix_, Tree_.Items[i].Text)) then
                begin
                  Result := GetPathTreeNode(umlDeleteFirstStr(Value_, Split_), Split_, Tree_, Tree_.Items[i]);
                  Result.Expand(False);
                  exit;
                end;
            end;
        end;
      Result := Tree_.Items.AddChild(RootNode_, Postfix_);
      with Result do
        begin
          ImageIndex := -1;
          StateIndex := -1;
          SelectedIndex := -1;
          Data := nil;
        end;
      Result := GetPathTreeNode(umlDeleteFirstStr(Value_, Split_), Split_, Tree_, Result);
    end
  else
    begin
      if (RootNode_.Count > 0) then
        begin
          for i := 0 to RootNode_.Count - 1 do
            begin
              if (RootNode_.Item[i].Parent = RootNode_) and (umlMultipleMatch(True, Postfix_, RootNode_.Item[i].Text)) then
                begin
                  Result := GetPathTreeNode(umlDeleteFirstStr(Value_, Split_), Split_, Tree_, RootNode_.Item[i]);
                  Result.Expand(False);
                  exit;
                end;
            end;
        end;
      Result := Tree_.Items.AddChild(RootNode_, Postfix_);
      with Result do
        begin
          ImageIndex := -1;
          StateIndex := -1;
          SelectedIndex := -1;
          Data := nil;
        end;
      Result := GetPathTreeNode(umlDeleteFirstStr(Value_, Split_), Split_, Tree_, Result);
    end;
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.FormCreate(Sender: TObject);
begin
  // 初始化saas网络的进入密码,该密码在传输中是抗量子的,但是在可执行文件中,通过逆向分析仍然能得到明文
  // 该密码也是服务器安全的唯一屏障,使用c4网络在运营时不是泄露可执行文件出去那么c4网络就是安全的
  Z.Net.C4.C40_Password := '123456';
  // 挂接StatusIO以便状态打印
  AddDoStatusHook(self, DoStatus_backcall);
  VarClient := nil;
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.FormDestroy(Sender: TObject);
begin
  Z.Net.C4.C40Clean;
  RemoveDoStatusHook(self);
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.netTimerTimer(Sender: TObject);
begin
  Z.Net.C4.C40Progress;
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.UpdateStateTimerTimer(Sender: TObject);
begin
  UpdateVarStates;
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.GoNetworkButtonClick(Sender: TObject);
begin
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(AddrEdit.Text, umlStrToInt(PortEdit.Text), dependEdit.Text, self);
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.InitLocalNMFromScriptButtonClick(Sender: TObject);
var
  i: Integer;
begin
  if VarClient = nil then
      exit;
  for i := 0 to ScriptMemo.Lines.Count - 1 do
      VarClient.GetNM(NMEdit.Text).RunScript(ScriptMemo.Lines[i]);
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.InitLocalNMButtonClick(Sender: TObject);
begin
  if VarClient = nil then
      exit;
  VarClient.GetNM(NMEdit.Text)['A'].AsInt64 := 1;
  VarClient.GetNM(NMEdit.Text)['B'].AsInt64 := 2;
  VarClient.GetNM(NMEdit.Text)['C'].AsInt64 := 3;
  VarClient.GetNM(NMEdit.Text)['D'].AsInt64 := 4;
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.SyncAsTempForRemoteButtonClick(Sender: TObject);
begin
  if VarClient = nil then
      exit;
  VarClient.NM_InitAsTemp(NMEdit.Text, 5000, False, VarClient.GetNM(NMEdit.Text));
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.openNMButtonClick(Sender: TObject);
begin
  if VarClient = nil then
      exit;
  VarClient.NM_OpenP([NMEdit.Text], nil);
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.closeNMButtonClick(Sender: TObject);
begin
  if VarClient = nil then
      exit;
  VarClient.NM_CloseAll(False);
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.removeNMButtonClick(Sender: TObject);
begin
  if VarClient = nil then
      exit;
  VarClient.NM_Remove(NMEdit.Text, False);
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.runScriptFromRemoteButtonClick(Sender: TObject);
var
  arry: U_StringArray;
  i: Integer;
begin
  if VarClient = nil then
      exit;
  SetLength(arry, ScriptMemo.Lines.Count);
  for i := 0 to ScriptMemo.Lines.Count - 1 do
      arry[i] := ScriptMemo.Lines[i];

  VarClient.NM_ScriptP(NMEdit.Text, arry, procedure(Sender: TC40_Var_Client; Result_: TExpressionValueVector)
    begin
      DoStatusE(Result_);
    end);
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.SyncNMToRemoteButtonClick(Sender: TObject);
begin
  if VarClient = nil then
      exit;
  VarClient.NM_Init(NMEdit.Text, False, VarClient.GetNM(NMEdit.Text));
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
begin

end;

procedure TC4_VAR_Tech_Demo_Cli_Form.C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
begin
  if Sender.DependNetworkClientPool.IndexOf(VarClient) >= 0 then
    begin
      VarClient := nil;
      TreeView.Items.Clear;
    end;
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin

end;

procedure TC4_VAR_Tech_Demo_Cli_Form.C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
  if Custom_Client_ is TC40_Var_Client then
      VarClient := TC40_Var_Client(Custom_Client_);
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

procedure TC4_VAR_Tech_Demo_Cli_Form.UpdateVarStates;
begin
  if VarClient = nil then
      exit;
  VarClient.NMBigPool.ProgressP(procedure(const NMPoolName_: PSystemString; NMPool_: TC40_VarService_NM_Pool)
    begin
      NMPool_.List.ProgressP(procedure(const NMName_: PSystemString; NM_: TNumberModule)
        var
          RN: TTreeNode;
        begin
          RN := GetPathTreeNode(Format('%s/%s', [NMPoolName_^, NMName_^]), '/', TreeView, nil);
          if RN.Count = 0 then
              GetPathTreeNode(Format('%s', [NM_.AsString]), '/', TreeView, RN)
          else
              RN[0].Text := Format('%s', [NM_.AsString]);
        end);
    end);
end;

end.
