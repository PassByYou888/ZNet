unit _3_Auth_IM_Client_Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.Notify,
  Z.Net,
  Z.ListEngine,
  Z.DFE,
  Z.Net.PhysicsIO,
  Z.Net.C4;

type
  {  After completing the background VM, it is very simple to be a client. Just encapsulate a few APIs  }
  TMyVA_Client = class(TC40_Base_VirtualAuth_Client)
  private type
    TON_Usr_NewLoginNameC = procedure(sender: TMyVA_Client; State_: Boolean; info_: SystemString);
    TON_Usr_NewLoginNameM = procedure(sender: TMyVA_Client; State_: Boolean; info_: SystemString) of object;
{$IFDEF FPC}
    TON_Usr_NewLoginNameP = procedure(sender: TMyVA_Client; State_: Boolean; info_: SystemString) is nested;
{$ELSE FPC}
    TON_Usr_NewLoginNameP = reference to procedure(sender: TMyVA_Client; State_: Boolean; info_: SystemString);
{$ENDIF FPC}

    TON_Usr_NewLoginName = class(TOnResult_Bridge)
    public
      Client: TMyVA_Client;
      OnResultC: TON_Usr_NewLoginNameC;
      OnResultM: TON_Usr_NewLoginNameM;
      OnResultP: TON_Usr_NewLoginNameP;
      constructor Create;
      procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
    end;

    TON_Usr_GetAliasC = procedure(sender: TMyVA_Client; Alias_: SystemString);
    TON_Usr_GetAliasM = procedure(sender: TMyVA_Client; Alias_: SystemString) of object;
{$IFDEF FPC}
    TON_Usr_GetAliasP = procedure(sender: TMyVA_Client; Alias_: SystemString) is nested;
{$ELSE FPC}
    TON_Usr_GetAliasP = reference to procedure(sender: TMyVA_Client; Alias_: SystemString);
{$ENDIF FPC}

    TON_Usr_GetAlias = class(TOnResult_Bridge)
    public
      Client: TMyVA_Client;
      OnResultC: TON_Usr_GetAliasC;
      OnResultM: TON_Usr_GetAliasM;
      OnResultP: TON_Usr_GetAliasP;
      constructor Create;
      procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
    end;

    TON_Usr_GetMyFriendC = procedure(sender: TMyVA_Client; FriendArry: U_StringArray);
    TON_Usr_GetMyFriendM = procedure(sender: TMyVA_Client; FriendArry: U_StringArray) of object;
{$IFDEF FPC}
    TON_Usr_GetMyFriendP = procedure(sender: TMyVA_Client; FriendArry: U_StringArray) is nested;
{$ELSE FPC}
    TON_Usr_GetMyFriendP = reference to procedure(sender: TMyVA_Client; FriendArry: U_StringArray);
{$ENDIF FPC}

    TON_Usr_GetMyFriend = class(TOnResult_Bridge)
    public
      Client: TMyVA_Client;
      OnResultC: TON_Usr_GetMyFriendC;
      OnResultM: TON_Usr_GetMyFriendM;
      OnResultP: TON_Usr_GetMyFriendP;
      constructor Create;
      procedure DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine); override;
    end;
  private
    procedure cmd_userMsg(sender: TPeerIO; InData: SystemString);
    procedure cmd_userOnline(sender: TPeerIO; InData: SystemString);
    procedure cmd_userOffline(sender: TPeerIO; InData: SystemString);
    procedure cmd_userRequestFriend(sender: TPeerIO; InData: SystemString);
  public
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure NewLoginName_C(NewLogin_Name_: U_String; OnResult: TON_Usr_NewLoginNameC);
    procedure NewLoginName_M(NewLogin_Name_: U_String; OnResult: TON_Usr_NewLoginNameM);
    procedure NewLoginName_P(NewLogin_Name_: U_String; OnResult: TON_Usr_NewLoginNameP);
    procedure NewAlias(NewAlias_Name_: U_String);
    procedure GetAlias_C(User_Name_: U_String; OnResult: TON_Usr_GetAliasC);
    procedure GetAlias_M(User_Name_: U_String; OnResult: TON_Usr_GetAliasM);
    procedure GetAlias_P(User_Name_: U_String; OnResult: TON_Usr_GetAliasP);
    procedure Msg(ToUserName_, msg_: U_String);
    procedure RequestFriend(ToUserName_, msg_: U_String);
    procedure ReponseFriend(ToUserName_, msg_: U_String; Accept_: Boolean);
    procedure RemoveFriend(ToUserName_: U_String);
    procedure GetMyFriend_C(OnResult: TON_Usr_GetMyFriendC);
    procedure GetMyFriend_M(OnResult: TON_Usr_GetMyFriendM);
    procedure GetMyFriend_P(OnResult: TON_Usr_GetMyFriendP);
  end;

  T_3_Auth_IM_Client_Form = class(TForm, IC40_PhysicsTunnel_Event)
    NetTimer: TTimer;
    Memo: TMemo;
    sendMsgButton: TButton;
    msgMemo: TMemo;
    Label1: TLabel;
    RequestFriendMemo: TMemo;
    Label2: TLabel;
    OnlineMemo: TMemo;
    Label3: TLabel;
    FriendListMemo: TMemo;
    Label4: TLabel;
    RequestFriendButton: TButton;
    ReponseAddFriendButton: TButton;
    refreshFriendButton: TButton;
    removeFriendButton: TButton;
    LoginInfoLabel: TLabel;
    procedure FormCreate(sender: TObject);
    procedure FormShow(sender: TObject);
    procedure FormDestroy(sender: TObject);
    procedure NetTimerTimer(sender: TObject);
    procedure refreshFriendButtonClick(sender: TObject);
    procedure removeFriendButtonClick(sender: TObject);
    procedure ReponseAddFriendButtonClick(sender: TObject);
    procedure RequestFriendButtonClick(sender: TObject);
    procedure sendMsgButtonClick(sender: TObject);
  private
    procedure C40_PhysicsTunnel_Connected(sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Disconnect(sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Build_Network(sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    procedure C40_PhysicsTunnel_Client_Connected(sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
  end;

var
  _3_Auth_IM_Client_Form: T_3_Auth_IM_Client_Form;

implementation

{$R *.dfm}


uses _3_Auth_IM_Client_LoginFrm, _3_Auth_IM_Client_SendMsgFrm,
  _3_Auth_IM_Client_RegFrm, _3_Auth_IM_Client_RequestAddFriendFrm,
  _3_Auth_IM_Client_ReponseAddFriendFrm, _3_Auth_IM_Client_RemoveFriendFrm;

constructor TMyVA_Client.TON_Usr_NewLoginName.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TMyVA_Client.TON_Usr_NewLoginName.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  State_: Boolean;
  info_: SystemString;
begin
  State_ := Result_.R.ReadBool;
  info_ := Result_.R.ReadString;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TMyVA_Client.TON_Usr_GetAlias.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TMyVA_Client.TON_Usr_GetAlias.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  Alias_: SystemString;
begin
  if Result_.R.NotEnd then
      Alias_ := Result_.R.ReadString
  else
      Alias_ := '';

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Alias_);
    if Assigned(OnResultM) then
        OnResultM(Client, Alias_);
    if Assigned(OnResultP) then
        OnResultP(Client, Alias_);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TMyVA_Client.TON_Usr_GetMyFriend.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TMyVA_Client.TON_Usr_GetMyFriend.DoStreamEvent(sender: TPeerIO; Result_: TDataFrameEngine);
var
  i: Integer;
  FriendArry: U_StringArray;
begin
  SetLength(FriendArry, Result_.Count);
  for i := 0 to Result_.Count - 1 do
      FriendArry[i] := Result_.ReadString(i);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, FriendArry);
    if Assigned(OnResultM) then
        OnResultM(Client, FriendArry);
    if Assigned(OnResultP) then
        OnResultP(Client, FriendArry);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TMyVA_Client.cmd_userMsg(sender: TPeerIO; InData: SystemString);
begin
  _3_Auth_IM_Client_Form.msgMemo.Lines.Add(PFormat('%s', [InData]));
end;

procedure TMyVA_Client.cmd_userOnline(sender: TPeerIO; InData: SystemString);
begin
  _3_Auth_IM_Client_Form.OnlineMemo.Lines.Add(PFormat('Friend online message%s', [InData]));
end;

procedure TMyVA_Client.cmd_userOffline(sender: TPeerIO; InData: SystemString);
begin
  _3_Auth_IM_Client_Form.OnlineMemo.Lines.Add(PFormat('Friend offline message%s', [InData]));
end;

procedure TMyVA_Client.cmd_userRequestFriend(sender: TPeerIO; InData: SystemString);
begin
  _3_Auth_IM_Client_Form.RequestFriendMemo.Lines.Add(PFormat('Request to add as friend%s', [InData]));
end;

constructor TMyVA_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  DTVirtualAuthClient.RecvTunnel.RegisterDirectConsole('userMsg').OnExecute := cmd_userMsg;
  DTVirtualAuthClient.RecvTunnel.RegisterDirectConsole('userOnline').OnExecute := cmd_userOnline;
  DTVirtualAuthClient.RecvTunnel.RegisterDirectConsole('userOffline').OnExecute := cmd_userOffline;
  DTVirtualAuthClient.RecvTunnel.RegisterDirectConsole('userRequestFriend').OnExecute := cmd_userRequestFriend;
end;

destructor TMyVA_Client.Destroy;
begin
  inherited Destroy;
end;

procedure TMyVA_Client.NewLoginName_C(NewLogin_Name_: U_String; OnResult: TON_Usr_NewLoginNameC);
var
  tmp: TON_Usr_NewLoginName;
  D: TDFE;
begin
  tmp := TON_Usr_NewLoginName.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(NewLogin_Name_);
  DTVirtualAuthClient.SendTunnel.SendStreamCmdM('NewLoginName', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure TMyVA_Client.NewLoginName_M(NewLogin_Name_: U_String; OnResult: TON_Usr_NewLoginNameM);
var
  tmp: TON_Usr_NewLoginName;
  D: TDFE;
begin
  tmp := TON_Usr_NewLoginName.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(NewLogin_Name_);
  DTVirtualAuthClient.SendTunnel.SendStreamCmdM('NewLoginName', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure TMyVA_Client.NewLoginName_P(NewLogin_Name_: U_String; OnResult: TON_Usr_NewLoginNameP);
var
  tmp: TON_Usr_NewLoginName;
  D: TDFE;
begin
  tmp := TON_Usr_NewLoginName.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(NewLogin_Name_);
  DTVirtualAuthClient.SendTunnel.SendStreamCmdM('NewLoginName', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure TMyVA_Client.NewAlias(NewAlias_Name_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(NewAlias_Name_);
  DTVirtualAuthClient.SendTunnel.SendDirectStreamCmd('NewAlias', D);
  DisposeObject(D);
end;

procedure TMyVA_Client.GetAlias_C(User_Name_: U_String; OnResult: TON_Usr_GetAliasC);
var
  tmp: TON_Usr_GetAlias;
  D: TDFE;
begin
  tmp := TON_Usr_GetAlias.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  D.WriteString(User_Name_);
  DTVirtualAuthClient.SendTunnel.SendStreamCmdM('GetAlias', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure TMyVA_Client.GetAlias_M(User_Name_: U_String; OnResult: TON_Usr_GetAliasM);
var
  tmp: TON_Usr_GetAlias;
  D: TDFE;
begin
  tmp := TON_Usr_GetAlias.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  D.WriteString(User_Name_);
  DTVirtualAuthClient.SendTunnel.SendStreamCmdM('GetAlias', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure TMyVA_Client.GetAlias_P(User_Name_: U_String; OnResult: TON_Usr_GetAliasP);
var
  tmp: TON_Usr_GetAlias;
  D: TDFE;
begin
  tmp := TON_Usr_GetAlias.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  D.WriteString(User_Name_);
  DTVirtualAuthClient.SendTunnel.SendStreamCmdM('GetAlias', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure TMyVA_Client.Msg(ToUserName_, msg_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(ToUserName_);
  D.WriteString(msg_);
  DTVirtualAuthClient.SendTunnel.SendDirectStreamCmd('Msg', D);
  DisposeObject(D);
end;

procedure TMyVA_Client.RequestFriend(ToUserName_, msg_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(ToUserName_);
  D.WriteString(msg_);
  DTVirtualAuthClient.SendTunnel.SendDirectStreamCmd('RequestFriend', D);
  DisposeObject(D);
end;

procedure TMyVA_Client.ReponseFriend(ToUserName_, msg_: U_String; Accept_: Boolean);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(ToUserName_);
  D.WriteString(msg_);
  D.WriteBool(Accept_);
  DTVirtualAuthClient.SendTunnel.SendDirectStreamCmd('ReponseFriend', D);
  DisposeObject(D);
end;

procedure TMyVA_Client.RemoveFriend(ToUserName_: U_String);
var
  D: TDFE;
begin
  D := TDFE.Create;
  D.WriteString(ToUserName_);
  DTVirtualAuthClient.SendTunnel.SendDirectStreamCmd('RemoveFriend', D);
  DisposeObject(D);
end;

procedure TMyVA_Client.GetMyFriend_C(OnResult: TON_Usr_GetMyFriendC);
var
  tmp: TON_Usr_GetMyFriend;
  D: TDFE;
begin
  tmp := TON_Usr_GetMyFriend.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  D := TDFE.Create;
  DTVirtualAuthClient.SendTunnel.SendStreamCmdM('GetMyFriend', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure TMyVA_Client.GetMyFriend_M(OnResult: TON_Usr_GetMyFriendM);
var
  tmp: TON_Usr_GetMyFriend;
  D: TDFE;
begin
  tmp := TON_Usr_GetMyFriend.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  D := TDFE.Create;
  DTVirtualAuthClient.SendTunnel.SendStreamCmdM('GetMyFriend', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure TMyVA_Client.GetMyFriend_P(OnResult: TON_Usr_GetMyFriendP);
var
  tmp: TON_Usr_GetMyFriend;
  D: TDFE;
begin
  tmp := TON_Usr_GetMyFriend.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  D := TDFE.Create;
  DTVirtualAuthClient.SendTunnel.SendStreamCmdM('GetMyFriend', D, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  DisposeObject(D);
end;

procedure T_3_Auth_IM_Client_Form.FormCreate(sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);
end;

procedure T_3_Auth_IM_Client_Form.FormDestroy(sender: TObject);
begin
  RemoveDoStatusHook(self);
end;

procedure T_3_Auth_IM_Client_Form.NetTimerTimer(sender: TObject);
begin
  C40Progress;
end;

procedure T_3_Auth_IM_Client_Form.C40_PhysicsTunnel_Connected(sender: TC40_PhysicsTunnel);
begin

end;

procedure T_3_Auth_IM_Client_Form.C40_PhysicsTunnel_Disconnect(sender: TC40_PhysicsTunnel);
begin
  _3_Auth_IM_Client_LoginForm.Show;
  _3_Auth_IM_Client_SendMsgForm.Close;
  _3_Auth_IM_Client_SendMsgForm.Close;
  _3_Auth_IM_Client_RequestAddFriendForm.Close;
  _3_Auth_IM_Client_ReponseAddFriendForm.Close;
  _3_Auth_IM_Client_RemoveFriendForm.Close;
end;

procedure T_3_Auth_IM_Client_Form.C40_PhysicsTunnel_Build_Network(sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin

end;

procedure T_3_Auth_IM_Client_Form.C40_PhysicsTunnel_Client_Connected(sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
var
  cli: TMyVA_Client;
begin
  if Custom_Client_ is TMyVA_Client then
    begin
      cli := Custom_Client_ as TMyVA_Client;
      if not cli.NoDTLink then
        begin
          {  Login succeeded. Close the login window and the registration window  }
          _3_Auth_IM_Client_LoginForm.Close;
          _3_Auth_IM_Client_RegForm.Close;
          LoginInfoLabel.Caption := PFormat('Login information:%s', [cli.UserName.Text]);
        end;
    end;
end;

procedure T_3_Auth_IM_Client_Form.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

procedure T_3_Auth_IM_Client_Form.FormShow(sender: TObject);
begin
  _3_Auth_IM_Client_LoginForm.Show;
end;

procedure T_3_Auth_IM_Client_Form.refreshFriendButtonClick(sender: TObject);
begin
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('MyVA', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    var
      cli: TMyVA_Client;
    begin
      cli := States_[0].Client_ as TMyVA_Client;
      cli.GetMyFriend_P(procedure(sender: TMyVA_Client; FriendArry: U_StringArray)
        var
          i: Integer;
        begin
          FriendListMemo.Clear;
          for i := 0 to length(FriendArry) - 1 do
              FriendListMemo.Lines.Add(FriendArry[i]);
        end);
    end)
end;

procedure T_3_Auth_IM_Client_Form.removeFriendButtonClick(sender: TObject);
begin
  _3_Auth_IM_Client_RemoveFriendForm.Show;
end;

procedure T_3_Auth_IM_Client_Form.ReponseAddFriendButtonClick(sender: TObject);
begin
  _3_Auth_IM_Client_ReponseAddFriendForm.Show;
end;

procedure T_3_Auth_IM_Client_Form.RequestFriendButtonClick(sender: TObject);
begin
  _3_Auth_IM_Client_RequestAddFriendForm.Show;
end;

procedure T_3_Auth_IM_Client_Form.sendMsgButtonClick(sender: TObject);
begin
  _3_Auth_IM_Client_SendMsgForm.Show;
end;

initialization

RegisterC40('MyVA', nil, TMyVA_Client);

end.
