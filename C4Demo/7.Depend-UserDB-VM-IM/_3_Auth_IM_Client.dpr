program _3_Auth_IM_Client;

uses
  Vcl.Forms,
  _3_Auth_IM_Client_Frm in '_3_Auth_IM_Client_Frm.pas' {_3_Auth_IM_Client_Form},
  _3_Auth_IM_Client_LoginFrm in '_3_Auth_IM_Client_LoginFrm.pas' {_3_Auth_IM_Client_LoginForm},
  _3_Auth_IM_Client_RegFrm in '_3_Auth_IM_Client_RegFrm.pas' {_3_Auth_IM_Client_RegForm},
  _3_Auth_IM_Client_SendMsgFrm in '_3_Auth_IM_Client_SendMsgFrm.pas' {_3_Auth_IM_Client_SendMsgForm},
  _3_Auth_IM_Client_RequestAddFriendFrm in '_3_Auth_IM_Client_RequestAddFriendFrm.pas' {_3_Auth_IM_Client_RequestAddFriendForm},
  _3_Auth_IM_Client_ReponseAddFriendFrm in '_3_Auth_IM_Client_ReponseAddFriendFrm.pas' {_3_Auth_IM_Client_ReponseAddFriendForm},
  _3_Auth_IM_Client_RemoveFriendFrm in '_3_Auth_IM_Client_RemoveFriendFrm.pas' {_3_Auth_IM_Client_RemoveFriendForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(T_3_Auth_IM_Client_Form, _3_Auth_IM_Client_Form);
  Application.CreateForm(T_3_Auth_IM_Client_LoginForm, _3_Auth_IM_Client_LoginForm);
  Application.CreateForm(T_3_Auth_IM_Client_RegForm, _3_Auth_IM_Client_RegForm);
  Application.CreateForm(T_3_Auth_IM_Client_SendMsgForm, _3_Auth_IM_Client_SendMsgForm);
  Application.CreateForm(T_3_Auth_IM_Client_RequestAddFriendForm, _3_Auth_IM_Client_RequestAddFriendForm);
  Application.CreateForm(T_3_Auth_IM_Client_ReponseAddFriendForm, _3_Auth_IM_Client_ReponseAddFriendForm);
  Application.CreateForm(T_3_Auth_IM_Client_RemoveFriendForm, _3_Auth_IM_Client_RemoveFriendForm);
  Application.Run;
end.
