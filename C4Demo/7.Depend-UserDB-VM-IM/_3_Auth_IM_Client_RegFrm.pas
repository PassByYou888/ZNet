unit _3_Auth_IM_Client_RegFrm;

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
  Z.Net.PhysicsIO,
  Z.Net.C4;

type
  T_3_Auth_IM_Client_RegForm = class(TForm)
    UserEdit: TLabeledEdit;
    Passwd1Edit: TLabeledEdit;
    Passwd2Edit: TLabeledEdit;
    AliasNameEdit: TLabeledEdit;
    cancelButton: TButton;
    regButton: TButton;
    procedure cancelButtonClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure regButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  _3_Auth_IM_Client_RegForm: T_3_Auth_IM_Client_RegForm;

implementation

{$R *.dfm}


uses _3_Auth_IM_Client_Frm, _3_Auth_IM_Client_LoginFrm;

procedure T_3_Auth_IM_Client_RegForm.cancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure T_3_Auth_IM_Client_RegForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
end;

procedure T_3_Auth_IM_Client_RegForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
      Close;
end;

procedure T_3_Auth_IM_Client_RegForm.regButtonClick(Sender: TObject);
begin
  if Passwd1Edit.Text <> Passwd2Edit.Text then
    begin
      DoStatus('密码不一致.');
      exit;
    end;

  Z.Net.C4.C40_PhysicsTunnelPool.SearchServiceAndBuildConnection(
    _3_Auth_IM_Client_LoginForm.HostEdit.Text, umlStrToInt(_3_Auth_IM_Client_LoginForm.PortEdit.Text), False, 'MyVA', _3_Auth_IM_Client_Form);

  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('MyVA', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    var
      cli: TMyVA_Client;
    begin
      cli := States_[0].Client_ as TMyVA_Client;
      cli.Client.RegisterUserAndLogin := True;
      cli.Client.Connect_P(UserEdit.Text, Passwd1Edit.Text,
        procedure(const State: Boolean)
        begin
          if State then
            begin
              cli.NewAlias(AliasNameEdit.Text);
              DoStatus('注册成功.');
              _3_Auth_IM_Client_LoginForm.UserEdit.Text := UserEdit.Text;
              _3_Auth_IM_Client_LoginForm.PasswdEdit.Text := Passwd1Edit.Text;
            end;
        end);
    end)
end;

end.
