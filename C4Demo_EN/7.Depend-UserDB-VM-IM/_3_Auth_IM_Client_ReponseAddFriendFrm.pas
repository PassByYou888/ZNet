unit _3_Auth_IM_Client_ReponseAddFriendFrm;

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
  T_3_Auth_IM_Client_ReponseAddFriendForm = class(TForm)
    ToUserNameEdit: TLabeledEdit;
    Memo: TMemo;
    AcceeptFriendRequeseButton: TButton;
    RejectFriendRequeseButton: TButton;
    Label1: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AcceeptFriendRequeseButtonClick(Sender: TObject);
    procedure RejectFriendRequeseButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  _3_Auth_IM_Client_ReponseAddFriendForm: T_3_Auth_IM_Client_ReponseAddFriendForm;

implementation

{$R *.dfm}

uses _3_Auth_IM_Client_Frm;

procedure T_3_Auth_IM_Client_ReponseAddFriendForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
end;

procedure T_3_Auth_IM_Client_ReponseAddFriendForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
      Close;
end;

procedure T_3_Auth_IM_Client_ReponseAddFriendForm.AcceeptFriendRequeseButtonClick(Sender: TObject);
begin
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('MyVA', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    var
      cli: TMyVA_Client;
    begin
      cli := States_[0].Client_ as TMyVA_Client;
      cli.ReponseFriend(ToUserNameEdit.Text, Memo.Text, True);
    end)
end;

procedure T_3_Auth_IM_Client_ReponseAddFriendForm.RejectFriendRequeseButtonClick(Sender: TObject);
begin
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('MyVA', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    var
      cli: TMyVA_Client;
    begin
      cli := States_[0].Client_ as TMyVA_Client;
      cli.ReponseFriend(ToUserNameEdit.Text, Memo.Text, False);
    end)
end;

end.
