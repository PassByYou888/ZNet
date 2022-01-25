unit _3_Auth_IM_Client_RemoveFriendFrm;

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
  T_3_Auth_IM_Client_RemoveFriendForm = class(TForm)
    ToUserNameEdit: TLabeledEdit;
    sendRequeseButton: TButton;
    CancelButton: TButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sendRequeseButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  _3_Auth_IM_Client_RemoveFriendForm: T_3_Auth_IM_Client_RemoveFriendForm;

implementation

{$R *.dfm}


uses _3_Auth_IM_Client_Frm;

procedure T_3_Auth_IM_Client_RemoveFriendForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure T_3_Auth_IM_Client_RemoveFriendForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
end;

procedure T_3_Auth_IM_Client_RemoveFriendForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
      Close;
end;

procedure T_3_Auth_IM_Client_RemoveFriendForm.sendRequeseButtonClick(Sender: TObject);
begin
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('MyVA', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    var
      cli: TMyVA_Client;
    begin
      cli := States_[0].Client_ as TMyVA_Client;
      cli.RemoveFriend(ToUserNameEdit.Text);
    end)
end;

end.
