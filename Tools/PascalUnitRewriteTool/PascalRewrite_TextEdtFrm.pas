unit PascalRewrite_TextEdtFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TPascalRewrite_TextEdtForm = class(TForm)
    _b_Panel: TPanel;
    Memo: TMemo;
    DoneButton: TButton;
    cancelButton: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PascalRewrite_TextEdtForm: TPascalRewrite_TextEdtForm;

implementation

{$R *.dfm}


uses PascalRewriteModelFrm;

procedure TPascalRewrite_TextEdtForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TPascalRewrite_TextEdtForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
      ModalResult := mrCancel;
end;

end.
