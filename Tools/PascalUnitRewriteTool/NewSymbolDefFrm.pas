unit NewSymbolDefFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TNewSymbolDefForm = class(TForm)
    OLD_Edit: TLabeledEdit;
    NewEdit: TLabeledEdit;
    OKButton: TButton;
    cancelButton: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NewSymbolDefForm: TNewSymbolDefForm;

implementation

{$R *.dfm}

uses PascalRewriteModelFrm;

procedure TNewSymbolDefForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TNewSymbolDefForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
      ModalResult := mrCancel;
end;

end.
