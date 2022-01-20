unit NewUnitNameFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TNewUnitNameForm = class(TForm)
    NewUnitEdit: TLabeledEdit;
    okButton: TButton;
    CancelButton: TButton;
    Memo: TMemo;
    Selected_CheckBox: TCheckBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
  public
  end;

var
  NewUnitNameForm: TNewUnitNameForm;

implementation

{$R *.dfm}

uses PascalRewriteModelFrm;



procedure TNewUnitNameForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := TCloseAction.caHide;
end;

procedure TNewUnitNameForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
      Close;
end;

end.
