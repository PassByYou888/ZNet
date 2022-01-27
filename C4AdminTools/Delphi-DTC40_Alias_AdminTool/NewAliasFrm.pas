unit NewAliasFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TNewAliasForm = class(TForm)
    AliasEdit: TLabeledEdit;
    NameEdit: TLabeledEdit;
    OKButton: TButton;
    cancelButton: TButton;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NewAliasForm: TNewAliasForm;

implementation

{$R *.dfm}


uses DTC40_Alias_AdminTool_Frm;

procedure TNewAliasForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
      ModalResult := mrCancel;
end;

procedure TNewAliasForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

end.
