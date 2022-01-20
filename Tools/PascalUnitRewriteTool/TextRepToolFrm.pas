unit TextRepToolFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TTextRepToolForm = class(TForm)
    OLDEdit: TLabeledEdit;
    NewEdit: TLabeledEdit;
    Word_CheckBox: TCheckBox;
    IgnoreCase_CheckBox: TCheckBox;
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
  TextRepToolForm: TTextRepToolForm;

implementation

{$R *.dfm}


uses PascalRewriteModelFrm;

procedure TTextRepToolForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TTextRepToolForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
      ModalResult := mrCancel;
end;

end.
