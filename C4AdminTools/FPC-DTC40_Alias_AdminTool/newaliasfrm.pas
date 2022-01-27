unit newaliasfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  ActnList, Menus,
  Variants, DateUtils, TypInfo,
  LCLType;

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

uses dtc40_alias_admintoolfrm;

{$R *.lfm}

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

