object NewUnitNameForm: TNewUnitNameForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 20
  Caption = 'Name define.'
  ClientHeight = 180
  ClientWidth = 531
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Consolas'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  PopupParent = PascalRewriteModelForm.Owner
  Position = poMainFormCenter
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 14
  object NewUnitEdit: TLabeledEdit
    Left = 131
    Top = 92
    Width = 186
    Height = 22
    EditLabel.Width = 112
    EditLabel.Height = 14
    EditLabel.Caption = 'New Unit define:'
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '%prefix%.%postfix%'
  end
  object okButton: TButton
    Left = 131
    Top = 155
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 3
  end
  object CancelButton: TButton
    Left = 212
    Top = 155
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 531
    Height = 76
    Lines.Strings = (
      '%filename%, source file name'
      '%prefix% or %before%, source file prefix'
      '%postfix% or %after% or %ext%, source file postfix')
    TabOrder = 0
  end
  object Selected_CheckBox: TCheckBox
    Left = 131
    Top = 121
    Width = 144
    Height = 17
    Caption = 'Only Selected.'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
end
