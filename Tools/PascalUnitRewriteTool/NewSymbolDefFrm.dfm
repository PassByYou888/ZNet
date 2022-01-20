object NewSymbolDefForm: TNewSymbolDefForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'New symbol defne.'
  ClientHeight = 177
  ClientWidth = 395
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  PopupParent = PascalRewriteModelForm.Owner
  Position = poMainFormCenter
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object OLD_Edit: TLabeledEdit
    Left = 141
    Top = 30
    Width = 121
    Height = 21
    EditLabel.Width = 24
    EditLabel.Height = 13
    EditLabel.Caption = 'OLD:'
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object NewEdit: TLabeledEdit
    Left = 141
    Top = 57
    Width = 121
    Height = 21
    EditLabel.Width = 25
    EditLabel.Height = 13
    EditLabel.Caption = 'New:'
    LabelPosition = lpLeft
    TabOrder = 1
  end
  object OKButton: TButton
    Left = 117
    Top = 114
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
  end
  object cancelButton: TButton
    Left = 198
    Top = 114
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
