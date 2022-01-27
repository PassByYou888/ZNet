object NewAliasForm: TNewAliasForm
  Left = 0
  Top = 0
  Caption = 'New Alias.'
  ClientHeight = 174
  ClientWidth = 435
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  PopupParent = DTC40_Alias_AdminToolForm.Owner
  Position = poMainFormCenter
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  DesignSize = (
    435
    174)
  PixelsPerInch = 96
  TextHeight = 13
  object AliasEdit: TLabeledEdit
    Left = 87
    Top = 27
    Width = 285
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 26
    EditLabel.Height = 13
    EditLabel.Caption = 'Alias:'
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object NameEdit: TLabeledEdit
    Left = 87
    Top = 66
    Width = 285
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 31
    EditLabel.Height = 13
    EditLabel.Caption = 'Name:'
    LabelPosition = lpLeft
    TabOrder = 1
  end
  object OKButton: TButton
    Left = 87
    Top = 120
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
  end
  object cancelButton: TButton
    Left = 168
    Top = 120
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
