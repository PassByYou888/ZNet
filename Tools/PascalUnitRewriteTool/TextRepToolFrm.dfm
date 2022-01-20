object TextRepToolForm: TTextRepToolForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 50
  Caption = 'Replace Tool.'
  ClientHeight = 145
  ClientWidth = 225
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
  object OLDEdit: TLabeledEdit
    Left = 67
    Top = 0
    Width = 131
    Height = 21
    EditLabel.Width = 63
    EditLabel.Height = 13
    EditLabel.Caption = 'OLD Pattern:'
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object NewEdit: TLabeledEdit
    Left = 67
    Top = 27
    Width = 131
    Height = 21
    EditLabel.Width = 64
    EditLabel.Height = 13
    EditLabel.Caption = 'New Pattern:'
    LabelPosition = lpLeft
    TabOrder = 1
  end
  object Word_CheckBox: TCheckBox
    Left = 69
    Top = 62
    Width = 97
    Height = 17
    Caption = 'Word'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object IgnoreCase_CheckBox: TCheckBox
    Left = 69
    Top = 85
    Width = 97
    Height = 17
    Caption = 'Ignore Case'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object OKButton: TButton
    Left = 69
    Top = 120
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 4
  end
  object cancelButton: TButton
    Left = 150
    Top = 120
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
