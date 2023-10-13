object _3_Auth_IM_Client_RemoveFriendForm: T_3_Auth_IM_Client_RemoveFriendForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 10
  Caption = 'Remove friends'
  ClientHeight = 72
  ClientWidth = 218
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  PopupParent = _3_Auth_IM_Client_Form.Owner
  Position = poMainFormCenter
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object ToUserNameEdit: TLabeledEdit
    Left = 55
    Top = 0
    Width = 118
    Height = 21
    EditLabel.Width = 52
    EditLabel.Height = 13
    EditLabel.Caption = 'Opposite account:'
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object sendRequeseButton: TButton
    Left = 55
    Top = 47
    Width = 75
    Height = 25
    Caption = 'remove'
    TabOrder = 1
    OnClick = sendRequeseButtonClick
  end
  object CancelButton: TButton
    Left = 143
    Top = 47
    Width = 75
    Height = 25
    Caption = 'cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
end
