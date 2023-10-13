object _3_Auth_IM_Client_RequestAddFriendForm: T_3_Auth_IM_Client_RequestAddFriendForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 10
  Caption = 'Request to add friends'
  ClientHeight = 136
  ClientWidth = 333
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
  object Label1: TLabel
    Left = 31
    Top = 29
    Width = 52
    Height = 13
    Caption = 'Message content:'
  end
  object ToUserNameEdit: TLabeledEdit
    Left = 31
    Top = 0
    Width = 118
    Height = 21
    EditLabel.Width = 52
    EditLabel.Height = 13
    EditLabel.Caption = 'Opposite account:'
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object Memo: TMemo
    Left = 31
    Top = 48
    Width = 302
    Height = 57
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object sendRequeseButton: TButton
    Left = 31
    Top = 111
    Width = 75
    Height = 25
    Caption = 'Send request'
    TabOrder = 2
    OnClick = sendRequeseButtonClick
  end
  object CancelButton: TButton
    Left = 119
    Top = 111
    Width = 75
    Height = 25
    Caption = 'cancel'
    TabOrder = 3
    OnClick = CancelButtonClick
  end
end
