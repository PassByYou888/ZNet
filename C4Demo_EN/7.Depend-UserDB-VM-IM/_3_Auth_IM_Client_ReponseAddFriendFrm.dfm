object _3_Auth_IM_Client_ReponseAddFriendForm: T_3_Auth_IM_Client_ReponseAddFriendForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 10
  Caption = 'Respond to add friend requests'
  ClientHeight = 136
  ClientWidth = 357
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
    Left = 55
    Top = 29
    Width = 52
    Height = 13
    Caption = 'Message content:'
  end
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
  object Memo: TMemo
    Left = 55
    Top = 48
    Width = 302
    Height = 57
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object AcceeptFriendRequeseButton: TButton
    Left = 55
    Top = 111
    Width = 75
    Height = 25
    Caption = 'accept'
    TabOrder = 2
    OnClick = AcceeptFriendRequeseButtonClick
  end
  object RejectFriendRequeseButton: TButton
    Left = 143
    Top = 111
    Width = 75
    Height = 25
    Caption = 'refuse'
    TabOrder = 3
    OnClick = RejectFriendRequeseButtonClick
  end
end
