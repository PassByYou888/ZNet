object _3_Auth_IM_Client_SendMsgForm: T_3_Auth_IM_Client_SendMsgForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 10
  Caption = 'Send Message.'
  ClientHeight = 249
  ClientWidth = 412
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
    Left = 43
    Top = 29
    Width = 52
    Height = 13
    Caption = #28040#24687#20869#23481':'
  end
  object ToUserNameEdit: TLabeledEdit
    Left = 43
    Top = 0
    Width = 217
    Height = 21
    EditLabel.Width = 40
    EditLabel.Height = 13
    EditLabel.Caption = #25910#20214#20154':'
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object Memo: TMemo
    Left = 43
    Top = 48
    Width = 369
    Height = 153
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object sendButton: TButton
    Left = 43
    Top = 224
    Width = 75
    Height = 25
    Caption = #21457#36865
    TabOrder = 2
    OnClick = sendButtonClick
  end
  object CancelButton: TButton
    Left = 131
    Top = 224
    Width = 75
    Height = 25
    Caption = #21462#28040
    TabOrder = 3
    OnClick = CancelButtonClick
  end
end
