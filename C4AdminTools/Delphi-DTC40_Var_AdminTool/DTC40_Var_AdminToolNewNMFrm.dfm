object DTC40_Var_AdminToolNewNMForm: TDTC40_Var_AdminToolNewNMForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 20
  Caption = 'New Number Module.'
  ClientHeight = 306
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmExplicit
  PopupParent = DTC40_Var_AdminToolForm.Owner
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 55
    Width = 30
    Height = 13
    Caption = 'script:'
  end
  object NameEdit: TLabeledEdit
    Left = 0
    Top = 16
    Width = 121
    Height = 21
    EditLabel.Width = 104
    EditLabel.Height = 13
    EditLabel.Caption = 'Number Module Name'
    TabOrder = 0
  end
  object ScriptMemo: TMemo
    Left = 0
    Top = 74
    Width = 460
    Height = 175
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      '// current Number Module example: Set(Key,Value)'
      '// System Number Module example: SetSys(Name,Key,Value)')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object TempCheckBox: TCheckBox
    Left = 134
    Top = 19
    Width = 91
    Height = 17
    Caption = 'Temp Module'
    TabOrder = 1
  end
  object LifeTimeEdit: TLabeledEdit
    Left = 224
    Top = 16
    Width = 52
    Height = 21
    EditLabel.Width = 106
    EditLabel.Height = 13
    EditLabel.Caption = 'Temp Module Life time'
    TabOrder = 2
    Text = '5*1000'
  end
  object CreateNMButton: TButton
    Left = 0
    Top = 281
    Width = 75
    Height = 25
    Caption = '&OK'
    TabOrder = 4
    OnClick = CreateNMButtonClick
  end
  object CancelButton: TButton
    Left = 81
    Top = 281
    Width = 75
    Height = 25
    Caption = '&Cancel'
    TabOrder = 5
    OnClick = CancelButtonClick
  end
end
