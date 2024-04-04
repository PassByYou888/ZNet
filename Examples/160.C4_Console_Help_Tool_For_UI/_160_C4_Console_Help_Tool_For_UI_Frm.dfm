object _160_C4_Console_Help_Tool_For_UI_Form: T_160_C4_Console_Help_Tool_For_UI_Form
  Left = 0
  Top = 0
  Caption = #22312'UI'#31243#24207#20013#20351#29992'C4'#21629#20196#34892#24037#20855
  ClientHeight = 411
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    852
    411)
  PixelsPerInch = 96
  TextHeight = 13
  object CMD_Edit: TLabeledEdit
    Left = 96
    Top = 23
    Width = 249
    Height = 21
    EditLabel.Width = 28
    EditLabel.Height = 13
    EditLabel.Caption = #21629#20196':'
    LabelPosition = lpLeft
    TabOrder = 0
    Text = 'help'
    OnKeyUp = CMD_EditKeyUp
  end
  object run_Button: TButton
    Left = 351
    Top = 19
    Width = 75
    Height = 25
    Caption = #25191#34892#21629#20196
    TabOrder = 1
    OnClick = run_ButtonClick
  end
  object Memo: TMemo
    Left = 32
    Top = 50
    Width = 793
    Height = 343
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'C4'#26694#26550#21629#20196#34892#24037#20855#19981#20809#21487#20197#29992#20110#20998#26512#26381#21153#22120','#36825#20063#26159#36890#29992#30340'Z'#31995#20869#26680#29366#24577#20998#26512#24037#20855'.'
      '')
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object sysTimer: TTimer
    Interval = 1
    OnTimer = sysTimerTimer
    Left = 424
    Top = 208
  end
end
