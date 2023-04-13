object user_login_Form: Tuser_login_Form
  Left = 0
  Top = 0
  Caption = 'user login'
  ClientHeight = 452
  ClientWidth = 752
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object _B_Splitter: TSplitter
    Left = 0
    Top = 213
    Width = 752
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    ExplicitLeft = -343
    ExplicitTop = 344
    ExplicitWidth = 1069
  end
  object logMemo: TMemo
    Left = 0
    Top = 216
    Width = 752
    Height = 236
    Align = alBottom
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object JoinHostEdit: TLabeledEdit
    Left = 70
    Top = 18
    Width = 105
    Height = 21
    EditLabel.Width = 35
    EditLabel.Height = 13
    EditLabel.Caption = 'c4'#22320#22336
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '127.0.0.1'
  end
  object JoinPortEdit: TLabeledEdit
    Left = 206
    Top = 18
    Width = 41
    Height = 21
    EditLabel.Width = 24
    EditLabel.Height = 13
    EditLabel.Caption = #31471#21475
    LabelPosition = lpLeft
    TabOrder = 2
    Text = '8990'
  end
  object userEdit: TLabeledEdit
    Left = 70
    Top = 45
    Width = 121
    Height = 21
    EditLabel.Width = 36
    EditLabel.Height = 13
    EditLabel.Caption = #29992#25143#21517
    LabelPosition = lpLeft
    TabOrder = 3
  end
  object passwdEdit: TLabeledEdit
    Left = 70
    Top = 72
    Width = 121
    Height = 21
    EditLabel.Width = 24
    EditLabel.Height = 13
    EditLabel.Caption = #23494#30721
    LabelPosition = lpLeft
    TabOrder = 4
  end
  object reguserButton: TButton
    Left = 70
    Top = 110
    Width = 75
    Height = 25
    Caption = #27880#20876#29992#25143
    TabOrder = 5
    OnClick = reguserButtonClick
  end
  object loginUserButton: TButton
    Left = 70
    Top = 141
    Width = 75
    Height = 25
    Caption = #29992#25143#30331#24405
    TabOrder = 6
    OnClick = loginUserButtonClick
  end
  object discButton: TButton
    Left = 70
    Top = 172
    Width = 75
    Height = 25
    Caption = #26029#24320
    TabOrder = 7
    OnClick = resetDependButtonClick
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 77
    Top = 317
  end
end
