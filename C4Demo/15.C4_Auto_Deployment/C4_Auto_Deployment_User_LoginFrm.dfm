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
    Top = 299
    Width = 752
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    ExplicitLeft = -343
    ExplicitTop = 344
    ExplicitWidth = 1069
  end
  object TopBarPanel: TPanel
    Left = 0
    Top = 0
    Width = 752
    Height = 31
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      752
      31)
    object JoinHostEdit: TLabeledEdit
      Left = 48
      Top = 5
      Width = 105
      Height = 21
      EditLabel.Width = 35
      EditLabel.Height = 13
      EditLabel.Caption = 'c4'#22320#22336
      LabelPosition = lpLeft
      TabOrder = 0
    end
    object JoinPortEdit: TLabeledEdit
      Left = 184
      Top = 5
      Width = 41
      Height = 21
      EditLabel.Width = 24
      EditLabel.Height = 13
      EditLabel.Caption = #31471#21475
      LabelPosition = lpLeft
      TabOrder = 1
    end
    object BuildDependNetButton: TButton
      Left = 618
      Top = 2
      Width = 76
      Height = 25
      Anchors = [akTop, akRight]
      Caption = #24314#31435'C4'#32593#32476
      TabOrder = 4
      OnClick = BuildDependNetButtonClick
    end
    object resetDependButton: TButton
      Left = 700
      Top = 2
      Width = 42
      Height = 25
      Anchors = [akTop, akRight]
      Caption = #37325#32622
      TabOrder = 5
      OnClick = resetDependButtonClick
    end
    object serviceComboBox: TComboBox
      Left = 336
      Top = 4
      Width = 276
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object queryButton: TButton
      Left = 231
      Top = 2
      Width = 99
      Height = 25
      Caption = #26597#35810#21487#29992#26381#21153'->'
      TabOrder = 2
      OnClick = queryButtonClick
    end
  end
  object logMemo: TMemo
    Left = 0
    Top = 302
    Width = 752
    Height = 150
    Align = alBottom
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
  object cliPanel: TPanel
    Left = 0
    Top = 31
    Width = 752
    Height = 268
    Align = alClient
    TabOrder = 2
    object userEdit: TLabeledEdit
      Left = 78
      Top = 13
      Width = 121
      Height = 21
      EditLabel.Width = 36
      EditLabel.Height = 13
      EditLabel.Caption = #29992#25143#21517
      LabelPosition = lpLeft
      TabOrder = 0
    end
    object passwdEdit: TLabeledEdit
      Left = 78
      Top = 40
      Width = 121
      Height = 21
      EditLabel.Width = 24
      EditLabel.Height = 13
      EditLabel.Caption = #23494#30721
      LabelPosition = lpLeft
      TabOrder = 1
    end
    object reguserButton: TButton
      Left = 78
      Top = 78
      Width = 75
      Height = 25
      Caption = #27880#20876#29992#25143
      TabOrder = 2
      OnClick = reguserButtonClick
    end
    object loginUserButton: TButton
      Left = 78
      Top = 109
      Width = 75
      Height = 25
      Caption = #29992#25143#30331#24405
      TabOrder = 3
      OnClick = loginUserButtonClick
    end
    object discButton: TButton
      Left = 78
      Top = 140
      Width = 75
      Height = 25
      Caption = #26029#24320
      TabOrder = 4
      OnClick = resetDependButtonClick
    end
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 77
    Top = 317
  end
end
