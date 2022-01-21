object UnitRewriteTool_C_Form: TUnitRewriteTool_C_Form
  Left = 0
  Top = 0
  Caption = 'UnitRewrite Tool and Freature model.'
  ClientHeight = 497
  ClientWidth = 1069
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
  object logMemo: TMemo
    Left = 0
    Top = 347
    Width = 1069
    Height = 150
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
  object PC: TPageControl
    Left = 0
    Top = 31
    Width = 1069
    Height = 316
    ActivePage = Test_TabSheet
    Align = alClient
    TabOrder = 1
    Visible = False
    object Test_TabSheet: TTabSheet
      Caption = 'Test Code'
      object Test_top_Panel: TPanel
        Left = 0
        Top = 0
        Width = 1061
        Height = 36
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 0
        object BuildMyCodeButton: TButton
          Left = 3
          Top = 3
          Width = 154
          Height = 30
          Align = alLeft
          Caption = 'Rewrite(Build-in Model)'
          TabOrder = 0
          OnClick = BuildMyCodeButtonClick
        end
      end
      object TestMemo: TMemo
        Left = 0
        Top = 36
        Width = 1061
        Height = 252
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -14
        Font.Name = 'Consolas'
        Font.Style = []
        Lines.Strings = (
          '//Test is easy for debugging developers'
          '//Using rewrite tool environment to build code'
          '//ctrl+v Paste the code and run the test'
          'unit my_unit;'
          ''
          'interface'
          ''
          'implementation'
          ''
          'end.')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
    end
  end
  object TopBarPanel: TPanel
    Left = 0
    Top = 0
    Width = 1069
    Height = 31
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      1069
      31)
    object JoinHostEdit: TLabeledEdit
      Left = 56
      Top = 5
      Width = 105
      Height = 21
      EditLabel.Width = 44
      EditLabel.Height = 13
      EditLabel.Caption = 'Join Host'
      LabelPosition = lpLeft
      TabOrder = 0
    end
    object JoinPortEdit: TLabeledEdit
      Left = 190
      Top = 5
      Width = 49
      Height = 21
      EditLabel.Width = 20
      EditLabel.Height = 13
      EditLabel.Caption = 'Port'
      LabelPosition = lpLeft
      TabOrder = 1
    end
    object DependEdit: TLabeledEdit
      Left = 284
      Top = 5
      Width = 147
      Height = 21
      EditLabel.Width = 37
      EditLabel.Height = 13
      EditLabel.Caption = 'Depend'
      LabelPosition = lpLeft
      TabOrder = 2
    end
    object BuildDependNetButton: TButton
      Left = 935
      Top = 2
      Width = 76
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Build Tunnel'
      TabOrder = 6
      OnClick = BuildDependNetButtonClick
    end
    object resetDependButton: TButton
      Left = 1017
      Top = 2
      Width = 42
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Reset'
      TabOrder = 7
      OnClick = resetDependButtonClick
    end
    object serviceComboBox: TComboBox
      Left = 528
      Top = 4
      Width = 236
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
    end
    object queryButton: TButton
      Left = 437
      Top = 2
      Width = 85
      Height = 25
      Caption = 'query to ->'
      TabOrder = 3
      OnClick = queryButtonClick
    end
    object DTC4PasswdEdit: TLabeledEdit
      Left = 825
      Top = 4
      Width = 104
      Height = 21
      Anchors = [akTop, akRight]
      EditLabel.Width = 52
      EditLabel.Height = 13
      EditLabel.Caption = 'C4 Passwd'
      LabelPosition = lpLeft
      PasswordChar = '*'
      TabOrder = 5
    end
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 74
    Top = 242
  end
end
