object DTC40_Alias_AdminToolForm: TDTC40_Alias_AdminToolForm
  Left = 0
  Top = 0
  Caption = 'Alias Administrator tool.'
  ClientHeight = 497
  ClientWidth = 1069
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object _B_Splitter: TSplitter
    Left = 0
    Top = 344
    Width = 1069
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    ExplicitTop = 31
    ExplicitWidth = 316
  end
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
  object TopBarPanel: TPanel
    Left = 0
    Top = 0
    Width = 1069
    Height = 31
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
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
  object cliPanel: TPanel
    Left = 0
    Top = 31
    Width = 1069
    Height = 313
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object logDBToolBarPanel: TPanel
      Left = 0
      Top = 0
      Width = 1069
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object AliasFilterEdit: TLabeledEdit
        Left = 56
        Top = 6
        Width = 128
        Height = 21
        EditLabel.Width = 26
        EditLabel.Height = 13
        EditLabel.Caption = 'Alias:'
        LabelPosition = lpLeft
        TabOrder = 0
      end
      object searchAliasButton: TButton
        Left = 190
        Top = 2
        Width = 54
        Height = 25
        Caption = 'Serach'
        TabOrder = 1
        OnClick = searchAliasButtonClick
      end
      object removeAliasButton: TButton
        Left = 250
        Top = 2
        Width = 54
        Height = 25
        Caption = 'Remove'
        TabOrder = 2
        OnClick = removeAliasButtonClick
      end
      object NewAliasButton: TButton
        Left = 310
        Top = 2
        Width = 63
        Height = 25
        Caption = 'New Alias'
        TabOrder = 3
        OnClick = NewAliasButtonClick
      end
    end
    object AliasListView: TListView
      Left = 0
      Top = 33
      Width = 1069
      Height = 280
      Align = alClient
      BevelOuter = bvNone
      BorderStyle = bsNone
      Columns = <
        item
          AutoSize = True
          Caption = 'Alias'
        end
        item
          AutoSize = True
          Caption = 'Name'
        end>
      DoubleBuffered = True
      HideSelection = False
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      ParentColor = True
      ParentDoubleBuffered = False
      TabOrder = 1
      ViewStyle = vsReport
      OnDblClick = AliasListViewDblClick
      OnKeyUp = AliasListViewKeyUp
    end
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 74
    Top = 242
  end
end
