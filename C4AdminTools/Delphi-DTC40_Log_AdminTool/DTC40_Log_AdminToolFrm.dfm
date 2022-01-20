object DTC40_Log_AdminToolForm: TDTC40_Log_AdminToolForm
  Left = 0
  Top = 0
  Caption = 'Cloud 4.0 Log Database Administrator Tool.'
  ClientHeight = 391
  ClientWidth = 934
  Color = clBtnFace
  Constraints.MinHeight = 450
  Constraints.MinWidth = 950
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu_
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object botSplitter: TSplitter
    Left = 0
    Top = 254
    Width = 934
    Height = 8
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    Color = clBlue
    MinSize = 120
    ParentColor = False
    ResizeStyle = rsUpdate
    ExplicitLeft = -107
    ExplicitTop = 294
  end
  object logMemo: TMemo
    Left = 0
    Top = 262
    Width = 934
    Height = 129
    Align = alBottom
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    DoubleBuffered = True
    ParentColor = True
    ParentDoubleBuffered = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object TopBarPanel: TPanel
    Left = 0
    Top = 0
    Width = 934
    Height = 31
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      934
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
      Left = 293
      Top = 5
      Width = 68
      Height = 21
      EditLabel.Width = 37
      EditLabel.Height = 13
      EditLabel.Caption = 'Depend'
      LabelPosition = lpLeft
      TabOrder = 2
    end
    object BuildDependNetButton: TButton
      Left = 800
      Top = 2
      Width = 76
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Build Tunnel'
      TabOrder = 6
      OnClick = BuildDependNetButtonClick
    end
    object resetDependButton: TButton
      Left = 882
      Top = 2
      Width = 42
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Reset'
      TabOrder = 7
      OnClick = resetDependButtonClick
    end
    object serviceComboBox: TComboBox
      Left = 450
      Top = 4
      Width = 179
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
    end
    object queryButton: TButton
      Left = 367
      Top = 2
      Width = 77
      Height = 25
      Caption = 'query to ->'
      TabOrder = 3
      OnClick = queryButtonClick
    end
    object DTC4PasswdEdit: TLabeledEdit
      Left = 690
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
      OnChange = DTC4PasswdEditChange
    end
  end
  object cliPanel: TPanel
    Left = 0
    Top = 31
    Width = 934
    Height = 223
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object lpLSplitter: TSplitter
      Left = 341
      Top = 0
      Width = 8
      Height = 223
      AutoSnap = False
      Color = clBlue
      MinSize = 250
      ParentColor = False
      ResizeStyle = rsUpdate
      ExplicitLeft = 506
      ExplicitTop = 6
      ExplicitHeight = 330
    end
    object leftPanel: TPanel
      Left = 0
      Top = 0
      Width = 341
      Height = 223
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object LogDBListView: TListView
        Left = 0
        Top = 33
        Width = 341
        Height = 190
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsNone
        Checkboxes = True
        Columns = <
          item
            AutoSize = True
            Caption = 'Log DB'
          end
          item
            Caption = 'States'
            Width = 80
          end>
        DoubleBuffered = True
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ParentColor = True
        ParentDoubleBuffered = False
        PopupMenu = PopupMenu_
        TabOrder = 0
        ViewStyle = vsReport
        OnCreateItemClass = LogDBListViewCreateItemClass
      end
      object logDBToolBarPanel: TPanel
        Left = 0
        Top = 0
        Width = 341
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object LogDBFilterEdit: TLabeledEdit
          Left = 56
          Top = 6
          Width = 83
          Height = 21
          EditLabel.Width = 37
          EditLabel.Height = 13
          EditLabel.Caption = 'Log DB:'
          LabelPosition = lpLeft
          TabOrder = 0
        end
        object searchLogDBButton: TButton
          Left = 145
          Top = 2
          Width = 54
          Height = 25
          Caption = 'Serach'
          TabOrder = 1
          OnClick = searchLogDBButtonClick
        end
        object checkAllButton: TButton
          Left = 205
          Top = 2
          Width = 40
          Height = 25
          Caption = 'check'
          TabOrder = 2
          OnClick = checkAllButtonClick
        end
        object uncheckAllButton: TButton
          Left = 251
          Top = 2
          Width = 54
          Height = 25
          Caption = 'uncheck'
          TabOrder = 3
          OnClick = uncheckAllButtonClick
        end
      end
    end
    object rCliPanel: TPanel
      Left = 349
      Top = 0
      Width = 585
      Height = 223
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object logQueryToolBarPanel: TPanel
        Left = 0
        Top = 0
        Width = 585
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 12
          Top = 10
          Width = 55
          Height = 13
          Alignment = taRightJustify
          Caption = 'time range:'
        end
        object filter1Edit: TLabeledEdit
          Left = 268
          Top = 6
          Width = 83
          Height = 21
          EditLabel.Width = 32
          EditLabel.Height = 13
          EditLabel.Caption = 'filter1:'
          LabelPosition = lpLeft
          TabOrder = 0
        end
        object SearchLogButton: TButton
          Left = 488
          Top = 4
          Width = 54
          Height = 25
          Caption = 'Serach'
          TabOrder = 1
          OnClick = SearchLogButtonClick
        end
        object filter2Edit: TLabeledEdit
          Left = 399
          Top = 6
          Width = 83
          Height = 21
          EditLabel.Width = 32
          EditLabel.Height = 13
          EditLabel.Caption = 'filter2:'
          LabelPosition = lpLeft
          TabOrder = 2
        end
        object TimeRangeComboBox: TComboBox
          Left = 73
          Top = 6
          Width = 145
          Height = 21
          Style = csDropDownList
          DropDownCount = 12
          ItemIndex = 8
          TabOrder = 3
          Text = 'Last 1 Year'
          Items.Strings = (
            'Last 60 seconds'
            'Last 10 minutes'
            'Last 1 hour'
            'ToDay'
            'Last 3 days'
            'Last week'
            'Last month'
            'Last 3 month'
            'Last 1 Year')
        end
      end
      object QueryMemo: TMemo
        Left = 0
        Top = 33
        Width = 585
        Height = 190
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        ParentColor = True
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
    end
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 309
    Top = 77
  end
  object ActionList_: TActionList
    Left = 309
    Top = 138
    object Action_RemoveLogDB: TAction
      Caption = 'Remove of Log DB'
      OnExecute = Action_RemoveLogDBExecute
    end
  end
  object MainMenu_: TMainMenu
    Left = 307
    Top = 204
    object File1: TMenuItem
      Caption = '&File'
      object RemoveofLogDB1: TMenuItem
        Action = Action_RemoveLogDB
      end
    end
  end
  object PopupMenu_: TPopupMenu
    Left = 305
    Top = 261
    object RemoveofLogDB2: TMenuItem
      Action = Action_RemoveLogDB
    end
  end
end
