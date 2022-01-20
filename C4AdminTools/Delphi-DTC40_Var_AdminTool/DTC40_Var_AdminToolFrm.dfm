object DTC40_Var_AdminToolForm: TDTC40_Var_AdminToolForm
  Left = 0
  Top = 0
  Caption = 'Cloud 4.0 Variant System Administrator Tool.'
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
      Left = 691
      Top = 4
      Width = 103
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
      Left = 425
      Top = 33
      Width = 8
      Height = 190
      AutoSnap = False
      Color = clBlue
      MinSize = 250
      ParentColor = False
      ResizeStyle = rsUpdate
      ExplicitLeft = 158
      ExplicitTop = 0
      ExplicitHeight = 284
    end
    object leftPanel: TPanel
      Left = 0
      Top = 33
      Width = 425
      Height = 190
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object NMListView: TListView
        Left = 0
        Top = 0
        Width = 425
        Height = 190
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Pool'
          end>
        DoubleBuffered = True
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ParentColor = True
        ParentDoubleBuffered = False
        PopupMenu = NM_PopupMenu_
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
        OnCreateItemClass = NMListViewCreateItemClass
        OnSelectItem = NMListViewSelectItem
      end
    end
    object listToolBarPanel: TPanel
      Left = 0
      Top = 0
      Width = 934
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object SearchEdit: TLabeledEdit
        Left = 53
        Top = 3
        Width = 108
        Height = 21
        EditLabel.Width = 20
        EditLabel.Height = 13
        EditLabel.Caption = 'var:'
        LabelPosition = lpLeft
        TabOrder = 0
      end
      object SearchButton: TButton
        Left = 245
        Top = 1
        Width = 66
        Height = 25
        Caption = 'Go Serach'
        TabOrder = 2
        OnClick = SearchButtonClick
      end
      object NumEdit: TLabeledEdit
        Left = 199
        Top = 3
        Width = 40
        Height = 21
        EditLabel.Width = 25
        EditLabel.Height = 13
        EditLabel.Caption = 'Num:'
        LabelPosition = lpLeft
        TabOrder = 1
        Text = '100'
      end
      object ScriptEdit: TLabeledEdit
        Left = 358
        Top = 3
        Width = 164
        Height = 21
        EditLabel.Width = 31
        EditLabel.Height = 13
        EditLabel.Caption = 'Script:'
        LabelPosition = lpLeft
        TabOrder = 3
      end
      object RunScriptButton: TButton
        Left = 528
        Top = 1
        Width = 66
        Height = 25
        Caption = 'Run Script'
        TabOrder = 4
        OnClick = RunScriptButtonClick
      end
    end
    object rCliPanel: TPanel
      Left = 433
      Top = 33
      Width = 501
      Height = 190
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object VarListView: TListView
        Left = 0
        Top = 0
        Width = 501
        Height = 190
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Var'
          end
          item
            AutoSize = True
            Caption = 'Value'
          end>
        DoubleBuffered = True
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ParentColor = True
        ParentDoubleBuffered = False
        PopupMenu = Var_PopupMenu_
        TabOrder = 0
        ViewStyle = vsReport
        OnCreateItemClass = VarListViewCreateItemClass
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
    object Action_NewNM: TAction
      Caption = 'New Number Module.'
      OnExecute = Action_NewNMExecute
    end
    object Action_RemoveNM: TAction
      Caption = 'Remove Number Module'
      OnExecute = Action_RemoveNMExecute
    end
    object Action_RemoveNMKey: TAction
      Caption = 'Remove Key-Value'
      OnExecute = Action_RemoveNMKeyExecute
    end
  end
  object MainMenu_: TMainMenu
    Left = 307
    Top = 204
    object File1: TMenuItem
      Caption = '&File'
      object NewNumberModule1: TMenuItem
        Action = Action_NewNM
      end
      object RemoveNumberModule1: TMenuItem
        Action = Action_RemoveNM
      end
    end
  end
  object NM_PopupMenu_: TPopupMenu
    Left = 232
    Top = 207
    object NewNumberModule2: TMenuItem
      Action = Action_NewNM
    end
    object RemoveNumberModule2: TMenuItem
      Action = Action_RemoveNM
    end
  end
  object Var_PopupMenu_: TPopupMenu
    Left = 503
    Top = 198
    object RemoveKeyValue1: TMenuItem
      Action = Action_RemoveNMKey
    end
  end
end
