object DTC40_UserDB_AdminToolForm: TDTC40_UserDB_AdminToolForm
  Left = 0
  Top = 0
  Caption = 'Cloud 4.0 UserDB Administrator tool.'
  ClientHeight = 431
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
    Top = 294
    Width = 934
    Height = 8
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    Color = clBlue
    MinSize = 120
    ParentColor = False
    ResizeStyle = rsUpdate
    ExplicitTop = 229
    ExplicitWidth = 791
  end
  object TopBarPanel: TPanel
    Left = 0
    Top = 0
    Width = 934
    Height = 31
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
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
      Left = 692
      Top = 4
      Width = 102
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
  object logMemo: TMemo
    Left = 0
    Top = 302
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
    TabOrder = 1
    WordWrap = False
  end
  object cliPanel: TPanel
    Left = 0
    Top = 31
    Width = 934
    Height = 263
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object lpLSplitter: TSplitter
      Left = 425
      Top = 0
      Width = 8
      Height = 263
      AutoSnap = False
      Color = clBlue
      MinSize = 250
      ParentColor = False
      ResizeStyle = rsUpdate
      ExplicitLeft = 158
      ExplicitHeight = 284
    end
    object leftPanel: TPanel
      Left = 0
      Top = 0
      Width = 425
      Height = 263
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object listToolBarPanel: TPanel
        Left = 0
        Top = 0
        Width = 425
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          425
          33)
        object SearchEdit: TLabeledEdit
          Left = 53
          Top = 3
          Width = 189
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 24
          EditLabel.Height = 13
          EditLabel.Caption = 'text:'
          LabelPosition = lpLeft
          TabOrder = 0
          OnKeyUp = SearchEditKeyUp
        end
        object SearchButton: TButton
          Left = 327
          Top = 2
          Width = 66
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Go Serach'
          TabOrder = 1
          OnClick = SearchButtonClick
        end
        object NumEdit: TLabeledEdit
          Left = 281
          Top = 3
          Width = 40
          Height = 21
          Anchors = [akTop, akRight]
          EditLabel.Width = 25
          EditLabel.Height = 13
          EditLabel.Caption = 'Num:'
          LabelPosition = lpLeft
          TabOrder = 2
          Text = '100'
        end
      end
      object UserListView: TListView
        Left = 0
        Top = 33
        Width = 425
        Height = 230
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Identifier'
          end
          item
            Caption = 'Last Login'
            Width = 160
          end
          item
            Alignment = taCenter
            Caption = 'Enabled'
          end
          item
            Alignment = taCenter
            Caption = 'Online'
          end>
        DoubleBuffered = True
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ParentColor = True
        ParentDoubleBuffered = False
        PopupMenu = PopupMenu_
        TabOrder = 1
        ViewStyle = vsReport
        OnCreateItemClass = UserListViewCreateItemClass
        OnSelectItem = UserListViewSelectItem
      end
    end
    object jsonMemo: TMemo
      Left = 433
      Top = 0
      Width = 501
      Height = 263
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Constraints.MaxHeight = 450
      DoubleBuffered = True
      ParentColor = True
      ParentDoubleBuffered = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      WordWrap = False
    end
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 193
    Top = 205
  end
  object Action_List: TActionList
    Left = 252
    Top = 137
    object Action_downloadtoDir: TAction
      Caption = 'Download selected to directory.'
      OnExecute = Action_downloadtoDirExecute
    end
    object Action_UploadJson: TAction
      Caption = 'Upload json to UserDB.'
      OnExecute = Action_UploadJsonExecute
    end
    object Action_LargeScaleRegistrationTool: TAction
      Caption = 'Large-Scale Registration Tool.'
      OnExecute = Action_LargeScaleRegistrationToolExecute
    end
    object Action_Kick: TAction
      Caption = 'Kick'
      OnExecute = Action_KickExecute
    end
    object Action_Enabled: TAction
      Caption = 'Enabled'
      OnExecute = Action_EnabledExecute
    end
    object Action_Disable: TAction
      Caption = 'Disable'
      OnExecute = Action_DisableExecute
    end
    object Action_Remove: TAction
      Caption = 'Remove'
      OnExecute = Action_RemoveExecute
    end
    object Action_UserDB_State: TAction
      Caption = 'User DB Service State.'
      OnExecute = Action_UserDB_StateExecute
    end
    object Action_exit: TAction
      Caption = 'Exit'
      OnExecute = Action_exitExecute
    end
  end
  object uploadJson_OpenDialog: TOpenDialog
    Options = [ofReadOnly, ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 343
    Top = 104
  end
  object PopupMenu_: TPopupMenu
    Left = 168
    Top = 137
    object Kick1: TMenuItem
      Action = Action_Kick
    end
    object Disable1: TMenuItem
      Action = Action_Disable
    end
    object Enabled1: TMenuItem
      Action = Action_Enabled
    end
    object Remove1: TMenuItem
      Action = Action_Remove
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Downloadselectedtodirectory1: TMenuItem
      Action = Action_downloadtoDir
    end
    object UploadjsontoUserDB1: TMenuItem
      Action = Action_UploadJson
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object UserDBServiceState2: TMenuItem
      Action = Action_UserDB_State
    end
  end
  object MainMenu_: TMainMenu
    Left = 78
    Top = 137
    object File1: TMenuItem
      Caption = '&File'
      object Kick2: TMenuItem
        Action = Action_Kick
      end
      object Disable2: TMenuItem
        Action = Action_Disable
      end
      object Enabled2: TMenuItem
        Action = Action_Enabled
      end
      object Remove2: TMenuItem
        Action = Action_Remove
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Downloadselectedtodirectory2: TMenuItem
        Action = Action_downloadtoDir
      end
      object UploadjsontoUserDB2: TMenuItem
        Action = Action_UploadJson
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object UserDBServiceState1: TMenuItem
        Action = Action_UserDB_State
      end
      object LargeScaleRegistrationTool2: TMenuItem
        Action = Action_LargeScaleRegistrationTool
      end
      object Exit1: TMenuItem
        Action = Action_exit
      end
    end
  end
end
