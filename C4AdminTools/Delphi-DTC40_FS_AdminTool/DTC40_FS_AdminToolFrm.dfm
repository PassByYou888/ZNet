object DTC40_FS_AdminToolForm: TDTC40_FS_AdminToolForm
  Left = 0
  Top = 0
  Caption = 'Cloud 4.0 File System Administrator Tool.'
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
    object FileListView: TListView
      Left = 0
      Top = 33
      Width = 934
      Height = 190
      Align = alClient
      BevelOuter = bvNone
      BorderStyle = bsNone
      Columns = <
        item
          AutoSize = True
          Caption = 'Remote Files'
        end
        item
          Alignment = taCenter
          Caption = 'File Time'
          Width = 120
        end
        item
          Alignment = taRightJustify
          Caption = 'Size'
          Width = 120
        end
        item
          Alignment = taRightJustify
          Caption = 'MD5'
          Width = 220
        end
        item
          Alignment = taCenter
          Caption = 'states'
          Width = 80
        end>
      DoubleBuffered = True
      HideSelection = False
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      ParentDoubleBuffered = False
      PopupMenu = PopupMenu_
      TabOrder = 0
      ViewStyle = vsReport
      OnCreateItemClass = FileListViewCreateItemClass
    end
    object listToolBarPanel: TPanel
      Left = 0
      Top = 0
      Width = 934
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object FS_Info_Label: TLabel
        Left = 335
        Top = 7
        Width = 12
        Height = 13
        Caption = '...'
      end
      object SearchEdit: TLabeledEdit
        Left = 67
        Top = 4
        Width = 109
        Height = 21
        EditLabel.Width = 49
        EditLabel.Height = 13
        EditLabel.Caption = 'File name:'
        LabelPosition = lpLeft
        TabOrder = 0
      end
      object SearchButton: TButton
        Left = 262
        Top = 3
        Width = 66
        Height = 25
        Caption = 'Go Serach'
        TabOrder = 1
        OnClick = SearchButtonClick
      end
      object NumEdit: TLabeledEdit
        Left = 216
        Top = 4
        Width = 40
        Height = 21
        EditLabel.Width = 25
        EditLabel.Height = 13
        EditLabel.Caption = 'Num:'
        LabelPosition = lpLeft
        TabOrder = 2
        Text = '100'
      end
    end
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 309
    Top = 77
  end
  object UploadFileOpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 201
    Top = 200
  end
  object ActionList_: TActionList
    Left = 309
    Top = 138
    object Action_DownloadFile: TAction
      Caption = 'Download'
      OnExecute = Action_DownloadFileExecute
    end
    object Action_UploadFile: TAction
      Caption = 'Upload File.'
      OnExecute = Action_UploadFileExecute
    end
    object Action_RemoveFile: TAction
      Caption = 'Remove file.'
      OnExecute = Action_RemoveFileExecute
    end
    object Action_exit: TAction
      Caption = 'Exit'
      OnExecute = Action_exitExecute
    end
  end
  object MainMenu_: TMainMenu
    Left = 307
    Top = 204
    object File1: TMenuItem
      Caption = '&File'
      object Download1: TMenuItem
        Action = Action_DownloadFile
      end
      object Removefile1: TMenuItem
        Action = Action_RemoveFile
      end
      object UploadFile1: TMenuItem
        Action = Action_UploadFile
      end
      object Exit1: TMenuItem
        Action = Action_exit
      end
    end
  end
  object PopupMenu_: TPopupMenu
    Left = 305
    Top = 261
    object Download2: TMenuItem
      Action = Action_DownloadFile
    end
    object Removefile2: TMenuItem
      Action = Action_RemoveFile
    end
    object UploadFile2: TMenuItem
      Action = Action_UploadFile
    end
  end
end
