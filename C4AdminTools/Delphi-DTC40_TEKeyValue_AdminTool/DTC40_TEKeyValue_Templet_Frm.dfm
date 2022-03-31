object DTC40_TEKeyValue_Templet_Form: TDTC40_TEKeyValue_Templet_Form
  Left = 0
  Top = 0
  Caption = 'C40 TEKeyValue Admin tool.'
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
  object _B_Splitter: TSplitter
    Left = 0
    Top = 339
    Width = 1069
    Height = 8
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    Color = clBlue
    ParentColor = False
    ExplicitTop = 344
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
    Height = 308
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object lpLSplitter: TSplitter
      Left = 326
      Top = 0
      Width = 8
      Height = 308
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
      Width = 326
      Height = 308
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object TE_ListView: TListView
        Left = 0
        Top = 33
        Width = 326
        Height = 275
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Text Engine'
          end>
        DoubleBuffered = True
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ParentColor = True
        ParentDoubleBuffered = False
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
        OnCreateItemClass = TE_ListViewCreateItemClass
        OnSelectItem = TE_ListViewSelectItem
      end
      object TE_L_ToolBarPanel: TPanel
        Left = 0
        Top = 0
        Width = 326
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          326
          33)
        object search_TE_Button: TButton
          Left = 260
          Top = 4
          Width = 54
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Serach'
          TabOrder = 0
          OnClick = search_TE_ButtonClick
        end
        object SearchEdit: TLabeledEdit
          Left = 56
          Top = 6
          Width = 120
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 24
          EditLabel.Height = 13
          EditLabel.Caption = 'text:'
          LabelPosition = lpLeft
          TabOrder = 1
        end
        object NumEdit: TLabeledEdit
          Left = 214
          Top = 6
          Width = 40
          Height = 21
          Anchors = [akTop, akRight]
          EditLabel.Width = 25
          EditLabel.Height = 13
          EditLabel.Caption = 'Num:'
          LabelPosition = lpLeft
          TabOrder = 2
          Text = '5000'
        end
      end
    end
    object rCliPanel: TPanel
      Left = 334
      Top = 0
      Width = 735
      Height = 308
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object TE_Memo: TMemo
        Left = 0
        Top = 33
        Width = 735
        Height = 275
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
        TabOrder = 0
        WordWrap = False
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 735
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          735
          33)
        object UpdateMemoTo_TE_Button: TButton
          Left = 182
          Top = 4
          Width = 69
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Update'
          TabOrder = 0
          OnClick = UpdateMemoTo_TE_ButtonClick
        end
        object TE_Name_Edit: TLabeledEdit
          Left = 56
          Top = 6
          Width = 120
          Height = 21
          EditLabel.Width = 46
          EditLabel.Height = 13
          EditLabel.Caption = 'TE Name:'
          LabelPosition = lpLeft
          TabOrder = 1
        end
      end
    end
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 74
    Top = 242
  end
end
