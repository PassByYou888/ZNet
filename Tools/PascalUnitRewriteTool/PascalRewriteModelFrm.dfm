object PascalRewriteModelForm: TPascalRewriteModelForm
  Left = 0
  Top = 0
  Caption = 'Build Model for Pascal rewrite.'
  ClientHeight = 561
  ClientWidth = 1384
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 1400
  ParentFont = True
  Menu = MainMenu_
  OldCreateOrder = True
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object memoSplitter: TSplitter
    Left = 0
    Top = 420
    Width = 1384
    Height = 8
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    ResizeStyle = rsUpdate
    ExplicitTop = 422
  end
  object PageC: TPageControl
    Left = 0
    Top = 0
    Width = 1384
    Height = 420
    ActivePage = TS_define
    Align = alClient
    TabOrder = 0
    OnChange = PageCChange
    object TS_define: TTabSheet
      Caption = 'Pascal Unit Define'
      object Panel_Redefine: TPanel
        Left = 0
        Top = 0
        Width = 1376
        Height = 392
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Panel_defRight: TPanel
          Left = 0
          Top = 0
          Width = 1376
          Height = 392
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Panel_ListTopToolBar: TPanel
            Left = 0
            Top = 0
            Width = 1376
            Height = 35
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              1376
              35)
            object AddUnitSourceButton: TButton
              Left = 6
              Top = 4
              Width = 115
              Height = 25
              Caption = 'Add Pascal Source'
              TabOrder = 0
              WordWrap = True
              OnClick = AddUnitSourceButtonClick
            end
            object UnitFilterEdit: TLabeledEdit
              Left = 340
              Top = 6
              Width = 79
              Height = 21
              EditLabel.Width = 28
              EditLabel.Height = 13
              EditLabel.Caption = 'Filter:'
              LabelPosition = lpLeft
              TabOrder = 2
              Text = '*'
              OnChange = UnitFilterEditChange
            end
            object AddUnitFromDirectoryButton: TButton
              Left = 127
              Top = 4
              Width = 170
              Height = 25
              Caption = 'Add Source from Directory'
              TabOrder = 1
              WordWrap = True
              OnClick = AddUnitFromDirectoryButtonClick
            end
            object rep_unit_def_Button: TButton
              Left = 1239
              Top = 4
              Width = 133
              Height = 25
              Anchors = [akTop, akRight]
              Caption = 'Replace for New Name'
              TabOrder = 3
              WordWrap = True
              OnClick = rep_unit_def_ButtonClick
            end
            object unitDefine_text_edt_Button: TButton
              Left = 1158
              Top = 4
              Width = 75
              Height = 25
              Anchors = [akTop, akRight]
              Caption = 'Text Editor'
              TabOrder = 4
              OnClick = unitDefine_text_edt_ButtonClick
            end
          end
          object PanelUnitDefList: TPanel
            Left = 0
            Top = 35
            Width = 1376
            Height = 357
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            object unitDefList: TListView
              Left = 0
              Top = 0
              Width = 1376
              Height = 357
              Align = alClient
              BevelInner = bvNone
              BevelOuter = bvNone
              BorderStyle = bsNone
              Columns = <
                item
                  Caption = 'Source'
                  MinWidth = 100
                  Width = 200
                end
                item
                  Caption = 'New Name'
                  MinWidth = 100
                  Width = 200
                end
                item
                  Caption = 'size'
                  MinWidth = 100
                  Width = 120
                end
                item
                  Caption = 'Time'
                  Width = 170
                end
                item
                  AutoSize = True
                  Caption = 'Directory'
                  MinWidth = 200
                end>
              DoubleBuffered = True
              MultiSelect = True
              ReadOnly = True
              RowSelect = True
              ParentDoubleBuffered = False
              PopupMenu = UnitDefPopupMenu
              TabOrder = 0
              ViewStyle = vsReport
              OnColumnClick = unitDefListColumnClick
              OnDblClick = Unit_Define_Click
              OnKeyUp = unitDefListKeyUp
            end
          end
        end
      end
    end
    object TS_SymbolRewriteDefine: TTabSheet
      Caption = 'Symbol Rewrite Define'
      ImageIndex = 3
      object feature_r_Splitter: TSplitter
        Left = 933
        Top = 0
        Height = 392
        Align = alRight
        AutoSnap = False
        MinSize = 440
        ResizeStyle = rsUpdate
        ExplicitLeft = 1050
        ExplicitTop = -3
        ExplicitHeight = 329
      end
      object symbol_rewrite_cli_Panel: TPanel
        Left = 0
        Top = 0
        Width = 933
        Height = 392
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object sym_def_C_T_Panel: TPanel
          Left = 0
          Top = 0
          Width = 933
          Height = 30
          Align = alTop
          Alignment = taLeftJustify
          BorderWidth = 2
          TabOrder = 0
          object paste_and_fill_fromtext_Button: TButton
            Left = 129
            Top = 3
            Width = 125
            Height = 24
            Align = alLeft
            Caption = 'Paste and Fill Feature'
            TabOrder = 0
            OnClick = Fill_TextAsSymbolDefine_Click
          end
          object reset_symbol_rewrite_define_Button: TButton
            Left = 890
            Top = 3
            Width = 40
            Height = 24
            Align = alRight
            Caption = 'Reset'
            TabOrder = 1
            OnClick = reset_symbol_rewrite_define_ButtonClick
          end
          object Paste_Replace_Sour_Edit: TLabeledEdit
            Left = 305
            Top = 4
            Width = 104
            Height = 21
            EditLabel.Width = 40
            EditLabel.Height = 13
            EditLabel.Caption = 'Pattern:'
            LabelPosition = lpLeft
            TabOrder = 2
          end
          object Paste_Replace_Dest_Edit: TLabeledEdit
            Left = 479
            Top = 4
            Width = 104
            Height = 21
            EditLabel.Width = 57
            EditLabel.Height = 13
            EditLabel.Caption = 'Replace To:'
            LabelPosition = lpLeft
            TabOrder = 3
          end
          object update_sym_Button: TButton
            Left = 82
            Top = 3
            Width = 47
            Height = 24
            Align = alLeft
            Caption = 'update'
            TabOrder = 4
            OnClick = Rebuild_Symbol_Click
          end
          object addSymbolButton: TButton
            Left = 3
            Top = 3
            Width = 79
            Height = 24
            Align = alLeft
            Caption = 'New symbol'
            TabOrder = 5
            OnClick = addSymbolButtonClick
          end
          object rep_sym_old_Button: TButton
            Left = 644
            Top = 3
            Width = 123
            Height = 24
            Align = alRight
            Caption = 'Replace OLD Symbol'
            TabOrder = 6
            OnClick = rep_sym_old_ButtonClick
          end
          object rep_sym_new_Button: TButton
            Left = 767
            Top = 3
            Width = 123
            Height = 24
            Align = alRight
            Caption = 'Replace New Symbol'
            TabOrder = 7
            OnClick = rep_sym_new_ButtonClick
          end
        end
        object SymbolJsonSourceMemo: TMemo
          Left = 0
          Top = 30
          Width = 933
          Height = 362
          Align = alClient
          DoubleBuffered = True
          ParentDoubleBuffered = False
          ScrollBars = ssBoth
          TabOrder = 1
          WordWrap = False
          OnExit = SymbolJsonSourceMemoExit
        end
      end
      object symbol_rewrite_L_Panel: TPanel
        Left = 936
        Top = 0
        Width = 440
        Height = 392
        Align = alRight
        TabOrder = 1
        object Splitter3: TSplitter
          Left = 1
          Top = 181
          Width = 438
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          AutoSnap = False
          MinSize = 150
          ResizeStyle = rsUpdate
          ExplicitTop = 31
          ExplicitWidth = 270
        end
        object sym_feature_r_t_Panel: TPanel
          Left = 1
          Top = 1
          Width = 438
          Height = 30
          Align = alTop
          Alignment = taRightJustify
          BorderWidth = 2
          Caption = 'Code Feature scan.'
          TabOrder = 0
          object AddCodeFeatureFileButton: TButton
            Left = 3
            Top = 3
            Width = 55
            Height = 24
            Align = alLeft
            Caption = 'Add Files'
            TabOrder = 0
            OnClick = AddCodeFeatureFileButtonClick
          end
          object ScanFeatureButton: TButton
            Left = 58
            Top = 3
            Width = 63
            Height = 24
            Align = alLeft
            Caption = 'Scan File'
            TabOrder = 1
            OnClick = ScanFeatureButtonClick
          end
          object CodeFeatureEdit: TLabeledEdit
            Left = 259
            Top = 3
            Width = 76
            Height = 21
            EditLabel.Width = 42
            EditLabel.Height = 13
            EditLabel.Caption = 'Feature:'
            LabelPosition = lpLeft
            TabOrder = 3
          end
          object ScanFeatureFromClipboardButton: TButton
            Left = 121
            Top = 3
            Width = 88
            Height = 24
            Align = alLeft
            Caption = 'Scan Clipboard'
            TabOrder = 2
            OnClick = ScanFeatureFromClipboardButtonClick
          end
        end
        object symbol_rewrite_file_features_LB: TListBox
          Left = 1
          Top = 31
          Width = 438
          Height = 150
          Align = alClient
          ItemHeight = 13
          MultiSelect = True
          TabOrder = 1
          OnKeyUp = symbol_rewrite_file_features_LBKeyUp
        end
        object symbol_rewrite_L_B_C_Panel: TPanel
          Left = 1
          Top = 184
          Width = 438
          Height = 207
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 2
          object FeatureOutputMemo: TMemo
            Left = 0
            Top = 19
            Width = 438
            Height = 188
            Align = alClient
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
          object symbol_rewrite_L_B_C_H_Panel: TPanel
            Left = 0
            Top = 0
            Width = 438
            Height = 19
            Align = alTop
            Caption = 'Feature Output'
            TabOrder = 1
          end
        end
      end
    end
    object TS_Model: TTabSheet
      Caption = 'Build Model'
      ImageIndex = 1
      object newDefList_Splitter: TSplitter
        Left = 808
        Top = 0
        Width = 8
        Height = 392
        AutoSnap = False
        MinSize = 250
        ResizeStyle = rsUpdate
        ExplicitLeft = 529
        ExplicitHeight = 411
      end
      object Sym_LSplitter: TSplitter
        Left = 400
        Top = 0
        Width = 8
        Height = 392
        AutoSnap = False
        MinSize = 250
        ResizeStyle = rsUpdate
        ExplicitLeft = 211
        ExplicitTop = -2
        ExplicitHeight = 296
      end
      object UnitRenamePanel: TPanel
        Left = 816
        Top = 0
        Width = 560
        Height = 392
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          560
          392)
        object ApplyToolPanel: TPanel
          Left = 0
          Top = 0
          Width = 560
          Height = 25
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
        end
        object DoCreateModel_Btn: TButton
          Left = 151
          Top = 77
          Width = 271
          Height = 40
          Caption = 'Do Build Pascal Code rewrite model.'
          TabOrder = 1
          OnClick = DoCreateModel_BtnClick
        end
        object ModelOuytputEdit: TLabeledEdit
          Left = 71
          Top = 37
          Width = 453
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 49
          EditLabel.Height = 13
          EditLabel.Caption = 'Model file:'
          LabelPosition = lpLeft
          TabOrder = 2
        end
        object Browse_ModelOuytput_Btn: TButton
          Left = 530
          Top = 35
          Width = 25
          Height = 25
          Anchors = [akTop, akRight]
          Caption = '..'
          TabOrder = 3
          OnClick = Browse_ModelOuytput_BtnClick
        end
      end
      object unit_L_Panel: TPanel
        Left = 408
        Top = 0
        Width = 400
        Height = 392
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        object Unit_Processor_LB: TListBox
          Left = 0
          Top = 26
          Width = 400
          Height = 366
          Align = alClient
          DoubleBuffered = True
          ItemHeight = 13
          MultiSelect = True
          ParentDoubleBuffered = False
          TabOrder = 0
          OnKeyUp = Unit_Processor_LBKeyUp
        end
        object u_model_t_Panel: TPanel
          Left = 0
          Top = 0
          Width = 400
          Height = 26
          Align = alTop
          Alignment = taLeftJustify
          Caption = 'Unit rewrite'
          TabOrder = 1
          object Button4: TButton
            Left = 89
            Top = 0
            Width = 71
            Height = 25
            Caption = 'Rebuild'
            TabOrder = 0
            OnClick = Rebuild_PreDefine_Click
          end
          object Button1: TButton
            Left = 166
            Top = 0
            Width = 57
            Height = 25
            Caption = 'Swap'
            TabOrder = 1
            OnClick = Swap_Predefine_Click
          end
        end
      end
      object Marco_rep_L_Panel: TPanel
        Left = 0
        Top = 0
        Width = 400
        Height = 392
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 2
        object SymbolReplaceListBox: TListBox
          Left = 0
          Top = 26
          Width = 400
          Height = 366
          Align = alClient
          DoubleBuffered = True
          ItemHeight = 13
          MultiSelect = True
          ParentDoubleBuffered = False
          TabOrder = 0
          OnKeyUp = Unit_Processor_LBKeyUp
        end
        object sym_model_t_Panel: TPanel
          Left = 0
          Top = 0
          Width = 400
          Height = 26
          Align = alTop
          Alignment = taLeftJustify
          Caption = 'Symbol Rewrite'
          TabOrder = 1
          DesignSize = (
            400
            26)
          object rebuild_sym_Button: TButton
            Left = 104
            Top = 0
            Width = 69
            Height = 25
            Caption = 'Rebuild'
            TabOrder = 0
            OnClick = Rebuild_Symbol_Click
          end
          object swap_sym_Button: TButton
            Left = 179
            Top = 0
            Width = 57
            Height = 25
            Caption = 'Swap'
            TabOrder = 1
            OnClick = Swap_Symbol_Click
          end
          object check_model_Button: TButton
            Left = 337
            Top = 0
            Width = 57
            Height = 25
            Anchors = [akTop, akRight]
            Caption = 'Check'
            TabOrder = 2
            OnClick = check_model_ButtonClick
          end
        end
      end
    end
    object TS_Test: TTabSheet
      Caption = 'test'
      ImageIndex = 2
      DesignSize = (
        1376
        392)
      object testMemo: TMemo
        Left = 0
        Top = 0
        Width = 1376
        Height = 392
        Align = alClient
        DoubleBuffered = True
        Lines.Strings = (
          '//Test is easy for debugging developers'
          '//Using rewrite tool environment to build code'
          '//ctrl+v Paste the code and run the test')
        ParentDoubleBuffered = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
      object testButton: TButton
        Left = 1287
        Top = 3
        Width = 65
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Do Test'
        TabOrder = 1
        OnClick = testButtonClick
      end
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 428
    Width = 1384
    Height = 133
    Align = alBottom
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
  object OpenUnitDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'all files'
        FileMask = '*.*'
      end>
    Options = [fdoAllowMultiSelect, fdoFileMustExist]
    Left = 429
    Top = 144
  end
  object UnitDefPopupMenu: TPopupMenu
    Left = 688
    Top = 213
    object NewUnitName1: TMenuItem
      Caption = 'Unit Define'
      OnClick = Unit_Define_Click
    end
    object RemoveUnit1: TMenuItem
      Caption = 'Remove Unit.'
      OnClick = RemoveUnit_Define_Click
    end
    object RefreshUnitlist1: TMenuItem
      Caption = 'Refresh Unit list'
      OnClick = RefreshUnitList_Click
    end
  end
  object fpsTimer: TTimer
    Interval = 100
    OnTimer = fpsTimerTimer
    Left = 428
    Top = 192
  end
  object SaveModelDialog: TSaveDialog
    DefaultExt = '.OX2'
    Filter = 'ZDB2 Files(*.OX2)|*.OX2'
    Left = 425
    Top = 240
  end
  object MainMenu_: TMainMenu
    Left = 691
    Top = 262
    object File1: TMenuItem
      Caption = '&Fille'
      object NewworkspacefromPassByYou888Model_MI: TMenuItem
        Caption = 'New workspace from PassByYou888 Model'
        OnClick = NewworkspacefromPassByYou888Model_MIClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Newworkspace_MI: TMenuItem
        Caption = '&New workspace'
        ShortCut = 16462
        OnClick = Newworkspace_MIClick
      end
      object OpenWorkspace_MI: TMenuItem
        Caption = '&Open workspace'
        ShortCut = 16463
        OnClick = OpenWorkspace_MIClick
      end
      object Saveworkspace_MI: TMenuItem
        Caption = '&Save workspace'
        ShortCut = 16467
        OnClick = Saveworkspace_MIClick
      end
      object SaveworkspaceAsMI: TMenuItem
        Caption = '&Save workspace as...'
        OnClick = SaveworkspaceAsMIClick
      end
      object Closeworkspace_MI: TMenuItem
        Caption = '&Close workspace'
        OnClick = Closeworkspace_MIClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = '&Exit'
        OnClick = Exit1Click
      end
    end
  end
  object SaveWorkspaceDialog: TSaveDialog
    DefaultExt = '.OX2'
    Filter = 'workspace Files(*.OX2)|*.OX2'
    Left = 188
    Top = 337
  end
  object OpenWorkspaceDialog: TOpenDialog
    DefaultExt = '.OX2'
    Filter = 'workspace Files(*.OX2)|*.OX2'
    Left = 187
    Top = 286
  end
end
