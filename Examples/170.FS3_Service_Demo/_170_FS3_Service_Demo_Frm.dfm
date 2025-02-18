object _170_FS3_Service_Demo_Form: T_170_FS3_Service_Demo_Form
  Left = 0
  Top = 0
  BorderWidth = 5
  Caption = '_170_FS3_Service_Demo_Form'
  ClientHeight = 401
  ClientWidth = 842
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object bot_Splitter: TSplitter
    Left = 0
    Top = 298
    Width = 842
    Height = 8
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    ExplicitTop = 303
  end
  object Log_Memo: TMemo
    Left = 0
    Top = 306
    Width = 842
    Height = 95
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object LV: TListView
    Left = 0
    Top = 41
    Width = 842
    Height = 257
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = #25991#20214#21517
      end
      item
        Caption = #22823#23567
        Width = 100
      end
      item
        Caption = #26102#38388
        Width = 140
      end
      item
        Caption = #29983#23384#21608#26399
        Width = 80
      end
      item
        Caption = #24207#21015'ID'
      end>
    HideSelection = False
    MultiSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object Tool_Panel: TPanel
    Left = 0
    Top = 0
    Width = 842
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Upload_Button: TButton
      Left = 8
      Top = 9
      Width = 97
      Height = 25
      Caption = #19978#20256#25991#20214
      TabOrder = 0
      OnClick = Upload_ButtonClick
    end
    object Download_Button: TButton
      Left = 108
      Top = 9
      Width = 97
      Height = 25
      Caption = #19979#36733#25991#20214
      TabOrder = 1
      OnClick = Download_ButtonClick
    end
    object Remove_Button: TButton
      Left = 208
      Top = 9
      Width = 97
      Height = 25
      Caption = #21024#38500#25991#20214
      TabOrder = 2
      OnClick = Remove_ButtonClick
    end
    object Refresh_Button: TButton
      Left = 308
      Top = 9
      Width = 97
      Height = 25
      Caption = #21047#26032#21015#34920
      TabOrder = 3
      OnClick = Refresh_ButtonClick
    end
  end
  object SysTimer: TTimer
    Interval = 10
    OnTimer = SysTimerTimer
    Left = 420
    Top = 212
  end
  object OpenDialog: TOpenDialog
    Filter = 'All Files(*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 520
    Top = 216
  end
  object SaveDialog: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 600
    Top = 216
  end
end
