object C4_VAR_Tech_Demo_Cli_Form: TC4_VAR_Tech_Demo_Cli_Form
  Left = 0
  Top = 0
  Caption = 'C4_VAR_Tech_Demo_Cli_Form'
  ClientHeight = 550
  ClientWidth = 1360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object botSplitter: TSplitter
    Left = 0
    Top = 408
    Width = 1360
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitLeft = -183
    ExplicitTop = 329
    ExplicitWidth = 1183
  end
  object topPanel: TPanel
    Left = 0
    Top = 0
    Width = 1360
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 1335
    object AddrEdit: TLabeledEdit
      Left = 24
      Top = 24
      Width = 161
      Height = 21
      EditLabel.Width = 84
      EditLabel.Height = 13
      EditLabel.Caption = 'Listening Address'
      TabOrder = 0
      Text = '127.0.0.1'
    end
    object PortEdit: TLabeledEdit
      Left = 191
      Top = 24
      Width = 50
      Height = 21
      EditLabel.Width = 20
      EditLabel.Height = 13
      EditLabel.Caption = 'Port'
      TabOrder = 1
      Text = '1296'
    end
    object dependEdit: TLabeledEdit
      Left = 247
      Top = 24
      Width = 402
      Height = 21
      EditLabel.Width = 74
      EditLabel.Height = 13
      EditLabel.Caption = 'depend Service'
      TabOrder = 2
      Text = 'var'
    end
    object GoNetworkButton: TButton
      Left = 655
      Top = 22
      Width = 98
      Height = 25
      Caption = 'Go Network.'
      TabOrder = 3
      OnClick = GoNetworkButtonClick
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 411
    Width = 1360
    Height = 139
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
    ExplicitWidth = 1335
  end
  object cliPanel: TPanel
    Left = 0
    Top = 49
    Width = 1360
    Height = 359
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 1335
    object LSplitter: TSplitter
      Left = 249
      Top = 0
      Height = 359
      AutoSnap = False
      ExplicitLeft = 296
      ExplicitTop = 112
      ExplicitHeight = 100
    end
    object LPanel: TPanel
      Left = 0
      Top = 0
      Width = 249
      Height = 359
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object TreeView: TTreeView
        Left = 0
        Top = 0
        Width = 249
        Height = 359
        Align = alClient
        Indent = 19
        TabOrder = 0
      end
    end
    object RPanel: TPanel
      Left = 252
      Top = 0
      Width = 1108
      Height = 359
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 1083
      object ScriptMemo: TMemo
        Left = 0
        Top = 34
        Width = 1108
        Height = 325
        Align = alClient
        Lines.Strings = (
          '//var'#26381#21153#33050#26412#21487#26412#22320#25191#34892','#28982#21518#20877#21516#27493#21040#26381#21153#22120
          '//var'#20134#21487#21457#36865#32473#36828#31243#25191#34892','#26381#21153#22120#20250#21516#27493#21040#26412#22320
          '//'#19979#21015#33050#26412#22343#22312#20004#31471#25191#34892
          '//'#21019#24314#21464#37327': set('#21464#37327#21517', '#20540')'
          '//'#21462#24471#21464#37327': '#21464#37327#21517'(),'#21464#37327#21517'+'#21464#37327#21517
          '//'#36171#20540#21464#37327': '#21464#37327#21517'('#20540')'
          '//'#40664#35748#33050#26412#35821#27861#20026'pascal'#39118#26684','#20134#21487#20351#29992'c'#39118#26684','#33050#26412#30001'zExpression'#24341#25806#39537#21160
          '//var'#38656#35201'open'#20197#21518#25165#26377#30417#21548#25928#26524
          '// by.qq600585'
          ''
          'Set('#39'A'#39', Float(1.0))'
          'Set('#39'B'#39', Int(2))'
          'Set('#39'C'#39', Int64(3))'
          'Set('#39'D'#39', Word(4))'
          'Set('#39'E'#39', '#39'hello world,'#39')'
          '')
        TabOrder = 0
        ExplicitWidth = 1083
      end
      object LTPanel: TPanel
        Left = 0
        Top = 0
        Width = 1108
        Height = 34
        Align = alTop
        BevelKind = bkSoft
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitWidth = 1083
        object InitLocalNMFromScriptButton: TButton
          Left = 110
          Top = 4
          Width = 155
          Height = 21
          Caption = '1.0 - Init Local NM From Script'
          TabOrder = 0
          OnClick = InitLocalNMFromScriptButtonClick
        end
        object InitLocalNMButton: TButton
          Left = 271
          Top = 4
          Width = 101
          Height = 21
          Caption = '1.1 - Init Local NM'
          TabOrder = 1
          OnClick = InitLocalNMButtonClick
        end
        object NMEdit: TLabeledEdit
          Left = 31
          Top = 4
          Width = 73
          Height = 21
          EditLabel.Width = 19
          EditLabel.Height = 13
          EditLabel.Caption = 'NM:'
          LabelPosition = lpLeft
          TabOrder = 2
          Text = 'test'
        end
        object SyncNMToRemoteButton: TButton
          Left = 378
          Top = 5
          Width = 111
          Height = 21
          Caption = '2.0 - Sync to Remote'
          TabOrder = 3
          OnClick = SyncNMToRemoteButtonClick
        end
        object removeNMButton: TButton
          Left = 813
          Top = 5
          Width = 89
          Height = 21
          Caption = '4.0 - Remove'
          TabOrder = 4
          OnClick = removeNMButtonClick
        end
        object SyncAsTempForRemoteButton: TButton
          Left = 495
          Top = 5
          Width = 152
          Height = 21
          Caption = '2.1 - sync to remote temp'
          TabOrder = 5
          OnClick = SyncAsTempForRemoteButtonClick
        end
        object runScriptFromRemoteButton: TButton
          Left = 908
          Top = 5
          Width = 157
          Height = 21
          Caption = '5.0 - Run Script From Remote'
          TabOrder = 6
          OnClick = runScriptFromRemoteButtonClick
        end
        object openNMButton: TButton
          Left = 653
          Top = 5
          Width = 76
          Height = 21
          Caption = '3.0 - open'
          TabOrder = 7
          OnClick = openNMButtonClick
        end
        object closeNMButton: TButton
          Left = 735
          Top = 5
          Width = 76
          Height = 21
          Caption = '3.1 - close'
          TabOrder = 8
          OnClick = closeNMButtonClick
        end
      end
    end
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 784
    Top = 8
  end
  object UpdateStateTimer: TTimer
    OnTimer = UpdateStateTimerTimer
    Left = 780
    Top = 97
  end
end
