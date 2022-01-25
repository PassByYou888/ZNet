object C4_VAR_Tech_Demo_Serv_Form: TC4_VAR_Tech_Demo_Serv_Form
  Left = 0
  Top = 0
  Caption = 'C4_VAR_Tech_Demo_Serv_Form'
  ClientHeight = 471
  ClientWidth = 1183
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
  object topPanel: TPanel
    Left = 0
    Top = 0
    Width = 1183
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
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
    object ServiceTypeEdit: TLabeledEdit
      Left = 247
      Top = 24
      Width = 402
      Height = 21
      EditLabel.Width = 65
      EditLabel.Height = 13
      EditLabel.Caption = 'Service types'
      TabOrder = 2
      Text = 'var'
      TextHint = 'dp|na|dna|va|dva|d|dd|fs|var|userdb'
    end
    object buildNetworkButton: TButton
      Left = 655
      Top = 22
      Width = 98
      Height = 25
      Caption = 'Build Service.'
      TabOrder = 3
      OnClick = buildNetworkButtonClick
    end
  end
  object cliPanel: TPanel
    Left = 0
    Top = 49
    Width = 1183
    Height = 422
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object LSplitter: TSplitter
      Left = 307
      Top = 0
      Height = 422
      AutoSnap = False
      ExplicitLeft = 296
      ExplicitTop = 112
      ExplicitHeight = 100
    end
    object LPanel: TPanel
      Left = 0
      Top = 0
      Width = 307
      Height = 422
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object TreeView: TTreeView
        Left = 0
        Top = 0
        Width = 307
        Height = 422
        Align = alClient
        Indent = 19
        TabOrder = 0
      end
    end
    object RPanel: TPanel
      Left = 310
      Top = 0
      Width = 873
      Height = 422
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Memo: TMemo
        Left = 0
        Top = 0
        Width = 873
        Height = 422
        Align = alClient
        Lines.Strings = (
          'Var'#26381#21153#26159'VM'#36816#34892#25903#25345#30340#26680#24515#31995#32479
          'Var'#26381#21153#21482#33021#36890#36807'build'#26041#24335#21019#24314
          'Var'#26381#21153#20855#26377#23433#20840#25968#25454#35835#20889#33021#21147','#22312#21019#24314#20197#21518#23558#20250#22797#21407#20197#21069#21464#37327#29366#24577
          'Var'#26381#21153#27599#38548#19968#27573#26102#38388#37117#20250#36827#34892#29366#24577#20445#23384
          ''
          'by.qq600585')
        ScrollBars = ssVertical
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 776
    Top = 16
  end
  object UpdateStateTimer: TTimer
    OnTimer = UpdateStateTimerTimer
    Left = 164
    Top = 121
  end
end
