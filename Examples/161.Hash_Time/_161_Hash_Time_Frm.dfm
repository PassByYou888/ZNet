object _161_Hash_Time_Form: T_161_Hash_Time_Form
  Left = 0
  Top = 0
  Caption = #26102#24207#22823#25968#25454'.'
  ClientHeight = 528
  ClientWidth = 1008
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    1008
    528)
  PixelsPerInch = 96
  TextHeight = 13
  object bTime_Edit: TLabeledEdit
    Left = 112
    Top = 32
    Width = 177
    Height = 21
    EditLabel.Width = 52
    EditLabel.Height = 13
    EditLabel.Caption = #36215#22987#26102#38388':'
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object eTime_Edit: TLabeledEdit
    Left = 360
    Top = 32
    Width = 177
    Height = 21
    EditLabel.Width = 52
    EditLabel.Height = 13
    EditLabel.Caption = #32467#26463#26102#38388':'
    LabelPosition = lpLeft
    TabOrder = 1
  end
  object data_Edit: TLabeledEdit
    Left = 112
    Top = 70
    Width = 425
    Height = 21
    EditLabel.Width = 28
    EditLabel.Height = 13
    EditLabel.Caption = #25968#25454':'
    LabelPosition = lpLeft
    TabOrder = 3
  end
  object Memo: TMemo
    Left = 24
    Top = 147
    Width = 961
    Height = 362
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      #26102#24207#22823#25968#25454#22522#20110'Z'#31995#20869#26680#20307#31995#26550#26500#32780#20986#30340#39640#36895#23481#22120
      #24212#29992#26102#24207#22823#25968#25454#38656#35201#32467#21512#25968#25454#24341#25806','#24314#35758#20351#29992'ZDB2'#20307#31995#30340#25968#25454#24341#25806
      #26102#24207#22823#25968#25454#30340#24212#29992#24605#36335':'#25968#25454#24341#25806#21482#29992#20110#23384#20648#25968#25454','#25171#24320#25968#25454#24211#26102','#35753#23427#36733#20837#26102#24207','#28982#21518','#24320#22987#22823#25968#25454#23384#20648#21644#26597#35810
      ''
      #26412'demo'#26159#26102#24207#22823#25968#25454#30340#32467#26500#29615#33410','#19981#21253#21547#23545#25968#25454#24341#25806#30340#20351#29992
      #22312#20869#23384#36275#22815#30340#21069#25552#19979','#26102#24207#22823#25968#25454#35268#27169#21487#36798#21040#19975#20159
      #26412'demo'#30340#25152#26377#22823#25209#37327#25805#20316#37117#26159#32447#31243#23433#20840
      ''
      'by.qq600585'
      '2024-6-22'
      '')
    ScrollBars = ssBoth
    TabOrder = 7
  end
  object Add_Range_Button: TButton
    Left = 112
    Top = 104
    Width = 137
    Height = 25
    Caption = #20197#33539#22260#26102#38388#22686#21152#25968#25454
    TabOrder = 4
    OnClick = Add_Range_ButtonClick
  end
  object Add_bTime_Button: TButton
    Left = 255
    Top = 104
    Width = 137
    Height = 25
    Caption = #20197#36215#22987#26102#38388#22686#21152#25968#25454
    TabOrder = 5
    OnClick = Add_bTime_ButtonClick
  end
  object Query_Button: TButton
    Left = 543
    Top = 30
    Width = 210
    Height = 25
    Caption = #20197#33539#22260#26102#38388#26597#35810#25968#25454' ('#32447#31243#23433#20840')'
    TabOrder = 2
    OnClick = Query_ButtonClick
  end
  object Add_Random_Range_Button: TButton
    Left = 398
    Top = 104
    Width = 335
    Height = 25
    Caption = #20197#38543#26426#33539#22260#26102#38388#22686#21152'100'#19975#26465#25968#25454' ('#32447#31243#23433#20840','#21487#20197#22810#24320')'
    TabOrder = 6
    OnClick = Add_Random_Range_ButtonClick
  end
  object For_Data_Button: TButton
    Left = 739
    Top = 104
    Width = 94
    Height = 25
    Caption = #36941#21382#25968#25454
    TabOrder = 8
    OnClick = For_Data_ButtonClick
  end
  object sysTimer: TTimer
    Interval = 10
    OnTimer = sysTimerTimer
    Left = 500
    Top = 268
  end
end
