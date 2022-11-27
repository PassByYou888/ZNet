object _129_ZDB2_Thread_App_DemoForm: T_129_ZDB2_Thread_App_DemoForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'ZDB2 Thread App Demo, create by.qq600585'
  ClientHeight = 501
  ClientWidth = 565
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Data_Num_Label: TLabel
    Left = 8
    Top = 52
    Width = 36
    Height = 13
    Caption = '.........'
  end
  object Remove_Num_Label: TLabel
    Left = 8
    Top = 71
    Width = 36
    Height = 13
    Caption = '.........'
  end
  object Add_Num_Label: TLabel
    Left = 8
    Top = 90
    Width = 36
    Height = 13
    Caption = '.........'
  end
  object Parallel_Load_Num_Label: TLabel
    Left = 8
    Top = 109
    Width = 36
    Height = 13
    Caption = '.........'
  end
  object For_Num_Label: TLabel
    Left = 8
    Top = 128
    Width = 36
    Height = 13
    Caption = '.........'
  end
  object Progress_Num_Label: TLabel
    Left = 8
    Top = 147
    Width = 36
    Height = 13
    Caption = '.........'
  end
  object Queue_Num_Label: TLabel
    Left = 8
    Top = 166
    Width = 36
    Height = 13
    Caption = '.........'
  end
  object Memo: TMemo
    Left = 0
    Top = 320
    Width = 565
    Height = 181
    Align = alBottom
    Lines.Strings = (
      'ZDB2'#26159#20808#36827#22823#25968#25454#24341#25806#65292#30452#25509#20174#24213#23618#25903#25345#22823#25968#25454'+'#22810#32447#31243#30340#22686#21024#26597#25913
      ''
      #27880#24847#65306#40664#35748'mm'#19981#30693#27492#39640#24182#21457#65292#24517#39035#20351#29992#25351#23450#30340'MM'#24211#65292'jemalloc4p or tcmalloc4p'
      ''
      #21517#35789#35299#37322
      #29289#29702#36941#21382#65306#24182#34892#21270#26041#24335#20174#29289#29702'IO'#20840#24211#35835#21462#65292#36890#24120#29992#20110#21551#21160#25968#25454#24211#30340#36733#20837#65292#19981#25903#25345#20013#26029
      #23454#20363#36941#21382#65306#24182#34892#21270#26041#24335#20174#21040#23614#24490#29615#23454#20363#65292#19981#31561#29289#29702'IO'#65292#21644'for'#30456#36817#65292#25903#25345#20013#26029
      #20027#24490#29615#65306#21482#24037#20316#19982#22823#24490#29615#27969#31243#31354#38386#19979#65292#36941#21382#27969#31243#27169#22411#20026#38750#24037#20316#29366#24577
      #23454#20363#22238#25910#65306#21024#38500#25968#25454#26102#23454#20363#24182#19981#20250#37322#25918#65292#32780#26159#25918#21040#22238#25910#31449
      #25968#25454#38142#22238#25910#65306#21024#38500#25968#25454#26102#25968#25454#30340#19978#19979#20381#36182#39033#19981#20250#21024#38500#65292#32780#26159#25918#21040#25968#25454#38142#22238#25910#31449
      ''
      'by.qq600585')
    ScrollBars = ssVertical
    TabOrder = 4
    WordWrap = False
  end
  object runButton: TButton
    Left = 8
    Top = 8
    Width = 97
    Height = 25
    Caption = #25191#34892'/'#20572#27490
    TabOrder = 0
    OnClick = runButtonClick
  end
  object Parallel_Load_CheckBox: TCheckBox
    Left = 111
    Top = 12
    Width = 66
    Height = 17
    Caption = #29289#29702#36941#21382
    TabOrder = 1
  end
  object For_CheckBox: TCheckBox
    Left = 183
    Top = 12
    Width = 66
    Height = 17
    Caption = #23454#20363#36941#21382
    TabOrder = 2
  end
  object Progress_CheckBox: TCheckBox
    Left = 255
    Top = 12
    Width = 58
    Height = 17
    Caption = #20027#24490#29615
    TabOrder = 3
  end
  object fpsTimer: TTimer
    Interval = 100
    OnTimer = fpsTimerTimer
    Left = 312
    Top = 136
  end
end
