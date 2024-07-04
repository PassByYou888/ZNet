object _163_NonLinearExpressionStack_Demo_Form: T_163_NonLinearExpressionStack_Demo_Form
  Left = 0
  Top = 0
  Caption = #38750#32447#24615#34920#36798#24335#30340#22534#26632#24212#29992#27169#22411
  ClientHeight = 373
  ClientWidth = 746
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Consolas'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Memo: TMemo
    Left = 0
    Top = 124
    Width = 746
    Height = 249
    Align = alBottom
    Lines.Strings = (
      #38750#32447#24615#34920#36798#24335#36890#36807#27169#25311#29289#29702#22534#26632','#20174#32780#32473#20986#20102#38750#32447#24615#27969#31243#30340#35299#20915#21150#27861'.'
      'TOpCode_NonLinear'#19981#20809#25903#25345#27169#25311#22534#26632','#36824#25552#20379#20102#32447#31243#23433#20840#26426#21046','#21487#20197#39640#24378#24230#26041#24335#38271#26102#38388#24037#20316#20110#26381#21153#22120#24179#21488'.'
      ''
      'by.qq600585'
      '2024-7'
      '')
    ScrollBars = ssBoth
    TabOrder = 3
    WordWrap = False
  end
  object expEdit: TLabeledEdit
    Left = 24
    Top = 40
    Width = 673
    Height = 23
    EditLabel.Width = 45
    EditLabel.Height = 15
    EditLabel.Caption = #34920#36798#24335
    TabOrder = 0
    Text = #39#23567#26126#30340#21517#23383#26159' '#39'+'#23567#26126'()+'#39', '#23567#26126#26159' '#39'+'#23567#26126#29240#29240'()+'#39' '#21644' '#39'+'#23567#26126#22920#22920'()+'#39' '#30340#20799#23376'.'#39
  end
  object runButton: TButton
    Left = 24
    Top = 72
    Width = 75
    Height = 25
    Caption = #25191#34892
    TabOrder = 1
    OnClick = runButtonClick
  end
  object runInThreadButton: TButton
    Left = 105
    Top = 72
    Width = 140
    Height = 25
    Caption = #20197#20223#30495#32447#31243#25191#34892
    TabOrder = 2
    OnClick = runInThreadButtonClick
  end
  object sysTimer: TTimer
    Interval = 10
    OnTimer = sysTimerTimer
    Left = 64
    Top = 116
  end
end
