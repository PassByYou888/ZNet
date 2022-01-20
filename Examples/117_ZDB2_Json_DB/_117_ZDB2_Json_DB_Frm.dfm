object TZDB2_Json_DB_Frm: TTZDB2_Json_DB_Frm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  BorderWidth = 10
  Caption = 'Json DB demo. Create by.qq600585'
  ClientHeight = 471
  ClientWidth = 842
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object MakeInfoLabel: TLabel
    Left = 420
    Top = 32
    Width = 8
    Height = 13
    Caption = '..'
  end
  object thInfoLabel: TLabel
    Left = 211
    Top = 99
    Width = 8
    Height = 13
    Caption = '..'
  end
  object MakeJsonButton: TButton
    Left = 24
    Top = 27
    Width = 378
    Height = 25
    Caption = #29983#25104'Json '#24635#20849'1'#19975#26465#27599#26465'500'#20010#34920#36798#24335#65292#24635#20849#29983#25104'500'#19975#34920#36798#24335
    TabOrder = 0
    OnClick = MakeJsonButtonClick
  end
  object Memo: TMemo
    Left = 0
    Top = 308
    Width = 842
    Height = 163
    Align = alBottom
    Lines.Strings = (
      'ZDB2'#30340'json'#25968#25454#24211#25805#20316'demo'#65292
      #35813'demo'#21253#21547
      #20667#29916#26597#35810#26041#27861#65292#23601#26159#29992'for'#65292#19968#20010#24490#29615#23601#26597#23436#20102
      
        #25968#25454#22788#29702#27969#27700#32447'IO'#38431#21015#25216#26415#65292#20351#29992#39640#25928#29575#22810#26680#30828#20214'('#32447#31243#25903#25345#20307#31995#65292#30446#21069#26159'pascal'#22280#30340'no.1'#65292#24615#33021#21644#32534#31243#27169#22411#65292#20860#23481'fpc/del' +
        'phi)'
      #38024#23545#25968#25454#22686#21024#26597#25913#30340#27169#22411#22522#26412#30340#20351#29992#33539#24335#65292#37117#26159#29992'for'#65292'save'#65292'remove'#26041#27861
      #30452#35266#24863#21463'ZDB2'#30340'json'#25968#25454#24211#24615#33021#65292'ZDB2'#25903#25345'10'#26469#31181#25968#25454#32467#26500#65292#33258#34892#30740#31350#21543
      ''
      #21457#29983#39039#21345#26159'ZDB2'#20869#26680#27491#22312#36827#34892': flush cache,'#21021#22987','#25193#23481','#22238#25910#20869#23384#65292#36825#20123#37117#22312'ZDB2'#20869#26680#36827#34892
      ''
      '2022-1-3'
      'by.qq600585'
      '')
    TabOrder = 1
    WordWrap = False
  end
  object newbieQueryButton: TButton
    Left = 24
    Top = 63
    Width = 181
    Height = 25
    Caption = #31616#26131#26597#35810
    TabOrder = 2
    OnClick = newbieQueryButtonClick
  end
  object expertQueryButton: TButton
    Left = 24
    Top = 94
    Width = 181
    Height = 25
    Caption = #19987#19994#26597#35810
    TabOrder = 3
    OnClick = expertQueryButtonClick
  end
  object removeButton: TButton
    Left = 24
    Top = 125
    Width = 181
    Height = 25
    Caption = #21024#38500
    TabOrder = 4
    OnClick = removeButtonClick
  end
  object ProgressBar: TProgressBar
    Left = 24
    Top = 268
    Width = 801
    Height = 17
    TabOrder = 5
  end
  object modifyButton: TButton
    Left = 24
    Top = 156
    Width = 181
    Height = 25
    Caption = #26356#25913
    TabOrder = 6
    OnClick = modifyButtonClick
  end
  object extractSpaceButton: TButton
    Left = 24
    Top = 205
    Width = 181
    Height = 25
    Caption = #25972#29702#31354#38388#30862#29255
    TabOrder = 7
    OnClick = extractSpaceButtonClick
  end
  object fpsTimer: TTimer
    Interval = 100
    OnTimer = fpsTimerTimer
    Left = 394
    Top = 144
  end
end
