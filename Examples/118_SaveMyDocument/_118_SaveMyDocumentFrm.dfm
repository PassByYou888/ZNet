object SaveMyDocumentForm: TSaveMyDocumentForm
  Left = 0
  Top = 0
  Caption = 'Save My Document. create by.qq600585'
  ClientHeight = 527
  ClientWidth = 712
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
  object Label1: TLabel
    Left = 47
    Top = 40
    Width = 56
    Height = 13
    Caption = 'document 1'
  end
  object Label2: TLabel
    Left = 47
    Top = 160
    Width = 56
    Height = 13
    Caption = 'document 2'
  end
  object Label3: TLabel
    Left = 209
    Top = 307
    Width = 404
    Height = 13
    Caption = 'TDFE '#26159#31867#24207#21015#21270#30340#23481#22120#65292#25903#25345#25152#26377#21407#23376#21464#37327#65292#36866#21512#23567#25968#25454#20445#23384#20197#21450#32593#32476#25910#21457'.'
  end
  object Label4: TLabel
    Left = 209
    Top = 338
    Width = 313
    Height = 13
    Caption = 'ZDB2'#30340#23384#20648#26041#24335#38750#24120#36866#21512#22823#25968#25454#65292#20860#39038#21152#23494#65292#24615#33021#65292#22823#35268#27169
  end
  object Memo1: TMemo
    Left = 47
    Top = 59
    Width = 604
    Height = 90
    Lines.Strings = (
      'document 1...')
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 47
    Top = 179
    Width = 604
    Height = 90
    Lines.Strings = (
      'document 2...')
    TabOrder = 1
  end
  object dfe_save_Button: TButton
    Left = 47
    Top = 302
    Width = 75
    Height = 25
    Caption = 'DFE save'
    TabOrder = 2
    OnClick = dfe_save_ButtonClick
  end
  object dfe_load_Button: TButton
    Left = 128
    Top = 302
    Width = 75
    Height = 25
    Caption = 'DFE load'
    TabOrder = 3
    OnClick = dfe_load_ButtonClick
  end
  object LogMemo: TMemo
    Left = 0
    Top = 438
    Width = 712
    Height = 89
    Align = alBottom
    TabOrder = 4
    ExplicitLeft = 248
    ExplicitTop = 421
    ExplicitWidth = 185
  end
  object ZDB2_save_Button: TButton
    Left = 47
    Top = 333
    Width = 75
    Height = 25
    Caption = 'ZDB2 Save'
    TabOrder = 5
    OnClick = ZDB2_save_ButtonClick
  end
  object ZDB2_Load_Button: TButton
    Left = 128
    Top = 333
    Width = 75
    Height = 25
    Caption = 'ZDB2 Load'
    TabOrder = 6
    OnClick = ZDB2_Load_ButtonClick
  end
  object fpsTimer: TTimer
    Interval = 100
    OnTimer = fpsTimerTimer
    Left = 360
    Top = 193
  end
end
