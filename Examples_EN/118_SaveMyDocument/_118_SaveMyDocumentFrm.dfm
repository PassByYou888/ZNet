object SaveMyDocumentForm: TSaveMyDocumentForm
  Left = 0
  Top = 0
  Caption = 'Save My Document. create by.qq600585'
  ClientHeight = 452
  ClientWidth = 889
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 56
    Height = 13
    Caption = 'document 1'
  end
  object Label2: TLabel
    Left = 8
    Top = 128
    Width = 56
    Height = 13
    Caption = 'document 2'
  end
  object Label3: TLabel
    Left = 170
    Top = 275
    Width = 667
    Height = 13
    Caption = 
      'TDFE is a container for class serialization, supporting all atom' +
      'ic variables, suitable for small data storage and network sendin' +
      'g and receiving'
  end
  object Label4: TLabel
    Left = 170
    Top = 306
    Width = 545
    Height = 13
    Caption = 
      'ZDB2'#39's storage method is very suitable for big data, taking into' +
      ' account encryption, performance, and large-scale'
  end
  object Memo1: TMemo
    Left = 8
    Top = 27
    Width = 810
    Height = 90
    Lines.Strings = (
      'document 1...')
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 8
    Top = 147
    Width = 810
    Height = 90
    Lines.Strings = (
      'document 2...')
    TabOrder = 1
  end
  object dfe_save_Button: TButton
    Left = 8
    Top = 270
    Width = 75
    Height = 25
    Caption = 'DFE save'
    TabOrder = 2
    OnClick = dfe_save_ButtonClick
  end
  object dfe_load_Button: TButton
    Left = 89
    Top = 270
    Width = 75
    Height = 25
    Caption = 'DFE load'
    TabOrder = 3
    OnClick = dfe_load_ButtonClick
  end
  object LogMemo: TMemo
    Left = 0
    Top = 363
    Width = 889
    Height = 89
    Align = alBottom
    TabOrder = 4
    ExplicitTop = 438
    ExplicitWidth = 712
  end
  object ZDB2_save_Button: TButton
    Left = 8
    Top = 301
    Width = 75
    Height = 25
    Caption = 'ZDB2 Save'
    TabOrder = 5
    OnClick = ZDB2_save_ButtonClick
  end
  object ZDB2_Load_Button: TButton
    Left = 89
    Top = 301
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
