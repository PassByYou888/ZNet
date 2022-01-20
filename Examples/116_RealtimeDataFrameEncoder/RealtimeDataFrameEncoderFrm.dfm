object RealtimeDataFrameEncoderForm: TRealtimeDataFrameEncoderForm
  Left = 0
  Top = 0
  Caption = 'Realtime DataFrame Encoder.'
  ClientHeight = 222
  ClientWidth = 851
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object EncStateLabel: TLabel
    Left = 168
    Top = 157
    Width = 29
    Height = 13
    Caption = 'state:'
  end
  object DecStateLabel: TLabel
    Left = 168
    Top = 188
    Width = 29
    Height = 13
    Caption = 'state:'
  end
  object Memo1: TMemo
    Left = 35
    Top = 22
    Width = 777
    Height = 114
    Lines.Strings = (
      'DataFrame'#21487#20197#23558#31243#24207#21464#37327#36716#23384#25104#21487#23384#20648#25968#25454#65292#19982#24207#21015#21270'/'#21453#24207#21015#21270#31867#20284#65292'DataFrame'#25903#25345#25152#26377#30340#21407#23376#21464#37327
      ''
      #26412'demo'#28436#31034#20102'DataFrame'#30340#29702#35770#21534#21520#37327#65292#36825#19968#25351#26631#20250#30452#25509#24433#21709#25991#20214#23384#20648#65292#32593#32476#25968#25454#25910#21457
      ''
      'by.qq600585'
      '')
    TabOrder = 0
  end
  object enPerfButton: TButton
    Left = 35
    Top = 152
    Width = 127
    Height = 25
    Caption = 'Encoder Performance'
    TabOrder = 1
    OnClick = enPerfButtonClick
  end
  object dePerfButton: TButton
    Left = 35
    Top = 183
    Width = 127
    Height = 25
    Caption = 'Decoder Performance'
    TabOrder = 2
    OnClick = dePerfButtonClick
  end
  object fpsTimer: TTimer
    Left = 419
    Top = 143
  end
  object perfTimer: TTimer
    OnTimer = perfTimerTimer
    Left = 497
    Top = 142
  end
end
