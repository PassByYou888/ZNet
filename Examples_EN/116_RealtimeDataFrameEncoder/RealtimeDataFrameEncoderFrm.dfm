object RealtimeDataFrameEncoderForm: TRealtimeDataFrameEncoderForm
  Left = 0
  Top = 0
  Caption = 'Realtime DataFrame Encoder.'
  ClientHeight = 245
  ClientWidth = 733
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object EncStateLabel: TLabel
    Left = 168
    Top = 181
    Width = 29
    Height = 13
    Caption = 'state:'
  end
  object DecStateLabel: TLabel
    Left = 168
    Top = 212
    Width = 29
    Height = 13
    Caption = 'state:'
  end
  object Memo1: TMemo
    Left = 35
    Top = 22
    Width = 662
    Height = 107
    Lines.Strings = (
      'DFE can convert program variables into storable data, similar to serialization/deserialization, DataFrame supports all atomic variables'
      'This demo demonstrates the theoretical throughput of DFE, which directly affects file storage and network data transmission and reception'
      
        'DFE supports parallel encoding and decoding technology. If thread efficiency is not high, external MM can be mounted, https://github.com/PassByYou888' +
        '/jemalloc4p'
      'The throughput of DFE depends on the North Bridge frequency+Physical Memory frequency+CPU frequency, and the server will be much higher than the PC'
      ''
      'by.qq600585')
    TabOrder = 0
  end
  object enPerfButton: TButton
    Left = 35
    Top = 176
    Width = 127
    Height = 25
    Caption = 'Encoder Performance'
    TabOrder = 1
    OnClick = enPerfButtonClick
  end
  object dePerfButton: TButton
    Left = 35
    Top = 207
    Width = 127
    Height = 25
    Caption = 'Decoder Performance'
    TabOrder = 2
    OnClick = dePerfButtonClick
  end
  object ThNumEdit: TLabeledEdit
    Left = 80
    Top = 141
    Width = 42
    Height = 21
    EditLabel.Width = 38
    EditLabel.Height = 13
    EditLabel.Caption = 'Thread:'
    LabelPosition = lpLeft
    TabOrder = 3
    Text = '10'
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
