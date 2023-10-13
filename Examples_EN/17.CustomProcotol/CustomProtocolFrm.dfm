object CustomProtocolForm: TCustomProtocolForm
  Left = 0
  Top = 0
  Caption = 'Custom Protocol support...'
  ClientHeight = 412
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 57
    Width = 852
    Height = 355
    Align = alClient
    BorderStyle = bsNone
    Lines.Strings = (
      'Standard server paradigm for creating external customized protocols'
      'We can develop our own FTP, HTTP, and other protocols based on this paradigm'
      'Develop your own external protocol based on ZS, which can naturally support high concurrency and cloud backend'
      ''
      'Develop external protocols based on ZS, mainly compatible with other third-party communication terminals'
      'Therefore, the client needs to solve the problem themselves, such as selecting Indy as the communication client'
      ''
      'by.qq 600585'
      '2018-1-24')
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 852
    Height = 57
    Align = alTop
    BorderStyle = bsSingle
    TabOrder = 1
    object connectOnIndyButton: TButton
      Left = 16
      Top = 15
      Width = 105
      Height = 25
      Caption = 'connect on indy'
      TabOrder = 0
      OnClick = connectOnIndyButtonClick
    end
    object SendDataOnIndyButton: TButton
      Left = 127
      Top = 15
      Width = 122
      Height = 25
      Caption = 'send data on indy'
      TabOrder = 1
      OnClick = SendDataOnIndyButtonClick
    end
    object connectOnZServerButton: TButton
      Left = 336
      Top = 15
      Width = 121
      Height = 25
      Caption = 'connect on ZServer'
      TabOrder = 2
      OnClick = connectOnZServerButtonClick
    end
    object SendDataOnZServerButton: TButton
      Left = 463
      Top = 15
      Width = 138
      Height = 25
      Caption = 'send data on ZServer'
      TabOrder = 3
      OnClick = SendDataOnZServerButtonClick
    end
  end
  object Timer: TTimer
    Interval = 100
    OnTimer = TimerTimer
    Left = 296
    Top = 168
  end
  object IdTCPClient1: TIdTCPClient
    ConnectTimeout = 0
    Host = '127.0.0.1'
    Port = 9989
    ReadTimeout = -1
    Left = 424
    Top = 208
  end
end
