object DoubleTunnelClientForm: TDoubleTunnelClientForm
  Left = 0
  Top = 0
  Caption = 'Double Tunnel Client'
  ClientHeight = 384
  ClientWidth = 634
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
  object Memo1: TMemo
    Left = 136
    Top = 38
    Width = 457
    Height = 241
    Lines.Strings = (
      'Zserver4d is a server middleware'
      'This demo is in bidirectional mode'
      ''
      'Bidirectional mode interpretation'
      'Both the client can actively send commands to the server for execution'
      'It is also possible for the server to initiate command execution from the client proactively'
      'Two channels are used in the linking mechanism, one for receiving and one for sending'
      'When both channels are successfully connected, the TunnelLink method can be used to complete bidirectional bridging'
      ''
      'command'
      'The command system can be a simple string and various data packaging, or it can be a Stream packaging'
      'In short, any data transmission and reception below 500k, including small files, can be done using commands'
      ''
      'When the data length is very large, the bigstream mechanism must be used in zsserver4d to send and receive data'
      ''
      'The attached client can be a mobile platform or a personal computer platform')
    TabOrder = 0
  end
  object ConnectButton: TButton
    Left = 32
    Top = 38
    Width = 89
    Height = 35
    Caption = 'connect'
    TabOrder = 1
    OnClick = ConnectButtonClick
  end
  object HostEdit: TLabeledEdit
    Left = 136
    Top = 8
    Width = 121
    Height = 21
    EditLabel.Width = 65
    EditLabel.Height = 13
    EditLabel.Caption = 'host address '
    LabelPosition = lpLeft
    TabOrder = 2
    Text = '127.0.0.1'
  end
  object HelloWorldBtn: TButton
    Left = 32
    Top = 143
    Width = 89
    Height = 34
    Caption = 'hello world'
    TabOrder = 3
    OnClick = HelloWorldBtnClick
  end
  object AsyncConnectButton: TButton
    Left = 32
    Top = 79
    Width = 89
    Height = 35
    Caption = 'async connect'
    TabOrder = 4
    OnClick = AsyncConnectButtonClick
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 344
    Top = 16
  end
end
