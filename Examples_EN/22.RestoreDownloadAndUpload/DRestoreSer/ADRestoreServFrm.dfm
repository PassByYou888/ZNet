object AuthDoubleServerForm: TAuthDoubleServerForm
  Left = 0
  Top = 0
  Caption = 'Auth Double Tunnel Server'
  ClientHeight = 456
  ClientWidth = 634
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    634
    456)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 144
    Top = 8
    Width = 473
    Height = 425
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Zserver4d is a server middleware'
      'This demo is a login based bidirectional mode'
      ''
      'Login Explanation'
      'Authentication is required when initiating a connection'
      'Because there is a user identity when logging in, each user has their own file storage and data in the logged in server system'
      #25454
      'storage space '
      ''
      'Bidirectional mode interpretation'
      'Both the client can actively send commands to the server for execution'
      'It is also possible for the server to initiate command execution from the client proactively'
      'Two channels are used in the linking mechanism, one for receiving and one for sending'
      'When both channels are successfully connected, the TunnelLink method can be used to complete bidirectional bridging'
      ''
      'The login bidirectional communication mode adds the following functions compared to the simple bidirectional working mode'
      'Asynchronous login (client)'
      'Large file transfer support (server, client)'
      'Stacked Instruction Storage (Server)'
      'Storage space and data management (server)'
      'Preemptive login (server)'
      ''
      'command'
      'The command system can be a simple string and various data packaging, or it can be a Stream packaging'
      'In short, any data transmission and reception below 500k, including small files, can be done using commands'
      ''
      'When the data length is very large, the bigstream mechanism must be used in zsserver4d to send and receive data'
      ''
      'The attached client can be a mobile platform or a personal computer platform'
      '')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object StartServiceButton: TButton
    Left = 32
    Top = 8
    Width = 89
    Height = 35
    Caption = 'start service'
    TabOrder = 1
    OnClick = StartServiceButtonClick
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 360
    Top = 8
  end
end
