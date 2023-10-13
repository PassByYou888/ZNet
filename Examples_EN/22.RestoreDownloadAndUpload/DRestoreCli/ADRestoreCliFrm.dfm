object AuthDoubleTunnelClientForm: TAuthDoubleTunnelClientForm
  Left = 0
  Top = 0
  Caption = 'Auth Double Tunnel Client'
  ClientHeight = 383
  ClientWidth = 630
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    630
    383)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 119
    Top = 62
    Width = 490
    Height = 306
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
      'The attached client can be a mobile platform or a personal computer platform')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object ConnectButton: TButton
    Left = 8
    Top = 94
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
  object BlockBatchOperationBtn: TButton
    Left = 8
    Top = 207
    Width = 105
    Height = 34
    Caption = 'Batch and Block'
    TabOrder = 3
    OnClick = BlockBatchOperationBtnClick
  end
  object UserEdit: TLabeledEdit
    Left = 136
    Top = 35
    Width = 97
    Height = 21
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'User'
    LabelPosition = lpLeft
    TabOrder = 4
    Text = 'test'
  end
  object PasswdEdit: TLabeledEdit
    Left = 288
    Top = 35
    Width = 97
    Height = 21
    EditLabel.Width = 36
    EditLabel.Height = 13
    EditLabel.Caption = 'Passwd'
    LabelPosition = lpLeft
    TabOrder = 5
    Text = '123456'
  end
  object RegUserButton: TButton
    Left = 8
    Top = 53
    Width = 89
    Height = 35
    Caption = 'Reg user'
    TabOrder = 6
    OnClick = RegUserButtonClick
  end
  object AsyncConnectButton: TButton
    Left = 8
    Top = 135
    Width = 89
    Height = 35
    Caption = 'async connect'
    TabOrder = 7
    OnClick = AsyncConnectButtonClick
  end
  object RestoreDownloadButton: TButton
    Left = 8
    Top = 247
    Width = 105
    Height = 34
    Caption = 'Restore download'
    TabOrder = 8
    OnClick = RestoreDownloadButtonClick
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 344
    Top = 48
  end
end
