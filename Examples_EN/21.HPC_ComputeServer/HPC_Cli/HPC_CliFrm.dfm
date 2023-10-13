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
    Height = 307
    Lines.Strings = (
      'HPC background computing server demonstration'
      ''
      'The backend computation occupies zero server resource time'
      ''
      'HPC background demonstration is very suitable for large-scale computing needs'
      ''
      'The technical mechanism mainly focuses on large-scale computing on the server side'
      'When the server receives a StreamCMD mode command, the traditional processing method is to process the command in which the server is'
      'Blocked, all other requests will be waiting'
      'We don'#39't let the server block at this moment, we use delay technology to tell the backend to pause feedback'
      '(PauseResultSend)'
      'Then, we start a background thread and let the background continue to feed back after the thread execution is completed'
      '(ContinueResultSend)'
      ''
      'In the HPC backend demonstration program, the above process is automated and easy to use. Using this mode can'
      'Infinite Stacked Code'
      ''
      ''
      'by.600585'
      '2018-5-22')
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
    Interval = 10
    OnTimer = Timer1Timer
    Left = 344
    Top = 16
  end
end
