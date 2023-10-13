object FragmentClientForm: TFragmentClientForm
  Left = 0
  Top = 0
  AutoSize = True
  Caption = 'Multi line file download demo'
  ClientHeight = 385
  ClientWidth = 944
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 40
    Width = 545
    Height = 345
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      
        'This demo demonstrates multi line file download (similar to Thun' +
        'der)'
      
        'The multi line download mechanism of this demo is a standard mec' +
        'hanism, so look at the code more, especially the data structure ' +
        'design'
      
        'The transmission mechanism of this demo is CompleteBuffer, which' +
        ' requires controlling memory overhead'
      
        'The demo server will not temporarily store all files. The file d' +
        'ata temporarily stored by the server depends on the download thr' +
        'ead. It will not explode'
      'by.qq600585'
      '')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object HostEdit: TLabeledEdit
    Left = 40
    Top = 8
    Width = 161
    Height = 21
    EditLabel.Width = 29
    EditLabel.Height = 13
    EditLabel.Caption = 'Host: '
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '127.0.0.1'
  end
  object connButton: TButton
    Left = 207
    Top = 0
    Width = 107
    Height = 34
    Caption = 'Create 10 physical connections'
    TabOrder = 2
    WordWrap = True
    OnClick = connButtonClick
  end
  object downloadButton: TButton
    Left = 320
    Top = 0
    Width = 129
    Height = 34
    Caption = 'Multi threaded download'
    TabOrder = 3
    WordWrap = True
    OnClick = downloadButtonClick
  end
  object stateMemo: TMemo
    Left = 551
    Top = 40
    Width = 393
    Height = 345
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 152
    Top = 120
  end
  object checkDownTimer: TTimer
    OnTimer = checkDownTimerTimer
    Left = 152
    Top = 192
  end
end
