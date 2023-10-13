object TZDB2_Json_DB_Frm: TTZDB2_Json_DB_Frm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  BorderWidth = 10
  Caption = 'Json DB demo. Create by.qq600585'
  ClientHeight = 471
  ClientWidth = 1057
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
  object MakeInfoLabel: TLabel
    Left = 599
    Top = 37
    Width = 45
    Height = 13
    Caption = '..'
  end
  object thInfoLabel: TLabel
    Left = 211
    Top = 99
    Width = 8
    Height = 13
    Caption = '..'
  end
  object MakeJsonButton: TButton
    Left = 24
    Top = 32
    Width = 569
    Height = 25
    Caption = 
      'Generate a total of 10000 Json entries with 500 expressions each' +
      ', resulting in a total of 5 million expressions'
    TabOrder = 0
    OnClick = MakeJsonButtonClick
  end
  object Memo: TMemo
    Left = 0
    Top = 308
    Width = 1057
    Height = 163
    Align = alBottom
    Lines.Strings = (
      'ZDB2'#39's JSON database operation demo,'
      'This demo contains'
      
        'The stupid query method is to use '#39'for'#39' and complete the query i' +
        'n one loop'
      
        'Data processing pipeline IO queue technology, using efficient mu' +
        'lti-core hardware (thread support system, currently No.1 in Pasc' +
        'al circles, performance and programming model, compatible with f' +
        'pc/del)phi)'
      
        'The basic usage paradigms of models for data addition, deletion,' +
        ' and modification are all using the for, save, and remove method' +
        's'
      
        'Intuitively experience the JSON database performance of ZDB2, wh' +
        'ich supports over 10 data structures. Study it yourself'
      ''
      
        'The occurrence of stuttering is ongoing in the ZDB2 kernel: flus' +
        'h cache, initialization, expansion, and memory reclamation, all ' +
        'of which are carried out in the ZDB2 kernel'
      ''
      '2022-1-3'
      'by.qq600585'
      '')
    TabOrder = 1
    WordWrap = False
    ExplicitWidth = 842
  end
  object newbieQueryButton: TButton
    Left = 24
    Top = 63
    Width = 181
    Height = 25
    Caption = 'Easy Query'
    TabOrder = 2
    OnClick = newbieQueryButtonClick
  end
  object expertQueryButton: TButton
    Left = 24
    Top = 94
    Width = 181
    Height = 25
    Caption = 'Professional inquiry'
    TabOrder = 3
    OnClick = expertQueryButtonClick
  end
  object removeButton: TButton
    Left = 24
    Top = 125
    Width = 181
    Height = 25
    Caption = 'delete'
    TabOrder = 4
    OnClick = removeButtonClick
  end
  object ProgressBar: TProgressBar
    Left = 24
    Top = 268
    Width = 1001
    Height = 17
    TabOrder = 5
  end
  object modifyButton: TButton
    Left = 24
    Top = 156
    Width = 181
    Height = 25
    Caption = 'change'
    TabOrder = 6
    OnClick = modifyButtonClick
  end
  object extractSpaceButton: TButton
    Left = 24
    Top = 205
    Width = 181
    Height = 25
    Caption = 'Organize space debris'
    TabOrder = 7
    OnClick = extractSpaceButtonClick
  end
  object fpsTimer: TTimer
    Interval = 100
    OnTimer = fpsTimerTimer
    Left = 394
    Top = 144
  end
end
