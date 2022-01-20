object PascalCommentRepForm: TPascalCommentRepForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsSingle
  BorderWidth = 20
  Caption = 'Pascal source Comments Replace. create by.qq600585'
  ClientHeight = 585
  ClientWidth = 1017
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Consolas'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 14
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 809
    Height = 585
    Lines.Strings = (
      'program helloworld;'
      'begin'
      '  //'
      '  // print hello world'
      '  //'
      '  writeln('#39'hello world'#39');'
      '  //'
      'end.')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Rep1Button: TButton
    Left = 815
    Top = 0
    Width = 202
    Height = 25
    Caption = 'replace //xx as { xx }'
    TabOrder = 1
    OnClick = Rep1ButtonClick
  end
  object removeEmptyButton: TButton
    Left = 815
    Top = 63
    Width = 202
    Height = 25
    Caption = 'remove empty comment'
    TabOrder = 3
    OnClick = removeEmptyButtonClick
  end
  object Rep2Button: TButton
    Left = 815
    Top = 31
    Width = 202
    Height = 25
    Caption = 'replace //xx as (* xx *)'
    TabOrder = 2
    OnClick = Rep2ButtonClick
  end
end
