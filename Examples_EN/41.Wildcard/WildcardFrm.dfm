﻿object WildcardForm: TWildcardForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderWidth = 10
  Caption = 'Wildcard Demo'
  ClientHeight = 188
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 64
    Width = 553
    Height = 124
    Lines.Strings = (
      'Wildcards are a special type of statement, mainly consisting of asterisks (*) and question marks (?), used to blur search for files.'
      'When searching for a folder, it can be used instead of one or more real characters;'
      'When you don'#39't know the real characters or are too lazy to enter the full name, you often use wildcards to replace one or more real characters.'
      '*Can represent any string;? Only represents a single string, but this single word must exist'
      ''
      'Umlmultiplematch is a wildcard string matching function')
    TabOrder = 0
  end
  object SourEdit: TLabeledEdit
    Left = 88
    Top = 0
    Width = 153
    Height = 21
    EditLabel.Width = 78
    EditLabel.Height = 13
    EditLabel.Caption = 'wildchar source:'
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '*3?5'
  end
  object TargetEdit: TLabeledEdit
    Left = 88
    Top = 27
    Width = 153
    Height = 21
    EditLabel.Width = 64
    EditLabel.Height = 13
    EditLabel.Caption = 'target string:'
    LabelPosition = lpLeft
    TabOrder = 2
    Text = '12345'
  end
  object MatchButton: TButton
    Left = 247
    Top = 25
    Width = 75
    Height = 25
    Caption = 'Match'
    TabOrder = 3
    OnClick = MatchButtonClick
  end
end
