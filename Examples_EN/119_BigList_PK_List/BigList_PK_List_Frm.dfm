object BigList_PK_List_Form: TBigList_PK_List_Form
  Left = 0
  Top = 0
  Caption = 'BigList PK List'
  ClientHeight = 455
  ClientWidth = 804
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 213
    Width = 804
    Height = 242
    Align = alBottom
    Lines.Strings = (
      
        'BigList is a 64 bit address segment container that can hold larg' +
        'e data of over 2 billion orders of magnitude in 64 bit mode'
      
        'BigList performs an average of 10 times better than List in queu' +
        'e, insert, delete, and other operations'
      
        'If BigList is used to directly replace List, natural algorithmic' +
        ' optimization can be achieved, and the project speed will be sig' +
        'nificantly increased'
      
        'Even using BigList instead of partially using List code can achi' +
        'eve algorithm optimization results'
      ''
      
        'BigList relies heavily on fragmented memory management capabilit' +
        'ies. If BigList is used on a large scale in threads, such as an ' +
        'HPC server'
      ''
      'Please refer to jemalloc4p and tcmalloc4p'
      'https://github.com/PassByYou888/jemalloc4p'
      'https://github.com/PassByYou888/tcmalloc4p'
      ''
      'Attention:'
      
        'Modern CPUs have instruction set optimization, and a performance' +
        ' gap of<50ms can be considered consistent'
      
        'Only when the gap reaches several times can it be used as a refe' +
        'rence for optimization results'
      ''
      'by.qq600585'
      '2022-4'
      '')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Add_Perf_Button: TButton
    Left = 26
    Top = 26
    Width = 255
    Height = 25
    Caption = 'Data addition capability PK'
    TabOrder = 1
    OnClick = Add_Perf_ButtonClick
  end
  object queue_perf_Button: TButton
    Left = 26
    Top = 90
    Width = 255
    Height = 25
    Caption = 'Data queue processing capability PK'
    TabOrder = 2
    OnClick = queue_perf_ButtonClick
  end
  object Traversal_perf_Button: TButton
    Left = 26
    Top = 121
    Width = 255
    Height = 25
    Caption = 'Data traversal processing capability PK'
    TabOrder = 3
    OnClick = Traversal_perf_ButtonClick
  end
  object delete_perf_Button: TButton
    Left = 26
    Top = 152
    Width = 255
    Height = 25
    Caption = 'Data deletion processing capability PK'
    TabOrder = 4
    OnClick = delete_perf_ButtonClick
  end
  object insert_perf_Button: TButton
    Left = 26
    Top = 57
    Width = 255
    Height = 25
    Caption = 'Data insertion capability PK'
    TabOrder = 5
    OnClick = insert_perf_ButtonClick
  end
end
