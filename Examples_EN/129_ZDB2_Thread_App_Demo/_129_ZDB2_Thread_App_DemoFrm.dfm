object _129_ZDB2_Thread_App_DemoForm: T_129_ZDB2_Thread_App_DemoForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'ZDB2 Thread App Demo, create by.qq600585'
  ClientHeight = 501
  ClientWidth = 565
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Data_Num_Label: TLabel
    Left = 8
    Top = 52
    Width = 36
    Height = 13
    Caption = '.........'
  end
  object Remove_Num_Label: TLabel
    Left = 8
    Top = 71
    Width = 36
    Height = 13
    Caption = '.........'
  end
  object Add_Num_Label: TLabel
    Left = 8
    Top = 90
    Width = 36
    Height = 13
    Caption = '.........'
  end
  object Parallel_Load_Num_Label: TLabel
    Left = 8
    Top = 109
    Width = 36
    Height = 13
    Caption = '.........'
  end
  object For_Num_Label: TLabel
    Left = 8
    Top = 128
    Width = 36
    Height = 13
    Caption = '.........'
  end
  object Progress_Num_Label: TLabel
    Left = 8
    Top = 147
    Width = 36
    Height = 13
    Caption = '.........'
  end
  object Queue_Num_Label: TLabel
    Left = 8
    Top = 166
    Width = 36
    Height = 13
    Caption = '.........'
  end
  object Memo: TMemo
    Left = 0
    Top = 320
    Width = 565
    Height = 181
    Align = alBottom
    Lines.Strings = (
      
        'ZDB2 is an advanced big data engine that directly supports the a' +
        'ddition, deletion, and modification of big data and multithreadi' +
        'ng from the bottom level'
      ''
      
        'Note: The default MM does not support high concurrency and must ' +
        'use the specified MM library, jemalloc4p or tcmalloc4p'
      ''
      'Explanation of Terms'
      
        'Physical traversal: Parallel mode reads from the entire physical' +
        ' IO library, usually used to initiate database loading and does ' +
        'not support interrupts'
      
        'Instance traversal: The parallelization method loops through ins' +
        'tances from end to end, with varying physical IO, similar to for' +
        ', and supports interrupts'
      
        'Main loop: Only working and when the large loop process is idle,' +
        ' the traversal process model is in a non working state'
      
        'Instance recycling: When deleting data, the instance is not rele' +
        'ased, but instead placed in the recycling bin'
      
        'Data chain recycling: When deleting data, the upper and lower de' +
        'pendencies of the data will not be deleted, but will be placed i' +
        'n the data chain recycling bin'
      ''
      'by.qq600585')
    ScrollBars = ssVertical
    TabOrder = 4
    WordWrap = False
  end
  object runButton: TButton
    Left = 8
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Execute/Stop'
    TabOrder = 0
    OnClick = runButtonClick
  end
  object Parallel_Load_CheckBox: TCheckBox
    Left = 111
    Top = 12
    Width = 114
    Height = 17
    Caption = 'Physical traversal'
    TabOrder = 1
  end
  object For_CheckBox: TCheckBox
    Left = 231
    Top = 12
    Width = 114
    Height = 17
    Caption = 'Instance traversal'
    TabOrder = 2
  end
  object Progress_CheckBox: TCheckBox
    Left = 351
    Top = 12
    Width = 74
    Height = 17
    Caption = 'Main loop'
    TabOrder = 3
  end
  object fpsTimer: TTimer
    Interval = 100
    OnTimer = fpsTimerTimer
    Left = 312
    Top = 136
  end
end
