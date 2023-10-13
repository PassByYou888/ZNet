object MHMainForm: TMHMainForm
  Left = 0
  Top = 0
  Caption = 'MemoryHook demo'
  ClientHeight = 515
  ClientWidth = 1063
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
    Top = 0
    Width = 1063
    Height = 441
    Align = alClient
    BorderStyle = bsNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      
        'There are 4 copies of memory hooks within the ZNet project, whic' +
        'h can cross all platforms and have excellent performance'
      
        'The principle is to control the memory overhead of our program b' +
        'y hooking up the memory management unit and recording the alloca' +
        'tion address'
      ''
      'Scenario 1:'
      
        'When we create a class, we tick it and monitor its memory overhe' +
        'ad'
      ''
      'Scenario 2:'
      
        'In the server, we can hook up a function. When the function exit' +
        's, we calculate the allocated space for this function, and final' +
        'ly, we can determine whether there is a leak in this function'
      ''
      'Scenario 3:'
      
        'Memory hooks are extensively used in ZDBEngine (ZDB1.0) to manag' +
        'e cache memory overhead'
      ''
      'Mh.pas memory hook for batch management'
      'MH_1. PAS hooked for the first time'
      'MH_2. PAS second hook'
      'MH_3. PAS third hook'
      ''
      'Here is the DoStatus information')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 441
    Width = 1063
    Height = 74
    Align = alBottom
    BorderStyle = bsSingle
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 16
      Width = 121
      Height = 41
      Caption = 'Function call monitoring'
      TabOrder = 0
      WordWrap = True
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 135
      Top = 16
      Width = 266
      Height = 41
      Caption = 
        'Calculate the actual memory size of the Record and forcibly rele' +
        'ase it'
      TabOrder = 1
      WordWrap = True
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 407
      Top = 16
      Width = 257
      Height = 41
      Caption = 
        'High performance hook optimization for high frequency memory sta' +
        'tistics'
      TabOrder = 2
      WordWrap = True
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 670
      Top = 16
      Width = 210
      Height = 41
      Caption = 'Record bulk memory requests'
      TabOrder = 3
      WordWrap = True
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 886
      Top = 16
      Width = 153
      Height = 41
      Caption = 'Strict memory monitoring'
      TabOrder = 4
      WordWrap = True
      OnClick = Button5Click
    end
  end
end
