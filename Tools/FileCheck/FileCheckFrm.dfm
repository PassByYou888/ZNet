object FileCheckForm: TFileCheckForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderWidth = 20
  Caption = 'File check tool. create by.qq600585'
  ClientHeight = 515
  ClientWidth = 761
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 7
    Width = 67
    Height = 13
    Caption = 'Addional files:'
  end
  object ThreadStateLabel: TLabel
    Left = 103
    Top = 328
    Width = 72
    Height = 13
    Caption = 'Thread States:'
  end
  object FilesMemo: TMemo
    Left = 0
    Top = 31
    Width = 761
    Height = 281
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object AddFilesButton: TButton
    Left = 81
    Top = 0
    Width = 75
    Height = 25
    Caption = 'Add Files'
    TabOrder = 1
    OnClick = AddFilesButtonClick
  end
  object LogMemo: TMemo
    Left = 0
    Top = 359
    Width = 761
    Height = 156
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object MD5CheckButton: TButton
    Left = 0
    Top = 318
    Width = 97
    Height = 35
    Caption = 'MD5 Check.'
    TabOrder = 3
    OnClick = MD5CheckButtonClick
  end
  object FileOpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'All Files(*.*)'
        FileMask = '*.*'
      end>
    Options = [fdoAllowMultiSelect, fdoPathMustExist, fdoFileMustExist]
    Left = 240
    Top = 72
  end
  object stateTimer: TTimer
    Interval = 200
    OnTimer = stateTimerTimer
    Left = 240
    Top = 168
  end
end
