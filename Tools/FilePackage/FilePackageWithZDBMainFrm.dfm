object FilePackageWithZDBMainForm: TFilePackageWithZDBMainForm
  Left = 0
  Top = 0
  Caption = 'File Package.'
  ClientHeight = 361
  ClientWidth = 1184
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 1200
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Consolas'
  Font.Style = [fsBold]
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 14
  object Log_Splitter: TSplitter
    Left = 0
    Top = 256
    Width = 1184
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    ResizeStyle = rsUpdate
    ExplicitTop = 389
    ExplicitWidth = 1167
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 1184
    Height = 41
    Align = alTop
    BorderWidth = 5
    TabOrder = 0
    object Bevel3: TBevel
      Left = 333
      Top = 6
      Width = 10
      Height = 29
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 211
      ExplicitTop = 2
      ExplicitHeight = 39
    end
    object Bevel4: TBevel
      Left = 143
      Top = 6
      Width = 10
      Height = 29
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 38
      ExplicitTop = 8
      ExplicitHeight = 33
    end
    object Bevel5: TBevel
      Left = 203
      Top = 6
      Width = 10
      Height = 29
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 98
      ExplicitTop = 0
      ExplicitHeight = 33
    end
    object Bevel7: TBevel
      Left = 762
      Top = 6
      Width = 10
      Height = 29
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 637
      ExplicitTop = 0
      ExplicitHeight = 33
    end
    object NewButton: TButton
      Left = 6
      Top = 6
      Width = 50
      Height = 29
      Align = alLeft
      Caption = 'New'
      TabOrder = 0
      OnClick = NewButtonClick
    end
    object OpenButton: TButton
      Left = 153
      Top = 6
      Width = 50
      Height = 29
      Align = alLeft
      Caption = 'Open'
      TabOrder = 2
      OnClick = OpenButtonClick
    end
    object SaveButton: TButton
      Left = 213
      Top = 6
      Width = 50
      Height = 29
      Align = alLeft
      Caption = 'Save'
      TabOrder = 3
      OnClick = SaveButtonClick
    end
    object SaveAsButton: TButton
      Left = 263
      Top = 6
      Width = 70
      Height = 29
      Align = alLeft
      Caption = 'Save as'
      TabOrder = 4
      OnClick = SaveAsButtonClick
    end
    object CacheStateMemo: TMemo
      Left = 772
      Top = 6
      Width = 406
      Height = 29
      Align = alClient
      TabOrder = 6
      ExplicitLeft = 1031
      ExplicitWidth = 147
    end
    object CompressAsButton: TButton
      Left = 343
      Top = 6
      Width = 84
      Height = 29
      Align = alLeft
      Caption = 'Build .OXC'
      TabOrder = 7
      OnClick = CompressAsButtonClick
    end
    object BuildIndexPackageButton: TButton
      Left = 609
      Top = 6
      Width = 153
      Height = 29
      Align = alLeft
      Caption = 'Build Index Package'
      TabOrder = 5
      OnClick = BuildIndexPackageButtonClick
    end
    object NewCustomButton: TButton
      Left = 56
      Top = 6
      Width = 87
      Height = 29
      Align = alLeft
      Caption = 'New Custom'
      TabOrder = 1
      OnClick = NewCustomButtonClick
    end
    object ParallelCompressAsButton: TButton
      Left = 427
      Top = 6
      Width = 85
      Height = 29
      Align = alLeft
      Caption = 'Build .OXP'
      TabOrder = 8
      OnClick = ParallelCompressAsButtonClick
    end
    object SaveAsZDB2Button: TButton
      Left = 512
      Top = 6
      Width = 97
      Height = 29
      Align = alLeft
      Caption = 'Build .ZDB2'
      TabOrder = 9
      OnClick = SaveAsZDB2ButtonClick
    end
  end
  object Memo: TMemo
    Tag = 100
    Left = 0
    Top = 261
    Width = 1184
    Height = 100
    Align = alBottom
    BorderStyle = bsNone
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'all files(*.OX;*.OXC;*.OXP;*.ImgMat)|*.OX;*.OXC;*.OXP;*.ImgMat|O' +
      'bject Data(*.OX)|*.OX|Compressed Object Data(*.OXC)|*.OXC|Parall' +
      'el Compressed Object Data(*.OXP)|*.OXP|All(*.*)|*.*'
    Options = [ofPathMustExist, ofFileMustExist, ofShareAware, ofNoTestFileCreate, ofEnableSizing]
    Left = 56
    Top = 104
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.OX'
    Filter = 'Object Data(*.OX)|*.OX|All(*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 184
    Top = 112
  end
  object Timer: TTimer
    Interval = 100
    OnTimer = TimerTimer
    Left = 56
    Top = 160
  end
  object SaveAsCompressedDialog: TSaveDialog
    DefaultExt = '.OXC'
    Filter = 'Object Data(*.OXC)|*.OXC|All(*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 184
    Top = 160
  end
  object SaveAsParallelCompressedDialog: TSaveDialog
    DefaultExt = '.OXP'
    Filter = 'Object Data(*.OXP)|*.OXP|All(*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 184
    Top = 216
  end
  object SaveAsZDB2Dialog: TSaveDialog
    DefaultExt = '.OX2'
    Filter = 'Object Data(*.OX2)|*.OX2|All(*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 183
    Top = 273
  end
end
