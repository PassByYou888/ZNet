object PascalRewrite_TextEdtForm: TPascalRewrite_TextEdtForm
  Left = 0
  Top = 0
  Caption = 'Text Editor.'
  ClientHeight = 426
  ClientWidth = 839
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  PopupParent = PascalRewriteModelForm.Owner
  Position = poMainFormCenter
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object _b_Panel: TPanel
    Left = 0
    Top = 380
    Width = 839
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object DoneButton: TButton
      Left = 14
      Top = 6
      Width = 106
      Height = 33
      Caption = '&Done'
      ModalResult = 1
      TabOrder = 0
    end
    object cancelButton: TButton
      Left = 126
      Top = 6
      Width = 106
      Height = 33
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 839
    Height = 380
    Align = alClient
    DoubleBuffered = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
end
