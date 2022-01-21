object UnitRewriteServiceForm: TUnitRewriteServiceForm
  Left = 0
  Top = 0
  Caption = 'Unit Rewrite Service'
  ClientHeight = 411
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 852
    Height = 411
    Align = alClient
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object fpsTimer: TTimer
    Interval = 100
    OnTimer = fpsTimerTimer
    Left = 97
    Top = 107
  end
end
