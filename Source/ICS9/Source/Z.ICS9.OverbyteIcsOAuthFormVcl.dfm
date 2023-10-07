object OAuthLoginForm: TOAuthLoginForm
  Left = 0
  Top = 0
  Caption = 'OAuth Login Window'
  ClientHeight = 627
  ClientWidth = 684
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 684
    Height = 46
    Align = alTop
    TabOrder = 0
    object LabelTitle: TLabel
      Left = 10
      Top = 5
      Width = 586
      Height = 20
      AutoSize = False
      Caption = 'Loading Web Page'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object LabelAccount: TLabel
      Left = 10
      Top = 27
      Width = 73
      Height = 15
      Caption = 'LabelAccount'
    end
    object doClose: TButton
      Left = 620
      Top = 10
      Width = 55
      Height = 25
      Caption = 'Close'
      TabOrder = 0
      OnClick = doCloseClick
    end
  end
  object PanelClient: TPanel
    Left = 0
    Top = 46
    Width = 684
    Height = 581
    Align = alClient
    TabOrder = 1
    object PanelWebBrowser: TPanel
      Left = 190
      Top = 115
      Width = 245
      Height = 196
      TabOrder = 0
      Visible = False
      object WebBrowser: TWebBrowser
        Left = 1
        Top = 1
        Width = 243
        Height = 194
        Align = alClient
        TabOrder = 0
        OnTitleChange = WebBrowserTitleChange
        OnBeforeNavigate2 = WebBrowserBeforeNavigate2
        OnNavigateComplete2 = WebBrowserNavigateComplete2
        ExplicitLeft = 0
        ExplicitTop = -9
        ControlData = {
          4C0000001D1900000D1400000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E12620A000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
    end
  end
  object TimerClose: TTimer
    Enabled = False
    Interval = 10000
    OnTimer = TimerCloseTimer
    Left = 30
    Top = 65
  end
end
