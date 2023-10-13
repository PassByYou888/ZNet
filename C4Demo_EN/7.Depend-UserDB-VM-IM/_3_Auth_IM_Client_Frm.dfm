object _3_Auth_IM_Client_Form: T_3_Auth_IM_Client_Form
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  BorderWidth = 10
  Caption = 'Auth IM Client - create by.qq600585'
  ClientHeight = 434
  ClientWidth = 918
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 47
    Width = 89
    Height = 13
    Caption = 'Received message'
  end
  object Label2: TLabel
    Left = 255
    Top = 47
    Width = 90
    Height = 13
    Caption = 'Add friend request'
  end
  object Label3: TLabel
    Left = 502
    Top = 47
    Width = 122
    Height = 13
    Caption = 'Friends online notification'
  end
  object Label4: TLabel
    Left = 749
    Top = 47
    Width = 66
    Height = 13
    Caption = 'My friends list'
  end
  object LoginInfoLabel: TLabel
    Left = 8
    Top = 17
    Width = 86
    Height = 13
    Caption = 'Login information:'
  end
  object Memo: TMemo
    Left = 0
    Top = 262
    Width = 918
    Height = 172
    Align = alBottom
    TabOrder = 0
    WordWrap = False
  end
  object sendMsgButton: TButton
    Left = 255
    Top = 8
    Width = 89
    Height = 33
    Caption = 'send message'
    TabOrder = 1
    OnClick = sendMsgButtonClick
  end
  object msgMemo: TMemo
    Left = 8
    Top = 63
    Width = 241
    Height = 185
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object RequestFriendMemo: TMemo
    Left = 255
    Top = 63
    Width = 241
    Height = 185
    ScrollBars = ssBoth
    TabOrder = 3
    WordWrap = False
  end
  object OnlineMemo: TMemo
    Left = 502
    Top = 63
    Width = 241
    Height = 185
    ScrollBars = ssBoth
    TabOrder = 4
    WordWrap = False
  end
  object FriendListMemo: TMemo
    Left = 749
    Top = 63
    Width = 155
    Height = 185
    ScrollBars = ssBoth
    TabOrder = 5
    WordWrap = False
  end
  object RequestFriendButton: TButton
    Left = 350
    Top = 8
    Width = 81
    Height = 33
    Caption = 'Add friends'
    TabOrder = 6
    OnClick = RequestFriendButtonClick
  end
  object ReponseAddFriendButton: TButton
    Left = 437
    Top = 8
    Width = 91
    Height = 33
    Caption = 'Respond to friend requests'
    TabOrder = 7
    OnClick = ReponseAddFriendButtonClick
  end
  object refreshFriendButton: TButton
    Left = 749
    Top = 8
    Width = 155
    Height = 33
    Caption = 'Refresh my friends list'
    TabOrder = 8
    OnClick = refreshFriendButtonClick
  end
  object removeFriendButton: TButton
    Left = 534
    Top = 8
    Width = 74
    Height = 33
    Caption = 'Remove friends'
    TabOrder = 9
    OnClick = removeFriendButtonClick
  end
  object NetTimer: TTimer
    Interval = 10
    OnTimer = NetTimerTimer
    Left = 144
    Top = 280
  end
end
