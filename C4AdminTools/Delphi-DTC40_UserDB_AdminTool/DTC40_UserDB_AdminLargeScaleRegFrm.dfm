object DTC40_UserDB_AdminLargeScaleRegForm: TDTC40_UserDB_AdminLargeScaleRegForm
  Left = 0
  Top = 0
  Caption = 'Large-scale registration.'
  ClientHeight = 369
  ClientWidth = 946
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poMainFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    946
    369)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 18
    Top = 23
    Width = 82
    Height = 13
    Caption = 'Corpus database'
  end
  object Label2: TLabel
    Left = 320
    Top = 23
    Width = 85
    Height = 13
    Caption = 'Registration Plan:'
  end
  object PlanListView: TListView
    Left = 320
    Top = 42
    Width = 597
    Height = 303
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'User Name'
        Width = 300
      end
      item
        AutoSize = True
        Caption = 'state'
      end>
    DoubleBuffered = True
    HideSelection = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    ParentDoubleBuffered = False
    TabOrder = 5
    ViewStyle = vsReport
    OnCreateItemClass = PlanListViewCreateItemClass
  end
  object makePlanButton: TButton
    Left = 411
    Top = 11
    Width = 129
    Height = 25
    Caption = 'Make Registration Plan'
    Default = True
    TabOrder = 0
    OnClick = makePlanButtonClick
  end
  object cleanPlanButton: TButton
    Left = 626
    Top = 11
    Width = 75
    Height = 25
    Caption = 'Clean plan'
    TabOrder = 2
    OnClick = cleanPlanButtonClick
  end
  object executePlanButton: TButton
    Left = 707
    Top = 11
    Width = 156
    Height = 25
    Caption = 'execute Registration Plan'
    TabOrder = 3
    OnClick = executePlanButtonClick
  end
  object CorpusListBox: TCheckListBox
    Left = 18
    Top = 42
    Width = 296
    Height = 303
    Anchors = [akLeft, akTop, akBottom]
    DoubleBuffered = True
    ItemHeight = 13
    ParentDoubleBuffered = False
    TabOrder = 4
  end
  object NumEdit: TLabeledEdit
    Left = 580
    Top = 15
    Width = 40
    Height = 21
    EditLabel.Width = 25
    EditLabel.Height = 13
    EditLabel.Caption = 'Num:'
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '100'
  end
end
