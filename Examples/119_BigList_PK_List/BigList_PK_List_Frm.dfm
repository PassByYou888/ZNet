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
      'BigList'#26159'64'#20301#22320#22336#27573#23481#22120#65292#22312'64'#20301#27169#24335#19979#21487#20197#35013#19979#22823#20110'20'#20159#25968#37327#32423#30340#22823#25968#25454
      'BigList'#22312#38431#21015#65292#25554#20837#65292#21024#38500#65292#31561#31561#25805#20316#19978#65292#24179#22343#24615#33021'10'#20493#20110'List'
      #22914#26524#29992'BigList'#30452#25509#26367#25442'List'#65292#21487#33719#24471#22825#28982#30340#31639#27861#21035#20248#21270#65292#39033#30446#25552#36895#20250#38750#24120#26126#26174
      #21363#20351#20351#29992'BigList'#26367#20195#37096#20998#20351#29992'List'#30340#20195#30721#65292#20063#33021#33719#24471#31639#27861#20248#21270#25928#26524
      ''
      'BigList'#38750#24120#20381#36182#30862#29255#20869#23384#31649#29702#33021#21147#65292#22914#26524#22312#32447#31243#20013#22823#35268#27169#20351#29992'BigList'#65292#20363#22914#65292'HPC'#26381#21153#22120
      ''
      #35831#21442#32771'jemalloc4p'#21644'tcmalloc4p'
      'https://github.com/PassByYou888/jemalloc4p'
      'https://github.com/PassByYou888/tcmalloc4p'
      ''
      #27880#24847#65306
      #22788#20110#22823#25968#25454#35282#24230#35774#35745#65292'BigList'#26159#19981#25903#25345#25490#24207#25805#20316#30340
      #29616#20195#21270#30340'CPU'#37117#26377#25351#20196#38598#20248#21270#65292#24615#33021#24046#36317'<50ms'#21487#20197#35270#20316#19968#33268
      #36798#21040#25968#20493#24046#36317#26102#65292#25165#33021#20316#20026#20248#21270#32467#26524#21442#32771
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
    Width = 164
    Height = 25
    Caption = #25968#25454#36861#21152#33021#21147'PK'
    TabOrder = 1
    OnClick = Add_Perf_ButtonClick
  end
  object queue_perf_Button: TButton
    Left = 26
    Top = 90
    Width = 164
    Height = 25
    Caption = #25968#25454#38431#21015#22788#29702#33021#21147'PK'
    TabOrder = 2
    OnClick = queue_perf_ButtonClick
  end
  object Traversal_perf_Button: TButton
    Left = 26
    Top = 121
    Width = 164
    Height = 25
    Caption = #25968#25454#36941#21382#22788#29702#33021#21147'PK'
    TabOrder = 3
    OnClick = Traversal_perf_ButtonClick
  end
  object delete_perf_Button: TButton
    Left = 26
    Top = 152
    Width = 164
    Height = 25
    Caption = #25968#25454#21024#38500#22788#29702#33021#21147'PK'
    TabOrder = 4
    OnClick = delete_perf_ButtonClick
  end
  object insert_perf_Button: TButton
    Left = 26
    Top = 57
    Width = 164
    Height = 25
    Caption = #25968#25454#25554#20837#33021#21147'PK'
    TabOrder = 5
    OnClick = insert_perf_ButtonClick
  end
end
