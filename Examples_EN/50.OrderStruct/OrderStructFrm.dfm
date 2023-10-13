object OrderStructForm: TOrderStructForm
  Left = 0
  Top = 0
  Caption = 'Order Struct. create by.qq600585'
  ClientHeight = 331
  ClientWidth = 993
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
    Left = 24
    Top = 16
    Width = 945
    Height = 241
    Lines.Strings = (
      'Orderstruct is a sequence structure driven framework located in the kernel library. Its idea is based on the push / pop of assembly language'
      'OrderStruct'#39's work breaks free from the influence of linked lists and arrays, directly accessing structures. In queue mechanisms and parallel programs, performance and simplicity are superior to linked lists'
      ''
      'TOrderStruct<T_>'#65306' Example of general sequence structure framework, < t_> It can be a class, a record, or an atomic variable'
      
        'TOrderPtrStruct<T_>'#65306' Sequence structure framework instance of automation pointer, < T > If the record / atomic variable, it will be automatically converted to a pointer' +
        'Manage memory'
      
        'TCriticalOrderStruct<T_>'#65306' (thread safe) general sequence structure framework instance, < T > It can be a class, a record, or a' +
        'As an atomic variable, the preferred data queue framework for parallel/multi line programs'
      
        'TCriticalOrderPtrStruct<T_>'#65306' (thread safety) sequence structure framework instance of automation pointer, < T > If record / original' +
        'Sub variables will be automatically converted into pointers to automatically manage memory'
      ''
      'Orderstruct is currently widely used in parallel and multithreaded data optimization in the background of AI'
      ''
      'by.qq600585'
      '')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 24
    Top = 272
    Width = 129
    Height = 25
    Caption = 'TOrderPtrStruct<T>'
    TabOrder = 1
    OnClick = Button1Click
  end
end
