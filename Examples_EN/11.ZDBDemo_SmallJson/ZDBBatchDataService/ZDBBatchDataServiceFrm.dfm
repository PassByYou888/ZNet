object ZDBBatchDataServiceForm: TZDBBatchDataServiceForm
  Left = 0
  Top = 0
  Caption = 'Dataset Services'
  ClientHeight = 744
  ClientWidth = 1402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusMemo: TMemo
    Left = 0
    Top = 0
    Width = 937
    Height = 744
    Align = alClient
    Lines.Strings = (
      'ZDB Dataset Service:'
      'Using Json as the data structure, demonstrate the actual application of ZDB in the project'
      'The ZDB interface used by the dataset service is a validation based interface that requires client login'
      'There are two default login accounts'
      'User: Test1 Password: test1'
      'User: Test2 Password: test2'
      ''
      'ZDB is a very powerful large database processing engine that is currently being commercialized in a large and medium-sized project. In the near future, ZDB will combine clustering algorithms to solve the problem of deep data processing'
      'Using ZDB for small and medium-sized enterprise applications is like killing chickens with a knife. If you want to apply it to the field of small and medium-sized enterprises, please rest assured to use ZDB'
      'On important data security issues, please use the CoreCipher library to encrypt and decrypt on your own (remember to turn on the parallel encoding switch of zDefineinc)'
      'ZDB queries are fed back in the form of fragments, which can be sent from 1000 lines. Fragments can still have correct feedback results'
      'ZDB database can work on two media: disk files and operating system memory'
      'Because virtual memory is not supported, when using ZDB, pay attention to controlling the cache annealing parameters based on your own backend configuration'
      
        'ZDB temporary database will use memory copy. For commercial use, please prepare one of fastmm, tcmalloc, jemalloc and nexusdb professional database' +
        'Memory support library, and please read the technical paper in the field of memory optimization'
      'Running ZDB does not require array support, ZDB only requires medium scale memory (such as running tens of millions of item queries+analysis, using 128GB of memory+2 core CPUs)'
      'Note: After the ZDB database entries exceed ten million, the memory throughput in the parallel circuit is very high. If there is not enough memory, please lower the Cache limit parameter to sacrifice performance in exchange for stability'
      'Note: Do not use the x86 platform to run the ZDB server'
      'Note: For Windows servers running ZDB, please choose a system with Server2012 or above and enable the parallel option in zDefineinc'
      'Note: servers running ZDB need to perform a Compress operation at least once a week to improve disk life and query efficiency'
      'Pay attention to the above four points, ZDB is a very violent large data engine'
      ''
      'The operation method of ZDB client is consistent with that of a single machine'
      'ZDB network clients work in zero blocking asynchronous mode'
      ''
      'The demo provides basic addition, modification, deletion, query and other demonstration methods'
      'There are two sets of ZDB network services, 1. with authentication and 2. without authentication'
      'This demo uses a ZDB server model without authentication'
      ''
      'The characteristics of ZDB network services are very distinct'
      'Queries can be fully manually operated without the need for SQL statements'
      'High speed submission and modification of Stream'
      'Support for big data cutting'
      'Support big data analysis'
      'Support professional HPC'
      'Single IO concurrency mechanism, supporting mid to low end cloud backend'
      'Annealed cache: The more parallel queries, the faster the speed. After the query task is completed, ZDB will automatically anneal, which can push the big data throughput limit'
      'ZDB'#39's network services are all conducted in the background, with zero blocking'
      'It is safe to frequently perform ZDB compression processing in the background'
      'There are no memory leaks in both the network and backend databases'
      'Compiling ZDB does not require installing controls, just setting a directory'
      'ZDB can handle both coarse and heavy data processing, and can also be used in companion data systems attached to the main database system'
      'Database data transmission encryption, data storage not encrypted (please manually resolve storage encryption)'
      'The underlying database comes with disk read ahead and write back support'
      'Using ZDB does not require consideration of synchronous, asynchronous, and concurrent issues, only requires writing data matching judgments and performing data processing'
      ''
      'About Testing'
      
        'There is almost transparent information on the internal working status of ZDB on both the server and client sides. If it is not enough, tools such as VMMap and DeskMonitor can be used to observe memory' +
        'And the IO status of the hard drive'
      
        'Due to time constraints, Demo will not do too much. For example, for Json based operation methods, please refer to TDataFrameEngine for use (do not save Json to T' +
        'In DataFrameEngine, ZDB can directly operate Json'
      'The author'#39's maintenance time for ZDB is fragmented. If any issues are found, please try to contact me via EMail, QQ email 600585 QQ. com')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object WatchMemo: TMemo
    Left = 937
    Top = 0
    Width = 465
    Height = 744
    Align = alRight
    ReadOnly = True
    TabOrder = 1
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 472
    Top = 328
  end
  object Timer2: TTimer
    OnTimer = Timer2Timer
    Left = 592
    Top = 376
  end
end
