{ ****************************************************************************** }
{ * ZDB 2.0 Large Data templet for HPC                                         * }
{ ****************************************************************************** }
unit Z.ZDB2.Thread.LargeData;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses DateUtils, SysUtils,
  Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ELSE FPC}
  System.IOUtils,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib,
  Z.MemoryStream,
  Z.Status, Z.Cipher, Z.ZDB2, Z.ListEngine, Z.TextDataEngine, Z.IOThread,
  Z.HashList.Templet, Z.DFE, Z.Geometry2D,
  Z.Notify, Z.ZDB2.Thread.Queue, Z.ZDB2.Thread;

type
  { Pre declaration }
  TZDB2_Large = class;
  TZDB2_Custom_Small_Data = class;
  TZDB2_Custom_Medium_Data = class;
  TZDB2_Custom_Large_Data = class;
  TZDB2_Custom_Small_Data_Class = class of TZDB2_Custom_Small_Data;
  TZDB2_Custom_Medium_Data_Class = class of TZDB2_Custom_Medium_Data;
  TZDB2_Custom_Large_Data_Class = class of TZDB2_Custom_Large_Data;

  { Sequence ID pool structure, providing reverse lookup function }
  TZDB2_Custom_Small_Sequence_ID_Pool = TCritical_Big_Hash_Pair_Pool<Int64, TZDB2_Custom_Small_Data>;
  TZDB2_Custom_Medium_Sequence_ID_Pool = TCritical_Big_Hash_Pair_Pool<Int64, TZDB2_Custom_Medium_Data>;
  TZDB2_Custom_Large_Sequence_ID_Pool = TCritical_Big_Hash_Pair_Pool<Int64, TZDB2_Custom_Large_Data>;

  { Chain tools, data exchange, caching, batching, computation, classification }
  TZDB2_Custom_Data_Pool = TBigList<TZDB2_Th_Engine_Data>;
  TZDB2_Custom_Small_Data_Pool = TBigList<Int64>;
  TZDB2_Custom_Medium_Data_Pool = TBigList<Int64>;
  TZDB2_Custom_Large_Data_Pool = TBigList<Int64>;

  TZDB2_Custom_Batch_Data_Post_Bridge = class;

  TZDB2_Custom_Batch_Data_Post_Bridge_Event_C = procedure(Sender: TZDB2_Custom_Batch_Data_Post_Bridge);
  TZDB2_Custom_Batch_Data_Post_Bridge_Event_M = procedure(Sender: TZDB2_Custom_Batch_Data_Post_Bridge) of object;
{$IFDEF FPC}
  TZDB2_Custom_Batch_Data_Post_Bridge_Event_P = procedure(Sender: TZDB2_Custom_Batch_Data_Post_Bridge) is nested;
{$ELSE FPC}
  TZDB2_Custom_Batch_Data_Post_Bridge_Event_P = reference to procedure(Sender: TZDB2_Custom_Batch_Data_Post_Bridge);
{$ENDIF FPC}

  { TZDB2_Custom_Batch_Data_Post_Bridge solves the optimal storage performance of combined data }
  { It supports saving one batch at a time and returning the stored results. This mechanism can be used for data binding, such as one json small data binding multiple medium data or Big data entries }
  TZDB2_Custom_Batch_Data_Post_Bridge = class
  private
    FCritical: TCritical;
    Total_Post: Integer;
    Post_Busy: Integer;
    procedure Do_Save_Data_Result(Sender: TZDB2_Th_Engine_Data; Successed: Boolean);
    procedure Do_Check_Post_Done;
    procedure Do_All_Post_Done;
  public
    FOwner_Large_Marshal: TZDB2_Large;
    Successed_Num: Integer;
    Error_Num: Integer;
    Error_Do_Remove_Data: Boolean;
    Post_Pool: TZDB2_Custom_Data_Pool;
    Done_Pool: TZDB2_Custom_Data_Pool;
    OnResult_C: TZDB2_Custom_Batch_Data_Post_Bridge_Event_C; // done event
    OnResult_M: TZDB2_Custom_Batch_Data_Post_Bridge_Event_M; // done event
    OnResult_P: TZDB2_Custom_Batch_Data_Post_Bridge_Event_P; // done event
    User_Hash_Variants: THashVariantList;
    User_Hash_Strings: THashStringList;
    constructor Create(Owner_Large_DB_Engine_: TZDB2_Large);
    destructor Destroy; override;
    procedure Begin_Post;
    function Post_Data_To_S_DB(data: TMS64; AutoFree_: Boolean): TZDB2_Custom_Small_Data;
    function Post_Data_To_M_DB(data: TMS64; AutoFree_: Boolean): TZDB2_Custom_Medium_Data;
    function Post_Data_To_L_DB(data: TMS64; AutoFree_: Boolean): TZDB2_Custom_Large_Data;
    procedure End_Post;
    procedure Done_Post;
  end;

  { TZDB2_Custom_Small_Data storage format }
  { Small data has a storage sequence mechanism that can be automatically preloaded, with full data loading as the preloading method }
  { The following data structure Make your own decision, and when opening the database, you will use Extract_Data_Source initializes data }
  { The number of small data is unlimited. Individual small data should not be treated as Big data. It is recommended that individuals should not exceed 10kb }
  { Small storage block of approximately 1KB, single library storage limit of 1.5TB }
  { Save DFE, JSON, XML, ini, HTML, logs, running device status, using small data }
  { Type aliases in the script: 'Small', 'Tiny', 'Little', 'S',' Lv1 ',' 1 ' }
  TZDB2_Custom_Small_Data = class(TZDB2_Th_Engine_Data)
  private
    FOwner_Large_Marshal: TZDB2_Large;
    { 0-7: Int64, serialization ID, representing the sequence of data entries, and opening the database will automatically restore the sorting order }
    FSequence_ID: Int64;
    { 8-24: MD5 of the data body, which represents the data body ending from 24. Providing MD5 here is equivalent to providing reference information for fault recovery }
    FMD5: TMD5;
  public
    property Owner_Large_Marshal: TZDB2_Large read FOwner_Large_Marshal;
    property Sequence_ID: Int64 read FSequence_ID write FSequence_ID;
    property MD5: TMD5 read FMD5 write FMD5;
    constructor Create(); override;
    destructor Destroy; override;
    { When data is removed }
    { If a lot of data is bound, it is necessary to delete the binding together }
    procedure Do_Remove(); override;
    { Unpack the original data body, which does not contain a sequence_ID, where you can store Json, ini, dfe, xml }
    { Suggest using the dfe structure: dfe supports incomplete data, with greater optimization space for data performance in the middle and later stages, and CPU+memory overhead optimization }
    { Programming can be done through external interfaces }
    procedure Extract_Data_Source(Data_Source: TMS64); virtual;
    { Encoding data body -> Store physical data of inverted database }
    { Programming can be done through external interfaces }
    function Encode_To_ZDB2_Data(Data_Source: TMS64; AutoFree_: Boolean): TMem64; virtual;
    { Decode the original data from the database and return the original data body, excluding Sequence_ID, MD5 }
    { Decoding returns TMS64 in Mapping mode without copying }
    { Data_Source is always complete data }
    { Programming can be done through external interfaces }
    function Decode_From_ZDB2_Data(Data_Source: TMem64; Update_: Boolean): TMS64; virtual;
    { Extended Data Header Technology for Solving the Decentralized performance Problem of hdd }
    { When the data volume is large, a dataset contains dozens or hundreds of small databases. During disk operation, disk pre reads will be read in blocks, which greatly consumes HDD read time }
    { Expanding the data header is to gather thousands of fragmented databases and read them all at once, thereby improving the startup efficiency of the database. At the same time, pre reading also requires higher memory requirements }
    { Pre reading technology can improve data loading efficiency in the vast majority of hdd systems }
    procedure Encode_External_Header_Data(External_Header_Data: TMem64); virtual;
    procedure Decode_External_Header_Data(External_Header_Data: TMem64); virtual;
  end;

  { TZDB2_Custom_Medium_Data storage format }
  { The middle data sequence mechanism is small block serialization, which does not read the entire data during preloading. It reads the first block data and retrieves the sequence ID from the block. The middle data block defaults to 8kb }
  { The highest efficiency of using block to pre read sequence IDs is in non encrypted mode. If in encrypted mode, the block will decode a large block and read the sequence ID again }
  { Some array systems have a pre read mechanism. If there are too many sub libraries in the data, the pre read mechanism will fail. In this case, please adjust it according to the array system parameters, and it is uncertain if it can improve the performance }
  { Small storage blocks are approximately 8KB, with a single library storage limit of 15TB }
  { Save Piture, PDFs, docs, codes, and other small files, using medium data }
  { Type aliases in the script: 'Default', 'Medium', 'Middle', 'M', 'Lv2', '2' }
  TZDB2_Custom_Medium_Data = class(TZDB2_Th_Engine_Data)
  private
    FOwner_Large_Marshal: TZDB2_Large;
    { 0-7: Int64, serialization ID, representing the sequence of data entries. Opening the database will automatically restore the sorting order, and Extract is not supported_Data_Source }
    FSequence_ID: Int64;
    { 8-24: The md5 of the data body represents the data body that ends from 24. Providing md5 here is equivalent to providing a guarantee for fault recovery }
    { Medium Big data will only calculate md5 when submitting and repairing the database. There will be a slight delay in calculating md5 when submitting, but the impact will be slight }
    FMD5: TMD5;
  public
    property Owner_Large_Marshal: TZDB2_Large read FOwner_Large_Marshal;
    property Sequence_ID: Int64 read FSequence_ID write FSequence_ID;
    property MD5: TMD5 read FMD5 write FMD5;
    constructor Create(); override;
    destructor Destroy; override;
    { When data is removed }
    { If a lot of data is bound, it is necessary to delete the binding together }
    procedure Do_Remove(); override;
    { Pre read parameters, default to 24, kernel IO for disk pre read default to 500 bytes }
    class function Get_Prepare_Block_Read_Size: Integer; virtual;
    { Encoding data body -> Store physical data of inverted database }
    { Programming can be done through external interfaces }
    function Encode_To_ZDB2_Data(Data_Source: TMS64; AutoFree_: Boolean): TMem64; virtual;
    { Decode the original data from the database and return the original data body, excluding Sequence_ID, MD5 }
    { Decoding returns TMS64 in Mapping mode without copying }
    { When data is opened, Data_Source's data is a block block block, while other scenario data_Source is the complete data }
    { Programming can be done through external interfaces }
    function Decode_From_ZDB2_Data(Data_Source: TMem64; Update_: Boolean): TMS64; virtual;
    { Extended Data Header Technology for Solving the Decentralized performance Problem of hdd }
    { When the data volume is large, a dataset contains dozens or hundreds of small databases. During disk operation, disk pre reads will be read in blocks, which greatly consumes HDD read time }
    { Expanding the data header is to gather thousands of fragmented databases and read them all at once, thereby improving the startup efficiency of the database. At the same time, pre reading also requires higher memory requirements }
    { Pre reading technology can improve data loading efficiency in the vast majority of hdd systems }
    procedure Encode_External_Header_Data(External_Header_Data: TMem64); virtual;
    procedure Decode_External_Header_Data(External_Header_Data: TMem64); virtual;
  end;

  { TZDB2_Custom_Large_Data storage format }
  { The Big data sequence mechanism is block read ahead, and the block is 64kb by default }
  { Small storage block 64KB, single library storage limit 100TB }
  { Big data can be used to store videos, compressed packages, AI data sets, single day data packaging, weekly data packaging, and large files }
  { For example, for a 500M video, it is recommended to split it into 500 1M segments and then write the sequence stored in the video into small data_ID, then save }
  { Type aliases in the script: 'Large', 'Big', 'Huge', 'L', 'Lv3', '3' }
  TZDB2_Custom_Large_Data = class(TZDB2_Th_Engine_Data)
  private
    FOwner_Large_Marshal: TZDB2_Large;
    { 0-7: Int64, serialization ID, representing the sequence of data entries. Opening the database will automatically restore the sorting order, and Extract is not supported_Data_Source }
    FSequence_ID: Int64;
    { 8-24: The md5 of the data body represents the data body that ends from 24. Providing md5 here is equivalent to providing a guarantee for fault recovery }
    { Medium Big data will only calculate md5 when submitting and repairing the database. There will be a slight delay in calculating md5 when submitting, but the impact will be slight }
    FMD5: TMD5;
  public
    property Owner_Large_Marshal: TZDB2_Large read FOwner_Large_Marshal;
    property Sequence_ID: Int64 read FSequence_ID write FSequence_ID;
    property MD5: TMD5 read FMD5 write FMD5;
    constructor Create(); override;
    destructor Destroy; override;
    { When data is removed }
    { If a lot of data is bound, it is necessary to delete the binding together }
    procedure Do_Remove(); override;
    { Pre read parameters, default to 24, kernel IO for disk pre read default to 500 bytes }
    class function Get_Prepare_Block_Read_Size: Integer; virtual;
    { Encoding data body -> Store physical data of inverted database }
    { Programming can be done through external interfaces }
    function Encode_To_ZDB2_Data(Data_Source: TMS64; AutoFree_: Boolean): TMem64; virtual;
    { Decode the original data from the database and return the original data body, excluding Sequence_ID, MD5 }
    { Decoding returns TMS64 in Mapping mode without copying }
    { When data is opened, Data_Source's data is a block block block, while other scenario data_Source is the complete data }
    { Programming can be done through external interfaces }
    function Decode_From_ZDB2_Data(Data_Source: TMem64; Update_: Boolean): TMS64; virtual;
    { Extended Data Header Technology for Solving the Decentralized performance Problem of hdd }
    { When the data volume is large, a dataset contains dozens or hundreds of small databases. During disk operation, disk pre reads will be read in blocks, which greatly consumes HDD read time }
    { Expanding the data header is to gather thousands of fragmented databases and read them all at once, thereby improving the startup efficiency of the database. At the same time, pre reading also requires higher memory requirements }
    { Pre reading technology can improve data loading efficiency in the vast majority of hdd systems }
    procedure Encode_External_Header_Data(External_Header_Data: TMem64); virtual;
    procedure Decode_External_Header_Data(External_Header_Data: TMem64); virtual;
  end;

  { Extended Data Header Technology for Solving the Decentralized performance Problem of hdd }
  { When the data volume is large, a dataset contains dozens or hundreds of small databases. During disk operation, disk pre reads will be read in blocks, which greatly consumes HDD read time }
  { Expanding the data header is to gather thousands of fragmented databases and read them all at once, thereby improving the startup efficiency of the database. At the same time, pre reading also requires higher memory requirements }
  { Pre reading technology can improve data loading efficiency in the vast majority of hdd systems }
  TS_Th_Engine_Marshal = class(TZDB2_Th_Engine_Marshal)
  public
    Owner_Large_Marshal: TZDB2_Large;
    procedure Prepare_Flush_External_Header(Th_Engine_: TZDB2_Th_Engine; var Sequence_Table: TZDB2_BlockHandle; Flush_Instance_Pool: TZDB2_Th_Engine_Data_Instance_Pool; External_Header_Data_: TMem64); override;
    procedure Do_Extract_Th_Eng(ThSender: TCompute); virtual;
    procedure Extract_External_Header(var Extract_Done: Boolean); virtual;
    function Begin_Custom_Build: TZDB2_Th_Engine; virtual;
    function End_Custom_Build(Eng_: TZDB2_Th_Engine): Boolean; virtual;
  end;

  { Extended Data Header Technology for Solving the Decentralized performance Problem of hdd }
  { When the data volume is large, a dataset contains dozens or hundreds of small databases. During disk operation, disk pre reads will be read in blocks, which greatly consumes HDD read time }
  { Expanding the data header is to gather thousands of fragmented databases and read them all at once, thereby improving the startup efficiency of the database. At the same time, pre reading also requires higher memory requirements }
  { Pre reading technology can improve data loading efficiency in the vast majority of hdd systems }
  TM_Th_Engine_Marshal = class(TZDB2_Th_Engine_Marshal)
  public
    Owner_Large_Marshal: TZDB2_Large;
    procedure Prepare_Flush_External_Header(Th_Engine_: TZDB2_Th_Engine; var Sequence_Table: TZDB2_BlockHandle; Flush_Instance_Pool: TZDB2_Th_Engine_Data_Instance_Pool; External_Header_Data_: TMem64); override;
    procedure Do_Extract_Th_Eng(ThSender: TCompute); virtual;
    procedure Extract_External_Header(var Extract_Done: Boolean); virtual;
    function Begin_Custom_Build: TZDB2_Th_Engine; virtual;
    function End_Custom_Build(Eng_: TZDB2_Th_Engine): Boolean; virtual;
  end;

  { Extended Data Header Technology for Solving the Decentralized performance Problem of hdd }
  { When the data volume is large, a dataset contains dozens or hundreds of small databases. During disk operation, disk pre reads will be read in blocks, which greatly consumes HDD read time }
  { Expanding the data header is to gather thousands of fragmented databases and read them all at once, thereby improving the startup efficiency of the database. At the same time, pre reading also requires higher memory requirements }
  { Pre reading technology can improve data loading efficiency in the vast majority of hdd systems }
  TL_Th_Engine_Marshal = class(TZDB2_Th_Engine_Marshal)
  public
    Owner_Large_Marshal: TZDB2_Large;
    procedure Prepare_Flush_External_Header(Th_Engine_: TZDB2_Th_Engine; var Sequence_Table: TZDB2_BlockHandle; Flush_Instance_Pool: TZDB2_Th_Engine_Data_Instance_Pool; External_Header_Data_: TMem64); override;
    procedure Do_Extract_Th_Eng(ThSender: TCompute); virtual;
    procedure Extract_External_Header(var Extract_Done: Boolean); virtual;
    function Begin_Custom_Build: TZDB2_Th_Engine; virtual;
    function End_Custom_Build(Eng_: TZDB2_Th_Engine): Boolean; virtual;
  end;

  TS_Th_Engine_Marshal_Class = class of TS_Th_Engine_Marshal;
  TM_Th_Engine_Marshal_Class = class of TM_Th_Engine_Marshal;
  TL_Th_Engine_Marshal_Class = class of TL_Th_Engine_Marshal;

  TZDB2_Large = class
  private
    { Linear lock }
    FCritical: TCritical;
    { Active batch Post instances }
    FBatch_Post_Num: Integer;
    { Sequence seed }
    FCurrent_S_DB_Sequence_ID: Int64;
    FCurrent_M_DB_Sequence_ID: Int64;
    FCurrent_L_DB_Sequence_ID: Int64;
    { Interface data Class }
    FSmall_Data_Class: TZDB2_Custom_Small_Data_Class;
    FMedium_Data_Class: TZDB2_Custom_Medium_Data_Class;
    FLarge_Data_Class: TZDB2_Custom_Large_Data_Class;
    { Interface engine Class }
    FS_Th_Engine_Marshal_Class: TS_Th_Engine_Marshal_Class;
    FM_Th_Engine_Marshal_Class: TM_Th_Engine_Marshal_Class;
    FL_Th_Engine_Marshal_Class: TL_Th_Engine_Marshal_Class;
    { Small data is suitable for traversal, providing indexes for medium to large size data, and preparing for rapid expansion }
    { Small databases can be packaged and run on terminals }
    { A small database usually needs one sub database. If the number of a single database is greater than 10 million, open another database and go through a loop }
    { Small storage block of approximately 1KB, single library storage limit of 1.5TB }
    { Save logs, run device status, and use small data }
    { Type aliases in the script: 'Small', 'Tiny', 'Little', 'S',' Lv1 ',' 1 ' }
    FS_DB: TS_Th_Engine_Marshal;
    FS_DB_Sequence_Pool: TZDB2_Custom_Small_Sequence_ID_Pool;
    { Medium data is not suitable for traversal, usually positioned around 10TB. If traversing 10TB, memory needs to reach 10TB, otherwise the efficiency is extremely low }
    { Chinese databases can be packaged and run on terminals }
    { The size of the medium data sub library should not exceed the number of super threads/2. Based on the access density, if the density is low, such as each read and write volume is not high, then a large number of 100 can be opened }
    { Small storage blocks are approximately 8KB, with a single library storage limit of 15TB }
    { Saving small files such as images, using data }
    { Type aliases in the script: 'Default', 'Medium', 'Middle', 'M', 'Lv2', '2' }
    FM_DB: TM_Th_Engine_Marshal;
    FM_DB_Sequence_Pool: TZDB2_Custom_Medium_Sequence_ID_Pool;
    { Large databases support petabyte level up storage, and do not directly support traversal }
    { The database in can only run on the server side }
    { The large database is an infinite sub database support mechanism. The more sub databases, the higher the requirement for CPU }
    { Small storage block 64KB, single library storage limit 100TB }
    { Store video, compressed package, AI data set, and use Big data }
    { Type aliases in the script: 'Large', 'Big', 'Huge', 'L', 'Lv3', '3' }
    FL_DB: TL_Th_Engine_Marshal;
    FL_DB_Sequence_Pool: TZDB2_Custom_Large_Sequence_ID_Pool;
    { Extended Data Header Technology for Solving the Decentralized performance Problem of hdd }
    { When the data volume is large, a dataset contains dozens or hundreds of small databases. During disk operation, disk pre reads will be read in blocks, which greatly consumes HDD read time }
    { Expanding the data header is to gather thousands of fragmented databases and read them all at once, thereby improving the startup efficiency of the database. At the same time, pre reading also requires higher memory requirements }
    { Pre reading technology can improve data loading efficiency in the vast majority of hdd systems }
    FS_DB_Engine_External_Header_Optimzied_Technology: Boolean;
    FM_DB_Engine_External_Header_Optimzied_Technology: Boolean;
    FL_DB_Engine_External_Header_Optimzied_Technology: Boolean;

    { Interface Class }
    procedure Set_Small_Data_Class(const Value: TZDB2_Custom_Small_Data_Class);
    procedure Set_Medium_Data_Class(const Value: TZDB2_Custom_Medium_Data_Class);
    procedure Set_Large_Data_Class(const Value: TZDB2_Custom_Large_Data_Class);
    { TZDB2_Custom_Small_Data storage format }
    { 0: Int64, serialized ID, representing the sequence of data entries }
    { The following data structure Make your own decision, and when opening the database, you will use Extract_Data_Source initializes data }
    procedure Do_Th_S_DB_Data_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMS64);
    function Do_S_DB_Data_Sort_By_Sequence_ID(var L, R: TZDB2_Th_Engine_Data): Integer;
    { TZDB2_Custom_Medium_Data storage format }
    { 0: Int64, serialized ID, representing the sequence of data entries }
    { The subsequent data structure is non user modified, and the system is dead set }
    procedure Do_Th_M_DB_Data_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMem64);
    function Do_M_DB_Data_Sort_By_Sequence_ID(var L, R: TZDB2_Th_Engine_Data): Integer;
    { TZDB2_Custom_Large_Data storage format }
    { 0: Int64, serialized ID, representing the sequence of data entries }
    { The subsequent data structure is non user modified, and the system is dead set }
    procedure Do_Th_L_DB_Data_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMem64);
    function Do_L_DB_Data_Sort_By_Sequence_ID(var L, R: TZDB2_Th_Engine_Data): Integer;
  public
    { Interface Class }
    property Small_Data_Class: TZDB2_Custom_Small_Data_Class read FSmall_Data_Class write Set_Small_Data_Class;
    property Medium_Data_Class: TZDB2_Custom_Medium_Data_Class read FMedium_Data_Class write Set_Medium_Data_Class;
    property Large_Data_Class: TZDB2_Custom_Large_Data_Class read FLarge_Data_Class write Set_Large_Data_Class;
    { Small data is suitable for traversal, providing indexes for medium to large size data, and preparing for rapid expansion }
    { Small databases can be packaged and run on terminals }
    { A small database usually needs one sub database. If the number of a single database is greater than 10 million, open another database and go through a loop }
    { Small storage block of approximately 1KB, single library storage limit of 1.5TB }
    { Save logs, run device status, and use small data }
    { Type aliases in the script: 'Small', 'Tiny', 'Little', 'S',' Lv1 ',' 1 ' }
    property S_DB: TS_Th_Engine_Marshal read FS_DB;
    property S_DB_Sequence_Pool: TZDB2_Custom_Small_Sequence_ID_Pool read FS_DB_Sequence_Pool;

    { Medium data is not suitable for traversal, usually positioned around 10TB. If traversing 10TB, memory needs to reach 10TB, otherwise the efficiency is extremely low }
    { Chinese databases can be packaged and run on terminals }
    { The size of the medium data cannot exceed the number of threads/2. Based on the access density, if the density is low, such as each read/write volume is not high, then a large number of threads can be opened, up to 100 can be opened }
    { Small storage blocks are approximately 8KB, with a single library storage limit of 15TB }
    { Saving small files such as images, using data }
    { Type aliases in the script: 'Default', 'Medium', 'Middle', 'M', 'Lv2', '2' }
    property M_DB: TM_Th_Engine_Marshal read FM_DB;
    property M_DB_Sequence_Pool: TZDB2_Custom_Medium_Sequence_ID_Pool read FM_DB_Sequence_Pool;

    { Large databases support petabyte level up storage, and do not directly support traversal }
    { The database in can only run on the server side }
    { The large database is an infinite sub database support mechanism. The more sub databases, the higher the requirement for CPU }
    { Small storage block 64KB, single library storage limit 100TB }
    { Store video, compressed package, AI data set, and use Big data }
    { Type aliases in the script: 'Large', 'Big', 'Huge', 'L', 'Lv3', '3' }
    property L_DB: TL_Th_Engine_Marshal read FL_DB;
    property L_DB_Sequence_Pool: TZDB2_Custom_Large_Sequence_ID_Pool read FL_DB_Sequence_Pool;

    { Active batch Post instances }
    property Batch_Post_Num: Integer read FBatch_Post_Num;
    property Post_Batch_Num: Integer read FBatch_Post_Num;

    constructor Create(); overload;
    constructor Create(
      Small_Data_Class_: TZDB2_Custom_Small_Data_Class;
      Medium_Data_Class_: TZDB2_Custom_Medium_Data_Class;
      Large_Data_Class_: TZDB2_Custom_Large_Data_Class
      ); overload;
    constructor Create(
      Small_Data_Class_: TZDB2_Custom_Small_Data_Class;
      Medium_Data_Class_: TZDB2_Custom_Medium_Data_Class;
      Large_Data_Class_: TZDB2_Custom_Large_Data_Class;
      S_Th_Engine_Marshal_Class_: TS_Th_Engine_Marshal_Class;
      M_Th_Engine_Marshal_Class_: TM_Th_Engine_Marshal_Class;
      L_Th_Engine_Marshal_Class_: TL_Th_Engine_Marshal_Class
      ); overload;
    destructor Destroy; override;

    { Creating or opening a database using a script }
    { Root_Path stores the root directory of the database, where many database files will be stored }
    { If the database 'database' value does not specify a drive+directory, the root directory will be used to place }
    { If the database 'database' value specifies a drive+directory, the data will be stored in a specific location }
    { If the database 'database' value is empty, it will be stored directly in memory }
    procedure Build_DB_From_Script(Root_Path_: U_String; te: TTextDataEngine; OnlyRead_: Boolean); virtual;
    { Generate Script Template }
    class function Make_Script(Name_: U_String; S_DB_Num, M_DB_Num, L_DB_Num: Integer; Cipher_Security_: TCipherSecurity): TTextDataEngine;
    { Automated API: Opening a Database from a Script File }
    function Open_DB(script_conf_: U_String): Boolean; overload;
    function Open_DB(script_conf_: U_String; OnlyRead_: Boolean): Boolean; overload;
    { Automated API: Closing Database }
    procedure Close_DB;

    { Extended Data Header Technology for Solving the Decentralized performance Problem of hdd }
    { When the data volume is large, a dataset contains dozens or hundreds of small databases. During disk operation, disk pre reads will be read in blocks, which greatly consumes HDD read time }
    { Expanding the data header is to gather thousands of fragmented databases and read them all at once, thereby improving the startup efficiency of the database. At the same time, pre reading also requires higher memory requirements }
    { Pre reading technology can improve data loading efficiency in the vast majority of hdd systems }
    property S_DB_Engine_External_Header_Optimzied_Technology: Boolean read FS_DB_Engine_External_Header_Optimzied_Technology write FS_DB_Engine_External_Header_Optimzied_Technology;
    property M_DB_Engine_External_Header_Optimzied_Technology: Boolean read FM_DB_Engine_External_Header_Optimzied_Technology write FM_DB_Engine_External_Header_Optimzied_Technology;
    property L_DB_Engine_External_Header_Optimzied_Technology: Boolean read FL_DB_Engine_External_Header_Optimzied_Technology write FL_DB_Engine_External_Header_Optimzied_Technology;

    { When unlocking the database, the data items in the parallel sub database will be sorted to achieve the purpose of restoration }
    procedure Extract_S_DB(ThNum_: Integer); virtual; { Complete traversal in parallel mode, Thread safety }
    { In parallel mode, it is solved by traversing (single block storage block), Thread safety }
    procedure Extract_M_DB(ThNum_: Integer); virtual;
    procedure Extract_L_DB(ThNum_: Integer); virtual;

    { Create a small data instance and generate an associated Sequence after creation_ID, data is empty and will not be immediately posted. Here, some custom programs can be made }
    { After completing the processing, manually post: Don't forget that each data requires MD5, refer to Post_Data_The internal implementation of methods like xxx is sufficient }
    function Create_Small_Data(): TZDB2_Custom_Small_Data;
    function Create_Medium_Data(): TZDB2_Custom_Medium_Data;
    function Create_Large_Data(): TZDB2_Custom_Large_Data;
    { Save small data. After each post, the small data will trigger Extract_Data_Source }
    function Post_Data_To_S_DB(data: TMS64; AutoFree_: Boolean): TZDB2_Custom_Small_Data;
    function Post_Data_To_S_DB_C(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C): TZDB2_Custom_Small_Data;
    function Post_Data_To_S_DB_M(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M): TZDB2_Custom_Small_Data;
    function Post_Data_To_S_DB_P(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P): TZDB2_Custom_Small_Data;
    { In storage data, each post will automatically add a sequence ID+MD5 header (24 bytes) to the data }
    function Post_Data_To_M_DB(data: TMS64; AutoFree_: Boolean): TZDB2_Custom_Medium_Data;
    function Post_Data_To_M_DB_C(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C): TZDB2_Custom_Medium_Data;
    function Post_Data_To_M_DB_M(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M): TZDB2_Custom_Medium_Data;
    function Post_Data_To_M_DB_P(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P): TZDB2_Custom_Medium_Data;
    { To store Big data, each post will automatically add a sequence id+md5 header (24 byte) to the data }
    function Post_Data_To_L_DB(data: TMS64; AutoFree_: Boolean): TZDB2_Custom_Large_Data;
    function Post_Data_To_L_DB_C(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C): TZDB2_Custom_Large_Data;
    function Post_Data_To_L_DB_M(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M): TZDB2_Custom_Large_Data;
    function Post_Data_To_L_DB_P(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P): TZDB2_Custom_Large_Data;
    { Batch submission of data: Batch submission ensures integrity. If one of the batch data is incorrect, the current batch data will be deleted }
    { Batch submission of data is also the most efficient way to submit correlated data, without using waiting threads or blocking TZDB2_Th_Queue }
    function Batch_Post(): TZDB2_Custom_Batch_Data_Post_Bridge;
    function Batch_Post_C(OnResult: TZDB2_Custom_Batch_Data_Post_Bridge_Event_C): TZDB2_Custom_Batch_Data_Post_Bridge;
    function Batch_Post_M(OnResult: TZDB2_Custom_Batch_Data_Post_Bridge_Event_M): TZDB2_Custom_Batch_Data_Post_Bridge;
    function Batch_Post_P(OnResult: TZDB2_Custom_Batch_Data_Post_Bridge_Event_P): TZDB2_Custom_Batch_Data_Post_Bridge;
    procedure Wait_Batch_Post();
    { Modify data, }
    { Wait_Modify_: Waiting for modification to return }
    { AutoFree_: Whether the modification is successful or not, release the data after completion }
    procedure Modify_S_DB_Data(Inst_: TZDB2_Custom_Small_Data; data: TMS64; Wait_Modify_, AutoFree_: Boolean);
    procedure Modify_M_DB_Data(Inst_: TZDB2_Custom_Medium_Data; data: TMS64; Wait_Modify_, AutoFree_: Boolean);
    procedure Modify_L_DB_Data(Inst_: TZDB2_Custom_Large_Data; data: TMS64; Wait_Modify_, AutoFree_: Boolean);

    { Processing the recycling system, which can be executed at a high frequency }
    procedure Check_Recycle_Pool; virtual;
    { To handle the main loop, this method should avoid frequent execution. It is recommended to run it every 5 minutes and start a thread to run it, as the main loop may get stuck after too many entries }
    function Progress: Integer; virtual;
    { Perform backup }
    procedure Backup(Reserve_: Word); virtual;
    procedure Backup_If_No_Exists(); virtual;
    { Clear cache }
    procedure Flush(WaitQueue_: Boolean); virtual;
    function Flush_Is_Busy: Boolean;
    { database space state }
    function Database_Size: Int64;
    function Database_Physics_Size: Int64;
    { data num }
    function Total: NativeInt;
    { task queue }
    function QueueNum: NativeInt;
    { solved for discontinuous space. }
    function Fragment_Buffer_Num: Int64;
    function Fragment_Buffer_Memory: Int64;

    { test case }
    class procedure Do_Test_Batch_Post(Eng_: TZDB2_Large);
    class procedure Do_Test_Post(Eng_: TZDB2_Large);
    class procedure Do_Test_Get_Data(Eng_: TZDB2_Large);
    class procedure Test();
  end;

implementation

procedure TZDB2_Custom_Batch_Data_Post_Bridge.Do_Save_Data_Result(Sender: TZDB2_Th_Engine_Data; Successed: Boolean);
begin
  FCritical.Lock;
  if Successed then
    begin
      Inc(Successed_Num);
      Done_Pool.Add(Sender);
    end
  else
    begin
      Inc(Error_Num);
      if Sender.SaveFailed_Do_Remove then
          Post_Pool.Remove_T(Sender);
    end;
  FCritical.UnLock;
  Do_Check_Post_Done();
end;

procedure TZDB2_Custom_Batch_Data_Post_Bridge.Do_Check_Post_Done;
begin
  FCritical.Lock;
  if (Post_Busy <= 0) and (Successed_Num + Error_Num >= Total_Post) then
      TCompute.RunM_NP(Do_All_Post_Done);
  FCritical.UnLock;
end;

procedure TZDB2_Custom_Batch_Data_Post_Bridge.Do_All_Post_Done;
begin
  try
    if Assigned(OnResult_C) then
        OnResult_C(Self);
    if Assigned(OnResult_M) then
        OnResult_M(Self);
    if Assigned(OnResult_P) then
        OnResult_P(Self);
  except
  end;

  if (Error_Do_Remove_Data) and (Error_Num > 0) then
    begin
      if Done_Pool.Num > 0 then
        begin
          with Done_Pool.Repeat_ do
            repeat
                Queue^.data.Remove(True);
            until not Next;
        end;
    end;

  DelayFreeObj(1.0, Self);
end;

constructor TZDB2_Custom_Batch_Data_Post_Bridge.Create(Owner_Large_DB_Engine_: TZDB2_Large);
begin
  if Owner_Large_DB_Engine_ = nil then
      RaiseInfo('error');
  inherited Create;
  Owner_Large_DB_Engine_.FCritical.Inc_(Owner_Large_DB_Engine_.FBatch_Post_Num);
  FCritical := TCritical.Create;
  Total_Post := 0;
  Post_Busy := 0;
  FOwner_Large_Marshal := Owner_Large_DB_Engine_;
  Successed_Num := 0;
  Error_Num := 0;
  Error_Do_Remove_Data := True;
  Post_Pool := TZDB2_Custom_Data_Pool.Create;
  Done_Pool := TZDB2_Custom_Data_Pool.Create;
  OnResult_C := nil;
  OnResult_M := nil;
  OnResult_P := nil;
  User_Hash_Variants := THashVariantList.Create;
  User_Hash_Strings := THashStringList.Create;
end;

destructor TZDB2_Custom_Batch_Data_Post_Bridge.Destroy;
begin
  FOwner_Large_Marshal.FCritical.Dec_(FOwner_Large_Marshal.FBatch_Post_Num);
  DisposeObject(FCritical);
  DisposeObject(Post_Pool);
  DisposeObject(Done_Pool);
  DisposeObject(User_Hash_Variants);
  DisposeObject(User_Hash_Strings);
  inherited Destroy;
end;

procedure TZDB2_Custom_Batch_Data_Post_Bridge.Begin_Post;
begin
  FCritical.Inc_(Post_Busy);
end;

function TZDB2_Custom_Batch_Data_Post_Bridge.Post_Data_To_S_DB(data: TMS64; AutoFree_: Boolean): TZDB2_Custom_Small_Data;
begin
  FCritical.Inc_(Total_Post);
  Result := FOwner_Large_Marshal.Post_Data_To_S_DB_M(data, AutoFree_, Do_Save_Data_Result);
end;

function TZDB2_Custom_Batch_Data_Post_Bridge.Post_Data_To_M_DB(data: TMS64; AutoFree_: Boolean): TZDB2_Custom_Medium_Data;
begin
  FCritical.Inc_(Total_Post);
  Result := FOwner_Large_Marshal.Post_Data_To_M_DB_M(data, AutoFree_, Do_Save_Data_Result);
end;

function TZDB2_Custom_Batch_Data_Post_Bridge.Post_Data_To_L_DB(data: TMS64; AutoFree_: Boolean): TZDB2_Custom_Large_Data;
begin
  FCritical.Inc_(Total_Post);
  Result := FOwner_Large_Marshal.Post_Data_To_L_DB_M(data, AutoFree_, Do_Save_Data_Result);
end;

procedure TZDB2_Custom_Batch_Data_Post_Bridge.End_Post;
begin
  FCritical.Dec_(Post_Busy);
  Do_Check_Post_Done();
end;

procedure TZDB2_Custom_Batch_Data_Post_Bridge.Done_Post;
begin
  FCritical.Dec_(Post_Busy);
  Do_Check_Post_Done();
end;

constructor TZDB2_Custom_Small_Data.Create;
begin
  inherited Create;
  FSequence_ID := 0;
  FMD5 := Null_MD5;
  FOwner_Large_Marshal := nil;
end;

destructor TZDB2_Custom_Small_Data.Destroy;
begin
  if FOwner_Large_Marshal <> nil then
    begin
      FOwner_Large_Marshal.FS_DB_Sequence_Pool.Delete(FSequence_ID);
    end;
  inherited Destroy;
end;

procedure TZDB2_Custom_Small_Data.Do_Remove();
begin
  { Remove reverse lookup structure }
  FOwner_Large_Marshal.S_DB_Sequence_Pool.Delete(FSequence_ID);
  { Remove associated medium Big data }
  inherited Do_Remove();
end;

procedure TZDB2_Custom_Small_Data.Extract_Data_Source(Data_Source: TMS64);
begin
end;

function TZDB2_Custom_Small_Data.Encode_To_ZDB2_Data(Data_Source: TMS64; AutoFree_: Boolean): TMem64;
begin
  Result := TMem64.Create;
  Result.Size := Data_Source.Size + 24;
  Result.Position := 0;
  Result.WriteInt64(FSequence_ID);
  Result.WriteMD5(FMD5);
  Result.WritePtr(Data_Source.Memory, Data_Source.Size);
  if AutoFree_ then
      DisposeObject(Data_Source);
end;

function TZDB2_Custom_Small_Data.Decode_From_ZDB2_Data(Data_Source: TMem64; Update_: Boolean): TMS64;
begin
  if Update_ then
    begin
      Data_Source.Position := 0;
      FSequence_ID := Data_Source.ReadInt64;
      FMD5 := Data_Source.ReadMD5;
    end
  else
      Data_Source.Position := 24;
  Result := TMS64.Create;
  Result.Mapping(Data_Source.PosAsPtr, Data_Source.Size - Data_Source.Position);
  OneWay_Ready_From_External_Header;
end;

procedure TZDB2_Custom_Small_Data.Encode_External_Header_Data(External_Header_Data: TMem64);
begin
  External_Header_Data.WriteInt64(Sequence_ID);
  External_Header_Data.WriteMD5(MD5);
end;

procedure TZDB2_Custom_Small_Data.Decode_External_Header_Data(External_Header_Data: TMem64);
begin
  Sequence_ID := External_Header_Data.ReadInt64;
  MD5 := External_Header_Data.ReadMD5;
  OneWay_Ready_From_External_Header;
end;

constructor TZDB2_Custom_Medium_Data.Create;
begin
  inherited Create;
  FSequence_ID := 0;
  FMD5 := Null_MD5;
  FOwner_Large_Marshal := nil;
end;

destructor TZDB2_Custom_Medium_Data.Destroy;
begin
  if FOwner_Large_Marshal <> nil then
    begin
      FOwner_Large_Marshal.FM_DB_Sequence_Pool.Delete(FSequence_ID);
    end;
  inherited Destroy;
end;

procedure TZDB2_Custom_Medium_Data.Do_Remove;
begin
  { Remove reverse lookup structure }
  FOwner_Large_Marshal.M_DB_Sequence_Pool.Delete(FSequence_ID);
  { Remove associated data }
  inherited Do_Remove();
end;

class function TZDB2_Custom_Medium_Data.Get_Prepare_Block_Read_Size: Integer;
begin
  Result := 24;
end;

function TZDB2_Custom_Medium_Data.Encode_To_ZDB2_Data(Data_Source: TMS64; AutoFree_: Boolean): TMem64;
begin
  Result := TMem64.Create;
  Result.Size := Data_Source.Size + 24;
  Result.Position := 0;
  Result.WriteInt64(FSequence_ID);
  Result.WriteMD5(FMD5);
  Result.WritePtr(Data_Source.Memory, Data_Source.Size);
  if AutoFree_ then
      DisposeObject(Data_Source);
end;

function TZDB2_Custom_Medium_Data.Decode_From_ZDB2_Data(Data_Source: TMem64; Update_: Boolean): TMS64;
begin
  if Update_ then
    begin
      Data_Source.Position := 0;
      FSequence_ID := Data_Source.ReadInt64;
      FMD5 := Data_Source.ReadMD5;
    end
  else
      Data_Source.Position := 24;
  Result := TMS64.Create;
  Result.Mapping(Data_Source.PosAsPtr, Data_Source.Size - Data_Source.Position);
  OneWay_Ready_From_External_Header;
end;

procedure TZDB2_Custom_Medium_Data.Encode_External_Header_Data(External_Header_Data: TMem64);
begin
  External_Header_Data.WriteInt64(Sequence_ID);
  External_Header_Data.WriteMD5(MD5);
end;

procedure TZDB2_Custom_Medium_Data.Decode_External_Header_Data(External_Header_Data: TMem64);
begin
  Sequence_ID := External_Header_Data.ReadInt64;
  MD5 := External_Header_Data.ReadMD5;
  OneWay_Ready_From_External_Header;
end;

constructor TZDB2_Custom_Large_Data.Create;
begin
  inherited Create;
  FSequence_ID := 0;
  FMD5 := Null_MD5;
  FOwner_Large_Marshal := nil;
end;

destructor TZDB2_Custom_Large_Data.Destroy;
begin
  if FOwner_Large_Marshal <> nil then
    begin
      FOwner_Large_Marshal.FL_DB_Sequence_Pool.Delete(FSequence_ID);
    end;
  inherited Destroy;
end;

procedure TZDB2_Custom_Large_Data.Do_Remove;
begin
  { Remove reverse lookup structure }
  FOwner_Large_Marshal.L_DB_Sequence_Pool.Delete(FSequence_ID);
  { Remove associated data }
  inherited Do_Remove();
end;

class function TZDB2_Custom_Large_Data.Get_Prepare_Block_Read_Size: Integer;
begin
  Result := 24;
end;

function TZDB2_Custom_Large_Data.Encode_To_ZDB2_Data(Data_Source: TMS64; AutoFree_: Boolean): TMem64;
begin
  Result := TMem64.Create;
  Result.Size := Data_Source.Size + 24;
  Result.Position := 0;
  Result.WriteInt64(FSequence_ID);
  Result.WriteMD5(FMD5);
  Result.WritePtr(Data_Source.Memory, Data_Source.Size);
  if AutoFree_ then
      DisposeObject(Data_Source);
end;

function TZDB2_Custom_Large_Data.Decode_From_ZDB2_Data(Data_Source: TMem64; Update_: Boolean): TMS64;
begin
  if Update_ then
    begin
      Data_Source.Position := 0;
      FSequence_ID := Data_Source.ReadInt64;
      FMD5 := Data_Source.ReadMD5;
    end
  else
      Data_Source.Position := 24;
  Result := TMS64.Create;
  Result.Mapping(Data_Source.PosAsPtr, Data_Source.Size - Data_Source.Position);
  OneWay_Ready_From_External_Header;
end;

procedure TZDB2_Custom_Large_Data.Encode_External_Header_Data(External_Header_Data: TMem64);
begin
  External_Header_Data.WriteInt64(Sequence_ID);
  External_Header_Data.WriteMD5(MD5);
end;

procedure TZDB2_Custom_Large_Data.Decode_External_Header_Data(External_Header_Data: TMem64);
begin
  Sequence_ID := External_Header_Data.ReadInt64;
  MD5 := External_Header_Data.ReadMD5;
  OneWay_Ready_From_External_Header;
end;

procedure TS_Th_Engine_Marshal.Prepare_Flush_External_Header(Th_Engine_: TZDB2_Th_Engine; var Sequence_Table: TZDB2_BlockHandle; Flush_Instance_Pool: TZDB2_Th_Engine_Data_Instance_Pool; External_Header_Data_: TMem64);
var
  tmp: TMem64;
begin
  if Flush_Instance_Pool.Num <= 0 then
      exit;
  if not TZDB2_Large(Owner).S_DB_Engine_External_Header_Optimzied_Technology then
      exit;

  External_Header_Data_.Clear;
  External_Header_Data_.WriteInt64(Flush_Instance_Pool.Num);
  with Flush_Instance_Pool.Repeat_ do
    repeat
      External_Header_Data_.WriteInt32(Queue^.data.ID);
      tmp := TMem64.CustomCreate(1536);
      TZDB2_Custom_Small_Data(Queue^.data).Encode_External_Header_Data(tmp);
      External_Header_Data_.WriteInt32(tmp.Size);
      External_Header_Data_.WritePtr(tmp.Memory, tmp.Size);
      DisposeObject(tmp);
    until not Next;
end;

procedure TS_Th_Engine_Marshal.Do_Extract_Th_Eng(ThSender: TCompute);
var
  Eng_: TZDB2_Th_Engine;
  Error_Num: PInt64;
  num_: Int64;
  ID_: Integer;
  siz_: Integer;
  Inst_: TZDB2_Custom_Small_Data;
  tmp: TMem64;
begin
  Eng_ := ThSender.UserObject as TZDB2_Th_Engine;
  Error_Num := ThSender.UserData;

  Eng_.External_Header_Data.Position := 0;
  num_ := Eng_.External_Header_Data.ReadInt64;

  while num_ > 0 do
    begin
      ID_ := Eng_.External_Header_Data.ReadInt32;
      Inst_ := TZDB2_Custom_Small_Data(Eng_.Th_Engine_ID_Data_Pool[ID_]);
      if Inst_ = nil then
        begin
          AtomInc(Error_Num^);
          break;
        end;
      try
        Inst_.FOwner_Large_Marshal := Owner_Large_Marshal;
        siz_ := Eng_.External_Header_Data.ReadInt32;
        tmp := TMem64.Create;
        tmp.Mapping(Eng_.External_Header_Data.PosAsPtr, siz_);
        Inst_.Decode_External_Header_Data(tmp);
        DisposeObject(tmp);
        Eng_.External_Header_Data.Position := Eng_.External_Header_Data.Position + siz_;
      except
        AtomInc(Error_Num^);
        break;
      end;
      dec(num_);
    end;
end;

procedure TS_Th_Engine_Marshal.Extract_External_Header(var Extract_Done: Boolean);
var
  Error_Num: Int64;

  function Check_External_Header: NativeInt;
  begin
    { external-header optimize tech }
    Result := 0;
    if Engine_Pool.Num > 0 then
      with Engine_Pool.Repeat_ do
        repeat
          if Queue^.data.External_Header_Data.Size >= 8 then
              Inc(Result);
        until not Next;
  end;

var
  Signal_: TBool_Signal_Array;
begin
  Extract_Done := False;
  if not TZDB2_Large(Owner).S_DB_Engine_External_Header_Optimzied_Technology then
      exit;
  Error_Num := 0;
  if Check_External_Header <> Engine_Pool.Num then
      exit;
  if Engine_Pool.Num > 0 then
    begin
      SetLength(Signal_, Engine_Pool.Num);
      with Engine_Pool.Repeat_ do
        repeat
            TCompute.RunM(@Error_Num, Queue^.data, Do_Extract_Th_Eng, @Signal_[I__], nil);
        until not Next;
      Wait_All_Signal(Signal_, False);
    end;
  Extract_Done := Error_Num = 0;
end;

function TS_Th_Engine_Marshal.Begin_Custom_Build: TZDB2_Th_Engine;
begin
  Result := TZDB2_Th_Engine.Create(Self);
end;

function TS_Th_Engine_Marshal.End_Custom_Build(Eng_: TZDB2_Th_Engine): Boolean;
begin
  Eng_.Build(Current_Data_Class);
  Result := Eng_.Ready;
end;

procedure TM_Th_Engine_Marshal.Prepare_Flush_External_Header(Th_Engine_: TZDB2_Th_Engine; var Sequence_Table: TZDB2_BlockHandle; Flush_Instance_Pool: TZDB2_Th_Engine_Data_Instance_Pool; External_Header_Data_: TMem64);
var
  tmp: TMem64;
begin
  if Flush_Instance_Pool.Num <= 0 then
      exit;
  if not TZDB2_Large(Owner).M_DB_Engine_External_Header_Optimzied_Technology then
      exit;

  External_Header_Data_.Clear;
  External_Header_Data_.WriteInt64(Flush_Instance_Pool.Num);
  with Flush_Instance_Pool.Repeat_ do
    repeat
      External_Header_Data_.WriteInt32(Queue^.data.ID);
      tmp := TMem64.CustomCreate(1536);
      TZDB2_Custom_Medium_Data(Queue^.data).Encode_External_Header_Data(tmp);
      External_Header_Data_.WriteInt32(tmp.Size);
      External_Header_Data_.WritePtr(tmp.Memory, tmp.Size);
      DisposeObject(tmp);
    until not Next;
end;

procedure TM_Th_Engine_Marshal.Do_Extract_Th_Eng(ThSender: TCompute);
var
  Eng_: TZDB2_Th_Engine;
  Error_Num: PInt64;
  num_: Int64;
  ID_: Integer;
  siz_: Integer;
  Inst_: TZDB2_Custom_Medium_Data;
  tmp: TMem64;
begin
  Eng_ := ThSender.UserObject as TZDB2_Th_Engine;
  Error_Num := ThSender.UserData;

  Eng_.External_Header_Data.Position := 0;
  num_ := Eng_.External_Header_Data.ReadInt64;

  while num_ > 0 do
    begin
      ID_ := Eng_.External_Header_Data.ReadInt32;
      Inst_ := TZDB2_Custom_Medium_Data(Eng_.Th_Engine_ID_Data_Pool[ID_]);
      if Inst_ = nil then
        begin
          AtomInc(Error_Num^);
          break;
        end;
      try
        Inst_.FOwner_Large_Marshal := Owner_Large_Marshal;
        siz_ := Eng_.External_Header_Data.ReadInt32;
        tmp := TMem64.Create;
        tmp.Mapping(Eng_.External_Header_Data.PosAsPtr, siz_);
        Inst_.Decode_External_Header_Data(tmp);
        DisposeObject(tmp);
        Eng_.External_Header_Data.Position := Eng_.External_Header_Data.Position + siz_;
      except
        AtomInc(Error_Num^);
        break;
      end;
      dec(num_);
    end;
end;

procedure TM_Th_Engine_Marshal.Extract_External_Header(var Extract_Done: Boolean);
var
  Error_Num: Int64;

  function Check_External_Header: NativeInt;
  begin
    { external-header optimize tech }
    Result := 0;
    if Engine_Pool.Num > 0 then
      with Engine_Pool.Repeat_ do
        repeat
          if Queue^.data.External_Header_Data.Size >= 8 then
              Inc(Result);
        until not Next;
  end;

var
  Signal_: TBool_Signal_Array;
begin
  Extract_Done := False;
  if not TZDB2_Large(Owner).M_DB_Engine_External_Header_Optimzied_Technology then
      exit;
  Error_Num := 0;
  if Check_External_Header <> Engine_Pool.Num then
      exit;
  if Engine_Pool.Num > 0 then
    begin
      SetLength(Signal_, Engine_Pool.Num);
      with Engine_Pool.Repeat_ do
        repeat
            TCompute.RunM(@Error_Num, Queue^.data, Do_Extract_Th_Eng, @Signal_[I__], nil);
        until not Next;
      Wait_All_Signal(Signal_, False);
    end;
  Extract_Done := Error_Num = 0;
end;

function TM_Th_Engine_Marshal.Begin_Custom_Build: TZDB2_Th_Engine;
begin
  Result := TZDB2_Th_Engine.Create(Self);
end;

function TM_Th_Engine_Marshal.End_Custom_Build(Eng_: TZDB2_Th_Engine): Boolean;
begin
  Eng_.Build(Current_Data_Class);
  Result := Eng_.Ready;
end;

procedure TL_Th_Engine_Marshal.Prepare_Flush_External_Header(Th_Engine_: TZDB2_Th_Engine; var Sequence_Table: TZDB2_BlockHandle; Flush_Instance_Pool: TZDB2_Th_Engine_Data_Instance_Pool; External_Header_Data_: TMem64);
var
  tmp: TMem64;
begin
  if Flush_Instance_Pool.Num <= 0 then
      exit;
  if not TZDB2_Large(Owner).L_DB_Engine_External_Header_Optimzied_Technology then
      exit;

  External_Header_Data_.Clear;
  External_Header_Data_.WriteInt64(Flush_Instance_Pool.Num);
  with Flush_Instance_Pool.Repeat_ do
    repeat
      External_Header_Data_.WriteInt32(Queue^.data.ID);
      tmp := TMem64.CustomCreate(1536);
      TZDB2_Custom_Large_Data(Queue^.data).Encode_External_Header_Data(tmp);
      External_Header_Data_.WriteInt32(tmp.Size);
      External_Header_Data_.WritePtr(tmp.Memory, tmp.Size);
      DisposeObject(tmp);
    until not Next;
end;

procedure TL_Th_Engine_Marshal.Do_Extract_Th_Eng(ThSender: TCompute);
var
  Eng_: TZDB2_Th_Engine;
  Error_Num: PInt64;
  num_: Int64;
  ID_: Integer;
  siz_: Integer;
  Inst_: TZDB2_Custom_Large_Data;
  tmp: TMem64;
begin
  Eng_ := ThSender.UserObject as TZDB2_Th_Engine;
  Error_Num := ThSender.UserData;

  Eng_.External_Header_Data.Position := 0;
  num_ := Eng_.External_Header_Data.ReadInt64;

  while num_ > 0 do
    begin
      ID_ := Eng_.External_Header_Data.ReadInt32;
      Inst_ := TZDB2_Custom_Large_Data(Eng_.Th_Engine_ID_Data_Pool[ID_]);
      if Inst_ = nil then
        begin
          AtomInc(Error_Num^);
          break;
        end;
      try
        Inst_.FOwner_Large_Marshal := Owner_Large_Marshal;
        siz_ := Eng_.External_Header_Data.ReadInt32;
        tmp := TMem64.Create;
        tmp.Mapping(Eng_.External_Header_Data.PosAsPtr, siz_);
        Inst_.Decode_External_Header_Data(tmp);
        DisposeObject(tmp);
        Eng_.External_Header_Data.Position := Eng_.External_Header_Data.Position + siz_;
      except
        AtomInc(Error_Num^);
        break;
      end;
      dec(num_);
    end;
end;

procedure TL_Th_Engine_Marshal.Extract_External_Header(var Extract_Done: Boolean);
var
  Error_Num: Int64;

  function Check_External_Header: NativeInt;
  begin
    { external-header optimize tech }
    Result := 0;
    if Engine_Pool.Num > 0 then
      with Engine_Pool.Repeat_ do
        repeat
          if Queue^.data.External_Header_Data.Size >= 8 then
              Inc(Result);
        until not Next;
  end;

var
  Signal_: TBool_Signal_Array;
begin
  Extract_Done := False;
  if not TZDB2_Large(Owner).L_DB_Engine_External_Header_Optimzied_Technology then
      exit;
  Error_Num := 0;
  if Check_External_Header <> Engine_Pool.Num then
      exit;
  if Engine_Pool.Num > 0 then
    begin
      SetLength(Signal_, Engine_Pool.Num);
      with Engine_Pool.Repeat_ do
        repeat
            TCompute.RunM(@Error_Num, Queue^.data, Do_Extract_Th_Eng, @Signal_[I__], nil);
        until not Next;
      Wait_All_Signal(Signal_, False);
    end;
  Extract_Done := Error_Num = 0;
end;

function TL_Th_Engine_Marshal.Begin_Custom_Build: TZDB2_Th_Engine;
begin
  Result := TZDB2_Th_Engine.Create(Self);
end;

function TL_Th_Engine_Marshal.End_Custom_Build(Eng_: TZDB2_Th_Engine): Boolean;
begin
  Eng_.Build(Current_Data_Class);
  Result := Eng_.Ready;
end;

procedure TZDB2_Large.Set_Small_Data_Class(const Value: TZDB2_Custom_Small_Data_Class);
begin
  FSmall_Data_Class := Value;
  FS_DB.Current_Data_Class := FSmall_Data_Class;
end;

procedure TZDB2_Large.Set_Medium_Data_Class(const Value: TZDB2_Custom_Medium_Data_Class);
begin
  FMedium_Data_Class := Value;
  FM_DB.Current_Data_Class := FMedium_Data_Class;
end;

procedure TZDB2_Large.Set_Large_Data_Class(const Value: TZDB2_Custom_Large_Data_Class);
begin
  FLarge_Data_Class := Value;
  FL_DB.Current_Data_Class := FLarge_Data_Class;
end;

procedure TZDB2_Large.Do_Th_S_DB_Data_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMS64);
var
  Inst_: TZDB2_Custom_Small_Data;
  tmp: TMS64;
begin
  Inst_ := Sender as TZDB2_Custom_Small_Data;
  Inst_.FOwner_Large_Marshal := Self;
  tmp := Inst_.Decode_From_ZDB2_Data(IO_.Mem64, True);
  tmp.Position := 0;
  Inst_.Extract_Data_Source(tmp);
  DisposeObject(tmp);
end;

function TZDB2_Large.Do_S_DB_Data_Sort_By_Sequence_ID(var L, R: TZDB2_Th_Engine_Data): Integer;
begin
  Result := CompareInt64(TZDB2_Custom_Small_Data(L).FSequence_ID, TZDB2_Custom_Small_Data(R).FSequence_ID);
end;

procedure TZDB2_Large.Do_Th_M_DB_Data_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMem64);
var
  Inst_: TZDB2_Custom_Medium_Data;
begin
  Inst_ := Sender as TZDB2_Custom_Medium_Data;
  Inst_.FOwner_Large_Marshal := Self;
  DisposeObject(Inst_.Decode_From_ZDB2_Data(IO_, True));
end;

function TZDB2_Large.Do_M_DB_Data_Sort_By_Sequence_ID(var L, R: TZDB2_Th_Engine_Data): Integer;
begin
  Result := CompareInt64(TZDB2_Custom_Medium_Data(L).FSequence_ID, TZDB2_Custom_Medium_Data(R).FSequence_ID);
end;

procedure TZDB2_Large.Do_Th_L_DB_Data_Loaded(Sender: TZDB2_Th_Engine_Data; IO_: TMem64);
var
  Inst_: TZDB2_Custom_Large_Data;
begin
  Inst_ := Sender as TZDB2_Custom_Large_Data;
  Inst_.FOwner_Large_Marshal := Self;
  DisposeObject(Inst_.Decode_From_ZDB2_Data(IO_, True));
end;

function TZDB2_Large.Do_L_DB_Data_Sort_By_Sequence_ID(var L, R: TZDB2_Th_Engine_Data): Integer;
begin
  Result := CompareInt64(TZDB2_Custom_Large_Data(L).FSequence_ID, TZDB2_Custom_Large_Data(R).FSequence_ID);
end;

constructor TZDB2_Large.Create();
begin
  inherited Create;
  FBatch_Post_Num := 0;
  FCurrent_S_DB_Sequence_ID := 1;
  FCurrent_M_DB_Sequence_ID := 1;
  FCurrent_L_DB_Sequence_ID := 1;
  FCritical := TCritical.Create;

  FSmall_Data_Class := TZDB2_Custom_Small_Data;
  FMedium_Data_Class := TZDB2_Custom_Medium_Data;
  FLarge_Data_Class := TZDB2_Custom_Large_Data;

  FS_Th_Engine_Marshal_Class := TS_Th_Engine_Marshal;
  FM_Th_Engine_Marshal_Class := TM_Th_Engine_Marshal;
  FL_Th_Engine_Marshal_Class := TL_Th_Engine_Marshal;

  FS_DB := FS_Th_Engine_Marshal_Class.Create(Self);
  FS_DB.Current_Data_Class := FSmall_Data_Class;
  FS_DB.Owner_Large_Marshal := Self;
  FS_DB_Sequence_Pool := TZDB2_Custom_Small_Sequence_ID_Pool.Create(1024 * 1024, nil);

  FM_DB := FM_Th_Engine_Marshal_Class.Create(Self);
  FM_DB.Current_Data_Class := FMedium_Data_Class;
  FM_DB.Owner_Large_Marshal := Self;
  FM_DB_Sequence_Pool := TZDB2_Custom_Medium_Sequence_ID_Pool.Create(1024 * 1024, nil);

  FL_DB := FL_Th_Engine_Marshal_Class.Create(Self);
  FL_DB.Current_Data_Class := FLarge_Data_Class;
  FL_DB.Owner_Large_Marshal := Self;
  FL_DB_Sequence_Pool := TZDB2_Custom_Large_Sequence_ID_Pool.Create(1024 * 1024, nil);

  FS_DB_Engine_External_Header_Optimzied_Technology := False;
  FM_DB_Engine_External_Header_Optimzied_Technology := False;
  FL_DB_Engine_External_Header_Optimzied_Technology := False;
end;

constructor TZDB2_Large.Create(
  Small_Data_Class_: TZDB2_Custom_Small_Data_Class;
  Medium_Data_Class_: TZDB2_Custom_Medium_Data_Class;
  Large_Data_Class_: TZDB2_Custom_Large_Data_Class);
begin
  inherited Create;
  FBatch_Post_Num := 0;
  FCurrent_S_DB_Sequence_ID := 1;
  FCurrent_M_DB_Sequence_ID := 1;
  FCurrent_L_DB_Sequence_ID := 1;
  FCritical := TCritical.Create;

  FSmall_Data_Class := Small_Data_Class_;
  FMedium_Data_Class := Medium_Data_Class_;
  FLarge_Data_Class := Large_Data_Class_;

  FS_Th_Engine_Marshal_Class := TS_Th_Engine_Marshal;
  FM_Th_Engine_Marshal_Class := TM_Th_Engine_Marshal;
  FL_Th_Engine_Marshal_Class := TL_Th_Engine_Marshal;

  FS_DB := FS_Th_Engine_Marshal_Class.Create(Self);
  FS_DB.Current_Data_Class := FSmall_Data_Class;
  FS_DB.Owner_Large_Marshal := Self;
  FS_DB_Sequence_Pool := TZDB2_Custom_Small_Sequence_ID_Pool.Create(1024 * 1024, nil);

  FM_DB := FM_Th_Engine_Marshal_Class.Create(Self);
  FM_DB.Current_Data_Class := FMedium_Data_Class;
  FM_DB.Owner_Large_Marshal := Self;
  FM_DB_Sequence_Pool := TZDB2_Custom_Medium_Sequence_ID_Pool.Create(1024 * 1024, nil);

  FL_DB := FL_Th_Engine_Marshal_Class.Create(Self);
  FL_DB.Current_Data_Class := FLarge_Data_Class;
  FL_DB.Owner_Large_Marshal := Self;
  FL_DB_Sequence_Pool := TZDB2_Custom_Large_Sequence_ID_Pool.Create(1024 * 1024, nil);

  FS_DB_Engine_External_Header_Optimzied_Technology := False;
  FM_DB_Engine_External_Header_Optimzied_Technology := False;
  FL_DB_Engine_External_Header_Optimzied_Technology := False;
end;

constructor TZDB2_Large.Create(
  Small_Data_Class_: TZDB2_Custom_Small_Data_Class;
  Medium_Data_Class_: TZDB2_Custom_Medium_Data_Class;
  Large_Data_Class_: TZDB2_Custom_Large_Data_Class;
  S_Th_Engine_Marshal_Class_: TS_Th_Engine_Marshal_Class;
  M_Th_Engine_Marshal_Class_: TM_Th_Engine_Marshal_Class;
  L_Th_Engine_Marshal_Class_: TL_Th_Engine_Marshal_Class);
begin
  inherited Create;
  FBatch_Post_Num := 0;
  FCurrent_S_DB_Sequence_ID := 1;
  FCurrent_M_DB_Sequence_ID := 1;
  FCurrent_L_DB_Sequence_ID := 1;
  FCritical := TCritical.Create;

  FSmall_Data_Class := Small_Data_Class_;
  FMedium_Data_Class := Medium_Data_Class_;
  FLarge_Data_Class := Large_Data_Class_;

  FS_Th_Engine_Marshal_Class := S_Th_Engine_Marshal_Class_;
  FM_Th_Engine_Marshal_Class := M_Th_Engine_Marshal_Class_;
  FL_Th_Engine_Marshal_Class := L_Th_Engine_Marshal_Class_;

  FS_DB := FS_Th_Engine_Marshal_Class.Create(Self);
  FS_DB.Current_Data_Class := FSmall_Data_Class;
  FS_DB.Owner_Large_Marshal := Self;
  FS_DB_Sequence_Pool := TZDB2_Custom_Small_Sequence_ID_Pool.Create(1024 * 1024, nil);

  FM_DB := FM_Th_Engine_Marshal_Class.Create(Self);
  FM_DB.Current_Data_Class := FMedium_Data_Class;
  FM_DB.Owner_Large_Marshal := Self;
  FM_DB_Sequence_Pool := TZDB2_Custom_Medium_Sequence_ID_Pool.Create(1024 * 1024, nil);

  FL_DB := FL_Th_Engine_Marshal_Class.Create(Self);
  FL_DB.Current_Data_Class := FLarge_Data_Class;
  FL_DB.Owner_Large_Marshal := Self;
  FL_DB_Sequence_Pool := TZDB2_Custom_Large_Sequence_ID_Pool.Create(1024 * 1024, nil);

  FS_DB_Engine_External_Header_Optimzied_Technology := False;
  FM_DB_Engine_External_Header_Optimzied_Technology := False;
  FL_DB_Engine_External_Header_Optimzied_Technology := False;
end;

destructor TZDB2_Large.Destroy;
begin
  { stop copy }
  FS_DB.Stop_Copy;
  FM_DB.Stop_Copy;
  FL_DB.Stop_Copy;

  { safe flush }
  FS_DB.Flush(False);
  FM_DB.Flush(False);
  FL_DB.Flush(False);

  { wait post bridge }
  Wait_Batch_Post;

  { reset owner }
  if FS_DB.Data_Marshal.Num > 0 then
    with FS_DB.Data_Marshal.Repeat_ do
      repeat
          TZDB2_Custom_Small_Data(Queue^.data).FOwner_Large_Marshal := nil;
      until not Next;

  { reset owner }
  if FM_DB.Data_Marshal.Num > 0 then
    with FM_DB.Data_Marshal.Repeat_ do
      repeat
          TZDB2_Custom_Medium_Data(Queue^.data).FOwner_Large_Marshal := nil;
      until not Next;

  { reset owner }
  if FL_DB.Data_Marshal.Num > 0 then
    with FL_DB.Data_Marshal.Repeat_ do
      repeat
          TZDB2_Custom_Large_Data(Queue^.data).FOwner_Large_Marshal := nil;
      until not Next;

  { free db }
  DisposeObjectAndNil(FS_DB);
  DisposeObjectAndNil(FS_DB_Sequence_Pool);
  DisposeObjectAndNil(FM_DB);
  DisposeObjectAndNil(FM_DB_Sequence_Pool);
  DisposeObjectAndNil(FL_DB);
  DisposeObjectAndNil(FL_DB_Sequence_Pool);

  DisposeObjectAndNil(FCritical);
  inherited Destroy;
end;

procedure TZDB2_Large.Build_DB_From_Script(Root_Path_: U_String; te: TTextDataEngine; OnlyRead_: Boolean);
var
  L: TPascalStringList;
  i: Integer;
  HL: THashStringList;
  n: U_String;
  Eng_: TZDB2_Th_Engine;
begin
  umlCreateDirectory(Root_Path_);
  L := TPascalStringList.Create;
  te.GetSectionList(L);

  if L.Count > 0 then
    begin
      for i := 0 to L.Count - 1 do
        begin
          HL := te.HStringList[L[i]];
          { prepare database file }
          n := umlTrimSpace(HL.GetDefaultValue('database', ''));
          if (n <> '') and (not n.Exists(['/', '\'])) then
              HL.SetDefaultValue('database', umlCombineFileName(Root_Path_, n));

          { update OnlyRead }
          HL.SetDefaultText_Bool('OnlyRead', OnlyRead_);

          { extract database type }
          n := HL.GetDefaultValue('Type', '');
          if n.Same('Small', 'Tiny', 'Little', 'S', 'Lv1', '1') then
            begin
              Eng_ := TZDB2_Th_Engine.Create(FS_DB);
              Eng_.ReadConfig(L[i], HL);
              if Eng_.BlockSize < 1024 then
                  Eng_.BlockSize := 1024;
            end
          else if n.Same('Default', 'Medium', 'Middle', 'M', 'Lv2', '2') then
            begin
              Eng_ := TZDB2_Th_Engine.Create(FM_DB);
              Eng_.ReadConfig(L[i], HL);
              if Eng_.BlockSize < 4 * 1024 then
                  Eng_.BlockSize := 4 * 1024;
            end
          else if n.Same('Large', 'Big', 'Huge', 'L', 'Lv3', '3') then
            begin
              Eng_ := TZDB2_Th_Engine.Create(FL_DB);
              Eng_.ReadConfig(L[i], HL);
              if Eng_.BlockSize < 16 * 1024 then
                  Eng_.BlockSize := 16 * 1024;
            end
          else
              RaiseInfo('script error %s -> %s', [L[i].Text, n.Text]);
        end;
    end;
  { free temp }
  DisposeObject(L);

  { create or open datgabase }
  FS_DB.Build();
  FM_DB.Build();
  FL_DB.Build();
end;

class function TZDB2_Large.Make_Script(Name_: U_String; S_DB_Num, M_DB_Num, L_DB_Num: Integer; Cipher_Security_: TCipherSecurity): TTextDataEngine;
var
  tmp: TZDB2_Th_Engine_Marshal;
  i: Integer;
  Eng_: TZDB2_Th_Engine;
  HL: THashStringList;
begin
  Result := TTextDataEngine.Create;
  tmp := TZDB2_Th_Engine_Marshal.Create(nil);
  for i := 0 to S_DB_Num - 1 do
    begin
      Eng_ := TZDB2_Th_Engine.Create(tmp);
      Eng_.Name := PFormat('%s_S(%d)', [Name_.Text, i + 1]);
      Eng_.Database_File := PFormat('%s_S(%d).ZDB2', [Name_.Text, i + 1]);
      Eng_.Fast_Alloc_Space := True;
      Eng_.First_Inited_Physics_Space := 100 * 1024 * 1024;
      Eng_.Auto_Append_Space := True;
      Eng_.Delta := 100 * 1024 * 1024;
      Eng_.BlockSize := 1024;
      Eng_.Cipher_Security := Cipher_Security_;
      HL := Result.HStringList[Eng_.Name];
      HL['Type'] := 'Small';
      Eng_.WriteConfig(HL);
    end;

  for i := 0 to M_DB_Num - 1 do
    begin
      Eng_ := TZDB2_Th_Engine.Create(tmp);
      Eng_.Name := PFormat('%s_M(%d)', [Name_.Text, i + 1]);
      Eng_.Database_File := PFormat('%s_M(%d).ZDB2', [Name_.Text, i + 1]);
      Eng_.Fast_Alloc_Space := True;
      Eng_.First_Inited_Physics_Space := Int64(500) * 1024 * 1024;
      Eng_.Auto_Append_Space := True;
      Eng_.Delta := Int64(500) * 1024 * 1024;
      Eng_.BlockSize := 8 * 1024;
      Eng_.Cipher_Security := Cipher_Security_;
      HL := Result.HStringList[Eng_.Name];
      HL['Type'] := 'Medium';
      Eng_.WriteConfig(HL);
    end;

  for i := 0 to L_DB_Num - 1 do
    begin
      Eng_ := TZDB2_Th_Engine.Create(tmp);
      Eng_.Name := PFormat('%s_L(%d)', [Name_.Text, i + 1]);
      Eng_.Database_File := PFormat('%s_L(%d).ZDB2', [Name_.Text, i + 1]);
      Eng_.Fast_Alloc_Space := True;
      Eng_.First_Inited_Physics_Space := Int64(1024) * 1024 * 1024;
      Eng_.Auto_Append_Space := True;
      Eng_.Delta := Int64(1024) * 1024 * 1024;
      Eng_.BlockSize := $FFFF;
      Eng_.Cipher_Security := Cipher_Security_;
      HL := Result.HStringList[Eng_.Name];
      HL['Type'] := 'Large';
      Eng_.WriteConfig(HL);
    end;

  DisposeObject(tmp);
end;

function TZDB2_Large.Open_DB(script_conf_: U_String): Boolean;
begin
  Result := Open_DB(script_conf_, False);
end;

function TZDB2_Large.Open_DB(script_conf_: U_String; OnlyRead_: Boolean): Boolean;
var
  te: TTextDataEngine;
begin
  Result := False;
  if not umlFileExists(script_conf_) then
      exit;
  Close_DB;
  te := TTextDataEngine.Create;
  try
    te.LoadFromFile(script_conf_);
    Build_DB_From_Script(umlGetFilePath(script_conf_), te, OnlyRead_);
    Result := True;
  except
      Close_DB;
  end;
  DisposeObject(te);
end;

procedure TZDB2_Large.Close_DB;
begin
  { stop copy }
  FS_DB.Stop_Copy;
  FM_DB.Stop_Copy;
  FL_DB.Stop_Copy;

  { safe flush }
  FS_DB.Flush(False);
  FM_DB.Flush(False);
  FL_DB.Flush(False);

  { wait post bridge }
  Wait_Batch_Post;

  { reset owner }
  if FS_DB.Data_Marshal.Num > 0 then
    with FS_DB.Data_Marshal.Repeat_ do
      repeat
          TZDB2_Custom_Small_Data(Queue^.data).FOwner_Large_Marshal := nil;
      until not Next;

  { reset owner }
  if FM_DB.Data_Marshal.Num > 0 then
    with FM_DB.Data_Marshal.Repeat_ do
      repeat
          TZDB2_Custom_Medium_Data(Queue^.data).FOwner_Large_Marshal := nil;
      until not Next;

  { reset owner }
  if FL_DB.Data_Marshal.Num > 0 then
    with FL_DB.Data_Marshal.Repeat_ do
      repeat
          TZDB2_Custom_Large_Data(Queue^.data).FOwner_Large_Marshal := nil;
      until not Next;

  { free db }
  DisposeObjectAndNil(FS_DB);
  DisposeObjectAndNil(FS_DB_Sequence_Pool);
  DisposeObjectAndNil(FM_DB);
  DisposeObjectAndNil(FM_DB_Sequence_Pool);
  DisposeObjectAndNil(FL_DB);
  DisposeObjectAndNil(FL_DB_Sequence_Pool);

  { rebuild }
  FCurrent_S_DB_Sequence_ID := 1;
  FCurrent_M_DB_Sequence_ID := 1;
  FCurrent_L_DB_Sequence_ID := 1;

  FS_DB := FS_Th_Engine_Marshal_Class.Create(Self);
  FS_DB.Current_Data_Class := FSmall_Data_Class;
  FS_DB.Owner_Large_Marshal := Self;
  FS_DB_Sequence_Pool := TZDB2_Custom_Small_Sequence_ID_Pool.Create(1024 * 1024, nil);

  FM_DB := FM_Th_Engine_Marshal_Class.Create(Self);
  FM_DB.Current_Data_Class := FMedium_Data_Class;
  FM_DB.Owner_Large_Marshal := Self;
  FM_DB_Sequence_Pool := TZDB2_Custom_Medium_Sequence_ID_Pool.Create(1024 * 1024, nil);

  FL_DB := FL_Th_Engine_Marshal_Class.Create(Self);
  FL_DB.Current_Data_Class := FLarge_Data_Class;
  FL_DB.Owner_Large_Marshal := Self;
  FL_DB_Sequence_Pool := TZDB2_Custom_Large_Sequence_ID_Pool.Create(1024 * 1024, nil);
end;

procedure TZDB2_Large.Extract_S_DB(ThNum_: Integer);
var
  Extract_Done: Boolean;
begin
  FS_DB_Sequence_Pool.Clear;

  Extract_Done := False;
  if FS_DB_Engine_External_Header_Optimzied_Technology then
      FS_DB.Extract_External_Header(Extract_Done); { external-header optimize tech }
  if not Extract_Done then
    begin
      FS_DB.Parallel_Load_M(ThNum_, Do_Th_S_DB_Data_Loaded, nil);
    end;

  FCurrent_S_DB_Sequence_ID := 1;
  if FS_DB.Data_Marshal.Num > 0 then
    begin
      { Restore structure pools in order }
      DoStatus('Rebuild Small Sequence..');
      FS_DB.Sort_M(Do_S_DB_Data_Sort_By_Sequence_ID);
      FCurrent_S_DB_Sequence_ID := TZDB2_Custom_Small_Data(FS_DB.Data_Marshal.Last^.data).FSequence_ID + 1;
      { Build Sequence_ID reverse lookup structure }
      with FS_DB.Data_Marshal.Repeat_ do
        repeat
            FS_DB_Sequence_Pool.Add(TZDB2_Custom_Small_Data(Queue^.data).FSequence_ID, TZDB2_Custom_Small_Data(Queue^.data), False);
        until not Next;
    end;
end;

procedure TZDB2_Large.Extract_M_DB(ThNum_: Integer);
var
  Extract_Done: Boolean;
begin
  FM_DB_Sequence_Pool.Clear;

  Extract_Done := False;
  if FM_DB_Engine_External_Header_Optimzied_Technology then
      FM_DB.Extract_External_Header(Extract_Done); { external-header optimize tech }
  if not Extract_Done then
    begin
      FM_DB.Parallel_Block_Load_M(ThNum_, 0, 0, TZDB2_Custom_Medium_Data.Get_Prepare_Block_Read_Size, Do_Th_M_DB_Data_Loaded, nil);
    end;

  FCurrent_M_DB_Sequence_ID := 1;
  if FM_DB.Data_Marshal.Num > 0 then
    begin
      { Restore structure pools in order }
      DoStatus('Rebuild Medium Sequence..');
      FM_DB.Sort_M(Do_M_DB_Data_Sort_By_Sequence_ID);
      FCurrent_M_DB_Sequence_ID := TZDB2_Custom_Medium_Data(FM_DB.Data_Marshal.Last^.data).FSequence_ID + 1;
      { Build Sequence_ID reverse lookup structure }
      with FM_DB.Data_Marshal.Repeat_ do
        repeat
            FM_DB_Sequence_Pool.Add(TZDB2_Custom_Medium_Data(Queue^.data).FSequence_ID, TZDB2_Custom_Medium_Data(Queue^.data), False);
        until not Next;
    end;
end;

procedure TZDB2_Large.Extract_L_DB(ThNum_: Integer);
var
  Extract_Done: Boolean;
begin
  FL_DB_Sequence_Pool.Clear;

  Extract_Done := False;
  if FL_DB_Engine_External_Header_Optimzied_Technology then
      FL_DB.Extract_External_Header(Extract_Done); { external-header optimize tech }
  if not Extract_Done then
    begin
      FL_DB.Parallel_Block_Load_M(ThNum_, 0, 0, TZDB2_Custom_Large_Data.Get_Prepare_Block_Read_Size, Do_Th_L_DB_Data_Loaded, nil);
    end;

  FCurrent_L_DB_Sequence_ID := 1;
  if FL_DB.Data_Marshal.Num > 0 then
    begin
      { Restore structure pools in order }
      DoStatus('Rebuild Large Sequence..');
      FL_DB.Sort_M(Do_L_DB_Data_Sort_By_Sequence_ID);
      FCurrent_L_DB_Sequence_ID := TZDB2_Custom_Large_Data(FL_DB.Data_Marshal.Last^.data).FSequence_ID + 1;
      { Build Sequence_ID reverse lookup structure }
      with FL_DB.Data_Marshal.Repeat_ do
        repeat
            FL_DB_Sequence_Pool.Add(TZDB2_Custom_Large_Data(Queue^.data).FSequence_ID, TZDB2_Custom_Large_Data(Queue^.data), False);
        until not Next;
    end;
end;

function TZDB2_Large.Create_Small_Data: TZDB2_Custom_Small_Data;
var
  data_inst_: TZDB2_Custom_Small_Data;
begin
  FCritical.Lock;
  data_inst_ := FS_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Small_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_S_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_S_DB_Sequence_ID);
      FS_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Create_Medium_Data: TZDB2_Custom_Medium_Data;
var
  data_inst_: TZDB2_Custom_Medium_Data;
begin
  FCritical.Lock;
  data_inst_ := FM_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Medium_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_M_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_M_DB_Sequence_ID);
      FM_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Create_Large_Data: TZDB2_Custom_Large_Data;
var
  data_inst_: TZDB2_Custom_Large_Data;
begin
  FCritical.Lock;
  data_inst_ := FL_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Large_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_L_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_L_DB_Sequence_ID);
      FL_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Post_Data_To_S_DB(data: TMS64; AutoFree_: Boolean): TZDB2_Custom_Small_Data;
var
  tmp: TMem64;
  data_inst_: TZDB2_Custom_Small_Data;
begin
  FCritical.Lock;
  data_inst_ := FS_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Small_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_S_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_S_DB_Sequence_ID);
      data_inst_.FMD5 := data.ToMD5;

      data_inst_.Extract_Data_Source(data);

      { rebuild sequence memory }
      tmp := data_inst_.Encode_To_ZDB2_Data(data, False);
      data_inst_.Async_Save_And_Free_Data(tmp);
      FS_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);

      if AutoFree_ then
          DisposeObject(data);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Post_Data_To_S_DB_C(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C): TZDB2_Custom_Small_Data;
var
  tmp: TMem64;
  data_inst_: TZDB2_Custom_Small_Data;
begin
  FCritical.Lock;
  data_inst_ := FS_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Small_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_S_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_S_DB_Sequence_ID);
      data_inst_.FMD5 := data.ToMD5;

      data_inst_.Extract_Data_Source(data);

      { rebuild sequence memory }
      tmp := data_inst_.Encode_To_ZDB2_Data(data, False);
      data_inst_.Async_Save_And_Free_Data_C(tmp, OnResult);
      FS_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);

      if AutoFree_ then
          DisposeObject(data);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Post_Data_To_S_DB_M(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M): TZDB2_Custom_Small_Data;
var
  tmp: TMem64;
  data_inst_: TZDB2_Custom_Small_Data;
begin
  FCritical.Lock;
  data_inst_ := FS_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Small_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_S_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_S_DB_Sequence_ID);
      data_inst_.FMD5 := data.ToMD5;

      data_inst_.Extract_Data_Source(data);

      { rebuild sequence memory }
      tmp := data_inst_.Encode_To_ZDB2_Data(data, False);
      data_inst_.Async_Save_And_Free_Data_M(tmp, OnResult);
      FS_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);

      if AutoFree_ then
          DisposeObject(data);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Post_Data_To_S_DB_P(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P): TZDB2_Custom_Small_Data;
var
  tmp: TMem64;
  data_inst_: TZDB2_Custom_Small_Data;
begin
  FCritical.Lock;
  data_inst_ := FS_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Small_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_S_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_S_DB_Sequence_ID);
      data_inst_.FMD5 := data.ToMD5;

      data_inst_.Extract_Data_Source(data);

      { rebuild sequence memory }
      tmp := data_inst_.Encode_To_ZDB2_Data(data, False);
      data_inst_.Async_Save_And_Free_Data_P(tmp, OnResult);
      FS_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);

      if AutoFree_ then
          DisposeObject(data);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Post_Data_To_M_DB(data: TMS64; AutoFree_: Boolean): TZDB2_Custom_Medium_Data;
var
  tmp: TMem64;
  data_inst_: TZDB2_Custom_Medium_Data;
begin
  FCritical.Lock;
  data_inst_ := FM_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Medium_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_M_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_M_DB_Sequence_ID);
      data_inst_.FMD5 := data.ToMD5;

      { rebuild sequence memory }
      tmp := data_inst_.Encode_To_ZDB2_Data(data, False);
      data_inst_.Async_Save_And_Free_Data(tmp);
      FM_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);

      if AutoFree_ then
          DisposeObject(data);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Post_Data_To_M_DB_C(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C): TZDB2_Custom_Medium_Data;
var
  tmp: TMem64;
  data_inst_: TZDB2_Custom_Medium_Data;
begin
  FCritical.Lock;
  data_inst_ := FM_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Medium_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_M_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_M_DB_Sequence_ID);
      data_inst_.FMD5 := data.ToMD5;

      { rebuild sequence memory }
      tmp := data_inst_.Encode_To_ZDB2_Data(data, False);
      data_inst_.Async_Save_And_Free_Data_C(tmp, OnResult);
      FM_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);

      if AutoFree_ then
          DisposeObject(data);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Post_Data_To_M_DB_M(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M): TZDB2_Custom_Medium_Data;
var
  tmp: TMem64;
  data_inst_: TZDB2_Custom_Medium_Data;
begin
  FCritical.Lock;
  data_inst_ := FM_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Medium_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_M_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_M_DB_Sequence_ID);
      data_inst_.FMD5 := data.ToMD5;

      { rebuild sequence memory }
      tmp := data_inst_.Encode_To_ZDB2_Data(data, False);
      data_inst_.Async_Save_And_Free_Data_M(tmp, OnResult);
      FM_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);

      if AutoFree_ then
          DisposeObject(data);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Post_Data_To_M_DB_P(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P): TZDB2_Custom_Medium_Data;
var
  tmp: TMem64;
  data_inst_: TZDB2_Custom_Medium_Data;
begin
  FCritical.Lock;
  data_inst_ := FM_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Medium_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_M_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_M_DB_Sequence_ID);
      data_inst_.FMD5 := data.ToMD5;

      { rebuild sequence memory }
      tmp := data_inst_.Encode_To_ZDB2_Data(data, False);
      data_inst_.Async_Save_And_Free_Data_P(tmp, OnResult);
      FM_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);

      if AutoFree_ then
          DisposeObject(data);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Post_Data_To_L_DB(data: TMS64; AutoFree_: Boolean): TZDB2_Custom_Large_Data;
var
  tmp: TMem64;
  data_inst_: TZDB2_Custom_Large_Data;
begin
  FCritical.Lock;
  data_inst_ := FL_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Large_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_L_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_L_DB_Sequence_ID);
      data_inst_.FMD5 := data.ToMD5;

      { rebuild sequence memory }
      tmp := data_inst_.Encode_To_ZDB2_Data(data, False);
      data_inst_.Async_Save_And_Free_Data(tmp);
      FL_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);

      if AutoFree_ then
          DisposeObject(data);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Post_Data_To_L_DB_C(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_C): TZDB2_Custom_Large_Data;
var
  tmp: TMem64;
  data_inst_: TZDB2_Custom_Large_Data;
begin
  FCritical.Lock;
  data_inst_ := FL_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Large_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_L_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_L_DB_Sequence_ID);
      data_inst_.FMD5 := data.ToMD5;

      { rebuild sequence memory }
      tmp := data_inst_.Encode_To_ZDB2_Data(data, False);
      data_inst_.Async_Save_And_Free_Data_C(tmp, OnResult);
      FL_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);

      if AutoFree_ then
          DisposeObject(data);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Post_Data_To_L_DB_M(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_M): TZDB2_Custom_Large_Data;
var
  tmp: TMem64;
  data_inst_: TZDB2_Custom_Large_Data;
begin
  FCritical.Lock;
  data_inst_ := FL_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Large_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_L_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_L_DB_Sequence_ID);
      data_inst_.FMD5 := data.ToMD5;

      { rebuild sequence memory }
      tmp := data_inst_.Encode_To_ZDB2_Data(data, False);
      data_inst_.Async_Save_And_Free_Data_M(tmp, OnResult);
      FL_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);

      if AutoFree_ then
          DisposeObject(data);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Post_Data_To_L_DB_P(data: TMS64; AutoFree_: Boolean; OnResult: TOn_ZDB2_Th_Engine_Save_Data_Event_P): TZDB2_Custom_Large_Data;
var
  tmp: TMem64;
  data_inst_: TZDB2_Custom_Large_Data;
begin
  FCritical.Lock;
  data_inst_ := FL_DB.Add_Data_To_Minimize_Size_Engine as TZDB2_Custom_Large_Data;
  FCritical.UnLock;
  if data_inst_ <> nil then
    begin
      data_inst_.FOwner_Large_Marshal := Self;
      data_inst_.FSequence_ID := FCurrent_L_DB_Sequence_ID;
      FCritical.Inc_(FCurrent_L_DB_Sequence_ID);
      data_inst_.FMD5 := data.ToMD5;

      { rebuild sequence memory }
      tmp := data_inst_.Encode_To_ZDB2_Data(data, False);
      data_inst_.Async_Save_And_Free_Data_P(tmp, OnResult);
      FL_DB_Sequence_Pool.Add(data_inst_.FSequence_ID, data_inst_, False);

      if AutoFree_ then
          DisposeObject(data);
    end;
  Result := data_inst_;
end;

function TZDB2_Large.Batch_Post(): TZDB2_Custom_Batch_Data_Post_Bridge;
begin
  Result := TZDB2_Custom_Batch_Data_Post_Bridge.Create(Self);
end;

function TZDB2_Large.Batch_Post_C(OnResult: TZDB2_Custom_Batch_Data_Post_Bridge_Event_C): TZDB2_Custom_Batch_Data_Post_Bridge;
begin
  Result := TZDB2_Custom_Batch_Data_Post_Bridge.Create(Self);
  Result.OnResult_C := OnResult;
end;

function TZDB2_Large.Batch_Post_M(OnResult: TZDB2_Custom_Batch_Data_Post_Bridge_Event_M): TZDB2_Custom_Batch_Data_Post_Bridge;
begin
  Result := TZDB2_Custom_Batch_Data_Post_Bridge.Create(Self);
  Result.OnResult_M := OnResult;
end;

function TZDB2_Large.Batch_Post_P(OnResult: TZDB2_Custom_Batch_Data_Post_Bridge_Event_P): TZDB2_Custom_Batch_Data_Post_Bridge;
begin
  Result := TZDB2_Custom_Batch_Data_Post_Bridge.Create(Self);
  Result.OnResult_P := OnResult;
end;

procedure TZDB2_Large.Wait_Batch_Post;
begin
  while FBatch_Post_Num > 0 do
      TCompute.Sleep(10);
end;

procedure TZDB2_Large.Modify_S_DB_Data(Inst_: TZDB2_Custom_Small_Data; data: TMS64; Wait_Modify_, AutoFree_: Boolean);
var
  tmp: TMem64;
begin
  if Inst_.FOwner_Large_Marshal <> Self then
      RaiseInfo('error.');
  Inst_.FMD5 := data.ToMD5;
  tmp := Inst_.Encode_To_ZDB2_Data(data, False);
  if Wait_Modify_ then
    begin
      Inst_.Save_Data(tmp);
      DisposeObject(tmp);
    end
  else
    begin
      Inst_.Async_Save_And_Free_Data(tmp);
    end;
  if AutoFree_ then
      DisposeObject(data);
end;

procedure TZDB2_Large.Modify_M_DB_Data(Inst_: TZDB2_Custom_Medium_Data; data: TMS64; Wait_Modify_, AutoFree_: Boolean);
var
  tmp: TMem64;
begin
  if Inst_.FOwner_Large_Marshal <> Self then
      RaiseInfo('error.');
  Inst_.FMD5 := data.ToMD5;
  tmp := Inst_.Encode_To_ZDB2_Data(data, False);
  if Wait_Modify_ then
    begin
      Inst_.Save_Data(tmp);
      DisposeObject(tmp);
    end
  else
    begin
      Inst_.Async_Save_And_Free_Data(tmp);
    end;
  if AutoFree_ then
      DisposeObject(data);
end;

procedure TZDB2_Large.Modify_L_DB_Data(Inst_: TZDB2_Custom_Large_Data; data: TMS64; Wait_Modify_, AutoFree_: Boolean);
var
  tmp: TMem64;
begin
  if Inst_.FOwner_Large_Marshal <> Self then
      RaiseInfo('error.');
  Inst_.FMD5 := data.ToMD5;
  tmp := Inst_.Encode_To_ZDB2_Data(data, False);
  if Wait_Modify_ then
    begin
      Inst_.Save_Data(tmp);
      DisposeObject(tmp);
    end
  else
    begin
      Inst_.Async_Save_And_Free_Data(tmp);
    end;
  if AutoFree_ then
      DisposeObject(data);
end;

procedure TZDB2_Large.Check_Recycle_Pool;
begin
  FS_DB.Check_Recycle_Pool;
  FM_DB.Check_Recycle_Pool;
  FL_DB.Check_Recycle_Pool;
end;

function TZDB2_Large.Progress: Integer;
begin
  Result := 0;
  if FS_DB.Progress then
      Inc(Result);
  if FM_DB.Progress then
      Inc(Result);
  if FL_DB.Progress then
      Inc(Result);
end;

procedure TZDB2_Large.Backup(Reserve_: Word);
begin
  FS_DB.Backup(Reserve_);
  FM_DB.Backup(Reserve_);
  FL_DB.Backup(Reserve_);
end;

procedure TZDB2_Large.Backup_If_No_Exists;
begin
  FS_DB.Backup_If_No_Exists();
  FM_DB.Backup_If_No_Exists();
  FL_DB.Backup_If_No_Exists();
end;

procedure TZDB2_Large.Flush(WaitQueue_: Boolean);
begin
  FS_DB.Flush(WaitQueue_);
  FM_DB.Flush(WaitQueue_);
  FL_DB.Flush(WaitQueue_);
end;

function TZDB2_Large.Flush_Is_Busy: Boolean;
begin
  Result :=
    FS_DB.Flush_Is_Busy or
    FM_DB.Flush_Is_Busy or
    FL_DB.Flush_Is_Busy;
end;

function TZDB2_Large.Database_Size: Int64;
begin
  Result :=
    FS_DB.Database_Size +
    FM_DB.Database_Size +
    FL_DB.Database_Size;
end;

function TZDB2_Large.Database_Physics_Size: Int64;
begin
  Result :=
    FS_DB.Database_Physics_Size +
    FM_DB.Database_Physics_Size +
    FL_DB.Database_Physics_Size;
end;

function TZDB2_Large.Total: NativeInt;
begin
  Result :=
    FS_DB.Total +
    FM_DB.Total +
    FL_DB.Total;
end;

function TZDB2_Large.QueueNum: NativeInt;
begin
  Result :=
    FS_DB.QueueNum +
    FM_DB.QueueNum +
    FL_DB.QueueNum;
end;

function TZDB2_Large.Fragment_Buffer_Num: Int64;
begin
  Result :=
    FS_DB.Fragment_Buffer_Num +
    FM_DB.Fragment_Buffer_Num +
    FL_DB.Fragment_Buffer_Num;
end;

function TZDB2_Large.Fragment_Buffer_Memory: Int64;
begin
  Result :=
    FS_DB.Fragment_Buffer_Memory +
    FM_DB.Fragment_Buffer_Memory +
    FL_DB.Fragment_Buffer_Memory;
end;

class procedure TZDB2_Large.Do_Test_Batch_Post(Eng_: TZDB2_Large);
var
  batch: TZDB2_Custom_Batch_Data_Post_Bridge;
  M64: TMS64;
  i: Integer;
begin
{$IFDEF DELPHI}
  for i := 0 to 300 do
    begin
      batch := Eng_.Batch_Post_P(procedure(Sender: TZDB2_Custom_Batch_Data_Post_Bridge)
        var
          d: TDFE;
          tmp_m64: TMS64;
        begin
          if Sender.Error_Num > 0 then
              exit;
          d := TDFE.Create;
          d.WriteInt64(Sender.User_Hash_Variants.GetDefaultValue('m_id', 0));
          d.WriteString(Sender.User_Hash_Strings.GetDefaultValue('m_md5', ''));
          d.WriteInt64(Sender.User_Hash_Variants.GetDefaultValue('l_id', 0));
          d.WriteString(Sender.User_Hash_Strings.GetDefaultValue('l_md5', ''));
          tmp_m64 := TMS64.Create;
          d.FastEncodeTo(tmp_m64);
          DisposeObject(d);
          Eng_.Post_Data_To_S_DB(tmp_m64, True);
        end);

      batch.Begin_Post;
      M64 := TMS64.Create;
      M64.Size := umlRR(8192, 500 * 1024);
      batch.User_Hash_Strings['m_md5'] := umlMD5ToStr(M64.ToMD5);
      batch.User_Hash_Variants['m_id'] := batch.Post_Data_To_M_DB(M64, True).FSequence_ID;

      M64 := TMS64.Create;
      M64.Size := umlRR(1048576, 1024 * 1024 * 8);
      batch.User_Hash_Strings['l_md5'] := umlMD5ToStr(M64.ToMD5);
      batch.User_Hash_Variants['l_id'] := batch.Post_Data_To_L_DB(M64, True).FSequence_ID;
      batch.End_Post;
    end;
{$ENDIF DELPHI}
  Eng_.Wait_Batch_Post;
end;

class procedure TZDB2_Large.Do_Test_Post(Eng_: TZDB2_Large);
var
  i: Integer;
  d: TDFE;
  M64: TMS64;
  tmp_m64: TMS64;
begin
  for i := 0 to 300 do
    begin
      d := TDFE.Create;

      M64 := TMS64.Create;
      M64.Size := umlRR(8192, 500 * 1024);
      d.WriteInt64(Eng_.Post_Data_To_M_DB(M64, False).FSequence_ID);
      d.WriteString(umlMD5ToStr(M64.ToMD5));
      DisposeObject(M64);

      M64 := TMS64.Create;
      M64.Size := umlRR(1048576, 1024 * 1024 * 8);
      d.WriteInt64(Eng_.Post_Data_To_L_DB(M64, False).FSequence_ID);
      d.WriteString(umlMD5ToStr(M64.ToMD5));
      DisposeObject(M64);

      tmp_m64 := TMS64.Create;
      d.FastEncodeTo(tmp_m64);
      DisposeObject(d);
      Eng_.Post_Data_To_S_DB(tmp_m64, True);

      if i mod 10 = 0 then
          Eng_.Flush(True);
    end;
  Eng_.Flush(True);
end;

class procedure TZDB2_Large.Do_Test_Get_Data(Eng_: TZDB2_Large);
var
  num_: Integer;
begin
  num_ := 0;
{$IFDEF DELPHI}
  Eng_.S_DB.Begin_Loop;
  if Eng_.S_DB.Data_Marshal.Num > 0 then
    with Eng_.S_DB.Repeat_ do
      repeat
        while num_ > 10 do
            TCompute.Sleep(1);
        AtomInc(num_);
        Queue^.data.Async_Load_Stream_P(procedure(Sender: TZDB2_Th_Engine_Data; stream: TMS64; Successed: Boolean)
          var
            tmp: TMS64;
            Inst_: TZDB2_Custom_Small_Data;
          begin
            if not Successed then
                exit;
            Inst_ := Sender as TZDB2_Custom_Small_Data;
            tmp := Inst_.Decode_From_ZDB2_Data(stream.Mem64, False);
            if not umlCompareMD5(tmp.ToMD5, Inst_.FMD5) then
                DoStatus('md5 error.');
            DisposeObject(tmp);
            AtomDec(num_);
          end);
      until not Next;
  Eng_.S_DB.End_Loop;
  while num_ > 0 do
      TCompute.Sleep(1);

  Eng_.M_DB.Begin_Loop;
  if Eng_.M_DB.Data_Marshal.Num > 0 then
    with Eng_.M_DB.Repeat_ do
      repeat
        while num_ > 10 do
            TCompute.Sleep(1);
        AtomInc(num_);
        Queue^.data.Async_Load_Stream_P(procedure(Sender: TZDB2_Th_Engine_Data; stream: TMS64; Successed: Boolean)
          var
            tmp: TMS64;
            Inst_: TZDB2_Custom_Medium_Data;
          begin
            if not Successed then
                exit;
            Inst_ := Sender as TZDB2_Custom_Medium_Data;
            tmp := Inst_.Decode_From_ZDB2_Data(stream.Mem64, False);
            if not umlCompareMD5(tmp.ToMD5, Inst_.FMD5) then
                DoStatus('md5 error.');
            DisposeObject(tmp);
            AtomDec(num_);
          end);
      until not Next;
  Eng_.M_DB.End_Loop;
  while num_ > 0 do
      TCompute.Sleep(1);

  Eng_.L_DB.Begin_Loop;
  if Eng_.L_DB.Data_Marshal.Num > 0 then
    with Eng_.L_DB.Repeat_ do
      repeat
        while num_ > 10 do
            TCompute.Sleep(1);
        AtomInc(num_);
        Queue^.data.Async_Load_Stream_P(procedure(Sender: TZDB2_Th_Engine_Data; stream: TMS64; Successed: Boolean)
          var
            tmp: TMS64;
            Inst_: TZDB2_Custom_Large_Data;
          begin
            if not Successed then
                exit;
            Inst_ := Sender as TZDB2_Custom_Large_Data;
            tmp := Inst_.Decode_From_ZDB2_Data(stream.Mem64, False);
            if not umlCompareMD5(tmp.ToMD5, Inst_.FMD5) then
                DoStatus('md5 error.');
            DisposeObject(tmp);
            AtomDec(num_);
          end);
      until not Next;
  Eng_.L_DB.End_Loop;
  while num_ > 0 do
      TCompute.Sleep(1);
{$ENDIF DELPHI}
end;

class procedure TZDB2_Large.Test;
var
  Eng_: TZDB2_Large;
  te: TTextDataEngine;
begin
  Eng_ := TZDB2_Large.Create;
  { make script templet }
  te := Eng_.Make_Script('test', 2, 2, 2, TCipherSecurity.csNone);
  Eng_.Build_DB_From_Script(umlCombinePath(umlCurrentPath, 'ZDB2_large_DB_Test'), te, False);
  DisposeObject(te);
  Eng_.S_DB_Engine_External_Header_Optimzied_Technology := True;
  Eng_.M_DB_Engine_External_Header_Optimzied_Technology := True;
  Eng_.L_DB_Engine_External_Header_Optimzied_Technology := True;

  Eng_.Extract_S_DB(10);
  Eng_.Extract_M_DB(10);
  Eng_.Extract_L_DB(10);

  if False then
      Do_Test_Batch_Post(Eng_);

  if False then
      Do_Test_Post(Eng_);

  if True then
      Do_Test_Get_Data(Eng_);

  DisposeObject(Eng_);
end;

end.
