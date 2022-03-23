{ ****************************************************************************** }
{ * cloud 4.0 NetDisk virtual directory                                        * }
{ ****************************************************************************** }
unit Z.Net.C4_NetDisk_Directory;

{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine,
  Z.Geometry2D, Z.DFE, Z.Json, Z.Expression,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.ZDB2, Z.ZDB2.ObjectDataManager, Z.ZDB2.DFE,
  Z.ZDB.ObjectData_LIB, Z.ZDB, Z.ZDB.ItemStream_LIB,
  Z.GHashList,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth,
  Z.Net.C4;

type
{$REGION 'service struct define'}
  TC40_NetDisk_Directory_Service = class;
  TDirectory_Service_User_File_DB = class;

  TDirectory_MD5_Data_Frag_Struct = record
    FS_AliasOrHash: U_String; // C4 alias or Hash
    FS_File: U_String;        // file name
    Pos_: Int64;              // position info
    Size_: Int64;             // size info
  end;

  PDirectory_MD5_Data_Frag_Struct = ^TDirectory_MD5_Data_Frag_Struct;

  TDirectory_MD5_Data_Frag_Struct_List_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PDirectory_MD5_Data_Frag_Struct>;

  TDirectory_MD5_Data_Frag_Struct_List = class(TDirectory_MD5_Data_Frag_Struct_List_Decl)
  public
    MD5: TMD5;
    Size: Int64;
    Time_: Double;
    Ref_: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Clean;
    function TotalFragSize(): Int64;
    procedure SortPos;
    procedure Encode(d: TDFE);
    procedure Decode(d: TDFE; Pool_: Boolean);
  end;

  TDirectory_Service_MD5_Data_Frag = class
  public
    Owner: TC40_NetDisk_Directory_Service;
    Stream: TZDB2_DFE;
    MD5: TMD5;
    Size: Int64;
    Time_: Double;
    Ref_: Integer;
    constructor Create(Owner_: TC40_NetDisk_Directory_Service; Stream_: TZDB2_DFE);
    destructor Destroy; override;
    procedure ReadInfo;
  end;

  TDirectory_Service_MD5_DataPool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericHashList<TDirectory_Service_MD5_Data_Frag>;

  TDirectory_Service_User_File_DB = class
  public
    Owner: TC40_NetDisk_Directory_Service;
    Stream: TZDB2_ObjectDataManager;
    DB_Name: U_String;
    Field_Num: Int64;
    Item_Num: Int64;
    ItemSpace: Int64;
    IsChanged: Boolean;
    FragSpaceUpdateTime: TTimeTick;
    constructor Create(Owner_: TC40_NetDisk_Directory_Service; Stream_: TZDB2_ObjectDataManager);
    destructor Destroy; override;
    procedure ComputeFragSpace;
  end;

  TDirectory_Service_User_File_DB_Pool = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericHashList<TDirectory_Service_User_File_DB>;

  TTemp_Compute_SpaceInfo = class
  public
    Service: TC40_NetDisk_Directory_Service;
    DB_Name: U_String;
    Field_Num: Int64;
    Item_Num: Int64;
    ItemSpace: Int64;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Compute_SpaceInfo(ThSender: THPC_Stream; ThInData, ThOutData: TDFE);
    procedure Do_Compute_SpaceInfo_Done(ThSender: THPC_Stream; IO: TPeerIO; ThInData, ThOutData: TDFE);
  end;
{$ENDREGION 'service struct define'}

  TC40_NetDisk_Directory_Service = class(TC40_Base_NoAuth_Service)
  protected
    procedure cmd_NewDB(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_RemoveDB(Sender: TPeerIO; InData: TDFE);
    procedure cmd_GetItemList(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_GetItemFrag(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_FoundMD5(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_PutItemFrag(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_PutItemMD5(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_RemoveField(Sender: TPeerIO; InData: TDFE);
    procedure cmd_RemoveItem(Sender: TPeerIO; InData: TDFE);
    procedure cmd_NewField(Sender: TPeerIO; InData: TDFE);
    procedure cmd_SpaceInfo(Sender: TPeerIO; InData, OutData: TDFE);
  public
    // directory
    Directory_ZDB2_RecycleMemoryTimeOut: TTimeTick;
    Directory_ZDB2_DeltaSpace: Int64;
    Directory_ZDB2_BlockSize: Word;
    Directory_ZDB2_EnabledCipher: Boolean;
    Directory_ZDB2_CipherName: U_String;
    Directory_ZDB2_Password: U_String;
    Directory_ZDB2_Cipher: TZDB2_Cipher;
    C40_Directory_Database_File: U_String;
    Directory_HashPool: TDirectory_Service_User_File_DB_Pool;
    Directory_Database: TZDB2_List_ObjectDataManager;
    // md5
    MD5_ZDB2_RecycleMemoryTimeOut: TTimeTick;
    MD5_ZDB2_DeltaSpace: Int64;
    MD5_ZDB2_BlockSize: Word;
    MD5_ZDB2_EnabledCipher: Boolean;
    MD5_ZDB2_CipherName: U_String;
    MD5_ZDB2_Password: U_String;
    MD5_ZDB2_Cipher: TZDB2_Cipher;
    C40_MD5_Database_File: U_String;
    MD5_Pool: TDirectory_Service_MD5_DataPool;
    MD5_Database: TZDB2_List_DFE;

    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
  end;

{$REGION 'bridge define'}

  TC40_NetDisk_Directory_Client = class;

  TON_NewDB_C = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString);
  TON_NewDB_M = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString) of object;
{$IFDEF FPC}
  TON_NewDB_P = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString) is nested;
{$ELSE FPC}
  TON_NewDB_P = reference to procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString);
{$ENDIF FPC}

  TON_Temp_NewDB = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_Directory_Client;
    OnResultC: TON_NewDB_C;
    OnResultM: TON_NewDB_M;
    OnResultP: TON_NewDB_P;
    constructor Create; override;
    procedure DoStreamEvent(Sender: TPeerIO; Result_: TDFE); override;
  end;

  TItemList_Data = record
    Name: U_String;
    Num: Int64;
    Time_: TDateTime;
  end;

  TItemList_Data_Array = array of TItemList_Data;

  TON_GetItemList_C = procedure(Sender: TC40_NetDisk_Directory_Client; arry: TItemList_Data_Array);
  TON_GetItemList_M = procedure(Sender: TC40_NetDisk_Directory_Client; arry: TItemList_Data_Array) of object;
{$IFDEF FPC}
  TON_GetItemList_P = procedure(Sender: TC40_NetDisk_Directory_Client; arry: TItemList_Data_Array) is nested;
{$ELSE FPC}
  TON_GetItemList_P = reference to procedure(Sender: TC40_NetDisk_Directory_Client; arry: TItemList_Data_Array);
{$ENDIF FPC}

  TON_Temp_GetItemList = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_Directory_Client;
    OnResultC: TON_GetItemList_C;
    OnResultM: TON_GetItemList_M;
    OnResultP: TON_GetItemList_P;
    constructor Create; override;
    procedure DoStreamEvent(Sender: TPeerIO; Result_: TDFE); override;
  end;

  TON_GetItemFrag_C = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; L: TDirectory_MD5_Data_Frag_Struct_List);
  TON_GetItemFrag_M = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; L: TDirectory_MD5_Data_Frag_Struct_List) of object;
{$IFDEF FPC}
  TON_GetItemFrag_P = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; L: TDirectory_MD5_Data_Frag_Struct_List) is nested;
{$ELSE FPC}
  TON_GetItemFrag_P = reference to procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; L: TDirectory_MD5_Data_Frag_Struct_List);
{$ENDIF FPC}

  TON_Temp_GetItemFrag = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_Directory_Client;
    OnResultC: TON_GetItemFrag_C;
    OnResultM: TON_GetItemFrag_M;
    OnResultP: TON_GetItemFrag_P;
    constructor Create; override;
    procedure DoStreamEvent(Sender: TPeerIO; Result_: TDFE); override;
  end;

  TON_FoundMD5_C = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean);
  TON_FoundMD5_M = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean) of object;
{$IFDEF FPC}
  TON_FoundMD5_P = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean) is nested;
{$ELSE FPC}
  TON_FoundMD5_P = reference to procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean);
{$ENDIF FPC}

  TON_Temp_FoundMD5 = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_Directory_Client;
    OnResultC: TON_FoundMD5_C;
    OnResultM: TON_FoundMD5_M;
    OnResultP: TON_FoundMD5_P;
    constructor Create; override;
    procedure DoStreamEvent(Sender: TPeerIO; Result_: TDFE); override;
  end;

  TON_PutItemFrag_C = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString);
  TON_PutItemFrag_M = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString) of object;
{$IFDEF FPC}
  TON_PutItemFrag_P = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString) is nested;
{$ELSE FPC}
  TON_PutItemFrag_P = reference to procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString);
{$ENDIF FPC}

  TON_Temp_PutItemFrag = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_Directory_Client;
    OnResultC: TON_PutItemFrag_C;
    OnResultM: TON_PutItemFrag_M;
    OnResultP: TON_PutItemFrag_P;
    constructor Create; override;
    procedure DoStreamEvent(Sender: TPeerIO; Result_: TDFE); override;
  end;

  TON_PutItemMD5_C = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString);
  TON_PutItemMD5_M = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString) of object;
{$IFDEF FPC}
  TON_PutItemMD5_P = procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString) is nested;
{$ELSE FPC}
  TON_PutItemMD5_P = reference to procedure(Sender: TC40_NetDisk_Directory_Client; Successed: Boolean; info: SystemString);
{$ENDIF FPC}

  TON_Temp_PutItemMD5 = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_Directory_Client;
    OnResultC: TON_PutItemMD5_C;
    OnResultM: TON_PutItemMD5_M;
    OnResultP: TON_PutItemMD5_P;
    constructor Create; override;
    procedure DoStreamEvent(Sender: TPeerIO; Result_: TDFE); override;
  end;

  TON_SpaceInfo_C = procedure(Sender: TC40_NetDisk_Directory_Client; Field_Num, Item_Num, ItemSpace: Int64);
  TON_SpaceInfo_M = procedure(Sender: TC40_NetDisk_Directory_Client; Field_Num, Item_Num, ItemSpace: Int64) of object;
{$IFDEF FPC}
  TON_SpaceInfo_P = procedure(Sender: TC40_NetDisk_Directory_Client; Field_Num, Item_Num, ItemSpace: Int64) is nested;
{$ELSE FPC}
  TON_SpaceInfo_P = reference to procedure(Sender: TC40_NetDisk_Directory_Client; Field_Num, Item_Num, ItemSpace: Int64);
{$ENDIF FPC}

  TON_Temp_SpaceInfo = class(TOnResultBridge)
  public
    Client: TC40_NetDisk_Directory_Client;
    OnResultC: TON_SpaceInfo_C;
    OnResultM: TON_SpaceInfo_M;
    OnResultP: TON_SpaceInfo_P;
    constructor Create; override;
    procedure DoStreamEvent(Sender: TPeerIO; Result_: TDFE); override;
  end;

{$ENDREGION 'bridge define'}

  TC40_NetDisk_Directory_Client = class(TC40_Base_NoAuth_Client)
  public
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;

    // new database
    procedure NewDB_C(DB_Name: U_String; OnResult: TON_NewDB_C);
    procedure NewDB_M(DB_Name: U_String; OnResult: TON_NewDB_M);
    procedure NewDB_P(DB_Name: U_String; OnResult: TON_NewDB_P);

    // remove database
    procedure RemoveDB(DB_Name: U_String);

    // list
    procedure GetItemList_C(DB_Name, DB_Field: U_String; OnResult: TON_GetItemList_C);
    procedure GetItemList_M(DB_Name, DB_Field: U_String; OnResult: TON_GetItemList_M);
    procedure GetItemList_P(DB_Name, DB_Field: U_String; OnResult: TON_GetItemList_P);

    // frag
    procedure GetItemFrag_C(DB_Name, DB_Field, DB_Item: U_String; OnResult: TON_GetItemFrag_C);
    procedure GetItemFrag_M(DB_Name, DB_Field, DB_Item: U_String; OnResult: TON_GetItemFrag_M);
    procedure GetItemFrag_P(DB_Name, DB_Field, DB_Item: U_String; OnResult: TON_GetItemFrag_P);

    // found md5
    procedure FoundMD5_C(frag_md5_name: U_String; OnResult: TON_FoundMD5_C);
    procedure FoundMD5_M(frag_md5_name: U_String; OnResult: TON_FoundMD5_M);
    procedure FoundMD5_P(frag_md5_name: U_String; OnResult: TON_FoundMD5_P);

    // put frag
    procedure PutItemFrag_C(DB_Name, DB_Field, DB_Item: U_String; L: TDirectory_MD5_Data_Frag_Struct_List; OnResult: TON_PutItemFrag_C);
    procedure PutItemFrag_M(DB_Name, DB_Field, DB_Item: U_String; L: TDirectory_MD5_Data_Frag_Struct_List; OnResult: TON_PutItemFrag_M);
    procedure PutItemFrag_P(DB_Name, DB_Field, DB_Item: U_String; L: TDirectory_MD5_Data_Frag_Struct_List; OnResult: TON_PutItemFrag_P);

    // put md5
    procedure PutItemMD5_C(DB_Name, DB_Field, DB_Item, frag_md5_name: U_String; OnResult: TON_PutItemMD5_C);
    procedure PutItemMD5_M(DB_Name, DB_Field, DB_Item, frag_md5_name: U_String; OnResult: TON_PutItemMD5_M);
    procedure PutItemMD5_P(DB_Name, DB_Field, DB_Item, frag_md5_name: U_String; OnResult: TON_PutItemMD5_P);

    // remove field
    procedure RemoveField(DB_Name, DB_Field, DB_Remove_Field_: U_String);

    // remove item
    procedure RemoveItem(DB_Name, DB_Field, DB_Remove_Item_: U_String);

    // new field
    procedure NewField(DB_Name, DB_Field: U_String);

    // space
    procedure SpaceInfo_C(DB_Name: U_String; OnResult: TON_SpaceInfo_C);
    procedure SpaceInfo_M(DB_Name: U_String; OnResult: TON_SpaceInfo_M);
    procedure SpaceInfo_P(DB_Name: U_String; OnResult: TON_SpaceInfo_P);
  end;

implementation

constructor TDirectory_MD5_Data_Frag_Struct_List.Create;
begin
  inherited Create;
  MD5 := NullMD5;
  Size := 0;
  Time_ := umlNow;
  Ref_ := 0;
end;

destructor TDirectory_MD5_Data_Frag_Struct_List.Destroy;
begin
  Clean;
  inherited Destroy;
end;

procedure TDirectory_MD5_Data_Frag_Struct_List.Clean;
var
  i: Integer;
  p: PDirectory_MD5_Data_Frag_Struct;
begin
  for i := 0 to count - 1 do
    begin
      p := items[i];
      p^.FS_AliasOrHash := '';
      p^.FS_File := '';
      Dispose(p);
    end;
  inherited Clear;
end;

function TDirectory_MD5_Data_Frag_Struct_List.TotalFragSize(): Int64;
var
  i: Integer;
  p: PDirectory_MD5_Data_Frag_Struct;
begin
  Result := 0;
  for i := 0 to count - 1 do
    begin
      p := items[i];
      inc(Result, p^.Size_);
    end;
end;

procedure TDirectory_MD5_Data_Frag_Struct_List.SortPos;

  function Compare_(Left, Right: PDirectory_MD5_Data_Frag_Struct): ShortInt;
  begin
    Result := CompareInt64(Left^.Pos_, Right^.Pos_);
  end;

  procedure fastSort_(L, R: Integer);
  var
    i, j: TGeoInt;
    p: PDirectory_MD5_Data_Frag_Struct;
  begin
    repeat
      i := L;
      j := R;
      p := items[(L + R) shr 1];
      repeat
        while Compare_(items[i], p) < 0 do
            inc(i);
        while Compare_(items[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
                Exchange(i, j);
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          fastSort_(L, j);
      L := i;
    until i >= R;
  end;

begin
  if count > 1 then
      fastSort_(0, count - 1);
end;

procedure TDirectory_MD5_Data_Frag_Struct_List.Encode(d: TDFE);
var
  i: Integer;
  p: PDirectory_MD5_Data_Frag_Struct;
begin
  d.WriteMD5(MD5);
  d.WriteInt64(Size);
  d.WriteDouble(Time_);
  d.WriteInteger(Ref_);
  for i := 0 to count - 1 do
    begin
      p := items[i];
      d.WriteString(p^.FS_AliasOrHash);
      d.WriteString(p^.FS_File);
      d.WriteInt64(p^.Pos_);
      d.WriteInt64(p^.Size_);
    end;
end;

procedure TDirectory_MD5_Data_Frag_Struct_List.Decode(d: TDFE; Pool_: Boolean);
var
  p: PDirectory_MD5_Data_Frag_Struct;
begin
  Clean;
  MD5 := d.R.ReadMD5;
  Size := d.R.ReadInt64;
  Time_ := d.R.ReadDouble;
  Ref_ := d.R.ReadInteger;
  if Pool_ then
    while d.R.NotEnd do
      begin
        new(p);
        p^.FS_AliasOrHash := d.R.ReadString;
        p^.FS_File := d.R.ReadString;
        p^.Pos_ := d.R.ReadInt64;
        p^.Size_ := d.R.ReadInt64;
        Add(p);
      end;
end;

constructor TDirectory_Service_MD5_Data_Frag.Create(Owner_: TC40_NetDisk_Directory_Service; Stream_: TZDB2_DFE);
begin
  inherited Create;
  Owner := Owner_;
  Stream := Stream_;
  MD5 := NullMD5;
  Size := 0;
  Time_ := umlNow;
  Ref_ := 0;
end;

destructor TDirectory_Service_MD5_Data_Frag.Destroy;
begin
  if (Owner.MD5_Database <> nil) and (Stream <> nil) then
      Owner.MD5_Database.Remove(Stream, True);
  inherited Destroy;
end;

procedure TDirectory_Service_MD5_Data_Frag.ReadInfo;
var
  L: TDirectory_MD5_Data_Frag_Struct_List;
begin
  L := TDirectory_MD5_Data_Frag_Struct_List.Create;
  Stream.Data.R.Index := 0;
  L.Decode(Stream.Data, False);
  MD5 := L.MD5;
  Size := L.Size;
  Time_ := L.Time_;
  Ref_ := L.Ref_;
  disposeObject(L);
end;

constructor TDirectory_Service_User_File_DB.Create(Owner_: TC40_NetDisk_Directory_Service; Stream_: TZDB2_ObjectDataManager);
begin
  inherited Create;
  Owner := Owner_;
  Stream := Stream_;
  DB_Name := '';
  Field_Num := 0;
  Item_Num := 0;
  ItemSpace := 0;
  IsChanged := False;
  FragSpaceUpdateTime := 0;
end;

destructor TDirectory_Service_User_File_DB.Destroy;
begin
  DB_Name := '';
  if (Owner.Directory_Database <> nil) and (Stream <> nil) then
      Owner.Directory_Database.Remove(Stream, True);
  inherited Destroy;
end;

procedure TDirectory_Service_User_File_DB.ComputeFragSpace;
var
  ir: TItemRecursionSearch;
  itm_stream: TItemStream;
  Size_: Int64;
begin
  if not IsChanged then
      exit;
  Field_Num := 0;
  Item_Num := 0;
  ItemSpace := 0;
  if Stream.Data.RecursionSearchFirst('/', '*', ir) then
    begin
      repeat
        case ir.ReturnHeader.ID of
          DB_Header_Field_ID: inc(Field_Num);
          DB_Header_Item_ID:
            begin
              inc(Item_Num);
              itm_stream := TItemStream.Create(Stream.Data, ir.ReturnHeader.CurrentHeader);
              try
                StreamIgnoreReadString(itm_stream);
                inc(ItemSpace, StreamReadInt64(itm_stream));
              except
              end;
              disposeObject(itm_stream);
            end;
        end;
      until not Stream.Data.RecursionSearchNext(ir);
    end;
  IsChanged := False;
  FragSpaceUpdateTime := GetTimeTick();
end;

procedure TC40_NetDisk_Directory_Service.cmd_NewDB(Sender: TPeerIO; InData, OutData: TDFE);
var
  DB_Name: U_String;
  fd: TDirectory_Service_User_File_DB;
begin
  DB_Name := InData.R.ReadString;
  if Directory_HashPool.Exists(DB_Name) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('repeat DB: %s', [DB_Name.Text]);
      exit;
    end;

  if not Test_Reserved_String(DB_Name) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('name so long DB: %s', [DB_Name.Text]);
      exit;
    end;

  fd := TDirectory_Service_User_File_DB.Create(self, Directory_Database.NewData);
  fd.DB_Name := DB_Name;
  fd.Stream.Data.Reserved := String_To_Reserved(DB_Name);
  fd.Stream.Data.UpdateIO;
  fd.Stream.Save;
  fd.IsChanged := True;
  Directory_HashPool.Add(fd.DB_Name, fd);

  OutData.WriteBool(True);
  OutData.WriteString('New DB done: %s', [DB_Name.Text]);
end;

procedure TC40_NetDisk_Directory_Service.cmd_RemoveDB(Sender: TPeerIO; InData: TDFE);
var
  DB_Name: U_String;
begin
  DB_Name := InData.R.ReadString;
  Directory_HashPool.Delete(DB_Name);
end;

procedure TC40_NetDisk_Directory_Service.cmd_GetItemList(Sender: TPeerIO; InData, OutData: TDFE);
var
  DB_Name: U_String;
  DB_Field: U_String;
  fd: TDirectory_Service_User_File_DB;
  fr: TFieldSearch;
  ir: TItemSearch;
  itm_stream: TItemStream;
  md5_name_: U_String;
  Size_: Int64;
begin
  DB_Name := InData.R.ReadString;
  DB_Field := InData.R.ReadString;
  fd := Directory_HashPool[DB_Name];
  if fd = nil then
    begin
      // create file db
      fd := TDirectory_Service_User_File_DB.Create(self, Directory_Database.NewData);
      fd.Stream.Data.Reserved := String_To_Reserved(DB_Name);
      fd.DB_Name := Reserved_To_String(fd.Stream.Data.Reserved);
      Directory_HashPool.Add(fd.DB_Name, fd);
      fd.Stream.Save;
    end;

  if fd.Stream.Data.FieldFindFirst(DB_Field, '*', fr) then
    begin
      repeat
        OutData.WriteString('f:%s', [fr.Name.Text]);                  // field
        OutData.WriteInt64(fr.HeaderCount);                           // children
        OutData.WriteDouble(fr.FieldSearch.RHeader.ModificationTime); // time
      until not fd.Stream.Data.FieldFindNext(fr);
    end;
  if fd.Stream.Data.ItemFindFirst(DB_Field, '*', ir) then
    begin
      repeat
        itm_stream := TItemStream.Create(fd.Stream.Data, ir.HeaderPOS);
        md5_name_ := StreamReadString(itm_stream);
        Size_ := StreamReadInt64(itm_stream);
        disposeObject(itm_stream);
        OutData.WriteString('i:%s|%s', [ir.Name.Text, md5_name_.Text]); // item
        OutData.WriteInt64(Size_);                                      // size
        OutData.WriteDouble(ir.FieldSearch.RHeader.ModificationTime);   // time
      until not fd.Stream.Data.ItemFindNext(ir);
    end;
end;

procedure TC40_NetDisk_Directory_Service.cmd_GetItemFrag(Sender: TPeerIO; InData, OutData: TDFE);
var
  DB_Name: U_String;
  DB_Field: U_String;
  DB_Item: U_String;
  fd: TDirectory_Service_User_File_DB;
  itmHnd: TItemHandle;
  itm_stream: TItemStream;
  md5_name_: U_String;
  Size_: Int64;
  md5_frag: TDirectory_Service_MD5_Data_Frag;
begin
  DB_Name := InData.R.ReadString;
  DB_Field := InData.R.ReadString;
  DB_Item := InData.R.ReadString;

  fd := Directory_HashPool[DB_Name];
  if fd = nil then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found db: %s', [DB_Name.Text]);
      exit;
    end;

  if fd.Stream.Data.ItemOpen(DB_Field, DB_Item, itmHnd) then
    begin
      itm_stream := TItemStream.Create(fd.Stream.Data, itmHnd);
      md5_name_ := StreamReadString(itm_stream);
      Size_ := StreamReadInt64(itm_stream);
      md5_frag := MD5_Pool[md5_name_];
      if md5_frag <> nil then
        begin
          OutData.WriteBool(True);
          OutData.WriteDataFrame(md5_frag.Stream.Data);
        end
      else
        begin
          OutData.WriteBool(False);
          OutData.WriteString('loss frag: %s:%s/%s', [DB_Name.Text, DB_Field.Text, DB_Item.Text]);
        end;
      disposeObject(itm_stream);
    end
  else
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found: %s:%s/%s', [DB_Name.Text, DB_Field.Text, DB_Item.Text]);
    end;
end;

procedure TC40_NetDisk_Directory_Service.cmd_FoundMD5(Sender: TPeerIO; InData, OutData: TDFE);
var
  frag_md5_name: U_String;
begin
  frag_md5_name := InData.R.ReadString;
  OutData.WriteBool(MD5_Pool.Exists(frag_md5_name));
end;

procedure TC40_NetDisk_Directory_Service.cmd_PutItemFrag(Sender: TPeerIO; InData, OutData: TDFE);
var
  DB_Name: U_String;
  DB_Field: U_String;
  DB_Item: U_String;
  frag_md5_name: U_String;
  fd: TDirectory_Service_User_File_DB;
  md5_frag: TDirectory_Service_MD5_Data_Frag;
  itmHnd: TItemHandle;
  itm_stream: TItemStream;
begin
  DB_Name := InData.R.ReadString;
  DB_Field := InData.R.ReadString;
  DB_Item := InData.R.ReadString;
  frag_md5_name := InData.R.ReadString;

  if MD5_Pool.Exists(frag_md5_name) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('%s frag is repeat md5: %s', [DB_Item.Text, frag_md5_name.Text]);
      exit;
    end;

  fd := Directory_HashPool[DB_Name];
  if fd = nil then
    begin
      // create file db
      fd := TDirectory_Service_User_File_DB.Create(self, Directory_Database.NewData);
      fd.Stream.Data.Reserved := String_To_Reserved(DB_Name);
      fd.DB_Name := Reserved_To_String(fd.Stream.Data.Reserved);
      Directory_HashPool.Add(fd.DB_Name, fd);
      fd.Stream.Save;
    end;

  fd.Stream.Data.CreateField(DB_Field, '');
  // check
  if fd.Stream.Data.ItemCreate(DB_Field, DB_Item, '', itmHnd) then
    begin
      // frag data
      md5_frag := TDirectory_Service_MD5_Data_Frag.Create(self, MD5_Database.NewData);
      InData.R.ReadDataFrame(md5_frag.Stream.Data);
      md5_frag.ReadInfo;
      MD5_Pool.Add(frag_md5_name, md5_frag);
      md5_frag.Stream.Save;

      // write item
      itm_stream := TItemStream.Create(fd.Stream.Data, itmHnd);
      StreamWriteString(itm_stream, frag_md5_name);
      StreamWriteInt64(itm_stream, md5_frag.Size);
      disposeObject(itm_stream);

      OutData.WriteBool(True);
      OutData.WriteString('item %s/%s create done', [DB_Field.Text, DB_Item.Text]);
    end
  else
    begin
      OutData.WriteBool(False);
      OutData.WriteString('item %s/%s create error: %s', [DB_Field.Text, DB_Item.Text, TranslateReturnCode(itmHnd.Item.RHeader.State).Text]);
    end;
  fd.IsChanged := True;
end;

procedure TC40_NetDisk_Directory_Service.cmd_PutItemMD5(Sender: TPeerIO; InData, OutData: TDFE);
var
  DB_Name: U_String;
  DB_Field: U_String;
  DB_Item: U_String;
  frag_md5_name: U_String;
  fd: TDirectory_Service_User_File_DB;
  md5_frag: TDirectory_Service_MD5_Data_Frag;
  itmHnd: TItemHandle;
  itm_stream: TItemStream;
begin
  DB_Name := InData.R.ReadString;
  DB_Field := InData.R.ReadString;
  DB_Item := InData.R.ReadString;
  frag_md5_name := InData.R.ReadString;
  md5_frag := MD5_Pool[frag_md5_name];
  if md5_frag = nil then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found md5: %s', [frag_md5_name.Text]);
      exit;
    end;
  fd := Directory_HashPool[DB_Name];
  if fd = nil then
    begin
      // create file db
      fd := TDirectory_Service_User_File_DB.Create(self, Directory_Database.NewData);
      fd.Stream.Data.Reserved := String_To_Reserved(DB_Name);
      fd.DB_Name := Reserved_To_String(fd.Stream.Data.Reserved);
      Directory_HashPool.Add(fd.DB_Name, fd);
      fd.Stream.Save;
    end;

  fd.Stream.Data.CreateField(DB_Field, '');
  // check
  if fd.Stream.Data.ItemCreate(DB_Field, DB_Item, '', itmHnd) then
    begin
      // write item
      itm_stream := TItemStream.Create(fd.Stream.Data, itmHnd);
      StreamWriteString(itm_stream, frag_md5_name);
      StreamWriteInt64(itm_stream, md5_frag.Size);
      disposeObject(itm_stream);
      OutData.WriteBool(True);
      OutData.WriteString('item %s/%s create done', [DB_Field.Text, DB_Item.Text]);
    end
  else
    begin
      OutData.WriteBool(False);
      OutData.WriteString('item %s/%s create error: %s', [DB_Field.Text, DB_Item.Text, TranslateReturnCode(itmHnd.Item.RHeader.State).Text]);
    end;
  fd.IsChanged := True;
end;

procedure TC40_NetDisk_Directory_Service.cmd_RemoveField(Sender: TPeerIO; InData: TDFE);
var
  DB_Name: U_String;
  DB_Field: U_String;
  DB_Remove_Field_: U_String;
  fd: TDirectory_Service_User_File_DB;
begin
  DB_Name := InData.R.ReadString;
  DB_Field := InData.R.ReadString;
  DB_Remove_Field_ := InData.R.ReadString;
  fd := Directory_HashPool[DB_Name];
  if fd = nil then
    begin
      // create file db
      fd := TDirectory_Service_User_File_DB.Create(self, Directory_Database.NewData);
      fd.Stream.Data.Reserved := String_To_Reserved(DB_Name);
      fd.DB_Name := Reserved_To_String(fd.Stream.Data.Reserved);
      Directory_HashPool.Add(fd.DB_Name, fd);
      fd.Stream.Save;
    end;
  fd.Stream.Data.FieldDelete(DB_Field, DB_Remove_Field_);
  fd.IsChanged := True;
end;

procedure TC40_NetDisk_Directory_Service.cmd_RemoveItem(Sender: TPeerIO; InData: TDFE);
var
  DB_Name: U_String;
  DB_Field: U_String;
  DB_Remove_Item_: U_String;
  fd: TDirectory_Service_User_File_DB;
begin
  DB_Name := InData.R.ReadString;
  DB_Field := InData.R.ReadString;
  DB_Remove_Item_ := InData.R.ReadString;
  fd := Directory_HashPool[DB_Name];
  if fd = nil then
    begin
      // create file db
      fd := TDirectory_Service_User_File_DB.Create(self, Directory_Database.NewData);
      fd.Stream.Data.Reserved := String_To_Reserved(DB_Name);
      fd.DB_Name := Reserved_To_String(fd.Stream.Data.Reserved);
      Directory_HashPool.Add(fd.DB_Name, fd);
      fd.Stream.Save;
    end;
  fd.Stream.Data.ItemDelete(DB_Field, DB_Remove_Item_);
  fd.IsChanged := True;
end;

procedure TC40_NetDisk_Directory_Service.cmd_NewField(Sender: TPeerIO; InData: TDFE);
var
  DB_Name: U_String;
  DB_Field: U_String;
  fd: TDirectory_Service_User_File_DB;
begin
  DB_Name := InData.R.ReadString;
  DB_Field := InData.R.ReadString;
  fd := Directory_HashPool[DB_Name];
  if fd = nil then
    begin
      // create file db
      fd := TDirectory_Service_User_File_DB.Create(self, Directory_Database.NewData);
      fd.Stream.Data.Reserved := String_To_Reserved(DB_Name);
      fd.DB_Name := Reserved_To_String(fd.Stream.Data.Reserved);
      Directory_HashPool.Add(fd.DB_Name, fd);
      fd.Stream.Save;
    end;
  fd.Stream.Data.CreateField(DB_Field, '');
  fd.IsChanged := True;
end;

procedure TC40_NetDisk_Directory_Service.cmd_SpaceInfo(Sender: TPeerIO; InData, OutData: TDFE);
var
  DB_Name: U_String;
  fd: TDirectory_Service_User_File_DB;
  m64: TMS64;
  tmp: TTemp_Compute_SpaceInfo;
begin
  DB_Name := InData.R.ReadString;
  fd := Directory_HashPool[DB_Name];
  if fd = nil then
    begin
      // create file db
      fd := TDirectory_Service_User_File_DB.Create(self, Directory_Database.NewData);
      fd.Stream.Data.Reserved := String_To_Reserved(DB_Name);
      fd.DB_Name := Reserved_To_String(fd.Stream.Data.Reserved);
      Directory_HashPool.Add(fd.DB_Name, fd);
      fd.Stream.Save;
    end;
  if GetTimeTick() - fd.FragSpaceUpdateTime > 3000 then
    begin
      fd.Stream.Data.UpdateIO;
      m64 := TMS64.Create;
      m64.LoadFromStream(fd.Stream.Data.StreamEngine);

      tmp := TTemp_Compute_SpaceInfo.Create;
      tmp.Service := self;
      tmp.DB_Name := DB_Name;
      RunHPC_StreamM(Sender, nil, m64, InData, OutData, {$IFDEF FPC}@{$ENDIF FPC}tmp.Do_Compute_SpaceInfo);
    end
  else
    begin
      OutData.WriteInt64(fd.Field_Num);
      OutData.WriteInt64(fd.Item_Num);
      OutData.WriteInt64(fd.ItemSpace + fd.Stream.Data.Size);
    end;
end;

constructor TC40_NetDisk_Directory_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
var
  Directory_FS: TCore_Stream;
  fd: TDirectory_Service_User_File_DB;
  i: Integer;
  MD5_FS: TCore_Stream;
  md5_frag: TDirectory_Service_MD5_Data_Frag;
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  DTNoAuthService.RecvTunnel.RegisterStream('NewDB').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_NewDB;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('RemoveDB').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemoveDB;
  DTNoAuthService.RecvTunnel.RegisterStream('GetItemList').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetItemList;
  DTNoAuthService.RecvTunnel.RegisterStream('GetItemFrag').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_GetItemFrag;
  DTNoAuthService.RecvTunnel.RegisterStream('FoundMD5').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FoundMD5;
  DTNoAuthService.RecvTunnel.RegisterStream('PutItemFrag').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_PutItemFrag;
  DTNoAuthService.RecvTunnel.RegisterStream('PutItemMD5').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_PutItemMD5;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('RemoveField').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemoveField;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('RemoveItem').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_RemoveItem;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('NewField').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_NewField;
  DTNoAuthService.RecvTunnel.RegisterStream('SpaceInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_SpaceInfo;
  // instance
  ServiceInfo.OnlyInstance := True;
  UpdateToGlobalDispatch;
  ParamList.SetDefaultValue('OnlyInstance', if_(ServiceInfo.OnlyInstance, 'True', 'False'));

  // directory database
  Directory_ZDB2_RecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('Directory_RecycleMemory', '5*1000'), 5 * 1000);
  Directory_ZDB2_DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('Directory_DeltaSpace', '128*1024*1024'), 128 * 1024 * 1024);
  Directory_ZDB2_BlockSize := EStrToInt(ParamList.GetDefaultValue('Directory_BlockSize', '1536'), 1536);
  Directory_ZDB2_EnabledCipher := EStrToBool(ParamList.GetDefaultValue('Directory_EnabledCipher', 'True'), True);
  Directory_ZDB2_CipherName := ParamList.GetDefaultValue('Directory_Cipher', TCipher.CCipherSecurityName[TCipherSecurity.csRijndael]);
  Directory_ZDB2_Password := ParamList.GetDefaultValue('Directory_Password', Z.Net.C4.C40_Password);

  if Directory_ZDB2_EnabledCipher then
      Directory_ZDB2_Cipher := TZDB2_Cipher.Create(Directory_ZDB2_CipherName, Directory_ZDB2_Password, 1, True, True)
  else
      Directory_ZDB2_Cipher := nil;
  C40_Directory_Database_File := umlCombineFileName(DTNoAuthService.PublicFileDirectory, PFormat('DTC40_%s.Directory', [ServiceInfo.ServiceTyp.Text]));

  Directory_HashPool := TDirectory_Service_User_File_DB_Pool.Create(True,
    EStrToInt64(ParamList.GetDefaultValue('Directory_HashPool', '4*1024*1024'), 4 * 1024 * 1024),
    nil);
  Directory_HashPool.IgnoreCase := True;

  if EStrToBool(ParamList.GetDefaultValue('ForeverSave', 'True'), True) and umlFileExists(C40_Directory_Database_File) then
      Directory_FS := TCore_FileStream.Create(C40_Directory_Database_File, fmOpenReadWrite)
  else
      Directory_FS := TCore_FileStream.Create(C40_Directory_Database_File, fmCreate);

  Directory_Database := TZDB2_List_ObjectDataManager.Create(
    TZDB2_ObjectDataManager,
    nil,
    Directory_ZDB2_RecycleMemoryTimeOut,
    Directory_FS,
    False,
    Directory_ZDB2_DeltaSpace,
    Directory_ZDB2_BlockSize,
    Directory_ZDB2_Cipher);
  Directory_Database.AutoFreeStream := True;

  i := 0;
  while i < Directory_Database.count do
    begin
      fd := TDirectory_Service_User_File_DB.Create(self, Directory_Database[i]);
      inc(i);
      fd.DB_Name := Reserved_To_String(fd.Stream.Data.Reserved);
      if fd.DB_Name <> '' then
        begin
          fd.IsChanged := True;
          fd.ComputeFragSpace;
          fd.Stream.RecycleMemory;
          Directory_HashPool.Add(fd.DB_Name, fd);
        end
      else
          disposeObject(fd);
    end;

  // md5 frag database
  MD5_ZDB2_RecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('MD5_RecycleMemory', '1*1000'), 1 * 1000);
  MD5_ZDB2_DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('MD5_DeltaSpace', '16*1024*1024'), 16 * 1024 * 1024);
  MD5_ZDB2_BlockSize := EStrToInt(ParamList.GetDefaultValue('MD5_BlockSize', '100'), 100);
  MD5_ZDB2_EnabledCipher := EStrToBool(ParamList.GetDefaultValue('MD5_EnabledCipher', 'True'), True);
  MD5_ZDB2_CipherName := ParamList.GetDefaultValue('MD5_Cipher', TCipher.CCipherSecurityName[TCipherSecurity.csRijndael]);
  MD5_ZDB2_Password := ParamList.GetDefaultValue('MD5_Password', Z.Net.C4.C40_Password);

  if MD5_ZDB2_EnabledCipher then
      MD5_ZDB2_Cipher := TZDB2_Cipher.Create(MD5_ZDB2_CipherName, MD5_ZDB2_Password, 1, True, True)
  else
      MD5_ZDB2_Cipher := nil;
  C40_MD5_Database_File := umlCombineFileName(DTNoAuthService.PublicFileDirectory, PFormat('DTC40_%s.MD5_Frag', [ServiceInfo.ServiceTyp.Text]));

  MD5_Pool := TDirectory_Service_MD5_DataPool.Create(True,
    EStrToInt64(ParamList.GetDefaultValue('MD5_HashPool', '16*1024*1024'), 16 * 1024 * 1024),
    nil);

  if EStrToBool(ParamList.GetDefaultValue('ForeverSave', 'True'), True) and umlFileExists(C40_MD5_Database_File) then
      MD5_FS := TCore_FileStream.Create(C40_MD5_Database_File, fmOpenReadWrite)
  else
      MD5_FS := TCore_FileStream.Create(C40_MD5_Database_File, fmCreate);

  MD5_Database := TZDB2_List_DFE.Create(
    TZDB2_DFE,
    nil,
    MD5_ZDB2_RecycleMemoryTimeOut,
    MD5_FS,
    False,
    MD5_ZDB2_DeltaSpace,
    MD5_ZDB2_BlockSize,
    MD5_ZDB2_Cipher);
  MD5_Database.AutoFreeStream := True;

  i := 0;
  while i < MD5_Database.count do
    begin
      md5_frag := TDirectory_Service_MD5_Data_Frag.Create(self, MD5_Database[i]);
      inc(i);
      md5_frag.ReadInfo;
      md5_frag.Stream.RecycleMemory;
      MD5_Pool.Add(umlMD5ToStr(md5_frag.MD5), md5_frag);
    end;
end;

destructor TC40_NetDisk_Directory_Service.Destroy;
{$IFDEF FPC}
  procedure fpc_progress_hash_pool_(const Name_: PSystemString; Obj_: TDirectory_Service_User_File_DB);
  begin
    Obj_.Stream := nil;
  end;
  procedure fpc_progress_md5_pool_(const Name_: PSystemString; Obj_: TDirectory_Service_MD5_Data_Frag);
  begin
    Obj_.Stream := nil;
  end;
{$ENDIF FPC}


begin
  Directory_Database.Flush;
  MD5_Database.Flush;
{$IFDEF FPC}
  Directory_HashPool.ProgressP(@fpc_progress_hash_pool_);
  MD5_Pool.ProgressP(@fpc_progress_md5_pool_);
{$ELSE FPC}
  Directory_HashPool.ProgressP(procedure(const Name_: PSystemString; Obj_: TDirectory_Service_User_File_DB)
    begin
      Obj_.Stream := nil;
    end);
  MD5_Pool.ProgressP(procedure(const Name_: PSystemString; Obj_: TDirectory_Service_MD5_Data_Frag)
    begin
      Obj_.Stream := nil;
    end);
{$ENDIF FPC}
  DisposeObjectAndNil(Directory_HashPool);
  DisposeObjectAndNil(Directory_Database);
  DisposeObjectAndNil(MD5_Pool);
  DisposeObjectAndNil(MD5_Database);
  DisposeObjectAndNil(Directory_ZDB2_Cipher);
  DisposeObjectAndNil(MD5_ZDB2_Cipher);
  inherited Destroy;
end;

procedure TC40_NetDisk_Directory_Service.SafeCheck;
begin
  inherited SafeCheck;
  Directory_Database.Flush;
  MD5_Database.Flush;
end;

procedure TC40_NetDisk_Directory_Service.Progress;
begin
  inherited Progress;
  Directory_Database.Progress;
  MD5_Database.Progress;
end;

constructor TTemp_Compute_SpaceInfo.Create;
begin
  inherited Create;
  Service := nil;
  DB_Name := '';
  Field_Num := 0;
  Item_Num := 0;
  ItemSpace := 0;
end;

destructor TTemp_Compute_SpaceInfo.Destroy;
begin
  inherited Destroy;
end;

procedure TTemp_Compute_SpaceInfo.Do_Compute_SpaceInfo(ThSender: THPC_Stream; ThInData, ThOutData: TDFE);
var
  dbEng: TObjectDataManager;
  ir: TItemRecursionSearch;
  itm_stream: TItemStream;
begin
  ThSender.OnDone_M := {$IFDEF FPC}@{$ENDIF FPC}Do_Compute_SpaceInfo_Done;
  dbEng := nil;
  try
    dbEng := TObjectDataManager.CreateAsStream(TMS64(ThSender.UserObject), '', DBMarshal.ID, False, False, True);
    if dbEng.RecursionSearchFirst('/', '*', ir) then
      begin
        repeat
          case ir.ReturnHeader.ID of
            DB_Header_Field_ID: inc(Field_Num);
            DB_Header_Item_ID:
              begin
                inc(Item_Num);
                itm_stream := TItemStream.Create(dbEng, ir.ReturnHeader.CurrentHeader);
                try
                  StreamIgnoreReadString(itm_stream);
                  inc(ItemSpace, StreamReadInt64(itm_stream));
                except
                end;
                disposeObject(itm_stream);
              end;
          end;
        until not dbEng.RecursionSearchNext(ir);
      end;
    ThOutData.WriteInt64(Field_Num);
    ThOutData.WriteInt64(Item_Num);
    ThOutData.WriteInt64(ItemSpace + dbEng.Size);
  except
    ThOutData.WriteInt64(0);
    ThOutData.WriteInt64(0);
    ThOutData.WriteInt64(0);
  end;
  disposeObject(dbEng);
end;

procedure TTemp_Compute_SpaceInfo.Do_Compute_SpaceInfo_Done(ThSender: THPC_Stream; IO: TPeerIO; ThInData, ThOutData: TDFE);
var
  fd: TDirectory_Service_User_File_DB;
begin
  try
    fd := Service.Directory_HashPool[DB_Name];
    if fd <> nil then
      begin
        fd.Field_Num := Field_Num;
        fd.Item_Num := Item_Num;
        fd.ItemSpace := ItemSpace;
        fd.FragSpaceUpdateTime := GetTimeTick();
      end;
  except
  end;
  DelayFreeObj(1.0, self);
end;

constructor TON_Temp_NewDB.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Temp_NewDB.DoStreamEvent(Sender: TPeerIO; Result_: TDFE);
var
  Successed: Boolean; info: SystemString;
begin
  Successed := Result_.R.ReadBool;
  info := Result_.R.ReadString;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TON_Temp_GetItemList.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Temp_GetItemList.DoStreamEvent(Sender: TPeerIO; Result_: TDFE);
var
  arry: TItemList_Data_Array;
  i: Integer;
begin
  SetLength(arry, Result_.count div 3);
  i := 0;
  while Result_.R.NotEnd do
    begin
      arry[i].Name := Result_.R.ReadString;
      arry[i].Num := Result_.R.ReadInt64;
      arry[i].Time_ := Result_.R.ReadDouble;
      inc(i);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;

  for i := low(arry) to high(arry) do
      arry[i].Name := '';
  SetLength(arry, 0);
  DelayFreeObject(1.0, self);
end;

constructor TON_Temp_GetItemFrag.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Temp_GetItemFrag.DoStreamEvent(Sender: TPeerIO; Result_: TDFE);
var
  Successed: Boolean;
  L: TDirectory_MD5_Data_Frag_Struct_List;
  tmp: TDFE;
begin
  Successed := Result_.R.ReadBool;
  L := TDirectory_MD5_Data_Frag_Struct_List.Create;
  if Successed then
    begin
      tmp := TDFE.Create;
      Result_.R.ReadDataFrame(tmp);
      L.Decode(tmp, True);
      disposeObject(tmp);
    end
  else
      DoStatus(Result_.R.ReadString);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, L);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, L);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, L);
  except
  end;
  DelayFreeObject(1.0, self, L);
end;

constructor TON_Temp_FoundMD5.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Temp_FoundMD5.DoStreamEvent(Sender: TPeerIO; Result_: TDFE);
var
  Successed: Boolean;
begin
  Successed := Result_.R.ReadBool;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TON_Temp_PutItemFrag.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Temp_PutItemFrag.DoStreamEvent(Sender: TPeerIO; Result_: TDFE);
var
  Successed: Boolean; info: SystemString;
begin
  Successed := Result_.R.ReadBool;
  info := Result_.R.ReadString;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TON_Temp_PutItemMD5.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Temp_PutItemMD5.DoStreamEvent(Sender: TPeerIO; Result_: TDFE);
var
  Successed: Boolean; info: SystemString;
begin
  Successed := Result_.R.ReadBool;
  info := Result_.R.ReadString;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Successed, info);
    if Assigned(OnResultM) then
        OnResultM(Client, Successed, info);
    if Assigned(OnResultP) then
        OnResultP(Client, Successed, info);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TON_Temp_SpaceInfo.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TON_Temp_SpaceInfo.DoStreamEvent(Sender: TPeerIO; Result_: TDFE);
var
  Field_Num, Item_Num, ItemSpace: Int64;
begin
  Field_Num := Result_.R.ReadInt64;
  Item_Num := Result_.R.ReadInt64;
  ItemSpace := Result_.R.ReadInt64;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, Field_Num, Item_Num, ItemSpace);
    if Assigned(OnResultM) then
        OnResultM(Client, Field_Num, Item_Num, ItemSpace);
    if Assigned(OnResultP) then
        OnResultP(Client, Field_Num, Item_Num, ItemSpace);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_NetDisk_Directory_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
end;

destructor TC40_NetDisk_Directory_Client.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_NetDisk_Directory_Client.Progress;
begin
  inherited Progress;
end;

procedure TC40_NetDisk_Directory_Client.NewDB_C(DB_Name: U_String; OnResult: TON_NewDB_C);
var
  tmp: TON_Temp_NewDB;
  d: TDFE;
begin
  tmp := TON_Temp_NewDB.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NewDB', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.NewDB_M(DB_Name: U_String; OnResult: TON_NewDB_M);
var
  tmp: TON_Temp_NewDB;
  d: TDFE;
begin
  tmp := TON_Temp_NewDB.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NewDB', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.NewDB_P(DB_Name: U_String; OnResult: TON_NewDB_P);
var
  tmp: TON_Temp_NewDB;
  d: TDFE;
begin
  tmp := TON_Temp_NewDB.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('NewDB', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.RemoveDB(DB_Name: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(DB_Name);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveDB', d);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.GetItemList_C(DB_Name, DB_Field: U_String; OnResult: TON_GetItemList_C);
var
  tmp: TON_Temp_GetItemList;
  d: TDFE;
begin
  tmp := TON_Temp_GetItemList.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetItemList', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.GetItemList_M(DB_Name, DB_Field: U_String; OnResult: TON_GetItemList_M);
var
  tmp: TON_Temp_GetItemList;
  d: TDFE;
begin
  tmp := TON_Temp_GetItemList.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetItemList', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.GetItemList_P(DB_Name, DB_Field: U_String; OnResult: TON_GetItemList_P);
var
  tmp: TON_Temp_GetItemList;
  d: TDFE;
begin
  tmp := TON_Temp_GetItemList.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetItemList', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.GetItemFrag_C(DB_Name, DB_Field, DB_Item: U_String; OnResult: TON_GetItemFrag_C);
var
  tmp: TON_Temp_GetItemFrag;
  d: TDFE;
begin
  tmp := TON_Temp_GetItemFrag.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetItemFrag', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.GetItemFrag_M(DB_Name, DB_Field, DB_Item: U_String; OnResult: TON_GetItemFrag_M);
var
  tmp: TON_Temp_GetItemFrag;
  d: TDFE;
begin
  tmp := TON_Temp_GetItemFrag.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetItemFrag', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.GetItemFrag_P(DB_Name, DB_Field, DB_Item: U_String; OnResult: TON_GetItemFrag_P);
var
  tmp: TON_Temp_GetItemFrag;
  d: TDFE;
begin
  tmp := TON_Temp_GetItemFrag.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('GetItemFrag', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.FoundMD5_C(frag_md5_name: U_String; OnResult: TON_FoundMD5_C);
var
  tmp: TON_Temp_FoundMD5;
  d: TDFE;
begin
  tmp := TON_Temp_FoundMD5.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(frag_md5_name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FoundMD5', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.FoundMD5_M(frag_md5_name: U_String; OnResult: TON_FoundMD5_M);
var
  tmp: TON_Temp_FoundMD5;
  d: TDFE;
begin
  tmp := TON_Temp_FoundMD5.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(frag_md5_name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FoundMD5', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.FoundMD5_P(frag_md5_name: U_String; OnResult: TON_FoundMD5_P);
var
  tmp: TON_Temp_FoundMD5;
  d: TDFE;
begin
  tmp := TON_Temp_FoundMD5.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(frag_md5_name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FoundMD5', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.PutItemFrag_C(DB_Name, DB_Field, DB_Item: U_String; L: TDirectory_MD5_Data_Frag_Struct_List; OnResult: TON_PutItemFrag_C);
var
  tmp: TON_Temp_PutItemFrag;
  d, nd: TDFE;
begin
  tmp := TON_Temp_PutItemFrag.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  d.WriteString(umlMD5ToStr(L.MD5));
  nd := TDFE.Create;
  L.Encode(nd);
  d.WriteDataFrame(nd);
  disposeObject(nd);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('PutItemFrag', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.PutItemFrag_M(DB_Name, DB_Field, DB_Item: U_String; L: TDirectory_MD5_Data_Frag_Struct_List; OnResult: TON_PutItemFrag_M);
var
  tmp: TON_Temp_PutItemFrag;
  d, nd: TDFE;
begin
  tmp := TON_Temp_PutItemFrag.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  d.WriteString(umlMD5ToStr(L.MD5));
  nd := TDFE.Create;
  L.Encode(nd);
  d.WriteDataFrame(nd);
  disposeObject(nd);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('PutItemFrag', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.PutItemFrag_P(DB_Name, DB_Field, DB_Item: U_String; L: TDirectory_MD5_Data_Frag_Struct_List; OnResult: TON_PutItemFrag_P);
var
  tmp: TON_Temp_PutItemFrag;
  d, nd: TDFE;
begin
  tmp := TON_Temp_PutItemFrag.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  d.WriteString(umlMD5ToStr(L.MD5));
  nd := TDFE.Create;
  L.Encode(nd);
  d.WriteDataFrame(nd);
  disposeObject(nd);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('PutItemFrag', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.PutItemMD5_C(DB_Name, DB_Field, DB_Item, frag_md5_name: U_String; OnResult: TON_PutItemMD5_C);
var
  tmp: TON_Temp_PutItemMD5;
  d: TDFE;
begin
  tmp := TON_Temp_PutItemMD5.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  d.WriteString(frag_md5_name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('PutItemMD5', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.PutItemMD5_M(DB_Name, DB_Field, DB_Item, frag_md5_name: U_String; OnResult: TON_PutItemMD5_M);
var
  tmp: TON_Temp_PutItemMD5;
  d: TDFE;
begin
  tmp := TON_Temp_PutItemMD5.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  d.WriteString(frag_md5_name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('PutItemMD5', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.PutItemMD5_P(DB_Name, DB_Field, DB_Item, frag_md5_name: U_String; OnResult: TON_PutItemMD5_P);
var
  tmp: TON_Temp_PutItemMD5;
  d: TDFE;
begin
  tmp := TON_Temp_PutItemMD5.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Item);
  d.WriteString(frag_md5_name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('PutItemMD5', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.RemoveField(DB_Name, DB_Field, DB_Remove_Field_: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Remove_Field_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveField', d);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.RemoveItem(DB_Name, DB_Field, DB_Remove_Item_: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  d.WriteString(DB_Remove_Item_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('RemoveItem', d);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.NewField(DB_Name, DB_Field: U_String);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(DB_Name);
  d.WriteString(DB_Field);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('NewField', d);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.SpaceInfo_C(DB_Name: U_String; OnResult: TON_SpaceInfo_C);
var
  tmp: TON_Temp_SpaceInfo;
  d: TDFE;
begin
  tmp := TON_Temp_SpaceInfo.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('SpaceInfo', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.SpaceInfo_M(DB_Name: U_String; OnResult: TON_SpaceInfo_M);
var
  tmp: TON_Temp_SpaceInfo;
  d: TDFE;
begin
  tmp := TON_Temp_SpaceInfo.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('SpaceInfo', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

procedure TC40_NetDisk_Directory_Client.SpaceInfo_P(DB_Name: U_String; OnResult: TON_SpaceInfo_P);
var
  tmp: TON_Temp_SpaceInfo;
  d: TDFE;
begin
  tmp := TON_Temp_SpaceInfo.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;

  d := TDFE.Create;
  d.WriteString(DB_Name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('SpaceInfo', d, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamEvent);
  disposeObject(d);
end;

initialization

RegisterC40('NetDisk_Directory', TC40_NetDisk_Directory_Service, TC40_NetDisk_Directory_Client);

end.