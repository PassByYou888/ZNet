{ ****************************************************************************** }
{ * cloud 4.0 network disk VM Client Task tool                                 * }
{ ****************************************************************************** }
unit Z.Net.C4_NetDisk_VM_Client.Task;

{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine,
  Z.Geometry2D, Z.DFE, Z.Json, Z.Expression,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.GHashList,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth,
  Z.Net.C4_NetDisk_Directory,
  Z.Net.C4,
  Z.Net.C4_NetDisk_VM_Client;

type
  TC40_NetDisk_VM_Client_Task = class;
  TC40_NetDisk_VM_Client_Task_Clone_Connection = class;
  TC40_NetDisk_VM_Client_Task_Auto_Post_Stream = class;
  TC40_NetDisk_VM_Client_Task_Auto_Get_Stream = class;
  TC40_NetDisk_VM_Client_Task_Auto_Post_Encrypt_Stream = class;
  TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream = class;
  TC40_NetDisk_VM_Client_Task_Auto_Post_File = class;
  TC40_NetDisk_VM_Client_Task_Auto_Get_File = class;
  TC40_NetDisk_VM_Client_Task_Auto_Get_Directory = class;
  TC40_NetDisk_VM_Client_Task_Auto_Post_Directory = class;
  TC40_NetDisk_VM_Client_Task_List = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalBigList<TC40_NetDisk_VM_Client_Task>;

  TC40_NetDisk_VM_Client_Task_Tool = class;
  TC40_NetDisk_VM_Client_Task_Pool_Event = procedure(sender: TC40_NetDisk_VM_Client_Task_Tool) of object;
  TC40_NetDisk_VM_Client_Task_Event = procedure(sender: TC40_NetDisk_VM_Client_Task) of object;

  TC40_NetDisk_VM_Client_Task_Tool = class
  private
    FList: TC40_NetDisk_VM_Client_Task_List;
    procedure Do_All_Done();
    procedure DoFree(var Data: TC40_NetDisk_VM_Client_Task);
  public
    Client: TC40_NetDisk_VM_Client;
    All_Done_Do_Auto_Free_Self: Boolean;
    All_Done_Do_Auto_Free_Client: Boolean;
    On_All_Done: TC40_NetDisk_VM_Client_Task_Pool_Event;
    On_Add_Task: TC40_NetDisk_VM_Client_Task_Event;
    On_Run: TC40_NetDisk_VM_Client_Task_Event;
    On_Done: TC40_NetDisk_VM_Client_Task_Event;
    constructor Create(Client_: TC40_NetDisk_VM_Client);
    destructor Destroy; override;
    procedure All_Done; virtual;
    function Is_Runing: Boolean;
    function Task_Num: NativeInt;
    function Repeat_: TC40_NetDisk_VM_Client_Task_List.TRepeat___;
    function First: TC40_NetDisk_VM_Client_Task;
    // automated clone
    function Add_Task_Clone_Connection(): TC40_NetDisk_VM_Client_Task_Clone_Connection;
    // stream
    function Add_Task_Post_Stream(Stream: TCore_Stream; AutoFreeStream: Boolean; DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Post_Stream;
    function Add_Task_Get_Stream(Stream: TCore_Stream; AutoFreeStream: Boolean; DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_Stream;
    function Add_Task_Get_Share_Stream(Stream: TCore_Stream; AutoFreeStream: Boolean; Share_Directory_DB_Name, DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_Stream;
    // private stream
    function Add_Task_Post_Encrypt_Stream(Encrypt_Security: TCipherSecurity; Encrypt_Key: U_String;
      Stream: TCore_Stream; AutoFreeStream: Boolean; DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Post_Encrypt_Stream;
    function Add_Task_Get_Decrypt_Stream(Decrypt_Security: TCipherSecurity; Decrypt_Key: U_String;
      Stream: TCore_Stream; AutoFreeStream: Boolean; DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream;
    function Add_Task_Get_Decrypt_Share_Stream(Decrypt_Security: TCipherSecurity; Decrypt_Key: U_String;
      Stream: TCore_Stream; AutoFreeStream: Boolean; Share_Directory_DB_Name, DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream;
    // file
    function Add_Task_Post_File(Local_File, DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Post_File;
    function Add_Task_Post_Directory(Local_Directory, DB_Field: U_String): TC40_NetDisk_VM_Client_Task_Auto_Post_Directory;
    function Add_Task_Get_File(Local_File, DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_File;
    function Add_Task_Get_Share_File(Share_Directory_DB_Name, Local_File, DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_File;
    function Add_Task_Get_Directory(Local_Directory, DB_Field: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_Directory;
    function Add_Task_Get_Share_Directory(Share_Directory_DB_Name, Local_Directory, DB_Field: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_Directory;
  end;

  TC40_NetDisk_VM_Client_Task = class
  private
    OwnerPool: TC40_NetDisk_VM_Client_Task_Tool;
    Pool_Ptr: TC40_NetDisk_VM_Client_Task_List.PQueueStruct;
    FTask_Is_Busy: Boolean;
    FTask_No: Integer;
    procedure Do_Go_Next_Task;
  public
    UserData: TCore_Object;
    UserData2: TCore_Object;
    UserDataInfo: U_String;
    On_Run: TC40_NetDisk_VM_Client_Task_Event;
    On_Done: TC40_NetDisk_VM_Client_Task_Event;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Run_Task; virtual;
    procedure Run_Task;
    procedure Go_Next_Task;
    function Client: TC40_NetDisk_VM_Client;
    property Is_Busy: Boolean read FTask_Is_Busy;
    property Task_No: Integer read FTask_No;
  end;

  TC40_NetDisk_VM_Client_Task_Clone_Connection = class(TC40_NetDisk_VM_Client_Task)
  public
    constructor Create;
    procedure Do_Clone_Done(sender: TC40_NetDisk_VM_Client; New_Instance: TC40_NetDisk_VM_Client);
    procedure Do_Run_Task; override;
  end;

  TC40_NetDisk_VM_Client_Task_Auto_Post_Stream_Event = procedure(sender: TC40_NetDisk_VM_Client_Task_Auto_Post_Stream;
    Stream: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String) of object;

  TC40_NetDisk_VM_Client_Task_Auto_Post_Stream = class(TC40_NetDisk_VM_Client_Task)
  public
    Stream: TCore_Stream;
    AutoFreeStream: Boolean;
    DB_Field, DB_Item: U_String;
    On_Post_Stream_Done: TC40_NetDisk_VM_Client_Task_Auto_Post_Stream_Event;
    constructor Create;
    procedure Do_Usr_Auto_Post_File(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object;
      Stream_: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String);
    procedure Do_Run_Task; override;
  end;

  TC40_NetDisk_VM_Client_Task_Auto_Get_Stream_Event = procedure(sender: TC40_NetDisk_VM_Client_Task_Auto_Get_Stream;
    Stream: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String) of object;

  TC40_NetDisk_VM_Client_Task_Auto_Get_Stream = class(TC40_NetDisk_VM_Client_Task)
  public
    Stream: TCore_Stream;
    AutoFreeStream: Boolean;
    Share_Directory_DB_Name, DB_Field, DB_Item: U_String;
    On_Get_Stream_Done: TC40_NetDisk_VM_Client_Task_Auto_Get_Stream_Event;
    constructor Create;
    procedure Do_Usr_Auto_Get_File(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object;
      Stream_: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String);
    procedure Do_Run_Task; override;
  end;

  TC40_NetDisk_VM_Client_Task_Auto_Post_Encrypt_Stream_Event = procedure(sender: TC40_NetDisk_VM_Client_Task_Auto_Post_Encrypt_Stream;
    Stream: TCore_Stream; Encrypt_Stream: TMS64; Successed: Boolean; info: U_String) of object;

  TC40_NetDisk_VM_Client_Task_Auto_Post_Encrypt_Stream = class(TC40_NetDisk_VM_Client_Task)
  public
    Stream: TCore_Stream;
    AutoFreeStream: Boolean;
    Encrypt_Stream: TMS64;
    Encrypt_Security: TCipherSecurity;
    Encrypt_Key: U_String;
    DB_Field, DB_Item: U_String;
    On_Post_Encrypt_Stream_Done: TC40_NetDisk_VM_Client_Task_Auto_Post_Encrypt_Stream_Event;
    Prepare_Done: Boolean;
    Source_MD5, Encrypt_MD5: TMD5;
    constructor Create;
    destructor Destroy; override;
    procedure Th_Encrypt();
    procedure Do_Usr_Auto_Post_File(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object;
      Stream_: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String);
    procedure Do_Run_Task; override;
  end;

  TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream_Event = procedure(sender: TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream;
    Stream: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String) of object;

  TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream = class(TC40_NetDisk_VM_Client_Task)
  public
    Stream: TCore_Stream;
    AutoFreeStream: Boolean;
    Decrypt_Security: TCipherSecurity;
    Decrypt_Key: U_String;
    Share_Directory_DB_Name, DB_Field, DB_Item: U_String;
    On_Get_Decrypt_Stream_Done: TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream_Event;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Usr_Auto_Get_File(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object;
      Stream_: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String);
    procedure Do_Run_Task; override;
  end;

  TC40_NetDisk_VM_Client_Task_Auto_Post_File = class(TC40_NetDisk_VM_Client_Task)
  public
    Local_File, DB_Field, DB_Item: U_String;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Usr_Auto_Post_File(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object;
      Stream: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String);
    procedure Do_Run_Task; override;
  end;

  TC40_NetDisk_VM_Client_Task_Auto_Get_File = class(TC40_NetDisk_VM_Client_Task)
  public
    Share_Directory_DB_Name, Local_File, DB_Field, DB_Item: U_String;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Usr_Auto_Get_File(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object;
      Stream: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String);
    procedure Do_Run_Task; override;
  end;

  TC40_NetDisk_VM_Client_Task_Auto_Get_Directory = class(TC40_NetDisk_VM_Client_Task)
  public
    Share_Directory_DB_Name, Local_Directory, DB_Field: U_String;
    Get_Directory_Task_Pool: TC40_NetDisk_VM_Client_Task_Tool;
    procedure Do_Done_Get_Directory_Task_Pool(sender: TC40_NetDisk_VM_Client_Task_Tool);
    procedure Do_Search_NetDisk_File(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String;
      arry: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File_Data_array);
    constructor Create;
    destructor Destroy; override;
    procedure Do_Run_Task; override;
  end;

  TC40_NetDisk_VM_Client_Task_Auto_Post_Directory = class(TC40_NetDisk_VM_Client_Task)
  public
    Local_Directory, DB_Field: U_String;
    Post_Directory_Task_Pool: TC40_NetDisk_VM_Client_Task_Tool;
    procedure Do_Done_Post_Directory_Task_Pool(sender: TC40_NetDisk_VM_Client_Task_Tool);
    constructor Create;
    destructor Destroy; override;
    procedure Do_Run_Task; override;
  end;

implementation

var
  Task_Seed_No: Integer;

procedure TC40_NetDisk_VM_Client_Task_Tool.Do_All_Done;
begin
  All_Done;
  if Assigned(On_All_Done) then
      On_All_Done(self);

  if All_Done_Do_Auto_Free_Self then
      DelayFreeObj(0, self);
  if All_Done_Do_Auto_Free_Client then
    begin
      Client.OnEvent := nil;
      DelayFreeObj(0, Client);
    end;
end;

procedure TC40_NetDisk_VM_Client_Task_Tool.DoFree(var Data: TC40_NetDisk_VM_Client_Task);
begin
  if Data <> nil then
    begin
      Data.OwnerPool := nil;
      Data.Pool_Ptr := nil;
      DisposeObjectAndNil(Data);
    end;
end;

constructor TC40_NetDisk_VM_Client_Task_Tool.Create(Client_: TC40_NetDisk_VM_Client);
begin
  inherited Create;
  FList := TC40_NetDisk_VM_Client_Task_List.Create;
  FList.OnFree := {$IFDEF FPC}@{$ENDIF FPC}DoFree;
  Client := Client_;
  All_Done_Do_Auto_Free_Self := True;
  All_Done_Do_Auto_Free_Client := False;
  On_All_Done := nil;
  On_Add_Task := nil;
  On_Run := nil;
  On_Done := nil;
end;

destructor TC40_NetDisk_VM_Client_Task_Tool.Destroy;
begin
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TC40_NetDisk_VM_Client_Task_Tool.All_Done;
begin
end;

function TC40_NetDisk_VM_Client_Task_Tool.Is_Runing: Boolean;
var
  num_: Integer;
  __Repeat__: TC40_NetDisk_VM_Client_Task_List.TRepeat___;
begin
  num_ := 0;
  if Task_Num > 0 then
    begin
      __Repeat__ := Repeat_;
      repeat
        if __Repeat__.Queue^.Data.FTask_Is_Busy then
            inc(num_);
      until not __Repeat__.Next;
    end;
  Result := num_ > 0;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Task_Num: NativeInt;
begin
  Result := FList.Num;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Repeat_: TC40_NetDisk_VM_Client_Task_List.TRepeat___;
begin
  Result := FList.Repeat_;
end;

function TC40_NetDisk_VM_Client_Task_Tool.First: TC40_NetDisk_VM_Client_Task;
begin
  Result := nil;
  if FList.First <> nil then
      Result := FList.First^.Data;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Add_Task_Clone_Connection(): TC40_NetDisk_VM_Client_Task_Clone_Connection;
var
  task_: TC40_NetDisk_VM_Client_Task_Clone_Connection;
begin
  task_ := TC40_NetDisk_VM_Client_Task_Clone_Connection.Create;
  task_.Pool_Ptr := FList.Add(task_);
  task_.OwnerPool := self;
  if Assigned(On_Add_Task) then
      On_Add_Task(task_);
  Result := task_;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Add_Task_Post_Stream(Stream: TCore_Stream; AutoFreeStream: Boolean; DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Post_Stream;
var
  task_: TC40_NetDisk_VM_Client_Task_Auto_Post_Stream;
begin
  task_ := TC40_NetDisk_VM_Client_Task_Auto_Post_Stream.Create;
  task_.Stream := Stream;
  task_.AutoFreeStream := AutoFreeStream;
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.OwnerPool := self;
  if Assigned(On_Add_Task) then
      On_Add_Task(task_);
  Result := task_;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Add_Task_Get_Stream(Stream: TCore_Stream; AutoFreeStream: Boolean; DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_Stream;
var
  task_: TC40_NetDisk_VM_Client_Task_Auto_Get_Stream;
begin
  task_ := TC40_NetDisk_VM_Client_Task_Auto_Get_Stream.Create;
  task_.Stream := Stream;
  task_.AutoFreeStream := AutoFreeStream;
  task_.Share_Directory_DB_Name := '';
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.OwnerPool := self;
  if Assigned(On_Add_Task) then
      On_Add_Task(task_);
  Result := task_;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Add_Task_Get_Share_Stream(Stream: TCore_Stream; AutoFreeStream: Boolean; Share_Directory_DB_Name, DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_Stream;
var
  task_: TC40_NetDisk_VM_Client_Task_Auto_Get_Stream;
begin
  task_ := TC40_NetDisk_VM_Client_Task_Auto_Get_Stream.Create;
  task_.Stream := Stream;
  task_.AutoFreeStream := AutoFreeStream;
  task_.Share_Directory_DB_Name := Share_Directory_DB_Name;
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.OwnerPool := self;
  if Assigned(On_Add_Task) then
      On_Add_Task(task_);
  Result := task_;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Add_Task_Post_Encrypt_Stream(Encrypt_Security: TCipherSecurity; Encrypt_Key: U_String;
  Stream: TCore_Stream; AutoFreeStream: Boolean; DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Post_Encrypt_Stream;
var
  task_: TC40_NetDisk_VM_Client_Task_Auto_Post_Encrypt_Stream;
begin
  task_ := TC40_NetDisk_VM_Client_Task_Auto_Post_Encrypt_Stream.Create;
  task_.Stream := Stream;
  task_.AutoFreeStream := AutoFreeStream;
  task_.Encrypt_Security := Encrypt_Security;
  task_.Encrypt_Key := Encrypt_Key;
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.OwnerPool := self;
  if Assigned(On_Add_Task) then
      On_Add_Task(task_);
  TCompute.RunM_NP({$IFDEF FPC}@{$ENDIF FPC}task_.Th_Encrypt);
  Result := task_;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Add_Task_Get_Decrypt_Stream(Decrypt_Security: TCipherSecurity; Decrypt_Key: U_String;
  Stream: TCore_Stream; AutoFreeStream: Boolean; DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream;
var
  task_: TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream;
begin
  task_ := TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream.Create;
  task_.Stream := Stream;
  task_.AutoFreeStream := AutoFreeStream;
  task_.Decrypt_Security := Decrypt_Security;
  task_.Decrypt_Key := Decrypt_Key;
  task_.Share_Directory_DB_Name := '';
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.OwnerPool := self;
  if Assigned(On_Add_Task) then
      On_Add_Task(task_);
  Result := task_;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Add_Task_Get_Decrypt_Share_Stream(Decrypt_Security: TCipherSecurity; Decrypt_Key: U_String;
  Stream: TCore_Stream; AutoFreeStream: Boolean; Share_Directory_DB_Name, DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream;
var
  task_: TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream;
begin
  task_ := TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream.Create;
  task_.Stream := Stream;
  task_.AutoFreeStream := AutoFreeStream;
  task_.Decrypt_Security := Decrypt_Security;
  task_.Decrypt_Key := Decrypt_Key;
  task_.Share_Directory_DB_Name := Share_Directory_DB_Name;
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.OwnerPool := self;
  if Assigned(On_Add_Task) then
      On_Add_Task(task_);
  Result := task_;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Add_Task_Post_File(Local_File, DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Post_File;
var
  task_: TC40_NetDisk_VM_Client_Task_Auto_Post_File;
begin
  task_ := TC40_NetDisk_VM_Client_Task_Auto_Post_File.Create;
  task_.Local_File := Local_File;
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.OwnerPool := self;
  if Assigned(On_Add_Task) then
      On_Add_Task(task_);
  Result := task_;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Add_Task_Post_Directory(Local_Directory, DB_Field: U_String): TC40_NetDisk_VM_Client_Task_Auto_Post_Directory;
var
  task_: TC40_NetDisk_VM_Client_Task_Auto_Post_Directory;
begin
  task_ := TC40_NetDisk_VM_Client_Task_Auto_Post_Directory.Create;
  task_.Local_Directory := Local_Directory;
  task_.DB_Field := DB_Field;
  task_.Pool_Ptr := FList.Add(task_);
  task_.OwnerPool := self;
  if Assigned(On_Add_Task) then
      On_Add_Task(task_);
  Result := task_;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Add_Task_Get_File(Local_File, DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_File;
var
  task_: TC40_NetDisk_VM_Client_Task_Auto_Get_File;
begin
  task_ := TC40_NetDisk_VM_Client_Task_Auto_Get_File.Create;
  task_.Local_File := Local_File;
  task_.Share_Directory_DB_Name := '';
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.OwnerPool := self;
  if Assigned(On_Add_Task) then
      On_Add_Task(task_);
  Result := task_;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Add_Task_Get_Share_File(Share_Directory_DB_Name, Local_File, DB_Field, DB_Item: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_File;
var
  task_: TC40_NetDisk_VM_Client_Task_Auto_Get_File;
begin
  task_ := TC40_NetDisk_VM_Client_Task_Auto_Get_File.Create;
  task_.Local_File := Local_File;
  task_.Share_Directory_DB_Name := Share_Directory_DB_Name;
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.OwnerPool := self;
  if Assigned(On_Add_Task) then
      On_Add_Task(task_);
  Result := task_;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Add_Task_Get_Directory(Local_Directory, DB_Field: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_Directory;
var
  task_: TC40_NetDisk_VM_Client_Task_Auto_Get_Directory;
begin
  task_ := TC40_NetDisk_VM_Client_Task_Auto_Get_Directory.Create;
  task_.Local_Directory := Local_Directory;
  task_.Share_Directory_DB_Name := '';
  task_.DB_Field := DB_Field;
  task_.Pool_Ptr := FList.Add(task_);
  task_.OwnerPool := self;
  if Assigned(On_Add_Task) then
      On_Add_Task(task_);
  Result := task_;
end;

function TC40_NetDisk_VM_Client_Task_Tool.Add_Task_Get_Share_Directory(Share_Directory_DB_Name, Local_Directory, DB_Field: U_String): TC40_NetDisk_VM_Client_Task_Auto_Get_Directory;
var
  task_: TC40_NetDisk_VM_Client_Task_Auto_Get_Directory;
begin
  task_ := TC40_NetDisk_VM_Client_Task_Auto_Get_Directory.Create;
  task_.Local_Directory := Local_Directory;
  task_.Share_Directory_DB_Name := Share_Directory_DB_Name;
  task_.DB_Field := DB_Field;
  task_.Pool_Ptr := FList.Add(task_);
  task_.OwnerPool := self;
  if Assigned(On_Add_Task) then
      On_Add_Task(task_);
  Result := task_;
end;

procedure TC40_NetDisk_VM_Client_Task.Do_Go_Next_Task;
var
  __Repeat__: TC40_NetDisk_VM_Client_Task_List.TRepeat___;
begin
  Client.DTNoAuth.ProgressEngine.PostDelayFreeObject(0, self);
  if (OwnerPool <> nil) and (Pool_Ptr <> nil) then
    begin
      if OwnerPool.Task_Num > 0 then
        begin
          __Repeat__ := OwnerPool.Repeat_;
          repeat
            if __Repeat__.Queue <> Pool_Ptr then
              if not __Repeat__.Queue^.Data.FTask_Is_Busy then
                begin
                  if Assigned(OwnerPool.On_Run) then
                      OwnerPool.On_Run(self);
                  if Assigned(On_Run) then
                      On_Run(self);
                  __Repeat__.Queue^.Data.FTask_Is_Busy := True;
                  __Repeat__.Queue^.Data.Do_Run_Task;
                  exit;
                end;
          until not __Repeat__.Next;
        end;
    end;
end;

constructor TC40_NetDisk_VM_Client_Task.Create;
begin
  inherited Create;
  OwnerPool := nil;
  Pool_Ptr := nil;
  FTask_Is_Busy := False;
  AtomInc(Task_Seed_No);
  FTask_No := Task_Seed_No;
  UserData := nil;
  UserData2 := nil;
  UserDataInfo := '';
  On_Run := nil;
  On_Done := nil;
end;

destructor TC40_NetDisk_VM_Client_Task.Destroy;
begin
  if (OwnerPool <> nil) and (Pool_Ptr <> nil) then
    begin
      if Assigned(OwnerPool.On_Done) then
          OwnerPool.On_Done(self);
      if Assigned(On_Done) then
          On_Done(self);
      Pool_Ptr^.Data := nil;
      OwnerPool.FList.Remove(Pool_Ptr);
      if (OwnerPool.Task_Num = 0) then
          Client.DTNoAuth.ProgressEngine.PostExecuteM_NP(0, {$IFDEF FPC}@{$ENDIF FPC}OwnerPool.Do_All_Done);
    end;
  inherited Destroy;
end;

procedure TC40_NetDisk_VM_Client_Task.Do_Run_Task;
begin

end;

procedure TC40_NetDisk_VM_Client_Task.Run_Task;
begin
  if Assigned(OwnerPool.On_Run) then
      OwnerPool.On_Run(self);
  if Assigned(On_Run) then
      On_Run(self);
  Do_Run_Task;
end;

procedure TC40_NetDisk_VM_Client_Task.Go_Next_Task;
begin
  Client.DTNoAuth.ProgressEngine.PostExecuteM_NP(0, {$IFDEF FPC}@{$ENDIF FPC}Do_Go_Next_Task);
end;

function TC40_NetDisk_VM_Client_Task.Client: TC40_NetDisk_VM_Client;
begin
  Result := OwnerPool.Client;
end;

constructor TC40_NetDisk_VM_Client_Task_Clone_Connection.Create;
begin
  inherited Create;
end;

procedure TC40_NetDisk_VM_Client_Task_Clone_Connection.Do_Clone_Done(sender: TC40_NetDisk_VM_Client; New_Instance: TC40_NetDisk_VM_Client);
begin
  if New_Instance <> nil then
    begin
      New_Instance.OnEvent := OwnerPool.Client.OnEvent;
      OwnerPool.Client := New_Instance;
      OwnerPool.All_Done_Do_Auto_Free_Client := True;
      Go_Next_Task;
    end;
end;

procedure TC40_NetDisk_VM_Client_Task_Clone_Connection.Do_Run_Task;
begin
  Client.Clone_M({$IFDEF FPC}@{$ENDIF FPC}Do_Clone_Done);
end;

constructor TC40_NetDisk_VM_Client_Task_Auto_Post_Stream.Create;
begin
  inherited Create;
  Stream := nil;
  AutoFreeStream := False;
  DB_Field := '';
  DB_Item := '';
  On_Post_Stream_Done := nil;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Post_Stream.Do_Usr_Auto_Post_File(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object;
  Stream_: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String);
begin
  if Assigned(On_Post_Stream_Done) then
      On_Post_Stream_Done(self, Stream_, Stream_Final_MD5__, Successed, info);
  Go_Next_Task;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Post_Stream.Do_Run_Task;
begin
  Client.Auto_Post_File_M(self, Stream, AutoFreeStream, umlNow(), DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_Auto_Post_File);
end;

constructor TC40_NetDisk_VM_Client_Task_Auto_Get_Stream.Create;
begin
  inherited Create;
  Stream := nil;
  AutoFreeStream := False;
  Share_Directory_DB_Name := '';
  DB_Field := '';
  DB_Item := '';
  On_Get_Stream_Done := nil;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Get_Stream.Do_Usr_Auto_Get_File(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object;
  Stream_: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String);
begin
  if Assigned(On_Get_Stream_Done) then
      On_Get_Stream_Done(self, Stream_, Stream_Final_MD5__, Successed, info);
  Go_Next_Task;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Get_Stream.Do_Run_Task;
begin
  if Share_Directory_DB_Name <> '' then
      Client.Auto_Get_File_From_Share_Disk_M(self, Stream, AutoFreeStream, Share_Directory_DB_Name, DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_Auto_Get_File)
  else
      Client.Auto_Get_File_M(self, Stream, AutoFreeStream, DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_Auto_Get_File);
end;

constructor TC40_NetDisk_VM_Client_Task_Auto_Post_Encrypt_Stream.Create;
begin
  inherited Create;
  Stream := nil;
  AutoFreeStream := False;
  Encrypt_Stream := TMS64.Create;
  Encrypt_Security := TCipherSecurity.csRijndael;
  Encrypt_Key := C40_Password;
  DB_Field := '';
  DB_Item := '';
  On_Post_Encrypt_Stream_Done := nil;
  Prepare_Done := False;
  Source_MD5 := Null_MD5;
  Encrypt_MD5 := Null_MD5;
end;

destructor TC40_NetDisk_VM_Client_Task_Auto_Post_Encrypt_Stream.Destroy;
begin
  if AutoFreeStream then
      DisposeObjectAndNil(Stream);
  DisposeObject(Encrypt_Stream);
  inherited Destroy;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Post_Encrypt_Stream.Th_Encrypt;
var
  enc_: TCipher_Base;
begin
  Encrypt_Stream.Size := Stream.Size;
  Stream.Position := 0;
  Stream.Read(Encrypt_Stream.Memory^, Stream.Size);
  Source_MD5 := umlMD5(Encrypt_Stream.Memory, Stream.Size);

  enc_ := CreateCipherClassFromPassword(Encrypt_Security, Encrypt_Key);
  enc_.Level := 1;
  enc_.ProcessTail := True;
  enc_.CBC := True;
  enc_.Encrypt(Encrypt_Stream.Memory, Encrypt_Stream.Size);
  DisposeObject(enc_);
  Prepare_Done := True;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Post_Encrypt_Stream.Do_Usr_Auto_Post_File(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object;
  Stream_: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String);
begin
  if Assigned(On_Post_Encrypt_Stream_Done) then
    begin
      Stream.Position := 0;
      Encrypt_Stream.Position := 0;
      Encrypt_MD5 := Stream_Final_MD5__;
      On_Post_Encrypt_Stream_Done(self, Stream, Encrypt_Stream, Successed, info);
    end;
  Go_Next_Task;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Post_Encrypt_Stream.Do_Run_Task;
begin
  while not Prepare_Done do
      TCompute.Sleep(1);
  Client.Auto_Post_File_M(self, Encrypt_Stream, False, umlNow(), DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_Auto_Post_File);
end;

constructor TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream.Create;
begin
  inherited Create;
  Stream := nil;
  AutoFreeStream := False;
  Decrypt_Security := TCipherSecurity.csRijndael;
  Decrypt_Key := C40_Password;
  Share_Directory_DB_Name := '';
  DB_Field := '';
  DB_Item := '';
  On_Get_Decrypt_Stream_Done := nil;
end;

destructor TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream.Destroy;
begin
  if AutoFreeStream then
      DisposeObject(Stream);
  inherited Destroy;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream.Do_Usr_Auto_Get_File(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object;
  Stream_: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String);
var
  Decrypt_Stream: TMS64;
  enc_: TCipher_Base;
begin
  if Successed then
    begin
      Decrypt_Stream := TMS64(Stream_);

      enc_ := CreateCipherClassFromPassword(Decrypt_Security, Decrypt_Key);
      enc_.Level := 1;
      enc_.ProcessTail := True;
      enc_.CBC := True;
      enc_.Decrypt(Decrypt_Stream.Memory, Decrypt_Stream.Size);
      DisposeObject(enc_);
      if Stream is TMS64 then
        begin
          TMS64(Stream).SwapInstance(Decrypt_Stream);
        end
      else
        begin
          Stream.Position := 0;
          Stream.Size := Decrypt_Stream.Size;
          Stream.Write(Decrypt_Stream.Memory^, Decrypt_Stream.Size);
        end;
      Stream.Position := 0;
    end;

  if Assigned(On_Get_Decrypt_Stream_Done) then
      On_Get_Decrypt_Stream_Done(self, Stream, Stream_Final_MD5__, Successed, info);
  Go_Next_Task;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Get_Decrypt_Stream.Do_Run_Task;
begin
  if Share_Directory_DB_Name <> '' then
      Client.Auto_Get_File_From_Share_Disk_M(self, TMS64.Create, True, Share_Directory_DB_Name, DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_Auto_Get_File)
  else
      Client.Auto_Get_File_M(self, TMS64.Create, True, DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_Auto_Get_File);
end;

constructor TC40_NetDisk_VM_Client_Task_Auto_Post_File.Create;
begin
  inherited Create;
  Local_File := '';
  DB_Field := '';
  DB_Item := '';
end;

destructor TC40_NetDisk_VM_Client_Task_Auto_Post_File.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Post_File.Do_Usr_Auto_Post_File(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object;
  Stream: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String);
begin
  Go_Next_Task;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Post_File.Do_Run_Task;
var
  fs: TCore_FileStream;
begin
  try
      fs := TCore_FileStream.Create(Local_File, fmOpenRead or fmShareDenyNone);
  except
    Go_Next_Task;
    exit;
  end;
  Client.Auto_Post_File_M(self, fs, True, umlGetFileTime(Local_File), DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_Auto_Post_File);
end;

constructor TC40_NetDisk_VM_Client_Task_Auto_Get_File.Create;
begin
  inherited Create;
  Share_Directory_DB_Name := '';
  Local_File := '';
  DB_Field := '';
  DB_Item := '';
end;

destructor TC40_NetDisk_VM_Client_Task_Auto_Get_File.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Get_File.Do_Usr_Auto_Get_File(sender: TC40_NetDisk_VM_Client; UserData: TCore_Object;
  Stream: TCore_Stream; Stream_Final_MD5__: TMD5; Successed: Boolean; info: U_String);
begin
  Go_Next_Task;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Get_File.Do_Run_Task;
var
  fs: TCore_FileStream;
begin
  try
      fs := TCore_FileStream.Create(Local_File, fmCreate);
  except
    Go_Next_Task;
    exit;
  end;

  if Share_Directory_DB_Name <> '' then
      Client.Auto_Get_File_From_Share_Disk_M(self, fs, True, Share_Directory_DB_Name, DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_Auto_Get_File)
  else
      Client.Auto_Get_File_M(self, fs, True, DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_Auto_Get_File);
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Get_Directory.Do_Done_Get_Directory_Task_Pool(sender: TC40_NetDisk_VM_Client_Task_Tool);
begin
  Go_Next_Task;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Get_Directory.Do_Search_NetDisk_File(sender: TC40_NetDisk_VM_Client; Successed: Boolean; info: U_String;
  arry: TC40_NetDisk_VM_Client_On_Usr_Search_NetDisk_File_Data_array);
var
  i: Integer;
  L_Dir, L_Name: U_String;
begin
  if not Successed then
    begin
      Go_Next_Task;
      exit;
    end;
  umlCreateDirectory(Local_Directory);
  for i := Low(arry) to high(arry) do
    begin
      L_Dir := umlCombinePath(Local_Directory, arry[i].Current_Field);
      umlCreateDirectory(L_Dir);
      if umlMultipleMatch('i:*', arry[i].FieldOrItem) then
        begin
          L_Name := umlDeleteFirstStr(arry[i].FieldOrItem, ':');

          if Share_Directory_DB_Name <> '' then
              Get_Directory_Task_Pool.Add_Task_Get_Share_File(Share_Directory_DB_Name,
              umlCombineFileName(L_Dir, L_Name),
              umlCombineUnixPath(DB_Field, arry[i].Current_Field),
              L_Name)
          else
              Get_Directory_Task_Pool.Add_Task_Get_File(umlCombineFileName(L_Dir, L_Name),
              umlCombineUnixPath(DB_Field, arry[i].Current_Field),
              L_Name);
        end;
    end;
  if Get_Directory_Task_Pool.Task_Num > 0 then
    begin
      Get_Directory_Task_Pool.First.Do_Run_Task;
    end
  else
      Go_Next_Task;
end;

constructor TC40_NetDisk_VM_Client_Task_Auto_Get_Directory.Create;
begin
  inherited Create;
  Share_Directory_DB_Name := '';
  Local_Directory := '';
  DB_Field := '';
  Get_Directory_Task_Pool := nil;
end;

destructor TC40_NetDisk_VM_Client_Task_Auto_Get_Directory.Destroy;
begin
  DisposeObjectAndNil(Get_Directory_Task_Pool);
  inherited Destroy;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Get_Directory.Do_Run_Task;
begin
  Get_Directory_Task_Pool := TC40_NetDisk_VM_Client_Task_Tool.Create(Client);
  Get_Directory_Task_Pool.All_Done_Do_Auto_Free_Self := False;
  Get_Directory_Task_Pool.On_All_Done := {$IFDEF FPC}@{$ENDIF FPC}Do_Done_Get_Directory_Task_Pool;
  Get_Directory_Task_Pool.On_Add_Task := OwnerPool.On_Add_Task;
  Get_Directory_Task_Pool.On_Run := OwnerPool.On_Run;
  Get_Directory_Task_Pool.On_Done := OwnerPool.On_Done;

  if Share_Directory_DB_Name <> '' then
      Client.Search_Share_NetDisk_File_M(Share_Directory_DB_Name, DB_Field, '*', {$IFDEF FPC}@{$ENDIF FPC}Do_Search_NetDisk_File)
  else
      Client.Search_NetDisk_File_M(DB_Field, '*', {$IFDEF FPC}@{$ENDIF FPC}Do_Search_NetDisk_File);
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Post_Directory.Do_Done_Post_Directory_Task_Pool(sender: TC40_NetDisk_VM_Client_Task_Tool);
begin
  Go_Next_Task;
end;

constructor TC40_NetDisk_VM_Client_Task_Auto_Post_Directory.Create;
begin
  inherited Create;
  Local_Directory := '';
  DB_Field := '';
  Post_Directory_Task_Pool := nil;
end;

destructor TC40_NetDisk_VM_Client_Task_Auto_Post_Directory.Destroy;
begin
  DisposeObjectAndNil(Post_Directory_Task_Pool);
  inherited Destroy;
end;

procedure TC40_NetDisk_VM_Client_Task_Auto_Post_Directory.Do_Run_Task;
  procedure Do_Scan_Directory(L_Directory_, R_Field_: U_String);
  var
    arry: U_StringArray;
    i: Integer;
  begin
    Client.CreateField(R_Field_);
    arry := umlGetDirListPath(L_Directory_);
    for i := low(arry) to high(arry) do
        Do_Scan_Directory(umlCombinePath(L_Directory_, arry[i]), umlCombineUnixPath(R_Field_, arry[i]));

    arry := umlGetFileListPath(L_Directory_);
    for i := low(arry) to high(arry) do
        Post_Directory_Task_Pool.Add_Task_Post_File(umlCombineFileName(L_Directory_, arry[i]), R_Field_, arry[i]);
  end;

begin
  Post_Directory_Task_Pool := TC40_NetDisk_VM_Client_Task_Tool.Create(Client);
  Post_Directory_Task_Pool.All_Done_Do_Auto_Free_Self := False;
  Post_Directory_Task_Pool.On_All_Done := {$IFDEF FPC}@{$ENDIF FPC}Do_Done_Post_Directory_Task_Pool;
  Post_Directory_Task_Pool.On_Add_Task := OwnerPool.On_Add_Task;
  Post_Directory_Task_Pool.On_Run := OwnerPool.On_Run;
  Post_Directory_Task_Pool.On_Done := OwnerPool.On_Done;

  Do_Scan_Directory(Local_Directory, DB_Field);
  if Post_Directory_Task_Pool.Task_Num > 0 then
      Post_Directory_Task_Pool.First.Do_Run_Task
  else
      Go_Next_Task;
end;

initialization

Task_Seed_No := 0;

end.
