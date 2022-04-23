{ ****************************************************************************** }
{ * cloud 4.0 network disk Client Task tool                                    * }
{ ****************************************************************************** }
unit Z.Net.C4_NetDisk_Client.Task;

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
  Z.Net.C4_NetDisk_Client;

type
  TC40_NetDisk_Client_Task = class;
  TC40_NetDisk_Client_Task_Auto_Post_Stream = class;
  TC40_NetDisk_Client_Task_Auto_Get_Stream = class;
  TC40_NetDisk_Client_Task_Auto_Post_File = class;
  TC40_NetDisk_Client_Task_Auto_Get_File = class;
  TC40_NetDisk_Client_Task_Auto_Get_Directory = class;
  TC40_NetDisk_Client_Task_Auto_Post_Directory = class;
  TC40_NetDisk_Client_Task_List = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalBigList<TC40_NetDisk_Client_Task>;

  TC40_NetDisk_Client_Task_Tool = class;
  TC40_NetDisk_Client_Task_Pool_Event = procedure(sender: TC40_NetDisk_Client_Task_Tool) of object;
  TC40_NetDisk_Client_Task_Event = procedure(sender: TC40_NetDisk_Client_Task) of object;

  TC40_NetDisk_Client_Task_Tool = class
  private
    FList: TC40_NetDisk_Client_Task_List;
    procedure Do_All_Done();
    procedure DoFree(var Data: TC40_NetDisk_Client_Task);
  public
    All_Done_Do_Auto_Free: Boolean;
    On_All_Done: TC40_NetDisk_Client_Task_Pool_Event;
    On_Run: TC40_NetDisk_Client_Task_Event;
    On_Done: TC40_NetDisk_Client_Task_Event;
    constructor Create;
    destructor Destroy; override;
    procedure All_Done; virtual;
    function Task_Num: NativeInt;
    function Repeat_: TC40_NetDisk_Client_Task_List.TRepeat___;
    function First: TC40_NetDisk_Client_Task;
    // stream
    function Add_Task_Post_Stream(Client: TC40_NetDisk_Client; Stream: TCore_Stream; AutoFreeStream: Boolean; DB_Field, DB_Item: U_String): TC40_NetDisk_Client_Task_Auto_Post_Stream;
    function Add_Task_Get_Stream(Client: TC40_NetDisk_Client; Stream: TCore_Stream; AutoFreeStream: Boolean; DB_Field, DB_Item: U_String): TC40_NetDisk_Client_Task_Auto_Get_Stream;
    function Add_Task_Get_Share_Stream(Client: TC40_NetDisk_Client; Stream: TCore_Stream; AutoFreeStream: Boolean; Share_Directory_DB_Name, DB_Field, DB_Item: U_String): TC40_NetDisk_Client_Task_Auto_Get_Stream;
    // file
    function Add_Task_Post_File(Client: TC40_NetDisk_Client; Local_File, DB_Field, DB_Item: U_String): TC40_NetDisk_Client_Task_Auto_Post_File;
    function Add_Task_Post_Directory(Client: TC40_NetDisk_Client; Local_Directory, DB_Field: U_String): TC40_NetDisk_Client_Task_Auto_Post_Directory;
    function Add_Task_Get_File(Client: TC40_NetDisk_Client; Local_File, DB_Field, DB_Item: U_String): TC40_NetDisk_Client_Task_Auto_Get_File;
    function Add_Task_Get_Share_File(Client: TC40_NetDisk_Client; Share_Directory_DB_Name, Local_File, DB_Field, DB_Item: U_String): TC40_NetDisk_Client_Task_Auto_Get_File;
    function Add_Task_Get_Directory(Client: TC40_NetDisk_Client; Local_Directory, DB_Field: U_String): TC40_NetDisk_Client_Task_Auto_Get_Directory;
    function Add_Task_Get_Share_Directory(Client: TC40_NetDisk_Client; Share_Directory_DB_Name, Local_Directory, DB_Field: U_String): TC40_NetDisk_Client_Task_Auto_Get_Directory;
  end;

  TC40_NetDisk_Client_Task = class
  private
    Pool_Ptr: TC40_NetDisk_Client_Task_List.PQueueStruct;
    Task_Is_Busy: Boolean;
    procedure Do_Go_Next_Task;
  public
    Client: TC40_NetDisk_Client;
    OwnerPool: TC40_NetDisk_Client_Task_Tool;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Run_Task; virtual;
    procedure Run_Task;
    procedure Go_Next_Task;
  end;

  TC40_NetDisk_Client_Task_Auto_Post_Stream = class(TC40_NetDisk_Client_Task)
  public
    Stream: TCore_Stream;
    AutoFreeStream: Boolean;
    DB_Field, DB_Item: U_String;
    constructor Create;
    procedure Do_Usr_Auto_Post_File(sender: TC40_NetDisk_Client; UserData: TCore_Object; Stream: TCore_Stream; Successed: Boolean; info: U_String);
    procedure Do_Run_Task; override;
  end;

  TC40_NetDisk_Client_Task_Auto_Get_Stream = class(TC40_NetDisk_Client_Task)
  public
    Stream: TCore_Stream;
    AutoFreeStream: Boolean;
    Share_Directory_DB_Name, DB_Field, DB_Item: U_String;
    constructor Create;
    procedure Do_Usr_Auto_Get_File(sender: TC40_NetDisk_Client; UserData: TCore_Object; Stream: TCore_Stream; Successed: Boolean; info: U_String);
    procedure Do_Run_Task; override;
  end;

  TC40_NetDisk_Client_Task_Auto_Post_File = class(TC40_NetDisk_Client_Task)
  public
    Local_File, DB_Field, DB_Item: U_String;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Usr_Auto_Post_File(sender: TC40_NetDisk_Client; UserData: TCore_Object; Stream: TCore_Stream; Successed: Boolean; info: U_String);
    procedure Do_Run_Task; override;
  end;

  TC40_NetDisk_Client_Task_Auto_Get_File = class(TC40_NetDisk_Client_Task)
  public
    Share_Directory_DB_Name, Local_File, DB_Field, DB_Item: U_String;
    constructor Create;
    destructor Destroy;
    procedure Do_Usr_Auto_Get_File(sender: TC40_NetDisk_Client; UserData: TCore_Object; Stream: TCore_Stream; Successed: Boolean; info: U_String);
    procedure Do_Run_Task; override;
  end;

  TC40_NetDisk_Client_Task_Auto_Get_Directory = class(TC40_NetDisk_Client_Task)
  public
    Share_Directory_DB_Name, Local_Directory, DB_Field: U_String;
    Get_Directory_Task_Pool: TC40_NetDisk_Client_Task_Tool;
    procedure Do_Done_Get_Directory_Task_Pool(sender: TC40_NetDisk_Client_Task_Tool);
    procedure Do_Search_NetDisk_File(sender: TC40_NetDisk_Client; Successed: Boolean; info: U_String;
      arry: TC40_NetDisk_Client_On_Usr_Search_NetDisk_File_Data_array);
    constructor Create;
    destructor Destroy; override;
    procedure Do_Run_Task; override;
  end;

  TC40_NetDisk_Client_Task_Auto_Post_Directory = class(TC40_NetDisk_Client_Task)
  public
    Local_Directory, DB_Field: U_String;
    Post_Directory_Task_Pool: TC40_NetDisk_Client_Task_Tool;
    procedure Do_Done_Post_Directory_Task_Pool(sender: TC40_NetDisk_Client_Task_Tool);
    constructor Create;
    destructor Destroy; override;
    procedure Do_Run_Task; override;
  end;

implementation

procedure TC40_NetDisk_Client_Task_Tool.Do_All_Done;
begin
  All_Done;
  if Assigned(On_All_Done) then
      On_All_Done(self);

  if All_Done_Do_Auto_Free then
      Free;
end;

procedure TC40_NetDisk_Client_Task_Tool.DoFree(var Data: TC40_NetDisk_Client_Task);
begin
  if Data <> nil then
    begin
      Data.OwnerPool := nil;
      Data.Pool_Ptr := nil;
      DisposeObjectAndNil(Data);
    end;
end;

constructor TC40_NetDisk_Client_Task_Tool.Create;
begin
  inherited Create;
  FList := TC40_NetDisk_Client_Task_List.Create;
  FList.OnFree := {$IFDEF FPC}@{$ENDIF FPC}DoFree;
  All_Done_Do_Auto_Free := True;
  On_All_Done := nil;
  On_Run := nil;
  On_Done := nil;
end;

destructor TC40_NetDisk_Client_Task_Tool.Destroy;
begin
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TC40_NetDisk_Client_Task_Tool.All_Done;
begin
end;

function TC40_NetDisk_Client_Task_Tool.Task_Num: NativeInt;
begin
  Result := FList.Num;
end;

function TC40_NetDisk_Client_Task_Tool.Repeat_: TC40_NetDisk_Client_Task_List.TRepeat___;
begin
  Result := FList.Repeat_;
end;

function TC40_NetDisk_Client_Task_Tool.First: TC40_NetDisk_Client_Task;
begin
  Result := nil;
  if FList.First <> nil then
      Result := FList.First^.Data;
end;

function TC40_NetDisk_Client_Task_Tool.Add_Task_Post_Stream(Client: TC40_NetDisk_Client; Stream: TCore_Stream; AutoFreeStream: Boolean; DB_Field, DB_Item: U_String): TC40_NetDisk_Client_Task_Auto_Post_Stream;
var
  task_: TC40_NetDisk_Client_Task_Auto_Post_Stream;
begin
  task_ := TC40_NetDisk_Client_Task_Auto_Post_Stream.Create;
  task_.Stream := Stream;
  task_.AutoFreeStream := AutoFreeStream;
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.Client := Client;
  task_.OwnerPool := self;
  Result := task_;
end;

function TC40_NetDisk_Client_Task_Tool.Add_Task_Get_Stream(Client: TC40_NetDisk_Client; Stream: TCore_Stream; AutoFreeStream: Boolean; DB_Field, DB_Item: U_String): TC40_NetDisk_Client_Task_Auto_Get_Stream;
var
  task_: TC40_NetDisk_Client_Task_Auto_Get_Stream;
begin
  task_ := TC40_NetDisk_Client_Task_Auto_Get_Stream.Create;
  task_.Stream := Stream;
  task_.AutoFreeStream := AutoFreeStream;
  task_.Share_Directory_DB_Name := '';
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.Client := Client;
  task_.OwnerPool := self;
  Result := task_;
end;

function TC40_NetDisk_Client_Task_Tool.Add_Task_Get_Share_Stream(Client: TC40_NetDisk_Client; Stream: TCore_Stream; AutoFreeStream: Boolean; Share_Directory_DB_Name, DB_Field, DB_Item: U_String): TC40_NetDisk_Client_Task_Auto_Get_Stream;
var
  task_: TC40_NetDisk_Client_Task_Auto_Get_Stream;
begin
  task_ := TC40_NetDisk_Client_Task_Auto_Get_Stream.Create;
  task_.Stream := Stream;
  task_.AutoFreeStream := AutoFreeStream;
  task_.Share_Directory_DB_Name := Share_Directory_DB_Name;
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.Client := Client;
  task_.OwnerPool := self;
  Result := task_;
end;

function TC40_NetDisk_Client_Task_Tool.Add_Task_Post_File(Client: TC40_NetDisk_Client; Local_File, DB_Field, DB_Item: U_String): TC40_NetDisk_Client_Task_Auto_Post_File;
var
  task_: TC40_NetDisk_Client_Task_Auto_Post_File;
begin
  task_ := TC40_NetDisk_Client_Task_Auto_Post_File.Create;
  task_.Local_File := Local_File;
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.Client := Client;
  task_.OwnerPool := self;
  Result := task_;
end;

function TC40_NetDisk_Client_Task_Tool.Add_Task_Post_Directory(Client: TC40_NetDisk_Client; Local_Directory, DB_Field: U_String): TC40_NetDisk_Client_Task_Auto_Post_Directory;
var
  task_: TC40_NetDisk_Client_Task_Auto_Post_Directory;
begin
  task_ := TC40_NetDisk_Client_Task_Auto_Post_Directory.Create;
  task_.Local_Directory := Local_Directory;
  task_.DB_Field := DB_Field;
  task_.Pool_Ptr := FList.Add(task_);
  task_.Client := Client;
  task_.OwnerPool := self;
  Result := task_;
end;

function TC40_NetDisk_Client_Task_Tool.Add_Task_Get_File(Client: TC40_NetDisk_Client; Local_File, DB_Field, DB_Item: U_String): TC40_NetDisk_Client_Task_Auto_Get_File;
var
  task_: TC40_NetDisk_Client_Task_Auto_Get_File;
begin
  task_ := TC40_NetDisk_Client_Task_Auto_Get_File.Create;
  task_.Local_File := Local_File;
  task_.Share_Directory_DB_Name := '';
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.Client := Client;
  task_.OwnerPool := self;
  Result := task_;
end;

function TC40_NetDisk_Client_Task_Tool.Add_Task_Get_Share_File(Client: TC40_NetDisk_Client; Share_Directory_DB_Name, Local_File, DB_Field, DB_Item: U_String): TC40_NetDisk_Client_Task_Auto_Get_File;
var
  task_: TC40_NetDisk_Client_Task_Auto_Get_File;
begin
  task_ := TC40_NetDisk_Client_Task_Auto_Get_File.Create;
  task_.Local_File := Local_File;
  task_.Share_Directory_DB_Name := Share_Directory_DB_Name;
  task_.DB_Field := DB_Field;
  task_.DB_Item := DB_Item;
  task_.Pool_Ptr := FList.Add(task_);
  task_.Client := Client;
  task_.OwnerPool := self;
  Result := task_;
end;

function TC40_NetDisk_Client_Task_Tool.Add_Task_Get_Directory(Client: TC40_NetDisk_Client; Local_Directory, DB_Field: U_String): TC40_NetDisk_Client_Task_Auto_Get_Directory;
var
  task_: TC40_NetDisk_Client_Task_Auto_Get_Directory;
begin
  task_ := TC40_NetDisk_Client_Task_Auto_Get_Directory.Create;
  task_.Local_Directory := Local_Directory;
  task_.Share_Directory_DB_Name := '';
  task_.DB_Field := DB_Field;
  task_.Pool_Ptr := FList.Add(task_);
  task_.Client := Client;
  task_.OwnerPool := self;
  Result := task_;
end;

function TC40_NetDisk_Client_Task_Tool.Add_Task_Get_Share_Directory(Client: TC40_NetDisk_Client; Share_Directory_DB_Name, Local_Directory, DB_Field: U_String): TC40_NetDisk_Client_Task_Auto_Get_Directory;
var
  task_: TC40_NetDisk_Client_Task_Auto_Get_Directory;
begin
  task_ := TC40_NetDisk_Client_Task_Auto_Get_Directory.Create;
  task_.Local_Directory := Local_Directory;
  task_.Share_Directory_DB_Name := Share_Directory_DB_Name;
  task_.DB_Field := DB_Field;
  task_.Pool_Ptr := FList.Add(task_);
  task_.Client := Client;
  task_.OwnerPool := self;
  Result := task_;
end;

procedure TC40_NetDisk_Client_Task.Do_Go_Next_Task;
var
  __Repeat__: TC40_NetDisk_Client_Task_List.TRepeat___;
begin
  Client.DTNoAuth.ProgressEngine.PostDelayFreeObject(0, self);
  if (OwnerPool <> nil) and (Pool_Ptr <> nil) then
    begin
      if OwnerPool.Task_Num > 0 then
        begin
          __Repeat__ := OwnerPool.Repeat_;
          repeat
            if __Repeat__.Queue <> Pool_Ptr then
              if not __Repeat__.Queue^.Data.Task_Is_Busy then
                begin
                  if Assigned(OwnerPool.On_Run) then
                      OwnerPool.On_Run(self);
                  __Repeat__.Queue^.Data.Task_Is_Busy := True;
                  __Repeat__.Queue^.Data.Do_Run_Task;
                  exit;
                end;
          until not __Repeat__.Next;
        end;
    end;
end;

constructor TC40_NetDisk_Client_Task.Create;
begin
  inherited Create;
  Pool_Ptr := nil;
  Client := nil;
  OwnerPool := nil;
  Task_Is_Busy := False;
end;

destructor TC40_NetDisk_Client_Task.Destroy;
begin
  if (OwnerPool <> nil) and (Pool_Ptr <> nil) then
    begin
      if Assigned(OwnerPool.On_Done) then
          OwnerPool.On_Done(self);
      Pool_Ptr^.Data := nil;
      OwnerPool.FList.Remove(Pool_Ptr);
      if (OwnerPool.Task_Num = 0) then
          Client.DTNoAuth.ProgressEngine.PostExecuteM_NP(0, {$IFDEF FPC}@{$ENDIF FPC}OwnerPool.Do_All_Done);
    end;
  inherited Destroy;
end;

procedure TC40_NetDisk_Client_Task.Do_Run_Task;
begin

end;

procedure TC40_NetDisk_Client_Task.Run_Task;
begin
  if Assigned(OwnerPool.On_Run) then
      OwnerPool.On_Run(self);
  Do_Run_Task;
end;

procedure TC40_NetDisk_Client_Task.Go_Next_Task;
begin
  Client.DTNoAuth.ProgressEngine.PostExecuteM_NP(0, {$IFDEF FPC}@{$ENDIF FPC}Do_Go_Next_Task);
end;

constructor TC40_NetDisk_Client_Task_Auto_Post_Stream.Create;
begin
  inherited Create;
  Stream := nil;
  AutoFreeStream := False;
  DB_Field := '';
  DB_Item := '';
end;

procedure TC40_NetDisk_Client_Task_Auto_Post_Stream.Do_Usr_Auto_Post_File(sender: TC40_NetDisk_Client; UserData: TCore_Object; Stream: TCore_Stream; Successed: Boolean; info: U_String);
begin
  Go_Next_Task;
end;

procedure TC40_NetDisk_Client_Task_Auto_Post_Stream.Do_Run_Task;
begin
  Client.Auto_Post_File_M(self, Stream, AutoFreeStream, umlNow(), DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_Auto_Post_File);
end;

constructor TC40_NetDisk_Client_Task_Auto_Get_Stream.Create;
begin
  inherited Create;
  Stream := nil;
  AutoFreeStream := False;
  Share_Directory_DB_Name := '';
  DB_Field := '';
  DB_Item := '';
end;

procedure TC40_NetDisk_Client_Task_Auto_Get_Stream.Do_Usr_Auto_Get_File(sender: TC40_NetDisk_Client; UserData: TCore_Object; Stream: TCore_Stream; Successed: Boolean; info: U_String);
begin
  Go_Next_Task;
end;

procedure TC40_NetDisk_Client_Task_Auto_Get_Stream.Do_Run_Task;
begin
  if Share_Directory_DB_Name <> '' then
      Client.Auto_Get_File_From_Share_Disk_M(self, Stream, AutoFreeStream, Share_Directory_DB_Name, DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_Auto_Get_File)
  else
      Client.Auto_Get_File_M(self, Stream, AutoFreeStream, DB_Field, DB_Item, {$IFDEF FPC}@{$ENDIF FPC}Do_Usr_Auto_Get_File);
end;

constructor TC40_NetDisk_Client_Task_Auto_Post_File.Create;
begin
  inherited Create;
  Local_File := '';
  DB_Field := '';
  DB_Item := '';
end;

destructor TC40_NetDisk_Client_Task_Auto_Post_File.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_NetDisk_Client_Task_Auto_Post_File.Do_Usr_Auto_Post_File(sender: TC40_NetDisk_Client; UserData: TCore_Object; Stream: TCore_Stream; Successed: Boolean; info: U_String);
begin
  Go_Next_Task;
end;

procedure TC40_NetDisk_Client_Task_Auto_Post_File.Do_Run_Task;
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

constructor TC40_NetDisk_Client_Task_Auto_Get_File.Create;
begin
  inherited Create;
  Share_Directory_DB_Name := '';
  Local_File := '';
  DB_Field := '';
  DB_Item := '';
end;

destructor TC40_NetDisk_Client_Task_Auto_Get_File.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_NetDisk_Client_Task_Auto_Get_File.Do_Usr_Auto_Get_File(sender: TC40_NetDisk_Client; UserData: TCore_Object; Stream: TCore_Stream; Successed: Boolean; info: U_String);
begin
  Go_Next_Task;
end;

procedure TC40_NetDisk_Client_Task_Auto_Get_File.Do_Run_Task;
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

procedure TC40_NetDisk_Client_Task_Auto_Get_Directory.Do_Done_Get_Directory_Task_Pool(sender: TC40_NetDisk_Client_Task_Tool);
begin
  Go_Next_Task;
end;

procedure TC40_NetDisk_Client_Task_Auto_Get_Directory.Do_Search_NetDisk_File(sender: TC40_NetDisk_Client; Successed: Boolean; info: U_String;
  arry: TC40_NetDisk_Client_On_Usr_Search_NetDisk_File_Data_array);
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
              Get_Directory_Task_Pool.Add_Task_Get_Share_File(Client,
              Share_Directory_DB_Name,
              umlCombineFileName(L_Dir, L_Name),
              umlCombineUnixPath(DB_Field, arry[i].Current_Field),
              L_Name)
          else
              Get_Directory_Task_Pool.Add_Task_Get_File(Client,
              umlCombineFileName(L_Dir, L_Name),
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

constructor TC40_NetDisk_Client_Task_Auto_Get_Directory.Create;
begin
  inherited Create;
  Share_Directory_DB_Name := '';
  Local_Directory := '';
  DB_Field := '';
  Get_Directory_Task_Pool := TC40_NetDisk_Client_Task_Tool.Create;
  Get_Directory_Task_Pool.All_Done_Do_Auto_Free := False;
  Get_Directory_Task_Pool.On_All_Done := {$IFDEF FPC}@{$ENDIF FPC}Do_Done_Get_Directory_Task_Pool;
end;

destructor TC40_NetDisk_Client_Task_Auto_Get_Directory.Destroy;
begin
  DisposeObject(Get_Directory_Task_Pool);
  inherited Destroy;
end;

procedure TC40_NetDisk_Client_Task_Auto_Get_Directory.Do_Run_Task;
begin
  if Share_Directory_DB_Name <> '' then
      Client.Search_Share_NetDisk_File_M(Share_Directory_DB_Name, DB_Field, '*', {$IFDEF FPC}@{$ENDIF FPC}Do_Search_NetDisk_File)
  else
      Client.Search_NetDisk_File_M(DB_Field, '*', {$IFDEF FPC}@{$ENDIF FPC}Do_Search_NetDisk_File);
end;

procedure TC40_NetDisk_Client_Task_Auto_Post_Directory.Do_Done_Post_Directory_Task_Pool(sender: TC40_NetDisk_Client_Task_Tool);
begin
  Go_Next_Task;
end;

constructor TC40_NetDisk_Client_Task_Auto_Post_Directory.Create;
begin
  inherited Create;
  Local_Directory := '';
  DB_Field := '';
  Post_Directory_Task_Pool := TC40_NetDisk_Client_Task_Tool.Create;
  Post_Directory_Task_Pool.All_Done_Do_Auto_Free := False;
  Post_Directory_Task_Pool.On_All_Done := {$IFDEF FPC}@{$ENDIF FPC}Do_Done_Post_Directory_Task_Pool;
end;

destructor TC40_NetDisk_Client_Task_Auto_Post_Directory.Destroy;
begin
  DisposeObject(Post_Directory_Task_Pool);
  inherited Destroy;
end;

procedure TC40_NetDisk_Client_Task_Auto_Post_Directory.Do_Run_Task;
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
        Post_Directory_Task_Pool.Add_Task_Post_File(Client, umlCombineFileName(L_Directory_, arry[i]), R_Field_, arry[i]);
  end;

begin
  Do_Scan_Directory(Local_Directory, DB_Field);
  if Post_Directory_Task_Pool.Task_Num > 0 then
      Post_Directory_Task_Pool.First.Do_Run_Task
  else
      Go_Next_Task;
end;

end.
