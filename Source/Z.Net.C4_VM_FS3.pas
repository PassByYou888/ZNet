(*
https://zpascal.net
https://github.com/PassByYou888/ZNet
https://github.com/PassByYou888/zRasterization
https://github.com/PassByYou888/ZSnappy
https://github.com/PassByYou888/Z-AI1.4
https://github.com/PassByYou888/InfiniteIoT
https://github.com/PassByYou888/zMonitor_3rd_Core
https://github.com/PassByYou888/tcmalloc4p
https://github.com/PassByYou888/jemalloc4p
https://github.com/PassByYou888/zCloud
https://github.com/PassByYou888/ZServer4D
https://github.com/PassByYou888/zShell
https://github.com/PassByYou888/ZDB2.0
https://github.com/PassByYou888/zGameWare
https://github.com/PassByYou888/CoreCipher
https://github.com/PassByYou888/zChinese
https://github.com/PassByYou888/zSound
https://github.com/PassByYou888/zExpression
https://github.com/PassByYou888/ZInstaller2.0
https://github.com/PassByYou888/zAI
https://github.com/PassByYou888/NetFileService
https://github.com/PassByYou888/zAnalysis
https://github.com/PassByYou888/PascalString
https://github.com/PassByYou888/zInstaller
https://github.com/PassByYou888/zTranslate
https://github.com/PassByYou888/zVision
https://github.com/PassByYou888/FFMPEG-Header
*)
{ ****************************************************************************** }
{ * cloud 4.0 File System 3.0 VM support                                       * }
{ ****************************************************************************** }
unit Z.Net.C4_VM_FS3;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ELSE FPC}
  System.IOUtils,
{$ENDIF FPC}
  Variants,
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine,
  Z.Geometry2D, Z.DFE, Z.Json, Z.Expression, Z.OpCode,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth, Z.Net.C4, Z.Net.C4.VM,
  Z.ZDB2, Z.ZDB2.Thread,
  Z.ZDB2.Thread.Queue,
  Z.Net.C4_FS3.ZDB2.LiteData;

type
  TC40_FS3_VM_Service = class;

  TC40_FS3_VM_Service_RecvTunnel_NoAuth = class(TService_RecvTunnel_UserDefine_NoAuth)
  public
    FS3_Service: TC40_FS3_VM_Service;
    Post_Queue_Tool: TZDB2_FS3_Sync_Post_Queue_Tool;
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TC40_FS3_VM_Service_SendTunnel_NoAuth = class(TService_SendTunnel_UserDefine_NoAuth)
  public
    FS3_Service: TC40_FS3_VM_Service;
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TC40_FS3_VM_Service_Get_File_Bridge = class
  public
    FS3_Service: TC40_FS3_VM_Service;
    Sequence_ID: Int64;
    bPos, ePos: Int64;
    Send_ID: Cardinal;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Run_Get_Th();
    procedure Do_Get_Fragment_Data(Sender: TZDB2_FS3_Lite; Successed: Boolean; Fragment: TMS64; Data_Pos: Int64);
    procedure Do_Get_Done(Sender: TZDB2_FS3_Lite; Successed: Boolean);
  end;

  TC40_FS3_VM_Service = class(TC40_NoAuth_VM_Service)
  protected
    procedure cmd_Get_Lite_Info(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Create_File_From_MD5(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Fast_Post(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure cmd_Begin_Post(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Post(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure cmd_End_Post(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_Get_File(Sender: TPeerIO; InData, OutData: TDFE);
    procedure do_Th_Get_File_List(thSender: THPC_Stream; ThInData, ThOutData: TDFE);
    procedure cmd_Get_File_List(Sender: TPeerIO; InData, OutData: TDFE);
  private
    // internal
    FLast_Life_Check_Time: TTimeTick;
  public
    // ZDB2 Lite-Data Engine
    FS3_Lite: TZDB2_FS3_Lite;
    FS3_Life_Check_Time: TTimeTick;
    constructor Create(Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
  end;

  TC40_FS3_VM_Client = class;
  TC40_FS3_VM_P2PVM_Recycle_Pool = TOrderStruct<TZNet_WithP2PVM_Client>;

  TC40_FS3_VM_Client_Post_File_Bridge = class;
  TC40_FS3_VM_Client_Get_File_Bridge = class;

  TC40_FS3_VM_Client_Post_File_DoneC = procedure(Sender: TC40_FS3_VM_Client; Bridge_: TC40_FS3_VM_Client_Post_File_Bridge; Successed: Boolean);
  TC40_FS3_VM_Client_Post_File_DoneM = procedure(Sender: TC40_FS3_VM_Client; Bridge_: TC40_FS3_VM_Client_Post_File_Bridge; Successed: Boolean) of object;
{$IFDEF FPC}
  TC40_FS3_VM_Client_Post_File_DoneP = procedure(Sender: TC40_FS3_VM_Client; Bridge_: TC40_FS3_VM_Client_Post_File_Bridge; Successed: Boolean) is nested;
{$ELSE FPC}
  TC40_FS3_VM_Client_Post_File_DoneP = reference to procedure(Sender: TC40_FS3_VM_Client; Bridge_: TC40_FS3_VM_Client_Post_File_Bridge; Successed: Boolean);
{$ENDIF FPC}

  TC40_FS3_VM_Client_Post_File_Bridge = class(TCore_Object_Intermediate)
  public
    p2pClient: TZNet_WithP2PVM_Client;
    Client: TC40_FS3_VM_Client;
    File_Name: U_String;
    File_Time: TDateTime;
    File_Life: Double;
    File_MD5: TMD5;
    Auto_Free_Stream: Boolean;
    Stream: TCore_Stream;
    OnResultC: TC40_FS3_VM_Client_Post_File_DoneC;
    OnResultM: TC40_FS3_VM_Client_Post_File_DoneM;
    OnResultP: TC40_FS3_VM_Client_Post_File_DoneP;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Done(Successed: Boolean);
    procedure Do_Compute_MD5_Th();
    procedure Do_Result_Create_File_From_MD5(Sender: TPeerIO; Result_: TDFE);
    procedure DoP2PVM_CloneConnectAndPostFile(Sender: TZNet_WithP2PVM_Client);
    procedure Do_Result_Begin_Post(Sender: TPeerIO; Result_: TDFE);
    procedure Do_Post_Th();
    procedure Do_Result_End_Post(Sender: TPeerIO; Result_: TDFE);
  end;

  TC40_FS3_VM_Client_Get_File_DoneC = procedure(Sender: TC40_FS3_VM_Client; Stream: TCore_Stream; MD5: TMD5; Successed: Boolean);
  TC40_FS3_VM_Client_Get_File_DoneM = procedure(Sender: TC40_FS3_VM_Client; Stream: TCore_Stream; MD5: TMD5; Successed: Boolean) of object;
{$IFDEF FPC}
  TC40_FS3_VM_Client_Get_File_DoneP = procedure(Sender: TC40_FS3_VM_Client; Stream: TCore_Stream; MD5: TMD5; Successed: Boolean) is nested;
{$ELSE FPC}
  TC40_FS3_VM_Client_Get_File_DoneP = reference to procedure(Sender: TC40_FS3_VM_Client; Stream: TCore_Stream; MD5: TMD5; Successed: Boolean);
{$ENDIF FPC}

  TC40_FS3_VM_Client_Get_File_Bridge = class(TCore_Object_Intermediate)
  public
    p2pClient: TZNet_WithP2PVM_Client;
    Client: TC40_FS3_VM_Client;
    File_Name: U_String;
    bPos, ePos: Int64;
    Stream: TCore_Stream;
    MD5: TMD5;
    Done_State: PBoolean;
    OnResultC: TC40_FS3_VM_Client_Get_File_DoneC;
    OnResultM: TC40_FS3_VM_Client_Get_File_DoneM;
    OnResultP: TC40_FS3_VM_Client_Get_File_DoneP;
    MD5_Tool: TMD5_Tool;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Done(Successed: Boolean);
    procedure Do_Run;
    procedure DoP2PVM_CloneConnectAndGetFile(Sender: TZNet_WithP2PVM_Client);
    procedure Do_Result_Get_File(Sender: TPeerIO; Result_: TDFE);
    procedure cmd_Fragment_Data(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure cmd_Done(Sender: TPeerIO; InData: TDFE);
  end;

  TC40_FS3_VM_Client_File_Info__ = record
    File_Name: U_String;
    File_Size: Int64;
    File_Time: TDateTime;
    File_Life: Double;
  end;

  TC40_FS3_VM_Client_File_List_Array = array of TC40_FS3_VM_Client_File_Info__;

  TC40_FS3_VM_Client_Get_File_List_DoneC = procedure(Sender: TC40_FS3_VM_Client; arry: TC40_FS3_VM_Client_File_List_Array);
  TC40_FS3_VM_Client_Get_File_List_DoneM = procedure(Sender: TC40_FS3_VM_Client; arry: TC40_FS3_VM_Client_File_List_Array) of object;
{$IFDEF FPC}
  TC40_FS3_VM_Client_Get_File_List_DoneP = procedure(Sender: TC40_FS3_VM_Client; arry: TC40_FS3_VM_Client_File_List_Array) is nested;
{$ELSE FPC}
  TC40_FS3_VM_Client_Get_File_List_DoneP = reference to procedure(Sender: TC40_FS3_VM_Client; arry: TC40_FS3_VM_Client_File_List_Array);
{$ENDIF FPC}

  TC40_FS3_VM_Client_Get_File_List_Bridge = class(TCore_Object_Intermediate)
  public
    Client: TC40_FS3_VM_Client;
    OnResultC: TC40_FS3_VM_Client_Get_File_List_DoneC;
    OnResultM: TC40_FS3_VM_Client_Get_File_List_DoneM;
    OnResultP: TC40_FS3_VM_Client_Get_File_List_DoneP;
    constructor Create;
    destructor Destroy; override;
    procedure Do_Result_Get_File_List(Sender: TPeerIO; Result_: TDFE);
  end;

  TC40_FS3_VM_Client = class(TC40_NoAuth_VM_Client)
  private
    FP2PVM_Recycle_Pool: TC40_FS3_VM_P2PVM_Recycle_Pool;
    FBody_Fragment_Size, FDatabase_Size: Int64;
    procedure Do_Result_Get_Lite_Info(Sender: TPeerIO; Result_: TDFE);
  protected
    procedure Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Client); override;
  public
    property Body_Fragment_Size: Int64 read FBody_Fragment_Size;
    property Database_Size: Int64 read FDatabase_Size;
    constructor Create(Param_: U_String); override;
    destructor Destroy; override;
    procedure Update_Lite_Info;

    procedure Post_File(File_Name: U_String; File_Time: TDateTime; File_Life: Double; File_Stream: TCore_Stream; Auto_Free_Stream: Boolean);
    procedure Post_File_C(File_Name: U_String; File_Time: TDateTime; File_Life: Double; File_Stream: TCore_Stream; Auto_Free_Stream: Boolean; OnResult: TC40_FS3_VM_Client_Post_File_DoneC);
    procedure Post_File_M(File_Name: U_String; File_Time: TDateTime; File_Life: Double; File_Stream: TCore_Stream; Auto_Free_Stream: Boolean; OnResult: TC40_FS3_VM_Client_Post_File_DoneM);
    procedure Post_File_P(File_Name: U_String; File_Time: TDateTime; File_Life: Double; File_Stream: TCore_Stream; Auto_Free_Stream: Boolean; OnResult: TC40_FS3_VM_Client_Post_File_DoneP);

    procedure Get_File(File_Name: U_String; bPos, ePos: Int64; Output: TCore_Stream; Done_State: PBoolean);
    procedure Get_File_C(File_Name: U_String; bPos, ePos: Int64; Output: TCore_Stream; OnResult: TC40_FS3_VM_Client_Get_File_DoneC);
    procedure Get_File_M(File_Name: U_String; bPos, ePos: Int64; Output: TCore_Stream; OnResult: TC40_FS3_VM_Client_Get_File_DoneM);
    procedure Get_File_P(File_Name: U_String; bPos, ePos: Int64; Output: TCore_Stream; OnResult: TC40_FS3_VM_Client_Get_File_DoneP);

    procedure Get_File_List_C(filter_: U_String; Max_Num: Int64; OnResult: TC40_FS3_VM_Client_Get_File_List_DoneC);
    procedure Get_File_List_M(filter_: U_String; Max_Num: Int64; OnResult: TC40_FS3_VM_Client_Get_File_List_DoneM);
    procedure Get_File_List_P(filter_: U_String; Max_Num: Int64; OnResult: TC40_FS3_VM_Client_Get_File_List_DoneP);
  end;

  TC40_FS3_VM_Client_List = TGenericsList<TC40_FS3_VM_Client>;

implementation

constructor TC40_FS3_VM_Service_RecvTunnel_NoAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  FS3_Service := nil;
  Post_Queue_Tool := nil;
end;

destructor TC40_FS3_VM_Service_RecvTunnel_NoAuth.Destroy;
begin
  DisposeObjectAndNil(Post_Queue_Tool);
  inherited Destroy;
end;

constructor TC40_FS3_VM_Service_SendTunnel_NoAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  FS3_Service := nil;
end;

destructor TC40_FS3_VM_Service_SendTunnel_NoAuth.Destroy;
begin
  inherited Destroy;
end;

constructor TC40_FS3_VM_Service_Get_File_Bridge.Create;
begin
  inherited Create;
  FS3_Service := nil;
  Sequence_ID := 0;
  bPos := 0;
  ePos := 0;
  Send_ID := 0;
end;

destructor TC40_FS3_VM_Service_Get_File_Bridge.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_FS3_VM_Service_Get_File_Bridge.Do_Run_Get_Th;
begin
  FS3_Service.FS3_Lite.Sync_Get_Data_M(Sequence_ID, bPos, ePos, Do_Get_Fragment_Data, Do_Get_Done);
  DelayFreeObj(1.0, self);
end;

procedure TC40_FS3_VM_Service_Get_File_Bridge.Do_Get_Fragment_Data(Sender: TZDB2_FS3_Lite; Successed: Boolean; Fragment: TMS64; Data_Pos: Int64);
var
  io_: TPeerIO;
  m64: TMem64;
begin
  if not Successed then
      exit;
  io_ := FS3_Service.DTNoAuth.RecvTunnel[Send_ID];
  if io_ = nil then
      exit;
  m64 := TMem64.Create;
  m64.Size := 8 + Fragment.Size;
  m64.WriteInt64(Data_Pos);
  m64.WritePtr(Fragment.Memory, Fragment.Size);
  io_.SendCompleteBuffer('Fragment_Data', m64, true);
end;

procedure TC40_FS3_VM_Service_Get_File_Bridge.Do_Get_Done(Sender: TZDB2_FS3_Lite; Successed: Boolean);
var
  io_: TPeerIO;
begin
  io_ := FS3_Service.DTNoAuth.RecvTunnel[Send_ID];
  if (io_ = nil) then
      exit;
  io_.SendDirectStreamCmd('Done', TDFE.Create.WriteBool(Successed).DelayFree);
end;

procedure TC40_FS3_VM_Service.cmd_Get_Lite_Info(Sender: TPeerIO; InData, OutData: TDFE);
begin
  OutData.WriteInt64(FS3_Lite.Body_Fragment_Size);
  OutData.WriteInt64(FS3_Lite.Database_Size);
end;

procedure TC40_FS3_VM_Service.cmd_Create_File_From_MD5(Sender: TPeerIO; InData, OutData: TDFE);
var
  FileName_: U_String;
  Time__: TDateTime;
  Life_: Double;
  MD5_: TMD5;
begin
  FileName_ := InData.R.ReadString;
  Time__ := InData.R.ReadDouble;
  Life_ := InData.R.ReadDouble;
  MD5_ := InData.R.ReadMD5;
  OutData.WriteBool(FS3_Lite.Create_FI_From_LT_MD5(FileName_, Time__, Life_, MD5_));
end;

procedure TC40_FS3_VM_Service.cmd_Fast_Post(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  tmp, m64: TMS64;
  FileName_: U_String;
  Time__: TDateTime;
  Life_: Double;
begin
  tmp := TMS64.Create;
  tmp.Mapping(InData, DataSize);
  FileName_ := tmp.ReadString;
  Time__ := tmp.ReadDouble;
  Life_ := tmp.ReadDouble;
  m64 := TMS64.Create;
  m64.WritePtr(tmp.PosAsPtr, tmp.Size - tmp.Position);
  disposeObject(tmp);
  FS3_Lite.Sync_Post_Data(m64, true, FileName_, Time__, Life_);
end;

procedure TC40_FS3_VM_Service.cmd_Begin_Post(Sender: TPeerIO; InData, OutData: TDFE);
var
  io_define_: TC40_FS3_VM_Service_RecvTunnel_NoAuth;
  FileName_: U_String;
  File_Size_: Int64;
  File_MD5_: TMD5;
  Time_: TDateTime;
  Life_: Double;
begin
  io_define_ := TC40_FS3_VM_Service_RecvTunnel_NoAuth(Sender.UserDefine);
  DisposeObjectAndNil(io_define_.Post_Queue_Tool);
  io_define_.Post_Queue_Tool := FS3_Lite.Create_Sync_Post_Queue;
  FileName_ := InData.R.ReadString;
  File_Size_ := InData.R.ReadInt64;
  File_MD5_ := InData.R.ReadMD5;
  Time_ := InData.R.ReadDouble;
  Life_ := InData.R.ReadDouble;
  io_define_.Post_Queue_Tool.Begin_Post(FileName_, File_Size_, File_MD5_, Time_, Life_);
  if io_define_.Post_Queue_Tool.Completed then
    begin
      DisposeObjectAndNil(io_define_.Post_Queue_Tool);
      OutData.WriteBool(true)
    end
  else
    begin
      OutData.WriteBool(False);
    end;
end;

procedure TC40_FS3_VM_Service.cmd_Post(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  io_define_: TC40_FS3_VM_Service_RecvTunnel_NoAuth;
  m64: TMem64;
begin
  io_define_ := TC40_FS3_VM_Service_RecvTunnel_NoAuth(Sender.UserDefine);
  if io_define_.Post_Queue_Tool = nil then
      exit;
  m64 := TMem64.Create;
  m64.SwapInstance(Sender.CompleteBuffer_Current_Trigger);
  io_define_.Post_Queue_Tool.Post(m64, true);
end;

procedure TC40_FS3_VM_Service.cmd_End_Post(Sender: TPeerIO; InData, OutData: TDFE);
var
  io_define_: TC40_FS3_VM_Service_RecvTunnel_NoAuth;
begin
  io_define_ := TC40_FS3_VM_Service_RecvTunnel_NoAuth(Sender.UserDefine);
  if io_define_.Post_Queue_Tool = nil then
      exit;
  io_define_.Post_Queue_Tool.End_Post_And_Free;
  io_define_.Post_Queue_Tool := nil;
end;

procedure TC40_FS3_VM_Service.cmd_Get_File(Sender: TPeerIO; InData, OutData: TDFE);
var
  FileName_: U_String;
  fi: TZDB2_FS3_FileInfo;
  Bridge_: TC40_FS3_VM_Service_Get_File_Bridge;
begin
  FileName_ := InData.R.ReadString;

  fi := FS3_Lite.FileInfo_Pool[FileName_];
  if fi = nil then
    begin
      OutData.WriteBool(False);
      exit;
    end;

  Bridge_ := TC40_FS3_VM_Service_Get_File_Bridge.Create;
  Bridge_.FS3_Service := self;
  Bridge_.Sequence_ID := fi.Sequence_ID;
  Bridge_.bPos := InData.R.ReadInt64;
  Bridge_.ePos := InData.R.ReadInt64;
  Bridge_.Send_ID := InData.R.ReadCardinal;
  OutData.WriteBool(true);

  TCompute.RunM_NP(Bridge_.Do_Run_Get_Th);
end;

procedure TC40_FS3_VM_Service.do_Th_Get_File_List(thSender: THPC_Stream; ThInData, ThOutData: TDFE);
var
  filter_: U_String;
  Max_Num, i: Int64;
  d: TDFE;
begin
  filter_ := ThInData.R.ReadString;
  Max_Num := ThInData.R.ReadInt64;
  if FS3_Lite.FileInfo_Pool.Num > 0 then
    begin
      FS3_Lite.FileInfo_Pool.Critical__.Lock;
      try
        i := 0;
        with FS3_Lite.FileInfo_Pool.Invert_Repeat_ do
          repeat
            if umlMultipleMatch(filter_, Queue^.Data^.Data.Second.File_Name) then
              begin
                d := TDFE.Create;
                d.WriteString(Queue^.Data^.Data.Second.File_Name);
                d.WriteInt64(Queue^.Data^.Data.Second.FileSize);
                d.WriteDouble(Queue^.Data^.Data.Second.Time_);
                d.WriteDouble(Queue^.Data^.Data.Second.Life);
                ThOutData.WriteDataFrame(d);
                disposeObject(d);
                inc(i);
              end;
          until ((Max_Num > 0) and (i >= Max_Num)) or (not Prev);
      finally
          FS3_Lite.FileInfo_Pool.Critical__.UnLock;
      end;
    end;
end;

procedure TC40_FS3_VM_Service.cmd_Get_File_List(Sender: TPeerIO; InData, OutData: TDFE);
begin
  RunHPC_StreamM(Sender, nil, nil, NULL, InData, OutData, do_Th_Get_File_List);
end;

constructor TC40_FS3_VM_Service.Create(Param_: U_String);
begin
  inherited Create(Param_);
  DTNoAuthService.FileSystem := true;
  // max complete buffer 10M
  DTNoAuthService.RecvTunnel.MaxCompleteBufferSize := EStrToInt64(ParamList.GetDefaultValue('MaxBuffer', '10*1024*1024'), 10 * 1024 * 1024);
  DTNoAuthService.RecvTunnel.CompleteBufferCompressed := False;
  DTNoAuthService.RecvTunnel.UserDefineClass := TC40_FS3_VM_Service_RecvTunnel_NoAuth;
  DTNoAuthService.SendTunnel.UserDefineClass := TC40_FS3_VM_Service_SendTunnel_NoAuth;
  // reg command
  DTNoAuthService.RecvTunnel.RegisterStream('Get_Lite_Info').OnExecute := cmd_Get_Lite_Info;
  DTNoAuthService.RecvTunnel.RegisterStream('Create_File_From_MD5').OnExecute := cmd_Create_File_From_MD5;
  DTNoAuthService.RecvTunnel.RegisterCompleteBuffer('Fast_Post').OnExecute := cmd_Fast_Post;
  DTNoAuthService.RecvTunnel.RegisterStream('Begin_Post').OnExecute := cmd_Begin_Post;
  DTNoAuthService.RecvTunnel.RegisterCompleteBuffer('Post').OnExecute := cmd_Post;
  DTNoAuthService.RecvTunnel.RegisterStream('End_Post').OnExecute := cmd_End_Post;
  DTNoAuthService.RecvTunnel.RegisterStream('Get_File').OnExecute := cmd_Get_File;
  DTNoAuthService.RecvTunnel.RegisterStream('Get_File_List').OnExecute := cmd_Get_File_List;

  FS3_Lite := TZDB2_FS3_Lite.Create(DTNoAuthService.PublicFileDirectory);
  FS3_Lite.Body_Fragment_Size := EStrToInt64(ParamList.GetDefaultValue('Body_Fragment_Size', '1024*1024'));
  FS3_Lite.Build_Script_And_Open(ParamList.GetDefaultValue('Lite_Prefix', 'FileSystem_VM'), EStrToBool(ParamList.GetDefaultValue('Lite_Temp_Runtime', 'False')));

  FS3_Life_Check_Time := EStrToUInt64(ParamList.GetDefaultValue('Life_Check_Time', '1000'));

  // internal
  FLast_Life_Check_Time := GetTimeTick;
end;

destructor TC40_FS3_VM_Service.Destroy;
begin
  disposeObject(FS3_Lite);
  inherited Destroy;
end;

procedure TC40_FS3_VM_Service.SafeCheck;
begin
  inherited SafeCheck;
  FS3_Lite.Flush;
end;

procedure TC40_FS3_VM_Service.Progress;
begin
  inherited Progress;
  if GetTimeTick - FLast_Life_Check_Time > FS3_Life_Check_Time then
    begin
      FLast_Life_Check_Time := GetTimeTick;
      FS3_Lite.Check_Life(FS3_Life_Check_Time * 0.001);
    end;
  FS3_Lite.Check_Recycle_Pool;
end;

constructor TC40_FS3_VM_Client_Post_File_Bridge.Create;
begin
  inherited Create;
  p2pClient := nil;
  Client := nil;
  File_Name := '';
  File_Time := 0;
  File_Life := 0;
  File_MD5 := NULL_MD5;
  Auto_Free_Stream := False;
  Stream := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TC40_FS3_VM_Client_Post_File_Bridge.Destroy;
begin
  if Auto_Free_Stream then
      disposeObject(Stream);
  if p2pClient <> nil then
    begin
      Client.FP2PVM_Recycle_Pool.Push(p2pClient);
      p2pClient := nil;
    end;
  inherited Destroy;
end;

procedure TC40_FS3_VM_Client_Post_File_Bridge.Do_Done(Successed: Boolean);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, self, Successed)
    else if Assigned(OnResultM) then
        OnResultM(Client, self, Successed)
    else if Assigned(OnResultP) then
        OnResultP(Client, self, Successed);
  except
  end;
end;

procedure TC40_FS3_VM_Client_Post_File_Bridge.Do_Compute_MD5_Th;
var
  d: TDFE;
begin
  // compute file md5
  File_MD5 := umlStreamMD5(Stream);

  // remote md5
  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteDouble(File_Time);
  d.WriteDouble(File_Life);
  d.WriteMD5(File_MD5);
  Client.DTNoAuth.SendTunnel.SendStreamCmdM('Create_File_From_MD5', d, Do_Result_Create_File_From_MD5);
  disposeObject(d);
end;

procedure TC40_FS3_VM_Client_Post_File_Bridge.Do_Result_Create_File_From_MD5(Sender: TPeerIO; Result_: TDFE);
begin
  if Result_.R.ReadBool then
    begin
      // Create_File_From_MD5 successed.
      Do_Done(true);
      DelayFreeObj(1.0, self);
    end
  else if Client.FP2PVM_Recycle_Pool.Num > 0 then
    begin
      DoP2PVM_CloneConnectAndPostFile(Client.FP2PVM_Recycle_Pool.First^.Data);
      Client.FP2PVM_Recycle_Pool.Next;
    end
  else if Client.Client.SendTunnel.CloneConnectM(DoP2PVM_CloneConnectAndPostFile) = nil then
    begin
      Do_Done(False);
      DelayFreeObj(1.0, self);
    end;
end;

procedure TC40_FS3_VM_Client_Post_File_Bridge.DoP2PVM_CloneConnectAndPostFile(Sender: TZNet_WithP2PVM_Client);
var
  d: TDFE;
  m64: TMem64;
begin
  if Sender = nil then
    begin
      Do_Done(False);
      DelayFreeObj(1.0, self);
      exit;
    end;
  p2pClient := Sender;
  if Stream.Size > Client.Body_Fragment_Size then
    begin
      d := TDFE.Create;
      d.WriteString(File_Name);
      d.WriteInt64(Stream.Size);
      d.WriteMD5(File_MD5);
      d.WriteDouble(File_Time);
      d.WriteDouble(File_Life);
      p2pClient.SendStreamCmdM('Begin_Post', d, Do_Result_Begin_Post);
      disposeObject(d);
    end
  else
    begin
      m64 := TMem64.Create;
      m64.Delta := Stream.Size;
      m64.WriteString(File_Name);
      m64.WriteDouble(File_Time);
      m64.WriteDouble(File_Life);
      Stream.Position := 0;
      m64.CopyFrom(Stream, Stream.Size);
      p2pClient.SendCompleteBuffer('Fast_Post', m64, true);
      p2pClient.SendStreamCmdM('End_Post', TDFE.Create.DelayFree, Do_Result_End_Post);
    end;
end;

procedure TC40_FS3_VM_Client_Post_File_Bridge.Do_Result_Begin_Post(Sender: TPeerIO; Result_: TDFE);
begin
  if Result_.R.ReadBool then
    begin
      // Begin_Post found md5
      Do_Done(true);
      DelayFreeObj(1.0, self);
    end
  else
    begin
      TCompute.RunM_NP(Do_Post_Th);
    end;
end;

procedure TC40_FS3_VM_Client_Post_File_Bridge.Do_Post_Th;
var
  m64: TMem64;
begin
  Stream.Position := 0;
  while Stream.Position + Client.Body_Fragment_Size < Stream.Size do
    begin
      m64 := TMem64.Create;
      m64.CopyFrom(Stream, Client.Body_Fragment_Size);
      p2pClient.SendCompleteBuffer('Post', m64, true);
      p2pClient.Send_NULL;
    end;
  if Stream.Position < Stream.Size then
    begin
      m64 := TMem64.Create;
      m64.CopyFrom(Stream, Stream.Size - Stream.Position);
      p2pClient.SendCompleteBuffer('Post', m64, true);
    end;
  p2pClient.SendStreamCmdM('End_Post', TDFE.Create.DelayFree, Do_Result_End_Post);
end;

procedure TC40_FS3_VM_Client_Post_File_Bridge.Do_Result_End_Post(Sender: TPeerIO; Result_: TDFE);
begin
  Do_Done(true);
  DelayFreeObj(1.0, self);
end;

constructor TC40_FS3_VM_Client_Get_File_Bridge.Create;
begin
  inherited Create;
  p2pClient := nil;
  Client := nil;
  File_Name := '';
  bPos := 0;
  ePos := 0;
  Stream := nil;
  MD5 := NULL_MD5;
  Done_State := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
  MD5_Tool := TMD5_Tool.Create;
end;

destructor TC40_FS3_VM_Client_Get_File_Bridge.Destroy;
begin
  if p2pClient <> nil then
    begin
      p2pClient.UnRegisted('Fragment_Data');
      p2pClient.UnRegisted('Done');
      Client.FP2PVM_Recycle_Pool.Push(p2pClient);
      p2pClient := nil;
    end;
  disposeObject(MD5_Tool);
  inherited Destroy;
end;

procedure TC40_FS3_VM_Client_Get_File_Bridge.Do_Done(Successed: Boolean);
begin
  try
    if Done_State <> nil then
        Done_State^ := true;
    if Assigned(OnResultC) then
        OnResultC(Client, Stream, MD5, Successed)
    else if Assigned(OnResultM) then
        OnResultM(Client, Stream, MD5, Successed)
    else if Assigned(OnResultP) then
        OnResultP(Client, Stream, MD5, Successed);
  except
  end;
end;

procedure TC40_FS3_VM_Client_Get_File_Bridge.Do_Run;
begin
  if Done_State <> nil then
      Done_State^ := False;
  if Client.FP2PVM_Recycle_Pool.Num > 0 then
    begin
      DoP2PVM_CloneConnectAndGetFile(Client.FP2PVM_Recycle_Pool.First^.Data);
      Client.FP2PVM_Recycle_Pool.Next;
    end
  else if Client.Client.SendTunnel.CloneConnectM(DoP2PVM_CloneConnectAndGetFile) = nil then
    begin
      Do_Done(False);
      DelayFreeObj(1.0, self);
    end;
end;

procedure TC40_FS3_VM_Client_Get_File_Bridge.DoP2PVM_CloneConnectAndGetFile(Sender: TZNet_WithP2PVM_Client);
var
  d: TDFE;
begin
  p2pClient := Sender;

  p2pClient.RegisterCompleteBuffer('Fragment_Data').OnExecute := cmd_Fragment_Data;
  p2pClient.RegisterDirectStream('Done').OnExecute := cmd_Done;

  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteInt64(bPos);
  d.WriteInt64(ePos);
  d.WriteCardinal(p2pClient.Client_ID);
  Client.DTNoAuthClient.SendTunnel.SendStreamCmdM('Get_File', d, Do_Result_Get_File);
end;

procedure TC40_FS3_VM_Client_Get_File_Bridge.Do_Result_Get_File(Sender: TPeerIO; Result_: TDFE);
begin
  if not Result_.R.ReadBool then
    begin
      Do_Done(False);
      DelayFreeObj(1.0, self);
    end;
end;

procedure TC40_FS3_VM_Client_Get_File_Bridge.cmd_Fragment_Data(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  m64: TMem64;
  Data_Pos: Int64;
begin
  m64 := TMem64.Create;
  m64.Mapping(InData, DataSize);
  Data_Pos := m64.ReadInt64;
  if Data_Pos > Stream.Size then
      Stream.Size := Data_Pos + (DataSize - 8);
  Stream.Position := Data_Pos;
  Stream.Write(m64.PosAsPtr^, m64.Size - m64.Position);
  MD5_Tool.Update(m64.PosAsPtr, m64.Size - m64.Position);
  disposeObject(m64);
end;

procedure TC40_FS3_VM_Client_Get_File_Bridge.cmd_Done(Sender: TPeerIO; InData: TDFE);
begin
  MD5 := MD5_Tool.FinalizeMD5;
  Do_Done(InData.R.ReadBool);
  DelayFreeObj(1.0, self);
end;

constructor TC40_FS3_VM_Client_Get_File_List_Bridge.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TC40_FS3_VM_Client_Get_File_List_Bridge.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_FS3_VM_Client_Get_File_List_Bridge.Do_Result_Get_File_List(Sender: TPeerIO; Result_: TDFE);
var
  arry: TC40_FS3_VM_Client_File_List_Array;
  i: Integer;
  d: TDFE;
begin
  SetLength(arry, Result_.Count);
  for i := 0 to Result_.Count - 1 do
    begin
      d := TDFE.Create;
      Result_.ReadDataFrame(i, d);
      arry[i].File_Name := d.R.ReadString;
      arry[i].File_Size := d.R.ReadInt64;
      arry[i].File_Time := d.R.ReadDouble;
      arry[i].File_Life := d.R.ReadDouble;
      disposeObject(d);
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry)
    else if Assigned(OnResultM) then
        OnResultM(Client, arry)
    else if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  DelayFreeObj(1.0, self);
end;

procedure TC40_FS3_VM_Client.Do_Result_Get_Lite_Info(Sender: TPeerIO; Result_: TDFE);
begin
  FBody_Fragment_Size := Result_.R.ReadInt64;
  FDatabase_Size := Result_.R.ReadInt64;
end;

procedure TC40_FS3_VM_Client.Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Client);
begin
  Update_Lite_Info();
end;

constructor TC40_FS3_VM_Client.Create(Param_: U_String);
begin
  inherited Create(Param_);
  FP2PVM_Recycle_Pool := TC40_FS3_VM_P2PVM_Recycle_Pool.Create;
  FBody_Fragment_Size := 1024 * 1024;
  FDatabase_Size := 0;
end;

destructor TC40_FS3_VM_Client.Destroy;
begin
  disposeObject(FP2PVM_Recycle_Pool);
  inherited Destroy;
end;

procedure TC40_FS3_VM_Client.Update_Lite_Info;
begin
  DTNoAuth.SendTunnel.SendStreamCmdM('Get_Lite_Info', nil, Do_Result_Get_Lite_Info);
end;

procedure TC40_FS3_VM_Client.Post_File(File_Name: U_String; File_Time: TDateTime; File_Life: Double; File_Stream: TCore_Stream; Auto_Free_Stream: Boolean);
var
  Bridge_: TC40_FS3_VM_Client_Post_File_Bridge;
begin
  Bridge_ := TC40_FS3_VM_Client_Post_File_Bridge.Create;
  Bridge_.Client := self;
  Bridge_.File_Name := File_Name;
  Bridge_.File_Time := File_Time;
  Bridge_.File_Life := File_Life;
  Bridge_.Stream := File_Stream;
  Bridge_.Auto_Free_Stream := Auto_Free_Stream;
  TCompute.RunM_NP(Bridge_.Do_Compute_MD5_Th);
end;

procedure TC40_FS3_VM_Client.Post_File_C(File_Name: U_String; File_Time: TDateTime; File_Life: Double; File_Stream: TCore_Stream; Auto_Free_Stream: Boolean; OnResult: TC40_FS3_VM_Client_Post_File_DoneC);
var
  Bridge_: TC40_FS3_VM_Client_Post_File_Bridge;
begin
  Bridge_ := TC40_FS3_VM_Client_Post_File_Bridge.Create;
  Bridge_.Client := self;
  Bridge_.File_Name := File_Name;
  Bridge_.File_Time := File_Time;
  Bridge_.File_Life := File_Life;
  Bridge_.Stream := File_Stream;
  Bridge_.Auto_Free_Stream := Auto_Free_Stream;
  Bridge_.OnResultC := OnResult;
  TCompute.RunM_NP(Bridge_.Do_Compute_MD5_Th);
end;

procedure TC40_FS3_VM_Client.Post_File_M(File_Name: U_String; File_Time: TDateTime; File_Life: Double; File_Stream: TCore_Stream; Auto_Free_Stream: Boolean; OnResult: TC40_FS3_VM_Client_Post_File_DoneM);
var
  Bridge_: TC40_FS3_VM_Client_Post_File_Bridge;
begin
  Bridge_ := TC40_FS3_VM_Client_Post_File_Bridge.Create;
  Bridge_.Client := self;
  Bridge_.File_Name := File_Name;
  Bridge_.File_Time := File_Time;
  Bridge_.File_Life := File_Life;
  Bridge_.Stream := File_Stream;
  Bridge_.Auto_Free_Stream := Auto_Free_Stream;
  Bridge_.OnResultM := OnResult;
  TCompute.RunM_NP(Bridge_.Do_Compute_MD5_Th);
end;

procedure TC40_FS3_VM_Client.Post_File_P(File_Name: U_String; File_Time: TDateTime; File_Life: Double; File_Stream: TCore_Stream; Auto_Free_Stream: Boolean; OnResult: TC40_FS3_VM_Client_Post_File_DoneP);
var
  Bridge_: TC40_FS3_VM_Client_Post_File_Bridge;
begin
  Bridge_ := TC40_FS3_VM_Client_Post_File_Bridge.Create;
  Bridge_.Client := self;
  Bridge_.File_Name := File_Name;
  Bridge_.File_Time := File_Time;
  Bridge_.File_Life := File_Life;
  Bridge_.Stream := File_Stream;
  Bridge_.Auto_Free_Stream := Auto_Free_Stream;
  Bridge_.OnResultP := OnResult;
  TCompute.RunM_NP(Bridge_.Do_Compute_MD5_Th);
end;

procedure TC40_FS3_VM_Client.Get_File(File_Name: U_String; bPos, ePos: Int64; Output: TCore_Stream; Done_State: PBoolean);
var
  Bridge_: TC40_FS3_VM_Client_Get_File_Bridge;
begin
  Bridge_ := TC40_FS3_VM_Client_Get_File_Bridge.Create;
  Bridge_.Client := self;
  Bridge_.File_Name := File_Name;
  Bridge_.bPos := bPos;
  Bridge_.ePos := ePos;
  Bridge_.Stream := Output;
  Bridge_.Done_State := Done_State;
  Bridge_.Do_Run;
end;

procedure TC40_FS3_VM_Client.Get_File_C(File_Name: U_String; bPos, ePos: Int64; Output: TCore_Stream; OnResult: TC40_FS3_VM_Client_Get_File_DoneC);
var
  Bridge_: TC40_FS3_VM_Client_Get_File_Bridge;
begin
  Bridge_ := TC40_FS3_VM_Client_Get_File_Bridge.Create;
  Bridge_.Client := self;
  Bridge_.File_Name := File_Name;
  Bridge_.bPos := bPos;
  Bridge_.ePos := ePos;
  Bridge_.Stream := Output;
  Bridge_.OnResultC := OnResult;
  Bridge_.Do_Run;
end;

procedure TC40_FS3_VM_Client.Get_File_M(File_Name: U_String; bPos, ePos: Int64; Output: TCore_Stream; OnResult: TC40_FS3_VM_Client_Get_File_DoneM);
var
  Bridge_: TC40_FS3_VM_Client_Get_File_Bridge;
begin
  Bridge_ := TC40_FS3_VM_Client_Get_File_Bridge.Create;
  Bridge_.Client := self;
  Bridge_.File_Name := File_Name;
  Bridge_.bPos := bPos;
  Bridge_.ePos := ePos;
  Bridge_.Stream := Output;
  Bridge_.OnResultM := OnResult;
  Bridge_.Do_Run;
end;

procedure TC40_FS3_VM_Client.Get_File_P(File_Name: U_String; bPos, ePos: Int64; Output: TCore_Stream; OnResult: TC40_FS3_VM_Client_Get_File_DoneP);
var
  Bridge_: TC40_FS3_VM_Client_Get_File_Bridge;
begin
  Bridge_ := TC40_FS3_VM_Client_Get_File_Bridge.Create;
  Bridge_.Client := self;
  Bridge_.File_Name := File_Name;
  Bridge_.bPos := bPos;
  Bridge_.ePos := ePos;
  Bridge_.Stream := Output;
  Bridge_.OnResultP := OnResult;
  Bridge_.Do_Run;
end;

procedure TC40_FS3_VM_Client.Get_File_List_C(filter_: U_String; Max_Num: Int64; OnResult: TC40_FS3_VM_Client_Get_File_List_DoneC);
var
  d: TDFE;
  Bridge_: TC40_FS3_VM_Client_Get_File_List_Bridge;
begin
  d := TDFE.Create;
  d.WriteString(filter_);
  d.WriteInt64(Max_Num);
  Bridge_ := TC40_FS3_VM_Client_Get_File_List_Bridge.Create;
  Bridge_.Client := self;
  Bridge_.OnResultC := OnResult;
  DTNoAuth.SendTunnel.SendStreamCmdM('Get_File_List', d, Bridge_.Do_Result_Get_File_List);
  disposeObject(d);
end;

procedure TC40_FS3_VM_Client.Get_File_List_M(filter_: U_String; Max_Num: Int64; OnResult: TC40_FS3_VM_Client_Get_File_List_DoneM);
var
  d: TDFE;
  Bridge_: TC40_FS3_VM_Client_Get_File_List_Bridge;
begin
  d := TDFE.Create;
  d.WriteString(filter_);
  d.WriteInt64(Max_Num);
  Bridge_ := TC40_FS3_VM_Client_Get_File_List_Bridge.Create;
  Bridge_.Client := self;
  Bridge_.OnResultM := OnResult;
  DTNoAuth.SendTunnel.SendStreamCmdM('Get_File_List', d, Bridge_.Do_Result_Get_File_List);
  disposeObject(d);
end;

procedure TC40_FS3_VM_Client.Get_File_List_P(filter_: U_String; Max_Num: Int64; OnResult: TC40_FS3_VM_Client_Get_File_List_DoneP);
var
  d: TDFE;
  Bridge_: TC40_FS3_VM_Client_Get_File_List_Bridge;
begin
  d := TDFE.Create;
  d.WriteString(filter_);
  d.WriteInt64(Max_Num);
  Bridge_ := TC40_FS3_VM_Client_Get_File_List_Bridge.Create;
  Bridge_.Client := self;
  Bridge_.OnResultP := OnResult;
  DTNoAuth.SendTunnel.SendStreamCmdM('Get_File_List', d, Bridge_.Do_Result_Get_File_List);
  disposeObject(d);
end;

end.

 
