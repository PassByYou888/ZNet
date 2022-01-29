{ ****************************************************************************** }
{ * cloud 4.0 File System 2.0                                                  * }
{ ****************************************************************************** }
unit Z.Net.C4_FS2;

{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ELSE FPC}
  System.IOUtils,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine,
  Z.Geometry2D, Z.DFE, Z.Json, Z.Expression,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.ZDB2, Z.ZDB2.MS64, Z.GHashList,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth, Z.Net.C4;

type
  TC40_FS2_Service = class(TC40_Base_NoAuth_Service)
  public type
    TFS2_Service_File_Data = class
    public
      Owner: TC40_FS2_Service;
      Stream: TZDB2_MS64;
      FileName: U_String;
      FileTime: TDateTime;
      FileRef: Integer;
      FileMD5: TMD5;
      FileSize: Int64;
      constructor Create(Owner_: TC40_FS2_Service; Stream_: TZDB2_MS64);
      destructor Destroy; override;
    end;

    TFS2_Service_File_Data_Pool = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericHashList<TFS2_Service_File_Data>;

  protected
    // command
    procedure cmd_FS2_PostFile(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure cmd_FS2_GetFile(Sender: TPeerIO; InData: TDFE);
    procedure cmd_FS2_GetFileMD5(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_FS2_RemoveFile(Sender: TPeerIO; InData: TDFE);
    procedure cmd_FS2_UpdateFileRef(Sender: TPeerIO; InData: TDFE);
    procedure cmd_FS2_IncFileRef(Sender: TPeerIO; InData: TDFE);
    // admin
    procedure cmd_FS2_Size(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_FS2_Search(Sender: TPeerIO; InData, OutData: TDFE);
  private
    FIOHnd: TIOHnd;
  public
    // ZDB2 Core
    ZDB2RecycleMemoryTimeOut: TTimeTick;
    ZDB2DeltaSpace: Int64;
    ZDB2BlockSize: Word;
    ZDB2EnabledCipher: Boolean;
    ZDB2CipherName: U_String;
    ZDB2Password: U_String;
    ZDB2Cipher: TZDB2_Cipher;
    C40_FS2_FileName: U_String;
    FileHashPool: TFS2_Service_File_Data_Pool;
    FileDatabase: TZDB2_List_MS64;

    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
  end;

  TC40_FS2_Client = class(TC40_Base_NoAuth_Client)
  public type
{$REGION 'bridge_define'}
    TFS2_Client_CacheData = class
      Owner: TC40_FS2_Client;
      Stream: TZDB2_MS64;
      LastAccess: TTimeTick;
      constructor Create(Owner_: TC40_FS2_Client);
      destructor Destroy; override;
    end;

    TFS2_Client_CacheHashPool = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericHashList<TFS2_Client_CacheData>;

    TON_FS2_PostFile_DoneC = procedure(Sender: TC40_FS2_Client; info_: U_String);
    TON_FS2_PostFile_DoneM = procedure(Sender: TC40_FS2_Client; info_: U_String) of object;
{$IFDEF FPC}
    TON_FS2_PostFile_DoneP = procedure(Sender: TC40_FS2_Client; info_: U_String) is nested;
{$ELSE FPC}
    TON_FS2_PostFile_DoneP = reference to procedure(Sender: TC40_FS2_Client; info_: U_String);
{$ENDIF FPC}

    TFS2_Temp_Post_File_Tunnel = class
    public
      p2pClient: TZNet_WithP2PVM_Client;
      Client: TC40_FS2_Client;
      File_Name: U_String;
      Stream: TMS64;
      OnResultC: TON_FS2_PostFile_DoneC;
      OnResultM: TON_FS2_PostFile_DoneM;
      OnResultP: TON_FS2_PostFile_DoneP;

      constructor Create;
      destructor Destroy; override;
      procedure DoP2PVM_CloneConnectAndPostFile(Sender: TZNet_WithP2PVM_Client);
      procedure cmd_PostDone(Sender: TPeerIO; InData: SystemString);
    end;

    TON_FS2_GetFile_DoneC = procedure(Sender: TC40_FS2_Client; Stream: TMS64; info_: U_String; Successed: Boolean);
    TON_FS2_GetFile_DoneM = procedure(Sender: TC40_FS2_Client; Stream: TMS64; info_: U_String; Successed: Boolean) of object;
{$IFDEF FPC}
    TON_FS2_GetFile_DoneP = procedure(Sender: TC40_FS2_Client; Stream: TMS64; info_: U_String; Successed: Boolean) is nested;
{$ELSE FPC}
    TON_FS2_GetFile_DoneP = reference to procedure(Sender: TC40_FS2_Client; Stream: TMS64; info_: U_String; Successed: Boolean);
{$ENDIF FPC}

    TFS2_Temp_Get_File_Tunnel = class
    public
      p2pClient: TZNet_WithP2PVM_Client;
      Client: TC40_FS2_Client;
      File_Name: U_String;
      OnResultC: TON_FS2_GetFile_DoneC;
      OnResultM: TON_FS2_GetFile_DoneM;
      OnResultP: TON_FS2_GetFile_DoneP;

      constructor Create;
      destructor Destroy; override;
      procedure cmd_Save(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
      procedure cmd_Error(Sender: TPeerIO; InData: SystemString);
      procedure DoP2PVM_CloneConnectAndGetFile(Sender: TZNet_WithP2PVM_Client);
    end;

    TFS2_Temp_GetFileMD5C = procedure(Sender: TC40_FS2_Client; State_: Boolean; info_: SystemString; MD5_: TMD5);
    TFS2_Temp_GetFileMD5M = procedure(Sender: TC40_FS2_Client; State_: Boolean; info_: SystemString; MD5_: TMD5) of object;
{$IFDEF FPC}
    TFS2_Temp_GetFileMD5P = procedure(Sender: TC40_FS2_Client; State_: Boolean; info_: SystemString; MD5_: TMD5) is nested;
{$ELSE FPC}
    TFS2_Temp_GetFileMD5P = reference to procedure(Sender: TC40_FS2_Client; State_: Boolean; info_: SystemString; MD5_: TMD5);
{$ENDIF FPC}

    TFS2_Temp_GetFileMD5 = class(TOnResultBridge)
    public
      Client: TC40_FS2_Client;
      OnResultC: TFS2_Temp_GetFileMD5C;
      OnResultM: TFS2_Temp_GetFileMD5M;
      OnResultP: TFS2_Temp_GetFileMD5P;
      constructor Create; override;
      procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
      procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
    end;

    TFS2_Temp_GetFileMD5_Cache = class
    public
      Client: TC40_FS2_Client;
      File_Name: U_String;
      OnResultC: TON_FS2_GetFile_DoneC;
      OnResultM: TON_FS2_GetFile_DoneM;
      OnResultP: TON_FS2_GetFile_DoneP;

      constructor Create;
      destructor Destroy; override;
      procedure Do_FS2_GetFileMD5(Sender: TC40_FS2_Client; State_: Boolean; info_: SystemString; MD5_: TMD5);
    end;

    TFS2_FileSizeInfo = record
      FileName: SystemString;
      Size: Int64;
    end;

    TFS2_FileSizeInfo_Array = array of TFS2_FileSizeInfo;

    TFS2_Temp_SizeC = procedure(Sender: TC40_FS2_Client; arry: TFS2_FileSizeInfo_Array);
    TFS2_Temp_SizeM = procedure(Sender: TC40_FS2_Client; arry: TFS2_FileSizeInfo_Array) of object;
{$IFDEF FPC}
    TFS2_Temp_SizeP = procedure(Sender: TC40_FS2_Client; arry: TFS2_FileSizeInfo_Array) is nested;
{$ELSE FPC}
    TFS2_Temp_SizeP = reference to procedure(Sender: TC40_FS2_Client; arry: TFS2_FileSizeInfo_Array);
{$ENDIF FPC}

    TFS2_Temp_Size = class(TOnResultBridge)
    public
      Client: TC40_FS2_Client;
      OnResultC: TFS2_Temp_SizeC;
      OnResultM: TFS2_Temp_SizeM;
      OnResultP: TFS2_Temp_SizeP;
      constructor Create; override;
      procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
      procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
    end;

    TFS2_FileInfo = record
      FileName: SystemString;
      FileTime: TDateTime;
      FileRef: Integer;
      Size: Int64;
      MD5: TMD5;
    end;

    TFS2_FileInfo_Array = array of TFS2_FileInfo;

    TFS2_Temp_SearchC = procedure(Sender: TC40_FS2_Client; arry: TFS2_FileInfo_Array);
    TFS2_Temp_SearchM = procedure(Sender: TC40_FS2_Client; arry: TFS2_FileInfo_Array) of object;
{$IFDEF FPC}
    TFS2_Temp_SearchP = procedure(Sender: TC40_FS2_Client; arry: TFS2_FileInfo_Array) is nested;
{$ELSE FPC}
    TFS2_Temp_SearchP = reference to procedure(Sender: TC40_FS2_Client; arry: TFS2_FileInfo_Array);
{$ENDIF FPC}

    TFS2_Temp_Search = class(TOnResultBridge)
    public
      Client: TC40_FS2_Client;
      OnResultC: TFS2_Temp_SearchC;
      OnResultM: TFS2_Temp_SearchM;
      OnResultP: TFS2_Temp_SearchP;
      constructor Create; override;
      procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE); override;
      procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE); override;
    end;

{$ENDREGION 'bridge_define'}
  protected
    FMaxFileSize: Cardinal;
    FRemoveCacheList: TPascalStringList;
    procedure Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client); override;
    procedure Do_Progress_FileCachePool(const Name_: PSystemString; Obj_: TFS2_Client_CacheData);
  public
    C40_FS2_Cache_FileName: U_String;
    ZDB2RecycleMemoryTimeOut: TTimeTick;
    ZDB2DeltaSpace: Int64;
    ZDB2BlockSize: Word;
    ZDB2EnabledCipher: Boolean;
    ZDB2CipherName: U_String;
    ZDB2Password: U_String;
    ZDB2Cipher: TZDB2_Cipher;
    Cache_File_Life: Int64;
    FileCacheHashPool: TFS2_Client_CacheHashPool;
    Cache: TZDB2_List_MS64;
    property MaxFileSize: Cardinal read FMaxFileSize;

    constructor Create(source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure SafeCheck; override;
    procedure Progress; override;
    // upload
    procedure FS2_PostFile(File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean);
    procedure FS2_PostFile_C(File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean; OnResult: TON_FS2_PostFile_DoneC);
    procedure FS2_PostFile_M(File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean; OnResult: TON_FS2_PostFile_DoneM);
    procedure FS2_PostFile_P(File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean; OnResult: TON_FS2_PostFile_DoneP);
    // download
    procedure FS2_GetFile_C(UsedCache_: Boolean; File_Name: U_String; OnResult: TON_FS2_GetFile_DoneC);
    procedure FS2_GetFile_M(UsedCache_: Boolean; File_Name: U_String; OnResult: TON_FS2_GetFile_DoneM);
    procedure FS2_GetFile_P(UsedCache_: Boolean; File_Name: U_String; OnResult: TON_FS2_GetFile_DoneP);
    // md5
    procedure FS2_GetFileMD5C(File_Name: U_String; OnResult: TFS2_Temp_GetFileMD5C);
    procedure FS2_GetFileMD5M(File_Name: U_String; OnResult: TFS2_Temp_GetFileMD5M);
    procedure FS2_GetFileMD5P(File_Name: U_String; OnResult: TFS2_Temp_GetFileMD5P);
    // remove
    procedure FS2_RemoveFile(File_Name: U_String);
    // ref
    procedure FS2_UpdateFileRef(File_Name: U_String; ref_: Integer);
    procedure FS2_IncFileRef(File_Name: U_String; inc_ref_: Integer);
    // admin
    procedure FS2_SizeC(FileNames: U_StringArray; OnResult: TFS2_Temp_SizeC);
    procedure FS2_SizeM(FileNames: U_StringArray; OnResult: TFS2_Temp_SizeM);
    procedure FS2_SizeP(FileNames: U_StringArray; OnResult: TFS2_Temp_SizeP);
    procedure FS2_SearchC(filter: U_String; MaxNum: Integer; OnResult: TFS2_Temp_SearchC);
    procedure FS2_SearchM(filter: U_String; MaxNum: Integer; OnResult: TFS2_Temp_SearchM);
    procedure FS2_SearchP(filter: U_String; MaxNum: Integer; OnResult: TFS2_Temp_SearchP);
  end;

implementation

constructor TC40_FS2_Service.TFS2_Service_File_Data.Create(Owner_: TC40_FS2_Service; Stream_: TZDB2_MS64);
begin
  inherited Create;
  Owner := Owner_;
  Stream := Stream_;
  FileName := '';
  FileTime := umlNow;
  FileRef := 0;
  FileMD5 := NullMD5;
  FileSize := 0;
end;

destructor TC40_FS2_Service.TFS2_Service_File_Data.Destroy;
begin
  FileName := '';
  if (Owner.FileDatabase <> nil) and (Stream <> nil) then
      Owner.FileDatabase.Remove(Stream, True);
  inherited Destroy;
end;

procedure TC40_FS2_Service.cmd_FS2_PostFile(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  fd: TFS2_Service_File_Data;
begin
  fd := TFS2_Service_File_Data.Create(self, FileDatabase.NewData);
  fd.Stream.Data.WritePtr(InData, DataSize);
  fd.Stream.Data.Position := 0;
  fd.FileName := fd.Stream.Data.ReadString;
  fd.FileTime := fd.Stream.Data.ReadDouble;
  fd.FileRef := fd.Stream.Data.ReadInt32;
  fd.FileMD5 := umlMD5(fd.Stream.Data.PosAsPtr, fd.Stream.Data.Size - fd.Stream.Data.Position);
  fd.FileSize := fd.Stream.Data.Size - fd.Stream.Data.Position;
  fd.Stream.Save;
  FileHashPool.Add(fd.FileName, fd);
  Sender.SendDirectConsoleCmd('PostDone', fd.FileName);
end;

procedure TC40_FS2_Service.cmd_FS2_GetFile(Sender: TPeerIO; InData: TDFE);
var
  File_Name: U_String;
  IO_ID: Cardinal;
  IO_: TPeerIO;
  fd: TFS2_Service_File_Data;
begin
  File_Name := InData.R.ReadString;
  IO_ID := InData.R.ReadCardinal;
  IO_ := DTNoAuthService.RecvTunnel[IO_ID];
  if IO_ = nil then
      exit;
  fd := FileHashPool[File_Name];
  if fd = nil then
      IO_.SendDirectConsoleCmd('Error', PFormat('no found "%s"', [File_Name.Text]))
  else
    begin
      IO_.SendCompleteBuffer('Save', fd.Stream.Data.Clone, True);
      fd.Stream.Save;
    end;
end;

procedure TC40_FS2_Service.cmd_FS2_GetFileMD5(Sender: TPeerIO; InData, OutData: TDFE);
var
  File_Name: U_String;
  fd: TFS2_Service_File_Data;
begin
  File_Name := InData.R.ReadString;
  fd := FileHashPool[File_Name];
  if fd = nil then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('no found "%s"', [File_Name.Text]);
      OutData.WriteMD5(NullMD5);
    end
  else
    begin
      OutData.WriteBool(True);
      OutData.WriteString(fd.FileName);
      OutData.WriteMD5(fd.FileMD5);
    end;
end;

procedure TC40_FS2_Service.cmd_FS2_RemoveFile(Sender: TPeerIO; InData: TDFE);
begin
  FileHashPool.Delete(InData.R.ReadString);
end;

procedure TC40_FS2_Service.cmd_FS2_UpdateFileRef(Sender: TPeerIO; InData: TDFE);
var
  fn: U_String;
  ref_: Integer;
  fd: TFS2_Service_File_Data;
  buff: array of byte;
  tmp1: Word;
  tmp2: NativeUInt;
begin
  fn := InData.R.ReadString;
  ref_ := InData.R.ReadInteger;
  fd := FileHashPool[fn];
  if fd = nil then
      exit;

  fd.FileRef := ref_;

  if fd.Stream.Data_Direct <> nil then
    begin
      // modify cache
      PInteger(GetPtr(fd.Stream.Data_Direct.Memory, PCardinal(fd.Stream.Data_Direct.Memory)^ + 4 + 8))^ := fd.FileRef;
    end
  else
    begin
      SetLength(buff, $FFFF);
      tmp1 := fd.Stream.CoreSpace.Block_IO_Read(@buff[0], fd.Stream.ID);
      if tmp1 > 0 then
        begin
          tmp2 := PCardinal(@buff[0])^ + 4 + 8;
          if tmp2 + 4 <= tmp1 then
            begin
              // io optimized overwrite
              PInteger(GetPtr(@buff[0], tmp2))^ := fd.FileRef;
              fd.Stream.CoreSpace.Block_IO_Write(@buff[0], fd.Stream.ID);
            end
          else
            begin
              // overwrite
              PInteger(GetPtr(fd.Stream.Data.Memory, PCardinal(fd.Stream.Data.Memory)^ + 4 + 8))^ := fd.FileRef;
            end;
        end;
      SetLength(buff, 0);
    end;
end;

procedure TC40_FS2_Service.cmd_FS2_IncFileRef(Sender: TPeerIO; InData: TDFE);
var
  fn: U_String;
  inc_ref_: Integer;
  fd: TFS2_Service_File_Data;
  buff: array of byte;
  tmp1: Word;
  tmp2: NativeUInt;
begin
  fn := InData.R.ReadString;
  inc_ref_ := InData.R.ReadInteger;
  fd := FileHashPool[fn];
  if fd = nil then
      exit;

  fd.FileRef := fd.FileRef + inc_ref_;

  if fd.Stream.Data_Direct <> nil then
    begin
      // modify cache
      PInteger(GetPtr(fd.Stream.Data_Direct.Memory, PCardinal(fd.Stream.Data_Direct.Memory)^ + 4 + 8))^ := fd.FileRef;
    end
  else
    begin
      SetLength(buff, $FFFF);
      tmp1 := fd.Stream.CoreSpace.Block_IO_Read(@buff[0], fd.Stream.ID);
      if tmp1 > 0 then
        begin
          tmp2 := PCardinal(@buff[0])^ + 4 + 8;
          if tmp2 + 4 <= tmp1 then
            begin
              // io optimized overwrite
              PInteger(GetPtr(@buff[0], tmp2))^ := fd.FileRef;
              fd.Stream.CoreSpace.Block_IO_Write(@buff[0], fd.Stream.ID);
            end
          else
            begin
              // overwrite
              PInteger(GetPtr(fd.Stream.Data.Memory, PCardinal(fd.Stream.Data.Memory)^ + 4 + 8))^ := fd.FileRef;
            end;
        end;
      SetLength(buff, 0);
    end;
end;

procedure TC40_FS2_Service.cmd_FS2_Size(Sender: TPeerIO; InData, OutData: TDFE);
var
  fd: TFS2_Service_File_Data;
begin
  while InData.R.NotEnd do
    begin
      fd := FileHashPool[InData.R.ReadString];
      if fd = nil then
          OutData.WriteInt64(-1)
      else
          OutData.WriteInt64(fd.FileSize);
    end;
end;

procedure TC40_FS2_Service.cmd_FS2_Search(Sender: TPeerIO; InData, OutData: TDFE);
var
  filter_: U_String;
  MaxNum: Integer;
{$IFDEF FPC}
  procedure fpc_progress_(const Name_: PSystemString; Obj_: TFS2_Service_File_Data);
  begin
    if (OutData.Count div 5) > MaxNum then
        exit;
    if umlSearchMatch(filter_, Name_^) then
      begin
        OutData.WriteString(Obj_.FileName);
        OutData.WriteDouble(Obj_.FileTime);
        OutData.WriteInteger(Obj_.FileRef);
        OutData.WriteInt64(Obj_.FileSize);
        OutData.WriteMD5(Obj_.FileMD5);
      end;
  end;
{$ENDIF FPC}


begin
  filter_ := InData.R.ReadString;
  MaxNum := InData.R.ReadInteger;

{$IFDEF FPC}
  FileHashPool.ProgressP(@fpc_progress_);
{$ELSE FPC}
  FileHashPool.ProgressP(procedure(const Name_: PSystemString; Obj_: TFS2_Service_File_Data)
    begin
      if (OutData.Count div 5) > MaxNum then
          exit;
      if umlSearchMatch(filter_, Name_^) then
        begin
          OutData.WriteString(Obj_.FileName);
          OutData.WriteDouble(Obj_.FileTime);
          OutData.WriteInteger(Obj_.FileRef);
          OutData.WriteInt64(Obj_.FileSize);
          OutData.WriteMD5(Obj_.FileMD5);
        end;
    end);
{$ENDIF FPC}

end;

constructor TC40_FS2_Service.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
var
  fs: TCore_Stream;
  fd: TFS2_Service_File_Data;
  i: Integer;
begin
  inherited Create(PhysicsService_, ServiceTyp, Param_);
  // max complete buffer 100M
  DTNoAuthService.RecvTunnel.MaxCompleteBufferSize := EStrToInt64(ParamList.GetDefaultValue('MaxBuffer', '100*1024*1024'), 100 * 1024 * 1024);
  DTNoAuthService.RecvTunnel.CompleteBufferCompressed := False;
  DTNoAuthService.RecvTunnel.RegisterCompleteBuffer('FS2_PostFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_PostFile;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('FS2_GetFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_GetFile;
  DTNoAuthService.RecvTunnel.RegisterStream('FS2_GetFileMD5').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_GetFileMD5;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('FS2_RemoveFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_RemoveFile;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('FS2_UpdateFileRef').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_UpdateFileRef;
  DTNoAuthService.RecvTunnel.RegisterDirectStream('FS2_IncFileRef').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_IncFileRef;
  DTNoAuthService.RecvTunnel.RegisterStream('FS2_Size').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_Size;
  DTNoAuthService.RecvTunnel.RegisterStream('FS2_Search').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_FS2_Search;
  // instance
  ServiceInfo.OnlyInstance := True;
  UpdateToGlobalDispatch;

  ZDB2RecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('RecycleMemory', '5*1000'), 5 * 1000);
  ZDB2DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('DeltaSpace', '128*1024*1024'), 128 * 1024 * 1024);
  ZDB2BlockSize := EStrToInt(ParamList.GetDefaultValue('BlockSize', '1536'), 1536);
  ZDB2EnabledCipher := EStrToBool(ParamList.GetDefaultValue('EnabledCipher', 'True'), True);
  ZDB2CipherName := ParamList.GetDefaultValue('Cipher', TCipher.CCipherSecurityName[TCipherSecurity.csRijndael]);
  ZDB2Password := ParamList.GetDefaultValue('Password', Z.Net.C4.C40_Password);

  if ZDB2EnabledCipher then
      ZDB2Cipher := TZDB2_Cipher.Create(ZDB2CipherName, ZDB2Password, 1, True, True)
  else
      ZDB2Cipher := nil;
  C40_FS2_FileName := umlCombineFileName(DTNoAuthService.PublicFileDirectory, PFormat('DTC40_%s.Space2', [ServiceInfo.ServiceTyp.Text]));

  FileHashPool := TFS2_Service_File_Data_Pool.Create(True, 16 * 1024 * 1024, nil);
  FileHashPool.IgnoreCase := True;

  if umlFileExists(C40_FS2_FileName) then
      fs := TCore_FileStream.Create(C40_FS2_FileName, fmOpenReadWrite)
  else
      fs := TCore_FileStream.Create(C40_FS2_FileName, fmCreate);

  FileDatabase := TZDB2_List_MS64.Create(
    TZDB2_MS64,
    nil,
    ZDB2RecycleMemoryTimeOut,
    fs,
    False,
    ZDB2DeltaSpace,
    ZDB2BlockSize,
    ZDB2Cipher);
  FileDatabase.AutoFreeStream := True;

  i := 0;
  while i < FileDatabase.Count do
    begin
      fd := TFS2_Service_File_Data.Create(self, FileDatabase[i]);
      inc(i);
      fd.Stream.Data.Position := 0;
      fd.FileName := fd.Stream.Data.ReadString;
      fd.FileTime := fd.Stream.Data.ReadDouble;
      fd.FileRef := fd.Stream.Data.ReadInt32;
      fd.FileMD5 := umlMD5(fd.Stream.Data.PosAsPtr, fd.Stream.Data.Size - fd.Stream.Data.Position);
      fd.FileSize := fd.Stream.Data.Size - fd.Stream.Data.Position;
      fd.Stream.RecycleMemory;
      FileHashPool.Add(fd.FileName, fd);
    end;
end;

destructor TC40_FS2_Service.Destroy;
{$IFDEF FPC}
  procedure fpc_progress_(const Name_: PSystemString; Obj_: TFS2_Service_File_Data);
  begin
    Obj_.Stream := nil;
  end;
{$ENDIF FPC}


begin
{$IFDEF FPC}
  FileHashPool.ProgressP(@fpc_progress_);
{$ELSE FPC}
  FileHashPool.ProgressP(procedure(const Name_: PSystemString; Obj_: TFS2_Service_File_Data)
    begin
      Obj_.Stream := nil;
    end);
{$ENDIF FPC}
  DisposeObjectAndNil(FileHashPool);
  DisposeObjectAndNil(FileDatabase);
  inherited Destroy;
end;

procedure TC40_FS2_Service.SafeCheck;
begin
  inherited SafeCheck;
  FileDatabase.Flush;
end;

procedure TC40_FS2_Service.Progress;
begin
  inherited Progress;
  FileDatabase.Progress;
end;

constructor TC40_FS2_Client.TFS2_Client_CacheData.Create(Owner_: TC40_FS2_Client);
begin
  inherited Create;
  Owner := Owner_;
  Stream := Owner.Cache.NewData;
  LastAccess := GetTimeTick();
end;

destructor TC40_FS2_Client.TFS2_Client_CacheData.Destroy;
begin
  Owner.Cache.Remove(Stream, True);
  inherited Destroy;
end;

constructor TC40_FS2_Client.TFS2_Temp_Post_File_Tunnel.Create;
begin
  inherited Create;
  p2pClient := nil;
  Client := nil;
  File_Name := '';
  Stream := TMS64.Create;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TC40_FS2_Client.TFS2_Temp_Post_File_Tunnel.Destroy;
begin
  DisposeObject(Stream);
  inherited Destroy;
end;

procedure TC40_FS2_Client.TFS2_Temp_Post_File_Tunnel.DoP2PVM_CloneConnectAndPostFile(Sender: TZNet_WithP2PVM_Client);
var
  tmp_File_Name_: U_String;
  tmp_Time_: TDateTime;
  Cache: TFS2_Client_CacheData;
begin
  Stream.Position := 0;
  tmp_File_Name_ := Stream.ReadString;
  tmp_Time_ := Stream.ReadDouble;
  Cache := TFS2_Client_CacheData.Create(Client);
  Client.FileCacheHashPool.Add(tmp_File_Name_, Cache);
  Sender.Print('build cache "%s"', [File_Name.Text]);
  Cache.Stream.Data.WritePtr(Stream.PosAsPtr, Stream.Size - Stream.Position);
  Cache.Stream.Save;
  Cache.LastAccess := GetTimeTick();
  Sender.Print('update cache "%s"', [File_Name.Text]);

  p2pClient := Sender;
  p2pClient.RegisterDirectConsole('PostDone').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_PostDone;
  Sender.SendCompleteBuffer('FS2_PostFile', Stream.Memory, Stream.Size, True);
  Stream.DiscardMemory;
end;

procedure TC40_FS2_Client.TFS2_Temp_Post_File_Tunnel.cmd_PostDone(Sender: TPeerIO; InData: SystemString);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, InData);
    if Assigned(OnResultM) then
        OnResultM(Client, InData);
    if Assigned(OnResultP) then
        OnResultP(Client, InData);
  except
  end;
  p2pClient.P2PVM_Clone_NextProgressDoFreeSelf := True;
end;

constructor TC40_FS2_Client.TFS2_Temp_Get_File_Tunnel.Create;
begin
  inherited Create;
  p2pClient := nil;
  Client := nil;
  File_Name := '';
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TC40_FS2_Client.TFS2_Temp_Get_File_Tunnel.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_FS2_Client.TFS2_Temp_Get_File_Tunnel.cmd_Save(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  tmp1, tmp2: TMS64;
  tmp_File_Name_: U_String;
  tmp_Time_: TDateTime;
  tmp_ref: Integer;
  Cache: TFS2_Client_CacheData;
begin
  tmp1 := TMS64.Create;
  tmp1.Mapping(InData, DataSize);
  tmp_File_Name_ := tmp1.ReadString;
  tmp_Time_ := tmp1.ReadDouble;
  tmp_ref := tmp1.ReadInt32;
  tmp2 := TMS64.Create;
  tmp2.Mapping(tmp1.PosAsPtr, tmp1.Size - tmp1.Position);

  Cache := TFS2_Client_CacheData.Create(Client);
  Client.FileCacheHashPool.Add(tmp_File_Name_, Cache);
  Sender.Print('build cache "%s"', [tmp_File_Name_.Text]);
  Cache.Stream.Data.LoadFromStream(tmp2);
  Cache.Stream.Save;
  Cache.LastAccess := GetTimeTick();
  Sender.Print('update cache "%s"', [tmp_File_Name_.Text]);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, tmp2, tmp_File_Name_, True);
    if Assigned(OnResultM) then
        OnResultM(Client, tmp2, tmp_File_Name_, True);
    if Assigned(OnResultP) then
        OnResultP(Client, tmp2, tmp_File_Name_, True);
  except
  end;

  DisposeObject(tmp1);
  DisposeObject(tmp2);

  p2pClient.P2PVM_Clone_NextProgressDoFreeSelf := True;
end;

procedure TC40_FS2_Client.TFS2_Temp_Get_File_Tunnel.cmd_Error(Sender: TPeerIO; InData: SystemString);
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Client, nil, InData, False);
    if Assigned(OnResultM) then
        OnResultM(Client, nil, InData, False);
    if Assigned(OnResultP) then
        OnResultP(Client, nil, InData, False);
  except
  end;

  p2pClient.P2PVM_Clone_NextProgressDoFreeSelf := True;
end;

procedure TC40_FS2_Client.TFS2_Temp_Get_File_Tunnel.DoP2PVM_CloneConnectAndGetFile(Sender: TZNet_WithP2PVM_Client);
var
  d: TDFE;
begin
  p2pClient := Sender;
  Sender.RegisterCompleteBuffer('Save').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Save;
  Sender.RegisterDirectConsole('Error').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_Error;
  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteCardinal(Sender.ClientIO.ID);
  Client.DTNoAuthClient.SendTunnel.SendDirectStreamCmd('FS2_GetFile', d);
  DisposeObject(d);
end;

constructor TC40_FS2_Client.TFS2_Temp_GetFileMD5.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_FS2_Client.TFS2_Temp_GetFileMD5.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  State_: Boolean;
  info_: SystemString;
  MD5: TMD5;
begin
  State_ := False;
  info_ := 'error';
  MD5 := NullMD5;
  if Result_.Count >= 2 then
    begin
      State_ := Result_.R.ReadBool;
      info_ := Result_.R.ReadString;
      MD5 := Result_.R.ReadMD5;
    end;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_, MD5);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_, MD5);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_, MD5);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TC40_FS2_Client.TFS2_Temp_GetFileMD5.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  State_: Boolean;
  info_: SystemString;
  MD5: TMD5;
begin
  State_ := False;
  info_ := 'error.';
  MD5 := NullMD5;

  try
    if Assigned(OnResultC) then
        OnResultC(Client, State_, info_, MD5);
    if Assigned(OnResultM) then
        OnResultM(Client, State_, info_, MD5);
    if Assigned(OnResultP) then
        OnResultP(Client, State_, info_, MD5);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_FS2_Client.TFS2_Temp_GetFileMD5_Cache.Create;
begin
  inherited Create;
  Client := nil;
  File_Name := '';
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TC40_FS2_Client.TFS2_Temp_GetFileMD5_Cache.Destroy;
begin
  File_Name := '';
  inherited Destroy;
end;

procedure TC40_FS2_Client.TFS2_Temp_GetFileMD5_Cache.Do_FS2_GetFileMD5(Sender: TC40_FS2_Client; State_: Boolean; info_: SystemString; MD5_: TMD5);
var
  Cache: TFS2_Client_CacheData;
  tmp: TFS2_Temp_Get_File_Tunnel;
begin
  if not State_ then
    begin
      DoStatus(info_);
      try
        if Assigned(OnResultC) then
            OnResultC(Client, nil, File_Name, False);
        if Assigned(OnResultM) then
            OnResultM(Client, nil, File_Name, False);
        if Assigned(OnResultP) then
            OnResultP(Client, nil, File_Name, False);
      except
      end;
      Client.DTNoAuth.ProgressEngine.PostDelayFreeObject(1.0, self, nil);
      exit;
    end;

  Cache := Client.FileCacheHashPool[File_Name];
  if (Cache <> nil) then
    begin
      if umlMD5Compare(umlStreamMD5(Cache.Stream.Data), MD5_) then
        begin
          Cache.LastAccess := GetTimeTick();
          Sender.DTNoAuth.RecvTunnel.Print('get file "%s" from cache', [File_Name.Text]);;
          try
            Cache.Stream.Data.Position := 0;
            if Assigned(OnResultC) then
                OnResultC(Client, Cache.Stream.Data, File_Name, True);
            if Assigned(OnResultM) then
                OnResultM(Client, Cache.Stream.Data, File_Name, True);
            if Assigned(OnResultP) then
                OnResultP(Client, Cache.Stream.Data, File_Name, True);
          except
          end;
          Client.DTNoAuth.ProgressEngine.PostDelayFreeObject(1.0, self, nil);
          exit;
        end;
    end;

  tmp := TFS2_Temp_Get_File_Tunnel.Create;
  tmp.Client := Client;
  tmp.File_Name := File_Name;
  tmp.OnResultC := OnResultC;
  tmp.OnResultM := OnResultM;
  tmp.OnResultP := OnResultP;
  Client.Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);
  Client.DTNoAuth.ProgressEngine.PostDelayFreeObject(1.0, self, nil);
end;

constructor TC40_FS2_Client.TFS2_Temp_Size.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_FS2_Client.TFS2_Temp_Size.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  arry: TFS2_FileSizeInfo_Array;
  i: Integer;
begin
  SendData.R.Index := 0;
  SetLength(arry, Result_.Count);
  i := 0;
  while Result_.R.NotEnd do
    begin
      arry[i].FileName := SendData.R.ReadString;
      arry[i].Size := Result_.R.ReadInt64;
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
  DelayFreeObject(1.0, self);
end;

procedure TC40_FS2_Client.TFS2_Temp_Size.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  arry: TFS2_FileSizeInfo_Array;
begin
  SetLength(arry, 0);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  DelayFreeObject(1.0, self);
end;

constructor TC40_FS2_Client.TFS2_Temp_Search.Create;
begin
  inherited Create;
  Client := nil;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

procedure TC40_FS2_Client.TFS2_Temp_Search.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  arry: TFS2_FileInfo_Array;
  i: Integer;
begin
  SetLength(arry, Result_.Count div 5);
  i := 0;
  while Result_.R.NotEnd do
    begin
      arry[i].FileName := Result_.R.ReadString;
      arry[i].FileTime := Result_.R.ReadDouble;
      arry[i].FileRef := Result_.R.ReadInteger;
      arry[i].Size := Result_.R.ReadInt64;
      arry[i].MD5 := Result_.R.ReadMD5;
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
  DelayFreeObject(1.0, self);
end;

procedure TC40_FS2_Client.TFS2_Temp_Search.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  arry: TFS2_FileInfo_Array;
begin
  SetLength(arry, 0);

  try
    if Assigned(OnResultC) then
        OnResultC(Client, arry);
    if Assigned(OnResultM) then
        OnResultM(Client, arry);
    if Assigned(OnResultP) then
        OnResultP(Client, arry);
  except
  end;
  DelayFreeObject(1.0, self);
end;

procedure TC40_FS2_Client.Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender: TDT_P2PVM_NoAuth_Custom_Client);
begin
  inherited Do_DT_P2PVM_NoAuth_Custom_Client_TunnelLink(Sender);
  FMaxFileSize := DTNoAuth.SendTunnel.ServerState^.MaxCompleteBufferSize;
end;

procedure TC40_FS2_Client.Do_Progress_FileCachePool(const Name_: PSystemString; Obj_: TFS2_Client_CacheData);
begin
  if GetTimeTick() - Obj_.LastAccess > Cache_File_Life then
      FRemoveCacheList.Add(Name_^);
end;

constructor TC40_FS2_Client.Create(source_: TC40_Info; Param_: U_String);
var
  i: Integer;
begin
  inherited Create(source_, Param_);
  C40_FS2_Cache_FileName := umlCombineFileName({$IFDEF FPC}C40_RootPath{$ELSE FPC}TPath.GetTempPath{$ENDIF FPC},
    PFormat('DTC40_FS2_Cache_%s_%s.tmp', [source_.ServiceTyp.Text, umlMD5ToStr(source_.Hash).Text]));
  i := 1;
  while umlFileExists(C40_FS2_Cache_FileName) do
    begin
      C40_FS2_Cache_FileName := umlCombineFileName({$IFDEF FPC}C40_RootPath{$ELSE FPC}TPath.GetTempPath{$ENDIF FPC},
        PFormat('DTC40_FS2_Cache_%s_%s(%d).tmp', [source_.ServiceTyp.Text, umlMD5ToStr(source_.Hash).Text, i]));
      inc(i);
    end;

  ZDB2RecycleMemoryTimeOut := EStrToInt64(ParamList.GetDefaultValue('RecycleMemory', '5*1000'), 5 * 1000);
  ZDB2DeltaSpace := EStrToInt64(ParamList.GetDefaultValue('DeltaSpace', '128*1024*1024'), 128 * 1024 * 1024);
  ZDB2BlockSize := EStrToInt(ParamList.GetDefaultValue('BlockSize', '1536'), 1536);
  ZDB2EnabledCipher := EStrToBool(ParamList.GetDefaultValue('EnabledCipher', 'True'), True);
  ZDB2CipherName := ParamList.GetDefaultValue('Cipher', TCipher.CCipherSecurityName[TCipherSecurity.csRijndael]);
  ZDB2Password := ParamList.GetDefaultValue('Password', Z.Net.C4.C40_Password);
  Cache_File_Life := EStrToInt64(ParamList.GetDefaultValue('CacheLife', '10*60*1000'), 10 * 60 * 1000);

  if ZDB2EnabledCipher then
      ZDB2Cipher := TZDB2_Cipher.Create(ZDB2CipherName, ZDB2Password, 1, True, True)
  else
      ZDB2Cipher := nil;

  FileCacheHashPool := TFS2_Client_CacheHashPool.Create(True, 64 * 1024 * 1024, nil);
  Cache := TZDB2_List_MS64.Create(
    TZDB2_MS64,
    nil,
    ZDB2RecycleMemoryTimeOut,
    TCore_FileStream.Create(C40_FS2_Cache_FileName, fmCreate),
    False,
    ZDB2DeltaSpace,
    ZDB2BlockSize,
    ZDB2Cipher);
  Cache.AutoFreeStream := True;

  FMaxFileSize := 0;
  FRemoveCacheList := TPascalStringList.Create;
end;

destructor TC40_FS2_Client.Destroy;
begin
  DisposeObject(FileCacheHashPool);
  DisposeObject(Cache);
  umlDeleteFile(C40_FS2_Cache_FileName);
  DisposeObject(FRemoveCacheList);
  inherited Destroy;
end;

procedure TC40_FS2_Client.SafeCheck;
var
  i: Integer;
begin
  inherited SafeCheck;
  FRemoveCacheList.Clear;
  FileCacheHashPool.ProgressM({$IFDEF FPC}@{$ENDIF FPC}Do_Progress_FileCachePool);
  if FRemoveCacheList.Count > 0 then
    begin
      for i := 0 to FRemoveCacheList.Count - 1 do
          FileCacheHashPool.Delete(FRemoveCacheList[i]);
      FRemoveCacheList.Clear;
    end;
  Cache.Flush;
end;

procedure TC40_FS2_Client.Progress;
begin
  inherited Progress;
  Cache.Progress;
end;

procedure TC40_FS2_Client.FS2_PostFile(File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean);
var
  tmp: TFS2_Temp_Post_File_Tunnel;
begin
  if Stream.Size = 0 then
    begin
      if doneFree then
          DisposeObject(Stream);
      exit;
    end;
  tmp := TFS2_Temp_Post_File_Tunnel.Create;
  tmp.Client := self;
  tmp.File_Name := File_Name;
  tmp.Stream.WriteString(File_Name); // name
  tmp.Stream.WriteDouble(umlNow);    // file time
  tmp.Stream.WriteInt32(0);          // ref
  Stream.Position := 0;
  tmp.Stream.CopyFrom(Stream, Stream.Size);
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
  if doneFree then
      DisposeObject(Stream);
end;

procedure TC40_FS2_Client.FS2_PostFile_C(File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean; OnResult: TON_FS2_PostFile_DoneC);
var
  tmp: TFS2_Temp_Post_File_Tunnel;
begin
  if Stream.Size = 0 then
    begin
      if doneFree then
          DisposeObject(Stream);
      exit;
    end;
  tmp := TFS2_Temp_Post_File_Tunnel.Create;
  tmp.Client := self;
  tmp.File_Name := File_Name;
  tmp.OnResultC := OnResult;
  tmp.Stream.WriteString(File_Name); // name
  tmp.Stream.WriteDouble(umlNow);    // file time
  tmp.Stream.WriteInt32(0);          // ref
  Stream.Position := 0;
  tmp.Stream.CopyFrom(Stream, Stream.Size);
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
  if doneFree then
      DisposeObject(Stream);
end;

procedure TC40_FS2_Client.FS2_PostFile_M(File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean; OnResult: TON_FS2_PostFile_DoneM);
var
  tmp: TFS2_Temp_Post_File_Tunnel;
begin
  if Stream.Size = 0 then
    begin
      if doneFree then
          DisposeObject(Stream);
      exit;
    end;
  tmp := TFS2_Temp_Post_File_Tunnel.Create;
  tmp.Client := self;
  tmp.File_Name := File_Name;
  tmp.OnResultM := OnResult;
  tmp.Stream.WriteString(File_Name); // name
  tmp.Stream.WriteDouble(umlNow);    // file time
  tmp.Stream.WriteInt32(0);          // ref
  Stream.Position := 0;
  tmp.Stream.CopyFrom(Stream, Stream.Size);
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
  if doneFree then
      DisposeObject(Stream);
end;

procedure TC40_FS2_Client.FS2_PostFile_P(File_Name: U_String; Stream: TCore_Stream; doneFree: Boolean; OnResult: TON_FS2_PostFile_DoneP);
var
  tmp: TFS2_Temp_Post_File_Tunnel;
begin
  if Stream.Size = 0 then
    begin
      if doneFree then
          DisposeObject(Stream);
      exit;
    end;
  tmp := TFS2_Temp_Post_File_Tunnel.Create;
  tmp.Client := self;
  tmp.File_Name := File_Name;
  tmp.OnResultP := OnResult;
  tmp.Stream.WriteString(File_Name); // name
  tmp.Stream.WriteDouble(umlNow);    // file time
  tmp.Stream.WriteInt32(0);          // ref
  Stream.Position := 0;
  tmp.Stream.CopyFrom(Stream, Stream.Size);
  Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndPostFile);
  if doneFree then
      DisposeObject(Stream);
end;

procedure TC40_FS2_Client.FS2_GetFile_C(UsedCache_: Boolean; File_Name: U_String; OnResult: TON_FS2_GetFile_DoneC);
var
  tmp_cache_: TFS2_Temp_GetFileMD5_Cache;
  tmp: TFS2_Temp_Get_File_Tunnel;
begin
  if UsedCache_ and FileCacheHashPool.Exists(File_Name) then
    begin
      tmp_cache_ := TFS2_Temp_GetFileMD5_Cache.Create;
      tmp_cache_.Client := self;
      tmp_cache_.File_Name := File_Name;
      tmp_cache_.OnResultC := OnResult;
      FS2_GetFileMD5M(File_Name, {$IFDEF FPC}@{$ENDIF FPC}tmp_cache_.Do_FS2_GetFileMD5);
    end
  else
    begin
      tmp := TFS2_Temp_Get_File_Tunnel.Create;
      tmp.Client := self;
      tmp.File_Name := File_Name;
      tmp.OnResultC := OnResult;
      Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);
    end;
end;

procedure TC40_FS2_Client.FS2_GetFile_M(UsedCache_: Boolean; File_Name: U_String; OnResult: TON_FS2_GetFile_DoneM);
var
  tmp_cache_: TFS2_Temp_GetFileMD5_Cache;
  tmp: TFS2_Temp_Get_File_Tunnel;
begin
  if UsedCache_ and FileCacheHashPool.Exists(File_Name) then
    begin
      tmp_cache_ := TFS2_Temp_GetFileMD5_Cache.Create;
      tmp_cache_.Client := self;
      tmp_cache_.File_Name := File_Name;
      tmp_cache_.OnResultM := OnResult;
      FS2_GetFileMD5M(File_Name, {$IFDEF FPC}@{$ENDIF FPC}tmp_cache_.Do_FS2_GetFileMD5);
    end
  else
    begin
      tmp := TFS2_Temp_Get_File_Tunnel.Create;
      tmp.Client := self;
      tmp.File_Name := File_Name;
      tmp.OnResultM := OnResult;
      Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);
    end;
end;

procedure TC40_FS2_Client.FS2_GetFile_P(UsedCache_: Boolean; File_Name: U_String; OnResult: TON_FS2_GetFile_DoneP);
var
  tmp_cache_: TFS2_Temp_GetFileMD5_Cache;
  tmp: TFS2_Temp_Get_File_Tunnel;
begin
  if UsedCache_ and FileCacheHashPool.Exists(File_Name) then
    begin
      tmp_cache_ := TFS2_Temp_GetFileMD5_Cache.Create;
      tmp_cache_.Client := self;
      tmp_cache_.File_Name := File_Name;
      tmp_cache_.OnResultP := OnResult;
      FS2_GetFileMD5M(File_Name, {$IFDEF FPC}@{$ENDIF FPC}tmp_cache_.Do_FS2_GetFileMD5);
    end
  else
    begin
      tmp := TFS2_Temp_Get_File_Tunnel.Create;
      tmp.Client := self;
      tmp.File_Name := File_Name;
      tmp.OnResultP := OnResult;
      Client.SendTunnel.CloneConnectM({$IFDEF FPC}@{$ENDIF FPC}tmp.DoP2PVM_CloneConnectAndGetFile);
    end;
end;

procedure TC40_FS2_Client.FS2_GetFileMD5C(File_Name: U_String; OnResult: TFS2_Temp_GetFileMD5C);
var
  tmp: TFS2_Temp_GetFileMD5;
  d: TDFE;
begin
  tmp := TFS2_Temp_GetFileMD5.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  d.WriteString(File_Name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_GetFileMD5', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_FS2_Client.FS2_GetFileMD5M(File_Name: U_String; OnResult: TFS2_Temp_GetFileMD5M);
var
  tmp: TFS2_Temp_GetFileMD5;
  d: TDFE;
begin
  tmp := TFS2_Temp_GetFileMD5.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  d.WriteString(File_Name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_GetFileMD5', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_FS2_Client.FS2_GetFileMD5P(File_Name: U_String; OnResult: TFS2_Temp_GetFileMD5P);
var
  tmp: TFS2_Temp_GetFileMD5;
  d: TDFE;
begin
  tmp := TFS2_Temp_GetFileMD5.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  d.WriteString(File_Name);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_GetFileMD5', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_FS2_Client.FS2_RemoveFile(File_Name: U_String);
var
  d: TDFE;
begin
  FileCacheHashPool.Delete(File_Name);
  d := TDFE.Create;
  d.WriteString(File_Name);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('FS2_RemoveFile', d);
  DisposeObject(d);
end;

procedure TC40_FS2_Client.FS2_UpdateFileRef(File_Name: U_String; ref_: Integer);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteInteger(ref_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('FS2_UpdateFileRef', d);
  DisposeObject(d);
end;

procedure TC40_FS2_Client.FS2_IncFileRef(File_Name: U_String; inc_ref_: Integer);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(File_Name);
  d.WriteInteger(inc_ref_);
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('FS2_IncFileRef', d);
  DisposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SizeC(FileNames: U_StringArray; OnResult: TFS2_Temp_SizeC);
var
  tmp: TFS2_Temp_Size;
  d: TDFE;
  i: Integer;
begin
  tmp := TFS2_Temp_Size.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  for i := 0 to length(FileNames) - 1 do
      d.WriteString(FileNames[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_Size', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SizeM(FileNames: U_StringArray; OnResult: TFS2_Temp_SizeM);
var
  tmp: TFS2_Temp_Size;
  d: TDFE;
  i: Integer;
begin
  tmp := TFS2_Temp_Size.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  for i := 0 to length(FileNames) - 1 do
      d.WriteString(FileNames[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_Size', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SizeP(FileNames: U_StringArray; OnResult: TFS2_Temp_SizeP);
var
  tmp: TFS2_Temp_Size;
  d: TDFE;
  i: Integer;
begin
  tmp := TFS2_Temp_Size.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  for i := 0 to length(FileNames) - 1 do
      d.WriteString(FileNames[i]);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_Size', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SearchC(filter: U_String; MaxNum: Integer; OnResult: TFS2_Temp_SearchC);
var
  tmp: TFS2_Temp_Search;
  d: TDFE;
  i: Integer;
begin
  tmp := TFS2_Temp_Search.Create;
  tmp.Client := self;
  tmp.OnResultC := OnResult;
  d := TDFE.Create;
  d.WriteString(filter);
  d.WriteInteger(MaxNum);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_Search', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SearchM(filter: U_String; MaxNum: Integer; OnResult: TFS2_Temp_SearchM);
var
  tmp: TFS2_Temp_Search;
  d: TDFE;
  i: Integer;
begin
  tmp := TFS2_Temp_Search.Create;
  tmp.Client := self;
  tmp.OnResultM := OnResult;
  d := TDFE.Create;
  d.WriteString(filter);
  d.WriteInteger(MaxNum);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_Search', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_FS2_Client.FS2_SearchP(filter: U_String; MaxNum: Integer; OnResult: TFS2_Temp_SearchP);
var
  tmp: TFS2_Temp_Search;
  d: TDFE;
  i: Integer;
begin
  tmp := TFS2_Temp_Search.Create;
  tmp.Client := self;
  tmp.OnResultP := OnResult;
  d := TDFE.Create;
  d.WriteString(filter);
  d.WriteInteger(MaxNum);
  DTNoAuthClient.SendTunnel.SendStreamCmdM('FS2_Search', d, nil, nil,
{$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamParamEvent, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoStreamFailedEvent);
  DisposeObject(d);
end;

initialization

RegisterC40('FS2', TC40_FS2_Service, TC40_FS2_Client);

end.
