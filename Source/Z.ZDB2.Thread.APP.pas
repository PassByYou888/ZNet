{ ****************************************************************************** }
{ * ZDB 2.0 Core-Thread Application for HPC                                    * }
{ ****************************************************************************** }
unit Z.ZDB2.Thread.APP;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses Z.Core,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib,
  Z.MemoryStream,
  Z.Status, Z.Cipher, Z.ZDB2, Z.ListEngine, Z.TextDataEngine, Z.IOThread,
  Z.Notify, Z.ZDB2.Thread;

type
  TZDB2_Th_Engine_Data_Mem64 = class(TZDB2_Th_Engine_Data)
  protected
    FAlive: TTimeTick;
    FData_Mem64: TMem64;
    FData_Mem64_MD5: TMD5;
  public
    TimeOut: TTimeTick;
    constructor Create; override;
    destructor Destroy; override;
    procedure Progress(); override;
    procedure Load; virtual;
    procedure Save; virtual;
    procedure RecycleMemory; virtual;
    function GetData_Mem64: TMem64; virtual;
    property Data_Mem64: TMem64 read GetData_Mem64;
    property Data_Mem64_MD5: TMD5 read FData_Mem64_MD5;
  end;

  TZDB2_Th_Engine_Data_MS64 = class(TZDB2_Th_Engine_Data)
  protected
    FAlive: TTimeTick;
    FData_MS64: TMS64;
    FData_MS64_MD5: TMD5;
  public
    TimeOut: TTimeTick;
    constructor Create; override;
    destructor Destroy; override;
    procedure Progress(); override;
    procedure Load; virtual;
    procedure Save; virtual;
    procedure RecycleMemory; virtual;
    function GetData_MS64: TMS64; virtual;
    property Data_MS64: TMS64 read GetData_MS64;
    property Data_MS64_MD5: TMD5 read FData_MS64_MD5;
  end;

implementation

constructor TZDB2_Th_Engine_Data_Mem64.Create;
begin
  inherited Create;
  FAlive := GetTimeTick();
  FData_Mem64 := nil;
  FData_Mem64_MD5 := NullMD5;
  TimeOut := 5000;
end;

destructor TZDB2_Th_Engine_Data_Mem64.Destroy;
begin
  RecycleMemory;
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Data_Mem64.Progress();
begin
  if FData_Mem64 = nil then
      exit;
  if (GetTimeTick() - FAlive > TimeOut) then
      Save();
end;

procedure TZDB2_Th_Engine_Data_Mem64.Load;
begin
  FData_Mem64_MD5 := NullMD5;
  FData_Mem64.Clear;
  if Load_Data(FData_Mem64) then
      FData_Mem64_MD5 := FData_Mem64.ToMD5;
end;

procedure TZDB2_Th_Engine_Data_Mem64.Save;
var
  tmp_md5: TMD5;
begin
  if FData_Mem64 = nil then
      exit;

  tmp_md5 := FData_Mem64.ToMD5;
  if (ID < 0) or umlMD5Compare(FData_Mem64_MD5, NullMD5) or (not umlMD5Compare(tmp_md5, FData_Mem64_MD5)) then
    begin
      FData_Mem64_MD5 := tmp_md5;
      Async_Save_And_Free_Data(FData_Mem64);
    end
  else
      disposeObjectAndNil(FData_Mem64);
end;

procedure TZDB2_Th_Engine_Data_Mem64.RecycleMemory;
begin
  disposeObjectAndNil(FData_Mem64);
end;

function TZDB2_Th_Engine_Data_Mem64.GetData_Mem64: TMem64;
begin
  if FData_Mem64 = nil then
    begin
      FData_Mem64 := TMem64.Create;
      Load;
    end;
  Result := FData_Mem64;
  FAlive := GetTimeTick;
end;

constructor TZDB2_Th_Engine_Data_MS64.Create;
begin
  inherited Create;
  FAlive := GetTimeTick();
  FData_MS64 := nil;
  FData_MS64_MD5 := NullMD5;
  TimeOut := 5000;
end;

destructor TZDB2_Th_Engine_Data_MS64.Destroy;
begin
  RecycleMemory;
  inherited Destroy;
end;

procedure TZDB2_Th_Engine_Data_MS64.Progress();
begin
  if FData_MS64 = nil then
      exit;
  if (GetTimeTick() - FAlive > TimeOut) then
      Save();
end;

procedure TZDB2_Th_Engine_Data_MS64.Load;
begin
  FData_MS64_MD5 := NullMD5;
  FData_MS64.Clear;
  if Load_Data(FData_MS64) then
      FData_MS64_MD5 := FData_MS64.ToMD5;
end;

procedure TZDB2_Th_Engine_Data_MS64.Save;
var
  tmp_md5: TMD5;
begin
  if FData_MS64 = nil then
      exit;

  tmp_md5 := FData_MS64.ToMD5;
  if (ID < 0) or umlMD5Compare(FData_MS64_MD5, NullMD5) or (not umlMD5Compare(tmp_md5, FData_MS64_MD5)) then
    begin
      FData_MS64_MD5 := tmp_md5;
      Async_Save_And_Free_Data(FData_MS64);
    end
  else
      disposeObjectAndNil(FData_MS64);
end;

procedure TZDB2_Th_Engine_Data_MS64.RecycleMemory;
begin
  disposeObjectAndNil(FData_MS64);
end;

function TZDB2_Th_Engine_Data_MS64.GetData_MS64: TMS64;
begin
  if FData_MS64 = nil then
    begin
      FData_MS64 := TMS64.Create;
      Load;
    end;
  Result := FData_MS64;
  FAlive := GetTimeTick;
end;

end.
