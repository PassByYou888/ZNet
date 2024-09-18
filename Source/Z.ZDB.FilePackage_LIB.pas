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
{ * file Package                                                               * }
{ ****************************************************************************** }
unit Z.ZDB.FilePackage_LIB;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib,
  Z.ZDB.ObjectData_LIB, Z.ZDB, Z.ZDB.ItemStream_LIB, Z.Status, Z.ListEngine;

procedure BeginImportStreamToDB(dbEng: TObjectDataManager; md5List: THashStringList);
procedure ImportStreamToDB(md5List: THashStringList; stream: TCore_Stream; FileName: SystemString; dbEng: TObjectDataManager); overload;
procedure EndImportStreamToDB(dbEng: TObjectDataManager; md5List: THashStringList);

procedure BatchImportPathToDB(InitDir, Filter: SystemString; dbEng: TObjectDataManager);
procedure BatchImportPathToDBFile(InitDir, Filter, dbFile: SystemString);
procedure BatchImportPathToDBStream(InitDir, Filter: SystemString; DBStream: TCore_Stream);

function ExtractFileInDB(dbEng: TObjectDataManager; FileName: SystemString; ExtractToStream: TCore_Stream): Boolean; overload;
function ExtractFileInDB(dbEng: TObjectDataManager; FieldPos: Int64; FileName: SystemString; ExtractToStream: TCore_Stream): Boolean; overload;
function ExtractFileInDB(dbEng: TObjectDataManager; DBPath, FileName: SystemString; ExtractToStream: TCore_Stream): Boolean; overload;
function ExtractFileInDB(dbFileName, DBPath, FileName: SystemString; ExtractToStream: TCore_Stream): Boolean; overload;
function ExtractFileInDB(DBStream: TCore_Stream; DBPath, FileName: SystemString; ExtractToStream: TCore_Stream): Boolean; overload;

function ExistsFileInDB(DBStream: TCore_Stream; DBPath, FileName: SystemString): Boolean; overload;
function ExistsFileInDB(dbFileName, DBPath, FileName: SystemString): Boolean; overload;
function ExistsFileInDB(dbEng: TObjectDataManager; DBPath, FileName: SystemString): Boolean; overload;
function ExistsFileInDB(dbEng: TObjectDataManager; FileName: SystemString): Boolean; overload;

procedure ExtractDBToPath(dbEng: TObjectDataManager; ExtractToDir: SystemString; OutputFileList: TCore_Strings); overload;
procedure ExtractDBToPath(dbEng: TObjectDataManager; ExtractToDir: SystemString); overload;
procedure ExtractDBFileToPath(dbFile, ExtractToDir: SystemString);

function TestFileInDB(dbEng: TObjectDataManager): Integer; overload;
function TestFileInDB(DBStream: TCore_Stream): Integer; overload;
function TestFileInDB(dbFile: SystemString): Integer; overload;

var
  C_MD5_File: SystemString;
  FP_EncryptStream: procedure(sour, dest: TCore_Stream);
  FP_DecryptStream: procedure(sour, dest: TCore_Stream);

implementation

uses Z.MemoryStream;

procedure DoEncryptStream_(sour, dest: TCore_Stream);
begin
  dest.CopyFrom(sour, sour.Size);
end;

procedure DoDecryptStream_(sour, dest: TCore_Stream);
begin
  dest.CopyFrom(sour, sour.Size);
end;

procedure BeginImportStreamToDB(dbEng: TObjectDataManager; md5List: THashStringList);
var
  hashTextStream: THashStringTextStream;
  srHnd: TItemSearch;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  md5List.Clear;

  LockObject(dbEng);
  try

    if dbEng.ItemFastFindLast(dbEng.RootField, C_MD5_File, srHnd) then
      begin
        hashTextStream := THashStringTextStream.Create(md5List);
        if dbEng.ItemFastOpen(srHnd.HeaderPOS, itmHnd) then
          begin
            itmStream := TItemStream.Create(dbEng, itmHnd);
            hashTextStream.LoadFromStream(itmStream);
            DisposeObject(itmStream);
          end;
        DisposeObject(hashTextStream);
        dbEng.FastDelete(dbEng.RootField, srHnd.HeaderPOS);
      end;
  finally
      UnLockObject(dbEng);
  end;
end;

procedure ImportStreamToDB(md5List: THashStringList; stream: TCore_Stream; FileName: SystemString; dbEng: TObjectDataManager);
var
  FieldPos: Int64;
  srHnd: TItemSearch;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
  md5: SystemString;
begin
  LockObject(dbEng);
  try
    FieldPos := dbEng.RootField;

    if md5List.Exists(FileName) then
      if dbEng.ItemFastFindFirst(FieldPos, FileName, srHnd) then
          dbEng.FastDelete(FieldPos, srHnd.HeaderPOS);

    if dbEng.ItemFastCreate(FieldPos, FileName, '', itmHnd) then
      begin
        try
          itmStream := TItemStream.Create(dbEng, itmHnd);

          stream.Position := 0;
          md5 := umlStreamMD5Char(stream).Text;
          md5List.Add(itmHnd.Name, md5);

          stream.Position := 0;

          FP_EncryptStream(stream, itmStream);

          itmStream.UpdateHandle;
          DisposeObject(itmStream);
        except
        end;
      end;
  finally
      UnLockObject(dbEng);
  end;
end;

procedure EndImportStreamToDB(dbEng: TObjectDataManager; md5List: THashStringList);
var
  hashTextStream: THashStringTextStream;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  LockObject(dbEng);
  try
    hashTextStream := THashStringTextStream.Create(md5List);
    if dbEng.ItemFastCreate(dbEng.RootField, C_MD5_File, '', itmHnd) then
      begin
        try
          itmStream := TItemStream.Create(dbEng, itmHnd);
          hashTextStream.SaveToStream(itmStream);
          itmStream.UpdateHandle;
          DisposeObject(itmStream);
        except
        end;
      end;
    DisposeObject(hashTextStream);

    dbEng.UpdateIO;
  finally
      UnLockObject(dbEng);
  end;
end;

procedure BatchImportPathToDB(InitDir, Filter: SystemString; dbEng: TObjectDataManager);

  procedure AddPath(APath: SystemString; aFieldPos: Int64);
  var
    fAry: U_StringArray;
    n, suffixn: SystemString;
    fs: TCore_FileStream;
    itmHnd: TItemHandle;
    itmStream: TItemStream;
    fPos: Int64;
    md5: SystemString;
    md5List: THashStringList;
    hashTextStream: THashStringTextStream;
    srHnd: TItemSearch;
  begin
    md5List := THashStringList.Create;

    if dbEng.ItemFastFindLast(aFieldPos, C_MD5_File, srHnd) then
      begin
        hashTextStream := THashStringTextStream.Create(md5List);
        if dbEng.ItemFastOpen(srHnd.HeaderPOS, itmHnd) then
          begin
            itmStream := TItemStream.Create(dbEng, itmHnd);
            hashTextStream.LoadFromStream(itmStream);
            DisposeObject(itmStream);
          end;
        DisposeObject(hashTextStream);
        dbEng.FastDelete(aFieldPos, srHnd.HeaderPOS);
      end;

    fAry := umlGet_File_Full_Array(APath);
    for n in fAry do
      begin
        suffixn := umlGetFileName(n).Text;
        if umlMultipleMatch(Filter, suffixn) then
          begin
            if md5List.Exists(suffixn) then
              if dbEng.ItemFastFindFirst(aFieldPos, suffixn, srHnd) then
                  dbEng.FastDelete(aFieldPos, srHnd.HeaderPOS);

            if dbEng.ItemFastCreate(aFieldPos, suffixn, '', itmHnd) then
              begin
                try
                  fs := TCore_FileStream.Create(n, fmOpenRead or fmShareDenyNone);
                  itmStream := TItemStream.Create(dbEng, itmHnd);

                  fs.Position := 0;
                  md5 := umlStreamMD5Char(fs).Text;
                  md5List.Add(itmHnd.Name, md5);

                  fs.Position := 0;
                  FP_EncryptStream(fs, itmStream);

                  itmStream.UpdateHandle;
                  DisposeObject(itmStream);
                  DisposeObject(fs);
                except
                end;
              end;
          end;
      end;

    hashTextStream := THashStringTextStream.Create(md5List);

    if dbEng.ItemFastCreate(aFieldPos, C_MD5_File, '', itmHnd) then
      begin
        try
          itmStream := TItemStream.Create(dbEng, itmHnd);
          hashTextStream.SaveToStream(itmStream);
          itmStream.UpdateHandle;
          DisposeObject(itmStream);
        except
        end;
      end;
    DisposeObject(hashTextStream);
    DisposeObject(md5List);

    fAry := umlGet_Path_Full_Array(APath);
    for n in fAry do
      begin
        suffixn := umlGetLastStr(n, '/\').Text;
        if dbEng.FastFieldCreate(aFieldPos, suffixn, '', fPos) then
            AddPath(n, fPos);
      end;
  end;

begin
  LockObject(dbEng);
  try
    if dbEng <> nil then
      begin
        AddPath(InitDir, dbEng.RootField);
        dbEng.UpdateIO;
      end;
  finally
      UnLockObject(dbEng);
  end;
end;

procedure BatchImportPathToDBFile(InitDir, Filter, dbFile: SystemString);
var
  dbEng: TObjectDataManager;
begin
  dbEng := ObjectDataMarshal.NewDB($FF, dbFile, False);
  BatchImportPathToDB(InitDir, Filter, dbEng);
  dbEng.UpdateIO;
  ObjectDataMarshal.CloseDB(dbEng);
end;

procedure BatchImportPathToDBStream(InitDir, Filter: SystemString; DBStream: TCore_Stream);
var
  dbEng: TObjectDataManager;
begin
  dbEng := TObjectDataManager.CreateAsStream($FF, DBStream, '', ObjectDataMarshal.ID, False, True, False);
  BatchImportPathToDB(InitDir, Filter, dbEng);
  dbEng.UpdateIO;
  DisposeObject(dbEng);
  DBStream.Position := 0;
end;

function ExtractFileInDB(dbEng: TObjectDataManager; FileName: SystemString; ExtractToStream: TCore_Stream): Boolean;
begin
  result := ExtractFileInDB(dbEng, dbEng.RootField, FileName, ExtractToStream);
end;

function ExtractFileInDB(dbEng: TObjectDataManager; FieldPos: Int64; FileName: SystemString; ExtractToStream: TCore_Stream): Boolean;
var
  itmSrHnd: TItemSearch;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  result := False;

  LockObject(dbEng);
  try
    if dbEng.ItemFastFindFirst(FieldPos, FileName, itmSrHnd) then
      begin
        if dbEng.ItemFastOpen(itmSrHnd.HeaderPOS, itmHnd) then
          begin
            itmStream := TItemStream.Create(dbEng, itmHnd);

            if itmHnd.Name.Same(C_MD5_File) then
                ExtractToStream.CopyFrom(itmStream, itmStream.Size)
            else
                FP_DecryptStream(itmStream, ExtractToStream);

            DisposeObject(itmStream);
            result := True;
          end;
      end;
  finally
      UnLockObject(dbEng);
  end;
end;

function ExtractFileInDB(dbEng: TObjectDataManager; DBPath, FileName: SystemString; ExtractToStream: TCore_Stream): Boolean;
var
  FieldPos: Int64;
begin
  result := dbEng.GetPathField(DBPath, FieldPos);
  result := result and ExtractFileInDB(dbEng, FieldPos, FileName, ExtractToStream);
end;

function ExtractFileInDB(dbFileName, DBPath, FileName: SystemString; ExtractToStream: TCore_Stream): Boolean;
var
  dbEng: TObjectDataManager;
begin
  if not umlFileExists(dbFileName) then
      Exit(False);
  dbEng := ObjectDataMarshal.Open(dbFileName, True);

  result := ExtractFileInDB(dbEng, DBPath, FileName, ExtractToStream);

  ObjectDataMarshal.CloseDB(dbEng);
end;

function ExtractFileInDB(DBStream: TCore_Stream; DBPath, FileName: SystemString; ExtractToStream: TCore_Stream): Boolean;
var
  dbEng: TObjectDataManager;
begin
  DBStream.Position := 0;
  dbEng := TObjectDataManager.CreateAsStream(DBStream, '', ObjectDataMarshal.ID, True, False, False);

  result := ExtractFileInDB(dbEng, DBPath, FileName, ExtractToStream);

  DisposeObject(dbEng);
  DBStream.Position := 0;
end;

function ExistsFileInDB(DBStream: TCore_Stream; DBPath, FileName: SystemString): Boolean;
var
  dbEng: TObjectDataManager;
begin
  dbEng := TObjectDataManager.CreateAsStream(DBStream, '', ObjectDataMarshal.ID, True, False, False);

  result := dbEng.ItemExists(DBPath, FileName);

  DisposeObject(dbEng);
end;

function ExistsFileInDB(dbFileName, DBPath, FileName: SystemString): Boolean;
var
  dbEng: TObjectDataManager;
begin
  if not umlFileExists(dbFileName) then
      Exit(False);
  dbEng := ObjectDataMarshal.Open(dbFileName, True);

  result := dbEng.ItemExists(DBPath, FileName);

  ObjectDataMarshal.CloseDB(dbEng);
end;

function ExistsFileInDB(dbEng: TObjectDataManager; DBPath, FileName: SystemString): Boolean;
begin
  LockObject(dbEng);
  try
      result := dbEng.ItemExists(DBPath, FileName);
  finally
      UnLockObject(dbEng);
  end;
end;

function ExistsFileInDB(dbEng: TObjectDataManager; FileName: SystemString): Boolean;
begin
  LockObject(dbEng);
  try
      result := dbEng.ItemFastExists(dbEng.RootField, FileName);
  finally
      UnLockObject(dbEng);
  end;
end;

procedure ExtractDBToPath(dbEng: TObjectDataManager; ExtractToDir: SystemString; OutputFileList: TCore_Strings);
  procedure ExportTo(AField: Int64; ToDir: SystemString);
  var
    itmSrHnd: TItemSearch;
    FieldSrHnd: TFieldSearch;
    fn: SystemString;
    fs: TCore_FileStream;

    itmHnd: TItemHandle;
    itmStream: TItemStream;
  begin
    umlCreateDirectory(ToDir);

    if dbEng.ItemFastFindFirst(AField, '*', itmSrHnd) then
      begin
        repeat
          try
            if not itmSrHnd.Name.Same(C_MD5_File) then
              if dbEng.ItemFastOpen(itmSrHnd.HeaderPOS, itmHnd) then
                begin
                  fn := umlCombineFileName(ToDir, itmSrHnd.Name).Text;
                  fs := TCore_FileStream.Create(fn, fmCreate);
                  itmStream := TItemStream.Create(dbEng, itmHnd);

                  FP_DecryptStream(itmStream, fs);

                  DisposeObject(fs);
                  DisposeObject(itmStream);
                  if OutputFileList <> nil then
                      OutputFileList.Add(fn);
                end;
          except
          end;
        until not dbEng.ItemFastFindNext(itmSrHnd);
      end;

    if dbEng.FieldFastFindFirst(AField, '*', FieldSrHnd) then
      begin
        repeat
            ExportTo(FieldSrHnd.HeaderPOS, umlCombineFileName(ToDir, FieldSrHnd.Name).Text);
        until not dbEng.FieldFastFindNext(FieldSrHnd);
      end;
  end;

begin
  LockObject(dbEng);
  try
    if dbEng <> nil then
        ExportTo(dbEng.RootField, ExtractToDir);
  finally
      UnLockObject(dbEng);
  end;
end;

procedure ExtractDBToPath(dbEng: TObjectDataManager; ExtractToDir: SystemString);
begin
  ExtractDBToPath(dbEng, ExtractToDir, nil);
end;

procedure ExtractDBFileToPath(dbFile, ExtractToDir: SystemString);
var
  dbEng: TObjectDataManager;
begin
  if not umlFileExists(dbFile) then
      Exit;
  dbEng := ObjectDataMarshal.Open(dbFile, True);
  ExtractDBToPath(dbEng, ExtractToDir);
  ObjectDataMarshal.CloseDB(dbEng);
end;

function TestFileInDB(dbEng: TObjectDataManager): Integer;

var
  MD5Success, MD5Failed: Integer;

  procedure VerifyField(AField: Int64);
  var
    Found_MD5File: Boolean;
    itmSrHnd: TItemSearch;
    FieldSrHnd: TFieldSearch;
    itmHnd: TItemHandle;
    itmStream: TItemStream;
    ms: TMS64;

    md5: TPascalString;
    md5List: THashStringList;
    hashTextStream: THashStringTextStream;
  begin
    md5List := THashStringList.Create;

    hashTextStream := THashStringTextStream.Create(md5List);
    Found_MD5File := dbEng.ItemFastFindLast(AField, C_MD5_File, itmSrHnd);
    if Found_MD5File then
      begin
        if dbEng.ItemFastOpen(itmSrHnd.HeaderPOS, itmHnd) then
          begin
            itmStream := TItemStream.Create(dbEng, itmHnd);
            hashTextStream.LoadFromStream(itmStream);
            DisposeObject(itmStream);
          end;
      end;
    DisposeObject(hashTextStream);

    if dbEng.ItemFastFindFirst(AField, '*', itmSrHnd) then
      begin
        repeat
          try
            if not itmSrHnd.Name.Same(C_MD5_File) then
              if dbEng.ItemFastOpen(itmSrHnd.HeaderPOS, itmHnd) then
                begin
                  itmStream := TItemStream.Create(dbEng, itmHnd);

                  ms := TMS64.Create;
                  FP_DecryptStream(itmStream, ms);
                  ms.Position := 0;
                  md5 := umlStreamMD5String(ms);
                  if (md5List.Exists(md5)) then
                    begin
                      inc(MD5Success);
                    end
                  else
                    begin
                      DoStatus('not md5 Verify "%s"', [itmStream.Hnd^.Name.Text]);
                    end;
                  DisposeObject(ms);

                  DisposeObject(itmStream);
                end;
          except
          end;
        until not dbEng.ItemFastFindNext(itmSrHnd);
      end;

    if dbEng.FieldFastFindFirst(AField, '*', FieldSrHnd) then
      begin
        repeat
            VerifyField(FieldSrHnd.HeaderPOS);
        until not dbEng.FieldFastFindNext(FieldSrHnd);
      end;

    DisposeObject(md5List);
  end;

begin
  result := -1;
  if dbEng <> nil then
    begin
      LockObject(dbEng);
      try
        MD5Success := 0;
        MD5Failed := 0;
        VerifyField(dbEng.RootField);
        result := MD5Success;
        if MD5Failed > 0 then
            result := -MD5Failed;
      finally
          UnLockObject(dbEng);
      end;
    end;
end;

function TestFileInDB(DBStream: TCore_Stream): Integer;
var
  dbEng: TObjectDataManager;
begin
  result := -1;
  DBStream.Position := 0;
  dbEng := TObjectDataManager.CreateAsStream(DBStream, '', ObjectDataMarshal.ID, True, False, False);
  if not dbEng.isAbort then
      result := TestFileInDB(dbEng);
  DisposeObject(dbEng);
end;

function TestFileInDB(dbFile: SystemString): Integer;
var
  dbEng: TObjectDataManager;
begin
  result := -1;
  if not umlFileExists(dbFile) then
      Exit;
  dbEng := ObjectDataMarshal.Open(dbFile, True);
  result := TestFileInDB(dbEng);
  ObjectDataMarshal.CloseDB(dbEng);
end;

initialization

C_MD5_File := '____md5.txt';
FP_EncryptStream := DoEncryptStream_;
FP_DecryptStream := DoDecryptStream_;

end.
 
