(*
https://zpascal.net
https://github.com/PassByYou888/ZNet
https://github.com/PassByYou888/zRasterization
https://github.com/PassByYou888/ZSnappy
https://github.com/PassByYou888/Z-AI1.4
https://github.com/PassByYou888/ZAI_1.41
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
{ * Media Center                                                               * }
{ ****************************************************************************** }
unit Z.MediaCenter;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses Classes, Types,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings,
  Z.ZDB.HashField_LIB, Z.UnicodeMixedLib,
  Z.ZDB, Z.MemoryStream, Z.TextDataEngine, Z.ListEngine, Z.ZDB.HashItem_LIB,
  Z.DFE, Z.ZDB.ObjectData_LIB, Z.ZDB.ItemStream_LIB,
  Z.ZDB2, Z.ZDB2.FileEncoder,
  Z.Geometry2D;

type
  PSearchConfigInfo = ^TSearchConfigInfo;

  TSearchConfigInfo = record
    Recursion: Boolean;
    Intf: TCore_Object;
    Info: SystemString;
    Alias: THashStringList;
  end;

  TSearchConfigInfo_List_Decl = TGenericsList<PSearchConfigInfo>;

  TFileIO = class(TCore_Object_Intermediate)
  private
    FCritical: TCritical;
    FList: TSearchConfigInfo_List_Decl;

    function GetSearchItems(index: Integer): PSearchConfigInfo;
    procedure SetSearchItems(index: Integer; const Value: PSearchConfigInfo);

    function SearchObjData(Root_, Name_: SystemString; Recursion: Boolean; Intf_: TObjectDataManager; var Hnd: TCore_Stream): Boolean;
    function SearchLib(LibName, ItmName: SystemString; Intf_: TObjectDataHashField; var Hnd: TCore_Stream): Boolean;
    function SearchStreamList(Name_: SystemString; Intf_: TObjectDataHashItem; var Hnd: TCore_Stream): Boolean;
    function Search_ZDB2_File_Decoder(Name_: SystemString; Intf_: TZDB2_File_Decoder; var Hnd: TCore_Stream): Boolean;

    function ExistsObjData(Root_, Name_: SystemString; Recursion: Boolean; Intf_: TObjectDataManager): Boolean;
    function ExistsLib(LibName, ItmName: SystemString; Intf_: TObjectDataHashField): Boolean;
    function ExistsStreamList(Name_: SystemString; Intf_: TObjectDataHashItem): Boolean;
    function Exists_ZDB2_File_Decoder(Name_: SystemString; Intf_: TZDB2_File_Decoder): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function FileIOStream(FileName: SystemString; Mode: Word): TCore_Stream;
    function FileIOStreamExists(FileName: SystemString): Boolean;
    property SearchItems[index: Integer]: PSearchConfigInfo read GetSearchItems write SetSearchItems; default;
    procedure Clear;
    function SearchCount: Integer;
    procedure AddPrioritySearchObj(Recursion: Boolean; Intf_: TCore_Object; Info: SystemString);
    procedure AddSearchObj(Recursion: Boolean; Intf_: TCore_Object; Info: SystemString);
    procedure DeleteSearchObj(Value: TCore_Object);
    procedure DeleteSearchIndex(Value: Integer);
    function ChangePrioritySearchOption(Intf_: TCore_Object; Recursion: Boolean; Info: SystemString): Boolean;
    function SetSearchObjAlias(Intf_: TCore_Object; SourFileName, DestFileName: SystemString): Boolean;
    function SetSearchObjAliasFromList(Intf_: TCore_Object; Alias: THashStringList): Boolean;
  end;

var
  FileIO: TFileIO = nil;

  // resource: sound.ox or "sound"
  SoundLibrary: TObjectDataHashField = nil; // ZDB1.0
  // resource: art.ox or "art"
  ArtLibrary: TObjectDataHashField = nil; // ZDB1.0
  // resource: tile.ox or "tile"
  TileLibrary: TObjectDataHashField = nil; // ZDB1.0
  // resource: brush.ox or "brush"
  BrushLibrary: TObjectDataHashField = nil; // ZDB1.0
  // resource: fonts.ox or "fonts"
  FontsLibrary: TObjectDataHashField = nil; // ZDB1.0
  // resource: ai.ox or "ai"
  AILibrary: TObjectDataHashField = nil; // ZDB1.0
  // resource: model.ox or "model"
  ModelLibrary: TObjectDataHashField = nil; // ZDB1.0
  // resource: geometry.ox or "geometry"
  GeometryLibrary: TObjectDataHashField = nil; // ZDB1.0
  // resource: dict.ox or "dict"
  DictLibrary: TObjectDataHashField = nil; // ZDB1.0
  // resource: user.ox or "user"
  UserLibrary: TObjectDataHashField = nil; // ZDB1.0
  // resource: user.OX2 or "user"
  ZDB2_User_Library: TZDB2_File_Decoder = nil; // ZDB2.0

function FileIOCreate(const FileName: SystemString): TCore_Stream;
function FileIOOpen(const FileName: SystemString): TCore_Stream;
function FileIOExists(const FileName: SystemString): Boolean;
function GetResourceStream(const FileName: SystemString): TStream;

type
  TGlobalMediaType = (gmtSound, gmtArt, gmtTile, gmtBrush, gmtFonts, gmtAI, gmtModel, gmtGeometry, gmtDict, gmtUser, gmt_ZDB2_User);
  TGlobalMediaTypes = set of TGlobalMediaType;

const
  AllGlobalMediaTypes: TGlobalMediaTypes = ([gmtSound, gmtArt, gmtTile, gmtBrush, gmtFonts, gmtAI, gmtModel, gmtGeometry, gmtDict, gmtUser, gmt_ZDB2_User]);

procedure InitGlobalMedia(t: TGlobalMediaTypes);
procedure FreeGlobalMedia;

implementation

uses
{$IFDEF DELPHI}
  IOUtils,
{$ENDIF DELPHI}
  SysUtils;

function FileIOCreate(const FileName: SystemString): TCore_Stream;
begin
  FileIO.FCritical.Acquire;
  try
      Result := FileIO.FileIOStream(FileName, fmCreate);
  finally
      FileIO.FCritical.Release;
  end;
end;

function FileIOOpen(const FileName: SystemString): TCore_Stream;
begin
  FileIO.FCritical.Acquire;
  try
      Result := FileIO.FileIOStream(FileName, fmOpenRead);
  finally
      FileIO.FCritical.Release;
  end;
end;

function FileIOExists(const FileName: SystemString): Boolean;
begin
  FileIO.FCritical.Acquire;
  try
      Result := FileIO.FileIOStreamExists(FileName);
  finally
      FileIO.FCritical.Release;
  end;
end;

function TFileIO.GetSearchItems(index: Integer): PSearchConfigInfo;
begin
  Result := FList[index];
end;

procedure TFileIO.SetSearchItems(index: Integer; const Value: PSearchConfigInfo);
begin
  FList[index] := Value;
end;

function TFileIO.SearchObjData(Root_, Name_: SystemString; Recursion: Boolean; Intf_: TObjectDataManager; var Hnd: TCore_Stream): Boolean;
var
  ItemHnd: TItemHandle;
  ItmSearchHnd: TItemSearch;
  ItmRecursionHnd: TItemRecursionSearch;
begin
  Hnd := nil;
  Result := False;
  if Recursion then
    begin
      if Intf_.RecursionSearchFirst(Root_, Name_, ItmRecursionHnd) then
        begin
          repeat
            case ItmRecursionHnd.ReturnHeader.ID of
              db_Header_Item_ID:
                begin
                  if Intf_.ItemFastOpen(ItmRecursionHnd.ReturnHeader.CurrentHeader, ItemHnd) then
                    begin
                      Hnd := TItemStream.Create(Intf_, ItemHnd);
                      Result := True;
                      Exit;
                    end;
                end;
            end;
          until not Intf_.RecursionSearchNext(ItmRecursionHnd);
        end;
    end
  else if Intf_.ItemFindFirst(Root_, Name_, ItmSearchHnd) then
    begin
      if Intf_.ItemFastOpen(ItmSearchHnd.HeaderPOS, ItemHnd) then
        begin
          Hnd := TItemStream.Create(Intf_, ItemHnd);
          Result := True;
          Exit;
        end;
    end;
end;

function TFileIO.SearchLib(LibName, ItmName: SystemString; Intf_: TObjectDataHashField; var Hnd: TCore_Stream): Boolean;
var
  n: SystemString;
  p: PHashItemData;
begin
  Hnd := nil;
  Result := False;
  if LibName = '' then
      n := ItmName
  else
      n := LibName + ':' + ItmName;
  p := Intf_.PathItems[n];
  if p <> nil then
    begin
      Hnd := TItemStream.Create(Intf_.DBEngine, p^.ItemHnd);
      Result := True;
    end;
end;

function TFileIO.SearchStreamList(Name_: SystemString; Intf_: TObjectDataHashItem; var Hnd: TCore_Stream): Boolean;
var
  p: PHashItemData;
begin
  Hnd := nil;
  Result := False;
  p := Intf_.Names[Name_];
  if p <> nil then
    begin
      Hnd := TItemStream.Create(Intf_.DBEngine, p^.ItemHnd);
      Result := True;
    end;
end;

function TFileIO.Search_ZDB2_File_Decoder(Name_: SystemString; Intf_: TZDB2_File_Decoder; var Hnd: TCore_Stream): Boolean;
var
  fi: TZDB2_FI;
begin
  Result := False;
  fi := Intf_.Files.FindFile(Name_);
  if fi = nil then
      Exit;
  Hnd := TMS64.Create;
  if Intf_.DecodeToStream(fi, Hnd) then
    begin
      Hnd.Position := 0;
    end
  else
    begin
      DisposeObject(Hnd);
      Hnd := nil;
    end;
end;

function TFileIO.ExistsObjData(Root_, Name_: SystemString; Recursion: Boolean; Intf_: TObjectDataManager): Boolean;
var
  ItmSearchHnd: TItemSearch;
  ItmRecursionHnd: TItemRecursionSearch;
begin
  Result := False;
  if Recursion then
    begin
      if Intf_.RecursionSearchFirst(Root_, Name_, ItmRecursionHnd) then
        begin
          repeat
            case ItmRecursionHnd.ReturnHeader.ID of
              db_Header_Item_ID:
                begin
                  Result := True;
                  Exit;
                end;
            end;
          until not Intf_.RecursionSearchNext(ItmRecursionHnd);
        end;
    end
  else
      Result := Intf_.ItemFindFirst(Root_, Name_, ItmSearchHnd);
end;

function TFileIO.ExistsLib(LibName, ItmName: SystemString; Intf_: TObjectDataHashField): Boolean;
var
  n: SystemString;
begin
  if LibName = '' then
      n := ItmName
  else
      n := LibName + ':' + ItmName;
  Result := Intf_.PathItems[n] <> nil;
end;

function TFileIO.ExistsStreamList(Name_: SystemString; Intf_: TObjectDataHashItem): Boolean;
begin
  Result := Intf_.Names[Name_] <> nil;
end;

function TFileIO.Exists_ZDB2_File_Decoder(Name_: SystemString; Intf_: TZDB2_File_Decoder): Boolean;
begin
  Result := Intf_.Files.FindFile(Name_) <> nil;
end;

constructor TFileIO.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FList := TSearchConfigInfo_List_Decl.Create;
{$IFDEF DELPHI}
  AddSearchObj(True, nil, TPath.GetLibraryPath);
{$ELSE DELPHI}
  AddSearchObj(True, nil, umlGetCurrentPath);
{$ENDIF DELPHI}
end;

destructor TFileIO.Destroy;
begin
  while SearchCount > 0 do
      DeleteSearchIndex(0);

  DisposeObject(FList);
  DisposeObject(FCritical);
  inherited Destroy;
end;

function TFileIO.FileIOStream(FileName: SystemString; Mode: Word): TCore_Stream;
var
  i: Integer;
  p: PSearchConfigInfo;
  n: SystemString;
begin
  FileName := umlTrimSpace(FileName);
  if FileName = '' then
    begin
      Result := nil;
      Exit;
    end;

  if (Mode = fmCreate) then
    begin
      Result := TCore_FileStream.Create(FileName, fmCreate);
      Exit;
    end;

  if umlMultipleMatch('?:\*', FileName) then
    if umlFileExists(FileName) then
      begin
        Result := TCore_FileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
        Exit;
      end;

  n := umlGetFileName(FileName);
  Result := nil;

  if (FList.Count > 0) then
    begin
      for i := 0 to FList.Count - 1 do
        begin
          p := PSearchConfigInfo(FList[i]);
          if p^.Intf is TObjectDataManager then
            begin
              if (p^.Alias <> nil) and (p^.Alias.Exists(n)) and
                (SearchObjData(p^.Info, p^.Alias[n], p^.Recursion, p^.Intf as TObjectDataManager, Result)) then
                  Exit;

              if SearchObjData(p^.Info, n, p^.Recursion, p^.Intf as TObjectDataManager, Result) then
                  Exit;
            end
          else if p^.Intf is TObjectDataHashField then
            begin
              if (p^.Alias <> nil) and (p^.Alias.Exists(n)) and
                (SearchLib(p^.Info, p^.Alias[n], p^.Intf as TObjectDataHashField, Result)) then
                  Exit;

              if SearchLib(p^.Info, n, p^.Intf as TObjectDataHashField, Result) then
                  Exit;
            end
          else if p^.Intf is TObjectDataHashItem then
            begin
              if (p^.Alias <> nil) and (p^.Alias.Exists(n)) and
                (SearchStreamList(p^.Alias[n], p^.Intf as TObjectDataHashItem, Result)) then
                  Exit;

              if SearchStreamList(n, p^.Intf as TObjectDataHashItem, Result) then
                  Exit;
            end
          else if p^.Intf is TZDB2_File_Decoder then
            begin
              if (p^.Alias <> nil) and (p^.Alias.Exists(n)) and
                (Search_ZDB2_File_Decoder(p^.Alias[n], p^.Intf as TZDB2_File_Decoder, Result)) then
                  Exit;

              if Search_ZDB2_File_Decoder(n, p^.Intf as TZDB2_File_Decoder, Result) then
                  Exit;
            end
          else if p^.Intf = nil then
            begin
              if (p^.Alias <> nil) and (p^.Alias.Exists(n)) and (umlFileExists(umlCombineFileName(p^.Info, p^.Alias[n])))
              then
                begin
                  try
                    Result := TMS64.Create;
                    TMS64(Result).LoadFromFile(umlCombineFileName(p^.Info, p^.Alias[n]));
                    Result.Position := 0;
                    Exit;
                  except
                  end;
                end
              else if umlFileExists(umlCombineFileName(p^.Info, n)) then
                begin
                  try
                    Result := TMS64.Create;
                    TMS64(Result).LoadFromFile(umlCombineFileName(p^.Info, n));
                    Result.Position := 0;
                    Exit;
                  except
                  end;
                end;
            end;
        end;
    end;

  if umlFileExists(FileName) then
    begin
      Result := TCore_FileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
      Exit;
    end;

{$IFDEF DELPHI}
  n := umlGetFileName(FileName);
  n := umlCombineFileName(TPath.GetLibraryPath, n);
  if umlFileExists(n) then
    begin
      Result := TCore_FileStream.Create(n, fmOpenRead or fmShareDenyNone);
      Exit;
    end;

  n := umlGetFileName(FileName);
  n := umlCombineFileName(TPath.GetDocumentsPath, n);
  if umlFileExists(n) then
    begin
      Result := TCore_FileStream.Create(n, fmOpenRead or fmShareDenyNone);
      Exit;
    end;

  n := umlGetFileName(FileName);
  n := umlCombineFileName(TPath.GetDownloadsPath, n);
  if umlFileExists(n) then
    begin
      Result := TCore_FileStream.Create(n, fmOpenRead or fmShareDenyNone);
      Exit;
    end;

  n := umlGetFileName(FileName);
  n := umlCombineFileName(TPath.GetSharedDownloadsPath, n);
  if umlFileExists(n) then
    begin
      Result := TCore_FileStream.Create(n, fmOpenRead or fmShareDenyNone);
      Exit;
    end;
{$ENDIF DELPHI}
  Result := nil;
end;

function TFileIO.FileIOStreamExists(FileName: SystemString): Boolean;
var
  i: Integer;
  p: PSearchConfigInfo;
  n: SystemString;
begin
  FileName := umlTrimSpace(FileName);
  if FileName = '' then
    begin
      Result := False;
      Exit;
    end;

  if umlMultipleMatch('?:\*', FileName) then
    if umlFileExists(FileName) then
      begin
        Result := True;
        Exit;
      end;

  n := umlGetFileName(FileName);
  Result := False;
  try
    if FList.Count > 0 then
      begin
        for i := 0 to FList.Count - 1 do
          begin
            p := FList[i];
            if p^.Intf is TObjectDataManager then
              begin
                Result := ((p^.Alias <> nil) and (p^.Alias.Exists(n)) and
                    (ExistsObjData(p^.Info, p^.Alias[n], p^.Recursion, p^.Intf as TObjectDataManager))) or
                  (ExistsObjData(p^.Info, n, p^.Recursion, p^.Intf as TObjectDataManager));
              end
            else if p^.Intf is TObjectDataHashField then
              begin
                Result := ((p^.Alias <> nil) and (p^.Alias.Exists(n)) and
                    (ExistsLib(p^.Info, p^.Alias[n], p^.Intf as TObjectDataHashField))) or (ExistsLib(p^.Info, n, p^.Intf as TObjectDataHashField));
              end
            else if p^.Intf is TObjectDataHashItem then
              begin
                Result := ((p^.Alias <> nil) and (p^.Alias.Exists(n)) and
                    (ExistsStreamList(p^.Alias[n], p^.Intf as TObjectDataHashItem))) or (ExistsStreamList(n, p^.Intf as TObjectDataHashItem));
              end
            else if p^.Intf is TZDB2_File_Decoder then
              begin
                Result := ((p^.Alias <> nil) and (p^.Alias.Exists(n)) and
                    (Exists_ZDB2_File_Decoder(p^.Alias[n], p^.Intf as TZDB2_File_Decoder))) or (Exists_ZDB2_File_Decoder(n, p^.Intf as TZDB2_File_Decoder));
              end
            else if p^.Intf = nil then
              begin
                Result := ((p^.Alias <> nil) and (p^.Alias.Exists(n)) and
                    (umlFileExists(umlCombineFileName(p^.Info, p^.Alias[n])))) or (umlFileExists(umlCombineFileName(p^.Info, n)));
              end;
            if Result then
                Exit;
          end;
      end;

    if umlFileExists(FileName) then
      begin
        Result := True;
        Exit;
      end;

{$IFDEF DELPHI}
    n := umlGetFileName(FileName);
    n := umlCombineFileName(TPath.GetLibraryPath, n);
    if umlFileExists(n) then
      begin
        Result := True;
        Exit;
      end;

    n := umlGetFileName(FileName);
    n := umlCombineFileName(TPath.GetDocumentsPath, n);
    if umlFileExists(n) then
      begin
        Result := True;
        Exit;
      end;

    n := umlGetFileName(FileName);
    n := umlCombineFileName(TPath.GetDownloadsPath, n);
    if umlFileExists(n) then
      begin
        Result := True;
        Exit;
      end;

    n := umlGetFileName(FileName);
    n := umlCombineFileName(TPath.GetSharedDownloadsPath, n);
    if umlFileExists(n) then
      begin
        Result := True;
        Exit;
      end;
{$ENDIF DELPHI}
  except
  end;
end;

procedure TFileIO.Clear;
begin
  while SearchCount > 0 do
      DeleteSearchIndex(0);
end;

function TFileIO.SearchCount: Integer;
begin
  Result := FList.Count;
end;

procedure TFileIO.AddPrioritySearchObj(Recursion: Boolean; Intf_: TCore_Object; Info: SystemString);
var
  p: PSearchConfigInfo;
begin
  new(p);
  p^.Recursion := Recursion;
  p^.Intf := Intf_;
  p^.Info := Info;
  p^.Alias := nil;
  if FList.Count > 0 then
    begin
      FList.Insert(0, p);
    end
  else
    begin
      FList.Add(p);
    end;
end;

procedure TFileIO.AddSearchObj(Recursion: Boolean; Intf_: TCore_Object; Info: SystemString);
var
  p: PSearchConfigInfo;
begin
  new(p);
  p^.Recursion := Recursion;
  p^.Intf := Intf_;
  p^.Info := Info;
  p^.Alias := nil;
  FList.Add(p);
end;

procedure TFileIO.DeleteSearchObj(Value: TCore_Object);
var
  i: Integer;
  p: PSearchConfigInfo;
begin
  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if p^.Intf = Value then
        begin
          FList.Delete(i);
          if p^.Alias <> nil then
            begin
              try
                  DisposeObject(p^.Alias);
              except
              end;
            end;
          Dispose(p);
          Break;
        end
      else
          inc(i);
    end;
end;

procedure TFileIO.DeleteSearchIndex(Value: Integer);
var
  p: PSearchConfigInfo;
begin
  p := FList[Value];
  if p^.Alias <> nil then
    begin
      try
          DisposeObject(p^.Alias);
      except
      end;
    end;
  Dispose(p);
  FList.Delete(Value);
end;

function TFileIO.ChangePrioritySearchOption(Intf_: TCore_Object; Recursion: Boolean; Info: SystemString): Boolean;
var
  i: Integer;
  p: PSearchConfigInfo;
begin
  Result := False;
  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if p^.Intf = Intf_ then
        begin
          p^.Recursion := Recursion;
          p^.Info := Info;
          if i > 0 then
              FList.Move(i, 0);
          Result := True;
        end;
      inc(i);
    end;
  if not Result then
      AddPrioritySearchObj(Recursion, Intf_, Info);
  Result := True;
end;

function TFileIO.SetSearchObjAlias(Intf_: TCore_Object; SourFileName, DestFileName: SystemString): Boolean;
var
  i: Integer;
  p: PSearchConfigInfo;
begin
  Result := False;
  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if p^.Intf = Intf_ then
        begin
          if p^.Alias = nil then
              p^.Alias := THashStringList.Create;

          p^.Alias[SourFileName] := DestFileName;
          Result := True;
        end;
      inc(i);
    end;
end;

function TFileIO.SetSearchObjAliasFromList(Intf_: TCore_Object; Alias: THashStringList): Boolean;
var
  i: Integer;
  p: PSearchConfigInfo;
begin
  Result := False;
  if Alias = nil then
      Exit;
  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if p^.Intf = Intf_ then
        begin
          if p^.Alias = nil then
              p^.Alias := THashStringList.Create;
          p^.Alias.CopyFrom(Alias);
          Result := True;
        end;
      inc(i);
    end;
end;

function GetResourceStream(const FileName: SystemString): TStream;
var
  n: SystemString;
begin
  Result := nil;

  if TPascalString(FileName).Exists('.') then
      n := umlDeleteLastStr(FileName, '.')
  else
      n := FileName;

  if FindResource(HInstance, PChar(n), PChar(10)) = 0 then
    begin
{$IFDEF FPC}
{$ELSE FPC}
      n := umlGetFileName(FileName);
      n := umlCombineFileName(TPath.GetLibraryPath, n);
      if umlFileExists(n) then
        begin
          Result := TCore_FileStream.Create(n, fmOpenRead or fmShareDenyNone);
          Exit;
        end;

      n := umlGetFileName(FileName);
      n := umlCombineFileName(TPath.GetDocumentsPath, n);
      if umlFileExists(n) then
        begin
          Result := TCore_FileStream.Create(n, fmOpenRead or fmShareDenyNone);
          Exit;
        end;

      n := umlGetFileName(FileName);
      n := umlCombineFileName(TPath.GetDownloadsPath, n);
      if umlFileExists(n) then
        begin
          Result := TCore_FileStream.Create(n, fmOpenRead or fmShareDenyNone);
          Exit;
        end;

      n := umlGetFileName(FileName);
      n := umlCombineFileName(TPath.GetSharedDownloadsPath, n);
      if umlFileExists(n) then
        begin
          Result := TCore_FileStream.Create(n, fmOpenRead or fmShareDenyNone);
          Exit;
        end;
{$ENDIF FPC}
      n := umlGetFileName(FileName);
      if FileIOExists(n) then
        begin
          Result := FileIOOpen(n);
          Exit;
        end;

      RaiseInfo('no exists resource file "%s"', [n]);
    end
  else
    begin
      Result := TResourceStream.Create(HInstance, n, PChar(10));
    end;
end;

procedure InitGlobalMedia(t: TGlobalMediaTypes);
var
  db: TObjectDataManager;
begin
  FreeGlobalMedia;

  if gmtSound in t then
    begin
      db := TObjectDataManagerOfCache.CreateAsStream(GetResourceStream('sound.ox'), 'sound.ox', ObjectDataMarshal.ID, True, False, True);
      SoundLibrary := TObjectDataHashField.Create(db, '/');
      SoundLibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, SoundLibrary, '/');
    end;

  if gmtArt in t then
    begin
      db := TObjectDataManagerOfCache.CreateAsStream(GetResourceStream('art.ox'), 'art.ox', ObjectDataMarshal.ID, True, False, True);
      ArtLibrary := TObjectDataHashField.Create(db, '/');
      ArtLibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, ArtLibrary, '/');
    end;

  if gmtTile in t then
    begin
      db := TObjectDataManagerOfCache.CreateAsStream(GetResourceStream('tile.ox'), 'tile.ox', ObjectDataMarshal.ID, True, False, True);
      TileLibrary := TObjectDataHashField.Create(db, '/');
      TileLibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, TileLibrary, '/');
    end;

  if gmtBrush in t then
    begin
      db := TObjectDataManagerOfCache.CreateAsStream(GetResourceStream('brush.ox'), 'brush.ox', ObjectDataMarshal.ID, True, False, True);
      BrushLibrary := TObjectDataHashField.Create(db, '/');
      BrushLibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, BrushLibrary, '/');
    end;

  if gmtFonts in t then
    begin
      db := TObjectDataManagerOfCache.CreateAsStream(GetResourceStream('fonts.ox'), 'fonts.ox', ObjectDataMarshal.ID, True, False, True);
      FontsLibrary := TObjectDataHashField.Create(db, '/');
      FontsLibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, FontsLibrary, '/');
    end;

  if gmtAI in t then
    begin
      db := TObjectDataManagerOfCache.CreateAsStream(GetResourceStream('ai.ox'), 'ai.ox', ObjectDataMarshal.ID, True, False, True);
      AILibrary := TObjectDataHashField.Create(db, '/');
      AILibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, AILibrary, '/');
    end;

  if gmtModel in t then
    begin
      db := TObjectDataManagerOfCache.CreateAsStream(GetResourceStream('model.ox'), 'model.ox', ObjectDataMarshal.ID, True, False, True);
      ModelLibrary := TObjectDataHashField.Create(db, '/');
      ModelLibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, ModelLibrary, '/');
    end;

  if gmtGeometry in t then
    begin
      db := TObjectDataManagerOfCache.CreateAsStream(GetResourceStream('geometry.ox'), 'geometry.ox', ObjectDataMarshal.ID, True, False, True);
      GeometryLibrary := TObjectDataHashField.Create(db, '/');
      GeometryLibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, GeometryLibrary, '/');
    end;

  if gmtDict in t then
    begin
      db := TObjectDataManagerOfCache.CreateAsStream(GetResourceStream('dict.ox'), 'dict.ox', ObjectDataMarshal.ID, True, False, True);
      DictLibrary := TObjectDataHashField.Create(db, '/');
      DictLibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, DictLibrary, '/');
    end;

  if gmtUser in t then
    begin
      db := TObjectDataManagerOfCache.CreateAsStream(GetResourceStream('user.ox'), 'user.ox', ObjectDataMarshal.ID, True, False, True);
      UserLibrary := TObjectDataHashField.Create(db, '/');
      UserLibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, UserLibrary, '/');
    end;

  if gmt_ZDB2_User in t then
    begin
      ZDB2_User_Library := TZDB2_File_Decoder.Create(GetResourceStream('user.OX2'), 2);
      ZDB2_User_Library.Core.Space_IOHnd^.IsOnlyRead := True;
      ZDB2_User_Library.Core.AutoCloseIOHnd := True;
      ZDB2_User_Library.Core.AutoFreeIOHnd := True;
      FileIO.AddSearchObj(True, ZDB2_User_Library, '');
    end;
end;

procedure FreeGlobalMedia;
begin
  if SoundLibrary <> nil then
    begin
      FileIO.DeleteSearchObj(SoundLibrary);
      DisposeObject(SoundLibrary);
      SoundLibrary := nil;
    end;

  if ArtLibrary <> nil then
    begin
      FileIO.DeleteSearchObj(ArtLibrary);
      DisposeObject(ArtLibrary);
      ArtLibrary := nil;
    end;

  if TileLibrary <> nil then
    begin
      FileIO.DeleteSearchObj(TileLibrary);
      DisposeObject(TileLibrary);
      TileLibrary := nil;
    end;

  if BrushLibrary <> nil then
    begin
      FileIO.DeleteSearchObj(BrushLibrary);
      DisposeObject(BrushLibrary);
      BrushLibrary := nil;
    end;

  if FontsLibrary <> nil then
    begin
      FileIO.DeleteSearchObj(FontsLibrary);
      DisposeObject(FontsLibrary);
      FontsLibrary := nil;
    end;

  if UserLibrary <> nil then
    begin
      FileIO.DeleteSearchObj(UserLibrary);
      DisposeObject(UserLibrary);
      UserLibrary := nil;
    end;

  if ZDB2_User_Library <> nil then
    begin
      FileIO.DeleteSearchObj(ZDB2_User_Library);
      DisposeObject(ZDB2_User_Library);
      ZDB2_User_Library := nil;
    end;

  if AILibrary <> nil then
    begin
      FileIO.DeleteSearchObj(AILibrary);
      DisposeObject(AILibrary);
      AILibrary := nil;
    end;

  if ModelLibrary <> nil then
    begin
      FileIO.DeleteSearchObj(ModelLibrary);
      DisposeObject(ModelLibrary);
      ModelLibrary := nil;
    end;

  if GeometryLibrary <> nil then
    begin
      FileIO.DeleteSearchObj(GeometryLibrary);
      DisposeObject(GeometryLibrary);
      GeometryLibrary := nil;
    end;

  if DictLibrary <> nil then
    begin
      FileIO.DeleteSearchObj(DictLibrary);
      DisposeObject(DictLibrary);
      DictLibrary := nil;
    end;
end;

initialization

FileIO := TFileIO.Create;

SoundLibrary := nil;
ArtLibrary := nil;
TileLibrary := nil;
BrushLibrary := nil;
FontsLibrary := nil;
UserLibrary := nil;
AILibrary := nil;
ModelLibrary := nil;
GeometryLibrary := nil;
DictLibrary := nil;
ZDB2_User_Library := nil;

finalization


FreeGlobalMedia;
if FileIO <> nil then
  begin
    DisposeObject(FileIO);
    FileIO := nil;
  end;

end.
 
