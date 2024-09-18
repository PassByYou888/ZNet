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
{ * pascal code tool                                                           * }
{ ****************************************************************************** }
unit Z.Pascal_Code_Tool;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses
  SysUtils,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Parsing, Z.MemoryStream, Z.ListEngine,
  Z.HashList.Templet, Z.ZDB2, Z.ZDB2.FileEncoder, Z.Cipher,
  Z.ZDB.ObjectData_LIB, Z.ZDB, Z.ZDB.ItemStream_LIB;

{$REGION 'BaseDef'}


type
  TPascal_Keyword = (
    kUnit, kProgram, kLibrary, kInterface, kUses, kType, kSet, kClass, KArray, kOf, kFunction, kProcedure, kConst, kVar, kRecord, kPacked, kInline,
    kImplementation, kBegin, kEnd, kIn, kOut, kIf, kFor, kWhile, kRepeat, kUntil, kLabel, kContinue, kBreak, kCase,
    kForward, kEol,
    kInherited, kOverload, kVirtual, kOverride, kStdcall, kCdecl, kPascal, kSafeCall, kRegister,
    kConstructor, kDestructor, kPublic, kPrivate, kProtected, kPublished,
    kAbstract, kSealed, kWeak, kUnsafe,
    kInitialization, kFinalization,
    kEmpty, kUnknow
    );
  TPascal_Keywords = set of TPascal_Keyword;

  TPascal_Keyword_Define_Struct = record
    key: TPascal_Keyword;
    Decl: string;
  end;

  TPascal_Keyword_DICT = array [TPascal_Keyword] of TPascal_Keyword_Define_Struct;

  TSource_Processor_Data_Pool = class;

  TSource_Define = record
    SourceFile: U_String;
    NewName: U_String;
  end;

  PSource_Define = ^TSource_Define;

  TSource_Define_Pool_Decl = TGenericsList<PSource_Define>;

  TSource_Define_Pool = class(TSource_Define_Pool_Decl)
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFile(SourFile: U_String);
    procedure AddCustom(SourFile, NewName: U_String; Overwrite_: Boolean);
    function ReplaceNewName(OLD_, New_: U_String; OnlyWord, IgnoreCase: Boolean): Integer;
    procedure Clean;
    procedure SaveToStream(stream: TCore_Stream; Foramted_: Boolean);
    procedure LoadFromStream(stream: TCore_Stream);
    procedure SaveToFile(fn: U_String);
    procedure LoadFromFile(fn: U_String);
    procedure Build_Unit_Processor(Processor: TSource_Processor_Data_Pool);
  end;

  TSource_Processor_Data = record
    OLD_Feature, New_Feature: U_String;
  end;

  PSource_Processor_Data = ^TSource_Processor_Data;

  TSource_Processor_Data_Pool_Decl = TGenericsList<PSource_Processor_Data>;

  TSource_Processor_Data_Pool = class(TSource_Processor_Data_Pool_Decl)
  public
    constructor Create;
    destructor Destroy; override;
    function Exists_OLD_Feature(OLD_Feature: U_String): Boolean;
    function Remove_OLD_Feature(OLD_Feature: U_String): Integer;
    procedure Add_Feature(OLD_Feature, New_Feature: U_String);
    function Replace_OLD_Feature(OLD_, New_: U_String; OnlyWord, IgnoreCase: Boolean): Integer;
    function Replace_New_Feature(OLD_, New_: U_String; OnlyWord, IgnoreCase: Boolean): Integer;
    procedure Import(source: TSource_Processor_Data_Pool);
    procedure Clean;
    procedure SaveToStream(stream: TCore_Stream; Foramted_: Boolean);
    procedure LoadFromStream(stream: TCore_Stream);
    procedure SaveToFile(fn: U_String);
    procedure LoadFromFile(fn: U_String);
    procedure Build_Hash_Pool(Output: THashStringList);
  end;

  TCustom_After_Source_Processor_Data = record
    File_Match, OLD_Feature, New_Feature: U_String;
  end;

  PCustom_After_Source_Processor_Data = ^TCustom_After_Source_Processor_Data;

  TCustom_After_Source_Processor_Data_Pool_Decl = TGenericsList<PCustom_After_Source_Processor_Data>;

  TCustom_After_Source_Processor_Data_Pool = class(TCustom_After_Source_Processor_Data_Pool_Decl)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add_Feature(File_Match, OLD_Feature, New_Feature: U_String);
    function Replace_OLD_Feature(OLD_, New_: U_String; OnlyWord, IgnoreCase: Boolean): Integer;
    function Replace_New_Feature(OLD_, New_: U_String; OnlyWord, IgnoreCase: Boolean): Integer;
    procedure Clean;
    procedure SaveToStream(stream: TCore_Stream; Foramted_: Boolean);
    procedure LoadFromStream(stream: TCore_Stream);
    function Build_Hash_Pool(FileName: U_String; Output: THashStringList): Boolean;
  end;

{$IFDEF FPC}

  TOnRewriteStatus = procedure(const Fmt: SystemString; const Args: array of const) is nested;
{$ELSE FPC}
  TOnRewriteStatus = reference to procedure(const Fmt: SystemString; const Args: array of const);
{$ENDIF FPC}


const
  Pascal_Keyword_DICT: TPascal_Keyword_DICT = (
    (key: kUnit; Decl: 'Unit'),
    (key: kProgram; Decl: 'Program'),
    (key: kLibrary; Decl: 'Library'),
    (key: kInterface; Decl: 'Interface'),
    (key: kUses; Decl: 'Uses'),
    (key: kType; Decl: 'Type'),
    (key: kSet; Decl: 'Set'),
    (key: kClass; Decl: 'Class'),
    (key: KArray; Decl: 'Array'),
    (key: kOf; Decl: 'Of'),
    (key: kFunction; Decl: 'function'),
    (key: kProcedure; Decl: 'Procedure'),
    (key: kConst; Decl: 'Const'),
    (key: kVar; Decl: 'var'),
    (key: kRecord; Decl: 'record'),
    (key: kPacked; Decl: 'packed'),
    (key: kInline; Decl: 'inline'),

    (key: kImplementation; Decl: 'implementation'),
    (key: kBegin; Decl: 'begin'),
    (key: kEnd; Decl: 'end'),
    (key: kIn; Decl: 'in'),
    (key: kOut; Decl: 'out'),
    (key: kIf; Decl: 'if'),
    (key: kFor; Decl: 'for'),
    (key: kWhile; Decl: 'while'),
    (key: kRepeat; Decl: 'repeat'),
    (key: kUntil; Decl: 'until'),
    (key: kLabel; Decl: 'label'),
    (key: kContinue; Decl: 'Continue'),
    (key: kBreak; Decl: 'Break'),
    (key: kCase; Decl: 'case'),
    (key: kForward; Decl: 'Forward'),
    (key: kEol; Decl: 'end.'),

    (key: kInherited; Decl: 'Inherited'),
    (key: kOverload; Decl: 'Overload'),
    (key: kVirtual; Decl: 'Virtual'),
    (key: kOverride; Decl: 'Override'),
    (key: kStdcall; Decl: 'Stdcall'),
    (key: kCdecl; Decl: 'Cdecl'),
    (key: kPascal; Decl: 'Pascal'),
    (key: kSafeCall; Decl: 'SafeCall'),
    (key: kRegister; Decl: 'Register'),

    (key: kConstructor; Decl: 'Constructor'),
    (key: kDestructor; Decl: 'Destructor'),
    (key: kPublic; Decl: 'Public'),
    (key: kPrivate; Decl: 'Private'),
    (key: kProtected; Decl: 'Protected'),
    (key: kPublished; Decl: 'Published'),

    (key: kAbstract; Decl: 'Abstract'),
    (key: kSealed; Decl: 'Sealed'),
    (key: kWeak; Decl: 'Weak'),
    (key: kUnsafe; Decl: 'Unsafe'),

    (key: kInitialization; Decl: 'Initialization'),
    (key: kFinalization; Decl: 'Finalization'),

    (key: kEmpty; Decl: ''),
    (key: kUnknow; Decl: ''));
{$ENDREGION 'BaseDef'}

function Pascal_Keyword(const s: TP_String): TPascal_Keyword;

function Replace_Pascal_Code(var Code_: TP_String;
  PatternHash_: THashStringList; TT_: TTokenTypes; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; FileInfo__: SystemString; OnStatus: TOnRewriteStatus): Boolean;
function Replace_ASCII_Code(var Code_: TP_String; PatternHash_: THashStringList; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; FileInfo__: SystemString; OnStatus: TOnRewriteStatus): Boolean;

type
  TRewrite_Trace_Pool = class;

  TRewrite_Trace = class(TCore_Object_Intermediate)
  public
    Current_: U_String;
    marco_hash_: THashStringList;
    Include_Files_: TPascalStringList;
    Uses_Files_: TPascalStringList;
    Resource_Files_: TPascalStringList;
    Link_Files_: TPascalStringList;
    IsCode_: Boolean;
    constructor Create();
    destructor Destroy; override;
  end;

  TRewrite_Trace_Pool_Decl = TGenericsList<TRewrite_Trace>;

  TRewrite_Trace_Pool = class(TRewrite_Trace_Pool_Decl)
  public
    procedure Clean;
  end;

function RewritePascal_Process_Code(const Code_: TCore_Strings; UnitHash_, PatternHash_: THashStringList; Trace_: TRewrite_Trace; FileInfo__: SystemString; OnStatus: TOnRewriteStatus): Boolean; overload;
function RewritePascal_Process_Code(const Code_: TCore_Strings; UnitHash_, PatternHash_: THashStringList; FileInfo__: SystemString; OnStatus: TOnRewriteStatus): Boolean; overload;

// rewrite system file
function RewritePascal_ProcessFile(fn: U_String; UnitHash_, PatternHash_: THashStringList; Trace_: TRewrite_Trace; FileInfo__: SystemString; OnStatus: TOnRewriteStatus): Boolean;
procedure RewritePascal_Include_File_Processor(UnitHash_, PatternHash_: THashStringList; Trace_Pool: TRewrite_Trace_Pool; OnStatus: TOnRewriteStatus);
procedure RewritePascal_Custom_After_Processor(fn: U_String; Custom_PatternHash_: THashStringList; OnlyWord_: Boolean; FileInfo__: SystemString; OnStatus: TOnRewriteStatus);
function RewritePascal_ProcessDirectory(Parallel_: Boolean; directory_: U_String; UnitHash_, PatternHash_: THashStringList; CustomPattern_: TCustom_After_Source_Processor_Data_Pool; OnStatus: TOnRewriteStatus): Integer; overload;
function RewritePascal_ProcessDirectory(Parallel_: Boolean; directory_: U_String; Model_: TMS64; Reverse_: Boolean; OnStatus: TOnRewriteStatus): Integer; overload;

// rewrite ZDB file
type
  TThread_RewritePascal_Process_ZDB_File = class(TCore_Object_Intermediate)
  public
    // runtime param
    Busy: Boolean;
    th_Post: TThreadPost;
    Eng_: TObjectDataManager;
    fn: U_String;
    UnitHash_, PatternHash_: THashStringList;
    Trace_: TRewrite_Trace;
    OnStatus: TOnRewriteStatus;
    // internal
    Code: TCore_StringList;
    ft: TDateTime;
    ph, OLD_Feature, New_Feature, new_fn: U_String;
    m64: TMem64;
    LEncode: TEncoding;
    // return
    Result_RewritePascal_Process_ZDB_File: Boolean;

    class function Init_(th_Post_: TThreadPost; Eng__: TObjectDataManager; fn_: U_String; UnitHash__, PatternHash__: THashStringList;
      Trace__: TRewrite_Trace; OnStatus_: TOnRewriteStatus): TThread_RewritePascal_Process_ZDB_File;
    destructor Destroy; override;
    procedure Do_Sync();
    procedure Do_RewritePascal_Process_Code();
    procedure Do_Run;
  end;

  TThread_RewritePascal_Process_ZDB_File_Pool = TBigList<TThread_RewritePascal_Process_ZDB_File>;

procedure Th_RewritePascal_ZDB_Include_File_Processor(Eng_: TObjectDataManager; UnitHash_, PatternHash_: THashStringList; Trace_Pool: TRewrite_Trace_Pool; OnStatus: TOnRewriteStatus);
procedure Th_RewritePascal_Process_ZDB_Directory(Eng_: TObjectDataManager; directory_: U_String; UnitHash_, PatternHash_: THashStringList; CustomPattern_: TCustom_After_Source_Processor_Data_Pool; OnStatus: TOnRewriteStatus); overload;
procedure Th_RewritePascal_Process_ZDB_Directory(Eng_: TObjectDataManager; directory_: U_String; Model_: TMS64; Reverse_: Boolean; OnStatus: TOnRewriteStatus); overload;

function RewritePascal_Process_ZDB_File(Eng_: TObjectDataManager; fn: U_String;
  UnitHash_, PatternHash_: THashStringList; Trace_: TRewrite_Trace;
  FileInfo__: SystemString; OnStatus: TOnRewriteStatus): Boolean;
procedure RewritePascal_ZDB_Include_File_Processor(Eng_: TObjectDataManager; UnitHash_, PatternHash_: THashStringList; Trace_Pool: TRewrite_Trace_Pool; OnStatus: TOnRewriteStatus);
procedure RewritePascal_ZDB_Custom_After_Processor(Eng_: TObjectDataManager; fn: U_String; Custom_PatternHash_: THashStringList; OnlyWord_: Boolean; FileInfo__: SystemString; OnStatus: TOnRewriteStatus);
function RewritePascal_Process_ZDB_Directory(Eng_: TObjectDataManager; directory_: U_String; UnitHash_, PatternHash_: THashStringList; CustomPattern_: TCustom_After_Source_Processor_Data_Pool; OnStatus: TOnRewriteStatus): Integer; overload;
function RewritePascal_Process_ZDB_Directory(Eng_: TObjectDataManager; directory_: U_String; Model_: TMS64; Reverse_: Boolean; OnStatus: TOnRewriteStatus): Integer; overload;

// model
function Load_RewritePascal_Model(Model_: TMS64; UnitData_, PatternData_: TSource_Processor_Data_Pool; CustomPattern_: TCustom_After_Source_Processor_Data_Pool): Boolean;
function Build_RewritePascal_Model(UnitData_, PatternData_: TSource_Processor_Data_Pool; CustomPattern_: TCustom_After_Source_Processor_Data_Pool): TMS64;
function Check_RewritePascal_Model(UnitData_, PatternData_: TSource_Processor_Data_Pool; CustomPattern_: TCustom_After_Source_Processor_Data_Pool): Boolean;

implementation

uses Z.Json, Z.Status, Z.UReplace;

function Pascal_Keyword(const s: TP_String): TPascal_Keyword;
var
  k: TPascal_Keyword;
begin
  if s.L = 0 then
      Exit(kUnknow);

  for k := low(TPascal_Keyword) to high(TPascal_Keyword) do
    if s.Same(TP_String(Pascal_Keyword_DICT[k].Decl)) then
        Exit(k);

  Exit(kUnknow);
end;

function Replace_Pascal_Code(var Code_: TP_String;
  PatternHash_: THashStringList; TT_: TTokenTypes; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; FileInfo__: SystemString; OnStatus: TOnRewriteStatus): Boolean;
var
  u_TP: TTextParsing;
  arry: TU_ArrayBatch;
  L: TU_BatchInfoList;

{$IFDEF FPC}
  procedure fpc_progress_(bPos, ePos: Integer; sour, dest: PUPascalString; var Accept: Boolean);
  begin
    Accept := u_TP.TokenPos[bPos]^.tokenType in TT_;
    if Accept then
      if Assigned(OnStatus) then
          OnStatus(FileInfo__ + 'rewrite feature symbol: %s -> %s', [sour^.Text, dest^.Text]);
  end;
{$ENDIF FPC}


begin
  Result := False;
  if PatternHash_ = nil then
      Exit;
  if PatternHash_.Count = 0 then
      Exit;
  if Assigned(OnStatus) then
      OnStatus(FileInfo__ + 'prepare feature.', []);
  arry := U_BuildBatch(PatternHash_);
  U_SortBatch(arry);
  u_TP := TTextParsing.Create(Code_, tsPascal, nil, TPascalString(SpacerSymbol.V).DeleteChar('.'));
  L := TU_BatchInfoList.Create;

{$IFDEF FPC}
  Code_.Text := U_BatchReplace(u_TP.Text.TrimChar(#0#13#10#32#9), arry, OnlyWord, IgnoreCase, 0, 0, L, fpc_progress_);
{$ELSE FPC}
  Code_.Text := U_BatchReplace(u_TP.Text.TrimChar(#0#13#10#32#9), arry, OnlyWord, IgnoreCase, 0, 0, L,
      procedure(bPos, ePos: Integer; sour, dest: PUPascalString; var Accept: Boolean)
    begin
      Accept := u_TP.TokenPos[bPos]^.tokenType in TT_;
      if Accept then
        if Assigned(OnStatus) then
            OnStatus(FileInfo__ + 'rewrite feature symbol: %s -> %s', [sour^.Text, dest^.Text]);
    end);
{$ENDIF FPC}
  Result := L.Count > 0;
  disposeObject(u_TP);
  disposeObject(L);
  U_ClearBatch(arry);
end;

function Replace_ASCII_Code(var Code_: TP_String; PatternHash_: THashStringList; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; FileInfo__: SystemString; OnStatus: TOnRewriteStatus): Boolean;
var
  arry: TU_ArrayBatch;
  L: TU_BatchInfoList;
begin
  Result := False;
  if PatternHash_ = nil then
      Exit;
  if PatternHash_.Count = 0 then
      Exit;
  if Assigned(OnStatus) then
      OnStatus(FileInfo__ + 'prepare feature.', []);
  arry := U_BuildBatch(PatternHash_);
  U_SortBatch(arry);
  L := TU_BatchInfoList.Create;

  Code_.Text := U_BatchReplace(Code_.TrimChar(#0#13#10#32#9), arry, OnlyWord, IgnoreCase, 0, 0, L, nil);
  Result := L.Count > 0;
  disposeObject(L);
  U_ClearBatch(arry);
end;

constructor TRewrite_Trace.Create();
begin
  inherited Create;
  Current_ := '';
  marco_hash_ := THashStringList.CustomCreate($FF);
  Include_Files_ := TPascalStringList.Create;
  Uses_Files_ := TPascalStringList.Create;
  Resource_Files_ := TPascalStringList.Create;
  Link_Files_ := TPascalStringList.Create;
  IsCode_ := False;
end;

destructor TRewrite_Trace.Destroy;
begin
  Current_ := '';
  disposeObject(marco_hash_);
  disposeObject(Include_Files_);
  disposeObject(Uses_Files_);
  disposeObject(Resource_Files_);
  disposeObject(Link_Files_);
  inherited Destroy;
end;

procedure TRewrite_Trace_Pool.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      disposeObject(items[i]);
  inherited Clear;
end;

function RewritePascal_Process_Code(const Code_: TCore_Strings; UnitHash_, PatternHash_: THashStringList; Trace_: TRewrite_Trace; FileInfo__: SystemString; OnStatus: TOnRewriteStatus): Boolean;
var
  marco_hash: THashStringList;
  fixedUName_Num: Integer;
  u_TP: TTextParsing;

  procedure Prepare_Rewrite_Unit_Define(OLD_, New_: U_String);
  begin
    marco_hash.Add(New_, New_);
    marco_hash.Add(OLD_, New_);
  end;

  function Run_Replace(p: TUPascalString): TUPascalString;
  var
    arry: TU_ArrayBatch;
{$IFDEF FPC}
    procedure fpc_progress_(bPos, ePos: Integer; sour, dest: PUPascalString; var Accept: Boolean);
    begin
      Accept := (u_TP.TokenPos[bPos]^.tokenType = ttAscii);
      if Accept then
        if Assigned(OnStatus) then
            OnStatus(FileInfo__ + 'symbol: %s -> %s', [sour^.Text, dest^.Text]);
    end;
{$ENDIF FPC}

  begin
    arry := U_BuildBatch(marco_hash);
    U_SortBatch(arry);

{$IFDEF FPC}
    Result := U_BatchReplace(p, arry, True, True, 0, 0, nil, fpc_progress_);
{$ELSE FPC}
    Result := U_BatchReplace(p, arry, True, True, 0, 0, nil,
      procedure(bPos, ePos: Integer; sour, dest: PUPascalString; var Accept: Boolean)
      begin
        Accept := (u_TP.TokenPos[bPos]^.tokenType = ttAscii);
        if Accept then
          if Assigned(OnStatus) then
              OnStatus(FileInfo__ + 'symbol: %s -> %s', [sour^.Text, dest^.Text]);
      end);
{$ENDIF FPC}
    U_ClearBatch(arry);
  end;

type
  TParsingState = (psUnit, psUses, psUses_In_File, psUnknow);
var
  i: Integer;
  p: PTokenData;
  State: TParsingState;
  N, N2, N3, Def_Name, prefix: TP_String;
  nFound: Boolean;
  nComp: Boolean;
  tmpHash: THashStringList;

begin
  marco_hash := THashStringList.Create;

  fixedUName_Num := 0;
  Result := False;

  if Assigned(OnStatus) then
      OnStatus(FileInfo__ + 'prepare parsing...', []);
  u_TP := TTextParsing.Create(Code_.Text, tsPascal, nil, TPascalString(SpacerSymbol.V).DeleteChar('.'));

  i := 0;
  State := psUnknow;
  while i < u_TP.TokenCount do
    begin
      p := u_TP.Tokens[i];
      if p^.tokenType = ttComment then
        begin
          Def_Name := p^.Text;
          if umlMultipleMatch('{*}', Def_Name) then // compiler define
            begin
              Def_Name.DeleteFirst;
              Def_Name.DeleteLast;
              N2 := Def_Name;

              if umlMultipleMatch(['$R *', '$Resouce *'], N2) then // resource
                begin
                  Def_Name := umlDeleteFirstStr(Def_Name, ' ').TrimChar(#32#39);
                  prefix := umlDeleteLastStr(Def_Name, '/\');
                  Def_Name := umlGetLastStr(Def_Name, '/\');
                  if UnitHash_.Exists(Def_Name) then
                    begin
                      N := UnitHash_[Def_Name];
                      if (Def_Name <> N) or (umlMultipleMatch(['$R *', '$Resouce *'], N2)) then
                        begin
                          if prefix.Len > 0 then
                              prefix := prefix.ReplaceChar('/', '\') + '\';

                          Def_Name := PFormat('{$R %s}', [prefix.Text + N.Text]);
                          nComp := p^.Text <> Def_Name;
                          if nComp then
                            begin
                              if Assigned(OnStatus) then
                                  OnStatus(FileInfo__ + 'rewrite resource : "%s" -> "%s"', [p^.Text.Text, Def_Name.Text]);
                              p^.Text := Def_Name;
                              Inc(fixedUName_Num);
                            end;
                          if Trace_ <> nil then
                              Trace_.Resource_Files_.Add(prefix.Text + N.Text);
                          Result := Result or nComp;
                        end;
                    end
                  else
                    begin
                      UnitHash_.FastAdd(Def_Name, Def_Name);
                      if umlMultipleMatch(['$R *', '$Resouce *'], N2) then
                        begin
                          Result := True;
                          N := Def_Name;
                          if prefix.Len > 0 then
                              prefix := prefix.ReplaceChar('/', '\') + '\';

                          Def_Name := PFormat('{$R %s}', [prefix.Text + N.Text]);
                          nComp := p^.Text <> Def_Name;
                          if nComp then
                            begin
                              if Assigned(OnStatus) then
                                  OnStatus(FileInfo__ + 'rewrite resource : "%s" -> "%s"', [p^.Text.Text, Def_Name.Text]);
                              p^.Text := Def_Name;
                              Inc(fixedUName_Num);
                            end;
                          if Trace_ <> nil then
                              Trace_.Resource_Files_.Add(prefix.Text + N.Text);
                          Result := Result or nComp;
                        end;
                    end;
                end
              else if umlMultipleMatch(['$L *', '$Link *'], N2) then // link
                begin
                  Def_Name := umlDeleteFirstStr(Def_Name, ' ').TrimChar(#32#39);
                  prefix := umlDeleteLastStr(Def_Name, '/\');
                  Def_Name := umlGetLastStr(Def_Name, '/\');
                  if UnitHash_.Exists(Def_Name) then
                    begin
                      N := UnitHash_[Def_Name];
                      if (Def_Name <> N) or (umlMultipleMatch(['$L *', '$Link *'], N2)) then
                        begin
                          if prefix.Len > 0 then
                              prefix := prefix.ReplaceChar('/', '\') + '\';

                          Def_Name := PFormat('{$L %s}', [prefix.Text + N.Text]);
                          nComp := p^.Text <> Def_Name;
                          if nComp then
                            begin
                              if Assigned(OnStatus) then
                                  OnStatus(FileInfo__ + 'rewrite Link : "%s" -> "%s"', [p^.Text.Text, Def_Name.Text]);
                              p^.Text := Def_Name;
                              Inc(fixedUName_Num);
                            end;
                          if Trace_ <> nil then
                              Trace_.Link_Files_.Add(prefix.Text + N.Text);
                          Result := Result or nComp;
                        end;
                    end
                  else
                    begin
                      UnitHash_.FastAdd(Def_Name, Def_Name);
                      if umlMultipleMatch(['$L *', '$Link *'], N2) then
                        begin
                          Result := True;
                          N := Def_Name;
                          if prefix.Len > 0 then
                              prefix := prefix.ReplaceChar('/', '\') + '\';

                          Def_Name := PFormat('{$L %s}', [prefix.Text + N.Text]);
                          nComp := p^.Text <> Def_Name;
                          if nComp then
                            begin
                              if Assigned(OnStatus) then
                                  OnStatus(FileInfo__ + 'rewrite Link : "%s" -> "%s"', [p^.Text.Text, Def_Name.Text]);
                              p^.Text := Def_Name;
                              Inc(fixedUName_Num);
                            end;
                          if Trace_ <> nil then
                              Trace_.Link_Files_.Add(prefix.Text + N.Text);
                          Result := Result or nComp;
                        end;
                    end;
                end
              else if umlMultipleMatch(['$I *', '$INCLUDE *'], N2) then // Include
                begin
                  Def_Name := umlDeleteFirstStr(Def_Name, ' ').TrimChar(#32#39);
                  prefix := umlDeleteLastStr(Def_Name, '/\');
                  Def_Name := umlGetLastStr(Def_Name, '/\');

                  if UnitHash_.Exists(Def_Name) then
                    begin
                      N := UnitHash_[Def_Name];
                      if (Def_Name <> N) or (umlMultipleMatch(['$I *', '$INCLUDE *'], N2)) then
                        begin
                          if prefix.Len > 0 then
                              prefix := prefix.ReplaceChar('/', '\') + '\';

                          Def_Name := PFormat('{$I %s}', [prefix.Text + N.Text]);
                          nComp := p^.Text <> Def_Name;
                          if nComp then
                            begin
                              if Assigned(OnStatus) then
                                  OnStatus(FileInfo__ + 'rewrite Include : "%s" -> "%s"', [p^.Text.Text, Def_Name.Text]);
                              p^.Text := Def_Name;
                              Inc(fixedUName_Num);
                            end;
                          if Trace_ <> nil then
                              Trace_.Include_Files_.Add(prefix.Text + N.Text);
                          Result := Result or nComp;
                        end;
                    end
                  else
                    begin
                      UnitHash_.FastAdd(Def_Name, Def_Name);
                      if umlMultipleMatch(['$I *', '$INCLUDE *'], N2) then
                        begin
                          Result := True;
                          N := Def_Name;
                          if prefix.Len > 0 then
                              prefix := prefix.ReplaceChar('/', '\') + '\';

                          Def_Name := PFormat('{$I %s}', [prefix.Text + N.Text]);
                          nComp := p^.Text <> Def_Name;
                          if nComp then
                            begin
                              if Assigned(OnStatus) then
                                  OnStatus(FileInfo__ + 'rewrite Include : "%s" -> "%s"', [p^.Text.Text, Def_Name.Text]);
                              p^.Text := Def_Name;
                              Inc(fixedUName_Num);
                            end;
                          if Trace_ <> nil then
                              Trace_.Include_Files_.Add(prefix.Text + N.Text);
                          Result := Result or nComp;
                        end;
                    end;
                end;
            end;
        end
      else
        begin
          case State of
            psUnit:
              begin
                if (p^.tokenType = ttAscii) then
                  begin
                    if UnitHash_.Exists(p^.Text + '.pas') then // delphi+fpc
                      begin
                        N := umlChangeFileExt(UnitHash_[p^.Text + '.pas'], '');
                        nComp := p^.Text <> N;
                        Result := Result or nComp;
                        if nComp then
                          begin
                            Prepare_Rewrite_Unit_Define(p^.Text, N);
                            if Assigned(OnStatus) then
                                OnStatus(FileInfo__ + 'rewrite unit define: %s -> %s', [p^.Text.Text, N.Text]);
                            Inc(fixedUName_Num);
                            p^.Text := N;
                          end;
                        if Trace_ <> nil then
                            Trace_.Uses_Files_.Add(N);
                      end
                    else if UnitHash_.Exists(p^.Text + '.pp') then // fpc
                      begin
                        N := umlChangeFileExt(UnitHash_[p^.Text + '.pp'], '');
                        nComp := p^.Text <> N;
                        Result := Result or nComp;
                        if nComp then
                          begin
                            Prepare_Rewrite_Unit_Define(p^.Text, N);
                            if Assigned(OnStatus) then
                                OnStatus(FileInfo__ + 'rewrite unit define: %s -> %s', [p^.Text.Text, N.Text]);
                            Inc(fixedUName_Num);
                            p^.Text := N;
                          end;
                        if Trace_ <> nil then
                            Trace_.Uses_Files_.Add(N);
                      end;
                  end
                else if (p^.tokenType = ttSymbol) and (p^.Text = ';') then
                    State := psUnknow;
              end;
            psUses:
              begin
                if (p^.tokenType = ttAscii) then
                  begin
                    if Pascal_Keyword(p^.Text) in [kEmpty, kUnknow] then
                      begin
                        if UnitHash_.Exists(p^.Text + '.pas') then // delphi+fpc
                          begin
                            N := umlChangeFileExt(UnitHash_[p^.Text + '.pas'], '');
                            nComp := p^.Text <> N;
                            Result := Result or nComp;
                            if nComp then
                              begin
                                Prepare_Rewrite_Unit_Define(p^.Text, N);
                                if Assigned(OnStatus) then
                                    OnStatus(FileInfo__ + 'rewrite uses: %s -> %s', [p^.Text.Text, N.Text]);
                                Inc(fixedUName_Num);
                                p^.Text := N;
                              end;
                            if Trace_ <> nil then
                                Trace_.Uses_Files_.Add(N);
                          end
                        else if UnitHash_.Exists(p^.Text + '.pp') then // fpc
                          begin
                            N := umlChangeFileExt(UnitHash_[p^.Text + '.pp'], '');
                            nComp := p^.Text <> N;
                            Result := Result or nComp;
                            if nComp then
                              begin
                                Prepare_Rewrite_Unit_Define(p^.Text, N);
                                if Assigned(OnStatus) then
                                    OnStatus(FileInfo__ + 'rewrite uses: %s -> %s', [p^.Text.Text, N.Text]);
                                Inc(fixedUName_Num);
                                p^.Text := N;
                              end;
                            if Trace_ <> nil then
                                Trace_.Uses_Files_.Add(N);
                          end
                        else
                          begin
                            UnitHash_.FastAdd(p^.Text, p^.Text);
                            if Trace_ <> nil then
                                Trace_.Uses_Files_.Add(p^.Text);
                          end;
                      end
                    else
                      case Pascal_Keyword(p^.Text) of
                        kIn: State := psUses_In_File;
                        kUses: State := psUses;
                        else State := psUnknow;
                      end;
                  end;
              end;
            psUses_In_File:
              begin
                if (p^.tokenType = ttSymbol) then
                  begin
                    if (p^.Text = ',') then
                        State := psUses
                    else if p^.Text = ';' then
                        State := psUnknow;
                  end
                else if p^.tokenType = ttTextDecl then
                  begin
                    N := TTextParsing.Translate_Pascal_Decl_To_Text(p^.Text);
                    N2 := umlGetFileName(N);
                    prefix := umlGetFilePath(N);
                    N3 := UnitHash_.GetDefaultValue(N2, N2);
                    N := TTextParsing.Translate_Text_To_Pascal_Decl(umlCombineFileName(prefix, N3));
                    nComp := p^.Text <> N;
                    Result := Result or nComp;
                    if nComp then
                      begin
                        p^.Text := N;
                        if Assigned(OnStatus) then
                            OnStatus(FileInfo__ + 'rewrite uses in file: %s -> %s', [N.Text, N3.Text]);
                      end;
                  end;
              end;
            psUnknow:
              begin
                if (p^.tokenType = ttAscii) then
                  begin
                    case Pascal_Keyword(p^.Text) of
                      TPascal_Keyword.kUnit, TPascal_Keyword.kProgram, TPascal_Keyword.kLibrary: State := psUnit;
                      TPascal_Keyword.kUses: State := psUses;
                    end;
                  end;
              end;
          end;
        end;
      Inc(i);
    end;

  if Result then
    begin
      if Assigned(OnStatus) then
          OnStatus(FileInfo__ + 'rebuild code...', []);
      u_TP.RebuildToken;
      Code_.Text := Run_Replace(u_TP.Text.TrimChar(#0#13#10#32#9));
    end;

  disposeObject(u_TP);

  if PatternHash_ <> nil then
    begin
      tmpHash := THashStringList.CustomCreate($FFFF);
      PatternHash_.MergeTo(tmpHash);
      marco_hash.MergeTo(tmpHash);
      N.Text := Code_.Text;
      if Replace_Pascal_Code(N, tmpHash, [ttAscii, ttNumber], True, True, 0, 0, FileInfo__, OnStatus) then
        begin
          Code_.Text := N;
          Result := True;
        end;
      N := '';
      disposeObject(tmpHash);
    end;
  if Trace_ <> nil then
      marco_hash.MergeTo(Trace_.marco_hash_);
  disposeObject(marco_hash);
end;

function RewritePascal_Process_Code(const Code_: TCore_Strings; UnitHash_, PatternHash_: THashStringList; FileInfo__: SystemString; OnStatus: TOnRewriteStatus): Boolean;
var
  tmp: TRewrite_Trace;
begin
  tmp := TRewrite_Trace.Create;
  Result := RewritePascal_Process_Code(Code_, UnitHash_, PatternHash_, tmp, FileInfo__, OnStatus);
  disposeObject(tmp);
end;

function RewritePascal_ProcessFile(fn: U_String; UnitHash_, PatternHash_: THashStringList; Trace_: TRewrite_Trace; FileInfo__: SystemString; OnStatus: TOnRewriteStatus): Boolean;
var
  Code: TCore_StringList;
  ft: TDateTime;
  ph, OLD_Feature, New_Feature, new_fn: U_String;
  m64: TMem64;

  procedure Process_AddionalFile(ext_: U_String);
  var
    tmp_old_fn, tmp_new_fn: U_String;
  begin
    tmp_old_fn := umlChangeFileExt(fn, ext_);
    tmp_new_fn := umlChangeFileExt(new_fn, ext_);
    if umlFileExists(tmp_old_fn) then
      begin
        m64 := TMem64.Create;
        m64.LoadFromFile(tmp_old_fn);
        umlDeleteFile(tmp_old_fn);
        m64.SaveToFile(tmp_new_fn);
        disposeObject(m64);
        if Assigned(OnStatus) then
            OnStatus(FileInfo__ + 'rename %s -> %s', [tmp_old_fn.Text, tmp_new_fn.Text]);
      end;
  end;

var
  LEncode: TEncoding;

  procedure checkAndLoadFile(fn_: U_String);
  var
    ms64_: TMS64;
  begin
    ms64_ := TMS64.Create;
    ms64_.LoadFromFile(fn_);
    if umlBufferIsASCII(ms64_.Memory, ms64_.Size) then
        LEncode := TEncoding.ANSI
    else
        LEncode := TEncoding.UTF8;

    try
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    except
      LEncode := TEncoding.ANSI;
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    end;
    disposeObject(ms64_);
  end;

begin
  Result := False;
  if Trace_ <> nil then
      Trace_.IsCode_ := False;

  ph := umlGetFilePath(fn);
  OLD_Feature := umlGetFileName(fn);
  New_Feature := UnitHash_.GetDefaultValue(OLD_Feature, OLD_Feature);
  new_fn := umlCombineFileName(ph, New_Feature);
  ft := umlGetFileTime(fn);

  if not OLD_Feature.Same(False, New_Feature) then
    begin
      m64 := TMem64.Create;
      m64.LoadFromFile(fn);
      umlDeleteFile(fn);
      m64.SaveToFile(new_fn);
      disposeObject(m64);
      if Assigned(OnStatus) then
          OnStatus(FileInfo__ + 'rename file %s -> %s', [OLD_Feature.Text, New_Feature.Text]);
      umlSetFileTime(new_fn, ft);
      if Assigned(OnStatus) then
          OnStatus(FileInfo__ + 'rewrite file time %s -> %s', [New_Feature.Text, DateTimeToStr(ft)]);

      if umlMultipleMatch(['*.pas', '*.pp'], OLD_Feature) then
        begin
          Process_AddionalFile('.dfm');
          Process_AddionalFile('.fmx');
          Process_AddionalFile('.lfm');
        end;
      if umlMultipleMatch(['*.dpr'], OLD_Feature) then
          Process_AddionalFile('.dproj');
    end;

  if umlMultipleMatch(['*.pas', '*.dpr', '*.lpr', '*.pp', '*.inc'], fn) then
    begin
      Code := TCore_StringList.Create;
      checkAndLoadFile(new_fn);
      if Trace_ <> nil then
          Trace_.Current_ := new_fn;
      Result := RewritePascal_Process_Code(Code, UnitHash_, PatternHash_, Trace_, FileInfo__, OnStatus);
      if Trace_ <> nil then
          Trace_.IsCode_ := True;
      if Result then
        begin
          try
              Code.SaveToFile(new_fn, LEncode);
          except
          end;
          if Assigned(OnStatus) then
              OnStatus(FileInfo__ + 'rebuild code %s', [New_Feature.Text]);
          umlSetFileTime(new_fn, ft);
          if Assigned(OnStatus) then
              OnStatus(FileInfo__ + 'rewrite file time %s -> %s', [New_Feature.Text, DateTimeToStr(ft)]);
        end;
      disposeObject(Code);
    end;
end;

procedure RewritePascal_Include_File_Processor(UnitHash_, PatternHash_: THashStringList; Trace_Pool: TRewrite_Trace_Pool; OnStatus: TOnRewriteStatus);
  function Found_File_From_Include(fn: U_String; Trace_: TRewrite_Trace): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to Trace_.Include_Files_.Count - 1 do
      if fn.Same(umlGetFileName(Trace_.Include_Files_[i])) then
          Exit(True);
    Result := False;
  end;

var
  LEncode: TEncoding;
  Code: TCore_StringList;
  ft: TDateTime;

  procedure checkAndLoadFile(fn_: U_String);
  var
    ms64_: TMS64;
  begin
    ms64_ := TMS64.Create;
    ms64_.LoadFromFile(fn_);
    if umlBufferIsASCII(ms64_.Memory, ms64_.Size) then
        LEncode := TEncoding.ANSI
    else
        LEncode := TEncoding.UTF8;

    try
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    except
      LEncode := TEncoding.ANSI;
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    end;
    disposeObject(ms64_);
  end;

var
  i, j: Integer;
  incl_: TRewrite_Trace_Pool;
  fn: U_String;
  incl_Trace: TRewrite_Trace;
  LNum: NativeInt;
  IsUpdate, IsChanged: Boolean;
  N: TP_String;
begin
  if Assigned(OnStatus) then
      OnStatus('include after running...', []);
  incl_ := TRewrite_Trace_Pool.Create;

  // build include space
  for i := 0 to Trace_Pool.Count - 1 do
    begin
      fn := umlGetFileName(Trace_Pool[i].Current_);
      if umlMultipleMatch(['*.inc'], fn) then
          incl_.Add(Trace_Pool[i]);
    end;

  // merge include
  IsChanged := False;
  repeat
    IsUpdate := False;
    for i := 0 to incl_.Count - 1 do
      begin
        incl_Trace := incl_[i];
        fn := umlGetFileName(incl_Trace.Current_);
        for j := 0 to Trace_Pool.Count - 1 do
          if Found_File_From_Include(fn, Trace_Pool[j]) then
            begin
              LNum := incl_Trace.marco_hash_.Count;
              Trace_Pool[j].marco_hash_.MergeTo(incl_Trace.marco_hash_);
              if incl_Trace.marco_hash_.Count > LNum then
                begin
                  IsUpdate := True;
                  IsChanged := True;
                end;
            end;
      end;
  until not IsUpdate;

  // rewrite include
  if IsChanged then
    for i := 0 to incl_.Count - 1 do
      begin
        incl_Trace := incl_[i];
        PatternHash_.MergeTo(incl_Trace.marco_hash_);
        ft := umlGetFileTime(fn);
        Code := TCore_StringList.Create;
        checkAndLoadFile(incl_Trace.Current_);

        N.Text := Code.Text;
        if Replace_Pascal_Code(N, incl_Trace.marco_hash_, [ttAscii], True, True, 0, 0, PFormat('"%s" ', [incl_Trace.Current_.Text]), OnStatus) then
          begin
            Code.Text := N;
            try
                Code.SaveToFile(incl_Trace.Current_, LEncode);
            except
            end;
            if Assigned(OnStatus) then
                OnStatus('rebuild code %s', [incl_Trace.Current_.Text]);

            umlSetFileTime(incl_Trace.Current_, ft);

            if Assigned(OnStatus) then
                OnStatus('rewrite file time %s -> %s', [incl_Trace.Current_.Text, DateTimeToStr(ft)]);
          end;
        N := '';
        disposeObject(Code);
      end;

  disposeObject(incl_);
  if Assigned(OnStatus) then
      OnStatus('include after done.', []);
end;

procedure RewritePascal_Custom_After_Processor(fn: U_String; Custom_PatternHash_: THashStringList; OnlyWord_: Boolean; FileInfo__: SystemString; OnStatus: TOnRewriteStatus);
var
  LEncode: TEncoding;
  Code: TCore_StringList;
  ft: TDateTime;

  procedure checkAndLoadFile(fn_: U_String);
  var
    ms64_: TMS64;
  begin
    ms64_ := TMS64.Create;
    ms64_.LoadFromFile(fn_);
    if umlBufferIsASCII(ms64_.Memory, ms64_.Size) then
        LEncode := TEncoding.ANSI
    else
        LEncode := TEncoding.UTF8;

    try
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    except
      LEncode := TEncoding.ANSI;
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    end;
    disposeObject(ms64_);
  end;

var
  N: TP_String;
  tmp_m64: TMS64;
begin
  ft := umlGetFileTime(fn);
  Code := TCore_StringList.Create;
  checkAndLoadFile(fn);

  N.Text := Code.Text;
  if Replace_ASCII_Code(N, Custom_PatternHash_, OnlyWord_, True, 0, 0, FileInfo__, OnStatus) then
    begin
      Code.Text := N;
      tmp_m64 := TMS64.Create;
      try
          Code.SaveToStream(tmp_m64, LEncode);
      except
      end;
      tmp_m64.SaveToFile(fn);
      disposeObject(tmp_m64);

      umlSetFileTime(fn, ft);
      if Assigned(OnStatus) then
          OnStatus('rebuild code %s', [fn.Text]);
    end;
  N := '';
  disposeObject(Code);
end;

function RewritePascal_ProcessDirectory(Parallel_: Boolean; directory_: U_String; UnitHash_, PatternHash_: THashStringList; CustomPattern_: TCustom_After_Source_Processor_Data_Pool; OnStatus: TOnRewriteStatus): Integer;
var
  arry: U_StringArray;
  num: Integer;
  Trace_Pool: TRewrite_Trace_Pool;
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor_File(pass: Integer);
  var
    tmp: TRewrite_Trace;
  begin
    tmp := TRewrite_Trace.Create;
    if RewritePascal_ProcessFile(arry[pass], UnitHash_, PatternHash_, tmp, PFormat('"%s" ', [arry[pass]]), OnStatus) then
        AtomInc(num);
    if tmp.IsCode_ then
      begin
        LockObject(Trace_Pool);
        Trace_Pool.Add(tmp);
        UnLockObject(Trace_Pool);
      end
    else
        disposeObject(tmp);
  end;
  procedure Nested_ParallelFor_File_Custom_Pattern(pass: Integer);
  var
    Custom_PatternHash_: THashStringList;
  begin
    Custom_PatternHash_ := THashStringList.CustomCreate($FFFF);
    if CustomPattern_.Build_Hash_Pool(umlGetFileName(arry[pass]), Custom_PatternHash_) then
        RewritePascal_Custom_After_Processor(arry[pass], Custom_PatternHash_, True, umlGetFileName(arry[pass]), OnStatus);
    disposeObject(Custom_PatternHash_);
  end;

  procedure Nested_ParallelFor_Dir(pass: Integer);
  begin
    AtomInc(num, RewritePascal_ProcessDirectory(Parallel_, arry[pass], UnitHash_, PatternHash_, CustomPattern_, OnStatus));
  end;
{$ENDIF FPC}
{$ENDIF Parallel}
  procedure DoFor();
  var
    pass: Integer;
    tmp: TRewrite_Trace;
    Custom_PatternHash_: THashStringList;
  begin
    for pass := 0 to Length(arry) - 1 do
      begin
        tmp := TRewrite_Trace.Create;
        if RewritePascal_ProcessFile(arry[pass], UnitHash_, PatternHash_, tmp, PFormat('"%s" ', [arry[pass]]), OnStatus) then
            Inc(num);
        if tmp.IsCode_ then
          begin
            Trace_Pool.Add(tmp);
          end
        else
            disposeObject(tmp);
      end;
    RewritePascal_Include_File_Processor(UnitHash_, PatternHash_, Trace_Pool, OnStatus);
    Trace_Pool.Clean;
    disposeObject(Trace_Pool);

    arry := umlGet_File_Full_Array(directory_);
    for pass := 0 to Length(arry) - 1 do
      begin
        Custom_PatternHash_ := THashStringList.CustomCreate($FFFF);
        if CustomPattern_.Build_Hash_Pool(umlGetFileName(arry[pass]), Custom_PatternHash_) then
            RewritePascal_Custom_After_Processor(arry[pass], Custom_PatternHash_, True, umlGetFileName(arry[pass]), OnStatus);
        disposeObject(Custom_PatternHash_);
      end;

    arry := umlGet_Path_Full_Array(directory_);
    for pass := 0 to Length(arry) - 1 do
        Inc(num, RewritePascal_ProcessDirectory(Parallel_, arry[pass], UnitHash_, PatternHash_, CustomPattern_, OnStatus));
  end;

begin
  num := 0;
  arry := umlGet_File_Full_Array(directory_);
  Trace_Pool := TRewrite_Trace_Pool.Create;

  if Parallel_ then
    begin
{$IFDEF Parallel}
{$IFDEF FPC}
      FPCParallelFor(Nested_ParallelFor_File, 0, Length(arry) - 1);
      RewritePascal_Include_File_Processor(UnitHash_, PatternHash_, Trace_Pool, OnStatus);
      Trace_Pool.Clean;
      disposeObject(Trace_Pool);
      arry := umlGet_File_Full_Array(directory_);
      FPCParallelFor(Nested_ParallelFor_File_Custom_Pattern, 0, Length(arry) - 1);
      arry := umlGet_Path_Full_Array(directory_);
      FPCParallelFor(Nested_ParallelFor_Dir, 0, Length(arry) - 1);
{$ELSE FPC}
      DelphiParallelFor(0, Length(arry) - 1, procedure(pass: Integer)
        var
          tmp: TRewrite_Trace;
        begin
          tmp := TRewrite_Trace.Create;
          if RewritePascal_ProcessFile(arry[pass], UnitHash_, PatternHash_, tmp, PFormat('"%s" ', [arry[pass]]), OnStatus) then
              AtomInc(num);
          if tmp.IsCode_ then
            begin
              LockObject(Trace_Pool);
              Trace_Pool.Add(tmp);
              UnLockObject(Trace_Pool);
            end
          else
              disposeObject(tmp);
        end);
      RewritePascal_Include_File_Processor(UnitHash_, PatternHash_, Trace_Pool, OnStatus);
      Trace_Pool.Clean;
      disposeObject(Trace_Pool);

      arry := umlGet_File_Full_Array(directory_);
      DelphiParallelFor(0, Length(arry) - 1, procedure(pass: Integer)
        var
          Custom_PatternHash_: THashStringList;
        begin
          Custom_PatternHash_ := THashStringList.CustomCreate($FFFF);
          if CustomPattern_.Build_Hash_Pool(umlGetFileName(arry[pass]), Custom_PatternHash_) then
              RewritePascal_Custom_After_Processor(arry[pass], Custom_PatternHash_, True, umlGetFileName(arry[pass]), OnStatus);
          disposeObject(Custom_PatternHash_);
        end);

      arry := umlGet_Path_Full_Array(directory_);
      DelphiParallelFor(0, Length(arry) - 1, procedure(pass: Integer)
        begin
          AtomInc(num, RewritePascal_ProcessDirectory(Parallel_, arry[pass], UnitHash_, PatternHash_, CustomPattern_, OnStatus));
        end);
{$ENDIF FPC}
{$ELSE Parallel}
      DoFor();
{$ENDIF Parallel}
    end
  else
      DoFor();

  Result := num;
end;

function RewritePascal_ProcessDirectory(Parallel_: Boolean; directory_: U_String; Model_: TMS64; Reverse_: Boolean; OnStatus: TOnRewriteStatus): Integer;
var
  dec: TZDB2_File_Decoder;
  fi: TZDB2_FI;
  UnitData_, PatternData_: TSource_Processor_Data_Pool;
  CustomPattern_: TCustom_After_Source_Processor_Data_Pool;
  m64: TMS64;
  UnitHash_, PatternHash_: THashStringList;
  i: Integer;
begin
  Result := 0;
  if not TZDB2_File_Decoder.Check(Model_) then
      Exit;

  dec := TZDB2_File_Decoder.Create(Model_, 0);
  UnitData_ := TSource_Processor_Data_Pool.Create;
  PatternData_ := TSource_Processor_Data_Pool.Create;
  CustomPattern_ := TCustom_After_Source_Processor_Data_Pool.Create;

  fi := dec.Files.FindFile('Unit');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      UnitData_.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Unit Rewrite Model.');
    end;

  fi := dec.Files.FindFile('Pattern');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      PatternData_.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Symbol Rewrite Model.');
    end;

  fi := dec.Files.FindFile('Custom');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      CustomPattern_.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Custom Rewrite Model.');
    end;

  disposeObject(dec);

  if Reverse_ then
    begin
      for i := 0 to UnitData_.Count - 1 do
        with UnitData_[i]^ do
            OLD_Feature.SwapInstance(New_Feature);

      for i := 0 to PatternData_.Count - 1 do
        with PatternData_[i]^ do
            OLD_Feature.SwapInstance(New_Feature);

      for i := 0 to CustomPattern_.Count - 1 do
        with CustomPattern_[i]^ do
            OLD_Feature.SwapInstance(New_Feature);
    end;

  UnitHash_ := THashStringList.CustomCreate($FFFF);
  UnitData_.Build_Hash_Pool(UnitHash_);
  disposeObject(UnitData_);

  PatternHash_ := THashStringList.CustomCreate($FFFF);
  PatternData_.Build_Hash_Pool(PatternHash_);
  disposeObject(PatternData_);

  Result := RewritePascal_ProcessDirectory(Parallel_, directory_, UnitHash_, PatternHash_, CustomPattern_, OnStatus);

  disposeObject(UnitHash_);
  disposeObject(PatternHash_);
  disposeObject(CustomPattern_);
end;

class function TThread_RewritePascal_Process_ZDB_File.Init_(th_Post_: TThreadPost; Eng__: TObjectDataManager; fn_: U_String; UnitHash__, PatternHash__: THashStringList;
Trace__: TRewrite_Trace; OnStatus_: TOnRewriteStatus): TThread_RewritePascal_Process_ZDB_File;
begin
  Result := TThread_RewritePascal_Process_ZDB_File.Create;
  Result.Busy := True;
  Result.th_Post := th_Post_;
  Result.Eng_ := Eng__;
  Result.fn := fn_;
  Result.UnitHash_ := UnitHash__;
  Result.PatternHash_ := PatternHash__;
  Result.Trace_ := Trace__;
  Result.OnStatus := OnStatus_;
  Result.Do_Run;
end;

destructor TThread_RewritePascal_Process_ZDB_File.Destroy;
begin
  inherited Destroy;
end;

procedure TThread_RewritePascal_Process_ZDB_File.Do_Sync();
var
  tmp_m64: TMS64;
begin
  if Trace_ <> nil then
      Trace_.IsCode_ := True;
  if Result_RewritePascal_Process_ZDB_File then
    begin
      tmp_m64 := TMS64.Create;
      try
          Code.SaveToStream(tmp_m64, LEncode);
      except
      end;
      tmp_m64.SaveTo_ZDB_File(Eng_, new_fn);
      disposeObject(tmp_m64);
      umlSetFileTime(new_fn, ft);
      if Assigned(OnStatus) then
          OnStatus('rebuild code %s', [New_Feature.Text]);
    end;
  disposeObject(Code);
  Busy := False;
end;

procedure TThread_RewritePascal_Process_ZDB_File.Do_RewritePascal_Process_Code;
begin
  if Assigned(OnStatus) then
      OnStatus('prepare %s -> %s', [OLD_Feature.Text, New_Feature.Text]);
  Result_RewritePascal_Process_ZDB_File := RewritePascal_Process_Code(Code, UnitHash_, PatternHash_, Trace_, OLD_Feature, OnStatus);
  th_Post.PostM1(Do_Sync);
end;

procedure TThread_RewritePascal_Process_ZDB_File.Do_Run;
  procedure Process_AddionalFile(ext_: U_String);
  var
    tmp_old_fn, tmp_new_fn: U_String;
  begin
    tmp_old_fn := umlChangeFileExt(fn, ext_);
    tmp_new_fn := umlChangeFileExt(new_fn, ext_);
    if Eng_.ItemExists(tmp_old_fn) then
      begin
        m64 := TMem64.Create;
        m64.LoadFrom_ZDB_File(Eng_, tmp_old_fn);
        Eng_.ItemDelete(tmp_old_fn);
        m64.SaveTo_ZDB_File(Eng_, tmp_new_fn);
        disposeObject(m64);
        if Assigned(OnStatus) then
            OnStatus('make %s', [tmp_new_fn.Text]);
      end;
  end;
  procedure checkAndLoadFile(fn_: U_String);
  var
    ms64_: TMS64;
  begin
    ms64_ := TMS64.Create;
    ms64_.LoadFrom_ZDB_File(Eng_, fn_);
    if umlBufferIsASCII(ms64_.Memory, ms64_.Size) then
        LEncode := TEncoding.ANSI
    else
        LEncode := TEncoding.UTF8;

    try
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    except
      LEncode := TEncoding.ANSI;
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    end;
    disposeObject(ms64_);
  end;

begin
  Result_RewritePascal_Process_ZDB_File := False;

  if Trace_ <> nil then
      Trace_.IsCode_ := False;

  ph := umlGetUnixFilePath(fn);
  OLD_Feature := umlGetUnixFileName(fn);
  New_Feature := UnitHash_.GetDefaultValue(OLD_Feature, OLD_Feature);
  new_fn := umlCombineUnixFileName(ph, New_Feature);
  ft := Eng_.ItemTime(fn);

  if not OLD_Feature.Same(False, New_Feature) then
    begin
      m64 := TMem64.Create;
      m64.LoadFrom_ZDB_File(Eng_, fn);
      Eng_.ItemDelete(fn);
      m64.SaveTo_ZDB_File(Eng_, new_fn);
      disposeObject(m64);
      Eng_.SetItemTime(new_fn, ft);

      if umlMultipleMatch(['*.pas', '*.pp'], OLD_Feature) then
        begin
          Process_AddionalFile('.dfm');
          Process_AddionalFile('.fmx');
          Process_AddionalFile('.lfm');
        end;
      if umlMultipleMatch(['*.dpr'], OLD_Feature) then
          Process_AddionalFile('.dproj');
    end;

  if umlMultipleMatch(['*.pas', '*.dpr', '*.lpr', '*.pp', '*.inc'], fn) then
    begin
      Code := TCore_StringList.Create;
      checkAndLoadFile(new_fn);
      if Trace_ <> nil then
          Trace_.Current_ := new_fn;
      TCompute.RunM_NP(Do_RewritePascal_Process_Code);
    end
  else
      Busy := False;
end;

procedure Th_RewritePascal_ZDB_Include_File_Processor(Eng_: TObjectDataManager; UnitHash_, PatternHash_: THashStringList; Trace_Pool: TRewrite_Trace_Pool; OnStatus: TOnRewriteStatus);

  function Found_File_From_Include(fn: U_String; Trace_: TRewrite_Trace): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to Trace_.Include_Files_.Count - 1 do
      if fn.Same(umlGetUnixFileName(Trace_.Include_Files_[i])) then
          Exit(True);
    Result := False;
  end;

var
  LEncode: TEncoding;
  Code: TCore_StringList;
  ft: TDateTime;

  procedure checkAndLoadFile(fn_: U_String);
  var
    ms64_: TMS64;
  begin
    ms64_ := TMS64.Create;
    ms64_.LoadFrom_ZDB_File(Eng_, fn_);
    if umlBufferIsASCII(ms64_.Memory, ms64_.Size) then
        LEncode := TEncoding.ANSI
    else
        LEncode := TEncoding.UTF8;

    try
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    except
      LEncode := TEncoding.ANSI;
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    end;
    disposeObject(ms64_);
  end;

var
  i, j: Integer;
  incl_: TRewrite_Trace_Pool;
  fn: U_String;
  incl_Trace: TRewrite_Trace;
  LNum: NativeInt;
  IsUpdate, IsChanged: Boolean;
  N: TP_String;
  tmp_m64: TMS64;
begin
  if Assigned(OnStatus) then
      OnStatus('include after running...', []);
  incl_ := TRewrite_Trace_Pool.Create;

  // build include space
  for i := 0 to Trace_Pool.Count - 1 do
    begin
      fn := umlGetUnixFileName(Trace_Pool[i].Current_);
      if umlMultipleMatch(['*.inc'], fn) then
          incl_.Add(Trace_Pool[i]);
    end;

  // merge include
  IsChanged := False;
  repeat
    IsUpdate := False;
    for i := 0 to incl_.Count - 1 do
      begin
        incl_Trace := incl_[i];
        fn := umlGetUnixFileName(incl_Trace.Current_);
        for j := 0 to Trace_Pool.Count - 1 do
          if Found_File_From_Include(fn, Trace_Pool[j]) then
            begin
              LNum := incl_Trace.marco_hash_.Count;
              Trace_Pool[j].marco_hash_.MergeTo(incl_Trace.marco_hash_);
              if incl_Trace.marco_hash_.Count > LNum then
                begin
                  IsUpdate := True;
                  IsChanged := True;
                end;
            end;
      end;
  until not IsUpdate;

  // rewrite include
  if IsChanged then
    for i := 0 to incl_.Count - 1 do
      begin
        incl_Trace := incl_[i];
        PatternHash_.MergeTo(incl_Trace.marco_hash_);
        ft := Eng_.ItemTime(fn);
        Code := TCore_StringList.Create;
        checkAndLoadFile(incl_Trace.Current_);

        N.Text := Code.Text;
        if Replace_Pascal_Code(N, incl_Trace.marco_hash_, [ttAscii], True, True, 0, 0, '', nil) then
          begin
            Code.Text := N;
            tmp_m64 := TMS64.Create;
            try
                Code.SaveToStream(tmp_m64, LEncode);
            except
            end;
            tmp_m64.SaveTo_ZDB_File(Eng_, incl_Trace.Current_);
            disposeObject(tmp_m64);

            Eng_.SetItemTime(incl_Trace.Current_, ft);
            if Assigned(OnStatus) then
                OnStatus('rebuild code %s', [incl_Trace.Current_.Text]);
          end;
        N := '';
        disposeObject(Code);
      end;

  disposeObject(incl_);
  if Assigned(OnStatus) then
      OnStatus('include after done.', []);
end;

procedure Th_RewritePascal_Process_ZDB_Directory(Eng_: TObjectDataManager; directory_: U_String; UnitHash_, PatternHash_: THashStringList; CustomPattern_: TCustom_After_Source_Processor_Data_Pool; OnStatus: TOnRewriteStatus);
var
  thID: TThreadID;
  th_Post: TThreadPost;
  th_Pool: TThread_RewritePascal_Process_ZDB_File_Pool;
  arry: U_StringArray;
  Trace_Pool: TRewrite_Trace_Pool;
  Custom_PatternHash_: THashStringList;
  pass: Integer;
  Busy: Integer;
begin
  thID := TCompute.CurrentThread.ThreadID;
  th_Post := TThreadPost.Create(thID);
  th_Post.OneStep := False;
  th_Pool := TThread_RewritePascal_Process_ZDB_File_Pool.Create;

  arry := Eng_.GetItemListWithFullPath(directory_);
  Trace_Pool := TRewrite_Trace_Pool.Create;

  for pass := 0 to Length(arry) - 1 do
    begin
      th_Pool.Add(TThread_RewritePascal_Process_ZDB_File.Init_(
        th_Post, Eng_, arry[pass], UnitHash_, PatternHash_, TRewrite_Trace.Create, OnStatus));
    end;

  repeat
    Busy := 0;
    th_Post.Progress(thID);
    if th_Pool.num > 0 then
      with th_Pool.Repeat_ do
        repeat
          if Queue^.Data.Busy then
              Inc(Busy);
        until not Next;
    if Busy > 0 then
        TCompute.Sleep(100);
  until Busy = 0;
  disposeObject(th_Post);

  while th_Pool.num > 0 do
    begin
      if th_Pool.First^.Data.Trace_.IsCode_ then
        begin
          Trace_Pool.Add(th_Pool.First^.Data.Trace_);
        end
      else
          disposeObject(th_Pool.First^.Data.Trace_);
      disposeObject(th_Pool.First^.Data);
      th_Pool.Next;
    end;
  disposeObject(th_Pool);

  // tracert pool
  Th_RewritePascal_ZDB_Include_File_Processor(Eng_, UnitHash_, PatternHash_, Trace_Pool, OnStatus);
  Trace_Pool.Clean;
  disposeObject(Trace_Pool);

  arry := Eng_.GetItemListWithFullPath(directory_);
  for pass := 0 to Length(arry) - 1 do
    begin
      Custom_PatternHash_ := THashStringList.CustomCreate($FFFF);
      if CustomPattern_.Build_Hash_Pool(umlGetUnixFileName(arry[pass]), Custom_PatternHash_) then
          RewritePascal_ZDB_Custom_After_Processor(Eng_, arry[pass], Custom_PatternHash_, True, umlGetUnixFileName(arry[pass]), OnStatus);
      disposeObject(Custom_PatternHash_);
    end;

  arry := Eng_.GetFieldListWithFullPath(directory_);
  for pass := 0 to Length(arry) - 1 do
      Th_RewritePascal_Process_ZDB_Directory(Eng_, arry[pass], UnitHash_, PatternHash_, CustomPattern_, OnStatus);
end;

procedure Th_RewritePascal_Process_ZDB_Directory(Eng_: TObjectDataManager; directory_: U_String; Model_: TMS64; Reverse_: Boolean; OnStatus: TOnRewriteStatus);
var
  dec: TZDB2_File_Decoder;
  fi: TZDB2_FI;
  UnitData_, PatternData_: TSource_Processor_Data_Pool;
  CustomPattern_: TCustom_After_Source_Processor_Data_Pool;
  m64: TMS64;
  UnitHash_, PatternHash_: THashStringList;
  i: Integer;
begin
  if not TZDB2_File_Decoder.Check(Model_) then
      Exit;

  dec := TZDB2_File_Decoder.Create(Model_, 0);
  UnitData_ := TSource_Processor_Data_Pool.Create;
  PatternData_ := TSource_Processor_Data_Pool.Create;
  CustomPattern_ := TCustom_After_Source_Processor_Data_Pool.Create;

  fi := dec.Files.FindFile('Unit');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      UnitData_.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Unit Rewrite Model.');
    end;

  fi := dec.Files.FindFile('Pattern');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      PatternData_.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Symbol Rewrite Model.');
    end;

  fi := dec.Files.FindFile('Custom');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      CustomPattern_.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Custom Rewrite Model.');
    end;

  disposeObject(dec);

  if Reverse_ then
    begin
      for i := 0 to UnitData_.Count - 1 do
        with UnitData_[i]^ do
            OLD_Feature.SwapInstance(New_Feature);

      for i := 0 to PatternData_.Count - 1 do
        with PatternData_[i]^ do
            OLD_Feature.SwapInstance(New_Feature);

      for i := 0 to CustomPattern_.Count - 1 do
        with CustomPattern_[i]^ do
            OLD_Feature.SwapInstance(New_Feature);
    end;

  UnitHash_ := THashStringList.CustomCreate($FFFF);
  UnitData_.Build_Hash_Pool(UnitHash_);
  disposeObject(UnitData_);

  PatternHash_ := THashStringList.CustomCreate($FFFF);
  PatternData_.Build_Hash_Pool(PatternHash_);
  disposeObject(PatternData_);

  Th_RewritePascal_Process_ZDB_Directory(Eng_, directory_, UnitHash_, PatternHash_, CustomPattern_, OnStatus);

  disposeObject(UnitHash_);
  disposeObject(PatternHash_);
  disposeObject(CustomPattern_);
end;

function RewritePascal_Process_ZDB_File(Eng_: TObjectDataManager; fn: U_String;
UnitHash_, PatternHash_: THashStringList; Trace_: TRewrite_Trace;
FileInfo__: SystemString; OnStatus: TOnRewriteStatus): Boolean;
var
  Code: TCore_StringList;
  ft: TDateTime;
  ph, OLD_Feature, New_Feature, new_fn: U_String;
  m64: TMem64;

  procedure Process_AddionalFile(ext_: U_String);
  var
    tmp_old_fn, tmp_new_fn: U_String;
  begin
    tmp_old_fn := umlChangeFileExt(fn, ext_);
    tmp_new_fn := umlChangeFileExt(new_fn, ext_);
    if Eng_.ItemExists(tmp_old_fn) then
      begin
        m64 := TMem64.Create;
        m64.LoadFrom_ZDB_File(Eng_, tmp_old_fn);
        Eng_.ItemDelete(tmp_old_fn);
        m64.SaveTo_ZDB_File(Eng_, tmp_new_fn);
        disposeObject(m64);
        if Assigned(OnStatus) then
            OnStatus(FileInfo__ + 'rename %s -> %s', [tmp_old_fn.Text, tmp_new_fn.Text]);
      end;
  end;

var
  LEncode: TEncoding;

  procedure checkAndLoadFile(fn_: U_String);
  var
    ms64_: TMS64;
  begin
    ms64_ := TMS64.Create;
    ms64_.LoadFrom_ZDB_File(Eng_, fn_);
    if umlBufferIsASCII(ms64_.Memory, ms64_.Size) then
        LEncode := TEncoding.ANSI
    else
        LEncode := TEncoding.UTF8;

    try
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    except
      LEncode := TEncoding.ANSI;
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    end;
    disposeObject(ms64_);
  end;

var
  tmp_m64: TMS64;
begin
  Result := False;
  if Trace_ <> nil then
      Trace_.IsCode_ := False;

  ph := umlGetUnixFilePath(fn);
  OLD_Feature := umlGetUnixFileName(fn);
  New_Feature := UnitHash_.GetDefaultValue(OLD_Feature, OLD_Feature);
  new_fn := umlCombineUnixFileName(ph, New_Feature);
  ft := Eng_.ItemTime(fn);

  if not OLD_Feature.Same(False, New_Feature) then
    begin
      m64 := TMem64.Create;
      m64.LoadFrom_ZDB_File(Eng_, fn);
      Eng_.ItemDelete(fn);
      m64.SaveTo_ZDB_File(Eng_, new_fn);
      disposeObject(m64);
      if Assigned(OnStatus) then
          OnStatus(FileInfo__ + 'rename file %s -> %s', [OLD_Feature.Text, New_Feature.Text]);
      Eng_.SetItemTime(new_fn, ft);
      if Assigned(OnStatus) then
          OnStatus(FileInfo__ + 'rewrite file time %s -> %s', [New_Feature.Text, DateTimeToStr(ft)]);

      if umlMultipleMatch(['*.pas', '*.pp'], OLD_Feature) then
        begin
          Process_AddionalFile('.dfm');
          Process_AddionalFile('.fmx');
          Process_AddionalFile('.lfm');
        end;
      if umlMultipleMatch(['*.dpr'], OLD_Feature) then
          Process_AddionalFile('.dproj');
    end;

  if umlMultipleMatch(['*.pas', '*.dpr', '*.lpr', '*.pp', '*.inc'], fn) then
    begin
      Code := TCore_StringList.Create;
      checkAndLoadFile(new_fn);
      if Trace_ <> nil then
          Trace_.Current_ := new_fn;
      Result := RewritePascal_Process_Code(Code, UnitHash_, PatternHash_, Trace_, FileInfo__, OnStatus);
      if Trace_ <> nil then
          Trace_.IsCode_ := True;
      if Result then
        begin
          tmp_m64 := TMS64.Create;
          try
              Code.SaveToStream(tmp_m64, LEncode);
          except
          end;
          tmp_m64.SaveTo_ZDB_File(Eng_, new_fn);
          disposeObject(tmp_m64);
          if Assigned(OnStatus) then
              OnStatus(FileInfo__ + 'rebuild code %s', [New_Feature.Text]);
          umlSetFileTime(new_fn, ft);
          if Assigned(OnStatus) then
              OnStatus(FileInfo__ + 'rewrite file time %s -> %s', [New_Feature.Text, DateTimeToStr(ft)]);
        end;
      disposeObject(Code);
    end;
end;

procedure RewritePascal_ZDB_Include_File_Processor(Eng_: TObjectDataManager; UnitHash_, PatternHash_: THashStringList; Trace_Pool: TRewrite_Trace_Pool; OnStatus: TOnRewriteStatus);

  function Found_File_From_Include(fn: U_String; Trace_: TRewrite_Trace): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to Trace_.Include_Files_.Count - 1 do
      if fn.Same(umlGetUnixFileName(Trace_.Include_Files_[i])) then
          Exit(True);
    Result := False;
  end;

var
  LEncode: TEncoding;
  Code: TCore_StringList;
  ft: TDateTime;

  procedure checkAndLoadFile(fn_: U_String);
  var
    ms64_: TMS64;
  begin
    ms64_ := TMS64.Create;
    ms64_.LoadFrom_ZDB_File(Eng_, fn_);
    if umlBufferIsASCII(ms64_.Memory, ms64_.Size) then
        LEncode := TEncoding.ANSI
    else
        LEncode := TEncoding.UTF8;

    try
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    except
      LEncode := TEncoding.ANSI;
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    end;
    disposeObject(ms64_);
  end;

var
  i, j: Integer;
  incl_: TRewrite_Trace_Pool;
  fn: U_String;
  incl_Trace: TRewrite_Trace;
  LNum: NativeInt;
  IsUpdate, IsChanged: Boolean;
  N: TP_String;
  tmp_m64: TMS64;
begin
  if Assigned(OnStatus) then
      OnStatus('include after running...', []);
  incl_ := TRewrite_Trace_Pool.Create;

  // build include space
  for i := 0 to Trace_Pool.Count - 1 do
    begin
      fn := umlGetUnixFileName(Trace_Pool[i].Current_);
      if umlMultipleMatch(['*.inc'], fn) then
          incl_.Add(Trace_Pool[i]);
    end;

  // merge include
  IsChanged := False;
  repeat
    IsUpdate := False;
    for i := 0 to incl_.Count - 1 do
      begin
        incl_Trace := incl_[i];
        fn := umlGetUnixFileName(incl_Trace.Current_);
        for j := 0 to Trace_Pool.Count - 1 do
          if Found_File_From_Include(fn, Trace_Pool[j]) then
            begin
              LNum := incl_Trace.marco_hash_.Count;
              Trace_Pool[j].marco_hash_.MergeTo(incl_Trace.marco_hash_);
              if incl_Trace.marco_hash_.Count > LNum then
                begin
                  IsUpdate := True;
                  IsChanged := True;
                end;
            end;
      end;
  until not IsUpdate;

  // rewrite include
  if IsChanged then
    for i := 0 to incl_.Count - 1 do
      begin
        incl_Trace := incl_[i];
        PatternHash_.MergeTo(incl_Trace.marco_hash_);
        ft := Eng_.ItemTime(fn);
        Code := TCore_StringList.Create;
        checkAndLoadFile(incl_Trace.Current_);

        N.Text := Code.Text;
        if Replace_Pascal_Code(N, incl_Trace.marco_hash_, [ttAscii], True, True, 0, 0, PFormat('"%s" ', [incl_Trace.Current_.Text]), OnStatus) then
          begin
            Code.Text := N;
            tmp_m64 := TMS64.Create;
            try
                Code.SaveToStream(tmp_m64, LEncode);
            except
            end;
            tmp_m64.SaveTo_ZDB_File(Eng_, incl_Trace.Current_);
            disposeObject(tmp_m64);

            Eng_.SetItemTime(incl_Trace.Current_, ft);
            if Assigned(OnStatus) then
                OnStatus('rebuild code %s', [incl_Trace.Current_.Text]);
          end;
        N := '';
        disposeObject(Code);
      end;

  disposeObject(incl_);
  if Assigned(OnStatus) then
      OnStatus('include after done.', []);
end;

procedure RewritePascal_ZDB_Custom_After_Processor(Eng_: TObjectDataManager; fn: U_String; Custom_PatternHash_: THashStringList; OnlyWord_: Boolean; FileInfo__: SystemString; OnStatus: TOnRewriteStatus);
var
  LEncode: TEncoding;
  Code: TCore_StringList;
  ft: TDateTime;

  procedure checkAndLoadFile(fn_: U_String);
  var
    ms64_: TMS64;
  begin
    ms64_ := TMS64.Create;
    ms64_.LoadFrom_ZDB_File(Eng_, fn_);
    if umlBufferIsASCII(ms64_.Memory, ms64_.Size) then
        LEncode := TEncoding.ANSI
    else
        LEncode := TEncoding.UTF8;

    try
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    except
      LEncode := TEncoding.ANSI;
      ms64_.Position := 0;
      Code.LoadFromStream(ms64_, LEncode);
    end;
    disposeObject(ms64_);
  end;

var
  N: TP_String;
  tmp_m64: TMS64;
begin
  ft := Eng_.ItemTime(fn);
  Code := TCore_StringList.Create;
  checkAndLoadFile(fn);

  N.Text := Code.Text;
  if Replace_ASCII_Code(N, Custom_PatternHash_, OnlyWord_, True, 0, 0, FileInfo__, OnStatus) then
    begin
      Code.Text := N;
      tmp_m64 := TMS64.Create;
      try
          Code.SaveToStream(tmp_m64, LEncode);
      except
      end;
      tmp_m64.SaveTo_ZDB_File(Eng_, fn);
      disposeObject(tmp_m64);

      Eng_.SetItemTime(fn, ft);
      if Assigned(OnStatus) then
          OnStatus('rebuild code %s', [fn.Text]);
    end;
  N := '';
  disposeObject(Code);
end;

function RewritePascal_Process_ZDB_Directory(Eng_: TObjectDataManager; directory_: U_String; UnitHash_, PatternHash_: THashStringList; CustomPattern_: TCustom_After_Source_Processor_Data_Pool; OnStatus: TOnRewriteStatus): Integer;
var
  arry: U_StringArray;
  num: Integer;
  Trace_Pool: TRewrite_Trace_Pool;
  pass: Integer;
  tmp: TRewrite_Trace;
  Custom_PatternHash_: THashStringList;
begin
  num := 0;
  arry := Eng_.GetItemListWithFullPath(directory_);
  Trace_Pool := TRewrite_Trace_Pool.Create;

  for pass := 0 to Length(arry) - 1 do
    begin
      tmp := TRewrite_Trace.Create;
      if RewritePascal_Process_ZDB_File(Eng_, arry[pass], UnitHash_, PatternHash_, tmp, PFormat('"%s" ', [arry[pass]]), OnStatus) then
          Inc(num);
      if tmp.IsCode_ then
        begin
          Trace_Pool.Add(tmp);
        end
      else
          disposeObject(tmp);
    end;
  RewritePascal_ZDB_Include_File_Processor(Eng_, UnitHash_, PatternHash_, Trace_Pool, OnStatus);

  Trace_Pool.Clean;
  disposeObject(Trace_Pool);

  arry := Eng_.GetItemListWithFullPath(directory_);
  for pass := 0 to Length(arry) - 1 do
    begin
      Custom_PatternHash_ := THashStringList.CustomCreate($FFFF);
      if CustomPattern_.Build_Hash_Pool(umlGetUnixFileName(arry[pass]), Custom_PatternHash_) then
          RewritePascal_ZDB_Custom_After_Processor(Eng_, arry[pass], Custom_PatternHash_, True, umlGetUnixFileName(arry[pass]), OnStatus);
      disposeObject(Custom_PatternHash_);
    end;

  arry := Eng_.GetFieldListWithFullPath(directory_);
  for pass := 0 to Length(arry) - 1 do
      Inc(num, RewritePascal_Process_ZDB_Directory(Eng_, arry[pass], UnitHash_, PatternHash_, CustomPattern_, OnStatus));

  Result := num;
end;

function RewritePascal_Process_ZDB_Directory(Eng_: TObjectDataManager; directory_: U_String; Model_: TMS64; Reverse_: Boolean; OnStatus: TOnRewriteStatus): Integer;
var
  dec: TZDB2_File_Decoder;
  fi: TZDB2_FI;
  UnitData_, PatternData_: TSource_Processor_Data_Pool;
  CustomPattern_: TCustom_After_Source_Processor_Data_Pool;
  m64: TMS64;
  UnitHash_, PatternHash_: THashStringList;
  i: Integer;
begin
  Result := 0;
  if not TZDB2_File_Decoder.Check(Model_) then
      Exit;

  dec := TZDB2_File_Decoder.Create(Model_, 0);
  UnitData_ := TSource_Processor_Data_Pool.Create;
  PatternData_ := TSource_Processor_Data_Pool.Create;
  CustomPattern_ := TCustom_After_Source_Processor_Data_Pool.Create;

  fi := dec.Files.FindFile('Unit');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      UnitData_.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Unit Rewrite Model.');
    end;

  fi := dec.Files.FindFile('Pattern');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      PatternData_.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Symbol Rewrite Model.');
    end;

  fi := dec.Files.FindFile('Custom');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      CustomPattern_.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Custom Rewrite Model.');
    end;

  disposeObject(dec);

  if Reverse_ then
    begin
      for i := 0 to UnitData_.Count - 1 do
        with UnitData_[i]^ do
            OLD_Feature.SwapInstance(New_Feature);

      for i := 0 to PatternData_.Count - 1 do
        with PatternData_[i]^ do
            OLD_Feature.SwapInstance(New_Feature);

      for i := 0 to CustomPattern_.Count - 1 do
        with CustomPattern_[i]^ do
            OLD_Feature.SwapInstance(New_Feature);
    end;

  UnitHash_ := THashStringList.CustomCreate($FFFF);
  UnitData_.Build_Hash_Pool(UnitHash_);
  disposeObject(UnitData_);

  PatternHash_ := THashStringList.CustomCreate($FFFF);
  PatternData_.Build_Hash_Pool(PatternHash_);
  disposeObject(PatternData_);

  Result := RewritePascal_Process_ZDB_Directory(Eng_, directory_, UnitHash_, PatternHash_, CustomPattern_, OnStatus);

  disposeObject(UnitHash_);
  disposeObject(PatternHash_);
  disposeObject(CustomPattern_);
end;

function Load_RewritePascal_Model(Model_: TMS64; UnitData_, PatternData_: TSource_Processor_Data_Pool; CustomPattern_: TCustom_After_Source_Processor_Data_Pool): Boolean;
var
  dec: TZDB2_File_Decoder;
  fi: TZDB2_FI;
  m64: TMS64;
begin
  Result := False;
  if not TZDB2_File_Decoder.Check(Model_) then
      Exit;
  dec := TZDB2_File_Decoder.Create(Model_, 0);

  fi := dec.Files.FindFile('Unit');
  if (UnitData_ <> nil) and (fi <> nil) then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      UnitData_.LoadFromStream(m64);
      disposeObject(m64);
      Result := True;
      DoStatus('Open Unit Rewrite Model.');
    end;

  fi := dec.Files.FindFile('Pattern');
  if (PatternData_ <> nil) and (fi <> nil) then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      PatternData_.LoadFromStream(m64);
      disposeObject(m64);
      Result := True;
      DoStatus('Open Symbol Rewrite Model.');
    end;

  fi := dec.Files.FindFile('Custom');
  if (CustomPattern_ <> nil) and (fi <> nil) then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      CustomPattern_.LoadFromStream(m64);
      disposeObject(m64);
      Result := True;
      DoStatus('Open Custom Rewrite Model.');
    end;

  disposeObject(dec);
end;

function Build_RewritePascal_Model(UnitData_, PatternData_: TSource_Processor_Data_Pool; CustomPattern_: TCustom_After_Source_Processor_Data_Pool): TMS64;
var
  enc: TZDB2_File_Encoder;
  fi: TZDB2_FI;
  tmp: TMS64;
begin
  Result := TMS64.CustomCreate(1024 * 1024);
  enc := TZDB2_File_Encoder.Create(Result, 0);

  if UnitData_ <> nil then
    begin
      tmp := TMS64.Create;
      UnitData_.SaveToStream(tmp, False);
      fi := enc.EncodeFromStream(tmp, 1024, TSelectCompressionMethod.scmZLIB_Max, 4096);
      fi.FileName := 'Unit';
      fi.FimeTime := umlNow;
      disposeObject(tmp);
      DoStatus('%s %s->%s ratio:%d%%',
        [
        'Unit Rewrite Model',
        umlSizeToStr(fi.Size).Text,
        umlSizeToStr(fi.Compressed).Text,
        100 - umlPercentageToInt64(fi.Size, fi.Compressed)]);
    end;

  if PatternData_ <> nil then
    begin
      tmp := TMS64.Create;
      PatternData_.SaveToStream(tmp, False);
      fi := enc.EncodeFromStream(tmp, 1024, TSelectCompressionMethod.scmZLIB_Max, 4096);
      fi.FileName := 'Pattern';
      fi.FimeTime := umlNow;
      disposeObject(tmp);
      DoStatus('%s %s->%s ratio:%d%%',
        [
        'Symbol Rewrite Model',
        umlSizeToStr(fi.Size).Text,
        umlSizeToStr(fi.Compressed).Text,
        100 - umlPercentageToInt64(fi.Size, fi.Compressed)]);
    end;

  if CustomPattern_ <> nil then
    begin
      tmp := TMS64.Create;
      CustomPattern_.SaveToStream(tmp, False);
      fi := enc.EncodeFromStream(tmp, 1024, TSelectCompressionMethod.scmZLIB_Max, 4096);
      fi.FileName := 'Custom';
      fi.FimeTime := umlNow;
      disposeObject(tmp);
      DoStatus('%s %s->%s ratio:%d%%',
        [
        'Custom Model',
        umlSizeToStr(fi.Size).Text,
        umlSizeToStr(fi.Compressed).Text,
        100 - umlPercentageToInt64(fi.Size, fi.Compressed)]);
    end;

  enc.Flush;
  disposeObject(enc);
end;

function Check_RewritePascal_Model(UnitData_, PatternData_: TSource_Processor_Data_Pool; CustomPattern_: TCustom_After_Source_Processor_Data_Pool): Boolean;
  function Check_UnitData_NewName(N: U_String): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to UnitData_.Count - 1 do
      if umlReplaceSum(@UnitData_[i]^.New_Feature, N, True, True, 0, 0, nil) > 0 then
        begin
          DoStatus('conflict %s -> %s', [N.Text, UnitData_[i]^.New_Feature.Text]);
          Exit(False);
        end;
    Result := True;
  end;

var
  i: Integer;
begin
  for i := 0 to PatternData_.Count - 1 do
    if not Check_UnitData_NewName(PatternData_[i]^.New_Feature) then
        Exit(False);
  for i := 0 to CustomPattern_.Count - 1 do
    if not Check_UnitData_NewName(CustomPattern_[i]^.New_Feature) then
        Exit(False);
  Result := True;
end;

constructor TSource_Define_Pool.Create;
begin
  inherited Create;
end;

destructor TSource_Define_Pool.Destroy;
begin
  inherited Destroy;
end;

procedure TSource_Define_Pool.AddFile(SourFile: U_String);
begin
  AddCustom(SourFile, umlGetFileName(SourFile), True);
end;

procedure TSource_Define_Pool.AddCustom(SourFile, NewName: U_String; Overwrite_: Boolean);
var
  i: Integer;
  p: PSource_Define;
  nf1, nf2: U_String;
begin
  if Overwrite_ then
    for i := 0 to Count - 1 do
      begin
        p := items[i];
        nf1 := umlCombineFileName(umlGetFilePath(p^.SourceFile), p^.NewName);
        nf2 := umlChangeFileExt(nf1, '') + '_LIB' + umlGetFileExt(nf1);
        if SourFile.Same(p^.SourceFile, nf1, nf2) then
            Exit;
      end;

  new(p);
  p^.SourceFile := SourFile;
  p^.NewName := NewName;
  Add(p);
end;

function TSource_Define_Pool.ReplaceNewName(OLD_, New_: U_String; OnlyWord, IgnoreCase: Boolean): Integer;
var
  num: Integer;
{$IFDEF FPC}
  procedure fpc_progress_(bPos, ePos: Integer; sour, dest: PPascalString; var Accept: Boolean);
  begin
    Inc(num);
  end;
{$ENDIF FPC}


var
  i: Integer;
  p: PSource_Define;
  N: U_String;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    begin
      p := items[i];
      num := 0;
{$IFDEF FPC}
      N := umlReplace(@p^.NewName, OLD_, New_, OnlyWord, IgnoreCase, 0, 0, nil, fpc_progress_);
{$ELSE FPC}
      N := umlReplace(@p^.NewName, OLD_, New_, OnlyWord, IgnoreCase, 0, 0, nil,
        procedure(bPos, ePos: Integer; sour, dest: PPascalString; var Accept: Boolean)
        begin
          Inc(num);
        end);
{$ENDIF FPC}
      Inc(Result, num);
      if not N.Same(@p^.NewName) then
        begin
          p^.NewName := N;
        end;
    end;
end;

procedure TSource_Define_Pool.Clean;
var
  i: Integer;
  p: PSource_Define;
begin
  for i := 0 to Count - 1 do
    begin
      p := items[i];
      p^.SourceFile := '';
      p^.NewName := '';
      dispose(p);
    end;
  inherited Clear;
end;

procedure TSource_Define_Pool.SaveToStream(stream: TCore_Stream; Foramted_: Boolean);
var
  js: TZ_JsonObject;
  arry: TZ_JsonArray;
  arry_js: TZ_JsonObject;
  i: Integer;
  p: PSource_Define;
begin
  js := TZ_JsonObject.Create;
  if Count > 0 then
    begin
      arry := js.A['Unit'];
      for i := 0 to Count - 1 do
        begin
          p := items[i];
          arry_js := arry.AddObject;
          arry_js.s['Source'] := p^.SourceFile;
          arry_js.s['NewName'] := p^.NewName;
        end;
    end;
  js.SaveToStream(stream, Foramted_);
  js.Free;
end;

procedure TSource_Define_Pool.LoadFromStream(stream: TCore_Stream);
var
  js: TZ_JsonObject;
  arry: TZ_JsonArray;
  arry_js: TZ_JsonObject;
  i: Integer;
begin
  Clean;
  js := TZ_JsonObject.Create;
  js.LoadFromStream(stream);
  arry := js.A['Unit'];
  for i := 0 to arry.Count - 1 do
    begin
      arry_js := arry.O[i];
      AddCustom(arry_js.s['Source'], arry_js.s['NewName'], False);
    end;
  js.Free;
end;

procedure TSource_Define_Pool.SaveToFile(fn: U_String);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  try
    SaveToStream(m64, True);
    m64.SaveToFile(fn);
  finally
      disposeObject(m64);
  end;
end;

procedure TSource_Define_Pool.LoadFromFile(fn: U_String);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  try
    m64.LoadFromFile(fn);
    m64.Position := 0;
  except
    disposeObject(m64);
    Exit;
  end;

  try
      LoadFromStream(m64);
  finally
      disposeObject(m64);
  end;
end;

procedure TSource_Define_Pool.Build_Unit_Processor(Processor: TSource_Processor_Data_Pool);
var
  tmpHash: THashStringList;

  procedure AddKey_(N: U_String);
  var
    N2: U_String;
  begin
    N2 := N + '_LIB';
    N2.First := N2.UpperChar[1];
    tmpHash.Add(N, N2);
  end;

  function Fixed_Pascal_Keyword(N: U_String): U_String;
  begin
    Result := tmpHash.Replace(N, True, True, 0, 0);
  end;

var
  i: Integer;
  p: PSource_Define;
  k: TPascal_Keyword;
begin
  Processor.Clean;
  tmpHash := THashStringList.Create;
  for k := low(TPascal_Keyword) to high(TPascal_Keyword) do
    if Pascal_Keyword_DICT[k].Decl <> '' then
        AddKey_(Pascal_Keyword_DICT[k].Decl);
  for i := 0 to Count - 1 do
    begin
      p := items[i];
      Processor.Add_Feature(umlGetFileName(p^.SourceFile), Fixed_Pascal_Keyword(umlGetFileName(p^.NewName)));
    end;
  disposeObject(tmpHash);
end;

constructor TSource_Processor_Data_Pool.Create;
begin
  inherited Create;
end;

destructor TSource_Processor_Data_Pool.Destroy;
begin
  inherited Destroy;
end;

function TSource_Processor_Data_Pool.Exists_OLD_Feature(OLD_Feature: U_String): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if OLD_Feature.Same(@items[i]^.OLD_Feature) then
        Exit;
  Result := False;
end;

function TSource_Processor_Data_Pool.Remove_OLD_Feature(OLD_Feature: U_String): Integer;
var
  i: Integer;
begin
  Result := 0;
  i := 0;
  while i < Count do
    begin
      if OLD_Feature.Same(@items[i]^.OLD_Feature) then
        begin
          Delete(i);
          Inc(Result);
        end
      else
          Inc(i);
    end;
end;

procedure TSource_Processor_Data_Pool.Add_Feature(OLD_Feature, New_Feature: U_String);
var
  p: PSource_Processor_Data;
begin
  Remove_OLD_Feature(OLD_Feature);
  new(p);
  p^.OLD_Feature := OLD_Feature;
  p^.New_Feature := New_Feature;
  Add(p);
end;

function TSource_Processor_Data_Pool.Replace_OLD_Feature(OLD_, New_: U_String; OnlyWord, IgnoreCase: Boolean): Integer;
var
  num: Integer;
{$IFDEF FPC}
  procedure fpc_progress_(bPos, ePos: Integer; sour, dest: PPascalString; var Accept: Boolean);
  begin
    Inc(num);
  end;
{$ENDIF FPC}


var
  i: Integer;
  p: PSource_Processor_Data;
  N: U_String;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    begin
      p := items[i];
      num := 0;
{$IFDEF FPC}
      N := umlReplace(@p^.OLD_Feature, OLD_, New_, OnlyWord, IgnoreCase, 0, 0, nil, fpc_progress_);
{$ELSE FPC}
      N := umlReplace(@p^.OLD_Feature, OLD_, New_, OnlyWord, IgnoreCase, 0, 0, nil,
        procedure(bPos, ePos: Integer; sour, dest: PPascalString; var Accept: Boolean)
        begin
          Inc(num);
        end);
{$ENDIF FPC}
      Inc(Result, num);
      if not N.Same(@p^.OLD_Feature) then
        begin
          p^.OLD_Feature := N;
        end;
    end;
end;

function TSource_Processor_Data_Pool.Replace_New_Feature(OLD_, New_: U_String; OnlyWord, IgnoreCase: Boolean): Integer;
var
  num: Integer;
{$IFDEF FPC}
  procedure fpc_progress_(bPos, ePos: Integer; sour, dest: PPascalString; var Accept: Boolean);
  begin
    Inc(num);
  end;
{$ENDIF FPC}


var
  i: Integer;
  p: PSource_Processor_Data;
  N: U_String;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    begin
      p := items[i];
      num := 0;
{$IFDEF FPC}
      N := umlReplace(@p^.New_Feature, OLD_, New_, OnlyWord, IgnoreCase, 0, 0, nil, fpc_progress_);
{$ELSE FPC}
      N := umlReplace(@p^.New_Feature, OLD_, New_, OnlyWord, IgnoreCase, 0, 0, nil,
        procedure(bPos, ePos: Integer; sour, dest: PPascalString; var Accept: Boolean)
        begin
          Inc(num);
        end);
{$ENDIF FPC}
      Inc(Result, num);
      if not N.Same(@p^.New_Feature) then
        begin
          p^.New_Feature := N;
        end;
    end;
end;

procedure TSource_Processor_Data_Pool.Import(source: TSource_Processor_Data_Pool);
var
  i: Integer;
  p: PSource_Processor_Data;
begin
  for i := 0 to source.Count - 1 do
    begin
      p := source[i];
      Add_Feature(p^.OLD_Feature, p^.New_Feature);
    end;
end;

procedure TSource_Processor_Data_Pool.Clean;
var
  i: Integer;
  p: PSource_Processor_Data;
begin
  for i := 0 to Count - 1 do
    begin
      p := items[i];
      p^.OLD_Feature := '';
      p^.New_Feature := '';
      dispose(p);
    end;
  inherited Clear;
end;

procedure TSource_Processor_Data_Pool.SaveToStream(stream: TCore_Stream; Foramted_: Boolean);
var
  js: TZ_JsonObject;
  arry: TZ_JsonArray;
  arry_js: TZ_JsonObject;
  i: Integer;
  p: PSource_Processor_Data;
begin
  js := TZ_JsonObject.Create;
  if Count > 0 then
    begin
      arry := js.A['Processor'];
      for i := 0 to Count - 1 do
        begin
          p := items[i];
          arry_js := arry.AddObject;
          arry_js.s['Old'] := p^.OLD_Feature;
          arry_js.s['New'] := p^.New_Feature;
        end;
    end;
  js.SaveToStream(stream, Foramted_);
  js.Free;
end;

procedure TSource_Processor_Data_Pool.LoadFromStream(stream: TCore_Stream);
var
  js: TZ_JsonObject;
  arry: TZ_JsonArray;
  arry_js: TZ_JsonObject;
  i: Integer;
begin
  Clean;
  js := TZ_JsonObject.Create;
  js.LoadFromStream(stream);
  arry := js.A['Processor'];
  for i := 0 to arry.Count - 1 do
    begin
      arry_js := arry.O[i];
      Add_Feature(arry_js.s['Old'], arry_js.s['New']);
    end;
  js.Free;
end;

procedure TSource_Processor_Data_Pool.SaveToFile(fn: U_String);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  try
    SaveToStream(m64, True);
    m64.SaveToFile(fn);
  finally
      disposeObject(m64);
  end;
end;

procedure TSource_Processor_Data_Pool.LoadFromFile(fn: U_String);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  try
    m64.LoadFromFile(fn);
    m64.Position := 0;
  except
    disposeObject(m64);
    Exit;
  end;

  try
      LoadFromStream(m64);
  finally
      disposeObject(m64);
  end;
end;

procedure TSource_Processor_Data_Pool.Build_Hash_Pool(Output: THashStringList);
var
  i: Integer;
  p: PSource_Processor_Data;
begin
  Output.Clear;
  for i := 0 to Count - 1 do
    begin
      p := items[i];
      Output.Add(p^.OLD_Feature, p^.New_Feature);
    end;
end;

constructor TCustom_After_Source_Processor_Data_Pool.Create;
begin
  inherited Create;
end;

destructor TCustom_After_Source_Processor_Data_Pool.Destroy;
begin
  inherited Destroy;
end;

procedure TCustom_After_Source_Processor_Data_Pool.Add_Feature(File_Match, OLD_Feature, New_Feature: U_String);
var
  p: PCustom_After_Source_Processor_Data;
begin
  new(p);
  p^.File_Match := File_Match;
  p^.OLD_Feature := OLD_Feature;
  p^.New_Feature := New_Feature;
  Add(p);
end;

function TCustom_After_Source_Processor_Data_Pool.Replace_OLD_Feature(OLD_, New_: U_String; OnlyWord, IgnoreCase: Boolean): Integer;
var
  num: Integer;
{$IFDEF FPC}
  procedure fpc_progress_(bPos, ePos: Integer; sour, dest: PPascalString; var Accept: Boolean);
  begin
    Inc(num);
  end;
{$ENDIF FPC}


var
  i: Integer;
  p: PCustom_After_Source_Processor_Data;
  N: U_String;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    begin
      p := items[i];
      num := 0;
{$IFDEF FPC}
      N := umlReplace(@p^.OLD_Feature, OLD_, New_, OnlyWord, IgnoreCase, 0, 0, nil, fpc_progress_);
{$ELSE FPC}
      N := umlReplace(@p^.OLD_Feature, OLD_, New_, OnlyWord, IgnoreCase, 0, 0, nil,
        procedure(bPos, ePos: Integer; sour, dest: PPascalString; var Accept: Boolean)
        begin
          Inc(num);
        end);
{$ENDIF FPC}
      Inc(Result, num);
      if not N.Same(@p^.OLD_Feature) then
        begin
          p^.OLD_Feature := N;
        end;
    end;
end;

function TCustom_After_Source_Processor_Data_Pool.Replace_New_Feature(OLD_, New_: U_String; OnlyWord, IgnoreCase: Boolean): Integer;
var
  num: Integer;
{$IFDEF FPC}
  procedure fpc_progress_(bPos, ePos: Integer; sour, dest: PPascalString; var Accept: Boolean);
  begin
    Inc(num);
  end;
{$ENDIF FPC}


var
  i: Integer;
  p: PCustom_After_Source_Processor_Data;
  N: U_String;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    begin
      p := items[i];
      num := 0;
{$IFDEF FPC}
      N := umlReplace(@p^.New_Feature, OLD_, New_, OnlyWord, IgnoreCase, 0, 0, nil, fpc_progress_);
{$ELSE FPC}
      N := umlReplace(@p^.New_Feature, OLD_, New_, OnlyWord, IgnoreCase, 0, 0, nil,
        procedure(bPos, ePos: Integer; sour, dest: PPascalString; var Accept: Boolean)
        begin
          Inc(num);
        end);
{$ENDIF FPC}
      Inc(Result, num);
      if not N.Same(@p^.New_Feature) then
        begin
          p^.New_Feature := N;
        end;
    end;
end;

procedure TCustom_After_Source_Processor_Data_Pool.Clean;
var
  i: Integer;
  p: PCustom_After_Source_Processor_Data;
begin
  for i := 0 to Count - 1 do
    begin
      p := items[i];
      p^.File_Match := '';
      p^.OLD_Feature := '';
      p^.New_Feature := '';
      dispose(p);
    end;
  inherited Clear;
end;

procedure TCustom_After_Source_Processor_Data_Pool.SaveToStream(stream: TCore_Stream; Foramted_: Boolean);
var
  js: TZ_JsonObject;
  arry: TZ_JsonArray;
  arry_js: TZ_JsonObject;
  i: Integer;
  p: PCustom_After_Source_Processor_Data;
begin
  js := TZ_JsonObject.Create;
  if Count > 0 then
    begin
      arry := js.A['Processor'];
      for i := 0 to Count - 1 do
        begin
          p := items[i];
          arry_js := arry.AddObject;
          arry_js.s['Match'] := p^.File_Match;
          arry_js.s['Old'] := p^.OLD_Feature;
          arry_js.s['New'] := p^.New_Feature;
        end;
    end;
  js.SaveToStream(stream, Foramted_);
  js.Free;
end;

procedure TCustom_After_Source_Processor_Data_Pool.LoadFromStream(stream: TCore_Stream);
var
  js: TZ_JsonObject;
  arry: TZ_JsonArray;
  arry_js: TZ_JsonObject;
  i: Integer;
begin
  Clean;
  js := TZ_JsonObject.Create;
  js.LoadFromStream(stream);
  arry := js.A['Processor'];
  for i := 0 to arry.Count - 1 do
    begin
      arry_js := arry.O[i];
      Add_Feature(arry_js.s['Match'], arry_js.s['Old'], arry_js.s['New']);
    end;
  js.Free;
end;

function TCustom_After_Source_Processor_Data_Pool.Build_Hash_Pool(FileName: U_String; Output: THashStringList): Boolean;
var
  fn: U_String;
  i: Integer;
  p: PCustom_After_Source_Processor_Data;
begin
  Output.Clear;
  fn := umlGetFileName(FileName);
  for i := 0 to Count - 1 do
    begin
      p := items[i];
      if umlMultipleMatch(True, p^.File_Match, fn) then
          Output.Add(p^.OLD_Feature, p^.New_Feature);
    end;
  Result := Output.Count > 0;
end;

end.
 
