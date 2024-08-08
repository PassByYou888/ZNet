{ ****************************************************************************** }
{ * json object library for delphi/objfpc                                      * }
{ ****************************************************************************** }

unit Z.Json;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses SysUtils,
{$IFDEF DELPHI}
  Z.Delphi.JsonDataObjects,
{$ELSE DELPHI}
  Z.FPC.GenericList,
  fpjson, jsonparser, jsonscanner,
{$ENDIF DELPHI}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status,
  Z.UnicodeMixedLib,
  Z.MemoryStream,
  Z.Int128;

type
  TZ_JsonObject = class;

{$IFDEF DELPHI}
  TZ_Instance_JsonArray = TJsonArray;
  TZ_Instance_JsonObject = TJsonObject;
{$ELSE DELPHI}
  TZ_Instance_JsonArray = TJsonArray;
  TZ_Instance_JsonObject = TJsonObject;
{$ENDIF DELPHI}

  TZ_JsonBase = class(TCore_Object_Intermediate)
  protected
    FParent: TZ_JsonBase;
    FList: TCore_ObjectList;
  public
    property Parent: TZ_JsonBase read FParent;
    constructor Create(Parent_: TZ_JsonBase); virtual;
    destructor Destroy; override;
  end;

  TZ_JsonArray = class(TZ_JsonBase)
  private
    FInstance: TZ_Instance_JsonArray;
  public
    property Instance: TZ_Instance_JsonArray read FInstance;
    constructor Create(Parent_: TZ_JsonBase); override;
    destructor Destroy; override;

    procedure Clear;
    procedure Delete(Index: Integer);

    procedure Add(const v_: string); overload;
    procedure Add(const v_: TPascalString); overload;
    procedure Add(const v_: Integer); overload;
    procedure Add(const v_: Int64); overload;
    procedure Add(const v_: UInt64); overload;
    procedure Add(const v_: Int128); overload;
    procedure Add(const v_: UInt128); overload;
    procedure AddF(const v_: Double); overload;
    procedure Add(const v_: TDateTime); overload;
    procedure Add(const v_: Boolean); overload;
    function AddArray: TZ_JsonArray;
    function AddObject: TZ_JsonObject; overload;

    procedure Insert(Index: Integer; const v_: string); overload;
    procedure Insert(Index: Integer; const v_: Integer); overload;
    procedure Insert(Index: Integer; const v_: Int64); overload;
    procedure Insert(Index: Integer; const v_: UInt64); overload;
    procedure Insert(Index: Integer; const v_: Int128); overload;
    procedure Insert(Index: Integer; const v_: UInt128); overload;
    procedure Insert(Index: Integer; const v_: Double); overload;
    procedure Insert(Index: Integer; const v_: TDateTime); overload;
    procedure Insert(Index: Integer; const v_: Boolean); overload;
    function InsertArray(Index: Integer): TZ_JsonArray;
    function InsertObject(Index: Integer): TZ_JsonObject; overload;

    function GetString(Index: Integer): string;
    procedure SetString(Index: Integer; const Value: string);
    function GetInt(Index: Integer): Integer;
    procedure SetInt(Index: Integer; const Value: Integer);
    function GetLong(Index: Integer): Int64;
    procedure SetLong(Index: Integer; const Value: Int64);
    function GetULong(Index: Integer): UInt64;
    procedure SetULong(Index: Integer; const Value: UInt64);

    function GetInt128(Index: Integer): Int128;
    procedure SetInt128(Index: Integer; const Value: Int128);
    function GetUInt128(Index: Integer): UInt128;
    procedure SetUInt128(Index: Integer; const Value: UInt128);

    function GetFloat(Index: Integer): Double;
    procedure SetFloat(Index: Integer; const Value: Double);
    function GetDateTime(Index: Integer): TDateTime;
    procedure SetDateTime(Index: Integer; const Value: TDateTime);
    function GetBool(Index: Integer): Boolean;
    procedure SetBool(Index: Integer; const Value: Boolean);
    function GetArray(Index: Integer): TZ_JsonArray;
    function GetObject(Index: Integer): TZ_JsonObject;

    property S[Index: Integer]: string read GetString write SetString;
    property I[Index: Integer]: Integer read GetInt write SetInt;
    property I32[Index: Integer]: Integer read GetInt write SetInt;
    property L[Index: Integer]: Int64 read GetLong write SetLong;
    property I64[Index: Integer]: Int64 read GetLong write SetLong;
    property I128[Index: Integer]: Int128 read GetInt128 write SetInt128;
    property U[Index: Integer]: UInt64 read GetULong write SetULong;
    property U64[Index: Integer]: UInt64 read GetULong write SetULong;
    property U128[Index: Integer]: UInt128 read GetUInt128 write SetUInt128;
    property F[Index: Integer]: Double read GetFloat write SetFloat;
    property D[Index: Integer]: TDateTime read GetDateTime write SetDateTime;
    property B[Index: Integer]: Boolean read GetBool write SetBool;
    property A[Index: Integer]: TZ_JsonArray read GetArray;
    property O[Index: Integer]: TZ_JsonObject read GetObject;

    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

  TZ_JsonObject = class(TZ_JsonBase)
  private
    FInstance: TZ_Instance_JsonObject;
    FTag: Integer;
  public
    property Tag: Integer read FTag write FTag;
    property Instance: TZ_Instance_JsonObject read FInstance;

    constructor Create(); overload;
    constructor Create(Parent_: TZ_JsonBase); overload; override;
    destructor Destroy; override;

    procedure SwapInstance(source_: TZ_JsonObject);
    procedure Assign(source_: TZ_JsonObject);
    function Clone: TZ_JsonObject;

    procedure Clear;
    function IndexOf(const Name: string): Integer;
    function Exists(const Name: string): Boolean;

    function GetInt128(const Name: string): Int128;
    procedure SetInt128(const Name: string; const Value: Int128);
    function GetUInt128(const Name: string): UInt128;
    procedure SetUInt128(const Name: string; const Value: UInt128);
    function GetString(const Name: string): string;
    procedure SetString(const Name, Value: string);
    function GetInt(const Name: string): Integer;
    procedure SetInt(const Name: string; const Value: Integer);
    function GetLong(const Name: string): Int64;
    procedure SetLong(const Name: string; const Value: Int64);
    function GetULong(const Name: string): UInt64;
    procedure SetULong(const Name: string; const Value: UInt64);
    function GetFloat(const Name: string): Double;
    procedure SetFloat(const Name: string; const Value: Double);
    function GetDateTime(const Name: string): TDateTime;
    procedure SetDateTime(const Name: string; const Value: TDateTime);
    function GetBool(const Name: string): Boolean;
    procedure SetBool(const Name: string; const Value: Boolean);
    function GetArray(const Name: string): TZ_JsonArray;
    function GetObject(const Name: string): TZ_JsonObject;

    property S[const Name: string]: string read GetString write SetString;
    property I[const Name: string]: Integer read GetInt write SetInt;
    property I32[const Name: string]: Integer read GetInt write SetInt;
    property L[const Name: string]: Int64 read GetLong write SetLong;
    property I64[const Name: string]: Int64 read GetLong write SetLong;
    property I128[const Name: string]: Int128 read GetInt128 write SetInt128;
    property U[const Name: string]: UInt64 read GetULong write SetULong;
    property U64[const Name: string]: UInt64 read GetULong write SetULong;
    property U128[const Name: string]: UInt128 read GetUInt128 write SetUInt128;
    property F[const Name: string]: Double read GetFloat write SetFloat;
    property D[const Name: string]: TDateTime read GetDateTime write SetDateTime;
    property B[const Name: string]: Boolean read GetBool write SetBool;
    property A[const Name: string]: TZ_JsonArray read GetArray;
    property O[const Name: string]: TZ_JsonObject read GetObject;

    function GetName(Index: Integer): string;
    property Names[Index: Integer]: string read GetName; default;
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function Get_Default_S(const Name, Value: string): string;
    procedure Set_Default_S(const Name, Value: string);

    function GetDefault_S(const Name, Value: string): string;
    procedure SetDefault_S(const Name, Value: string);

    procedure SaveToStream(stream: TCore_Stream; Formated_: Boolean); overload;
    procedure SaveToStream(stream: TCore_Stream); overload;
    procedure LoadFromStream(stream: TCore_Stream);

    procedure SaveToLines(L_: TCore_Strings);
    procedure LoadFromLines(L_: TCore_Strings);
    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);
    procedure LoadFromText(Text_: TPascalString);

    function GetMD5: TMD5;
    property MD5: TMD5 read GetMD5;

    procedure ParseText(Text_: TPascalString);

    function ToJSONString(Formated_: Boolean): TPascalString; overload;
    function ToJSONString: TPascalString; overload;
    property ToJson: TPascalString read ToJSONString;
    class procedure Test;
  end;

  TZ_JsonObject_List_Decl = TGenericsList<TZ_JsonObject>;

  TZ_JsonObject_List = class(TZ_JsonObject_List_Decl)
  public
    AutoFreeObj: Boolean;
    constructor Create(AutoFreeObj_: Boolean);
    destructor Destroy; override;
    function AddFromText(Text_: TPascalString): TZ_JsonObject;
    function AddFromStream(stream: TCore_Stream): TZ_JsonObject;
    function AddFromFile(FileName: U_String): TZ_JsonObject;
    procedure Remove(obj: TZ_JsonObject);
    procedure Delete(Index: Integer);
    procedure Clear;
    procedure Clean;
  end;

  TZJArry = TZ_JsonArray;
  TZJ = TZ_JsonObject;
  TZJList = TZ_JsonObject_List;
  TZJL = TZ_JsonObject_List;

implementation

{$IFDEF DELPHI}
{$I Z.Json_delphi.inc}
{$ELSE DELPHI}
{$I Z.Json_fpc.inc}
{$ENDIF DELPHI}


constructor TZ_JsonBase.Create(Parent_: TZ_JsonBase);
begin
  inherited Create;
  FParent := Parent_;
  if FParent <> nil then
      FParent.FList.Add(self);

  FList := TCore_ObjectList.Create;
  FList.AutoFreeObj := True;
end;

destructor TZ_JsonBase.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

constructor TZ_JsonArray.Create(Parent_: TZ_JsonBase);
begin
  inherited Create(Parent_);
end;

destructor TZ_JsonArray.Destroy;
begin
  inherited Destroy;
end;

procedure TZ_JsonArray.Add(const v_: Int128);
begin
  Add(v_.ToLString);
end;

procedure TZ_JsonArray.Add(const v_: UInt128);
begin
  Add(v_.ToLString);
end;

procedure TZ_JsonArray.Insert(Index: Integer; const v_: Int128);
begin
  Insert(Index, v_.ToLString.Text);
end;

procedure TZ_JsonArray.Insert(Index: Integer; const v_: UInt128);
begin
  Insert(Index, v_.ToLString.Text);
end;

function TZ_JsonArray.GetInt128(Index: Integer): Int128;
begin
  Result := Int128(TPascalString(GetString(index)));
end;

procedure TZ_JsonArray.SetInt128(Index: Integer; const Value: Int128);
begin
  SetString(Index, Value.ToLString.Text);
end;

function TZ_JsonArray.GetUInt128(Index: Integer): UInt128;
begin
  Result := UInt128(TPascalString(GetString(index)));
end;

procedure TZ_JsonArray.SetUInt128(Index: Integer; const Value: UInt128);
begin
  SetString(Index, Value.ToLString.Text);
end;

constructor TZ_JsonObject.Create;
begin
  Create(nil);
end;

constructor TZ_JsonObject.Create(Parent_: TZ_JsonBase);
begin
  inherited Create(Parent_);
  FTag := 0;
  if Parent = nil then
      FInstance := TZ_Instance_JsonObject.Create;
end;

destructor TZ_JsonObject.Destroy;
begin
  if Parent = nil then
      FInstance.Free;
  inherited Destroy;
end;

procedure TZ_JsonObject.SwapInstance(source_: TZ_JsonObject);
var
  bak_FParent: TZ_JsonBase;
  bak_FList: TCore_ObjectList;
  bak_FInstance: TZ_Instance_JsonObject;
  bak_FTag: Integer;
begin
  if FParent <> nil then
      raiseInfo('error.');
  bak_FParent := FParent;
  bak_FList := FList;
  bak_FInstance := FInstance;
  bak_FTag := FTag;

  FParent := source_.FParent;
  FList := source_.FList;
  FInstance := source_.FInstance;
  FTag := source_.FTag;

  source_.FParent := bak_FParent;
  source_.FList := bak_FList;
  source_.FInstance := bak_FInstance;
  source_.FTag := bak_FTag;
end;

procedure TZ_JsonObject.Assign(source_: TZ_JsonObject);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  source_.SaveToStream(m64);
  m64.Position := 0;
  LoadFromStream(m64);
  disposeObject(m64);
end;

function TZ_JsonObject.Clone: TZ_JsonObject;
begin
  Result := TZ_JsonObject.Create;
  Result.Assign(self);
end;

function TZ_JsonObject.Exists(const Name: string): Boolean;
begin
  Result := IndexOf(Name) >= 0;
end;

function TZ_JsonObject.GetInt128(const Name: string): Int128;
begin
  Result := Int128(TPascalString(GetString(Name)));
end;

procedure TZ_JsonObject.SetInt128(const Name: string; const Value: Int128);
begin
  SetString(Name, Value.ToLString);
end;

function TZ_JsonObject.GetUInt128(const Name: string): UInt128;
begin
  Result := UInt128(TPascalString(GetString(Name)));
end;

procedure TZ_JsonObject.SetUInt128(const Name: string; const Value: UInt128);
begin
  SetString(Name, Value.ToLString);
end;

function TZ_JsonObject.Get_Default_S(const Name, Value: string): string;
begin
  if Exists(name) then
      Result := S[name]
  else
      Result := Value;
end;

procedure TZ_JsonObject.Set_Default_S(const Name, Value: string);
begin
  S[name] := Value;
end;

function TZ_JsonObject.GetDefault_S(const Name, Value: string): string;
begin
  if Exists(name) then
      Result := S[name]
  else
      Result := Value;
end;

procedure TZ_JsonObject.SetDefault_S(const Name, Value: string);
begin
  S[name] := Value;
end;

procedure TZ_JsonObject.SaveToStream(stream: TCore_Stream);
begin
  SaveToStream(stream, True);
end;

procedure TZ_JsonObject.SaveToLines(L_: TCore_Strings);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  SaveToStream(m64);
  m64.Position := 0;
{$IFDEF FPC}
  L_.LoadFromStream(m64);
{$ELSE}
  L_.LoadFromStream(m64, TEncoding.UTF8);
{$ENDIF}
  m64.Free;
end;

procedure TZ_JsonObject.LoadFromLines(L_: TCore_Strings);
var
  bak: Boolean;
  m64: TMS64;
begin
  bak := L_.WriteBOM;
  L_.WriteBOM := False;
  m64 := TMS64.Create;
{$IFDEF FPC}
  L_.SaveToStream(m64);
{$ELSE}
  L_.SaveToStream(m64, TEncoding.UTF8);
{$ENDIF}
  L_.WriteBOM := bak;
  m64.Position := 0;
  LoadFromStream(m64);
  m64.Free;
end;

procedure TZ_JsonObject.SaveToFile(FileName: SystemString);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  try
    SaveToStream(m64);
    m64.SaveToFile(FileName);
  finally
      disposeObject(m64);
  end;
end;

procedure TZ_JsonObject.LoadFromFile(FileName: SystemString);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  try
      m64.LoadFromFile(FileName);
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

procedure TZ_JsonObject.LoadFromText(Text_: TPascalString);
var
  buff: TBytes;
  m64: TMS64;
begin
  buff := Text_.Bytes;
  m64 := TMS64.Create;
  m64.Mapping(buff, length(buff));
  LoadFromStream(m64);
  disposeObject(m64);
  SetLength(buff, 0);
end;

function TZ_JsonObject.GetMD5: TMD5;
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  SaveToStream(m64, False);
  Result := umlStreamMD5(m64);
  disposeObject(m64);
end;

procedure TZ_JsonObject.ParseText(Text_: TPascalString);
var
  buff: TBytes;
  m64: TMS64;
begin
  buff := Text_.Bytes;
  m64 := TMS64.Create;
  if length(buff) > 0 then
      m64.SetPointerWithProtectedMode(@buff[0], length(buff));
  LoadFromStream(m64);
  m64.Free;
  SetLength(buff, 0);
end;

function TZ_JsonObject.ToJSONString(Formated_: Boolean): TPascalString;
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  SaveToStream(m64, Formated_);
  Result.Bytes := m64.ToBytes;
  m64.Free;
end;

function TZ_JsonObject.ToJSONString: TPascalString;
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  SaveToStream(m64, True);
  Result.Bytes := m64.ToBytes;
  m64.Free;
end;

class procedure TZ_JsonObject.Test;
var
  js: TZ_JsonObject;
  ii: Integer;
  m64: TMS64;
begin
  js := TZ_JsonObject.Create();
  js.S['abc'] := '123';
  DoStatus(js.S['abc']);

  for ii := 1 to 3 do
      js.A['arry'].Add(ii);

  for ii := 0 to js.A['arry'].Count - 1 do
    begin
      DoStatus(js.A['arry'].I[ii]);
    end;

  js.A['arry'].AddObject.S['tt'] := 'inobj';

  js.O['obj'].S['fff'] := '999';

  DoStatus(js.ToJSONString(True));
  DoStatus('');
  DoStatus(js.O['obj'].ToJSONString(True));

  m64 := TMS64.Create;
  js.SaveToStream(m64);
  m64.Position := 0;
  js.O['obj'].LoadFromStream(m64);
  DoStatus(js.ToJSONString(True));
  js.Free;
end;

constructor TZ_JsonObject_List.Create(AutoFreeObj_: Boolean);
begin
  inherited Create;
  AutoFreeObj := AutoFreeObj_;
end;

destructor TZ_JsonObject_List.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TZ_JsonObject_List.AddFromText(Text_: TPascalString): TZ_JsonObject;
begin
  Result := TZ_JsonObject.Create(nil);
  Result.ParseText(Text_);
  Add(Result);
end;

function TZ_JsonObject_List.AddFromStream(stream: TCore_Stream): TZ_JsonObject;
begin
  Result := TZ_JsonObject.Create(nil);
  Result.LoadFromStream(stream);
  Add(Result);
end;

function TZ_JsonObject_List.AddFromFile(FileName: U_String): TZ_JsonObject;
begin
  Result := TZ_JsonObject.Create(nil);
  Result.LoadFromFile(FileName);
  Add(Result);
end;

procedure TZ_JsonObject_List.Remove(obj: TZ_JsonObject);
begin
  if AutoFreeObj then
      disposeObject(obj);
  inherited Remove(obj);
end;

procedure TZ_JsonObject_List.Delete(Index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      if AutoFreeObj then
          disposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TZ_JsonObject_List.Clear;
var
  I: Integer;
begin
  if AutoFreeObj then
    for I := 0 to Count - 1 do
        disposeObject(Items[I]);
  inherited Clear;
end;

procedure TZ_JsonObject_List.Clean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
      disposeObject(Items[I]);
  inherited Clear;
end;

initialization

end.
