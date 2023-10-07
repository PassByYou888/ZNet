{ ****************************************************************************** }
{ * cloud 4.0 Unit Rewrite Tool                                                * }
{ ****************************************************************************** }
unit Z.Net.C4_PascalRewrite_Client;

{$DEFINE FPC_DELPHI_MODE}
{$I ..\Z.Define.inc}

interface

uses Variants, SysUtils,
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib,
  Z.Geometry2D, Z.DFE, Z.ListEngine,
  Z.Parsing, Z.Expression, Z.OpCode,
  Z.Json, Z.HashList.Templet,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth, Z.Net.C4;

type
  TC40_Pascal_Rewrite_Tool = class;

  TPascal_Source_ = class
  public
    FileName: U_String;
    Status: TPascalStringList;
    OldCode: TCore_StringList;
    NewCode: TCore_StringList;
    Enc: TEncoding;
    constructor Create();
    destructor Destroy; override;
  end;

  TPascal_Code_Table_Decl = TGenericsList<TPascal_Source_>;
  TPascal_Rewrite_Tool_CodePool = class;

  TON_Pascal_Rewrite_DoneC = procedure(Sender: TPascal_Rewrite_Tool_CodePool);
  TON_Pascal_Rewrite_DoneM = procedure(Sender: TPascal_Rewrite_Tool_CodePool) of object;
{$IFDEF FPC}
  TON_Pascal_Rewrite_DoneP = procedure(Sender: TPascal_Rewrite_Tool_CodePool) is nested;
{$ELSE FPC}
  TON_Pascal_Rewrite_DoneP = reference to procedure(Sender: TPascal_Rewrite_Tool_CodePool);
{$ENDIF FPC}

  TPascal_Rewrite_Tool_CodePool = class
  private
    procedure Do_Done;
    procedure DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
    procedure DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
  public
    Owner: TC40_Pascal_Rewrite_Tool;
    CodeTable: TPascal_Code_Table_Decl;
    OnResultC: TON_Pascal_Rewrite_DoneC;
    OnResultM: TON_Pascal_Rewrite_DoneM;
    OnResultP: TON_Pascal_Rewrite_DoneP;
    constructor Create(Owner_: TC40_Pascal_Rewrite_Tool);
    destructor Destroy; override;
    procedure AddSource(FileName_: U_String; Code: TCore_Strings);
    procedure AddSourceFile(FileName_: U_String);
    procedure DoRewrite;
  end;

  TC40_Pascal_Rewrite_Tool = class(TC40_Base_NoAuth_Client)
  private
    procedure cmd_Rewrite_Status(Sender: TPeerIO; InData: SystemString);
  public
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
    destructor Destroy; override;
    procedure Progress; override;

    procedure SetDefaultModel;
    procedure UpdateModel(UnitModel_, SymbolModel_: THashStringList);
    function Build_CodePool: TPascal_Rewrite_Tool_CodePool;
  end;

  TC40_Pascal_Rewrite_Tool_List = TGenericsList<TC40_Pascal_Rewrite_Tool>;

implementation

constructor TPascal_Source_.Create;
begin
  inherited Create;
  FileName := '';
  Status := TPascalStringList.Create;
  OldCode := TCore_StringList.Create;
  NewCode := TCore_StringList.Create;
  Enc := TEncoding.Default;
end;

destructor TPascal_Source_.Destroy;
begin
  FileName := '';
  DisposeObject(Status);
  DisposeObject(OldCode);
  DisposeObject(NewCode);
  inherited Destroy;
end;

procedure TPascal_Rewrite_Tool_CodePool.Do_Done;
begin
  try
    if Assigned(OnResultC) then
        OnResultC(Self);
    if Assigned(OnResultM) then
        OnResultM(Self);
    if Assigned(OnResultP) then
        OnResultP(Self);
  except
  end;
end;

procedure TPascal_Rewrite_Tool_CodePool.DoStreamParamEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDFE);
var
  i: integer;
begin
  if SendData.Count + (SendData.Count shr 1) = Result_.Count then
    begin
      i := 0;
      while Result_.R.NotEnd do
        begin
          CodeTable[i].FileName := Result_.R.ReadString;
          Result_.R.ReadPascalStrings(CodeTable[i].Status);
          Result_.R.ReadStrings(CodeTable[i].NewCode);
          inc(i);
        end;
    end;
  Owner.DTNoAuth.PostProgress.PostExecuteM_NP(0, Do_Done);
end;

procedure TPascal_Rewrite_Tool_CodePool.DoStreamFailedEvent(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDFE);
var
  i: integer;
begin
  for i := 0 to CodeTable.Count - 1 do
      CodeTable[i].NewCode.Clear;
  Owner.DTNoAuth.PostProgress.PostExecuteM_NP(0, Do_Done);
end;

constructor TPascal_Rewrite_Tool_CodePool.Create(Owner_: TC40_Pascal_Rewrite_Tool);
begin
  inherited Create;
  Owner := Owner_;
  CodeTable := TPascal_Code_Table_Decl.Create;
  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

destructor TPascal_Rewrite_Tool_CodePool.Destroy;
var
  i: integer;
begin
  for i := 0 to CodeTable.Count - 1 do
      DisposeObject(CodeTable[i]);
  DisposeObject(CodeTable);
  inherited Destroy;
end;

procedure TPascal_Rewrite_Tool_CodePool.AddSource(FileName_: U_String; Code: TCore_Strings);
var
  tmp: TPascal_Source_;
begin
  tmp := TPascal_Source_.Create;
  tmp.FileName := FileName_;
  tmp.OldCode.Assign(Code);
  CodeTable.Add(tmp);
end;

procedure TPascal_Rewrite_Tool_CodePool.AddSourceFile(FileName_: U_String);
var
  tmp: TPascal_Source_;

  procedure checkAndLoadFile(fn_: U_String);
  var
    ms64_: TMS64;
  begin
    ms64_ := TMS64.Create;
    ms64_.LoadFromFile(fn_);
    if umlBufferIsASCII(ms64_.Memory, ms64_.Size) then
        tmp.Enc := TEncoding.ANSI
    else
        tmp.Enc := TEncoding.UTF8;

    try
      ms64_.Position := 0;
      tmp.OldCode.LoadFromStream(ms64_, tmp.Enc);
    except
      tmp.Enc := TEncoding.ANSI;
      ms64_.Position := 0;
      tmp.OldCode.LoadFromStream(ms64_, tmp.Enc);
    end;
    DisposeObject(ms64_);
  end;

begin
  tmp := TPascal_Source_.Create;
  tmp.FileName := FileName_;
  checkAndLoadFile(FileName_);
  CodeTable.Add(tmp);
end;

procedure TPascal_Rewrite_Tool_CodePool.DoRewrite;
var
  d: TDFE;
  i: integer;
begin
  d := TDFE.Create;
  for i := 0 to CodeTable.Count - 1 do
    begin
      d.WriteString(CodeTable[i].FileName);
      d.WriteStrings(CodeTable[i].OldCode);
      CodeTable[i].NewCode.Clear;
      CodeTable[i].Status.Clear;
    end;
  Owner.DTNoAuthClient.SendTunnel.SendStreamCmdM('RewritePascal', d, nil, nil,
    DoStreamParamEvent, DoStreamFailedEvent);
  DisposeObject(d);
end;

procedure TC40_Pascal_Rewrite_Tool.cmd_Rewrite_Status(Sender: TPeerIO; InData: SystemString);
begin
  if not C40_QuietMode then
      DoStatus(InData);
end;

constructor TC40_Pascal_Rewrite_Tool.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  DTNoAuthClient.RecvTunnel.RegisterDirectConsole('Rewrite_Status').OnExecute := cmd_Rewrite_Status;
  DTNoAuth.SendTunnel.SendDataCompressed := True;
end;

destructor TC40_Pascal_Rewrite_Tool.Destroy;
begin
  inherited Destroy;
end;

procedure TC40_Pascal_Rewrite_Tool.Progress;
begin
  inherited Progress;
end;

procedure TC40_Pascal_Rewrite_Tool.SetDefaultModel;
begin
  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('SetDefaultModel');
end;

procedure TC40_Pascal_Rewrite_Tool.UpdateModel(UnitModel_, SymbolModel_: THashStringList);
var
  i: integer;
  p: PHashListData;
  js: TZJ;
  arry: TZ_JsonArray;
  arry_js: TZ_JsonObject;
  m64: TMS64;
  d: TDFE;
begin
  d := TDFE.Create;

  js := TZJ.Create;
  if UnitModel_.HashList.Count > 0 then
    begin
      arry := js.A['Processor'];
      i := 0;
      p := UnitModel_.HashList.FirstPtr;
      while i < UnitModel_.HashList.Count do
        begin
          arry_js := arry.AddObject;
          arry_js.s['Old'] := p^.OriginName;
          arry_js.s['New'] := PHashStringListData(p^.Data)^.V;
          inc(i);
          p := p^.Next;
        end;
    end;
  m64 := TMS64.Create;
  js.SaveToStream(m64, False);
  DisposeObject(js);
  d.WriteStream(m64);
  DisposeObject(m64);

  js := TZJ.Create;
  if SymbolModel_.HashList.Count > 0 then
    begin
      arry := js.A['Processor'];
      i := 0;
      p := SymbolModel_.HashList.FirstPtr;
      while i < SymbolModel_.HashList.Count do
        begin
          arry_js := arry.AddObject;
          arry_js.s['Old'] := p^.OriginName;
          arry_js.s['New'] := PHashStringListData(p^.Data)^.V;
          inc(i);
          p := p^.Next;
        end;
    end;
  m64 := TMS64.Create;
  js.SaveToStream(m64, False);
  DisposeObject(js);
  d.WriteStream(m64);
  DisposeObject(m64);

  DTNoAuthClient.SendTunnel.SendDirectStreamCmd('UpdateModel', d);
  DisposeObject(d);
end;

function TC40_Pascal_Rewrite_Tool.Build_CodePool: TPascal_Rewrite_Tool_CodePool;
begin
  Result := TPascal_Rewrite_Tool_CodePool.Create(Self);
end;

initialization

RegisterC40('Pascal_Rewrite', nil, TC40_Pascal_Rewrite_Tool);

end.
