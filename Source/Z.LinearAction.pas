{ ****************************************************************************** }
{ * liner action                                                               * }
{ ****************************************************************************** }
unit Z.LinearAction;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.Status, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Cadencer;

type
  TCoreActionState = (asPlaying, asPause, asStop, asOver);
  TCoreActionStates = set of TCoreActionState;
  TLAction = class;
  TLActionList = class;
  TLAction_Linear = class;

  TLAction = class(TCore_Object_Intermediate)
  private
    State: TCoreActionStates;
  public
    Owner: TLActionList;
    constructor Create(Owner_: TLActionList); virtual;
    destructor Destroy; override;
    procedure Run(); virtual;
    procedure Over(); virtual;
    procedure Done();
    procedure Stop(); virtual;
    procedure Pause(); virtual;
    procedure Progress(deltaTime: Double); virtual;
  end;

  TLActionClass = class of TLAction;

  TLActionList_Decl = TGenericsList<TLAction>;

  TLActionList = class(TCore_Object_Intermediate)
  protected
    FSequenceList: TLActionList_Decl;
    FIndex: Integer;
    FLast: TLAction;
    FCadender: TCadencer;
    procedure Do_CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
  public
    Owner: TLAction_Linear;
    constructor Create(Owner_: TLAction_Linear);
    destructor Destroy; override;
    procedure Clear;
    function Add(ActionClass_: TLActionClass): TLAction;
    procedure Run();
    procedure Over();
    procedure Stop();
    function IsOver(): Boolean;
    function IsStop(): Boolean;
    property Last: TLAction read FLast;
    procedure Progress(deltaTime: Double); overload;
    procedure Progress; overload;
    property List: TLActionList_Decl read FSequenceList;
  end;

  TLActionList_Decl_List_Decl = TGenericsList<TLActionList>;

  TLAction_Linear = class(TCore_Object_Intermediate)
  protected
    FLinear_List: TLActionList_Decl_List_Decl;
    FIndex: Integer;
    FLast: TLActionList;
    FCadender: TCadencer;
    procedure Do_CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
  public
    constructor Create();
    destructor Destroy; override;
    procedure Clear;
    function Add: TLActionList;
    procedure Run();
    procedure Stop();
    procedure Over();
    property Last: TLActionList read FLast;
    procedure Progress(deltaTime: Double); overload;
    procedure Progress; overload;
    property List: TLActionList_Decl_List_Decl read FLinear_List;

    class procedure Test();
  end;

implementation

constructor TLAction.Create(Owner_: TLActionList);
begin
  inherited Create;
  Owner := Owner_;
  State := [];
end;

destructor TLAction.Destroy;
begin
  inherited Destroy;
end;

procedure TLAction.Run;
begin
  State := [asPlaying];
end;

procedure TLAction.Over;
begin
  if asPlaying in State then
      State := [asOver];
end;

procedure TLAction.Done;
begin
  Over();
end;

procedure TLAction.Stop;
begin
  if asPlaying in State then
      State := [asStop];
end;

procedure TLAction.Pause;
begin
  if asPlaying in State then
      State := [asPlaying, asPause];
end;

procedure TLAction.Progress(deltaTime: Double);
begin

end;

procedure TLActionList.Do_CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  Progress(deltaTime);
end;

constructor TLActionList.Create(Owner_: TLAction_Linear);
begin
  inherited Create;
  FSequenceList := TLActionList_Decl.Create;
  FIndex := -1;
  FLast := nil;
  FCadender := nil;
  Owner := Owner_;
end;

destructor TLActionList.Destroy;
begin
  Clear;
  DisposeObjectAndNil(FCadender);
  DisposeObjectAndNil(FSequenceList);
  inherited Destroy;
end;

procedure TLActionList.Clear;
var
  i: Integer;
begin
  for i := FSequenceList.Count - 1 downto 0 do
      DisposeObject(FSequenceList[i]);
  FSequenceList.Clear;
end;

function TLActionList.Add(ActionClass_: TLActionClass): TLAction;
begin
  Result := ActionClass_.Create(Self);
  FSequenceList.Add(Result);
end;

procedure TLActionList.Run();
begin
  if FSequenceList.Count > 0 then
    begin
      FIndex := 0;
      FLast := FSequenceList[FIndex] as TLAction;
    end
  else
    begin
      FIndex := -1;
      FLast := nil;
    end;
end;

procedure TLActionList.Over;
begin
  if FLast <> nil then
    begin
      FIndex := FSequenceList.Count;
      if Owner <> nil then
          Owner.Over;
    end;
end;

procedure TLActionList.Stop;
begin
  if FLast <> nil then
      FIndex := -1;
end;

function TLActionList.IsOver: Boolean;
begin
  Result := FIndex >= FSequenceList.Count;
end;

function TLActionList.IsStop: Boolean;
begin
  Result := FIndex < 0;
end;

procedure TLActionList.Progress(deltaTime: Double);
begin
  if (FIndex < 0) or (FIndex >= FSequenceList.Count) then
      Exit;

  FLast := FSequenceList[FIndex];

  if FLast.State = [] then
    begin
      FLast.Run;
      Exit;
    end;

  if asPlaying in FLast.State then
    begin
      FLast.Progress(deltaTime);
      Exit;
    end;

  if asStop in FLast.State then
    begin
      FIndex := -1;
      if Owner <> nil then
          Owner.Stop;
      Exit;
    end;

  if asOver in FLast.State then
    begin
      inc(FIndex);
      if (FIndex >= FSequenceList.Count) and (Owner <> nil) then
          Owner.Over;
      Exit;
    end;
end;

procedure TLActionList.Progress;
begin
  if FCadender = nil then
    begin
      FCadender := TCadencer.Create;
      FCadender.OnProgress := Do_CadencerProgress;
    end;
  FCadender.Progress;
end;

procedure TLAction_Linear.Do_CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  Progress(deltaTime);
end;

constructor TLAction_Linear.Create();
begin
  inherited Create;
  FLinear_List := TLActionList_Decl_List_Decl.Create;
  FIndex := -1;
  FLast := nil;
  FCadender := nil;
end;

destructor TLAction_Linear.Destroy;
begin
  Clear;
  DisposeObjectAndNil(FCadender);
  DisposeObjectAndNil(FLinear_List);
  inherited Destroy;
end;

procedure TLAction_Linear.Clear;
var
  i: Integer;
begin
  for i := FLinear_List.Count - 1 downto 0 do
      DisposeObject(FLinear_List[i]);
  FLinear_List.Clear;
  FIndex := -1;
  FLast := nil;
end;

function TLAction_Linear.Add: TLActionList;
begin
  Result := TLActionList.Create(Self);
  FLinear_List.Add(Result);
end;

procedure TLAction_Linear.Run;
begin
  if FLinear_List.Count > 0 then
    begin
      FIndex := 0;
      FLast := FLinear_List[FIndex];
    end
  else
    begin
      FIndex := -1;
      FLast := nil;
    end;
end;

procedure TLAction_Linear.Stop;
begin
  Clear;
end;

procedure TLAction_Linear.Over;
begin
  inc(FIndex);
  if FIndex < FLinear_List.Count then
    begin
      FLast := FLinear_List[FIndex];
    end
  else
    begin
      Clear;
    end;
end;

procedure TLAction_Linear.Progress(deltaTime: Double);
begin
  if FLast <> nil then
      FLast.Progress(deltaTime);
end;

procedure TLAction_Linear.Progress;
begin
  if FCadender = nil then
    begin
      FCadender := TCadencer.Create;
      FCadender.OnProgress := Do_CadencerProgress;
    end;
  FCadender.Progress;
end;

class procedure TLAction_Linear.Test();
var
  L: TLActionList;
  i: Integer;
begin
  L := TLActionList.Create(nil);
  for i := 1 to 10 do
      L.Add(TLAction);
  L.Run;
  while True do
    begin
      L.Progress(0.1);
      L.Last.Over;
      if L.IsOver or L.IsStop then
          Break;
    end;
  DisposeObject(L);
end;

end.
