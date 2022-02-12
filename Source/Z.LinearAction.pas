{ ****************************************************************************** }
{ * liner action                                                               * }
{ ****************************************************************************** }
unit Z.LinearAction;

{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.Status, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib;

type
  TCoreActionState = (asPlaying, asPause, asStop, asOver);
  TCoreActionStates = set of TCoreActionState;
  TLAction = class;
  TLActionList = class;
  TLAction_Linear = class;

  TLAction = class
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

  TLActionList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TLAction>;

  TLActionList = class
  protected
    FSequenceList: TLActionList_Decl;
    FIndex: Integer;
    FLast: TLAction;
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
    procedure Progress(deltaTime: Double);
  end;

  TLActionList_Decl_List_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TLActionList>;

  TLAction_Linear = class
  protected
    FSequenceList: TLActionList_Decl_List_Decl;
    FIndex: Integer;
    FLast: TLActionList;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Clear;
    function Add: TLActionList;
    procedure Run();
    procedure Stop();
    procedure Over();
    property Last: TLActionList read FLast;
    procedure Progress(deltaTime: Double);

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

constructor TLActionList.Create(Owner_: TLAction_Linear);
begin
  inherited Create;
  FSequenceList := TLActionList_Decl.Create;
  FIndex := -1;
  FLast := nil;
  Owner := Owner_;
end;

destructor TLActionList.Destroy;
begin
  Clear;
  DisposeObject(FSequenceList);
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

constructor TLAction_Linear.Create();
begin
  inherited Create;
  FSequenceList := TLActionList_Decl_List_Decl.Create;
  FIndex := -1;
  FLast := nil;
end;

destructor TLAction_Linear.Destroy;
begin
  Clear;
  DisposeObject(FSequenceList);
  inherited Destroy;
end;

procedure TLAction_Linear.Clear;
var
  i: Integer;
begin
  for i := FSequenceList.Count - 1 downto 0 do
      DisposeObject(FSequenceList[i]);
  FSequenceList.Clear;
  FIndex := -1;
  FLast := nil;
end;

function TLAction_Linear.Add: TLActionList;
begin
  Result := TLActionList.Create(Self);
  FSequenceList.Add(Result);
end;

procedure TLAction_Linear.Run;
begin
  if FSequenceList.Count > 0 then
    begin
      FIndex := 0;
      FLast := FSequenceList[FIndex];
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
  if FIndex < FSequenceList.Count then
    begin
      FLast := FSequenceList[FIndex];
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
