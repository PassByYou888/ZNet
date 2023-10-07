{ ****************************************************************************** }
{ * movement engine imp                                                        * }
{ ****************************************************************************** }
unit Z.MovementEngine;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses SysUtils, Z.Geometry2D, Z.Core, Math;

type
  TMovementStepData = record
    Position: TVec2;
    Angle: TGeoFloat;
    Index: Integer;
  end;

  IMovementEngineInterface = interface
    function GetPosition: TVec2;
    procedure SetPosition(const Value: TVec2);
    function GetRollAngle: TGeoFloat;
    procedure SetRollAngle(const Value: TGeoFloat);
    procedure DoStartMovement;
    procedure DoMovementDone;
    procedure DoRollMovementStart;
    procedure DoRollMovementOver;
    procedure DoLoop;
    procedure DoStop;
    procedure DoPause;
    procedure DoResume;
    procedure DoMovementStepChange(OldStep, NewStep: TMovementStepData);
  end;

  TMovementOperationMode = (momMovementPath, momStopRollAngle);

  TMovementEngine = class(TCore_Object)
  private
    FOnInterface: IMovementEngineInterface;
    FSteps: array of TMovementStepData;
    FActive: Boolean;
    FPause: Boolean;
    FMoveSpeed: TGeoFloat;
    FRollSpeed: TGeoFloat;
    FRollMoveThreshold: TGeoFloat;
    FOperationMode: TMovementOperationMode;
    FLooped: Boolean;
    FStopRollAngle: TGeoFloat;
    FLastProgressNewTime: Double;
    FLastProgressDeltaTime: Double;
    FCurrentPathStepTo: Integer;
    FFromPosition: TVec2;
    FToPosition: TVec2;
    FMovementDone, FRollDone: Boolean;
  protected
    function GetPosition: TVec2;
    procedure SetPosition(const Value: TVec2);
    function GetRollAngle: TGeoFloat;
    procedure SetRollAngle(const Value: TGeoFloat);
    function FirstStep: TMovementStepData;
    function LastStep: TMovementStepData;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(To_: TVec2); overload;
    procedure Start(Paths_: TV2L); overload;
    procedure Start; overload;
    procedure stop;
    procedure Pause;
    procedure Progress(const deltaTime: Double);
    property OnInterface: IMovementEngineInterface read FOnInterface write FOnInterface;
    property Position: TVec2 read GetPosition write SetPosition;
    property RollAngle: TGeoFloat read GetRollAngle write SetRollAngle;
    property IsPause: Boolean read FPause;
    property Active: Boolean read FActive;
    property MoveSpeed: TGeoFloat read FMoveSpeed write FMoveSpeed;
    property RollSpeed: TGeoFloat read FRollSpeed write FRollSpeed;
    property RollMoveThreshold: TGeoFloat read FRollMoveThreshold write FRollMoveThreshold;
    property OperationMode: TMovementOperationMode read FOperationMode write FOperationMode;
    property Looped: Boolean read FLooped write FLooped;
    property FromPosition: TVec2 read FFromPosition;
    property ToPosition: TVec2 read FToPosition;
  end;

implementation

uses Z.Geometry3D;

function TMovementEngine.GetPosition: TVec2;
begin
  Result := FOnInterface.GetPosition;
end;

procedure TMovementEngine.SetPosition(const Value: TVec2);
begin
  FOnInterface.SetPosition(Value);
end;

function TMovementEngine.GetRollAngle: TGeoFloat;
begin
  Result := FOnInterface.GetRollAngle;
end;

procedure TMovementEngine.SetRollAngle(const Value: TGeoFloat);
begin
  FOnInterface.SetRollAngle(Value);
end;

function TMovementEngine.FirstStep: TMovementStepData;
begin
  Result := FSteps[0];
end;

function TMovementEngine.LastStep: TMovementStepData;
begin
  Result := FSteps[length(FSteps) - 1];
end;

constructor TMovementEngine.Create;
begin
  inherited Create;
  SetLength(FSteps, 0);
  FOnInterface := nil;

  FActive := False;
  FPause := False;
  FMoveSpeed := 100;
  FRollSpeed := 180;
  FRollMoveThreshold := 0.5;
  FOperationMode := momMovementPath;

  FLooped := False;
  FStopRollAngle := 0;

  FLastProgressDeltaTime := 0;

  FCurrentPathStepTo := -1;

  FFromPosition := NULLPoint;
  FToPosition := NULLPoint;

  FMovementDone := False;
  FRollDone := False;
end;

destructor TMovementEngine.Destroy;
begin
  SetLength(FSteps, 0);
  FOnInterface := nil;
  inherited Destroy;
end;

procedure TMovementEngine.Start(To_: TVec2);
begin
  if not FActive then
    begin
      SetLength(FSteps, 0);
      FStopRollAngle := CalcAngle(Position, To_);
      FOperationMode := momStopRollAngle;
      FActive := True;
      FPause := False;
      FToPosition := To_;
      FOnInterface.DoStartMovement;
    end;
end;

procedure TMovementEngine.Start(Paths_: TV2L);
var
  i: Integer;
begin
  Paths_.RemoveSame;

  if not FActive then
    begin
      FCurrentPathStepTo := 0;
      FFromPosition := NULLPoint;
      FMovementDone := False;
      FRollDone := False;
      FOperationMode := momMovementPath;

      FActive := (Paths_ <> nil) and (Paths_.Count > 0) and (FOnInterface <> nil);
      if FActive then
        begin
          SetLength(FSteps, Paths_.Count);
          for i := 0 to Paths_.Count - 1 do
            with FSteps[i] do
              begin
                Position := Paths_[i]^;
                if i > 0 then
                    Angle := CalcAngle(Paths_[i - 1]^, Paths_[i]^)
                else
                    Angle := CalcAngle(Position, Paths_[i]^);
                index := i;
              end;

          FPause := False;
          FFromPosition := Position;

          FStopRollAngle := 0;

          FToPosition := Paths_.Last^;
          FOnInterface.DoStartMovement;
        end;
    end;
end;

procedure TMovementEngine.Start;
begin
  if (FActive) and (FPause) then
    begin
      FPause := False;
      FOnInterface.DoResume;
    end;
end;

procedure TMovementEngine.stop;
begin
  if FActive then
    begin
      SetLength(FSteps, 0);
      FCurrentPathStepTo := 0;
      FFromPosition := NULLPoint;
      FMovementDone := False;
      FRollDone := True;
      FPause := False;
      FActive := False;
      FOperationMode := momMovementPath;
      FOnInterface.DoStop;
    end;
end;

procedure TMovementEngine.Pause;
begin
  if not FPause then
    begin
      FPause := True;
      if FActive then
          FOnInterface.DoPause;
    end;
end;

procedure TMovementEngine.Progress(const deltaTime: Double);
var
  CurrentDeltaTime: Double;
  toStep: TMovementStepData;
  FromV, ToV, v: TVec2;
  dt, RT: Double;
  d: TGeoFloat;
begin
  FLastProgressDeltaTime := deltaTime;
  if FActive then
    begin
      CurrentDeltaTime := deltaTime;
      FActive := (length(FSteps) > 0) or (FOperationMode = momStopRollAngle);
      if (not FPause) and (FActive) then
        begin
          case FOperationMode of
            momStopRollAngle:
              begin
                RollAngle := SmoothAngle(RollAngle, FStopRollAngle, deltaTime * FRollSpeed);
                FActive := not AngleEqual(RollAngle, FStopRollAngle);
              end;
            momMovementPath:
              begin
                FromV := Position;

                while True do
                  begin
                    if FMovementDone and FRollDone then
                      begin
                        FActive := False;
                        Break;
                      end;

                    if FMovementDone and not FRollDone then
                      begin
                        RollAngle := SmoothAngle(RollAngle, LastStep.Angle, deltaTime * FRollSpeed);
                        FRollDone := not AngleEqual(RollAngle, LastStep.Angle);
                        Break;
                      end;

                    if FCurrentPathStepTo >= length(FSteps) then
                      begin
                        v := LastStep.Position;
                        Position := v;
                        if not AngleEqual(RollAngle, LastStep.Angle) then
                          begin
                            FOperationMode := momStopRollAngle;
                            FStopRollAngle := LastStep.Angle;
                          end
                        else
                            FActive := False;
                        Break;
                      end;

                    toStep := FSteps[FCurrentPathStepTo];
                    ToV := toStep.Position;
                    FMovementDone := FCurrentPathStepTo >= length(FSteps);

                    if (FRollDone) and (not AngleEqual(RollAngle, toStep.Angle)) then
                        FOnInterface.DoRollMovementStart;

                    if (not FRollDone) and (AngleEqual(RollAngle, toStep.Angle)) then
                        FOnInterface.DoRollMovementOver;

                    FRollDone := AngleEqual(RollAngle, toStep.Angle);

                    if FRollDone then
                      begin
                        dt := MovementDistanceDeltaTime(FromV, ToV, FMoveSpeed);
                        if dt > CurrentDeltaTime then
                          begin
                            // direct compute
                            v := MovementDistance(FromV, ToV, CurrentDeltaTime * FMoveSpeed);
                            Position := v;
                            Break;
                          end
                        else
                          begin
                            CurrentDeltaTime := CurrentDeltaTime - dt;
                            FromV := ToV;
                            inc(FCurrentPathStepTo);

                            // trigger event
                            if (FCurrentPathStepTo < length(FSteps)) then
                                FOnInterface.DoMovementStepChange(toStep, FSteps[FCurrentPathStepTo]);
                          end;
                      end
                    else
                      begin
                        // uses roll attenuation

                        RT := AngleRollDistanceDeltaTime(RollAngle, toStep.Angle, FRollSpeed);
                        d := Distance(FromV, ToV);

                        if RT >= CurrentDeltaTime then
                          begin
                            if d > CurrentDeltaTime * FMoveSpeed * FRollMoveThreshold then
                              begin
                                // position vector dont cross endge
                                v := MovementDistance(FromV, ToV, CurrentDeltaTime * FMoveSpeed * FRollMoveThreshold);
                                Position := v;
                                RollAngle := SmoothAngle(RollAngle, toStep.Angle, CurrentDeltaTime * FRollSpeed);
                                Break;
                              end
                            else
                              begin
                                // position vector cross endge
                                dt := MovementDistanceDeltaTime(FromV, ToV, FMoveSpeed * FRollMoveThreshold);
                                v := ToV;
                                Position := v;
                                RollAngle := SmoothAngle(RollAngle, toStep.Angle, dt * FRollSpeed);
                                CurrentDeltaTime := CurrentDeltaTime - dt;
                                FromV := ToV;
                                inc(FCurrentPathStepTo);

                                // trigger event
                                if (FCurrentPathStepTo < length(FSteps)) then
                                    FOnInterface.DoMovementStepChange(toStep, FSteps[FCurrentPathStepTo]);
                              end;
                          end
                        else
                          begin
                            // preprocess roll movement speed attenuation
                            if RT * FMoveSpeed * FRollMoveThreshold > d then
                              begin
                                // position vector cross endge
                                dt := MovementDistanceDeltaTime(FromV, ToV, FMoveSpeed * FRollMoveThreshold);
                                v := ToV;
                                Position := v;
                                RollAngle := SmoothAngle(RollAngle, toStep.Angle, dt * FRollSpeed);
                                CurrentDeltaTime := CurrentDeltaTime - dt;
                                FromV := ToV;
                                inc(FCurrentPathStepTo);

                                // trigger event
                                if (FCurrentPathStepTo < length(FSteps)) then
                                    FOnInterface.DoMovementStepChange(toStep, FSteps[FCurrentPathStepTo]);
                              end
                            else
                              begin
                                // position vector dont cross endge
                                v := MovementDistance(FromV, ToV, RT * FMoveSpeed * FRollMoveThreshold);
                                Position := v;
                                RollAngle := toStep.Angle;
                                CurrentDeltaTime := CurrentDeltaTime - RT;
                              end;
                          end;
                      end;
                  end;
              end;
          end;

          if (not FActive) then
            begin
              if (FLooped) and (length(FSteps) > 0) then
                begin
                  FCurrentPathStepTo := 0;
                  FActive := True;
                  FMovementDone := False;
                  FRollDone := False;
                  FOperationMode := momMovementPath;
                  FSteps[0].Angle := CalcAngle(Position, FSteps[0].Position);
                  FOnInterface.DoLoop;
                end
              else
                begin
                  FCurrentPathStepTo := 0;
                  FFromPosition := NULLPoint;
                  FMovementDone := False;
                  FRollDone := False;
                  FOperationMode := momMovementPath;
                  FOnInterface.DoMovementDone;
                end;
            end;
        end;
    end;
end;

end.
