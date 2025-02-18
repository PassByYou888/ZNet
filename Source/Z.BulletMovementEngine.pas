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
{ * bullet movement                                                            * }
{ ****************************************************************************** }
unit Z.BulletMovementEngine;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core,
  Z.Geometry2D, Z.Geometry3D;

type
  TBulletMovementStepData = record
    Position: TVec2;
    Angle: TGeoFloat;
    Index: Integer;
  end;

  IBulletMovementInterface = interface
    function GetBulletPosition: TVec2;
    procedure SetBulletPosition(const Value: TVec2);
    function GetBulletRollAngle: TGeoFloat;
    procedure SetBulletRollAngle(const Value: TGeoFloat);
    procedure StartBulletMovement;
    procedure DoneBulletMovement;
    procedure StartBulletRoll;
    procedure DoneBulletRoll;
    procedure StopBullet;
    procedure PauseBullet;
    procedure ResumeBullet;
    procedure BulletStep(OldStep, NewStep: TBulletMovementStepData);
    procedure BulletProgress(deltaTime: Double);
  end;

  TBulletMovementMode = (bmmBulletMovementPath, bmmStopRollAngle);

  TStepHistoryData = record
    Position: TVec2;
    Angle: TGeoFloat;
  end;

  TBulletMovementStepHistory = TOrderStruct<TStepHistoryData>;

  TBulletMovementEngine = class(TCore_Object_Intermediate)
  private
    FOnInterface: IBulletMovementInterface;
    FSteps: array of TBulletMovementStepData;
    FActive: Boolean;
    FPause: Boolean;
    FMoveSpeed: TGeoFloat;
    FRollSpeed: TGeoFloat;
    FOperationMode: TBulletMovementMode;
    FMaxStepHistoryNum: Integer;
    FStepHistory: TBulletMovementStepHistory;
    FStopRollAngle: TGeoFloat;
    FLastProgressDeltaTime: Double;
    FCurrentPathStepTo: Integer;
    FFromPosition: TVec2;
    FToPosition: TVec2;
    FBulletMovementDone, FRollDone: Boolean;
  protected
    function GetPosition: TVec2;
    procedure SetPosition(const Value: TVec2);
    function GetRollAngle: TGeoFloat;
    procedure SetRollAngle(const Value: TGeoFloat);
    function FirstStep: TBulletMovementStepData;
    function LastStep: TBulletMovementStepData;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(To_: TVec2); overload;
    procedure Start(Paths_: TV2L); overload;
    procedure Start; overload;
    procedure stop;
    procedure Pause;
    procedure Progress(const deltaTime: Double);
    property OnInterface: IBulletMovementInterface read FOnInterface write FOnInterface;
    property Position: TVec2 read GetPosition write SetPosition;
    property RollAngle: TGeoFloat read GetRollAngle write SetRollAngle;
    property IsPause: Boolean read FPause;
    property Active: Boolean read FActive;
    property MoveSpeed: TGeoFloat read FMoveSpeed write FMoveSpeed;
    property RollSpeed: TGeoFloat read FRollSpeed write FRollSpeed;
    property OperationMode: TBulletMovementMode read FOperationMode write FOperationMode;
    property FromPosition: TVec2 read FFromPosition;
    property ToPosition: TVec2 read FToPosition;
    property MaxStepHistoryNum: Integer read FMaxStepHistoryNum write FMaxStepHistoryNum;
    property StepHistory: TBulletMovementStepHistory read FStepHistory;
  end;

implementation

function TBulletMovementEngine.GetPosition: TVec2;
begin
  Result := FOnInterface.GetBulletPosition;
end;

procedure TBulletMovementEngine.SetPosition(const Value: TVec2);
begin
  FOnInterface.SetBulletPosition(Value);
end;

function TBulletMovementEngine.GetRollAngle: TGeoFloat;
begin
  Result := FOnInterface.GetBulletRollAngle;
end;

procedure TBulletMovementEngine.SetRollAngle(const Value: TGeoFloat);
begin
  FOnInterface.SetBulletRollAngle(Value);
end;

function TBulletMovementEngine.FirstStep: TBulletMovementStepData;
begin
  Result := FSteps[0];
end;

function TBulletMovementEngine.LastStep: TBulletMovementStepData;
begin
  Result := FSteps[length(FSteps) - 1];
end;

constructor TBulletMovementEngine.Create;
begin
  inherited Create;
  SetLength(FSteps, 0);
  FOnInterface := nil;
  FActive := False;
  FPause := False;
  FMoveSpeed := 300;
  FRollSpeed := 360;
  FOperationMode := bmmBulletMovementPath;
  FMaxStepHistoryNum := 0;
  FStepHistory := TBulletMovementStepHistory.Create;
  FStopRollAngle := 0;
  FLastProgressDeltaTime := 0;
  FCurrentPathStepTo := -1;
  FFromPosition := NULLPoint;
  FToPosition := NULLPoint;
  FBulletMovementDone := False;
  FRollDone := False;
end;

destructor TBulletMovementEngine.Destroy;
begin
  DisposeObject(FStepHistory);
  SetLength(FSteps, 0);
  FOnInterface := nil;
  inherited Destroy;
end;

procedure TBulletMovementEngine.Start(To_: TVec2);
begin
  if not FActive then
    begin
      SetLength(FSteps, 0);
      FStopRollAngle := CalcAngle(Position, To_);
      FOperationMode := bmmStopRollAngle;
      FActive := True;
      FPause := False;
      FToPosition := To_;
      FOnInterface.StartBulletMovement;
    end;
end;

procedure TBulletMovementEngine.Start(Paths_: TV2L);
var
  i: Integer;
begin
  Paths_.RemoveSame;

  if not FActive then
    begin
      FCurrentPathStepTo := 0;
      FFromPosition := NULLPoint;
      FBulletMovementDone := False;
      FRollDone := False;
      FOperationMode := bmmBulletMovementPath;

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
                Index := i;
              end;
          FPause := False;
          FFromPosition := Position;
          FStopRollAngle := 0;
          FToPosition := Paths_.Last^;
          FOnInterface.StartBulletMovement;
        end;
    end;
end;

procedure TBulletMovementEngine.Start;
begin
  if (FActive) and (FPause) then
    begin
      FPause := False;
      FOnInterface.ResumeBullet;
    end;
end;

procedure TBulletMovementEngine.stop;
begin
  if FActive then
    begin
      SetLength(FSteps, 0);
      FCurrentPathStepTo := 0;
      FFromPosition := NULLPoint;
      FBulletMovementDone := False;
      FRollDone := True;
      FPause := False;
      FActive := False;
      FOperationMode := bmmBulletMovementPath;
      FOnInterface.StopBullet;
    end;
end;

procedure TBulletMovementEngine.Pause;
begin
  if not FPause then
    begin
      FPause := True;
      if FActive then
          FOnInterface.PauseBullet;
    end;
end;

procedure TBulletMovementEngine.Progress(const deltaTime: Double);
var
  CurrentDeltaTime: Double;
  toStep: TBulletMovementStepData;
  FromV, ToV, v: TVec2;
  dt, RT: Double;
  d: TGeoFloat;
  Order_: TStepHistoryData;
begin
  FLastProgressDeltaTime := deltaTime;
  if FActive then
    begin
      CurrentDeltaTime := deltaTime;
      FActive := (length(FSteps) > 0) or (FOperationMode = bmmStopRollAngle);
      if (not FPause) and (FActive) then
        begin
          FOnInterface.BulletProgress(CurrentDeltaTime);

          case FOperationMode of
            bmmStopRollAngle:
              begin
                RollAngle := SmoothAngle(RollAngle, FStopRollAngle, deltaTime * FRollSpeed);
                FActive := not AngleEqual(RollAngle, FStopRollAngle);
              end;
            bmmBulletMovementPath:
              begin
                FromV := Position;

                while True do
                  begin
                    if FBulletMovementDone and FRollDone then
                      begin
                        FActive := False;
                        Break;
                      end;

                    if FBulletMovementDone and not FRollDone then
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
                            FOperationMode := bmmStopRollAngle;
                            FStopRollAngle := LastStep.Angle;
                          end
                        else
                            FActive := False;
                        Break;
                      end;

                    toStep := FSteps[FCurrentPathStepTo];
                    ToV := toStep.Position;
                    FBulletMovementDone := FCurrentPathStepTo >= length(FSteps);

                    if (FRollDone) and (not AngleEqual(RollAngle, toStep.Angle)) then
                        FOnInterface.StartBulletRoll;

                    if (not FRollDone) and (AngleEqual(RollAngle, toStep.Angle)) then
                        FOnInterface.DoneBulletRoll;

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
                                FOnInterface.BulletStep(toStep, FSteps[FCurrentPathStepTo]);
                          end;
                      end
                    else
                      begin
                        // uses roll attenuation
                        RT := AngleRollDistanceDeltaTime(RollAngle, toStep.Angle, FRollSpeed);
                        d := Distance(FromV, ToV);

                        if RT >= CurrentDeltaTime then
                          begin
                            if d > CurrentDeltaTime * FMoveSpeed then
                              begin
                                // position vector dont cross endge
                                v := MovementDistance(FromV, ToV, CurrentDeltaTime * FMoveSpeed);
                                Position := v;
                                RollAngle := SmoothAngle(RollAngle, toStep.Angle, CurrentDeltaTime * FRollSpeed);
                                Break;
                              end
                            else
                              begin
                                // position vector cross endge
                                dt := MovementDistanceDeltaTime(FromV, ToV, FMoveSpeed);
                                v := ToV;
                                Position := v;
                                RollAngle := SmoothAngle(RollAngle, toStep.Angle, dt * FRollSpeed);
                                CurrentDeltaTime := CurrentDeltaTime - dt;
                                FromV := ToV;
                                inc(FCurrentPathStepTo);

                                // trigger event
                                if (FCurrentPathStepTo < length(FSteps)) then
                                    FOnInterface.BulletStep(toStep, FSteps[FCurrentPathStepTo]);
                              end;
                          end
                        else
                          begin
                            // preprocess roll speed attenuation
                            if RT * FMoveSpeed > d then
                              begin
                                // position vector cross endge
                                dt := MovementDistanceDeltaTime(FromV, ToV, FMoveSpeed);
                                v := ToV;
                                Position := v;
                                RollAngle := SmoothAngle(RollAngle, toStep.Angle, dt * FRollSpeed);
                                CurrentDeltaTime := CurrentDeltaTime - dt;
                                FromV := ToV;
                                inc(FCurrentPathStepTo);

                                // trigger event
                                if (FCurrentPathStepTo < length(FSteps)) then
                                    FOnInterface.BulletStep(toStep, FSteps[FCurrentPathStepTo]);
                              end
                            else
                              begin
                                // position vector dont cross endge
                                v := MovementDistance(FromV, ToV, RT * FMoveSpeed);
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
              FCurrentPathStepTo := 0;
              FFromPosition := NULLPoint;
              FBulletMovementDone := False;
              FRollDone := False;
              FOperationMode := bmmBulletMovementPath;
              FOnInterface.DoneBulletMovement;
              FStepHistory.Clear;
            end
          else if FMaxStepHistoryNum > 0 then
            begin
              Order_.Position := Position;
              Order_.Angle := RollAngle;
              FStepHistory.Push(Order_);
              while FStepHistory.Num > FMaxStepHistoryNum do
                  FStepHistory.Next;
            end;
        end;
    end;
end;

end.
 
