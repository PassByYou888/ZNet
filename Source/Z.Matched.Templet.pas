{ ****************************************************************************** }
{ * matched algorithm                                                          * }
{ ****************************************************************************** }
unit Z.Matched.Templet;

{$I Z.Define.inc}

interface

uses Z.Core;

type
  {$IFDEF FPC}generic{$ENDIF FPC}
  TBidirectional_Matched<T1_> = class(TCore_Object)
  public type
    TData_Pool___ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<T1_>;
    TPair_Pool___ = {$IFDEF FPC}specialize {$ENDIF FPC} TPair_Pool<T1_, T1_>;
  public
    Primary_Pool, Second_Pool: TData_Pool___;
    Pair_Pool: TPair_Pool___;
    reject: Single;
    constructor Create(const reject_: Single);
    destructor Destroy; override;
    function Diff(const Primary_, Second_: T1_): Single; virtual; abstract;
    procedure Do_Matched(const Primary_, Second_: T1_); virtual;
    function Compute_Matched(): NativeInt; virtual;
  end;

  {$IFDEF FPC}generic{$ENDIF FPC}
  TBidirectional_Matched_D<T1_> = class(TCore_Object)
  public type
    TData_Pool___ = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<T1_>;
    TPair_Pool___ = {$IFDEF FPC}specialize {$ENDIF FPC} TPair_Pool<T1_, T1_>;
  public
    Primary_Pool, Second_Pool: TData_Pool___;
    Pair_Pool: TPair_Pool___;
    reject: Double;
    constructor Create(const reject_: Double);
    destructor Destroy; override;
    function Diff(const Primary_, Second_: T1_): Double; virtual; abstract;
    procedure Do_Matched(const Primary_, Second_: T1_); virtual;
    function Compute_Matched(): NativeInt; virtual;
  end;

implementation

constructor TBidirectional_Matched{$IFNDEF FPC}<T1_>{$ENDIF FPC}.Create(const reject_: Single);
begin
  inherited Create;
  Primary_Pool := TData_Pool___.Create;
  Second_Pool := TData_Pool___.Create;
  Pair_Pool := TPair_Pool___.Create;
  reject := reject_;
end;

destructor TBidirectional_Matched{$IFNDEF FPC}<T1_>{$ENDIF FPC}.Destroy;
begin
  DisposeObject(Primary_Pool);
  DisposeObject(Second_Pool);
  DisposeObject(Pair_Pool);
  inherited Destroy;
end;

procedure TBidirectional_Matched{$IFNDEF FPC}<T1_>{$ENDIF FPC}.Do_Matched(const Primary_, Second_: T1_);
begin
  Pair_Pool.Add_Pair(Primary_, Second_);
end;

function TBidirectional_Matched{$IFNDEF FPC}<T1_>{$ENDIF FPC}.Compute_Matched: NativeInt;
var
  tmp_ptr: TData_Pool___.PQueueStruct;
  p_rep, p_rep_2, s_rep: TData_Pool___.TRepeat___;
  min_d, tmp_min_d: Single;
  successed: Boolean;
begin
  Result := 0;
  Pair_Pool.L.Clear;

  if (Primary_Pool.Num <= 0) or (Second_Pool.Num <= 0) then
      exit;

  // bidirectional matched algorithm
  p_rep := Primary_Pool.Repeat_;
  while (Primary_Pool.Num > 0) and (Second_Pool.Num > 0) do
    begin
      // 1 linear matched
      tmp_ptr := Second_Pool.First;
      min_d := Diff(p_rep.queue^.Data, tmp_ptr^.Data);
      s_rep := Second_Pool.Repeat_;
      repeat
        tmp_min_d := Diff(p_rep.queue^.Data, s_rep.queue^.Data);
        if tmp_min_d < min_d then
          begin
            tmp_ptr := s_rep.queue;
            min_d := tmp_min_d;
          end;
      until not s_rep.Next;

      // 2 linear matched
      if min_d < reject then
        begin
          successed := True;
          p_rep_2 := Primary_Pool.Repeat_;
          repeat
            if (p_rep.queue <> p_rep_2.queue) and (Diff(p_rep_2.queue^.Data, tmp_ptr^.Data) < min_d) then
                successed := False;
          until (not successed) or (not p_rep_2.Next);

          if successed then
            begin
              Do_Matched(p_rep.queue^.Data, tmp_ptr^.Data); // done
              Second_Pool.Remove_P(tmp_ptr); // optimize pool
              inc(Result);
            end;
        end;

      p_rep.Discard; // optimize pool
      if not p_rep.Next then // do next
          break;
    end;
end;

constructor TBidirectional_Matched_D{$IFNDEF FPC}<T1_>{$ENDIF FPC}.Create(const reject_: Double);
begin
  inherited Create;
  Primary_Pool := TData_Pool___.Create;
  Second_Pool := TData_Pool___.Create;
  Pair_Pool := TPair_Pool___.Create;
  reject := reject_;
end;

destructor TBidirectional_Matched_D{$IFNDEF FPC}<T1_>{$ENDIF FPC}.Destroy;
begin
  DisposeObject(Primary_Pool);
  DisposeObject(Second_Pool);
  DisposeObject(Pair_Pool);
  inherited Destroy;
end;

procedure TBidirectional_Matched_D{$IFNDEF FPC}<T1_>{$ENDIF FPC}.Do_Matched(const Primary_, Second_: T1_);
begin
  Pair_Pool.Add_Pair(Primary_, Second_);
end;

function TBidirectional_Matched_D{$IFNDEF FPC}<T1_>{$ENDIF FPC}.Compute_Matched: NativeInt;
var
  tmp_ptr: TData_Pool___.PQueueStruct;
  p_rep, p_rep_2, s_rep: TData_Pool___.TRepeat___;
  min_d, tmp_min_d: Double;
  successed: Boolean;
begin
  Result := 0;
  Pair_Pool.L.Clear;

  if (Primary_Pool.Num <= 0) or (Second_Pool.Num <= 0) then
      exit;

  // bidirectional matched algorithm
  p_rep := Primary_Pool.Repeat_;
  while (Primary_Pool.Num > 0) and (Second_Pool.Num > 0) do
    begin
      // 1 linear matched
      tmp_ptr := Second_Pool.First;
      min_d := Diff(p_rep.queue^.Data, tmp_ptr^.Data);
      s_rep := Second_Pool.Repeat_;
      repeat
        tmp_min_d := Diff(p_rep.queue^.Data, s_rep.queue^.Data);
        if tmp_min_d < min_d then
          begin
            tmp_ptr := s_rep.queue;
            min_d := tmp_min_d;
          end;
      until not s_rep.Next;

      // 2 linear matched
      if min_d < reject then
        begin
          successed := True;
          p_rep_2 := Primary_Pool.Repeat_;
          repeat
            if (p_rep.queue <> p_rep_2.queue) and (Diff(p_rep_2.queue^.Data, tmp_ptr^.Data) < min_d) then
                successed := False;
          until (not successed) or (not p_rep_2.Next);

          if successed then
            begin
              Do_Matched(p_rep.queue^.Data, tmp_ptr^.Data); // done
              Second_Pool.Remove_P(tmp_ptr); // optimize pool
              inc(Result);
            end;
        end;

      p_rep.Discard; // optimize pool
      if not p_rep.Next then // do next
          break;
    end;
end;

end.
