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
    function Diff(const p1, p2: T1_): Single; virtual; abstract;
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
    function Diff(const p1, p2: T1_): Double; virtual; abstract;
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

function TBidirectional_Matched{$IFNDEF FPC}<T1_>{$ENDIF FPC}.Compute_Matched: NativeInt;
var
  tmp_ptr: TData_Pool___.PQueueStruct;
  p_rep, p_rep_2, s_rep: TData_Pool___.TRepeat___;
  min_d, tmp_min_d: Single;
  successed: Boolean;
begin
  Result := 0;

  if (Primary_Pool.Num <= 0) or (Second_Pool.Num <= 0) then
      exit;
  Pair_Pool.L.Clear;

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
            if (p_rep.queue <> p_rep_2.queue) and
              (Diff(tmp_ptr^.Data, p_rep_2.queue^.Data) < min_d) then
              begin
                successed := False;
                break;
              end;
          until not p_rep_2.Next;

          // build matched
          if successed then
            begin
              Pair_Pool.Add_Pair(p_rep.queue^.Data, tmp_ptr^.Data); // matched
              Second_Pool.Remove_P(tmp_ptr); // optimize pool
              inc(Result);
            end;
        end;

      p_rep.Discard; // optimize pool
      // do next
      if not p_rep.Next then
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

function TBidirectional_Matched_D{$IFNDEF FPC}<T1_>{$ENDIF FPC}.Compute_Matched: NativeInt;
var
  tmp_ptr: TData_Pool___.PQueueStruct;
  p_rep, p_rep_2, s_rep: TData_Pool___.TRepeat___;
  min_d, tmp_min_d: Double;
  successed: Boolean;
begin
  Result := 0;

  if (Primary_Pool.Num <= 0) or (Second_Pool.Num <= 0) then
      exit;
  Pair_Pool.L.Clear;

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
            if (p_rep.queue <> p_rep_2.queue) and
              (Diff(tmp_ptr^.Data, p_rep_2.queue^.Data) < min_d) then
              begin
                successed := False;
                break;
              end;
          until not p_rep_2.Next;

          // build matched
          if successed then
            begin
              Pair_Pool.Add_Pair(p_rep.queue^.Data, tmp_ptr^.Data); // matched
              Second_Pool.Remove_P(tmp_ptr); // optimize pool
              inc(Result);
            end;
        end;

      p_rep.Discard; // optimize pool
      // do next
      if not p_rep.Next then
          break;
    end;
end;

end.

