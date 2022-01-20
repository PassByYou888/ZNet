{ ****************************************************************************** }
{ * IOThread                                                                   * }
{ ****************************************************************************** }
unit Z.IOThread;

{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core;

type
  TIO_Thread_Data = class;
  TIO_Thread_Data_State = (idsDone, idsRunning, idsReady, idsInited);

  TIO_Thread_On_C = procedure(Sender: TIO_Thread_Data);
  TIO_Thread_On_M = procedure(Sender: TIO_Thread_Data) of object;
{$IFDEF FPC}
  TIO_Thread_On_P = procedure(Sender: TIO_Thread_Data) is nested;
{$ELSE FPC}
  TIO_Thread_On_P = reference to procedure(Sender: TIO_Thread_Data);
{$ENDIF FPC}

  TIO_Thread_Data = class
  private
    FState: TIO_Thread_Data_State;
    FOn_C: TIO_Thread_On_C;
    FOn_M: TIO_Thread_On_M;
    FOn_P: TIO_Thread_On_P;
  public
    Data: Pointer;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Process; virtual;
    property On_C: TIO_Thread_On_C read FOn_C write FOn_C;
    property On_M: TIO_Thread_On_M read FOn_M write FOn_M;
    property On_P: TIO_Thread_On_P read FOn_P write FOn_P;
  end;

  TIO_Thread_Queue = {$IFDEF FPC}specialize {$ENDIF FPC} TOrderStruct<TIO_Thread_Data>;

  TIO_Thread_Interface = interface
    function Count(): Integer;
    procedure Enqueue(IOData: TIO_Thread_Data); overload;
    procedure Enqueue_C(IOData: TIO_Thread_Data; Data: Pointer; On_C: TIO_Thread_On_C);
    procedure Enqueue_M(IOData: TIO_Thread_Data; Data: Pointer; On_M: TIO_Thread_On_M);
    procedure Enqueue_P(IOData: TIO_Thread_Data; Data: Pointer; On_P: TIO_Thread_On_P);
    function Dequeue(): TIO_Thread_Data;
    procedure Wait;
  end;

  TIO_Thread = class(TCore_InterfacedObject, TIO_Thread_Interface)
  protected
    FCritical: TCritical;
    FThRunning: TAtomBool;
    FThNum: Integer;
    FQueue: TIO_Thread_Queue;
    FDoneQueue: TIO_Thread_Queue;
    procedure ThRun(Sender: TCompute);
  public
    constructor Create(ThNum_: Integer);
    destructor Destroy; override;
    procedure Reset();
    procedure ThEnd();

    function QueueCount(): Integer;
    function DoneCount(): Integer;
    function Count(): Integer;
    procedure Enqueue(IOData: TIO_Thread_Data); overload;
    procedure Enqueue_C(IOData: TIO_Thread_Data; Data: Pointer; On_C: TIO_Thread_On_C);
    procedure Enqueue_M(IOData: TIO_Thread_Data; Data: Pointer; On_M: TIO_Thread_On_M);
    procedure Enqueue_P(IOData: TIO_Thread_Data; Data: Pointer; On_P: TIO_Thread_On_P);
    function Dequeue(): TIO_Thread_Data;
    procedure Wait;

    class procedure Test();
  end;

  TIO_Direct = class(TCore_InterfacedObject, TIO_Thread_Interface)
  protected
    FQueue: TIO_Thread_Queue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset();

    function Count(): Integer;
    property QueueCount: Integer read Count;
    procedure Enqueue(IOData: TIO_Thread_Data); overload;
    procedure Enqueue_C(IOData: TIO_Thread_Data; Data: Pointer; On_C: TIO_Thread_On_C);
    procedure Enqueue_M(IOData: TIO_Thread_Data; Data: Pointer; On_M: TIO_Thread_On_M);
    procedure Enqueue_P(IOData: TIO_Thread_Data; Data: Pointer; On_P: TIO_Thread_On_P);
    function Dequeue(): TIO_Thread_Data;
    procedure Wait;

    class procedure Test();
  end;

  TPost_ThreadPool = class;

  TPost_Thread = class
  private
    FOwner: TPost_ThreadPool;
    FBindTh: TCompute;
    FPost: TThreadPost;
    FActivted: TAtomBool;
    procedure ThRun(thSender: TCompute);
  public
    constructor Create(Owner_: TPost_ThreadPool);
    destructor Destroy; override;

    property BindTh: TCompute read FBindTh;
    property Post: TThreadPost read FPost;

    procedure PostC1(OnSync: TThreadPost_C1);
    procedure PostC2(Data1: Pointer; OnSync: TThreadPost_C2);
    procedure PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3);
    procedure PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4);

    procedure PostM1(OnSync: TThreadPost_M1);
    procedure PostM2(Data1: Pointer; OnSync: TThreadPost_M2);
    procedure PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3);
    procedure PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4);

    procedure PostP1(OnSync: TThreadPost_P1);
    procedure PostP2(Data1: Pointer; OnSync: TThreadPost_P2);
    procedure PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3);
    procedure PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4);
  end;

  TPost_ThreadPool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TPost_Thread>;

  TPost_ThreadPool = class(TPost_ThreadPool_Decl)
  private
    FCritical: TCritical;
    FQueueOptimized: Boolean;
    FNextID: Integer;
    procedure AddTh(th: TPost_Thread);
    procedure RemoveTh(th: TPost_Thread);
  public
    constructor Create(ThNum_: Integer);
    destructor Destroy; override;
    property QueueOptimized: Boolean read FQueueOptimized write FQueueOptimized;

    function ThNum: Integer;
    function TaskNum: Integer;

    procedure Wait(); overload;
    procedure Wait(th: TPost_Thread); overload;

    function Next_Thread: TPost_Thread;
    function MinLoad_Thread: TPost_Thread;
    function IDLE_Thread: TPost_Thread;

    procedure DoTest_C();
    class procedure Test();
  end;

procedure Test_IOData__C(Sender: TIO_Thread_Data);

implementation

uses Z.Status;

procedure Test_IOData__C(Sender: TIO_Thread_Data);
begin
end;

constructor TIO_Thread_Data.Create;
begin
  inherited Create;
  FState := idsInited;
  FOn_C := nil;
  FOn_M := nil;
  FOn_P := nil;
  Data := nil;
end;

destructor TIO_Thread_Data.Destroy;
begin
  inherited Destroy;
end;

procedure TIO_Thread_Data.Process;
begin
  try
    if Assigned(FOn_C) then
        FOn_C(Self);
    if Assigned(FOn_M) then
        FOn_M(Self);
    if Assigned(FOn_P) then
        FOn_P(Self);
  except
  end;
end;

procedure TIO_Thread.ThRun(Sender: TCompute);
var
  i: Integer;
  d: TIO_Thread_Data;
  LTK, L: TTimeTick;
  p: TIO_Thread_Queue.POrderStruct_;
begin
  AtomInc(FThNum);
  LTK := GetTimeTick();
  while FThRunning.V do
    begin
      FCritical.Lock;
      d := nil;

      while (FQueue.First <> nil) and (FQueue.First^.Data.FState = idsDone) do
        begin
          FDoneQueue.Push(FQueue.First^.Data);
          FQueue.Next;
        end;

      p := FQueue.First;
      while p <> nil do
        begin
          if p^.Data.FState = idsReady then
            begin
              d := p^.Data;
              break;
            end;
          p := p^.Next;
        end;

      if d <> nil then
        begin
          d.FState := idsRunning;
          FCritical.UnLock;
          d.Process;
          FCritical.Lock;
          d.FState := idsDone;
          FCritical.UnLock;
          LTK := GetTimeTick();
        end
      else
        begin
          FCritical.UnLock;
          L := GetTimeTick() - LTK;
          if L > 1000 then
              TCompute.Sleep(1);
        end;
    end;
  AtomDec(FThNum);
end;

constructor TIO_Thread.Create(ThNum_: Integer);
var
  i: Integer;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FThRunning := TAtomBool.Create(True);
  FThNum := 0;
  FQueue := TIO_Thread_Queue.Create;
  FDoneQueue := TIO_Thread_Queue.Create;

  for i := 0 to ThNum_ - 1 do
      TCompute.RunM({$IFDEF FPC}@{$ENDIF FPC}ThRun);
  while FThNum < ThNum_ do
      TCompute.Sleep(1);
end;

destructor TIO_Thread.Destroy;
begin
  ThEnd();
  FCritical.Free;
  FThRunning.Free;
  DisposeObject(FQueue);
  DisposeObject(FDoneQueue);
  inherited Destroy;
end;

procedure TIO_Thread.Reset;
var
  n, i: Integer;
begin
  n := FThNum;
  ThEnd();
  FThNum := 0;
  for i := 0 to n - 1 do
      TCompute.RunM({$IFDEF FPC}@{$ENDIF FPC}ThRun);
  while FThNum < n do
      TCompute.Sleep(1);
end;

procedure TIO_Thread.ThEnd();
begin
  FThRunning.V := False;
  while FThNum > 0 do
      TCompute.Sleep(1);
  FCritical.Lock;
  FQueue.Clear;
  FDoneQueue.Clear;
  FCritical.UnLock;
end;

function TIO_Thread.QueueCount(): Integer;
begin
  FCritical.Lock;
  Result := FQueue.Num;
  FCritical.UnLock;
end;

function TIO_Thread.DoneCount(): Integer;
begin
  FCritical.Lock;
  Result := FDoneQueue.Num;
  FCritical.UnLock;
end;

function TIO_Thread.Count(): Integer;
begin
  FCritical.Lock;
  Result := FQueue.Num + FDoneQueue.Num;
  FCritical.UnLock;
end;

procedure TIO_Thread.Enqueue(IOData: TIO_Thread_Data);
begin
  if IOData.FState <> idsInited then
      RaiseInfo('illegal error.');
  IOData.FState := idsReady;
  FCritical.Lock;
  FQueue.Push(IOData);
  FCritical.UnLock;
end;

procedure TIO_Thread.Enqueue_C(IOData: TIO_Thread_Data; Data: Pointer; On_C: TIO_Thread_On_C);
begin
  IOData.Data := Data;
  IOData.FOn_C := On_C;
  Enqueue(IOData);
end;

procedure TIO_Thread.Enqueue_M(IOData: TIO_Thread_Data; Data: Pointer; On_M: TIO_Thread_On_M);
begin
  IOData.Data := Data;
  IOData.FOn_M := On_M;
  Enqueue(IOData);
end;

procedure TIO_Thread.Enqueue_P(IOData: TIO_Thread_Data; Data: Pointer; On_P: TIO_Thread_On_P);
begin
  IOData.Data := Data;
  IOData.FOn_P := On_P;
  Enqueue(IOData);
end;

function TIO_Thread.Dequeue(): TIO_Thread_Data;
begin
  Result := nil;
  FCritical.Lock;
  if FDoneQueue.Num > 0 then
    begin
      Result := FDoneQueue.First^.Data;
      FDoneQueue.Next;
    end;
  FCritical.UnLock;
end;

procedure TIO_Thread.Wait;
begin
  while Count > 0 do
      TCompute.Sleep(1);
end;

class procedure TIO_Thread.Test();
var
  i, j: Integer;
  d: TIO_Thread_Data;
begin
  with TIO_Thread.Create(CpuCount) do
    begin
      for i := 1 to 1000000 do
          Enqueue_C(TIO_Thread_Data.Create, Pointer(i), {$IFDEF FPC}@{$ENDIF FPC}Test_IOData__C);

      j := 1;
      while Count > 0 do
        begin
          d := Dequeue();
          if d <> nil then
            begin
              if Integer(d.Data) = j then
                  inc(j)
              else
                  DoStatus('IO Thread test error.');
              d.Free;
            end;
        end;
      Free;
    end;
end;

constructor TIO_Direct.Create;
begin
  inherited Create;
  FQueue := TIO_Thread_Queue.Create;
end;

destructor TIO_Direct.Destroy;
begin
  Reset;
  DisposeObject(FQueue);
  inherited Destroy;
end;

procedure TIO_Direct.Reset;
begin
  FQueue.Clear;
end;

function TIO_Direct.Count: Integer;
begin
  Result := FQueue.Num;
end;

procedure TIO_Direct.Enqueue(IOData: TIO_Thread_Data);
begin
  if IOData.FState <> idsInited then
      RaiseInfo('illegal error.');

  IOData.FState := idsRunning;
  IOData.Process;
  IOData.FState := idsDone;
  FQueue.Push(IOData);
end;

procedure TIO_Direct.Enqueue_C(IOData: TIO_Thread_Data; Data: Pointer; On_C: TIO_Thread_On_C);
begin
  IOData.Data := Data;
  IOData.FOn_C := On_C;
  Enqueue(IOData);
end;

procedure TIO_Direct.Enqueue_M(IOData: TIO_Thread_Data; Data: Pointer; On_M: TIO_Thread_On_M);
begin
  IOData.Data := Data;
  IOData.FOn_M := On_M;
  Enqueue(IOData);
end;

procedure TIO_Direct.Enqueue_P(IOData: TIO_Thread_Data; Data: Pointer; On_P: TIO_Thread_On_P);
begin
  IOData.Data := Data;
  IOData.FOn_P := On_P;
  Enqueue(IOData);
end;

function TIO_Direct.Dequeue(): TIO_Thread_Data;
begin
  Result := nil;
  if FQueue.Num > 0 then
    if FQueue.First^.Data.FState = idsDone then
      begin
        Result := FQueue.First^.Data;
        FQueue.Next;
      end;
end;

procedure TIO_Direct.Wait;
begin
end;

class procedure TIO_Direct.Test;
var
  i: Integer;
  d: TIO_Thread_Data;
begin
  with TIO_Direct.Create do
    begin
      for i := 1 to 1000000 do
          Enqueue_C(TIO_Thread_Data.Create, nil, {$IFDEF FPC}@{$ENDIF FPC}Test_IOData__C);
      while Count > 0 do
        begin
          d := Dequeue;
          if d <> nil then
              d.Free;
        end;
      Free;
    end;
end;

procedure TPost_Thread.ThRun(thSender: TCompute);
var
  L: Integer;
  LastTK, IdleTK: TTimeTick;
begin
  FBindTh := thSender;
  FPost := TThreadPost.Create(thSender.ThreadID);
  FPost.OneStep := False;
  FPost.ResetRandomSeed := False;
  FActivted := TAtomBool.Create(True);

  FOwner.AddTh(Self);

  LastTK := GetTimeTick();
  while FActivted.V do
    begin
      L := FPost.Progress(FPost.ThreadID);
      if L > 0 then
          LastTK := GetTimeTick()
      else
        begin
          IdleTK := GetTimeTick() - LastTK;
          if IdleTK > 1000 then
              TCompute.Sleep(1);
        end;
    end;

  FBindTh := nil;
  DisposeObjectAndNil(FPost);
  DisposeObjectAndNil(FActivted);

  FOwner.RemoveTh(Self);
  Free;
end;

constructor TPost_Thread.Create(Owner_: TPost_ThreadPool);
begin
  inherited Create;
  FOwner := Owner_;
  FBindTh := nil;
  FPost := nil;
  FActivted := nil;
  TCompute.RunM(nil, Self, {$IFDEF FPC}@{$ENDIF FPC}ThRun);
end;

destructor TPost_Thread.Destroy;
begin
  inherited Destroy;
end;

procedure TPost_Thread.PostC1(OnSync: TThreadPost_C1);
begin
  FPost.PostC1(OnSync);
end;

procedure TPost_Thread.PostC2(Data1: Pointer; OnSync: TThreadPost_C2);
begin
  FPost.PostC2(Data1, OnSync);
end;

procedure TPost_Thread.PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3);
begin
  FPost.PostC3(Data1, Data2, Data3, OnSync);
end;

procedure TPost_Thread.PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4);
begin
  FPost.PostC4(Data1, Data2, OnSync);
end;

procedure TPost_Thread.PostM1(OnSync: TThreadPost_M1);
begin
  FPost.PostM1(OnSync);
end;

procedure TPost_Thread.PostM2(Data1: Pointer; OnSync: TThreadPost_M2);
begin
  FPost.PostM2(Data1, OnSync);
end;

procedure TPost_Thread.PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3);
begin
  FPost.PostM3(Data1, Data2, Data3, OnSync);
end;

procedure TPost_Thread.PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4);
begin
  FPost.PostM4(Data1, Data2, OnSync);
end;

procedure TPost_Thread.PostP1(OnSync: TThreadPost_P1);
begin
  FPost.PostP1(OnSync);
end;

procedure TPost_Thread.PostP2(Data1: Pointer; OnSync: TThreadPost_P2);
begin
  FPost.PostP2(Data1, OnSync);
end;

procedure TPost_Thread.PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3);
begin
  FPost.PostP3(Data1, Data2, Data3, OnSync);
end;

procedure TPost_Thread.PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4);
begin
  FPost.PostP4(Data1, Data2, OnSync);
end;

procedure TPost_ThreadPool.AddTh(th: TPost_Thread);
begin
  FCritical.Lock;
  Add(th);
  FCritical.UnLock;
end;

procedure TPost_ThreadPool.RemoveTh(th: TPost_Thread);
var
  i: Integer;
begin
  FCritical.Lock;
  i := 0;
  while i < Count do
    begin
      if items[i] = th then
          Delete(i)
      else
          inc(i);
    end;
  FCritical.UnLock;
end;

constructor TPost_ThreadPool.Create(ThNum_: Integer);
var
  i: Integer;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FNextID := 0;
  FQueueOptimized := True;
  for i := 0 to ThNum_ - 1 do
      TPost_Thread.Create(Self);
  while ThNum() < ThNum_ do
      TCompute.Sleep(1);
end;

destructor TPost_ThreadPool.Destroy;
var
  i: Integer;
begin
  FCritical.Lock;
  for i := 0 to Count - 1 do
      items[i].FActivted.V := False;
  FCritical.UnLock;

  while ThNum > 0 do
      TCompute.Sleep(1);

  DisposeObject(FCritical);
  inherited Destroy;
end;

function TPost_ThreadPool.ThNum: Integer;
begin
  FCritical.Lock;
  Result := Count;
  FCritical.UnLock;
end;

function TPost_ThreadPool.TaskNum: Integer;
var
  i: Integer;
begin
  FCritical.Lock;
  Result := 0;
  for i := 0 to Count - 1 do
      inc(Result, items[i].FPost.Count);
  FCritical.UnLock;
end;

procedure TPost_ThreadPool.Wait;
begin
  while TaskNum > 0 do
      TCompute.Sleep(1);
end;

procedure TPost_ThreadPool.Wait(th: TPost_Thread);
begin
  while th.FPost.Count > 0 do
      TCompute.Sleep(1);
end;

function TPost_ThreadPool.Next_Thread: TPost_Thread;
begin
  if ThNum = 0 then
      RaiseInfo('pool is empty.');
  FCritical.Acquire;
  try
    if FNextID >= Count then
        FNextID := 0;
    Result := items[FNextID];
    inc(FNextID);
  finally
      FCritical.Release;
  end;
end;

function TPost_ThreadPool.MinLoad_Thread: TPost_Thread;
var
  i, id_: Integer;
  th: TPost_Thread;
begin
  if ThNum = 0 then
      RaiseInfo('pool is empty.');
  FCritical.Acquire;
  try
    for i := 0 to Count - 1 do
      if (not items[i].FPost.Busy) then
        begin
          if (FQueueOptimized) and (i < Count - 1) then
              Move(i, Count - 1);
          Result := items[i];
          exit;
        end;

    th := items[0];
    id_ := 0;
    for i := 1 to Count - 1 do
      if items[i].FPost.Count < th.FPost.Count then
        begin
          th := items[i];
          id_ := i;
        end;
    if (FQueueOptimized) and (id_ < Count - 1) then
        Move(id_, Count - 1);
    Result := th;
  finally
      FCritical.Release;
  end;
end;

function TPost_ThreadPool.IDLE_Thread: TPost_Thread;
var
  i: Integer;
begin
  if ThNum = 0 then
      RaiseInfo('pool is empty.');
  FCritical.Acquire;
  Result := nil;
  try
    for i := 0 to Count - 1 do
      if not items[i].FPost.Busy then
          exit(items[i]);
  finally
      FCritical.Release;
  end;
end;

procedure TPost_ThreadPool.DoTest_C;
begin
  DoStatus('current post thread: %d', [TCompute.CurrentThread.ThreadID]);
end;

class procedure TPost_ThreadPool.Test;
var
  pool: TPost_ThreadPool;
  i: Integer;
begin
  pool := TPost_ThreadPool.Create(2);
  for i := 0 to 9 do
      pool.Next_Thread.PostM1({$IFDEF FPC}@{$ENDIF FPC}pool.DoTest_C);
  pool.Wait;
  for i := 0 to 9 do
      pool.MinLoad_Thread.PostM1({$IFDEF FPC}@{$ENDIF FPC}pool.DoTest_C);
  pool.Wait;
  DisposeObject(pool);
end;

end.
