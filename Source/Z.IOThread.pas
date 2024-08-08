{ ****************************************************************************** }
{ * IO-queue-Thread                                                            * }
{ ****************************************************************************** }
unit Z.IOThread;

{$DEFINE FPC_DELPHI_MODE}
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

  TIO_Thread_Data = class(TCore_Object_Intermediate)
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

  TIO_Thread_Queue = TOrderStruct<TIO_Thread_Data>;

  TIO_Thread_Base = class(TCore_Object_Intermediate)
  public
    function Count(): Integer; virtual; abstract;
    procedure Enqueue(IOData: TIO_Thread_Data); virtual; abstract;
    procedure Enqueue_C(IOData: TIO_Thread_Data; Data: Pointer; On_C: TIO_Thread_On_C); virtual; abstract;
    procedure Enqueue_M(IOData: TIO_Thread_Data; Data: Pointer; On_M: TIO_Thread_On_M); virtual; abstract;
    procedure Enqueue_P(IOData: TIO_Thread_Data; Data: Pointer; On_P: TIO_Thread_On_P); virtual; abstract;
    function Dequeue(): TIO_Thread_Data; virtual; abstract;
    procedure Wait; virtual; abstract;
  end;

  TIO_Thread = class(TIO_Thread_Base)
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
    function Count(): Integer; override;
    procedure Enqueue(IOData: TIO_Thread_Data); override;
    procedure Enqueue_C(IOData: TIO_Thread_Data; Data: Pointer; On_C: TIO_Thread_On_C); override;
    procedure Enqueue_M(IOData: TIO_Thread_Data; Data: Pointer; On_M: TIO_Thread_On_M); override;
    procedure Enqueue_P(IOData: TIO_Thread_Data; Data: Pointer; On_P: TIO_Thread_On_P); override;
    function Dequeue(): TIO_Thread_Data; override;
    procedure Wait; override;

    class procedure Test();
  end;

  TIO_Direct = class(TIO_Thread_Base)
  protected
    FCritical: TCritical;
    FQueue: TIO_Thread_Queue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset();

    function Count(): Integer; override;
    property QueueCount: Integer read Count;
    procedure Enqueue(IOData: TIO_Thread_Data); override;
    procedure Enqueue_C(IOData: TIO_Thread_Data; Data: Pointer; On_C: TIO_Thread_On_C); override;
    procedure Enqueue_M(IOData: TIO_Thread_Data; Data: Pointer; On_M: TIO_Thread_On_M); override;
    procedure Enqueue_P(IOData: TIO_Thread_Data; Data: Pointer; On_P: TIO_Thread_On_P); override;
    function Dequeue(): TIO_Thread_Data; override;
    procedure Wait; override;

    class procedure Test();
  end;

  TThread_Event_Pool__ = class;

  TThread_Pool_Decl = TBigList<TThread_Event_Pool__>;

  TThread_Pool = class(TThread_Pool_Decl)
  private
    FCritical: TCritical;
    FQueueOptimized: Boolean;
  public
    constructor Create(ThNum_: Integer);
    destructor Destroy; override;
    property QueueOptimized: Boolean read FQueueOptimized write FQueueOptimized;

    function ThNum: NativeInt;
    function TaskNum: NativeInt;

    procedure Wait(); overload;
    procedure Wait(Th: TThread_Event_Pool__); overload;

    function Next_Thread: TThread_Event_Pool__;
    function MinLoad_Thread: TThread_Event_Pool__;

    procedure DoTest_C();
    class procedure Test();
  end;

  TThread_Event_Pool__ = class(TCore_Object_Intermediate)
  private
    FOwner: TThread_Pool;
    FBindTh: TCompute;
    FPost: TThreadPost;
    FActivted: TAtomBool;
    FPool_Data_Ptr: TThread_Pool_Decl.PQueueStruct;
    procedure ThRun(ThSender: TCompute);
  public
    constructor Create(Owner_: TThread_Pool);
    destructor Destroy; override;

    property Post: TThreadPost read FPost;
    // post thread Call
    procedure PostC1(OnSync: TThreadPost_C1); overload;
    procedure PostC1(OnSync: TThreadPost_C1; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostC2(Data1: Pointer; OnSync: TThreadPost_C2); overload;
    procedure PostC2(Data1: Pointer; OnSync: TThreadPost_C2; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3); overload;
    procedure PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4); overload;
    procedure PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4; IsRuning_, IsExit_: PBoolean); overload;
    // post thread Method
    procedure PostM1(OnSync: TThreadPost_M1); overload;
    procedure PostM1(OnSync: TThreadPost_M1; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostM2(Data1: Pointer; OnSync: TThreadPost_M2); overload;
    procedure PostM2(Data1: Pointer; OnSync: TThreadPost_M2; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3); overload;
    procedure PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4); overload;
    procedure PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4; IsRuning_, IsExit_: PBoolean); overload;
    // post thread Proc
    procedure PostP1(OnSync: TThreadPost_P1); overload;
    procedure PostP1(OnSync: TThreadPost_P1; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostP2(Data1: Pointer; OnSync: TThreadPost_P2); overload;
    procedure PostP2(Data1: Pointer; OnSync: TThreadPost_P2; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3); overload;
    procedure PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3; IsRuning_, IsExit_: PBoolean); overload;
    procedure PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4); overload;
    procedure PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4; IsRuning_, IsExit_: PBoolean); overload;
  end;

procedure Test_IOData__C(Sender: TIO_Thread_Data);

implementation

uses Z.Notify;

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
        FOn_C(Self)
    else if Assigned(FOn_M) then
        FOn_M(Self)
    else if Assigned(FOn_P) then
        FOn_P(Self);
  except
  end;
end;

procedure TIO_Thread.ThRun(Sender: TCompute);
var
  d: TIO_Thread_Data;
  LTK, L: TTimeTick;
  p: TIO_Thread_Queue.POrderStruct;
begin
  Sender.Thread_Info := ClassName;

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
          if L > 100 then
              TCompute.Sleep(10)
          else
              TCompute.Sleep(1);
        end;
    end;
  AtomDec(FThNum);
end;

constructor TIO_Thread.Create(ThNum_: Integer);
var
  n, i: Integer;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FThRunning := TAtomBool.Create(True);
  FThNum := 0;
  FQueue := TIO_Thread_Queue.Create;
  FDoneQueue := TIO_Thread_Queue.Create;

  n := if_(ThNum_ < 2, 1, ThNum_);

  for i := 0 to n - 1 do
      TCompute.RunM(ThRun);
  while FThNum < n do
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
      TCompute.RunM(ThRun);
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
  with TIO_Thread.Create(Get_Parallel_Granularity) do
    begin
      for i := 1 to 1000000 do
          Enqueue_C(TIO_Thread_Data.Create, Pointer(i), Test_IOData__C);

      j := 1;
      while Count > 0 do
        begin
          d := Dequeue();
          if d <> nil then
            begin
              if Integer(d.Data) = j then
                  inc(j)
              else
                  RaiseInfo('IO Thread test error.');
              d.Free;
            end;
        end;
      Free;
    end;
end;

constructor TIO_Direct.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FQueue := TIO_Thread_Queue.Create;
end;

destructor TIO_Direct.Destroy;
begin
  Reset;
  FCritical.Free;
  DisposeObject(FQueue);
  inherited Destroy;
end;

procedure TIO_Direct.Reset;
begin
  FCritical.Lock;
  FQueue.Clear;
  FCritical.UnLock;
end;

function TIO_Direct.Count: Integer;
begin
  FCritical.Lock;
  Result := FQueue.Num;
  FCritical.UnLock;
end;

procedure TIO_Direct.Enqueue(IOData: TIO_Thread_Data);
begin
  if IOData.FState <> idsInited then
      RaiseInfo('illegal error.');

  IOData.FState := idsRunning;
  IOData.Process;
  IOData.FState := idsDone;
  FCritical.Lock;
  FQueue.Push(IOData);
  FCritical.UnLock;
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
  FCritical.Lock;
  if FQueue.Num > 0 then
    begin
      if FQueue.First^.Data.FState = idsDone then
        begin
          Result := FQueue.First^.Data;
          FQueue.Next;
        end;
    end;
  FCritical.UnLock;
end;

procedure TIO_Direct.Wait;
begin
  while Count > 0 do
      TCompute.Sleep(1);
end;

class procedure TIO_Direct.Test;
var
  i: Integer;
  d: TIO_Thread_Data;
begin
  with TIO_Direct.Create do
    begin
      for i := 1 to 1000000 do
          Enqueue_C(TIO_Thread_Data.Create, nil, Test_IOData__C);
      while Count > 0 do
        begin
          d := Dequeue;
          if d <> nil then
              d.Free;
        end;
      Free;
    end;
end;

constructor TThread_Pool.Create(ThNum_: Integer);
var
  i: Integer;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FQueueOptimized := True;

  for i := 0 to ThNum_ - 1 do
      TThread_Event_Pool__.Create(Self);

  while ThNum() < ThNum_ do
      TCompute.Sleep(1);
end;

destructor TThread_Pool.Destroy;
var
  __Repeat__: TThread_Pool_Decl.TRepeat___;
begin
  Wait();

  if Num > 0 then
    begin
      FCritical.Lock;
      __Repeat__ := Repeat_;
      repeat
          __Repeat__.Queue^.Data.FActivted.V := False;
      until not __Repeat__.Next;
      FCritical.UnLock;
    end;

  while ThNum > 0 do
      TCompute.Sleep(1);

  DisposeObject(FCritical);
  inherited Destroy;
end;

function TThread_Pool.ThNum: NativeInt;
begin
  FCritical.Lock;
  Result := Num;
  FCritical.UnLock;
end;

function TThread_Pool.TaskNum: NativeInt;
var
  R_: NativeInt;
begin
  R_ := 0;
  FCritical.Lock;
  try
    if Num > 0 then
      with Repeat_ do
        repeat
            inc(R_, Queue^.Data.FPost.Num);
        until not Next;
  finally
      FCritical.UnLock;
  end;
  Result := R_;
end;

procedure TThread_Pool.Wait;
begin
  while TaskNum > 0 do
      TCompute.Sleep(1);
end;

procedure TThread_Pool.Wait(Th: TThread_Event_Pool__);
begin
  while Th.FPost.Count > 0 do
      TCompute.Sleep(1);
end;

function TThread_Pool.Next_Thread: TThread_Event_Pool__;
begin
  Result := nil;
  if Num > 0 then
    begin
      FCritical.Acquire;
      try
        Result := First^.Data;
        MoveToLast(First);
      finally
          FCritical.Release;
      end;
    end;
end;

function TThread_Pool.MinLoad_Thread: TThread_Event_Pool__;
var
  Eng_: PQueueStruct;
begin
  Result := nil;
  if Num > 0 then
    begin
      FCritical.Acquire;
      try
        Eng_ := nil;
        with Repeat_ do
          repeat
            if Eng_ = nil then
                Eng_ := Queue
            else if Queue^.Data.FPost.Num < Eng_^.Data.FPost.Num then
                Eng_ := Queue;
          until not Next;
        if Eng_ <> nil then
          begin
            Result := Eng_^.Data;
            MoveToLast(First);
          end;
      finally
          FCritical.Release;
      end;
    end;
end;

procedure TThread_Pool.DoTest_C;
begin
  TCompute.Sleep(16);
end;

class procedure TThread_Pool.Test;
var
  pool: TThread_Pool;
  i: Integer;
begin
  pool := TThread_Pool.Create(2);
  for i := 0 to 9 do
      pool.Next_Thread.PostM1(pool.DoTest_C);
  pool.Wait;
  for i := 0 to 9 do
      pool.MinLoad_Thread.PostM1(pool.DoTest_C);
  pool.Wait;
  DisposeObject(pool);
end;

procedure TThread_Event_Pool__.ThRun(ThSender: TCompute);
var
  L: NativeInt;
  Last_TK, IDLE_TK: TTimeTick;
begin
  ThSender.Thread_Info := ClassName;
  FBindTh := ThSender;
  FPost := TThreadPost.Create(ThSender.ThreadID);
  FPost.OneStep := False;
  FPost.ResetRandomSeed := False;
  FActivted := TAtomBool.Create(True);

  FOwner.FCritical.Lock;
  FPool_Data_Ptr := FOwner.Add(Self);
  FOwner.FCritical.UnLock;

  Last_TK := GetTimeTick();
  while FActivted.V do
    begin
      L := FPost.Progress(FPost.ThreadID);
      if L > 0 then
          Last_TK := GetTimeTick()
      else
        begin
          IDLE_TK := GetTimeTick() - Last_TK;
          if IDLE_TK > 1000 then
              TCompute.Sleep(100)
          else
              TCompute.Sleep(1);
        end;
    end;

  FOwner.FCritical.Lock;
  FOwner.Remove_P(FPool_Data_Ptr);
  FOwner.FCritical.UnLock;

  FBindTh := nil;
  DisposeObjectAndNil(FPost);
  DisposeObjectAndNil(FActivted);
  DelayFreeObj(1.0, Self);
end;

constructor TThread_Event_Pool__.Create(Owner_: TThread_Pool);
begin
  inherited Create;
  FOwner := Owner_;
  FBindTh := nil;
  FPost := nil;
  FActivted := nil;
  TCompute.RunM(nil, Self, ThRun);
end;

destructor TThread_Event_Pool__.Destroy;
begin
  inherited Destroy;
end;

procedure TThread_Event_Pool__.PostC1(OnSync: TThreadPost_C1);
begin
  FPost.PostC1(OnSync);
end;

procedure TThread_Event_Pool__.PostC1(OnSync: TThreadPost_C1; IsRuning_, IsExit_: PBoolean);
begin
  FPost.PostC1(OnSync, IsRuning_, IsExit_);
end;

procedure TThread_Event_Pool__.PostC2(Data1: Pointer; OnSync: TThreadPost_C2);
begin
  FPost.PostC2(Data1, OnSync);
end;

procedure TThread_Event_Pool__.PostC2(Data1: Pointer; OnSync: TThreadPost_C2; IsRuning_, IsExit_: PBoolean);
begin
  FPost.PostC2(Data1, OnSync, IsRuning_, IsExit_);
end;

procedure TThread_Event_Pool__.PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3);
begin
  FPost.PostC3(Data1, Data2, Data3, OnSync);
end;

procedure TThread_Event_Pool__.PostC3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_C3; IsRuning_, IsExit_: PBoolean);
begin
  FPost.PostC3(Data1, Data2, Data3, OnSync, IsRuning_, IsExit_);
end;

procedure TThread_Event_Pool__.PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4);
begin
  FPost.PostC4(Data1, Data2, OnSync);
end;

procedure TThread_Event_Pool__.PostC4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_C4; IsRuning_, IsExit_: PBoolean);
begin
  FPost.PostC4(Data1, Data2, OnSync, IsRuning_, IsExit_);
end;

procedure TThread_Event_Pool__.PostM1(OnSync: TThreadPost_M1);
begin
  FPost.PostM1(OnSync);
end;

procedure TThread_Event_Pool__.PostM1(OnSync: TThreadPost_M1; IsRuning_, IsExit_: PBoolean);
begin
  FPost.PostM1(OnSync, IsRuning_, IsExit_);
end;

procedure TThread_Event_Pool__.PostM2(Data1: Pointer; OnSync: TThreadPost_M2);
begin
  FPost.PostM2(Data1, OnSync);
end;

procedure TThread_Event_Pool__.PostM2(Data1: Pointer; OnSync: TThreadPost_M2; IsRuning_, IsExit_: PBoolean);
begin
  FPost.PostM2(Data1, OnSync, IsRuning_, IsExit_);
end;

procedure TThread_Event_Pool__.PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3);
begin
  FPost.PostM3(Data1, Data2, Data3, OnSync);
end;

procedure TThread_Event_Pool__.PostM3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_M3; IsRuning_, IsExit_: PBoolean);
begin
  FPost.PostM3(Data1, Data2, Data3, OnSync, IsRuning_, IsExit_);
end;

procedure TThread_Event_Pool__.PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4);
begin
  FPost.PostM4(Data1, Data2, OnSync);
end;

procedure TThread_Event_Pool__.PostM4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_M4; IsRuning_, IsExit_: PBoolean);
begin
  FPost.PostM4(Data1, Data2, OnSync, IsRuning_, IsExit_);
end;

procedure TThread_Event_Pool__.PostP1(OnSync: TThreadPost_P1);
begin
  FPost.PostP1(OnSync);
end;

procedure TThread_Event_Pool__.PostP1(OnSync: TThreadPost_P1; IsRuning_, IsExit_: PBoolean);
begin
  FPost.PostP1(OnSync, IsRuning_, IsExit_);
end;

procedure TThread_Event_Pool__.PostP2(Data1: Pointer; OnSync: TThreadPost_P2);
begin
  FPost.PostP2(Data1, OnSync);
end;

procedure TThread_Event_Pool__.PostP2(Data1: Pointer; OnSync: TThreadPost_P2; IsRuning_, IsExit_: PBoolean);
begin
  FPost.PostP2(Data1, OnSync, IsRuning_, IsExit_);
end;

procedure TThread_Event_Pool__.PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3);
begin
  FPost.PostP3(Data1, Data2, Data3, OnSync);
end;

procedure TThread_Event_Pool__.PostP3(Data1: Pointer; Data2: TCore_Object; Data3: Variant; OnSync: TThreadPost_P3; IsRuning_, IsExit_: PBoolean);
begin
  FPost.PostP3(Data1, Data2, Data3, OnSync, IsRuning_, IsExit_);
end;

procedure TThread_Event_Pool__.PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4);
begin
  FPost.PostP4(Data1, Data2, OnSync);
end;

procedure TThread_Event_Pool__.PostP4(Data1: Pointer; Data2: TCore_Object; OnSync: TThreadPost_P4; IsRuning_, IsExit_: PBoolean);
begin
  FPost.PostP4(Data1, Data2, OnSync, IsRuning_, IsExit_);
end;

end.
