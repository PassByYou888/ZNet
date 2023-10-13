program AsyncStateDemo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  Variants,
  Z.Core,
  Z.Status;

{  Simple demonstration of using secure state machines in threads  }
procedure StateDemo();
var
  State: TAtomBool;
begin
  {  Thread Safety: State Variables  }
  State := TAtomBool.Create(False);
  {  Worker thread  }
  TCompute.RunP_NP(procedure
    var
      i: Cardinal;
    begin
      for i := 0 to $FFFFFFFF do
          nop;
      DoStatus('Complete a cycle');
      {  Change state variable  }
      State.V := True;
    end);
  DoStatus('Wait for the cycle to complete');
  while not State.V do
    begin
      DoStatus();
      TCompute.Sleep(1);
    end;
  DisposeObject(State);
end;

{  Simple demonstration of using post mechanism in thread  }
{  Post is equivalent to sending code to a target thread we have built for execution. Post does not pick linear types and does not distinguish between main/sub threads. Any thread can use it  }
procedure PostDemo();
var
  P: TThreadPost; {  Tthreadpost, post mechanism support  }
  done: TAtomBool;        {  Thread Safety: State Variables  }
  over: boolean;          {  The local variable will be automatically destroyed after the call is completed. Therefore, we must do a monitoring work here_End state  }
begin
  {  Thread Safety: State Variables  }
  done := TAtomBool.Create(False);
  P := TThreadPost.Create(0);
  {  Worker thread  }
  TCompute.RunP_NP(procedure
    begin
      P.ThreadID := TCompute.CurrentThread.ThreadID;
      while not done.V do
        begin
          P.Progress(); {  Post main loop  }
          TCompute.Sleep(1);
        end;
      over := True;
    end);
  {  Send task to worker thread  }
  {  These tasks are performed in strict order of input  }
  P.PostP1(procedure
    begin
      DoStatus(1);
    end);
  P.PostP1(procedure
    begin
      DoStatus(2);
    end);
  TCompute.RunP_NP(procedure
    var
      i: Integer;
    begin
      for i := 3 to 20 do
        begin
          TCompute.Sleep(100);
          P.PostP3(nil, nil, i, procedure(Data1: Pointer; Data2: TCore_Object; Data3: Variant)
            begin
              DoStatus(VarToStr(Data3));
            end);
        end;
      {  End of worker thread  }
      P.PostP1(procedure
        begin
          done.V := True;
        end);
    end);
  {  After execution  }
  while not over do
    begin
      DoStatus();
      TCompute.Sleep(1);
    end;
  DisposeObjectAndNil(P);
  DisposeObjectAndNil(done);
end;

begin
  StateDemo();
  PostDemo();
  DoStatus('Press enter to end the program');
  readln;

end.
