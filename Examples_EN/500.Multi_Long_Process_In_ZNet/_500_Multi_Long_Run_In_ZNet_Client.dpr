program _500_Multi_Long_Run_In_ZNet_Client;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core, Z.Status, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.DFE, Z.Expression, Z.OpCode,
  Z.Notify,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth;

var
  cli_: TDT_P2PVM_NoAuth_Client;

type
  {  The characteristic of long processing is background processing+delayed feedback, which is also the simplest multitask parallelization mechanism  }
  {  Long processing mechanism is widely used in the c4 architecture  }
  {  Long processing program paradigms are not easy to store, and this is a technique that can be used as appropriate in project development: long run is considered a paradigm like technique  }

  {  Define feedback structure  }
  TLong_Run_Struct_ = record
    user_info: U_String;
    procedure Do_Done(d: TDFE);
  end;

  PLong_Run_Struct_ = ^TLong_Run_Struct_;

procedure TLong_Run_Struct_.Do_Done(d: TDFE);
begin
  DoStatus('Calculation of '#39'%s'#39' completed with a total of%d feedback items', [user_info.Text, d.Count]);
end;

procedure do_delay_send;
var
  p: PLong_Run_Struct_;
  d: TDFE;
  i: Integer;
begin
  new(p);
  p^.user_info := umlPointerToStr(p);

  d := TDFE.Create;
  d.WritePointer(p); {  Save feedback pointer  }
  for i := 1 to 1000 * 50 do {  Generate calculation scripts  }
      d.WriteString('%d*%d', [umlRR(1, 1000000), umlRR(1, 1000000)]);

  DoStatus(#39'%s'#39' has been sent to the server for calculation', [p^.user_info.Text]);
  cli_.SendTunnel.SendDirectStreamCmd('Large_Expression', d);
  d.Free;

  if cli_.SendTunnel.CmdSendStatistics['Large_Expression'] < 10 then
      SysPost.PostExecuteC_NP(0.1, do_delay_send);
end;

procedure cmd_Done_Large_Expression(Sender: TPeerIO; InData: TDFE);
var
  p: PLong_Run_Struct_;
begin
  p := PLong_Run_Struct_(InData.R.ReadPointer);
  p^.Do_Done(InData);
  Dispose(p);
end;

procedure Run_Client;
begin
  cli_ := TDT_P2PVM_NoAuth_Client.Create(TDTClient_NoAuth);
  cli_.RecvTunnel.RegisterDirectStream('Done_Large_Expression').OnExecute_C := cmd_Done_Large_Expression; {  Complete callback  }
  cli_.QuietMode := True; {  Commissioning  }
  cli_.Connect_P('127.0.0.1', '10999', '123456', procedure(const state: Boolean)
    begin
      if state then
        begin
          DoStatus('Successfully connected');
          do_delay_send();
        end;
    end);

  while True do
    begin
      CheckThread();
      cli_.Progress;
      TCompute.Sleep(1);
    end;
end;

begin
  Run_Client();

end.
