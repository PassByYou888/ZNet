program _500_Multi_Long_Run_In_ZNet_Serv;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  FastMM5, {  Multi threaded backend external MM library for speed up use  }
  SysUtils,
  Z.Core, Z.Status, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.DFE, Z.Expression, Z.OpCode,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth;

var
  serv_: TDT_P2PVM_NoAuth_Service;

  {  The characteristic of long processing is background processing+delayed feedback, which is also the simplest multitask parallelization mechanism  }
  {  Long processing mechanism is widely used in the c4 architecture  }
  {  Long processing program paradigms are not easy to store, and this is a technique that can be used as appropriate in project development: long run is considered a paradigm like technique  }
procedure Do_Th_Large_Expression(ThSender: THPC_DirectStream; ThInData: TDFE);
var
  user_data_: UInt64; {  Remote structure  }
  i: Integer;
  OutData: TDFE;
begin
  user_data_ := ThInData.R.ReadPointer;
  OutData := TDFE.Create;
  OutData.WritePointer(user_data_); {  Feedback pointer  }
  while ThInData.R.NotEnd do
      OutData.WriteString(umlVarToStr(EvaluateExpressionValue(False, ThInData.R.ReadString)));

  {  Send channel feedback back to S_ID feedback is secure  }
  serv_.SendTunnel.SendDirectStreamCmd(ThSender.Send_Tunnel_ID, 'Done_Large_Expression', OutData);

  {  Suggested approach  }
  // TZNet_Server(ThSender.Send_Tunnel).SendDirectStreamCmd(ThSender.Send_Tunnel_ID, 'Done_Large_Expression', OutData);

  {  The second active sending method may result in remote IO being disconnected and causing exceptions after processing. At this time, TCompute's exception redundancy is used to digest these exceptions  }
  // ThSender.IO.Get_Send_Tunnel_IO.SendDirectStreamCmd('Done_Large_Expression', OutData);

  OutData.Free;
end;

procedure cmd_Large_Expression(Sender: TPeerIO; InData: TDFE);
begin
  {  Handing over processing to threads  }
  RunHPC_DirectStreamC(Sender, nil, nil, InData, Do_Th_Large_Expression);
end;

procedure run_serv;
begin
  serv_ := TDT_P2PVM_NoAuth_Service.Create(TDTService_NoAuth);
  serv_.QuietMode := True; {  Commissioning  }
  serv_.RecvTunnel.RegisterDirectStream('Large_Expression').OnExecute_C := cmd_Large_Expression;

  serv_.StartService('0.0.0.0', '10999', '123456');
  while True do
    begin
      CheckThread();
      serv_.Progress;
      TCompute.Sleep(1);
    end;
end;

begin
  run_serv;

end.
