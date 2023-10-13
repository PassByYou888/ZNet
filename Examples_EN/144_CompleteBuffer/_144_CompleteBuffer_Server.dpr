program _144_CompleteBuffer_Server;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  FastMM5,
  System.SysUtils,
  Z.Core, Z.DFE, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status,
  Z.Parsing, Z.Expression, Z.OpCode, Z.MemoryStream,
  Z.Net, Z.Net.C4, Z.Net.C4_Console_APP;

type
  TDemo_Bridge_Server = class(TC40_Base_NoAuth_Service)
  public
    procedure cmd_cb_hello_world(Sender: TPeerIO; InData, OutData: TDFE);
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
  end;

  TDemo_Bridge_Client = class(TC40_Base_NoAuth_Client);

procedure TDemo_Bridge_Server.cmd_cb_hello_world(Sender: TPeerIO; InData, OutData: TDFE);
begin
  OutData.WriteString('bridge hello world');
end;

constructor TDemo_Bridge_Server.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited;
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_NoWait_Stream('cb_hello_world').OnExecute := cmd_cb_hello_world;
end;

type
  TDemo_Server = class(TC40_Base_NoAuth_Service)
  public
    deploy_bridge: TDemo_Bridge_Client; { Automatically Deployed Server Interface }
  public
    procedure cmd_cb_directstream(Sender: TPeerIO; InData: TDFE);
    procedure cmd_cb_async_directstream(Sender: TPeerIO; InData: TDFE);
    procedure cmd_cb_stream(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_cb_thread_stream(Sender: TPeerIO; InData, OutData: TDFE);
    procedure cmd_cb_bridge_stream(Sender: TCommandCompleteBuffer_NoWait_Bridge; InData, OutData: TDFE);
    procedure cmd_cb_bridge_stream_HPC(Sender: TCommandCompleteBuffer_NoWait_Bridge; InData, OutData: TDFE);
    constructor Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String); override;
  end;

procedure TDemo_Server.cmd_cb_directstream(Sender: TPeerIO; InData: TDFE);
begin
  DoStatus('2. PostBatchStream//Start transmitting our query results', [umlMD5ToStr(InData.GetMD5(True)).Text]);
end;

procedure TDemo_Server.cmd_cb_async_directstream(Sender: TPeerIO; InData: TDFE);
begin
  DoStatus('Cmd_Cb_Async_Directstream received data%s', [umlMD5ToStr(InData.GetMD5(True)).Text]);
end;

procedure TDemo_Server.cmd_cb_stream(Sender: TPeerIO; InData, OutData: TDFE);
begin
  DoStatus('Cmd_Cb_Stream received data%s', [umlMD5ToStr(InData.GetMD5(True)).Text]);
  OutData.Assign(InData);
  DoStatus('Cmd_Cb_Stream returned data%s', [umlMD5ToStr(OutData.GetMD5(True)).Text]);
end;

procedure TDemo_Server.cmd_cb_thread_stream(Sender: TPeerIO; InData, OutData: TDFE);
begin
  DoStatus('Cmd_Cb_Thread_Stream received data%s', [umlMD5ToStr(InData.GetMD5(True)).Text]);
  OutData.Assign(InData);
  DoStatus('Cmd_Cb_Thread_Stream returned data%s', [umlMD5ToStr(OutData.GetMD5(True)).Text]);
end;

procedure TDemo_Server.cmd_cb_bridge_stream(Sender: TCommandCompleteBuffer_NoWait_Bridge; InData, OutData: TDFE);
var
  bridge_: TCompleteBuffer_Stream_Event_Bridge;
begin
  { Bridging is event pointing, and the rest is handled automatically by the bridge }
  bridge_ := TCompleteBuffer_Stream_Event_Bridge.Create(Sender);
  deploy_bridge.DTNoAuth.SendTunnel.SendCompleteBuffer_NoWait_StreamM('cb_hello_world', InData, bridge_.DoStreamEvent);
end;

procedure TDemo_Server.cmd_cb_bridge_stream_HPC(Sender: TCommandCompleteBuffer_NoWait_Bridge; InData, OutData: TDFE);
begin
  RunHPC_CompleteBuffer_StreamP(Sender, nil, nil, InData, OutData, procedure(thSender: THPC_CompleteBuffer_Stream; ThInData, ThOutData: TDFE)
    begin
      ThOutData.WriteString('HPC hello world.');
    end);
end;

constructor TDemo_Server.Create(PhysicsService_: TC40_PhysicsService; ServiceTyp, Param_: U_String);
begin
  inherited;
  { CompleteBuffer, main thread working mode, non blocking, InData will be decoded on the main thread, triggering the command sequence to follow the strict queue model }
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_DirectStream('cb_directstream').OnExecute := cmd_cb_directstream;

  { CompleteBuffer, thread working mode, non blocking, InData decoded in thread, execution command in non strict queue }
  { InData decoding in threads }
  { InData decoding in threads }
  { InData decoding in threads }
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_Asynchronous_DirectStream('cb_async_directstream').OnExecute := cmd_cb_async_directstream;

  { CompleteBuffer, main thread working mode, non blocking, InData will be decoded on the main thread, OutData will be encoded on the main thread, triggering the command sequence to follow the strict queue model, and feedback commands will be non strict queues }
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_NoWait_Stream('cb_stream').OnExecute := cmd_cb_stream;

  { CompleteBuffer, thread working mode, non blocking, InData will be decoded in the thread, OutData will be encoded in the thread, triggering events will be executed in the thread, and if IO is lost during thread execution (such as disconnection), memory can be reclaimed normally, non strict sequence }
  { Execute in Thread }
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_NoWait_Stream_Thread('cb_thread_stream').OnExecute := cmd_cb_thread_stream;

  { CompleteBuffer, main thread working mode, non blocking, InData will be decoded on the main thread, OutData will be encoded on the main thread, triggering the command sequence to follow the strict queue model, and feedback commands will be non strict queues }
  { Function points that require special attention, important things to say three times }
  { By RegisterCompleteBuffer_NoWait_Bridge_Stream triggered events support bridging+support for HPC offloading threads }
  { By RegisterCompleteBuffer_NoWait_Bridge_Stream triggered events support bridging+support for HPC offloading threads }
  { By RegisterCompleteBuffer_NoWait_Bridge_Stream triggered events support bridging+support for HPC offloading threads }
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_NoWait_Bridge_Stream('cb_bridge_stream').OnExecute := cmd_cb_bridge_stream;
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_NoWait_Bridge_Stream('cb_bridge_stream_HPC').OnExecute := cmd_cb_bridge_stream_HPC;

  { If we design a server framework that relies on other server frameworks, then we use automatic deployment technology: after the server is built, all dependent things are automatically initialized }
  { The role of automatic deployment is to assign communication components that have already been connected to the network to the target variable }
  { Because all c4 clients can disconnect and reconnect, once the assignment is successful, it will be a long connection. Even if disconnected, the connection will be restored later }
  { Deploy only when the IDC fault event is triggered_The bridge will truly disappear, and there will be an inability to access memory error }
  { The IDC failure event of c4 will only be triggered when there is no connection for a week. Check the server status every week and restart if any issues are found }
  { Write the automatic deployment as a generic, which can match types and avoid writing errors and searching for variables during program deployment }
  { TC40_Auto_Deployment_Client mechanism description: During execution, the target variable will not be assigned immediately. After C4 is ready for network connection, the target variable will be automatically assigned. If the target variable is not assigned for a long time, it means that the network connection is not successful }
  TC40_Auto_Deployment_Client<TDemo_Bridge_Client>.Create('Demo_Bridge', deploy_bridge);
end;

type
  TDemo_Client = class(TC40_Base_NoAuth_Client)
  public
    procedure help_test(var OP_Param: TOpParam);
    constructor Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String); override;
  end;

procedure TDemo_Client.help_test(var OP_Param: TOpParam);
begin
  TCompute.RunP_NP(procedure
    var
      m64: TMS64;
      d: TDFE;
      i: Integer;
    begin
      d := TDFE.Create;
      m64 := TMS64.Create;
      m64.Size := 10 * 1024 * 1024;
      d.WriteStream(m64);
      m64.Free;
      DoStatus('buff size: %s', [umlSizeToStr(d.ComputeEncodeSize).Text]);

      { Say important things three times }
      { If the communication command model is a non strict sequence, ZNet only needs to replace SendDirectStream with SendCompleteBuffer for traffic optimization_DirectStream and switch to thread }
      { If the communication command model is a non strict sequence, ZNet only needs to replace SendDirectStream with SendCompleteBuffer for traffic optimization_DirectStream and switch to thread }
      { If the communication command model is a non strict sequence, ZNet only needs to replace SendDirectStream with SendCompleteBuffer for traffic optimization_DirectStream and switch to thread }
      DTNoAuth.SendTunnel.SendCompleteBuffer_DirectStream('cb_directstream', d);
      DTNoAuth.SendTunnel.SendCompleteBuffer_DirectStream('cb_async_directstream', d);

      { Say important things three times }
      { If the communication command model is a non strict sequence, ZNet only needs to replace SendStream with SendCompleteBuffer for traffic optimization_NoWait_Stream and switch to thread }
      { If the communication command model is a non strict sequence, ZNet only needs to replace SendStream with SendCompleteBuffer for traffic optimization_NoWait_Stream and switch to thread }
      { If the communication command model is a non strict sequence, ZNet only needs to replace SendStream with SendCompleteBuffer for traffic optimization_NoWait_Stream and switch to thread }
      DTNoAuth.SendTunnel.SendCompleteBuffer_NoWait_StreamP('cb_stream', d, procedure(Sender: TPeerIO; Result_: TDFE)
        begin
          DoStatus('Cb_Stream client received data%s', [umlMD5ToStr(Result_.GetMD5(True)).Text]);
        end);
      DTNoAuth.SendTunnel.SendCompleteBuffer_NoWait_StreamP('cb_thread_stream', d, procedure(Sender: TPeerIO; Result_: TDFE)
        begin
          DoStatus('Cb_Thread_Stream client received data%s', [umlMD5ToStr(Result_.GetMD5(True)).Text]);
        end);
      DTNoAuth.SendTunnel.SendCompleteBuffer_NoWait_StreamP('cb_bridge_stream', d, procedure(Sender: TPeerIO; Result_: TDFE)
        begin
          DoStatus('2. PostBatchStream//Start transmitting our query results', [Result_.R.ReadString]);
        end);
      DTNoAuth.SendTunnel.SendCompleteBuffer_NoWait_StreamP('cb_bridge_stream_HPC', d, procedure(Sender: TPeerIO; Result_: TDFE)
        begin
          DoStatus('2. PostBatchStream//Start transmitting our query results', [Result_.R.ReadString]);
        end);
      d.Free;
    end);
end;

constructor TDemo_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  Register_ConsoleCommand('test', 'Test(), testing CompleteBuffr').OnEvent_M := help_test; { Register help command }
end;

begin
  { Register }
  RegisterC40('Demo_Bridge', TDemo_Bridge_Server, TDemo_Bridge_Client);
  RegisterC40('Demo_CB', TDemo_Server, TDemo_Client);

  { To facilitate script writing, use C-style text expressions }
  C40AppParsingTextStyle := TTextStyle.tsC;

  { Here is the C4 script, creating a server+client and completing the connection }
  { This is equivalent to opening 2 servers and 2 clients, so I'm too lazy to start a demo project and just pile them together to write }
  { Demo usage method: After successful startup, when you see that the dependent service is ready for operation, type the testm command }
  if C40_Extract_CmdLine([
    'Service("0.0.0.0", "127.0.0.1", 9093, "Demo_Bridge")', { This line represents creating the server and completing listening }
    'Service("0.0.0.0", "127.0.0.1", 9099, "Demo_CB")', { This line represents creating the server and completing listening }
    'Client("127.0.0.1", 9099, "Demo_CB")', { This line represents creating the client and completing the connection using 127.0.0.1 }
    'Client("127.0.0.1", 9093, "Demo_Bridge")' { This line represents creating the client and completing the connection using 127.0.0.1 }
    ]) then
      C40_Execute_Main_Loop; { Console dedicated, main loop, where you can type the help command }

  { Release C4 }
  Z.Net.C4.C40Clean;

end.
