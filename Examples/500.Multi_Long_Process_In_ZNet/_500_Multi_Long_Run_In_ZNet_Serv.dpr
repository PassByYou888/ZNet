program _500_Multi_Long_Run_In_ZNet_Serv;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  FastMM5, // 多线程后台外MM库,提速用
  SysUtils,
  Z.Core, Z.Status, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.DFE, Z.Expression, Z.OpCode,
  Z.Net, Z.Net.PhysicsIO, Z.Net.DoubleTunnelIO.NoAuth;

var
  serv_: TDT_P2PVM_NoAuth_Service;

  // 长处理的特点就是后台化处理+延迟化反馈,这也是最简单的多任务并行化机制
  // 长处理机制,在c4架构中被大量使用.
  // 长处理程序范式不容易入库,这是在项目开发中酌情使用的技术:long run算一种范式性质的技术
procedure Do_Th_Large_Expression(ThSender: THPC_DirectStream; ThInData: TDFE);
var
  user_data_: UInt64; // 远程结构
  i: Integer;
  OutData: TDFE;
begin
  user_data_ := ThInData.R.ReadPointer;
  OutData := TDFE.Create;
  OutData.WritePointer(user_data_); // 反馈指针
  while ThInData.R.NotEnd do
      OutData.WriteString(umlVarToStr(EvaluateExpressionValue(False, ThInData.R.ReadString)));

  // 通过发送通道反馈回去,以S_ID方式反馈是安全的
  serv_.SendTunnel.SendDirectStreamCmd(ThSender.Send_Tunnel_ID, 'Done_Large_Expression', OutData);

  // 建议方式
  // TZNet_Server(ThSender.Send_Tunnel).SendDirectStreamCmd(ThSender.Send_Tunnel_ID, 'Done_Large_Expression', OutData);

  // 第二种主动发送方式,这种方式会出现处理完以后,远程IO刚好断线从而异常,这时候,使用TCompute的异常冗余来消化这些异常
  // ThSender.IO.Get_Send_Tunnel_IO.SendDirectStreamCmd('Done_Large_Expression', OutData);

  OutData.Free;
end;

procedure cmd_Large_Expression(Sender: TPeerIO; InData: TDFE);
begin
  // 处理转交给线程
  RunHPC_DirectStreamC(Sender, nil, nil, InData, Do_Th_Large_Expression);
end;

procedure run_serv;
begin
  serv_ := TDT_P2PVM_NoAuth_Service.Create(TDTService_NoAuth);
  serv_.QuietMode := True; // 调试用
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
