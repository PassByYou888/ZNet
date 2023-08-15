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
  // 长处理的特点就是后台化处理+延迟化反馈,这也是最简单的多任务并行化机制
  // 长处理机制,在c4架构中被大量使用.
  // 长处理程序范式不容易入库,这是在项目开发中酌情使用的技术:long run算一种范式性质的技术

  // 定义反馈结构
  TLong_Run_Struct_ = record
    user_info: U_String;
    procedure Do_Done(d: TDFE);
  end;

  PLong_Run_Struct_ = ^TLong_Run_Struct_;

procedure TLong_Run_Struct_.Do_Done(d: TDFE);
begin
  DoStatus('"%s" 计算完成 总共反馈 %d 条', [user_info.Text, d.Count]);
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
  d.WritePointer(p); // 保存反馈指针
  for i := 1 to 1000 * 50 do // 生成计算脚本
      d.WriteString('%d*%d', [umlRR(1, 1000000), umlRR(1, 1000000)]);

  DoStatus('"%s" 已经发送到服务器计算', [p^.user_info.Text]);
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
  cli_.RecvTunnel.RegisterDirectStream('Done_Large_Expression').OnExecute_C := cmd_Done_Large_Expression; // 完成回调
  cli_.QuietMode := True; // 调试用
  cli_.Connect_P('127.0.0.1', '10999', '123456', procedure(const state: Boolean)
    begin
      if state then
        begin
          DoStatus('连接成功.');
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
