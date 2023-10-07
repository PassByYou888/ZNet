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
    deploy_bridge: TDemo_Bridge_Client; // 自动部署的服务器接口
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
  DoStatus('cmd_cb_directstream 接收到的数据 %s', [umlMD5ToStr(InData.GetMD5(True)).Text]);
end;

procedure TDemo_Server.cmd_cb_async_directstream(Sender: TPeerIO; InData: TDFE);
begin
  DoStatus('cmd_cb_async_directstream 接收到的数据 %s', [umlMD5ToStr(InData.GetMD5(True)).Text]);
end;

procedure TDemo_Server.cmd_cb_stream(Sender: TPeerIO; InData, OutData: TDFE);
begin
  DoStatus('cmd_cb_stream 接收到的数据 %s', [umlMD5ToStr(InData.GetMD5(True)).Text]);
  OutData.Assign(InData);
  DoStatus('cmd_cb_stream 返回的数据 %s', [umlMD5ToStr(OutData.GetMD5(True)).Text]);
end;

procedure TDemo_Server.cmd_cb_thread_stream(Sender: TPeerIO; InData, OutData: TDFE);
begin
  DoStatus('cmd_cb_thread_stream 接收到的数据 %s', [umlMD5ToStr(InData.GetMD5(True)).Text]);
  OutData.Assign(InData);
  DoStatus('cmd_cb_thread_stream 返回的数据 %s', [umlMD5ToStr(OutData.GetMD5(True)).Text]);
end;

procedure TDemo_Server.cmd_cb_bridge_stream(Sender: TCommandCompleteBuffer_NoWait_Bridge; InData, OutData: TDFE);
var
  bridge_: TCompleteBuffer_Stream_Event_Bridge;
begin
  // 架桥就是事件指向,剩下的让桥自动处理
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
  // CompleteBuffer,主线程工作模式,非阻塞,InData会在主线程解码,触发命令序列走严格队列模型
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_DirectStream('cb_directstream').OnExecute := cmd_cb_directstream;

  // CompleteBuffer,线程工作模式,非阻塞,InData在线程中解码,执行命令为非严格队列
  // InData在线程中解码
  // InData在线程中解码
  // InData在线程中解码
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_Asynchronous_DirectStream('cb_async_directstream').OnExecute := cmd_cb_async_directstream;

  // CompleteBuffer,主线程工作模式,非阻塞,InData会在主线程解码,OutData会在主线程编码,触发命令序列走严格队列模型,反馈命令为非严格队列
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_NoWait_Stream('cb_stream').OnExecute := cmd_cb_stream;

  // CompleteBuffer,线程工作模式,非阻塞,InData会在线程解码,OutData会在线程编码,触发事件在线程中执行,在线程执行中IO如果丢失(断线之类),内存可以正常回收,非严格序列
  // 线程中执行
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_NoWait_Stream_Thread('cb_thread_stream').OnExecute := cmd_cb_thread_stream;

  // CompleteBuffer,主线程工作模式,非阻塞,InData会在主线程解码,OutData会在主线程编码,触发命令序列走严格队列模型,反馈命令为非严格队列
  // 需要特别注意的功能点,重要的事情说三次
  // 由RegisterCompleteBuffer_NoWait_Bridge_Stream触发的事件支持架桥+支持HPC分载线程
  // 由RegisterCompleteBuffer_NoWait_Bridge_Stream触发的事件支持架桥+支持HPC分载线程
  // 由RegisterCompleteBuffer_NoWait_Bridge_Stream触发的事件支持架桥+支持HPC分载线程
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_NoWait_Bridge_Stream('cb_bridge_stream').OnExecute := cmd_cb_bridge_stream;
  DTNoAuth.RecvTunnel.RegisterCompleteBuffer_NoWait_Bridge_Stream('cb_bridge_stream_HPC').OnExecute := cmd_cb_bridge_stream_HPC;

  // 假如我们设计完成一个服务器框架以后,该框架会依赖别的服务器框架,这时候,就用自动部署技术:让服务器构建以后,自动把依赖的东西都初始化
  // 自动部署作用是将已经入网的通讯组件赋予到目标变量
  // 因为c4的所有客户端都能断线重连,这里的赋值一旦成功,就会是长连接,即使断开,过一会也会恢复连接
  // 只有当触发idc故障事件时deploy_bridge才会真正消失掉,会出现无法访问内存错误
  // c4的idc故障事件只有当持续无连接达到一周才会触发,每周看一下服务器状态,发现有问题重启就行了
  // 把自动部署写成泛型,这样可以类型匹配,避免程序部署时写错变量找半天
  // TC40_Auto_Deployment_Client机制说明:执行时不会立即赋值目标变量,在C4入网就绪以后,目标变量会被自动赋值,如果目标变量长时间未赋值就是未入网成功
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

      // 重要的事情说三次
      // 如果通讯命令模型是非严格序列,ZNet做流量优化时只需要把SendDirectStream替换成SendCompleteBuffer_DirectStream,并且切换成线程
      // 如果通讯命令模型是非严格序列,ZNet做流量优化时只需要把SendDirectStream替换成SendCompleteBuffer_DirectStream,并且切换成线程
      // 如果通讯命令模型是非严格序列,ZNet做流量优化时只需要把SendDirectStream替换成SendCompleteBuffer_DirectStream,并且切换成线程
      DTNoAuth.SendTunnel.SendCompleteBuffer_DirectStream('cb_directstream', d);
      DTNoAuth.SendTunnel.SendCompleteBuffer_DirectStream('cb_async_directstream', d);

      // 重要的事情说三次
      // 如果通讯命令模型是非严格序列,ZNet做流量优化时只需要把SendStream替换成SendCompleteBuffer_NoWait_Stream,并且切换成线程
      // 如果通讯命令模型是非严格序列,ZNet做流量优化时只需要把SendStream替换成SendCompleteBuffer_NoWait_Stream,并且切换成线程
      // 如果通讯命令模型是非严格序列,ZNet做流量优化时只需要把SendStream替换成SendCompleteBuffer_NoWait_Stream,并且切换成线程
      DTNoAuth.SendTunnel.SendCompleteBuffer_NoWait_StreamP('cb_stream', d, procedure(Sender: TPeerIO; Result_: TDFE)
        begin
          DoStatus('cb_stream 客户端 接收到的数据 %s', [umlMD5ToStr(Result_.GetMD5(True)).Text]);
        end);
      DTNoAuth.SendTunnel.SendCompleteBuffer_NoWait_StreamP('cb_thread_stream', d, procedure(Sender: TPeerIO; Result_: TDFE)
        begin
          DoStatus('cb_thread_stream 客户端 接收到的数据 %s', [umlMD5ToStr(Result_.GetMD5(True)).Text]);
        end);
      DTNoAuth.SendTunnel.SendCompleteBuffer_NoWait_StreamP('cb_bridge_stream', d, procedure(Sender: TPeerIO; Result_: TDFE)
        begin
          DoStatus('cb_bridge_stream 客户端 接收到的数据 %s', [Result_.R.ReadString]);
        end);
      DTNoAuth.SendTunnel.SendCompleteBuffer_NoWait_StreamP('cb_bridge_stream_HPC', d, procedure(Sender: TPeerIO; Result_: TDFE)
        begin
          DoStatus('cb_bridge_stream 客户端 接收到的数据 %s', [Result_.R.ReadString]);
        end);
      d.Free;
    end);
end;

constructor TDemo_Client.Create(PhysicsTunnel_: TC40_PhysicsTunnel; source_: TC40_Info; Param_: U_String);
begin
  inherited Create(PhysicsTunnel_, source_, Param_);
  Register_ConsoleCommand('test', 'test(),测试CompleteBuffr').OnEvent_M := help_test; // 注册help命令
end;

begin
  // 注册一下
  RegisterC40('Demo_Bridge', TDemo_Bridge_Server, TDemo_Bridge_Client);
  RegisterC40('Demo_CB', TDemo_Server, TDemo_Client);

  // 为了方便书写脚本,使用C风格文本表达式
  C40AppParsingTextStyle := TTextStyle.tsC;

  // 这里是C4脚本,创建服务器+客户端,并且完成连接
  // 这里相当于开了2个服务器+2个客户端,懒得开demo工程,直接堆一起写
  // Demo使用方法:启动成功以后,看到依赖服务已经ready ok, 敲testm命令
  if C40_Extract_CmdLine([
    'Service("0.0.0.0", "127.0.0.1", 9093, "Demo_Bridge")', // 这一行表示创建服务器,并完成侦听
    'Service("0.0.0.0", "127.0.0.1", 9099, "Demo_CB")', // 这一行表示创建服务器,并完成侦听
    'Client("127.0.0.1", 9099, "Demo_CB")', // 这一行表示创建客户端,并使用127.0.0.1完成连接
    'Client("127.0.0.1", 9093, "Demo_Bridge")' // 这一行表示创建客户端,并使用127.0.0.1完成连接
    ]) then
      C40_Execute_Main_Loop; // Console专用,主循环,这里可以敲help命令

  // 释放C4
  Z.Net.C4.C40Clean;

end.
