program _148_C4_CPM;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  FastMM5, // 跑CRM的d系要给FastMM5,fpc系用jem+tcm
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.DFE, Z.Parsing, Z.Expression, Z.Opcode,
  Z.Status,
  Z.Net, Z.Net.C4, Z.Net.C4_Console_APP, Z.Net.C4_XNAT_Cluster_Port_Mapping;

// CPM = Cluster port mapping,群集端口映射
// 群集端口映射是系统集成中常用的端口代理功能,在系统集成和大规模部署中非常实用!!
// Server常用范例为命令行,以 portproxy 为例 "netsh interface portproxy add v4tov4 listenaddress=xxx listenport=xxx connectaddress=xxx connectport=xxx"
// 而在轻云服务器,很多场合会使用FRP,Nignx等等反向代理技术
// 这类代理技术有个难点: 正反双向代理规模堆大以后,很难管理,并且非常不易于部署于,尤其在拓扑网络不稳定的环境
// CPM解决了大规模端口映射的可编程问题,并且支持重连,优于linux-iptables与win-netsh
// CRM对CPU要求会比较云服务器更高
// CPM可以单独运行,也可以直接集成到C4主系统
// CRM必须使用CrossSocket物理接口搭配IOCP+EPOLL,小并发的Synapse不支持CRM群集
// 运行CRM系统建议使用进程保护程序,定时重启+系统过载保护,如果没有这类支持体系,可以自己做一个
// 提示:CRM版的XNAT真正解决了流量冗余问题,如果是不对称流量,例如千兆对穿万兆会进入数据暂存模型,万兆发过来,千兆消化不及时就用硬盘暂存,不再堆内存空间
begin
  if C40_Extract_CmdLine(TTextStyle.tsC, ['Service("0.0.0.0","127.0.0.1", 9001, "CPM@XNAT_Port=10000")', 'Client("127.0.0.1", 9001, "CPM")']) then // C4套路
    begin
      TC40_Auto_Deployment_Client<TC40_CPM_Client_Tool>.Create_P(procedure(var Sender: TC40_CPM_Client_Tool) // C4自动化部署系统,当CRM客户端就绪
        var
          i: Integer;
        begin
          for i := 0 to 10 do // 正常情况下可以拉1000个端口侦听,debug模式需要调低,调试器无法支持大规模线程
              Sender.Add_CPM_Service_Listening(False, 9009 + i, PFormat('a%d', [i]), 60 * 1000, ''); // User_Data参数为自定义数据,可以json,xml,ini,yaml数据源
          Sender.Open_CPM_Service_Tunnel;
          Sender.Get_CPM_MappingP(procedure(Sender: TC40_CPM_Client_Tool; L: TC40_CPM_Info_List) // 获取侦听端口信息,L包含User_Data
            var
              j: Integer;
            begin
              for j := 0 to L.Count - 1 do
                  DoStatus('地址:%s 端口:%d 标记:%s 测试侦听:%s', [Sender.C40PhysicsTunnel.PhysicsAddr.Text, L[j].ListenPort, L[j].Mapping.Text, umlBoolToStr(L[j].Test_Listening_Passed).Text]);
            end);
          // 定义目标端口转发
          Sender.Begin_CPM_Address_Mapping;
          for i := 0 to 10 do
              Sender.Add_CPM_Address_Mapping(PFormat('a%d', [i]), '192.168.2.33', PFormat('%d', [8111 + i]));
          Sender.End_CPM_Address_Mapping;
        end);
      C40_Execute_Main_Loop; // C4套路
    end;
  C40Clean; // C4套路

end.
