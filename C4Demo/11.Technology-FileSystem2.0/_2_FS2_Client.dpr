program _2_FS2_Client;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Windows,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.MemoryStream,
  Z.Notify,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_FS2,
  Z.Net.C4_Console_APP;

const
  // 调度服务器端口公网地址,可以是ipv4,ipv6,dns
  // 公共地址,不能给127.0.0.1这类
  Internet_DP_Addr_ = '127.0.0.1';
  // 调度服务器端口
  Internet_DP_Port_ = 8387;

function GetMyFS_Client: TC40_FS2_Client;
begin
  Result := TC40_FS2_Client(C40_ClientPool.ExistsConnectedServiceTyp('FS2'));
end;

function ConsoleProc(CtrlType: DWORD): Bool; stdcall;
begin
  case CtrlType of
    CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT, CTRL_LOGOFF_EVENT, CTRL_SHUTDOWN_EVENT:
      begin
        TCompute.SyncC(Z.Net.C4.C40Clean);
      end;
  end;
  Result := True;
end;

begin
  SetConsoleCtrlHandler(@ConsoleProc, True);

  Z.Net.C4.C40_QuietMode := True;
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'DP|FS2', nil);

  // FS也是一种主要基础设施，主要用于存放大型数据，例如图片，列表，配置数据等等
  // C4的FS支持高频率擦写，VM服务器可以用FS作为数据交换，但要区分Var网络变量
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('FS2', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    var
      tmp1, tmp2: TMS64;
    begin
      GetMyFS_Client.FS2_RemoveFile('test');

      tmp1 := TMS64.Create;
      tmp1.Size := 1024 * 1024;
      MT19937Rand32(MaxInt, tmp1.Memory, tmp1.Size div 4);
      tmp2 := tmp1.Clone;
      DoStatus('origin md5: ' + umlStreamMD5String(tmp1));
      // 往服务器仍文件，这个文件的token会自动覆盖已有的，覆盖在存储空间使用擦写机制处理
      // postfile的api，会构建一个新的p2pVM隧道，永不排队
      // p2pVM并发隧道传输文件，如果两个同名文件同时并行传输，服务器会依据IO触发完成传输的先后顺序擦写操作，网速慢的覆盖网速快的

      // FS2.0的postfile机制与FS不一样：FS2会检查远程FS服务是否有相同数据，如果有，就做个copy副本操作，秒传
      GetMyFS_Client.FS2_PostFile_P(True, 'test', tmp1, True, procedure(Sender: TC40_FS2_Client; info_: U_String)
        begin
          DoStatus('remote md5(test):' + info_);

          // FS2.0的postfile机制与FS不一样：FS2会检查远程FS服务是否有相同数据，如果有，就做个copy副本操作，秒传
          GetMyFS_Client.FS2_PostFile_P(True, 'aaaa', tmp2, True, procedure(Sender: TC40_FS2_Client; info2_: U_String)
            begin
              DoStatus('remote md5(aaaa):' + info2_);
              // 不使用cache
              // 当post完成后，我们将文件get下来，get文件也会构建新的p2pVM隧道并发传输，不会发生排队等
              GetMyFS_Client.FS2_GetFile_P(
                False,
                'aaaa',
                procedure(Sender: TC40_FS2_Client; stream: TMS64; Token: U_String; Successed: Boolean)
                begin
                  if Successed then
                      DoStatus('downloaded md5: ' + umlStreamMD5String(stream));

                  // 使用cache
                  // 当post完成后，我们将文件get下来，get文件也会构建新的p2pVM隧道并发传输，不会发生排队等
                  GetMyFS_Client.FS2_GetFile_P(
                    True,
                    'aaaa',
                    procedure(Sender2: TC40_FS2_Client; stream2: TMS64; Token2: U_String; Successed2: Boolean)
                    begin
                      if Successed2 then
                          DoStatus('use cache downloaded md5: ' + umlStreamMD5String(stream2));

                      // FS2_PoolFrag方法可以实现对FS2.0的大规模远程遍历，对FS2.0的备份和同步，提供基础遍历支持功能
                      // 注意：FS2_PoolFrag的遍历机制必须分批进行
                      GetMyFS_Client.FS2_PoolFragP(procedure(Sender: TC40_FS2_Client; arry: TFS2_PoolFragInfo_Array)
                        var
                          i: integer;
                        begin
                          for i := low(arry) to high(arry) do
                              DoStatus(arry[i].FileName + ' md5:' + umlMD5ToStr(arry[i].MD5));
                        end);
                    end);
                end);
            end);
        end);
    end);

  // 主循环
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
