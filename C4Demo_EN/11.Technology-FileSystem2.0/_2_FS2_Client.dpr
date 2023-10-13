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
  {  The public network address of the dispatching server port, which can be IPv4, IPv6 or DNS  }
  {  Public address, cannot be given to a type of 127.0.0.1  }
  Internet_DP_Addr_ = '127.0.0.1';
  {  Scheduling Server Port  }
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

  {  FS is also a major infrastructure, which is mainly used to store large data, such as pictures, lists, configuration data, etc  }
  {  C4's FS supports high-frequency erasure, and VM servers can use FS as data exchange, but Var network variables need to be distinguished  }
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
      {  When a file is returned to the server, the token of the file will automatically overwrite the existing one, which will be overwritten in the storage space and processed by the erasure mechanism  }
      {  The postfile API will build a new p2pvm tunnel and never queue  }
      {  P2P VM concurrent tunnel transmission files. If two files with the same name are simultaneously transmitted in parallel, the server will perform write operations based on the order in which the IO trigger completes the transmission. The slow network speed will overwrite the fast network speed  }

      {  The postfile mechanism of FS2.0 is different from FS: FS2 checks whether the remote FS service has the same data, and if so, performs a copy copy operation, which is transmitted in seconds  }
      GetMyFS_Client.FS2_PostFile_P(True, 'test', tmp1, True, procedure(Sender: TC40_FS2_Client; info_: U_String)
        begin
          DoStatus('remote md5(test):' + info_);

          {  The postfile mechanism of FS2.0 is different from FS: FS2 checks whether the remote FS service has the same data, and if so, performs a copy copy operation, which is transmitted in seconds  }
          GetMyFS_Client.FS2_PostFile_P(True, 'aaaa', tmp2, True, procedure(Sender: TC40_FS2_Client; info2_: U_String)
            begin
              DoStatus('remote md5(aaaa):' + info2_);
              {  Do not use cache  }
              {  After the post is completed, we will get the file, and the get file will also build a new p2pvm tunnel for concurrent transmission without queuing  }
              GetMyFS_Client.FS2_GetFile_P(
                False,
                'aaaa',
                procedure(Sender: TC40_FS2_Client; stream: TMS64; Token: U_String; Successed: Boolean)
                begin
                  if Successed then
                      DoStatus('downloaded md5: ' + umlStreamMD5String(stream));

                  {  Using cache  }
                  {  After the post is completed, we will get the file, and the get file will also build a new p2pvm tunnel for concurrent transmission without queuing  }
                  GetMyFS_Client.FS2_GetFile_P(
                    True,
                    'aaaa',
                    procedure(Sender2: TC40_FS2_Client; stream2: TMS64; Token2: U_String; Successed2: Boolean)
                    begin
                      if Successed2 then
                          DoStatus('use cache downloaded md5: ' + umlStreamMD5String(stream2));

                      {  FS2_The PoolFrag method can achieve large-scale remote traversal of FS2.0, backup and synchronization of FS2.0, and provide basic traversal support functions  }
                      {  Attention: FS2_The traversal mechanism of PoolFrag must be performed in batches  }
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

  {  Main loop  }
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
