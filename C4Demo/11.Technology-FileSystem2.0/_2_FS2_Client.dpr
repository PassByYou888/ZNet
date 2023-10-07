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
  // ���ȷ������˿ڹ�����ַ,������ipv4,ipv6,dns
  // ������ַ,���ܸ�127.0.0.1����
  Internet_DP_Addr_ = '127.0.0.1';
  // ���ȷ������˿�
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

  // FSҲ��һ����Ҫ������ʩ����Ҫ���ڴ�Ŵ������ݣ�����ͼƬ���б��������ݵȵ�
  // C4��FS֧�ָ�Ƶ�ʲ�д��VM������������FS��Ϊ���ݽ�������Ҫ����Var�������
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
      // �����������ļ�������ļ���token���Զ��������еģ������ڴ洢�ռ�ʹ�ò�д���ƴ���
      // postfile��api���ṹ��һ���µ�p2pVM����������Ŷ�
      // p2pVM������������ļ����������ͬ���ļ�ͬʱ���д��䣬������������IO������ɴ�����Ⱥ�˳���д�������������ĸ������ٿ��

      // FS2.0��postfile������FS��һ����FS2����Զ��FS�����Ƿ�����ͬ���ݣ�����У�������copy�����������봫
      GetMyFS_Client.FS2_PostFile_P(True, 'test', tmp1, True, procedure(Sender: TC40_FS2_Client; info_: U_String)
        begin
          DoStatus('remote md5(test):' + info_);

          // FS2.0��postfile������FS��һ����FS2����Զ��FS�����Ƿ�����ͬ���ݣ�����У�������copy�����������봫
          GetMyFS_Client.FS2_PostFile_P(True, 'aaaa', tmp2, True, procedure(Sender: TC40_FS2_Client; info2_: U_String)
            begin
              DoStatus('remote md5(aaaa):' + info2_);
              // ��ʹ��cache
              // ��post��ɺ����ǽ��ļ�get������get�ļ�Ҳ�ṹ���µ�p2pVM����������䣬���ᷢ���Ŷӵ�
              GetMyFS_Client.FS2_GetFile_P(
                False,
                'aaaa',
                procedure(Sender: TC40_FS2_Client; stream: TMS64; Token: U_String; Successed: Boolean)
                begin
                  if Successed then
                      DoStatus('downloaded md5: ' + umlStreamMD5String(stream));

                  // ʹ��cache
                  // ��post��ɺ����ǽ��ļ�get������get�ļ�Ҳ�ṹ���µ�p2pVM����������䣬���ᷢ���Ŷӵ�
                  GetMyFS_Client.FS2_GetFile_P(
                    True,
                    'aaaa',
                    procedure(Sender2: TC40_FS2_Client; stream2: TMS64; Token2: U_String; Successed2: Boolean)
                    begin
                      if Successed2 then
                          DoStatus('use cache downloaded md5: ' + umlStreamMD5String(stream2));

                      // FS2_PoolFrag��������ʵ�ֶ�FS2.0�Ĵ��ģԶ�̱�������FS2.0�ı��ݺ�ͬ�����ṩ��������֧�ֹ���
                      // ע�⣺FS2_PoolFrag�ı������Ʊ����������
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

  // ��ѭ��
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
