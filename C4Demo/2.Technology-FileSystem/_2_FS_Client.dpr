program _2_FS_Client;

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
  Z.Net.C4_FS,
  Z.Net.C4_Console_APP;

var
  exit_signal: Boolean;

procedure Do_Check_On_Exit;
var
  n: string;
  cH: TC40_Console_Help;
begin
  cH := TC40_Console_Help.Create;
  repeat
    TCompute.Sleep(100);
    Readln(n);
    cH.Run_HelpCmd(n);
  until cH.IsExit;
  disposeObject(cH);
  exit_signal := True;
end;

const
  // ���ȷ������˿ڹ�����ַ,������ipv4,ipv6,dns
  // ������ַ,���ܸ�127.0.0.1����
  Internet_DP_Addr_ = '127.0.0.1';
  // ���ȷ������˿�
  Internet_DP_Port_ = 8387;

function GetMyFS_Client: TC40_FS_Client;
begin
  Result := TC40_FS_Client(C40_ClientPool.ExistsConnectedServiceTyp('FS'));
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
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'DP|FS', nil);

  // FSҲ��һ����Ҫ������ʩ����Ҫ���ڴ�Ŵ������ݣ�����ͼƬ���б��������ݵȵ�
  // C4��FS֧�ָ�Ƶ�ʲ�д��VM������������FS��Ϊ���ݽ�������Ҫ����Var�������
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('FS', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    var
      tmp: TMS64;
    begin
      GetMyFS_Client.FS_RemoveFile('test');

      tmp := TMS64.Create;
      tmp.Size := 1024 * 1024;
      MT19937Rand32(MaxInt, tmp.Memory, tmp.Size div 4);
      DoStatus('origin md5: ' + umlStreamMD5String(tmp));
      // �����������ļ�������ļ���token���Զ��������еģ������ڴ洢�ռ�ʹ�ò�д���ƴ���
      // postfile��api���ṹ��һ���µ�p2pVM����������Ŷ�
      // p2pVM������������ļ����������ͬ���ļ�ͬʱ���д��䣬������������IO������ɴ�����Ⱥ�˳���д�������������ĸ������ٿ��
      GetMyFS_Client.FS_PostFile_P('test', tmp, True, procedure(Sender: TC40_FS_Client; info_: U_String)
        begin
          DoStatus('remote md5:' + info_);
          // ��ʹ��cache
          // ��post��ɺ����ǽ��ļ�get������get�ļ�Ҳ�ṹ���µ�p2pVM����������䣬���ᷢ���Ŷӵ�
          GetMyFS_Client.FS_GetFile_P(
            False,
            'test',
            procedure(Sender: TC40_FS_Client; stream: TMS64; Token: U_String; Successed: Boolean)
            begin
              if Successed then
                  DoStatus('downloaded md5: ' + umlStreamMD5String(stream));

              // ʹ��cache
              // ��post��ɺ����ǽ��ļ�get������get�ļ�Ҳ�ṹ���µ�p2pVM����������䣬���ᷢ���Ŷӵ�
              GetMyFS_Client.FS_GetFile_P(
                True,
                'test',
                procedure(Sender2: TC40_FS_Client; stream2: TMS64; Token2: U_String; Successed2: Boolean)
                begin
                  if Successed2 then
                      DoStatus('use cache downloaded md5: ' + umlStreamMD5String(stream2));
                end);
            end);

        end);
    end);

  // ��ѭ��
  StatusThreadID := False;
  exit_signal := False;
  TCompute.RunC_NP(@Do_Check_On_Exit);
  while not exit_signal do
      Z.Net.C4.C40Progress;

  Z.Net.C4.C40Clean;

end.
