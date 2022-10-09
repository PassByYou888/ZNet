program _2_UserDB_Client;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_UserDB,
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

function GetMyUserDB_Client: TC40_UserDB_Client;
begin
  Result := TC40_UserDB_Client(C40_ClientPool.ExistsConnectedServiceTyp('userDB'));
end;

begin
  // ��Log��Ϣ
  Z.Net.C4.C40_QuietMode := False;

  // ��ͨ���ȶ˺��û�������ݿ����
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'DP|UserDB', nil);

  // WaitConnectedDone����ͬʱ��������������Ƿ����
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('DP|UserDB', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    begin
      GetMyUserDB_Client.Usr_RegC('testUser', '123456', nil);

      // ��testUser��������һ���������¼�ı��������������û��ĵ����ʼ����ֻ�����
      GetMyUserDB_Client.Usr_NewIdentifierP('testUser', 'test@mail.com',
          procedure(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString)
        begin
          DoStatus(info_);
        end);

      // ʹ�ñ���Զ����֤�û���ݣ�����ֻ����֤���أ�����VM������������userDB���������κε�¼������¼������VM��������
      GetMyUserDB_Client.Usr_AuthP('test@mail.com', '123456',
        procedure(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString)
        begin
          DoStatus(info_);
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
