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

const
  {  The public network address of the dispatching server port, which can be IPv4, IPv6 or DNS  }
  {  Public address, cannot be given to a type of 127.0.0.1  }
  Internet_DP_Addr_ = '127.0.0.1';
  {  Scheduling Server Port  }
  Internet_DP_Port_ = 8387;

function GetMyUserDB_Client: TC40_UserDB_Client;
begin
  Result := TC40_UserDB_Client(C40_ClientPool.ExistsConnectedServiceTyp('userDB'));
end;

begin
  {  Open Log Information  }
  Z.Net.C4.C40_QuietMode := False;

  {  Connect the scheduling end and user identity database service  }
  Z.Net.C4.C40_PhysicsTunnelPool.GetOrCreatePhysicsTunnel(Internet_DP_Addr_, Internet_DP_Port_, 'DP|UserDB', nil);

  {  WaitConnectedDone can simultaneously check if multiple dependent services are ready  }
  Z.Net.C4.C40_ClientPool.WaitConnectedDoneP('DP|UserDB', procedure(States_: TC40_Custom_ClientPool_Wait_States)
    begin
      GetMyUserDB_Client.Usr_RegC('testUser', '123456', nil);

      {  Permanently add a login alias to testuser, such as e-mail and mobile phone number  }
      GetMyUserDB_Client.Usr_NewIdentifierP('testUser', 'test@mail.com',
          procedure(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString)
        begin
          DoStatus(info_);
        end);

      {  The alias is used to remotely verify the user's identity. Here is only the verification return, which is convenient for the VM server to work. The userdb does not do any login processing, and the login processing is done on the VM server  }
      GetMyUserDB_Client.Usr_AuthP('test@mail.com', '123456',
        procedure(sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString)
        begin
          DoStatus(info_);
        end);
    end);

  {  Main loop  }
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
