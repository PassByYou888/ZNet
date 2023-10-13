program C4_For_Android_Server;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_FS,
  Z.Net.C4_FS2,
  Z.Net.C4_UserDB,
  Z.Net.C4_Var,
  Z.Net.C4_Log_DB,
  Z.Net.C4_TEKeyValue,
  Z.Status,
  Z.Net.C4_Console_APP;

const
  Internet_DP_Addr_ = '192.168.2.32';
  Internet_DP_Port_ = 8387;

begin
  Z.Net.C4.C40_QuietMode := False;

  // build service
  with Z.Net.C4.TC40_PhysicsService.Create(Internet_DP_Addr_, Internet_DP_Port_, Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      BuildDependNetwork('dp|FS|FS2|Var|UserDB|TEKeyValue|Log');
      StartService;
    end;

  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
