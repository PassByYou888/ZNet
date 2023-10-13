program C4_Auto_Deployment_Server_VM;

{$APPTYPE CONSOLE}


uses
  Z.Core,
  Z.PascalStrings,
  Z.UPascalStrings,
  Z.Status,
  Z.UnicodeMixedLib,
  Z.ListEngine,
  Z.Geometry2D,
  Z.DFE,
  Z.Json,
  Z.Expression,
  Z.OpCode,
  Z.Parsing,
  Z.Notify,
  Z.Cipher,
  Z.MemoryStream,
  Z.HashList.Templet,
  Z.ZDB2,
  Z.ZDB2.Thread.Queue,
  Z.ZDB2.Thread,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_UserDB,
  Z.Net.C4_Log_DB,
  Z.Net.C4_Console_APP,
  C4_Auto_Deployment_IMP_VM_Serv in 'C4_Auto_Deployment_IMP_VM_Serv.pas',
  C4_Auto_Deployment_IMP_VM_Cli in 'C4_Auto_Deployment_IMP_VM_Cli.pas';

{$R *.res}


begin
  StatusThreadID := False;

  {  Starting TC40AppTempletForm through script automation  }
  C40AppParsingTextStyle := tsC;
  if Z.Net.C4_Console_APP.C40_Extract_CmdLine([
      'Title("Runtime Backcall tech demo service")',
      'AppTitle("Runtime Backcall tech demo service")',
      'DisableUI(True)',
      'Service("0.0.0.0","127.0.0.1","8991","UserDB|Log")',
      'Auto("127.0.0.1","8991","UserDB|Log")']) then
    begin
      with TAuto_Deployment_Service.Create('') do
          StartService('0.0.0.0', '8990', '123456');
      C40_Execute_Main_Loop;
    end;
  Z.Net.C4.C40Clean;

end.
