program C4_Auto_Deployment_Server;

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
  Z.AI.RealTimeVideo.Info,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_UserDB,
  Z.Net.C4_Var,
  Z.Net.C4_FS,
  Z.Net.C4_RandSeed,
  Z.Net.C4_Log_DB,
  Z.Net.C4_Alias,
  Z.Net.C4_FS2,
  Z.Net.C4_TEKeyValue,
  Vcl.Forms,
  C4_Auto_Deployment_IMP_Serv in 'C4_Auto_Deployment_IMP_Serv.pas',
  C4_Auto_Deployment_IMP_Cli in 'C4_Auto_Deployment_IMP_Cli.pas',
  C40AppTempletFrm in '..\..\C4AdminTools\Delphi-C4AppTemplet\C40AppTempletFrm.pas' {C40AppTempletForm};

{$R *.res}


begin
  // 通过脚本自动化启动TC40AppTempletForm
  C40AppParsingTextStyle := tsC;
  C40AppParam := [
    'Title("Runtime Backcall tech demo service")',
    'AppTitle("Runtime Backcall tech demo service")',
    'DisableUI(True)',
    'Service("0.0.0.0","127.0.0.1","8990","Auto_Deployment_Demo|UserDB|Log")',
    'Auto("127.0.0.1","8990","UserDB|Log")'];

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TC40AppTempletForm, C40AppTempletForm);
  Application.Run;

end.
