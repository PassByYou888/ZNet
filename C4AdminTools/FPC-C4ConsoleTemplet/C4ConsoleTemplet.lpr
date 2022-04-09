program C4ConsoleTemplet;

{$mode objfpc}{$H+}

uses
  jemalloc4p,
  { https://github.com/PassByYou888/jemalloc4p } {$IFNDEF MSWINDOWS} cthreads, {$ENDIF MSWINDOWS}
  Classes,
  SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UPascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.ListEngine,
  Z.GHashList,
  Z.Expression,
  Z.OpCode,
  Z.Parsing,
  Z.DFE,
  Z.TextDataEngine,
  Z.MemoryStream,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_UserDB,
  Z.Net.C4_Var,
  Z.Net.C4_FS,
  Z.Net.C4_RandSeed,
  Z.Net.C4_Log_DB,
  Z.Net.C4_XNAT,
  Z.Net.C4_Alias,
  Z.Net.C4_FS2,
  Z.Net.C4_PascalRewrite_Client,
  Z.Net.C4_PascalRewrite_Service,
  Z.Net.C4_NetDisk_Service, Z.Net.C4_NetDisk_Client, Z.Net.C4_NetDisk_Directory,
  Z.Net.C4_TEKeyValue,
  Z.Net.C4_Console_APP;

var
  exit_signal: boolean;

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

begin
  StatusThreadID := False;
  Z.Net.C4_Console_APP.C40_Init_AppParamFromSystemCmdLine;
  if Z.Net.C4_Console_APP.C40_Extract_CmdLine then
  begin
    exit_signal := False;
    TCompute.RunC_NP(@Do_Check_On_Exit);
    while not exit_signal do
      Z.Net.C4.C40Progress;
  end;

  Z.Net.C4.C40Clean;
end.
