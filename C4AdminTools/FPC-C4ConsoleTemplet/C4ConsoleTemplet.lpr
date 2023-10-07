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
  Z.HashList.Templet,
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
  Z.Net.C4_NetDisk_Service, Z.Net.C4_NetDisk_Client,
  Z.Net.C4_NetDisk_VM_Service, Z.Net.C4_NetDisk_VM_Client,
  Z.Net.C4_NetDisk_Directory,
  Z.Net.C4_TEKeyValue,
  Z.Net.C4_Console_APP;

begin
  StatusThreadID := False;
  Z.Net.C4_Console_APP.C40_Init_AppParamFromSystemCmdLine;
  if Z.Net.C4_Console_APP.C40_Extract_CmdLine then
      Z.Net.C4_Console_APP.C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;
end.
