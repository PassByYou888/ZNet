{ ****************************************************************************** }
{ * Low MemoryHook                                                             * }
{ ****************************************************************************** }

unit Z.MH1;

{$I Z.Define.inc}

interface

uses Z.ListEngine, Z.Core;

procedure BeginMemoryHook; overload;
procedure BeginMemoryHook(cacheLen: Integer); overload;
procedure EndMemoryHook;
function GetHookMemorySize: nativeUInt; overload;
function GetHookMemorySize(p: Pointer): nativeUInt; overload;
function GetHookMemoryMinimizePtr: Pointer;
function GetHookMemoryMaximumPtr: Pointer;
function GetHookPtrList: TPointerHashNativeUIntList;
function GetMemoryHooked: TAtomBool;

implementation

var
  HookPtrList: TPointerHashNativeUIntList;
  MemoryHooked: TAtomBool;

{$IFDEF FPC}
{$I Z.MH_fpc.inc}
{$ELSE}
{$I Z.MH_delphi.inc}
{$ENDIF}


initialization

InstallMemoryHook;

finalization

UnInstallMemoryHook;

end.
