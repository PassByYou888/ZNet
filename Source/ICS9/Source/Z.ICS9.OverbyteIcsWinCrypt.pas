unit Z.ICS9.OverbyteIcsWinCrypt;

{ June 2015 - Angus renamed from WinCrypt and moved to main source dir       }
{ Nov 2, 2018 - V8.58 - Use namespaces to keep FMX happy }
{ Aug 10, 2021 - V8.67  Added ncryptdll }
{ Oct 14, 2021 - V8.68 - Trying to keep C++ happy }
{ Aug 08, 2023 V9.0  Updated version to major release 9. }

{ note this unit includes a Jedi header file JwaWinCrypt rnmamed to }
{ OverbyteIcsJwaWinCrypt.inc with conditionals to make it independent of JCL }
{ and stop it being compiled alone }

{$IFNDEF BCB}             { V8.68 }
  {$WEAKPACKAGEUNIT}
{$ENDIF}

{$I Include\Z.ICS9.OverbyteIcsDefs.inc}

interface
{$IFDEF MSWINDOWS}

uses
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF};

type
  {$EXTERNALSYM LPBYTE}
  LPBYTE      = PByte;
  {$EXTERNALSYM GUID}
  GUID        = TGUID;
  {$EXTERNALSYM PVOID}
  PVOID       = Pointer;
  {$EXTERNALSYM LPVOID}
  LPVOID      = Pointer;
  {$EXTERNALSYM LPLPVOID}
  LPLPVOID    = PPointer;
  {$EXTERNALSYM LPLPSTR}
  LPLPSTR     = PLPSTR;
  {$EXTERNALSYM LPLPWSTR}
  LPLPWSTR    = PLPWSTR;
  {$EXTERNALSYM LPLPCSTR}
  LPLPCSTR    = ^LPCSTR;
  {$EXTERNALSYM LPLPCWSTR}
  LPLPCWSTR   = ^LPCWSTR;
  {$EXTERNALSYM LPLPCTSTR}
  LPLPCTSTR   = ^LPCTSTR;
{$IFNDEF WIN64}
  {$EXTERNALSYM ULONG_PTR}
  ULONG_PTR   = LongWord;
  {$EXTERNALSYM size_t}
  size_t      = LongWord;
{$ELSE}
  {$EXTERNALSYM ULONG_PTR}
  ULONG_PTR   = NativeUInt;
  {$EXTERNALSYM size_t}
  size_t      = NativeUInt;
{$ENDIF}
  {$EXTERNALSYM LPINT}
  LPINT       = ^Integer;
  {$EXTERNALSYM LPFILETIME}
  LPFILETIME  = PFileTime;
  {$EXTERNALSYM LONG}
  LONG        = Longint;
  {$EXTERNALSYM HANDLE}
  HANDLE      = THANDLE;

{$DEFINE JWA_INCLUDEMODE}
{$DEFINE JWA_INTERFACESECTION}
{$I Z.ICS9.OverbyteIcsJwaWinCrypt.inc}
{$ENDIF}

implementation
{$IFDEF MSWINDOWS}

const
  advapi32  = 'advapi32.dll';
  crypt32   = 'crypt32.dll';
  softpub   = 'softpub.dll';
  ncryptdll = 'ncrypt.dll';
{$IFDEF UNICODE}
  AWSuffix = 'W';
{$ELSE}
  AWSuffix = 'A';
{$ENDIF UNICODE}

{$UNDEF JWA_INTERFACESECTION}
{$DEFINE JWA_IMPLEMENTATIONSECTION}
{$I Z.ICS9.OverbyteIcsJwaWinCrypt.inc}

{$ENDIF}
end.
