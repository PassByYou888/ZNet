{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Windows only tools, accessing Windows registry, firewall,
              services, tasks, hardware, simple encyrption for passwords, etc.
              Built from selected Magenta Systems libraries.
              Common tools needed to build and control Windows Service apps.
Warnings:     This unit is NOT intended for use by ICS components, only by
              applications and ICS samples. This unit is not included in
              any ICS packsges.
              only currently supported on modern unicode compilers, too many
              Windows API functions missing in old versions of Delphi.
Creation:     Jan 2025
Updated:      July 2025
Version:      V9.5
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2002-2025 by Angus Robertson, Magenta Systems Ltd,
              Croydon, England. delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.


Feb 9, 2025  V9.4  Baseline built from selected Magenta Systems libraries,
                     MagServices, MagTaskWin, MagFirewall, MagSubs1.
Jul 23, 2025 V9.5  Added Windows memory reporting functions IcsMemInfoProg,
                     IcsMemInfoGlob and IcsMemInfoPerf.
                   Added IcsMemWarning to check for low or critical memory
                     problems, returns Warning at 85% physical or page file
                     usage, critical at 95% usage (reboot probably required).



Note GetPerformanceInfo fails under Win32 on Windows 11 24H2, no data or error
returned and memory corrupted, so conditionally only used for Win64.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Z.ICS9.OverbyteIcsWinUtils;

{$IFDEF MSWINDOWS}

{$I Include\Z.ICS9.OverbyteIcsDefs.inc}

{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$H+}             { Use long strings                    }
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF Rtl_Namespaces}WinApi.WinSvc{$ELSE}WinSvc{$ENDIF},
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    {$Ifdef Rtl_Namespaces}System.Variants{$Else}Variants{$Endif},
    {$IFDEF Rtl_Namespaces}System.Win.Registry{$ELSE}Registry{$ENDIF},
    {$IFDEF Rtl_Namespaces}System.Win.ComObj{$ELSE}ComObj{$ENDIF},
    {$IFDEF Rtl_Namespaces}WinAPI.ActiveX{$ELSE}ActiveX{$ENDIF},
    {$IFDEF Rtl_Namespaces}WinAPI.Psapi{$ELSE}Psapi{$ENDIF},
    {$IFDEF Rtl_Namespaces}WinAPI.ShellAPI{$ELSE}ShellAPI{$ENDIF},
    Z.ICS9.OverbyteIcsUtils,
    Z.ICS9.OverbyteIcsTypes;

var
  InstanceMutexHandle: THandle = 0;

type
  TServFunc = (servfCheck, servfStart, servfStop) ;
  TIcsMemWarn = (MemWarnNone, MemWarnPoor, MemWarnCritical);    { V9.5 result for IcsMemWarning }


// Delphi winsvc.pas does not include all APIs added for Windows 2000 and later, so here are a few
// Service description string, Unicode only
type
    _SERVICE_DESCRIPTIONW = record
        lpDescription: LPWSTR ;
    end;
    SERVICE_DESCRIPTIONW = _SERVICE_DESCRIPTIONW ;
    PSERVICE_DESCRIPTIONW = ^_SERVICE_DESCRIPTIONW ;
    TServiceDescriptionW = _SERVICE_DESCRIPTIONW ;
    PServiceDescriptionW = ^_SERVICE_DESCRIPTIONW ;

// Actions to take on service failure
type
   _SC_ACTION_TYPE = DWORD ;
    SC_ACTION_TYPE = _SC_ACTION_TYPE ;
    TScActionType = _SC_ACTION_TYPE ;

const
    SC_ACTION_NONE          = 0 ;
    SC_ACTION_RESTART       = 1 ;
    SC_ACTION_REBOOT        = 2 ;
    SC_ACTION_RUN_COMMAND   = 3 ;

    Ics_SERV_ADMIN = SERVICE_ALL_ACCESS;     { ICS SCM access literals }
    Ics_SCM_ADMIN = SC_MANAGER_ALL_ACCESS;   { ICS SCM access literals }
    Ics_SCM_QUERY = GENERIC_READ;

type
    _SC_ACTION = record
        aType : SC_ACTION_TYPE ;
        Delay : DWORD ;
    end;
    SC_ACTION = _SC_ACTION ;
    PSC_ACTION = ^_SC_ACTION ;
    TScAction = _SC_ACTION ;
    PScAction = ^_SC_ACTION ;

type
    _SERVICE_FAILURE_ACTIONSW = record
        dwResetPeriod: DWORD ;
        lpRebootMsg: LPWSTR ;
        lpCommand: LPWSTR ;
        cActions: DWORD ;
        lpsaActions : PSC_ACTION ;
    end;
    SERVICE_FAILURE_ACTIONSW = _SERVICE_FAILURE_ACTIONSW ;
    PSERVICE_FAILURE_ACTIONSW = ^_SERVICE_FAILURE_ACTIONSW ;
    TServiceFailureActionW = _SERVICE_FAILURE_ACTIONSW ;
    PServiceFailureActionW = ^_SERVICE_FAILURE_ACTIONSW ;

// Windows Advacnced Firewall constants, using OLE
Const
  NET_FW_PROFILE_DOMAIN = 0;
  NET_FW_PROFILE_STANDARD = 1;
  NET_FW_PROFILE_CURRENT  = 2;

  NET_FW_IP_PROTOCOL_UDP = 17;
  NET_FW_IP_PROTOCOL_TCP = 6;
  NET_FW_IP_PROTOCOL_ICMPv4 = 1;
  NET_FW_IP_PROTOCOL_ICMPv6 = 58;
  NET_FW_IP_PROTOCOL_ANY = 256;

  NET_FW_IP_VERSION_V4 = 0;
  NET_FW_IP_VERSION_V6 = 1;
  NET_FW_IP_VERSION_ANY = 2;

  NET_FW_SCOPE_ALL = 0;
  NET_FW_SCOPE_LOCAL_SUBNET = 1;
  NET_FW_SCOPE_CUSTOM = 2;

  NET_FW_SERVICE_FILE_AND_PRINT = 0;
  NET_FW_SERVICE_UPNP = 1;
  NET_FW_SERVICE_REMOTE_DESKTOP = 2;
  NET_FW_SERVICE_NONE = 3;

  NET_FW_PROFILE2_DOMAIN  = 1;
  NET_FW_PROFILE2_PRIVATE = 2;
  NET_FW_PROFILE2_PUBLIC  = 4;
  NET_FW_PROFILE2_ALL     = $7fffffff;

  NET_FW_RULE_DIR_IN = 1;
  NET_FW_RULE_DIR_OUT = 2;

  NET_FW_ACTION_BLOCK = 0;
  NET_FW_ACTION_ALLOW = 1;

  NET_FW_MODIFY_STATE_OK = 0;
  NET_FW_MODIFY_STATE_GP_OVERRIDE = 1;
  NET_FW_MODIFY_STATE_INBOUND_BLOCKED = 2;

  NET_FW_EDGE_TRAVERSAL_TYPE_DENY = 0;
  NET_FW_EDGE_TRAVERSAL_TYPE_ALLOW = 1;
  NET_FW_EDGE_TRAVERSAL_TYPE_DEFER_TO_APP = 2;
  NET_FW_EDGE_TRAVERSAL_TYPE_DEFER_TO_USER = 3;

  NET_FW_RULE_CATEGORY_BOOT = 0;
  NET_FW_RULE_CATEGORY_STEALTH = 1;
  NET_FW_RULE_CATEGORY_FIREWALL	= 2;
  NET_FW_RULE_CATEGORY_CONSEC = 3;

type
  TFirewallDir = (FirewallNone, FirewallIn, FirewallOut, FirewallBoth);

// build list of Windows, with handles so they can be closed
  TIcsWindowObject = record
                    WinHandle  : HWnd;    {Window Handle}
                    WinCaption : String;  {Window Caption Text (If any)}
                    ProcessID  : DWord;   {Process the window belongs to}
                    IsVisible  : Boolean; {Is the window visible?}
                    IsEnabled  : Boolean; {Is the window enabled for mouse/keyboard input?}
                    IsIconic   : Boolean; {Is the window minimized?}
                    WindowRect : TRect;   {Window Dimensions}
                    ThreadId   : DWord; {thread that created the window - Angus }
                    WinClass   : String;  {Window Class (If any)}
                    {Add more properties here if you like, then fill them in at the WindowCallback function.}
                  end;
  PTIcsWindowObject = ^TIcsWindowObject;

  TIcsProcessObject = record
                    ProcessID: DWORD;       // this process
                    ExeFile: string ;       // Path
                    DefaultHeapID: DWORD;  // NOTE, none of the following currently set
                    ModuleID: DWORD;        // associated exe
                    CountThreads: DWORD;
                    ParentProcessID: DWORD; // this process's parent process
                    PriClassBase: Longint;  // Base priority of process's threads
                    Flags: DWORD;
                end ;
  PTIcsProcessObject = ^TIcsProcessObject;

  TIcsWindowList = class(TComponent)
  private
    WindowLst : TList;
    FCount : Integer;
  protected
    Function GetAWindow(Index : Integer) : TIcsWindowObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    Procedure Refresh;
    Property Windows[Index : Integer]: TIcsWindowObject read GetAWindow;
    Property Count : Integer read FCount;
  published
    { Published declarations }
  end;

// build list of processes, so they can be closed
  TIcsProcessList = class(TComponent)
  private
    ProcessLst : TList;
    FCount : Integer;
    FOnlyExe : boolean ;
  protected
    Function GetAProcess(Index : Integer) : TIcsProcessObject;
    procedure GetWinNT ;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;

    Procedure Refresh;
    Property Process[Index : Integer]: TIcsProcessObject read GetAProcess;
    Property Count : Integer read FCount;
    Property OnlyExe : boolean read FOnlyExe write FOnlyExe ;
  published
    { Published declarations }
  end;

// simple functions
function IcsFormatLastError: string ;
function IcsCheckUniqueInstance: Boolean ;
function IcsIsWin64: Boolean;
function IcsIsWow64: Boolean ;
function IcsStrBXEncrypt(const S: String; Key: Word): String;
function IcsStrBXDecrypt(const S: String; Key: Word): String;
function IcsStrBXDecryptEx(const S: String; Key: Word): String;

// various functions for running programs and checking if they are running
function IcsWinExecAndWait(const aCmdLine: String; Visibility: Word): integer;
function IcsStartExe (const aCmdLine, aWorkDir: String; ShowState: Word): TProcessInformation ;
function IcsfileExec(const aCmdLine, aWorkDir: String; ShowState: Word; aWait: Boolean): Boolean ;
function IcsGetExePID (const AppName: string): DWORD ;
function IcsGetPIDWin (PID: DWORD): DWORD ;
function IcsGetExeWin (const AppName: string): DWORD ;
function IcsFileShellOpenEx (aFile: String; var PID: longword): boolean ;
function IcsfileGetOpen (aFile: String; var ExeName: string): boolean ;
function IcsCheckExePID (PID: DWORD): boolean ;
function IcsTermPHandle (PHandle: THandle; exitcode: integer): boolean ;
function IcsCheckPHandle (PHandle: THandle): boolean ;
function IcsCloseExe (PID: DWORD; WinMess: Integer = WM_CLOSE): boolean ;    // Aug 2024 allow specific message
function IcsCloseExeEx (PID: DWORD; WinMess: Integer = WM_CLOSE): boolean ;    // Aug 2024 all windows
function IcsGetExeName(PID: DWORD): String;                      // Aug 2024
function IcsCurProcID: DWORD;                                    // July 2024
function IcsCheckPID (PID: DWORD): boolean ;                     // July 2024
function IcsTermPID (PID: DWORD; exitcode: integer): boolean ;
function IcsGetWorkingSetSize : DWORD;
procedure IcsGetConsoleOutputWait (const CommandLine: string; const WaitSecs: integer; var Output : TStringList);

// functions to control and install Windows Service applications
function IcsSCMOpen(NewAccess: DWORD; var ServResp: string): Boolean;
procedure IcsSCMClose;
function IcsCrtlService(const Sname: string; ServFunc: TServFunc; Wait: integer; var ServState: Integer; var ServResp: string): boolean ;
function IcsInstService(const Sname, Dname, DriverPath: string; ServiceType, ServiceStart: DWORD ; const Account,
        Password: string; Dependencies: PWideChar; const Descr: string ; RestartDelay: integer ; var Resp: string): boolean ;
function IcsRemService(const Sname: string; var Resp: string): boolean ;
function IcsSetDescrServ(const Sname, Descr: string; var Resp: string): boolean ;

// functions to access the Local Machine registry, need admin rights to write
function IcsRegGetHlm(const RegKey, RegName: String): String;
function IcsRegPutHlm(const RegKey, RegName, RegValue: String): Boolean;

// Windows Adcanced Firewall, list installed rules, add or remove rules
// needed for Windows servers to listen without being blocked by the firewall.
// currently enables apps, rather than specific IPs and ports.
function IcsFireWallRulesEnum(const Search: String): String;
function IcsFireWallRulesAdd(const EntryName, GroupName, Descr, AppPathAndExe: string; Direction: TFirewallDir = FirewallNone): String;

// memory reporting functions
//function IcsGetProcessHandles(ProcId: DWORD): Integer;  {V9.5 }
function IcsMemInfoProg: String;      { V9.5 }
function IcsMemInfoGlob: String;      { V9.5 }
function IcsMemInfoPerf: String;      { V9.5 }
function IcsMemWarning(var Info: String; MemPer: Cardinal = 85; PagePer: Cardinal = 85; FatalPer: Cardinal = 95): TIcsMemWarn;  { V9.5 }

var
    ServiceCDatabaseHandle: SC_HANDLE;  // Windows Service control database handle
    ServiceAccess: LongWord ;

// from XE2
{  July 2025
type
  DWORDLONG = UInt64;
  PMemoryStatusEx = ^TMemoryStatusEx;
  _MEMORYSTATUSEX = record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: DWORDLONG;
    ullAvailPhys: DWORDLONG;
    ullTotalPageFile: DWORDLONG;
    ullAvailPageFile: DWORDLONG;
    ullTotalVirtual: DWORDLONG;
    ullAvailVirtual: DWORDLONG;
    ullAvailExtendedVirtual: DWORDLONG;
  end;
  TMemoryStatusEx = _MEMORYSTATUSEX;
  MEMORYSTATUSEX = _MEMORYSTATUSEX;
  LPMEMORYSTATUSEX = PMemoryStatusEx;   }
//function GlobalMemoryStatusEx(var lpBuffer : TMEMORYSTATUSEX): BOOL; stdcall;
//function GlobalMemoryStatusEx; external kernel32 name 'GlobalMemoryStatusEx';

type
  _PROCESS_MEMORY_COUNTERS_EX2 = record     { V9.5 missing from D12 }
    cb: DWORD;
    PageFaultCount: DWORD;
    PeakWorkingSetSize: SIZE_T;
    WorkingSetSize: SIZE_T;
    QuotaPeakPagedPoolUsage: SIZE_T;
    QuotaPagedPoolUsage: SIZE_T;
    QuotaPeakNonPagedPoolUsage: SIZE_T;
    QuotaNonPagedPoolUsage: SIZE_T;
    PagefileUsage: SIZE_T;
    PeakPagefileUsage: SIZE_T;
    PrivateUsge: SIZE_T;
    PrivateWorkingSetSize: SIZE_T;
    SharedCommitUsage: ULONG64;
  end;
  {$EXTERNALSYM _PROCESS_MEMORY_COUNTERS_EX2}
  PROCESS_MEMORY_COUNTERS_EX2 = _PROCESS_MEMORY_COUNTERS_EX2;
  {$EXTERNALSYM PROCESS_MEMORY_COUNTERS_EX2}
  PPROCESS_MEMORY_COUNTERS_EX2 = ^_PROCESS_MEMORY_COUNTERS_EX2;
  {$EXTERNALSYM PPROCESS_MEMORY_COUNTERS_EX2}
  TProcessMemoryCountersEx2 = _PROCESS_MEMORY_COUNTERS_EX2;
  PProcessMemoryCountersEx2 = ^_PROCESS_MEMORY_COUNTERS_EX2;

    TPerformanceInformation = record
        cb                 : DWORD ;
        CommitTotal        : SIZE_T ;
        CommitLimit        : SIZE_T ;
        CommitPeak         : SIZE_T ;
        PhysicalTotal      : SIZE_T ;
        PhysicalAvailable  : SIZE_T ;
        SystemCache        : SIZE_T ;
        KernelTotal        : SIZE_T ;
        KernelPaged        : SIZE_T ;
        KernelNonpaged     : SIZE_T ;
        PageSize           : SIZE_T ;
        HandleCount        : DWORD ;
        ProcessCount       : DWORD ;
        ThreadCount        : DWORD ;
    end ;
    TpPerformanceInformation = ^TPerformanceInformation ;


var
    hPerfApi : THandle;
// function GetPerformanceInfo(pPerformanceInformation: TpPerformanceInformation; cb: DWord) : Integer ; external kernel32 name 'K32GetPerformanceInfo';
    GetPerformanceInfo: Function(pPerformanceInformation: TpPerformanceInformation; cb: DWord) : Integer ;  { V9.5 }

{$ENDIF MSWINDOWS}

implementation

{$IFDEF MSWINDOWS}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ check if another instance of this application is running, by file name, no path }
function IcsCheckUniqueInstance: Boolean ;
var
    AppString: WideString ;
    I: integer ;
begin
    AppString := Uppercase (ExtractFileName (ParamStr(0))) ;
    if AppString = '' then
        AppString := 'UNKNOWN' ;
    AppString := 'APPLICATION-' + AppString ;
    for I := 1 to Length (AppString) do begin
        if AppString [I] = '\' then
            AppString [I] := '_';
    end;

// Check to see if the mutex is already there
    InstanceMutexHandle := OpenMutexW (MUTEX_ALL_ACCESS, false, PWideChar (AppString)) ;

// no handle, this is the first instance, create new mutex
    if InstanceMutexHandle = 0 then begin
        InstanceMutexHandle := CreateMutexW (nil, false, PWideChar (AppString)) ;
    // Error checking to see if anyone beat us...
        if InstanceMutexHandle = 0 then
            result := false
        else
            result := true;
    end
    else
        result := false;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFillStr (const Ch : Char; const N : Integer): string;
var
  I: integer ;
begin
    SetLength (Result, N);
    for I := 1 to N do
        Result [I] := Char (Ch) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsPadChLeftStr (const S : string; const Ch : Char; const Len : Integer): string;
var
    N: Integer;
begin
    N := Length (S);
    if N < Len then
        Result := IcsFillStr (Ch, Len - N) + S
    else
        Result := S;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// add leading zeros
function IcsInt2StrZ (L, Len: Integer): String;
begin
    Result := IntToStr (L);
    Result := IcsPadChLeftStr (Copy (Result, 1, Len), '0', Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// must turn off range checking or encyption stuff dies !!!!!
{$R-}
{$Q-}
const
  D2 = 52845;

// simple string encryption with numeric key, binary output, max 254 long
function IcsStrREncrypt(const S: AnsiString; Key: Word): AnsiString;
var
    I, Len: integer;
    D1: word ;
    Outstr: string [255] ;
begin
    Result := '';
    Len := Length(S) ;
    if Len = 0 then
        Exit;
    Randomize;
    D1 := Random (65535) ;
    Key := Key + D1 ;
    Outstr [0] := AnsiChar (Len + 2) ;
    Move (D1, Outstr [1], 2) ;   // keep multiply value
    for I := 1 to Len do begin
        Outstr [I + 2] := AnsiChar (Ord (S [I]) xor (Key shr 8));
        Key := (Ord (Outstr [I + 2]) + Key) * D1 + D2;
    end ;
    Result := Copy (Outstr, 1, Len + 2) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// simple string decryption with numeric key, binary input, max 254 long
function IcsStrRDecrypt(const S: AnsiString; Key: Word): AnsiString;
var
    I, Len: Integer;
    D1: word ;
    Outstr: string [255] ;
begin
    Result := '';
    len := Length(S) ;
    if len < 2 then
        Exit;
    Move (S [1], D1, 2) ;   // keep multiply value
    Key := Key + D1 ;
    Outstr [0] := AnsiChar (Len - 2) ;
    for I := 1 to Len do begin
        Outstr[I] := AnsiChar (Ord (S [I + 2]) xor (Key shr 8));
        Key := (Ord (S [I + 2]) + Key) * D1 + D2;
    end ;
    Result := Copy (Outstr, 1, Len - 2) ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$R+}
{$Q+}
// simple string encryption with numeric key, Base64 output, max 254 long
function IcsStrBXEncrypt(const S: String; Key: Word): String;
var
    Temp: AnsiString ;
begin
    Result := '' ;
    if Length (S) > 254 then
        Exit ;
    Temp := IcsStrREncrypt (AnsiString (S), Key) ;
    Temp :=  AnsiString (IcsInt2StrZ (Length (Temp), 3)) + Temp ;  // add length to start
    Result := String(IcsBase64encodeA (temp)) ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// simple string decryption with numeric key, base64 input, max 254 long
// returns blank if illegal
function IcsStrBXDecrypt(const S: String; Key: Word): String;
var
    Keylen: integer;
    Temp: AnsiString ;
begin
    Result := '' ;
    try
        Temp := IcsBase64decodeA (AnsiString(S)) ;
        if length (temp) < 4 then
            Exit ;
        Keylen := atoi(copy (Temp, 1, 3)) ;  // get length from start
        if Keylen = 0 then
            exit ;
        if (Keylen + 3) <> length (Temp) then
            Exit ;
        Temp :=  Copy (Temp, 4, 999) ;
        Result := String (IcsStrRDecrypt (Temp, Key)) ;
    except ;
        Result := '' ;
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// simple string decryption with numeric key, base64 input, max 254 long
// if decryption fails, assume it was clear text and return input
function IcsStrBXDecryptEx(const S: String; Key: Word): String;
var
    Keylen, Slen: integer;
    Temp: AnsiString ;
begin
    Result := '';
    if S = '' then
        Exit ;
    try
        Temp := IcsBase64decodeA (AnsiString(S)) ;
        Slen := length (Temp) ;
        if (Slen = 3) and (temp = '000') then
            Exit ;  // blank encoded string
        if (Slen > 3) then begin
            Keylen := atoi(Copy (Temp, 1, 3)) ;  // get length from start
            if (Keylen + 3) = Slen then begin
                temp := Copy (Temp, 4, 999) ;
                Result := String (IcsStrRDecrypt (Temp, Key)) ;
            end;
        end;
        if Result = '' then
            Result := S ;  // input
    except ;
        Result := '' ;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsFormatLastError: string ;
begin
    result := SysErrorMessage (GetLastError) + ' [' + IntToStr (GetLastError) + ']' ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsSCMClose;
begin
    if ServiceCDatabaseHandle <> 0 then begin
        CloseServiceHandle (ServiceCDatabaseHandle) ;
        ServiceCDatabaseHandle := 0;
        ServiceAccess := 0;
    end;

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// open Service Control Manager, generally stays open until app closes
// SERVICE_ALL_ACCESS requires app to have administrator rights, will fail otherwise
function IcsSCMOpen(NewAccess: DWORD; var ServResp: string): Boolean;
begin
    Result := False;
 // if opened for queries only, see if need to reopen with write access
    if (ServiceAccess <> 0) and (ServiceCDatabaseHandle <> 0) then begin
         if (ServiceAccess = Ics_SCM_QUERY) and (NewAccess = Ics_SCM_ADMIN) then
            IcsSCMClose;
    end;
    if ServiceCDatabaseHandle = 0 then begin
        if IcsIsProgAdmin then
            NewAccess := Ics_SCM_ADMIN;    // avoid doing it later
        ServiceCDatabaseHandle := OpenSCManager (Nil , Nil, NewAccess) ;
        if (ServiceCDatabaseHandle <> 0) then begin
            Result := True;
            ServiceAccess := NewAccess;
        end
        else begin
            if (NOT IcsIsProgAdmin) and (NewAccess = Ics_SCM_ADMIN) then
                ServResp := 'Need Admin Rights to Control Windows Services'
            else
                ServResp := 'Failed to Open Service Manager: ' + IcsFormatLastError + ', Handle: ' + IntToStr(ServiceCDatabaseHandle);
        end;
    end
    else
        Result := True;
    if Result then
        ServResp := 'Service Control Manager Opened OK';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetServResp(ServState: Integer): String;
begin
    Result := 'Unknown';
    case ServState of
        SERVICE_STOPPED: Result := 'Service Stopped';
        SERVICE_START_PENDING: Result := 'Service Start Pending';
        SERVICE_STOP_PENDING: Result := 'Service Stop Pending';
        SERVICE_RUNNING: Result := 'Service Runnning';
        SERVICE_CONTINUE_PENDING: Result := 'Service Continue Pending';
        SERVICE_PAUSE_PENDING : Result := 'Service Pause Pending';
        SERVICE_PAUSED: Result := 'Service Paused';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// control Windows Service application, start/stop/check
// returns service state and desription, false if unable to access service }
// wait units are 50ms using sleep, zero none, no messages are processed!!!
function IcsCrtlService(const Sname: string; ServFunc: TServFunc; Wait: integer; var ServState: Integer; var ServResp: string): boolean ;
var
    Args: PChar ;
    LocalHandle: SC_HANDLE ;
    ServiceStatus: TServiceStatus;
    WideName: WideString ;  // Unicode
    I: Integer;
    NewAccess, SrvAccess: DWORD;

    procedure GetServiceState;
    begin
        ServState := -1;
        if NOT QueryServiceStatus (LocalHandle, ServiceStatus) then begin
            ServResp := 'Failed to Query Service: ' + IcsFormatLastError ;
            exit ;
        end ;
        Result := True ;
        ServState := ServiceStatus.dwCurrentState;
        ServResp := IcsGetServResp(ServState);
    end;

begin
    Result := false ;
    ServResp := '' ;
    ServState := 0;
    WideName := Sname ;
    FillChar (ServiceStatus, SizeOf (ServiceStatus), 0) ;
    if ServFunc = servfCheck then begin
        NewAccess := Ics_SCM_QUERY;
        SrvAccess := Ics_SCM_QUERY;
    end
    else begin
        NewAccess := Ics_SCM_ADMIN;
        SrvAccess := Ics_SERV_ADMIN;
    end;
    if NOT IcsSCMOpen(NewAccess, ServResp) then
        exit ;
    try // except
        LocalHandle := OpenServiceW (ServiceCDatabaseHandle, PWideChar (WideName), SrvAccess) ;
        if LocalHandle <> 0 then begin
            try
                GetServiceState;
                if ServFunc = servfCheck then   // nothing more to do
                    exit
                else if ServFunc = servfStop then begin
                    if (ServState = SERVICE_STOPPED) then begin
                        ServResp := 'Service Already Stopped' ;
                        exit ;
                    end ;
                    if NOT ControlService (LocalHandle, SERVICE_CONTROL_STOP, ServiceStatus) then begin
                        ServResp := 'Failed to Stop Service: ' + IcsFormatLastError;
                        Result := False;
                        exit ;
                    end ;
                    ServState := ServiceStatus.dwCurrentState;
                    if (ServState = SERVICE_STOPPED) then begin
                        ServResp := 'Service Stopped OK' ;
                        exit ;
                    end ;
                    ServResp := 'Unable to Confirm Service Stopped' ;
                end
                else if ServFunc = servfStart then begin
                    if (ServState = SERVICE_RUNNING) then begin
                        ServResp := 'Service Already Runnning' ;
                        exit ;
                    end ;
                    if NOT StartService (LocalHandle, 0, Args) then begin   // does not wait until started
                        ServResp := 'Failed to Start Service: ' +  IcsFormatLastError;
                        Result := False;
                        exit ;
                    end ;
                    ServResp := 'Unable to Confirm Service Started' ;
                end ;
                GetServiceState;   // should return pending for start or stop

             // wait for something to happen, might take a long time to stop or start
             // units are 50ms
               if Wait > 0 then begin
                    for I := 1 to Wait do begin
                        GetServiceState;
                        if (ServFunc = servfStart) and (ServState = SERVICE_RUNNING) then begin
                            ServResp := 'Service Run OK' ;
                            exit ;
                        end ;
                        if (ServFunc = servfStop) and (ServState = SERVICE_STOPPED) then begin
                            ServResp := 'Service Stopped OK' ;
                            exit ;
                        end ;
                        Sleep (50) ;
                    end;
               end;
            finally
                CloseServiceHandle (LocalHandle) ;
            end ;
        end
        else
            ServResp := 'Failed to Open Service: ' + Sname + ' - ' + IcsFormatLastError;
    except
        on E:Exception do begin
            ServResp := 'Exception Controlling Service - ' + E.Message ;
            Result := False;
        end;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// install new service or update existing service - with logon
// Sname is short name, DName display name, Descr long description
// ServiceType is usually SERVICE_WIN32_OWN_PROCESS
// ServiceStart is SERVICE_DEMAND_START or SERVICE_AUTO_START
// Account/Password if not blank are account to use, needs .\ at start
// Dependencies is a MultiSZ string of service names
// RestartDelay if none-zero is restart on failure aftrer how many seconds for ever
function IcsInstService(const Sname, Dname, DriverPath: string; ServiceType, ServiceStart: DWORD ;  const Account,
            Password: string; Dependencies: PWideChar; const Descr: string ; RestartDelay: integer ;  var Resp: string): boolean ;
var
    LocalHandle: SC_HANDLE ;
    AccountPtr: Pointer;
    WideSName, WideDName, WideDescr: WideString ;  // Unicode
    WidePath, WideAccount, WidePassword: WideString ;  // Unicode
    ServiceDescription: TServiceDescriptionW ;
    ServiceFailureAction: TServiceFailureActionW ;
    ScAction: TScAction ;
begin
    result := false ;
    resp := '' ;
    LocalHandle := 0 ;
    WideSName := Sname ;
    WideDName := Dname ;
    WidePath := DriverPath ;
    WideAccount := Account ;
    WidePassword := Password ;
    WideDescr := Descr ;
    if NOT IcsSCMOpen(Ics_SCM_ADMIN, Resp) then
        exit ;
    try // finally
        try // except
            if Account = '' then
                AccountPtr := nil
            else
                AccountPtr := PWideChar (WideAccount) ;
            LocalHandle := OpenServiceW (ServiceCDatabaseHandle, PWideChar (WideSName), SERVICE_ALL_ACCESS) ;
            if LocalHandle <> 0 then begin
                if ChangeServiceConfigW(LocalHandle, ServiceType, ServiceStart, SERVICE_ERROR_NORMAL, PWideChar (WidePath),
                                 nil, nil, Dependencies, AccountPtr, PWideChar (WidePassword), PWideChar (WideDName)) then begin
                    resp := 'Existing Service Updated' ;
                    result := true ;
                end
                else begin
                    resp := 'Failed to Update Existing Service: ' + IcsFormatLastError;
                    exit ;
                end;
            end
            else
            begin
                LocalHandle := CreateServiceW(ServiceCDatabaseHandle, PWideChar (WideSName), PWideChar (WideDName),
                     SERVICE_ALL_ACCESS, ServiceType, ServiceStart, SERVICE_ERROR_NORMAL, PWideChar (WidePath), nil, nil,
                                                                            Dependencies, AccountPtr, PWideChar (WidePassword)) ;
                if LocalHandle <> 0 then begin
                    resp := 'Service Installed' ;
                    result := true ;
                end
                else begin
                    resp := 'Failed to Install Service: ' + IcsFormatLastError;
                    exit ;
                end;
            end ;

         // 1 March 2010, update description and failure actions
            ServiceDescription.Lpdescription := PWideChar (WideDescr) ;
            if Length (WideDescr) = 0 then
                ServiceDescription.Lpdescription := Nil ;  // clear it
            if NOT ChangeServiceconfig2W (LocalHandle, SERVICE_CONFIG_DESCRIPTION, @ServiceDescription) then
               resp := 'Failed to Set Service Description: ' + IcsFormatLastError;

         // 1 March 2010 update failure actions
            ServiceFailureAction.lpRebootMsg := Nil ;
            ServiceFailureAction.lpCommand := Nil ;
            ServiceFailureAction.dwResetPeriod := INFINITE ;
            ServiceFailureAction.cActions := 1;
            if RestartDelay > 0 then begin
                ScAction.Atype := SC_ACTION_RESTART ;
            end
            else begin
                ScAction.Atype := SC_ACTION_NONE ;
            end;
            ScAction.Delay := LongWord (RestartDelay) * 1000 ;  // convert seconds to millisecs
            ServiceFailureAction.lpsaActions := @ScAction ;
            if NOT ChangeServiceConfig2W (LocalHandle, SERVICE_CONFIG_FAILURE_ACTIONS,  @ServiceFailureAction) then
                resp := 'Failed to Set Service Failure Action: ' + IcsFormatLastError;
        except
            on E:Exception do
                resp := 'Exception Install Service - ' + E.Message ;
        end ;
    finally
        if LocalHandle <> 0 then
                CloseServiceHandle (LocalHandle) ;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// remove a service
function IcsRemService(const Sname: string; var Resp: string): boolean ;
var
    LocalHandle: SC_HANDLE ;
    WideName: WideString ;  // Unicode
begin
    result := false ;
    resp := '' ;
    WideName := Sname ;
    if NOT IcsSCMOpen(Ics_SCM_ADMIN, Resp) then
        exit ;
    try // except
        LocalHandle := OpenServiceW (ServiceCDatabaseHandle, PWideChar (WideName), SERVICE_ALL_ACCESS) ;
        try // finally
            if LocalHandle <> 0 then begin
                if NOT DeleteService (LocalHandle) then
                    resp := 'Failed to Remove Service: ' + IcsFormatLastError
                else begin
                    resp := 'Service Removed' ;
                    result := true ;
                end ;
                CloseServiceHandle (LocalHandle) ;
            end
            else
                resp := 'Failed to Remove Service: ' + IcsFormatLastError;
        finally
            if LocalHandle <> 0 then
                CloseServiceHandle (LocalHandle) ;
        end ;
    except
        on E:Exception do
            resp := 'Exception Removing Service - ' + E.Message ;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsSetDescrServ (const Sname, Descr: string; var Resp: string): boolean ;
var
    LocalHandle: SC_HANDLE ;
    WideName, WideDescr: WideString ;  // Unicode
    ServiceDescription: TServiceDescriptionW ;
begin
    result := false ;
    WideName := Sname ;
    WideDescr := Descr ;
    if NOT IcsSCMOpen(Ics_SCM_ADMIN, Resp) then
        exit ;
    try // except
        LocalHandle := OpenServiceW (ServiceCDatabaseHandle, PWideChar (WideName), SERVICE_ALL_ACCESS) ;
        try // finally
            if LocalHandle <> 0 then begin
                ServiceDescription.Lpdescription := PWideChar (WideDescr) ;
                if Length (WideDescr) = 0 then
                    ServiceDescription.Lpdescription := Nil ;  // clear it
                if ChangeServiceconfig2W (LocalHandle, SERVICE_CONFIG_DESCRIPTION, @ServiceDescription) then begin
                    resp := 'Service Description Updated' ;
                    result := true ;
                end;
                CloseServiceHandle (LocalHandle) ;
            end
            else
                resp := 'Failed to Update Service: ' + IcsFormatLastError;
        finally
            if LocalHandle <> 0 then
                CloseServiceHandle (LocalHandle) ;
        end;
    except
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{Note that this function is not a member of WindowList.  Therefore, the list to be filled needs to be passed
 as a pointer. Note that this is passed as a VAR. if you  don't do this, bad things happen in memory.}
Function WindowCallback(WHandle : HWnd; Var Parm : Pointer) : Boolean; stdcall; {This function is called once for each window}
Var  Buffer: array[0..255] of WideChar ;
     MyLongWord : DWord ;
     MyWindowPtr : PTIcsWindowObject;
     MyLen : integer ;
begin
    New(MyWindowPtr);

    {Window Handle (Passed by the enumeration)}
    MyWindowPtr.WinHandle := WHandle;

    {Window title caption text}
    GetWindowTextW (WHandle,Buffer,255);    // unicode
    MyWindowPtr.WinCaption := Buffer;

    {Window class text}
    MyLen := GetClassNameW (WHandle,Buffer,255);    // unicode
    if MyLen <> 0 then
        MyWindowPtr.WinClass := Buffer
    else
        MyWindowPtr.WinClass := '' ;

    {Process ID - Angus function returned thread not process }
    MyLongWord := 0;
    MyWindowPtr.ThreadID := GetWindowThreadProcessId(WHandle,@MyLongWord);
    MyWindowPtr.ProcessID := MyLongWord ;

    {Visiblity}
    MyWindowPtr.IsVisible := IsWindowVisible(WHandle);

    {Enabled}
    MyWindowPtr.IsEnabled := IsWindowEnabled(WHandle);

    {Iconic}
    MyWindowPtr.IsIconic := IsIconic(WHandle);

    {Window Dimensions}
    MyWindowPtr.WindowRect := Rect(0,0,0,0);
    GetWindowRect(WHandle,MyWindowPtr.WindowRect);

    {Add the structure to the list. Do not dereference Parm... once again, bad things happen.}
    TList(Parm).Add(MyWindowPtr);
    Result := True; {Everything's okay. Continue to enumerate windows}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsWindowList.Create(AOwner: TComponent);
begin
    inherited;
    WindowLst := TList.Create;
    try

{Thanks Serge, I should've done this from the start :)
 Sloppy me. }
        if not ( csDesigning in ComponentState ) then begin
            EnumWindows(@WindowCallback, THandle(@WindowLst)); { Aug 2024 was Int, but failed with Win64 }
            FCount := WindowLst.Count;
        end
    else
        FCount := 0;
    except
        FCount := 0;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsWindowList.Destroy;
    var I : Integer;
begin
    try
        if WindowLst.Count > 0 then begin
            for I := 0 to (WindowLst.Count - 1) do
                Dispose(PTIcsWindowObject(WindowLst[I]));
        end;
        WindowLst.Free;
    except
    end ;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWindowList.Refresh;
begin
    try
        WindowLst.Clear; {Clear the list!}
        EnumWindows (@WindowCallback,Longint(@WindowLst));
        FCount := WindowLst.Count;
    except
        FCount := 0;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsWindowList.GetAWindow(Index : Integer) : TIcsWindowObject;
begin
    Result := PTIcsWindowObject(WindowLst[Index])^;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsProcessList.Create (AOwner: TComponent) ;
begin
    inherited;
    ProcessLst := TList.Create;
    FOnlyExe := true ;
    if not ( csDesigning in ComponentState ) then
        Refresh
    Else
        FCount := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsProcessList.Destroy;
    var I : Integer;
begin
    If ProcessLst.Count > 0 Then
    Begin
        For I := 0 To (ProcessLst.Count - 1) Do
            Dispose(PTIcsProcessObject(ProcessLst[I]));
    End;
    ProcessLst.Free;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsProcessList.GetWinNT ;
var
    Processes: array [0..4095] of DWORD;
    cbNeededP: Cardinal;
    i: integer;
    ProcObj: PTIcsProcessObject;
    FileName: string ;
begin
    cbNeededP := 0;
    if EnumProcesses (@Processes, SizeOf(Processes), cbNeededP) then begin
        if cbNeededP = 0 then
            Exit;
        for i := 0 to ((cbNeededP div SizeOf (DWORD)) - 1) do begin
            FileName := IcsGetExeName(Processes [i]);
            if FileName <> '' then begin
                New(ProcObj);
                with ProcObj^ do begin
                    ProcessID := Processes [i] ;
                    ExeFile := FileName ;
                    DefaultHeapID := 0 ;
                    ModuleID := 0 ;
                    CountThreads := 0 ;
                    ParentProcessID := 0 ;
                    PriClassBase := 0 ;
                    Flags := 0 ;
                end ;
                ProcessLst.Add (ProcObj) ;
            end;

        end ;
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsProcessList.Refresh;
begin
    ProcessLst.Clear; {Clear the list!}
    FCount := 0 ;
    GetWinNT ;
    if ProcessLst.Count > 0 then
        FCount := ProcessLst.Count;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsProcessList.GetAProcess (Index: Integer): TIcsProcessObject;
begin
  Result := PTIcsProcessObject (ProcessLst[Index])^;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsWinExecAndWait(const aCmdLine: String; Visibility: Word): integer;
var
    Msg: TMsg;
    lpExitCode : longword ;
    StartupInfo: TStartupInfoW;
    ProcessInfo: TProcessInformation;
    lpCommandLine: WideString;  // Unicode - CreateProcessW may modify this string, don't use constant
begin
    lpCommandLine := aCmdLine ;
    FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
    with StartupInfo do begin
        cb := SizeOf(TStartupInfo);
        dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
        wShowWindow := visibility;  {you could pass sw_show or sw_hide as parameter}
    end;
    if CreateProcessW (nil,PWideChar (lpCommandLine),nil, nil, False,  NORMAL_PRIORITY_CLASS, nil, nil,
                                                                                        StartupInfo, ProcessInfo) then begin
        repeat
            while PeekMessage(Msg, 0, 0, 0, pm_Remove) do begin
                if Msg.Message = wm_Quit then
                    Halt(Msg.WParam);
                TranslateMessage(Msg);
                DispatchMessage(Msg);
            end;
            GetExitCodeProcess(ProcessInfo.hProcess,lpExitCode);
        until lpExitCode<>Still_Active;

        with ProcessInfo do begin
            CloseHandle(hThread);
            CloseHandle(hProcess);
        end;
        result := 0; {sucess}
    end
    else
        result := GetLastError;  {error occurs during CreateProcess see help for details}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetExeName(PID: DWORD): String;  // Aug 2024
var
    hProcess: THandle;
    len: integer;
    szProcessName: array [0..MAX_PATH-1] of WideChar;
begin
    Result := '';
    hProcess := OpenProcess (PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, FALSE, PID);
    if hProcess <> 0 then begin
        len := GetModuleFileNameExW(hProcess, 0, szProcessName, Length(szProcessName));
        if len > 0 then
            Result := String(szProcessName) ;
    end;
    CloseHandle (hProcess);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// run a program and return immediately with all process information
function IcsStartExe (const aCmdLine, aWorkDir: String; ShowState: Word): TProcessInformation ;
var
    StartupInfo : TStartupInfoW;
    ProcessInfo : TProcessInformation;
    lpCommandLine: WideString;  // Unicode - CreateProcessW may modify this string, don't use constant
    lpCurrentDirectory: WideString ; // Unicode
    lpDirPtr: PWideChar ;           // Unicode, nil needed for blank directory
begin
  {setup the startup information for the application }
    lpCommandLine := aCmdLine ;
    lpCurrentDirectory := aWorkDir ;
    lpDirPtr := Nil ;
    if Length (lpCurrentDirectory) > 2 then
        lpDirPtr := @lpCurrentDirectory [1] ;
    FillChar (StartupInfo, SizeOf (TStartupInfo), 0);
    FillChar (Result, SizeOf (Result), 0);
    with StartupInfo do begin
        cb := SizeOf (TStartupInfo);
        dwFlags := STARTF_FORCEONFEEDBACK ;
        if ShowState <> 0 then begin
            dwFlags := dwFlags OR STARTF_USESHOWWINDOW ;
            wShowWindow := ShowState ;  //  SW_SHOWNORMAL, SW_SHOWMINIMIZED or SW_HIDE
        end ;
    end;
    if CreateProcessW(nil, PWideChar (lpCommandLine), nil, nil, False, NORMAL_PRIORITY_CLASS, nil, lpDirPtr,
                                                                                                StartupInfo, ProcessInfo) then
        Result := ProcessInfo ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// run a program and optionally return immediately
function IcsfileExec(const aCmdLine, aWorkDir: String; ShowState: Word; aWait: Boolean): Boolean ;
var
    ProcessInfo : TProcessInformation;
begin
    result := false ;
    ProcessInfo := IcsStartExe (aCmdLine, aWorkDir, ShowState) ;
    if ProcessInfo.hProcess = 0 then exit ;
    result := true ;
    if NOT aWait then
        exit ;
    WaitForInputIdle (ProcessInfo.hProcess, INFINITE);     // handle not PID
    WaitForSingleObject (ProcessInfo.hProcess, INFINITE);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// run a program, URL or document, returning process handle
function IcsfileShellOpenEx (aFile: String; var PID: longword): boolean ;
var
    shellinfo: TShellExecuteInfoW ;       // unicode
    WideFileName: WideString ;            // unicode
begin
    WideFileName := aFile ;
    FillChar (shellinfo, SizeOf (shellinfo), 0);
    PID := 0 ;
    with shellinfo do begin
        cbSize := SizeOf (TShellExecuteInfo) ;
        fmask := SEE_MASK_NOCLOSEPROCESS OR SEE_MASK_FLAG_DDEWAIT OR  SEE_MASK_FLAG_NO_UI ;
        Wnd := hInstance ;
        lpVerb := 'open' ;
        lpFile := PWideChar(WideFileName) ;
        nShow :=  SW_NORMAL ;
    end ;
    result := NOT ShellExecuteExW (@shellinfo) ;
    if NOT result then
        PID := shellinfo.hProcess ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// run a program, URL or document, returning exe file name
function IcsFileGetOpen (aFile: String; var ExeName: string): boolean ;
var
    NewFile: PWideChar ;
    retcode: integer ;
    WideFileName: WideString ;   // unicode
begin
    ExeName := '' ;
    WideFileName := aFile ;
    NewFile := nil ;
    result := true ;
    retcode := FindExecutableW (PWideChar(WideFileName), Nil, @NewFile) ;    // unicode
    if (retcode > 32) and (NewFile <> Nil) then begin
        result := false ;
        ExeName := NewFile ;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// gets the processid for this application   - Aug 2024
function IcsCurProcID: DWORD;
begin
    Result := GetCurrentProcessId;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// gets the processid for a running application by file name, zero means not running
function IcsGetExePID (const AppName: string): DWORD ;
var
    ProcList: TIcsProcessList ;
    item: integer ;
    ProcFile: string ;
begin
    result := 0 ;
    ProcList := TIcsProcessList.Create (Nil) ;
    try
        if ProcList.Count = 0 then exit ;
        for item := 0 to ProcList.Count - 1 do begin
            ProcFile := ProcList.Process [item].ExeFile ;
            if (pos ('\', AppName) = 0) then
                ProcFile := ExtractFileName (ProcFile) ;
            if CompareText (AppName, ProcFile) = 0 then begin
                result := ProcList.Process [item].ProcessID ;
                exit ;
            end ;
        end ;
    finally
        if Assigned (ProcList) then
            ProcList.Destroy ;
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// check if processid is for a running application, by checking table
// much fastrer to use IcsCheckPID !!!!!
function IcsCheckExePID (PID: DWORD): boolean ;
var
    ProcList: TIcsProcessList ;
    item: integer ;
begin
    result := false ;
    ProcList := TIcsProcessList.Create (Nil) ;
    try
        if ProcList.Count = 0 then
            exit ;
        for item := 0 to Pred (ProcList.Count) do begin
            if PID = ProcList.Process [item].ProcessID then begin
                result := true ;
                exit ;
            end ;
        end ;
    finally
        if Assigned (ProcList) then
            ProcList.Destroy ;
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// get the windows handle for a process identifier
function IcsGetPIDWin (PID: DWORD): DWORD ;
var
    WinList: TIcsWindowList ;
    item: integer ;
begin
    result := 0 ;
    WinList := TIcsWindowList.Create (Nil) ;
    try
        if WinList.Count = 0 then
            exit ;
        for item := 0 to WinList.Count - 1 do begin
            if (WinList.Windows [item].ProcessId = PID) then begin
                if (WinList.Windows [item].IsIconic) or (WinList.Windows [item].IsVisible) then begin
                    Result := WinList.Windows [item].WinHandle ;
                    exit ;
                end ;
            end ;
        end ;
    finally
        if Assigned (WinList) then
            WinList.Destroy ;
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// get the windows handle for a running application file name, zero means not running
// Win32 app can not see Win64 processes
function IcsGetExeWin (const AppName: string): DWORD ;
var
    procid: DWORD ;
begin
    result := 0 ;
    procid := IcsGetExePID (AppName) ;
    if procid = 0 then
        exit ;
    result := IcsGetPIDWin (procid) ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// terminate a process using handle from CreateProcess or OpenProcess, not Process ID
// this is brutal, crashes program
function IcsTermPHandle (PHandle: THandle; exitcode: integer): boolean ;
begin
    result := TerminateProcess (PHandle, exitcode) ;
    CloseHandle (PHandle) ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// check if program running using handle from CreateProcess or OpenProcess, not Process ID
function IcsCheckPHandle (PHandle: THandle): boolean ;
var
    lpExitCode: longword ;
begin
    GetExitCodeProcess(PHandle, lpExitCode) ;
    result := (lpExitCode = Still_Active) ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// check if program running using Process ID
function IcsCheckPID (PID: DWORD): boolean ;        // July 2024
var
    hProcess: THandle ;
    lpExitCode: longword ;
begin
    result := false ;
    hProcess := OpenProcess (PROCESS_QUERY_INFORMATION, false, PID) ;
    if hProcess = 0 then
        exit ;
    GetExitCodeProcess(hProcess, lpExitCode) ;
    result := (lpExitCode = Still_Active) ;
    CloseHandle (hProcess) ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// close a process using the Process ID, single window only
// the proper way to close a program,
function IcsCloseExe (PID: DWORD; WinMess: Integer = WM_CLOSE): boolean ;    // or WM_QUIT
var
    winhandle: THandle ;
begin
    result := false ;
    winhandle := IcsGetPIDWin (PID) ;
    if winhandle = 0 then
        exit ;
    PostMessage (winhandle, WinMess, 0, 0) ;
    result := true ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Aug 2024, close a process using the Process ID, all windows
// the proper way to close a program,
function IcsCloseExeEx (PID: DWORD; WinMess: Integer = WM_CLOSE): boolean ;    // Aug 2024 all windows
var
    WinList: TIcsWindowList ;
    item: integer ;
begin
    result := false ;
    WinList := TIcsWindowList.Create (Nil) ;
    try
        if WinList.Count <> 0 then begin
            for item := 0 to WinList.Count - 1 do begin
                if (WinList.Windows [item].ProcessId = PID) then begin
                    if (WinList.Windows [item].IsIconic) or (WinList.Windows [item].IsVisible) then begin
                        PostMessage (WinList.Windows [item].WinHandle, WinMess, 0, 0) ;
                        result := true ;
                    end ;
                end ;
            end ;
        end;
    finally
        WinList.Destroy ;
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Terminate a process using the Process ID
// this is brutal, crashes program
function IcsTermPID (PID: DWORD; exitcode: integer): boolean ;
var
    hProcess: THandle ;
begin
    result := false ;
    hProcess := OpenProcess (PROCESS_TERMINATE, false, PID) ;
    if hProcess = 0 then
        exit ;
    result := TerminateProcess (hProcess, exitcode) ;
    CloseHandle (hProcess) ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// memory being used by application
function IcsGetWorkingSetSize : DWORD;
var
    MemCounters: PROCESS_MEMORY_COUNTERS;  // Defined in PsAPI
    ProcessHandle : THandle;
begin
    ProcessHandle := OpenProcess(PROCESS_ALL_ACCESS, FALSE, GetCurrentProcessID);
    FillChar(MemCounters, SizeOf(MemCounters), 0);
    MemCounters.cb := SizeOf(MemCounters);
    GetProcessMemoryInfo(ProcessHandle, @MemCounters, SizeOf(MemCounters));
    CloseHandle(ProcessHandle);
    Result := MemCounters.WorkingSetSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// run a console or command line application and return it's screen output in a StringList
procedure IcsGetConsoleOutputWait (const CommandLine: string; const WaitSecs: integer; var Output : TStringList);
var
    SA: TSecurityAttributes;
    SI: TStartupInfoW;
    PI: TProcessInformation;
    StdOutFile, AppProcess, AppThread : THandle;
    RootDir, StdOutFileName: WideString;  // Unicode
    ret, exitcode, waitticks: integer ;
    lpCommandLine: WideString;  // Unicode - CreateProcessW may modify this string, don't use constant
    pCurrentDirectory: WideString ; // Unicode
const
  FUNC_NAME = 'GetConsoleOuput';
begin
    StdOutFile := 0;
    AppProcess := 0;
    AppThread := 0;
    lpCommandLine := CommandLine ; // unicode
    try

    // Initialize dirs
    RootDir := ExtractFilePath(ParamStr(0));
    pCurrentDirectory := ExtractFilePath(CommandLine);

    // Check WorkDir
    if not (FileSearch(ExtractFileName(CommandLine), pCurrentDirectory) <> '') then
      pCurrentDirectory := RootDir;

    // Initialize output file security attributes
    FillChar(SA, SizeOf(SA), #0);
    SA.nLength := SizeOf(SA);
    SA.lpSecurityDescriptor := nil;
    SA.bInheritHandle := True;

    // Create Output File
    StdOutFileName := pCurrentDirectory + 'output.tmp';
    StdOutFile := CreateFileW(PWideChar(StdOutFileName), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE,
                                                      @SA, CREATE_ALWAYS, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_WRITE_THROUGH, 0);

    // Check Output Handle
    if StdOutFile = INVALID_HANDLE_VALUE then
      raise Exception.CreateFmt('Function %s() failed!' + IcsCRLF + 'Command line = %s',[FUNC_NAME,CommandLine]);

    // Initialize Startup Info
    FillChar(SI, SizeOf(SI), #0);
    with SI do begin
      cb := SizeOf(SI);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE);
      hStdError := StdOutFile;
      hStdOutput := StdOutFile;
    end;

    // Create the process
    if CreateProcessW (nil, PWideChar(lpCommandLine), nil, nil, True, 0, nil, PWideChar(pCurrentDirectory), SI, PI) then begin
      waitticks := WaitSecs * 1000 ;
      if waitticks < 2000 then
        waitticks := 2000 ;
      ret := WaitForSingleObject(PI.hProcess, waitticks);
      AppProcess := PI.hProcess;
      AppThread := PI.hThread;
      exitcode := 0 ;
      if ret = WAIT_TIMEOUT then begin
          TerminateProcess (AppProcess, exitcode) ;
          Sleep (2000);  // wait for program to die
      end;
    end
    else
      raise Exception.CreateFmt('CreateProcess() in function %s() failed!'  + IcsCRLF + 'Command line = %s',[FUNC_NAME,CommandLine]);

    CloseHandle(StdOutFile);
    StdOutFile := 0;

    Output.Clear;
    Output.LoadFromFile (StdOutFileName);
    if ret = WAIT_TIMEOUT then
        Output.Add ('Abandoned Waiting for Program to Finish') ;

  finally
    // Close handles
    if StdOutFile <> 0 then
        CloseHandle(StdOutFile);
    if AppProcess <> 0 then
        CloseHandle(AppProcess);
    if AppThread <> 0 then
        CloseHandle(AppThread);

    // Delete Output file
    if FileExists(StdOutFileName) then
        IcsDeleteFile(StdOutFileName, False);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsIsWin64: Boolean;
begin
{$IFDEF CPUX64}
    Result := True;
{$ELSE}
    Result := False;
{$ENDIF}
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
type
    TIsWow64Process = function (hProcess: THandle; var Wow64Process: BOOL): BOOL; stdcall;

{ Are we running under a Win64 windows OS, used from Win32 apps that need to know }
function IcsIsWow64: Boolean ;
var
    IsWow64ProcessX: TIsWow64Process;
    flag: LongBool;
begin
    Result := false;
    IsWow64ProcessX := GetProcAddress (GetModuleHandle ('kernel32'), 'IsWow64Process') ;   // not available in early Delphi versions
    if Assigned(IsWow64ProcessX) then begin
        flag := false ;  // warning, returns false for 64-bit application under 64-bit windows
        try
            // probably not Windows 32-bit editions!
            if IsWow64ProcessX(GetCurrentProcess(), flag) then
                Result := flag;
        except
        end;
    end;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
  KEY_WOW64_64KEY        = $0100;     // missing from D7

{ Read a string value from the HLM registry, compatibible with Win32 and Win64 apps  }
function IcsRegGetHlm(const RegKey, RegName: String): String;
begin
    Result := '';
    with TRegistry.Create do
    try
        RootKey := HKEY_LOCAL_MACHINE;
        Access := KEY_QUERY_VALUE ;
        if IcsIsWin64 or IcsIsWow64 then
            Access := Access OR KEY_WOW64_64KEY;  // don't use WOW6432Node keys
        if NOT OpenKey(RegKey, False) then
            Exit;
        if ValueExists (RegName) then begin
            if GetDataType (RegName) = rdString then
                Result := ReadString (RegName) ;
        end;
        CloseKey;
    finally
        Free;
    end
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Write a string value to the HLM registry, needs admin rights }
function IcsRegPutHlm(const RegKey, RegName, RegValue: String): Boolean;
begin
    Result := False;
    with TRegistry.Create do
    try
        RootKey := HKEY_LOCAL_MACHINE;
        Access := KEY_SET_VALUE ;
        if IcsIsWin64 or IcsIsWow64 then
            Access := Access OR KEY_WOW64_64KEY;  // don't use WOW6432Node keys
        if NOT OpenKey(RegKey, True) then
            Exit;
        WriteString (RegName, RegValue);
        Result := True;
        CloseKey;
    finally
        Free;
    end
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Windows Fieewall support, list all advanced firewall rules, that partially match Search,
    in name, group or file, blank search just returns fileware settings }
function IcsFireWallRulesEnum(const Search: String): String;
var
    SearchLC, Profiles: String;
    objShell: OleVariant;
    CurrentProfiles: Integer;
    fwPolicy2: OleVariant;
    RulesObject, Rule: OleVariant;
    InterfacesArray, InterfaceName: OleVariant;
    RuleEnum, InterfaceEnum: IEnumvariant;
    Value, Value2: LongWord;
    ProfActive: Boolean;

    procedure AddRow(const S: String);
    begin
        Result := Result + S + IcsCRLF;
    end;

    procedure ListSettings(Profile: Integer);
    begin
        if fwPolicy2.FirewallEnabled[Profile] then
            AddRow('Firewall is Enabled')
        else
            AddRow('Firewall is Disabled');

        if fwPolicy2.BlockAllInboundTraffic[Profile] then
            AddRow('Block All Inbound Traffic is Enabled')
        else
            AddRow('Block All Inbound Traffic is Disabled');

        if fwPolicy2.NotificationsDisabled[Profile] then
            AddRow('Notifications Disabled is Enabled')
        else
            AddRow('Notifications Disabled is Disabled');

        if fwPolicy2.DefaultInboundAction[Profile] then
            AddRow('Default Inbound Action is Allow')
        else
            AddRow('Default Inbound Action is Block');

        if fwPolicy2.DefaultOutboundAction[Profile] then
            AddRow('Default Outbound Action is Allow')
        else
            AddRow('Default Outbound Action is Block');
    end;

begin
    Result := '';
    try
      // check firewall service is running, else APIs fail
        objShell := CreateOLEObject ('Shell.Application');
        If NOT objShell.IsServiceRunning ('mpssvc') then begin
            VarClear (objShell);
            Result := 'Windows Firewall Service Not Running';
            Exit;
        end;
        VarClear (objShell);

      // Create the FwPolicy2 object.
        fwPolicy2 := CreateOleObject('HNetCfg.FwPolicy2');
        CurrentProfiles := fwPolicy2.CurrentProfileTypes;

        ProfActive := ((CurrentProfiles AND NET_FW_PROFILE2_DOMAIN)<>0);
        if ProfActive then
            AddRow('Domain Firewall Profile is active');
        if ProfActive or (Search <> '') then begin
            AddRow('Settings for Domain Profile:');
            ListSettings(NET_FW_PROFILE2_DOMAIN);
        end;

        ProfActive := ((CurrentProfiles AND NET_FW_PROFILE2_PRIVATE )<>0);
        if ProfActive then
            AddRow('Private Firewall Profile is active');
        if ProfActive or (Search <> '') then begin
            AddRow('Settings for Private Profile:');
            ListSettings(NET_FW_PROFILE2_PRIVATE);
        end;

        ProfActive := ((CurrentProfiles AND NET_FW_PROFILE2_PUBLIC )<>0);
        if ProfActive then
            AddRow('Public Firewall Profile is active');
        if ProfActive or (Search <> '') then begin
            AddRow('Settings for Public Profile:');
            ListSettings(NET_FW_PROFILE2_PUBLIC);
        end;

        if (Search <> '') then begin
            AddRow('Rules:');
            RulesObject := fwPolicy2.Rules;
            RuleEnum := IUnknown(Rulesobject._NewEnum) as IEnumVariant;
            SearchLC := Lowercase(Search);
            while (RuleEnum.Next(1, Rule, Value) = 0) do  begin
                if (Pos(SearchLC, Lowercase(Rule.Grouping)) > 0) or (Pos(SearchLC, Lowercase(Rule.Name)) > 0)  or
                                                           (Pos(SearchLc, Lowercase(Rule.ApplicationName)) > 0)  then begin
                    AddRow('Rule Group: ' + Rule.Grouping);
                    AddRow('Rule Name: ' + Rule.Name);
                    AddRow('Description: ' + Rule.Description);
                    AddRow('Application Name: ' + Rule.ApplicationName);
                    AddRow('Service Name: ' + Rule.ServiceName);

                    Case Rule.Protocol of
                       NET_FW_IP_PROTOCOL_TCP    : AddRow('IP Protocol: TCP');
                       NET_FW_IP_PROTOCOL_UDP    : AddRow('IP Protocol: UDP');
                       NET_FW_IP_PROTOCOL_ICMPv4 : AddRow('IP Protocol: UDP');
                       NET_FW_IP_PROTOCOL_ICMPv6 : AddRow('IP Protocol: UDP');
                       NET_FW_IP_PROTOCOL_ANY    : AddRow('IP Protocol: Any');
                    Else                           AddRow('IP Protocol: ' + VarToStr(Rule.Protocol));
                    End;

                    if (Rule.Protocol = NET_FW_IP_PROTOCOL_TCP) or (Rule.Protocol = NET_FW_IP_PROTOCOL_UDP) or
                                                                          (Rule.Protocol = NET_FW_IP_PROTOCOL_ANY) then begin
                      AddRow('Local Ports: ' + Rule.LocalPorts);
                      AddRow('Remote Ports: ' + Rule.RemotePorts);
                      AddRow('LocalAddresses: ' + Rule.LocalAddresses);
                      AddRow('RemoteAddresses: ' + Rule.RemoteAddresses);
                    end;

                    if (Rule.Protocol = NET_FW_IP_PROTOCOL_ICMPv4) or (Rule.Protocol = NET_FW_IP_PROTOCOL_ICMPv6) then
                      AddRow('ICMP Type and Code: ' + Rule.IcmpTypesAndCodes);

                    Case Rule.Direction of
                        NET_FW_RULE_DIR_IN :  AddRow('Direction: In');
                        NET_FW_RULE_DIR_OUT:  AddRow('Direction: Out');
                    End;

                    AddRow('Rule Enabled: ' + VarToStr(Rule.Enabled));
                    AddRow('Edge Traversal: ' + VarToStr(Rule.EdgeTraversal));

                    Profiles := '';
                    if (Rule.Profiles AND NET_FW_PROFILE2_DOMAIN)<>0 then
                        Profiles := ' Domain';
                    if (Rule.Profiles AND NET_FW_PROFILE2_PRIVATE )<>0 then
                        Profiles := Profiles + ' Private';
                    if (Rule.Profiles AND NET_FW_PROFILE2_PUBLIC )<>0 then
                        Profiles := Profiles + ' Public';
                    AddRow('Profile:' + Profiles);

                    Case Rule.Action of
                       NET_FW_ACTION_ALLOW : AddRow('Rule Action: Allow');
                       NET_FW_ACTION_BLOCk : AddRow('Rule Action: Block');
                    End;

                    AddRow('Interface Types: ' + Rule.InterfaceTypes);
                    if Rule.InterfaceTypes <> 'All' then begin
                        InterfacesArray := rule.Interfaces;
                        Value2 := 0;
                        InterfaceEnum := IUnknown(InterfacesArray._NewEnum) as IEnumVariant;
                        while (InterfaceEnum.Next(1, InterfaceName, Value2) = 0) do begin
                            AddRow('Interface Name: ' + VarToStr(InterfaceName));
                        end;
                    end;

                    AddRow('----------------------------------------------');
                    AddRow('');
                end;
                Rule := Unassigned;
            end;
        end;
    except
        on E:EOleException do
            Result := Result + 'Firewall Error: ' + E.Message;
        on E:Exception do
            Result := Result + E.Classname + ': ' + E.Message;
    end;

  // cleanup
    VarClear (Rule) ;
    VarClear (RulesObject) ;
    VarClear (InterfacesArray) ;
    VarClear (InterfaceName) ;
    VarClear (fwPolicy2) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Windows Fieewall support, add or remove advanced firewall blank apppath is remove rule, direction is FirewallIn or
   FirewallOut with in/out added to name, FirewallBoth for two rules, FirewallNone is in with simple name. }
function IcsFireWallRulesAdd(const EntryName, GroupName, Descr, AppPathAndExe: string; Direction: TFirewallDir = FirewallNone): String;
var
    objShell, fwPolicy2, RulesObject, Rule, NewRule: OleVariant;
//    InterfacesArray: OleVariant;

    function AddRule(const NewName, NewDescr: String; Dir: Integer): String;
    begin
    // see if modifying existing rule, need to delete old rule first
        Result := '';
        Rule := Unassigned;  // not found
        try
            if NOT VarIsEmpty(Rulesobject) then        // Dec 2020
                Rule := Rulesobject.Item(NewName);
        except
            Rule := Unassigned;  // not found
        end;
        if NOT VarIsClear(Rule) then begin
            if (Rule.Applicationname = AppPathAndExe) then begin
                Result := 'Firewall Rule Already Exists: ' + NewName + ', Group: ' + Rule.Grouping;
                Exit;
            end;
            RulesObject.Remove(NewName);  // can not edit it
            if AppPathAndExe = '' then begin   // and no new one
                Result := 'Firewall Rule Removed: ' + NewName;
                Exit;
            end;
        end
        else if AppPathAndExe = '' then begin   // and no new one
            Result := 'Firewall Rule Not Found: ' + NewName;
            Exit;
        end;

     // Create new rule
        NewRule := CreateOleObject('HNetCfg.FWRule');
        NewRule.Name := NewName;
        NewRule.Description := NewDescr;
        NewRule.Applicationname := AppPathAndExe;
        NewRule.Protocol := NET_FW_IP_PROTOCOL_ANY;

    //  other fule options we could use for incoming rules
    //  NewRule.LocalPorts := 5000, 5001;
    //  NewRule.LocalAddresses := xxx

    //  other fule options we could use for outgoing rules
    //  NewRule.RemotePorts := 5000;
    //  NewRule.RemoteAddresses := LocalSubnet;

    //  NewRule.Interfacetypes := 'LAN';
    // InterfaceArray = Array("Local Area Connection")
    // NewRule.Interfaces := InterfaceArray

        NewRule.Direction := Dir;
        NewRule.Enabled := True;
        NewRule.Grouping := GroupName;
        NewRule.Profiles := NET_FW_PROFILE2_ALL; // or CurrentProfiles;
        NewRule.Action := NET_FW_ACTION_ALLOW;

    // add the new rule
        RulesObject.Add(NewRule);
        Result := 'Firewall Rule Added: ' + NewName + ' for: ' + AppPathAndExe;
    end;


begin
    Result := '';
    try
     // pending, use Ics

      // check firewall service is running, else APIs fail
        objShell := CreateOLEObject ('Shell.Application');
        If NOT objShell.IsServiceRunning ('mpssvc') then begin
            VarClear (objShell);
            Result := 'Windows Firewall Service Not Running';
            Exit;
        end;
        VarClear (objShell);

      // Create the FwPolicy2 object and get the list of rules
        fwPolicy2 := CreateOleObject('HNetCfg.FwPolicy2');
        RulesObject := fwPolicy2.Rules;
        case Direction of
            FirewallNone: Result := AddRule(EntryName, Descr, NET_FW_RULE_DIR_IN);
            FirewallIn:   Result := AddRule(EntryName + ' (All-In)', 'Inbound ' + Descr, NET_FW_RULE_DIR_IN);
            FirewallOut:  Result := AddRule(EntryName + ' (All-Out)', 'Outbound ' + Descr, NET_FW_RULE_DIR_OUT);
            FirewallBoth:
                begin
                          Result := AddRule(EntryName + ' (All-In)', 'Inbound ' + Descr, NET_FW_RULE_DIR_IN);
                          Result := Result + #13#10 +
                                    AddRule(EntryName + ' (All-Out)', 'Outbound ' + Descr, NET_FW_RULE_DIR_OUT);
                end;
        end;
    except
        on E:EOleException do
            Result := Result + 'Firewall Error: ' + E.Message;
        on E:Exception do
            Result := Result + E.Classname + ': ' + E.Message;
    end;

  // cleanup
    VarClear (NewRule) ;
    VarClear (Rule) ;
    VarClear (RulesObject) ;
 //   VarClear (InterfacesArray) ;
    VarClear (fwPolicy2) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*function IcsGetProcessHandles(ProcId: DWORD): Integer;  {V9.5 }
var
    HandleCount: DWORD ;
begin
{  ?? when did GetProcessHandleCount get added to Delphi???

// handle for DLL
var
    ProcHandleModule: THandle;

GetProcessHandleCount: function (hProcess: THandle; var HandleCount: DWORD): BOOL; stdcall;
   if NOT Assigned (GetProcessHandleCount) then begin
        GetProcessHandleCount := GetProcAddress (GetModuleHandle(kernel32), 'GetProcessHandleCount') ;
        if NOT Assigned (GetProcessHandleCount) then
            exit ;
    end;      }
//     exit;
    HandleCount := 0;
    GetProcessHandleCount(GetCurrentProcess, HandleCount);  // XP SP1 and later
    result := HandleCount;
end;   *)


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsMemInfoProg: String;      { V9.5 }
var
//  ProcMem: TProcessMemoryCounters;
    ProcMem: TProcessMemoryCountersEx2;
begin
    try
        if NOT GetProcessMemoryInfo (GetCurrentProcess, @ProcMem, SizeOf(TProcessMemoryCountersEx2)) then begin
            Result := 'GetProcessMemoryInfo not loaded';
            Exit;
        end;
        with ProcMem do begin
         //   Result := 'Current Program Resources: Handles ' + IcsIntToCStr (IcsGetProcessHandles(GetCurrentProcess)) ;  // range exception
            Result := Result + 'Memory: Working Set ' + IcsIntToKbyte (WorkingSetSize)  +
                 ', Paged Pool: Usage: ' + IcsIntToKbyte (QuotaPagedPoolUsage) +
                 ', Non Paged ' + IcsIntToKbyte (QuotaNonPagedPoolUsage) +
                 ', Peak NP ' + IcsIntToKbyte (QuotaPeakNonPagedPoolUsage) + IcsCRLF +
                 'Page File: Usage ' + IcsIntToKbyte (PagefileUsage) +
                 ', Peak ' + IcsIntToKbyte (PeakPagefileUsage) +
                 ', Private ' + IcsIntToKbyte (PrivateUsge) +
                 ', Working Set: Private ' + IcsIntToKbyte (PrivateWorkingSetSize) +
                 ', Shared ' + IcsIntToKbyte (SharedCommitUsage);
        end ;
    except
        Result := 'GetProcessMemoryInfo exception';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsMemInfoGlob: String;      { V9.5 }
var
    MemStatus: TMemoryStatusEx ;
begin
    try
        MemStatus.dwLength := SizeOf (MemStatus) ;
        MemStatus.ullTotalPhys := 0 ;
        GlobalMemoryStatusEx (MemStatus) ;
        with MemStatus do begin
             Result := 'Global Memory Usage: ' +
                 'Load ' + IcsIntToKbyte (dwMemoryLoad) +
                 '%, Total Physical ' + IcsIntToKbyte (ullTotalPhys) +
                 ', Usage ' +  IcsIntToKbyte (ullTotalPhys - ullAvailPhys) +
                 ', Free ' +  IcsIntToKbyte (ullAvailPhys ) + IcsCRLF +
                 'Paged File: Usage ' + IcsIntToKbyte (ullTotalPageFile - ullAvailPageFile) +
                 ', Max Limit ' + IcsIntToKbyte (ullTotalPageFile) +  // may not be system wide, use PerformanceInfo
                 ', Free ' + IcsIntToKbyte (ullAvailPageFile);
        end ;
    except
        Result := 'GlobalMemoryStatusEx exception';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsMemInfoPerf: String;      { V9.5 }
{$IFDEF WIN64}
var
    PerfInfo: TPerformanceInformation ;
{$ENDIF}
begin
    try
        Result := '';

 { V9.5 GetPerformanceInfo fails under Win32 on Windows 11 24H2, no data returned and memory corrupted  }
{$IFDEF WIN64}
        if NOT Assigned (GetPerformanceInfo) then begin
            if hPerfApi = 99 then
                Exit;
            if hPerfApi = 0 then begin
                hPerfApi := LoadLibrary('PSAPI.dll');
                if hPerfApi = 0 then begin
                    hPerfApi := 99;
                    Exit ;
                end;
            end;
            GetPerformanceInfo := GetProcAddress (hPerfApi, 'GetPerformanceInfo');
        //    GetPerformanceInfo := GetProcAddress (GetModuleHandle(kernel32), 'K32GetPerformanceInfo');
            if NOT Assigned (GetPerformanceInfo) then begin
                hPerfApi := 99;
                Exit ;
            end;
        end;
        PerfInfo.cb := Sizeof (TPerformanceInformation);
        if (GetPerformanceInfo (@PerfInfo, Sizeof (TPerformanceInformation)) <> 0) then begin
            with PerfInfo do begin
                Result := 'Global Memory and Counters: ' +
                    'Physical: Total ' + IcsIntToKbyte (PhysicalTotal * PageSize) +
                    ', Usage ' +  IcsIntToKbyte ((PhysicalTotal - PhysicalAvailable) * PageSize) +
                    ', Free ' + IcsIntToKbyte (PhysicalAvailable * PageSize) +
                    ', System Cache ' + IcsIntToKbyte (SystemCache * PageSize) + IcsCRLF +
                    'Paged File: Used ' + IcsIntToKbyte (CommitTotal * PageSize) +
                    ', Max Limit ' + IcsIntToKbyte (CommitLimit * PageSize) +
                    ', Peak ' + IcsIntToKbyte (CommitPeak * PageSize) +
                    ', Free ' + IcsIntToKbyte ((CommitLimit - CommitTotal) * PageSize) + IcsCRLF +
                    'Memory Kernel: Total ' + IcsIntToKbyte (KernelTotal * PageSize) +
                    ', Paged ' + IcsIntToKbyte (KernelPaged * PageSize) +
                    ', Nonpaged ' + IcsIntToKbyte (KernelNonpaged * PageSize) + IcsCRLF +
                    'Total Handles ' + IcsIntToCStr (HandleCount) +
                    ', Total Processes ' + IcsIntToCStr (ProcessCount) +
                    ', Total Threads ' + IcsIntToCStr (ThreadCount) ;
            end ;
        end
        else
            Result := 'GetPerformanceInfo failed: ' + SysErrorMessage(GetLastError);
{$ENDIF}
    except
        Result := 'GetPerformanceInfo exception';
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ check for low or critical memory problems  }
{ returns Warning at 85% physical memory or paged memory file usage, critical at 95% usage }
function IcsMemWarning(var Info: String; MemPer: Cardinal = 85; PagePer: Cardinal = 85; FatalPer: Cardinal = 95): TIcsMemWarn;  { V9.5 }
var
    MemStatus: TMemoryStatusEx ;
    PFilePer: Int64;
begin
    Result := MemWarnNone;
    Info := '';
    try
        MemStatus.dwLength := SizeOf (MemStatus) ;
        MemStatus.ullTotalPhys := 0 ;
        GlobalMemoryStatusEx (MemStatus) ;
        with MemStatus do begin
            PFilePer := 100 - ((ullAvailPageFile * 100) div ullTotalPageFile);
            Info := 'Physical Used ' + IntToStr(dwMemoryLoad) + '%' + ', Paging Used ' + IntToStr (PFilePer) + '%';
            if dwMemoryLoad >= MemPer then begin
                Result := MemWarnPoor;
                Info := '!! ' + Info;
                if dwMemoryLoad >= FatalPer then begin
                    Result := MemWarnCritical;
                    Info := '!' + Info;
                end;
                Exit;
            end;
            if PFilePer >= PagePer then begin
                Result := MemWarnPoor;
                Info := '!! ' + Info;
                if PFilePer >= FatalPer then begin
                    Result := MemWarnCritical;
                    Info := '!' + Info;
                end;
                Exit;
            end;
        end ;
    except
        Info := 'GlobalMemoryStatusEx exception';
    end;
end;

{$ENDIF WINDOWS}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$IFDEF MSWINDOWS}

Initialization

finalization
    if InstanceMutexHandle <> 0 then begin
        CloseHandle (InstanceMutexHandle) ;
        InstanceMutexHandle := 0 ;
    end ;
    IcsSCMClose;
 { V9.5 }
  if hPerfApi <> 0 then
  begin
    FreeLibrary(hPerfApi);
    hPerfApi  := 0;
  end;

{$ENDIF}

end.
