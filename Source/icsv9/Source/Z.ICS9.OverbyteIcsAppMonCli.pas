{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  The ICS Application Monitor TIcsAppMonCli client component is
              designed to report to an ICS Application Monitor server, which
              will ensure the main application remains running, restarting it
              if it stops or becomes non-responsive, or on demand.  Primarily
              to keep ICS server Windows services running non-stop, but may
              also be used for network wide monitoring of ICS applications.
Creation:     Sept 2024
Updated:      Sept 2025
Version:      V9.5
EMail:        francois.piette@overbyte.be  https://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2025 by Angus Robertson, Magenta Systems Ltd,
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


Application Independent Monitoring
----------------------------------
The ICS Application Monitor server is designed to monitor any ICS applications
using the TIcsAppMonCli client component, no configuration of the server is
needed other than setting it's listening IP address and email account details,
and the server has no prior knowledge of applications that may connect to it.

The server broadcasts it's availability, IP address and port to clients by
three methods:  Windows HLM registry entries, a named Windows message, and
optionally a UDP broadcast message.  The Windows registry and named messages
are only valid for applications on the same PC as the server, UDP messages
can be broadcast to the local LAN.

When clients access the server by the broadcasted IP address, they call the
component Start method that sends a HELLO request, containing the required
operating mode, currently 'Monitor Only', 'Non-Stop Monitor' or 'Installation',
and provide all the information the server needs, such as application name,
executable file name, Windows Service name, Windows Handle, Process ID, etc,
and general information the client can add for information. The client then
keeps the server socket connection open.  Note Non-Stop and Installation
modes are only accepted it the application is running on the same PC as the
server, and the server has the administrator rights to control the application.

Once accepted for monitoring, clients behaving correctly should call the
Awake method regularly, usually from a timer every few seconds, which
causes the component to send a small PING packet once a minute so the
server knows it's running OK. The Awake method also allows a status
message to be sent for the server display application information, such
as how many web pages it's sent.

To stop monitoring, the component Stop method is called that sends a STOP
packet, usually saying this is deliberate and no restart is required.  If
the client wants to be deliberately restarted, for instance after an
unexpected exception or terminal error, use the RestartNow method which
sends a special STOP packet that initiates the application restart process.
Not sending Awake regularly will also initiate a restart.

If the client connection is closed without a STOP packet or the PING packets
stop for a while, the server will attempt to restart the client, by first
stopping the Windows service or program, waiting for it to stop, and then
starting the service or program again.  If the Windows service does not
stop cleanly due to being non-responsive, the server will attempt to
terminate the program by process ID to ensure it stops and then restart.

Warning, it is crucial that applications ensure the Stop method is called
before deliberately closing down, otherwise the AppMon server will simply
restart them, continually.

When sending packets, the client may add an email message that the server
will send to the admin email address, perhaps application start-up or close
down information, error information, anything useful really.  The server
also sends admin emails when clients start and stop monitoring.

Not implemented yet, but installation mode is similar to restarting, except
the exe file is replaced by a new version while being restarted, allowing
the application to update itself with a new version.  Longer term, there
may be application updating component to include downlaoding new versions
and support to install multiple files.

The server can potentially monitor an unlimited number of applications,
whose status is available from a continuously refreshed web page using
Websockets, and also as Json for Websocket client applications.

There is a USERINFO command allowing the client and server to exchange
application defined information, not implemented yet.


Sep 25, 2024 V9.3 - Baseline
Feb 21, 2025 V9.4  Moved some functions to OverbyteWinUtils.
                   Correctly send AppStatus in timer PING.
                   TIcsAMApp has more display fields.
                   Corrected Awake only sending packets if logging enabled.
                   Only send Ping if Awake is being called.
                   Added diagnostic, STOP('CRASH-ME') will immediately stop
                     monitoring without a STOP packet to simulate a dead app.
Oct 03, 2025 V9.5  If server declines NONSTOP monitoring, retry with MONITOR
                     if MonAnyway property true.
                   Added OnWakeEvent so application can do something to try
                     and start sending hellos again.


See OverbyteIcsDDWebServiceSrv.pas and OverbyteIcsIpStmLogTst1.pas for examples
of how to use TIcsAppMonCli to monitor a Windows web server service
application and a simple Windows application.

The server sample is IcsAppMon.dproj which uses the TIcsAppMonSrv component
in OverbyteIcsAppMonSrv.pas, and may be run as a Windows GUI or installed as
a Windows Service.  The server sample also includes web and websocket servers
that provide status information on the applications being monitored.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit Z.ICS9.OverbyteIcsAppMonCli;

interface

{$I include\Z.ICS9.OverbyteIcsDefs.inc}

Uses
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$IFDEF Rtl_Namespaces}Winapi.Messages{$ELSE}Messages{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}Vcl.Graphics{$ELSE}Graphics{$ENDIF},
  Z.ICS9.OverbyteIcsTypes,
  Z.ICS9.OverbyteIcsUtils,
  Z.ICS9.OverbyteIcsTicks64,
  Z.ICS9.OverbyteIcsWndControl,
  Z.ICS9.OverbyteIcsIpStreamLog,
  Z.ICS9.OverbyteIcsWinUtils;      { V9.4 }

const
    IcsAppMonName = 'IcsAppMon' ;                   // packets and windows service
    IcsAppMonTitle = 'ICS Application Monitor';
    IcsAppMonRegKey = 'Software\ICS\IcsAppMon' ;    // HLM
    IcsAppMonRegAddr = 'IpAddr';
    IcsAppMonRegPort = 'IpPort';
    IcsAppMonRegDT  = 'LastDT';
    IcsAppMonVerNr = '001';
    IcsAppMonDate = '2025-09-10';
    IcsAppMonMessQuery = 'AppMonMessQuery' ;  // broadcast message clients querying server for IP and port
    IcsAppMonMessInfo = 'AppMonMessInfo' ;    // broadcast message from server with IP and port
    IcsAppMonMessParam = 1234567;           // payload for IcsAppMonMessQuery
    DefBcastPort = '17777';
    DefSrvPort = '17778';
    DefWebPort = '17779';
    DefBCastSec = 300;      // how often to broadcast our IP/port
    DefDeadSecs = 300;      // how long server waits before assuming application is dead, client controlled
    DefMinDeadSecs = 30;    // minimum wait before restarting server
    DefStopWaitSecs = 60;   // how long to wait for application to stop
    DefStartWaitSecs = 15;  // how long to wait before restarting application, after stop
    DefMaxCliRecLen = DefMaxIpLogLine;  // 4096
    ServiceNone = 'NONE';   // service name if not running as a service
    ProtDelim =  '|';
    WebServStatusPage = '/ServerStatus.htm';
    WebServStatusJson = '/ServerStatus.json';
    WebSocStatusWeb  = '/WebSocket/StatusWeb';
    WebSocStatusJson = '/WebSocket/StatusJson';

// commands from client to server
    ProtCmdHello = 'HELLO';       // client introduction to server, in response to UDP broadcast from server
    ProtCmdPing = 'PING';         // client regular alive to server
    ProtCmdStop = 'STOP';         // client is normally closing to server, with restart information
    ProtCmdUserInfo = 'USERINFO'; // client is sending user information for server to log or process
    ProtCmdInstall = 'INSTALL';   // client wants us to stop it, install new files and restart
    ProtCmdPause = 'PAUSE';       // client is pausing PINGs for X minutes while it does something, don't restart

// commands from server to client
    ProtCmdPong = 'PONG';         // server acknowledgement of client commands
    ProtCmdWake = 'WAKE';         // server wants a ping from client, overdue
    ProtCmdDecline = 'DECLINE';   // server declines to support client, too many clients, or service/desktop mismatch

// Broadcast packet fields, only used for LANWide monitoring
// ie: 'IcsAppMonn|HELLO|VER01|COMPNAME|192.168.1.121|17777|
   BcastFieldAppMon = 0 ;
   BcastFieldCommand = 1 ;
   BcastFieldVersion = 2 ;
   BcastFieldSrvName = 3 ;
   BcastFieldSrvIP = 4 ;
   BcastFieldSrvPort = 5 ;
   BcastFieldLast = 5 ;

// Client packet fields to server
// ie: IcsAppMonn|HELLO|Started Email|WebAppTelecom|C:\magenta\webapps\webapp_telecom.exe|WebappTelcom|1234567|COMPNAME|192.168.1.101|180|ProgInfo|
// ie: IcsAppMonn|PING|WebAppTelecom|Optional Email|optional Status|
// ie: IcsAppMonn|STOP||WebAppTelecom|Normal Close Down|NOT|
// ie: IcsAppMonn|INSTALL||WebAppTelecom|Install New Program|curexe.exe|newexe.exe|oldexe-001.exe|
// common record fields
   CliCFieldAppMon = 0 ;
   CliCFieldCommand = 1 ;
   CliCFieldAppTitle = 2 ;
   CliCFieldEmail = 3 ;       // blank no email, CRLF must be escaped \n
   CliCFieldLast = 3;         // previous common to all commands
// HELLO record fields
   CliHFieldAppMode = CliCFieldLast + 1;       // server mode requested
   CliHFieldExeFile = CliCFieldLast + 2;
   CliHFieldServiceName = CliCFieldLast + 3;   // NONE if not a Windows Service
   CliHFieldWinHnd = CliCFieldLast + 4;
   CliHFieldWinPID = CliCFieldLast + 5;
   CliHFieldCompName = CliCFieldLast + 6;
   CliHFieldLocalIP = CliCFieldLast + 7;
   CliHFieldDeadSecs = CliCFieldLast + 8;
   CliHFieldAppGeneral = CliCFieldLast + 9;  // general info, version, date, OpenSSL, etc,
   CliHFieldLast = CliCFieldLast + 9;
// PING record fields
   CliPFieldStatus = CliCFieldLast + 1;
// STOP record fields
   CliSFieldRestart = CliCFieldLast + 1;
// INSTALL record fields
   CliIFieldCurExe = CliCFieldLast + 1;
   CliIFieldNewExe = CliCFieldLast + 2;
   CliIFieldArchExe = CliCFieldLast + 3;

// Server packet fields to client
// ie: IcsAppMonn|PONG|Info|
// ie: IcsAppMonn|WAKE|Info|
// ie: IcsAppMonn|DECLINE|Not running as service||
   SrvFieldAppMon = 0;
   SrvFieldCommand = 1;
   SrvFieldInfo = 2;
   SrvFieldLast = 2;

// colours for webpages and ListView (reverse them)
   sColorBYell = 'FFFF00';    // bright yellow
   sColorPYell = 'FFFF99';    // pale yellow
   sColorPRed = 'FF7A33';     // pale red
   sColorBGreen = '00FF00';   // bright green


type
    TAMLogEvent = procedure (Sender: TObject; const Line: string) of object;

// application monitor command modes, defines which command can be sent
    TAMMode = (AMModeMonitor, AMModeNonStop, AMModeInstall);

// client communication state with monitoring sever
    TAMCliState = (CliStateNone,            // not connected
                   CliStateListen,          // waiting UDP broadcast or windows message with server IP and port
                   CliStateStart,           // opening connection to server
                   CliStateRegister,        // connected to server, registering application details
                   CliStateOK,              // connected to server, periodic ping/pongs
                   CliStateReconnnect,      // disconnected, but trying to reconnect to server
                   CliStateStopping,        // monitoring being deliberately stopped
                   CliStateStopped);        // monitoring has stopped, timer has stopped

// server restart mode instruction
    TAMRestartMode = (RestModeNone,       // no restarting
                      RestModeAuto,       // automatically restart app if stopped
                      RestModeRequest,    // app has requested a deliberate restart, may stop itself
                      RestModeItelf,      // app is trying to restart itelf, usually after an error
                      RestModeNot);       // app is closing deliberately, no restart required

 // monitoring state for each application
    TAMAppState = (AppStateNone,           // not monitoring yet
                   AppStateOK,             // monitoring OK, app is sending pings
                   AppStateQuiet,          // app has stopped talking us, waking it up and waiting 30 seconds
                   AppStateStopping,       // app is stopping deliberately, don't restart - RestModeNot or RestModeAuto
                   AppStateProgStop,       // app is being stopped ready for restart - RestModeRequest or RestModeItself
                   AppStateProgStart,      // app is being retarted (after stopping) - RestModeRequest or RestModeItself
                   AppStateStopped);       // monitoring has stopped, maybe waiting for application to restart

 // stopping state for each application
    TAMStopState = (StopStateNone,           // not stopping
                    StopStateStopProg,       // stop application, service will be stopped or application closed with message
                    StopStateStopWait,       // waiting for application to stop
                    StopStateCrashProg,      // application did not stop normally, crash with quit message or terminate process
                    StopStateCrashWait,      // waiting for application to stop again
                    StopStateStopped);       // stopped OK

 // starting state for each application
    TAMStartState = (StartStateNone,           // not starting
                     StartStateNotStopped,     // application has not stopped, try to stop again
                     StartStateStartWait,      // application was stopped, short wait before starting again
                     StartStateProgStarting,   // starting service or application
                     StartStateProgStarted);   // application is running again, monitoring will now stop since it has a new process ID

const
    AMModeCmds:  array [TAMMode] of PChar = ('MONITOR', 'NONSTOP', 'INSTALL');
    AMModeNames:  array [TAMMode] of PChar = ('Monitor Only', 'Non-Stop Monitor', 'Installation');
    AMCliStateNames:  array [TAMCliState] of PChar = ('None', 'Listening', 'Starting', 'Register', 'OK Connected',
                                                      'Reconnect', 'Stopping', 'Stopped');
    AMRestModeCmds: array [TAMRestartMode] of PChar = ('NONE', 'AUTO', 'REQUESTED', 'MYSELF', 'NOT');
    AMRestModeNames: array [TAMRestartMode] of PChar = ('None', 'Auto Restart', 'Requested Restart', 'Restarting Myself', 'No Restart');
    AMAppStateNames: array [TAMAppState] of PChar = ('None', 'OK', 'Quiet', 'Stopping Deliberately', 'Stopping Prog',
                                                                                                'Restarting Prog', 'Prog Stopped');
    AMAppStopStateNames: array [TAMStopState] of PChar = ('None', 'Prog Stop', 'Wait for Stop 1', 'Prog Crash',
                                                                                                  'Wait for Stop 2', 'Stopped OK');
    AMAppStartStateNames: array [TAMStartState] of PChar = ('None', 'Not Stopped', 'Wait for Start', 'Prog Starting', 'Prog Started');

// structure used by server to monitor each remote client application
type
    TIcsAMApp = record
        AppClientTitle: String;
        AppMonMode: TAMMode;
        AppClientID: Integer;
        AppExeFile: String;
        AppServiceName: String;
        AppWinHnd: THandle;
        AppWinPID: THandle;
        AppCompName: String;
        AppLocalIP: String;
        AppDeadSecs: Integer;
        AppGeneral: String;
        AppStatus: String;
        AppStarted: TDateTime;
        AppStopped: TDateTime;
        AppLastStartTick: Int64;
        AppLastPingTick: Int64;
        AppLastPongTick: Int64;
        AppStoppingTick: Int64;
        AppSuppState: TAMAppState;
        AppRestartMode: TAMRestartMode;
        AppStopState: TAMStopState;
        AppStartState: TAMStartState;
        AppEmailTitle: String;
        AppRestartInfo: String;
        AppEmailLines: String;
        AppEmailTick: Int64;
        AppLastOKDisp: String;           // used by IcsAppMonSrv
        AppStartStopDisp: String;        // used by IcsAppMonSrv
        AppStateInfoDisp: String;        // used by IcsAppMonSrv
        AppColorDisp: String;            // used by IcsAppMonSrv
        AppSevrNr: Integer;              // used by IcsAppMonMan
        AppLastState: TAMAppState;       // used by IcsAppMonMan
    end;
    TIcsAMApps = array of TIcsAMApp;

type
  TIcsAppMonCli = class(TComponent)
  private
    { Private declarations }
    MonitorClient: TIcsIpStrmLog;
    MonitorBroadcast: TIcsIpStrmLog;
    FMonitorTimer: TIcsTimer;
    FLocalIP: String;
    FAppTitle: String;
    FAppGeneral: String;
    FServiceName: String;
    FAutoStart: Boolean;
    FLANWide: Boolean;
    FLogPackets: Boolean;
    FPingMinSecs: Integer;
    FPingMaxSecs: Integer;
    FWinMess_Query: Cardinal;
    FWinMess_Info: Cardinal;
    FWindowHandle: HWND;
    FAppHandle: HWND;
    FLogEvent: TAMLogEvent;
    FAppMonMode: TAMMode;
    FAppMonRestart: TAMRestartMode;
    FCliState: TAMCliState;
    FLastBcastTick: Int64;
    FLastAwakeTick: Int64;
    FLastPingTick: Int64;
    FLastPongTick: Int64;
    FSrvIP: String;
    FSrvPort: String;
    FSrvName: String;
    FExeName: String;
    FFields: TStringList;
    FStartEmail: String;
    FLastBCastPacket: String;
    FLastAppStatus: String;      { V9.4 }
    FMonAnyway: Boolean;         { V9.5 }
    FOnWakeEvent: TNotifyEvent;  { V9.5 }
    procedure MonitorBroadcastLogProgEvent(Sender: TObject; Socnr: Integer; LogOption: TLogOption; const Msg: string);
    procedure MonitorBroadcastLogRecvEvent(Sender: TObject; Socnr: Integer; const Line: string);
    procedure MonitorClientProgEvent(Sender: TObject; Socnr: Integer; LogOption: TLogOption; const Msg: string);
    procedure MonitorClientChangeEvent(Sender: TObject; Socnr: integer; LogState: TStrmLogState);
    procedure MonitorClientRecvEvent(Sender: TObject; Socnr: Integer; const Line: string);
    procedure AddLogLine (S1: string);
    procedure MonitorTimerTimer(Sender: TObject);

  protected
    { Protected declarations }
    procedure WndProc(var Msg: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start(EmailBody: String);
    procedure Stop(EmailBody: String);
    procedure Awake(EmailBody: String = ''; AppStatus: String = '');
    procedure RestartNow(EmailBody: String);
    procedure Restarting(EmailBody: String);
    procedure ConnToServer;
    property CliState: TAMCliState        read FCliState;
  published
    { Published declarations }
    property LocalIP: String              read FLocalIP write FLocalIP;
    property AppTitle: String             read FAppTitle write FAppTitle;
    property AppGeneral: String           read FAppGeneral write FAppGeneral;
    property AppMonMode: TAMMode          read FAppMonMode write FAppMonMode;
    property AppHandle: HWND              read FAppHandle write FAppHandle;
    property ServiceName: String          read FServiceName write FServiceName;
    property AutoStart: Boolean           read FAutoStart write FAutoStart;
    property LANWide: Boolean             read FLANWide write FLANWide;
    property SrvIP: String                read FSrvIP write FSrvIP;
    property SrvPort: String              read FSrvPort write FSrvPort;
    property SrvName: String              read FSrvName write FSrvName;
    property LogPackets: Boolean          read FLogPackets write FLogPackets;
    property PingMinSecs: Integer         read FPingMinSecs write FPingMinSecs;
    property PingMaxSecs: Integer         read FPingMaxSecs write FPingMaxSecs;
    property MonAnyway: Boolean           read FMonAnyway write FMonAnyway;      { V9.5 }
    property onLogEvent: TAMLogEvent      read FLogEvent write FLogEvent;
    property OnWakeEvent: TNotifyEvent    read FOnWakeEvent write FOnWakeEvent;  { V9.5 }
  end;

  function IcsUnEscProtField(const Value: String): String;
  function IcsEscapeProtField(const Value: String): String;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// unescape protocol field
function IcsUnEscProtField(const Value: String): String;
begin
    Result := IcsUnEscapeCRLF(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// escape protocol field, can not contain CRLF or |
function IcsEscapeProtField(const Value: String): String;
begin
    Result := IcsEscapeCRLF(Value);
    if (Pos(ProtDelim, Result) > 0) then
        Result := IcsTransChar(Result, ProtDelim, '!'); // lazy, should not contain |
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonCli.AddLogLine (S1: string);
begin
    if Assigned(onLogEvent) then
        onLogEvent(Self, S1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsAppMonCli.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FFields := TStringList.Create;
    FFields.Delimiter := ProtDelim;
    FCliState := CliStateNone;
    FAppMonMode := AMModeMonitor;
    FAppTitle := 'MyAppId';
    FAutoStart := False;
    MonitorBroadcast := TIcsIpStrmLog.Create(Self);
    MonitorBroadcast.onLogProgEvent := MonitorBroadcastLogProgEvent;
    MonitorBroadcast.onLogRecvEvent := MonitorBroadcastLogRecvEvent;
    MonitorBroadcast.LogTitle := 'Client';
    MonitorClient := TIcsIpStrmLog.Create(Self);
    MonitorClient.onLogProgEvent := MonitorClientProgEvent;
    MonitorClient.onLogChangeEvent := MonitorClientChangeEvent;
    MonitorClient.onLogRecvEvent := MonitorClientRecvEvent;
    MonitorClient.LogTitle := 'Client';
    MonitorClient.MaxLineLen := DefMaxCliRecLen;
    FMonitorTimer := TIcsTimer.Create(MonitorClient);
    FMonitorTimer.OnTimer := MonitorTimerTimer;
    FMonitorTimer.Interval := 1 * TicksPerSecond ;
    FMonitorTimer.Enabled := False ;
    FExeName := ParamStr(0);
  // strip off arguments ?
    FPingMinSecs := 60;        // don't ping more often than x seconds
    FPingMaxSecs := DefDeadSecs;    // restart if not pinged for more than x seconds
    FLocalIP := '';
    FAppTitle := '';
    FServiceName := '';
    FSrvIP := '';
    FSrvPort := '';
    FLANWide := False;
    FAppMonRestart := RestModeNone;
    FMonAnyway := True;   { V9.5 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsAppMonCli.Destroy;
begin
    try
        FMonitorTimer.Enabled := False;
        if NOT (FCliState in [CliStateNone, CliStateStopped]) then begin
            MonitorBroadcast.StopLogging;
        end;
        FFields.Free;
        if Assigned( FMonitorTimer) then
            FMonitorTimer.Free;
        MonitorClient.Free;
        MonitorBroadcast.Free;
        DeallocateHWnd (FWindowHandle);
    finally
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonCli.WndProc(var Msg: TMessage);
var
    NewIP, NewPort: String;
begin
  // server monitor is sending us it's TCP/IP address and port
    if (Msg.Msg = FWinMess_Info) then begin
    //  AddLogLine('Broadcast Message, W=' + IntToStr(Msg.wParam) + ', L=' + IntToStr(Msg.lParam));  // !! TEMP

       if FCliState <= CliStateListen then begin                           // ignore if connected to server
            if (Msg.lParam > 1024) and (Msg.lParam < 65555) then begin     // sanity check
                NewIP := WSocketIPv4ToStr(Msg.wParam);
                NewPort := IntToStr(Msg.lParam);
                AddLogLine('Server Info Message, Server IP ' + NewIP + ', Port ' + NewPort);
                if (NewIP <> FSrvIP) or (NewPort <> FSrvPort) then begin
                    FSrvName := '';
                    FSrvIP := NewIP;
                    FSrvPort := NewPort;
                end;
                if FCliState = CliStateListen then
                    FCliState := CliStateStart;      // triggers server connect
            end;
        end;
    end
    else
        Msg.Result := DefWindowProc (FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonCli.Start(EmailBody: String);
begin
    try
        AddLogLine('Starting Monitoring Client, Version: ' + IcsAppMonDate);
        FWindowHandle := AllocateHWnd (WndProc);
        FWinMess_Query := RegisterWindowMessage (IcsAppMonMessQuery) ;
        FWinMess_Info := RegisterWindowMessage (IcsAppMonMessInfo) ;

    // set initial state and triggers
        FLastAwakeTick := Trigger64Disabled;
        FLastPingTick := Trigger64Disabled;
        FLastPongTick := Trigger64Disabled;
        FLastBcastTick := Trigger64Disabled;
        FCliState := CliStateNone;
        FAppMonRestart := RestModeNot;
        FLastAppStatus := 'Starting';
        if FAppMonMode = AMModeNonStop then
            FAppMonRestart := RestModeAuto;
//        FLastAwakeTick:= IcsGetTickCount64;
         if Length(EmailBody) > (DefMaxCliRecLen - 100) then  // truncate email if too long for packet
           SetLength(EmailBody, DefMaxCliRecLen - 100);
        FStartEmail := EmailBody;

   // get server IP and port from registry, if running on same PC, and not previously set
        if (FSrvIP = '') or (NOT FLANWide) then begin
            FSrvIP := IcsRegGetHlm(IcsAppMonRegKey, IcsAppMonRegAddr);
            FSrvPort := IcsRegGetHlm(IcsAppMonRegKey, IcsAppMonRegPort);
         // ignore server timestamp for first connect attempt
            if (FSrvIP <> '') then begin
                AddLogLine('Server Info from Registry, IP ' + FSrvIP + ', Port ' + FSrvPort);
                FCliState := CliStateStart;  // skip listening for broadcast or message with IP address
                FLastBcastTick := IcsGetTickCount64;
            end
            else
                AddLogLine('Server Registry Info Not Found: HLM:' + IcsAppMonRegKey + '\' + IcsAppMonRegAddr);
        end;

    // if sever supports LAN wide computers, listen for broadcast messages
    // note only one application can listen on the broadcast port on the same computer
        try
            if FLANWide then begin
                MonitorBroadcast.LocalIpAddr := FLocalIP;
                MonitorBroadcast.LocalIpPort := DefBcastPort;
                MonitorBroadcast.LogProtocol := logprotUdpServer;
                MonitorBroadcast.AddCRLF := True;
                MonitorBroadcast.StartLogging;
                AddLogLine('Started Broadcast on Port ' + DefBcastPort) ;
            end;
        except
            AddLogLine('Aborting - Failed to Start Monitoring Broadcast - ' + IcsGetExceptMess (ExceptObject)) ;
        end;

    // all ready
        FMonitorTimer.Enabled := True ;
        AddLogLine('Started Monitoring Client OK');

    // open monitor connection immediately, if possible
        MonitorTimerTimer(Self);
    except
        AddLogLine('Failed to Start Monitoring Client - ' + IcsGetExceptMess (ExceptObject)) ;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonCli.MonitorTimerTimer(Sender: TObject);
var
    PingSecs, AwakeSecs: Integer;
    Packet: String;
    ServerDT: TDateTime;
begin
    FMonitorTimer.Enabled := False;
    try

    // ask server monitor to send it information message so we know the TCP/IP server address and port
    // only works for client and server on same computer, otherwise need to wait for UDP broadcast packet
        if FCliState = CliStateNone then begin
            FCliState := CliStateListen;

        // need server IP
            if (FSrvIP = '') then begin
         // messages are only useful if monitor is running in the desktop window
                AddLogLine('Query Server Information Message');  // !! TEMP
                PostMessage(HWND_BROADCAST, FWinMess_Query, IcsAppMonMessParam, 0);
                FLastBcastTick := IcsGetTickCount64;

            // see if server has started
                FSrvIP := IcsRegGetHlm(IcsAppMonRegKey, IcsAppMonRegAddr);
                FSrvPort := IcsRegGetHlm(IcsAppMonRegKey, IcsAppMonRegPort);
                if (FSrvIP <> '') then begin
                    AddLogLine('Server Info from Registry, IP ' + FSrvIP + ', Port ' + FSrvPort);
                    FCliState := CliStateStart;
                end;
            end
            else begin
                if (FSrvIP <> '') then begin
                    FCliState := CliStateStart;
                end;
            end;
        end;

    // start up, got server information, connect to server
        if (FCliState = CliStateStart) and (FSrvIP <> '') then begin
            FLastBcastTick := IcsGetTickCount64;
            ConnToServer;
        end;

    // see if time to send a PING, and check server responsed with a PONG
        if (FCliState = CliStateOK) then begin
        if True then

            PingSecs := IcsElapsedSecs64 (FLastPingTick);

        // exceeded our maximum wait time
            if PingSecs > FPingMaxSecs then begin
                RestartNow('Restart Server Request, No Awake Ping for ' + IntToStr(PingSecs) + ' seconds');
                Exit;
            end;

        // ensure pong received or restart client
            if (FLastPongTick <> 0) and (FLastPongTick < FLastPingTick) then begin
                if (PingSecs > 30) then begin
                    FCliState := CliStateStart;
                    MonitorClient.StopLogging;
                end;
                exit;
            end;

        // only send pings after minimum interval, awake may be more often
            AwakeSecs := IcsElapsedSecs64 (FLastAwakeTick);
            if (FLastPingTick = Trigger64Disabled) or            { V9.4 first time always }
                  (AwakeSecs < FPingMinSecs) and (PingSecs >= FPingMinSecs) then begin    { V9.4 no ping if no Awake }
                Packet := IcsAppMonName + ProtDelim +           // CliCFieldAppMon
                          ProtCmdPing + ProtDelim +             // CliCFieldCommand
                          FAppTitle + ProtDelim +               // CliCFieldAppTitle
                          '' + ProtDelim +                      // CliCFieldEmail
                          IcsEscapeProtField(FLastAppStatus) + ProtDelim +  // CliSFieldStatus   V9.4
                          ProtDelim;
                MonitorClient.SendLogLine(Packet);
                if FLogPackets then
                    AddLogLine('PING Packet Sent: ' + Packet);
                FLastPingTick := IcsGetTickCount64;
            end;
        end;

    // not connected to server, check if registry updated by server every 30 seconds
        if (FCliState = CliStateReconnnect) then begin

        // if server updated registry in the last three minutes, try another connect
            if (NOT FLANWide) and (IcsElapsedSecs64 (FLastBcastTick) > 30) then begin
                ServerDT := RFC3339_StrToDate(IcsRegGetHlm(IcsAppMonRegKey, IcsAppMonRegDT));
                if (NOw - ServerDT) < (OneMinuteDT * 3)  then begin
                    AddLogLine('Monitor Server Seems to Running Again, Reconnnect Starting');
                    if (FSrvIP <> '') then
                        FCliState := CliStateStart
                    else
                        FCliState := CliStateNone;
                end;
            end;

        // LAN Wide, retry every fine minites
            if FLANWide and (IcsElapsedMins64 (FLastBcastTick) > 5) then begin
                AddLogLine('Attempting Reconnect to Monitor Server');
                if (FSrvIP <> '') then
                    FCliState := CliStateStart
                else
                    FCliState := CliStateNone;
            end;
       end;

   // no response to our STOP message, stop anyway
        if FCliState = CliStateStopping then begin
            PingSecs := IcsElapsedSecs64 (FLastPingTick);
            if PingSecs > 30 then begin
                AddLogLine('No Response to STOP Packet, Stopping');
                FCliState := CliStateStopped;
            end;
        end;

   // close server
        if FCliState = CliStateStopped then begin   // got PONG for STOP packet, close connection
            AddLogLine('Stopping Monitoring Client');
            MonitorClient.StopLogging;
            MonitorBroadcast.StopLogging;
        end;

    finally
        if FCliState <> CliStateStopped then
            FMonitorTimer.Enabled := True
        else
            AddLogLine('Stopped Monitoring Client');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonCli.ConnToServer;
begin
    if (FSrvIP = '') or (FSrvPort = '') then begin
        AddLogLine('Can Not Connect to Monitor Server without it''s IP Address');
        Exit;
    end;

    AddLogLine ('Connecting to Monitor Server on ' + FSrvIP + ':' + FSrvPort);
    try
        if MonitorClient.LogActive then
            MonitorClient.StopLogging;
        MonitorClient.RemoteHost := FSrvIP;
        MonitorClient.RemoteIpPort := FSrvPort;
        MonitorClient.LogProtocol := logprotTcpClient;
        MonitorClient.RetryAttempts := -1;  // no retries, let timer retry to connect
        MonitorClient.AutoReconnect := False;  // we handle reconnect ourself here
        MonitorClient.MaxSockets := 1;
        MonitorClient.MaxLineLen := 1024;
        MonitorClient.AddCRLF := True;
        if MonitorClient.StartLogging then begin
            FCliState := CliStateRegister;   // wait for connect
        end
        else begin
            AddLogLine('Failed to Start Monitor Server - ' + MonitorClient.LastErrorStr) ;
            FCliState := CliStateReconnnect;
        end;
    except
        AddLogLine('Failed to Connect to Monitor Server - ' + IcsGetExceptMess (ExceptObject)) ;
        FCliState := CliStateReconnnect;
    end ;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// should be called by the application every few seconds from a timer
procedure TIcsAppMonCli.Awake(EmailBody: String = ''; AppStatus: String = '');
var
    Packet: String;
begin
    if (FCliState <> CliStateOK) then
        Exit;
    FLastAwakeTick:= IcsGetTickCount64;
    try

    // only send PING now if sending email or no status sent otherwise sent from timer every x seconds
        if (FLastPingTick = Trigger64Disabled) or            { V9.4 first time always }
                ((EmailBody <> '') or ((FLastAppStatus = '') and (AppStatus <> ''))) then begin
            if Length(EmailBody) > (DefMaxCliRecLen - 100) then  // truncate email if too long for packet
                SetLength(EmailBody, DefMaxCliRecLen - 100);
            Packet := IcsAppMonName + ProtDelim +           // CliCFieldAppMon
                      ProtCmdPing + ProtDelim +             // CliCFieldCommand
                      FAppTitle + ProtDelim +               // CliCFieldAppTitle
                      IcsEscapeProtField(EmailBody) + ProtDelim +      // CliCFieldEmail
                      IcsEscapeProtField(AppStatus) + ProtDelim +      // CliSFieldStatus
                      ProtDelim;
            MonitorClient.SendLogLine(Packet);       { V9.4 }
            if FLogPackets then
                AddLogLine('PING Packet Sent: ' + Packet);
            FLastPingTick:= IcsGetTickCount64;
        end;
        FLastAppStatus := AppStatus;   { V9.4 }
    except
        AddLogLine('Failed to Send Packet - ' + IcsGetExceptMess (ExceptObject)) ;
        FCliState := CliStateNone;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// tell server application monitoring is stopping
// very important!, tells servers whether to restart us or not, according to FAppMonRestart, ONESTOP mode usually restarts
procedure TIcsAppMonCli.Stop(EmailBody: String);
var
    Packet: String;
begin
    if FCliState >= CliStateStopping then
        exit;
    try
     // special case of immediately cease monitoring, to simulate application going non-responsive
        if (EmailBody = 'CRASH-ME') then begin
            AddLogLine('Stopping Monitoring Client Immediately, No STOP Packet');
            FCliState := CliStateStopped;
            MonitorClient.StopLogging;
            MonitorBroadcast.StopLogging;
            FMonitorTimer.Enabled := False;
            Exit;
        end;

    // normal close, tell server
        if FCliState = CliStateOK then begin
            if FAppMonRestart = RestModeAuto then
                FAppMonRestart := RestModeNot;
            AddLogLine('Stopping Monitoring Client Connection to Server');
            if Length(EmailBody) > (DefMaxCliRecLen - 100) then  // truncate email if too long for packet
                SetLength(EmailBody, DefMaxCliRecLen - 100);
            Packet := IcsAppMonName + ProtDelim +                     // CliCFieldAppMon
                      ProtCmdStop + ProtDelim +                       // CliCFieldCommand
                      FAppTitle + ProtDelim +                         // CliCFieldAppTitle
                      IcsEscapeProtField(EmailBody) + ProtDelim +     // CliCFieldEmail
                      AMRestModeCmds[FAppMonRestart] + ProtDelim +     // CliSFieldRestart
                      ProtDelim;
            MonitorClient.SendLogLine(Packet);
            if FLogPackets then
                AddLogLine('STOP Packet Sent: ' + Packet);
     //       FLastPingTick := IcsGetTickCount64;
        end;

     // wait for PONG packet before closing client, or 30 seconds
        FCliState := CliStateStopping;
    except
        AddLogLine(IcsAppMonTitle + ' : Failed to Stop Monitoring Client - ' + IcsGetExceptMess (ExceptObject)) ;
        FMonitorTimer.Enabled := False;   // dead
        MonitorClient.StopLogging;
        MonitorBroadcast.StopLogging;
        FCliState := CliStateStopped;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// ask AppMon to restart this application, with a reason
procedure TIcsAppMonCli.RestartNow(EmailBody: String);
begin
    FAppMonRestart := RestModeRequest;
    Stop(EmailBody);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// client is trying to restart itself, usually by letting Service Manager do it on stop
procedure TIcsAppMonCli.Restarting(EmailBody: String);
begin
    FAppMonRestart := RestModeItelf;
    Stop(EmailBody);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonCli.MonitorBroadcastLogProgEvent(Sender: TObject; Socnr: Integer; LogOption: TLogOption; const Msg: string);
begin
    AddLogLine(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonCli.MonitorBroadcastLogRecvEvent(Sender: TObject; Socnr: Integer; const Line: string);
var
    NewSrv: String;
begin
    if FLastBCastPacket = Line then
        Exit;
    if FLogPackets then
        AddLogLine('UDP Packet Recvd: ' + Trim(Line));
    try
        FFields.Clear;
        FFields.DelimitedText := Line;
        if FFields.Count > BcastFieldLast then begin
            FLastBCastPacket := Line;
            if FFields[BcastFieldAppMon] = IcsAppMonName then begin
                if FFields[BcastFieldCommand] = ProtCmdHello then begin
                    NewSrv := FFields[BcastFieldSrvName];
                    if (NOT FLANWide) and (NewSrv <> Uppercase(IcsGetCompName)) then begin
                        AddLogLine('Ignored Broadcast from: ' + NewSrv);
                        Exit;
                    end;
                    FSrvName := FFields[BcastFieldSrvName];
                    FSrvIP := FFields[BcastFieldSrvIP];
                    FSrvPort := FFields[BcastFieldSrvPort];
                    if FCliState = CliStateListen then
                       FCliState := CliStateStart;      // triggers server connect
                end
                else
                    AddLogLine('Unexpected UDP Packet, Bad Command');
            end
            else
                AddLogLine('Unexpected UDP Packet, Bad AppId');
        end
        else
            AddLogLine('Unexpected UDP Packet, Too Short');
    except
        AddLogLine('Failed Process UDP Packet - ' + IcsGetExceptMess (ExceptObject)) ;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonCli.MonitorClientProgEvent(Sender: TObject; Socnr: Integer; LogOption: TLogOption; const Msg: string);
begin
    AddLogLine(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonCli.MonitorClientChangeEvent(Sender: TObject; Socnr: integer; LogState: TStrmLogState);
var
    Packet: String;
begin
    AddLogLine ('Client Socket State ' + StrmLogStateNames[LogState]) ;

 // connected to server, announce ourselves with HELLO command
    try
        if LogState = logstateOK then begin
            if FCliState = CliStateRegister then begin
                if FAppHandle = 0 then
                    FAppHandle := FWindowHandle;
                Packet := IcsAppMonName + ProtDelim +            // CliCFieldAppMon
                    ProtCmdHello + ProtDelim +                   // CliCFieldCommand
                    FAppTitle + ProtDelim +                      // CliCCFieldAppTitle
                    IcsEscapeProtField(FStartEmail) + ProtDelim +   // CliCFieldEmail
                    AMModeCmds[FAppMonMode] + ProtDelim +        // CliHFieldAppMode
                    FExeName + ProtDelim +                       // CliHFieldExeFile
                    FServiceName + ProtDelim +                   // CliHFieldServiceName
                    IntToStr(Int64(FAppHandle)) + ProtDelim +    // CliHFieldWinHnd
                    IntToStr(Int64(GetCurrentProcessId)) + ProtDelim +  // CliHFieldWinPID
                    Uppercase(IcsGetCompName)  + ProtDelim +     // CliHFieldCompName
                    FLocalIP  + ProtDelim +                      // CliHFieldLocalIP
                    IntToStr(FPingMaxSecs) + ProtDelim +         // CliHFieldDeadSecs
                    IcsEscapeProtField(FAppGeneral) + ProtDelim +   // CliHFieldAppGeneral
                    ProtDelim;
                MonitorClient.SendLogLine(Packet);
                if FLogPackets then
                    AddLogLine('Registered with Monitor Server: ' + Packet)
                else
                    AddLogLine('Registered with Monitor Server');
                FCliState := CliStateOK;                     // beware, might get DECLINE packet back
           //     FLastPingTick:= IcsGetTickCount64;
            end;
            if FCliState <> CliStateOK then begin
                AddLogLine('!! Unexpected State, Setting OK');
                FCliState := CliStateOK;
            end;
        end;

      // starting, must register
        if LogState = logstateStart then
            FCliState := CliStateRegister;

     // if stopping or stopped, client should try to connect again
        if (LogState in [logstateStopping, logstateNone]) then begin
            if FCliState < CliStateStopping then begin
                FCliState := CliStateReconnnect;
                AddLogLine('Monitor Server Connection Lost, Will Attempt Reconnection');
            end
            else
                AddLogLine('Monitor Server Connection Closed and Stopped');
        end;
    except
        AddLogLine('Failed to Send Packet - ' + IcsGetExceptMess (ExceptObject)) ;
        FCliState := CliStateStopped;    // close client in timer
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// handle packets received from monitor server
procedure TIcsAppMonCli.MonitorClientRecvEvent(Sender: TObject; Socnr: Integer; const Line: string);
begin
    if FLogPackets then
        AddLogLine('Server Packet Recvd: ' + Trim(Line));
    try
        FFields.Clear;
        FFields.DelimitedText := Line;
        if FFields.Count > SrvFieldLast then begin
            if FFields[SrvFieldAppMon] = IcsAppMonName then begin
                if FFields[SrvFieldCommand] = ProtCmdPong then begin
                    if FLogPackets then
                        AddLogLine('PONG Packet Received');
                    FLastPongTick := IcsGetTickCount64;
                    if FCliState = CliStateStopping then begin  // got PONG for STOP packet, close connection
                       FCliState := CliStateStopped;   // close client in timer
                       AddLogLine('Server Acknowleged Stopping Request');
                    end;
                end
                else if FFields[SrvFieldCommand] = ProtCmdWake then begin
                    AddLogLine('WAKEUP Packet Received, Sending PING');
                    FLastAwakeTick := IcsGetTickCount64 - TicksPerHour;  // force awake trigger
                    if Assigned(FOnWakeEvent) then                       // V9.5 allow application to wake up, in some way...
                        FOnWakeEvent(Self);
                end
                else if FFields[SrvFieldCommand] = ProtCmdDecline then begin
                    AddLogLine('DECLINE Packet Received, Stop Monitoring');
                    if FMonAnyway and (FAppMonMode = AMModeNonStop) then begin      { V9.5 }
                        FAppMonMode := AMModeMonitor;
                        FCliState := CliStateStart;
                        AddLogLine(IcsAppMonTitle + ' has declined to NONSTOP monitor, try MONITOR Only');
                    end
                    else begin
                      // must not send any more packets
                        FCliState := CliStateStopped;    // close client in timer
                        AddLogLine(IcsAppMonTitle + ' has declined to monitor this application, Stopping Monitoring');
                    end;
                end
                else
                    AddLogLine('Unexpected Server Packet, Bad Command');
            end
            else
                AddLogLine('Unexpected Server Packet, Bad Command');
        end;
    except
        AddLogLine('Failed to Handle Received Packet - ' + IcsGetExceptMess (ExceptObject)) ;
        FCliState := CliStateStopped;    // close client in timer
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
