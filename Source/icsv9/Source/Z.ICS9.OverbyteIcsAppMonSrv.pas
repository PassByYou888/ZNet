{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  The ICS Application Monitor TIcsAppMonSrv server is designed to monitor
              ICS applications using the TIcsAppMonCli client component,
              and ensure they remain running, restarting the application
              if it stops or becomes non-responsive, or on demand.
              Primarily to keep ICS server Windows services running
              non-stop, but may also be used for network wide monitoring
              of ICS applications.
Creation:     Jan 2025
Updated:      Sep 2025
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



Feb 20, 2025 V9.4 - Baseline, split from sample app IcsAppMonMain, using new
                      OverbyteIcsWinUtils unit instead of MagXXX units.
Sep 10, 2025 V9.5 - If Windows Service fails to start, keep trying to restart
                      it rather than giving up.



See OverbyteIcsDDWebServiceSrv.pas and OverbyteIcsIpStmLogTst1.pas for examples
of how to use TIcsAppMonCli to monitor a Windows web server service
application and a simple Windows application.

The server sample is IcsAppMon.dproj which uses the TIcsAppMonSrv component
in OverbyteIcsAppMonSrv.pas, and may be run as a Windows GUI or installed as
a Windows Service.  The server sample also includes web and websocket servers
that provide status information on the applications being monitored.

Pending - support for INSTALL command to install applications, really needs
  downloading of new software as well for complete updating solution.
Pending - server and/or client send USERINFO to each other.

Warning - unicode compilers only

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit Z.ICS9.OverbyteIcsAppMonSrv;

interface

{$I include\Z.ICS9.OverbyteIcsDefs.inc}

Uses
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$IFDEF Rtl_Namespaces}Winapi.Messages{$ELSE}Messages{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.IniFiles{$ELSE}IniFiles{$ENDIF},
  {$IFDEF Rtl_Namespaces}WinApi.WinSvc{$ELSE}WinSvc{$ENDIF},
  Z.ICS9.OverbyteIcsTypes,
  Z.ICS9.OverbyteIcsUtils,
  Z.ICS9.OverbyteIcsTicks64,
  Z.ICS9.OverbyteIcsWndControl,
  Z.ICS9.OverbyteIcsIpStreamLog,
  Z.ICS9.OverbyteIcsHtmlUtils,
  Z.ICS9.OverbyteIcsUrl,
  Z.ICS9.OverbyteIcsWinUtils,
  Z.ICS9.OverbyteIcsAppMonCli;

const
    sTableData = '<table class="data">' ;
    sTableOpen = '<table class="open">' ;
    sTableEnd = '</table>' ;
    sTagTRs = '<tr>' + IcsCRLF ;
    sTagTRsc = '<tr bgcolor="#' ;  // row colour start hex RGB, https://htmlcolorcodes.com/
    sTagTRse = '">' + IcsCRLF ;    // row colour end
    sTagTRe = '</tr>' + IcsCRLF ;
    sTagPs  = '<p>' ;
    sTagPBolds = '<p class="bodybold">';
    sTagPe  = '</p>' + IcsCRLF ;
    sTagTDs = '<td>' ;
    sTagTDe = '</td>' + IcsCRLF ;
    sTagBR = '<br>' + IcsCRLF ;
    sTagAnd = '&amp;' ;
    sTagSpace = '&nbsp;' ;

type
  TAMEmailEvent = procedure (Sender: TObject; const Subject, Body: string; Fatal: Boolean = False) of object;

  TIcsAppMonSrv = class(TComponent)
  private
    { Private declarations }
    FMonIpBcast: TIcsIpStrmLog;
    FMonIpServer: TIcsIpStrmLog;
    FMonSuppApp: TIcsAMApps;
    FMonTimer: TIcsTimer;
    FMonSrvIP: String;
    FMonSrvPort: String;
    FMonBcastIP: String;
    FMonBcastPort: String;
    FMaxSuppApps: Integer;
    FBCastSec: Integer;        // how often to broadcast our IP/port
    FDeadSecs: Integer;        // how long server waits before assuming application is dead, client controlled
    FMinDeadSecs: Integer;     // minimum wait before restarting server
    FStopWaitSecs : Integer;   // how long to wait for application to stop
    FStoppingFlag: Boolean;
    FStartWaitSecs: Integer;   // how long to wait before restarting application
    FLogPackets: Boolean;      // diag log raw packets
    FWinService: Boolean;      // must set if Windows Service app
    FEmailEvent: TAMEmailEvent;
    FLogEvent: TAMLogEvent;
    FChangeEvent: TNotifyEvent;
 //    FHardwareEvents: THardwareEvents;
    FTotSuppApps: Integer;
    FWinMess_Query: Cardinal;
    FWinMess_Info: Cardinal;
    FTriggerBcast: Int64;
    FTriggerMinute: Int64;
    FAppFields: TStringList;
    FLANWide: Boolean;
    FIsAdmin: Boolean;
    FAppsChangedFlag: Boolean;
    FJsonRecord: TRestParams;
    FJsonResult: TRestParams;
    procedure MonTimerTimer(Sender: TObject);
    procedure MonIpBcastLogProgEvent(Sender: TObject; Socnr: Integer; LogOption: TLogOption; const Msg: string);
    procedure MonIpBcastLogRecvEvent(Sender: TObject; Socnr: Integer; const Line: string);
    procedure MonIpServerLogProgEvent(Sender: TObject; Socnr: Integer; LogOption: TLogOption; const Msg: string);
    procedure MonIpServerLogRecvEvent(Sender: TObject; Socnr: Integer; const Line: string);
    procedure MonIpServerLogChangeEvent(Sender: TObject; Socnr: Integer; LogState: TStrmLogState);
//    procedure HardwareEventsPowerEvent(Sender: TObject; PowerEvent: Integer; const Desc: string);
//    procedure TriggerMessageEvent(var Msg: TMsg; var Handled: Boolean); virtual;
    procedure AddLogLine(S1: string);
    procedure SendAdminEmail (const Subject, Body: string; Fatal: Boolean = False) ;
    function  GetMonActive: Boolean;
    function GetListenStates: String;
    function GetStatusWeb: String;
    function GetStatusJson: String;
    function GetCurClients: Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    SrvInfoJson: TRestParams;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MonStartup: Boolean;
    procedure MonStop;
    procedure SendBroadcast;
    function AddSuppApp: Integer;
    function FindSuppApp(ClientID: Integer): Integer;
    procedure RemSuppAppId(ClientID: Integer);
    procedure RemSuppAppNr(AppNr: Integer);
    procedure ClrSuppAppNr(AppNr: Integer);
    function IsAppRunning(AppNr: Integer): Boolean;
    procedure AppStopOne(AppNr: Integer);
    procedure AppStartOne(AppNr: Integer);
    procedure UpdateDispInfo;
    property  MonActive: Boolean      read GetMonActive;
    property  ListenStates: String    read GetListenStates;
    property  StatusWeb: String       read GetStatusWeb;
    property  StatusJson: String      read GetStatusJson;
    property  TotSuppApps: Integer    read FTotSuppApps;
    property  MonSuppApp: TIcsAMApps  read FMonSuppApp;
    property  CurClients: Integer     read GetCurClients;
  published
    { Published declarations }
    property MaxSuppApps: Integer     read FMaxSuppApps write FMaxSuppApps;
    property MonSrvIP: String         read FMonSrvIP write FMonSrvIP;
    property MonSrvPort: String       read FMonSrvPort write FMonSrvPort;
    property MonBcastIP: String       read FMonBcastIP write FMonBcastIP;
    property MonBcastPort: String     read FMonBcastPort write FMonBcastPort;
    property LANWide: Boolean         read FLANWide write FLANWide;
    property BCastSec: Integer        read FBCastSec write FBCastSec;
    property DeadSecs: Integer        read FDeadSecs write FDeadSecs;
    property MinDeadSecs: Integer     read FMinDeadSecs write FMinDeadSecs;
    property StopWaitSecs: Integer    read FStopWaitSecs write FStopWaitSecs;
    property StartWaitSecs: Integer   read FStartWaitSecs write FStartWaitSecs;
    property LogPackets: Boolean      read FLogPackets write FLogPackets;
    property WinService: Boolean      read FWinService write FWinService;
    property onEmailEvent: TAMEmailEvent read FEmailEvent write FEmailEvent;
    property onLogEvent: TAMLogEvent  read FLogEvent write FLogEvent;
    property onChangeEvent: TNotifyEvent read FChangeEvent write FChangeEvent;
  end;

procedure IcsLoadAppMonSrvFromIni(MyIniFile: TCustomIniFile; IcsAppMonSrv: TIcsAppMonSrv; const Section: String = 'IcsAppMonSrv');

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonSrv.AddLogLine (S1: string);
begin
    if Assigned(FLogEvent) then
        FLogEvent(Self, S1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonSrv.SendAdminEmail(const Subject, Body: string; Fatal: Boolean = False) ;
begin
    if Assigned(FEmailEvent) then
        FEmailEvent(Self, Subject, Body, Fatal);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsAppMonSrv.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FStoppingFlag := False;
    FMaxSuppApps := 32;
    FBCastSec := DefBCastSec;
    FDeadSecs := DefDeadSecs;
    FMinDeadSecs := DefMinDeadSecs;
    FStopWaitSecs := DefStopWaitSecs;
    FStartWaitSecs := DefStartWaitSecs;

    FMonIpServer := TIcsIpStrmLog.Create(Self);
    FMonIpServer.onLogRecvEvent := MonIpServerLogRecvEvent;
    FMonIpServer.onLogChangeEvent := MonIpServerLogChangeEvent;
    FMonIpServer.onLogProgEvent := MonIpServerLogProgEvent;

    FMonIpBcast := TIcsIpStrmLog.Create(Self);
    FMonIpBcast.onLogRecvEvent := MonIpBcastLogRecvEvent;
    FMonIpBcast.onLogProgEvent := MonIpBcastLogProgEvent;

    FJsonRecord := TRestParams.Create(Nil);
    FJsonRecord.PContent := PContJson;
    FJsonResult := TRestParams.Create(Nil);
    FJsonResult.PContent := PContJson;
    SrvInfoJson := TRestParams.Create(Nil);
    SrvInfoJson.PContent := PContJson;

// power events
//    FHardwareEvents := THardwareEvents.Create(Self);
//    FHardwareEvents.onPowerEvent := HardwareEventsPowerEvent;

    FMonTimer := TIcsTimer.Create(FMonIpBcast);
    FMonTimer.OnTimer := MonTimerTimer;
    FMonTimer.Interval :=1000;
    FMonTimer.Enabled := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsAppMonSrv.Destroy;
begin
    try
        if Assigned(FMonTimer) then
            FMonTimer.Enabled := False;
        if NOT FStoppingFlag then begin
            MonStop;
        end;
        FreeAndNil(FJsonRecord);
        FreeAndNil(FJsonResult);
        FreeAndNil(SrvInfoJson);
        FreeAndNil(FMonIpServer);
        FreeAndNil(FMonIpBcast);
        if Assigned(FMonTimer) then begin
            try
                FMonTimer.Free;   // strange pointer exception
            except
            end;
            FMonTimer := Nil;
        end;
    finally
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsAppMonSrv.GetMonActive: Boolean;
begin
    Result := False;
    if NOT Assigned(FMonIpServer) then
        Exit;
    if NOT Assigned(FMonIpBcast) then
        Exit;
     Result := FMonIpServer.LogActive {and FMonIpBcast.LogActive};
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsAppMonSrv.GetCurClients: Integer;
begin
    Result := 0;
    if NOT Assigned(FMonIpServer) then
        Exit;
     Result := FMonIpServer.CurSockets;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsAppMonSrv.AddSuppApp: Integer;
var
    I: Integer;
begin
    Result := -1;
    if FTotSuppApps >= FMaxSuppApps then begin
        FMaxSuppApps := FMaxSuppApps * 2;
        SetLength(FMonSuppApp, FMaxSuppApps);
    end;
    FTotSuppApps := FTotSuppApps + 1;
    for I := 0 to FMaxSuppApps - 1 do begin
        if (FMonSuppApp[I].AppClientID = 0) then begin
            Result := I;
            ClrSuppAppNr(I);
            exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsAppMonSrv.FindSuppApp(ClientID: Integer): Integer;
var
    I: Integer;
begin
    Result := -1;
    if FTotSuppApps = 0 then
        Exit;
    for I := 0 to FMaxSuppApps - 1 do begin
        if (FMonSuppApp[I].AppClientID = ClientId) then begin
            Result := I;
            exit;
        end;
    end;
end;


// note, not currently removing apps when they stop, just setting clientId to zero so they can be reused by same exe/service
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonSrv.RemSuppAppId(ClientID: Integer);
var
    Nr: Integer;
begin
    Nr := FindSuppApp(ClientID);
    if Nr >= 0 then
        RemSuppAppNr(Nr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonSrv.RemSuppAppNr(AppNr: Integer);
begin
    if AppNr >= FMaxSuppApps then
        Exit;
    ClrSuppAppNr(AppNr);
    FTotSuppApps := FTotSuppApps - 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonSrv.ClrSuppAppNr(AppNr: Integer);
begin
    with FMonSuppApp[AppNr] do begin
        AppClientTitle := 'New Remote Client';
        AppClientID := 0;
        AppExeFile := '';
        AppServiceName := 'NONE';
        AppWinHnd := 0;
        AppCompName := '';
        AppLocalIP := '';
        AppDeadSecs := 0;
        AppStarted := 0;
        AppLastStartTick := Trigger64Disabled;
        AppLastPingTick := Trigger64Disabled;
        AppLastPongTick := Trigger64Disabled;
        AppStoppingTick := Trigger64Disabled;
        AppSuppState := AppStateNone;
        AppRestartMode := RestModeAuto;
        AppStopState := StopStateNone;
        AppStartState := StartStateNone;
        AppEmailLines := '';
        AppEmailTick := Trigger64Disabled;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsAppMonSrv.MonStartup: Boolean;
var
    Access: DWORD;
    S: String;
begin
    FStoppingFlag := False;
    FAppsChangedFlag := False;
    Result := False;
    try
        FTotSuppApps := 0;
        SetLength(FMonSuppApp, FMaxSuppApps);
        FAppFields := TStringList.Create;
        FAppFields.Delimiter := ProtDelim;
        FAppFields.StrictDelimiter := True;

    // Windows Service Manager
        FIsAdmin := IcsIsProgAdmin;
        if FIsAdmin then
            Access := Ics_SCM_ADMIN
        else
            Access := Ics_SCM_QUERY;
        if IcsSCMOpen(Access, S) then
            AddLogLine (S) ;

   // TCP/IP server for clients to talk to us
        FMonIpServer.SrvIcsHosts.Clear;
        FMonIpServer.SrvIcsHosts.Add;
        with FMonIpServer.SrvIcsHosts[0] do begin
            BindIpAddr := FMonSrvIP ;
            BindNonPort := atoi(FMonSrvPort);
            HostEnabled := True;
            HostTag := 'MonServer' ;
            Descr := HostTag;
        end;
        FMonIpServer.LogProtocol := logprotTcpServer;
        FMonIpServer.LogTitle := IcsAppMonName + ' TCP Server';
        FMonIpServer.MaxSockets := FMaxSuppApps + 5;
        FMonIpServer.MaxLineLen := 4096;
        FMonIpServer.AddCRLF := True;
        if NOT FMonIpServer.StartLogging then   // error should have been logged
            Exit;
        AddLogLine ('Started Monitor Server on ' + FMonSrvIP + ':' + FMonSrvPort);

    // broadcast to clients to tell them who we are
        FWinMess_Query := RegisterWindowMessage (IcsAppMonMessQuery) ;
        FWinMess_Info := RegisterWindowMessage (IcsAppMonMessInfo) ;
        if FLANWide then begin
            FMonIpBcast.RemoteHost := FMonBcastIP;
            FMonIpBcast.RemoteIpPort := FMonBcastPort;
            FMonIpBcast.LocalIpAddr := FMonSrvIP;
            FMonIpBcast.LocalIpPort := '0';
            FMonIpBcast.LogProtocol := logprotUdpClient;
            FMonIpBcast.LogTitle := IcsAppMonName + ' UDP Client';
            FMonIpBcast.AddCRLF := True;
            if NOT FMonIpBcast.StartLogging then
                Exit;
            AddLogLine ('Started Monitor Broadcast to Port ' + FMonBcastPort) ;
        end;
        if (FMonSrvIP <> IcsRegGetHlm(IcsAppMonRegKey, IcsAppMonRegAddr)) or
                                    (FMonSrvPort <> IcsRegGetHlm(IcsAppMonRegKey, IcsAppMonRegPort)) then begin
            if FIsAdmin then begin
                if NOT IcsRegPutHlm(IcsAppMonRegKey, IcsAppMonRegAddr, FMonSrvIP) then
                    AddLogLine ('!! Failed to Update HLM Registry: ' + IcsAppMonRegKey) ;
                IcsRegPutHlm(IcsAppMonRegKey, IcsAppMonRegPort, FMonSrvPort);
                IcsRegPutHlm(IcsAppMonRegKey, IcsAppMonRegDT, RFC3339_DateToStr(Now));  // ISO
            end
            else
                AddLogLine ('!! Program Needs Adminstrator Rights to Set Server IP Settings in Registry') ;
        end;

    // open firewall and add rule for our server - needs admin rights
       if FIsAdmin then begin
            try
                AddLogLine (Trim(IcsFireWallRulesEnum('')));  // list settings
                S := IcsFireWallRulesAdd(IcsAppMonTitle, 'ICS', IcsAppMonTitle, ParamStr(0), FirewallBoth);
                AddLogLine ('Windows Firewall Rule: ' + S);
            except
                AddLogLine ('Failed to Access Firewall - ' + IcsGetExceptMess (ExceptObject)) ;
            end;
       end;

    // triggers for timer events
        FTriggerBcast := Trigger64Immediate;
        FTriggerMinute := Trigger64Immediate;
        FMonTimer.Enabled := True;
        SendAdminEmail ('Started OK', IcsAppMonTitle + ' Started OK, Listing on:' + FMonSrvIP + ':' + FMonSrvPort);
        if Assigned(FChangeEvent) then
            FChangeEvent(Self);
      Result := True;
    except
        AddLogLine('Failed to Start Server Monitor: ' + IcsGetExceptMess (ExceptObject)) ;
        SendAdminEmail ('Failed to Start', IcsAppMonTitle + ' Failed to Start');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonSrv.MonStop;
begin
    if NOT FStoppingFlag then begin
        FStoppingFlag := True;
        FMonTimer.Enabled := False;
        AddLogLine ('Stopping Client Monitoring and Servers');
      // clear date in registry
        if FIsAdmin then
            IcsRegPutHlm(IcsAppMonRegKey, IcsAppMonRegDT, '');
        if Assigned(FChangeEvent) then
            FChangeEvent(Self);
        FMonIpBcast.StopLogging;
        FMonIpServer.StopLogging;
        IcsSCMClose;
        AddLogLine ('Stopped Client Monitoring and Servers OK');
        SendAdminEmail ('Stopped OK', IcsAppMonTitle + ' Stopped Client Monitornng and Servers OK');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsAppMonSrv.GetListenStates: String;
begin
    Result := FMonIpServer.ListenStates;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// build HTML applications status table to be inserted into web page using Websocket
const
    TotWebHeader = 10;
    WebHeaders: array [1..TotWebHeader] of PChar =
         ('Server', 'App Title', 'State', 'Last Active', 'Monitor Start/Stop', 'Mode', 'Service Name', 'Status' ,'Program File', 'CliID');

function TIcsAppMonSrv.GetStatusWeb: String;
var
    I: Integer;
begin
    Result := '';
    if FMaxSuppApps = 0 then begin
        Result := sTagPBolds + 'No Applications Yet Monitored' + sTagPe + IcsCRLF;
        Exit;
    end;
    UpdateDispInfo;
    Result := sTableData + sTagTRs;
    for I := 1 to TotWebHeader do
        Result := Result + sTagTDs + WebHeaders[I] + sTagTDe;  // headers
    Result := Result + sTagTRe;
    for I := 0 to FMaxSuppApps - 1 do begin
        with FMonSuppApp[I] do begin
            if (AppSuppState = AppStateNone) then  // record never used
                Continue;
            Result := Result + sTagTRsc + AppColorDisp + sTagTRse +
                               sTagTDs + AppCompName + sTagTDe +
                               sTagTDs + TextToHtmlText(AppClientTitle) + sTagTDe +
                               sTagTDs + AppStateInfoDisp + sTagTDe +
                               sTagTDs + AppLastOKDisp + sTagTDe +
                               sTagTDs + AppStartStopDisp + sTagTDe +
                               sTagTDs + AMModeCmds[AppMonMode] + sTagTDe +
                               sTagTDs + TextToHtmlText(AppServiceName) + sTagTDe +
                               sTagTDs + TextToHtmlText(AppStatus) + sTagTDe +
                               sTagTDs + ExtractFileName(AppExeFile) + sTagTDe +
                               sTagTDs + IntToStr(AppClientID) + sTagTDe +
                               sTagTRe;
        end;
    end;
    Result := Result + sTableEnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// update display fields for web pages and webclient display
procedure TIcsAppMonSrv.UpdateDispInfo;
var
    I: Integer;
begin
    for I := 0 to FMaxSuppApps - 1 do begin
        with FMonSuppApp[I] do begin
            AppLastOKDisp := '';
            AppStateInfoDisp := '';
            AppStartStopDisp := '';
            AppColorDisp := sColorBYell; // not used, in theory
            if (AppSuppState = AppStateNone) then  // record never used
                Continue;
            AppStateInfoDisp := AMAppStateNames[AppSuppState];
            AppStartStopDisp := 'Started ' + IcsDateTimeToAStr(AppStarted);
            case AppSuppState of
                AppStateOK: begin
                    AppColorDisp := sColorBGreen;
                    AppLastOKDisp := IcsDateTimeToAStr(AppStarted + ((AppLastPingTick - AppLastStartTick) / 1000 / SecsPerDay));
                end;
                AppStateStopped: begin
                    AppColorDisp := sColorPRed;
                    AppStartStopDisp := 'Stopped ' + IcsDateTimeToAStr(AppStopped);
                    if AppRestartMode > RestModeNone then
                        AppStateInfoDisp := AppStateInfoDisp + ', ' + AMRestModeNames[AppRestartMode];
                end;
            else begin
                    AppColorDisp := sColorPYell;
                    if (AppStopped > 10) and (AppStopped > AppStarted) then
                        AppStartStopDisp := 'Stopped ' + IcsDateTimeToAStr(AppStopped);
                    if AppRestartMode > RestModeNone then
                        AppStateInfoDisp := AppStateInfoDisp + ', ' + AMRestModeNames[AppRestartMode];
                end;
            end;

        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// build Json block status record
function TIcsAppMonSrv.GetStatusJson: String;
var
    I, Tot: Integer;
    JsonArray:  AnsiString;  // UTF8
begin
    Result := '';
    FJsonResult.Clear;
    FJsonResult.AddItemDT('timestamp', Now);
    FJsonResult.AddItem('servername', IcsGetCompName);
    FJsonResult.AddItemA('serverinfo', SrvInfoJson.GetParameters, True);
    if FMaxSuppApps = 0 then begin
        FJsonResult.AddItem('success', False);
        FJsonResult.AddItem('reccount', 0);
        FJsonResult.AddItem('errdesc',  'No Applications Yet Monitored');
        Result := String(FJsonResult.GetParameters);  // UTF8
        Exit;
    end;
    JsonArray := '';
    Tot := 0;
    UpdateDispInfo;
    for I := 0 to FMaxSuppApps - 1 do begin
        with FMonSuppApp[I] do begin
            if (AppSuppState = AppStateNone) then  // record never used
                Continue;
            FJsonRecord.Clear;
            FJsonRecord.AddItem('compname', AppCompName);
            FJsonRecord.AddItem('clienttitle', AppClientTitle);
            FJsonRecord.AddItem('stateinfodisp', AppStateInfoDisp);
            FJsonRecord.AddItem('suppstateval', Ord(AppSuppState));
            FJsonRecord.AddItem('lastokdisp', AppLastOKDisp);
            FJsonRecord.AddItem('startstopdisp', AppStartStopDisp);
            FJsonRecord.AddItem('monmodedisp', AMModeCmds[AppMonMode]);
            FJsonRecord.AddItem('monmodeval', Ord(AppMonMode));
            FJsonRecord.AddItem('servicename', AppServiceName);
            FJsonRecord.AddItem('status', AppStatus);
            FJsonRecord.AddItem('exefile', AppExeFile);
            FJsonRecord.AddItem('clientid', AppClientID);
            FJsonRecord.AddItemDT('startedDT', AppStarted);
            FJsonRecord.AddItemDT('stoppedDT', AppStopped);
            FJsonRecord.AddItem('restartmode', Ord(AppRestartMode));
            FJsonRecord.AddItem('stopstate', Ord(AppStopState));
            FJsonRecord.AddItem('startstate', Ord(AppStartState));
            FJsonRecord.AddItem('restartinfo', AppRestartInfo);
            FJsonRecord.AddItem('general', AppGeneral);
            FJsonRecord.AddItem('colordisp', AppColorDisp);
            Tot := Tot + 1;
            if Tot > 1 then
                JsonArray := JsonArray + ',';
            JsonArray := JsonArray + FJsonRecord.GetParameters;  // UTF8
        end;
    end;
    JsonArray := '[' + JsonArray + ']';
    FJsonResult.AddItem('success', True);
    FJsonResult.AddItem('reccount', Tot);
    FJsonResult.AddItem('errdesc',  '');
    FJsonResult.AddItemA('records', JsonArray, True); // raw json
    Result := String(FJsonResult.GetParameters);  // UTF8
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// tell other applications we are running, always local broadcast message to this PC, maybe remote UDP broadcast
procedure TIcsAppMonSrv.SendBroadcast;
var
    Packet: String;
    Mywp: WPARAM;
    Mylp: LPARAM;
    Success: Boolean;
begin
  // broadcast server IP to other applications on this desktop
    try
        Mywp := WPARAM(WSocketStrToIPv4(FMonSrvIP, Success));  // returns integer!
        Mylp := atoi(FMonSrvPort);
        if Mylp > 0 then begin
            PostMessage(HWND_BROADCAST, FWinMess_Info, Mywp, Mylp);
            if FLogPackets then
                AddLogLine ('Broadcast Server Info Message');
        end;
    except
        AddLogLine ('Post Message Exception');
    end;

 // UDP broadcast to other computers
    try
        if FLANWide and (FMonIpBcast.LogActive) then begin
            Packet := IcsAppMonName + ProtDelim +     // BcastFieldServMon = 0
                      ProtCmdHello + ProtDelim +      // BcastFieldCommand = 1
                      IcsAppMonVerNr + ProtDelim +    // BcastFieldVersion = 2
                      IcsGetCompName + ProtDelim +    // BcastFieldSrvName = 3
                      FMonSrvIP + ProtDelim +          // BcastFieldSrvIP = 4
                      FMonSrvPort + ProtDelim;         // BcastFieldSrvPort = 5
            FMonIpBcast.SendLogLine(Packet);
            if FLogPackets then
                AddLogLine ('UDP Broadcast: ' + Packet);
        end;
    except
        AddLogLine ('Exception Broadcast: ' + IcsGetExceptMess (ExceptObject)) ;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// is Windows service or program running (by Process ID)
// paused Windows service is considered running
function TIcsAppMonSrv.IsAppRunning(AppNr: Integer): Boolean;
{var
    ServResp: String;
    ServState: Integer;  }
begin
    Result := False;
    if AppNr >= FMaxSuppApps then
        Exit;
    try
        with FMonSuppApp[AppNr] do begin
            if (AppServiceName <> '') and (AppServiceName <> ServiceNone) then begin

            // service manager may say stopping or stopped, but exe has not died!!!
       {         if NOT IcsCrtlService(AppServiceName, servfCheck, 0, ServState, ServResp) then begin  // no wait
                    AddLogLine (AppServiceName + ' : ' + ServResp);
                    Exit;
                end;
            //    AddLogLine (AppServiceName + ' : ' + ServResp);    // TEMP
                Result := (ServState in [SERVICE_START_PENDING, SERVICE_CONTINUE_PENDING, SERVICE_RUNNING,
                                                       SERVICE_PAUSED, SERVICE_PAUSE_PENDING, SERVICE_PAUSE_CONTINUE]);

             // check same service is still running, it might have restarted!
                if Result and }

                if (AppWinPID <> 0) then
                    Result := IcsCheckPID (AppWinPID);
            end
            else begin
                if AppWinPID <> 0 then
                    Result := IcsCheckPID (AppWinPID);
            end;
        end;
    except
        AddLogLine ('Exception IsAppRunning: ' + IcsGetExceptMess (ExceptObject)) ;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// stop or closing a Windows service or program, always exit after changing State
procedure TIcsAppMonSrv.AppStopOne(AppNr: Integer);
var
    ServResp: String;
    ServState: Integer;
    StoppedSecs, exitcode: Integer;
    Running: Boolean;
//    ProcInfo: TProcessInformation;
begin
    if FStoppingFlag then
       Exit;
    if AppNr >= FMaxSuppApps then
        Exit;
    Running := IsAppRunning(AppNr);
    exitcode := 0;
    with FMonSuppApp[AppNr] do begin
    try
        StoppedSecs := IcsElapsedSecs64 (AppStoppingTick);
        if AppExeFile = '' then begin
            AddLogLine (AppClientTitle + ': Stop Failed, No EXE File');
            AppStartState := StartStateNone;
            Exit;
        end;

     // service needs to be stopped, program confirmed as not running, then service started again
     // beware, this program needs admin rights to start and stop Windows services, not under the debugger
        if FIsAdmin and (AppServiceName <> '') and (AppServiceName <> ServiceNone) then begin

        // stop windows service, if running
            if AppStopState = StopStateStopProg then begin
                AppStopState := StopStateStopWait;
                if Running then begin
                    AddLogLine(AppClientTitle + ': Stopping Windows Service - ' +  AppServiceName);
             // no wait, keep checking if service stops, don't want to block this app
                    if NOT IcsCrtlService (AppServiceName, servfStop, 0, ServState, ServResp) then begin
                        AddLogLine (AppClientTitle + ': ' + ServResp);
                        AppStopState := StopStateNone;
                        Exit;
                    end;
                    if NOT (ServState in [SERVICE_STOPPED, SERVICE_STOP_PENDING]) then
                        AddLogLine (AppServiceName + ': WARNING !!, Unexpected Response - ' + ServResp)
                    else
                        AddLogLine (AppServiceName + ': ' + ServResp);
                    AddLogLine (AppClientTitle + ': Waiting for Windows Service to Stop');
                    Exit;
                end;
            end;

        // wait for Windows service to stop
        // if does not stop, crash program by process ID or WM_QUIT message
            if AppStopState = StopStateStopWait then begin
                if NOT Running then begin
                    AppStopState := StopStateStopped;
                    AppStoppingTick := IcsGetTickCount64;  // reset stop timer
                    AddLogLine (AppClientTitle + ': Windows Service Has Stopped');
                    AppWinPID := 0;
                    AppWinHnd := 0;
                    Exit;
                end
                else begin
                    if StoppedSecs > FStopWaitSecs then begin
                        AppStoppingTick := IcsGetTickCount64;  // reset stop timer
                        AppStopState := StopStateCrashProg;
                        if AppWinPID <> 0 then begin
                            AddLogLine (AppClientTitle + ': Service Failed to Stop, Terminating Program by Process ID');
                            if NOT IcsTermPID (AppWinPID, exitcode) then
                                AddLogLine (AppClientTitle + ': Failed to Terminate Program - ' + IcsFormatLastError);
                        end
                        else if AppWinHnd <> 0 then begin
                            AddLogLine (AppClientTitle + ': Service Failed to Stop, Terminating Program by QUIT Message');
                            PostMessage (AppWinHnd, WM_QUIT, 0, 0) ;
                        end
                        else begin
                            AddLogLine (AppClientTitle + ': Failed to Stop Program, No Handles');
                            AppStopState := StopStateNone;
                            Exit;
                        end;
                        AppStopState := StopStateCrashWait;
                        Exit;
                    end;
                end;
            end;

        // still waiting for Windows service to stop
            if AppStopState = StopStateCrashWait then begin
                if NOT Running then begin
                    AppStopState := StopStateStopped;
                    AppStoppingTick := IcsGetTickCount64;  // reset stop timer
                    AddLogLine (AppClientTitle + ': Windows Service Has Stopped');
                    AppWinPID := 0;
                    AppWinHnd := 0;
                    Exit;
                end
                else begin
                    if StoppedSecs > FStopWaitSecs then begin   // give up
                        AddLogLine (AppClientTitle + ': Failed to Stop Windows Service');
                        AppStopState := StopStateNone;
                        Exit;
                    end;
                end;
            end;
        end

    // normal application should be cleanly closed, or crashed
    // beware a Windows service can not easily run a program in the desktop window
        else begin

         // stop program cleanly by WM_CLOSE message to main window, or by process ID if no handle
            if AppStopState = StopStateStopProg then begin
                if Running then begin
                    AddLogLine(AppClientTitle + ': Stopping Windows Program - ' + AppExeFile);
                    if AppWinHnd <> 0 then begin
                        AddLogLine (AppClientTitle + ': Stopping Windows Program by CLOSE Message');
                        PostMessage (AppWinHnd, WM_CLOSE, 0, 0) ;
                    end
                    else if AppWinPID <> 0 then begin
                        AddLogLine (AppServiceName + ' : Stopping Program by Windows Process ID');
                        if NOT IcsTermPID (AppWinPID, exitcode) then
                            AddLogLine (AppClientTitle + ': Failed to Terminate Windows Program - ' + IcsFormatLastError);
                    end
                    else begin
                        AddLogLine (AppClientTitle + ': Failed to Stop Windows Program, No Handles');
                        AppStopState := StopStateNone;
                        Exit;
                    end;
                    AddLogLine (AppClientTitle + ': Waiting for Windows Program to Stop');
                end;
                AppStopState := StopStateStopWait;
                Exit;
            end;

        // wait for procgram to stop
        // if does not stop, crash program by process ID or WM_QUIT message
            if AppStopState = StopStateStopWait then begin
                if NOT Running then begin
                    AppStopState := StopStateStopped;
                    AppStoppingTick := IcsGetTickCount64;  // reset stop timer
                    AddLogLine (AppClientTitle + ': Windows Program Has Stopped');
                    AppWinPID := 0;
                    AppWinHnd := 0;
                    Exit;
                end
                else begin
                    if StoppedSecs > FStopWaitSecs then begin
                        AppStoppingTick := IcsGetTickCount64;  // reset stop timer
                        AppStopState := StopStateCrashProg;
                        if AppWinPID <> 0 then begin
                            AddLogLine (AppClientTitle + ': Windows Program Failed to Stop, Terminating Program by Process ID');
                            if NOT IcsTermPID (AppWinPID, exitcode) then
                                AddLogLine (AppClientTitle + ': Failed to Terminate Windows Program - ' + IcsFormatLastError);
                        end
                        else if AppWinHnd <> 0 then begin
                            AddLogLine (AppClientTitle + ': Windows Program Failed to Stop, Terminating Program by QUIT Message');
                            PostMessage (AppWinHnd, WM_QUIT, 0, 0) ;
                        end;
                        AppStopState := StopStateCrashWait;
                        Exit;
                    end;
                end;
            end;

        // still waiting for program to stop
            if AppStopState = StopStateCrashWait then begin
                if NOT Running then begin
                    AppStopState := StopStateStopped;
                    AppStoppingTick := IcsGetTickCount64;  // reset stop timer
                    AddLogLine (AppClientTitle + ': Windows Program Has Stopped');
                end
                else begin
                    if StoppedSecs > FStopWaitSecs then begin   // give up
                        AddLogLine (AppClientTitle + ': Failed to Stop Windows Program');
                        Exit;
                    end;
                end;
            end;
        end;
        except
            AddLogLine ('Exception AppStopOne: ' + IcsGetExceptMess (ExceptObject)) ;
            AppStopState := StopStateNone;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// start a Windows service or program, always exit after changing State
procedure TIcsAppMonSrv.AppStartOne(AppNr: Integer);
var
    ServResp: String;
    ServState: Integer;
    StoppedSecs: Integer;
    Running: Boolean;
    ProcInfo: TProcessInformation;
begin
    if FStoppingFlag then
       Exit;
    if AppNr >= FMaxSuppApps then
        Exit;
    Running := IsAppRunning(AppNr);
    with FMonSuppApp[AppNr] do begin
    try
        StoppedSecs := IcsElapsedSecs64 (AppStoppingTick);
        if AppExeFile = '' then begin
            AddLogLine (AppClientTitle + ': Hello Failed, No EXE Filer');
            AppStartState := StartStateNone;
            Exit;
        end;

     // service needs to be stopped, program confirmed as not running, then service started again
     // beware, this program needs admin rights to start and stop Windows services, not under the debugger
        if FIsAdmin and (AppServiceName <> '') and (AppServiceName <> ServiceNone) then begin

        // maybe wait 15s before starting Windows service, it may not have closed immediately, and may start itself
            if AppStartState = StartStateStartWait then begin
                if Running then begin
                    AppEmailLines := AppEmailLines + IcsDateTimeToAStr(Now) + IcsSpace + AppClientTitle +
                            ': Windows Service Already Restarted - ' + AppServiceName + IcsCRLF +
                            'Reason: ' + AppRestartInfo + IcsCRLF + IcsCRLF;
                    AppEmailTick := IcsGetTickCount64;  // sends email in 30 seconds
                    AppStartState := StartStateProgStarted;
                    Exit;
                end;
                if StoppedSecs > FStartWaitSecs then begin
                    AppStartState := StartStateProgStarting;
                    AppStoppingTick := IcsGetTickCount64;  // reset stop timer
                end;
            end;

        // now start Windows service again
            if AppStartState = StartStateProgStarting then begin
                AddLogLine (AppClientTitle + ': Starting Windows Service - ' +  AppServiceName);
                AppEmailLines := AppEmailLines + IcsDateTimeToAStr(Now) + IcsSpace + AppClientTitle +
                            ': Starting Windows Service - ' + AppServiceName + IcsCRLF +
                            'Reason: ' + AppRestartInfo + IcsCRLF + IcsCRLF;
             // no wait or error checking, keep checking if service starts, don't want to block this app
                if NOT IcsCrtlService(AppServiceName, servfStart, 0, ServState, ServResp) then begin
                    AddLogLine (AppClientTitle + ': ' + ServResp);   // failed to start, probably not stopped yet
                    if GetLastError = 1056 then begin  // instance already running
                        AppStartState := StartStateNotStopped;
                        AppStoppingTick := IcsGetTickCount64;  // reset stop timer
                    end
                    else
                     // V9.5 failed to start, may be no memory, try again
                        AppStartState := StartStateStartWait;
                     //   AppStartState := StartStateNone;
                    Exit;
                end;
                if NOT (ServState in [SERVICE_START_PENDING, SERVICE_RUNNING]) then
                    AddLogLine (AppServiceName + ': WARNING !!, Unexpected Response - ' + ServResp)
                else
                    AddLogLine (AppServiceName + ' : ' + ServResp);
                AppStartState := StartStateProgStarted;
                AppStoppingTick := IcsGetTickCount64;  // reset stop timer
                Exit;
            end;

        // wait and check Windows service is actually running
        // BEWARE, may never reach here if client socket reconnects and starts new monitoring session, resettings states
            if AppStartState = StartStateProgStarted then begin
                if Running then begin
                    AddLogLine (AppClientTitle + ': Started Windows Service OK - ' +  AppServiceName);
                    AppEmailLines := AppEmailLines  + IcsDateTimeToAStr(Now) + IcsSpace + AppClientTitle +
                                            ': Restarted Windows Service OK - ' + AppServiceName + IcsCRLF;
                    AppEmailTick := IcsGetTickCount64;  // sends email in 30 seconds
                    AppStartState := StartStateProgStarted;
                    Exit;
                end
                else begin
                    if StoppedSecs > FStopWaitSecs then begin   // give up
                        AddLogLine (AppClientTitle + ': Unable to Confirm Service Started');
                        AppEmailLines := AppEmailLines  + IcsDateTimeToAStr(Now) + IcsSpace + AppClientTitle +
                                ': Failed to Restart Windows Service - ' + AppServiceName + IcsCRLF +
                                'Reason: ' + AppRestartInfo + IcsCRLF + IcsCRLF;
                        AppEmailTick := IcsGetTickCount64;  // sends email in 30 seconds
                        AppStartState := StartStateNone;
                        Exit;
                    end;
                end;
            end;

        // sanity test, already running, nothing to do
            if Running then begin
                AddLogLine(AppClientTitle + ': Windows Service Aleady Running - ' +  AppServiceName);
                AppStartState := StartStateNone;
                Exit;
            end;
        end

    // normal application should be cleanly closed, or crashed, then run again
        else begin

         // stop program cleanly by WM_CLOSE message to main window, or by process ID if no handle
            if (IcsGetExePID(AppExeFile) <> 0) then begin   // check process lists to see if our EXE is runnng, don't have handle or PID
                AddLogLine(AppClientTitle + ': Windows Program Aleady Running - ' +  AppExeFile);
                AppStartState := StartStateNone;
                Exit;
            end;

        // wait 15s before starting program, it may not have closed immediately, and may restart itself
            if AppStartState = StartStateStartWait then begin
                if (IcsGetExePID(AppExeFile) <> 0) then begin   // check process lists to see if our EXE is runnng, don't have handle or PID
                    AppEmailLines := AppEmailLines  + IcsDateTimeToAStr(Now) + IcsSpace + AppClientTitle +
                             ': Windows Program Already Restarted - ' + AppExeFile + IcsCRLF +
                             'Reason: ' + AppRestartInfo + IcsCRLF + IcsCRLF;
                    AppEmailTick := IcsGetTickCount64;  // sends email in 30 seconds
                    AppStartState := StartStateProgStarted;
                    Exit;
                end;
                if StoppedSecs > FStartWaitSecs then begin
                    AppStartState := StartStateProgStarting;
                    AppStoppingTick := IcsGetTickCount64;  // reset stop timer
                end;
            end;

        // now start program again
            if AppStartState = StartStateProgStarting then begin
                AddLogLine (AppClientTitle + ': Starting Windows Program - ' +  AppExeFile);
                ProcInfo := IcsStartExe (AppExeFile, '', SW_SHOWNORMAL);
                if ProcInfo.hProcess <> 0 then begin
                    AppWinPID := ProcInfo.dwProcessId;
                    AddLogLine (AppClientTitle + ': Started Windows Program OK - ' +  AppExeFile + ', PID=' + IntToStr(AppWinPID));
                    AppEmailLines := AppEmailLines + IcsDateTimeToAStr(Now) + IcsSpace + AppClientTitle +
                            ': Restarted Windows Program OK - ' + AppExeFile + IcsCRLF +
                            'Reason: ' + AppRestartInfo + IcsCRLF + IcsCRLF;
                    AppEmailTick := IcsGetTickCount64;  // sends email in 30 seconds
                    AppStartState := StartStateProgStarted;
                    Exit;
                end
                else begin
                    AddLogLine (AppClientTitle + ': Failed to Run Windows Program - ' +  AppExeFile);
                    AppEmailLines := AppEmailLines + IcsDateTimeToAStr(Now) + IcsSpace + AppClientTitle +
                            ': Failed to Started Windows Program - ' + AppExeFile + IcsCRLF +
                            'Reason: ' + AppRestartInfo + IcsCRLF + IcsCRLF;
                    AppEmailTick := IcsGetTickCount64;  // sends email in 30 seconds
                    AppStartState := StartStateNone;
                    Exit;
                end;
            end
        end;
        except
            AddLogLine ('Exception AppStartOne: ' + IcsGetExceptMess (ExceptObject)) ;
            AppStartState := StartStateNone;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonSrv.MonTimerTimer(Sender: TObject);
var
    Packet: String;
    AppNr, DeadSecs: Integer;
begin
    FMonTimer.Enabled := False;
    try
        try
            if IcsTestTrgTick64(FTriggerBcast) then begin
                FTriggerBcast := IcsGetTrgSecs64 (FBCastSec) ;
                SendBroadcast;
            end;

            if IcsTestTrgTick64(FTriggerMinute) then begin
                FTriggerMinute := IcsGetTrgSecs64 (60) ;
              // update date in registry
                if FIsAdmin then
                    IcsRegPutHlm(IcsAppMonRegKey, IcsAppMonRegDT, RFC3339_DateToStr(Now));  // ISO date
            end;

        // check all applications once a second
            if FTotSuppApps <> 0 then begin
                for AppNr := 0 to FMaxSuppApps - 1 do begin
                    with FMonSuppApp[AppNr] do begin

                     // monitoring has stopped, send pending emails, application might connect again if it restarts
                        if (AppSuppState in [AppStateNone, AppStateStopped]) then begin
                            if AppEmailLines <> '' then begin
                                AddLogLine(AppClientTitle + ': Queuing Email');
                                SendAdminEmail (AppClientTitle, AppEmailLines);
                                AppEmailLines := '';
                            end;
                            Continue;
                        end;

                     // stopping monitoring, close server connection if still open
                     // maybe restart application
                        if AppSuppState = AppStateStopping then begin
                            if (AppClientID > 0) and (FMonIpServer.States[AppClientId] = logstateOK) then begin
                               AddLogLine(AppClientTitle + ': Disconnecting Client');
                               FMonIpServer.CloseSrvCli(AppClientId);
                               AppClientID := 0; // no longer available
                            end;
                            AddLogLine(AppClientTitle + ': Stopped Monitoring, No Client Restart');
                            if AppEmailLines <> '' then begin
                                AddLogLine(AppClientTitle + ': Queuing Email');
                                SendAdminEmail (AppClientTitle, AppEmailLines);
                            end;
                            AppSuppState := AppStateStopped;
                            AppLastStartTick := 0;
                            AppStopped := Now;
                            FAppsChangedFlag := True;
                            Continue;
                        end;

                     // stop an application, note this state remains until application finally stops, couple of minutes maybe
                        if AppSuppState = AppStateProgStop then begin
                            if AppRestartMode = RestModeNot then begin    // sanity check, must not restart
                                AppSuppState := AppStateStopping;
                                Continue;
                            end;

                        // first time here, StopState controls what happens next
                            if AppStopState = StopStateNone then begin
                                AddLogLine(AppClientTitle + ': Stopping Client');
                                AppStopState := StopStateStopProg;
                                AppStoppingTick := IcsGetTickCount64;
                            end;

                        // handle stop events, called once a second
                            AppStopOne(AppNr);

                          // if application has been stopped, see if restarting it
                            if (AppStopState in [StopStateNone, StopStateStopped]) then begin
                                if (AppRestartMode in [RestModeAuto, RestModeRequest, RestModeItelf]) then begin
                                    AddLogLine(AppClientTitle + ': Restarting Client After ' + IntToStr(FStartWaitSecs) + ' Seconds Wait');
                                    AppSuppState := AppStateProgStart;
                                    AppStartState := StartStateStartWait;  //  wait 15 secs for prog to stop
                                end
                                else
                                    AppSuppState := AppStateStopped;
                                AppStopped := Now;
                                FAppsChangedFlag := True;
                            end;
                            Continue;
                          end;

                     // start application, note this state remains until it starts
                        if AppSuppState = AppStateProgStart then begin

                        // first time here, StartState controls what happens next
                            if AppStartState = StartStateNone then begin
                                AddLogLine(AppClientTitle + ': Starting Client');
                                AppStartState := StartStateProgStarting;
                                AppStoppingTick := IcsGetTickCount64;
                                FAppsChangedFlag := True;
                            end;

                        // handle start events, called once a second, but stop monitoring once it's running
                        // newly started application will send HELLO and we restart monitoring with new PID
                            AppStartOne(AppNr);
                            if AppStartState = StartStateNotStopped then begin
                                AddLogLine(AppClientTitle + ': Failed to Start Application, Trying to Stop Again');
                                AppSuppState := AppStateProgStop;  // try and stop again
                                AppStopState := StopStateNone;
                            end
                            else if (AppStartState in [StartStateNone, StartStateProgStarted]) then begin
                                AppSuppState := AppStateStopped;   // old application has stopped, wating for new app
                                AppStopped := Now;
                                FAppsChangedFlag := True;
                            end;
                            Continue;
                        end;

                     // lost monitoring connection with client, can not send any more commands
                     // state should have been changed in event, this is fail safe }
                        if (AppSuppState = AppStateOK) and (AppClientID = 0) and (AppStopState = StopStateNone) then begin
                            AppRestartInfo := 'Monitor Socket Closed';
                            AddLogLine(AppClientTitle + ': Monitor Socket Closed, Restarting');
                            AppSuppState := AppStateProgStop;
                            FAppsChangedFlag := True;
                            Continue;
                        end;

                     // acknowledge packet received with PONG back to client
                        if (AppSuppState = AppStateOK) and (AppLastPingTick > AppLastPongTick) then begin
                            AppLastPongTick := IcsGetTickCount64;
                            Packet := IcsAppMonName + ProtDelim +
                                      ProtCmdPong + ProtDelim +
                                      AppClientTitle + ProtDelim;
                            if FLogPackets then
                                AddLogLine('Sending: ' + Packet);
                            FMonIpServer.SendLogLine(Packet, AppClientID);
                            Continue;
                        end;

                    // exceeded our maximum wait time with no PINGs, try and wake up client
                        if AppSuppState = AppStateOK then begin
                            DeadSecs := IcsElapsedSecs64 (AppLastPingTick);
                            if DeadSecs > AppDeadSecs then begin
                                AppSuppState := AppStateQuiet;
                                AppLastPongTick := IcsGetTickCount64;
                                Packet := IcsAppMonName + ProtDelim +
                                          ProtCmdWake + ProtDelim +
                                          AppClientTitle + ProtDelim;
                                AddLogLine('Client gone QUIET, Sending: ' + Packet);
                                FMonIpServer.SendLogLine(Packet, AppClientID);
                                FAppsChangedFlag := True;
                                Continue;
                            end;
                        end;

                    // wait 30 seconds for response to WAKE, otherwsie stop and restart application
                        if AppSuppState = AppStateQuiet then begin
                            DeadSecs := IcsElapsedSecs64 (AppLastPongTick);
                            if DeadSecs > AppDeadSecs then begin
                                AddLogLine(AppClientTitle + ': Client Still Quiet After ' + IntToStr(DeadSecs) + ', Restarting');
                                AppSuppState := AppStateProgStop;
                                AppRestartInfo := 'Client Stopped Sending Pings';
                                FAppsChangedFlag := True;
                                Continue;
                            end;
                        end;

                     // if application has stopped on it's own, try and restart it
                     // should have lost monitor socket already
                        if AppSuppState = AppStateOK then begin
                            if NOT IsAppRunning(AppNr) then begin
                                AddLogLine(AppClientTitle + ': Program No Longer Running, Restarting');
                                AppRestartInfo := 'Program Not Running';
                                AppSuppState := AppStateProgStop;
                                FAppsChangedFlag := True;
                                Continue;
                            end;
                        end;

                     // see if pending email, wait 30 seconds for more lines to be added
                        if AppEmailTick <> Trigger64Disabled then begin
                            if IcsElapsedSecs64 (AppEmailTick) > 30 then begin
                                AppEmailTick := Trigger64Disabled;
                                if AppEmailLines <> '' then begin
                                    AddLogLine(AppClientTitle + ': Queuing Email');
                                    SendAdminEmail (AppClientTitle, AppEmailLines);
                                end;
                                AppEmailLines := '';
                            end;
                        end;

                    end;
                end;
            end;

        // tell application table has been updated
            if FAppsChangedFlag then begin
                FAppsChangedFlag := False;
                if Assigned(FChangeEvent) then
                    FChangeEvent(Self);
            end;
        except
            AddLogLine ('Exception Main Timer: ' + IcsGetExceptMess (ExceptObject)) ;
        end;
    finally
        FMonTimer.Enabled := True;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(* hardware and application events, not used yet
procedure TIcsAppMonSrv.TriggerMessageEvent (var Msg: TMsg; var Handled: Boolean) ;
begin

// a client is asking us to broadcast our HELLO message immediately
    if (Msg.message = FWinMess_Query) and (Msg.wParam = IcsAppMonMessParam) then begin
    //    AddLogLine ('WinMess_QueryInfo: ' + IntToStr(Msg.wParam)) ;
        if FTriggerBcast <> Trigger64Immediate then begin  // try and stop multiple loggings
            AddLogLine ('WinMess_QueryInfo: ' + IntToStr(Msg.wParam)) ;
            FTriggerBcast := Trigger64Immediate;
        end;
        Handled := true ;
    end;

// see if system is closing down
    if (Msg.message = WM_QUERYENDSESSION) then begin
        AddLogLine ('!! Windows Wants to Close Down, QuerySessionEnd') ;
    end;
    if (Msg.message = WM_ENDSESSION) then begin
        AddLogLine ('!! Windows Is Closing Down, Stop Monitoring SessionEnd') ;
        MonStop;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonSrv.HardwareEventsPowerEvent(Sender: TObject; PowerEvent: Integer; const Desc: string);
begin
    AddLogLine ('System Power Event: ' + Desc + ', Event=' + IntToStr(PowerEvent));
//    if PowerEvent += PBT_ then

end;
*)


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonSrv.MonIpBcastLogProgEvent(Sender: TObject; Socnr: Integer; LogOption: TLogOption; const Msg: string);
begin
    AddLogLine ('Broadcast Event: ' + Msg) ;   // ignore it
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonSrv.MonIpBcastLogRecvEvent(Sender: TObject; Socnr: Integer; const Line: string);
begin
    AddLogLine ('Broadcast response from: ' + FMonIpBcast.Socket[SocNr].PeerAddr + ' - ' + Line);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonSrv.MonIpServerLogChangeEvent(Sender: TObject; Socnr: Integer; LogState: TStrmLogState);
var
    AppNr: Integer;
    Title: String;
begin
    AppNr := -1;
    Title := 'Unknown';
    if (SocNr > 0) then begin
        AppNr := FindSuppApp(SocNr);
        if (AppNr >= 0) and (LogState >= logstateOK) then
             Title := FMonSuppApp[AppNr].AppClientTitle;
        AddLogLine (Title + ': State ' + StrmLogStateNames[LogState] + ' CliId ' + IntToStr(SocNr)) ;
    end;
    if  (AppNr >= 0) and (LogState = logstateStopping) then begin
        with FMonSuppApp[AppNr] do begin
            if (AppRestartMode = RestModeNot) or FStoppingFlag then begin
                AddLogLine(AppClientTitle + ': Stopped Application Monitoring');
                AppEmailLines := AppEmailLines + IcsDateTimeToAStr(Now) + IcsSpace + AppClientTitle +
                                                ': Stopped Application Monitoring' + IcsCRLF + IcsCRLF;
                AppEmailTick := IcsGetTickCount64;  // sends email in 30 seconds
                AppSuppState := AppStateStopped;
                AppLastStartTick := 0;
                AppStopped := Now;
            end
            else begin
                if AppSuppState = AppStateOK then begin
                    AppSuppState := AppStateProgStop;
                    AppRestartInfo := 'Monitor Socket Closed Unexpectedly';
                    AppEmailLines := AppEmailLines + IcsDateTimeToAStr(Now) + IcsSpace + AppClientTitle +
                                            ': Monitor Socket Closed Unexpectedly, Restarting' + IcsCRLF + IcsCRLF;
                    AppEmailTick := IcsGetTickCount64;  // sends email in 30 seconds
                    AddLogLine(AppClientTitle + ': Monitor Socket Closed Unexpectedly, Restarting');
                    AppLastStartTick := 0;
                    AppClientID := 0; // long longer available
                end;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonSrv.MonIpServerLogProgEvent(Sender: TObject; Socnr: Integer; LogOption: TLogOption; const Msg: string);
var
    AppNr: Integer;
    Title: String;
begin
    Title := 'New Client';
    if (SocNr > 0) then begin
        AppNr := FindSuppApp(SocNr);
        if (AppNr >= 0) and (FMonSuppApp[AppNr].AppSuppState >= AppStateOK) then
             Title := FMonSuppApp[AppNr].AppClientTitle;
        AddLogLine(Title + ': ' + Msg + ' CliId ' + IntToStr(SocNr))
    end
    else
        AddLogLine (Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAppMonSrv.MonIpServerLogRecvEvent(Sender: TObject; Socnr: Integer; const Line: string);
var
    AppNr: Integer;
    CliMode: TAMMode;
    EmailBody, Command, Packet, CliTitle: String;

    function FindAMMode(const Lit: String): TAMMode;
    var
        Mode: TAMMode;
    begin
        Result := AMModeMonitor;
        for Mode := Low(TAMMode) to High(TAMMode) do begin
            if Lit = AMModeCmds[Mode] then begin
                Result := Mode;
                Exit;
            end;
        end;
    end;

    function FindAMRestart(const Lit: String): TAMRestartMode;
    var
        Mode: TAMRestartMode;
    begin
        Result := RestModeNone;
        for Mode := Low(TAMRestartMode) to High(TAMRestartMode) do begin
            if Lit = AMRestModeCmds[Mode] then begin
                Result := Mode;
                Exit;
            end;
        end;
    end;

    function FindOldApp: Integer;
    var
        I: Integer;
    begin
        Result := -1;
        if FTotSuppApps = 0 then
            Exit;
        for I := 0 to FMaxSuppApps - 1 do begin
            if (FMonSuppApp[I].AppClientTitle = CliTitle) and (FMonSuppApp[I].AppMonMode = CliMode) then begin
                Result := I;
                exit;
            end;
        end;
    end;

begin
    if FLogPackets then
        AddLogLine ('Server message from ClientId: ' + IntToStr(SocNr) + ' - ' + Line) ;
    FAppFields.Clear;
    EmailBody := '';
    FAppFields.DelimitedText := Line;
    try
        if (FAppFields.Count > CliCFieldLast) then begin

         // sanity checking
            if FAppFields[CliCFieldAppMon] = IcsAppMonName then begin
                CliTitle := FAppFields[CliCFieldAppTitle];

             // find AppNr for current session from SocNr
                AppNr := FindSuppApp(SocNr);
                if Appnr < 0 then begin

             // find AppNr for old session from AppTitle and Mode
                    CliMode := AMModeMonitor;
                    if (FAppFields.Count > CliHFieldDeadSecs) then    // assume HELLO
                        CliMode := FindAMMode(FAppFields[CliHFieldAppMode]);
                    Appnr := FindOldApp;
                    if Appnr < 0 then begin

             // app not connected before, new record
                        AppNr := AddSuppApp;
                        AddLogLine('Allocate New App Record ' + IntToStr(AppNr + 1));
                   end
                   else begin
                        AddLogLine('Using Old App Record ' + IntToStr(AppNr + 1));
                        with FMonSuppApp[AppNr] do begin
                            if (AppStartState > StartStateNone) then begin  // were we restarting application?
                                AddLogLine (AppClientTitle + ': Application Restarted OK');
                                AppEmailLines := AppEmailLines + IcsDateTimeToAStr(Now) + IcsSpace + AppClientTitle +
                                                            ': Application Restarted OK' + IcsCRLF + IcsCRLF;
                                AppEmailTick := IcsGetTickCount64;  // sends email in 30 seconds
                            end;
                        end;
                    end;
                    FMonSuppApp[AppNr].AppClientId := SocNr;
                    FAppsChangedFlag := True;
                end;
                if Appnr < 0 then begin
                    AddLogLine('Failed to Allocate New Record');
                    exit;
                end;

            // handle received commands from clients
               with FMonSuppApp[AppNr] do begin
                    Command := FAppFields[CliCFieldCommand];
                    EmailBody := IcsUnEscProtField(FAppFields[CliCFieldEmail]);
                    if EmailBody <> '' then begin
                        AppEmailLines := AppEmailLines + IcsDateTimeToAStr(Now) + ': ' + EmailBody + IcsCRLF + IcsCRLF;
                        AppEmailTick := IcsGetTickCount64;  // sends email in 30 seconds
                    end;

                // HELLO command, remote client is introducing itself
                    if (Command = ProtCmdHello) and (FAppFields.Count > CliHFieldLast)  then begin
                        try
                            FAppsChangedFlag := True;
                            AppClientId := SocNr;
                            AppClientTitle := CliTitle;
                            AppMonMode := CliMode;
                            AppRestartMode := RestModeNone;
                            AppStopState := StopStateNone;
                            AppStartState := StartStateNone;
                         // NonStop mode means we automatically restart unless told otherwise
                            if AppMonMode = AMModeNonStop then
                                AppRestartMode := RestModeAuto;
                            AppExeFile := Trim(FAppFields[CliHFieldExeFile]);
                            AppServiceName := Trim(FAppFields[CliHFieldServiceName]);
                            if AppServiceName = '' then
                               AppServiceName := ServiceNone;
                            AppWinHnd := atoi64(FAppFields[CliHFieldWinHnd]);
                            AppWinPID := atoi64(FAppFields[CliHFieldWinPID]);
                            AppCompName := Trim(FAppFields[CliHFieldCompName]);
                            AppLocalIP := Trim(FAppFields[CliHFieldLocalIP]);
                            AppDeadSecs := atoi(FAppFields[CliHFieldDeadSecs]);
                            AppGeneral := IcsUnEscProtField(FAppFields[CliHFieldAppGeneral]);
                            AppStatus := '';
                            if AppDeadSecs < 30 then
                                AppDeadSecs := FDeadSecs;
                            AppStarted := Now;
                            AppLastStartTick := IcsGetTickCount64;
                            AppSuppState := AppStateOK;
                            AppLastPingTick := IcsGetTickCount64;
                            if AppLocalIP = '' then
                            try
                                AppLocalIP := FMonIpServer.SrvClientSoc[SocNr].CPeerAddr;
                            except
                                AddLogLine('Failed to get CPeerAddr');
                            end;
                            AppEmailTitle := AppClientTitle + ' on ' + AppCompName;
                            if AppServiceName <> ServiceNone then
                                AppEmailTitle := AppEmailTitle + ', Service: ' + AppServiceName;

                        // see if unable to restart application
                            if AppMonMode = AMModeNonStop then begin
                                if (NOT FIsAdmin) and (AppServiceName <> ServiceNone) then begin
                                    AddLogLine(AppClientTitle + ': Need Admin Rights to Control Windows Services: ' + AppServiceName);
                                    Packet := IcsAppMonName + ProtDelim +
                                              ProtCmdDecline + ProtDelim +
                                              AppClientTitle + ProtDelim +
                                              ProtDelim;
                                    AddLogLine(AppClientTitle + ': Declined Monitoring, Sending: ' + Packet);
                                    FMonIpServer.SendLogLine(Packet, AppClientID);
                                    AppEmailLines := AppEmailLines + IcsDateTimeToAStr(Now) + IcsSpace + AppClientTitle +
                                                                      ': Declined Monitoring' + IcsCRLF + IcsCRLF;
                                    AppRestartMode := RestModeNot;
                                    AppSuppState := AppStateStopping;
                                    Exit;
                                end;
                                if FWinService and (AppServiceName = ServiceNone) then begin
                                    AddLogLine(AppClientTitle + ': Windows Services Can Not Run Desktop Programs');
                                    Packet := IcsAppMonName + ProtDelim +
                                              ProtCmdDecline + ProtDelim +
                                              AppClientTitle + ProtDelim +
                                              ProtDelim;
                                    AddLogLine(AppClientTitle + ': Declined Monitoring, Sending: ' + Packet);
                                    FMonIpServer.SendLogLine(Packet, AppClientID);
                                    AppEmailLines := AppEmailLines + IcsDateTimeToAStr(Now) + IcsSpace + AppClientTitle +
                                                                      ': Declined Monitoring' + IcsCRLF + IcsCRLF;
                                    AppRestartMode := RestModeNot;
                                    AppSuppState := AppStateStopping;
                                    Exit;
                                end;
                            end;
                            AddLogLine(AppClientTitle + ': Application Monitoring Started: ' + Copy(EmailBody, 1, 80));

                            if IsAppRunning(AppNr) then
                                AddLogLine(AppClientTitle + ': ' + AppExeFile + ' is Running OK')
                            else begin
                                if AppServiceName = ServiceNone then
                                   AddLogLine(AppClientTitle + ': ' + AppExeFile + ' Not Found Running!!! PID=' + IntToStr(AppWinPID));
                            end;

                        // email, may have message from app as well
                            AppEmailLines := AppEmailLines + IcsDateTimeToAStr(Now) + IcsSpace + AppClientTitle +
                                       ': Application Monitoring Started' + IcsCRLF + 'File : ' + AppExeFile + IcsCRLF + IcsCRLF;
                            AppEmailTick := IcsGetTickCount64;  // sends email in 30 seconds
                        except
                            AddLogLine (CliTitle + ': Exception HELLO Packet: ' + IcsGetExceptMess (ExceptObject)) ;
                            AppRestartMode := RestModeNot;
                            AppSuppState := AppStateStopping;
                        end;
                    end

                 // PING command, expected regularly from remote server
                    else if Command = ProtCmdPing then begin
                        try
                            if (AppLastStartTick = 0) or (AppExeFile = '') then begin
                                FAppsChangedFlag := True;
                                AppClientId := SocNr;
                                AddLogLine(AppClientTitle + ': Non-Registered Application, Closing');
                                AppRestartMode := RestModeNot;
                                AppSuppState := AppStateStopping;
                                Exit;
                            end;
                            if (FAppFields.Count > CliPFieldStatus) then
                                AppStatus := IcsUnEscProtField(FAppFields[CliPFieldStatus]);
                            if EmailBody <> '' then
                                AddLogLine(AppClientTitle + ': PING and Email Received from Client: ' +
                                                                                 Copy(EmailBody, 1, 80) + ' - ' + AppStatus)
                            else
                                AddLogLine(AppClientTitle + ': PING Packet Received from Client - ' + AppStatus);
                            AppLastPingTick := IcsGetTickCount64;
                            if AppSuppState <> AppStateOK then
                                FAppsChangedFlag := True;
                            AppSuppState := AppStateOK;
                        except
                            AddLogLine (CliTitle + ': Exception PING Packet: ' + IcsGetExceptMess (ExceptObject)) ;
                            AppRestartMode := RestModeNot;
                            AppSuppState := AppStateStopping;
                        end;
                    end

                 // STOP command, remote server wants to stop being monitored, it is probably stopping
                 // generally don't restart unless a RESTART command has been received already
                    else if (Command = ProtCmdStop) and (FAppFields.Count > CliSFieldRestart) then begin
                        try
                            FAppsChangedFlag := True;
                            if EmailBody <> '' then
                                AddLogLine(AppClientTitle + ': STOP and Email Received from Client: ' + Copy(EmailBody, 1, 80) +
                                                                                         ', Mode: ' + FAppFields[CliSFieldRestart])
                            else
                                AddLogLine(AppClientTitle + ': STOP Packet Received, Mode: ' + FAppFields[CliSFieldRestart]);
                            if AppSuppState >= AppStateStopping then   // duplicate or restarting
                                Exit;
                            AppLastPingTick := IcsGetTickCount64;
                            if FMonIpServer.States[SocNr] = logstateOK then begin
                                Packet := IcsAppMonName + ProtDelim +
                                          ProtCmdPong + ProtDelim +
                                          AppClientTitle + ProtDelim +
                                          ProtDelim;
                                AddLogLine(AppClientTitle + ': Acknowledged STOP command: ' + Packet);
                            end;
                            FMonIpServer.SendLogLine(Packet, AppClientID);

                        // NONESTOP mode, see what are doing
                            if AppMonMode = AMModeNonStop then begin
                                AppRestartMode := FindAMRestart(FAppFields[CliSFieldRestart]);
                                AddLogLine(AppClientTitle + ': Control Requested: ' + AMRestModeNames[AppRestartMode]);
                            end
                            else begin
                                AppRestartMode := RestModeNot;
                                AddLogLine(AppClientTitle + ': Not Restarting Client');
                            end;
                            AppRestartInfo := AMRestModeNames[AppRestartMode];

                        // don't change state if already stopping
                            if AppSuppState <> AppStateProgStop then begin
                                case AppRestartMode of
                                    RestModeNone: AppSuppState := AppStateStopping;
                                    RestModeAuto: AppSuppState := AppStateProgStop;
                                    RestModeRequest: AppSuppState := AppStateProgStop;
                                    RestModeItelf: begin
                                                      AppSuppState := AppStateProgStop;  // don't stop, but check it does stop
                                                      AppStopState := StopStateStopWait;
                                                   end;
                                    RestModeNot: AppSuppState := AppStateStopping;
                                end;
                                AppStoppingTick := IcsGetTickCount64;
                            end;
                        except
                            AddLogLine (CliTitle + ': Exception STOP Packet: ' + IcsGetExceptMess (ExceptObject)) ;
                            AppRestartMode := RestModeNot;
                            AppSuppState := AppStateStopping;
                        end;
                    end

                 // USERINFO command, might have EmailBody and will send email, maybe app wants it?
                    else if Command = ProtCmdUserInfo then begin
                        FAppsChangedFlag := True;
                        AddLogLine(AppClientTitle + ': USERINFO Packet Received - ' + Line);
                        AppLastPingTick := IcsGetTickCount64;
                        AppSuppState := AppStateOK;
                     // onUserInfoEvent(Line)
                     // pending
                    end

                 // INSTALL command, replace running EXE with new one, stopping and restarting
                    else if Command = ProtCmdInstall then begin
                        FAppsChangedFlag := True;
                        AddLogLine(AppClientTitle + ': INSTALL Packet Received - ' + Line);
                        AppLastPingTick := IcsGetTickCount64;
                        AppSuppState := AppStateOK;
                     // onxxerInfoEvent(Line)
                     // pending
                    end

                    else begin
                        AddLogLine('Unexpected Server Packet, Bad Command - ' + Line);
                    end;
                end;
            end
            else
                AddLogLine('Unexpected Server Packet, Bad Command - ' + Line);
        end
        else
             AddLogLine('Unexpected Server Packet, Too Few Fields - ' + Line);
    except
        AddLogLine ('Exception Processing Packet: ' + IcsGetExceptMess (ExceptObject)) ;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsLoadAppMonSrvFromIni(MyIniFile: TCustomIniFile; IcsAppMonSrv: TIcsAppMonSrv; const Section: String = 'IcsAppMonSrv');
begin
    if NOT Assigned (MyIniFile) then
        raise Exception.Create('Must open and assign INI file first');
    if NOT Assigned (IcsAppMonSrv) then
        raise Exception.Create('Must assign IcsAppMonSrv first');

    with IcsAppMonSrv do begin
        MaxSuppApps := MyIniFile.ReadInteger(Section, 'MaxSuppApps', MaxSuppApps);
        MonSrvIP := MyIniFile.ReadString (section, 'MonSrvIP', MonSrvIP) ;
        MonSrvPort := MyIniFile.ReadString (section, 'MonSrcPort', MonSrvPort) ;
        MonBcastIP := MyIniFile.ReadString (section, 'MonBcastIP', 'MonBcastIP') ;
        MonBcastPort := MyIniFile.ReadString (section, 'MonBcastPort', MonBcastPort) ;
        LogPackets := IcsCheckTrueFalse(MyIniFile.ReadString(Section, 'LogPackets', 'False'));
        LANWide := IcsCheckTrueFalse(MyIniFile.ReadString(Section, 'LANWide', 'False'));
        BCastSec := MyIniFile.ReadInteger(Section, 'KeepAliveTimeSec', BCastSec);
        DeadSecs := MyIniFile.ReadInteger(Section, 'DeadSecs', DeadSecs);
        MinDeadSecs := MyIniFile.ReadInteger(Section, 'MinDeadSecs', MinDeadSecs);
        StopWaitSecs := MyIniFile.ReadInteger(Section, 'KeepAliveTimeSec', StopWaitSecs);
        StartWaitSecs := MyIniFile.ReadInteger(Section, 'StartWaitSecs', StartWaitSecs);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
