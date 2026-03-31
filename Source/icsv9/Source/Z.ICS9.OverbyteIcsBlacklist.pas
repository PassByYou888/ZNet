{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  TIcsBlackList supports blackisting or block listing of IP addresses
              that attempt repeated failed access to TCP/IP servers. It maintains
              a list of IP addresses or Values that have previously exceeded a
              specific number of failed attempts, against which new attempts may
              be checked.
Creation:     March 2009
Updated:      Oct 2025
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


TIcsBlacklist
-------------

TIcsBlackList supports blackisting or block listing of IP addresses that attempt
repeated failed access to TCP/IP servers. It maintains a list of IP addresses or
Values that have previously exceeded a specific number of failed attempts, against
which new attempts may be checked.

BlockAttempts - maximum attempts allowed within BlockAfterMins period
BlockAfterMins - period in minutes in which BlockAttempts allowed
BlockForMins - minutes for which further attempts blocked after max reached




Baseline - March 2009
2 June 2010  - better attempt to clear old data when removed IPs from blacklist
18 July 2012 - added GetCountAll and GetCountBlock
14 Oct 2016  - added SaveAscii to save strings instead of IP addresses
               added ListName propertry for events
               better check for old saved duplicates
14 Nar 2019  - V8.60 - Adapted for main ICS packages and FMX support.
                       Renamed from TMagBlacklist to TIcsBlacklist
                       Added TIcsStringBuild to efficiently build Ansi or Unicode
                        strings on all versions of Delphi.
                       Added TIcsBuffLogStream buffered log stream designed to
                        write large log files, flushing regularly to disk by
                        opening, writing and closing each time the buffer fills
                        or after a timeout of X seconds. The file name is
                        date/time mask format, typically for one log file per day.
                        Write files in ANSI, UTF-9 or UTF-16, with a BOM.
                       Added IcsSimpleLogging write text to end of old or new
                        file, opening and closing file, ignores any errors,
                        not designed for continual updating!  The file name is
                        date/time mask format, typically for one log file per day.
Dec 09, 2020 - V8.65  Fix for Posix.
                      Renamed Ics.Posix.Messages.pas to Ics.Posix.PXMessages.pas.
Jun 24, 2021 - V8.67  Moved TIcsStringBuild class to OverbyteIcsUtils.
                      Internally use BlockedFlag instead of setting attempts
                        to 9999 so we can keep counting failed attempts.
                      SaveAscii defaulted to true to save IPv6 addresses.
Jun 24, 2022 - V8.70  TIcsBuffLogStream.WriteLine is more tolerant of
                         code pages.
Jan 25, 2023 - V8.71  Using Int64 ticks.
Aug 08, 2023 V9.0  Updated version to major release 9.
Aug 07, 2024 V9.3  Moved TIcsBuffLogStream to OverbyteIcsSslUtils, while
                     IcsSimpleLogging is in OverbyteIcsUtils.
Oct 02, 2024 V9.4  Removed Ics.Posix.PXMessages, not needed here.
Oct 16, 2025 V9.5  Major rewrite to TIcsBlacklist to support saving IPv6 addresses in
                     binary as well as ASCII, they sort better in reports and take less
                     memory. SaveAscii now defaults to False.
                   Allow adding and checking an TSockAddrIn6 which avoids conversion
                     to strings if binary saving used for IP addresses.
                   Added NeverSaveFile which prevents flushing to file, for memory only.
                   Logging blacklist now shows days if timestamp not within 24 hours.
                   AddBlackList and FailedBlackList allows ISO country code and ASN to
                     be saved.
                   Added CountryNameEvent called so application can look up a country
                     name from an ISO country code, using TIcsGeoTools
                   ReportBlackList now uses TIcsStringBuild for efficiency with
                     massive lists.
                   Make data available for better reporting.
                   Saving list to file is now sorted.
                   Increased default flush and clean mins from 2 to 60 minutes.
                   Removed Mask and WhiteList, never implemented.
                   TBlackListEvent is now OnAppLog: TIcsAppLogEvent for commonality
                     with other components.
                   Added new components TIcsFilterList and TIcsIpAddrList to replace
                     TestFilters using HackFilterList and TestIpWhiteList using
                     WhiteIpList in sample OverbyteIcsSslMultiWebServ1.pas.
                   TIcsFilterList reads same file hackfilterlist.txt containing
                     key=value pairs which are used to filter incoming connections
                     for path, remhost, country, useragent or referrer, trying
                     to filter out abusive remote hosts.
                   TIcsIpAddrList reads same file whiteiplist.txt which is a list
                     of ASCII IP full or partial addresses, generally that should
                     not be blocked by filters.   Pending, add range masks.
                   Time stamp converted to real time, not UTC, in reports.



Note: don't attempt to save non-IP addreses with SaveAscii=False
Note: Tested with over one million random IP addresses on a public server.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsBlacklist;
{$ENDIF}

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
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Posix.Time,
    Z.ICS9.Ics.Posix.WinTypes,
{$ENDIF}
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    {$IFDEF Rtl_Namespaces}System.DateUtils{$ELSE}DateUtils{$ENDIF},
{$IFDEF FMX}
    Z.ICS9.Ics.Fmx.OverbyteIcsWndControl,
{$ELSE}
    Z.ICS9.OverbyteIcsWndControl,
{$ENDIF FMX}
    Z.ICS9.OverbyteIcsTypes,
    Z.ICS9.OverbyteIcsTicks64,
    Z.ICS9.OverbyteIcsUtils;


const
    CopyRight    : String     = ' TIcsBlacklist  (c) 2025 V9.5 ';

type
    TBlackRec = record
        SocFamily: TSocketFamily;  { V9.5 }
        IpBytes: TTcsIpBytes;      { V9.5 4 or 16 bytes, replaces Addr, never used mask, sorts properly  }
        AValue: string ;       { ascii IP address or other value, only use IpBytes or Value, never both }
    //    Ascii: boolean ;     { V9.5 gone uses fSaveAscci }
        FirstUtime: int64 ;    // Unix time, seconds since 1970
        LastUtime: int64 ;
        Attempts: integer ;
        ReasonId: integer ;
        BlockedFlag: Boolean;  { V8.67 overrides Attempts so we can keep counting }
        ISOA2: String;         { V9.5 two character ISO country code, ie GB, NL, US }
        ASN: String;           { V9.5 ASN information -ie Alibaba (US) Technology Co., Ltd. (45102) }
    end ;
    PTBlackRec = ^TBlackRec ;
    TBlackRecs = array of TBlackRec;   { V9.5 }

    TIcsBlacklist = class(TIcsWndControl)
     protected
     { Protected declarations }
        fBlackRecs: TBlackRecs ;       { V9.5 main storage, may be a million records or more }
        fBlackIdx4: TIcsFindList ;     { V9.5 non-IP addresses also indexed here }
        fBlackIdx6: TIcsFindList ;     { V9.5 separate indexes for IPv4 and IPv6 }
        fTotBlackRecs: integer ;
        fBlackFile: string ;
        fTimer: TIcsTimer ;
        fChangedFlag: boolean ;
        fFlushMins: integer ;
        fCleanMins: integer ;
        fFlushTrg: int64 ;
        fCleanTrg: int64 ;
        fBlockAfterMins: integer ;
        fBlockAttempts: integer ;
        fBlockForMins: integer ;
        fSaveAscii: boolean ;  // Oct 2016
        fListName: string ;    // Oct 2016
        fLastFreeIndex: integer;   { V9.5 }
        fTotFreeBlackRec: integer; { V9.5 }
        fNeverSaveFile: Boolean;   { V9.5 }
        FBlackLogEvent: TIcsAppLogEvent;           { V9.5 }
        FCountryNameEvent: TIcsCountryNameEvent;   { V9.5 }
        procedure SetFlushMins (Value: integer) ;
        procedure SetCleanMins (Value: integer) ;
        procedure SetBlackFile (Value: string) ;
        procedure OnTimer (Sender: TObject);
        function ResizeArray (NewLen: integer): integer ;
        function GetMaxlackRec: Integer;
     public
     { Public declarations }
        constructor Create(Aowner:TComponent); override;
        destructor Destroy; override;
        procedure AppLog(const Msg: string);
        procedure FlushToFiles ;
        procedure LoadFromFiles ;
        procedure FlushandReload;
        procedure ClearList ;
        procedure CleanList ;
        procedure RebuildList;
        procedure ClearBlackRec(var BlackRec: TBlackRec);   { V9.5 }
        function NewBlackRec(const Value: string; ASockAddr: TSockAddrIn6; var BlackRec: TBlackRec): Boolean;  { V9.5 }
        function FindBlackRec(const MyBlackRec: TBlackRec; var FoundRec: PTBlackRec): Integer ;    { V9.5 }
        function InternalAddBlack(const Value: string; ASockAddr: TSockAddrIn6; MoreAttempts: boolean; ReasonId: integer;
                                                                         const ISOA2, ASN: String): TBlackRec ;  { V9.5 added SocAddr, ISOA2, ASN }
        function InternalFindBlack(const Value: string; ASockAddr: TSockAddrIn6): TBlackRec;   { V9.5 }
        procedure InternalRemoveBlack(const Value: string; ASockAddr: TSockAddrIn6);   { V9.5 }
        procedure AddBlackList (const Value: string; ReasonId: integer; const ISOA2: String = ''; const ASN: String = '') ;  { V9.5 added ISO and ASN }
        function FailedBlackList(const Value: string; ReasonId: integer; const ISOA2: String = ''; const ASN: String = ''): boolean;  { V9.5 }
        procedure RemBlackList (const Value: string) ;
        function CheckBlackList (const Value: string): boolean ;
        function GetFullBlackList(const Value: string): TBlackRec ;
        procedure AddBlackListSoc (ASockAddr: TSockAddrIn6; ReasonId: integer; const ISOA2: String = ''; const ASN: String = '') ;  { V9.5 added ISO }
        function CheckBlackListSoc (ASockAddr: TSockAddrIn6): boolean ;     { V9.5 }
        function FailedBlackListSoc(ASockAddr: TSockAddrIn6; ReasonId: integer; const ISOA2: String = ''; const ASN: String = ''): boolean;  { V9.5 }
        procedure RemBlackListSoc (ASockAddr: TSockAddrIn6) ;               { V9.5 }
        function GetFullBlackListSoc(ASockAddr: TSockAddrIn6): TBlackRec ;  { V9.5 }
        function ReportBlackList (All: boolean; Blocked: Boolean = True): string ;  { V9.5 added Blocked }
        function GetCountAll: integer ;    // 18 July 2012
        function GetCountBlock: integer ;   // 18 July 2012
        property BlackRecs: TBlackRecs       read FBlackRecs ;      { V9.5 make data available for better reporting }
        property BlackIdx4: TIcsFindList      read FBlackIdx4;
        property BlackIdx6: TIcsFindList      read FBlackIdx6;
        property TotBlackRecs: integer       read FTotBlackRecs;
        property TotFreeBlackRec: integer    read fTotFreeBlackRec; { V9.5 }
        property MaxBlackRec: integer        read GetMaxlackRec;    { V9.5 }
    published
    { Published declarations }
        property SaveAscii: boolean         read fSaveAscii      write fSaveAscii;  // Oct 2016
        property NeverSaveFile: Boolean     read fNeverSaveFile  write fNeverSaveFile;   { V9.5 }
        property ListName: string           read fListName       write fListName;
        property BlackFile: string          read fBlackFile      write SetBlackFile;
        property FlushMins: integer         read fFlushMins      write SetFlushMins;
        property CleanMins: integer         read fCleanMins      write SetCleanMins;
        property BlockAfterMins: integer    read fBlockAfterMins write fBlockAfterMins;
        property BlockAttempts: integer     read fBlockAttempts  write fBlockAttempts;
        property BlockForMins: integer      read fBlockForMins   write fBlockForMins;
        property OnBlackLogEvent: TIcsAppLogEvent
                                            read FBlackLogEvent  write FBlackLogEvent;       { V9.5 }
        property OnCountryNameEvent: TIcsCountryNameEvent
                                            read FCountryNameEvent write FCountryNameEvent;   { V9.5 }
      end;

{ V9.5 TIcsFilterList class }
    TFilterRec = record
        AKey: String;
        AValue: String;
    end;
    PTFilterRec = ^TFilterRec;
    TFilterRecs = array of TFilterRec;

    TIcsFilterList = class(TComponent)
    private
        FFilterRecs: TFilterRecs;
        FFilterKey: TStringList ;
        FTotFilters: Integer;
        FFileName: String;
        FOnAppLog: TIcsAppLogEvent;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure AppLog(const Msg: string);
        function LoadList: Boolean;
        function TestFilters(const Key, Value: String; XLeft: Boolean = False; Partial: Boolean = True): Boolean ;
        property FilterRecs: TFilterRecs read FFilterRecs;
        property TotFilters: Integer read FTotFilters;
        property FileName: string read FFileName write FFileName;
        property OnAppLog: TIcsAppLogEvent read FOnAppLog write FOnAppLog;
    end;

{ V9.5 TIcsIpAddrList class }
    TIpAddrRec = record
        IpBytes: TTcsIpBytes;      { 4 or 16 bytes }
        SocFamily: TSocketFamily;
        MaskBits: Integer;       { up to 32 IPv4, 128 IPv6 }
        AValue: String;        // TEMP ASCII IP
        ISOA2: String;
    end;
    PTIpAddrRec = ^TIpAddrRec;
    TIpAddrRecs = array of TIpAddrRec;

    TIcsIpAddrList = class(TComponent)
    private
        FIpAddrRecs: TIpAddrRecs;
        FIpAddrIdx: TIcsFindList ;
        FTotIpAddrs: Integer;
        FFileName: String;
        FOnAppLog: TIcsAppLogEvent;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure AppLog(const Msg: string);
        function LoadList: Boolean;
        procedure ClearIpAddrRec(var IpAddrRec: TIpAddrRec);
        function NewIpAddrRec(const Value, Mask: string; var IpAddrRec: TIpAddrRec): Boolean;
//        function FindIpAddrRec(const MyIpAddrRec: TIpAddrRec; var FoundRec: PTIpAddrRec): Integer;
        function InternalAddIpAddr(const Value, Mask: string; ISOA2: String): TIpAddrRec ;
//        function InternalFindIpAddr(const Value: string; ASockAddr: TSockAddrIn6): TIpAddrRec;
//        procedure InternalRemoveIpAddr(const Value, Mask: string);
//        procedure AddIpAddr(const Value, Mask: string; ISOA2: String = '');
        function CheckIpAddr(const Value: string; Log: Boolean = True): Boolean;
//        function CheckIpAddrSoc (ASockAddr: TSockAddrIn6): boolean ;
//        procedure RemIpAddr(const Value: string) ;
        property IpAddrRecs: TIpAddrRecs read FIpAddrRecs;
        property TotIpAddrs: Integer read FTotIpAddrs;
        property FileName: string read FFileName write FFileName;
        property OnAppLog: TIcsAppLogEvent read FOnAppLog write FOnAppLog;
    end;

var
    GSaveAscii: Boolean;  // V9.5 set for compare function

implementation

{ TIcsBlacklist }

constructor TIcsBlacklist.Create(Aowner: TComponent);
begin
    inherited;
    SetLength (fBlackRecs, 0) ;
    fBlackIdx4 := TIcsFindList.Create ;
    fBlackIdx4.Sorted := true ;
    fBlackIdx6 := TIcsFindList.Create ;
    fBlackIdx6.Sorted := true ;
    fTotBlackRecs := 0 ;
    fBlackFile := '' ;
    fChangedFlag := false ;
    SetFlushMins (60) ;          { V9.5 flush and clear mins from 2 to 60 minutes }
    SetCleanMins (60) ;
    fBlockAfterMins := 5 ;
    fBlockAttempts := 5 ;
    fBlockForMins := 120 ;
    fSaveAscii := False;       { V9.5 more efficient now we handle IPv6 addresses properly }
    fNeverSaveFile := False;   { V9.5 for memory only lists }
    fListName := 'Blacklist' ;  // Oct 2016
    ResizeArray (64) ;        { V9.5 initial size }
    FTotFreeBlackRec := 64;   { V9.5 initial free }
    AllocateHWnd;
    fTimer := TIcsTimer.Create (self) ;
    fTimer.OnTimer := OnTimer ;
    fTimer.Interval := 30 * TicksPerSecond ;
    fTimer.Enabled := true ;
end;

destructor TIcsBlacklist.Destroy;
begin
    try
        FreeAndNil (fTimer);
        if fChangedFlag then
            FlushToFiles ;
    except
    end ;
    SetLength (fBlackRecs, 0) ;
    FreeAndNil (fBlackIdx4);
    FreeAndNil (fBlackIdx6);
    inherited;
end;

procedure TIcsBlacklist.AppLog(const Msg: string);
begin
    if Assigned(FBlackLogEvent) then
        FBlackLogEvent(Self, Msg);
end;


// called by TFindList for sort and find comparison
function CompareBlackRec (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal and > 0 if Item1 is greater than Item2.
begin
    if GSaveAscii then
        result := CompareText (PTBlackRec (Item1).AValue, PTBlackRec (Item2).AValue)
    else
        Result := IcsCompareTBytes(PTBlackRec (Item1).IpBytes, PTBlackRec (Item2).IpBytes);
end ;


procedure TIcsBlacklist.ClearBlackRec(var BlackRec: TBlackRec);   { V9.5 }
begin
    with BlackRec do begin
        SetLength(IpBytes, 0) ;  // clear new addresses
        AValue := '' ;
        SocFamily := sfAny;
        Attempts := 0 ;
        FirstUtime := 0 ;
        LastUtime := 0 ;
        BlockedFlag := False;
        ISOA2 := '';
        ASN := '';
    end;
end;


function TIcsBlacklist.GetMaxlackRec: Integer;  { V9.5 }
begin
    Result := Length (fBlackRecs);
end;


// rebuild black list sorted index to array ignoring blank records
// pending - could also compact list periodically if too many blanks
procedure TIcsBlacklist.RebuildList;
var
    I: integer ;
begin
    GSaveAscii := FSaveAscii;   { V9.5 for sorting }
    fTotBlackRecs := 0;
    fLastFreeIndex := -1;  // V9.5 reset next free
    fBlackIdx4.Clear ;
    fBlackIdx6.Clear ;
    if fBlackIdx4.Capacity < (Length (fBlackRecs) * 2) then
                        fBlackIdx4.Capacity := Length (fBlackRecs) * 2 ;
    fBlackIdx6.Capacity := fBlackIdx4.Capacity;
    for I := 0 to Pred (Length (fBlackRecs)) do
    begin
        if (fBlackRecs [I].Attempts > 0) then begin
            if (fBlackRecs [I].SocFamily = sfIpv6) then
                fBlackIdx6.AddSorted(@fBlackRecs [I], CompareBlackRec)
            else
                fBlackIdx4.AddSorted(@fBlackRecs [I], CompareBlackRec) ;
            fTotBlackRecs := fTotBlackRecs + 1;
        end
        else begin
            if fLastFreeIndex = -1 then
                fLastFreeIndex := I ;     // keep first free record
        end;
    end ;
    fTotFreeBlackRec := Length (fBlackRecs) - fTotBlackRecs;
    if fTotBlackRecs > 0 then
        AppLog(fListName + ': Rebuilt List, Total Items ' + IntToStr (fTotBlackRecs) +
                                                          ', Free Items ' + IntToStr(fTotFreeBlackRec));
    fChangedFlag := true ;
end ;


// resize array and list for new records
function TIcsBlacklist.ResizeArray(NewLen: integer): integer ;
var
    OldLen, K: integer ;
begin
    GSaveAscii := FSaveAscii;   { V9.5 for sorting }
    OldLen := Length (fBlackRecs) ;
    if NewLen < 64 then
        NewLen := 64;
    SetLength (fBlackRecs, NewLen) ;
    if OldLen > NewLen then
        AppLog(fListName + ': Reduced Empty List Size to ' + IntToStr (NewLen))  { V9.5 }
    else begin
        if OldLen <> 0 then
            AppLog(fListName + ': Increased Empty List Size to ' + IntToStr (NewLen)) ;
        for K := OldLen to Pred (NewLen) do
            ClearBlackRec(fBlackRecs [K]);
    end;
    fBlackIdx4.Capacity := NewLen ;
    fBlackIdx6.Capacity := NewLen ;
    Result := NewLen ;
    RebuildList;  // rebuild index, sets various totals
end ;


// V9.5 find record in indexes, IPv6 and IPv4 in separate indexes
function TIcsBlacklist.FindBlackRec(const MyBlackRec: TBlackRec; var FoundRec: PTBlackRec): Integer ;    { V9.5 }
begin
    Result := -1;
    if fTotBlackRecs = 0 then
        Exit;
    GSaveAscii := FSaveAscii;   { V9.5 for sorting }
    if (MyBlackRec.SocFamily = sfIpv6) then begin
        if fBlackIdx6.Find (@MyBlackRec, CompareBlackRec, Result) then
            FoundRec := PTBlackRec (fBlackIdx6 [Result])
        else
            Result := -1;
    end
    else begin
        if fBlackIdx4.Find (@MyBlackRec, CompareBlackRec, Result) then
            FoundRec := PTBlackRec (fBlackIdx4 [Result])
        else
            Result := -1;
    end;
end;

// create a new BlackRec from an ASCII value or IPv4/v6 address or SocAddr, returns false if invalid IP address and non-ASCII
// record may be used for searching or adding with more fields completed
function TIcsBlacklist.NewBlackRec(const Value: string; ASockAddr: TSockAddrIn6; var BlackRec: TBlackRec): Boolean;  { V9.5 }
var
    LocSockAddr: TSockAddrIn6;
begin
    Result := False;
    ClearBlackRec(BlackRec);
    BlackRec.SocFamily := IcsFamilyFromSocAddr(ASockAddr);  // so we know Ipv4 or IPv6
    if (BlackRec.SocFamily = sfAny) and (Value = '') then  // nothing useful passed, die
       exit ;

 // V9.5 we only save ascii IP address or TBBytes binary version, not both to save memory
 // if wrong version passed, convert to correct version
    if fSaveAscii then begin
        if Value = '' then
            BlackRec.AValue := WSocketSockAddrToStr(ASockAddr)  {V9.5 }
        else begin
            WSocketIsIPEx(Value, BlackRec.SocFamily);  { V9.5 keep family, will be sfAny for non-IP address }
            BlackRec.AValue := Value ;
        end;
    end
    else
    begin
        if (BlackRec.SocFamily = sfAny) then begin
            LocSockAddr :=  WSocketIPStrToSocAddr(Value, Result);
            if NOT Result then
                Exit;
            BlackRec.SocFamily := IcsFamilyFromSocAddr(LocSockAddr);
            if (BlackRec.SocFamily = sfAny) then  // only store real IPs as TBytes
                Exit;
            BlackRec.IpBytes := IcsSocIpBytes(LocSockAddr);
        end
        else begin
            BlackRec.IpBytes := IcsSocIpBytes(ASockAddr);
        end;
    end ;
    Result := True;
end;


// internal add IP address to blacklist, incremented attempts if done already
// information is saved in a dynamic array, with a sorted list pointing to the
// each record used to search actual IP address (saved as 32-bits).  If an IP
// is removed, the IP for the record is set to zero, and the record then re-used.
// The array is increased in size if too short.
function TIcsBlacklist.InternalAddBlack(const Value: string; ASockAddr: TSockAddrIn6; MoreAttempts: boolean;
                                                                    ReasonId: integer; const ISOA2, ASN: String): TBlackRec;
var
    BlackRec: TBlackRec ;
    Index, NewLen, K: integer ;
    OldBlackRec: PTBlackRec;    { V9.5 }
    S: String;
begin
    ClearBlackRec(Result);   // in case of error
    GSaveAscii := FSaveAscii;   { V9.5 for sorting }

 // create new record from IP to search
    if NOT NewBlackRec(Value, ASockAddr, BlackRec) then
        Exit;

 // V9.5 check if old record exists and can be updated
    Index := FindBlackRec(BlackRec, OldBlackRec);

 // IP already listed, update record and increment attempts
    if Index >= 0 then begin
        OldBlackRec.LastUtime := IcsGetUnixTime ;
        inc (OldBlackRec.Attempts) ;

    // see if exceeded maximum attempts allowed and not yet blocked
        if MoreAttempts then begin
            if (OldBlackRec.Attempts >= fBlockAttempts) and (NOT OldBlackRec.BlockedFlag) then begin
                fChangedFlag := true ;
                OldBlackRec.BlockedFlag := True;     { V8.67 }
            end ;
        end
        else begin
            OldBlackRec.BlockedFlag := True;     { V8.67 }
            fChangedFlag := true ;
        end ;
        Result := OldBlackRec^ ;
    end

    // create new record for this IP
    else begin
        BlackRec.ReasonId := ReasonId ;
        BlackRec.FirstUtime := IcsGetUnixTime ;
        BlackRec.LastUtime := BlackRec.FirstUtime ;
        BlackRec.Attempts := 1;
        BlackRec.BlockedFlag := False;       { V8.67 }
        if NOT MoreAttempts then
            BlackRec.BlockedFlag := True;    { V8.67 }
        BlackRec.ISOA2 := ISOA2;             { V9.5 }
        BlackRec.ASN := ASN;                 { V9.5 }
        Result := BlackRec ;

     // find first free record in array
     // V9.5 keep track of last free record found so we can can skip
        Index := -1 ;
        if fLastFreeIndex >= Length (fBlackRecs) then
            fLastFreeIndex := 0;
        for K := fLastFreeIndex to Pred (Length (fBlackRecs)) do begin
            if fBlackRecs [K].Attempts = 0 then begin
                Index := K ;
                break ;
            end ;
        end;

    // no free record, allocate more memory in array
        if Index < 0 then begin
            NewLen := Length (fBlackRecs) ;
            if NewLen > 10000 then
                NewLen := NewLen + 10000
            else
                NewLen := NewLen * 2;
            ResizeArray (NewLen) ;
            Index := fLastFreeIndex;
            if Index < 0 then   // sanity test
                Exit;
         end ;
        fLastFreeIndex := Index; { V9.5 search from here next time }

    // add record to array and correct sorted list
        fBlackRecs [Index] := BlackRec ;
        if (BlackRec.SocFamily = sfIpv6) then   { V9.5 }
            fBlackIdx6.AddSorted (@fBlackRecs [Index], CompareBlackRec)
        else
            fBlackIdx4.AddSorted (@fBlackRecs [Index], CompareBlackRec) ;  // ascii as well as IPv4
        inc (fTotBlackRecs) ;
        fChangedFlag := true ;
        if Value = '' then
            S := WSocketSockAddrToStr(ASockAddr)
        else
            S := Value;
        AppLog(fListName + ': Added New Item: ' + S + ' (' + ISOA2 + '), Total Items ' + IntToStr (fTotBlackRecs)) ;
    end ;
end;


// unconditional add IP address to blacklist, no more attempts allowed
procedure TIcsBlacklist.AddBlackList(const Value: string; ReasonId: integer; const ISOA2: String = ''; const ASN: String = '') ;  { V9.5 added ISO }
begin
    InternalAddBlack(Value, BlankSockAddr, false, ReasonId, ISOA2, ASN);
end;


procedure TIcsBlacklist.AddBlackListSoc(ASockAddr: TSockAddrIn6; ReasonId: integer; const ISOA2: String = ''; const ASN: String = '') ;  { V9.5 }
begin
    InternalAddBlack('', ASockAddr, false, ReasonId, ISOA2, ASN);
end;


// get black list record for specific IP address
function TIcsBlacklist.InternalFindBlack(const Value: string; ASockAddr: TSockAddrIn6): TBlackRec;   { V9.5 }
var
    BlackRec: TBlackRec ;
    OldBlackRec: PTBlackRec;    { V9.5 }
    Index: integer ;
begin
    ClearBlackRec(Result);   // in case of error

 // create new record from IP to search
    if NOT NewBlackRec(Value, ASockAddr, BlackRec) then
        Exit;

 // V9.5 keeping IPv6 and IPv4 in separate indexes
    Index := FindBlackRec(BlackRec, OldBlackRec);
    if (Index >= 0) then
        Result := OldBlackRec^;
end;


function TIcsBlacklist.GetFullBlackList(const Value: string): TBlackRec ;
begin
    Result := InternalFindBlack(Value, BlankSockAddr);
end;


function TIcsBlacklist.GetFullBlackListSoc(ASockAddr: TSockAddrIn6): TBlackRec ;  { V9.5 }
begin
    Result := InternalFindBlack('', ASockAddr);
end;


// check if IP address in black list and exceeded maximum attempts
// return true if blocked
function TIcsBlacklist.CheckBlackList(const Value: string): boolean;
var
    BlackRec: TBlackRec ;
begin
    result := false ;
    BlackRec := InternalFindBlack(Value, BlankSockAddr);
    if (BlackRec.Attempts = 0) then
        exit ;  // not found
    if BlackRec.BlockedFlag then
        result := true ;  { V8.67 }
    if (BlackRec.Attempts >= fBlockAttempts) then
        result := true ;
end;


function TIcsBlacklist.CheckBlackListSoc (ASockAddr: TSockAddrIn6): boolean ;     { V9.5 }
var
    BlackRec: TBlackRec ;
begin
    result := false ;
    BlackRec := InternalFindBlack('', ASockAddr);
    if (BlackRec.Attempts = 0) then
        exit ;  // not found
    if BlackRec.BlockedFlag then
        result := true ;  { V8.67 }
    if (BlackRec.Attempts >= fBlockAttempts) then
        result := true ;
end;

// notify a failed login attempt, add IP address to black list if not already
// done otherwise increment attempts counter, return true if now blocked

function TIcsBlacklist.FailedBlackList(const Value: string; ReasonId: integer; const ISOA2: String = ''; const ASN: String = ''): boolean;  { V9.5 }
var
    BlackRec: TBlackRec ;
begin
    result := false ;
    BlackRec := InternalAddBlack (Value, BlankSockAddr, true, ReasonId, ISOA2, ASN) ;
    if (BlackRec.Attempts = 0) then
        exit ;  // not found
    if BlackRec.BlockedFlag then
        result := true ;  { V8.67 }
    if (BlackRec.Attempts >= fBlockAttempts) then
        result := true ;
end;


function TIcsBlacklist.FailedBlackListSoc(ASockAddr: TSockAddrIn6; ReasonId: integer; const ISOA2: String = ''; const ASN: String = ''): boolean;  { V9.5 }
var
    BlackRec: TBlackRec ;
begin
    result := false ;
    BlackRec := InternalAddBlack ('', ASockAddr, true, ReasonId, ISOA2, ASN) ;
    if (BlackRec.Attempts = 0) then
        exit ;  // not found
    if BlackRec.BlockedFlag then
        result := true ;  { V8.67 }
    if (BlackRec.Attempts >= fBlockAttempts) then
        result := true ;
end;

// remove an IP address from black list by setting record to zeroes and removing from index
procedure TIcsBlacklist.InternalRemoveBlack(const Value: string; ASockAddr: TSockAddrIn6);   { V9.5 }
var
    BlackRec: TBlackRec ;
    OldBlackRec: PTBlackRec;    { V9.5 }
    Index: integer ;
begin
 // create new record from IP to search
    if NOT NewBlackRec(Value, ASockAddr, BlackRec) then
        Exit;
    Index := FindBlackRec(BlackRec, OldBlackRec);
    if (Index >= 0) then begin
        ClearBlackRec(OldBlackRec^);
        fLastFreeIndex := 0;  // reset
        if (BlackRec.SocFamily = sfIpv6) then
             fBlackIdx6.Delete (Index)
        else
             fBlackIdx4.Delete (Index);
        Dec (fTotBlackRecs) ;
        inc (fTotFreeBlackRec) ;
        fChangedFlag := true ;
    end;
end;


// remove an IP address from black list
procedure TIcsBlacklist.RemBlackList(const Value: string);
begin
    InternalRemoveBlack(Value, BlankSockAddr);
end;


procedure TIcsBlacklist.RemBlackListSoc (ASockAddr: TSockAddrIn6) ;               { V9.5 }
begin
    InternalRemoveBlack('', ASockAddr);
end;


// clean black list by removing any IP addresses that have been blocked but
// whose blocked time has now expired, and IPs not blocked because they did not
// reach the maximum within the allowed period

procedure TIcsBlacklist.CleanList;
var
    I: integer ;
    rebuild: boolean ;
    utime: int64 ;
begin
    if (fTotBlackRecs = 0) then
        exit ;
    rebuild := false ;
    utime := IcsGetUnixTime ;
    for I := 0 to Pred (Length (fBlackRecs)) do begin
        with fBlackRecs [I] do begin
            if (Attempts > 0) then begin
                if (Attempts >= fBlockAttempts) or BlockedFlag then begin
                    if (((utime - LastUtime) div 60) >= fBlockForMins) or (LastUtime > utime) then begin
                        if fSaveAscii then
                            AppLog(fListName + ': Removed Blocked Item: ' + AValue)
                        else
                            AppLog(fListName + ': Removed Blocked Item: ' + IcsIpBytesToStr(IpBytes)) ;  { V9.5 }
                        ClearBlackRec(fBlackRecs [I]);   { V9.5 }
                        rebuild := true ;
                    end ;
                end
                else begin
                    if (((utime - FirstUtime) div 60) >= fBlockAfterMins) or (FirstUtime > utime) then begin
                        if fSaveAscii then
                            AppLog(fListName + ': Removed Old Item: ' + AValue)
                        else
                            AppLog(fListName + ': Removed Old Item: ' + IcsIpBytesToStr(IpBytes)) ;  { V9.5 }
                        ClearBlackRec(fBlackRecs [I]);   { V9.5 }
                        rebuild := true ;
                    end ;
                end ;
            end ;
        end ;
    end ;
    if rebuild then
        RebuildList;
end;

// clear black list of all IP addresses
procedure TIcsBlacklist.ClearList;
begin
    fTotBlackRecs := 0 ;
    fTotFreeBlackRec := 0 ;
    SetLength (fBlackRecs, 0) ;
    fBlackIdx4.Clear ;
    fBlackIdx6.Clear ;
    fChangedFlag := true ;
end;


// flush black lists to comma separated ASCII file, V9.5 now sorted to make it faster to read again
// this is done automatically periodically and when the component is destroyed
procedure TIcsBlacklist.FlushToFiles;
var
    FileLines: TStringList ;
    I, TotSaved: integer ;

    procedure AddLine(BlackRec: PTBlackRec);             { V9.5 }
    var
        Sip, S1: String;
    begin
        with BlackRec^ do begin
            if (Attempts = 0) or (FirstUtime = 0) then
                Exit;
            if fSaveAscii then
                Sip := AValue
            else
                Sip := IcsIpBytesToStr(IpBytes);
            if Sip = '' then
                Exit;
            if BlockedFlag then
                S1 := 'Y'
            else
                S1 := 'N';
           { V9.5 added ISO, mask (2nd field) is now blank }
            FileLines.Add (Sip + ',,' + IntToStr (FirstUtime) + ',' + IntToStr (LastUtime) + ',' +
                IntToStr (Attempts) + ',' + IntToStr (ReasonId) + ',' + S1  + ',' + ISOA2  + ',' + AnsiQuotedStr(ASN, '"'));
            TotSaved := TotSaved + 1;
        end;
    end;

begin
    if fNeverSaveFile then    { V9.5 }
        Exit;
    fChangedFlag := false ;
    if fBlackFile = '' then
        exit ;  // no file name means nothing to save
    if NOT Assigned (fBlackIdx4) then
        exit ;  // sanity check
    TotSaved := 0;
    FileLines := TStringList.Create ;
    try

        if (FBlackIdx4.Count > 0) then begin
            for I := 0 to Pred (FBlackIdx4.Count) do
                AddLine(FBlackIdx4[I]);
        end;
        if (FBlackIdx6.Count > 0) then begin
            for I := 0 to Pred (FBlackIdx6.Count) do
                AddLine(FBlackIdx6[I]);
        end;
        try
            if FileExists (fBlackFile) then
                {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif}.DeleteFile (fBlackFile) ;
            FileLines.SaveToFile (fBlackFile) ;
            AppLog(fListName + ': Saved to File: ' + fBlackFile + ', Total Records: ' + IcsIntToCStr(TotSaved)) ;
        except
            AppLog(fListName + ': Failed File Save: ' + fBlackFile + ' - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FileLines.Free ;
        fChangedFlag := false ;
    end ;
end;

// load black and white lists from comma separated ASCII files,ignoring expired records
// this is done automatically when the BlackFile property is assigned a file name
// note prior to V9.5, Mask was saved in file, but is blank now
procedure TIcsBlacklist.LoadFromFiles;
var
    FileLines, FileRecord: TStringList ;
    BlackRec: TBlackRec ;
    I, BadNr, ExpiredNr, NewLen: integer ;
    OldBlackRec: PTBlackRec;    { V9.5 }
    utime: int64 ;              { V9.5 }
    OldFmtFlag: Boolean;
begin
    fChangedFlag := false ;
    if fBlackFile = '' then
        exit ;  // no file name means nothing to read
    if NOT Assigned (fBlackIdx4) then
        exit ;  // sanity check
    FileLines := TStringList.Create ;
    FileRecord := TStringList.Create ;
    try
        try
            if NOT FileExists (fBlackFile) then begin
                AppLog(fListName + ': File Not Found: ' + fBlackFile) ;
                exit ;
            end ;
            FileLines.LoadFromFile (fBlackFile) ;
        except
            AppLog(fListName + ': Failed to File Load: ' + fBlackFile + ' - ' + IcsGetExceptMess (ExceptObject)) ;
            Exit ;
        end ;
        ClearList;
        if FileLines.Count = 0 then begin
            AppLog(fListName + ': File Empty: ' + fBlackFile) ;
            Exit;
        end;
        BadNr := 0 ;
        ExpiredNr := 0;
        NewLen := FileLines.Count;
        if NewLen > 10000 then
            NewLen := NewLen + 10000
        else
            NewLen := NewLen * 2;
        fTotBlackRecs := 0;
        ResizeArray (NewLen) ;
        OldFmtFlag := False;   // are old records unsorted???  if so, must AddSorted, else faster simple Add.
        utime := IcsGetUnixTime ;
        for I := 0 to Pred (FileLines.Count) do begin
            FileRecord.CommaText := FileLines [I] ;  // always ascii IP address in file
            if FileRecord.Count < 6 then begin
                inc (BadNr) ;
                Continue;
            end;

           // always ascii IP address in file, first record
            if NOT NewBlackRec(FileRecord [0], BlankSockAddr, BlackRec) then begin    { V9.5 }
                inc (BadNr) ;
                Continue;
            end;

          // check if mask saved by old version of component, means not sorted order already
            if FileRecord [1] <> '' then
                OldFmtFlag := True;

           // no time or attempts, ignore
            if (atoi64 (FileRecord [2]) = 0) or (atoi (FileRecord [4]) = 0) then begin
                inc (BadNr) ;
                Continue;
            end;

         // check not a duplicate
            if (FindBlackRec(BlackRec, OldBlackRec) >= 0) then begin    { V9.5 }
                inc (BadNr) ;
                Continue;
            end;

        // get rest of record and save to indexes
           with BlackRec do begin
                FirstUtime := atoi64 (FileRecord [2]) ;
                LastUtime := atoi64 (FileRecord [3]) ;
                Attempts := atoi (FileRecord [4]) ;
                ReasonId := atoi (FileRecord [5]) ;
                if (FileRecord.Count >= 7) and (FileRecord [6] = 'Y') then
                    BlockedFlag := True;                                   { V8.67 }
                if (FileRecord.Count >= 8)  then
                    ISOA2 := FileRecord [7]                          { V9.5 }
                else
                    ISOA2 := '';
                if (FileRecord.Count >= 9)  then begin
                    ASN := FileRecord [8];      { V9.5 }
                    if (Length(ASN) >= 2) and (ASN[1] = '"') then
                        ASN := Copy(ASN, 2, Length(ASN) - 2);
                end
                else
                    ASN := '';

        // V9.5 don't add expired records, they'll get removed again immediately
                if (Attempts >= fBlockAttempts) or BlockedFlag then begin
                    if (((utime - LastUtime) div 60) >= fBlockForMins) or (LastUtime > utime) then begin
                        ExpiredNr := ExpiredNr + 1;
                        Continue;
                    end
                end
                else begin
                    if (((utime - FirstUtime) div 60) >= fBlockAfterMins) or (FirstUtime > utime) then begin
                        ExpiredNr := ExpiredNr + 1;
                        Continue;
                    end;
                end;
            end;

        // finally add new record to main array and index to that record
            fBlackRecs [fTotBlackRecs] := BlackRec;
            if (BlackRec.SocFamily = sfIpv6) then begin   { V9.5 }
                if OldFmtFlag then
                    fBlackIdx6.AddSorted(@fBlackRecs [fTotBlackRecs], CompareBlackRec)
                else
                    fBlackIdx6.Add(@fBlackRecs [fTotBlackRecs]);
            end
            else begin
                if OldFmtFlag then
                    fBlackIdx4.AddSorted(@fBlackRecs [fTotBlackRecs], CompareBlackRec)
                 else
                    fBlackIdx4.Add(@fBlackRecs [fTotBlackRecs]);  { V9.5 faster }
            end;
            inc (fTotBlackRecs) ;
        end ;
        AppLog(fListName + ': Loaded from File: ' + fBlackFile + ', Total Items ' + IntToStr (fTotBlackRecs) +
                                        ', Bad Records ' + IntToStr (BadNr) + ', Expired Records ' + IntToStr (ExpiredNr)) ;
        fChangedFlag := false ;
    finally
        FileLines.Free ;
        FileRecord.Free ;
        fChangedFlag := false ;
    end ;
end;


// V9.5 save records to file and reload to list, so removing any expired records and saving memory
procedure TIcsBlacklist.FlushandReload;
begin
    if fNeverSaveFile then    { V9.5 }
        Exit;
    FlushToFiles;
    LoadFromFiles;
end;


// timer called every 30 seconds to Clean and Flush lists
procedure TIcsBlacklist.OnTimer(Sender: TObject);
begin
    fTimer.Enabled := false ;
    try
        if IcsTestTrgTick64 (fCleanTrg) then
        begin
            fCleanTrg := IcsGetTrgMins64 (fCleanMins) ;
            CleanList ;
            if fChangedFlag then
                FlushToFiles ;
        end ;
        if IcsTestTrgTick64 (fFlushTrg) then
        begin
            fFlushTrg := IcsGetTrgMins64 (fFlushMins) ;
            if fChangedFlag then
                FlushToFiles ;
        end ;

    finally
        fTimer.Enabled := true ;
    end ;
end;

function TIcsBlacklist.ReportBlackList (All: boolean; Blocked: Boolean = True): string ;  { V9.5 added Blocked }
var
    I: integer ;
    NowUtime: Int64;
    S: String;
    SB: TIcsStringBuild;   { V9.5 }

    function GetLast(UTime: Int64): String;     { V9.5 }
    begin
        if (NowUTime - UTime) < SecsPerDay then
{$IFDEF COMPILER23_UP}
            Result := TimeToStr(UnixToDateTime(UTime, False))   { V9.5 return real time not UTC for modern compilers }
{$ELSE}
            Result := TimeToStr(UnixToDateTime(UTime))
{$ENDIF}
        else
            Result := IntToStr((NowUTime - UTime) div SecsPerDay) + ' day(s)';
    end;

    procedure AddLine(BlackRec: PTBlackRec);
    var
        CName: String;
    begin
        with BlackRec^ do begin
            if (Attempts = 0) then
                Exit;
            if (Attempts >= FBlockAttempts) or All then begin
                if fSaveAscii then
                    SB.AppendBuf(AValue)
                else
                    SB.AppendBuf(IcsIpBytesToStr(IpBytes)) ;
                SB.AppendBuf(' attempts ' + IcsIntToCStr (Attempts) +
                    ', first at ' + GetLast(FirstUtime) + ', last at ' + GetLast (LastUtime)) ;   { V9.5 show days beyond 1 }
                if BlockedFlag and Blocked then
                    SB.AppendBuf(' BLOCKED') ;  { V8.67 }
                if ISOA2 <> '' then begin
                    CName := '' ;
                    if Assigned(FCountryNameEvent) then   { V9.5 see if application can look up country names }
                        FCountryNameEvent(Self, ISOA2, CName);
                    if CName = '' then
                        SB.AppendBuf(' ISO=' + ISOA2)
                    else
                       SB.AppendBuf(IcsSpace + CName);
                end;
                if ASN <> '' then
                    SB.AppendBuf(IcsSpace + ASN);    { V9.5 }
                SB.AppendBuf(IcsCRLF);   { V9.5 }
            end;
        end;
    end;

begin
    result := '' ;
    NowUtime := IcsGetUnixTime ;
    SB := TIcsStringBuild.Create(FTotBlackRecs * 20, True);   // V9.5 use String Builder
    try
        I := FBlockForMins;
        if fBlockAfterMins > I then
            I := fBlockAfterMins;
        if I >= MinsPerDay then
            S := IntToStr(I div MinsPerDay) + ' day(s)'
        else
            S := IntToStr(I div HoursPerDay) + ' hours';
        SB.AppendBuf(FListName + ' Total: ' + IntToStr(GetCountAll) + ' in Last ' + S +  ', Report:' + IcsCRLF);  { V9.5 }
        if (FBlackIdx4.Count > 0) then begin
            for I := 0 to Pred (FBlackIdx4.Count) do
                AddLine(FBlackIdx4[I]);
        end;
        if (FBlackIdx6.Count > 0) then begin
            for I := 0 to Pred (FBlackIdx6.Count) do
                AddLine(FBlackIdx6[I]);
        end;
        SB.AppendBuf(IcsCRLF);
        Result := SB.GetWString;
    finally
        SB.Free;
    end;
end;


function TIcsBlacklist.GetCountAll: integer ;    // 18 July 2012
var
    I: integer ;
begin
    result := 0 ;
    if (fTotBlackRecs = 0) then
        exit ;
    for I := 0 to Pred (fBlackIdx4.Count) do begin
        if (PTBlackRec (fBlackIdx4 [I])^.Attempts > 0) then
            inc (result) ;
    end;
    for I := 0 to Pred (fBlackIdx6.Count) do begin
        if (PTBlackRec (fBlackIdx6 [I])^.Attempts > 0) then
            inc (result) ;
    end;
end;

function TIcsBlacklist.GetCountBlock: integer ;   // 18 July 2012
var
    I: integer ;
begin
    result := 0 ;
    if (fTotBlackRecs = 0) then
        exit ;
    for I := 0 to Pred (fBlackIdx4.Count) do begin
        with PTBlackRec (fBlackIdx4[I])^ do begin
            if (Attempts = 0) then
                continue ;
            if (Attempts >= fBlockAttempts) then
                inc (result) ;
        end;
    end;
    for I := 0 to Pred (fBlackIdx6.Count) do begin
        with PTBlackRec (fBlackIdx6[I])^ do begin
            if (Attempts = 0) then
                continue ;
            if (Attempts >= fBlockAttempts) then
                inc (result) ;
        end;
    end;
end;

// property setters

procedure TIcsBlacklist.SetCleanMins(Value: integer);
begin
    fCleanMins := Value ;
    if fCleanMins = 0 then
        fCleanMins := 2 ;
    fCleanTrg := IcsGetTrgMins64 (fCleanMins) ;
end;

procedure TIcsBlacklist.SetFlushMins(Value: integer);
begin
    fFlushMins := Value ;
    if fFlushMins = 0 then
        fFlushMins := 2 ;
    fFlushTrg := IcsGetTrgMins64 (fFlushMins) ;
end;

procedure TIcsBlacklist.SetBlackFile(Value: string);
begin
    if Value <> fBlackFile then
    begin
        fBlackFile := Value ;
        if fTotBlackRecs = 0 then
            LoadFromFiles;  // don't kill existing black list
    end ;
end;

{ V9.5 TIcsFilterList class }

constructor TIcsFilterList.Create(AOwner: TComponent);
begin
    Inherited Create(AOwner);
    FFilterKey := TStringList.Create;
    FFilterKey.Sorted := True;
    FFilterKey.Duplicates := dupIgnore;
    FTotFilters := 0;
    FFileName := 'FilterList.txt';
end;


destructor TIcsFilterList.Destroy;
begin
    FreeAndNil(FFilterKey);
    SetLength(FFilterRecs, 0);
    Inherited Destroy;
end;

procedure TIcsFilterList.AppLog(const Msg: string);
begin
    if Assigned(FOnAppLog) then
        FOnAppLog(Self, Msg);
end;


function TIcsFilterList.LoadList: Boolean;
var
    I, J: Integer;
    Line: String;
    MyList: TStringList;
begin
    Result := False;
    if NOT FileExists (FFilename) then begin
        AppLog('List File Not Found: ' + FFilename) ;
        exit ;
    end;
    MyList := TStringList.Create;
    try
        try
            MyList.LoadFromFile(FFilename) ;
            FFilterKey.Clear;
            SetLength(FFilterRecs, MyList.Count + 5);
            FTotFilters := 0;

         // load filter lines, removing comments from end  ***'
         // path=/.env              *** July 2025
         // country=AR
         // remhost=.as62651.net
         // remaddr=107.189.10.  *** Tor exit
            if MyList.Count > 0 then begin
              // clean up, lower case, remove comments
                for I := 0 to MyList.Count - 1 do begin
                    Line := IcsLowerCase(Trim(MyList[I]));
                    J := Pos(' **', Line);                 // ** comment end, March 2025 was ***
                    if J > 4 then
                        Line := Trim(Copy(Line, 1, J));
                    J := Pos(IcsSpace + IcsSpace + IcsSpace, Line);   // trim line after three blanks
                    if J > 4 then
                        Line := Trim(Copy(Line, 1, J));
                    if Pos('*', Line) = 1 then
                        Line := '';  // * comment start
                    if Pos(';', Line) = 1 then
                        Line := '';  // ; comment start
                    if Pos('=', Line) = 0 then
                        Line := '';  // no key=value
                    MyList[I] := Line;
                end;

            // sort list by keys, then add records, build index to first duplicate key
                MyList.Sort;
                for I := 0 to MyList.Count - 1 do begin
                    Line := MyList[I];
                    J := Pos('=', Line);
                    if J > 2 then begin
                        with FFilterRecs[FTotFilters] do begin
                            AKey := Copy(Line, 1, J -1);
                            AValue := Trim(Copy(Line, J + 1, 999));
                            if NOT FFilterKey.Find(AKey, J) then
                                FFilterKey.AddObject(AKey, Pointer(FTotFilters)); // key and record number in array
                        end;
                        FTotFilters := FTotFilters + 1;
                    end;
                end;
            end;
            if FTotFilters = 0 then
                 AppLog('List is empty: ' + FFilename)
            else begin
                 AppLog('Loaded List OK: ' + FFilename + ', total records ' + IntToStr(FTotFilters) +
                                                                   ', total keys ' + IntToStr(FFilterKey.Count)) ;
                 Result := True;
            end;
        except
            AppLog('Failed to Load List: ' + FFilename);
        end;
    finally
        MyList.Free;
    end;
end;


function TIcsFilterList.TestFilters(const Key, Value: String; XLeft: Boolean = False; Partial: Boolean = True): Boolean;
var
    I, J, FirstRecNr: Integer;
    SearchKey, SearchValue: String;
begin
    Result := False;
    if FTotFilters = 0 then
        Exit;
    SearchValue := IcsLowercase(Trim(Value));
    if SearchValue = '' then
        Exit;
    SearchKey := IcsLowercase(Trim(Key));
    if NOT FFilterKey.Find(SearchKey, J) then   // key not found
        Exit;
    FirstRecNr := Integer(FFilterKey.Objects[J]);
    if (FirstRecNr < 0) or (FirstRecNr >= FTotFilters) then begin
        AppLog('Failed to Test Filter, Bad Index Object');
        Exit;
    end;
    for I := FirstRecNr to FTotFilters - 1 do
    begin
        with FFilterRecs[I] do begin
            if AKey <> SearchKey then
                Exit;
            J := Pos (AValue, SearchValue);
            if (XLeft and (J = 1)) or ((NOT XLeft) and (J >= 1)) then begin
                if Partial or (AValue = SearchValue) then begin
                    Result := True;
                    AppLog('Filter List matched: Key ' + Key + ': ' + AValue + ' - ' + SearchValue);
                    Exit;
                end;
            end;
        end;
    end;
end;


{ V9.5 TIcsIpAddrList class }

constructor TIcsIpAddrList.Create(AOwner: TComponent);
begin
    Inherited Create(AOwner);
    FIpAddrIdx := TIcsFindList.Create;
    FTotIpAddrs := 0;
    FFileName := 'IpAddrList.txt';
end;


destructor TIcsIpAddrList.Destroy;
begin
    FreeAndNil(FIpAddrIdx);
    SetLength(FIpAddrRecs, 0);
    Inherited Destroy;
end;


procedure TIcsIpAddrList.AppLog(const Msg: string);
begin
    if Assigned(FOnAppLog) then
        FOnAppLog(Self, Msg);
end;


function TIcsIpAddrList.LoadList: Boolean;
var
    I, J: Integer;
    Line: String;
    MyList: TStringList;
begin
    Result := False;

 // pending - handle blocks ie 178.255.82.64/27 is 178.255.82.64 through 178.255.82.95
    if NOT FileExists (FFilename) then begin
        AppLog('List File Not Found: ' + FFilename) ;
        exit ;
    end;
    MyList := TStringList.Create;
    try
        MyList.LoadFromFile(FFilename) ;
        FIpAddrIdx.Clear;
        SetLength(FIpAddrRecs, MyList.Count + 5);
        FTotIpAddrs := 0;

// load IP address lines, removing comments from end  ***'
// 192.168.1.
// 217.146.102.1
// 2a00:1940:     *** IPv6
// 193.113.57
        if MyList.Count > 0 then begin
          // clean up, lower case, remove comments
            for I := 0 to MyList.Count - 1 do begin
                Line := IcsLowerCase(Trim(MyList[I]));
                J := Pos(' **', Line);
                if J > 4 then
                    Line := Trim(Copy(Line, 1, J));
                J := Pos(IcsSpace + IcsSpace + IcsSpace, Line);   // trim line after three blanks
                if J > 4 then
                    Line := Trim(Copy(Line, 1, J));
                if Pos('*', Line) = 1 then
                    Line := '';  // * comment start
                if Pos(';', Line) = 1 then
                    Line := '';  // ; comment start
                if Line <> '' then
                   InternalAddIpAddr(Line, '', '');
            end;
        end;
        if FTotIpAddrs = 0 then
             AppLog('List is empty: ' + FFilename)
        else begin
             AppLog('Loaded List OK: ' + FFilename + ', total records ' + IntToStr(FTotIpAddrs)) ;
             Result := True;
        end;
    finally
        MyList.Free;
    end;

end;

// called by TFindList for sort and find comparison
function CompareIpAddrRec (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal and > 0 if Item1 is greater than Item2.
begin
    result := CompareText (PTIpAddrRec (Item1).AValue, PTIpAddrRec(Item2).AValue);
end ;

procedure TIcsIpAddrList.ClearIpAddrRec(var IpAddrRec: TIpAddrRec);
begin
    with IpAddrRec do begin
        SetLength(IpBytes, 0) ;  // clear new addresses
        AValue := '' ;
        SocFamily := sfAny;
        MaskBits := 0 ;
        ISOA2 := '';
    end;
end;


function TIcsIpAddrList.NewIpAddrRec(const Value, Mask: string; var IpAddrRec: TIpAddrRec): Boolean;
begin
    ClearIpAddrRec(IpAddrRec);
    IpAddrRec.AValue := Value;   // partial IP address
  // pending, find mask from partial, convert to binary
   Result := True;
end;


// only works for equal not partial
{
function TIcsIpAddrList.FindIpAddrRec(const MyIpAddrRec: TIpAddrRec; var FoundRec: PTIpAddrRec): Integer;
begin
    Result := -1;
    if FTotIpAddrs = 0 then
        Exit;
    if FIpAddrIdx.Find (@MyIpAddrRec, CompareIpAddrRec, Result) then
        FoundRec := PTIpAddrRec(Result);
end; }


function TIcsIpAddrList.InternalAddIpAddr(const Value, Mask: string; ISOA2: String): TIpAddrRec;
var
    MyIpAddrRec: TIpAddrRec;
begin
    if NOT NewIpAddrRec (Value, Mask, MyIpAddrRec) then
        Exit;
    IpAddrRecs[FTotIpAddrs] := MyIpAddrRec;
    FIpAddrIdx.AddSorted(@IpAddrRecs[FTotIpAddrs], CompareIpAddrRec);
    FTotIpAddrs := FTotIpAddrs + 1;
end;

{
function TIcsIpAddrList.InternalFindIpAddr(const Value: string; ASockAddr: TSockAddrIn6): TIpAddrRec;
begin
   c
end;


procedure TIcsIpAddrList.InternalRemoveIpAddr(const Value, Mask: string);
begin

end;


procedure TIcsIpAddrList.AddIpAddr(const Value, Mask: string; ISOA2: String = '');
begin

end;  }


function TIcsIpAddrList.CheckIpAddr(const Value: string; Log: Boolean = True): boolean;
var
    I: Integer;
begin
    Result := False;
    if FTotIpAddrs = 0 then
        Exit;
    for I := 0 to FTotIpAddrs- 1 do begin
        if Pos(FIpAddrRecs[I].AValue, Value) = 1 then begin
            Result := True;
            if Log then
                AppLog('IP Address List matched: ' + FIpAddrRecs[I].AValue + ' - ' + Value);
            Exit;
        end;
    end;
end;

{
function TIcsIpAddrList.CheckIpAddrSoc (ASockAddr: TSockAddrIn6): boolean ;
begin

end;


procedure TIcsIpAddrList.RemIpAddr (const Value: string) ;
begin

end;  }



end.
