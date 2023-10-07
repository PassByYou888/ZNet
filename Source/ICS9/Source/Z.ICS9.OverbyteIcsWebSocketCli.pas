{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Jaroslav Kulísek, updated by Angus Robertson.
Description:  WebSocket client protocol.
Creation:     Dec 2022
Updated:      Aug 2023
Version:      V9.0
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1996-2023 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.

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

Updates:
Apr 19, 2023 - V8.71 - baseline.
               The sample demo for this component is OverbyteIcsHttpRestTst.dpr.
Aug 08, 2023 V9.0  Updated version to major release 9.



Note - currently the WSConnect methods is synchronous with a timeout.
Note - some classes in this unit are shared wth the WebSocket server component.

WebSocket is a full duplex TCP protocol for web servers to support interactive web pages,
typically dynamic updating such as chat sessions, spell checkers as you type, search
hints, etc.  WebSocket extends the HTTP protocol and can be carried through HTTP proxies
using the same ports as HTTP. The WebSocket protocol includes ping/pong keep alive so
long lived connections are not dropped.

TSslWebSocketCli is a WebSocket client component, an alternate to the clients built
into web browsers (Javascript mysocket = new WebSocket(host)) used for complex web pages.

TSslWebSocketCli has been tested echoing against wss://echo.websocket.events/ and
wss://ws.postman-echo.com/raw and against both the older ICS WebSocket server
OverbyteIcsWebSocketS component and the new OverbyteIcsWebSocketSrv component integrated
with the ICS web server.

TSslWebSocketCli descends from TSslHttpRest so most of it's properties and events
are common, but there are new methods and events all beginning with WS.

To use TSslWebSocketCli, set the URL and any other HTTP related properties (authentication,
SSL, etc), set DebugLevel to DebugParams to see frames logged (DebugBody for more info),
then call WSConnect to open a connection to the server, the OnWSConnected event
is called when the WebSocket protocol is negotiated check IsWSConnected in the event.

While the connection is open, the client will send a ping to the server every WSPingSecs
(minimum five seconds) to keep the connection alive if frames are not being sent.  If
the server sends a ping the component will respond with a Pong.

The server may immediately start sending frames of information that arrive in the
OnWSFrameRcvd event, as a simple String and a TWebSocketReceivedFrame record with a lot
more information.

You can send a simple text frame using the WSSendText method, binary using WSSendBinary or
from a stream using WSSendBinaryStream. The OnWSFrameSent event will be called after send,
you should not send another frame until the last one goes, there is currently no queue.

Finally call WSClose with a reason that is sent to the server and closes the connection,
with the OnWSDiconnected event called.

Note some classes in this unit are used by OverbyteIcsWebSocketSrv.pas which is a WebSocket
server unit.


}


{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsWebSocketCli;
{$ENDIF}

interface

{$I Include\Z.ICS9.OverbyteIcsDefs.inc}
{$IFNDEF COMPILER7_UP}
  {$MESSAGE FATAL 'Sorry, we do not want to support ancient compilers any longer'};
{$ENDIF}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}


uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Z.ICS9.Ics.Posix.WinTypes,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Sysutils{$ELSE}Sysutils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.TypInfo{$ELSE}TypInfo{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.UITypes, System.UIConsts,{$ENDIF}
    Z.ICS9.OverbyteIcsWinsock,
    Z.ICS9.OverbyteIcsTypes,
    Z.ICS9.OverbyteIcsUtils,
    Z.ICS9.OverbyteIcsSSLEAY, Z.ICS9.OverbyteIcsLIBEAY,
{$IFDEF FMX}
    Z.ICS9.Ics.Fmx.OverbyteIcsWndControl,
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocket,
    Z.ICS9.Ics.Fmx.OverbyteIcsHttpProt,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslHttpRest,
{$ELSE}
    Z.ICS9.OverbyteIcsWndControl,
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsHttpProt,
    Z.ICS9.OverbyteIcsSslHttpRest,
{$ENDIF FMX}
    Z.ICS9.OverbyteIcsCharsetUtils,
    Z.ICS9.OverbyteIcsSha1,
    Z.ICS9.OverbyteIcsLogger,      { for TLogOption }
    Z.ICS9.OverbyteIcsTicks64;    { V8.71 }

const
  MaxWSFrameHdrSize = 2+8+4; // the header should not be longer, +2B basic info, +8B max 64bit length, +4B mask
  MaxWSFrameSize = 16384-MaxWSFrameHdrSize;  // max frame size

type
    TWebSocketFrameKind = (wsfkUnknown, wsfkContinue, wsfkText, wsfkBin, wsfkClose, wsfkPing, wsfkPong, wsfkReserved);

    TWebSocketCloseReason = (wscrNormalClosure, wscrGoingAway, wscrProtocolError,
                            wscrUnsupportedData, wscrInvalidFramePayloadData, wscrPolicyViolation,
                            wscrMessageTooBig, wscrMandatoryExt, wscrInternalServerError);

    TWebSocketState = (wssHttp, wssConnecting, wssReady);

    TWSFrameState = (wsfsNotInitialized, wsfsInvalid, wsfsNotComplete, wsfsCompleted);

    TWSFrameDataMask = array[0..3] of Byte;
    PWSFrameDataMask = ^TWSFrameDataMask;

    TUint16Bytes = packed record
        case Integer of
        0: (B: array[0..1] of Byte);
        1: (W: Word);
    end;

    TUint64Bytes = packed record
        case Integer of
        0: (B: array[0..7] of Byte);
        1: (Q: Int64);
    end;

    TMemoryStreamCapacity = class(TMemoryStream)
    public
        property Capacity;
    end;

    TLimitedSizeMemStream = class(TStream)
    protected
        FCurrPos: Int64;
        procedure SwitchToFileStream;
        procedure SetSize(NewSize: Longint); override;
        procedure SetSize(const NewSize: Int64); override;
    public
        MaxMemStreamSize: Int64;
        TempFileNumberPrefix: String;
        TempFileNumDigits: Byte;
        TempFileNumberPostfix: String;
        TempFileName: String;
        Stream: TStream;
        constructor Create(AMaxMemStreamSize: Int64; const ATempFileNumberPrefix: String;
                                                  ATempFileNumDigits: Byte; const ATempFileNumberPostfix: String);
        destructor Destroy; override;
        function Read(var Buffer; Count: Longint): Longint; override;
        function Write(const Buffer; Count: Longint): Longint; override;
        function Seek(Offset: Longint; Origin: Word): Longint; override;
        function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
        procedure GrowCapacityTo(const ACapacity: Integer);
    end;


    TXORStream = class(TStream)
    protected
        Buf: TBytes;
        FStream: TStream;
        FXORS: TBytes; // what is xored
        FI: Longint;   // current index
        FL: Longint;   // used length of FXORS
        FOffset: Longint;
        procedure SetSize(NewSize: Longint); override;
        procedure SetSize(const NewSize: Int64); override;
    public
        constructor Create(const AStream: TStream; const AXORBytes: TBytes);
        constructor CreateEx(const AStream: TStream; const AXORBytes: TBytes; AOffset: Longint);
        constructor CreateExB(const AStream: TStream; const AXORB : array of Byte; AOffset: Longint);
        constructor CreateAnsi(const AStream: TStream; const AXORAnsiString: RawByteString);
        constructor CreateAnsiEx(const AStream: TStream; const AXORAnsiString: RawByteString; AOffset: Longint);
        destructor Destroy; override;
        function Read(var Buffer; Count: Longint): Longint; override;
        function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
        function Write(const Buffer; Count: Longint): Longint; override;
      end;


    TMemoryReadStream = class(TCustomMemoryStream)
    public
        // prevents the memory from being overwritten
        constructor Create(const APMemory: Pointer; const ASize: Integer);
        function Write(const Buffer; Count: Longint): Longint; override;
    end;


    TSslWebSocketCli = class;

    TWebSocketReceivedFrame = class(TObject)
    public
        State: TWSFrameState;
        Kind: TWebSocketFrameKind;
        IsFinal: Boolean;
        IsMasked: Boolean;
        DataBytes: Int64;
        StoredBytes: Int64;
        Mask: TWSFrameDataMask;
        Data: TStream;
        constructor Create;
        destructor Destroy; override;
        procedure Clear;
        function Parse(PBuf: Pointer; Size: Integer ): Boolean;
    end;

    TWebSocketOutgoingFrame = class(TObject)
    public
        FrameSourceID: Pointer;  // used to identify which frame it really sent
        Kind: TWebSocketFrameKind;
        IsFinal: Boolean;
        Data: TStream;
        DataBytes: Int64;
        constructor Create;
        destructor Destroy; override;
        procedure InitFrameData(AFrameSourceID: Pointer; AKind: TWebSocketFrameKind; AData: TStream; ADataBytes: Int64; AIsFinal: Boolean; DoMask: Boolean);
    end;


    TWSFrameRcvdEvent = procedure(Sender: TSslWebSocketCli; const APacket: String; var AFrame: TWebSocketReceivedFrame) of object;
    TWSFrameSentEvent = procedure(Sender: TSslWebSocketCli; var AFrame: TWebSocketOutgoingFrame) of object;

    TSslWebSocketCli = class(TSslHttpRest)
    protected
        FPeriodicTimer: TIcsTimer;
        WSClosing: Boolean;
        WSState: TWebSocketState;
        LastReceivedDataTickCount: Int64;  // if nothing arrives for a long time, try ping
        LastSentPingTickCount: Int64;      // and if nothing even then, the connection is dropped
        ClientKey: String;
        HeaderUpgrade: String;
        HeaderConnection: String;
        HeaderSecWebSocketAccept: String;
        HeaderSecWebSocketProtocol: String;
        HeaderAccessControlAllowOrigin: String;
        CurrFrame: TWebSocketReceivedFrame;
        CurrMultiFrame: TWebSocketReceivedFrame;
        CurrOutgoingFrame: TWebSocketOutgoingFrame;
        FWSFrameCounter: Integer;
        FWSPingSecs: Integer;
        FOnWSConnected: TNotifyEvent;
        FOnWSDisconnected: TNotifyEvent;
        FOnWSFrameRcvd: TWSFrameRcvdEvent;
        FOnWSFrameSent: TWSFrameSentEvent;
        procedure PeriodicTimerTimer(Sender: TObject);
    public
        OutgoingFrames: TList;     //queue of frames to send
        MaxMemStreamSize: Int64;
        StreamTempFolder: String;   // if is set, then TLimitedSizeMemStream is used
        LocationChangeCount: Integer;
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure LocationChange(Sender: TObject);
        function GetRemoteAddr(IncludePort: Boolean): String;
        procedure ClearOutgoingFrames;
        procedure ProcessReceivedFrame(AFrame: TWebSocketReceivedFrame);
        procedure TriggerSessionClosed; override;
        procedure TriggerBeforeHeaderSend(const Method: String; Headers: TStrings); override;
        procedure TriggerHeaderBegin; override;
        procedure TriggerHeaderFieldData(var AHeaderField, AHeaderData: String ); override;
        function GetProtocolPort(const AProtocol: String): String; override;
        function IsSSLProtocol(const AProtocol: String): Boolean; override;
        function IsWSProtocol(const AProtocol: String): Boolean;
        function IsWSProtocolURL(const AURL: String): Boolean;
        function IsKnownProtocol(const AProtocol: String): Boolean; override;
        function IsKnownProtocolURL(const AURL: String): Boolean; override;
        procedure SocketDataSent(Sender: TObject; ErrCode: Word); override;
        procedure SocketDataAvailable(Sender: TObject; ErrCode: Word); override;
        function IsWSConnected: Boolean;
        procedure ProcessPeriodicTasks;
        function WSConnect: Boolean;
        procedure WSClose( CloseReason: TWebSocketCloseReason; ADescription: String );
        procedure WSSendFrame(AFrameSourceID: Pointer; AFrameKind: TWebSocketFrameKind; const AStream: TStream);
        procedure WSSendFrameBytes(AFrameSourceID: Pointer; AFrameKind: TWebSocketFrameKind; const AData: TBytes);
        procedure WSSendFrameMemory(AFrameSourceID: Pointer; AFrameKind: TWebSocketFrameKind; const APData: Pointer; ADataBytes: Integer);
        procedure WSSendPing(const AData: TBytes);
        procedure WSSendPong(const AData: TBytes);
        procedure WSSendText(AFrameSourceID: Pointer; AText: String);
        procedure WSSendBinaryStream(AFrameSourceID: Pointer; AStream: TStream);
        property  WSFrameCounter: Integer read FWSFrameCounter;
    published
        property WSPingSecs: Integer read FWSPingSecs write FWSPingSecs;
        property OnWSConnected: TNotifyEvent read FOnWSConnected write FOnWSConnected;
        property OnWSDisconnected: TNotifyEvent read FOnWSDisconnected write FOnWSDisconnected;
        property OnWSFrameRcvd: TWSFrameRcvdEvent read FOnWSFrameRcvd write FOnWSFrameRcvd;
        property OnWSFrameSent: TWSFrameSentEvent read FOnWSFrameSent write FOnWSFrameSent;
    end;

function WSDumpFrame(const Desc: String; AFrame: TWebSocketReceivedFrame): String;
function GetWSFrameKind(Kind: TWebSocketFrameKind): String;

implementation

uses RTLConsts;



function FileDirExists(const FileDirName: String): Boolean;
var
    SR: TSearchRec;
begin
    Result := False;
    if FindFirst( FileDirName, faAnyFile, SR) = 0 then
    try
        Result := True;
    finally
        FindClose(SR);
  end;
end;

function GetNotExistingFileName( const FileNumberPrefix : String;  NumDigits : Byte; FileNumberPostfix : String ): String;
var
  s,ss : String;
  i,mi : Longint;
begin
  Result := '';

  if NumDigits <= 0 then Exit;
  if FileNumberPrefix = '' then Exit;
  ss := '%.'+IntToStr(NumDigits)+'d';
  mi := 1;
  for i := 1 to NumDigits do mi := mi*10;
  i := 0;
  repeat
    Inc(i);
    if i >= mi then Exit;
    s := FileNumberPrefix + Format(ss,[i]) + FileNumberPostfix;
  until not FileDirExists( s );
  Result := s;
end;

procedure AddSepStr( var s : String; const ss, sep  : String );
begin
  if s <> '' then s := s + sep;
  s := s + ss;
end;

procedure AddSepStrNoEmpty( var s : String; const ss, sep  : String );
begin
  if ss = '' then Exit;

  if s <> '' then s := s + sep;
  s := s + ss;
end;

function GetWSFrameKind(Kind: TWebSocketFrameKind): String;
begin
    case Kind of
        wsfkContinue : Result := 'Continuation frame';
        wsfkText     : Result := 'Text frame';
        wsfkBin      : Result := 'Binary frame';
        wsfkClose    : Result := 'Close frame';
        wsfkPing     : Result := 'Ping frame';
        wsfkPong     : Result := 'Pong frame';
        wsfkReserved : Result := 'Reserved frame';
    else
      Result := 'Unknown frame';
    end;
end;

function WSDumpFrame(const Desc: String; AFrame: TWebSocketReceivedFrame): String;
var
    ss,sd,sn : String;
    i: Integer;
    Buf : TBytes;
    OldPos : Int64;
    EC : TUint16Bytes;
begin
    Result := 'WS Frame Dump: ' +  Desc;
    if AFrame.IsFinal then
        AddSepStr(Result, 'Final Frame', ', ' )
    else
        AddSepStr(Result, 'Continuation Frame', ', ' );
    ss:= GetWSFrameKind(AFrame.Kind);
    AddSepStr(Result, ss, ', ' );
    if AFrame.IsMasked then
        AddSepStr(Result, 'XOR Masked', ', ' )
    else
        AddSepStr(Result, 'Not Masked', ', ' );
    AddSepStr(Result, 'DataSize: ' + IntToStr(AFrame.DataBytes), ', ' );
//    SetLength(Buf, 8192);
    if AFrame.Data <> nil then begin
        OldPos := AFrame.Data.Position;
        try
            AFrame.Data.Position := 0;
            sd := '';
            if AFrame.DataBytes > 0 then begin
                case AFrame.Kind of
                    wsfkText, wsfkPing, wsfkPong,  wsfkClose: begin
                        EC.W := 0;
                        if AFrame.Kind = wsfkClose then begin // close frame has word close reason at start
                            AFrame.Data.Read(EC, 2);
                        end;
                        sd := IcsHtmlToStr(AFrame.Data, CP_UTF8, False); // get packet as simple string
                        if Length(sd) > 132 then begin
                            SetLength(sd, 132);
                            sn := '"...'
                        end else
                            sn := '"';
                        if AFrame.Kind = wsfkClose then
                            sd := 'Close Reason: ' + IntToStr(Swap(EC.W)) + ' "' + Copy(sd, 3, MaxInt) + sn
                        else
                            sd := 'Text: "' + sd + sn;
                    end;
                    wsfkBin:  begin
                        SetLength(Buf, AFrame.DataBytes);
                        if Length(Buf) > 132 then
                            SetLength(Buf, 132);
                        AFrame.Data.Read(Buf[0], Length(Buf));
                        for i := 0 to Length(Buf) - 1 do begin
                            if Buf[i] < 32 then
                                 Buf[i] := Ord('?');  // strip non-ascii chars
                        end;
                        IcsMoveTBytesToString(Buf, 0, sd, 1, Length(Buf));
                        sd := 'Data: ' + sd + sn;
                    end;
                end;
            end;
            AddSepStrNoEmpty(Result, sd, ', ');  // final dump
        finally
          AFrame.Data.Position := OldPos;
        end;
    end;
end;


{  TLimitedSizeMemStream  }

constructor TLimitedSizeMemStream.Create(AMaxMemStreamSize: Int64;  const ATempFileNumberPrefix: String;
                                                        ATempFileNumDigits: Byte; const ATempFileNumberPostfix: String);
begin
    inherited Create;
    MaxMemStreamSize := AMaxMemStreamSize;
    TempFileNumberPrefix := ATempFileNumberPrefix;
    TempFileNumDigits := ATempFileNumDigits;
    TempFileNumberPostfix := ATempFileNumberPostfix;
    TempFileName := '';
    Stream := TMemoryStreamCapacity.Create;
    FCurrPos := Stream.Position;
end;

destructor TLimitedSizeMemStream.Destroy;
begin
    try
        if Stream <> nil then
        try
            Stream.Free;
        except
        end;
        if TempFileName <> '' then
            DeleteFile(TempFileName);
    finally
        inherited Destroy;
    end;
end;

procedure TLimitedSizeMemStream.SwitchToFileStream;
var
    MS: TCustomMemoryStream;
begin
    if TempFileName <> '' then Exit;
    if not (Stream is TCustomMemoryStream) then Exit;
    FCurrPos := Stream.Position;
    MS := Stream as TCustomMemoryStream;
    TempFileName := GetNotExistingFileName(TempFileNumberPrefix, TempFileNumDigits, TempFileNumberPostfix);
    if TempFileName = '' then
        raise EWriteError.CreateRes(@SWriteError);
    Stream := TFileStream.Create( TempFileName, fmCreate );
    Stream.Position := 0;
    MS.SaveToStream(Stream);
    MS.Free;
    Stream.Position := FCurrPos;
end;

procedure TLimitedSizeMemStream.SetSize(NewSize: Longint);
begin
    SetSize(Int64(NewSize));
end;

procedure TLimitedSizeMemStream.SetSize(const NewSize: Int64);
begin
    if NewSize >= MaxMemStreamSize then
        if TempFileName = '' then
            SwitchToFileStream;
     Stream.Size := NewSize;
    FCurrPos := Stream.Position;
end;

function TLimitedSizeMemStream.Read(var Buffer; Count: Longint): Longint;
begin
    Result := Stream.Read( Buffer, Count );
    FCurrPos := FCurrPos + Result;
end;

function TLimitedSizeMemStream.Write(const Buffer; Count: Longint): Longint;
begin
    if FCurrPos + Count >= MaxMemStreamSize then
        if TempFileName = '' then
            SwitchToFileStream;
    Result := Stream.Write( Buffer, Count );
    FCurrPos := FCurrPos + Result;
end;

function TLimitedSizeMemStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
    Result := Stream.Seek( Offset, Origin );
    FCurrPos := Result;
end;

function TLimitedSizeMemStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
    Result := Stream.Seek( Offset, Origin );
    FCurrPos := Result;
end;

procedure TLimitedSizeMemStream.GrowCapacityTo( const ACapacity : Integer );
begin
    if Stream <> nil then
        if Stream is TMemoryStreamCapacity then
            if ACapacity > (Stream as TMemoryStreamCapacity).Capacity then
                (Stream as TMemoryStreamCapacity).Capacity := ACapacity;
end;


{ TXORStream }

constructor TXORStream.Create(const AStream: TStream; const AXORBytes: TBytes);
begin
    CreateEx( AStream, AXORBytes, 0 );
end;

constructor TXORStream.CreateEx(const AStream: TStream; const AXORBytes: TBytes; AOffset: Longint);
var
    FC: Longint;
begin
    inherited Create;
    SetLength(Buf, 0);
    FStream := AStream;
    FXORS   := AXORBytes;
    SetLength(FXORS, Length(FXORS)); // copy content, not only reference
    FOffset := AOffset;
    FL := Length(FXORS);
    if FL > 0 then begin
        FC := FL - (FOffset mod FL);
        FI := ((AStream.Position + FC) mod FL);
    end
    else
        FI := 0;
end;

constructor TXORStream.CreateExB(const AStream: TStream; const AXORB: array of Byte; AOffset: Longint);
var
    FC : Longint;
begin
    inherited Create;
    SetLength(Buf, 0);
    FStream := AStream;
    SetLength(FXORS, Length(AXORB));
    if Length(FXORS) > 0 then
        Move(AXORB[0], FXORS[0], Length(FXORS));
    FOffset := AOffset;
    FL := Length(FXORS);
    if FL > 0 then begin
        FC := FL - (FOffset mod FL);
        FI := ((AStream.Position + FC) mod FL);
    end
    else
        FI := 0;
end;


constructor TXORStream.CreateAnsi(const AStream: TStream; const AXORAnsiString: RawByteString);
begin
    CreateAnsiEx(AStream, AXORAnsiString, 0);
end;

constructor TXORStream.CreateAnsiEx(const AStream: TStream; const AXORAnsiString: RawByteString;  AOffset: Longint);
var
    FC : Longint;
begin
    inherited Create;
    SetLength(Buf, 0);
    FStream := AStream;
    SetLength(FXORS, Length(AXORAnsiString));
    Move(AXORAnsiString[1], FXORS[0], Length( AXORAnsiString ));
    FOffset := AOffset;
    FL := Length( FXORS );
    if FL > 0 then  begin
        FC := FL - (FOffset mod FL);
        FI := ((AStream.Position + FC) mod FL);
    end
    else
        FI := 0;
end;

destructor TXORStream.Destroy;
begin
    try
        SetLength(Buf, 0);
        SetLength(FXORS, 0);
    finally
        inherited Destroy;
    end;
end;

procedure TXORStream.SetSize(NewSize: Longint);
begin
    SetSize(Int64(NewSize));
end;

procedure TXORStream.SetSize(const NewSize: Int64);
begin
    FStream.Size := NewSize;
    Seek(0, soCurrent); // to refresh the position related stuff,
                      // because some bad streams, e.g. TStringStream
                      // they change FPosition with SetSize directly and not via Seek
end;

function TXORStream.Read(var Buffer; Count: Longint): Longint;
var
    i : Longint;
    P : PByte;
begin
    Result := FStream.Read(Buffer, Count);
    if FL <= 0 then Exit;
    if Result <= 0 then Exit;
    P := @Buffer;
    for i := 0 to Result-1 do begin
        P^ := P^ xor FXORS[FI];
        Inc(P);
        Inc(FI);
        if FI >= FL then
            FI := 0;
    end;
end;

function TXORStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
    FC : Longint;
begin
    Result := FStream.Seek(Offset, Origin);
    if FL > 0 then begin
        FC := FL - (FOffset mod FL);
        FI := ((Result + FC) mod FL);
    end
    else
        FI := 0;
end;

function TXORStream.Write(const Buffer; Count: Longint): Longint;
var
    i : Longint;
    P : PByte;
begin
    Result := 0;
    if Count <= 0 then
        Exit;
    if FL > 0 then begin
        if Length( Buf ) < Count then
            SetLength( Buf, Count );
        P := @Buffer;
        for i := 0 to Count-1 do begin
            Buf[i] := P^ xor FXORS[FI];
            Inc(P);
            Inc(FI);
            if FI >= FL then FI := 0;
        end;
        Result := FStream.Write(Buf[0], Count);
    end
    else
        Result := FStream.Write(Buffer, Count);
end;


{ TMemoryReadStream }

constructor TMemoryReadStream.Create(const APMemory: Pointer; const ASize: Integer);
begin
    inherited Create;
    SetPointer(APMemory, ASize);
end;

function TMemoryReadStream.Write(const Buffer; Count: Longint): Longint;
begin
    Result := 0;
end;


{  TWebSocketReceivedFrame  }

constructor TWebSocketReceivedFrame.Create;
begin
    inherited Create;
    Data := nil;
    Clear;
end;

destructor TWebSocketReceivedFrame.Destroy;
begin
    try
        if Data <> nil then
            Data.Free;
    finally
        inherited Destroy;
    end;
end;

procedure TWebSocketReceivedFrame.Clear;
var
    i : Integer;
begin
    State := wsfsNotInitialized;
    Kind := wsfkUnknown;
    IsFinal := False;
    IsMasked := False;
    DataBytes := 0;
    StoredBytes := 0;
    for i := Low(Mask) to High(Mask) do
        Mask[i] := 0;
    if Data <> nil then
        Data.Size := 0;
end;

function TWebSocketReceivedFrame.Parse(PBuf: Pointer; Size: Integer): Boolean;
type
    TFrameData = array[0..0] of Byte;
    PFrameData = ^TFrameData;

    TFrameShort = packed record
        FinOpCode: Byte;
        MaskedPayLoadLength: Byte;
        Data: TFrameData;
    end;

    PFrameShort = ^TFrameShort;

    TFrameShortMasked = packed record
        FinOpCode: Byte;
        MaskedPayLoadLength: Byte;
        Mask: TWSFrameDataMask;
        Data: TFrameData;
    end;

    PFrameShortMasked = ^TFrameShortMasked;

    TFrameSmall = packed record
        FinOpCode: Byte;
        MaskedPayLoadLength: Byte;
        ExtPayLoadLength: TUint16Bytes;
        Data: TFrameData;
    end;

    PFrameSmall = ^TFrameSmall;

    TFrameSmallMasked = packed record
        FinOpCode: Byte;
        MaskedPayLoadLength: Byte;
        ExtPayLoadLength: TUint16Bytes;
        Mask: TWSFrameDataMask;
        Data: TFrameData;
    end;

    PFrameSmallMasked = ^TFrameSmallMasked;

    TFrameLarge = packed record
        FinOpCode: Byte;
        MaskedPayLoadLength: Byte;
        ExtPayLoadLength: TUint64Bytes;
        Data: TFrameData;
    end;

    PFrameLarge = ^TFrameLarge;

    TFrameLargeMasked = packed record
        FinOpCode: Byte;
        MaskedPayLoadLength: Byte;
        ExtPayLoadLength: TUint64Bytes;
        Mask: TWSFrameDataMask;
        Data: TFrameData;
    end;

    PFrameLargeMasked = ^TFrameLargeMasked;

    procedure SwapBytes(var A: array of Byte);
    var
        i,j: Integer;
        X: Byte;
    begin
        i := Low(A);
        j := High(A);

        while i < j do begin
            X := A[i];
            A[i] := A[j];
            A[j] := X;
            Inc(i);
            Dec(j);
        end;
    end;

var
    L,i,j : Integer;
    PData : PFrameData;
    DataBytesInBuf : Integer;
begin
  Result := False;

    if State = wsfsNotComplete then begin
        State := wsfsInvalid;
        if Size < 0 then
            Exit;
        if Data = nil then
        Exit;
        PData := PBuf;
        DataBytesInBuf := Size;
        if PData = nil then
            State := wsfsInvalid
        else if StoredBytes + DataBytesInBuf = DataBytes then
            State := wsfsCompleted
        else if (DataBytesInBuf >= 0) and (StoredBytes + DataBytesInBuf < DataBytes) then
            State := wsfsNotComplete
        else
            State := wsfsInvalid;

        if State in [wsfsCompleted, wsfsNotComplete] then begin
            if IsMasked then begin
                j := StoredBytes mod 4;
                for i := 0 to DataBytesInBuf-1 do begin
                    PData^[i] := PData^[i] xor Mask[j];
                    Inc(j);
                if j > 3 then
                    j := 0;
                end;
            end;
            Data.Position := StoredBytes;
            Data.WriteBuffer(PData^[0], DataBytesInBuf);
            StoredBytes := StoredBytes + DataBytesInBuf;
            Result := True;
        end;
    end
    else begin
        Clear;
        if Size < 2 then
            Exit;
        IsFinal := ((PFrameShort( PBuf )^.FinOpCode and $80) = $80);
        case (PFrameShort( PBuf )^.FinOpCode and $F) of
            $0     : Kind := wsfkContinue;
            $1     : Kind := wsfkText;
            $2     : Kind := wsfkBin;
            $3..$7 : Kind := wsfkReserved;
            $8     : Kind := wsfkClose;
            $9     : Kind := wsfkPing;
            $A     : Kind := wsfkPong;
            $B..$F : Kind := wsfkReserved;
            else begin
                    Kind := wsfkUnknown;
                    Exit;
            end;
        end;
        L := (PFrameShort( PBuf )^.MaskedPayLoadLength and $7F);
        IsMasked := ((PFrameShort( PBuf )^.MaskedPayLoadLength and $80) = $80);
        if L <= 0 then begin
            if IsMasked then
                DataBytesInBuf := Size - (SizeOf(TFrameShortMasked) - 1)
            else
                DataBytesInBuf := Size - (SizeOf(TFrameShort) - 1);
            if DataBytesInBuf = DataBytes then begin
                State := wsfsCompleted;
                Result := True;
            end
            else
                State := wsfsInvalid;
            Exit;
        end;
        PData := nil;
        DataBytesInBuf := 0; // due warning

        if IsMasked then begin   // Masked
            if L <= 125 then begin
                Mask := PFrameShortMasked( PBuf )^.Mask;
                PData := @(PFrameShortMasked( PBuf )^.Data);
                DataBytes := L;
                DataBytesInBuf := Size - (SizeOf(TFrameShortMasked) - 1);
            end
            else if L = 126 then begin
                Mask := PFrameSmallMasked( PBuf )^.Mask;
                PData := @(PFrameSmallMasked( PBuf )^.Data);
                SwapBytes( PFrameSmallMasked( PBuf )^.ExtPayLoadLength.B );
                DataBytes := PFrameSmallMasked( PBuf )^.ExtPayLoadLength.W;
                DataBytesInBuf := Size - (SizeOf(TFrameSmallMasked) - 1);
            end
            else if L = 127 then begin
                Mask := PFrameLargeMasked( PBuf )^.Mask;
                PData := @(PFrameLargeMasked( PBuf )^.Data);
                SwapBytes( PFrameLargeMasked( PBuf )^.ExtPayLoadLength.B );
                DataBytes := PFrameLargeMasked( PBuf )^.ExtPayLoadLength.Q;
                DataBytesInBuf := Size - (SizeOf(TFrameLargeMasked) - 1);
            end;

            if PData = nil then
                State := wsfsInvalid
            else if DataBytesInBuf = DataBytes then
                State := wsfsCompleted
            else if (DataBytesInBuf >= 0) and (DataBytesInBuf < DataBytes) then
                State := wsfsNotComplete
            else
                State := wsfsInvalid;

            if State in [wsfsCompleted, wsfsNotComplete] then begin
                j := 0;
                for i := 0 to DataBytesInBuf-1 do begin
                    PData^[i] := PData^[i] xor Mask[j];
                    Inc(j);
                    if j > 3 then
                        j := 0;
                end;
            end;
        end
        else begin
            if L <= 125 then begin
                PData := @(PFrameShort(PBuf )^.Data);
                DataBytes := L;
                DataBytesInBuf := Size - (SizeOf(TFrameShort) - 1);
            end
            else if L = 126 then begin
                PData := @(PFrameSmall(PBuf )^.Data);
                SwapBytes( PFrameSmall(PBuf )^.ExtPayLoadLength.B );
                DataBytes := PFrameSmall(PBuf )^.ExtPayLoadLength.W;
                DataBytesInBuf := Size - (SizeOf(TFrameSmall) - 1);
            end
            else if L = 127 then begin
                PData := @(PFrameLarge(PBuf)^.Data);
                SwapBytes( PFrameLarge(PBuf)^.ExtPayLoadLength.B );
                DataBytes := PFrameLarge(PBuf)^.ExtPayLoadLength.Q;
                DataBytesInBuf := Size - (SizeOf(TFrameLarge) - 1);
            end;

            if PData = nil then
                State := wsfsInvalid
            else if DataBytesInBuf = DataBytes then
                State := wsfsCompleted
            else if (DataBytesInBuf >= 0) and (DataBytesInBuf < DataBytes) then
                State := wsfsNotComplete
            else
                State := wsfsInvalid;
        end;

        if State in [wsfsCompleted, wsfsNotComplete] then begin
            if Data = nil then begin
            {   if Sender <> nil then
              if (Sender.MaxMemStreamSize > 0) and (Sender.StreamTempFolder <> '') then begin
                Data := TLimitedSizeMemStream.Create( Sender.MaxMemStreamSize, IncludeTrailingPathDelimiter( Sender.StreamTempFolder ) + 'WSFRM_', 4, '.tmp' );
              end; }
            if Data = nil then
                Data := TMemoryStream.Create;
            end;
            Data.Size := DataBytes;
            Data.Position := 0;
            Data.WriteBuffer(PData^[0], DataBytesInBuf);
            StoredBytes := DataBytesInBuf;
            Result := True;
        end;
    end;
end;


{  TWebSocketOutgoingFrame  }

constructor TWebSocketOutgoingFrame.Create;
begin
    inherited Create;
    FrameSourceID := nil;
    Kind := wsfkUnknown;
    IsFinal := True;
    Data := nil;
end;

destructor TWebSocketOutgoingFrame.Destroy;
begin
    try
        if Data <> nil then
            Data.Free;
    finally
        inherited Destroy;
    end;
end;

procedure TWebSocketOutgoingFrame.InitFrameData(AFrameSourceID: Pointer; AKind: TWebSocketFrameKind; AData: TStream;
                                                                                ADataBytes: Int64; AIsFinal: Boolean; DoMask: Boolean);
var
    Mask: TWSFrameDataMask;
    Frame: array[0..MaxWSFrameHdrSize-1] of Byte;
    U16: TUint16Bytes;
    U64: TUint64Bytes;
    i, N: Integer;
    MaxDataBytes: Int64;
    FrameStream: TStream;
begin
    FrameSourceID:= AFrameSourceID;
    Kind := AKind;
    IsFinal := AIsFinal;
    if Data <> nil then begin
        Data.Free;
        Data := nil;
    end;

    if AKind = wsfkUnknown then
        Exit;

    if AData <> nil then begin
        MaxDataBytes := AData.Size - AData.Position;
        if MaxDataBytes < 0 then
            MaxDataBytes := 0;
        if ADataBytes > MaxDataBytes then
            ADataBytes := MaxDataBytes;
    end
    else
        ADataBytes := 0;
    if ADataBytes < 0 then
        ADataBytes := 0;
    case AKind of
        wsfkContinue : Frame[0] := $0;
        wsfkText     : Frame[0] := $1;
        wsfkBin      : Frame[0] := $2;
        wsfkClose    : Frame[0] := $8;
        wsfkPing     : Frame[0] := $9;
        wsfkPong     : Frame[0] := $A;
    else
        Exit;
    end;
    if IsFinal then
        Frame[0] := Frame[0] or $80;
    if ADataBytes <= 125 then begin
        Frame[1] := (ADataBytes and $7F);
        N := 2;
    end
    else if ADataBytes <= $7FFF then begin
        Frame[1] := 126;
        N := 2;
        U16.W := ADataBytes;
        for i := High(U16.B) downto Low(U16.B) do begin
            Frame[N] := U16.B[i];
            Inc(N);
        end;
    end
    else begin
        Frame[1] := 127;
        N := 2;
        U64.Q := ADataBytes;
        for i := High(U64.B) downto Low(U64.B) do begin
            Frame[N] := U64.B[i];
            Inc(N);
        end;
    end;

    // only client masks data, not server, no idea why
    if DoMask then begin
        Frame[1] := Frame[1] OR $80;  // set bit to say we mask, then generate random number
        for i := Low(Mask) to High(Mask) do begin
            Mask[i] := Random(256);
            Frame[N] := Mask[i];
            Inc(N);
        end;
    end;

    {  Data := nil;
    {  if Sender <> nil then
    if (Sender.MaxMemStreamSize > 0) and (Sender.StreamTempFolder <> '') then
      if N + ADataBytes > Sender.MaxMemStreamSize then
        Data := TLimitedSizeMemStream.Create( Sender.MaxMemStreamSize, IncludeTrailingPathDelimiter( Sender.StreamTempFolder ) + 'WSFRM_', 4, '.tmp' );
    if Data = nil then  }

    // read frame into stream, then XOR mask it if client
    Data := TMemoryStream.Create;
    Data.Size := N + ADataBytes;
    Data.Position := 0;
    Data.WriteBuffer(Frame[0], N );
    if (ADataBytes > 0) and (AData <> nil) then begin
        if DoMask then begin
            FrameStream := TXORStream.CreateExB(AData, Mask, AData.Position);
            try
                Data.CopyFrom(FrameStream, ADataBytes);
            finally
                FrameStream.Free;
            end;
        end
        else
            Data.CopyFrom(AData, ADataBytes);  // copy no XOR
    end;
end;


{  TSslWebSocketCli  }

constructor TSslWebSocketCli.Create(AOwner : TComponent);
begin
    inherited Create( AOwner );
    FWSPingSecs := 10;
    WSClosing := False;
    WSState := wssHttp;
    LastReceivedDataTickCount := 0;
    LastSentPingTickCount := 0;
    CurrFrame := nil;
    CurrMultiFrame := nil;
    CurrOutgoingFrame := nil;
    OutgoingFrames := TList.Create;
    FOnWSFrameRcvd := nil;
    FOnWSFrameSent := nil;
    MaxMemStreamSize := 0;
    StreamTempFolder := '';
    ClientKey := '';
    HeaderUpgrade := '';
    HeaderConnection := '';
    HeaderSecWebSocketAccept := '';
    HeaderSecWebSocketProtocol := '';
    HeaderAccessControlAllowOrigin := '';
    LocationChangeCount := 0;
    FOnLocationChange := LocationChange;
    FPeriodicTimer := TIcsTimer.Create(Self);
    FPeriodicTimer.Interval := 1000;  // 1 second
    FPeriodicTimer.OnTimer := PeriodicTimerTimer;
    FPeriodicTimer.Enabled := false;
end;

destructor TSslWebSocketCli.Destroy;
begin
    try
        FPeriodicTimer.Enabled := false ;
        FreeAndNil(FPeriodicTimer);
        if CurrFrame <> nil then
            CurrFrame.Free;
        if CurrMultiFrame <> nil then
            CurrMultiFrame.Free;
        if CurrOutgoingFrame <> nil then
            CurrOutgoingFrame.Free;
        ClearOutgoingFrames;
        OutgoingFrames.Free;
    finally
        inherited Destroy;
    end;
end;

procedure TSslWebSocketCli.ClearOutgoingFrames;
var
    Obj: TObject;
    i: Integer;
begin
    try
        for i := 0 to OutgoingFrames.Count-1 do begin
            Obj := OutgoingFrames.Items[i];
            if Obj <> nil then
                Obj.Free;
        end;
    finally
        OutgoingFrames.Clear;
    end;
end;

procedure TSslWebSocketCli.LocationChange(Sender : TObject);
begin
    Inc(LocationChangeCount);
end;

function TSslWebSocketCli.GetRemoteAddr(IncludePort: Boolean): String;
begin
    Result := FDnsResult;
    if IncludePort then
        Result := Result + ':' + FPort;
end;

procedure TSslWebSocketCli.TriggerSessionClosed;
begin
    FPeriodicTimer.Enabled := false ;
    if Assigned(FOnWSDisconnected) then
        FOnWSDisconnected(Self);
    inherited;
end;

procedure TSslWebSocketCli.TriggerBeforeHeaderSend(const Method : String; Headers : TStrings);
var
    i: Integer;
    s: String;
    FoundGET: Boolean;
    B: String;
begin
(*
   When a client starts a WebSocket connection, it sends its part of the
   opening handshake.  The server must parse at least part of this
   handshake in order to obtain the necessary information to generate
   the server part of the handshake.

   The client's opening handshake consists of the following parts.  If
   the server, while reading the handshake, finds that the client did
   not send a handshake that matches the description below (note that as
   per [RFC2616], the order of the header fields is not important),
   including but not limited to any violations of the ABNF grammar
   specified for the components of the handshake, the server MUST stop
   processing the client's handshake and return an HTTP response with an
   appropriate error code (such as 400 Bad Request).

   1.   An HTTP/1.1 or higher GET request, including a "Request-URI"
        [RFC2616] that should be interpreted as a /resource name/
        defined in Section 3 (or an absolute HTTP/HTTPS URI containing
        the /resource name/).

   2.   A |Host| header field containing the server's authority.

   3.   An |Upgrade| header field containing the value "websocket",
        treated as an ASCII case-insensitive value.

   4.   A |Connection| header field that includes the token "Upgrade",
        treated as an ASCII case-insensitive value.

   5.   A |Sec-WebSocket-Key| header field with a base64-encoded (see
        Section 4 of [RFC4648]) value that, when decoded, is 16 bytes in
        length.

   6.   A |Sec-WebSocket-Version| header field, with a value of 13.

   7.   Optionally, an |Origin| header field.  This header field is sent
        by all browser clients.  A connection attempt lacking this
        header field SHOULD NOT be interpreted as coming from a browser
        client.

   8.   Optionally, a |Sec-WebSocket-Protocol| header field, with a list
        of values indicating which protocols the client would like to
        speak, ordered by preference.

   9.   Optionally, a |Sec-WebSocket-Extensions| header field, with a
        list of values indicating which extensions the client would like
        to speak.  The interpretation of this header field is discussed
        in Section 9.1.

The following new header fields can be sent during the handshake from
   the client to the server:

      Sec-WebSocket-Key = base64-value-non-empty
      Sec-WebSocket-Extensions = extension-list
      Sec-WebSocket-Protocol-Client = 1#token
      Sec-WebSocket-Version-Client = version

      base64-value-non-empty = (1*base64-data [ base64-padding ]) |
                                base64-padding
      base64-data      = 4base64-character
      base64-padding   = (2base64-character "==") |
                         (3base64-character "=")
      base64-character = ALPHA | DIGIT | "+" | "/"
      extension-list = 1#extension
      extension = extension-token *( ";" extension-param )
      extension-token = registered-token
      registered-token = token

      extension-param = token [ "=" (token | quoted-string) ]
           ; When using the quoted-string syntax variant, the value
           ; after quoted-string unescaping MUST conform to the
           ; 'token' ABNF.
      NZDIGIT       =  "1" | "2" | "3" | "4" | "5" | "6" |
                       "7" | "8" | "9"
      version = DIGIT | (NZDIGIT DIGIT) |
                ("1" DIGIT DIGIT) | ("2" DIGIT DIGIT)
                ; Limited to 0-255 range, with no leading zeros
*)

    inherited;
    FoundGET := False;
    for i := 0 to Headers.Count-1 do begin
        if IcsTextOnStart('GET ', Trim(Headers.Strings[i])) then begin
            FoundGet := True;
            Break;
        end;
    end;
    if not FoundGet then Exit;

    // GET method = handshake attempt
    WSClosing := False;
    WSState := wssConnecting;
    Randomize;
    SetLength(B, 16);
    for i := 1 to Length(B) do
        B[i] := Char(Random(256));
    ClientKey := Base64Encode(B);

    // we leave only what can / must be there in the header
    for i := Headers.Count-1 downto 0 do begin
        s := Trim(Headers.Strings[i]);
        if IcsTextOnStart('Get ', s) or
           IcsTextOnStart('Host:', s) or
           IcsTextOnStart('Proxy', s) or
           IcsTextOnStart('Cookie:', s) or
           IcsTextOnStart('Authorization:', s) then
                Continue;
        Headers.Delete( i );
    end;

    // add websocket specific records
    Headers.Add('Upgrade: websocket');
    Headers.Add('Connection: Upgrade');
    Headers.Add('Sec-WebSocket-Key: ' + ClientKey);
    Headers.Add('Sec-WebSocket-Version: 13');
    //  Headers.Add('Origin: http://example.com');
    //  Headers.Add('Sec-WebSocket-Protocol: chat, superchat');
end;

procedure TSslWebSocketCli.TriggerHeaderBegin;
begin
    HeaderUpgrade := '';
    HeaderConnection := '';
    HeaderSecWebSocketAccept := '';
    HeaderSecWebSocketProtocol := '';
    HeaderAccessControlAllowOrigin := '';
end;

procedure TSslWebSocketCli.TriggerHeaderFieldData(var AHeaderField, AHeaderData: String);
begin
(*
   5.  If the server chooses to accept the incoming connection, it MUST
       reply with a valid HTTP response indicating the following.

       1.  A Status-Line with a 101 response code as per RFC 2616
           [RFC2616].  Such a response could look like "HTTP/1.1 101
           Switching Protocols".

       2.  An |Upgrade| header field with value "websocket" as per RFC
           2616 [RFC2616].

       3.  A |Connection| header field with value "Upgrade".

       4.  A |Sec-WebSocket-Accept| header field.  The value of this
           header field is constructed by concatenating /key/, defined
           above in step 4 in Section 4.2.2, with the string "258EAFA5-
           E914-47DA-95CA-C5AB0DC85B11", taking the SHA-1 hash of this
           concatenated value to obtain a 20-byte value and base64-
           encoding (see Section 4 of [RFC4648]) this 20-byte hash.

           The ABNF [RFC2616] of this header field is defined as
           follows:

           Sec-WebSocket-Accept     = base64-value-non-empty
           base64-value-non-empty = (1*base64-data [ base64-padding ]) |
                                    base64-padding
           base64-data      = 4base64-character
           base64-padding   = (2base64-character "==") |
                              (3base64-character "=")
           base64-character = ALPHA | DIGIT | "+" | "/"

   NOTE: As an example, if the value of the |Sec-WebSocket-Key| header
   field in the client's handshake were "dGhlIHNhbXBsZSBub25jZQ==", the
   server would append the string "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
   to form the string "dGhlIHNhbXBsZSBub25jZQ==258EAFA5-E914-47DA-95CA-
   C5AB0DC85B11".  The server would then take the SHA-1 hash of this
   string, giving the value 0xb3 0x7a 0x4f 0x2c 0xc0 0x62 0x4f 0x16 0x90
   0xf6 0x46 0x06 0xcf 0x38 0x59 0x45 0xb2 0xbe 0xc4 0xea.  This value
   is then base64-encoded, to give the value
   "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=", which would be returned in the
   |Sec-WebSocket-Accept| header field.

       5.  Optionally, a |Sec-WebSocket-Protocol| header field, with a
           value /subprotocol/ as defined in step 4 in Section 4.2.2.


       6.  Optionally, a |Sec-WebSocket-Extensions| header field, with a
           value /extensions/ as defined in step 4 in Section 4.2.2.  If
           multiple extensions are to be used, they can all be listed in
           a single |Sec-WebSocket-Extensions| header field or split
           between multiple instances of the |Sec-WebSocket-Extensions|
           header field.

   This completes the server's handshake.  If the server finishes these
   steps without aborting the WebSocket handshake, the server considers
   the WebSocket connection to be established and that the WebSocket
   connection is in the OPEN state.  At this point, the server may begin
   sending (and receiving) data.


   The following new header fields can be sent during the handshake from
   the server to the client:

      Sec-WebSocket-Extensions = extension-list
      Sec-WebSocket-Accept     = base64-value-non-empty
      Sec-WebSocket-Protocol-Server = token
      Sec-WebSocket-Version-Server = 1#version
*)

    if SameText(AHeaderField, 'upgrade') then
        HeaderUpgrade := Trim(AHeaderData)
    else if SameText(AHeaderField, 'connection') then
        HeaderConnection := Trim(AHeaderData)
    else if SameText(AHeaderField, 'sec-websocket-accept') then
        HeaderSecWebSocketAccept := Trim(AHeaderData)
    else if SameText(AHeaderField, 'sec-websocket-protocol') then
        HeaderSecWebSocketProtocol := Trim(AHeaderData)
    else if SameText(AHeaderField, 'access-control-allow-origin' ) then
        HeaderAccessControlAllowOrigin := Trim(AHeaderData);
    inherited TriggerHeaderFieldData(AHeaderField, AHeaderData);
end;

function TSslWebSocketCli.GetProtocolPort(const AProtocol: String): String;
begin
    if SameText(AProtocol, 'ws') then
        Result := '80'
    else if SameText(AProtocol, 'wss') then
        Result := '443'
    else
        Result := inherited GetProtocolPort(AProtocol);
end;

function TSslWebSocketCli.IsSSLProtocol(const AProtocol: String): Boolean;
begin
    Result := SameText(AProtocol, 'wss') or inherited IsSSLProtocol(AProtocol);
end;

function TSslWebSocketCli.IsWsProtocol(const AProtocol: String): Boolean;
begin
    Result := SameText(AProtocol, 'ws') or SameText(AProtocol, 'wss');
end;

function TSslWebSocketCli.IsKnownProtocol(const AProtocol: String): Boolean;
begin
  Result := IsWsProtocol(AProtocol) or inherited IsKnownProtocol(AProtocol);
end;

function TSslWebSocketCli.IsWsProtocolURL(const AURL: String): Boolean;
begin
  Result := (CompareText(Copy(AURL, 1, 5), 'ws://') = 0) or (CompareText(Copy(AURL, 1, 6), 'wss://') = 0);
end;

function TSslWebSocketCli.IsKnownProtocolURL(const AURL: String): Boolean;
begin
  Result := IsWsProtocolURL(URL) or inherited IsKnownProtocolURL(AURL);
end;

function TSslWebSocketCli.WSConnect: Boolean;
var
    ServerKey : String;
    s : AnsiString;
begin
    Result := False;
    RequestVer := '1.1';
    HeaderUpgrade := '';
    HeaderConnection := '';
    HeaderSecWebSocketAccept := '';
    HeaderSecWebSocketProtocol := '';
    HeaderAccessControlAllowOrigin := '';
    FWSFrameCounter := 0;
    if (DebugLevel >= DebugConn) then
        LogEvent('WebSocket: Connecting to: ' + URL);
//    WSState := wssConnecting;  { in case server returns data quickly }
    Get;  // sync request
    if (StatusCode = 101) then begin
        if SameText(HeaderUpgrade, 'websocket') and SameText(HeaderConnection, 'Upgrade') then begin
            s := AnsiString(ClientKey + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11');
            ServerKey := Base64Encode(String(SHA1ofStr(s)));
            Result := (HeaderSecWebSocketAccept = ServerKey);
            if (NOT Result) and (DebugLevel >= DebugConn) then
                LogEvent('WebSocket: Server Key Comparison Failed');
        end
        else if (DebugLevel >= DebugConn) then
            LogEvent('WebSocket: Failed to Upgrade to WebSocket Protocol');
        end
        else if (DebugLevel >= DebugConn) then
            LogEvent('WebSocket: Failed to Connect: ' + LastResponse);
        if Result then begin
            if (DebugLevel >= DebugConn) then
                LogEvent('WebSocket: Connected OK');
            WSState := wssReady;  { in case server returns data quickly }
            LastReceivedDataTickCount := IcsGetTickCount64;
            LastSentPingTickCount := 0;
            if (FWSPingSecs > 0) and (FWSPingSecs < 5) then
                FWSPingSecs := 5;
            FPeriodicTimer.Enabled := true;
        end
    else
        WSState := wssHttp;
    if Assigned(FOnWSConnected) then
        FOnWSConnected(Self);
end;

procedure TSslWebSocketCli.WSClose(CloseReason: TWebSocketCloseReason; ADescription: String);
var
    EC: TUint16Bytes;
    B: TBytes;
begin
    FPeriodicTimer.Enabled := false;
    case CloseReason of
        wscrNormalClosure : EC.W := 1000;    // 1000 indicates a normal closure, meaning that the purpose for
                                             // which the connection was established has been fulfilled.
        wscrGoingAway : EC.W := 1001;        // 1001 indicates that an endpoint is "going away", such as a server
                                             // going down or a browser having navigated away from a page.
        wscrProtocolError : EC.W := 1002;    // 1002 indicates that an endpoint is terminating the connection due
                                             // to a protocol error.
        wscrUnsupportedData : EC.W := 1003;  // 1003 indicates that an endpoint is terminating the connection
                                             // because it has received a type of data it cannot accept (e.g., an
                                             // endpoint that understands only text data MAY send this if it
                                             // receives a binary message).
{
   1004     Reserved.  The specific meaning might be defined in the future.
   1005     1005 is a reserved value and MUST NOT be set as a status code in a
      Close control frame by an endpoint.  It is designated for use in
      applications expecting a status code to indicate that no status
      code was actually present.
   1006     1006 is a reserved value and MUST NOT be set as a status code in a
      Close control frame by an endpoint.  It is designated for use in
      applications expecting a status code to indicate that the
      connection was closed abnormally, e.g., without sending or
      receiving a Close control frame.
}
        wscrInvalidFramePayloadData : EC.W := 1007;   // 1007 indicates that an endpoint is terminating the connection
                                                      // because it has received data within a message that was not
                                                      // consistent with the type of the message (e.g., non-UTF-8 [RFC3629]
                                                      // data within a text message).
        wscrPolicyViolation : EC.W := 1008;     // 1008 indicates that an endpoint is terminating the connection
                                                // because it has received a message that violates its policy.  This
                                                // is a generic status code that can be returned when there is no
                                                // other more suitable status code (e.g., 1003 or 1009) or if there
                                                // is a need to hide specific details about the policy.
        wscrMessageTooBig : EC.W := 1009;       // 1009 indicates that an endpoint is terminating the connection
                                                // because it has received a message that is too big for it to process.
        wscrMandatoryExt : EC.W := 1010;        // 1010 indicates that an endpoint (client) is terminating the
                                                // connection because it has expected the server to negotiate one or
                                                // more extension, but the server didn't return them in the response
                                                // message of the WebSocket handshake.  The list of extensions that
                                                // are needed SHOULD appear in the /reason/ part of the Close frame.
                                                // Note that this status code is not used by the server, because it
                                                // can fail the WebSocket handshake instead.
        wscrInternalServerError : EC.W := 1011; // 1011 indicates that a server is terminating the connection because
                                                // it encountered an unexpected condition that prevented it from
                                                // fulfilling the request.
{
   1015    1015 is a reserved value and MUST NOT be set as a status code in a
      Close control frame by an endpoint.  It is designated for use in
      applications expecting a status code to indicate that the
      connection was closed due to a failure to perform a TLS handshake
      (e.g., the server certificate can't be verified).
}
        else
        EC.W := 1000;
    end;

    B := StringToUtf8TB( 'XX' + ADescription );
  // I need to write EC.B[1] and EC.B[0] bytes at the beginning
  // character X is one byte in utf 8, so XX makes room for 2 bytes
    B[0] := EC.B[1];
    B[1] := EC.B[0];
    if (DebugLevel >= DebugConn) then
        LogEvent('WebSocket : Closing Connection');
    WSSendFrameBytes(nil, wsfkClose, B);
    WSClosing := True;
end;

procedure TSslWebSocketCli.SocketDataSent(Sender: TObject; ErrCode: Word);
var
    TmpFrame: TWebSocketOutgoingFrame;
    Len: Integer;
    TmpDumpFrame: TWebSocketReceivedFrame;
label
    RetrySendFrame;
begin
    if not ((State = httpReady) and (WSState <> wssHttp)) then begin
        inherited SocketDataSent(Sender, ErrCode);
        Exit;
    end;

RetrySendFrame :
    if CurrOutgoingFrame = nil then begin
        while OutgoingFrames.Count > 0 do begin
            TmpFrame := OutgoingFrames.Items[0];
            OutgoingFrames.Delete(0);
            if (TmpFrame.Kind <> wsfkUnknown) and (TmpFrame.Data <> nil) then begin
                CurrOutgoingFrame := TmpFrame;
                CurrOutgoingFrame.Data.Position := 0;
                Break;
            end;
        end;
    end;
    if CurrOutgoingFrame = nil then
        Exit;
    if Length(FSendBuffer) <= 8192 then
        SetLength(FSendBuffer, 8192);
    Len := CurrOutgoingFrame.Data.Read(FSendBuffer[0], Length(FSendBuffer));

 // no frame to send
    if Len <= 0 then begin
        TmpFrame := CurrOutgoingFrame;
        CurrOutgoingFrame := nil;
        if (DebugLevel >= DebugBody) and (TmpFrame.Data is TMemoryStream) then begin
            TmpDumpFrame := TWebSocketReceivedFrame.Create;
            try
                TmpDumpFrame.Parse((TmpFrame.Data as TMemoryStream).Memory, TmpFrame.Data.Size );
                LogEvent(WSDumpFrame('Sent', TmpDumpFrame));
            finally
                TmpDumpFrame.Free;
            end;
        end;
        if Assigned(FOnWSFrameSent) then
            FOnWSFrameSent(Self, TmpFrame);
        if TmpFrame <> nil then
            TmpFrame.Free;
       // the current frame has been sent, so we need to check if there is something else to send
        goto RetrySendFrame;
    end
 // send frame, once sent event will tigger next frame in queue
    else begin
        FSentCount := FSentCount + Len;
        FCtrlSocket.Send( @FSendBuffer[0], Len );
        if (DebugLevel >= DebugHdr) then
            LogEvent('WebSocket: Sending ' + GetWSFrameKind(CurrOutgoingFrame.Kind) + ', ' + IntToStr( Len ) + ' bytes');
    end;
end;

procedure TSslWebSocketCli.ProcessReceivedFrame(AFrame: TWebSocketReceivedFrame);
var
    APacket: String;
    Bindata: TBytes;
begin
    APacket := '';
    try
        case AFrame.Kind of
            wsfkPing: begin                     { server is saying hello, reply with Pong }
                if AFrame.Data <> nil then
                    AFrame.Data.Position := 0;
                WSSendFrame(nil, wsfkPong, AFrame.Data);
            end;
            wsfkPong: begin                     { server responded to our Ping, only reset timers }
                //
            end;
            wsfkClose: begin
                if not WSClosing then begin
                    if AFrame.Data <> nil then
                        AFrame.Data.Position := 0;
                    WSSendFrame(nil, wsfkClose, AFrame.Data);
                end;
                WSClosing := True;
                // when this happens, the server should close the connection itself
                // FCtrlSocket.CloseDelayed;
            end;
            wsfkBin: begin
                if AFrame.Data <> nil then begin
                    AFrame.Data.Position := 0;
                    SetLength(BinData, AFrame.Data.Size );
                    AFrame.Data.Read(BinData[0], Length(BinData));
                    IcsMoveTBytesToString(BinData, 0, APacket, 1, AFrame.Data.Size);
                    AFrame.Data.Position := 0;
                end;
            end;
            wsfkText: begin
                if AFrame.Data <> nil then begin
                    APacket := IcsHtmlToStr(AFrame.Data, CP_UTF8, False); // get packet as simple string
                    AFrame.Data.Position := 0;
                end;
            end;
        end;
        FWSFrameCounter := FWSFrameCounter + 1;
        if Assigned( FOnWSFrameRcvd ) then
        try
            FOnWSFrameRcvd(Self, APacket, AFrame);
        except
        end;
    finally
        if AFrame <> nil then
            AFrame.Free;
    end;
end;

procedure TSslWebSocketCli.SocketDataAvailable(Sender: TObject; ErrCode: Word);
var
    Len: Integer;
begin
    if (NOT (State = httpReady)) OR (WSState <> wssReady) then begin
        inherited SocketDataAvailable(Sender, ErrCode);
     //   if FReceiveLen >= 2 then
     //       LogEvent('WebSocket: Inherited Recv Raw: ' + IcsTBytesToString(FReceiveBuffer, FReceiveLen));  // !!! TEMP
        if (WSState <> wssConnecting) then
            Exit;
    end;
    if Length(FReceiveBuffer) < 8192 then
        SetLength( FReceiveBuffer, 8192 );
    if FReceiveLen > 0 then begin { content that arrived with header already in FReceiveBuffer }
        Len := FReceiveLen;       { beware frame received event will be triggered before connected event }
        FReceiveLen := 0;
    end
    else
        Len := FCtrlSocket.Receive(@FReceiveBuffer[0], Length(FReceiveBuffer));
    if (DebugLevel >= DebugHdr) then begin
        if (Len < 0) and (FCtrlSocket.LastError <> WSAEWOULDBLOCK) then
            LogEvent('WebSocket: Error - ' + WSocketErrorDesc(FCtrlSocket.LastError));
     //   if Len >= 2 then
     //       LogEvent('WebSocket: Recv Raw: ' + IcsTBytesToString(FReceiveBuffer, Len));  // !!! TEMP
    end;
    if Len <= 0 then
        Exit;

  // find frame
    LastReceivedDataTickCount := IcsGetTickCount64;
    LastSentPingTickCount := 0;
    if CurrFrame = nil then
       CurrFrame := TWebSocketReceivedFrame.Create;
    if not CurrFrame.Parse(@FReceiveBuffer[0], Len ) then begin
        if (DebugLevel >= DebugBody) then
            LogEvent(WSDumpFrame('Received Invalid', CurrFrame));
        WSClose(wscrUnsupportedData, 'Unknown frame structure');
        Exit;
    end;
    if CurrFrame.IsMasked then begin // the server must not send a masked frame
        WSClose( wscrProtocolError, 'Frame is masked' );
        Exit;
    end;
    if CurrFrame.State = wsfsCompleted then begin
        if CurrFrame.Kind = wsfkContinue then begin
            if CurrMultiFrame <> nil then begin
                CurrMultiFrame.IsFinal := CurrFrame.IsFinal;
                if CurrFrame.Data <> nil then begin
                    if CurrMultiFrame.Data = nil then begin
                        CurrMultiFrame.Data := CurrFrame.Data;
                        CurrFrame.Data := nil;
                        CurrMultiFrame.DataBytes := CurrFrame.DataBytes;
                        CurrMultiFrame.StoredBytes := CurrFrame.StoredBytes;
                    end
                    else begin
                        CurrMultiFrame.Data.Size := CurrMultiFrame.DataBytes + CurrFrame.DataBytes;
                        CurrMultiFrame.Data.Position := CurrMultiFrame.DataBytes;
                        CurrMultiFrame.Data.CopyFrom( CurrFrame.Data, 0 );
                        CurrMultiFrame.DataBytes := CurrMultiFrame.DataBytes + CurrFrame.DataBytes;
                        CurrMultiFrame.StoredBytes := CurrMultiFrame.StoredBytes + CurrFrame.StoredBytes;
                    end;
                end;

                if CurrMultiFrame.IsFinal then begin
                    if (DebugLevel >= DebugBody) then
                        LogEvent(WSDumpFrame( 'Received Multiframe', CurrMultiFrame));
                    ProcessReceivedFrame( CurrMultiFrame );
                    CurrMultiFrame := nil;
                end;
            end;

      // if there is nothing to connect it to, it is simply discarded
            CurrFrame.Free;
            CurrFrame := nil;
            Exit;
        end
        else if not CurrFrame.IsFinal then
        begin
          // it's the first frame from the message (because <> CurrFrame.Kind = wsfc Continue)
          // and it's not final, so if some multiframe message is being processed
          // it will be discarded
            if CurrMultiFrame <> nil then
                CurrMultiFrame.Free;
          // and the current frame becomes a multiframe
            CurrMultiFrame := CurrFrame;
            CurrFrame := nil;
            Exit;
        end;

     // if it comes here, the frame is final
        if (DebugLevel >= DebugBody) then
                LogEvent(WSDumpFrame( 'Received Singleframe', CurrFrame));
        ProcessReceivedFrame( CurrFrame );
        CurrFrame := nil;
    end;
end;

function TSslWebSocketCli.IsWSConnected: Boolean;
begin
    Result := (State = httpReady) and (FCtrlSocket.State = wsConnected) and (WSState <> wssHttp);
end;

procedure TSslWebSocketCli.ProcessPeriodicTasks;
var
    Data: TBytes;
begin
    if (OutgoingFrames.Count <= 0) and (CurrOutgoingFrame = nil) and
           (FWSPingSecs >= 5) and (IcsElapsedSecs64( LastReceivedDataTickCount) > FWSPingSecs) then begin
      // no sending is running and for x seconds nothing arrived, not even a single Byte
        if LastSentPingTickCount = 0 then begin
            SetLength( Data, 0 );
            WSSendPing( Data );
            LastSentPingTickCount := IcsGetTickCount64;
        end
        else if IcsElapsedSecs64( LastSentPingTickCount) > 10 then begin
          // 10 seconds after ping no response
            WSClose( wscrProtocolError, 'No response to ping' );
            FCtrlSocket.CloseDelayed;
        end;
    end;

    if (OutgoingFrames.Count > 0) and (CurrOutgoingFrame = nil) then
        SocketDataSent(FCtrlSocket, 0);
end;

procedure TSslWebSocketCli.PeriodicTimerTimer(Sender: TObject);
begin
    FPeriodicTimer.Enabled := false;
    try
        try
            ProcessPeriodicTasks;
        except
      //
        end;
    finally
        FPeriodicTimer.Enabled := true;
    end;
end;

(*
A fragmented message consists of a single frame with the FIN bit
clear and an opcode other than 0, followed by zero or more frames
with the FIN bit clear and the opcode set to 0, and terminated by
a single frame with the FIN bit set and an opcode of 0.  A
fragmented message is conceptually equivalent to a single larger
message whose payload is equal to the concatenation of the
payloads of the fragments in order; however, in the presence of
extensions, this may not hold true as the extension defines the
interpretation of the "Extension data" present.  For instance,
"Extension data" may only be present at the beginning of the first
fragment and apply to subsequent fragments, or there may be
"Extension data" present in each of the fragments that applies
only to that particular fragment.  In the absence of "Extension
data", the following example demonstrates how fragmentation works.

EXAMPLE: For a text message sent as three fragments, the first
fragment would have an opcode of 0x1 and a FIN bit clear, the
second fragment would have an opcode of 0x0 and a FIN bit clear,
and the third fragment would have an opcode of 0x0 and a FIN bit
that is set.
*)

procedure TSslWebSocketCli.WSSendFrame(AFrameSourceID: Pointer; AFrameKind: TWebSocketFrameKind; const AStream: TStream);
var
    AFrame: TWebSocketOutgoingFrame;
    i: Integer;
    RestSize: Int64;
    FrameSize: Int64;
    AKind: TWebSocketFrameKind;
begin
    FWSFrameCounter := FWSFrameCounter + 1;
    if WSClosing then
        Exit;

    if AFrameKind = wsfkUnknown then
        Exit;

  // A Pong frame sent in response to a Ping frame must have identical
  // "Application data" as found in the message body of the Ping frame
  // being replied to.

  // If an endpoint receives a Ping frame and has not yet sent Pong
  // frame(s) in response to previous Ping frame(s), the endpoint MAY
  // elect to send a Pong frame for only the most recently processed Ping frame.

  // An endpoint MUST be capable of handling control frames in the
  // middle of a fragmented message.

  // A sender MAY create fragments of any size for non-control messages.

  // Clients and servers MUST support receiving both fragmented and unfragmented messages.

  // As control frames cannot be fragmented, an intermediary MUST NOT
  // attempt to change the fragmentation of a control frame.
    if AStream <> nil then
        RestSize := AStream.Size - AStream.Position
    else
        RestSize := 0;
    if AFrameKind in [wsfkPing, wsfkPong, wsfkClose] then begin
    // pong will be deleted from the queue (according to the RFC, just reply to the last ping)
    // so pongs that haven't left yet don't have to leave either
    // and I don't have to send 100x pings when some is already in the queue
        for i := OutgoingFrames.Count-1 downto 0 do begin
            AFrame := OutgoingFrames.Items[i];
            if (AFrame.Kind = AFrameKind) and AFrame.IsFinal then begin
                OutgoingFrames.Delete(i);
                AFrame.Free;
            end;
        end;
        AFrame := TWebSocketOutgoingFrame.Create;
        AFrame.InitFrameData(AFrameSourceID, AFrameKind, AStream, RestSize, True, True );

        // the control frame must be sent ASAP, and must not be split into multiple frames
        OutgoingFrames.Insert( 0, AFrame );
    end
    else begin
    // if necessary, the stream is split into several frames, because:
    // A fragmented message is conceptually equivalent to a single larger
    // message whose payload is equal to the concatenation of the
    // payloads of the fragments in order
        AKind := AFrameKind;
        repeat
          FrameSize := RestSize;
          if FrameSize > MaxWSFrameSize then
                FrameSize := MaxWSFrameSize;
          RestSize := RestSize - FrameSize;
          AFrame := TWebSocketOutgoingFrame.Create;
          AFrame.InitFrameData(AFrameSourceID, AKind, AStream, FrameSize, RestSize <= 0, True );
          OutgoingFrames.Add( AFrame );
          AKind := wsfkContinue;
        until RestSize <= 0;
    end;
    if CurrOutgoingFrame = nil then
        SocketDataSent( FCtrlSocket, 0 );
end;

procedure TSslWebSocketCli.WSSendFrameBytes(AFrameSourceID: Pointer; AFrameKind: TWebSocketFrameKind; const AData: TBytes);
var
    AStream: TStream;
begin
    AStream := nil;
    try
        if Length( AData ) > 0 then begin
            AStream := TMemoryReadStream.Create(AData, Length(AData));
            AStream.Position := 0;
        end;
        WSSendFrame(AFrameSourceID, AFrameKind, AStream);
    finally
        if AStream <> nil then
            AStream.Free;
    end;
end;

procedure TSslWebSocketCli.WSSendFrameMemory(AFrameSourceID: Pointer; AFrameKind: TWebSocketFrameKind; const APData: Pointer; ADataBytes: Integer);
var
    AStream : TStream;
begin
    AStream := nil;
    try
        if (APData <> nil) and (ADataBytes > 0) then begin
            AStream := TMemoryReadStream.Create( APData, ADataBytes );
            AStream.Position := 0;
        end;
        WSSendFrame( AFrameSourceID, AFrameKind, AStream );
    finally
        if AStream <> nil then
            AStream.Free;
    end;
end;

procedure TSslWebSocketCli.WSSendPing(const AData: TBytes);
begin
    WSSendFrameBytes(nil, wsfkPing, AData);
end;

procedure TSslWebSocketCli.WSSendPong(const AData: TBytes);
begin
    WSSendFrameBytes(nil, wsfkPong, AData);
end;

procedure TSslWebSocketCli.WSSendText(AFrameSourceID: Pointer; AText: String);
begin
    WSSendFrameBytes(AFrameSourceID, wsfkText, StringToUtf8TB(AText));
end;

procedure TSslWebSocketCli.WSSendBinaryStream(AFrameSourceID: Pointer; AStream: TStream);
begin
    WSSendFrame(AFrameSourceID, wsfkBin, AStream);
end;


end.
