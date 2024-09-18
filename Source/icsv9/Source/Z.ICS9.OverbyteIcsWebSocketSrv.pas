{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, including work from Jaroslav Kulísek.
Description:  Websocket server protocol.
Creation:     Jan 2023
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
               The sample demo for this component is OverbyteIcsSslMultiWebServ.dpr.
Aug 08, 2023 V9.0  Updated version to major release 9.



WebSocket is a full duplex TCP protocol for web servers to support interactive web pages,
typically dynamic updating such as chat sessions, spell checkers as you type, search
hints, etc.  WebSocket extends the HTTP protocol and can be carried through HTTP proxies
using the same ports as HTTP. The WebSocket protocol includes ping/pong keep alive so
long lived connections are not dropped.

This component supports the WebSocket protocol by overriding the TSslHttpAppSrv application
web server client class THttpAppSrvConnection (or THttpConnection) with a new class
THttpWSSrvConn.  The new class has new WSxx events, methods and properties supporting the
WebSocket protocol as illustrated in the sample OverbyteIcsSslMultiWebServ.dpr which
includes simple echo servers and a chat server.

Note this unit shares some code with the WebSocket client unit OverbyteIcsWebSocketCli.pas
and most of the methods and events are identical since WebSocket is a two-way protocol,
so you send messages with WSSendText or WSSendBinaryStream in client and server, and
receive messages in the OnWSFrameRcvd event.  The client is responsible for opening the
WebSocket with WSConnect and a URL which triggers the server OnWSHandshake event where
the page is checked for validity, either server or client can call WSClose to send the
session.

To use THttpWSSrvConn, in the Create event set the various WebSocket event handlers;
OnWSHandshake is triggered when an upgrade to WebSocket request is made allowing the
program to check the page name for validity and accept the upgrade; OnWSReady is
triggered when the WebSocket is ready for two-way traffic; OnWSFrameRcvd is triggered
when a message frame is received, including keep-alive pings which are generally
ignored; OnWSFrameSent is triggered when a message packet is sent and can usually
be ignored; OnWSPingTimer is triggered if WSPingEnabled=true before the ping is sent
and may be used to regularly push data to the client; OnWSDisconnected is triggered
when a close frame is received or the server closes the connection.  The server can
send a simple text frame using the WSSendText method, binary using WSSendBinary or
from a stream using WSSendBinaryStream.

}


{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsWebSocketSrv;
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
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocketS,
    Z.ICS9.Ics.Fmx.OverbyteIcsHttpSrv,
    Z.ICS9.Ics.Fmx.OverbyteIcsHttpAppServer,
    Z.ICS9.Ics.Fmx.OverbyteIcsWebSocketCli,
{$ELSE}
    Z.ICS9.OverbyteIcsWndControl,
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsHttpProt,
    Z.ICS9.OverbyteIcsSslHttpRest,
    Z.ICS9.OverbyteIcsWSocketS,
    Z.ICS9.OverbyteIcsHttpSrv,
    Z.ICS9.OverbyteIcsHttpAppServer,
    Z.ICS9.OverbyteIcsWebSocketCli,
{$ENDIF FMX}
    Z.ICS9.OverbyteIcsCharsetUtils,
    Z.ICS9.OverbyteIcsSha1,
    Z.ICS9.OverbyteIcsLogger,     { for TLogOption }
    Z.ICS9.OverbyteIcsTicks64;    { V8.71 }

type
    THttpWSSrvConn = class;
    TWSLogEvent = procedure (Sender: TObject; const Msg: string) of object;
    TWSHandshakeEvent = procedure(Client: THttpWSSrvConn; var OK: Boolean; var WelcomeMsg: String) of object;
    TWSFrameSrvRcvdEvent = procedure(Client: THttpWSSrvConn; const APacket: String; var AFrame: TWebSocketReceivedFrame) of object;
    TWSFrameSrvSentEvent = procedure(Client: THttpWSSrvConn; var AFrame: TWebSocketOutgoingFrame) of object;
    TWSReadyEvent = procedure(Client: THttpWSSrvConn) of object;
    TWSPingTimerEvent = procedure(Client: THttpWSSrvConn) of object;

    THttpWSSrvConn = class(THttpAppSrvConnection)
    public
        WSClient: Boolean;                       { a WebSocket client is connected }
        WSPendingReady: Boolean;                 { should ready event be triggered after handshaeke }
        WSClosing : Boolean;
        WSReqPage: String;
        WSReqParams: String;
        WSReqKey: String;
        WSReqVersion: String;
        WSReqProtocol: String;
        WSReqExtensions: String;
        DebugLevel: THttpDebugLevel;
        SendBuffer: TBytes;
        RcvdBuffer: TBytes;
        LastReceivedDataTickCount: Int64;  // if nothing arrives for a long time, try ping
        LastSentPingTickCount: Int64;      // and if nothing even then, the connection is dropped
        CurrFrame : TWebSocketReceivedFrame;
        CurrMultiFrame : TWebSocketReceivedFrame;
        CurrOutgoingFrame : TWebSocketOutgoingFrame;
        OutgoingFrames : TList;     // send frame queue
        MaxMemStreamSize : Int64;
        StreamTempFolder : String;   // if is set, then TLimitedSizeMemStream is used
        WSFrameCounter: Integer;
        WSPingEnabled: Boolean;
        WSPingSecs: Integer;
        FOnWSLogEvent : TWSLogEvent;
        FOnWSHandshake: TWSHandshakeEvent;
        FOnWSDisconnected: TNotifyEvent;
        FOnWSFrameRcvd: TWSFrameSrvRcvdEvent;
        FOnWSFrameSent: TWSFrameSrvSentEvent;
        FOnWSReady: TWSReadyEvent;
        FOnWSPingTimer: TWSPingTimerEvent;
        FPeriodicTimer: TIcsTimer;                              { send regular pings }
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure ConnectionDataSent(Sender : TObject; ErrCode : Word); override;
        procedure ConnectionDataAvailable(Sender: TObject; ErrCode: Word); override;
        procedure TriggerHttpRequestDone; override;
        procedure TriggerGetDocument(var Flags : THttpGetFlag); override;
        procedure BgExceptionEvent (Sender : TObject; E : Exception; var CanClose : Boolean);
        procedure WSLogEvent(const Msg: String);
        procedure WSClose(CloseReason: TWebSocketCloseReason; ADescription: String);
        procedure WSSendFrame(AFrameSourceID: Pointer; AFrameKind: TWebSocketFrameKind; const AStream: TStream);
        procedure WSSendFrameBytes(AFrameSourceID: Pointer; AFrameKind: TWebSocketFrameKind; const AData: TBytes);
        procedure WSSendFrameMemory( AFrameSourceID: Pointer; AFrameKind: TWebSocketFrameKind; const APData: Pointer; ADataBytes: Integer);
        procedure WSSendPing(const AData: TBytes);
        procedure WSSendPong(const AData: TBytes);
        procedure WSSendText(AFrameSourceID: Pointer; AText: String);
        procedure WSSendBinaryStream(AFrameSourceID: Pointer; AStream: TStream);
        procedure Answer500(const Reason: String);
        procedure ClearOutgoingFrames;
        procedure ProcessReceivedFrame(AFrame: TWebSocketReceivedFrame);
        procedure PeriodicTimerTimer(Sender: TObject);
        procedure ProcessPeriodicTasks;
    published
        property OnWSLogEvent : TWSLogEvent read FOnWSLogEvent write FOnWSLogEvent;
        property OnWSHandshake : TWSHandshakeEvent read FOnWSHandshake write FOnWSHandshake;
        property OnWSDisconnected : TNotifyEvent read FOnWSDisconnected write FOnWSDisconnected;
        property OnWSFrameRcvd : TWSFrameSrvRcvdEvent read FOnWSFrameRcvd write FOnWSFrameRcvd;
        property OnWSFrameSent : TWSFrameSrvSentEvent read FOnWSFrameSent write FOnWSFrameSent;
        property OnWSReady: TWSReadyEvent read FOnWSReady write FOnWSReady;
        property OnWSPingTimer: TWSPingTimerEvent read FOnWSPingTimer write FOnWSPingTimer;
    end ;



implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THttpWSSrvConn.Create(AOwner: TComponent);
begin
    inherited;
    WSClient := False;      // not using WebSocket protocol, skip most code here }
    WSPendingReady := False;
    WSPingEnabled := False;
    WSPingSecs := 15;    // our client is 10s, so slower
    { rest of WebSocket variables are initialised after handshake since not needed otherwise }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THttpWSSrvConn.Destroy;
begin
    try
        if WSClient then begin
            if Assigned(FPeriodicTimer) then begin
                FPeriodicTimer.Enabled := false ;
                FreeAndNil(FPeriodicTimer);
            end;
            if CurrFrame <> nil then
                CurrFrame.Free;
            if CurrMultiFrame <> nil then
                CurrMultiFrame.Free;
            if CurrOutgoingFrame <> nil then
                CurrOutgoingFrame.Free;
            ClearOutgoingFrames;
            OutgoingFrames.Free;
        end;
    finally
        inherited;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.WSLogEvent(const Msg: String);
begin
    if Assigned(FOnWSLogEvent) then
        FOnWSLogEvent(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.ClearOutgoingFrames;
var
    Obj : TObject;
    i : Integer;
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


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.Answer500(const Reason: String);
var
    Body : String;
begin
    Body := '501 ' + Reason;
    SendHeader('HTTP/1.1 ' + Body + IcsCRLF +
               'Content-Type: text/plain' + IcsCRLF +
               'Content-Length: ' + IntToStr(Length(Body)) + IcsCRLF +
               IcsCRLF);
    if FSendType = httpSendHead then
        Send(nil, 0)
    else
        SendStr(Body);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.TriggerGetDocument(var Flags : THttpGetFlag);
var
    OK: Boolean;
    Resp, WelcomeMsg: String;
begin
    if NOT WSClient then begin
      // see if swapping to WebSocket connection mode, Firefox sends Connection: keep-alive, Upgrade
        if (Lowercase(RequestUpgrade) = 'websocket') and (Pos('upgrade', Lowercase(RequestConnection)) > 0)  then begin
            Flags := hgWillSendMySelf ;

          // look for key
            WSReqKey := Trim(RequestHeader.Values['Sec-Websocket-Key']);
            if WSReqKey = '' then begin
                KeepAlive := False ;
                Answer500('Invalid Sec-Websocket-Key Header');
                CloseDelayed;
                Exit;
            end;

        { keep handshake information so OnWSHandshake can decide whether to accept the connection }
            WSReqPage := Self.Path;
            WSReqParams := Self.Params;
            WSReqVersion := Trim(RequestHeader.Values['Sec-WebSocket-Version']);
            WSReqProtocol := Trim(RequestHeader.Values['Sec-WebSocket-Protocol']);
            WSReqExtensions := Trim(RequestHeader.Values['Sec-WebSocket-Extensions']);
            OK := True;
            WelcomeMsg := '';
            if Assigned(FOnWSHandshake) then  { user can check if page allowed to support websockets }
                FOnWSHandshake(Self, OK, WelcomeMsg);
            if NOT OK then begin
                KeepAlive := False ;
                Answer500('Websockets Not Available');
                CloseDelayed;
                Exit;
            end;

        { tell client websocket is available, prepare all internal variables }
            WSClient := True;
            LineMode := False;  // web server uses LineMode for headers, turn it off for binary frames
            LastReceivedDataTickCount := IcsGetTickCount64;
            LastSentPingTickCount := 0;
            CurrFrame := nil;
            CurrMultiFrame := nil;
            CurrOutgoingFrame := nil;
            OutgoingFrames := TList.Create;
            MaxMemStreamSize := 0;
            StreamTempFolder := '';
            if WSPingEnabled and (WSPingSecs >= 5) then begin
                FPeriodicTimer := TIcsTimer.Create(Self);
                FPeriodicTimer.Interval := 1000;  // 1 second
                FPeriodicTimer.OnTimer := PeriodicTimerTimer;
                FPeriodicTimer.Enabled := True;
            end;
            FAnswerStatus := 200;  // for logging
            Resp := 'HTTP/1.1 101 Switching to WebSocket Protocol' + IcsCRLF +
              'Upgrade: WebSocket' + IcsCRLF +
              'Connection: Upgrade' + IcsCRLF;
            if WSReqProtocol <> '' then
                Resp := Resp + 'Sec-WebSocket-Protocol: ' + WSReqProtocol; { Should really choose one! }
          { hash challenge key we received with GUID and return it to client }
            Resp := Resp + 'Sec-WebSocket-Accept: ' + String(Base64Encode(SHA1ofStr(AnsiString(WSReqKey) +
                                                                '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'))) + IcsCRLF + IcsCRLF;
            KeepAlive := True ;
            SendText(Resp);    // send 101 response header

        // risk of sending now since HTTP request has not yet been completed, better in FOnWSReady event
            if WelcomeMsg <> '' then begin
                WelcomeMsg := IcsTransChar(WelcomeMsg, IcsCR, IcsSpace);  // CRLF in first message may confuse reveive header handler
                WelcomeMsg := IcsTransChar(WelcomeMsg, IcsLF, IcsSpace);
                WSSendText(Nil, WelcomeMsg);  // send first websocket message
                ConnectionDataSent(Self, 0 );
            end;
            WSPendingReady := True;   // trigger event once message is sent
            PostMessage(Handle, FMsg_WM_HTTP_DONE, 0, 0);
            Exit;
        end;
    end
    else begin
        Exit;  { ignore requests in websocket mode }
    end;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.TriggerHttpRequestDone;
begin
    if WSClient then begin
        if WSPendingReady then begin
            WSPendingReady := False;
            if Assigned(FOnWSReady) then
                FOnWSReady(Self);         // event may be used to start pushing messages to client
        end;
    end;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.BgExceptionEvent (Sender : TObject; E : Exception; var CanClose : Boolean);
begin
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.ConnectionDataSent(Sender : TObject; ErrCode : Word);
var
  TmpFrame : TWebSocketOutgoingFrame;
  Len : Integer;
  TmpDumpFrame : TWebSocketReceivedFrame;
label
  RetrySendFrame;
begin
    if NOT WSClient then begin
        inherited ConnectionDataSent(Sender, ErrCode);
        exit;
    end;

RetrySendFrame :
    if CurrOutgoingFrame = nil then begin

    // look for queued send frame, remove from queue
        while OutgoingFrames.Count > 0 do  begin
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
    if Length(SendBuffer) <= MaxWSFrameSize then
        SetLength(SendBuffer, MaxWSFrameSize);
    Len := CurrOutgoingFrame.Data.Read(SendBuffer[0], Length(SendBuffer));
    if Len <= 0 then begin
        TmpFrame := CurrOutgoingFrame;
        CurrOutgoingFrame := nil;
        if (DebugLevel >= DebugBody) and (TmpFrame.Data is TMemoryStream) then begin
            TmpDumpFrame := TWebSocketReceivedFrame.Create;
            try
                TmpDumpFrame.Parse((TmpFrame.Data as TMemoryStream).Memory, TmpFrame.Data.Size );
                WSLogEvent(WSDumpFrame('Sent', TmpDumpFrame));
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
    else begin
        SendTB(SendBuffer, Len);
        if (DebugLevel >= DebugHdr) then
            WSLogEvent('WebSocket : Sending ' + IntToStr(Len) + ' bytes');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.ProcessReceivedFrame(AFrame: TWebSocketReceivedFrame);
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
                    WSSendFrame( nil, wsfkClose, AFrame.Data );
                end;
                WSClosing := True;
              // when this happens, the server should close the connection itself
                CloseDelayed;
                if Assigned(FOnWSDisconnected) then
                    FOnWSDisconnected(Self);
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
        WSFrameCounter := WSFrameCounter + 1;
        if Assigned(FOnWSFrameRcvd) then
        try
            FOnWSFrameRcvd(Self, APacket, AFrame);
        except
        end;
    finally
        if AFrame <> nil then
            AFrame.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.ConnectionDataAvailable(Sender: TObject; ErrCode: Word);
var
  Len : Integer;
begin
    if NOT WSClient then begin
        inherited;
        exit;
    end;
    if Length(RcvdBuffer) < MaxWSFrameSize then
    SetLength(RcvdBuffer, MaxWSFrameSize );
    Len := ReceiveTB(RcvdBuffer);
{  if (DebugLevel >= DebugHdr) then begin
     LogEvent( 'WebSocket : ' + Format('FReceiveLen : %d', [FreceiveLen]));
        if Len < 0 then
        LogEvent( 'WebSocket : ' + WSocketErrorDesc( FCtrlSocket.LastError ))
      else
        LogEvent( 'WebSocket : Received ' + IntToStr( Len ) + ' bytes...');
    end; }
    if Len <= 0 then Exit;
    LastReceivedDataTickCount := IcsGetTickCount64;
    LastSentPingTickCount := 0;
    if CurrFrame = nil then
    CurrFrame := TWebSocketReceivedFrame.Create;
    if not CurrFrame.Parse(RcvdBuffer, Len) then begin
        if (DebugLevel >= DebugBody) then
            WSLogEvent(WSDumpFrame('Received Invalid', CurrFrame));
        WSClose(wscrUnsupportedData, 'Unknown frame structure');
        CloseDelayed;
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
                        WSLogEvent(WSDumpFrame('Received Multiframe', CurrMultiFrame));
                    ProcessReceivedFrame( CurrMultiFrame );
                    CurrMultiFrame := nil;
                end;
            end;

      // if there is nothing to connect it to, it is simply discarded
            CurrFrame.Free;
            CurrFrame := nil;
            Exit;
        end
        else if not CurrFrame.IsFinal then begin
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
            WSLogEvent(WSDumpFrame('Received Singleframe', CurrFrame));
        ProcessReceivedFrame(CurrFrame);
        CurrFrame := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.WSClose(CloseReason: TWebSocketCloseReason; ADescription: String);
var
    EC: TUint16Bytes;
    B: TBytes;
begin
    FPeriodicTimer.Enabled := false;
    case CloseReason of
        wscrNormalClosure : EC.W := 1000;
        wscrGoingAway : EC.W := 1001;
        wscrProtocolError : EC.W := 1002;
        wscrUnsupportedData : EC.W := 1003;
        wscrInvalidFramePayloadData : EC.W := 1007;
        wscrPolicyViolation : EC.W := 1008;
        wscrMessageTooBig : EC.W := 1009;
        wscrMandatoryExt : EC.W := 1010;
        wscrInternalServerError : EC.W := 1011;
    else
        EC.W := 1000;
    end;

    B := StringToUtf8TB( 'XX' + ADescription );
    // I need to write EC.B[1] and EC.B[0] bytes at the beginning
    // character X is one byte in utf 8, so XX makes room for 2 bytes
    B[0] := EC.B[1];
    B[1] := EC.B[0];
    if (DebugLevel >= DebugConn) then
        WSLogEvent('WebSocket: Closing Connection');
    WSSendFrameBytes( nil, wsfkClose, B );
    WSClosing := True;
    if Assigned(FOnWSDisconnected) then
        FOnWSDisconnected(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.WSSendFrame(AFrameSourceID: Pointer; AFrameKind: TWebSocketFrameKind; const AStream: TStream);
var
    AFrame: TWebSocketOutgoingFrame;
    i: Integer;
    RestSize: Int64;
    FrameSize: Int64;
    AKind: TWebSocketFrameKind;
begin
    WSFrameCounter := WSFrameCounter + 1;
    if WSClosing then
        Exit;

    if AFrameKind = wsfkUnknown then
        Exit;

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
        AFrame.InitFrameData(AFrameSourceID, AFrameKind, AStream, RestSize, True, False);

        // the control frame must be sent ASAP, and must not be split into multiple frames
        if (DebugLevel >= DebugHdr) then
            WSLogEvent('WebSocket: Added ' + GetWSFrameKind(AFrameKind) + ' to queue');
        OutgoingFrames.Insert(0, AFrame);     // add to send frame queue
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
            AFrame.InitFrameData(AFrameSourceID, AKind, AStream, FrameSize, RestSize <= 0, False);
            OutgoingFrames.Add(AFrame);   // add to send frame queue
            if (DebugLevel >= DebugHdr) then
                WSLogEvent('WebSocket: Added ' + GetWSFrameKind(AKind) + ' to queue');
            AKind := wsfkContinue;
        until RestSize <= 0;
    end;
    if CurrOutgoingFrame = nil then
        ConnectionDataSent(Self, 0 );
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.WSSendFrameBytes(AFrameSourceID: Pointer; AFrameKind: TWebSocketFrameKind; const AData: TBytes);
var
    AStream : TStream;
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


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.WSSendFrameMemory( AFrameSourceID: Pointer; AFrameKind: TWebSocketFrameKind; const APData: Pointer; ADataBytes: Integer);
var
    AStream : TStream;
begin
    AStream := nil;
    try
        if (APData <> nil) and (ADataBytes > 0) then begin
            AStream := TMemoryReadStream.Create(APData, ADataBytes);
            AStream.Position := 0;
        end;
        WSSendFrame(AFrameSourceID, AFrameKind, AStream);
    finally
        if AStream <> nil then
            AStream.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.WSSendPing(const AData: TBytes);
begin
    WSSendFrameBytes(nil, wsfkPing, AData);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.WSSendPong(const AData: TBytes);
begin
    WSSendFrameBytes( nil, wsfkPong, AData );
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.WSSendText(AFrameSourceID: Pointer; AText: String);
begin
    WSSendFrameBytes(AFrameSourceID, wsfkText, StringToUtf8TB( AText));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.WSSendBinaryStream(AFrameSourceID: Pointer; AStream: TStream);
begin
    WSSendFrame( AFrameSourceID, wsfkBin, AStream );
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.ProcessPeriodicTasks;
var
    Data: TBytes;
begin
    if (OutgoingFrames.Count <= 0) and (CurrOutgoingFrame = nil) and (WSPingSecs >= 5)
           and (IcsElapsedSecs64(LastReceivedDataTickCount) > WSPingSecs) then begin

       // see if user wants to push data to the client
        if Assigned(OnWSPingTimer) then begin
            OnWSPingTimer(Self);
        end;

      // no sending is running and for x seconds nothing arrived, not even a single Byte
        if LastSentPingTickCount = 0 then begin
            SetLength(Data, 0);
            WSSendPing(Data);
            LastSentPingTickCount := IcsGetTickCount64;
        end
        else if IcsElapsedSecs64(LastSentPingTickCount) > 10 then begin
        // 10 seconds after ping no response
            WSClose( wscrProtocolError, 'No response to ping' );
            CloseDelayed;
        end;
    end;
    if (OutgoingFrames.Count > 0) and (CurrOutgoingFrame = nil) then
        ConnectionDataSent(Self, 0 );
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpWSSrvConn.PeriodicTimerTimer(Sender: TObject);
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


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
finalization
end.
