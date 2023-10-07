{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Description:  MQ Telemetry Transport is a lightweight, publish-subscribe, machine
              to machine network protocol for message queue/message queuing service.
              The MQTT protocol defines two types of network entities: a message broker
              and a number of clients. An MQTT broker is a server that receives all
              messages from the clients and then routes the messages to the appropriate
              destination clients. An MQTT client is any device (from a micro controller
              up to a fully-fledged server) that runs an MQTT library and connects to
              an MQTT broker over a network.
Creation:     March 2009
Updated:      Aug 2023
Version:      V9.0
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2008-2023 by pjde and Geoffrey Smith,
              https://github.com/pjde/delphi-mqtt
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

(*  MQTT web Sites
http://www.alphaworks.ibm.com/tech/rsmb
http://www.mqtt.org

Permission to copy and display the MQ Telemetry Transport specification (the
"Specification"), in any medium without fee or royalty is hereby granted by Eurotech
and International Business Machines Corporation (IBM) (collectively, the "Authors"),
provided that you include the following on ALL copies of the Specification, or portions
thereof, that you make:
A link or URL to the Specification at one of
1. the Authors' websites.
2. The copyright notice as shown in the Specification.

The Authors each agree to grant you a royalty-free license, under reasonable,
non-discriminatory terms and conditions to their respective patents that they deem
necessary to implement the Specification. THE SPECIFICATION IS PROVIDED "AS IS,"
AND THE AUTHORS MAKE NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR
IMPLIED, INCLUDING, BUT NOT LIMITED TO, WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT, OR TITLE; THAT THE
CONTENTS OF THE SPECIFICATION ARE SUITABLE FOR ANY PURPOSE; NOR THAT THE
IMPLEMENTATION OF SUCH CONTENTS WILL NOT INFRINGE ANY THIRD PARTY
PATENTS, COPYRIGHTS, TRADEMARKS OR OTHER RIGHTS. THE AUTHORS WILL NOT
BE LIABLE FOR ANY DIRECT, INDIRECT, SPECIAL, INCIDENTAL OR CONSEQUENTIAL
DAMAGES ARISING OUT OF OR RELATING TO ANY USE OR DISTRIBUTION OF THE
SPECIFICATION *)


Feb 24th 2023 V8.71 Baseline incorporated into ICS packages.
                    Renamed TMQTTParser to TIcsMQTTParser, TMQTTClient to TIcsMQTTClient,
                    TMQTTServer to TIcsMQTTServer to avoid cpnflicts if the original
                    units are installed, and TClient to TSrvrClient for clarity.
                    Added SSL/TLS support for client and server including automatic
                      certificate ordering.  The server will listen on both 1883 and 8883
                      at the same time.
Aug 08, 2023 V9.0  Updated version to major release 9.






 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Z.ICS9.OverbyteIcsMQTT;

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

{$IFDEF USE_SSL}    { THIS UNIT ONLY BUILDS WITH _USE_SSL ENABLED }

uses
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    {$Ifdef Rtl_Namespaces}System.Types{$Else}Types{$Endif},
    Z.ICS9.OverbyteIcsSSLEAY, Z.ICS9.OverbyteIcsLIBEAY,
    Z.ICS9.OverbyteIcsWndControl,
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsWSocketS,
    Z.ICS9.OverbyteIcsSslHttpRest,
{$IFDEF AUTO_X509_CERTS}
    Z.ICS9.OverbyteIcsSslX509Certs,
{$ENDIF} // AUTO_X509_CERTS
    Z.ICS9.OverbyteIcsSslX509Utils,
    Z.ICS9.OverbyteIcsIniFiles,
    Z.ICS9.OverbyteIcsTicks64,
    Z.ICS9.OverbyteIcsUtils;


const
    CopyRight    : String     = ' TIcsMQTTServer (c) 2023 V9.0 ';

const
  MQTT_PROTOCOL = 'MQIsdp';
  MQTT_VERSION = 3;
  MQTT_MinVersion = 3;
  MQTT_PortNon = 1883;
  MQTT_PortSsl = 8883;


  DefRetryTime  = 60;   // 6 seconds
  DefMaxRetries = 8;

  rsHdr           = 0;
  rsLen           = 1;
  rsVarHdr        = 2;
  rsPayload       = 3;

  frKEEPALIVE     = 0;    // keep alive exceeded
  frMAXRETRIES    = 1;

  rcACCEPTED      = 0;    // Connection Accepted
  rcPROTOCOL      = 1;    // Connection Refused: unacceptable protocol version
  rcIDENTIFIER    = 2;    // Connection Refused: identifier rejected
  rcSERVER        = 3;    // Connection Refused: server unavailable
  rcUSER          = 4;    // Connection Refused: bad user name or password
  rcAUTHORISED    = 5;    // Connection Refused: not authorised
                          // 6-255 Reserved for future use
  ny : array [boolean] of string = ('NO', 'YES');

type
  //  Message type
  TMQTTMessageType =
  (
//    mtReserved0,    //  0 Reserved
    mtBROKERCONNECT,  //  0 Broker request to connect to Broker
    mtCONNECT,        //    1   Client request to connect to Broker
    mtCONNACK,        //    2   Connect Acknowledgment
    mtPUBLISH,        //    3   Publish message
    mtPUBACK,         //    4   Publish Acknowledgment
    mtPUBREC,         //    5   Publish Received (assured delivery part 1)
    mtPUBREL,         //    6   Publish Release (assured delivery part 2)
    mtPUBCOMP,        //    7   Publish Complete (assured delivery part 3)
    mtSUBSCRIBE,      //    8   Client Subscribe request
    mtSUBACK,         //    9   Subscribe Acknowledgment
    mtUNSUBSCRIBE,    // 10 Client Unsubscribe request
    mtUNSUBACK,       // 11 Unsubscribe Acknowledgment
    mtPINGREQ,        // 12 PING Request
    mtPINGRESP,       // 13 PING Response
    mtDISCONNECT,     // 14 Client is Disconnecting
    mtReserved15      // 15
  );

  TMQTTQOSType =
  (
    qtAT_MOST_ONCE,   //  0 At most once Fire and Forget        <=1
    qtAT_LEAST_ONCE,  //  1 At least once Acknowledged delivery >=1
    qtEXACTLY_ONCE,   //  2 Exactly once Assured delivery       =1
    qtReserved3       //  3 Reserved
  );

  TMQTTStreamEvent = procedure (Sender : TObject; anID : Word; Retry : integer; aStream : TMemoryStream) of object;
  TMQTTMonEvent = procedure (Sender : TObject; const aStr : string) of object;
  TMQTTCheckUserEvent = procedure (Sender : TObject; aUser, aPass : UTF8String; var Allowed : Boolean) of object;
  TMQTTPubResponseEvent = procedure (Sender : TObject; aMsg : TMQTTMessageType; anID : Word) of object;
  TMQTTIDEvent = procedure (Sender : TObject; anID : Word) of object;
  TMQTTAckEvent = procedure (Sender : TObject; aCode : Byte) of object;
  TMQTTDisconnectEvent = procedure (Sender : TObject; Graceful : Boolean) of object;
  TMQTTSubscriptionEvent = procedure (Sender : TObject; aTopic : UTF8String; var RequestedQos : TMQTTQOSType) of object;
  TMQTTSubscribeEvent = procedure (Sender : TObject; anID : Word; Topics : TStringList) of object;
  TMQTTUnsubscribeEvent = procedure (Sender : TObject; anID : Word; Topics : TStringList) of object;
  TMQTTSubAckEvent = procedure (Sender : TObject; anID : Word; Qoss : array of TMQTTQosType) of object;
  TMQTTFailureEvent = procedure (Sender : TObject; aReason : integer; var CloseClient : Boolean) of object;
  TMQTTMsgEvent = procedure (Sender : TObject; aTopic : UTF8String; const aMessage : AnsiString; aQos : TMQTTQOSType; aRetained : boolean) of object;
  TMQTTRetainEvent = procedure (Sender : TObject; aTopic : UTF8String; aMessage : AnsiString; aQos : TMQTTQOSType) of object;
  TMQTTRetainedEvent = procedure (Sender : TObject; Subscribed : UTF8String; var aTopic : UTF8String; var aMessage : AnsiString; var aQos : TMQTTQOSType) of object;
  TMQTTPublishEvent = procedure (Sender : TObject; anID : Word; aTopic : UTF8String; const aMessage : AnsiString) of object;
  TIcsMQTTClientIDEvent = procedure (Sender : TObject; var aClientID : UTF8String) of object;
  TMQTTConnectEvent = procedure (Sender : TObject; Protocol : UTF8String; Version : byte; ClientID, UserName, Password : UTF8String; KeepAlive : Word; Clean : Boolean) of object;
  TMQTTWillEvent = procedure (Sender : TObject; aTopic, aMessage : UTF8String; aQos : TMQTTQOSType; aRetain : boolean) of object;
  TMQTTObituaryEvent = procedure (Sender : TObject; var aTopic, aMessage : UTF8String; var aQos : TMQTTQOSType) of object;
  TMQTTHeaderEvent = procedure (Sender : TObject; MsgType: TMQTTMessageType; Dup: Boolean; Qos: TMQTTQOSType; Retain: Boolean) of object;
  TMQTTSessionEvent = procedure (Sender : TObject; aClientID : UTF8String) of object;

  TIcsMQTTParser = class
  private
    FOnSend: TMQTTStreamEvent;
    FTxStream : TMemoryStream;
    FRxStream : TMemoryStream;
    FKeepAliveCount : cardinal;
    FKeepAlive : Word;
    FWillFlag : boolean;
    FRxState, FRxMult, FRxVal : integer;
    FOnConnAck: TMQTTAckEvent;
    FOnUnsubAck: TMQTTIDEvent;
    FOnSubscribe: TMQTTSubscribeEvent;
    FOnPing: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnPingResp: TNotifyEvent;
    FOnPublish: TMQTTPublishEvent;
    FOnConnect: TMQTTConnectEvent;
    FOnUnsubscribe: TMQTTUnsubscribeEvent;
    FOnSubAck: TMQTTSubAckEvent;
    FOnSetWill: TMQTTWillEvent;
    FOnHeader: TMQTTHeaderEvent;
    FOnMon: TMQTTMonEvent;
    FOnPubAck: TMQTTIDEvent;
    FOnPubRel: TMQTTIDEvent;
    FOnPubComp: TMQTTIDEvent;
    FOnPubRec: TMQTTIDEvent;
    FMaxRetries: Word;
    FRetryTime: Word;
    FOnBrokerConnect: TMQTTConnectEvent;
    procedure SetKeepAlive(const Value: Word);
  public
    NosRetries : integer;
    RxMsg : TMQTTMessageType;
    RxQos : TMQTTQOSType;
    RxDup, RxRetain : Boolean;
    UserName, Password,
    WillTopic: UTF8String;
    WillMessage : UTF8String;
    WillRetain : Boolean;
    WillQos : TMQTTQOSType;
    ClientID : UTF8String;
    Clean : Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure Parse (aStream : TStream); overload;
    procedure Parse (const aStr : AnsiString); overload;
    procedure SetWill (aTopic, aMessage : UTF8String; aQos : TMQTTQOSType; aRetain : boolean);
    function CheckKeepAlive : boolean;
    procedure Mon (const aStr: string);
    // client
    procedure SendBrokerConnect (aClientID, aUsername, aPassword : UTF8String; aKeepAlive : Word; aClean : Boolean);  // non standard
    procedure SendConnect (aClientID, aUsername, aPassword : UTF8String; aKeepAlive : Word; aClean : Boolean);
    procedure SendPublish (anID : Word; aTopic : UTF8String; const aMessage : AnsiString; aQos : TMQTTQOSType; aDup : boolean = false; aRetain : boolean = false);
    procedure SendPing;
    procedure SendDisconnect;
    procedure SendSubscribe (anID : Word; aTopic : UTF8String; aQOS : TMQTTQOSType); overload;
    procedure SendSubscribe (anID : Word; Topics : TStringList); overload;
    procedure SendUnsubscribe (anID : Word; aTopic : UTF8String); overload;
    procedure SendUnsubscribe (anID : Word; Topics : TStringList); overload;
    // server
    procedure SendConnAck (aCode : byte);
    procedure SendPubAck (anID : Word);
    procedure SendPubRec (anID : Word);
    procedure SendPubRel (anID : Word; aDup : Boolean = false);
    procedure SendPubComp (anID : Word);
    procedure SendSubAck (anID : Word; Qoss : array of TMQTTQosType);
    procedure SendUnsubAck (anID : Word);
    procedure SendPingResp;
    property KeepAlive : Word read FKeepAlive write SetKeepAlive;
    property RetryTime : Word read FRetryTime write FRetryTime;
    property MaxRetries : Word read FMaxRetries write FMaxRetries;
    // client
    property OnConnAck : TMQTTAckEvent read FOnConnAck write FOnConnAck;
    property OnSubAck: TMQTTSubAckEvent read FOnSubAck write FOnSubAck;
    property OnPubAck : TMQTTIDEvent read FOnPubAck write FOnPubAck;
    property OnPubRel : TMQTTIDEvent read FOnPubRel write FOnPubRel;
    property OnPubRec : TMQTTIDEvent read FOnPubRec write FOnPubRec;
    property OnPubComp : TMQTTIDEvent read FOnPubComp write FOnPubComp;
    property OnUnsubAck : TMQTTIDEvent read FOnUnsubAck write FOnUnsubAck;
    property OnPingResp : TNotifyEvent read FOnPingResp write FOnPingResp;
    // server
    property OnBrokerConnect : TMQTTConnectEvent read FOnBrokerConnect write FOnBrokerConnect;  // non standard
    property OnConnect : TMQTTConnectEvent read FOnConnect write FOnConnect;
    property OnPublish : TMQTTPublishEvent read FOnPublish write FOnPublish;
    property OnPing : TNotifyEvent read FOnPing write FOnPing;
    property OnDisconnect : TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnSubscribe : TMQTTSubscribeEvent read FOnSubscribe write FOnSubscribe;
    property OnUnsubscribe : TMQTTUnsubscribeEvent read FOnUnsubscribe write FOnUnsubscribe;
    property OnSetWill : TMQTTWillEvent read FOnSetWill write FOnSetWill;
    property OnHeader : TMQTTHeaderEvent read FOnHeader write FOnHeader;
    property OnMon : TMQTTMonEvent read FOnMon write FOnMon;
    property OnSend : TMQTTStreamEvent read FOnSend write FOnSend;
  end;

const
  MsgNames : array [TMQTTMessageType] of string =
  (
//    'Reserved',       //  0   Reserved
    'BROKERCONNECT',    //  0   Broker request to connect to Broker
    'CONNECT',        //    1   Client request to connect to Broker
    'CONNACK',        //    2   Connect Acknowledgment
    'PUBLISH',        //    3   Publish message
    'PUBACK',         //    4   Publish Acknowledgment
    'PUBREC',         //    5   Publish Received (assured delivery part 1)
    'PUBREL',         //    6   Publish Release (assured delivery part 2)
    'PUBCOMP',        //    7   Publish Complete (assured delivery part 3)
    'SUBSCRIBE',      //    8   Client Subscribe request
    'SUBACK',         //    9   Subscribe Acknowledgment
    'UNSUBSCRIBE',    // 10 Client Unsubscribe request
    'UNSUBACK',       // 11 Unsubscribe Acknowledgment
    'PINGREQ',        // 12 PING Request
    'PINGRESP',       // 13 PING Response
    'DISCONNECT',     // 14 Client is Disconnecting
    'Reserved15'      // 15
  );

  QOSNames : array [TMQTTQOSType] of string =
  (
    'AT_MOST_ONCE',   //  0 At most once Fire and Forget        <=1
    'AT_LEAST_ONCE',  //  1 At least once Acknowledged delivery >=1
    'EXACTLY_ONCE',   //  2 Exactly once Assured delivery       =1
    'RESERVED'        //  3 Reserved
  );

type
  TSrvrClient = class;
  TIcsMQTTClient = class;
  TMQTTPacketStore = class;
  TMQTTMessageStore = class;

  TMQTTPacket = class
    ID : Word;
    Stamp : TDateTime;
    Counter : cardinal;
    Retries : integer;
    Publishing : Boolean;
    Msg : TMemoryStream;
    procedure Assign (From : TMQTTPacket);
    constructor Create;
    destructor Destroy; override;
  end;

  TMQTTMessage = class
    ID : Word;
    Stamp : TDateTime;
    LastUsed : TDateTime;
    Qos : TMQTTQOSType;
    Retained : boolean;
    Counter : cardinal;
    Retries : integer;
    Topic : UTF8String;
    Message : AnsiString;
    procedure Assign (From : TMQTTMessage);
    constructor Create;
    destructor Destroy; override;
  end;

  TMQTTSession = class
    ClientID : UTF8String;
    Stamp : TDateTime;
    InFlight : TMQTTPacketStore;
    Releasables : TMQTTMessageStore;
    constructor Create;
    destructor Destroy; override;
  end;

  TMQTTSessionStore = class
    List :  TList;
    Stamp : TDateTime;
    function GetItem (Index: Integer): TMQTTSession;
    procedure SetItem (Index: Integer; const Value: TMQTTSession);
    property Items [Index: Integer]: TMQTTSession read GetItem write SetItem; default;
    function Count : integer;
    procedure Clear;
    function GetSession (ClientID : UTF8String) : TMQTTSession;
    procedure StoreSession (ClientID : UTF8String; aClient : TSrvrClient); overload;
    procedure StoreSession (ClientID : UTF8String; aClient : TIcsMQTTClient); overload;
    procedure DeleteSession (ClientID : UTF8String);
    procedure RestoreSession (ClientID : UTF8String; aClient : TSrvrClient); overload;
    procedure RestoreSession (ClientID : UTF8String; aClient : TIcsMQTTClient); overload;
    constructor Create;
    destructor Destroy; override;
  end;

  TMQTTPacketStore = class
    List : TList;
    Stamp : TDateTime;
    function GetItem (Index: Integer): TMQTTPacket;
    procedure SetItem (Index: Integer; const Value: TMQTTPacket);
    property Items [Index: Integer]: TMQTTPacket read GetItem write SetItem; default;
    function Count : integer;
    procedure Clear;
    procedure Assign (From : TMQTTPacketStore);
    function AddPacket (anID : Word; aMsg : TMemoryStream; aRetry : cardinal; aCount : cardinal) : TMQTTPacket;
    procedure DelPacket (anID : Word);
    function GetPacket (anID : Word) : TMQTTPacket;
    procedure Remove (aPacket : TMQTTPacket);
    constructor Create;
    destructor Destroy; override;
  end;

  TMQTTMessageStore = class
    List : TList;
    Stamp : TDateTime;
    function GetItem (Index: Integer): TMQTTMessage;
    procedure SetItem (Index: Integer; const Value: TMQTTMessage);
    property Items [Index: Integer]: TMQTTMessage read GetItem write SetItem; default;
    function Count : integer;
    procedure Clear;
    procedure Assign (From : TMQTTMessageStore);
    function AddMsg (anID : Word; aTopic : UTF8String; aMessage : AnsiString; aQos : TMQTTQOSType; aRetry : cardinal; aCount : cardinal; aRetained : Boolean = false) : TMQTTMessage;
    procedure DelMsg (anID : Word);
    function GetMsg (anID : Word) : TMQTTMessage;
    procedure Remove (aMsg : TMQTTMessage);
    constructor Create;
    destructor Destroy; override;
  end;

  TSrvrClient = class (TSslWSocketClient)
  private
    FOnMon : TMQTTMonEvent;
    FGraceful : boolean;
    FBroker : Boolean;        // non standard
    FOnSubscriptionChange: TNotifyEvent;
    procedure DoSend (Sender : TObject; anID : Word; aRetry : integer; aStream : TMemoryStream);
    procedure RxSubscribe (Sender : TObject; anID : Word; Topics : TStringList);
    procedure RxUnsubscribe (Sender : TObject; anID : Word; Topics : TStringList);
    procedure RxPubAck (Sender : TObject; anID : Word);
    procedure RxPubRec (Sender : TObject; anID : Word);
    procedure RxPubRel (Sender : TObject; anID : Word);
    procedure RxPubComp (Sender : TObject; anID : Word);
  public
    Subscriptions : TStringList;
    Parser : TIcsMQTTParser;
    InFlight : TMQTTPacketStore;
    Releasables : TMQTTMessageStore;
    procedure Mon (const aStr: string);
    procedure DoData (Sender : TObject; ErrCode : Word);
    procedure DoSetWill (Sender : TObject; aTopic, aMessage : UTF8String; aQOS : TMQTTQOSType; aRetain : boolean);
    constructor Create (anOwner : TComponent); override;
    destructor Destroy; override;
    property OnSubscriptionChange : TNotifyEvent read FOnSubscriptionChange write FOnSubscriptionChange;
    property OnMon : TMQTTMonEvent read FOnMon write FOnMon;
  end;

  TIcsMQTTClient = class (TComponent)
  private
    Timers : HWnd;
    FUsername, FPassword : UTF8String;
    FMessageID : Word;
    FHost : string;
    FPort : integer;
    FEnable, FOnline : Boolean;
    FGraceful : Boolean;
    FOnMon : TMQTTMonEvent;
    FOnOnline: TNotifyEvent;
    FOnOffline: TMQTTDisconnectEvent;
    FOnEnableChange: TNotifyEvent;
    FOnMsg: TMQTTMsgEvent;
    FOnFailure: TMQTTFailureEvent;
    FLocalBounce: Boolean;
    FAutoSubscribe: Boolean;
    FOnClientID : TIcsMQTTClientIDEvent;
    FBroker: Boolean;     // non standard
    FSslVerifyCerts: Boolean;
    FSslReportChain: Boolean;
    FSslRevocation: Boolean;
    FRootCA: string;
    FSslCliSecurity: TSslCliSecurity;
    FOcspHttp: TOcspHttp;
    procedure DoSend (Sender : TObject; anID : Word; aRetry : integer; aStream : TMemoryStream);
    procedure RxConnAck (Sender : TObject; aCode : byte);
    procedure RxSubAck (Sender : TObject; anID : Word; Qoss : array of TMQTTQosType);
    procedure RxPubAck (Sender : TObject; anID : Word);
    procedure RxPubRec (Sender : TObject; anID : Word);
    procedure RxPubRel (Sender : TObject; anID : Word);
    procedure RxPubComp (Sender : TObject; anID : Word);
    procedure RxPublish (Sender: TObject; anID: Word; aTopic : UTF8String; const aMessage : AnsiString);
    procedure RxUnsubAck (Sender : TObject; anID : Word);
    procedure LinkConnected (Sender: TObject; ErrCode: Word);
    procedure LinkClosed (Sender: TObject; ErrCode: Word);
    procedure LinkData (Sender: TObject; ErrCode: Word);
    procedure TimerProc (var aMsg : TMessage);
    function GetClientID: UTF8String;
    procedure SetClientID (const Value: UTF8String);
    function GetKeepAlive: Word;
    procedure SetKeepAlive(const Value: Word);
    function GetMaxRetries : integer;
    procedure SetMaxRetries(const Value: integer);
    function GetRetryTime : cardinal;
    procedure SetRetryTime (const Value : cardinal);
    function GetClean: Boolean;
    procedure SetClean(const Value: Boolean);
    function GetPassword: UTF8String;
    function GetUsername: UTF8String;
    procedure SetPassword(const Value: UTF8String);
    procedure SetUsername(const Value: UTF8String);
    procedure SslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);  { V8.71 }
  public
    LinkSocket : TSslWSocket;
    LinkSslContext: TSslContext;  { V8.71 }
    Parser : TIcsMQTTParser;
    InFlight : TMQTTPacketStore;
    Releasables : TMQTTMessageStore;
    Subscriptions : TStringList;
    function Enabled : boolean;
    function Online : boolean;
    function NextMessageID : Word;
    procedure Subscribe (aTopic : UTF8String; aQos : TMQTTQOSType); overload;
    procedure Subscribe (Topics : TStringList); overload;
    procedure Unsubscribe (aTopic : UTF8String); overload;
    procedure Unsubscribe (Topics : TStringList); overload;
    procedure Ping;
    procedure Publish (aTopic : UTF8String; const aMessage : AnsiString; aQos : TMQTTQOSType; aRetain : Boolean = false);
    procedure SetWill (aTopic, aMessage : UTF8String; aQos : TMQTTQOSType; aRetain : Boolean = false);
    procedure Mon (const aStr: string);
    procedure Activate (Enable : Boolean);
    constructor Create (anOwner : TComponent); override;
    destructor Destroy; override;
  published
    property ClientID : UTF8String read GetClientID write SetClientID;
    property KeepAlive : Word read GetKeepAlive write SetKeepAlive;
    property MaxRetries : integer read GetMaxRetries write SetMaxRetries;
    property RetryTime : cardinal read GetRetryTime write SetRetryTime;
    property Clean : Boolean read GetClean write SetClean;
    property Broker : Boolean read FBroker write FBroker;   // no standard
    property AutoSubscribe : Boolean read FAutoSubscribe write FAutoSubscribe;
    property Username : UTF8String read GetUsername write SetUsername;
    property Password : UTF8String read GetPassword write SetPassword;
    property Host : string read FHost write FHost;
    property Port : integer read FPort write FPort;
    property LocalBounce : Boolean read FLocalBounce write FLocalBounce;
    property SslVerifyCerts: Boolean read FSslVerifyCerts write FSslVerifyCerts;
    property SslReportChain: boolean read FSslReportChain write FSslReportChain;
    property SslRevocation: boolean read FSslRevocation write FSslRevocation;
    property OcspHttp: TOcspHttp read FOcspHttp write FOcspHttp;
    property RootCA: string read FRootCA write FRootCA;
    property SslCliSecurity: TSslCliSecurity read FSslCliSecurity write FSslCliSecurity;
    property OnClientID : TIcsMQTTClientIDEvent read FOnClientID write FOnClientID;
    property OnMon : TMQTTMonEvent read FOnMon write FOnMon;
    property OnOnline : TNotifyEvent read FOnOnline write FOnOnline;
    property OnOffline : TMQTTDisconnectEvent read FOnOffline write FOnOffline;
    property OnEnableChange : TNotifyEvent read FOnEnableChange write FOnEnableChange;
    property OnFailure : TMQTTFailureEvent read FOnFailure write FOnFailure;
    property OnMsg : TMQTTMsgEvent read FOnMsg write FOnMsg;
  end;

  TIcsMQTTServer = class (TComponent)
  private
    FServer : TSslWSocketServer;
    FOnMon : TMQTTMonEvent;
    FOnClientsChange: TMQTTIDEvent;
    FOnCheckUser: TMQTTCheckUserEvent;
    Timers : HWnd;
//    FPort : integer;                           { V8.71 using IcsHosts }
    FEnable : boolean;
    FOnBrokerOffline: TMQTTDisconnectEvent;
    FOnBrokerOnline: TNotifyEvent;
    FOnBrokerEnableChange: TNotifyEvent;
    FOnObituary: TMQTTObituaryEvent;
    FOnEnableChange: TNotifyEvent;
    FLocalBounce: Boolean;
    FOnSubscription: TMQTTSubscriptionEvent;
    FOnFailure: TMQTTFailureEvent;
    FMaxRetries: integer;
    FRetryTime: cardinal;
    FOnStoreSession: TMQTTSessionEvent;
    FOnRestoreSession: TMQTTSessionEvent;
    FOnDeleteSession: TMQTTSessionEvent;
//    FOnRetain: TMQTTRetainEvent;
//    FOnGetRetained: TMQTTRetainedEvent;
    procedure TimerProc (var aMsg : TMessage);
    procedure DoMon (Sender: TObject; const aStr: string);
    // broker events
    procedure BkrOnline (Sender : TObject);
    procedure BkrOffline (Sender : TObject; Graceful : boolean);
    procedure BkrEnableChanged (Sender : TObject);
    procedure BkrSubscriptionChange (Sender : TObject);
    procedure BkrMsg (Sender: TObject; aTopic : UTF8String; const aMessage : AnsiString; aQos : TMQTTQOSType; aRetained : boolean);
    // socket events
    procedure DoClientConnect (Sender: TObject; Client: TWSocketClient; Error: Word);
    procedure DoClientDisconnect (Sender: TObject; Client: TWSocketClient; Error: Word);
    procedure DoClientCreate (Sender: TObject; Client: TWSocketClient);
    // parser events
    procedure RxDisconnect (Sender : TObject);
    procedure RxPing (Sender : TObject);
    procedure RxPublish (Sender: TObject; anID: Word; aTopic : UTF8String; const aMessage: AnsiString);
    procedure RxHeader (Sender : TObject; MsgType: TMQTTMessageType; Dup: Boolean; Qos: TMQTTQOSType; Retain: Boolean);
    procedure RxConnect (Sender : TObject; aProtocol : UTF8String; aVersion : byte; aClientID, aUserName, aPassword : UTF8String; aKeepAlive : Word; aClean : Boolean);
    procedure RxBrokerConnect (Sender : TObject; aProtocol : UTF8String; aVersion : byte; aClientID, aUserName, aPassword : UTF8String; aKeepAlive : Word; aClean : Boolean);    // non standard
    procedure SetMaxRetries (const Value: integer);
    procedure SetRetryTime (const Value: cardinal);
    function  GetIcsHosts: TIcsHostCollection;                    { V8.71 }
    procedure SetIcsHosts(const Value: TIcsHostCollection);       { V8.71 }
    function  GetRootCA: String;                                  { V8.71 }
    procedure SetRootCA(const Value: String);                     { V8.71 }
    function  GetCertExpireDays: Integer;                         { V8.71 }
    procedure SetCertExpireDays(const Value : Integer);           { V8.71 }
    function  GetSslCertAutoOrder: Boolean;                       { V8.71 }
    procedure SetSslCertAutoOrder(const Value : Boolean);         { V8.71 }
{$IFDEF AUTO_X509_CERTS}
    function  GetSslX509Certs: TSslX509Certs;                     { V8.71 }
    procedure SetSslX509Certs(const Value : TSslX509Certs);       { V8.71 }
    function  GetOcspSrvStapling: Boolean;                        { V8.71 }
    procedure SetOcspSrvStapling(const Value : Boolean);          { V8.71 }
{$ENDIF} // AUTO_X509_CERTS
public
    FOnMonHdr : TMQTTHeaderEvent;
    MessageID : Word;
    Brokers : TList;    // of wsocket
    Sessions : TMQTTSessionStore;
    Retained : TMQTTMessageStore;
    function NextMessageID : Word;
    procedure Mon (const aStr: string);
    procedure Activate (Enable : boolean);
    procedure LoadBrokers (const anIniFile: string);
    procedure StoreBrokers (const anIniFile: string);
    function GetClient (aParser : TIcsMQTTParser) : TSrvrClient; overload;
    function GetClient (aClientID : UTF8String) : TSrvrClient; overload;
    procedure PublishToAll (From : TObject; aTopic : UTF8String; const aMessage : AnsiString; aQos : TMQTTQOSType; wasRetained : boolean = false);
    function Enabled : boolean;
    function AddBroker (const aHost: string; aPort: integer): TIcsMQTTClient;
    procedure SyncBrokerSubscriptions (aBroker : TIcsMQTTClient);
    constructor Create (anOwner : TComponent); override;
    destructor Destroy; override;
    function ValidateHosts(Stop1stErr: Boolean=True; NoExceptions: Boolean=False): String;
    function RecheckSslCerts(var CertsInfo: String; Stop1stErr: Boolean=True; NoExceptions: Boolean=False): Boolean;
    function ListenAllOK: Boolean;
    function ListenStates: String;
    property Server : TSslWSocketServer read FServer;
  published
    property MaxRetries : integer read FMaxRetries write SetMaxRetries;
    property RetryTime : cardinal read FRetryTime write SetRetryTime;   // in secs
    property LocalBounce : Boolean read FLocalBounce write FLocalBounce;
//    property Port : integer read FPort write FPort;                               { V8.71 replaced by IcsHosts }
    property  IcsHosts : TIcsHostCollection read  GetIcsHosts write SetIcsHosts;    { V8.71 }
    property  RootCA : String                       read  GetRootCA
                                                    write SetRootCA;                { V8.71 }
    property  SslCertAutoOrder: Boolean             read  GetSslCertAutoOrder
                                                    write SetSslCertAutoOrder;      { V8.71 }
    property  CertExpireDays: Integer               read  GetCertExpireDays
                                                    write SetCertExpireDays;        { V8.71 }
{$IFDEF AUTO_X509_CERTS}  { V8.59 }
    property  SslX509Certs: TSslX509Certs           read  GetSslX509Certs
                                                    write SetSslX509Certs;          { V8.71 }
    property  OcspSrvStapling: Boolean              read  GetOcspSrvStapling
                                                    write SetOcspSrvStapling;       { V8.71 }
{$ENDIF} // AUTO_X509_CERTS
    property OnFailure : TMQTTFailureEvent read FOnFailure write FOnFailure;
    property OnStoreSession : TMQTTSessionEvent read FOnStoreSession write FOnStoreSession;
    property OnRestoreSession : TMQTTSessionEvent read FOnRestoreSession write FOnRestoreSession;
    property OnDeleteSession : TMQTTSessionEvent read FOnDeleteSession write FOnDeleteSession;
//    property OnRetain : TMQTTRetainEvent read FOnRetain write FOnRetain;
//    property OnGetRetained : TMQTTRetainedEvent read FOnGetRetained write FOnGetRetained;
    property OnBrokerOnline : TNotifyEvent read FOnBrokerOnline write FOnBrokerOnline;
    property OnBrokerOffline : TMQTTDisconnectEvent read FOnBrokerOffline write FOnBrokerOffline;
    property OnBrokerEnableChange : TNotifyEvent read FOnBrokerEnableChange write FOnBrokerEnableChange;
    property OnEnableChange : TNotifyEvent read FOnEnableChange write FOnEnableChange;
    property OnSubscription : TMQTTSubscriptionEvent read FOnSubscription write FOnSubscription;
    property OnClientsChange : TMQTTIDEvent read FOnClientsChange write FOnClientsChange;
    property OnCheckUser : TMQTTCheckUserEvent read FOnCheckUser write FOnCheckUser;
    property OnObituary : TMQTTObituaryEvent read FOnObituary write FOnObituary;
    property OnMon : TMQTTMonEvent read FOnMon write FOnMon;
  end;

function SubTopics (aTopic : UTF8String) : TStringList;
function IsSubscribed (aSubscription, aTopic : UTF8String) : boolean;
function MqttCodeNames (aCode : byte) : string;
function MqttFailureNames (aCode : byte) : string;

{$ENDIF USE_SSL}

implementation

{$IFDEF USE_SSL}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ExtractFileNameOnly (const FileName : string) : string;
begin
  Result := ExtractFileName (FileName);
  SetLength (Result, Length (Result) - Length (ExtractFileExt (FileName)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MqttCodeNames (aCode : byte) : string;
begin
  case (aCode) of
    rcACCEPTED   : Result := 'ACCEPTED';                    // Connection Accepted
    rcPROTOCOL   : Result := 'PROTOCOL UNACCEPTABLE';       // Connection Refused: unacceptable protocol version
    rcIDENTIFIER : Result := 'IDENTIFIER REJECTED';         // Connection Refused: identifier rejected
    rcSERVER     : Result := 'SERVER UNAVILABLE';           // Connection Refused: server unavailable
    rcUSER       : Result := 'BAD LOGIN';                   // Connection Refused: bad user name or password
    rcAUTHORISED : Result := 'NOT AUTHORISED'
    else           Result := 'RESERVED ' + IntToStr (aCode);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MqttFailureNames (aCode : byte) : string;
begin
  case (aCode) of
    frKEEPALIVE  : Result := 'KEEP ALIVE TIMEOUT';
    frMAXRETRIES : Result := 'MAX RETRIES EXCEEDED';
    else           Result := 'RESERVED ' + IntToStr (aCode);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DebugStr (const aStr : string);
begin
{$IFDEF MSWINDOWS}
  OutputDebugString (PChar (aStr));
{$ELSE}

{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure AddByte (aStream : TStream; aByte: Byte);
begin
  aStream.Write (aByte, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure AddHdr (aStream : TStream; MsgType: TMQTTMessageType; Dup: Boolean;
  Qos: TMQTTQOSType; Retain: Boolean);
begin
  { Fixed Header Spec:
    bit    |7 6 5   4       | |3         | |2   1        |  |  0   |
    byte 1 |Message Type| |DUP flag| |QoS level|    |RETAIN| }
  AddByte (aStream, (Ord (MsgType) shl 4) + (ord (Dup) shl 3) + (ord (Qos) shl 1) + ord (Retain));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure AddLength (aStream : TStream; aLen: integer);
var
  x : integer;
  dig : byte;
begin
  x := aLen;
  repeat
    dig := x mod 128;
    x := x div 128;
    if (x > 0) then
      dig := dig or $80;
    AddByte (aStream, dig);
  until (x = 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure AddStr (aStream : TStream; aStr: UTF8String);
var
  l : integer;
begin
  l := length (aStr);
  AddByte (aStream, l div $100);
  AddByte (aStream, l mod $100);
  aStream.Write (aStr[1], length (aStr));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ReadByte (aStream : TStream) : Byte;
begin
  if aStream.Position = aStream.Size then
    Result := 0
  else
    aStream.Read (Result, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ReadHdr (aStream : TStream; var MsgType: TMQTTMessageType; var Dup: Boolean;  var Qos: TMQTTQOSType; var Retain: Boolean) : byte;
begin
  Result := ReadByte (aStream);
  { Fixed Header Spec:
    bit    |7 6 5   4       | |3         | |2   1        |  |  0   |
    byte 1 |Message Type| |DUP flag| |QoS level|    |RETAIN| }
  MsgType := TMQTTMessageType ((Result and $f0) shr 4);
  Dup := (Result and $08) > 0;
  Qos := TMQTTQOSType ((Result and $06) shr 1);
  Retain := (Result and $01) > 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ReadLength (aStream : TStream) : integer;
var
  mult : integer;
  x : byte;
begin
  mult := 0;
  Result := 0;
  repeat
    x := ReadByte (aStream);
    Result := Result + ((x and $7f) * mult);
  until (x and $80) <> 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ReadStr (aStream : TStream) : UTF8String;
var
  l : integer;
begin
  l := ReadByte (aStream) * $100 + ReadByte (aStream);
  if aStream.Position + l <= aStream.Size then
    begin
      SetLength (Result, l);
      aStream.Read (Result[1], l);
    end;
end;


{ TIcsMQTTParser }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTParser.CheckKeepAlive: boolean;
begin
  Result := true;
  if FKeepAliveCount > 0 then
    begin
      FKeepAliveCount := FKeepAliveCount - 1;
      Result := (FKeepAliveCount > 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsMQTTParser.Create;
begin
  KeepAlive := 10;
  FKeepAliveCount := 0;
  FMaxRetries := DefMaxRetries;
  FRetryTime := DefRetryTime;
  NosRetries := 0;
  ClientID := '';
  WillTopic := '';
  WillMessage := '';
  FWillFlag := false;
  WillQos := qtAT_LEAST_ONCE;
  WillRetain := false;
  Username := '';
  Password := '';
  FRxState := rsHdr;
  FRxMult := 0;
  FRxVal := 0;
  RxMsg := mtReserved15;
  RxQos := qtAT_MOST_ONCE;
  RxDup := false;
  RxRetain := false;
  FTxStream := TMemoryStream.Create;
  FRxStream := TMemoryStream.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsMQTTParser.Destroy;
begin
  try
    FTxStream.Free;
    FRxStream.Free;
  finally
    inherited;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.Mon (const aStr: string);
begin
  if Assigned (FOnMon) then FOnMon (Self, 'P ' + aStr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.Parse (const aStr: AnsiString);
var
  aStream : TMemoryStream;
begin
  aStream := TMemoryStream.Create;
  if length (aStr) > 0 then     { V8.71 }
    aStream.Write (aStr[1], length (aStr));
  aStream.Seek (0, soFromBeginning);
  Parse (aStream);
  aStream.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.Reset;
begin
  FRxState := rsHdr;
  FRxStream.Clear;
  FTxStream.Clear;
  RxMsg := mtReserved15;
  RxDup := false;
  RxQOs := qtAT_MOST_ONCE;
  RxRetain := false;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.Parse (aStream: TStream);
var
  x, fl, vr, wq : byte;
  id, ka : Word;
  wr, wf, un, ps, cl: Boolean;
  wt, wm, ci, pt : UTF8String;
  aStr, bStr : UTF8String;
  Str : AnsiString;
  Strs : TStringList;
  Qoss : array of TMQTTQOSType;
begin
  while aStream.Position <> aStream.Size do
    begin
      case FRxState of
        rsHdr :
          begin
            ReadHdr (aStream, RxMsg, RxDup, RxQos, RxRetain);
            FRxState := rsLen;
            FRxMult := 1;
            FRxVal := 0;
            if Assigned (FOnHeader) then
                FOnHeader (Self, RxMsg, RxDup, RxQos, RxRetain);
          end;
        rsLen :
          begin
            x := ReadByte (aStream);
            FRxVal := FRxVal + ((x and $7f) * FRxMult);
            FRxMult := FRxMult * $80;
            if (x and $80) = 0 then
              begin
                FKeepAliveCount := KeepAlive * 10;
                FRxStream.Clear;
                if FRxVal = 0 then
                  begin
                    case RxMsg of
                      mtPINGREQ     :
                        if Assigned (FOnPing) then
                            FOnPing (Self);
                      mtPINGRESP    :
                        if Assigned (FOnPingResp) then
                            FOnPingResp (Self);
                      mtDISCONNECT  :
                        if Assigned (FOnDisconnect) then
                            FOnDisconnect (Self);
                      end;
                    FRxState := rsHdr;
                  end
                else
                  begin
                    FRxState := rsVarHdr;
                  end;
              end;
          end;
        rsVarHdr :
          begin
            x := ReadByte (aStream);
            FRxStream.Write (x, 1);
            FRxVal := FRxVal - 1;
            if FRxVal = 0 then
              begin
                FRxStream.Seek (0, soFromBeginning);
                case RxMsg of
                  mtBROKERCONNECT,
                  mtCONNECT  :
                    begin
                      pt := ReadStr (FRxStream);        // protocol
                      vr := ReadByte (FRxStream);       // version
                      fl := ReadByte (FRxStream);
                      ka := ReadByte (FRxStream) * $100 + ReadByte (FRxStream);
                      ci := ReadStr (FRxStream);
                      wf := (fl and $04) > 0;     // will flag
                      wr := (fl and $20) > 0;     // will retain
                      wq := (fl and $18) shr 3;   // will qos
                      un := (fl and $80) > 0;     // user name
                      ps := (fl and $40) > 0;     // pass word
                      cl := (fl and $02) > 0;     // clean
                      wt := '';
                      wm := '';
                      if wf then
                        begin
                          wt := ReadStr (FRxStream);        // will topic
                          wm := ReadStr (FRxStream);        // will message
                          if Assigned (FOnSetWill) then
                            FOnSetWill (Self, wt, wm, TMQTTQOSType (wq), wr);
                        end;
                      aStr := '';
                      bStr := '';
                      if un then aStr := ReadStr (FRxStream);        // username
                      if ps then bStr := ReadStr (FRxStream);        // password
                      if RxMsg = mtCONNECT then
                        begin
                          if Assigned (FOnConnect) then
                            FOnConnect (Self, pt, vr, ci, aStr, bStr, ka, cl);
                        end
                      else if RxMsg = mtBROKERCONNECT then
                        begin
                          if Assigned (FOnBrokerConnect) then
                            FOnBrokerConnect (Self, pt, vr, ci, aStr, bStr, ka, cl);
                        end;
                    end;
                  mtPUBLISH  :
                    if FRxStream.Size >= 4 then
                      begin
                        aStr := ReadStr (FRxStream);
                        if RxQos in [qtAT_LEAST_ONCE, qtEXACTLY_ONCE] then
                          id := ReadByte (FRxStream) * $100 + ReadByte (FRxStream)
                        else
                          id := 0;   // no id when RxQos = 0
                        SetLength (Str, FRxStream.Size - FRxStream.Position);
                        if length (Str) > 0 then
                            FRxStream.Read (Str[1], length (Str));
                        if Assigned (FOnPublish) then
                            FOnPublish (Self, id, aStr, Str);
                      end;
                  mtPUBACK,
                  mtPUBREC,
                  mtPUBREL,
                  mtPUBCOMP  :
                    if FRxStream.Size = 2 then
                      begin
                        id := ReadByte (FRxStream) * $100 + ReadByte (FRxStream);
                        case RxMsg of
                          mtPUBACK  : if Assigned (FOnPubAck) then
                                            FonPubAck (Self, id);
                          mtPUBREC  : if Assigned (FOnPubRec) then
                                            FonPubRec (Self, id);
                          mtPUBREL  : if Assigned (FOnPubRel) then
                                            FonPubRel (Self, id);
                          mtPUBCOMP : if Assigned (FOnPubComp) then
                                            FonPubComp (Self, id);
                        end;
                      end;
                  mtCONNACK     :
                    if FRxStream.Size = 2 then
                      begin
                        ReadByte (FRxStream);
                        id := ReadByte (FRxStream);
                        if Assigned (FOnConnAck) then
                          FOnConnAck (Self, id);
                      end;
                  mtSUBACK     :
                    if FRxStream.Size >= 2 then
                      begin
                        SetLength (Qoss, 0);
                        id := ReadByte (FRxStream) * $100 + ReadByte (FRxStream);
                        while FRxStream.Position < FRxStream.Size do
                          begin
                            SetLength (Qoss, Length (Qoss) + 1);
                            Qoss[Length (Qoss) - 1] := TMQTTQOSType (ReadByte (FRxStream) and $03);
                          end;
                        if Assigned (FOnSubAck) then
                            FOnSubAck (Self, id, Qoss);
                      end;
                  mtUNSUBACK    :
                    if FRxStream.Size = 2 then
                      begin
                        ReadByte (FRxStream);
                        id := ReadByte (FRxStream);
                        if Assigned (FOnUnsubAck) then
                          FOnUnsubAck (Self, id);
                      end;
                  mtUNSUBSCRIBE  :
                   if FRxStream.Size >= 2 then
                      begin
                        id := ReadByte (FRxStream) * $100 + ReadByte (FRxStream);
                        Strs := TStringList.Create;
                        while FRxStream.Size >= FRxStream.Position + 2  do   // len
                          begin
                            aStr := ReadStr (FRxStream);
                            Strs.Add (string (aStr));
                          end;
                        if Assigned (FOnUnsubscribe) then
                            FOnUnsubscribe (Self, id, Strs);
                        Strs.Free;
                      end;
                  mtSUBSCRIBE    :
                   if FRxStream.Size >= 2 then
                      begin
                        id := ReadByte (FRxStream) * $100 + ReadByte (FRxStream);
                        Strs := TStringList.Create;
                        while FRxStream.Size >= FRxStream.Position + 3  do   // len + qos
                          begin
                            aStr := ReadStr (FRxStream);
                            x := ReadByte (FRxStream) and $03;
                            Strs.AddObject (string (aStr), TObject (x));
                          end;
                        if Assigned (FOnSubscribe) then
                            FOnSubscribe (Self, id, Strs);
                        Strs.Free;
                      end;
                end;
                FKeepAliveCount := KeepAlive * 10;
                FRxState := rsHdr;
              end;
          end;
      end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendConnect (aClientID, aUsername, aPassword : UTF8String; aKeepAlive : Word; aClean : Boolean);
var
  s : TMemoryStream;
  x : byte;
begin
  KeepAlive := aKeepAlive;

  FTxStream.Clear;        // dup, qos, retain not used
  AddHdr (FTxStream, mtCONNECT, false, qtAT_LEAST_ONCE, false);
  s := TMemoryStream.Create;
  // generate payload
  AddStr (s, aClientID);
  if FWillFlag then
    begin
      AddStr (s, WillTopic);
      AddStr (s, WillMessage);
    end;
  if length (aUserName) > 0 then
    AddStr (s, aUserName);
  if length (aPassword) > 0 then
    AddStr (s, aPassword);
        // finish fixed header
  AddLength (FTxStream, 12 + s.Size);
      // variable header
  AddStr (FTxStream, MQTT_PROTOCOL);         // 00 06  MQIsdp  (8)
  AddByte (FTxStream, MQTT_VERSION);         // 3              (1)
  x := 0;
  if length (aUserName) > 0 then
    x := x or $80;
  if length (aPassword) > 0 then
    x := x or $40;
  if FWillFlag then
    begin
      x := x or $04;
      if WillRetain then
        x := x or $20;
      x := x or (ord (WillQos) shl 3);
    end;
  if Clean then
    x := x or $02;
  AddByte (FTxStream, x);                    //              (1)
  AddByte (FTxStream, aKeepAlive div $100);  //              (1)
  AddByte (FTxStream, aKeepAlive mod $100);  //              (1)
  // payload
  s.Seek (0, soFromBeginning);
  FTxStream.CopyFrom (s, s.Size);
  s.Free;
  if Assigned (FOnSend) then
    FOnSend (Self, 0, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendBrokerConnect(aClientID, aUsername, aPassword: UTF8String; aKeepAlive: Word; aClean: Boolean);
var
  s : TMemoryStream;
  x : byte;
begin
  KeepAlive := aKeepAlive;
  FTxStream.Clear;        // dup, qos, retain not used
  AddHdr (FTxStream, mtBROKERCONNECT, false, qtAT_LEAST_ONCE, false);
  s := TMemoryStream.Create;
  // generate payload
  AddStr (s, aClientID);
  if FWillFlag then
    begin
      AddStr (s, WillTopic);
      AddStr (s, WillMessage);
    end;
  if length (aUserName) > 0 then
    AddStr (s, aUserName);
  if length (aPassword) > 0 then
    AddStr (s, aPassword);
        // finish fixed header
  AddLength (FTxStream, 12 + s.Size);
      // variable header
  AddStr (FTxStream, MQTT_PROTOCOL);         // 00 06  MQIsdp  (8)
  AddByte (FTxStream, MQTT_VERSION);         // 3              (1)
  x := 0;
  if length (aUserName) > 0 then
    x := x or $80;
  if length (aPassword) > 0 then
    x := x or $40;
  if FWillFlag then
    begin
      x := x or $04;
      if WillRetain then
        x := x or $20;
      x := x or (ord (WillQos) shl 3);
    end;
  if Clean then
    x := x or $02;
  AddByte (FTxStream, x);                    //              (1)
  AddByte (FTxStream, aKeepAlive div $100);  //              (1)
  AddByte (FTxStream, aKeepAlive mod $100);  //              (1)
  // payload
  s.Seek (0, soFromBeginning);
  FTxStream.CopyFrom (s, s.Size);
  s.Free;
  if Assigned (FOnSend) then
    FOnSend (Self, 0, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendConnAck (aCode: byte);
begin
  FTxStream.Clear;        // dup, qos, retain not used
  AddHdr (FTxStream, mtCONNACK, false, qtAT_MOST_ONCE, false);
  AddLength (FTxStream, 2);
  AddByte (FTxStream, 0);                    // reserved      (1)
  AddByte (FTxStream, aCode);         //                (1)
  if Assigned (FOnSend) then
    FOnSend (Self, 0, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendPublish (anID : Word; aTopic : UTF8String; const aMessage : AnsiString; aQos : TMQTTQOSType; aDup : boolean = false; aRetain : boolean = false);
var
  s : TMemoryStream;
begin
  FTxStream.Clear;     // dup qos and retain used
  AddHdr (FTxStream, mtPUBLISH, aDup, aQos, aRetain);
  s := TMemoryStream.Create;
  AddStr (s, aTopic);
  if aQos in [qtAT_LEAST_ONCE, qtEXACTLY_ONCE] then
    begin
      AddByte (s, anID div $100);
      AddByte (s, anID mod $100);
    end;
  if length (aMessage) > 0 then
    s.Write (aMessage[1], length (aMessage));
  // payload
  s.Seek (0, soFromBeginning);
  AddLength (FTxStream, s.Size);
  FTxStream.CopyFrom (s, s.Size);
  s.Free;
  if Assigned (FOnSend) then
    FOnSend (Self, anID, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendPubAck (anID: Word);
begin
  FTxStream.Clear;        // dup, qos, retain not used
  AddHdr (FTxStream, mtPUBACK, false, qtAT_MOST_ONCE, false);
  AddLength (FTxStream, 2);
  AddByte (FTxStream, anID div $100);
  AddByte (FTxStream, anID mod $100);
  if Assigned (FOnSend) then
    FOnSend (Self, anID, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendPubRec (anID: Word);
begin
  FTxStream.Clear;       // dup, qos, retain are used
  AddHdr (FTxStream, mtPUBREC, false, qtAT_MOST_ONCE, false);
  AddLength (FTxStream, 2);
  AddByte (FTxStream, anID div $100);
  AddByte (FTxStream, anID mod $100);
  if Assigned (FOnSend) then
    FOnSend (Self, anID, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendPubRel (anID: Word; aDup : Boolean = false);
begin
  FTxStream.Clear;
  AddHdr (FTxStream, mtPUBREL, aDup, qtAT_LEAST_ONCE, false);
  AddLength (FTxStream, 2);
  AddByte (FTxStream, anID div $100);
  AddByte (FTxStream, anID mod $100);
  if Assigned (FOnSend) then
    FOnSend (Self, anID, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendPubComp (anID: Word);
begin
  FTxStream.Clear;        // dup, qos, retain not used
  AddHdr (FTxStream, mtPUBCOMP, false, qtAT_MOST_ONCE, false);
  AddLength (FTxStream, 2);
  AddByte (FTxStream, anID div $100);
  AddByte (FTxStream, anID mod $100);
  if Assigned (FOnSend) then
    FOnSend (Self, anID, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendSubscribe (anID : Word; aTopic : UTF8String; aQOS : TMQTTQOSType);
begin
  FTxStream.Clear;                // qos and dup used
  AddHdr (FTxStream, mtSUBSCRIBE, false, qtAT_LEAST_ONCE, false);
  AddLength (FTxStream, 5 + length (aTopic));
  AddByte (FTxStream, anID div $100);
  AddByte (FTxStream, anID mod $100);
  AddStr (FTxStream, aTopic);
  AddByte (FTxStream, ord (aQos));
  if Assigned (FOnSend) then
    FOnSend (Self, anID, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendSubscribe (anID: Word; Topics: TStringList);
var
  i : integer;
  s : TMemoryStream;
begin
  FTxStream.Clear;     // dup qos and retain used
  AddHdr (FTxStream, mtSUBSCRIBE, false, qtAT_LEAST_ONCE, false);
  s := TMemoryStream.Create;
  AddByte (s, anID div $100);
  AddByte (s, anID mod $100);
  for i := 0 to Topics.Count - 1 do
    begin
      AddStr (s, UTF8String (Topics[i]));
      AddByte (s, byte (Topics.Objects[i]) and $03);
    end;
  // payload
  s.Seek (0, soFromBeginning);
  AddLength (FTxStream, s.Size);
  FTxStream.CopyFrom (s, s.Size);
  s.Free;
  if Assigned (FOnSend) then
    FOnSend (Self, anID, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendUnsubscribe (anID: Word; Topics: TStringList);
var
  i : integer;
  s : TMemoryStream;
begin
  FTxStream.Clear;      // qos and dup used
  AddHdr (FTxStream, mtUNSUBSCRIBE, false, qtAT_LEAST_ONCE, false);
  s := TMemoryStream.Create;
  AddByte (s, anID div $100);
  AddByte (s, anID mod $100);
  for i := 0 to Topics.Count - 1 do
    AddStr (s, UTF8String (Topics[i]));
  // payload
  s.Seek (0, soFromBeginning);
  AddLength (FTxStream, s.Size);
  FTxStream.CopyFrom (s, s.Size);
  s.Free;
  if Assigned (FOnSend) then
    FOnSend (Self, anID, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendSubAck (anID: Word; Qoss : array of TMQTTQOSType);
var
  i : integer;
begin
  FTxStream.Clear;      // dup, qos, retain not used
  AddHdr (FTxStream, mtSUBACK, false, qtAT_MOST_ONCE, false);
  AddLength (FTxStream, 2 + length (Qoss));
  AddByte (FTxStream, anID div $100);
  AddByte (FTxStream, anID mod $100);
  for i := low (Qoss) to high (Qoss) do
    AddByte (FTxStream, ord (Qoss[i]));
  if Assigned (FOnSend) then
    FOnSend (Self, anID, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendUnsubscribe (anID: Word; aTopic: UTF8String);
begin
  FTxStream.Clear;      // qos and dup used
  AddHdr (FTxStream, mtUNSUBSCRIBE, false, qtAT_LEAST_ONCE, false);
  AddLength (FTxStream, 4 + length (aTopic));
  AddByte (FTxStream, anID div $100);
  AddByte (FTxStream, anID mod $100);
  AddStr (FTxStream, aTopic);
  if Assigned (FOnSend) then
    FOnSend (Self, anID, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendUnsubAck (anID: Word);
begin
  FTxStream.Clear;        // dup, qos, retain not used
  AddHdr (FTxStream, mtUNSUBACK, false, qtAT_MOST_ONCE, false);
  AddLength (FTxStream, 2);
  AddByte (FTxStream, anID div $100);
  AddByte (FTxStream, anID mod $100);
  if Assigned (FOnSend) then
    FOnSend (Self, anID, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendPing;
begin
  FTxStream.Clear;        // dup, qos, retain not used
  AddHdr (FTxStream, mtPINGREQ, false, qtAT_MOST_ONCE, false);
  AddLength (FTxStream, 0);
  if Assigned (FOnSend) then
    FOnSend (Self, 0, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendPingResp;
begin
  FTxStream.Clear;         // dup, qos, retain not used
  AddHdr (FTxStream, mtPINGRESP, false, qtAT_MOST_ONCE, false);
  AddLength (FTxStream, 0);
  if Assigned (FOnSend) then
    FOnSend (Self, 0, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SendDisconnect;
begin
  FTxStream.Clear;
  AddHdr (FTxStream, mtDISCONNECT, false, qtAT_MOST_ONCE, false);
  AddLength (FTxStream, 0);
  if Assigned (FOnSend) then
    FOnSend (Self, 0, 0, FTxStream);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SetKeepAlive (const Value: Word);
begin
  FKeepAlive := Value;
  FKeepAliveCount := Value * 10;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTParser.SetWill (aTopic, aMessage : UTF8String; aQos : TMQTTQOSType; aRetain : boolean);
begin
  WillTopic := aTopic;
  WillMessage := aMessage;
  WillRetain := aRetain;
  WillQos := aQos;
  FWillFlag := (length (aTopic) > 0) and (length (aMessage) > 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StateStr (aState: TSocketState): string;
begin
  case aState of
    wsInvalidState    : result := 'Invalid State';
    wsOpened          : result := 'Opened';
    wsBound           : result := 'Bound';
    wsConnecting      : Result := 'Connecting';
    wsSocksConnected  : Result := 'Sock Connected';
    wsConnected       : result := 'Connected';
    wsAccepting       : result := 'Accepting';
    wsListening       : result := 'Listening';
    wsClosed          : result := 'Closed';
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SubTopics (aTopic : UTF8String) : TStringList;
var
  i : integer;
begin
  Result := TStringList.Create;
  Result.Add ('');
  for i := 1 to length (aTopic) do
    begin
      if aTopic[i] = '/' then
        Result.Add('')
      else
        Result[Result.Count - 1] := Result[Result.Count - 1] + Char (aTopic[i]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsSubscribed (aSubscription, aTopic : UTF8String) : boolean;
var
  s, t : TStringList;
  i : integer;
  MultiLevel : Boolean;
begin
  s := SubTopics (aSubscription);
  t := SubTopics (aTopic);
  MultiLevel := (s[s.Count - 1] = '#');   // last field is #
  if not MultiLevel then
    Result := (s.Count = t.Count)
  else
    Result := (s.Count <= t.Count + 1);
  if Result then
    begin
      for i := 0 to s.Count - 1 do
        begin
          if (i >= t.Count) then Result := MultiLevel
          else if (i = s.Count - 1) and (s[i] = '#') then break
          else if s[i] = '+' then continue    // they match
          else
            Result := Result and (s[i] = t[i]);
          if not Result then break;
        end;
    end;
  s.Free;
  t.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure SetDup (aStream : TMemoryStream; aState : boolean);
var
  x : byte;
begin
  if aStream.Size = 0 then exit;
  aStream.Seek (0, soFromBeginning);
  aStream.Read (x, 1);
  x := (x and $F7) or (ord (aState) * $08);
  aStream.Seek (0, soFromBeginning);
  aStream.Write (x, 1);
end;


{ TSrvrClient }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSrvrClient.Create (anOwner: TComponent);
begin
  inherited;
  FBroker := false;       // non standard
  Parser := TIcsMQTTParser.Create;
  Parser.OnSend := DoSend;
  Parser.OnSetWill := DoSetWill;
  Parser.OnSubscribe := RxSubscribe;
  Parser.OnUnsubscribe := RxUnsubscribe;
  Parser.OnPubAck := RxPubAck;
  Parser.OnPubRel := RxPubRel;
  Parser.OnPubRec := RxPubRec;
  Parser.OnPubComp := RxPubComp;
  InFlight := TMQTTPacketStore.Create;
  Releasables := TMQTTMessageStore.Create;
  Subscriptions := TStringList.Create;
  OnDataAvailable := DoData;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSrvrClient.Destroy;
begin
  try
      InFlight.Clear;
      InFlight.Free;
      Releasables.Clear;
      Releasables.Free;
      Parser.Free;
      Subscriptions.Free;
  finally
      inherited;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvrClient.DoData (Sender: TObject; ErrCode: Word);
begin
  if ErrCode = 0 then
    Parser.Parse (ReceiveStrA);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvrClient.DoSend (Sender: TObject; anID : Word; aRetry : integer; aStream: TMemoryStream);
var
  x : byte;
begin
  if State = wsConnected then
    begin
      aStream.Seek (0, soFromBeginning);
      aStream.Read (x, 1);
      if (TMQTTQOSType ((x and $06) shr 1) in [qtAT_LEAST_ONCE, qtEXACTLY_ONCE]) and
         (TMQTTMessageType ((x and $f0) shr 4) in [{mtPUBREL,} mtPUBLISH, mtSUBSCRIBE, mtUNSUBSCRIBE]) and (anID > 0) then
        begin
          InFlight.AddPacket (anID, aStream, aRetry, Parser.RetryTime);      // start disabled
          mon (string (Parser.ClientID) + ' Message ' + IntToStr (anID) + ' created.');
        end;
      Send (aStream.Memory, aStream.Size);
      Sleep (0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvrClient.DoSetWill (Sender: TObject; aTopic, aMessage: UTF8String; aQos : TMQTTQOSType; aRetain: boolean);
begin
  Parser.WillTopic := aTopic;
  Parser.WillMessage := aMessage;
  Parser.WillQos := aQos;
  Parser.WillRetain := aRetain;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvrClient.Mon (const aStr: string);
begin
  if Assigned (FOnMon) then
    FOnMon (Self, aStr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvrClient.RxPubAck (Sender: TObject; anID: Word);
begin
  InFlight.DelPacket (anID);
  Mon (string (Parser.ClientID) + ' ACK Message ' + IntToStr (anID) + ' disposed of.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvrClient.RxPubComp (Sender: TObject; anID: Word);
begin
  InFlight.DelPacket (anID);
  Mon (string (Parser.ClientID) + ' COMP Message ' + IntToStr (anID) + ' disposed of.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvrClient.RxPubRec (Sender: TObject; anID: Word);
var
  aPacket : TMQTTPacket;
begin
  aPacket := InFlight.GetPacket (anID);
  if aPacket <> nil then
    begin
      aPacket.Counter := Parser.RetryTime;
      if aPacket.Publishing then
        begin
          aPacket.Publishing := false;
          Mon (string (Parser.ClientID) + ' REC Message ' + IntToStr (anID) + ' recorded.');
        end
      else
        Mon (string (Parser.ClientID) + ' REC Message ' + IntToStr (anID) + ' already recorded.');
    end
  else
    Mon (string (Parser.ClientID) + ' REC Message ' + IntToStr (anID) + ' not found.');
  Parser.SendPubRel (anID);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvrClient.RxPubRel (Sender: TObject; anID: Word);
var
  aMsg : TMQTTMessage;
begin
  aMsg := Releasables.GetMsg (anID);
  if (aMsg <> nil) and (Owner.Owner is TIcsMQTTServer) then
    begin
      Mon (string (Parser.ClientID) + ' REL Message ' + IntToStr (anID) + ' publishing @ ' + QOSNames[aMsg.Qos]);
      TIcsMQTTServer (Owner.Owner).PublishToAll (Self, aMsg.Topic, aMsg.Message, aMsg.Qos);
      Releasables.Remove (aMsg);
      aMsg.Free;
      Mon (string (Parser.ClientID) + ' REL Message ' + IntToStr (anID) + ' removed from storage.');
    end
  else
    Mon (string (Parser.ClientID) + ' REL Message ' + IntToStr (anID) + ' has been already removed from storage.');
  Parser.SendPubComp (anID);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvrClient.RxSubscribe (Sender: TObject; anID: Word; Topics: TStringList);
var
  x : cardinal;
  q : TMQTTQOSType;
  i, j : integer;
  found : boolean;
  Qoss : array of TMQTTQOSType;
  aServer : TIcsMQTTServer;
  bMsg : TMQTTMessage;
  aQos : TMQTTQOSType;
begin
  SetLength (Qoss, Topics.Count);
  aServer := nil;
  if Owner is TWSocketServer then
    if Owner.Owner is TIcsMQTTServer then
      aServer := TIcsMQTTServer (Owner.Owner);
  if aServer = nil then exit;
  for i := 0 to Topics.Count - 1 do
    begin
      found := false;
      x := cardinal (Topics.Objects[i]) and $03;
      q := TMQTTQOSType (x);
      if Assigned (aServer.FOnSubscription) then
        aServer.FOnSubscription (Self, UTF8String (Topics[i]), q);
      for j := 0 to Subscriptions.Count - 1 do
        if Subscriptions[j] = Topics[i] then
          begin
            found := true;
            Subscriptions.Objects[j] := TObject (q);
            break;
          end;
      if not found then
        begin
          Subscriptions.AddObject (Topics[i], TObject (q));
        end;
      Qoss[i] := q;
      for j := 0 to aServer.Retained.Count - 1 do     // set retained
        begin
          bMsg := aServer.Retained[j];
          if IsSubscribed (UTF8String (Topics[i]), bMsg.Topic) then
            begin
              aQos := bMsg.Qos;
              if q < aQos then aQos := q;
              bMsg.LastUsed := Now;
              Parser.SendPublish (aServer.NextMessageID, bMsg.Topic, bMsg.Message, aQos, false, true);
            end;
        end;
    end;
  if Parser.RxQos = qtAT_LEAST_ONCE then
    Parser.SendSubAck (anID, Qoss);
  if Assigned (FOnSubscriptionChange) then
    FOnSubscriptionChange (Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvrClient.RxUnsubscribe (Sender: TObject; anID: Word; Topics: TStringList);
var
  i, j : integer;
  changed : boolean;
begin
  changed := false;
  for i := 0 to Topics.Count - 1 do
    begin
      for j := Subscriptions.Count - 1 downto 0 do
        begin
          if Subscriptions[j] = Topics[i] then
            begin
              Subscriptions.Delete (j);
              changed := true;
            end;
        end;
    end;
  if changed and  Assigned (FOnSubscriptionChange) then
    FOnSubscriptionChange (Self);
  if Parser.RxQos = qtAT_LEAST_ONCE then
    Parser.SendUnSubAck (anID);
end;


{ TIcsMQTTServer }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsMQTTServer.Create (anOwner: TComponent);
begin
  inherited;
  Timers := AllocateHWnd (TimerProc);
  MessageID := 1000;
  FOnMonHdr := nil;
  FMaxRetries := DefMaxRetries;
  FRetryTime := DefRetryTime;
  Brokers := TList.Create;
  Sessions := TMQTTSessionStore.Create;
  Retained := TMQTTMessageStore.Create;
  FServer := TSslWSocketServer.Create (Self);
  FServer.OnClientCreate := DoClientCreate;
  FServer.OnClientDisconnect := DoClientDisconnect;
  FServer.OnClientConnect := DoClientConnect;
//  FServer.OnSslHandshakeDone :=  only needed if we want to check client certificates
  FServer.SslMode := sslModeServer ;
  FServer.IcsHosts.Add;                           { V8.71 }
  FServer.IcsHosts[0].BindIpAddr := '0.0.0.0';
  FServer.IcsHosts[0].BindNonPort := 1883;
//  FServer.IcsHosts[0].BindSslPort := 8883;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsMQTTServer.Destroy;
var
  i : integer;
begin
  try
      Activate (false);
      DeallocateHWnd (Timers);
      for i := 0 to Brokers.Count - 1 do
        TIcsMQTTClient (Brokers[i]).Free;
      Brokers.Free;
      Retained.Free;
      Sessions.Free;
      FServer.Free;
  finally
    inherited;
  end;
end;


procedure TIcsMQTTServer.Activate (Enable: boolean);
var
  i : integer;
  Errs: String;
begin
  if FEnable = Enable then
    exit;
  if (Enable) then
    begin
        FServer.Banner := '';
        if Server.IcsHosts.Count = 0 then    { V8.71 sanity check }
        begin
            Mon('Failed to start server, no IcsHosts specified');
            Exit;
        end;
        FServer.Proto := 'tcp';
        FServer.ClientClass := TSrvrClient;
        try
            if (FServer.RootCA = '') or (NOT FileExists(FServer.RootCA)) then
                FServer.RootCA := sslRootCACertsBundle;

        // validate hosts
            Errs := FServer.ValidateHosts(False, True, True); // don't stop on first error, no exceptions, allow self sign
            if Errs <> '' then
            begin
                Mon('Server Validation Errors:' + icsCRLF + Errs);
            end;
            Errs := FServer.MultiListenEx;      { V8.71 using IcsHosts }
            if Errs <> '' then
                Mon(Errs);
            if FServer.ListenAllOK then
            begin
                FEnable := true;
                Mon('Listen Bindings:' + icsCRLF + FServer.ListenStates);
                if (FServer.IcsHosts[0].CertInfo <> '') then   // assume only one binding
                    Mon (FServer.IcsHosts[0].CertInfo) ;
            end
            else
                Mon('Failed Listen Bindings:' + icsCRLF + FServer.ListenStates);
        except
            on E:Exception do
            begin
                Mon('Failed to Listen: ' + E.Message);
              //  Mon('Failed Listen Bindings:' + icsCRLF + FServer.ListenStates);
                FEnable := false;
            end;
        end;
        if FEnable then
            SetTimer (Timers, 3, 100, nil);
    end
  else
    begin
      FEnable := false;
      for i := 0 to FServer.ClientCount - 1 do
        try
          TSrvrClient (FServer.Client[i]).Close;
        except
        end;
      try
        FServer.MultiClose;
      except
      end;
      KillTimer (Timers, 1);
      KillTimer (Timers, 2);
      KillTimer (Timers, 3);
    end;
  if Assigned (FOnEnableChange) then
    FOnEnableChange (Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTServer.AddBroker (const aHost: string; aPort: integer): TIcsMQTTClient;
begin
  Result := TIcsMQTTClient.Create (Self);
  Result.Host := aHost;
  Result.Port := aPort;
  Result.Broker := true;
  Result.LocalBounce := false;
  Result.OnOnline := BkrOnline;
  Result.OnOffline := BkrOffline;
  Result.OnEnableChange := BkrEnableChanged;
  Result.OnMsg := BkrMsg;
  Brokers.Add (Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.BkrEnableChanged (Sender: TObject);
begin
  if Assigned (FOnBrokerEnableChange) then
    FOnBrokerEnableChange (Sender);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.BkrOffline (Sender: TObject; Graceful: boolean);
begin
  TIcsMQTTClient (Sender).Subscriptions.Clear;
  if Assigned (FOnBrokerOffline) then
    FOnBrokerOffline (Sender, Graceful);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.BkrOnline(Sender: TObject);
begin
  SyncBrokerSubscriptions (TIcsMQTTClient (Sender));
  if Assigned (FOnBrokerOnline) then
    FOnBrokerOnline (Sender);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.BkrMsg (Sender: TObject; aTopic : UTF8String; const aMessage : AnsiString; aQos : TMQTTQOSType; aRetained : boolean);
var
  aBroker : TIcsMQTTClient;
  i : integer;
  aMsg : TMQTTMessage;
begin
  aBroker := TIcsMQTTClient (Sender);
  mon ('Received Retained Message from a Broker - Retained ' + ny[aRetained]);
  if aRetained then
    begin
      mon ('Retaining "' + string (aTopic) + '" @ ' + QOSNames[aQos]);
      for i := Retained.Count - 1 downto 0 do
        begin
          aMsg := Retained[i];
          if aMsg.Topic = aTopic then
            begin
              Retained.Remove (aMsg);
              aMsg.Free;
              break;
            end;
        end;
      Retained.AddMsg (0, aTopic, aMessage, aQos, 0, 0);
    end
  else
    mon ('Received Message from a Broker - Publishing..');
  PublishToAll (Sender, aTopic, aMessage, aBroker.Parser.RxQos, aRetained);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.BkrSubscriptionChange(Sender: TObject);
var
  i : integer;
begin
  mon ('Subscriptions changed...');
  for i := 0 to Brokers.Count - 1 do
    SyncBrokerSubscriptions (TIcsMQTTClient (Brokers[i]));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTServer.GetIcsHosts: TIcsHostCollection;
begin
    if Assigned(FServer) then
        Result := FServer.GetIcsHosts
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.SetIcsHosts(const Value: TIcsHostCollection);
begin
    if Assigned(FServer) then
        FServer.SetIcsHosts(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTServer.GetRootCA: String;
begin
    if Assigned(FServer) then
        Result := FServer.RootCA
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.SetRootCA(const Value: String);
begin
    if Assigned(FServer) then
        FServer.RootCA := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTServer.GetSslCertAutoOrder: Boolean;                       { V8.57 }
begin
    if Assigned(FServer) then
        Result := TSslWSocketServer(FServer).SslCertAutoOrder
    else
        Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.SetSslCertAutoOrder(const Value : Boolean);         { V8.57 }
begin
    if Assigned(FServer) then
        TSslWSocketServer(FServer).SslCertAutoOrder := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTServer.GetCertExpireDays: Integer;                         { V8.57 }
begin
    if Assigned(FServer) then
        Result := TSslWSocketServer(FServer).CertExpireDays
    else
        Result := 30;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.SetCertExpireDays(const Value : Integer);           { V8.57 }
begin
    if Assigned(FServer) then
        TSslWSocketServer(FServer).CertExpireDays := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF AUTO_X509_CERTS}  { V8.59 }
function TIcsMQTTServer.GetSslX509Certs: TSslX509Certs;    { V8.57 }
begin
    if Assigned(FServer) then
        Result := TSslWSocketServer(FServer).GetSslX509Certs as TSslX509Certs
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.SetSslX509Certs(const Value : TSslX509Certs);    { V8.57 }
begin
    if Assigned(FServer) then
        TSslWSocketServer(FServer).SetSslX509Certs(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTServer.GetOcspSrvStapling: Boolean;                        { V8.69 }
begin
    if Assigned(FServer) then
        Result := TSslWSocketServer(FServer).OcspSrvStapling
    else
        Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.SetOcspSrvStapling(const Value : Boolean);          { V8.69 }
begin
    if Assigned(FServer) then
        TSslWSocketServer(FServer).OcspSrvStapling := Value;
end;


{$ENDIF} // AUTO_X509_CERTS
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTServer.ValidateHosts(Stop1stErr: Boolean=True; NoExceptions: Boolean=False): String;
begin
    Result := '';
    if Assigned(FServer) then begin
        Result := TSslWSocketServer(FServer).ValidateHosts(Stop1stErr, NoExceptions);
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTServer.RecheckSslCerts(var CertsInfo: String; Stop1stErr: Boolean=True; NoExceptions: Boolean=False): Boolean;
begin
    Result := False;
    if Assigned(FServer) then begin
        Result := TSslWSocketServer(FServer).RecheckSslCerts(CertsInfo, Stop1stErr, NoExceptions);
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTServer.ListenAllOK: Boolean;
begin
    if Assigned(FServer) then
        Result := FServer.ListenAllOK
    else
        Result := False;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTServer.ListenStates: String;
begin
    if Assigned(FServer) then
        Result := FServer.ListenStates
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.RxPing (Sender: TObject);
begin
  if not (Sender is TIcsMQTTParser) then exit;
  TIcsMQTTParser (Sender).SendPingResp;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.RxPublish (Sender: TObject; anID: Word; aTopic : UTF8String; const aMessage: AnsiString);
var
  aParser : TIcsMQTTParser;
  aClient : TSrvrClient;
  aMsg : TMQTTMessage;
  i : integer;

begin
  if not (Sender is TIcsMQTTParser) then exit;
  aParser := TIcsMQTTParser (Sender);
  aClient := GetClient (aParser);
  if aClient = nil then exit;
  if aParser.RxRetain then
    begin
      mon ('Retaining "' + string (aTopic) + '" @ ' + QOSNames[aParser.RxQos]);
      for i := Retained.Count - 1 downto 0 do
        begin
          aMsg := Retained[i];
          if aMsg.Topic = aTopic then
            begin
              Retained.Remove (aMsg);
              aMsg.Free;
              break;
            end;
        end;
      Retained.AddMsg (0, aTopic, aMessage, aParser.RxQos, 0, 0);
    end;
  case aParser.RxQos of
    qtAT_MOST_ONCE  :
      PublishToAll (aClient, aTopic, aMessage, aParser.RxQos, aParser.RxRetain);
    qtAT_LEAST_ONCE :
      begin
        aParser.SendPubAck (anID);
        PublishToAll (aClient, aTopic, aMessage, aParser.RxQos, aParser.RxRetain);
      end;
    qtEXACTLY_ONCE  :
      begin
        aMsg := aClient.Releasables.GetMsg (anID);
        if aMsg = nil then
          begin
            aClient.Releasables.AddMsg (anID, aTopic, aMessage, aParser.RxQos, 0, 0);
            mon (string (aClient.Parser.ClientID) + ' Message ' + IntToStr (anID) + ' stored and idle.');
          end
        else
          mon (string (aClient.Parser.ClientID) + ' Message ' + IntToStr (anID) + ' already stored.');
        aParser.SendPubRec (anID);
      end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.LoadBrokers (const anIniFile: string);
var
  i : integer;
  Sections : TStringList;
  aBroker : TIcsMQTTClient;
  EnFlag : Boolean;
begin
  for i := 0 to Brokers.Count - 1 do
    TIcsMQTTClient (Brokers[i]).Free;
  Brokers.Clear;
  Sections := TStringList.Create;
  with TIcsIniFile.Create (anIniFile) do
    begin
      ReadSections (Sections);
      for i := 0 to Sections.Count - 1 do
        begin
          if Copy (Sections[i], 1, 6) = 'BROKER' then
            begin
              aBroker := AddBroker ('', 0);
              aBroker.Host := ReadString (Sections[i], 'Prim Host', '');
              aBroker.Port := ReadInteger (Sections[i], 'Port', 1883);
              EnFlag := ReadBool (Sections[i], 'Enabled', false);
              if EnFlag then
                aBroker.Activate (true);
            end;
        end;
      Free;
    end;
  Sections.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.SetMaxRetries (const Value: integer);
var
  i : integer;
begin
  FMaxRetries := Value;
  for i := 0 to Server.ClientCount - 1 do
    TSrvrClient (Server.Client[i]).Parser.MaxRetries := Value;
 for i := 0 to Brokers.Count - 1 do
    TIcsMQTTClient (Brokers[i]).Parser.MaxRetries := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.SetRetryTime (const Value: cardinal);
var
  i : integer;
begin
  FRetryTime := Value;
  for i := 0 to Server.ClientCount - 1 do
    TSrvrClient (Server.Client[i]).Parser.KeepAlive := Value;
 for i := 0 to Brokers.Count - 1 do
    TIcsMQTTClient (Brokers[i]).Parser.KeepAlive := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.StoreBrokers (const anIniFile: string);
var
  i : integer;
  aBroker : TIcsMQTTClient;
  Sections : TStringList;
begin
  Sections := TStringList.Create;
  with TIcsIniFile.Create (anIniFile) do
    begin
      ReadSections (Sections);
      for i := 0 to Sections.Count - 1 do
        if Copy (Sections[i], 1, 6) = 'BROKER' then
          EraseSection (Sections[i]);
      for i := 0 to Brokers.Count - 1 do
        begin
          aBroker := Brokers[i];
          WriteString (format ('BROKER%.3d', [i]), 'Prim Host', aBroker.Host);
          WriteInteger (format ('BROKER%.3d', [i]), 'Port', aBroker.Port);
          WriteBool (format ('BROKER%.3d', [i]), 'Enabled', aBroker.Enabled);
        end;
      Free;
    end;
  Sections.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.SyncBrokerSubscriptions (aBroker: TIcsMQTTClient);
var
  i, j, k : integer;
  x : cardinal;
  ToSub, ToUnsub : TStringList;
  aClient : TSrvrClient;
  found : boolean;
begin
  ToSub := TStringList.Create;
  ToUnsub := TStringList.Create;
  for i := 0 to Server.ClientCount - 1 do
    begin
      aClient := TSrvrClient (Server.Client[i]);
      for j := 0 to aClient.Subscriptions.Count - 1 do
        begin
          found := false;
          for k := 0 to ToSub.Count - 1 do
            begin
              if aClient.Subscriptions[j] = ToSub[k] then
                begin
                  found := true;
                  break;
                end;
            end;
          if not found then ToSub.AddObject (aClient.Subscriptions[j], aClient.Subscriptions.Objects[j]);
        end;
    end;
  // add no longer used to unsubscribe
  for i := aBroker.Subscriptions.Count - 1 downto 0 do
    begin
      found := false;
      for j := 0 to ToSub.Count - 1 do
        begin
          if aBroker.Subscriptions[i] = ToSub[j] then
            begin
              x := cardinal (aBroker.Subscriptions.Objects[i]) and $03;      // change to highest
              if x > (cardinal (ToSub.Objects[j]) and $03) then
                ToSub.Objects[j] := TObject (x);
              found := true;
              break;
            end;
        end;
      if not found then
        ToUnsub.AddObject (aBroker.Subscriptions[i], aBroker.Subscriptions.Objects[i]);
    end;
  // remove those already subscribed to
  for i := 0 to aBroker.Subscriptions.Count - 1 do
    begin
      for j := ToSub.Count - 1 downto 0 do
        begin
          if aBroker.Subscriptions[i] = ToSub[j] then
            ToSub.Delete (j);     // already subscribed
        end;
    end;
  for i := 0 to ToSub.Count - 1 do
    aBroker.Subscribe (UTF8String (ToSub[i]), TMQTTQOSType (cardinal (ToSub.Objects[i]) and $03));
  for i := 0 to ToUnsub.Count - 1 do
    aBroker.Unsubscribe (UTF8String (ToUnsub[i]));
  ToSub.Free;
  ToUnsub.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.Mon (const aStr: string);
begin
  if Assigned (FOnMon) then
    FOnMon (Self, aStr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTServer.NextMessageID: Word;
var
  i, j : integer;
  Unused : boolean;
  aMsg : TMQTTPacket;
  aClient : TSrvrClient;
begin
  repeat
    Unused := true;
    MessageID := MessageID + 1;
    if MessageID = 0 then MessageID := 1;   // exclude 0
    for i := 0 to Server.ClientCount - 1 do
      begin
        aClient := TSrvrClient (Server.Client[i]);
        for j := 0 to aClient.InFlight.Count - 1 do
          begin
            aMsg := aClient.InFlight[j];
            if aMsg.ID = MessageID then
              begin
                Unused := false;
                break;
              end;
          end;
        if not Unused then
            break;
      end;
  until Unused;
  Result := MessageID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.PublishToAll (From : TObject; aTopic : UTF8String; const aMessage : AnsiString; aQos : TMQTTQOSType; wasRetained : boolean);
var
  i, j : integer;
  sent : boolean;
  aClient : TSrvrClient;
  aBroker : TIcsMQTTClient;
  bQos : TMQTTQOSType;
begin
  mon ('Publishing -- Was Retained ' + ny[wasRetained]);
  for i := 0 to Server.ClientCount - 1 do
    begin
      aClient := TSrvrClient (Server.Client[i]);
      if (aClient = From) and (aClient.FBroker) then continue;  // don't send back to self if broker - non standard
      //not LocalBounce then continue;
      sent := false;
      for j := 0 to aClient.Subscriptions.Count - 1 do
        begin
          if IsSubscribed (UTF8String (aClient.Subscriptions[j]), aTopic) then
            begin
              bQos := TMQTTQOSType (cardinal (aClient.Subscriptions.Objects[j]) and $03);
              if aClient.FBroker then
                 mon ('Publishing to Broker ' + string (aClient.Parser.ClientID) + ' "' + string (aTopic) + '" Retained ' + ny[wasRetained and aClient.FBroker])

              else
                mon ('Publishing to Client ' + string (aClient.Parser.ClientID) + ' "' + string (aTopic) + '"');
              if bQos > aQos then bQos := aQos;
              aClient.Parser.SendPublish (NextMessageID, aTopic, aMessage, bQos, false, wasRetained and aClient.FBroker);
              sent := true;
              break;    // only do first
            end;
        end;
      if (not sent) and (wasRetained) and (aClient.FBroker) then
        begin
          mon ('Forwarding Retained message to broker');
          aClient.Parser.SendPublish (NextMessageID, aTopic, aMessage, qtAT_LEAST_ONCE, false, true);
        end;
    end;
  for i := 0 to Brokers.Count - 1 do    // brokers get all messages -> downstream
    begin
      aBroker := TIcsMQTTClient (Brokers[i]);
      if aBroker = From then
        continue;
      if not aBroker.Enabled then
        continue;
    //  if aBroker then
      mon ('Publishing to Broker ' + string (aBroker.ClientID) + ' "' + string (aTopic) + '" @ ' + QOSNames[aQos] + ' Retained ' + ny[wasretained]);
      aBroker.Publish (aTopic, aMessage, aQos, wasRetained);
   end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.TimerProc (var aMsg: TMessage);
var
  i, j : integer;
  bPacket : TMQTTPacket;
  aClient : TSrvrClient;
  WillClose : Boolean;
begin
  if aMsg.Msg = WM_TIMER then
    begin
      KillTimer (Timers, aMsg.WParam);
  //    Mon ('Timer ' + IntToStr (aMsg.WParam) + ' triggered');
      case aMsg.WParam of
        3 : begin
              for i := Server.ClientCount - 1 downto 0 do
                begin
                  aClient := TSrvrClient (Server.Client[i]);
                  if not aClient.Parser.CheckKeepAlive then
                    begin
                      WillClose := true;
                      if Assigned (FOnFailure) then
                        FOnFailure (aClient, frKEEPALIVE, WillClose);
                      if WillClose then
                        aClient.CloseDelayed;
                    end
                  else
                    begin
                      for j := aClient.InFlight.Count - 1 downto 0 do
                        begin
                          bPacket := aClient.InFlight[j];
                          if bPacket.Counter > 0 then
                            begin
                              bPacket.Counter := bPacket.Counter - 1;
                              if bPacket.Counter = 0 then
                                begin
                                  bPacket.Retries := bPacket.Retries + 1;
                                  if bPacket.Retries <= aClient.Parser.MaxRetries then
                                    begin
                                      if bPacket.Publishing then
                                        begin
                                          aClient.InFlight.List.Remove (bPacket);
                                          mon ('Message ' + IntToStr (bPacket.ID) + ' disposed of..');
                                          mon ('Re-issuing Message ' + inttostr (bPacket.ID) + ' Retry ' + inttostr (bPacket.Retries));
                                          SetDup (bPacket.Msg, true);
                                          aClient.DoSend (aClient.Parser, bPacket.ID, bPacket.Retries, bPacket.Msg);
                                          bPacket.Free;
                                        end
                                      else
                                        begin
                                          mon ('Re-issuing PUBREL Message ' + inttostr (bPacket.ID) + ' Retry ' + inttostr (bPacket.Retries));
                                          aClient.Parser.SendPubRel (bPacket.ID, true);
                                          bPacket.Counter := aClient.Parser.RetryTime;
                                        end;
                                    end
                                  else
                                    begin
                                      WillClose := true;
                                      if Assigned (FOnFailure) then
                                        FOnFailure (Self, frMAXRETRIES, WillClose);
                                      if WillClose then
                                        aClient.CloseDelayed;
                                    end;
                                end;
                            end;
                        end;
                    end;
                end;
              SetTimer (Timers, 3, 100, nil);
            end;
        end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.DoClientConnect (Sender: TObject; Client: TWSocketClient; Error: Word);
begin
  if Sender = Server then
    Mon ('Client Connected...');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.DoClientCreate (Sender: TObject; Client: TWSocketClient);
begin
  with TSrvrClient (Client) do
    begin
      Parser.OnPing := RxPing;
      Parser.OnDisconnect := RxDisconnect;
      Parser.OnPublish := RxPublish;
      Parser.OnPubRec := RxPubRec;
      Parser.OnConnect := RxConnect;
      Parser.OnBrokerConnect := RxBrokerConnect;    // non standard
      Parser.OnHeader := RxHeader;
      Parser.MaxRetries := FMaxRetries;
      Parser.RetryTime := FRetryTime;
      OnMon := DoMon;
      OnSubscriptionChange := BkrSubscriptionChange;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.DoClientDisconnect (Sender: TObject;  Client: TWSocketClient; Error: Word);
var
  aTopic, aMessage : UTF8String;
  aQos : TMQTTQOSType;
begin
  with TSrvrClient (Client) do
    begin
      Mon ('Client Disconnected.  Graceful ' + ny[TSrvrClient (Client).FGraceful]);
      if (InFlight.Count > 0) or (Releasables.Count > 0) then
        begin
          if Assigned (FOnStoreSession) then
            FOnStoreSession (Client, Parser.ClientID)
          else
            Sessions.StoreSession (Parser.ClientID, TSrvrClient (Client));
        end;
      if not FGraceful then
        begin
          aTopic := Parser.WillTopic;
          aMessage := Parser.WillMessage;
          aQos := Parser.WillQos;
          if Assigned (FOnObituary) then
            FOnObituary (Client, aTopic, aMessage, aQos);
          PublishToAll (nil, aTopic, AnsiString (aMessage), aQos);
        end;
    end;
  if Assigned (FOnClientsChange) then
    FOnClientsChange (Server, Server.ClientCount - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.DoMon (Sender: TObject; const aStr: string);
begin
  Mon (aStr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTServer.Enabled: boolean;
begin
  Result := FEnable;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTServer.GetClient (aClientID: UTF8String): TSrvrClient;
var
  i : integer;
begin
  for i := 0 to Server.ClientCount - 1 do
    begin
      Result := TSrvrClient (Server.Client[i]);
      if Result.Parser.ClientID = aClientID then exit;
    end;
(*  for i := 0 to BrokerServer.ClientCount - 1 do
    begin
      Result := TSrvrClient (BrokerServer.Client[i]);
      if Result.Parser.ClientID = aClientID then exit;
    end;       *)
  Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTServer.GetClient (aParser: TIcsMQTTParser): TSrvrClient;
var
  i : integer;
begin
  for i := 0 to Server.ClientCount - 1 do
    begin
      Result := TSrvrClient (Server.Client[i]);
      if Result.Parser = aParser then exit;
    end;
(*  for i := 0 to BrokerServer.ClientCount - 1 do
    begin
      Result := TSrvrClient (BrokerServer.Client[i]);
      if Result.Parser = aParser then exit;
    end;       *)
  Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.RxBrokerConnect(Sender: TObject; aProtocol: UTF8String; aVersion: byte; aClientID, aUserName,
                                                                    aPassword: UTF8String; aKeepAlive: Word; aClean: Boolean);
var
  aClient : TSrvrClient;
begin
  if not (Sender is TIcsMQTTParser) then exit;
  aClient := GetClient (TIcsMQTTParser (Sender));
  if aClient = nil then exit;
  aClient.FBroker := true;
  RxConnect (Sender, aProtocol, aVersion, aClientID, aUserName, aPassword, aKeepAlive, aClean);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.RxConnect (Sender: TObject; aProtocol: UTF8String; aVersion: byte; aClientID, aUserName,
                                                                                        aPassword: UTF8String; aKeepAlive: Word;
  aClean: Boolean);
var
  aClient : TSrvrClient;
  aServer : TWSocketServer;
  Allowed : Boolean;
begin
  Allowed := false;
  if not (Sender is TIcsMQTTParser) then exit;
  aClient := GetClient (TIcsMQTTParser (Sender));
  if aClient = nil then exit;
  aServer := TWSocketServer (aClient.Owner);
  aClient.FGraceful := true;
  if Assigned (FOnCheckUser) then
    FOnCheckUser (aServer, aUserName, aPassword, Allowed);
  if Allowed then
    begin
      if aVersion < MQTT_MinVersion then
        begin
          aClient.Parser.SendConnAck (rcPROTOCOL);  // identifier rejected
          aClient.CloseDelayed;
        end
      else if (length (aClientID) < 1) or (length (aClientID) > 23) then
        begin
          aClient.Parser.SendConnAck (rcIDENTIFIER);  // identifier rejected
          aClient.CloseDelayed;
        end
      else if GetClient (aClientID) <> nil then
        begin
          aClient.Parser.SendConnAck (rcIDENTIFIER);  // identifier rejected
          aClient.CloseDelayed;
        end
      else
        begin
      //    mon ('Client ID ' + ClientID + ' User '  + striUserName + ' Pass ' + PassWord);
          aClient.Parser.Username := aUserName;
          aClient.Parser.Password := aPassword;
          aClient.Parser.ClientID := aClientID;
          aClient.Parser.KeepAlive := aKeepAlive;
          aClient.Parser.Clean := aClean;
          mon ('Clean ' + ny[aClean]);
          if not aClean then
            begin
              if Assigned (FOnRestoreSession) then
                FOnRestoreSession (aClient, aClientID)
              else
                Sessions.RestoreSession (aClientID, aClient);
            end;
          if Assigned (FOnDeleteSession) then
            FOnDeleteSession (aClient, aClientID)
          else
            Sessions.DeleteSession (aClientID);
          aClient.Parser.SendConnAck (rcACCEPTED);
          aClient.FGraceful := false;
          mon ('Accepted. Is Broker ' + ny[aClient.FBroker]);
          if Assigned (FOnClientsChange) then
            FOnClientsChange (aServer, aServer.ClientCount);
        end;
    end
  else
    begin
      aClient.Parser.SendConnAck (rcUSER);
      aClient.CloseDelayed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.RxDisconnect (Sender: TObject);
var
  aClient : TSrvrClient;
begin
  if not (Sender is TIcsMQTTParser) then exit;
  aClient := GetClient (TIcsMQTTParser (Sender));
  if aClient = nil then exit;
  aClient.FGraceful := true;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTServer.RxHeader (Sender: TObject; MsgType: TMQTTMessageType; Dup: Boolean; Qos: TMQTTQOSType; Retain: Boolean);
begin
  if Assigned (FOnMonHdr) then
    FOnMonHdr (Self, MsgType, Dup, Qos, Retain);
end;


{ TIcsMQTTClient }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.Activate (Enable: Boolean);
begin
  if Enable = FEnable then exit;
  FEnable := Enable;
  try
    if (LinkSocket.State = wsConnected) then
      begin
        Parser.SendDisconnect;
        FGraceful := true;
      end;
    LinkSocket.CloseDelayed;
  except
    end;
  if Enable then
    SetTimer (Timers, 1, 100, nil)
  else
    begin
      KillTimer (Timers, 1);
      KillTimer (Timers, 2);
      KillTimer (Timers, 3);
    end;
  if Assigned (FOnEnableChange) then
    FOnEnableChange (Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsMQTTClient.Create (anOwner: TComponent);
begin
  inherited;
  FHost := '';
  FUsername := '';
  FPassword := '';
  FPort := 1883;
  FEnable := false;
  FGraceful := false;
  FOnline := false;
  FBroker := false;         // non standard
  FLocalBounce := false;
  FAutoSubscribe := false;
  FMessageID := 0;
  Subscriptions := TStringList.Create;
  Releasables := TMQTTMessageStore.Create;
  Parser := TIcsMQTTParser.Create;
  Parser.OnSend := DoSend;
  Parser.OnConnAck := RxConnAck;
  Parser.OnPublish := RxPublish;
  Parser.OnSubAck := RxSubAck;
  Parser.OnUnsubAck := RxUnsubAck;
  Parser.OnPubAck := RxPubAck;
  Parser.OnPubRel := RxPubRel;
  Parser.OnPubRec := RxPubRec;
  Parser.OnPubComp := RxPubComp;
  Parser.KeepAlive := 10;
  Timers := AllocateHWnd (TimerProc);
  InFlight := TMQTTPacketStore.Create;
  LinkSslContext := TSslContext.Create(Self);  { V8.71 }
  LinkSocket := TSslWSocket.Create (Self);
  LinkSocket.OnDataAvailable := LinkData;
  LinkSocket.OnSessionConnected := LinkConnected;
  LinkSocket.OnSessionClosed := LinkClosed;
  LinkSocket.SslContext := LinkSslContext;              { V8.71 }
  LinkSocket.OnSslHandshakeDone := SslHandshakeDone;    { V8.71 }
  FOcspHttp := TOcspHttp.Create(Self);                  { V8.71 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsMQTTClient.Destroy;
begin
  try
      Releasables.Clear;
      Releasables.Free;
      Subscriptions.Free;
      InFlight.Clear;
      InFlight.Free;
      KillTimer (Timers, 1);
      KillTimer (Timers, 2);
      KillTimer (Timers, 3);
      DeAllocateHWnd (Timers);
      try
        LinkSocket.Close;
      finally
        LinkSocket.Free;
      end;
      LinkSslContext.Free;
      FOcspHttp.Free;
      Parser.Free;
  finally
    inherited;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.RxConnAck (Sender: TObject; aCode: byte);
var
  i : integer;
  x : cardinal;
begin
  Mon ('Connection ' + MqttCodeNames(aCode));
  if aCode = rcACCEPTED then
    begin
      FOnline := true;
      FGraceful := false;
      SetTimer (Timers, 3, 100, nil);  // start retry counters
      if Assigned (FOnOnline) then
        FOnOnline (Self);
      if (FAutoSubscribe) and (Subscriptions.Count > 0) then
        begin
          for i := 0 to Subscriptions.Count - 1 do
            begin
              x := cardinal (Subscriptions.Objects[i]) and $03;
              Parser.SendSubscribe (NextMessageID, UTF8String (Subscriptions[i]), TMQTTQOSType (x));
            end;
        end;
    end
  else
    Activate (false); // not going to connect
end;


// publishing
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.RxPublish (Sender: TObject; anID: Word; aTopic : UTF8String; const aMessage : AnsiString);
var
  aMsg : TMQTTMessage;
begin
  case Parser.RxQos of
    qtAT_MOST_ONCE  :
      if Assigned (FOnMsg) then
        FOnMsg (Self, aTopic, aMessage, Parser.RxQos, Parser.RxRetain);
    qtAT_LEAST_ONCE :
      begin
        Parser.SendPubAck (anID);
        if Assigned (FOnMsg) then
            FOnMsg (Self, aTopic, aMessage, Parser.RxQos, Parser.RxRetain);
      end;
    qtEXACTLY_ONCE  :
      begin
        Parser.SendPubRec (anID);
        aMsg := Releasables.GetMsg (anID);
        if aMsg = nil then
          begin
            Releasables.AddMsg (anID, aTopic, aMessage, Parser.RxQos, 0, 0, Parser.RxRetain);
            mon ('Message ' + IntToStr (anID) + ' stored and idle.');
          end
        else
          mon ('Message ' + IntToStr (anID) + ' already stored.');
      end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.RxPubAck (Sender: TObject; anID: Word);
begin
  InFlight.DelPacket (anID);
  Mon ('ACK Message ' + IntToStr (anID) + ' disposed of.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.RxPubComp (Sender: TObject; anID: Word);
begin
  InFlight.DelPacket (anID);
  Mon ('COMP Message ' + IntToStr (anID) + ' disposed of.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.RxPubRec (Sender: TObject; anID: Word);
var
  aPacket : TMQTTPacket;
begin
  aPacket := InFlight.GetPacket (anID);
  if aPacket <> nil then
    begin
      aPacket.Counter := Parser.RetryTime;
      if aPacket.Publishing then
        begin
          aPacket.Publishing := false;
          Mon ('REC Message ' + IntToStr (anID) + ' recorded.');
        end
      else
        Mon ('REC Message ' + IntToStr (anID) + ' already recorded.');
    end
  else
    Mon ('REC Message ' + IntToStr (anID) + ' not found.');
  Parser.SendPubRel (anID);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.RxPubRel (Sender: TObject; anID: Word);
var
  aMsg : TMQTTMessage;
begin
  aMsg := Releasables.GetMsg (anID);
  if aMsg <> nil then
    begin
      Mon ('REL Message ' + IntToStr (anID) + ' publishing @ ' + QOSNames[aMsg.Qos]);
      if Assigned (FOnMsg) then
        FOnMsg (Self, aMsg.Topic, aMsg.Message, aMsg.Qos, aMsg.Retained);
      Releasables.Remove (aMsg);
      aMsg.Free;
      Mon ('REL Message ' + IntToStr (anID) + ' removed from storage.');
    end
  else
    Mon ('REL Message ' + IntToStr (anID) + ' has been already removed from storage.');
  Parser.SendPubComp (anID);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.SetClean (const Value: Boolean);
begin
  Parser.Clean := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.SetClientID (const Value: UTF8String);
begin
  Parser.ClientID := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.SetKeepAlive (const Value: Word);
begin
  Parser.KeepAlive := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.SetMaxRetries (const Value: integer);
begin
  Parser.MaxRetries := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.SetPassword (const Value: UTF8String);
begin
  Parser.Password := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.SetRetryTime (const Value: cardinal);
begin
  Parser.RetryTime := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.SetUsername (const Value: UTF8String);
begin
  Parser.UserName := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.SetWill (aTopic, aMessage : UTF8String; aQos: TMQTTQOSType;
  aRetain: Boolean);
begin
  Parser.SetWill (aTopic, aMessage, aQos, aRetain);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.Subscribe (Topics: TStringList);
var
  j : integer;
  i, x : cardinal;
  anID : Word;
  found : boolean;
begin
  if Topics = nil then exit;
  anID := NextMessageID;
  for i := 0 to Topics.Count - 1 do
    begin
      found := false;
      // 255 denotes acked
      if i > 254 then
        x := (cardinal (Topics.Objects[i]) and $03)
      else
        x := (cardinal (Topics.Objects[i]) and $03) + (anID shl 16) + (i shl 8) ;
      for j := 0 to Subscriptions.Count - 1 do
        if Subscriptions[j] = Topics[i] then
          begin
            found := true;
            Subscriptions.Objects[j] := TObject (x);
            break;
          end;
      if not found then
        Subscriptions.AddObject (Topics[i], TObject (x));
    end;
  Parser.SendSubscribe (anID, Topics);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.Subscribe (aTopic: UTF8String; aQos: TMQTTQOSType);
var
  i : integer;
  x : cardinal;
  found : boolean;
  anID : Word;
begin
  if aTopic = '' then exit;
  found := false;
  anID := NextMessageID;
  x := ord (aQos) + (anID shl 16);
  for i := 0 to Subscriptions.Count - 1 do
    if Subscriptions[i] = string (aTopic) then
      begin
        found := true;
        Subscriptions.Objects[i] := TObject (x);
        break;
      end;
  if not found then
    Subscriptions.AddObject (string (aTopic), TObject (x));
  Parser.SendSubscribe (anID, aTopic, aQos);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.DoSend (Sender: TObject; anID : Word; aRetry : integer; aStream: TMemoryStream);
var
  x : byte;
begin
  if LinkSocket.State = wsConnected then
    begin
      KillTimer (Timers, 2);       // 75% of keep alive
      if KeepAlive > 0 then SetTimer (Timers, 2, KeepAlive * 750, nil);
      aStream.Seek (0, soFromBeginning);
      aStream.Read (x, 1);
      if (TMQTTQOSType ((x and $06) shr 1) in [qtAT_LEAST_ONCE, qtEXACTLY_ONCE]) and
         (TMQTTMessageType ((x and $f0) shr 4) in [{mtPUBREL,} mtPUBLISH, mtSUBSCRIBE, mtUNSUBSCRIBE]) and
         (anID > 0) then
        begin
          InFlight.AddPacket (anID, aStream, aRetry, Parser.RetryTime);
          mon ('Message ' + IntToStr (anID) + ' created.');
        end;
      LinkSocket.Send (aStream.Memory, aStream.Size);
      Sleep (0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTClient.Enabled: boolean;
begin
  Result := FEnable;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTClient.GetClean: Boolean;
begin
  Result := Parser.Clean;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTClient.GetClientID: UTF8String;
begin
  Result := Parser.ClientID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTClient.GetKeepAlive: Word;
begin
  Result := Parser.KeepAlive;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTClient.GetMaxRetries: integer;
begin
  Result := Parser.MaxRetries;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTClient.GetPassword: UTF8String;
begin
  Result := Parser.Password;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTClient.GetRetryTime: cardinal;
begin
  Result := Parser.RetryTime;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTClient.GetUsername: UTF8String;
begin
  Result := Parser.UserName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.RxSubAck (Sender: TObject; anID: Word; Qoss : array of TMQTTQosType);
var
  j : integer;
  i, x : cardinal;
begin
  InFlight.DelPacket (anID);
  Mon ('Message ' + IntToStr (anID) + ' disposed of.');
  for i := low (Qoss) to high (Qoss) do
    begin
      if i > 254 then break;      // only valid for first 254
      for j := 0 to Subscriptions.Count - 1 do
        begin
          x := cardinal (Subscriptions.Objects[j]);
          if (hiword (x) = anID) and ((x and $0000ff00) shr 8 = i) then
            Subscriptions.Objects[j] :=  TObject ($ff00 + ord (Qoss[i]));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.RxUnsubAck (Sender: TObject; anID: Word);
begin
  InFlight.DelPacket (anID);
  Mon ('Message ' + IntToStr (anID) + ' disposed of.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.LinkConnected (Sender: TObject; ErrCode: Word);
var
  aClientID : UTF8String;

  function TimeString : UTF8string;
  begin
//  86400  secs
    Result := UTF8String (IntToHex (Trunc (Date), 5) + IntToHex (Trunc (Frac (Time) * 864000), 7));
  end;

begin
  if ErrCode = 0 then
    begin
      FGraceful := false;    // still haven't connected but expect to
      Parser.Reset;
   //   mon ('Time String : ' + Timestring);
   //=   mon ('xaddr ' + LinkSocket.GetXAddr);
      aClientID := ClientID;
      if aClientID = '' then
        aClientID := 'CID' + UTF8String (LinkSocket.GetXPort); // + TimeString;
      if Assigned (FOnClientID) then
        FOnClientID (Self, aClientID);
      ClientID := aClientID;
      if Parser.Clean then
        begin
          InFlight.Clear;
          Releasables.Clear;
        end;
      if FBroker then
        Parser.SendBrokerConnect (aClientID, Parser.UserName, Parser.Password, KeepAlive, Parser.Clean)
      else
        Parser.SendConnect (aClientID, Parser.UserName, Parser.Password, KeepAlive, Parser.Clean);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.LinkData (Sender: TObject; ErrCode: Word);
begin
  if ErrCode = 0 then
    Parser.Parse (LinkSocket.ReceiveStrA);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.Mon(const aStr: string);
begin
  if Assigned (FOnMon) then
    FOnMon (Self, aStr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTClient.NextMessageID: Word;
var
  i : integer;
  Unused : boolean;
  aMsg : TMQTTPacket;
begin
  repeat
    Unused := true;
    FMessageID := FMessageID + 1;
    if FMessageID = 0 then
        FMessageID := 1;   // exclude 0
    for i := 0 to InFlight.Count - 1 do
      begin
        aMsg := InFlight.List[i];
        if aMsg.ID = FMessageID then
          begin
            Unused := false;
            break;
          end;
      end;
  until Unused;
  Result := FMessageID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsMQTTClient.Online: boolean;
begin
  Result := FOnline;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.Ping;
begin
  Parser.SendPing;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.Publish (aTopic : UTF8String; const aMessage : AnsiString; aQos : TMQTTQOSType; aRetain : Boolean);
var
  i : integer;
  found : boolean;
begin
  if FLocalBounce and Assigned (FOnMsg) then
    begin
      found := false;
      for i := 0 to Subscriptions.Count - 1 do
        if IsSubscribed (UTF8String (Subscriptions[i]), aTopic) then
          begin
            found := true;
            break;
          end;
      if found then
        begin
          Parser.RxQos := aQos;
          FOnMsg (Self, aTopic, aMessage, aQos, false);
        end;
    end;
  Parser.SendPublish (NextMessageID, aTopic, aMessage, aQos, false, aRetain);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.TimerProc (var aMsg: TMessage);
var
  i : integer;
  bPacket : TMQTTPacket;
  WillClose : Boolean;
begin
  if aMsg.Msg = WM_TIMER then
    begin
      KillTimer (Timers, aMsg.WParam);
      case aMsg.WParam of
        1 : begin
              Mon ('Connecting to ' + FHost + ' on Port ' + IntToStr (FPort));
              LinkSocket.Addr := FHost;
              LinkSocket.Port := IntToStr (FPort);
              LinkSocket.Proto := 'tcp';
              if Port = MQTT_PortSsl then      { V8.71 SSL stuff }
              begin
                LinkSocket.SslEnable := True;
                LinkSocket.SslMode := sslModeClient ;
                LinkSocket.SslServerName := FHost;
                LinkSslContext.SslCliSecurity := FSslCliSecurity;
                LinkSslContext.SslVerifyPeer := FSslVerifyCerts;
                LinkSslContext.SslOcspStatus := FSslRevocation;
                LinkSslContext.SslVerifyPeerModes := [SslVerifyMode_PEER] ;
                if (FRootCA = '') or (NOT FileExists(FRootCA)) then
                    LinkSslContext.SslCALines.Text := sslRootCACertsBundle
                else
                   LinkSslContext.SslCAFile := FRootCA;
              end;

              try
                LinkSocket.Connect;
              except
                on e: Exception do
                  Mon('Connect Error ' + e.Message);
              end;
            end;
        2 : Ping;
        3 : begin         // send duplicates
              for i := InFlight.Count - 1 downto 0 do
                begin
                  bPacket := InFlight.List[i];
                  if bPacket.Counter > 0 then
                    begin
                      bPacket.Counter := bPacket.Counter - 1;
                      if bPacket.Counter = 0 then
                        begin
                          bPacket.Retries := bPacket.Retries + 1;
                          if bPacket.Retries <=  MaxRetries then
                            begin
                              if bPacket.Publishing then
                                begin
                                  InFlight.List.Remove (bPacket);
                                  mon ('Message ' + IntToStr (bPacket.ID) + ' disposed of..');
                                  mon ('Re-issuing Message ' + inttostr (bPacket.ID) + ' Retry ' + inttostr (bPacket.Retries));
                                  SetDup (bPacket.Msg, true);
                                  DoSend (Parser, bPacket.ID, bPacket.Retries, bPacket.Msg);
                                  bPacket.Free;
                                end
                              else
                                begin
                                  mon ('Re-issuing PUBREL Message ' + inttostr (bPacket.ID) + ' Retry ' + inttostr (bPacket.Retries));
                                  Parser.SendPubRel (bPacket.ID, true);
                                  bPacket.Counter := Parser.RetryTime;
                                end;
                            end
                          else
                            begin
                              WillClose := true;
                              if Assigned (FOnFailure) then
                                FOnFailure (Self, frMAXRETRIES, WillClose);
                              if WillClose then
                                LinkSocket.CloseDelayed;
                            end;
                        end;
                    end;
                end;
              SetTimer (Timers, 3, 100, nil);
            end;
      end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.Unsubscribe (Topics: TStringList);
var
  i, J : integer;
begin
  if Topics = nil then exit;
  for i := 0 to Topics.Count - 1 do
    begin
      for j := Subscriptions.Count - 1 downto 0 do
        if Subscriptions[j] = Topics[i] then
          begin
            Subscriptions.Delete (j);
            break;
          end;
    end;
  Parser.SendUnsubscribe (NextMessageID, Topics);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.Unsubscribe (aTopic: UTF8String);
var
  i : integer;
begin
  if aTopic = '' then exit;
  for i := Subscriptions.Count - 1 downto 0 do
    if Subscriptions[i] = string (aTopic) then
      begin
        Subscriptions.Delete (i);
        break;
      end;
  Parser.SendUnsubscribe (NextMessageID, aTopic);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsMQTTClient.LinkClosed (Sender: TObject; ErrCode: Word);
begin
//  Mon ('Link Closed...');
  KillTimer (Timers, 2);
  KillTimer (Timers, 3);
  if Assigned (FOnOffline) and (FOnline) then
    FOnOffline (Self, FGraceful);
  FOnline := false;
  if FEnable then
    SetTimer (Timers, 1, 6000, nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// SSL Handshaking done, if OK may check certificate chain
procedure TIcsMQTTClient.SslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
var
    CertChain: TX509List;
    Hash, info, VerifyInfo: String;
    Safe: Boolean;
begin
    with Sender as TSslWSocket do
    begin
        if (ErrCode <> 0) or Disconnect then
        begin
            Mon ('SSL Handshake Failed - ' + SslHandshakeRespMsg);
            CloseDelayed;
            Exit;
        end;
        Mon ('SSL Handshake OK - ' + SslHandshakeRespMsg);
        if SslSessionReused OR (NOT SslVerifyCerts) then
        begin
            Exit; // nothing to do, go ahead
        end ;

     // Is current host already in the list of temporarily accepted hosts ?
        Hash := PeerCert.Sha1Hex ;
        if SslAcceptableHosts.IndexOf (SslServerName + Hash ) > -1 then
        begin
            Exit; // nothing to do, go ahead
        end ;

     // TSslWSocket property SslCertChain contains all certificates in current verify chain
        CertChain := SslCertChain;
        VerifyInfo := PeerCert.FirstVerifyErrMsg;
        Safe := (PeerCert.VerifyResult = X509_V_OK);   { check whether SSL chain verify result was OK }

      { check OCSP to see if revoked, if we got a chain of certificates }
      { note this is a soft check, if we don't have a stapled OCSP response from the TLS handshake, we get it from an
        OCSP HTTP server and cache it but don't wait for the response. So next attempt comes from cache.  }
        if (Safe and FSslRevocation and PeerCert.IsCertLoaded and (CertChain.Count > 0)) then
        begin
            FOcspHttp.ClearOcsp;
            FOcspHttp.OcspCert := PeerCert;
            FOcspHttp.OcspInters := CertChain;
            if (Length(OcspStapleRaw) > 50) and (OcspStapleStatus = OCSP_RESPONSE_STATUS_SUCCESSFUL) then
                FOcspHttp.OcspRespRaw := OcspStapleRaw;
            if FOcspHttp.CheckOcspRevoked(LinkSslContext.GetX509Store, 0) then
                Safe := False;
            VerifyInfo := FOcspHttp.OcspLastResp;
            FOcspHttp.OcspInters := Nil;
            Mon (VerifyInfo);
        end;

    // allow self signed certs
        if (CertChain.Count > 0) and (CertChain[0].FirstVerifyResult = X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN) then
        begin
            Safe := true ;
            Mon ('SSL Self Signed Certificate Succeeded: ' + IcsUnwrapNames (PeerCert.IssuerCName));
        end;

     // tell user verification failed
        if NOT Safe then
        begin
            info := 'SSL Chain Verification Failed: ' + VerifyInfo + ', Domain: ';
            if PeerCert.SubAltNameDNS = '' then
                info := info + IcsUnwrapNames (PeerCert.SubjectCName)
            else
                info := info + IcsUnwrapNames (PeerCert.SubAltNameDNS) ;
            info := info + ', Expected: ' + SslServerName ;
            Mon (info);
        end

     // check certificate was issued to remote host for out connection
        else
        begin
            Mon ('SSL Chain Verification Succeeded, Domain: ' + SslCertPeerName);
        end;

   // if certificate checking failed, see if the host is specifically listed as being allowed anyway
        if (NOT Safe) and (SslAcceptableHosts.IndexOf (SslServerName) > -1) then
        begin
            SslAcceptableHosts.Add (SslServerName + Hash);  // keep it to avoid checking again
            Mon ('SSL Succeeded with Acceptable Host Name');
        end ;

      // tell user about all the certificates we found
        if FSslReportChain and (CertChain.Count > 0) then
        begin
            info := 'Verify Result: ' + VerifyInfo + IcsCRLF ;
            info := info + IntToStr (CertChain.Count) + ' SSL Certificates in the verify chain:'+ IcsCRLF + CertChain.AllCertInfo (true, true) ;
            Mon (info);
        end;
    end;
end;


{ TMQTTPacketStore }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMQTTPacketStore.AddPacket (anID : Word; aMsg : TMemoryStream; aRetry : cardinal; aCount : cardinal) : TMQTTPacket;
begin
  Result := TMQTTPacket.Create;
  Result.ID := anID;
  Result.Counter := aCount;
  Result.Retries := aRetry;
  aMsg.Seek (0, soFromBeginning);
  Result.Msg.CopyFrom (aMsg, aMsg.Size);
  List.Add (Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTPacketStore.Assign (From : TMQTTPacketStore);
var
  i : integer;
  aPacket, bPacket : TMQTTPacket;
begin
  Clear;
  for i := 0 to From.Count - 1 do
    begin
      aPacket := From[i];
      bPacket := TMQTTPacket.Create;
      bPacket.Assign (aPacket);
      List.Add (bPacket);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTPacketStore.Clear;
var
  i : integer;
begin
  for i := 0 to List.Count - 1 do
    TMQTTPacket (List[i]).Free;
  List.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMQTTPacketStore.Count: integer;
begin
  Result := List.Count;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMQTTPacketStore.Create;
begin
  Stamp := Now;
  List := TList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTPacketStore.DelPacket (anID: Word);
var
  i : integer;
  aPacket : TMQTTPacket;
begin
  for i := List.Count - 1 downto 0 do
    begin
      aPacket := List[i];
      if aPacket.ID = anID then
        begin
          List.Remove (aPacket);
          aPacket.Free;
          exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TMQTTPacketStore.Destroy;
begin
  Clear;
  List.Free;
  inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMQTTPacketStore.GetItem (Index: Integer): TMQTTPacket;
begin
  if (Index >= 0) and (Index < Count) then
    Result := List[Index]
  else
    Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMQTTPacketStore.GetPacket (anID: Word): TMQTTPacket;
var
  i : integer;
begin
  for i := 0 to List.Count - 1 do
    begin
      Result := List[i];
      if Result.ID = anID then exit;
    end;
  Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTPacketStore.Remove (aPacket : TMQTTPacket);
begin
  List.Remove (aPacket);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTPacketStore.SetItem (Index: Integer; const Value: TMQTTPacket);
begin
  if (Index >= 0) and (Index < Count) then
    List[Index] := Value;
end;


{ TMQTTPacket }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTPacket.Assign (From: TMQTTPacket);
begin
  ID := From.ID;
  Stamp := From.Stamp;
  Counter := From.Counter;
  Retries := From.Retries;
  Msg.Clear;
  From.Msg.Seek (0, soFromBeginning);
  Msg.CopyFrom (From.Msg, From.Msg.Size);
  Publishing := From.Publishing;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMQTTPacket.Create;
begin
  ID := 0;
  Stamp := Now;
  Publishing := true;
  Counter := 0;
  Retries := 0;
  Msg := TMemoryStream.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TMQTTPacket.Destroy;
begin
  Msg.Free;
  inherited;
end;


{ TMQTTMessage }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTMessage.Assign (From: TMQTTMessage);
begin
  ID := From.ID;
  Stamp := From.Stamp;
  LastUsed := From.LastUsed;
  Retained := From.Retained;
  Counter := From.Counter;
  Retries := From.Retries;
  Topic := From.Topic;
  Message := From.Message;
  Qos := From.Qos;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMQTTMessage.Create;
begin
  ID := 0;
  Stamp := Now;
  LastUsed := Stamp;
  Retained := false;
  Counter := 0;
  Retries := 0;
  Qos := qtAT_MOST_ONCE;
  Topic := '';
  Message := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TMQTTMessage.Destroy;
begin
  inherited;
end;


{ TMQTTMessageStore }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMQTTMessageStore.AddMsg (anID: Word; aTopic : UTF8String; aMessage : AnsiString; aQos : TMQTTQOSType;
                                                                 aRetry, aCount: cardinal; aRetained : Boolean) : TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.ID := anID;
  Result.Topic := aTopic;
  Result.Message := aMessage;
  Result.Qos := aQos;
  Result.Counter := aCount;
  Result.Retries := aRetry;
  Result.Retained := aRetained;
  List.Add (Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTMessageStore.Assign (From: TMQTTMessageStore);
var
  i : integer;
  aMsg, bMsg : TMQTTMessage;
begin
  Clear;
  for i := 0 to From.Count - 1 do
    begin
      aMsg := From[i];
      bMsg := TMQTTMessage.Create;
      bMsg.Assign (aMsg);
      List.Add (bMsg);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTMessageStore.Clear;
var
  i : integer;
begin
  for i := 0 to List.Count - 1 do
    TMQTTMessage (List[i]).Free;
  List.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMQTTMessageStore.Count: integer;
begin
  Result := List.Count;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMQTTMessageStore.Create;
begin
  Stamp := Now;
  List := TList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTMessageStore.DelMsg (anID: Word);
var
  i : integer;
  aMsg : TMQTTMessage;
begin
  for i := List.Count - 1 downto 0 do
    begin
      aMsg := List[i];
      if aMsg.ID = anID then
        begin
          List.Remove (aMsg);
          aMsg.Free;
          exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TMQTTMessageStore.Destroy;
begin
  Clear;
  List.Free;
  inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMQTTMessageStore.GetItem (Index: Integer): TMQTTMessage;
begin
  if (Index >= 0) and (Index < Count) then
    Result := List[Index]
  else
    Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMQTTMessageStore.GetMsg (anID: Word): TMQTTMessage;
var
  i : integer;
begin
  for i := 0 to List.Count - 1 do
    begin
      Result := List[i];
      if Result.ID = anID then exit;
    end;
  Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTMessageStore.Remove (aMsg: TMQTTMessage);
begin
  List.Remove (aMsg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTMessageStore.SetItem (Index: Integer; const Value: TMQTTMessage);
begin
  if (Index >= 0) and (Index < Count) then
    List[Index] := Value;
end;


{ TMQTTSession }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMQTTSession.Create;
begin
  ClientID := '';
  Stamp := Now;
  InFlight := TMQTTPacketStore.Create;
  Releasables := TMQTTMessageStore.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TMQTTSession.Destroy;
begin
  InFlight.Clear;
  InFlight.Free;
  Releasables.Clear;
  Releasables.Free;
  inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TMQTTSessionStore }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTSessionStore.Clear;
var
  i : integer;
begin
  for i := 0 to List.Count - 1 do
    TMQTTSession (List[i]).Free;
  List.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMQTTSessionStore.Count: integer;
begin
  Result := List.Count;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMQTTSessionStore.Create;
begin
  Stamp := Now;
  List := TList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTSessionStore.DeleteSession (ClientID: UTF8String);
var
  aSession : TMQTTSession;
begin
  aSession := GetSession (ClientID);
  if aSession <> nil then
    begin
      List.Remove (aSession);
      aSession.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TMQTTSessionStore.Destroy;
begin
  Clear;
  List.Free;
  inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMQTTSessionStore.GetItem (Index: Integer): TMQTTSession;
begin
  if (Index >= 0) and (Index < Count) then
    Result := List[Index]
  else
    Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMQTTSessionStore.GetSession (ClientID: UTF8String): TMQTTSession;
var
  i : integer;
begin
  for i := 0 to List.Count - 1 do
    begin
      Result := List[i];
      if Result.ClientID = ClientID then exit;
    end;
  Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTSessionStore.RestoreSession (ClientID: UTF8String;
  aClient: TSrvrClient);
var
  aSession : TMQTTSession;
begin
  aClient.InFlight.Clear;
  aClient.Releasables.Clear;
  aSession := GetSession (ClientID);
  if aSession <> nil then
    begin
      aClient.InFlight.Assign (aSession.InFlight);
      aClient.Releasables.Assign (aSession.Releasables);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTSessionStore.RestoreSession (ClientID: UTF8String; aClient: TIcsMQTTClient);
var
  aSession : TMQTTSession;
begin
  aClient.InFlight.Clear;
  aClient.Releasables.Clear;
  aSession := GetSession (ClientID);
  if aSession <> nil then
    begin
      aClient.InFlight.Assign (aSession.InFlight);
      aClient.Releasables.Assign (aSession.Releasables);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTSessionStore.StoreSession (ClientID: UTF8String; aClient: TIcsMQTTClient);
var
  aSession : TMQTTSession;
begin
  aSession := GetSession (ClientID);
  if aSession <> nil then
    begin
      aSession := TMQTTSession.Create;
      aSession.ClientID := ClientID;
      List.Add (aSession);
    end;

  aSession.InFlight.Assign (aClient.InFlight);
  aSession.Releasables.Assign (aClient.Releasables);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTSessionStore.StoreSession (ClientID: UTF8String; aClient: TSrvrClient);
var
  aSession : TMQTTSession;
begin
  aClient.InFlight.Clear;
  aClient.Releasables.Clear;
  aSession := GetSession (ClientID);
  if aSession <> nil then
    begin
      aSession := TMQTTSession.Create;
      aSession.ClientID := ClientID;
      List.Add (aSession);
    end;
  aSession.InFlight.Assign (aClient.InFlight);
  aSession.Releasables.Assign (aClient.Releasables);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMQTTSessionStore.SetItem (Index: Integer; const Value: TMQTTSession);
begin
  if (Index >= 0) and (Index < Count) then
    List[Index] := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$ENDIF USE_SSL}

end.
