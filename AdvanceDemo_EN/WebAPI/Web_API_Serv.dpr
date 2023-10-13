program Web_API_Serv;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  {  Basic Library  }
  SysUtils, Classes, DateUtils, IOUtils, TypInfo,
  {  Using ICS for web servers  }
  Z.OverbyteIcsDigestAuth, Z.OverbyteIcsWSocket, Z.OverbyteIcsHttpSrv, Z.OverbyteIcsMimeUtils, Z.OverbyteIcsFormDataDecoder,
  {  Z series various libraries  }
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status, Z.HashList.Templet, Z.Expression, Z.OpCode, Z.Parsing,
  Z.MemoryStream, Z.ListEngine, Z.Json, Z.Notify, Z.DFE, Z.Net, Z.Net.PhysicsIO, Z.Net.C4, Z.Net.C4_Console_APP,
  C4_Demo_Service in 'C4_Demo_Service.pas'; {  Demo service  }

{  The web API server is detached from existing projects, with a complexity index equivalent to operational projects, but it is a demo  }
{  You can directly use these codes in the project  }
{  Usage, browser access, http://127.0.0.1:8888/api/GetDemoInfo?FormatJson=True  }
{  Omit the call method of delphi/fpc and search for the relevant demo of http get  }
{  The get form web API can be connected to any language, including c++/c #/java/pas/swift/oc/js/ts/py  }

const
  Content_Json = 'application/json;charset=utf-8'; {  For browser support  }
  NO_CACHE = 'Pragma: no-cache' + #13#10 + 'Expires: -1' + #13#10; {  For browser support  }

type
  {  Web-api operation idea: Firstly, the server of c4 is a deployable mechanism, and it will have multiple server resources at any time  }
  {  Upon web request, the c4 network will traverse all currently valid c4 server resources and execute remote requests  }
  {  Web-api supports polling from multiple c4 servers  }
  {  Web-api operation idea compatible with mpp large-scale database model  }
  {  The underlying mechanism of c4 will be disconnected and reconnected. Once deployed in the data center, unmanned maintenance is sufficient  }

  TArry_Demo_Client = array of TC40_Demo_Client;

  TWebAPI_Http_Client_Bridge = class;

  TWebAPI_Http_Client_Connection = class(THttpConnection)
  private
    Bridge_: TWebAPI_Http_Client_Bridge;
    Last_Flags: THttpGetFlag;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {  Get from multiple servers_Demo_Info framework  }
  {  Each request is sent to the entire c4 network, and after all responses are received, HTTP feedback is triggered. After that, the process is completed  }
  TWebAPI_Http_Client_Bridge_Get_Demo_Info = class
  public
    Bridge: TWebAPI_Http_Client_Bridge;
    Demo_Info_Final_Buff: TPascalStringList;
    task_num: Integer;
    FormatJson: Boolean;
    procedure Do_All_Done;
    procedure Do_Get_Demo_Info_Result(Sender: TPeerIO; Result_: TDFE);
    constructor Create(Bridge_: TWebAPI_Http_Client_Bridge);
    destructor Destroy; override;
  end;

  {  Command bridge framework to avoid HttpServer_The GetDocument heap is too complex, otherwise it is anti human and difficult to stack code  }
  TWebAPI_Http_Client_Bridge = class
  private
    task_num: Integer;
    connection: TWebAPI_Http_Client_Connection;
  public
    constructor Create(connection_: TWebAPI_Http_Client_Connection);
    destructor Destroy; override;
    procedure Get_Demo_Info(FormatJson: Boolean);
  end;

  {  Web API service standard, using ICS for HTTP listening, SSL is not connected here  }
  TWeb_Service = class
  private
    procedure HttpServer_Connect(Sender: TObject; Client: TObject; Error: Word);
    procedure HttpServer_GetDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
  public
    mime: TMimeTypesList; // http mime
    serv: THttpServer; // web service instance
    constructor Create(ssl: Boolean);
    destructor Destroy; override;
    procedure Read_Param(param: THashStringList);
  end;

  // *********************************************************************************************************
var
  host, port: U_String; // c4 host
  web: TWeb_Service; // web service instance
  exit_signal: Boolean; // c4 exit signal

function Get_Demo_Client: TArry_Demo_Client;
var
  arry: TC40_Custom_Client_Array;
  i: Integer;
begin
  arry := C40_ClientPool.SearchClass(TC40_Demo_Client, True);
  SetLength(Result, length(arry));
  for i := 0 to length(arry) - 1 do
      Result[i] := arry[i] as TC40_Demo_Client;
end;

procedure Build_C4_Net(host, port: U_String);
var
  L: THashStringList;
  arry: TArrayBatch;
begin
  L := THashStringList.Create;
  L['<host>'] := host;
  L['<port>'] := port;
  arry := umlBuildBatch(L);
  DisposeObject(L);
  Z.Net.C4_Console_APP.C40_Extract_CmdLine(tsC, [umlBatchReplace('KeepAlive("<host>",<port>,"demo", True)', arry, False, True, 0, 0)]);
end;

constructor TWebAPI_Http_Client_Connection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Bridge_ := nil;
end;

destructor TWebAPI_Http_Client_Connection.Destroy;
begin
  if Bridge_ <> nil then
    begin
      Bridge_.connection := nil;
      if Bridge_.task_num <= 0 then
          DisposeObject(Bridge_);
    end;
  inherited Destroy;
end;

procedure TWebAPI_Http_Client_Bridge_Get_Demo_Info.Do_All_Done;
var
  js: TZJ;
  ja: TZJArry;
  i: Integer;
begin
  js := TZJ.Create;
  ja := js.A['demo_info'];
  for i := 0 to Demo_Info_Final_Buff.Count - 1 do
      ja.Add(Demo_Info_Final_Buff[i]);

  dec(Bridge.task_num);
  if Bridge.connection <> nil then
    begin
      Bridge.connection.Last_Flags := hgWillSendMySelf;
      Bridge.connection.DocStream.Free;
      Bridge.connection.DocStream := TMS64.Create;
      js.SaveToStream(Bridge.connection.DocStream, FormatJson);
      Bridge.connection.AnswerStream(
        Bridge.connection.Last_Flags,
        { Default Status '200 OK' }
        '',
        { Content-Type }
        Content_Json,
        { header }
        NO_CACHE);
    end;
  DisposeObject(js);

  if (Bridge.connection = nil) and (Bridge.task_num <= 0) then
      DelayFreeObj(1.0, self, Bridge)
  else
      DelayFreeObj(1.0, self);
end;

procedure TWebAPI_Http_Client_Bridge_Get_Demo_Info.Do_Get_Demo_Info_Result(Sender: TPeerIO; Result_: TDFE);
begin
  if Result_.Count > 0 then
      Demo_Info_Final_Buff.Add(Result_.R.ReadString);

  dec(task_num);
  if task_num <= 0 then
      SysPost.PostExecuteM_NP(0, Do_All_Done);
end;

constructor TWebAPI_Http_Client_Bridge_Get_Demo_Info.Create(Bridge_: TWebAPI_Http_Client_Bridge);
begin
  inherited Create;
  Bridge := Bridge_;
  Demo_Info_Final_Buff := TPascalStringList.Create;
  task_num := 0;
  FormatJson := False;
end;

destructor TWebAPI_Http_Client_Bridge_Get_Demo_Info.Destroy;
begin
  DisposeObject(Demo_Info_Final_Buff);
  inherited Destroy;
end;

constructor TWebAPI_Http_Client_Bridge.Create(connection_: TWebAPI_Http_Client_Connection);
begin
  inherited Create;
  task_num := 0;
  connection := connection_;
  connection.Bridge_ := self;
end;

destructor TWebAPI_Http_Client_Bridge.Destroy;
begin
  inherited Destroy;
end;

procedure TWebAPI_Http_Client_Bridge.Get_Demo_Info(FormatJson: Boolean);
var
  tmp: TWebAPI_Http_Client_Bridge_Get_Demo_Info;
  arry: TArry_Demo_Client;
  rv_client: TC40_Demo_Client;
begin
  Inc(task_num);

  tmp := TWebAPI_Http_Client_Bridge_Get_Demo_Info.Create(self);
  tmp.FormatJson := FormatJson;

  arry := Get_Demo_Client;
  for rv_client in arry do
    begin
      Inc(tmp.task_num);
      rv_client.Get_Demo_Info_M(tmp.Do_Get_Demo_Info_Result);
    end;

  {  If all c4 servers are disconnected, trigger the return directly  }
  if tmp.task_num <= 0 then
      SysPost.PostExecuteM_NP(0.5, tmp.Do_All_Done);
end;

procedure TWeb_Service.HttpServer_Connect(Sender, Client: TObject; Error: Word);
var
  http_cli: TWebAPI_Http_Client_Connection;
begin
  http_cli := Client as TWebAPI_Http_Client_Connection;
  http_cli.Bridge_ := TWebAPI_Http_Client_Bridge.Create(http_cli);
end;

procedure TWeb_Service.HttpServer_GetDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
var
  http_cli: TWebAPI_Http_Client_Connection;
  path: U_String;
  param: U_String;
  L: THashStringList;
begin
  http_cli := Client as TWebAPI_Http_Client_Connection;
  http_cli.Last_Flags := Flags;

  path := umlTrimSpace(http_cli.path);
  param := umlTrimSpace(http_cli.Params);
  L := THashStringList.Create;
  L.AsText := umlReplace(umlURLDecode(param, True), '&', #13#10, False, False);
  if isDebug then
      DoStatus(L.AsText);

  if umlMultipleMatch(['/api/GetDemoInfo', '/api/DemoInfo', '/api/GetDemo', '/api/Get_Demo_Info'], path) then
    begin
      // http://127.0.0.1:8888/api/GetDemoInfo?FormatJson=True
      http_cli.Bridge_.Get_Demo_Info(EStrToBool(L.GetDefaultValue('FormatJson', 'False'), False));
      Flags := hgWillSendMySelf;
    end;

  DisposeObject(L);
end;

constructor TWeb_Service.Create(ssl: Boolean);
begin
  inherited Create;
  mime := TMimeTypesList.Create(nil);
  if ssl then
      serv := TSslHttpServer.Create(nil)
  else
      serv := THttpServer.Create(nil);
  serv.port := '8888';
  serv.MimeTypesList := mime;
  serv.ClientClass := TWebAPI_Http_Client_Connection;
  serv.OnClientConnect := HttpServer_Connect;
  serv.OnGetDocument := HttpServer_GetDocument;
end;

destructor TWeb_Service.Destroy;
begin
  DisposeObject(serv);
  DisposeObject(mime);
  inherited Destroy;
end;

procedure TWeb_Service.Read_Param(param: THashStringList);
var
  n: U_String;
  auth_type: TAuthenticationType;
  auth_types: TAuthenticationTypes;
  Http_Option: THttpOption;
  Http_Options: THttpOptions;
  value_: Integer;
begin
  serv.Addr := param.GetDefaultValue('Addr', serv.Addr);
  serv.port := param.GetDefaultValue('Port', serv.port);
  serv.ServerHeader := param.GetDefaultValue('ServerHeader', 'Server: Z-ZNet HttpServer');
  serv.SizeCompressMax := EStrToInt(param.GetDefaultValue('SizeCompressMax', umlIntToStr(serv.SizeCompressMax)));
  serv.SizeCompressMin := EStrToInt(param.GetDefaultValue('SizeCompressMin', umlIntToStr(serv.SizeCompressMin)));
  serv.DocDir := param.GetDefaultValue('DocDir', umlCurrentPath);
  serv.DefaultDoc := param.GetDefaultValue('DefaultDoc', serv.DefaultDoc);
  serv.TemplateDir := param.GetDefaultValue('TemplateDir', umlCurrentPath);
  serv.WellKnownPath := param.GetDefaultValue('WellKnownPath', umlCurrentPath);
  serv.BandwidthLimit := EStrToInt(param.GetDefaultValue('BandwidthLimit', umlIntToStr(serv.BandwidthLimit)));
  serv.BandwidthSampling := EStrToInt(param.GetDefaultValue('BandwidthSampling', umlIntToStr(serv.BandwidthSampling)));
  serv.ExclusiveAddr := EStrToBool(param.GetDefaultValue('ExclusiveAddr', umlBoolToStr(serv.ExclusiveAddr)));
  serv.KeepAliveTimeSec := EStrToInt(param.GetDefaultValue('KeepAliveTimeSec', umlIntToStr(serv.KeepAliveTimeSec)));
  serv.KeepAliveTimeXferSec := EStrToInt(param.GetDefaultValue('KeepAliveTimeXferSec', umlIntToStr(serv.KeepAliveTimeXferSec)));
  serv.MaxBlkSize := EStrToInt(param.GetDefaultValue('MaxBlkSize', umlIntToStr(serv.MaxBlkSize)));
  serv.MaxClients := EStrToInt(param.GetDefaultValue('MaxClients', umlIntToStr(serv.MaxClients)));
  serv.MaxRequestsKeepAlive := EStrToInt(param.GetDefaultValue('MaxRequestsKeepAlive', umlIntToStr(serv.MaxRequestsKeepAlive)));
  serv.LingerOnOff := TSocketLingerOnOff(GetEnumValue(TypeInfo(TSocketLingerOnOff),
      param.GetDefaultValue('LingerOnOff', GetEnumName(TypeInfo(TSocketLingerOnOff), Ord(serv.LingerOnOff)))));
  serv.LingerTimeout := EStrToInt(param.GetDefaultValue('LingerTimeout', umlIntToStr(serv.LingerTimeout)));
  serv.ListenBacklog := EStrToInt(param.GetDefaultValue('ListenBacklog', umlIntToStr(serv.ListenBacklog)));

  serv.SocketErrs := TSocketErrs(GetEnumValue(TypeInfo(TSocketErrs),
      param.GetDefaultValue('SocketErrs', GetEnumName(TypeInfo(TSocketErrs), Ord(serv.SocketErrs)))));
  serv.SocketFamily := TSocketFamily(GetEnumValue(TypeInfo(TSocketFamily),
      param.GetDefaultValue('SocketFamily', GetEnumName(TypeInfo(TSocketFamily), Ord(serv.SocketFamily)))));

  serv.AuthDigestMethod := TAuthDigestMethod(GetEnumValue(TypeInfo(TAuthDigestMethod),
      param.GetDefaultValue('AuthDigestMethod', GetEnumName(TypeInfo(TAuthDigestMethod), Ord(serv.AuthDigestMethod)))));
  serv.AuthDigestNonceLifeTimeMin := EStrToInt(param.GetDefaultValue('AuthDigestNonceLifeTimeMin', umlIntToStr(serv.AuthDigestNonceLifeTimeMin)));
  serv.AuthRealm := param.GetDefaultValue('AuthRealm', serv.AuthRealm);

  auth_types := serv.AuthTypes;

  n := '';
  for auth_type in auth_types do
    begin
      if n.L > 0 then
          n.Append(',');
      n.Append(GetEnumName(TypeInfo(TAuthenticationType), Ord(auth_type)));
    end;

  n := param.GetDefaultValue('AuthTypes', n);
  while n.L > 0 do
    begin
      value_ := GetEnumValue(TypeInfo(TAuthenticationType), umlTrimSpace(umlGetFirstStr(n, ',')));
      auth_types := auth_types + [TAuthenticationType(value_)];
      n := umlDeleteFirstStr(n, ',');
    end;
  serv.AuthTypes := auth_types;

  Http_Options := serv.Options;
  n := '';
  for Http_Option in Http_Options do
    begin
      if n.L > 0 then
          n.Append(',');
      n.Append(GetEnumName(TypeInfo(THttpOption), Ord(Http_Option)));
    end;
  n := param.GetDefaultValue('Options', n);
  while n.L > 0 do
    begin
      value_ := GetEnumValue(TypeInfo(THttpOption), umlTrimSpace(umlGetFirstStr(n, ',')));
      Http_Options := Http_Options + [THttpOption(value_)];
      n := umlDeleteFirstStr(n, ',');
    end;
  serv.Options := Http_Options;
end;

function Init_Http_Serv(param: THashStringList): Boolean;
begin
  web := TWeb_Service.Create(EStrToBool(param.GetDefaultValue('SSL', 'False'), False));
  web.Read_Param(param);
  DoStatus('');
  DoStatus('http-server parameter.');
  DoStatus(umlReplace('-W' + param.AsText, #10, #10'-W', False, False));
  DoStatus('');
  web.serv.Start;
  Result := web.serv.ListenAllOK;
  if Result then
      DoStatus('http-server listening addr:%s port:%s successed', [web.serv.Addr, web.serv.port])
  else
      DoStatus('http-server listening addr:%s port:%s failed!', [web.serv.Addr, web.serv.port]);
end;

procedure Print_Help;
begin
  DoStatus('Web API help.', []);
  DoStatus('');
  DoStatus('base paramemter');
  DoStatus('-help, print this help info.', []);
  DoStatus('-c4:ip or host name', []);
  DoStatus('');
  DoStatus('http service paramemter');
  DoStatus('-WAddr=0.0.0.0, http service listening ip');
  DoStatus('-WPort=8888, http service listening port');
  DoStatus('-WSSL=False, open or close SSL for HTTP Service');
  DoStatus('-WServerHeader=Server: Z-ZNet HttpServer, http header info');
  DoStatus('-WSizeCompressMax=5000000, max compress');
  DoStatus('-WSizeCompressMin=5000, min compress');
  DoStatus('-WDocDir=%s, default document directory', [umlCurrentPath.Text]);
  DoStatus('-WDefaultDoc=index.html, default document file');
  DoStatus('-WTemplateDir=%s, default templet directory', [umlCurrentPath.Text]);
  DoStatus('-WWellKnownPath=%s', [umlCurrentPath.Text]);
  DoStatus('-WBandwidthLimit=0, bandwidth limit,0=infinite');
  DoStatus('-WBandwidthSampling=1000');
  DoStatus('-WExclusiveAddr=True');
  DoStatus('-WKeepAliveTimeSec=10');
  DoStatus('-WKeepAliveTimeXferSec=300');
  DoStatus('-WMaxBlkSize=8192');
  DoStatus('-WMaxClients=0');
  DoStatus('-WMaxRequestsKeepAlive=500');
  DoStatus('-WLingerOnOff=wsLingerNoSet');
  DoStatus('-WLingerTimeout=0');
  DoStatus('-WListenBacklog=15');
  DoStatus('-WSocketErrs=wsErrTech');
  DoStatus('-WSocketFamily=sfIPv4');
  DoStatus('-WAuthDigestMethod=daAuth');
  DoStatus('-WAuthDigestNonceLifeTimeMin=1');
  DoStatus('-WAuthRealm=ics');
  DoStatus('-WAuthTypes');
  DoStatus('-WOptions');
  DoStatus('');
  DoStatus('2022-11');
  DoStatus('enjoy, by.qq600585');
  DoStatus('');
end;

procedure Do_Check_On_Exit;
var
  n: string;
  cH: TC40_Console_Help;
begin
  cH := TC40_Console_Help.Create;
  repeat
    TCompute.Sleep(100);
    Readln(n);
    cH.Run_HelpCmd(n);
  until cH.IsExit;
  DisposeObject(cH);
  exit_signal := True;
end;

function Extract_CMD: Boolean;
var
  i: Integer;
  n, tmp: U_String;
  isHelp: Boolean;
  web_param: THashStringList;
begin
  Result := False;
  host := '127.0.0.1';
  port := '9399';
  web_param := THashStringList.Create;

  for i := 1 to ParamCount() do
    begin
      n := ParamStr(i);
      if umlMultipleMatch(['-h', '-help'], n) then
        begin
          isHelp := True;
        end
      else if umlMultipleMatch(['-c4:*'], n) then
        begin
          host := umlDeleteFirstStr(n, ':');
          ExtractHostAddress(host, port);
        end
      else if umlMultipleMatch(['-W*:*', '-W*=*', '/W*:*'], n) then
        begin
          tmp := umlGetFirstStr(n, ':=');
          tmp.DeleteFirst;
          tmp.DeleteFirst;
          web_param.Add(tmp, umlDeleteFirstStr(n, ':='));
        end;
    end;

  if isHelp then
    begin
      Print_Help;
      exit;
    end;

  Build_C4_Net(host, port);

  web_param.AutoUpdateDefaultValue := True;
  Init_Http_Serv(web_param);
  DisposeObject(web_param);

  exit_signal := False;
  TCompute.RunC_NP(@Do_Check_On_Exit);
  while not exit_signal do
    begin
      C40Progress(1);
      web.serv.ProcessMessages;
    end;
end;

begin
  ExitCode := 0;
  if Extract_CMD() then
      ExitCode := 1;
  C40Clean();
  DoStatus('exit code:%d', [ExitCode]);
end.
