{ ****************************************************************************** }
{ * IndyInterface                                                              * }
{ ****************************************************************************** }
(*
  update history
*)
unit Z.Net.Client.Indy;

{$I ..\Z.Define.inc}

interface

uses Z.Net, Z.Core,
  Z.DFE, Z.ListEngine, Z.MemoryStream,

  Classes, SysUtils,
  IdTCPClient, IdYarn, IdStack,
  IDGlobal, IdBaseComponent, IdComponent, IdTCPConnection, IdContext,
  Z.Status, Z.UnicodeMixedLib, Z.PascalStrings, Z.UPascalStrings;

type
  TZNet_Client_Indy = class;

  TIDClient_PeerIO = class(TPeerIO)
  public
    function Context: TIdTCPClient;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure Write_IO_Buffer(const buff: PByte; const Size: NativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;
  end;

  TZNet_Client_Indy = class(TZNet_Client)
  protected
    FDriver: TIdTCPClient;
    ClientIntf: TIDClient_PeerIO;
    FProgressing: Boolean;

    FOnAsyncConnectNotify_C: TOnState_C;
    FOnAsyncConnectNotify_M: TOnState_M;
    FOnAsyncConnectNotify_P: TOnState_P;

    procedure AsyncConnect(addr: SystemString; Port: Word; OnResultCall: TOnState_C; OnResultMethod: TOnState_M; OnResultProc: TOnState_P); overload;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure TriggerDoConnectFailed; override;
    procedure TriggerDoConnectFinished; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerIO; override;
    procedure Progress; override;

    procedure AsyncConnectC(addr: SystemString; Port: Word; const OnResult: TOnState_C); overload; override;
    procedure AsyncConnectM(addr: SystemString; Port: Word; const OnResult: TOnState_M); overload; override;
    procedure AsyncConnectP(addr: SystemString; Port: Word; const OnResult: TOnState_P); overload; override;
    function Connect(addr: SystemString; Port: Word): Boolean; override;
    procedure Disconnect; override;
  end;

procedure CheckIPV6(hostName: SystemString; Port: Word);

var
  DefaultIPVersion: TIdIPVersion;

implementation

procedure CheckIPV6(hostName: SystemString; Port: Word);
var
  cli: TIdTCPClient;
begin
  cli := TIdTCPClient.Create(nil);
  cli.Host := hostName;
  cli.Port := Port;
  cli.BoundIP := '';
  cli.BoundPort := 0;
  cli.IPVersion := TIdIPVersion.Id_IPv6;
  cli.ReuseSocket := TIdReuseSocket.rsFalse;
  cli.UseNagle := False;

  try
    cli.Connect;
    if cli.Connected then
      begin
        DefaultIPVersion := TIdIPVersion.Id_IPv6;
        cli.Disconnect;
      end
    else
        DefaultIPVersion := TIdIPVersion.Id_IPv4;
  except
  end;

  DisposeObject(cli);
end;

function ToIDBytes(p: PByte; Size: Integer): TIdBytes;
begin
  SetLength(Result, Size);
  CopyPtr(p, @Result[0], Size);
end;

function TIDClient_PeerIO.Context: TIdTCPClient;
begin
  Result := IOInterface as TIdTCPClient;
end;

function TIDClient_PeerIO.Connected: Boolean;
begin
  Result := Context.Connected;
end;

procedure TIDClient_PeerIO.Disconnect;
begin
  CheckAndTriggerFailedWaitResult();
  Context.Disconnect;
end;

procedure TIDClient_PeerIO.Write_IO_Buffer(const buff: PByte; const Size: NativeInt);
begin
  if Size > 0 then
      Context.IOHandler.write(ToIDBytes(buff, Size));
end;

procedure TIDClient_PeerIO.WriteBufferOpen;
begin
  Context.IOHandler.WriteBufferOpen;
end;

procedure TIDClient_PeerIO.WriteBufferFlush;
begin
  Context.IOHandler.WriteBufferFlush;
end;

procedure TIDClient_PeerIO.WriteBufferClose;
begin
  Context.IOHandler.WriteBufferClose;
end;

function TIDClient_PeerIO.GetPeerIP: SystemString;
begin
  Result := Context.Host;
end;

procedure TZNet_Client_Indy.AsyncConnect(addr: SystemString; Port: Word; OnResultCall: TOnState_C; OnResultMethod: TOnState_M; OnResultProc: TOnState_P);
begin
  Disconnect;

  DisposeObject(ClientIntf);

  FDriver := TIdTCPClient.Create(nil);
  ClientIntf := TIDClient_PeerIO.Create(Self, FDriver);
  FProgressing := False;

  if IsIPv4(addr) then
      FDriver.IPVersion := TIdIPVersion.Id_IPv4
  else if IsIPV6(addr) then
      FDriver.IPVersion := TIdIPVersion.Id_IPv6
  else
      FDriver.IPVersion := DefaultIPVersion;

  FDriver.Host := addr;
  FDriver.Port := Port;
  FDriver.BoundIP := '';
  FDriver.BoundPort := 0;
  FDriver.ReuseSocket := TIdReuseSocket.rsFalse;
  FDriver.UseNagle := False;
  Progress;

  FOnAsyncConnectNotify_C := OnResultCall;
  FOnAsyncConnectNotify_M := OnResultMethod;
  FOnAsyncConnectNotify_P := OnResultProc;

  FDriver.ConnectTimeout := 500;
  try
    FDriver.Connect;
    Progress;
  except
    if (IsIPv4(addr)) or (IsIPV6(addr)) then
      begin
        FDriver := TIdTCPClient.Create(nil);
        FDriver.IPVersion := DefaultIPVersion;
        FDriver.ConnectTimeout := 0;
        FDriver.ReadTimeout := -1;
        FDriver.UseNagle := False;

        DisposeObject(ClientIntf);
        ClientIntf := TIDClient_PeerIO.Create(Self, FDriver);

        TriggerDoConnectFailed;

        Exit;
      end
    else
      begin
        if DefaultIPVersion = TIdIPVersion.Id_IPv4 then
            DefaultIPVersion := TIdIPVersion.Id_IPv6
        else
            DefaultIPVersion := TIdIPVersion.Id_IPv4;

        try
          FDriver.Connect;
          Progress;
        except
          if (not IsIPv4(addr)) and (not IsIPV6(addr)) then
            begin
              if DefaultIPVersion = TIdIPVersion.Id_IPv4 then
                  DefaultIPVersion := TIdIPVersion.Id_IPv6
              else
                  DefaultIPVersion := TIdIPVersion.Id_IPv4;
            end;

          FDriver := TIdTCPClient.Create(nil);
          FDriver.IPVersion := DefaultIPVersion;
          FDriver.ConnectTimeout := 0;
          FDriver.ReadTimeout := -1;
          FDriver.UseNagle := False;

          DisposeObject(ClientIntf);
          ClientIntf := TIDClient_PeerIO.Create(Self, FDriver);

          TriggerDoConnectFailed;

          Exit;
        end;
      end;
  end;

  if not FDriver.Connected then
    begin
      TriggerDoConnectFailed;
      Exit;
    end;

  DoConnected(ClientIntf);
end;

constructor TZNet_Client_Indy.Create;
begin
  inherited Create;
  EnabledAtomicLockAndMultiThread := False;

  FDriver := TIdTCPClient.Create(nil);
  FDriver.IPVersion := DefaultIPVersion;
  FDriver.ConnectTimeout := 0;
  FDriver.ReadTimeout := -1;
  FDriver.UseNagle := False;

  ClientIntf := TIDClient_PeerIO.Create(Self, FDriver);
  FProgressing := False;

  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;

  name:='INDY-Client';
end;

destructor TZNet_Client_Indy.Destroy;
begin
  try
      FDriver.Disconnect;
  except
  end;

  try
    // disposeObject(FDriver);
      FDriver := nil;
  except
  end;

  try
      DisposeObject(ClientIntf);
  except
  end;

  inherited Destroy;
end;

procedure TZNet_Client_Indy.TriggerDoConnectFailed;
begin
  inherited TriggerDoConnectFailed;

  try
    if Assigned(FOnAsyncConnectNotify_C) then
        FOnAsyncConnectNotify_C(False);
    if Assigned(FOnAsyncConnectNotify_M) then
        FOnAsyncConnectNotify_M(False);
    if Assigned(FOnAsyncConnectNotify_P) then
        FOnAsyncConnectNotify_P(False);
  except
  end;

  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;
end;

procedure TZNet_Client_Indy.TriggerDoConnectFinished;
begin
  inherited TriggerDoConnectFinished;

  try
    if Assigned(FOnAsyncConnectNotify_C) then
        FOnAsyncConnectNotify_C(True);
    if Assigned(FOnAsyncConnectNotify_M) then
        FOnAsyncConnectNotify_M(True);
    if Assigned(FOnAsyncConnectNotify_P) then
        FOnAsyncConnectNotify_P(True);
  except
  end;

  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;
end;

function TZNet_Client_Indy.Connected: Boolean;
begin
  try
      Result := FDriver.Connected;
  except
    FDriver := TIdTCPClient.Create(nil);
    FDriver.IPVersion := DefaultIPVersion;
    FDriver.ConnectTimeout := 0;
    FDriver.ReadTimeout := -1;
    FDriver.UseNagle := False;

    DisposeObject(ClientIntf);

    ClientIntf := TIDClient_PeerIO.Create(Self, FDriver);
    Result := False;
  end;
end;

function TZNet_Client_Indy.ClientIO: TPeerIO;
begin
  Result := ClientIntf;
end;

procedure TZNet_Client_Indy.Progress;
var
  t: TTimeTick;
  iBuf: TIdBytes;
begin
  if FProgressing then
      Exit;

  if Connected then
    begin
      FProgressing := True;

      try
        FDriver.IOHandler.CheckForDataOnSource(2);
        while FDriver.IOHandler.InputBuffer.Size > 0 do
          begin
            FDriver.IOHandler.InputBuffer.ExtractToBytes(iBuf);
            FDriver.IOHandler.InputBuffer.Clear;
            ClientIntf.Write_Physics_Fragment(@iBuf[0], length(iBuf));
            SetLength(iBuf, 0);
            FDriver.IOHandler.CheckForDataOnSource(5);
          end;
      finally
          FProgressing := False;
      end;

      try
          inherited Progress;
      except
        ClientIntf.Disconnect;
        FProgressing := False;
      end;
    end
  else if LastConnectIsSuccessed then
      TriggerDoDisconnect;
end;

procedure TZNet_Client_Indy.AsyncConnectC(addr: SystemString; Port: Word; const OnResult: TOnState_C);
begin
  AsyncConnect(addr, Port, OnResult, nil, nil);
end;

procedure TZNet_Client_Indy.AsyncConnectM(addr: SystemString; Port: Word; const OnResult: TOnState_M);
begin
  AsyncConnect(addr, Port, nil, OnResult, nil);
end;

procedure TZNet_Client_Indy.AsyncConnectP(addr: SystemString; Port: Word; const OnResult: TOnState_P);
begin
  AsyncConnect(addr, Port, nil, nil, OnResult);
end;

function TZNet_Client_Indy.Connect(addr: SystemString; Port: Word): Boolean;
var
  t: TTimeTick;
begin
  Result := False;

  Disconnect;

  DisposeObject(ClientIntf);

  FDriver := TIdTCPClient.Create(nil);
  ClientIntf := TIDClient_PeerIO.Create(Self, FDriver);
  FProgressing := False;

  if IsIPv4(addr) then
      FDriver.IPVersion := TIdIPVersion.Id_IPv4
  else if IsIPV6(addr) then
      FDriver.IPVersion := TIdIPVersion.Id_IPv6
  else
      FDriver.IPVersion := DefaultIPVersion;

  FDriver.Host := addr;
  FDriver.Port := Port;
  FDriver.BoundIP := '';
  FDriver.BoundPort := 0;
  FDriver.ReuseSocket := TIdReuseSocket.rsFalse;
  FDriver.UseNagle := False;
  Progress;

  FOnAsyncConnectNotify_C := nil;
  FOnAsyncConnectNotify_M := nil;
  FOnAsyncConnectNotify_P := nil;

  FDriver.ConnectTimeout := 3000;
  try
    FDriver.Connect;
    Progress;
  except
    if (IsIPv4(addr)) or (IsIPV6(addr)) then
      begin
        FDriver := TIdTCPClient.Create(nil);
        FDriver.IPVersion := DefaultIPVersion;
        FDriver.ConnectTimeout := 0;
        FDriver.ReadTimeout := -1;
        FDriver.UseNagle := False;

        DisposeObject(ClientIntf);
        ClientIntf := TIDClient_PeerIO.Create(Self, FDriver);
        Exit;
      end
    else
      begin
        if DefaultIPVersion = TIdIPVersion.Id_IPv4 then
            DefaultIPVersion := TIdIPVersion.Id_IPv6
        else
            DefaultIPVersion := TIdIPVersion.Id_IPv4;

        try
          FDriver.Connect;
          Progress;
        except
          if (not IsIPv4(addr)) and (not IsIPV6(addr)) then
            begin
              if DefaultIPVersion = TIdIPVersion.Id_IPv4 then
                  DefaultIPVersion := TIdIPVersion.Id_IPv6
              else
                  DefaultIPVersion := TIdIPVersion.Id_IPv4;
            end;

          FDriver := TIdTCPClient.Create(nil);
          FDriver.IPVersion := DefaultIPVersion;
          FDriver.ConnectTimeout := 0;
          FDriver.ReadTimeout := -1;
          FDriver.UseNagle := False;

          DisposeObject(ClientIntf);
          ClientIntf := TIDClient_PeerIO.Create(Self, FDriver);
          Exit;
        end;
      end;
  end;

  if not FDriver.Connected then
      Exit;

  DoConnected(ClientIntf);

  t := GetTimeTick + 3000;
  while (not RemoteInited) and (FDriver.Connected) and (GetTimeTick < t) do
    begin
      Progress;
      FDriver.IOHandler.CheckForDataOnSource(100);
    end;

  Result := (RemoteInited) and (FDriver.Connected);
end;

procedure TZNet_Client_Indy.Disconnect;
begin
  if not Connected then
      Exit;

  FDriver.Disconnect;
end;

initialization

DefaultIPVersion := TIdIPVersion.Id_IPv4;

finalization

end.
