(*
https://zpascal.net
https://github.com/PassByYou888/ZNet
https://github.com/PassByYou888/zRasterization
https://github.com/PassByYou888/ZSnappy
https://github.com/PassByYou888/Z-AI1.4
https://github.com/PassByYou888/InfiniteIoT
https://github.com/PassByYou888/zMonitor_3rd_Core
https://github.com/PassByYou888/tcmalloc4p
https://github.com/PassByYou888/jemalloc4p
https://github.com/PassByYou888/zCloud
https://github.com/PassByYou888/ZServer4D
https://github.com/PassByYou888/zShell
https://github.com/PassByYou888/ZDB2.0
https://github.com/PassByYou888/zGameWare
https://github.com/PassByYou888/CoreCipher
https://github.com/PassByYou888/zChinese
https://github.com/PassByYou888/zSound
https://github.com/PassByYou888/zExpression
https://github.com/PassByYou888/ZInstaller2.0
https://github.com/PassByYou888/zAI
https://github.com/PassByYou888/NetFileService
https://github.com/PassByYou888/zAnalysis
https://github.com/PassByYou888/PascalString
https://github.com/PassByYou888/zInstaller
https://github.com/PassByYou888/zTranslate
https://github.com/PassByYou888/zVision
https://github.com/PassByYou888/FFMPEG-Header
*)
{ ****************************************************************************** }
{ * XNAT tunnel                                                                * }
{ ****************************************************************************** }
unit Z.Net.XNAT.Service;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine, Z.TextDataEngine,
  Z.Cipher, Z.DFE, Z.MemoryStream, Z.Net, Z.Net.XNAT.Physics;

type
  TXNATService = class;
  TXServerCustomProtocol = class;
  TXServiceListen = class;

  TXServiceRecvVM_Special = class(TPeer_IO_User_Special)
  private
    OwnerMapping: TXServiceListen;
    RecvID, SendID: Cardinal;
    FMaxWorkload, CurrentWorkload: Cardinal;
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TXServiceSendVM_Special = class(TPeer_IO_User_Special)
  private
    OwnerMapping: TXServiceListen;
    RecvID, SendID: Cardinal;
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TXCustomP2PVM_Server = class(TZNet_WithP2PVM_Server)
  private
    OwnerMapping: TXServiceListen;
  end;

  TXServiceListen = class(TCore_Object_Intermediate)
  private
    FOwner: TXNATService;
    FListenAddr: TPascalString;
    FListenPort: TPascalString;
    FMapping: TPascalString;

    FProtocol: TXServerCustomProtocol;
    FActivted: Boolean;
    FTest_Listening_Passed: Boolean;

    FRecvTunnel: TXCustomP2PVM_Server;
    FRecvTunnel_IPV6: TIPV6;
    FRecvTunnel_Port: Word;

    FSendTunnel: TXCustomP2PVM_Server;
    FSendTunnel_IPV6: TIPV6;
    FSendTunnel_Port: Word;

    { Distributed Workload supported }
    DistributedWorkload: Boolean;

    XServerTunnel: TXNATService;
    TimeOut: TTimeTick;

    { complete buffer metric }
    Complete_Buffer_Sum: Int64;

    procedure Init;
    function Open: Boolean;

    { worker tunnel }
    procedure PickWorkloadTunnel(var rID, sID: Cardinal);

    { requestListen: activted listen and reponse states }
    procedure cmd_RequestListen(Sender: TPeerIO; InData, OutData: TDFE);
    { workload: update workload states }
    procedure cmd_workload(Sender: TPeerIO; InData: TDFE);
    { connect forward }
    procedure cmd_connect_reponse(Sender: TPeerIO; InData: TDFE);
    procedure cmd_disconnect_reponse(Sender: TPeerIO; InData: TDFE);
    { data forward }
    procedure cmd_data(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    { states }
    procedure SetActivted(const Value: Boolean);
  public
    UserData: Pointer;
    UserObject: TCore_Object;
    constructor Create(Owner_: TXNATService); virtual;
    destructor Destroy; override;

    property Protocol: TXServerCustomProtocol read FProtocol; // physics protocol

    property ListenAddr: TPascalString read FListenAddr;
    property ListenPort: TPascalString read FListenPort;
    property Mapping: TPascalString read FMapping;
    property Test_Listening_Passed: Boolean read FTest_Listening_Passed;
    property Activted: Boolean read FActivted write SetActivted;
  end;

  TXServiceListen_Class = class of TXServiceListen;

  TXServerUserSpecial = class(TPeer_IO_User_Special)
  private
    RemoteProtocol_ID: Cardinal;
    RemoteProtocol_Inited: Boolean;
    RequestBuffer: TMS64;
    r_id, s_id: Cardinal; { IO in TXServiceListen }
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TXServerCustomProtocol = class(TXPhysicsServer)
  private
    ShareListen: TXServiceListen;
  public
    procedure OnReceiveBuffer(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean); override;
    procedure DoIOConnectBefore(Sender: TPeerIO); override;
    procedure DoIODisconnect(Sender: TPeerIO); override;
  end;

  TPhysicsEngine_Special = class(TPeer_IO_User_Special)
  protected
    FXNAT_VS: TXNATService;
    procedure PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
    procedure PhysicsVMBuildAuthToken_Result;
    procedure PhysicsOpenVM_Result(const cState: Boolean);
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TXServiceMappingList = TGenericsList<TXServiceListen>;
  TOn_XNATService_Open_Tunnel_Done = procedure(Sender: TXNATService; State: Boolean) of object;

  TXNATService = class(TCore_InterfacedObject_Intermediate, IIOInterface, IZNet_VMInterface)
  private
    { external tunnel }
    FShareListenList: TXServiceMappingList;
    { internal physics tunnel }
    FPhysicsEngine: TZNet;
    FQuiet: Boolean;
    FActivted: Boolean;
    WaitAsyncConnecting: Boolean;
    WaitAsyncConnecting_BeginTime: TTimeTick;

    { physis protocol }
    procedure IPV6Listen(Sender: TPeerIO; InData, OutData: TDFE);
    { IO Interface }
    procedure PeerIO_Create(const Sender: TPeerIO);
    procedure PeerIO_Destroy(const Sender: TPeerIO);
    { p2pVM Interface }
    procedure p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
    procedure p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
    procedure p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
    procedure p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
    procedure p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
    { backcall }
    procedure PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
    { trigger open done }
    procedure Do_Open_Done(State: Boolean);
    procedure Set_Quiet(const Value: Boolean);
  public
    { tunnel parameter }
    Host: TPascalString;
    Port: TPascalString;
    AuthToken: TPascalString;
    MaxVMFragment: TPascalString;
    {
      Compression of CompleteBuffer packets using zLib
      feature of zLib: slow compression and fast decompression.
      XNAT is used to non compression or non encryption protocol, the option can be opened so upspeed.
      else. protocol is encrypted or compressed, opening this ProtocolCompressed additional burden on CPU.
      ProtocolCompressed set closed by default.
    }
    ProtocolCompressed: Boolean;
    Instance_Class: TXServiceListen_Class;
    On_Open_Tunnel_Done: TOn_XNATService_Open_Tunnel_Done;
    Open_Done: Boolean;
  public
    property Activted: Boolean read FActivted;
    property PhysicsEngine: TZNet read FPhysicsEngine;
    property ShareListenList: TXServiceMappingList read FShareListenList;
    property Quiet: Boolean read FQuiet write Set_Quiet;
    constructor Create;
    destructor Destroy; override;
    procedure Reset();
    function AddMapping(const ListenAddr, ListenPort, FMapping: TPascalString; TimeOut: TTimeTick): TXServiceListen;
    function AddNoDistributedMapping(const ListenAddr, ListenPort, FMapping: TPascalString; TimeOut: TTimeTick): TXServiceListen;
    procedure OpenTunnel(MODEL: TXNAT_PHYSICS_MODEL); overload;
    procedure OpenTunnel; overload;
    procedure Progress;
  end;

implementation

constructor TXServiceRecvVM_Special.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  OwnerMapping := nil;
  RecvID := 0;
  SendID := 0;
  FMaxWorkload := 100;
  CurrentWorkload := 0;
end;

destructor TXServiceRecvVM_Special.Destroy;
var
  IO_Array: TIO_Array;
  p_id: Cardinal;
  p_io: TPeerIO;
begin
  if (OwnerMapping <> nil) then
    begin
      OwnerMapping.FSendTunnel.Disconnect(SendID);

      OwnerMapping.FProtocol.GetIO_Array(IO_Array);
      for p_id in IO_Array do
        begin
          p_io := OwnerMapping.FProtocol.PeerIO[p_id];
          if TXServerUserSpecial(p_io.UserSpecial).r_id = FOwner.ID then
              p_io.DelayClose(0);
        end;
    end;

  inherited Destroy;
end;

constructor TXServiceSendVM_Special.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  OwnerMapping := nil;
  RecvID := 0;
  SendID := 0;
end;

destructor TXServiceSendVM_Special.Destroy;
begin
  try
    if (OwnerMapping <> nil) then
      if OwnerMapping.FRecvTunnel.ExistsID(RecvID) then
          OwnerMapping.FRecvTunnel.Disconnect(RecvID);
  except
  end;
  inherited Destroy;
end;

procedure TXServiceListen.Init;
begin
  FListenAddr := '';
  FListenPort := '0';
  FMapping := '';
  FProtocol := nil;
  FActivted := False;
  FTest_Listening_Passed := False;
  FRecvTunnel := nil;
  FillPtrByte(@FRecvTunnel_IPV6, SizeOf(TIPV6), 0);
  FRecvTunnel_Port := 0;
  FSendTunnel := nil;
  FillPtrByte(@FSendTunnel_IPV6, SizeOf(TIPV6), 0);
  FSendTunnel_Port := 0;
  DistributedWorkload := False;
  XServerTunnel := nil;
  TimeOut := 0;
  Complete_Buffer_Sum := 0;
  UserData := nil;
  UserObject := nil;
end;

function TXServiceListen.Open: Boolean;
var
  nt: Pointer;
begin
  { build receive tunnel }
  if FRecvTunnel = nil then
    begin
      FRecvTunnel := TXCustomP2PVM_Server.Create;
      FRecvTunnel.QuietMode := FOwner.Quiet;
      FRecvTunnel.CompleteBufferSwapSpace := True;
    end;

  { sequence sync }
  FRecvTunnel.SyncOnCompleteBuffer := True;
  FRecvTunnel.SyncOnResult := True;
  FRecvTunnel.SwitchMaxPerformance;
  { mapping interface }
  FRecvTunnel.OwnerMapping := Self;
  FRecvTunnel.UserSpecialClass := TXServiceRecvVM_Special;
  { compressed complete buffer }
  FRecvTunnel.CompleteBufferCompressed := XServerTunnel.ProtocolCompressed;
  { build virtual address }
  nt := @FRecvTunnel;
  TSHA3.SHAKE128(@FRecvTunnel_IPV6, @nt, SizeOf(nt), 128);
  { build virtual port }
  FRecvTunnel_Port := umlCRC16(@FRecvTunnel_IPV6, SizeOf(TIPV6));
  { disable data status print }
  FRecvTunnel.PrintParams[C_Connect_reponse] := False;
  FRecvTunnel.PrintParams[C_Disconnect_reponse] := False;
  FRecvTunnel.PrintParams[C_Data] := False;
  FRecvTunnel.PrintParams[C_Workload] := False;

  if not FRecvTunnel.ExistsRegistedCmd(C_RequestListen) then
      FRecvTunnel.RegisterStream(C_RequestListen).OnExecute := cmd_RequestListen;

  if not FRecvTunnel.ExistsRegistedCmd(C_Workload) then
      FRecvTunnel.RegisterDirectStream(C_Workload).OnExecute := cmd_workload;

  if not FRecvTunnel.ExistsRegistedCmd(C_Connect_reponse) then
      FRecvTunnel.RegisterDirectStream(C_Connect_reponse).OnExecute := cmd_connect_reponse;

  if not FRecvTunnel.ExistsRegistedCmd(C_Disconnect_reponse) then
      FRecvTunnel.RegisterDirectStream(C_Disconnect_reponse).OnExecute := cmd_disconnect_reponse;

  if not FRecvTunnel.ExistsRegistedCmd(C_Data) then
      FRecvTunnel.RegisterCompleteBuffer(C_Data).OnExecute := cmd_data;

  { build send tunnel }
  if FSendTunnel = nil then
    begin
      FSendTunnel := TXCustomP2PVM_Server.Create;
      FSendTunnel.QuietMode := FOwner.Quiet;
      FSendTunnel.CompleteBufferSwapSpace := True;
    end;

  { sequence sync }
  FSendTunnel.SyncOnCompleteBuffer := True;
  FSendTunnel.SyncOnResult := True;
  FSendTunnel.SwitchMaxPerformance;
  { mapping interface }
  FSendTunnel.OwnerMapping := Self;
  FSendTunnel.UserSpecialClass := TXServiceSendVM_Special;
  { compressed complete buffer }
  FSendTunnel.CompleteBufferCompressed := XServerTunnel.ProtocolCompressed;
  { build virtual address }
  nt := @FSendTunnel;
  TSHA3.SHAKE128(@FSendTunnel_IPV6, @nt, SizeOf(nt), 128);
  { build virtual port }
  FSendTunnel_Port := umlCRC16(@FSendTunnel_IPV6, SizeOf(TIPV6));
  { disable data status print }
  FSendTunnel.PrintParams[C_Connect_request] := False;
  FSendTunnel.PrintParams[C_Disconnect_request] := False;
  FSendTunnel.PrintParams[C_Data] := False;

  FRecvTunnel.StartService(IPv6ToStr(FRecvTunnel_IPV6), FRecvTunnel_Port);
  FSendTunnel.StartService(IPv6ToStr(FSendTunnel_IPV6), FSendTunnel_Port);

  if FProtocol = nil then
      FProtocol := TXServerCustomProtocol.Create;
  FProtocol.QuietMode := FOwner.Quiet;
  FProtocol.ShareListen := Self;
  FProtocol.Protocol := cpCustom;
  FProtocol.UserSpecialClass := TXServerUserSpecial;
  FProtocol.TimeOutIDLE := TimeOut;

  SetActivted(True);
  Result := FActivted;
  FTest_Listening_Passed := FActivted;
  SetActivted(False);

  if not Result then
      FProtocol.Error('detect listen bind %s:%s failed!', [TranslateBindAddr(FListenAddr), FListenPort.Text]);
end;

procedure TXServiceListen.PickWorkloadTunnel(var rID, sID: Cardinal);
var
  rVM: TXServiceRecvVM_Special;
  buff: TIO_Array;
  ID: Cardinal;
  r_io: TPeerIO;
  f, d: Double;
begin
  rID := 0;
  sID := 0;
  if FRecvTunnel.Count = 0 then
      exit;
  if FSendTunnel.Count = 0 then
      exit;

  rVM := TXServiceRecvVM_Special(FRecvTunnel.FirstIO.UserSpecial);
  f := rVM.CurrentWorkload / rVM.FMaxWorkload;

  FRecvTunnel.GetIO_Array(buff);
  for ID in buff do
    begin
      r_io := FRecvTunnel.PeerIO[ID];
      if (r_io <> nil) and (r_io.UserSpecial <> rVM) then
        begin
          with TXServiceRecvVM_Special(r_io.UserSpecial) do
              d := CurrentWorkload / FMaxWorkload;
          if d < f then
            begin
              f := d;
              rVM := TXServiceRecvVM_Special(r_io.UserSpecial);
            end;
        end;
    end;

  if not FSendTunnel.Exists(rVM.SendID) then
      exit;

  rID := rVM.RecvID;
  sID := rVM.SendID;
end;

procedure TXServiceListen.cmd_RequestListen(Sender: TPeerIO; InData, OutData: TDFE);
var
  RecvID, SendID: Cardinal;
  rVM: TXServiceRecvVM_Special;
  sVM: TXServiceSendVM_Special;
begin
  RecvID := InData.Reader.ReadCardinal;
  SendID := InData.Reader.ReadCardinal;

  if DistributedWorkload then
    begin
      if not FRecvTunnel.Exists(RecvID) then
        begin
          OutData.WriteBool(False);
          OutData.WriteString(PFormat('receive tunnel ID illegal %d', [RecvID]));
          exit;
        end;

      if not FSendTunnel.Exists(SendID) then
        begin
          OutData.WriteBool(False);
          OutData.WriteString(PFormat('send tunnel ID illegal %d', [SendID]));
          exit;
        end;

      if not Activted then
        begin
          Activted := True;
          if (not Activted) then
            begin
              OutData.WriteBool(False);
              OutData.WriteString(PFormat('remote service illegal bind IP %s port:%s', [FListenAddr.Text, FListenPort.Text]));
              exit;
            end;
        end;

      rVM := TXServiceRecvVM_Special(FRecvTunnel.PeerIO[RecvID].UserSpecial);
      rVM.OwnerMapping := Self;
      rVM.RecvID := RecvID;
      rVM.SendID := SendID;

      sVM := TXServiceSendVM_Special(FSendTunnel.PeerIO[SendID].UserSpecial);
      sVM.OwnerMapping := Self;
      sVM.RecvID := RecvID;
      sVM.SendID := SendID;

      OutData.WriteBool(True);
      OutData.WriteString(PFormat('bridge XNAT service successed, bind IP %s port:%s', [FListenAddr.Text, FListenPort.Text]));
    end
  else
    begin
      if Activted then
        begin
          OutData.WriteBool(False);
          OutData.WriteString(PFormat('bridge service no support distributed workload', []));
          exit;
        end;

      if not FRecvTunnel.Exists(RecvID) then
        begin
          OutData.WriteBool(False);
          OutData.WriteString(PFormat('receive tunnel ID illegal %d', [RecvID]));
          exit;
        end;

      if not FSendTunnel.Exists(SendID) then
        begin
          OutData.WriteBool(False);
          OutData.WriteString(PFormat('send tunnel ID illegal %d', [SendID]));
          exit;
        end;

      Activted := True;
      if (not Activted) then
        begin
          OutData.WriteBool(False);
          OutData.WriteString(PFormat('remote service illegal bind IP %s port:%s', [FListenAddr.Text, FListenPort.Text]));
          exit;
        end;

      rVM := TXServiceRecvVM_Special(FRecvTunnel.PeerIO[RecvID].UserSpecial);
      rVM.OwnerMapping := Self;
      rVM.RecvID := RecvID;
      rVM.SendID := SendID;

      sVM := TXServiceSendVM_Special(FSendTunnel.PeerIO[SendID].UserSpecial);
      sVM.OwnerMapping := Self;
      sVM.RecvID := RecvID;
      sVM.SendID := SendID;

      OutData.WriteBool(True);
      OutData.WriteString(PFormat('bridge XNAT service successed, bind IP %s port:%s', [FListenAddr.Text, FListenPort.Text]));
    end;
end;

procedure TXServiceListen.cmd_workload(Sender: TPeerIO; InData: TDFE);
var
  rVM: TXServiceRecvVM_Special;
begin
  rVM := TXServiceRecvVM_Special(Sender.UserSpecial);
  rVM.FMaxWorkload := InData.Reader.ReadCardinal;
  rVM.CurrentWorkload := InData.Reader.ReadCardinal;
end;

procedure TXServiceListen.cmd_connect_reponse(Sender: TPeerIO; InData: TDFE);
var
  cState: Boolean;
  remote_id, local_id: Cardinal;
  phy_io, s_io: TPeerIO;
  XUserSpec: TXServerUserSpecial;
  nSiz: NativeInt;
  nBuff: PByte;
begin
  cState := InData.Reader.ReadBool;
  remote_id := InData.Reader.ReadCardinal;
  local_id := InData.Reader.ReadCardinal;
  phy_io := FProtocol.PeerIO[local_id];

  if phy_io = nil then
      exit;

  if cState then
    begin
      XUserSpec := TXServerUserSpecial(phy_io.UserSpecial);
      XUserSpec.RemoteProtocol_ID := remote_id;
      XUserSpec.RemoteProtocol_Inited := True;

      if XUserSpec.RequestBuffer.Size > 0 then
        begin
          s_io := FSendTunnel.PeerIO[XUserSpec.s_id];
          if s_io <> nil then
            begin
              Build_XNAT_Buff(XUserSpec.RequestBuffer.Memory, XUserSpec.RequestBuffer.Size, Sender.ID, XUserSpec.RemoteProtocol_ID, nSiz, nBuff);
              s_io.SendCompleteBuffer(C_Data, nBuff, nSiz, True);
            end;
          XUserSpec.RequestBuffer.Clear;
        end;
    end
  else
      phy_io.DelayClose;
end;

procedure TXServiceListen.cmd_disconnect_reponse(Sender: TPeerIO; InData: TDFE);
var
  remote_id, local_id: Cardinal;
  phy_io: TPeerIO;
begin
  remote_id := InData.Reader.ReadCardinal;
  local_id := InData.Reader.ReadCardinal;
  phy_io := FProtocol.PeerIO[local_id];

  if phy_io = nil then
      exit;

  phy_io.DelayClose(1.0);
end;

procedure TXServiceListen.cmd_data(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  local_id, remote_id: Cardinal;
  destSiz: NativeInt;
  destBuff: PByte;
  phy_io: TPeerIO;
begin
  Extract_XNAT_Buff(InData, DataSize, remote_id, local_id, destSiz, destBuff);
  phy_io := FProtocol.PeerIO[local_id];

  if phy_io <> nil then
    begin
      FProtocol.BeginWriteBuffer(phy_io);
      FProtocol.WriteBuffer(phy_io, destBuff, destSiz);
      FProtocol.EndWriteBuffer(phy_io);
    end;
end;

procedure TXServiceListen.SetActivted(const Value: Boolean);
begin
  if Value then
    begin
      FActivted := FProtocol.StartService(FListenAddr, umlStrToInt(FListenPort));
      FProtocol.Print('Start listen %s %s', [TranslateBindAddr(FListenAddr.Text), FListenPort.Text]);
    end
  else
    begin
      FProtocol.StopService;
      FActivted := False;
      FProtocol.Print('Close listen %s %s', [TranslateBindAddr(FListenAddr.Text), FListenPort.Text]);
    end;
end;

constructor TXServiceListen.Create(Owner_: TXNATService);
begin
  inherited Create;
  FOwner := Owner_;
  Init;
end;

destructor TXServiceListen.Destroy;
begin
  if FProtocol <> nil then
    begin
      FProtocol.StopService;
    end;

  if FRecvTunnel <> nil then
    begin
      FRecvTunnel.StopService;
    end;

  if FSendTunnel <> nil then
    begin
      FSendTunnel.StopService;
    end;

  DisposeObject(FRecvTunnel);
  DisposeObject(FSendTunnel);
  DisposeObject(FProtocol);
  inherited Destroy;
end;

constructor TXServerUserSpecial.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  RemoteProtocol_ID := 0;
  RemoteProtocol_Inited := False;
  RequestBuffer := TMS64.CustomCreate(8192);
  r_id := 0;
  s_id := 0;
end;

destructor TXServerUserSpecial.Destroy;
begin
  DisposeObject(RequestBuffer);
  inherited Destroy;
end;

procedure TXServerCustomProtocol.OnReceiveBuffer(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
var
  XUserSpec: TXServerUserSpecial;
  nSiz: NativeInt;
  nBuff: PByte;
  s_io: TPeerIO;
begin
  if (ShareListen.FSendTunnel.Count <> 1) and (not ShareListen.DistributedWorkload) then
    begin
      Sender.Print('share listen "%s:%s" no remote support', [ShareListen.FListenAddr.Text, ShareListen.FListenPort.Text]);
      exit;
    end;

  XUserSpec := TXServerUserSpecial(Sender.UserSpecial);
  if not XUserSpec.RemoteProtocol_Inited then
    begin
      XUserSpec.RequestBuffer.WritePtr(buffer, Size);
      exit;
    end;

  s_io := ShareListen.FSendTunnel.PeerIO[XUserSpec.s_id];
  if s_io <> nil then
    begin
      Build_XNAT_Buff(buffer, Size, Sender.ID, XUserSpec.RemoteProtocol_ID, nSiz, nBuff);
      s_io.SendCompleteBuffer(C_Data, nBuff, nSiz, True);
      inc(ShareListen.Complete_Buffer_Sum, nSiz);
      if ShareListen.Complete_Buffer_Sum > 10 * 1024 * 1024 then
        begin
          s_io.Send_NULL;
          ShareListen.Complete_Buffer_Sum := 0;
        end
      else
          ShareListen.FSendTunnel.Progress_IO_Now_Send(s_io);
    end
  else
      Sender.DelayClose(1.0);
end;

procedure TXServerCustomProtocol.DoIOConnectBefore(Sender: TPeerIO);
var
  de: TDFE;
  XUserSpec: TXServerUserSpecial;
  s_io: TPeerIO;
begin
  if (ShareListen.FSendTunnel.Count <> 1) and (not ShareListen.DistributedWorkload) then
    begin
      Sender.Print('share listen "%s:%s" no remote support', [ShareListen.FListenAddr.Text, ShareListen.FListenPort.Text]);
      exit;
    end;

  XUserSpec := TXServerUserSpecial(Sender.UserSpecial);

  if XUserSpec.RemoteProtocol_Inited then
      exit;

  ShareListen.PickWorkloadTunnel(XUserSpec.r_id, XUserSpec.s_id);

  if ShareListen.FSendTunnel.Exists(XUserSpec.s_id) then
    begin
      s_io := ShareListen.FSendTunnel.PeerIO[XUserSpec.s_id];
      de := TDFE.Create;
      de.WriteCardinal(Sender.ID);
      de.WriteString(Sender.PeerIP);
      s_io.SendDirectStreamCmd(C_Connect_request, de);
      DisposeObject(de);
      s_io.Progress;
    end;
  inherited DoIOConnectBefore(Sender);
end;

procedure TXServerCustomProtocol.DoIODisconnect(Sender: TPeerIO);
var
  de: TDFE;
  XUserSpec: TXServerUserSpecial;
  s_io: TPeerIO;
begin
  if (ShareListen.FSendTunnel.Count <> 1) and (not ShareListen.DistributedWorkload) then
    begin
      Sender.Print('share listen "%s:%s" no remote support', [ShareListen.FListenAddr.Text, ShareListen.FListenPort.Text]);
      exit;
    end;

  XUserSpec := TXServerUserSpecial(Sender.UserSpecial);
  if not XUserSpec.RemoteProtocol_Inited then
      exit;

  if ShareListen.FSendTunnel.Exists(XUserSpec.s_id) then
    begin
      s_io := ShareListen.FSendTunnel.PeerIO[XUserSpec.s_id];
      de := TDFE.Create;
      de.WriteCardinal(Sender.ID);
      de.WriteCardinal(TXServerUserSpecial(Sender.UserSpecial).RemoteProtocol_ID);
      s_io.SendDirectStreamCmd(C_Disconnect_request, de);
      DisposeObject(de);
      s_io.Progress;
    end;
  inherited DoIODisconnect(Sender);
end;

procedure TPhysicsEngine_Special.PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
begin
  if cState then
      FOwner.BuildP2PAuthTokenM(PhysicsVMBuildAuthToken_Result)
  else
    begin
      FXNAT_VS.WaitAsyncConnecting := False;
      FXNAT_VS.Do_Open_Done(False);
    end;
end;

procedure TPhysicsEngine_Special.PhysicsVMBuildAuthToken_Result;
begin
  {
    QuantumCryptographyPassword: used sha-3 shake256 cryptography as 512 bits password

    SHA-3 (Secure Hash Algorithm 3) is the latest member of the Secure Hash Algorithm family of standards,
    released by NIST on August 5, 2015.[4][5] Although part of the same series of standards,
    SHA-3 is internally quite different from the MD5-like structure of SHA-1 and SHA-2.

    Keccak is based on a novel approach called sponge construction.
    Sponge construction is based on a wide random function or random permutation, and allows inputting ("absorbing" in sponge terminology) any amount of data,
    and outputting ("squeezing") any amount of data,
    while acting as a pseudorandom function with regard to all previous inputs. This leads to great flexibility.

    NIST does not currently plan to withdraw SHA-2 or remove it from the revised Secure Hash Standard.
    The purpose of SHA-3 is that it can be directly substituted for SHA-2 in current applications if necessary,
    and to significantly improve the robustness of NIST's overall hash algorithm toolkit

    ref wiki
    https://en.wikipedia.org/wiki/SHA-3
  }
  FOwner.OpenP2pVMTunnelM(True, GenerateQuantumCryptographyPassword(FXNAT_VS.AuthToken), PhysicsOpenVM_Result)
end;

procedure TPhysicsEngine_Special.PhysicsOpenVM_Result(const cState: Boolean);
var
  i: Integer;
  shLt: TXServiceListen;
begin
  if cState then
    begin
      FOwner.p2pVMTunnel.MaxVMFragmentSize := umlStrToInt(FXNAT_VS.MaxVMFragment, FOwner.p2pVMTunnel.MaxVMFragmentSize);
      FXNAT_VS.FActivted := True;

      { open share listen }
      for i := 0 to FXNAT_VS.FShareListenList.Count - 1 do
        begin
          shLt := FXNAT_VS.FShareListenList[i];
          shLt.Open;

          { install p2pVM }
          FOwner.p2pVMTunnel.InstallLogicFramework(shLt.FSendTunnel);
          FOwner.p2pVMTunnel.InstallLogicFramework(shLt.FRecvTunnel);
        end;
    end;
  FXNAT_VS.WaitAsyncConnecting := False;
  FXNAT_VS.Do_Open_Done(cState);
end;

constructor TPhysicsEngine_Special.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  FXNAT_VS := nil;
end;

destructor TPhysicsEngine_Special.Destroy;
begin
  inherited Destroy;
end;

procedure TXNATService.IPV6Listen(Sender: TPeerIO; InData, OutData: TDFE);
var
  i: Integer;
  shLt: TXServiceListen;
begin
  for i := 0 to FShareListenList.Count - 1 do
    begin
      shLt := FShareListenList[i];
      OutData.WriteString(shLt.FMapping);

      OutData.WriteString(shLt.FListenAddr);
      OutData.WriteString(shLt.FListenPort);

      OutData.WriteString(IPv6ToStr(shLt.FRecvTunnel_IPV6));
      OutData.WriteWORD(shLt.FRecvTunnel_Port);

      OutData.WriteString(IPv6ToStr(shLt.FSendTunnel_IPV6));
      OutData.WriteWORD(shLt.FSendTunnel_Port);
    end;
end;

procedure TXNATService.PeerIO_Create(const Sender: TPeerIO);
begin
  if FPhysicsEngine is TZNet_Server then
    begin
    end
  else if FPhysicsEngine is TZNet_Client then
    begin
      TPhysicsEngine_Special(Sender.UserSpecial).FXNAT_VS := Self;
    end;
end;

procedure TXNATService.PeerIO_Destroy(const Sender: TPeerIO);
begin
end;

procedure TXNATService.p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
begin
  {
    QuantumCryptographyPassword: used sha-3 shake256 cryptography as 512 bits password

    SHA-3 (Secure Hash Algorithm 3) is the latest member of the Secure Hash Algorithm family of standards,
    released by NIST on August 5, 2015.[4][5] Although part of the same series of standards,
    SHA-3 is internally quite different from the MD5-like structure of SHA-1 and SHA-2.

    Keccak is based on a novel approach called sponge construction.
    Sponge construction is based on a wide random function or random permutation, and allows inputting ("absorbing" in sponge terminology) any amount of data,
    and outputting ("squeezing") any amount of data,
    while acting as a pseudorandom function with regard to all previous inputs. This leads to great flexibility.

    NIST does not currently plan to withdraw SHA-2 or remove it from the revised Secure Hash Standard.
    The purpose of SHA-3 is that it can be directly substituted for SHA-2 in current applications if necessary,
    and to significantly improve the robustness of NIST's overall hash algorithm toolkit

    ref wiki
    https://en.wikipedia.org/wiki/SHA-3
  }

  if FPhysicsEngine is TZNet_Server then
    begin
    end
  else if FPhysicsEngine is TZNet_Client then
    begin
    end;

  Accept := CompareQuantumCryptographyPassword(AuthToken, Token);
  if Accept then
      Sender.Print('p2pVM auth Successed!')
  else
      Sender.Print('p2pVM auth failed!');
end;

procedure TXNATService.p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
var
  i: Integer;
  shLt: TXServiceListen;
begin
  if FPhysicsEngine is TZNet_Server then
    begin
      for i := FShareListenList.Count - 1 downto 0 do
        begin
          shLt := FShareListenList[i];
          Sender.p2pVM.MaxVMFragmentSize := umlStrToInt(MaxVMFragment, Sender.p2pVM.MaxVMFragmentSize);
          Sender.p2pVM.InstallLogicFramework(shLt.FRecvTunnel);
          Sender.p2pVM.InstallLogicFramework(shLt.FSendTunnel);
        end;
    end
  else if FPhysicsEngine is TZNet_Client then
    begin
    end;
  Sender.Print('XTunnel Open Before on %s', [Sender.PeerIP]);
end;

procedure TXNATService.p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
begin
  if FPhysicsEngine is TZNet_Server then
    begin
    end
  else if FPhysicsEngine is TZNet_Client then
    begin
    end;
  Sender.Print('XTunnel Open on %s', [Sender.PeerIP]);
end;

procedure TXNATService.p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
begin
  if FPhysicsEngine is TZNet_Server then
    begin
    end
  else if FPhysicsEngine is TZNet_Client then
    begin
    end;
  Sender.Print('XTunnel Open After on %s', [Sender.PeerIP]);
end;

procedure TXNATService.p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
var
  i: Integer;
  shLt: TXServiceListen;
begin
  if FPhysicsEngine is TZNet_Server then
    begin
      for i := FShareListenList.Count - 1 downto 0 do
        begin
          shLt := FShareListenList[i];
          Sender.p2pVM.UnInstallLogicFramework(shLt.FRecvTunnel);
          Sender.p2pVM.UnInstallLogicFramework(shLt.FSendTunnel);
        end;
    end
  else if FPhysicsEngine is TZNet_Client then
    begin
    end;
  Sender.Print('XTunnel Close on %s', [Sender.PeerIP]);
end;

procedure TXNATService.PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
begin
  if FPhysicsEngine is TZNet_Server then
    begin
    end
  else if FPhysicsEngine is TZNet_Client then
    begin
      if cState then
        begin
          if TZNet_Client(FPhysicsEngine).ClientIO <> nil then
              TPhysicsEngine_Special(TZNet_Client(FPhysicsEngine).ClientIO.UserSpecial).PhysicsConnect_Result_BuildP2PToken(cState);
        end
      else
        begin
          Do_Open_Done(False);
        end;
    end;
end;

procedure TXNATService.Do_Open_Done(State: Boolean);
begin
  if Assigned(On_Open_Tunnel_Done) then
    begin
      try
          On_Open_Tunnel_Done(Self, State);
      except
      end;
      On_Open_Tunnel_Done := nil;
    end;
  Open_Done := True;
end;

procedure TXNATService.Set_Quiet(const Value: Boolean);
var
  i: Integer;
  shLt: TXServiceListen;
begin
  FQuiet := Value;
  if FPhysicsEngine <> nil then
      Set_Instance_QuietMode(FPhysicsEngine, FQuiet);
  for i := FShareListenList.Count - 1 downto 0 do
    begin
      shLt := FShareListenList[i];
      if shLt.FRecvTunnel <> nil then
          Set_Instance_QuietMode(shLt.FRecvTunnel, FQuiet);
      if shLt.FSendTunnel <> nil then
          Set_Instance_QuietMode(shLt.FSendTunnel, FQuiet);
      if shLt.FProtocol <> nil then
          Set_Instance_QuietMode(shLt.FProtocol, FQuiet);
    end;
end;

constructor TXNATService.Create;
begin
  inherited Create;
  FShareListenList := TXServiceMappingList.Create;

  FPhysicsEngine := nil;
  FQuiet := False;
  FActivted := False;
  WaitAsyncConnecting := False;

  { parameter }
  Host := '';
  Port := '4921';
  AuthToken := 'ZServer';
  MaxVMFragment := '8192';
  ProtocolCompressed := False;
  Instance_Class := TXServiceListen;
  On_Open_Tunnel_Done := nil;
  Open_Done := False;
end;

destructor TXNATService.Destroy;
var
  i: Integer;
begin
  for i := 0 to FShareListenList.Count - 1 do
      DisposeObject(FShareListenList[i]);
  DisposeObjectAndNil(FShareListenList);

  if FPhysicsEngine <> nil then
    begin
      if FPhysicsEngine is TZNet_Server then
        begin
          TZNet_Server(FPhysicsEngine).StopService;
        end
      else if FPhysicsEngine is TZNet_Client then
        begin
          TZNet_Client(FPhysicsEngine).Disconnect;
        end;
      DisposeObjectAndNil(FPhysicsEngine);
    end;

  inherited Destroy;
end;

procedure TXNATService.Reset;
var
  i: Integer;
begin
  FActivted := False;
  WaitAsyncConnecting := False;

  for i := 0 to FShareListenList.Count - 1 do
      DisposeObject(FShareListenList[i]);
  FShareListenList.Clear;

  if FPhysicsEngine <> nil then
    begin
      if FPhysicsEngine is TZNet_Server then
        begin
          TZNet_Server(FPhysicsEngine).StopService;
        end
      else if FPhysicsEngine is TZNet_Client then
        begin
          TZNet_Client(FPhysicsEngine).Disconnect;
        end;
      DisposeObjectAndNil(FPhysicsEngine);
    end;

  On_Open_Tunnel_Done := nil;
  Open_Done := False;
end;

function TXNATService.AddMapping(const ListenAddr, ListenPort, FMapping: TPascalString; TimeOut: TTimeTick): TXServiceListen;
var
  i: Integer;
  shLt: TXServiceListen;
begin
  for i := 0 to FShareListenList.Count - 1 do
    begin
      shLt := FShareListenList[i];
      if ListenAddr.Same(@shLt.FListenAddr) and ListenPort.Same(@shLt.FListenPort) then
          exit(shLt);
    end;

  shLt := Instance_Class.Create(Self);
  shLt.FListenAddr := ListenAddr;
  shLt.FListenPort := ListenPort;
  shLt.FMapping := FMapping;
  shLt.DistributedWorkload := True;
  shLt.XServerTunnel := Self;
  shLt.TimeOut := TimeOut;

  if shLt.FRecvTunnel <> nil then
      Set_Instance_QuietMode(shLt.FRecvTunnel, FQuiet);
  if shLt.FSendTunnel <> nil then
      Set_Instance_QuietMode(shLt.FSendTunnel, FQuiet);
  if shLt.FProtocol <> nil then
      Set_Instance_QuietMode(shLt.FProtocol, FQuiet);

  FShareListenList.Add(shLt);

  if FActivted and (FPhysicsEngine is TZNet_Server) then
      shLt.Open;
  Result := shLt;
end;

function TXNATService.AddNoDistributedMapping(const ListenAddr, ListenPort, FMapping: TPascalString; TimeOut: TTimeTick): TXServiceListen;
var
  i: Integer;
  shLt: TXServiceListen;
begin
  for i := 0 to FShareListenList.Count - 1 do
    begin
      shLt := FShareListenList[i];
      if ListenAddr.Same(@shLt.FListenAddr) and ListenPort.Same(@shLt.FListenPort) then
          exit(shLt);
    end;

  shLt := Instance_Class.Create(Self);
  shLt.FListenAddr := ListenAddr;
  shLt.FListenPort := ListenPort;
  shLt.FMapping := FMapping;
  shLt.DistributedWorkload := False;
  shLt.XServerTunnel := Self;
  shLt.TimeOut := TimeOut;

  if shLt.FRecvTunnel <> nil then
      Set_Instance_QuietMode(shLt.FRecvTunnel, FQuiet);
  if shLt.FSendTunnel <> nil then
      Set_Instance_QuietMode(shLt.FSendTunnel, FQuiet);
  if shLt.FProtocol <> nil then
      Set_Instance_QuietMode(shLt.FProtocol, FQuiet);

  FShareListenList.Add(shLt);

  if FActivted and (FPhysicsEngine is TZNet_Server) then
      shLt.Open;
  Result := shLt;
end;

procedure TXNATService.OpenTunnel(MODEL: TXNAT_PHYSICS_MODEL);
var
  i: Integer;
  shLt: TXServiceListen;
  listening_: Boolean;
begin
  FActivted := True;
  Open_Done := False;

  { init tunnel engine }
  if FPhysicsEngine = nil then
    begin
      if MODEL = TXNAT_PHYSICS_MODEL.XNAT_PHYSICS_SERVICE then
          FPhysicsEngine := TXPhysicsServer.Create
      else
          FPhysicsEngine := TXPhysicsClient.Create;
    end;

  FPhysicsEngine.UserSpecialClass := TPhysicsEngine_Special;
  FPhysicsEngine.IOInterface := Self;
  FPhysicsEngine.VMInterface := Self;

  Set_Instance_QuietMode(FPhysicsEngine, FQuiet);

  { Security protocol }
  FPhysicsEngine.SwitchMaxPerformance;

  { regsiter protocol }
  if not FPhysicsEngine.ExistsRegistedCmd(C_IPV6Listen) then
      FPhysicsEngine.RegisterStream(C_IPV6Listen).OnExecute := IPV6Listen;

  if FPhysicsEngine is TZNet_Server then
    begin
      { service }
      listening_ := TZNet_Server(FPhysicsEngine).StartService(Host, umlStrToInt(Port));
      if listening_ then
          FPhysicsEngine.Print('Tunnel Open %s:%s successed', [TranslateBindAddr(Host), Port.Text])
      else
          FPhysicsEngine.Print('error: Tunnel is Closed for %s:%s', [TranslateBindAddr(Host), Port.Text]);

      { open share listen }
      for i := 0 to FShareListenList.Count - 1 do
        begin
          shLt := FShareListenList[i];
          shLt.Open;
          if shLt.FRecvTunnel <> nil then
              Set_Instance_QuietMode(shLt.FRecvTunnel, FQuiet);
          if shLt.FSendTunnel <> nil then
              Set_Instance_QuietMode(shLt.FSendTunnel, FQuiet);
          if shLt.FProtocol <> nil then
              Set_Instance_QuietMode(shLt.FProtocol, FQuiet);
        end;
      Do_Open_Done(listening_);
    end
  else if FPhysicsEngine is TZNet_Client then
    begin
      { reverse connection }
      if not TZNet_Client(FPhysicsEngine).Connected then
        begin
          WaitAsyncConnecting := True;
          WaitAsyncConnecting_BeginTime := GetTimeTick;
          TZNet_Client(FPhysicsEngine).AsyncConnectM(Host, umlStrToInt(Port), PhysicsConnect_Result_BuildP2PToken);
        end;
    end;
end;

procedure TXNATService.OpenTunnel;
begin
  OpenTunnel(TXNAT_PHYSICS_MODEL.XNAT_PHYSICS_SERVICE);
end;

procedure TXNATService.Progress;
var
  i: Integer;
  shLt: TXServiceListen;
begin
  if (FPhysicsEngine <> nil) then
    begin
      if (FPhysicsEngine is TZNet_Client) then
        begin
          if WaitAsyncConnecting and (GetTimeTick - WaitAsyncConnecting_BeginTime > 15000) then
              WaitAsyncConnecting := False;

          if FActivted and (not TZNet_Client(FPhysicsEngine).Connected) then
            begin
              if not WaitAsyncConnecting then
                begin
                  OpenTunnel(TXNAT_PHYSICS_MODEL.XNAT_PHYSICS_CLIENT);
                end;
            end;
        end;
      FPhysicsEngine.Progress;
    end;

  for i := FShareListenList.Count - 1 downto 0 do
    begin
      shLt := FShareListenList[i];
      if (shLt.FRecvTunnel <> nil) and (shLt.FSendTunnel <> nil) then
        begin
          if (shLt.FRecvTunnel.Count = 0) and (shLt.FSendTunnel.Count = 0) and (shLt.Activted) then
              shLt.Activted := False;

          shLt.FRecvTunnel.Progress;
          shLt.FSendTunnel.Progress;
          shLt.FProtocol.Progress;
        end;
    end;
end;

end.
 
