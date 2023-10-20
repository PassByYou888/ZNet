{ ****************************************************************************** }
{ * XNAT tunnel                                                                * }
{ ****************************************************************************** }
unit Z.Net.XNAT.Client;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine, Z.TextDataEngine,
  Z.Cipher, Z.DFE, Z.MemoryStream, Z.Net, Z.Notify,
  Z.HashList.Templet, Z.Net.XNAT.Physics;

type
  TXNATClient = class;
  TXClientCustomProtocol = class;
  TXClientCustomProtocol_List = TGenericsList<TXClientCustomProtocol>;

  TXClientMapping = class(TCore_Object)
  private
    Owner: TXNATClient;
    FAddr: TPascalString;
    FPort: TPascalString;
    FMapping: TPascalString;

    ProtocolPool: TXClientCustomProtocol_List;
    LastProtocolID: Cardinal;
    ProtocolHash: TUInt32HashObjectList;

    RecvTunnel: TZNet_WithP2PVM_Client;
    RecvTunnel_IPV6: TPascalString;
    RecvTunnel_Port: Word;

    SendTunnel: TZNet_WithP2PVM_Client;
    SendTunnel_IPV6: TPascalString;
    SendTunnel_Port: Word;

    MaxWorkload: Cardinal;
    LastUpdateWorkload: Cardinal;
    LastUpdateTime: TTimeTick;

    Remote_ListenAddr, Remote_ListenPort: TPascalString;

    XClientTunnel: TXNATClient;

    { complete buffer metric }
    Complete_Buffer_Sum: Int64;

    procedure Init;
    procedure SendTunnel_ConnectResult(const cState: Boolean);
    procedure RecvTunnel_ConnectResult(const cState: Boolean);

    procedure RequestListen_Result(Sender: TPeerIO; Result_: TDFE);
    procedure delay_RequestListen(Sender: TN_Post_Execute);

    procedure Open;

    procedure cmd_connect_request(Sender: TPeerIO; InData: TDFE);
    procedure cmd_disconnect_request(Sender: TPeerIO; InData: TDFE);
    procedure cmd_data(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
  public
    property Addr: TPascalString read FAddr;
    property Port: TPascalString read FPort;
    property Mapping: TPascalString read FMapping;
    constructor Create(Owner_: TXNATClient);
    destructor Destroy; override;
    procedure UpdateWorkload(force: Boolean);
  end;

  TXClientCustomProtocol = class(TXPhysicsClient)
  protected
    procedure DoConnected(Sender: TPeerIO); override;
    procedure DoDisconnect(Sender: TPeerIO); override;
  public
    LocalProtocol_ID, RemoteProtocol_ID: Cardinal;
    Remote_IP: SystemString;
    Mapping: TXClientMapping;
    Activted: Boolean;
    RequestBuffer: TMS64;

    procedure OnReceiveBuffer(const buffer: PByte; const Size: NativeInt; var FillDone: Boolean); override;
    procedure OnConnect_Result(const cState: Boolean);
    constructor Create; override;
    destructor Destroy; override;
  end;

  TPhysicsEngine_Special = class(TPeer_IO_User_Special)
  protected
    XNAT: TXNATClient;
    procedure PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
    procedure PhysicsVMBuildAuthToken_Result;
    procedure PhysicsOpenVM_Result(const cState: Boolean);
    procedure IPV6Listen_Result(Sender: TPeerIO; Result_: TDFE);
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TXClientMappingList = TGenericsList<TXClientMapping>;
  TXClientHashMapping = TGeneric_String_Object_Hash<TXClientMapping>;
  TOn_XNATClient_Open_Tunnel_Done = procedure(Sender: TXNATClient; State: Boolean) of object;

  TXNATClient = class(TCore_InterfacedObject, IIOInterface, IZNet_VMInterface)
  private
    FMappingList: TXClientMappingList;
    FHashMapping: TXClientHashMapping;
    Activted: Boolean;
    WaitAsyncConnecting: Boolean;
    WaitAsyncConnecting_BeginTime: TTimeTick;
    FPhysicsEngine: TZNet;
    FQuiet: Boolean;

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
    On_Open_Tunnel_Done: TOn_XNATClient_Open_Tunnel_Done;
  public
    property MappingList: TXClientMappingList read FMappingList;
    property HashMapping: TXClientHashMapping read FHashMapping;
    property PhysicsEngine: TZNet read FPhysicsEngine;
    property Quiet: Boolean read FQuiet write Set_Quiet;
    constructor Create;
    destructor Destroy; override;
    procedure AddMapping(const MAddr, MPort, Mapping: TPascalString; MaxWorkload: Cardinal);
    procedure OpenTunnel(MODEL: TXNAT_PHYSICS_MODEL); overload;
    procedure OpenTunnel; overload;
    procedure Progress;
  end;

implementation

uses Z.Net.C4;

procedure TXClientMapping.Init;
begin
  FAddr := '';
  FPort := '';
  FMapping := '';
  ProtocolPool := nil;
  LastProtocolID := 1;
  ProtocolHash := nil;
  RecvTunnel := nil;
  RecvTunnel_IPV6 := '';
  RecvTunnel_Port := 0;
  SendTunnel := nil;
  SendTunnel_IPV6 := '';
  SendTunnel_Port := 0;
  MaxWorkload := 100;
  LastUpdateWorkload := 0;
  LastUpdateTime := GetTimeTick();
  Remote_ListenAddr := '';
  Remote_ListenPort := '0';
  XClientTunnel := nil;
  Complete_Buffer_Sum := 0;
end;

procedure TXClientMapping.SendTunnel_ConnectResult(const cState: Boolean);
begin
  if cState then
    begin
      SendTunnel.Print('[%s] Send Tunnel connect success.', [FMapping.Text]);
      if not RecvTunnel.Connected then
          RecvTunnel.AsyncConnectM(RecvTunnel_IPV6, RecvTunnel_Port, RecvTunnel_ConnectResult)
      else
          RecvTunnel_ConnectResult(True);
    end
  else
      SendTunnel.Print('error: [%s] Send Tunnel connect failed!', [FMapping.Text]);
end;

procedure TXClientMapping.RecvTunnel_ConnectResult(const cState: Boolean);
begin
  if cState then
    begin
      RecvTunnel.Print('[%s] Receive Tunnel connect success.', [FMapping.Text]);
      SendTunnel.ProgressPost.PostExecuteM(0, delay_RequestListen);
    end
  else
      RecvTunnel.Print('error: [%s] Receive Tunnel connect failed!', [FMapping.Text]);
end;

procedure TXClientMapping.RequestListen_Result(Sender: TPeerIO; Result_: TDFE);
begin
  if Result_.Reader.ReadBool then
    begin
      SendTunnel.Print('success: remote host:%s port:%s mapping to host:%s port:%s', [XClientTunnel.Host.Text, Remote_ListenPort.Text, FAddr.Text, FPort.Text]);
      UpdateWorkload(True);
    end
  else
      SendTunnel.Print('failed: remote host:%s port:%s listen error!', [XClientTunnel.Host.Text, Remote_ListenPort.Text]);
end;

procedure TXClientMapping.delay_RequestListen(Sender: TN_Post_Execute);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteCardinal(SendTunnel.RemoteID);
  de.WriteCardinal(RecvTunnel.RemoteID);
  SendTunnel.SendStreamCmdM(C_RequestListen, de, RequestListen_Result);
  disposeObject(de);
end;

procedure TXClientMapping.Open;
var
  io_array: TIO_Array;
  p_id: Cardinal;
  p_io: TPeerIO;
begin
  if ProtocolPool = nil then
      ProtocolPool := TXClientCustomProtocol_List.Create;

  if ProtocolHash = nil then
      ProtocolHash := TUInt32HashObjectList.CustomCreate(8192);

  if RecvTunnel = nil then
    begin
      RecvTunnel := TZNet_WithP2PVM_Client.Create;
      RecvTunnel.QuietMode := Owner.Quiet;
      RecvTunnel.CompleteBufferSwapSpace := True;
    end;
  if SendTunnel = nil then
    begin
      SendTunnel := TZNet_WithP2PVM_Client.Create;
      SendTunnel.QuietMode := Owner.Quiet;
      SendTunnel.CompleteBufferSwapSpace := True;
    end;

  XClientTunnel.FPhysicsEngine.GetIO_Array(io_array);
  for p_id in io_array do
    begin
      p_io := TPeerIO(XClientTunnel.FPhysicsEngine.PeerIO_HashPool[p_id]);
      if p_io <> nil then
        begin
          { uninstall p2pVM }
          p_io.p2pVMTunnel.UninstallLogicFramework(SendTunnel);
          p_io.p2pVMTunnel.UninstallLogicFramework(RecvTunnel);

          { install p2pVM }
          p_io.p2pVMTunnel.InstallLogicFramework(SendTunnel);
          p_io.p2pVMTunnel.InstallLogicFramework(RecvTunnel);
        end;
    end;
  SetLength(io_array, 0);

  { sequence sync }
  RecvTunnel.SyncOnCompleteBuffer := True;
  RecvTunnel.SyncOnResult := True;
  RecvTunnel.SwitchMaxPerformance;

  SendTunnel.SyncOnCompleteBuffer := True;
  SendTunnel.SyncOnResult := True;
  SendTunnel.SwitchMaxPerformance;

  { compressed complete buffer }
  SendTunnel.CompleteBufferCompressed := XClientTunnel.ProtocolCompressed;
  RecvTunnel.CompleteBufferCompressed := XClientTunnel.ProtocolCompressed;

  if not RecvTunnel.ExistsRegistedCmd(C_Connect_request) then
      RecvTunnel.RegisterDirectStream(C_Connect_request).OnExecute := cmd_connect_request;

  if not RecvTunnel.ExistsRegistedCmd(C_Disconnect_request) then
      RecvTunnel.RegisterDirectStream(C_Disconnect_request).OnExecute := cmd_disconnect_request;

  if not RecvTunnel.ExistsRegistedCmd(C_Data) then
      RecvTunnel.RegisterCompleteBuffer(C_Data).OnExecute := cmd_data;

  if not SendTunnel.Connected then
      SendTunnel.AsyncConnectM(SendTunnel_IPV6, SendTunnel_Port, SendTunnel_ConnectResult)
  else
      SendTunnel_ConnectResult(True);

  SendTunnel.PrintParams[C_Connect_reponse] := False;
  SendTunnel.PrintParams[C_Disconnect_reponse] := False;
  SendTunnel.PrintParams[C_Data] := False;
  SendTunnel.PrintParams[C_Workload] := False;

  RecvTunnel.PrintParams[C_Connect_request] := False;
  RecvTunnel.PrintParams[C_Disconnect_request] := False;
  RecvTunnel.PrintParams[C_Data] := False;
end;

procedure TXClientMapping.cmd_connect_request(Sender: TPeerIO; InData: TDFE);
var
  Remote_id: Cardinal;
  Remote_IP: SystemString;
  xCli: TXClientCustomProtocol;
begin
  Remote_id := InData.Reader.ReadCardinal;
  Remote_IP := InData.Reader.ReadString;

  xCli := TXClientCustomProtocol.Create;
  xCli.QuietMode := Owner.Quiet;
  xCli.Protocol := cpCustom;
  while ProtocolHash.Exists(LastProtocolID) do
      inc(LastProtocolID);
  xCli.LocalProtocol_ID := LastProtocolID;
  xCli.Remote_IP := Remote_IP;
  inc(LastProtocolID);
  ProtocolPool.Add(xCli);
  ProtocolHash.Add(xCli.LocalProtocol_ID, xCli, False);

  xCli.RemoteProtocol_ID := Remote_id;
  xCli.Mapping := Self;
  xCli.Activted := False;

  { async connection }
  xCli.AsyncConnectM(FAddr, umlStrToInt(FPort), xCli.OnConnect_Result);
end;

procedure TXClientMapping.cmd_disconnect_request(Sender: TPeerIO; InData: TDFE);
var
  local_id, Remote_id: Cardinal;
  phy_io: TXClientCustomProtocol;
begin
  Remote_id := InData.Reader.ReadCardinal;
  local_id := InData.Reader.ReadCardinal;

  phy_io := TXClientCustomProtocol(ProtocolHash[local_id]);
  if phy_io <> nil then
      phy_io.Disconnect;
end;

procedure TXClientMapping.cmd_data(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  local_id, Remote_id: Cardinal;
  destSiz: NativeInt;
  destBuff: PByte;
  phy_io: TXClientCustomProtocol;
begin
  Extract_XNAT_Buff(InData, DataSize, Remote_id, local_id, destSiz, destBuff);

  phy_io := TXClientCustomProtocol(ProtocolHash[local_id]);
  if phy_io <> nil then
    begin
      phy_io.BeginWriteBuffer;
      phy_io.WriteBuffer(destBuff, destSiz);
      phy_io.EndWriteBuffer;
    end;
end;

constructor TXClientMapping.Create(Owner_: TXNATClient);
begin
  inherited Create;
  Owner := Owner_;
  Init;
end;

destructor TXClientMapping.Destroy;
var
  j: integer;
begin
  if ProtocolPool <> nil then
    begin
      for j := 0 to ProtocolPool.Count - 1 do
        begin
          ProtocolPool[j].Disconnect;
          disposeObject(ProtocolPool[j]);
        end;
      disposeObject(ProtocolPool);
    end;

  if ProtocolHash <> nil then
      disposeObject(ProtocolHash);

  if SendTunnel <> nil then
    begin
      SendTunnel.Disconnect;
      disposeObject(SendTunnel);
    end;

  if RecvTunnel <> nil then
    begin
      RecvTunnel.Disconnect;
      disposeObject(RecvTunnel);
    end;

  inherited Destroy;
end;

procedure TXClientMapping.UpdateWorkload(force: Boolean);
var
  de: TDFE;
begin
  if SendTunnel = nil then
      exit;
  if RecvTunnel = nil then
      exit;
  if not SendTunnel.Connected then
      exit;
  if (not force) then
    if (ProtocolHash.Count = LastUpdateWorkload) or (GetTimeTick() - LastUpdateTime < 1000) then
        exit;

  de := TDFE.Create;
  de.WriteCardinal(MaxWorkload);
  de.WriteCardinal(ProtocolHash.Count);
  SendTunnel.SendDirectStreamCmd(C_Workload, de);
  disposeObject(de);

  LastUpdateWorkload := ProtocolHash.Count;
  LastUpdateTime := GetTimeTick();
end;

procedure TXClientCustomProtocol.DoConnected(Sender: TPeerIO);
begin
  inherited DoConnected(Sender);
end;

procedure TXClientCustomProtocol.DoDisconnect(Sender: TPeerIO);
var
  de: TDFE;
begin
  if Activted then
    begin
      de := TDFE.Create;
      de.WriteCardinal(LocalProtocol_ID);
      de.WriteCardinal(RemoteProtocol_ID);
      Mapping.SendTunnel.SendDirectStreamCmd(C_Disconnect_reponse, de);
      disposeObject(de);
    end;
  inherited DoDisconnect(Sender);
end;

procedure TXClientCustomProtocol.OnReceiveBuffer(const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
var
  nSiz: NativeInt;
  nBuff: PByte;
begin
  if Activted then
    begin
      Build_XNAT_Buff(buffer, Size, LocalProtocol_ID, RemoteProtocol_ID, nSiz, nBuff);
      Mapping.SendTunnel.SendCompleteBuffer(C_Data, nBuff, nSiz, True);
      inc(Mapping.Complete_Buffer_Sum, nSiz);
      if Mapping.Complete_Buffer_Sum > 10 * 1024 * 1024 then
        begin
          Mapping.SendTunnel.Send_NULL;
          Mapping.Complete_Buffer_Sum := 0;
        end;
    end
  else
    begin
      RequestBuffer.WritePtr(buffer, Size);
    end;
end;

procedure TXClientCustomProtocol.OnConnect_Result(const cState: Boolean);
var
  de: TDFE;
  nSiz: NativeInt;
  nBuff: PByte;
begin
  de := TDFE.Create;
  de.WriteBool(cState);
  de.WriteCardinal(LocalProtocol_ID);
  de.WriteCardinal(RemoteProtocol_ID);
  Mapping.SendTunnel.SendDirectStreamCmd(C_Connect_reponse, de);
  disposeObject(de);

  if cState then
    begin
      Activted := True;
      if RequestBuffer.Size > 0 then
        begin
          Build_XNAT_Buff(RequestBuffer.Memory, RequestBuffer.Size, LocalProtocol_ID, RemoteProtocol_ID, nSiz, nBuff);
          Mapping.SendTunnel.SendCompleteBuffer(C_Data, nBuff, nSiz, True);
          RequestBuffer.Clear;
        end;
    end
  else
    begin
      de := TDFE.Create;
      de.WriteCardinal(LocalProtocol_ID);
      de.WriteCardinal(RemoteProtocol_ID);
      Mapping.SendTunnel.SendDirectStreamCmd(C_Disconnect_reponse, de);
      disposeObject(de);

      disposeObject(Self);
    end;
end;

constructor TXClientCustomProtocol.Create;
begin
  inherited Create;
  LocalProtocol_ID := 0;
  RemoteProtocol_ID := 0;
  Remote_IP := '';
  Mapping := nil;
  Activted := False;
  RequestBuffer := TMS64.Create;
end;

destructor TXClientCustomProtocol.Destroy;
var
  i: integer;
begin
  Mapping.ProtocolHash.Delete(LocalProtocol_ID);
  i := 0;
  while i < Mapping.ProtocolPool.Count do
    begin
      if Mapping.ProtocolPool[i] = Self then
          Mapping.ProtocolPool.Delete(i)
      else
          inc(i);
    end;
  disposeObject(RequestBuffer);
  inherited Destroy;
end;

procedure TPhysicsEngine_Special.PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
begin
  if cState then
      Owner.BuildP2PAuthTokenM(PhysicsVMBuildAuthToken_Result)
  else
    begin
      XNAT.WaitAsyncConnecting := False;
      XNAT.Do_Open_Done(False);
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
  Owner.OpenP2pVMTunnelM(True, GenerateQuantumCryptographyPassword(XNAT.AuthToken), PhysicsOpenVM_Result)
end;

procedure TPhysicsEngine_Special.PhysicsOpenVM_Result(const cState: Boolean);
begin
  if cState then
    begin
      Owner.p2pVMTunnel.MaxVMFragmentSize := umlStrToInt(XNAT.MaxVMFragment, Owner.p2pVMTunnel.MaxVMFragmentSize);
      Owner.SendStreamCmdM(C_IPV6Listen, nil, IPV6Listen_Result);
    end
  else
    begin
      XNAT.WaitAsyncConnecting := False;
      XNAT.Do_Open_Done(False);
    end;
end;

procedure TPhysicsEngine_Special.IPV6Listen_Result(Sender: TPeerIO; Result_: TDFE);
var
  Mapping: TPascalString;
  Remote_ListenAddr, Remote_ListenPort: TPascalString;
  RecvTunnel_IPV6: TPascalString;
  RecvTunnel_Port: Word;
  SendTunnel_IPV6: TPascalString;
  SendTunnel_Port: Word;
  tunMp: TXClientMapping;
begin
  while Result_.Reader.NotEnd do
    begin
      Mapping := Result_.Reader.ReadString;
      Remote_ListenAddr := Result_.Reader.ReadString;
      Remote_ListenPort := Result_.Reader.ReadString;
      SendTunnel_IPV6 := Result_.Reader.ReadString;
      SendTunnel_Port := Result_.Reader.ReadWord;
      RecvTunnel_IPV6 := Result_.Reader.ReadString;
      RecvTunnel_Port := Result_.Reader.ReadWord;
      tunMp := XNAT.FHashMapping[Mapping];
      if tunMp <> nil then
        begin
          tunMp.RecvTunnel_IPV6 := RecvTunnel_IPV6;
          tunMp.RecvTunnel_Port := RecvTunnel_Port;
          tunMp.SendTunnel_IPV6 := SendTunnel_IPV6;
          tunMp.SendTunnel_Port := SendTunnel_Port;
          tunMp.Remote_ListenAddr := Remote_ListenAddr;
          tunMp.Remote_ListenPort := Remote_ListenPort;
          tunMp.Open;
        end;
    end;
  XNAT.Activted := True;
  XNAT.WaitAsyncConnecting := False;
  XNAT.Do_Open_Done(True);
end;

constructor TPhysicsEngine_Special.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  XNAT := nil;
end;

destructor TPhysicsEngine_Special.Destroy;
begin
  inherited Destroy;
end;

procedure TXNATClient.PeerIO_Create(const Sender: TPeerIO);
begin
  TPhysicsEngine_Special(Sender.UserSpecial).XNAT := Self;
end;

procedure TXNATClient.PeerIO_Destroy(const Sender: TPeerIO);
begin
end;

procedure TXNATClient.p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
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

procedure TXNATClient.p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
begin
  if FPhysicsEngine is TZNet_Server then
    begin
    end
  else if FPhysicsEngine is TZNet_Client then
    begin
    end;
  Sender.Print('XTunnel Open Before on %s', [Sender.PeerIP]);
end;

procedure TXNATClient.p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
begin
  if FPhysicsEngine is TZNet_Server then
    begin
    end
  else if FPhysicsEngine is TZNet_Client then
    begin
    end;
  Sender.Print('XTunnel Open on %s', [Sender.PeerIP]);
end;

procedure TXNATClient.p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
begin
  if FPhysicsEngine is TZNet_Server then
    begin
      Sender.SendStreamCmdM(C_IPV6Listen, nil, TPhysicsEngine_Special(Sender.UserSpecial).IPV6Listen_Result);
    end
  else if FPhysicsEngine is TZNet_Client then
    begin
    end;
  Sender.Print('XTunnel Open After on %s', [Sender.PeerIP]);
end;

procedure TXNATClient.p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
begin
  if FPhysicsEngine is TZNet_Server then
    begin
    end
  else if FPhysicsEngine is TZNet_Client then
    begin
    end;
  Sender.Print('XTunnel Close on %s', [Sender.PeerIP]);
end;

procedure TXNATClient.PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
begin
  if cState then
    begin
      TPhysicsEngine_Special(TZNet_Client(FPhysicsEngine).ClientIO.UserSpecial).PhysicsConnect_Result_BuildP2PToken(cState);
    end
  else
    begin
      WaitAsyncConnecting := False;
      Do_Open_Done(False);
    end;
end;

procedure TXNATClient.Do_Open_Done(State: Boolean);
begin
  if Assigned(On_Open_Tunnel_Done) then
    begin
      try
          On_Open_Tunnel_Done(Self, State);
      except
      end;
      On_Open_Tunnel_Done := nil;
    end;
end;

procedure TXNATClient.Set_Quiet(const Value: Boolean);
var
  i, j: integer;
  tunMp: TXClientMapping;
  xCliProt: TXClientCustomProtocol;
begin
  FQuiet := Value;

  if FPhysicsEngine <> nil then
      C40Set_Instance_QuietMode(FPhysicsEngine, FQuiet);

  i := 0;
  while i < FMappingList.Count do
    begin
      tunMp := FMappingList[i] as TXClientMapping;
      if tunMp.RecvTunnel <> nil then
          C40Set_Instance_QuietMode(tunMp.RecvTunnel, FQuiet);

      if tunMp.SendTunnel <> nil then
          C40Set_Instance_QuietMode(tunMp.SendTunnel, FQuiet);

      if tunMp.ProtocolPool <> nil then
        begin
          j := 0;
          while j < tunMp.ProtocolPool.Count do
            begin
              xCliProt := tunMp.ProtocolPool[j];
              C40Set_Instance_QuietMode(xCliProt, FQuiet);
              inc(j);
            end;
        end;
      inc(i);
    end;
end;

constructor TXNATClient.Create;
begin
  inherited Create;
  Host := '';
  Port := '4921';
  AuthToken := '';
  MaxVMFragment := '8192';
  ProtocolCompressed := False;
  FMappingList := TXClientMappingList.Create;
  FHashMapping := TXClientHashMapping.Create(False, $FF, nil);
  Activted := False;
  WaitAsyncConnecting := False;
  FPhysicsEngine := nil;
  FQuiet := False;
  On_Open_Tunnel_Done := nil;
end;

destructor TXNATClient.Destroy;
var
  i: integer;
begin
  for i := FMappingList.Count - 1 downto 0 do
      disposeObject(FMappingList[i]);
  disposeObject(FMappingList);
  disposeObject(FHashMapping);

  if FPhysicsEngine <> nil then
    begin
      if FPhysicsEngine is TZNet_Client then
        begin
          TZNet_Client(FPhysicsEngine).Disconnect;
        end;
    end;

  disposeObject(FPhysicsEngine);
  inherited Destroy;
end;

procedure TXNATClient.AddMapping(const MAddr, MPort, Mapping: TPascalString; MaxWorkload: Cardinal);
var
  tunMp: TXClientMapping;
begin
  tunMp := TXClientMapping.Create(Self);
  tunMp.FAddr := MAddr;
  tunMp.FPort := MPort;
  tunMp.FMapping := Mapping;
  tunMp.MaxWorkload := MaxWorkload;
  tunMp.XClientTunnel := Self;

  if tunMp.RecvTunnel <> nil then
      C40Set_Instance_QuietMode(tunMp.RecvTunnel, FQuiet);
  if tunMp.SendTunnel <> nil then
      C40Set_Instance_QuietMode(tunMp.SendTunnel, FQuiet);

  FMappingList.Add(tunMp);
  FHashMapping.Add(tunMp.FMapping, tunMp);
end;

procedure TXNATClient.OpenTunnel(MODEL: TXNAT_PHYSICS_MODEL);
begin
  Activted := True;

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

  C40Set_Instance_QuietMode(FPhysicsEngine, FQuiet);

  { Security protocol }
  FPhysicsEngine.SwitchMaxPerformance;

  if FPhysicsEngine is TZNet_Server then
    begin
      if TZNet_Server(FPhysicsEngine).StartService(Host, umlStrToInt(Port)) then
        begin
          Do_Open_Done(True);
          FPhysicsEngine.Print('Tunnel Open %s:%s successed', [TranslateBindAddr(Host), Port.Text]);
        end
      else
        begin
          Do_Open_Done(False);
          FPhysicsEngine.Print('error: Tunnel is Closed for %s:%s', [TranslateBindAddr(Host), Port.Text]);
        end;
    end
  else if FPhysicsEngine is TZNet_Client then
    begin
      if not TZNet_Client(FPhysicsEngine).Connected then
        begin
          WaitAsyncConnecting := True;
          WaitAsyncConnecting_BeginTime := GetTimeTick;
          TZNet_Client(FPhysicsEngine).AsyncConnectM(Host, umlStrToInt(Port), PhysicsConnect_Result_BuildP2PToken);
        end;
    end;
end;

procedure TXNATClient.OpenTunnel;
begin
  OpenTunnel(TXNAT_PHYSICS_MODEL.XNAT_PHYSICS_CLIENT);
end;

procedure TXNATClient.Progress;
var
  i, j: integer;
  tunMp: TXClientMapping;
  xCliProt: TXClientCustomProtocol;
begin
  if (FPhysicsEngine <> nil) then
    begin
      if (FPhysicsEngine is TZNet_Client) then
        begin
          if WaitAsyncConnecting and (GetTimeTick - WaitAsyncConnecting_BeginTime > 15000) then
              WaitAsyncConnecting := False;

          if Activted and (not TZNet_Client(FPhysicsEngine).Connected) then
            begin
              if not WaitAsyncConnecting then
                begin
                  OpenTunnel(TXNAT_PHYSICS_MODEL.XNAT_PHYSICS_CLIENT);
                end;
            end;
        end;
      FPhysicsEngine.Progress;
    end;

  i := 0;
  while i < FMappingList.Count do
    begin
      tunMp := FMappingList[i] as TXClientMapping;

      tunMp.UpdateWorkload(False);

      if tunMp.RecvTunnel <> nil then
          tunMp.RecvTunnel.Progress;

      if tunMp.SendTunnel <> nil then
          tunMp.SendTunnel.Progress;

      if tunMp.ProtocolPool <> nil then
        begin
          j := 0;
          while j < tunMp.ProtocolPool.Count do
            begin
              xCliProt := tunMp.ProtocolPool[j];
              try
                if xCliProt.Activted and (not xCliProt.Connected) then
                  begin
                    disposeObject(xCliProt);
                    break;
                  end
                else
                  begin
                    xCliProt.Progress;
                    inc(j);
                  end;
              except
                  break;
              end;
            end;
        end;
      inc(i);
    end;
end;

end.
