{ ****************************************************************************** }
{ * XNAT virtual service                                                       * }
{ ****************************************************************************** }
unit Z.Net.XNAT.MappingOnVirutalService;

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
  TXNAT_VS_Mapping = class;
  TXNAT_MappingOnVirutalService = class;

  TXNAT_MappingOnVirutalService_IO = class(TPeerIO)
  protected
    Remote_ID: Cardinal;
    Remote_IP: SystemString;
    SendingStream: TMS64;
  public
    procedure CreateAfter; override;
    destructor Destroy; override;

    function OwnerVS: TXNAT_MappingOnVirutalService;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure Write_IO_Buffer(const buff: PByte; const Size: nativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;
    function WriteBuffer_is_NULL: Boolean; override;
    procedure Progress; override;
  end;

  TXNAT_MappingOnVirutalService = class(TZNet_Server)
  private
    Mapping: TPascalString;

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
    XNAT: TXNAT_VS_Mapping;

    procedure Init;
    procedure SendTunnel_ConnectResult(const cState: Boolean);
    procedure RecvTunnel_ConnectResult(const cState: Boolean);

    procedure RequestListen_Result(Sender: TPeerIO; Result_: TDFE);
    procedure delay_RequestListen(Sender: TN_Post_Execute);

    procedure Open;

    procedure cmd_connect_request(Sender: TPeerIO; InData: TDFE);
    procedure cmd_disconnect_request(Sender: TPeerIO; InData: TDFE);
    procedure cmd_data(Sender: TPeerIO; InData: PByte; DataSize: nativeInt);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure UpdateWorkload(force: Boolean); virtual;

    function StartService(Host: SystemString; Port: Word): Boolean; override;
    procedure StopService; override;
    procedure Progress; override;
    function WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; TimeOut_: TTimeTick): SystemString; override;
    procedure WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, Result_: TDFE; TimeOut_: TTimeTick); override;
  end;

  TPhysicsEngine_Special = class(TPeerIOUserSpecial)
  protected
    XNAT: TXNAT_VS_Mapping;
    procedure PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
    procedure PhysicsVMBuildAuthToken_Result;
    procedure PhysicsOpenVM_Result(const cState: Boolean);
    procedure IPV6Listen_Result(Sender: TPeerIO; Result_: TDFE);
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TXVirutalServiceMappingList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TXNAT_MappingOnVirutalService>;
  TXVirutalServiceHashMapping = {$IFDEF FPC}specialize {$ENDIF FPC} TGeneric_String_Object_Hash<TXNAT_MappingOnVirutalService>;
  TXNAT_VS_Mapping_List_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TXNAT_VS_Mapping>;

  TXNAT_VS_Mapping = class(TCore_InterfacedObject, IIOInterface, IZNet_VMInterface)
  private
    MappingList: TXVirutalServiceMappingList;
    HashMapping: TXVirutalServiceHashMapping;
    Activted: Boolean;
    WaitAsyncConnecting: Boolean;
    WaitAsyncConnecting_BeginTime: TTimeTick;
    PhysicsEngine: TZNet;
    Progressing: Boolean;

    { IO Interface }
    procedure PeerIO_Create(const Sender: TPeerIO);
    procedure PeerIO_Destroy(const Sender: TPeerIO);
    { p2pVM Interface }
    procedure p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
    procedure p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TZNet_WithP2PVM);
    procedure p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TZNet_WithP2PVM);
    procedure p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TZNet_WithP2PVM);
    procedure p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TZNet_WithP2PVM);
    { backcall }
    procedure PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
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

    constructor Create;
    destructor Destroy; override;

    { create new instance. }
    function AddMappingService(const Mapping: TPascalString; MaxWorkload: Cardinal): TXNAT_MappingOnVirutalService;
    function AddMappingServer(const Mapping: TPascalString; MaxWorkload: Cardinal): TXNAT_MappingOnVirutalService;

    procedure OpenTunnel(MODEL: TXNAT_PHYSICS_MODEL); overload;
    procedure OpenTunnel; overload;
    procedure Progress;

    function GetCount: Integer;
    function GetServices(const index: Integer): TXNAT_MappingOnVirutalService;
    function GetServicesOnMapping(const Mapping: SystemString): TXNAT_MappingOnVirutalService;

    property Count: Integer read GetCount;
    property Services[const index: Integer]: TXNAT_MappingOnVirutalService read GetServices; default;
    property ServicesOnMapping[const Mapping: SystemString]: TXNAT_MappingOnVirutalService read GetServicesOnMapping;
  end;

implementation

procedure TXNAT_MappingOnVirutalService_IO.CreateAfter;
begin
  inherited CreateAfter;
  SendingStream := TMS64.Create;
  SendingStream.Delta := 2048;
  Remote_ID := 0;
  Remote_IP := '';
end;

destructor TXNAT_MappingOnVirutalService_IO.Destroy;
begin
  DisposeObject(SendingStream);
  inherited Destroy;
end;

function TXNAT_MappingOnVirutalService_IO.OwnerVS: TXNAT_MappingOnVirutalService;
begin
  Result := OwnerFramework as TXNAT_MappingOnVirutalService;
end;

function TXNAT_MappingOnVirutalService_IO.Connected: Boolean;
begin
  Result := True;
end;

procedure TXNAT_MappingOnVirutalService_IO.Disconnect;
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteCardinal(ID);
  de.WriteCardinal(Remote_ID);
  OwnerVS.SendTunnel.SendDirectStreamCmd(C_Disconnect_reponse, de);
  DisposeObject(de);
  DelayFree(5.0);
end;

procedure TXNAT_MappingOnVirutalService_IO.Write_IO_Buffer(const buff: PByte; const Size: nativeInt);
begin
  SendingStream.WritePtr(buff, Size);
end;

procedure TXNAT_MappingOnVirutalService_IO.WriteBufferOpen;
begin
  SendingStream.Clear;
end;

procedure TXNAT_MappingOnVirutalService_IO.WriteBufferFlush;
var
  nSiz: nativeInt;
  nBuff: PByte;
begin
  if SendingStream.Size > 0 then
    begin
      Build_XNAT_Buff(SendingStream.Memory, SendingStream.Size, ID, Remote_ID, nSiz, nBuff);
      OwnerVS.SendTunnel.SendCompleteBuffer(C_Data, nBuff, nSiz, True);
      SendingStream.Clear;
    end;
end;

procedure TXNAT_MappingOnVirutalService_IO.WriteBufferClose;
begin
  WriteBufferFlush;
end;

function TXNAT_MappingOnVirutalService_IO.GetPeerIP: SystemString;
begin
  Result := Remote_IP;
end;

function TXNAT_MappingOnVirutalService_IO.WriteBuffer_is_NULL: Boolean;
begin
  Result := True;
end;

procedure TXNAT_MappingOnVirutalService_IO.Progress;
begin
  inherited Progress;
  Process_Send_Buffer();
end;

procedure TXNAT_MappingOnVirutalService.Init;
begin
  Mapping := '';
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
  XNAT := nil;
end;

procedure TXNAT_MappingOnVirutalService.SendTunnel_ConnectResult(const cState: Boolean);
begin
  if cState then
    begin
      DoStatus('[%s] Send Tunnel connect success.', [Mapping.Text]);
      if not RecvTunnel.Connected then
          RecvTunnel.AsyncConnectM(RecvTunnel_IPV6, RecvTunnel_Port, {$IFDEF FPC}@{$ENDIF FPC}RecvTunnel_ConnectResult)
      else
          RecvTunnel_ConnectResult(True);
    end
  else
      DoStatus('error: [%s] Send Tunnel connect failed!', [Mapping.Text]);
end;

procedure TXNAT_MappingOnVirutalService.RecvTunnel_ConnectResult(const cState: Boolean);
begin
  if cState then
    begin
      DoStatus('[%s] Receive Tunnel connect success.', [Mapping.Text]);
      SendTunnel.ProgressPost.PostExecuteM(0, {$IFDEF FPC}@{$ENDIF FPC}delay_RequestListen);
    end
  else
      DoStatus('error: [%s] Receive Tunnel connect failed!', [Mapping.Text]);
end;

procedure TXNAT_MappingOnVirutalService.RequestListen_Result(Sender: TPeerIO; Result_: TDFE);
begin
  if Result_.Reader.ReadBool then
    begin
      DoStatus('success: remote host:%s port:%s mapping to local Service', [XNAT.Host.Text, Remote_ListenPort.Text]);
      UpdateWorkload(True);
    end
  else
      DoStatus('failed: remote host:%s port:%s listen error!', [XNAT.Host.Text, Remote_ListenPort.Text]);
end;

procedure TXNAT_MappingOnVirutalService.delay_RequestListen(Sender: TN_Post_Execute);
var
  de: TDFE;
begin
  de := TDFE.Create;
  de.WriteCardinal(SendTunnel.RemoteID);
  de.WriteCardinal(RecvTunnel.RemoteID);
  SendTunnel.SendStreamCmdM(C_RequestListen, de, {$IFDEF FPC}@{$ENDIF FPC}RequestListen_Result);
  DisposeObject(de);
end;

procedure TXNAT_MappingOnVirutalService.Open;
var
  io_array: TIO_Array;
  p_id: Cardinal;
  p_io: TPeerIO;
begin
  if RecvTunnel = nil then
    begin
      RecvTunnel := TZNet_WithP2PVM_Client.Create;
      { sequence sync }
      RecvTunnel.SyncOnCompleteBuffer := True;
      RecvTunnel.SyncOnResult := True;
      RecvTunnel.SwitchMaxPerformance;
      { compressed complete buffer }
      RecvTunnel.CompleteBufferCompressed := XNAT.ProtocolCompressed;
      { automated swap space }
      RecvTunnel.CompleteBufferSwapSpace := True;
      { register cmd }
      if not RecvTunnel.ExistsRegistedCmd(C_Connect_request) then
          RecvTunnel.RegisterDirectStream(C_Connect_request).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_connect_request;
      if not RecvTunnel.ExistsRegistedCmd(C_Disconnect_request) then
          RecvTunnel.RegisterDirectStream(C_Disconnect_request).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_disconnect_request;
      if not RecvTunnel.ExistsRegistedCmd(C_Data) then
          RecvTunnel.RegisterCompleteBuffer(C_Data).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_data;
      { disable status }
      RecvTunnel.PrintParams[C_Connect_request] := False;
      RecvTunnel.PrintParams[C_Disconnect_request] := False;
      RecvTunnel.PrintParams[C_Data] := False;
    end;
  if SendTunnel = nil then
    begin
      SendTunnel := TZNet_WithP2PVM_Client.Create;
      { sequence sync }
      SendTunnel.SyncOnCompleteBuffer := True;
      SendTunnel.SyncOnResult := True;
      SendTunnel.SwitchMaxPerformance;
      { compressed complete buffer }
      SendTunnel.CompleteBufferCompressed := XNAT.ProtocolCompressed;
      { automated swap space }
      SendTunnel.CompleteBufferSwapSpace := True;
      { disable status }
      SendTunnel.PrintParams[C_Connect_reponse] := False;
      SendTunnel.PrintParams[C_Disconnect_reponse] := False;
      SendTunnel.PrintParams[C_Data] := False;
      SendTunnel.PrintParams[C_Workload] := False;
    end;

  XNAT.PhysicsEngine.GetIO_Array(io_array);
  for p_id in io_array do
    begin
      p_io := TPeerIO(XNAT.PhysicsEngine.PeerIO_HashPool[p_id]);
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

  if not SendTunnel.Connected then
      SendTunnel.AsyncConnectM(SendTunnel_IPV6, SendTunnel_Port, {$IFDEF FPC}@{$ENDIF FPC}SendTunnel_ConnectResult)
  else
      SendTunnel_ConnectResult(True);
end;

procedure TXNAT_MappingOnVirutalService.cmd_connect_request(Sender: TPeerIO; InData: TDFE);
var
  Remote_ID: Cardinal;
  x_io: TXNAT_MappingOnVirutalService_IO;
  de: TDFE;
begin
  Remote_ID := InData.Reader.ReadCardinal;
  x_io := TXNAT_MappingOnVirutalService_IO.Create(Self, XNAT);
  x_io.Remote_ID := Remote_ID;
  x_io.Remote_IP := InData.Reader.ReadString;

  de := TDFE.Create;
  de.WriteBool(True);
  de.WriteCardinal(x_io.ID);
  de.WriteCardinal(x_io.Remote_ID);
  SendTunnel.SendDirectStreamCmd(C_Connect_reponse, de);
  DisposeObject(de);
end;

procedure TXNAT_MappingOnVirutalService.cmd_disconnect_request(Sender: TPeerIO; InData: TDFE);
var
  local_id, Remote_ID: Cardinal;
  p_io: TPeerIO;
begin
  Remote_ID := InData.Reader.ReadCardinal;
  local_id := InData.Reader.ReadCardinal;
  p_io := PeerIO[local_id];
  if p_io <> nil then
      DisposeObject(p_io);
end;

procedure TXNAT_MappingOnVirutalService.cmd_data(Sender: TPeerIO; InData: PByte; DataSize: nativeInt);
var
  local_id, Remote_ID: Cardinal;
  destSiz: nativeInt;
  destBuff: PByte;
  x_io: TXNAT_MappingOnVirutalService_IO;
begin
  Extract_XNAT_Buff(InData, DataSize, Remote_ID, local_id, destSiz, destBuff);
  x_io := TXNAT_MappingOnVirutalService_IO(PeerIO[local_id]);
  if x_io <> nil then
    begin
      x_io.Write_Physics_Fragment(destBuff, destSiz);
    end;
end;

constructor TXNAT_MappingOnVirutalService.Create;
begin
  inherited Create;
  Init;
end;

destructor TXNAT_MappingOnVirutalService.Destroy;
begin
  if SendTunnel <> nil then
    begin
      SendTunnel.Disconnect;
      DisposeObject(SendTunnel);
    end;

  if RecvTunnel <> nil then
    begin
      RecvTunnel.Disconnect;
      DisposeObject(RecvTunnel);
    end;

  inherited Destroy;
end;

procedure TXNAT_MappingOnVirutalService.UpdateWorkload(force: Boolean);
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
    if (Count = LastUpdateWorkload) or (GetTimeTick() - LastUpdateTime < 1000) then
        exit;

  de := TDFE.Create;
  de.WriteCardinal(MaxWorkload);
  de.WriteCardinal(Count);
  SendTunnel.SendDirectStreamCmd(C_Workload, de);
  DisposeObject(de);

  LastUpdateWorkload := Count;
  LastUpdateTime := GetTimeTick();
end;

function TXNAT_MappingOnVirutalService.StartService(Host: SystemString; Port: Word): Boolean;
begin
  Result := True;
end;

procedure TXNAT_MappingOnVirutalService.StopService;
begin
end;

procedure TXNAT_MappingOnVirutalService.Progress;
begin
  if (XNAT <> nil) and (not XNAT.Progressing) then
    begin
      XNAT.Progress;
      exit;
    end;

  inherited Progress;

  if SendTunnel <> nil then
      SendTunnel.Progress;
  if RecvTunnel <> nil then
      RecvTunnel.Progress;
end;

function TXNAT_MappingOnVirutalService.WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; TimeOut_: TTimeTick): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport');
end;

procedure TXNAT_MappingOnVirutalService.WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, Result_: TDFE; TimeOut_: TTimeTick);
begin
  RaiseInfo('WaitSend no Suppport');
end;

procedure TPhysicsEngine_Special.PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
begin
  if cState then
      Owner.BuildP2PAuthTokenM({$IFDEF FPC}@{$ENDIF FPC}PhysicsVMBuildAuthToken_Result)
  else
      XNAT.WaitAsyncConnecting := False;
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
  Owner.OpenP2pVMTunnelM(True, GenerateQuantumCryptographyPassword(XNAT.AuthToken), {$IFDEF FPC}@{$ENDIF FPC}PhysicsOpenVM_Result)
end;

procedure TPhysicsEngine_Special.PhysicsOpenVM_Result(const cState: Boolean);
begin
  if cState then
    begin
      Owner.p2pVMTunnel.MaxVMFragmentSize := umlStrToInt(XNAT.MaxVMFragment, Owner.p2pVMTunnel.MaxVMFragmentSize);
      Owner.SendStreamCmdM(C_IPV6Listen, nil, {$IFDEF FPC}@{$ENDIF FPC}IPV6Listen_Result);
    end
  else
      XNAT.WaitAsyncConnecting := False;
end;

procedure TPhysicsEngine_Special.IPV6Listen_Result(Sender: TPeerIO; Result_: TDFE);
var
  Mapping: TPascalString;
  Remote_ListenAddr, Remote_ListenPort: TPascalString;
  RecvTunnel_IPV6: TPascalString;
  RecvTunnel_Port: Word;
  SendTunnel_IPV6: TPascalString;
  SendTunnel_Port: Word;
  tunMp: TXNAT_MappingOnVirutalService;
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
      tunMp := XNAT.HashMapping[Mapping];
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

procedure TXNAT_VS_Mapping.PeerIO_Create(const Sender: TPeerIO);
begin
  TPhysicsEngine_Special(Sender.UserSpecial).XNAT := Self;
end;

procedure TXNAT_VS_Mapping.PeerIO_Destroy(const Sender: TPeerIO);
begin
end;

procedure TXNAT_VS_Mapping.p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
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

  if PhysicsEngine is TZNet_Server then
    begin
    end
  else if PhysicsEngine is TZNet_Client then
    begin
    end;

  Accept := CompareQuantumCryptographyPassword(AuthToken, Token);
  if Accept then
      Sender.Print('p2pVM auth Successed!')
  else
      Sender.Print('p2pVM auth failed!');
end;

procedure TXNAT_VS_Mapping.p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TZNet_WithP2PVM);
begin
  if PhysicsEngine is TZNet_Server then
    begin
    end
  else if PhysicsEngine is TZNet_Client then
    begin
    end;
  DoStatus('XTunnel Open Before on %s', [Sender.PeerIP]);
end;

procedure TXNAT_VS_Mapping.p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TZNet_WithP2PVM);
begin
  if PhysicsEngine is TZNet_Server then
    begin
    end
  else if PhysicsEngine is TZNet_Client then
    begin
    end;
  DoStatus('XTunnel Open on %s', [Sender.PeerIP]);
end;

procedure TXNAT_VS_Mapping.p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TZNet_WithP2PVM);
begin
  if PhysicsEngine is TZNet_Server then
    begin
      Sender.SendStreamCmdM(C_IPV6Listen, nil, {$IFDEF FPC}@{$ENDIF FPC}TPhysicsEngine_Special(Sender.UserSpecial).IPV6Listen_Result);
    end
  else if PhysicsEngine is TZNet_Client then
    begin
    end;
  DoStatus('XTunnel Open After on %s', [Sender.PeerIP]);
end;

procedure TXNAT_VS_Mapping.p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TZNet_WithP2PVM);
begin
  if PhysicsEngine is TZNet_Server then
    begin
    end
  else if PhysicsEngine is TZNet_Client then
    begin
    end;
  DoStatus('XTunnel Close on %s', [Sender.PeerIP]);
end;

procedure TXNAT_VS_Mapping.PhysicsConnect_Result_BuildP2PToken(const cState: Boolean);
begin
  TPhysicsEngine_Special(TZNet_Client(PhysicsEngine).ClientIO.UserSpecial).PhysicsConnect_Result_BuildP2PToken(cState);
end;

constructor TXNAT_VS_Mapping.Create;
begin
  inherited Create;
  Host := '';
  Port := '4921';
  AuthToken := '';
  MaxVMFragment := '8192';
  ProtocolCompressed := False;
  MappingList := TXVirutalServiceMappingList.Create;
  HashMapping := TXVirutalServiceHashMapping.Create(False, 64, nil);
  Activted := False;
  WaitAsyncConnecting := False;
  PhysicsEngine := nil;
  Progressing := False;
end;

destructor TXNAT_VS_Mapping.Destroy;
var
  i: Integer;
begin
  for i := MappingList.Count - 1 downto 0 do
      DisposeObject(MappingList[i]);
  DisposeObject(MappingList);
  DisposeObject(HashMapping);

  if PhysicsEngine <> nil then
    begin
      if PhysicsEngine is TZNet_Client then
        begin
          TZNet_Client(PhysicsEngine).Disconnect;
        end;
    end;

  DisposeObject(PhysicsEngine);
  inherited Destroy;
end;

function TXNAT_VS_Mapping.AddMappingService(const Mapping: TPascalString; MaxWorkload: Cardinal): TXNAT_MappingOnVirutalService;
begin
  Result := TXNAT_MappingOnVirutalService.Create;
  Result.Mapping := Mapping;
  Result.MaxWorkload := MaxWorkload;
  Result.XNAT := Self;
  MappingList.Add(Result);
  HashMapping.Add(Result.Mapping, Result);
end;

function TXNAT_VS_Mapping.AddMappingServer(const Mapping: TPascalString; MaxWorkload: Cardinal): TXNAT_MappingOnVirutalService;
begin
  Result := AddMappingService(Mapping, MaxWorkload);
end;

procedure TXNAT_VS_Mapping.OpenTunnel(MODEL: TXNAT_PHYSICS_MODEL);
begin
  Activted := True;

  { init tunnel engine }
  if PhysicsEngine = nil then
    begin
      if MODEL = TXNAT_PHYSICS_MODEL.XNAT_PHYSICS_SERVICE then
          PhysicsEngine := TXPhysicsServer.Create
      else
          PhysicsEngine := TXPhysicsClient.Create;
    end;

  PhysicsEngine.UserSpecialClass := TPhysicsEngine_Special;
  PhysicsEngine.IOInterface := Self;
  PhysicsEngine.VMInterface := Self;

  { Security protocol }
  PhysicsEngine.SwitchMaxSecurity;

  if PhysicsEngine is TZNet_Server then
    begin
      if TZNet_Server(PhysicsEngine).StartService(Host, umlStrToInt(Port)) then
          DoStatus('Tunnel Open %s:%s successed', [TranslateBindAddr(Host), Port.Text])
      else
          DoStatus('error: Tunnel is Closed for %s:%s', [TranslateBindAddr(Host), Port.Text]);
    end
  else if PhysicsEngine is TZNet_Client then
    begin
      if not TZNet_Client(PhysicsEngine).Connected then
        begin
          WaitAsyncConnecting := True;
          WaitAsyncConnecting_BeginTime := GetTimeTick;
          TZNet_Client(PhysicsEngine).AsyncConnectM(Host, umlStrToInt(Port), {$IFDEF FPC}@{$ENDIF FPC}PhysicsConnect_Result_BuildP2PToken);
        end;
    end;
end;

procedure TXNAT_VS_Mapping.OpenTunnel;
begin
  OpenTunnel(TXNAT_PHYSICS_MODEL.XNAT_PHYSICS_CLIENT);
end;

procedure TXNAT_VS_Mapping.Progress;
var
  i: Integer;
  tunMp: TXNAT_MappingOnVirutalService;
begin
  if Progressing then
      exit;

  Progressing := True;

  if (PhysicsEngine <> nil) then
    begin
      if (PhysicsEngine is TZNet_Client) then
        begin
          if WaitAsyncConnecting and (GetTimeTick - WaitAsyncConnecting_BeginTime > 15000) then
              WaitAsyncConnecting := False;

          if Activted and (not TZNet_Client(PhysicsEngine).Connected) then
            begin
              if not WaitAsyncConnecting then
                begin
                  OpenTunnel(TXNAT_PHYSICS_MODEL.XNAT_PHYSICS_CLIENT);
                end;
            end;
        end;
      PhysicsEngine.Progress;
    end;

  i := 0;
  while i < MappingList.Count do
    begin
      tunMp := MappingList[i];
      tunMp.UpdateWorkload(False);
      tunMp.Progress;
      inc(i);
    end;

  Progressing := False;
end;

function TXNAT_VS_Mapping.GetCount: Integer;
begin
  Result := MappingList.Count;
end;

function TXNAT_VS_Mapping.GetServices(const index: Integer): TXNAT_MappingOnVirutalService;
begin
  Result := MappingList[index];
end;

function TXNAT_VS_Mapping.GetServicesOnMapping(const Mapping: SystemString): TXNAT_MappingOnVirutalService;
begin
  Result := HashMapping[Mapping];
end;

end.
