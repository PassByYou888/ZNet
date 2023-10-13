program reverse_p2pVM_serv;

{$APPTYPE CONSOLE}

{$R *.res}

{  P2PVM is a virtual machine based on IO work, which has no requirements for server and client models,
Simply put,
P2PVM servers and clients can work on both physical clients and physical servers
As long as there is IO present, P2PVM can work
If we build a backend server based on P2PVM, the scalability space will be very flexible and not limited to any network environment  }

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Status;

{  This demo will first create a physical client and then run a P2PVM server on top of the physical client  }
type
  TMyP2PVM_Server = class(TZNet_WithP2PVM_Server)
  public
  end;

var
  MyP2PVM_Server: TMyP2PVM_Server;

type
  TMyPhysics_Client = class(TPhysicsClient)
  public
    procedure p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean); override;
    procedure p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM); override;
    procedure p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM); override;
    procedure p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM); override;
    procedure p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM); override;
  end;

procedure TMyPhysics_Client.p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
begin
  inherited;
  {  P2pvm handshake step 1: Remote Authentication  }
  Accept := True;
end;

procedure TMyPhysics_Client.p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
begin
  inherited;
  DoStatus('P2P VM openbefore requester IP:%s', [Sender.PeerIP]);
  Sender.p2pVM.QuietMode := True;
  Sender.p2pVM.InstallLogicFramework(MyP2PVM_Server);
end;

procedure TMyPhysics_Client.p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
begin
  inherited;
  DoStatus('P2P VM open requester IP:%s', [Sender.PeerIP]);
end;

procedure TMyPhysics_Client.p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
begin
  inherited;
  DoStatus('P2PVM OpenAfter requester IP:%s', [Sender.PeerIP]);
end;

procedure TMyPhysics_Client.p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TZNet_P2PVM);
begin
  inherited;
  DoStatus('P2PVM Close Requestor IP:%s', [Sender.PeerIP]);
  Sender.p2pVM.UninstallLogicFramework(MyP2PVM_Server);
end;

var
  MyPhysics_Client: TMyPhysics_Client;

begin
  MyP2PVM_Server := TMyP2PVM_Server.Create;
  MyP2PVM_Server.StartService('::', 99);
  MyP2PVM_Server.QuietMode := True;

  MyPhysics_Client := TMyPhysics_Client.Create;

  while True do
    begin
{$IFDEF MSWINDOWS}
      SetConsoleTitle(PWideChar(Format('P2PVM server online:%d', [MyP2PVM_Server.Count])));
{$ENDIF MSWINDOWS}
      {  Automatic reconnection  }
      if not MyPhysics_Client.RemoteInited then
        if not MyPhysics_Client.Connect('127.0.0.1', 19899) then
          begin
            Z.Core.CheckThreadSynchronize(100);
            continue;
          end;

      {  After p2pvm is installed by the installlogicframework, progress will automatically call myp2pvm_Server.Progress  }
      if MyPhysics_Client.Connected then
          MyPhysics_Client.Progress;

      {  Green and environmentally friendly, saving energy for CPUs  }
      Z.Core.CheckThreadSynchronize(10);
    end;

end.
