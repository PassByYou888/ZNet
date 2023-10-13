﻿unit cliFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Z.Core, Z.Net, Z.PascalStrings,
  Z.Net.DoubleTunnelIO.NoAuth, Z.DFE, Z.Net.PhysicsIO,
  Z.Status;

type

  { TcliForm }

  TcliForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    fpsTimer: TTimer;
    hostEdit: TLabeledEdit;
    expEdit: TLabeledEdit;
    Memo: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure fpsTimerTimer(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
    procedure DoAutomatedP2PVMClientConnectionDone(Sender: TZNet; P_IO: TPeerIO);
    procedure Tunnel_Result(const state:Boolean);
    procedure RunExpResult(Sender: TPeerIO; ResultData: TDFE);
  public

  end;

var
  cliForm: TcliForm;
  phyIO: TPhysicsClient;
  recvIO, sendIO: TZNet_WithP2PVM_Client;
  doubleCli: TDTClient_NoAuth;

implementation

{$R *.lfm}

{ TcliForm }

procedure TcliForm.fpsTimerTimer(Sender: TObject);
begin
  CheckThreadSynchronize;
  phyIO.Progress;
  doubleCli.Progress;
end;

procedure TcliForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(text_);
end;

procedure TcliForm.DoAutomatedP2PVMClientConnectionDone(Sender: TZNet; P_IO: TPeerIO);
begin
  doubleCli.TunnelLinkM(@Tunnel_Result);
end;

procedure TcliForm.Tunnel_Result(const state: Boolean);
begin
  if state then
  DoStatus('Successfully established dual channel');
end;

procedure TcliForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, @DoStatus_backcall);
  phyIO:= TPhysicsClient.Create;
  recvIO:= TZNet_WithP2PVM_Client.Create;
  sendIO:= TZNet_WithP2PVM_Client.Create;

  phyIO.AutomatedP2PVMBindClient.AddClient(sendIO, '::', 1);
  phyIO.AutomatedP2PVMBindClient.AddClient(recvIO, '::', 2);
  phyIO.AutomatedP2PVMClient:=True;
  phyIO.AutomatedP2PVMAuthToken:='IOT_p2pVM';

  doubleCli:= TDTClient_NoAuth.Create(recvIO, sendIO);
  doubleCli.RegisterCommand;
end;

procedure TcliForm.Button1Click(Sender: TObject);
begin
  phyIO.OnAutomatedP2PVMClientConnectionDone_M:=@DoAutomatedP2PVMClientConnectionDone;
  phyIO.AsyncConnectM(hostedit.Text, 7189, nil);
end;

procedure TcliForm.RunExpResult(Sender: TPeerIO; ResultData: TDFE);
begin
  DoStatus('Expression returned:%s', [ResultData.R.ReadString]);
end;

procedure TcliForm.Button2Click(Sender: TObject);
var
   d:TDFE;
begin
  d:=TDFE.Create;
  d.WriteString(expEdit.Text);
  sendIO.SendStreamCmdM('runExp', d, @RunExpResult);
  d.Free;
end;

end.
