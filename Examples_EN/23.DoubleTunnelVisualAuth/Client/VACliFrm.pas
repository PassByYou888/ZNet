﻿unit VACliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.Net,
  Z.Status, Z.Core,
  Z.Net.Client.CrossSocket,
  Z.Net.Client.ICS, Z.Net.Client.Indy,
  Z.Cadencer, Z.DFE, Z.Net.DoubleTunnelIO.VirtualAuth;

type
  TAuthDoubleTunnelClientForm = class(TForm)
    Memo1: TMemo;
    ConnectButton: TButton;
    HostEdit: TLabeledEdit;
    Timer1: TTimer;
    UserEdit: TLabeledEdit;
    PasswdEdit: TLabeledEdit;
    AsyncConnectButton: TButton;
    TimeLabel: TLabel;
    fixedTimeButton: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure AsyncConnectButtonClick(Sender: TObject);
    procedure fixedTimeButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);

    procedure cmd_ChangeCaption(Sender: TPeerClient; InData: TDFE);
    procedure cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDFE);
  public
    { Public declarations }
    RecvTunnel: TZNet_Client_CrossSocket;
    SendTunnel: TZNet_Client_CrossSocket;
    client    : TZNet_DoubleTunnelClient_VirtualAuth;
  end;

var
  AuthDoubleTunnelClientForm: TAuthDoubleTunnelClientForm;

implementation

{$R *.dfm}


procedure TAuthDoubleTunnelClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TAuthDoubleTunnelClientForm.fixedTimeButtonClick(Sender: TObject);
begin
  {  High speed synchronization without progress trigger  }
  {  This is done to minimize the delay rate of time  }
  client.SendTunnel.SyncOnResult := True;
  client.SyncCadencer;
  client.SendTunnel.WaitP(1000, procedure(const cState: Boolean)
    begin
      {  Because after synconresult is opened, nested deadlocks will occur in anonymous functions  }
      {  We now turn it off to ensure nested execution of anonymous functions  }
      client.SendTunnel.SyncOnResult := False;
    end);
end;

procedure TAuthDoubleTunnelClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  RecvTunnel := TZNet_Client_CrossSocket.Create;
  SendTunnel := TZNet_Client_CrossSocket.Create;
  client := TZNet_DoubleTunnelClient_VirtualAuth.Create(RecvTunnel, SendTunnel);

  client.RegisterCommand;

  {  Registering communication instructions that can be initiated by the server  }
  client.RecvTunnel.RegisterDirectStream('ChangeCaption').OnExecute := cmd_ChangeCaption;
  client.RecvTunnel.RegisterStream('GetClientValue').OnExecute := cmd_GetClientValue;
end;

procedure TAuthDoubleTunnelClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TAuthDoubleTunnelClientForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  client.Progress;
  TimeLabel.Caption := Format('sync time:%f', [client.CadencerEngine.UpdateCurrentTime]);
end;

procedure TAuthDoubleTunnelClientForm.cmd_ChangeCaption(Sender: TPeerClient; InData: TDFE);
begin
  Caption := InData.Reader.ReadString;
end;

procedure TAuthDoubleTunnelClientForm.cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDFE);
begin
  OutData.WriteString('getclientvalue:abc');
end;

procedure TAuthDoubleTunnelClientForm.ConnectButtonClick(Sender: TObject);
begin
  SendTunnel.Connect(HostEdit.Text, 9815);
  RecvTunnel.Connect(HostEdit.Text, 9816);

  {  Check if both channels have been successfully linked and ensure that initialization work such as symmetric encryption has been completed  }
  while (not client.RemoteInited) and (client.Connected) do
    begin
      TThread.Sleep(10);
      client.Progress;
    end;

  if client.Connected then
    begin
      {  Nested anonymous function support  }
      client.UserLoginP(UserEdit.Text, PasswdEdit.Text,
        procedure(const State: Boolean)
        begin
          if State then
              client.TunnelLinkP(
              procedure(const State: Boolean)
              begin
                DoStatus('double tunnel link success!');
              end)
        end);
    end;
end;

procedure TAuthDoubleTunnelClientForm.AsyncConnectButtonClick(Sender: TObject);
begin
  {  Method 2, asynchronous dual channel link  }
  client.AsyncConnectP(HostEdit.Text, 9816, 9815,
    procedure(const cState: Boolean)
    begin
      if cState then
        begin
          {  Nested anonymous function support  }
          client.UserLoginP(UserEdit.Text, PasswdEdit.Text,
            procedure(const lState: Boolean)
            begin
              if lState then
                begin
                  client.TunnelLinkP(
                    procedure(const tState: Boolean)
                    begin
                      if tState then
                          DoStatus('double tunnel link success!')
                      else
                          DoStatus('double tunnel link failed!');
                    end)
                end
              else
                begin
                  if lState then
                      DoStatus('login success!')
                  else
                      DoStatus('login failed!');
                end;
            end);
        end
      else
        begin
          if cState then
              DoStatus('connected success!')
          else
              DoStatus('connected failed!');
        end;
    end);

end;

end.
