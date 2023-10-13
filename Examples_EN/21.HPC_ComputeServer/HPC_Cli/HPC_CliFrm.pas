unit HPC_CliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.Net,
  Z.Status, Z.Core,
  Z.Net.Client.CrossSocket,
  Z.Net.Client.ICS, Z.Net.Client.Indy,
  Z.Cadencer, Z.DFE, Z.Net.DoubleTunnelIO.NoAuth;

type
  TDoubleTunnelClientForm = class(TForm)
    Memo1: TMemo;
    ConnectButton: TButton;
    HostEdit: TLabeledEdit;
    Timer1: TTimer;
    HelloWorldBtn: TButton;
    AsyncConnectButton: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure HelloWorldBtnClick(Sender: TObject);
    procedure AsyncConnectButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);

    procedure cmd_ChangeCaption(Sender: TPeerClient; InData: TDFE);
    procedure cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDFE);
  public
    { Public declarations }
    RecvTunnel: TZNet_Client_CrossSocket;
    SendTunnel: TZNet_Client_CrossSocket;
    client    : TZNet_DoubleTunnelClient_NoAuth;
  end;

var
  DoubleTunnelClientForm: TDoubleTunnelClientForm;

implementation

{$R *.dfm}


procedure TDoubleTunnelClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TDoubleTunnelClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  RecvTunnel := TZNet_Client_CrossSocket.Create;
  SendTunnel := TZNet_Client_CrossSocket.Create;
  client := TZNet_DoubleTunnelClient_NoAuth.Create(RecvTunnel, SendTunnel);

  client.RegisterCommand;

  {  Registering communication instructions that can be initiated by the server  }
  client.RecvTunnel.RegisterDirectStream('ChangeCaption').OnExecute := cmd_ChangeCaption;
  client.RecvTunnel.RegisterStream('GetClientValue').OnExecute := cmd_GetClientValue;
end;

procedure TDoubleTunnelClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TDoubleTunnelClientForm.HelloWorldBtnClick(Sender: TObject);
var
  SendDe, ResultDE: TDFE;
begin
  {  Asynchronous sending and receiving Stream instructions, feedback triggered by proc callback  }
  SendDe := TDFE.Create;
  SendDe.WriteString('123456');
  client.SendTunnel.SendStreamCmdP('helloWorld_Stream_Result', SendDe,
    procedure(Sender: TPeerClient; ResultData: TDFE)
    begin
      if ResultData.Count > 0 then
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
    end);
  DisposeObject([SendDe]);

  {  Blocking sending and receiving Stream instructions  }
  SendDe := TDFE.Create;
  ResultDE := TDFE.Create;
  SendDe.WriteString('654321');
  client.SendTunnel.WaitSendStreamCmd('helloWorld_Stream_Result', SendDe, ResultDE, 5000);
  if ResultDE.Count > 0 then
      DoStatus('server response:%s', [ResultDE.Reader.ReadString]);
  DisposeObject([SendDe, ResultDE]);
end;

procedure TDoubleTunnelClientForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  client.Progress;
end;

procedure TDoubleTunnelClientForm.cmd_ChangeCaption(Sender: TPeerClient; InData: TDFE);
begin
  Caption := InData.Reader.ReadString;
end;

procedure TDoubleTunnelClientForm.cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDFE);
begin
  OutData.WriteString('getclientvalue:abc');
end;

procedure TDoubleTunnelClientForm.ConnectButtonClick(Sender: TObject);
begin
  {  Method 1, Blocking Links  }
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
      {  Merge two channels asynchronously  }
      client.TunnelLinkP(
        procedure(const State: Boolean)
        begin
          if State then
            begin
              {  Dual channel link successful  }
              DoStatus('double tunnel link success!');
            end;
        end);
      {  Merge two channels synchronously  }
      // if client.TunnelLink then
      // begin
      {  //Dual channel link succeeded  }
      // end;
    end;
end;

procedure TDoubleTunnelClientForm.AsyncConnectButtonClick(Sender: TObject);
begin
  {  Method 2, asynchronous dual channel link  }
  client.AsyncConnectP(HostEdit.Text, 9816, 9815,
    procedure(const cState: Boolean)
    begin
      if cState then
        {  Merge two channels asynchronously  }
          client.TunnelLinkP(
          procedure(const tState: Boolean)
          begin
            if tState then
              begin
                {  Dual channel link successful  }
                DoStatus('double tunnel link success!');
              end;
          end);
      {  Merge two channels synchronously  }
      // if client.TunnelLink then
      // begin
      {  //Dual channel link succeeded  }
      // end;
    end);
end;

end.
