unit FMXAuthDoubleTunnelCliFrm;


interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,

  Z.Net.Client.Indy, Z.DFE,
  Z.Net, Z.Core, Z.Status,
  Z.Net.DoubleTunnelIO, FMX.Memo.Types;

type
  TFMXAuthDoubleClientForm = class(TForm)
    Memo1: TMemo;
    connectButton: TButton;
    HostEdit: TEdit;
    HelloWorldBtn: TButton;
    Timer1: TTimer;
    UserEdit: TEdit;
    PasswdEdit: TEdit;
    RegUserButton: TButton;
    AsyncButton: TButton;
    timeLabel: TLabel;
    FixedTimeButton: TButton;
    connectTunnelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure HelloWorldBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure RegUserButtonClick(Sender: TObject);
    procedure AsyncButtonClick(Sender: TObject);
    procedure FixedTimeButtonClick(Sender: TObject);
    procedure connectTunnelButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);

    procedure cmd_ChangeCaption(Sender: TPeerClient; InData: TDFE);
    procedure cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDFE);
  public
    { Public declarations }
    // vm���
    // vm����������������������У�ͬʱ��������Э��ջ�Ĺ���
    // ����������ὫRecvTunnel+SendTunnelͬʱ����VMTunnel�У�ֻ��һ������ʵ��˫ͨ������
    // vm����������κ�socket���ܣ�indy��ics��crossSocket���ȵȾ�֧��vm���
    VMTunnel: TZNet_Client_Indy;

    // zs������ͨѶ���
    RecvTunnel: TZNet_WithP2PVM_Client;
    SendTunnel: TZNet_WithP2PVM_Client;
    client: TZNet_DoubleTunnelClient;
  end;

var
  FMXAuthDoubleClientForm: TFMXAuthDoubleClientForm;

implementation

{$R *.fmx}

{ TFMXClientForm }

procedure TFMXAuthDoubleClientForm.cmd_ChangeCaption(Sender: TPeerClient; InData: TDFE);
begin
  DoStatus('Change Caption:%s', [InData.Reader.ReadString]);
end;

procedure TFMXAuthDoubleClientForm.cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDFE);
begin
  OutData.WriteString('getclientvalue:abc');
end;

procedure TFMXAuthDoubleClientForm.connectButtonClick(Sender: TObject);
begin
  SendTunnel.Connect('::', 2);
  RecvTunnel.Connect('::', 1);

  // ���˫ͨ���Ƿ��Ѿ��ɹ����ӣ�ȷ������˶ԳƼ��ܵȵȳ�ʼ������
  while (not client.RemoteInited) and (client.Connected) do
    begin
      TThread.Sleep(10);
      client.Progress;
    end;

  if client.Connected then
    begin
      // Ƕ��ʽ��������֧��
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

procedure TFMXAuthDoubleClientForm.connectTunnelButtonClick(
  Sender: TObject);
begin
  if not VMTunnel.Connect(HostEdit.Text, 9899) then
      exit;

  VMTunnel.ClientIO.BuildP2PAuthTokenP(procedure
    begin
      VMTunnel.ClientIO.OpenP2PVMTunnelP(True, '', procedure(const VMauthState: Boolean)
        begin
          // ���VM������ֳɹ�
          if VMauthState then
            begin
              // ���ͻ��˿�ܰ󶨵������
              // �����������ͻ��ˣ����Ƕ��󶨽�ȥ
              VMTunnel.ClientIO.p2pVMTunnel.InstallLogicFramework(SendTunnel);
              VMTunnel.ClientIO.p2pVMTunnel.InstallLogicFramework(RecvTunnel);
            end
        end);
    end);
end;

procedure TFMXAuthDoubleClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TFMXAuthDoubleClientForm.FixedTimeButtonClick(Sender: TObject);
begin
  // ����ͬ����������Progress����
  // �������ǽ�ʱ����ӳ��ʽ��͵���С
  client.SendTunnel.SyncOnResult := True;
  client.SyncCadencer;
  client.SendTunnel.WaitP(1000, procedure(const cState: Boolean)
    begin
      // ��Ϊ����SyncOnResult���������������Ƕ������
      // �������ڹر������Ա�֤����������Ƕ��ִ��
      client.SendTunnel.SyncOnResult := False;
    end);
end;

procedure TFMXAuthDoubleClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  // vm���
  // vm����������������������У�ͬʱ��������Э��ջ�Ĺ���
  // ����������ὫRecvTunnel+SendTunnelͬʱ����VMTunnel�У�ֻ��һ������ʵ��˫ͨ������
  VMTunnel := TZNet_Client_Indy.Create;

  // zs������ͨѶ���
  RecvTunnel := TZNet_WithP2PVM_Client.Create;
  SendTunnel := TZNet_WithP2PVM_Client.Create;
  client := TZNet_DoubleTunnelClient.Create(RecvTunnel, SendTunnel);

  client.RegisterCommand;

  // ע������ɷ����������ͨѶָ��
  client.RecvTunnel.RegisterDirectStream('ChangeCaption').OnExecute := cmd_ChangeCaption;
  client.RecvTunnel.RegisterStream('GetClientValue').OnExecute := cmd_GetClientValue;
end;

procedure TFMXAuthDoubleClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TFMXAuthDoubleClientForm.HelloWorldBtnClick(Sender: TObject);
var
  SendDe, ResultDE: TDFE;
begin
  // ������������һ��console��ʽ��hello worldָ��
  client.SendTunnel.SendDirectConsoleCmd('helloWorld_Console', '');

  // ������������һ��stream��ʽ��hello worldָ��
  SendDe := TDFE.Create;
  SendDe.WriteString('directstream 123456');
  client.SendTunnel.SendDirectStreamCmd('helloWorld_Stream', SendDe);
  DisposeObject([SendDe]);

  // �첽��ʽ���ͣ����ҽ���Streamָ�������proc�ص�����
  SendDe := TDFE.Create;
  SendDe.WriteString('123456');
  client.SendTunnel.SendStreamCmdP('helloWorld_Stream_Result', SendDe,
    procedure(Sender: TPeerClient; ResultData: TDFE)
    begin
      if ResultData.Count > 0 then
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
    end);
  DisposeObject([SendDe]);

  // ������ʽ���ͣ����ҽ���Streamָ��
  SendDe := TDFE.Create;
  ResultDE := TDFE.Create;
  SendDe.WriteString('123456');
  client.SendTunnel.WaitSendStreamCmd('helloWorld_Stream_Result', SendDe, ResultDE, 5000);
  if ResultDE.Count > 0 then
      DoStatus('server response:%s', [ResultDE.Reader.ReadString]);
  DisposeObject([SendDe, ResultDE]);
end;

procedure TFMXAuthDoubleClientForm.RegUserButtonClick(Sender: TObject);
begin
  SendTunnel.Connect('::', 2);
  RecvTunnel.Connect('::', 1);

  client.RegisterUserP(UserEdit.Text, PasswdEdit.Text, procedure(const rState: Boolean)
    begin
      client.Disconnect;
    end);
end;

procedure TFMXAuthDoubleClientForm.AsyncButtonClick(Sender: TObject);
begin
  // �첽ʽ˫ͨ������
  client.AsyncConnectP('::', 1, 2,
    procedure(const cState: Boolean)
    begin
      if cState then
        begin
          DoStatus('connected success!');
          // Ƕ��ʽ��������֧��
          client.UserLoginP(UserEdit.Text, PasswdEdit.Text,
            procedure(const lState: Boolean)
            begin
              if lState then
                begin
                  DoStatus('login successed!');
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
                  DoStatus('login failed!');
                end;
            end);
        end
      else
        begin
          DoStatus('connected failed!');
        end;
    end);
end;

procedure TFMXAuthDoubleClientForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  VMTunnel.Progress;
  client.Progress;
  timeLabel.Text := Format('sync time:%f', [client.CadencerEngine.UpdateCurrentTime]);
end;

end.
