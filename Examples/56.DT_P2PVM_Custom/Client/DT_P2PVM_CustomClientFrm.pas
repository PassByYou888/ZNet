unit DT_P2PVM_CustomClientFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  Z.Core, Z.PascalStrings, Z.Status, Z.DFE,
  Z.Net, Z.Net.PhysicsIO,
  Z.Net.DoubleTunnelIO,
  Z.Net.DoubleTunnelIO.NoAuth;

type
  TDT_P2PVM_CustomClientForm = class(TForm)
    Memo: TMemo;
    netTimer: TTimer;
    connButton: TButton;
    disButton: TButton;
    UserEdit: TLabeledEdit;
    PasswdEdit: TLabeledEdit;
    Clone_Button: TButton;
    procedure Clone_ButtonClick(Sender: TObject);
    procedure connButtonClick(Sender: TObject);
    procedure disButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure netTimerTimer(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
  end;

var
  DT_P2PVM_CustomClientForm: TDT_P2PVM_CustomClientForm;
  PhysicsTunnel: TPhysicsClient;
  AuthCli: TDT_P2PVM_Custom_Client;

implementation

{$R *.dfm}


procedure TDT_P2PVM_CustomClientForm.Clone_ButtonClick(Sender: TObject);
begin
  TDT_P2PVM_Custom_Client.Create_Clone(AuthCli);
end;

procedure TDT_P2PVM_CustomClientForm.connButtonClick(Sender: TObject);
begin
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_P := procedure(Sender: TZNet; P_IO: TPeerIO)
    begin
      AuthCli.Connect_P(UserEdit.Text, PasswdEdit.Text, procedure(const cState: Boolean)
        begin
          if cState then
              DoStatus('用户 %s 双通道已建立', [UserEdit.Text]);
        end);
    end;
  PhysicsTunnel.AsyncConnectP('127.0.0.1', 11938, procedure(const cState: Boolean)
    begin
      DoStatus('物理链接已建立');
    end);
end;

procedure TDT_P2PVM_CustomClientForm.disButtonClick(Sender: TObject);
begin
  AuthCli.Disconnect;
  PhysicsTunnel.Disconnect;
end;

procedure TDT_P2PVM_CustomClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(AuthCli);
  DisposeObject(PhysicsTunnel);
  DeleteDoStatusHook(self);
end;

procedure TDT_P2PVM_CustomClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);
  PhysicsTunnel := TPhysicsClient.Create;

  AuthCli := TDT_P2PVM_Custom_Client.Create(TDTClient, PhysicsTunnel, 'ACR', '::', '101', 'ACS', '::', '100');
  AuthCli.QuietMode := False;
end;

procedure TDT_P2PVM_CustomClientForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

procedure TDT_P2PVM_CustomClientForm.netTimerTimer(Sender: TObject);
begin
  CheckThread;
  AuthCli.Progress;
end;

end.
