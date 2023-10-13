﻿unit DT_P2PVM_CustomServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  System.IOUtils,

  Z.Core, Z.PascalStrings, Z.Status, Z.DFE,
  Z.Net, Z.Net.PhysicsIO,
  Z.Net.DoubleTunnelIO,
  Z.Net.DoubleTunnelIO.NoAuth;

type
  TDT_P2PVM_CustomServerForm = class(TForm)
    Memo: TMemo;
    startservButton: TButton;
    netTimer: TTimer;
    stopservButton: TButton;
    UserEdit: TLabeledEdit;
    PasswdEdit: TLabeledEdit;
    regUsrButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure netTimerTimer(Sender: TObject);
    procedure regUsrButtonClick(Sender: TObject);
    procedure startservButtonClick(Sender: TObject);
    procedure stopservButtonClick(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
  end;

var
  DT_P2PVM_CustomServerForm: TDT_P2PVM_CustomServerForm;
  PhysicsTunnel: TPhysicsServer;
  AuthServ: TDT_P2PVM_Custom_Service;

implementation

{$R *.dfm}


procedure TDT_P2PVM_CustomServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);

  PhysicsTunnel := TPhysicsServer.Create;

  AuthServ := TDT_P2PVM_Custom_Service.Create(TDTService, PhysicsTunnel, 'AuthR', '::', '100', 'AuthS', '::', '101');
  AuthServ.QuietMode := False;
  AuthServ.DTService.RootPath := TPath.GetLibraryPath;
  AuthServ.DTService.PublicPath := TPath.GetLibraryPath;
  AuthServ.DTService.LoadUserDB;
  AuthServ.DTService.AllowRegisterNewUser := False;
  AuthServ.DTService.AllowSaveUserInfo := True;
end;

procedure TDT_P2PVM_CustomServerForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
end;

procedure TDT_P2PVM_CustomServerForm.netTimerTimer(Sender: TObject);
begin
  CheckThread;
  AuthServ.Progress;
end;

procedure TDT_P2PVM_CustomServerForm.startservButtonClick(Sender: TObject);
begin
  AuthServ.StartService();
  if PhysicsTunnel.StartService('0.0.0.0', 11938) then
      DoStatus('Listening to physical IP%s:%d succeeded!', [TranslateBindAddr('0.0.0.0'), 11938]);
end;

procedure TDT_P2PVM_CustomServerForm.stopservButtonClick(Sender: TObject);
begin
  AuthServ.StopService();
  PhysicsTunnel.StopService;
end;

procedure TDT_P2PVM_CustomServerForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

procedure TDT_P2PVM_CustomServerForm.regUsrButtonClick(Sender: TObject);
begin
  if AuthServ.DTService.RegUser(UserEdit.Text, PasswdEdit.Text, nil) then
      DoStatus('register %s successed.', [UserEdit.Text])
  else
      DoStatus('register %s failed!', [UserEdit.Text]);
end;

end.
