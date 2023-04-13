unit C4_Auto_Deployment_User_LoginFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,

  Vcl.FileCtrl,
  System.IOUtils, System.DateUtils, System.TypInfo,

  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status,
  Z.ListEngine, Z.HashList.Templet, Z.Expression, Z.OpCode, Z.Parsing, Z.DFE, Z.TextDataEngine,
  Z.Json, Z.Geometry2D, Z.Geometry3D, Z.Number,
  Z.MemoryStream, Z.Cipher, Z.Notify, Z.IOThread,
  Z.Net,
  Z.Net.C4, Z.Net.C4.VM,
  C4_Auto_Deployment_IMP_VM_Cli;

type
  Tuser_login_Form = class(TForm)
    netTimer: TTimer;
    logMemo: TMemo;
    _B_Splitter: TSplitter;
    JoinHostEdit: TLabeledEdit;
    JoinPortEdit: TLabeledEdit;
    userEdit: TLabeledEdit;
    passwdEdit: TLabeledEdit;
    reguserButton: TButton;
    loginUserButton: TButton;
    discButton: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure netTimerTimer(Sender: TObject);
    procedure loginUserButtonClick(Sender: TObject);
    procedure reguserButtonClick(Sender: TObject);
    procedure resetDependButtonClick(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
    ValidService: TC40_InfoList;
    MyClient: TAuto_Deployment_Client;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  user_login_Form: Tuser_login_Form;

implementation

{$R *.dfm}


procedure Tuser_login_Form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure Tuser_login_Form.netTimerTimer(Sender: TObject);
begin
  Z.Net.C4.C40Progress;
end;

procedure Tuser_login_Form.loginUserButtonClick(Sender: TObject);
begin
  if MyClient = nil then
      MyClient := TAuto_Deployment_Client.Create('');
  if MyClient.DTVirtualAuth.LinkOk then
      exit;
  MyClient.Client.RegisterUserAndLogin := False;
  MyClient.Connect_P(JoinHostEdit.Text, JoinPortEdit.Text, '123456', userEdit.Text, passwdEdit.Text, procedure(const State: Boolean)
    begin
      if State then
          ShowMessage('µÇÂ¼³É¹¦')
      else
          ShowMessage('µÇÂ¼Ê§°Ü');

      SysPost.PostExecuteC_NP(1.0, C40Clean_Client); // ÈÓ¸ö1ÃëÑÓ³ÙÊÂ¼þ,ÊÍ·Åc4
      MyClient := nil;
    end);
end;

procedure Tuser_login_Form.reguserButtonClick(Sender: TObject);
begin
  if MyClient = nil then
      MyClient := TAuto_Deployment_Client.Create('');
  if MyClient.DTVirtualAuth.LinkOk then
      exit;

  MyClient.Client.RegisterUserAndLogin := True;
  MyClient.Connect_P(JoinHostEdit.Text, JoinPortEdit.Text, '123456', userEdit.Text, passwdEdit.Text, procedure(const State: Boolean)
    begin
      if State then
          ShowMessage('×¢²á³É¹¦')
      else
          ShowMessage('×¢²áÊ§°Ü');

      SysPost.PostExecuteC_NP(1.0, C40Clean_Client); // ÈÓ¸ö1ÃëÑÓ³ÙÊÂ¼þ,ÊÍ·Åc4
      MyClient := nil;
    end);
end;

procedure Tuser_login_Form.resetDependButtonClick(Sender: TObject);
begin
  DisposeObjectAndNil(MyClient);
end;

procedure Tuser_login_Form.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  if logMemo.Lines.Count > 2000 then
      logMemo.Clear;
  logMemo.Lines.Add(DateTimeToStr(now) + ' ' + Text_);
end;

constructor Tuser_login_Form.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AddDoStatusHook(self, DoStatus_backcall);
  ValidService := TC40_InfoList.Create(True);
end;

destructor Tuser_login_Form.Destroy;
begin
  C40Clean;
  RemoveDoStatusHook(self);
  inherited Destroy;
end;

end.
