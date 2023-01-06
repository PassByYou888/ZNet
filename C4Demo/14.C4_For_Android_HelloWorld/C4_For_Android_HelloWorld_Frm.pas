unit C4_For_Android_HelloWorld_Frm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Edit, FMX.StdCtrls, FMX.Layouts,

  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Notify,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_FS,
  Z.Net.C4_FS2,
  Z.Net.C4_UserDB,
  Z.Net.C4_Var,
  Z.Net.C4_Log_DB,
  Z.Net.C4_TEKeyValue,
  Z.Status,
  Z.Net.C4_Console_APP;

type
  TC4_For_Android_HelloWorld_Form = class(TForm, IC40_PhysicsTunnel_Event)
    Memo: TMemo;
    Layout1: TLayout;
    Run_TestButton: TButton;
    ip_Label: TLabel;
    ipEdit: TEdit;
    StyleBook1: TStyleBook;
    netTimer: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure netTimerTimer(Sender: TObject);
    procedure Run_TestButtonClick(Sender: TObject);
  private
    procedure backcall_DoStatus(Text_: SystemString; const ID: Integer);

    // C4 event
    procedure C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    procedure C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
  public
  end;

var
  C4_For_Android_HelloWorld_Form: TC4_For_Android_HelloWorld_Form;

const
  Internet_DP_Addr_ = '192.168.2.32';
  Internet_DP_Port_ = 8387;

implementation

{$R *.fmx}


procedure TC4_For_Android_HelloWorld_Form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RemoveDoStatusHook(Self);
  Z.Net.C4.C40Clean;
end;

procedure TC4_For_Android_HelloWorld_Form.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
end;

procedure TC4_For_Android_HelloWorld_Form.FormCreate(Sender: TObject);
begin
  Z.Net.C4.C40_QuietMode := False;
  AddDoStatusHook(Self, backcall_DoStatus);
  ipEdit.Text := Internet_DP_Addr_;
end;

procedure TC4_For_Android_HelloWorld_Form.netTimerTimer(Sender: TObject);
begin
  Z.Net.C4.C40Progress;
end;

procedure TC4_For_Android_HelloWorld_Form.Run_TestButtonClick(Sender: TObject);
begin
  with Z.Net.C4.C40_PhysicsTunnelPool.SearchServiceAndBuildConnection(
    Internet_DP_Addr_, Internet_DP_Port_, False, 'dp|FS|FS2|Var|UserDB|TEKeyValue|Log', Self) do
    begin
      OnDone_P := procedure(Done_ClientPool: TC40_Custom_ClientPool)
        var
          L: TStringList;
          i: Integer;
        begin
          L := TStringList.Create;
          for i := 0 to Done_ClientPool.Count - 1 do
              L.Add(Format('%s 已与服务器握手成功，服务器状态为最小负载', [Done_ClientPool[i].ClientInfo.ServiceTyp.Text]));

          SysPost.PostExecuteP(1.0, procedure(nSender: TN_Post_Execute)
            begin
              ShowMessage(TStringList(nSender.Data1).Text);
              DelayFreeObj(1.0, nSender.Data1);
            end).Data1 := L;
        end;
    end;
end;

procedure TC4_For_Android_HelloWorld_Form.backcall_DoStatus(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

procedure TC4_For_Android_HelloWorld_Form.C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
begin

end;

procedure TC4_For_Android_HelloWorld_Form.C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
begin

end;

procedure TC4_For_Android_HelloWorld_Form.C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin

end;

procedure TC4_For_Android_HelloWorld_Form.C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin

end;

end.
