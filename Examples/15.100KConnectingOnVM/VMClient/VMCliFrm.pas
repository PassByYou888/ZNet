unit VMCliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.TypInfo,
  Z.Net,
  Z.Net.IO,
  Z.Net.Server.ICS,
  Z.Net.Server.Indy,
  Z.Net.Server.CrossSocket,
  Z.Net.Client.ICS,
  Z.Net.Client.Indy,
  Z.Net.Client.CrossSocket,
  Z.PascalStrings, Z.Status, Z.Core, Z.DFE, Z.UnicodeMixedLib, Z.MemoryStream,
  Z.Notify, Z.Net.Test, Z.ListEngine;

const
  MaxClient = 100000;

type
  TMyClient = class(TZNet_WithP2PVM_Client)
  protected
  end;

  TClientArry = array [0 .. MaxClient - 1] of TMyClient;
  TTestArry = array [0 .. MaxClient - 1] of TCommunicationTestIntf;

  TVMCliForm = class(TForm)
    ProgressTimer: TTimer;
    Panel1: TPanel;
    AddrEdit: TLabeledEdit;
    CreateVMButton: TButton;
    VMAddrEdit: TLabeledEdit;
    ConnectVMButton: TButton;
    TestButton: TButton;
    DisconnectButton: TButton;
    Memo: TMemo;
    StateMemo: TMemo;
    PrintStateTimer: TTimer;
    StatusCheckBox: TCheckBox;
    OriginDataLabel: TLabel;
    MaxTestButton: TButton;
    procedure CreateVMButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure ConnectVMButtonClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PrintStateTimerTimer(Sender: TObject);
    procedure MaxTestButtonClick(Sender: TObject);
  private
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
    { Private declarations }
  public
    { Public declarations }
    ClientTunnel: TZNet_Client_ICS;
    ClientWithVM: TClientArry;
    ClientWithVMTest: TTestArry;

    procedure cmd_SimulateKeepAlivte(Sender: TPeerIO; InData: TDFE);
  end;

var
  VMCliForm: TVMCliForm;

implementation

{$R *.dfm}


procedure TVMCliForm.CreateVMButtonClick(Sender: TObject);
begin
  // VM.CloseP2PVMTunnel;
  ClientTunnel.AsyncConnectP(AddrEdit.Text, 9988, procedure(const cState: Boolean)
    begin
      DoStatus('VM���������...');
      if cState then
        begin
          DoStatus('VM��������...');
          ClientTunnel.ClientIO.BuildP2PAuthTokenP(procedure
            begin
              ClientTunnel.ClientIO.OpenP2PVMTunnelP(10000 * 10, True, '',
                procedure(const vState: Boolean)
                var
                  i: Integer;
                begin
                  ClientTunnel.ClientIO.p2pVMTunnel.MaxVMFragmentSize := 8192;
                  ClientTunnel.ClientIO.p2pVMTunnel.QuietMode := False;

                  for i := low(ClientWithVM) to high(ClientWithVM) do
                    begin
                      ClientWithVM[i].QuietMode := False;
                      ClientTunnel.ClientIO.p2pVMTunnel.InstallLogicFramework(ClientWithVM[i]);
                    end;

                  DoStatus('VM������');
                end);
            end);
        end;
    end);
end;

procedure TVMCliForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  AddDoStatusHook(Self, DoStatusMethod);
  ClientTunnel := TZNet_Client_ICS.Create;
  ClientTunnel.QuietMode := True;
  ClientTunnel.RegisterDirectStream('SimulateKeepAlivte').OnExecute := cmd_SimulateKeepAlivte;

  for i := low(ClientWithVM) to high(ClientWithVM) do
    begin
      ClientWithVM[i] := TMyClient.CustomCreate(i + 99);
      ClientWithVM[i].SwitchMaxPerformance;
      ClientWithVMTest[i] := TCommunicationTestIntf.Create;
      ClientWithVMTest[i].RegCmd(ClientWithVM[i]);
    end;
end;

procedure TVMCliForm.ProgressTimerTimer(Sender: TObject);
var
  i: Integer;
  TotalCli, connectingCli, ConnectedCli: Integer;
begin
  CheckThread(10);
  ClientTunnel.Progress;

  for i := low(ClientWithVM) to high(ClientWithVM) do
      ClientWithVM[i].Progress;

  TotalCli := 0;
  connectingCli := 0;
  ConnectedCli := 0;

  if ClientTunnel.Connected then
    if ClientTunnel.ClientIO.p2pVMTunnel <> nil then
        ClientTunnel.ClientIO.p2pVMTunnel.ProgressZNet_P(procedure(PeerFramework: TZNet)
        begin
          if (PeerFramework is TZNet_WithP2PVM_Client) then
            begin
              inc(TotalCli);

              if TZNet_WithP2PVM_Client(PeerFramework).RemoteInited then
                  inc(ConnectedCli)
              else if TZNet_WithP2PVM_Client(PeerFramework).Connected then
                  inc(connectingCli);
            end;
        end);

  Caption := Format('VM�ͻ���(%d)...�뿪������(%d) �������(%d)', [TotalCli, connectingCli, ConnectedCli]);
end;

procedure TVMCliForm.ConnectVMButtonClick(Sender: TObject);
var
  i: Integer;
  cCount: Integer;
begin
  if not ClientTunnel.Connected then
      exit;

  StatusCheckBox.Checked := False;

  ClientTunnel.ClientIO.Progress;
  for i := low(ClientWithVM) to high(ClientWithVM) do
    begin
      ClientWithVM[i].AsyncConnectTimeout := 10 * 60 * 1000;
      ClientWithVM[i].AsyncConnect(VMAddrEdit.Text, 11139);
    end;
end;

procedure TVMCliForm.TestButtonClick(Sender: TObject);
var
  i: Integer;
begin
  if not ClientTunnel.Connected then
      exit;
  for i := low(ClientWithVM) to high(ClientWithVM) do
      ClientWithVMTest[i].ExecuteAsyncTest(ClientWithVM[i].ClientIO);
end;

procedure TVMCliForm.DisconnectButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := low(ClientWithVM) to high(ClientWithVM) do
      ClientWithVM[i].Disconnect;
end;

procedure TVMCliForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  DeleteDoStatusHook(Self);
  for i := low(ClientWithVM) to high(ClientWithVM) do
    begin
      DisposeObject(ClientWithVMTest[i]);
      DisposeObject(ClientWithVM[i]);
    end;
  DisposeObject(ClientTunnel);
end;

procedure TVMCliForm.MaxTestButtonClick(Sender: TObject);
var
  i: Integer;
begin
  if not ClientTunnel.Connected then
      exit;
  for i := low(ClientWithVM) to high(ClientWithVM) do
      ClientWithVMTest[i].ExecuteAsyncTestWithBigStream(ClientWithVM[i].ClientIO);
end;

procedure TVMCliForm.PrintStateTimerTimer(Sender: TObject);
  procedure PrintServerState(var arry: TClientArry);
  var
    buff: array [TStatisticsType] of Int64;
    comm: TZNet;
    st: TStatisticsType;
    i: Integer;
    v: Int64;
    n: string;
  begin
    for st := low(TStatisticsType) to high(TStatisticsType) do
        buff[st] := 0;

    for comm in arry do
      begin
        for st := low(TStatisticsType) to high(TStatisticsType) do
            buff[st] := buff[st] + comm.Statistics[st];
      end;

    StateMemo.Lines.BeginUpdate;
    StateMemo.Lines.Clear;
    StateMemo.Lines.Add('Statistics...');
    for st := low(TStatisticsType) to high(TStatisticsType) do
      begin
        v := buff[st];
        if v > 8192 then
            n := umlSizeToStr(v).Text
        else
            n := IntToStr(v);
        StateMemo.Lines.Add(GetEnumName(TypeInfo(TStatisticsType), Ord(st)) + ' : ' + n);
      end;
    StateMemo.Lines.EndUpdate;
  end;

begin
  PrintServerState(ClientWithVM);
end;

procedure TVMCliForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  if StatusCheckBox.Checked then
      Memo.Lines.Add(AText);
end;

procedure TVMCliForm.cmd_SimulateKeepAlivte(Sender: TPeerIO; InData: TDFE);
begin
  OriginDataLabel.Caption := Format('������:%s', [InData.Reader.ReadString]);
end;

end.
