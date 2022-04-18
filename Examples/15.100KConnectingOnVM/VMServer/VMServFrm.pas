unit VMServFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.TypInfo,
  Z.Net,
  Z.Net.IO,
  Z.Net.Server.ICS,
  Z.Net.Server.Indy,
  Z.Net.Server.CrossSocket,
  Z.PascalStrings, Z.Status, Z.Core, Z.DFE, Z.UnicodeMixedLib, Z.MemoryStream,
  Z.Notify, Z.Net.Test, Z.ListEngine;

type
  TMyServer = class(TZNet_Server_CrossSocket)
  protected
    procedure p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean); override;
    procedure p2pVMTunnelOpen(Sender: TPeerClient; p2pVMTunnel: TZNet_WithP2PVM); override;
    procedure p2pVMTunnelClose(Sender: TPeerClient; p2pVMTunnel: TZNet_WithP2PVM); override;
  end;

  TVMServForm = class(TForm)
    ProgressTimer: TTimer;
    Panel2: TPanel;
    StateMemo: TMemo;
    ReceiveMemo: TMemo;
    SendMemo: TMemo;
    CpuMemo: TMemo;
    Panel1: TPanel;
    VMListenButton: TButton;
    VMStopListenButton: TButton;
    CloseAllClientButton: TButton;
    ExecuteTestButton: TButton;
    Memo: TMemo;
    PrintStateTimer: TTimer;
    StatusCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure VMListenButtonClick(Sender: TObject);
    procedure VMStopListenButtonClick(Sender: TObject);
    procedure CloseAllClientButtonClick(Sender: TObject);
    procedure ExecuteTestButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PrintStateTimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ServTunnel: TMyServer;
    ServWithVM: TZNet_WithP2PVM_Server;
    ServWithVMTest: TCommunicationTestIntf;

    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  end;

var
  VMServForm: TVMServForm;

implementation

{$R *.dfm}

procedure TMyServer.p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TMyServer.p2pVMTunnelOpen(Sender: TPeerClient; p2pVMTunnel: TZNet_WithP2PVM);
begin
  inherited p2pVMTunnelOpen(Sender, p2pVMTunnel);
  p2pVMTunnel.InstallLogicFramework(VMServForm.ServWithVM);
end;

procedure TMyServer.p2pVMTunnelClose(Sender: TPeerClient; p2pVMTunnel: TZNet_WithP2PVM);
begin
  p2pVMTunnel.UninstallLogicFramework(VMServForm.ServWithVM);
  inherited p2pVMTunnelClose(Sender, p2pVMTunnel);
end;

procedure TVMServForm.CloseAllClientButtonClick(Sender: TObject);
begin
  ServWithVM.CloseAllClient;
end;

procedure TVMServForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  if StatusCheckBox.Checked then
      Memo.Lines.Add(AText);
end;

procedure TVMServForm.ExecuteTestButtonClick(Sender: TObject);
begin
  ServWithVM.ProgressPeerIOP(procedure(PeerClient: TPeerClient)
    begin
      ServWithVMTest.ExecuteAsyncTest(PeerClient);
    end);
end;

procedure TVMServForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
  ServTunnel := TMyServer.Create;
  ServTunnel.StartService('0.0.0.0', 9988);
  ServTunnel.QuietMode := True;

  ServWithVM := TZNet_WithP2PVM_Server.CustomCreate($FFFF, 88);
  ServWithVM.SwitchMaxPerformance;
  ServWithVM.QuietMode := False;
  ServWithVMTest := TCommunicationTestIntf.Create;
  ServWithVMTest.RegCmd(ServWithVM);
end;

procedure TVMServForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
  DisposeObject(ServWithVMTest);
  DisposeObject(ServWithVM);
  DisposeObject(ServTunnel);
end;

procedure TVMServForm.PrintStateTimerTimer(Sender: TObject);
  procedure PrintServerState(const arry: array of TZNet);
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

  procedure PrintServerCMDStatistics(const arry: array of TZNet);
  var
    RecvLst, SendLst: TCommand_Num_Hash_Pool;
    ExecuteConsumeLst: TCommand_Tick_Hash_Pool;
    comm: TZNet;
    i: Integer;
    lst: TPascalStringList;
  begin
    RecvLst := TCommand_Num_Hash_Pool.Create($FFFF, 0);
    SendLst := TCommand_Num_Hash_Pool.Create($FFFF, 0);
    ExecuteConsumeLst := TCommand_Tick_Hash_Pool.Create($FFFF, 0);
    for comm in arry do
      begin
        RecvLst.IncValue(comm.CmdRecvStatistics);
        SendLst.IncValue(comm.CmdSendStatistics);
        ExecuteConsumeLst.SetMax(comm.CmdMaxExecuteConsumeStatistics);
      end;

    lst := TPascalStringList.Create;
    RecvLst.GetKeyList(lst);
    ReceiveMemo.Lines.BeginUpdate;
    ReceiveMemo.Lines.Clear;
    ReceiveMemo.Lines.Add('Received commands...');
    for i := 0 to lst.Count - 1 do
        ReceiveMemo.Lines.Add(lst[i] + ' : ' + IntToStr(RecvLst[lst[i]]));
    ReceiveMemo.Lines.EndUpdate;
    DisposeObject(lst);

    lst := TPascalStringList.Create;
    SendLst.GetKeyList(lst);
    SendMemo.Lines.BeginUpdate;
    SendMemo.Lines.Clear;
    SendMemo.Lines.Add('Send commands...');
    for i := 0 to lst.Count - 1 do
        SendMemo.Lines.Add(lst[i] + ' : ' + IntToStr(SendLst[lst[i]]));
    SendMemo.Lines.EndUpdate;
    DisposeObject(lst);

    lst := TPascalStringList.Create;
    ExecuteConsumeLst.GetKeyList(lst);
    CpuMemo.Lines.BeginUpdate;
    CpuMemo.Lines.Clear;
    CpuMemo.Lines.Add('usage cpu...');
    for i := 0 to lst.Count - 1 do
        CpuMemo.Lines.Add(lst[i] + ' : ' + IntToStr(ExecuteConsumeLst[lst[i]]) + 'ms');
    CpuMemo.Lines.EndUpdate;
    DisposeObject(lst);

    DisposeObject([RecvLst, SendLst, ExecuteConsumeLst]);
  end;

begin
  PrintServerState([ServWithVM]);
  PrintServerCMDStatistics([ServWithVM]);
end;

procedure TVMServForm.ProgressTimerTimer(Sender: TObject);
var
  connectingcount, InitedCount: Integer;
begin
  ServTunnel.Progress;
  ServWithVM.Progress;

  connectingcount := 0;
  InitedCount := 0;
  ServWithVM.FastProgressPeerIOP(procedure(PeerClient: TPeerClient)
    begin
      if PeerClient.RemoteExecutedForConnectInit then
          inc(InitedCount)
      else
          inc(connectingcount);
    end);

  Caption := Format('VM���÷�����... ��ͻ��� %d �뿪���� %d ������ %d', [ServWithVM.Count, connectingcount, InitedCount]);
end;

procedure TVMServForm.VMListenButtonClick(Sender: TObject);
begin
  ServWithVM.StartService('::99', 11139);
  ServWithVM.StartService('::FF:F203:99', 11132);
  ServWithVM.StartService('::FF:F233:99', 21132);
  ServWithVM.StartService('::FF:F243:99', 31132);
  ServWithVM.StartService('::FF:F253:99', 41132);

  StatusCheckBox.Checked := False;
end;

procedure TVMServForm.VMStopListenButtonClick(Sender: TObject);
begin
  ServWithVM.StopService;
end;

end.
