unit XNATMobileDeviceFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, System.TypInfo,
  FMX.TabControl, FMX.Edit, FMX.Layouts, FMX.ListBox,
  Z.Core, Z.PascalStrings, Z.UnicodeMixedLib, Z.Net,
  Z.Net.XNAT.MappingOnVirutalService, Z.Net.XNAT.Physics, Z.Net.Test, Z.Status, Z.Notify,
  FMX.Memo.Types;

type
  TXNATMobileDeviceForm = class(TForm)
    Timer1: TTimer;
    OpenButton: TButton;
    InfoLabel: TLabel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    LogMemo: TMemo;
    Layout1: TLayout;
    HostEdit: TEdit;
    Label1: TLabel;
    LogCheckBox: TCheckBox;
    InfoListBox: TListBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    XCli: TXNAT_VS_Mapping;
    server: TZNet_Server;
    server_test: TCommunicationTestIntf;
    procedure DoStatusIntf(AText: SystemString; const ID: Integer);
  end;

var
  XNATMobileDeviceForm: TXNATMobileDeviceForm;

implementation

{$R *.fmx}


procedure TXNATMobileDeviceForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusIntf);

  XCli := TXNAT_VS_Mapping.Create;

  {
    ��͸Э��ѹ��ѡ��
    ����ʹ�ó���:
    �������������Ѿ�ѹ����������ʹ��https���෽ʽ���ܹ���ѹ������Ч������ѹ�������ݸ���
    ���ʱ������Э�飬����ftp,����s��http,tennet��ѹ�����ؿ��Դ򿪣�����С������
  }
  XCli.ProtocolCompressed := True;

  server := XCli.AddMappingServer('my18888', 5); // ��������������18888�˿ڷ��������Ϊ���ط���������������ֻ��1��

  // server.OfflineTimeout := 3 * 60 * 1000;        // �������������������ߺ�3���ӾͶϿ�stableIO��ʵ��
  // server.PhysicsServer.TimeOutIDLE := 60 * 1000; // ����ͻ���60������Ӧ��������״̬

  server.QuietMode := False;

  server_test := TCommunicationTestIntf.Create;
  server_test.RegCmd(server);
end;

procedure TXNATMobileDeviceForm.DoStatusIntf(AText: SystemString; const ID: Integer);
begin
  if not LogCheckBox.IsChecked then
      exit;
  LogMemo.Lines.Add(AText);
  LogMemo.GoToTextEnd;
end;

procedure TXNATMobileDeviceForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  while server.Count > 0 do
      DisposeObject(server.FirstIO);
  DisposeObject(XCli);
end;

procedure TXNATMobileDeviceForm.OpenButtonClick(Sender: TObject);
begin
  XCli.Host := HostEdit.Text; // ������������IP
  XCli.Port := '7890';        // �����������Ķ˿ں�
  XCli.AuthToken := '123456'; // Э����֤�ַ���
  // ����������͸
  // ��������������͸�������󣬱��ط��������Զ�StartService�����ط��������������κζ˿�
  XCli.OpenTunnel;
end;

procedure TXNATMobileDeviceForm.Timer1Timer(Sender: TObject);
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

    while InfoListBox.Count < Ord(high(TStatisticsType)) + 1 do
        TListBoxItem.Create(InfoListBox).Parent := InfoListBox;

    for st := low(TStatisticsType) to high(TStatisticsType) do
      begin
        v := buff[st];
        n := IntToStr(v);
        InfoListBox.ListItems[Ord(st)].Text := GetEnumName(TypeInfo(TStatisticsType), Ord(st)) + ' : ' + n;
      end;
  end;

begin
  if XCli <> nil then
    begin
      InfoLabel.Text := PFormat('connection: %d' + #13#10 + 'physics: %d', [server.Count, server.Count]);
      XCli.Progress;
      server.Progress;
    end;
  PrintServerState([server]);
end;

end.
