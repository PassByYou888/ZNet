unit MobileFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  Z.Core, Z.PascalStrings, Z.UnicodeMixedLib, Z.Net,
  Z.Net.XNAT.Physics, Z.Net.XNAT.Client, Z.Status, FMX.Memo.Types;

type
  TMobileForm = class(TForm)
    Memo1: TMemo;
    netTimer: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure netTimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    XCli: TXNATClient;
    procedure DoStatusIntf(AText: SystemString; const ID: Integer);
  end;

var
  MobileForm: TMobileForm;

implementation

{$R *.fmx}


procedure TMobileForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusIntf);

  XCli := TXNATClient.Create;

  {
    ��͸Э��ѹ��ѡ��
    ����ʹ�ó���:
    �������������Ѿ�ѹ����������ʹ��https���෽ʽ���ܹ���ѹ������Ч������ѹ�������ݸ���
    �����������Э�飬����ftp,����s��http,tennet��ѹ�����ؿ��Դ򿪣�����С������
  }
  XCli.ProtocolCompressed := True;

  XCli.Host := '127.0.0.1';                           // ������������IP
  XCli.Port := '7890';                                // �����������Ķ˿ں�
  XCli.AuthToken := '123456';                         // Э����֤�ַ���
  XCli.AddMapping('127.0.0.1', '80', 'web8000', 100); // ��������������8000�˿ڷ����������80�˿�
  XCli.OpenTunnel;                                    // ����������͸
end;

procedure TMobileForm.DoStatusIntf(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToLineEnd;
end;

procedure TMobileForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DisposeObject(XCli);
end;

procedure TMobileForm.netTimerTimer(Sender: TObject);
begin
  if XCli <> nil then
      XCli.Progress;
end;

end.
