unit CustomStableIOClientOnFMXMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox, FMX.Memo,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  Z.Core, Z.ZDB.Engine, Z.ZDB.LocalManager, Z.Status, Z.DFE, Z.PascalStrings,
  Z.ListEngine, Z.UnicodeMixedLib, Z.MemoryStream,
  Z.Net, Z.Net.Test, Z.Net.XNAT.Physics, FMX.Memo.Types;

type
  TMyClient = class(TZNet_StableClient)
  protected
    procedure DoConnected(Sender: TPeerIO); override;
    procedure DoDisconnect(Sender: TPeerIO); override;
  public
    Test: TCommunicationTestIntf;
    constructor Create; override;
    destructor Destroy; override;
    function isOffline: Boolean;
  end;

  TForm1 = class(TForm)
    Layout1: TLayout;
    Label1: TLabel;
    HostEdit: TEdit;
    connectButton: TButton;
    Memo1: TMemo;
    RunTestButton: TButton;
    DisconnectButton: TButton;
    Timer1: TTimer;
    IOStateLabel: TLabel;
    InfoLabel: TLabel;
    procedure connectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure RunTestButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MyClient: TMyClient;
    procedure backcall_DoStatus(AText: SystemString; const ID: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


constructor TMyClient.Create;
begin
  inherited;
  Test := TCommunicationTestIntf.Create;
  Test.RegCmd(Self);
end;

destructor TMyClient.Destroy;
begin
  DisposeObject(Test);
  inherited;
end;

procedure TMyClient.DoConnected(Sender: TPeerIO);
begin
  inherited;
end;

procedure TMyClient.DoDisconnect(Sender: TPeerIO);
begin
  inherited;
end;

function TMyClient.isOffline: Boolean;
begin
  Result := (not StableClientIO.Activted) or
    ((StableClientIO.Activted) and (StableClientIO.WaitConnecting));
end;

procedure TForm1.backcall_DoStatus(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TForm1.connectButtonClick(Sender: TObject);
begin
  MyClient.AsyncConnectP(HostEdit.Text, 11977, procedure(const cState: Boolean)
    begin
    end);
end;

procedure TForm1.DisconnectButtonClick(Sender: TObject);
begin
  MyClient.Disconnect;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DisposeObject(MyClient);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, backcall_DoStatus);
  MyClient := TMyClient.Create;

  {  Important parameters  }
  {  We use XNAT as the physical client for StableIO, lazy and unwilling to define different communication interfaces used by various platforms  }
  MyClient.OwnerIOClient := TXPhysicsClient.Create;

  {  Important parameters  }
  {  When the client connects to the server, it will automatically enter the working mode of the sequence package  }
  {  The sequence package will use 2-3 times the memory under normal conditions  }
  {  The function of LimitSequencePacketMemoryUsage is to limit the memory usage of sequence packet working modes  }
  {  0 represents unlimited memory usage, 1024 represents a limit of 1kb of memory  }
  {  If you want to limit it, it is recommended to give 64 * 1024 * 1024 on the server side, which means that the maximum limit is to maintain the 64M sequence table memory cost. If the client can directly not limit it  }
  {  When the limit is exceeded, the virtualized IO connection will be forcibly closed and memory will be reclaimed  }
  {  Under normal circumstances, only 2 scenarios will heavily utilize sequence pack memory  }
  {  1： In offline mode, because the data sent cannot reach the target, it will accumulate in memory until it exceeds the memory security limit or reaches the offline time limit  }
  {  2： In high-speed networks, such as local to local or Gigabit, when sending and receiving large completebuffer and bigstream, the sequence packet communication mode will use 2-3 times the normal memory overhead  }
  {  Using the stable IO mechanism, it is recommended to use a 64 bit operating system, windows or Linux, and ensure that sufficient memory is configured  }
  MyClient.LimitSequencePacketMemoryUsage := 0;

  {  be of no great importance  }
  {  MyClient fails to connect for the first time and will continue to retry, which is done asynchronously and automatically in the background  }
  MyClient.AutomatedConnection := True;

  {  be of no great importance  }
  {  Automatically release the physical server TXPhysicsClient when MyClient is released  }
  MyClient.AutoFreeOwnerIOClient := True;

  {  be of no great importance  }
  {  When processing the MyClient main loop, it also handles the physical server TXPhysicsClient  }
  MyClient.AutoProgressOwnerIOClient := True;
end;

procedure TForm1.RunTestButtonClick(Sender: TObject);
begin
  if MyClient.Connected then
      MyClient.Test.ExecuteAsyncTestWithBigStream(MyClient.ClientIO);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  discard, recv, send, sequmem: string;
begin
  CheckThread;
  MyClient.Progress;

  {  Iobusy is an IO status checking mechanism applicable to any platform. When IO has data to process, it will return true  }
  if MyClient.IOBusy then
      IOStateLabel.Text := 'IO Busy...'
  else
      IOStateLabel.Text := 'IO IDLE';

  discard := Format(
    'discard: %d, size: %s', [MyClient.Statistics[TStatisticsType.stSequencePacketDiscard],
    umlSizeToStr(MyClient.Statistics[TStatisticsType.stSequencePacketDiscardSize]).Text]);

  recv := Format('received: %d', [MyClient.Statistics[TStatisticsType.stReceiveSize]]);
  send := Format('sending: %d', [MyClient.Statistics[TStatisticsType.stSendSize]]);
  sequmem := Format('swap memory: %s', [umlSizeToStr(MyClient.ClientIO.SequencePacketUsagePhysicsMemory).Text]);

  InfoLabel.Text := Format('%s'#13#10'%s'#13#10'%s'#13#10'%s'#13#10'StopCommunicationTimeTick: %f ',
    [recv, send, discard, sequmem, MyClient.StopCommunicationTimeTick * 0.001]);
end;

end.
