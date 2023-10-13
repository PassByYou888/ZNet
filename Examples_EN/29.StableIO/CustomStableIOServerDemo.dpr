program CustomStableIOServerDemo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
  Classes,
  Variants,
  Z.Core,
  Z.Status,
  Z.DFE,
  Z.PascalStrings,
  Z.ListEngine,
  Z.UnicodeMixedLib,
  Z.MemoryStream,
  Z.Net,
  Z.Net.Test,
  Z.Net.XNAT.Physics;

{  StableIO principle
StableIO intercepts the sending and receiving events of physical IO (any synapse, corss, diocp, indy, ico IO) and independently creates a stable IO called StableIO
StableIO is virtualized and does not behave like physical IO. After a disconnection, both the instance and the site are destroyed. StableIO is still working after the disconnection, and it is in an offline working mode.
When physical IO reconnects, StableIO will leave offline and enter online mode, restoring the scene
The server for StableIO must be a TCommunicationFramework_StableServer
The client for StableIO must be a TCommunicationFramework_StableClient
These two must be matched in order to perform automated disconnection reconnection and restore on-site capabilities
Ordinary clients can also directly use the StableIO server, but it does not have the function of disconnecting and reconnecting
Running StableIO demo makes it easier to test and verify using a physical network system  }

type
  TMyServer = class(TZNet_StableServer)
  public
    Test: TCommunicationTestIntf;
    constructor Create; override;
    destructor Destroy; override;
  end;

constructor TMyServer.Create;
begin
  inherited;
  Test := TCommunicationTestIntf.Create;
  Test.RegCmd(Self);
end;

destructor TMyServer.Destroy;
begin
  DisposeObject(Test);
  inherited;
end;

procedure MainLoop;
var
  MyServer: TMyServer;
  iostate, discard, recv, send, sequmem, n: string;
begin
  MyServer := TMyServer.Create;

  {  Important parameters  }
  {  We use xnat as the physical server for stable IO, because the author is lazy and doesn't want to define different communication interfaces used by various platforms  }
  MyServer.OwnerIOServer := TXPhysicsServer.Create;

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
  MyServer.LimitSequencePacketMemoryUsage := 0;

  {  Important parameters  }
  {  After the client goes offline, StableIO will activate the offline working mode, and all data sent to offline will be temporarily stored in memory  }
  {  This parameter is how long the client is allowed to go offline, in milliseconds  }
  {  If it is high-frequency network data transmission and reception, the accumulation time is too long, which will cause memory corruption  }
  {  We use the 5-minute offline limit here. Within 5 minutes after unplugging the network cable, reconnection can be restored to the scene  }
  MyServer.OfflineTimeout := 5 * 60 * 1000;

  {  be of no great importance  }
  {  When Myserver is released, the physical server txphysicsserver is automatically released  }
  MyServer.AutoFreeOwnerIOServer := True;

  {  be of no great importance  }
  {  During the Myserver main loop processing, the physical server txphysicsserver is also processed  }
  MyServer.AutoProgressOwnerIOServer := True;

  if MyServer.StartService('0.0.0.0', 11977) then
      DoStatus('stableIO listen success!');

  while True do
    begin
      MyServer.Progress;
{$IFDEF MSWINDOWS}
      {  Iobusy is an IO status checking mechanism applicable to any platform. When IO has data to process, it will return true  }
      if MyServer.IOBusy then
          iostate := 'Busy'
      else
          iostate := 'Idle';
      // SetConsoleTitle('Server IDLE');

      discard := Format(
        'discard: %d, size: %s', [MyServer.Statistics[TStatisticsType.stSequencePacketDiscard],
        umlSizeToStr(MyServer.Statistics[TStatisticsType.stSequencePacketDiscardSize]).Text]);

      recv := Format('received: %d', [MyServer.Statistics[TStatisticsType.stReceiveSize]]);
      send := Format('sending: %d', [MyServer.Statistics[TStatisticsType.stSendSize]]);
      sequmem := Format('swap memory: %s', [umlSizeToStr(MyServer.Statistics[TStatisticsType.stSequencePacketMemoryOnSending]).Text]);

      SetConsoleTitle(PWideChar(Format('%s - IO:%d PIO:%d - %s - %s - %s - %s',
        [iostate, MyServer.Count, MyServer.OwnerIOServer.Count, recv, send, discard, sequmem])));
{$ENDIF MSWINDOWS}
      Z.Core.CheckThreadSynchronize(10);
    end;
end;

begin
  try
      MainLoop;
  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
