unit VirtualServFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  Z.Core, Z.PascalStrings, Z.UnicodeMixedLib, Z.Net,
  Z.Net.XNAT.MappingOnVirutalService, Z.Net.XNAT.Physics, Z.Net.Test, Z.Status, Z.Notify,
  FMX.Memo.Types;

type
  TVirtualServForm = class(TForm)
    Memo1: TMemo;
    netTimer: TTimer;
    TestButton: TButton;
    OpenButton: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure netTimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    XCli: TXNAT_VS_Mapping;
    server: TZNet_Server;
    server_test: TCommunicationTestIntf;

    {  Simulator test client  }
    {  We can open two apps for simulation testing to facilitate direct built-in implementation  }
    client: TZNet_Client;
    client_test: TCommunicationTestIntf;
    procedure DoStatusIntf(AText: SystemString; const ID: Integer);
  end;

var
  VirtualServForm: TVirtualServForm;

implementation

{$R *.fmx}


procedure TVirtualServForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusIntf);

  XCli := TXNAT_VS_Mapping.Create;

  {  Penetration protocol compression options
Recommended usage scenarios:
If the agent's data has been compressed or encrypted using HTTPS, the compression will be invalid, and even the compressed data will be larger
If it is a bare data protocol, such as FTP, HTTP without s, TenneT, the compression switch can be turned on and the speed can be increased slightly  }
  XCli.ProtocolCompressed := True;

  XCli.Host := '127.0.0.1';                         {  IP address of public network server  }
  XCli.Port := '7890';                              {  The port number of the public server  }
  XCli.AuthToken := '123456';                       {  Protocol Validation String  }
  server := XCli.AddMappingServer('web8000', 1000); {  Reverse proxy the 8000 port of the public server to become a local server  }

  server_test := TCommunicationTestIntf.Create;
  server_test.RegCmd(server);

  client := TXPhysicsClient.Create;
  client_test := TCommunicationTestIntf.Create;
  client_test.RegCmd(client);
end;

procedure TVirtualServForm.DoStatusIntf(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TVirtualServForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  {  First, physically disconnect the client  }
  client.Disconnect;
  while client.RemoteInited do
      client.Progress;

  {  Pay attention to reading the nested relationship of the main loop  }
  {  If progress has been done in XCli, it will automatically call the vs server member progress  }
  server.StopService;
  while server.Count > 0 do
      XCli.Progress;

  DisposeObject(client);
  DisposeObject(XCli);
  DisposeObject(server_test);
  DisposeObject(client_test);
  DeleteDoStatusHook(Self);
end;

procedure TVirtualServForm.OpenButtonClick(Sender: TObject);
begin
  {  Start intranet penetration  }
  {  After starting the intranet penetration server, the local server will automatically start Service and will not listen to any ports  }
  XCli.OpenTunnel;
end;

procedure TVirtualServForm.TestButtonClick(Sender: TObject);
begin
  {  Simulation test: Connect to a public network server  }
  if client.RemoteInited then {  If connected, directly open the test function  }
      client_test.ExecuteAsyncTestWithBigStream(client.ClientIO)
  else
    begin
      {  Not connected, create a new physical at this time  }
      {  This is a built-in client access to the built-in server, and the client has a blocking mechanism. Pay attention to dead loops  }
      {  The method of avoiding dead loop directly uses asynchronous mode  }
      client.AsyncConnectP('127.0.0.1', 8000, procedure(const cState: Boolean)
        begin
          if cState then
            begin
              client_test.ExecuteAsyncTestWithBigStream(client.ClientIO);
            end;
        end);
    end;
end;

procedure TVirtualServForm.netTimerTimer(Sender: TObject);
begin
  CheckThread(5);
  if XCli <> nil then
    begin
      XCli.Progress;
    end;
  client.Progress;
end;

end.
