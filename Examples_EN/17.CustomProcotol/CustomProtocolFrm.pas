unit CustomProtocolFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  Z.Net, Z.PascalStrings,
  Z.Net.Server.CrossSocket, Z.Net.Client.CrossSocket,
  Z.Status, Z.MemoryStream, Z.Core,

  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IDGlobal;

type
  {  TPeerClientUserSpecial is a special instance interface after each client's P2P connection  }
  {  We can also achieve the same function by inheriting TPeerClientUserDefine  }
  TMyPeerClientUserSpecial = class(TPeerClientUserSpecial)
  public
    myBuffer: TMemoryStream64;

    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;

    {  Here is the synchronization event, where we implement the fragmented buffer processing process for overlay writes  }
    procedure Progress; override;
  end;

  TMyServer = class(TZNet_Server_CrossSocket)
  public
    {  Get external customized processing buffer interface from server  }
    {  The buffers here are all fragmented buffers  }
    procedure OnReceiveBuffer(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean); override;
  end;

  TMyClient = class(TZNet_Client_CrossSocket)
  public
    myBuffer: TMemoryStream64;

    constructor Create; override;
    destructor Destroy; override;
    {  Get external customized processing buffer interface from server  }
    {  The buffers here are all fragmented buffers  }
    procedure OnReceiveBuffer(const buffer: PByte; const Size: NativeInt; var FillDone: Boolean); override;
  end;

  TCustomProtocolForm = class(TForm)
    Memo: TMemo;
    Timer: TTimer;
    Panel1: TPanel;
    connectOnIndyButton: TButton;
    SendDataOnIndyButton: TButton;
    IdTCPClient1: TIdTCPClient;
    connectOnZServerButton: TButton;
    SendDataOnZServerButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure connectOnIndyButtonClick(Sender: TObject);
    procedure connectOnZServerButtonClick(Sender: TObject);
    procedure SendDataOnIndyButtonClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SendDataOnZServerButtonClick(Sender: TObject);
  private
  public
    {  Custom Protocol Server  }
    myServer: TMyServer;

    {  Custom protocol client  }
    myClient: TMyClient;

    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  end;

var
  CustomProtocolForm: TCustomProtocolForm;

implementation

{$R *.dfm}


constructor TMyPeerClientUserSpecial.Create(AOwner: TPeerClient);
begin
  inherited;
  myBuffer := TMemoryStream64.Create;

end;

destructor TMyPeerClientUserSpecial.Destroy;
begin
  DisposeObject(myBuffer);

  inherited;
end;

procedure TMyPeerClientUserSpecial.Progress;
begin
  inherited;
  {  Here is the synchronization event, where we implement the fragmented buffer processing process for overlay writes  }

  {  First check whether the buffer is empty  }
  if myBuffer.Size > 0 then
    begin
      {  If the buffer is not empty  }
      {  We print the received data content  }
      DoStatus(format('receive [%s] [%d] ', [Owner.PeerIP, Owner.ID]), myBuffer.Memory, myBuffer.Size, 16);
      {  We feed back the received data to the sender intact  }
      Owner.WriteCustomBuffer(myBuffer.Memory, myBuffer.Size);

      {  Clear the buffer to prepare for the next processing  }
      myBuffer.Clear;
    end;
end;

procedure TMyServer.OnReceiveBuffer(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
begin
  {  The Role of FillDone  }
  {  If filldone is true, the kernel will think that you have processed the buffer. After exiting the event, it will not process it again  }
  {  If filldone is false, the kernel will handle it according to the normal mechanism of ZS after exiting the event, including dragging the secret key and decompressing it  }
  {  Filldone is true when protocol: = cpcustom. If protocol: = cpzserver, this event will not be triggered  }

  {  Get external customized processing buffer interface from server  }
  {  The buffers here are all fragmented buffers  }
  {  We will append the fragment buffer to myBuffer  }
  TMyPeerClientUserSpecial(Sender.UserSpecial).myBuffer.WritePtr(buffer, Size);
end;

constructor TMyClient.Create;
begin
  inherited Create;
  myBuffer := TMemoryStream64.Create;
end;

destructor TMyClient.Destroy;
begin
  DisposeObject(myBuffer);
  inherited Destroy;
end;

procedure TMyClient.OnReceiveBuffer(const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
begin
  {  The Role of FillDone  }
  {  If filldone is true, the kernel will think that you have processed the buffer. After exiting the event, it will not process it again  }
  {  If filldone is false, the kernel will handle it according to the normal mechanism of ZS after exiting the event, including dragging the secret key and decompressing it  }
  {  Filldone is true when protocol: = cpcustom. If protocol: = cpzserver, this event will not be triggered  }

  {  Get external customized processing buffer interface from server  }
  {  The buffers here are all fragmented buffers  }
  {  We will append the fragment buffer to myBuffer  }
  myBuffer.WritePtr(buffer, Size);
end;

procedure TCustomProtocolForm.connectOnIndyButtonClick(Sender: TObject);
begin
  IdTCPClient1.Connect;

  if IdTCPClient1.Connected then
      DoStatus('connect on indy ok!');
end;

procedure TCustomProtocolForm.connectOnZServerButtonClick(Sender: TObject);
begin
  myClient.AsyncConnectP('127.0.0.1', 9989, procedure(const cState: Boolean)
    begin
      if cState then
          DoStatus('ZServer custom client connection successful');
    end);
end;

procedure TCustomProtocolForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(AText);
end;

procedure TCustomProtocolForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DisposeObject(myServer);
  DisposeObject(myClient);
end;

procedure TCustomProtocolForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
  myServer := TMyServer.Create;
  {  Using custom communication protocols  }
  myServer.Protocol := cpCustom;
  {  Specify the P2P instance interface for the client  }
  myServer.PeerClientUserSpecialClass := TMyPeerClientUserSpecial;
  myServer.StartService('', 9989);

  myClient := TMyClient.Create;
  myClient.Protocol := cpCustom;
end;

procedure TCustomProtocolForm.TimerTimer(Sender: TObject);
var
  iBuf: TIdBytes;
begin
  CheckThread;
  myServer.Progress;
  myClient.Progress;

  if IdTCPClient1.Connected then
    begin
      {  Check feedback from the server  }
      if IdTCPClient1.IOHandler.InputBuffer.Size > 0 then
        begin
          {  When we receive feedback, we print the feedback from the server  }
          IdTCPClient1.IOHandler.InputBuffer.ExtractToBytes(iBuf);
          IdTCPClient1.IOHandler.InputBuffer.Clear;
          DoStatus(format('response ', []), @iBuf[0], length(iBuf), 16);
        end;
    end;

  if myClient.Connected then
    begin
      if myClient.myBuffer.Size > 0 then
        begin
          DoStatus(format('response ', []), myClient.myBuffer.Memory, myClient.myBuffer.Size, 16);
          myClient.myBuffer.Clear;
        end;
    end;
end;

procedure TCustomProtocolForm.SendDataOnIndyButtonClick(Sender: TObject);
var
  d: UInt64;
begin
  d := ($ABCDEF1234567890);
  {  We use the indy interface to send a uint variable to the server  }
  IdTCPClient1.IOHandler.WriteBufferOpen;
  {  It should be noted that if the conversion parameter is turned on, Indy uses a large end byte order conversion (earlier versions of Indy were designed to be compatible with non Intel architectures), so we need to turn off the conversion  }
  IdTCPClient1.IOHandler.Write(d, False);
  IdTCPClient1.IOHandler.WriteBufferFlush;
  IdTCPClient1.IOHandler.WriteBufferClose;
end;

procedure TCustomProtocolForm.SendDataOnZServerButtonClick(Sender: TObject);
var
  d: UInt64;
begin
  d := ($ABCDEF1234567890);
  myClient.BeginWriteBuffer;
  myClient.WriteBuffer(@d, SizeOf(d));
  myClient.EndWriteBuffer;
end;

end.
