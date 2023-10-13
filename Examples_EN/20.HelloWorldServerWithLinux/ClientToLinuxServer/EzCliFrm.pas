unit EzCliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.Net,
  Z.Status, Z.Core,
  Z.Net.Client.CrossSocket,
  Z.Cadencer, Z.DFE, Z.UnicodeMixedLib,
  Vcl.Mask;

type
  TEZClientForm = class(TForm)
    Memo1: TMemo;
    ConnectButton: TButton;
    HostEdit: TLabeledEdit;
    Timer1: TTimer;
    HelloWorldBtn: TButton;
    sendMiniStreamButton: TButton;
    SendBigStreamButton: TButton;
    SendCompletebufferButton: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure HelloWorldBtnClick(Sender: TObject);
    procedure sendMiniStreamButtonClick(Sender: TObject);
    procedure SendBigStreamButtonClick(Sender: TObject);
    procedure SendCompletebufferButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
    procedure BackCall_helloWorld_Stream_Result(Sender: TPeerClient; ResultData: TDataFrameEngine);
  public
    { Public declarations }
    Client: TZNet_Client;
  end;

var
  EZClientForm: TEZClientForm;

implementation

{$R *.dfm}


procedure TEZClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TEZClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  Client := TZNet_Client_CrossSocket.Create;
end;

procedure TEZClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(Client);
  DeleteDoStatusHook(self);
end;

procedure TEZClientForm.BackCall_helloWorld_Stream_Result(Sender: TPeerClient; ResultData: TDataFrameEngine);
begin
  if ResultData.Count > 0 then
      DoStatus('server response:%s', [ResultData.Reader.ReadString]);
end;

procedure TEZClientForm.HelloWorldBtnClick(Sender: TObject);
var
  SendDe, ResultDE: TDataFrameEngine;
begin
  {  Send a console form hello world instruction to the server  }
  Client.SendDirectConsoleCmd('helloWorld_Console', '');

  {  Send a hello world instruction in the form of stream to the server  }
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('directstream 123456');
  Client.SendDirectStreamCmd('helloWorld_Stream', SendDe);
  DisposeObject([SendDe]);

  {  Asynchronous sending and receiving Stream instructions, feedback triggered by method callback  }
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  Client.SendStreamCmdM('helloWorld_Stream_Result', SendDe, BackCall_helloWorld_Stream_Result);
  DisposeObject([SendDe]);

  {  Asynchronous sending and receiving Stream instructions, feedback triggered by proc callback  }
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  Client.SendStreamCmdP('helloWorld_Stream_Result', SendDe,
      procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    begin
      if ResultData.Count > 0 then
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
    end);
  DisposeObject([SendDe]);

  {  Blocking sending and receiving Stream instructions  }
  SendDe := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  Client.WaitSendStreamCmd('helloWorld_Stream_Result', SendDe, ResultDE, 5000);
  if ResultDE.Count > 0 then
      DoStatus('server response:%s', [ResultDE.Reader.ReadString]);
  DisposeObject([SendDe, ResultDE]);
end;

procedure TEZClientForm.SendBigStreamButtonClick(Sender: TObject);
var
  ms: TMemoryStream;
  p: PInt64;
  i: Integer;
begin
  {  The MS contains 16m large data, which is equivalent to executing a common command on the server  }
  ms := TMemoryStream.Create;
  ms.SetSize(16 * 1024 * 1024);

  DoStatus('Create a 16M temporary big data stream');
  p := ms.Memory;
  for i := 1 to ms.Size div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('Calculate temporary big data stream md5');
  DoStatus('bigstream md5:' + umlMD5Char(ms.Memory, ms.Size).Text);

  {  Send a Big Stream instruction to the server  }
  Client.SendBigStream('Test128MBigStream', ms, True);
end;

procedure TEZClientForm.SendCompletebufferButtonClick(Sender: TObject);
var
  buff: Pointer;
  p: PInt64;
  i: Integer;
begin
  {  The MS contains 16m large data, which is equivalent to executing a common command on the server  }
  buff := GetMemory(16 * 1024 * 1024);

  DoStatus('Create 128M temporary big data stream');
  p := buff;
  for i := 1 to (16 * 1024 * 1024) div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('Calculate temporary big data stream md5');
  DoStatus('complete buffer md5:' + umlMD5String(buff, 16 * 1024 * 1024).Text);

  {  Send a CompleteBuffer instruction to the server  }
  {  The final Boolean parameter indicates whether to release the buff after completing the transmission  }
  Client.SendCompleteBuffer('TestCompleteBuffer', buff, 16 * 1024 * 1024, True);
end;

procedure TEZClientForm.sendMiniStreamButtonClick(Sender: TObject);
var
  ms: TMemoryStream;
  SendDe: TDataFrameEngine;
  p: PInt64;
  i: Integer;
begin
  {  In SendDE, 512k of large data is included, which is equivalent to executing 512 ordinary commands on the server side  }
  ms := TMemoryStream.Create;
  ms.SetSize(512 * 1024);

  p := ms.Memory;
  for i := 1 to ms.Size div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('mini stream md5:' + umlMD5Char(ms.Memory, ms.Size).Text);

  {  Send an instruction in the form of direct stream to the server  }
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteStream(ms);
  Client.SendDirectStreamCmd('TestMiniStream', SendDe);
  DisposeObject([SendDe, ms]);
end;

procedure TEZClientForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  Client.Progress;
end;

procedure TEZClientForm.ConnectButtonClick(Sender: TObject);
begin
  {  Method 1, Blocking Links  }
  // if client.Connect(HostEdit.Text, 9818) then
  {  Dostatus ('link succeeded ')  }
  // else
  {  DoStatus ('link failed ');  }

  {  Method 2, asynchronous high-speed link  }
  Client.AsyncConnectP(HostEdit.Text, 9818, procedure(const cState: Boolean)
    begin
      if cState then
        begin
          DoStatus('Link successful');
          DoStatus('current client id: %d', [Client.ClientIO.ID]);
        end
      else
          DoStatus('link failure');
    end);

end;

end.
