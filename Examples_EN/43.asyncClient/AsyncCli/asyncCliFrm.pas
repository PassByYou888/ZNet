unit asyncCliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  Z.Core,
  Z.Status,
  Z.Cadencer, Z.DFE, Z.UnicodeMixedLib,
  Z.Net.PhysicsIO,
  Z.Net;

type
  TAsyncClientForm = class(TForm)
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
    client: TZNet_Client;
    procedure DoStatusNear(AText: string; const ID: Integer);
    procedure BackCall_helloWorld_Stream_Result(Sender: TPeerClient; ResultData: TDFE);
  public
  end;

var
  AsyncClientForm: TAsyncClientForm;

implementation

{$R *.dfm}


procedure TAsyncClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TAsyncClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  client := TPhysicsClient.Create;
end;

procedure TAsyncClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TAsyncClientForm.BackCall_helloWorld_Stream_Result(Sender: TPeerClient; ResultData: TDFE);
begin
  if ResultData.Count > 0 then
      DoStatus('server response:%s', [ResultData.Reader.ReadString]);
end;

procedure TAsyncClientForm.HelloWorldBtnClick(Sender: TObject);
begin
  {  Generally speaking, if tcompute.postp1 is used to avoid the main thread of the card, once the threading mechanism is used, the waitsendxxx method should be avoided on the client as much as possible  }
  TCompute.RunP(procedure(thSender: TCompute)
    var
      busy_: TAtomBool; {  Develop a state machine to detect whether asynchronous work has ended TAtom starts with thread safe variable state machines  }
      SendDe, ResultDE: TDFE;
    begin
      busy_ := TAtomBool.Create(True); {  Develop a state machine to detect whether asynchronous work has ended TAtom starts with thread safe variable state machines  }

      {  Throw the asynchronous method in the thread to the main thread  }
      {  Tthread.synchronize will get stuck in nested mode, which will not happen to tcompute.postxx  }
      {  Send a console form hello world instruction to the server  }
      client.SendDirectConsoleCmd('helloWorld_Console', '');

      {  Send a hello world instruction in the form of stream to the server  }
      client.SendDirectStreamCmd('helloWorld_Stream', TDFE.Create.WriteString('directstream 123456').DelayFree);

      {  Asynchronous sending and receiving Stream instructions, feedback triggered by method callback  }
      client.SendStreamCmdM('helloWorld_Stream_Result', TDFE.Create.WriteString('123456').DelayFree, BackCall_helloWorld_Stream_Result);

      {  Asynchronous sending and receiving Stream instructions, feedback triggered by proc callback  }
      client.SendStreamCmdP('helloWorld_Stream_Result', TDFE.Create.WriteString('123456').DelayFree, procedure(Sender: TPeerClient; ResultData: TDFE)
        begin
          if ResultData.Count > 0 then
              DoStatus('server response:%s', [ResultData.Reader.ReadString]);
          busy_.V := False;
        end);

      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('Thread ended');
    end);
end;

procedure TAsyncClientForm.SendBigStreamButtonClick(Sender: TObject);
begin
  {  Generally speaking, if tcompute.postp1 is used to avoid the main thread of the card, once the threading mechanism is used, the waitsendxxx method should be avoided on the client as much as possible  }
  TCompute.RunP(procedure(thSender: TCompute)
    var
      busy_: TAtomBool; {  Develop a state machine to detect whether asynchronous work has ended TAtom starts with thread safe variable state machines  }
      ms: TMemoryStream;
      p: PInt64;
      i: Integer;
    begin
      busy_ := TAtomBool.Create(True); {  Develop a state machine to detect whether asynchronous work has ended TAtom starts with thread safe variable state machines  }

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
      client.SendBigStream('Test128MBigStream', ms, True);
      {  Waitp is asynchronous and other feedback, because bigstream needs time to complete, we can't do busy_V operation  }
      client.WaitP(0, procedure(const wState: Boolean)
        begin
          busy_.V := False;
        end);

      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('Thread ended');
    end);
end;

procedure TAsyncClientForm.SendCompletebufferButtonClick(Sender: TObject);
begin
  {  Generally speaking, if tcompute.postp1 is used to avoid the main thread of the card, once the threading mechanism is used, the waitsendxxx method should be avoided on the client as much as possible  }
  TCompute.RunP(procedure(thSender: TCompute)
    var
      busy_: TAtomBool; {  Develop a state machine to detect whether asynchronous work has ended TAtom starts with thread safe variable state machines  }
      buff: Pointer;
      p: PInt64;
      i: Integer;
    begin
      busy_ := TAtomBool.Create(True); {  Develop a state machine to detect whether asynchronous work has ended TAtom starts with thread safe variable state machines  }
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
      client.SendCompleteBuffer('TestCompleteBuffer', buff, 16 * 1024 * 1024, True);
      {  WaitP provides asynchronous and other feedback, as SendCompleteBuffer takes time to complete and we cannot directly do busy_V operation  }
      client.WaitP(0, procedure(const wState: Boolean)
        begin
          busy_.V := False;
        end);

      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('Thread ended');
    end);
end;

procedure TAsyncClientForm.sendMiniStreamButtonClick(Sender: TObject);
begin
  {  Generally speaking, if tcompute.postp1 is used to avoid the main thread of the card, once the threading mechanism is used, the waitsendxxx method should be avoided on the client as much as possible  }
  TCompute.RunP(procedure(thSender: TCompute)
    var
      busy_: TAtomBool; {  Develop a state machine to detect whether asynchronous work has ended TAtom starts with thread safe variable state machines  }
      ms: TMemoryStream;
      SendDe: TDFE;
      p: PInt64;
      i: Integer;
    begin
      busy_ := TAtomBool.Create(True); {  Develop a state machine to detect whether asynchronous work has ended TAtom starts with thread safe variable state machines  }

      {  In SendDE, it contains 4M of large data, which is equivalent to executing 512 ordinary commands on the server side  }
      ms := TMemoryStream.Create;
      ms.SetSize(4 * 1024 * 1024);
      p := ms.Memory;
      for i := 1 to ms.Size div SizeOf(Int64) do
        begin
          p^ := Random(MaxInt);
          inc(p);
        end;
      DoStatus('mini stream md5:' + umlMD5Char(ms.Memory, ms.Size).Text);
      {  Send an instruction in the form of direct stream to the server  }
      SendDe := TDFE.Create;
      SendDe.WriteStream(ms);
      client.SendDirectStreamCmd('TestMiniStream', SendDe);
      DisposeObject([SendDe, ms]);

      {  Waitp is asynchronous and other feedback, because senddirectstreamcmd takes time to complete, we can't do busy_V operation  }
      client.WaitP(0, procedure(const wState: Boolean)
        begin
          busy_.V := False;
        end);

      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('Thread ended');
    end);
end;

procedure TAsyncClientForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  client.Progress;
end;

procedure TAsyncClientForm.ConnectButtonClick(Sender: TObject);
begin
  {  Zserver4d itself is an asynchronous framework. In most cases, it can be operated directly on the main thread  }
  {  Occasionally, we also need to use threads. Here we demonstrate the use of state machine mechanism+TCompute to complete work asynchronously in threads, which is better than using the WaitSendXXX method  }
  TCompute.RunP_NP(procedure
    var
      busy_: TAtomBool; {  Develop a state machine to detect whether asynchronous work has ended TAtom starts with thread safe variable state machines  }
    begin
      busy_ := TAtomBool.Create(True); {  Develop a state machine to detect whether asynchronous work has ended TAtom starts with thread safe variable state machines  }
      client.AsyncConnectP(HostEdit.Text, 9818, procedure(const cState: Boolean)
        begin
          if cState then
            begin
              DoStatus('Link successful');
              DoStatus('current client id: %d', [client.ClientIO.ID]);
            end
          else
              DoStatus('link failure');
          busy_.V := False;
        end);

      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('Thread ended');
    end);
end;

end.
