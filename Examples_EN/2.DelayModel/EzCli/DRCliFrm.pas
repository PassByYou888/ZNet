unit DRCliFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.Net,
  Z.Status, Z.Core,
  Z.Net.Client.CrossSocket,
  Z.Net.Client.ICS,
  Z.Net.Client.Indy,
  Z.Cadencer, Z.DFE;

type
  TDRClientForm = class(TForm)
    Memo1: TMemo;
    ConnectButton: TButton;
    HostEdit: TLabeledEdit;
    Timer1: TTimer;
    DelayResponseBtn: TButton;
    DelayResponse2Btn: TButton;
    procedure DelayResponse2BtnClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure DelayResponseBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    client: TZNet_Client_CrossSocket;
  end;

var
  DRClientForm: TDRClientForm;

implementation

{$R *.dfm}


procedure TDRClientForm.DelayResponse2BtnClick(Sender: TObject);
type
  TMyDefine = record
    a, b, c: Integer;
  end;

  PMyDefine = ^TMyDefine;

var
  SendDe: TDataFrameEngine;
  p: PMyDefine;
begin
  {  Due to asynchronous operations, it is often difficult for clients to write using normal processes, so we often need to use exchange structures  }
  {  Pmydefine is a switching structure, which maintains the data consistency of asynchronous programs  }
  new(p);
  p^.a := 1;
  p^.b := 2;
  p^.c := 3;

  SendDe := TDataFrameEngine.Create;
  client.SendStreamCmdP('DelayResponse', SendDe, p, nil,
    procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: TDataFrameEngine)
    var
      p2: PMyDefine;
    begin
      {  This event is triggered if the client is not offline and receives feedback from the server  }
      {  When this event is triggered, the call of delayresponse2btnclick has ended. At this time, we cannot directly access the P variable because the stack has been destroyed. We need to retrieve the pointer data of pmydefine to P2  }

      p2 := Param1;

      DoStatus('a:%d', [p2^.a]);
      DoStatus('b:%d', [p2^.b]);
      DoStatus('c:%d', [p2^.c]);

      while ResultData.Reader.NotEnd do
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);

      {  If the other party goes offline, this event will not be triggered, and the PMyDefine memory we just applied for will also be lost  }
      dispose(p2);
    end,
    procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDataFrameEngine)
    var
      p2: PMyDefine;
    begin
      p2 := Param1;
      {  While waiting for feedback, the line is disconnected. This event is triggered  }
      DoStatus('No feedback received, abnormal disconnection');
      {  Since it will not trigger a successful feedback event, PMyDefine needs to be released here  }
      dispose(p2);
    end
    );
  disposeObject([SendDe]);
end;

procedure TDRClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TDRClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  client := TZNet_Client_CrossSocket.Create;
end;

procedure TDRClientForm.FormDestroy(Sender: TObject);
begin
  disposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TDRClientForm.DelayResponseBtnClick(Sender: TObject);
var
  SendDe: TDataFrameEngine;
  a: Integer;
begin
  {  Asynchronous sending and receiving Stream instructions, feedback triggered by proc callback  }
  a := 123;
  SendDe := TDataFrameEngine.Create;
  client.SendStreamCmdP('DelayResponse', SendDe,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    begin
      {  When the event here is triggered, DelayResponseBtnClick has already been executed and variable a no longer exists, at least it is out of the normal program scope  }
      {  When an asynchronous event is triggered, a is in an unbroken stack space, which is out of normal use because it is an asynchronous event  }
      {  Do not refer to external local variables in asynchronous events. Try to use global variables or asynchronous events in para mode. Specify and pass variables in pointer mode. Refer to the implementation of delayresponse2btnclick  }
      while ResultData.Reader.NotEnd do
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
      {  Do you know whether the reference of variable a is a copy or a pointer?  }
      {  The answer is a pointer. Delphi's anonymous function automatically references an external variable as a pointer, and when you reference it, you are accessing something in an unknown region  }
      {  A printed here is 456  }
      DoStatus(a);
    end);
  disposeObject([SendDe]);
  a := 456;
end;

procedure TDRClientForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  client.Progress;
end;

procedure TDRClientForm.ConnectButtonClick(Sender: TObject);
begin
  if client.Connect(HostEdit.Text, 9818) then
      DoStatus('connect success');
end;

end.
