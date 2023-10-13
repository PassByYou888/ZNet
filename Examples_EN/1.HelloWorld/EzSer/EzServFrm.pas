unit EzServFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.Json,
  Z.Net,
  Z.Net.Server.ICS,
  Z.Net.Server.Indy,
  Z.Net.Server.CrossSocket,
  Z.Status, Z.Core,
  Z.DFE, Z.UnicodeMixedLib, Z.MemoryStream;

type
  { Tpeerclientuserspecial is an instance that is automatically created on a per user basis }
  { Please pay attention to freeing memory when using it }
  { Tpeerclientuserdefine is used for auth, DB and other services }
  { The role of TPeerClientUserSpecial is to provide exclusive instances to developers in case of conflicts with Auth and DB of advanced services }
  TMySpecialDefine = class(TPeerClientUserSpecial)
  public
    tempStream: TMemoryStream64;
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TEZServerForm = class(TForm)
    Memo1: TMemo;
    StartServiceButton: TButton;
    Timer1: TTimer;
    procedure StartServiceButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);

    procedure cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
    procedure cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDFE);
    procedure cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDFE);

    procedure cmd_Json_Stream(Sender: TPeerClient; InData: TDFE);

    procedure cmd_TestMiniStream(Sender: TPeerClient; InData: TDFE);

    procedure cmd_Test128MBigStream(Sender: TPeerClient; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64);

    procedure cmd_TestCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
  public
    { Public declarations }
    Server: TZNet_Server_CrossSocket;
  end;

var
  EZServerForm: TEZServerForm;

implementation

{$R *.dfm}


procedure TEZServerForm.cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
begin
  DoStatus('client: %s', [InData]);
end;

procedure TEZServerForm.cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDFE);
begin
  DoStatus('client: %s', [InData.Reader.ReadString]);
end;

procedure TEZServerForm.cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDFE);
begin
  OutData.WriteString('result 654321');
end;

procedure TEZServerForm.cmd_Json_Stream(Sender: TPeerClient; InData: TDFE);
var
  js: TZ_JsonObject;
  ns: TStringList;
begin
  js := TZ_JsonObject.Create;
  ns := TStringList.Create;
  InData.Reader.ReadJson(js);
  js.SaveToLines(ns);
  DoStatus(ns);
  disposeObject(ns);
  disposeObject(js);
end;

procedure TEZServerForm.cmd_TestMiniStream(Sender: TPeerClient; InData: TDFE);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  InData.Reader.ReadStream(ms);

  DoStatus(umlMD5Char(ms.Memory, ms.Size).Text);

  disposeObject(ms);
end;

procedure TEZServerForm.cmd_Test128MBigStream(Sender: TPeerClient; InData: TCore_Stream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  tempStream: TMemoryStream64;
begin
  tempStream := TMySpecialDefine(Sender.UserSpecial).tempStream;
  tempStream.CopyFrom(InData, InData.Size);
  DoStatus('%d/%d', [BigStreamTotal, BigStreamCompleteSize]);

  // bigstream complete
  if tempStream.Size = BigStreamTotal then
    begin
      Sender.Print('bigsteram finish');
      Sender.Print('bigsteram md5:' + umlMD5Char(tempStream.Memory, tempStream.Size).Text);
      tempStream.Clear;
    end;
end;

procedure TEZServerForm.cmd_TestCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
begin
  Sender.Print('Complete buffer md5: %s', [umlMD5String(InData, DataSize).Text]);
end;

procedure TEZServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TEZServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  Server := TZNet_Server_CrossSocket.Create;
  Server.PeerClientUserSpecialClass := TMySpecialDefine;
  Server.TimeOutIDLE := 5 * 60 * 1000;

  { Change the maximum completebuffer. It is only used for testing. The server runs normally. It is generally 4m here }
  Server.MaxCompleteBufferSize := 128 * 1024 * 1024;

  Server.RegisterDirectConsole('helloWorld_Console').OnExecute := cmd_helloWorld_Console;
  Server.RegisterDirectStream('helloWorld_Stream').OnExecute := cmd_helloWorld_Stream;
  Server.RegisterStream('helloWorld_Stream_Result').OnExecute := cmd_helloWorld_Stream_Result;

  Server.RegisterDirectStream('Json_Stream').OnExecute := cmd_Json_Stream;

  Server.RegisterDirectStream('TestMiniStream').OnExecute := cmd_TestMiniStream;
  Server.RegisterBigStream('Test128MBigStream').OnExecute := cmd_Test128MBigStream;

  { Register the completebuffer directive }
  Server.RegisterCompleteBuffer('TestCompleteBuffer').OnExecute := cmd_TestCompleteBuffer;
end;

procedure TEZServerForm.FormDestroy(Sender: TObject);
begin
  disposeObject(Server);
  DeleteDoStatusHook(self);
end;

procedure TEZServerForm.StartServiceButtonClick(Sender: TObject);
begin
  { Based on the official crosssocket document, if the binding string is empty, bind IPv6 + IPv4 }
  if Server.StartService('', 9818) then
      DoStatus('start service success')
  else
      DoStatus('start service failed!')
end;

procedure TEZServerForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  Server.Progress;
end;

{ TMySpecialDefine }

constructor TMySpecialDefine.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  tempStream := TMemoryStream64.Create;
end;

destructor TMySpecialDefine.Destroy;
begin
  DoStatus('%s disconnect', [Owner.GetPeerIP]);
  disposeObject(tempStream);
  inherited Destroy;
end;

end.
