unit ADRestoreCliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  Z.Status, Z.Core, Z.PascalStrings, Z.UnicodeMixedLib, Z.ListEngine, Z.MemoryStream, Z.Notify,
  Z.Net,
  Z.Net.DoubleTunnelIO,
  Z.Net.Client.CrossSocket,
  Z.Cadencer, Z.DFE, Vcl.Mask;

type
  TAuthDoubleTunnelClientForm = class(TForm)
    Memo1: TMemo;
    ConnectButton: TButton;
    HostEdit: TLabeledEdit;
    Timer1: TTimer;
    BlockBatchOperationBtn: TButton;
    UserEdit: TLabeledEdit;
    PasswdEdit: TLabeledEdit;
    RegUserButton: TButton;
    AsyncConnectButton: TButton;
    RestoreDownloadButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure RegUserButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure AsyncConnectButtonClick(Sender: TObject);
    procedure BlockBatchOperationBtnClick(Sender: TObject);
    procedure RestoreDownloadButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    RecvTunnel: TZNet_Client_CrossSocket;
    SendTunnel: TZNet_Client_CrossSocket;
    Client: TZNet_DoubleTunnelClient;
  end;

var
  AuthDoubleTunnelClientForm: TAuthDoubleTunnelClientForm;

implementation

{$R *.dfm}


procedure TAuthDoubleTunnelClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  RecvTunnel := TZNet_Client_CrossSocket.Create;
  SendTunnel := TZNet_Client_CrossSocket.Create;
  Client := TZNet_DoubleTunnelClient.Create(RecvTunnel, SendTunnel);

  Client.RegisterCommand;
end;

procedure TAuthDoubleTunnelClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(Client);
  DeleteDoStatusHook(self);
end;

procedure TAuthDoubleTunnelClientForm.Timer1Timer(Sender: TObject);
begin
  CheckThread;
  Client.Progress;
end;

procedure TAuthDoubleTunnelClientForm.RegUserButtonClick(Sender: TObject);
begin
  SendTunnel.Connect(HostEdit.Text, 9815);
  RecvTunnel.Connect(HostEdit.Text, 9816);

  {  Check if both channels have been successfully linked and ensure that initialization work such as symmetric encryption has been completed  }
  while (not Client.RemoteInited) and (Client.Connected) do
    begin
      TThread.Sleep(10);
      Client.Progress;
    end;

  if Client.Connected then
      Client.RegisterUser(UserEdit.Text, PasswdEdit.Text);

  SendTunnel.Disconnect;
  RecvTunnel.Disconnect;
end;

procedure TAuthDoubleTunnelClientForm.ConnectButtonClick(Sender: TObject);
begin
  SendTunnel.Connect(HostEdit.Text, 9815);
  RecvTunnel.Connect(HostEdit.Text, 9816);

  {  Check if both channels have been successfully linked and ensure that initialization work such as symmetric encryption has been completed  }
  while (not Client.RemoteInited) and (Client.Connected) do
    begin
      TThread.Sleep(10);
      Client.Progress;
    end;

  if Client.Connected then
    begin
      {  Nested anonymous function support  }
      Client.UserLoginP(UserEdit.Text, PasswdEdit.Text,
          procedure(const State: Boolean)
        begin
          if State then
              Client.TunnelLinkP(procedure(const State: Boolean)
              begin
                DoStatus('double tunnel link success!');
              end)
        end);
    end;
end;

procedure TAuthDoubleTunnelClientForm.AsyncConnectButtonClick(Sender: TObject);
begin
  {  Method 2, asynchronous dual channel link  }
  Client.AsyncConnectP(HostEdit.Text, 9816, 9815,
    procedure(const cState: Boolean)
    begin
      if cState then
        begin
          {  Nested anonymous function support  }
          Client.UserLoginP(UserEdit.Text, PasswdEdit.Text,
              procedure(const lState: Boolean)
            begin
              if lState then
                begin
                  Client.TunnelLinkP(
                      procedure(const tState: Boolean)
                    begin
                      if tState then
                          DoStatus('double tunnel link success!')
                      else
                          DoStatus('double tunnel link failed!');
                    end)
                end
              else
                begin
                  if lState then
                      DoStatus('login success!')
                  else
                      DoStatus('login failed!');
                end;
            end);
        end
      else
        begin
          if cState then
              DoStatus('connected success!')
          else
              DoStatus('connected failed!');
        end;
    end);

end;

procedure TAuthDoubleTunnelClientForm.BlockBatchOperationBtnClick(Sender: TObject);
var
  lst: TCore_StringList;
  hashSiz: THashVariantList;
  hashMD5: THashStringList;
  i: Integer;
begin
  lst := TCore_StringList.Create;
  Client.GetPublicFileList('*.*', lst);
  hashSiz := THashVariantList.CustomCreate(1024);
  hashMD5 := THashStringList.CustomCreate(1024);

  {  Asynchronously obtaining remote file information, as it loops through all in a for without going through the Progress (time slice main loop), here is a batch  }
  for i := 0 to lst.Count - 1 do
    begin
      Client.GetPublicFileInfoP(lst[i], nil, nil,
        procedure(const UserData: Pointer; const UserObject: TCore_Object;
          const fileName: SystemString; const Existed: Boolean; const fSiz: Int64)
        begin
          if Existed then
              hashSiz.Add(fileName, fSiz);
        end);
    end;

  {  Wait until the above asynchronous command processing is completed. Here, blocking is simulated by batch  }
  Client.SendTunnel.Wait(20 * 1000);

  {  When a batch is executed, the second batch is traversed  }
  hashSiz.ProgressP(procedure(Sender: THashVariantList; Name: PSystemString; const v: Variant)
    begin
      Client.GetPublicFileMD5P(Name^, 0, v, nil, nil,
          procedure(const UserData: Pointer; const UserObject: TCore_Object;
          const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5)
        begin
          hashMD5.Add(fileName, umlMD5ToStr(MD5).Text);
        end);
    end);

  {  Wait until the above asynchronous command processing is completed. Here, blocking is simulated by batch  }
  Client.SendTunnel.Wait(20 * 1000);

  {  When the second batch traversal is completed, we print out the MD5 information  }
  DoStatus('Remote file list:');
  hashMD5.ProgressP(procedure(Sender: THashStringList; Name: PSystemString; const v: SystemString)
    begin
      DoStatus('File name:%s Volume:%s md5:%s', [Name^, umlSizeToStr(hashSiz[Name^]).Text, v]);
    end);

  DisposeObject(lst);
  DisposeObject(hashSiz);
  DisposeObject(hashMD5);
end;

procedure TAuthDoubleTunnelClientForm.RestoreDownloadButtonClick(Sender: TObject);
begin
  {  Download the complete file from the remote first  }
  Client.GetPublicFileP('ADRestoreServer.exe', umlCurrentPath, nil, nil,
    procedure(const UserData: Pointer; const UserObject: TCore_Object; stream: TCore_Stream; const fileName: SystemString)
    var
      m5: TMD5;
      m64: TMemoryStream64;
    begin
      m5 := umlStreamMD5(stream, 0, 512);
      DoStatus('Local MD5(0..512):%s', [umlMD5ToStr(m5).Text]);
      m64 := TMemoryStream64.Create;
      m64.LoadFromStream(stream);

      {  Get MD5 for remote file truncation  }
      Client.GetPublicFileMD5P('ADRestoreServer.exe', 0, 512, nil, nil,
          procedure(const UserData: Pointer; const UserObject: TCore_Object;
          const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5)
        begin
          {  Only retrieve the first 512byte of the file and store it again, laying the foundation for the subsequent breakpoint continuation demo  }
          DoStatus('remote MD5(0..512):%s', [umlMD5ToStr(MD5).Text]);
          m64.Size := 512;
          m5 := umlStreamMD5(m64);
          DoStatus('Local MD5(0..512):%s', [umlMD5ToStr(m5).Text]);
          m64.SaveToFile(umlCombineFileName(umlCurrentPath, 'ADRestoreServer.exe'));
          DisposeObject(m64);

          {  Get full MD5 for remote files  }
          Client.GetPublicFileMD5P('ADRestoreServer.exe', 0, 0, nil, nil,
              procedure(const UserData: Pointer; const UserObject: TCore_Object;
              const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5)
            begin
              DoStatus('remote MD5:%s', [umlMD5ToStr(MD5).Text]);
              {  Download from remote breakpoint continuation  }
              Client.GetPublicFileP('ADRestoreServer.exe', 512, umlCurrentPath, nil, nil,
                  procedure(const UserData: Pointer; const UserObject: TCore_Object; stream: TCore_Stream; const fileName: SystemString)
                begin
                  {  If the above two MD5 print results are the same, it indicates that the file download of breakpoint continuation is complete  }
                  DoStatus('Local MD5:%s', [umlMD5ToStr(umlStreamMD5(stream)).Text]);

                  DoStatus('Start to demonstrate the upload of breakpoint continuation in about 1 second');

                  {  Now let's demonstrate the upload function of breakpoint continuation  }
                  Client.ProgressEngine.PostExecuteP(1.0, procedure(Sender: TNPostExecute)
                    var
                      m64: TMemoryStream64;
                    begin
                      m64 := TMemoryStream64.Create;
                      m64.loadFromFile(umlCombineFileName(umlCurrentPath, 'ADRestoreServer.exe'));
                      {  Truncate the file body to 999byte  }
                      m64.Size := 999;
                      DoStatus('private local md5(0..999):%s', [umlMD5ToStr(umlStreamMD5(m64)).Text]);
                      {  We use private space to store uploaded files. The last parameter indicates that M64 will be released automatically after uploading  }
                      Client.PostStreamToPrivate('testUpload.dat', '', m64, True);
                      {  Because the sendtunnel is used to upload files, now we do a wait event in sendtunnel to detect whether the upload is completed  }
                      Client.SendTunnel.WaitP(10 * 1000,
                          procedure(const State: Boolean)
                        begin
                          {  Triggering an event here indicates that the upload has been completed  }
                          Client.GetPrivateFileMD5P('testUpload.dat', '', 0, 0, nil, nil,
                              procedure(const UserData: Pointer; const UserObject: TCore_Object;
                              const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5)
                            var
                              m64_2: TMemoryStream64;
                            begin
                              DoStatus('private remote md5(0..999):%s', [umlMD5ToStr(MD5).Text]);
                              m64_2 := TMemoryStream64.Create;
                              m64_2.loadFromFile(umlCombineFileName(umlCurrentPath, 'ADRestoreServer.exe'));
                              DoStatus('private local md5:%s', [umlMD5ToStr(umlStreamMD5(m64_2)).Text]);
                              {  We began to upload the breakpoint continuation  }
                              Client.PostStreamToPrivate('testUpload.dat', '', m64_2, 999, True);

                              {  Because the sendtunnel is used to upload files, now we do a wait event in sendtunnel to detect whether the upload is completed  }
                              Client.SendTunnel.WaitP(10 * 1000,
                                  procedure(const State: Boolean)
                                begin
                                  Client.GetPrivateFileMD5P('testUpload.dat', '', 0, 0, nil, nil,
                                      procedure(const UserData: Pointer; const UserObject: TCore_Object;
                                      const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5)
                                    begin
                                      {  If the above two MD5 are the same, it indicates that the upload of breakpoint continuation has been successful  }
                                      DoStatus('private remote md5:%s', [umlMD5ToStr(MD5).Text]);
                                      DoStatus('restore demo over!');
                                    end);
                                end);
                            end);
                        end);
                    end);
                end);
            end);
        end);
    end);
end;

procedure TAuthDoubleTunnelClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

end.
