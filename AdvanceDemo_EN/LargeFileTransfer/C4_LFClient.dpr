program C4_LFClient;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UPascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.ListEngine,
  Z.HashList.Templet,
  Z.Expression,
  Z.OpCode,
  Z.Parsing,
  Z.DFE,
  Z.TextDataEngine,
  Z.MemoryStream,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_FS2,
  Z.Net.C4_Console_APP,
  Z.LinearAction;

var
  Exit_Signal: Boolean;
  Internet_IP_Or_DNS_Address: U_String;
  Internet_Port: WORD;
  Upload_File: U_String;
  Upload_Split: Int64;
  Download_File: U_String;
  To_Dest_File: U_String;
  Is_Download: Boolean;
  Is_Upload: Boolean;
  L_Action: TLActionList;

procedure DoHelp;
begin
  DoStatus('C4_LFClient.exe help.');
  DoStatus('');
  DoStatus('C4_LFClient.exe -H -I -P -U -S -D -T');
  DoStatus('');
  DoStatus('"-H" help info.');
  DoStatus('"-I/-IP:ip/dns" LF-Service host', []);
  DoStatus('"-P/-Port:port" LF-Service port,default is %d', [Internet_Port]);
  DoStatus('"-U/-Upload:file" upload File from local.');
  DoStatus('"-S/-Split:num" upload File split option.');
  DoStatus('"-D/-Down/-Download:file" download from remote.');
  DoStatus('"-T/-To/-Dest:file" download or uplaod to local/remote.');
  DoStatus('');
  DoStatus('custom upload my file examples, hint: Split 1024*1024=1M fragments file to upload');
  DoStatus('C4_LFClient "-I:localhost" "-P:%d" "-U:c:\mydatabase.ox2" "-s:1024*1024" "-T:my_remote_backup.ox2"', [Internet_Port]);
  DoStatus('');
  DoStatus('custom download my file examples');
  DoStatus('C4_LFClient "-I:localhost" "-P:%d" "-D:my_remote_backup.ox2" "-T:c:\mydatabase.ox2"', [Internet_Port]);
  DoStatus('');
end;

function Fill_CMD: Boolean;
var
  i: Integer;
  n: U_String;
  H_: Boolean;
begin
  Result := False;

  H_ := False;
  Internet_IP_Or_DNS_Address := '';
  Internet_Port := 9188;
  Upload_File := '';
  Upload_Split := 128 * 1024;
  Download_File := '';
  To_Dest_File := '';
  Is_Download := False;
  Is_Upload := False;

  for i := 1 to ParamCount do
    begin
      n := ParamStr(i);
      if umlMultipleMatch(['-H', '-Help'], n) then
          H_ := True
      else if umlMultipleMatch(['-i:*', '-ip:*'], n) then
          Internet_IP_Or_DNS_Address := umlDeleteFirstStr(n, ': ')
      else if umlMultipleMatch(['-p:*', '-port:*'], n) then
          Internet_Port := umlStrToInt(umlDeleteFirstStr(n, ': '))
      else if umlMultipleMatch(['-u:*', '-upload:*'], n) then
        begin
          Upload_File := umlDeleteFirstStr(n, ': ');
          Is_Upload := True;
        end
      else if umlMultipleMatch(['-s:*', '-split:*'], n) then
          Upload_Split := EStrToInt64(umlDeleteFirstStr(n, ': '), Upload_Split)
      else if umlMultipleMatch(['-d:*', '-down:*', '-download:*'], n) then
        begin
          Download_File := umlDeleteFirstStr(n, ': ');
          Is_Download := True;
        end
      else if umlMultipleMatch(['-t:*', '-to:*', '-dest:*'], n) then
          To_Dest_File := umlDeleteFirstStr(n, ': ')
    end;

  if H_ or (Internet_IP_Or_DNS_Address.L = 0) or ((Upload_File.L = 0) and (Download_File.L = 0)) then
    begin
      DoHelp;
      exit;
    end;

  if Is_Upload then
    begin
      if not umlFileExists(Upload_File) then
        begin
          DoStatus('no found "%s"', [Upload_File.Text]);
          exit;
        end;
      if To_Dest_File.L = 0 then
          To_Dest_File := umlGetFileName(Upload_File);
    end;

  if Is_Download then
    begin
      if To_Dest_File.L = 0 then
          To_Dest_File := umlCombineFileName(Z.Net.C4.C40_RootPath, Download_File);
    end;

  Result := True;
end;

type
  {  TLAction is a linear event model used to handle linear processes, such as processes 1,2,3,4,5. TLActon can execute trigger events step by step in order  }
  {  TSplit_Upload_The function of Action is to achieve serialization and block uploading  }
  TSplit_Upload_Action = class(TLAction)
  public
    LClient: TC40_FS2_Client;
    LFile: U_String;
    LPos: Int64;
    LSiz: Int64;
    LMD5: TMD5;

    constructor Create(Owner_: TLActionList); override;
    procedure Run(); override;
    procedure Do_FS2_PostFile_Done(Sender: TC40_FS2_Client; info_: U_String);
  end;

constructor TSplit_Upload_Action.Create(Owner_: TLActionList);
begin
  inherited Create(Owner_);
  LClient := nil;
  LFile := '';
  LPos := 0;
  LMD5 := NullMD5;
end;

procedure TSplit_Upload_Action.Do_FS2_PostFile_Done(Sender: TC40_FS2_Client; info_: U_String);
begin
  DoStatus('done upaload pos:%d size:%d', [LPos, LSiz]);
  {  Complete the current block upload and proceed to the next one  }
  Done;
end;

procedure TSplit_Upload_Action.Run;
var
  FS: TCore_FileStream;
  m64: TMS64;
begin
  inherited Run;
  if not LClient.Connected then
    begin
      Stop;
      exit;
    end;
  FS := TCore_FileStream.Create(LFile, fmOpenRead or fmShareDenyNone);
  FS.Position := LPos;
  m64 := TMS64.Create;
  m64.Size := LSiz;
  m64.Position := 0;
  m64.CopyFrom(FS, LSiz);
  DisposeObject(FS);
  LMD5 := m64.ToMD5;
  m64.SaveToFile(umlCombineFileName(Z.Net.C4.C40_RootPath, PFormat('%s.cache', [umlMD5ToStr(LMD5).Text])));
  LClient.FS2_PostFile_M(True, umlMD5ToStr(LMD5), m64, True, Do_FS2_PostFile_Done);
end;

type
  {  TSplit_Upload_Done_The function of Action is to generate a fragmented index for the uploaded data, and then upload it as a file  }
  {  When downloading later, first download this index, then add fragments, and finally combine the fragments to complete the file download  }
  TSplit_Upload_Done_Action = class(TLAction)
  public
    LClient: TC40_FS2_Client;
    FileSiz: Int64;
    constructor Create(Owner_: TLActionList); override;
    procedure Run(); override;
    procedure Do_FS2_PostFile_Done(Sender: TC40_FS2_Client; info_: U_String);
  end;

constructor TSplit_Upload_Done_Action.Create(Owner_: TLActionList);
begin
  inherited Create(Owner_);
  LClient := nil;
  FileSiz := 0;
end;

procedure TSplit_Upload_Done_Action.Run;
var
  i: Integer;
  d: TDFE;
  UA: TSplit_Upload_Action;
  m64: TMS64;
begin
  inherited Run;
  d := TDFE.Create;
  d.WriteInt64(FileSiz);
  for i := 0 to Owner.List.Count - 1 do
    if Owner.List[i] is TSplit_Upload_Action then
      begin
        UA := Owner.List[i] as TSplit_Upload_Action;
        d.WriteInt64(UA.LPos);
        d.WriteMD5(UA.LMD5);
      end;
  m64 := TMS64.Create;
  d.EncodeAsZLib(m64, True);
  DisposeObject(d);
  m64.SaveToFile(umlCombineFileName(Z.Net.C4.C40_RootPath, PFormat('%s.cache', [umlMD5ToStr(m64.ToMD5).Text])));
  LClient.FS2_PostFile_M(True, To_Dest_File, m64, True, Do_FS2_PostFile_Done);
end;

procedure TSplit_Upload_Done_Action.Do_FS2_PostFile_Done(Sender: TC40_FS2_Client; info_: U_String);
begin
  Done;
  DoStatus('done upload "%s" -> "%s"', [Upload_File.Text, To_Dest_File.Text]);
  {  Block upload completed, exit the loop directly  }
  Exit_Signal := True;
end;

type
  {  TDownload_The function of Action is to download fragments and then write them to the target file  }
  TDownload_Action = class(TLAction)
  public
    LClient: TC40_FS2_Client;
    LFS: TCore_FileStream;
    LPos: Int64;
    LMD5: TMD5;
    constructor Create(Owner_: TLActionList); override;
    procedure Run(); override;
    procedure Do_Download_Fragment_Done(Sender: TC40_FS2_Client; Stream: TMS64; info_: U_String; Successed: Boolean);
  end;

constructor TDownload_Action.Create(Owner_: TLActionList);
begin
  inherited Create(Owner_);
  LClient := nil;
  LFS := nil;
  LPos := 0;
  LMD5 := NullMD5;
end;

procedure TDownload_Action.Run();
var
  cache_f: U_String;
  m64: TMS64;
begin
  inherited Run();
  cache_f := umlCombineFileName(Z.Net.C4.C40_RootPath, PFormat('%s.cache', [umlMD5ToStr(LMD5).Text]));
  if umlFileExists(cache_f) then
    begin
      m64 := TMS64.Create;
      m64.LoadFromFile(cache_f);
      m64.Position := 0;
      Do_Download_Fragment_Done(LClient, m64, umlGetFileName(cache_f), True);
      DisposeObject(m64);
    end
  else
    begin
      LClient.FS2_GetFile_M(True, umlMD5ToStr(LMD5), Do_Download_Fragment_Done);
    end;
end;

procedure TDownload_Action.Do_Download_Fragment_Done(Sender: TC40_FS2_Client; Stream: TMS64; info_: U_String; Successed: Boolean);
var
  cache_f: U_String;
begin
  if Successed then
    begin
      cache_f := umlCombineFileName(Z.Net.C4.C40_RootPath, PFormat('%s.cache', [umlMD5ToStr(LMD5).Text]));
      LFS.Position := LPos;
      Stream.Position := 0;
      LFS.CopyFrom(Stream, Stream.Size);
      if not umlFileExists(cache_f) then
          Stream.SaveToFile(cache_f);
      DoStatus('done download pos:%d size:%d', [LPos, Stream.Size]);
      Done;
    end
  else
    begin
      DoStatus(info_);
      Exit_Signal := True;
      Stop;
    end;
end;

type
  TDownload_Successed_Action = class(TLAction)
  public
    LFS: TCore_FileStream;
    constructor Create(Owner_: TLActionList); override;
    procedure Run(); override;
  end;

constructor TDownload_Successed_Action.Create(Owner_: TLActionList);
begin
  inherited Create(Owner_);
  LFS := nil;
end;

procedure TDownload_Successed_Action.Run();
begin
  {  close the target file  }
  DisposeObject(LFS);
  {  These two steps are to operate the Linear model  }
  inherited Run();
  Done;
  DoStatus('done download "%s" -> "%s"', [Download_File.Text, To_Dest_File.Text]);
  {  Download completed, exit the main loop  }
  Exit_Signal := True;
end;

procedure Do_Download_Index_Done(Sender: TC40_FS2_Client; Stream: TMS64; info_: U_String; Successed: Boolean);
var
  d: TDFE;
  FS: TCore_FileStream;
  DA: TDownload_Action;
  DSA: TDownload_Successed_Action;
begin
  if Successed then
    begin
      {  Download the index file and decode it  }
      try
        d := TDFE.Create;
        DoStatus('decode "%s"', [info_.Text]);
        d.DecodeFrom(Stream, True);
        FS := TCore_FileStream.Create(To_Dest_File, fmCreate);
        FS.Size := d.R.ReadInt64;
        DoStatus('begin download "%s"', [To_Dest_File.Text]);
        while d.R.NotEnd do
          begin
            {  Give the fragments to the Linear model and have him download them one by one  }
            DA := L_Action.Add(TDownload_Action) as TDownload_Action;
            DA.LClient := Sender;
            DA.LFS := FS;
            DA.LPos := d.R.ReadInt64;
            DA.LMD5 := d.R.ReadMD5;
          end;
        DSA := L_Action.Add(TDownload_Successed_Action) as TDownload_Successed_Action;
        DSA.LFS := FS;
        DisposeObject(d);
        L_Action.Run;
      except
        {  If an error occurs, just step back out  }
          Exit_Signal := True;
      end;
    end
  else
    begin
      {  The remote server did not find the index and exited the loop directly  }
      DoStatus(info_);
      Exit_Signal := True;
    end;
end;

procedure Do_FS2_Ready(Client: TC40_FS2_Client);
var
  UA: TSplit_Upload_Action;
  UD: TSplit_Upload_Done_Action;
  pos_, siz: Int64;
begin
  if Is_Upload then
    begin
      {  FS2.0 upload requires first breaking the file into fragments  }
      {  The simplest way is to directly disassemble it into memory, but large files are often very large, which makes memory impractical  }
      {  So, using the LinearAction mechanism, break down large files in sequence of events, and then upload fragments. The upload mechanism is not concurrent, and fragments are transmitted in sequence  }
      {  If you want to create concurrent uploads, we suggest the following two methods  }
      {  1. Modify TSplit_Upload_Action mechanism, changing one fragment at a time to multiple fragments  }
      {  2. I drove an extra one_L_Action instance, folding and uploading. If I implement it here, the program will be very complex and difficult to read and understand  }
      {  Supports interruption and continuation mechanisms, and if the client disconnects midway or encounters problems, it does not affect it. Simply run the program again and it will be automatically restored  }
      {  During the upload process, ensure that the file has not been modified, otherwise the upload will fail  }
      siz := umlGetFileSize(Upload_File);
      pos_ := 0;
      while pos_ < siz do
        begin
          UA := L_Action.Add(TSplit_Upload_Action) as TSplit_Upload_Action;
          UA.LClient := Client;
          UA.LFile := Upload_File;
          UA.LPos := pos_;
          if siz - pos_ >= Upload_Split then
              UA.LSiz := Upload_Split
          else
              UA.LSiz := siz - pos_;
          inc(pos_, UA.LSiz);
        end;
      UD := L_Action.Add(TSplit_Upload_Done_Action) as TSplit_Upload_Done_Action;
      UD.LClient := Client;
      UD.FileSiz := siz;
      L_Action.Run;
    end
  else if Is_Download then
    begin
      {  Lower index file  }
      Client.FS2_GetFile_C(True, Download_File, Do_Download_Index_Done);
      DoStatus('prepare download "%s"', [Download_File.Text]);
    end
  else
      Exit_Signal := True;
end;

procedure Do_Wait_FS2_Connected_Done(States_: TC40_Custom_ClientPool_Wait_States);
var
  i: Integer;
begin
  for i := low(States_) to high(States_) do
    if States_[i].Client_ is TC40_FS2_Client then
      begin
        Do_FS2_Ready(TC40_FS2_Client(States_[i].Client_));
        break;
      end;
end;

var
  tmp_tk: TTimeTick;

begin
  if Fill_CMD then
    begin
      L_Action := TLActionList.Create(nil);
      Z.Net.C4.C40_QuietMode := True;

      {  This server can directly copy code to the Laz environment to build IOT and Linux systems  }
      {  Note: LCL cannot be used in fpc console applications and must be a NoUI program. C4 can support the No LCL application environment  }
      Z.Net.C4_Console_APP.C40AppParsingTextStyle := TTextStyle.tsC;
      Z.Net.C4_Console_APP.C40AppParam := [
        Format('AutoTunnel("%s",%d,"FS2",True)', [Internet_IP_Or_DNS_Address.Text, Internet_Port])
        ];

      if Z.Net.C4_Console_APP.C40_Extract_CmdLine then
        begin
          Z.Net.C4.C40_ClientPool.WaitConnectedDoneC('FS2', @Do_Wait_FS2_Connected_Done);

          Exit_Signal := False;
          while not Exit_Signal do
            begin
              L_Action.Progress;
              Z.Net.C4.C40Progress;
            end;

          {  After the exit signal is issued, the main loop ends. Here, a security processing is performed, taking 0.1 seconds as the final loop  }
          tmp_tk := GetTimeTick + 100;
          while GetTimeTick < tmp_tk do
            begin
              L_Action.Progress;
              Z.Net.C4.C40Progress;
            end;
        end;

      DisposeObject(L_Action);
      Z.Net.C4.C40Clean;
    end;

end.
