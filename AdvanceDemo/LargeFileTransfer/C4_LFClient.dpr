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
  Z.GHashList,
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
  // TLAction是线性事件模型，用于处理线性流程，例如流程为，1,2,3,4,5，TLActon可以按次序逐步执行触发事件
  // TSplit_Upload_Action的作用是实现序列化分块上传
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
  // 完成当前分块上传，进行下一个
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
  // TSplit_Upload_Done_Action的作用是给已上传的数据生成碎片索引，然后，再以文件方式上传
  // 后面要下载时，先下这个索引，然后再下碎片，最后组合碎片，完成文件下载
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
  // 分块上传完成，直接退出循环
  Exit_Signal := True;
end;

type
  // TDownload_Action的作用是下载碎片，然后写入目标文件
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
  // 关闭目标文件
  DisposeObject(LFS);
  // 这两步是操作Linear模型
  inherited Run();
  Done;
  DoStatus('done download "%s" -> "%s"', [Download_File.Text, To_Dest_File.Text]);
  // 下载完成，退出主循环
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
      // 下载索引文件，然后解码
      try
        d := TDFE.Create;
        DoStatus('decode "%s"', [info_.Text]);
        d.DecodeFrom(Stream, True);
        FS := TCore_FileStream.Create(To_Dest_File, fmCreate);
        FS.Size := d.R.ReadInt64;
        DoStatus('begin download "%s"', [To_Dest_File.Text]);
        while d.R.NotEnd do
          begin
            // 把碎片仍给Linear模型，让他依次下载
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
        // 发生错误，直接退出来
          Exit_Signal := True;
      end;
    end
  else
    begin
      // 远程服务器没有找到索引，直接退出循环
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
      // FS2.0上传需要先把文件拆成碎片
      // 最简单的办法是直接拆到内存中，但是大文件往往非常大，这样，内存是行不通的
      // 所以，使用LinearAction机制，按序列事件依次拆开大文件，然后上传碎片，上传机制不是并发的，碎片按序列传
      // 如果要做成并发上传，建议以下两种方式
      // 1，修改 TSplit_Upload_Action 机制，将每次传一个碎片，改成，传多个碎片即可
      // 2，开多了个 _L_Action 实例，折叠上传，如果我在这里实现，程序会非常复杂，且难以阅读理解
      // 支持中断和续传机制，客户端中途断线或则出问题不影响，直接再次运行程序就行了，会自动化恢复
      // 上传期间要保证文件没有改动，否则上传会失败
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
      // 下索引文件
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

      // 本服务器可以直接copy代码到laz环境构建成IOT,linux系统
      // 注意：在fpc-console应用是无法使用LCL的，必须是NoUI程序，C4可以支持No LCL应用环境
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

          // 退出信号发出以后，主循环结束，这里做一个安全性处理，拿0.1秒做最后循环
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

