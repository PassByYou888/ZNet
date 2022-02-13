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
  _Exit_Signal: Boolean;
  _Internet_IP_Or_DNS_Address: U_String;
  _Internet_Port: WORD;
  _Upload_File: U_String;
  _Upload_Split: Int64;
  _Download_File: U_String;
  _To_Dest_File: U_String;
  _Is_Download: Boolean;
  _Is_Upload: Boolean;
  _L_Action: TLActionList;

procedure DoHelp;
begin
  DoStatus('C4_LFClient.exe help.');
  DoStatus('');
  DoStatus('C4_LFClient.exe -H -I -P -U -S -D -T');
  DoStatus('');
  DoStatus('"-H" help info.');
  DoStatus('"-I/-IP:ip/dns" LF-Service host', []);
  DoStatus('"-P/-Port:port" LF-Service port,default is %d', [_Internet_Port]);
  DoStatus('"-U/-Upload:file" upload File from local.');
  DoStatus('"-S/-Split:num" upload File split option.');
  DoStatus('"-D/-Down/-Download:file" download from remote.');
  DoStatus('"-T/-To/-Dest:file" download to local/remote.');
  DoStatus('');
  DoStatus('custom upload my file examples, hint: Split 1024*1024=1M fragments file to upload');
  DoStatus('C4_LFClient "-I:localhost" "-P:%d" "-U:c:\mydatabase.ox2" "-s:1024*1024" "-T:my_remote_backup.ox2"', [_Internet_Port]);
  DoStatus('');
  DoStatus('custom download my file examples');
  DoStatus('C4_LFClient "-I:localhost" "-P:%d" "-D:my_remote_backup.ox2" "-T:c:\mydatabase.ox2"', [_Internet_Port]);
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
  _Internet_IP_Or_DNS_Address := '';
  _Internet_Port := 9188;
  _Upload_File := '';
  _Upload_Split := 128 * 1024;
  _Download_File := '';
  _To_Dest_File := '';
  _Is_Download := False;
  _Is_Upload := False;

  for i := 1 to ParamCount do
    begin
      n := ParamStr(i);
      if umlMultipleMatch(['-H', '-Help'], n) then
          H_ := True
      else if umlMultipleMatch(['-i:*', '-ip:*'], n) then
          _Internet_IP_Or_DNS_Address := umlDeleteFirstStr(n, ': ')
      else if umlMultipleMatch(['-p:*', '-port:*'], n) then
          _Internet_Port := umlStrToInt(umlDeleteFirstStr(n, ': '))
      else if umlMultipleMatch(['-u:*', '-upload:*'], n) then
        begin
          _Upload_File := umlDeleteFirstStr(n, ': ');
          _Is_Upload := True;
        end
      else if umlMultipleMatch(['-s:*', '-split:*'], n) then
          _Upload_Split := EStrToInt64(umlDeleteFirstStr(n, ': '), _Upload_Split)
      else if umlMultipleMatch(['-d:*', '-down:*', '-download:*'], n) then
        begin
          _Download_File := umlDeleteFirstStr(n, ': ');
          _Is_Download := True;
        end
      else if umlMultipleMatch(['-t:*', '-to:*', '-dest:*'], n) then
          _To_Dest_File := umlDeleteFirstStr(n, ': ')
    end;

  if H_ or (_Internet_IP_Or_DNS_Address.L = 0) or ((_Upload_File.L = 0) and (_Download_File.L = 0)) then
    begin
      DoHelp;
      exit;
    end;

  if _Is_Upload then
    begin
      if not umlFileExists(_Upload_File) then
        begin
          DoStatus('no found "%s"', [_Upload_File.Text]);
          exit;
        end;
      if _To_Dest_File.L = 0 then
          _To_Dest_File := umlGetFileName(_Upload_File);
    end;

  if _Is_Download then
    begin
      if _To_Dest_File.L = 0 then
          _To_Dest_File := umlCombineFileName(Z.Net.C4.C40_RootPath, _Download_File);
    end;

  Result := True;
end;

type
  // TLAction�������¼�ģ�ͣ����ڴ����������̣���������Ϊ��1,2,3,4,5��TLActon���԰�������ִ�д����¼�
  // TSplit_Upload_Action��������ʵ�����л��ֿ��ϴ�
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
  // ��ɵ�ǰ�ֿ��ϴ���������һ��
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
  LClient.FS2_PostFile_M(True, umlMD5ToStr(LMD5), m64, True, Do_FS2_PostFile_Done);
end;

type
  // TSplit_Upload_Done_Action�������Ǹ����ϴ�������������Ƭ������Ȼ�������ļ���ʽ�ϴ�
  // ����Ҫ����ʱ���������������Ȼ��������Ƭ����������Ƭ������ļ�����
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
  d.EncodeAsZLib(m64, False);
  DisposeObject(d);
  LClient.FS2_PostFile_M(True, _To_Dest_File, m64, True, Do_FS2_PostFile_Done);
end;

procedure TSplit_Upload_Done_Action.Do_FS2_PostFile_Done(Sender: TC40_FS2_Client; info_: U_String);
begin
  Done;
  DoStatus('done upload "%s" -> "%s"', [_Upload_File.Text, _To_Dest_File.Text]);
  // �ֿ��ϴ���ɣ�ֱ���˳�ѭ��
  _Exit_Signal := True;
end;

type
  // TDownload_Action��������������Ƭ��Ȼ��д��Ŀ���ļ�
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
begin
  LClient.FS2_GetFile_M(True, umlMD5ToStr(LMD5), Do_Download_Fragment_Done);
  inherited Run();
end;

procedure TDownload_Action.Do_Download_Fragment_Done(Sender: TC40_FS2_Client; Stream: TMS64; info_: U_String; Successed: Boolean);
begin
  if Successed then
    begin
      LFS.Position := LPos;
      Stream.Position := 0;
      LFS.CopyFrom(Stream, Stream.Size);
      DoStatus('done download pos:%d size:%d', [LPos, Stream.Size]);
      Done;
    end
  else
    begin
      DoStatus(info_);
      _Exit_Signal := True;
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
  // �ر�Ŀ���ļ�
  DisposeObject(LFS);
  // �������ǲ���Linearģ��
  inherited Run();
  Done;
  DoStatus('done download "%s" -> "%s"', [_Download_File.Text, _To_Dest_File.Text]);
  // ������ɣ��˳���ѭ��
  _Exit_Signal := True;
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
      // ���������ļ���Ȼ�����
      try
        d := TDFE.Create;
        d.DecodeFrom(Stream, False);
        FS := TCore_FileStream.Create(_To_Dest_File, fmCreate);
        FS.Size := d.R.ReadInt64;
        DoStatus('begin download "%s"', [_To_Dest_File.Text]);
        while d.R.NotEnd do
          begin
            // ����Ƭ�Ը�Linearģ�ͣ�������������
            DA := _L_Action.Add(TDownload_Action) as TDownload_Action;
            DA.LClient := Sender;
            DA.LFS := FS;
            DA.LPos := d.R.ReadInt64;
            DA.LMD5 := d.R.ReadMD5;
          end;
        DSA := _L_Action.Add(TDownload_Successed_Action) as TDownload_Successed_Action;
        DSA.LFS := FS;
        DisposeObject(d);
        _L_Action.Run;
      except
        // ��������ֱ���˳���
          _Exit_Signal := True;
      end;
    end
  else
    begin
      // Զ�̷�����û���ҵ�������ֱ���˳�ѭ��
      DoStatus(info_);
      _Exit_Signal := True;
    end;
end;

procedure Do_FS2_Ready(Client: TC40_FS2_Client);
var
  UA: TSplit_Upload_Action;
  UD: TSplit_Upload_Done_Action;
  pos_, siz: Int64;
begin
  if _Is_Upload then
    begin
      // FS2.0�ϴ���Ҫ�Ȱ��ļ������Ƭ
      // ��򵥵İ취��ֱ�Ӳ��ڴ��У����Ǵ��ļ������ǳ����������ڴ����в�ͨ��
      // ���ԣ�ʹ��LinearAction���ƣ��������¼����β𿪴��ļ���Ȼ���ϴ���Ƭ���ϴ����Ʋ��ǲ����ģ���Ƭ�����д�
      // ���Ҫ���ɲ����ϴ��������������ַ�ʽ
      // 1���޸� TSplit_Upload_Action ���ƣ���ÿ�δ�һ����Ƭ���ĳɣ��������Ƭ����
      // 2�������˸� _L_Action ʵ�����۵��ϴ��������������ʵ�֣������ǳ����ӣ��������Ķ����
      // ֧���жϺ��������ƣ��ͻ�����;���߻�������ⲻӰ�죬ֱ���ٴ����г�������ˣ����Զ����ָ�
      // �ϴ��ڼ�Ҫ��֤�ļ�û�иĶ��������ϴ���ʧ��
      siz := umlGetFileSize(_Upload_File);
      pos_ := 0;
      while pos_ < siz do
        begin
          UA := _L_Action.Add(TSplit_Upload_Action) as TSplit_Upload_Action;
          UA.LClient := Client;
          UA.LFile := _Upload_File;
          UA.LPos := pos_;
          if siz - pos_ >= _Upload_Split then
              UA.LSiz := _Upload_Split
          else
              UA.LSiz := siz - pos_;
          inc(pos_, UA.LSiz);
        end;
      UD := _L_Action.Add(TSplit_Upload_Done_Action) as TSplit_Upload_Done_Action;
      UD.LClient := Client;
      UD.FileSiz := siz;
      _L_Action.Run;
    end
  else if _Is_Download then
    begin
      // �������ļ�
      Client.FS2_GetFile_C(True, _Download_File, Do_Download_Index_Done);
    end
  else
      _Exit_Signal := True;
end;

procedure Do_Wait_FS2_Connected_Done(States_: TC40_Custom_ClientPool_Wait_States);
var
  i: Integer;
begin
  for i := low(States_) to high(States_) do
    if States_[i].Client_ is TC40_FS2_Client then
        Do_FS2_Ready(TC40_FS2_Client(States_[i].Client_));
end;

var
  tmp_tk: TTimeTick;

begin
  if Fill_CMD then
    begin
      _L_Action := TLActionList.Create(nil);
      Z.Net.C4.C40_QuietMode := True;

      // ������������ֱ��copy���뵽laz����������IOT,linuxϵͳ
      // ע�⣺��fpc-consoleӦ�����޷�ʹ��LCL�ģ�������NoUI����C4����֧��No LCLӦ�û���
      Z.Net.C4_Console_APP.C40AppParsingTextStyle := TTextStyle.tsC;
      Z.Net.C4_Console_APP.C40AppParam := [
        Format('Tunnel("%s",%d,"FS2")', [_Internet_IP_Or_DNS_Address.Text, _Internet_Port])
        ];

      if Z.Net.C4_Console_APP.C40_Extract_CmdLine then
        begin
          Z.Net.C4.C40_ClientPool.WaitConnectedDoneC('FS2', @Do_Wait_FS2_Connected_Done);

          _Exit_Signal := False;
          while not _Exit_Signal do
            begin
              _L_Action.Progress;
              Z.Net.C4.C40Progress;
            end;

          // �˳��źŷ����Ժ���ѭ��������������һ����ȫ�Դ�����0.1�������ѭ��
          tmp_tk := GetTimeTick + 100;
          while GetTimeTick < tmp_tk do
            begin
              _L_Action.Progress;
              Z.Net.C4.C40Progress;
            end;
        end;

      DisposeObject(_L_Action);
      Z.Net.C4.C40Clean;
    end;

end.
