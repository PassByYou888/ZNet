program LargeScale_ZDB_DataBuild;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Math,
  Classes,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Parsing,
  Z.Status,
  Z.MemoryStream,
  Z.ListEngine,
  Z.ZDB.Engine,
  Z.ZDB.LocalManager;

function DestDBPath: SystemString;
begin
  Result := umlGetCurrentPath;
end;

// ģ�⹹��.CSV��ʽ�ļ�
procedure BuildRandCSVData;
const
  c_MaxFileSize = Int64(1) * Int64(1024 * 1024); // ��Ҫ������csv�ļ��ߴ�
var
  ioHnd: TIOHnd;
  i: Integer;
  n, tmp: U_String;
  c: Int64;
  buff: TBytes;
begin
  DoStatus('��ʼ�������ģcsv');
  InitIOHnd(ioHnd);
  umlFileCreate(umlCombineFileName(DestDBPath, 'big.csv'), ioHnd);

  // prepare csv header
  n := '';
  for i := 0 to 5 do
      n.Append('%d,', [i]);
  n.DeleteLast;
  n.Append(#13#10);
  buff := n.PlatformBytes;
  umlBlockWrite(ioHnd, buff[0], length(buff));
  SetLength(buff, 0);
  n := '';

  // build csv body
  c := 0;
  while (ioHnd.Size < c_MaxFileSize) do
    begin
      n := '';
      for i := 0 to 5 do
        begin
          tmp := TPascalString.RandomString(umlRandomRange(10, 20));
          tmp.DeleteChar('()[]"' + #39);
          n.Append(tmp + ',');
        end;
      n.DeleteLast;
      n.Append(#13#10);
      buff := n.PlatformBytes;
      umlBlockWrite(ioHnd, buff[0], length(buff));
      SetLength(buff, 0);
      n := '';
      inc(c);

      if c mod 100000 = 0 then
          DoStatus('.CSV ������.. �Ѿ���� %s Ŀ�� %s', [umlSizeToStr(ioHnd.Size).Text, umlSizeToStr(c_MaxFileSize).Text]);
    end;

  umlFileClose(ioHnd);
end;

procedure BuildZDB;
var
  LM: TZDBLocalManager;
  db: TZDBLMStore;
  r: TStreamReader;
begin
  DoStatus('��ʼ�������ģZDB');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;

  r := TStreamReader.Create(umlCombineFileName(DestDBPath, 'big.csv').Text, TEncoding.UTF8);
  db := LM.InitNewDB('big');

  CustomImportCSV_P(
      procedure(var L: TPascalString; var IsEnd: Boolean)
    begin
      IsEnd := r.EndOfStream;
      if not IsEnd then
          L := r.ReadLine;
    end,
    procedure(const sour: TPascalString; const king, Data: TArrayPascalString)
    var
      i: Integer;
      VT: THashStringList;
    begin
      VT := THashStringList.CustomCreate(16);
      for i := Low(king) to High(king) do
          VT[king[i]] := Data[i];
      db.AddData(VT);
      DisposeObject(VT);

      // ÿ����10000����¼���������ݵ�����Ӳ��
      if db.Count mod 10000 = 0 then
        begin
          DoStatus('����� %d ������, ���ݿ�ߴ� %s �ں�״̬ %s %s',
            [db.Count, umlSizeToStr(db.DBEngine.Size).Text,
              db.CacheAnnealingState, db.DBEngine.CacheStatus]);
        end;

      // TZDBLocalManager.Progress��������ÿ�뱣��һ�����ݿ�
      LM.Progress;

      // ��������ǲ�ʹ��TZDBLocalManager.Progressʱ��Ҳ�����ֶ��ͷ�cache��ÿ����20������¼ʱ���һ��cache
      if db.Count mod 200000 = 0 then
        begin
          db.DBEngine.CleaupCache;
        end;
    end);

  DisposeObject(r);
  DisposeObject(LM);
end;

procedure QueryZDB1;
var
  LM: TZDBLocalManager;
  db: TZDBLMStore;
  LVT: TDBListVT;
  tk: TTimeTick;
  i, j: Integer;
begin
  DoStatus('���ٲ�ѯģ��');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;
  db := LM.InitDB('big');

  // ����������������Ҫ�Ӵ��ں˵�hash��������
  db.DBEngine.ResetCachePool(100 * 10000);

  // �����ڴ淽ʽ��ѯ����Ҫ��ʱ��ʱ��ܳ������ĵȴ�
  DoStatus('���������ڴ�', []);
  LVT := TDBListVT.Create;

  // ͬ����ʽ���룬���ַ�ʽ���ص㣺�ڶ�ȡ�����ڼ䣬����������Ӧ�ģ�LoadFromStoreEngine����ֻ�ܹ��������߳���
  // LVT.LoadFromStoreEngine(db);

  // �첽��ʽ�������ݣ����ַ�ʽ���ص�����ò�ѯ���ƣ������ܱ�ͬ����ʽ��ʽҪ��һ�㣬���ǿ����ǵ��첽�������������ڼ�����ó������������
  // ���Ǵ���һ��loading�Ĳ�ѯ�����ڸ���������step��ʽ���룬���Ǽ��첽��ʽ
  db.QueryP('loading', False,
    procedure(var qState: TQueryState)
    begin
      if qState.IsVT then
          LVT.Add(qState.Eng.BuildVT(qState));
    end,
    procedure()
    begin
    end);

  // �ȴ���̨��ѯ����loading���
  tk := GetTimeTick;
  while db.QueryProcessing do
    begin
      Check_Soft_Thread_Synchronize(100);
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('loading ����״̬ %d/%d �ں�״̬ %s %s', [LVT.Count, db.Count, db.CacheAnnealingState, db.DBEngine.CacheStatus]);
          tk := GetTimeTick;
        end;
    end;

  // ������ɺ���ʱ��ѯ�ͻ�ǳ����ˣ����������ݶ������
  tk := GetTimeTick;
  for j := 0 to 99 do
    for i := 0 to LVT.Count - 1 do
      begin
        CompareText(LVT[i]['1'], 'abc');
      end;
  DoStatus('�ڴ淽ʽƽ����ѯ��ʱ%dms', [(GetTimeTick - tk) div 100]);
  DisposeObject(LVT);

  DoStatus('���ٲ�ѯģ�������.');
  DisposeObject(LM);
end;

procedure QueryZDB2;
var
  LM: TZDBLocalManager;
  db: TZDBLMStore;
  tk: TTimeTick;
  i: Integer;
begin
  DoStatus('��̨��ѯģ��');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;
  db := LM.InitDB('big');

  // ����������������Ҫ�Ӵ��ں˵�hash��������
  db.DBEngine.ResetCachePool(100 * 10000);

  // ������ʽ��ѯ
  // ���˻�����İ����£��������ѯ��ȵ��������
  // ģ��200��ͬʱ��ѯ������
  for i := 0 to 200 - 1 do
    begin
      LM.QueryDBP(
      False, // ����ѯ���д�뵽һ����ʱ���ݿ�
      True, // ��ʱ���ݿ����ڴ�ģʽ
      Odd(MT19937Rand32(MaxInt)), // ������������ѯ
      db.Name, // Ŀ�����ݿ�
      '', // ��ʱ���ݿ����֣�������ָ��վ����������
      True, // ��ѯ����������ݿ���Զ����ͷ�
      0.0, // �ͷ���ʱ���ݿ���ӳ�ʱ��
      0, // ��Ƭ���ݷ���ʱ��,�����ṩ��online����ʹ�õĲ���,cs�ܹ���ZDB
      ifThen(i = 0, 70, umlRandomRangeD(1, 70)), // ���Ʋ�ѯʱ�䣬���xx-xx��
      0, // ��������ѯ�ı�����¼
      0, // ��������ѯ�ķ��ؼ�¼
        procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
        begin
          Allowed := True;
        end,
          procedure(dPipe: TZDBPipeline)
        begin
          DoStatus('%s ��%sʱ���� ��� %d ����¼��ѯ',
            [dPipe.PipelineName, umlTimeTickToStr(round(dPipe.QueryConsumTime * 1000)).Text, dPipe.QueryCounter]);
        end
        );
    end;

  // �ȴ���̨��ѯ�������
  tk := GetTimeTick;
  while db.QueryProcessing do
    begin
      LM.Progress;
      Check_Soft_Thread_Synchronize(100);
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('�ں�״̬ %s %s', [db.CacheAnnealingState, db.DBEngine.CacheStatus]);
          tk := GetTimeTick;
        end;
    end;

  DoStatus('��̨��ѯģ�������.');
  DisposeObject(LM);
end;

procedure QueryZDB3;
var
  LM: TZDBLocalManager;
  db: TZDBLMStore;
  tk: TTimeTick;
  i: Integer;
begin
  DoStatus('�����ں�cache���ģ�����ѯģ��');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;
  db := LM.InitDB('big');

  // ����������������Ҫ�Ӵ��ں˵�hash��������
  db.DBEngine.ResetCachePool(100 * 10000);

  // ���ַ�ʽ��ֱ�ӹر��˻����棬��ʵ��ȫ�����嵽ZDB���ں���
  // �ڻ�����ɺ󣬲�ѯ�ٶȷǳ���
  db.CacheStyle := csAlways;
  // ��ʼ��������
  LM.QueryDBP(False, // ����ѯ���д�뵽һ����ʱ���ݿ�
  True, // ��ʱ���ݿ����ڴ�ģʽ
  True, // �������ѯ
  db.Name, // Ŀ�����ݿ�
  '', // ��ʱ���ݿ����֣�������ָ��վ����������
  True, // ��ѯ����������ݿ���Զ����ͷ�
  0.0, // �ͷ���ʱ���ݿ���ӳ�ʱ��
  0, // ��Ƭ���ݷ���ʱ��,�����ṩ��online����ʹ�õĲ���,cs�ܹ���ZDB
  0, // ���Ʋ�ѯʱ�䣬0������
  0, // ��������ѯ�ı�����¼
  0, // ��������ѯ�ķ��ؼ�¼
    procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    begin
      Allowed := False;
      // GetVT�������Լ�����ʵ��
      qState.Eng.GetVT(qState);
    end,
      procedure(dPipe: TZDBPipeline)
    begin
    end
    );

  // �ȴ���̨��ѯ�������
  tk := GetTimeTick;
  while db.QueryProcessing do
    begin
      LM.Progress;
      Check_Soft_Thread_Synchronize(100);
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('�ں˻���״̬ %s %s', [db.CacheAnnealingState, db.DBEngine.CacheStatus]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('ȫ����ʵ���Ѿ��������.');

  DoStatus('��ʼģ��2����ѯ����.');
  // ģ��2��ͬʱ��ѯ������
  for i := 0 to 2 - 1 do
    begin
      LM.QueryDBP(False, // ����ѯ���д�뵽һ����ʱ���ݿ�
      True, // ��ʱ���ݿ����ڴ�ģʽ
      Odd(MT19937Rand32(MaxInt)), // ������������ѯ
      db.Name, // Ŀ�����ݿ�
      '', // ��ʱ���ݿ����֣�������ָ��վ����������
      True, // ��ѯ����������ݿ���Զ����ͷ�
      0.0, // �ͷ���ʱ���ݿ���ӳ�ʱ��
      0, // ��Ƭ���ݷ���ʱ��,�����ṩ��online����ʹ�õĲ���,cs�ܹ���ZDB
      0, // ���Ʋ�ѯʱ�䣬0������
      0, // ��������ѯ�ı�����¼
      0, // ��������ѯ�ķ��ؼ�¼
        procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
        begin
          if qState.IsVT then
              Allowed := CompareText(qState.Eng.VT[qState.StorePos]['0'], 'abc') > 0;
        end,
          procedure(dPipe: TZDBPipeline)
        begin
          DoStatus('%s ��%sʱ�� ��� %d ����¼��ѯ',
            [dPipe.PipelineName, umlTimeTickToStr(round(dPipe.QueryConsumTime * 1000)).Text, dPipe.QueryCounter]);
        end
        );
    end;

  // �ȴ���̨��ѯ�������
  while db.QueryProcessing do
    begin
      LM.Progress;
      Check_Soft_Thread_Synchronize(10);
    end;

  DoStatus('�����ں�cache���ģ�����ѯ�����.');
  DisposeObject(LM);
end;

procedure QueryZDB4;
var
  LM: TZDBLocalManager;
  db: TZDBLMStore;
  tk: TTimeTick;
  arry: TStoreArray;
  i, j: Integer;
begin
  DoStatus('���ô洢��ַ��ѯģ��');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;
  db := LM.InitDB('big');

  // ����������������Ҫ�Ӵ��ں˵�hash��������
  db.DBEngine.ResetCachePool(100 * 10000);

  // ���ַ�ʽ��ֱ�ӹر��˻����棬��ʵ��ȫ�����嵽ZDB���ں���
  // �ڻ�����ɺ󣬲�ѯ�ٶȷǳ���
  db.CacheStyle := csAlways;
  // ��ʼ��������
  LM.QueryDBP(False, // ����ѯ���д�뵽һ����ʱ���ݿ�
  True, // ��ʱ���ݿ����ڴ�ģʽ
  True, // �������ѯ
  db.Name, // Ŀ�����ݿ�
  '', // ��ʱ���ݿ����֣�������ָ��վ����������
  True, // ��ѯ����������ݿ���Զ����ͷ�
  0.0, // �ͷ���ʱ���ݿ���ӳ�ʱ��
  0, // ��Ƭ���ݷ���ʱ��,�����ṩ��online����ʹ�õĲ���,cs�ܹ���ZDB
  0, // ���Ʋ�ѯʱ�䣬0������
  0, // ��������ѯ�ı�����¼
  0, // ��������ѯ�ķ��ؼ�¼
    procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    begin
      Allowed := False;
      // GetVT�������Լ�����ʵ��
      qState.Eng.GetVT(qState);
    end,
      procedure(dPipe: TZDBPipeline)
    begin
    end
    );

  // �ȴ���̨��ѯ�������
  tk := GetTimeTick;
  while db.QueryProcessing do
    begin
      LM.Progress;
      Check_Soft_Thread_Synchronize(100);
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('�ں˻���״̬ %s %s', [db.CacheAnnealingState, db.DBEngine.CacheStatus]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('ȫ����ʵ���Ѿ��������.');

  DoStatus('�����洢��ַ����');
  db.BuildStoreArray(False, @arry);

  DoStatus('����ʹ�ô洢��ַ��������ʽ��ѯ100��');
  tk := GetTimeTick;
  for j := 0 to 100 - 1 do
    for i := Low(arry) to high(arry) do
        CompareText(db.VT[arry[i]]['0'], 'abc');
  DoStatus('��ѯ�Ѿ���ɣ���������ƽ����ʱ %dms', [(GetTimeTick - tk) div 100]);

  DoStatus('���ô洢��ַ��ѯ�����.');
  DisposeObject(LM);
end;

begin
  BuildRandCSVData;
  BuildZDB;
  QueryZDB1;
  QueryZDB2;
  QueryZDB3;
  QueryZDB4;
  DoStatus('�س����˳�.');
  readln;

end.
