program DatasetSimOnLinuxServer;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Classes,
  Variants,
  Z.Core,
  Z.ZDB.Engine,
  Z.ZDB.LocalManager,
  Z.Status,
  Z.DFE,
  Z.PascalStrings,
  Z.ListEngine,
  Z.UnicodeMixedLib,
  Z.MemoryStream,
  Z.Parsing,
  Z.Expression,
  Z.OpCode,
  Z.Net,
  Z.Net.DoubleTunnelIO.NoAuth,
  Z.Net.Server.CrossSocket;

{
  ��Demoʹ��ZDB��Ϊ����ʵ�壬ģ����Dataset�Ĵ������

  Dataset��Զ��sql����˼·:

  �����ڿͻ��ˣ��ȴ���һ�� Record������������ڴ�����ǵĲ�ѯ���
  Ȼ�����Ƿ���һ����ѯ���� "QueryMyDatabase" ����������ͬʱ����Record��Ϊ����
  ��������������ʹ��query�����Լ���������ݴ����գ�����Ĳ�ѯ�������д���
  �������������ݵĲ�ѯ������ɺ󣬷������ᰴ�� BatchStream ����������

  BatchStream�������ģʽ����ž�����4��

  1,ClearBatchStream  //��λԶ��BatchStream����
  2,PostBatchStream   //��ʼ�������ǵĲ�ѯ���
  3,SendQueryResult   //���߿ͻ��ˣ���ѯ����Ѿ��������
  4,ClearBatchStream  //��λԶ��BatchStream����

  ��4���ķ��͹��������ڷ�������
  ��4���Ľ��պʹ������ڿͻ��˸�

  �ڸ�Demo�У�û��ֱ��ʹ��delphi�Դ���Query�͸߼������ݿ�����
  �ڸ�Demo�У�ʹ��zdb��ģ��query�͸߼����ݿ��ѯ���棬�乤�����ƺ�ʵ��˼·�����ݿ�������ͬ
}

type
  TMyDoubleServer = class(TZNet_DoubleTunnelService_NoAuth)
  public
    zdb: TZDBLocalManager;
    procedure cmd_QueryMyDatabase(Sender: TPeerIO; InData: TDataFrameEngine);
    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

type
  POpCodeRun_Record = ^TOpCodeRun_Record;

  TOpCodeRun_Record = record
    op: TOpCode;
    opR: TOpCustomRunTime;
    sql: TPascalString;
    Remote_backCall: UInt64;
    dPipe: TZDBPipeline;
    qState: PQueryState;
    function zExp_DB(var Param: TOpParam): Variant;
  end;

function TOpCodeRun_Record.zExp_DB(var Param: TOpParam): Variant;
begin
  Result := qState.Eng.VT[qState^.StorePos]['Key'];
end;

procedure TMyDoubleServer.cmd_QueryMyDatabase(Sender: TPeerIO; InData: TDataFrameEngine);
var
  queryRec: POpCodeRun_Record;
begin
  new(queryRec);
  // Զ�̻ص���ַ
  queryRec^.Remote_backCall := InData.Reader.ReadPointer;
  // ͨ��sql��ѯһ���������ݿ⣬��Ȼ��Ҳ������Զ�̵����ݿ�
  queryRec^.sql := InData.Reader.ReadString;
  // ʹ��zExpressionģ��sql���
  queryRec^.dPipe := nil;
  queryRec^.qState := nil;
  queryRec^.opR := TOpCustomRunTime.Create;
  queryRec^.opR.RegOpM('val', queryRec^.zExp_DB);
  queryRec^.opR.RegOpM('value', queryRec^.zExp_DB);
  queryRec^.op := BuildAsOpCode(True, tsPascal, queryRec^.sql, queryRec^.opR);
  if queryRec^.op = nil then
    begin
      Sender.PrintError('build opCode error:' + queryRec^.sql);
      DisposeObject(queryRec^.opR);
      Dispose(queryRec);
      exit;
    end;

  // sql���ݿ�ֱ��ʹ�����Ʒ���
  // ���෽��һ����ִ��ǰ�������и��﷨���
  // query.executeSql(sql)
  // ����ѯ��ɺ�
  // delphi��lazarus�����ݿⶼ��һ���ػ��࣬��Dataset��Query��ִ�в�ѯ�󣬶�����һ���㶨���Dataset���������ѯ���
  // dataset���ܱ��棬�����֪����ô���棬��ȥ����SaveToStream�ķ������ܻ��ҵ�
  // dataset.savetoStream(mystream);
  // ����ʹ�����ݿ���Ҫ��Module�������鷳����ֱ��ʹ��zdb��ģ�����ݿ�Ĳ�ѯ��ʽ���ҵķ����ܱ��������ǰ����ݴ��
  with zdb.QueryDBP(
    True,   // ��ѯ���д�뵽���ر�
    True,   // ��ѯ�ķ��ر����ڴ�������False����һ��ʵ����ļ���
    False,  // �����ʼ��ѯ
    'mydb', // ��ѯ��Ŀ�����ݿ�����
    '',     // ���ر�����ƣ�������ջ��������һ����ʱ�ı�����
    True,   // ��ѯ���ʱ���ͷŷ��ر�
    30,     // ��ѯ���ʱ���ͷŷ��ر���ӳ�ʱ�䣬��λ����
    0.1,    // ��Ƭ����ʱ�䣬����ѯ�кܶ෴��ʱ��ÿ���۵����ʱ�䣬�ʹ��������¼������������������ڻ���ʱ���У����ݶ��������ڴ�
    0,      // ��ѯִ��ʱ��,0������
    0,      // ���Ĳ�ѯ��Ŀƥ��������0������
    1000,   // ���Ĳ�ѯ���������0��ʾ����
      procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    var
        queryRec_Ptr: POpCodeRun_Record;
    begin
        queryRec_Ptr := dPipe.UserPointer;

        queryRec_Ptr^.dPipe := dPipe;
        queryRec_Ptr^.qState := @qState;
        try
          Allowed := Boolean(queryRec_Ptr^.op.OpCode_Execute(queryRec_Ptr^.opR));
          if Allowed then
            nop;
        except
          // �����ѯ�з����쳣������������ѯ����
          qState.Aborted := True;
          // Ҳ����ͨ������ķ���������ѯ����
          // dPipe.stop;
      end;
    end,
    procedure(dPipe: TZDBPipeline)
    var
      queryRec_Ptr: POpCodeRun_Record;
      r_io_def: TService_RecvTunnel_UserDefine_NoAuth;
      de: TDataFrameEngine;
      mystream: TMemoryStream64;
    begin

      queryRec_Ptr := dPipe.UserPointer;
      // ���ﶼ�ǵ���ѯ��ɺ�ɵ���

      // �ͷŲ���ʹ�õ�OpRunTime��
      DisposeObject(queryRec_Ptr^.op);
      DisposeObject(queryRec_Ptr^.opR);

      // ����ѯ��ɺ�����ɿͻ����Ѿ����ߣ�������ߣ��ͷ��ڴ棬��������
      if not RecvTunnel.Exists(Cardinal(dPipe.UserVariant)) then
        begin
          Dispose(queryRec_Ptr);
          exit;
        end;

      // ����ѯ��ɺ�����ɿͻ��˻�����

      r_io_def := GetUserDefineRecvTunnel(RecvTunnel.PeerIO[dPipe.UserVariant]);

      // ����ѯ�Ľ���ŵ�mystream�У�dPipe.OutputDB.SaveToStream��ͬ��Dataset.SaveToStream
      dPipe.OutputDB.Update;
      mystream := TMemoryStream64.Create;
      dPipe.OutputDB.SaveToStream(mystream);

      // ���ڣ�����Ҫ��myStream���͸��ͻ���ͬʱ���������ͻ��˲�ѯ��ͳ�ƽ���ͻص������¼�

      // ��λBatchStream
      ClearBatchStream(r_io_def.SendTunnel.Owner);
      PostBatchStream(r_io_def.SendTunnel.Owner, mystream, True);

      // �����ص�ָ����ڴ��ַ
      de := TDataFrameEngine.Create;
      de.WritePointer(queryRec_Ptr^.Remote_backCall);
      de.WriteInteger(dPipe.OutputDB.Count);
      de.WriteString(dPipe.PipelineName);
      r_io_def.SendTunnel.Owner.SendDirectStreamCmd('QueryDone', de);
      DisposeObject(de);

      // ��λBatchStream
      ClearBatchStream(r_io_def.SendTunnel.Owner);

      // ����ͷ�����ʹ�õ� queryRec_Ptr �ڴ�ָ��
      Dispose(queryRec_Ptr);
    end) do
    begin
      UserVariant := Sender.id;
      UserPointer := queryRec;
    end;
end;

procedure TMyDoubleServer.RegisterCommand;
begin
  inherited;
  FRecvTunnel.RegisterDirectStream('QueryMyDatabase').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_QueryMyDatabase;
end;

procedure TMyDoubleServer.UnRegisterCommand;
begin
  inherited;
  FRecvTunnel.DeleteRegistedCMD('QueryMyDatabase');
end;

{ ��ѭ�� }
procedure MainLoop;
var
  serv: TMyDoubleServer;
  rt, st: TZNet_Server;
  // ģ��ʵ�����ݿ�ʹ��
  i: integer;
  VT: TDBEngineVT;
begin
  doStatus('  ��Demoʹ��ZDB��Ϊ����ʵ�壬ģ����Dataset�Ĵ������'#13#10 +
    #13#10 +
    '  Dataset��Զ��sql����˼·:'#13#10 +
    #13#10 +
    '  �����ڿͻ��ˣ��ȴ���һ�� Record������������ڴ�����ǵĲ�ѯ���'#13#10 +
    '  Ȼ�����Ƿ���һ����ѯ���� "QueryMyDatabase" ����������ͬʱ����Record��Ϊ����'#13#10 +
    '  ��������������ʹ��query�����Լ���������ݴ����գ�����Ĳ�ѯ�������д���'#13#10 +
    '  �������������ݵĲ�ѯ������ɺ󣬷������ᰴ�� BatchStream ����������'#13#10 +
    #13#10 +
    '  BatchStream�������ģʽ����ž�����4��'#13#10 +
    #13#10 +
    '  1,ClearBatchStream  //��λԶ��BatchStream����'#13#10 +
    '  2,PostBatchStream   //��ʼ�������ǵĲ�ѯ���'#13#10 +
    '  3,SendQueryResult   //���߿ͻ��ˣ���ѯ����Ѿ��������'#13#10 +
    '  4,ClearBatchStream  //��λԶ��BatchStream����'#13#10 +
    #13#10 +
    '  ��4���ķ��͹��������ڷ�������'#13#10 +
    '  ��4���Ľ��պʹ������ڿͻ��˸�'#13#10 +
    #13#10 +
    '  �ڸ�Demo�У�û��ֱ��ʹ��delphi�Դ���Query�͸߼������ݿ�����'#13#10 +
    '  �ڸ�Demo�У�ʹ��zdb��ģ��query�͸߼����ݿ��ѯ���棬�乤�����ƺ�ʵ��˼·�����ݿ�������ͬ'#13#10);

  rt := TZNet_Server_CrossSocket.Create.StableIO;
  if rt.StartService('', 10991) then
      doStatus('listen %s on port:10991 success', [TranslateBindAddr('')])
  else
      doStatus('listen %s on port:10991 failed', [TranslateBindAddr('')]);

  st := TZNet_Server_CrossSocket.Create.StableIO;
  if st.StartService('', 10992) then
      doStatus('listen %s on port:10992 success', [TranslateBindAddr('')])
  else
      doStatus('listen %s on port:10992 failed', [TranslateBindAddr('')]);

  // ����ʹ��StableIO����������˫ͨ���Ľ�������
  // StableIO�����ڲ��ƻ�ʵ����ǰ���£��Զ�����������
  // ��Ȼ���ͻ���Ҳ��Ҫ��StableIO�ķ�ʽ
  // ���Demo����ѭ�����񣬲����˳������Բ��ͷŷ�������StableIO��û���ڴ�й©��
  serv := TMyDoubleServer.Create(rt, st);
  serv.RegisterCommand;

  // ģ��ʵ�����ݿ�
  serv.zdb := TZDBLocalManager.Create;
  // ģ�����ݿ����ڴ���
  serv.zdb.InitMemoryDB('mydb');
  // ����10�������ݼ�¼
  doStatus('database building...');
  for i := 1 to 1 * 10000 do
    begin
      VT := TDBEngineVT.Create;
      VT['key'] := inttostr(umlRandomRange(-10000, 10000));
      serv.zdb.PostData('mydb', VT);
      DisposeObject(VT);
    end;
  doStatus('database build done,total: %d ,size: %s', [serv.zdb['mydb'].Count, umlSizeToStr(serv.zdb['mydb'].DBEngine.Size).Text]);

  doStatus('server prepare ok.');
  while True do
    begin
      serv.Progress;
      serv.zdb.Progress;
      if serv.RecvTunnel.Count > 0 then
          Z.Core.CheckThreadSynchronize(1)
      else
          Z.Core.CheckThreadSynchronize(10);
    end;
end;

begin
  try
      MainLoop;
  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
