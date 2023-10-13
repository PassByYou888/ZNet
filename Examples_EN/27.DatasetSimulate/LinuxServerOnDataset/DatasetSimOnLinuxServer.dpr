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

{  This demo uses ZDB as the data entity and simulates the transmission operation of the dataset
The working concept of remote SQL for Dataset:
On the client side, we first create a Record, which is a container used to store our query results
Then, we send a query command "QueryMyDatabase" to the server, with Record as a parameter
Next, the server uses query or self-defined data processing techniques to process your query conditions
After the server completes the query processing of the data, it will provide feedback according to the combined commands of BatchStream
The BatchStream combination command mode is roughly these four steps
1. ClearBatchStream//Reset the remote BatchStream container
2. PostBatchStream//Start transmitting our query results
3. SendQueryResult//Tell the client that the query result has been transmitted
4. ClearBatchStream//Reset the remote BatchStream container
These four steps of sending work are all done on the server
The reception and processing of these four steps are all done on the client side
In this demo, Delphi's built-in Query and advanced database engine were not directly used
In this demo, zdb is used to simulate query and advanced database query engines, with the same working mechanism and implementation approach as the database engine  }

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
  {  Remote callback address  }
  queryRec^.Remote_backCall := InData.Reader.ReadPointer;
  {  Query a local database through SQL. Of course, it can also be a remote database  }
  queryRec^.sql := InData.Reader.ReadString;
  {  Simulate SQL statements using zexpression  }
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

  {  SQL databases use similar methods directly  }
  {  This type of method usually has a syntax check before execution  }
  // query.executeSql(sql)
  {  When the query is complete  }
  {  Both Delphi and Lazarus databases have a foundation class called Dataset. After executing a query, Query will output the query results to a dataset that you define  }
  {  All datasets can be saved. If you don't know how to save them, search the savetostream method and you'll always find it  }
  // dataset.savetoStream(mystream);
  {  Due to the need to create a Module when using a database, which is too cumbersome, I directly used zdb to simulate the query method of the database. My method is very violent, which is to package the data  }
  with zdb.QueryDBP(
    True,   {  The query results are written to the return table  }
    True,   {  The return table of the query is a memory table, and if it is false, it is an entity's file table  }
    False,  {  Query from the end  }
    'mydb', {  The name of the target database for the query  }
    '',     {  Returns the name of the table. If it is left blank, a temporary table name will be randomly generated  }
    True,   {  When the query is completed, release the return table  }
    30,     {  The delay time in seconds for releasing the return table when the query is completed  }
    0.1,    {  Fragment accumulation time: when there is a lot of feedback in the query, the feedback event will be triggered every time it is accumulated to facilitate batch operation. During the accumulation time, the data exists in memory  }
    0,      {  Query execution time, 0 is infinite  }
    0,      {  The maximum number of matching query entries, 0 is infinite  }
    1000,   {  Maximum query result feedback, 0 represents infinity  }
      procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    var
        queryRec_Ptr: POpCodeRun_Record;
    begin
        queryRec_Ptr := dPipe.UserPointer;

        queryRec_Ptr^.dPipe := dPipe;
        queryRec_Ptr^.qState := @qState;
        try
          Allowed := Boolean(queryRec_Ptr^.op.Execute(queryRec_Ptr^.opR));
          if Allowed then
            nop;
        except
          {  If an exception occurs in the query, end the query task immediately  }
          qState.Aborted := True;
          {  You can also end the query task by the following method  }
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
      {  This is all done after the query is completed  }

      {  Release OpRunTime classes that are no longer in use  }
      DisposeObject(queryRec_Ptr^.op);
      DisposeObject(queryRec_Ptr^.opR);

      {  When the query is completed, if the client has dropped the line, if the line drops, the memory will be released and no feedback will be given  }
      if not RecvTunnel.Exists(Cardinal(dPipe.UserVariant)) then
        begin
          Dispose(queryRec_Ptr);
          exit;
        end;

      {  When the query is completed, if the client is still online  }

      r_io_def := GetUserDefineRecvTunnel(RecvTunnel.PeerIO[dPipe.UserVariant]);

      {  Put the query results into mystream. Dpipe.outputdb.savetostream is equivalent to dataset.savetostream  }
      dPipe.OutputDB.Update;
      mystream := TMemoryStream64.Create;
      dPipe.OutputDB.SaveToStream(mystream);

      {  Now, we will send mystream to the client and feed back the statistical results of the client query and the callback trigger event  }

      {  Reset BatchStream  }
      ClearBatchStream(r_io_def.SendTunnel.Owner);
      PostBatchStream(r_io_def.SendTunnel.Owner, mystream, True);

      {  Memory address of feedback callback pointer  }
      de := TDataFrameEngine.Create;
      de.WritePointer(queryRec_Ptr^.Remote_backCall);
      de.WriteInteger(dPipe.OutputDB.Count);
      de.WriteString(dPipe.PipelineName);
      r_io_def.SendTunnel.Owner.SendDirectStreamCmd('QueryDone', de);
      DisposeObject(de);

      {  Reset BatchStream  }
      ClearBatchStream(r_io_def.SendTunnel.Owner);

      {  Finally, release the queryrec we use_PTR memory pointer  }
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

{  Main loop  }
procedure MainLoop;
var
  serv: TMyDoubleServer;
  rt, st: TZNet_Server;
  {  Simulate the use of physical databases  }
  i: integer;
  VT: TDBEngineVT;
begin
  doStatus('The demo uses ZDB as the data entity to simulate the transmission operation of dataset' +
    #13#10 +
    'Working idea of remote SQL for dataset:' +
    #13#10 +
    'On the client side, we first create a Record, which is a container used to store our query results' +
    'Then, we send a query command "QueryMyDatabase" to the server, with Record as a parameter' +
    'Next, the server uses query or self-defined data processing techniques to process your query conditions' +
    'After the server completes the query and processing of data, the server will feed back according to the combined command of batchstream' +
    #13#10 +
    'The BatchStream combination command mode is roughly these four steps' +
    #13#10 +
    '1. ClearBatchStream//Reset the remote BatchStream container' +
    '2. PostBatchStream//Start transmitting our query results' +
    '3. Sendqueryresult / / tells the client that the query result has been transmitted' +
    '4. ClearBatchStream//Reset the remote BatchStream container' +
    #13#10 +
    'These four steps of sending work are all done on the server' +
    'The reception and processing of these four steps are done at the client' +
    #13#10 +
    'In this demo, Delphi'#39's built-in Query and advanced database engine were not directly used' +
    'In this demo, ZDB is used to simulate query and advanced database query engine. Its working mechanism and implementation idea are the same as that of database engine');

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

  {  We use StableIO technology to build dual channel interactive services  }
  {  Stable IO can automatically disconnect and reconnect without destroying the instance  }
  {  Cmd_Cb_Directstream received data %s  }
  {  This demo is a dead loop service and will not exit, so the server will not be released. StableIO has no memory leaks  }
  serv := TMyDoubleServer.Create(rt, st);
  serv.RegisterCommand;

  {  Simulate physical databases  }
  serv.zdb := TZDBLocalManager.Create;
  {  Simulate database in memory  }
  serv.zdb.InitMemoryDB('mydb');
  {  Create 100000 data records  }
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
