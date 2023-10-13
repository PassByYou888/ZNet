program _2_FS2_Multi_Client;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.MemoryStream,
  Z.Notify,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_FS2,
  Z.Net.C4_Console_APP;

function GetMyFS_Client: TC40_FS2_Client;
begin
  Result := TC40_FS2_Client(C40_ClientPool.ExistsConnectedServiceTyp('FS2'));
end;

var
  i: Integer;
  arry_tunnel: array of TC40_PhysicsTunnel;
  arry_fs2_tunnel: TC40_Custom_Client_Array;
  arry_m64: array of TMS64;
  PostFile_Num: Integer;
  PoolFrag_Num: Integer;

begin
  SetLength(arry_tunnel, 5);
  for i := low(arry_tunnel) to high(arry_tunnel) do
    begin
      arry_tunnel[i] := TC40_PhysicsTunnel.Create('127.0.0.1', 8387);
      arry_tunnel[i].ResetDepend('FS2');
      arry_tunnel[i].BuildDependNetwork();
    end;

  repeat
    Z.Net.C4.C40Progress;
    arry_fs2_tunnel := Z.Net.C4.C40_ClientPool.SearchServiceTyp('FS2', True);
  until length(arry_fs2_tunnel) = length(arry_tunnel); {  Ensure all connections are ready  }
  DoStatus('all connection ok.');

  {  After all are ready, C4 will automatically initiate disconnection and reconnection  }
  {  Here is a demonstration of uploading and downloading  }
  PostFile_Num := 0;
  SetMT19937Seed(0);
  SetLength(arry_m64, length(arry_fs2_tunnel));
  for i := low(arry_fs2_tunnel) to high(arry_fs2_tunnel) do
    begin
      arry_m64[i] := TMS64.Create;
      arry_m64[i].Size := 1024 * 1024;
      MT19937Rand32(MaxInt, arry_m64[i].Memory, arry_m64[i].Size shr 2);
      TC40_FS2_Client(arry_fs2_tunnel[i]).FS2_PostFile_P(True, umlMD5ToStr(arry_m64[i].ToMD5), arry_m64[i], True,
        procedure(Sender: TC40_FS2_Client; info_: U_String)
        begin
          Dec(PostFile_Num);
        end);
      inc(PostFile_Num);
    end;
  {  Wait for 10 upload tasks to be completed  }
  while PostFile_Num > 0 do
      Z.Net.C4.C40Progress;
  SetLength(arry_m64, 0);

  {  List file  }
  PoolFrag_Num := 0;
  for i := low(arry_fs2_tunnel) to high(arry_fs2_tunnel) do
    begin
      TC40_FS2_Client(arry_fs2_tunnel[i]).FS2_PoolFragP(procedure(Sender: TC40_FS2_Client; arry: TFS2_PoolFragInfo_Array)
        var
          info: TFS2_PoolFragInfo;
        begin
          for info in arry do
              DoStatus(info.FileName);
          Dec(PoolFrag_Num);
        end);
      inc(PoolFrag_Num);
    end;
  {  Wait for all 10 list tasks to be completed  }
  while PoolFrag_Num > 0 do
      Z.Net.C4.C40Progress;

  {  Main loop  }
  StatusThreadID := False;
  C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
