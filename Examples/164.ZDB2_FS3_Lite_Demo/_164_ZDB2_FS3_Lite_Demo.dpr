program _164_ZDB2_FS3_Lite_Demo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  Z.Core,
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status, Z.Notify, Z.Cadencer, Z.MemoryStream,
  Z.ZDB2,
  Z.ZDB2.Thread,
  Z.ZDB2.Thread.LiteData,
  Z.Net.C4_FS3.ZDB2.LiteData;

{
  FS3-Lite是C4-FS3系统的底层数据库引擎(使用ZDB2设计数据引擎),FS3-Lite用于直接操作数据,不需要经过网络

  使用FS3-Lite前必须了解的事情

  FS3-Lite的整个设计放弃了使用线程锁机制
  繁多线程锁会让设计环节无法深入,也会导致使用中的陷进,作为周全考虑,线程锁放在用户层来干,怎么调度,由用户自定

  FS3-Lite有管线概念:当写入速度>物理IO速度,数据就会在内存滞留
  长时间运行这会让系统崩溃,通常万兆以太跑不满物理带宽,只需要做到IO写入速度>网络存取速度,该问题也就迎刃而解
  使用FS3-Lite时一定要明确管线,IO读取->写入FS3->数据放入FS3写入队列->完成写入

  FS3-Lite的设计和结构,以符合计算机运行机理方式编写而出,请自行研究代码
  理解FS3-Lite使用细节最好的Demo是Z.Net.C4_FS3.pas库,这在监控系统被大量使用
}

type
  TFS3_Lite_Demo = class
  public
    cad: TCadencer;
    inst: TZDB2_FS3_Lite;
    constructor Create;
    destructor Destroy; override;
    procedure do_Progress(Sender: TObject; const deltaTime, newTime: Double);
    procedure Do_Write_File_Demo;
    procedure Do_Get_File_Demo;
    procedure Run_FS3_Lite_Demo;
  end;

constructor TFS3_Lite_Demo.Create;
begin
  inherited Create;
  cad := TCadencer.Create;
  cad.OnProgress := do_Progress;
  inst := TZDB2_FS3_Lite.Create(umlCombinePath(umlCurrentPath, 'FS3-Demo'));
  inst.Debug_Mode := True; // 开Debug是为了看见lite里面的运行状态

  // 将lite脚本生成为临时文件模式,当每次启动程序时会创建一个新数据库,同时,退出时也会直接删除数据库文件
  // 临时文件模式在脚本中,这里只是生成
  // 如果要取消临时文件模式,修改脚本即可
  inst.Build_Script_And_Open('Demo', True);
end;

destructor TFS3_Lite_Demo.Destroy;
begin
  DisposeObject(inst);
  DisposeObject(cad);
  inherited Destroy;
end;

procedure TFS3_Lite_Demo.do_Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
  inst.Check_Life(deltaTime);
end;

procedure TFS3_Lite_Demo.Do_Write_File_Demo;
var
  i: Integer;
  tmp: TMem64;
  n: U_String;
  post_tool: TZDB2_FS3_Sync_Post_Queue_Tool;
begin
  for i := 0 to 10 do // 仿真写入10个文件,每个文件生命周期为5秒
    begin
      tmp := TMem64.Create;
      tmp.Size := umlRR(64 * 1024, 1024 * 1024);
      TMT19937.Rand32(MaxInt, tmp.Memory, tmp.Size shr 2);

      n := TPascalString.RandomString(30);

      // Create_FI_From_LT_MD5会以数据md5作为重复数据检查
      // 如果发现md5重复,直接生成file-info链接,不再处理body
      // TZDB2_FS3_Sync_Post_Queue_Tool中已经具备了自动化的重复md5检查能力,这里在demo中写成代码是说明使用方法
      if inst.Create_FI_From_LT_MD5(n, umlNow, 5, tmp.ToMD5) then
        begin
          DelayFreeObj(1.0, tmp);
        end
      else
        begin
          // post方法受io读延迟影响:先读,再将读出数据放在内存,然后以队列方式写入
          post_tool := inst.Create_Sync_Post_Queue;

          // begin_post是预置生成
          post_tool.Begin_Post(n, tmp.Size, tmp.ToMD5, umlNow, 5);

          // post可以多次,例如一个10GB的大文件,可以每次post只写1M,通过多次post实现大文件写入
          post_tool.Post(tmp, True);

          // end_post会生成文件链接表,md5校验数据
          // End_Post_And_Free会先调用end_post再以后置方式释放
          post_tool.End_Post_And_Free;
        end;
    end;
end;

procedure TFS3_Lite_Demo.Do_Get_File_Demo;
var
  arry: TZDB2_FS3_FileInfo_Pair_Pool.TArray_Key;
  n: U_String;
  i: Integer;
  fi: TZDB2_FS3_FileInfo;
  md5_tool: TMD5_Tool;
begin
  if inst.FileInfo_Pool.Num < 5 then
      exit;

  // 实现随机抽取文件,先将全部文件生成数组
  arry := inst.FileInfo_Pool.ToArray_Key;

  for i := 0 to length(arry) - 1 do
    begin
      n := arry[i];
      // 根据文件名获取file-info的sequence-id(唯一id)
      fi := inst.FileInfo_Pool[n];
      md5_tool := TMD5_Tool.Create;
      // 根据sequence-id获取文件,Sync_Get_Data会等待每个文件全部读取完成才会返回出来
      // 一般来说,Sync_Get_Data可以放在多线程或则并发程序中调用
      inst.Sync_Get_Data_P(fi.Sequence_ID, 0, 0, procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean; Fragment: TMS64; Data_Pos: Int64)
        begin
          // 这里的事件会按序列每次返回文件数据的part,其part坐标由data_pos表示,这些数据要怎么处理自定
          // 返回的data_pos机制由低到高
          md5_tool.Update(Fragment.Memory, Fragment.Size);
        end,
        procedure(Sender: TZDB2_FS3_Lite; Successed: Boolean)
        begin
          // 完成状态
          if Successed then
            begin
              DoStatus('获取 %s 成功, md5:%s', [fi.File_Name.Text, umlMD5ToStr(md5_tool.FinalizeMD5).Text]);
            end
          else
              DoStatus('获取 %s 失败!!!!', [fi.File_Name.Text]);
          nop;
        end);
      DisposeObject(md5_tool);
    end;
  SetLength(arry, 0);
end;

procedure TFS3_Lite_Demo.Run_FS3_Lite_Demo;
var
  tk: TTimeTick;
  running: Boolean;
begin
  DoStatus('开始运行FS3-Lite写入Demo,10秒后以IO完成状态自动退出.');

  // 单独开一条写入线程,仿真生成文件
  // 写入线程无锁,一次开一条是安全的,如果要一次开多条仿真写线程,最好锁一下
  TCompute.RunM_NP(Do_Write_File_Demo, @running, nil);

  while running do
    begin
      cad.Progress;
      inst.Check_Recycle_Pool;
      CheckThread;
      TCompute.Sleep(100);
    end;
  Do_Get_File_Demo;
  DoStatus('全部仿真数据写入完成.');
  DoStatus('10秒后退出.');
  tk := GetTimeTick;
  while GetTimeTick - tk < 10000 do
    begin
      cad.Progress;
      inst.Check_Recycle_Pool;
      CheckThread;
      TCompute.Sleep(100);
    end;
end;

begin
  with TFS3_Lite_Demo.Create do
    begin
      Run_FS3_Lite_Demo;
      Free;
    end;

end.
