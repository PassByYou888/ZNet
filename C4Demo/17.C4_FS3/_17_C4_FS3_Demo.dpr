program _17_C4_FS3_Demo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  Z.Core,
  Z.PascalStrings,
  Z.UPascalStrings,
  Z.Status,
  Z.UnicodeMixedLib,
  Z.ListEngine,
  Z.Geometry2D,
  Z.DFE,
  Z.Json,
  Z.Expression,
  Z.OpCode,
  Z.Parsing,
  Z.Notify,
  Z.Cipher,
  Z.MemoryStream,
  Z.HashList.Templet,
  Z.ZDB2,
  Z.ZDB2.Thread.Queue,
  Z.ZDB2.Thread,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_Console_APP,
  Z.Net.C4_FS3,
  Z.Net.C4_FS3.ZDB2.LiteData;

procedure run_fs3_demo(sender: TC40_FS3_Client);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  m64.Size := 1024 * 1024 * 2 + 1999;
  TMT19937.Rand32(MaxInt, m64.Memory, m64.Size div 4);
  sender.Post_File_P('demo_file', umlNow(), 30.0, m64, True, procedure(sender: TC40_FS3_Client; Bridge_: TC40_FS3_Client_Post_File_Bridge; Successed: Boolean)
    begin
      if not Successed then
          exit;
      sender.Get_File_P('demo_file', 0, 0, TMS64.CustomCreate(1024 * 1024), procedure(sender: TC40_FS3_Client; Stream: TCore_Stream; MD5: TMD5; Successed: Boolean)
        begin
          if Successed then
              DoStatus('get-file md5:%s', [umlMD5ToStr(MD5).Text]);
          DisposeObject(Stream);

          sender.Get_File_List_P('', 0, procedure(sender: TC40_FS3_Client; arry: TC40_FS3_Client_File_List_Array)
            var
              i: Integer;
            begin
              for i := 0 to length(arry) - 1 do
                  DoStatus('%s size:%s', [arry[i].File_Name.Text, umlSizeToStr(arry[i].File_Size).Text]);
            end);
        end);
    end);
end;

begin
  // FS3服务器创建以后会在 ".exe\FS3\" 目录位置有许多.conf的配置脚本文件,这些.conf用于指定lite数据库参数
  // 扩展阵列,优化数据库性能,修改这些.conf配置脚本即可
  // FS3是尖端技术方案,在zdb2体系没有给出文档前,这里无法说明,靠自己去研究代码
  Z.Net.C4_Console_APP.C40AppParsingTextStyle := tsC;
  Z.Net.C4_Console_APP.C40AppParam := ['Service("0.0.0.0","127.0.0.1",9188,"FS3"),Client("127.0.0.1","9188","FS3")'];

  // 自动工具,如果TC40_FS3_Client已经完成连接,并且就绪
  TC40_Auto_Deployment_Client<TC40_FS3_Client>.Create_P(procedure(var sender: TC40_FS3_Client)
    begin
      run_fs3_demo(sender);
    end);

  if Z.Net.C4_Console_APP.C40_Extract_CmdLine then
      Z.Net.C4_Console_APP.C40_Execute_Main_Loop;
  Z.Net.C4.C40Clean;

end.
