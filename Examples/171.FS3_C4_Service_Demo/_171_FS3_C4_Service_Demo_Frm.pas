unit _171_FS3_C4_Service_Demo_Frm;

// 本Demo是把服务器和前端直接放在一个app,同时app也没有使用双主线程技术这些东西
// 上传文件可能会出现小卡:这是服务器多线程机制,服务器统一使用主线程来保证数据调度一致,避免这个机制需要在服务器开个线程调度器,懒得麻烦直接跑
// 耐心好的程序,可以自己动手把FS3的服务器和前端分开,独立跑,这就是不会出现小卡情况了
// 另外,FS3支持上亿的文件数量,这里使用listview显列表只是demo作用
// by.qq600585

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,

  Vcl.FileCtrl,

  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib,
  Z.Status, Z.Notify,
  Z.Net, Z.Net.C4, Z.HashList.Templet,
  Z.Net.C4_FS3, Z.Net.C4_Console_APP, Z.Parsing, Z.OpCode;

type
  TTFS3_C4_Service_Demo_Form = class(TForm, IC40_PhysicsTunnel_Event)
    SysTimer: TTimer;
    Log_Memo: TMemo;
    LV: TListView;
    Tool_Panel: TPanel;
    bot_Splitter: TSplitter;
    Upload_Button: TButton;
    Download_Button: TButton;
    Remove_Button: TButton;
    Refresh_Button: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure SysTimerTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Upload_ButtonClick(Sender: TObject);
    procedure Remove_ButtonClick(Sender: TObject);
    procedure Download_ButtonClick(Sender: TObject);
    procedure Refresh_ButtonClick(Sender: TObject);
  private // c4接口事件
    procedure C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
    procedure C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
    procedure C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
  private
    procedure backcall_DoStatus(Text_: SystemString; const ID: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh_List;
    procedure Upload_Files(files_: TStrings);
    procedure Download_File(file_name, save_to: U_String);
  end;

var
  TFS3_C4_Service_Demo_Form: TTFS3_C4_Service_Demo_Form;
  // FS3是先进的散列文件存储系统,稍微修改即可做成网盘,各种数据库
  // FS3的内部机理和设计思路没有demo和文档,需要靠自己研究摸索,给我留言也可
  FS3_Cli: TC40_FS3_Client;

implementation

{$R *.dfm}


procedure TTFS3_C4_Service_Demo_Form.SysTimerTimer(Sender: TObject);
begin
  CheckThread;
end;

procedure TTFS3_C4_Service_Demo_Form.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  C40Clean();
  CanClose := False;
  OnCloseQuery := nil;
  SysPost.PostExecuteP_NP(0, procedure
    begin
      Close;
    end);
end;

procedure TTFS3_C4_Service_Demo_Form.Upload_ButtonClick(Sender: TObject);
begin
  if not OpenDialog.Execute then
      exit;
  Upload_Files(OpenDialog.Files);
end;

procedure TTFS3_C4_Service_Demo_Form.Remove_ButtonClick(Sender: TObject);
var
  i: Integer;
  itm: TListItem;
begin
  if FS3_Cli = nil then
      exit;
  for i := 0 to LV.Items.Count - 1 do
    begin
      itm := LV.Items[i];
      if itm.Selected then
          FS3_Cli.Remove_File(StrToInt64(itm.SubItems[3]));
    end;
  SysPost.PostExecuteM_NP(1.0, Refresh_List);
end;

procedure TTFS3_C4_Service_Demo_Form.Download_ButtonClick(Sender: TObject);
var
  s: string;
  i: Integer;
  itm: TListItem;
begin
  if LV.SelCount = 1 then
    begin
      if not SaveDialog.Execute then
          exit;
      Download_File(LV.Selected.Caption, SaveDialog.FileName);
    end
  else if LV.SelCount > 1 then
    begin
      s := C40_RootPath;
      if not SelectDirectory('下载到目录', '', s) then
          exit;
      for i := 0 to LV.Items.Count - 1 do
        begin
          itm := LV.Items[i];
          if itm.Selected then
              Download_File(LV.Items[i].Caption, umlCombineFileName(s, LV.Items[i].Caption));
        end;
    end;
end;

procedure TTFS3_C4_Service_Demo_Form.Refresh_ButtonClick(Sender: TObject);
begin
  Refresh_List;
end;

procedure TTFS3_C4_Service_Demo_Form.C40_PhysicsTunnel_Connected(Sender: TC40_PhysicsTunnel);
begin

end;

procedure TTFS3_C4_Service_Demo_Form.C40_PhysicsTunnel_Disconnect(Sender: TC40_PhysicsTunnel);
begin
  FS3_Cli := nil;
end;

procedure TTFS3_C4_Service_Demo_Form.C40_PhysicsTunnel_Build_Network(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
end;

procedure TTFS3_C4_Service_Demo_Form.C40_PhysicsTunnel_Client_Connected(Sender: TC40_PhysicsTunnel; Custom_Client_: TC40_Custom_Client);
begin
  if Custom_Client_ is TC40_FS3_Client then
    begin
      FS3_Cli := Custom_Client_ as TC40_FS3_Client;
    end;
end;

procedure TTFS3_C4_Service_Demo_Form.backcall_DoStatus(Text_: SystemString; const ID: Integer);
begin
  if Log_Memo.Lines.Count > 5000 then
    begin
      Log_Memo.Lines.BeginUpdate;
      Log_Memo.Lines.Clear;
      Log_Memo.Lines.EndUpdate;
    end;
  Log_Memo.Lines.Add(Text_);
end;

constructor TTFS3_C4_Service_Demo_Form.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AddDoStatusHook(self, backcall_DoStatus);
  C40SetQuietMode(True);

  // C4框架使用脚本构建连接,这主要是解决服务器部署繁琐这些问题
  On_C40_PhysicsTunnel_Event_Console := self;
  C40_Extract_CmdLine(TSC, [
    'Service("0.0.0.0", "127.0.0.1", "9778", "FS3@Lite_Temp_Runtime=False")',
    'Connect("127.0.0.1", "9778", "FS3")'
    ]);

  // TC40_Auto_Deploy连接就绪以后的客户端,触发事件
  TC40_Auto_Deploy<TC40_FS3_Client>.Create_P(procedure(var Sender: TC40_FS3_Client)
    begin
      Refresh_List;
    end);
end;

destructor TTFS3_C4_Service_Demo_Form.Destroy;
begin
  RemoveDoStatusHook(self);
  inherited Destroy;
end;

procedure TTFS3_C4_Service_Demo_Form.Refresh_List;
begin
  if FS3_Cli = nil then
      exit;
  FS3_Cli.Get_File_List_P('*', 0, procedure(Sender: TC40_FS3_Client; arry: TC40_FS3_Client_File_List_Array)
    var
      i: Integer;
      itm: TListItem;
    begin
      LV.Items.BeginUpdate;
      LV.Items.Clear;
      for i := 0 to length(arry) - 1 do
        begin
          itm := LV.Items.Add;
          itm.Caption := arry[i].file_name;
          itm.SubItems.Add(umlSizeToStr(arry[i].File_Size));
          itm.SubItems.Add(umlDT(arry[i].File_Time));
          itm.SubItems.Add(FloatToStr(arry[i].File_Life));
          itm.SubItems.Add(IntToStr(arry[i].File_ID));
        end;
      LV.Items.EndUpdate;
      LV.Height := LV.Height + 1;
    end);
end;

procedure TTFS3_C4_Service_Demo_Form.Upload_Files(files_: TStrings);
var
  i: Integer;
  fn: U_String;
  fs: TFileStream;
begin
  if FS3_Cli = nil then
      exit;
  for i := 0 to files_.Count - 1 do
    begin
      fn := files_[i];
      if umlFileExists(fn) then
        begin
          fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
          FS3_Cli.Post_File_P(umlGetFileName(fn), umlGetFileTime(fn), 5 * 60 * 1000, fs, True,
            procedure(Sender: TC40_FS3_Client; Bridge_: TC40_FS3_Client_Post_File_Bridge; Successed: Boolean)
            begin
              DoStatus('文件上传 %s %s', [Bridge_.file_name.Text, if_(Successed, '成功', '失败')]);
            end);
        end;
    end;
end;

procedure TTFS3_C4_Service_Demo_Form.Download_File(file_name, save_to: U_String);
begin
  if FS3_Cli = nil then
      exit;
  FS3_Cli.Get_File_P(file_name, 0, 0, TFileStream.Create(save_to, fmCreate),
    procedure(Sender: TC40_FS3_Client; Stream: TCore_Stream; MD5: TMD5; Successed: Boolean)
    begin
      DoStatus('文件下载 %s %s', [TFileStream(Stream).FileName, if_(Successed, '成功', '失败')]);
      DisposeObject(Stream);
    end);
end;

end.
