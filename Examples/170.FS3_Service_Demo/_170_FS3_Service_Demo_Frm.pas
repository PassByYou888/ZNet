unit _170_FS3_Service_Demo_Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,

  Vcl.FileCtrl,

  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib,
  Z.Status, Z.Notify,
  Z.Net, Z.Net.C4, Z.HashList.Templet,
  Z.Net.C4_VM_FS3;

type
  T_170_FS3_Service_Demo_Form = class(TForm)
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
  _170_FS3_Service_Demo_Form: T_170_FS3_Service_Demo_Form;

  // 在ZNet-C4框架中含有VM字样的服务,都是可以独立运行的,这就是传统的CS模型
  // 反之,必须使用C4框架来运行
  // 在项目实际开发中,可以根据需要,灵活选择使用C4 or VM框架

  // FS3是先进的散列文件存储系统,稍微修改即可做成网盘,各种数据库
  // FS3的内部机理和设计思路没有demo和文档,需要靠自己研究摸索,给我留言也可
  FS3_Serv: TC40_FS3_VM_Service;
  FS3_Cli: TC40_FS3_VM_Client;

implementation

{$R *.dfm}


procedure T_170_FS3_Service_Demo_Form.SysTimerTimer(Sender: TObject);
begin
  CheckThread;
end;

procedure T_170_FS3_Service_Demo_Form.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  OnCloseQuery := nil;
  SysPost.PostExecuteP_NP(0, procedure
    begin
      DisposeObject(FS3_Cli);
      DisposeObject(FS3_Serv);
      Close;
    end);
end;

procedure T_170_FS3_Service_Demo_Form.Upload_ButtonClick(Sender: TObject);
begin
  if not OpenDialog.Execute then
      exit;
  Upload_Files(OpenDialog.Files);
end;

procedure T_170_FS3_Service_Demo_Form.Remove_ButtonClick(Sender: TObject);
var
  i: Integer;
  itm: TListItem;
begin
  for i := 0 to LV.Items.Count - 1 do
    begin
      itm := LV.Items[i];
      if itm.Selected then
          FS3_Cli.Remove_File(StrToInt64(itm.SubItems[3]));
    end;
  SysPost.PostExecuteM_NP(1.0, Refresh_List);
end;

procedure T_170_FS3_Service_Demo_Form.Download_ButtonClick(Sender: TObject);
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

procedure T_170_FS3_Service_Demo_Form.Refresh_ButtonClick(Sender: TObject);
begin
  Refresh_List;
end;

procedure T_170_FS3_Service_Demo_Form.backcall_DoStatus(Text_: SystemString; const ID: Integer);
begin
  if Log_Memo.Lines.Count > 5000 then
    begin
      Log_Memo.Lines.BeginUpdate;
      Log_Memo.Lines.Clear;
      Log_Memo.Lines.EndUpdate;
    end;
  Log_Memo.Lines.Add(Text_);
end;

constructor T_170_FS3_Service_Demo_Form.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AddDoStatusHook(self, backcall_DoStatus);
  C40SetQuietMode(True);
  FS3_Serv := TC40_FS3_VM_Service.Create('Lite_Temp_Runtime=False');
  FS3_Cli := TC40_FS3_VM_Client.Create('');
  SysPost.PostExecuteP_NP(0.5, procedure
    begin
      FS3_Serv.StartService('0.0.0.0', '9777', '123456');
      SysPost.PostExecuteP_NP(0.5, procedure
        begin
          FS3_Cli.Connect_P('127.0.0.1', '9777', '123456', procedure(const state: Boolean)
            begin
              SysPost.PostExecuteM_NP(1.0, Refresh_List);
            end);
        end);
    end);
end;

destructor T_170_FS3_Service_Demo_Form.Destroy;
begin
  RemoveDoStatusHook(self);
  inherited Destroy;
end;

procedure T_170_FS3_Service_Demo_Form.Refresh_List;
begin
  FS3_Cli.Get_File_List_P('*', 0, procedure(Sender: TC40_FS3_VM_Client; arry: TC40_FS3_VM_Client_File_List_Array)
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

procedure T_170_FS3_Service_Demo_Form.Upload_Files(files_: TStrings);
var
  i: Integer;
  fn: U_String;
  fs: TFileStream;
begin
  for i := 0 to files_.Count - 1 do
    begin
      fn := files_[i];
      if umlFileExists(fn) then
        begin
          fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
          FS3_Cli.Post_File_P(umlGetFileName(fn), umlGetFileTime(fn), 5 * 60 * 1000, fs, True,
            procedure(Sender: TC40_FS3_VM_Client; Bridge_: TC40_FS3_VM_Client_Post_File_Bridge; Successed: Boolean)
            begin
              DoStatus('文件上传 %s %s', [Bridge_.file_name.Text, if_(Successed, '成功', '失败')]);
            end);
        end;
    end;
end;

procedure T_170_FS3_Service_Demo_Form.Download_File(file_name, save_to: U_String);
begin
  FS3_Cli.Get_File_P(file_name, 0, 0, TFileStream.Create(save_to, fmCreate),
    procedure(Sender: TC40_FS3_VM_Client; Stream: TCore_Stream; MD5: TMD5; Successed: Boolean)
    begin
      DoStatus('文件下载 %s %s', [TFileStream(Stream).FileName, if_(Successed, '成功', '失败')]);
      DisposeObject(Stream);
    end);
end;

end.
