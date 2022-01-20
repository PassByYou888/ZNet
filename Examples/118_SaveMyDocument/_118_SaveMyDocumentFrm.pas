unit _118_SaveMyDocumentFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  Z.Core, Z.PascalStrings, Z.MemoryStream, Z.UnicodeMixedLib, Z.Cipher, Z.Status,
  Z.DFE, Z.ZDB2, Z.ZDB2.FileEncoder, Z.IOThread;

type
  TSaveMyDocumentForm = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    Memo2: TMemo;
    Label2: TLabel;
    dfe_save_Button: TButton;
    dfe_load_Button: TButton;
    Label3: TLabel;
    LogMemo: TMemo;
    ZDB2_save_Button: TButton;
    ZDB2_Load_Button: TButton;
    Label4: TLabel;
    fpsTimer: TTimer;
    procedure dfe_save_ButtonClick(Sender: TObject);
    procedure dfe_load_ButtonClick(Sender: TObject);
    procedure ZDB2_save_ButtonClick(Sender: TObject);
    procedure ZDB2_Load_ButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure fpsTimerTimer(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
    DFE_Mem: TMS64;
    ZDB2_Mem: TMS64;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SaveMyDocumentForm: TSaveMyDocumentForm;

implementation

{$R *.dfm}


procedure TSaveMyDocumentForm.dfe_save_ButtonClick(Sender: TObject);
var
  d: TDFE; // DFE是万用存储结构，小数据首选，可以支持大数据，但大数据不建议用DFE，例如，100G的数据，DFE会先解码再copy结构，十分消耗硬件
begin
  DFE_Mem.Clear;
  d := TDFE.Create;
  // 序列化方式写入
  d.WriteStrings(Memo1.Lines); // DFE支持字符串时都会转UTF8来存，无需操心数据转换
  d.WriteStrings(Memo2.Lines);
  d.Encrypt(DFE_Mem, True, 10, U_String('123456').Bytes); // DFE支持加密，压缩，内置多种编码方式
  d.Free;
end;

procedure TSaveMyDocumentForm.dfe_load_ButtonClick(Sender: TObject);
var
  d: TDFE; // DFE是万用存储结构，小数据首选，可以支持大数据，但大数据不建议用DFE，例如，100G的数据，DFE会先解码再copy结构，十分消耗硬件
begin
  DFE_Mem.Position := 0;
  d := TDFE.Create;
  if not d.Decrypt(DFE_Mem, U_String('123456').Bytes) then // DFE的解码具有自动化识别机制
    begin
      DoStatus('密码错误或则数据空');
      exit;
    end;
  // 序列化方式读取
  d.Reader.ReadStrings(Memo1.Lines);
  d.Reader.ReadStrings(Memo2.Lines);
  d.Free;
end;

procedure TSaveMyDocumentForm.ZDB2_save_ButtonClick(Sender: TObject);
var
  cipher_: TZDB2_Cipher;
  enc: TZDB2_File_Encoder; // 大数据首选，天然多核支持，兼顾性能，安全，大数据支持，易用性，TZDB2_File_Encoder不适合用以万计的小数据
  tmp: TMS64;
begin
  ZDB2_Mem.Clear;
  // ZDB2加密器
  cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csSerpent, '123456', 2, True, True);

  // ZDB2编码引擎，天然多核支持
  enc := TZDB2_File_Encoder.Create(cipher_, ZDB2_Mem, 4);

  // enc使用ZDB2内核+IOThread+SelectCompression对数据进行压缩，这套方法很适合大数据，图片，视频，文本文件等等
  tmp := TMS64.Create;
  Memo1.Lines.SaveToStream(tmp, TEncoding.UTF8);
  // 直接把stream扔过去，自动化多核编码
  enc.EncodeFromStream(tmp, 32 * 1024, TSelectCompressionMethod.scmZLIB_Max, 1536);
  tmp.Free;

  // enc使用ZDB2内核+IOThread+SelectCompression对数据进行压缩，这套方法很适合大数据，图片，视频，文本文件等等
  tmp := TMS64.Create;
  Memo2.Lines.SaveToStream(tmp, TEncoding.UTF8);
  // 直接把stream扔过去，自动化多核编码
  enc.EncodeFromStream(tmp, 32 * 1024, TSelectCompressionMethod.scmZLIB_Max, 1536);
  tmp.Free;

  enc.Flush;
  enc.Free;
  cipher_.Free;
end;

procedure TSaveMyDocumentForm.ZDB2_Load_ButtonClick(Sender: TObject);
var
  cipher_: TZDB2_Cipher;
  dec: TZDB2_File_Decoder; // 大数据首选，天然多核支持，兼顾性能，安全，大数据支持，易用性，TZDB2_File_Decoder不适合用以万计的小数据
  tmp: TMS64;
begin
  ZDB2_Mem.Position := 0;
  // ZDB2加密器
  cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csSerpent, '123456', 2, True, True);
  if not TZDB2_File_Decoder.Check(cipher_, ZDB2_Mem) then
    begin
      DoStatus('密码错误或则数据空');
      exit;
    end;

  // ZDB2解码引擎
  dec := TZDB2_File_Decoder.Create(cipher_, ZDB2_Mem, 2);

  tmp := TMS64.Create;
  dec.DecodeToStream(dec.Files[0], tmp);
  tmp.Position := 0;
  // 自动化多核解码
  Memo1.Lines.LoadFromStream(tmp, TEncoding.UTF8);
  tmp.Free;

  tmp := TMS64.Create;
  dec.DecodeToStream(dec.Files[1], tmp);
  tmp.Position := 0;
  // 自动化多核解码
  Memo2.Lines.LoadFromStream(tmp, TEncoding.UTF8);
  tmp.Free;

  dec.Free;
  cipher_.Free;
end;

procedure TSaveMyDocumentForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DeleteDoStatusHook(self);
end;

procedure TSaveMyDocumentForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  LogMemo.Lines.Add(Text_);
end;

constructor TSaveMyDocumentForm.Create(AOwner: TComponent);
begin
  inherited;
  AddDoStatusHook(self, DoStatus_backcall);
  DFE_Mem := TMS64.Create;
  ZDB2_Mem := TMS64.Create;
end;

destructor TSaveMyDocumentForm.Destroy;
begin
  DFE_Mem.Free;
  ZDB2_Mem.Free;
  inherited;
end;

procedure TSaveMyDocumentForm.fpsTimerTimer(Sender: TObject);
begin
  CheckThread;
end;

end.
