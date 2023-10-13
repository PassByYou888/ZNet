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
  d: TDFE; {  DFE is a versatile storage structure that is preferred for small data and can support big data. However, it is not recommended to use DFE for big data. For example, for 100GB of data, DFE will first decode and then copy the structure, which is very hardware intensive  }
begin
  DFE_Mem.Clear;
  d := TDFE.Create;
  {  Serialization Write  }
  d.WriteStrings(Memo1.Lines); {  When DFE supports strings, it will be converted to UTF8 for storage, so there is no need to worry about data conversion  }
  d.WriteStrings(Memo2.Lines);
  d.Encrypt(DFE_Mem, True, 10, U_String('123456').Bytes); {  DFE supports encryption, compression, and built-in multiple encoding methods  }
  d.Free;
end;

procedure TSaveMyDocumentForm.dfe_load_ButtonClick(Sender: TObject);
var
  d: TDFE; {  DFE is a versatile storage structure that is preferred for small data and can support big data. However, it is not recommended to use DFE for big data. For example, for 100GB of data, DFE will first decode and then copy the structure, which is very hardware intensive  }
begin
  DFE_Mem.Position := 0;
  d := TDFE.Create;
  if not d.Decrypt(DFE_Mem, U_String('123456').Bytes) then {  The decoding of DFE has an automated recognition mechanism  }
    begin
      DoStatus('Password error or empty data');
      exit;
    end;
  {  Serialization read  }
  d.Reader.ReadStrings(Memo1.Lines);
  d.Reader.ReadStrings(Memo2.Lines);
  d.Free;
end;

procedure TSaveMyDocumentForm.ZDB2_save_ButtonClick(Sender: TObject);
var
  cipher_: TZDB2_Cipher;
  enc: TZDB2_File_Encoder; {  Big data preferred, natural multi-core support, balancing performance, security, big data support, ease of use, TZDB2_File_Encoder is not suitable for small data in tens of thousands  }
  tmp: TMS64;
begin
  ZDB2_Mem.Clear;
  {  ZDB2 encryptor  }
  cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csSerpent, '123456', 2, True, True);

  {  ZDB2 encoding engine with natural multi-core support  }
  enc := TZDB2_File_Encoder.Create(cipher_, ZDB2_Mem, 4);

  {  Enc uses ZDB2 kernel+IOThread+SelectCompression to compress data, which is very suitable for big data, images, videos, text files, and more  }
  tmp := TMS64.Create;
  Memo1.Lines.SaveToStream(tmp, TEncoding.UTF8);
  {  Throw the stream directly and automate multi-core encoding  }
  enc.EncodeFromStream(tmp, 32 * 1024, TSelectCompressionMethod.scmZLIB_Max, 1536);
  tmp.Free;

  {  Enc uses ZDB2 kernel+IOThread+SelectCompression to compress data, which is very suitable for big data, images, videos, text files, and more  }
  tmp := TMS64.Create;
  Memo2.Lines.SaveToStream(tmp, TEncoding.UTF8);
  {  Throw the stream directly and automate multi-core encoding  }
  enc.EncodeFromStream(tmp, 32 * 1024, TSelectCompressionMethod.scmZLIB_Max, 1536);
  tmp.Free;

  enc.Flush;
  enc.Free;
  cipher_.Free;
end;

procedure TSaveMyDocumentForm.ZDB2_Load_ButtonClick(Sender: TObject);
var
  cipher_: TZDB2_Cipher;
  dec: TZDB2_File_Decoder; {  Big data preferred, natural multi-core support, balancing performance, security, big data support, ease of use, TZDB2_File_Decoder is not suitable for small data in tens of thousands  }
  tmp: TMS64;
begin
  ZDB2_Mem.Position := 0;
  {  ZDB2 encryptor  }
  cipher_ := TZDB2_Cipher.Create(TCipherSecurity.csSerpent, '123456', 2, True, True);
  if not TZDB2_File_Decoder.Check(cipher_, ZDB2_Mem) then
    begin
      DoStatus('Password error or empty data');
      exit;
    end;

  {  ZDB2 decoding engine  }
  dec := TZDB2_File_Decoder.Create(cipher_, ZDB2_Mem, 2);

  tmp := TMS64.Create;
  dec.DecodeToStream(dec.Files[0], tmp);
  tmp.Position := 0;
  {  Automated multi-core decoding  }
  Memo1.Lines.LoadFromStream(tmp, TEncoding.UTF8);
  tmp.Free;

  tmp := TMS64.Create;
  dec.DecodeToStream(dec.Files[1], tmp);
  tmp.Position := 0;
  {  Automated multi-core decoding  }
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
