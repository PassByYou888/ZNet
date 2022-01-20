unit RealtimeDataFrameEncoderFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  Z.Core, Z.PascalStrings, Z.UnicodeMixedLib, Z.DFE, Z.MemoryStream;

type
  TRealtimeDataFrameEncoderForm = class(TForm)
    Memo1: TMemo;
    enPerfButton: TButton;
    EncStateLabel: TLabel;
    fpsTimer: TTimer;
    perfTimer: TTimer;
    dePerfButton: TButton;
    DecStateLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure enPerfButtonClick(Sender: TObject);
    procedure dePerfButtonClick(Sender: TObject);
    procedure perfTimerTimer(Sender: TObject);
  private
  public
    SourDF: TDFE;
    SourEncoded: TMS64;
    EncodeSizeOfPerSecond: TAtomInt64;
    DecodeSizeOfPerSecond: TAtomInt64;
    EncTestActivted: TAtomBool;
    DecTestActivted: TAtomBool;
    procedure DoTestDFEncoder;
    procedure DoTestDFDecoder;
  end;

var
  RealtimeDataFrameEncoderForm: TRealtimeDataFrameEncoderForm;

implementation

{$R *.dfm}


procedure TRealtimeDataFrameEncoderForm.dePerfButtonClick(Sender: TObject);
begin
  if DecTestActivted.V then
    begin
      DecTestActivted.V := False;
      DecStateLabel.Caption := '..';
      dePerfButton.Caption := 'Decoder Performance';
    end
  else
    begin
      dePerfButton.Caption := 'stop';
      DecTestActivted.V := True;
      TCompute.RunM_NP(DoTestDFDecoder);
    end;
end;

procedure TRealtimeDataFrameEncoderForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  SourDF := TDFE.Create;
  for i := 0 to 9 do
    begin
      SourDF.WriteString(TPascalString.RandomString(100, [cVisibled]));
      SourDF.WriteVariant(umlRandomRange(-10000, 10000));
      SourDF.WriteSingle(umlRandomRangeD(-10000, 10000));
      SourDF.WriteInt64(umlRandomRange(-10000, 10000));
    end;
  SourEncoded := TMS64.Create;
  SourDF.FastEncodeTo(SourEncoded);
  SourEncoded.Position := 0;

  EncodeSizeOfPerSecond := TAtomInt64.Create(0);
  DecodeSizeOfPerSecond := TAtomInt64.Create(0);
  EncTestActivted := TAtomBool.Create(False);
  DecTestActivted := TAtomBool.Create(False);
  EncStateLabel.Caption := '..';
  DecStateLabel.Caption := '..';
end;

procedure TRealtimeDataFrameEncoderForm.FormDestroy(Sender: TObject);
begin
  SourDF.Free;
  SourEncoded.Free;
  EncodeSizeOfPerSecond.Free;
  DecodeSizeOfPerSecond.Free;
  EncTestActivted.Free;
  DecTestActivted.Free;
end;

procedure TRealtimeDataFrameEncoderForm.enPerfButtonClick(Sender: TObject);
begin
  if EncTestActivted.V then
    begin
      EncTestActivted.V := False;
      EncStateLabel.Caption := '..';
      enPerfButton.Caption := 'Encoder Performance';
    end
  else
    begin
      enPerfButton.Caption := 'stop';
      EncTestActivted.V := True;
      TCompute.RunM_NP(DoTestDFEncoder);
    end;
end;

procedure TRealtimeDataFrameEncoderForm.perfTimerTimer(Sender: TObject);
begin
  if EncTestActivted.V then
    begin
      EncStateLabel.Caption := PFormat('per second: %s', [umlSizeToStr(EncodeSizeOfPerSecond.V).Text]);
      EncodeSizeOfPerSecond.V := 0;
    end;

  if DecTestActivted.V then
    begin
      DecStateLabel.Caption := PFormat('per second: %s', [umlSizeToStr(DecodeSizeOfPerSecond.V).Text]);
      DecodeSizeOfPerSecond.V := 0;
    end;
end;

procedure TRealtimeDataFrameEncoderForm.DoTestDFEncoder;
var
  m64: TMS64;
  siz_: Int64;
  num: Integer;
begin
  siz_ := 0;
  num := 0;
  m64 := TMS64.Create;
  while EncTestActivted.V do
    begin
      SourDF.FastEncodeTo(m64);
      inc(siz_, m64.Size);
      m64.Clear;
      inc(num);
      if num > 100 then
        begin
          with EncodeSizeOfPerSecond do
              UnLock(Lock + siz_);
          siz_ := 0;
          num := 0;
        end;
    end;
  m64.Free;
end;

procedure TRealtimeDataFrameEncoderForm.DoTestDFDecoder;
var
  m64: TMS64;
  tmp: TDFE;
  siz_: Int64;
  num: Integer;
begin
  siz_ := 0;
  num := 0;
  m64 := TMS64.Create;
  m64.Mapping(SourEncoded.Memory, SourEncoded.Size);
  while DecTestActivted.V do
    begin
      tmp := TDFE.Create;
      m64.Position := 0;
      tmp.DecodeFrom(m64, True);
      inc(siz_, m64.Size);
      tmp.Free;
      inc(num);
      if num > 100 then
        begin
          with DecodeSizeOfPerSecond do
              UnLock(Lock + siz_);
          siz_ := 0;
          num := 0;
        end;
    end;
  m64.Free;
end;

end.
 
