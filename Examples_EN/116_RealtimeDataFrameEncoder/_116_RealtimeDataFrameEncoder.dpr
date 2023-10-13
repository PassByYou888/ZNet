program _116_RealtimeDataFrameEncoder;

uses
  Vcl.Forms,
  RealtimeDataFrameEncoderFrm in 'RealtimeDataFrameEncoderFrm.pas' {RealtimeDataFrameEncoderForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TRealtimeDataFrameEncoderForm, RealtimeDataFrameEncoderForm);
  Application.Run;
end.
