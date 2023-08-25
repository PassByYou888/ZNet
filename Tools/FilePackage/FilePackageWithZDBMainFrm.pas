unit FilePackageWithZDBMainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  ObjectDataManagerFrameUnit, Z.MemoryStream, Z.ZDB.HashField_LIB, Z.ZDB,
  Z.UnicodeMixedLib, Z.Core, Z.Status, Z.PascalStrings, Z.UPascalStrings, Z.ZDB.FileIndexPackage_LIB;

type
  TFilePackageWithZDBMainForm = class(TForm, IMemoryStream64ReadWriteTrigger)
    TopPanel: TPanel;
    NewButton: TButton;
    OpenButton: TButton;
    SaveButton: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SaveAsButton: TButton;
    Memo: TMemo;
    CacheStateMemo: TMemo;
    Timer: TTimer;
    CompressAsButton: TButton;
    Bevel3: TBevel;
    SaveAsCompressedDialog: TSaveDialog;
    Log_Splitter: TSplitter;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel7: TBevel;
    BuildIndexPackageButton: TButton;
    NewCustomButton: TButton;
    ParallelCompressAsButton: TButton;
    SaveAsParallelCompressedDialog: TSaveDialog;
    SaveAsZDB2Button: TButton;
    SaveAsZDB2Dialog: TSaveDialog;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TimerTimer(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure NewCustomButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SaveAsButtonClick(Sender: TObject);
    procedure CompressAsButtonClick(Sender: TObject);
    procedure ParallelCompressAsButtonClick(Sender: TObject);
    procedure SaveAsZDB2ButtonClick(Sender: TObject);
    procedure BuildIndexPackageButtonClick(Sender: TObject);
  private
    FDBEng: TObjectDataManager;
    FDBManFrame: TObjectDataManagerFrame;
    FTotalRead, FTotalWrite: Int64;
    FOpenFile: U_String;
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
    procedure TriggerWrite64(Count: Int64);
    procedure TriggerRead64(Count: Int64);
    procedure Disable_All;
    procedure Enabled_All;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NewPackage;
    procedure OpenFile_Th(thSender: TCompute);
    procedure OpenFile_Th_Done(thSender: TCompute);
    procedure OpenFile(fileName: SystemString);
    procedure SaveToFile_Th(thSender: TCompute);
    procedure SaveToFile(fileName: SystemString);
    procedure SaveTo_OXC_File_Th(thSender: TCompute);
    procedure SaveTo_OXC_File(fileName: SystemString);
    procedure SaveTo_OXP_File_Th(thSender: TCompute);
    procedure SaveTo_OXP_File(fileName: SystemString);
    procedure SaveTo_ZDB2_File_Th(thSender: TCompute);
    procedure SaveTo_ZDB2_File(fileName: SystemString);
  end;

var
  FilePackageWithZDBMainForm: TFilePackageWithZDBMainForm;

implementation

{$R *.dfm}


uses BuildIndexPackageOptFrm, NewDBOptFrm;

procedure TFilePackageWithZDBMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
end;

procedure TFilePackageWithZDBMainForm.TimerTimer(Sender: TObject);
begin
  if FDBEng <> nil then
      CacheStateMemo.Text := Format('File Size:%s IO Read:%s IO Write:%s',
      [umlSizeToStr(FDBEng.Size).Text, umlSizeToStr(FTotalRead).Text, umlSizeToStr(FTotalWrite).Text]);
  CheckThread;
end;

procedure TFilePackageWithZDBMainForm.NewButtonClick(Sender: TObject);
begin
  FTotalRead := 0;
  FTotalWrite := 0;
  FDBManFrame.ResourceData := nil;
  disposeObject(FDBEng);
  FDBEng := TObjectDataManager.CreateAsStream(TMemoryStream64OfReadWriteTrigger.Create(Self), '', ObjectDataMarshal.ID, False, True, True);
  FDBManFrame.ResourceData := FDBEng;
  FOpenFile := '';

  FDBEng.UpdateIO;
  FDBEng.StreamEngine.Position := 0;
  DoStatus('new DB. [fixed string size: %d]', [FDBEng.Handle^.FixedStringL]);
end;

procedure TFilePackageWithZDBMainForm.NewCustomButtonClick(Sender: TObject);
var
  l: Integer;
begin
  if NewDBOptForm.ShowModal <> mrOk then
      exit;
  l := umlClamp(umlStrToInt(NewDBOptForm.FixedStringEdit.Text, 65), 10, $FF);

  FTotalRead := 0;
  FTotalWrite := 0;
  FDBManFrame.ResourceData := nil;
  disposeObject(FDBEng);
  FDBEng := TObjectDataManager.CreateAsStream(Byte(l), TMemoryStream64OfReadWriteTrigger.Create(Self), '', ObjectDataMarshal.ID, False, True, True);
  FDBManFrame.ResourceData := FDBEng;
  FOpenFile := '';

  FDBEng.UpdateIO;
  FDBEng.StreamEngine.Position := 0;
  DoStatus('new DB. [fixed string size: %d]', [FDBEng.Handle^.FixedStringL]);
end;

procedure TFilePackageWithZDBMainForm.OpenButtonClick(Sender: TObject);
begin
  if not OpenDialog.Execute then
      exit;

  OpenFile(OpenDialog.fileName);
end;

procedure TFilePackageWithZDBMainForm.SaveButtonClick(Sender: TObject);
begin
  if FOpenFile = '' then
    if not SaveDialog.Execute then
        exit;

  if FOpenFile = '' then
    begin
      FOpenFile := SaveDialog.fileName;
    end;

  Caption := PFormat('Package: %s', [umlGetFileName(FOpenFile).Text]);
  Application.Title := PFormat('Package: %s', [umlGetFileName(FOpenFile).Text]);

  SaveToFile(FOpenFile);
end;

procedure TFilePackageWithZDBMainForm.SaveAsButtonClick(Sender: TObject);
begin
  if not SaveDialog.Execute then
      exit;
  FOpenFile := SaveDialog.fileName;

  Caption := PFormat('Package: %s', [umlGetFileName(FOpenFile).Text]);
  Application.Title := PFormat('Package: %s', [umlGetFileName(FOpenFile).Text]);

  SaveToFile(FOpenFile);
end;

procedure TFilePackageWithZDBMainForm.CompressAsButtonClick(Sender: TObject);
begin
  SaveAsCompressedDialog.fileName := umlChangeFileExt(FOpenFile, SaveAsCompressedDialog.DefaultExt);
  if not SaveAsCompressedDialog.Execute then
      exit;
  SaveTo_OXC_File(SaveAsCompressedDialog.fileName);
end;

procedure TFilePackageWithZDBMainForm.ParallelCompressAsButtonClick(Sender: TObject);
var
  m64, C64: TMS64;
  fn: string;
begin
  SaveAsParallelCompressedDialog.fileName := umlChangeFileExt(FOpenFile, SaveAsParallelCompressedDialog.DefaultExt);
  if not SaveAsParallelCompressedDialog.Execute then
      exit;
  SaveTo_OXP_File(SaveAsParallelCompressedDialog.fileName);
end;

procedure TFilePackageWithZDBMainForm.SaveAsZDB2ButtonClick(Sender: TObject);
begin
  SaveAsZDB2Dialog.fileName := umlChangeFileExt(FOpenFile, SaveAsZDB2Dialog.DefaultExt);
  if not SaveAsZDB2Dialog.Execute then
      exit;
  SaveTo_ZDB2_File(SaveAsZDB2Dialog.fileName);
end;

procedure TFilePackageWithZDBMainForm.BuildIndexPackageButtonClick(Sender: TObject);
var
  destDB: TObjectDataManager;
begin
  if BuildIndexPackageOptForm.ShowModal <> mrOk then
      exit;
  destDB := TObjectDataManager.CreateNew(FDBEng.Handle^.FixedStringL, BuildIndexPackageOptForm.DestDBEdit.Text, DBMarshal.ID);
  destDB.OverWriteItem := False;
  BuildIndexPackage(FDBEng, destDB, ParallelCompressStream_C, BuildIndexPackageOptForm.DataPathEdit.Text);
  if CheckIndexPackage(destDB, BuildIndexPackageOptForm.DataPathEdit.Text) then
      DoStatus('check index package: no error.');
  disposeObject(destDB);
  if messageDlg(Format('Do you want to open the "%s" file?', [BuildIndexPackageOptForm.DestDBEdit.Text]), mtInformation, [mbYes, mbNO], 0) <> mrYes then
      exit;
  OpenFile(BuildIndexPackageOptForm.DestDBEdit.Text);
end;

procedure TFilePackageWithZDBMainForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  if Memo.Lines.Count > 2000 then
      Memo.Lines.Clear;
  Memo.Lines.Add(PFormat('%s - %s', [TimeToStr(Time), Text_]));
end;

procedure TFilePackageWithZDBMainForm.TriggerWrite64(Count: Int64);
begin
  AtomInc(FTotalWrite, Count);
end;

procedure TFilePackageWithZDBMainForm.TriggerRead64(Count: Int64);
begin
  AtomInc(FTotalRead, Count);
end;

procedure TFilePackageWithZDBMainForm.Disable_All;
begin
  TopPanel.Enabled := False;
  FDBManFrame.Enabled := False;
end;

procedure TFilePackageWithZDBMainForm.Enabled_All;
begin
  TopPanel.Enabled := True;
  FDBManFrame.Enabled := True;
end;

constructor TFilePackageWithZDBMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StatusThreadID := False;
  FTotalRead := 0;
  FTotalWrite := 0;
  AddDoStatusHook(Self, DoStatus_backcall);

  FDBEng := nil;

  FDBManFrame := TObjectDataManagerFrame.Create(Self);
  FDBManFrame.Parent := Self;
  FDBManFrame.Align := alClient;
  FDBManFrame.ResourceData := nil;
  FOpenFile := '';

  NewButtonClick(NewButton);
end;

destructor TFilePackageWithZDBMainForm.Destroy;
begin
  DeleteDoStatusHook(Self);
  disposeObject(FDBManFrame);
  disposeObject(FDBEng);
  inherited Destroy;
end;

procedure TFilePackageWithZDBMainForm.NewPackage;
begin
  FTotalRead := 0;
  FTotalWrite := 0;
  FDBManFrame.ResourceData := nil;
  disposeObject(FDBEng);
  FDBEng := TObjectDataManager.CreateAsStream(TMemoryStream64OfReadWriteTrigger.Create(Self), '', ObjectDataMarshal.ID, False, True, True);
  FDBManFrame.ResourceData := FDBEng;
  FOpenFile := '';

  FDBEng.UpdateIO;
  FDBEng.StreamEngine.Position := 0;
  DoStatus('new DB. [fixed string size: %d]', [FDBEng.Handle^.FixedStringL]);
end;

procedure TFilePackageWithZDBMainForm.OpenFile_Th(thSender: TCompute);
var
  m64, C64: TMS64;
begin
  m64 := TMS64(thSender.UserObject);
  if umlMultipleMatch(True, '*.OXC', FOpenFile) then
    begin
      C64 := TMS64.Create;
      try
        C64.LoadFromFile(FOpenFile);
        C64.Position := 0;
        DecompressStream(C64, m64);
        m64.Position := 0;
        FOpenFile := '';
      except
        disposeObject(C64);
        C64 := nil;
        m64.Clear;
        m64.LoadFromFile(FOpenFile);
        m64.Position := 0;
      end;
      disposeObject(C64);
    end
  else if umlMultipleMatch(True, '*.OXP', FOpenFile) then
    begin
      C64 := TMS64.Create;
      try
        C64.LoadFromFile(FOpenFile);
        C64.Position := 0;
        ParallelDecompressStream(C64, m64);
        m64.Position := 0;
        FOpenFile := '';
      except
        disposeObject(C64);
        C64 := nil;
        m64.Clear;
        m64.LoadFromFile(FOpenFile);
        m64.Position := 0;
      end;
      disposeObject(C64);
    end
  else
    begin
      m64.LoadFromFile(FOpenFile);
      m64.Position := 0;
    end;
end;

procedure TFilePackageWithZDBMainForm.OpenFile_Th_Done(thSender: TCompute);
var
  m64: TMS64;
begin
  m64 := TMS64(thSender.UserObject);
  FTotalRead := 0;
  FTotalWrite := 0;
  m64.Position := 0;
  FDBEng := TObjectDataManager.CreateAsStream(m64, '', ObjectDataMarshal.ID, False, False, True);
  FDBManFrame.ResourceData := FDBEng;
  DoStatus('open %s [fixed string size: %d]', [FOpenFile.Text, FDBEng.Handle^.FixedStringL]);
  Caption := PFormat('Package: %s', [umlGetFileName(FOpenFile).Text]);
  Application.Title := PFormat('Package: %s', [umlGetFileName(FOpenFile).Text]);
  BuildIndexPackageOptForm.DestDBEdit.Text := umlChangeFileExt(FOpenFile, '') + '_index.OX';
  BuildIndexPackageOptForm.DataPathEdit.Text := umlCombinePath(umlGetFilePath(FOpenFile), 'DataCache\');
  Enabled_All;
end;

procedure TFilePackageWithZDBMainForm.OpenFile(fileName: SystemString);
var
  m64: TMS64;
begin
  FOpenFile := fileName;
  FDBManFrame.ResourceData := nil;
  disposeObjectAndNil(FDBEng);

  m64 := TMemoryStream64OfReadWriteTrigger.Create(Self);
  m64.Delta := 8 * 1024 * 1024;

  Disable_All;
  TCompute.RunM(nil, m64, OpenFile_Th, OpenFile_Th_Done);
end;

procedure TFilePackageWithZDBMainForm.SaveToFile_Th(thSender: TCompute);
var
  p: PString;
  stream: TFileStream;
begin
  p := PString(thSender.UserData);

  stream := TFileStream.Create(p^, fmCreate);
  try
    FDBEng.SaveToStream(stream);
    DoStatus('save %s', [p^]);
  finally
    disposeObject(stream);
    dispose(p);
  end;
  MainThreadProgress.PostM1(Enabled_All);
end;

procedure TFilePackageWithZDBMainForm.SaveToFile(fileName: SystemString);
var
  p: PString;
begin
  Disable_All;
  new(p);
  p^ := fileName;
  TCompute.RunM(p, nil, SaveToFile_Th, TRun_Thread_M(nil));
end;

procedure TFilePackageWithZDBMainForm.SaveTo_OXC_File_Th(thSender: TCompute);
var
  p: PString;
  m64, C64: TMS64;
begin
  p := PString(thSender.UserData);
  m64 := TMS64.Create;
  C64 := TMS64.Create;
  try
    FDBEng.SaveToStream(m64);
    m64.Position := 0;

    m64.Position := 0;
    MaxCompressStream(m64, C64);

    C64.SaveToFile(p^);

    DoStatus('save as Compressed %s (source:%s compressed:%s)', [p^, umlSizeToStr(m64.Size).Text, umlSizeToStr(C64.Size).Text]);
  finally
    disposeObject([m64, C64]);
    dispose(p);
  end;
  MainThreadProgress.PostM1(Enabled_All);
end;

procedure TFilePackageWithZDBMainForm.SaveTo_OXC_File(fileName: SystemString);
var
  p: PString;
begin
  Disable_All;
  new(p);
  p^ := fileName;
  TCompute.RunM(p, nil, SaveTo_OXC_File_Th, TRun_Thread_M(nil));
end;

procedure TFilePackageWithZDBMainForm.SaveTo_OXP_File_Th(thSender: TCompute);
var
  p: PString;
  m64, C64: TMS64;
begin
  p := PString(thSender.UserData);

  m64 := TMS64.Create;
  C64 := TMS64.Create;
  try
    FDBEng.SaveToStream(m64);
    m64.Position := 0;

    m64.Position := 0;
    ParallelCompressMemory(TSelectCompressionMethod.scmZLIB_Max, m64, C64);

    C64.SaveToFile(p^);

    DoStatus('save as Compressed %s (source:%s compressed:%s)', [p^, umlSizeToStr(m64.Size).Text, umlSizeToStr(C64.Size).Text]);
  finally
    disposeObject([m64, C64]);
    dispose(p);
  end;
  MainThreadProgress.PostM1(Enabled_All);
end;

procedure TFilePackageWithZDBMainForm.SaveTo_OXP_File(fileName: SystemString);
var
  p: PString;
begin
  Disable_All;
  new(p);
  p^ := fileName;
  TCompute.RunM(p, nil, SaveTo_OXP_File_Th, TRun_Thread_M(nil));
end;

procedure TFilePackageWithZDBMainForm.SaveTo_ZDB2_File_Th(thSender: TCompute);
var
  p: PString;
  stream: TCore_Stream;
begin
  p := PString(thSender.UserData);

  stream := TCore_FileStream.Create(p^, fmCreate);
  try
    FDBEng.Save_To_ZDB2_Stream(stream);
    DoStatus('save "%s" (source:%s compressed:%s)', [p^, umlSizeToStr(FDBEng.Size).Text, umlSizeToStr(stream.Size).Text]);
  finally
    disposeObject(stream);
    dispose(p);
  end;
  MainThreadProgress.PostM1(Enabled_All);
end;

procedure TFilePackageWithZDBMainForm.SaveTo_ZDB2_File(fileName: SystemString);
var
  p: PString;
begin
  Disable_All;
  new(p);
  p^ := fileName;
  TCompute.RunM(p, nil, SaveTo_ZDB2_File_Th, TRun_Thread_M(nil));
end;

end.
