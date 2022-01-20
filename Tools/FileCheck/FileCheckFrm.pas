unit FileCheckFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl, Vcl.ComCtrls,

  System.IOUtils,

  Z.Core,
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status,
  Z.TextDataEngine, Z.ListEngine,
  Z.Geometry2D, Z.Geometry3D, Z.Cipher,
  Z.MemoryStream;

type
  TFileCheckForm = class(TForm)
    Label2: TLabel;
    FilesMemo: TMemo;
    AddFilesButton: TButton;
    FileOpenDialog: TFileOpenDialog;
    LogMemo: TMemo;
    MD5CheckButton: TButton;
    ThreadStateLabel: TLabel;
    stateTimer: TTimer;
    procedure AddFilesButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MD5CheckButtonClick(Sender: TObject);
    procedure SHA1ButtonClick(Sender: TObject);
    procedure stateTimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FileCheckForm: TFileCheckForm;

implementation

{$R *.dfm}


procedure TFileCheckForm.AddFilesButtonClick(Sender: TObject);
begin
  if not FileOpenDialog.Execute then
      exit;
  FilesMemo.Lines.AddStrings(FileOpenDialog.Files);
end;

procedure TFileCheckForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := TCompute.TotalTask = 0;
end;

procedure TFileCheckForm.MD5CheckButtonClick(Sender: TObject);
begin
  TCompute.RunP_NP(procedure
    begin
      DelphiParallelFor(0, FilesMemo.Lines.Count - 1, procedure(pass: Integer)
        var
          f: U_String;
          md5: TMD5;
        begin
          TCompute.Sync(procedure
            begin
              f := FilesMemo.Lines[pass];
            end);
          md5 := umlFileMD5(f);
          TCompute.Sync(procedure
            begin
              LogMemo.Lines.Add(Format('%s=%s', [umlGetFileName(f).Text, umlMD5ToStr(md5).Text]));
            end);
        end);
    end);
end;

procedure TFileCheckForm.SHA1ButtonClick(Sender: TObject);
begin
  TCompute.RunP_NP(procedure
    begin
      DelphiParallelFor(0, FilesMemo.Lines.Count - 1, procedure(pass: Integer)
        var
          f: U_String;
          md5: TMD5;
        begin
          TCompute.Sync(procedure
            begin
              f := FilesMemo.Lines[pass];
            end);
          md5 := umlFileMD5(f);
          TCompute.Sync(procedure
            begin
              LogMemo.Lines.Add(Format('%s=%s', [umlMD5ToStr(md5).Text, umlGetFileName(f).Text]));
            end);
        end);
    end);
end;

procedure TFileCheckForm.stateTimerTimer(Sender: TObject);
begin
  ThreadStateLabel.Caption := 'Thread Status: ' + TCompute.State;
end;

end.
