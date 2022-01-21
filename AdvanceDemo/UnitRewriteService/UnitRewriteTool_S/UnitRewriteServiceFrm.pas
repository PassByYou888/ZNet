unit UnitRewriteServiceFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  System.IOUtils,

  Z.Core,
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib,
  Z.Net, Z.Net.C4, Z.DFE, Z.Status, Z.MemoryStream,
  Z.ZDB2;

type
  TUnitRewriteServiceForm = class(TForm)
    fpsTimer: TTimer;
    Memo: TMemo;
    procedure fpsTimerTimer(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  UnitRewriteServiceForm: TUnitRewriteServiceForm;

implementation

{$R *.dfm}

uses Z.Net.C4_PascalRewrite_Service, Z.Net.C4_PascalRewrite_Client;


procedure TUnitRewriteServiceForm.fpsTimerTimer(Sender: TObject);
begin
  CheckThread;
end;

procedure TUnitRewriteServiceForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  if Memo.Lines.Count > 5000 then
      Memo.Lines.Clear;
  Memo.Lines.Add(Text_);
end;

constructor TUnitRewriteServiceForm.Create(AOwner: TComponent);
begin
  inherited;
  AddDoStatusHook(self, DoStatus_backcall);
end;

destructor TUnitRewriteServiceForm.Destroy;
begin
  RemoveDoStatusHook(self);
  inherited;
end;

end.
