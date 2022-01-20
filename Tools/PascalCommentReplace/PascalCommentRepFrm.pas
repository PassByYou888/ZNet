unit PascalCommentRepFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Parsing, Z.UnicodeMixedLib;

type
  TPascalCommentRepForm = class(TForm)
    Memo: TMemo;
    Rep1Button: TButton;
    removeEmptyButton: TButton;
    Rep2Button: TButton;
    procedure Rep1ButtonClick(Sender: TObject);
    procedure removeEmptyButtonClick(Sender: TObject);
    procedure Rep2ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PascalCommentRepForm: TPascalCommentRepForm;

implementation

{$R *.dfm}


procedure TPascalCommentRepForm.Rep1ButtonClick(Sender: TObject);
var
  tp: TTextParsing;
  i: Integer;
  p: PTokenData;
begin
  tp := TTextParsing.Create(Memo.Text, tsPascal);
  for i := 0 to tp.TokenCount - 1 do
    begin
      p := tp.Token[i];
      if (p^.tokenType = ttComment) and (umlMultipleMatch('//*', p^.Text)) then
        begin
          p^.Text.DeleteFirst;
          p^.Text.DeleteFirst;
          p^.Text := '{ ' + umlTrimSpace(p^.Text) + ' }';
        end;
    end;

  tp.RebuildToken;
  Memo.Text := tp.Text;

  disposeObject(tp);
end;

procedure TPascalCommentRepForm.removeEmptyButtonClick(Sender: TObject);
var
  tp: TTextParsing;
  i: Integer;
  p: PTokenData;
begin
  tp := TTextParsing.Create(Memo.Text, tsPascal);
  for i := 0 to tp.TokenCount - 1 do
    begin
      p := tp.Token[i];
      if (p^.tokenType = ttComment) then
        begin
          if p^.Text.DeleteChar('//(*){} '#13#10).L = 0 then
              p^.Text := '';
        end;
    end;

  tp.RebuildToken;
  Memo.Text := tp.Text;

  disposeObject(tp);
end;

procedure TPascalCommentRepForm.Rep2ButtonClick(Sender: TObject);
var
  tp: TTextParsing;
  i: Integer;
  p: PTokenData;
begin
  tp := TTextParsing.Create(Memo.Text, tsPascal);
  for i := 0 to tp.TokenCount - 1 do
    begin
      p := tp.Token[i];
      if (p^.tokenType = ttComment) and (umlMultipleMatch('//*', p^.Text)) then
        begin
          p^.Text.DeleteFirst;
          p^.Text.DeleteFirst;
          p^.Text := '(* ' + umlTrimSpace(p^.Text) + ' *)';
        end;
    end;

  tp.RebuildToken;
  Memo.Text := tp.Text;

  disposeObject(tp);
end;

end.
 
