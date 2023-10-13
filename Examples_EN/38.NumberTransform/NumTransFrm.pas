unit NumTransFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  System.Math,

  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Parsing, Z.UnicodeMixedLib;

type
  TNumTransForm = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    bin2decButton: TButton;
    dec2binButton: TButton;
    procedure bin2decButtonClick(Sender: TObject);
    procedure dec2binButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NumTransForm: TNumTransForm;

implementation

{$R *.dfm}


function BinToInt(Value: U_String): UInt64;
var
  i, Size: Integer;
begin
  Result := 0;
  Size := length(Value);
  for i := Size downto 1 do
    begin
      if Value[i] = '1' then
          Result := Result + (1 shl (Size - i));
    end;
end;

function IntToBin(v: UInt64): U_String;
begin
  if v = 0 then
    begin
      Result := '0';
      exit;
    end;
  Result := '';
  while v > 0 do
    begin
      if v and $1 = 1 then
          Result := '1' + Result
      else
          Result := '0' + Result;
      v := v shr 1;
    end;
  while Result.First = '0' do
      Result.DeleteFirst;
end;

procedure TNumTransForm.bin2decButtonClick(Sender: TObject);
var
  dest: TPascalString;
  procedure Append(s: SystemString);
  begin
    dest.Append(s);
  end;

var
  T: TTextParsing;
  i: Integer;
  p: PTokenData;
  Prev: PTokenData;
  n: TPascalString;
begin
  T := TTextParsing.Create(Memo1.Text, TTextStyle.tsPascal);

  dest := '';
  Prev := nil;
  for i := 0 to T.TokenCount - 1 do
    begin
      p := T.Tokens[i];
      case p^.tokenType of
        ttTextDecl:
          begin
            n := TTextParsing.Translate_Pascal_Decl_To_Text(p^.Text);
            Append(TTextParsing.Translate_Text_To_Pascal_Decl(n));
          end;
        ttComment:
          begin
            n := TTextParsing.Translate_Pascal_Decl_Comment_To_Text(p^.Text);
            Append(TTextParsing.Translate_Text_To_Pascal_Decl_Comment(umlTrimSpace(n)));
          end;
        ttNumber:
          begin
            if (Prev <> nil) and (Prev^.tokenType = ttSymbol) and (Prev^.Text = '%') then
              begin
                while (dest.Len > 0) and (dest.Last <> '%') do
                    dest.DeleteLast;
                if (dest.Len > 0) and (dest.Last = '%') then
                    dest.DeleteLast;

                Append(IntToStr(BinToInt(p^.Text)));
              end
            else if (Prev <> nil) and (Prev^.tokenType = ttSymbol) and (Prev^.Text = '$') then
              begin
                while (dest.Len > 0) and (dest.Last <> '$') do
                    dest.DeleteLast;
                if (dest.Len > 0) and (dest.Last = '$') then
                    dest.DeleteLast;

                Append(IntToStr(StrToInt('$' + p^.Text)));
              end
            else
              begin
                if p^.Text.First = '$' then
                    Append(IntToStr(StrToInt64(p^.Text)))
                else
                    Append(p^.Text);
              end;
          end;
        ttSymbol: Append(p^.Text);
        ttAscii: Append(p^.Text);
        ttSpecialSymbol: Append(p^.Text);
        else Append(p^.Text);
      end;

      if p^.tokenType <> ttUnknow then
          Prev := p;
    end;
  Memo2.Text := dest;
end;

procedure TNumTransForm.dec2binButtonClick(Sender: TObject);
var
  dest: TPascalString;
  procedure Append(s: SystemString);
  begin
    dest.Append(s);
  end;

var
  T: TTextParsing;
  i: Integer;
  p: PTokenData;
  Prev: PTokenData;
  n: TPascalString;
begin
  T := TTextParsing.Create(Memo2.Text, TTextStyle.tsPascal);

  dest := '';
  Prev := nil;
  for i := 0 to T.TokenCount - 1 do
    begin
      p := T.Tokens[i];
      case p^.tokenType of
        ttTextDecl:
          begin
            n := TTextParsing.Translate_Pascal_Decl_To_Text(p^.Text);
            Append(TTextParsing.Translate_Text_To_Pascal_Decl(n));
          end;
        ttComment:
          begin
            n := TTextParsing.Translate_Pascal_Decl_Comment_To_Text(p^.Text);
            Append(TTextParsing.Translate_Text_To_Pascal_Decl_Comment(umlTrimSpace(n)));
          end;
        ttNumber:
          begin
            if not umlIsFloatNumber(Prev^.Text) then
              begin
                Append('%' + IntToBin(umlStrToInt64(p^.Text, 0)));
              end
            else
              begin
                Append(p^.Text);
              end;
          end;
        ttSymbol: Append(p^.Text);
        ttAscii: Append(p^.Text);
        ttSpecialSymbol: Append(p^.Text);
        else Append(p^.Text);
      end;

      if p^.tokenType <> ttUnknow then
          Prev := p;
    end;
  Memo1.Text := dest;
end;

end.

