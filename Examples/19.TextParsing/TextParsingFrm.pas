unit TextParsingFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Z.PascalStrings, Z.Parsing, Z.Core, Z.UnicodeMixedLib, Z.Expression, Z.OpCode, Z.MemoryStream, Z.ListEngine,
  Z.Status,
  TypInfo,
  Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Memo1: TMemo;
    Button1: TButton;
    Memo2: TMemo;
    Button2: TButton;
    TabSheet2: TTabSheet;
    Memo3: TMemo;
    Button3: TButton;
    Memo4: TMemo;
    TabSheet3: TTabSheet;
    Memo5: TMemo;
    Panel1: TPanel;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.Button1Click(Sender: TObject);
var
  t: TTextParsing;
  i: Integer;
  pt: PTokenData;
begin
  Z.Parsing.SpacerSymbol.V:=umlDeleteChar(Z.Parsing.SpacerSymbol.V, '.');

  t := TTextParsing.Create(Memo1.Text, TTextStyle.tsPascal, nil);

  Memo2.Clear;

  for i := 0 to t.ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      pt := t.ParsingData.Cache.TokenDataList[i];
      if pt^.tokenType <> TTokenType.ttUnknow then
          Memo2.Lines.Add(Format('���� %d ����:%s ֵ %s', [i, GetEnumName(TypeInfo(TTokenType), Ord(pt^.tokenType)), pt^.Text.Text]));
    end;

  DisposeObject(t);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  t: TTextParsing;
  i: Integer;
  pt: PTokenData;
begin
  t := TTextParsing.Create(Memo1.Text, TTextStyle.tsC, nil);

  Memo2.Clear;

  for i := 0 to t.ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      pt := t.ParsingData.Cache.TokenDataList[i];
      if pt^.tokenType <> TTokenType.ttUnknow then
          Memo2.Lines.Add(Format('���� %d ����:%s ֵ %s', [i, GetEnumName(TypeInfo(TTokenType), Ord(pt^.tokenType)), pt^.Text.Text]));
    end;

  DisposeObject(t);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  t: TTextParsing;
  i: Integer;
  pt: PTokenData;

  PrepareProc: Boolean;
begin
  t := TTextParsing.Create(Memo3.Text, TTextStyle.tsPascal, nil);

  Memo2.Clear;
  PrepareProc := False;

  for i := 0 to t.ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      pt := t.ParsingData.Cache.TokenDataList[i];

      if PrepareProc then
        begin
          if (pt^.tokenType = TTokenType.ttSymbol) then
              PrepareProc := False
          else if (pt^.tokenType = TTokenType.ttAscii) then
              Memo4.Lines.Add(Format('���� %d ����:%s ֵ %s', [i, GetEnumName(TypeInfo(TTokenType), Ord(pt^.tokenType)), pt^.Text.Text]));
        end
      else
          PrepareProc := (pt^.tokenType = TTokenType.ttAscii) and (pt^.Text.Same('function') or pt^.Text.Same('procedure'));
    end;

  DisposeObject(t);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  rt: TOpCustomRunTime;
  v: Variant;
begin
  Memo5.Lines.Add('����ʹ��demo');
  // rtΪze�����к���֧�ֿ�
  rt := TOpCustomRunTime.Create;
  rt.RegOpP('myAddFunction', function(var Param: TOpParam): Variant
    // (a+b)*0.5
    begin
      Result := (Param[0] + Param[1]) * 0.5;
    end);
  rt.RegOpP('myStringFunction', function(var Param: TOpParam): Variant
    begin
      Result := Format('�ַ�������Ϊ:%d', [Length(VarToStr(Param[0]) + VarToStr(Param[1]))]);
    end);

  // ����ѧ���ʽ
  v := EvaluateExpressionValue(False, '1000+{ �����Ǳ�ע ze����ʶ��pascal��c�ı�ע�Լ��ַ���д�� } myAddFunction(1+1/2*3/3.14*9999, 599+2+2*100 shl 3)', rt);
  Memo5.Lines.Add(VarToStr(v));

  // ���ַ������ʽ��ze��Ĭ���ı������ʽΪPascal
  v := EvaluateExpressionValue(False, 'myStringFunction('#39'abc'#39', '#39'123'#39')', rt);
  Memo5.Lines.Add(VarToStr(v));

  // ���ַ������ʽ������ʹ��c���ı���ʽ��c��֧�ֵ�˫���ţ����ǲ�֧��#�ַ����ʽ
  v := EvaluateExpressionValue(tsC, 'myStringFunction("abc", "123")', rt);
  Memo5.Lines.Add(VarToStr(v));
  v := EvaluateExpressionValue(tsC, 'myStringFunction('#39'abc'#39', '#39'123'#39')', rt);
  Memo5.Lines.Add(VarToStr(v));

  DisposeObject(rt);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  tmpSym: TSymbolExpression;
  op: TOpCode;
  rt: TOpCustomRunTime;
  m64: TMemoryStream64;
begin
  Memo5.Lines.Add('����������ִ��demo');
  // rtΪze�����к���֧�ֿ�
  rt := TOpCustomRunTime.Create;
  rt.RegOpP('myAddFunction', function(var Param: TOpParam): Variant
    // (a+b)*0.5
    begin
      Result := (Param[0] + Param[1]) * 0.5;
    end);
  rt.RegOpP('myStringFunction', function(var Param: TOpParam): Variant
    begin
      Result := Format('�ַ�������Ϊ:%d', [Length(VarToStr(Param[0]) + VarToStr(Param[1]))]);
    end);

  // ʹ��ParseTextExpressionAsSymbol�����������ʽ����ɴʷ���
  tmpSym := ParseTextExpressionAsSymbol_M(nil, TTextParsing, tsPascal, '', '1000+myAddFunction(1+1/2*3/3.14*9999, 599+2+2*100 shl 3)', nil, rt);
  // BuildAsOpCode�Ὣ�ʷ����ٴη�����﷨����Ȼ���ٻ����﷨������op����
  op := BuildAsOpCode(tmpSym);
  DisposeObject(tmpSym);
  // ����ִ��һ��op
  Memo5.Lines.Add(Format('op���з���ֵ(��ȷֵΪ4489.2962): %s', [VarToStr(op.OpCode_Execute(rt))]));

  m64 := TMemoryStream64.Create;
  op.SaveToStream(m64);

  // �����Ѿ��ͷ���op
  DisposeObject(op);

  // ��stream���ٶ�ȡop�������������
  m64.Position := 0;
  if LoadOpFromStream(m64, op) then
    begin
      Memo5.Lines.Add(Format('op���з���ֵ(��ȷֵΪ4489.2962): %s', [VarToStr(op.OpCode_Execute(rt))]));
    end;

  DisposeObject([op, rt, m64]);

  Memo5.Lines.Add('����������ִ��demo���������');
end;

procedure TForm1.Button6Click(Sender: TObject);
type
  TState = (sUnknow, sIF, sTrue, sFalse); // �����õļ�״̬��
label gFillStruct;
var
  t: TTextParsing;                                  // �ʷ���������
  cp, ep: Integer;                                  // ������
  wasNumber, wasText, wasAscii, wasSymbol: Boolean; // �����ı�״̬��
  state: TState;                                    // �����ṹ״̬��
  decl: TPascalString;                              // ��ǰ�����ʷ��壬����
  ifMatchBody: TPascalString;                       // ���������ж�������
  ifTrueBody: TPascalString;                        // ��������������
  ifFalseBody: TPascalString;                       // ����������������
  rt: TOpCustomRunTime;                             // ���к�����֧��
begin
  // ����pascal���ַ�����������д�ڳ����У���������c����ַ���
  t := TTextParsing.Create('if 1+1=/* comment */2 then writeln/* comment */("if was true") else writeln/* comment */("if was false");', tsC, nil);
  cp := 1;
  ep := 1;
  state := sUnknow;
  ifMatchBody := '';
  ifTrueBody := '';
  ifFalseBody := '';

  // ������ѭ��
  while cp < t.Len do
    begin
      // ����Ǵ��뱸ע������ȥ
      if t.IsComment(cp) then
        begin
          ep := t.GetCommentEndPos(cp);
          cp := ep;
          continue;
        end;

      // �ʷ����̷�ʽ�����״˷�ʽ���Գ���ʷ�����Ϊ����û�п������ܣ������Ҫ�������нű����뿼�Ǳ�������ݽṹ�洢���Ը��ٷ�ʽ��������
      wasNumber := t.IsNumber(cp);
      wasText := t.IsTextDecl(cp);
      wasAscii := t.IsAscii(cp);
      wasSymbol := t.IsSymbol(cp);

      if wasNumber then
        begin
          ep := t.GetNumberEndPos(cp);
          decl := t.GetStr(cp, ep);
          cp := ep;
          goto gFillStruct;
        end;

      if wasText then
        begin
          ep := t.GetTextDeclEndPos(cp);
          decl := t.GetStr(cp, ep);
          cp := ep;
          goto gFillStruct;
        end;

      if wasAscii then
        begin
          ep := t.GetAsciiEndPos(cp);
          decl := t.GetStr(cp, ep);
          cp := ep;
          goto gFillStruct;
        end;

      if wasSymbol then
        begin
          decl := t.ParsingData.Text[cp];
          inc(cp);
          ep := cp;
          goto gFillStruct;
        end;

      inc(cp);
      continue;
      // �ʷ����̷�ʽ�����������������ṹ���ж�

    gFillStruct:

      if wasAscii then
        begin
          // �ʷ��ṹ
          if decl.Same('if') then
            begin
              if state <> sUnknow then
                begin
                  Memo5.Lines.Add('if ��ʽ��������');
                  break;
                end;
              state := sIF;
              continue;
            end;

          if decl.Same('then') then
            begin
              if state <> sIF then
                begin
                  Memo5.Lines.Add('then ��ʽ��������');
                  break;
                end;
              state := sTrue;
              continue;
            end;

          if decl.Same('else') then
            begin
              if state <> sTrue then
                begin
                  Memo5.Lines.Add('else ��д��ʽ��������');
                  break;
                end;
              state := sFalse;
              continue;
            end;
        end;

      case state of
        sIF: ifMatchBody.Append(decl);    // ��TPascalString�У�ʹ��Append������Ҫ��string:=string+stringЧ�ʸ���
        sTrue: ifTrueBody.Append(decl);   // ��TPascalString�У�ʹ��Append������Ҫ��string:=string+stringЧ�ʸ���
        sFalse: ifFalseBody.Append(decl); // ��TPascalString�У�ʹ��Append������Ҫ��string:=string+stringЧ�ʸ���
      end;
    end;

  // ����һ��������if�ṹ����Ѿ������ɹ��ˣ�����ֱ�����г��򼴿�
  if state = sFalse then
    begin
      rt := TOpCustomRunTime.Create;
      rt.RegOpP('writeln', function(var Param: TOpParam): Variant
        begin
          Memo5.Lines.Add(VarToStr(Param[0]));
          Result := 0;
        end);
      // �����Ҫ���ܣ�����Ľṹ������Կ��������ݽṹ���洢��ʵ�ֿ��ٽű�
      // opCache.Clear;
      if EvaluateExpressionValue(tsC, ifMatchBody, rt) = True then
          EvaluateExpressionValue(tsC, ifTrueBody, rt)
      else
          EvaluateExpressionValue(tsC, ifFalseBody, rt);
      DisposeObject(rt);
    end;

  DisposeObject(t);
end;

procedure TForm1.Button7Click(Sender: TObject);

  function Macro(var AText: string; const HeadToken, TailToken: string; const rt: TOpCustomRunTime): TPascalString; inline;
  var
    sour: TPascalString;
    ht, tt: TPascalString;
    bPos, ePos: Integer;
    KeyText: SystemString;
    i: Integer;
    tmpSym: TSymbolExpression;
    op: TOpCode;
  begin
    Result := '';
    sour.Text := AText;
    ht.Text := HeadToken;
    tt.Text := TailToken;

    i := 1;

    while i <= sour.Len do
      begin
        if sour.ComparePos(i, @ht) then
          begin
            bPos := i;
            ePos := sour.GetPos(@tt, i + ht.Len);
            if ePos > 0 then
              begin
                KeyText := sour.copy(bPos + ht.Len, ePos - (bPos + ht.Len)).Text;

                // ��TPascalString�У�ʹ��Append������Ҫ��string:=string+stringЧ�ʸ���
                Result.Append(VarToStr(EvaluateExpressionValue(KeyText, rt)));
                i := ePos + tt.Len;
                continue;
              end;
          end;

        // ��TPascalString�У�ʹ��Append������Ҫ��string:=string+stringЧ�ʸ���
        Result.Append(sour[i]);
        inc(i);
      end;
  end;

var
  n: string;
  i: Integer;
  t: TTimeTick;
  rt: TOpCustomRunTime;
begin
  Memo5.Lines.Add('����ʾ�ýű�����װzExpression');
  // rtΪze�����к���֧�ֿ�
  rt := TOpCustomRunTime.Create;
  rt.RegOpP('OverFunction', function(var Param: TOpParam): Variant
    begin
      Result := 'лл';
    end);

  // ��������ʹ�ú괦��1+1�Ա��ʽ������
  n := '����1+1=<begin>1+1<end>������һ��UInt48λ����:<begin>1<<48<end>������ <begin>OverFunction<end>';

  Memo5.Lines.Add('ԭ��:' + n);
  Memo5.Lines.Add('������' + Macro(n, '<begin>', '<end>', rt).Text);

  Memo5.Lines.Add('zExpression���ڲ������ܣ�������ԭ����10��δ���');

  t := GetTimeTick;

  // �ظ���1��ξ䷨���ʽ�����ʹ���
  for i := 1 to 10 * 10000 do
      Macro(n, '<begin>', '<end>', rt);

  Memo5.Lines.Add(Format('zExpression���ܲ�����ɣ���ʱ:%dms', [GetTimeTick - t]));

  DisposeObject([rt]);
end;

procedure TForm1.Button8Click(Sender: TObject);
// �߼�Demo��ʵ���ڲ������ĸ�ֵ
// �����Ҵ���һ���ű�����γ����ķ����������е�࣬����ԭ��ֻ������
var
  sourTp, t: TTextParsing;            // �ʷ���������
  setBefore, setAfter: TPascalString; // ��ֵ��ǰ���������͸�ֵ�ĺ�������
  splitVarDecl: TArrayPascalString;   // �п��ı��ʽ��
  myvars: TArrayPascalString;         // ������Ҫ��ֵ����ʱ�������Զ��ŷָ�
  WasAssignment: Boolean;             // �ڱ��ʽ���ҵ��˸�ֵ
  HashVars: THashVariantList;         // ������hash�洢�ṹ�����ǿ��Դ�ŵ�Ӳ���е�
  rt: TOpCustomRunTime;               // ���к�����֧��
  op: TOpCode;                        // ����������cache��op����
  i: Integer;                         // forʹ��
  dynvar: Integer;                    // ��̬����
begin
  // ������c��pascal����д���������޸ı�ע����
  sourTp := TTextParsing.Create('myvar1/*�����Ǳ�ע*/,myvar2,myvar3 = 123+456+" ����: "+dynamic', tsC, nil); // �ʷ��������棬��c�﷨Ϊ��
  // sourTp := TTextParsing.Create('myvar1(*�����Ǳ�ע*),myvar2,myvar3 := 123+456+'#39' ����: '#39'+dynamic', tsPascal); // �ʷ��������棬��c�﷨Ϊ��
  // sourTp := TTextParsing.Create('123+456+dynamic', tsPascal); // �ʷ��������棬��c�﷨Ϊ��

  HashVars := THashVariantList.CustomCreate(16); // 16��hash��buff���ȣ���ֵԽ����ٶ�Խ��

  SetLength(splitVarDecl, 0);
  SetLength(myvars, 0);

  // ��һ����������ֵ����
  case sourTp.TextStyle of
    tsPascal:
      begin
        // pascal�ĸ�ֵ����Ϊ :=
        WasAssignment := sourTp.SplitString(1, ':=', ';', splitVarDecl) = 2; // ���ַ�����Ϊ�и�Ǻţ��Դ���:=�Ǻŵ��ַ��������и�
        if WasAssignment then
          begin
            setBefore := splitVarDecl[0];
            setAfter := splitVarDecl[1];

            t := TTextParsing.Create(setBefore, tsPascal, nil);
            t.DeletedComment;
            if t.SplitChar(1, ',', ';', myvars) = 0 then // ���ﲻ���ַ����������ַ���Ϊ�и�Ǻţ��Դ���,���ַ������и�
                Memo5.Lines.Add(Format('������ֵ�﷨���� %s', [setBefore.Text]));
            DisposeObject(t);
          end;
      end;
    tsC:
      begin
        // c�ĸ�ֵ����Ϊ =
        WasAssignment := sourTp.SplitChar(1, '=', ';', splitVarDecl) = 2; // ���ﲻ���ַ����������ַ���Ϊ�и�Ǻţ��Դ���=���ַ������и�
        if WasAssignment then
          begin
            setBefore := splitVarDecl[0];
            setAfter := splitVarDecl[1];

            t := TTextParsing.Create(setBefore, tsC, nil);
            t.DeletedComment;
            if t.SplitChar(1, ',', ';', myvars) = 0 then // ���ﲻ���ַ����������ַ���Ϊ�и�Ǻţ��Դ���,���ַ������и�
                Memo5.Lines.Add(Format('������ֵ�﷨���� %s', [setBefore.Text]));
            DisposeObject(t);
          end;
      end;
    else
      begin
        Memo5.Lines.Add('��֧�ֱ��ʽ');
        WasAssignment := False;
      end;
  end;

  rt := TOpCustomRunTime.Create;
  rt.RegOpP('dynamic', function(var Param: TOpParam): Variant
    begin
      Result := dynvar;
      inc(dynvar);
    end);
  rt.RegOpP('myvar1', function(var Param: TOpParam): Variant
    begin
      // ��myvar1���ж�̬����
      Result := HashVars['myvar1'];
    end);

  dynvar := 1;

  // �ڶ���������ҵ��˸�ֵ����
  if WasAssignment then
    begin
      Memo5.Lines.Add('�����˱�����ֵ���ʽ');

      op := BuildAsOpCode(sourTp.TextStyle, setAfter, rt);

      for i := low(myvars) to high(myvars) do
          HashVars[myvars[i].TrimChar(#32).Text] := op.OpCode_Execute(rt); // ��һ����β�ո�ü���ִ��op�����������ĸ�ֵ

      Memo5.Lines.Add('������ֵ����');
      Memo5.Lines.Add(HashVars.AsText);

      // ���������ñ����ڱ��ʽ�еĸ���
      Memo5.Lines.Add('���ڣ����ǿ�ʼ��̬�������Ǹղ������ı�������̬�����ǽ�������const��ʽ���б���');

      // ����opCache�������Զ������еģ��������κ�ʱ����const���ñ���ʱ��Ҫ�����
      CleanOpCache;

      Memo5.Lines.Add(VarToStr(EvaluateExpressionValue_P(False, nil, TTextParsing, tsC, '"��̬���� "+myvar1',
        procedure(const DeclName: SystemString; var ValType: TExpressionDeclType; var Value: Variant)
        begin
          if HashVars.Exists(DeclName) then
            begin
              Value := HashVars[DeclName];
              ValType := TExpressionDeclType.edtString; // ������Ҫ���߱��������ñ���������
            end;
        end)));

      Memo5.Lines.Add(VarToStr(EvaluateExpressionValue_P(False, nil, TTextParsing, tsC, '"��̬���� "+myvar4',
        procedure(const DeclName: SystemString; var ValType: TExpressionDeclType; var Value: Variant)
        begin
          // myvar4�ǲ����ڵ�
          // Ȼ�� ������myvar2������
          Value := HashVars['myvar2'];
          ValType := TExpressionDeclType.edtString; // ������Ҫ���߱��������ñ���������
        end)));

      Memo5.Lines.Add('���ڣ����ǿ�ʼ��̬�������Ǹղ������ı���');
      Memo5.Lines.Add(VarToStr(EvaluateExpressionValue(tsC, '"��̬���� "+myvar1', rt)));

      HashVars['myvar1'] := 'abc';
      Memo5.Lines.Add(VarToStr(EvaluateExpressionValue(tsC, '"��̬���� "+myvar1', rt)));
    end
  else
    begin
      Memo5.Lines.Add('û�з����˱�����ֵ');
      Memo5.Lines.Add(Format('���ʽ "%s"' + #13#10 + '���н�� %s',
        [sourTp.ParsingData.Text.Text, VarToStr(EvaluateExpressionValue(sourTp.TextStyle, sourTp.ParsingData.Text, rt))]));
    end;

  DisposeObject([sourTp, HashVars, rt]);
end;

procedure TForm1.Button9Click(Sender: TObject);
// ������ź���
var
  SpecialAsciiToken: TPascalStringList;
  rt: TOpCustomRunTime;
  v: Variant;
begin
  Memo5.Lines.Add('ȫ�ֵĴʷ�̽ͷǰ׺������ʹ��');

  // ����ǰ׺��@@����,����Ϊascii������
  SpecialAsciiToken := TPascalStringList.Create;
  SpecialAsciiToken.Add('@@');
  SpecialAsciiToken.Add('&&');

  // rtΪze�����к���֧�ֿ�
  rt := TOpCustomRunTime.Create;
  rt.RegOpP('@@a&&', function(var Param: TOpParam): Variant
    // (a+b)*0.5
    begin
      Result := (Param[0] + Param[1]) * 0.5;
    end);
  rt.RegOpP('@@combineString&&', function(var Param: TOpParam): Variant
    // (a+b)*0.5
    begin
      Result := VarToStr(Param[0]) + VarToStr(Param[1]);
    end);

  // ����@@ǰ׺��asciiҲ�����ں�׺����������ţ�������ų��Ȳ�����
  v := EvaluateExpressionValue(SpecialAsciiToken, False, '{ ��ע } @@a&&(1,2)', rt);
  Memo5.Lines.Add(VarToStr(v));

  // ���ַ������ʽ��ze��Ĭ���ı������ʽΪPascal
  v := EvaluateExpressionValue(SpecialAsciiToken, False, '@@combineString&&('#39'abc'#39', '#39'123'#39')', rt);
  Memo5.Lines.Add(VarToStr(v));

  // ���ַ������ʽ������ʹ��c���ı���ʽ
  v := EvaluateExpressionValue(SpecialAsciiToken, tsC, '@@combineString&&("abc", "123")', rt);
  Memo5.Lines.Add(VarToStr(v));
  v := EvaluateExpressionValue(SpecialAsciiToken, tsC, '@@combineString&&('#39'abc'#39', '#39'123'#39')', rt);
  Memo5.Lines.Add(VarToStr(v));

  DisposeObject(rt);

  DisposeObject(SpecialAsciiToken);
end;

end.
