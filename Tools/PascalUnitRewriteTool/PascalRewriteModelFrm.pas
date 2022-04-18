unit PascalRewriteModelFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.StdCtrls, System.Actions, Vcl.ActnList, Vcl.Menus,

  Vcl.Clipbrd,

  Vcl.FileCtrl,
  System.IOUtils, System.DateUtils,

  Z.Core, Z.Status, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Parsing, Z.MemoryStream,
  Z.ListEngine, Z.Pascal_Code_Tool, Z.Json,
  Z.MediaCenter,
  Z.ZDB2, Z.ZDB2.FileEncoder, Z.Cipher, Vcl.Mask;

type
  TPascalRewriteModelForm = class(TForm)
    PageC: TPageControl;
    TS_define: TTabSheet;
    Panel_Redefine: TPanel;
    Panel_defRight: TPanel;
    Panel_ListTopToolBar: TPanel;
    Memo: TMemo;
    memoSplitter: TSplitter;
    TS_Model: TTabSheet;
    newDefList_Splitter: TSplitter;
    UnitRenamePanel: TPanel;
    AddUnitSourceButton: TButton;
    OpenUnitDialog: TFileOpenDialog;
    PanelUnitDefList: TPanel;
    unitDefList: TListView;
    UnitDefPopupMenu: TPopupMenu;
    NewUnitName1: TMenuItem;
    RemoveUnit1: TMenuItem;
    RefreshUnitlist1: TMenuItem;
    UnitFilterEdit: TLabeledEdit;
    AddUnitFromDirectoryButton: TButton;
    ApplyToolPanel: TPanel;
    fpsTimer: TTimer;
    TS_Test: TTabSheet;
    testMemo: TMemo;
    testButton: TButton;
    unit_L_Panel: TPanel;
    Unit_Processor_LB: TListBox;
    Marco_rep_L_Panel: TPanel;
    SymbolReplaceListBox: TListBox;
    Sym_LSplitter: TSplitter;
    u_model_t_Panel: TPanel;
    Button4: TButton;
    Button1: TButton;
    TS_SymbolRewriteDefine: TTabSheet;
    sym_model_t_Panel: TPanel;
    rebuild_sym_Button: TButton;
    swap_sym_Button: TButton;
    symbol_rewrite_cli_Panel: TPanel;
    sym_def_C_T_Panel: TPanel;
    paste_and_fill_fromtext_Button: TButton;
    SymbolJsonSourceMemo: TMemo;
    feature_r_Splitter: TSplitter;
    symbol_rewrite_L_Panel: TPanel;
    sym_feature_r_t_Panel: TPanel;
    symbol_rewrite_file_features_LB: TListBox;
    AddCodeFeatureFileButton: TButton;
    ScanFeatureButton: TButton;
    CodeFeatureEdit: TLabeledEdit;
    reset_symbol_rewrite_define_Button: TButton;
    Paste_Replace_Sour_Edit: TLabeledEdit;
    Paste_Replace_Dest_Edit: TLabeledEdit;
    update_sym_Button: TButton;
    addSymbolButton: TButton;
    Splitter3: TSplitter;
    DoCreateModel_Btn: TButton;
    ModelOuytputEdit: TLabeledEdit;
    Browse_ModelOuytput_Btn: TButton;
    SaveModelDialog: TSaveDialog;
    rep_unit_def_Button: TButton;
    rep_sym_old_Button: TButton;
    rep_sym_new_Button: TButton;
    check_model_Button: TButton;
    symbol_rewrite_L_B_C_Panel: TPanel;
    FeatureOutputMemo: TMemo;
    symbol_rewrite_L_B_C_H_Panel: TPanel;
    MainMenu_: TMainMenu;
    File1: TMenuItem;
    OpenWorkspace_MI: TMenuItem;
    Saveworkspace_MI: TMenuItem;
    Newworkspace_MI: TMenuItem;
    SaveworkspaceAsMI: TMenuItem;
    Closeworkspace_MI: TMenuItem;
    Exit1: TMenuItem;
    SaveWorkspaceDialog: TSaveDialog;
    OpenWorkspaceDialog: TOpenDialog;
    unitDefine_text_edt_Button: TButton;
    NewworkspacefromPassByYou888Model_MI: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    ScanFeatureFromClipboardButton: TButton;
    procedure fpsTimerTimer(Sender: TObject);
    procedure PageCChange(Sender: TObject);
    procedure NewworkspacefromPassByYou888Model_MIClick(Sender: TObject);
    procedure Newworkspace_MIClick(Sender: TObject);
    procedure OpenWorkspace_MIClick(Sender: TObject);
    procedure Saveworkspace_MIClick(Sender: TObject);
    procedure SaveworkspaceAsMIClick(Sender: TObject);
    procedure Closeworkspace_MIClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure AddUnitSourceButtonClick(Sender: TObject);
    procedure AddUnitFromDirectoryButtonClick(Sender: TObject);
    procedure UnitFilterEditChange(Sender: TObject);
    procedure Unit_Define_Click(Sender: TObject);
    procedure RefreshUnitList_Click(Sender: TObject);
    procedure RemoveUnit_Define_Click(Sender: TObject);
    procedure unitDefListColumnClick(Sender: TObject; Column: TListColumn);
    procedure unitDefListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Unit_Processor_LBKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure unitDefine_text_edt_ButtonClick(Sender: TObject);
    procedure SymbolJsonSourceMemoExit(Sender: TObject);
    procedure addSymbolButtonClick(Sender: TObject);
    procedure Fill_TextAsSymbolDefine_Click(Sender: TObject);
    procedure reset_symbol_rewrite_define_ButtonClick(Sender: TObject);
    procedure symbol_rewrite_file_features_LBKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AddCodeFeatureFileButtonClick(Sender: TObject);
    procedure ScanFeatureButtonClick(Sender: TObject);
    procedure ScanFeatureFromClipboardButtonClick(Sender: TObject);
    procedure DoCreateModel_BtnClick(Sender: TObject);
    procedure Browse_ModelOuytput_BtnClick(Sender: TObject);
    procedure check_model_ButtonClick(Sender: TObject);
    procedure rep_unit_def_ButtonClick(Sender: TObject);
    procedure rep_sym_old_ButtonClick(Sender: TObject);
    procedure rep_sym_new_ButtonClick(Sender: TObject);
    procedure Rebuild_PreDefine_Click(Sender: TObject);
    procedure Swap_Predefine_Click(Sender: TObject);
    procedure Rebuild_Symbol_Click(Sender: TObject);
    procedure Swap_Symbol_Click(Sender: TObject);
    procedure testButtonClick(Sender: TObject);
  private
    UnitDefineList: TSourceDefinePool;
    UnitProcessorList: TSourceProcessorDataPool;
    SymbolProcessorList: TSourceProcessorDataPool;
    CurrentProj: U_String;
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);

    procedure NewProj;
    procedure NewProj_Stream(stream_: TCore_Stream);
    procedure OpenProj_File(fn: U_String);
    procedure SaveProj(fn: U_String);

    procedure DoAddUnitFromDirectory(dir: U_String);
    procedure UpdateUnitDefineListView;
    procedure UpdateUnitProcessorList(rebuild_: Boolean);
    procedure UpdateSymbolReplaceList(rebuild_: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

function CompInt(const A, b: Int64): Integer;
function CompText(const t1, t2: TPascalString): Integer;
function LV_Sort1(lParam1, lParam2, lParamSort: LParam): Integer; stdcall;
function LV_Sort2(lParam2, lParam1, lParamSort: LParam): Integer; stdcall;

var
  PascalRewriteModelForm: TPascalRewriteModelForm;

implementation

{$R *.dfm}


uses NewUnitNameFrm, NewSymbolDefFrm, TextRepToolFrm,
  PascalRewrite_TextEdtFrm;

function CompInt(const A, b: Int64): Integer;
begin
  if A = b then
      Result := 0
  else if A < b then
      Result := -1
  else
      Result := 1;
end;

function CompText(const t1, t2: TPascalString): Integer;
  function IsWide(p: PPascalString): Byte;
  var
    C: SystemChar;
  begin
    for C in p^.buff do
      if Ord(C) > 127 then
          Exit(1);
    Result := 0;
  end;

var
  d: Double;
  Same, Diff: Integer;
begin
  Result := CompInt(IsWide(@t1), IsWide(@t2));
  if Result = 0 then
    begin
      Result := CompInt(length(t1), length(t2));
      if Result = 0 then
          Result := CompareText(t1, t2);
    end;
end;

function LV_Sort1(lParam1, lParam2, lParamSort: LParam): Integer;
var
  itm1, itm2: TListItem;
  p1, p2: PSourceDefine;
begin
  itm1 := TListItem(lParam1);
  itm2 := TListItem(lParam2);
  p1 := itm1.Data;
  p2 := itm2.Data;
  try
    if lParamSort = 0 then
        Result := CompText(itm1.Caption, itm2.Caption)
    else if lParamSort = 1 then
        Result := CompText(p1^.NewName, p2^.NewName)
    else if lParamSort = 2 then
        Result := CompInt(umlGetFileSize(p1^.SourceFile), umlGetFileSize(p2^.SourceFile))
    else if lParamSort = 3 then
        Result := CompareDateTime(umlGetFileTime(p1^.SourceFile), umlGetFileTime(p2^.SourceFile))
    else if lParamSort = 4 then
        Result := CompText(itm1.SubItems[3], itm2.SubItems[3]);
  except
  end;
end;

function LV_Sort2(lParam2, lParam1, lParamSort: LParam): Integer;
var
  itm1, itm2: TListItem;
  p1, p2: PSourceDefine;
begin
  itm1 := TListItem(lParam1);
  itm2 := TListItem(lParam2);
  p1 := itm1.Data;
  p2 := itm2.Data;
  try
    if lParamSort = 0 then
        Result := CompText(itm1.Caption, itm2.Caption)
    else if lParamSort = 1 then
        Result := CompText(p1^.NewName, p2^.NewName)
    else if lParamSort = 2 then
        Result := CompInt(umlGetFileSize(p1^.SourceFile), umlGetFileSize(p2^.SourceFile))
    else if lParamSort = 3 then
        Result := CompareDateTime(umlGetFileTime(p1^.SourceFile), umlGetFileTime(p2^.SourceFile))
    else if lParamSort = 4 then
        Result := CompText(itm1.SubItems[3], itm2.SubItems[3]);
  except
  end;
end;

procedure TPascalRewriteModelForm.fpsTimerTimer(Sender: TObject);
begin
  CheckThread;
end;

procedure TPascalRewriteModelForm.PageCChange(Sender: TObject);
var
  m64: TMS64;
begin
  if PageC.ActivePage = TS_define then
      UpdateUnitDefineListView
  else if PageC.ActivePage = TS_SymbolRewriteDefine then
    begin
      m64 := TMS64.Create;
      SymbolProcessorList.SaveToStream(m64, True);
      m64.Position := 0;
      SymbolJsonSourceMemo.Lines.LoadFromStream(m64);
      disposeObject(m64);
    end
  else if PageC.ActivePage = TS_Model then
    begin
      UpdateSymbolReplaceList(False);
      UpdateUnitProcessorList(False);
    end;
end;

procedure TPascalRewriteModelForm.NewworkspacefromPassByYou888Model_MIClick(Sender: TObject);
var
  stream: TCore_Stream;
begin
  stream := FileIOOpen('PassByYou888UpLevelModel.OX2');
  NewProj_Stream(stream);
  stream.Free;
end;

procedure TPascalRewriteModelForm.Newworkspace_MIClick(Sender: TObject);
begin
  NewProj;
end;

procedure TPascalRewriteModelForm.OpenWorkspace_MIClick(Sender: TObject);
begin
  if not OpenWorkspaceDialog.Execute then
      Exit;
  OpenProj_File(OpenWorkspaceDialog.FileName);
end;

procedure TPascalRewriteModelForm.Saveworkspace_MIClick(Sender: TObject);
begin
  if CurrentProj = '' then
    begin
      if not SaveWorkspaceDialog.Execute then
          Exit;
      CurrentProj := SaveWorkspaceDialog.FileName;
    end;
  SaveProj(CurrentProj);
end;

procedure TPascalRewriteModelForm.SaveworkspaceAsMIClick(Sender: TObject);
begin
  if not SaveWorkspaceDialog.Execute then
      Exit;
  CurrentProj := SaveWorkspaceDialog.FileName;
  SaveProj(CurrentProj);
end;

procedure TPascalRewriteModelForm.Closeworkspace_MIClick(Sender: TObject);
begin
  NewProj;
end;

procedure TPascalRewriteModelForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TPascalRewriteModelForm.AddUnitSourceButtonClick(Sender: TObject);
var
  i: Integer;
begin
  if not OpenUnitDialog.Execute then
      Exit;
  for i := 0 to OpenUnitDialog.Files.Count - 1 do
      UnitDefineList.AddFile(OpenUnitDialog.Files[i]);
  UpdateUnitDefineListView;
end;

procedure TPascalRewriteModelForm.AddUnitFromDirectoryButtonClick(Sender: TObject);
var
  s: string;
begin
  if SelectDirectory('select unit directory', '', s, [sdNewFolder, sdShowEdit, sdShowShares, sdNewUI]) then
    begin
      DoAddUnitFromDirectory(s);
      UpdateUnitDefineListView;
    end;
end;

procedure TPascalRewriteModelForm.UnitFilterEditChange(Sender: TObject);
begin
  UpdateUnitDefineListView;
end;

procedure TPascalRewriteModelForm.Unit_Define_Click(Sender: TObject);
var
  i: Integer;
  itm: TListItem;
  p: PSourceDefine;
  hash: THashStringList;
  fp, fn, ext: U_String;
begin
  if unitDefList.SelCount = 0 then
    begin
      NewUnitNameForm.NewUnitEdit.Text := '%filename%';
      NewUnitNameForm.Selected_CheckBox.Checked := False;
    end
  else if unitDefList.SelCount = 1 then
    begin
      p := unitDefList.Selected.Data;
      NewUnitNameForm.NewUnitEdit.Text := p^.NewName;
      NewUnitNameForm.Selected_CheckBox.Checked := True;
    end
  else if unitDefList.SelCount > 1 then
    begin
      NewUnitNameForm.NewUnitEdit.Text := '%filename%';
      NewUnitNameForm.Selected_CheckBox.Checked := True;
    end;
  if NewUnitNameForm.ShowModal <> mrOk then
      Exit;

  hash := THashStringList.Create;

  for i := 0 to unitDefList.Items.Count - 1 do
    if (not NewUnitNameForm.Selected_CheckBox.Checked) or unitDefList.Items[i].Selected then
      begin
        itm := unitDefList.Items[i];
        p := itm.Data;
        fp := umlGetFilePath(p^.SourceFile);
        fn := umlGetFileName(p^.SourceFile);

        // prefix
        hash['%before%'] := umlChangeFileExt(fn, '');
        hash['%prefix%'] := umlChangeFileExt(fn, '');

        // ext
        ext := umlGetFileExt(fn);
        hash['%ext%'] := ext;
        if (ext.L > 0) and (ext[1] = '.') then
            ext.DeleteFirst;
        hash['%after%'] := ext;
        hash['%postfix%'] := ext;

        // filename
        hash['%filename%'] := fn;

        p^.NewName := hash.Replace(NewUnitNameForm.NewUnitEdit.Text, False, True, 0, 0);
        itm.SubItems[0] := p^.NewName;
      end;
end;

procedure TPascalRewriteModelForm.RefreshUnitList_Click(Sender: TObject);
begin
  UpdateUnitDefineListView;
end;

procedure TPascalRewriteModelForm.RemoveUnit_Define_Click(Sender: TObject);
var
  i: Integer;
begin
  if unitDefList.SelCount = 0 then
      Exit;
  i := 0;
  while i < unitDefList.Items.Count do
    if unitDefList.Items[i].Selected then
      begin
        UnitDefineList.Remove(unitDefList.Items[i].Data);
        unitDefList.Items[i].Delete;
      end
    else
        Inc(i);
  UpdateUnitProcessorList(True);
end;

procedure TPascalRewriteModelForm.unitDefListColumnClick(Sender: TObject; Column: TListColumn);
var
  i: Integer;
begin
  // reset other sort column
  for i := 0 to unitDefList.columns.Count - 1 do
    if unitDefList.columns[i] <> Column then
        unitDefList.columns[i].Tag := 0;

  // imp sort
  if Column.Tag = 0 then
    begin
      unitDefList.CustomSort(@LV_Sort1, Column.Index);
      Column.Tag := 1;
    end
  else
    begin
      unitDefList.CustomSort(@LV_Sort2, Column.Index);
      Column.Tag := 0;
    end;
end;

procedure TPascalRewriteModelForm.unitDefListKeyUp(Sender: TObject; var Key:
  Word; Shift: TShiftState);
begin
  if unitDefList.IsEditing then
      Exit;
  if (unitDefList.SelCount > 0) and (Key = VK_DELETE) then
      RemoveUnit_Define_Click(nil)
  else if Key = VK_F5 then
      UpdateUnitDefineListView;
end;

procedure TPascalRewriteModelForm.Unit_Processor_LBKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F5 then
      UpdateUnitProcessorList(False);
end;

procedure TPascalRewriteModelForm.unitDefine_text_edt_ButtonClick(Sender: TObject);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  UnitDefineList.SaveToStream(m64, True);
  m64.Position := 0;
  PascalRewrite_TextEdtForm.Memo.Lines.LoadFromStream(m64);
  disposeObject(m64);
  if PascalRewrite_TextEdtForm.ShowModal <> mrOk then
      Exit;

  m64 := TMS64.Create;
  PascalRewrite_TextEdtForm.Memo.Lines.SaveToStream(m64);
  m64.Position := 0;
  UnitDefineList.LoadFromStream(m64);
  disposeObject(m64);
  UpdateUnitDefineListView;
end;

procedure TPascalRewriteModelForm.SymbolJsonSourceMemoExit(Sender: TObject);
begin
  UpdateSymbolReplaceList(True);
end;

procedure TPascalRewriteModelForm.addSymbolButtonClick(Sender: TObject);
var
  m64: TMS64;
begin
  UpdateSymbolReplaceList(True);
  if NewSymbolDefForm.ShowModal <> mrOk then
      Exit;

  SymbolProcessorList.AddUName(NewSymbolDefForm.OLD_Edit.Text, NewSymbolDefForm.NewEdit.Text);

  m64 := TMS64.Create;
  SymbolProcessorList.SaveToStream(m64, True);
  m64.Position := 0;
  SymbolJsonSourceMemo.Lines.LoadFromStream(m64);
  disposeObject(m64);

  SymbolJsonSourceMemo.SelStart := length(SymbolJsonSourceMemo.Text);
  SymbolJsonSourceMemo.SelLength := 0;
  SymbolJsonSourceMemo.SetFocus;
end;

procedure TPascalRewriteModelForm.Fill_TextAsSymbolDefine_Click(Sender: TObject);
var
  js: TZJ;
  i: Integer;
  L: TStringList;
  clip: TClipboard;
  N1, N2, n3: U_String;
  m64: TMS64;
begin
  L := TStringList.Create;
  clip := TClipboard.Create;
  L.Text := clip.AsText;
  disposeObject(clip);

  for i := 0 to L.Count - 1 do
    begin
      N1 := L[i];
      N1 := N1.TrimChar(#32#9);
      if N1 <> '' then
        begin
          if N1.Exists('=') then
            begin
              n3 := umlGetLastStr(N1, '=');
              N1 := umlGetFirstStr(N1, '=');
            end
          else
            begin
              n3 := N1;
            end;
          if not SymbolProcessorList.Exists_OLD_Name(N1) then
            begin
              if Paste_Replace_Sour_Edit.Text <> '' then
                  N2 := umlReplace(n3, Paste_Replace_Sour_Edit.Text, Paste_Replace_Dest_Edit.Text, False, False, 0, 0, nil)
              else
                  N2 := n3;
              SymbolProcessorList.AddUName(N1, N2);
            end;
        end;
    end;
  disposeObject(L);

  m64 := TMS64.Create;
  SymbolProcessorList.SaveToStream(m64, True);
  m64.Position := 0;
  SymbolJsonSourceMemo.Clear;
  SymbolJsonSourceMemo.Lines.LoadFromStream(m64);
  disposeObject(m64);
  UpdateSymbolReplaceList(True);
end;

procedure TPascalRewriteModelForm.reset_symbol_rewrite_define_ButtonClick(Sender: TObject);
var
  m64: TMS64;
begin
  SymbolProcessorList.Clean;
  m64 := TMS64.Create;
  SymbolProcessorList.SaveToStream(m64, True);
  m64.Position := 0;
  SymbolJsonSourceMemo.Lines.LoadFromStream(m64);
  disposeObject(m64);
  UpdateSymbolReplaceList(True);
end;

procedure TPascalRewriteModelForm.symbol_rewrite_file_features_LBKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
      symbol_rewrite_file_features_LB.DeleteSelected;
end;

procedure TPascalRewriteModelForm.AddCodeFeatureFileButtonClick(Sender: TObject);
var
  i: Integer;
begin
  if not OpenUnitDialog.Execute then
      Exit;
  for i := 0 to OpenUnitDialog.Files.Count - 1 do
      umlAddNewStrTo(OpenUnitDialog.Files[i], symbol_rewrite_file_features_LB.Items);
end;

procedure TPascalRewriteModelForm.ScanFeatureButtonClick(Sender: TObject);
begin
  if umlTrimSpace(CodeFeatureEdit.Text) = '' then
      Exit;
  PageC.Enabled := False;
  TCompute.RunP_NP(procedure
    var
      featureHash: THashList;
      i: Integer;
      fn: U_String;
      TP: TTextParsing;
      Code: TPascalStringList;
      n: U_String;
      Info: TBatchInfoList;
      j: Integer;
      p: PTokenData;
      k: TPascal_Keyword;
    begin
      featureHash := THashList.CustomCreate($FFFF);

      for i := 0 to symbol_rewrite_file_features_LB.Items.Count - 1 do
        begin
          fn := symbol_rewrite_file_features_LB.Items[i];
          DoStatus('Scan "%s" ...', [umlGetFileName(fn).Text]);
          Code := TPascalStringList.Create;
          Code.LoadFromFile(fn);
          n := Code.AsText;
          disposeObject(Code);
          TP := TTextParsing.Create(n, tsPascal, nil, TPascalString(SpacerSymbol.V + '.'));
          Info := TBatchInfoList.Create;
          umlReplaceSum(n, CodeFeatureEdit.Text, False, False, 0, 0, Info);
          for j := 0 to Info.Count - 1 do
            begin
              try
                  p := TP.CharToken[Info[j].sour_bPos];
              except
                  p := nil;
              end;
              if (p <> nil) and (p^.tokenType = ttASCII) then
                  featureHash.Add(p^.Text, nil, True);
            end;
          disposeObject(Info);
          disposeObject(TP);
          DoStatus('Scan "%s" done.', [umlGetFileName(fn).Text]);
        end;

      for k := low(TPascal_Keyword) to high(TPascal_Keyword) do
          featureHash.Delete(Pascal_Keyword_DICT[k].Decl);

      TCompute.Sync(procedure
        begin
          FeatureOutputMemo.Clear;
          featureHash.ProgressP(procedure(Name_: PSystemString; hData: PHashListData)
            begin
              FeatureOutputMemo.Lines.Add(PFormat('%s=%s', [Name_^, Name_^]));
            end);
          PageC.Enabled := True;
          FeatureOutputMemo.CopyToClipboard;
        end);
      disposeObject(featureHash);
    end);
end;

procedure TPascalRewriteModelForm.ScanFeatureFromClipboardButtonClick(Sender: TObject);
begin
  if umlTrimSpace(CodeFeatureEdit.Text) = '' then
      Exit;
  PageC.Enabled := False;
  TCompute.RunP_NP(procedure
    var
      featureHash: THashList;
      n: U_String;
      i: Integer;
      TP: TTextParsing;
      Code: TPascalStringList;
      Info: TBatchInfoList;
      j: Integer;
      p: PTokenData;
      k: TPascal_Keyword;
    begin
      featureHash := THashList.CustomCreate($FFFF);
      DoStatus('Scan...');
      n.Text := Clipboard.AsText;
      TP := TTextParsing.Create(n, tsPascal, nil, TPascalString(SpacerSymbol.V + '.'));
      Info := TBatchInfoList.Create;
      umlReplaceSum(n, CodeFeatureEdit.Text, False, False, 0, 0, Info);
      for j := 0 to Info.Count - 1 do
        begin
          try
              p := TP.CharToken[Info[j].sour_bPos];
          except
              p := nil;
          end;
          if (p <> nil) and (p^.tokenType = ttASCII) then
              featureHash.Add(p^.Text, nil, True);
        end;
      disposeObject(Info);
      disposeObject(TP);
      DoStatus('Scan done.');

      for k := low(TPascal_Keyword) to high(TPascal_Keyword) do
          featureHash.Delete(Pascal_Keyword_DICT[k].Decl);

      TCompute.Sync(procedure
        begin
          FeatureOutputMemo.Clear;
          featureHash.ProgressP(procedure(Name_: PSystemString; hData: PHashListData)
            begin
              FeatureOutputMemo.Lines.Add(PFormat('%s=%s', [Name_^, Name_^]));
            end);
          PageC.Enabled := True;
          FeatureOutputMemo.CopyToClipboard;
        end);
      disposeObject(featureHash);
    end);
end;

procedure TPascalRewriteModelForm.DoCreateModel_BtnClick(Sender: TObject);
var
  m64: TMS64;
begin
  if not Check_RewritePascal_Model(UnitProcessorList, SymbolProcessorList) then
    begin
      DoStatus('model error.');
      Exit;
    end
  else
    begin
      DoStatus('model check ok.');
    end;

  if ModelOuytputEdit.Text = '' then
      Exit;
  m64 := Build_RewritePascal_Model(UnitProcessorList, SymbolProcessorList);
  m64.SaveToFile(ModelOuytputEdit.Text);
  DoStatus('Done model %s', [ModelOuytputEdit.Text]);
  disposeObject(m64);
end;

procedure TPascalRewriteModelForm.Browse_ModelOuytput_BtnClick(Sender: TObject);
begin
  SaveModelDialog.FileName := ModelOuytputEdit.Text;
  if not SaveModelDialog.Execute then
      Exit;
  ModelOuytputEdit.Text := SaveModelDialog.FileName;
end;

procedure TPascalRewriteModelForm.check_model_ButtonClick(Sender: TObject);
begin
  if not Check_RewritePascal_Model(UnitProcessorList, SymbolProcessorList) then
    begin
      DoStatus('model error.');
      Exit;
    end
  else
    begin
      DoStatus('model check ok.');
    end;
end;

procedure TPascalRewriteModelForm.rep_unit_def_ButtonClick(Sender: TObject);
begin
  if TextRepToolForm.ShowModal <> mrOk then
      Exit;
  UnitDefineList.ReplaceNewName(TextRepToolForm.OLDEdit.Text, TextRepToolForm.NewEdit.Text,
    TextRepToolForm.Word_CheckBox.Checked, TextRepToolForm.IgnoreCase_CheckBox.Checked);
  UpdateUnitDefineListView;
end;

procedure TPascalRewriteModelForm.rep_sym_old_ButtonClick(Sender: TObject);
var
  m64: TMS64;
begin
  UpdateSymbolReplaceList(True);
  if TextRepToolForm.ShowModal <> mrOk then
      Exit;
  SymbolProcessorList.ReplaceOLD_Name(TextRepToolForm.OLDEdit.Text, TextRepToolForm.NewEdit.Text,
    TextRepToolForm.Word_CheckBox.Checked, TextRepToolForm.IgnoreCase_CheckBox.Checked);

  m64 := TMS64.Create;
  SymbolProcessorList.SaveToStream(m64, True);
  m64.Position := 0;
  SymbolJsonSourceMemo.Lines.LoadFromStream(m64);
  disposeObject(m64);
  UpdateSymbolReplaceList(False);
end;

procedure TPascalRewriteModelForm.rep_sym_new_ButtonClick(Sender: TObject);
var
  m64: TMS64;
begin
  UpdateSymbolReplaceList(True);
  if TextRepToolForm.ShowModal <> mrOk then
      Exit;
  SymbolProcessorList.ReplaceNew_Name(TextRepToolForm.OLDEdit.Text, TextRepToolForm.NewEdit.Text,
    TextRepToolForm.Word_CheckBox.Checked, TextRepToolForm.IgnoreCase_CheckBox.Checked);

  m64 := TMS64.Create;
  SymbolProcessorList.SaveToStream(m64, True);
  m64.Position := 0;
  SymbolJsonSourceMemo.Lines.LoadFromStream(m64);
  disposeObject(m64);
  UpdateSymbolReplaceList(False);
end;

procedure TPascalRewriteModelForm.Rebuild_PreDefine_Click(Sender: TObject);
begin
  UpdateUnitProcessorList(True);
end;

procedure TPascalRewriteModelForm.Swap_Predefine_Click(Sender: TObject);
var
  i: Integer;
  p: PSourceProcessorData;
begin
  for i := 0 to UnitProcessorList.Count - 1 do
    begin
      p := UnitProcessorList[i];
      p^.OLD_Name.SwapInstance(p^.New_Name);
    end;
  UpdateUnitProcessorList(False);
end;

procedure TPascalRewriteModelForm.Rebuild_Symbol_Click(Sender: TObject);
begin
  UpdateSymbolReplaceList(True);
end;

procedure TPascalRewriteModelForm.Swap_Symbol_Click(Sender: TObject);
var
  i: Integer;
  p: PSourceProcessorData;
begin
  for i := 0 to SymbolProcessorList.Count - 1 do
    begin
      p := SymbolProcessorList[i];
      p^.OLD_Name.SwapInstance(p^.New_Name);
    end;
  UpdateSymbolReplaceList(False);
end;

procedure TPascalRewriteModelForm.testButtonClick(Sender: TObject);
begin
  PageC.Enabled := False;
  TCompute.RunP_NP(procedure
    var
      uHash, PatternDefine: THashStringList;
      Code: TCore_StringList;
    begin
      uHash := THashStringList.CustomCreate(1024);
      UnitProcessorList.Build_uHash(uHash);
      PatternDefine := THashStringList.CustomCreate(1024);
      SymbolProcessorList.Build_uHash(PatternDefine);
      Code := TCore_StringList.Create;
      TCompute.Sync(procedure
        begin
          Code.Assign(testMemo.Lines);
        end);

      if RewritePascal_Process_Code(Code, uHash, PatternDefine, 'TestCode', procedure(const Fmt: SystemString; const Args: array of const)
        begin
          DoStatus(Fmt, Args);
        end) then
        begin
          TCompute.Sync(procedure
            begin
              testMemo.Lines.Assign(Code);
            end);
        end;

      disposeObject(Code);
      disposeObject(uHash);
      disposeObject(PatternDefine);
      DoStatus('all apply done.');
      TCompute.Sync(procedure
        begin
          PageC.Enabled := True;
        end);
    end);
end;

procedure TPascalRewriteModelForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  if Memo.Lines.Count > 5000 then
      Memo.Lines.Clear;
  Memo.Lines.Add(Text_);
end;

procedure TPascalRewriteModelForm.NewProj;
var
  m64: TMS64;
begin
  CurrentProj := '';
  UnitDefineList.Clean;
  UnitProcessorList.Clean;
  SymbolProcessorList.Clean;

  try
    m64 := TMS64.Create;
    SymbolProcessorList.SaveToStream(m64, True);
    m64.Position := 0;
    SymbolJsonSourceMemo.Lines.LoadFromStream(m64);
    disposeObject(m64);
  except
  end;

  UpdateUnitDefineListView;
  UpdateUnitProcessorList(True);
  UpdateSymbolReplaceList(True);
  DoStatus('New Workspace.');
end;

procedure TPascalRewriteModelForm.NewProj_Stream(stream_: TCore_Stream);
var
  dec: TZDB2_File_Decoder;
  fi: TZDB2_FI;
  m64: TMS64;
begin
  if not TZDB2_File_Decoder.Check(stream_) then
      Exit;

  NewProj();

  dec := TZDB2_File_Decoder.Create(stream_, 2);

  fi := dec.Files.FindFile('Define');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      UnitDefineList.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Unit Define Data.');
    end;

  fi := dec.Files.FindFile('Unit');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      UnitProcessorList.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Unit Rewrite Model.');
    end;

  fi := dec.Files.FindFile('Pattern');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      SymbolProcessorList.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Symbol Rewrite Model.');
    end;

  disposeObject(dec);

  try
    m64 := TMS64.Create;
    SymbolProcessorList.SaveToStream(m64, True);
    m64.Position := 0;
    SymbolJsonSourceMemo.Lines.LoadFromStream(m64);
    disposeObject(m64);
  except
  end;

  UpdateUnitDefineListView;
  UpdateUnitProcessorList(True);
  UpdateSymbolReplaceList(True);
end;

procedure TPascalRewriteModelForm.OpenProj_File(fn: U_String);
var
  dec: TZDB2_File_Decoder;
  fi: TZDB2_FI;
  m64: TMS64;
begin
  if not TZDB2_File_Decoder.CheckFile(fn) then
      Exit;

  NewProj();

  dec := TZDB2_File_Decoder.CreateFile(fn, 2);

  fi := dec.Files.FindFile('Define');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      UnitDefineList.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Unit Define Data.');
    end;

  fi := dec.Files.FindFile('Unit');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      UnitProcessorList.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Unit Rewrite Model.');
    end;

  fi := dec.Files.FindFile('Pattern');
  if fi <> nil then
    begin
      m64 := TMS64.Create;
      dec.DecodeToStream(fi, m64);
      m64.Position := 0;
      SymbolProcessorList.LoadFromStream(m64);
      disposeObject(m64);
      DoStatus('Open Symbol Rewrite Model.');
    end;

  disposeObject(dec);

  try
    m64 := TMS64.Create;
    SymbolProcessorList.SaveToStream(m64, True);
    m64.Position := 0;
    SymbolJsonSourceMemo.Lines.LoadFromStream(m64);
    disposeObject(m64);
  except
  end;

  UpdateUnitDefineListView;
  UpdateUnitProcessorList(True);
  UpdateSymbolReplaceList(True);

  CurrentProj := fn;
  DoStatus('open done "%s"', [umlGetFileName(CurrentProj).Text]);
end;

procedure TPascalRewriteModelForm.SaveProj(fn: U_String);
var
  enc: TZDB2_File_Encoder;
  fi: TZDB2_FI;
  tmp: TMS64;
begin
  enc := TZDB2_File_Encoder.CreateFile(fn, 2);

  tmp := TMS64.Create;
  UnitDefineList.SaveToStream(tmp, False);
  fi := enc.EncodeFromStream(tmp, 1024, TSelectCompressionMethod.scmZLIB_Max, 4096);
  fi.FileName := 'Define';
  fi.FimeTime := umlNow;
  disposeObject(tmp);
  DoStatus('%s %s->%s ratio:%d%%',
    [
    'Unit Define Data',
    umlSizeToStr(fi.Size).Text,
    umlSizeToStr(fi.Compressed).Text,
    100 - umlPercentageToInt64(fi.Size, fi.Compressed)]);

  tmp := TMS64.Create;
  UnitProcessorList.SaveToStream(tmp, False);
  fi := enc.EncodeFromStream(tmp, 1024, TSelectCompressionMethod.scmZLIB_Max, 4096);
  fi.FileName := 'Unit';
  fi.FimeTime := umlNow;
  disposeObject(tmp);
  DoStatus('%s %s->%s ratio:%d%%',
    [
    'Unit Rewrite Model',
    umlSizeToStr(fi.Size).Text,
    umlSizeToStr(fi.Compressed).Text,
    100 - umlPercentageToInt64(fi.Size, fi.Compressed)]);

  tmp := TMS64.Create;
  SymbolProcessorList.SaveToStream(tmp, False);
  fi := enc.EncodeFromStream(tmp, 1024, TSelectCompressionMethod.scmZLIB_Max, 4096);
  fi.FileName := 'Pattern';
  fi.FimeTime := umlNow;
  disposeObject(tmp);
  DoStatus('%s %s->%s ratio:%d%%',
    [
    'Symbol Rewrite Model',
    umlSizeToStr(fi.Size).Text,
    umlSizeToStr(fi.Compressed).Text,
    100 - umlPercentageToInt64(fi.Size, fi.Compressed)]);

  enc.Flush;
  disposeObject(enc);

  CurrentProj := fn;
  DoStatus('save done "%s" size: %s', [umlGetFileName(CurrentProj).Text, umlSizeToStr(umlGetFileSize(CurrentProj)).Text]);
end;

procedure TPascalRewriteModelForm.DoAddUnitFromDirectory(dir: U_String);
var
  fArry, dArry: U_StringArray;
  n: U_SystemString;
begin
  fArry := umlGetFileListWithFullPath(dir);
  for n in fArry do
    if umlMultipleMatch(['*.pas', '*.inc', '*.pp'], n) then
        UnitDefineList.AddFile(n);
  dArry := umlGetDirListWithFullPath(dir);
  for n in dArry do
      DoAddUnitFromDirectory(n);
end;

procedure TPascalRewriteModelForm.UpdateUnitDefineListView;
var
  i: Integer;
  p: PSourceDefine;
  itm: TListItem;
  nf1, nf2: U_String;
begin
  unitDefList.Clear;
  unitDefList.Items.BeginUpdate;
  for i := 0 to UnitDefineList.Count - 1 do
    begin
      p := UnitDefineList[i];
      if umlSearchMatch(UnitFilterEdit.Text, p^.SourceFile) or umlSearchMatch(UnitFilterEdit.Text, p^.NewName) then
        begin
          itm := unitDefList.Items.Add;
          itm.Caption := umlGetFileName(p^.SourceFile);
          itm.SubItems.Add(p^.NewName);
          nf1 := umlCombineFileName(umlGetFilePath(p^.SourceFile), p^.NewName);
          nf2 := umlChangeFileExt(nf1, '') + '_LIB' + umlGetFileExt(nf1);
          if umlFileExists(p^.SourceFile) then
            begin
              itm.SubItems.Add(umlSizeToStr(umlGetFileSize(p^.SourceFile)));
              itm.SubItems.Add(DateTimeToStr(umlGetFileTime(p^.SourceFile)));
            end
          else if umlFileExists(nf1) then
            begin
              itm.SubItems.Add(umlSizeToStr(umlGetFileSize(nf1)));
              itm.SubItems.Add(DateTimeToStr(umlGetFileTime(nf1)));
            end
          else if umlFileExists(nf2) then
            begin
              itm.SubItems.Add(umlSizeToStr(umlGetFileSize(nf2)));
              itm.SubItems.Add(DateTimeToStr(umlGetFileTime(nf2)));
            end
          else
            begin
              itm.SubItems.Add(umlSizeToStr(0));
              itm.SubItems.Add(DateTimeToStr(0));
              DoStatus('no found %s', [p^.SourceFile.Text]);
            end;
          itm.SubItems.Add(umlGetFilePath(p^.SourceFile));
          itm.Data := p;
        end;
    end;
  unitDefList.Items.EndUpdate;
  unitDefList.Width := unitDefList.Width - 1;
  UpdateUnitProcessorList(False);
end;

procedure TPascalRewriteModelForm.UpdateUnitProcessorList(rebuild_: Boolean);
var
  i: Integer;
  p: PSourceProcessorData;
  itm: TListItem;
begin
  if rebuild_ or (UnitProcessorList.Count = 0) then
    if UnitDefineList.Count > 0 then
        UnitDefineList.UpdateToUnitProcessor(UnitProcessorList);

  Unit_Processor_LB.Clear;
  Unit_Processor_LB.Items.BeginUpdate;
  for i := 0 to UnitProcessorList.Count - 1 do
    begin
      p := UnitProcessorList[i];
      Unit_Processor_LB.Items.Add(p^.OLD_Name + ' -> ' + p^.New_Name);
    end;
  Unit_Processor_LB.Items.EndUpdate;
end;

procedure TPascalRewriteModelForm.UpdateSymbolReplaceList(rebuild_: Boolean);
var
  m64: TMS64;
  i: Integer;
  p: PSourceProcessorData;
  itm: TListItem;
begin
  if rebuild_ or (SymbolProcessorList.Count = 0) then
    begin
      try
        m64 := TMS64.Create;
        SymbolJsonSourceMemo.Lines.WriteBOM := False;
        SymbolJsonSourceMemo.Lines.SaveToStream(m64, TEncoding.UTF8);
        m64.Position := 0;
        SymbolProcessorList.Clean;
        SymbolProcessorList.LoadFromStream(m64);
        disposeObject(m64);
      except
          SymbolProcessorList.Clean;
      end;
    end;

  SymbolReplaceListBox.Clear;
  SymbolReplaceListBox.Items.BeginUpdate;
  for i := 0 to SymbolProcessorList.Count - 1 do
    begin
      p := SymbolProcessorList[i];
      SymbolReplaceListBox.Items.Add(p^.OLD_Name + ' -> ' + p^.New_Name);
    end;
  SymbolReplaceListBox.Items.EndUpdate;
end;

constructor TPascalRewriteModelForm.Create(AOwner: TComponent);
var
  m64: TMS64;
begin
  inherited Create(AOwner);
  UnitDefineList := TSourceDefinePool.Create;
  UnitProcessorList := TSourceProcessorDataPool.Create;
  SymbolProcessorList := TSourceProcessorDataPool.Create;
  AddDoStatusHook(self, DoStatus_backcall);
  StatusThreadID := False;

  InitGlobalMedia([gmtUser]);

  NewProj;

  try
    m64 := TMS64.Create;
    SymbolProcessorList.SaveToStream(m64, True);
    m64.Position := 0;
    SymbolJsonSourceMemo.Lines.LoadFromStream(m64);
    disposeObject(m64);
  except
  end;

  TCompute.RunP_NP(procedure()
    begin
      TCompute.Sleep(500);
      TCompute.Sync(procedure
        begin
          UpdateUnitDefineListView();
        end);
    end);
end;

destructor TPascalRewriteModelForm.Destroy;
begin
  FreeGlobalMedia;
  inherited Destroy;
end;

end.
