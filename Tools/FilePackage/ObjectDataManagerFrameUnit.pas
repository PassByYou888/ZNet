unit ObjectDataManagerFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ActnList, Menus,
  ShellAPI, Actions, FileCtrl, Types, IOUtils,
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.ZDB.ObjectData_LIB, Z.ZDB,
  Z.Notify,
  ObjectDataTreeFrameUnit, Z.ZDB.ItemStream_LIB;

type
  TObjectDataManagerFrame = class(TFrame)
    Splitter: TSplitter;
    ListView: TListView;
    ActionList: TActionList;
    ActionCreateDir: TAction;
    ActionRemove: TAction;
    ActionImportFile: TAction;
    TreePanel: TPanel;
    ActionRename: TAction;
    ActionExport: TAction;
    PopupMenu: TPopupMenu;
    CreateDirectory1: TMenuItem;
    Rename1: TMenuItem;
    Importfile1: TMenuItem;
    ExportTo1: TMenuItem;
    Remove1: TMenuItem;
    SaveDialog: TSaveDialog;
    Action_Open: TAction;
    Open1: TMenuItem;
    N2: TMenuItem;
    ActionImportDirectory: TAction;
    ImportDirectory1: TMenuItem;
    OpenDialog: TFileOpenDialog;
    Action_Search_Item: TAction;
    SearchItem1: TMenuItem;
    procedure ActionCreateDirExecute(Sender: TObject);
    procedure ActionExportExecute(Sender: TObject);
    procedure ActionImportFileExecute(Sender: TObject);
    procedure ActionImportDirectoryExecute(Sender: TObject);
    procedure ActionRemoveExecute(Sender: TObject);
    procedure ActionRenameExecute(Sender: TObject);
    procedure Action_OpenExecute(Sender: TObject);
    procedure Action_Search_ItemExecute(Sender: TObject);
    procedure ListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
    procedure ListViewEdited(Sender: TObject; Item: TListItem; var s: string);
    procedure ListViewEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
    procedure ListViewKeyUp(Sender: TObject; var key: Word; Shift: TShiftState);
  private
    { Private declarations }
    FResourceData: TObjectDataManager;
    FResourceTreeFrame: TObjectDataTreeFrame;
    FIsModify: Boolean;

    FFileFilter: string;

    procedure SetResourceData(Value: TObjectDataManager);
    function GetCurrentObjectDataPath: string;
    procedure SetCurrentObjectDataPath(const Value: string);
    procedure OpenObjectDataPath(Path_: string);

    procedure SetFileFilter(const Value: string);
    function GetMultiSelect: Boolean;
    procedure SetMultiSelect(const Value: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateItemList(Path_: string);
    procedure Refresh_All;

    procedure ExportDBPathToPath(DBPath_, destDir_: string);
    procedure ExportToFile(DBPath_, DBItem_, destDir_, destFileName_: string; var showMsg: Boolean);
    procedure ImportFromFile(FileName_: string; var showMsg: Boolean);

    property ResourceData: TObjectDataManager read FResourceData write SetResourceData;

    property CurrentObjectDataPath: string read GetCurrentObjectDataPath write SetCurrentObjectDataPath;
    property ResourceTreeFrame: TObjectDataTreeFrame read FResourceTreeFrame write FResourceTreeFrame;
    property IsModify: Boolean read FIsModify write FIsModify;

    property fileFilter: string read FFileFilter write SetFileFilter;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect;
  end;

  TZDB_List_Item = class(TListItem)
  public
    isField, isItem: Boolean;
    constructor Create(AOwner: TListItems); override;
    destructor Destroy; override;
  end;

implementation

uses Z.Status, Z.ZDB.HashField_LIB, Z.ZDB.HashItem_LIB;

{$R *.dfm}


constructor TZDB_List_Item.Create(AOwner: TListItems);
begin
  inherited Create(AOwner);
  isField := False;
  isItem := False;
end;

destructor TZDB_List_Item.Destroy;
begin
  inherited Destroy;
end;

procedure TObjectDataManagerFrame.ActionCreateDirExecute(Sender: TObject);
var
  itm: TZDB_List_Item;
begin
  if FResourceData = nil then
      Exit;

  itm := ListView.Items.Add as TZDB_List_Item;
  itm.isField := True;
  itm.Caption := '';
  itm.SubItems.Add('Wait Edition...');
  itm.SubItems.Add('Wait Edition...');
  itm.EditCaption;
  IsModify := True;
end;

procedure TObjectDataManagerFrame.ActionExportExecute(Sender: TObject);
var
  showMsg: Boolean;
  i: Integer;
  destDir: string;
begin
  if FResourceData = nil then
      Exit;
  if ListView.IsEditing then
      Exit;
  if ListView.SelCount = 1 then
    begin
      if TZDB_List_Item(ListView.Selected).isField then
        begin
          if not SelectDirectory('export to', '', destDir, [sdNewFolder, sdShowEdit, sdShowShares, sdNewUI]) then
              Exit;
          ExportDBPathToPath(umlCombineUnixPath(CurrentObjectDataPath, ListView.Selected.Caption), umlCombinePath(destDir, ListView.Selected.Caption));
        end
      else if TZDB_List_Item(ListView.Selected).isItem then
        begin
          SaveDialog.FileName := ListView.Selected.Caption;
          if not SaveDialog.Execute() then
              Exit;
          showMsg := True;
          ExportToFile(CurrentObjectDataPath, ListView.Selected.Caption, umlGetFilePath(SaveDialog.FileName), umlGetFileName(SaveDialog.FileName), showMsg);
        end;
    end
  else
    begin
      if not SelectDirectory('export to', '', destDir, [sdNewFolder, sdShowEdit, sdShowShares, sdNewUI]) then
          Exit;

      showMsg := True;
      for i := 0 to ListView.Items.Count - 1 do
        begin
          with ListView.Items[i] as TZDB_List_Item do
            begin
              if (Selected) or (ListView.SelCount = 0) then
                begin
                  if isField then
                      ExportDBPathToPath(umlCombineUnixPath(CurrentObjectDataPath, Caption), umlCombinePath(destDir, Caption))
                  else if isItem then
                      ExportToFile(CurrentObjectDataPath, Caption, destDir, Caption, showMsg);
                end;
            end;
        end;
    end;
end;

procedure TObjectDataManagerFrame.ActionImportFileExecute(Sender: TObject);
var
  i: Integer;
  showMsg: Boolean;
begin
  if FResourceData = nil then
      Exit;
  if OpenDialog.Execute then
    begin
      if OpenDialog.Files.Count > 0 then
        begin
          showMsg := True;
          for i := 0 to OpenDialog.Files.Count - 1 do
              ImportFromFile(OpenDialog.Files[i], showMsg);
          UpdateItemList(CurrentObjectDataPath);
          FResourceTreeFrame.RefreshList;
          IsModify := True;
        end;
    end;
end;

procedure TObjectDataManagerFrame.ActionImportDirectoryExecute(Sender: TObject);
  procedure ImpFromPath(ImpPath, DBPath: U_String);
  var
    fAry: U_StringArray;
    n: U_SystemString;
    fPos: Int64;
    fs: TCore_FileStream;
    itmHnd: TItemHandle;
    itmStream: TItemStream;
    longName: Boolean;
  begin
    DBPath := umlCharReplace(DBPath, '\', '/');
    if not FResourceData.DirectoryExists(DBPath) then
        FResourceData.CreateField(DBPath, '');
    fPos := FResourceData.GetPathFieldPos(DBPath);

    fAry := umlGetFileListWithFullPath(ImpPath);
    for n in fAry do
      begin
        longName := FResourceData.Handle^.IOHnd.CheckFixedStringLoss(umlGetFileName(n));

        if longName then
            MessageDlg(Format('File name %s is too long, which causes character loss!', [umlGetFileName(n).Text]), mtWarning, [mbOk], 0);

        FResourceData.ItemFastCreate(fPos, umlGetFileName(n), '', itmHnd);
        itmHnd.CreateTime := umlGetFileTime(n);
        itmHnd.ModificationTime := itmHnd.CreateTime;
        fs := TCore_FileStream.Create(n, fmOpenRead);
        itmStream := TItemStream.Create(FResourceData, itmHnd);
        DoStatus('import %s', [umlCombineFileName(DBPath, itmHnd.Name).Text]);
        try
            itmStream.CopyFrom(fs, fs.Size)
        except
        end;
        itmStream.CloseHandle;
        DisposeObject(fs);
        DisposeObject(itmStream);
      end;

    fAry := umlGetDirListPath(ImpPath);
    for n in fAry do
        ImpFromPath(umlCombinePath(ImpPath, n), umlCombinePath(DBPath, n));
  end;

var
  d: string;
begin
  if FResourceData = nil then
      Exit;
  if not SelectDirectory('Import directory', '', d, [sdNewFolder, sdShowEdit, sdNewUI]) then
      Exit;

  ImpFromPath(d, CurrentObjectDataPath);

  UpdateItemList(CurrentObjectDataPath);
  FResourceTreeFrame.RefreshList;
  IsModify := True;
end;

procedure TObjectDataManagerFrame.ActionRemoveExecute(Sender: TObject);
var
  i: Integer;
begin
  if FResourceData = nil then
      Exit;
  if ListView.IsEditing then
      Exit;
  if MessageDlg('remove?', mtWarning, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  if ListView.SelCount > 0 then
    begin
      for i := 0 to ListView.Items.Count - 1 do
        begin
          with ListView.Items[i] as TZDB_List_Item do
            begin
              if Selected then
                begin
                  if isField then
                    begin
                      if FResourceData.FieldDelete(CurrentObjectDataPath, Caption) then
                          DoStatus(Format('delete Field "%s" success', [Caption]));
                    end
                  else if isItem and FResourceData.ItemDelete(CurrentObjectDataPath, Caption) then
                      DoStatus(Format('delete item "%s" success', [Caption]));
                end;
            end;
        end;
      UpdateItemList(CurrentObjectDataPath);
      FResourceTreeFrame.RefreshList;
      IsModify := True;
    end;
end;

procedure TObjectDataManagerFrame.ActionRenameExecute(Sender: TObject);
begin
  if FResourceData = nil then
      Exit;
  if ListView.IsEditing then
      Exit;
  if ListView.Selected <> nil then
      ListView.Selected.EditCaption;
end;

procedure TObjectDataManagerFrame.Action_OpenExecute(Sender: TObject);
var
  showMsg: Boolean;
begin
  if FResourceData = nil then
      Exit;
  if ListView.IsEditing then
      Exit;
  if ListView.SelCount = 1 then
    begin
      if TZDB_List_Item(ListView.Selected).isField then
        begin
          CurrentObjectDataPath := umlCombineUnixPath(CurrentObjectDataPath, ListView.Selected.Caption);
        end
      else if TZDB_List_Item(ListView.Selected).isItem then
        begin
          showMsg := False;
          ExportToFile(CurrentObjectDataPath, ListView.Selected.Caption, TPath.GetTempPath, ListView.Selected.Caption, showMsg);
          ShellExecute(0, 'open', PWideChar(umlCombineFileName(TPath.GetTempPath, ListView.Selected.Caption).Text), '', PWideChar(TPath.GetTempPath), SW_SHOW);
        end;
    end;
end;

procedure TObjectDataManagerFrame.Action_Search_ItemExecute(Sender: TObject);
var
  field_pos: Int64;
  s: string;
  ir: TItemRecursionSearch;
  tmp: U_String;
begin
  if FResourceData = nil then
      Exit;

  if not InputQuery('search item', 'search item:', s) then
      Exit;

  field_pos := FResourceData.GetPathFieldPos(CurrentObjectDataPath);
  if field_pos <= 0 then
      Exit;
  if FResourceData.RecursionSearchFirst(CurrentObjectDataPath, s, ir) then
    begin
      repeat
        tmp := FResourceData.GetFieldPath(ir.CurrentField.RHeader.CurrentHeader, field_pos);
        case ir.ReturnHeader.ID of
          DB_Header_Field_ID:
            begin
              // DoStatus('current: "%s" search result field:"%s"', [CurrentObjectDataPath, umlCombineUnixPath(tmp, ir.ReturnHeader.Name).Text]);
            end;
          DB_Header_Item_ID:
            begin
              DoStatus('current: "%s" search result item:"%s"', [CurrentObjectDataPath, umlCombineUnixFileName(tmp, ir.ReturnHeader.Name).Text]);
            end;
        end;
      until not FResourceData.RecursionSearchNext(ir);
    end;
end;

procedure TObjectDataManagerFrame.ListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
begin
  ItemClass := TZDB_List_Item;
end;

procedure TObjectDataManagerFrame.ListViewEdited(Sender: TObject; Item: TListItem; var s: string);
var
  Field_Pos_: Int64;
  ItemHnd: TItemHandle;
begin
  if FResourceData = nil then
      Exit;
  if TZDB_List_Item(Item).isField and (Item.Caption = '') then
    begin
      if not FResourceData.CreateField(CurrentObjectDataPath + '/' + s, '') then
          Item.Free;
      DoStatus(Format('create new directory "%s"', [CurrentObjectDataPath + '/' + s]));
    end
  else if TZDB_List_Item(Item).isField and (FResourceData.GetPathField(CurrentObjectDataPath + '/' + Item.Caption, Field_Pos_)) then
    begin
      DoStatus(Format('Rename directory "%s" to "%s" .', [Item.Caption, s]));
      if not FResourceData.FieldReName(Field_Pos_, s, '') then
          Item.Free;
    end
  else if TZDB_List_Item(Item).isItem and (FResourceData.GetPathField(CurrentObjectDataPath, Field_Pos_)) then
    begin
      if FResourceData.ItemOpen(CurrentObjectDataPath, Item.Caption, ItemHnd) then
        begin
          DoStatus(Format('Rename Item "%s" to "%s" .', [Item.Caption, s]));
          if not FResourceData.ItemReName(Field_Pos_, ItemHnd, s, '') then
              Item.Free;
        end;
    end;
  IsModify := True;
  SysProgress.PostM1(Refresh_All);
end;

procedure TObjectDataManagerFrame.ListViewEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
begin
  AllowEdit := True;
end;

procedure TObjectDataManagerFrame.ListViewKeyUp(Sender: TObject; var key: Word; Shift: TShiftState);
begin
  if (Sender as TListView).IsEditing then
      Exit;
  case key of
    VK_DELETE:
      ActionRemoveExecute(ActionRemove);
    VK_F5:
      FResourceTreeFrame.RefreshList;
    VK_F2:
      ActionRenameExecute(ActionRename);
  end;
end;

procedure TObjectDataManagerFrame.SetResourceData(Value: TObjectDataManager);
begin
  ListView.Items.BeginUpdate;
  ListView.Items.Clear;
  ListView.Items.EndUpdate;

  FResourceData := Value;
  FResourceTreeFrame.ObjectDataEngine := Value;
  FResourceTreeFrame.CurrentObjectDataPath := '/';
end;

function TObjectDataManagerFrame.GetCurrentObjectDataPath: string;
begin
  Result := FResourceTreeFrame.CurrentObjectDataPath;
end;

procedure TObjectDataManagerFrame.SetCurrentObjectDataPath(const Value: string);
begin
  FResourceTreeFrame.CurrentObjectDataPath := Value;
end;

procedure TObjectDataManagerFrame.OpenObjectDataPath(Path_: string);
begin
  UpdateItemList(Path_);
end;

procedure TObjectDataManagerFrame.SetFileFilter(const Value: string);
begin
  FFileFilter := Value;
  FResourceTreeFrame.RefreshList;
end;

function TObjectDataManagerFrame.GetMultiSelect: Boolean;
begin
  Result := ListView.MultiSelect;
end;

procedure TObjectDataManagerFrame.SetMultiSelect(const Value: Boolean);
begin
  ListView.MultiSelect := Value;
end;

constructor TObjectDataManagerFrame.Create(AOwner: TComponent);
begin
  inherited;
  FResourceData := nil;

  FResourceTreeFrame := TObjectDataTreeFrame.Create(nil);
  FResourceTreeFrame.Parent := TreePanel;
  FResourceTreeFrame.Align := alClient;
  FResourceTreeFrame.OnOpenObjectDataPath := OpenObjectDataPath;
  FResourceTreeFrame.CurrentObjectDataPath := '/';
  FResourceTreeFrame.ObjectDataEngine := nil;

  FIsModify := False;

  FFileFilter := '*';
  MultiSelect := True;
end;

destructor TObjectDataManagerFrame.Destroy;
begin
  FResourceTreeFrame.Free;
  inherited;
end;

procedure TObjectDataManagerFrame.UpdateItemList(Path_: string);
var
  ItmSR: TItemSearch;
  FieldSR: TFieldSearch;
  Filter: TArrayPascalString;
  itm: TZDB_List_Item;
begin
  umlGetSplitArray(FFileFilter, Filter, '|;');
  ListView.Items.BeginUpdate;
  ListView.Items.Clear;
  CurrentObjectDataPath := Path_;
  if FResourceData <> nil then
    begin
      if FResourceData.FieldFindFirst(Path_, '*', FieldSR) then
        begin
          repeat
            itm := ListView.Items.Add as TZDB_List_Item;
            itm.isField := True;
            itm.Caption := FieldSR.Name;
            itm.SubItems.Add('Field');
            itm.SubItems.Add('Files : ' + umlIntToStr(FieldSR.HeaderCount));
          until not FResourceData.FieldFindNext(FieldSR);
        end;

      if FResourceData.ItemFindFirst(Path_, '*', ItmSR) then
        begin
          repeat
            if umlMultipleMatch(Filter, ItmSR.Name) then
              begin
                itm := ListView.Items.Add as TZDB_List_Item;
                itm.isItem := True;
                itm.Caption := ItmSR.Name;
                itm.SubItems.Add(IntToHex(ItmSR.FieldSearch.RHeader.UserProperty, 8));
                itm.SubItems.Add(umlSizeToStr(ItmSR.Size));
                itm.SubItems.Add(DateTimeToStr(ItmSR.FieldSearch.RHeader.CreateTime));
                itm.SubItems.Add(DateTimeToStr(ItmSR.FieldSearch.RHeader.ModificationTime));
              end;
          until not FResourceData.ItemFindNext(ItmSR);
        end;
    end;
  ListView.Items.EndUpdate;
end;

procedure TObjectDataManagerFrame.Refresh_All;
begin
  FResourceTreeFrame.RefreshList;
  UpdateItemList(CurrentObjectDataPath);
end;

procedure TObjectDataManagerFrame.ExportDBPathToPath(DBPath_, destDir_: string);
begin
  if FResourceData <> nil then
      FResourceData.ExpPathToDisk(DBPath_, destDir_, True);
end;

procedure TObjectDataManagerFrame.ExportToFile(DBPath_, DBItem_, destDir_, destFileName_: string; var showMsg: Boolean);
var
  ItemHnd: TItemHandle;
  s: TItemStream;
  fs: TFileStream;
begin
  if FResourceData <> nil then
    begin
      if not umlDirectoryExists(destDir_) then
          umlCreateDirectory(destDir_);

      if (showMsg) and (umlFileExists(umlCombineFileName(destDir_, destFileName_))) then
        begin
          case MessageDlg(Format('File "%s" alread exists, overwirte?', [ExtractFilename(destFileName_)]), mtInformation, [mbYes, mbNo, mbAll], 0) of
            mrNo:
              Exit;
            mrAll:
              showMsg := False;
          end;
        end;
      if FResourceData.ItemOpen(DBPath_, DBItem_, ItemHnd) then
        begin
          s := TItemStream.Create(FResourceData, ItemHnd);
          fs := TFileStream.Create(umlCombineFileName(destDir_, destFileName_), fmCreate);
          fs.CopyFrom(s, s.Size);
          fs.Free;
          s.Free;
          umlSetFileTime(umlCombineFileName(destDir_, destFileName_), ItemHnd.Item.RHeader.CreateTime);
          DoStatus('export file:%s', [umlCombineFileName(destDir_, destFileName_).Text]);
        end;
    end;
end;

procedure TObjectDataManagerFrame.ImportFromFile(FileName_: string; var showMsg: Boolean);
var
  ItemHnd: TItemHandle;
  fs: TFileStream;
  longName: Boolean;
begin
  if FResourceData <> nil then
    begin
      if (showMsg) and (FResourceData.ItemExists(CurrentObjectDataPath, ExtractFilename(FileName_))) then
        begin
          case MessageDlg(Format('Item "%s" alread exists, overwirte?', [ExtractFilename(FileName_)]), mtInformation, [mbYes, mbNo, mbAll], 0) of
            mrNo:
              Exit;
            mrAll:
              showMsg := False;
          end;
        end;

      longName := FResourceData.Handle^.IOHnd.CheckFixedStringLoss(umlGetFileName(FileName_));

      if longName then
          MessageDlg(Format('File name %s is too long, which causes character loss!', [umlGetFileName(FileName_).Text]), mtWarning, [mbOk], 0);

      fs := TFileStream.Create(FileName_, fmOpenRead);
      FResourceData.ItemCreate(CurrentObjectDataPath, ExtractFilename(FileName_), '', ItemHnd);
      ItemHnd.CreateTime := umlGetFileTime(FileName_);
      ItemHnd.ModificationTime := ItemHnd.CreateTime;
      try
        if FResourceData.ItemWriteFromStream(ItemHnd, fs) then
            DoStatus('import file:%s', [ExtractFilename(FileName_)]);
      finally
          fs.Free;
      end;
      FResourceData.ItemClose(ItemHnd);
    end;
end;

initialization

finalization

end.
