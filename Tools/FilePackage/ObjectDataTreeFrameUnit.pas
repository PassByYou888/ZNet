unit ObjectDataTreeFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.ZDB;

type
  TOpenObjectDataPath = procedure(Path__: string) of object;

  TObjectDataTreeFrame = class(TFrame)
    TreeView: TTreeView;
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewKeyUp(Sender: TObject; var key: Word; Shift: TShiftState);
  private
    DefaultFolderImageIndex: Integer;
    FCurrentObjectDataPath: string;
    FObjectDataEngine: TObjectDataManager;
    FOnOpenObjectDataPath: TOpenObjectDataPath;

    function GetObjectDataEngine: TObjectDataManager;
    procedure SetObjectDataEngine(const Value: TObjectDataManager);
    procedure SetCurrentObjectDataPath(const Value: string);

    class function GetPathTreeNode(Text_, Path_Split_: U_String; TreeView_: TTreeView; RootNode_: TTreeNode): TTreeNode;
    function GetNodeObjDataPath(DestNode_: TTreeNode; Split_: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateFieldList(Owner_Node_: TTreeNode; Path__: string);
    procedure RefreshList;
    property CurrentObjectDataPath: string read FCurrentObjectDataPath write SetCurrentObjectDataPath;

    property ObjectDataEngine: TObjectDataManager read GetObjectDataEngine write SetObjectDataEngine;
    property OnOpenObjectDataPath: TOpenObjectDataPath read FOnOpenObjectDataPath write FOnOpenObjectDataPath;
  end;

implementation

{$R *.dfm}


procedure TObjectDataTreeFrame.TreeViewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
begin
  AllowExpansion := True;
  if Node.Count = 0 then
      UpdateFieldList(Node, GetNodeObjDataPath(Node, '/'));
end;

procedure TObjectDataTreeFrame.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if (Node.selected) and (not(Sender as TTreeView).IsEditing) then
    begin
      FCurrentObjectDataPath := GetNodeObjDataPath(Node, '/');
      if FCurrentObjectDataPath = '' then
          FCurrentObjectDataPath := '/';
      if Assigned(FOnOpenObjectDataPath) then
          FOnOpenObjectDataPath(FCurrentObjectDataPath);
    end;
end;

function TObjectDataTreeFrame.GetObjectDataEngine: TObjectDataManager;
begin
  Result := FObjectDataEngine;
end;

procedure TObjectDataTreeFrame.SetObjectDataEngine(const Value: TObjectDataManager);
var
  RootNode_: TTreeNode;
begin
  if FObjectDataEngine <> Value then
    begin
      FObjectDataEngine := Value;
    end;
  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;
  if FObjectDataEngine <> nil then
    begin
      RootNode_ := TreeView.Items.AddFirst(nil, 'Root');
      with RootNode_ do
        begin
          ImageIndex := DefaultFolderImageIndex;
          selectedIndex := DefaultFolderImageIndex;
          StateIndex := DefaultFolderImageIndex;
          selected := True;
          Data := nil;
        end;
      UpdateFieldList(RootNode_, '/');
    end;
  TreeView.Items.EndUpdate;
end;

procedure TObjectDataTreeFrame.SetCurrentObjectDataPath(const Value: string);
begin
  FCurrentObjectDataPath := Value;
  if ObjectDataEngine <> nil then
    begin
      if ObjectDataEngine.DirectoryExists(FCurrentObjectDataPath) then
        with GetPathTreeNode('/Root/' + Value, '/', TreeView, nil) do
            selected := True;
    end;
end;

class function TObjectDataTreeFrame.GetPathTreeNode(Text_, Path_Split_: U_String; TreeView_: TTreeView; RootNode_: TTreeNode): TTreeNode;
const
  Key_Value_Split: SystemChar = #0;
var
  i: Integer;
  prefix_, match_, value_: U_String;
begin
  prefix_ := umlGetFirstStr(Text_, Path_Split_);
  if prefix_.Exists(Key_Value_Split) then
    begin
      match_ := umlGetFirstStr(prefix_, Key_Value_Split);
      value_ := umlDeleteFirstStr(prefix_, Key_Value_Split);
    end
  else
    begin
      match_ := prefix_;
      value_ := prefix_;
    end;

  if prefix_ = '' then
      Result := RootNode_
  else if RootNode_ = nil then
    begin
      if TreeView_.Items.Count > 0 then
        begin
          for i := 0 to TreeView_.Items.Count - 1 do
            begin
              if (TreeView_.Items[i].Parent = RootNode_) and umlMultipleMatch(True, match_, TreeView_.Items[i].Text) then
                begin
                  TreeView_.Items[i].Text := value_;
                  Result := GetPathTreeNode(umlDeleteFirstStr(Text_, Path_Split_), Path_Split_, TreeView_, TreeView_.Items[i]);
                  exit;
                end;
            end;
        end;
      Result := TreeView_.Items.AddChild(RootNode_, value_);
      with Result do
        begin
          ImageIndex := -1;
          StateIndex := -1;
          SelectedIndex := -1;
          Data := nil;
        end;
      Result := GetPathTreeNode(umlDeleteFirstStr(Text_, Path_Split_), Path_Split_, TreeView_, Result);
    end
  else
    begin
      if (RootNode_.Count > 0) then
        begin
          for i := 0 to RootNode_.Count - 1 do
            begin
              if (RootNode_.Item[i].Parent = RootNode_) and umlMultipleMatch(True, match_, RootNode_.Item[i].Text) then
                begin
                  RootNode_.Item[i].Text := value_;
                  Result := GetPathTreeNode(umlDeleteFirstStr(Text_, Path_Split_), Path_Split_, TreeView_, RootNode_.Item[i]);
                  exit;
                end;
            end;
        end;
      Result := TreeView_.Items.AddChild(RootNode_, value_);
      with Result do
        begin
          ImageIndex := -1;
          StateIndex := -1;
          SelectedIndex := -1;
          Data := nil;
        end;
      Result := GetPathTreeNode(umlDeleteFirstStr(Text_, Path_Split_), Path_Split_, TreeView_, Result);
    end;
end;

function TObjectDataTreeFrame.GetNodeObjDataPath(DestNode_: TTreeNode; Split_: string): string;
begin
  if DestNode_.level > 0 then
      Result := GetNodeObjDataPath(DestNode_.Parent, Split_) + Split_ + DestNode_.Text
  else
      Result := '';
end;

constructor TObjectDataTreeFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefaultFolderImageIndex := -1;
  FObjectDataEngine := nil;
end;

destructor TObjectDataTreeFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TObjectDataTreeFrame.UpdateFieldList(Owner_Node_: TTreeNode; Path__: string);
var
  FieldSR_: TFieldSearch;
  nd: TTreeNode;
begin
  if ObjectDataEngine <> nil then
    begin
      if ObjectDataEngine.FieldFindFirst(Path__, '*', FieldSR_) then
        begin
          repeat
            nd := TreeView.Items.AddChild(Owner_Node_, FieldSR_.Name);
            with nd do
              begin
                HasChildren := ObjectDataEngine.FastFieldExists(FieldSR_.HeaderPOS, '*');
                ImageIndex := DefaultFolderImageIndex;
                selectedIndex := DefaultFolderImageIndex;
                StateIndex := DefaultFolderImageIndex;
                Data := nil;
              end;
            if nd.HasChildren then
                UpdateFieldList(nd, ObjectDataEngine.GetFieldPath(FieldSR_.HeaderPOS));
          until not ObjectDataEngine.FieldFindNext(FieldSR_);
        end;
    end;
end;

procedure TObjectDataTreeFrame.RefreshList;
var
  N_: string;
begin
  N_ := CurrentObjectDataPath;
  SetObjectDataEngine(ObjectDataEngine);
  SetCurrentObjectDataPath(N_);
end;

procedure TObjectDataTreeFrame.TreeViewKeyUp(Sender: TObject; var key: Word; Shift: TShiftState);
begin
  case key of
    VK_F5:
      RefreshList;
  end;
end;

end.
