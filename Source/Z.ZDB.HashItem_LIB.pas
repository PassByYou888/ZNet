{ ****************************************************************************** }
{ * fast StreamQuery                                                           * }
{ ****************************************************************************** }
unit Z.ZDB.HashItem_LIB;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses SysUtils, Z.ZDB, Z.ZDB.ItemStream_LIB, Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.ListEngine;

type
  TObjectDataHashItem = class;

  THashItemData = record
    qHash: THash;
    LowerCaseName, OriginName: SystemString;
    stream: TItemStream;
    ItemHnd: TItemHandle;
    CallCount: Integer;
    ForceFreeCustomObject: Boolean;
    CustomObject: TCore_Object;
    ID: Integer;
    Owner: TObjectDataHashItem;
  end;

  PHashItemData = ^THashItemData;

  TObjectDataHashItem = class(TCore_Object_Intermediate)
  protected
    FCounter: Boolean;
    FCount: Integer;
    FName: SystemString;
    FDescription: SystemString;
    FDBEngine: TObjectDataManager;
    FFieldPos: Int64;
    FAryList: array of TCore_List;
    FData: Pointer;

    function GetListTable(hash: THash; AutoCreate: Boolean): TCore_List;
    procedure RefreshDBLst(DBEngine_: TObjectDataManager; var FieldPos_: Int64);
    procedure SetHashBlockCount(cnt: Integer);
    function GetNames(Name_: SystemString): PHashItemData;
  public
    constructor Create(DBEngine_: TObjectDataManager; FieldPos_: Int64);
    destructor Destroy; override;
    procedure Clear;
    procedure Refresh;
    procedure GetOriginNameListFromFilter(Filter_: SystemString; Output_: TCore_Strings);
    procedure GetListFromFilter(Filter_: SystemString; Output_: TCore_List);
    procedure GetOriginNameList(Output_: TCore_Strings); overload;
    procedure GetOriginNameList(Output_: TListString); overload;
    procedure GetList(Output_: TCore_List);
    function Find(Name_: SystemString): PHashItemData;
    function Exists(Name_: SystemString): Boolean;

    property Names[Name_: SystemString]: PHashItemData read GetNames; default;

    property DBEngine: TObjectDataManager read FDBEngine;
    property FieldPos: Int64 read FFieldPos;
    property Name: SystemString read FName write FName;
    property Description: SystemString read FDescription write FDescription;
    property Counter: Boolean read FCounter write FCounter;
    property Count: Integer read FCount;
    property Data: Pointer read FData write FData;
  end;

implementation

uses Z.UnicodeMixedLib;

function TObjectDataHashItem.GetListTable(hash: THash; AutoCreate: Boolean): TCore_List;
var
  idx: Integer;
begin
  idx := hashMod(hash, length(FAryList));

  if (AutoCreate) and (FAryList[idx] = nil) then
      FAryList[idx] := TCore_List.Create;
  Result := FAryList[idx];
end;

procedure TObjectDataHashItem.RefreshDBLst(DBEngine_: TObjectDataManager; var FieldPos_: Int64);
var
  ItemSearchHnd: TItemSearch;
  ICnt: Integer;

  procedure AddLstItem(_ItemPos: Int64);
  var
    newhash: THash;
    p: PHashItemData;
    idxLst: TCore_List;
    lName: SystemString;
    ItemHnd: TItemHandle;
  begin
    if FDBEngine.ItemFastOpen(_ItemPos, ItemHnd) then
      if umlGetLength(ItemHnd.Name) > 0 then
        begin
          lName := ItemHnd.Name.LowerText;
          newhash := MakeHashS(lName);
          idxLst := GetListTable(newhash, True);
          new(p);
          p^.qHash := newhash;
          p^.LowerCaseName := lName;
          p^.OriginName := ItemHnd.Name;
          p^.stream := nil;
          p^.ItemHnd := ItemHnd;
          p^.CallCount := 0;
          p^.ForceFreeCustomObject := False;
          p^.CustomObject := nil;
          p^.ID := ICnt;
          p^.Owner := Self;
          idxLst.Add(p);
          inc(ICnt);
        end;
  end;

begin
  FDBEngine := DBEngine_;
  FFieldPos := FieldPos_;
  FCount := 0;
  ICnt := 0;
  if FDBEngine.ItemFastFindFirst(FFieldPos, '*', ItemSearchHnd) then
    begin
      repeat
        inc(FCount);
        AddLstItem(ItemSearchHnd.HeaderPOS);
      until not FDBEngine.ItemFastFindNext(ItemSearchHnd);
    end;
end;

procedure TObjectDataHashItem.SetHashBlockCount(cnt: Integer);
var
  i: Integer;
begin
  Clear;
  SetLength(FAryList, cnt);
  for i := low(FAryList) to high(FAryList) do
      FAryList[i] := nil;
end;

constructor TObjectDataHashItem.Create(DBEngine_: TObjectDataManager; FieldPos_: Int64);
begin
  inherited Create;
  FCounter := True;
  FCount := 0;
  FData := nil;
  SetLength(FAryList, 0);
  SetHashBlockCount(10000);
  RefreshDBLst(DBEngine_, FieldPos_);
end;

destructor TObjectDataHashItem.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TObjectDataHashItem.Clear;
var
  i: Integer;
  j: Integer;
begin
  FCount := 0;
  if length(FAryList) = 0 then
      Exit;

  for i := low(FAryList) to high(FAryList) do
    begin
      if FAryList[i] <> nil then
        begin
          with FAryList[i] do
            begin
              if Count > 0 then
                begin
                  for j := 0 to Count - 1 do
                    begin
                      with PHashItemData(Items[j])^ do
                        begin
                          if stream <> nil then
                            begin
                              DisposeObject(stream);
                              if (ForceFreeCustomObject) and (CustomObject <> nil) then
                                begin
                                  try
                                    DisposeObject(CustomObject);
                                    CustomObject := nil;
                                  except
                                  end;
                                end;
                            end;
                        end;
                      try
                          Dispose(PHashItemData(Items[j]));
                      except
                      end;
                    end;
                end;
            end;

          DisposeObject(FAryList[i]);
          FAryList[i] := nil;
        end;
    end;
end;

procedure TObjectDataHashItem.Refresh;
begin
  Clear;
  RefreshDBLst(FDBEngine, FFieldPos);
end;

procedure TObjectDataHashItem.GetOriginNameListFromFilter(Filter_: SystemString; Output_: TCore_Strings);
var
  i: Integer;
  L: TCore_List;
  p: PHashItemData;
begin
  L := TCore_List.Create;
  GetList(L);

  Output_.Clear;
  if L.Count > 0 then
    for i := 0 to L.Count - 1 do
      begin
        p := PHashItemData(L[i]);
        if umlMultipleMatch(Filter_, p^.OriginName) then
            Output_.Add(p^.OriginName);
      end;

  DisposeObject(L);
end;

procedure TObjectDataHashItem.GetListFromFilter(Filter_: SystemString; Output_: TCore_List);
var
  i: Integer;
  L: TCore_List;
  p: PHashItemData;
begin
  L := TCore_List.Create;
  GetList(L);

  Output_.Clear;
  if L.Count > 0 then
    for i := 0 to L.Count - 1 do
      begin
        p := PHashItemData(L[i]);
        if umlMultipleMatch(Filter_, p^.OriginName) then
            Output_.Add(p);
      end;

  DisposeObject(L);
end;

procedure TObjectDataHashItem.GetOriginNameList(Output_: TCore_Strings);
var
  i: Integer;
  L: TCore_List;
begin
  L := TCore_List.Create;
  GetList(L);

  Output_.Clear;
  if L.Count > 0 then
    for i := 0 to L.Count - 1 do
        Output_.Add(PHashItemData(L[i])^.OriginName);

  DisposeObject(L);
end;

procedure TObjectDataHashItem.GetOriginNameList(Output_: TListString);
var
  i: Integer;
  L: TCore_List;
begin
  L := TCore_List.Create;
  GetList(L);

  Output_.Clear;
  if L.Count > 0 then
    for i := 0 to L.Count - 1 do
        Output_.Add(PHashItemData(L[i])^.OriginName);

  DisposeObject(L);
end;

procedure TObjectDataHashItem.GetList(Output_: TCore_List);
  function ListSortCompare(Item1, Item2: Pointer): Integer;
    function Compare_(const a, b: Int64): Integer;
    begin
      if a = b then
          Result := 0
      else if a < b then
          Result := -1
      else
          Result := 1;
    end;

  begin
    Result := Compare_(PHashItemData(Item1)^.ID, PHashItemData(Item2)^.ID);
  end;

  procedure QuickSortList(var SortList: TCore_PointerList; L, r: Integer);
  var
    i, j: Integer;
    p, t: Pointer;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while ListSortCompare(SortList[i], p) < 0 do
            inc(i);
        while ListSortCompare(SortList[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

var
  i, j: Integer;
begin
  Output_.Clear;

  if Count > 0 then
    begin
      Output_.Capacity := Count;

      for i := low(FAryList) to high(FAryList) do
        begin
          if FAryList[i] <> nil then
            begin
              with FAryList[i] do
                if Count > 0 then
                  begin
                    for j := 0 to Count - 1 do
                      with PHashItemData(Items[j])^ do
                        begin
                          if stream = nil then
                              stream := TItemStream.Create(FDBEngine, ItemHnd)
                          else
                              stream.SeekStart;
                          if FCounter then
                              inc(CallCount);
                          Output_.Add(Items[j]);
                        end;
                  end;
            end;
        end;

      if Output_.Count > 1 then
          QuickSortList(Output_.ListData^, 0, Output_.Count - 1);
    end;
end;

function TObjectDataHashItem.Find(Name_: SystemString): PHashItemData;
var
  i, j: Integer;
begin
  Result := nil;
  for i := low(FAryList) to high(FAryList) do
    begin
      if FAryList[i] <> nil then
        begin
          with FAryList[i] do
            if Count > 0 then
              begin
                for j := 0 to Count - 1 do
                  begin
                    if umlMultipleMatch(True, Name_, PHashItemData(Items[j])^.OriginName) then
                      begin
                        Result := Items[j];
                        if Result^.stream = nil then
                            Result^.stream := TItemStream.Create(FDBEngine, Result^.ItemHnd)
                        else
                            Result^.stream.SeekStart;
                        if FCounter then
                            inc(Result^.CallCount);
                        Exit;
                      end;
                  end;
              end;
        end;
    end;
end;

function TObjectDataHashItem.Exists(Name_: SystemString): Boolean;
var
  newhash: THash;
  i: Integer;
  idxLst: TCore_List;
  lName: SystemString;
begin
  Result := False;
  if umlGetLength(Name_) > 0 then
    begin
      lName := LowerCase(Name_);
      newhash := MakeHashS(lName);
      idxLst := GetListTable(newhash, False);
      if idxLst <> nil then
        if idxLst.Count > 0 then
          for i := 0 to idxLst.Count - 1 do
            if (newhash = PHashItemData(idxLst[i])^.qHash) and (PHashItemData(idxLst[i])^.LowerCaseName = lName) then
                Exit(True);
    end;
end;

function TObjectDataHashItem.GetNames(Name_: SystemString): PHashItemData;
var
  newhash: THash;
  i: Integer;
  idxLst: TCore_List;
  lName: SystemString;
begin
  Result := nil;
  if umlGetLength(Name_) > 0 then
    begin
      lName := LowerCase(Name_);
      newhash := MakeHashS(lName);
      idxLst := GetListTable(newhash, False);
      if idxLst <> nil then
        if idxLst.Count > 0 then
          for i := 0 to idxLst.Count - 1 do
            begin
              if (newhash = PHashItemData(idxLst[i])^.qHash) and (PHashItemData(idxLst[i])^.LowerCaseName = lName) then
                begin
                  Result := idxLst[i];
                  if Result^.stream = nil then
                      Result^.stream := TItemStream.Create(FDBEngine, Result^.ItemHnd)
                  else
                      Result^.stream.SeekStart;
                  if FCounter then
                      inc(Result^.CallCount);
                  Exit;
                end;
            end;
    end;
end;

end.
