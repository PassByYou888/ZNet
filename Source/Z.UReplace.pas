{ ****************************************************************************** }
{ * Unicode Replace support (delphi/FPC)                                       * }
{ ****************************************************************************** }
unit Z.UReplace;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses
{$IFDEF FPC}
  Z.FPC.GenericList,
{$ENDIF FPC}
  Z.Core, Z.UPascalStrings, Z.ListEngine;

type
  TU_Batch = record
  private
    procedure Swap_(var inst: TU_Batch);
  public
    sour, dest: TUPascalString;
    sum: Integer;
  end;

  PU_Batch = ^TU_Batch;

  TU_ArrayBatch = array of TU_Batch;

  TU_BatchInfo = record
    Batch: Integer;
    sour_bPos, sour_ePos: Integer;
    dest_bPos, dest_ePos: Integer;
  end;

  TU_BatchInfoList = TGenericsList<TU_BatchInfo>;

{$IFDEF FPC}
  TOnUBatchProc = procedure(bPos, ePos: Integer; sour, dest: PUPascalString; var Accept: Boolean) is nested;
{$ELSE FPC}
  TOnUBatchProc = reference to procedure(bPos, ePos: Integer; sour, dest: PUPascalString; var Accept: Boolean);
{$ENDIF FPC}

function U_BuildBatch(L: THashStringList): TU_ArrayBatch; overload;
function U_BuildBatch(L: THashVariantList): TU_ArrayBatch; overload;
procedure U_ClearBatch(var arry: TU_ArrayBatch);
procedure U_SortBatch(var arry: TU_ArrayBatch); overload;
function U_CharIsSymbol(c: USystemChar): Boolean; overload;
function U_CharIsSymbol(c: USystemChar; const CustomSymbol_: TUArrayChar): Boolean; overload;
function U_IsWord(p: PUPascalString; bPos, ePos: Integer): Boolean; overload;
function U_IsWord(s: TUPascalString; bPos, ePos: Integer): Boolean; overload;
function U_ExtractWord(s: TUPascalString): TUArrayPascalString; overload;
function U_ExtractWord(s: TUPascalString; const CustomSymbol_: TUArrayChar): TUArrayPascalString; overload;
function U_BatchSum(p: PUPascalString; var arry: TU_ArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList): Integer; overload;
function U_BatchSum(s: TUPascalString; var arry: TU_ArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList): Integer; overload;
function U_BatchSum(s: TUPascalString; var arry: TU_ArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): Integer; overload;
function U_BatchReplace(p: PUPascalString; var arry: TU_ArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList; On_P: TOnUBatchProc): TUPascalString; overload;
function U_BatchReplace(s: TUPascalString; var arry: TU_ArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList; On_P: TOnUBatchProc): TUPascalString; overload;
function U_BatchReplace(s: TUPascalString; var arry: TU_ArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList): TUPascalString; overload;
function U_BatchReplace(s: TUPascalString; var arry: TU_ArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): TUPascalString; overload;
function U_ReplaceSum(p: PUPascalString; Pattern: TUPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList): Integer; overload;
function U_ReplaceSum(s, Pattern: TUPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList): Integer; overload;
function U_Replace(p: PUPascalString; OldPattern, NewPattern: TUPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList; On_P: TOnUBatchProc): TUPascalString; overload;
function U_Replace(s, OldPattern, NewPattern: TUPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList; On_P: TOnUBatchProc): TUPascalString; overload;
function U_Replace(s, OldPattern, NewPattern: TUPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList): TUPascalString; overload;
function U_Replace(p: PUPascalString; OldPattern, NewPattern: TUPascalString; OnlyWord, IgnoreCase: Boolean): TUPascalString; overload;
function U_Replace(s, OldPattern, NewPattern: TUPascalString; OnlyWord, IgnoreCase: Boolean): TUPascalString; overload;
function U_ComputeTextPoint(p: PUPascalString; Pos_: Integer): TPoint;

implementation

uses Z.MemoryStream, Variants;

procedure TU_Batch.Swap_(var inst: TU_Batch);
begin
  sour.SwapInstance(inst.sour);
  dest.SwapInstance(inst.dest);
  TSwap<Integer>.Do_(sum, inst.sum);
end;

function U_BuildBatch(L: THashStringList): TU_ArrayBatch;
var
  arry: TU_ArrayBatch;
  i: Integer;
  p: PHashListData;
begin
  SetLength(arry, L.Count);
  if L.HashList.Count > 0 then
    begin
      i := 0;
      p := L.HashList.FirstPtr;
      while i < L.HashList.Count do
        begin
          arry[i].sour := p^.OriginName;
          arry[i].dest := PHashStringListData(p^.Data)^.v;
          inc(i);
          p := p^.Next;
        end;
    end;
  Result := arry;
end;

function U_BuildBatch(L: THashVariantList): TU_ArrayBatch;
var
  arry: TU_ArrayBatch;
  i: Integer;
  p: PHashListData;
begin
  SetLength(arry, L.Count);
  if L.HashList.Count > 0 then
    begin
      i := 0;
      p := L.HashList.FirstPtr;
      while i < L.HashList.Count do
        begin
          arry[i].sour := p^.OriginName;
          arry[i].dest := VarToStr(PHashVariantListData(p^.Data)^.v);
          inc(i);
          p := p^.Next;
        end;
    end;
  Result := arry;
end;

procedure U_ClearBatch(var arry: TU_ArrayBatch);
var
  i: Integer;
begin
  for i := low(arry) to high(arry) do
    begin
      arry[i].sour := '';
      arry[i].dest := '';
    end;
  SetLength(arry, 0);
end;

procedure U_SortBatch(var arry: TU_ArrayBatch);

  function CompareInt_(const i1, i2: Integer): ShortInt;
  begin
    if i1 = i2 then
        Result := 0
    else if i1 < i2 then
        Result := -1
    else
        Result := 1;
  end;

  function Compare_(var Left, Right: TU_Batch): ShortInt;
  begin
    Result := CompareInt_(Right.sour.L, Left.sour.L);
  end;

  procedure fastSort_(L, r: Integer);
  var
    i, j: Integer;
    p: TU_Batch;
  begin
    if L < r then
      begin
        repeat
          if (r - L) = 1 then
            begin
              if Compare_(arry[L], arry[r]) > 0 then
                  arry[L].Swap_(arry[r]);
              break;
            end;
          i := L;
          j := r;
          p := arry[(L + r) shr 1];
          repeat
            while Compare_(arry[i], p) < 0 do
                inc(i);
            while Compare_(arry[j], p) > 0 do
                dec(j);
            if i <= j then
              begin
                if i <> j then
                  begin
                    arry[i].Swap_(arry[j]);
                  end;
                inc(i);
                dec(j);
              end;
          until i > j;
          if (j - L) > (r - i) then
            begin
              if i < r then
                  fastSort_(i, r);
              r := j;
            end
          else
            begin
              if L < j then
                  fastSort_(L, j);
              L := i;
            end;
        until L >= r;
      end;
  end;

begin
  if length(arry) > 1 then
      fastSort_(0, length(arry) - 1);
end;

function U_CharIsSymbol(c: USystemChar): Boolean;
begin
  Result := UCharIn(c,
    [#13, #10, #9, #32, #46, #44, #43, #45, #42, #47, #40, #41, #59, #58, #61, #35, #64, #94,
      #38, #37, #33, #34, #91, #93, #60, #62, #63, #123, #125, #39, #36, #124]);
end;

function U_CharIsSymbol(c: USystemChar; const CustomSymbol_: TUArrayChar): Boolean;
begin
  Result := UCharIn(c, CustomSymbol_);
end;

function U_IsWord(p: PUPascalString; bPos, ePos: Integer): Boolean;
begin
  if (bPos > ePos) or (bPos < 1) or (ePos > p^.L) then
      Result := False
  else if bPos = 1 then
    begin
      if ePos = p^.L then
          Result := True
      else
          Result := U_CharIsSymbol(p^[ePos + 1]);
    end
  else if ePos = p^.L then
      Result := U_CharIsSymbol(p^[bPos - 1])
  else
      Result := U_CharIsSymbol(p^[bPos - 1]) and U_CharIsSymbol(p^[ePos + 1]);
end;

function U_IsWord(s: TUPascalString; bPos, ePos: Integer): Boolean;
begin
  Result := U_IsWord(@s, bPos, ePos);
end;

function U_ExtractWord(s: TUPascalString): TUArrayPascalString;
var
  i, bPos, ePos, j: Integer;
begin
  SetLength(Result, 0);
  if s.L = 0 then
      exit;

  // compute buff size
  j := 0;
  i := 1;
  while i <= s.L do
    begin
      bPos := i;
      while bPos <= s.L do
        if U_CharIsSymbol(s[bPos]) then
            inc(bPos)
        else
            break;

      ePos := bPos;
      while ePos <= s.L do
        if not U_CharIsSymbol(s[ePos]) then
            inc(ePos)
        else
            break;

      if ePos > bPos then
          inc(j);
      i := ePos;
    end;

  if j = 0 then
      exit;

  // fill buff
  SetLength(Result, j);
  j := 0;
  i := 1;
  while i <= s.L do
    begin
      bPos := i;
      while bPos <= s.L do
        if U_CharIsSymbol(s[bPos]) then
            inc(bPos)
        else
            break;

      ePos := bPos;
      while ePos <= s.L do
        if not U_CharIsSymbol(s[ePos]) then
            inc(ePos)
        else
            break;

      if ePos > bPos then
        begin
          Result[j] := s.GetString(bPos, ePos);
          inc(j);
        end;
      i := ePos;
    end;
end;

function U_ExtractWord(s: TUPascalString; const CustomSymbol_: TUArrayChar): TUArrayPascalString;
var
  i, bPos, ePos, j: Integer;
begin
  SetLength(Result, 0);
  if s.L = 0 then
      exit;

  // compute buff size
  j := 0;
  i := 1;
  while i <= s.L do
    begin
      bPos := i;
      while bPos <= s.L do
        if U_CharIsSymbol(s[bPos], CustomSymbol_) then
            inc(bPos)
        else
            break;

      ePos := bPos;
      while ePos <= s.L do
        if not U_CharIsSymbol(s[ePos], CustomSymbol_) then
            inc(ePos)
        else
            break;

      if ePos > bPos then
          inc(j);
      i := ePos;
    end;

  if j = 0 then
      exit;

  // fill buff
  SetLength(Result, j);
  j := 0;
  i := 1;
  while i <= s.L do
    begin
      bPos := i;
      while bPos <= s.L do
        if U_CharIsSymbol(s[bPos], CustomSymbol_) then
            inc(bPos)
        else
            break;

      ePos := bPos;
      while ePos <= s.L do
        if not U_CharIsSymbol(s[ePos], CustomSymbol_) then
            inc(ePos)
        else
            break;

      if ePos > bPos then
        begin
          Result[j] := s.GetString(bPos, ePos);
          inc(j);
        end;
      i := ePos;
    end;
end;

function U_BatchSum(p: PUPascalString; var arry: TU_ArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList): Integer;
  function Match_(Pos_: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := Low(arry) to high(arry) do
      if (arry[i].sour.L > 0) and ((not OnlyWord) or U_IsWord(p, Pos_, Pos_ + arry[i].sour.L - 1))
        and p^.ComparePos(Pos_, @arry[i].sour, IgnoreCase) then
          exit(i);
  end;

var
  i, r, BP, EP: Integer;
  found_: Boolean;
  BatchInfo: TU_BatchInfo;
begin
  Result := 0;
  if p^.L = 0 then
      exit;

  if (ePos <= 0) or (ePos > p^.L) then
      EP := p^.L
  else
      EP := ePos;

  if bPos < 1 then
      BP := 1
  else if bPos > EP then
      BP := EP
  else
      BP := bPos;

  for i := low(arry) to high(arry) do
      arry[i].sum := 0;

  i := 1;
  while i <= p^.L do
    begin
      found_ := False;
      if (i >= BP) and (i <= EP) then
        begin
          r := Match_(i);
          found_ := r >= 0;
          if found_ then
            begin
              if Info <> nil then
                begin
                  BatchInfo.Batch := r;
                  BatchInfo.sour_bPos := i;
                  BatchInfo.sour_ePos := i + arry[r].sour.L - 1;
                  BatchInfo.dest_bPos := BatchInfo.sour_bPos;
                  BatchInfo.dest_ePos := BatchInfo.sour_ePos;
                  Info.Add(BatchInfo);
                end;
              inc(i, arry[r].sour.L);
              inc(arry[r].sum);
              inc(Result);
            end;
        end;
      if not found_ then
        begin
          inc(i);
        end;
    end;
end;

function U_BatchSum(s: TUPascalString; var arry: TU_ArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList): Integer;
begin
  Result := U_BatchSum(@s, arry, OnlyWord, IgnoreCase, bPos, ePos, Info);
end;

function U_BatchSum(s: TUPascalString; var arry: TU_ArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): Integer;
begin
  Result := U_BatchSum(@s, arry, OnlyWord, IgnoreCase, bPos, ePos, nil);
end;

function U_BatchReplace(p: PUPascalString; var arry: TU_ArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList; On_P: TOnUBatchProc): TUPascalString;
  function Match_(Pos_: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := Low(arry) to high(arry) do
      if (arry[i].sour.L > 0) and ((not OnlyWord) or U_IsWord(p, Pos_, Pos_ + arry[i].sour.L - 1))
        and p^.ComparePos(Pos_, @arry[i].sour, IgnoreCase) then
          exit(i);
  end;

var
  i, r, BP, EP: Integer;
  found_: Boolean;
  m64: TMem64;
  BatchInfo: TU_BatchInfo;
begin
  Result := '';
  if p^.L = 0 then
      exit;
  m64 := TMem64.CustomCreate(p^.L);

  if (ePos <= 0) or (ePos > p^.L) then
      EP := p^.L
  else
      EP := ePos;

  if bPos < 1 then
      BP := 1
  else if bPos > EP then
      BP := EP
  else
      BP := bPos;

  for i := low(arry) to high(arry) do
      arry[i].sum := 0;

  i := 1;
  while i <= p^.L do
    begin
      found_ := False;
      if (i >= BP) and (i <= EP) then
        begin
          r := Match_(i);
          found_ := r >= 0;
          if found_ and Assigned(On_P) then
              On_P(i, i + (arry[r].sour.L - 1), @arry[r].sour, @arry[r].dest, found_);
          if found_ then
            begin
              if Info <> nil then
                begin
                  BatchInfo.Batch := r;
                  BatchInfo.sour_bPos := i;
                  BatchInfo.sour_ePos := i + (arry[r].sour.L - 1);
                  BatchInfo.dest_bPos := m64.Size div USystemCharSize + 1;
                  BatchInfo.dest_ePos := BatchInfo.dest_bPos + (arry[r].dest.L - 1);
                  Info.Add(BatchInfo);
                end;
              if arry[r].dest.L > 0 then
                  m64.Write64(arry[r].dest.buff[0], USystemCharSize * arry[r].dest.L);
              inc(arry[r].sum);
              inc(i, arry[r].sour.L);
            end;
        end;
      if not found_ then
        begin
          m64.Write64(p^.buff[i - 1], USystemCharSize);
          inc(i);
        end;
    end;
  Result.L := m64.Size div USystemCharSize;
  if Result.L > 0 then
      CopyPtr(m64.Memory, @Result.buff[0], m64.Size);
  DisposeObject(m64);
end;

function U_BatchReplace(s: TUPascalString; var arry: TU_ArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList; On_P: TOnUBatchProc): TUPascalString;
begin
  Result := U_BatchReplace(@s, arry, OnlyWord, IgnoreCase, bPos, ePos, Info, On_P);
end;

function U_BatchReplace(s: TUPascalString; var arry: TU_ArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList): TUPascalString;
begin
  Result := U_BatchReplace(@s, arry, OnlyWord, IgnoreCase, bPos, ePos, Info, nil);
end;

function U_BatchReplace(s: TUPascalString; var arry: TU_ArrayBatch; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer): TUPascalString;
begin
  Result := U_BatchReplace(@s, arry, OnlyWord, IgnoreCase, bPos, ePos, nil, nil);
end;

function U_ReplaceSum(p: PUPascalString; Pattern: TUPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList): Integer;
var
  i, BP, EP: Integer;
  found_: Boolean;
  BatchInfo: TU_BatchInfo;
begin
  Result := 0;
  if p^.L = 0 then
      exit;

  if (ePos <= 0) or (ePos > p^.L) then
      EP := p^.L
  else
      EP := ePos;

  if bPos < 1 then
      BP := 1
  else if bPos > EP then
      BP := EP
  else
      BP := bPos;

  i := 1;
  while i <= p^.L do
    begin
      found_ := False;
      if (i >= BP) and (i <= EP) then
        begin
          found_ := ((not OnlyWord) or U_IsWord(p, i, i + Pattern.L - 1)) and p^.ComparePos(i, @Pattern, IgnoreCase);
          if found_ then
            begin
              if Info <> nil then
                begin
                  BatchInfo.Batch := -1;
                  BatchInfo.sour_bPos := i;
                  BatchInfo.sour_ePos := BatchInfo.sour_bPos + (Pattern.L - 1);
                  BatchInfo.dest_bPos := BatchInfo.sour_bPos;
                  BatchInfo.dest_ePos := BatchInfo.sour_ePos;
                  Info.Add(BatchInfo);
                end;
              inc(i, Pattern.L);
              inc(Result);
            end;
        end;
      if not found_ then
        begin
          inc(i);
        end;
    end;
end;

function U_ReplaceSum(s, Pattern: TUPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList): Integer;
begin
  Result := U_ReplaceSum(@s, Pattern, OnlyWord, IgnoreCase, bPos, ePos, Info);
end;

function U_Replace(p: PUPascalString; OldPattern, NewPattern: TUPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList; On_P: TOnUBatchProc): TUPascalString;
var
  i, BP, EP: Integer;
  found_: Boolean;
  m64: TMem64;
  BatchInfo: TU_BatchInfo;
begin
  Result := '';
  if p^.L = 0 then
      exit;
  if OldPattern.L = 0 then
    begin
      Result := p^;
      exit;
    end;
  m64 := TMem64.CustomCreate(p^.L);

  if (ePos <= 0) or (ePos > p^.L) then
      EP := p^.L
  else
      EP := ePos;

  if bPos < 1 then
      BP := 1
  else if bPos > EP then
      BP := EP
  else
      BP := bPos;

  i := 1;
  while i <= p^.L do
    begin
      found_ := False;
      if (i >= BP) and (i <= EP) then
        begin
          found_ := ((not OnlyWord) or U_IsWord(p, i, i + OldPattern.L - 1)) and p^.ComparePos(i, @OldPattern, IgnoreCase);
          if found_ and Assigned(On_P) then
              On_P(i, i + (OldPattern.L - 1), @OldPattern, @NewPattern, found_);
          if found_ then
            begin
              if Info <> nil then
                begin
                  BatchInfo.Batch := -1;
                  BatchInfo.sour_bPos := i;
                  BatchInfo.sour_ePos := i + (OldPattern.L - 1);
                  BatchInfo.dest_bPos := m64.Size div USystemCharSize + 1;
                  BatchInfo.dest_ePos := BatchInfo.dest_bPos + (NewPattern.L - 1);
                  Info.Add(BatchInfo);
                end;
              m64.Write64(NewPattern.buff[0], USystemCharSize * NewPattern.L);
              inc(i, OldPattern.L);
            end;
        end;
      if not found_ then
        begin
          m64.Write64(p^.buff[i - 1], USystemCharSize);
          inc(i);
        end;
    end;
  Result.L := m64.Size div USystemCharSize;
  if Result.L > 0 then
      CopyPtr(m64.Memory, @Result.buff[0], m64.Size);
  DisposeObject(m64);
end;

function U_Replace(s, OldPattern, NewPattern: TUPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList; On_P: TOnUBatchProc): TUPascalString;
begin
  Result := U_Replace(@s, OldPattern, NewPattern, OnlyWord, IgnoreCase, bPos, ePos, Info, On_P);
end;

function U_Replace(s, OldPattern, NewPattern: TUPascalString; OnlyWord, IgnoreCase: Boolean; bPos, ePos: Integer; Info: TU_BatchInfoList): TUPascalString;
begin
  Result := U_Replace(@s, OldPattern, NewPattern, OnlyWord, IgnoreCase, bPos, ePos, Info, nil);
end;

function U_Replace(p: PUPascalString; OldPattern, NewPattern: TUPascalString; OnlyWord, IgnoreCase: Boolean): TUPascalString;
var
  i, r: Integer;
  m64: TMem64;
begin
  Result := '';
  if p^.L = 0 then
      exit;
  if OldPattern.L = 0 then
    begin
      Result := p^;
      exit;
    end;
  m64 := TMem64.CustomCreate(p^.L);
  i := 1;
  while i <= p^.L do
    begin
      if ((not OnlyWord) or U_IsWord(p, i, i + OldPattern.L - 1)) and p^.ComparePos(i, @OldPattern, IgnoreCase) then
        begin
          m64.Write64(NewPattern.buff[0], USystemCharSize * NewPattern.L);
          inc(i, OldPattern.L);
        end
      else
        begin
          m64.Write64(p^.buff[i - 1], USystemCharSize);
          inc(i);
        end;
    end;
  Result.L := m64.Size div USystemCharSize;
  if Result.L > 0 then
      CopyPtr(m64.Memory, @Result.buff[0], m64.Size);
  DisposeObject(m64);
end;

function U_Replace(s, OldPattern, NewPattern: TUPascalString; OnlyWord, IgnoreCase: Boolean): TUPascalString;
begin
  Result := U_Replace(@s, OldPattern, NewPattern, OnlyWord, IgnoreCase);
end;

function U_ComputeTextPoint(p: PUPascalString; Pos_: Integer): TPoint;
var
  i, j: Integer;
begin
  Result.X := 1;
  Result.Y := 1;
  if Pos_ < p^.L then
      j := Pos_
  else
      j := p^.L;
  for i := 1 to j do
    if p^[i] = #10 then
      begin
        Result.X := 1;
        inc(Result.Y);
      end
    else if p^[i] = #13 then
        Result.X := 0
    else
        inc(Result.X);
end;

end.
