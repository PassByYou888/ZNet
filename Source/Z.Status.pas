{ ****************************************************************************** }
{ * Status Output                                                              * }
{ ****************************************************************************** }
unit Z.Status;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses
{$IFDEF DELPHI}
{$IF Defined(WIN32) or Defined(WIN64)}
  Windows,
{$ELSEIF not Defined(Linux)}
  FMX.Types,
{$ENDIF}
{$ENDIF DELPHI}
  // compatible
  SysUtils, Classes, SyncObjs,
{$IFDEF FPC}
  Z.FPC.GenericList, fgl,
{$ENDIF FPC}
  Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Core;

type
{$IFDEF FPC}
  TDoStatus_P = procedure(Text_: SystemString; const ID: Integer) is nested;
{$ELSE FPC}
  TDoStatus_P = reference to procedure(Text_: SystemString; const ID: Integer);
{$ENDIF FPC}
  TDoStatus_M = procedure(Text_: SystemString; const ID: Integer) of object;
  TDoStatus_C = procedure(Text_: SystemString; const ID: Integer);

procedure AddDoStatusHook(TokenObj: TCore_Object; OnNotify: TDoStatus_M);
procedure AddDoStatusHookM(TokenObj: TCore_Object; OnNotify: TDoStatus_M);
procedure AddDoStatusHookC(TokenObj: TCore_Object; OnNotify: TDoStatus_C);
procedure AddDoStatusHookP(TokenObj: TCore_Object; OnNotify: TDoStatus_P);
procedure DeleteDoStatusHook(TokenObj: TCore_Object);
procedure RemoveDoStatusHook(TokenObj: TCore_Object);
procedure DisableStatus;
procedure EnabledStatus;
function Is_EnabledStatus: Boolean;
function Is_DisableStatus: Boolean;
function Get_DoStatus_Queue_Num: NativeInt;
procedure Wait_DoStatus_Queue;

procedure DoStatus(Text_: SystemString; const ID: Integer); overload;
procedure DoStatus(const v: Pointer; siz, width: NativeInt); overload;
procedure DoStatus(prefix: SystemString; v: Pointer; siz, width: NativeInt); overload;
procedure DoStatus(const v: TCore_Strings); overload;
procedure DoStatus(const v: Int64); overload;
procedure DoStatus(const v: Integer); overload;
procedure DoStatus(const v: Single); overload;
procedure DoStatus(const v: Double); overload;
procedure DoStatus(const v: Pointer); overload;
procedure DoStatus(const v: SystemString; const Args: array of const); overload;
procedure DoStatus(const v: SystemString); overload;
procedure DoStatus(const v: TPascalString); overload;
procedure DoStatus(const v: TUPascalString); overload;
procedure DoStatus(const v: TMD5); overload;
procedure DoStatus(const p: Pointer; const siz: Integer); overload;
procedure DoStatus; overload;

procedure DoStatusNoLn(const v: TPascalString); overload;
procedure DoStatusNoLn(const v: SystemString; const Args: array of const); overload;
procedure DoStatusNoLn; overload;

// dispose object and print info
function DisposeObject_PrintInfo(const Obj: TObject): Boolean;
function DisposeObjectAndNil_PrintInfo(var Obj): Boolean;

var
  LastDoStatus: SystemString;
  IDEOutput: Boolean;
  ConsoleOutput: Boolean;
  OnDoStatusHook: TDoStatus_C;
  StatusThreadID: Boolean;
  One_Step_Status_Limit: Integer;

implementation

uses Z.Cipher;

procedure bufHashToString(hash: Pointer; Size: NativeInt; var output: TPascalString);
const
  HexArr: array [0 .. 15] of SystemChar = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  i: Integer;
begin
  output.Len := Size * 2;
  for i := 0 to Size - 1 do
    begin
      output.buff[i * 2] := HexArr[(PByte(nativeUInt(hash) + i)^ shr 4) and $0F];
      output.buff[i * 2 + 1] := HexArr[PByte(nativeUInt(hash) + i)^ and $0F];
    end;
end;

procedure DoStatus(Text_: SystemString; const ID: Integer);
begin
  try
      OnDoStatusHook(Text_, ID);
  except
  end;
end;

procedure DoStatus(const v: Pointer; siz, width: NativeInt);
var
  S: TPascalString;
  i: Integer;
  n: SystemString;
begin
  bufHashToString(v, siz, S);
  n := '';
  for i := 1 to S.Len div 2 do
    begin
      if n <> '' then
          n := n + #32 + S[i * 2 - 1] + S[i * 2]
      else
          n := S[i * 2 - 1] + S[i * 2];

      if i mod (width div 2) = 0 then
        begin
          DoStatus(n);
          n := '';
        end;
    end;
  if n <> '' then
      DoStatus(n);
end;

procedure DoStatus(prefix: SystemString; v: Pointer; siz, width: NativeInt);
var
  S: TPascalString;
  i: Integer;
  n: SystemString;
begin
  bufHashToString(v, siz, S);
  n := '';
  for i := 1 to S.Len div 2 do
    begin
      if n <> '' then
          n := n + #32 + S[i * 2 - 1] + S[i * 2]
      else
          n := S[i * 2 - 1] + S[i * 2];

      if i mod (width div 2) = 0 then
        begin
          DoStatus(prefix + n);
          n := '';
        end;
    end;
  if n <> '' then
      DoStatus(prefix + n);
end;

procedure DoStatus(const v: TCore_Strings);
var
  i: Integer;
  o: TCore_Object;
begin
  for i := 0 to v.Count - 1 do
    begin
      o := v.Objects[i];
      if o <> nil then
          DoStatus('%s<%s>', [v[i], o.ClassName])
      else
          DoStatus(v[i]);
    end;
end;

procedure DoStatus(const v: Int64);
begin
  DoStatus(IntToStr(v));
end;

procedure DoStatus(const v: Integer);
begin
  DoStatus(IntToStr(v));
end;

procedure DoStatus(const v: Single);
begin
  DoStatus(FloatToStr(v));
end;

procedure DoStatus(const v: Double);
begin
  DoStatus(FloatToStr(v));
end;

procedure DoStatus(const v: Pointer);
begin
  try
      DoStatus(Format('0x%p', [v]));
  except
  end;
end;

procedure DoStatus(const v: SystemString; const Args: array of const);
begin
  try
      DoStatus(Format(v, Args));
  except
      DoStatus('format text error %s', [v]);
  end;
end;

procedure DoStatus(const v: SystemString);
begin
  DoStatus(v, 0);
end;

procedure DoStatus(const v: TPascalString);
begin
  DoStatus(v.Text, 0);
end;

procedure DoStatus(const v: TUPascalString);
begin
  DoStatus(v.Text, 0);
end;

procedure DoStatus(const v: TMD5);
begin
  DoStatus(umlMD5ToString(v).Text);
end;

procedure DoStatus(const p: Pointer; const siz: Integer);
begin
  DoStatus(TCipher.BuffToString(p, siz));
end;

type
  TEvent_Struct__ = record
    TokenObj: TCore_Object;
    OnStatusM: TDoStatus_M;
    OnStatusC: TDoStatus_C;
    OnStatusP: TDoStatus_P;
  end;

  PEvent_Struct__ = ^TEvent_Struct__;

  TText_Queue_Data = record
    S: SystemString;
    Th: TCore_Thread;
    TriggerTime: TTimeTick;
    ID: Integer;
  end;

  PText_Queue_Data = ^TText_Queue_Data;

  TNo_Ln_Text = record
    S: TPascalString;
    Th: TCore_Thread;
    TriggerTime: TTimeTick;
  end;

  PNo_Ln_Text = ^TNo_Ln_Text;

  TEvent_Pool___Decl = TBigList<PEvent_Struct__>;

  TEvent_Pool__ = class(TEvent_Pool___Decl)
  public
    procedure DoFree(var Data: PEvent_Struct__); override;
  end;

  TText_Queue_Data_Pool___Decl = TBigList<PText_Queue_Data>;

  TText_Queue_Data_Pool__ = class(TText_Queue_Data_Pool___Decl)
  public
    procedure DoFree(var Data: PText_Queue_Data); override;
  end;

  TNo_Ln_Text_Pool___Decl = TBigList<PNo_Ln_Text>;

  TNo_Ln_Text_Pool__ = class(TNo_Ln_Text_Pool___Decl)
  public
    procedure DoFree(var Data: PNo_Ln_Text); override;
  end;

procedure TEvent_Pool__.DoFree(var Data: PEvent_Struct__);
begin
  dispose(Data);
  Data := nil;
end;

procedure TText_Queue_Data_Pool__.DoFree(var Data: PText_Queue_Data);
begin
  Data^.S := '';
  dispose(Data);
  Data := nil;
end;

procedure TNo_Ln_Text_Pool__.DoFree(var Data: PNo_Ln_Text);
begin
  Data^.S := '';
  dispose(Data);
  Data := nil;
end;

var
  Status_Active__: Boolean;
  Event_Pool__: TEvent_Pool__;
  Text_Queue_Data_Pool__: TText_Queue_Data_Pool__;
  Status_Critical__: TCritical;
  No_Ln_Text_Pool__: TNo_Ln_Text_Pool__;
  Hooked_OnCheckThreadSynchronize: TOnCheckThreadSynchronize;

function GetOrCreateStatusNoLnData_(Th_: TCore_Thread): PNo_Ln_Text;
var
  R_: PNo_Ln_Text;
  Tick: TTimeTick;
begin
  R_ := nil;
  Tick := GetTimeTick();

  if No_Ln_Text_Pool__.Num > 0 then
    with No_Ln_Text_Pool__.Repeat_ do
      repeat
        if Queue^.Data^.Th = Th_ then
          begin
            R_ := Queue^.Data;
            R_^.TriggerTime := Tick;
          end
        else if Tick - Queue^.Data^.TriggerTime > C_Tick_Minute then
          begin
            No_Ln_Text_Pool__.Push_To_Recycle_Pool(Queue);
          end;
      until not Next;
  No_Ln_Text_Pool__.Free_Recycle_Pool;

  if R_ = nil then
    begin
      new(R_);
      R_^.S := '';
      R_^.Th := Th_;
      R_^.TriggerTime := Tick;
      No_Ln_Text_Pool__.Add(R_);
    end;
  Result := R_;
end;

function GetOrCreateStatusNoLnData(): PNo_Ln_Text;
begin
  Result := GetOrCreateStatusNoLnData_(TCore_Thread.CurrentThread);
end;

procedure DoStatusNoLn(const v: TPascalString);
var
  L, i: Integer;
  StatusNoLnData: PNo_Ln_Text;
  pSS: PText_Queue_Data;
begin
  Status_Critical__.Acquire;
  StatusNoLnData := GetOrCreateStatusNoLnData();
  try
    L := v.Len;
    i := 1;
    while i <= L do
      begin
        if CharIn(v[i], [#13, #10]) then
          begin
            if StatusNoLnData^.S.Len > 0 then
              begin
                new(pSS);
                pSS^.S := StatusNoLnData^.S.Text;
                pSS^.Th := TCore_Thread.CurrentThread;
                pSS^.TriggerTime := GetTimeTick;
                pSS^.ID := 0;
                Text_Queue_Data_Pool__.Add(pSS);
                StatusNoLnData^.S := '';
              end;
            repeat
                inc(i);
            until (i > L) or (not CharIn(v[i], [#13, #10]));
          end
        else
          begin
            StatusNoLnData^.S.Append(v[i]);
            inc(i);
          end;
      end;
  finally
      Status_Critical__.Release;
  end;
end;

procedure DoStatusNoLn(const v: SystemString; const Args: array of const);
begin
  try
      DoStatusNoLn(Format(v, Args));
  except
      DoStatusNoLn('format text error %s', [v]);
  end;
end;

procedure DoStatusNoLn;
var
  StatusNoLnData: PNo_Ln_Text;
  S: SystemString;
begin
  Status_Critical__.Acquire;
  StatusNoLnData := GetOrCreateStatusNoLnData();
  S := StatusNoLnData^.S;
  StatusNoLnData^.S := '';
  Status_Critical__.Release;
  if Length(S) > 0 then
      DoStatus(S);
end;

function DisposeObject_PrintInfo(const Obj: TObject): Boolean;
{$IFDEF SHOW_DISPOSEOBJECT_PRINTINFO_LOG}
var
  n: SystemString;
{$ENDIF SHOW_DISPOSEOBJECT_PRINTINFO_LOG}
begin
{$IFDEF SHOW_DISPOSEOBJECT_PRINTINFO_LOG}
  Result := False;
  if Obj = nil then
      exit;
  n := Obj.ClassName;
  Result := DisposeObject(Obj);
  DoStatus('free %s', [n]);
{$ELSE SHOW_DISPOSEOBJECT_PRINTINFO_LOG}
  Result := DisposeObject(Obj);
{$ENDIF SHOW_DISPOSEOBJECT_PRINTINFO_LOG}
end;

function DisposeObjectAndNil_PrintInfo(var Obj): Boolean;
{$IFDEF SHOW_DISPOSEOBJECT_PRINTINFO_LOG}
var
  n: SystemString;
{$ENDIF SHOW_DISPOSEOBJECT_PRINTINFO_LOG}
begin
{$IFDEF SHOW_DISPOSEOBJECT_PRINTINFO_LOG}
  Result := False;
  if TObject(Obj) = nil then
      exit;
  n := TObject(Obj).ClassName;
  Result := DisposeObjectAndNil(Obj);
  DoStatus('free and nil %s', [n]);
{$ELSE SHOW_DISPOSEOBJECT_PRINTINFO_LOG}
  Result := DisposeObjectAndNil(Obj);
{$ENDIF SHOW_DISPOSEOBJECT_PRINTINFO_LOG}
end;

procedure _InternalOutput(const Text_: U_String; const ID: Integer);
var
  tmp: U_String;
begin
  if Text_.Exists(#10) then
    begin
      tmp := Text_.DeleteChar(#13);
      _InternalOutput(umlGetFirstStr_Discontinuity(tmp, #10), ID);
      tmp := umlDeleteFirstStr_Discontinuity(tmp, #10);
      _InternalOutput(tmp, ID);
      exit;
    end;
  if (Status_Active__) and (Event_Pool__.Num > 0) then
    begin
      LastDoStatus := Text_;
      with Event_Pool__.Repeat_ do
        repeat
          try
            if Assigned(Queue^.Data^.OnStatusM) then
                Queue^.Data^.OnStatusM(Text_, ID);
            if Assigned(Queue^.Data^.OnStatusC) then
                Queue^.Data^.OnStatusC(Text_, ID);
            if Assigned(Queue^.Data^.OnStatusP) then
                Queue^.Data^.OnStatusP(Text_, ID);
          except
          end;
        until not Next;
    end;

{$IFDEF DELPHI}
  if Status_Active__ and IDEOutput and (DebugHook <> 0) then
    begin
{$IF Defined(WIN32) or Defined(WIN64)}
      OutputDebugString(PWideChar('"' + Text_.Text + '"'));
{$ELSEIF not Defined(Linux)}
      FMX.Types.Log.d('"' + Text_.Text + '"');
{$ENDIF}
    end;
{$ENDIF DELPHI}
  if Status_Active__ and ConsoleOutput and IsConsole then
    begin
{$IFDEF FPC}
      Writeln(UTF8Decode(Text_.Text));
{$ELSE FPC}
      Writeln(Text_.Text);
{$ENDIF FPC}
    end;
end;

procedure CheckDoStatus(Th: TCore_Thread);
var
  i: Integer;
begin
  if Status_Critical__ = nil then
      exit;
  if (Th = nil) or (Th.ThreadID <> MainThreadID) then
      exit;
  Status_Critical__.Acquire;
  try
    i := 0;
    while (Text_Queue_Data_Pool__.Num > 0) and (i < One_Step_Status_Limit) do
      begin
        _InternalOutput(Text_Queue_Data_Pool__.First^.Data^.S, Text_Queue_Data_Pool__.First^.Data^.ID);
        Text_Queue_Data_Pool__.Next;
        inc(i);
      end;
  finally
      Status_Critical__.Release;
  end;
end;

procedure DoStatus;
begin
  CheckDoStatus(TCore_Thread.CurrentThread);
end;

procedure InternalDoStatus(Text_: SystemString; const ID: Integer);
var
  Th: TCore_Thread;
  pSS: PText_Queue_Data;
begin
  Th := TCore_Thread.CurrentThread;
  if (Th = nil) or (Th.ThreadID <> MainThreadID) then
    begin
      new(pSS);
      if StatusThreadID then
          pSS^.S := '[' + IntToStr(Th.ThreadID) + '] ' + umlReplace(Text_, #10, #10 + '[' + IntToStr(Th.ThreadID) + '] ', False, False)
      else
          pSS^.S := Text_;
      pSS^.Th := Th;
      pSS^.TriggerTime := GetTimeTick();
      pSS^.ID := ID;
      Status_Critical__.Acquire;
      Text_Queue_Data_Pool__.Add(pSS);
      Status_Critical__.Release;
      exit;
    end;

  CheckDoStatus(Th);
  _InternalOutput(Text_, ID);
end;

procedure AddDoStatusHook(TokenObj: TCore_Object; OnNotify: TDoStatus_M);
begin
  AddDoStatusHookM(TokenObj, OnNotify);
end;

procedure AddDoStatusHookM(TokenObj: TCore_Object; OnNotify: TDoStatus_M);
var
  p: PEvent_Struct__;
begin
  new(p);
  p^.TokenObj := TokenObj;
  p^.OnStatusM := OnNotify;
  p^.OnStatusC := nil;
  p^.OnStatusP := nil;
  Event_Pool__.Add(p);
end;

procedure AddDoStatusHookC(TokenObj: TCore_Object; OnNotify: TDoStatus_C);
var
  p: PEvent_Struct__;
begin
  new(p);
  p^.TokenObj := TokenObj;
  p^.OnStatusM := nil;
  p^.OnStatusC := OnNotify;
  p^.OnStatusP := nil;
  Event_Pool__.Add(p);
end;

procedure AddDoStatusHookP(TokenObj: TCore_Object; OnNotify: TDoStatus_P);
var
  p: PEvent_Struct__;
begin
  new(p);
  p^.TokenObj := TokenObj;
  p^.OnStatusM := nil;
  p^.OnStatusC := nil;
  p^.OnStatusP := OnNotify;
  Event_Pool__.Add(p);
end;

procedure DeleteDoStatusHook(TokenObj: TCore_Object);
begin
  RemoveDoStatusHook(TokenObj);
end;

procedure RemoveDoStatusHook(TokenObj: TCore_Object);
begin
  Event_Pool__.Free_Recycle_Pool;
  if Event_Pool__.Num > 0 then
    with Event_Pool__.Repeat_ do
      repeat
        if Queue^.Data^.TokenObj = TokenObj then
            Event_Pool__.Push_To_Recycle_Pool(Queue);
      until not Next;
  Event_Pool__.Free_Recycle_Pool;
end;

procedure DisableStatus;
begin
  Status_Active__ := False;
end;

procedure EnabledStatus;
begin
  Status_Active__ := True;
end;

function Is_EnabledStatus: Boolean;
begin
  Result := Status_Active__;
end;

function Is_DisableStatus: Boolean;
begin
  Result := not Status_Active__;
end;

function Get_DoStatus_Queue_Num: NativeInt;
begin
  Result := Text_Queue_Data_Pool__.Num;
end;

procedure Wait_DoStatus_Queue;
begin
  if TCompute.CurrentThread.ThreadID <> MainThreadID then
    begin
      while Get_DoStatus_Queue_Num > 0 do
          TCompute.Sleep(1);
    end
  else
    begin
      while Get_DoStatus_Queue_Num > 0 do
        begin
          DoStatus;
          TCompute.Sleep(1);
        end;
    end;
end;

procedure DoCheckThreadSynchronize;
begin
  DoStatus();
  if Assigned(Hooked_OnCheckThreadSynchronize) then
      Hooked_OnCheckThreadSynchronize();
end;

procedure _DoInit;
begin
  Event_Pool__ := TEvent_Pool__.Create;
  Text_Queue_Data_Pool__ := TText_Queue_Data_Pool__.Create;
  Status_Critical__ := TCritical.Create;
  No_Ln_Text_Pool__ := TNo_Ln_Text_Pool__.Create;

  Status_Active__ := True;
  LastDoStatus := '';
  IDEOutput := False;
  ConsoleOutput := True;
  OnDoStatusHook := InternalDoStatus;
  StatusThreadID := True;
  One_Step_Status_Limit := 20;

  Hooked_OnCheckThreadSynchronize := Z.Core.OnCheckThreadSynchronize;
  Z.Core.OnCheckThreadSynchronize := DoCheckThreadSynchronize;
end;

procedure _DoFree;
begin
  Event_Pool__.Free;
  Text_Queue_Data_Pool__.Free;
  No_Ln_Text_Pool__.Free;
  Status_Critical__.Free;
  Status_Active__ := True;
  Status_Critical__ := nil;
end;

initialization

_DoInit;

finalization

_DoFree;

end.
