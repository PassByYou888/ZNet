{ ****************************************************************************** }
{ * Status IO                                                                  * }
{ ****************************************************************************** }
unit Z.Status;

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
{$ELSE FPC}
  System.Generics.Collections,
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
procedure DoError(v: SystemString; const Args: array of const); overload;
procedure DoStatus(const v: SystemString); overload;
procedure DoStatus(const v: TPascalString); overload;
procedure DoStatus(const v: TUPascalString); overload;
procedure DoStatus(const v: TMD5); overload;
procedure DoStatus(const p: Pointer; const siz: Integer); overload;
procedure DoStatus; overload;

procedure DoStatusNoLn(const v: TPascalString); overload;
procedure DoStatusNoLn(const v: SystemString; const Args: array of const); overload;
procedure DoStatusNoLn; overload;

function StrInfo(S: TPascalString): string; overload;
function StrInfo(S: TUPascalString): string; overload;
function BytesInfo(S: TBytes): string; overload;

var
  LastDoStatus: SystemString;
  IDEOutput: Boolean;
  ConsoleOutput: Boolean;
  OnDoStatusHook: TDoStatus_C;
  StatusThreadID: Boolean;

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
  DoStatus(Format('0x%p', [v]));
end;

procedure DoStatus(const v: SystemString; const Args: array of const);
begin
  DoStatus(Format(v, Args));
end;

procedure DoError(v: SystemString; const Args: array of const);
begin
  DoStatus(Format(v, Args), 2);
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

  TEvent_Pool___Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<PEvent_Struct__>;

  TEvent_Pool__ = class(TEvent_Pool___Decl)
  public
    procedure DoFree(var Data: PEvent_Struct__); override;
  end;

  TText_Queue_Data_Pool___Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<PText_Queue_Data>;

  TText_Queue_Data_Pool__ = class(TText_Queue_Data_Pool___Decl)
  public
    procedure DoFree(var Data: PText_Queue_Data); override;
  end;

  TNo_Ln_Text_Pool___Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<PNo_Ln_Text>;

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
{$IFDEF FPC}
  procedure do_fpc_progress_(Index_: NativeInt; p: TNo_Ln_Text_Pool__.PQueueStruct; var Aborted: Boolean);
  begin
    if p^.Data^.Th = Th_ then
      begin
        R_ := p^.Data;
        R_^.TriggerTime := Tick;
      end
    else if Tick - p^.Data^.TriggerTime > C_Tick_Minute then
      begin
        No_Ln_Text_Pool__.Push_To_Recycle_Pool(p);
      end;
  end;
{$ENDIF FPC}


begin
  R_ := nil;
  Tick := GetTimeTick();

{$IFDEF FPC}
  No_Ln_Text_Pool__.Progress_P(@do_fpc_progress_);
{$ELSE FPC}
  No_Ln_Text_Pool__.Progress_P(procedure(Index_: NativeInt; p: TNo_Ln_Text_Pool__.PQueueStruct; var Aborted: Boolean)
    begin
      if p^.Data^.Th = Th_ then
        begin
          R_ := p^.Data;
          R_^.TriggerTime := Tick;
        end
      else if Tick - p^.Data^.TriggerTime > C_Tick_Minute then
        begin
          No_Ln_Text_Pool__.Push_To_Recycle_Pool(p);
        end;
    end);
{$ENDIF FPC}
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
  DoStatusNoLn(Format(v, Args));
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

function StrInfo(S: TPascalString): string;
begin
  Result := BytesInfo(S.Bytes);
end;

function StrInfo(S: TUPascalString): string;
begin
  Result := BytesInfo(S.Bytes);
end;

function BytesInfo(S: TBytes): string;
begin
  Result := umlStringOf(S);
end;

procedure _InternalOutput(const Text_: U_String; const ID: Integer);
{$IFDEF FPC}
  procedure do_fpc_progress_(Index_: NativeInt; p: TEvent_Pool__.PQueueStruct; var Aborted: Boolean);
  begin
    try
      if Assigned(p^.Data^.OnStatusM) then
          p^.Data^.OnStatusM(Text_, ID);
      if Assigned(p^.Data^.OnStatusC) then
          p^.Data^.OnStatusC(Text_, ID);
      if Assigned(p^.Data^.OnStatusP) then
          p^.Data^.OnStatusP(Text_, ID);
    except
    end;
  end;
{$ENDIF FPC}


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
{$IFDEF FPC}
      Event_Pool__.Progress_P(@do_fpc_progress_);
{$ELSE FPC}
      Event_Pool__.Progress_P(procedure(Index_: NativeInt; p: TEvent_Pool__.PQueueStruct; var Aborted: Boolean)
        begin
          try
            if Assigned(p^.Data^.OnStatusM) then
                p^.Data^.OnStatusM(Text_, ID);
            if Assigned(p^.Data^.OnStatusC) then
                p^.Data^.OnStatusC(Text_, ID);
            if Assigned(p^.Data^.OnStatusP) then
                p^.Data^.OnStatusP(Text_, ID);
          except
          end;
        end);
{$ENDIF FPC}
    end;

{$IFDEF DELPHI}
  if (Status_Active__) and ((IDEOutput) or (ID = 2)) and (DebugHook <> 0) then
    begin
{$IF Defined(WIN32) or Defined(WIN64)}
      OutputDebugString(PWideChar('"' + Text_.Text + '"'));
{$ELSEIF not Defined(Linux)}
      FMX.Types.Log.d('"' + Text_.Text + '"');
{$ENDIF}
    end;
{$ENDIF DELPHI}
  if (Status_Active__) and ((ConsoleOutput) or (ID = 2)) and (IsConsole) then
    begin
{$IFDEF FPC}
      Writeln(UTF8Decode(Text_.Text));
{$ELSE FPC}
      Writeln(Text_.Text);
{$ENDIF FPC}
    end;
end;

procedure CheckDoStatus(Th: TCore_Thread);
begin
  if Status_Critical__ = nil then
      exit;
  if (Th = nil) or (Th.ThreadID <> MainThreadID) then
      exit;
  Status_Critical__.Acquire;
  try
    while Text_Queue_Data_Pool__.Num > 0 do
      begin
        _InternalOutput(Text_Queue_Data_Pool__.First^.Data^.S, Text_Queue_Data_Pool__.First^.Data^.ID);
        Text_Queue_Data_Pool__.Next;
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
          pSS^.S := '[' + IntToStr(Th.ThreadID) + '] ' + Text_
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
{$IFDEF FPC}
  procedure do_fpc_progress_(Index_: NativeInt; p: TEvent_Pool__.PQueueStruct; var Aborted: Boolean);
  begin
    if p^.Data^.TokenObj = TokenObj then
        Event_Pool__.Push_To_Recycle_Pool(p);
  end;
{$ENDIF FPC}


begin
  Event_Pool__.Free_Recycle_Pool;
{$IFDEF FPC}
  Event_Pool__.Progress_P(@do_fpc_progress_);
{$ELSE FPC}
  Event_Pool__.Progress_P(procedure(Index_: NativeInt; p: TEvent_Pool__.PQueueStruct; var Aborted: Boolean)
    begin
      if p^.Data^.TokenObj = TokenObj then
          Event_Pool__.Push_To_Recycle_Pool(p);
    end);
{$ENDIF FPC}
  Event_Pool__.Free_Recycle_Pool;
end;

procedure RemoveDoStatusHook(TokenObj: TCore_Object);
begin
  DeleteDoStatusHook(TokenObj);
end;

procedure DisableStatus;
begin
  Status_Active__ := False;
end;

procedure EnabledStatus;
begin
  Status_Active__ := True;
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
  IDEOutput := {$IFDEF FPC}False{$ELSE FPC}DebugHook > 0{$ENDIF FPC};
  ConsoleOutput := True;
  OnDoStatusHook := {$IFDEF FPC}@{$ENDIF FPC}InternalDoStatus;
  StatusThreadID := True;

  Hooked_OnCheckThreadSynchronize := Z.Core.OnCheckThreadSynchronize;
  Z.Core.OnCheckThreadSynchronize := {$IFDEF FPC}@{$ENDIF FPC}DoCheckThreadSynchronize;
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
