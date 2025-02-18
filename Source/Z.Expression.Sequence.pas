(*
https://zpascal.net
https://github.com/PassByYou888/ZNet
https://github.com/PassByYou888/zRasterization
https://github.com/PassByYou888/ZSnappy
https://github.com/PassByYou888/Z-AI1.4
https://github.com/PassByYou888/ZAI_1.41
https://github.com/PassByYou888/InfiniteIoT
https://github.com/PassByYou888/zMonitor_3rd_Core
https://github.com/PassByYou888/tcmalloc4p
https://github.com/PassByYou888/jemalloc4p
https://github.com/PassByYou888/zCloud
https://github.com/PassByYou888/ZServer4D
https://github.com/PassByYou888/zShell
https://github.com/PassByYou888/ZDB2.0
https://github.com/PassByYou888/zGameWare
https://github.com/PassByYou888/CoreCipher
https://github.com/PassByYou888/zChinese
https://github.com/PassByYou888/zSound
https://github.com/PassByYou888/zExpression
https://github.com/PassByYou888/ZInstaller2.0
https://github.com/PassByYou888/zAI
https://github.com/PassByYou888/NetFileService
https://github.com/PassByYou888/zAnalysis
https://github.com/PassByYou888/PascalString
https://github.com/PassByYou888/zInstaller
https://github.com/PassByYou888/zTranslate
https://github.com/PassByYou888/zVision
https://github.com/PassByYou888/FFMPEG-Header
*)
{ ****************************************************************************** }
{ * Expression sequence                                                        * }
{ ****************************************************************************** }
unit Z.Expression.Sequence;

{$DEFINE FPC_DELPHI_MODE}
{$I Z.Define.inc}

interface

uses SysUtils, Variants, Math,
  Z.Core, TypInfo, Z.Parsing, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Notify,
  Z.Status, Z.ListEngine, Z.Expression, Z.OpCode;

type
  TExpression_Sequence = class;
  TExpression_Sequence_RunTime = class;
  TExpression_Sequence_RunTime_Class = class of TExpression_Sequence_RunTime;

  TOn_Expression_Sequence_Create_RunTime = procedure(Sender: TExpression_Sequence; NewRunTime: TExpression_Sequence_RunTime) of object;
  TOn_Expression_Sequence_Step = procedure(Sender: TExpression_Sequence; Current_Step: TExpression_Sequence_RunTime) of object;
  TOn_Expression_Sequence_Done = procedure(Sender: TExpression_Sequence) of object;

  TExpression_Sequence = class(TBig_Object_List<TExpression_Sequence_RunTime>)
  private
    FDebugMode: Boolean;
    FRun_Num, FRun_Done_Num: Integer;
    FFocus: TExpression_Sequence_RunTime;
    FError: Boolean;
    FPost___: TThreadPost;
    FN_Progress: TCadencer_N_Progress_Tool;
    FSequence_Class: TExpression_Sequence_RunTime_Class;
    FOn_Expression_Sequence_Create_RunTime: TOn_Expression_Sequence_Create_RunTime;
    FOn_Expression_Sequence_Step: TOn_Expression_Sequence_Step;
    FOn_Expression_Sequence_Done: TOn_Expression_Sequence_Done;
    procedure Do_Step; virtual;
  public
    property DebugMode: Boolean read FDebugMode write FDebugMode;
    property Sequence_Class: TExpression_Sequence_RunTime_Class read FSequence_Class write FSequence_Class;
    property On_Expression_Sequence_Create_RunTime: TOn_Expression_Sequence_Create_RunTime read FOn_Expression_Sequence_Create_RunTime write FOn_Expression_Sequence_Create_RunTime;
    property On_Expression_Sequence_Step: TOn_Expression_Sequence_Step read FOn_Expression_Sequence_Step write FOn_Expression_Sequence_Step;
    property On_Expression_Sequence_Done: TOn_Expression_Sequence_Done read FOn_Expression_Sequence_Done write FOn_Expression_Sequence_Done;
    constructor Create;
    destructor Destroy; override;
    function Extract_Code(Style: TTextStyle; Code: TCore_Strings): Boolean; overload;
    function Extract_Code(Style: TTextStyle; Code: U_String): Boolean; overload;
    function Check_Syntax: Boolean;
    procedure Run;
    property Run_Num: Integer read FRun_Num;
    property Run_Done_Num: Integer read FRun_Done_Num;
    procedure Wait;
    function Is_Running: Boolean;
    property Is_Error: Boolean read FError;
    property Focus: TExpression_Sequence_RunTime read FFocus;
    procedure Do_End; virtual;
    procedure Progress; virtual;
    property Post_Progress: TCadencer_N_Progress_Tool read FN_Progress;
    property N_Progress: TCadencer_N_Progress_Tool read FN_Progress;
    class procedure Test();
  end;

  TExpression_Sequence_Pool_ = TBig_Object_List<TExpression_Sequence>;

  TExpression_Sequence_Pool = class(TExpression_Sequence_Pool_)
  public
    procedure Progress;
  end;

  TExpression_Sequence_RunTime = class(TOpCustomRunTime)
  private
    Queue_Data: TExpression_Sequence.PQueueStruct;
    NonLinear_Tool: TOpCode_NonLinear;
    Done: Boolean;
    Error: Boolean;
    procedure Do_NonLinear_Done(Sender: TOpCode_NonLinear);
    procedure Do_Run__;
    procedure Do_End__;
    procedure Do_Error__;
  public
    Owner: TExpression_Sequence;
    Line: Integer;
    Style: TTextStyle;
    Code_: U_String;
    constructor Create(Owner_: TExpression_Sequence); virtual;
    destructor Destroy; override;
    procedure Reg_RunTime; virtual;
    procedure Do_Run;
    procedure Do_Begin;
    procedure Do_End; overload;
    procedure Do_End(Result_: Variant); overload;
    procedure Do_End_And_Result(Result_: Variant);
    function Running: Boolean;
    function Is_Wait_End: Boolean;
    function Get_Code_Result: Variant;
    procedure Set_Code_Result(Value: Variant);
    property Code_Result: Variant read Get_Code_Result write Set_Code_Result;
    property Enb_Result: Variant read Get_Code_Result write Set_Code_Result;
    property Result_: Variant read Get_Code_Result write Set_Code_Result;
    procedure Do_Error;
    procedure Progress; virtual;
  end;

  TTest_Expression_Sequence_RunTime = class(TExpression_Sequence_RunTime)
  public
    function OP_Test_Direct_Execute(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; OP_Code: TOpCode; var OP_Param: TOpParam): Variant;
    function OP_Test_Delay_Execute(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; OP_Code: TOpCode; var OP_Param: TOpParam): Variant;
    procedure Reg_RunTime; override;
  end;

implementation

procedure TExpression_Sequence.Do_Step;
begin
  if FFocus = nil then
      exit;
  if FFocus.Running then
      exit;
  if FFocus.Error then
    begin
      FError := True;
      FFocus := nil;
      exit;
    end;

  if FFocus.Done then
    begin
      if Assigned(FOn_Expression_Sequence_Step) then
          FOn_Expression_Sequence_Step(self, FFocus);

      if FFocus.Queue_Data <> Last then
        begin
          FFocus := FFocus.Queue_Data^.Next^.Data;
          FFocus.Do_Run;
        end
      else
        begin
          FFocus := nil;
          Inc(FRun_Done_Num);
          Do_End();
          if Assigned(FOn_Expression_Sequence_Done) then
              FOn_Expression_Sequence_Done(self);
        end;
    end;
end;

constructor TExpression_Sequence.Create;
begin
  inherited Create(True);
  FDebugMode := {$IFDEF Print_OPCode_Debug}True{$ELSE Print_OPCode_Debug}False{$ENDIF Print_OPCode_Debug};
  FRun_Num := 0;
  FRun_Done_Num := 0;
  FFocus := nil;
  FError := False;
  FPost___ := TThreadPost.Create(0);
  FN_Progress := TCadencer_N_Progress_Tool.Create;
  FSequence_Class := TExpression_Sequence_RunTime;
  FOn_Expression_Sequence_Create_RunTime := nil;
  FOn_Expression_Sequence_Step := nil;
  FOn_Expression_Sequence_Done := nil;
end;

destructor TExpression_Sequence.Destroy;
begin
  DisposeObject(FPost___);
  DisposeObject(FN_Progress);
  inherited Destroy;
end;

function TExpression_Sequence.Extract_Code(Style: TTextStyle; Code: TCore_Strings): Boolean;
var
  i, j: Integer;
  inst: TExpression_Sequence_RunTime;
  n: U_String;
  T: TTextParsing;
  L: TPascalStringList;
begin
  FRun_Num := 0;
  FRun_Done_Num := 0;
  Result := False;
  for i := 0 to Code.Count - 1 do
    begin
      n := Code[i];
      if IsNullExpression(n, Style) then
          continue;
      if IsSymbolVectorExpression(n, Style) then
        begin
          T := TTextParsing.Create(n, Style, nil, SpacerSymbol.V);
          L := TPascalStringList.Create;
          if T.Extract_Symbol_Vector(L) then
            begin
              for j := 0 to L.Count - 1 do
                begin
                  inst := FSequence_Class.Create(self);
                  inst.Line := i;
                  inst.Style := Style;
                  inst.Code_ := L[j];
                  inst.Queue_Data := Add(inst);
                  if Assigned(FOn_Expression_Sequence_Create_RunTime) then
                      FOn_Expression_Sequence_Create_RunTime(self, inst);
                end;
              DisposeObject(T);
              DisposeObject(L);
            end
          else
            begin
              DisposeObject(T);
              DisposeObject(L);
              DoStatus('error line:%d, code: %s', [i, n.Text]);
              exit;
            end;
        end
      else
        begin
          inst := FSequence_Class.Create(self);
          inst.Line := i;
          inst.Style := Style;
          inst.Code_ := n;
          inst.Queue_Data := Add(inst);
          if Assigned(FOn_Expression_Sequence_Create_RunTime) then
              FOn_Expression_Sequence_Create_RunTime(self, inst);
        end;
    end;
  Result := True;
end;

function TExpression_Sequence.Extract_Code(Style: TTextStyle; Code: U_String): Boolean;
var
  tmp: TCore_Strings;
begin
  tmp := TCore_StringList.Create;
  tmp.Text := Code;
  Result := Extract_Code(Style, tmp);
  DisposeObject(tmp);
end;

function TExpression_Sequence.Check_Syntax: Boolean;
var
  tmp: TSymbolExpression;
begin
  Result := True;
  if Num > 0 then
    begin
      with repeat_ do
        repeat
          tmp := ParseTextExpressionAsSymbol(queue^.Data.Style, '', queue^.Data.Code_, nil, queue^.Data);
          if tmp <> nil then
              DisposeObjectAndNil(tmp)
          else
              exit(False);
        until not Next;
    end;
end;

procedure TExpression_Sequence.Run;
var
  i: Integer;
begin
  if Is_Running then
      exit;

  Inc(FRun_Num);

  // reset error state
  FError := False;

  if Num > 0 then
    begin
      with repeat_ do
        repeat
          queue^.Data.Done := False;
          queue^.Data.Error := False;
        until not Next;

      FFocus := First^.Data;
      FFocus.Do_Run;
    end;
end;

procedure TExpression_Sequence.Wait;
begin
  while Is_Running do
      Progress();
end;

function TExpression_Sequence.Is_Running: Boolean;
begin
  Result := FFocus <> nil;
end;

procedure TExpression_Sequence.Do_End;
begin
  if not FDebugMode then
      exit;
  if Num > 0 then
    with repeat_ do
      repeat
          DoStatus('%s = %s', [queue^.Data.Code_.Text, VarToStr(queue^.Data.Code_Result)]);
      until not Next;
end;

procedure TExpression_Sequence.Progress;
begin
  if (FFocus <> nil) and (Num > 0) then
      FFocus.Progress;
  FPost___.Progress(FPost___.ThreadID);
  FN_Progress.Progress;
end;

class procedure TExpression_Sequence.Test;
var
  L: TCore_Strings;
  inst: TExpression_Sequence;
begin
  L := TCore_StringList.Create;
  L.Add('Direct()');
  L.Add('Delay()');
  L.Add('Direct()');
  L.Add('Delay()');
  L.Add('Direct(),Delay(1.1),Delay(1.2),Direct()');
  inst := TExpression_Sequence.Create;
  inst.DebugMode := True;
  inst.Sequence_Class := TTest_Expression_Sequence_RunTime;
  inst.Extract_Code(tsPascal, L);
  DoStatus('Check_Syntax=%s', [umlBoolToStr(inst.Check_Syntax()).Text]);
  DisposeObject(L);
  inst.Run;
  inst.Wait();
  DisposeObject(inst);
end;

procedure TExpression_Sequence_Pool.Progress;
begin
  if Num > 0 then
    with repeat_ do
      repeat
          queue^.Data.Progress;
      until not Next;
end;

procedure TExpression_Sequence_RunTime.Do_NonLinear_Done(Sender: TOpCode_NonLinear);
begin
  if VarIsNull(Code_Result) then
      Owner.FPost___.PostM1(Do_Error__)
  else
      Owner.FPost___.PostM1(Do_End__);
end;

procedure TExpression_Sequence_RunTime.Do_Run__;
begin
  if Owner.FDebugMode then
      DoStatus('Debug Mode ' + Code_);
  Done := False;
  Error := False;
  if NonLinear_Tool = nil then
    begin
      NonLinear_Tool := TOpCode_NonLinear.Create_From_Expression(Style, Code_, self);
      NonLinear_Tool.On_Done_M := Do_NonLinear_Done;
    end
  else
    begin
      NonLinear_Tool.Reinit;
    end;
  NonLinear_Tool.Execute;
end;

procedure TExpression_Sequence_RunTime.Do_End__;
begin
  Done := True;
  Error := False;
  Owner.Do_Step;
end;

procedure TExpression_Sequence_RunTime.Do_Error__;
begin
  Done := False;
  Error := True;
  DoStatus('error line:%d, code: %s', [Line, Code_.Text]);
  Owner.Do_Step;
end;

constructor TExpression_Sequence_RunTime.Create(Owner_: TExpression_Sequence);
begin
  inherited CustomCreate($FF);
  Queue_Data := nil;
  NonLinear_Tool := nil;
  Done := False;
  Error := False;
  Owner := Owner_;
  Line := 0;
  Style := TTextStyle.tsPascal;
  Code_ := '';
  Reg_RunTime();
end;

destructor TExpression_Sequence_RunTime.Destroy;
begin
  Code_ := '';
  DisposeObjectAndNil(NonLinear_Tool);
  inherited Destroy;
end;

procedure TExpression_Sequence_RunTime.Reg_RunTime;
begin
end;

procedure TExpression_Sequence_RunTime.Do_Run;
begin
  Done := False;
  Error := False;
  Owner.FPost___.PostM1(Do_Run__);
end;

procedure TExpression_Sequence_RunTime.Do_Begin;
begin
  NonLinear_Tool.Do_Begin;
end;

procedure TExpression_Sequence_RunTime.Do_End;
begin
  NonLinear_Tool.Do_End;
end;

procedure TExpression_Sequence_RunTime.Do_End(Result_: Variant);
begin
  NonLinear_Tool.Do_End(Result_);
end;

procedure TExpression_Sequence_RunTime.Do_End_And_Result(Result_: Variant);
begin
  Do_End(Result_);
end;

function TExpression_Sequence_RunTime.Running: Boolean;
begin
  if NonLinear_Tool <> nil then
      Result := NonLinear_Tool.Is_Running
  else
      Result := False;
end;

function TExpression_Sequence_RunTime.Is_Wait_End: Boolean;
begin
  if NonLinear_Tool <> nil then
      Result := NonLinear_Tool.Is_Wait_End
  else
      Result := False;
end;

function TExpression_Sequence_RunTime.Get_Code_Result: Variant;
begin
  if NonLinear_Tool <> nil then
      Result := NonLinear_Tool.Result_
  else
      Result := NULL;
end;

procedure TExpression_Sequence_RunTime.Set_Code_Result(Value: Variant);
begin
  if NonLinear_Tool <> nil then
      NonLinear_Tool.Result_ := Value;
end;

procedure TExpression_Sequence_RunTime.Do_Error;
begin
  NonLinear_Tool.Do_Error;
end;

procedure TExpression_Sequence_RunTime.Progress;
begin
  if NonLinear_Tool <> nil then
      NonLinear_Tool.Process;
end;

function TTest_Expression_Sequence_RunTime.OP_Test_Direct_Execute(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; OP_Code: TOpCode; var OP_Param: TOpParam): Variant;
begin
  Do_Begin();
  Result := 1;
  Do_End();
end;

function TTest_Expression_Sequence_RunTime.OP_Test_Delay_Execute(Sender: TOpCustomRunTime; OP_RT_Data: POpRTData; OP_Code: TOpCode; var OP_Param: TOpParam): Variant;
var
  d: Double;
begin
  Do_Begin();
  Result := 2;
  if length(OP_Param) > 0 then
      d := OP_Param[0]
  else
      d := 1.0;
  Owner.Post_Progress.PostExecuteM_NP(d, Do_End);
end;

procedure TTest_Expression_Sequence_RunTime.Reg_RunTime;
begin
  inherited Reg_RunTime;
  Reg_Code_OpM('Direct', '', OP_Test_Direct_Execute);
  Reg_Code_OpM('Delay', '', OP_Test_Delay_Execute);
end;

end.
 
