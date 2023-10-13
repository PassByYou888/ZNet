unit MHMainFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Z.Status, Z.PascalStrings, Z.Core, Z.UnicodeMixedLib, Z.ListEngine;

type
  TMHMainForm = class(TForm)
    Memo: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  end;

var
  MHMainForm: TMHMainForm;

implementation

{$R *.dfm}


uses Z.MH1, Z.MH2, Z.MH3, Z.MH;

procedure TMHMainForm.Button1Click(Sender: TObject);

  procedure leakproc(x, m: Integer);
  begin
    GetMemory(x);
    if x > m then
        leakproc(x - 1, m);
  end;

begin
  Z.MH.BeginMemoryHook_1;
  leakproc(100, 98);
  Z.MH.EndMemoryHook_1;

  {  We will find a leak here  }
  DoStatus('The leakproc function allocated%d bytes of memory', [Z.MH.GetHookMemorySize_1]);

  Z.MH.GetHookPtrList_1.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      DoStatus('Leaked address: 0x%s', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2)]);
      DoStatus(NPtr, uData, 80);

      {  Now we can directly release the address  }
      Dispose(NPtr);

      DoStatus('Successfully released address: 0x%s occupied%d bytes of memory', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2), uData]);
    end);
end;

procedure TMHMainForm.Button2Click(Sender: TObject);
type
  PMyRec = ^TMyRec;

  TMyRec = record
    s1: string;
    s2: string;
    s3: TPascalString;
    obj: TObject;
  end;

var
  p: PMyRec;
begin
  Z.MH.BeginMemoryHook_1;
  new(p);
  p^.s1 := #7#8#9;
  p^.s2 := #$20#$20#$20#$20#$20#$20#$20#$20#$20#$20#$20#$20;
  p^.s3.Text := #1#2#3#4#5#6;
  p^.obj := TObject.Create;
  Z.MH.EndMemoryHook_1;

  {  We will find a leak here  }
  DoStatus('Tmyrec allocated %d times of memory and occupied %d bytes of space,', [Z.MH.GetHookPtrList_1.Count, Z.MH.GetHookMemorySize_1]);

  Z.MH.GetHookPtrList_1.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      DoStatus('Leaked address: 0x%s', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2)]);
      DoStatus(NPtr, uData, 80);

      {  Now we can directly release the address  }
      FreeMem(NPtr);

      DoStatus('Successfully released address: 0x%s occupied%d bytes of memory', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2), uData]);
    end);
end;

procedure TMHMainForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(AText);
end;

procedure TMHMainForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
end;

procedure TMHMainForm.Button3Click(Sender: TObject);
type
  PMyRec = ^TMyRec;

  TMyRec = record
    s1: string;
    p: PMyRec;
  end;

var
  p: PMyRec;
  i: Integer;
begin
  {  100000 repeated hooks and releases  }
  {  This scenario can be used to count your program overhead and record memory consumption  }
  for i := 0 to 10 * 10000 do
    begin
      Z.MH2.BeginMemoryHook(4);
      new(p);
      p^.s1 := '12345';
      new(p^.p);
      p^.p^.s1 := '54321';
      Z.MH2.EndMemoryHook;

      Z.MH2.GetHookPtrList.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
        begin
          {  Now we can release the address  }
          FreeMem(NPtr);
        end);
    end;
end;

procedure TMHMainForm.Button4Click(Sender: TObject);
type
  PMyRec = ^TMyRec;

  TMyRec = record
    s1: string;
    p: PMyRec;
  end;

var
  p: PMyRec;
  i: Integer;
  hl: TPointerHashNativeUIntList;
begin
  {  200000 large batch record memory applications, final one-time release  }
  {  This scenario can be used to release leaked memory in batches  }

  {  We built 200000 hash arrays for storage  }
  {  The larger the parameter of beginmemoryhook, the better the performance of high-frequency recording for mass storage, but also the more memory consumption  }
  Z.MH3.BeginMemoryHook(200000);

  for i := 0 to 20 * 10000 do
    begin
      new(p);
      new(p^.p);
      {  Simulate string assignment and trigger realloc call at high frequency  }
      p^.s1 := '111111111111111';
      p^.s1 := '1111111111111111111111111111111111';
      p^.s1 := '11111111111111111111111111111111111111111111111111111111111111';
      p^.p^.s1 := '1';
      p^.p^.s1 := '11111111111111111111';
      p^.p^.s1 := '1111111111111111111111111111111111111';
      p^.p^.s1 := '11111111111111111111111111111111111111111111111111111111111111111111111111';

      if i mod 99999 = 0 then
        begin
          {  Here is the iteration call. We don't record the MH_3. Set memoryhooked to false  }
          Z.MH3.GetMemoryHooked.V := False;
          Button1Click(nil);
          Application.ProcessMessages;
          {  Continue recording memory requests  }
          Z.MH3.GetMemoryHooked.V := True;
        end;
    end;
  Z.MH3.EndMemoryHook;

  DoStatus('Total memory allocation%d times occupying%s space, address span:%s', [Z.MH3.GetHookPtrList.Count, umlSizeToStr(Z.MH3.GetHookMemorySize).Text,
    umlSizeToStr(NativeUInt(Z.MH3.GetHookMemoryMaximumPtr) - NativeUInt(Z.MH3.GetHookMemoryMinimizePtr)).Text]);

  Z.MH3.GetHookPtrList.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      {  Now we can release the address  }
      FreeMem(NPtr);
    end);
  Z.MH3.GetHookPtrList.PrintHashReport;
  Z.MH3.GetHookPtrList.SetHashBlockCount(0);
end;

procedure TMHMainForm.Button5Click(Sender: TObject);

var
  s: string;
  sptr: PString;
begin
  Z.MH1.BeginMemoryHook(16);

  Memo.Lines.Add('123'); {  Because there is no previous or subsequent reference, neither Realloc nor GetMem will be recorded here  }
  s := '12345';           {  Because the s string has already been initialized at the beginning of the call and there is no previous or subsequent reference, the Realloc here will not be recorded  }

  new(sptr); {  The GetMem address of sptr will be recorded here  }
  sptr^ := '123';
  sptr^ := '123456789'; {  When realloc to sptr occurs, MH will look for the previous and subsequent text. If the realloc recording conditions are met, MH will record it and release it later  }

  {  Mh supports control creation and release  }
  {  MH does not support the release of the tform window, because the tform window will register global parameters. After MH releases the tform, some callbacks will report an error if there is no address  }
  TButton.Create(Self).Free;

  Z.MH1.EndMemoryHook;

  Z.MH1.GetHookPtrList.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      {  Now we can release the address  }
      DoStatus(NPtr, uData, 80);
      FreeMem(NPtr);
    end);

  Z.MH1.GetHookPtrList.SetHashBlockCount(0);
end;

end.

 
