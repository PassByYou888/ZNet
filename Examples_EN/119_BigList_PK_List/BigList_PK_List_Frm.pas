unit BigList_PK_List_Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.Status;

type
  TBigList_PK_List_Form = class(TForm)
    Memo: TMemo;
    Add_Perf_Button: TButton;
    insert_perf_Button: TButton;
    queue_perf_Button: TButton;
    Traversal_perf_Button: TButton;
    delete_perf_Button: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Add_Perf_ButtonClick(Sender: TObject);
    procedure insert_perf_ButtonClick(Sender: TObject);
    procedure queue_perf_ButtonClick(Sender: TObject);
    procedure Traversal_perf_ButtonClick(Sender: TObject);
    procedure delete_perf_ButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
    { Public declarations }
  end;

  TMy_Struct_List = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<Integer>;
  TMy_Struct_BigList = {$IFDEF FPC}specialize {$ENDIF FPC} TBigList<Integer>;

var
  BigList_PK_List_Form: TBigList_PK_List_Form;

implementation

{$R *.dfm}


procedure TBigList_PK_List_Form.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);
end;

procedure TBigList_PK_List_Form.Add_Perf_ButtonClick(Sender: TObject);
var
  tk: TTimeTick;
  i: Integer;
  L1: TMy_Struct_List;
  L2: TMy_Struct_BigList;
begin
  tk := GetTimeTick();
  L1 := TMy_Struct_List.Create;
  {  1 million base  }
  for i := 1 to 10000 * 100 do
    begin
      L1.Add(i);
    end;
  DoStatus('1 million data base, additional, List completion time:%dms', [GetTimeTick - tk]);
  L1.Free;

  tk := GetTimeTick();
  L2 := TMy_Struct_BigList.Create;
  {  1 million base  }
  for i := 1 to 10000 * 100 do
    begin
      L2.Add(i);
    end;
  DoStatus('1 million data base, additional, BigList completion time:%dms', [GetTimeTick - tk]);
  L2.Free;
end;

procedure TBigList_PK_List_Form.insert_perf_ButtonClick(Sender: TObject);
var
  tk: TTimeTick;
  i: Integer;
  L1: TMy_Struct_List;
  L2: TMy_Struct_BigList;
begin
  tk := GetTimeTick();
  L1 := TMy_Struct_List.Create;
  {  100000 base  }
  for i := 1 to 10000 * 10 do
    begin
      if L1.Count > 0 then
          L1.Insert(0, i)
      else
          L1.Add(i);
    end;
  DoStatus('100000 data base, insert, List completion time:%dms', [GetTimeTick - tk]);
  L1.Free;

  tk := GetTimeTick();
  L2 := TMy_Struct_BigList.Create;
  {  100000 base  }
  for i := 1 to 10000 * 10 do
    begin
      if L2.Count > 0 then
          L2.Insert(i, L2.First)
      else
          L2.Add(i);
    end;
  DoStatus('100000 data base, insert, BigList completion time:%dms', [GetTimeTick - tk]);
  L2.Free;
end;

procedure TBigList_PK_List_Form.queue_perf_ButtonClick(Sender: TObject);
var
  tk: TTimeTick;
  i: Integer;
  L1: TMy_Struct_List;
  L2: TMy_Struct_BigList;
begin
  L1 := TMy_Struct_List.Create;
  {  100000 base  }
  for i := 1 to 10000 * 10 do
    begin
      L1.Add(i);
    end;
  tk := GetTimeTick();
  while L1.Count > 0 do
      L1.Delete(0);
  DoStatus('100000 data base, queue pickup, List completion time:%dms', [GetTimeTick - tk]);
  L1.Free;

  L2 := TMy_Struct_BigList.Create;
  {  100000 base  }
  for i := 1 to 10000 * 10 do
    begin
      L2.Add(i);
    end;
  tk := GetTimeTick();
  while L2.Count > 0 do
      L2.Remove_P(L2.First);
  DoStatus('100000 data base, queue pickup, BigList completion time:%dms', [GetTimeTick - tk]);
  L2.Free;
end;

procedure TBigList_PK_List_Form.Traversal_perf_ButtonClick(Sender: TObject);
var
  tk: TTimeTick;
  i: Integer;
  L1: TMy_Struct_List;
  L2: TMy_Struct_BigList;
begin
  L1 := TMy_Struct_List.Create;
  {  10 million base  }
  for i := 1 to 10000 * 1000 do
    begin
      L1.Add(i);
    end;
  tk := GetTimeTick();
  for i := 0 to L1.Count - 1 do
    if L1[i] = 9999999 then
        break;
  DoStatus('10 million data base, traversal search, List completion time:%dms', [GetTimeTick - tk]);
  L1.Free;

  L2 := TMy_Struct_BigList.Create;
  {  10 million base  }
  for i := 1 to 10000 * 1000 do
    begin
      L2.Add(i);
    end;
  tk := GetTimeTick();
  if L2.Count > 0 then
    with L2.Repeat_ do
      repeat
        if Queue^.Data = 9999999 then
            break;
      until not next;
  DoStatus('10 million data base, traversal search, BigList completion time:%dms', [GetTimeTick - tk]);
  L2.Free;
end;

procedure TBigList_PK_List_Form.delete_perf_ButtonClick(Sender: TObject);
var
  tk: TTimeTick;
  i: Integer;
  L1: TMy_Struct_List;
  L2: TMy_Struct_BigList;
begin
  L1 := TMy_Struct_List.Create;
  {  100000 base  }
  for i := 1 to 10000 * 10 do
    begin
      L1.Add(i);
    end;
  tk := GetTimeTick();
  {  List using optimization methods, deleting from back to front  }
  for i := L1.Count - 1 downto 0 do
    if L1[i] mod 2 = 0 then
        L1.Delete(i);
  DoStatus('100000 data base, data deletion, List completion time:%dms', [GetTimeTick - tk]);
  L1.Free;

  L2 := TMy_Struct_BigList.Create;
  {  100000 base  }
  for i := 1 to 10000 * 10 do
    begin
      L2.Add(i);
    end;
  tk := GetTimeTick();
  if L2.Count > 0 then
    with L2.Repeat_ do
      repeat
        if Queue^.Data mod 2 = 0 then
            L2.Push_To_Recycle_Pool(Queue);
      until not next;
  {  Release Recycle Bin  }
  L2.Free_Recycle_Pool;
  DoStatus('100000 data base, data deletion, BigList completion time:%dms', [GetTimeTick - tk]);
  L2.Free;
end;

procedure TBigList_PK_List_Form.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

end.
