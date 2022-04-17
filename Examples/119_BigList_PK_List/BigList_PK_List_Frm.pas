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
  // 100万基数
  for i := 1 to 10000 * 100 do
    begin
      L1.Add(i);
    end;
  DoStatus('100万数据基数，追加，List完成时间:%dms', [GetTimeTick - tk]);
  L1.Free;

  tk := GetTimeTick();
  L2 := TMy_Struct_BigList.Create;
  // 100万基数
  for i := 1 to 10000 * 100 do
    begin
      L2.Add(i);
    end;
  DoStatus('100万数据基数，追加，BigList完成时间:%dms', [GetTimeTick - tk]);
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
  // 10万基数
  for i := 1 to 10000 * 10 do
    begin
      if L1.Count > 0 then
          L1.Insert(0, i)
      else
          L1.Add(i);
    end;
  DoStatus('10万数据基数，插入，List完成时间:%dms', [GetTimeTick - tk]);
  L1.Free;

  tk := GetTimeTick();
  L2 := TMy_Struct_BigList.Create;
  // 10万基数
  for i := 1 to 10000 * 10 do
    begin
      if L2.Count > 0 then
          L2.Insert(i, L2.First)
      else
          L2.Add(i);
    end;
  DoStatus('10万数据基数，插入，BigList完成时间:%dms', [GetTimeTick - tk]);
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
  // 10万基数
  for i := 1 to 10000 * 10 do
    begin
      L1.Add(i);
    end;
  tk := GetTimeTick();
  while L1.Count > 0 do
      L1.Delete(0);
  DoStatus('10万数据基数，队列拾取，List完成时间:%dms', [GetTimeTick - tk]);
  L1.Free;

  L2 := TMy_Struct_BigList.Create;
  // 10万基数
  for i := 1 to 10000 * 10 do
    begin
      L2.Add(i);
    end;
  tk := GetTimeTick();
  while L2.Count > 0 do
      L2.Remove(L2.First);
  DoStatus('10万数据基数，队列拾取，BigList完成时间:%dms', [GetTimeTick - tk]);
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
  // 1000万基数
  for i := 1 to 10000 * 1000 do
    begin
      L1.Add(i);
    end;
  tk := GetTimeTick();
  for i := 0 to L1.Count - 1 do
    if L1[i] = 9999999 then
        break;
  DoStatus('1000万数据基数，遍历查找，List完成时间:%dms', [GetTimeTick - tk]);
  L1.Free;

  L2 := TMy_Struct_BigList.Create;
  // 1000万基数
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
  DoStatus('1000万数据基数，遍历查找，BigList完成时间:%dms', [GetTimeTick - tk]);
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
  // 10万基数
  for i := 1 to 10000 * 10 do
    begin
      L1.Add(i);
    end;
  tk := GetTimeTick();
  // List用优化方法，从后向前删
  for i := L1.Count - 1 downto 0 do
    if L1[i] mod 2 = 0 then
        L1.Delete(i);
  DoStatus('10万数据基数，数据删除，List完成时间:%dms', [GetTimeTick - tk]);
  L1.Free;

  L2 := TMy_Struct_BigList.Create;
  // 10万基数
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
  // 释放回收站
  L2.Free_Recycle_Pool;
  DoStatus('10万数据基数，数据删除，BigList完成时间:%dms', [GetTimeTick - tk]);
  L2.Free;
end;

procedure TBigList_PK_List_Form.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

end.
