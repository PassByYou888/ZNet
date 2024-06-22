unit _161_Hash_Time_Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  DateUtils,

  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib,
  Z.HashList.Templet,
  Z.HashHours.Templet, // 时序数据结构模板:以小时为分割单位
  Z.HashMinutes.Templet, // 时序数据结构模板:以分钟为分割单位
  Z.Status, Z.Notify, Vcl.ComCtrls;

type
  TMy_Data = record
    text_: U_String; // 数据结构体,这里使用的U_String没有引用计数,安全存储
  end;

  PMy_Data = ^TMy_Data;

  // 时序结构体是走算法路线的高级泛型结构,在6代监控的数据中心后台被大量用于含有时间的查询加速,这些查询都以10亿为起步规模
  // 时序结构体有数据体匹配机制,例如删除数据时可以直接以数据体来删除,内部会自动删除时序序列中的这条数据
  // 例如"abc"这条结构体对应最近7天的数据,只需要以abc的可匹配指针来干
  // 数据体避免直接给结构,应使用指针和类
  // 另外,时序结构体同时也支持以时间范围来删除
  TMy_Time_Pool = THours_Buffer_Pool<PMy_Data>;

  T_161_Hash_Time_Form = class(TForm)
    bTime_Edit: TLabeledEdit;
    eTime_Edit: TLabeledEdit;
    data_Edit: TLabeledEdit;
    Memo: TMemo;
    Add_Range_Button: TButton;
    Add_bTime_Button: TButton;
    Query_Button: TButton;
    Add_Random_Range_Button: TButton;
    sysTimer: TTimer;
    For_Data_Button: TButton;
    procedure sysTimerTimer(Sender: TObject);
    procedure Query_ButtonClick(Sender: TObject);
    procedure Add_Range_ButtonClick(Sender: TObject);
    procedure Add_bTime_ButtonClick(Sender: TObject);
    procedure Add_Random_Range_ButtonClick(Sender: TObject);
    procedure For_Data_ButtonClick(Sender: TObject);
  private
    procedure DoStatus_backcall(text_: SystemString; const ID: Integer);
  public
    My_Time_Pool: TMy_Time_Pool;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Do_Time_Pool_State;
  end;

var
  _161_Hash_Time_Form: T_161_Hash_Time_Form;

implementation

{$R *.dfm}


procedure T_161_Hash_Time_Form.sysTimerTimer(Sender: TObject);
begin
  CheckThread;
end;

procedure T_161_Hash_Time_Form.Query_ButtonClick(Sender: TObject);
begin
  TCompute.RunP_NP(procedure
    var
      n: U_String;
    begin
      with My_Time_Pool.Search_Span(umlDT(bTime_Edit.Text), umlDT(eTime_Edit.Text)) do
        begin
          if Num > 0 then
            with Repeat_ do
              repeat
                  n := queue^.Data^.text_;
              until not next;
          DoStatus('全部查询完成,找到%d条符合时间范围的结果.', [Num]);
          Free;
        end;
    end);
end;

procedure T_161_Hash_Time_Form.Add_Range_ButtonClick(Sender: TObject);
begin
  TCompute.RunP_NP(procedure
    var
      p: PMy_Data;
    begin
      new(p);
      p^.text_ := data_Edit.Text;

      // 以时间跨度增加一条数据
      // 如果时间跨度为1天,内部数据条目=24*60
      // 线程安全
      My_Time_Pool.Add_Span(umlDT(bTime_Edit.Text), umlDT(eTime_Edit.Text), p);
      Do_Time_Pool_State;
    end);
end;

procedure T_161_Hash_Time_Form.Add_bTime_ButtonClick(Sender: TObject);
begin
  TCompute.RunP_NP(procedure
    var
      p: PMy_Data;
    begin
      new(p);
      p^.text_ := data_Edit.Text;

      // 以起始时间跨度增加一条数据
      // 线程安全
      My_Time_Pool.Add_Span(umlDT(bTime_Edit.Text), p);
      Do_Time_Pool_State;
    end);
end;

procedure T_161_Hash_Time_Form.Add_Random_Range_ButtonClick(Sender: TObject);
begin
  TCompute.RunP_NP(procedure
    var
      tt: TDateTime;
      i: Integer;
      p: PMy_Data;
      bDT, eDT: TDateTime;
      tk: TTimeTick;
    begin
      MT19937Randomize();
      tt := umlDT(bTime_Edit.Text);
      tk := GetTimeTick;
      for i := 1 to 100 * 10000 do
        begin
          bDT := IncMinute(tt, umlRR(-10000000, 10000000));
          eDT := IncMinute(bDT, umlRR(-10, 10));
          new(p);
          p^.text_ := umlDT(bDT) + ' -> ' + umlDT(eDT);
          My_Time_Pool.Add_Span(bDT, eDT, p);

          if i mod 1000 = 0 then
            if GetTimeTick - tk > 1000 then
              begin
                Do_Time_Pool_State;
                tk := GetTimeTick;
              end;
        end;
      Do_Time_Pool_State;
    end);
end;

procedure T_161_Hash_Time_Form.For_Data_ButtonClick(Sender: TObject);
begin
  TCompute.RunP_NP(procedure
    var
      n: U_String;
    begin
      // 对数据体进行遍历
      My_Time_Pool.Critical.Lock;
      if My_Time_Pool.Time_Data_Pool.Num > 0 then
        with My_Time_Pool.Time_Data_Pool.Repeat_ do
          repeat
              n := queue^.Data^.Data.Primary^.text_;
          until not next;
      DoStatus('遍历了%d条数据', [My_Time_Pool.Time_Data_Pool.Num]);
      My_Time_Pool.Critical.UnLock;
    end);
end;

procedure T_161_Hash_Time_Form.DoStatus_backcall(text_: SystemString; const ID: Integer);
begin
  if Memo.Lines.Count > 1000 then
    begin
      Memo.Lines.BeginUpdate;
      Memo.Lines.Clear;
      Memo.Lines.EndUpdate;
    end;
  Memo.Lines.Add(text_);
end;

constructor T_161_Hash_Time_Form.Create(AOwner: TComponent);
begin
  inherited;
  AddDoStatusHook(self, DoStatus_backcall);

  // 影响内存开销,实际使用时缓冲越大,面对数亿条目提速越明显
  My_Time_Pool := TMy_Time_Pool.Create($FFFF); // 当数据量多,Hash长度可以给大,有提速作用
  My_Time_Pool.Level_2_Hash_Size := 200;

  bTime_Edit.Text := umlDT(IncMinute(Now, -50));
  eTime_Edit.Text := umlDT(Now);
end;

destructor T_161_Hash_Time_Form.Destroy;
begin
  RemoveDoStatusHook(self);
  DisposeObject(My_Time_Pool);
  inherited;
end;

procedure T_161_Hash_Time_Form.Do_Time_Pool_State;
begin
  DoStatus('时序数据实例:%d 时间锚数量:%d 跨度缓冲池:%d', [My_Time_Pool.Total, My_Time_Pool.Num, My_Time_Pool.Buffered_Time_Num]);
end;

end.
