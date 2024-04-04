unit _160_C4_Console_Help_Tool_For_UI_Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status, Z.Notify,
  Z.Parsing, Z.Expression, Z.OpCode,
  Z.Net, Z.Net.C4;

type
  T_160_C4_Console_Help_Tool_For_UI_Form = class(TForm)
    CMD_Edit: TLabeledEdit;
    run_Button: TButton;
    Memo: TMemo;
    sysTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CMD_EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sysTimerTimer(Sender: TObject);
    procedure run_ButtonClick(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
    console_help: TC40_Console_Help;
  end;

var
  _160_C4_Console_Help_Tool_For_UI_Form: T_160_C4_Console_Help_Tool_For_UI_Form;

implementation

{$R *.dfm}


procedure T_160_C4_Console_Help_Tool_For_UI_Form.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);
  console_help := TC40_Console_Help.Create;
  console_help.HelpTextStyle := tsPascal;
end;

procedure T_160_C4_Console_Help_Tool_For_UI_Form.FormDestroy(Sender: TObject);
begin
  RemoveDoStatusHook(self);
  DisposeObject(console_help);
end;

procedure T_160_C4_Console_Help_Tool_For_UI_Form.CMD_EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
      run_ButtonClick(run_Button);
end;

procedure T_160_C4_Console_Help_Tool_For_UI_Form.sysTimerTimer(Sender: TObject);
begin
  CheckThread;
end;

procedure T_160_C4_Console_Help_Tool_For_UI_Form.run_ButtonClick(Sender: TObject);
begin
  SysPost.PostExecuteP_NP(0, procedure
    begin
      console_help.IsExit := False;
      console_help.Run_HelpCmd(CMD_Edit.Text);
      CMD_Edit.Text := '';
      if console_help.IsExit then
          Close;
    end);
end;

procedure T_160_C4_Console_Help_Tool_For_UI_Form.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  if Memo.Lines.Count > 5000 then
      Memo.Lines.Clear;
  Memo.Lines.Add(Text_);
end;

end.
