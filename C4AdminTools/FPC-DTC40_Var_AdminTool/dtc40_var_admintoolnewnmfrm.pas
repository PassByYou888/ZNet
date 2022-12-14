unit dtc40_var_admintoolnewnmfrm;

{$mode objFPC}{$H+}
{$MODESWITCH AdvancedRecords}
{$MODESWITCH NestedProcVars}
{$MODESWITCH NESTEDCOMMENTS}
{$NOTES OFF}
{$STACKFRAMES OFF}
{$COPERATORS OFF}
{$GOTO ON}
{$INLINE ON}
{$MACRO ON}
{$HINTS ON}
{$IEEEERRORS ON}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  ActnList, Menus,
  Variants, DateUtils, TypInfo,

  LCLType,

  {$IFDEF FPC}
  Z.FPC.GenericList,
  {$ENDIF FPC}
  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status,
  Z.ListEngine, Z.HashList.Templet, Z.Expression, Z.OpCode, Z.Parsing, Z.DFE, Z.TextDataEngine,
  Z.Json, Z.Geometry2D, Z.Geometry3D, Z.Number,
  Z.MemoryStream, Z.Cipher, Z.Notify, Z.IOThread,
  Z.Net,
  Z.Net.DoubleTunnelIO,
  Z.Net.DoubleTunnelIO.NoAuth,
  Z.Net.DoubleTunnelIO.VirtualAuth,
  Z.Net.DataStoreService,
  Z.Net.DataStoreService.NoAuth,
  Z.Net.DataStoreService.VirtualAuth,
  Z.Net.DataStoreService.Common,
  Z.ZDB.ObjectData_LIB, Z.ZDB, Z.ZDB.Engine, Z.ZDB.LocalManager,
  Z.ZDB.FileIndexPackage_LIB, Z.ZDB.FilePackage_LIB, Z.ZDB.ItemStream_LIB, Z.ZDB.HashField_LIB, Z.ZDB.HashItem_LIB,
  Z.ZDB2, Z.ZDB2.DFE, Z.ZDB2.HS, Z.ZDB2.HV, Z.ZDB2.Json, Z.ZDB2.MS64, Z.ZDB2.NM, Z.ZDB2.TE, Z.ZDB2.FileEncoder,
  Z.Net.C4, Z.Net.C4_UserDB, Z.Net.C4_Var, Z.Net.C4_FS, Z.Net.C4_RandSeed, Z.Net.C4_Log_DB,
  Z.Net.PhysicsIO;

type
  TDTC40_Var_AdminToolNewNMForm = class(TForm)
    NameEdit: TLabeledEdit;
    Label1: TLabel;
    ScriptMemo: TMemo;
    TempCheckBox: TCheckBox;
    LifeTimeEdit: TLabeledEdit;
    CreateNMButton: TButton;
    CancelButton: TButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure CreateNMButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
  public
  end;

var
  DTC40_Var_AdminToolNewNMForm: TDTC40_Var_AdminToolNewNMForm;

implementation

{$R *.lfm}


uses DTC40_Var_AdminToolFrm;

procedure TDTC40_Var_AdminToolNewNMForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TDTC40_Var_AdminToolNewNMForm.CreateNMButtonClick(Sender: TObject);
var
  i: Integer;
  n: U_String;
  nmPool: TC40_Var_Service_NM_Pool;
begin
  if DTC40_Var_AdminToolForm.CurrentClient = nil then
      exit;

  nmPool := DTC40_Var_AdminToolForm.CurrentClient.GetNM(NameEdit.Text);
  for i := 0 to ScriptMemo.Lines.Count - 1 do
    begin
      n := ScriptMemo.Lines[i];
      if n.L > 0 then
        begin
          if nmPool.IsVectorScript(n, tsPascal) then
              nmPool.RunVectorScript(n)
          else
              nmPool.RunScript(n);
        end;
    end;

  if TempCheckBox.Checked then
      DTC40_Var_AdminToolForm.CurrentClient.NM_InitAsTemp(NameEdit.Text, EStrToInt(LifeTimeEdit.Text, 5 * 1000), True, nmPool)
  else
      DTC40_Var_AdminToolForm.CurrentClient.NM_Init(NameEdit.Text, True, nmPool);
end;

procedure TDTC40_Var_AdminToolNewNMForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

end.
