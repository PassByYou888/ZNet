unit DTC40_UserDB_AdminLargeScaleRegFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.CheckLst,

  Vcl.FileCtrl,
  System.IOUtils, System.DateUtils, System.TypInfo,

  Z.Core, Z.PascalStrings, Z.UPascalStrings, Z.UnicodeMixedLib, Z.Status,
  Z.ListEngine, Z.GHashList, Z.Expression, Z.OpCode, Z.Parsing, Z.DFE, Z.TextDataEngine,
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
  Z.ZDB2.Custom, Z.ZDB2, Z.ZDB2.DFE, Z.ZDB2.HS, Z.ZDB2.HV, Z.ZDB2.Json, Z.ZDB2.MS64, Z.ZDB2.NM, Z.ZDB2.TE, Z.ZDB2.FileEncoder,
  Z.Net.C4, Z.Net.C4_UserDB, Z.Net.C4_Var, Z.Net.C4_FS, Z.Net.C4_RandSeed, Z.Net.C4_Log_DB,
  Z.Net.PhysicsIO, Z.MediaCenter, Z.GBKMediaCenter, Z.FastGBK, Z.GBK;

type
  TDTC40_UserDB_AdminLargeScaleRegForm = class(TForm)
    Label1: TLabel;
    PlanListView: TListView;
    makePlanButton: TButton;
    cleanPlanButton: TButton;
    executePlanButton: TButton;
    CorpusListBox: TCheckListBox;
    NumEdit: TLabeledEdit;
    Random_Prefix_CheckBox: TCheckBox;
    Label2: TLabel;
    procedure PlanListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
    procedure makePlanButtonClick(Sender: TObject);
    procedure cleanPlanButtonClick(Sender: TObject);
    procedure executePlanButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
  public
    IsBusy: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshCorpus;
  end;

  TReg_Item = class(TListItem)
  public
    oriName, UserName, Passwd: string;
    procedure do_Usr_NewIdentifier(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
    procedure Do_Usr_Reg(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
  end;

var
  DTC40_UserDB_AdminLargeScaleRegForm: TDTC40_UserDB_AdminLargeScaleRegForm;

implementation

{$R *.dfm}


uses DTC40_UserDB_AdminToolFrm;

procedure TReg_Item.do_Usr_NewIdentifier(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
begin
  SubItems[0] := SubItems[0] + info_;
  MakeVisible(True);
end;

procedure TReg_Item.Do_Usr_Reg(Sender: TC40_UserDB_Client; State_: Boolean; info_: SystemString);
begin
  SubItems[0] := info_;

  if State_ then
    begin
      if DTC40_UserDB_AdminToolForm.CurrentClient = nil then
          exit;
      DTC40_UserDB_AdminToolForm.CurrentClient.Usr_NewIdentifierM(UserName, oriName, do_Usr_NewIdentifier);
    end;
end;

procedure TDTC40_UserDB_AdminLargeScaleRegForm.PlanListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
begin
  ItemClass := TReg_Item;
end;

procedure TDTC40_UserDB_AdminLargeScaleRegForm.makePlanButtonClick(Sender: TObject);
begin
  if IsBusy then
      exit;
  IsBusy := True;
  Enabled := False;
  TCompute.RunP_NP(procedure
    var
      HashPool: THashList;
      L: TCore_List;
      i, j: Integer;
      tmp: TPascalStringList;
    begin
      HashPool := THashList.CustomCreate(1024 * 1024);
      HashPool.IgnoreCase := True;
      HashPool.AutoFreeData := False;
      HashPool.AccessOptimization := True;

      for i := 0 to CorpusListBox.Items.Count - 1 do
        if CorpusListBox.Checked[i] then
          begin
            tmp := TPascalStringList.Create;
            tmp.LoadFromStream(DictLibrary.ROOT[CorpusListBox.Items[i]]^.stream);
            for j := 0 to tmp.Count - 1 do
              if length(tmp[j].Bytes) >= 4 then
                  HashPool.Add(T2S(tmp[j]), nil);
            DisposeObject(tmp);
          end;

      TCompute.Sync(procedure
        var
          tmpPool: THashList;
          L: TCore_List;
          itm: TReg_Item;
          p: PHashListData;
          num: Integer;
          af, bf: U_String;
        begin
          MT19937Randomize();
          L := TCore_List.Create;
          tmpPool := THashList.CustomCreate(1024 * 1024);
          HashPool.GetListData(L);

          PlanListView.Items.BeginUpdate;
          PlanListView.Items.Clear;
          num := EStrToInt(NumEdit.Text, 1000);
          while tmpPool.Count < num do
            begin
              p := PHashListData(L[umlRandomRange(0, L.Count - 1)]);
              itm := PlanListView.Items.Add as TReg_Item;
              //
              if Random_Prefix_CheckBox.Checked then
                begin
                  af := TPascalString.RandomString(umlRandomRange(1, 3), [cHiAtoZ]) + '_';
                  bf := '_' + TPascalString.RandomString(umlRandomRange(1, 4), [c0to9, cAtoZ]);
                  itm.oriName := af + p^.OriginName + bf;
                end
              else
                  itm.oriName := p^.OriginName;
              //
              if Random_Prefix_CheckBox.Checked then
                begin
                  af := TPascalString.RandomString(umlRandomRange(1, 3), [cHiAtoZ]) + '_';
                  bf := '_' + TPascalString.RandomString(umlRandomRange(1, 4), [c0to9, cAtoZ]);
                  itm.UserName := af + PyNoSpace(p^.OriginName) + bf;
                end
              else
                  itm.UserName := PyNoSpace(p^.OriginName);
              //
              itm.Passwd := itm.UserName;

              itm.Caption := IntTostr(tmpPool.Count + 1) + ': ' + p^.OriginName + ' = ' + itm.oriName + ' + ' + itm.UserName;
              itm.SubItems.Add('plan...');
              tmpPool.Add(p^.OriginName, nil, False);
              if tmpPool.Count mod 100 = 0 then
                  Application.ProcessMessages;
            end;
          PlanListView.Items.EndUpdate;
          PlanListView.Height := PlanListView.Height - 1;
          PlanListView.Height := PlanListView.Height + 1;
          DisposeObject(L);
          DisposeObject(tmpPool);
          Enabled := True;
        end);

      DisposeObject(HashPool);
      IsBusy := False;
    end);
end;

procedure TDTC40_UserDB_AdminLargeScaleRegForm.cleanPlanButtonClick(Sender: TObject);
begin
  if IsBusy then
      exit;
  PlanListView.Clear;
end;

procedure TDTC40_UserDB_AdminLargeScaleRegForm.executePlanButtonClick(Sender: TObject);
begin
  if IsBusy then
      exit;
  IsBusy := True;
  TCompute.RunP_NP(procedure
    var
      i, num, queue: Integer;
    begin
      TCompute.Sync(procedure
        begin
          PlanListView.Enabled := False;
        end);
      num := PlanListView.Items.Count;
      for i := 0 to num - 1 do
        begin
          repeat
            TCompute.Sync(procedure
              begin
                queue := 0;
                if DTC40_UserDB_AdminToolForm.CurrentClient = nil then
                    exit;
                queue := DTC40_UserDB_AdminToolForm.CurrentClient.DTNoAuth.SendTunnel.QueueCmdCount;
              end);
            TCompute.Sleep(10);
          until queue < 100;
          TCompute.Sync(procedure
            var
              itm: TReg_Item;
            begin
              if DTC40_UserDB_AdminToolForm.CurrentClient = nil then
                  exit;
              itm := PlanListView.Items[i] as TReg_Item;
              DTC40_UserDB_AdminToolForm.CurrentClient.Usr_RegM(itm.UserName, itm.Passwd, itm.Do_Usr_Reg);
            end);
          TCompute.Sleep(10);
        end;
      TCompute.Sync(procedure
        begin
          PlanListView.Enabled := True;
          IsBusy := False;
        end);
    end);
end;

procedure TDTC40_UserDB_AdminLargeScaleRegForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TDTC40_UserDB_AdminLargeScaleRegForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not IsBusy;
end;

constructor TDTC40_UserDB_AdminLargeScaleRegForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsBusy := False;
end;

destructor TDTC40_UserDB_AdminLargeScaleRegForm.Destroy;
begin
  inherited Destroy;
end;

procedure TDTC40_UserDB_AdminLargeScaleRegForm.RefreshCorpus;
var
  i: Integer;
begin
  DictLibrary.ROOT.GetOriginNameList(CorpusListBox.Items);
  for i := 0 to CorpusListBox.Items.Count - 1 do
      CorpusListBox.Checked[i] := True;
end;

end.
