unit C4_VAR_Tech_Demo_Serv_Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,

  Z.Core, Z.PascalStrings, Z.Status, Z.UnicodeMixedLib, Z.ListEngine,
  Z.Geometry2D, Z.DFE, Z.Json, Z.Number,
  Z.Notify, Z.Cipher, Z.MemoryStream,
  Z.Net, Z.Net.PhysicsIO, Z.Net.C4, Z.Net.C4_Var;

type
  TC4_VAR_Tech_Demo_Serv_Form = class(TForm)
    netTimer: TTimer;
    topPanel: TPanel;
    AddrEdit: TLabeledEdit;
    PortEdit: TLabeledEdit;
    ServiceTypeEdit: TLabeledEdit;
    buildNetworkButton: TButton;
    cliPanel: TPanel;
    LSplitter: TSplitter;
    LPanel: TPanel;
    RPanel: TPanel;
    TreeView: TTreeView;
    Memo: TMemo;
    UpdateStateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure netTimerTimer(Sender: TObject);
    procedure buildNetworkButtonClick(Sender: TObject);
    procedure UpdateStateTimerTimer(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
    procedure Do_NMPool_Remove(Sender: TC40_Var_Service; NMPool_: TC40_Var_Service_NM_Pool);
  public
  end;

function GetPathTreeNode(Value_, Split_: string; Tree_: TTreeView; RootNode_: TTreeNode): TTreeNode;

var
  C4_VAR_Tech_Demo_Serv_Form: TC4_VAR_Tech_Demo_Serv_Form;

implementation

{$R *.dfm}


function GetPathTreeNode(Value_, Split_: string; Tree_: TTreeView; RootNode_: TTreeNode): TTreeNode;
var
  i: Integer;
  Postfix_: string;
begin
  Postfix_ := umlGetFirstStr(Value_, Split_);
  if Value_ = '' then
      Result := RootNode_
  else if RootNode_ = nil then
    begin
      if Tree_.Items.Count > 0 then
        begin
          for i := 0 to Tree_.Items.Count - 1 do
            begin
              if (Tree_.Items[i].Parent = RootNode_) and (umlMultipleMatch(True, Postfix_, Tree_.Items[i].Text)) then
                begin
                  Result := GetPathTreeNode(umlDeleteFirstStr(Value_, Split_), Split_, Tree_, Tree_.Items[i]);
                  Result.Expand(False);
                  exit;
                end;
            end;
        end;
      Result := Tree_.Items.AddChild(RootNode_, Postfix_);
      with Result do
        begin
          ImageIndex := -1;
          StateIndex := -1;
          SelectedIndex := -1;
          Data := nil;
        end;
      Result := GetPathTreeNode(umlDeleteFirstStr(Value_, Split_), Split_, Tree_, Result);
    end
  else
    begin
      if (RootNode_.Count > 0) then
        begin
          for i := 0 to RootNode_.Count - 1 do
            begin
              if (RootNode_.Item[i].Parent = RootNode_) and (umlMultipleMatch(True, Postfix_, RootNode_.Item[i].Text)) then
                begin
                  Result := GetPathTreeNode(umlDeleteFirstStr(Value_, Split_), Split_, Tree_, RootNode_.Item[i]);
                  Result.Expand(False);
                  exit;
                end;
            end;
        end;
      Result := Tree_.Items.AddChild(RootNode_, Postfix_);
      with Result do
        begin
          ImageIndex := -1;
          StateIndex := -1;
          SelectedIndex := -1;
          Data := nil;
        end;
      Result := GetPathTreeNode(umlDeleteFirstStr(Value_, Split_), Split_, Tree_, Result);
    end;
end;

procedure TC4_VAR_Tech_Demo_Serv_Form.FormCreate(Sender: TObject);
begin
  {  Initialize the entry password of the SAAS network, which is quantum resistant during transmission, but in the executable file, clear text can still be obtained through reverse analysis  }
  {  The password is also the only barrier to server security. If the executable file is not leaked when using the C4 network, the C4 network is secure  }
  Z.Net.C4.C40_Password := '123456';
  {  Hook up statusio for status printing  }
  AddDoStatusHook(self, DoStatus_backcall);
end;

procedure TC4_VAR_Tech_Demo_Serv_Form.FormDestroy(Sender: TObject);
begin
  Z.Net.C4.C40Clean;
  RemoveDoStatusHook(self);
end;

procedure TC4_VAR_Tech_Demo_Serv_Form.netTimerTimer(Sender: TObject);
begin
  Z.Net.C4.C40Progress;
end;

procedure TC4_VAR_Tech_Demo_Serv_Form.buildNetworkButtonClick(Sender: TObject);
var
  arry: TC40_Custom_Service_Array;
  i: Integer;
begin
  with TC40_PhysicsService.Create(AddrEdit.Text, umlStrToInt(PortEdit.Text), Z.Net.PhysicsIO.TPhysicsServer.Create) do
    begin
      AutoFreePhysicsTunnel := True;
      BuildDependNetwork(ServiceTypeEdit.Text);
      StartService;
    end;
  arry := C40_ServicePool.GetFromServiceTyp('var');
  for i := 0 to length(arry) - 1 do
      TC40_Var_Service(arry[i]).OnRemove := Do_NMPool_Remove;
end;

procedure TC4_VAR_Tech_Demo_Serv_Form.UpdateStateTimerTimer(Sender: TObject);
var
  arry: TC40_Custom_Service_Array;
begin
  arry := C40_ServicePool.GetFromServiceTyp('var');
  if length(arry) = 0 then
      exit;
  TC40_Var_Service(arry[0]).NMBigPool.ProgressP(procedure(const NMPoolName_: PSystemString; NMPool_: TC40_Var_Service_NM_Pool)
    begin
      NMPool_.List.ProgressP(procedure(const NMName_: PSystemString; NM_: TNumberModule)
        var
          RN: TTreeNode;
        begin
          RN := GetPathTreeNode(Format('%s/%s', [NMPoolName_^, NMName_^]), '/', TreeView, nil);
          if RN.Count = 0 then
              GetPathTreeNode(Format('%s', [NM_.AsString]), '/', TreeView, RN)
          else
              RN[0].Text := Format('%s', [NM_.AsString]);
        end);
    end);
end;

procedure TC4_VAR_Tech_Demo_Serv_Form.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

procedure TC4_VAR_Tech_Demo_Serv_Form.Do_NMPool_Remove(Sender: TC40_Var_Service; NMPool_: TC40_Var_Service_NM_Pool);
var
  RN: TTreeNode;
begin
  RN := GetPathTreeNode(NMPool_.Name, '/', TreeView, nil);
  RN.Delete;
end;

end.
