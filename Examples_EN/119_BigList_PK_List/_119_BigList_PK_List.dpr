program _119_BigList_PK_List;

uses
  Vcl.Forms,
  BigList_PK_List_Frm in 'BigList_PK_List_Frm.pas' {BigList_PK_List_Form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TBigList_PK_List_Form, BigList_PK_List_Form);
  Application.Run;
end.
