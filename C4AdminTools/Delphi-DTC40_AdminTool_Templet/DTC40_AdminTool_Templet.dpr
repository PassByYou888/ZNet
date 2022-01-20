program DTC40_AdminTool_Templet;

uses
  Vcl.Forms,
  DTC40_AdminTool_Templet_Frm in 'DTC40_AdminTool_Templet_Frm.pas' {DTC40_AdminTool_Templet_Form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDTC40_AdminTool_Templet_Form, DTC40_AdminTool_Templet_Form);
  Application.Run;
end.
