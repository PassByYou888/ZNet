program C4_LFService;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UPascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.ListEngine,
  Z.HashList.Templet,
  Z.Expression,
  Z.OpCode,
  Z.Parsing,
  Z.DFE,
  Z.TextDataEngine,
  Z.MemoryStream,
  Z.Net,
  Z.Net.PhysicsIO,
  Z.Net.C4,
  Z.Net.C4_FS2,
  Z.Net.C4_Console_APP;

var
  exit_signal: Boolean;

procedure Do_Check_On_Exit;
var
  n: string;
  cH: TC40_Console_Help;
begin
  cH := TC40_Console_Help.Create;
  repeat
    TCompute.Sleep(100);
    Readln(n);
    cH.Run_HelpCmd(n);
  until cH.IsExit;
  disposeObject(cH);
  exit_signal := True;
end;

begin
  {  Large scale file transfer services include file databases  }
  {  Do not close the console mode directly  }
  {  This server can directly copy code to the Laz environment to build IOT and Linux systems. Note: LCL cannot be used in fpc console applications and must be a NoUI program. C4 can support the No LCL application environment  }
  Z.Net.C4_Console_APP.C40AppParsingTextStyle := TTextStyle.tsC;
  Z.Net.C4_Console_APP.C40AppParam := [
    Format('Service("0.0.0.0","%s",9188,"DP")', ['127.0.0.1']),
    Format('Service("0.0.0.0","%s",9189,"FS2")', ['127.0.0.1'])
    ];

  DoStatus('Prepare service.');

  if Z.Net.C4_Console_APP.C40_Extract_CmdLine then
    begin
      exit_signal := False;
      TCompute.RunC_NP(@Do_Check_On_Exit);
      while not exit_signal do
          Z.Net.C4.C40Progress;
    end;

  Z.Net.C4.C40Clean;

end.
