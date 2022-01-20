program PRP;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UPascalStrings,
  Z.UnicodeMixedLib,
  Z.Parsing,
  Z.MemoryStream,
  Z.Status,
  Z.Pascal_Code_Tool,
  Z.Pascal_Rewrite_Model_Data;

procedure DoHelp;
begin
  DoStatus('PRP.exe help.');
  DoStatus('');
  DoStatus('PRP.exe -H -P -T -M -R -D');
  DoStatus('');
  DoStatus('"-H" help info.');
  DoStatus('"-P:ON/OFF" parallel switch,default is ON');
  DoStatus('"-T:Num" parallel thread number,default is %d', [Z.Core.GetParallelGranularity]);
  DoStatus('"-M:file" Model file based on ZDB2 Engine.The PassByYou888 model will be used by default');
  DoStatus('"-R" reverse model');
  DoStatus('"-D:Directory" Recursive processing of directories and subdirectories. support files: *.pas *.pp *.dpr *.lpr *.inc *.res');
  DoStatus('');
  DoStatus('custom my pascal library examples, hint: Make model "DefaultModel.OX2" open "pascalrewritemodel.exe"');
  DoStatus('PRP -P:ON -T:%d "-M:.\DefaultModel.OX2" "-D:%s"', [Z.Core.GetParallelGranularity, umlCurrentPath.Text]);
  DoStatus('');
  DoStatus('UpLevel my pascal library to PassByYou888 newest code examples:');
  DoStatus('PRP -P:ON -T:%d "-D:%s"', [Z.Core.GetParallelGranularity, umlCurrentPath.Text]);
  DoStatus('');
  DoStatus('DownLevel my pascal library examples:');
  DoStatus('PRP -P:ON -T:%d -R "-D:%s"', [Z.Core.GetParallelGranularity, umlCurrentPath.Text]);
  DoStatus('');
end;

procedure Fill_CMD;
var
  i: Integer;
  n: U_String;
  H_: Boolean;
  parallel_: Boolean;
  ThNum_: Integer;
  Model_: U_String;
  Reverse_: Boolean;
  Dir_: U_String;
  model_stream: TMS64;
begin
  H_ := False;
  parallel_ := True;
  ThNum_ := Z.Core.GetParallelGranularity;
  Model_ := '';
  Reverse_ := False;
  Dir_ := '';

  for i := 1 to ParamCount do
    begin
      n := ParamStr(i);
      if umlMultipleMatch(['-H', '/H', '-Help', '/Help'], n) then
          H_ := True
      else if umlMultipleMatch(['-P:*', '/P:*'], n) then
          parallel_ := umlStrToBool(umlDeleteFirstStr(n, ': '))
      else if umlMultipleMatch(['-T:*', '/T:*'], n) then
          ThNum_ := umlStrToInt(umlDeleteFirstStr(n, ': '))
      else if umlMultipleMatch(['-M:*', '/M:*'], n) then
          Model_ := umlDeleteFirstStr(n, ': ')
      else if umlMultipleMatch(['-R', '/R'], n) then
          Reverse_ := True
      else if umlMultipleMatch(['-D:*', '/D:*'], n) then
          Dir_ := umlDeleteFirstStr(n, ': ');
    end;

  if not umlDirectoryExists(Dir_) then
    begin
      DoHelp;
      exit;
    end;

  model_stream := TMS64.Create;
  if umlFileExists(Model_) then
      model_stream.LoadFromFile(Model_)
  else
      Get_PassByYou888UpLevelModel_Stream(model_stream);

  TCompute.RunP_NP(procedure
    begin
      RewritePascal_ProcessDirectory(parallel_, Dir_, model_stream, Reverse_,
        procedure(const Fmt: SystemString; const Args: array of const)
        begin
          DoStatus(Fmt, Args);
        end);
    end);

  while TCompute.TotalTask > 0 do
      CheckThread(1);

  DisposeObject(model_stream);
  DoStatus('done.');
end;

begin
  Fill_CMD;

end.
