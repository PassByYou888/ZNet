program QuantumEncryptDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Z.Core,
  Z.PascalStrings,
  Z.UnicodeMixedLib,
  Z.Status,
  Z.MemoryStream,
  Z.DFE,
  Z.Cipher;

procedure QuantumSecurity_DataFrameEngine;
var
  d: TDataFrameEngine;
  m64: TMemoryStream64;
begin
  // �ú���ʾ���˿���������������TDataFrameEngine��ʹ��
  // ZS��DataFrameEninge���շ�ǰ������ʹ�ø÷������м���
  // ע�⣺���������������޷��ﵽʵʱ�����ǿ���ѡ�����һЩ��Ҫ������

  d := TDataFrameEngine.Create;
  d.WriteString('hello world');

  DoStatus('DataFrameEngine ����ǰ������:%s', [umlMD5ToStr(d.GetMD5(True)).Text]);

  // ������"password123456"���� Rijndael �����㷨��key
  // Ȼ����Rijndael�����㷨�Ի�������1024���ص�����
  // ���뽫����2��sha3-512λ���б���
  // Ϊʲô˵��ô���ǿ������ƽ�ģ�
  // ��Ϊsha3���ǿ���������ƣ�����ȫ��������֤�˶��Rijndael�����ǰ�ȫ��
  // ��˫�ذ�ȫ�����£�Encrypt�������Ե���δ�������ӹ���
  m64 := TMemoryStream64.Create;
  d.Encrypt(m64, True, 1024, TPascalString('password123456').Bytes);

  d.Clear;
  m64.Position := 0;
  // �ڲ�֪�����������£�m64�е�������Զ�����ܱ����ܳ���
  if d.Decrypt(m64, TPascalString('password123456').Bytes) then
      DoStatus('�ɹ�����')
  else
      DoStatus('�������');
  DoStatus('DataFrameEngine ���ܺ������:%s', [umlMD5ToStr(d.GetMD5(True)).Text]);

  DisposeObject([d, m64]);
end;

procedure QuantumSecurity_Buffer;
var
  sour: TMemoryStream64;
  m64: TMemoryStream64;
begin
  // �ú���ʾ����������stream���ݽ��п����Ӽӽ���
  // stream��������zs��completeBuffer,bigStream,BatchStream�ȵȻ�����
  // ע�⣺���������������޷��ﵽʵʱ�����ǿ���ѡ�����һЩ��Ҫ������
  sour := TMemoryStream64.Create;
  sour.WriteString('hello world');
  DoStatus('buffer ����ǰ������:%s', [umlStreamMD5String(sour).Text]);

  m64 := TMemoryStream64.Create;

  sour.Position := 0;
  // ������"password123456"���� Rijndael �����㷨��key
  // Ȼ����Rijndael�����㷨�Ի�������1024���ص�����
  // ���뽫����2��sha3-512λ���б���
  // Ϊʲô˵��ô���ǿ������ƽ�ģ�
  // ��Ϊsha3���ǿ���������ƣ�����ȫ��������֤�˶��Rijndael�����ǰ�ȫ��
  // ��˫�ذ�ȫ�����£�Encrypt�������Ե���δ�������ӹ���
  Z.Cipher.QuantumEncrypt(sour, m64, 1024, TPascalString('password123456').Bytes);

  sour.Clear;
  m64.Position := 0;
  // �ڲ�֪�����������£�m64�е�������Զ�����ܱ����ܳ���
  if Z.Cipher.QuantumDecrypt(m64, sour, TPascalString('password123456').Bytes) then
      DoStatus('�ɹ�����')
  else
      DoStatus('�������');
  DoStatus('buffer ���ܺ������:%s', [umlStreamMD5String(sour).Text]);

  DisposeObject([sour, m64]);
end;

begin
  QuantumSecurity_DataFrameEngine;
  QuantumSecurity_Buffer;
  readln;

end.
