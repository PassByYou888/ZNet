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
  { This function demonstrates how anti quantum cryptography can be used in TDataFrameEngine }
  { ZS's dataframeeninge can use this method for encryption before sending and receiving }
  { Note: the performance of anti quantum cryptography cannot achieve real-time, so we can choose to encrypt some important data }

  d := TDataFrameEngine.Create;
  d.WriteString('hello world');

  DoStatus('Data before DataFrameEngine encryption:%s', [umlMD5ToStr(d.GetMD5(True)).Text]);

  { Generate key for Rijndael encryption algorithm with password 'password123456' }
  { Then perform 1024 overlapping encryption on the buffer using the Rijndael encryption algorithm }
  { The password will be encoded in 3-512 bits twice }
  { Why is it said that doing so is anti quantum cracking? }
  { Because sha3 is a resistance quantum design, and a secure password ensures that multiple Rijndael encryption is secure }
  { Under a dual security mechanism, the Encrypt method can resist future quantum attacks }
  m64 := TMemoryStream64.Create;
  d.Encrypt(m64, True, 1024, TPascalString('password123456').Bytes);

  d.Clear;
  m64.Position := 0;
  { Without knowing the password, the data in M64 can never be decrypted }
  if d.Decrypt(m64, TPascalString('password123456').Bytes) then
      DoStatus('Successfully decrypted')
  else
      DoStatus('Password error');
  DoStatus('DataFrameEngine decrypted data:%s', [umlMD5ToStr(d.GetMD5(True)).Text]);

  DisposeObject([d, m64]);
end;

procedure QuantumSecurity_Buffer;
var
  sour: TMemoryStream64;
  m64: TMemoryStream64;
begin
  { This function demonstrates how to perform quantum encryption and decryption resistance on stream data }
  { Stream can be used for ZS's completebuffer, bigstream, batchstream and other mechanisms }
  { Note: the performance of anti quantum cryptography cannot achieve real-time, so we can choose to encrypt some important data }
  sour := TMemoryStream64.Create;
  sour.WriteString('hello world');
  DoStatus('Data before buffer encryption:%s', [umlStreamMD5String(sour).Text]);

  m64 := TMemoryStream64.Create;

  sour.Position := 0;
  { Generate key for Rijndael encryption algorithm with password 'password123456' }
  { Then perform 1024 overlapping encryption on the buffer using the Rijndael encryption algorithm }
  { The password will be encoded in 3-512 bits twice }
  { Why is it said that doing so is anti quantum cracking? }
  { Because sha3 is a resistance quantum design, and a secure password ensures that multiple Rijndael encryption is secure }
  { Under a dual security mechanism, the Encrypt method can resist future quantum attacks }
  Z.Cipher.QuantumEncrypt(sour, m64, 1024, TPascalString('password123456').Bytes);

  sour.Clear;
  m64.Position := 0;
  { Without knowing the password, the data in M64 can never be decrypted }
  if Z.Cipher.QuantumDecrypt(m64, sour, TPascalString('password123456').Bytes) then
      DoStatus('Successfully decrypted')
  else
      DoStatus('Password error');
  DoStatus('Data decrypted by buffer:%s', [umlStreamMD5String(sour).Text]);

  DisposeObject([sour, m64]);
end;

begin
  QuantumSecurity_DataFrameEngine;
  QuantumSecurity_Buffer;
  readln;

end.
