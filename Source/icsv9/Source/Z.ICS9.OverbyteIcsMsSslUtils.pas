{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  MS crypto API utilities. These allow checking and validation of
              SSL certificates using the Windows root certificate store to
              avoid applications needing to include their own file of root
              PEM certificates, also includes certificate Revocation checks,
              warning these are slow since they need to access remote web sites.
              See sample OverbyteIcsMsVerify for usage and demos
Creation:     May 2011
Version:      V9.0
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2023 by Arno Garrels, Berlin

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.


History:
June 2015 - V8.00 Angus moved to main source dir
                  now using OverbyteIcsWinCrypt and OverbyteIcsCryptUiApi
Sep 25, 2018 V8.57 Updated to build with FMX only with SSL and windows.
Feb 18, 2021 V8.66 Renamed all OpenSSL functions to original names removing ICS
                     f_ prefix.
                   Added support for YuOpenSSL which provides OpenSSL in a pre-built
                     DCU linked into applications, rather than using external DLLs.
Sep 02, 2021 V8.67 Added new class TMsX509List descends from TX509List adding a method
                     LoadFromStore to load the list from a Windows certificate store
                     by store name TMsCertStore and location MsCertLocation.  Option
                     to not empty list first, allowing multiple stores to be loaded
                     into the same list.  For My/Personal store, attempts to load
                     private keys if they are allowed to be exported unencrypted.
                   TMsX509List also has ListPKeys method to list content of private
                     key stores which are independent of certificates, DeleteFromStore
                     to delete one certificate by SHA1 digest, and DeletePKey to remove
                     a private keys from the stores by key name.
                   Added new class TMsCertTools descends from TSslCertTools adding
                     methods SaveToStorePfx and LoadFromMyStore to access Windows certificate
                     stores.  SaveToStorePfx saves the loaded certificate and private key
                     to My/Personal store and any intermediates to the CA store. Note this
                     method store private keys so they can be exported from Windows.
                     LoadFromMyStore loads one certificate and private key from
                     My/Personal store and the rest as intermediate, not ideal if more
                     than one certificate.  Demos in OverbyteIcsPemtool.
                   Added IcsIsProgAdmin function does program have administrator access.
Apr 05, 2022 V8.69 Added MsKeyProvider types and constants for use with TPM and Smartkey
                     providers.
Jul 27, 2023 V8.71 Moved IcsIsProgAdmin to Utils for use without SSL.
                   LoadFromStore is now able to read EC keys as well as RSA, using a lot
                     code contributed by uso that marks them exportable which the Windows
                     GUI fails to do, many thanks for the effort.
                   Removed LoadFromMyStore from TMsCertTools, not really useful.
                   Added FindBest method to TMsX509List which finds a server certificate with
                     a private key matching a search string, checking common name, part friendly
                     name or any alternate domain name, selecting the one with the latest expiry
                     if more than one.
                   Added LoadOneFromStore method to TMsCertTools which is used by IcsHosts to find a
                     server certificate bundle from the MsLocCurUser or MsLocMachine My/Personal
                     store, searched using FindBest including * for wildcard, loads the certificate
                     and private key from the store then searches for required intermediate
                     certificates in the MsStoreCA store and adds up to two.
                   Implemented Assign in TMsCertTools, thanks to uso.
Aug 08, 2023 V9.0  Updated version to major release 9.


Note: reading private keys from the Windows Store and loading certificates into the
Windows Store requires the application to have administrator access.

Note: this application should be able to read private keys from the Windows Store that
have been loaded so certificates can be exported from Windows, it will not read keys
in external hardware devices, and probably not in Trusted Platform Modules either.



Pending - load simple certificate into any store.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsMsSslUtils;
{$ENDIF}

{$I include\Z.ICS9.OverbyteIcsDefs.inc}

interface

{$IFDEF MSWINDOWS}
{$IFDEF USE_SSL}

uses
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Sysutils{$ELSE}Sysutils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SysConst{$ELSE}SysConst{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
  Z.ICS9.OverbyteIcsWinCrypt,
  Z.ICS9.OverbyteIcsCryptUiApi,
{$IFDEF FMX}
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocket,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslX509Utils,
{$ELSE}
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsSslX509Utils,
{$ENDIF FMX}
  Z.ICS9.OverbyteIcsMimeUtils,
  Z.ICS9.OverbyteIcsUtils,
  Z.ICS9.OverbyteIcsTypes,   { V8.71 }
  Z.ICS9.OverbyteIcsLIBEAY,
  Z.ICS9.OverbyteIcsSSLEAY
{$IFDEF YuOpenSSL}, YuOpenSSL{$ENDIF YuOpenSSL};

type
  EMsCrypto = class(Exception);

  EMsCertChainEngine = Exception;

  TMsVerifyOptions = set of (mvoRevocationCheckEndCert,
                             mvoRevocationCheckChain,
                             mvoRevocationCheckChainExcludeRoot,
                             mvoRevocationCheckCacheOnly,
                             mvoRevocationAccumulativeTimeOut,
                             mvoCertChainDisableAuthRootAutoUpdate);

  TMsCertChainEngine = class(TObject)
  private
    FhCertChainEngine         : HCERTCHAINENGINE;
    FCertEngineConfig         : TCertChainEngineConfig;
    FEnhkeyUsage              : CERT_ENHKEY_USAGE;
    FCertUsage                : TCertUsageMatch;
    FChainPara                : TCertChainPara;
    FCurCertCtx               : PCCERT_CONTEXT;
    FhCurTempStore            : HCERTSTORE;
    FUrlRetrievalTimeoutMsec  : LongWord;
    FVerifyOptions            : TMsVerifyOptions;
    FMsVerifyOptions          : LongWord;
    FhCAStore                 : HCERTSTORE;
    FhRootStore               : HCERTSTORE;
    procedure Init;
    procedure Deinit;
    procedure SetUrlRetrievalTimeoutMsec(const Value: LongWord);
    procedure SetVerifyOptions(const Value: TMsVerifyOptions);
    function  InternalViewCert(ACertCtx: PCCERT_CONTEXT; AHwnd: HWND;
      AEnableAddToStore: Boolean; ATitle: PChar): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function VerifyCert(ACert: TX509Base; ACertChain: TX509List;
      out AChainVerifyResult: LongWord; AUpdateCertChain: Boolean = TRUE): Boolean;
    function ViewCertLastVerified(AHwnd: HWND = 0;
      ADlgEnableAddToStore: Boolean = True; ATitle: PChar = nil): Boolean;
    function ViewCertVerified(ACert: TX509Base; AHwnd: HWND = 0;
      ADlgEnableAddToStore: Boolean = True; ATitle: PChar = nil): Boolean;
    function ViewCert(ACert: TX509Base; ACertChain: TX509List;
      AHwnd: HWND = 0; ADlgEnableAddToStore: Boolean = True;
      ATitle: PChar = nil): Boolean;
    property UrlRetrievalTimeoutMsec: LongWord
                                           read  FUrlRetrievalTimeoutMsec
                                           write SetUrlRetrievalTimeoutMsec;
    property VerifyOptions: TMsVerifyOptions
                                           read  FVerifyOptions
                                           write SetVerifyOptions;
  end;

{ V8.67 a list of certificates from the Microsoft Windows certificate store }
{ https://docs.microsoft.com/en-us/windows/win32/api/wincrypt/ }

type
    TMsCertStoreType = (MsStoreMy, MsStoreRoot, MsStoreTrust, MsStoreCA, MsStoreAddr, MsStoreSPC);
    TMsCertLocation = (MsLocCurUser, MsLocMachine, MsLocServices, MsLocCurServ, MsLocUsers);
    TMsKeyProvider = (MsKeySoftware, MsKeySmartcard, MsKeyTPM);                 { V8.69 }

    TStorePkeyInfo = record
      KeyName: String;
      KeyAlg: String;
      KeyType: DWORD;
      KeyFlags: DWORD;
  end;
  TStorePkeyInfos = array of TStorePkeyInfo;

const
    MsCertStoreNames: array [TMsCertStoreType] of LPCWSTR = (
              'My','Root','Trust','CA','AddressBook','SPC');
    MsKeyProviderNames: array [TMsKeyProvider] of LPCWSTR = (
                MS_KEY_STORAGE_PROVIDER, MS_SMART_CARD_KEY_STORAGE_PROVIDER,
                MS_PLATFORM_CRYPTO_PROVIDER);                                     { V8.69 }
    MsCertLocNames: array [TMsCertLocation] of LongWord = (
               CERT_SYSTEM_STORE_CURRENT_USER, CERT_SYSTEM_STORE_LOCAL_MACHINE,
               CERT_SYSTEM_STORE_SERVICES, CERT_SYSTEM_STORE_CURRENT_SERVICE,
               CERT_SYSTEM_STORE_USERS);
    MsCertStoreTitles: array [TMsCertStoreType] of PChar = (
              'Personal Store','Trusted Root CAs','Enterprise Trust',
              'Certifcation Authorities','Trusted People', 'Publishers');
    MsKeyProviderTitles: array [TMsKeyProvider] of PChar = (
               'Software Key Provider','Smartcard Key Provider','Trusted Platform Module');    { V8.69 }
    MsCertLocTitles: array [TMsCertLocation] of PChar = (
                'Current User', 'Local Machine','Services',
                'Current Service','Users');
    InternalPW = 'password';

type
{ V8.67 component holding a list of SSL/TLS certificates }
  TMsX509List  = class(TX509List)
  private
  protected
  public
    MsStoreProvider: LPCSTR;
    MsKeyProvider: LPCWSTR;    { V8.68 }
    MsLastError: String;
    function LoadFromStore(MsCertStoreType: TMsCertStoreType; MsCertLocation: TMsCertLocation;
                                                                Empty: Boolean = True): Integer;
    function DeleteFromStore(Sha1Digest: THashBytes20; MsCertStoreType: TMsCertStoreType;
                                        MsCertLocation: TMsCertLocation; DelPKey: Boolean): Boolean;
    function ListPKeys(MsCertLocation: TMsCertLocation; var StorePkeyInfos: TStorePkeyInfos): Integer;
    function DeletePKey(const KeyName: String; MsCertLocation: TMsCertLocation): Boolean;
    function FindBest(const Search: String; Pkey: Boolean = True): Integer;                { V8.71 }
  end;

{ V8.67 component to create SSL certificates and keys }
  TMsCertTools = class(TSslCertTools)
  private
  protected
  public
    MsStoreProvider: LPCSTR;
    MsKeyProvider: LPCWSTR;    { V8.68 }
    procedure Assign(Source: TPersistent); override;  { V8.71 }
//  function LoadFromMyStore(MsCertLocation: TMsCertLocation): Integer;   V8.71 gone, not useful
//    procedure SaveToStoreNc(MsCertLocation: TMsCertLocation; IncludePKey, IncludeInters: Boolean);  not sure it worked
    function LoadOneFromStore(MsCertLocation: TMsCertLocation; const Search: String; Pkey: Boolean = True): String;    { V8.71 }
    procedure SaveToStorePfx(MsCertLocation: TMsCertLocation; IncludePKey, IncludeInters: Boolean);
  end;


  function X509ToMsCert(x: PX509): PCCERT_CONTEXT;
  function MsCertToX509(x: PCCERT_CONTEXT): PX509;
  function MsChainVerifyErrorToStr(const ErrCode: LongWord): string;
  function MsCertVerifyErrorToStr(const ErrCode: LongWord): string;
//  function IcsIsProgAdmin: Boolean;   { V8.67 }

{$ENDIF} // MSWINDOWS
{$ENDIF} // USE_SSL}

implementation

{$IFDEF MSWINDOWS}
{$IFDEF USE_SSL}

const
  RGB_SALT_SIZE = 8;

type
  CRYPT_PKCS12_PBE_PARAMS = record
    iIterations: Integer;
    cbSalt: Integer;
  end;

  PBE_PARAMS = record
    Params: CRYPT_PKCS12_PBE_PARAMS;
    rgbSalt: array [0..(RGB_SALT_SIZE-1)] of Byte;
  end;

{ old compilers happy don't like modern records
  PBE_PARAMS = record
  const
    RgbSaltSize: Integer = RGB_SALT_SIZE;
  var
    Params: CRYPT_PKCS12_PBE_PARAMS;
    rgbSalt: array [0..(RGB_SALT_SIZE-1)] of Byte;
  end;    }

const
  CRYPT_E_REVOKED     = $80092010;
  CRYPT_E_EXISTS      = $80092005;

  sCryptoApiError   = 'CryptoAPI Error. Code: %d.'+#13#10+'%s';
  sUnKnownCryptoApi = 'A call to a CryptoAPI function failed';

  NTE_BAD_FLAGS: Cardinal = $80090009;
  NTE_BAD_KEY_STATE: Cardinal = $8009000B;
  NTE_FAIL: Cardinal = $80090020;
  NTE_INVALID_HANDLE: Cardinal = $80090026;
  NTE_NOT_SUPPORTED: Cardinal = $80090029;
  PBE_SHA1_3DES: AnsiString = '1.2.840.113549.1.12.1.3'#0;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RaiseOSSLError(AErrCode: LongWord);
begin
  if AErrCode > 0 then
    raise EOpenSslError.CreateFmt('OpenSSL Error #%d.'+ #13#10 + '%s',
                                  [AErrCode, OpenSslErrMsg(AErrCode)])
  else
    raise EOpenSslError.Create('A call to an OpenSSL function failed');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RaiseLastOSSLError;
begin
  RaiseOSSLError(ERR_get_error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RaiseMsCryptoError(LastError: LongWord);
begin
  if LastError <> 0 then
    raise EMsCrypto.CreateFmt(sCryptoApiError, [LastError,
                              SysErrorMessage(LastError)])
  else
    raise EMsCrypto.Create(sUnKnownCryptoApi);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RaiseMyLastCryptoError;
var
  ErrCode: LongWord;
begin
  ErrCode := GetLastError;
  if ErrCode > 0 then
    RaiseMsCryptoError(ErrCode)
  else begin
    ErrCode := ERR_get_error;
    //f_ERR_clear_error;
    RaiseOSSLError(ErrCode);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function X509ToMsCert(x: PX509): PCCERT_CONTEXT;
var
  Buf, P: PByte;
  Len: Integer;
begin
  Result := nil;
  Len := i2d_X509(x, nil);
  if Len > 0 then begin
    GetMem(Buf, Len);
    try
      P := Buf; // This is important since f_i2d_X509 increments P by Len
      Len := i2d_X509(x, @P);
      if Len > 0 then
        Result := CertCreateCertificateContext(X509_ASN_ENCODING, Buf, Len);
    finally
      FreeMem(Buf);
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MsCertToX509(x: PCCERT_CONTEXT): PX509;
var
  p: PByte;
begin
  p := x.pbCertEncoded; // Use of a temporary variable is mandatory
  Result := d2i_X509(nil, @p, x.cbCertEncoded);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function InternalChainVerifyErrorToStr(const ErrCode: LongWord): string;
begin
  Result := '';
  if ErrCode and CERT_TRUST_IS_NOT_TIME_VALID = CERT_TRUST_IS_NOT_TIME_VALID then
    Result := Result + 'One of the certificates in the certificate chain is not time valid.'#13#10;
  if ErrCode and CERT_TRUST_IS_NOT_TIME_NESTED = CERT_TRUST_IS_NOT_TIME_NESTED then
    Result := Result + 'One of the certificates in the certificate chain is not time nested.'#13#10; //??
  if ErrCode and CERT_TRUST_IS_REVOKED = CERT_TRUST_IS_REVOKED then
    Result := Result + 'Trust for one of the certificates in the certificate chain has been revoked.'#13#10;
  if ErrCode and CERT_TRUST_IS_NOT_SIGNATURE_VALID = CERT_TRUST_IS_NOT_SIGNATURE_VALID then
    Result := Result + 'One of the certificates in the certificate chain does not have a valid signature.'#13#10;
  if ErrCode and CERT_TRUST_IS_NOT_VALID_FOR_USAGE = CERT_TRUST_IS_NOT_VALID_FOR_USAGE then
    Result := Result + 'The certificate chain is not valid for its proposed usage.'#13#10;
  if ErrCode and CERT_TRUST_IS_UNTRUSTED_ROOT = CERT_TRUST_IS_UNTRUSTED_ROOT then
    Result := Result + 'The certificate chain is based on an untrusted root.'#13#10;
  if ErrCode and CERT_TRUST_REVOCATION_STATUS_UNKNOWN = CERT_TRUST_REVOCATION_STATUS_UNKNOWN then
    Result := Result + 'The revocation status of one of the certificates in the certificate chain is unknown.'#13#10;
  if ErrCode and CERT_TRUST_IS_CYCLIC = CERT_TRUST_IS_CYCLIC then
    Result := Result + 'One of the certificates in the chain was issued by a certification authority that the original certificate had certified.'#13#10;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function InternalCertVerifyErrorToStr(const ErrCode: LongWord): string;
begin
  Result := '';
  if ErrCode and CERT_TRUST_IS_NOT_TIME_VALID = CERT_TRUST_IS_NOT_TIME_VALID then
    Result := Result + 'This certificate is not time valid.'#13#10;
  if ErrCode and CERT_TRUST_IS_NOT_TIME_NESTED = CERT_TRUST_IS_NOT_TIME_NESTED then
    Result := Result + 'This certificate is not time nested.'#13#10; //??
  if ErrCode and CERT_TRUST_IS_REVOKED = CERT_TRUST_IS_REVOKED then
    Result := Result + 'Trust for this certificate has been revoked.'#13#10;
  if ErrCode and CERT_TRUST_IS_NOT_SIGNATURE_VALID = CERT_TRUST_IS_NOT_SIGNATURE_VALID then
    Result := Result + 'The certificate does not have a valid signature.'#13#10;
  if ErrCode and CERT_TRUST_IS_NOT_VALID_FOR_USAGE = CERT_TRUST_IS_NOT_VALID_FOR_USAGE then
    Result := Result + 'The certificate is not valid for its proposed usage.'#13#10;
  if ErrCode and CERT_TRUST_IS_UNTRUSTED_ROOT = CERT_TRUST_IS_UNTRUSTED_ROOT then
    Result := Result + 'The certificate is based on an untrusted root.'#13#10;
  if ErrCode and CERT_TRUST_REVOCATION_STATUS_UNKNOWN = CERT_TRUST_REVOCATION_STATUS_UNKNOWN then
    Result := Result + 'The revocation status of the certificate is unknown.'#13#10;
  if ErrCode and CERT_TRUST_IS_CYCLIC = CERT_TRUST_IS_CYCLIC then
    Result := Result + 'One of the certificates in the chain was issued by a certification authority that the original certificate had certified.'#13#10;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MsChainVerifyErrorToStr(const ErrCode: LongWord): string;
begin
  if ErrCode = 0 then
    Result := 'No error found for this chain.'
  else begin
    Result := '';
    if ErrCode and CERT_TRUST_IS_PARTIAL_CHAIN = CERT_TRUST_IS_PARTIAL_CHAIN then
      Result := Result + 'The certificate chain is not complete.'#13#10;
    if ErrCode and CERT_TRUST_CTL_IS_NOT_TIME_VALID = CERT_TRUST_CTL_IS_NOT_TIME_VALID then
      Result := Result + 'A certificate trust list (CTL) used to create this chain was not time valid.'#13#10;
    if ErrCode and CERT_TRUST_CTL_IS_NOT_SIGNATURE_VALID = CERT_TRUST_CTL_IS_NOT_SIGNATURE_VALID then
      Result := Result + 'A CTL used to create this chain did not have a valid signature.'#13#10;
    if ErrCode and CERT_TRUST_CTL_IS_NOT_VALID_FOR_USAGE = CERT_TRUST_CTL_IS_NOT_VALID_FOR_USAGE then
      Result := Result + 'A CTL used to create this chain is not valid for this usage.'#13#10;
    Result := Result + InternalChainVerifyErrorToStr(ErrCode);
    while (Length(Result) > 0) and (Ord(Result[Length(Result)]) in [$0d, $0a]) do
      SetLength(Result, Length(Result) - 1);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MsCertVerifyErrorToStr(const ErrCode: LongWord): string;
begin
  if ErrCode = 0 then
    Result := 'No error found for this certificate.'
  else begin
    Result := '';
    Result := Result + InternalCertVerifyErrorToStr(ErrCode);
    while (Length(Result) > 0) and (Ord(Result[Length(Result)]) in [$0d, $0a]) do
      SetLength(Result, Length(Result) - 1);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TCertChainEngine }

constructor TMsCertChainEngine.Create;
begin
  inherited Create;
  FhCAStore    := CertOpenSystemStore(0, 'CA');
  if FhCAStore = nil then
    RaiseMsCryptoError(GetLastError);
  FhRootStore  := CertOpenSystemStore(0, 'Root');
  if FhRootStore = nil then
    RaiseMsCryptoError(GetLastError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsCertChainEngine.Deinit;
begin
  if FhCertChainEngine <> 0 then begin
      CertFreeCertificateChainEngine(FhCertChainEngine);
     FhCertChainEngine := 0;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TMsCertChainEngine.Destroy;
begin
  DeInit;
  if FCurCertCtx <> nil then
    CertFreeCertificateContext(FCurCertCtx);
  if FhCurTempStore <> nil then
    CertCloseStore(FhCurTempStore, 0);
  if FhCAStore <> nil then
    CertCloseStore(FhCAStore, 0);
  if FhRootStore <> nil then
    CertCloseStore(FhRootStore, 0);
  inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsCertChainEngine.Init;
begin
  DeInit;
  FEnhkeyUsage.cUsageIdentifier           := 0;
  FEnhkeyUsage.rgpszUsageIdentifier       := nil;

  FCertUsage.dwType                       := USAGE_MATCH_TYPE_AND;
  FCertUsage.Usage                        := FEnhkeyUsage;

  FChainPara.cbSize                       := SizeOf(TCertChainPara);
  FChainPara.RequestedUsage               := FCertUsage;

  FillChar(FCertEngineConfig, SizeOf(FCertEngineConfig), 0);
  FCertEngineConfig.cbSize                     := SizeOf(FCertEngineConfig);
  //FCertEngineConfig.hRestrictedRoot            := nil;
  //FCertEngineConfig.hRestrictedTrust           := nil;
  //FCertEngineConfig.hRestrictedOther           := nil;
  //FCertEngineConfig.cAdditionalStore           := 0;
  //FCertEngineConfig.rghAdditionalStore         := nil;
  FCertEngineConfig.dwFlags                    := CERT_CHAIN_CACHE_END_CERT;
  FCertEngineConfig.dwUrlRetrievalTimeout      := FUrlRetrievalTimeoutMsec;
  //FChainConfig.MaximumCachedCertificates       := 0;
  //FChainConfig.CycleDetectionModulus           := 0;
  if not CertCreateCertificateChainEngine(@FCertEngineConfig, FhCertChainEngine) then
      RaiseMsCryptoError(GetLastError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsCertChainEngine.SetUrlRetrievalTimeoutMsec(const Value: LongWord);
begin
  if Value <> FUrlRetrievalTimeoutMsec then
  begin
    FUrlRetrievalTimeoutMsec := Value;
    Init;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsCertChainEngine.SetVerifyOptions(const Value: TMsVerifyOptions);
begin
  if FVerifyOptions <> Value then
  begin
    FVerifyOptions := Value;
    FMsVerifyOptions := 0;
    if mvoRevocationCheckEndCert in Value then
      FMsVerifyOptions := FMsVerifyOptions or CERT_CHAIN_REVOCATION_CHECK_END_CERT;
    if mvoRevocationCheckChain in Value then
      FMsVerifyOptions := FMsVerifyOptions or CERT_CHAIN_REVOCATION_CHECK_CHAIN;
    if mvoRevocationCheckChainExcludeRoot in Value then
      FMsVerifyOptions := FMsVerifyOptions or CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT;
    if mvoRevocationCheckCacheOnly in Value then
      FMsVerifyOptions := FMsVerifyOptions or CERT_CHAIN_REVOCATION_CHECK_CACHE_ONLY;
    if mvoRevocationAccumulativeTimeOut in Value then
      FMsVerifyOptions := FMsVerifyOptions or CERT_CHAIN_REVOCATION_ACCUMULATIVE_TIMEOUT;
    if mvoCertChainDisableAuthRootAutoUpdate in Value then
      FMsVerifyOptions := FMsVerifyOptions or CERT_CHAIN_DISABLE_AUTH_ROOT_AUTO_UPDATE;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMsCertChainEngine.VerifyCert(ACert: TX509Base; ACertChain: TX509List;
  out AChainVerifyResult: LongWord; AUpdateCertChain: Boolean = TRUE): Boolean;
type
  TCertSimpleChain   = array [0..0] of PCERT_SIMPLE_CHAIN;
  PCertSimpleChain   = ^TCertSimpleChain;
  TCertChainElements = array [0..31] of PCERT_CHAIN_ELEMENT;
  PCertChainElements = ^TCertChainElements;

var
  pMsCertCtx      : PCCERT_CONTEXT;
  pTemp           : PCCERT_CONTEXT;
  I{, J}          : Integer;
  PChainElements  : PCertChainElements;
  PChainElement   : PCERT_CHAIN_ELEMENT;
  PSimpleChain    : PCertSimpleChain;
  x               : PX509;
  ChainContext    : PCCERT_CHAIN_CONTEXT;
  Err             : LongWord;
begin
  AChainVerifyResult := LongWord(-1);
  Assert(ACert <> nil);
  try
    if ACert.X509 = nil then
      raise EMsCertChainEngine.Create('Invalid parameter. ACert.X509 is nil');

    if FCurCertCtx <> nil then
    begin
      CertFreeCertificateContext(FCurCertCtx);
      FCurCertCtx := nil;
    end;

    if FhCurTempStore = nil then
    begin
      FhCurTempStore := CertOpenStore(sz_CERT_STORE_PROV_MEMORY,
                                      X509_ASN_ENCODING, 0, 0, nil);
      if FhCurTempStore = nil then
        RaiseMsCryptoError(GetLastError);
    end;

    if FhCertChainEngine = 0 then
      Init;

    if Assigned(ACertChain) and (ACertChain.Count > 0) then
    begin
      for I := ACertChain.Count -1 downto 0 do
      begin
        { We store all certificates except the end certificate if they are }
        { not in the system store temporarily.                             }
        if ACert.SameHash(ACertChain[I]) then
          Continue;
        pMsCertCtx := X509ToMsCert(ACertChain[I].X509);
        if pMsCertCtx = nil then
          RaiseMyLastCryptoError;
        try
          if X509_check_issued(ACertChain[I].X509, ACertChain[I].X509) = 0 then
            { Self-signed Root }
            pTemp := CertFindCertificateInStore(FhRootStore,
                      X509_ASN_ENCODING, 0, CERT_FIND_EXISTING, pMsCertCtx, nil)
          else
            { Assume CA }
            pTemp := CertFindCertificateInStore(FhCaStore,
                      X509_ASN_ENCODING, 0, CERT_FIND_EXISTING, pMsCertCtx, nil);
          if Assigned(pTemp) then
          begin
            CertFreeCertificateContext(pTemp);
            Continue; // Found
          end;
          if not CertAddCertificateContextToStore(FhCurTempStore,
                      pMsCertCtx, CERT_STORE_ADD_NEW, nil) then
          begin
            Err := GetLastError;
            if Err <> CRYPT_E_EXISTS then
                RaiseMsCryptoError(Err);
          end;
        finally
            CertFreeCertificateContext(pMsCertCtx);
        end;
      end;
    end;

    FCurCertCtx := X509ToMsCert(ACert.X509);
    if FCurCertCtx = nil then
      RaiseMyLastCryptoError;
    try
      if not CertGetCertificateChain(
                  0,                    // use the default chain engine
                  FCurCertCtx,          // pointer to the end certificate
                  nil,                  // use the default time
                  FhCurTempStore,       // search additional stores
                  @FChainPara,          // use AND logic and enhanced key usage
                                        // as indicated in the ChainPara
                                        // data structure
                  FMsVerifyOptions,     // Flags
                  nil,                  // currently reserved
                  @ChainContext)        // return a pointer to the chain created
      then
        RaiseMsCryptoError(GetLastError);

      AChainVerifyResult := ChainContext^.TrustStatus.dwErrorStatus;
      ACert.CustomVerifyResult := Integer(AChainVerifyResult);
      Result := AChainVerifyResult = 0;

      if Assigned(ACertChain) and AUpdateCertChain then
      begin
        ACertChain.Clear;
        { If AUpdateCertChain is TRUE the function returns the certificate }
        { chain and sets each cert's property CustomVerifyResult.          }
        if ChainContext^.cChain > 0 then
        begin
          PSimpleChain  := Pointer(ChainContext^.rgpChain);
          { It's not clear (to me) in which cases there will be multiple simple chains }
          if PSimpleChain^[0].cElement > 0 then
          begin
            if PSimpleChain^[0].cElement >
                SizeOf(TCertChainElements) div SizeOf(Pointer) then
              raise EMsCertChainEngine.CreateFmt(
                '! Fatal: Certificate chain too long (%d)',
                [PSimpleChain^[0].cElement]);
            PChainElements := Pointer(PSimpleChain^[0].rgpElement);
            for I := 0 to PSimpleChain^[0].cElement - 1 do
            begin
              PChainElement := PChainElements^[I];
              x := MsCertToX509(PChainElement^.pCertContext);
              if x = nil then
                RaiseLastOSSLError;
              try
                {J := FindX509InChain(x, ACertChain);
                if J >= 0 then
                  ACertChain[J].CustomVerifyResult :=
                                  PChainElement^.TrustStatus.dwErrorStatus
                else begin}
                  ACertChain.Insert(0, x);
                  ACertChain[0].VerifyDepth := I;
                  ACertChain[0].CustomVerifyResult :=
                                Integer(PChainElement^.TrustStatus.dwErrorStatus);
                //end;
              finally
                X509_free(x);
              end;
            end;
          end;
        end;
      end; // UpdateCertChain
    finally
      CertFreeCertificateChain(ChainContext);
    end;
  except
    on E: Exception do begin
      if FCurCertCtx <> nil then
      begin
        CertFreeCertificateContext(FCurCertCtx);
        FCurCertCtx := nil;
      end;
      if E is EMsCertChainEngine then
        raise
      else
        raise EMsCertChainEngine.Create(E.Message);
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Use this method if ACert has not been verified yet (slow).                }
function TMsCertChainEngine.ViewCert(ACert: TX509Base; ACertChain: TX509List;
  AHwnd: HWND = 0; ADlgEnableAddToStore: Boolean = True;
  ATitle: PChar = nil): Boolean;
var
  ChainVerifyResult : LongWord;
begin
  VerifyCert(ACert, ACertChain, ChainVerifyResult, False);
  Result := ViewCertLastVerified(AHwnd, ADlgEnableAddToStore, ATitle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Use this method if ACert has been already verified (faster).              }
function TMsCertChainEngine.ViewCertVerified(ACert: TX509Base; AHwnd: HWND = 0;
  ADlgEnableAddToStore: Boolean = True; ATitle: PChar = nil): Boolean;
var
  PMsCtx: PCCERT_CONTEXT;
begin
  if (ACert <> nil) and (ACert.X509 <> nil) then
  begin
    PMsCtx := X509ToMsCert(ACert.X509);
    if PMsCtx = nil then
      RaiseMyLastCryptoError;
    try
      Result := InternalViewCert(PMsCtx, AHwnd, ADlgEnableAddToStore, ATitle);
    finally
      CertFreeCertificateContext(PMsCtx);
    end;
  end
  else
      Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Use this method to show the last verified cert (fastest).                 }
function TMsCertChainEngine.ViewCertLastVerified(AHwnd: HWND = 0;
  ADlgEnableAddToStore: Boolean = True; ATitle: PChar = nil): Boolean;
begin
  Result := InternalViewCert(FCurCertCtx, AHwnd, ADlgEnableAddToStore, ATitle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMsCertChainEngine.InternalViewCert(ACertCtx: PCCERT_CONTEXT; AHwnd: HWND;
  AEnableAddToStore: Boolean; ATitle: PChar): Boolean;
var
  Info : CRYPTUI_VIEWCERTIFICATE_STRUCT;
begin
  if FCurCertCtx = nil then
  begin
    Result := False;
    Exit;
  end;
  FillChar(Info, SizeOf(Info), 0);
  Info.dwSize       := SizeOf(Info);
  Info.hwndParent   := AHwnd;
  Info.szTitle      := ATitle;
  Info.pCertContext := ACertCtx;
  if not AEnableAddToStore then
    Info.dwFlags := CRYPTUI_DISABLE_ADDTOSTORE;
  if FhCurTempStore <> nil then
  begin
    Info.cStores   := 1;
    Info.rghStores := @FhCurTempStore;
  end;
  Result := CryptUIDlgViewCertificate(@Info, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.67 a list of certificates from the Microsoft Windows certificate store }
{ load a Windows store into the list of certificates, optionally not emptying it first }
function TMsX509List.LoadFromStore(MsCertStoreType: TMsCertStoreType; MsCertLocation: TMsCertLocation;
                                                                         Empty: Boolean = True): Integer;
 { V8.71 workaround for EC keys, thanks to uso }
  function CngTrySetEccPrivateKeyExportable(const ACertStore: HCERTSTORE; var AKeyHandle: NCRYPT_KEY_HANDLE; var AError: string): Boolean;
  var
    LKeyData: RawByteString;
    LParameterList: TNCryptBufferDesc;
    LCryptBuffers: array[0..4] of TNCryptBuffer;
    LParams: PBE_PARAMS;
    dwRes: DWORD;
    dwFlags: DWORD;
    LKeyLen: Cardinal;
    LError: Cardinal;
    LKeyPol: DWORD;
    LhProvider: NCRYPT_PROV_HANDLE;
  begin
    Result := False;

    // Prepare ECC private key export in a way that requirements of Crypto-API/CNG are statisfied
    FillChar(LCryptBuffers[0], SizeOf(LCryptBuffers), #0);
    FillChar(LParams, SizeOf(LParams), #0);
//    LParams.Params.cbSalt := LParams.RgbSaltSize;
    LParams.Params.cbSalt := RGB_SALT_SIZE;

    LCryptBuffers[0].BufferType := NCRYPTBUFFER_PKCS_ALG_OID;
    LCryptBuffers[0].cbBuffer := Length(PBE_SHA1_3DES);
    LCryptBuffers[0].pvBuffer := @PBE_SHA1_3DES[1];

    LCryptBuffers[1].BufferType := NCRYPTBUFFER_PKCS_SECRET;
    LCryptBuffers[1].cbBuffer := 0;
    LCryptBuffers[1].pvBuffer := nil;

    LCryptBuffers[2].BufferType := NCRYPTBUFFER_PKCS_ALG_PARAM;
    LCryptBuffers[2].cbBuffer := SizeOf(LParams);
    LCryptBuffers[2].pvBuffer := @LParams;

    dwRes := 0;
    dwFlags := NCRYPT_SILENT_FLAG;

    LParameterList.ulVersion := NCRYPTBUFFER_VERSION;
    LParameterList.cBuffers := 3;
    LParameterList.pBuffers := @LCryptBuffers[0];

    // Get private key length
    LError := NCryptExportKey(AKeyHandle, 0, NCRYPT_PKCS8_PRIVATE_KEY_BLOB, @LParameterList, nil, 0, dwRes, dwFlags);
    if ERROR_SUCCESS <> LError then
    begin
      AError := AError + 'Could not export private key - ' + GetWindowsErr(LError);
      Exit;
    end;

    // Get private key
    LKeyLen := dwRes;
    SetLength(LKeyData, LKeyLen);
    FillChar(LKeyData[1], LKeyLen, #0);
    LError := NCryptExportKey(AKeyHandle, 0, NCRYPT_PKCS8_PRIVATE_KEY_BLOB, @LParameterList, @LKeyData[1], LKeyLen, LKeyLen, dwFlags);
    if ERROR_SUCCESS <> LError then
    begin
      SetLength(LKeyData, 0);
      AError := AError + 'Could not export private key - ' + GetWindowsErr(LError);
      Exit;
    end;

    // Get current key export policy
    LKeyPol := 0;
    LError := NCryptGetProperty(AKeyHandle, NCRYPT_EXPORT_POLICY_PROPERTY, @LKeyPol, SizeOf(LKeyPol), dwRes, dwFlags);
    if ERROR_SUCCESS <> LError then
    begin
      SetLength(LKeyData, 0);
      AError := AError + 'Error fetching private key properties - ' + GetWindowsErr(LError);
      Exit;
    end;

    if NCRYPT_ALLOW_PLAINTEXT_EXPORT_FLAG = (LKeyPol and NCRYPT_ALLOW_PLAINTEXT_EXPORT_FLAG) then
    begin
      AError := AError + 'Key already marked as exportable. Don''t know what went wrong.';
      Exit;
    end;

    LError := NCryptFreeObject(AKeyHandle);
    if ERROR_SUCCESS <> LError then
    begin
      AError := AError + 'Error freeing previous key handle - ' + GetWindowsErr(LError);
      Exit;
    end;

    LhProvider := 0;
    try
      LError := NCryptOpenStorageProvider(LhProvider, MS_KEY_STORAGE_PROVIDER, 0);
      if ERROR_SUCCESS <> LError then
      begin
        AError := AError + 'Error opening key storage provider - ' + GetWindowsErr(LError);
        Exit;
      end;

      // Reimport the private key
      FillChar(LCryptBuffers[0], SizeOf(LCryptBuffers), #0);
      LCryptBuffers[0].BufferType := NCRYPTBUFFER_PKCS_SECRET;
      LCryptBuffers[0].cbBuffer := 0;
      LCryptBuffers[0].pvBuffer := nil;
      LParameterList.cBuffers := 1;
      dwFlags := NCRYPT_SILENT_FLAG or  NCRYPT_SILENT_FLAG OR NCRYPT_OVERWRITE_KEY_FLAG OR NCRYPT_DO_NOT_FINALIZE_FLAG;
      LError := NCryptImportKey(LhProvider, 0, NCRYPT_PKCS8_PRIVATE_KEY_BLOB, @LParameterList, AKeyHandle, @LKeyData[1], LKeyLen, dwFlags);
      if ERROR_SUCCESS <> LError then
      begin
        AError := AError + 'Error reimporting private key - ' + GetWindowsErr(LError);
        Exit;
      end;

      // Allow plain text export.
      // It seems the NCRYPT_ALLOW_PLAINTEXT_EXPORT_FLAG is volatile in the context of ECC private keys.
      dwFlags := NCRYPT_SILENT_FLAG;
      LKeyPol := LKeyPol or NCRYPT_ALLOW_PLAINTEXT_EXPORT_FLAG;
      LError := NCryptSetProperty(AKeyHandle, NCRYPT_EXPORT_POLICY_PROPERTY, @LKeyPol, SizeOf(LKeyPol), dwFlags);
      if ERROR_SUCCESS <> LError then
      begin
        AError := AError + 'Error adjusting private key properties - ' + GetWindowsErr(LError);
        Exit;
      end;

      LError := NCryptFinalizeKey(AKeyHandle, dwFlags);
      if ERROR_SUCCESS <> LError then
      begin
        AError := AError + 'Error finalizing private key - ' + GetWindowsErr(LError);
        Exit;
      end;
    finally
      if 0 <> LhProvider then
      begin
        NCryptFreeObject(LhProvider);
      end;
    end;
    Result := True;
  end;

var
    hSystemStore: HCERTSTORE;
    pCertContext: PCCERT_CONTEXT;
    NewX509: PX509;
    hKey: NCRYPT_KEY_HANDLE;
    PKeyBlob: AnsiString;
    PkeyBio: PBIO;
    PKey: PEVP_PKEY;
    dwFlags, dwKeySpec, dwResLen, PKeyLen: DWORD;
    fCallerFreeProvOrNCryptKey: BOOL;
    Ret: SECURITY_STATUS;
    ParameterList: TNCryptBufferDesc;
    CryptBuffers: array of TNCryptBuffer;
    KeyPolicy: DWORD;
    PropWStr: WideString;
    PKeyInfo: String;
begin
    Result := 0;
    if Empty then Clear;
    pCertContext := nil;
    MsLastError := '';
    if MsStoreProvider = Nil then
        MsStoreProvider := CERT_STORE_PROV_SYSTEM;

 { Open the Windows certificate store }
    hSystemStore := CertOpenStore(MsStoreProvider, 0, 0, (MsCertLocNames[MsCertLocation] or
                                    CERT_STORE_READONLY_FLAG), MsCertStoreNames [MsCertStoreType]);
    if hSystemStore = nil  then begin
        MsLastError := 'Could not open the ' + MsCertStoreNames [MsCertStoreType] + ' system store';
        Exit;
    end;

    { Enum all the certs in the store and store them in PEM format }
    pCertContext := CertEnumCertificatesInStore(hSystemStore, pCertContext);
    try
        while pCertContext <> nil do begin
            NewX509 := MsCertToX509(pCertContext);
            if Assigned(NewX509) then begin
                Self.Add(NewX509);
                X509_free(NewX509);
                Self[Result].Comments := '';
                PKeyInfo := '';
                Self[Result].PrivateKey := Nil;   { V8.71 may not find one }
                SetLength(PropWStr, 256);
                dwResLen := 255;
                if CertGetCertificateContextProperty(pCertContext, CERT_FRIENDLY_NAME_PROP_ID, @PropWStr[1], dwResLen) then
                    Self[Result].CertName := Trim(String(Copy(PropWStr, 1, dwResLen div 2)))  // remove trailing null
                else if CertGetCertificateContextProperty(pCertContext, CERT_DESCRIPTION_PROP_ID, @PropWStr[1], dwResLen) then
                    Self[Result].CertName := Trim(String(Copy(PropWStr, 1, dwResLen div 2)))
                else
                    Self[Result].CertName := '<none>'; // no friendly name

              // see if private key exists for personal store
              // will fail if not exportable or from a TPM, needs admin rights to read local machine keys
                if MsCertStoreType = MsStoreMy then begin
                  if CertGetCertificateContextProperty(pCertContext, CERT_KEY_SPEC_PROP_ID, @dwKeySpec, dwResLen) then begin

                    if CryptAcquireCertificatePrivateKey(pCertContext, CRYPT_ACQUIRE_SILENT_FLAG OR
                             CRYPT_ACQUIRE_PREFER_NCRYPT_KEY_FLAG, Nil, hKey, @dwKeySpec, @fCallerFreeProvOrNCryptKey) then begin
                        if NOT (dwKeySpec AND CERT_NCRYPT_KEY_SPEC = CERT_NCRYPT_KEY_SPEC) then  // key is a CNG key
                             PKeyInfo := 'Acquired private key not CNG'
                           // do these still exist?
                        else begin
                         //     dwFlags := NCRYPT_SILENT_FLAG OR NCRYPT_PERSIST_ONLY_FLAG;
                                dwFlags := 0;

                         // see if key can be exported, but will fail anyway if not allowed
                         //   KeyPolicy := 0;
                         //   NCryptGetProperty(hKey, NCRYPT_EXPORT_POLICY_PROPERTY, @KeyPolicy, 4, dwResLen, dwFlags);
                         //   PKeyInfo := 'Key export properties = x' + IntToHex(KeyPolicy, 1) + ' - ';   // !!! TEMP DIAG
                         //   if KeyPolicy AND NCRYPT_ALLOW_PLAINTEXT_EXPORT_FLAG =  NCRYPT_ALLOW_PLAINTEXT_EXPORT_FLAG then begin

                                NCryptGetProperty(hKey, NCRYPT_KEY_TYPE_PROPERTY, @KeyPolicy, 4, dwResLen, dwFlags);
                                if KeyPolicy = 1 then
                                     PKeyInfo := 'Private key in Local Machine Store, '
                                else
                                     PKeyInfo := 'Private key in User Store, ';
                                dwResLen := 0;
                            { returns the key name, should match certificate but might be duplicates }
                                Ret := NCryptGetProperty(hKey, NCRYPT_NAME_PROPERTY, @PropWStr[1], 255, dwResLen, dwFlags);
                                if Ret = ERROR_SUCCESS then
                                    Self[Result].KeyName := Trim(String(Copy(PropWStr, 1, dwResLen div 2)));
                                dwResLen := 0;
                                dwFlags := NCRYPT_SILENT_FLAG;
                                SetLength(CryptBuffers, 1);
                                CryptBuffers[0].BufferType := NCRYPTBUFFER_PKCS_SECRET;
                                CryptBuffers[0].pvBuffer := Nil;
                                CryptBuffers[0].cbBuffer := 0;
                                ParameterList.ulVersion := NCRYPTBUFFER_VERSION;
                                ParameterList.cBuffers := 1;
                                ParameterList.pBuffers := @CryptBuffers[0];
                                Ret := NCryptExportKey(hKey, 0, NCRYPT_PKCS8_PRIVATE_KEY_BLOB, @ParameterList, Nil, 0, dwResLen, dwFlags);
                                if NTE_NOT_SUPPORTED = Cardinal(Ret) then  { V8.71 workaround for EC keys, thanks to uso }
                                begin
                                    if CngTrySetEccPrivateKeyExportable(hSystemStore, hKey, PKeyInfo) then
                                    begin
                                        Ret := NCryptExportKey(hKey, 0, NCRYPT_PKCS8_PRIVATE_KEY_BLOB, @ParameterList, Nil, 0, dwResLen, dwFlags);
                                    end;
                                end;

                                if Ret <> ERROR_SUCCESS then
                                    PKeyInfo := PKeyInfo + 'Could not export private key - ' + GetWindowsErr(Ret)
                                else begin
                                    PKeyLen := dwResLen;
                                    SetLength(PKeyBlob, PKeyLen + 16);
                                    Ret := NCryptExportKey(hKey, 0, NCRYPT_PKCS8_PRIVATE_KEY_BLOB, @ParameterList, @PKeyBlob[1], PKeyLen, dwResLen, dwFlags);
                                    if Ret <> ERROR_SUCCESS then
                                        PKeyInfo := PKeyInfo + 'Could not export private key - ' + GetWindowsErr(Ret)
                                    else begin
                                     // convert PKCS8 private key buffer to internal key
                                        SetLength(PKeyBlob, dwResLen);
                                        PkeyBio := BIO_new_mem_buf(@PKeyBlob[1], dwResLen);
                                        PKey := Nil;
                                        try
                                            BIO_ctrl(PkeyBio, BIO_CTRL_RESET, 0, nil);
                                            PKey := d2i_PrivateKey_bio(PkeyBio, Nil);
                                            if Assigned(PKey) then begin
                                                Self[Result].PrivateKey := PKey;
                                                if NOT (Self[Result].CheckCertAndPKey) then
                                                    PKeyInfo := PKeyInfo + 'Private key does not match certificate'
                                                else
                                                    PKeyInfo := PKeyInfo + 'Private key exported OK'
                                            end
                                            else begin
                                                PKeyInfo := PKeyInfo + 'Failed to convert private key to internal format - ' +
                                                                                         String(LastOpenSslErrMsg(False));
                                             end;
                                        finally
                                            bio_free(PkeyBio);
                                            EVP_PKEY_free(PKey);
                                        end;
                                    end;
                                end;
                            // end; //export
                            if fCallerFreeProvOrNCryptKey then
                                NCryptFreeObject (hKey);
                        end;
                    end
                    else
                          PKeyInfo := 'Could not acquire private key - ' + GetWindowsErr(GetLastError);

                  end;
                end;
                Self[Result].Comments := PKeyInfo;
                Result := Result + 1;
            end;
            pCertContext := CertEnumCertificatesInStore(hSystemStore, pCertContext);
        end;
    finally
        if pCertContext <> nil then
            CertFreeCertificateContext(pCertContext);
        if hSystemStore <> nil then
            CertCloseStore(hSystemStore, CERT_CLOSE_STORE_FORCE_FLAG);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.71 search for best certificate in store optionally with a matching private key }
{ note private key only available if the app has administrator rights, so false for debugging }
function TMsX509List.FindBest(const Search: String; Pkey: Boolean = True): Integer;
var
    I, SelNr: Integer;
    curDT, expireDT: TDateTime;
    MyName, WildSrch: String;

    procedure CheckDates(J: Integer);
    var
        certexpDT: TDateTime;
    begin
        certexpDT := Self[J].ValidNotAfter;
        if (curDT > Self[J].ValidNotBefore) and (curDT < certexpDT) and (Self[J].CheckCertAndPKey or (NOT Pkey)) then begin
            if (expireDT = 0) then begin
                expireDT := certexpDT;
                SelNr := J;
            end
        // ignore if have a later exiry already
            else if (certexpDT > expireDT) then begin
                expireDT := certexpDT;
                SelNr := J;
            end;
        end;
    end;

begin
    Result := -1;
    if Count = 0 then
        Exit;
    curDT := Now;
    SelNr := -1;
    WildSrch := Search;
    I := Pos('.', Search);
    if I > 0 then
        WildSrch := '*' + Copy(Search, I, 9999);

// look for exact common name match with longest expiration date
    expireDT := 0;
    for I := 0 to Count - 1 do begin
        MyName := Self[I].SubjectCName;
        if (MyName = Search) or (MyName = WildSrch) then
            CheckDates(I)
    end;

// look for partial friendly name that might have a date, ie www.magsys.co.uk_2023-06-20T17:11:29
    if SelNr < 0 then begin
        for I := 0 to Count - 1 do begin
            MyName := Self[I].CertName;
            if (Pos(Search, MyName) = 1) or (Pos(Search, MyName) = 1) then
                CheckDates(I);
        end;
    end;

// look for alternate domain name in list
    if SelNr < 0 then begin
        for I := 0 to Count - 1 do begin
            MyName := Self[I].SubAltNameDNS;
            if (Pos(Search, MyName) > 0) or (Pos(WildSrch, MyName) > 0) then
                CheckDates(I);
        end;
    end;

// we found a good certificate
    if SelNr >= 0 then
        Result := SelNr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.67 delete one certificate and private key from the Microsoft Windows certificate store }
function TMsX509List.DeleteFromStore(Sha1Digest: THashBytes20; MsCertStoreType: TMsCertStoreType;
                                        MsCertLocation: TMsCertLocation; DelPKey: Boolean): Boolean;
var
    hSystemStore: HCERTSTORE;
    pCertContext: PCCERT_CONTEXT;
    HashBlob: TCryptHashBlob;
    CertId: TCertId;
    hKey: NCRYPT_KEY_HANDLE;
    dwKeySpec, dwResLen: DWORD;
    Ret: SECURITY_STATUS;
    fCallerFreeProvOrNCryptKey: BOOL;
begin
    Result := False;
    MsLastError := '';
    pCertContext := Nil;
    if MsStoreProvider = Nil then MsStoreProvider := CERT_STORE_PROV_SYSTEM;

 { Open the Windows certificate store }
    hSystemStore := CertOpenStore(MsStoreProvider, 0, 0, MsCertLocNames[MsCertLocation], MsCertStoreNames [MsCertStoreType]);
    if hSystemStore = nil  then begin
        MsLastError := 'Could not open the personal/my system store';
        Exit;
    end;

    try
     { find certs we want to delete by SHA1 hash }
        HashBlob.cbData := Length(Sha1Digest);
        HashBlob.pbData := @Sha1Digest[0];
        CertId.dwIdChoice := CERT_ID_SHA1_HASH;
        CertId.HashId := HashBlob;
        pCertContext := CertFindCertificateInStore(hSystemStore, X509_ASN_ENCODING, 0, CERT_FIND_CERT_ID, @CertId, Nil);
        if pCertContext = nil  then begin
            MsLastError := 'Could not find certificate in store - ' + GetWindowsErr(GetLastError);
            Exit;
        end;

     { see if we have a private key to delete first }
        if DelPkey then begin
            dwResLen := 4;
            if CertGetCertificateContextProperty(pCertContext, CERT_KEY_SPEC_PROP_ID, @dwKeySpec, dwResLen) then begin
                if CryptAcquireCertificatePrivateKey(pCertContext, CRYPT_ACQUIRE_SILENT_FLAG OR
                     CRYPT_ACQUIRE_PREFER_NCRYPT_KEY_FLAG, Nil, hKey, @dwKeySpec, @fCallerFreeProvOrNCryptKey) then begin
                    if (dwKeySpec AND CERT_NCRYPT_KEY_SPEC = CERT_NCRYPT_KEY_SPEC) then begin // key is a CNG key
                        Ret := NCryptDeleteKey(hKey, NCRYPT_SILENT_FLAG);
                        if Ret <> ERROR_SUCCESS then
                            MsLastError := 'Could not delete private key - ' + GetWindowsErr(Ret);
                       // handle no longer valid
                    end;
                end;
            end;
        end;

    { delete the certificate }
        if NOT CertDeleteCertificateFromStore(pCertContext) then
            MsLastError := 'Could not delete certificate from store - ' + GetWindowsErr(GetLastError)
        else
            Result := True;
        pCertContext := nil;  // function always frees content memory even with errors

    finally
        if pCertContext <> nil then
            CertFreeCertificateContext(pCertContext);
        if hSystemStore <> nil then
            CertCloseStore(hSystemStore, CERT_CLOSE_STORE_FORCE_FLAG);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.67 a list private key names for store, needs admin access }
function TMsX509List.ListPKeys(MsCertLocation: TMsCertLocation; var StorePkeyInfos: TStorePkeyInfos): Integer;
var
    phProvider: NCRYPT_PROV_HANDLE;
    ppKeyName: PNCryptKeyName;
    ppEnumState: Pointer;
    dwFlags: DWORD;
    Ret: SECURITY_STATUS;
begin
    Result := 0;
    if StorePkeyInfos = Nil then exit;
    phProvider := 0;
    ppEnumState := nil;
    ppKeyName := nil;
    MsLastError := '';
    if MsStoreProvider = Nil then MsStoreProvider := CERT_STORE_PROV_SYSTEM;
    if MsKeyProvider = Nil then MsKeyProvider := MS_KEY_STORAGE_PROVIDER;   { V8.68 }

 { Open the Windows certificate store }
   Ret := NCryptOpenStorageProvider(phProvider, MsKeyProvider, 0);          { V8.68 }
    if phProvider = 0  then begin
        MsLastError := 'Could not open the key store - ' + GetWindowsErr(Ret);
        Exit;
    end;

 { Enum all the private keys in the store and store names in records }
    try
        SetLength(StorePkeyInfos, 20);
        while true do begin
            if Result >= Length(StorePkeyInfos) then
                SetLength(StorePkeyInfos, Result * 2);
            StorePkeyInfos[Result].KeyName := '';
            dwFlags := NCRYPT_SILENT_FLAG;
            if MsCertLocation = MsLocMachine then
               dwFlags := dwFlags OR NCRYPT_MACHINE_KEY_FLAG;
            Ret := NCryptEnumKeys(phProvider, Nil, ppKeyName, ppEnumState, dwFlags);
            if (Ret <> ERROR_SUCCESS) then begin
                if Ret <> -2146893782 { NTE_NO_MORE_ITEMS } then
                    MsLastError := 'Enumerate error - ' + GetWindowsErr(Ret);
                break;
            end;
            if (ppKeyName <> Nil) then begin
                StorePkeyInfos[Result].KeyName := Trim(String(ppKeyName.pszName));
                StorePkeyInfos[Result].KeyAlg := Trim(String(ppKeyName.pszAlgid));
                StorePkeyInfos[Result].KeyType := ppKeyName.dwLegacyKeySpec;
                StorePkeyInfos[Result].KeyFlags := ppKeyName.dwFlags;
                Result := Result + 1;
                NCryptFreeObject (NCRYPT_HANDLE(ppKeyName));
                ppKeyName := Nil;
            end;
        end;
    finally
        SetLength(StorePkeyInfos, Result);
        if (ppKeyName <> Nil) then
           NCryptFreeObject(NCRYPT_HANDLE(ppKeyName));
        if phProvider <> 0 then
           NCryptFreeObject(phProvider);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.67 delete a named private key from the store, needs admin access }
function TMsX509List.DeletePKey(const KeyName: String; MsCertLocation: TMsCertLocation): Boolean;
var
    phProvider: NCRYPT_PROV_HANDLE;
    KeyNameW: WideString;
    hKey: NCRYPT_KEY_HANDLE;
    dwFlags: DWORD;
    Ret: SECURITY_STATUS;
begin
    Result := False;
    phProvider := 0;
    MsLastError := '';
    if MsStoreProvider = Nil then MsStoreProvider := CERT_STORE_PROV_SYSTEM;
    if MsKeyProvider = Nil then MsKeyProvider := MS_KEY_STORAGE_PROVIDER;   { V8.68 }

 { Open the Windows certificate store }
    KeyNameW := WideString(KeyName) + IcsNull;
    ret := NCryptOpenStorageProvider(phProvider, MsKeyProvider, 0);         { V8.68 }
    if phProvider = 0  then begin
        MsLastError := 'Could not open the key store - ' + GetWindowsErr(Ret);
        Exit;
    end;

 { open named private key in the store }
    try
        dwFlags := NCRYPT_SILENT_FLAG;
        if MsCertLocation = MsLocMachine then
           dwFlags := dwFlags OR NCRYPT_MACHINE_KEY_FLAG;
        Ret := NCryptOpenKey(phProvider, hKey, @KeyNameW[1], 0, dwFlags);
        if (Ret <> ERROR_SUCCESS) then
            MsLastError := 'Can not open private key - ' + GetWindowsErr(Ret)
        else begin
            Ret := NCryptDeleteKey(hKey, NCRYPT_SILENT_FLAG);
            if Ret <> ERROR_SUCCESS then
                MsLastError := 'Could not delete private key - ' + GetWindowsErr(Ret)
             else
                Result := True;
           // handle no longer valid
        end;
    finally
        if phProvider <> 0 then
           NCryptFreeObject(phProvider);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMsCertTools.Assign(Source: TPersistent);                                   { V8.71 }
var
    LSource: TMsCertTools;
begin
    if Source is TMsCertTools then begin
        LSource := Source as TMsCertTools;
        MsStoreProvider := LSource.MsStoreProvider;
        MsKeyProvider   := LSource.MsKeyProvider;
    end;
    inherited Assign(Source);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ search and load one certificate from the Windows personal store with private keys and intermediate }
function TMsCertTools.LoadOneFromStore(MsCertLocation: TMsCertLocation; const Search: String; Pkey: Boolean = True): String;    { V8.71 }
var
    Total, Index: Integer;
    MyMsX509List: TMsX509List;

    function FindInter(const AName: string): Integer;
    var
        J: Integer;
    begin
        Result := -1;
        for J := 0 to MyMsX509List.Count - 1 do begin
            if (MyMsX509List[J].SubjectCName = AName) or (MyMsX509List[J].SubjectOName = AName) then begin
                if Now < MyMsX509List[J].ValidNotAfter then begin
                    Result := J;
                    Exit;
                end;
            end;
        end;
    end;

begin
    Result := '';  // blank is success
    if Search = '' then begin
        Result := 'Need certificate name to search';
        Exit;
    end;
    MyMsX509List := TMsX509List.Create(nil, True);
    try
        Total := MyMsX509List.LoadFromStore(MsStoreMy, MsCertLocation, True);
        if MyMsX509List.MsLastError <> '' then begin
             Result := 'Error reading store: ' + MyMsX509List.MsLastError;
             Exit;
        end;
        if Total <= 0 then begin
            Result := 'No certificates found in store';
            Exit;
        end;

      // find certificate, ignore private key under debugger
        Index := MyMsX509List.FindBest(Search, Pkey);
        if Index < 0 then begin
            Result := 'Can not find certificate';
            if Pkey then
                Result := Result + ' and private key';
            Result := Result + ' in Windows Store: ' + Search;
            Exit;
        end;
        Self.ClearAll;
        Self.Assign (TMsCertTools(MyMsX509List[Index]));
        if NOT Self.IsCertLoaded then begin
            Result := 'Failed to load certificate from Windows Store';
            Exit;
        end;

      // now look for intermediates in certificate authorities store (not root store)
        MyMsX509List.Clear;
        Total := MyMsX509List.LoadFromStore(MsStoreCA, MsCertLocation, True);
        if Total <> 0 then begin
            Index := FindInter(Self.IssuerCName);
            if Index >= 0 then begin
                Self.AddToInters(MyMsX509List[Index].X509);       // first intermediate
                Index := FindInter(MyMsX509List[Index].IssuerCName);
                if Index >= 0 then begin                          // see if signed by another in same store
                    if MyMsX509List[Index].IssuerCName <> 'DST Root CA X3' then  // ignore intermediate for expired root
                        Self.AddToInters(MyMsX509List[Index].X509);   // second intermediate
                end;
            end;
        end;
        Result := '';
    finally
        MyMsX509List.Free;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ load one certificate from the Windows personal store with private keys }
{ warning - currently exports all certificates in store, as intermediates }
(* V8.71 removed, never used by applications, not that useful
function TMsCertTools.LoadFromMyStore(MsCertLocation: TMsCertLocation): Integer;
var
    hSystemStore: HCERTSTORE;
    P12Buf: AnsiString;
    PW: WideString;
    PFXBlob: TCryptDataBlob;
begin
    Result := 0;
//  MsLastError := '';
    if MsStoreProvider = Nil then
        MsStoreProvider := CERT_STORE_PROV_SYSTEM;

 { Open the Windows certificate store }
    hSystemStore := CertOpenStore(MsStoreProvider, 0, 0, (MsCertLocNames[MsCertLocation] or
                                          CERT_STORE_READONLY_FLAG), MsCertStoreNames [MsStoreMy]);
    if hSystemStore = nil  then
        raise EX509Exception.Create('Could not open the My/Personal system store');
    try
        PFXBlob.cbData := 0;
        PFXBlob.pbData := Nil;  // find length of buffer
        PW := InternalPW;
        if NOT PFXExportCertStore(hSystemStore, @PFXBlob, PWideChar(PW), EXPORT_PRIVATE_KEYS) then
            raise EX509Exception.Create('Could not create PFX blob from store - ' + GetWindowsErr(GetLastError))
        else begin
            SetLength(P12Buf, PFXBlob.cbData + 16);
            PFXBlob.pbData := @P12Buf[1];
            if NOT PFXExportCertStore(hSystemStore, @PFXBlob, PWideChar(PW), EXPORT_PRIVATE_KEYS) then
                raise EX509Exception.Create('Could not create PFX blob from store - ' + GetWindowsErr(GetLastError));
            if PFXBlob.cbData > 0 then begin
                LoadFromP12Buffer(@P12Buf[1], PFXBlob.cbData, croTry, croTry, InternalPW);
                if IsCertLoaded then Result := 1;
                Result := Result + GetInterCount;
            end;
        end;
    finally
        if hSystemStore <> nil then
            CertCloseStore(hSystemStore, CERT_CLOSE_STORE_FORCE_FLAG);
    end;
end;    *)


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.67 install certificate into the Microsoft Windows certificate store }
{ save main certificate, private key and intermediates to various Windows certificate stores }
{ uses NCrypt key functions }
{ Note - there appears to be some magic required to save private keys correctly to the Windows certificate
  store and then be able to use them for IIS web site bindings, this version gives an error 'A specified
  logon session does not exist' when trying to use the certificate.  }
(*
procedure TMsCertTools.SaveToStoreNc(MsCertLocation: TMsCertLocation; IncludePKey, IncludeInters: Boolean);
var
    hSystemStore: HCERTSTORE;
    pCertCtxCert: PCCERT_CONTEXT;
    hProvider: NCRYPT_PROV_HANDLE;
    hKey: NCRYPT_KEY_HANDLE;
    PropIdBlob: TCryptDataBlob;
    keyProvInfo: TCryptKeyProvInfo;
    ParameterList: TNCryptBufferDesc;
    CryptBuffers: array of TNCryptBuffer;
    InterTot, I, Pkeylen: Integer;
    KeyPolicy, Ret, dwFlags, dwResLen: DWORD;
    Pkeyblob: AnsiString;
    PropWStr: WideString;
    PkeyBio: PBIO;
    Cert: TX509Base;
begin
    if MsStoreProvider = Nil then MsStoreProvider := CERT_STORE_PROV_SYSTEM;
    if not Assigned(X509) then
        raise EX509Exception.Create('X509 not assigned');
    if IncludePKey and (Not Assigned(PrivateKey)) then
        raise EX509Exception.Create('Private key not assigned');
    InterTot := 0;
    if IncludeInters then begin
        InterTot := GetInterCount;
    end;
    if IncludePKey and (NOT CheckCertAndPKey) then
        raise EX509Exception.Create('Certificate and private key do not match');
     hSystemStore := nil;
     hProvider := 0;
     hKey := 0;
     pCertCtxCert := X509ToMsCert(X509);
     if pCertCtxCert = Nil then
        raise EX509Exception.Create('Failed to create store certificate context');

 { add common name as friendly name, necessary to save in persident storage, add cert issue date so unique }
    PropWStr := WideString(SubjectCName) + '_' + RFC3339_DateToStr(ValidNotBefore) + IcsNull;  // needs a null
    PropIdBlob.cbData := Length(PropWStr)*2;
    PropIdBlob.pbData := @PropWStr[1];
    dwFlags := 0;
    if NOT CertSetCertificateContextProperty(pCertCtxCert, CERT_FRIENDLY_NAME_PROP_ID, dwFlags, @PropIdBlob) then
        raise EX509Exception.Create('Could set name prop-id - ' + GetWindowsErr(GetLastError));

    try
        if MsStoreProvider = Nil then
            MsStoreProvider := CERT_STORE_PROV_SYSTEM;

     { private key }
        if IncludePKey then begin
            PkeyBio := BIO_new(BIO_s_mem);
            try
                keyProvInfo.pwszContainerName := @PropWStr[1];
                keyProvInfo.pwszProvName := MS_KEY_STORAGE_PROVIDER;
                keyProvInfo.dwProvType := 0;
                keyProvInfo.dwKeySpec := AT_SIGNATURE;
                if MsCertLocation = MsLocMachine then
                    keyProvInfo.dwFlags := NCRYPT_MACHINE_KEY_FLAG
                else
                    keyProvInfo.dwFlags := 0;

            // get PKCS8 blob for Windows
                if i2d_PKCS8PrivateKey_bio(PkeyBio, PrivateKey, nil, nil, 0, nil, nil) = 0 then
                    raise EX509Exception.Create('Failed to convert private key to PKCS8');
                Pkeylen := BIO_ctrl(PkeyBio, BIO_CTRL_PENDING_, 0, nil);
                Pkeyblob := ReadStrBio(PkeyBio, Pkeylen);
                Ret := NCryptOpenStorageProvider(hProvider, MS_KEY_STORAGE_PROVIDER, 0);
                if Ret <> ERROR_SUCCESS then
                   raise EX509Exception.Create('Could not open key storage provider - ' + GetWindowsErr(Ret));

            // import key using NCryptImportKey
                SetLength(CryptBuffers, 2);
                CryptBuffers[0].BufferType := NCRYPTBUFFER_PKCS_KEY_NAME;
                CryptBuffers[0].pvBuffer := @PropWStr[1];    // same name as certificate
                CryptBuffers[0].cbBuffer := Length(PropWStr)*2;
                CryptBuffers[1].BufferType := NCRYPTBUFFER_PKCS_SECRET; // password
                CryptBuffers[1].pvBuffer := @PropWStr[1];
                CryptBuffers[1].cbBuffer := Length(PropWStr)*2;
                ParameterList.ulVersion := NCRYPTBUFFER_VERSION;
                ParameterList.cBuffers := 1;    // only using key_name, ignoring password
                ParameterList.pBuffers := @CryptBuffers[0];
                dwFlags := NCRYPT_SILENT_FLAG OR NCRYPT_OVERWRITE_KEY_FLAG OR NCRYPT_DO_NOT_FINALIZE_FLAG;
                if MsCertLocation = MsLocMachine then
                    dwFlags := dwFlags OR NCRYPT_MACHINE_KEY_FLAG;
                Ret := NCryptImportKey(hProvider, 0, NCRYPT_PKCS8_PRIVATE_KEY_BLOB, @ParameterList, hKey, @Pkeyblob[1], Pkeylen, dwFlags);
                if Ret <> ERROR_SUCCESS then
                   raise EX509Exception.Create('Could not import private key - ' + GetWindowsErr(Ret));
                if (hKey <> 0) then begin
                  // check key is being saved to correct store
                    dwFlags := NCRYPT_SILENT_FLAG;
                    Ret := NCryptGetProperty(hKey, NCRYPT_KEY_TYPE_PROPERTY, @KeyPolicy, 4, dwResLen, dwFlags);
                    if Ret <> ERROR_SUCCESS then
                       raise EX509Exception.Create('Could not get private key type property - ' + GetWindowsErr(Ret));
                    if (KeyPolicy = 0) and (MsCertLocation = MsLocMachine) then
                       raise EX509Exception.Create('Key in user store instead of local machine');

                  // set key export policy
                    KeyPolicy := NCRYPT_ALLOW_PLAINTEXT_EXPORT_FLAG OR NCRYPT_ALLOW_EXPORT_FLAG;
                    Ret := NCryptSetProperty(hKey, NCRYPT_EXPORT_POLICY_PROPERTY, @KeyPolicy, 4, dwFlags);
                    if Ret <> ERROR_SUCCESS then
                       raise EX509Exception.Create('Could not set private key export properties - ' + GetWindowsErr(Ret));
                    dwFlags := NCRYPT_SILENT_FLAG;
                    NCryptFinalizeKey(hKey, dwFlags);

                 { now set key handle to the certificate properties }
                    dwFlags := 0;
                    if NOT CertSetCertificateContextProperty(pCertCtxCert, CERT_KEY_PROV_INFO_PROP_ID, dwFlags, @keyProvInfo) then
                        raise EX509Exception.Create('Could set key provider info prop-id - ' + GetWindowsErr(GetLastError));
                    if NOT CertSetCertificateContextProperty(pCertCtxCert, CERT_NCRYPT_KEY_HANDLE_PROP_ID, dwFlags, @hKey) then
                        raise EX509Exception.Create('Could set key handle prop-id - ' + GetWindowsErr(GetLastError));
                end;
            finally
                bio_free(PkeyBio);
            end;
        end;

     { Open the Windows certificate personal/my store }
     { MsLocMachine store needs administrator rights to update the HLM registry and fails with access conflict }
        hSystemStore := CertOpenStore(MsStoreProvider, 0, 0, MsCertLocNames[MsCertLocation], MsCertStoreNames [MsStoreMy]);
        if hSystemStore = nil then
            raise EX509Exception.Create('Could not open the personal store - ' + GetWindowsErr(GetLastError));
        if NOT CertAddCertificateContextToStore(hSystemStore, pCertCtxCert, CERT_STORE_ADD_ALWAYS, Nil) then
            raise EX509Exception.Create('Could not add certificate to personal store - ' + GetWindowsErr(GetLastError));
        CertFreeCertificateContext(pCertCtxCert);
        pCertCtxCert := Nil;
        CertCloseStore(hSystemStore, CERT_CLOSE_STORE_FORCE_FLAG);
        hSystemStore := nil;

      { Open the Windows certificate CA store for intermediates, if any }
        if InterTot > 0 then begin
            hSystemStore := CertOpenStore(MsStoreProvider, 0, 0, MsCertLocNames[MsCertLocation], MsCertStoreNames [MsStoreCA]);
            if hSystemStore = nil then
                raise EX509Exception.Create('Could not open the CA store - ' + GetWindowsErr(GetLastError));
            Cert := TX509Base.Create (self);
            for I := 0 to InterTot - 1 do begin
                Cert.X509 := PX509(OPENSSL_sk_value(X509Inters, I));
                if (Cert.IsCertLoaded) and (NOT Cert.SelfSigned) then begin
                    pCertCtxCert := X509ToMsCert(Cert.X509);
                    if NOT CertAddCertificateContextToStore(hSystemStore, pCertCtxCert, CERT_STORE_ADD_ALWAYS, Nil) then
                        raise EX509Exception.Create('Could not add certificate to CA store - ' + GetWindowsErr(GetLastError));
                    CertFreeCertificateContext(pCertCtxCert);
                    pCertCtxCert := Nil;
                end;
            end;
            Cert.Free;
            CertCloseStore(hSystemStore, CERT_CLOSE_STORE_FORCE_FLAG);
            hSystemStore := nil;
        end;

     finally
        if (hProvider <> 0) then
           NCryptFreeObject (hProvider);
        if (hKey <> 0) then
           NCryptFreeObject (hKey);
        if pCertCtxCert <> nil then
            CertFreeCertificateContext(pCertCtxCert);
        if hSystemStore <> nil then
            CertCloseStore(hSystemStore, CERT_CLOSE_STORE_FORCE_FLAG);
      end;
end;    *)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.67 install certificate into the Microsoft Windows certificate store }
{ save main certificate, private key and intermediates to various Windows certificate stores }
{ uses PFXImportCertStore function }
procedure TMsCertTools.SaveToStorePfx(MsCertLocation: TMsCertLocation; IncludePKey, IncludeInters: Boolean);
var
    hTempStore, hSystemStore: HCERTSTORE;
    pCertContext: PCCERT_CONTEXT;
    P12Buf: AnsiString;
    P12KeyCipher: TSslPrivKeyCipher;
    PFXBlob: TCryptDataBlob;
    dwFlags: DWORD;
    InterTot, I: Integer;
    Cert: TX509Base;
    PropIdBlob: TCryptDataBlob;
    PropWStr, PWWStr: WideString;
begin
    if MsStoreProvider = Nil then MsStoreProvider := CERT_STORE_PROV_SYSTEM;
    if not Assigned(X509) then
        raise EX509Exception.Create('X509 not assigned');
    if IncludePKey and (Not Assigned(PrivateKey)) then
        raise EX509Exception.Create('Private key not assigned');
    if IncludePKey and (NOT CheckCertAndPKey) then
        raise EX509Exception.Create('Certificate and private key do not match');
    InterTot := 0;
    if IncludeInters then begin
        InterTot := GetInterCount;
    end;
    hSystemStore := Nil;
    hTempStore := Nil;
    pCertContext := Nil;

 { add common name as friendly name with valid from date so unique }
    PropWStr := WideString(SubjectCName) + '_' + RFC3339_DateToStr(ValidNotBefore) + IcsNull;  // needs a null
    PropIdBlob.cbData := Length(PropWStr)*2;
    PropIdBlob.pbData := @PropWStr[1];

 // get PKCS12 blcb from certificate and key
  {  P12KeyCipher := PrivKeyEncAES256;   // currently seems to work without encryption
    if (ICS_OPENSSL_VERSION_MAJOR < 3) or ICS_OSSL3_LOADED_LEGACY then
        P12KeyCipher := PrivKeyEncTripleDES;   }
    P12KeyCipher := PrivKeyEncNone;
    P12Buf := SaveToP12Buf(InternalPW, False, P12KeyCipher);
    PWWStr := {InternalPW +} IcsNull;

 // save blob into temporary store
    PFXBlob.cbData := Length(P12Buf);
    PFXBlob.pbData := @P12Buf[1];

 // check our blob
    dwFlags := 0;
    if NOT PFXIsPFXBlob(@PFXBlob) then
        raise EX509Exception.Create('Invalid PFX blob - ' + GetWindowsErr(GetLastError));
    if NOT PFXVerifyPassword(@PFXBlob, PWideChar(PWWStr), dwFlags) then
        raise EX509Exception.Create('Invalid internal password for PFX blob');

  // import blob into windows store
  // unfortunately there is no equivalent of NCRYPT_ALLOW_PLAINTEXT_EXPORT_FLAG so we can not export as PKCS8
    dwFlags := CRYPT_EXPORTABLE OR PKCS12_ALLOW_OVERWRITE_KEY OR PKCS12_ALWAYS_CNG_KSP OR PKCS12_INCLUDE_EXTENDED_PROPERTIES;
    if MsCertLocation = MsLocMachine then
       dwFlags := dwFlags OR CRYPT_MACHINE_KEYSET
    else
       dwFlags := dwFlags OR CRYPT_USER_KEYSET;
    try
    // this API does some magic placing the private key in the store than NCryptImportKey does not manage }
        hTempStore := PFXImportCertStore(@PFXBlob, PWideChar(PWWStr), dwFlags);
        if hTempStore = nil then
            raise EX509Exception.Create('Could not import PFX blob into store - ' + GetWindowsErr(GetLastError));

    // Open the Windows certificate personal/my store }
    // MsLocMachine store needs administrator rights to update the HLM registry and fails with access conflict
        hSystemStore := CertOpenStore(MsStoreProvider, 0, 0, MsCertLocNames[MsCertLocation], MsCertStoreNames [MsStoreMy]);
        if hSystemStore = nil then
            raise EX509Exception.Create('Could not open the personal store - ' + GetWindowsErr(GetLastError));

    { Enum the cert in the store and store them in the system store - should only be one }
        pCertContext := CertEnumCertificatesInStore(hTempStore, pCertContext);
        while pCertContext <> nil do begin
            dwFlags := 0;
            CertSetCertificateContextProperty(pCertContext, CERT_FRIENDLY_NAME_PROP_ID, dwFlags, @PropIdBlob);
            if NOT CertAddCertificateContextToStore(hSystemStore, pCertContext, CERT_STORE_ADD_ALWAYS, Nil) then
                raise EX509Exception.Create('Could not add certificate to personal store - ' + GetWindowsErr(GetLastError));
            pCertContext := CertEnumCertificatesInStore(hTempStore, pCertContext);
        end;
        CertCloseStore(hSystemStore, CERT_CLOSE_STORE_FORCE_FLAG);
        hSystemStore := nil;

      { Open the Windows certificate CA store for intermediates, if any }
        if InterTot > 0 then begin
            hSystemStore := CertOpenStore(MsStoreProvider, 0, 0, MsCertLocNames[MsCertLocation], MsCertStoreNames [MsStoreCA]);
            if hSystemStore = nil then
                raise EX509Exception.Create('Could not open the CA store - ' + GetWindowsErr(GetLastError));
            Cert := TX509Base.Create (self);
            for I := 0 to InterTot - 1 do begin
                Cert.X509 := PX509(OPENSSL_sk_value(X509Inters, I));
                if (Cert.IsCertLoaded) and (NOT Cert.SelfSigned) then begin
                    pCertContext := X509ToMsCert(Cert.X509);
                    if NOT CertAddCertificateContextToStore(hSystemStore, pCertContext, CERT_STORE_ADD_ALWAYS, Nil) then
                        raise EX509Exception.Create('Could not add certificate to CA store - ' + GetWindowsErr(GetLastError));
                    CertFreeCertificateContext(pCertContext);
                end;
            end;
            Cert.Free;
            CertCloseStore(hSystemStore, CERT_CLOSE_STORE_FORCE_FLAG);
            hSystemStore := nil;
        end;

    finally
        if hSystemStore <> nil then
            CertCloseStore(hSystemStore, CERT_CLOSE_STORE_FORCE_FLAG);
        if hTempStore <> nil then
            CertCloseStore(hTempStore, 0);
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.67 does program have administrator access }
(* V8.71 moved to Utils
function IcsIsProgAdmin: Boolean;
var
    psidAdmin: Pointer;
    Token: THandle;
    Count: DWORD;
    TokenInfo: PTokenGroups;
    HaveToken: Boolean;
    I: Integer;
const
    SE_GROUP_USE_FOR_DENY_ONLY  = $00000010;
    SECURITY_NT_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
    SECURITY_BUILTIN_DOMAIN_RID  = ($00000020);
    DOMAIN_ALIAS_RID_ADMINS      = ($00000220);
begin
    Result := False;
    if Win32Platform <> VER_PLATFORM_WIN32_NT then
    begin
       result := true ;
       exit ;
    end ;
    psidAdmin := nil;
    TokenInfo := nil;
    HaveToken := False;
    try
        HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, Token);
        if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
            HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token);
        if HaveToken then begin
            Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
                SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdmin));
            if GetTokenInformation(Token, TokenGroups, nil, 0, Count) or
                                (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
                RaiseLastOSError;
            TokenInfo := PTokenGroups(AllocMem(Count));
            Win32Check(GetTokenInformation(Token, TokenGroups, TokenInfo, Count, Count));
            for I := 0 to TokenInfo^.GroupCount - 1 do begin
            {$RANGECHECKS OFF} // Groups is an array [0..0] of TSIDAndAttributes, ignore ERangeError
                Result := EqualSid(psidAdmin, TokenInfo^.Groups[I].Sid) and
                      (TokenInfo^.Groups[I].Attributes and SE_GROUP_USE_FOR_DENY_ONLY = 0); //Vista??
            {$IFDEF RANGECHECKS_ON}
            {$RANGECHECKS ON}
            {$ENDIF RANGECHECKS_ON}
                if Result then
                    Break;
            end;
        end;
    finally
        if TokenInfo <> nil then
            FreeMem(TokenInfo);
        if HaveToken then
            CloseHandle(Token);
        if psidAdmin <> nil then
            FreeSid(psidAdmin);
    end;
end;   *)


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF} // MSWINDOWS
{$ENDIF} // USE_SSL
end.
