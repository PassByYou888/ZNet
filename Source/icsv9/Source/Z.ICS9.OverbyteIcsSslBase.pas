{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  SSL/TLS base function, certificate handling, with TSslBaseComponent,
              TX509Base and TX509List.
Creation:     Nov 2023 (split from OverbyteIcsWSocket, 1996)
Updated:      Jun 2025
Version:      V9.5
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1996-2025 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany

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

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Jan 22, 2024 V9.1 Baseline.
                  Moved TSslContext, TSslBaseComponent, T509Base and TX509List here from
                    OverbyteIcsWSocket which is a massive unit, but left TSslContext
                    callbacks in WSocket since they need access to it, now set in
                    TWSocket.SetSslCallbacks instead of InitContext.
                  Moved function sslRootCACertsBundle here from X509Utils.
                  Added property X509PubKeyTB to TX509Base to get the certificate public
                    in DER binary format as TBytes, from where it may be converted to hex
                    or base64, used for Raw Public Key certificate validation.
                  Made more TX509Base functions and variables public so they can
                    be accessed from other units.
                  Added DHE-RSA-CHACHA20-POLY1305 to TLS/1.2 sslCiphersMozillaSrvTLS12.
                  Added IcsReadTBBio, IcsWriteStrBio, IcsWriteTBBio, IcsSslLoadStackFromP12TB
                    which are internal functions for handling TBytes and certificates, to
                    simplify code (we use too many AnsiStrings for binary data).
                  TX509List can now load and save PKCS#12 certificate bundle files, smaller
                    than PEM files, added SaveToP12File, SaveToP12TB, LoadAllFromP12File,
                    LoadAllFromP12TB, intended to load a certificate bundle. LoadAllFromPemFile
                    and LoadAllFromPemTB renamed from LoadAllFromFileEx and LoadAllFromFStringEx
                    with new versions handling both PEM and PKCS#12 certificate bundle files.
                  TX509List has new method ListCerts that returns one listing line per cert.
                  Added new TSslRootCAStore component derived from TX509List with an Initialize
                    method that loads OpenSSL, then tries to load the internal certificate
                    sslRootCACertsBundle that should be linked into the app, if missing then
                    tries to load DefRootCABundle.pem from C:\ProgramData\ICS-OpenSSL\ or the a
                    pp path. It also tries to load ExtraRootCABundle.pem which is an optional
                    private root bundle that can be used for private customer or devel roots.
                  Added public IcsSslRootCAStore component created and intialised when this
                    unit is loaded so a common root store is ready for any SslContext or other
                    components. Define OpenSSL_AutoLoad_CA_Bundle can be suppressed to stop
                    OpenSSL and the bundled being automatically loaded, if not needed.
                  SslContext has new property UseSharedCAStore which causes the properties
                    CAFile, CALines and CAPath to be ignored, uses IcsSslRootCAStore instead.
                  Added function IcsReportOpenSSLVer to centralise version reporting,
                    optionally adding number of CA root certificates loaded.
                  Saving a private key with a PCKS12 file is now optional.
                  Moved BuildCertFName here from WSocketS as IcsIcsBuildCertFName.
                  ICSRootCA.pem and ICS_Intermediate_Short-bundle.pem certificates linked
                    as resources, root is added to IcsSslRootCAStore.
Apr 08, 2024 V9.2 Builds with D7 again.
Sep 18, 2024 V9.3 TSslRootCAStore now loads resource linked roots on Posix.
                  Ignore P12 TripleDES encryption unless legacy provider is loaded.
                  Added IcsSslLoad and IcsSslUnload as better named methods to load OpenSSL.
                  Added IcsSetSignTest if called before OpenSSL is loaded, will check DLLs
                    are digitally signed (avoids setting global variables).
                  Added IcsGetOpenSSLVer if non-zero, the OpenSSL version number that can
                    be compared against OSSL_VER_xx literals, less or greater.
                  Moved IcsX509VerifyErrorToStr here from Libeay
                  Removed unused cipher suites, updated clients to use better ciphers.
                  If SslCryptoGroups are specified, actually load them, thanks to Roger Tinembart.
                  SslContext has new property SslCipherList13 which defaults to the ciphers
                    supported by TLSv1.3, but which may be changed if required, note the
                    name format is different to CipherList, thanks to Roger Tinembart.
                  Corrected IcsSslLoadStackFromInfoFileEx so CRLs are also loaded from a
                    PEM bundle file, thanks to Roger Tinembart.
Oct 04, 2024 V9.4 Using new IcsPkey functions to avoid calling OpenSSL APIs directly.
Aug 25, 2025 V9.5 ClientHello reports more extensions and groups from new OpenSSL versions.
                  TClientHelloData record has CliCertType and SrvCertType, and report them.
                  Added TX509Base SerialNumTB property with binary certificate serial number.
                  Added TlsCertTypes to SslContext to support raw public keys.
                  Added TX509Base ListAltNameDNS and ListAltNameIP properties that return
                    subject alternate DNS names and IPs as a TStringDynArray, easier to process
                    than multiple lines returned by SubAltNameDNS.
                  GetKeyDesc no longer uses deprecated OpenSSL functions.
                  TSslContext.LoadDHParams will fail with an except without DEFINE
                    OpenSSL_Deprecated, no longer needed for modern cyphers.
                  TX509Base.ComparePkey updated to use new OpenSSL function.


           pending Post Quantum Cryptography (PQC) stuff

Note: if you need a public key pointer from a private key pointer, you need to copy via
text export, a private key can not contain a public key pointer and no simple OpenSSL API.
PubKey(TX509Base).PublicKeyLoadFromText(PrivKey(TX509Base).PrivateKey.PublicKeySaveToText)

TSslRootCAStore is made public as IcsSslRootCAStore for common use by SslContexts and other
          SSL components that need to check certificates against public roots.


Pending - server certificate bundle files may not have server certificate as first
Pending - intermediate certificate bundle files may have self signed root that should be ignored
Both these are fixed for certificates ordered by TSslX509Certs.


Use of certificates for SSL clients:
Client SSL applications will usually work without any certificates because all
the encryption is done by the server.  If a client needs to confirm the identity
of a server, set SslVerifyPeer=true and specify a certificate authority root
bundle as SslCAFile, SslCAPath or SslCALines, that contains the certificates
used to sign the server certificate or intermediate certificate, to confirm
they are trusted.  To permanently trust an unknown certificate, save it to
the CA file or path, or add it temporarily using TrustCert.

More rarely in high security operations, the server will need
a client to identify itself with a private certificate before granting access,
and this is where a client SSL certificate and private key are needed.  Client
certificate checking is controlled by the server.  An SslPassPhrase is only
needed if the private key is password protected.

Use of certificates for SSL servers:
Server SSL applications always require an SSL certificate and matching private
key because these control the SSL encryption.  The certificate may also confirm
the identity of the web site using the domain name and often the company name.
To be trusted by browsers and other applications, the SSL certificate needs to
be signed by a root certificate available for local checking.  SSL certificates
are often signed by intermediate certificates rather than root certificates, and
these also need to sent by the server as part of a chain, the intermediate will
have been signed by a trusted root certificate.  To configure an SSL server,
SslCertFile or SslCertLines specify the SSL certificate and optionally
intermediate certificates in same file as a bundle; SslPrivKeyFile or
SslPrivKeyLines specify the private key used to generate the certificate, which
may be optionally password protected by SslPassPhrase; and SslCAFile, SslCAPath
or SslCALines specifies the intermediate certificates if not in the certificate
bundle file.  Also, SslDHParamFile or SslDHParamLines should specify DHParams
which are a secondary encryption key used for some ciphers, ICS has default
DHParams but ideally applications should use unique DHParams.

Sometimes SSL certificates are withdrawn due to misuse such as being stolen
and appear in Certificate Revocation Lists (CRL) that are published by SSL
certificate issuers.  Such lists in PEM format may be loaded by
LoadCrlFromFile or LoadCrlFromPath.

Rarely, a server may want to check the identify of clients by requesting a
client SSL certificate by setting SslVerifyPeerModes=SslVerifyMode_PEER.
AddClientCAFromFile and SetClientCAListFromFile are used to set acceptable
CAs For the client certificate.

OpenSSL 1.1.0 and later support security levels, as follows:
    TSslSecLevel = (
         sslSecLevelAny,        // 0 - anything allowed, old compatibility
         sslSecLevel80bits,     // 1 - default, RSA/DH keys=>1024, ECC=>160, no MD5
         sslSecLevel112bits,    // 2 - RSA/DH keys=>2048, ECC=>224, no RC4, no SSL3, no SHA1 certs
         sslSecLevel128bits,    // 3 - RSA/DH keys=>3072, ECC=>256, FS forced, no TLS/1.0
         sslSecLevel192bits,    // 4 - RSA/DH keys=>7680, ECC=>384, no SHA1 suites, no TLS/1.1
         sslSecLevel256bits);   // 5 - RSA/DH keys=>15360, ECC=>512


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsSslBase;
{$ENDIF}

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long strings                    }
{$J+}           { Allow typed constant to be modified }
{$ALIGN 8}
{$I Include\Z.ICS9.OverbyteIcsDefs.inc}

{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       ON}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF POSIX}                 { V8.65 }
    {$DEFINE PUREPASCAL}
{$ENDIF}

interface

{$IFDEF USE_SSL}   // unit does not build without SSL

uses
{$IFDEF MSWINDOWS}
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
  {$IFDEF RTL_NAMESPACES}System.Types{$ELSE}Types{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Contnrs{$ELSE}Contnrs{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Masks{$ELSE}Masks{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SysConst{$ELSE}SysConst{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.TypInfo{$ELSE}TypInfo{$ENDIF},
{$IFDEF POSIX}
  System.Generics.Collections,
  Posix.Pthread,
  Posix.SysTypes,
  Posix.Base,
  Posix.Errno,
  Z.ICS9.Ics.Posix.WinTypes,
{$ENDIF}
  Z.ICS9.OverbyteIcsSSLEAY,
  Z.ICS9.OverbyteIcsLIBEAY,
{$IFNDEF NO_DEBUG_LOG}
  Z.ICS9.OverbyteIcsLogger,
{$ENDIF}
  Z.ICS9.OverbyteIcsUtils,
 {$IFDEF YuOpenSSL}YuOpenSSL,{$ENDIF YuOpenSSL}    { V8.66 }
  Z.ICS9.OverbyteIcsTicks64,                              { V8.71 }
  Z.ICS9.OverbyteIcsTypes; // for TBytes and TThreadID V9.2

{$R ICSCerts.RES}       { V9.1 ICSRootCA.pem and ICS_Intermediate_Short-bundle.pem certificates }

const
     sslProtocolError                 = 20100;
     msgSslCtxNotInit                 = 'SSL context not initialized';

  { V8.10 - TSslContext default SslCipherList
    note a client generally wants to talk to as many servers as possible so use sCiphersNormal,
    while a server wants to force clients to use the highest security possible, and Mozilla
    kindly publishes it's internal recommendations for OpenSSL server configuration, see below. }
    sslCiphersNormal = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH';
    sslCiphersServer = 'TLSv1+HIGH:!SSLv2:RC4+MEDIUM:!aNULL:!eNULL:!3DES:!CAMELLIA@STRENGTH';

  { V8.46 similar to normal but blocking DH and DHE ciphers, needed for forums.embarcadero.com }
    sslCiphersNoDH = 'ALL:!ADH:!DH:RC4+RSA:+SSLv2:@STRENGTH';

  { V8.51 1.1.1 and later }
  { V9.3 OpenSSL 3.3 supported curve groups for TLSv1.3 are P-256, P-384, P-521, X25519, X448, brainpoolP256r1tls13,
     brainpoolP384r1tls13, brainpoolP512r1tls13, ffdhe2048, ffdhe3072, ffdhe4096, ffdhe6144 and ffdhe8192.  }
    sslCryptoGroupsDef = 'P-256:X25519:P-384:P-521';

  { V9.5 pending Post Quantum (PQ) groups, beware P384 is CPU intensive, all cause longer key exchange messages }
  // X25519MLKEM768:SecP256r1MLKEM768:SecP384r1MLKEM1024


{ from https://wiki.mozilla.org/Security/Server_Side_TLS - Version 4.0 - February 2016, newer versions below
   Note these ciphers change peridically, old ones remain with their version
    Configuration   Oldest compatible client
        sslCiphersMozillaSrvHigh - Firefox 27, Chrome 30, IE 11, Edge, Opera 17, Safari 9, Android 5, Java 8
        sslCiphersMozillaSrvInter -  Firefox 1, Chrome 1, IE 7, Opera 5, Safari 1, Windows XP IE8, Android 2.3, Java 7
        sslCiphersMozillaSrvBack - Windows XP IE6, Java 6 }

    { Nov 2017 - beware the Mozilla ciphers don't support TLS/1.3 yet, but ICS adds special TLS/1.3
      ciphers to these lists if enabled }
    { Nov 2023 V9.1 added DHE-RSA-CHACHA20-POLY1305 per Mozilla V5.7  }

   { Backward Compatible, works with all clients back to Windows XP/IE6,  Versions: SSLv3, TLSv1, TLSv1.1, TLSv1.2
    RSA key size: 2048, DH Parameter size: 1024, Elliptic curves: secp256r1, secp384r1, secp521r1,
    Certificate signature: sha1WithRSAEncryption (windows XP pre-sp3 is incompatible with sha-256)  }
   { note SSLv3, TLSv1, TLSv1.1 are not supported by OpenSSL 3.0 and later }
    sslCiphersMozillaSrvBack =
        'ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-RSA-AES128-GCM-SHA256:' +
        'ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384:' +
        'DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:kEDH+AESGCM:ECDHE-RSA-AES128-SHA256:' +
        'ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:' +
        'ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:DHE-RSA-AES128-SHA256:' +
        'DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA:DHE-RSA-AES256-SHA:' +
        'ECDHE-RSA-DES-CBC3-SHA:ECDHE-ECDSA-DES-CBC3-SHA:EDH-RSA-DES-CBC3-SHA:AES128-GCM-SHA256:' +
        'ES256-GCM-SHA384:AES128-SHA256:AES256-SHA256:AES128-SHA:AES256-SHA:AES:DES-CBC3-SHA:HIGH:SEED:' +
        '!aNULL:!eNULL:!EXPORT:!DES:!RC4:!MD5:!PSK:!RSAPSK:!aDH:!aECDH:!EDH-DSS-DES-CBC3-SHA:!KRB5-DES-CBC3-SHA:!SRP' ;

  { For services that don't need backward compatibility, the parameters below provide a higher level of security
   Versions: TLSv1.2, RSA key size: 2048, DH Parameter size: none, Elliptic curves: secp256r1, secp384r1, secp521r1,
    TLS curves: prime256v1, secp384r1, secp521r1, Certificate ECDSA, signature sha256WithRSAEncryption, ecdsa-with-SHA256,
    ecdsa-with-SHA384, ecdsa-with-SHA512, ECDH Parameter size: 256, HSTS: max-age=15724800  }
  { V8.66 no longer used by IcsHosts, sslCiphersMozillaSrvTLS12 instead }
  {  sslCiphersMozillaSrvHigh =
        'ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:' +
        'ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:' +
        'ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA256';  }

  {Intermediate compatibility in 2016 - For services that don't need compatibility with legacy clients (mostly WinXP),
   but still need to support a wide range of clients, this configuration is recommended. It is is compatible with
   Firefox 1, Chrome 1, IE 7, Opera 5 and Safari 1.   Versions: TLSv1, TLSv1.1, TLSv1.2,  RSA key size: 2048
    DH Parameter size: 2048, Elliptic curves: secp256r1, secp384r1, secp521r1 (at a minimum)
    ECDH Parameter size: 256, Certificate signature: sha256WithRSAEncryption}
  { V8.66 no longer used by IcsHosts, sslCiphersMozillaSrvTLS12 instead }
  {  sslCiphersMozillaSrvInter =
        'ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:' +
        'ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:' +
        'DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA256:' +
        'ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES256-SHA384:' +
        'ECDHE-ECDSA-AES256-SHA:ECDHE-RSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-RSA-AES256-SHA256:' +
        'DHE-RSA-AES256-SHA:ECDHE-ECDSA-DES-CBC3-SHA:ECDHE-RSA-DES-CBC3-SHA:EDH-RSA-DES-CBC3-SHA:AES128-GCM-SHA256:' +
        'AES256-GCM-SHA384:AES128-SHA256:AES256-SHA256:AES128-SHA:AES256-SHA:DES-CBC3-SHA:!DSS';  }

  { V8.41 similar to Intermediate compatibility but removing all ciphers without forward security }
  { V8.66 no longer used by IcsHosts, sslCiphersMozillaSrvTLS12 instead }
 {   sslCiphersMozillaSrvInterFS =
        'ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:' +
        'ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:' +
        'DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA256:' +
        'ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES256-SHA384:' +
        'ECDHE-ECDSA-AES256-SHA:ECDHE-RSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-RSA-AES256-SHA256:' +
        'DHE-RSA-AES256-SHA:ECDHE-ECDSA-DES-CBC3-SHA:ECDHE-RSA-DES-CBC3-SHA';  }

  { V8.52 TLSv1.3 ciphers supported by OpenSSL 1.1.1 and later, should be added to front
    of one of the sslCiphersMozilla ciphers, v8.66 removed CCM ciphers }
  { V9.1 Comptabile with Firefox 63, Android 10.0, Chrome 70, Edge 75, MSIE none, Opera 57, Safari 12.1, Java 11 }
  { V9.3 gone, ciphers now configurable }
//    sslCipherTLS13 =
//        'TLS13-CHACHA20-POLY1305-SHA256:TLS13-AES-256-GCM-SHA384:TLS13-AES-128-GCM-SHA256:';
 //        { 'TLS13-AES-128-CCM-8-SHA256:TLS13-AES-128-CCM-SHA256:';  }
  { V9.3 TLSv1.3 ciphers supported by OpenSSL 3.0 using the SSL_CTX_set_ciphersuites function }
    sslCipherSuitesTLS13 =
        'TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256';

  {Intermediate compatibility in 2023 - For services that don't need compatibility with legacy clients (TLS1 and 1.1),
   but still need to support a wide range of clients, this configuration is recommended. It is is compatible with
   Firefox 27, Chrome 31, IE 11 (Win7), Opera 20 and Safari 9.   Versions: TLSv1.2, TLSv1.3,  RSA key size: 2048
    DH Parameter size: 2048, Elliptic curves: secp256r1, secp384r1, secp521r1 (at a minimum)
    ECDH Parameter size: 256, Certificate signature: sha256WithRSAEncryption, forward security }
    sslCiphersMozillaSrvTLS12 =
         'ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:' +
         'ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:' +
         'DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384:DHE-RSA-CHACHA20-POLY1305';

   { V8.66 In 2021 Mozilla now recommends TLSv1.3 as modern ciphers and TLSv1.2/1.3
      as Intermediate supporting all browsers from last five years, so IcsHosts now
      use sslCiphersMozillaSrvTLS12 as Intermediate level, also Mozilla
       recommends no cipher server preference so changed that. }

{$IFDEF OpenSSL_Deprecated}   { V9.5 }
   { V8.27 default 2048 and 4096-bit DH Params needed for DH/DHE ciphers - ideally create your own !!!! }
   { note DH params are not needed for ECDHE ciphers, only DHE, so not needed for sslCiphersMozillaSrvHigh }
    sslDHParams2048 =
        '-----BEGIN DH PARAMETERS-----' + #13#10 +
        'MIIBCAKCAQEA5lgSzWKPV8ZthosYUuPWuawgmUFfSyR/1srizVn7tXNPYE10Pz/t' + #13#10 +
        'z1i0f1JppaoBBdFQMQnVlTrZjEIinavAZwLH9HRbmjvglO0gNL46NpgzgcXQbKbn' + #13#10 +
        'jZs4BSFF9LbhP4VvvIIKI7lR/yQFNw5GtKtV+Pi/tZ5dCaRvALadAtzAXOmEadv0' + #13#10 +
        'KNZXc7hONXf9kyRmtwr6C5AdeIH50enVBss6zRwwGi3fW7e5D6z3FvUrHzD9fot+' + #13#10 +
        'y89hX5iXD/v3BurTkN3rG12JoTypQ3W1VD1lEfRrJm8rbvQTqO0RCSgxc2KwIULb' + #13#10 +
        '3ONsf1ln/Lb+UuRiUpGeb4GQqPDkn7XW8wIBAg==' + #13#10 +
        '-----END DH PARAMETERS-----' + #13#10;

    sslDHParams4096 =
        '-----BEGIN DH PARAMETERS-----' + #13#10 +
        'MIICCAKCAgEA45KZVdTCptcakXZb7jJvSuuOdMlUbl1tpncHbQcYbFhRbcFmmefp' + #13#10 +
        'bOmZsTowlWHQpoYRRTe6NEvYox8J+44i/X5cJkMTlIgMb0ZBty7t76U9f6qAId/O' + #13#10 +
        '6elE0gnk2ThER9nmBcUA0ZKgSXn0XCBu6j5lzZ0FS+bx9OVNhlzvIFBclRPXbI58' + #13#10 +
        '71dRoTjOjfO1SIzV69T3FoKJcqur58l8b+no/TOQzekMzz4XJTRDefqvePhj7ULP' + #13#10 +
        'Z/Zg7vtEh11h8gHR0/rlF378S05nRMq5hbbJeLxIbj9kxQunETSbwwy9qx0SyQgH' + #13#10 +
        'g+90+iUCrKCJ9Fb7WKqtQLkQuzJIkkXkXUyuxUuyBOeeP9XBUAOQu+eYnRPYSmTH' + #13#10 +
        'GkhyRbIRTPCDiBWDFOskdyGYYDrxiK7LYJQanqHlEFtjDv9t1XmyzDm0k7W9oP/J' + #13#10 +
        'p0ox1+WIpFgkfv6nvihqCPHtAP5wevqXNIQADhDk5EyrR3XWRFaySeKcmREM9tbc' + #13#10 +
        'bOvmsEp5MWCC81ZsnaPAcVpO66aOPojNiYQZUbmm70fJsr8BDzXGpcQ44+wmL4Ds' + #13#10 +
        'k3+ldVWAXEXs9s1vfl4nLNXefYl74cV8E5Mtki9hCjUrUQ4dzbmNA5fg1CyQM/v7' + #13#10 +
        'JuP6PBYFK7baFDjG1F5YJiO0uHo8sQx+SWdJnGsq8piI3w0ON9JhUvMCAQI=' + #13#10 +
        '-----END DH PARAMETERS-----' + #13#10;
{$ENDIF OpenSSL_Deprecated}   { V9.5 }

{$IFNDEF NO_SSL_MT}
var
     LockPwdCB          : TIcsCriticalSection;
     LockVerifyCB       : TIcsCriticalSection;
     LockInfoCB         : TIcsCriticalSection;
     LockRemSessCB      : TIcsCriticalSection;
     LockNewSessCB      : TIcsCriticalSection;
     LockGetSessCB      : TIcsCriticalSection;
     LockClientCertCB   : TIcsCriticalSection;
     LockServerNameCB   : TIcsCriticalSection;
{$ENDIF}

type
    EOpenSslError = class(Exception);
    TSslBaseComponent = class(TComponent)
    protected
        FSslInitialized : Boolean;
        FLastSslError   : Cardinal;                 { V8.66 was Integer }
        FLastSslErrMsg  : String;                   { V8.55 }
        FSslPWUtf8      : Boolean;                  { V8.55 }

    {$IFNDEF NO_DEBUG_LOG}                                             { V5.21 }
        FIcsLogger  : TIcsLogger;
        procedure   SetIcsLogger(const Value : TIcsLogger); virtual;   { V5.21 }
        procedure   Notification(AComponent  : TComponent;             { V5.21 }
                                 Operation   : TOperation); override;
        procedure   DebugLog(LogOption : TLogOption;                   { V5.21 }
                             const Msg : string); virtual;
        function    CheckLogOptions(const LogOption: TLogOption): Boolean; virtual; { V5.21 }
    {$ENDIF}
        procedure   RaiseLastOpenSslError(EClass          : ExceptClass;
                                          Dump            : Boolean = FALSE;
                                          const CustomMsg : String  = ''); virtual;
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Assign(Source: TPersistent); override;                 { V8.71 }
        procedure   InitializeSsl;                                         { V8.41 was protected }   { V9.3 not inline }
        procedure   FinalizeSsl;                                           { V8.41 }
        function    PasswordConvert(const PW: String): AnsiString;   { V8.55 }
        property    LastSslError : Cardinal               read FLastSslError;    { V8.66 was Integer }
        property    LastSslErrMsg : String                read FLastSslErrMsg;   { V8.55 }
        property    IsSslInitialized: Boolean             read FSslInitialized;  { V8.41 }
{$IFNDEF NO_DEBUG_LOG}
    published
        property    IcsLogger : TIcsLogger                read  FIcsLogger    { V5.21 }
                                                          write SetIcsLogger;
{$ENDIF}
    end;

{$IFNDEF COMPILER6_UP}
const                                                             {AG 02/06/06}
    MinDateTime: TDateTime = -657434.0;      { 01/01/0100 12:00:00.000 AM }
    MaxDateTime: TDateTime =  2958465.99999; { 12/31/9999 11:59:59.999 PM }
{$ENDIF}
type
    EX509Exception = class(Exception);

    TExtension = record
        Critical  : Boolean;
        ShortName : String;
        Value     : String; // may be also one or multiple Name=value pairs,
    end;                    // separated by a CRLF
    PExtension = ^TExtension;

    THashBytes20 = array of Byte;

type
    { V8.40 added read only, V8.41 added WriteBin }
    TBioOpenMethode = (bomRead, bomWrite, bomReadOnly, bomWriteBin);

    TX509List  = class;

    TX509Base = class(TSslBaseComponent)
    private
        FX509               : Pointer;     { PX509, ^TX509_st }
        FPrivateKey         : Pointer;     { PEVP_PKEY, ^TEVP_PKEY_st  }
        FSha1Digest         : THashBytes20;
        FSha1Hex            : String;
        FX509Inters         : PStack;        { V8.41 Stack of PX509 }
        FSha256Digest       : THashBytes20;  { V8.63 }
        FSha256Hex          : String;        { V8.63 }
        FListAltNameDNS     : TStringDynArray; { V9.5 }
        FListAltNameIP      : TStringDynArray; { V9.5 }
    protected
        FVerifyResult       : Integer;  // current verify result
        FVerifyDepth        : Integer;
        FCustomVerifyResult : Integer;
        FFirstVerifyResult  : Integer;                      {05/21/2007 AG}
        FComments           : String;                     { V8.67 }
        FCertName           : String;                     { V8.67 }
        FKeyName            : String;                     { V8.67 }
        procedure   SetX509(X509: Pointer);
        procedure   SetPrivateKey(PKey: Pointer);
        procedure   SetX509Inters(X509Inters: PStack);   { V8.41 }
        function    GetX509PublicKey: Pointer;           { V8.52 renamed from GetPublicKey }
        function    GetVerifyErrorMsg: String;
        function    GetFirstVerifyErrorMsg: String;         {05/21/2007 AG}
        function    GetIssuerOneLine: String;
        function    GetSubjectOneLine: String;
        function    GetSerialNum: Int64; virtual;           { V8.40 was integer }
        function    GetSubjectCName: String;
        function    GetSubjectAltName: TExtension; virtual;
        function    GetExtension(Index: Integer): TExtension; virtual;
        function    GetExtensionCount: Integer;
        function    ExtByName(const ShortName: String): Integer;
        function    GetValidNotBefore: TDateTime;              {AG 02/06/06}
        function    GetValidNotAfter: TDateTime;               {AG 02/06/06}
        function    GetHasExpired: Boolean;                    {AG 02/06/06}
        function    GetSelfSigned: Boolean;
        function    UnknownExtDataToStr(Ext: PX509_Extension) : String;
        function    GetSha1Hash: AnsiString; deprecated
          {$IFDEF COMPILER12_UP}'Use GetSha1Digest or GetSha1Hex'{$ENDIF};
        function    GetSha1Digest: THashBytes20;
        function    GetSha1Hex: String;                { aka fingerprint }
       { V8.39 moved these from TX509Ex }
        function    GetSubjectOName : String;
        function    GetSubjectOUName : String;
        function    GetSubjectCOName: String;
        function    GetSubjectSTName: String;
        function    GetSubjectLName: String;
        function    GetSubjectEmailName: String;
        function    GetSubjectSerialName: String;
        function    GetSubAltNameDNS: String;
        function    GetSubAltNameIP: String;
        function    GetKeyUsage: String;
        function    GetExKeyUsage: String;
        function    GetBasicConstraints: String;
        function    GetAuthorityInfoAccess: String;
        function    GetIssuerOName: String;
        function    GetIssuerOUName: String;
        function    GetIssuerCName: String;
        function    GetIssuerCOName: String;
        function    GetIssuerSTName: String;
        function    GetIssuerLName: String;
        function    GetIssuerEmailName: String;
        function    GetSignAlgo: String;
        function    GetKeyInfo: string;
        function    GetSerialNumHex: String;
        function    GetKeyDesc(pkey: PEVP_PKEY): string;                   { V8.41 }
        function    GetPrivateKeyInfo: string;                             { V8.40 }
        function    GetCertPolicies: String;                               { V8.40 }
        function    GetAuthorityKeyId: String;                             { V8.40 }
        function    GetSubjectKeyId: String;                               { V8.40 }
        function    GetCRLDistribution: String;                            { V8.40 }
        function    GetExtendedValidation: boolean;                        { V8.40 }
        function    GetIsCertLoaded: Boolean;                              { V8.41 }
        function    GetIsPKeyLoaded: Boolean;                              { V8.41 }
        function    GetIsInterLoaded: Boolean;                             { V8.41 }
        function    GetInterCount: Integer;                                { V8.41 }
        function    GetSha256Digest: THashBytes20;                         { V8.63 }
        function    GetSha256Hex: String;        { aka fingerprint }       { V8.63 }
        function    GetAuthInfo(const ID: String): String;                 { V8.69 }
        function    GetUrlOcsp: String;                                    { V8.69 }
        function    GetUrlIssuer: String;                                  { V8.69 }
        function    GetX509PubKeyTB: TBytes;                               { V9.1 }
        function    GetSerialNumTB: TBytes;                                { V9.5 }
        function    GetListAltNameDNS: TStringDynArray;                    { V9.5 }
        function    GetListAltNameIP: TStringDynArray;                     { V9.5 }
    public
        constructor Create(AOwner: TComponent; X509: Pointer = nil); reintroduce;
        destructor  Destroy; override;
        procedure   FreeAndNilX509;                       { V9.1 was private }
        procedure   FreeAndNilX509Inters;                 { V9.1 was private }
        procedure   FreeAndNilPrivateKey;                 { V9.1 was private }
        procedure   AssignDefaults; virtual;              { V9.1 was private }
        procedure   Assign(Source: TPersistent); override;                      { V8.71 }
        function    PostConnectionCheck(HostOrIp: String): Boolean; virtual;
        function    CheckHost(const Host: string; Flags: integer): String;       { V8.39 }
        function    CheckEmail(const Email: string; Flags: integer): Boolean;    { V8.39 }
        function    CheckIPaddr(const IPadddr: string; Flags: integer): Boolean; { V8.39 }
        procedure   ReadFromAStr(const PemStr: AnsiString; IncludePKey, IncludeInters: TCertReadOpt; const Password: String; var Errs: String); virtual;  { V8.65 }
        procedure   WriteToBio(ABio: PBIO; IncludePrivateKey: Boolean = FALSE;
                                              AddInfoText: Boolean = FALSE; const FName: String = ''); virtual;
        procedure   WriteCertToBio(ABio: PBIO; AddInfoText: Boolean = FALSE; const FName: String = ''); virtual;  { V8.40 }
        procedure   WritePkeyToBio(ABio: PBIO; const Password: String = '';
                                      PrivKeyType: TSslPrivKeyCipher = PrivKeyEncNone; const FName: String = ''); virtual;  { V8.40 }
        procedure   WriteIntersToBio(ABio: PBIO; AddInfoText: Boolean = FALSE; const FName: String = ''); virtual;  { V8.41 }
//        function    ReadStrBio(ABio: PBIO; MaxLen: Integer): AnsiString;  { V8.41 } { V869 now IcsReadStrBio }
        procedure   WriteStrBio(ABio: PBIO; Str: AnsiString; StripCR: Boolean = False);  { V8.41 }
        function    GetRawText: String;
        function    SavePKeyToText(const Password: String = ''; PrivKeyType: TSslPrivKeyCipher = PrivKeyEncNone): String;   { V8.40}
        function    SaveCertToText(AddInfoText: Boolean = FALSE): String;       { V8.40}
        function    SaveToP12Buf(const Password: String; IncludeInters: Boolean = FALSE;
                              PrivKeyType: TSslPrivKeyCipher = PrivKeyEncNone; IncludeKey: Boolean = TRUE): AnsiString;   { V8.67, V9.1 addd Key }
        procedure   LoadFromPemFileEx(const FileName: String; IncludePKey: TCertReadOpt;
                                                IncludeInters: TCertReadOpt; const Password: String; var Errs: String);    { V8.65 }
        procedure   LoadFromPemFile(const FileName: String; IncludePKey: TCertReadOpt;
                                               IncludeInters: TCertReadOpt = croNo; const Password: String = ''); overload;   { V8.40 }
        procedure   LoadFromPemFile(const FileName: String; IncludePrivateKey: Boolean = False; const Password: String = ''); overload;   { V8.40 }
        procedure   SaveToPemFile(const FileName: String; IncludePrivateKey: Boolean = FALSE;
                                    AddInfoText: Boolean = FALSE; IncludeInters: Boolean = FALSE;
                                             const Password: String = ''; PrivKeyType: TSslPrivKeyCipher = PrivKeyEncNone);   { V8.40 added inters and password }
        procedure   PrivateKeyLoadFromPemFile(const FileName: String; const Password: String = '');
        procedure   PrivateKeySaveToPemFile(const FileName: String; const Password:
                                                String = ''; PrivKeyType: TSslPrivKeyCipher = PrivKeyEncNone);   { V8.40 more params }
        procedure   LoadFromTextEx(Lines: String; IncludePKey: TCertReadOpt;
                                                IncludeInters: TCertReadOpt; const Password: String; var Errs: String);   { V8.65 }
        procedure   LoadFromText(Lines: String; IncludePKey: TCertReadOpt = croNo;
                                        IncludeInters: TCertReadOpt = croNo; const Password: String = ''); overload;    { V8.40 }
        procedure   LoadFromText(Lines: String; IncludePrivateKey: Boolean = False;     { V8.27 }
                                                  const Password: String = ''); overload;
        procedure   PrivateKeyLoadFromText(Lines: String; const Password: String = '');  { V8.27 }
        procedure   LoadFromP12File(const FileName: String; IncludePKey: TCertReadOpt = croNo;
                                            IncludeInters: TCertReadOpt = croNo; const Password: String = '');     { V8.40 }
        procedure   LoadFromP12Buffer(ABuffer: Pointer; ABufferSize: Cardinal;
                                           IncludePKey, IncludeInters: TCertReadOpt; const Password: String);  { V8.63 }
        procedure   LoadFromP7BFile(const FileName: String; IncludeInters: TCertReadOpt = croNo); { V8.40 }
        procedure   LoadFromFileEx(const FileName: String; IncludePKey: TCertReadOpt;
                                           IncludeInters: TCertReadOpt; const Password: String; var Errs: String);   { V8.65 }
        procedure   LoadFromFile(const FileName: String; IncludePKey: TCertReadOpt = croNo;
                                           IncludeInters: TCertReadOpt = croNo; const Password: String = ''); { V8.40 }
        procedure   SaveToP12File(const FileName, Password: String; IncludeInters: Boolean = FALSE;
                                         PrivKeyType: TSslPrivKeyCipher = PrivKeyEncNone; IncludeKey: Boolean = TRUE);   { V8.40, V9.1 }
        procedure   SaveToDERFile(const FileName: String);                     { V8.40 }
        procedure   SaveToP7BFile(const FileName: String; IncludeInters: Boolean = FALSE; Base64: Boolean = FALSE);  { V8.41 }
        procedure   SaveToFile(const FileName: String; IncludePrivateKey: Boolean = FALSE;
                                        AddInfoText: Boolean = False; IncludeInters: Boolean = FALSE;
                                              const Password: String = '';  PrivKeyType: TSslPrivKeyCipher = PrivKeyEncNone);   { V8.40 }
        function    CheckCertAndPKey: boolean;                             { V8.40 }
        procedure   ClearAll;                                              { V8.40 }
        function    GetPKeyRawText(PubOnly: Boolean = False): String;      { V8.40, V8.65}
        procedure   PublicKeySaveToPemFile(const FileName: String);        { V8.40 }
        function    PublicKeySaveToText: String;                           { V8.52 }
        procedure   PublicKeyLoadFromText(const Lines: String);            { V8.52 }
        function    CertInfo(Brief: Boolean=False): String;                { V8.41 added Brief }
        function    CertMainInfo: String;                                  { V8.67 }
        procedure   LoadIntersFromPemFile(const FileName: String);         { V8.41 }
        procedure   LoadIntersFromString(const Value: String);             { V8.41 }
        procedure   SaveIntersToToPemFile(const FileName: String; AddInfoText: Boolean = FALSE);         { V8.41 }
        procedure   GetIntersList(CertList: TX509List);                    { V8.41 }
        procedure   AddToInters(X509: Pointer);                            { V8.41 }
        function    ListInters: string;                                    { V8.41 }
        function    ValidateCertChain(Host: String; X509CAList: TX509List;
                                      var CertStr, ErrStr: String; ExpireDays: Integer = 30): TChainResult;   { V8.57, V8.64 }
        function    GetPX509NameByNid(XName: PX509_NAME; ANid: Integer): String;               { V8.41 }
        function    CheckExtName(Ext: PX509_EXTENSION; const ShortName: String): Boolean;      { V8.41 }
        function    ComparePkey(ACert: TX509Base): Boolean;                                    { V8.67 }
        function    GetExtDetail(Ext: PX509_EXTENSION): TExtension;                            { V8.41 }
        function    IssuedBy(ACert: TX509Base): Boolean;
        function    IssuerOf(ACert: TX509Base): Boolean;
        function    SameHash(const ACert: TX509Base): Boolean;
       { V8.39 moved next four from TX509Ex }
        function    GetNameEntryByNid(IsSubject: Boolean; ANid: Integer): String;
        function    GetExtensionByName(const S: String): TExtension;
        function    GetExtField(Ext: TExtension; const FieldName: String): String;   { V8.41 }
        function    GetExtensionValuesByName(const ShortName, FieldName: String): String;
        function    UnwrapNames(const S: String): String;
        property    IssuerOneLine       : String        read  GetIssuerOneLine;
        property    SubjectOneLine      : String        read  GetSubjectOneLine;
        property    SerialNum           : Int64         read  GetSerialNum;      { V8.40 was integer }
        property    SerialNumHex        : String        read  GetSerialNumHex;
        property    SerialNumTB         : TBytes        read  GetSerialNumTB;    { V9.5 }
        property    VerifyResult        : Integer       read  FVerifyResult
                                                        write FVerifyResult;
        property    VerifyErrMsg        : String        read  GetVerifyErrorMsg;
        property    VerifyDepth         : Integer       read  FVerifyDepth
                                                        write FVerifyDepth;
        property    CustomVerifyResult  : Integer       read  FCustomVerifyResult
                                                        write FCustomVerifyResult;
        property    FirstVerifyResult   : Integer       read  FFirstVerifyResult {05/21/2007 AG}
                                                        write FFirstVerifyResult;
        property    FirstVerifyErrMsg   : String        read  GetFirstVerifyErrorMsg; {05/21/2007 AG}
        property    X509                : Pointer       read  FX509
                                                        write SetX509;
        property    PrivateKey          : Pointer       read  FPrivateKey
                                                        write SetPrivateKey;
        property    X509PublicKey       : Pointer       read  GetX509PublicKey; { V8.52 renamed from PublicKey }
        property    X509PubKeyTB        : TBytes        read  GetX509PubKeyTB;  { V9.1 }
        property    X509Inters          : PStack        read  FX509Inters
                                                        write SetX509Inters;    { V8.41 }
        property    SslPWUtf8           : Boolean       read  FSslPWUtf8
                                                        write FSslPWUtf8;       { V8.55 }
        property    Comments            : String        read  FComments
                                                        write FComments;        { V8.67 }
        property    CertName            : String        read  FCertName
                                                        write FCertName;        { V8.67 }
        property    KeyName             : String        read  FKeyName
                                                        write FKeyName;         { V8.67 }
        property    SubjectCName        : String        read  GetSubjectCName;
        property    SubjectAltName      : TExtension    read  GetSubjectAltName;
        property    ExtensionCount      : Integer       read  GetExtensionCount;
        property    Extensions[index: Integer] : TExtension read GetExtension;
        function    Sha1Hash            : AnsiString;   deprecated
          {$IFDEF COMPILER12_UP}'Use Sha1Digest or Sha1Hex'{$ENDIF};
        property    Sha1Digest          : THashBytes20  read  GetSha1Digest
                                                        write FSha1Digest;       { V9.1 }
        property    Sha1Hex             : String        read  GetSha1Hex         { aka fingerprint }
                                                        write FSha1Hex;          { V9.1 }
        property    ValidNotBefore      : TDateTime     read  GetValidNotBefore; {AG 02/06/06}
        property    ValidNotAfter       : TDateTime     read  GetValidNotAfter;  {AG 02/06/06}
        property    HasExpired          : Boolean       read  GetHasexpired;     {AG 02/06/06}
        property    SelfSigned          : Boolean       read  GetSelfSigned;
       { V8.39 moved these from TX509Ex }
        property    SubjectOName        : String        read GetSubjectOName;
        property    SubjectOUName       : String        read GetSubjectOUName;
        property    SubjectCOName       : String        read GetSubjectCOName;
        property    SubjectSTName       : String        read GetSubjectSTName;
        property    SubjectLName        : String        read GetSubjectLName;
        property    SubjectEmailName    : String        read GetSubjectEmailName;
        property    SubjectSerialName   : String        read GetSubjectSerialName;
        property    SubAltNameDNS       : String        read GetSubAltNameDNS;
        property    ListNamesDNS        : TStringDynArray read GetListAltNameDNS;  { V9.5 }
        property    SubAltNameIP        : String        read GetSubAltNameIP;
        property    ListNameIP          : TStringDynArray read GetListAltNameIP;   { V9.5 }
        property    KeyUsage            : String        read GetKeyUsage;
        property    ExKeyUsage          : String        read GetExKeyUsage;
        property    BasicConstraints    : String        read GetBasicConstraints;
        property    AuthorityInfoAccess : String        read GetAuthorityInfoAccess;
        property    IssuerOName         : String        read GetIssuerOName;
        property    IssuerOUName        : String        read GetIssuerOUName;
        property    IssuerCName         : String        read GetIssuerCName;
        property    IssuerCOName        : String        read GetIssuerCOName;
        property    IssuerSTName        : String        read GetIssuerSTName;
        property    IssuerLName         : String        read GetIssuerLName;
        property    IssuerEmailName     : String        read GetIssuerEmailName;
        property    SignatureAlgorithm  : String        read GetSignAlgo;
        property    KeyInfo             : string        read GetKeyInfo;
        property    PrivateKeyInfo      : String        read GetPrivateKeyInfo;       { V8.40 }
        property    CertPolicies        : String        read GetCertPolicies;         { V8.40 }
        property    AuthorityKeyId      : String        read GetAuthorityKeyId;       { V8.40 }
        property    SubjectKeyId        : String        read GetSubjectKeyId;         { V8.40 }
        property    CRLDistribution     : String        read GetCRLDistribution;      { V8.40 }
        property    ExtendedValidation  : boolean       read GetExtendedValidation;   { V8.40 }
        property    IsCertLoaded        : Boolean       read GetIsCertLoaded;         { V8.41 }
        property    IsPKeyLoaded        : Boolean       read GetIsPKeyLoaded;         { V8.41 }
        property    IsInterLoaded       : Boolean       read GetIsInterLoaded;        { V8.41 }
        property    InterCount          : Integer       read GetInterCount;           { V8.41 }
        property    Sha256Digest        : THashBytes20  read GetSha256Digest          { V8.63 }
                                                        write FSha256Digest;          { V9.1 }
        property    Sha256Hex           : String        read GetSha256Hex   { aka fingerprint V8.63 }
                                                        write FSha256Hex;             { V9.1 }
        property    UrlOcsp             : String        read GetUrlOcsp;              { V8.69 }
        property    UrlIssuer           : String        read GetUrlIssuer;            { V8.69 }
    end;

    TX509Class = class of TX509Base;
    TX509Ex = TX509Base;  { V8.39 moved from OverbyteIcsSslX509Utils }

    TX509ListSort = (xsrtIssuerFirst, xsrtIssuedFirst);
    TX509List  = class(TObject)
    private
        FList               : TComponentList;
        FX509Class          : TX509Class;
        FOwner              : TComponent;
        FLastVerifyResult   : Integer;
        FX509Store          : PX509_STORE;                { V8.69 }
        FOcspRespStatus     : Integer;                    { V8.69 }
        FOcspCertStatus     : Integer;                    { V8.69 }
        FOcspCertReason     : Integer;                    { V8.69 }
        FOcspRevokeDT       : TDateTime;                  { V8.69 }
        FOcspUpdateDT       : TDateTime;                  { V8.69 }
        FOcspNextUpdDT      : TDateTime;                  { V8.69 }
    protected
        function    GetCount: Integer;
        function    GetX509Base(Index: Integer): TX509Base;
        procedure   SetX509Base(Index: Integer; Value: TX509Base);
        procedure   SetX509Class(const Value: TX509Class);
        function    GetOwnsObjects: Boolean;
        procedure   SetOwnsObjects(const Value: Boolean);
        function    GetOcspCertReasonStr: String;                          { V8.69 }
        function    GetOcspRespStatusStr: String;                          { V8.69 }
        function    GetOcspCertStatusStr: String;                          { V8.69 }
    public
        constructor Create(AOwner: TComponent; AOwnsObjects: Boolean = TRUE); reintroduce;
        destructor  Destroy; override;
        procedure   Clear;
        function    Add(X509 : PX509 = nil) : TX509Base;
        function    AddItem(AItem: TX509Base): Integer;
        function    Insert(Index: Integer; X509: PX509 = nil): TX509Base;
        procedure   InsertItem(Index: Integer; AItem: TX509Base);
        procedure   Delete(const Index: Integer);
        function    IndexOf(const X509Base : TX509Base): Integer;
        function    GetByHash(const Sha1Hash : AnsiString): TX509Base; deprecated
                    {$IFDEF COMPILER12_UP}'Use method Find'{$ENDIF};
        function    FindDigest(const ASha1Digest: THashBytes20): TX509Base;      { V8.66 was Find but ambiguous }
        function    Find(const ASha1Hex: String): TX509Base; overload;
        function    Find(const AX509: PX509): TX509Base; overload;
        function    Remove(Item: TX509Base): Integer;
        function    Extract(Item: TX509Base): TX509Base;
        function    LoadAllFromFile(const Filename: string): integer;                          { V8.39 }
        function    AllCertInfo(Brief: Boolean=False; Reverse: Boolean=False): String;         { V8.41 }
        function    LoadAllStack(CertStack: PStack): integer;                                  { V8.41 }
        function    LoadAllFromString(const Value: String): integer;                           { V8.64 }
        function    LoadAllFromPemFile(const Filename: string; var Errs: String): integer;     { V9.1 }
        function    LoadAllFromPemTB(const Certs: TBytes; var Errs: String): integer;          { V9.1 }
        function    LoadAllFromTB(const Certs: TBytes; var Errs: String): integer;             { V9.1 }
        function    LoadAllFromFileEx(const Filename: string; var Errs: String): integer;      { V8.65 }
        function    LoadAllFromStringEx(const Value: String; var Errs: String): integer;       { V8.65 }
        function    SaveAllToP12TB: TBytes;                                                    { V9.1 }
        function    SaveAllToP12File(const FileName: String): Integer;                         { V9.1 }
        function    LoadAllFromP12TB(const Certs: TBytes; var Errs: String): Integer;          { V9.1 }
        function    LoadAllFromP12File(const Filename: string; var Errs: String): Integer;     { V9.1 }
        function    FindSubject(const SubjOneLine: String): TX509Base; overload;               { V8.69 }
        function    IndexOfSubj(const SubjOneLine: String): Integer;                           { V8.69 }
        function    SaveToStack: PStack;                                                       { V8.69 }
        function    SetX509Store: Boolean;                                                     { V8.69 }
        function    BuildOCSPCertId(Cert: TX509Base): AnsiString;                              { V8.69 }
        function    OCSPReqInfo(const ReqRaw: AnsiString): String;                             { V8.69 }
        function    OCSPRespInfo(const RespRaw: AnsiString): String;                           { V8.69 }
        function    BuildOCSPReq(const IdRaw: AnsiString): AnsiString;                         { V8.69 }
        function    CheckOCSPResp(const IdRaw, RespRaw: AnsiString; X509Store: PX509_STORE): Integer; { V8.69 }
        function    ListCerts: String;                                                         { V9.1 }
        procedure   SortChain(ASortOrder: TX509ListSort);
        property    Count                       : Integer       read  GetCount;
        property    Items[index: Integer]       : TX509Base     read  GetX509Base
                                                                write SetX509Base; default;
        property    X509Class                   : TX509Class    read  FX509Class
                                                                write SetX509Class;
        property    LastVerifyResult            : Integer       read  FLastVerifyResult
                                                                write FLastVerifyResult;      { V9.1 }
        property    OwnsObjects                 : Boolean       read  GetOwnsObjects
                                                                write SetOwnsObjects;
        property    X509Store                   : PX509_STORE   read  FX509Store;             { V8.69 }
        property    OcspRespStatus      : Integer               read FOcspRespStatus;         { V8.69 }
        property    OcspRespStatusStr   : String                read GetOcspRespStatusStr;    { V8.69 }
        property    OcspCertStatus      : Integer               read FOcspCertStatus;         { V8.69 }
        property    OcspCertStatusStr   : String                read GetOcspCertStatusStr;    { V8.69 }
        property    OcspCertReason      : Integer               read FOcspCertReason;         { V8.69 }
        property    OcspCertReasonStr   : String                read GetOcspCertReasonStr;    { V8.69 }
        property    OcspRevokeDT        : TDateTime             read FOcspRevokeDT;           { V8.69 }
        property    OcspUpdateDT        : TDateTime             read FOcspUpdateDT;           { V8.69 }
        property    OcspNextUpdDT       : TDateTime             read FOcspNextUpdDT;          { V8.69 }
    end;

  { V9.1 TSslRootCAStore is made public as IcsSslRootCAStore for common use by SslContexts and other
          SSL components that need to check certificates against public roots. }
    TSslRootCAStore  = class(TX509List)
    private
        FInitFlag: Boolean;
        FCAStoreSource: String;
    public
        DefRootFile: string;
        ExtraRootFile: String;
        constructor Create(AOwner: TComponent; AOwnsObjects: Boolean = TRUE);
        destructor  Destroy; override;
        procedure Initialise;
        procedure Reset;
        property  InitFlag: Boolean          read  FInitFlag;
        property  CAStoreSource: String      read FCAStoreSource;
    end;



    // single SSL Version selection  - old
    TSslVersionMethod = (sslV2,                 { V8.27 gone 1.1.0 }
                         sslV2_CLIENT,          { V8.27 gone 1.1.0 }
                         sslV2_SERVER,          { V8.27 gone 1.1.0 }
                         sslV3,
                         sslV3_CLIENT,
                         sslV3_SERVER,
                         sslTLS_V1,
                         sslTLS_V1_CLIENT,
                         sslTLS_V1_SERVER,
                         sslV23,
                         sslV23_CLIENT,
                         sslV23_SERVER,
                         sslTLS_V1_1,           { V8.15 added 1.1 and 1.2  }
                         sslTLS_V1_1_CLIENT,
                         sslTLS_V1_1_SERVER,
                         sslTLS_V1_2,
                         sslTLS_V1_2_CLIENT,
                         sslTLS_V1_2_SERVER,
                         sslBestVer,           { V8.15 same as sslV23 but easier to understand, now default }
                         sslBestVer_CLIENT,
                         sslBestVer_SERVER);

    // range SSL Version selection  - new V8.27 used to set minimum and maxium supported versions
    TSslVerMethod = (sslVerSSL3,
                     sslVerTLS1,
                     sslVerTLS1_1,
                     sslVerTLS1_2,
                     sslVerTLS1_3,  { not yet supported, still draft }
                     sslVerMax);

    TSslVerifyPeerMode = (SslVerifyMode_NONE,
                          SslVerifyMode_PEER,
                          SslVerifyMode_FAIL_IF_NO_PEER_CERT,
                          SslVerifyMode_CLIENT_ONCE);
    TSslVerifyPeerModes = set of TSslVerifyPeerMode;

    TSslVerifyFlag  = (
                      sslX509_V_FLAG_CB_ISSUER_CHECK,
                      sslX509_V_FLAG_USE_CHECK_TIME,
                      sslX509_V_FLAG_CRL_CHECK,
                      sslX509_V_FLAG_CRL_CHECK_ALL,
                      sslX509_V_FLAG_IGNORE_CRITICAL,
                      sslX509_V_FLAG_X509_STRICT,
                      sslX509_V_FLAG_ALLOW_PROXY_CERTS,
                      sslX509_V_FLAG_POLICY_CHECK,    { V8.39 lots more flags }
                      sslX509_V_FLAG_EXPLICIT_POLICY,
                      sslX509_V_FLAG_INHIBIT_ANY,
                      sslX509_V_FLAG_INHIBIT_MAP ,
                      sslX509_V_FLAG_NOTIFY_POLICY,
                      sslX509_V_FLAG_EXTENDED_CRL_SUPPORT,
                      sslX509_V_FLAG_USE_DELTAS,
                      sslX509_V_FLAG_CHECK_SS_SIGNATURE,
                      sslX509_V_FLAG_TRUSTED_FIRST,
                      sslX509_V_FLAG_SUITEB_128_LOS_ONLY,
                      sslX509_V_FLAG_SUITEB_192_LOS,
                      sslX509_V_FLAG_SUITEB_128_LOS,
                      sslX509_V_FLAG_PARTIAL_CHAIN,
                      sslX509_V_FLAG_NO_ALT_CHAINS,
                      sslX509_V_FLAG_NO_CHECK_TIME);
    TSslVerifyFlags = set of TSslVerifyFlag;

   { V8.39 }
    TSslCheckHostFlag = (
                     sslX509_NO_HOST_CHECK,
                     sslX509_CHECK_FLAG_ALWAYS_CHECK_SUBJECT,
                     sslX509_CHECK_FLAG_NO_WILDCARDS,
                     sslX509_CHECK_FLAG_NO_PARTIAL_WILDCARDS,
                     sslX509_CHECK_FLAG_MULTI_LABEL_WILDCARDS,
                     sslX509_CHECK_FLAG_SINGLE_LABEL_SUBDOMAINS,
                     sslX509_CHECK_FLAG_NEVER_CHECK_SUBJECT);
    TSslCheckHostFlags = set of TSslCheckHostFlag;

type
  { V8.51 now only used for 1.0.2 and 1.1.0, many unused for 1.1.0, ignored for 1.1.1 and later }
  { V8.66 ignored, use TSslOption2 instead, but left to avoid problems opening saved forms }
    TSslOption  = (sslOpt_CIPHER_SERVER_PREFERENCE,
                   sslOpt_MICROSOFT_SESS_ID_BUG,        { V8.27 gone 1.1.0 }
                   sslOpt_NETSCAPE_CHALLENGE_BUG,       { V8.27 gone 1.1.0 }
                   sslOpt_NETSCAPE_REUSE_CIPHER_CHANGE_BUG,   { V8.27 gone 1.1.0 }
                   sslOpt_SSLREF2_REUSE_CERT_TYPE_BUG,  { V8.27 gone 1.1.0 }
                   sslOpt_MICROSOFT_BIG_SSLV3_BUFFER,   { V8.27 gone 1.1.0 }
                   sslOpt_MSIE_SSLV2_RSA_PADDING,       { V8.27 gone 1.1.0 }
                   sslOpt_SSLEAY_080_CLIENT_DH_BUG,     { V8.27 gone 1.1.0 }
                   sslOpt_TLS_D5_BUG,                   { V8.27 gone 1.1.0 }
                   sslOpt_TLS_BLOCK_PADDING_BUG,        { V8.27 gone 1.1.0 }
                   sslOpt_TLS_ROLLBACK_BUG,
                   sslOpt_DONT_INSERT_EMPTY_FRAGMENTS,
                   sslOpt_SINGLE_DH_USE,                { V8.27 gone 1.1.0 }
                   sslOpt_EPHEMERAL_RSA,                { V8.27 gone 1.1.0 }
                   sslOpt_NO_SSLv2,                     { V8.27 gone 1.1.0 }
                   sslOpt_NO_SSLv3,
                   sslOpt_NO_TLSv1,
                   sslOpt_PKCS1_CHECK_1,                { V8.27 gone 1.1.0 }
                   sslOpt_PKCS1_CHECK_2,                { V8.27 gone 1.1.0 }
                   sslOpt_NETSCAPE_CA_DN_BUG,           { V8.27 gone 1.1.0 }
                   //sslOP_NO_TICKET,             { no session tickets, this is forced later }
                   sslOpt_NO_SESSION_RESUMPTION_ON_RENEGOTIATION, // 12/09/05
                   sslOpt_NETSCAPE_DEMO_CIPHER_CHANGE_BUG,  { V8.27 gone 1.1.0 }
                   sslOpt_ALLOW_UNSAFE_LEGACY_RENEGOTIATION, // Since OSSL 0.9.8n
                   sslOpt_NO_COMPRESSION,         { V8.15 }
                   sslOpt_TLSEXT_PADDING,         { V8.15 }
                   sslOpt_SAFARI_ECDHE_ECDSA_BUG, { V8.15 }
                   sslOpt_CISCO_ANYCONNECT,       { V8.15 }
                   sslOpt_NO_TLSv1_1,             { V8.15 }
                   sslOpt_NO_TLSv1_2,             { V8.15 }
                   SslOpt_SINGLE_ECDH_USE);       { V8.27 gone 1.1.0 }
   TSslOptions = set of TSslOption;

 { V8.51 only options supported by 1.1.0 and later }
   TSslOption2  = (sslOpt2_CIPHER_SERVER_PREFERENCE,      { server only }
                   sslOpt2_NO_RENEGOTIATION,       { V8.51 new 1.1.1 }
                   sslOpt2_NO_SESSION_RESUMPTION_ON_RENEGOTIATION,  { server only }
                   sslOpt2_NO_COMPRESSION,         { V8.15 }
                   SslOpt2_NO_ENCRYPT_THEN_MAC,    { V8.51 new 1.1.1 }
                   sslOpt2_NO_TICKET,              { V8.51 no longer forced later }
                   sslOpt2_NO_SSLv3,               { No_xx are deprecated for 1.1.0, use Min/MaxProto }
                   sslOpt2_NO_TLSv1,
                   sslOpt2_NO_TLSv1_1,             { V8.15 }
                   sslOpt2_NO_TLSv1_2,             { V8.15 }
                   sslOpt2_NO_TLSv1_3,             { V8.51 new 1.1.1 }
                   sslOpt2_TLS_ROLLBACK_BUG,
                   sslOpt2_DONT_INSERT_EMPTY_FRAGMENTS,
                   sslOpt2_ALLOW_UNSAFE_LEGACY_RENEGOTIATION, // Since OSSL 0.9.8n
                   sslOpt2_TLSEXT_PADDING,         { V8.15 }
                   sslOpt2_SAFARI_ECDHE_ECDSA_BUG, { V8.15 }
                   sslOpt2_CISCO_ANYCONNECT,       { V8.15 }
                   SslOpt2_LEGACY_SERVER_CONNECT,  { V8.51 new 1.1.0 }
                   SslOpt2_ALLOW_NO_DHE_KEX);      { V8.51 new 1.1.1 }
   TSslOptions2 = set of TSslOption2;

const
 (*   SslIntOptions: array[TSslOption] of Integer =                   { V7.30 }
           (SSL_OP_CIPHER_SERVER_PREFERENCE,
            SSL_OP_MICROSOFT_SESS_ID_BUG,
            SSL_OP_NETSCAPE_CHALLENGE_BUG,
            SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG,
            SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG,
            SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER,
            SSL_OP_MSIE_SSLV2_RSA_PADDING,
            SSL_OP_SSLEAY_080_CLIENT_DH_BUG,
            SSL_OP_TLS_D5_BUG,
            SSL_OP_TLS_BLOCK_PADDING_BUG,
            SSL_OP_TLS_ROLLBACK_BUG,
            SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS,
            SSL_OP_SINGLE_DH_USE,
            SSL_OP_EPHEMERAL_RSA,
            SSL_OP_NO_SSLv2,
            SSL_OP_NO_SSLv3,
            SSL_OP_NO_TLSv1,
            SSL_OP_PKCS1_CHECK_1,
            SSL_OP_PKCS1_CHECK_2,
            SSL_OP_NETSCAPE_CA_DN_BUG,
            SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION,
            SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG,
            SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION,  // Since OSSL 0.9.8n
            SSL_OP_NO_COMPRESSION,         { V8.15 }
            SSL_OP_TLSEXT_PADDING,         { V8.15 }
            SSL_OP_SAFARI_ECDHE_ECDSA_BUG, { V8.15 }
            SSL_OP_CISCO_ANYCONNECT,       { V8.15 }
            SSL_OP_NO_TLSv1_1,             { V8.15 }
            SSL_OP_NO_TLSv1_2,             { V8.15 }
            SSL_OP_SINGLE_ECDH_USE);        { V8.16 }

   { V8.27 OSSL 1.1.0 lost many old options }
    SslIntOptions110: array[TSslOption] of Integer =
           (SSL_OP_CIPHER_SERVER_PREFERENCE,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            SSL_OP_TLS_ROLLBACK_BUG,
            SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS,
            0,
            0,
            0,
            SSL_OP_NO_SSLv3,
            SSL_OP_NO_TLSv1,
            0,
            0,
            0,
            SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION,
            0,
            SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION,
            SSL_OP_NO_COMPRESSION,
            SSL_OP_TLSEXT_PADDING,
            SSL_OP_SAFARI_ECDHE_ECDSA_BUG,
            SSL_OP_CISCO_ANYCONNECT,
            SSL_OP_NO_TLSv1_1,
            SSL_OP_NO_TLSv1_2,
            0);                                     *)

   { V8.51 OSSL 1.1.0 and 1.1.1, latter has few new options }
    SslIntOptions2: array[TSslOption2] of Integer =
           (SSL_OP_CIPHER_SERVER_PREFERENCE,
            SSL_OP_NO_RENEGOTIATION,       { V8.51 }
            SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION,
            SSL_OP_NO_COMPRESSION,
            SSL_OP_NO_ENCRYPT_THEN_MAC,    { V8.51 }
            SSL_OP_NO_TICKET,
            SSL_OP_NO_SSLv3,
            SSL_OP_NO_TLSv1,
            SSL_OP_NO_TLSv1_1,
            SSL_OP_NO_TLSv1_2,
            SSL_OP_NO_TLSv1_3,             { V8.51 }
            SSL_OP_TLS_ROLLBACK_BUG,
            SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS,
            SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION,
            SSL_OP_TLSEXT_PADDING,
            SSL_OP_SAFARI_ECDHE_ECDSA_BUG,
            SSL_OP_CISCO_ANYCONNECT,
            SSL_OP_LEGACY_SERVER_CONNECT,  { V8.51 }
            SSL_OP_ALLOW_NO_DHE_KEX);      { V8.51 }

type
    TSslSessCacheMode = (sslSESS_CACHE_CLIENT,
                         sslSESS_CACHE_SERVER,
                         sslSESS_CACHE_NO_AUTO_CLEAR,
                         sslSESS_CACHE_NO_INTERNAL_LOOKUP,
                         sslSESS_CACHE_NO_INTERNAL_STORE);
    TSslSessCacheModes = set of TSslSessCacheMode;

    TSslSessionIdContext = String;//[SSL_MAX_SSL_SESSION_ID_LENGTH];

  // V8.15 ECDH (Ellliptic Curve Diiffie-Hellman) method selection, auto is OpenSSL 1.0.2 and later
    TSslECDHMethod = (sslECDHNone,
                      sslECDHAuto,
                      sslECDH_P256,
                      sslECDH_P384,
                      sslECDH_P521);

const
  SslIntSessCacheModes: array[TSslSessCacheMode] of Integer =     { V7.30 }
            (SSL_SESS_CACHE_CLIENT,
             SSL_SESS_CACHE_SERVER,
             SSL_SESS_CACHE_NO_AUTO_CLEAR,
             SSL_SESS_CACHE_NO_INTERNAL_LOOKUP,
             SSL_SESS_CACHE_NO_INTERNAL_STORE);


  SslIntVerifyFlags: array[TSslVerifyFlag] of Integer =
           (X509_V_FLAG_CB_ISSUER_CHECK,
            X509_V_FLAG_USE_CHECK_TIME,
            X509_V_FLAG_CRL_CHECK,
            X509_V_FLAG_CRL_CHECK_ALL,
            X509_V_FLAG_IGNORE_CRITICAL,
            X509_V_FLAG_X509_STRICT,
            X509_V_FLAG_ALLOW_PROXY_CERTS,
            X509_V_FLAG_POLICY_CHECK,          { V8.39 lots more flags }
            X509_V_FLAG_EXPLICIT_POLICY,
            X509_V_FLAG_INHIBIT_ANY,
            X509_V_FLAG_INHIBIT_MAP,
            X509_V_FLAG_NOTIFY_POLICY,
            X509_V_FLAG_EXTENDED_CRL_SUPPORT,
            X509_V_FLAG_USE_DELTAS,
            X509_V_FLAG_CHECK_SS_SIGNATURE,
            X509_V_FLAG_TRUSTED_FIRST,
            X509_V_FLAG_SUITEB_128_LOS_ONLY,
            X509_V_FLAG_SUITEB_192_LOS,
            X509_V_FLAG_SUITEB_128_LOS,
            X509_V_FLAG_PARTIAL_CHAIN,
            X509_V_FLAG_NO_ALT_CHAINS,
            X509_V_FLAG_NO_CHECK_TIME);

  SslIntCheckHostFlags: array[TSslCheckHostFlag] of Integer =     {V8.39 }
           (0,
            X509_CHECK_FLAG_ALWAYS_CHECK_SUBJECT,
            X509_CHECK_FLAG_NO_WILDCARDS,
            X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS,
            X509_CHECK_FLAG_MULTI_LABEL_WILDCARDS,
            X509_CHECK_FLAG_SINGLE_LABEL_SUBDOMAINS,
            X509_CHECK_FLAG_NEVER_CHECK_SUBJECT);

 SslECDHMethods: array [TSslECDHMethod] of integer =   { V8.15 }
           (0,
            0,
            NID_X9_62_prime256v1,     { V8.67 aka secp256r1 }
            NID_secp384r1,
            NID_secp521r1);

 SslVerMethods: array [TSslVerMethod] of Cardinal =   { V8.27 }
           (SSL3_VERSION,
            TLS1_VERSION,
            TLS1_1_VERSION,
            TLS1_2_VERSION,
            TLS1_3_VERSION,                   { V8.51 }
            TLS_MAX_VERSION);


(* V9.3 moved to OverbyteIcsTypes
  SslSrvSecurityNames: array [TSslSrvSecurity ] of PChar = (   { V8.55, V8.60 added TLS version and 1.2/1.3 only }
                     'None',                                      { 0 - all protocols and ciphers, any key lengths }
                     'SSLv3 Only',                                { 1 - SSL3 only, all ciphers, any key lengths, MD5 }
                     'Backward Ciphers, TLS1 or Later',           { 2 - TLS1 or later, backward ciphers, RSA/DH keys=>1024, ECC=>160, no MD5, SHA1 }
                     'Intermediate Ciphers, TLS1.1 or Later',     { 3 - TLS1.1 or later, intermediate ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     'Intermediate Ciphers FS, TLS1.1 or Later',  { 4 - TLS1.1 or later, intermediate FS ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     'High 112 bit Ciphers, TLS1.2 or Later',     { 5 - TLS1.2 or later, high ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     'High 128 bit Ciphers, TLS1.2 or Later',     { 6 - TLS1.2 or later, high ciphers, RSA/DH keys=>3072, ECC=>256, FS forced }
                     'High 192 bit Ciphers, TLS1.2 or Later',     { 7 - TLS1.2 or later, high ciphers, RSA/DH keys=>7680, ECC=>384, FS forced }
                     'TLSv1.2 or Earlier',                        { 8 - TLSv1.2 or earlier, intermediate FS ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     'TLSv1.3 Only');                             { 9 - TLSv1.3 only, intermediate FS ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }

 SslCliSecurityNames: array [TSslCliSecurity] of PChar = (   { V8.55 }
                     'Ignore',         { 0 - ignore, use old settings }
                     'None',           { 1 - all protocols and ciphers, any key lengths }
                     'SSLv3 Only',     { 2 - SSLv3 only, all ciphers, any key lengths, MD5 }
                     'TLSv1 Only',     { 3 - TLSv1 only, all ciphers, RSA/DH keys=>2048 }
                     'TLSv1.1 Only',   { 4 - TLSv1.1 only, all ciphers, RSA/DH keys=>2048 }
                     'TLSv1.2 Only',   { 5 - TLSv1.2 only, all ciphers, RSA/DH keys=>2048 }
                     'TLSv1.3 Only',   { 6 - TLSv1.3 only, all ciphers, RSA/DH keys=>2048 }
                     'TLSv1 or Better',       { 7 - TLSv1 or later, all ciphers, RSA/DH keys=>1024 }
                     'TLSv1.1 or Better',     { 8 - TLSv1.1 or later, all ciphers, RSA/DH keys=>1024 }
                     'TLSv1.2 or Better',     { 9 - TLSv1.2 or later, all ciphers, RSA/DH keys=>2048 }
                     'Backward Ciphers',      { 10 - TLSv1 or later, backward ciphers, RSA/DH keys=>1024, ECC=>160, no MD5, SHA1 }
                     'Intermediate Ciphers',  { 11 - TLSv1.1 or later, intermediate ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     'High Ciphers, 2048 keys',  { 12 - TLSv1.2 or later, high ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     'High Ciphers, 3072 keys',  { 13 - TLSv1.2 or later, high ciphers, RSA/DH keys=>3072, ECC=>256, FS forced }
                     'High Ciphers, 7680 keys'); { 14 - TLSv1.2 or later, high ciphers, RSA/DH keys=>7680, ECC=>384, FS forced }
*)

type
    TIcsIntArray = Array of Integer;
    TIcsWordArray = Array of Word;
    TTlsExtError = (teeOk, teeAlertWarning, teeAlertFatal, teeNoAck);
    TInfoExtractMode = (emCert, {emKey,} emCRL);
    ESslTX509Exception = class(Exception);     { V9.1 }


 { V8.64 data received by a server when a client starts a session, used by the
   server to decide what ciphers and protocol to initiate, see
   https://tls.ulfheim.net/  and https://tls13.ulfheim.net/ }
    TClientHelloData = record
        ServerName: String;
        PunyServerName: String;
        SslTlsExtErr: TTlsExtError;
        Sslv2: Boolean;  // too old to support
        LegacyVersion: Cardinal;
        SuppVersions: TIcsWordArray;
        Random: TBytes;
        SessionId: TBytes;
        CipherSuites: TBytes;
        ExtnList: TIcsIntArray;
        ExtnTotal: Integer;
        AlpnRaw: TBytes;
        AlpnList: String;
        EllipCurves: TIcsWordArray;
        SigAlgos: TIcsWordArray;
        ECPoints: TIcsWordArray;
        Renegotiate: TBytes;
        StatusRequest: TBytes;
        KeyShare: TBytes;
        PSKExchMode: TBytes;
        PSKData: TBytes;
        CliCertType: TBytes;    { V9.5 }
        SrvCertType: TBytes;    { V9.5 }
    end;

(*
type

{$IFNDEF OPENSSL_NO_ENGINE}
   ESslEngineError = class(Exception);
    TSslEngineState = (esClosed, esOpen, esInit);
    TSslEngineCtxCapabilities = set of (eccLoadPrivKey, eccLoadPubKey{, eccLoadClientCert});
    TSslEngine = class(TSslBaseComponent)
    private
        FEngine           : PEngine;
        FNameID           : String;
        FState            : TSslEngineState;
        FCtxCapabilities  : TSslEngineCtxCapabilities;
        FLastErrorMsg     : String;
        FKeyID            : String;
        procedure SetNameID(const Value: String);
    public
        destructor Destroy; override;
        function  Open: Boolean;
        function  Control(const Cmd, Arg: String): Boolean;
        procedure Close;
        function  Init: Boolean;
        property  E : PEngine read FEngine;
        property  State : TSslEngineState read FState;
        property  LastErrorMsg : String read FLastErrorMsg write FLastErrorMsg;
    published
        property  KeyID : String read FKeyID write FKeyID;
        property  NameID : String read FNameID write SetNameID;
        property  CtxCapabilities : TSslEngineCtxCapabilities read FCtxCapabilities write FCtxCapabilities;
    end;
{$ENDIF}
*)

    ESslContextException = class(Exception);
    TSslContextRemoveSession = procedure(Sender: TObject; SslSession : Pointer) of object;

    // TSslCertKeyFormat = (ckfPem {$IFNDEF OPENSSL_NO_ENGINE}, ckfEngine {$ENDIF}); {ckfPkcs12,}
    TSslContext = class(TSslBaseComponent)
    protected
        FSslCtx                     : PSSL_CTX;
        FSslVersionMethod           : TSslVersionMethod;  { V8.27 ignored }
        FSslMinVersion              : TSslVerMethod; { V8.27 }
        FSslMaxVersion              : TSslVerMethod; { V8.27 }
        FSslCertFile                : String;
        FSslCertLines               : TStrings;  { V8.27 }
        FSslCertX509                : TX509Base; { V8.39 }
        FSslPassPhrase              : String;
        FSslPrivKeyFile             : String;
        FSslPrivKeyLines            : TStrings;  { V8.27 }
        FSslCAFile                  : String;
        FSslCALines                 : TStrings;  { V8.27 }
        FSslCAPath                  : String;
        FSslCRLFile                 : String;
        FSslCRLPath                 : String;
        FSslDHParamFile             : String;    { V8.15 }
        FSslDHParamLines            : TStrings;  { V8.27 }
        FSslVerifyPeer              : Boolean;
        FSslVerifyDepth             : Integer;
        FSslVerifyFlagsValue        : Integer;  { V9.1 was FSslVerifyFlags, addd Value }
        FVerifyCallbackPtr          : Pointer;  { V9.1 pointer to verify callback in wsocket }
//        FSslOptionsValue            : Integer;  {V8.51 now storing TSslOptions instead }
        FSslOptions                 : TSslOptions;     { V8.51 }
        FSslOptions2                : TSslOptions2;    { V8.51 }
        FSslCipherList              : String;
        FSslECDHMethod              : TSslECDHMethod;  { V8.15, ignored for OpenSSL 1.1.0 and later }
        FSslCheckHostFlagsValue     : Integer;         { V9.1 added Value }
        FSslSecLevel                : TSslSecLevel;    { V8.40 }
        FSslCryptoGroups            : String;          { V8.51 1.1.1 and later, 'P-256:X25519:P-384:P-521' }
        FSslCliSecurity             : TSslCliSecurity; { V8.54 }
        FSslAlpnProtoList           : TStrings;        { V8.56 }
        FSslOcspStatus              : Boolean;         { V8.69 should we get OCSP staple status in handshake }
        FUseSharedCAStore           : Boolean;         { V9.1 ignores CAFile, CALines, CAPath, uses IcsSslRootCAStore }
        FSslCipherList13            : String;          { V9.3 TLSv1.3 cipher lists }
        FCliCertTypes               : TTlsCertTypes;   { V9.5 TLS certificate types supported for clients }
        FSrvCertTypes               : TTlsCertTypes;   { V9.5 TLS certificate types supported for servers }
        FSslSessCacheModeValue      : Integer;
        FSslSessionCacheSize        : Integer;
        FSslSessionTimeout          : Cardinal;
        FSslDefaultSessionIDContext : TSslSessionIdContext;
        FOnRemoveSession            : TSslContextRemoveSession;
        FSslVerifyPeerModes         : TSslVerifyPeerModes;
        FSslVerifyPeerModesValue    : Integer;
        FOnBeforeInit               : TNotifyEvent;
        //FSslKeyFormat             : TSslCertKeyFormat;
        FAutoEnableBuiltinEngines   : Boolean;     { V9.1 no longer supported but published for backward compatibility }
    {$IFNDEF OPENSSL_NO_ENGINE}
 //       FCtxEngine                  : TSslEngine;   V9.1 not supported for years
    {$ENDIF}
{$IFNDEF NO_SSL_MT}
        FLock                       : TIcsCriticalSection;
        procedure Lock; {$IFDEF USE_INLINE} inline; {$ENDIF}
        procedure Unlock; {$IFDEF USE_INLINE} inline; {$ENDIF}
{$ENDIF}
        function  InitializeCtx : PSSL_CTX;
        procedure SetSslCertFile(const Value : String);
        procedure SetSslCertLines(Value: TStrings);    { V8.27 }
        procedure SetSslPassPhrase(const Value : String);
        procedure SetSslPrivKeyFile(const Value : String);
        procedure SetSslPrivKeyLines(Value: TStrings); { V8.27 }
        procedure SetSslCAFile(const Value : String);
        procedure SetSslCALines(Value: TStrings);      { V8.27 }
        procedure SetSslCAPath(const Value : String);
        procedure SetSslDHParamFile(const Value : String);   { V8.15 }
        procedure SetSslDHParamLines(Value: TStrings); { V8.27 }
        procedure SetSslCRLFile(const Value : String);
        procedure SetSslCRLPath(const Value : String);
        procedure SetSslSessionCacheSize(Value : Integer);
        procedure SetSslSessCacheModes(Value : TSslSessCacheModes);
        function  GetSslSessCacheModes : TSslSessCacheModes;
        procedure SetSslCipherList(const Value : String);
        procedure SetSslVerifyPeerModes(const Value : TSslVerifyPeerModes);
        procedure SetSslVerifyPeer(const Value: Boolean);
        procedure SetSslDefaultSessionIDContext(Value: TSslSessionIdContext);
        procedure SetSslSessionTimeout(Value : Cardinal);
        procedure SetSslVersionMethod(Value : TSslVersionMethod);
        procedure SetSslMinVersion(Value : TSslVerMethod);   { V8.27 }
        procedure SetSslMaxVersion(Value : TSslVerMethod);   { V8.27 }
        procedure SetSslECDHMethod(Value : TSslECDHMethod);
        function  GetIsCtxInitialized : Boolean;
        function  GetSslVerifyFlags: TSslVerifyFlags;
        procedure SetSslVerifyFlags(const Value: TSslVerifyFlags);
        function  GetSslCheckHostFlags: TSslCheckHostFlags;                 { V8.39 }
        procedure SetSslCheckHostFlags(const Value: TSslCheckHostFlags);    { V8.39 }
        procedure SetSslCliSecurity(Value: TSslCliSecurity);                { V8.54 }
        procedure SetSslCliSec;                                             { V8.54 }
        procedure UpdateAlpnProtocols;                                      { V8.56 }
        procedure SetSslCipherList13(const Value: String);                  { V9.3 }

    {$IFNDEF OPENSSL_NO_ENGINE}
   //     procedure Notification(AComponent: TComponent; Operation: TOperation); override;
   //     procedure SetCtxEngine(const Value: TSslEngine);     V9.1 not supported for years
    {$ENDIF}
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   InitContext;
        procedure   DeInitContext;
        function    TrustCert(Cert : TX509Base): Boolean;
        procedure   LoadCrlFromFile(const Filename: String);
        procedure   LoadCrlFromPath(const Path: String);
        procedure   LoadVerifyLocations(const CAFile, CAPath: String);
        procedure   LoadCertFromChainFile(const FileName : String);
        procedure   LoadPKeyFromFile(const FileName : String);
        procedure   LoadDHParamsFromFile(const FileName: String); { V8.15 }
        procedure   LoadCertFromString(const Value: String);      { V8.27 }
        procedure   LoadPKeyFromString(const Value: String);      { V8.27 }
        procedure   LoadDHParamsFromString(const Value: String);  { V8.27 }
        procedure   LoadCAFromString(const Value: String);        { V8.27 }
    {$IFNDEF OPENSSL_NO_ENGINE}
 //       procedure   LoadPKeyFromEngine(CtxEngine: TSslEngine);
        //function    SetupEngine(Engine: String; Commands: TStrings): PENGINE;
    {$ENDIF}
        procedure   AddClientCAFromFile(const FileName: String);
        procedure   FreeNotification(AComponent: TComponent);
        procedure   RemoveFreeNotification(AComponent: TComponent);
        procedure   SetClientCAListFromFile(const FileName: String);
        property    IsCtxInitialized : Boolean read GetIsCtxInitialized;
        function    SslGetAllCiphers: String;     { V8.27 }
        function    SslGetAllCerts(CertList: TX509List): integer;       { V8.39 }
        function    SslBuildCertChain(Flags: Integer): integer;         { V8.40 }
        procedure   LoadCAFromStack(CertStack: PStack);                 { V8.41 }
        function    CheckPrivateKey: boolean;                           { V8.40 }
        function    SslGetCerts(Cert: TX509Base): integer;              { V8.41 }
        procedure   SslSetCertX509;                                     { V8.41 }
        procedure   SetProtoSec;                                        { V8.54 }
        procedure   SetSslAlpnProtocols(ProtoList: TStrings);           { V8.56 }
        procedure   LoadCertFromAStr(const Value: AnsiString; var Errs: String);   { V8.65 }
        function    GetX509Store: PX509_STORE;                          { V8.69 }
        function    GetCAStoreTotal: Integer;                           { V9.1 }
        procedure   LoadCAFromTB(const Certs: TBytes);                  { V9.1 }
        property    SslCertX509     : TX509Base         read  FSslCertX509
                                                        write FSslCertX509;   { V8.41 }
        property    SslCtxPtr       : PSSL_CTX          read  FSslCtx;        { V8.62 }
        property    VerifyCallbackPtr : Pointer         read  FVerifyCallbackPtr
                                                        write FVerifyCallbackPtr;      { V9.1 pointer to verify callback in wsocket }
    published
        property  SslCertFile       : String            read  FSslCertFile
                                                        write SetSslCertFile;
        property  SslCertLines      : TStrings          read  FSslCertLines     { V8.27 }
                                                        write SetSslCertLines;
        property  SslPassPhrase     : String            read  FSslPassPhrase
                                                        write SetSslPassPhrase;
        property  SslPrivKeyFile    : String            read  FSslPrivKeyFile
                                                        write SetSslPrivKeyFile;
        property  SslPrivKeyLines   : TStrings          read  FSslPrivKeyLines  { V8.27 }
                                                        write SetSslPrivKeyLines;
        property  SslCAFile         : String            read  FSslCAFile
                                                        write SetSslCAFile;
        property  SslCALines        : TStrings          read  FSslCALines       { V8.27 }
                                                        write SetSslCALines;
        property  SslCAPath         : String            read  FSslCAPath
                                                        write SetSslCAPath;
        property  SslCRLFile        : String            read  FSslCRLFile
                                                        write SetSslCRLFile;
        property  SslCRLPath        : String            read  FSslCRLPath
                                                        write SetSslCRLPath;
        property  SslDHParamFile    : String            read  FSslDHParamFile
                                                        write SetSslDHParamFile; { V8.15 }
        property  SslDHParamLines   : TStrings          read  FSslDHParamLines   { V8.27 }
                                                        write SetSslDHParamLines;
        property  SslVerifyPeer     : Boolean           read  FSslVerifyPeer
                                                        write SetSslVerifyPeer;
        property  SslVerifyDepth    : Integer           read  FSslVerifyDepth
                                                        write FSslVerifyDepth;
        property  SslVerifyFlags    : TSslVerifyFlags   read  GetSslVerifyFlags
                                                        write SetSslVerifyFlags;
        property  SslVerifyFlagsValue : Integer         read  FSslVerifyFlagsValue
                                                        write FSslVerifyFlagsValue;    { V9.1 }
        property  SslCheckHostFlags : TSslCheckHostFlags  read  GetSslCheckHostFlags   { V8.39 }
                                                        write SetSslCheckHostFlags;
        property  SslCheckHostFlagsValue : Integer      read  FSslCheckHostFlagsValue
                                                        write FSslCheckHostFlagsValue;     { V9.1 }
        property  SslSecLevel       : TSslSecLevel      read FSslSecLevel             { V8.40 }
                                                        write FSslSecLevel;
        property  SslOptions        : TSslOptions       read  FSslOptions     { V8.66 sets only 1.1.1 options }
                                                        write FSslOptions;    { V8.51 removed setters }
        property  SslOptions2       : TSslOptions2      read  FSslOptions2
                                                        write FSslOptions2;   { V8.51 }
        property  SslVerifyPeerModes : TSslVerifyPeerModes
                                                        read  FSslVerifyPeerModes
                                                        write SetSslVerifyPeerModes;
        property  SslVerifyPeerModesValue : Integer     read  FSslVerifyPeerModesValue
                                                        write FSslVerifyPeerModesValue;     { V9.1 }
        property  SslSessionCacheModes : TSslSessCacheModes
                                                    read  GetSslSessCacheModes
                                                    write SetSslSessCacheModes;
        property  SslCipherList     : String        read  FSslCipherList
                                                    write SetSslCipherList;
        property  SslCipherList13   : String        read  FSslCipherList13
                                                    write FSslCipherList13;   { V9.3 TLSv1.3 cipher lists }
        property  SslVersionMethod  : TSslVersionMethod
                                                    read  FSslVersionMethod
                                                    write SetSslVersionMethod;
        property  SslMinVersion  : TSslVerMethod    read  FSslMinVersion        { V8.27 }
                                                    write SetSslMinVersion;
        property  SslMaxVersion  : TSslVerMethod    read  FSslMaxVersion        { V8.27 }
                                                    write SetSslMaxVersion;
        property  SslECDHMethod  : TSslECDHMethod  { V8.15 }
                                                    read  FSslECDHMethod
                                                    write SetSslECDHMethod;
        property  SslCryptoGroups  : String         read  FSslCryptoGroups
                                                    write FSslCryptoGroups;    { V8.51 }
        property  SslCliSecurity  : TSslCliSecurity read  FSslCliSecurity
                                                    write SetSslCliSecurity;   { V8.54 }
        property  SslAlpnProtocols : TStrings       read  FSslAlpnProtoList
                                                    write SetSslAlpnProtocols; { V8.56 }
        property  SslOcspStatus : Boolean           read  FSslOcspStatus
                                                    write FSslOcspStatus;      { V8.69 should we get OCSP staple status in handshake }
        property  UseSharedCAStore : Boolean        read  FUseSharedCAStore
                                                    write FUseSharedCAStore;   { V9.1 ignores CAFile, CALines, CAPath, uses IcsSslRootCAStore }
        property  CliCertTypes : TTlsCertTypes      read  FCliCertTypes
                                                    write FCliCertTypes;       { V9.5 TLS certificate types supported for clients }
        property  SrvCertTypes : TTlsCertTypes      read  FSrvCertTypes
                                                    write FSrvCertTypes;       { V9.5 TLS certificate types supported for servers }
        property  SslSessionTimeout : Cardinal      read  FSslSessionTimeout
                                                    write SetSslSessionTimeout;
        property  SslSessionCacheSize : Integer     read  FSslSessionCacheSize
                                                    write SetSslSessionCacheSize;
        property  SslDefaultSessionIDContext : TSslSessionIdContext
                                                read  FSslDefaultSessionIDContext
                                                write SetSslDefaultSessionIDContext;
        property  OnRemoveSession   : TSslContextRemoveSession
                                                    read  FOnRemoveSession
                                                    write FOnRemoveSession;
        property  OnBeforeInit  : TNotifyEvent      read  FOnBeforeInit
                                                    write FOnBeforeInit;
    {xIFNDEF OPENSSL_NO_ENGINE}
        property  AutoEnableBuiltinEngines : Boolean read  FAutoEnableBuiltinEngines
                                                     write FAutoEnableBuiltinEngines;
    //    property  CtxEngine : TSslEngine read FCtxEngine write SetCtxEngine;
    {xENDIF}
    end;

//procedure OutputDebugString(const Msg: String);
    function IcsSslOpenFileBio( const FileName : String;  Methode: TBioOpenMethode): PBIO;         { V8.39 was in TSslContext }
    function IcsSslOpenFileAStr(const FileName: String; Methode: TBioOpenMethode): AnsiString;     { V8.65 }
    function IcsSslLoadStackFromInfoFile(const FileName: String; Mode: TInfoExtractMode): PStack;  { V8.39 was in TSslContext }
    function IcsSslLoadStackFromInfoString(const Value: String; Mode: TInfoExtractMode): PStack;   { V8.41 was in TSslContext }
    function IcsSslLoadStackFromAStr(const PemStr: AnsiString; var Errs: String; Mode: TInfoExtractMode = emCert): PStack;  { V8.65 }
    function IcsSslLoadStackFromStr(const PemStr: String; var Errs: String; Mode: TInfoExtractMode = emCert): PStack;  { V8.65 }
    function IcsSslLoadStackFromInfoFileEx(const FileName: String; var Errs: String; Mode: TInfoExtractMode = emCert): PStack;   { V8.65 }
    function IcsSslLoadPkeyFromAStr(const PemStr, APassword: AnsiString; var Err: String): PEVP_PKEY;    { V8.65 }
    function IcsUnwrapNames(const S: String): String;                                              { V8.39 multi-line with comma line }
    function WSocketGetSslVerStr(ver: Cardinal): String;                  { V8.64 }
    function WSocketGetCliHelloStr(CliHello: TClientHelloData): String;   { V8.64 }
    function IcsReadStrBio(ABio: PBIO; MaxLen: Integer): AnsiString;      { V8.69 }
    function IcsSslLoadStackFromPemTB(const Certs: TBytes; var Errs: String; Mode: TInfoExtractMode = emCert): PStack;  { V9.1 }
    function IcsSslLoadStackFromP12TB(const Certs: TBytes; var Errs: String): PStack;   { V9.1 }
    function IcsReadTBBio(ABio: PBIO; MaxLen: Integer): TBytes;           { V9.1 }
    function IcsWriteStrBio(const Data: AnsiString): PBIO;                { V9.1 }
    function IcsWriteTBBio(const Data: TBytes): PBIO;                     { V9.1 }
    function sslRootCACertsBundle: TBytes;                                { V9.1 was string but now PKCS12 binary data }
    function IcsReportOpenSSLVer(RootCA: Boolean = False): String;        { V9.1 centralise version reporting }
    function IcsBuildCertFName(const Domain: String): String;             { V9.1 moved from WSocketS  }
    procedure IcsSetSignTest(SCheck: Boolean = True; SCertificate: Boolean = True);    { V9.3 }
    function IcsGetOpenSSLVer: Cardinal;                                  { V9.3 }
    function IcsSslLoadProviders(Legacy, Fips: Boolean): Boolean;         { V9.3 }
    procedure IcsSslLoad;                                                 { V9.3 }
    procedure IcsSslUnload;                                               { V9.3 }
    function IcsX509VerifyErrorToStr(ErrCode: Integer): String;           { V9.3 }

var
    SslCritSect : TIcsCriticalSection;
    IcsSslRootCAStore: TSslRootCAStore;                                  { V9.1 common Root CA Store }
    CARootTxt, CARootVer: String;                                        { V9.1 information about root bundle resource file }

{$ENDIF USE_SSL}

implementation

{$IFDEF USE_SSL}



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSslBaseComponent }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseComponent.RaiseLastOpenSslError(
    EClass          : ExceptClass;
    Dump            : Boolean = FALSE;
    const CustomMsg : String  = '');
begin
    FLastSslError := ERR_peek_error;
    FLastSslErrMsg := String(LastOpenSslErrMsg(False));   { V8.55 }
    if Length(CustomMsg) > 0 then
        FLastSslErrMsg := CustomMsg + ' - ' + FLastSslErrMsg;
    raise EClass.Create(#13#10 + FLastSslErrMsg + #13#10)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslBaseComponent.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FLastSslError   := 0;
    FSslInitialized := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslBaseComponent.Destroy;
begin
    try          { V8.71 JK }
        FinalizeSsl;
{$IFNDEF NO_DEBUG_LOG}
        { Removes IcsLogger's free notification in a thread-safe way }
        SetIcsLogger(nil);
{$ENDIF}
    finally
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseComponent.Assign(Source: TPersistent);          { V8.71 }
var
    LSource: TSslBaseComponent;
begin
    if Source is TSslBaseComponent then begin
        LSource := Source as TSslBaseComponent;
        FSslInitialized := LSource.FSslInitialized ;
        FLastSslError   := LSource.LastSslError;
        FLastSslErrMsg  := LSource.LastSslErrMsg;
        FSslPWUtf8      := LSource.FSslPWUtf8;
{$IFNDEF NO_DEBUG_LOG}
        SetIcsLogger(LSource.IcsLogger);
{$ENDIF}
    end;
  //  inherited Assign(Source);    { V8.71 we've done it all already }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseComponent.FinalizeSsl;
begin
    if not FSslInitialized then
        Exit;
    IcsUnloadSsl;      { V9.1 }
    FSslInitialized := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseComponent.InitializeSsl;
begin
    if FSslInitialized then
        Exit;
{$IFNDEF NO_DEBUG_LOG}
   if CheckLogOptions(loSslInfo) then  { V8.40 }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' LoadSSL');
{$ENDIF}
    IcsLoadSsl;     { V9.1 }
    FSslInitialized := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslBaseComponent.PasswordConvert(const PW: String): AnsiString;    { V8.55 }
begin
    if FSslPWUtf8 then    { V8.66 }
        Result := StringToUtf8(PW)
    else
        Result := AnsiString(PW);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}  { V5.21 }
{$IFNDEF NO_DEBUG_LOG}
function TSslBaseComponent.CheckLogOptions(const LogOption: TLogOption): Boolean;
begin
    Result := Assigned(FIcsLogger) and (LogOption in FIcsLogger.LogOptions);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseComponent.DebugLog(LogOption: TLogOption; const Msg: String);
begin
    if Assigned(FIcsLogger) then
        {if loAddStamp in FIcsLogger.LogOptions then
            FIcsLogger.DoDebugLog(Self, LogOption,
                                  IcsLoggerAddTimeStamp + ' ' + Msg)
        else}
        FIcsLogger.DoDebugLog(Self, LogOption, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseComponent.Notification(
    AComponent : TComponent;
    Operation  : TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if AComponent = FIcsLogger then
            FIcsLogger := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseComponent.SetIcsLogger(const Value: TIcsLogger);
begin
    if Value <> FIcsLogger then begin
        if FIcsLogger <> nil then
            FIcsLogger.RemoveFreeNotification(Self);
        if Value <> nil then
            Value.FreeNotification(Self);
        FIcsLogger := Value;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TX509Base }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TX509Base.Create(AOwner: TComponent; X509: Pointer = nil);
begin
    inherited Create(AOwner);
    FX509 := nil;
    FPrivateKey := nil;
    FX509Inters := nil;     { V8.41 }
    AssignDefaults;
    FSslPWUtf8 := True;     { V8.55 }
    if Assigned(X509) then begin
        InitializeSsl;
        FX509 := X509_dup(X509);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TX509Base.Destroy;
begin
    try          { V8.71 JK }
        ClearAll;
    finally
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.Assign(Source: TPersistent);          { V8.71 }
var
    LSource: TX509Base;
begin
    if Source is TX509Base then begin
        LSource := Source as TX509Base;
        SetX509(LSource.X509);
        SetPrivateKey(LSource.PrivateKey);
        SetX509Inters(LSource.X509Inters);
        Comments := LSource.Comments;
        CertName := LSource.CertName;
        KeyName := LSource.KeyName;
    end;
    inherited Assign(Source);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.FreeAndNilX509;
begin
    if Assigned(FX509) then begin
        X509_free(FX509);
        FX509 := nil;
        FSha1Hex  := '';
        FSha1Digest  := nil;
        FSha256Hex  := '';      { V8.83 }
        FSha256Digest  := nil;
        FComments := '';        { V8.67 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.FreeAndNilX509Inters;         { V8.41 }
begin
    if Assigned(FX509Inters) then begin
        OPENSSL_sk_pop_free(FX509Inters, @X509_free); { V8.65 fix memory leak }
        FX509Inters := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.FreeAndNilPrivateKey;
begin
    if Assigned(FPrivateKey) then begin
        EVP_PKEY_free(FPrivateKey);
        FPrivateKey := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.ClearAll;                                   { V8.40 }
begin
    if NOT FSslInitialized then Exit;
    FreeAndNilX509;
    FreeAndNilPrivateKey;
    FreeAndNilX509Inters;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.SetX509(X509: Pointer);
begin
    InitializeSsl;
    FreeAndNilX509;
    AssignDefaults;
    if Assigned(X509) then begin
        FX509 := X509_dup(X509);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.SetX509Inters(X509Inters: PStack);  { V8.41 }
var
    I: Integer;
begin
    InitializeSsl;
    FreeAndNilX509Inters;
    if Assigned(X509Inters) then begin
  //      FX509Inters := OPENSSL_sk_dup(X509Inters);   { V8.71 beware copies pointers only }
        FX509Inters := OPENSSL_sk_new_null;
        for I := 0 to OPENSSL_sk_num(X509Inters) - 1 do
            AddToInters(PX509(OPENSSL_sk_value(X509Inters, I)));  { V8.71 dup certs }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.SetPrivateKey(PKey: Pointer);
begin
    InitializeSsl;
    FreeAndNilPrivateKey;   { V8.71 }
    if Assigned(PKey) then
        FPrivateKey := Ics_EVP_PKEY_dup(PKey);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetX509PublicKey: Pointer;    { V8.52 renamed from GetPublicKey }
begin
    if Assigned(FX509) then
        Result := X509_get_pubkey(FX509)
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetX509PubKeyTB: TBytes;    { V9.1 }
var
    ABio : PBIO;
    PubKey: Pointer;
begin
    if NOT Assigned(FX509) then begin
        SetLength(Result, 0);
        Exit;
    end;
    ABio := BIO_new(BIO_s_mem);
    if Assigned(ABio) then
    try
        PubKey := X509_get_X509_PUBKEY(FX509);
        if PubKey = Nil then
            Exit;
        if i2d_X509_PUBKEY_bio(ABio, PubKey) = 0 then
            Exit;
        Result := IcsReadTBBio(ABio, 0);
    finally
        bio_free(ABio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetIsCertLoaded : Boolean;     { V8.41 }
begin
    result := Assigned(FX509);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TX509Base.GetIsPKeyLoaded : Boolean;     { V8.41 }
begin
    result := Assigned(FPrivateKey);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetIsInterLoaded : Boolean;    { V8.41 }
begin
    result := Assigned(FX509Inters);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetInterCount: Integer;                                { V8.41 }
begin
    Result := 0;
    if NOT GetIsInterLoaded then Exit;
    Result := OPENSSL_sk_num(FX509Inters);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetExtensionCount: Integer;
begin
    if Assigned(FX509) then
        Result := X509_get_ext_count(FX509)
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.CheckExtName(Ext: PX509_EXTENSION; const ShortName: String): Boolean;  { V8.41 }
var
    Len     : Integer;
    ExtStr  : PAnsiChar;
    B       : PBIO;
    Nid     : Integer;
begin
    Result := False;
    if NOT Assigned(Ext) then Exit;
    Nid := OBJ_obj2nid(X509_EXTENSION_get_object(Ext));
    if Nid <> NID_undef then begin
        ExtStr := OBJ_nid2sn(Nid);
        if StrLIComp(ExtStr, PAnsiChar(AnsiString(ShortName)), 255) = 0 then begin
            Result := True;
            Exit;
        end;
    end
    else begin // custom extension
        B := BIO_new(BIO_s_mem);
        if Assigned(B) then begin
            try
                i2a_ASN1_OBJECT(B, X509_EXTENSION_get_object(Ext));
                Len := BIO_ctrl(B, BIO_CTRL_PENDING_, 0, nil);          { V8.66 literal changed }
                if Len > 0 then begin
                    GetMem(ExtStr, Len);
                    try
                        Bio_read(B, ExtStr, Len);
                        if StrLIComp(ExtStr, PAnsiChar(AnsiString(ShortName)), 255) = 0 then begin
                            Result := True;
                            Exit;
                        end;
                    finally
                        FreeMem(ExtStr);
                    end;
                end;
            finally
                bio_free(B);
            end;
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetExtDetail(Ext: PX509_EXTENSION): TExtension;      { V8.41 }
var
    J        : Integer;
    Value    : PAnsiChar;
    Meth     : PX509V3_EXT_METHOD;
    Data     : PAnsiChar;
    Val      : PSTACK;
    NVal     : PCONF_VALUE;
    ext_str  : Pointer;
    B        : PBIO;
    Nid      : Integer;
    Extvalue : PASN1_STRING;   { V8.40 was octetstring  }
    Extlen   : Integer;
begin
    Result.Critical  := FALSE;
    Result.ShortName := '';
    Result.Value     := '';
    if not Assigned(Ext) then
        raise EX509Exception.Create('Extension not assigned');
    Value   := nil;
    Meth    := nil;
    Val     := nil;
    ext_str := nil;
    Result.Critical := X509_EXTENSION_get_critical(Ext) > 0;
    Nid := OBJ_obj2nid(X509_EXTENSION_get_object(Ext));
    if Nid <> NID_undef then
        Result.ShortName := String(StrPas(OBJ_nid2sn(Nid)))
    else begin // custom extension
        //B := nil;
        B := BIO_new(BIO_s_mem);
        if Assigned(B) then begin
            try
                i2a_ASN1_OBJECT(B, X509_EXTENSION_get_object(Ext));
             //   J := BIO_ctrl(B, BIO_CTRL_PENDING_, 0, nil);     { V8.69 now done in ReadStrBio }
                Result.ShortName := String(IcsReadStrBio(B, 0));   { V8.41 }
            finally
                bio_free(B);
            end;
        end;
    end;

    try
        Meth := Pointer(X509V3_EXT_get(Ext));
        if Meth = nil then begin
            Result.Value := UnknownExtDataToStr(Ext);
            Exit;
        end;
        Extvalue := X509_EXTENSION_get_data(Ext);   { V8.27 }
        Data := ASN1_STRING_get0_data(Extvalue);    { V8.40 }
        Extlen := ASN1_STRING_length(Extvalue);
        if Assigned(Meth^.it) then
            ext_str := ASN1_item_d2i(nil,
                                       @Data,
                                       Extlen,
                                       ASN1_ITEM_ptr(Meth^.it))
        else
            ext_str := Meth^.d2i(nil, @Data, Extlen);

        if not Assigned(ext_str) then begin
            Result.Value := UnknownExtDataToStr(Ext);
            Exit;
        end;

        if Assigned(Meth^.i2s) then begin
            Value := Meth^.i2s(Meth, ext_str);
            if Assigned(Value) then
                Result.Value := String(StrPas(Value));
        end
        else if Assigned(Meth^.i2v) then begin
            Val := Meth^.i2v(Meth, ext_str, nil);
            if not Assigned(Val) then
                Exit;
            J := 0;
            while J < OPENSSL_sk_num(val) do begin
                NVal := PCONF_VALUE(OPENSSL_sk_value(Val, J));
                if Length(Result.Value) > 0 then
                    Result.Value := Result.Value + #13#10;
                Result.Value := Result.Value + String(StrPas(NVal^.name));
                if (StrPas(NVal^.value) <> '') and (StrPas(NVal^.name) <> '') then
                    Result.Value := Result.Value + '=';
                Result.Value := Result.Value + String(StrPas(NVal^.value));
                Inc(J);
            end;
        end
        else if Assigned(Meth^.i2r) then begin
            //B := nil;
            B := BIO_new(BIO_s_mem);
            if Assigned(B) then
                try
                    Meth.i2r(Meth, ext_str, B, 0);
                    J := BIO_ctrl(B, BIO_CTRL_PENDING_, 0, nil);        { V8.66 literal changed }
                    if J > 0 then begin
                        Result.Value := String(IcsReadStrBio(B, J)); { V8.41 }
                        { This method separates multiple values by LF } // should I remove this stuff?
                        while (Length(Result.Value) > 0) and
                              (Result.Value[Length(Result.Value)] = #10) do
                            SetLength(Result.Value, Length(Result.Value) -1);
                        Result.Value := StringReplace(Result.Value, #10, #13#10, [rfReplaceAll]);
                    end;
                finally
                    bio_free(B);
                end;
        end;
    finally
        if Assigned(Val) then
            OPENSSL_sk_pop_free(Val, @X509V3_conf_free);
        if Assigned(Value) then
            CRYPTO_free(Value, Nil, 0);    { V8.66 }
        if Assigned(Meth) and Assigned(ext_str) then
            if Assigned(Meth^.it) then
                ASN1_item_free(ext_str, ASN1_ITEM_ptr(Meth^.it))
            else
                Meth^.ext_free(ext_str);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.ExtByName(const ShortName: String): Integer;
var
    Ext     : PX509_EXTENSION;
    Count   : Integer;
    I       : Integer;
begin
    Result := -1;
    if Assigned(FX509) then begin
        Count := GetExtensionCount;
        for I := 0 to Count -1 do begin
            Ext := X509_get_ext(FX509, I);
            if not Assigned(Ext) then
                Continue;
            if CheckExtName(Ext, ShortName) then begin    { V8.41 simplify }
                Result := I;
                Exit;
            end;
        end;
    end
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.UnknownExtDataToStr(Ext: PX509_Extension) : String;
begin
    Result := Asn1ToString(PASN1_STRING(X509_EXTENSION_get_data(Ext)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetExtension(Index: Integer): TExtension;
var
    ExtCount : Integer;
    Ext      : PX509_EXTENSION;
begin
    Result.Critical  := FALSE;
    Result.ShortName := '';
    Result.Value     := '';
    if not Assigned(FX509) then
        Exit;
    ExtCount := ExtensionCount;
    if (Index < 0) or (Index > ExtCount -1) then
        raise EX509Exception.Create('Extension index out of bounds');
    Ext := X509_get_ext(FX509, Index);
    if not Assigned(Ext) then
        raise EX509Exception.Create('Extension not assigned');
    Result := GetExtDetail(Ext);        { V8.41 simplify }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetExtensionByName(const S: String): TExtension;
var
    I       : Integer;
    Ext     : PX509_EXTENSION;
    Count   : Integer;
begin
    Result.Critical  := FALSE;
    Result.ShortName := '';
    Result.Value     := '';
    if NOT Assigned(FX509) then Exit;
    Count := GetExtensionCount;
    for I := 0 to Count - 1 do begin
        Ext := X509_get_ext(FX509, I);
        if not Assigned(Ext) then
            Continue;
        if CheckExtName(Ext, S) then begin    { V8.41 simplify }
            Result := GetExtDetail(Ext);
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetExtField(Ext: TExtension; const FieldName: String): String;   { V8.41 simplify }
var
    I  : Integer;
    Li : TStringList;
begin
    Result := '';
    Li := TStringList.Create;
    try
        if Length(Ext.ShortName) > 0 then begin
            Li.Text := Ext.Value;
            for I := 0 to Li.Count -1 do begin
                if (FieldName = '') then begin
                    if Result <> '' then Result := Result + #13#10;
                { V8.64 if domain has ACE xn--. convert it to Unicode, ignore errors }
                    Result := Result + IcsIDNAToUnicode(Li[I]);
                end
                else if (Pos(FieldName, IcsUpperCase(Li.Names[I])) = 1) then begin
                    if Result <> '' then Result := Result + #13#10;
                    Result := Result + IcsIDNAToUnicode(Copy (Li[I], Length(Li.Names[I])+2,999));
                end;
            end;
        end;
    finally
        Li.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetExtensionValuesByName(const ShortName, FieldName: String): String;
var
    Ext : TExtension;
begin
    Result := '';
    if not Assigned(X509) then
        Exit;
    Ext := GetExtensionByName(ShortName);
    if Length(Ext.ShortName) > 0 then begin
        Result := GetExtField(Ext, FieldName);   { V8.41 simplify }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.UnwrapNames(const S: String): String;
begin
    Result := IcsUnwrapNames(S);     { V8.56 simplify }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetIssuerOneLine: String;
var
    Str : AnsiString;
begin
    Result := '';
    if not Assigned(FX509) then
        Exit;

    SetLength(Str, 512);
    Str := X509_NAME_oneline(X509_get_issuer_name(FX509),
                               PAnsiChar(Str),
                               Length(Str));
    SetLength(Str, StrLen(PAnsiChar(Str)));
    Result := String(Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSerialNum: Int64;      { V8.40 was integer }
var
    serial_asn1: PASN1_INTEGER;
    serial_uint: UInt64;
begin
    if Assigned(FX509) then begin
        serial_asn1 := X509_get_serialNumber(FX509);
        serial_uint := 0;
        if (ASN1_INTEGER_get_uint64(@serial_uint, serial_asn1) = 0) then begin    { V8.66 var gone }
            Result := -1;
            ERR_clear_error;
        end
        else
            Result := Int64 (serial_uint);
    end
    else
        Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSha1Hash: AnsiString; { V7.31 }
{ * Deprecated and slow, use GetSha1Digest or GetSha1Hex * }
begin
    if Assigned(FX509) then begin
        SetLength(Result, 20);
        Move(Sha1Digest[0], Pointer(Result)^, 20);
    end
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.Sha1Hash: AnsiString;
{ * Deprecated and slow, use Sha1Digest or Sha1Hex * }
begin
    Result := GetSha1Hash;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSha1Digest: THashBytes20;
var
    Len : Integer;
begin
    if Assigned(FX509) and (FSha1Digest = nil) then begin
        SetLength(FSha1Digest, 20);
        if X509_digest(FX509, EVP_sha1, @FSha1Digest[0], @Len) = 0 then
        begin
            FSha1Digest := nil;
            RaiseLastOpenSslError(EX509Exception, TRUE);
        end;
    end;
    Result := FSha1Digest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSha1Hex: String;            { aka fingerprint }
begin
    if FSha1Hex = '' then begin
        GetSha1Digest;
        if Assigned(FSha1Digest) then   { V8.53 sanity test, may be no certificate }
            FSha1Hex := IcsBufferToHex(FSha1Digest[0], 20);
    end;
    Result := FSha1Hex;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSha256Digest: THashBytes20;                 { V8.63 }
var
    Len : Integer;
begin
    if Assigned(FX509) and (FSha256Digest = nil) then begin
        SetLength(FSha256Digest, 32);
        if X509_digest(FX509, EVP_sha256, @FSha256Digest[0], @Len) = 0 then
        begin
            FSha256Digest := nil;
            RaiseLastOpenSslError(EX509Exception, TRUE);
        end;
    end;
    Result := FSha256Digest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSha256Hex: String;            { aka fingerprint V8.63 }
begin
    if FSha256Hex = '' then begin
        GetSha256Digest;
        if Assigned(FSha256Digest) then
            FSha256Hex := IcsBufferToHex(FSha256Digest[0], 32);
    end;
    Result := FSha256Hex;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSubjectAltName: TExtension;
begin
    Result := GetExtensionByName('subjectAltName');  { V8.41 simplify }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Changed to return a list, separated by CRLF }
function TX509Base.GetSubjectCName: String;
var
    Subj    : PX509_NAME;
    Entry   : PX509_NAME_ENTRY;
    Asn1    : PASN1_STRING;
    LastPos : Integer;
begin
    Result := '';
{$IFNDEF WIN64}    { V7.81 }
  {$IFNDEF COMPILER24_UP}
    Entry  := nil;
  {$ENDIF}
{$ENDIF}
    if not Assigned(FX509) then
        Exit;
    Subj := X509_get_subject_name(FX509);
    if Subj <> nil then
    begin
        LastPos := -1;
        repeat
            LastPos := X509_NAME_get_index_by_NID(Subj, NID_commonName, LastPos);
            if LastPos > -1 then
                Entry := X509_NAME_get_entry(Subj, LastPos)
            else
                Break;
            if Assigned(Entry) then begin
                Asn1 := X509_NAME_ENTRY_get_data(Entry);
                if Assigned(Asn1) then
                { V8.64 if domain has ACE xn--. convert it to Unicode, ignore errors }
                    Result := Result + IcsIDNAToUnicode(Asn1ToString(Asn1)) + #13#10;
            end;
        until
            LastPos = -1;

        while (Length(Result) > 0) and (Word(Result[Length(Result)]) in [Ord(#13), Ord(#10)]) do
            SetLength(Result, Length(Result) - 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSubjectOneLine: String;   { legacy, does not handle UTF8 characters }
var
    Str : AnsiString;
begin
    Result := '';
    if not Assigned(FX509) then
        Exit;
    SetLength(Str, 512);
    Str := X509_NAME_oneline(X509_get_subject_name(FX509), PAnsiChar(Str), Length(Str));
    SetLength(Str, StrLen(PAnsiChar(Str)));
    Result := String(Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetVerifyErrorMsg: String;
begin
    if Assigned(FX509) then
        Result := IcsX509VerifyErrorToStr(FVerifyResult)  { V8.39 better function }
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetFirstVerifyErrorMsg: String;            {05/21/2007 AG}
begin
    if Assigned(FX509) then
        Result := IcsX509VerifyErrorToStr(FFirstVerifyResult)  { V8.39 better function }
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetValidNotAfter: TDateTime;                 {AG 02/06/06}
begin
    if Assigned(FX509) then
        if Asn1ToUTDateTime(X509_get0_notAfter(FX509), Result) then    { V8.66 }
            Exit;
    Result := MinDateTime;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetValidNotBefore: TDateTime;                {AG 02/06/06}
begin
    if Assigned(FX509) then
        if Asn1ToUTDateTime(X509_get0_notBefore(FX509), Result) then   { V8.66 }
            Exit;
    Result := MaxDateTime;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetHasExpired: Boolean;                      {AG 02/06/06}

    function GetCurrentBias : TDateTime;
    const
        MinsPerDay = 1440;
    begin
        Result := IcsGetLocalTimeZoneBias / MinsPerDay;
    end;

    function CompDateTime(const A, B: TDateTime): Integer;
    begin
        if Trunc(A) = Trunc(B) then
            Result := 0
        else if A < B then
            Result := -1
        else
            Result := 1;
    end;

var
    CurUT  : TDateTime;
begin
    CurUT  :=  Now + GetCurrentBias;
    Result := (CompDateTime(CurUT, ValidNotAfter)  = 1) or (CompDateTime(CurUT, ValidNotBefore) = -1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.AssignDefaults;
begin
    FVerifyDepth        := 0;
    FSha1Hex            := '';
    FSha1Digest         := nil;
    FSha256Hex          := '';
    FSha256Digest       := nil;
    FVerifyResult       := X509_V_ERR_APPLICATION_VERIFICATION;
    FCustomVerifyResult := X509_V_ERR_APPLICATION_VERIFICATION;
    FFirstVerifyResult  := X509_V_ERR_APPLICATION_VERIFICATION;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ note - generally don't need this any more since Host is checked during verificaton }
function TX509Base.PostConnectionCheck(HostOrIp: String): Boolean;
var
    I      : Integer;
    Ext    : TExtension;
    Li     : TStringList;
    Mask   : TMask;
begin
    Result := FALSE;
    if (not Assigned(FX509)) or (Length(HostOrIp) = 0) then
        Exit;
    Li := TStringList.Create;
    try
        Li.Text := GetSubjectCName;
        for I := 0 to Li.Count - 1 do begin
            if Li[I] <> '' then begin
                Mask := TMask.Create(Li[I]);
                try
                    Result := Mask.Matches(HostOrIP);
                    if Result then Exit;
                finally
                    Mask.Free;
                end;
            end;
        end;

        Ext := GetSubjectAltName;
        if Length(Ext.ShortName) > 0 then begin
            Li.Text := Ext.Value;
            for I := 0 to Li.Count -1 do begin
                if (Pos('IP',  IcsUpperCase(Li.Names[I])) = 1) or
                   (Pos('DNS', IcsUpperCase(Li.Names[I])) = 1) then begin
            {        Mask := TMask.Create(Li.Values[Li.Names[I]]); only checks first name, ignores alternatives  }
                    Mask := TMask.Create(Copy(Li[I], Length(Li.Names[I])+2,999)); { V8.09 }
                    try
                        Result := Mask.Matches(HostOrIP);
                        if Result then Exit;
                    finally
                        Mask.Free;
                    end;
                end;
            end;
        end;
    finally
        Li.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.PrivateKeyLoadFromPemFile(const FileName: String; const Password: String = '');
var
    PKey    : PEVP_PKEY;
    PemStr: AnsiString;  { V8.65 }
    Err: String;
begin
    InitializeSsl;
    PemStr := IcsSslOpenFileAStr(FileName, bomReadOnly);  { V8.65 AStr instead of BIO }
    PKey := IcsSslLoadPkeyFromAStr(PemStr, PasswordConvert(Password), Err);  { V8.65 }
    if not Assigned(PKey) then
        raise ESslTX509Exception.Create('Can''t read private key file: ' + Err + ' - ' + Filename);
    try
        if Assigned(FX509) and (X509_check_private_key(FX509, PKey) < 1) then
            raise EX509Exception.Create('Certificate and private key do not match - ' + Filename);
        SetPrivateKey(PKey);
    finally
        EVP_PKEY_free(PKey);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 allow encryption of private key with extra params }
{ save as PKCS#8 EncryptedPrivateKeyInfo with PKCS#5 v2.0 encryption }
procedure TX509Base.PrivateKeySaveToPemFile(const FileName: String;
                                          const Password: String = ''; PrivKeyType: TSslPrivKeyCipher = PrivKeyEncNone);
var
    FileBio : PBIO;
begin
    InitializeSsl;
    if not Assigned(FPrivateKey) then
        raise EX509Exception.Create('Private key not assigned');
    FileBio := IcsSslOpenFileBio(FileName, bomWrite);  { V8.40 }
    try
        WritePkeyToBio(FileBio, Password, PrivKeyType);       { V8.40 }
    finally
        bio_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.PublicKeySaveToPemFile(const FileName: String);        { V8.40 }
var
    FileBio : PBIO;
begin
    InitializeSsl;
    if not Assigned(FPrivateKey) then
        raise EX509Exception.Create('Private key not assigned');
    FileBio := IcsSslOpenFileBio(FileName, bomWrite);  { V8.40 }
    try
        if PEM_write_bio_PUBKEY(FileBio, FPrivateKey) = 0 then
            RaiseLastOpenSslError(EX509Exception, TRUE,
                                       'Error writing public key to ' + FileName);
    finally
        bio_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.PublicKeySaveToText: String;                             { V8.52 }
var
    MemBio: PBIO;
//  Len: Integer;
begin
    InitializeSsl;
    Result := '';
    if not Assigned(FPrivateKey) then
        raise EX509Exception.Create('Private key not assigned');
    MemBio := BIO_new(BIO_s_mem);
    if Assigned(MemBio) then
    try
        if PEM_write_bio_PUBKEY(MemBio, FPrivateKey) = 0 then
            RaiseLastOpenSslError(EX509Exception, TRUE,'Error writing public key to text');
     // Len := BIO_ctrl(MemBio, BIO_CTRL_PENDING_, 0, nil);          { V8.69 now done in ReadStrBio }
        Result := String(IcsReadStrBio(MemBio, 0));
    finally
        bio_free(MemBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.PublicKeyLoadFromText(const Lines: String);               { V8.52 }
var
    MemBio : PBIO;
begin
    if (Pos(PEM_STRING_HDR_BEGIN, Lines) = 0) and (Pos(PEM_STRING_HDR_END, Lines) = 0) then
               Raise EX509Exception.Create('Expected a Base64 encoded PEM public key');
    MemBio := BIO_new_mem_buf(PAnsiChar(AnsiString(Lines)), Length (Lines));
    try
        PrivateKey := PEM_read_bio_PUBKEY(MemBio, Nil, Nil, Nil);
        if NOT Assigned (FPrivateKey) then
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Failed to read public key');
    finally
        bio_free(MemBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ .PEM, .CER, .CRT - Base64 encoded DER  }
{ .DER, .CER, .CRT - binary DER }
procedure TX509Base.LoadFromPemFileEx(const FileName: String; IncludePKey: TCertReadOpt;
                                           IncludeInters: TCertReadOpt; const Password: String; var Errs: String);    { V8.65 }
var
    PemStr: AnsiString;  { V8.65 }
begin
    InitializeSsl;
    PemStr := IcsSslOpenFileAStr(FileName, bomReadOnly);  { V8.65 AStr instead of BIO }
    ReadFromAStr(PemStr, IncludePKey, IncludeInters, Password, Errs);  { V8.65 }
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ .PEM, .CER, .CRT - Base64 encoded DER  }
{ .DER, .CER, .CRT - binary DER }
procedure TX509Base.LoadFromPemFile(const FileName: String; IncludePKey: TCertReadOpt;
                                                IncludeInters: TCertReadOpt = croNo; const Password: String = '');    { V8.40 }
var
    Errs: String;
begin
    LoadFromPemFileEx(FileName, IncludePKey, IncludeInters, Password, Errs);
    if Errs <> '' then
        raise EX509Exception.Create(Errs + ' - ' + Filename);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.LoadFromPemFile(const FileName: String; IncludePrivateKey: Boolean = FALSE; const Password: String = '');
var
    IncludePKey: TCertReadOpt;
begin
    if IncludePrivateKey then
        IncludePKey := croYes
    else
        IncludePKey := croNo;
    LoadFromPemFile(FileName, IncludePKey, croNo, Password);     { V8.40 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ base64 enccoded PEM }
procedure TX509Base.LoadFromTextEx(Lines: String; IncludePKey: TCertReadOpt;
                                                    IncludeInters: TCertReadOpt; const Password: String; var Errs: String);   { V8.65 }
begin
    ReadFromAStr(AnsiString(Lines), IncludePKey, IncludeInters, Password, Errs);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ base64 enccoded PEM }
procedure TX509Base.LoadFromText(Lines: String; IncludePKey: TCertReadOpt = croNo;
                                                           IncludeInters: TCertReadOpt = croNo; const Password: String = '');   { V8.40 }
var
    Errs: String;
begin
    LoadFromTextEx(Lines, IncludePKey, IncludeInters, Password, Errs);
    if Errs <> '' then
        raise EX509Exception.Create(Errs);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.LoadFromText(Lines: String; IncludePrivateKey: Boolean = False; const Password: String = ''); { V8.27 }
var
    IncludePKey: TCertReadOpt;
begin
    if IncludePrivateKey then
        IncludePKey := croYes
    else
        IncludePKey := croNo;
    LoadFromText(Lines, IncludePKey, croNo, Password);     { V8.40 }
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 check certificate and private key loaded and they match }
function TX509Base.CheckCertAndPKey: boolean;                         { V8.40 }
begin
    Result := False;
    if NOT Assigned(FX509) then Exit;
    if NOT Assigned(FPrivateKey) then Exit;
    Result := (X509_check_private_key(FX509, FPrivateKey) = 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 reads base64 or binary DER certificates }
{ warning - if PrivasteKey already loaded and not IncludePrivateKey will }
{   attempt to verify new certificate and key match }
procedure TX509Base.ReadFromAStr(const PemStr: AnsiString; IncludePKey,
                                                   IncludeInters: TCertReadOpt; const Password: String; var Errs: String);    { V8.65 }
var
    X         : PX509;
    PKey      : PEVP_PKEY;
    TotInfo, MaxLen: Integer;
    MyStack   : PStack;
    MemBio    : PBIO;
    Err       : String;
begin
    Errs := '';
    InitializeSsl;

 { V8.41 check if base64 or binary DER by search for ---BEGIN }
    MaxLen := Length(PemStr);
    if (IcsAnsiPosEx(PEM_STRING_HDR_BEGIN, PemStr) = 0) then begin

 { DER file only has a single certificate, no key }
        MemBio := BIO_new_mem_buf(PAnsiChar(PemStr), MaxLen);
        X := d2i_X509_bio(MemBio, Nil);  { V8.40 binary DER }
        BIO_free(MemBIO);
        if NOT Assigned (X) then begin
            ERR_clear_error; { ignore SSL error }
            Errs := 'Error reading X509 DER certificate';  {V8.41 }
        end;
        SetX509(X);
        Exit;
    end;

 { V8.41 read multiple base64 certificates from PEM file or lines }
 { V8.65 new loader that returns errors for any bad certificates  }
    MyStack := IcsSslLoadStackFromAStr(PemStr, Errs, emCert);
    if Errs <> '' then Errs := 'Reading X509 Base64 certificate: ' + Errs;   { V8.65 }
    if NOT Assigned (MyStack) then Exit;
    try
        TotInfo := OPENSSL_sk_num(MyStack);
        if TotInfo = 0 then begin
            Errs := Errs + 'No X509 Base64 certificate found';
            Exit;
        end;

 // pending, search stack for server certificate, might not be first

    { first in stack is server certificate }
//        SetX509(PX509(OPENSSL_sk_delete(MyStack, 0)));
        X := PX509(OPENSSL_sk_delete(MyStack, 0));  { V8.65 fix memory leak }
        SetX509(X);
        X509_free(X);

    { remainder are intermediates }
        if (TotInfo > 1) and (IncludeInters > croNo) then begin
            FreeAndNilX509Inters;
            FX509Inters := OPENSSL_sk_new_null;
            if FX509Inters = nil then begin
                Errs := Errs + 'Error creating X509 stack';
                Exit;
            end;
            while OPENSSL_sk_num(MyStack) > 0 do begin
                X := X509_dup(PX509(OPENSSL_sk_delete(MyStack, 0)));
                OPENSSL_sk_insert(FX509Inters, PAnsiChar(X), - 1);
            end;
        end;
   finally
       OPENSSL_sk_pop_free(MyStack, @X509_free);
   end;

 { look for PKCS8 PRIVATE KEY or ENCRYPTED PRIVATE KEY in PEM file }
 { V8.65 find private key block to avoid passing certs and comments to APIs }
    if IncludePKey > croNo then begin
        PKey := IcsSslLoadPkeyFromAStr(PemStr, PasswordConvert(Password), Err);
        if not Assigned(PKey) then begin
            if IncludePKey = croYes then begin { V8.40 require key so error }
                Errs := Errs + Err;
                Exit;
            end;
            ERR_clear_error; // ignore SSL error
        end
        else begin
            try
                if X509_check_private_key(FX509, PKey) < 1 then
                    Errs := Errs + 'Certificate and private key do not match';
                 SetPrivateKey(PKey);
            finally
                EVP_PKEY_free(PKey);
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 write X509 certificate to BIO, optionally with raw certificate fields }
procedure TX509Base.WriteCertToBio(ABio: PBIO; AddInfoText: Boolean = FALSE; const FName: String = '');
begin
    if not Assigned(ABio) then
        raise EX509Exception.Create('BIO not assigned');
    if not Assigned(FX509) then
        raise EX509Exception.Create('X509 not assigned');
    if AddInfoText then begin
        WriteStrBio(ABio, StringToUtf8(CertInfo) + #13#10, True);  { V8.65 UTF8 not ANSI }
    end;
    if PEM_write_bio_X509(ABio, FX509) = 0 then
        RaiseLastOpenSslError(EX509Exception, TRUE, 'Error writing certificate to ' + FName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 write private key to BIO with encryption }
procedure TX509Base.WritePkeyToBio(ABio: PBIO; const Password: String = '';
                                                    PrivKeyType: TSslPrivKeyCipher = PrivKeyEncNone; const FName: String = '');
var
    ret : integer;
    PWStr: AnsiString;  { V8.65 local password }
begin
    if not Assigned(ABio) then
        raise EX509Exception.Create('BIO not assigned');
    if not Assigned(FPrivateKey) then
        raise EX509Exception.Create('Private key not assigned');
  //  ret := PEM_write_bio_PrivateKey(ABio, FPrivateKey, nil, nil, 0, nil, nil); // { old way }
    if PrivKeyType = PrivKeyEncNone then  { V8.40 }
        ret := PEM_write_bio_PKCS8PrivateKey(ABio, FPrivateKey, nil, nil, 0, nil, nil)
    else begin
        PWStr := PasswordConvert(Password);  { V8.55 convert it to UTF8 if supported }
        ret := PEM_write_bio_PKCS8PrivateKey(ABio, FPrivateKey,
                 IcsSslGetEVPCipher (SslPrivKeyEvpCipher[PrivKeyType]),
                                    PAnsiChar(PWStr), Length(PWStr), Nil, Nil);   { V8.65 }
    end;
    if ret = 0 then
        RaiseLastOpenSslError(EX509Exception, TRUE, 'Error writing private key to ' + FName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.WriteToBio(ABio: PBIO; IncludePrivateKey: Boolean = FALSE; AddInfoText: Boolean = FALSE; const FName: String = '');
begin
    WriteCertToBio(ABio, AddInfoText, FName);          { V8.40 split writing cert and key }
    if IncludePrivateKey then begin
        WriteStrBio(ABio, #10#10);  { V8.41 blank lines between certs }
        WritePkeyToBio(ABio, FName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ returns formatted text with raw certificate fields }
function TX509Base.GetRawText: String;    {05/21/2007 AG}
var
    ABio : PBIO;
 // Len  : Integer;
begin
    Result := '';
    if FX509 = nil then Exit;
    ABio := BIO_new(BIO_s_mem);
    if Assigned(ABio) then
    try
        X509_print(ABio, FX509);
    //  Len := BIO_ctrl(ABio, BIO_CTRL_PENDING_, 0, nil);             { V8.69 now done in ReadStrBio }
        Result := String(IcsReadStrBio(ABio, 0));  { V8.41 }
    finally
        bio_free(ABio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ returns formatted text with raw private, public and parameter key fields }
function TX509Base.GetPKeyRawText(PubOnly: Boolean = False): String;    { V8.40, V8.65}
var
    ABio : PBIO;
//  Len  : Integer;
begin
    Result := '';
    if FPrivateKey = nil then Exit;
    ABio := BIO_new(BIO_s_mem);
    if Assigned(ABio) then
    try
        if NOT PubOnly then  { V8.65 }
            EVP_PKEY_print_private(ABio, FPrivateKey, 4, Nil);
        EVP_PKEY_print_public(ABio, FPrivateKey, 4, Nil);
 //       EVP_PKEY_print_params(ABio, FPrivateKey, 4, Nil);  { V8.65 not sure how useful }
//      Len := BIO_ctrl(ABio, BIO_CTRL_PENDING_, 0, nil);         { V8.69 now done in ReadStrBio }
        Result := String(IcsReadStrBio(ABio, 0));  { V8.41 }
    finally
        bio_free(ABio);
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ returns base64 encoded DER PEM certificate as string }
function TX509Base.SaveCertToText(AddInfoText: Boolean = FALSE): String;       { V8.40}
var
    ABio : PBIO;
//  Len  : Integer;
begin
    Result := '';
    if FX509 = nil then Exit;
    ABio := BIO_new(BIO_s_mem);
    if Assigned(ABio) then
    try
        WriteCertToBio(ABio, AddInfoText);          { V8.40 split writing cert and key }
    //  Len := BIO_ctrl(ABio, BIO_CTRL_PENDING_, 0, nil);           { V8.69 now done in ReadStrBio }
        Result := String(IcsReadStrBio(ABio, 0));  { V8.41 }
    finally
        bio_free(ABio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 allow encryption of private key with extra params }
procedure TX509Base.SaveToPemFile(const FileName: String; IncludePrivateKey: Boolean = FALSE; AddInfoText: Boolean = FALSE;
                    IncludeInters: Boolean = FALSE; const Password: String = ''; PrivKeyType: TSslPrivKeyCipher = PrivKeyEncNone);
var
    FileBio : PBIO;
begin
    InitializeSsl;
    FileBio := IcsSslOpenFileBio(FileName, bomWrite);  { V8.40 }
    try
        WriteCertToBio(FileBio, AddInfoText, FileName);          { V8.40 split writing cert and key }
        if IncludePrivateKey and IsPKeyLoaded then begin
            if NOT CheckCertAndPKey then                   { V8.41 }
                raise EX509Exception.Create('Certificate and private key do not match');
            WriteStrBio(FileBio, #10#10);  { V8.41 blank lines between certs }
            WriteStrBio(FileBio, StringToUtf8(GetPrivateKeyInfo) + #10);  { V8.65 UTF8 }
            WritePkeyToBio(FileBio, Password, PrivKeyType, FileName);
        end;
        // write intermediate certificates
        if IncludeInters and IsInterLoaded then begin
            WriteStrBio(FileBio, #10#10);  { blank lines between certs }
            WriteIntersToBio(FileBio, AddInfoText, FileName);
        end;
    finally
        bio_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ returns private key base64 encoded DER PEM }
function TX509Base.SavePKeyToText(const Password: String = ''; PrivKeyType: TSslPrivKeyCipher = PrivKeyEncNone): String;   { V8.40}
var
    ABio  : PBIO;
//  Len  : Integer;
begin
    Result := '';
    if not Assigned(FPrivateKey) then
        raise EX509Exception.Create('Private key not assigned');
    ABio := BIO_new(BIO_s_mem);
    if Assigned(ABio) then
    try
        WritePkeyToBio(ABio, Password, PrivKeyType);
 //     Len := BIO_ctrl(ABio, BIO_CTRL_PENDING_, 0, nil);           { V8.69 now done in ReadStrBio }
        Result := String(IcsReadStrBio(ABio, 0));  { V8.41 }
    finally
        bio_free(ABio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.SaveToP12Buf(const Password: String; IncludeInters: Boolean = FALSE;
                              PrivKeyType: TSslPrivKeyCipher = PrivKeyEncNone; IncludeKey: Boolean = TRUE): AnsiString;   { V8.67, V9.1 addd Key }
var
    ABio : PBIO;
    P12 : PPKCS12;
    PW: PAnsiChar;
    PWStr: AnsiString;    { V8.65 local password }
//    Len,
    certenc, keyenc, iterenc, itermac: integer;
    castack: Pointer;
const
{ MS key usage constants }
    KEY_EX  = $10;   { signing and encryption }
    KEY_SIG = $80;   { signing only }
begin
    Result := '';
    if not Assigned(FX509) then
        raise EX509Exception.Create('X509 not assigned');
  { must write cert and key, otherwise assumes only intermediates }
    if IncludeKey then begin     { V9.1 }
        if not Assigned(FPrivateKey) then
            raise EX509Exception.Create('Private key not assigned');
        if NOT CheckCertAndPKey then                   { V8.41 }
            raise EX509Exception.Create('Certificate and private key do not match');
    end;
    if IncludeInters and not Assigned(FX509Inters) then
        raise EX509Exception.Create('Intermediate X509 not assigned');
    ABio := BIO_new(BIO_s_mem);
    try
        PW := Nil;
        castack := Nil;
        keyenc := -1;
        certenc := -1;
        iterenc := 0;
        itermac := -1;
        if (Length(Password) > 0) and (PrivKeyType <> PrivKeyEncNone) then begin
            PWStr := PasswordConvert(Password);  { V8.55 convert it to UTF8 if supported }
            PW := PAnsiChar(PWStr);  { V8.65 avoid pointer going out of scope }

          { V8.67 3DES not supported by OpenSSL 3.0 unless legacy module is loaded,
             but AES256 not recognised until Windows Server 2016 v1709 and Windows 10 v1709,
             so Server 2012, Windows 10 RTM and earlier won't load AES passworded keys.  }
         { V9.3 ignore P12 TripleDES encryption unless legacy provider is loaded }
            if (PrivKeyType = PrivKeyEncTripleDES) and ICS_OSSL3_LOADED_LEGACY then begin
                certenc := NID_pbe_WithSHA1And128BitRC2_CBC;
                keyenc := NID_pbe_WithSHA1And3_Key_TripleDES_CBC;
                iterenc := 128;
                itermac := 1;
            end
            else begin
                certenc := NID_aes_256_cbc;
                keyenc := NID_aes_256_cbc;
                iterenc := 2048;
                itermac := 1;
            end;
        end;
        if IncludeInters then
            castack := OPENSSL_sk_dup(FX509Inters);    { V8.41 only if required }
        P12 := PKCS12_create(PW, PAnsiChar(AnsiString(IcsIDNAToASCII(IcsUnwrapNames(SubjectCName)))),  { V8.64 }
                 Ics_EVP_PKEY_dup(FPrivateKey), X509_dup(FX509), castack, keyenc, certenc, iterenc, itermac, KEY_EX);   { V8.67 better backward compatibility }

        if not Assigned(P12) then begin
            if (PrivKeyType = PrivKeyEncTripleDES) and (ICS_OPENSSL_VERSION_MAJOR >= 3) then  { V8.67 }
                raise EX509Exception.Create('3DES key encryption not supported without legacy module')
            else
                RaiseLastOpenSslError(EX509Exception, TRUE, 'Error creating PKCS12 certificate');
        end;
        if i2d_PKCS12_bio(ABio, P12) = 0 then
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error writing PKCS12 certificate to BIO');
    //  Len := BIO_ctrl(ABio, BIO_CTRL_PENDING_, 0, nil);     { V8.69 now done in ReadStrBio }
        Result := IcsReadStrBio(ABio, 0);  // warning binary data
        PKCS12_free(P12);
    finally
        bio_free(ABio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.LoadFromP12Buffer(ABuffer: Pointer; ABufferSize: Cardinal;
                                                 IncludePKey, IncludeInters: TCertReadOpt; const Password: String);  { V8.63 }
var
    FileBio: PBIO;
    P12: PPKCS12;
    Cert: PX509;
    PKey: PEVP_PKEY;
    Ca: PSTACK_OF_X509;
    PW: PAnsiChar;
    PWStr: AnsiString;    { V8.65 local password }
    I, err: integer;
begin
    InitializeSsl;

    FileBio := BIO_new_mem_buf(ABuffer, ABufferSize);
    if not Assigned(FileBio) then
      RaiseLastOpenSslError(EX509Exception, TRUE, 'Error reading PKCS12 certificates from buffer');
    try
        P12 := d2i_PKCS12_bio(FileBio, Nil);
        if not Assigned(P12) then
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error reading PKCS12 certificates from buffer - ' + String(IcsSsl_print_error));
        try
            Cert := Nil;
            Pkey := Nil;
            Ca := Nil;
            PW := Nil;
            if Length(Password) > 0 then begin
                PWStr := PasswordConvert(Password);  { V8.55 convert it to UTF8 if supported }
                PW := PAnsiChar(PWStr);              { V8.65 avoid pointer going out of scope }
            end;
            if PKCS12_parse(P12, PW, @Pkey, @Cert, @Ca) = 0 then begin
                err := Ics_Ssl_ERR_GET_REASON(ERR_peek_error);
                if ((err = 108) or (err = 113)) and (PW <> Nil) then begin { PKCS12_R_MAC_ABSENT or PKCS12_R_MAC_VERIFY_FAILURE }
                    if PKCS12_parse(P12, Nil, @Pkey, @Cert, @Ca) = 0 then   { V8.67 retry without password }
                        err := Ics_Ssl_ERR_GET_REASON(ERR_peek_error)
                    else
                        err := 0;
                end ;
                if err <> 0 then begin
                    if ((err = 108) or (err = 113)) then
                        raise EX509Exception.Create('Error PKCS12 certificate password invalid')
                    else
                        raise EX509Exception.Create('Error PKCS12 certificate - ' + String(IcsSsl_print_error));  { V8.69 more detail}
                end;
            end;
            if (IncludePKey > croNo) then begin
                if Assigned(PKey) then begin
                    if (X509_check_private_key(Cert, PKey) < 1) then
                        raise EX509Exception.Create('Certificate and private key do not match');
                     SetX509(Cert);
                     SetPrivateKey(PKey);
                     EVP_PKEY_free(PKey);
                end
                else begin
                    if IncludePKey = croYes then  { V8.50 require  private key so error }
                        raise EX509Exception.Create('Error reading private key from buffer');
                end;
            end
            else
                SetX509(Cert);
            X509_free(Cert);
            FreeAndNilX509Inters;

          { intermediate certificates are optional, no error if none found }
            if (IncludeInters > croNo) and Assigned(Ca) then begin
                FX509Inters := OPENSSL_sk_new_null;
                for I := 0 to OPENSSL_sk_num(Ca) - 1 do
                    OPENSSL_sk_insert(FX509Inters, PAnsiChar(X509_dup (PX509(OPENSSL_sk_value(Ca, I)))), I);
                OPENSSL_sk_free(Ca);
            end;
        finally
            PKCS12_free(p12);
        end;
    finally
        bio_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ PKCS#12, may contain binary certificate(s) and private keys (password protected)
  typical file extension .P12 or .PFX }
procedure TX509Base.LoadFromP12File(const FileName: String; IncludePKey: TCertReadOpt = croNo;
                                                      IncludeInters: TCertReadOpt = croNo; const Password: String = '');    { V8.40 }
var
    P12Str: AnsiString;  { V8.65 }
begin
    InitializeSsl;
    P12Str := IcsSslOpenFileAStr(FileName, bomReadOnly);  { V8.65 AStr instead of BIO }
    if Length(P12Str) = 0 then Exit;  { V8.69 }
    LoadFromP12Buffer(@P12Str[1], Length(P12Str), IncludePKey, IncludeInters, Password);  { V8.65 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ PKCS#7 certificates, typical file extension .P7B, .P7R, .P7S, .SPC }
procedure TX509Base.LoadFromP7BFile(const FileName: String; IncludeInters: TCertReadOpt = croNo);   { V8.40 }
var
    FileBio : PBIO;
    p7 : PPKCS7;
    nid, total, I : integer;
    MyStack: PSTACK_OF_X509;
    AStr: AnsiString;
const
    ReadMax = 2048;
begin
    InitializeSsl;
    FileBio := IcsSslOpenFileBio(FileName, bomReadOnly);
    MyStack := Nil;
    try
     { V8.41 check if base64 or binary DER by search for ---BEGIN }
        AStr := IcsReadStrBio(FileBio, Readmax);
        if Length(AStr) <= 0 then
            raise EX509Exception.Create('Certificate file is empty, ' + FileName);  {V8.41 }
        BIO_ctrl(FileBio, BIO_CTRL_RESET, 0, nil);
        if (Pos(PEM_STRING_HDR_BEGIN, String(AStr)) > 0) then
            p7 := Pointer(PEM_read_bio_PKCS7(FileBio, Nil, Nil, Nil))  { base64 version }     { V8.66 }
        else
            p7 := Pointer(d2i_PKCS7_bio(FileBio, Nil));            { DER binary version }      { V8.66 }
        if NOT Assigned (p7) then
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error reading PKCS7 certificate from file ' + FileName);
        // now extract X509
        nid := OBJ_obj2nid(p7.type_);
        if nid = NID_pkcs7_signed then
            if p7.d.sign <> Nil then MyStack := p7.d.sign.cert  { V8.68 support OpenSSL 3.0 structure }
        else if nid = NID_pkcs7_signedAndEnveloped then
            if p7.d.signed_and_enveloped <> Nil then MyStack := p7.d.signed_and_enveloped.cert    { V8.68 support OpenSSL 3.0 structure }
        else
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error no signed type found in PKCS7 file ' + FileName);
        total := OPENSSL_sk_num(MyStack);   { don't free stack }
        if total = 0 then
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error no certificate found in PKCS7 file ' + FileName);
        SetX509(PX509_OBJECT(OPENSSL_sk_value(MyStack, 0))); // first is cert
        FreeAndNilX509Inters;
     // save others as CA
        if (IncludeInters > croNo) and (total >= 2) then begin
            FX509Inters := OPENSSL_sk_new_null;
            for I := 1 to total - 1 do
                OPENSSL_sk_insert(FX509Inters, PAnsiChar(X509_dup (PX509(OPENSSL_sk_value(MyStack, I)))), I-1);
        end;
        PKCS7_free(Pointer(p7));     { V8.66 }
    finally
        bio_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ check certificate file extension for content, returning error for unknown file extensions }
{ returns list of errors }
{ .PEM, .CER, .CRT, .0, .1, .2, etc - Base64 encoded DER  }
{ .DER, .CER, .CRT - binary DER }
{ .P7B, .P7R, .P7S, .SPC - PKCS#7 }
{ .PFX, .P12 - PKCS#12 }
{ note update SslCertFileOpenExts if more extensions added }
procedure TX509Base.LoadFromFileEx(const FileName: String; IncludePKey: TCertReadOpt;
                                        IncludeInters: TCertReadOpt; const Password: String; var Errs: String);   { V8.65 }
var
    fext: string;
    digit: boolean;
begin
    ClearAll;           { free anything we might update so no mismatches }
    digit := false;
    Errs := '';
    fext := IcsLowerCase(ExtractFileExt(FileName));
    if Length(fext) = 2 then digit := IsXDigit(fext [2]);    { assume .0, .1, .2 etc are PEM }
    if (fext = '.pfx') or  (fext = '.p12') then
        LoadFromP12File(FileName, IncludePKey, IncludeInters, Password)
    else if (fext = '.p7b') or (fext = '.p7r') or (fext = '.p7s') or (fext = '.spc') then
        LoadFromP7BFile(FileName, IncludeInters)
    else if (fext = '.pem') or (fext = '.der') or (fext = '.cer') or (fext = '.crt') or digit then
        LoadFromPemFileEx(FileName, IncludePKey, IncludeInters, Password, Errs)   // handles DER as well
    else
        Errs := 'Unknown certificate file extension';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ check certificate file extension for content, returning exception for unknown file extensions }
{ .PEM, .CER, .CRT, .0, .1, .2, etc - Base64 encoded DER  }
{ .DER, .CER, .CRT - binary DER }
{ .P7B, .P7R, .P7S, .SPC - PKCS#7 }
{ .PFX, .P12 - PKCS#12 }
procedure TX509Base.LoadFromFile(const FileName: String; IncludePKey: TCertReadOpt = croNo;
                                                    IncludeInters: TCertReadOpt = croNo; const Password: String = '');  { V8.40 }
var
    Errs: string;
begin
    LoadFromFileEx(FileName, IncludePKey, IncludeInters, Password, Errs);
    if Errs <> '' then
        raise EX509Exception.Create(Errs + ' - ' + FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.67 rewrite to use SaveToP12Buf }
procedure TX509Base.SaveToP12File(const FileName, Password: String; IncludeInters: Boolean = FALSE;
                                    PrivKeyType: TSslPrivKeyCipher = PrivKeyEncNone; IncludeKey: Boolean = TRUE);   { V8.40, V9.1 }
var
    FileBio: PBIO;
    P12Buf: AnsiString;
begin
    P12Buf := SaveToP12Buf(Password, IncludeInters, PrivKeyType, IncludeKey);
    if Length(P12Buf) = 0 then
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error writing PKCS12 certificate to ' + FileName);
    FileBio := IcsSslOpenFileBio(FileName, bomWriteBin);   { binary file }
    try
       if BIO_write(FileBio, @P12Buf [1], Length (P12Buf)) = 0 then
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error writing PKCS12 certificate to ' + FileName);
    finally
        bio_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.SaveToDERFile(const FileName: String);      { V8.40 }
var
    FileBio : PBIO;
begin
    if not Assigned(FX509) then
        raise EX509Exception.Create('Certificate not assigned');
    FileBio := IcsSslOpenFileBio(FileName, bomWriteBin);    { binary file }
    try
        if i2d_X509_bio(FileBio, FX509) = 0 then
            RaiseLastOpenSslError(EX509Exception, TRUE,
                           'Error writing DER certificate to ' + FileName);
    finally
        bio_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.SaveToP7BFile(const FileName: String; IncludeInters: Boolean = FALSE; Base64: Boolean = FALSE);  { V8.41 }
var
    FileBio : PBIO;
    p7  : PPKCS7;
 //   p7s : PPKCS7_SIGNED;
    Tot,I : Integer;
begin
    if not Assigned(FX509) then
        raise EX509Exception.Create('Certificate not assigned');
    if IncludeInters and not Assigned(FX509Inters) then
        raise EX509Exception.Create('Intermediate X509 not assigned');
    FileBio := IcsSslOpenFileBio(FileName, bomWriteBin);   { binary file }
    try
        p7 := Pointer(PKCS7_new);           { V8.66 }
        if NOT Assigned(p7)then
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error creating PKCS7 object');
(*        p7s := Pointer(PKCS7_SIGNED_new);     { V8.66 }
        if NOT Assigned(p7s)then
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error creating PKCS7 Signed object');
        p7.type_ := OBJ_nid2obj(NID_pkcs7_signed);  // set structure type
        p7.d.sign := p7s;       { V8.68 support OpenSSL 3.0 structure }
        p7s.contents.type_ := OBJ_nid2obj(NID_pkcs7_data);        { V8.41 }
        if ASN1_INTEGER_set(p7s.version, 1) = 0 then
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error setting version in PKCS7 file - ' + FileName);
 *)
        if PKCS7_set_type(@p7, NID_pkcs7_signed) = 0 then     { V9.1 }
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error setting PKCS7 type');
        if PKCS7_content_new(@p7, NID_pkcs7_data) = 0 then     { V9.1 }
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error setting PKCS7 content');

        if PKCS7_add_certificate(@p7, @FX509) = 0 then     { V8.66 }
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error adding certificate to PKCS7 file - ' + FileName);

     // write intermediate certificates
        if IncludeInters then begin
            Tot := OPENSSL_sk_num(FX509Inters);
            if Tot > 0 then begin
                for I := 0 to Pred (Tot) do begin
                    if PKCS7_add_certificate(@p7, PX509(OPENSSL_sk_value(@FX509Inters, I))) = 0 then      { V8.66 }
                        RaiseLastOpenSslError(EX509Exception, TRUE, 'Error adding certificate to PKCS7 file - ' + FileName);
                end;
            end;
        end;

     // write to file
        if NOT Base64 then begin
            if i2d_PKCS7_bio(FileBio, @P7) = 0 then // binary DER       { V8.66 }
                RaiseLastOpenSslError(EX509Exception, TRUE, 'Error writing PKCS7 binary certificate to ' + FileName);
        end
        else begin
            if PEM_write_bio_PKCS7(FileBio, @P7) = 0 then  // base64 PEM     { V8.66 }
                RaiseLastOpenSslError(EX509Exception, TRUE,  'Error writing PKCS7 base64 certificate to ' + FileName);
        end;
        PKCS7_free(@p7);
    finally
        bio_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.PrivateKeyLoadFromText(Lines: String; const Password: String = '');
var
    PKey: PEVP_PKEY;
    Err: String;
begin
    InitializeSsl;
    PKey := IcsSslLoadPkeyFromAStr(AnsiString(Lines), PasswordConvert(Password), Err);  { V8.65 }
    if not Assigned(PKey) then
        raise EX509Exception.Create('Can''t read private key lines: ' + Err);
    try
        if Assigned(FX509) and (X509_check_private_key(FX509, PKey) < 1 )then
            raise EX509Exception.Create('Certificate and private key do not match');
        SetPrivateKey(PKey);
    finally
        EVP_PKEY_free(PKey);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ check certificate file extension for content }
{ .PEM, .CER, .CRT - Base64 encoded DER  }
{ .DER - binary DER }
{ .P7B, .P7R, .P7S, .SPC - PKCS#7 }
{ .PFX, .P12 - PKCS#12 }
procedure TX509Base.SaveToFile(const FileName: String; IncludePrivateKey: Boolean = FALSE; AddInfoText: Boolean = False;
            IncludeInters: Boolean = FALSE; const Password: String = ''; PrivKeyType: TSslPrivKeyCipher = PrivKeyEncNone);   { V8.40 }
var
    fext: string;
begin
    fext := IcsLowerCase(ExtractFileExt(FileName));
    if (fext = '.pfx') or  (fext = '.p12') then
        SaveToP12File(FileName, Password, IncludeInters, PrivKeyType, IncludePrivateKey)   { V9.1 added key }
    else if (fext = '.p7b') or (fext = '.p7r') or (fext = '.p7s') or (fext = '.spc') then
        SaveToP7BFile(FileName, IncludeInters, false) { binary not base64 }
    else if (fext = '.der') then
        SaveToDERFile(FileName)
    else
        SaveToPemFile(FileName, IncludePrivateKey, AddInfoText,
                                            IncludeInters, Password, PrivKeyType);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSelfSigned: Boolean;
begin
    if Assigned(FX509) then
        Result := (X509_check_issued(FX509, FX509) = 0) or (IssuerCName = SubjectCName)
          { V8.63 double check, check_issued does not always work }
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.IssuedBy(ACert: TX509Base): Boolean;
begin
    if Assigned(ACert) and Assigned(ACert.X509) and Assigned(FX509) then
        Result := X509_check_issued(ACert.X509, FX509) = 0
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.IssuerOf(ACert: TX509Base): Boolean;
begin
    if Assigned(ACert) and Assigned(ACert.X509) and Assigned(FX509) then
        Result := X509_check_issued(FX509, ACert.X509) = 0
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.SameHash(const ACert: TX509Base): Boolean;
var
    I : Integer;
    P1, P2 : PInteger;
begin
    if (FX509 <> nil) and (ACert <> nil) and (ACert.X509 <> nil) then begin
        P1 := @ACert.Sha1Digest[0];
        P2 := @Sha1Digest[0];
        for I := 1 to 4 do begin
            if (P1^ <> P2^) then
                Break;
            Inc(P1);
            Inc(P2);
        end;
        if P1^ = P2^ then
            Result := TRUE
        else
            Result := FALSE
    end
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Returns a CRLF-separated list if multiple entries exist }
{ used for certs and requests }
function TX509Base.GetPX509NameByNid(XName: PX509_NAME; ANid: Integer): String;  { V8.41 }
var
    Entry   : PX509_NAME_ENTRY;
    Asn1    : PASN1_STRING;
    LastPos : Integer;
begin
    Result := '';
{$IFNDEF WIN64}
  {$IFNDEF COMPILER24_UP}
    Entry  := nil; { Make dcc32 happy }
  {$ENDIF}
{$ENDIF}
    if XName = nil then Exit;
    LastPos := -1;
    repeat
        LastPos := X509_NAME_get_index_by_NID(XName, ANid, LastPos);
        if LastPos > -1 then
            Entry := X509_NAME_get_entry(XName, LastPos)
        else
            Break;
        if Assigned(Entry) then begin
            Asn1 := X509_NAME_ENTRY_get_data(Entry);
      { V8.64 if domain has ACE xn--. convert it to Unicode, ignore errors }
            if Assigned(Asn1) then
                Result := Result + IcsIDNAToUnicode(Asn1ToString(Asn1)) + #13#10;
        end;
    until
        LastPos = -1;

    while (Length(Result) > 0) and (Word(Result[Length(Result)]) in [Ord(#13), Ord(#10)]) do
        SetLength(Result, Length(Result) - 1);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Returns a CRLF-separated list if multiple entries exist }
function TX509Base.GetNameEntryByNid(IsSubject: Boolean; ANid: Integer): String;
var
    Name    : PX509_NAME;
begin
    Result := '';
{$IFNDEF WIN64}
//    Entry  := nil; { Make dcc32 happy }
{$ENDIF}
    if not Assigned(X509) then
        Exit;
    if IsSubject then
        Name := X509_get_subject_name(X509)
    else
        Name := X509_get_issuer_name(X509);
    if Name <> nil then
        Result := GetPX509NameByNid(Name, ANid);   { V8.41 simplify }
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSubjectOName: String;
begin
    Result := GetNameEntryByNid(TRUE, NID_organizationName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSubjectOUName: String;
begin
    Result := GetNameEntryByNid(TRUE, NID_organizationalUnitName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSubjectCOName: String;
begin
    Result := GetNameEntryByNid(TRUE, NID_countryName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSubjectSTName: String;
begin
    Result := GetNameEntryByNid(TRUE, NID_stateOrProvinceName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSubjectLName: String;
begin
    Result := GetNameEntryByNid(TRUE, NID_localityName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSubjectEmailName: String;
begin
    Result := GetNameEntryByNid(TRUE, NID_pkcs9_emailAddress);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSubjectSerialName: String;
begin
    Result := GetNameEntryByNid(TRUE, NID_serialNumber);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetIssuerOName: String;
begin
    Result := GetNameEntryByNid(FALSE, NID_organizationName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetIssuerOUName: String;
begin
    Result := GetNameEntryByNid(FALSE, NID_organizationalUnitName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetIssuerCName: String;
begin
    Result := GetNameEntryByNid(FALSE, NID_commonName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSubAltNameDNS: String;
begin
    Result := GetExtensionValuesByName('subjectAltName', 'DNS');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetListAltNameDNS: TStringDynArray;                 { V9.5 }
begin
    if Length(FListAltNameDNS) = 0 then
        FListAltNameDNS := IcsLinesToDynArray(GetSubAltNameDNS);
    Result := FListAltNameDNS;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSubAltNameIP: String;
begin
    Result := GetExtensionValuesByName('subjectAltName', 'IP');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetListAltNameIP: TStringDynArray;                  { V9.5 }
begin
    if Length(FListAltNameIP) = 0 then
        FListAltNameIP := IcsLinesToDynArray(GetSubAltNameIP);
    Result := FListAltNameIP;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetKeyUsage: String;
begin
    Result := GetExtensionValuesByName('keyUsage', '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetExKeyUsage: String;
begin
    Result := GetExtensionValuesByName('extendedKeyUsage', '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetBasicConstraints: String;
begin
    Result := GetExtensionValuesByName('basicConstraints', '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetAuthorityInfoAccess: String;
begin
    Result := GetExtensionValuesByName('authorityInfoAccess', '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetAuthInfo(const ID: String): String;                     {V8.69 }
var
    AuthInfo: String;
    I: Integer;
begin
    Result := '';
    AuthInfo := IcsLowerCase(GetAuthorityInfoAccess);
// OCSP - URI=http://r3.o.lencr.org, CA Issuers - URI=http://r3.i.lencr.org/
    I := Pos(IcsLowerCase(ID), AuthInfo);
    if I = 0 then Exit;
    AuthInfo := Copy(AuthInfo, I + 4, 999);
    I := Pos(IcsCRLF, AuthInfo);
    if I > 0 then SetLength(AuthInfo, I - 1);
    I := Pos('uri=http', AuthInfo);
    if I = 0 then Exit;
    Result := Copy(AuthInfo, I + 4, 999);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetUrlOcsp: String;                     {V8.69 }
begin
    Result := GetAuthInfo('ocsp');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetUrlIssuer: String;                     {V8.69 }
begin
    Result := GetAuthInfo('issuers');  // might be 'CA Issuers' or 'caIssuers'
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetCertPolicies: String;            { V8.40 }
begin
    Result := GetExtensionValuesByName('certificatePolicies', '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetExtendedValidation: boolean;            { V8.40 }
var
    policy: String;
    I: Integer;
begin
    Result := False;
    policy := GetExtensionValuesByName('certificatePolicies', '');
    I :=  Pos('1.3.6.1.4.1.', policy);     { V8.41 need to check more carefully }
    if (I > 0) then begin
        if Length (policy) < (I+16) then Exit;
        policy := Copy(policy, I+12, 99);
        Result := (Pos('34697.2', policy) <> 0) or          // AffirmTrust
                  (Pos('6449.1.2.1.5.1', policy) <> 0) or   // Comodo
                  (Pos('14370.1.6', policy) <> 0) or        // Geotrust
                  (Pos('4146.1.1', policy) <> 0) or         // GlobalSign
                  (Pos('782.1.2.1.8.1', policy) <> 0) or    // Network Solutions
                  (Pos('22234.2.5.2.3.1', policy) <> 0) or  // OpenTrust
                  (Pos('23223.2', policy) <> 0) or          // Startcom
                  (Pos('6334.1.100.1', policy) <> 0) or     // Verizon
                  (Pos('36305.2', policy) <> 0);            // Wosign
        Exit;
    end;
    I :=  Pos('2.16.840.1.', policy);
    if (I > 0) then begin
        if Length (policy) < (I+16) then Exit;
        policy := Copy(policy, I+11, 99);
        Result := (Pos('114412.2.1', policy) <> 0) or          // Digicert
                  (Pos('114412.1.3.0.2', policy) <> 0) or      // Digicert
                  (Pos('114028.10.1.2', policy) <> 0) or       // Entrust
                  (Pos('114413.1.7.23.3', policy) <> 0) or     // GoDaddy
                  (Pos('114414.1.7.23.3', policy) <> 0) or     // Starfield
                  (Pos('113733.1.7.48.1', policy) <> 0) or     // Thawte
                  (Pos('114404.1.1.2.4.1', policy) <> 0) or    // Trustwave
                  (Pos('113733.1.7.23.6', policy) <> 0);       // Symantec/Verisign
        Exit;
    end;
    Result := (Pos('2.16.756.1.83.21.0', policy) <> 0) or      // SwissCom
              (Pos('2.16.756.80.1.1.1.1', policy) <> 0);       // SwissSign
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetAuthorityKeyId: String;           { V8.40 }
begin
    Result := GetExtensionValuesByName('authorityKeyIdentifier', '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSubjectKeyId: String;           { V8.40 }
begin
    Result := GetExtensionValuesByName('subjectKeyIdentifier', '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetCRLDistribution: String;           { V8.40 }
begin
    Result := GetExtensionValuesByName('crlDistributionPoints', '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetIssuerCOName: String;
begin
    Result := GetNameEntryByNid(TRUE, NID_countryName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetIssuerSTName: String;
begin
    Result := GetNameEntryByNid(TRUE, NID_stateOrProvinceName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetIssuerLName: String;
begin
    Result := GetNameEntryByNid(TRUE, NID_localityName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetIssuerEmailName: String;
begin
    Result := GetNameEntryByNid(TRUE, NID_pkcs9_emailAddress);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSignAlgo: String;     { V1.09 }
var
    Nid: integer ;
    Str : AnsiString;
//    MyX509: PX509;
begin
    Result := '';
    if not Assigned(X509) then
        Exit;
    { V8.27 need new export for 1.1.0, was in 1.0.2 }
    Nid := X509_get_signature_nid(X509);
    if Nid <> NID_undef then begin
        SetLength(Str, 256);
        Str := OBJ_nid2ln(Nid);
        SetLength(Str, IcsStrLen(PAnsiChar(Str)));     { V8.20 }
        Result := String(Str);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetKeyDesc(pkey: PEVP_PKEY): string;       { V8.40 }
var
    Bits, {Nid,} keytype, GLen: Integer;
    Str : AnsiString;
//  eckey: PEC_KEY;
//  ecgroup: PEC_GROUP;
begin
    result := '' ;
    if not Assigned(pkey) then
        Exit;
    keytype := IcsPkeyBaseid(pkey); { V8.41 key type V9.4 new name }
    Bits := IcsPkeyBits(pkey);   { V9.4 new name }
    if keytype = EVP_PKEY_RSA then begin
        Result := 'RSA Key Encryption';
    end
    else if keytype = EVP_PKEY_DSA then begin
        Result := 'DSA Key Encryption';
    end
    else if keytype = EVP_PKEY_DH then begin
        Result := 'DH Key Encryption';
    end
    else if keytype = EVP_PKEY_EC then begin
        Result := 'ECDSA Key Encryption';
      // EC has curves, not bits
    {   eckey := EVP_PKEY_get1_EC_KEY(pkey);
        if eckey = nil then Exit;
        ecgroup := EC_KEY_get0_group(eckey);
        if Assigned (ecgroup) then begin
            Nid := EC_GROUP_get_curve_name(ecgroup);
            Result := Result + ' ' + String(OBJ_nid2ln(Nid));
        end;
        EC_KEY_free(eckey);  }
        SetLength(Str, 33);
        if EVP_PKEY_get_group_name(PrivateKey, @Str[1], 32, @Glen) = 1 then begin  { V9.5 stop using deprecated functions }
            SetLength(Str, Glen);
            Result := Result + ' ' + String(Str);
        end;
    end
    else if keytype = EVP_PKEY_ED25519 then begin    { V8.51 }
        Result := 'ED25519 Key Encryption';
    end
    else if keytype = EVP_PKEY_X25519 then begin     { V8.52 }
        Result := 'X25519 Key Encryption';
    end
    else if keytype = EVP_PKEY_RSA_PSS then begin    { V8.51 }
        Result := 'RSA-PSS Key Encryption';
    end
    else begin   { lots of obscure key types }
        SetLength(Str, 256);
        Str := OBJ_nid2ln(keytype);   { V8.41 }
        SetLength(Str, IcsStrLen(PAnsiChar(Str)));
        Result := String(Str);   { V8.41 }
    end;
    if Bits > 0 then
        Result := Result + ' ' + IntToStr(Bits) + ' bits';
    Bits := IcsPkeySecurityBits(pkey);         { V9.4 }
    if Bits > 0 then
        Result := Result + ', ' + IntToStr(Bits) + ' security bits';  { V8.51 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetKeyInfo: string;       { V1.09 }
var
    pubkey: PEVP_PKEY;
begin
    result := '' ;
    if not Assigned(FX509) then Exit;
//  pubkey := X509_get_pubkey(FX509);
    pubkey := X509_get0_pubkey(FX509);   {  V8.65 fix memory leak }
    Result := GetKeyDesc(pubkey);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSerialNumTB: TBytes;   { V9.5 }
var
    serial: PASN1_INTEGER;
    serlen: Integer;
begin
    SetLength(Result, 0);
    if not Assigned(FX509) then
        Exit;
    serial := X509_get_serialNumber(FX509);
    serlen := ASN1_STRING_length(serial);
    SetLength(Result, serlen);
    Move(ASN1_STRING_get0_data(serial)[0], Result[0], serlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSerialNumHex: String;   { V1.09 }
var
//    serial: PASN1_INTEGER;
    serialTB: TBytes;
begin
    Result := '';
//    if not Assigned(FX509) then Exit;
//    serial := X509_get_serialNumber(FX509);
//    Result := IcsLowerCase(IcsBufferToHex(serial^.data [0], serial^.length)) ;  { V8.40 }
//    Result := IcsLowerCase(IcsBufferToHex(ASN1_STRING_get0_data(serial)[0], ASN1_STRING_length(serial)));  { V8.66}
    SerialTB := GetSerialNumTB;
    Result := IcsLowerCase(IcsTBToHex(SerialTB));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ provides several lines of certificate information }
function TX509Base.CertInfo(Brief: Boolean = False): String;   { V8.41 added Brief }
begin
    Result := '';
    if NOT IsCertLoaded then Exit;  { V8.57 sanity check }
    if SubjectCName = '' then
        Result := 'Issued to (CN): (Blank)'    { V8.63 }
    else
        Result := 'Issued to (CN): ' + IcsUnwrapNames (SubjectCName);
    if SubjectOName  <> '' then Result := Result + ', (O): '  + IcsUnwrapNames (SubjectOName);
    if SubjectOUName <> '' then Result := Result + ', (OU): ' + IcsUnwrapNames (SubjectOUName);  { V8.53 }
    Result := Result + #13#10;
    if SubAltNameDNS <> '' then
        Result := Result + 'Alt Domains (SAN): ' + IcsUnwrapNames (SubAltNameDNS) + #13#10;
    if SubAltNameIP <> '' then
        Result := Result + 'Alt IP: ' + IcsUnwrapNames (SubAltNameIP) + #13#10;   { V8.41 }
    if SelfSigned then
        Result := Result + 'Issuer: Self Signed' + #13#10
    else begin
        Result := Result + 'Issuer (CN): ' + IcsUnwrapNames (IssuerCName);
        if IssuerOName  <> '' then Result := Result + ', (O): '  + IcsUnwrapNames (IssuerOName);
        if IssuerOUName <> '' then Result := Result + ', (OU): ' + IcsUnwrapNames (IssuerOUName);   { V8.53 }
        Result := Result + #13#10;
    end;
    Result := Result + 'Expires: ' + RFC3339_DateToStr (ValidNotAfter) +    { V8.45 need expiry for brief, V8.61 added time, V8.65 ISO time }
                       ', Signature: ' + SignatureAlgorithm + #13#10;
    if NOT Brief then begin
        Result := Result + 'Valid From: ' + RFC3339_DateToStr (ValidNotBefore) +    { V8.61 added time, V8.65 ISO time }
            ', Serial Number: ' + GetSerialNumHex + #13#10 +     { V8.40 }
            'Fingerprint (sha256): ' + IcsLowerCase(Sha256Hex) + #13#10 +   { V8.41, V8.63 was Sha1 }
            'Public Key: ' + KeyInfo;                                       { V8.53 not brief }
        if ExtendedValidation then
            Result := Result + #13#10 + 'Extended Validation (EV) SSL Server Certificate';   { V8.40 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ provides a single line with the main certificate information }
function TX509Base.CertMainInfo: String;                                                     { V8.67 }
begin
    Result := '';
    if NOT IsCertLoaded then Exit;
    if SubjectCName = '' then
        Result := 'CN: (Blank)'
    else if SubAltNameDNS <> '' then
        Result := 'SAN: ' + IcsUnwrapNames(SubAltNameDNS)
    else
        Result := 'CN: ' + IcsUnwrapNames (SubjectCName);
    if SubjectOName <> '' then
        Result := Result + ', O: ' + IcsUnwrapNames(SubjectOName);
    if SubjectOUName <> '' then
        Result := Result + ', OU: ' + IcsUnwrapNames(SubjectOUName);
    if SelfSigned then
        Result := Result + ', Issuer: Self Signed'
    else if IssuerCName <> '' then
        Result := Result + ', Issuer: ' + IcsUnwrapNames (IssuerCName)
    else if IssuerOName  <> '' then
            Result := Result + ', Issuer: '  + IcsUnwrapNames (IssuerOName);
    Result := Result + ', Expires: ' + DateToStr(ValidNotAfter);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ compare supplied private key with ours }
function TX509Base.ComparePkey(ACert: TX509Base): Boolean;                                { V8.67 }
begin
    Result := False;
    if NOT Assigned(ACert) then Exit;
    if NOT Assigned(ACert.PrivateKey) then Exit;
    if NOT Assigned(FPrivateKey) then Exit;
//    Result := (EVP_PKEY_cmp(FPrivateKey, ACert.PrivateKey) = 1);   { V9.5 deprecated }
    Result := (EVP_PKEY_eq(FPrivateKey, ACert.PrivateKey) = 1);      { V9.5  3.0 and later }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ check if host matches certificate common name or alternate names, if so
  returns the matching name or blank for failed.  Flags are X509_CHECK_Flag_xx
  which may change how the check is performed }
function TX509Base.CheckHost(const Host: string; Flags: integer): String;   { V8.39 }
var
    peername: AnsiString;
    PunycodeHost: AnsiString;
begin
    Result := '';
    if not Assigned(FX509) then Exit;
    SetLength (peername, 512);
 { V8.64 needs A-Label punycode, not ANSI }
    PunycodeHost := AnsiString(IcsIDNAToASCII(IcsTrim(Host)));
    if X509_check_host (FX509, PAnsiChar(PunycodeHost),
                        Length(PunycodeHost), Flags, @peername[1]) <> 1 then exit;
    SetLength (peername, StrLen(PAnsiChar(peername)));
    Result := String (peername);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ check if email matches certificate common name or alternate names }
function TX509Base.CheckEmail(const Email: string; Flags: integer): Boolean;    { V8.39 }
begin
    Result := False;
    if not Assigned(FX509) then Exit;
    result := (X509_check_email (FX509, PAnsiChar(AnsiString(Email)), Length(Email), Flags) <> 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ check if IP address matches certificate common name or alternate names }
function TX509Base.CheckIPaddr(const IPadddr: string; Flags: integer): Boolean; { V8.39 }
begin
    Result := False;
    if not Assigned(FX509) then Exit;
    result := (X509_check_ip_asc (FX509, PAnsiChar(AnsiString(IPadddr)), Flags) <> 1);  { V8.64 corrected declaration }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetPrivateKeyInfo: string;                         { V8.40 }
begin
    result := '' ;
    if not Assigned(PrivateKey) then Exit;
    Result := GetKeyDesc(PrivateKey);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.LoadIntersFromPemFile(const FileName: String);         { V8.41 }
var
    MyStack: PStack;    { V8.65 }
begin
    InitializeSsl;
    MyStack := IcsSslLoadStackFromInfoFile(FileName, emCert);  { V8.65 fix memory leak, stack never freed }
    SetX509Inters(MyStack);
    OPENSSL_sk_free(MyStack);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.LoadIntersFromString(const Value: String);             { V8.41 }
var
    MyStack: PStack;    { V8.65 }
begin
    InitializeSsl;
    MyStack := IcsSslLoadStackFromInfoString(Value, emCert); { V8.65 fix memory leak, stack never freed }
    SetX509Inters(MyStack);
    OPENSSL_sk_free(MyStack);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ should pass UTF8 to this function }
procedure TX509Base.WriteStrBio(ABio: PBIO; Str: AnsiString; StripCR: Boolean = False);  { V8.41 }
begin
    if not Assigned(ABio) then
        raise EX509Exception.Create('BIO not assigned');
    if Length (Str) = 0 then Exit;
    if StripCR then      { OpenSSL assume LF only and adds CR for Windows, so remove them }
        Str := AnsiString(StringReplace (String(Str), #13, '', [rfReplaceAll]));
    if BIO_write(ABio, @Str [1], Length (Str)) = 0 then
        RaiseLastOpenSslError(EX509Exception, TRUE, 'Error writing text to BIO');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.WriteIntersToBio(ABio: PBIO; AddInfoText: Boolean = FALSE; const FName: String = '');  { V8.41 }
var
    Tot,I : Integer;
    Cert: TX509Base;
begin
    if not Assigned(ABio) then
        raise EX509Exception.Create('BIO not assigned');
    if not IsInterLoaded then
        raise EX509Exception.Create('X509Inters not assigned');
    Tot := GetInterCount;
    if Tot = 0 then Exit;
    Cert := TX509Base.Create (self);
    try
        for I := 0 to Pred (Tot) do begin
            Cert.X509 := PX509(OPENSSL_sk_value(FX509Inters, I));
         { V8.57 a self signed certificate is a root not an intermediate }
            if (Cert.IsCertLoaded) and (NOT Cert.SelfSigned) then begin
                if AddInfoText then
                    WriteStrBio(ABio, StringToUtf8(Cert.CertInfo) + #13#10, True);  { V8.65 UTF8 not ANSI }
                if PEM_write_bio_X509(ABio, Cert.X509) = 0 then
                    RaiseLastOpenSslError(EX509Exception, TRUE, 'Error writing certificate to ' + FName);
                WriteStrBio(ABio, #10#10);  { blank lines between certs }
            end;
        end;
    finally
        Cert.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.SaveIntersToToPemFile(const FileName: String;
                                            AddInfoText: Boolean = FALSE);    { V8.41 }
var
    FileBio : PBIO;
begin
    InitializeSsl;
    FileBio := IcsSslOpenFileBio(FileName, bomWrite);
    try
        WriteIntersToBio(FileBio, AddInfoText);
    finally
        bio_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.GetIntersList(CertList: TX509List);             { V8.41 }
begin
    if not Assigned(CertList) then Exit;
    CertList.Clear;
    if not IsInterLoaded then Exit;
    CertList.LoadAllStack(FX509Inters);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.ListInters: string;                               { V8.41 }
var
    Tot: Integer;
    Certs: TX509List;
begin
    Result := '';
    Tot := GetInterCount;
    if Tot = 0 then Exit;
    Certs := TX509List.Create (self);
    try
        Result := 'Total ' + IntToStr (Tot) + #13#10;
        Certs.LoadAllStack(FX509Inters);
        Result := Result + Certs.AllCertInfo(True);
    finally
        Certs.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.AddToInters(X509: Pointer);                       { V8.41 }
begin
    if NOT IsInterLoaded then begin
        FX509Inters := OPENSSL_sk_new_null;
    end;
    OPENSSL_sk_insert(FX509Inters, PAnsiChar(X509_dup(PX509(X509))), -1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ this function is designed to validate and report a server certificate chain
 before the server SSL context is initialised.  It checks a server certificate
 is loaded, any chained intermediate certificates and optionally a CA root
 bundle (available as literal sslRootCACertsBundle), and also certificate
 expiry dates.  If returns chainOK for no errors, chainWarning for non-fetal
 error and chainFail if the chain is broken or expired.  CertStr returns a
 reportable list of all certificates in the chain for logs, etc.  }
{ V8.41, V8.57 make ExpireDays configurable }
{ V8.64 pass X509CAList as parameter rather than loading it from FX509CATrust }
function TX509Base.ValidateCertChain(Host: String; X509CAList: TX509List;
                                    var CertStr, ErrStr: string; ExpireDays: Integer = 30): TChainResult;
var
    curUTC: TDateTime;
    InterList: TX509List;
    CertIssuer, NextIssuer, OUIssuer: string;
    CATotal, I: integer;

    function FindInter(const AName: string): Boolean;
    var
        J: Integer;
    begin
        Result := False;
        for J := 0 to InterList.Count - 1 do begin
            if (InterList[J].SubjectCName = AName) or
                   (InterList[J].SubjectOName = AName) then begin
                CertStr := CertStr + #13#10 + 'Intermediate: ' +  InterList[J].CertInfo(False);
                NextIssuer := InterList[J].IssuerCName;
                OUIssuer := InterList[J].IssuerOUName;  { V8.53 }
                if curUTC > InterList[J].ValidNotAfter then
                    ErrStr := 'SSL certificate has expired - ' +  InterList[J].SubjectCName
                else begin
                    if (curUTC + ExpireDays) > InterList[J].ValidNotAfter then begin
                        ErrStr := 'SSL certificate expires on ' + DateToStr(InterList[J].ValidNotAfter) + ' - ' + InterList[J].SubjectCName;;
                    end;
                end;
                Result := True;
                Exit;
            end;
        end;
    end;

    function FindCA(const AName, OUName: string): Boolean;
    var
        J: Integer;
    begin
        Result := False;
        if (CATotal = 0) then Exit;
        for J := 0 to CATotal - 1 do begin
            if ((X509CAList[J].SubjectCName = AName) or
                  (X509CAList[J].SubjectOName = AName)) then begin
              { V8.53 also check Organisation Name, if used }
                if (OUName = '') or (X509CAList[J].SubjectOUName = OUName) then begin
                    CertStr := CertStr + #13#10 + 'Trusted CA: ' + X509CAList[J].CertInfo(False);
                    Result := True;
                    Exit;
                end;
            end;
        end;
    end;

begin
    Result := chainFail;
    InterList := Nil;
    try // finally
    try // except
        CertStr := '';
        ErrStr := '';
        curUTC := IcsGetUTCTime;   { V8.61 certificates have UTC time }
        if NOT IsCertLoaded then begin
            ErrStr := 'No SSL certificate loaded';
            Exit;
        end;

     { keep server cert details }
        CertStr := 'Server: ' + CertInfo(False);

     { check not expired }
        if curUTC < ValidNotBefore then begin
            ErrStr := 'SSL certificate not valid yet - ' + SubjectCName;
            Exit;
        end;
        if curUTC > ValidNotAfter then begin
            ErrStr := 'SSL certificate has expired - ' + SubjectCName;
            Exit;
        end;
        if ((curUTC + ExpireDays) > ValidNotAfter) then begin
            Result := chainWarn; // not fatal
            ErrStr := 'SSL certificate expires on ' + DateToStr(ValidNotAfter) + ' - ' + SubjectCName;
        end;

      { check host is listed - optional, may not be using SNI }
      { WARNING - Host may have several lines, should check each one }
        if (Host <> '') and NOT PostConnectionCheck (Host) then begin
            Result := chainWarn;
            ErrStr := 'SSL certificate expected host name not found: ' + Host + ', certificate DNS: ';
            if (SubAltNameDNS <> '') then         { V8.47 sometimes blank }
                ErrStr := ErrStr + IcsUnwrapNames(SubAltNameDNS)
            else
                ErrStr := ErrStr + SubjectCName;
        end;

     { self signed means nothing to check }
        if SelfSigned then begin
            Result := chainWarn;
            ErrStr := 'SSL certificate is self signed - ' + SubjectCName;;
            Exit;
        end;

     { build lists of inter and CA }
        if IsInterLoaded then begin
            InterList := TX509List.Create(self);
            InterList.LoadAllStack(FX509Inters);
        end;

     { V8.64 now using a passed shared list of CA root certificates }
        CATotal := 0;
        if Assigned(X509CAList) then
            CATotal := X509CAList.Count;

      { check inter chain or CA contains certificate that signed ours  }
        CertIssuer := IssuerCName;
        OUIssuer := IssuerOUName;  { V8.53 }
        NextIssuer := '';
        if IsInterLoaded and (NOT Selfsigned) then begin

        { keep inter cert details }
            CertStr := CertStr + #13#10;

         { check server certificate was issued by our iutermediates }
            if FindInter(CertIssuer) then
                CertIssuer := ''; { OK don't check again }

        { now check for multiple intermediates }
            if (InterList.Count > 1) and (NextIssuer <> '') then begin
             { V8.67 intermediate may be signed by CA root, and by another intermediate }
                if FindCA(NextIssuer, OUIssuer) then begin
                    if Result = chainFail then Result := chainOK;
                    CertStr := CertStr + #13#10 + 'Intermediate in Trusted Store, Multiple Verify Paths!';
                end;
                for I := 0 to InterList.Count - 1 do begin
                    if NOT FindInter(NextIssuer) then begin
                        if (CATotal = 0) then begin
                            CertStr := CertStr + #13#10 + 'Intermediates, Total ' + IntToStr (InterList.Count) + #13#10 +
                                        InterList.AllCertInfo(False, false);
                            Result := chainWarn;
                            ErrStr := 'Issuer for SSL certificate not found - ' + InterList[I].IssuerOName;
                            Exit;
                        end;
                    end;
                end ;
            end;
        end ;

    { see if server signed directly by trusted CA }
        if CertIssuer <> '' then begin
            if FindCA(CertIssuer, OUIssuer) then begin
                if Result = chainFail then Result := chainOK;  // no warnings so OK  V8.47
                Exit;
            end;
        end;

   { see if intermediate signed by a trusted CA }
        if (NextIssuer <> '') then begin
            if FindCA(NextIssuer, OUIssuer) then begin
                if Result = chainFail then Result := chainOK;  // no warnings so OK  V8.47
                Exit;
            end;
        end;
        if CertIssuer <> '' then begin
            Result := chainWarn;
            ErrStr := 'Issuer ' + CertIssuer + ' not found for SSL certificate - ' + SubjectCName;
        end
        else if (NextIssuer <> '') then begin
            if NextIssuer <> 'DST Root CA X3' then begin  { V8.68 ignore expired Let's Encrypt root }
                Result := chainWarn;
                ErrStr := 'Issuer for SSL certificate not found - ' + NextIssuer;
            end;
        end;
        if Result = chainFail then Result := chainOK;  // no warnings so OK
    except
        on E: Exception do begin   { V8.64 more error handling }
           ErrStr := 'Failed to check certificate chain: ' + E.Classname + ' ' + E.Message;
        end;
    end;
    finally
        if Assigned(InterList) then InterList.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TX509List }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TX509List.Create(AOwner: TComponent; AOwnsObjects: Boolean = TRUE);
begin
    inherited Create;
    FOwner            := AOwner;
    FX509Class        := TX509Base;
    FList             := TComponentList.Create(AOwnsObjects);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TX509List.Destroy;
begin
    try          { V8.71 JK }
        if Assigned(FX509Store) then              { V8.69 }
            X509_STORE_free(FX509Store);
        FList.Free;
    finally
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509List.Clear;
begin
    FList.Clear;
    if Assigned(FX509Store) then begin        { V8.69 }
        X509_STORE_free(FX509Store);
        FX509Store := Nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509List.Delete(const Index: Integer);
begin
    FList.Delete(Index);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.Remove(Item: TX509Base): Integer;
begin
    Result := FList.Remove(Item);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.Extract(Item: TX509Base): TX509Base;
begin
    Result := TX509Base(FList.Extract(Item));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.GetByHash(const Sha1Hash: AnsiString): TX509Base;
{ * Deprecated use Find * }
var
    I, J : Integer;
    P1, P2 : PInteger;
begin
    if Length(Sha1Hash) = 20 then begin
        for I := 0 to FList.Count -1 do begin
            Result := TX509Base(FList[I]);
            if Assigned(Result) and Assigned(Result.X509) then begin
                P1 := @Result.Sha1Digest[0];
                P2 := Pointer(Sha1Hash);
                for J := 1 to 4 do begin
                    if (P1^ <> P2^) then
                        Break;
                    Inc(P1);
                    Inc(P2);
                end;
                if P1^ = P2^ then
                    Exit;
            end;
        end;
    end;
    Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.FindDigest(const ASha1Digest: THashBytes20): TX509Base;   { V8.66 was Find but ambiguous }
var
    I, J : Integer;
    P1, P2 : PInteger;
begin
    if Length(ASha1Digest) = 20 then begin
        for I := 0 to FList.Count -1 do begin
            Result := TX509Base(FList[I]);
            if Assigned(Result) and Assigned(Result.X509) then begin
                P1 := @Result.Sha1Digest[0];
                P2 := @ASha1Digest[0];
                for J := 1 to 4 do begin
                    if (P1^ <> P2^) then
                        Break;
                    Inc(P1);
                    Inc(P2);
                end;
                if P1^ = P2^ then
                    Exit;
            end;
        end;
    end;
    Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.Find(const ASha1Hex: String): TX509Base;
var
    Digest: THashBytes20;
    I, J: Integer;
begin
    Result := nil;
    if Length(ASha1Hex) <> 40 then
        Exit;
    SetLength(Digest, 20);
    J := 0;
    for I := 1 to 39 do begin
        if Odd(I) then begin
            if (not IsXDigit(ASha1Hex[I])) or  (not IsXDigit(ASha1Hex[I + 1])) then
                Exit;
            Digest[J] := XDigit2(PChar(@ASha1Hex[I]));
            Inc(J);
        end;
    end;
    Result := FindDigest(Digest);   { V8.66 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.Find(const AX509: PX509): TX509Base;
var
    Len  : Integer;
    Digest : THashBytes20;
begin
    if Assigned(AX509) then begin
        Len := 20;
        SetLength(Digest, Len);
        if X509_digest(AX509, EVP_sha1, @Digest[0], @Len) <> 0 then
            Result := FindDigest(Digest)    { V8.66 }
        else
            Result := nil;
    end
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.FindSubject(const SubjOneLine: String): TX509Base;  { V8.69 }
var
    I: Integer;
begin
    Result := nil;
    I := IndexOfSubj(SubjOneLine);
    if I >= 0 then
        Result := TX509Base(FList[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.GetCount: Integer;
begin
    Result := FList.Count;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.GetX509Base(Index: Integer): TX509Base;
begin
    Result := TX509Base(FList[Index]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.IndexOf(const X509Base: TX509Base): Integer;
begin
    Result := FList.IndexOf(X509Base);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.IndexOfSubj(const SubjOneLine: String): Integer;      { V8.69 }
begin
    Result := -1;
    if Length(SubjOneLine) < 1 then Exit;
    for Result := 0 to FList.Count - 1 do begin
       if TX509Base(FList[Result]).SubjectOneLine = SubjOneLine then
            Exit;
    end;
    Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509List.SetX509Base(Index: Integer; Value: TX509Base);
begin
    Assert(Value is FX509Class);
    FList[Index] := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509List.SetX509Class(const Value: TX509Class);
begin
    if Value <> FX509Class then begin
        Assert(GetCount = 0, 'The X509 class should only be set when the list is empty');
        FX509Class := Value;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.GetOwnsObjects: Boolean;
begin
    Result := FList.OwnsObjects;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509List.SetOwnsObjects(const Value: Boolean);
begin
    FList.OwnsObjects := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.Add(X509: PX509 = nil): TX509Base;
begin
    Result := FX509Class.Create(FOwner, X509);
    FList.Add(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.AddItem(AItem: TX509Base): Integer;
begin
    Assert(AItem is FX509Class);
    Result := FList.Add(AItem);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.Insert(Index: Integer; X509: PX509 = nil): TX509Base;
begin
    Result := FX509Class.Create(FOwner, X509);
    try
        FList.Insert(Index, Result);
    except
        Result.Free;
        raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509List.InsertItem(Index: Integer; AItem: TX509Base);
begin
    Assert(AItem is FX509Class);
    FList.Insert(Index, AItem);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509List.SortChain(ASortOrder: TX509ListSort);
var
    I      : Integer;
    Cur    : Integer;
    x1, x2 : PX509;
begin
    Cur := 0;
    while Cur < FList.Count do begin
        x1 := TX509Base(FList[Cur]).X509;
        for I := 0 to FList.Count - 1 do begin
            x2 := TX509Base(FList[I]).X509;
            if X509_check_issued(x1, x2) = 0 then begin
                if ASortOrder = xsrtIssuerFirst then begin
                    if Cur + 1 <> I then
                        FList.Move(Cur, I);
                end
                else if Cur - 1 <> I then
                    FList.Move(I, Cur);
                Break;
            end
            else if X509_check_issued(x2, x1) = 0 then begin
                if ASortOrder = xsrtIssuedFirst then begin
                    if Cur + 1 <> I then
                        FList.Move(Cur, I);
                end
                else if Cur - 1 <> I then
                    FList.Move(I, Cur);
                Break;
            end;
            if I = FList.Count - 1 then begin
                { Current is neither issuer nor issued by a cert in chain }
                { This should not happen in a valid certificate chain.    }
                if ASortOrder = xsrtIssuedFirst then
                    FList.Move(Cur, 0)
                else
                    FList.Move(Cur, FList.Count - 1);
            end;
        end;
        Inc(Cur);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.41 load all certificates from an X509 stack }
function TX509List.LoadAllStack(CertStack: PStack): integer;
var
    I: integer;
    P: PAnsiChar;
begin
    Result := 0;
    if NOT Assigned (CertStack) then
        Exit;
    Result := OPENSSL_sk_num(CertStack);   { don't free stack }
    if Result = 0 then Exit;
    for I := 0 to Result - 1 do begin
        P := OPENSSL_sk_value(CertStack, I);
        if Assigned(P) then  { V8.64 sanity test }
            Add(PX509(P));       { V8.65 don't need dup, causes memory leask }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.1 load all PEM certificates from TBytes returning list of errors }
function TX509List.LoadAllFromPemTB(const Certs: TBytes; var Errs: String): integer;          { V9.1 }
var
    MyStack: PStack;
begin
    MyStack := IcsSslLoadStackFromPemTB(Certs, Errs, emCert);
    try
        Result := LoadAllStack(MyStack);
    finally
       OPENSSL_sk_pop_free(MyStack, @X509_free);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.1 load all PEM certificates from a file returning list of errors }
function TX509List.LoadAllFromPemFile(const Filename: string; var Errs: String): integer;     { V9.1 }
var
    Certs: TBytes;
begin
    Result := 0;
    Certs := IcsDataLoadFile(FileName);
    if Length(Certs) = 0  then begin
        Errs := 'Failed to Open File: ' + FileName;
        Exit;
    end;
    Result := LoadAllFromPemTB(Certs, Errs)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.LoadAllFromP12TB(const Certs: TBytes; var Errs: String): Integer;                            { V9.1 }
var
    MyStack: PStack;
begin
    MyStack := IcsSslLoadStackFromP12TB(Certs, Errs);
    try
        Result := LoadAllStack(MyStack);
    finally
       OPENSSL_sk_pop_free(MyStack, @X509_free);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.LoadAllFromP12File(const Filename: string; var Errs: String): Integer;                       { V9.1 }
var
    Certs: TBytes;
begin
    Result := 0;
    Certs := IcsDataLoadFile(FileName);
    if Length(Certs) = 0  then begin
        Errs := 'Failed to Open File: ' + FileName;
        Exit;
    end;
    Result := LoadAllFromP12TB(Certs, Errs);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.1 load all certificates from a TBytes returning list of errors }
{ supports PEM and PKCS12 bundles }
function TX509List.LoadAllFromTB(const Certs: TBytes; var Errs: String): integer;  { V9.1 }
begin
    if IcsTBytesPos(PEM_STRING_HDR_BEGIN, Certs, 0, 999) >= 0 then
        Result := LoadAllFromPemTB(Certs, Errs)
    else
        Result := LoadAllFromP12TB(Certs, Errs);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.65 load all certificates from a string returning list of errors }
{ supports PEM bundle, PKCS12 will fail due to string needing binary data }
function TX509List.LoadAllFromStringEx(const Value: String; var Errs: String): integer;  { V8.65 }
var
    Certs: TBytes;
begin
    IcsMoveStringToTBytes(Value, Certs, 0);
    Result := LoadAllFromTB(Certs, Errs);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 load all certificates from a file returning list of errors }
{ supports PEM and PKCS12 bundles }
function TX509List.LoadAllFromFileEx(const Filename: string; var Errs: String): integer; { V8.65 }
var
    Fext: string;
begin
    Errs := '';
    Result := 0;
    fext := IcsLowerCase(ExtractFileExt(FileName));
    if (fext = '.pfx') or  (fext = '.p12') then
        Result := LoadAllFromP12File(FileName, Errs)
    else if (fext = '.pem') or (fext = '.cer') then
        Result := LoadAllFromPemFile(FileName, Errs)
    else
        Errs := 'Unknown bundle file extension';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.39 load all certificates from a file }
{ supports PEM and PKCS12 bundles }
function TX509List.LoadAllFromFile(const Filename: string): integer;
var
    Errs: String;
begin
    Result := LoadAllFromFileEx(Filename, Errs);
    if Errs <> '' then
        raise ESslTX509Exception.Create('Reading file of PEM certificates: ' +  Errs + ' - ' + FileName);   { V8.65 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 load all certificates from a string }
{ supports PEM bundle, PKCS12 will fail due to uncode string and binary data }
function TX509List.LoadAllFromString(const Value: string): integer;
var
    Errs: String;
begin
    Result := LoadAllFromStringEx(Value, Errs);
    if Errs <> '' then
        raise ESslTX509Exception.Create('Reading list of PEM certificates: ' + Errs);   { V8.65 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.SaveAllToP12TB: TBytes;                                                      { V9.1 }
var
    ABio : PBIO;
    P12 : PPKCS12;
    Title: AnsiString;
begin
    SetLength(Result, 0);
    if Self.Count =- 0 then
        Exit;
    ABio := BIO_new(BIO_s_mem);
    try
        Title := 'Certificate Bundle';
    // blank certificate and key, only intermediate bundle, no encryption
        P12 := PKCS12_create(Nil, PAnsiChar(Title), Nil, Nil, Self.SaveToStack, -1, -1, 0, -1, 0);
        if not Assigned(P12) then
            raise ESslTX509Exception.Create('Error creating PKCS12 bundle - ' + String(LastOpenSslErrMsg(False)));
        if i2d_PKCS12_bio(ABio, P12) = 0 then
            raise ESslTX509Exception.Create('Error creating PKCS12 BIO - ' + String(LastOpenSslErrMsg(False)));
        Result := IcsReadTBBio(ABio, 2000000);
        PKCS12_free(P12);
    finally
        bio_free(ABio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.SaveAllToP12File(const FileName: String): Integer;                     { V9.1 }
var
    P12TB: TBytes;
begin
    Result := 0;
    P12TB := SaveAllToP12TB;
    if Length(P12TB) = 0 then
        Exit;
    if NOT IcsDataSaveFile(P12TB, FileName) then
        raise ESslTX509Exception.Create('Error creating PKCS12 File - ' + FileName)
    else
        Result := Self.Count;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.41 list all certificates, forwards or backwards (client handshake) }
function TX509List.AllCertInfo(Brief: Boolean=False; Reverse: Boolean=False): String;   { V8.41 }
var
    I, J: Integer;
begin
    Result := '';
    if FList.Count = 0 then Exit;
    if Reverse then
        J := FList.Count - 1
    else
        J := 0;
    for I := 0 to FList.Count - 1 do begin
         if I > 0 then Result := Result + #13#10{#13#10};    { V8.69 one line gap }
         Result := Result + '#' + IntToStr(J + 1) + ' ' + TX509Base(FList[J]).CertInfo(True);
         if Reverse then
            J := J - 1
         else
            J := J + 1;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.1 list all certificates, single line each }
function TX509List.ListCerts: String;                                            { V9.1 }
var
    I: Integer;
    Num: String;
begin
    Result := '';
    if FList.Count = 0 then
        Exit;
    for I := 1 to FList.Count do begin
        Num := IntToStr (I);
        while Length(Num) < 3 do
            Num := '0' + Num;
        Result := Result + '#' + Num + ' ';
        with TX509Base(FList [I-1]) do begin
            if SubAltNameDNS <> '' then
                Result := Result + IcsUnwrapNames(SubAltNameDNS)
            else if SubjectCName <> '' then
                Result := Result + IcsUnwrapNames(SubjectCName)
            else
                Result := Result + IcsUnwrapNames(SubjectOName);
            Result := Result + ' (' + IcsUnwrapNames(SubjectOName) + ')';
        //    if SubjectOUName <> '' then
        //        Result := Result + ' OU: ' + IcsUnwrapNames(SubjectOUName);
            if NOT SelfSigned then begin
                if IssuerCName <> '' then
                    Result := Result + ', Issuer: ' + IcsUnwrapNames (IssuerCName)
                 else
                    Result := Result + ', Issuers:: '  + IcsUnwrapNames (IssuerOName);
            end;
            Result := Result + ', Expires: ' + FormatDateTime(ISODateMask, ValidNotAfter);
            Result := Result + ', Fingerprint: ' + IcsLowerCase(Sha256Hex) + IcsCRLF;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.SaveToStack: PStack;                                                  { V8.69 }
var
    I: Integer;
begin
    Result := Nil;
    if FList.Count = 0 then Exit;
    Result := OPENSSL_sk_new_null;
    for I := 0 to FList.Count - 1 do begin
        OPENSSL_sk_insert(Result, TX509Base(FList[I]).FX509, I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.SetX509Store: Boolean;                                                  { V8.69 }
var
    I: Integer;
begin
    Result := False;
    if Assigned(FX509Store) then
        X509_STORE_free(FX509Store);
    FX509Store := Nil;
    if FList.Count = 0 then Exit;
    FX509Store := X509_STORE_new;
    for I := 0 to FList.Count - 1 do begin
        X509_STORE_add_cert(FX509Store, TX509Base(FList[I]).FX509);
    end;
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.BuildOCSPCertId(Cert: TX509Base): AnsiString;   { V8.69 }
var
    OcspCertID: POCSP_CERTID;
    RawPtr: PAnsiChar;
    I, RawLen: Integer;
begin
    Result := '';
    FOcspRespStatus := -1;
    FOcspCertStatus := -1;
    FOcspCertReason := -1;
    FOcspRevokeDT := 0;
    FOcspUpdateDT := 0;
    FOcspNextUpdDT := 0;
    if NOT Assigned(Cert.X509) then Exit;
    I := IndexOfSubj(Cert.IssuerOneLine);
    if I < 0 then Exit;
 // !! currently only using one Intermediate, should search for subjectoneline
    OcspCertID := OCSP_cert_to_id(EVP_sha1, Cert.X509, TX509Base(FList[I]).X509);
    if Assigned(OcspCertID) then begin
        RawLen := i2d_OCSP_CERTID(OcspCertID, Nil);
        if RawLen <= 0 then Exit;
        SetLength(Result, RawLen + 1);
        RawPtr := Pointer(Result);
        RawLen := i2d_OCSP_CERTID(OcspCertID, @RawPtr);   // rawptr is updated to end of buffer!
        SetLength(Result, RawLen);
        OCSP_CERTID_free(OcspCertId);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.OCSPReqInfo(const ReqRaw: AnsiString): String;            { V8.69 }
var
    OcspRequest: POCSP_REQUEST;
    MemBio: PBIO;
    RawPtr: PAnsiChar;
    RawLen: Integer;
begin
    Result := '';
    RawLen := Length(ReqRaw);
    if RawLen = 0 then Exit;
    try
   // convert DER binary OCSP request into internal structure
   // beware badly formatted DER raise exception
        RawPtr := Pointer(ReqRaw);
        OcspRequest := d2i_OCSP_REQUEST(Nil, @RawPtr, RawLen);
        if Assigned(OcspRequest) then begin
            MemBio := BIO_new(BIO_s_mem);
            if Assigned(MemBio) then begin
                OCSP_REQUEST_print(MemBio, OcspRequest, 0);
                Result := String(IcsReadStrBio(MemBIO, 0));
                bio_free(MemBio);
            end;
            OCSP_REQUEST_free(OcspRequest);
        end;
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.OCSPRespInfo(const RespRaw: AnsiString): String;        { V8.69 }
var
    OcspResponse: POCSP_RESPONSE;
    MemBio: PBIO;
    RawPtr: PAnsiChar;
    RawLen: Integer;
begin
    Result := '';
    RawLen := Length(RespRaw);
    if RawLen = 0 then Exit;
    try
   // convert DER binary OCSP response into internal structure
   // beware badly formatted DER raise exception
        RawPtr := Pointer(RespRaw);
        OcspResponse := d2i_OCSP_RESPONSE(Nil, @RawPtr, RawLen);
        if Assigned(OcspResponse) then begin
            MemBio := BIO_new(BIO_s_mem);
            if Assigned(MemBio) then begin
                OCSP_RESPONSE_print(MemBio, OcspResponse, 0);
                Result := String(IcsReadStrBio(MemBIO, 0));
                bio_free(MemBio);
            end;
            OCSP_RESPONSE_free(OcspResponse);
        end;
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ build OCSP request for certificate }
function TX509List.BuildOCSPReq(const IdRaw: AnsiString): AnsiString; { V8.69 }
var
    OcspCertId: POCSP_CERTID;
    OcspRequest: POCSP_REQUEST;
    OcspOneReq: POCSP_ONEREQ;
    RawPtr: PAnsiChar;
    RawLen: Integer;
begin
    Result := '';
    if IdRaw = '' then Exit;

 // convert DER binary OCSP CertID into internal structure
    RawPtr := Pointer(IdRaw);
    OcspCertId := d2i_OCSP_CERTID(Nil, @RawPtr, Length(IdRaw));
    if NOT Assigned(OcspCertId) then Exit;

 // build request, could add several certs
    OcspRequest := OCSP_REQUEST_new;
    OcspOneReq := OCSP_request_add0_id(OcspRequest, OcspCertId);  // main certificate, do not free OcspCertId
    if Assigned(OcspOneReq) then begin
       // could add intermediate ??
       // OCSP_request_add1_nonce  - add nonce, probably not since mostly ignored and we want to cache request
        RawLen := i2d_OCSP_REQUEST(OcspRequest, Nil);
        SetLength(Result, RawLen + 1);
        RawPtr := Pointer(Result);
        RawLen := i2d_OCSP_REQUEST(OcspRequest, @RawPtr);   // rawptr is updated to end of buffer!
        SetLength(Result, RawLen);
//        OCSP_ONEREQ_free(OcspOneReq);   // causes crash with request_free
    end;
    OCSP_REQUEST_free(OcspRequest);
 //   OCSP_CERTID_free(OcspCertId);       // cause crash, freed in request_free?
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.GetOcspCertReasonStr: String;                                 { V8.69 }
begin
    if FOcspCertReason < 0 then
        Result := ''
    else
        Result := String(OCSP_crl_reason_str(FOcspCertReason));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.GetOcspRespStatusStr: String;                                 { V8.69 }
begin
    if FOcspRespStatus < 0 then
        Result := ''
    else
        Result := String(OCSP_response_status_str(FOcspRespStatus));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.GetOcspCertStatusStr: String;                                  { V8.69 }
begin
    if FOcspCertStatus = -2 then
        Result := String(LastOpenSslErrMsg(False))
    else if FOcspCertStatus < 0 then
        Result := ''
    else
        Result := String(OCSP_cert_status_str(FOcspCertStatus));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ check OCSP response against certificate, sets OcspCertStatus, OcspRespStatus and OcspCertReason if result non-nil }
{ assumes the issuer intermediate is in this list }
function TX509List.CheckOCSPResp(const IdRaw, RespRaw: AnsiString; X509Store: PX509_STORE): Integer;  { V8.69 }
var
    OcspCertId: POCSP_CERTID;
    OcspResponse: POCSP_RESPONSE;
    OcspBasicResp: POCSP_BASICRESP;
    revtime, thisupd, nextupd: PASN1_GENERALIZEDTIME;
    RawPtr: PAnsiChar;
    RawLen: Integer;
    VFlags: LongWord;
    CertStack: PStack;
begin
    Result := -1;
    FOcspCertStatus := -1;
    FOcspRespStatus := -1;
    FOcspCertReason := -1;

 // no raw response, nothing to do
    RawLen := Length(RespRaw);
    if RawLen = 0 then Exit;
    if IdRaw = '' then Exit;

    try
     // convert DER binary OCSP CertID into internal structure
        RawPtr := Pointer(IdRaw);
        OcspCertId := d2i_OCSP_CERTID(Nil, @RawPtr, Length(IdRaw));
        if Assigned(OcspCertId) then begin

         // convert DER binary OCSP response into internal structure
         // beware badly formatted DER raise exception
            RawPtr := Pointer(RespRaw);
            OcspResponse := d2i_OCSP_RESPONSE(Nil, @RawPtr, RawLen);
            if Assigned(OcspResponse) then begin
                FOcspRespStatus := OCSP_response_status(OcspResponse);
                if FOcspRespStatus = OCSP_RESPONSE_STATUS_SUCCESSFUL then begin

                 // get BasicResponse, locate main certificate and get status
                    OcspBasicResp := OCSP_response_get1_basic(OcspResponse);
                    if Assigned(OcspBasicResp) then begin

                    // check basic response matches our certificate, get the cert status and dates about response
                        revtime := Nil;
                        thisupd := Nil;
                        nextupd := Nil;
                        if OCSP_resp_find_status(OcspBasicResp, OcspCertId,  @FOcspCertStatus, @FOcspCertReason, @revtime, @thisupd, @nextupd) = 1 then begin
                            if FOcspCertStatus = V_OCSP_CERTSTATUS_REVOKED then
                                Asn1ToUTDateTime(revtime, FOcspRevokeDT);
                            Asn1ToUTDateTime(thisupd, FOcspUpdateDT);
                            Asn1ToUTDateTime(nextupd, FOcspNextUpdDT);
                        //     ASN1_OBJECT_free(revtime);
                        //    ASN1_item_free(thisupd, x);
                        //    ASN1_item_free(nextupd);

                        // many issuers sign the response with the same intermediate as the certificate, so we have it already
                        // some delegate signing and include a certificate in the response, Microsoft
                            VFlags := 0; // OCSP_TRUSTOTHER;

                        // check response is signed correctly, needs stack of intermediates and X509_STORE with CA certs
                        // no store or issuer, skip verify
                            if (Self.GetCount > 0) then begin
                                if Assigned(X509Store) then begin
                                    CertStack := Self.SaveToStack;
                                    if OCSP_basic_verify(OcspBasicResp, CertStack, X509Store, VFlags) <> 1 then begin
                                       FOcspCertStatus := -2;   // get OpenSSL errors in GetOcspCertStatusStr
                                    end;
                                    OPENSSL_sk_free(CertStack);
                                end;
                            end;
                        end
                        else begin
                             // did not find CertId in response
                        end;
                        OCSP_BASICRESP_free(OcspBasicResp);
                    end;
                end;
                OCSP_RESPONSE_free(OcspResponse);
            end;
            OCSP_CERTID_free(OcspCertId);
        end;
        Result := FOcspRespStatus;
    except
        Result := -1;
        FOcspCertStatus := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ SSL functions used to read and write certificates }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ build AnsiString from an OpenSSL BIO, may be ANSI, UTF8 or binary }
function IcsReadStrBio(ABio: PBIO; MaxLen: Integer): AnsiString;                   { V8.69 }
var
    Len: integer;
begin
    Result := '';
    if not Assigned(ABio) then Exit;
    if MaxLen <= 0 then
        MaxLen := BIO_ctrl(ABio, BIO_CTRL_PENDING_, 0, nil);
    if MaxLen < 0 then Exit;
    SetLength(Result, MaxLen);
    Len := Bio_read(ABio, PAnsiChar(Result), MaxLen);
    SetLength(Result, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ build TBytes from an OpenSSL BIO }
function IcsReadTBBio(ABio: PBIO; MaxLen: Integer): TBytes;                   { V9.1 }
var
    Len: integer;
begin
    SetLength(Result, 0);
    if not Assigned(ABio) then Exit;
    if MaxLen <= 0 then
        MaxLen := BIO_ctrl(ABio, BIO_CTRL_PENDING_, 0, nil);
    if MaxLen < 0 then Exit;
    SetLength(Result, MaxLen);
    Len := Bio_read(ABio, PAnsiChar(Result), MaxLen);
    if Len > 0 then
        SetLength(Result, Len)
    else
        SetLength(Result, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ build OpenSSL BIO from AnsiString }
function IcsWriteStrBio(const Data: AnsiString): PBIO;                         { V9.1 }
var
    Len: integer;
begin
    Result := BIO_new(BIO_s_mem);
    if NOT Assigned(Result) then
        Exit;
    if (Length (Data) = 0) then
        Exit;
    Len := BIO_write(Result, @Data [1], Length (Data));
    if Len <> Length (Data) then
       raise ESslTX509Exception.Create('Failed to Write MemBIO');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ build OpenSSL BIO from TBytes }
function IcsWriteTBBio(const Data: TBytes): PBIO;                            { V9.1 }
var
    Len: integer;
begin
    Result := BIO_new(BIO_s_mem);
    if NOT Assigned(Result) then
        Exit;
    if (NOT Assigned(Data)) or (Length (Data) = 0) then
        Exit;
    Len := BIO_write(Result, @Data [0], Length (Data));
    if Len <> Length (Data) then
       raise ESslTX509Exception.Create('Failed to Write MemBIO');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsSslOpenFileBio(const FileName: String; Methode: TBioOpenMethode): PBIO;   { V8.39 was in TSslContext }
var
    fsize: Integer;
    AFName: AnsiString;   { V8.65 was PAnsiChar but pointer may be lost }
begin
    if Filename = '' then
        raise ESslTX509Exception.Create('File name not specified');
{$IFNDEF YuOpenSSL} // BIO_new_file() is always available.
    if NOT Assigned(BIO_new_file) then   { V8.64 sanity check }
        raise ESslTX509Exception.Create('OPENSSL not yet loaded');
{$ENDIF YuOpenSSL}
    if (Methode in [bomRead, bomReadOnly]) then begin
        fsize := IcsGetFileSize(Filename);   { V8.52 check PEM file not empty, which gives strange ASN errors }
        if fsize < 0 then
            raise ESslTX509Exception.Create('File not found "' + Filename + '"');
        if fsize < 16 then    { V8.52 }
            raise ESslTX509Exception.Create('File empty "' + Filename + '"');
    end;
  { V8.64 open and save certificate file with Unicode names not ANSI }
    AFName := StringToUtf8(Filename);  { V8.65 copy to local variable }
    if Methode = bomRead then
        Result := BIO_new_file(PAnsiChar(AFName), PAnsiChar('r+b'))
    else if Methode = bomReadOnly then             { V8.40 mostly we don't want to update certs }
        Result := BIO_new_file(PAnsiChar(AFName), PAnsiChar('rb'))
    else if Methode = bomWriteBin then             { V8.40 mostly we don't want to update certs }
        Result := BIO_new_file(PAnsiChar(AFName), PAnsiChar('w+b'))   { V8.41 binary write mode }
    else
        Result := BIO_new_file(PAnsiChar(AFName), PAnsiChar('w+'));   { writes ASCII CRLF }
    if Result = nil then
        raise ESslTX509Exception.Create ('Error on opening file "' + Filename + '"');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.65 reads a File BIO into an AnsiString, no attempt at UTF8 conversion since only reading base64 }
function IcsSslOpenFileAStr(const FileName: String; Methode: TBioOpenMethode): AnsiString;   { V8.65 }
var
    inBIO: PBIO;
    Len, MaxLen: Integer;
begin
    Result := '';
    InBIO := IcsSslOpenFileBio(FileName, Methode);
    if NOT Assigned (InBIO) then Exit;
    BIO_ctrl(InBio, BIO_CTRL_RESET, 0, nil);
    MaxLen := BIO_ctrl(InBio, BIO_CTRL_PENDING_, 0, nil);           { V8.66 literal changed }
    if MaxLen = 0 then
        MaxLen := IcsGetFileSize(FileName);  // file BIO does not know size
    SetLength(Result, MaxLen);
    if MaxLen > 0 then begin
        Len := Bio_read(InBio, PAnsiChar(Result), MaxLen);
        SetLength(Result, Len);
    end;
    BIO_free(InBIO);
    if MaxLen = 0 then
        raise ESslTX509Exception.Create('File empty "' + Filename + '"');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.65 read multiple PEM certificates from ANSI string, build stack, returning list of errors }
{ no longer using PEM_X509_INFO_read_bio because it does not like 8-bit characters
  and died with the first corrupted certificate }
function IcsSslLoadStackFromAStr(const PemStr: AnsiString; var Errs: String; Mode: TInfoExtractMode = emCert): PStack;  { V8.65 }
var
    cstart, cend, clen, I, tot: Integer;
    OneCert: AnsiString;
    OneErr: String;
    MemBIO : PBIO;
    X509Cert: PX509;
    X509CRL: PX509_CRL;
begin
    Result := nil;
    Errs := '';
    if (IcsAnsiPosEx(PEM_STRING_HDR_BEGIN, PemStr) = 0) and (IcsAnsiPosEx(PEM_STRING_HDR_END, PemStr) = 0) then begin
        Errs := 'No PEM Content Found';
        Exit;
    end;
    cstart := 1;
    tot := 0;
    clen := Length(PemStr);

 // find each certificate in bundle and add to stack
    while true do begin
        try
            if cstart >= clen then break;  // beyond end

         // find begin-base64-end block
            cstart := IcsAnsiPosEx(PEM_STRING_HDR_BEGIN, PemStr, cstart);
            if cstart < 1 then break;
            cend := IcsAnsiPosEx(PEM_STRING_HDR_END, PemStr, cstart);
            if cend < cstart then break;
            I := IcsAnsiPosEx(IcsLF, PemStr, cend); // end of line
            if I > cend then
                cend := I
             else
                cend := clen;    // no end of line
            OneCert := Copy(PemStr, cstart, cend - cstart + 1);
            cstart := cend;

         // sanity check for some base64 lines to process
            if Length (OneCert) < 390 then continue;

         // process one certificate
            tot := tot + 1;
            MemBio := BIO_new_mem_buf(PAnsiChar(OneCert), Length (OneCert));
            BIO_ctrl(MemBio, BIO_CTRL_RESET, 0, nil);

            OneErr := '';
            if (Mode = emCrl) and (IcsAnsiPosEx(PEM_STRING_X509_CRL, OneCert) > 0) then begin
                 X509CRL := PEM_read_bio_X509_CRL(MemBio, Nil, Nil, Nil);   // no passwords
                BIO_free(MemBio);
                if Assigned(X509CRL) then begin
                    if Result = Nil then
                        Result := OPENSSL_sk_new_null;
                    OPENSSL_sk_insert(Result, PAnsiChar(X509_CRL_dup(X509CRL)), OPENSSL_sk_num(Result) + 1);
                    X509_CRL_free(X509CRL);
                end
                else begin
                    OneErr := 'Error CRL ' + IntToStr(tot) + ' - ' + String(LastOpenSslErrMsg(FALSE));
                end;
            end
            else if (IcsAnsiPosEx(PEM_STRING_X509, OneCert) > 0) or (IcsAnsiPosEx(PEM_STRING_X509_OLD, OneCert) > 0)  then  begin
                X509Cert := PEM_read_bio_X509(MemBio, Nil, Nil, Nil);   // no passwords
                BIO_free(MemBio);
                if Assigned(X509Cert) then begin
                    if Result = Nil then
                        Result := OPENSSL_sk_new_null;
                    OPENSSL_sk_insert(Result, PAnsiChar(X509_dup(X509Cert)), OPENSSL_sk_num(Result) + 1);
                    X509_free(X509Cert);
                end
                else begin
                    I := IcsAnsiPosEx(IcsLF, OneCert, 50); // end of line, skip -begin-
                    if I > 100 then
                        OneErr := 'Maximum line length in PEM file is 64'
                    else
                        OneErr := String(LastOpenSslErrMsg(FALSE));
                    OneErr := 'Error Cert ' + IntToStr(tot) + ' - ' + OneErr;
                end;
            end
            else
                BIO_free(MemBio);   { V8.65 fix memory leak }

            // ignore private keys and other stuff

            if OneErr <> '' then begin
                if Errs <> '' then Errs := Errs + ';' + IcsCRLF;
                Errs := Errs + OneErr;
            end;
        except
            if Errs = '' then Errs := Errs + ', ';
            Errs := Errs + 'Exception Cert ' + IntToStr(tot);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsSslLoadStackFromP12TB(const Certs: TBytes; var Errs: String): PStack;             { V9.1 }
var
    ABio: PBIO;
    P12: PPKCS12;
    Cert: PX509;
    PKey: PEVP_PKEY;
    PW: PAnsiChar;
begin
    Result := Nil;
    if Length(Certs) = 0  then begin
        Errs := 'PKCS12 bundle empty';
        Exit;
    end;
    ABio := IcsWriteTBBio(Certs);
    if NOT Assigned(ABio) then begin
        Errs := 'Error reading PKCS12 bundle from TBytes';
        Exit;
    end;
    try
        P12 := d2i_PKCS12_bio(ABio, Nil);
        if NOT Assigned(P12) then begin
            Errs := 'Error reading PKCS12 bundle d2i - ' + String(LastOpenSslErrMsg(False));
            Exit;
        end;
        try
            Cert := Nil;
            Pkey := Nil;
//            Ca := Nil;
            PW := Nil;
            if PKCS12_parse(P12, PW, @Pkey, @Cert, @Result) = 0 then begin
                Errs := 'Error parsing PKCS12 bundle - ' + String(LastOpenSslErrMsg(False));
                Exit;
            end;
            if Assigned(Cert) then begin
                if NOT Assigned(Result) then
                    Result := OPENSSL_sk_new_null;
                OPENSSL_sk_insert(Result, PAnsiChar(Cert), 1);      // generally not expecting main cert
                X509_free(Cert);
            end;
        finally
            PKCS12_free(p12);
        end;
    finally
        bio_free(ABio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.65 load a private key from a PEM String, ignore any certificates }
{ supports traditional and PKCS#8 format encrypted and unencrypted keys }
function IcsSslLoadPkeyFromAStr(const PemStr, APassword: AnsiString;  var Err: String): PEVP_PKEY;    { V8.65 }
var
    cstart, cend, MaxLen, I: Integer;
    OneCert   : AnsiString;
    MemBio    : PBIO;
begin
    Err := '';
    Result := Nil;
    MaxLen := Length(PemStr);

 { look for PKCS8 PRIVATE KEY or ENCRYPTED PRIVATE KEY in PEM file }
 { V8.65 find private key block to avoid passing certs and comments to APIs }
    cstart := 1;
    while TRUE do begin
        if cstart >= MaxLen then break;
        cstart := IcsAnsiPosEx(PEM_STRING_HDR_BEGIN, PemStr, cstart);
        if cstart < 1 then break;
        cend := IcsAnsiPosEx(PEM_STRING_HDR_END, PemStr, cstart);
        if cend < cstart then break;
        I := IcsAnsiPosEx(IcsLF, PemStr, cend); // end of line
        if I > cend then
            cend := I
         else
            cend := MaxLen;    // no end of line
        OneCert := Copy(PemStr, cstart, cend - cstart + 1);
        if IcsAnsiPosEx(PEM_STRING_PKCS8INF, OneCert) > 0 then begin
            MemBio := BIO_new_mem_buf(PAnsiChar(OneCert), Length(OneCert));
            BIO_ctrl(MemBio, BIO_CTRL_RESET, 0, nil);
            Result := PEM_read_bio_PrivateKey(MemBio, nil, nil, PAnsiChar(APassword));
            if (Result = Nil) and (APassword <> '') then
                Result := PEM_read_bio_PrivateKey(MemBio, nil, nil, nil);  { V8.67 retry without password }
            if Result = Nil then
                Err := 'Error private key - ' + String(LastOpenSslErrMsg(FALSE));
            BIO_free(MemBIO);
            exit;  // only one private key per file
        end;
        cstart := cend;  // skip certificates
    end;
    if Result = Nil then
        Err := 'No private key found in PEM file';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.65 read multiple certificates from string, build stack, return list of errors }
function IcsSslLoadStackFromPemTB(const Certs: TBytes; var Errs: String; Mode: TInfoExtractMode = emCert): PStack;  { V9.1 }
var
    AStr: AnsiString;
begin
    IcsMoveTBytesToString(Certs, 0, AStr, 1, 0);
    Result := IcsSslLoadStackFromAStr(AStr, Errs, Mode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.65 read multiple certificates from string, build stack, return list of errors }
function IcsSslLoadStackFromStr(const PemStr: String; var Errs: String; Mode: TInfoExtractMode = emCert): PStack;   { V8.65 }
begin
    Result := IcsSslLoadStackFromAStr(AnsiString(PemStr), Errs, Mode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ read multiple certificates from certifcate bunde file, build stack, returning list of errors }
{ supports PEM and PKCS12 bundles }
function IcsSslLoadStackFromInfoFileEx(const FileName: String; var Errs: String; Mode: TInfoExtractMode = emCert): PStack;   { V8.65 }
var
//    AStr: AnsiString;
    Fext: string;
    Certs: TBytes;
begin
    Errs := '';
    Result := Nil;
    fext := IcsLowerCase(ExtractFileExt(FileName));
    if (fext = '.pfx') or  (fext = '.p12') or (fext = '.pem') or (fext = '.cer') then begin
        Certs := IcsDataLoadFile(FileName);
        if Length(Certs) = 0 then begin
            Errs := 'Failed to Open File: ' + FileName;
            Exit;
        end;
        if IcsTBytesPos(PEM_STRING_HDR_BEGIN, Certs, 0, 999) >= 0 then
            Result := IcsSslLoadStackFromPemTB(Certs, Errs, Mode)  { V9.3 added Mode so CRLs get located }
        else
            Result := IcsSslLoadStackFromP12TB(Certs, Errs);
    end
    else
        Errs := 'Unknown bundle file extension';
//    AStr := IcsSslOpenFileAStr(FileName, bomReadOnly);  { V8.65 AStr instead of BIO }
//    Result := IcsSslLoadStackFromAStr(AStr, Errs, Mode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ read multiple certificates from certifcate bunde file, build stack, returning list of errors }
{ supports PEM and PKCS12 bundles }
function IcsSslLoadStackFromInfoFile(const FileName: String; Mode: TInfoExtractMode): PStack;  { V8.39 was in TSslContext }
var
    Errs: String;
begin
    Result := IcsSslLoadStackFromInfoFileEx(FileName, Errs, Mode);
    if Errs <> '' then
        raise ESslTX509Exception.Create('Reading file of PEM certificates: ' + Errs + ', from: ' + FileName);   { V8.65 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.41 read multiple certificates from lines as base64, build stack }
function IcsSslLoadStackFromInfoString(const Value: String; Mode: TInfoExtractMode): PStack;
var
    Errs: String;
begin
    Result := IcsSslLoadStackFromStr(Value, Errs, emCert);  { V8.65 }
    if Errs <> '' then
        raise ESslTX509Exception.Create('Reading list of PEM certificates: ' + Errs);   { V8.65 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsUnwrapNames(const S: String): String;     { V8.39 multi-line with comma line }
begin
    Result := StringReplace(S, #13#10, ', ', [rfReplaceAll]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// V8.62 create certificate file name from domain common name, change . and space to _ and * to x
function IcsBuildCertFName(const Domain: String): String;  // V9.1 moved from WSocketS
begin
    Result := StringReplace (Domain, '.', '_', [rfReplaceAll]) ;
    Result := StringReplace (Result, IcsSpace, '_', [rfReplaceAll]) ;  { V8.64 }
    if Result = '' then
        Exit;
    if Result [1] = '*' then
        Result [1] := 'x';  // can not have * in file names
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 get name of TLS extension type }
function WSocketGetTlsExtStr(Etype: Integer): String;
begin
    Result := 'Unknown ' + IntToHex(Etype, 4);
    case Etype of
      TLSEXT_TYPE_server_name :
          Result := 'server name';
      TLSEXT_TYPE_max_fragment_length :
          Result := 'max frag length';
      TLSEXT_TYPE_client_certificate_url :
          Result := 'client cert URL';
      TLSEXT_TYPE_trusted_ca_keys :
          Result := 'trusted CA keys';
      TLSEXT_TYPE_truncated_hmac :
          Result := 'truncated HMAC';
      TLSEXT_TYPE_status_request :
          Result := 'status request (OCSP stapling)';
      TLSEXT_TYPE_elliptic_curves :
          Result := 'elliptic curves';
      TLSEXT_TYPE_ec_point_formats :
          Result := 'EC point formats';
      TLSEXT_TYPE_session_ticket :
          Result := 'server ticket';
      TLSEXT_TYPE_signature_algorithms :
          Result := 'signature algos';    { V8.56 }
      TLSEXT_TYPE_use_srtp :
          Result := 'use srtp';    { V8.56 }
      TLSEXT_TYPE_heartbeat :
          Result := 'heartbeat';    { V8.56 }
      TLSEXT_TYPE_application_layer_protocol_negotiation :
          Result := 'app layer prot neg';    { V8.56 }
      TLSEXT_TYPE_signed_certificate_timestamp :
          Result := 'signed cert stamp';    { V8.56 }
      TLSEXT_TYPE_padding :
          Result := 'padding';    { V8.56 }
      TLSEXT_TYPE_encrypt_then_mac :
          Result := 'encrypt then mac';    { V8.56 }
      TLSEXT_TYPE_extended_master_secret :
          Result := 'ext master secret';    { V8.56 }
      TLSEXT_TYPE_psk :
          Result := 'psk';    { V8.56 }
      TLSEXT_TYPE_early_data :
          Result := 'early_data';    { V8.56 }
      TLSEXT_TYPE_supported_versions :
          Result := 'supp versions';    { V8.56 }
      TLSEXT_TYPE_cookie :
          Result := 'cookie';    { V8.56 }
      TLSEXT_TYPE_psk_kex_modes :
          Result := 'psk kex modes';    { V8.56 }
      TLSEXT_TYPE_certificate_authorities :
          Result := 'cert auth';    { V8.56 }
      TLSEXT_TYPE_post_handshake_auth :
          Result := 'post handshake auth';    { V8.56 }
      TLSEXT_TYPE_signature_algorithms_cert :
          Result := 'sig algo cert';    { V8.56 }
      TLSEXT_TYPE_key_share :
          Result := 'key share';    { V8.56 }
      TLSEXT_TYPE_renegotiate :
          Result := 'renegotiate';    { V8.56 }
      TLSEXT_TYPE_next_proto_neg:
        Result := 'next proto neg';
      TLSEXT_TYPE_compress_certificate :
        Result := 'compress certificate';  { V9.5 }
      TLSEXT_TYPE_client_cert_type:
        Result := 'client_cert_type';      { V9.5 }
      TLSEXT_TYPE_server_cert_type:
        Result := 'server_cert_type';      { V9.5 }
      TLSEXT_TYPE_quic_transport_parameters:
        Result := 'quic_transport_parameters';  { V9.5 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 get name of Elliptic Curve group }
{ from t1_lib.c, can not find API }
function WSocketGetECStr(EC: Cardinal): String;
begin
    case EC of
      $14:  Result := 'secp224k1';
      $15:  Result := 'secp224r1';
      $16:  Result := 'secp256k1';
      $17:  Result := 'secp256r1';
      $18:  Result := 'secp384r1';
      $19:  Result := 'secp521r1';
      $1D:  Result := 'X25519';
      $1E:  Result := 'X448';
      $0100:  Result := 'ffdhe2048';  { following TLSv1.3 with Firefox }
      $0101:  Result := 'ffdhe3072';
      $0102:  Result := 'ffdhe4096';
      $0103:  Result := 'ffdhe6144';
      $0104:  Result := 'ffdhe8192';
      $0200:  Result := 'mlkem512';      { V9.5  }
      $0201:  Result := 'mlkem768';      { V9.5  }
      $0202:  Result := 'mlkem1024';     { V9.5  }
      $11EB:  Result := 'SecP256r1MLKEM768';     { V9.5  }
      $11EC:  Result := 'X25519MLKEM768';        { V9.5  }
      $11ED:  Result := 'SecP384r1MLKEM1024';    { V9.5  }
    else
        Result := IntToHex(EC, 4);
     // GREASE (Generate Random Extensions And Sustain Extensibility) by Google
        if Pos ('A', Result) > 0 then
            Result := 'Grease-' + Result
        else
            Result := 'Unknown ' + Result;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 get name of Signature Algorithm }
{ from s_cb.c, can not find API }
function WSocketGetSigAlgStr(SigAlg: Cardinal): String;
begin
    case SigAlg of
      $0101:  Result := 'rsa_pkcs1_md5';
      $0201:  Result := 'rsa_pkcs1_sha1';
      $0202:  Result := 'dsa_sha1';
      $0203:  Result := 'ecdsa_sha1';
      $0301:  Result := 'drsa_pkcs1_sha224';
      $0302:  Result := 'dsa_sha224';
      $0303:  Result := 'ecdsa_sha224';
      $0401:  Result := 'rsa_pkcs1_sha256';
      $0402:  Result := 'dsa_pkcs1_sha256';
      $0403:  Result := 'ecdsa_secp256r1_sha256';
      $0501:  Result := 'rsa_pkcs1_sha384';
      $0502:  Result := 'dsa_pkcs1_sha384';
      $0503:  Result := 'ecdsa_secp384r1_sha384';
      $0601:  Result := 'rsa_pkcs1_sha384';
      $0602:  Result := 'dsa_pkcs1_sha384';
      $0603:  Result := 'ecdsa_secp521r1_sha512';
      $0804:  Result := 'rsa_pss_rsae_sha256';
      $0805:  Result := 'rsa_pss_rsae_sha384';
      $0806:  Result := 'rsa_pss_rsae_sha512';
      $0807:  Result := 'ed25519';
      $0808:  Result := 'ed448';
      $0809:  Result := 'rsa_pss_pss_sha256';
      $080A:  Result := 'rsa_pss_pss_sha384';
      $080B:  Result := 'rsa_pss_pss_sha512';
    else
        Result := IntToHex(SigAlg, 4);
     // GREASE (Generate Random Extensions And Sustain Extensibility) by Google
        if Pos ('A', Result) > 0 then
            Result := 'Grease-' + Result
        else
            Result := 'Unknown ' + Result;
    end;
 end;


 {* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V0.5 get name of Certificate Type }
function WSocketGetCertType(CType: Byte): String;
begin
    case CType of
      TLSEXT_cert_type_x509:  Result := 'X509';
      TLSEXT_cert_type_pgp:  Result := 'PGP';
      TLSEXT_cert_type_rpk:  Result := 'RawPubKey';
      TLSEXT_cert_type_1609dot2:  Result := '1609dot2';
    else
        Result := '';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ better named method to load OpenSSL }
procedure IcsSslLoad;     { V9.3 }
begin
    IcsLoadSsl;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ better named method to unload OpenSSL }
procedure IcsSslUnload;     { V9.3 }
begin
    IcsUnloadSsl;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 get name TLS version }
function WSocketGetSslVerStr(ver: Cardinal): String;
begin
    if ver = SSL3_VERSION then
        Result := 'SSLv3'
    else if (ver >= TLS1_VERSION) and (ver <= TLS1_3_VERSION) then
        Result := 'TLSv1.' + IntToStr(ver-SSL3_VERSION-1)
    else begin
        Result := IntToHex(ver, 4);
     // GREASE (Generate Random Extensions And Sustain Extensibility) by Google
        if Pos ('A', Result) > 0 then Result := 'Grease-' + Result;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 get description of important parts of Client Hello which lists
   it's capabilities so the server can select protocols and ciphers that match
   Note CipherSuite names are ignored here but can be listed with SslBytesToCiphers }
function WSocketGetCliHelloStr(CliHello: TClientHelloData): String;
var
    I: Integer;
    S: String;
begin
    S := WSocketGetSslVerStr(CliHello.LegacyVersion);
    if Length(CliHello.SuppVersions) > 0 then begin
        for I := 0 to Length(CliHello.SuppVersions) - 1 do
            S := S + ', ' + WSocketGetSslVerStr(CliHello.SuppVersions[I]);
    end;
    Result := 'Server Name: ' + CliHello.ServerName + ', ' +
            'ALPN: ' + CliHello.AlpnList + ', Versions: ' + S;
    if Length(CliHello.KeyShare) > 0 then begin
        Result := Result + ', TLSv1.3 Key Share Data';
    end;
    Result := Result + IcsCRLF;
    S := 'Extensions';
    if CliHello.ExtnTotal > 0 then begin
        for I := 0 to CliHello.ExtnTotal - 1 do
            S := S + ', ' + WSocketGetTlsExtStr(CliHello.ExtnList[I]);
    end;
    Result := Result + S + IcsCRLF;
    S := 'Cipher Suites, Total: ' + IntToStr(Length(CliHello.CipherSuites) div 2);
    if Length(CliHello.EllipCurves) > 0 then begin
        S := S + ', EC Groups';
        for I := 0 to Length(CliHello.EllipCurves) - 1 do
            S := S + ', ' + WSocketGetECStr(CliHello.EllipCurves[I]);
    Result := Result + S + IcsCRLF;
    end;
    if Length(CliHello.SigAlgos) > 0 then begin
        S := 'Signature Algorithms';
        for I := 0 to Length(CliHello.SigAlgos) - 1 do
            S := S + ', ' + WSocketGetSigAlgStr(CliHello.SigAlgos[I]);
        Result := Result + S + IcsCRLF;
    end;
    if Length(CliHello.SrvCertType) > 0 then begin     { V9.5 }
        S := 'Server Certificate Types';
        for I := 0 to Length(CliHello.SrvCertType) - 1 do
            S := S + ', ' + WSocketGetCertType(CliHello.SrvCertType[I]);
        Result := Result + S + IcsCRLF;
    end;
    if Length(CliHello.CliCertType) > 0 then begin     { V9.5 }
        S := 'Client Certificate Types';
        for I := 0 to Length(CliHello.CliCertType) - 1 do
            S := S + ', ' + WSocketGetCertType(CliHello.CliCertType[I]);
        Result := Result + S + IcsCRLF;
    end;
    SetLength(Result, Length(Result) - 2); // strip last CRLF
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsReportOpenSSLVer(RootCA: Boolean = False): String;        { V9.1 centralise version reporting }
begin
    if ICS_OPENSSL_VERSION_NUMBER = 0 then
        Result := 'Not Loaded'
    else if ICS_OPENSSL_VERSION_NUMBER = 999 then
        Result := 'Failed to Load'
    else begin
        if GSSLStaticLinked then
            Result := 'Static ' + OpenSslVersion
        else begin
            Result := 'DLLs ' + OpenSslVersion;
            Result := Result + ' from ' + GSSLEAY_DLL_FileName;
        end;
        if ICS_OSSL3_LOADED_LEGACY then
            Result := Result + ', Legacy Provider Loaded OK'
        else
            Result := Result + ', Legacy Provider Not Loaded';
        if RootCA then begin
            Result := Result + IcsCRLF + IntToStr(IcsSslRootCAStore.Count) + ' CA root certificates loaded';
            if IcsSslRootCAStore.InitFlag then
                Result := Result + ' from internal bundle: ' + IcsSslRootCAStore.CAStoreSource;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ if called before OpenSSL is loaded, will check DLLs are digitally signed }
procedure IcsSetSignTest(SCheck: Boolean = True; SCertificate: Boolean = True);      { V9.3 }
begin
    GSSL_SignTest_Check := SCheck;               { check digitally signed }
    GSSL_SignTest_Certificate := SCertificate;   { check digital certificate }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ if non-zero, the OpenSSL version number that can be compared against OSSL_VER_xx literals, less or greater }
function IcsGetOpenSSLVer: Cardinal;        { V9.3 }
begin
    Result := ICS_OPENSSL_VERSION_NUMBER;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ loads OpenSSL legacy and/or Fips providers, if OpenSSL is loaded  }
function IcsSslLoadProviders(Legacy, Fips: Boolean): Boolean;         { V9.3 }
begin
    Result := LibeayLoadProviders(Legacy, Fips);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ ICS has three optional root certificate authority bundles that can be      }
{ linked as resources, only one of these should be uncommented.              }
{$IFDEF OpenSSL_CA_Bundle_Small}
    {$R sslRootCACertsBundle.RES}
{$ENDIF}
{$IFDEF OpenSSL_CA_Bundle_Medium}
    {$R TrustedCaBundle.RES}
{$ENDIF}
{$IFDEF OpenSSL_CA_Bundle_Large}
    {$R RootCaCertsBundle.RES}
{$ENDIF}

function sslRootCACertsBundle: TBytes;      { V9.1 was string but now PKCS12 binary data }
begin
//    result :=  sslRootCACerts001 + used to build bundle from literals, gone V9.1
    try
        Result := IcsResourceGetTB('ROOTBUNDLEP12', RT_RCDATA);
        if Assigned(Result) and (Length(Result) > 0) then begin
            CARootTxt := IcsTBytesToString(IcsResourceGetTB('ROOTBUNDLETXT', RT_RCDATA));
            CARootVer := IcsTBytesToString(IcsResourceGetTB('ROOTBUNDLEVER', RT_RCDATA));
        end
        else
            SetLength(Result, 0);
    except
        SetLength(Result, 0);
    end;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.3 moved from Libeay, less resource string stuff }
function IcsX509VerifyErrorToStr(ErrCode: Integer): String;
begin
    if ICS_OPENSSL_VERSION_NUMBER > 0 then   { V8.71 check function is available }
        Result := String(AnsiString(X509_verify_cert_error_string(ErrCode)))
    else
        Result := 'OpenSSL not loaded yet';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSslRootCAStore  }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.1 TSslRootCAStore is made public as IcsSslRootCAStore for common use by SslContexts and other
          SSL components that need to check certificates against public roots. }
constructor TSslRootCAStore.Create(AOwner: TComponent; AOwnsObjects: Boolean = TRUE);
begin
    inherited Create(AOwner, AOwnsObjects);
    DefRootFile := GSSL_DEFROOT_NAME; // 'DefRootCABundle.pem'
    ExtraRootFile := GSSL_EXTRAROOT_NAME; // 'ExtraRootCABundle.pem'
    FInitFlag := False;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor  TSslRootCAStore.Destroy;
begin
    Reset;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslRootCAStore.Reset;
begin
    FInitFlag := False;
    Self.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ if loading OpenSSL fails, GSSL_LOAD_ERRS will be non-blank with the error and
  ICS_OPENSSL_VERSION_NUMBER set to either 0 or 999 }
procedure TSslRootCAStore.Initialise;
var
    Errs, FName: String;
    MyBundle: TBytes;
begin
    if FInitFlag then
        Exit;
    try
        IcsLoadSsl;
        if (ICS_OPENSSL_VERSION_NUMBER = 0) or (ICS_OPENSSL_VERSION_NUMBER = 999) then
            Exit;
        Self.Clear;

      { first try internal store resource }
        MyBundle := sslRootCACertsBundle;
        if (Length(MyBundle) > 0) then
            Self.LoadAllFromTB(MyBundle, Errs);
        if FList.Count <> 0 then begin
            FInitFlag := True;
            FCAStoreSource := CARootTxt;
        end

     { now try C:\ProgramData\ICS-OpenSSL\DefRootCABundle.pem }
        else begin
            FName := GSSL_PUBLIC_DIR + DefRootFile;
            if FileExists(FName) then
                Self.LoadAllFromFileEx(FName, Errs)
            else begin
          { missing, try DefRootCABundle.pem somewhere }
                FName := DefRootFile;
                if FileExists(FName) then
                    Self.LoadAllFromFileEx(FName, Errs);
            end;
            if FList.Count <> 0 then begin
                FInitFlag := True;
                FCAStoreSource := FName;
            end;
        end;
        SetLength(MyBundle, 0);

     { load ICS root resource if linked }
        try
            MyBundle := IcsResourceGetTB('ICSROOTCAPEM', RT_RCDATA);
            if Assigned(MyBundle) and (Length(MyBundle) > 0) then begin
                Self.LoadAllFromTB(MyBundle, Errs);
                FCAStoreSource := FCAStoreSource + ' + ICS';
            end;
        except
        end;

      { seeing if any extra certificates to load, perhaps private local roots }
        FName := GSSL_PUBLIC_DIR + ExtraRootFile;
        if FileExists(FName) then begin
            Self.LoadAllFromFileEx(FName, Errs);
            FCAStoreSource := FCAStoreSource + ' + Extra';
        end;

      { build X509_STORE that will be used by SslContexts }
        SetX509Store;
    except
        FInitFlag := False;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSslContext }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslContext.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
{$IFNDEF NO_SSL_MT}
    FLock := TIcsCriticalSection.Create;
{$ENDIF}
    FSslCtx := nil;
    SetSslVerifyPeerModes([SslVerifyMode_PEER]);
    SetSslCipherList(sslCiphersNormal);  // V8.10 same as 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    FSslVersionMethod    := sslBestVer;  // V8.15 same as sslV23 but easier to understand
    FSslMinVersion       := sslVerSSL3;  { V8.27 }
    FSslMaxVersion       := sslVerMax;   { V8.27 }
//  FSslECDHMethod  ignored for OpenSSL 1.1.0 and later
//    FSslECDHMethod       := sslECDHAuto; // V8.20 web sites are increasingly needing ECDH so default it on
    SslVerifyDepth       := 9;
    FSslSessionTimeOut   := 0; // OSSL-default
    FSslSessionCacheSize := SSL_SESSION_CACHE_MAX_SIZE_DEFAULT;
    FSslCertLines        := TStringList.Create;  { V8.27 }
    FSslPrivKeyLines     := TStringList.Create;  { V8.27 }
    FSslCALines          := TStringList.Create;  { V8.27 }
    FSslDHParamLines     := TStringList.Create;  { V8.27 }
{$IFDEF OpenSSL_Deprecated}   { V9.5 }
    FSslDHParamLines.Text := sslDHParams4096;    { V8.27 set default, ideally change if for your own }
{$ENDIF OpenSSL_Deprecated}   { V9.5 }
    FSslCertX509         := TX509Base.Create(Self);   { V8.39 }
    FSslSecLevel         := sslSecLevel80bits;   { V8.40 }
    FSslCryptoGroups     := sslCryptoGroupsDef;  { V8.51 1.1.1 and later }
    FSslCliSecurity      := sslCliSecIgnore;     { V8.54 make backward compatible }
    FSslAlpnProtoList    := TStringList.Create;  { V8.56 }
  { V8.69 added SslOpt2_LEGACY_SERVER_CONNECT, needed for servers that don't support secure renegotiation, 1.1.1 always set this  }
  { V8.69 added sslOpt2_ALLOW_UNSAFE_LEGACY_RENEGOTIATION for clients to connect to such servers }
    FSslOptions2         := [SslOpt2_LEGACY_SERVER_CONNECT, sslOpt2_ALLOW_UNSAFE_LEGACY_RENEGOTIATION];
    FSslCipherList13     := sslCipherSuitesTLS13;   { V9.3 }
    FCliCertTypes        := [];                { V9.5 TLS certificate types supported for clients }
    FSrvCertTypes        := [CertTypeX509];    { V9.5 TLS certificate types supported for servers }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslContext.Destroy;
begin
    try          { V8.71 JK }
        DeInitContext;
        FSslCertLines.Free;     { V8.27 }
        FSslPrivKeyLines.Free;  { V8.27 }
        FSslCALines.Free;       { V8.27 }
        FSslDHParamLines.Free;  { V8.27 }
        FSslCertX509.Free;      { V8.39 }
        FSslAlpnProtoList.Free; { V8.56 }
{$IFNDEF NO_SSL_MT}
        FLock.Free;
{$ENDIF}
    finally
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.FreeNotification(AComponent: TComponent);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        inherited FreeNotification(AComponent);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.RemoveFreeNotification(AComponent: TComponent);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        inherited RemoveFreeNotification(AComponent);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslContext.InitializeCtx: PSSL_CTX;
begin
    Result := SSL_CTX_new(TLS_method);   { V8.66 we set min/max versions later }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslContext.GetIsCtxInitialized : Boolean;
begin
    Result := FSslCtx <> nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslContext.TrustCert(Cert: TX509Base): Boolean;
var
    St : PX509_STORE;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        Result := FALSE;
        if (not Assigned(FSslCtx)) then
            raise ESslContextException.Create(msgSslCtxNotInit);
        if (not Assigned(Cert)) or (not Assigned(Cert.X509)) then
            Exit;
        //St := nil;
        St := SSL_CTX_get_cert_store(FSslCtx);
        if Assigned(St) then
            Result := X509_STORE_add_cert(St, Cert.X509) <> 0;
        { Fails if cert exists in store }
{$IFNDEF NO_DEBUG_LOG}
        if (not Result) and
            CheckLogOptions(loSslErr) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loSslErr, String(LastOpenSslErrMsg(TRUE)));
{$ELSE}
        if (not Result) then
            ERR_clear_error;
{$ENDIF}
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RemoveSessionCallback(const Ctx : PSSL_CTX; Sess : PSSL_SESSION); {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
var
    Obj : TSslContext;
begin
   { If remove_session_cb is not null, it will be called when               }
   { a session-id is removed from the cache.  After the call,               }
   { OpenSSL will SSL_SESSION_free() it.                                    }
   { Also: It is invoked whenever a SSL_SESSION is destroyed.  It is called }
   { just before the session object is destroyed because it is invalid or   }
   { has expired.                                                           }
{$IFNDEF NO_SSL_MT}
    LockRemSessCB.Enter;
    try
{$ENDIF}
        Obj := TSslContext(SSL_CTX_get_ex_data(Ctx, 0));
        if Assigned(Obj) then begin
{$IFNDEF NO_DEBUG_LOG}
            if Obj.CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                Obj.DebugLog(loSslInfo,'RSCB> Session removed');
{$ENDIF}
            if Assigned(Obj.FOnRemoveSession) then
                Obj.FOnRemoveSession(Obj, Sess);
        end;
{$IFNDEF NO_SSL_MT}
    finally
        LockRemSessCB.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_SSL_MT}
procedure TSslContext.Lock;
begin
    FLock.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.Unlock;
begin
    FLock.Leave;
end;

{$ENDIF}



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ certificate revocation list (CRL) - not tested for many years             }
{ PEM format only, the file may contain multiple certificates.              }
{ Loads intermediate CA certificates needed to build a complete chain.      }
{ PEM format only, any file of a given directory }
{ PEM format only, the file may contain multiple CRL's }
procedure TSslContext.LoadCrlFromFile(const Filename: String);
var
    CRL      : PX509_CRL;
    St       : PX509_STORE;
    CrlStack : PStack;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if not Assigned(FSslCtx) then
            raise ESslContextException.Create(msgSslCtxNotInit);
        if (Filename <> '') and (not FileExists(Filename)) then
            raise ESslContextException.Create('CRL file not found "' + Filename + '"');
        if Filename <> '' then begin
            CrlStack := IcsSslLoadStackFromInfoFile(FileName, emCrl);   { V8.39 }
            if not Assigned(CrlStack) then
                raise ESslContextException.Create('Error on reading CRL file "' + Filename + '"');
            try
                St := SSL_CTX_get_cert_store(FSslCtx);
                if not Assigned(St) then
                    raise ESslContextException.Create('Error on opening store');
                while OPENSSL_sk_num(CrlStack) > 0 do begin
                    Crl := PX509_CRL(OPENSSL_sk_delete(CrlStack, 0));
                    if Assigned(Crl) then
                        try
                            { Fails if CRL is already in hash table }
                            if X509_STORE_add_crl(St, Crl) = 0 then
{$IFNDEF NO_DEBUG_LOG}
                                if CheckLogOptions(loSslErr) then  { V5.21 }
                                    DebugLog(loSslErr, String(LastOpenSslErrMsg(True)));
{$ELSE}
                                ERR_clear_error;
{$ENDIF};
                        finally
                            X509_CRL_free(Crl);
                        end;
                    end;
            finally
                OPENSSL_sk_pop_free(CrlStack, @X509_CRL_free);
            end;
        end;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ certificate revocation list (CRL)                                         }
{ PEM format only, any file of a given directory }
procedure TSslContext.LoadCrlFromPath(const Path: String);
var
    SRec  : TSearchRec;
    Found : Boolean;
    S     : String;
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (Path <> '') and (not DirectoryExists(Path)) then
        raise ESslContextException.Create('CRL directory not found "' + Path + '"');
    if Path <> '' then begin
        S := IncludeTrailingPathDelimiter(Path);
        Found := FindFirst(S + '*.*', faAnyFile - faDirectory, SRec) = 0;
        if Found then
            try
                while Found do begin
                    LoadCrlFromFile(S + SRec.Name);
                    Found := FindNext(SRec) = 0;
                end;
            finally
                FindClose(SRec);
            end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.LoadVerifyLocations(const CAFile, CAPath: String);
var
    PCAPath : PAnsiChar;
    PCAFile : PAnsiChar;
begin
    if FUseSharedCAStore then      { V9.1 skip if using shared store }
        Exit;

    // Load the CAs we trust
    //
    // If CAfile is not NIL, it points to a file of CA certificates in PEM
    // format. The file can contain several CA certificates.
    //
    // If CApath is not NIL, it points to a directory containing CA
    // certificates in PEM format. The files each contain one CA certificate.
    // The files are looked up by the CA subject name hash value, which must
    // hence be available. If more than one CA certificate with the same name
    // hash value exist, the extension must be different (e.g. 9d66eef0.0,
    // 9d66eef0.1 etc). The search is performed in the ordering of the
    // extension number, regardless of other properties of the certificates.
    // The certificates in CApath are only looked up when required, e.g. when
    // building the certificate chain or when actually performing the
    // verification of a peer certificate. When looking up CA certificates,
    // the OpenSSL library will first search the certificates in CAfile, then
    // those in CApath.

    if FSslCtx = nil then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (CAFile <> '') and (not FileExists(CAFile)) then
        raise ESslContextException.Create('File not found "' + CAFile + '"');
    if (Length(CAPath) > 0) and (not DirectoryExists(CAPath)) then
        raise ESslContextException.Create('Directory not found "' + CAPath + '"');

    if CAPath <> '' then
        PCAPath := PAnsiChar(AnsiString(CAPath))
    else
        PCAPath := nil;
    if CAFile <> '' then
        PCAFile := PAnsiChar(AnsiString(CAFile))
    else
        PCAFile := nil;
    if ((PCAFile <> nil) or (PCAPath <> nil)) and
       (SSL_CTX_load_verify_locations(FSslCtx, PCAFile, PCAPath) = 0) then
           RaiseLastOpenSslError(ESslContextException, TRUE,
                'Can''t read CA File "' + CAFile + '" or ' + 'CA Path "' + CAPath + '"');

 { V8.66 don't let OpenSSL search unexpected CA locations, we'll load something soon
    if (PCAFile = nil) and (PCAPath = nil) and
       (SSL_CTX_set_default_verify_paths(FSslCtx) <> 1) then
        RaiseLastOpenSslError(ESslContextException, TRUE,
             'Error loading default CA file ' + 'and/or directory');    }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetX509SubjectOneLine(Cert: PX509): String;   { V8.27 legacy, does not handle UTF8 characters  }
var
    Str : AnsiString;
begin
    Result := '';
    if not Assigned(Cert) then  Exit;
    SetLength(Str, 512);
    Str := X509_NAME_oneline(X509_get_subject_name(Cert), PAnsiChar(Str), Length(Str));
    SetLength(Str, StrLen(PAnsiChar(Str)));
    Result := String(Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.LoadCAFromStack(CertStack: PStack);        { V8.41 }
var
    Cert: PX509;
    Store: PX509_STORE;
    Tot, I: integer;
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if not Assigned(CertStack) then
        raise ESslContextException.Create('No CA stack to load');
    Tot := OPENSSL_sk_num(CertStack);
    if Tot = 0 then Exit;
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        Store := SSL_CTX_get_cert_store(FSslCtx);
        if not Assigned(Store) then
            raise ESslContextException.Create('Error on opening store');
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
            DebugLog(loSslInfo, 'Read ' + IntToStr(Tot) + ' CA certificates from stack');
{$ENDIF};
        for I := 0 to Tot - 1 do begin
            Cert := X509_dup(PX509(OPENSSL_sk_value(CertStack, I)));
            if Assigned(Cert) then
            try
{$IFNDEF NO_DEBUG_LOG}
                if CheckLogOptions(loSslInfo) then
                    DebugLog(loSslInfo, 'Certificate: ' + GetX509SubjectOneLine(Cert));
{$ENDIF};
              { Fails if Cert is already in hash table }
                if X509_STORE_add_cert(Store, Cert) = 0 then
{$IFNDEF NO_DEBUG_LOG}
                    if CheckLogOptions(loSslErr) then
                        DebugLog(loSslErr, String(LastOpenSslErrMsg(True)));
{$ELSE}
                    ERR_clear_error;
{$ENDIF};
            finally
                X509_free(Cert);
            end;
        end;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.LoadCAFromTB(const Certs: TBytes);                  { V9.1 }
var
    CertStack: PStack;
    Errs: String;
begin
    if FUseSharedCAStore then      { V9.1 skip if using shared store }
        Exit;
    if Length(Certs) = 0 then
        Exit;
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if IcsTBytesPos(PEM_STRING_HDR_BEGIN, Certs, 0, 999) >= 0 then
        CertStack := IcsSslLoadStackFromPemTB(Certs, Errs)
    else
        CertStack := IcsSslLoadStackFromP12TB(Certs, Errs);
    if not Assigned(CertStack) then
        raise ESslContextException.Create('Error on reading CA certificate lines');
    try
       LoadCAFromStack(CertStack);
    finally
       OPENSSL_sk_pop_free(CertStack, @X509_free);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.LoadCAFromString(const Value: String);        { V8.27 }
var
    Certs: TBytes;
begin
    IcsMoveStringToTBytes(Value, Certs, 0);
    LoadCAFromTB(Certs);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.41 get X509 certificate, private key and intermediates from context }
function TSslContext.SslGetCerts(Cert: TX509Base): integer;
var
    Store: PX509_STORE;
    MyStack: PStack;
    Tot, I: integer;
    MyX509Obj: PX509_OBJECT;
begin
    Result := 0;
    if not Assigned(FSslCtx) then exit;
    if not Assigned(Cert) then exit;
    Cert.X509 := SSL_CTX_get0_certificate(FSslCtx);
    Cert.PrivateKey := SSL_CTX_get0_privatekey(FSslCtx);
    Cert.FreeAndNilX509Inters;
    Store := SSL_CTX_get_cert_store(FSslCtx);
    if NOT Assigned(Store) then Exit;
    MyStack := X509_STORE_get0_objects(Store);
    Tot := OPENSSL_sk_num(MyStack);   { !!! don't free stack }
    if Tot = 0 then Exit;
    for I := 0 to Tot - 1 do begin
        MyX509Obj := PX509_OBJECT(OPENSSL_sk_value(MyStack, I));
        if X509_OBJECT_get_type(MyX509Obj) = X509_LU_X509 then
           Cert.AddToInters({f_X509_dup(}X509_OBJECT_get0_X509 (MyX509Obj));  { V8.64 memory leak }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.41 set context certificate and/or private key and intermediate certificates }
procedure TSslContext.SslSetCertX509;
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if FSslCertX509.IsCertLoaded then begin   { V8.44 did not load inter unless cert set }
        if (SSL_CTX_use_certificate(FSslCtx, FSslCertX509.X509) = 0) then begin
        {$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslErr) then
                    DebugLog(loSslErr, String(LastOpenSslErrMsg(TRUE)));
        {$ELSE}
            ERR_clear_error;
        {$ENDIF}
            RaiseLastOpenSslError(ESslContextException, TRUE,
                                      'Can''t add certificate to context');
        end;
        if FSslCertX509.IsPKeyLoaded then begin
            if (SSL_CTX_use_PrivateKey(FSslCtx, FSslCertX509.PrivateKey) = 0) then begin
            {$IFNDEF NO_DEBUG_LOG}
                if CheckLogOptions(loSslErr) then
                        DebugLog(loSslErr, String(LastOpenSslErrMsg(TRUE)));
            {$ELSE}
                ERR_clear_error;
            {$ENDIF}
                RaiseLastOpenSslError(ESslContextException, TRUE,
                                          'Can''t add private key to context');
            end;
       end;
    end;
    if FSslCertX509.IsInterLoaded then begin
        LoadCAFromStack(FSslCertX509.X509Inters);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 check certificate and private key match }
function TSslContext.CheckPrivateKey: boolean;
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    result := SSL_CTX_check_private_key(FSslCtx) <> 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.65 load a PEM certificate chain from a string, returns list of any errors }
{ NOTE does not load private key }
procedure TSslContext.LoadCertFromAStr(const Value: AnsiString; var Errs: String);
var
    Cert: PX509;
    CertStack: PStack;
begin
    if Length(Value) = 0 then Exit;
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        CertStack := IcsSslLoadStackFromAStr(Value, Errs, emCert);
        if not Assigned(CertStack) then
            raise ESslContextException.Create('Error on reading certificate lines - ' + Errs);
        try
          if (SSL_CTX_clear_chain_certs(FSslCtx) = 0) then
                raise ESslContextException.Create('Error on clearing chain certs');
{$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslInfo) then  { V5.21 }
                DebugLog(loSslInfo, 'Read ' + IntToStr(OPENSSL_sk_num(CertStack)) +
                                                    ' certificates from strings');
{$ENDIF};
         { first certificate is current used for encryption and identification }
            Cert := PX509(OPENSSL_sk_delete(CertStack, 0));
            if Assigned(Cert) then
            try
{$IFNDEF NO_DEBUG_LOG}
                if CheckLogOptions(loSslInfo) then  { V5.21 }
                     DebugLog(loSslInfo, 'Current certificate: ' + GetX509SubjectOneLine(Cert));
{$ENDIF};
                if (SSL_CTX_use_certificate(FSslCtx, Cert) = 0) then begin
            {$IFNDEF NO_DEBUG_LOG}
                    if CheckLogOptions(loSslErr) then
                        DebugLog(loSslErr, String(LastOpenSslErrMsg(TRUE)));
            {$ELSE}
                    ERR_clear_error;
            {$ENDIF}
                    RaiseLastOpenSslError(ESslContextException, TRUE,
                                          'Can''t add certificate to context');
                end;
            finally
                X509_free(Cert);
            end;

        { remaining certificates are chain used to sign current certificate, usually one or two }
        { !! this function is supposed to be for a server requesting a client certificate, but
             seems to also store extra chain certificates }
            LoadCAFromStack(CertStack);
         finally
            OPENSSL_sk_pop_free(CertStack, @X509_free);
         end;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.27 load a PEM certificate chain from a string }
procedure TSslContext.LoadCertFromString(const Value: String);
var
    Errs: String;
begin
    LoadCertFromAStr(AnsiString(Value), Errs);
    if Errs <> '' then
        raise ESslContextException.Create('Error on reading certificate lines - ' + Errs);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The PEM file specified may contain just a single certificate or a         }
{ certificate chain. A chain must start with the server or client           }
{ certificate followed by intermediate CA certificates until the root       }
{ certificate (optional). This entire chain is sent to the peer for         }
{ verification.                                                             }
{ V8.65 reworked, blank certificate is OK }

procedure TSslContext.LoadCertFromChainFile(const FileName: String);
var
    PemStr: AnsiString;  { V8.65 }
    Errs: String;
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (FileName <> '') then begin
        PemStr := IcsSslOpenFileAStr(FileName, bomReadOnly);  { V8.65 }
        LoadCertFromAStr(PemStr, Errs);
        if Errs <> '' then raise ESslContextException.Create
                ('Error on reading certificate file - ' + Errs + ' - ' + FileName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.LoadPKeyFromFile(const FileName: String);
var
    PemStr: String;  { V8.65 }
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (FileName <> '') and (not FileExists(FileName)) then
        raise ESslContextException.Create('File not found "' + FileName + '"');
    if (FileName = '') then Exit;
    PemStr := String(IcsSslOpenFileAStr(FileName, bomReadOnly));  { V8.65 }
    LoadPKeyFromString(PemStr);                                   { V8.65 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.27 load a PEM private key from a string }
{ V8.65 reworked to use more robust IcsSslLoadPkeyFromAStr }
procedure TSslContext.LoadPKeyFromString(const Value: String);
var
    Pkey: PEVP_PKEY;
    Err: String;
begin
    if Length(Value) = 0 then Exit;
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    Pkey := nil;
    try
        PKey := IcsSslLoadPkeyFromAStr(AnsiString(Value), PasswordConvert(SslPassPhrase), Err);   { V8.65 }
        if NOT Assigned (Pkey) then begin
    {$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslErr) then
                DebugLog(loSslErr, Err);
    {$ELSE}
            ERR_clear_error;
    {$ENDIF}
            raise ESslContextException.Create('Can''t read private key lines: ' + Err);
        end;
        if (SSL_CTX_use_PrivateKey(FSslCtx, Pkey) = 0) then begin
    {$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslErr) then
                DebugLog(loSslErr, String(LastOpenSslErrMsg(TRUE)));
    {$ELSE}
            ERR_clear_error;
    {$ENDIF}
            RaiseLastOpenSslError(ESslContextException, TRUE,
                                  'Can''t read add private key to context');
        end;
    finally
        if Assigned (Pkey) then EVP_PKEY_free(Pkey);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.LoadDHParamsFromFile(const FileName: String);   { V8.15 }
{$IFDEF OpenSSL_Deprecated}  { V9.5 }
var
    FileBio : PBIO;
    MyPDH: PDH;
{$ENDIF OpenSSL_Deprecated}   { V9.5 }
begin
{$IFDEF OpenSSL_Deprecated}  { V9.5 }
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (FSslVersionMethod < sslV3) then Exit;   { V8.24 SSLv2 does not support DH }
    if (FileName <> '') and (not FileExists(FileName)) then
        raise ESslContextException.Create('File not found "' + FileName + '"');
    if (FileName <> '') then begin
        FileBio := IcsSslOpenFileBio(FileName, bomReadOnly);  { V8.40 }
        MyPDH := nil;
        try
            MyPDH := PEM_read_bio_DHParams(FileBio, nil, nil, nil);
            if not Assigned(MyPDH) then
                RaiseLastOpenSslError(EX509Exception, TRUE,
                     'Error reading DHparam file "' +  Filename + '"');
            if (SSL_CTX_set_tmp_dh(FSslCtx, MyPDH) = 0) then begin
{$IFNDEF NO_DEBUG_LOG}
               if CheckLogOptions(loSslErr) then       { V8.57 was loSslInfo }
                    DebugLog(loSslErr, String(LastOpenSslErrMsg(TRUE)));
{$ELSE}
               ERR_clear_error;
{$ENDIF}
               RaiseLastOpenSslError(ESslContextException, TRUE,
                     'Can''t load DHParam ' + 'file "' + FileName + '"');
            end;
        finally
            bio_free(FileBio);
            if Assigned (MyPDH) then DH_free(MyPDH);
        end;
    end;
{$ELSE}
    RaiseLastOpenSslError(ESslContextException, TRUE, 'Can''t load DHParamss, Deprecated');
{$ENDIF OpenSSL_Deprecated}   { V9.5 }

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.LoadDHParamsFromString(const Value: String);
{$IFDEF OpenSSL_Deprecated}  { V9.5 }
var
    Bio: PBIO;
    MyPDH: PDH;
{$ENDIF OpenSSL_Deprecated}   { V9.5 }
begin
{$IFDEF OpenSSL_Deprecated}  { V9.5 }
    if Length(Value) = 0 then Exit;
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (FSslVersionMethod < sslV3) then Exit;   { V8.24 SSLv2 does not support DH }
    if (Pos(PEM_STRING_HDR_BEGIN, Value) = 0) and (Pos(PEM_STRING_HDR_END, Value) = 0) then
            raise ESslContextException.Create('Expected a Base64 encoded DH params');

    MyPDH := nil;
    Bio := BIO_new_mem_buf(PAnsiChar(AnsiString(Value)), Length (Value));
    try
        if NOT Assigned(Bio) then
            raise ESslContextException.Create('Failed to read encoded DH params');
        MyPDH := PEM_read_bio_DHParams(Bio, nil, Nil, Nil);  { V8.65 no password }
        if not Assigned(MyPDH) then
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error reading DHparams');
        if (SSL_CTX_set_tmp_dh(FSslCtx, MyPDH) = 0) then begin
{$IFNDEF NO_DEBUG_LOG}
           if CheckLogOptions(loSslErr) then   { V8.57 was loSslInfo }
                DebugLog(loSslErr, String(LastOpenSslErrMsg(TRUE)));
{$ELSE}
           ERR_clear_error;
{$ENDIF}
           RaiseLastOpenSslError(ESslContextException, TRUE, 'Can''t load DHParamss');
        end;
    finally
        if Assigned (MyPDH) then DH_free(MyPDH);
        if Assigned (Bio) then BIO_free(Bio);
    end;
{$ELSE}
    RaiseLastOpenSslError(ESslContextException, TRUE, 'Can''t load DHParamss, Deprecated');
{$ENDIF OpenSSL_Deprecated}   { V9.5 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF OPENSSL_NO_ENGINE}
(*
procedure TSslContext.Notification(
    AComponent : TComponent;
    Operation  : TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if AComponent = FCtxEngine then
            FCtxEngine := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetCtxEngine(const Value: TSslEngine);
begin
    FCtxEngine := Value;
    if Value <> nil then
        Value.FreeNotification(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.LoadPKeyFromEngine(CtxEngine: TSslEngine);
var
    PKey : PEVP_PKEY;
    Uim  : PUI_METHOD;
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (CtxEngine = nil) or (CtxEngine.KeyID = '') then
        raise ESslContextException.Create('Engine and KeyID may not be empty');

    if CtxEngine.State <> esInit then
        if not CtxEngine.Init then
            raise ESslContextException.Create(CtxEngine.LastErrorMsg);
        Uim := UI_create_method(PAnsiChar('ICS WIN32 UI'));
        UI_method_set_reader(Uim, @PinCallback);   { V8.66 }
        PKey := ENGINE_load_private_key(CtxEngine.E,
                                          PAnsiChar(AnsiString(CtxEngine.KeyID)),
                                          Uim, Pointer(Self));

        if PKey = nil then
            RaiseLastOpenSslError(ESslContextException, TRUE,
                              'Can''t load private key from Engine');

        if SSL_CTX_use_PrivateKey(FSslCtx, PKey) = 0 then
            RaiseLastOpenSslError(ESslContextException, TRUE,
                                  'Can''t use private key');
end;
*)
{$ENDIF OPENSSL_NO_ENGINE}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Open a PEM CA certificate file and add the CA name extracted              }
{ to the list of CAs sent to the client when requesting a client            }
{ certificate, usefull only in server mode.                                 }
{ NOTE this is used when clients send certificates to servers, which is rare }
procedure TSslContext.AddClientCAFromFile(const FileName: String);
var
    X : TX509Base;
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (Filename <> '') and (not FileExists(Filename)) then
        raise ESslContextException.Create('Certificate file not found "' + Filename + '"');
    if Filename <> '' then begin
        X := TX509Base.Create(nil);
        try
            X.LoadFromPemFile(FileName, croNo, croNo, '');   { V8.40 }
            if SSL_CTX_add_client_CA(FSslCtx, X.X509) <> 1 then
                RaiseLastOpenSslError(ESslContextException, TRUE, 'Can''t load client CA ' + 'file "' + FileName + '"');
        finally
            X.Free;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Scan all certificates in a PEM CAfile and list their names as acceptable  }
{ CAs sent to the client when we request a client certificate. Useful only }
{ in server mode.                                                           }
{ NOTE this is used when clients send certificates to servers, which is rare }
procedure TSslContext.SetClientCAListFromFile(const FileName: String);
var
    Sk : PSTACK_OF_X509_NAME;
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (Filename <> '') and (not FileExists(Filename)) then
        raise ESslContextException.Create('Certificate file not found "' + Filename + '"');
    if Filename <> '' then begin
        Sk := SSL_load_client_CA_file(PAnsiChar(AnsiString(FileName)));
        if not Assigned(Sk) then
            raise ESslContextException.Create('Error on reading certificate ' + 'file "' + Filename + '"');
        SSL_CTX_set_client_CA_list(FSslCTX, Sk); // frees Sk
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.54, set TLS protocol and security level, before loading certs }
procedure TSslContext.SetProtoSec;
var
    LOpts, NewOpts, CTLen: Integer;
    Opt2: TSslOption2;   { V8.51 }
    CTArray: TBytes;  { V9.5 }

{$IFNDEF NO_DEBUG_LOG}
    function GetMaskBits(Value: Cardinal): string;
    var
        MyOpts: LongWord;  { V8.65 stop overflow }
        I: Integer;
    begin
        result := '';
        MyOpts := 1;
        for I := 0 to 30 do begin
            if (Value and MyOpts) <> 0 then begin
                result := result + IntToHex(MyOpts, 8) + ', ';
            end;
            MyOpts := MyOpts * 2;
        end;
    end;
{$ENDIF}

    procedure BuildCTArray(CertTypes: TTlsCertTypes);   { V9.5 build byte array for CTX_set1_client/server_cert_type }

        procedure AddOne(ExtVal: Byte);
        begin
            CTLen := CTLen + 1;
            SetLength(CTArray, CTLen);
            CTArray[CTLen-1] := ExtVal;
        end;

    begin
        SetLength(CTArray, 0);
        CTLen := 0;
        if CertTypeX509 in CertTypes then
            AddOne(TLSEXT_cert_type_x509);
        if CertTypeRPK in CertTypes then
            AddOne(TLSEXT_cert_type_rpk);
    end;

begin
    SSL_CTX_set_security_level(FSslCtx, Ord(FSslSecLevel));
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V8.40 }
        DebugLog(loSslInfo, 'Set Security Level: ' + GetEnumName(TypeInfo(TSslSecLevel), Ord(FSslSecLevel)));
{$ENDIF}

// V8.52 build options bit mask from Delphi sets
    LOpts := 0;
    if (FSslOptions2 <> []) then begin  { V8.66 }
        for Opt2 := Low(TSslOption2) to High(TSslOption2) do begin
            if Opt2 in FSslOptions2 then
                LOpts := LOpts or SslIntOptions2[Opt2];
        end;
    end;

  // V8.27 set TLS min and max versions for OpenSSL 1.1.0 and later }
    if FSslMaxVersion >= FSslMinVersion then begin
        SSL_CTX_set_min_proto_version(FSslCtx, SslVerMethods [FSslMinVersion]);
        SSL_CTX_set_max_proto_version(FSslCtx, SslVerMethods [FSslMaxVersion]);
    end;

{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V8.40 }
        DebugLog(loSslInfo, 'SslMinVersion: ' +  GetEnumName(TypeInfo(TSslVerMethod),
                 Ord(FSslMinVersion)) + ', SslMaxVersion: ' +
                        GetEnumName(TypeInfo(TSslVerMethod), Ord(FSslMaxVersion)));
{$ENDIF}

    { Adds the options set via bitmask to Ctx, leaving defaults alone }
    NewOpts := SSL_CTX_set_options(FSslCtx, LOpts);   { V8.51 }
    if NewOpts <> LOpts then begin             { V8.51 check it worked }
        if (NewOpts = 0) and (Lopts <> 0) then
            raise ESslContextException.Create('Failed to set context options');
    end;

 { TLSv1.2 ciphers }
    if FSslCipherList <> '' then begin
        if SSL_CTX_set_cipher_list(FSslCtx, PAnsiChar(AnsiString(FSslCipherList))) = 0 then
            RaiseLastOpenSslError(ESslContextException, TRUE, 'Error loading cipher list');
    end
    else
        raise ESslContextException.Create('Cipher list empty');

 { V9.3 TLSv1.3 ciphers }
    if FSslCipherList <> '' then begin
        if SSL_CTX_set_ciphersuites(FSslCtx, PAnsiChar(AnsiString(FSslCipherList13))) = 0 then
            RaiseLastOpenSslError(ESslContextException, TRUE, 'Error loading TLSv13 cipher suites');
    end;

 { V9.3 set crypto curve groups allowed }
 { currently ignoring error caused by illegal group names, should really validate names before setting them
   or generate a warning instead of a terminal error }
    if FSslCryptoGroups <> '' then begin
        SSL_CTX_set1_groups_list(FSslCtx, PAnsiChar(AnsiString(FSslCryptoGroups)));
      //  if SSL_CTX_set1_groups_list(FSslCtx, PAnsiChar(AnsiString(FSslCryptoGroups))) = 0 then
      //      RaiseLastOpenSslError(ESslContextException, TRUE, 'Error loading crytpo group list: ' + FSslCryptoGroups);
    end;

  { V9.5 set certificate types accepted for clients and servers, to recognise Raw Public Keys (RPK), OpenSSL 3.2 and later }
    if ICS_OPENSSL_VERSION_NUMBER >= OSSL_VER_3200 then begin
        if FCliCertTypes <> [] then begin
            BuildCTArray(FCliCertTypes);
            if CTLen > 0 then begin
                if SSL_CTX_set1_client_cert_type(FSslCtx, @CTArray[0], CTLen) = 0 then
                    RaiseLastOpenSslError(ESslContextException, TRUE, 'Error setting client cert types');
            end;
        end;
        if FSrvCertTypes <> [] then begin
            BuildCTArray(FSrvCertTypes);
            if CTLen > 0 then begin
                if SSL_CTX_set1_server_cert_type(FSslCtx, @CTArray[0], CTLen) = 0 then
                    RaiseLastOpenSslError(ESslContextException, TRUE, 'Error setting server cert types');
            end;
        end;
    end;

{$IFNDEF NO_DEBUG_LOG}
        { V8.28 list all options }
            if CheckLogOptions(loSslInfo) then begin
                DebugLog(loSslInfo, 'SSL Options, Requested: ' +  GetMaskBits(LOpts));
                DebugLog(loSslInfo, 'SSL Options, Actual: ' +  GetMaskBits(NewOpts));  { V8.51 and what was set }
        { V8.27 list all ciphers available for connection }
                DebugLog(loSslInfo, 'SSL Ciphers Available: ' + #13#10 + SslGetAllCiphers);
           end;
{$ENDIF}
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.56 set application layer protocols supported, clients only }
procedure TSslContext.UpdateAlpnProtocols;
var
    buffer: TBytes;
    bufflen: Integer;
begin
    if FSslAlpnProtoList.Count = 0 then
        Exit;
    if NOT Assigned(FSslCtx) then
        Exit;
    IcsStrListToWireFmt(FSslAlpnProtoList, buffer);
    bufflen := Length(buffer);
    if bufflen = 0 then Exit;
    if SSL_CTX_set_alpn_protos(FSslCtx, @buffer[0], bufflen) <> 0 then   // warning reversed 0=ok
        RaiseLastOpenSslError(Exception, TRUE, 'Error setting alpn protos');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.56 keep application layer protocols supported, clients only }
{ V9.1 not sure if we can clear context ALPN once set, so added similar function to TSslWSocket }
procedure TSslContext.SetSslAlpnProtocols(ProtoList: TStrings);
begin
    if NOT Assigned(ProtoList) then
        Exit;
    if FSslAlpnProtoList.Text <> ProtoList.Text then
        FSslAlpnProtoList.Assign(ProtoList);
    if Assigned(FSslCtx) then
        UpdateAlpnProtocols;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.InitContext;
var
    SslSessCacheModes : TSslSessCacheModes;
//    MyECkey: PEC_KEY;
begin
    InitializeSsl; //loads libs
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
    {$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
         DebugLog(loSslInfo, 'Init SSL Context, OpenSSL version: ' + OpenSslVersion);
    {$ENDIF}
    {$IFNDEF OPENSSL_NO_ENGINE}

   {     if (not GSslRegisterAllCompleted) and FAutoEnableBuiltinEngines then
        begin
            // Register all of them for every algorithm they collectively implement /
            ENGINE_register_all_complete;
            GSslRegisterAllCompleted := TRUE;
        end;   }

    {$ENDIF}

        if not Assigned(FSslCtx) then begin
            // Create new context
            FSslCtx := InitializeCtx;
            if not Assigned(FSslCtx) then
                raise ESslContextException.Create('Failed to initialize context');
        end;

        try
            if Assigned(FOnBeforeInit) then
                FOnBeforeInit(Self);

         { V8.40 set security level for 1.1.0 and later
           rarely more than sslSecLevel80bits if backward compatibility needed }
            if FSslCliSecurity = sslCliSecIgnore then
                SetProtoSec  { V8.54 commonise code, security, protocol, cipher, options }
            else
                SetSslCliSec; { V8.54 sets FSslCliSecurity security, protocol, cipher, calls SetProtoSec }

          { V8.51 other context stuff we could set
              SSL_CTX_set_mode(ctx, SSL_MODE_ASYNC);
              SSL_CTX_set_max_send_fragment(ctx, max_send_fragment))
              SSL_CTX_set_split_send_fragment(ctx, split_send_fragment));
              SSL_CTX_set_max_pipelines(ctx, max_pipelines);
              SSL_CTX_set_default_read_buffer_len(ctx, read_buf_len);
              SSL_CTX_set_tlsext_max_fragment_length(ctx, maxfraglen);
          }

          { Load our key and certificate }

        {$IFNDEF OPENSSL_NO_ENGINE}

        {    if (FCtxEngine <> nil) and
                      (eccLoadPrivKey in FCtxEngine.CtxCapabilities) then
                LoadPKeyFromEngine(FCtxEngine)
            else begin    }

        {$ENDIF}
                // Set the password callback and our custom user data
                // V8.65 not needed
            //    SSL_CTX_set_default_passwd_cb(FSslCtx, PasswordCallBack);
            //    SSL_CTX_set_default_passwd_cb_userdata(FSslCtx, Self);

              { V8.27 load server private key from file or PEM string list }
              { V8.41 unless about to load it from FSslCertX509 }
                if NOT FSslCertX509.IsPKeyLoaded then begin
                    if (FSslPrivKeyLines.Count > 0) and (FSslPrivKeyFile = '') then
                        LoadPkeyFromString(FSslPrivKeyLines.Text)
                    else
                        LoadPKeyFromFile(FSslPrivKeyFile);
                end;
        {$IFNDEF OPENSSL_NO_ENGINE}
      //      end;
        {$ENDIF}

         { V8.27 load server certificate from file or PEM string list }
         { note this may include one or more intermediate certificates, or they may be in CAFile }
            if FSslCertX509.IsCertLoaded or FSslCertX509.IsInterLoaded then begin  { V8.41 load from FSslCertX509 }
                SslSetCertX509;
            end
            else begin
                if (FSslCertLines.Count > 0) and (FSslCertFile = '') then
                    LoadCertFromString(FSslCertLines.Text)
                else
                    LoadCertFromChainFile(FSslCertFile);
            end;

         { V8.27 load CA certificate from file or PEM string list }
          { V8.41 unless loaded it from FSslCertX509 }
            if NOT FSslCertX509.IsInterLoaded then begin
                if FUseSharedCAStore then begin     { V9.1 if using shared store }
                    if NOT IcsSslRootCAStore.InitFlag then   { V9.1 if internal not loaded, do it }
                        IcsSslRootCAStore.Initialise;
                    SSL_CTX_set1_cert_store(FSslCtx, IcsSslRootCAStore.X509Store);
                end
                else begin
                    if (FSslCALines.Count > 0) and (FSslCAFile = '') and (FSslCAPath = '') then
                        LoadCAFromString(FSslCALines.Text)
                    else
                        LoadVerifyLocations(FSslCAFile, FSslCAPath);
                end;
            end;

          { load certificate revocation lists (CRL) - need to set SslVerifyFlags to use them }
          { Warning not tested for years, if ever }
            LoadCRLFromFile(FSslCRLFile);
            LoadCRLFromPath(FSslCRLPath);

            // V8.51 Don't want any retries
            SSL_CTX_set_mode(FSslCtx, SSL_MODE_AUTO_RETRY);

            { V8.15 Diffie-Hellman key agreement protocol.
              DHparam file needed to generate DH and DHE keys, but not ECDH or ECDHE.
              V8.27 load DHParams from file or PEM string list, note FSslDHParamLines
                is defaulted with 4096 params so used if FSslDHParamFile blank  }
           { V8.62 only needed for servers, don't if using client security }
           { V8.66 DhParams only needed for old backward ciphers }
{$IFDEF OpenSSL_Deprecated}   { V9.5 }
            if FSslCliSecurity = sslCliSecIgnore then begin
                if (FSslDHParamLines.Count > 0) and (FSslDHParamFile = '') then
                    LoadDHParamsFromString(FSslDHParamLines.Text)
                else
                    LoadDHParamsFromFile(FSslDHParamFile);
             end;
 {$ENDIF OpenSSL_Deprecated}   { V9.5 }

            //raise Exception.Create('Test');

            // Now the verify stuff
            // V9.1 done again in InitSSLConnection where callback is set
            SetSslVerifyPeerModes(SslVerifyPeerModes);

            // Session caching stuff
            SslSessCacheModes := GetSslSessCacheModes;

            if SslSessCacheModes <> [] then   // AG 03/03/06 internal cache is ON by default
                SSL_CTX_set_session_cache_mode(FSslCtx, FSslSessCacheModeValue);

       { In TLSv1.3 NewSessionTicket messages arrive after the handshake and can
         come at any time. Therefore we use a callback to write out the session
         when we know about it. This approach works for < TLSv1.3 as well. }

            if not (sslSESS_CACHE_NO_INTERNAL_STORE in SslSessCacheModes) then begin
                { Exdata needed in RemoveCallback only }
                if SSL_CTX_set_ex_data(FSslCtx, 0, PAnsiChar(Self)) = 0 then
                    RaiseLastOpenSslError(ESslContextException, TRUE, 'SSL_CTX_set_ex_data failed');
                SSL_CTX_sess_set_remove_cb(FSslCtx, @RemoveSessionCallback);           { V8.66 }
                if FSslSessionCacheSize <> SSL_SESSION_CACHE_MAX_SIZE_DEFAULT then
                    SSL_CTX_sess_set_cache_size(FSslCtx, FSslSessionCacheSize);
            end;
           { V8.55 callback for both client and server }
           { Set the timeout for newly created sessions                }
            if FSslSessionTimeout > 0 then
                SSL_CTX_set_timeout(FSslCtx, FSslSessionTimeout);

          { V8.56 see if setting APLN Protocol for HTTP/2 or something, client only  }
            UpdateAlpnProtocols;

            if Length(FSslDefaultSessionIDContext) > 0 then
                if SSL_CTX_set_session_id_context(FSslCtx, @FSslDefaultSessionIDContext[1],
                                                                          Length(FSslDefaultSessionIDContext)) = 0 then
                    RaiseLastOpenSslError(ESslContextException, TRUE, 'ssl_ctx_set_session_id_context failed');
{$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslInfo) then  { V8.40 }
                DebugLog(loSslInfo, 'Set SSL Session Cache Mode');   { V8.65 reworeded }
{$ENDIF}

     { V9.1 set callbacks in TCustomSslWSocket.SetSslCallbacks }
     (*       { Set session callbacks, ssl server mode only }
            SSL_CTX_sess_set_new_cb(FSslCtx, @NewSessionCallback);   { V8.66 }
            SSL_CTX_sess_set_get_cb(FSslCtx, @GetSessionCallback);   { V8.66 }

         { V8.64 OpenSSL 1.1.1 and later server uses client_hello callback instead of ServerName }
            SSL_CTX_set_client_hello_cb(FSslCtx, @ClientHelloCallBack, Self);   { V8.66 }
     *)
          { V8.56 set application layer protocol select callback, servers only }
       //     SSL_CTX_set_alpn_select_cb(FSslCtx, @AlpnSelectCallBack, Self);  { V8.66 }

          { V8.69 OCSP stapling status requested, check certificate still valid }
          { on client we receive status in callback, on server we sent cached status }
      (*     if FSslOcspStatus then begin
                SSL_CTX_set_tlsext_status_type(FSslCtx, TLSEXT_STATUSTYPE_ocsp);
                if SSL_CTX_set_tlsext_status_cb(FSslCtx, @TlsStatusCallback) = 0 then
                    RaiseLastOpenSslError(ESslContextException, TRUE, 'SSL_CTX_set_tlsext_status_cb failed');
                SSL_CTX_set_tlsext_status_arg(FSslCtx, Self);
            end;
      *)
        except
            if Assigned(FSslCtx) then begin
                SSL_CTX_free(FSslCtx);
                FSslCtx := nil;
            end;
            raise
        end;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.DeInitContext;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if Assigned(FSslCtx) then begin
            { The context lives as long as there are open sessions associated }
            { even when we called SSL_CTX_free(), so some cleanup is needed }
            SSL_CTX_set_ex_data(FSslCtx, 0, nil);    //MainFix    // AG 12/25/07
            { It may be a good idea to disable all callbacks as well }
            { before freeing the context pointer, should not hurt,   }
            { otherwise please let me know }
            SSL_CTX_sess_set_remove_cb(FSslCtx, nil);             // AG 12/25/07
            SSL_CTX_sess_set_new_cb(FSslCtx, nil);                // AG 12/25/07
            SSL_CTX_sess_set_get_cb(FSslCtx, nil);                // AG 12/25/07
            SSL_CTX_set_tlsext_status_cb(FSslCtx, Nil);             { V8.69 }
            SSL_CTX_free(FSslCtx);
            FSslCtx := nil;
        end;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
    FinalizeSsl;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCAFile(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if IcsCompareStr(FSslCAFile, Value) = 0 then
            Exit;
        FSslCAFile := Value;
        if Assigned(FSslCtx) and (FSslCAFile <> '') then  { V8.27 }
            LoadVerifyLocations(FSslCAFile, FSslCAPath);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCALines(Value: TStrings);    { V8.27 }
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if IcsCompareStr(FSslCALines.Text, Value.Text) = 0 then
            Exit;
        FSslCALines.Assign(Value);
        if Assigned(FSslCtx) and (FSslCALines.Count > 0) and
            (FSslCAFile = '') and (FSslCAPath = '') then
                  LoadCAFromString(FSslCALines.Text);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCAPath(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if IcsCompareStr(FSslCAPath, Value) = 0 then
            Exit;
        FSslCAPath := Value;
        if Assigned(FSslCtx) and (FSslCAPath <> '') then  { V8.27 }
            LoadVerifyLocations(FSslCAFile, FSslCAPath);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCertFile(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if IcsCompareStr(Value, FSslCertFile) = 0 then
            Exit;
        FSslCertFile := Value;
        if (FSslCertFile <> '') and
                Assigned(FSslCtx) then begin
                    LoadCertFromChainFile(FSslCertFile);
                end;

{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCertLines(Value: TStrings);    { V8.27 }
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if IcsCompareStr(Value.Text, FSslCertLines.Text) = 0 then
            Exit;
        FSslCertLines.Assign(Value);
        if (FSslCertFile = '') and
          (FSslCertLines.Count > 0) and
              Assigned(FSslCtx) then
                 LoadCertFromString(FSslCertLines.Text);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslDHParamFile(const Value : String);    { V8.15 }
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslDHParamFile := Value;
{$IFDEF OpenSSL_Deprecated}   { V9.5 }
        if (FSslDHParamFile <> '') and Assigned(FSslCtx) then
           LoadDHParamsFromFile(FSslDHParamFile);   { V8.27 }
{$ENDIF OpenSSL_Deprecated}   { V9.5 }
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslDHParamLines(Value : TStrings);     { V8.27 }
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslDHParamLines.Assign(Value);
{$IFDEF OpenSSL_Deprecated}   { V9.5 }
       if (FSslDHParamLines.Count > 0) and
         (FSslDHParamFile = '') and Assigned(FSslCtx) then
             LoadDHParamsFromString(FSslDHParamLines.Text);
{$ENDIF OpenSSL_Deprecated}   { V9.5 }
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCRLFile(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslCRLFile := Value
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCRLPath(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslCRLPath := Value
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslPassPhrase(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslPassPhrase := Value
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslPrivKeyFile(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if (IcsCompareStr(Value, FSslPrivKeyFile) = 0) then
            Exit;
        FSslPrivKeyFile := Value;
        if (FSslPrivKeyFile <> '') and
          Assigned(FSslCtx) then
            LoadPKeyFromFile(FSslPrivKeyFile);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslPrivKeyLines(Value: TStrings);      { V8.27 }
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if (IcsCompareStr(Value.Text, FSslPrivKeyLines.Text) = 0) then
            Exit;
        FSslPrivKeyLines.Assign(Value);
        if (FSslPrivKeyFile = '') and
          (FSslPrivKeyLines.Count > 0) and
             Assigned(FSslCtx) then
                LoadPKeyFromString(FSslPrivKeyLines.Text);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslSessionCacheSize(Value: Integer);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslSessionCacheSize := Value
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslSessionTimeout(Value: Cardinal);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslSessionTimeout := Value
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslVersionMethod(Value: TSslVersionMethod);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslVersionMethod := Value
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslMinVersion(Value : TSslVerMethod);   { V8.27 }
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslMinVersion := Value;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslMaxVersion(Value : TSslVerMethod);   { V8.27 }
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslMaxVersion := Value
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslECDHMethod(Value : TSslECDHMethod);    { V8.15 }
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslECDHMethod := Value;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslSessCacheModes(Value: TSslSessCacheModes); { V7.30 }
var
    SessMode: TSslSessCacheMode;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslSessCacheModeValue := SSL_SESS_CACHE_OFF;
        for SessMode := Low(TSslSessCacheMode) to High(TSslSessCacheMode) do
            if SessMode in Value then
                FSslSessCacheModeValue := FSslSessCacheModeValue or SslIntSessCacheModes[SessMode];
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslContext.GetSslSessCacheModes: TSslSessCacheModes; { V7.30 }
var
    SessMode: TSslSessCacheMode;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        Result := [];
        for SessMode := Low(TSslSessCacheMode) to High(TSslSessCacheMode) do
            if FSslSessCacheModeValue and SslIntSessCacheModes[SessMode] <> 0 then
                Include(Result, SessMode);

{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslContext.GetSslVerifyFlags: TSslVerifyFlags;
var
    VFlag: TSslVerifyFlag;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        Result := [];
        for VFlag := Low(TSslVerifyFlag) to High(TSslVerifyFlag) do
            if (FSslVerifyFlagsValue and SslIntVerifyFlags[VFlag]) <> 0 then    { V9.1 added raw }
                Include(Result, VFlag);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslVerifyFlags(
  const Value: TSslVerifyFlags);
var
    VFlag: TSslVerifyFlag;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslVerifyFlagsValue := 0;
        for VFlag := Low(TSslVerifyFlag) to High(TSslVerifyFlag) do
            if VFlag in Value then
               FSslVerifyFlagsValue := FSslVerifyFlagsValue or SslIntVerifyFlags[VFlag];  { V9.1 added raw }
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.54  simplify setting SSL client security by using common levels
         which set protocols, security and ciphers  }
procedure TSslContext.SetSslCliSec;
begin
    if FSslCliSecurity = sslCliSecIgnore then Exit;
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslMinVersion := sslVerTLS1;
        FSslMaxVersion := sslVerMax;
        FSslCipherList := 'ALL';
        FSslSecLevel := sslSecLevelAny;
        case FSslCliSecurity of
            sslCliSecNone: begin        { all protocols, any key lengths }
              FSslMinVersion := sslVerSSL3;
            end;
            sslCliSecSsl3Only: begin    { SSL3 only, any key lengths, MD5 }
              FSslMinVersion := sslVerSSL3;
              FSslMaxVersion := sslVerSSL3;
            end;
            sslCliSecTls1Only: begin   { TLS1 only }
              FSslMinVersion := sslVerTLS1;
              FSslMaxVersion := sslVerTLS1;
              FSslSecLevel := sslSecLevel112bits;
            end;
            sslCliSecTls11Only: begin   { TLS1.1 only }
              FSslMinVersion := sslVerTLS1_1;
              FSslMaxVersion := sslVerTLS1_1;
              FSslSecLevel := sslSecLevel112bits;
            end;
            sslCliSecTls12Only: begin   { TLS1.2 only }
              FSslMinVersion := sslVerTLS1_2;
              FSslMaxVersion := sslVerTLS1_2;
              FSslSecLevel := sslSecLevel112bits;
            end;
            sslCliSecTls13Only: begin   { TLS1.3 only }
              FSslMinVersion := sslVerTLS1_3;
              FSslMaxVersion := sslVerTLS1_3;
              FSslSecLevel := sslSecLevel112bits;
            end;
            sslCliSecTls1: begin   { TLS1 or later }
              FSslMinVersion := sslVerTLS1;
              FSslSecLevel := sslSecLevel80bits;
            end;
            sslCliSecTls12: begin   { TLS1.2 or later }
              FSslMinVersion := sslVerTLS1_2;
              FSslSecLevel := sslSecLevel112bits;
            end;
            sslCliSecBack: begin   { TLS1 or later, backward ciphers, RSA/DH keys=>1024, ECC=>160, no MD5, SHA1 }
              FSslMinVersion := sslVerTLS1;
//              FSslCipherList := AddTls13(sslCiphersMozillaSrvBack);   { V9.3 now set separately }
              FSslSecLevel := sslSecLevel80bits;
            end;
            sslCliSecInter: begin   { TLS1.1 or later, backward ciphers, RSA/DH keys=>1024, ECC=>160, no MD5, SHA1 }
              FSslMinVersion := sslVerTLS1_1;  { V8.55 }
//              FSslCipherList := AddTls13(sslCiphersMozillaSrvBack);
              FSslSecLevel := sslSecLevel112bits;
            end;
            sslCliSecHigh: begin    { TLS1.2 or later, high ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
              FSslMinVersion := sslVerTLS1_2;
//              FSslCipherList := AddTls13(sslCiphersMozillaSrvTLS12);
              FSslSecLevel := sslSecLevel112bits;
            end;
            sslCliSecHigh128: begin { TLS1.2 or later, high ciphers, RSA/DH keys=>3072, ECC=>256, FS forced }
              FSslMinVersion := sslVerTLS1_2;
//              FSslCipherList := AddTls13(sslCiphersMozillaSrvTLS12);
              FSslSecLevel := sslSecLevel128bits;
            end;
            sslCliSecHigh192: begin { TLS1.2 or later, high ciphers, RSA/DH keys=>7680, ECC=>384, FS forced }
              FSslMinVersion := sslVerTLS1_2;
//              FSslCipherList := AddTls13(sslCiphersMozillaSrvTLS12);
              FSslSecLevel := sslSecLevel192bits;
            end;
        end;
        if GetIsCtxInitialized then
            SetProtoSec;  { set security, protocol, options }

{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCliSecurity(Value: TSslCliSecurity);                { V8.54 }
begin
    if Value = FSslCliSecurity then Exit;
    FSslCliSecurity := Value;
    SetSslCliSec;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslContext.GetSslCheckHostFlags: TSslCheckHostFlags;                 { V8.39 }
var
    VFlag: TSslCheckHostFlag;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        Result := [];
        if FSslCheckHostFlagsValue = -1 then begin
            Result := [sslX509_NO_HOST_CHECK];
            Exit;
        end;
        for VFlag := Low(TSslCheckHostFlag) to High(TSslCheckHostFlag) do
            if (FSslCheckHostFlagsValue and SslIntCheckHostFlags[VFlag]) <> 0 then
                Include(Result, VFlag);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCheckHostFlags(const Value: TSslCheckHostFlags);    { V8.39 }
var
    VFlag: TSslCheckHostFlag;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslCheckHostFlagsValue := 0;
        if sslX509_NO_HOST_CHECK in Value then begin
            FSslCheckHostFlagsValue  := -1;
            Exit;
        end;
        for VFlag := Low(TSslCheckHostFlag) to High(TSslCheckHostFlag) do
            if VFlag in Value then
               FSslCheckHostFlagsValue := FSslCheckHostFlagsValue or SslIntCheckHostFlags[VFlag];
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCipherList(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if FSslCipherList = Value then
            Exit;   // No change, do nothing
        FSslCipherList := Value;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCipherList13(const Value: String);    { V9.3 }
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if FSslCipherList13 = Value then
            Exit;   // No change, do nothing
        FSslCipherList13 := Value;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslVerifyPeerModes(const Value: TSslVerifyPeerModes);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if Value <> FSslVerifyPeerModes then begin
            FSslVerifyPeerModesValue := 0;
            if (SslVerifyMode_NONE in Value) then
                FSslVerifyPeerModesValue := FSslVerifyPeerModesValue or SSL_VERIFY_NONE;
            if (SslVerifyMode_PEER in Value) then
                FSslVerifyPeerModesValue := FSslVerifyPeerModesValue or SSL_VERIFY_PEER;
            if (SslVerifyMode_FAIL_IF_NO_PEER_CERT in Value) then
                FSslVerifyPeerModesValue := FSslVerifyPeerModesValue or SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
            if (SslVerifyMode_CLIENT_ONCE in Value) then
                FSslVerifyPeerModesValue := FSslVerifyPeerModesValue or SSL_VERIFY_CLIENT_ONCE;
            FSslVerifyPeerModes := Value;
        end;

        if (not Assigned(FSslCtx)) or (not Assigned(fVerifyCallbackPtr)) then  { V9.1 Ptr set in wsocket }
            Exit;

        { We may change these settings any time since they won't change active Ssl's }
        if FSslVerifyPeer then begin
            if SSL_CTX_get_verify_mode(FSslCtx) <> FSslVerifyPeerModesValue then begin
          //      SSL_CTX_set_verify(FSslCtx, FSslVerifyPeerModesValue, @PeerVerifyCallback);   { V8.66 }
                SSL_CTX_set_verify(FSslCtx, FSslVerifyPeerModesValue, fVerifyCallbackPtr);   { V9.1 Ptr set in wsocket }
                SSL_CTX_set_verify_depth(FSslCtx, FSslVerifyDepth);
            end;
        end
        else begin
            SSL_CTX_set_verify(FSslCtx, 0, nil);
            SSL_CTX_set_verify_depth(FSslCtx, 0);
        end;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslVerifyPeer(const Value: Boolean);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if Value <> FSslVerifyPeer then begin
            FSslVerifyPeer := Value;
            SetSslVerifyPeerModes(FSslVerifyPeerModes);
        end;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslDefaultSessionIDContext(
    Value: TSslSessionIdContext);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if Length(Value) > SSL_MAX_SSL_SESSION_ID_LENGTH then
            SetLength(Value, SSL_MAX_SSL_SESSION_ID_LENGTH);
        if FSslDefaultSessionIDContext <> Value then begin
            FSslDefaultSessionIDContext := Value;
            if Assigned(FSslCtx) and (SSL_SESS_CACHE_SERVER and
               FSslSessCacheModeValue <> 1) then begin
                if Length(Value) > 0 then
                    SSL_CTX_set_session_id_context(FSslCtx, @Value[1], Length(Value))
                else
                    SSL_CTX_set_session_id_context(FSslCtx, nil, 0);
            end;
        end;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.27 list all ciphers for current SSL context which must be initialised }
{ seems to return all ciphers irrespective of whether supported by protocols }
function TSslContext.SslGetAllCiphers: String;
var
    Next: PAnsiChar;
    Priority: Integer;
    MySsl: PSSL;
begin
    Result := '';
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
  //Create temporary SSL Object
    MySsl := SSL_new(FSslCtx);
    if not Assigned(MySsl) then
        RaiseLastOpenSslError(Exception, TRUE, 'Error on creating the Ssl object');
    Priority := 0;
    while True do begin
        Next := SSL_get_cipher_list(MySsl, Priority);
        if Next = Nil then Break;
        Inc (Priority);
        if Priority > 100 then Break; // sanity check
        Result := Result + String(Next) + #13#10;
    end;
    SSL_free(MySsl);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.39 list all certificates saved in the context CA store, note this
  excludes the server or client certtificate  }
function TSslContext.SslGetAllCerts (CertList: TX509List): integer;
var
    MyStack: PStack;
    I: integer;
    MyX509Obj: PX509_OBJECT;
begin
    Result := 0;
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if NOT Assigned(CertList) then exit;
    CertList.Clear;
    MyStack := X509_STORE_get0_objects(SSL_CTX_get_cert_store(FSslCtx));
    Result := OPENSSL_sk_num(MyStack);   { !!! don't free stack }
    if Result = 0 then Exit;
    for I := 0 to Result - 1 do begin
        MyX509Obj := PX509_OBJECT(OPENSSL_sk_value(MyStack, I));
        if X509_OBJECT_get_type(MyX509Obj) = X509_LU_X509 then
           CertList.Add({f_X509_dup(}X509_OBJECT_get0_X509 (MyX509Obj));  { V8.64 memory leak }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.69 get pointer to context CA store, used for OCSP }
function TSslContext.GetX509Store: PX509_STORE;
begin
    Result := Nil;
    if not Assigned(FSslCtx) then Exit;
    Result := SSL_CTX_get_cert_store(FSslCtx);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.1 get total certificates in CA Store, so we know if it's empty and need to load it }
function TSslContext.GetCAStoreTotal: Integer;
var
    Store: PX509_STORE;
    MyStack: PStack;
begin
    Result := 0;
    Store := GetX509Store;
    if NOT Assigned(Store) then
        Exit;
    MyStack := X509_STORE_get0_objects(Store);
    Result := OPENSSL_sk_num(MyStack);   { !!! don't free stack }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.40 for servers, build certificate chain for current context certificate  }
{ use SSL_BUILD_CHAIN_FLAG_xx flags OR'd to control verification, 1=OK }
{ flag SSL_BUILD_CHAIN_FLAG_IGNORE_ERROR will return 2 on error instead of exception }
{ !! WARNING - may stop intermediates being sent if error returned }
function TSslContext.SslBuildCertChain (Flags: Integer): integer;
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    result := SSL_CTX_build_cert_chain(FSslCtx, Flags);
    if result = 0 then RaiseLastOpenSslError(Exception, TRUE, 'Failed to build certificate chain');
end;



{$ENDIF USE_SSL}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
{$IFDEF USE_SSL}
    SslCritSect := TIcsCriticalSection.Create;
    {$IFNDEF NO_SSL_MT}
        LockPwdCB         := TIcsCriticalSection.Create;
        LockVerifyCB      := TIcsCriticalSection.Create;
        LockInfoCB        := TIcsCriticalSection.Create;
        LockRemSessCB     := TIcsCriticalSection.Create;
        LockNewSessCB     := TIcsCriticalSection.Create;
        LockGetSessCB     := TIcsCriticalSection.Create;
        LockClientCertCB  := TIcsCriticalSection.Create;
        LockServerNameCB  := TIcsCriticalSection.Create;
    {$ENDIF}
    IcsSslRootCAStore := TSslRootCAStore.Create(Nil);            { V9.1 common Root CA Store }

//  if NOT GSSL_LOAD_OPENSSL_MYSELF then begin    { V9.3 not sure how to set this before unit is created }
        {$IFDEF OpenSSL_Check_Signed}         { V9.3 }
            GSSL_SignTest_Check := True;        // check digitally signed
        {$ENDIF}
        {$IFDEF OpenSSL_Check_SignCert}       { V9.3 }
            GSSL_SignTest_Certificate := True;  // check digital certificate
        {$ENDIF}
        {$IFDEF OpenSSL_AutoLoad_CA_Bundle}
            try
                IcsSslRootCAStore.Initialise;   { loads OpenSSL and certificate bundle }
            except;   { V9.3 ignore exceptions }
            end;
        {$ENDIF}
//   end;

finalization
    FreeAndNil(IcsSslRootCAStore);                                   { V9.1 }
    {$IFNDEF NO_SSL_MT}
        FreeAndNil(LockPwdCB);
        FreeAndNil(LockVerifyCB);
        FreeAndNil(LockInfoCB);
        FreeAndNil(LockRemSessCB);
        FreeAndNil(LockNewSessCB);
        FreeAndNil(LockGetSessCB);
        FreeAndNil(LockClientCertCB);
        FreeAndNil(LockServerNameCB);
    {$ENDIF}
    FreeAndNil(SslCritSect);
    IcsUnloadSsl;
{$ENDIF USE_SSL}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


end.
