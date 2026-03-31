{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  A TWSocket that has server functions: it listen to connections
              an create other TWSocket to handle connection for each client.
Creation:     Aug 29, 1999
Updated:      Sep 2025
Version:      V9.5
EMail:        francois.piette@overbyte.be     http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1999-2025 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>
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
Sep 05, 1999 V1.01 Adpted for Delphi 1
Oct 09, 1999 V1.02 Added intermediate class TCustomWSocket
Nov 12, 1999 V1.03 Added OnClientCreate event just after client component has
                   been created.
Apr 02, 2000 V1.04 Added FSessionClosedFlag to avoid double SessionClosed
                   event triggering
Apr 13, 2002 V1.05 When sending banner to client, add LineEnd instead of CR/LF
                   as suggested by David Aguirre Grazio <djagra@xaire.com>
Sep 13, 2002 V1.06 Check if Assigned(Server) in TriggerSessionClosed.
                   Reported by Matthew Meadows <matthew.meadows@inquisite.com>
Sep 16, 2002 V1.07 Fixed a Delphi 1 issue in TriggerSessionClosed where
                   property was used in place of field variable.
Jan 04, 2003 V1.08 Renamed BannerToBusy to BannerTooBusy. This will cause
                   trouble in applications already using this property. You
                   have to rename the property in your app !
Jan 24, 2003 V5.00 Skipped to version 5 because of SSL code
Jan 26, 2004 V5.01 Introduced ICSDEFS.INC and reordered uses for FPC
                   compatibility.
May 01, 2004 V5.02 WMClientClosed was incorrectly referencing global Error
                   variable instead of the real winsock error code. Now pass
                   the errcode in WParam at the time of PostMessage.
                   Removed Forms and Graphics units from the uses clause.
May 23, 2005 V5.03 Added intermediate variable NewHSocket in procedure
                   TriggerSessionAvailable
Dec 30, 2005 V6.00b A.Garrels added IcsLogger
Jan 06, 2008 V6.01 Angus added Disconnect(Client) and DisconnectAll
May 01, 2008 V6.02 A. Garrels - Function names adjusted according to changes in
                   OverbyteIcsLibrary.pas.
May 14, 2008 V6.03 A. Garrels - Type change from String to AnsiString in
                   TWSocketClient (FPeerPort and FPeerAddr).
Aug 11, 2008 V6.04 A. Garrels - Type AnsiString rolled back String.
Nov 6,  2008 V7.00 Angus added CliId property used to ensure correct client freed
                    (did not call it ID to avoid conflicts with existing clients)
Aug 8,  2010 V7.01 FPiette enhanced TriggerSessionAvailable so catch exception
                   in client class constructor and ClientCreate, and close the
                   remote socket in that case.
Feb 4,  2011 V7.02 Angus added bandwidth throttling using TCustomThrottledWSocket.
                   Client sockets inherit server settings for BandwidthLimit and
                   BandwidthSampling, but these can be changed ideally in
                   OnClientCreate event, but also in OnClientConnect but note a
                   timer may have been started by then so better to default to
                   BandwidthLimit=0 and set it, than to disable it.
Apr 15, 2011 V7.03 Arno prepared for 64-bit.
May 13, 2011 V7.04 Anton S. found a small issue with CliId.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Jul 21, 2012 V8.01 Fix in TCustomMultiListenWSocketServer.TriggerClientConnect.
Jun 03, 2013 V8.02 FPiette added unit "Types" so that some inlines are
                   expanded.
Jun 09, 2013 V8.03 FPiette WMClientClosed clear CliId before freeing.
Aug 18, 2013 V8.04 Arno - It was not possible to clear both string properties
                   Banner and BannerTooBusy in OI since empty strings were not
                   stored in the .dfm.
Aug 18, 2013 V8.05 Arno added some default property specifiers.
Mar 10, 2015 V8.06 Angus CloseDelayed when too many clients so closes cleanly
Mar 23, 2015 V8.07 Angus onSslServerName and OnBgException events set for clients
Oct 26, 2016 V8.36 Angus TWSocketMultiListenItem now has SslEnable moved from SSL class
                   Added extended exception information
Nov 9,  2016  V8.37 Client inherits server SocketErrs
Apr 11, 2017  V8.45 Added multiple SSL host support to TSslWSocketServer.
                    There is a new IcsHosts property which allows multiple hosts
                      to be specified, each with one or two IP addresses and
                      non-SSL and SSL port bindings, SSL certificates and private
                      key, SSL context and security level, and other web server
                      host related properties (not used here).
                    If IcsHosts is specified, TSslWSocketServer ignores existing
                      bindings and SSLContext, and creates new bindings and
                      initialises an SSL context for each host checking and
                      reporting all server certificate chains.
                    IcsHosts includes SslSrvSecurity SSL server security
                       level, that sets protocol, cipher and SslSecLevel according
                       to eight levels from sslSrvSecNone to sslSrvSecHigh192 to
                       avoid needing to set protocols and ciphers in SSL context.
                    IcsHosts includes SslCert, SslKey, SslPassword and SslInter.
                      SslCert may be a file name or a ASCII PEM string containing
                      an Ssl server certificate. The file may be PEM/CER/DER/PFX/P12/etc,
                      either a single certificate or bundle with private key and
                      any intermediates (PEM/PFX/P12 only), SslPassword is the private
                      key password if encrypted.  If a bundle is not found, SslKey is
                      a PEM file or ASCII PEM private key, and SslInters is any
                      intermediate SSL certificates used to sign the server SSL cert
                      as a file name in PEM/CER/DER format or ASCII PEM.
                    TSslWSocketServer has new RootCA property which should be loaded
                      with CA Root certificates to validates the server chain and
                      new DHParams property for a context DHParams file or string.
                    The ValidateHosts property checks all host properties and
                      attempts to validate SSL certificate chains and hosts,
                      it keeps certificate chain information in the IcsHosts CertInfo
                      property and validation warnings or error in ErrInfo.
                    TWSocketClient includes new properties IcsHostIdx, MultiListenIdx
                      and HostTag to identify which IcsHost and MultiListen socket
                      accepted the connection.  Published client server and remote peer
                      address and port as CServerAddr, CServerPort, CPeerAddr and
                      CPeerPort since many clients need this information.
Apr 20, 2017  V8.46 New RootCA property is now a String (filename or base84 string)
                    Improved IcsHost.GetDisplayName a little
                    Set IcsLogger for SSL context
May 16, 2017  V8.47 IcsHosts keeps file time stamps of SSL certs to check if
                      changed, and BindInfo with reportable bindings.
                    Added IcsLoadIcsHostsFromIni function which loads IcsHosts from
                      an open INI file to simplify application creation.
                    Fixed IcsLogger conditional.
May 24, 2017  V8.48 ValidateHosts has options to return all errors as a string
                      instead of raising an exception on the first error.  The idea
                      is that some hosts may still work, even if one or more SSL
                      certificates are bad.
                    Added RecheckSslCerts which shoulld be called at least once a
                      day (after midnight) to check if new SSL certificates are
                      available and if old ones have expired.
                    Added ListenAllOK which returns true if all sockets are listening
                      OK, note starting a multilistener server does not give errors
                      if some listeners fail due to port conflicts.
                    Added ListenStates which returns a multiline string listing the
                       IP, port, SSL and state of all socket listeners.
                    Updated Added IcsLoadIcsHostsFromIni to add web server properties.
June 23, 2017 V8.49 Fixes so we support MacOS again, thanks to Michael Berg.
                    Added more elements to IcsHosts for a .well-known path and
                      web redirection.
                    Added MultiListenEx which opens all possible sockets ignoring
                      errors, which are returned as a string.
Aug 10, 2017  V8.50 Minor clean up
Nov 22, 2017  V8.51 SSL certificate file stamp now stored as UTC date to avoid
                       summer time triggers as file system stamps change
Feb 14, 2018  V8.52 Better error reporting when validating SSL certificates
                    Add TLSv3 ciphers for OpenSSL 1.1.1 and later only
Jun 12, 2018  V8.55 sslSrvSecInter/FS now requires TLS1.1, PCI council EOF TLS1.0 30 June 2018
Jul 6, 2018   V8.56 Added OnSslAlpnSelect called after OnSslServerName for HTTP/2.
Oct 5, 2018   V8.57 Fixed bug so that a newly found SSL certificate is immediately
                      loaded to the context.
                    IcsHosts INI file now accepts enum string for SslSecLevel, ie
                       SslSecLevel=sslSrvSecHigh as well as sslSrvSecHigh=5 .
                    IcsHosts INI file prefers new HostEnabled to older Enabled,
                       avoids confusion with other Enabled properties.
                    Added SslCliCertMethod to allow server to request a client
                       SSL certificate from the browser, NOTE you should check it
                       the OnSslHandshakeDone event and close the connection if
                       invalid, beware this usually causes the browser to request
                       a certificate which can be obtrusive.
                    Allow SSL certificates to be ordered and installed automatically
                       by RecheckSslCerts if SslCertAutoOrder=True and so specified in
                       IcsHosts, if a TSslX509Certs component is attached and a
                       certificate supplier account has been created (by the
                       OverbyteIcsX509CertsTst sample application).
                    Added LoadOneCert which consolidates all certificate loading code
                       from ValidateHosts and RecheckSslCerts.
Oct 19, 2018  V8.58 Increased ListenBacklog property default to 15 to handle
                      higher server loads before rejecting new connections.
                    Documentation on IcsHosts and main components.
Dec 04, 2018  V8.59 Sanity checks reading mistyped enumerated values from INI file.
                    Updated ListenStates so unit compiles without USE_SSL
                    Added AUTO_X509_CERTS define set in OverbyteIcsDefs.inc which
                      can be disabled to remove a lot of units if automatic SSL/TLS
                      ordering is not required, saves up to 350KB of code.
Mar 18, 2019  V8.60 Added WebLogIdx to IcsHosts for web server logging.
                    Added sslSrvSecTls12Less and sslSrvSecTls13Only to disable
                       in server IcsHosts if TLS1.3 fails.
Aug 06, 2019  V8.62 When ordering X509 certificate, ChallFileSrv challenge now
                     uses separate local web server for servers not using ports
                      80 or 443 such as FTP, SMTP, proxies, etc.
                    If IcsHosts SslCert is not found but have a valid directory,
                      try default certificate file name based on Common Name instead,
                      ie www_domain_com.pfx.  Ideally application should check if
                      SslCert changes during ValidateHosts and update persistent
                      storage.
                    Moved BuildCertName here from X509Certs.
                    Added source to HandleBackGroundException so we know where
                        errors come from, when using IcsLogger.
Nov 18, 2019 V8.63 ValidateHosts, RecheckSslCerts and LoadOneCert have new
                     AllowSelfSign to stop errors with self signed certificates.
                   Automatic cert ordering now works if cert file name has -bundle
                     or -cert appended to end.
                   IcsHosts has new AuthSslCmd property for when SSL is allowed
                     on non-SSL port after AUTH SSL command or similar.
                   Automatic cert ordering now closes the account and local web
                     server automatically and after order errors, and when the
                     the server is stopped.
                   Automatic cert ordering now Logs activity via the SslX509Certs
                     component since this one does not have logging.
                   Corrected INI file reading NostEnabled instead of HostEnabled.
May 18, 2020 V8.64 IcsHosts allows NonSSlPort to be zero for random port, not for SSL.
                   Added BindPort read only property with real port while listening.
                   Added BindSrvPort to IcsHosts set while listening.
                   ListenStates now shows BindPort rather than requested port.
                   IcsHosts has new AuthForceSsl property, login only allowed once
                     SSL/TLS negotiated so credentials never sent clear.
                   Added sanity check to OrderClose.
                   Support ChallengeType=ChallAlpnApp for tls-alpn-01 SSL
                      ertificate challenge, client has new OnClientAlpnChallg
                      event that should provide the new self signed ACME certificate
                      for the challenge sent using a temporary SslContext.
                   Support CertChallenge=ChallFileApp for http challenge, needs
                       code adding in HTTP server onWellKnownDir event but avoids
                       writing files.
                   Removed ALPN handling added in V8.62 which was called too
                      late by OpenSSL to change the SSL context, now checking
                      ALPN earlier from new ClientHello callback.
                   Breaking change!  Internally replaced FRootCAX509 with FX509CAList
                      TX509List for use by ValidateCertChain as Root CA store,
                      specified by file RootCA, loaded by LoadRootCAList and
                      available as RootCAList.  Any applications directly calling
                      ValidateCertChain will need to load CA certificates into a
                      TX509List to pass to that function.
                   Create a self signed SSL certificate so server can start if
                      certificate file can not be found or is missing.  Needed
                      for ChallengeType=ChallAlpnApp to create a new certificate,
                      but always done to make life easier.  Tries to use the
                      specified certificate directory, TEMPDIR if none specified.
                   Don't start SSL certificate order until server is listening.
                   ACME tls-alpn-01 challenge context swapping now logged to
                      X509Certs log instead of debug log.
Dec 09, 2020 V8.65 FPiette made OverbyteIcsSslX509Utils unit loaded only when
                      USE_SLL is defined.
                   Changed Longint to Integer and LongWord to Cardinal to keep
                      MacOS64 happy since it thinks both are 64-bits.
                   Posix fixes.
                   Use X509Log for SSL SNI IcsHost lookup result so we know
                      the correct host has been found.
                   When checking IcsHosts for server name using AUTH command,
                     also check non-SSL ports.
                   Renamed Ics.Posix.Messages.pas to Ics.Posix.PXMessages.pas
Mar 22, 2021 V8.66 Removed support for OpenSSL 1.0.2 and 1.1.0 whose support ceased
                     Dec 2019, also Fips mode pending 3.0.
                   Added OnBeforeContextInit event which if set is called once before
                     each IcsHosts SslContext is initialised to allow the context
                     parameters to be adjusted for special ciphers or protocols.
                   In 2021 Mozilla now recommends TLSv1.3 as modern ciphers and TLSv1.2/1.3
                     as Intermediate supporting all browsers from last five years, so IcsHosts
                     now use sslCiphersMozillaSrvTLS12 as Intermediate level, also Mozilla
                     recommends no cipher server preference so changed that.
                   Updated SslSrvSecurity levels: sslSrvSecInter, sslSrvSecInterFS and
                     sslSrvSecHigh now all the same TLSv1.2 or 1.3, sslSrvSecTls12Less now
                     TLSv1.2 only, sslSrvSecSsl3 not supported, only sslSrvSecBack supports
                     TLSv1 and 1.1, sslSrvSecTls13Only unchanged TLSv1.3 only.
                   Better error handling when IcsHost SslCert is blank, so SslContext is
                      still created for self signed certificate, thanks to Linden Roth.
Sep 01, 2021 V8.67 OpenSSL 3.0 makes several old ciphers and digests legacy so default
                     for encrypting PFX/P12 files is now PrivKeyEncAES256 with 3.0
                     unless the legacy DLL is loaded when still PrivKeyEncTripleDES
                     so older versions of Windows can load them.
                    Set certificate order output format to OutFmtPwP12 if a password
                      is specified for ordering new certificates.
May 26, 2022 V8.69 Added OCSP (Online Certificate Status Protocol) support using the TOcspHttp
                     component to confirm server SSL/TLS certificates are legitimate and not
                     revoked for security reasons (which (Let's Encrypt did with two days
                     notice on 28 Jan 2022).  The certificate OCSP response is also stapled
                     to the initial SSL/TLS HELO handshake and sent to the client to avoid
                     it needing to lookup OCSP using HTTP itself. OCSP responses are cached
                     and saved to a file for reloading later, but are refreshed every time
                     the certificate is validated, at least once a day. The server property
                     OcspSrvStapling=True enables OCSP checks and stapling only with
                     AUTO_X509_CERTS define since it adds extra HTTP client code. A revoked
                     certificate will be auto ordered.  OCSP checking is done in LoadOneCert
                     and the stapled response sent in TriggerSslServerName when checking SNI.
Oct 10, 2022 V8.70 Removed some AnsiString casts.
Jul 06, 2023 V8.71 Clarify some local vars during certificate ordering.
                   Renamed BuildCertName to BuildCertFName.
                   Ensure inherited destroy called.
                   Increased default IcsHosts SslSrvSecurity to sslSrvSecHigh for TLS/1.2
                     minimum, with certificate key size 2,049 bits and SHA-256 digest.
                   Added CliCertMethod to IcsHosts to allow specific hosts to request a
                     client SSL/TLS certificate rather than all hosts (with SslCliCertMethod).
                     Note either client or server setting may request a client cert request.
                   Added SslLoadSource to IcsHosts to allow SSL/TLS certificates to be
                     loaded from the Windows Certificate Store instead of PEM/PXF files.
                     Defaults to CertLoadFile for files, CertWinStoreMachine or CertWinStoreUser
                     for Windows Stores, HostName[0] is then the common name, part friendly
                     name or a single host name to choose from the store, if there are
                     duplicates the longest expiry date will be chosen. If the certificate
                     name is a wildcard (*), this will be matched with any first node. Note
                     only certificates with exportable private keys will be selected, and the
                     application must have administrator rights to access the Local Machine
                     store.  Only supported if AUTO_X509_CERTS is defined, to avoid bringing
                     in extra units.  Automatic certificate ordering is supported provided
                     SslCert is supplied, and it will be installed into the Windows Store.
                     Although the Current User Store does not need admin rights it is less
                     useful for Windows services which need a login of that user.
Aug 08, 2023 V9.0  Updated version to major release 9.
Feb 25, 2024 V9.1  Added new property NoSSL that prevents use of SSL/TLS, must be set before
                     server is started.
                   TOcspHttp now in OverbyteIcsSslUtils rather than OverbyteIcsSslHttpRest to
                     ease linking.
                   Added OverbyteIcsSslBase which now includes TSslContext, TX509Base and TX509List.
                   Replaced FX509CAList with public IcsSslRootCAStore.
                   Moved BuildCertFName to SslBase.
                   When creating a local SSL/TLS certificate to allow a server to start, ICS
                     now creates a certificate with the IcsHosts.Hosts names signed by an
                     internal ICS intermediate 'ICS Intermediate Short' signed by 'ICS Root CA'
                     which if installed in Windows and browsers will stop certificate warnings
                     appearing.  Previously ICS only created self signed certificates.  The
                     global GSSL_INTER_FILE may be changed to an alternate intermediate bundle.
                     The ICS bundle has the password 'password' and a maximum 100 day life,
                     so new intermediates will be required regularly, to prevent misuse.
                   Use the function IcsInstallIcsRoot to install the ICS root certificate into
                     the Windows Root Store, needs admin rights for the Local Machine store.
                   Added property ListenAny returns true if any sockets are listening, ie server
                     is running.
                   If locally created certificate signed by ICS intermediate is about to expire,
                     replace it.
                   Replace locally created certificate signed by ICS intermediate by Let's
                     Encrypt certificate if automatic ordering enabled.
Sep 16, 2024  V9.3 Using OverbyteIcsTypes for consolidated types and constants, allowing
                     other import units to be removed.
                   Using define MSCRYPT_Servers instead of MSWINDOWS to define IcsHosts
                     can access server certificates in the Windows Store.
                   Ciphers for TLSv12 and TLSv13 now specified separately in SslContext.
                   IcsHosts has new SslCipherList12, SslCipherList13 and SslCryptoGroups
                      properties, which if set are used after the defaults for the specifed
                      sslSrvSecurity have been set, to override them.  May be specified in
                      the IcsHosts INI file separately for each host.
Dec 09, 2024 V9.4  Replace expiring ICS created certificate at same time warnings start
                     instead of 7 days before expiry.
                   Added Error to Disconnect() client method.
Sep 10, 2025 V9.5  Using DEFINE OpenSSL_OcspStaple to conditionally link code relating
                     to stapling an OCSP response to check if server certificate is revoked.
                     Let's Encrypt stopped adding an OCSP URL to certificates in May 2025
                     so only enable this if using authorities that still support OCSP.
                     Note properties OcspSrvStapling and OcspSrvHttp are still published
                     for backward compabibility, but are ignored without the define.
                   IcsHosts has SslRawKeyFile which if non-blank is the private key file
                     for Raw Public Key (RPK) authentication instead of using an X509
                     certificate.  RPK maybe used for client or server authentication.
                       !!!  NOTE PRK not finished yet, pending client and SslContext changes !!!!
                   IcsHosts has AcmeSupplier as TAcmeSupplier to support suppliers other
                     than Let'S Encrypt, and SupplierTitle to specify name of than supplier
                     instead of specifying CertDirWork which will be looked up from
                     C:\ProgramData\ICS-Acme-Accounts\ics-acme-accounts.db.  This supplier
                     accounts database is generally maintained by the OverbyteIcsX509CertsTst
                     sample, which must be used to create accounts for new suppliers,
                     and which may be used to view certificate orders.
                   IcsHosts has AcmeCertProfile to specify the type of certificate
                     requested for Let's Encrypt, listed in FAcmeProfileNames array,
                     default classic, optional tlsserver and shortlived (7 day, not yet
                     available).
                   IcsHosts has AcmeCertValidity to specify certificate life in days, default
                     90, only Google at present, down to 3 days.
                   Certificate ordering now makes use of the Renewal Information API that
                     specifies how many days before expiry a certificate should be
                     renewed, and how often these dates should be rechecked to see if he
                     certificate needs immediate renewal due to being revoked.  This
                     overrides CertExpireDays.  Renewal Information is checked each time
                     the certificate chain is checked, but is cached so there is usually
                     only a server API call every six hours. Note with OCSP gone, this
                     is now the only way to check if a certificate is revoked.
                   Reworked certificate checking if automatic ordering is enabled so
                     that Acme account information is looked up when the certificate is
                     first loaded to get renewal information and maybe working directory,
                     rather than only when time to order a new certificate, so there is
                     more logging and error checking at load time.
                   Temporary ICS self signed certificates are now created in GSSL_CERTS_DIR
                     instead of TempPath.
                   When starting a certificate order, if the challenges have been
                     previously completed OK, collect order immediately, don't try to
                     start them again.  Note with Google collect may not work immediately
                     since certificate processing does not happen during the request, the
                     order timer will repeat collect attempt each five seconds.
                   IcsHosts now ignores DHParams without DEFINE OpenSSL_Deprecated, no
                     longer needed for modern cyphers, probably means DH and DHE ciphers
                     will fail.
                   Fixed a long term problem where SNI checking for a matching IcsHost used
                     the certifcate SANs that might have included a wild card, instead of
                     the Hosts list of host names.  If one IcsHost allowed wild cards it
                     might have been found instead a specific IcsHost for a single host.
                   Automatic certificate ordering in IcsHosts now has a database property
                     CertRenewNow that if set true in the database using the GUI,
                     will override certificate expiry checking and cause an immediate new
                     certificate replacement order by in servers with IcsHosts the next time
                     RecheckSslCerts is called by the server, typically every two hours.
                   Added OnClientAcceptFilter event called before TWSocketServer accepts an
                     incoming connection allowing filtering on the remote IP address so the
                     connection is refused without any more events being called. Server
                     completes TIcsSessIpInfo record with remote and local addresses and
                     ports, event can complete others fields.
Sep 19, 2025 V9.5  IcsHosts has FOrderPending for immediate certificate ordering instead
                     of setting ExpiryDays to -1, improved logging for certificate order
                     reason.
                   certificate ordering supplier account only opened in DoCheckOrderCert,
                     before called DoProcOrderCert.
Oct 15, 2025 V9.5  Unit will build without USE_SSL again.
                   To simplify IcsHosts configuration, added seven new default certificate
                     properties to TSslWSocketServer, SrvSupplierTitle, SrvAcmeSupplier,
                     SrvAcmeCertProfile, SrvAcmeCertValidity, SrvCertChallenge,
                     SrvCertPKeyType and SrvCertSignDigest, which will be used if the
                     similar named (less Srv) IcsHosts properties are not specified.
                     So each IcsHosts only needs CertSupplierProto specified to enable or
                     disable certificate ordering.
                   Updated IcsLoadIcsHostsFromIni with new Server: TWSocketServer argument
                     from which the defaults will be taken. For web servers, pass
                     HttpAppSrv.WSocketServer with the defaults from IcsLoadTHttpAppSrvFromIni.
                   Create a new let's Encrypt account if sufficient details in IcsHosts and
                     there are zero suppliers configured using OverbyteIcsX509CertsTst, the
                     samples have a SupplierTitle 'LetsEncrypt-New'.  Only happens once, in
                     case other INI have incorrect titles. OverbyteIcsX509CertsTst can create
                     multiple acccounts, and must be used for Google since Google account
                     information is needed.
                   Better error handling opening bad certificate file so we can create self
                     signed certificate and then order new cert.
                   Updated all documentation.




Note: Certificate ordering only tested OK with Let's Encrypt and Google.
The extra lines needed for an IcsHost to allow ordering are essentially:

WellKnownPath=d:\websites\well-known\test  (wwere your web site will look for \well-known challenges )
SslCert=c:\certificates\local\test1_ftptest_org.pfx  (file name must match first Hosts name)
CertSupplierProto=SuppProtoAcmeV2 or SuppProtoNone (to disable ordering)
CertChallenge=ChallFileApp or ChallAlpnApp
CertPKeyType=PrivKeyRsa2048 or PrivKeyECsecp256 (or stronger, see below)
CertSignDigest=Digest_sha256  (or stronger, see below)
AcmeCertProfile=tlsserver or classic (only Let's Encrypt at present, shortlived 7 days later in 2025)
AcmeCertValidity=90 (only Google at present, down to 3 days)
AcmeSupplier=AcmeLetsEncrypt or AcmeGoogle
SupplierTitle=AcmeLetsEncrypt-1 or AcmeGoogle-1
(Supplier Database title, maintained by OverbyteIcsX509CertsTst sample, where the Work
Directory and Acme Supplier are specified, and account credentials for Google)
or
CertDirWork=d:\weblogs\acme-certs\  (no Supplier Database, AcmeLetsEncrypt only)





Quick reference guide:
----------------------

TWSocketServer will normally be used to listen on a given TCP port. When a
client connect, it will instantiate a new TWSocketClient component to handle
communication with client. Normally you will derive your own component from
TWSocketClient to add private data and methods to handle it. You tell
TWSocketServer which component it has to instantiate using ClientClass
property. You have to initialize instances from OnClientConnect event handler.
TWSocketServer maintain a list of connected clients. You can access it using
Client[] indexed property and ClientCount property, beware this list is dynamic
as connections are opened and closed, so don't access instances by count.


Automatic Certificate Ordering Supplier Accounts
------------------------------------------------

From V9.5, ICS has a central Acme Supplier Account database that contains a list
of the Acme work directories previously only referenced in separate IcsHosts INI
files. The database is at C:\ProgramData\ICS-Acme-Accounts\ics-acme-accounts.db,
and each Supplier Account has a Supplier Account Title, Acme Supplier Name, and
Account Directory, which defaults to: C:\ProgramData\ICS-Acme-Accounts\SupplierTitle\
but can be located elsewhere if required.

If an ICS server with automatic certificate ordering starts and no Acme Supplier
Accounts are configured, the server will attempt to create one.  If the INI file
was used prior to V9.5 and CertDirWork is specified with old account details,
a new supplier AcmeLetsEncrypt-Old is created for that existing directory, and
everything should work as previously.  If the INI file has SupplierTitle, no
CertDirWork and sufficient details in IcsHosts, a new supplier is created with
that title, the ICS samples use 'LetsEncrypt-New'.  Note this only happens if
no suppliers are configured, so a typo in INI file does not create an unwanted
supplier.

Otherwise, Acme Supplier Accounts must be created manually using the sample
OverbyteIcsX509CertsTst or functions from the TSslX509Certs component in your
application.  Note Google and other supplier accounts must be created manually,
since they need extra external accounting details that are not in IcsHosts.

Each Supplier Account has an Account Directory in which the account private
key file AcmePrivateKey.pem is created and stored, and all certificate and
related files will be saved, with a database file ics-control.db containing
information about the account, certificate orders and pending challenges.
The account private key (separate to certificate private keys) is used to
identify the account to the supplier and is created automatically if the
directory is blank. This account private key will be needed to cancel or
revoke any certificates issued using it.  Note an account directory and
database can only be accessed by one application at a time, they can
generally not be shared between different servers.

The sample OverbyteIcsX509CertsTst has tabs 'Acme Suppliers' and  'Supplier
Details' that allows accounts to be created and edited, see later for details.


IcsHosts
--------

When originally designed TWSocketServer only supported listening on a single IP
address and port, subsequently MultiListenSockets were added to listen on multiple
IP addresses and ports, and TSslWSocketServer SSL support required a lot of extra
code in the application to specify SSL certificates, protocols, ciphers and
security using multiple SslContexts for multiple hosts.

The IcsHosts property is an alternate way for specifying multiple listeners for
TSslWSocketServer that allows multiple hosts to be specified, each with one or
two IP addresses and non-SSL and SSL port bindings, SSL certificates and private
key (usually combined in a bundle), SSL context and security level, and other web
server host related properties (used by higher level components).  Each IcsHost
has one or more HostNames to which it will recognise, that can share IP addresses.
The same IP address can be used by two more IcsHosts with differing host names
and other settings.  IcsHosts include properties for use by other components,
such as web, proxy and FTP servers, and for automatic SSL/TLS certificate ordering.

If IcsHosts is specified, TSslWSocketServer ignores existing bindings and SSL
context, and creates new bindings and initializes an SSL context for each host
checking and reporting all server certificate chains.  To ease implementation,
functions are provided to read IcsHosts and TWSocketServer from an INI file, or
they may be specified through IDE form properties and saved by other means.

Note IcsHosts is only available for TSslWSocketServer, not TWSocketServer, but
you don't need to use SSL for any Hosts.

HostNames     - A list of one or more domain Host Names for which this IcsHost
                will respond, set as a TStringList, but read from the INI file
                as a  comma separated list, no quotes. Host Names are matched
                initially against SSL Server Name Indication (SNI), or against
                the HTTP Host: header if no SSL or SNI for web and proxy servers.
                Note INI file reads as Hosts.  Wildcard host names with a * are
                not allowed.
HostEnabled   - True or False if this IcsHost is enabled, NOTE INI file also
                reads Enabled if HostEnabled missing for backward compatibility.
BindIpAddr    - Listening IPv4 or IPv6 Address for this IcsHost, may be 0.0.0.0
                for all IP Addresses, must exist.  Multiple IcsHosts can use the
                same IP Address which will then be chosen from HostNames using
                SNI or Host: header.
BindIpAddr2   - Optional second IP Address for this IcsHost, perhaps an IPv6
                address.
BindNonPort   - Optional non-SSL/TLS Port, may be blank or zero if only SSL is
                supported by IcsHost, usually 80 for HTTP, required if
                WellKnownPath specified for SSL automatic certificates.
BindSslPort   - Optional SSL/TLS Port, may be blank or zero if SSL not supported
                by IcsHost, if used generally 443 for HTTPS, and several other SSL
                parameters are required.  Either BindNonPort or BindSslPort is
                required.
HostTag       - A short alphabetic name for the IcsHost, used variously in
                high level servers.  The web application server uses HostTag in
                the AddGetHandler, AddPostHandler and AddGetAllowedPath to cause
                that handler to be matched.  The proxy server uses HostTag to
                match source to one or more targets.  When SocketServer creates
                a TWSocketClient for a new connection it sets the IcsHostIdx and
                HostTag properties to this value, also ServerAddr and ServerPort
                for the bindings.
Descr         - Optional free description of the IcsHost, may be used for logging.
Proto         - Optional high level protocol used by this IcsHost, used variously
                in servers.  The proxy server uses HTTP, SMTP, POP3, etc.
ForwardProxy  - Optional True/False used by the proxy server if this IcsHost is
                functioning as a Forward Proxy rather than a Reverse Proxy. A
                forward supports multiple targets according to the URL, while a
                reverse proxy generally ignores the URL and has a single fixed
                target, sometimes multiple fixed targets depending on the URL.
                See OverbyteIcsProxy.pas for more information.
WebDocDir     - Optional default web document directory for this IcsHost, used by
                the web server.
WebTemplDir   - Optional default web template directory for this IcsHost, used by
                the web server.
WebDefDoc     - Optional default web document for this IcsHost, used by the web
                server.
WebLogDir     - Optional web logging directory for this IcsHost, for use by web
                server applications (not by the server itself which has not
                logging).
WebLogIdx     - Optional web logging index for this IcsHost, for use by web
                server applications (not by the server itself which has not
                logging).
AuthSslCmd    - Optional, if SSL/TLS should be initialised to support an AUTH SSL
                command or similar on an initial non-SSL connection. Used by FTP.
AuthForceSsl  - Optional, if login only allowed once SSL/TLS negotiated so
                   credentials never sent clear.  Used by FTP.
SslLoadSource - Optional, specifies the source for loading SSL/TLS certificates.
                Defaults to CertLoadFile for PEM/PFX files where SslCert is a
                file name with path and extension. CertWinStoreMachine or
                CertWinStoreUser specifies load from the Windows Certificate Stores
                where HostNames[0] is the common name, part friendly name or a single
                host name to choose from the store, if there are duplicates the
                longest expiry data will be chosen.  If the certificate name is a
                wildcard (*), this will also matched with any first node. Note only
                certificates with exportable private keys will be selected, and the
                application must have administrator rights to access the Local Machine
                store.
SslCert       - Optional SSL server certificate file name, may be PEM, DER, PFX,
                P12, P7 format, optionally a bundle including a private key and
                one or more intermediate certificates.  Required if BindSslPort
                set. If a private key is included, will use SslPassword for an
                encrypted file. Rather than a file name, may be an ASCII PEM string
                containing the certificate without any line endings.  HostNames
                will be checked against those listed on the certificate, and a not
                warning given for mismatches so a single certificate needs to
                contain all the HostName, or wild cards * so they match.
                TSslWSocketServer has a method RecheckSslCerts that should be
                called from the server application periodically, at least once
                a day, which will check the certificate file time stamp and
                reload it if changed, issue warnings if it expires within
                CertExpireDays (default 30) and optionally order a new
                certificate, see CertSupplierProto below.  Note if automatic
                certificate ordering is used, the file name in SslCert must
                conform to the format used by the TSslX509Certs component, with
                the host name having all dots replaced by underscore and * by x,
                then with suffix .pfx or -bundle.pem, ie test3_telecom-tariffs_co_uk-bundle.pem
                or test3_telecom-tariffs_co_uk.pfx.
                Important: if the SslCert file does not exist, ICS will create
                it signed by the ICS intermediate and root, so the server cam
                start, generally expecting it to be replaced automatically by
                one automatically ordered from Let's Encrypt or Google Trust Services.
SslKey        - Optional SSL private key file name, PEM format, will use
                SslPassword for an encrypted file.  Ignored if SslCert was a
                bundle file with a private key.  Note all SSL servers need an
                SSL certificate and matching private key to function, and these
                are checked before the server will start. Rather than a file
                name, may be an ASCII PEM string containing the private key
                without any line endings.
SslInter      - Optional SSL intermediate certificate bundle file name, PEM, DER,
                P12 or P7 format. Ignored if SslCert was a bundle with
                intermediate certificates.  The SSL certificate chains is checked
                to ensure the server certificate is signed by an intermediate or
                trusted root certificate, and likewise the intermediate(s).
                ICS is usually built with a bundle of trusted root certificates
                linked into applications and loaded on start-up.
SslPassword   - Optional password for the SSL private key, if encrypted.  Note
                this is clear text in the INI file, steps should be taken to read
                an encrypted password if security is required.  Beware most
                PFX/P12 bundles need a password, since Windows will not import
                them without a password.
SslSrvSecurity - Optional SSL server security level, that sets protocol, cipher
                and SslSecLevel according to eight levels from type TSslSrvSecurity:
                sslSrvSecNone, sslSrvSecSsl3, sslSrvSecBack, sslSrvSecInter,
                sslSrvSecInterFS, sslSrvSecHigh, sslSrvSecHigh128, sslSrvSecHigh192.
                Details of each are in the OverbyteIcsSSLEAY.pas unit.  Beware the
                server may not start, ie High128 requires an RSA private key length
                greater than the 2,048 bits commonly used.  If not specified,
                sslSrvSecDefault is used which is currently sslSrvSecHigh
                meaning TLSv1.2 or later with forward security ciphers.
CliCertMethod  - Optional, defaults to sslCliCertNone, allows host to request a client
                SSL certificate from the browser or remote application, as type
                TSslCliCertMethod, for sslCliCertOptional or sslCliCertRequire.
                Require will close the connection unless a valid certificate is
                received and validated against RootCA. Beware requesting a client
                certificate usually causes the browser to prompt the user for which
                certificate to send which can be obtrusive.
SslCipherList12 - Optional, list of ciphers the server will accept for TLSv1.2
                connections, see constant sslCiphersMozillaSrvTLS12 for example.
SslCipherList13 - Optional, list of ciphers the server will accept for TLSv1.3
                connections, see constant sslCipherSuitesTLS13 for example.
SslCryptoGroups - Optional, list of crypto groups the server will accept, see
                constant sslCryptoGroupsDef for example.
SslRawKeyFile   - Optional, the private key file for Raw Public Key (RPK)
                authentication instead of using an X509 certificate.  RPK maybe
                used for client or server authentication. !!  NOTE RPK not finished yet !!!
WellKnownPath   - Optional, full file directory path for .Well-Known web URLs, used
                 by the web and proxy servers for automated SSL certificates.
WebRedirectURL  - Optional, a web server redirection URL, used in the proxy server
                to redirect HTTP connections to HTTPS, if WebRedirectStat is none
                zero.  Not currently used by the web server, but web server
                applications can add event code to use these values, see event
                SslHttpAppSrv1GetDocument in OverbyteIcsSslMultiWebServ1.pas.
                Or more intelligent redirection could be implemented like
                changing http:// to https://.
WebRedirectStat - Optional, a web server redirection status code, 301, 302, 307
                or 308 depending on reason.
CertSupplierProto - Optional, if SSL X509 certificates are to be automatically
                ordered, downloaded and installed, specifies the Protocol to be
                used as type TSupplierProto. Currently supports only SuppProtoNone
                and SuppProtoAcmeV2. Ignored unless TSslWSocketServer property
                SslCertAutoOrder is True.  As of V9.5, this is the only IcsHost
                property needed for automatic certificate ordering, since the
                other properties now have common defaults in TSslWSocketServer.
                Note any SSL certificates ordered will use all HostNames, also
                wild card certificates can not be ordered by IcsHosts, only using
                TSslX509Certs.
                Automatic certificate ordering is triggered by calling the server
                method RecheckSslCerts after the server has started listening,
                which checks all certificates and chains for expiry within
                CertExpireDays (default 30) and will then order a new certificate
                for any that fail, including certificate file missing.  The server
                application should call RecheckSslCerts at least once a day, but
                it can be more often to give more chances to check for ordering
                problems.  The TSslX509Certs component has an internal timer that
                checks for order completion and calls an onNewCert event when the
                new certificate is ready, this event should call RecheckSslCerts
                again which will find the new certificate file and load it into
                SslContext.
CertDirWork     - Optional, ignored if CertSupplierProto is not SuppProtoNone.
                For V9.5 and later, not needed since accounts are saved by
                SupplierName, generally in C:\ProgramData\ICS-Acme-Accounts.
                For earlier versions, specifies the Certificate Database Working
                Directory, in which an account that matches CertSupplierProto
                should already have been created, see OverbyteIcsSslX509Certs.pas
                for more information.
SupplierTitle   - Optional, defaults to SrvSupplierTitle in TSslWSocketServer so
                generally not needed in IcsHosts.  Supplier Account Title, used to
                look up AcmeSupplier and CertDirWork from ics-acme-accounts.db.
AcmeSupplier    - Optional, defaults to SrvAcmeSupplier in TSslWSocketServer so
                generally not needed in IcsHosts.  Specifies one of several ACME
                suppliers of type TAcmeSupplier: AcmeLetsEncrypt, AcmeLetsEncryptTest,
                AcmeZeroSSL, AcmeGoogle, AcmeGoogleTest, AcmeDigicert,
                AcmeDigicertTest, AcmeSslcomRSA, AcmeSslcomECC.  The names with test
                specify use the suppliers testing or staging servers that generally
                issue fake certificates for testing and don't care about errors.
AcmeCertProfile - Optional, defaults to SrvAcmeCertProfile in TSslWSocketServer so
                generally not needed in IcsHosts.  Currently only used by Let's
                Encrypt to specify certificate profile: classic, tlsserver or
                shortlived (seven days).
AcmeCertValidity - Optional, defaults to SrvAcmeCertValidity in TSslWSocketServer
                so generally not needed in IcsHosts. Currently Google only,
                specifies the certificate expiry days, default 90, seven supported.
CertChallenge   - Optional, defaults to SrvCertChallenge in TSslWSocketServer so
                generally not needed in IcsHosts. Specifies the domain name
                Challenge Type as TChallengeType:  ChallNone, ChallFileUNC,
                ChallFileFtp, ChallFileSrv, ChallFileApp, ChallDnsAuto, ChallDnsMan,
                ChallEmail, ChallAlpnUNC, ChallAlpnSrv and ChallAlpnApp.
CertPKeyType  - Optional, defaults to SrvCertPKeyType in TSslWSocketServer so
                generally not needed in IcsHosts. Specifies the new SSL certificate
                Private Key algorithm and key length as type TSslPrivKeyType:
                PrivKeyRsa2048, PrivKeyRsa3072, PrivKeyRsa4096, PrivKeyECsecp256,
                PrivKeyECsecp384, PrivKeyECsecp512.
CertSignDigest - Optional, defaults to SrvCertSignDigest in TSslWSocketServer so
                generally not needed in IcsHosts. Specifies the new SSL certificate
                request signing digest as type TEvpDigest: Digest_sha256,
                Digest_sha384 or Digest_sha512.


Example from \Samples\Delphi\SslInternet\OverbyteIcsSslMultiWebServ.ini which can
be read using the functions IcsLoadTHttpAppSrvFromIni and IcsLoadIcsHostsFromIni.

[WebAppServer]
SslCertAutoOrder=True
CertExpireDays=30
X509ProxyURL=
SrvSupplierTitle=LetsEncrypt-New
SrvAcmeSupplier=AcmeLetsEncrypt
SrvAcmeCertProfile=tlsserver
SrvAcmeCertValidity=90
SrvCertChallenge=ChallAlpnApp
SrvCertPKeyType=PrivKeyECsecp256
SrvCertSignDigest=Digest_sha256

[Host4]
Hosts=test7.ftptest.org,test7.ftptest.org.uk,test7.ftptest.co.uk
HostTag=HTTP-FTPTEST
Desc=test7-LetsEncrypt
BindIpAddr=192.168.1.123
BindNonPort=80
BindSslPort=443
HostEnabled=False
SslSecLevel=sslSrvSecInterFS
WellKnownPath=c:\websites\well-known\
WebRedirectURL=https://www.telecom-tariffs.co.uk/
WebRedirectStat=301
SslCert=c:\certificates\local\test7_ftptest_org.pfx
SslPassword=password
CliCertMethod=sslCliCertNone
CertSupplierProto=SuppProtoAcmeV2

Once IcsHosts have been validated, a number of read only properties are available
with more information about the IcsHost from the certificate and bindings, that
may be usefully logged for configuration and error checking purposes.

SslCtx       - TSslContext component used by IcsHost.
HostNameTot  - Number of items in HostNames TStrings.
DisplayName  - IcsHost display name, variously using HostTag, Descr, and bind
             IP addresses and ports.
CertDomains  - Comma separated list of SSL certificate host names from Subject
             Alternate Names field.
CertInfo     - A long multiple line text block describing all the main fields of
             one or more SSL certificates in the chain including subject,
             issuer, expiry, public key type and signature method.
BindInfo     - List of one or more IP addresses and ports.
CertExiry    - SSL certificate expiry UTC date and time, as TDateTime.
CertFStamp   - Unix file time stamp for SSL server certificate, to check if
             changed.
InterFStamp  - Unix file time stamp for SSL intermediate certificates, to check
             if changed.
CertErrs     - Description of any errors found in the SSL certificate chain
             during validation.
CertValRes   - SSL certificate chain validation result as type TChainResult,
             chainOK, chainFail, chainWarn, chainNone.
BindIdxNone  - MultiListenIdx for first IP address, non-SSL.
BindIdxSsl   - MultiListenIdx for first IP address, SSL.
BindIdx2None - MultiListenIdx for second IP address, non-SSL.
BindIdx2Ssl  - MultiListenIdx for second IP address, SSL.
BindSrvPort  - Actual listening port, useful if Port=0 for dynamic port.
RenewalDays  - For automatically issued certificates, how many days before
             expiry the supplier recommends renewing the certificate,
             generally two thirds of the way through duration, but could
             be sooner if the certificate is cancelled or revoked.
             Internally, this tells ICS when to order a new certificate.
RenewRetryDT - For automatically issued certificates, when the supplier
             recommends next checking the renew dates, they may change.
             Usually every six to 24 hours.
RenewCheckDT - For automatically issued certificates, when last checked
             renewal dates.


TSslWSocketServer
-----------------

Proto        - Server protocol, always TCP.
Addr         - Server listen IP address, IPv4 or IPv6, maybe 0.0.0.0 or :: to
               listen on all available addresses, ignored if any IcsHosts
               specified.  IP address must exist and not be in use elsewhere
               for the same port.
Port         - Server listen port, usually 80 or 443, ignored if any IcsHosts
               specified, zero means dynamic port.
BindPort     - While listening actual port, useful if Port=0 for dynamic port.
SocketFamily - IP address socket family as TSocketFamily, from sfIPv4, sfIPv6.
SslEnable    - True if an SSL connection should be negotiated. ignored if any
               IcsHosts specified.
SslContext   - Assign to an TSslContext component for SSL support, where SSL
               certificates, keys, protocols and ciphers are specified, ignored
               if any IcsHosts specified.
MultiListenSockets - Allows one or more extra server listen IP addresses and
               ports to be specified, as type TWSocketMultiListenCollection,
               allowing server to listen on several IP addresses/ports at the
               same time, in addition to the that specified as Addr/Port props.
               Each TWSocketMultiListenItem has Addr, Port, SocketFamily,
               SslEnable and ListenBacklog properties. Better to use IcsHosts
               for new applications which does the same, ignored if any IcsHosts
               specified.
MultiListenIndex - Read only, which listen socket accepted last connection, -1
               if default Addr.Poprt used, 0 or above is index into
               MultiListenSockets collection.
MaxClients   - Maximum number of simultaneous clients the server should accept
               if non-zero.  Any further clients will receive then BannerTooBusy
               response and the connection closed, until earlier connections
               are closed.
Banner       - If non-blank, a text string that will be sent in response to a
               new incoming connection, should generally be blank.
BannerTooBusy - If non-blank, a text string that will be sent once MaxClients
               is exceeded.
ClientClass  - The class type the application has derived from TSslWSocketClient
               for client application code.
Client       - Currently active clients of ClientClass, indexed base zero.  Note
               the index value may change each time a new client connects or
               disconnects, so check CliId to confirm it's the correct one.
ClientCount  - Number of active client connected.
SocketErrs   - How socket error messages should be presented as type TSocketErrs,
               wsErrTech or wsErrFriendly.
ExclusiveAddr - True is other applications should be blocked from sharing the
               server IP addresses.
ListenBacklog - How many new client connections should be queued by Windows while
               the server accepts them, before the server starts rejecting new
               connections by immediately closing them.  Recommended as 15 for
               heavy use servers, may be up to 250.
IcsHosts     - Allows one or more TIcsHosts to be set, as TIcsHostCollection, an
               alternate way for specifying multiple listeners that allows
               multiple hosts to be specified, each with one or two IP addresses
               and non-SSL and SSL port bindings, SSL certificates and private
               key (perhaps combined in a bundle), SSL context and security level,
               and other web server host related properties (used by higher level
               components).  Each IcsHost has one or more HostNames to which it
               will recognise, that can share IP addresses.  See above for more
               detail.
RootCA       - Specifying a file name containing a PEM bundle of trusted root SSL
               certificates allows validation of SSL server certificate chains.
               ICS includes RootCaCertsBundle.pem (large) and TrustedCABundle.pem
               (medium size), and a default built-in (small) that will be used
               if no file is specified.
DHParams     - Specifies a DHParams file name, created using the PenTools sample,
               or use the provided dhparam1024.pem or dhparam2048.pem files. Used
               for DH and DHE ciphers, but not needed for modern ECDHE ciphers.
               Rather than a file name, may be an ASCII PEM string containing
               the DHParams without any line endings.
SslCliCertMethod - Allows server to request a client SSL certificate from the
               browser or remote application, as type TSslCliCertMethod, with
               sslCliCertNone, sslCliCertOptional or sslCliCertRequire. Require
               will close the connection unless a valid certificate is received
               and validated against RootCA. Beware requesting a client
               certificate usually causes the browser to prompt the user for
               which certificate to send which can be obtrusive.
CertExpireDays - When using IcsHosts, the number of days before an SSL server
               certificate is due to expire that warnings will be generated (by
               the method RecheckSslCerts, perhaps triggering automatic SSL
               certificate ordering.  May be overriden by automatically ordered
               certificates that declare their own renewal recommendation.
SslCertAutoOrder - True if IcsHosts are allowed to order and install SSL
               certificates automatically.  Requires SslX509Certs property to
               be set, and CertSupplierProto set for any IcsHost that will order
               certificates.
ServSupplierTitle - Optional, ignored without SslCertAutoOrder=True. Supplier
               Account Title, used to look up AcmeSupplier and CertDirWork from
               ics-acme-accounts.db.  See earlier for more information.
               Note that all IcsHosts must have the same SupplierTitle and
               AcmeSupplier, you can not mix suppliers on a server.
ServAcmeSupplier - Optional, ignored without SslCertAutoOrder=True.  Specifies
                one of several ACME suppliers of type TAcmeSupplier: AcmeLetsEncrypt,
                AcmeLetsEncryptTest, AcmeZeroSSL, AcmeGoogle, AcmeGoogleTest,
                AcmeDigicert, AcmeDigicertTest, AcmeSslcomRSA, AcmeSslcomECC.
                The names with test specify use the suppliers testing or staging
                servers that generally issue fake certificates for testing and
                don't care about errors.
                Note: TSslWSocketServer will only automatically create a new
                supplier account for AcmeLetsEncrypt, any other AcmeSuppliers
                must have an account created using externally, usually using
                the OverbyteIcsX509CertsTst sample, since they may need extra
                information not specified here.
ServAcmeCertProfile - Optional, ignored without SslCertAutoOrder=True.
                Currently only used by Let's Encrypt to specify certificate
                profile: classic, tlsserver or shortlived (seven days).
ServAcmeCertValidity - Optional, ignored without SslCertAutoOrder=True.
                Currently Google only, specifies the certificate expiry days,
                default 90, seven days supported.
ServCertChallenge - Optional, ignored without SslCertAutoOrder=True.
                Specifies the domain name Challenge Type as TChallengeType:
                ChallNone, ChallFileUNC, ChallFileFtp, ChallFileSrv, ChallFileApp,
                ChallDnsAuto, ChallDnsMan, ChallEmail, ChallAlpnUNC, ChallAlpnSrv
                and ChallAlpnApp. Note most challenges need a little extra code
                in the server application.  Different types of servers usually
                need differing challenges: web servers are usually ChallFileApp
                where the well-known directory is used on port 80 or
                ChallAlpnApp where the SSL HELO on port 443 passes a special
                ALPN; FTP servers don't listen on ports 80 or 443, so need to
                use a local web server on port 80 with ChallFileSrv, likewise
                proxy servers not using 80/43.
ServCertPKeyType - Optional, ignored without SslCertAutoOrder=True.  Specifies
                the new SSL certificate Private Key algorithm and key length
                as type TSslPrivKeyType: PrivKeyRsa2048, PrivKeyRsa3072,
                PrivKeyRsa4096, PrivKeyECsecp256, PrivKeyECsecp384,
                PrivKeyECsecp512, although the supplier may reject any of them.
                Beware RSA keys longer than 4,096 bits can take many minutes
                to generate the key blocking the server.
ServCertSignDigest - Optional, ignored without SslCertAutoOrder=True. Specifies
                the new SSL certificate request signing digest as type
                TEvpDigest: Digest_sha256, Digest_sha384 or Digest_sha512.
SslX509Certs - Assign to a TSslX509Certs component if automatic SSL certificate
               ordering is required.  Ir ia very important that the onCertProg
               is used to log progress messages from the certificate ordering
               process in case of errors.  The onCertsChallengeDNS event is
               called if a DNS server should be updated, onCertsOAuthAuthUrl if
               0Auth2 authenication is needed, and onCertsNewCert when a new
               certificate is available which should be logged and the
               RecheckSslCerts method called to cause the server to load it.
OcspStapling - Support OCSP revocation checking and OCSP Stapling for all
               hosts and certificates.  V9.5 reqyuires DEFINE OpenSSL_OcspStaple.
NoSSL        - If True, prevents SSL/TLS being used.


TSslWSocketClient
-----------------

Each new incoming connection to TSslWSocketServer causes a new instance of
TSslWSocketClient to be created, with a number of read only properties set,
that may be useful for client application code.  Generally, applications will
create their own class descended from TSslWSocketClient which is where all
the code to listen to requests and send responses will go.

Server      - TCustomWSocketServer component to which the client belongs.
PeerAddr    - Remote IP address of the client, IPv4 or IPv6.
PeerPort    - Remote IP port of the client.
SessionClosedFlag - True if the client is in the process of closing.
CliId       - A sequential client ID number that may be used to identify the
              client, note clients are creating and destroyed regularly.
IcsHostIdx  - If 0 or higher index into the IcsHosts collection, -1 if
              IcsHosts not used.
MultiListenIdx - -1 if default SocketServer listener used, 0 or above is
              index into MultiListenSockets collection.
HostTag     - A short alphabetic name for the IcsHost, used variously in
              high level servers.
CServerAddr - Local server IP address for listener.
CServerPort - Local server IP port for listener.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsWSocketS;
{$ENDIF}

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$I Include\Z.ICS9.OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  OFF}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Types{$ELSE}Types{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.IniFiles{$ELSE}IniFiles{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.TypInfo{$ELSE}TypInfo{$ENDIF},
    Z.ICS9.OverbyteIcsWinsock,
{$ENDIF MSWINDOWS}
{$IFDEF MACOS}  { V8.49 }
    System.Types,
{$ENDIF MACOS}
{$IFDEF POSIX}
    System.TypInfo,   { V8.65 }
    System.IniFiles,
    Posix.Errno,
    Posix.NetinetIn,
    Posix.SysSocket,
    Z.ICS9.Ics.Posix.WinTypes,
    Z.ICS9.Ics.Posix.PXMessages,
{$ENDIF POSIX}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
{$IFDEF USE_SSL}
    Z.ICS9.OverbyteIcsSSLEAY, Z.ICS9.OverbyteIcsLIBEAY,
{$ENDIF USE_SSL}
{$IFNDEF NO_DEBUG_LOG}
    Z.ICS9.OverbyteIcsLogger,
{$ENDIF NO_DEBUG_LOG}
{$IFDEF FMX}
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocket,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslBase,  { V9.1 TX509Base }
{$IFDEF USE_SSL}
    Z.ICS9.Ics.Fmx.OverbyteIcsSslX509Utils,
{$IFDEF AUTO_X509_CERTS}
//    Ics.Fmx.OverbyteIcsSslHttpRest,    { V9.1 }
    Z.ICS9.Ics.Fmx.OverbyteIcsSslUtils,     { V9.1 }
{$IFDEF MSCRYPT_Servers}
    Z.ICS9.Ics.Fmx.OverbyteIcsMsSslUtils,     { V8.71 }
{$ENDIF MSCRYPT_Servers}
{$ENDIF AUTO_X509_CERTS}
{$ENDIF USE_SSL}
{$ELSE}
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsSslBase,    { V9.1 TX509Base }
{$IFDEF USE_SSL}
    Z.ICS9.OverbyteIcsSslX509Utils,  { V8.64 }
{$IFDEF AUTO_X509_CERTS}
{$IFDEF MSCRYPT_Servers}
    Z.ICS9.OverbyteIcsMsSslUtils,    { V8.71 }
{$ENDIF MSCRYPT_Servers}
    Z.ICS9.OverbyteIcsSslUtils,   { V9.1 OcspHttp }
{$ENDIF AUTO_X509_CERTS}
{$ENDIF USE_SSL}
{$ENDIF FMX}
    Z.ICS9.OverbyteIcsUtils,
    Z.ICS9.OverbyteIcsTypes,
    Z.ICS9.OverbyteIcsTicks64;

const
    WSocketServerVersion     = 905;
    CopyRight : String       = ' TWSocketServer (c) 1999-2025 F. Piette V9.5 ';
 { V8.62 ALPN requests that may arrive during SSL helo }
    AlpnAcmeTls1 = 'acme-tls/1';
    AlpnHttp11   = 'http/1.1';
    AlpnHttp2    = 'h2';

type
    TCustomWSocketServer       = class;
    TWSocketClient             = class;
    TWSocketClientClass        = class of TWSocketClient;
    TWSocketClientCreateEvent  = procedure (Sender: TObject; Client: TWSocketClient) of object;
    TWSocketClientConnectEvent = procedure (Sender: TObject; Client: TWSocketClient; Error: Word) of object;
    TClientAcceptFilterEvent   = procedure (Sender: TObject; Client: TWSocketClient; var SessIpInfo: TIcsSessIpInfo;
                                                                                          var Allowed: Boolean) of object;  { V9.5 }

    TClientIdRec = record    { angus V7.00 }
        PClient : Pointer;
        CliId   : Integer;
    end;
    PClientIdRec = ^TClientIdRec;

    { TWSocketClient is used to handle all client connections.           }
    { Altough you may use it directly, you'll probably wants to use your }
    { own derived component to add data and methods suited to your       }
    { application.                                                       }
    { If you use a derived component, then assign it's class to          }
    { TWSocketServer ClientClass property.                               }
    TWSocketClient = class(TWSocket)
    protected
        FBanner            : String;
        FServer            : TCustomWSocketServer;
        FPeerAddr          : String;                       { V9.5 use SessionIpInfo.RemoteAddr instead }
        FPeerPort          : String;                       { V9.5 use SessionIpInfo.RemotePort instead }
        FSessionClosedFlag : Boolean;
        FCliId             : Integer;          { angus V7.00 }
{$IFDEF USE_SSL}
        FIcsHostIdx        : Integer;          { V8.45 }
        FMultiListenIdx    : Integer;          { V8.45 }
        FHostTag           : String;           { V8.45 }
        FServerAddr        : String;           { V8.45 }  { V9.5 use SessionIpInfo.LocalAddr instead }
        FServerPort        : String;           { V8.45 }  { V9.5 use SessionIpInfo.LocalPort instead }
{$ENDIF} // USE_SSL
    public
        procedure   StartConnection; virtual;
        procedure   TriggerSessionClosed(ErrCode : Word); override;
        procedure   Dup(newHSocket : TSocket); override;
        function    GetPeerAddr: String; override;
        function    GetPeerPort: String; override;
        property    Server : TCustomWSocketServer read  FServer
                                                  write FServer;
        property    CliId : Integer               read  FCliId              { angus V7.00 }
                                                  write FCliId;
        property    CPeerAddr : String            read  FPeerAddr;        { V8.45 }
        property    CPeerPort : String            read  FPeerPort;        { V8.45 }
{$IFDEF USE_SSL}
        property    IcsHostIdx : Integer          read FIcsHostIdx;       { V8.45 }
        property    MultiListenIdx : Integer      read FMultiListenIdx;   { V8.45 }
        property    HostTag : String              read FHostTag;          { V8.45 }
        property    CServerAddr : String          read FServerAddr;      { V8.45 }
        property    CServerPort : String          read FServerPort;      { V8.45 }
{$ENDIF} // USE_SSL
    published
        property    Banner : String               read  FBanner
                                                  write FBanner;
    end;

    { TWSocketServer is made for listening for tcp client connections.      }
    { For each connection, it instanciate a new TWSocketClient (or derived) }
    { to handle connection. Use ClientClass to specify your derived.        }
    TCustomWSocketServer = class(TWSocket)
    private
        procedure ReadBannerValue(Reader: TReader);
        procedure WriteBannerValue(Writer: TWriter);
        procedure WriteBannerTooBusyValue(Writer: TWriter);
        procedure ReadBannerTooBusyValue(Reader: TReader);
        function  IsBannerStored: Boolean;
        function  IsBannerTooBusyStored: Boolean;
    protected
        FBanner                 : String;
        FBannerTooBusy          : String;
        FClientClass            : TWSocketClientClass;
        FClientList             : TList;
        FClientNum              : Integer;
        FMaxClients             : Integer;
        FMsg_WM_CLIENT_CLOSED   : UINT;
        FBindPort               : string;                           { V8.64 }
        FOnClientCreate         : TWSocketClientCreateEvent;
        FOnClientConnect        : TWSocketClientConnectEvent;
        FOnClientDisconnect     : TWSocketClientConnectEvent;
        FOnClientAcceptFilter   : TClientAcceptFilterEvent;         { V9.5 }
        procedure DefineProperties(Filer: TFiler); override;
        procedure WndProc(var MsgRec: TMessage); override;
        procedure Notification(AComponent: TComponent; operation: TOperation); override;
        procedure TriggerSessionAvailable(Error : Word); override;
        procedure TriggerCliAcceptFilter(Client: TWSocketClient; var SessIpInfo: TIcsSessIpInfo; var Allowed: Boolean); virtual; { V9.5 }
        procedure TriggerClientCreate(Client : TWSocketClient); virtual;
        procedure TriggerClientConnect(Client : TWSocketClient; Error : Word); virtual;
        procedure TriggerClientDisconnect(Client : TWSocketClient; Error : Word); virtual;
        function  GetClientCount : Integer; virtual;
        function  GetClient(nIndex : Integer) : TWSocketClient; virtual;
        procedure WMClientClosed(var msg: TMessage); virtual;
        function  MsgHandlersCount: Integer; override;
        procedure AllocateMsgHandlers; override;
        procedure FreeMsgHandlers; override;
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        { Check  if a given object is one of our clients }
        function  IsClient(SomeThing : TObject) : Boolean;
        procedure Listen; override;                                   { V8.64 }
        function  AcceptCli(Client: TWSocketClient): TSocket; virtual;  { V9.5 }
        procedure Disconnect(Client: TWSocketClient; ErrCode: Integer = WSAECONNABORTED); virtual; { V9.4 added ErrorCode }
        procedure DisconnectAll; virtual;                             { angus V6.01 }
        property  BindPort: string                    read FBindPort; { V8.64 }
    protected
        { TWSocketClient derived class to instanciate for each client }
        property  ClientClass            : TWSocketClientClass
                                                      read  FClientClass
                                                      write FClientClass;
        { How many active clients we currently have }
        property  ClientCount   : Integer             read  GetClientCount;
        { Client[] give direct access to anyone of our clients }
        property  Client[nIndex : Integer] : TWSocketClient
                                                      read  GetClient;
    published
        { Banner sent to client as welcome message. Can be empty. }
        property  Banner                 : String     read  FBanner
                                                      write FBanner
                                                      stored IsBannerStored;
        property  BannerTooBusy          : String     read  FBannerTooBusy
                                                      write FBannerTooBusy
                                                      stored IsBannerTooBusyStored;
        property  MaxClients             : Integer    read  FMaxClients
                                                      write FMaxClients
                                                      default 0;
        { Triggered when a client disconnect }
        property  OnClientDisconnect     : TWSocketClientConnectEvent
                                                      read  FOnClientDisconnect
                                                      write FOnClientDisconnect;
        { Triggered when a new client is connecting }
        property  OnClientConnect        : TWSocketClientConnectEvent
                                                      read  FOnClientConnect
                                                      write FOnClientConnect;
        { Triggered when a new client component has been created }
        property  OnClientCreate         : TWSocketClientCreateEvent
                                                      read  FOnClientCreate
                                                      write FOnClientCreate;
        { Triggered before a new client is connected, so it can be refused by IP address }
        property OnClientAcceptFilter: TClientAcceptFilterEvent  read  FOnClientAcceptFilter
                                                                 write FOnClientAcceptFilter;     { V9.5 }
    end;

    TWSocketServer = class;

    TCustomMultiListenWSocketServer = class;

    TWSocketMultiListenItem = class(TCollectionItem {$IFDEF POSIX}, IIcsEventSource{$ENDIF})
    private
      FAddr: string;
      FHSocket: TSocket;
      FListenBacklog: Integer;
      FPort: string;
      FSocketFamily: TSocketFamily;
      FOldSocketFamily: TSocketFamily;
      FState: TSocketState;
      FPortNum: Integer;
      FLastError: Integer;
      FCloseInvoked: Boolean;
      FPaused: Boolean;
      FSelectEvent: Cardinal;
{$IFDEF USE_SSL}
      FSslEnable : Boolean;         { V8.36 moved from SSL class }
{$ENDIF} // USE_SSL
      FBindPort: string;                           { V8.64 }
      procedure SetAddr(const Value: string);
      procedure SetSocketFamily(const Value: TSocketFamily);
      function GetAddrResolved: string;
  {$IFDEF POSIX} { IIcsEventSource }
    strict private
      FPxEventMask        : Cardinal;
      FPxFileDescriptor   : Integer;
      FPxEventState       : TIcsAsyncEventState;
      FPxEventMessageID   : UINT;
      FPxEventWindow      : HWND;
      FPxObjectID         : NativeInt;
      function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
      function  GetEventMask: Cardinal;
      procedure SetEventMask(const AValue: Cardinal);
      function  GetNotifyMessageID: UINT;
      procedure SetNotifyMessageID(const AValue: UINT);
      function  GetNotifyWindow: HWND;
      function  GetEventState: TIcsAsyncEventState;
      function  GetFileDescriptor: Integer;
      procedure SetFileDescriptor(const AValue: Integer);
      function  GetObject: TObject;
      procedure SetEventState(const AValue: TIcsAsyncEventState);
      procedure SetNotifyWindow(const AValue: HWND);
      function  GetObjectID: NativeInt;
  {$ENDIF POSIX IIcsEventSource}
    protected
      procedure AssignDefaults; virtual;
      procedure SetCloseInvoked(const AValue: Boolean);
      function  GetCloseInvoked: Boolean;
      property  CloseInvoked: Boolean read GetCloseInvoked write SetCloseInvoked;
    public
      Fsin: TSockAddrIn6;
      constructor Create(Collection: TCollection); override;
      destructor Destroy; override;
      procedure Close;
      procedure Listen;
      function  OwnerServer: TCustomMultiListenWSocketServer;
      function  Pause: Boolean;
      function  Resume: Boolean;
      property  AddrResolved: string read GetAddrResolved;
      property  HSocket: TSocket read FHSocket write FHSocket;
      property  LastError: Integer read FLastError write FLastError;
      property  Paused: Boolean read FPaused;
      property  PortNum: Integer read FPortNum write FPortNum;
      property  State: TSocketState read FState write FState;
      function  SetAddressListChangeNotification: Boolean;
      function  SetRoutingInterfaceChangeNotification: Boolean;
      property  SelectEvent: Cardinal read FSelectEvent;
      property  BindPort: string read FBindPort;                      { V8.64 }
    published
      property Addr: string read FAddr write SetAddr;
      property ListenBacklog: Integer           read  FListenBacklog
                                                write FListenBacklog default 5;
      property Port: string read FPort write FPort;
      property SocketFamily: TSocketFamily      read  FSocketFamily
                                                write SetSocketFamily
                                                default DefaultSocketFamily;
 {$IFDEF USE_SSL}
      property SslEnable : Boolean read FSslEnable write FSslEnable;   { V8.36 moved from SSL class }
{$ENDIF} // USE_SSL
   end;

    TWSocketMultiListenItemClass = class of TWSocketMultiListenItem;

    TWSocketMultiListenCollection = class(TOwnedCollection)
    protected
      function GetItem(Index: Integer): TWSocketMultiListenItem;
        {$IFDEF USE_INLINE} inline; {$ENDIF}
      procedure SetItem(Index: Integer; Value: TWSocketMultiListenItem);
        {$IFDEF USE_INLINE} inline; {$ENDIF}
    public
      constructor Create(AOwner     : TPersistent;
                         AItemClass : TWSocketMultiListenItemClass);
      function Add: TWSocketMultiListenItem;
          {$IFDEF USE_INLINE} inline; {$ENDIF}
      function FindItemIndex(const AHSocket: TSocket): Integer;
          {$IFDEF USE_INLINE} inline; {$ENDIF}
      function FindItemHandle(const AHSocket: TSocket): TWSocketMultiListenItem;
      function FindItemID(ID: Integer): TWSocketMultiListenItem;
          {$IFDEF USE_INLINE} inline; {$ENDIF}
      function Insert(Index: Integer): TWSocketMultiListenItem;
          {$IFDEF USE_INLINE} inline; {$ENDIF}
      function Owner: TCustomMultiListenWSocketServer;
      property Items[Index: Integer]: TWSocketMultiListenItem
                                                        read  GetItem
                                                        write SetItem; default;
    end;

    TCustomMultiListenWSocketServer = class(TCustomWSocketServer)
    private
        FMultiListenSockets: TWSocketMultiListenCollection;
        FMultiListenIndex: Integer;
    protected
        procedure Ml_Do_FD_CLOSE(AItem: TWSocketMultiListenItem;
                                  AMsg: TMessage); virtual;
        procedure MlListen(AItem: TWSocketMultiListenItem); virtual;
        procedure MlClose(AItem: TWSocketMultiListenItem); virtual;
        procedure MlSocketError(AItem: TWSocketMultiListenItem;
            const ASockFunc: String; ALastError: Integer = 0;
                FriendlyMsg: String = ''); virtual;  { V8.36 added FriendlyMsg }
        procedure MlPause(AItem: TWSocketMultiListenItem); virtual;
        procedure MlResume(AItem: TWSocketMultiListenItem); virtual;
        procedure MlSetAddr(var FldAddr              : string;
                            var FldSocketFamily      : TSocketfamily;
                            const FldOldSocketFamily : TSocketfamily;
                            const NewValue           : string); virtual;
        procedure MlSetSocketFamily(var FldSocketFamily    : TSocketfamily;
                                    var FldOldSocketFamily : TSocketfamily;
                                    const NewValue         : TSocketFamily);
        function  MultiListenItemClass: TWSocketMultiListenItemClass; virtual;
        procedure SetMultiListenIndex(const Value: Integer);
        procedure TriggerClientConnect(Client: TWSocketClient; Error: Word); override;
        procedure WMASyncSelect(var msg: TMessage); override;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        function  AcceptCli(Client: TWSocketClient): TSocket; override;   { V9.5 }
        procedure Close; override;
        procedure Listen; override;
        procedure MultiListen; virtual;
        function  MultiListenEx: string; virtual;                    { V8.49 }
        procedure MultiClose; virtual;
        procedure ThreadAttach; override;
        procedure ThreadDetach; override;
        function  ListenAllOK: Boolean;                              { V8.48 }
        function  ListenAny: Boolean ;                               { V9.1 }
        function  ListenStates: String;                              { V8.48 }
        property  MultiListenIndex: Integer read  FMultiListenIndex;

        property  MultiListenSockets: TWSocketMultiListenCollection
                                                      read  FMultiListenSockets
                                                      write FMultiListenSockets;
    end;

    TWSocketServer = class(TCustomMultiListenWSocketServer)
    public
        property  ClientClass;
        property  ClientCount;
        property  Client;
    published
    {$IFNDEF NO_DEBUG_LOG}
        property  IcsLogger;                                 { V5.04 }
    {$ENDIF}
        property  Banner;
        property  BannerTooBusy;
        property  MaxClients;
        property  MultiListenSockets;
        property  OnClientDisconnect;
        property  OnClientConnect;
        property  OnClientAcceptFilter;                      { V9.5 }
    end;

{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}

type

  TClientAlpnChallgEvent = procedure (Sender: TObject; const Host: string; var CertFName: string) of object;   { V8.62 }

{ TIcsHost defines the bindings for an SSL TCP/IP server, usually a web server,
   such as which address and port to listen, which host names to accept using
   which SSL certificate, and server related set-up such as directories }

  TIcsHost = class(TCollectionItem)    { V8.45 }
  private
    FHostNames: TStrings;
    FHostEnabled: boolean;
    FBindIpAddr: String;
    FBindIpAddr2: String;
    FBindSslPort: integer;
    FBindNonPort: integer;
    FHostTag: String;
    FDescr: String;
    FProto: String;
    FForwardProxy: Boolean;
    FWebDocDir: string;
    FWebTemplDir: string;
    FWebDefDoc: string;
    FWebLogDir: string;
    FSslCert: string;
    FSslKey: string;
    FSslInter: string;
    FSslPassword: string;
    FSslSrvSecurity: TSslSrvSecurity;
    FCertDomains: string;
    FCertInfo: string;
    FBindInfo: string;
    FCertExiry: TDateTime;
    FCertFStamp: TDateTime;    { V8.51 was integer }
    FInterFStamp: TDateTime;   { V8.51 was integer }
    FCertErrs: string;
    FCertValRes: TChainResult;
    FBindIdxNone: Integer;
    FBindIdxSsl: Integer;
    FBindIdx2None: Integer;
    FBindIdx2Ssl: Integer;
    FWellKnownPath: string;    { V8.49 }
    { see http://www.iana.org/assignments/well-known-uris/well-known-uris.xhtml }
    FWebRedirectURL: string;   { V8.49 }
    FWebRedirectStat: integer; { V8.49 }
  { V8.57 following are for automatic ordering and installation of SSL certificates }
    FCertSupplierProto: TSupplierProto;
    FCertDirWork: String;
    FCertChallenge: TChallengeType;
    FCertPKeyType: TSslPrivKeyType;
    FCertProduct: String;   { V9.5 no longer used, but still published }
    FCertSignDigest: TEvpDigest;
    FWebLogIdx: Integer;    { V8.60 web logging }
    FAuthSslCmd: Boolean;   { V8.63 }
    FBindSrvPort: string;   { V8.64 our port for reporting }
    FAuthForceSsl: Boolean; { V8.64 non SSL connection may negotiate SSL later }
    FOcspStapleResp: AnsiString;          { V8.69 send during initial HELO handshake when checking SNI }
    FCliCertMethod: TSslCliCertMethod;    { V8.71 initial HELO requests a certificate from the client for authentication }
    FSslLoadSource: TSslLoadSource;       { V8.71 SSL certificate source in Windows Store }
    FSslCipherList12: String;             { V9.3 TLSv1.2 cipher lists }
    FSslCipherList13: String;             { V9.3 TLSv1.3 cipher lists }
    FSslCryptoGroups: String;             { V9.3 crypto groups, 'P-256:X25519:P-384:P-521' }
    FSslRawKeyFile: String;               { V9.5 raw private key file for Raw Public Key (RPK) without certificate }
    FAcmeSupplier: TAcmeSupplier;         { V9.5 account ACME supplier }
    FSupplierTitle: String;               { V9.5 account title, used to look up CertDirWork from ics-acme-accounts.db  }
    FAcmeCertProfile: String;             { V9.5 ACME certificate profile, default classic, also tlsserver or shortlived }
    FRenewalDays: Integer;                { V9.5 from CertRenewalDays or calculated from renewal-request date }
    FRenewRetryDT: TDateTime;             { V9.5 when to update renew dates, they may change }
    FRenewCheckDT: TDateTime;             { V9.5 when last checked renew dates }
    FRenewalId: String;                   { V9.5 certificate renewal identifier, for renewal-request }
    FAcmeCertValidity: Integer;           { V9.5 certificate expiry days, default 90, Google only at present }
    FOrderPending: Boolean;               { V9.5 new certificate order in progress, cleared when certificate loaded }
  protected
    function GetDisplayName: string; override;
    function GetHostNameTot: integer;
    procedure SetHostNames(Value : TStrings);
  public
    SslCtx: TSslContext;
    property HostNameTot : Integer               read  GetHostNameTot;
    property DisplayName : String                read  GetDisplayName;
    property CertDomains : String                read  FCertDomains;
    property CertInfo : String                   read  FCertInfo;
    property BindInfo : String                   read  FBindInfo;
    property CertExiry : TDateTime               read  FCertExiry;
    property CertFStamp: TDateTime               read  FCertFStamp;
    property InterFStamp: TDateTime              read  FInterFStamp;
    property CertErrs : String                   read  FCertErrs;
    property CertValRes : TChainResult           read  FCertValRes;
    property BindIdxNone : Integer               read  FBindIdxNone;
    property BindIdxSsl : Integer                read  FBindIdxSsl;
    property BindIdx2None : Integer              read  FBindIdx2None;
    property BindIdx2Ssl : Integer               read  FBindIdx2Ssl;
    property OcspStapleResp: AnsiString          read  FOcspStapleResp
                                                 write FOcspStapleResp;  { V8.69 }
    property RenewRetryDT: TDateTime             read  FRenewRetryDT;    { V9.5 when to update renew dates, they may change }
    property RenewCheckDT: TDateTime             read  FRenewCheckDT;    { V9.5 when last checked renew dates }
  published
    constructor Create (Collection: TCollection); Override ;
    destructor Destroy; override;
    property HostNames : TStrings                read  FHostNames
                                                 write SetHostNames;
    property HostEnabled : boolean               read  FHostEnabled
                                                 write FHostEnabled;
    property BindIpAddr : String                 read  FBindIpAddr
                                                 write FBindIpAddr;
    property BindIpAddr2 : String                read  FBindIpAddr2
                                                 write FBindIpAddr2;
    property BindSslPort : Integer               read  FBindSslPort
                                                 write FBindSslPort;
    property BindNonPort : Integer               read  FBindNonPort
                                                 write FBindNonPort;
    property HostTag: String                     read  FHostTag
                                                 write FHostTag;
    property Descr: String                       read  FDescr
                                                 write FDescr;
    property Proto: String                       read  FProto
                                                 write FProto;
    property ForwardProxy : Boolean              read  FForwardProxy
                                                 write FForwardProxy;
    property WebDocDir : String                  read  FWebDocDir
                                                 write FWebDocDir;
    property WebTemplDir : String                read  FWebTemplDir
                                                 write FWebTemplDir;
    property WebDefDoc : String                  read  FWebDefDoc
                                                 write FWebDefDoc;
    property WebLogDir : String                  read  FWebLogDir
                                                 write FWebLogDir;
    property WebLogIdx : Integer                 read  FWebLogIdx
                                                 write FWebLogIdx; { V8.60 }
    property SslCert : String                    read  FSslCert
                                                 write FSslCert;
    property SslKey : String                     read  FSslKey
                                                 write FSslKey;
    property SslInter : String                   read  FSslInter
                                                 write FSslInter;
    property SslPassword : String                read  FSslPassword
                                                 write FSslPassword;
    property SslSrvSecurity : TSslSrvSecurity    read  FSslSrvSecurity
                                                 write FSslSrvSecurity;
    property AuthSslCmd : Boolean                read  FAuthSslCmd
                                                 write FAuthSslCmd;      { V8.63 }
    property AuthForceSsl: Boolean               read  FAuthForceSsl
                                                 write FAuthForceSsl;    { V8.64 }
    property WellKnownPath: string               read  FWellKnownPath
                                                 write FWellKnownPath;   { V8.49 }
    property WebRedirectURL: string              read  FWebRedirectURL
                                                 write FWebRedirectURL;  { V8.49 }
    property WebRedirectStat: integer            read  FWebRedirectStat
                                                 write FWebRedirectStat; { V8.49 }
    property CliCertMethod: TSslCliCertMethod    read  FCliCertMethod
                                                 write FCliCertMethod;   { V8.71 }
    property SslLoadSource: TSslLoadSource       read  FSslLoadSource
                                                 write FSslLoadSource;   { V8.71 }
    property SslCipherList12: String             read  FSslCipherList12
                                                 write FSslCipherList12;   { V9.3 TLSv1.2 cipher lists }
    property SslCipherList13: String             read  FSslCipherList13
                                                 write FSslCipherList13;   { V9.3 TLSv1.3 cipher lists }
    property SslCryptoGroups: String             read  FSslCryptoGroups
                                                 write FSslCryptoGroups;   { V9.3 }
    property SslRawKeyFile: String               read  FSslRawKeyFile
                                                 write FSslRawKeyFile;     { V9.5 raw private key file for Raw Public Key (RPK) without certificate }
  { V8.57 following are for automatic ordering and installation of SSL certificates }
    property CertSupplierProto: TSupplierProto   read  FCertSupplierProto
                                                 write FCertSupplierProto;
    property CertDirWork: String                 read  FCertDirWork
                                                 write FCertDirWork;
    property CertChallenge: TChallengeType       read  FCertChallenge
                                                 write FCertChallenge;
    property CertPKeyType: TSslPrivKeyType       read  FCertPKeyType
                                                 write FCertPKeyType;
    property CertProduct: String                 read  FCertProduct      { V9.5 no longer used }
                                                 write FCertProduct;
    property CertSignDigest: TEvpDigest          read  FCertSignDigest
                                                 write FCertSignDigest;
    property SupplierTitle: String               read  FSupplierTitle
                                                 write FSupplierTitle;    { V9.5 }
    property AcmeSupplier: TAcmeSupplier         read  FAcmeSupplier
                                                 write FAcmeSupplier;     { V9.5 }
    property AcmeCertProfile: String             read  FAcmeCertProfile
                                                 write FAcmeCertProfile;  { V9.5 }
    property RenewalDays: integer                read  FRenewalDays
                                                 write FRenewalDays;       { V9.5 }
    property AcmeCertValidity: Integer           read  FAcmeCertValidity
                                                 write FAcmeCertValidity; { V9.5 }
  end;

  { TIcsHosts defines a collection of TIcsHost }

  TIcsHostCollection = class(TCollection)     { V8.45 }
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TIcsHost;
    procedure SetItem(Index: Integer; Value: TIcsHost);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent);
    property Items[Index: Integer]: TIcsHost read GetItem write SetItem; default;
  end;

type
    TBeforeCtxInitEvent = procedure (Sender: TObject; IcsHost: TIcsHost) of object;  { V8.66 }

    TSslWSocketMultiListenItem = class(TWSocketMultiListenItem)
    public
      constructor Create(Collection: TCollection); override;
    end;

    TSslWSocketClient = class(TWSocketClient)
    public
        AlpnSslCtx: TSslContext;                          { V8.64 used during tls-alpn-01 cert swapping }
        OnClientAlpnChallg: TClientAlpnChallgEvent;       { V8.62 }
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;                    { V8.62 }
        procedure   StartConnection; override;
        procedure   TriggerSslServerName(var Ctx: TSslContext; var ErrCode: TTlsExtError); override; { V8.45 }
        procedure   TriggerSslAlpnSelect(ProtoList: TStrings; var SelProto: String; var ErrCode: TTlsExtError); override; { V8.56 }
    end;

    TSslWSocketServer = class(TWSocketServer)
    protected
        FIcsHosts: TIcsHostCollection;            { V8.45 }
//      FX509CAList: TX509List;                   { V9.1 now using IcsSslRootCAStore }
        FRootCA: String;                          { V8.46 }
        FDHParams: String;                        { V8.45 }
        FValidated: Boolean;                      { V8.48 }
        FSslCliCertMethod: TSslCliCertMethod;     { V8.57 }
        FSslCertAutoOrder: Boolean;               { V8.57 }
        FCertExpireDays: Integer;                 { V8.57 }
        FNoSSL: Boolean;                          { V9.1 }
        FAcmeAccountOpen: Boolean;                { V9.5 has ACME account been opened for renewal check, must be closed }
     { should be TSslX509Certs but causes circular reference, so need to cast }
{$IFDEF AUTO_X509_CERTS}  { V8.59 }
        FSslX509Certs: TComponent;                                  { V8.57 }
{$ENDIF} // AUTO_X509_CERTS
        FOcspSrvStapling: Boolean;                                  { V8.69 }
        FOcspSrvHttp: TOcspHttp;                                    { V8.69 }
        FonBeforeContextInit: TBeforeCtxInitEvent;                  { V8.66 }
     // following only published if AUTO_X509_CERTS set
        FSrvSupplierTitle: String;               { V9.5 account title, used to look up CertDirWork from ics-acme-accounts.db  }
        FSrvAcmeSupplier: TAcmeSupplier;         { V9.5 account ACME supplier }
        FSrvAcmeCertProfile: String;             { V9.5 ACME certificate profile, default classic, also tlsserver or shortlived }
        FSrvAcmeCertValidity: Integer;           { V9.5 certificate expiry days, default 90, Google only at present }
        FSrvCertChallenge: TChallengeType;       { V9.5 ACME certificate DNS challenge }
        FSrvCertPKeyType: TSslPrivKeyType;       { V9.5 SSL private key type, RSA or EC }
        FSrvCertSignDigest: TEvpDigest;          { V9.5 SSL certificate sign digest, sha256 or better }
        procedure TriggerClientConnect(Client : TWSocketClient; Error : Word); override;
        function  MultiListenItemClass: TWSocketMultiListenItemClass; override;
    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        procedure Close; override;                                   { V8.63 }
        property  ClientClass;
        property  ClientCount;
        property  Client;
        property  SslMode;
        procedure Listen; override;
        function  GetIcsHosts: TIcsHostCollection;                   { V8.45 }
        procedure SetIcsHosts(const Value: TIcsHostCollection);      { V8.45 }
        function  FindBinding(const MAddr: String; MPort: Integer;
                                 var MIndex: Integer): boolean;      { V8.45 }
        function  ValidateHosts(Stop1stErr: Boolean=True; NoExceptions: Boolean=False;
                                          AllowSelfSign: Boolean=False): String; virtual; { V8.48, V8.63 }
        function  RecheckSslCerts(var CertsInfo: String; Stop1stErr: Boolean=True;
                        NoExceptions: Boolean=False; AllowSelfSign: Boolean=False): Boolean; { V8.48, V8.63 }
        function  LoadOneCert(HostNr: Integer; ForceLoad: Boolean;
                                var LoadNew: Boolean; AllowSelfSign: Boolean=False): Boolean; { V8.57, V8.63 }
        function  DoProcOrderCert(HostNr: Integer): Boolean;           { V8.57 }
        function  DoCheckOrderCert(HostNr: Integer): Boolean;          { V9.5 }
        procedure IcsLogEvent(Sender: TObject; LogOption: TLogOption; const Msg : String);    { V8.69 }
        procedure OrderClose;                                        { V8.63 }
        procedure LoadRootCAList;                                    { V8.64 }
{$IFDEF AUTO_X509_CERTS}  { V8.59 }
        function  GetSslX509Certs: TComponent;                       { V8.57 }
        procedure SetSslX509Certs(const Value : TComponent);         { V8.57 }
{$ENDIF} // AUTO_X509_CERTS
        property  Validated: Boolean                     read FValidated;    { V8.64 }
   //     property  RootCAList: TX509List                  read  FX509CAList;  { V9.1 probably not used! }
    published
        property  SslContext;
        property  Banner;
        property  BannerTooBusy;
        property  MaxClients;
        property  OnClientDisconnect;
        property  OnClientConnect;
        property  SslEnable;
        property  SslAcceptableHosts;
        property  IcsHosts: TIcsHostCollection           read  GetIcsHosts
                                                         write SetIcsHosts;   { V8.45 }
        property  RootCA: String                         read  FRootCA
                                                         write FRootCA;       { V8.46 }
        property  DHParams: String                       read  FDHParams
                                                         write FDHParams;     { V8.45 }
        property  SslCliCertMethod: TSslCliCertMethod    read  FSslCliCertMethod
                                                         write FSslCliCertMethod; { V8.57 }
        property  SslCertAutoOrder: Boolean              read  FSslCertAutoOrder
                                                         write FSslCertAutoOrder; { V8.57 }
        property  CertExpireDays: Integer                read  FCertExpireDays
                                                         write FCertExpireDays;   { V8.57 }
        property  NoSSL: Boolean                         read  FNoSSL
                                                         write FNoSSL;            { V9.1 }
{$IFDEF AUTO_X509_CERTS}  { V8.59 }
        property  SslX509Certs: TComponent               read  GetSslX509Certs
                                                         write SetSslX509Certs;      { V8.57 }
        property  SrvSupplierTitle: String               read  FSrvSupplierTitle
                                                         write FSrvSupplierTitle;    { V9.5 }
        property  SrvAcmeSupplier: TAcmeSupplier         read  FSrvAcmeSupplier
                                                         write FSrvAcmeSupplier;     { V9.5 }
        property  SrvAcmeCertProfile: String             read  FSrvAcmeCertProfile
                                                         write FSrvAcmeCertProfile;  { V9.5 }
        property  SrvAcmeCertValidity: Integer           read  FSrvAcmeCertValidity
                                                         write FSrvAcmeCertValidity; { V9.5 }
        property  SrvCertChallenge: TChallengeType       read  FSrvCertChallenge
                                                         write FSrvCertChallenge;    { V9.5 }
        property  SrvCertPKeyType: TSslPrivKeyType       read  FSrvCertPKeyType
                                                         write FSrvCertPKeyType;     { V9.5 }
        property  SrvCertSignDigest: TEvpDigest          read  FSrvCertSignDigest
                                                         write FSrvCertSignDigest;   { V9.5 }
{$ENDIF} // AUTO_X509_CERTS
        property  OcspSrvStapling: Boolean               read  FOcspSrvStapling
                                                         write FOcspSrvStapling;   { V8.69 }
        property  OcspSrvHttp: TOcspHttp                 read  FOcspSrvHttp
                                                         write FOcspSrvHttp;       { V8.69 }
        property  OnSslVerifyPeer;
        property  OnSslSetSessionIDContext;
        property  OnSslSvrNewSession;
        property  OnSslSvrGetSession;
        property  OnSslHandshakeDone;
        property  OnSslServerName;    { V8.07 }
        property  OnSslAlpnSelect;    { V8.56 }
        property  OnBeforeContextInit: TBeforeCtxInitEvent  read  FonBeforeContextInit
                                                            write FonBeforeContextInit;      { V8.66 }
  end;

{ public functions }
function IcsLoadIcsHostsFromIni(MyIniFile: TCustomIniFile; IcsHosts: TIcsHostCollection;
                                 const Prefix: String = 'IcsHost'; Server: TWSocketServer = Nil): Integer;  { V9.5 added Server }

{$ENDIF} // USE_SSL

implementation

{$IFDEF USE_SSL}
{$IFDEF AUTO_X509_CERTS}  { V8.59 }
{$IFDEF FMX}
Uses
    Z.ICS9.Ics.Fmx.OverbyteIcsSslX509Certs;  { V8.57 }
{$ELSE}
Uses
    Z.ICS9.OverbyteIcsSslX509Certs; { V8.57 }
{$ENDIF FMX}
{$ENDIF AUTO_X509_CERTS}
{$ENDIF USE_SSL}

const
    DefaultBanner            = 'Welcome to OverByte ICS TcpSrv';
    DefaultBannerTooBusy     = 'Sorry, too many clients';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.DefineProperties(Filer: TFiler);
begin
    inherited DefineProperties(Filer);
    Filer.DefineProperty('Banner', ReadBannerValue, WriteBannerValue, (Banner = ''));
    Filer.DefineProperty('BannerTooBusy', ReadBannerTooBusyValue,
      WriteBannerTooBusyValue, (BannerTooBusy = ''));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocketServer.IsBannerStored: Boolean;
begin
    Result := Banner <> DefaultBanner;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocketServer.IsBannerTooBusyStored: Boolean;
begin
  Result := BannerTooBusy <> DefaultBannerTooBusy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.WriteBannerValue(Writer: TWriter);
begin
    Writer.WriteString(Banner);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.WriteBannerTooBusyValue(Writer: TWriter);
begin
    Writer.WriteString(BannerTooBusy);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.ReadBannerValue(Reader: TReader);
begin
    Banner := Reader.ReadString;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.ReadBannerTooBusyValue(Reader: TReader);
begin
    BannerTooBusy := Reader.ReadString;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomWSocketServer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FClientList      := TList.Create;
    FClientClass     := TWSocketClient;
    FBanner          := DefaultBanner;
    FBannerTooBusy   := DefaultBannerTooBusy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomWSocketServer.Destroy;
var
    I : Integer;
begin
    try
        if Assigned(FClientList) then begin
            { We need to destroy all clients }
            for I := FClientList.Count - 1 downto 0 do begin
                try
                    TWSocketClient(FClientList.Items[I]).Free;
                except
                    { Ignore any exception here }
                end;
            end;
            { Then we can destroy client list }
            FreeAndNil(FClientList);
        end;
    { And finally destroy ourself }
    finally
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocketServer.MsgHandlersCount : Integer;
begin
    Result := 1 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_CLIENT_CLOSED := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then
        FWndHandler.UnregisterMessage(FMsg_WM_CLIENT_CLOSED);
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Message handler                                                           }
procedure TCustomWSocketServer.WndProc(var MsgRec: TMessage);
begin
    with MsgRec do begin
        if Msg = FMsg_WM_CLIENT_CLOSED then begin
            { We *MUST* handle all exception to avoid application shutdown }
            try
                WMClientClosed(MsgRec)
            except
                on E:Exception do
                    HandleBackGroundException(E, 'TCustomWSocketServer.WndProc');
            end;
        end
        else
            inherited WndProc(MsgRec);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Called by destructor when child component (a clients) is create or        }
{ destroyed.                                                                }
procedure TCustomWSocketServer.Notification(
    AComponent : TComponent;
    Operation  : TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Assigned(FClientList) and (AComponent is TWSocketClient) then begin
        if Operation = opInsert then
            { A new client has been created, add it to our list }
            FClientList.Add(AComponent)
        else if Operation = opRemove then
            { If one of our client has been destroyed, remove it from our list }
            FClientList.Remove(AComponent);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.TriggerCliAcceptFilter(Client: TWSocketClient; var SessIpInfo: TIcsSessIpInfo; var Allowed: Boolean);  { V9.5 }
begin
    if Assigned(FOnClientAcceptFilter) then
        FOnClientAcceptFilter(Self, Client, SessIpInfo, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 WSAAccept callback, keep remote and local IP addresses, onSessAcceptFilter event allows connection to be refused }
function WSAAcceptCallBack(lpCallerId: LPWSABUF; lpCallerData: LPWSABUF; lpSQOS: LPQOS; lpGQOS: LPQOS;
                lpCalleeId: LPWSABUF; lpCalleeData: LPWSABUF;  g: PCardinal;  dwCallbackData: Pointer): Integer; stdcall;
var
    Allowed: Boolean;
    Client: TWSocketClient;
begin
    Allowed := True;
    Result := Ics_CF_ACCEPT;
    Client := TWSocketClient(dwCallbackData);
    if not Assigned(Client) then
       raise Exception.Create('WSAAcceptCallBack no object error!');
    try
        if lpCallerId.len > SizeOf(TSockAddrIn6) then  // sanity check
            Exit;
        with Client.FSessIpInfo do begin
            Move(lpCalleeId.buf^, SocLocalAddr, lpCalleeId.len);
            Move(lpCallerId.buf^, SocRemoteAddr, lpCallerId.len);
            LocalAddr := WSocketSockAddrToStr(SocLocalAddr);
            LocalPort := IntToStr(Swap(SocLocalAddr.sin6_port));
            RemoteAddr := WSocketSockAddrToStr(SocRemoteAddr);
            RemotePort := IntToStr(Swap(SocRemoteAddr.sin6_port));
            SocFamily := WSocketFamilyFromAF(SocLocalAddr.sin6_family);
            SocType := SOCK_STREAM;
            Proto := IPPROTO_TCP;
            StartTick := IcsGetTickCount64;
            Client.FPeerAddr := RemoteAddr;   { V9.5 previously done in DUP, save two API calls }
            Client.FPeerPort := RemotePort;
{$IFDEF USE_SSL}     { V9.6 }
            Client.FServerAddr := LocalAddr;  { V9.5 }
            Client.FServerPort := LocalPort;
{$ENDIF} // USE_SSL
            ISOA2 := '';   // these can be filled in from GEO MaxMind database using TIcsGeoTools in event
            CountryName :='';  // ditto
            RegionName := '';
            RemoteRDNS := '';  // reverse DNS lookup for server logs
        end;
        Client.Server.TriggerCliAcceptFilter(Client, Client.FSessIpInfo, Allowed);   // pass addresses, see if allowed }
    except
    end;
    if NOT Allowed then
        Result := Ics_CF_REJECT;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocketServer.AcceptCli(Client: TWSocketClient): TSocket;    { V9.5 }
var
    len : Integer;
  {$IFDEF POSIX}
    LastErr: Integer;
  {$ENDIF}
begin
{ no inherited !! }
  {$IFDEF POSIX}
    try
  {$ENDIF}
        if FState <> wsListening then begin
            WSocket_Synchronized_WSASetLastError(WSAEINVAL);
            SocketError('not a listening socket');
            Result := INVALID_SOCKET;
            Exit;
        end;
        len := IcsSizeOfAddr(sin6);

      { V9.5 filter connections by IP address on FOnSessAcceptFilter event }
        FASocket := WSocket_Synchronized_WSAAccept(FHSocket, @sin6, @len, @WSAAcceptCallBack, Client);

      { if callback declines connection, error is WSAECONNREFUSED }
        if WSocket_WSAGetLastError = WSAECONNREFUSED then begin
{$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loWsockInfo) then
                DebugLog(loWsockErr, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' Socket not accepted by user event');
{$ENDIF}
            Result := INVALID_SOCKET;
            LastError := WSAECONNREFUSED;
            Exit;
        end;
        Result := FASocket;

        if (FASocket = INVALID_SOCKET) then begin
          {$IFDEF MSWINDOWS}
            SocketError('AcceptCli');
          {$ENDIF}
          {$IFDEF POSIX}
            LastErr := WSocket_WSAGetLastError;
            if LastErr <> WSAEWOULDBLOCK then
                SocketError('Accept', LastErr);
          {$ENDIF}
            Exit;
        end;
  {$IFDEF POSIX}
    finally
        if FState = wsListening then
            WSocketSynchronizedEnableAcceptEvent(Self);
    end;
  {$ENDIF}

{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loWsockInfo) then
        DebugLog(loWsockInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' ' + 'Socket accepted ' + IntToStr(FASocket));
{$ENDIF}

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Called when a session is available, that is when a client is connecting   }
procedure TCustomWSocketServer.TriggerSessionAvailable(Error : Word);
var
    Client     : TWSocketClient;
    TempHandle : TSocket;
begin
{$IFDEF DEBUG_OUTPUT}
    OutputDebugString('OnSessionAvailable');
{$ENDIF}
    { Call parent event handler }
    inherited TriggerSessionAvailable(Error);
    { In case of error, do nothing }
    if Error <> 0 then
        Exit;

    if Cardinal(FClientNum) >= Cardinal(MaxInt) then    { V7.04 }
        FClientNum := 0;                                { angus V7.00 }
    Inc(FClientNum);
    Client := nil;
    try                                                 { FPiette V7.01 }
        Client := FClientClass.Create(Self);
        Client.FCliId := FClientNum;                   { angus V7.00 }
        Client.OnBgException := FOnBgException;        { angus V8.07 }
        Client.SocketErrs := FSocketErrs;              { V8.37 }
{$IFDEF BUILTIN_THROTTLE}
        Client.BandwidthLimit := Self.BandwidthLimit;        { angus V7.02 may be changed in event for different limit }
        Client.BandwidthSampling := Self.BandwidthSampling;  { angus V7.02 }
{$ENDIF}
        Client.ClearSocketIpInfo;                      { V9.5 }
        TriggerClientCreate(Client);
    except
        try                                              { FPiette V7.01 }
            TempHandle := Self.Accept;
            if TempHandle <> INVALID_SOCKET then
                WSocket_closesocket(TempHandle);
            if Assigned(Client) then
                Client.Free;
        except
            // safely ignore any exception here. Component user may already
            // have accepted and closed the connection.
        end;
        raise;
    end;

    Client.Name := Name + 'Client' + IntToStr(FClientNum);
    Client.Banner := FBanner;
    Client.Server := Self;
{$IFNDEF NO_DEBUG_LOG}
    Client.IcsLogger := IcsLogger;                           { V5.04 }
{$ENDIF}

{$IFDEF MSWINDOWS} { V9.5 was MSCRYPT_Servers since V9.3 }
    Client.HSocket := Self.AcceptCli(Client);   { V9.5 special server version of Accept }
    if (Client.HSocket = INVALID_SOCKET) then begin   { V9.5 event may have declined connection  }
        TriggerClientDisconnect(Client, LastError);   { tell user }
        Client.Free;
        Exit;
    end;
{$ENDIF}

{$IFDEF POSIX}
    TempHandle := Accept;
    { Accept() doesn't raise a socket error for WSAEWOULDBLOCK in POSIX. }
    { IMO Accept() should never raise a socket error here but we should  }
    { call Dup() only if Accept() returned a valid socket handle,        }
    { otherwise pass the error code to TriggerClientConnect() and free   }
    { the client object afterwards, so this is just a workaround.  AG    }
    if (TempHandle = INVALID_SOCKET) and (LastError = WSAEWOULDBLOCK) then
        Error := LastError
    else
        Client.HSocket := TempHandle;
{$ENDIF}

{ user may check and close connection in event handler, log stuff, etc  }
    TriggerClientConnect(Client, Error);
    { The event handler may have destroyed the client ! }
    if FClientList.IndexOf(Client) < 0 then begin
        Client.Free;      { V9.5 }
        Exit;
    end;
{$IFDEF POSIX}
    if Error <> 0 then begin
        Client.Free;
        Exit;
    end;
{$ENDIF}
    { The event handler may have closed the connection }
    if Client.State <> wsConnected then begin
        Client.Free;      { V9.5 }
        Exit;
    end;
    { Ok, the client is still there, process with the connection }
    if (FMaxClients > 0) and (FMaxClients < ClientCount) then begin
        { Sorry, toomuch clients }
        Client.Banner := FBannerTooBusy;
        Client.StartConnection;
        Client.CloseDelayed;  { V8.06 was Close but too quick }
    end
    else
        Client.StartConnection;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.TriggerClientConnect(
    Client : TWSocketClient; Error : Word);
begin
    if Assigned(FOnClientConnect) then
        FOnClientConnect(Self, Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.TriggerClientCreate(Client : TWSocketClient);
begin
    if Assigned(FOnClientCreate) then
        FOnClientCreate(Self, Client);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.TriggerClientDisconnect(
    Client : TWSocketClient; Error : Word);
begin
    if Assigned(FOnClientDisconnect) then
        FOnClientDisconnect(Self, Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ get number of connect clients                                               }
function TCustomWSocketServer.GetClientCount : Integer;
begin
    if Assigned(FClientList) then
        Result := FClientList.Count
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Acces method to return a client by index.                                   }
{ Return nil if index is out of range.                                        }
function TCustomWSocketServer.GetClient(nIndex : Integer) : TWSocketClient;
begin
    if not Assigned(FClientList) then begin
        Result := nil;
        Exit;
    end;
    if (nIndex < 0) or (nIndex >= FClientList.Count) then begin
        Result := nil;
        Exit;
    end;
    Result := TWSocketClient(FClientList.Items[nIndex]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.Listen;     { V8.64 }
begin
    inherited Listen;
    FBindPort := GetXPort;  { V8.64 keep actual port listening, might be random }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Client has closed. Remove it from client list and destroy component.        }
procedure TCustomWSocketServer.WMClientClosed(var msg: TMessage);
var
    Client : TWSocketClient;
    PIdRec : PClientIdRec;
begin
    PIdRec := PClientIdRec(Msg.LParam);  { angus V7.00 }
    try
        Client := TWSocketClient(PIdRec^.PClient);
        { angus V7.00 ensure client not freed already }
        if IsClient(Client) and (Client.CliId = PIdRec^.CliId) then begin
            try
                TriggerClientDisconnect(Client, Msg.WParam);
            finally
                { Calling Free will automatically remove client from list     }
                { because we installed a notification handler.                }
                Client.CliId := 0;  { V8.03 }
                Client.Free;
            end;
        end;
    finally
        System.Dispose(PIdRec);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check if a given object is one of our clients.                              }
function TCustomWSocketServer.IsClient(SomeThing : TObject) : Boolean;
begin
    if not Assigned(FClientList) then
        Result := FALSE
    else
        Result := (FClientList.IndexOf(SomeThing) >= 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.Disconnect(Client: TWSocketClient; ErrCode: Integer = WSAECONNABORTED);    { V9.4 added ErrorCode }
var
    Msg : TMessage;
    PIdRec : PClientIdRec;
begin
    FillChar(Msg, SizeOf(Msg), 0);
{ angus V7.00 pass CliId to WMClientClosed so correct client is closed  }
    try
        New(PIdRec);
        PIdRec^.PClient := Client;
        PIdRec^.CliId   := Client.CliId;
        Msg.WParam      := ErrCode;
        Msg.LParam      := LPARAM(PIdRec);
        WMClientClosed(Msg);    // don't send message since called by DisconnectAll and expect immediate response
    except
    end;
 end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.DisconnectAll;                             { angus V6.01 }
begin
    while ClientCount > 0 do
        Disconnect(Client[0], WSAECONNABORTED);    { V9.4 added ErrorCode }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{*                                                                           *}
{*                   TCustomMultiListenWSocketServer                         *}
{*                                                                           *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(* function SizeOfAddr(const AAddr: TSockAddrIn6): Integer;  moved to Utils
    {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    if AAddr.sin6_family = AF_INET6 then
        Result := SizeOf(TSockAddrIn6)
    else
        Result := SizeOf(TSockAddrIn);
end;  *)


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomMultiListenWSocketServer.AcceptCli(Client: TWSocketClient): TSocket;
var
    Len     : Integer;
    AItem   : TWSocketMultiListenItem;
  {$IFDEF POSIX}
    LastErr : Integer;
  {$ENDIF}
begin
    if FMultiListenIndex = -1 then
    begin
        Result := inherited AcceptCli(Client);
    end
    else begin
      {$IFDEF POSIX}
        AItem := nil;
        try
      {$ENDIF}
            AItem := FMultiListenSockets[FMultiListenIndex];
            if AItem.State <> wsListening then begin
                WSocket_WSASetLastError(WSAEINVAL);
                MlSocketError(AItem, 'not a listening socket');
                Result := INVALID_SOCKET;
                Exit;
            end;
            Len := SizeOf(AItem.Fsin);
          { V9.5 filter connections by IP address on FOnSessAcceptFilter event }
            FASocket := WSocket_Synchronized_WSAAccept(AItem.HSocket, @AItem.Fsin, @Len, @WSAAcceptCallBack, Client);

          { if callback declines connection, error is WSAECONNREFUSED }
            if WSocket_WSAGetLastError =  WSAECONNREFUSED then begin
    {$IFNDEF NO_DEBUG_LOG}
                if CheckLogOptions(loWsockInfo) then
                    DebugLog(loWsockErr, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' Socket not accepted by user event');
    {$ENDIF}
                Result := INVALID_SOCKET;
                Exit;
            end;
            Result := FASocket;
            if FASocket = INVALID_SOCKET then begin
              {$IFDEF MSWINDOWS}
                MlSocketError(AItem, 'AcceptCli');
              {$ENDIF}
              {$IFDEF POSIX}
                LastErr := WSocket_WSAGetLastError;
                if LastErr <> WSAEWOULDBLOCK then
                    MlSocketError(AItem, 'Accept', LastErr);
              {$ENDIF}
                Exit;
            end;
      {$IFDEF POSIX}
        finally
            if (AItem <> nil) and (AItem.State = wsListening) then
                WSocketSynchronizedEnableAcceptEvent(AItem);
        end;
      {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.Close;
begin
    FMultiListenIndex := -1;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomMultiListenWSocketServer.Create(AOwner: TComponent);
begin
    inherited;
    FMultiListenIndex := -1;
    FMultiListenSockets := TWSocketMultiListenCollection.Create(
                              Self, MultiListenItemClass);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomMultiListenWSocketServer.Destroy;
begin
    try  { V8.71 }
        FreeAndNil(FMultiListenSockets);
    finally
        inherited;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.Listen;
begin
    FMultiListenIndex := -1;
    inherited Listen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MlClose(
  AItem: TWSocketMultiListenItem);
var
    iStatus : Integer;
begin
    FMultiListenIndex := AItem.Index;
    if AItem.HSocket = INVALID_SOCKET then
    begin
        AItem.AssignDefaults;
        Exit;
    end;

    if AItem.State = wsClosed then
        Exit;

    if AItem.HSocket <> INVALID_SOCKET then begin
        repeat
            { Close the socket }
            iStatus := WSocket_closesocket(AItem.HSocket);
            if iStatus <> 0 then begin
                AItem.LastError := WSocket_WSAGetLastError;
                if AItem.LastError <> WSAEWOULDBLOCK then begin
                  {$IFDEF POSIX}
                    WSocketSynchronizedRemoveEvents(AItem, False);
                    IcsClearMessages(Handle, FMsg_WM_ASYNCSELECT, WPARAM(AItem.HSocket));
                  {$ENDIF}
                    AItem.HSocket := INVALID_SOCKET;
                  {$IFDEF MSWINDOWS}
                    { Ignore the error occuring when winsock DLL not      }
                    { initialized (occurs when using TWSocket from a DLL) }
                    if AItem.LastError = WSANOTINITIALISED then
                        Break;
                  {$ENDIF}
                    MlSocketError(AItem, 'Disconnect (closesocket)');
                    Exit;
                end;
                MessagePump;
            end;
        until iStatus = 0;
      {$IFDEF POSIX}
        WSocketSynchronizedRemoveEvents(AItem, True);
        IcsClearMessages(Handle, FMsg_WM_ASYNCSELECT, WPARAM(AItem.HSocket));
      {$ENDIF}
        AItem.HSocket := INVALID_SOCKET;
    end;
    AItem.State := wsClosed;
    if (not (csDestroying in ComponentState)) and
       (not AItem.CloseInvoked) {and Assigned(FOnSessionClosed)} then begin
        AItem.CloseInvoked := TRUE;
        TriggerSessionClosed(0);
    end;
    { 29/09/98 Protect AssignDefaultValue because SessionClosed event handler }
    { may have destroyed the component.                                       }
    try
        AItem.AssignDefaults;
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MlSocketError(
    AItem: TWSocketMultiListenItem; const ASockFunc: String;
    ALastError: Integer = 0; FriendlyMsg: String = '');   { V8.36 added FriendlyMsg }
var
    ErrCode  : Integer;
    Line : String;
begin
    FMultiListenIndex := AItem.Index;
    try
        if ALastError = 0 then
            ErrCode := WSocket_WSAGetLastError
        else
            ErrCode := ALastError;
        Line  := 'Listening socket index #' + IntToStr(FMultiListenIndex) + ' ' +
                  WSocketErrorDesc(ErrCode) + ' (#' + IntToStr(ErrCode) +
                  ' in ' + ASockFunc + ')' ;
        if (ErrCode = WSAECONNRESET) or
           (ErrCode = WSAENOTCONN) then begin
            WSocket_closesocket(AItem.HSocket);
            AItem.HSocket := INVALID_SOCKET;
            if AItem.State <> wsClosed then
               TriggerSessionClosed(ErrCode);
            AItem.State := wsClosed;
        end;

        AItem.LastError := ErrCode;
        LastError := ErrCode;
        RaiseException(Line, ErrCode, WSocketErrorDesc(ErrCode), FriendlyMsg,  { V8.49 }
                                   ASockfunc, AItem.Addr, AItem.Port, FProtoStr);  { V8.36 }
    finally
        FMultiListenIndex := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.Ml_Do_FD_CLOSE(
    AItem : TWSocketMultiListenItem;
    AMsg  : TMessage);
begin
    if (AItem.HSocket <> INVALID_SOCKET) then begin
        if not AItem.CloseInvoked then
        begin
            AItem.CloseInvoked := TRUE;
            TriggerSessionClosed(IcsHiWord(AMsg.LParam));
        end;
        if AItem.State <> wsClosed then
            MlClose(AItem);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MlListen(
    AItem: TWSocketMultiListenItem);
var
    iStatus : Integer;
    ErrorCode : Integer;
    FriendlyMsg : string;
{$IFDEF MSWINDOWS}
    optval : Integer;
{$ENDIF MSWINDOWS}
    saddr : TSockAddrIn6;
    saddrlen : Integer;
begin
    FMultiListenIndex := AItem.Index;
    FriendlyMsg := '';
    AItem.FBindPort := '';  { V8.64 }
    try
        if (AItem.State <> wsClosed) then begin
            WSocket_WSASetLastError(WSAEINVAL);
            MlSocketError(AItem, 'listen: socket is already listening');
            Exit;
        end;

        if IcsLowerCase(FProtoStr) <> 'tcp' then begin
            WSocket_WSASetLastError(WSAEINVAL);
            MlSocketError(AItem, 'listen: protocol unsupported');
            Exit;
        end;

        if AItem.Port = '' then begin
            WSocket_WSASetLastError(WSAEINVAL);
            MlSocketError(AItem, 'listen: port not assigned');
            Exit;
        end;

        if AItem.Addr = '' then begin
            //WSocket_Synchronized_WSASetLastError(WSAEINVAL);
            WSocket_WSASetLastError(WSAEINVAL);
            MlSocketError(AItem, 'listen: address not assigned');
            Exit;
        end;

        try
            { The next line will trigger an exception in case of failure }
            AItem.PortNum := WSocketResolvePort(AItem.Port, 'tcp');  { V8.70 }
            AItem.Fsin.sin6_port := WSocket_htons(AItem.PortNum);

            { The next line will trigger an exception in case of failure }
            if AItem.SocketFamily = sfIPv4 then
            begin
                AItem.Fsin.sin6_family := AF_INET;
                PSockAddrIn(@AItem.Fsin).sin_addr.s_addr := WSocketResolveHost(AItem.Addr).s_addr;  { V8.70 }
            end
            else
                WSocketResolveHost(AItem.Addr, AItem.Fsin, AItem.SocketFamily, IPPROTO_TCP);
        except
            on E: Exception do begin
                AItem.AssignDefaults;
                raise ESocketException.Create('listen: ' + E.Message);
            end;
        end;

        { Remove any data from the internal output buffer }
        { (should already be empty !)                     }
        DeleteBufferedData;

        AItem.HSocket := WSocket_socket(AItem.Fsin.sin6_family, SOCK_STREAM, IPPROTO_TCP);
        if AItem.HSocket = INVALID_SOCKET then begin
            MlSocketError(AItem, 'listen: socket');
            Exit;
        end;

      {$IFDEF MSWINDOWS}  { V8.49 }
        if FExclusiveAddr then begin
        { V8.36 Prevent other applications accessing this address and port }
            optval  := -1;
            iStatus := WSocket_SetSockOpt(AItem.HSocket, SOL_SOCKET,
                                                       SO_EXCLUSIVEADDRUSE,
                                                       @optval, SizeOf(optval));
            if iStatus <> 0 then begin
                MlSocketError(AItem, 'setsockopt(SO_EXCLUSIVEADDRUSE)');
                MlClose(AItem);
                Exit;
            end;
        end;
      {$ENDIF}

        iStatus := WSocket_bind(AItem.HSocket, PSockAddr(@AItem.Fsin)^, IcsSizeOfAddr(AItem.Fsin));   { V9.5 }
        if iStatus = 0 then
            AItem.State := wsBound
        else begin
            ErrorCode := WSocket_WSAGetLastError;
            if (ErrorCode = WSAEADDRINUSE) or (ErrorCode = WSAEACCES)  then   { V8.36 more friendly message for common error }
                FriendlyMsg := 'Another server is already listening on ' +
                                                 AItem.Addr + ':' + AItem.Port;
            MlSocketError(AItem, 'listen: Bind', ErrorCode, FriendlyMsg);
            MlClose(AItem);
            Exit;
        end;

        iStatus := WSocket_listen(AItem.HSocket, AItem.ListenBacklog);
        if iStatus = 0 then begin
            AItem.State := wsListening;
         { V8.64 keep actual port }
            saddrlen := sizeof(saddr);
            if WSocket_getsockname(AItem.HSocket,
                                      PSockAddr(@saddr)^, saddrlen) = 0 then
                AItem.FBindPort := IntToStr(WSocket_ntohs(saddr.sin6_port));
        end
        else begin
            MlSocketError(AItem, 'listen: Listen');
            Exit;
        end;

        AItem.FSelectEvent := FD_ACCEPT or FD_CLOSE;

      {$IFDEF MSWINDOWS}
        if wsoNotifyAddressListChange in ComponentOptions then
            AItem.FSelectEvent := AItem.FSelectEvent or FD_ADDRESS_LIST_CHANGE;
        if wsoNotifyRoutingInterfaceChange in ComponentOptions then
            AItem.FSelectEvent := AItem.FSelectEvent or FD_ROUTING_INTERFACE_CHANGE;
      {$ENDIF}

        iStatus := WSocket_WSAASyncSelect(
                                        {$IFDEF POSIX}
                                          AItem,
                                        {$ENDIF}
                                          AItem.HSocket,
                                          Handle,
                                          FMsg_WM_ASYNCSELECT,
                                          AItem.FSelectEvent);
        if iStatus <> 0 then begin
            MlSocketError(AItem, 'listen: WSAASyncSelect');
            Exit;
        end;

      {$IFDEF MSWINDOWS}
        if (wsoNotifyAddressListChange in ComponentOptions) and
           (not AItem.SetAddressListChangeNotification) then begin
            MlSocketError(AItem, 'listen: SetAddressListChangeNotification');
            Exit;
        end;

        if (wsoNotifyRoutingInterfaceChange in ComponentOptions) and
           (not AItem.SetRoutingInterfaceChangeNotification)then begin
            MlSocketError(AItem, 'listen: SetRoutingInterfaceChangeNotification');
            Exit;
        end;
      {$ENDIF}
    finally
        FMultiListenIndex := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MlPause(
    AItem: TWSocketMultiListenItem);
begin
    if not AItem.Paused then
        AItem.FPaused := WSocket_WSAASyncSelect(
                                              {$IFDEF POSIX}
                                                AItem,
                                              {$ENDIF}
                                                AItem.HSocket,
                                                Handle, 0, 0) = 0;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MLResume(
    AItem: TWSocketMultiListenItem);
begin
    if AItem.Paused then
        AItem.FPaused := not (WSocket_WSAASyncSelect(
                                                {$IFDEF POSIX}
                                                  AItem,
                                                {$ENDIF}
                                                  AItem.HSocket,
                                                  Handle,
                                                  FMsg_WM_ASYNCSELECT,
                                                  AItem.SelectEvent) = 0);

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MlSetAddr(
  var FldAddr: string; var FldSocketFamily: TSocketfamily;
  const FldOldSocketFamily: TSocketfamily;
  const NewValue: string);
var
    LSocketFamily: TSocketFamily;
begin
    FldAddr := IcsTrim(NewValue);
    if FldAddr = '' then
        Exit;
    { If the address is either a valid IPv4 or IPv6 address }
    { change current SocketFamily.                          }
    if WSocketIsIP(FldAddr, LSocketFamily) then
    begin
        if (LSocketFamily = sfIPv4) or (IsIPv6APIAvailable) then
            FldSocketFamily := LSocketFamily
        else
            FldSocketFamily := FldOldSocketFamily;
    end
    else
        FldSocketFamily := FldOldSocketFamily;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MlSetSocketFamily(
  var FldSocketFamily: TSocketfamily;
  var FldOldSocketFamily: TSocketfamily;
  const NewValue: TSocketFamily);
begin
    if NewValue <> FldSocketFamily then begin
        if NewValue <> sfIPv4 then begin
            try
                if not IsIPv6APIAvailable then
                    raise ESocketException.Create(
                     'SetSocketFamily: New API requires winsock 2.2 ' +
                     'and Windows XP, property "SocketFamily" reset to "sfIPv4"');
            except
                FldSocketFamily := sfIPv4;
                FldOldSocketFamily := FldSocketFamily;
                Exit;
            end;
        end;
        FldSocketFamily := NewValue;
        FldOldSocketFamily :=FldSocketFamily;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MultiClose;
var
    I: Integer;
begin
    if State <> wsClosed then
        Close;
    if Assigned(FMultiListenSockets) then begin
        for I := 0 to FMultiListenSockets.Count - 1 do
            if FMultiListenSockets[I].State <> wsClosed then
                MlClose(FMultiListenSockets[I]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ beware this function does not give errors if some listeners fail due to port conflicts }
procedure TCustomMultiListenWSocketServer.MultiListen;
var
    I: Integer;
begin
    if State <> wsListening then
        Listen;
    if Assigned(FMultiListenSockets) then begin
        for I := 0 to FMultiListenSockets.Count - 1 do begin
            if FMultiListenSockets[I].State <> wsListening then
            MlListen(FMultiListenSockets[I]);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ this function ignores errors if some listeners fail due to port conflicts }
function TCustomMultiListenWSocketServer.MultiListenEx: string;    { V8.49 }
var
    I: Integer;
begin
    Result := '';
    if State <> wsListening then
    try
        Listen;
    except
        on E:Exception do
            Result := 'Socket 1 Listen Failed: ' + Self.Addr +
                          ' port ' + Self.Port + ' - ' + E.Message;
    end;
    if Assigned(FMultiListenSockets) then begin
        for I := 0 to FMultiListenSockets.Count - 1 do begin
            if FMultiListenSockets[I].State <> wsListening then
            try
                MlListen(FMultiListenSockets[I]);
            except
                on E:Exception do begin
                    if Result <> '' then Result := Result + #13#10;
                    Result := Result + 'Socket ' + IntToStr (I + 1) +
                        ' Listen Failed: ' + Self.Addr +
                              ' port ' + Self.Port + ' - ' + E.Message;
                end;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomMultiListenWSocketServer.MultiListenItemClass: TWSocketMultiListenItemClass;
begin
    Result := TWSocketMultiListenItem;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.SetMultiListenIndex(
  const Value: Integer);
begin
    FMultiListenIndex := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.ThreadAttach;
var
    I : Integer;
    LItem : TWSocketMultiListenItem;
begin
    FMultiListenIndex := -1;
    inherited ThreadAttach;
    for I := 0 to FMultiListenSockets.Count -1 do begin
        LItem := FMultiListenSockets[I];
        if (LItem.HSocket <> INVALID_SOCKET) then
            WSocket_WSAASyncSelect(
                                  {$IFDEF POSIX}
                                    LItem,
                                  {$ENDIF}
                                    LItem.HSocket,
                                    Handle, FMsg_WM_ASYNCSELECT,
                                    LItem.SelectEvent);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.ThreadDetach;
var
    I : Integer;
    LItem : TWSocketMultiListenItem;
begin
    FMultiListenIndex := -1;
    if ThreadID = IcsGetCurrentThreadID then begin // not thread-safe
        for I := 0 to FMultiListenSockets.Count -1 do begin
            LItem := FMultiListenSockets[I];
            if (LItem.HSocket <> INVALID_SOCKET) then
                WSocket_WSAASyncSelect(
                                      {$IFDEF POSIX}
                                        LItem,
                                      {$ENDIF}
                                        LItem.HSocket,
                                        Handle, 0, 0);
        end;
    end;
    inherited ThreadDetach;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.TriggerClientConnect(
    Client : TWSocketClient; Error : Word);
begin
    inherited TriggerClientConnect(Client, Error);
    { Finally reset the MultiListenIndex just to avoid bad component use }
    //FMultiListenIndex := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.WMASyncSelect(var msg: TMessage);
var
    Check   : Word;
    ParamLo : Word;
    AItem   : TWSocketMultiListenItem;
begin
    if msg.wParam = WPARAM(FHSocket) then begin
        FMultiListenIndex := -1;

        if FPaused then
          Exit;

        ParamLo := LoWord(msg.lParam);
        Check := ParamLo and FD_ACCEPT;
        if Check <> 0 then begin
            FSelectMessage := FD_ACCEPT;
            Do_FD_ACCEPT(msg);
        end;

        Check := ParamLo and FD_CLOSE;
        if Check <> 0 then begin
            FSelectMessage := FD_CLOSE;
            Do_FD_CLOSE(msg);
        end;

      {$IFDEF MSWINDOWS}
        if ParamLo and FD_ROUTING_INTERFACE_CHANGE <> 0 then begin
            FSelectMessage := FD_ROUTING_INTERFACE_CHANGE;
            Do_FD_ROUTING_INTERFACE_CHANGE(msg);
        end;

        if ParamLo and FD_ADDRESS_LIST_CHANGE <> 0 then begin
            FSelectMessage := FD_ADDRESS_LIST_CHANGE;
            Do_FD_ADDRESS_LIST_CHANGE(msg);
        end;
      {$ENDIF}

        FSelectMessage := 0;

    end
    else begin
        FMultiListenIndex := FMultiListenSockets.FindItemIndex(msg.wParam);
        if FMultiListenIndex = -1 then
            Exit;
        AItem := FMultiListenSockets[FMultiListenIndex];

        if AItem.Paused then
          Exit;

        ParamLo := LoWord(msg.lParam);

        Check := ParamLo and FD_ACCEPT;
        if Check <> 0 then
            Do_FD_ACCEPT(msg);

        Check := ParamLo and FD_CLOSE;
        if Check <> 0 then
            Ml_Do_FD_CLOSE(AItem, msg);

      {$IFDEF MSWINDOWS}
        if ParamLo and FD_ROUTING_INTERFACE_CHANGE <> 0 then begin
            FSelectMessage := FD_ROUTING_INTERFACE_CHANGE;
            Do_FD_ROUTING_INTERFACE_CHANGE(msg);
        end;

        if ParamLo and FD_ADDRESS_LIST_CHANGE <> 0 then begin
            FSelectMessage := FD_ADDRESS_LIST_CHANGE;
            Do_FD_ADDRESS_LIST_CHANGE(msg);
        end;
      {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ returns state, ip address and port for all sockets }
function TCustomMultiListenWSocketServer.ListenStates: String;
var
    K: integer ;
    RealPort: String;   { V8.64 }
{$IFDEF USE_SSL}
    ListenItem: TSslWSocketMultiListenItem;    { V8.59 }
{$ELSE}
    ListenItem: TWSocketMultiListenItem;
{$ENDIF}
begin
    RealPort := Self.Port;
    if Self.FState = wsListening then
        RealPort := Self.BindPort;  { V8.64 for dynamic ports }
    Result := 'Socket 1 State: ' + SocketStateNames[Self.FState] + ' ' +
                  SocketFamilyNames [Self.SocketFamily] + ' on ' +
                                        Self.Addr + ' port ' + RealPort;
{$IFDEF USE_SSL}
    if SslEnable then
        Result := Result + ' SSL' + #13#10
    else
{$ENDIF}
        Result := Result + #13#10;
    if MultiListenSockets.Count > 0 then begin
        for K := 0 to MultiListenSockets.Count - 1 do begin
            ListenItem := MultiListenSockets [K] as
{$IFDEF USE_SSL}
                TSslWSocketMultiListenItem;
{$ELSE}
                TWSocketMultiListenItem;
{$ENDIF}
            RealPort := ListenItem.FPort;
            if ListenItem.FState = wsListening then RealPort := ListenItem.BindPort;  { V8.64 for dynamic ports }
            Result := Result + 'Socket ' + IntToStr (K + 2) +
                 ' State: ' + SocketStateNames[ListenItem.FState] + ' ' +
                     SocketFamilyNames [ListenItem.SocketFamily] +
                       ' on ' + ListenItem.FAddr + ' port ' + RealPort;
{$IFDEF USE_SSL}
            if ListenItem.SslEnable then
               Result := Result + ' SSL' + #13#10
            else
{$ENDIF}
                Result := Result + #13#10;
        end;
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ return true is all sockets are listening OK }
function TCustomMultiListenWSocketServer.ListenAllOK: Boolean ;
var
    K: integer ;
begin
    Result := False;
    if FState <> wsListening then Exit;
    if MultiListenSockets.Count > 0 then begin
        for K := 0 to MultiListenSockets.Count - 1 do begin
            if MultiListenSockets [K].FState <> wsListening then Exit;
        end;
    end;
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ return true if any sockets are listening, ie server is running }
function TCustomMultiListenWSocketServer.ListenAny: Boolean ;        { V9.1 }
var
    K: integer ;
begin
    Result := True;
    if FState = wsListening then
        Exit;
    if MultiListenSockets.Count > 0 then begin
        for K := 0 to MultiListenSockets.Count - 1 do begin
            if MultiListenSockets [K].FState = wsListening then Exit;
        end;
    end;
    Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TWSocketMultiListenItem }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$IFDEF POSIX}
{ Impl. IIcsEventSource }
function TWSocketMultiListenItem.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem._AddRef: Integer;
begin
  Result := -1;  // no ref count
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem._Release: Integer;
begin
  Result := -1; // no ref count
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetEventMask: Cardinal;
begin
    Result := FPxEventMask;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetEventMask(const AValue: Cardinal);
begin
    FPxEventMask := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetNotifyMessageID: UINT;
begin
    Result := FPxEventMessageID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetNotifyMessageID(const AValue: UINT);
begin
    FPxEventMessageID := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetNotifyWindow: HWND;
begin
    Result := FPxEventWindow;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetNotifyWindow(const AValue: HWND);
begin
    FPxEventWindow := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetEventState: TIcsAsyncEventState;
begin
    Result := FPxEventState;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetEventState(const AValue: TIcsAsyncEventState);
begin
    FPxEventState := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetFileDescriptor: Integer;
begin
    Result := FPxFileDescriptor;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetFileDescriptor(const AValue: Integer);
begin
    FPxFileDescriptor := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetObject: TObject;
begin
    Result := Self;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetObjectID: NativeInt;
begin
    Result := FPxObjectID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF POSIX IIcsEventSource}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure TWSocketMultiListenItem.AssignDefaults;
begin
    FHSocket            := INVALID_SOCKET;
    FPortNum            := 0;
    FState              := wsClosed;
    FPaused             := FALSE;
    FCloseInvoked       := FALSE;
    FillChar(Fsin, SizeOf(Fsin), 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.Close;
begin
    OwnerServer.MlClose(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TWSocketMultiListenItem.Create(Collection: TCollection);
begin
    inherited Create(Collection);
    FListenBackLog := 15; { V8.57 was 5 }
    FSocketFamily := DefaultSocketFamily;
    FOldSocketFamily := FSocketFamily;
    AssignDefaults;
{$IFDEF POSIX}
    FPxObjectID := WSocketGenerateObjectID;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TWSocketMultiListenItem.Destroy;
begin
    try
        if (FState <> wsInvalidState) and (FState <> wsClosed) then
            OwnerServer.MlClose(Self);
    finally
        inherited;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetAddrResolved: string;
begin
    if Fsin.sin6_family = AF_INET6 then
        Result := WSocketIPv6ToStr(@Fsin)
    else
        Result := WSocketIPv4ToStr(PInteger(@PSockAddr(@Fsin)^.sin_addr)^);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetCloseInvoked: Boolean;
begin
    Result := FCloseInvoked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.Listen;
begin
    OwnerServer.MlListen(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.OwnerServer: TCustomMultiListenWSocketServer;
begin
    Result := TWSocketMultiListenCollection(Collection).Owner;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.Pause: Boolean;
begin
    OwnerServer.MlPause(Self);
    Result := FPaused;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.Resume: Boolean;
begin
    OwnerServer.MlResume(Self);
    Result := not FPaused;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetAddr(const Value: string);
begin
    OwnerServer.MlSetAddr(FAddr, FSocketFamily, FOldSocketFamily, Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetCloseInvoked(const AValue: Boolean);
begin
    FCloseInvoked := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetSocketFamily(const Value: TSocketFamily);
begin
    OwnerServer.MlSetSocketFamily(FSocketFamily, FOldSocketFamily, Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.SetAddressListChangeNotification: Boolean;
{$IFDEF MSWINDOWS}
var
    LBytesRcvd : Cardinal;
begin
    if FHSocket <> INVALID_SOCKET then begin
        if FSelectEvent and FD_ADDRESS_LIST_CHANGE = 0 then begin
            FSelectEvent := FSelectEvent or FD_ADDRESS_LIST_CHANGE;
            Result := WSocket_WSAASyncSelect(FHSocket,
                                             OwnerServer.Handle,
                                             OwnerServer.FMsg_WM_ASYNCSELECT,
                                             FSelectEvent) <> SOCKET_ERROR;
        end
        else
            Result := True;
        if Result then
            Result := (WSocket_WSAIoctl(FHSocket, SIO_ADDRESS_LIST_CHANGE, nil, 0,
                                        nil, 0, LBytesRcvd, nil, nil) <> SOCKET_ERROR) or
                      (WSocket_WSAGetLastError = WSAEWOULDBLOCK);
    end
    else
        Result := False;
{$ENDIF}
{$IFDEF POSIX}
begin
    Result := False;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.SetRoutingInterfaceChangeNotification: Boolean;
{$IFDEF MSWINDOWS}
var
    LBytesRcvd : Cardinal;
begin
    if FHSocket <> INVALID_SOCKET then begin
        if FSelectEvent and FD_ROUTING_INTERFACE_CHANGE = 0 then begin
            FSelectEvent := FSelectEvent or FD_ROUTING_INTERFACE_CHANGE;
            Result := WSocket_WSAASyncSelect(FHSocket,
                                             OwnerServer.Handle,
                                             OwnerServer.FMsg_WM_ASYNCSELECT,
                                             FSelectEvent) <> SOCKET_ERROR;
        end
        else
            Result := True;
        if Result then
            Result := (WSocket_WSAIoctl(FHSocket, SIO_ROUTING_INTERFACE_CHANGE,
                                        @Fsin, IcsSizeOfAddr(Fsin), nil, 0, LBytesRcvd,         { V9.5 }
                                        nil, nil) <> SOCKET_ERROR) or
                      (WSocket_WSAGetLastError = WSAEWOULDBLOCK);
    end
    else
        Result := False;
{$ENDIF}
{$IFDEF POSIX}
begin
    Result := False;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TWSocketMultiListenCollection }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function TWSocketMultiListenCollection.Add: TWSocketMultiListenItem;
begin
    Result := TWSocketMultiListenItem(inherited Add);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TWSocketMultiListenCollection.Create(AOwner: TPersistent;
    AItemClass: TWSocketMultiListenItemClass);
begin
    inherited Create(AOwner, AItemClass);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenCollection.FindItemHandle(
    const AHSocket: TSocket): TWSocketMultiListenItem;
var
    I: Integer;
begin
    for I := 0 to Count -1 do
    begin
      Result := Items[I];
      if Result.FHSocket = AHSocket then
          Exit;
    end;
    Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenCollection.FindItemIndex(
    const AHSocket: TSocket): Integer;
begin
    for Result := 0 to Count -1 do
    begin
      if Items[Result].FHSocket = AHSocket then
          Exit;
    end;
    Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenCollection.FindItemID(ID: Integer): TWSocketMultiListenItem;
begin
    Result := TWSocketMultiListenItem(inherited FindItemID(ID));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenCollection.GetItem(Index: Integer): TWSocketMultiListenItem;
begin
    Result := TWSocketMultiListenItem(inherited GetItem(Index));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenCollection.Insert(
  Index: Integer): TWSocketMultiListenItem;
begin
    Result := TWSocketMultiListenItem(inherited Insert(Index));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenCollection.Owner: TCustomMultiListenWSocketServer;
begin
    Result := TCustomMultiListenWSocketServer(GetOwner);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenCollection.SetItem(Index: Integer;
  Value: TWSocketMultiListenItem);
begin
    inherited SetItem(Index, Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{*                                                                           *}
{*                            TWSocketClient                                 *}
{*                                                                           *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketClient.StartConnection;
begin
    if Length(FBanner) > 0 then
        SendStr(FBanner + FLineEnd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Triggered when socket is closed. Need to inform server socket to update   }
{ client list and trigger client disconnect event.                          }
procedure TWSocketClient.TriggerSessionClosed(ErrCode : Word);
var
    PIdRec : PClientIdRec;
begin
    if not FSessionClosedFlag then begin
        FSessionClosedFlag := TRUE;
        if Assigned(FServer) then begin
            New(PIdRec);
            PIdRec^.PClient := Self;
            PIdRec^.CliId   := FCliId;
            if NOT PostMessage(Server.Handle, Server.FMsg_WM_CLIENT_CLOSED,
                               WPARAM(ErrCode), LPARAM(PIdRec))
            then
                System.Dispose(PIdRec);
        end;
        inherited TriggerSessionClosed(ErrCode);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This override base class GetPeerAddr. It return cached value.             }
function TWSocketClient.GetPeerAddr: String;
begin
    Result := FPeerAddr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This override base class GetPeerPort. It return cached value.             }
function TWSocketClient.GetPeerPort: String;
begin
    Result := FPeerPort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Override base class. Dup is called when a client is assigned to a         }
{ TWSocket. Assigning HSocket property will call Dup.                       }
procedure TWSocketClient.Dup(newHSocket : TSocket);
begin
    if newHSocket = INVALID_SOCKET then    { V9.5 connection refused, should not come here }
        Exit;
    inherited Dup(newHSocket);
    { Cache PeerAddr value }
//    FPeerAddr := inherited GetPeerAddr;   { V9.5 done in accept callback }
//    FPeerPort := inherited GetPeerPort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF USE_SSL}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslWSocketServer.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    // Server socket doesn't use SSL to listen for clients
    FSslEnable       := TRUE;
    Port             := '443';
    Proto            := 'tcp';
    Addr             := '0.0.0.0';
    SslMode          := sslModeServer;
    FIcsHosts        := TIcsHostCollection.Create(self);            { V8.45 }
    FValidated       := False;                                      { V8.48 }
    FCertExpireDays  := 30;                                         { V8.57 }
{$IFDEF OpenSSL_OcspStaple}  { V9.5 }
    FOcspSrvHttp := TOcspHttp.Create(Self);                            { V8.69 }
    FOcspSrvHttp.CacheFName := 'ocspservercache.recs';
    FOcspSrvHttp.CacheRefrDays := 1;
    FOcspSrvHttp.CacheFlushMins := 1;
{$ENDIF} // OpenSSL_OcspStaple
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslWSocketServer.Destroy;                                 { V8.45 }
begin
    try  { V8.71 }
{$IFDEF OpenSSL_OcspStaple}  { V9.5 }
        FreeAndNil(FOcspSrvHttp);    { V8.69 }
{$ENDIF} // OpenSSL_OcspStaple
        if Assigned(FIcsHosts) then
            FreeAndNil(FIcsHosts);
    finally
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWSocketServer.Close;                        { V8.63 }
begin
    try  { V8.71 }
        OrderClose;   { stop any pending X509 certificate orders }
    finally
        inherited Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslWSocketServer.MultiListenItemClass: TWSocketMultiListenItemClass;
begin
    Result := TSslWSocketMultiListenItem;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWSocketServer.TriggerClientConnect(Client : TWSocketClient; Error : Word);
var
    I: Integer;
begin
   { V8.45 set SslEnable before event handler, so it can be used there }
    if FMultiListenIndex = -1 then
        Client.SslEnable := Self.SslEnable      { V8.50 }
    else begin
        Assert(MultiListenIndex < MultiListenSockets.Count);
        Client.SslEnable := TSslWSocketMultiListenItem(MultiListenSockets[FMultiListenIndex]).SslEnable;
    end;

 { V8.45 keep server binding information for client }
    Client.FMultiListenIdx := FMultiListenIndex;
 (* V9.5 done already in callback
  if FMultiListenIndex = -1 then begin
        Client.FServerAddr := GetXAddr;
        Client.FServerPort := FBindPort;  { V8.64 }
    end
    else begin
        Client.FServerAddr := MultiListenSockets[FMultiListenIndex].Addr;  { V8.64 }
        Client.FServerPort := MultiListenSockets[FMultiListenIndex].FBindPort; { V8.64 }
    end ;   *)

  { V8.45 check binding for IcsHost - may be changed later if SNI or Host: header checked }
  { beware multiple hosts may have the same binding }
    if FIcsHosts.Count > 0 then begin
        for I := 0 to FIcsHosts.Count - 1 do begin
            if NOT (FIcsHosts [I].HostEnabled) then
                continue;
            if (FIcsHosts [I].FBindIdxNone = FMultiListenIndex) or (FIcsHosts [I].FBindIdxSsl = FMultiListenIndex) or
                   (FIcsHosts [I].FBindIdx2None = FMultiListenIndex) or (FIcsHosts [I].FBindIdx2Ssl = FMultiListenIndex) then begin
                Client.FIcsHostIdx := I;
                Client.FHostTag := FIcsHosts [I].HostTag;
                Break;
            end;
        end;
    end;

    inherited TriggerClientConnect(Client, Error);
    { The event handler may have closed the connection }
    { The event handler may also have started the SSL }
    if (Error <> 0) or (Client.State <> wsConnected) or (Client.SslState > sslNone) then
        Exit;

    if Client.SslEnable then begin
        Client.SslMode                  := FSslMode;
        Client.SslAcceptableHosts       := FSslAcceptableHosts;
       { correct IcsHosts context selected in TriggerSslServerName, but need a default }
        Client.SslContext               := FSslContext;
        Client.OnSslVerifyPeer          := OnSslVerifyPeer;
        Client.OnSslSetSessionIDContext := OnSslSetSessionIDContext;
        Client.OnSslSvrNewSession       := OnSslSvrNewSession;
        Client.OnSslSvrGetSession       := OnSslSvrGetSession;
        Client.OnSslHandshakeDone       := OnSslHandshakeDone;
        Client.OnSslServerName          := OnSslServerName;   { V8.07 }
        Client.OnSslAlpnSelect          := OnSslAlpnSelect;   { V8.56 }
        try
            if Client.SslMode = sslModeClient then
                Client.StartSslHandshake
            else
                Client.AcceptSslHandshake;
        except
            on E: Exception do begin                            // AG 12/18/05
                Client.SslEnable := False;
                Client.Abort;
                { Don't abort silently }
                Client.HandleBackGroundException(E, 'TriggerClientConnectd Accept SSL Handshake');    { V8.62 }
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslWSocketServer.GetIcsHosts: TIcsHostCollection;                   { V8.45 }
begin
    Result := FIcsHosts;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWSocketServer.SetIcsHosts(const Value: TIcsHostCollection);   { V8.45 }
begin
    if FIcsHosts <> Value then begin
        if Assigned(FIcsHosts) then
            FIcsHosts.Free;
        FIcsHosts := Value;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF AUTO_X509_CERTS}  { V8.59 }
function  TSslWSocketServer.GetSslX509Certs: TComponent;                       { V8.57 }
begin
    Result := FSslX509Certs;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWSocketServer.SetSslX509Certs(const Value : TComponent);   { V8.57 }
begin
    if Value <> FSslX509Certs then begin
        FSslX509Certs := Value;
    end;
end;


{$ENDIF} // AUTO_X509_CERTS
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ are we listening on this binding already }
function TSslWSocketServer.FindBinding(const MAddr: String; MPort: Integer; var MIndex: Integer): boolean;      { V8.45 }
var
    J: integer;
begin
    Result := False;
    if (Addr = MAddr) and (Port = IntToStr(MPort)) then begin
        Result := True;
        MIndex := -1;
        Exit;
    end;
    if MultiListenSockets.Count = 0 then Exit;
    for J := 0 to MultiListenSockets.Count -1 do begin
        if (MultiListenSockets[J].Addr = MAddr) and  (MultiListenSockets[J].Port = IntToStr(MPort)) then begin
            Result := True;
            MIndex := J;
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWSocketServer.IcsLogEvent(Sender: TObject; LogOption: TLogOption; const Msg : String);          { V8.69 }
begin
{$IFDEF AUTO_X509_CERTS}
    if Assigned(FSslX509Certs) then
        (FSslX509Certs as TSslX509Certs).LogEvent(Msg);
{$ENDIF}
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 check if we can order a new certificates from supplier, get renewal dates }
{ called regularly to get renewal information, disables ordering for any problems  }
{ returns true if dates found, new cert should be orderd }
function TSslWSocketServer.DoCheckOrderCert(HostNr: Integer): Boolean;    { V9.5 }
{$IFDEF AUTO_X509_CERTS}
var
    Idx: Integer;
    MyCommonName: String;

 { log what is happening via SslX509Certs component }
    procedure X509Log(const Msg: String);
    begin
        FIcsHosts[HostNr].FCertErrs := Msg;
        (FSslX509Certs as TSslX509Certs).LogEvent(Msg);
    end;

{$ENDIF}
begin
    Result := False;
    if (HostNr < 0) or (HostNr >= FIcsHosts.Count) then
        Exit;
    with FIcsHosts[HostNr] do begin
        if (CertSupplierProto <> SuppProtoAcmeV2)  then
            Exit;

{$IFDEF AUTO_X509_CERTS}
        if ((FCertDirWork = '') and (FSupplierTitle = '')) or (NOT FSslCertAutoOrder) or (NOT Assigned(FSslX509Certs)) then begin
            FCertSupplierProto := SuppProtoNone;
            X509Log('Certificate Ordering Disabled, Bad Configuration, No Supplier or Directory');
            Exit;
        end;

    // no certificate, no renewal information, nothing more to do unless opening account for order
        if (NOT FOrderPending) and (FSslCert = '') or (NOT FileExists(FSSlCert)) then begin  // IcsHosts
            X509Log('Certificate Not Found: ' + FSSlCert);
            Exit;
        end;

        MyCommonName := HostNames[0];
        X509Log('Checking SSL/TLS Certificate Ordering for Domain: ' + MyCommonName);
        FCertErrs := '';  // last one was not an error

     // need to cast type here to avoid circular references with TSslX509Certs unit which uses this server
        with (FSslX509Certs as TSslX509Certs) do begin
            try
                FSupplierTitle := Trim(FSupplierTitle);
            // once only load supplier accounts database
            // if no database, create it with existing Lets Encrypt account if found or new account if have a title
            // C:\ProgramData\ICS-Acme-Accounts\ics-acme-accounts.db
                if AcmeSuppTot = 0 then begin
                    DBSuppliersRead ;
                    if AcmeSuppTot = 0 then begin
                        if FAcmeSupplier <> AcmeLetsEncrypt then begin
                            X509Log('Supplier Must Be Created Externally: ' + FSupplierTitle);
                            FCertSupplierProto := SuppProtoNone;
                            FCertDirWork := '';
                            CloseAccount;
                            Exit;
                        end;
                        if (FSupplierTitle = '') and (FCertDirWork <> '') and (IcsDirExists(FCertDirWork)) then
                            FSupplierTitle := 'AcmeLetsEncrypt-Old';
                        if DBAddAccSupp(FSupplierTitle, FCertDirWork, FAcmeSupplier, True) then
                            X509Log('Creating New Supplier Account: ' + FSupplierTitle)
                        else begin
                            X509Log('Failed to Create Supplier Account: ' + FSupplierTitle);
                        end;
                    end;
                    if AcmeSuppTot = 0 then begin
                        X509Log('Certificate Ordering Disabled, No Suppliers Configured');
                        FCertSupplierProto := SuppProtoNone;
                        CloseAccount;
                        Exit;
                    end;
                end;

            // see if looking up account directory from supplier name in ics-acme-accounts.db
                if FCertDirWork = '' then begin
                    if AcmeSuppTot = 0 then begin
                        FCertSupplierProto := SuppProtoNone;
                        X509Log('Certificate Ordering Disabled, No Suppliers Configured');
                        CloseAccount;
                        Exit;
                    end;
                    Idx := DBFindAccSupp(FSupplierTitle);
                    if Idx < 0 then begin
                        FCertSupplierProto := SuppProtoNone;
                        X509Log('Certificate Ordering Disabled, Supplier Not Found: ' + FSupplierTitle);
                        CloseAccount;
                        Exit;
                    end;
                    FCertDirWork := AcmeSuppRecs[Idx].ASuppDir;
                    FAcmeSupplier := AcmeSuppRecs[Idx].ASupplier;
                end;

            // get Acme supplier account, based on file directory, may fail if servers offline, try again later
               if (CompareText(DirCertWork, FCertDirWork) <> 0) then begin
                    X509Log('Certificate Supplier Database Directory: ' + FCertDirWork);
                    DirCertWork := FCertDirWork;
                    AcmeSupplier := FAcmeSupplier;
                    if NOT SetAcmeAccount(False) then begin  // don't create new account
                        X509Log('Failed to Open Acme Account - ' + LastError + ': ' + FCertDirWork);
                        Exit;
                    end;
                    X509Log('Opened Acme Account OK: ' + FSupplierTitle);
               end;
               FAcmeAccountOpen := True;

            // get old renewal dates, if any from account database, may not have any
                if (FRenewalId = '') or (FRenewCheckDT < 10) then begin
                    if DBReadCNDomDates(MyCommonName) then begin
                        FRenewRetryDT := CertRenewRetryDT;     { V9.5 IcsHosts }
                        FRenewalDays := CertRenewDays;
                        FRenewCheckDT := CertRenewCheckDT;
                        FRenewalId := CertRenewalId;
                        X509Log('Read Old Renewal Information from Acme Account for Domain: ' + MyCommonName);
                    end
                    else begin
                        X509Log('Failed to Read Renewal Information from Account for Domain - ' + MyCommonName);
                        Exit;
                    end;
                end;
                FCertErrs := '';  // no errors if got here

            // still have valid information
                if (FRenewRetryDT >= 10) and (FRenewRetryDT > Now) then begin
                    LogEvent('Next Renewal Retry Check ' + IcsDateTimeToAStr(FRenewRetryDT) + ': ' + MyCommonName);
                    FCertErrs := '';  // no errors
                    Result := True;  // got dates
                    Exit;
                end;

            // refresh information from Acme server
                Result := CertRenewalDomain(MyCommonName);   // logs dates, true if got new dates
                if Result or (FRenewCheckDT < 10) then begin   // dates were updated
                    FRenewRetryDT := CertRenewRetryDT;
                    FRenewalDays := CertRenewDays;
                    FRenewCheckDT := CertRenewCheckDT;
                    FRenewalId := CertRenewalId;
                end;
                if CertRenewNow then begin   // order immediately
                    FOrderPending := True;   { V9.5 IcsHosts }
                    LogEvent('Renewal Check Immediate Order for: ' + MyCommonName);
                end;
            except
                on E:Exception do begin
                    LogEvent('Renewal Check Exception: ' + E.Message);
                    FCertErrs := E.Message;
                    Result := False;
                    CloseAccount;
                    FAcmeAccountOpen := False;
                end;
            end;
        end;

{$ELSE}
       FCertErrs := 'Server Does Not Support Certificate Ordering';
       FCertSupplierProto := SuppProtoNone;
       FCertDirWork := '';
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.57 order a new certificates from supplier }
{ V9.5 account must be opened first }
function TSslWSocketServer.DoProcOrderCert(HostNr: Integer): Boolean;
{$IFDEF AUTO_X509_CERTS}  { V8.59 }
var
    I: Integer;
    FileName, CertName, MyCommonName: String;   { V8.71 use MyCommonName instead of published }

 { V8.63 log what is happening via SslX509Certs component }
    procedure X509Log(const Msg: String);
    begin
        FIcsHosts[HostNr].FCertErrs := Msg;
        (FSslX509Certs as TSslX509Certs).LogEvent(Msg);
    end;

{$ENDIF}
begin
    Result := False;
    if (HostNr < 0) or (HostNr >= FIcsHosts.Count) then
        Exit;
    with FIcsHosts[HostNr] do begin
{$IFDEF AUTO_X509_CERTS}  { V8.59 }
        FRenewalDays := 0;
        if NOT FSslCertAutoOrder then begin
            FCertErrs := 'Server Certificate Ordering Disabled';
            Exit;
        end;
        if NOT Assigned(FSslX509Certs) then begin
            FCertErrs := 'Server Does Not Support Certificate Ordering';
            Exit;
        end;
        if NOT (FSslX509Certs as TSslX509Certs).IsAccountOpen then begin   { V9.5 }
            FCertErrs := 'Must Open Supplier Account Before Certificate Ordering';
            Exit;
        end;

     { V8.63 something sensible in log before all the ordering stuff }
        MyCommonName := HostNames[0];
       X509Log('Starting SSL/TLS Certificate Order for Domain: ' + MyCommonName + ', Using Challenge: ' +
                            ChallengeTypeLits[CertChallenge] + ', With: ' + SupplierProtoLits[CertSupplierProto]);
        FCertErrs := '';  // last one was not an error

        if CertChallenge in [ChallFileFtp, ChallDnsMan, ChallEmail] then begin    { V8.62 }
            X509Log('Unsupported Challenge for Socket Server');
            Exit;
        end;

     { V8.62 HTTP challenges assumes this is a web server that supports the .well-known path or SSI ALPN and only works after server starts }
        if CertChallenge in [ChallFileUNC, ChallAlpnUNC, ChallAlpnApp, ChallFileApp] then begin    { V8.62 }
            if WellKnownPath = '' then begin
                X509Log('No .Well-Known Directory Specified for HTTP Challenge');
                Exit;
            end;
            if (CertChallenge in [ChallFileUNC, ChallFileApp] ) and (FBindNonPort <> 80) then begin
                X509Log('Port 80 Not Listening');
                Exit;
            end;
            if (CertChallenge in [ChallAlpnUNC, ChallAlpnApp]) and (FBindSslPort <> 443) then begin
                X509Log('Port 443 Not Listening');
                Exit;
            end;
            if (FState <> wsListening) then begin
                X509Log('Server not Listening');
                Exit;
            end;
        end;

     { V8.62 Local Server uses a separate local web server for servers not using
        ports 80 or 443 such as FTP, SMTP, etc, configure it, will be started automatrically }
        if (CertChallenge = ChallFileSrv) or (CertChallenge = ChallAlpnSrv) then begin
            (FSslX509Certs as TSslX509Certs).DomWebSrvIP := FBindIpAddr;
        end;

      // some sanity checks to make sure sensible settings made
        if CertSupplierProto = SuppProtoNone then begin
            X509Log('No Supplier Protocol');
            Exit;
        end;
        if CertDirWork = '' then begin
            X509Log('No Work Directory Specified');
            Exit;
        end;

     // need to cast type here to avoid circular references with TSslX509Certs unit which uses this server
        with (FSslX509Certs as TSslX509Certs) do begin
            try

             // open supplier account, based on file directory  V9.5 account should be open already

             // must have correct expected SSL certificate file name
                FileName := IcsExtractNameOnly(FSslCert);
             // V8.63 allow for file name having -bundle or -cert appended to end
                CertName := IcsBuildCertFName(MyCommonName);
                if Pos (CertName, FileName) <> 1 then begin
                    X509Log('Certificate File Name Mismatch, File: ' + FileName + ',  Common Name: ' + CertName);
                    Result := False;  // V8.63
                    Exit;
                end;

            // load all domain order properties, if common name not in database, set minimal stuff
                ClearCertOrder;      { V9.5 }
                if NOT DBReadCNDomain(MyCommonName) then begin   // sets CertCommonName
                    X509Log('Creating New Certificate Order, Common Name: ' + MyCommonName);
                    CertCommonName := MyCommonName;
                    CertCsrOrigin := CsrOriginProps;
                    CertApprovEmail := '';
                    CertSerNumType := SerNumRandom;
                    CertOutFmts := [OutFmtBudl, OutFmtP12, OutFmtPwP12];  { V8.67 password PFX }
                end
                else begin
                    X509Log('Found Old Certificate Order, Issue State: ' + IssueStateLits[IssueState] + ', Common Name: ' + CertCommonName);
                end;

           // V9.5 don't start a new order if partially through one today
                if (IssueState < IssStateChallgOK) or (Trunc(ChallgDoneDT) <> Date) then begin

                // update domains in case server IcsHosts INI file changed since last time
                    DirWellKnown := WellKnownPath;
                    DirPubWebCert.Text := ExtractFilePath(SslCert);
                    PrivKeyPassword := SslPassword;
                    if PrivKeyPassword <> '' then begin
                        CertOutFmts := CertOutFmts + [OutFmtPwP12];  { V8.67 password PFX }
                     { V8.67 if 3DES available, prefer that so older versions of Windows will load our PFX file }
                     { V9.3 no longer prefer TripleDES, too old }
                        PrivKeyCipher := PrivKeyEncAES256;
                      //  if (ICS_OPENSSL_VERSION_MAJOR < 3) or ICS_OSSL3_LOADED_LEGACY then
                      //      P12KeyCipher := PrivKeyEncTripleDES
                    end
                    else
                        PrivKeyCipher := PrivKeyEncNone;
                    PrivKeyType := CertPKeyType;
    {$IFDEF MSCRYPT_Servers}
                    if SslLoadSource >= CertWinStoreMachine then begin { V8.71 install in Window Store }
                        CertOutFmts := CertOutFmts + [OutFmtWinStore];
                    end;
    {$ENDIF}
                    CertSignDigestType := CertSignDigest;
                    SuppCertChallenge := CertChallenge;
                 //   SuppCertProduct := CertProduct;   { V9.5 set automatically }
                    CertSubAltNames.Clear;
                    for I := 0 to HostNames.Count - 1 do
                        CertSubAltNames.AddItem(HostNames[I], DirWellKnown, DirPubWebCert.Text, CertApprovEmail);
                    AutoOrderComplete := True;   { V9.5 always for servers   }
                    if AcmeCertProfile <> '' then
                        CertAcmeProfile := AcmeCertProfile;  { V9.5 profile from IcsHost, if available }
                    if (AcmeSupplier = AcmeLetsEncrypt) and (CertAcmeProfile = '') then   { V9.5 }
                        CertAcmeProfile := 'classic';
                    FRenewCheckDT := 0;   { V9.5 force refresh of renewal information after certificate is loaeded }
                    CertValidity := AcmeCertValidity;  { V9.5 Google only }

               // resets issue state for old order, checks and starts new order
                    Result := AcmeCheckSaveOrder(True, True);   { V9.5 was CertSaveDomain }
                    if NOT Result then begin
                        X509Log('Failed to Start Certificate Order - ' + LastError);  // V8.63, V9.5 real error message, like too many orders
                        CloseAccount;  // V8.63
                        Exit;
                    end;

                // V8.64 get challenges first
                    Result := AcmeV2GetChallgs;   { V9.5 }
                    if NOT Result then begin
                        X509Log('Failed to Get Certificate Challenges - ' + LastError);  // V8.63
                        CloseAccount;  // V8.63
                        Exit;
                    end;

                 // start challenges for new order, unless done already
                    if IssueState < IssStateChallgOK then begin   { V9.5 check it }
                          Result := AcmeV2TestChallgs;
                        if NOT Result then begin
                            X509Log('Failed to Test Certificate Challenges - ' + LastError);  // V8.63
                            CloseAccount;
                            Exit;
                        end;
                        Result := AcmeV2StartChallgs;    { V9.5 }
                        if NOT Result then begin
                            X509Log('Failed to Order New Certificate - ' + LastError);  // V8.63
                            CloseAccount;  // V8.63
                            Exit;
                        end;
                    end;
                end;

             // V9.5 challenges previously completed, finalise order now
                if IssueState in [IssStateChallgOK, IssStateFinalPend]  then begin
                    X509Log('Challenges Completed, Finalising Certificate Order');
                    AcmeV2OrderFinal(True);
                end;

              // note order may not be completed yet
              // AutoOrderComplete=true means order steps continue in OrderTime, every five seconds
              // NewCert event called in main application, which triggers RecheckSslCerts to find new certificate
                FAcmeAccountOpen := False;   // don't close account until order completes  !!!
            except
                on E:Exception do begin
                    FCertErrs := 'Exception Ordering New Certificate DoOrderCert - ' + E.Message;
                    X509Log(FCertErrs);
                    FCertErrs := E.Message; { V8.52 keep exception }
                    Result := False;  // V8.63
                    CloseAccount; // V8.63
                end;
            end;
        end;
{$ELSE}
       FCertErrs := 'Server Does Not Support Certificate Ordering';
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.63 close certificate ordering account, stop order timer and local challenge web server }
procedure TSslWSocketServer.OrderClose;
begin
    FAcmeAccountOpen := False;
{$IFDEF AUTO_X509_CERTS}
    if NOT Assigned(FIcsHosts) then
        Exit;   { V8.64 }
    if FIcsHosts.Count = 0 then
        Exit;
    if NOT FSslCertAutoOrder then
        Exit;
    if NOT Assigned(FSslX509Certs) then
        Exit;
    try
        with (FSslX509Certs as TSslX509Certs) do begin
            if (SupplierProto > SuppProtoNone) then  begin // if not closed already
                if ActiveOrders.Count = 0 then begin                  { V9.5 }
                    LogEvent('No Active Order, Closing Account');
                    CloseAccount;
                end
                else
                    LogEvent('Active Orders, Skipped Closing Account');
            end;
        end;
    except
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.64 load Root CA to validate SSL certificate chains }
procedure TSslWSocketServer.LoadRootCAList;
begin
    if FNoSSL then             { V9.1 }
        Exit;
    if NOT IcsSslRootCAStore.InitFlag then   { V9.1 if internal not loaded, do it }
        IcsSslRootCAStore.Initialise;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.57 load certificates for one host }
{ ForceLoad=True means ignore timestamps and always load certs but no ordering }
function TSslWSocketServer.LoadOneCert(HostNr: Integer; ForceLoad: Boolean;
                                                       var LoadNew: Boolean; AllowSelfSign: Boolean=False): Boolean;  { V8.63 }
var
    NewFStamp: TDateTime;
    FDir, FullName, Info, MyCommonName, MyErrs: String;
{$IFDEF OpenSSL_OcspStaple}    { V9.5 }
    Inters: TX509List;
{$ENDIF}
{$IFDEF MSCRYPT_Servers}
    MsCertTools: TMsCertTools;
    MsCertLoc: TMsCertLocation;
{$ENDIF}

 { V8.64 SslX509Certs component allows better logging }
    procedure X509Log(const Msg: String);
    begin
{$IFDEF AUTO_X509_CERTS}
        if Assigned(FSslX509Certs) then
            (FSslX509Certs as TSslX509Certs).LogEvent(Msg);
{$ENDIF}
    end;

  { V8.64 create self signed certificate, V9.1 CA signed certificate if intermediate is available }
    function CreateSelfSigned(MyHostNr: Integer; Replace: Boolean): Boolean;  { V9.1 added Replace }
    begin
        Result := False;
        AllowSelfSign := True;
        with FIcsHosts[MyHostNr] do begin
            if FullName = '' then begin
                FDir := ExtractFileDir(FSslCert);
                if (FDir = '') OR (NOT DirectoryExists(FDir)) then
                    FDir := GSSL_CERTS_DIR;  { V9.5 was IcsGetTempPath }
                FullName := IncludeTrailingPathDelimiter(FDir) + IcsBuildCertFName(HostNames[0]) + '.pfx';
            end;
            if Replace or (NOT FileExists(FullName)) then begin
                try
                    IcsDeleteFile(FullName, True);   { V9.5 }
                    CreateSelfSignCertEx(FullName, HostNames[0], HostNames, PrivKeyECsecp256, FSslPassword, '', GSSL_INTER_FILE);  { V9.1 added CA }
                    NewFStamp := IcsGetFileUAge(FullName);
                    FCertErrs := 'Created New Local Certificate: ' + FullName;
                    Result := True;
                except
                   on E:Exception do begin
                        FCertErrs := 'Failed to Create Local Certificate: ' + FullName + ' - ' + E.Message;
                        X509Log(FCertErrs);
                        Exit;
                   end;
                end;
            end
            else begin
                Result := True;
                FCertErrs := 'Skipped New Local Certificate, Using Old File: ' + FullName;
            end;

      { should we try and download a new certificate }
            if FSslCertAutoOrder and (CertSupplierProto > SuppProtoNone) then
                FOrderPending := True;  // V9.5 need new cert
        end;
    end;


begin
    Result := False;
    LoadNew := False;
    if (HostNr < 0) or (HostNr >= FIcsHosts.Count) then
        Exit;
    if FNoSSL then             { V9.1 }
        Exit;
    with FIcsHosts[HostNr] do begin
        try
            FCertErrs := '';
            FCertInfo := '';
            FCertValRes := chainOK;
            FullName := '';
            FOrderPending := False;         { V9.5 }
            if FHostNames.Count = 0 then
                Exit;  // sanity check
            MyCommonName := FHostNames[0];  { V9.5 }

        { load certificate, private key and optional intermediates, that may all be
          in the same PEM or PFX bundle file or seperate files, or may be base64 text,
          or may be in the Windows Store }

            if SslLoadSource > CertLoadFile then begin
{$IFDEF AUTO_X509_CERTS}
{$IFDEF MSCRYPT_Servers}
                MsCertTools := TMsCertTools.Create(Nil);   { V8.71 load certificate from Windows Store }
                try
                    if SslLoadSource = CertWinStoreMachine then
                        MsCertLoc := MsLocMachine    // administrator rights needed to access private keys
                    else
                        MsCertLoc := MsLocCurUser;
                    try
                      // FHostNames[0] should be common name, part friendly name or one host name
                        FCertErrs := MsCertTools.LoadOneFromStore(MsCertLoc, MyCommonName, True);
                        if FCertErrs = '' then begin
                            if ForceLoad or (MsCertTools.Sha1Hex <> SslCtx.SslCertX509.Sha1Hex) then begin
                                LoadNew := True;
                                SslCtx.SslCertX509.ClearAll;
                                SslCtx.SslCertX509.Assign(MsCertTools);
                                X509Log('Loaded certificate from Windows Store: ' + MsCertTools.CertName);
                            end
                            else
                                X509Log('Skipped loading certificate from Windows Store: ' + MsCertTools.CertName);
                        end
                        else
                            X509Log('Error loading certificate from Windows Store: ' + FCertErrs);
                    except
                        on E:Exception do
                            FCertErrs := E.Message;
                    end;
                finally
                    MsCertTools.Free;
                end;
{$ENDIF}
{$ENDIF}
            end
      { V9.5 raw private key file for Raw Public Key (RPK) without certificate }
            else if (FSslRawKeyFile <> '') then begin
                if (Pos(PEM_STRING_HDR_BEGIN, FSslRawKeyFile) > 0) then begin
                   SslCtx.SslCertX509.PrivateKeyLoadFromText(FSslRawKeyFile, FSslPassword)
                end
                else
                   SslCtx.SslCertX509.PrivateKeyLoadFromPemFile(FSslRawKeyFile, FSslPassword);
           // pending tell context we have TLSEXT_cert_type_rpk
           //    SSL_CTX_set1_server_cert_type(SSL_CTX *ctx, const unsigned char *val, size_t len)
            end

      { most common, load certificate and private key from files, or perhaps as text }
            else begin  // SslLoadSource = CertLoadFile

{$IFDEF AUTO_X509_CERTS}

      { V9.5 for automatic ordering, look for renewal information from database, before checking chain for expiry dates }
      { if SupplierTitle is available, will look up CertDirWork if blank, and maybe SslCert }
                if FSslCertAutoOrder and (CertSupplierProto = SuppProtoAcmeV2) and Assigned(FSslX509Certs) then begin
                    if (FRenewRetryDT < Now) or (FRenewCheckDT < 10) or (FCertDirWork = '') then begin
                        if DoCheckOrderCert(HostNr) then begin // true got dates
                            if FOrderPending then   // IcsHosts
                                X509Log('Certificate needs to be renewed immediately - ' + MyCommonName)
                            else
                                X509Log('Loaded New Renewal Information for: ' + MyCommonName);
                        end
                        else begin
                            if FOrderPending then
                                X509Log('Certificate needs to be renewed immediately - ' + MyCommonName)
                            else begin
                                X509Log('No Renewal Information for: ' + MyCommonName);
                                FRenewalDays := 3;   // !!!! TEMP
                            end;
                        end;
                    end
                    else
                       // no check need yet, usually only every six hours
                        X509Log('Certificate Renewal Check Skipped for ' + MyCommonName);
                end;
{$ENDIF}
         { finally load certificates from text lines or file }
                if (Pos(PEM_STRING_HDR_BEGIN, FSslCert) > 0) then begin
                  { can we check this ?? }
                    SslCtx.SslCertX509.LoadFromText(FSslCert, croTry, croTry, FSslPassword)
                end
                else begin
                    NewFStamp := -1;
                    if FSslCert <> '' then begin   { V8.66 no file directory, can not process name further }
                        NewFStamp := IcsGetFileUAge(FSslCert);  { keep file time stamp to check nightly, V8.51 UTC time }
                        if NewFStamp <= 0 then begin
                            FCertErrs := 'SSL Certificate Not Found: ' + FSslCert;

                          { V8.62 see if adjusting name to default used by ordering, need directory }
                            FDir := ExtractFileDir(FSslCert);
                            if (FDir <> '') and DirectoryExists(FDir) then begin
                                FullName := IncludeTrailingPathDelimiter(FDir) + IcsBuildCertFName(MyCommonName) + '.pfx';
                                NewFStamp := IcsGetFileUAge(FullName);
                                if (NewFStamp > 0) or FSslCertAutoOrder then begin
                                    if FSslCert <> FullName then begin
                                        FCertErrs := FCertErrs + ', Creating Using Default Name: ' + FullName;
                                        FSslCert := FullName;
                                    end;
                                end;
                            end;
                        end
                        else
                            FCertErrs := 'No SSL Certificate Configured';   { V8.66 }
                    end;

                 { really need an SSL certificate otherwise SSL does not work }
                    if NewFStamp <= 0 then begin
                         X509Log(FCertErrs + ': ' + MyCommonName);    { V9.5  }

                      { V8.64 create new self signed certificate so server can start }
                         if NOT CreateSelfSigned(HostNr, True) then
                            Exit;
                         FSslCert := FullName;
                    end;

                { V9.5 handle exception opening certificate file here so we can create self signed }
                    if ForceLoad or (FCertFStamp <> NewFStamp) then begin
                        LoadNew := True;
                        FCertFStamp := NewFStamp;
                        SslCtx.SslCertX509.PrivateKey := Nil;  { V8.57 clear old key }
                   //     SslCtx.SslCertX509.LoadFromFile(FSslCert, croTry, croTry, FSslPassword);
                        MyErrs := '';
                        SslCtx.SslCertX509.LoadFromFileEx(FSslCert, croTry, croTry, FSslPassword, MyErrs);   { V9.5 }
                        if MyErrs <> '' then begin
                            FCertErrs := 'Failed to Open Certificate File: ' + FSslCert + ' - ' + MyErrs;
                            X509Log(FCertErrs);
                            if NOT CreateSelfSigned(HostNr, True) then
                                Exit;
                            FSslCert := FullName;
                        end;
                    end;
                end;
            end;

      // should now have a certificate, check it's valid
            if NOT SslCtx.SslCertX509.IsCertLoaded then begin
                FCertErrs := 'SSL Certificate Failed to Load - ' + FSslCert;

           { V8.64 create self signed certificate so server can start }
                if NOT CreateSelfSigned(HostNr, False) then
                    Exit;
                FSslCert := FullName;
                SslCtx.SslCertX509.PrivateKey := Nil;
                SslCtx.SslCertX509.LoadFromFile(FSslCert, croTry, croTry, FSslPassword);
            end;
            if NOT SslCtx.SslCertX509.IsPKeyLoaded then begin
                if (FSslKey = '') then begin
                    FCertErrs := 'SSL Private Key Can Not Be Blank for ' + FSslCert;
                    Exit;
                end;
                if (Pos(PEM_STRING_HDR_BEGIN, FSslKey) > 0) then begin
                   SslCtx.SslCertX509.PrivateKeyLoadFromText(FSslKey, FSslPassword)
                end
                else
                   SslCtx.SslCertX509.PrivateKeyLoadFromPemFile(FSslKey, FSslPassword);
            end ;
            if (NOT SslCtx.SslCertX509.IsInterLoaded) and (FSslInter <> '') then begin
                if (Pos(PEM_STRING_HDR_BEGIN, FSslInter) > 0) then begin
                  { can we check this ?? }
                    SslCtx.SslCertX509.LoadIntersFromString(FSslInter)
                end
                else begin
                    NewFStamp := IcsGetFileUAge(FSslInter);  { keep file time stamp to check nightly, V8.51 UTC time }
                    if NewFStamp <= 0 then begin
                        FCertErrs := 'SSL Intermediate Certificate Not Found: ' + FSslInter + ' for Certificate ' + FSslCert;
                        Exit;
                    end;
                    if ForceLoad or (FInterFStamp <> NewFStamp) then begin
                        LoadNew := True;
                        FInterFStamp := NewFStamp;
                        SslCtx.SslCertX509.LoadIntersFromPemFile(FSslInter);
                    end;
                end;
            end ;

        { V9.1 special case for expired temporary ICS signed certificate }
            FCertExiry := SslCtx.SslCertX509.ValidNotAfter;
            if (Pos(GSSL_INTER_CNAME, SslCtx.SslCertX509.IssuerCName) = 1) then begin
                if ((FCertExiry - Now) <= FRenewalDays) then begin    { V9.4 replace after FExpireDays, was 7 days }
                    X509Log('Replacing Expired ICS Certificate');
                    if NOT CreateSelfSigned(HostNr, True) then    { V9.4 must replace it }
                        Exit;
                    FSslCert := FullName;
                    SslCtx.SslCertX509.PrivateKey := Nil;
                    SslCtx.SslCertX509.LoadFromFile(FSslCert, croTry, croTry, FSslPassword);
                end;
            end;

         { validate SSL certificate chain, helps to ensure server will work! }
            FCertDomains := IcsUnwrapNames (SslCtx.SslCertX509.SubAltNameDNS);

         { V8.47 warning, currently only checking first Host name }
         { V8.57 expire days now configurable }
         { V8.63 optionally don't check self signed }
            if SslCtx.SslCertX509.SelfSigned then begin
                if AllowSelfSign then
                    FCertValRes := chainOK
                else
                    FCertValRes := chainWarn;
                FCertErrs := ', Certificate self signed';
                FCertInfo := 'Server: ' + SslCtx.SslCertX509.CertInfo(False);
            end
            else begin
                FCertErrs := '';
              { V8.64 pass CAList to validate against }
                if NOT IcsSslRootCAStore.InitFlag then   { V9.1 if internal not loaded, do it }
                    IcsSslRootCAStore.Initialise;
                try
                    FCertValRes := SslCtx.SslCertX509.ValidateCertChain(FHostNames[0], IcsSslRootCAStore,
                                                                                    FCertInfo, FCertErrs, FRenewalDays);
                    if FCertErrs <> '' then
                        FCertErrs := ', ' + FCertErrs;
                except
                    FCertValRes := chainFail;
                    FCertErrs := 'Failed to Validate Cert Chain, Exception';   { V8.71 report exception }
                end;
                if FOrderPending and (FCertValRes = chainOK) then begin   { V9.5 Renewal Information says replace it }
                    FCertValRes := chainWarn;
                    FCertErrs := FCertErrs + ', Certificate Needs Replacement Immediately';
                end;

            { V9.5 extra check for expiry }
                if ((FCertExiry - Now) <= 3) then begin
                    FCertValRes := chainWarn;
                    FOrderPending := True;  // need new cert
                    X509Log('Certificate Expires in Less than Three Days, Needs Replacement Immediately: ' + MyCommonName);
                    if FSslCertAutoOrder then
                        X509Log('Certificate Immediate Order for: ' + MyCommonName);
                    FCertErrs := FCertErrs + ', Certificate Expires In Less Than Three Days, Needs Replacement Immediately';
                end;

              { V8.69 check OCSP server if revoked and keep OCSP response for status stapling }
              { V9.5 in May 2025 Let's Encrypt stopped adding X509.UrlOcsp to new certificates, so this
                code becomes superflous, unless using certificates from suppliers still using OCSP }
    {$IFDEF OpenSSL_OcspStaple}
                if (FCertValRes <> chainFail) and FOcspSrvStapling and (SslCtx.SslCertX509.UrlOcsp <> '') then begin
                    X509Log('OCSP Check Starting for: ' + FCertDomains);
                    Inters := TX509List.Create(self);
                    try
                        Inters.X509Class := TX509Base;
                        Inters.LoadAllStack(SslCtx.SslCertX509.X509Inters);
                        FOcspSrvHttp.OnOcspProg := IcsLogEvent;
                        FOcspSrvHttp.DebugLevel := DebugConn;
                        FOcspSrvHttp.ClearOcsp;
                        FOcspSrvHttp.OcspCert := SslCtx.SslCertX509;
                        FOcspSrvHttp.OcspInters := Inters;
                     // must block and wait for OCSP response, so checks don't overlap
                        if FOcspSrvHttp.CheckOcspRevoked(IcsSslRootCAStore.X509Store, 3) then begin   // three seconds wait
                            FCertValRes := chainFail;
                        end;
                        if FOcspSrvHttp.OcspRespStatus <> OCSP_RESPONSE_STATUS_SUCCESSFUL then begin
                            FCertErrs := FCertErrs + ', OCSP Check Failed: ' + FOcspSrvHttp.OcspInters.OcspRespStatusStr;
                        end
                        else if FOcspSrvHttp.OcspCertStatus = V_OCSP_CERTSTATUS_GOOD then begin
                            FCertErrs := FCertErrs + ', OCSP Check OK Not Revoked';
                        end
                        else begin
                            FCertErrs := FCertErrs + ', OCSP Check Failed: ' + FOcspSrvHttp.OcspInters.OcspCertStatusStr;
                        end;
                        if FOcspSrvHttp.DebugLevel > DebugNone then
                            X509Log(FOcspSrvHttp.OcspLastResp);

                   // keep response in Hosts, sent when checking SNI to each connection
                        FOcspStapleResp := FOcspSrvHttp.OcspRespRaw;
                    finally
                        FOcspSrvHttp.OcspInters := Nil;
                        Inters.Free;
                    end;
                end;
    {$ENDIF OpenSSL_OcspStaple}
            end;

         { V9.1 special case for temporary ICS signed certificate, that should be replaced by Let's Encrypt }
            if (FCertValRes = chainOK) and (Pos(GSSL_INTER_CNAME, SslCtx.SslCertX509.IssuerCName) = 1) then begin
    {$IFDEF AUTO_X509_CERTS}  { V8.62 }
                if FSslCertAutoOrder and (CertSupplierProto > SuppProtoNone) and Assigned(FSslX509Certs) then begin
                    FCertValRes := chainWarn;
                    FOrderPending := True;  // need new cert
                    FCertErrs := 'ICS Temporary Certificate Pending Replacement';
                    X509Log(FCertErrs);
                end;
    {$ENDIF}
            end;

         { V8.71 report certificate location }
            Info := '';
            if SslLoadSource = CertLoadFile then begin
                if (FSslRawKeyFile <> '') then    { V9.5 }
                    info := 'Location File ' + FSslRawKeyFile
                else
                    Info := 'Location File ' + SslCert;
                if (Pos(PEM_STRING_HDR_BEGIN, Info) > 0) then   { V9.5 sanity check for textual content not file names }
                    Info := '';
            end
            else
                Info := 'Location Windows Store ' + SslCtx.SslCertX509.CertName;

        { report validation result }
            if FCertValRes = chainOK then begin
                FCertErrs := Info + ', Chain Validated OK' + FCertErrs;
                Result := True;   // nothing more to do
            end
            else begin
                if FCertValRes = chainWarn then begin
                    FCertErrs := Info + ', SSL Certificate Chain Warning' + FCertErrs;
                    Result := True;
                end
                else begin
                    FCertErrs := Info + ', SSL Certificate Chain Failed' + FCertErrs;
                end;
                X509Log(FCertErrs);
                FOrderPending := True;  // need new cert, if automatic ordering available
                X509Log('Certificate Chain Failure, Immediate Order for: ' + MyCommonName);
            end;

         // should we try and order a new certificate from Let's Encrypt
         // V8.64 only once the server is listening, challenges fail otherwise
    {$IFDEF AUTO_X509_CERTS}
           try
                if FOrderPending then begin  { V9.5 chain might be ok, but still want new cert }
                    FOrderPending := False;
                    with (FSslX509Certs as TSslX509Certs) do begin
                        if FSslCertAutoOrder and (CertSupplierProto > SuppProtoNone) and Assigned(FSslX509Certs) then begin
                            CertRenewNow := False; { reset forced order flag }
                            if ForceLoad then
                                FCertErrs := FCertErrs + ', Pending Order fo New Certificate'
                            else begin
                            // main certificate ordering function
                                if NOT IsAccountOpen then { V9.5 open account if not done }
                                    DoCheckOrderCert(HostNr);
                                if (AcmeSuppTot > 0) and DoProcOrderCert(HostNr) then begin
                                    FCertErrs := FCertErrs + ', Ordered New Certificate';
                                end
                                else
                                    FCertErrs := FCertErrs + ', Failed to Order New Certificate';
                            end;
                        end;
                        X509Log(FCertErrs);
                    end;
                end;
            except
                on E:Exception do begin
                    FCertErrs := 'Exception Ordering New Certificate for Renewal - ' + E.Message;
                    X509Log(FCertErrs);
                end;
            end;
    {$ENDIF}
            FRenewalDays := 0;
            FOrderPending := False;
        except
            on E:Exception do begin
                FCertErrs := E.Message; { V8.52 keep exception }
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
 { V8.45 if IcsHostCollection has been specified, use it to define all
   bindings and SSL contexts, including SSL certificates and security,
   with SSL Server Name Indication (SNI) used to select the correct host.
   HTTP descendants of SocketServer might check the Host: header for
   non=SSL connections }

function TSslWSocketServer.ValidateHosts(Stop1stErr: Boolean=True; NoExceptions: Boolean=False; AllowSelfSign: Boolean=False): String; { V8.48, V8.63 }
var
    I, FirstSsl: integer;
    FirstHost, LoadNew, LoadFlag: Boolean;

    procedure AddBinding(const MAddr: String; MPort: Integer; SslFlag: Boolean; var MIndex: Integer; var Info: String);
    var
        SockFam: TSocketFamily;
        ListenItem: TSslWSocketMultiListenItem;
    begin
        if (MAddr = '') OR (NOT WSocketIsIPEx (MAddr, SockFam)) then begin
            raise ESocketException.Create('Host #' + IntToStr(I) +  ', Invalid host listen IP address: ' + MAddr);
            exit ;
        end ;
        if FirstHost then begin    { first is not a multi-listen }
            FirstHost := False;
            Self.Addr := MAddr;
            Self.Port := IntToStr(MPort);
            Self.SocketFamily := SockFam;
            Self.SslEnable := SslFlag;   { V8.50 }
            MIndex := -1;
        end
        else begin
            { ignore duplicates, but keep binding index }
            if NOT FindBinding(MAddr, MPort, MIndex) then begin
                MultiListenSockets.Add;
                MIndex := MultiListenSockets.Count - 1;
                ListenItem := MultiListenSockets [MIndex] as TSslWSocketMultiListenItem;
                ListenItem.Addr := MAddr;
                ListenItem.Port := IntToStr(MPort);
                ListenItem.SocketFamily := SockFam;
                ListenItem.SslEnable := SslFlag;
            end;
        end;
        if Info <> '' then Info := Info + ', ';        { V8.47 }
        Info := Info + MAddr + ':' + IntToStr(MPort);
    end;

begin
    Result := '';
    FValidated := False;                                      { V8.48 }
    if FIcsHosts.Count = 0 then
        Exit;
    Port := '0';        { V8.64 illegal port in case no hosts set }
    FirstSsl := -1;
    FirstHost := True;

// set-up bindings and SSL context for each host
    MultiListenSockets.Clear;
    for I := 0 to FIcsHosts.Count - 1 do begin
        with FIcsHosts [I] do begin
        try
            FCertInfo := 'No certificates available';   { V8.52 }
            FBindInfo := '';
            FCertErrs := '';
            FBindIdxNone := -2;  { bindings are -1 to +x }
            FBindIdx2None := -2;
            FBindIdxSsl := -2;
            FBindIdx2Ssl := -2;
            if (NOT FHostEnabled) then
                continue;

         { create up to four bindings for host, IPv4, IPv6, non-SSL, SSL }
         { V8.64 allow NonSSlPort to be zero for random port, not for SSL }
            if (FBindNonPort > 0) or (FBindSslPort = 0) then begin
                AddBinding(FBindIpAddr, FBindNonPort, False, FBindIdxNone, FBindInfo);
                if FBindIpAddr2 <> '' then
                    AddBinding(FBindIpAddr2, FBindNonPort, False, FBindIdx2None, FBindInfo);
                if FNoSSL then
                    FAuthSslCmd := False;           { V9.1 }
            end;
            if (FBindSslPort > 0) then begin
                if FNoSSL then begin            { V9.1 }
                    FCertErrs := 'Host #' + IntToStr(I) + ', SSL/TLS Not Allowed';
                    Result := Result + FCertErrs + #13#10;
                    if Stop1stErr then
                        raise ESocketException.Create(Result);
                    continue;
                end;
                AddBinding(FBindIpAddr, FBindSslPort, True, FBindIdxSsl, FBindInfo);
                if FBindIpAddr2 <> '' then
                    AddBinding(FBindIpAddr2, FBindSslPort, True, FBindIdx2Ssl, FBindInfo);
            end;

     { if using SSL, set-up context with server certificates }
            if (FBindSslPort > 0) or FAuthSslCmd then begin  { V8.63 optional SSL }
                if (SslLoadSource = CertLoadFile) and (FSslCert = '') then begin   { V8.71 not for Windows Store }
                    FCertErrs := 'Host #' + IntToStr(I) + ', SSL certificate can not be blank';
                    Result := Result + FCertErrs + #13#10;  { V8.52 }
                    if Stop1stErr then
                        raise ESocketException.Create(Result);
                //    continue;   { V8.66 must still create SslContext }
                end;
                FRenewalDays := CertExpireDays;  { V9.5 may be updated by renewal information }

             { V8.64 really need a hostname of some sort to create a certificate file }
                if Length(FHostNames[0]) <= 1 then
                    FHostNames[0] := 'unknown';
                if NOT Assigned(SslCtx) then
                    SslCtx := TSslContext.Create(Self);
                if FirstSsl < 0 then
                    FirstSsl := I;
           {$IFNDEF NO_DEBUG_LOG}
                SslCtx.IcsLogger := IcsLogger;                           { V8.46 }
           {$ENDIF}
                SslCtx.SslVersionMethod := sslBestVer_SERVER;
                SslCtx.SslMinVersion := sslVerTLS1_2;  { V9.3 TLSv1 no longer supported }
                SslCtx.SslMaxVersion := sslVerMax;
                SslCtx.SslOptions2 := [sslOpt2_NO_SESSION_RESUMPTION_ON_RENEGOTIATION, sslOpt2_NO_COMPRESSION];
                SslCtx.SslCipherList := sslCiphersMozillaSrvTLS12;
                SslCtx.SslCipherList13 := sslCipherSuitesTLS13;   { V9.3 }

             { set SSL security choices before loading certificates, which may then fail }
                case FSslSrvSecurity of
                  sslSrvSecNone: begin                { all protocols and ciphers, any key lenghts }
                      SslCtx.SslCipherList := 'ALL';
                      SslCtx.SslSecLevel := sslSecLevelAny;
                      SslCtx.SslOptions2 := SslCtx.SslOptions2 + [sslOpt2_CIPHER_SERVER_PREFERENCE];  { V8.66 }
                  end;
                  sslSrvSecBack: begin                { TLS1 or later, backward ciphers, RSA/DH keys=>1024, ECC=>160, no MD5, SHA1 }
                      SslCtx.SslCipherList := sslCiphersMozillaSrvBack;   { V9.3 AddTls13 gone }
                      SslCtx.SslSecLevel := sslSecLevel80bits;
                      SslCtx.SslOptions2 := SslCtx.SslOptions2 + [sslOpt2_CIPHER_SERVER_PREFERENCE];  { V8.66 }
                  end;
                  sslSrvSecInter, sslSrvSecInterFS, sslSrvSecHigh: begin   { TLS1.2 or later, intermediate ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                      SslCtx.SslCipherList := sslCiphersMozillaSrvTLS12;   { V8.66 was sslCiphersMozillaSrvInterFS }
                      SslCtx.SslSecLevel := sslSecLevel112bits;  // Dec 2016  keys=>2048, ECC=>224, no RC4, no SSL3, no SHA1 certs
                  end;
                  sslSrvSecHigh128: begin               { TLS1.2 or later, RSA/DH keys=>3072, ECC=>256, FS forced }
                      SslCtx.SslCipherList := sslCiphersMozillaSrvTLS12;   { V8.66 was sslCiphersMozillaSrvInterFS }
                      SslCtx.SslSecLevel :=sslSecLevel128bits;
                  end;
                  sslSrvSecHigh192: begin                { TLS1.2 or later, RSA/DH keys=>7680, ECC=>384, FS forced }
                      SslCtx.SslCipherList := sslCiphersMozillaSrvTLS12;   { V8.66 was sslCiphersMozillaSrvInterFS }
                      SslCtx.SslSecLevel := sslSecLevel192bits;
                  end;
                  sslSrvSecTls12Less: begin             { TLS1.2 only, intermediate ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                      SslCtx.SslMaxVersion := sslVerTLS1_2;
                      SslCtx.SslCipherList := sslCiphersMozillaSrvTLS12;   { V8.66 was sslCiphersMozillaSrvInterFS }
                      SslCtx.SslSecLevel := sslSecLevel112bits;
                  end;
                  sslSrvSecTls13Only: begin             { TLS1.3 or later, intermediate FS ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                      SslCtx.SslMinVersion := sslVerTLS1_3;
                      SslCtx.SslCipherList := sslCiphersMozillaSrvTLS12;   { V8.66 was sslCiphersMozillaSrvInterFS }
                      SslCtx.SslSecLevel := sslSecLevel112bits;
                  end;

                end;
                SslCtx.SslECDHMethod := sslECDHAuto;

             { V9.3 see of overriding ciphers }
                if FSslCipherList12 <> '' then
                     SslCtx.SslCipherList := FSslCipherList12;
                if FSslCipherList13 <> '' then
                     SslCtx.SslCipherList13 := FSslCipherList13;
                if FSslCryptoGroups <> '' then
                     SslCtx.SslCryptoGroups := FSslCryptoGroups;

            { Enables OpenSsl's internal session caching }
                SslCtx.SslSessionCacheModes := [sslSESS_CACHE_SERVER];

            { V8.69 are we supporting OCSP stapling }
{$IFDEF OpenSSL_OcspStaple}  { V9.5 }
                SslCtx.SslOcspStatus := FOcspSrvStapling;
{$ENDIF} // OpenSSL_OcspStaple

             { V8.57 do we want a client SSL certificate from the browser,
                  NOTE you should check it the OnSslHandshakeDone event and
                  close the connection if invalid, beware this usually causes
                  the browser to request a certificate which can be obtrusive. }
             { V8.71 allow client certificate to be specified for individual hosts }
                SslCtx.SslVerifyPeer := false;
                if (FSslCliCertMethod > sslCliCertNone) or (FCliCertMethod > sslCliCertNone) then begin
                    SslCtx.SslVerifyPeer := True;
                    if (FSslCliCertMethod = sslCliCertOptional) or (FCliCertMethod = sslCliCertOptional)  then
                        SslCtx.SslVerifyPeerModes := [SslVerifyMode_PEER, SslVerifyMode_CLIENT_ONCE]
                    else
                        SslCtx.SslVerifyPeerModes := [SslVerifyMode_PEER, SslVerifyMode_FAIL_IF_NO_PEER_CERT, SslVerifyMode_CLIENT_ONCE];
                end;
                SslCtx.SslSessionTimeout := 300; //sec
                SslCtx.SslDefaultSessionIDContext := 'AnyStringForSessionCaching';
{$IFDEF OpenSSL_Deprecated}   { V9.5 }
                if FDHParams <> '' then begin
                    if (Pos(PEM_STRING_HDR_BEGIN, FDHParams) > 0) then
                        SslCtx.SslDHParamLines.Text := FDHParams
                    else
                        SslCtx.SslDHParamFile := FDHParams;
                end;
{$ENDIF OpenSSL_Deprecated}   { V9.5 }

            { V8.66 allow application to modify IcsHost and SslContext for special ciphers or protocols }
                if Assigned(FonBeforeContextInit) then begin
                    FOnBeforeContextInit(self, FIcsHosts [I]);
                 end;

            { load certificate, private key and optional intermediates, that may all be
              in the same PEM or PFX bundle file or seperate files, or may be base64 text,
              validate SSL certificate chain, helps to ensure server will work!  Note
              will not order new certificate because server not yet running to check domain }
                LoadFlag := LoadOneCert(I, True, LoadNew, AllowSelfSign);  { V8.63 self sign }
                if (NOT LoadFlag) or (FCertValRes <> chainOK) then
                    Result := Result + 'Host #' + IntToStr(I) + ' ' + FHostNames[0] + ', '  + FCertErrs + #13#10;
                if NOT LoadFlag then begin
                    if Stop1stErr then begin   //  we may load cert later
                        if NoExceptions then Exit;
                        raise ESocketException.Create(FCertErrs);
                    end;
                end;
                SslCtx.InitContext;
            end;
            except
                on E:Exception do begin
                    FCertErrs := E.Message; { V8.52 keep exception }
                    Result := Result + 'Host #' + IntToStr(I) +  ', ' + E.Message + #13#10;  { V8.52 cosmetic }
                    if Stop1stErr then
                        Raise;
                end;
            end;
        end;
    end;
    if FirstHost then begin    { V8.64 sanity check }
        Result := Result + 'No Bindings Found';
        if NoExceptions then Exit;
        raise ESocketException.Create(Result);
    end;

  { set server context as first host with SSL }
    if FirstSsl >= 0 then
        FSslContext := FIcsHosts [FirstSsl].SslCtx;
    FValidated := True;                                      { V8.48 }
    if FAcmeAccountOpen then          { V9.5 has ACME account been opened for renewal check, must be closed }
        OrderClose;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.48 if IcsHostCollection has been specified, recheck SSL certificates
  to see if new files found (returns True) or old certificates are about to expire }
function TSslWSocketServer.RecheckSslCerts(var CertsInfo: String; Stop1stErr: Boolean=True;
                                       NoExceptions: Boolean=False; AllowSelfSign: Boolean=False): Boolean; { V8.48, V8.63 }
var
    I: integer;
    LoadNew, LoadFlag: Boolean;
begin
    Result := False;
    CertsInfo := '';
    if FIcsHosts.Count = 0 then
        Exit;
    if FNoSSL then             { V9.1 }
        Exit;
    for I := 0 to FIcsHosts.Count - 1 do begin
        with FIcsHosts [I] do begin
            if NOT FHostEnabled then continue;
            if (FBindSslPort = 0) and (NOT FAuthSslCmd) then continue;   { V8.63 optional SSL }

          { load any new certificates, might order new SSL certificate }
            try
                LoadFlag := LoadOneCert(I, False, LoadNew, AllowSelfSign); { V8.63 }
                if FCertValRes <> chainOK then
                    CertsInfo := CertsInfo + 'Host #' + IntToStr(I) + ' ' + FHostNames[0] + ', '  + FCertErrs + #13#10;
                if NOT LoadFlag then begin
                    if Stop1stErr then begin
                        if NOT NoExceptions then
                            raise ESocketException.Create(FCertErrs);
                        Exit;
                    end;
                end;
                if LoadNew and LoadFlag then begin
                    SslCtx.SslSetCertX509;   { V8.57 needed so new certificate recognised }
                    CertsInfo := CertsInfo + 'Loaded new SSL certificate: ' + FSslCert + #13#10;
                    Result := True;
                end;
            except
                on E:Exception do begin
                    FCertErrs := E.Message; { V8.52 keep exception }
                    CertsInfo := CertsInfo + 'Host #' + IntToStr(I) +  ', ' + E.Message + #13#10;  { V8.52 cosmetic }
                    if Stop1stErr then
                        Raise;
                end;
            end;
        end;
    end;
    if FAcmeAccountOpen then          { V9.5 has ACME account been opened for renewal check, must be closed }
        OrderClose;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ also called when MultiListen is called }
{ beware this function does not give errors if some listeners fail due to port conflicts }
procedure TSslWSocketServer.Listen;                             { V8.46 }
var
    I, K: Integer;
begin
{ better to call this before Listen and handle errors better, but in case not }
    if NOT FValidated then
        ValidateHosts(True, False, False);    { V8.64 exception for IcsHosts errors }
    FValidated := False;
    inherited Listen;

  { V8.64 keep bindport for each IcsHost }
    if FIcsHosts.Count > 0 then begin
        FIcsHosts[0].FBindSrvPort := Self.FBindPort;
        if MultiListenSockets.Count > 0 then begin
            for K := 0 to MultiListenSockets.Count - 1 do begin
                for I := 0 to FIcsHosts.Count - 1 do begin
                    if NOT (FIcsHosts[I].HostEnabled) then continue;
                    if (FIcsHosts[I].FBindIdxNone = K) or (FIcsHosts[I].FBindIdxSsl = K) then
                        FIcsHosts[I].FBindSrvPort := MultiListenSockets[K].FBindPort;
                end;
            end;
        end;
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslWSocketClient.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FIcsHostIdx := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslWSocketClient.Destroy;   { V8.62 }
begin
    try
        FreeAndNil(AlpnSslCtx);
    finally
        inherited;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWSocketClient.StartConnection;
begin
    inherited StartConnection;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ SSL client has sent a host name using SNI, look up IcsHost }
{ V8.64 now called from ClientHello callback so have ALPN information }
procedure TSslWSocketClient.TriggerSslServerName(var Ctx: TSslContext; var ErrCode: TTlsExtError);  { V8.45 }
var
    I,J: Integer;
    MyServer: TSslWSocketServer;
    PortFlag, SNIFlag: Boolean;
{$IFDEF AUTO_X509_CERTS}  { V8.62 }
    CertFName: String;
{$ENDIF}

 { V8.64 log what is happening via SslX509Certs component }
    procedure X509Log(const Msg: String);
    begin
{$IFDEF AUTO_X509_CERTS}  { V8.69 }
        if Assigned(MyServer.FSslX509Certs) then
            (MyServer.FSslX509Certs as TSslX509Certs).LogEvent(Msg);
{$ENDIF}
    end;

begin
    inherited TriggerSslServerName(Ctx, ErrCode);
    MyServer := FServer as TSslWSocketServer;

  { if event has not set an SslContext, look for IcsHost instead }
    if NOT Assigned(Ctx) then begin
        with MyServer do begin
            if FIcsHosts.Count > 0 then begin
                SNIFlag := False;
                for I := 0 to FIcsHosts.Count - 1 do begin
                    with MyServer.FIcsHosts [I] do begin
                        if NOT (HostEnabled) then
                            continue;
                        PortFlag := False;
                     { V8.65 if using AUTH command check for non-SSL port }
                        if FAuthSslCmd then begin
                            if (FBindIdxNone = Self.FMultiListenIdx) then
                                PortFlag := True;
                            if (FBindIdx2None = Self.FMultiListenIdx) then
                                PortFlag := True;
                        end;
                        if (FBindIdxSsl = Self.FMultiListenIdx) then
                            PortFlag := True;
                        if (FBindIdx2Ssl = Self.FMultiListenIdx) then
                            PortFlag := True;
                        if NOT PortFlag then
                            continue;
                        if (HostNameTot = 0) then
                            Continue;
                  { V9.5 check Hosts lists for SNI, not certificate SANs which might find wild card certificate we don't want }
                   // if SslCtx.SslCertX509.PostConnectionCheck(Self.FSslServerName) then begin  // check certificate subject names
                        for J := 0 to HostNameTot - 1 do begin
                           // V9.5 do we want a wild card host name ????
                            if ( {(HostNames [J] = '*') or} (HostNames [J] = Self.FSslServerName)) then begin
                                Self.FIcsHostIdx := I;
                                Self.FHostTag := HostTag;
                                Ctx := SslCtx;
    {$IFDEF OpenSSL_OcspStaple}  { V9.5 }
                                Self.OcspStapleRaw := FOcspStapleResp; { V8.69 set OCSP staple response }
    {$ENDIF} // OpenSSL_OcspStaple
                                X509Log('SSL SNI Found: ' + Self.FSslServerName + ' for IcsHost #' + IntToStr(I));  { V8.65 always log }
                                SNIFlag := True;
                                break;
                            end;
                            if SNIFlag then
                                break;
                        end;
                    end;
                end;
                if NOT SNIFlag then
                    X509Log('SSL SNI Not Found: ' + Self.FSslServerName + ' in IcsHosts');  { V8.65 always log }
            end;
        end;
    end;

{$IFDEF AUTO_X509_CERTS}  { V8.62 }

  { V8.64 check for Acme TLS challenge for Acme SSL certificate, load it }
    if (CliHelloData.AlpnList = AlpnAcmeTls1) then begin
        ErrCode := teeAlertFatal;   // connection dies unless we handle challenge
        if Assigned(OnClientAlpnChallg) then begin

        // if application can handle ACME tls-alpn-01 challenge, ask for certificate
            CertFName := '';
            try
                OnClientAlpnChallg(Self, Self.SslServerName, CertFName);

              // load new certifcate into ALPN context and swap to it
                if (CertFName <> '') and FileExists(CertFName) then begin
                    if NOT Assigned(AlpnSslCtx) then
                        AlpnSslCtx := TSslContext.Create(Self);
                    //    AlpnSslCtx.Assign(Ctx);   { V9.1 copy existing context to keep callbacks }
                    AlpnSslCtx.SslCertX509.LoadFromFile(CertFName, croTry, croTry, 'password');
                    AlpnSslCtx.SslVerifyPeer := false;
                    AlpnSslCtx.SslVersionMethod := sslBestVer_SERVER;
                    AlpnSslCtx.SslMinVersion := sslVerTLS1_2;
                    AlpnSslCtx.SslMaxVersion := sslVerMax;
                    AlpnSslCtx.SslCipherList := sslCiphersServer;
                    AlpnSslCtx.InitContext;
                    SetSslCallbacks(AlpnSslCtx.SslCtxPtr);     { V9.1 set callbacks for new context }
                    if AlpnSslCtx.CheckPrivateKey then begin
                        Ctx := AlpnSslCtx;
                        X509Log('Client Hello Loaded new tls-alpn-01 challenge certificate: ' + AlpnSslCtx.SslCertX509.CertInfo(true));
                        ErrCode := teeOk;
                    end
                    else
                        X509Log('Client Hello Failed to load certificate, no private key or Context Error: ' + CertFName);
                end
                else
                    X509Log('Client Hello Failed to find tls-alpn-01 challenge certificate: ' + CertFName);
            except
                on E:Exception do begin
                    X509Log('Client Hello Exception handling tls-alpn-01 challenge: ' + E.Message);
                end;
            end ;
        end
        else
            X509Log('Client Hello ACME tls-alpn-01 challenge not handled');
    end;
   {$ENDIF}
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.62 application layer protocol negotiation, servers only }
procedure TSslWSocketClient.TriggerSslAlpnSelect(ProtoList: TStrings; var SelProto: String; var ErrCode: TTlsExtError);
begin
    inherited  TriggerSslAlpnSelect(ProtoList, SelProto, ErrCode);

 // check for Acme TLS challenge for Acme SSL certificate, set it
    if ProtoList.Count = 0 then Exit;     { sanity check }
    if (ProtoList[0] = AlpnAcmeTls1) then begin
        SelProto := AlpnAcmeTls1;
        ErrCode := teeOk;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSslWSocketMultiListenItem }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslWSocketMultiListenItem.Create(Collection: TCollection);
begin
    inherited Create(Collection);
    FSslEnable := TRUE;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsHost }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsHost.Create(Collection: TCollection);
begin
    inherited;
    FHostNames := TStringList.Create;
    FHostNames.Add ('*');
    FHostTag := 'HostTag';
    FBindIpAddr := ICS_ANY_HOST_V4;
    FBindNonPort := 0;
    FBindSslPort := 0;
    FSslSrvSecurity := sslSrvSecHigh;    { V8.71 was sslSrvSecBack but n ow TLS/1.2 and later }
    FCertPKeyType := PrivKeyRsa2048;     { V8.71 }
    FCertSignDigest := Digest_sha256;    { V8.71 }
    FCliCertMethod := sslCliCertNone;    { V8.71 }
    FRenewalDays := 30;                  { V9.5 }
    FAcmeCertValidity := 90;             { V9.5 }
    FOrderPending := False;              { V9.5 }
    FHostEnabled := True;
    SslCtx := Nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsHost.Destroy;
begin
    try  { V8.71 }
        FreeAndNil (SslCtx);
        FreeAndNil (FHostNames);
    finally
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsHost.GetDisplayName: string;
begin
    Result := FHostTag + ' - ';
    if FDescr <> '' then
        Result := Result + FDescr
    else begin
        Result := Result + FBindIpAddr + ':';
        if FBindNonPort = 0 then
            Result := Result + IntToStr(FBindSslPort)
        else begin
           Result := Result + IntToStr(FBindNonPort);
            if FBindSslPort <> 0 then
                Result := Result + '/' + IntToStr(FBindSslPort);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsHost.SetHostNames(Value : TStrings);
begin
    FHostNames.Assign(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsHost.GetHostNameTot: integer;
begin
    if Assigned (FHostNames) then
        Result := FHostNames.Count
     else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsHosts }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsHostCollection.Create(Owner: TPersistent);
begin
  FOwner := Owner;
  inherited Create(TIcsHost);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsHostCollection.GetItem(Index: Integer): TIcsHost;
begin
  Result := TIcsHost(inherited GetItem(Index));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsHostCollection.SetItem(Index: Integer; Value: TIcsHost);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsHostCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsLoadIcsHostsFromIni(MyIniFile: TCustomIniFile; IcsHosts: TIcsHostCollection;
                                 const Prefix: String = 'IcsHost'; Server: TWSocketServer = Nil): Integer;  { V9.5 added Server }
var
    J, V: Integer;
    section, hosts, S: String;
    DefSupplierTitle: String;
    DefAcmeSupplier: String;
    DefAcmeCertProfile: String;
    DefAcmeCertValidity: Integer;
    DefCertChallenge: String;
    DefCertPKeyType: String;
    DefCertSignDigest: String;
begin
    Result := 0;
    if NOT Assigned (MyIniFile) then
        raise ESocketException.Create('Must open and assign INI file first');
    if NOT Assigned (IcsHosts) then
        raise ESocketException.Create('Must assign IcsHosts first');
    IcsHosts.Clear;

  { V9.5 see if server has any certificate ordering defaults, avoids specifying them for each host }
    if Assigned(Server) then begin
        with Server as TSslWSocketServer do begin
            DefSupplierTitle := SrvSupplierTitle;
            DefAcmeSupplier := GetEnumName (TypeInfo (TAcmeSupplier), Ord (SrvAcmeSupplier));
            DefAcmeCertProfile := SrvAcmeCertProfile;
            DefAcmeCertValidity := SrvAcmeCertValidity;
            DefCertChallenge := GetEnumName (TypeInfo (TChallengeType), Ord(SrvCertChallenge));
            DefCertPKeyType := GetEnumName (TypeInfo (TSslPrivKeyType), Ord(SrvCertPKeyType));
            DefCertSignDigest := GetEnumName (TypeInfo (TEvpDigest), Ord(SrvCertSignDigest));
        end;
    end
    else begin
        DefSupplierTitle := '';
        DefAcmeSupplier := 'AcmeLetsEncrypt';
        DefAcmeCertProfile := '';
        DefAcmeCertValidity := 90;
        DefCertChallenge := 'ChallNone';
        DefCertPKeyType := 'PrivKeyRsa2048';
        DefCertSignDigest := 'Digest_sha256';
    end;

  { allow up to 100 hosts }
    for J := 1 to 100 do begin
        section := Prefix + IntToStr (J);
        hosts := IcsTrim(MyIniFile.ReadString(section, 'Hosts', ''));
        if hosts = '' then continue;
     { V8.57 prefer new HostEnabled over older Enabled, if supplied }
        S := MyIniFile.ReadString(section, 'HostEnabled', '');   { V8.63 typo, was Nost }
        if S = '' then S := MyIniFile.ReadString(section, 'Enabled', 'False');
        if NOT IcsCheckTrueFalse(S) then continue;
        IcsHosts.Add;
        Result := Result + 1;

    { read site hosts from INI file   }
        with IcsHosts[IcsHosts.Count - 1] do begin
            HostEnabled := True;
            HostNames.CommaText := StringReplace(IcsLowercase(hosts), ' ', '', [rfReplaceAll]);
            BindIpAddr := MyIniFile.ReadString(section, 'BindIpAddr', '');
            BindIpAddr2 := MyIniFile.ReadString(section, 'BindIpAddr2', '');
            BindNonPort := MyIniFile.ReadInteger(section, 'BindNonPort', 0);
            BindSslPort := MyIniFile.ReadInteger(section, 'BindSslPort', 0);
            HostTag := IcsTrim(MyIniFile.ReadString(section, 'HostTag', ''));
            Descr := IcsTrim(MyIniFile.ReadString(section, 'Descr', ''));
            ForwardProxy := IcsCheckTrueFalse(MyIniFile.ReadString (section, 'ForwardProxy', 'False'));
            Proto := IcsTrim(MyIniFile.ReadString(section, 'Proto', ''));
            AuthSslCmd := IcsCheckTrueFalse(MyIniFile.ReadString (section, 'AuthSslCmd', 'False'));      { V8.63 }
            AuthForceSsl := IcsCheckTrueFalse(MyIniFile.ReadString (section, 'AuthForceSsl', 'False'));  { V8.64 }
            WebDocDir := IcsTrim(MyIniFile.ReadString(section, 'WebDocDir', ''));
            WebTemplDir := IcsTrim(MyIniFile.ReadString(section, 'WebTemplDir', ''));
            WebDefDoc := IcsTrim(MyIniFile.ReadString(section, 'WebDefDoc', ''));
            WebLogDir := IcsTrim(MyIniFile.ReadString(section, 'WebLogDir', ''));
            WellKnownPath := IcsTrim(MyIniFile.ReadString(section, 'WellKnownPath', ''));   { V8.49 }
            WebRedirectURL := IcsTrim(MyIniFile.ReadString(section, 'WebRedirectURL', '')); { V8.49 }
            WebRedirectStat := MyIniFile.ReadInteger(section, 'WebRedirectStat', 0);        { V8.49 }
            CertSupplierProto := SuppProtoNone;   { V8.59 }
            FOrderPending := False;   { V9.5 }
            FRenewCheckDT := 0;       { V9.5  }

            if BindSslPort <> 0 then begin
                S := IcsTrim(MyIniFile.ReadString(section, 'SslSecLevel', ''));
                if S = '' then
                    V := -1
                else if IsDigit(S[1]) then   { V8.57 ini may contain integer or enum type string }
                    V := atoi(S)
                else
                    V := GetEnumValue (TypeInfo (TSslSrvSecurity), S);
                if V < 0 then V := Ord(sslSrvSecDefault); // sanity check
                SslSrvSecurity := TSslSrvSecurity(V);
                SslCert := IcsTrim(MyIniFile.ReadString(section, 'SslCert', ''));
                SslKey := IcsTrim(MyIniFile. ReadString(section, 'SslKey', ''));
                SslPassword := IcsTrim(MyIniFile.ReadString(section, 'SslPassword', ''));
                SslInter := IcsTrim(MyIniFile.ReadString(section, 'SslInter', ''));
                CliCertMethod := TSslCliCertMethod(GetEnumValue (TypeInfo (TSslCliCertMethod),
                                      IcsTrim(MyIniFile.ReadString(section, 'CliCertMethod', 'sslCliCertNone'))));    { V8.71 }
                if CliCertMethod > High(TSslCliCertMethod) then
                    CliCertMethod := sslCliCertNone;                                                      { sanity test }
                SslLoadSource := TSslLoadSource(GetEnumValue (TypeInfo (TSslLoadSource),
                                           IcsTrim(MyIniFile.ReadString(section, 'SslLoadSource', 'CertLoadFile'))));      { V8.71 }
                if SslLoadSource > High(TSslLoadSource) then
                    SslLoadSource := CertLoadFile;                                                         { sanity test }
                SslCipherList12 := IcsTrim(MyIniFile. ReadString(section, 'SslCipherList12', ''));     { V9.3 }
                SslCipherList13  := IcsTrim(MyIniFile. ReadString(section, 'SslCipherList13', ''));    { V9.3 }
                SslCryptoGroups  := IcsTrim(MyIniFile. ReadString(section, 'SslCryptoGroups', ''));    { V9.3 }
                SslRawKeyFile := IcsTrim(MyIniFile. ReadString(section, 'SslRawKeyFile', ''));         { V9.5 raw private key file for Raw Public Key (RPK) without certificate }

              { V8.57 following are for automatic ordering and installation of SSL certificates }
                CertSupplierProto := TSupplierProto(GetEnumValue (TypeInfo (TSupplierProto),
                                            IcsTrim(MyIniFile.ReadString(section, 'CertSupplierProto', 'SuppProtoNone'))));
                if CertSupplierProto > High(TSupplierProto) then
                    CertSupplierProto := SuppProtoNone;                          { V8.59 sanity test }
                CertDirWork := IcsTrim(MyIniFile.ReadString(section, 'CertDirWork', ''));

             { V9.5 ensure that AcmeSupplier is the same for all IcsHosts, we can not handle multiple suppliers  }
                S := IcsTrim(MyIniFile.ReadString(section, 'AcmeSupplier', DefAcmeSupplier));
                if DefAcmeSupplier = '' then
                    DefAcmeSupplier := S;
                if DefAcmeSupplier = '' then
                    DefAcmeSupplier := 'AcmeLetsEncrypt';
                if DefAcmeSupplier <> S then
                    S := DefAcmeSupplier;
                AcmeSupplier := TAcmeSupplier(GetEnumValue (TypeInfo (TAcmeSupplier), S));      { V9.5 }
                if (AcmeSupplier > High(TAcmeSupplier)) or (AcmeSupplier < Low(TAcmeSupplier)) then
                    AcmeSupplier := AcmeLetsEncrypt;

                SupplierTitle := Trim(MyIniFile.ReadString(section, 'SupplierTitle', DefSupplierTitle));            { V9.5 }
                AcmeCertProfile := IcsTrim(MyIniFile.ReadString(section, 'AcmeCertProfile', DefAcmeCertProfile));   { V9.5 }
                AcmeCertValidity := MyIniFile.ReadInteger(section, 'AcmeCertValidity', DefAcmeCertValidity);        { V9.5 }
                CertChallenge := TChallengeType(GetEnumValue (TypeInfo (TChallengeType),
                                                      IcsTrim(MyIniFile.ReadString(section, 'CertChallenge', DefCertChallenge))));
                if CertChallenge > High(TChallengeType) then
                    CertChallenge := ChallNone;                                 { V8.59 sanity test }
                CertPKeyType := TSslPrivKeyType(GetEnumValue (TypeInfo (TSslPrivKeyType),
                                                IcsTrim(MyIniFile.ReadString(section, 'CertPKeyType', DefCertPKeyType))));
                if CertPKeyType > High(TSslPrivKeyType) then
                    CertPKeyType := PrivKeyRsa2048;                             { V8.59 sanity test }
                CertSignDigest := TEvpDigest(GetEnumValue (TypeInfo (TEvpDigest),
                                              IcsTrim(MyIniFile.ReadString(section, 'CertSignDigest', DefCertSignDigest))));
                if CertSignDigest > High(TEvpDigest) then
                    CertSignDigest := Digest_sha256;                          { V8.59 sanity test }
            end;
        end;
    end;
end;




{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$ENDIF} // USE_SSL

end.
