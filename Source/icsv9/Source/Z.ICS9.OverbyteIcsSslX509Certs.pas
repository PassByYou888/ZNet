{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Automatically download SSL X509 certificates from various
              issuers, including free certificates from Let's Encrypt, and
              other Acme suupliers such as Google Trust Services.
              Supports ACME V2 protocols (RFC8555).
Creation:     Apr 2018
Updated:      Oct 2025
Version:      V9.5
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2025 by Angus Robertson, Magenta Systems Ltd,
              Croydon, England. delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

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

Trade Marks:  Let’s Encrypt and ISRG are trademarks of the Internet Security
              Research Group. All rights reserved.


Overview
--------

SSL/TLS X509 certificates
-------------------------

There are effectively three classes of SSL/TLS X509 certificates, Domain Validated,
Organisation Validated and Extended Validated, in order of cost and benefit,
usually with three variations, single domain, multiple domains (SANs), and
wildcard.  Adding multiple domains to a certificate can ease administration and
is cheaper than multiple certificates, wild card means any sub domains usually for
the cost of about six single domains.

Domain Validated certificate issuance is mostly automated so they are cheap (or
free), using one of four challenge methods: file validation where the supplier
checks for a specific file under the domain, usually http://domain/.well-known/file,
domain validation where a special DNS record is created that can be accessed by
the supplier, TLS-ALPN SSL SNI (server name indication) validated where an
https://domain/ connection is opened passing data using the ALPN extension, with
the server returning a special self signed SSL certificate. and email validation
where an email is sent to a predefined address at the domain, ie admin@domain,
with a supplier link that must be clicked to confirm receipt and domain ownership.

File, TLS-ALPN and domain validation challenges can be automated, file is easiest
using a simple HTTP server, TLS-ALPN using an HTTPS server, while domain validation
is dependent on being able to access and control the DNS server of which there are
many different products.  Note validating challenges are not instant, the supplier
may have a queue of challenges waiting to the tested, but usually happen within a
couple of minutes. Applications need to be aware the wait may be longer.
Automated wild card certificates typically use the domain validation challenge.

Organisation and Extended Validated certificates can be ordered online, but
require manual validation that the company or organisation legally exists and is
entitled to use the domain name which may take several days or weeks for extended
validation if legal evidence is required.  Once approved, the certificate and be
downloaded automatically.


ACME Suppliers Supported
------------------------

More information about these suppliers and Acme in general at:
https://acmeprotocol.dev/getting-started/

AcmeLetsEncrypt, AcmeLetsEncryptTest
--------------------------
Let's Encrypt, domain validated, multiple domains, multiple wildcards allowed, 90 day.
https://letsencrypt.org/
External account: Not Required
ICS Status: Tested OK, email no longer required.
Certificate: 90 day expiry, reducing to 10 days in 2029.
Challenges: HTTP, DNS, ALPN


AcmeGoogle, AcmeGoogleTest
--------------------------
Public Certificate Authority, from Google Trust Services, multiple wild cards allowed, 1 to 90 day.
https://pki.goog/
External account: Required, get EAB Credentials by installing Google Cloud CLI Windows application, creating
a new Google Cloud project, binding an existing Google account to the project, enabled GoogleAPIs, then finally
running a command that returns the EAB credentials, just a new command lines in a Command Window.   EAB
credential can only be used once for a new Acme account (ie new public key).
ICS Status: Tested OK.
Certificate: 1 to 90 day expiry selectable, Issuer: WR1, Root: GTS Root R1 (optionally Globalsign)
Limits: unlimited and free.
Challenges: HTTP, DNS, ALPN


AcmeDigicert, AcmeDigicertTest
------------------------------
No free certificates, only organisation and extended validation commercial certificates
https://docs.digicert.com/en/certcentral/certificate-tools/certificate-lifecycle-automation-guides/third-party-acme-integration.html
External account: Required, https://www.digicert.com/account/signup/, certificate organisations need pre-authorisation,
payment details needed, different EAB credentials seem to be provided for different types of certificates, although
directory seems to be the same.
ICS Status: Testing for account only, no certificates ordered, only commercial so needs prepayment.
Challenges: HTTP, DNS


AcmeZeroSSL
-----------
ZeroSSL, Domain Validated, 3 90 day certs free, paid accounts for more and for one year and wild card.
https://zerossl.com/documentation/api/
External account: Required, get EAB Credentials from https://app.zerossl.com/dashboard/start
ICS Status: Testing failed, could not create an Acme account, might need billing information.


AcmeSslcomRSA, AcmeSslcomECC
----------------------------
Free 90 day single domain certificates, paid accounts one year and other certificate types.
If account has funds, issues one year.
https://www.ssl.com/how-to/order-free-90-day-ssl-tls-certificates-with-acme/
External account: Required, get EAB Credentials from https://secure.ssl.com/users/new
ICS Status: Testing failed, could not create an Acme account, might need billing information.


Not spent much time testing the commercial ACME suppliers, will only do so if
there is commercial need.  It's possible the component will need tweaking slightly
since each supplier interprets the ACME RFC in slightly different ways.



TSslX509Certs Overview
----------------------

The TSslX509Certs component automatically downloads SSL/TLS X509 certificates from
various ACME suppliers, including free certificates from Let's Encrypt and Google
Trust Services, and commercial certificates from other ACME suppliers such as
Digicert, ZeroSSL, SSL.com, The component automates the process from creating
a new private key and certificate request, placing the order, arranging for
domain validated certificates to be checked by various challenge methods,
collecting the certificate and intermediate, creating PEM and PKC12 bundle files
with the private key, then copying the files to the web server ready for
automatic installation. The TSslWSocketServer, TSslHttpServer, TSslHttpAppSrv,
TIcsProxy, TIcsHttpProxy, TSslFtpServer and TIcsIpStrmLog components can assign
a TSslX509Certs component to support automatic certificate ordering of domain
validated certificates with very little extra code.

The component supports the Acme V2 protocol specified in RFC8555 as originally
implemented by Let's Encrypt to download free domain validated certificates.
You don't need to register with Let's Encrypt, but it only supplies domain
validated certificates so the domains for which you order certificates must
already be registered and have DNS pointing to a live server to satisfy most
challenges.

Earlier versions of this component also supported a custom REST API at CertCentre
AG in Germany for DigiCert commercial certificates, but this service is now
discontinued and support removed from the component. Digicert themselves now
use ACME for commercial certificates, but this has not yet been tested with ICS.

The TSslX509Certs component includes a database of certificate orders and pending
challenges, allowing certificates to be re-ordered and the supplier periodically
checked to see if a challenge has been successful when the X509 certificate can
be automatically downloaded and installed.  Events are generated upon completion
or failure, allowing the application to inform the user (by email) of certificate
ordering progress.

ICS includes a sample OverbyteIcsX509CertsTst that is effectively an X509
certificate management and ordering tool that can be used standalone to order
certificates from Let's Encrypt and Google Trust Services for testing or
commercial use, including wild card certificates provided you run your own
Windows DNS Server.  These certificates can be used automatically by most
non-ICS servers such as Windows IIS or Apache.



TSslX509Certs Acme Supplier Accounts
------------------------------------
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


TSslX509Certs Work and Certificate Files
----------------------------------------

When ordering a new certificate, temporary files may be created in the account
directory, the new certificate private key and certificate request.  Once the
order is accepted and an order number is available, these files are renamed to
include the order number for historic purposes, and when the order is completed
a second copy of all files is saved without the order number for final distribution
to the web server, and optionally copied to the web server using a UNC file share.
Note the files without order numbers are always automatically overwritten by new
orders.  So an Let's Encrypt order for the domain test3.telecom-tariffs.co.uk
will generally find the following files upon completion:

ics-control.db
AcmePrivateKey.pem
AcmePublicKey.pem
LE-SA-v1.2-November-15-2017.pdf
LE-5860824-test3_telecom-tariffs_co_uk-bundle.pem
LE-5860824-test3_telecom-tariffs_co_uk-certonly.pem
LE-5860824-test3_telecom-tariffs_co_uk-inters.pem
LE-5860824-test3_telecom-tariffs_co_uk-privatekey.pem
LE-5860824-test3_telecom-tariffs_co_uk-request.pem
LE-5860824-test3_telecom-tariffs_co_uk.pfx
test3_telecom-tariffs_co_uk-bundle.pem
test3_telecom-tariffs_co_uk-certonly.pem
test3_telecom-tariffs_co_uk-inters.pem
test3_telecom-tariffs_co_uk-privatekey.pem
test3_telecom-tariffs_co_uk-request.pem
test3_telecom-tariffs_co_uk.pfx

There is a PEM certificate signing request (CSR) file, separate PEM files for the
private key, domain certificate and intermediate certificates, then a combined PEM
bundle  with the certificates and private key, and a PKCS12 PFX file which is a
similar bundle that Windows certificate store accepts.  The certificate private
key files (and bundles) may be optionally password protected, the PFX file always
has a password since Windows requires that, it will be 'password' if not otherwise
specified. Note AcmePrivateKey is unprotected.  A wildcard order for
*.telecom-tariffs.co.uk will have a file name x_telecom-tariffs_co_uk since * can
not be used in file names.  Until the order number is available, the file name
will be LE-work, or CC-work for CertCentre.   There are component options to
ignore some of these files, if not needed.


TSslX509Certs Supplier Order Database
-------------------------------------

For each account there is a database file ics-control.db containing information
about the account. certificate orders and pending challenges.  This is a simple
INI file, and is generally updated only by the TSslX509Certs component.

There is an [account] section or record with general information about the
supplier, logging, next order sequence number, ACME account private key, etc.

There are then multiple [domain-mydomain] sections or records for each X509 order
placed where my-domain is the Common Name of the certificate, and then one
section or record for each Subject Alternate Name on the certificate
[san-mydomain=mysan] including the Common Name which are used for domain
validation. These records are updated as the order progresses and may be
checked afterwards to see the main files created and certificate details.  The
record may be used to re-order a certificate, but not on the same day a
certificate was downloaded to prevent wild repeated orders.

When an order has been placed, a temporary section [challenge-mysan] is created
for each SAN on the certificate to keep track of challenge progress, effectively
a queue of waiting challenges.  This queue is checked every 30 seconds and
the supplier contacted to see if the challenge has completed or failed, the
section is then removed with the main domain and san records updated.

The sample OverbyteIcsX509CertsTst has tabs 'Supplier Orders',  'Cert Domain',
'Cert Admin' and 'Supplier New Order' that show all existing orders and allow
new orders to be started.


TSslX509Certs Challenges
------------------------

To authenticate Domain Validated X509 certificates, the TSslX509Certs component
offers various challenge methods, variously used by different suppliers, products
and ICS components.  Challenges work by the supplier generating a short random
phrase which must become accessible on the public internet using the domains for
which a certificate is being ordered to prove that domain is controlled by whoever
placed the order.

ChallFileUNC  - File - Web Server - UNC: copies a small file into the .well-known
                directory of the server using a UNC path, may be any type of web
                server on the same or remote PC.  Once the challenge is done,
                the issued X509 certificate may be copied to that server.
                Requires a web server listening on port 80 and the domain being
                validated and path /.well-known/.  If a certificate is being
                ordered with two or more Alternate Subject Names, separate
                challenges are required for each separate domain name.  Wild card
                certificates are not supported.

ChallFileFtp  - File - Web Server - FTP: similar to ChallFileUNC, but the
                application needs extra code to copy the file using FTP.

ChallFileApp  - File - App Web Server: similar to ChallFileUNC, but handles the
                challenge file virtually without creating any files using a
                little code in the web server onWellKnownDir event to call an
                function in TSslX509Certs where the path is checked and a virtual
                file returned with the challenge data.  Currently supported by
                the SslHttpServer, TSslHttpAppSrv, TIcsProxy and TIcsHttpProxy
                components, see SslHttpAppSrv1WellKnownDir in the sample
                OverbyteIcsSslMultiWebServ1.pas for an example.

ChallFileSrv  - File - Local Web Server: similar to ChallFileApp, but uses a
                local web server TSimpleWebSrv listening on port 80 and the
                domain being validated, provided no other web server is using
                the same port and IPv4 and IPv6 addresses.  This is used by the
                sample OverbyteIcsX509CertsTst to order certificates separately
                without needing a web server, and by the TSslFtpServer and other
                components which don't usually listen on port 80.  The local web
                server is only run while waiting for the challenge to be accessed
                by the supplier, usually about a minute, but can not conflict
                with any other web server on the same IP address.

ChallDnsAuto  - DNS - Automatic: the challenge comprises a TXT record in the
                Domain Name Server Forward Lookup Zone for the domain.  Currently,
                this requires application code to access the DNS server which is
                in the sample OverbyteIcsX509CertsTst.  It uses WMI to access a
                Windows 2012 or later public DNS Server on the same PC.  Future
                releases could support Cloudflare DNS via a REST API, and maybe
                other cloud providers.  The benefit of using DNS is no conflicts
                with web servers, and ordering wild card certificates like
                *.ftptest.org that work with any sub-domains to avoid ordering
                certificates for each.  A certificate may have multiple wild card
                names, such as *.ftptest.org.uk and *.ftptest.co.uk.

ChallDnsAcnt  - DNS Account- Automatic: similar to ChallDnsAuto, but adds an
                account hash before _acme-challenge.domain. This challenge is
                Acme account specific, so different accounts can get certificates
                for the same domain name allowing redundancy.  Not tested yet
                since not supported by any suppliers.

ChallDnsMan   - DNS - Manual: similar to ChallDnsAuto, but just calls an event
                in the application which finds an alternate method of updating
                the Domain Name Server, supported in OverbyteIcsX509CertsTst
                which allows DNS to be updated manually before starting the
                actual certificate order.

ChallAlpnApp  - TLS-ALPN - App Web: this challenge uses the normal SSL port 443
                to avoid needing a non-SSL web server running on port 80. A
                special SSL/TLS certificate is created for the domain containing
                the challenge phrase which is returned instead of the normal
                certificate when the SSL client hello includes a special ALPN.
                This needs one line in the onClientConnect event to call a
                function in TSslX509Certs where the certificate is created.
                See the sample OverbyteIcsSslMultiWebServ1.pas for an example.
                Only catch with this challenge is requesting the first certificate
                for a new domain, the server won't start without a certificate,
                so the TWSocketServer component automatically creates an ICS
                signed certificate so the service can start, which is ideally
                replaced a few seconds later when the order completes.

ChallAlpnSrv  - TLS-ALPN - Local Web: similar to ChallAlpnApp, but uses a local
                web server TSimpleWebSrv listening on port 443 and the domain
                being validated, provided no other web server is using the same
                port and address, see comments about ChallFileSrv above.

ChallAlpnUNC  - TLS-ALPN - Web UNC: similar to ChallAlpnApp, but copies the
                special SSL certificate to another server that is responsible
                for implementing the ALPN part of the process, don't know of
                such a web server.

ChallEmail    - Email Manually:  calls an event in the application, that could
                send an email automatically, was supported for CertCentre.

For all the automated challenges above, the certificate order process involves
first testing the challenge with locally generated data to ensure the servers
are responding correctly from the public internet, then getting the real
challenge data from the supplier and again checking it can be accessed from the
public internet, before asking the supplier to start testing the challenges.
Let's Encrypt and Google now test challenges several times from servers in
different countries networks to avoid DNS spoofing.  In practice this all
takes place within seconds.

Beware that within a few seconds of a certificate order being completed, hackers
will start making intrusion attempts on the server domain name, typically looking
for PHP pages used to administer popular web servers, which ICS applications
will ignore.  This happens because all SSL certificate appear in public
transparency logs, and the hackers watch these logs.


TSslX509Certs IssueState
-------------------------

The component keeps the IssueState for each order and pending challenge reflecting
the order progress, saved in the database as type TIssueState. Several of these
states relate directly to the supplier Acme order status. The states are as follows:

IssStateNone       - Not started, previous order completed or new order failed
                     and needs to be restarted.  Acme supplier status: Invalid.

IssStateChecked    - Basic local checks completed, chosen domain challenge allowed,
                     such as copying a test file to .WellKnown directory or creating
                     an TlsApln certificate, starting the local web server if needed,
                     and then accessing the file or certificate by domain name from
                     the public internet or checking a domain name server can be
                     reached.  Note the Acme Supplier is not accessed at this stage.

IssStateChallgReq  - The order process has started and the supplier has been given
                     a list of domain names and maybe IP addresses to be included
                     in the certificate, and the certificate profile and expiry
                     details, depending on supplier.  If acceptable, an order
                     number is created and a URL for each domain which contains
                     one or more domain challenges. The component selects the
                     appropriate challenge according to the chosen method and will
                     prepare some challenges, others may need to done manually
                     or by the application such as DNS.  The challenges should
                     remain valid for up to a week, the expiry date is shown
                     on the Supplier Order tab. Acme supplier status: Pending.

IssStateChallgWaitTest - Does not appear to be used at present.

IssStateChallgTest - Challenges have been tested locally as ready for checking by
                     supplier, .WellKnown and TlsApln have been accessed from the
                     public internet or the DNS returns the correct TXT record.
                     The supplier order is ready to start. Acme supplier status:
                     Pending.

IssStateChallgPend - The order process has started and the supplier has been asked
                     to check the order domain challenges.  Waiting for a response
                     from the certificate supplier to the challenges, may happen
                     within 15 seconds, but each challenge is accessed from multiple
                     servers around the world so may take longer.  DNS challenges
                     sometimes take longer to propagate to secondary DNS servers.
                     Acme supplier status: Pending.

IssStateChallgOK   - All domain challenges have been passed by the supplier who is
                     now ready to accept a certificate supply request (CSR) and
                     issue the CA signed certificate. Acme supplier status: Ready.

IssStateFinalPend  - All domain challenges have been passed by the supplier who is
                     now ready to accept a certificate supply request (CSR) and
                     issue the CA signed certificate. Acme supplier status: Ready.

ssStateIssuePend   - The order has been accepted and finalised, but there is now
                     a short delay while the new certificate is issued, perhaps a
                     queue is the supplier is busy. Acme supplier status: Processing.

IssStateIssued     - The new certificate has been issued and is ready to be
                     collected by the component.  This normally happens
                     automatically. Acme supplier status: Valid.

IssStateCollect    - The new certificate has been collected OK, and all the
                     various certificate files, private keys, etc, have been
                     created and copied to the specified directories.  The
                     certificate can be collected again if something failed
                     for a few days, usually. Acme supplier status: Valid.

IssStateFinished   - The new certificate has been collected OK and the supplier
                     has finished with this order, so it can not be collected
                     again. Acme supplier status: Invalid.

IssStateCancel     - Cancelled order, perhaps revoked certificate, so can not
                     collect certificate again.


TSslX509Certs Sample Application
--------------------------------

There is a application Samples\demos-delphi-vcl\OverbyteIcsX509CertsTst.dpr
that illustrates all the functionality of the TSslX509Certs component, allowing
certificates to be ordered and collected by clicking a few buttons.   The
sample also shows all certificates ordered by ICS components and saved in the
supplier account databases and allows them to be re-ordered.


Tab: Acme Suppliers
-------------------

The 'Acme Suppliers' tab shows all the supplier accounts created with their
details, and which allows new accounts to be created or old ones deleted.  If
upgrading from ICS 9.4 or earlier, use the 'Import Old Account' button to
import the old Let's Encrypt account into a new supplier 'AcmeLetsEncrypt-Old'.

A supplier account must be opened by double clicking on the list or clicking
Open Supplier Account before orders may be viewed.  There is an inactivity
Account Close Timeout on the Account Common tab, that closes accounts in
case applications attempt to access them, since there is currently no account
locking, defaults to five minutes.

To create a new Acme Supplier, select the Acme Supplier Name from the list,
a default Account Title and Account Directory will be filled but can be changed
if required.  Most suppliers offers live and staging servers, the latter is
best for testing since it issue certificates signed by a fake CA and there are
fewer rate limits than the live server which may get blocked if you make
too many mistakes during ordering. When 'Save New Account' is clicked, the
Supplier Details tab will appear for more supplier information.


Tab: Supplier Details
---------------------

The 'Supplier Details' tab specifies the type of private and public keys
for the account, Elliptic Curve secp256 is smallest so best. These keys
are used to authenticate the account to the supplier, using a Json Web Key
created from the private key to sign each POST request with a Json Web
Signature, no user name is needed.

HTTP proxy URL is if the server is behind a NAT firewall so does not have a
public address, the URL format is 'http://host:port' where :port is optional.

Let's Encrypt does not require any email or external accounting information,
but these are needed for most other suppliers, taken from the console where
you created an ACME account, see details about different suppliers earlier.

Socket Family is generally Any, unless the account should only be accessed
over IPv4 or IPv6 specifically.  The Account Local Web Server is only needed
if the sample uses the ChallFileSrv or ChallAlpnSrv challenges to locally
order certificates without using a separate web server, for domains whose
address is this server.  Select the server IP addresses, they may be public
addresses or LAN if there is a NAT router forwarding an external IP to this
PC, as is often done for development and testing. If the domain has both
IPv4 (A) and IPv6 (AAAA) records, both family IP addresses must be specified,
since they are both tested.  The local web server is only started when order
processing starts and closed immediately after, but will not start if any
other server is using the same IP addresses.

There are various logging options, to keep track of activity and for
diagnostics when things don't work as expected, if the Log Directory is
not blank.  There are several levels of debug logging from just connections,
through SSL negotiations, then HTTP headers and content, also Json logging for
protocol errors (or changes).

Click Save Supplier Details and the component will attempt to create an
account or check it already exists.  The account number will appear on
the tab, but is not really used for anything.


Tab: Accounts Common
--------------------

The Accounts Common tab contains information common to all accounts on the
PC, mostly specific to this sample.

Log Directory if not blank is where the sample will write a daily log file
of all activity in the sample, as shown in the log window.  This directory
does not apply to other applications using the component, they must
implement their own logging.  Account Close Timeout is how long the sample
keeps an account open for, default five minutes.  DNS Challenge can be
selected as Local Windows DNS Server or Cloudflare DNS Server, but the
latter is not yet supported, this applies when usinng the DNS Automatic
challenges that only work if this server has a Windows DNS Server hosting
the domain records for which an order is being placed, which will be
accessed using WMI functions.

Private Key File Encryption specifies the type of encyption and password
for any private key files generated, nearly always AES256.  Note the
password is not encrpyted in the database files.  It is primarily to
allow private keys to be imported into Windows Store.  Default password
is 'password'.


Tab: Supplier Orders - View Orders
----------------------------------

Once an old Suppler account is opened, the 'Supplier Orders' tab shows all
the orders created for that account, and has buttons to start edit old
orders to review certificates.  To start a new local order, skip this tab
and go straight to the Cert Domain tab.

The order lists shows the Common Name, Order Date, Issue State,  Order Id,
Issued Date, Expiry Date, Renew From Date, Challenge Type, Profile and
Subject Alternate Names. Clicking on an order row shows further details
with certificate dates and challenge details, maybe error for failures.

Orders for local server challenges can be started from this tab, old
orders deleted or revoked, or old orders opened for limited editing.


Tab: Cert Domain
----------------

A new local order can be created by opening a Supplier Account, and selecting
the 'Cert Domain' tab. Specify the Certificate Domain Common Name and any
Subject Alternate Names, the sample application will add the Common Name to
the SAN list if not done manually. If Domain Challenge is for UNC file, set
the Web Server UNC HTTP .Well-Known Directory' for the Common Name, and
optionally for each SAN if different.

If the final certificates are to be copied to the web server, set the Web
Server UNC Public Certificates Directory.  There are various Output
Certificate Formats that may be ticked or unticked to reduce the number
of unneeded files, Separate PEM Files, PEM Bundle File, PKCS12/PFX Bundle
File, PKCS7 Bundle File and CSR PEM File.

Windows Cert Store means the final certificate will be installed into the
specified Windows Certificate Store for use by IIS web servers or other
servers. The two stores are Current User or Local Machine Store, the latter
needs Admin Rights for the sample if used for local orders, the former is
not available for most Windows Services.


Tab: Cert Admin
---------------

The 'Cert Admin' tab contains further information needed for a new certificate
order.  All certificates need to specify a Private Key Type and Size depending
on security requirements, RSA2048 used to be common but Elliptic Curve secp256
is smaller and more common now. Signature Digest Type usually SHA256. Serial
Number Type is only used for Own CA orders, and is either random or sequential
(stored in the database).

Most of the other admin fields, names, addresses, email, phone nummber, etc
are for commercial certificates only, not domain validated certificates, and
may be ignored.


Tab: Supplier New Order - Settings
----------------------------------

Once the Cert Domain and Cert Admin tabs have been completed for a new order,
click on the 'Supplier New Order' tab.  Specify the 'Domain Challenge Method',
details of which were discussed above but are: File - Web Server UNC (external),
File - Web Server FTP (manual), File - Local Web Server (built in), File - App
Web Server (ICS server), DNS Automatic, DNS Account Automatic,  DNS manual,
Email Manually (not supported), TLS-ALPN - Web UNC (external), TLS-ALPN -
Local Web (built-in) TLS-ALPN - App Web (ICS server).

Certificate CSR Origin specifies whether the component should create a
new private key and CSR for a new order, or use files previously created, in
which case both should be specified and many properties will be ignored.  Click
the 'Check CSR' button to read the files and check they contain the correct
domain and the key matches.  Normally used for commercial orders only.

Automatic Order Completion being ticked means the component will check every
few seconds to see if an order is ready for collection and finish it
automatically.

Cert Profile or Type is currently for Let's Encrypt only and customises the
certificate.  'Classic' is the original 90 days expiry certificate, 'tlsserver'
is a simpler certificate missing the common name which is always duplicated
in the list of Alternate Subject Names. 'Shortlived' is a 160 hour (6.6 days)
expiring certificate that may also include IP addresses, currently only from
the test staging server, due for public use in late 2025.  There is also a
tlsclient profile but this disappears in early 2026.  The profiles also
change order validity periods, classic authorisations remains valid for seven
days but that is reduced to one hour new newer profile, order lifetime used
to be seven days, but is now eight hours.

Certificate Period (days) is currently for Google certificates only, instead
of the Profile used by Let's Encrypt, to specify the expiry period, anywhere
from three to 90 days at present, but the maximum will reduce over the next
few years. Very useful to allow testing short lived renewals today.

Once the order settings are specified, click the 'Save Order' button to
update the account database.  The 'Check Order' button may be clicked to
locally check the order, see below.


Tab: Supplier New Order - Local Order
-------------------------------------

The 'Supplier New Order' tab has a series of buttons labelled with steps 1 to
7 that allow the sample to order a certificate using the Local Web Server
specified for the acccount, with Local Web File or ALPN, or DNS Automatic
challenges (if the server has a public Windows Domain Name Server).  Local
orders may also be started from the  Supplier Order tab, but some of these
seven steps are combined so there only five buttons.  Buttons are greyed and
unresponsive until prior order steps are compeleted, and may by bypassed
if Automatic Order Completion is selected with the buttons being enabled in
sequence as the order progresses, often completed in less than one minute.

Clicking 'Check Order (1)' which will check the challenge method is valid for
a local order by the sample, will then create local test challenges for all
domains specified and make HTTP requests to those domains to ensure they are
accessible from the public internet.

Generally, the sample will be run on a PC or server with public internet
addresses.  But for testing, the PC or server might only have LAN IP
addresses, with a NAT firewall forwarding public request to the LAN IPs.
In this case, it may be necessary to use a public HTTP proxy server to
access the public IP addresses, set-up on the Supplier Details tab.

If the local challenges are not accessible, there is no point in the order
being placed since it fail.  If the domains are accessible, the Order Issue
State changes to Checked, and the order can be started.

Clicking 'Start Order (2)' will to start the order process with the supplier.
They are asked to issue challenges for each of the domain names requested,
which the component then prepares and writes to queue records in the database.
As discussed above, challenges may include writing server files, creating
special SSL certificates or adding records to a Domain Name Server.
Previously satisfied challenges remain valid for a period varying from eight
hours to 30 days (for classic certificates) so the supplier may report the
challenges already complete at the this stage.

Once the challenges are ready, Acme Order State becomes Pending, and the
Order Issue State changes to ChallgReq.

Clicking 'Test Challenges (3)' starts testing the supplier challenges, to
make sure the domains, files, etc are available from the public internet
correctly, effectively repeating 'Check Order' but with real challenges.
Once successful the Order Issue State changes to ChallgTest.

Clicking 'Start Challenges (4)' asks the supplier to test the challenges,
which usually takes from 15 seconds to a minute, unless they are very busy.
There are usually several challenge tests from different networks in
different countries.  The Order Issue State will be updated to ChallgPend.
For HTTP challenges, the log window will display activity for the local
web server, which should be receiving and responding to multiple page
requests.

If Automatic Order Completion is enabled, the component will check the
Order Status (next button) for successful or failed challenges every five
seconds while the sample application is running, waiting for the Acme
Order Status to change from Pending to Ready, then updating Issue Status
to ChallgOK when that happens.

Clicking 'Order Status (5)' makes a request to the supplier to check the
Acme Order Status which will change from Pending to Ready once the challenges
for all domains have been successfully tested.  The Order Issue State then
changes to FinalPend.  If any challenges fail, the Order Issue State changes
to None and the order must be restarted.   The Supplier Orders tab should
display the reason for failure, usually just unable to acccess the public
server.

Clicking 'Finalize Order (6)' is only possible once all challenges are
checked by the supplier and the Order Issue State is ChallgOK or FinalPend.
The component creates a new private key and certificate signing request
using the domain details and sends a request to supplier to finalize the
order.  The supplier then matches the CSR against approved domain challenge
and commences issue of a new certificate, which may take a few seconds. The
Acme Order status changes to Processing and Order Issue State to IssuePend.
Clicking 'Order Status (5)' again will check the Acme Order Status which
changhes to Valid once the certificate is issued, with Order Issue State
to Issued. If Automatic Order Completion is enabled, this happens
automatically.

Clicking 'Collect Certificate (7)' is only possible once Order Issue State
is Issued.  The new SSL/TLS certificate is downloaded with any intermediate
certificates needed to validate the trust chain.  The component then creates
several files including the order number containing certificate, private
keys, bundles, etc, depending on the 'Output Certificate Format' settings
on the Cert Domain tab.  Finally the component runs a check to validate
the certificate chain, and reports all the details in the log. If validation
passes, all the files are saved a second time without the order number, as
detailed above and the Order Issue State updated to Collected.  If a Web
Server UNC Public Certificate Directory has been specified, the certificates
will be copied to the servers.

Finally, the component will ask the supplier for Renewal Information for the
certificate.  This specifies how many days before expiry a certificate should
be renewed, and how often these dates should be rechecked to see if the
certificate needs immediate renewal due to being revoked. These dates are
reported on the Supplier Orders tab.  Recheck is usually between six and 24
hours and is done automatically by ICS servers.

Beware the account database is not designed to shared between multiple
applications running at the same time.  So it is better for each application
that will order certificates to use a separate Acme account and directory,
and only use the sample application to briefly check orders.

Tab: Supplier Orders - Start or Revoke Orders
---------------------------------------------

Back to the Supplier Order tab, there are various buttons, some of which
match the supplier New Order tab and do the same thing, others combine tasks.

Check Order - if the order use challenges FileApp or AlpnApp for a server,
just enables the Start Order button.  For a local order, does as it
suggests, checking local challenges.

Start Order - if the order use challenges FileApp or AlpnApp for a server,
a flag is set in the account database that will be checked next time the
server check renewal information, and will start an immediate order,
may take several hours since the database is not checked regularly.  For
a local order, starts the order, gets and checks the challenges, then
asks the supplier to check the challenges.  If Automatic Order Completion
is enabled, Order status is checked every five secondss and everything
just happens until the order is completed.

Order Status - as above.

Finalise Order - as above.

Collect Order - as above.

Cancel Order - allows an order to be stopped part way through the process.

Revoke Certificate - if a certificate has been issued but should no longer
be trustedi, it may be revoked.  Revoking an order should generally only be
done if the certificate is in public use and the private key has been
compromised.  Revoke means the certificate will be added to supplier CRL
databases which may be checked by browsers to prevent compromised
certificates being trusted until expiry. The reduced issue period of
certificates is partly to avoid then needing to be revoked.

Remove Order - clears an old order from the supplier database but does not
revoke the certificate.

Reset Order - sometimes challenge failure can mean the order gets stuck
and not complete or do anything useful, so this button resets the Issue
State to none so it can be restarted.

Renewal Information - works for all unexpired collected or finished
orders. Ask the supplier for Renewal Information for the certificate.
This specifies how many days before expiry a certificate should be
renewed, and how often these dates should be rechecked to see if the
certificate needs immediate renewal due to being revoked.

Edit Order - loads the selected order into Cert Domain, Cert Admin
and Supplier New Order tabs, allowing some settings to be changed
and saved by the 'Save Order' button on the New Order tab.  While
this works for local orders, those created by servers may change
the order information according to the IcsHosts and server settings.

List Challenges - for an order in progress, will log the state of
challenges being tested.

Redistribute - for a collected order, redistributes the certificate
files according to the 'Output Certificate Format' settings.


Tab: Own CA
------------------- -

For internal network use and testing, it is possible to issue your own
SSL/TLS certificates for devices that are not accessible from the public
internet.  These can be self signed certificates, but to try and avoid
horrible browser warnings it is better to issue certificates signed by
trusted root CA certificate that is installed in the Windows Store (and
maybe other stores) as a trusted root on each device that will access
servers running certificates issued by the CA.

ICS now has it's own SSL root certificate 'ICS Root CA' and two intermediates, 'ICS
Intermediate' and 'ICS Intermediate Short', the last of which includes a private key
so can be used to automatically sign new certificates by ICS server applications,
rather than just self signed certificates as before. If the 'ICS Root CA' certificate
is installed in the Window Store and browser stores, it should stop certificate
warnings appearing. ICS applications automatically trust the ICS root, so will give
no warnings. The short intermediate has a maximum 200 day expiry, so new versions will
be issued regularly. There is a single function CreateSelfSignCertEx that creates
signed certificates, and another IcsInstallIcsRoot that installs the ICS root into
the Windows Store, so easy to use. It is possible to replace the ICS root with your
own private root certificate and have servers create their own certificates against
that root, for internal networks.

The ICS SSL root certificate is loaded automatically with the SslRootCAStore.Initialise
function that loads the CA Root Store, and with the define OpenSSL_AutoLoad_CA_Bundle,
to verify any ICS issued certificates.  The SslRootCAStore.Initialise functions also
tries to load the file C:\ProgramData\ICS-OpenSSL\ExtraRootCABundle.pem which is an
optional private root bundle that can be used for private customer or developer root
CA certificates, in PEM format.

Alternatively, you can create your own certificate authority and intermediate
to sign your own certificates.

The sample application has a button that will create self signed SSL certificates
with a 'CA Cert' check box so it can sign other certificates., but the
OverbyteIcsPemTool sample has more control over fields. You should create a new
private key and self signed certificate with your organisation's common name,
ie Magenta Development CA.  Rather than signing certificates directly with the
new trusted CA, it is better create an intermediate CA signed by the trusted CA,
allowing variations of key types and digests, ie Magenta Intermediate EC CA1.
The intermediate CA certificate does not need to be installed on client devices,
only the trusted CA.

On the Own CA tab, the Certificate Directory should be specified, note the
account database for Own CA certificates currently does not store certificate
details so they can not be ordered automatically by TWSocketServer. Then specify
the CA Certificate or Bundle File and CA Private Key File (if not in bundle) and
click the 'Load CA'  button check and load it, with Issued to and Issued by
being shown.

To issue your own signed SSL certificates, the usual settings on the Cert Domain
and Cert Admin tabs should be completed, and the 'CA Signed Cert' button pressed.
The component will then create a new private key and CSR files, then a new
certificate signed by the Own CA, in a process otherwise identical to collecting
an ACME certificate, using a sequential order number, with files saved with and
without the order number and optionally distributed to the web server.  Note
this sample sets common certificate extensions only, for more control you
should create your own CSR using OverbyteIcsPemTool or it's functions.

Each certificate signed by the Own CA is logged to the index.txt database file
in the Certificate Directory, in OpenSSL CA command format, which is tab
delimited with status, expiry date, SHA1 fingerprint, file name, subject and
subject alternate names. This file could be updated if the certificate is
revoked and used to support Certificate Revocation Lists (not implemented).


TSslX509Certs: Main Methods for Accounts and Orders
---------------------------------------------------

The TSslX509Certs component provides many methods for dealing with accounts
and the database, which relate closely to the buttons on the Orders tab.
These functions are used by the TSslWSocketServer methods DoCheckOrderCert and
DoProcOrderCert which are how severs using IcsHosts order certificates.

function DBSuppliersRead: Boolean;

Opens and read the Acme Supplier Database C:\ProgramData\ICS-Acme-Accounts\ics-acme-accounts.db
into an array of AcmeSuppRecs records.


function DBFindAccSupp(const ATitle: String): Integer;

Searches for a Supplier Title in the AcmeSuppRecs records, returning the index if
found, or -1.  The application should set the component properties:
X509Cert.CertDirWork := AcmeSuppRecs[Idx].ASuppDir;
X509Cert.AcmeSupplier := AcmeSuppRecs[Idx].ASupplier;


function SetAcmeAccount(CreateNew: Boolean = False): Boolean;

Opens the supplier account as in CertDirWork, optionally creating a new
account for AcmeSupplier if the directory does not exist or is empty.
For a new account, several properties can be set: DebugLevel, DomWebSrvIP,
LogJson, LogPkeys, SupplierEmail, SupplierProto, SupplierServer.

Once the account is opened, the property DomainItems returns an array of
TDomainItems containing the main details of each domain record in the database,
and an event is triggered whenever this changes.

The function will fail if ics-control.db can not be found or the working
directory mismatches the database. Call ClearAccount before using this.


function CloseAccount: Boolean;

Close the account, if open.


function DBReadCNDomain(const CNDomain: String): Boolean;

For an open supplier account, reads all the properties for an order with
the Common Name from the account database, if found, including one or more
subject alternate names.

The function will fail if the domain has not been saved in the database.
Call ClearCertOrder before using this.


function DBWriteCNDomain: Boolean;

Save or update properties for CertCommonName certificate to the account database.
There are many possible properties, all of which are illustrated in the sample
application.  The main properties are: CertCommonName, AcmeSupplier,
SuppCertChallenge, CertPKeyType, CertSignDigest, CertCsrOrigin, CertSerNumType,
CertOutFmts, DirWellKnown, DirPubWebCert, PrivKeyPassword, PrivKeyCipher,
CertSubAltNames, CertAcmeProfile and CertValidity (Google only).

Fails if account not opened, but does not check properties for validity.


function AcmeCheckSaveOrder(DomainCheck: Boolean = True; UpdateDB: Boolean = False): Boolean;

This is the main function to start a certificate order, used by the
'Check Order' buttons in the sample.  All required order properties must
have been read using DBReadCNDomain or set as properties, and will be saved
using DBWriteCNDomain if UpdateDB is set.  It checks the Common Name is
included in the SANs and adds it if not, then checks the challenge method
and SAN number are supported by the certificate product.

This function restarts the order process for an old order setting Order
Issue State changes to None, even if old challenges are still valid.
If DomainCheck is true, checks the domain is accessible from the public
internet for domain challenges.  If the domains are accessible, the
Order Issue State changes to Checked, and the order can be started.


function AcmeV2GetChallgs: Boolean;

This is the main function to place a certificate order with the supplier,
used by the 'Start Order' buttons in the sample. AcmeCheckSaveOrder must
have passed first.   The supplier provides challenges for each domain in
the order which the components prepares and the Order Issue State changes
to ChallgReq.


function AcmeV2TestChallgs: Boolean;

Once challenges have been acquired from the supplier, this function locally
tests them, called by the 'Test Challenges' button.  Once done OK, the Order
Issue State changes to ChallgTest.


function AcmeV2StartChallgs: Boolean;

Once the challenges have been tested, this function asks the supplier to
test them, called by the 'Start Challenges' button. Testing usually takes
from 15 seconds to a minute, sometimes a lot longer, and the Order Issue
State will be updated to ChallgPend. If Automatic Order Completion is
enabled, the component will check the Order Status for successful or
failed challenges every five seconds waiting for the Acme Order Status
to change from Pending to Ready, then updating Issue Status to ChallgOK
when that happens, and then calling AcmeV2OrderFinal and AcmeV2CollectCert.


function AcmeV2OrderStatus: Boolean;

Requests the Acme order status at any point during the order process,
but in particular when waiting for something to happen, called by
the five second timer that runs Automatic Order Completion to check
for status changes with the Order Issue State updated appropriately.
Called by the 'Order Status 'button'.


function AcmeV2OrderFinal(LogErrors: Boolean = True): Boolean;

Once the challenges have all been tested by the supplier, the order
can be finalised, called by the 'Finalize Order' button.  The component
creates a new private key and certificate signing request using the domain
details and sends a request to supplier to finalize the order.  The
supplier then matches the CSR against approved domain challenge and
commences issue of a new certificate, which may take a few seconds.
The Order Issue State becomes IssuePend. If Automatic Order Completion
is enabled, the component will wait for the certificate to be issued
by checking AcmeV2OrderStatus for Order Issue State Issued.


function AcmeV2CollectCert: Boolean;

Once the certificate is issued by the supplier, it may be collected,
called by the 'Collect Certificate' button or by Automatic Order
Completion'.  The new SSL/TLS certificate is downloaded with any
intermediate certificates needed to validate the trust chain.  The
component then creates several files depending on the 'Output
Certificate Format' settings, runs a check to validate the certificate
chain and the Order Issue State updated to Collected.  If a Web
Server UNC Public Certificate Directory has been specified, the
certificates will be copied to the servers.


function DBReadCNDomDates(const CNDomain: String): Boolean;

Similar to DBReadCNDomain but only reads sufficient properties to
check certificate renewal and whether the Acme supplier should
be contacted to update renewal information.


function CertRenewalDomain(const aDomain: String): Boolean;

Only available if a previously issued certificate is available, asks
the Acme supplier how many days before expiry the certificate should
be renewed.


function CertCancelDomain(const aDomain: String): Boolean;

For an order with issue state Collected or Installed, will cancel the order
with the supplier, which may result in a refund if done within a reasonable
period.


function CertRevokeDomain(const aDomain: String): Boolean;

For an order with issue state Collected or Installed, will cancel and revoke the
order with the supplier.


function CertRemoveDomain(const aDomain: String): Boolean;

Removes any order from the database, to stop further processing or re-use.


function CertRedistDomain(const aDomain: String): Boolean;

For an order with issue state Collected or Installed, copies the certificate
files to the web server again, if lost.



----------------------------------------------------------------

Updates:
May 22, 2018  - V8.54 - baseline
July 03, 2018 - V8.55 - don't load LoadCATrust in Create, it loads OpenSSL in IDE.
Oct 2, 2018   - V8.57 - Added database for domains and challenges (INI file).
                        Added challenges for local web server, FTP and DNS server.
                        Order AcmeV2 certificates with multiple SANs or wildcard.
                        Order CertCentre orders with multiple SANs.
                        Automatic order completion when challenge completed.
                        Added own CA to issue local certificates.
                        Use own CSR and PKey instead of creating them new.
                        Builds with FMX but not tested.
Nov 2, 2018   - V8.58 - Bug fixes and more documentation.
                        Descend components from TIcsWndControl not TComponent
Feb 6, 2019   - V8.60   Added SocketFamily property to allow both IPv4 and IPv6.
Apr 16, 2017  - V8.61   Certificate dates are in UTC not local time.
Aug 07, 2019  - V8.62   TDomainItem adds DDirWellKnown and DDirPubWebCert
                        Added literals for various types to assist apps.
                        Removed Acme V1 protocol support (withdrawn from Nov 2019)
                        AcmeV2 now supports POST-as-GET per RFC8555 for the final
                          ACME specification, GET alone being removed later in 2019.
                        Added Proxy URL support, might be needed for servers behind
                          NAT firewalls for public access.
                       CertCenter AlwaysOn is discontinued and removed.
                       Comodo is now called Sectigo, sometimes old name still used.
                       Moved BuildCertName to OverbyteIcsWSocketS.
                       Check can create certificate directories before order starts.
                       Added ChallFileApp and ChallAlpnApp which mean SocketServer
                         checks the challenge database in this unit using an event
                         rather than writing files.
                       Builds without USE_SSL
Nov 12, 2019 - V8.63 - OpenAccount will now create a new account correctly.
                       Better response for CertOrderDomain if order collected.
                       Changed challenge checking from every 30 seconds to 10 seconds
                         after a challenge started for faster completion.
                       Clear Acme nonce after errors so a fresh nonce is found.
                       Added AutoAccountClose property so account is closed after
                         order is completed or fails and AccountTimeOutMins to
                         close it anyway when idle.  This avoids potential hacking
                         attempts that often follow listing in SSL certificate
                         transparency logs immediately after issue.
                       Improved local web server and REST logging.
                       Added LastError to try and keep the last real order error.
                       Expire and remove challenges from the database after 24 hours
                         or a week for manual/email/dns.
May 18, 2020 - V8.64 - Added support for International Domain Names for Applications (IDNA),
                         i.e. using accents and unicode characters in domain names.
                       X509 certificates always have A-Lavels (Punycode ASCII) domain names,
                        never UTF8 or Unicode.   IDNs are converted back to Unicode
                       Fixed bug that stopped new orders after a successful one
                         saying no more today, due to date not being cleared.
                       Allow use of EC keys for ACME account, still don't work yet.
                       Added sanity check for private key type and check private
                          key is generated OK.
                       With automatic order completion don't report errors if the
                         challenges are not actually started, may take several minutes
                         for manual DNS updating.
                       The onChallengeDNS/Email/FTP events have an extra parameter
                         ChlgOK which the application should set once the challenge
                         have been set-up, so the component can stop if necessary.
                         Sorry, this requires application changes.
                       Now storing wildcard and host challenges separately, so they
                         can be used together for the same domain.
                       Fixed several literal typos, sorry.
                       Support tls-alpn-01 challenge for local web server on 443.
                       WebSrvHttp event to check http-01 ChallFileApp well-known
                         challenge without neding to save a file.
                       Avoid possible logging exception with StopDomSrv during destroy.
                       Added testing DNS challenges against public DNS servers.
                       Major restructure of ordering process so that challenges are
                         obtained and locally tested before the Acme order process
                         starts, Acme challenges are valid for seven days, so
                         this allows manual DNS challenges to be set-up and tested
                         later.
                       Implemented SelfSigned function to create certificate from
                         properties, optionally as a CA (for Own CA).
                       Implement AcmeV2OrderCancel to cancel order and revoke order,
                         former useful to remove old challenges (which remain valid
                         for a week or more) so fresh challenges can be tested.
Dec 09, 2020 - V8.65 - Using new SuperObject DateTime methods.
                       Better validation for remote certificate directories.
                       Better logging when waiting for challenges to complete.
                       Remove old challenges when starting new Acme order,
                         but leave fake DNS record.
                       Save local server IP address with domains, and check using
                         correct address, may be different for different orders.
                       Added extra IssStateChallgWaitTest.
                       Close local web server before collecting certificate to stop
                         hacking immediately certificate issued (and listed in
                         certificate transparency log).
                       Builds without AUTO_X509_CERTS, without some functionality.
                       Renamed Ics.Posix.Messages.pas to Ics.Posix.PXMessages.pas.
Mar 23, 2021 - V8.66 - Bad version.
                       When starting local web server, check server IP address still
                         exists or change to 0.0.0.0, may not work if multiple IPs exist.
                       Minor fixes to file challenges, thanks to Oliver Dahlmann.
Sep 22, 2021 - V8.67 - Improved error messages if testing challenge fails.
                       OpenSSL 3.0 makes several old ciphers and digests legacy so default
                         for encrypting PFX/P12 files is now PrivKeyEncAES256 with 3.0
                         unless the legacy DLL is loaded when still PrivKeyEncTripleDES
                         so older versions of Windows can load them.
                       Changed extraction of download PEM bundle so that main certificate
                         does not need to be first in file, log them all, ignore any
                         self signed root certificates.
                       If testing dns-01 challenge fails, rotate to next public server
                         and three retries (previously only happened on timeout).
                       When saving files with private keys, log encryption type used.
                       Another attempt as consistently formatted literals, fewer capitals.
                       Added more certificate output formats, OutFmtPwPem and OutFmtPwP12
                         specify whether to password PEM and P12/PFX private keys. Note
                         Windows always needs passworded P12/PRX files, while Apache web
                         server only accepts PEM files without a password.
                       Allow automatic installation of new certificates to the Windows
                         Certificate Store so they can be used by IIS web sites, by
                         setting output format to OutFmtWinStore.  Note application must
                         have administrator rights to do this.
                       Truncate certificate chain info to 1,000 bytes avoid overloading
                         database INI file.
                       Check new certificate bundles can actually be opened.
Dec 20, 2021 - V8.68 - Added property KeepOldCA set true to keep LA intermediate for
                         expired DST Root CA X3 root in bundles (for old Androids).
                         Keeping it may prevent some clients verifying the chain and
                         SslLabs testing gives a chain warning.
                       HttpRest now allows saving a file, so change Acme terms to do so.
Apr 05, 2022 - V8.69 - Builds on MacOS again, MsSslUtils is Windows only so don't attempt
                         to install certificate into Windows store on Mac.
                       Added unit OverbyteIcsSslHttpOAuth for stuff previously in HttpRest.
                       When revoking certificate, look for bundle file if single file missing.
Aug 23, 2022 - V8.70 - Create well known directory if missing for ALPN challenges.
                       Ignore ACME zero length error in AcmeGetRequest.
Jul 06, 2022 - V8.71 - Removed OAuth2 authentication code from the component and instead get
                         authentication token using OnX509OAToken event from main application.
                         OAuth2 is only needed for CertCentre, so this avoid linking a lot of
                         code for applications that only need Let's Encrypt.
                       Note CertCentre no longer offers Comondo/Sectigo certificates.
                       Using Int64 ticks.
                       Local web server now supports IPv6 addresses.
                       Added DomWebSrvIP2 second local web server IP so it can listen on
                         both IPv4 and IPv6 addresses for domains with both.  To use the
                         second address with existing saved domains in the database, they
                         must be deleted and resaved.
                       Simplified checking DNS challenges with sync method, but repeat
                         DNS query six times in case of missing response to new TXT
                         challenges, DNS servers seem to randomly ignore TXT records.
                       Better sanity check checking challenges after order.
                       Added MsCertLoc property to specify which Windows Certificate Store
                         to save certificate if OutFmtWinStore is specified, may be
                         MsLocMachine (default) or MsLocCurUser which does not need admin
                         rights.
Aug 08, 2023 V9.0  Updated version to major release 9.
Jan 27, 2024 V9.1  Make sure certificate extensions are set for server certificate before
                     creating certificate request so international domain name with accents
                     gets processed, got broken in June 2023 due to change in DoCertReqProps.
                   Added OverbyteIcsSslBase which now includes TX509Base and TX509List.
                   Validation now uses public IcsSslRootCAStore and ignores root bundle.
                   OwnCASign to sign our own certificates with OwnCA now creates
                     intermediates as well.
                   OwnCASign now adds signing certificate as intermediate to bundle.
                   Using TBytes Jose functions where possible.
Apr 10, 2024 V9.2  Added function CertResetDomain to reset an order state to None, if
                     the order process stalls or gets confused due to errors.
                   If AcmeV2StartChallgs fails because there are no pending challenges,
                     reset to order to None so it starts again next time and does not loop.
                   No warning message if local web server is using 0.0.0.0 or ::.
Aug 08, 2024 V9.3  Made all output file names available as properties, ie FileCertPem.
                   No longer prefer TripleDES for P12 files, too old.
                   Undefining MSCRYPT_Tools stops TSslX509Certs saving certifates to the
                     Windows Store.
Feb 10, 2025 V9.4  Updated Base64 encoding functions to IcsBase64 functions.
                   Removed Ics.Posix.PXMessages, not needed here.
                   Change 3DES cipher to AES256 if legacy provider is not loaded, really
                     cosmetic since the AES256 would be used anyway.
Sep 09, 2025 V9.5  Removed CertCentre Json ordering (lots of code), now integrated into
                     Digicert CertCentral which supports Acmev2.  Simplified component by
                     removing some common functions that called Acme or CertCentral since
                     no longer needed, see below.
                   Since Let's Encrypt introduced the Acme protocol, other suppliers have
                     added automated ordering using the same API, mostly with extra account
                     information for commercial certificates, so added several of these.
                   AcmeSupplier is possible supplier types TAcmeSupplier: AcmeLetsEncrypt,
                     AcmeLetsEncryptTest, AcmeZeroSSL, AcmeGoogle, AcmeGoogleTest,
                     AcmeDigicert, AcmeDigicertTest, AcmeSslcomRSA, AcmeSslcomECC.
                     Note most need an external account to be created before Acme can be used,
                     with AcmeEABKid and AcmeEABHmacKey specified to access the APIs. Most
                     suppliers offer API testing endpoints, with Test at the end. The status
                     of AcmeSupplier testing is detailed near the top of this unit.
                   ICS now has a central Acme database that contains a list of the Acme work
                     directories previously only referenced in separate IcsHosts INI files:
                      C:\ProgramData\ICS-Acme-Accounts\ics-acme-accounts.db
                   By default, new work directories will be in: C:\ProgramData\ICS-Acme-Accounts\
                   Added function AcmeV2RenewalInfo which for an existing certificate order,
                     finds when renewal is recommended, uses CertRenewalId built from certificate
                     Authority Key Id and serial number, calculates CertRenewDays from old
                     certificate expiry date, CertRenewNow=true if renew immediately, sets
                     CertRenewRetryDT which is when this request should be retried for fresh
                     information, for instance if the certificate is revoked, usually every six hours.
                   Added ClearAccount and ClearAccount to centralise resetting everything.
                   DBReadCNDomain no longer has the option to skip reading order properties
                     set manually which was used in AcmeCheckOrder.
                   AcmeCheckOrder renamed AcmeCheckSaveOrder, before calling it you must call
                     DBReadCNDomain and update any new properties for the order.
                   AcmeV2GetCert split into three functions, AcmeV2OrderStatus, AcmeV2OrderFinal
                      and AcmeV2CollectCert.
                   AcmeV2CheckChallg and ChallengeTimer are no longer used since the order process
                     no longer checks individual challenges are finished, AcmeV2OrderStatus
                     instead checks if all challenges finished.
                   CertCheckDomain and CertSaveDomain have gone, use AcmeCheckSaveOrder instead.
                   CertReadDomain has gone, use DBReadCNDomain instead.
                   CertGetChallgDomain has gone, use AcmeV2GetChallg.
                   CertOrderDomain has gone, use AcmeV2StartChallgs instead.
                   CertCollectDomain has gone, use AcmeV2OrderFinal and AcmeV2Collect instead.
                   Google allows certificate CertValidity expiry to be specified in days, always
                     part of the Acme spec, but Let's Encrypt did not support it.
                   Supporting asynchronous order finalization, part of Acme spec. If AcmeV2OrderFinal
                    returns 'processing' status set IssStateIssuePend, call AcmeV2OrderStatus
                     until IssStateIssued, then call AcmeV2CollectCert. Required by Google and
                     Let's Encrypt staging, but introduction by Let's Encrypt production delayed
                     in April 2023 by too few clients supporting it, including ICS!.
                   Implemented the onChallgRefresh event which is called when the Acme order
                     object status changes, usually after Acme methods are called, but also
                     while waiting for challenges or orders to complete.
                   Added new ChallengeDNSAcnt for the dns-account-01 challenge that adds an
                     account hash before _acme-challenge.domain, not tested yet since no
                     suppliers are using it yet.  This challenge is Acme account specific,
                     so different accounts can get certificates for the same domain name.
                   Added property CertRenewNow that if set true in the database using the GUI,
                     will override certificate expiry checking and cause an immediate new
                     certificate replacement order by in servers with IcsHosts the next time
                     RecheckSslCerts is called by the server, typically every two hours.
                   Allow IP addresses for certificate orders as well as host domain names,
                     currently only for Acme Testing with shortlived profile.
                     Only HTTP and APLN challenges, latter needs reversed SNI name ie
                     165.21.168.192.in-addr.arpa
                   Signing Acme requests with Elliptic Curve keys now correctly use IEEE P1363
                     digests so finally work properly, been looking for this since 2018.
                   Let's Encrypt ALPN-01 challenge TLS certificate now requires SAN name or
                     gives OID 2.5.29.17 is not present error.
Sep 18, 2025 V9.5  Added onGeoEvent that allows the application to look-up challenge web server
                     geo information like country and ASN for each new remote connection.
                   Call onChallgRefresh event couple more times, applications doing GEO blocking
                     should check IssueState for IssStateChallgTest/Pend/OK and skip blocking
                     since challenges are checked from several different countries and orders
                     will fail if too many can not connect.
                   If old certificate order dates get lost, get them from old certificate so
                      renew process does not stall.  More logging so we know why order is placed.
                   Failure to collect order like save file errors is fatal, so reset order.
Oct 15, 2025 V9.5  Fixed renewal information being found but reported as not being saved.
                   SupplierEmail is optional for Let's Encrypt, some suppliers may require it.
                   Added IsAccountOpen to simplify checking.
                   DBAddAccSupp adds a new supplier account, optionally creating the Acme
                     account as well, but may fail if not Let's Encrypt due to extra stuff
                     being needed.
                   Updated all documentation.



The docunentation earlier in this unit is repeated with screen shots at:

https://wiki.overbyte.eu/wiki/index.php/FAQ_Order_SSL_Certificates

Pending - IssStateChallgWaitTest does not seem to be used.
Pending - allow only one pending order at once, things may be confused if more than one order trying to collect at the sametime.
Pending - Add self signed and CA certs to database
}

{$IFNDEF ICS_INCLUDE_MODE}
unit Z.ICS9.OverbyteIcsSslX509Certs;
{$ENDIF}

{$I Include\Z.ICS9.OverbyteIcsDefs.inc}

{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$H+}             { Use long strings                    }
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

interface

{$IFDEF USE_SSL}

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Posix.Time,
    Z.ICS9.Ics.Posix.WinTypes,
{$ENDIF}
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    {$Ifdef Rtl_Namespaces}System.Types{$Else}Types{$Endif},           { V9.5 }
    {$IFDEF RTL_NAMESPACES}System.TypInfo{$ELSE}TypInfo{$ENDIF},
    Z.ICS9.OverbyteIcsSSLEAY, Z.ICS9.OverbyteIcsLIBEAY,
{$IFDEF FMX}
    Z.ICS9.Ics.Fmx.OverbyteIcsWndControl,
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocket,
    Z.ICS9.Ics.Fmx.OverbyteIcsWSocketS,
    Z.ICS9.Ics.Fmx.OverbyteIcsHttpProt,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslJose,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslHttpRest,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslHttpOAuth,  { V8.69 }
    Z.ICS9.Ics.Fmx.OverbyteIcsSslX509Utils,
    Z.ICS9.Ics.Fmx.OverbyteIcsSslBase,  { V9.1 TX509Base }
{$IFDEF MSCRYPT_Tools}
    Z.ICS9.Ics.Fmx.OverbyteIcsMsSslUtils,
{$ENDIF}
    Z.ICS9.Ics.Fmx.OverbyteIcsDnsQuery,
{$ELSE}
    Z.ICS9.OverbyteIcsWndControl,
    Z.ICS9.OverbyteIcsWSocket,
    Z.ICS9.OverbyteIcsWSocketS,
//    OverbyteIcsHttpProt,
    Z.ICS9.OverbyteIcsSslJose,
    Z.ICS9.OverbyteIcsSslHttpRest,
    Z.ICS9.OverbyteIcsSslHttpOAuth,  { V8.69 }
    Z.ICS9.OverbyteIcsSslX509Utils,
    Z.ICS9.OverbyteIcsSslBase,    { V9.1 TX509Base }
{$IFDEF MSCRYPT_Tools}
    Z.ICS9.OverbyteIcsMsSslUtils,
{$ENDIF}
    Z.ICS9.OverbyteIcsDnsQuery,    { V8.64 }
{$ENDIF FMX}
    Z.ICS9.OverbyteIcsTypes,
    Z.ICS9.OverbyteIcsIniFiles,
    Z.ICS9.OverbyteIcsUtils,
    Z.ICS9.OverbyteIcsUrl,
    Z.ICS9.OverbyteIcsMimeUtils,
    Z.ICS9.OverbyteIcsTicks64,
    Z.ICS9.OverbyteIcsSuperObject;

{ NOTE - these components only build with SSL, there is no non-SSL option }

const
    ComponentVersion = 'V9.5';  // used in user agent

 // file suffixes to build various file names
    FileSuffPKey     = '-privatekey.pem' ;
    FileSuffCSR      = '-request.pem' ;
    FileSuffCertPem  = '-certonly.pem' ;
    FileSuffInterPem = '-inters.pem' ;
    FileSuffBundPem  = '-bundle.pem' ;
    FileSuffBundP12  = '.pfx' ;
    FileSuffBundP7   = '.p7' ;
    FileIcsCntlDB    = 'ics-control.db';  // INI file for single Acme account
    FileCADB         = 'index.txt';       // Own CA serial number database
    FileBuypassInter = 'BPClass2CA5.pem'; // C:\ProgramData\ICS-OpenSSL\ICS-Certs\

    FileIcsAcmeAccDB = 'ics-acme-accounts.db';  { V9.5 INI file listing ics-control.db directories   }
    DirAcmeAcc       = 'ICS-Acme-Accounts\';     { V9.5 default for Acme account files,  C:\ProgramData\ICS-Acme-Accounts\ }

 // INI file section headers
    CntlDBAccount   = 'account' ;    // ie, [account]
    CntlDBDomainPre = 'domain-' ;    // ie, [domain-www.magsys.co.uk]
    CntlDBSANPre    = 'san-' ;       // ie, [san-www.magsys.co.uk=www.magsys.uk]
    CntlDBChallenge = 'challenge-' ; // ie, [challenge-wild-www.magsys.co.uk]

    DateMaskPacked = 'yyyymmdd"-"hhnnss' ;

    OneSecond: TDateTime = 1 / SecsPerDay ;            { V9.5 }
    OneMinute: TDateTime = 1 / (SecsPerDay / 60) ;
    OneHour: TDateTime = 1 / (SecsPerDay / (60 * 60)) ;

type

 // certificate serial number
    TSerNumType = (SerNumRandom, SerNumSequential);

 // issue state within the component
    TIssueState = (IssStateNone, IssStateChecked,              { V9.5 account not used }
       { V8.64 added ChallgReq and ChallgTest, V8.65 IssStateChallgWaitTest  }
          IssStateChallgReq, IssStateChallgWaitTest, IssStateChallgTest,
          IssStateChallgPend, IssStateChallgOK, IssStateFinalPend, { V9.5 added FinalPend }
          IssStateIssuePend, IssStateIssued, IssStateCollected,  { V9.5 added IssuePend, StateIssued }
          IssStateFinished, IssStateCancel);                     { V9.5 Install now Finished }

 // certificate CSR origin
    TCertCsrOrigin = (CsrOriginProps, CsrOriginFile);

 // certificate output formats
    TCertOutFmt = (OutFmtSep, OutFmtBudl, OutFmtP12, OutFmtP7, OutFmtReq,
                                   OutFmtPwPem, OutFmtPwP12, OutFmtWinStore);   { V8.67 added last three }
    TCertOutFmts = Set of TCertOutFmt;

 // domains information from database, display only
    TDomainItem = record
        DCommonName: String;
        DCertSANs: String;
        DProduct: String;
        DSuppOrderId: String;
        DIssueState: TIssueState;
        DSuppCertChallg: TChallengeType;
        DCertStartDT: TDateTime;
        DCertEndDT: TDateTime;
{$IFDEF MSCRYPT_Tools}
//        DMsCertLoc: TMsCertLocation; { V8.71 }
{$ENDIF}
        DIssueStartDT: TDateTime;     { V9.5 }
        DCertProfile: String;         { V9.5 }
        DCertRenewDays: Integer;     { V9.5 }
        DCertRenewNow: Boolean;       { V9.5 }
    end;
    TDomainItems = array of TDomainItem;

 // challenge item for a single domain, a certificate may have several domains
    TChallengeItem = record
        CDomain: String;      // domain being tested, might be CommonName or SAN
        CIdType: String;      // V9.5 dns or ip for CDomain
        CWildcard: Boolean;   // V8.64 need to distinguish wildcard DNS challenges
        CCommonName: String;  // common name for certificate, might be same as Domain
        CSanIdx: Integer;     // index into SubAltNames
        CSuppOrderId: String;
        CDirWellKnown: String;
        CDirPubWebCert: String;
        CWKFullName: String;    // challenge full file name
        CSupplierProto: TSupplierProto;
        CType: TChallengeType;
        CIssueState: TIssueState;
        CAuthzURL: String;      // Authorisation object URL or DNS Pointer (TXT or CNAME)
        ChallengeURL: String;  // challenge URL at supplier
        ChallgToken: String;   // random token from supplier
        CPage: String;         // .well-known/page URL for challenge or domain for DNS
        CResp: String;         // page token content for challenge
        CDNSValue: String;     // DNS record value
        CAcmeAlpnCert: String; // V8.62 full file name for acme-tls/1 challenge SSL certificate
        CAlpnSNI: String;      // V9.5 SNI expected for acme-tls/1 challenge, domain or in-addr.arpa reversed IP address
        CStartDT: TDateTime;
        CDoneDT: TDateTime;
        CExpireDT: TDateTime;   // V8.64 when challenges expire, usually one week
        CValidResult: String;   // challenge validation result or error
    end;
    TChallengeItems = array of TChallengeItem;

 // Acme URLs for specific commands
    AcmeActionDir = record
        Action: string ;
        URL: string ;
    end;

    TChallengeEvent = procedure (Sender: TObject; ChallengeItem:
                         TChallengeItem; var ChlgOK: Boolean) of object;   { V8.64 added ChlgOK }

// V8.71 event to get a new OAuth2 token
    TX509OATokenEvent = Procedure(var AccToken: String; var TokExpireDT: TDateTime) of object ;

// v9.5 Acme Suppliers List database
    TAcmeSuppRec = record
        ASuppTitle: String;
        ASupplier: TAcmeSupplier;
        ASuppDir: String;
    end;
    TAcmeSuppRecs = array of TAcmeSuppRec;

// V9.5 Acme order record domains, filled from order object
    TAcmeOrderDomain = record
        IdDns: String;
        IdValue: String;
        AuthzURL: string;
    end;
    TAcmeOrderDomains = array of TAcmeOrderDomain;

(*
{ Acme V2
  "Qa5SoBHy3FM": "https://community.letsencrypt.org/t/adding-random-entries-to-the-directory/33417",
  "keyChange": "https://acme-staging-v02.api.letsencrypt.org/acme/key-change",
  "meta": {
    "termsOfService": "https://letsencrypt.org/documents/LE-SA-v1.2-November-15-2017.pdf"
  },
  "newAccount": "https://acme-staging-v02.api.letsencrypt.org/acme/new-acct",
  "newNonce": "https://acme-staging-v02.api.letsencrypt.org/acme/new-nonce",
  "newOrder": "https://acme-staging-v02.api.letsencrypt.org/acme/new-order",
  "revokeCert": "https://acme-staging-v02.api.letsencrypt.org/acme/revoke-cert"
}
Acme v2 2025
{
  "J3x-Nn4YyGI": "https://community.letsencrypt.org/t/adding-random-entries-to-the-directory/33417",
  "keyChange": "https://acme-staging-v02.api.letsencrypt.org/acme/key-change",
  "meta": {
    "caaIdentities": [
      "letsencrypt.org"
    ],
    "profiles": {
      "classic": "https://letsencrypt.org/docs/profiles#classic",
      "shortlived": "https://letsencrypt.org/docs/profiles#shortlived (not yet generally available)",
      "tlsserver": "https://letsencrypt.org/docs/profiles#tlsserver (not yet generally available)"
    },
    "termsOfService": "https://letsencrypt.org/documents/LE-SA-v1.4-April-3-2024.pdf",
    "website": "https://letsencrypt.org/docs/staging-environment/"
  },
  "newAccount": "https://acme-staging-v02.api.letsencrypt.org/acme/new-acct",
  "newNonce": "https://acme-staging-v02.api.letsencrypt.org/acme/new-nonce",
  "newOrder": "https://acme-staging-v02.api.letsencrypt.org/acme/new-order",
  "renewalInfo": "https://acme-staging-v02.api.letsencrypt.org/draft-ietf-acme-ari-03/renewalInfo",
  "revokeCert": "https://acme-staging-v02.api.letsencrypt.org/acme/revoke-cert"
}

https://acme.zerossl.com/v2/DV90     2025
{
  "newNonce": "https://acme.zerossl.com/v2/DV90/newNonce",
  "newAccount": "https://acme.zerossl.com/v2/DV90/newAccount",
  "newOrder": "https://acme.zerossl.com/v2/DV90/newOrder",
  "revokeCert": "https://acme.zerossl.com/v2/DV90/revokeCert",
  "renewalInfo": "https://ari.trust-provider.com/renewalInfo",
  "keyChange": "https://acme.zerossl.com/v2/DV90/keyChange",
  "meta": {
    "termsOfService": "https://www.sectigo.com/uploads/files/Certificate-Subscriber-Agreement-2.7-click.pdf",
    "website": "https://zerossl.com",
    "caaIdentities": ["sectigo.com", "trust-provider.com", "usertrust.com", "comodoca.com", "comodo.com", "entrust.net", "affirmtrust.com"],
    "externalAccountRequired": true
  }
}

https://api.buypass.com/acme/directory
https://api.test4.buypass.no/acme/directory
{
"keyChange":"https://api.buypass.com/acme/key-change",
"meta":
    {"caaIdentities":["buypass.com"],
     "termsOfService":"https://api.buypass.com/acme/terms/1061",
     "website":"https://buypass.com/",
     "externalAccountRequired":false
     },
"newAccount":"https://api.buypass.com/acme/new-acct",
"newAuthz":"https://api.buypass.com/acme/new-authz",
"newNonce":"https://api.buypass.com/acme/new-nonce",
"newOrder":"https://api.buypass.com/acme/new-order",
"revokeCert":"https://api.buypass.com/acme/revoke-cert",
"renewalInfo":"https://api.buypass.com/acme/renewal-info"
}

Google Cloud 2025
https://dv.acme-v02.test-api.pki.goog/directory

https://dv.acme-v02.api.pki.goog/directory

{
"newNonce":"https://dv.acme-v02.api.pki.goog/new-nonce",
"newAccount":"https://dv.acme-v02.api.pki.goog/new-account",
"newOrder":"https://dv.acme-v02.api.pki.goog/new-order",
"newAuthz":"https://dv.acme-v02.api.pki.goog/new-authz",
"revokeCert":"https://dv.acme-v02.api.pki.goog/revoke-cert",
"keyChange":"https://dv.acme-v02.api.pki.goog/key-change",
"renewalInfo":"https://dv.acme-v02.api.pki.goog/renewal-info",
"meta": {
   "termsOfService":"https://pki.goog/GTS-SA.pdf",
   "website":"https://pki.goog",
   "caaIdentities":["pki.goog"],
   "externalAccountRequired":true
   }
}

https://acme.ssl.com/sslcom-dv-ecc

{
"newNonce":"https://acme.ssl.com/ejbca/acme/sslcom-dv-ecc/newNonce",
"newAccount":"https://acme.ssl.com/ejbca/acme/sslcom-dv-ecc/newAccount",
"newOrder":"https://acme.ssl.com/ejbca/acme/sslcom-dv-ecc/newOrder",
"revokeCert":"https://acme.ssl.com/ejbca/acme/sslcom-dv-ecc/revokeCert",
"keyChange":"https://acme.ssl.com/ejbca/acme/sslcom-dv-ecc/keyChange",
"meta":{
    "termsOfService":"https://legal.ssl.com/documents/SSLcom-Subscriber-Agreement-v1.4.pdf",
    "website":"https://www.ssl.com",
    "caaIdentities":["ssl.com"],
    "externalAccountRequired":false
    }
}

https://acme.ssl.com/sslcom-dv-rsa

{
"newNonce":"https://acme.ssl.com/ejbca/acme/sslcom-dv-rsa/newNonce",
"newAccount":"https://acme.ssl.com/ejbca/acme/sslcom-dv-rsa/newAccount",
"newOrder":"https://acme.ssl.com/ejbca/acme/sslcom-dv-rsa/newOrder",
"revokeCert":"https://acme.ssl.com/ejbca/acme/sslcom-dv-rsa/revokeCert",
"keyChange":"https://acme.ssl.com/ejbca/acme/sslcom-dv-rsa/keyChange",
"meta":{
    "termsOfService":"https://legal.ssl.com/documents/SSLcom-Subscriber-Agreement-v1.4.pdf",
    "website":"https://www.ssl.com",
    "caaIdentities":["ssl.com"],
    "externalAccountRequired":false
    }
}

https://one.digicert.com/mpki/api/v1/acme/v2/directory

{
"newNonce":"https://one.digicert.com/mpki/api/v1/acme/v2/new-nonce",
"newAccount":"https://one.digicert.com/mpki/api/v1/acme/v2/new-account",
"newOrder":"https://one.digicert.com/mpki/api/v1/acme/v2/new-order",
"revokeCert":"https://one.digicert.com/mpki/api/v1/acme/v2/revoke-cert",
"renewalInfo":"https://one.digicert.com/mpki/api/v1/acme/v2/renewal-info",
"meta":{
    "termsOfService":"https://www.digicert.com/master-services-agreement",
    "externalAccountRequired":true
    }
}

 *)

const
    SupplierProtoLits: array[TSupplierProto] of String =
        ('None', 'Own CA', 'Acme Supplier' ,'CertCentre', 'Servertastic' );     { V9.5 CertCentre gone, Servtas never supported }

 { V9.5 Acme protocol supplier names, now supported by most certificate providers, often with a test envrionment }
    AcmeSupplierLits: array[TAcmeSupplier] of String =
        ('Let''s Encrypt','Let''s Encrypt Testing',
         'ZeroSSL','Google','Google Testing','Digicert','Digicert Testing',
         'Sslcom RSA', 'Sslcom ECC');

 { V9.5 Acme protocol supplier file prefixes }
    AcmeSupplierPrefixes: array[TAcmeSupplier] of String =
        ('LE-','LE-','ZER-','GOO-','GOO-','DIG-','DIG-','SSL-', 'SSL-');

 { V9.5 Acme protocol supplier action API URLs }
    AcmeSupplierApiURLs: array[TAcmeSupplier] of String = (
        'https://acme-v02.api.letsencrypt.org/directory',
        'https://acme-staging-v02.api.letsencrypt.org/directory',
     {   'https://api.buypass.com/acme/directory',
        'https://api.test4.buypass.no/acme/directory',  }
        'https://acme.zerossl.com/v2/DV90',
        'https://dv.acme-v02.api.pki.goog/directory',
        'https://dv.acme-v02.test-api.pki.goog/directory',
        'https://one.digicert.com/mpki/api/v1/acme/v2/directory',
        'https://one.digicert.com/mpki/api/v1/acme/v2/directory',
        'https://acme.ssl.com/sslcom-dv-rsa',
        'https://acme.ssl.com/sslcom-dv-ecc');

 { V9.5 Acme protocol supplier action API URLs }
    AcmeSupplierWebURLs: array[TAcmeSupplier] of String = (
        'https://letsencrypt.org/docs/',
        'https://letsencrypt.org/docs/staging-environment/',
        'https://zerossl.com/documentation/acme/',
        'https://pki.goog',
        'https://pki.goog',
        'https://docs.digicert.com/en/certcentral/certificate-tools/certificate-lifecycle-automation-guides/third-party-acme-integration.html',
        'https://docs.digicert.com/en/certcentral/certificate-tools/certificate-lifecycle-automation-guides/third-party-acme-integration.html',
        'https://www.ssl.com/how-to/order-free-90-day-ssl-tls-certificates-with-acme/',
        'https://www.ssl.com/how-to/order-free-90-day-ssl-tls-certificates-with-acme/');

    ChallengeTypeLits: array[TChallengeType] of String =
        ('None', 'File - Web Server - UNC', 'File - Web Server - FTP',
        'File - Local Web Server', 'File - App Web Server', 'DNS - Automatic',
        'DNS Account - Automatic', 'DNS - Manual', 'Email Manually', 'TLS-ALPN - Web UNC',  { V8.64 dns man, V9.5 dns account }
        'TLS-ALPN - Local Web', 'TLS-ALPN - App Web', 'Manual');

    IssueStateLits: array[TIssueState] of String =
         ('None', 'Checked', 'Challg Request', 'Challg Wait', 'Challg Test', 'Challg Pending', 'Challg OK',
          'Final Pending', 'Issue Pending', 'Issued', 'Collected', 'Finished', 'Cancelled');

 // Acme resource action URLs
    AcmeResNewNonce = 'newNonce';     // V2 only
    AcmeResNewAccount = 'newAccount'; // V2
    AcmeResNewOrder = 'newOrder';     // V2 aka new-cert
    AcmeResRevokeCert = 'revokeCert'; // V2   V8.64 typo
    AcmeResKeyChange = 'keyChange';   // V2   V8.64 typo
    AcmeResNewAuthz = 'newAuthz';         // V2  V9.5 accounting, not used yet pre-authorize domain before challenge
    AcmeResRenewalInfo = 'renewalInfo';   // V2 V9.5

// V9.5 Acme meta actions
    AcmeResCaaIdentities = 'caaIdentities';     // Json array of domain names
    AcmeResProfiles = 'profiles';               // Json array of objects, name=URL
    AcmeResTermsOfService = 'termsOfService';   // URL
    AcmeResWebsite = 'website';                 // URL
    AcmeResExternalAccountRequired = 'externalAccountRequired';  // Boolean

 // Acme Actions
    AcmeNewNonce = 1;
    AcmeNewAccount = 2;
    AcmeNewOrder = 3;
    AcmeRevokeCert = 4;
    AcmeKeyChange = 5;
    AcmeNewAuthz =  6;
    AcmeRenewalInfo =  7;      { V9.5 }

    AcmeActionTot = 7 ;

var
    AcmeActionDirs: array [1..AcmeActionTot] of AcmeActionDir = (
      ( Action: AcmeResNewNonce; URL: ''),
      ( Action: AcmeResNewAccount; URL: ''),
      ( Action: AcmeResNewOrder; URL: ''),
      ( Action: AcmeResRevokeCert; URL: ''),
      ( Action: AcmeResKeyChange; URL: ''),
      ( Action: AcmeResNewAuthz; URL: ''),
      ( Action: AcmeResRenewalInfo; URL: '') );   { V9.5 }

type
{ TSubAltName is one subject alternate domain name }
  TSubAltName = class(TCollectionItem)
  private
    SADomain: String;
    SADirWellKnown: String;
    SADirPubWebCert: String;
    SAApprovalEmail: String;
    SAIssueState: TIssueState;
    SAStartDT: TDateTime;
    SADoneDT: TDateTime;
    SAValidResult: String;   // challenge validation result or error
  protected
    function GetDisplayName: string; override;
  published
    constructor Create (Collection: TCollection); Override ;
    property Domain: String                read  SADomain
                                           write SADomain;
    property DirWellKnown: String          read  SADirWellKnown
                                           write SADirWellKnown;
    property DirPubWebCert: String         read  SADirPubWebCert
                                           write SADirPubWebCert;
    property ApprovalEmail: String         read  SAApprovalEmail
                                           write SAApprovalEmail;
  end;

{ TSubAltNames defines a collection of TSubAltName }
  TSubAltNames = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TSubAltName;
    procedure SetItem(Index: Integer; Value: TSubAltName);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent);
    function IndexOf(const aDomain: String): Integer;
    function AddItem(const aDomain: String; aDirWellKnown: string = ''; aDirPubWebCert: string = ''; aApprovalEmail: String = ''): Integer;
    property Items[Index: Integer]: TSubAltName     read GetItem
                                                    write SetItem; default;
  end;

TSslX509Certs = class(TIcsWndControl)
  private
    { Private declarations }
// components
    FHttpRest: TSslHttpRest;
    FHttpTest: TSslHttpRest;
    FDomWebServer: TSimpleWebSrv;
{$IFDEF MSCRYPT_Tools}
    FNewSslCert: TMsCertTools;    { V8.67 was TSslCertTools }
{$ELSE}
    FNewSslCert: TSslCertTools;   { V8.69 no Windows cert store on Posix }
{$ENDIF}
    FAcmePrivKey: TSslCertTools;
    FDnsQuery: TDnsQuery;     { V8.64 }
    FOrderTimer: TIcsTimer;        { V9.5 }

// published properties
    FAcmeAccKeyType: TSslPrivKeyType;
    FCertAddress: String;
    FCertApprovEmail: String;
    FCertSubAltNames: TSubAltNames;
    FCertCommonName: String;
    FCertContactEmail: String;
    FCertContactFirst: String;
    FCertContactLast: String;
    FCertContactPhone: String;
    FCertContactTitle: String;
    FCertCountry: String;
    FCertDescr: String;
    FCertLocality: String;
    FCertOrgUnit: String;
    FCertOrganization: String;
    FCertPostcode: String;
    FCertPhone: String;
    FCertSignDigestType: TEvpDigest;
    FCertState: String;
    FCertValidity: Integer;
    FDebugLevel: THttpDebugLevel;
    FDirCertWork: String;
    FDirPubWebCert: TStringList;  // may be several wildcard servers
    FDirWellKnown: String;
    FDomWebSrvIP: String;
    FLogJson: Boolean;
    FLogPkeys: Boolean;
    FOAAccToken: String;
    FOAExpireDT: TDateTime;
    FOAAppUrl: String;         { V8.71 following 14 OA properties ignored }
    FOAAuthType: TOAuthType;
    FOAClientId: String;
    FOAClientSecret: String;
    FOARedirectUrl: String;
    FOARefrMinsPrior: Integer;
    FOARefreshAuto: Boolean;
    FOARefreshToken: String;
    FOAScope: String;
    FOATokenUrl: String;
    FOAWebSrvIP: String;
    FOAWebSrvPort: String;       { V8.71 14 OA ignored }
    FPrivKeyCipher: TSslPrivKeyCipher;
    FPrivKeyPassword: String;
    FPrivKeyType: TSslPrivKeyType;
    FSuppCertChallenge: TChallengeType;
    FSuppCertFeatures: String;
    FSuppCertProduct: String;
    FSuppOrderId: String;
    FSuppOrderRef: String;
    FSupplierProto: TSupplierProto;
    FSupplierServer: String;
    FSupplierEmail: String;
    FOnCertProg: THttpRestProgEvent;
    FOnNewToken: TNotifyEvent;
    FOnNewCert: TNotifyEvent;
    FOnDomainsRefresh: TNotifyEvent;
    FOnSuppDBRefresh: TNotifyEvent;
    FOnChallgRefresh: TNotifyEvent;
    FOnOAuthAuthUrl: TOAuthAuthUrlEvent;
    FonChallengeEmail: TChallengeEvent;
    FonChallengeFTP: TChallengeEvent;
    FonChallengeDNS: TChallengeEvent;
    FonChallengeManual: TChallengeEvent;
    FAutoOrderComplete: Boolean;
    FCAPkeyPw: String;
    FOwnCACertDir: String;
    FCertCsrOrigin: TCertCsrOrigin;
    FCAPkeyFile: String;
    FCACertFile: String;
    FCertOldCsrFile: String;
    FCertOldPrvKey: String;
    FCertOutFmts: TCertOutFmts;
    FSupplierTitle: String;
    FSeqOrderNum: Integer;
    FCertSerNumType: TSerNumType;
    FProxyURL: String;         // following V8.62
    FAutoAccountClose: Boolean;  // following V8.63
    FAccountTimeOutMins: Integer;
//    FDnsPubNr: Integer;         { V8.64 }
    FDnsServer: String;         { V8.64 }
    FChallgExpireDT: TDateTime;  { V8.64 was FAcmeOrderExpiresDT }
    FChallgSrvIP: String;        { V8.65 }
    FChallgSrvIP2: String;       { V8.71 }
    FKeepOldCA: Boolean;         { V8.68 }
    FOnX509OAToken: TX509OATokenEvent;  { V8.71 }
    FDomWebSrvIP2: String;       { V8.71 }
{$IFDEF MSCRYPT_Tools}
    FMsCertLoc: TMsCertLocation; { V8.71 }
{$ENDIF}
    FAcmeSupplier: TAcmeSupplier;   { V9.5 }
    FAcmeAccountNum: String;        { V9.5 }
    FAcmeAccountUrl: String;        { V9.5 used in all requests for an account }
    FAcmeEABKid: String;            { V9.5 external accounting }
    FAcmeEABHmacKey: String;        { V9.5 }
    FCertAcmeProfile: String;       { V9.5 }
    FCertRenewDays: Integer;        { V9.5 how many days before expiry to renew certificate }
    FCertRenewNow: Boolean;         { V9.5 set if cert order should be placed as soon as possible }
    FCertRenewRetryDT: TDateTime;   { V9.5 when to update renew dates, they may change }
    FCertRenewCheckDT: TDateTime;   { V9.5 when last checked renew dates }
    FCertRenewalId: String;         { V9.5 certificate renewal identifier, for renewal-request }
    FAcmeOrderDomains: TAcmeOrderDomains;   { V9.5 filled by AcmeV2OrderStatus for orders }
    FActiveOrders: TStringList;     { V9.5 list of common names for active order waiting challenges or processng }
    FAcmeChallngReport: String;     { V9.5 multiline challenge report updated by AcmeV2ChallngStatus }

// internal vars
    FAcmeHost: String;
    FIssueState: TIssueState;
//    FCCLastStatCode: Integer;
    FPartFNameWork: String;
    FPartFNameFinal: String;
    FPartFNameServer: TStringList;
    FPartFNameOrder: String;
    FFileCSR: string;
    FFilePrvKey: string;
    FFileCertPem: string;
    FFileBundPem: string;
    FFileInterPem: string;
    FFileBundP12: String;
    FFileBundP7: String;
    FCSRLines: string;
    FPrvKeyLines: string;
    FNewCertPrefix: string;
    FNewCertLines: string;
    FNewInterLines: string;
    FNewCertP7Lines: string;
    FNewCertCN: string;
    fNewCertSAN: string ;
    FNewCertChainInfo: string;
    FNewCertErrs: string;
    FNewCertEndDT: TDateTime;
    FNewCertStartDT: TDateTime;
    FNewCertValRes: TChainResult;
    FNewOrderNum: Integer;
    FAcmePubFName: string;
    FAcmePrivFName: string;
//    FLastErrMsg: String;
    FProductJson: ISuperObject;
    FProductDVAuth: String;
    FProductFeatures: String;
    FProductInfo: String;
    FProductCertType: String;
    FProductCA: String;
    FProductMaxSan: Integer;
    FProductQuote: String;
    FProductList: TStringList;
    FApproverEmails: TStringList;
    FAcmeLastStatus: Integer;
    FAcmeRespNonce: string;
    FAcmeRespLink: String;
    FAcmeRespRequester: String;
    FAcmeRespLocation: String;
    FAcmeRespContLoc: string;
    FAcmeKwkPub: String;
    FAcmeKwkKid: String;
    FAcmeJwsAlg: String;
    FAcmeJoseAlg: TJoseAlg;
    FAcmeTermsUrl: String;
    FAcmeKwkThumb: String;
    FAcmeCertLines: String;
    FAcmeCertUrl: String;
    FAcmeCertSerial: String;
    FAcmeOrderFinalizeUrl: String;
    FAcmeOrderStatus: String;
    FAcmeOrderObjUrl: String;
    FDbIniSections: TStringList;
    FDomainItems: TDomainItems;
    FControlFile: TIcsIniFile;
    FCnrtFileName: String;
    FChallengeItems: TChallengeItems;  // domain blank is unused
    FChallengesTot: Integer;           // used challenges
    FFileFinalCSR: String;
    FFileFinalPrvKey: String;
    FFileFinalBundle: String;
    FFileFinalCert: String;
    FOrderStartDT: TDateTime;
    FChallgStartDT: TDateTime;
    FChallgDoneDT: TDateTime;
    FOrderCertsDT: TDateTime;
    FOrderAttempts: Integer;
    FCertSANs: TStringList;  // matches FCertSubAltNames.Domain
    FCertSANTot: Integer;
    FPendingChallg: Integer;
    FX509BusyFlag: Boolean;
//    FPendOpenAccount: String;
    FLastResponse: String;
    FSocketFamily: TSocketFamily;   { V8.60 }
    FFileCertLocalHost: String;     { V8.60 }
    FChkChallgTrg: Int64;           { V8.63 }
    FPendAccountClose: Boolean;     { V8.63 }
    FAccountLastTick: Int64;        { V8.63 }
    FLastError: String;             { V8.63 }
    FAcmeCaaIndenties: TStringDynArray;  { V9.5 }
    FAcmeProfileNames: TStringDynArray;  { V9.5 }
    FAcmeProfileURL: TStringDynArray;    { V9.5 }
    FAcmeWebsite: String;                { V9.5 }
    FAcmeExtAccount: Boolean;            { V9.5 }
    FAccSuppDir: String;                 { V9.5 }
    FAccSuppFile: String;                { V9.5 }
    FOnGeoEvent: TIcsGeoEvent;           { V9.6 }

  protected
    { Protected declarations }
    procedure RestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
    procedure LogTimeStamp;
    procedure WebSrvReq(Sender: TObject; const Host, Path, Params: string; var RespCode, Body: string);
//    procedure ChallengeOnTimer(Sender: TObject);        { V9.5 gone }
    procedure OrderOnTimer(Sender: TObject);              { V9.5 }
    procedure AddActiveOrder(const CName: String);        { V9.5 }
    procedure RemoveActiveOrder(const CName: String);     { V9.5 }
    procedure SetSubAltNames(Value: TSubAltNames);
    procedure SetCertCommonName(const Value: String);
    procedure SetDirWellKnown(const Value: String);
    procedure SetDirPubWebCert(const Value: TStringList);
    procedure SetDirCertWork(const Value: String);
    procedure WebSrvProg(Sender: TObject; LogOption: TLogOption; const Msg: string);  { V8.63 }
    procedure IcsLogEvent(Sender: TObject; const Msg: string);      { V8.71 used by DnsQuery }
    procedure IcsGeoEvent(Sender: TObject; const IpStr: string; var Geo: String);  { V9.6 }
  public
    { Public declarations }
    AcmeSuppRecs: TAcmeSuppRecs;                                       { V9.5 }
    AcmeSuppTot: Integer;                                              { V9.5 }
    constructor  Create(Aowner: TComponent); override;
    destructor   Destroy; override;
    procedure LogEvent(const Msg: String);                              { V8.63  was protected }


(* V9.5 all CertCentre functions have gone, closed down
    function SetCertCentre(CreateNew: Boolean = False): boolean;
    function CCGetRequest(HttpReq: THttpRequest; const PageURL: String; const RawParams: String = ''): boolean;
    function CCGetProfile: Boolean;
    function CCGetProducts(Log: Boolean = False): boolean;
    function CCGetOneProduct(const Product: String): Boolean;
    function CCGetApproverEmail: Boolean;
    function CCFullfillment (JsonOrder: ISuperObject; const CertName: string): Boolean;
    function CCListAllOrders: Boolean;
    function CCGetCert: Boolean;
    function CCCheckChallg(ChallgNum: Integer): Boolean;
    function CCOrderCert: Boolean;
    function CCCheckOrder(DomainCheck: Boolean = True; UpdateDB: Boolean = False): Boolean;
    function CCCancelOrder(Revoke: Boolean): Boolean;

// most of these were Acme or Cert Centre wrappers, some old way of check Acme challenges
    function GetServerAPIUrl(Supplier: TSupplierProto; TestApi: Boolean = False): String;   { V9.5 gone }
    function AcmeV2GetCert(LogErrors: Boolean = True): Boolean;   { V8.64 added param }
    function AcmeV2WaitChallg(LogErrors: Boolean = True): Boolean;   { V9.5 }
    function AcmeV2CheckChallg(ChallgNum: Integer): Boolean;   // V9.5 probably not used
    function CheckChallg(const aDomain: String): Boolean;   V8.64 not used??
    function DBReadCNDomain(const CNDomain: String; UseStoredProps: Boolean): Boolean;
    function OpenAccount(const WorkDir: String; CreateNew: Boolean = False): Boolean;   V9.5 use SetAcmeAccount instead
    function CertReadDomain(const aDomain: String): Boolean;     V9.5 use DBReadCNDomain instead
    function CertSaveDomain(const aDomain: String): Boolean;     V9.5 use AcmeCheckSaveOrder instead
    function CertCheckDomain(const aDomain: String): Boolean;    V9.5 use AcmeCheckSaveOrder instead
    function CertGetChallgDomain(const aDomain: String): Boolean; V9.5 use AcmeV2GetChallgs instead
    function CertOrderDomain(const aDomain: String): Boolean;    V9.5 use AcmeV2StartChallgs
    function CertCollectDomain(const aDomain: String): Boolean;  V9.5 use AcmeV2OrderFinal and AcmeV2Collect
*)

    function  StartDomSrv(const HostName, CertBundle: String): Boolean ;
    function  StopDomSrv(NoLogging: Boolean = False): boolean ;         { V8.64 add param }
    function  DomSrvIsRunning: Boolean;
    function StartLocalServer(ChallgType: TChallengeType; const HostName: String): Boolean; { V8.64, V9.5 HostName }
    function PrepOneChallenge(Item: TChallengeItem): Boolean;            { V8.64 }
    function TestOneChallenge(Item: TChallengeItem): Boolean;            { V8.64 }
    function LocalOneChallenge(Item: TChallengeItem): Boolean;           { V8.64 }
    function TestChallenge(const aDomain, aDirWellKnown: String): Boolean;  { V8.64 }
    procedure CleanupChallenge(Item: TChallengeItem);                    { V8.64 }
    function TestAltNameChallgs: Boolean;  { V8.64 }
    function SaveDataFile(const FName, Data: String): Boolean;
    procedure SetFullFileNames(const FileDir: String);
    function CreateKeyandReq(IsCA: Boolean = False): boolean;                   { V9.1 CA }
    procedure DumpJson(const Item: String = '');
    function SetPartFNames(ReadOnly: Boolean = False): Boolean;
    function SaveCertificateFiles(const CertName: string): Boolean;
    function RedistribteFiles: Boolean;
    function SetAcmeAccount(CreateNew: Boolean = False): boolean;
    function AcmeGetRequest(HttpReq: THttpRequest; const FullURL: String; AcmeJson: String): boolean;   { V9.5 pass String not SO }
    function AcmeLoadPKey(New: Boolean): Boolean;
    function AcmeGetActions: Boolean;
    function AcmeCheckNonce: Boolean;   { V8.64 }
    function AcmeCheckSaveOrder(DomainCheck: Boolean = True; UpdateDB: Boolean = False): Boolean;  { V9.5 was AcmeCheckOrder }
    function AcmeV2NewAccount(ExistsOnly: Boolean): Boolean;    { V9.5 allow lookups }
    function AcmeV2GetChallgs: Boolean;    { V8.64 split from AcmeV2OrderCert }
    function AcmeV2TestChallgs: Boolean;   { V8.64 new stage }
    function AcmeV2StartChallgs: Boolean;  { V8.64 was AcmeV2OrderCert }
    function AcmeV2OrderStatus: Boolean;    { V9.5 }
    procedure ProcOrderStatus;   { V9.5 process order status }
    function AcmeV2CollectCert: Boolean;    { V9.5 }
    function AcmeV2OrderFinal(LogErrors: Boolean = True): Boolean;   { V9.5 }
    procedure AcmeGetChallngReport;      { V9.5 }
    function AcmeV2OrderCancel (Revoke: Boolean): Boolean;
    procedure RemoveOldChallgs(const CNDomain: String);
    function DBOpenINI(const WorkDir: String; CreateNew: Boolean = False): Boolean;
    function DBReadCNDomain(const CNDomain: String): Boolean;      { V9.5 always read stored properties }
    function DBWriteCNDomain: Boolean;
    function DBDeleteCNDomain(const CNDomain: String): Boolean;
    function DBReadAccount(const WorkDir: String): Boolean;  { V9.5 always reads stored props }
    function DBWriteAccount: Boolean;
    function DBNewOrderNum: Integer;
    function DBFindSAN(const adomain: String): Integer;
    function DBWriteOneChallenge (Item: TChallengeItem): Integer;
    function DBReadChallenges: Boolean;
    function DBFindDomain(const CNDomain: String): Integer;
    function DBReadSections: Boolean;
    function DBAddChallenge(Item: TChallengeItem): Integer;
    function DBRemoveChallenge(ChallgNum: Integer): Boolean;
    function DBFindChallengeNum (const Domain: String; Wildcard: Boolean): Integer;
    function DBFreeChallengeNum: Integer;
    function DBDeleteChallenge (const Domain: String; Wildcard: Boolean): Boolean;
    function CloseAccount: Boolean;
    procedure BuildSANList;
    function CertCancelDomain(const aDomain: String): Boolean;
    function CertRevokeDomain(const aDomain: String): Boolean;
    function CertRemoveDomain(const aDomain: String): Boolean;
    function CertResetDomain(const aDomain: String): Boolean;      { V9.2 }
    function CertRedistDomain(const aDomain: String): Boolean;
    function LoadOwnCA: Boolean;
    function OwnCASign(IsCA: Boolean): Boolean;                { V9.1 allow to create intermediate }
    function SelfSign(IsCA: Boolean): Boolean;                 { V8.64 }
    function CheckCSR(RequirePkey: Boolean = True): Boolean;
    function GetOrderResult: String;
    function CreateAcmeAlpnCert(const FileName, CName, KeyAuth: String): Boolean;  { V8.62 }
    procedure WebSrvAlpn(Sender: TObject; const Host: string; var CertFName: string);  { V8.64 }
    procedure WebSrvHttp(Sender: TObject; const Host, Path: string; var RespData: string);   { V8.64 }
    function DBSuppliersRead: Boolean;     { V9.5 }
    function DBSuppliersSave: Boolean;     { V9.5 }
    function DBFindAccSupp(const ATitle: String): Integer;      { V9.5 }
    function DBAddAccSupp(const ATitle, ADirAcc: String; ASupplier: TAcmeSupplier; CreateNew: Boolean;
                                                                AccKeyType: TSslPrivKeyType = PrivKeyECsecp256): Boolean;  { V9.5 }
    function GetAcmeRenewalInfo(Cert: TX509Base): String;       { V9.5 }
    function AcmeV2RenewalInfo: Boolean;                        { V9.5 }
    function DBReadCNDomDates(const CNDomain: String): Boolean; { V9.5 }
    function DBWriteCNDomDates: Boolean;                        { V9.5 }
    function CertRenewalDomain(const aDomain: String): Boolean; { V9.5 }
    procedure ClearAccount;                                     { V9.5 }
    procedure ClearCertOrder;                                   { V9.5 }
    procedure ClearRenewals;                                    { V9.5 }
    function IsAccountOpen: Boolean;                            { V9.5 }

    property ProductJson: ISuperObject              read FProductJson;
    property ProductDVAuth: String                  read FProductDVAuth;
    property ProductFeatures: String                read FProductFeatures;
    property ProductInfo: String                    read FProductInfo;
    property ProductList: TStringList               read FProductList;
    property ProductQuote: String                   read FProductQuote;
    property ApproverEmails: TStringList            read FApproverEmails;
{$IFDEF MSCRYPT_Tools}
    property NewSslCert: TMsCertTools               read FNewSslCert;     { V8.67 was TSslCertTools }
{$ELSE}
    property NewSslCert: TSslCertTools              read FNewSslCert;     { V8.69 no Windows cert store on Posix }
{$ENDIF}
    property IssueState: TIssueState                read FIssueState;
    property NewCertPrefix: string                  read FNewCertPrefix;
    property NewCertLines: string                   read FNewCertLines;
    property NewInterLines: string                  read FNewInterLines;
    property NewCertP7Lines: string                 read FNewCertP7Lines;
    property NewCertCN: string                      read FNewCertCN;
    property NewCertSAN: string                     read FNewCertSAN;
    property NewCertChainInfo: string               read FNewCertChainInfo;
    property NewCertErrs: string                    read FNewCertErrs;
    property NewCertEndDT: TDateTime                read FNewCertEndDT;
    property NewCertStartDT: TDateTime              read FNewCertStartDT;
    property NewCertValRes: TChainResult            read FNewCertValRes;
    property NewOrderNum: Integer                   read FNewOrderNum;
    property DomainItems: TDomainItems              read FDomainItems;
    property ChallengeItems: TChallengeItems        read FChallengeItems;
    property ChallengesTot: Integer                 read FChallengesTot;
    property CertSANs: TStringList                  read FCertSANs;  // matches FCertSubAltNames.Domain
    property LastResponse: String                   read FLastResponse;
    property FileBundPem: String                    read FFileBundPem;      { V8.62 }
    property FileBundP12: String                    read FFileBundP12;      { V8.62 }
    property FileFinalPrvKey: String                read FFileFinalPrvKey;  { V8.62 }
    property LastError: String                      read FLastError;        { V8.63 }
    property FileCSR: String                        read FFileCSR;          { V9.3 }
    property FilePrvKey: String                     read FFilePrvKey;       { V9.3 }
    property FileCertPem: String                    read FFileCertPem;      { V9.3 }
    property FileInterPem: String                   read FFileInterPem;     { V9.3 }
    property AcmeCaaIndenties: TStringDynArray      read FAcmeCaaIndenties;  { V9.5 }
    property AcmeProfileNames: TStringDynArray      read FAcmeProfileNames;  { V9.5 }
    property AcmeProfileURL: TStringDynArray        read FAcmeProfileURL;    { V9.5 }
    property AcmeWebsite: String                    read FAcmeWebsite;       { V9.5 }
    property AcmeExtAccount: Boolean                read FAcmeExtAccount;    { V9.5 }
    property AccSuppDir: String                     read FAccSuppDir;        { V9.5 }
    property AccSuppFile: String                    read FAccSuppFile;       { V9.5 }
    property CertRenewDays: Integer                 read FCertRenewDays;    { V9.5 }
    property CertRenewCheckDT: TDateTime            read FCertRenewCheckDT;  { V9.5 }
    property CertRenewalId: String                  read FCertRenewalId;     { V9.5 }
    property FileFinalBundle: String                read FFileFinalBundle;   { V9.5 }
    property OrderStartDT: TDateTime                read FOrderStartDT;      { V9.5 }
    property ChallgStartDT: TDateTime               read FChallgStartDT;     { V9.5 }
    property ChallgDoneDT: TDateTime                read FChallgDoneDT;      { V9.5 }
    property ChallgExpireDT: TDateTime              read FChallgExpireDT;    { V9.5 }
    property AcmeChallngReport: String              read FAcmeChallngReport; { V9.5 }
    property OrderCertsDT: TDateTime                read FOrderCertsDT;      { V9.5 }
    property AcmeOrderDomains: TAcmeOrderDomains    read FAcmeOrderDomains;  { V9.5 }
    property ChallgSrvIP: String                    read FChallgSrvIP;       { V9.5 }
    property ChallgSrvIP2: String                   read FChallgSrvIP2;      { V9.5 }
    property ActiveOrders: TStringList              read FActiveOrders;      { V9.5 }

  published
    { Published declarations }
    property AcmeAccKeyType: TSslPrivKeyType        read  FAcmeAccKeyType
                                                    write FAcmeAccKeyType;
    property AutoOrderComplete: Boolean             read  FAutoOrderComplete
                                                    write FAutoOrderComplete;
    property CAPkeyFile: String                     read  FCAPkeyFile
                                                    write FCAPkeyFile;
    property CAPkeyPw: String                       read  FCAPkeyPw
                                                    write FCAPkeyPw;
    property CACertFile: String                     read  FCACertFile
                                                    write FCACertFile;
    property CertAddress: String                    read  FCertAddress
                                                    write FCertAddress;
    property CertSubAltNames: TSubAltNames          read  FCertSubAltNames
                                                    write SetSubAltNames;
    property CertApprovEmail: String                read  FCertApprovEmail
                                                    write FCertApprovEmail;
    property CertCommonName: String                 read  FCertCommonName
                                                    write SetCertCommonName;
    property CertContactEmail: String               read  FCertContactEmail
                                                    write FCertContactEmail;
    property CertContactFirst: String               read  FCertContactFirst
                                                    write FCertContactFirst;
    property CertContactLast: String                read  FCertContactLast
                                                    write FCertContactLast;
    property CertContactPhone: String               read  FCertContactPhone
                                                    write FCertContactPhone;
    property CertContactTitle: String               read  FCertContactTitle
                                                    write FCertContactTitle;
    property CertCountry: String                    read  FCertCountry
                                                    write FCertCountry;
    property CertCsrOrigin: TCertCsrOrigin          read  FCertCsrOrigin
                                                    write FCertCsrOrigin;
    property CertDescr: String                      read  FCertDescr
                                                    write FCertDescr;
    property CertLocality: String                   read  FCertLocality
                                                    write FCertLocality;
    property CertOldCsrFile: String                 read  FCertOldCsrFile
                                                    write FCertOldCsrFile;
    property CertOldPrvKey: String                  read  FCertOldPrvKey
                                                    write FCertOldPrvKey;
    property CertOrgUnit: String                    read  FCertOrgUnit
                                                    write FCertOrgUnit;
    property CertOrganization: String               read  FCertOrganization
                                                    write FCertOrganization;
    property CertOutFmts: TCertOutFmts              read  FCertOutFmts
                                                    write FCertOutFmts;
    property CertPhone: String                      read  FCertPhone
                                                    write FCertPhone;
    property CertPostCode: String                   read  FCertPostCode
                                                    write FCertPostCode;
    property CertSerNumType: TSerNumType            read  FCertSerNumType
                                                    write FCertSerNumType;
    property CertSignDigestType: TEvpDigest         read  FCertSignDigestType
                                                    write FCertSignDigestType;
    property CertState: String                      read  FCertState
                                                    write FCertState;
    property CertValidity: Integer                  read  FCertValidity
                                                    write FCertValidity;
    property DebugLevel: THttpDebugLevel            read  FDebugLevel
                                                    write FDebugLevel;
    property DirCertWork: String                    read  FDirCertWork
                                                    write SetDirCertWork;
    property DirPubWebCert: TStringList             read  FDirPubWebCert
                                                    write SetDirPubWebCert;
    property DirWellKnown: String                   read  FDirWellKnown
                                                    write SetDirWellKnown;
    property DnsServer: String                      read  FDnsServer      { V8.64 }
                                                    write FDnsServer;
    property DomWebSrvIP: String                    read  FDomWebSrvIP
                                                    write FDomWebSrvIP;
    property DomWebSrvIP2: String                   read  FDomWebSrvIP2   { V8.71 }
                                                    write FDomWebSrvIP2;
{$IFDEF MSCRYPT_Tools}
    property MsCertLoc: TMsCertLocation             read  FMsCertLoc   { V8.71 }
                                                    write FMsCertLoc;
{$ENDIF}
    property KeepOldCA: Boolean                     read  FKeepOldCA
                                                    write FKeepOldCA;     { V8.68 }
    property CertRenewNow: Boolean                  read  FCertRenewNow
                                                    write FCertRenewNow;  { V9.5 }
    property CertRenewRetryDT: TDateTime            read  FCertRenewRetryDT
                                                    write FCertRenewRetryDT;  { V9.5 }
    property LogJson: Boolean                       read  FLogJson
                                                    write FLogJson;
    property LogPkeys: Boolean                      read  FLogPkeys
                                                    write FLogPkeys;
    property OAAccToken: String                     read  FOAAccToken
                                                    write FOAAccToken;
    property OAExpireDT: TDateTime                  read  FOAExpireDT
                                                    write FOAExpireDT;
    property OAAppUrl: string                       read  FOAAppUrl       { V8.71 following 14 OAuth2 properties ignored }
                                                    write FOAAppUrl;      { but not removed for backward compatibility }
    property OAClientId: string                     read  FOAClientId
                                                    write FOAClientId;
    property OAAuthType: TOAuthType                 read  FOAAuthType
                                                    write FOAAuthType;
    property OAClientSecret: string                 read  FOAClientSecret
                                                    write FOAClientSecret;
    property OARedirectUrl: string                  read  FOARedirectUrl
                                                    write FOARedirectUrl;
    property OARefreshAuto: Boolean                 read  FOARefreshAuto
                                                    write FOARefreshAuto;
    property OARefrMinsPrior: Integer               read  FOARefrMinsPrior
                                                    write FOARefrMinsPrior;
    property OARefreshToken: string                 read  FOARefreshToken
                                                    write FOARefreshToken;
    property OAScope: string                        read  FOAScope
                                                    write FOAScope;
    property OATokenUrl: string                     read  FOATokenUrl
                                                    write FOATokenUrl;
    property OAWebSrvIP: string                     read  FOAWebSrvIP
                                                    write FOAWebSrvIP;
    property OAWebSrvPort: string                   read  FOAWebSrvPort
                                                    write FOAWebSrvPort;    { V8.71 OA stuff ignored to here }
    property OwnCACertDir: String                   read  FOwnCACertDir
                                                    write FOwnCACertDir;
    property PrivKeyCipher: TSslPrivKeyCipher       read  FPrivKeyCipher
                                                    write FPrivKeyCipher;
    property PrivKeyPassword: string                read  FPrivKeyPassword
                                                    write FPrivKeyPassword;
    property PrivKeyType: TSslPrivKeyType           read  FPrivKeyType
                                                    write FPrivKeyType;
    property ProxyURL: String                       read  FProxyURL       { V8.62 }
                                                    write FProxyURL;
    property AutoAccountClose: Boolean              read  FAutoAccountClose     { V8.63 }
                                                    write FAutoAccountClose;
    property AccountTimeOutMins: Integer            read  FAccountTimeOutMins  { V8.63 }
                                                    write FAccountTimeOutMins;
    property SeqOrderNum: Integer                   read  FSeqOrderNum
                                                    write FSeqOrderNum;
    property SocketFamily: TSocketFamily            read  FSocketFamily
                                                    write FSocketFamily;   { V8.60 }
    property SuppCertChallenge: TChallengeType      read  FSuppCertChallenge
                                                    write FSuppCertChallenge;
    property SuppCertFeatures: String               read  FSuppCertFeatures
                                                    write FSuppCertFeatures;
    property SuppOrderId: String                    read  FSuppOrderId
                                                    write FSuppOrderId;
    property SuppOrderRef: String                   read  FSuppOrderRef
                                                    write FSuppOrderRef;
    property SuppCertProduct: String                read  FSuppCertProduct
                                                    write FSuppCertProduct;
    property SupplierEmail: String                  read  FSupplierEmail
                                                    write FSupplierEmail;
    property SupplierProto: TSupplierProto          read  FSupplierProto
                                                    write FSupplierProto;
    property SupplierServer: String                 read  FSupplierServer
                                                    write FSupplierServer;
    property SupplierTitle: String                  read  FSupplierTitle
                                                    write FSupplierTitle;
    property AcmeSupplier: TAcmeSupplier            read  FAcmeSupplier
                                                    write FAcmeSupplier;          { V9.5 }
    property AcmeAccountNum: String                 read  FAcmeAccountNum
                                                    write FAcmeAccountNum;        { V9.5 }
    property AcmeAccountUrl: String                 read  FAcmeAccountUrl
                                                    write FAcmeAccountUrl;        { V9.5 }
    property AcmeEABKid: String                     read  FAcmeEABKid
                                                    write FAcmeEABKid;            { V9.5 }
    property AcmeEABHmacKey: String                 read  FAcmeEABHmacKey
                                                    write FAcmeEABHmacKey;        { V9.5 }
    property CertAcmeProfile: String                read  FCertAcmeProfile
                                                    write FCertAcmeProfile;       { V9.5 }

    property OnCertProg: THttpRestProgEvent         read  FOnCertProg
                                                    write FOnCertProg;
    property OnNewCert: TNotifyEvent                read  FOnNewCert
                                                    write FOnNewCert;
    property OnNewToken: TNotifyEvent               read  FOnNewToken
                                                    write FOnNewToken;
    property OnOAuthAuthUrl: TOAuthAuthUrlEvent     read  FOnOAuthAuthUrl
                                                    write FOnOAuthAuthUrl;
    property OnDomainsRefresh: TNotifyEvent         read  FOnDomainsRefresh
                                                    write FOnDomainsRefresh;
    property OnSuppDBRefresh: TNotifyEvent          read  FOnSuppDBRefresh
                                                    write FOnSuppDBRefresh;
    property OnChallgRefresh: TNotifyEvent          read  FOnChallgRefresh
                                                    write FOnChallgRefresh;
    property OnChallengeEmail: TChallengeEvent      read  FOnChallengeEmail
                                                    write FOnChallengeEmail;
    property OnChallengeFTP: TChallengeEvent        read  FOnChallengeFTP
                                                    write FOnChallengeFTP;
    property OnChallengeDNS: TChallengeEvent        read  FOnChallengeDNS
                                                    write FOnChallengeDNS;
    property OnChallengeManual: TChallengeEvent     read  FOnChallengeManual
                                                    write FOnChallengeManual;
    property OnX509OAToken: TX509OATokenEvent       read  FOnX509OAToken
                                                    write FOnX509OAToken;     { V8.71 }
    property OnGeoEvent: TIcsGeoEvent               read  FOnGeoEvent
                                                    write FOnGeoEvent;        { V9.6 }
   end;

{$ENDIF USE_SSL}

implementation

{$IFDEF USE_SSL}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSubAltName }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSubAltName.Create(Collection: TCollection);
begin
    inherited;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSubAltName.GetDisplayName: string;
begin
   Result := Inherited GetDisplayName
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSubAltNames }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSubAltNames.Create(Owner: TPersistent);
begin
  FOwner := Owner;
  inherited Create(TSubAltName);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSubAltNames.GetItem(Index: Integer): TSubAltName;
begin
  Result := TSubAltName(inherited GetItem(Index));
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSubAltNames.SetItem(Index: Integer; Value: TSubAltName);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSubAltNames.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSubAltNames.IndexOf(const aDomain: string): Integer;
var
    I: Integer;
begin
    Result := -1;
    if Count = 0 then
        Exit;
    for I := 0 to Count - 1 do begin
        if Items[I].Domain = aDomain then begin
            Result := I;
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSubAltNames.AddItem(const aDomain: String; aDirWellKnown: string = ''; aDirPubWebCert: string = ''; aApprovalEmail: String = ''): Integer;
begin
    Result := -1;
    if Trim(aDomain) = '' then
        Exit;
    Result := IndexOf(aDomain);
    if Result < 0 then begin
        Result := Count;
        Add;
    end;
    Items[Result].SADomain := IcsLowercase(Trim(aDomain));
    if aDirWellKnown <> '' then
        Items[Result].SADirWellKnown := IncludeTrailingPathDelimiter(Trim(aDirWellKnown))
    else
        Items[Result].SADirWellKnown := '';
    if aDirPubWebCert <> '' then
        Items[Result].SADirPubWebCert := IncludeTrailingPathDelimiter(Trim(aDirPubWebCert))
    else
        Items[Result].SADirPubWebCert := '';
    Items[Result].SAApprovalEmail := aApprovalEmail;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSslX509Certs }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslX509Certs.Create(Aowner: TComponent);
begin
    inherited Create(AOwner);
    FDomWebServer := TSimpleWebSrv.Create(self);
    FDomWebServer.OnServerProg := WebSrvProg;      { V8.63 }
    FDomWebServer.OnSimpWebSrvReq := WebSrvReq;
    FDomWebServer.OnSimpWebSrvAlpn := WebSrvAlpn;  { V8.62 }
    FDomWebServer.OnGeoEvent := IcsGeoEvent;       { V9.6 }
    FHttpRest := TSslHttpRest.Create(self);   // REST requests
    FHttpRest.OnHttpRestProg := RestProg;
    FHttpTest := TSslHttpRest.Create(self);   // test .well-known requests
    FHttpTest.OnHttpRestProg := RestProg;
    FOrderTimer := TIcsTimer.Create(FHttpRest);    { V9.5 }
    FOrderTimer.OnTimer := OrderOnTimer;
    FOrderTimer.Interval := 5 * TicksPerSecond;
    FOrderTimer.Enabled := False;
{$IFDEF MSCRYPT_Tools}
    FNewSslCert := TMsCertTools.Create(self) ;    { V8.67 was TSslCertTools }
{$ELSE}
    FNewSslCert := TSslCertTools.Create(self);   { V8.69 no Windows cert store on Posix }
{$ENDIF}
    FAcmePrivKey := TSslCertTools.Create(self);
    FDirPubWebCert := TStringList.Create;
    FProductList := TStringList.Create;
    FApproverEmails := TStringList.Create;
    FCertSubAltNames := TSubAltNames.Create(self);
    FCertSANs := TStringList.Create;  // matches FCertSubAltNames.Domain
    FDBIniSections := TStringList.Create;
    FPartFNameServer := TStringList.Create;
    FActiveOrders := TStringList.Create;    { V9.5 }
    Randomize;

 { initialise all saved and internal variables }
    ClearAccount;            { V9.5 moved all account inits here }
    ClearCertOrder;          { V9.5 moved all certificate order inits here }

  { V9.5 common C:\ProgramData\ICS-Acme-Accounts\ics-acme-accounts.db file that lists other account databases }
    FAccSuppDir := GSSL_PUBLIC_DIR;   { V9.5 }
    SetLength(FAccSuppDir, Length(FAccSuppDir) - Length(GSSL_RES_SUBDIR) - 1);  // strip last directory
    FAccSuppDir := FAccSuppDir + DirAcmeAcc;
    try
        ForceDirectories(FAccSuppDir);
    except
    end;
    FAccSuppFile := FAccSuppDir + FileIcsAcmeAccDB;       { V9.5 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslX509Certs.Destroy;
begin
    try    { V8.71 }
        FOrderTimer.Enabled := False;
        StopDomSrv(True);  { V8.64 }
        FreeAndNil(FOrderTimer);
        FreeAndNil(FHttpTest);
        FreeAndNil(FHttpRest);
        FreeAndNil(FDomWebServer);
        FreeAndNil(FNewSslCert);
        FreeAndNil(FAcmePrivKey);
        FreeAndNil(FDirPubWebCert);
        FreeAndNil(FProductList);
        FreeAndNil(FApproverEmails);
        FreeAndNil(FControlFile);
        FreeAndNil(FCertSubAltNames);
        FreeAndNil(FDBIniSections);
        FreeAndNil(FCertSANs);
        FreeAndNil(FPartFNameServer);
        FreeAndNil(FActiveOrders);
    finally
        inherited Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.IcsGeoEvent(Sender: TObject; const IpStr: string; var Geo: String);  { V9.6 }
begin
    if Assigned(FOnGeoEvent) then begin   { V9.6 see if application has GEO tools, for country and ASN }
      FOnGeoEvent(Sender, IpStr, Geo);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.ClearAccount;            { V9.5 }
begin
    FOrderTimer.Enabled := False;
    FActiveOrders.Clear;
    StopDomSrv(True);  { V8.64 }
    FAccountTimeOutMins := 10;
    SetLength(FAcmeCaaIndenties, 0);
    SetLength(FAcmeProfileNames, 0);
    SetLength(FAcmeProfileURL, 0);
    FAcmeWebsite := '';
    FAcmeExtAccount := false ;
    FAcmeAccKeyType := PrivKeyRsa2048;
    FAcmeAccountNum := '';
    FAcmeAccountUrl := '';
    FAcmeEABHmacKey := '';
    FAcmeEABKid := '';
    FAcmeSupplier := AcmeLetsEncrypt;
    FCACertFile := '';
    FCAPkeyFile := '';
    FCAPkeyPw := '';
    FDebugLevel := DebugConn;
    FDirCertWork := '';
    FCnrtFileName := '';
    FDomWebSrvIP := ICS_ANY_HOST_V4;
    FDomWebSrvIP2 := '';
    FLogJson := false ;
    FLogPkeys := false ;
    FProxyURL := '';
    FSocketFamily := sfAny;
    FSupplierEmail := '';
    FSupplierProto := SuppProtoNone;
    FSupplierServer := '';
    FSupplierTitle := 'Unknown Supplier';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.ClearRenewals;            { V9.5 }
begin
    FCertRenewNow := False;
    FCertRenewDays := 0;
    FCertRenewCheckDT := 0;
    FCertRenewRetryDT := 0;
    FCertRenewalId := '';
    FNewCertCN := '';
    FNewCertChainInfo := '';
    FNewCertEndDT := 0;
    FNewCertErrs := '';
    FNewCertSAN := '';
    FNewCertStartDT := 0;
    FNewCertValRes := chainNone ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.ClearCertOrder;            { V9.5 }
begin
    FCertRenewNow := False;
    FAcmeOrderFinalizeUrl := '';
    FAcmeOrderObjUrl := '';
    FAcmeChallngReport := '';
    FAutoOrderComplete := true;
    FCertAcmeProfile := '';
    FCertAddress := '';
    FCertApprovEmail := '';
    FCertCommonName := '';
    FCertContactEmail := '';
    FCertContactFirst := '';
    FCertContactLast := '';
    FCertContactTitle := ''; ;
    FCertCountry := '';
    FCertCsrOrigin := CsrOriginProps;
    FCertLocality := '';
    FCertOldCsrFile := '';
    FCertOldPrvKey := '';
    FCertOrgUnit := '';
    FCertOrganization := '';
    FCertOutFmts := [];
    FCertPhone := '';
    FCertPostCode := '';
    FCertSANTot := 0;
    FCertSANs.Clear;
    FCertSerNumType := SerNumRandom;
    FCertSignDigestType := Digest_sha256;
    FCertState := '';
    FCertSubAltNames.Clear;
    FCertValidity := 90;        { V9.5 was 365 }
    FChallgDoneDT := 0;
    FChallgExpireDT := 0;
    FChallgSrvIP := '';
    FChallgSrvIP2 := '';
    FChallgStartDT := 0;
    FDirPubWebCert.Clear;
    FDirWellKnown := '';
    FFileFinalBundle := '';
    FFileFinalCSR := '';
    FFileFinalCert := '';
    FFileFinalPrvKey := '';
    FIssueState := IssStateNone;
    FNewOrderNum := 0;
    FOrderAttempts := 0;
    FOrderCertsDT := 0;
    FOrderStartDT := 0;
    FPartFNameFinal := '';
    FPartFNameOrder := '';
    FPartFNameServer.Clear;
    FPendingChallg := 0;
    FPrivKeyCipher := PrivKeyEncNone;
    FPrivKeyPassword := '';
    FPrivKeyType := PrivKeyRsa2048;
    FProductCA := '';
    FProductCertType := '';
    FProductDVAuth := '';
    FProductFeatures := '';
    FProductMaxSan := 0;
    FProductQuote := '';
    FSuppCertChallenge := ChallNone;
    FSuppCertProduct := '';
    FSuppOrderId := '';
    FSuppOrderRef := '';

{$IFDEF MSCRYPT_Tools}
    FMsCertLoc := MsLocMachine;
{$ENDIF}

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.SetSubAltNames(Value: TSubAltNames);
begin
    FCertSubAltNames.Assign(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.SetCertCommonName(const Value: String);
begin
    if FCertCommonName <> Value then begin
        if (Pos(IcsSpace, Trim(Value)) > 0) then  { V8.64 allow for CA names }
            FCertCommonName := Trim(Value)
        else
            FCertCommonName := IcsLowercase(Trim(Value));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.SetDirWellKnown(const Value: String);
begin
    if FDirWellKnown <> Value then begin
        if Value = '' then
            FDirWellKnown := ''
        else
            FDirWellKnown := IncludeTrailingPathDelimiter(Trim(Value));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.SetDirPubWebCert(const Value: TStringList);
var
    I: Integer;
begin
    if FDirPubWebCert.Text <> Value.Text then begin
        FDirPubWebCert.Clear;
        if Value.Count > 0 then begin
            for I := 0 to Value.Count - 1 do begin
                if Trim(Value[I]) <> '' then
                    FDirPubWebCert.Add(IncludeTrailingPathDelimiter(Trim(Value[I])));
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.SetDirCertWork(const Value: String);
begin
    if FDirCertWork <> Value then begin
        if Value = '' then
            FDirCertWork := ''
        else
            FDirCertWork := IncludeTrailingPathDelimiter(Trim(Value));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.RestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
begin
    if Assigned(FOnCertProg) then
        FOnCertProg(Self, LogOption, 'HTTP REST ' + Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.WebSrvProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
begin
    if Assigned(FOnCertProg) then
        FOnCertProg(Self, LogOption, 'Challenge Web Server ' + Msg);    { V8.63 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.LogEvent(const Msg : String);
begin
    FLastResponse := Msg;
    if FDebugLevel = DebugNone then
        Exit;
    if Assigned(FOnCertProg) then begin
        FOnCertProg(Self, loProtSpecInfo, Msg);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.LogTimeStamp;
begin
    LogEvent(RFC3339_DateToStr(Now));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ local web server for challenges, HTTPS if HostName not blank, otherwise HTTP }
{ creates self signed certificate for HTTPS if CertBundle blank }
function TSslX509Certs.StartDomSrv(const HostName, CertBundle: String): Boolean ;
var
    AltNames: TStringList;
begin
    Result := DomSrvIsRunning;
    if Result then begin   { V8.64 not running SSL and need it  }
        if (FDomWebServer.WebSrvHostName <> HostName) then begin
            LogEvent('Stopping challenge web server');    { V9.5 }
            StopDomSrv;
            Result := False;
         end;
    end;
    if (NOT Result) then begin
        LogEvent('Starting challenge web server for Host: ' + HostName);    { V9.5 }
        DBReadChallenges;  // get pending challenges
        FDomWebServer.DebugLevel := Self.FDebugLevel;
     { V8.66 check ChallgSrvIP should be defaulted }
        if (FChallgSrvIP = '') or (FChallgSrvIP = ICS_ANY_HOST_V4) then begin
            FChallgSrvIP := FDomWebSrvIP;
            FChallgSrvIP2 := FDomWebSrvIP2;   { V8.71 }
        end;
      { V8.66 check ChallgSrvIP exists, or set any IP, may fail if other servers using some IPs }
        if (LocalIPList(sfAny).IndexOf(FChallgSrvIP) < 0) and (FChallgSrvIP <> ICS_ANY_HOST_V4) then begin { V9.2 allow 0.0.0.0 }
            LogEvent('Warning, local IP ' + FChallgSrvIp + ' not found, using 0.0.0.0');
            FChallgSrvIP := ICS_ANY_HOST_V4;
        end;
        if (FChallgSrvIP2 <> '')  then begin       { V9.5 only if IPv6 allowed }
            if FChallgSrvIP = FChallgSrvIP2 then begin
                LogEvent('Need two different local web server addresses');
                Exit;
            end;
            if (LocalIPList(sfIPv6).IndexOf(FChallgSrvIP2) < 0) and (FChallgSrvIP2 <> ICS_ANY_HOST_V6) then begin  { V9.2 allow :: }
                if (FSocketFamily in [sfAny, sfAnyIPv6, sfIPv6]) then begin
                    LogEvent('Warning, local IPv6 ' + FChallgSrvIp2 + ' not found, using ::');
                    FChallgSrvIP2 := ICS_ANY_HOST_V6;
                end
                else begin
                    LogEvent('Warning, ignoring IPv6 address');
                    FChallgSrvIP2 := '';
                end;
            end;
        end;
        FDomWebServer.WebSrvIP := FChallgSrvIP;   { V8.65 may be different for diff domains }
        FDomWebServer.WebSrvIP2 := FChallgSrvIP2; { V8.71 IPv6 }
        FDomWebServer.WebSrvPort := '80';
        FDomWebServer.WebSrvPortSsl := '0';
        FDomWebServer.WebSrvHostName := '';
{$IFDEF AUTO_X509_CERTS}  { V8.65 }
        FDomWebServer.WebServer.SslX509Certs := Self;  { V8.64 so we get ALPN logging }
{$ENDIF} // AUTO_X509_CERTS
        if HostName <> '' then begin   { V8.62 }
            FDomWebServer.WebSrvPortSsl := '443';
            FDomWebServer.WebSrvPort := '0';        { V8.65 }
            FDomWebServer.WebSrvHostName := HostName;
            if (CertBundle <> '') and FileExists(CertBundle) then  { V8.64 }
                FDomWebServer.WebSrvCertBundle := CertBundle
            else begin
                if FDirCertWork = '' then begin
                    LogEvent('Need working directory to create localhost certificate');
                    Exit;
                end;
            // note don't use real host name for certificate, that is used for SNI/APLN
                FFileCertLocalHost := IncludeTrailingPathDelimiter(FDirCertWork) + 'cert-localhost.pem';
                if NOT FileExists(FFileCertLocalHost) then begin
                    try
                        AltNames := TStringList.Create;
                        AltNames.Add('localhost');   { V9.5 SAN name }
                        CreateSelfSignCertEx(FFileCertLocalHost, 'localhost', AltNames, PrivKeyECsecp256, '', '');  { V8.64 }
                        AltNames.Free;
                    except
                        on E:Exception do begin
                            LogEvent('Failed to create TLS/SSL certificate for: localhost - ' + E.Message);
                            Exit;
                        end;
                    end;
                end;
                FDomWebServer.WebSrvCertBundle := FFileCertLocalHost;
            end;
        end;
        Result := FDomWebServer.StartSrv;
        if Result then
            LogEvent('Challenge web server started on: ' + FDomWebServer.ListenStates)    { V8.62 }
        else
            LogEvent('Challenge web server failed to start');
    end
    else
       LogEvent('Challenge web server already running');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.StopDomSrv(NoLogging: Boolean = False): boolean ;         { V8.64 add param, returns false if stopped }
begin
    Result := False;   { V8.63 only if running, log it }
    if FDomWebServer.IsRunning then begin
        Result := FDomWebServer.StopSrv;
        if NOT Nologging then begin  { no logging during destroy }
            if Result then
                LogEvent('Challenge web server failed to stop')
            else
                LogEvent('Challenge web server stopped OK');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.DomSrvIsRunning: Boolean;
begin
    Result := FDomWebServer.IsRunning;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ event called by simple web server when any page is requested }
procedure TSslX509Certs.WebSrvReq(Sender: TObject; const Host, Path, Params: string; var RespCode, Body: string);
var
    Title, Msg, RespData: String;
    SWebSrv: TSimpleClientSocket;

    procedure BuildBody;
    begin
        Body := '<HTML><HEAD><TITLE>' + Title + '</TITLE></HEAD>' + IcsCRLF +
            '<BODY>' + IcsCRLF +
            '<H1>' + Title + '</H1>' + Msg + '<P>' + IcsCRLF +
            '</BODY></HTML>' + IcsCRLF;
        LogEvent('Challenge web response: ' + RespCode);
    end;

begin
 // ignore favicon requests completely
    if Path = '/favicon.ico' then begin
        RespCode := '404 Not Found';
        Title := RespCode;
        Msg := 'Error: File Not Found';
        BuildBody;
        Exit;
    end;

    SWebSrv := (Sender as TSimpleClientSocket);   { V9.5 }

    LogEvent(TimeToStr(Now) +  ': Challenge web request, Host: ' + Host + ', Path: ' + Path + ', Params: ' + Params);  { V9.5 added time }
    LogEvent('Request Host: ' + SWebSrv.RequestHost +  ', Request HostName: ' + SWebSrv.RequestHostName);  { V9.5 !! TEMP }
 //   FullURL := 'http://' + Host + Path;
    RespData := '';

  /// check if URL is for .well-known and matches a pending challenge
    if (Pos ('/.well-known/', Path) = 1) and (Length(FChallengeItems) > 0) then begin
        WebSrvHttp(Self, IcsLowerCase(Host), Path, RespData);   { V8.64 }
    end;

    if (RespData = '') then begin
        RespCode := '404 Not Found';
        Title := RespCode;
        Msg := 'Error: File Not Found';
        BuildBody;
    end
    else begin
 // found a page o return
        LogEvent('Challenge web server response sent for: ' + Host);
        FChkChallgTrg := IcsGetTrgSecs64 (10);  { V8.63 next check 10 secs }
        RespCode := '200 OK';
        Body := RespData;
    end;
  { web page is sent by event handler }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ event called by web server when http-01 challenge requested }
procedure TSslX509Certs.WebSrvHttp(Sender: TObject; const Host, Path: string; var RespData: string);  { V8.64 }
var
    I: Integer;
begin
    LogEvent('Challenge web http-01 request, URL: http://' + Host + Path);
    try
      /// check if host has Acme certificate created - beware path is mixed case
        if ( FChallengesTot > 0) and (Length(FChallengeItems) > 0) then begin
            for I := 0 to Length(FChallengeItems) - 1 do begin
                if (FChallengeItems [I].CDomain = Host) then begin  // was it for our domain
                    with FChallengeItems [I] do begin
                 // check if our page requested - beware need forward slashes
                        if (Pos(CPage, Path) >= 1) and (CResp <> '') then begin
                            RespData := CResp;
                            LogEvent('Sending web http-01 challenge response: ' + RespData);
                            break;
                        end;
                    end;
                end;
            end;
        end;
        if RespData = '' then     { V8.67 report error }
            LogEvent('Challenge web http-01 response: not found in list');
    except
        on E:Exception do
            LogEvent('Failed to find http-01 challenge - ' + E.Message);
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ event called by web server when tls-alpn-01 challenge requested }
procedure TSslX509Certs.WebSrvAlpn(Sender: TObject; const Host: string; var CertFName: string);
var
    I: Integer;
begin
    LogEvent('Challenge web tls-alpn-01 request, Host: ' + Host);
    try
      /// check if host has Acme certificate created
        if ( FChallengesTot > 0) and (Length(FChallengeItems) > 0) then begin
            for I := 0 to Length(FChallengeItems) - 1 do begin
                if FChallengeItems [I].CAlpnSNI = IcsLowerCase(Host) then begin  // was it for our domain, V9.5 AlpnSNI
                    with FChallengeItems [I] do begin
                        if (CAcmeAlpnCert <> '') then begin
                            if FileExists(CAcmeAlpnCert) then begin
                                CertFName := CAcmeAlpnCert;
                                LogEvent('Sending web tls-alpn-01 challenge certificate: ' + CertFName);
                                break;
                            end
                            else
                                LogEvent('Failed to find tls-alpn-01 challenge certificate: ' + CertFName);
                        end;
                    end;
                end;
            end;
        end;
    except
        on E:Exception do begin
            LogEvent('Failed to find tls-alpn-01 challenge - ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.SaveDataFile(const FName, Data: String): Boolean;
var
    NewFStream: TFileStream;
    Attempts: integer;
begin
    Result := False ; ;
    for attempts := 1 to 3 do
    begin
        if attempts > 1 then LogEvent('Failed to save fail, retrying');
        try
            if FileExists(FName) then
            begin
                if NOT DeleteFile(FName) then
                begin
                    LogEvent('Failed to delete old file: ' + FName) ;
                    exit ;
                end;
            end;
            if NOT ForceDirectories(ExtractFileDir (FName)) then
            begin
                LogEvent('Failed to create directory: ' + FName);
                continue;
            end;
            try
                NewFStream := TFileStream.Create (FName, fmCreate) ;
                NewFStream.WriteBuffer(AnsiString(Data) [1], Length (Data)) ;
                LogEvent('Saved file OK: ' + FName);
                Result := true ;
                Exit;
            finally
                FreeAndNil(NewFStream) ;
            end;
        except
            on E:Exception do begin
                LogEvent('Failed to save file: ' + FName + ' - ' + E.Message);
            end;
        end;
    end;
end ;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// start local web server if neeeded
function TSslX509Certs.StartLocalServer(ChallgType: TChallengeType; const HostName: String): Boolean; { V8.64, V9.5 HostName }
begin
    Result := False;
    if ChallgType = ChallFileSrv then
        Result := StartDomSrv('', '');
    if ChallgType = ChallAlpnSrv then
        Result := StartDomSrv(HostName, '');   { V9.5 correct HostName }
 end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// prepare challenge for one domain, either local or for certificate supplier
// might be well-know file, ALPN TLS certificate or DNS record
// starts local web server if needed for challenge
function TSslX509Certs.PrepOneChallenge(Item: TChallengeItem): Boolean;            { V8.64 }
begin
    Result := False;

 // check well known dircectory exists to create challenge file or APLN certificate, create it if not }
    if Item.CType in [ChallFileUNC, ChallAlpnUNC,  ChallAlpnApp, ChallAlpnSrv] then begin { V8.70 added Apln }
        if (Pos ('\', Item.CDirWellKnown) = 0) or
                    (Item.CDirWellKnown[Length(Item.CDirWellKnown)] <> '\') then begin
            FLastError := 'Invalid challenge well known directory: ' + Item.CDirWellKnown;
            LogEvent(FLastError);
            Exit;
        end;
        if NOT ForceDirectories (Item.CDirWellKnown) then                   { V8.70 }
            LogEvent ('Failed to create directory: ' + Item.CDirWellKnown);
    end;

  // Well-Known file challenges
    if Item.CType in [ChallFileUNC, ChallFileFtp, ChallFileApp, ChallFileSrv] then begin

     // UNC file share, create file on remote server
        if Item.CType in [ChallFileUNC] then begin
            LogEvent ('Built domain challenge validation file name: ' + Item.CWKFullName + ', saving token: ' + Item.CResp);
            Result := SaveDataFile (StringReplace(Item.CWKFullName, '/', '\', [rfReplaceAll]), Item.CResp); { V8.66 correct path }
        end

    // FTP handled by application - never tested !!
        else if (Item.CType = ChallFileFtp) then begin
            if Assigned(FonChallengeFTP) then begin
                LogEvent ('Built domain challenge validation file name: ' + Item.CWKFullName + ', saving token: ' + Item.CResp);
                if NOT SaveDataFile (Item.CWKFullName, Item.CResp) then Exit ;
                LogEvent ('!!! Must FTP challenge file: ' + Item.CWKFullName +' to .well-known directory for: ' + Item.CDomain);
                FonChallengeFTP(Self, Item, Result);
            end;
            if NOT Result then
                LogEvent ('FTP challenge setup failed');
        end

     // internal challenge database
        else if Item.CType in [ChallFileSrv, ChallFileApp] then begin
            LogEvent ('Using internal challenge validation for: ' + Item.CWKFullName + ', against token: ' + Item.CResp);
            Result := True;
        end;
    end

  // tls-alpn-01 challenges
    else if Item.CType in [ChallAlpnUNC, ChallAlpnSrv, ChallAlpnApp] then begin

        LogEvent('Creating ACME ALPN TLS/SSL challenge certificate: ' + Item.CAcmeAlpnCert);
     // UNC file share, create file on remote server
        if Item.CType = ChallAlpnUNC then begin
            Result := CreateAcmeAlpnCert(Item.CAcmeAlpnCert, Item.CDomain, Item.CResp);
            if NOT Result then begin
                FLastError := 'Failed to create ACME ALPN SSL certificate';
                LogEvent(FLastError);
            end;
        end

     // internal challenge database
        else if Item.CType in [ChallAlpnSrv, ChallAlpnApp] then begin
            Result := CreateAcmeAlpnCert(Item.CAcmeAlpnCert, Item.CDomain, Item.CResp);
            if NOT Result then begin
                FLastError := 'Failed to create ACME ALPN SSL certificate';
                LogEvent(FLastError);
            end;
        end;
    end

  // V8.64 DNS challenges - handled by application
    else if Item.CType in [ChallDNSAuto, ChallDnsAcnt, ChallDnsMan] then begin   { V9.5 added ChallDnsAcnt }
        LogEvent ('!!! Add challenge DNS TXT record for: ' + Item.CPage + ', with: ' + Item.CDNSValue);
        if Assigned(FOnChallengeDNS) then begin
            Result := False;
            FOnChallengeDNS(Self, Item, Result);
            if NOT Result then begin
                FLastError := 'DNS challenge setup failed';
                LogEvent(FLastError);
            end;
        end
        else
            LogEvent ('DNS challenge not supported by application');
    end;

  // start local server if needed
    if Result and (Item.CType in [ChallFileSrv, ChallAlpnSrv]) then begin
        if FDomWebServer.WebSrvIP <> FChallgSrvIP then
            StopDomSrv;  { V8.65 check correct IP }
        Result := StartLocalServer(Item.CType, Item.CAlpnSNI);  { V9.5 }
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.IcsLogEvent(Sender: TObject; const Msg: string);      { V8.71 used by DnsQuery }
begin
    LogEvent (Msg) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// test one challenge, either local or data from supplier for proper challenge
function TSslX509Certs.TestOneChallenge(Item: TChallengeItem): Boolean;            { V8.64 }
var
    Content, errinfo, TestUrl: string;
    StatCode, Loop, I: integer;
    Trg: Int64;
begin
    Result := False;

  // start local server if needed
    if (Item.CType in [ChallFileSrv, ChallAlpnSrv]) then begin
        if FDomWebServer.WebSrvIP <> FChallgSrvIP then
            StopDomSrv;  { V8.65 check correct IP }
        if NOT StartLocalServer(Item.CType, Item.CAlpnSNI) then  { V9.5 }
            Exit;
    end;

  // Well-Known file challenges
    if Item.CType in [ChallFileUNC, ChallFileFtp, ChallFileSrv, ChallFileApp] then begin
        TestUrl := 'http://' + Item.CDomain + '/.well-known/' + Item.CPage;
        LogEvent('Testing HTTP challenge ' + ChallengeTypeLits[Item.CType] + ' against: ' + TestUrl);

      // try and read it via HTTP
        try
            FHttpTest.AlpnProtocols.Clear;                   // V9.5 clear not blank
            FHttpTest.DebugLevel := FDebugLevel;             // V9.5 need logging
            FHttpTest.SocketFamily := FSocketFamily;         // V8.60 allow IPv6
            FHttpTest.ProxyURL := FProxyURL;                 // V8.62 proxy support
            FHttpTest.RestParams.Clear;
            StatCode := FHttpTest.RestRequest(HttpGET, TestUrl, False, '');
            errinfo := FHttpTest.ReasonPhrase;
        except
            on E:Exception do begin
                errinfo := 'Exception: ' + E.Message;     { V8.67 }
                StatCode := 99;
            end;
        end;
        if StatCode <> 200 then begin
            LogEvent('Failed to access HTTP challenge URL: ' + TestUrl + ' - ' + errinfo);  { V8.67 better message }
        end
        else begin
            Content := String(FHttpTest.ResponseOctet); // ignore content coding
            if Content = Item.CResp then begin
                 LogEvent('Successfully tested HTTP challenge for: ' + Item.CDomain);
                 Result := true;
            end
            else
                 LogEvent('Failed to compare challenge content  for: ' + Item.CDomain + ' - ' + Content);
        end;
    end;

  // tls-alpn-01 challenges
    if Item.CType in [ChallAlpnUNC, ChallAlpnSrv, ChallAlpnApp] then begin
        TestUrl := 'https://' + Item.CDomain + '/';
        LogEvent('Testing tls-alpn-01 challenge ' + ChallengeTypeLits[Item.CType] + ' against: ' + TestUrl);

      // try and connect via HTTPS, expect to fail due to self signed certificate
        try
            FHttpTest.AlpnProtocols.Clear;                   // V9.5 clear then add
            FHttpTest.AlpnProtocols.Add(AlpnAcmeTls1);
          //  FHttpTest.AlpnProtocols.Text := AlpnAcmeTls1;
            FHttpTest.DebugLevel := FDebugLevel;             // V9.5 need logging
            FHttpTest.SocketFamily := FSocketFamily;
            FHttpTest.ProxyURL := FProxyURL;
            FHttpTest.CertVerMethod := CertVerBundle;
            FHttpTest.SslAllowSelfSign := True;
            FHttpTest.SslReportChain := True;
            FHttpTest.RestParams.Clear;
     //       FHttpTest.Connection := 'close';   { V8.64 }
            FHttpTest.RestRequest(HttpGET, TestUrl, False, '');
            errinfo := FHttpTest.ReasonPhrase;
            if (FHttpTest.StatusCode <> 200) and (FHttpTest.GetAlpnProtocol = '') then begin
                LogEvent('Failed to test Tls-Alpn-01 challenge, No APLN:  ' + errinfo);
            end
            else begin
                Result := (FHttpTest.GetAlpnProtocol = AlpnAcmeTls1); // we reached Acme aware server
                if Result then
                    LogEvent('Successfully tested Tls-Alpn-01 challenge for: ' + Item.CDomain)
                else
                    LogEvent('Failed Tls-Alpn-01 challenge, ALPN received: '  + Item.CDomain + ' - ' + FHttpTest.GetAlpnProtocol);  { V8.67 }
            // pending, check certificate had correct stuff??
            end;
        except
            on E:Exception do begin
                LogEvent('Exception accessing challenge ALPN: ' + E.Message);  { V8.67 }
            end;
        end;
    end;

  // V8.64 DNS challenges
  // if a DNS server address is supplied tries that first, then runs through
  // list of well known public servers from Cloudfare, Google, etc, until one answers
    if Item.CType in [ChallDNSAuto, ChallDnsAcnt, ChallDnsMan] then begin   { V9.5 added ChallDnsAcn }
        LogEvent('Testing dns-01 challenge ' + ChallengeTypeLits[Item.CType] + ' for: ' + Item.CDomain);
        try
            if NOT Assigned(FDnsQuery) then
                FDnsQuery := TDnsQuery.Create(Nil);
            FDnsQuery.Proto := 'udp';
            if FDnsServer <> '' then begin
                FDnsQuery.ServerStrat := SrvStratOne;     { V8.71 }
                FDnsQuery.Addr := FDnsServer;
            end
            else
                FDnsQuery.ServerStrat := SrvStratPub;          { V8.71 use public server list }
            FDnsQuery.OnLogEvent := IcsLogEvent;               { V8.71 }
            LogEvent('DNS Query: ' + Item.CPage);
            for Loop := 1 to 6 do begin      // DNS servers seem to ignore TXT records and work next time!
             {   if FDnsPubNr >= 0 then begin
                    if (FDnsPubNr > High(DnsPublicServerTable)) then FDnsPubNr := 0;
                    FDnsQuery.Addr := DnsPublicServerTable[FDnsPubNr];
                end; }
                FDnsQuery.QueryAnySync(Item.CPage, DnsQueryTXT) ;
                if FDnsQuery.TXTRecordCount > 0 then begin
                    for I := 0 to FDnsQuery.TXTRecordCount - 1 do begin
                        if (FDnsQuery.TXTRecord[I] = Item.CDNSValue) then begin
                            LogEvent('Successfully tested DNS challenge for: ' + Item.CPage + ', Data=' + Item.CDNSValue);
                            Result := True;
                            Exit;
                        end;
                    end;
                end;
                LogEvent('Failed to test DNS ' + Item.CPage + ' challenge, no natching TXT record');
                if Loop = 3 then
                    break;
                LogEvent('Waiting 5 seconds for DNS retry');
                Trg := IcsGetTrgSecs64 (5) ;
                while (NOT IcsTestTrgTick64 (Trg)) do
                begin
                     FHttpTest.CtrlSocket.ProcessMessages ;
                     if FHttpTest.CtrlSocket.Terminated then
                        break ;
                end ;
            end;
        except
            on E:Exception do begin
                LogEvent('DNS query failed: ' + E.Message);
            end;
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// clean up challenge, remove old files or records
procedure TSslX509Certs.CleanupChallenge(Item: TChallengeItem);      { V8.64 }
var
    Flag: Boolean;
begin
    if Item.CDomain = '' then
        Exit;   { V8.65 }
    LogEvent('Cleaning up old challenge for: ' + Item.CDomain);

    if Item.CType in [ChallFileUNC] then begin
        if (Item.CWKFullName <> '') and FileExists(Item.CWKFullName) then
            DeleteFile(Item.CWKFullName);
    end;
    if (Item.CAcmeAlpnCert <> '') then begin
        if FileExists(Item.CAcmeAlpnCert) then
            DeleteFile(Item.CAcmeAlpnCert);
    end;

 // remove old DNS records, except fake which never change
    if Item.CType in [ChallDNSAuto, ChallDnsAcnt, ChallDnsMan] then begin   { V9.5 }
        if Pos ('fake', Item.CPage) > 0 then
            Item.CDNSValue := '';   { V8.65 }
        if (Item.CDNSValue <> '') and Assigned(FOnChallengeDNS) then begin
            LogEvent ('!!! Remove DNS TXT record for: ' + Item.CPage + ', with: ' + Item.CDNSValue);
            Item.CIssueState := IssStateCancel;  // delete DNS
            Flag := False;
            FOnChallengeDNS(Self, Item, Flag);
            if NOT Flag then
                LogEvent ('DNS challenge removal failed');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// local challenge test, fake challenge data
// warning other functions are dependent upon these CPage prefixes
function TSslX509Certs.LocalOneChallenge(Item: TChallengeItem): Boolean;    { V8.64 }
var
   ChallgNum: Integer;
begin
   LogEvent('Starting local challenge test for: ' + Item.CDomain + ', using method: ' + ChallengeTypeLits[Item.CType]);
   Item.CWKFullName := '';
   Item.CIssueState := IssStateNone ;
   Item.CWildcard := False;

    if Item.CType in [ChallFileUNC, ChallFileFtp, ChallFileSrv, ChallFileApp] then begin
        Item.CResp := 'My ICS Random String at ' + DateTimeToStr (Now) + ' for ' + Item.CDomain;
        Item.CPage := 'ics-validation/icstestfile-' + IntToStr(Random(9999999));  // forward slashes
        Item.CWKFullName := Item.CDirWellKnown + Item.CPage;
    end
    else if Item.CType in [ChallAlpnUNC, ChallAlpnSrv, ChallAlpnApp] then begin
        Item.CPage := 'fake-acmealpn-' + IcsBuildCertFName(Item.CDomain) + '.pem';
        Item.CResp := '1234';
        Item.CAcmeAlpnCert := Item.CDirWellKnown + Item.CPage;
    end
    else if Item.CType in [ChallDNSAuto, ChallDnsAcnt, ChallDnsMan] then begin   { V9.5 }
      // strip off wild card prefix
        if Pos ('*.', Item.CDomain) = 1 then begin
            Item.CDomain := Copy(Item.CDomain, 3, 99);
            Item.CWildcard := True;
            Item.CDNSValue := String(IcsBase64UrlEncodeA(IcsHashDigest('5678', Digest_sha256)));
        end
        else
            Item.CDNSValue := String(IcsBase64UrlEncodeA(IcsHashDigest('1234', Digest_sha256)));
        Item.CPage := '_fake-challenge.' + IcsIDNAToASCII(Item.CDomain);
    end
    else begin
        Result := True;
        LogEvent('Test skipped, method not supported');
        Exit;
    end;
    Result := PrepOneChallenge(Item);
    if Result then begin
    // save database challenge - only in array, never saved to database since temporary
        ChallgNum := DBAddChallenge(Item);
        Result := TestOneChallenge(Item);
        if NOT Result then
            Exit;

    // clean up challenge once OK
        CleanupChallenge (Item);
        if ChallgNum >= 0 then
            DBRemoveChallenge(ChallgNum);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// locally test challenge for one domain name
function TSslX509Certs.TestChallenge(const aDomain, aDirWellKnown: String): Boolean;  { V8.64 }
var
    CurChallenge: TChallengeItem;
begin
    CurChallenge.CCommonName := fCertCommonName;
    CurChallenge.CSuppOrderId := FSuppOrderId;
    CurChallenge.CSupplierProto := FSupplierProto;
    CurChallenge.CType := fSuppCertChallenge;
    CurChallenge.CIssueState := IssStateNone;
    CurChallenge.CDomain := aDomain;
    CurChallenge.CAlpnSNI := String(IcsReverseIPArpa(aDomain));   { V9.5 }
    CurChallenge.CDirWellKnown := aDirWellKnown;
    Result := LocalOneChallenge(CurChallenge);
    if (NOT Result) and (FLastError = '') then
        FLastError := FLastResponse;   // keep first real error
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// locally test all challenges for all alternate names
function TSslX509Certs.TestAltNameChallgs: Boolean;  { V8.64 }
var
    I: Integer;
begin
    Result := False;
    if FCertSubAltNames.Count = 0 then
        Exit;
    Result := True;
    for I := 0 to FCertSubAltNames.Count - 1 do begin
        if FCertSubAltNames[I].SADirWellKnown = '' then
            FCertSubAltNames[I].SADirWellKnown := FDirWellKnown;   { V8.66 sanity check }
        if NOT TestChallenge(FCertSubAltNames[I].SADomain,
                      FCertSubAltNames[I].SADirWellKnown) then Result := False;
    end;
    StopDomSrv;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// create a self signed certificate for the ACME tls-alpn-01 challenge with the provided token
// also creates simple self signed if no keyauth, for localhost
function TSslX509Certs.CreateAcmeAlpnCert(const FileName, CName, KeyAuth: String): Boolean;
var
    MySslCertTools: TSslCertTools;
    Hash: AnsiString;
    AcmeId, S: String;
    AltNames: TStringList;
begin
    Result := False;
    if KeyAuth <> '' then begin
        Hash := IcsHashDigest(AnsiString(KeyAuth), Digest_sha256);
        AcmeId := IcsBufferToHex(Hash);
    end;

    MySslCertTools := TSslCertTools.Create(nil);
    AltNames := TStringList.Create;
    try
        try
            if NOT ForceDirectories(ExtractFileDir (FileName)) then        { V8.70 }
            begin
                LogEvent('Failed to create directory: ' + FileName);
                Exit;
            end;
            AltNames.Add(CName);   { V9.5 Acme now requires SAN name or gives OID 2.5.29.17 is not present error }
            CreateSelfSignCertEx(FileName, CName, AltNames, PrivKeyECsecp256, '', AcmeId);  { V8.64 }
           { write cert to log }
            MySslCertTools.LoadFromFile(FileName, croTry, croTry);
            S := 'SSL certificate:';
            if KeyAuth <> '' then
                S := S + ' Acme keyAuthorization: ' + KeyAuth + ', Acme Identifier: ' + AcmeId;
            if FDebugLevel >= DebugSslLow then
                 S := S + IcsCRLF + MySslCertTools.GetRawText
            else
                S := S + IcsCRLF + MySslCertTools.CertInfo(False);
            LogEvent(S);
            Result := True;
        except
            on E:Exception do
                LogEvent ('Failed to create SSL certificate: ' + E.Message);
        end;
    finally
        MySslCertTools.Free;
        AltNames.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// read Acme supplier accounts list
function TSslX509Certs.DBSuppliersRead: Boolean;     { V9.5 }
var
    section, title: String;
    J: Integer;
    SupplieIni: TIcsIniFile;
begin
    Result := False;
    AcmeSuppTot := 0;
    SetLength(AcmeSuppRecs, 10);
    if NOT FileExists(FAccSuppFile) then
        Exit;
    try
        SupplieIni := TIcsIniFile.Create(FAccSuppFile);
      { allow up to 100 account directories }
        for J := 1 to 100 do begin
            section := 'Account-' + IntToStr (J);
            title := IcsTrim(SupplieIni.ReadString(section, 'SuppTitle', ''));
            if title = '' then
                continue;
            if (AcmeSuppTot + + 1) >= Length(AcmeSuppRecs) then
                SetLength(AcmeSuppRecs, AcmeSuppTot + 10);
            with AcmeSuppRecs[AcmeSuppTot] do begin
                ASuppTitle := title;
                ASupplier := TAcmeSupplier(GetEnumValue (TypeInfo (TAcmeSupplier),
                                      IcsTrim(SupplieIni.ReadString(section, 'Supplier', 'AcmeLetsEncryptLive'))));
                if ASupplier > High(TAcmeSupplier) then
                   ASupplier := AcmeLetsEncrypt;               { sanity test }
                ASuppDir := SupplieIni.ReadString(section, 'SuppDir', '');
                if ASuppDir <> ''  then
                    ASuppDir := IncludeTrailingPathDelimiter(Trim(ASuppDir)); // sanity test
            end;
            AcmeSuppTot := AcmeSuppTot + 1;
        end;
        SetLength(AcmeSuppRecs, AcmeSuppTot);
        Result := True;
    except
        on E:Exception do begin
            LogEvent('Could not open supplier accounts database: ' + FAccSuppFile + ' - ' + E.Message);
            Exit;
        end;
    end;
    SupplieIni.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// save Acme suppliers list
// creates C:\ProgramData\ICS-Acme-Accounts\ics-acme-accounts.db if missing
function TSslX509Certs.DBSuppliersSave: Boolean;     { V9.5 }
var
    section: String;
    J, Tot, DelNr: Integer;
    SupplieIni: TIcsIniFile;
begin
    Result := False;
    if AcmeSuppTot <= 0 then
        Exit;
    Tot := 0;
    DelNr := 0;
    try
        SupplieIni := TIcsIniFile.Create(FAccSuppFile);   // create if not exists
        for J := 0 to AcmeSuppTot - 1 do begin
            with AcmeSuppRecs[J] do begin
                if ASuppTitle <> ''  then begin  // not deleted
                    Tot := Tot + 1;
                    section := 'Account-' + IntToStr (Tot);
                    SupplieIni.WriteString(section, 'SuppTitle', Trim(ASuppTitle));
                    SupplieIni.WriteString(section, 'Supplier', GetEnumName (TypeInfo (TAcmeSupplier), Ord (ASupplier)));
                    SupplieIni.WriteString(section, 'SuppDir', IncludeTrailingPathDelimiter(Trim(ASuppDir)));
                end
                else
                    DelNr := DelNr + 1;
            end;
        end;

     // delete any extra sections
        while (DelNr > 0) do begin
            Tot := Tot + 1;
            DelNr := DelNr - 1;
            section := 'Account-' + IntToStr (Tot);
            SupplieIni.EraseSection(section);
        end;
        SupplieIni.UpdateFile; // write INI file
        Result := True;
    except
        on E:Exception do begin
            LogEvent('Could not create supplier accounts database: ' + FAccSuppFile + ' - ' + E.Message);
            Exit;
        end;
    end;
    SupplieIni.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// find Acme supplier account from list
function TSslX509Certs.DBFindAccSupp(const ATitle: String): Integer;     { V9.5 }
var
    J: Integer;
begin
    Result := -1;
    if AcmeSuppTot = 0 then
        Exit;
    for J := 0 to AcmeSuppTot - 1 do begin
        if AcmeSuppRecs[J].ASuppTitle = ATitle then begin
            Result := J;
            Exit;
        end;
   end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// add new Acme supplier account to supplier list, optional create account database
// closes account
function TSslX509Certs.DBAddAccSupp(const ATitle, ADirAcc: String; ASupplier: TAcmeSupplier; CreateNew: Boolean;
                                                                     AccKeyType: TSslPrivKeyType = PrivKeyECsecp256): Boolean;  { V9.5 }
var
    MySuppTitle, MyDirAcc, AccDBFile: String;
begin
    Result := False;
    MySuppTitle := Trim(ATitle);

// if account exists, nothing more to do
// pendng, should we check directory as well???
    if AcmeSuppTot = 0 then
        DBSuppliersRead ;
    if (DBFindAccSupp(MySuppTitle) >= 0) then begin
        LogEvent('Supplier already exists: ' + MySuppTitle);
        Result := True;
        Exit;
    end;
    if Pos(IcsSpace, MySuppTitle) > 0 then begin
        LogEvent('Supplier Name Not Allowed Blanks: ' + MySuppTitle);
        Exit;
    end;

// create account directory name if not passed
    MyDirAcc := ADirAcc;
    if MyDirAcc = '' then
        MyDirAcc := IncludeTrailingPathDelimiter(AccSuppDir) + MySuppTitle + '\';
    if NOT ForceDirectories (MyDirAcc) then begin
        LogEvent('Failed to Create Account Directory: ' + MyDirAcc);
        Exit;
    end;

// see if creating new supplier account database and Acme account with supplier
    AccDBFile := IncludeTrailingPathDelimiter(AccDBFile) + FileIcsCntlDB;
    if (NOT FileExists(AccDBFile)) then begin
        LogEvent('Creating New Supplier Account: ' + MySuppTitle + ' for Account Dir: ' + MyDirAcc);
        ClearAccount;
        FSupplierTitle := MySuppTitle;
        FAcmeAccKeyType := AccKeyType;
        FAcmeSupplier := ASupplier;
        FDirCertWork := MyDirAcc;
        SupplierProto := SuppProtoAcmeV2;
        SupplierServer := AcmeSupplierApiURLs[FAcmeSupplier];
        try
            if NOT DBOpenINI(FDirCertWork, True) then begin
                LogEvent('Failed to Create Supplier Account Database: ' + AccDBFile);
                Exit;
            end;
            if NOT DBWriteAccount then begin
                LogEvent('Failed to Save Supplier Account');
                Exit;
            end;

         // create new private key, then send it to Acme supplier to creare new account
            if CreateNew then begin
                if NOT SetAcmeAccount(True) then begin
                    LogEvent('Failed to Open Acme Account - ' + LastError + ': ' + MyDirAcc);
                    if ASupplier in [AcmeLetsEncrypt, AcmeLetsEncryptTest] then
                        Exit;
                    LogEvent('Need to Use GUI to Create Acme Account');
                end
                else
                    LogEvent('Created New Acme Account OK: ' + MySuppTitle);
            end;
        finally
            FreeAndNil(FControlFile);
        end;
    end;

// add new record to supplier table and save it
    AcmeSuppTot := AcmeSuppTot + 1;
    SetLength(AcmeSuppRecs, AcmeSuppTot);
    with AcmeSuppRecs[AcmeSuppTot - 1] do begin
        ASuppTitle := MySuppTitle;
        ASupplier := ASupplier;
        ASuppDir := MyDirAcc;
    end;
    Result := DBSuppliersSave;
    if NOT Result then
        LogEvent('Failed to Create New Supplier Account')
     else
        LogEvent('Created New Supplier Account OK: ' + MySuppTitle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// certificate supplier specific stuff,
function TSslX509Certs.DBOpenINI(const WorkDir: String; CreateNew: Boolean = False): Boolean;
var
    fname: String;
begin
    Result := False;
    fname := IncludeTrailingPathDelimiter(WorkDir) + FileIcsCntlDB;
    if Assigned(FControlFile) then begin
        if FCnrtFileName = fname then begin
            Result := True;
            Exit;
        end;
        FControlFile.Free;
    end;
    FCnrtFileName := fname;
    if (NOT CreateNew) and (NOT FileExists(FCnrtFileName)) then begin
        LogEvent('Account database not found: ' + FCnrtFileName);
        FCnrtFileName := '';
        Exit;
    end;
    try
        FControlFile := TIcsIniFile.Create(FCnrtFileName);
        Result := True;
    except
        on E:Exception do begin
            LogEvent('Could not open account database: ' + FCnrtFileName + ' - ' + E.Message);
            FCnrtFileName := '';
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// certificate supplier specific stuff,
function TSslX509Certs.DBReadAccount(const WorkDir: String): Boolean;
var
    section, OldWorkDir: String;
    OldProto: TSupplierProto;
begin
    Result := False;
    if NOT DBOpenINI(WorkDir, False) then
        Exit;
    section := CntlDBAccount;
    with FControlFile do begin

    // common stuff
        OldProto := TSupplierProto (GetEnumValue (TypeInfo (TSupplierProto), ReadString (section, 'SupplierProto', 'SuppProtoNone'))) ;
        if (Ord(OldProto) <= 0) or (OldProto <= SuppProtoNone) then begin
            LogEvent('Existing account not found');
            Exit;
        end;
        OldWorkDir := ReadString (section, 'DirCertWork', '') ;
        if (OldWorkDir <> '') and (CompareText(OldWorkDir, WorkDir) <> 0) then begin
            LogEvent('Certificate work directory, expected ' + WorkDir + ' but found ' + OldWorkDir);
        //    Exit;
        end;

     // do not read/write 'SeqOrderNum' here
        FSupplierProto := OldProto;
        FDirCertWork := OldWorkDir;
        FDebugLevel := THttpDebugLevel(GetEnumValue(TypeInfo(THttpDebugLevel), ReadString (section, 'DebugLevel', 'DebugNone'))) ;
        if (FDebugLevel < Low(THttpDebugLevel)) or (FDebugLevel > High(THttpDebugLevel)) then
            FDebugLevel := DebugConn;
        FDomWebSrvIP := ReadString (section, 'DomWebSrvIP', '') ;
        FDomWebSrvIP2 := ReadString (section, 'DomWebSrvIP2', '') ;    { V9.5 }
        FSocketFamily := TSocketFamily(GetEnumValue(TypeInfo(TSocketFamily), ReadString (section, 'SocketFamily', 'sfAny'))) ;  { V9.5 }
        if (FSocketFamily < Low(TSocketFamily)) or (FSocketFamily > High(TSocketFamily)) then
            FSocketFamily := sfAny;
        FProxyURL := ReadString (section, 'ProxyURL', '');             { V9.5 }
        if ReadString (section, 'LogJson', 'False') = 'True' then
            FLogJson := true
        else
            FLogJson := false ;
        if ReadString (section, 'LogPkeys', 'False') = 'True' then
            FLogPkeys := true
        else
            FLogPkeys := false ;
        FSupplierEmail := ReadString (section, 'SupplierEmail', '') ;
        FSupplierServer := ReadString (section, 'SupplierServer', '') ;
        FSupplierTitle := ReadString (section, 'SupplierTitle', 'UnknownSupplier') ;      { V9.5 }

     // own CA stuff
        if FSupplierProto = SuppProtoOwnCA then begin
            FCACertFile := ReadString (section, 'CACertFile', '') ;
            FCAPkeyFile := ReadString (section, 'CAPkeyFile', '') ;
            FCAPkeyPw := ReadString (section, 'CAPkeyPw', '') ;
        end;

    // ACME stuff
        if FSupplierProto = SuppProtoAcmeV2 then begin
            FAcmeAccKeyType := TSslPrivKeyType (GetEnumValue (TypeInfo (TSslPrivKeyType), ReadString (section, 'AcmeAccKeyType', 'PrivKeyRsa2048'))) ;
            fAcmeAccountUrl := ReadString (section, 'AcmeAccountUrl', '') ;
            fAcmeAccountNum := ReadString (section, 'AcmeAccountNum', '') ;
            FAcmeSupplier := TAcmeSupplier(GetEnumValue (TypeInfo (TAcmeSupplier),
                                      IcsTrim(ReadString(section, 'AcmeSupplier', 'AcmeLetsEncryptLive'))));  { V9.5 }
            if (FAcmeSupplier > High(TAcmeSupplier)) then
                   FAcmeSupplier := AcmeLetsEncrypt;               { sanity test }
            FAcmeEABKid := ReadString (section, 'AcmeEABKid', '') ;                    { V9.5 }
            FAcmeEABHmacKey := ReadString (section, 'AcmeEABHmacKey', '') ;            { V9.5 }
            if fAcmeAccountUrl <> '' then begin
                LogTimeStamp;
                LogEvent('Loaded existing ACME account: ' + fAcmeAccountUrl + ' from ' + WorkDir);
            end;
        end;
        LogEvent('Opened Supplier Account for: ' + FSupplierTitle + ', Protocol: ' +   SupplierProtoLits [FSupplierProto] +
                                                    ', Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', File: ' + FCnrtFileName);
    end;
    Result := DBReadSections;
    if Result then
        LogEvent('Common Name domains found, total: ' + IntToStr(Length(FDomainItems)));
 end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.DBReadSections: Boolean;
var
    I: Integer;
    MyDomains: TStringList;
    section: String;
begin
    Result := False;
    if NOT DBOpenINI(FDirCertWork, False) then
        Exit;
    MyDomains := TStringList.Create;
    try
        FControlFile.ReadSections(FDBIniSections);    // get cert domains and challenges
        for I := 0 to FDBIniSections.Count - 1 do begin
            if Pos(CntlDBDomainPre, FDBIniSections[I]) = 1 then
                 MyDomains.Add(Copy(FDBIniSections[I], Length(CntlDBDomainPre) + 1, 999));
        end;
        MyDomains.Sort;
        SetLength(FDomainItems, MyDomains.Count);
        if MyDomains.Count > 0 then begin
            for I := 0 to MyDomains.Count - 1 do begin
                with FDomainItems[I], FControlFile do begin
                    section := IcsLowerCase(CntlDBDomainPre + MyDomains[I]);
                    DCommonName := MyDomains[I];
                    DCertSANs := ReadString (section, 'CertSubAltList', '') ;
                    DProduct := ReadString (section, 'SuppCertProduct', '') ;
                    DSuppOrderId := ReadString (section, 'SuppOrderId', '') ;
                    DIssueState := TIssueState(GetEnumValue (TypeInfo (TIssueState), ReadString (section, 'IssueState', 'IssStateNone')));
                    if DIssueState > High(TIssueState) then DIssueState := IssStateNone;
                    DSuppCertChallg := TChallengeType (GetEnumValue (TypeInfo (TChallengeType), ReadString (section, 'SuppCertChallenge', 'ChallNone'))) ;
                    if DSuppCertChallg > High(TChallengeType) then DSuppCertChallg := ChallNone;
                    DCertStartDT := RFC3339_StrToDate(ReadString (section, 'NewCertStartDT', '')) ;
                    DCertEndDT := RFC3339_StrToDate(ReadString (section, 'NewCertEndDT', '')) ;
{$IFDEF MSCRYPT_Tools}
           //       DMsCertLoc := TMsCertLocation(GetEnumValue (TypeInfo (TMsCertLocation), ReadString (section, 'MsCertLoc', 'MsLocMachine'))) ; { V8.71 }
{$ENDIF}
                    DIssueStartDT := RFC3339_StrToDate(ReadString (section, 'ChallgStartDT', '')) ;  { V9.5 }
                    DCertProfile := ReadString (section, 'CertAcmeProfile', '') ;     { V9.5 }
                    DCertRenewDays := ReadInteger (section, 'CertRenewDays', 0) ;  { V9.5 }
                    if ReadString (section, 'CertRenewNow', 'False') = 'True' then DCertRenewNow := true else DCertRenewNow := false ; { V9.5 }
                end;
            end;
        end;
        Result := True;
        if Assigned(FOnDomainsRefresh) then
            FOnDomainsRefresh(Self);
    finally
        MyDomains.Free;
    end;
 end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.DBWriteAccount: Boolean;
var
    section, temp: string;
begin
    Result := False;
    if NOT DBOpenINI(FDirCertWork, False) then
        Exit;
    section := CntlDBAccount;
    with FControlFile do begin

    // common stuff
     // do not read/write 'SeqOrderNum' here
        WriteString (section, 'DebugLevel', GetEnumName (TypeInfo (THttpDebugLevel), Ord(FDebugLevel))) ;
        WriteString (section, 'DirCertWork', FDirCertWork) ;
        WriteString (section, 'DomWebSrvIP', FDomWebSrvIP) ;
        WriteString (section, 'DomWebSrvIP2', FDomWebSrvIP2) ;     { V9.5 }
        WriteString (section, 'SocketFamily', GetEnumName(TypeInfo(TSocketFamily), Ord(FSocketFamily)));  { V9.5 }
        WriteString (section, 'ProxyURL', FProxyURL);             { V9.5 }
        if FLogJson then temp := 'True' else temp := 'False' ; WriteString (section, 'LogJson', temp) ;
        if FLogPkeys then temp := 'True' else temp := 'False' ; WriteString (section, 'LogPkeys', temp) ;
        WriteString (section, 'SupplierEmail', FSupplierEmail) ;
        WriteString (section, 'SupplierProto',  GetEnumName (TypeInfo (TSupplierProto), Ord (FSupplierProto)));
        WriteString (section, 'SupplierServer', FSupplierServer) ;
        WriteString (section, 'SupplierTitle', FSupplierTitle) ;

    // ACME stuff
        if FSupplierProto = SuppProtoAcmeV2 then begin
            WriteString (section, 'AcmeAccKeyType', GetEnumName (TypeInfo (TSslPrivKeyType), Ord(FAcmeAccKeyType))) ;
            WriteString (section, 'AcmeAccountNum', fAcmeAccountNum) ;
            WriteString (section, 'AcmeAccountUrl', fAcmeAccountUrl) ;
            WriteString (section, 'AcmeSupplier', GetEnumName (TypeInfo (TAcmeSupplier), Ord(FAcmeSupplier))) ;  { V9.5 }
            WriteString (section, 'AcmeEABKid', FAcmeEABKid) ;                      { V9.5 }
            WriteString (section, 'AcmeEABHmacKey', FAcmeEABHmacKey) ;              { V9.5 }
        end;

     // own CA stuff
        if FSupplierProto = SuppProtoOwnCA then begin
            WriteString (section, 'CACertFile', FCACertFile) ;
            WriteString (section, 'CAPkeyFile', FCAPkeyFile) ;
            WriteString (section, 'CAPkeyPw', FCAPkeyPw) ;
        end;
    end;
    FControlFile.UpdateFile;  // write INI file
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.DBNewOrderNum: Integer;
var
    section: string;
begin
    Result := 0;
    if NOT DBOpenINI(FDirCertWork, False) then
        Exit;
    section := CntlDBAccount;
    FSeqOrderNum := FControlFile.ReadInteger (section, 'SeqOrderNum', 0) ;  { V8.64 }
    if FSeqOrderNum < 1001 then begin
        LogEvent('Failed to read valid SeqOrderNum, resetting');
  //    LogEvent('SeqOrderNum=' + FControlFile.ReadString (section, 'SeqOrderNum', '')); // !! TEMP DIAG
        FSeqOrderNum := 1001;
    end;
    Result := FSeqOrderNum;
    LogEvent('New sequential order number: ' + IntToStr(Result)) ;
    FSeqOrderNum := FSeqOrderNum + 1;
    FControlFile.WriteInteger (section, 'SeqOrderNum', FSeqOrderNum) ;
    FControlFile.UpdateFile;  // write INI file
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.DBFindDomain(const CNDomain: String): Integer;
var
    I: Integer;
begin
    Result := -1;
    if Length(FDomainItems) > 0 then begin
        for I := 0 to Length(FDomainItems) - 1 do begin
            if FDomainItems[I].DCommonName = CNDomain then begin
                Result := I;
                Exit;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// a certificate common name domain, perhaps with sub domains as well
// UseStoredProps replaces all public properties from database values
//function TSslX509Certs.DBReadCNDomain(const CNDomain: String; UseStoredProps: Boolean): Boolean;
function TSslX509Certs.DBReadCNDomain(const CNDomain: String): Boolean;  { V9.5 always reads Stored Properties }
var
    section, temp, aDomain, aDirWellKnown, aDirPubWebCert, aApprovalEmail, S: String;
    I, idx: Integer;
begin
    Result := False;
//    if UseStoredProps then
    ClearCertOrder;      { V9.5 }
    if NOT DBOpenINI(FDirCertWork, False) then
        Exit;
    section := IcsLowerCase(CntlDBDomainPre + CNDomain);
    with FControlFile do begin
        temp := ReadString (section, 'CertCommonName', 'xx') ;
        if temp <> CNDomain then begin
            LogEvent('Certificate domain not found in database: ' + CNDOmain);
            Exit;
         end;
        FCertCommonName := temp;

    // stuff supplied by user from public properties
        FCertAddress := ReadString (Section, 'CertAddress', '') ;
        FCertApprovEmail := ReadString (section, 'CertApprovEmail', '') ;
        FCertContactEmail := ReadString (section, 'CertContactEmail', '') ;
        FCertContactFirst := ReadString (section, 'CertContactFirst', '') ;
        FCertContactLast := ReadString (section, 'CertContactLast', '') ;
        FCertContactTitle := ReadString (section, 'CertContactTitle', '') ;
        FCertCountry := ReadString (section, 'CertCountry', '') ;
        FCertCsrOrigin := TCertCsrOrigin (GetEnumValue (TypeInfo (TCertCsrOrigin), ReadString (section, 'CertCsrOrigin', 'CsrOriginProps'))) ;
        FCertLocality := ReadString (section, 'CertLocality', '') ;
        FCertOldCsrFile := ReadString (section, 'CertOldCsrFile', '') ;
        FCertOldPrvKey := ReadString (section, 'CertOldPrvKey', '') ;
        FCertOrgUnit := ReadString (section, 'CertOrgUnit', '') ;
        FCertOrganization := ReadString (section, 'CertOrganization', '') ;
        IcsStrToSet(TypeInfo (TCertOutFmt), ReadString (section, 'CertOutFmts', ''), FCertOutFmts, SizeOf(FCertOutFmts)) ;
        FCertPhone := ReadString (section, 'CertPhone', '') ;
        FCertPostCode := ReadString (section, 'CertPostCode', '') ;
        FCertSerNumType := TSerNumType (GetEnumValue (TypeInfo (TSerNumType), ReadString (section, 'CertSerNumType', 'SerNumRandom'))) ;
        FCertSignDigestType := TEvpDigest (GetEnumValue (TypeInfo (TEvpDigest), ReadString (section, 'CertSignDigestType', 'Digest_sha256'))) ;
        FCertState := ReadString (section, 'CertState', '') ;
        FCertValidity := ReadInteger (section, 'CertValidity', 0) ;
        FDirPubWebCert.CommaText := ReadString (section, 'DirPubWebCert', '') ;
        FDirWellKnown := ReadString (section, 'DirWellKnown', '') ;
        S := ReadString (section, 'PrivKeyCipher', 'PrivKeyEncNone');
        FPrivKeyCipher := TSslPrivKeyCipher (GetEnumValue (TypeInfo (TSslPrivKeyCipher), S)) ;
        FPrivKeyPassword := ReadString (section, 'PrivKeyPassword', '') ;  // encrypt password !!!!
        FPrivKeyType := TSslPrivKeyType (GetEnumValue (TypeInfo (TSslPrivKeyType), ReadString (section, 'PrivKeyType', 'PrivKeyRsa2048'))) ;
        FSuppCertChallenge := TChallengeType (GetEnumValue (TypeInfo (TChallengeType), ReadString (section, 'SuppCertChallenge', 'ChallNone'))) ;
        if FSuppCertChallenge > High(TChallengeType) then
            FSuppCertChallenge := ChallNone;
        FSuppCertProduct := ReadString (section, 'SuppCertProduct', '') ;
        FSuppOrderId := ReadString (section, 'SuppOrderId', '') ;
        FSuppOrderRef := ReadString (section, 'SuppOrderRef', '') ;
        FChallgSrvIP := ReadString (section, 'ChallgSrvIP', FDomWebSrvIP) ;    { V8.65 }
        FChallgSrvIP2 := ReadString (section, 'ChallgSrvIP2', FDomWebSrvIP2) ;  { V8.71 }
{$IFDEF MSCRYPT_Tools}
        FMsCertLoc := TMsCertLocation(GetEnumValue (TypeInfo (TMsCertLocation), ReadString (section, 'MsCertLoc', 'MsLocMachine'))) ; { V8.71 }
{$ENDIF}
        FCertAcmeProfile := ReadString (section, 'CertAcmeProfile', '') ;     { V9.5 }
        if ReadString (section, 'AutoOrderComplete', 'True') = 'True' then FAutoOrderComplete := true else FAutoOrderComplete := false ; { V9.5 }
        if ReadString (section, 'CertRenewNow', 'False') = 'True' then FCertRenewNow := true else FCertRenewNow := false ; { V9.5 }

    // stuff created during certificate processing
        FAcmeOrderFinalizeUrl := ReadString (section, 'AcmeOrderFinalizeUrl', '');
        FAcmeOrderObjUrl := ReadString (section, 'AcmeOrderObjUrl', '');
        FChallgDoneDT := RFC3339_StrToDate(ReadString (section, 'ChallgDoneDT', '')) ;
        FChallgExpireDT := RFC3339_StrToDate(ReadString (section, 'ChallgExpireDT', '')); { V8.64 }
        FChallgStartDT := RFC3339_StrToDate(ReadString (section, 'ChallgStartDT', '')) ;
        FFileFinalBundle := ReadString (section, 'FileFinalBundle', '') ;
        FFileFinalCSR := ReadString (section, 'FileFinalCSR', '') ;
        FFileFinalCert := ReadString (section, 'FileFinalCert', '') ;
        FFileFinalPrvKey := ReadString (section, 'FileFinalPrvKey', '') ;
        FIssueState := TIssueState(GetEnumValue (TypeInfo (TIssueState), ReadString (section, 'IssueState', 'IssStateNone')));
        if FIssueState > High(TIssueState) then
            FIssueState := IssStateNone;
        FNewCertCN := ReadString (section, 'NewCertCN', '') ;
        FNewCertChainInfo := ReadString (section, 'NewCertChainInfo', '') ;
        FNewCertEndDT := RFC3339_StrToDate(ReadString (section, 'NewCertEndDT', '')) ;
        FNewCertErrs := ReadString (section, 'NewCertErrs', '') ;
        FNewCertSAN := ReadString (section, 'NewCertSAN', '') ;
        FNewCertStartDT := RFC3339_StrToDate(ReadString (section, 'NewCertStartDT', '')) ;
        FNewCertValRes := TChainResult(GetEnumValue (TypeInfo (TChainResult), ReadString (section, 'NewCertValRes', 'chainOK'))) ;
        FNewOrderNum := ReadInteger (section, 'NewOrderNum', 0) ;
        FOrderAttempts := ReadInteger (section, 'OrderAttempts', 0) ;
        FOrderCertsDT := RFC3339_StrToDate(ReadString (section, 'OrderCertsDT', '')) ;
        FOrderStartDT := RFC3339_StrToDate(ReadString (section, 'OrderStartDT', '')) ;
        FPartFNameFinal := ReadString (section, 'PartFNameFinal', '') ;
        FPartFNameOrder := ReadString (section, 'PartFNameOrder', '') ;
        FPartFNameServer.CommaText := ReadString (section, 'PartFNameServer', '') ;
        FPendingChallg := ReadInteger (section, 'PendingChallg', 0) ;
        FProductCA := ReadString (section, 'ProductCA', '') ;
        FProductCertType := ReadString (section, 'ProductCertType', '') ;
        FProductDVAuth := ReadString (section, 'ProductDVAuth', '') ;
        FProductFeatures := ReadString (section, 'ProductFeatures', '') ;
        FProductMaxSan := ReadInteger (section, 'ProductMaxSan', 0) ;
        FProductQuote := ReadString (section, 'ProductQuote', '') ;
        FCertRenewDays := ReadInteger (section, 'CertRenewDays', 0) ;                         { V9.5 }
        FCertRenewCheckDT := RFC3339_StrToDate(ReadString (section, 'CertRenewCheckDT', '')) ;  { V9.5 }
        FCertRenewRetryDT := RFC3339_StrToDate(ReadString (section, 'CertRenewRetryDT', '')) ;  { V9.5 }
        FCertRenewalId := ReadString (section, 'CertRenewalId', '') ;                           { V9.5 }
        FAcmeChallngReport := IcsUnEscapeCRLF(ReadString (section, 'AcmeChallngReport', '')) ; { V9.5 }

    // find Subject Alternate Names
 //       if UseStoredProps then begin
            FCertSANs.CommaText := ReadString (section, 'CertSubAltList', '') ;
            FCertSubAltNames.Clear;
            FCertSANTot := FCertSANs.Count;
            if FCertSANTot > 0 then begin
                for I := 0 to FCertSANTot - 1 do begin
                    section := IcsLowerCase(CntlDBSANPre + FCertCommonName + '=' + FCertSANs[I]);
                    aDomain := ReadString (section, 'Domain', '') ;
                    aDirWellKnown := ReadString (section, 'DirWellKnown', '') ;
                    aDirPubWebCert := ReadString (section, 'DirPubWebCert', '') ;
                    aApprovalEmail := ReadString (section, 'ApprovalEmail', '') ;
                    if aDomain <> '' then begin
                        idx := FCertSubAltNames.AddItem (aDomain, aDirWellKnown, aDirPubWebCert, aApprovalEmail);
                        with FCertSubAltNames [idx] do begin
                            SAIssueState := TIssueState(GetEnumValue (TypeInfo (TIssueState), ReadString (section, 'SAIssueState', 'IssStateNone'))) ;
                            if SAIssueState > High(TIssueState) then SAIssueState := IssStateNone;
                            SAStartDT := RFC3339_StrToDate(ReadString (section, 'SAStartDT', '')) ;
                            SADoneDT := RFC3339_StrToDate(ReadString (section, 'SADoneDT', '')) ;
                            SAValidResult := ReadString (section, 'SAValidResult', '') ;
                        end;
                    end;
                end;
            end;
 //       end;
    end;
    Result := True;

  // try and update one FDomainItems to avoid reloading lot
    idx := DBFindDomain(CNDomain);
    if idx < 0 then
        DBReadSections
    else begin
        FDomainItems[idx].DCertSANs := FCertSANs.CommaText;
        FDomainItems[idx].DProduct := FSuppCertProduct;
        FDomainItems[idx].DSuppOrderId := FSuppOrderId;
        FDomainItems[idx].DIssueState := FIssueState;
        FDomainItems[idx].DCertStartDT := FNewCertStartDT;
        FDomainItems[idx].DCertEndDT := FNewCertEndDT;
        FDomainItems[idx].DIssueStartDT := FChallgStartDT;       { V9.5 }
        FDomainItems[idx].DCertProfile := FCertAcmeProfile;      { V9.5 }
        FDomainItems[idx].DCertRenewDays := FCertRenewDays;    { V9.5 }
        FDomainItems[idx].DCertRenewNow := FCertRenewNow;        { V9.5 }
    end;
    if Assigned(FOnDomainsRefresh) then
       FOnDomainsRefresh(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// V9.5 get renewal infomation dates only for certificate common name domain }
// only used to build IcsHosts dates
function TSslX509Certs.DBReadCNDomDates(const CNDomain: String): Boolean;     { V9.5 }
var
    section, temp: String;
begin
    Result := False;
    if NOT DBOpenINI(FDirCertWork, False) then
        Exit;
    section := IcsLowerCase(CntlDBDomainPre + CNDomain);
    with FControlFile do begin
        temp := ReadString (section, 'CertCommonName', 'xx') ;
        if temp <> CNDomain then begin
            LogEvent('Certificate domain not found in database: ' + CNDOmain);
            Exit;
         end;
        FCertCommonName := 'xx';   // deliberately corrupted to ensure all props read again
        FIssueState := TIssueState(GetEnumValue (TypeInfo (TIssueState), ReadString (section, 'IssueState', 'IssStateNone')));
        if FIssueState > High(TIssueState) then
            FIssueState := IssStateNone;
        FAcmeOrderObjUrl := ReadString (section, 'AcmeOrderObjUrl', '');
        FChallgExpireDT := RFC3339_StrToDate(ReadString (section, 'ChallgExpireDT', ''));
        FNewCertEndDT := RFC3339_StrToDate(ReadString (section, 'NewCertEndDT', '')) ;
        FNewCertStartDT := RFC3339_StrToDate(ReadString (section, 'NewCertStartDT', '')) ;
        FCertRenewDays := ReadInteger (section, 'CertRenewDays', 0) ;
        if ReadString (section, 'CertRenewNow', 'False') = 'True' then FCertRenewNow := true else FCertRenewNow := false ;
        FCertRenewCheckDT := RFC3339_StrToDate(ReadString (section, 'CertRenewCheckDT', '')) ;
        FCertRenewRetryDT := RFC3339_StrToDate(ReadString (section, 'CertRenewRetryDT', '')) ;
        FCertRenewalId := ReadString (section, 'CertRenewalId', '') ;
        FFileFinalBundle := ReadString (section, 'FileFinalBundle', '') ;
        Result := True;
   end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.DBWriteCNDomain: Boolean;
var
    section, temp: string;
    I, J: Integer;
    flag: Boolean;
begin
    Result := False;
    if FCertCommonName = ''  then begin         { V8.71 }
        LogEvent('Can not save domain with blank Common Name') ;
        Exit;
    end;
    if NOT DBOpenINI(FDirCertWork, False) then
        Exit;
    section := CntlDBDomainPre + FCertCommonName;
    with FControlFile do begin
        WriteString (section, 'AcmeOrderFinalizeUrl', FAcmeOrderFinalizeUrl);
        WriteString (section, 'AcmeOrderObjUrl', FAcmeOrderObjUrl);
        WriteString (Section, 'CertAddress', FCertAddress) ;
        WriteString (section, 'CertApprovEmail', FCertApprovEmail) ;
        WriteString (section, 'CertCommonName', FCertCommonName) ;
        WriteString (section, 'CertContactEmail', FCertContactEmail) ;
        WriteString (section, 'CertContactFirst', FCertContactFirst) ;
        WriteString (section, 'CertContactLast', FCertContactLast) ;
        WriteString (section, 'CertContactTitle', FCertContactTitle) ;
        WriteString (section, 'CertCountry', FCertCountry) ;
        WriteString (section, 'CertCsrOrigin', GetEnumName (TypeInfo (TCertCsrOrigin), Ord(CertCsrOrigin))) ;
        WriteString (section, 'CertLocality', FCertLocality) ;
        WriteString (section, 'CertOldCsrFile', CertOldCsrFile) ;
        WriteString (section, 'CertOldPrvKey', CertOldPrvKey) ;
        WriteString (section, 'CertOrgUnit', FCertOrgUnit) ;
        WriteString (section, 'CertOrganization', FCertOrganization) ;
        WriteString (section, 'CertOutFmts', IcsSetToStr(TypeInfo (TCertOutFmt), FCertOutFmts, SizeOf(FCertOutFmts))) ;
        WriteString (section, 'CertPhone', FCertPhone) ;
        WriteString (section, 'CertPostCode', FCertPostCode) ;
        WriteString (section, 'CertSerNumType', GetEnumName (TypeInfo (TSerNumType), Ord(FCertSerNumType))) ;
        WriteString (section, 'CertSignDigestType', GetEnumName (TypeInfo (TEvpDigest), Ord(FCertSignDigestType))) ;
        WriteString (section, 'CertState', FCertState) ;
        WriteInteger (section, 'CertValidity', FCertValidity) ;
        WriteString (section, 'ChallgDoneDT', RFC3339_DateToStr(FChallgDoneDT)) ;
        WriteString (section, 'ChallgExpireDT', RFC3339_DateToStr(FChallgExpireDT)); { V8.64 }
        WriteString (section, 'ChallgStartDT', RFC3339_DateToStr(FChallgStartDT)) ;
        WriteString (section, 'DirPubWebCert', FDirPubWebCert.CommaText) ;
        WriteString (section, 'DirWellKnown', FDirWellKnown) ;
        WriteString (section, 'FileFinalBundle', FFileFinalBundle) ;
        WriteString (section, 'FileFinalCSR', FFileFinalCSR) ;
        WriteString (section, 'FileFinalCert', FFileFinalCert) ;
        WriteString (section, 'FileFinalPrvKey', FFileFinalPrvKey) ;
        WriteString (section, 'IssueState', GetEnumName (TypeInfo (TIssueState), Ord(FIssueState)));
        WriteString (section, 'NewCertCN', FNewCertCN) ;
        WriteString (section, 'NewCertChainInfo', FNewCertChainInfo) ;
        WriteString (section, 'NewCertEndDT', RFC3339_DateToStr(FNewCertEndDT)) ;
        WriteString (section, 'NewCertErrs', FNewCertErrs) ;
        WriteString (section, 'NewCertSAN', FNewCertSAN) ;
        WriteString (section, 'NewCertStartDT', RFC3339_DateToStr(FNewCertStartDT)) ;
        WriteString (section, 'NewCertValRes', GetEnumName (TypeInfo (TChainResult), Ord(FNewCertValRes))) ;
        WriteInteger (section, 'NewOrderNum', FNewOrderNum) ;
        WriteInteger (section, 'OrderAttempts', FOrderAttempts) ;
        WriteString (section, 'OrderCertsDT', RFC3339_DateToStr(FOrderCertsDT)) ;
        WriteString (section, 'OrderStartDT', RFC3339_DateToStr(FOrderStartDT)) ;
        WriteString (section, 'PartFNameFinal', FPartFNameFinal) ;
        WriteString (section, 'PartFNameOrder', FPartFNameOrder) ;
        WriteString (section, 'PartFNameServer', FPartFNameServer.CommaText) ;
        WriteInteger (section, 'PendingChallg', FPendingChallg) ;
        WriteString (section, 'PrivKeyCipher', GetEnumName (TypeInfo (TSslPrivKeyCipher), Ord(FPrivKeyCipher))) ;
        WriteString (section, 'PrivKeyPassword', FPrivKeyPassword) ;  // encrypt password !!!!
        WriteString (section, 'PrivKeyType', GetEnumName (TypeInfo (TSslPrivKeyType), Ord(FPrivKeyType))) ;
        WriteString (section, 'ProductCertType', FProductCertType) ;
        WriteString (section, 'ProductDVAuth', FProductDVAuth) ;
        WriteString (section, 'ProductFeatures', FProductFeatures) ;
        WriteInteger (section, 'ProductMaxSan', FProductMaxSan) ;
        WriteString (section, 'ProductQuote', FProductQuote) ;
        WriteString (section, 'SuppCertChallenge', GetEnumName (TypeInfo (TChallengeType), Ord(FSuppCertChallenge)));
        WriteString (section, 'SuppOrderId', FSuppOrderId) ;
        WriteString (section, 'SuppOrderRef', FSuppOrderRef) ;
        WriteString (section, 'SuppCertProduct', FSuppCertProduct) ;
        WriteString (section, 'ProductCA', FProductCA) ;
        WriteString (section, 'ChallgSrvIP', FChallgSrvIP) ;    { V8.65 }
        WriteString (section, 'ChallgSrvIP2', FChallgSrvIP2) ;  { V8.71 }
{$IFDEF MSCRYPT_Tools}
        WriteString (section, 'MsCertLoc', GetEnumName (TypeInfo (TMsCertLocation), Ord(FMsCertLoc)));  { V8.71 }
{$ENDIF}
        WriteString (section, 'CertAcmeProfile', FCertAcmeProfile);                 { V9.5 }
        if FAutoOrderComplete then temp := 'True' else temp := 'False' ; WriteString (section, 'AutoOrderComplete', temp) ;    { V9.5 }
        if FCertRenewNow then temp := 'True' else temp := 'False' ; WriteString (section, 'CertRenewNow', temp) ;    { V9.5 }
        WriteInteger (section, 'CertRenewDays', FCertRenewDays) ;                        { V9.5 }
        WriteString (section, 'CertRenewCheckDT', RFC3339_DateToStr(FCertRenewCheckDT)) ;  { V9.5 }
        WriteString (section, 'CertRenewRetryDT', RFC3339_DateToStr(FCertRenewRetryDT)) ;  { V9.5 }
        WriteString (section, 'CertRenewalId', FCertRenewalId) ;                           { V9.5 }
        WriteString (section, 'AcmeChallngReport', IcsEscapeCRLF(FAcmeChallngReport)) ;   { V9.5 }

    // write INI file
        FControlFile.UpdateFile;

    // erase any old SAN sections
        FControlFile.ReadSections(FDBIniSections);
        section := CntlDBSANPre + FCertCommonName + '=';
        for J := 0 to FDBIniSections.Count - 1 do begin
            if Pos(section, FDBIniSections[J]) = 1 then begin
                flag := False;
                if FCertSubAltNames.Count > 0 then begin
                    temp := Copy(FDBIniSections[J], Length(section), 999);
                    for I := 0 to FCertSubAltNames.Count - 1 do begin
                        if FCertSubAltNames[I].SADomain = temp then begin
                            flag := True;
                            break;
                        end;
                    end;
                end;
                if NOT flag then
                    FControlFile.EraseSection(FDBIniSections[J]);
            end;
        end;

    // save Subject Alternate Names
        temp := '';
        FCertSANs.Clear;
        if CertSubAltNames.Count > 0 then begin
        // save current SAN sections
            for I := 0 to FCertSubAltNames.Count - 1 do begin
                with FCertSubAltNames[I] do begin
                    if SADomain = '' then Continue; // sanity check
                    FCertSANs.Add(SADomain);
                    section := CntlDBSANPre + FCertCommonName + '=' + SADomain;
                    WriteString (section, 'Domain', SADomain) ;
                    WriteString (section, 'DirWellKnown', SADirWellKnown) ;
                    WriteString (section, 'DirPubWebCert', SADirPubWebCert) ;
                    WriteString (section, 'ApprovalEmail', SAApprovalEmail) ;
                    WriteString (section, 'SAIssueState', GetEnumName (TypeInfo (TIssueState), Ord(SAIssueState))) ;
                    WriteString (section, 'SAStartDT', RFC3339_DateToStr(SAStartDT)) ;
                    WriteString (section, 'SADoneDT', RFC3339_DateToStr(SADoneDT)) ;
                    WriteString (section, 'SAValidResult', SAValidResult) ;
                end;
            end;
        end;
        section := CntlDBDomainPre + FCertCommonName;
        WriteString (section, 'CertSubAltList', FCertSANs.CommaText) ;
    end;
    FControlFile.UpdateFile;  // write INI file
    DBReadSections;
    if Assigned(FOnDomainsRefresh) then
        FOnDomainsRefresh(Self);
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// V9.5 update renewal infomation dates, perhaps daily
function TSslX509Certs.DBWriteCNDomDates: Boolean;          { V9.5 }
var
    section, temp: string;
begin
    Result := False;
    if FCertCommonName = ''  then begin         { V8.71 }
        LogEvent('Can not save domain with blank Common Name') ;
        Exit;
    end;
    if NOT DBOpenINI(FDirCertWork, False) then begin
        LogEvent('Failed to Open Account Database: ' + FDirCertWork);
        Exit;
    end;
    section := CntlDBDomainPre + FCertCommonName;
    with FControlFile do begin
        WriteString (section, 'IssueState', GetEnumName (TypeInfo (TIssueState), Ord(FIssueState)));
        WriteString (section, 'AcmeOrderObjUrl', FAcmeOrderObjUrl);
        WriteString (section, 'ChallgExpireDT', RFC3339_DateToStr(FChallgExpireDT));
        WriteInteger (section, 'CertRenewDays', FCertRenewDays) ;
        if FCertRenewNow then temp := 'True' else temp := 'False' ; WriteString (section, 'CertRenewNow', temp) ;
        WriteString (section, 'CertRenewCheckDT', RFC3339_DateToStr(FCertRenewCheckDT)) ;
        WriteString (section, 'CertRenewRetryDT', RFC3339_DateToStr(FCertRenewRetryDT)) ;
        WriteString (section, 'CertRenewalId', FCertRenewalId) ;
        WriteString (section, 'NewCertEndDT', RFC3339_DateToStr(FNewCertEndDT)) ;
        WriteString (section, 'NewCertStartDT', RFC3339_DateToStr(FNewCertStartDT)) ;
        UpdateFile;
        Result := True;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// delete certificate common name domain, perhaps with sub domains as well
function TSslX509Certs.DBDeleteCNDomain(const CNDomain: String): Boolean;
var
    section: string;
    J: Integer;
begin
    Result := False;
    if NOT DBOpenINI(FDirCertWork, False) then
        Exit;
    section := CntlDBDomainPre + CNDomain;
    if FControlFile.SectionExists(section) then
               FControlFile.EraseSection(section);

  // erase any SAN sections
    FControlFile.ReadSections(FDBIniSections);
    section := IcsLowerCase(CntlDBSANPre + CNDomain + '=');
    for J := 0 to FDBIniSections.Count - 1 do begin
        if Pos(section, FDBIniSections[J]) = 1 then begin
             FControlFile.EraseSection(FDBIniSections[J]);
        end;
    end;
    FControlFile.UpdateFile;  // write INI file
    DBReadSections;
    if Assigned(FOnDomainsRefresh) then FOnDomainsRefresh(Self);
    Result := True;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// find SAB record for domain, allowing for wildcards
function TSslX509Certs.DBFindSAN(const adomain: String): Integer;
var
    I: Integer;
begin
    Result := -1;
    for I := 0 to FCertSubAltNames.Count - 1 do begin
        if (adomain = FCertSubAltNames[I].SADomain) then begin
            Result := I;
            Exit;
        end
     // wild card removes *. from domain
        else if (Pos(adomain, FCertSubAltNames[I].SADomain) > 1) and (Pos ('*.', FCertSubAltNames[I].SADomain) = 1) then begin
            Result := I;
            Exit;
        end;
     end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.DBFindChallengeNum(const Domain: String; Wildcard: Boolean): Integer;
var
    I: Integer;
begin
    Result := -1;
    if FChallengesTot = 0 then
        Exit;
    for I := 0 to Length(FChallengeItems) - 1 do begin
        if (FChallengeItems [I].CDomain = IcsLowerCase(Domain)) and (FChallengeItems [I].CWildcard = Wildcard) then begin  { V8.64 }
            Result := I;
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// find first blank challenge in array, or increase array size
function TSslX509Certs.DBFreeChallengeNum: Integer;
var
    I, tot: Integer;
begin
    tot := Length(FChallengeItems);
    if tot > 0 then begin
        for I := 0 to Length(FChallengeItems) - 1 do begin
            if FChallengeItems [I].CDomain = '' then begin
                Result := I;
                Exit;
            end;
        end;
    end;
    SetLength(FChallengeItems, tot + 8);
    for I := tot to Length(FChallengeItems) - 1 do
        FChallengeItems [I].CDomain := '';    // clear all records
    Result := tot;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.DBReadChallenges: Boolean;
var
    section: string;
    I, Days: Integer;
    StartedDT, ExpiredDT: TDateTime;
    Updated: Boolean;
    ChallgType: TChallengeType;
begin
    Result := False;
    if NOT DBReadSections then
        Exit;
    FChallengesTot := 0;
    Updated := False;
    SetLength(FChallengeItems, FDBIniSections.Count + 2);
    for I := 0 to Length(FChallengeItems) - 1 do
        FChallengeItems [I].CDomain := '';    // clear all records
    for I := 0 to FDBIniSections.Count - 1 do begin
        if Pos(CntlDBChallenge, FDBIniSections[I]) = 1 then begin
            section := FDBIniSections[I];
            with FControlFile do begin

              { V8.63 expire and remove challenge after 24 hours or a week }
                StartedDT := RFC3339_StrToDate(ReadString (section, 'CStartDT', '')) ;
                ExpiredDT := RFC3339_StrToDate(ReadString (section, 'CExpireDT', '')) ; // V8.64
                ChallgType := TChallengeType(GetEnumValue (TypeInfo (TChallengeType), ReadString (section, 'CType', ''))) ;
                if ChallgType > High(TChallengeType) then ChallgType := ChallNone;
                if ExpiredDT < 10 then begin
                    Days := 1;
                    if (ChallgType in [ChallDNSAuto, ChallDnsAcnt, ChallDnsMan, ChallEmail, ChallManual]) then Days := 7;
                    ExpiredDT := StartedDT + Days;
                 end;
                if ((ExpiredDT + 1) < Now) then begin   // remove out of date challenge, V9.5 add one day for time zone problems
                    LogEvent ('Removing expired challenge: ' + section + ', Expired: ' + IcsDateTimeToAStr(ExpiredDT)); { V9.5 add date }
                    EraseSection(section);
                    Updated := True;
                    Continue;
                end;
                with FChallengeItems [FChallengesTot] do begin
                    CDomain := ReadString (section, 'CDomain', '') ;
                    CWildcard := ReadBool (section, 'CWildcard', False) ;
                    CCommonName := ReadString (section, 'CCommonName', '') ;
                    CSanIdx := ReadInteger (section, 'CSanIdx', -1) ;
                    CSuppOrderId := ReadString (section, 'CSuppOrderId', '') ;
                    CDirWellKnown := ReadString (section, 'CDirWellKnown', '') ;
                    CDirPubWebCert := ReadString (section, 'CDirPubWebCert', '') ;
                    CWKFullName := ReadString (section, 'CWKFullName', '') ;
                    CSupplierProto := TSupplierProto(GetEnumValue (TypeInfo (TSupplierProto), ReadString (section, 'CSupplierProto', ''))) ;
                    CType := ChallgType;
                    CIssueState := TIssueState(GetEnumValue (TypeInfo (TIssueState), ReadString (section, 'CIssueState', ''))) ;
                    if CIssueState > High(TIssueState) then CIssueState := IssStateNone;
                    CAuthzURL := ReadString (section, 'CAuthzURL', '') ;
                    ChallgToken := IcsUnEscapeCRLF(ReadString (section, 'ChallgToken', '')) ;
                    ChallengeURL := ReadString (section, 'ChallengeURL', '') ;
                    CPage := ReadString (section, 'CPage', '') ;
                    CResp := IcsUnEscapeCRLF(ReadString (section, 'CResp', '')) ;
                    CDNSValue := ReadString (section, 'CDNSValue', '') ;
                    CAcmeAlpnCert := ReadString (section, 'CAcmeAlpnCert', '') ; // V8.62
                    CStartDT := StartedDT;
                    CExpireDT := ExpiredDT;
                    CDoneDT := RFC3339_StrToDate(ReadString (section, 'CDoneDT', '')) ;
                    CValidResult := ReadString (section, 'CValidResult', '') ;
                end;
                FChallengesTot := FChallengesTot + 1;
            end;
        end;
    end;
    LogEvent('Number of domain challenges found: ' + IntToStr(FChallengesTot));
    if Updated then
        FControlFile.UpdateFile;  // write INI file
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// add or update challenge in array
function TSslX509Certs.DBAddChallenge(Item: TChallengeItem): Integer;
begin
    Result := DBFindChallengeNum(Item.CDomain, Item.CWildcard);
    if Result < 0 then begin
        Result := DBFreeChallengeNum;
        FChallengesTot := FChallengesTot + 1;
    end;
    FChallengeItems [Result] := Item;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// reemove challenge from array by clearing domain
function TSslX509Certs.DBRemoveChallenge(ChallgNum: Integer): Boolean;
begin
    Result := False;
    if (ChallgNum >= 0) and (ChallgNum < Length(FChallengeItems)) then begin
        FChallengeItems [ChallgNum].CDomain := '';
        FChallengesTot := FChallengesTot - 1;
        Result := True;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.DBWriteOneChallenge(Item: TChallengeItem): Integer;
var
    section: string;
begin
    Result := -1;
    if NOT DBOpenINI(FDirCertWork, False) then
        Exit;
    with Item do begin
        CDomain := IcsLowerCase(CDomain);
        CCommonName := IcsLowerCase(CCommonName);
        if CWildcard then    { V8.64 may have separate challenges for wild and host domains }
            section := CntlDBChallenge + 'wild-' + CDomain
        else
            section := CntlDBChallenge + 'host-' + CDomain;
        with FControlFile do begin
            WriteString (section, 'CDomain', CDomain) ;
            WriteBool (section, 'CWildcard', CWildcard) ;
            WriteString (section, 'CCommonName', CCommonName) ;
            WriteInteger (section, 'CSanIdx', CSanIdx) ;
            WriteString (section, 'CSuppOrderId', CSuppOrderId) ;
            WriteString (section, 'CDirWellKnown', CDirWellKnown) ;
            WriteString (section, 'CDirPubWebCert', CDirPubWebCert) ;
            WriteString (section, 'CWKFullName', CWKFullName) ;
            WriteString (section, 'CSupplierProto', GetEnumName (TypeInfo (TSupplierProto), Ord(CSupplierProto))) ;
            WriteString (section, 'CType', GetEnumName (TypeInfo (TChallengeType),Ord(CType))) ;
            WriteString (section, 'CIssueState', GetEnumName (TypeInfo (TIssueState),Ord(CIssueState))) ;
            WriteString (section, 'CAuthzURL', CAuthzURL) ;
            WriteString (section, 'ChallgToken', IcsEscapeCRLF(ChallgToken)) ;
            WriteString (section, 'ChallengeURL', ChallengeURL) ;
            WriteString (section, 'CPage', CPage) ;
            WriteString (section, 'CResp', IcsEscapeCRLF(CResp)) ;
            WriteString (section, 'CDNSValue', CDNSValue) ;
            WriteString (section, 'CAcmeAlpnCert', CAcmeAlpnCert) ; // V8.62
            if CStartDT < 10 then CStartDT := Now; // sanity check, needed to expire record
            WriteString (section, 'CStartDT', RFC3339_DateToStr(CStartDT)) ;
            WriteString (section, 'CDoneDT', RFC3339_DateToStr(CDoneDT)) ;
            WriteString (section, 'CExpireDT', RFC3339_DateToStr(CExpireDT)) ;  // V8.64
            WriteString (section, 'CValidResult', CValidResult) ;
        end;
    end;
    FControlFile.UpdateFile;  // write INI file

 // now update array
    Result := DBAddChallenge(Item);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.DBDeleteChallenge(const Domain: String; Wildcard: Boolean): Boolean;
var
    section: string;
begin
    Result := False;
    if NOT DBOpenINI(FDirCertWork, False) then
        Exit;
    if WildCard then    { V8.64 may have separate challenges for wild and host domains }
        section := CntlDBChallenge + 'wild-' + Domain
    else
        section := CntlDBChallenge + 'host-' + Domain;
    if NOT FControlFile.SectionExists(section) then
        Exit;
    FControlFile.EraseSection(section);
    FControlFile.UpdateFile;  // write INI file
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.BuildSANList;
var
    flag: Boolean;
    I: Integer;
    PubDir: string;
begin
 // make sure common name is also in SANs, so we can ignore it subsequently
    flag := false;
    if FCertSubAltNames.Count > 0 then begin
        for I := 0 to FCertSubAltNames.Count - 1 do begin
            if FCertSubAltNames[I].SADomain <> fCertCommonName then flag := True;
        end;
    end;
    if NOT flag then begin
        PubDir := '';
        if FDirPubWebCert.Count > 0 then PubDir := FDirPubWebCert[0];
        FCertSubAltNames.AddItem(fCertCommonName, FDirWellKnown, PubDir, FCertApprovEmail);
    end;
    FCertSANTot := FCertSubAltNames.Count;
    FCertSANs.Clear;
    for  I := 0 to FCertSANTot - 1 do
        FCertSANs.Add(FCertSubAltNames[I].SADomain);
 end;


 {* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// set partial directory names for work and final certificate dictorties
function TSslX509Certs.SetPartFNames(ReadOnly: Boolean = False): Boolean;
var
    CN, NewDir: string;
    I: Integer;
begin
    FPartFNameWork := '';
    FPartFNameFinal := '';
    FPartFNameOrder := '';
    FPartFNameServer.Clear;
    FPartFNameServer.Sorted := True;
    FPartFNameServer.Duplicates := dupIgnore;
    FPartFNameServer.CaseSensitive := false;
    Result := False ;

  // check for domain  V9.1 check before coming here
  {  if Pos ('.', FCertCommonName) = 0 then begin
        LogEvent ('Must specify host domain name');
        exit;
    end;  }

  // V9.1 fatal error
    if FDirCertWork = '' then begin
        FLastError := 'No certificate work directory set to build file names';
        LogEvent(FLastError);
        exit;
    end;

  // work dir where we will save our certificates and keys
    if NOT ReadOnly then begin
        if NOT ForceDirectories (FDirCertWork) then begin
            FLastError := 'Failed to create directory: ' + FDirCertWork;
            LogEvent(FLastError);
            exit;
        end;
    end;

  // V9.1 fatal error
    if NOT IcsDirExists(FDirCertWork) then begin
        FLastError := 'Certificate order work directory not found: ' + FDirCertWork;
        LogEvent(FLastError);
        exit;
    end;

  // create certificate file name from domain common name, change . to _ and * to x
    CN := IcsBuildCertFName(FCertCommonName) ;

    FPartFNameWork := IncludeTrailingPathDelimiter(FDirCertWork) + FNewCertPrefix + 'work-' + CN ;
    if fSuppOrderId <> '' then begin
        FPartFNameOrder := IncludeTrailingPathDelimiter(FDirCertWork) + FNewCertPrefix + FSuppOrderId + '-' + CN ;
        FPartFNameWork := FPartFNameOrder;
    end;
    fPartFNameFinal := IncludeTrailingPathDelimiter(fDirCertWork) + CN ;

  // build list of remote web server certificate locations, ignoing duplicates
  // also check work directory and remote server are not the same
    if (FDirPubWebCert.Count > 0) then begin
        for I := 0 to FDirPubWebCert.Count - 1 do begin
            NewDir := FDirPubWebCert[I];
            if (Pos('\\', NewDir) = 1) or (Pos (':', NewDir) = 2) then begin  { V8.65 sanity check }
                NewDir := IncludeTrailingPathDelimiter(NewDir);
                FDirPubWebCert[I] := NewDir;
                if NewDir <> fPartFNameFinal then
                    FPartFNameServer.Add(NewDir);
            end;
        end;
    end;
    if (FCertSubAltNames.Count > 0) then begin
        for I := 0 to FCertSubAltNames.Count - 1 do begin
            NewDir := FCertSubAltNames[I].SADirPubWebCert;
            if (Pos('\\', NewDir) = 1) or (Pos (':', NewDir) = 2) then begin  { V8.65 sanity check }
                NewDir := IncludeTrailingPathDelimiter(NewDir);
                if NewDir <> fPartFNameFinal then
                    FPartFNameServer.Add(NewDir);
            end;
        end;
    end;

 // see if creating remote directories
    if (NOT ReadOnly) and (FPartFNameServer.Count > 0) then begin
        for I := 0 to FPartFNameServer.Count - 1 do begin
            if NOT ForceDirectories (FPartFNameServer[I]) then begin
                LogEvent ('Failed to create certificate directory: ' + FPartFNameServer[I]);
                Exit;
            end;
        end;
    end;

 // finally add common domain name
    FPartFNameServer.Sorted := False;  // now editing list
    if (FPartFNameServer.Count > 0) then begin
        for I := 0 to FPartFNameServer.Count - 1 do begin
            FPartFNameServer[I] := IncludeTrailingPathDelimiter(FPartFNameServer[I]) + CN ;
        end;
    end;
    Result := True ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.SetFullFileNames (const FileDir: String) ;
begin
    FFileCSR := FileDir + FileSuffCSR ;
    FFilePrvKey := FileDir + FileSuffPKey ;
    FFileCertPem := FileDir + FileSuffCertPem ;
    FFileInterPem := FileDir + FileSuffInterPem ;
    FFileBundPem := FileDir + FileSuffBundPem ;
    FFileBundP12 := FileDir + FileSuffBundP12 ;
    FFileBundP7 := FileDir + FileSuffBundP7 ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// create new private key and certificate signing request
function TSslX509Certs.CreateKeyandReq(IsCA: Boolean = False): boolean;     { V9.1 add CA }
var
    I: Integer;
    SocFamily: TSocketFamily;  { V9.5 }
begin
    Result := False ;

 // see if using old CSR
    if FCertCsrOrigin = CsrOriginFile then begin
        if NOT FNewSslCert.IsReqLoaded then begin
            LogEvent ('No old certificate request loaded: ' + FCertOldCsrFile);
            Exit;
        end;
        if NOT FNewSslCert.IsPKeyLoaded then begin
            LogEvent ('No old private key loaded: ' + FCertOldPrvKey);
            Exit;
        end;
    end
    else begin
        if (fCertCommonName = '') { or (FCertSubAltNames.Count = 0)} then begin     { V9.1 optional }
            LogEvent ('No domain name for request');
            Exit;
        end;
        LogEvent ('Generating private and public key pair, please wait');
        try
            FNewSslCert.X509Req := Nil;   { V8.64 before key creation }
            if FPrivKeyType < PrivKeyRsa2048 then
                FPrivKeyType := PrivKeyRsa2048; { V8.64 sanity check }
            FNewSslCert.PrivKeyType := FPrivKeyType;
            FNewSslCert.PrivateKey := Nil;
            FNewSslCert.DoKeyPair;
            if FNewSslCert.IsPKeyLoaded then  { V8.64 check actually created }
                LogEvent ('Generated private and public key pair OK:' + IcsCRLF + FNewSslCert.PrivateKeyInfo)
            else begin
                LogEvent ('Failed to generate private key - bad key parameters?');
                exit ;
            end;
        except
            on E:Exception do  begin
                LogEvent ('Failed to generate private key - ' + E.Message);
                exit ;
            end;
        end;

        LogEvent ('Generating certificate signing request');
        try
            with fNewSslCert do begin
                if WSocketIsIP(fCertCommonName, SocFamily) then  { V9.5 cmmon name not allowed IP address }
                    CommonName := ''
                else
                    CommonName := fCertCommonName;
                AltDNSList.Clear;
                AltIpList.Clear;
                if FCertSubAltNames.Count > 0 then begin   { V9.1 optional }
                    for I := 0 to FCertSubAltNames.Count - 1 do begin
                        if WSocketIsIP(FCertSubAltNames[I].SADomain, SocFamily) then  { V9.5 allow IP addresses }
                            AltIpList.Add(FCertSubAltNames[I].SADomain)
                        else
                            AltDNSList.Add(FCertSubAltNames[I].SADomain);
                    end;
                end;
                CertDigest := fCertSignDigestType ;
                Country := FCertCountry;
                State := FCertState;
                Locality := FCertLocality;
                Organization := FCertOrganization;
                OrgUnit := FCertOrgUnit;
                Descr := FCertDescr;
                Email := FCertContactEmail;

            // V9.1 set extensions
                BasicIsCA := IsCA;              { V9.1 }
                ExpireDays := FCertValidity;
                BasicPathLen := 0;
                KeyCertSign := False;
                KeyCRLSign := False;
                KeyDigiSign := False;
                KeyDataEnc := False;
                KeyKeyEnc  := True;
                KeyKeyAgree := False;
                KeyNonRepud  := False;
                KeyExtClient  := False;
                KeyExtServer  := True;       { V9.1 make sure IDN processed }
                KeyExtEmail  := False;
                KeyExtCode := False;
            end;
            FNewSslCert.DoCertReqProps;
            LogEvent('Created certificate signing request OK:' + IcsCRLF + FNewSslCert.ReqCertInfo);
        except
            on E:Exception do begin
                LogEvent ('Failed to generate CSR - ' + E.Message);
                exit ;
            end;
        end;
    end;
    try
        FNewSslCert.PrivateKeySaveToPemFile (fFilePrvKey, FPrivKeyPassword, FPrivKeyCipher) ;
        fPrvKeyLines := FNewSslCert.SavePKeyToText (FPrivKeyPassword, FPrivKeyCipher);
        LogEvent ('Saved private key file with ' + SslPrivKeyCipherLits[TSslPrivKeyCipher(FPrivKeyCipher)] +
                                                                                 ' key cipher: ' + FFilePrvKey); { V8.67 }
        if FLogPkeys then LogEvent (IcsCRLF + fPrvKeyLines + IcsCRLF) ;
        FNewSslCert.SaveReqToFile(fFileCSR, true);
        fCSRLines := FNewSslCert.SaveReqToText (false) ;  // no comments, confused order
        LogEvent ('Saved certificate signing request file: ' + fFileCSR + IcsCRLF + IcsCRLF + fCSRLines + IcsCRLF) ;
    except
        on E:Exception do begin
            LogEvent ('Failed to save CSR or key - ' + E.Message);
            exit ;
        end;
    end;
    Result := true;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// check old CSR file and old private key files, keep common name and SANs
function TSslX509Certs.CheckCSR(RequirePkey: Boolean = True): Boolean;
var
    ReqSANs: TStringList;
    PubDir: String;
    I: Integer;
begin
    Result := False;
    if (FCertOldCsrFile = '') or (NOT FileExists(FCertOldCsrFile)) then begin
        LogEvent ('Can not find old certificate signing request file: ' + FCertOldCsrFile);
        Exit;
    end;
    if (FCertOldPrvKey = '') or (NOT FileExists(FCertOldPrvKey)) then begin
        LogEvent ('Can not find old private key gile: ' + FCertOldPrvKey);
        if RequirePkey then
            Exit;
    end;
    try
        FNewSslCert.LoadReqFromFile(FCertOldCsrFile);
        if NOT FNewSslCert.IsReqLoaded then begin
            LogEvent ('Failed to load CSR file: ' + FCertOldCsrFile);
            Exit;
        end;
        LogEvent('Loaded certificate request OK' + IcsCRLF + FNewSslCert.ReqCertInfo);
        if (FCertOldPrvKey <> '') then begin
            try
                FNewSslCert.PrivateKeyLoadFromPemFile (FCertOldPrvKey, FPrivKeyPassword) ;
            except
                on E:Exception do begin
                    LogEvent ('Failed to load private key file: ' + E.Message);
                end;
            end;
            if NOT FNewSslCert.IsPKeyLoaded then begin
                LogEvent ('Failed to load old private key file: ' + FCertOldPrvKey);
                if RequirePkey then
                    Exit;
            end;
        end;
        FCertCommonName := FNewSslCert.ReqSubjCName;
        FCertSubAltNames.Clear;
        PubDir := '';
        if FDirPubWebCert.Count > 0 then PubDir := FDirPubWebCert[0];
        FCertSubAltNames.AddItem(FCertCommonName, FDirWellKnown, PubDir);
        ReqSANs := TStringList.Create;
        try
            ReqSANs.Text := FNewSslCert.ReqSubjAltNameDNS;
            if ReqSANs.Count > 0 then begin
                for I := 0 to ReqSANs.Count - 1 do begin
                    if (ReqSANs[I] <> FCertCommonName) then
                        FCertSubAltNames.AddItem(ReqSANs[I], FDirWellKnown, PubDir);
                end;
            end;
        finally
            ReqSANs.Free;
        end;
    except
        on E:Exception do begin
            LogEvent ('Failed to Load CSR File: ' + E.Message);
        end;
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 get certificate info for Acme renewal information and ordernew requests }
{ built from certificate AuthorityKeyId and SerialNum }
function TSslX509Certs.GetAcmeRenewalInfo(Cert: TX509Base): String;

    function TrimB64(const Value: String): String;  // remove base64 padding = from end
    var
        Len: Integer;
    begin
        Result := Value;
        while true do begin
            len := Length(Result);
            if (len <= 1) then
                Exit;
            if Result[len] <> '=' then
                Exit;
            SetLength(Result, len - 1);
        end;
    end;

begin
    Result := '';
    if NOT Assigned(Cert) then
        Exit;
//    LogEvent ('AcmeRenewalInfo, Auth=' + Cert.AuthorityKeyId + ', Serial=' + Cert.SerialNumHex); // !!! TEMP DIAG
    Result := TrimB64(IcsBase64UrlEncodeTB(IcsHexToTB(AnsiString(Cert.AuthorityKeyId)))) + '.' +
                                                                    TrimB64(IcsBase64UrlEncodeTB(Cert.SerialNumTB));
    LogEvent ('AcmeRenewalInfo, Combined=' + Result);     // !!! TEMP DIAG
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ saves up to three copies of all certificate files:
    1 - work directory, includes unique order number for historic purposes
    2 - work directory, final cert name without order, replacing older version
    3 - server directory, final cert name without order, replacing older version
  Also validates certificate chain to ensure intermediate matches and CA available
  Returns false on failure, with LastError
}
function TSslX509Certs.SaveCertificateFiles(const CertName: string): Boolean;
var
    P12KeyCipher, PemKeyCipher: TSslPrivKeyCipher;
    P12Password, FDir, CName, IName, SAName{, FileInter}: String;
    I, J: Integer;
    X509List: TX509List;  { V8.67 }
    SIPs: String;

    function TestBundle(const FName, PW: String): Boolean;    { V8.67 check we can open bundle }
    var
        TestCert: TX509Base;
    begin
        Result := False;
        TestCert := TX509Base.Create(Nil);
        try
            try
                TestCert.LoadFromFile(FName, croTry, croTry, PW);
                Result := True;
            except
                on E:Exception do begin
                    LogEvent ('Failed to open certificate bundle: ' + E.Message + ' - ' + FName);
                end;
            end;
        finally
            TestCert.Free;
        end;

    end;

    function SaveAllCerts: Boolean;
    begin
        Result := False;
        try
            if (FCSRLines <> '') and (OutFmtReq in FCertOutFmts) then begin
                SaveDataFile (FFileCSR, FCSRLines);
                LogEvent('Saved certificate request file: ' + FFileCSR);
            end;
            if (OutFmtSep in FCertOutFmts) then begin
                FNewSslCert.PrivateKeySaveToPemFile (FFilePrvKey, FPrivKeyPassword, PemKeyCipher);
                LogEvent ('Saved private key file with ' +
                                    SslPrivKeyCipherLits[TSslPrivKeyCipher(PemKeyCipher)] + ' key cipher: ' + FFilePrvKey);  { V8.67 }
                FNewSslCert.SaveToPemFile(FFileCertPem, False, True, False);  // no private key or inters
                LogEvent('Saved PEM certificate alone: ' + FFileCertPem);
                if FNewSslCert.IsInterLoaded then begin
                    FNewSslCert.SaveIntersToToPemFile(FFileInterPem, True);
                    LogEvent('Saved PEM intermediate certificate: ' + FFileInterPem);
                end;
            end;
            if (OutFmtBudl in FCertOutFmts) then begin
                FNewSslCert.SaveToPemFile(FFileBundPem, True, True, FNewSslCert.IsInterLoaded,  { V8.64 }
                                                                 FPrivKeyPassword, PemKeyCipher);  // add private key and inters
                LogEvent('Saved PEM bundle with certificate, key and intermediate with ' +
                                    SslPrivKeyCipherLits[TSslPrivKeyCipher(PemKeyCipher)] + ' key cipher: ' + FFileBundPem);   { V8.67 }
            end;
            if (OutFmtP12 in FCertOutFmts) then begin
                FNewSslCert.SaveToP12File(fFileBundP12, P12Password, FNewSslCert.IsInterLoaded, P12KeyCipher); { V8.64 add private key and inters }
                LogEvent('Saved PKCS12 bundle with certificate, key and intermediate with ' +
                                    SslPrivKeyCipherLits[TSslPrivKeyCipher(P12KeyCipher)] + ' key cipher: ' + FFileBundP12);   { V8.67 }
            end;
            if (FNewCertP7Lines <> '') and (OutFmtP7 in FCertOutFmts) then begin
                SaveDataFile (FFileBundP7, FNewCertP7Lines) ;
                LogEvent('Saved PKCS7 bundle: ' + FFileBundP7);
            end;
            Result := True;
        except
            on E:Exception do begin
                FLastError := 'Failed to save file - ' + E.Message;
                LogEvent(FLastError);
            end;
        end;
    end;

begin
    Result := False;
    FLastError := '';   { V9.5 }
    LogTimeStamp;
    LogEvent ('Saving SSL certificate files for: ' + CertName);
    if FPartFNameOrder = '' then begin
        FLastError := 'Can not save certificate files without directory';
        LogEvent(FLastError);
        Exit ;
    end;
    SetFullFileNames (FPartFNameOrder);
    if (Pos(PEM_STRING_HDR_BEGIN, FNewCertLines) = 0) then begin
        FLastError := 'Did not teceive a valid PEM certificate';
        LogEvent(FLastError);
        Exit ;
    end;
    ClearRenewals;    { V9.5 clear old cert and renewal stuff }

 // sanity check, must save some files
    if NOT ((OutFmtSep in FCertOutFmts) OR (OutFmtBudl in
                            FCertOutFmts) OR (OutFmtP12 in FCertOutFmts)) then
        FCertOutFmts := FCertOutFmts + [OutFmtSep];

 // Windows will not load PKCS12 file without a password, so create one if missing
    FPrivKeyPassword := Trim(FPrivKeyPassword);
    P12KeyCipher := PrivKeyEncNone;
    PemKeyCipher := PrivKeyEncNone;
    if (OutFmtP12 in FCertOutFmts) and (OutFmtPwP12 in FCertOutFmts) then begin  { V8.67 now configurable, but defaults true }
        P12KeyCipher := FPrivKeyCipher;
        P12Password := FPrivKeyPassword;
        if (P12Password = '') then begin     { V8.67 simplified }
            P12Password := 'password';
            LogEvent('Set required PKCS12 file password to "password"');
        end;
        if (P12KeyCipher = PrivKeyEncNone) then
            P12KeyCipher := PrivKeyEncAES256;
     { V8.67 is 3DES available, prefer that so older versions of Windows will load our PFX file }
     { V9.3 no longer prefer TripleDES, too old }
      //  if (ICS_OPENSSL_VERSION_MAJOR < 3) or ICS_OSSL3_LOADED_LEGACY then
      //      P12KeyCipher := PrivKeyEncTripleDES
        if (P12KeyCipher = PrivKeyEncTripleDES) and (NOT ICS_OSSL3_LOADED_LEGACY) then   { V9.4 change 3DES if not available }
            P12KeyCipher := PrivKeyEncAES256;
    end;

 { V8.67 see if protecting PEM keys }
 { note: Apache web server can not use encrypted private keys }
    if (OutFmtPwPem in FCertOutFmts) and (FPrivKeyPassword <> '') then begin
        PemKeyCipher := FPrivKeyCipher;
    end;

// V8.67 load all downloaded certificates from CertLines and check what we received
// build final bundle in FNewSslCert
    X509List := TX509List.Create(self);
    try
        try
            X509List.LoadAllFromString(FNewCertLines);
            if (X509List.Count = 0) then begin
                FLastError := 'Did not receive a valid PEM certificate';
                LogEvent(FLastError);
                exit ;
            end;
            for J := 0 to X509List.Count - 1 do begin
                CName := X509List[J].SubjectCName;
                if (CName = '') and (Length(X509List[J].ListNamesDNS) > 0) then  { V9.5 blank common name, use Alt DNS }
                    CName := X509List[J].ListNamesDNS[0];
                IName := X509List[J].IssuerCName;
                SAName := IcsUnwrapNames (X509List[J].SubAltNameDNS);

               { V9.5 certificate IP addresses, add to SAN list and common name if blank }
                SIPs := IcsUnwrapNames (X509List[J].SubAltNameIP);
                if SIPs <> '' then begin
                    if (CName = '') then
                        CName := SIPs;
                    if SAName = '' then
                        SAName := SIPs
                    else
                        SAName := SAName + ', ' + SIPs;
                end;

            // look for main certificate by common name
                LogEvent('Certificate ' + IntToStr(J + 1) + ', Common Name: ' + CName + ', SAN: ' + SAName + ', Issuer: ' + IName);
                if (CertName = CName) or (Pos (CertName, SAName) > 0) then begin
                    FNewCertCN := CName;
                    FNewCertSAN := SAName;
                    FNewSslCert.X509 := X509List[J].X509;
                    if NOT FNewSslCert.CheckCertAndPKey then
                        LogEvent ('!!! WARNING, private key does not match certificate public key')
                    else
                        LogEvent ('Found order certificate with matching private key, subject alt names (SAN): ' + FNewCertSAN);
                end

            // any others are intermediates, don't want any self signed root certificates
                else begin

                // V8.68 see if removing Lets Encrypt intermediate for expired root CA, needed for old Androids only
                    if NOT FKeepOldCA then begin
                        if IName = 'DST Root CA X3' then begin
                            LogEvent ('Ignored intermediate for expired root: ' + IName);
                            continue;
                        end;
                        if CName = 'GTS Root R1' then begin    { V9.5 Google root signed by Globalsign }
                            LogEvent ('Ignored intermediate for cross signed root: ' + CName);
                            continue;
                        end;
                    end;
                    if X509List[J].SelfSigned then
                        LogEvent ('!!! WARNING, Ignored self signed certificate: ' + CName)
                    else
                        FNewSslCert.AddToInters(X509List[J].X509);  // keep intermediate for our bundle
              end;
            end;
            if NOT FNewSslCert.IsCertLoaded then begin
                FLastError := 'Did not find certificate in downloaded PEM bundle: ' + CertName;
                LogEvent(FLastError);
                Exit;
            end;
        except
            on E:Exception do begin
                FLastError := 'Failed to recognise downloaded PEM bundle - ' + E.Message;
                LogEvent(FLastError);
                exit ;
             end;
        end;
    finally
        X509List.Free;
    end;

// save separate PEM file
    if (OutFmtSep in FCertOutFmts) then begin
       FNewSslCert.SaveToPemFile(FFileCertPem, False, True, False);  // no private key or inters
        LogEvent('Saved PEM certificate alone: ' + FFileCertPem);
        FFileFinalBundle := FFileCertPem; // in case no bundle specified
    end;

// keep issue dates
    FNewCertEndDT := FNewSslCert.ValidNotAfter;
    FNewCertStartDT := FNewSslCert.ValidNotBefore;
    FCertRenewDays := Trunc(FNewCertEndDT - FNewCertStartDT) div 3;  // V9.5 temporary in case renewal info fails

// V9.5 get renewal recommended dates from server
    FCertRenewalId := GetAcmeRenewalInfo(FNewSslCert);
    AcmeV2RenewalInfo;
    FCertRenewNow := False;

// V9.5 Buypass may need local intermediate - no longer supporting Acme
 {   if (NOT FNewSslCert.IsInterLoaded) and (FAcmeSupplier in [AcmeBuypass, AcmeBuypassTest]) then begin
        FileInter := IncludeTrailingPathDelimiter(GSSL_CERTS_DIR) + FileBuypassInter;
        if FileExists(FileInter) then begin
            LogEvent ('Adding local BuyPass intermediate certificate to bundle');
            try
                FNewSslCert.LoadIntersFromPemFile(FileInter);
            except
                on E:Exception do begin
                    LogEvent('Failed to add intermediate certificate - ' + E.Message);
                 end;
            end;
       end;
    end;      }

// OwnCA only, get intermediate
    if (NOT FNewSslCert.IsInterLoaded) and (FNewInterLines <> '') then begin   { V8.64 }
        LogEvent (IcsCRLF + 'PEM intermediate certificate' + IcsCRLF + FNewInterLines + IcsCRLF);
        if (Pos(PEM_STRING_HDR_BEGIN, FNewInterLines) = 0) then begin
            LogEvent ('Did not receive a valid PEM intermediate certificate: ' + FNewInterLines);
            FNewInterLines := '';
        end
        else begin
            try
                if FNewInterLines <> '' then
                    FNewSslCert.LoadIntersFromString(FNewInterLines);
            except
                on E:Exception do begin
                    LogEvent('Failed to recognise intermediate certificate - ' + E.Message);
                 end;
            end;
        end;
    end;

// save inter as separate file
    if FNewSslCert.IsInterLoaded and (OutFmtSep in FCertOutFmts) then begin
        try
            FNewSslCert.SaveIntersToToPemFile(FFileInterPem, True);
            LogEvent('Saved PEM intermediate certificate: ' + FFileInterPem);
        except
            on E:Exception do begin
                LogEvent ('Failed to recognise intermediate certificate - ' + E.Message);
             end;
        end;
    end;

// save PKCS7 file
    if (FNewCertP7Lines <> '') and (OutFmtP7 in FCertOutFmts) then begin
        LogEvent(IcsCRLF + 'PEM PKCS7 certificate' + IcsCRLF + FNewCertP7Lines + IcsCRLF) ;
        SaveDataFile(FFileBundP7, FNewCertP7Lines) ;
    end;

 // log certificate content
    LogEvent (IcsCRLF + 'Certificate Details: ' + IcsCRLF +  FNewSslCert.CertInfo(False) + IcsCRLF + IcsCRLF);

 // save PEM bundle file for Apache and PKCS12 bundle for Windows, both with key passworded
    try
        if (OutFmtBudl in FCertOutFmts) then begin
            FNewSslCert.SaveToPemFile(FFileBundPem, True, True, FNewSslCert.IsInterLoaded,  { V8.64 }
                                                            FPrivKeyPassword, PemKeyCipher);  // add private key and inters
            LogEvent('Saved PEM bundle with certificate, key and intermediate with ' +
                                    SslPrivKeyCipherLits[TSslPrivKeyCipher(PemKeyCipher)] + ' key cipher: ' + fFileBundPem);
            FFileFinalBundle := FFileBundPem;
            if NOT TestBundle(FFileBundPem, FPrivKeyPassword) then begin  { V8.67 check we can open bundle }
                FLastError := 'Failed to open new PEM bundle - ' + FFileBundPem;
                LogEvent(FLastError);
                Exit;
            end;
        end;

        if (OutFmtP12 in FCertOutFmts) then begin
            FNewSslCert.SaveToP12File(FFileBundP12, P12Password, FNewSslCert.IsInterLoaded, { V8.64 }
                                                                               P12KeyCipher);  // add private key and inters
            LogEvent('Saved PKCS12 bundle with certificate, key and intermediate with ' +
                                    SslPrivKeyCipherLits[TSslPrivKeyCipher(P12KeyCipher)] + ' key cipher:  ' + FFileBundP12);
            if NOT (OutFmtBudl in FCertOutFmts) then    { V8.69 prioritise PEM over P12 }
                FFileFinalBundle := FFileBundP12;
            if NOT TestBundle(FFileBundP12, P12Password) then begin  { V8.67 check we can open bundle }
                FLastError := 'Failed to open new PKCS12 bundle - ' + FFileBundP12;
                LogEvent(FLastError);
                Exit;
            end;
        end;
    except
        on E:Exception do
        begin
            FLastError := 'Failed to save bundle - ' + E.Message;
            LogEvent(FLastError);
            Exit;
        end;
    end;

// now validate and report certificate chain
    try
        if NOT IcsSslRootCAStore.InitFlag then   { V9.1 if internal not loaded, do it }
            IcsSslRootCAStore.Initialise;
     { V8.47 warning, currently only checking first Host name }
        FNewCertValRes := FNewSslCert.ValidateCertChain(CertName, IcsSslRootCAStore, FNewCertChainInfo, FNewCertErrs, 30);
        if FNewCertValRes = chainOK then
            LogEvent ('SSL certificate chain validated OK: ' + IcsCRLF +  FNewCertChainInfo + IcsCRLF + IcsCRLF)
        else begin
            if FNewCertValRes = chainWarn then
                FNewCertErrs := 'Chain Warning - ' + FNewCertErrs
            else begin
                FNewCertErrs := 'Chain Failed - ' + FNewCertErrs;
                LogEvent ('SSL certificate errors - ' + FNewCertErrs + IcsCRLF + IcsCRLF);
             end;
        end;
        FNewCertChainInfo := IcsEscapeCRLF(FNewCertChainInfo);
     { V8.67 truncate chain info to avoid overloading DB INI file }
        if Length(FNewCertChainInfo) > 800 then     { V9.2 was 1000, now 800 }
            SetLength(FNewCertChainInfo, 800);
        FNewCertErrs := IcsEscapeCRLF(FNewCertErrs);
    except
        on E:Exception do begin
            FLastError := 'Failed to validate chain - ' + E.Message;
            LogEvent(FLastError);
            Exit;
        end;
    end;
    FFileFinalCSR := FFileCSR;
    FFileFinalPrvKey := FFilePrvKey;
    FFileFinalCert := FFileCertPem;

// finally save files again without order number, locally
    LogEvent ('Saving final versions of all files without order numbers locally');
    SetFullFileNames (FPartFNameFinal);
    if NOT SaveAllCerts then begin
        if NOT SaveAllCerts then  // two repeats
            Exit;
    end;

 // pending - did order succeed if some of this copying failed!!!!

// see if copying files to web server directories
    if (FPartFNameServer.Count > 0) then begin
        LogEvent ('Saving final versions of all files without order numbers on server');
        for I := 0 to FPartFNameServer.Count - 1 do begin
            FDir := FPartFNameServer[I];
            if (Pos ('\\', FDir) = 1) or (Pos (':', FDir) = 2) then begin  { V8.64 }
                SetFullFileNames (FDir);
                if NOT SaveAllCerts then begin
                    if NOT SaveAllCerts then begin
                        if NOT SaveAllCerts then  // two repeats
                            Exit;
                    end;
                end;
            end
            else
                LogEvent ('Skipped copying, bad directory: ' + FDir);
        end;
    end;
    LogEvent('Finished collecting and saving certificate for ' + CertName + IcsCRLF + IcsCRLF);

// V8.67 load into Windows Certificate Store for IIS
// V8.69 Windows store is only on Windows
{$IFDEF MSCRYPT_Tools}
    if (OutFmtWinStore in FCertOutFmts) then begin
        try
            if IcsIsProgAdmin or (FMsCertLoc =  MsLocCurUser) then begin   { V8.71 current user or local machine stores }
                FNewSslCert.SaveToStorePfx(FMsCertLoc, True, True);
                LogEvent('Installed certificate bundle ino Windows Certificate ' + MsCertLocTitles[FMsCertLoc] + ' Store OK');
             end
             else
                LogEvent('Program needs administrator rights to install bundle in Windows Certificate Local Machine Store');
        except
            on E:Exception do begin
                FLastError := 'Failed to install bundle in Windows Certificate ' + MsCertLocTitles[FMsCertLoc] + ' Store - ' + E.Message;
                LogEvent(FLastError);
                Exit;
            end;
        end;
    end;
{$ENDIF}

    Result := True;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function TSslX509Certs.RedistribteFiles: Boolean;
begin
    Result := False;
    if (fCertCommonName = '') then begin
        LogEvent('Must specify domain Common Name for certificate');
        Exit;
    end;

 // read internal variables and public properties from database, so same as order
    if NOT DBReadCNDomain(fCertCommonName) then
        Exit;
    if FIssueState < IssStateCollected then begin
        LogEvent('Certificate order must be completed first');
        Exit;
    end;
    LogEvent ('Redistributing old certificate');
    FNewSslCert.ClearAll;
    try
        if FFileFinalBundle <> '' then
            FNewSslCert.LoadFromFile(FFileFinalBundle, croTry, croTry, FPrivKeyPassword);
        if NOT (FNewSslCert.IsCertLoaded and FNewSslCert.IsPKeyLoaded) then begin
            LogEvent('Failed to read certificate bundle, trying aeparate files');
            FNewSslCert.LoadFromFile(FFileFinalCert, croTry, croTry, FPrivKeyPassword);
            FNewSslCert.PrivateKeyLoadFromPemFile(FFileFinalPrvKey, FPrivKeyPassword);
        end;
    except
        on E:Exception do begin
            LogEvent ('Failed to read old certificate - ' + E.Message);
            Exit;
        end;
    end;
    if NOT (FNewSslCert.IsCertLoaded and FNewSslCert.IsPKeyLoaded) then begin
        LogEvent ('Failed to read old certificate - ' + FFileFinalBundle);
        Exit;
    end;
    if FNewSslCert.ValidNotAfter < IcsGetUTCTime then begin  { V8.61 }
        LogEvent ('Old certificate has expired');
        Exit;
    end;
    fNewCertLines := FNewSslCert.SaveCertToText(False);

// save lots of certificates in different formats and places
    if NOT SaveCertificateFiles(fCertCommonName) then
        Exit;
    DBWriteCNDomain;
    if Assigned(FOnNewCert) then FOnNewCert(Self);
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.DumpJson(const Item: String = '');
begin
    if NOT FLogJson then Exit ;
    if FDebugLevel >= DebugBody then
        Exit; // already done it
    LogEvent('Response (length ' + IntToKbyte(Length(FHttpRest.ResponseRaw)) +
                                              ')' + IcsCRLF +  FHttpRest.ResponseRaw);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.AcmeGetRequest(HttpReq: THttpRequest; const FullURL: String; AcmeJson: String): boolean;   { V9.5 pass String not SO }
var
    I: integer;
    S, JsonWebSig: string;
begin
    result := false;
    if Pos ('https://', FullURL) <> 1 then  begin
        FLastError := 'AcmeGetRequest, Invalid URL: ' + FullURL;
        LogEvent(FLastError);
        exit;
    end;
    FHttpRest.ServerAuth := httpAuthNone;
    FHttpRest.DebugLevel := FDebugLevel;
    FHttpRest.Agent := 'ICS-ACME-' +  ComponentVersion; // V8.60
    FHttpRest.SocketFamily := FSocketFamily;        // V8.60 allow IPv6
    FHttpRest.ProxyURL := FProxyURL;                // V8.62 proxy support
    FHttpRest.AlpnProtocols.Clear;                  // V9.5 clear then add
    FHttpRest.AlpnProtocols.Add(ALPN_ID_HTTP11);    // V9.5
    if (Pos ('/new-cert', FullURL) > 1) or (Pos ('/cert/', FullURL) > 1) or (Pos ('issuer-cert', FullURL) > 1) then
        FHttpRest.Accept := 'application/pkix-cert'
     else
        FHttpRest.Accept := '*/*' ;
    FHttpRest.FollowRelocation := False;  // nonce will fail since unchanged
    FHttpRest.ContentTypePost := 'application/jose+json';
    fAcmeRespLocation := '';
    if (FHttpRest.HttpDownFileName <> '') then   { V8.68 are we downloading a file }
        FHttpRest.HttpMemStrategy := HttpStratFile
    else
        FHttpRest.HttpMemStrategy := HttpStratMem;
    FAccountLastTick := IcsGetTickCount64;  { V8.63 idle account timeout }
    try

      // Json parameters need to be signed by private key as a Json Web Signature
      // adding nonce from last request to prevent playback
        if HttpReq = httpPOST then begin
            if FDebugLevel >= DebugParams then
                LogEvent ('AcmeJson: ' + AcmeJson);

      // Acme v2 sends the public key once, which is stored on the server and then
      // a shorter Key Id (kid) sent in subsequent requests, with nonce and URL
            if fAcmeKwkKid = '' then
                JsonWebSig := IcsJoseJWSJson(fAcmeJoseAlg, AcmeJson, '', fAcmePrivKey.PrivateKey, '',
                                                                       fAcmeKwkPub, '', fAcmeRespNonce, FullURL, True)  { V9.5 ECDSA IEEE }
            else
                JsonWebSig := IcsJoseJWSJson(fAcmeJoseAlg, AcmeJson, '', fAcmePrivKey.PrivateKey, '',
                                                                           '', fAcmeKwkKid, fAcmeRespNonce, FullURL, True);  { V9.5 ECDSA IEEE }
            FHttpRest.RestParams.Clear;
            fAcmeLastStatus := FHttpRest.RestRequest(httpPOST, FullURL, false, JsonWebSig);
        end
        else
            fAcmeLastStatus := FHttpRest.RestRequest(HttpReq, FullURL, false, '');
    except
        on E:Exception do begin
            LogEvent('Failed to contact Acme Server: ' + E.Message);
            // don't exit, may still have something useful
        end;
    end;

 // pending, should be loop to retry  ???

    try
      { look for special Acme headers
        HTTP/1.1 409 Conflict
        Content-Type: application/problem+json
        Boulder-Requester: 5592135
        Location: https://acme-staging.api.letsencrypt.org/acme/reg/5592135
        Replay-Nonce: r1PKRhkQqI7GWwk1sGNwH_V5w2h0U7S0C44jq8vNhH0

        HTTP/1.1 201 Created
        Content-Type: application/json
        Boulder-Requester: 5592485
        Link: <https://acme-staging.api.letsencrypt.org/acme/new-authz>;rel="next"
        Link: <https://letsencrypt.org/documents/LE-SA-v1.2-November-15-2017.pdf>;rel="terms-of-service"
        Location: https://acme-staging.api.letsencrypt.org/acme/reg/5592485
        Replay-Nonce: JcprpXrX-4qNnubjMHWy9XjXcCi-B2xbvpS9spgJrqE    }

        if FHttpRest.RcvdHeader.Count > 0 then begin
            for I := 0 to Pred (FHttpRest.RcvdHeader.Count) do begin
                S := FHttpRest.RcvdHeader [I];
                if Pos ('Replay-Nonce: ', S) = 1 then
                    fAcmeRespNonce := Copy (S, 15, 999);
             // warning, may be two or more locations
                if Pos ('Link: ', S) = 1 then
                    fAcmeRespLink := Copy (S, 7, 999);
                if Pos ('Boulder-Requester: ', S) = 1 then
                    fAcmeRespRequester := Copy (S, 20, 999);  // account number ???
                if Pos ('Location: ', S) = 1 then
                    fAcmeRespLocation := Copy (S, 11, 999);   // account URL with account number, used as KID for other requests
                if Pos ('Content-Location: ', S) = 1 then
                    fAcmeRespContLoc := Copy (S, 19, 999);
            end;
        end;
        if HttpReq = httpHEAD then begin
            if (fAcmeLastStatus <> 200) and (fAcmeLastStatus <> 204) and (fAcmeLastStatus <> 405) then begin
                FLastError := 'Failed to contact Server, HEAD:' + FHttpRest.LastResponse ;
                LogEvent(FLastError);
            end
            else
                Result := true;  // got a new nonce, hopefully
            exit;
        end;

      { if binary DER for one certificate, convert it to PEM, should really break lines  }
       if (Pos('application/pkix-cert', FHttpRest.ContentType) = 1) then begin
            fAcmeCertLines := '';
            fAcmeCertLines := '-----BEGIN CERTIFICATE-----' + IcsCRLF +
                              String(IcsBase64EncodeA(FHttpRest.ResponseOctet)) + IcsCRLF +     { V9.4 }
                              '-----END CERTIFICATE-----' + IcsCRLF;
        end;

      { V2 provides multiple proper PEM certificates }
        if (Pos('application/pem-certificate-chain', FHttpRest.ContentType) = 1) then begin
            fAcmeCertLines := '';
            fAcmeCertLines := String(FHttpRest.ResponseOctet);
        end;
    except
        on E:Exception do begin
            LogEvent ('Failed to process response: ' + E.Message);
        end;
    end;
    result := true ;  // OK
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// find or create Acme account, get action URLs
function TSslX509Certs.SetAcmeAccount(CreateNew: Boolean = False): boolean;
var
    Proto, User, Pass, Port, Path : String;
    OldAccNum: String;
begin
    LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Get Account for: ' + FDirCertWork);
    Result := False;
    FAcmeAccountUrl := '';
    FAcmeAccountNum := '';
    if NOT DirectoryExists(FDirCertWork) and (NOT CreateNew) then begin
        LogEvent('Can not find account work directory: ' + FDirCertWork);
        Exit;
    end;
    if NOT DBReadAccount(FDirCertWork) then begin   { V9.5 read Acme account }
        if (NOT CreateNew) then begin
            LogEvent('Failed to read old account');
            Exit;
         end;

    // create account working directory
        LogEvent ('Checking account work directory: ' + FDirCertWork);
        if NOT ForceDirectories (FDirCertWork) then begin
            LogEvent ('Failed to create directory: ' + FDirCertWork);
            Exit;
        end;
        if NOT DBOpenINI(FDirCertWork, True) then   // allow to create a new account
            Exit;
        DBWriteAccount;
        LogEvent ('Created new account work directory: ' + FDirCertWork);
    end;

 // V9.5 check spplier server URL against table
    if FSupplierServer <> AcmeSupplierApiURLs[FAcmeSupplier] then begin
        LogEvent('Invalid certificate supplier server: ' + FSupplierServer);
        Exit;
    end;
    if (SupplierEmail <> '') and (Pos ('@', SupplierEmail) = 0) then begin   { V9.5 optional for Let's Encrypt }
        LogEvent('Must specify supplier email address');
        Exit;
    end;
    LogEvent('Opening ACME account');
    OldAccNum := FAcmeAccountNum;
    ParseURL(FSupplierServer, Proto, User, Pass, fAcmeHost, Port, Path);
    FAcmePubFName := FDirCertWork + 'AcmePublicKey.pem' ;
    FAcmePrivFName := FDirCertWork + 'AcmePrivateKey.pem' ;
    FAcmeKwkKid := '';
    FNewCertPrefix := AcmeSupplierPrefixes[FAcmeSupplier] ;   { V9.5 }
    FAcmeOrderFinalizeURL := '';
    FAcmeOrderObjUrl := '';
    FSuppOrderId := '';
    FAcmeRespNonce := '';
    if NOT AcmeLoadPKey(True) then
        Exit; // new account and new directory
    if NOT AcmeGetActions then
        Exit;

  // see if account already exists
    if (FAcmeAccountUrl <> '') and (FAcmeAccountNum <> '') then begin
        FAcmeKwkKid := FAcmeAccountUrl;  // this is our Kid for future requests
        LogEvent('Opening ACME account');
        Result := True;
    end
    else begin
        if SupplierProto = SuppProtoAcmeV2 then
            Result := AcmeV2NewAccount(OldAccNum <> '');  { V9.5 don't create new account if old one exists }
    end;
    if Result and (CreateNew or (FAcmeAccountNum  <> OldAccNum)) then  { V9.5 failsafe }
        Result := DBWriteAccount;
end;


 {* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// create or get Acme account private key for signing Json requests
// an DirCertWork is considered an account, one dir per account
// Not used for any certificates !!!!
function TSslX509Certs.AcmeLoadPKey(New: Boolean): Boolean;
begin
    Result := False;
  {  if FAcmeAccKeyType > PrivKeyRsa4096 then begin   // V8.64 allow testing EC keys
        LogEvent ('Sorry, Only RSA Private Keys Currently Supported for Acme Accounts');
        exit;
    end;   }

    try
    // get private keys, Acme prefers Elliptic Curve since shorter
        if FAcmeAccKeyType < PrivKeyRsa2048 then
            FAcmeAccKeyType := PrivKeyRsa2048;  { V8.64 sanity check }
        FAcmePrivKey.PrivKeyType := FAcmeAccKeyType;
        case FAcmeAccKeyType of
            PrivKeyRsa2048, PrivKeyRsa3072, PrivKeyRsa4096: FAcmeJoseAlg := jsigRsa256;
            PrivKeyECsecp256: FAcmeJoseAlg := jsigEcdsa256;
            PrivKeyECsecp384: FAcmeJoseAlg := jsigEcdsa384;
            PrivKeyECsecp512: FAcmeJoseAlg := jsigEcdsa512;
            PrivKeyRsaPss2048, PrivKeyRsaPss3072, PrivKeyRsaPss4096: FAcmeJoseAlg := jsigRsaPss256;
            PrivKeyEd25519: FAcmeJoseAlg := jsigEdDSA;
        end;
        FAcmePrivKey.PrivateKey := Nil;

        if (FileExists (FAcmePrivFName)) then begin  // load account private key
            try
                LogEvent ('Loading old private key file: ' + FAcmePrivFName);
                fAcmePrivKey.PrivateKeyLoadFromPemFile (FAcmePrivFName, '');
                LogEvent ('Loaded old private key OK: ' + FAcmePrivKey.PrivateKeyInfo);
            except
                on E:Exception do begin
                    LogEvent ('Exception loading private key: ' + E.Message + ' - ' + FAcmePrivFName);
                    exit;
                end;
            end;
        end
        else begin
            if NOT New then begin
                LogEvent ('Failed to find old private key: ' + FAcmePrivFName);
                exit;
            end;
            try
                FAcmePrivKey.DoKeyPair;
                if NOT FAcmePrivKey.IsPKeyLoaded then begin { V8.64 check actually created }
                    LogEvent ('Failed to generate private key - Bad parameters?');
                    exit ;
                end;
                LogEvent ('Generated private key OK: ' + FAcmePrivKey.PrivateKeyInfo);
                FAcmePrivKey.PrivateKeySaveToPemFile (FAcmePrivFName, '', PrivKeyEncNone);
                LogEvent ('Saved private key file: ' + FAcmePrivFName);
                FAcmePrivKey.PublicKeySaveToPemFile (FAcmePubFName);
                FPrvKeyLines := fAcmePrivKey.SavePKeyToText ('', PrivKeyEncNone);
                LogEvent ('Saved public key file: ' + FAcmePubFName);
                if FLogPkeys then
                    LogEvent (IcsCRLF + FPrvKeyLines + IcsCRLF);
            except
                on E:Exception do begin
                    LogEvent ('Failed to generate private key - ' + E.Message);
                    exit ;
                end;
            end;
         end;

     // build public Json Web Key for Json Web Signing
     // basic jwk, no alg, kid or sig
        FAcmeJwsAlg := IcsJoseFindAlg(FAcmeJoseAlg, FAcmePrivKey.PrivateKey);
        FAcmeKwkPub := IcsJoseJWKPubKey(FAcmePrivKey.PrivateKey, '', '', '');
        LogEvent ('JWK Public: ' + FAcmeJwsAlg + ' - ' + FAcmeKwkPub);

     // create JWK Thumbprint, used for challenge
        FAcmeKwkThumb := IcsBase64UrlEncodeTB(IcsHashDigestTB(IcsStringToTBytes(FAcmeKwkPub), Digest_sha256));  { V9.1 using TBytes }
        LogEvent ('Thumbprint: ' + FAcmeKwkThumb);
        Result := true;
    except
        on E:Exception do begin
            LogEvent ('Fatal ACME protocol error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
//  must have a valid nonce to do POST requests
function TSslX509Certs.AcmeCheckNonce: Boolean;   { V8.64 }
begin
    Result := True;
    if fAcmeRespNonce <> '' then
        Exit;
    LogEvent ('Get new nonce');
    Result := AcmeGetRequest(httpHEAD, AcmeActionDirs [AcmeNewNonce].URL, '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// contact ACME server to get ACME Server Action URLs and T&Cs
function TSslX509Certs.AcmeGetActions: Boolean;
var
    MetaJson, MetaArray: ISuperObject;
    JsonEnum: TSuperAvlIterator;
    JsonItem: TSuperAvlEntry;
//  TermsFname, FullName: String;
    I, PLen: Integer;

    function BuildDynArray(MyArray: ISuperObject): TStringDynArray;
    var
        J, ALen: Integer;
    begin
        ALen :=  MyArray.AsArray.Length;
        SetLength(Result, ALen);
        if ALen > 0 then begin
            for J := 0 to ALen - 1 do
                Result[j] := MyArray.AsArray[J].AsString;
         end;
    end;

begin
    Result := False;
    try
        LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Getting actions from ACME server: ' + FAcmeHost);
        FAcmeRespNonce := '';
        if NOT AcmeGetRequest(httpGET, FSupplierServer, '') then
            exit;
        if (fAcmeLastStatus <> 200) or (NOT Assigned(FHttpRest.ResponseJson)) then begin     { V8.62 }
            LogEvent ('Failed to get ACME server action URLs');
            exit;
        end;
        LogEvent ('Downloaded ACME server action URLs OK');
        for I := 1 to AcmeActionTot do begin
            AcmeActionDirs [I].URL := FHttpRest.ResponseJson.S[AcmeActionDirs [I].Action];
        end;

   // extra account information, varies between suppliers
        FAcmeTermsUrl := '';
        FAcmeWebsite := '';
        SetLength(FAcmeCaaIndenties, 0);
        SetLength(FAcmeProfileNames, 0);
        SetLength(FAcmeProfileURL, 0);
        MetaJson := FHttpRest.ResponseJson.O['meta'];
        if Assigned(MetaJson) then begin

       // V8.67 termsOfService PDF
         (*   FAcmeTermsUrl := MetaJson.S[AcmeResTermsOfService]; // V2      V9.5 not really necessary
            if FAcmeTermsUrl <> '' then begin
                I := LastDelimiter('/', FAcmeTermsUrl);
                if I > 0 then begin
                    TermsFname := Copy (FAcmeTermsUrl, I + 1, 999);
                    FullName := DirCertWork + TermsFName;
                    if (IcsGetFileSize (FullName) < 500) then begin  { V8.62 not blank file }
                        FHttpRest.HttpDownFileName := FullName;       { V8.68 can now download to a file }
                        if NOT AcmeGetRequest(httpGET, fAcmeTermsUrl, '') then
                            exit;
                        if (fAcmeLastStatus = 200) and (FHttpRest.ResponseStream.Size > 500) then begin
                            try
                                LogEvent ('Downloaded new Terms: ' + FullName);
                            except
                                on E:Exception do
                                    LogEvent ('Failed to save terms file: ' + Fullname + ' - ' + E.Message);
                            end;
                        end
                        else
                            LogEvent ('Failed to read terms file: ' + fAcmeTermsUrl + ' - ' + FHttpRest.LastResponse);
                        FHttpRest.HttpDownFileName := '';  { V8.68 clear for next request }
                    end
                    else
                        LogEvent ('Terms already downloaded: ' + FullName);
                end;
            end;    *)

         { V9.5 meta has more information about account we may need later }
            FAcmeExtAccount := MetaJson.B[AcmeResExternalAccountRequired];   // Google, ZeroSSL, DigiCert
            FAcmeWebsite := MetaJson.S[AcmeResWebsite];
            MetaArray := MetaJson.O[AcmeResCaaIdentities];
            FAcmeCaaIndenties := BuildDynArray(MetaArray);
            MetaArray := MetaJson.O[AcmeResProfiles];                        // Lets Encrypt
            if Assigned(MetaArray) then begin
            //    PLen :=  MetaArray.AsArray.Length;     // ??
                PLen := 9;
                SetLength(FAcmeProfileNames, PLen);
                SetLength(FAcmeProfileURL, PLen);
                JsonEnum := MetaArray.AsObject.GetEnumerator;   // array of Json objects, so don't have names
                I := 0;
                while JsonEnum.MoveNext do begin
                    JsonItem := JsonEnum.GetIter;
                    if NOT Assigned(JsonItem) then
                        continue;
                    FAcmeProfileNames[I] := JsonItem.Name;
                    FAcmeProfileURL[I] := JsonItem.Value.AsString;
                    I := I + 1;
                end;
                JsonEnum.Free;
                SetLength(FAcmeProfileNames, I);
                SetLength(FAcmeProfileURL, I);
            end;
        end;
        Result := True;
    except
        on E:Exception do begin
            LogEvent ('Fatal ACME protocol error: ' + E.Message);
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.AcmeV2NewAccount(ExistsOnly: Boolean): Boolean;    { V9.5 allow lookups }
var
    I: Integer;
    JsonParams: TRestParams;
    JsonStr, EABValue: String;
begin
    LogTimeStamp;
    LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Registering account with ACME server: ' + fAcmeHost);
    Result := False;
    fAcmeAccountUrl := '';
    fAcmeKwkKid := '';   // must be blank for AcmeNewAccount2
    fAcmeAccountNum := '';
    FPendAccountClose := False; { V8.63 }
    FAcmeRespLocation := '';
    try

    // get first Nonce
        fAcmeRespNonce := '';
        if NOT AcmeCheckNonce then
            exit;
        LogEvent ('Initial nonce: ' + fAcmeRespNonce);

     // register and create an account, may have one already for this key if old
        JsonParams := TRestParams.Create(Nil);
        JsonParams.PContent := PContJson;
        if ExistsOnly then
            JsonParams.AddItem('onlyReturnExisting', true);    { V9.5 don't create a new account }
        JsonParams.AddItem('termsOfServiceAgreed', true);
        if FSupplierEmail <> '' then   { V9.5 optional for Let's Encrypt }
            JsonParams.AddItemSO('contact', SA(['mailto:' + FSupplierEmail]));
        if FAcmeExtAccount or (FAcmeEABKid <> '') then begin   { V9.5 some suppliers need an external account specifying }
           EABValue := IcsJoseJWSJson(jsigHmac256, fAcmeKwkPub, FAcmeEABHmacKey, Nil,
                                                    '', '', FAcmeEABKid, '', AcmeActionDirs [AcmeNewAccount].URL, True);  { V9.5 ECDSA IEEE }
           if FDebugLevel >= DebugParams then
               LogEvent ('EAK Json: ' + EABValue);
           JsonParams.AddItem('externalAccountBinding', EABValue, True);
        end;
        JsonStr := String(JsonParams.GetParameters);
        JsonParams.Free;
        if NOT AcmeGetRequest(httpPOST, AcmeActionDirs [AcmeNewAccount].URL, JsonStr) then
          exit;
        DumpJson;

    // did we create an account OK, or find one that matches the private key
        if (fAcmeRespLocation <> '') then begin
            if fAcmeRespRequester <> '' then
                fAcmeAccountNum := fAcmeRespRequester    { V9.5 }
            else begin
                I := LastDelimiter('/', fAcmeRespLocation);   // find account number at end of URL
                if I > 10 then
                    fAcmeAccountNum := Copy(fAcmeRespLocation, I + 1, 999);  // not in response with v2
            end;
            if (fAcmeLastStatus = 200) or (fAcmeLastStatus = 409) then begin
                fAcmeAccountUrl := fAcmeRespLocation;
                fAcmeKwkKid := fAcmeAccountUrl;       // this is our Kid for future requests
                LogEvent('Using old Acme account for this key: ' + fAcmeAccountNum + ', URL/KID: ' + fAcmeAccountUrl) ;
            end
            else if fAcmeLastStatus = 201 then begin
                fAcmeAccountUrl := fAcmeRespLocation;
                fAcmeKwkKid := fAcmeAccountUrl;       // this is our Kid for future requests
                LogEvent('Created Acme account: ' + fAcmeAccountNum + ', URL/KID: ' + fAcmeAccountUrl) ;
            end;
        end
        else begin
            LogEvent('Failed to Create Acme account: ' + FHttpRest.ResponseJson.S['type'] + ', ' + FHttpRest.ResponseJson.S['detail']) ;
            FAcmeRespNonce := ''; { V8.63  clear nonce since out of sequence now }
            Exit;
        end;
        Result := True;
    except
        on E:Exception do begin
            LogEvent ('Fatal ACME protocol error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Acme check new order for a SSL certificate, V9.5 must call DBReadCNDomain to get order information
// note this restarts the order process for an old order, although old challenges may still be valid
// optionally check domain exists for domain challenge
// optionally save order to database
function TSslX509Certs.AcmeCheckSaveOrder(DomainCheck: Boolean = True; UpdateDB: Boolean = False): Boolean;  { V9.5 was AcmeCheckOrder }
begin
    Result := false ;
    FLastError := '';
    LogTimeStamp;
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    if (FCertCommonName = '') then begin
        FLastError := 'Must specify domain Common Name for certificate';   { V9.5 keep for validation logging }
        LogEvent(FLastError);
        Exit;
    end;
    LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Checking Order for: ' + FCertCommonName);

// see if using details from old CSR
    FNewSslCert.DoClearCerts;
    if FCertCsrOrigin = CsrOriginFile then begin
        if NOT CheckCSR(True) then
            Exit;  // sets CommonName and SANs
    end;

// initial set-up

 // read internal variables for old order, but not saved public properties, they may be new
 // V9.5 assumes everything read before calling this function

 // reset old order
    if FIssueState > IssStateChecked then begin
        LogEvent('Resetting old order');
        FIssueState := IssStateNone;
        fAcmeOrderStatus := '';
        FAcmeOrderFinalizeUrl := '';
        FAcmeOrderObjUrl := '';
//        FNewCertStartDT := 0;
        FChallgStartDT := 0;
        FChallgExpireDT := 0;
        FCertRenewNow := False;
    end;

   if Assigned(FOnChallgRefresh) then
       FOnChallgRefresh(Self);  // V9.5 so app can update progress

 // make sure common name is also in SANs, so we can ignore it subsequently
    BuildSANList;

// see if challenge supported
    if (fSuppCertChallenge = ChallNone) then begin
        FLastError := 'No challenge validation specified';
        LogEvent(FLastError);
        Exit;
    end;

 // only allowed five duplicate orders for the same certificate each week
 // so one only per day
     if (FNewCertStartDT > 10) and ((IcsGetUTCTime - FNewCertStartDT) < 1) then begin  { V8.61 }
        FLastError := 'Only one order per domain per day allowed';
        LogEvent(FLastError);
        Exit;
     end;

 // validate some settings
    if (fSuppCertChallenge in [ChallEmail, ChallManual]) then begin
        FLastError := 'EMAIL and Manual validation not available for this supplier';
        LogEvent(FLastError);
        Exit;
    end;
    if (fSuppCertChallenge in [ChallAlpnUNC, ChallAlpnSrv, ChallAlpnApp]) then begin   { V9.5 all challenges?? }
        if (Pos(':', FDirWellKnown) < 2) then begin
            FLastError := 'Well-Known Directory Must Be Specified';
            LogEvent(FLastError);
            Exit;
        end;
        ForceDirectories(FDirWellKnown);
    end;

 // see if checking challenge by creatng and reading Well-Known file or ALPN certificate
    if DomainCheck then begin

     // V8.64 if order has currently valid challenges, don't need to check it again
        if (FIssueState >= IssStateChallgReq) and (FChallgStartDT > 10) and (FChallgExpireDT > Now) then begin
            if (FIssueState >= IssStateChallgOK) then
                LogEvent('Challenges already passed and still valid')
            else
                LogEvent('Skipped check order, already have pending challenges');
            Result := True;
        end

      // where the well known directory is located for each domain
        else if FIssueState < IssStateChecked then begin
            if NOT TestAltNameChallgs then begin
            FLastError := 'Failed to check all local challenges for Common Name: ' + fCertCommonName;
                LogEvent(FLastError);
            end
            else begin
                Result := True;
                LogEvent ('Order checking passed: ' + fCertCommonName);
                FIssueState := IssStateChecked;
            end;
        end
        else begin
            Result := True;
            LogEvent ('Order already checked: ' + fCertCommonName);
        end;
    end
    else
        Result := True;

  // write database
    if UpdateDB then begin
        if DBWriteCNDomain then
            LogEvent ('Saved domain to database: ' + fCertCommonName)
        else begin
            FLastError := 'Failed to save domain to database: ' + fCertCommonName;
            LogEvent(FLastError);
        end;

     // V8.65 delete old challenges
         RemoveOldChallgs(fCertCommonName);

    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Acme get challenges for all domains on our certificate, set them up but don't
// ask Acme to test them until we've done it ourself, challenges remain valid
// for a week.  Allows challenges to be set-up manually if necessary.
{ must have used DBReadCNDomain to set all domain variables }
function TSslX509Certs.AcmeV2GetChallgs: Boolean;   { V8.64 split from AcmeV2OrderCert }
var
    ArrayChallg, ArrayIdents, AcmeJson: ISuperObject;
    ChallgJson, IdentsJson: ISuperObject;
    AuthMethod, ChallgStatus, S: string ;
    I, J, TotAuth: integer;
    CurChallenge: TChallengeItem;
    SocFamily: TSocketFamily;  { V9.5 }
begin
    Result := False;
    FLastError := '';
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    if (Pos ('.', FCertCommonName) = 0) or (FCertSubAltNames.Count = 0) then begin      { V9.1 was blank }
        FLastError := 'Must specify domain Common Name for certificate';
        LogEvent(FLastError);
        Exit;
    end;
    LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Getting Challenges for: ' + FCertCommonName);
    if (fAcmeKwkKid = '') then begin
        FLastError := 'Must create or open ACME account first, No KID';
        LogEvent(FLastError);
        Exit;
    end;

 // new order so clear stuff from last order
    FAcmeOrderFinalizeUrl := '';
    FAcmeOrderObjUrl := '';
    FAcmeChallngReport := '';
    FPartFNameFinal := '';
    FPartFNameServer.Clear;
    FFileFinalCSR := '';
    FFileFinalPrvKey := '';
    FFileFinalBundle := '';
    FChallgStartDT := 0;
    FOrderStartDT := Now;
    FOrderCertsDT := 0;
    FOrderAttempts := FOrderAttempts + 1;
    FPendingChallg := 0;
    FPendAccountClose := False; { V8.63 }

{    FNewCertCN := '';      not until new order collected
    FChallgDoneDT := 0;
    FNewCertSAN := '';
    FNewCertValRes := chainFail;
    FNewCertErrs := '';
    FNewCertChainInfo := '';
    FNewCertEndDT := 0;
    FNewCertStartDT := 0;      }

// V8.62 check we can create certificate directories before order starts
    if NOT SetPartFNames (False) then
        Exit ;
    LogTimeStamp;
    LogEvent ('Getting Acme domain challenges for Common Name: ' + fCertCommonName);

 // check challenge allowed,  and well known directory for each domain on certificate
 // loads internal variables from database
    if FIssueState <> IssStateChecked then begin
        LogEvent('Need to check challenges locally first');
        if NOT AcmeCheckSaveOrder(True, True) then    // update database
            Exit;    // sets LastError
    end
    else
        LogEvent('Already checked challenges locally');

 // V8.64 if order has currently valid challenges, don't need to check it again
    if (FIssueState >= IssStateChallgReq) and (FChallgStartDT > 10) and (FChallgExpireDT > Now) then begin
        if (FIssueState >= IssStateChallgOK) then
            LogEvent('Challenges already passed and still valid')
        else
            LogEvent('Skipped get challenges, already have pending challenges');
        Result := True;
        Exit;
    end;

 // order info
    FNewOrderNum := DBNewOrderNum;
    if FSuppOrderRef = '' then
        FSuppOrderRef := 'ICS-' + IntToStr(FNewOrderNum);
    if FSuppCertProduct = '' then begin   { V9.5 }
        FSuppCertProduct := AcmeSupplierLits[FAcmeSupplier];
        if FAcmeSupplier in [AcmeLetsEncrypt, AcmeLetsEncryptTest]  then
            FSuppCertProduct := FSuppCertProduct + ' - ' + FCertAcmeProfile;
        if FAcmeSupplier in [AcmeGoogle, AcmeGoogleTest]  then
            FSuppCertProduct := FSuppCertProduct + ' - Days Validity ' + IntToStr(FCertValidity);
    end;

    try
        case fSuppCertChallenge of
            ChallFileUNC: AuthMethod := 'http-01';
            ChallFileFtp: AuthMethod := 'http-01';
            ChallFileSrv: AuthMethod := 'http-01';
            ChallFileApp: AuthMethod := 'http-01';
            ChallDnsAuto: AuthMethod := 'dns-01';           { account independent challenge }
            ChallDnsAcnt: AuthMethod := 'dns-account-01';   { V9.5 account dependent challenge }
            ChallDnsMan:  AuthMethod := 'dns-01';
            ChallAlpnUNC: AuthMethod := 'tls-alpn-01';
            ChallAlpnSrv: AuthMethod := 'tls-alpn-01';
            ChallAlpnApp: AuthMethod := 'tls-alpn-01';
            else begin
                LogEvent ('Unsupported Challenge Method');
            // others: email-reply-00, tkauth-01, onion-csr-01 (dns), bp-nodeid-00 and tkauth-01
                Exit;
            end;
        end;

    // V8.64 start order process with new nonce
        fAcmeRespNonce := '';
        if NOT AcmeCheckNonce then
            exit;

    // new order authorisation request, get order object with lots of URLs for our domains
    // Acme V2 may have multiple host names, which will get separate challenges
    // ACME Identifier Types: dns [RFC8555], ip [RFC8738], email [RFC8823], TNAuthList [RFC9448], bundleEID and NfInstanceId
        ArrayIdents := SA([]);
        for I := 0 to FCertSubAltNames.Count - 1 do begin
            if WSocketIsIP(FCertSubAltNames[I].SADomain, SocFamily) then  { V9.5 allow IP addresses }
                ArrayIdents.O[''] := SO (['type', 'ip', 'value', FCertSubAltNames[I].SADomain])
            else
                ArrayIdents.O[''] := SO (['type', 'dns', 'value', IcsIDNAToASCII(FCertSubAltNames[I].SADomain)]); { V8.64 }
            FCertSubAltNames[I].SAIssueState := FIssueState;
            if FIssueState < IssStateChallgPend then begin
                FCertSubAltNames[I].SAStartDT := 0;
                FCertSubAltNames[I].SADoneDT := 0;
                FCertSubAltNames[I].SAValidResult := '';
            end;
        end;
        AcmeJson := SO(['identifiers', ArrayIdents]);  { V9.5 }

     { V9.5 tell them we are replacing an not expired certificate, if we know the cert info }
        if (FCertRenewalId <> '') and (FNewCertEndDT > Now) then
          AcmeJson.S['replaces'] := FCertRenewalId;

     { V9.5 request certificate profile, if we have it }
        if FCertAcmeProfile <> '' then
          AcmeJson.S['profile'] := FCertAcmeProfile;

     { V9.5 Google allows certificate expiry to be specified }
        if (FAcmeSupplier in [AcmeGoogle, AcmeGoogleTest]) and (FCertValidity >= 3) and (FCertValidity <= 90) then begin
            AcmeJson.S['notBefore'] := RFC3339_DateToStr(Now - OneHour, True);  // add timezone, +00:00, earlier in case of clock errors
            AcmeJson.S['notAfter'] := RFC3339_DateToStr(Now + FCertValidity, True);  // add timezone, +00:00
        end;

        if NOT DBWriteCNDomain then
            Exit; // save public and private properties
        if NOT AcmeGetRequest(httpPOST, AcmeActionDirs [AcmeNewOrder].URL, AcmeJson.AsJson(False, False)) then
            Exit;
        DumpJson;

(*   Acme Order Object, also returned by AcmeV2OrderStatus when checking order progress
Request completed: 201 Created
Location: https://acme-v02.api.letsencrypt.org/acme/order/31733331/18533383
Response (length 741)
{
  "status": "pending",
  "expires": "2018-07-30T11:26:45.071131264Z",
  "identifiers": [
    {
      "type": "dns",
      "value": "test1.telecom-tariffs.co.uk"
    },
    {
      "type": "dns",
      "value": "test2.telecom-tariffs.co.uk"
    },
    {
      "type": "dns",
      "value": "test3.telecom-tariffs.co.uk"
    }
  ],
  "authorizations": [
    "https://acme-v02.api.letsencrypt.org/acme/authz/JjB-jDQjtA15ogoY-qfAc5cqJscAcBg6gQp0lr4_iFU",
    "https://acme-v02.api.letsencrypt.org/acme/authz/pH4NQhnkRbK1m-VfSSCkijYDkPx8hmceZFUG6YVSrCU",
    "https://acme-v02.api.letsencrypt.org/acme/authz/7Op_FW4ZzPftOFIZvkbGnjQjc_TRHjNEhgEvztvxJGo"
  ],
  "finalize": "https://acme-v02.api.letsencrypt.org/acme/finalize/31717591/21234582"
}
*)

 { V9.5 note, this code duplicates a lot of AcmeV2OrderStatus, could simplify in the future }
        if fAcmeLastStatus <> 201 then begin
            LogEvent('Failed to get ACME order object: ' + FHttpRest.ResponseJson.S['type'] +
                                                                                 ', ' + FHttpRest.ResponseJson.S['detail']) ;
            FAcmeRespNonce := ''; { V8.63  clear nonce since out of sequence now }
        // see if closing account
            if FAutoAccountClose then
                FPendAccountClose := True; { V8.63 }
            Exit;
        end;
        FAcmeOrderObjUrl := fAcmeRespLocation;  // order object
        I := LastDelimiter('/', FAcmeOrderObjUrl);
        if I > 10 then
            fSuppOrderId := Copy(FAcmeOrderObjUrl, I + 1, 999);

   //  V9.5 update IssueState according to order object, gets list of authorisations
        ProcOrderStatus;
        if FIssueState = IssStateChallgPend then   // special case since order not started yet
            FIssueState := IssStateChallgReq;

     // challenges already completed OK, nothing more to do here, collect order
        if (FIssueState >= IssStateChallgOK) then begin
            if FChallgStartDT < 10 then
                FChallgStartDT := Now;
            if (FChallgDoneDT < 10) then
                FChallgDoneDT := Now;  { V8.64 }
            LogEvent('ACME certificate order already completed, collect certificate' + IcsCRLF);

         // V9.5 add order to active list, which starts order timer so order completes automatically
            if FAutoOrderComplete then
               AddActiveOrder(FCertCommonName);
            Result := True;
        end

     // some fatal error
        else if (FIssueState = IssStateNone) then begin
            LogEvent('ACME certificate order failed, start again' + IcsCRLF);
            FAcmeRespNonce := ''; { V8.63  clear nonce since out of sequence now }
            FChallgExpireDT := 0;
        // see if closing account
            if FAutoAccountClose then
                FPendAccountClose := True; { V8.63 }
        end

      // not started challenges yet, add challenges to database
        else if (FIssueState = IssStateChallgReq) then begin
            FChallgDoneDT := 0;
            FChallgStartDT := Now;
            TotAuth := Length(FAcmeOrderDomains);
            if TotAuth = 0 then begin
                LogEvent('!! Warning, order object has no authorizations');
                FIssueState := IssStateNone;
                FAcmeRespNonce := '';
                FChallgExpireDT := 0;
             // see if closing account
                if FAutoAccountClose then
                    FPendAccountClose := True; { V8.63 }
            end
            else begin

            // must have a valid nonce to do POST requests
                if NOT AcmeCheckNonce then
                    exit;

             // some challenge stuff is common to all domaims
                CurChallenge.CCommonName := fCertCommonName;
                CurChallenge.CSuppOrderId := FSuppOrderId;
                CurChallenge.CSupplierProto := FSupplierProto;
                CurChallenge.CType := fSuppCertChallenge;
                CurChallenge.CIssueState := IssStateChallgPend;
                CurChallenge.CStartDT := Now;
                CurChallenge.CExpireDT := fChallgExpireDT;  // V8.64
                CurChallenge.CDoneDT := 0;

                for I := 0 to TotAuth - 1 do begin
                    CurChallenge.CAuthzURL := FAcmeOrderDomains[I].AuthzURL;
                    if CurChallenge.CAuthzURL = '' then begin
                        FLastError := 'Failed to find authorization: ' + AuthMethod;
                        LogEvent(FLastError);
                        Exit;
                    end;

                 // now start each authorisation, one for each domain
                    if NOT AcmeGetRequest(httpPOST, CurChallenge.CAuthzURL, '') then
                        exit;
                    if fAcmeLastStatus <> 200 then begin
                        FLastError := 'Failed to get ACME challenges: ' + FHttpRest.ResponseJson.S['type'] + ', ' + FHttpRest.ResponseJson.S['detail'] ;
                        LogEvent(FLastError);
                        FAcmeRespNonce := ''; { V8.63  clear nonce since out of sequence now }
                        Exit;
                    end;
                    DumpJson;
(*
Request completed: 200 OK
Response (length 919)
{
  "identifier": {
    "type": "dns",
    "value": "test1.telecom-tariffs.co.uk"
  },
  "status": "pending",
  "expires": "2018-07-30T11:26:45Z",
  "challenges": [
    {
      "type": "dns-01",
      "status": "pending",
      "url": "https://acme-v02.api.letsencrypt.org/acme/challenge/pH4NQhnkRbK1m-VfSSCkijYDkPx8hmceZFUG6YVSrCU/5877772577",
      "token": "S1nSmbqllXEnGIPBstR-Vk9vS5bUs80pBKjVVfcEXsc"
    },
    {
      "type": "http-01",
      "status": "pending",
      "url": "https://acme-v02.api.letsencrypt.org/acme/challenge/pH4NQhnkRbK1m-VfSSCkijYDkPx8hmceZFUG6YVSrCU/5877772578",
      "token": "VGRCea7jdbIjQ9ND-KpRAd96r4hAfJXEMsxUKqZp1H8"
    },
    {
      "type": "tls-alpn-01",
      "status": "pending",
      "url": "https://acme-v02.api.letsencrypt.org/acme/challenge/pH4NQhnkRbK1m-VfSSCkijYDkPx8hmceZFUG6YVSrCU/5877772579",
      "token": "EVM0K8eRUstKD0bk0avHFqDgOGC36xXPixOSPqQT3h0"
    }
  ]
}   *)
               // ACME should have offered us several challenges with different types, we need to choose
               // one and start it, ignore the others
                    IdentsJson := FHttpRest.ResponseJson.O['identifier'];
                    CurChallenge.CDomain := IcsIDNAToUnicode(IdentsJson.S['value']);  { V8.64 }
                    CurChallenge.CIdType := IcsIDNAToUnicode(IdentsJson.S['type']);   { V9.5 }
                    CurChallenge.CWildCard := FHttpRest.ResponseJson.B['wildcard'];   { V8.64 }
                    ChallgStatus := FHttpRest.ResponseJson.S['status'];
                    CurChallenge.ChallengeURL := '';
                    CurChallenge.CDirWellKnown := '';
                    CurChallenge.CDirPubWebCert := '';
                    CurChallenge.CIssueState := IssStateNone;
                    CurChallenge.CSanIdx := DBFindSAN(CurChallenge.CDomain);
                    if CurChallenge.CSanIdx < 0 then begin  // sanity check
                        CurChallenge.CSanIdx := 0;
                        LogEvent('!!! Failed to find sub alt name for ' + CurChallenge.CDomain);
                    end;

                    if ChallgStatus = 'valid' then begin
                        CurChallenge.CIssueState := IssStateChallgOK;
                        if CurChallenge.CDoneDT < 10 then
                            CurChallenge.CDoneDT := Now;
                        LogEvent('Challenge Already Passed for ' + CurChallenge.CDomain);
                    end
                    else begin
                        ArrayChallg := FHttpRest.ResponseJson.O['challenges'];  // array of challenges
                        for J := 0 to ArrayChallg.AsArray.Length - 1 do begin
                            ChallgJson := ArrayChallg.AsArray[J];
                            if ChallgJson.S['type'] = AuthMethod then begin
                                CurChallenge.ChallengeURL := ChallgJson.S['url'];
                                CurChallenge.ChallgToken := ChallgJson.S['token'];
                                CurChallenge.CResp := CurChallenge.ChallgToken  + '.' + fAcmeKwkThumb;   // key authorization

                                if CurChallenge.CType in [ChallAlpnSrv, ChallAlpnUNC, ChallAlpnApp] then begin  { V8.62 }
                                    CurChallenge.CAcmeAlpnCert := 'acmealpn-' + IcsBuildCertFName(CurChallenge.CDomain) + '.pem';
                                    if fSuppCertChallenge = ChallAlpnUNC then
                                        CurChallenge.CPage := 'acme-challenge\' + CurChallenge.CAcmeAlpnCert ; // file path for copying
                                    if CurChallenge.CIdType = 'ip' then     { V9.5 }
                                // IPv6  4321:0:1:2:3:4:567:89ab becomes  b.a.9.8.7.6.5.0.4.0.0.0.3.0.0.0.2.0.0.0.1.0.0.0.0.0.0.0.1.2.3.4.ip6.arpa
                                // IPv4  217.146.102.139 becomes 139.102.146.217.in-addr.arpa
                                        CurChallenge.CAlpnSNI := String(IcsReverseIPArpa(CurChallenge.CDomain))
                                    else
                                        CurChallenge.CAlpnSNI := CurChallenge.CDomain;  { V9.5 }
                                    break;
                                end;
                                if CurChallenge.CType in [ChallFileSrv, ChallFileApp] then
                                    CurChallenge.CPage := 'acme-challenge/' + CurChallenge.ChallgToken  // URL path for local server
                                else
                                    CurChallenge.CPage := 'acme-challenge\' + CurChallenge.ChallgToken; // file path for copying
                                if (CurChallenge.CType in [ChallDnsAuto, ChallDnsAcnt, ChallDnsMan]) then begin     { V9.5 }
                            // Acme create base64 SHA256 digest of CResp for
                            // ie  _acme-challenge.example.org. 300 IN TXT "gfj9Xq...Rg85nM"
                                    CurChallenge.CPage :=  '_acme-challenge.' + IcsIDNAToASCII(CurChallenge.CDomain);  { V8.64 }
                                 { V9.5 dns-account-01 challenge adds account hash before _acme-xxx }
                                    if (CurChallenge.CType = ChallDnsAcnt) then
                                        CurChallenge.CPage := '_' + String(IcsBase64UrlEncodeA (IcsHashDigest(
                                                      AnsiString(FAcmeAccountUrl), Digest_sha256))) + '.' + CurChallenge.CPage;
                                    CurChallenge.CDNSValue := String(IcsBase64UrlEncodeA
                                                                (IcsHashDigest(AnsiString(CurChallenge.CResp), Digest_sha256)));
                                    break;
                                end;
                            end;
                        end;
                        if CurChallenge.ChallengeURL = '' then begin
                            FLastError := 'Failed to find challenge: ' + AuthMethod + ' for ' + CurChallenge.CDomain;
                            LogEvent(FLastError);
                            Exit;
                        end;
                        CurChallenge.CIssueState := IssStateChallgReq;  // V8.64 only requested, not pending yet
                        CurChallenge.CDirWellKnown := FCertSubAltNames[CurChallenge.CSanIdx].SADirWellKnown;
                        CurChallenge.CDirPubWebCert := FCertSubAltNames[CurChallenge.CSanIdx].SADirPubWebCert;
                        CurChallenge.CWKFullName := CurChallenge.CDirWellKnown + CurChallenge.CPage;
                        CurChallenge.CAcmeAlpnCert := FDirCertWork + CurChallenge.CAcmeAlpnCert;
                        S := 'Prepared Challenge for: ' + CurChallenge.CDomain + ', Method: ' + AuthMethod;
                        if CurChallenge.CDNSValue <> '' then
                            S := S +  ', Domain Name: ' + CurChallenge.CPage +  ', TXT: ' + CurChallenge.CDNSValue
                        else
                            S := S + ', Page URL: ' + CurChallenge.CPage + ', Token: ' + CurChallenge.ChallgToken;
                        LogEvent(S);  { V9.5 }

                    // prepare challenge - starts local web server
                       if NOT PrepOneChallenge(CurChallenge) then
                            Exit;
                    end ;

                 // update database with new challenge
                    if (DBWriteOneChallenge(CurChallenge) < 0) then begin
                        FLastError := 'Failed to update challenge database';
                        LogEvent(FLastError);
                        Exit;
                    end;
                    FCertSubAltNames[CurChallenge.CSanIdx].SAIssueState := CurChallenge.CIssueState;
                end;

             // challenges ready for testing, but don't start yet
                FIssueState := IssStateChallgReq;  // V8.64 only requested, not pending yet
                LogEvent('ACME challenges ready for testing' + IcsCRLF);
                Result := True;
            end;
        end;
        DBWriteCNDomain;
        if Assigned(FOnChallgRefresh) then
            FOnChallgRefresh(Self);  // V9.5 so app can update progress
      except
        on E:Exception do begin
            LogEvent ('Fatal ACME protocol error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// locally test the challenges to make sure they are set-up correctly.
// challenges remain valid for a week
function TSslX509Certs.AcmeV2TestChallgs: Boolean;    { V8.64 new stage }
var
    UpdateFlag: Boolean;
    I: integer;
begin
    Result := False;
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    if (fCertCommonName = '') or (FCertSubAltNames.Count = 0) then begin
        LogEvent('Must specify domain Common Name for certificate');
        Exit;
    end;
    LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Locally Testing Challenges for: ' + FCertCommonName);
    if (fAcmeKwkKid = '') then begin
        LogEvent ('Must create or open ACME account first');
        Exit;
    end;

 // if order has currently valid challenges, don't need to test it again
    if (FIssueState >= IssStateChallgOK) and (FChallgStartDT > 10) and (FChallgExpireDT > Now) then begin
        LogEvent('Challenges already passed and still valid');
        Result := True;
        Exit;
    end;

  // look through challenges, test them
    if NOT DBReadChallenges then begin
       LogEvent('Failed to read challenge database');
        Exit;
    end;
    if FChallengesTot = 0 then begin
        LogEvent('No challenges in database');
        Exit;
    end;
    UpdateFlag := False;
    Result := True;
    for I := 0 to Length(FChallengeItems) - 1 do begin
        if FChallengeItems [I].CCommonName = fCertCommonName then begin
            with FChallengeItems [I] do begin
               // test challenge ourself
                if CIssueState = IssStateChallgReq then begin
                    LogEvent('Local Test ACME Challenge for: ' + CDomain);
                    if TestOneChallenge(FChallengeItems [I]) then begin
                        CIssueState := IssStateChallgTest;
                        DBWriteOneChallenge(FChallengeItems [I]);
                        FCertSubAltNames[CSanIdx].SAIssueState := CIssueState;
                        UpdateFlag := True;
                     end;
                end;
            // any domains not tested means order not tested either
                if CIssueState <= IssStateChallgReq then
                    Result := False;
            end;
        end;
    end;

  // see if challenges for all domains tested
    if Result then begin
        FIssueState := IssStateChallgTest;
        if Assigned(FOnChallgRefresh) then
            FOnChallgRefresh(Self);  // V9.5 so app can update progress
    end;
    if UpdateFlag OR Result then
        DBWriteCNDomain;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 get Acme Order status, was part of AcmeV2GetCert }
function TSslX509Certs.AcmeV2OrderStatus: Boolean;   { V9.5 split AcmeV2GetCert into three functions }
begin
    Result := False;
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    fAcmeOrderStatus := 'none';
    if FIssueState >= IssStateFinished then begin  // V9.5 can not check status once finished
        Result := True;
        Exit;
    end;
//    if (FChallgExpireDT > 10) and (FChallgExpireDT < Now) then    // V9.5 order has expired, ??? often now ???
//        FAcmeOrderObjUrl := '';  // URL no longer works, clear it
    if (FAcmeOrderObjUrl = '') then begin
        LogEvent('Can not check Acme Order Objec, Expired: ' + fCertCommonName);
        FChallgExpireDT := 0;
        FIssueState := IssStateNone;
        if Assigned(FOnChallgRefresh) then
            FOnChallgRefresh(Self);  // V9.5 so app can update progress
        Exit;
    end;

(* Acme order object, notBefore/After are requested certificate start and end dates, ie validity
AcmeOrderObjUrl:  https://dv.acme-v02.api.pki.goog/order/gGJnoUUkE45PSQte-0xwOQ
  {"status":"processing",
   "expires":"2025-07-01T16:38:44.801800378Z",
   "identifiers":[
       {"type":"dns","value":"test4.ftptest.co.uk"},
       {"type":"dns","value":"test4.ftptest.org"}
     ],
   "notBefore":"2025-06-03T15:54:41Z",
   "notAfter":"2025-07-03T16:54:41Z",
   "authorizations":[
        "https://dv.acme-v02.api.pki.goog/authz/XOZFBuCbDxugiEAyQqb6Mw",
        "https://dv.acme-v02.api.pki.goog/authz/w83V_jzF9Fo3LRncJpP-2w"
     ],
   "finalize":"https://dv.acme-v02.api.pki.goog/order/gGJnoUUkE45PSQte-0xwOQ/finalize"
  }
*)

    try
    // must have a valid nonce to do POST requests
        if NOT AcmeCheckNonce then
            exit;

    // V2 get order object, tells if challenges completed or pending
        LogEvent('Checking Acme Order Object: ' + fCertCommonName);
        if NOT AcmeGetRequest(httpPOST, FAcmeOrderObjUrl, '') then   // Post-as-get, zero payload, not finalize URL !!!!
            exit;
        if fAcmeLastStatus > 202 then begin
            LogEvent('Acme could not find order: ' + FAcmeOrderObjUrl);
            FAcmeOrderObjUrl := '';  // URLs no longer work, clear them
            FChallgExpireDT := 0;
            Exit;
        end;
        DumpJson;
        ProcOrderStatus;
        Result := True;
    except
        on E:Exception do begin
            LogEvent ('Fatal AcmeV2OrderStatus protocol error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 update order issue state from Acme Order status object }
{ used by AcmeV2OrderStatus, could be used by AcmeV2OrderFinal and AcmeV2StartChallgs which also have order object }
procedure TSslX509Certs.ProcOrderStatus;   { V9.5 process order status }
var
    OldState: TIssueState;
    ArrayAuthz, ArrayIdents: ISuperObject;
    TotAuth, I: Integer;
begin
    try
        OldState := FIssueState;

      // order has finished and been removed, update state
        if (FIssueState >= IssStateChallgReq) and ((FChallgExpireDT = 0) or (FAcmeOrderObjUrl = '')) then begin
           if (FIssueState = IssStateCollected) then
               FIssueState := IssStateFinished;
           FAcmeOrderObjUrl := '';  // URLs no longer work, clear them
           FAcmeOrderFinalizeUrl := '';
           SetLength(FAcmeOrderDomains, 0);
           FChallgExpireDT := 0;
           OldState := IssStateNone; // force database update to clear old stuff
           LogEvent('ACME certificate order finished OK, updated issue state');
        end
        else begin
            if NOT Assigned(FHttpRest.ResponseJson) then
                Exit;
            fAcmeOrderStatus := FHttpRest.ResponseJson.S['status'];
            if (fAcmeOrderStatus = '') then
                Exit;
            FChallgExpireDT := FHttpRest.ResponseJson.DT['expires'];
            fAcmeOrderFinalizeUrl := FHttpRest.ResponseJson.S['finalize'];
            FAcmeCertUrl := FHttpRest.ResponseJson.S['certificate'];

  // "invalid": The certificate will not be issued.  Consider this order process abandoned
  // "pending": The server does not believe that the challenges are done, check the "authorizations" array
  // "ready": The server agrees that the challenges are done, and is awaiting finalization request
  // "processing": The certificate is being issued.  Repeat this request until response changes
  //    (note this is optional, processing is often skipped and ready changes to valid)
  // "valid": The server has issued the certificate and set its URL to the "certificate" field of the order, download it

            if (fAcmeOrderStatus = 'invalid') then begin           // order dead,
                LogEvent('ACME certificate order failed, start again' + IcsCRLF);
                FIssueState := IssStateNone;
             // delete old challenges
                RemoveOldChallgs(fCertCommonName);
            end
            else if (fAcmeOrderStatus = 'pending') then begin      // challenges pending being tested by supplier
                if FIssueState < IssStateChallgPend then begin
                    FIssueState := IssStateChallgPend;
                    LogEvent('ACME challenges not compeleted yet');
                end;
            end
            else if (fAcmeOrderStatus = 'ready') then begin         // challenges done, finalize order
                if FIssueState < IssStateFinalPend then begin
                    FIssueState := IssStateFinalPend;
                 // delete old challenges
                    RemoveOldChallgs(fCertCommonName);
                    LogEvent('ACME challenges completed, finalise order');
                end;
            end
            else if (fAcmeOrderStatus = 'processing') then begin      // wating for certificate to be issued
                if FIssueState < IssStateIssuePend then begin
                    FIssueState := IssStateIssuePend;
                    LogEvent('ACME certificate issue in process');
                end;
            end
            else if (fAcmeOrderStatus = 'valid') then begin           // certificate ready to collect
                FAcmeCertURL := FHttpRest.ResponseJson.S['certificate'];
                if FIssueState < IssStateIssued then begin
                    FIssueState := IssStateIssued;
                    LogEvent('ACME certificate issued OK');
                end;
            end;

         // keep authoriziations for all domains so we can check individual challenges
            if (FIssueState >= IssStateChallgPend) or (fAcmeOrderStatus = 'invalid') then begin
                ArrayAuthz := FHttpRest.ResponseJson.O['authorizations'];  // authorization URLs
                ArrayIdents := FHttpRest.ResponseJson.O['identifiers'];
                if Assigned(ArrayAuthz) and Assigned(ArrayIdents) then begin
                    TotAuth := ArrayAuthz.AsArray.Length;
                    SetLength(FAcmeOrderDomains, TotAuth);
                    if TotAuth > 0 then begin
                        for I := 0 to TotAuth - 1 do begin
                            with FAcmeOrderDomains[I] do begin
                                AuthzURL := ArrayAuthz.AsArray[I].AsString;
                                IdDns := ArrayIdents.AsArray[I].AsObject.S['dns'];
                                IdValue := IcsIDNAToUnicode(ArrayIdents.AsArray[I].AsObject.S['value']);
                            end;
                        end;
                    end;
                end;
            end
            else
                SetLength(FAcmeOrderDomains, 0);
        end;

    // if state has changed, get challenge report, update database and event to application
        if OldState <> FIssueState then begin
            if (Length(FAcmeOrderDomains) > 0) then begin
                LogEvent('Building ACME challenge report');
                AcmeGetChallngReport;
                LogEvent(FAcmeChallngReport);
            end;
            DBWriteCNDomain;  // update database
            if Assigned(FOnChallgRefresh) then
                FOnChallgRefresh(Self);  // V9.5 so app can update progress
        end;
    except
        on E:Exception do begin
            LogEvent ('Acme ProcOrderStatus failed to process order object: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 get challenge status report from last Order Status Ojbect, if available }
procedure TSslX509Certs.AcmeGetChallngReport;      { V9.5 }
var
    TotAuth, I, J, K: integer;
    ChallgStatus, ChallgType: String;
    ValidDT: TDateTime;
    ValidJson, RecJson, ArrayChallg, ChallgJson: ISuperObject;
begin
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    TotAuth :=  Length(FAcmeOrderDomains);
    if TotAuth = 0 then begin
        FAcmeChallngReport := 'No challenges found for order: ' +  FCertCommonName;
        Exit;
    end;

 (*  Acme V2
Request completed: 200 OK
Response (length 554)
{
  "type": "http-01",
  "status": "valid",
  "url": "https://acme-v02.api.letsencrypt.org/acme/challenge/7Op_FW4ZzPftOFIZvkbGnjQjc_TRHjNEhgEvztvxJGo/5877772581",
  "token": "tEZim0F1rZEXZ6dSN8YlT-J4vYOrBEaRSaTGDXgh6mU",
  "validationRecord": [
    {
      "url": "http://test2.telecom-tariffs.co.uk/.well-known/acme-challenge/tEZim0F1rZEXZ6dSN8YlT-J4vYOrBEaRSaTGDXgh6mU",
      "hostname": "test2.telecom-tariffs.co.uk",
      "port": "80",
      "addressesResolved": [
        "217.146.115.84"
      ],
      "addressUsed": "217.146.115.84"
    }
  ]
}

Response (length 223)
{
  "type": "http-01",
  "status": "pending",
  "url": "https://acme-v02.api.letsencrypt.org/acme/challenge/pH4NQhnkRbK1m-VfSSCkijYDkPx8hmceZFUG6YVSrCU/5877772578",
  "token": "VGRCea7jdbIjQ9ND-KpRAd96r4hAfJXEMsxUKqZp1H8"
}

HTTP REST Response (length 454)
{
  "type": "dns-01",
  "status": "invalid",
  "error": {
    "type": "urn:ietf:params:acme:error:dns",
    "detail": "DNS problem: SERVFAIL looking up CAA for magsys.uk - the domain's nameservers may be malfunctioning",
    "status": 400
  },
  "url": "https://acme-v02.api.letsencrypt.org/acme/chall-v3/6620893218/-plNHA",
  "token": "h0xtJh-0MYJNGKj5jCElB-XOFZS8pCjyVBHPO0Hh4TE",
  "validationRecord": [
    {
      "hostname": "magsys.uk"
    }
  ]
}

Response xx
{
  "identifier": {
    "type": "dns",
    "value": "ftptest.org"
  },
  "status": "invalid",
  "expires": "2018-07-31T16:41:05Z",
  "challenges": [
    {
      "type": "dns-01",
      "status": "invalid",
      "error": {
        "type": "urn:ietf:params:acme:error:dns",
        "detail": "DNS problem: NXDOMAIN looking up TXT for _acme-challenge.ftptest.org",
        "status": 400
      },
      "url": "https://acme-v02.api.letsencrypt.org/acme/challenge/SRE4O3zoMyvXzgJRN6HCxUfU0tMfIgmEcOEqZuJPFoE/5907963322",
      "token": "jjtcTRjzhJgNl1CyQsY967xdsWDL2ViQYpRdgx6OvcU"
    }
  ],
  "wildcard": true
}

Response (length 675)
{
  "identifier": {
    "type": "dns",
    "value": "croydonducks.co.uk"
  },
  "status": "invalid",
  "expires": "2025-06-20T15:48:29Z",
  "challenges": [
    {
      "type": "dns-01",
      "url": "https://acme-v02.api.letsencrypt.org/acme/chall/42407551/535547859992/QqgyBQ",
      "status": "invalid",
      "validated": "2025-06-13T15:48:30Z",
      "error": {
        "type": "urn:ietf:params:acme:error:unauthorized",
        "detail": "Incorrect TXT record \"WvHermc5jHze4LHqFmZFo6bO1X0KmTQX31-EN-eB1-4\" (and 1 more) found at _acme-challenge.croydonducks.co.uk",
        "status": 403
      },
      "token": "kbl8YzSoj-_Td_wv2cZPKf13slmykm0ReMJkwYDYMEs"
    }
  ]
}

{
  "type": "dns-01",
  "status": "valid",
  "url": "https://acme-v02.api.letsencrypt.org/acme/chall-v3/3957825699/Hw_0xA",
  "token": "6GUNvnPDs0xn1TCcGzKa2Ukz1hj-Cu3Tl3_bXXlwhDg",
  "validationRecord": [
    {
      "hostname": "ftptest.co.uk"
    }
  ]
}
{
  "identifier": {
    "type": "dns",
    "value": "test5.telecom-tariffs.co.uk"
  },
  "status": "valid",
  "expires": "2020-04-18T16:38:49Z",
  "challenges": [
    {
      "type": "tls-alpn-01",
      "status": "valid",
      "url": "https://acme-v02.api.letsencrypt.org/acme/chall-v3/3449781566/ShFmZw",
      "token": "jHMLZhMTzqjuR_536ujGhgry-w8unP8qaojj87MHUQk",
      "validationRecord": [
        {
          "hostname": "test5.telecom-tariffs.co.uk",
          "port": "443",
          "addressesResolved": [
            "217.146.115.85"
          ],
          "addressUsed": "217.146.115.85"
        }
      ]
    }
  ]
}

*)
 // must have a valid nonce to do POST requests
    if NOT AcmeCheckNonce then
      exit;
    FAcmeChallngReport := '';
    for I := 0 to TotAuth - 1 do begin
        if FAcmeChallngReport <> '' then
            FAcmeChallngReport := FAcmeChallngReport + IcsCRLF;
        FAcmeChallngReport := FAcmeChallngReport + 'Challenge Domain: ' + FAcmeOrderDomains[I].IdValue;
        if NOT AcmeGetRequest(httpPOST, FAcmeOrderDomains[I].AuthzURL, '') then begin
            FAcmeChallngReport := FAcmeChallngReport + ', Failed to check ACME challenge: ' + FHttpRest.ResponseJson.S['type'] +
                                                                         ', ' + FHttpRest.ResponseJson.S['detail'] + IcsCRLF ;
            Continue;
        end;
        if fAcmeLastStatus > 202 then begin
            FAcmeChallngReport := FAcmeChallngReport + ', Bad HTTP Status: ' + IntToStr(fAcmeLastStatus) + IcsCRLF ;
            Continue;
        end;
        DumpJson;
        ChallgStatus := FHttpRest.ResponseJson.S['status'];
        FAcmeChallngReport := FAcmeChallngReport + ', Status: ' + ChallgStatus +
                                                ', Expires: ' + IcsDateTimeToAStr(FHttpRest.ResponseJson.DT['expires']);
        if ChallgStatus = 'pending' then begin
            //
        end
        else if ChallgStatus = 'processing' then begin
           //
        end

     // success or invalid, keep details for each challenge, valid or invalid
        else if (ChallgStatus = 'valid') or (ChallgStatus = 'invalid') then begin
            ArrayChallg := FHttpRest.ResponseJson.O['challenges'];  // array of challenges
            if Assigned(ArrayChallg) and (ArrayChallg.AsArray.Length > 0) then begin
                for J := 0 to ArrayChallg.AsArray.Length - 1 do begin
                    ChallgJson := ArrayChallg.AsArray[J];      // single challenge
                    ChallgType := ChallgJson.S['type'];
                    FAcmeChallngReport := FAcmeChallngReport + ', Type: ' + ChallgType;
                    ValidDT := ChallgJson.DT['validated'];
                    if ValidDT > 10 then
                        FAcmeChallngReport := FAcmeChallngReport + ' at ' + IcsDateTimeToAStr(ValidDT);
                    ValidJson := ChallgJson.O['validationRecord'];  // array of records
                    if (ValidJson <> Nil) then begin  // probably only for valid
                        for K := 0 to ValidJson.AsArray.Length - 1 do begin // should only be one
                            RecJson := ValidJson.AsArray[K];
                            if (Length(RecJson.S['addressUsed']) > 6) then
                                FAcmeChallngReport := FAcmeChallngReport + ', IP address ' +  RecJson.S['addressUsed'];
                            if RecJson.S['url'] <> '' then
                                FAcmeChallngReport := FAcmeChallngReport + ', URL: ' + RecJson.S['url'];
                        end;
                    end;
                    if (ChallgStatus = 'invalid') then begin
                        RecJson := ChallgJson.O['error'];
                        if RecJson <> Nil then
                            FAcmeChallngReport := FAcmeChallngReport + ', ' + RecJson.S['detail'] + ', Status: ' + RecJson.S['status'];
                    end;
                end;
             end
             else begin
                RecJson := FHttpRest.ResponseJson.O['error'];
                if RecJson <> Nil then
                    FAcmeChallngReport := FAcmeChallngReport + ', ' + RecJson.S['detail'] + ', Status: ' + RecJson.S['status'];
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Acme now starts testing the challenges to prove we own the certificate domains.
// Ideally we passed AcmeV2TestChallgs first, but not absolutely necessary.
// May take 30 seconds or several days for challenge to be checked.
{ must have used DBReadCNDomain to set all domain variables }
function TSslX509Certs.AcmeV2StartChallgs: Boolean;    { V8.64 renamed from OrderCert }
var
    ChallgStatus: string ;
    I: integer;
begin
    Result := False;
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    if (fCertCommonName = '') or (FCertSubAltNames.Count = 0) then begin
        LogEvent('Must specify domain Common Name for certificate');
        Exit;
    end;

    if FIssueState < IssStateChallgReq then begin
        LogEvent('No ACME challenge requested, must get challenges first');
        Exit;
    end;
    LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Requesting Start Testing Challenges for: ' + FCertCommonName);
    if (fAcmeKwkKid = '') then begin
        LogEvent ('Must create or open ACME account first');
        Exit;
    end;
    FCertRenewNow := False;

 // V8.64 if order has currently valid challenges, don't need to start it again
    if (FIssueState >= IssStateChallgOK) and (FChallgStartDT > 10) and (FChallgExpireDT > Now) then begin
        LogEvent('Challenges already passed and still valid');
     // V9.5 add order to active list, which starts order timer so order completes automatically
        if FAutoOrderComplete then
            AddActiveOrder(FCertCommonName);
        Result := True;
        Exit;
    end;

  // look through challenges, keep results
    if NOT DBReadChallenges then begin
        LogEvent('Failed to read challenge database, resetting state');
        FIssueState := IssStateNone;   { V9.2 reset state so order can restart }
        DBWriteCNDomain;
        Exit;
    end;
    if FChallengesTot = 0 then begin
        LogEvent('No challenges in database, resetting state');
        FIssueState := IssStateNone;   { V9.2 reset state so order can restart }
        DBWriteCNDomain;
        Exit;
    end;

  // ensure state updated
    LogEvent('Starting ACME challenges for order; ' + fCertCommonName);
    if (FIssueState = IssStateChallgWaitTest) then begin               { V8.65 }
        FIssueState := IssStateChallgTest;
        DBWriteCNDomain;
    end;

  // start local server if needed
    if (FSuppCertChallenge in [ChallFileSrv, ChallAlpnSrv]) then begin
        if FDomWebServer.WebSrvIP <> FDomWebSrvIP then
            StopDomSrv;  { V8.65 check correct IP }
        if NOT StartLocalServer(FSuppCertChallenge, fCertCommonName) then { V9.5 !!!! PENDING NEED CORRECT SNI FOR CHALLENGE }
            Exit;
        LogEvent('Local web server running');
    end;

  // must have a valid nonce to do POST requests
    fAcmeRespNonce := '';  { V8.65 seem to need a fresh none }
    if NOT AcmeCheckNonce then
        exit;

 // start each challenge
    try
        for I := 0 to Length(FChallengeItems) - 1 do begin
            if FChallengeItems [I].CCommonName = fCertCommonName then begin
                with FChallengeItems [I] do begin

                   //  start challenge, so they look up our file, no parameters, just a special URL
                    if CIssueState <> IssStateChallgOK then begin
                        CIssueState := IssStateChallgTest;   { V8.65 }
                        LogEvent('Starting ACME Challenge for: ' + CDomain);
                        if NOT AcmeGetRequest(httpPOST, ChallengeURL, '{}') then
                            exit;
                        DumpJson;
        (* HTTP/1.1 200 OK
         {
          "type": "http-01",
          "status": "pending",
          "url": "https://acme-staging-v02.api.letsencrypt.org/acme/challenge/6mDYqM5A5a7L3QnD0FxOUYI5FflSq-3MMqvfOWYPxcg/110522443",
          "token": "Or9PmU6EtQUEjph3-g8ljyQWmoMiBiQy_YJtKWrF_O8"
        }
        *)
                     // was challenge accepted ???
                        if fAcmeLastStatus <> 200 then begin
                            LogEvent('Failed to start Acme challenge: ' + FHttpRest.ResponseJson.S['type'] +
                                                                                     ', ' + FHttpRest.ResponseJson.S['detail']) ;
                            FAcmeRespNonce := ''; { V8.63  clear nonce since out of sequence now }
                            Exit;
                        end;
                        ChallgStatus := FHttpRest.ResponseJson.S['status'];
                        if ChallgStatus = 'valid' then begin
                            CIssueState := IssStateChallgOK;
                            LogEvent('Challenge passed OK for: ' + CDomain);
                        end
                        else begin
                            LogEvent('Challenge requested for: ' + CDomain);
                            CIssueState := IssStateChallgPend;
                            FPendingChallg := FPendingChallg + 1;  // pending challenges
                        end;
                    end;
                    DBWriteOneChallenge(FChallengeItems [I]);
                    FCertSubAltNames[CSanIdx].SAIssueState := CIssueState;
                    FCertSubAltNames[CSanIdx].SAStartDT := CStartDT;
                    FCertSubAltNames[CSanIdx].SADoneDT := CDoneDT;
                end;
            end;
        end;

      // challenges for all domains now started or validated, wait for Acme server to check them
        if FChallgStartDT < 10 then
            FChallgStartDT := Now;  { V8.64 now set when getting challenges }
        FIssueState := IssStateChallgPend;
        DBWriteCNDomain;

 // V9.5 add order to active list, which starts order timer so order completes automatically
        if FAutoOrderComplete then
            AddActiveOrder(FCertCommonName);
  //        FChkChallgTrg := IcsGetTrgSecs64 (10);   { V8.63 first check in 10 seconds }
        if FOrderTimer.Enabled then
            LogEvent('ACME certificate order placed, automatic collection enabled' + IcsCRLF)
        else
            LogEvent('ACME certificate order placed, manually collect when complete' + IcsCRLF);
        Result := True;
        if Assigned(FOnChallgRefresh) then
            FOnChallgRefresh(Self);  // V9.5 so app can update progress
    except
        on E:Exception do begin
            LogEvent ('Fatal AcmeV2StartChallgs protocol error: ' + E.Message);
        end;
    end;
 end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// check if challenge for a domain is completed
// beware, may not have common domain settings loaded
// V9.5  probably not used !!!!!
(*
function TSslX509Certs.AcmeV2CheckChallg(ChallgNum: Integer): Boolean;
var
    ValidJson, RecJson: ISuperObject;
    {ArrayChallg, ChallgJson,}
    ErrorJson: ISuperObject;
    ChallgStatus: string ;
    J: integer;
begin
    Result := False;
    with FChallengeItems [ChallgNum] do begin
        if (CDomain = '') or (ChallengeURL = '') then begin  { V8.64 }
            LogEvent('Invalid unused challenge');
            Exit;
        end;
        LogTimeStamp;
        LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Check if Challenge Done for: ' + CDomain);

    // must have a valid nonce to do POST requests
        if NOT AcmeCheckNonce then
            exit;

        if NOT AcmeGetRequest(httpPOST, ChallengeURL, '') then begin
            LogEvent('Failed to check ACME challenge: ' + FHttpRest.ResponseJson.S['type'] + ', ' + FHttpRest.ResponseJson.S['detail']) ;
            FAcmeRespNonce := ''; { V8.63  clear nonce since out of sequence now }
            Exit;
        end;
        if fAcmeLastStatus > 202 then
            Exit;  // not done
        DumpJson;
        ChallgStatus := FHttpRest.ResponseJson.S['status'];
        if ChallgStatus = 'pending' then begin
            LogEvent('Acme has not yet responded to challenge for: ' + CDomain);
            Exit;
        end;
        if ChallgStatus = 'processing' then begin
            LogEvent('Acme is still processing the challenge for: ' + CDomain);
            Exit;
        end;

     // success, keep validation details
        if ChallgStatus = 'valid' then begin
            CIssueState := IssStateChallgOK;
            CDoneDT := Now;
            Result := True;
            CValidResult := 'OK';
            ValidJson := FHttpRest.ResponseJson.O['validationRecord'];  { V8.65 moved from later }
            if ValidJson <> Nil then begin
                for J := 0 to ValidJson.AsArray.Length - 1 do begin // should only be one
                    RecJson := ValidJson.AsArray[J];
                    if (IcsIDNAToUnicode(RecJson.S['hostname']) = CDomain) then begin  { V8.64 }
                        if RecJson.S['url'] <> '' then
                            CValidResult := 'OK, URL: ' + RecJson.S['url']
                        else
                            CValidResult := 'OK, Hostname: ' + CDomain;
                        if (Length(RecJson.S['[addressUsed]']) > 6) then
                            CValidResult := CValidResult + ', IP address ' +  RecJson.S['[addressUsed]'];
                        LogEvent('Challenge validated: ' + CValidResult + '  for: ' + CDomain);
                        Break;
                    end;
                end;
           end;
        end

     // failed, try and find error
        else if ChallgStatus = 'invalid' then begin
            ErrorJson := FHttpRest.ResponseJson.O['error'];
            if ErrorJson <> Nil then
                CValidResult := 'Failed: ' + ErrorJson.S['detail']  { V8.65 }
            else
                CValidResult := 'Failed';
            LogEvent('Acme challenge ' + CValidResult + '  for: ' + CDomain);
            CIssueState := IssStateNone;
        end
        else begin
            CIssueState := IssStateNone;
            Exit;
        end;

    // delete challenge file if no longer need it
        if CType <> ChallDnsAuto then   { V8.71 not DNS for the moment, might be more than one }
            CleanupChallenge(FChallengeItems [ChallgNum]);  { V8.64 }

 (*
 Acme V2
Request completed: 200 OK
Response (length 554)
{
  "type": "http-01",
  "status": "valid",
  "url": "https://acme-v02.api.letsencrypt.org/acme/challenge/7Op_FW4ZzPftOFIZvkbGnjQjc_TRHjNEhgEvztvxJGo/5877772581",
  "token": "tEZim0F1rZEXZ6dSN8YlT-J4vYOrBEaRSaTGDXgh6mU",
  "validationRecord": [
    {
      "url": "http://test2.telecom-tariffs.co.uk/.well-known/acme-challenge/tEZim0F1rZEXZ6dSN8YlT-J4vYOrBEaRSaTGDXgh6mU",
      "hostname": "test2.telecom-tariffs.co.uk",
      "port": "80",
      "addressesResolved": [
        "217.146.115.84"
      ],
      "addressUsed": "217.146.115.84"
    }
  ]
}

Response (length 223)
{
  "type": "http-01",
  "status": "pending",
  "url": "https://acme-v02.api.letsencrypt.org/acme/challenge/pH4NQhnkRbK1m-VfSSCkijYDkPx8hmceZFUG6YVSrCU/5877772578",
  "token": "VGRCea7jdbIjQ9ND-KpRAd96r4hAfJXEMsxUKqZp1H8"
}

HTTP REST Response (length 454)
{
  "type": "dns-01",
  "status": "invalid",
  "error": {
    "type": "urn:ietf:params:acme:error:dns",
    "detail": "DNS problem: SERVFAIL looking up CAA for magsys.uk - the domain's nameservers may be malfunctioning",
    "status": 400
  },
  "url": "https://acme-v02.api.letsencrypt.org/acme/chall-v3/6620893218/-plNHA",
  "token": "h0xtJh-0MYJNGKj5jCElB-XOFZS8pCjyVBHPO0Hh4TE",
  "validationRecord": [
    {
      "hostname": "magsys.uk"
    }
  ]
}

Response xx
{
  "identifier": {
    "type": "dns",
    "value": "ftptest.org"
  },
  "status": "invalid",
  "expires": "2018-07-31T16:41:05Z",
  "challenges": [
    {
      "type": "dns-01",
      "status": "invalid",
      "error": {
        "type": "urn:ietf:params:acme:error:dns",
        "detail": "DNS problem: NXDOMAIN looking up TXT for _acme-challenge.ftptest.org",
        "status": 400
      },
      "url": "https://acme-v02.api.letsencrypt.org/acme/challenge/SRE4O3zoMyvXzgJRN6HCxUfU0tMfIgmEcOEqZuJPFoE/5907963322",
      "token": "jjtcTRjzhJgNl1CyQsY967xdsWDL2ViQYpRdgx6OvcU"
    }
  ],
  "wildcard": true
}

{
  "type": "dns-01",
  "status": "valid",
  "url": "https://acme-v02.api.letsencrypt.org/acme/chall-v3/3957825699/Hw_0xA",
  "token": "6GUNvnPDs0xn1TCcGzKa2Ukz1hj-Cu3Tl3_bXXlwhDg",
  "validationRecord": [
    {
      "hostname": "ftptest.co.uk"
    }
  ]
}
{
  "identifier": {
    "type": "dns",
    "value": "test5.telecom-tariffs.co.uk"
  },
  "status": "valid",
  "expires": "2020-04-18T16:38:49Z",
  "challenges": [
    {
      "type": "tls-alpn-01",
      "status": "valid",
      "url": "https://acme-v02.api.letsencrypt.org/acme/chall-v3/3449781566/ShFmZw",
      "token": "jHMLZhMTzqjuR_536ujGhgry-w8unP8qaojj87MHUQk",
      "validationRecord": [
        {
          "hostname": "test5.telecom-tariffs.co.uk",
          "port": "443",
          "addressesResolved": [
            "217.146.115.85"
          ],
          "addressUsed": "217.146.115.85"
        }
      ]
    }
  ]
}


*)
     // success, keep validation details
 (*       if CIssueState = IssStateChallgOK then begin
            ValidJson := FHttpRest.ResponseJson.O['validationRecord'];
            if ValidJson <> Nil then begin
                for J := 0 to ValidJson.AsArray.Length - 1 do begin // should only be one
                    RecJson := ValidJson.AsArray[J];
                    if (IcsIDNAToUnicode(RecJson.S['hostname']) = CDomain) then begin  { V8.64 }
                        if RecJson.S['url'] <> '' then
                            CValidResult := 'OK, URL: ' + RecJson.S['url']
                        else
                            CValidResult := 'OK, Hostname: ' + CDomain;
                        if RecJson.S['[addressUsed]'] <> '' then CValidResult :=
                            CValidResult + ', IP address ' +  RecJson.S['[addressUsed]'];
                        LogEvent('Challenge Validated: ' + CValidResult + '  for: ' + CDomain);
                        Break;
                    end;
                end;
           end;
        end

     // failed, try and find error
        else if CIssueState = IssStateNone then begin
            ArrayChallg := FHttpRest.ResponseJson.O['challenges'];  // array of challenges
            if ArrayChallg <> Nil then begin
                for J := 0 to ArrayChallg.AsArray.Length - 1 do begin
                    ChallgJson := ArrayChallg.AsArray[J];
                    if ChallgJson.S['status'] = ChallgStatus then begin
                        ErrorJson := ChallgJson.O['error'];
                        if ErrorJson <> Nil then CValidResult := ErrorJson.S['detail'];
                        LogEvent('Challenge Failed: ' + CValidResult + '  for: ' + CDomain);
                        break;
                    end;
                end;
            end
            else begin
                ErrorJson := FHttpRest.ResponseJson.O['error'];
                if ErrorJson <> Nil then CValidResult := ErrorJson.S['detail'];
                LogEvent('Challenge Failed: ' + CValidResult + '  for: ' + CDomain);
            end;
        end;    *)

    // update sub domains with progress
    // beware we may not have domain loaded, need to check it, may need it to collect certs
(*        if (FCertCommonName <> CCommonName) then
            DBReadCNDomain(CCommonName);
        if (FCertCommonName = CCommonName) then begin
            if FPendingChallg > 0 then FPendingChallg := FPendingChallg - 1;  // pending challenges
            if (CSanIdx >= 0) then begin
                FCertSubAltNames[CSanIdx].SAIssueState := CIssueState;
                if  FCertSubAltNames[CSanIdx].SADoneDT < 10 then
                    FCertSubAltNames[CSanIdx].SADoneDT := CDoneDT;
                FCertSubAltNames[CSanIdx].SAValidResult := CValidResult;
            end;
            DBWriteCNDomain;
        end
        else
            LogEvent('Challenge checking failed to read Common Name domain: ' + CCommonName);
    end;

  // upodate database and file
    DBWriteOneChallenge(FChallengeItems [ChallgNum]);

end;    *)


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ once challenges are all completed OK, finalise order so certificate is issued, note may not happen immediately }
{ must have used DBReadCNDomain to set all domain variables }
function TSslX509Certs.AcmeV2OrderFinal(LogErrors: Boolean = True): Boolean;   { V9.5 split AcmeV2GetCert into three functions }
var
    CSREn, errstr: string ;
    AcmeJson: ISuperObject;
begin
    Result := False;
    LogTimeStamp;
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    if (Pos ('.', FCertCommonName) = 0) or (FCertSubAltNames.Count = 0) then begin      { V9.1 was blank }
        if LogErrors then  { V8.64 }
            LogEvent('Must specify domain Common Name for certificate');
        Exit;
    end;
     if FIssueState < IssStateChallgReq then begin
        LogEvent('No ACME challenge requested, must get challenges first');
        Exit;
    end;
    LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Finalize order for: ' + FCertCommonName);
   if NOT (FIssueState in [IssStateChallgOK, IssStateFinalPend]) then begin
        if LogErrors then
            LogEvent('Order is not ready to finalize');
        Exit;
    end;
    RemoveOldChallgs(fCertCommonName);    // should have been done earlier
    if FChallgDoneDT = 0 then
        FChallgDoneDT := Now;
    FOrderCertsDT := Now;
    FCertRenewNow := False;
 { V8.65 don't need web server any longer, stops hacking after certificate issued }
    StopDomSrv;
    DBWriteCNDomain;

 // finalise order, with certificate service request, issues certificate, but may not ready to collect immediately
    try

    // work file names, in account directory, with orderid (no work names)
    // fail now if can not create directories
        fAcmeCertLines := '';
        fNewInterLines := '';
        FNewSslCert.ClearAll;
        if NOT SetPartFNames (False) then
            Exit ;
        SetFullFileNames (FPartFNameWork) ;

      // create private key and certificate service request
        if NOT CreateKeyandReq then
            exit ;

      // Acme needs DER request UrlBase64 encoded no headers, not PEM base64
        CSREn := IcsBase64UrlEncode(String(FNewSslCert.SaveReqToDERText));
        AcmeJson := SO([ 'csr', CSREn]);    { V9.5 }

     // order certificate
        LogEvent('Finalising order, sending CSR for SSL certificate to be issued: ' + fCertCommonName);
        if NOT AcmeGetRequest(httpPOST, fAcmeOrderFinalizeURL, AcmeJson.AsJson(False, False) ) then
            Exit;
        if fAcmeLastStatus > 200 then begin
            errstr := FHttpRest.ResponseJson.S['type'];
            LogEvent('FFailed to finalize order: ' + errstr  + ', ' + FHttpRest.ResponseJson.S['detail']) ;
            FAcmeRespNonce := ''; { V8.63  clear nonce since out of sequence now }

         // one repeat for badnonce
            if Pos('badNonce', errstr) = 0 then
                Exit;
            if NOT AcmeGetRequest(httpHEAD, AcmeActionDirs [AcmeNewNonce].URL, '') then
                exit;
            if NOT AcmeGetRequest(httpPOST, fAcmeOrderFinalizeURL, AcmeJson.AsJson(False, False) ) then
                Exit;
            if fAcmeLastStatus > 200 then begin
                errstr := FHttpRest.ResponseJson.S['type'];
                LogEvent('Failed to finalize order: ' + errstr  +  ', ' + FHttpRest.ResponseJson.S['detail']) ;
                Exit;
            end;
        end;
        DumpJson;
(*  {
  "status": "valid",
  "expires": "2018-03-26T16:57:17Z",
  "identifiers": [
    {
      "type": "dns",
      "value": "test3.telecom-tariffs.co.uk"
    }
  ],
  "authorizations": [
    "https://acme-staging-v02.api.letsencrypt.org/acme/authz/6mDYqM5A5a7L3QnD0FxOUYI5FflSq-3MMqvfOWYPxcg"
  ],
  "finalize": "https://acme-staging-v02.api.letsencrypt.org/acme/finalize/5763117/97378",
  "certificate": "https://acme-staging-v02.api.letsencrypt.org/acme/cert/fac3d324243e1a7c73126018c851287377b5"
}
*)

   //  V9.5 update IssueState according to order object
        ProcOrderStatus;

      // some fatal error
        if (FIssueState = IssStateNone) then begin
            LogEvent('ACME certificate order failed, start again' + IcsCRLF);
            FAcmeRespNonce := '';
            FChallgExpireDT := 0;
        // see if closing account
            if FAutoAccountClose then
                FPendAccountClose := True; { V8.63 }
        end

    // async certificate processing, need to wait a fewe seconds
        else if (FIssueState = IssStateIssuePend) then begin
            LogEvent('Certificate issued OK, but not ready to collect, retry in a few seconds');
            Result := True;
        end

    // worked OK
        else if (FIssueState = IssStateIssued) then begin
            if (fAcmeCertURL = '') then
                LogEvent('Certificate issued OK, but no certificate URL')
            else begin
                LogEvent('Certificate issued OK, ready to collect');
                Result := True;
            end;
        end
        else begin
            LogEvent('Failed to finalize order, status: ' + fAcmeOrderStatus);
        end;
        DBWriteCNDomain;  // update database

    // V9.5 add order so it completes automatically, should have been done when challenges started
        if FAutoOrderComplete then
            AddActiveOrder(FCertCommonName);

        if Assigned(FOnChallgRefresh) then
            FOnChallgRefresh(Self);  // V9.5 so app can update progress
    except
        on E:Exception do begin
            LogEvent ('Fatal AcmeV2OrderFinal protocol error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ once certificate jas been issued, collect it and distribute files everywhere }
function TSslX509Certs.AcmeV2CollectCert: Boolean;   { V9.5 split AcmeV2GetCert into three functions }
var
    I: Integer;
begin
    Result := False;
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', collect certificate for: ' + FCertCommonName);
    if (FIssueState >= IssStateFinished) then begin
        LogEvent('Certificate order previously finished, can no longer collect again');
        Exit;
    end;
    if (FIssueState = IssStateIssuePend) then begin
        if NOT AcmeV2OrderStatus then    // see if order status has changed
            Exit;
        if (FIssueState = IssStateIssuePend) then begin
           LogEvent('Certificate issued OK, but not ready to collect, retry in a few seconds');
           exit;
        end;
    end;
    if (FIssueState < IssStateIssued) or (fAcmeCertURL = '') then begin
        LogEvent('Unable to collect certificate, must finailize order first');
        Exit;
    end;

    try
     // download certificate raw lines or PEM chain
        LogEvent ('Certificate download URL: ' + fAcmeCertURL);
        if (NOT AcmeGetRequest(httpPOST, fAcmeCertURL, '')) then
            exit;
        if fAcmeLastStatus > 202 then begin
            LogEvent('Failed to download SSL certificate from: ' + fAcmeCertURL);
            Exit;
        end;
        I := LastDelimiter('/', fAcmeCertURL);
        if I > 0 then begin
            fAcmeCertSerial := Copy (fAcmeCertURL, I + 1, 999);
            LogEvent ('Certificate serial: ' + fAcmeCertSerial);
        end;

    // do we need to load private key
        if NOT FNewSslCert.IsPKeyLoaded then begin
            try
                LogEvent('Loading old private key from: ' + fFilePrvKey);
                FNewSslCert.PrivateKeyLoadFromPemFile(fFilePrvKey, FPrivKeyPassword);
            except
                on E:Exception do
                begin
                    LogEvent ('Failed to load old private key: ' + E.Message);
                    Exit;
                end;
            end;
        end;
        fNewCertLines := fAcmeCertLines;
        LogEvent ('Downloaded Acme certificate(s):' + IcsCRLF + fAcmeCertLines);

    // save lots of certificates in different formats and places, V9.5 failed is fatal error
        if NOT SaveCertificateFiles(fCertCommonName) then begin
            FIssueState := IssStateNone;
            LogEvent('ACME certificate collection and save failed, start again' + IcsCRLF);
            RemoveActiveOrder(FCertCommonName);
            FAcmeRespNonce := '';
            DBWriteCNDomain;
        // see if closing account
            if FAutoAccountClose then
                FPendAccountClose := True; { V8.63 }
            Exit;
        end;

    // all done
        FIssueState := IssStateCollected;
        for I := 0 to FCertSubAltNames.Count - 1 do begin
            FCertSubAltNames[I].SAIssueState := FIssueState;
            FCertSubAltNames[I].SADoneDT := FOrderCertsDT;
        end;
        DBWriteCNDomain;

    // delete old challenges
        RemoveOldChallgs(fCertCommonName);

    // V9.5 remove active name, before calling event
        RemoveActiveOrder(FCertCommonName);   // finsihed

    // tell application order finished, may close account
        if Assigned(FOnNewCert) then
            FOnNewCert(Self);

    // see if closing account
        if FAutoAccountClose then
            FPendAccountClose := True; { V8.63 }

        Result := True;
    except
        on E:Exception do begin
            LogEvent ('Fatal ACME protocol error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.5 for a specific certificate, find when renewal is recommended }
{ CertRenewalId is built from certificate Authority Key Id and serial number }
{ sets CertRenewNow if new cert needed, returns true if dates found }
function TSslX509Certs.AcmeV2RenewalInfo: Boolean;     { V9.5 }
var
    ReqURL: string ;
    AcmeJson: ISuperObject;
    RenewStartDT, RenewEndDT: TDateTime;
begin
    Result := False ;
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Getting Renewal Info for: ' + FCertCommonName);
    if FCertRenewNow and (FIssueState <> IssStateCollected) then begin      // nothing more to do
        LogEvent('Old Certificate should be renewed now');
        Result := True;   // save to database
        Exit;
    end;
    if FCertRenewalId = '' then begin
        LogEvent('No Certificate Renewal Identfifier');
        Exit;
    end;
    ReqURL := AcmeActionDirs [AcmeRenewalInfo].URL;
    if ReqURL = '' then begin
        LogEvent('No Acme Renewal URL');
        Exit;
    end;
    if FNewCertEndDT < 10 then begin
        LogEvent('No Old Certificate Expiry Date');
        Exit;
    end;

// if challenges have expired after a few days, update issue state to finished
// until challenges expire, order can be collected again
    if FIssueState = IssStateCollected then begin
        if (FChallgExpireDT > 10) and (FChallgExpireDT < Now) then begin  // order has expired
            FAcmeOrderObjUrl := '';  // URL no longer works, clear it
            FAcmeOrderFinalizeUrl := '';
            FChallgExpireDT := 0;
            FIssueState := IssStateFinished;
            FCertRenewNow := False;
            Result := True;  // save to database
        end;
    end;
    ReqURL := ReqURL + '/' + FCertRenewalId;
    FCertRenewRetryDT := 0;
    FCertRenewCheckDT := Now;
    FCertRenewDays := 0;
    if FNewCertEndDT < Now then begin
        LogEvent('Old certificate has expired, renew now');
        FCertRenewNow := True;
        Result := True;
        Exit;
    end;
    try
        if NOT AcmeGetRequest(httpGET, ReqURL, '') then
            exit;
        DumpJson;
        if fAcmeLastStatus <> 200 then begin
            LogEvent('Acme could not find certificate info for: ' + FCertCommonName);
        end
        else begin
            AcmeJson := FHttpRest.ResponseJson.O['suggestedWindow'];
            if NOT Assigned(AcmeJson) then
                Exit;
            RenewStartDT := AcmeJson.DT['start'];   // returns time with Z (UTC), are we converting to localtime correctly???
            RenewEndDT := AcmeJson.DT['end'];
            Result := True;

        // expire days -1 means immediately
            if RenewStartDT < Now then begin
                FCertRenewNow := True;
            end
            else
                FCertRenewDays := Trunc(FNewCertEndDT - RenewStartDT);
            if FHttpRest.RespRetryDT > FCertRenewCheckDT then
                FCertRenewRetryDT := FHttpRest.RespRetryDT;
            LogEvent(FCertCommonName + ': Recommnded Expiry Days ' + IntToStr(FCertRenewDays) +
                                                            ', Next Recheck ' + IcsDateTimeToAStr(FCertRenewRetryDT));
            LogEvent(FCertCommonName + ': Recommnded Renewal Date Period ' + IcsDateTimeToAStr(RenewStartDT) +
                                                                               ' to ' + IcsDateTimeToAStr(RenewEndDT));
        end;
   except
        on E:Exception do begin
            LogEvent ('Fatal ACME RenewalInfo protocol error: ' + E.Message);
        end;
   end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.AcmeV2OrderCancel (Revoke: Boolean): Boolean;  { V8.64 }
var
    CertDerEn, CertFile: string ;
  {  ArrayAuthz, IdentsJson, }AcmeJson: ISuperObject;
    I, TotAuth: Integer;
    ErrFlag: Boolean;
begin
    Result := false ;
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Cancel Order for: ' + FCertCommonName);
    fAcmeOrderStatus := '';
    FAcmeOrderFinalizeUrl := '';
    FAcmeOrderObjUrl := '';

    if (Pos ('.', FCertCommonName) = 0) then begin      { V9.1 was blank }
        LogEvent('Must specify domain Common Name for certificate');
        Exit;
    end;

 // read internal variables and public properties from database, so same as order
    if NOT DBReadCNDomain(fCertCommonName) then
        Exit;
    if (FAcmeOrderObjUrl = '') then begin
        LogEvent('No ACME order found in database');
        Exit;
    end;
    if (FIssueState = IssStateCancel) and (NOT Revoke) then begin
        LogEvent('ACME order already cancelled');
        Exit;
    end;
    LogEvent('Starting to cancel ACME order for: ' + fCertCommonName);

    try

    //get ACME order object, with challenge authorizations URL list
        if NOT AcmeV2OrderStatus then   { V9.5 simplify }
            Exit;
        if fAcmeOrderStatus = 'invalid' then begin
            LogEvent('ACME order state invalid for: ' + fCertCommonName);
            FIssueState := IssStateNone;
        end
        else begin
            TotAuth := Length(FAcmeOrderDomains);
            if TotAuth = 0 then begin
                LogEvent('Can not find any domain order authorisations to cancel');
                Exit;
            end;
            ErrFlag := False;

         // now deactiveate authorisations
            for I := 0 to TotAuth - 1 do begin
                AcmeJson := SO([ 'status', 'deactivated']);  { V9.5 }
                if NOT AcmeGetRequest(httpPOST, FAcmeOrderDomains[I].AuthzURL, AcmeJson.AsJson(False, False)) then
                    exit;
                if fAcmeLastStatus <> 200 then begin
                    LogEvent('Failed to get ACME challenges: ' + FHttpRest.ResponseJson.S['type'] +
                                                                                ', ' + FHttpRest.ResponseJson.S['detail']) ;
                    ErrFlag := True;
                    Continue;
                end;
                DumpJson;
                LogEvent('Deactivated ACME challenge for ' + FAcmeOrderDomains[I].IdValue + ' Wildcard=' +
                                FHttpRest.ResponseJson.S['wildcard'] + ', Status: ' + FHttpRest.ResponseJson.S['status']);
            end;
            if ErrFlag then
                LogEvent('Failed to cancel ACME order for: ' + fCertCommonName)
            else begin
                FIssueState := IssStateCancel;
                LogEvent('Cancelled ACME order OK for: ' + fCertCommonName);
                Result := True;
            end;

         // delete old challenges
            RemoveOldChallgs(fCertCommonName);
            DBWriteCNDomain;  // update database
        end;
     except
        on E:Exception do begin
            LogEvent ('Fatal ACME protocol error: ' + E.Message);
        end;
    end;

  // for revoke, we need the old certificate
    if Revoke then begin
       Result := False;
       CertFile := FFileFinalCert;
       if NOT FileExists (CertFile) then begin
           CertFile := FFileFinalBundle;    { V8.69 no single file, try bundle }
           if NOT FileExists (CertFile) then begin
                LogEvent ('Can not find cetificate for this order: ' +  CertFile);
                Exit ;
           end;
        end;

        try
            FNewSslCert.ClearAll ;
            FNewSslCert.LoadFromFile (CertFile, croNo, croNo, FPrivKeyPassword);  { V8.69 may need password, load P12  }
        except
            on E:Exception do begin
                LogEvent ('Failed to load certificate file: ' + CertFile + ' - ' + E.Message);
                Exit ;
            end;
        end;
        if NOT FNewSslCert.IsCertLoaded then begin
            LogEvent ('Failed to load cetificate for this order: ' +  CertFile);
            Exit ;
        end;

  // Acme needs DER certificate UrlBase64 encoded no headers, not PEM base64
        LogEvent ('Requesting revocation of certificate: ' +  CertFile);
        CertDerEn := IcsBase64UrlEncode(String(FNewSslCert.SaveCertToDERText));
        AcmeJson := SO([ 'certificate', CertDerEn, 'reason', 4]);  { V9.5 }

    // reason 0=unspecified, 1=key compromise, 2=ca compromise, 3=affilication changed,
    // 4=superseded, 5=ceased, 6-cert hold, 8=remove from CRL, 9=priv withdrawn, 10=aAcomprmise
        if NOT AcmeGetRequest(httpPOST, AcmeActionDirs [AcmeRevokeCert].URL, AcmeJson.AsJson(False, False)) then
            exit;
        if fAcmeLastStatus <> 200 then
            LogEvent('Failed to revoke certificate: ' + FHttpRest.ResponseJson.S['type'] + ', ' + FHttpRest.ResponseJson.S['detail'])
        else begin
            LogEvent ('Revoked certificate OK: ' +  CertFile);
            FIssueState := IssStateCancel;
            DBWriteCNDomain;  // update database
            Result := True;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// remove all challenges for certificate common name, once completed
procedure TSslX509Certs.RemoveOldChallgs(const CNDomain: String);
var
    I: Integer;
begin
    if FChallengesTot = 0 then
        Exit;
    for I := 0 to Length(FChallengeItems) - 1 do begin
        with FChallengeItems [I] do begin
            if (CCommonName = CNDomain) then begin
             // delete old challenge files
                CleanupChallenge(FChallengeItems [I]);   { V8.64 }
                DBDeleteChallenge(CDomain, CWildcard);  // remove from database
                DBRemoveChallenge(I);                   // remove from array
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// add active ordrer to list, start timer
procedure TSslX509Certs.AddActiveOrder(const CName: String);     { V9.5 }
var
    Idx: Integer;
begin
    if NOT FActiveOrders.Find(CName, Idx) then
        FActiveOrders.Add(CName);
    FOrderTimer.Enabled := True;
    LogEvent ('Automatic order completion started for: ' + CName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// remove active ordrer from list, stop timer is none left
procedure TSslX509Certs.RemoveActiveOrder(const CName: String);       { V9.5 }
var
    Idx: Integer;
begin
    if FActiveOrders.Count = 0 then
        Exit;
    if NOT FActiveOrders.Find(CName, Idx) then
        Exit;
    FActiveOrders.Delete(Idx);
    if FActiveOrders.Count = 0 then
        FOrderTimer.Enabled := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// check for order status changes, by completed challenges or order finalisation
procedure TSslX509Certs.OrderOnTimer(Sender: TObject);       { V9.5 }
var
    CurCommonName: String;
    I: Integer;
begin
    if FActiveOrders.Count = 0 then
        Exit;
    if FX509BusyFlag then
        Exit;
    FOrderTimer.Enabled := False;
    FX509BusyFlag := True;
    try // finally

        try  // except
            for I := 0 to FActiveOrders.Count -1 do begin
                CurCommonName := FActiveOrders[I];

             // if handline more than one order, load it
                if FCertCommonName <> CurCommonName then begin
                    if NOT DBReadCNDomain(CurCommonName) then begin
                        RemoveActiveOrder(CurCommonName);   // dead, can not process order
                        Exit;
                    end;
                end;

             // certificate challenges not started, give up
                 if FIssueState < IssStateChallgPend then begin
                        RemoveActiveOrder(CurCommonName);
                        Exit;
                    end;

            // get current order status
                if NOT AcmeV2OrderStatus then begin
                    RemoveActiveOrder(CurCommonName);   // dead, can not process order
                    Exit;
                end;

            // temp diag
                LogEvent('Order Timer, Issue State ' + IssueStateLits[IssueState]  + ' for ' + CurCommonName);

            // waiting for challenges to process, nothing more to do
                if FIssueState < IssStateChallgOK then begin
                    Continue;
                end;

            // challenges done, finalize order, which starts creation of certificates
                if (FIssueState in [IssStateChallgOK, IssStateFinalPend]) then begin
                    RemoveOldChallgs(CurCommonName);   // delete challenge now completed
                    if NOT AcmeV2OrderFinal(True) then begin
                        RemoveActiveOrder(CurCommonName);   // dead, can not process order
                        Exit;
                    end;
                end;

            // waiting for certifcate to be issued
                if FIssueState = IssStateIssuePend then begin
                    Continue;
                end;

            // if certificate issued, collect it
                if FIssueState >= IssStateIssued then begin
                    LogEvent(TimeToStr(Now) + ' Order Completed, Collecting Certificate: ' + FCertCommonName);
                    AcmeV2CollectCert;
                    if (FIssueState >= IssStateCollected) then begin
                        RemoveActiveOrder(CurCommonName);   // finsihed
                        RemoveOldChallgs(CurCommonName);   // delete challenge now completed
                        Exit;
                    end;
                    LogEvent(TimeToStr(Now) + '  Failed to Collect Ccertificate: ' + FCertCommonName);
                end;
           end;
        except
            on E:Exception do begin
              LogEvent ('Failed to read order in timer: ' + E.Message);
            end;
        end;

    finally
        FX509BusyFlag := False;

      { order finished should we stop server and close account }
        if (( FActiveOrders.Count = 0) and FPendAccountClose) or (IcsElapsedMins64(FAccountLastTick) >= FAccountTimeOutMins) then begin
            LogEvent ('No waiting orders, stopping timer, closing account');
            StopDomSrv;
            CloseAccount;
            FPendAccountClose := False;
        end
        else
           FOrderTimer.Enabled := True;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// is supplier account open
function TSslX509Certs.IsAccountOpen: Boolean;                            { V9.5 }
begin
    Result := False;
    if (FSupplierProto = SuppProtoNone) or (FControlFile = Nil) or
                                   (FCnrtFileName = '') { or (FDirCertWork = '') } then
        Exit;
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// close certificate supplier account, close database INI file, should be no pending writes!!
function TSslX509Certs.CloseAccount: Boolean;
begin
    Result := False;
    if (FSupplierProto > SuppProtoNone) then  // probably closed already
        LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Closing Account for ' + FSupplierTitle);
    FOrderTimer.Enabled := False;
    FActiveOrders.Clear;
    FPendAccountClose := False;
    StopDomSrv;
    if (FSupplierProto = SuppProtoNone) and (FControlFile = Nil) and (FCnrtFileName = '') then
        Exit;
    FSupplierProto := SuppProtoNone;
    fAcmeRespNonce := '';  { V8.65 }
    FDirCertWork := '';
    FIssueState := IssStateNone;
    FCertRenewNow := False;     { V9.5 }
    FCnrtFileName := '';
    FreeAndNil(FControlFile);
    SetLength(FDomainItems, 0);
    FChallengesTot := 0;
    SetLength(FChallengeItems, 0);
    FCertSANs.Clear;
    ClearAccount;
    LogEvent ('Acme Account Closed OK');
    Result := True;
    if Assigned(FOnChallgRefresh) then
        FOnChallgRefresh(Self);  // V9.5 so app can update progress
    if Assigned(FOnSuppDBRefresh) then
        FOnSuppDBRefresh(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// open certificate supplier account from a specific directory
// optionally create new account if not found
// sets DbCNDomains list of certificate common name domains created using
// the account
(* function TSslX509Certs.OpenAccount(const WorkDir: String; CreateNew: Boolean = False): Boolean;
var
    NewProto: TSupplierProto ;
    I: Integer;
begin
    Result := False;
    LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Opening Account for ' + FSupplierTitle);
    NewProto := FSupplierProto ;  { V8.63 }
    FSupplierProto := SuppProtoNone;
    FCertRenewNow := False;     { V9.5 }
    CloseAccount;
    FAccountLastTick := IcsGetTickCount64;  { V8.63 }
    FLastError := '';                       { V8.63 }
    if FAccountTimeOutMins <= 2 then
        FAccountTimeOutMins := 10; { V8.63 }
    fAcmeRespNonce := '';  { V8.65 }

  // read everything from database
    if NOT DBReadAccount(IncludeTrailingPathDelimiter(WorkDir)) then begin
        if NOT CreateNew then
            Exit;
        LogEvent ('Old account not found, creating new account in: ' + WorkDir);
        FSupplierProto := NewProto ;   { V8.63 }
    end;
    if FSupplierProto = SuppProtoAcmeV2 then
        Result := SetAcmeAccount (CreateNew)
    else begin
        LogEvent ('Can not open account, unknown supplier protocol');
    end;

 // check for old challenges, start timer to see if done
    if Result then begin
        LogEvent ('Opened supplier account for: ' + FSupplierTitle +  ', protocol: ' +
                                                            SupplierProtoLits [FSupplierProto] + ', From: ' + WorkDir);
        DBReadChallenges;
        if FChallengesTot > 0 then begin
            LogEvent ('Found Old Challenges, Trying to Complete Orders: ' + IntToStr(FChallengesTot));
            for I := 0 to FChallengesTot - 1 do begin
                AddActiveOrder(FChallengeItems [I].CCommonName) ;
            end;
         end;
     end
     else begin
         FSupplierProto := SuppProtoNone;
         FLastError := FLastResponse;   { V8.63 }
     end;
    if Assigned(FOnSuppDBRefresh) then
        FOnSuppDBRefresh(Self);
end;   *)


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// cancel order, still pending or completed, may get refund for commercial order
function TSslX509Certs.CertCancelDomain(const aDomain: String): Boolean;
begin
    Result := false ;
    FLastError := '';                       { V8.63 }
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Starting cancel order: ' + aDomain); { V8.65 }
    if aDomain <> fCertCommonName then begin
        if NOT DBReadCNDomain(aDomain) then   { V9.5 }
            Exit;
    end;
    if FIssueState < IssStateChallgPend then begin
        LogEvent('Can not cancel, no pending order');
        Exit;
    end;
    if SupplierProto = SuppProtoAcmeV2 then
        Result := AcmeV2OrderCancel(False)
     else begin
        LogEvent('Can not cancel order, unknown supplier protocol');
     end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// cancel order, still pending or completed, may get refund for commercial order
// also revoke certificate if already issued
function TSslX509Certs.CertRevokeDomain(const aDomain: String): Boolean;
begin
    Result := false ;
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Starting cancel and revoke for order: ' + aDomain); { V8.65 }
    if aDomain <> fCertCommonName then begin
        if NOT DBReadCNDomain(aDomain) then   { V9.5 }
//      if NOT CertReadDomain(aDomain) then
            Exit;
    end;
    if FIssueState < IssStateCollected then begin
        LogEvent('Can not revoke, no certificate collected');
        Exit;
    end;
    if SupplierProto = SuppProtoAcmeV2 then
        Result := AcmeV2OrderCancel(True)
 {   else if SupplierProto = SuppProtoCertCentre then
        Result := CCCancelOrder(True)
    else if SupplierProto = SuppProtoServtas then
      //  Result := ServtasCancelOrder(True);  }
    else begin
        LogEvent('Can not cancel order, unknown supplier protocol');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// redistribute previously collected certificate, locally and to servers
function TSslX509Certs.CertRedistDomain(const aDomain: String): Boolean;
begin
    Result := false ;
    if aDomain <> fCertCommonName then begin
      if NOT DBReadCNDomain(aDomain) then   { V9.5 }
//    if NOT CertReadDomain(aDomain) then
            Exit;
    end;
    Result := RedistribteFiles;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// remove domain from database
function TSslX509Certs.CertRemoveDomain(const aDomain: String): Boolean;
begin
    Result := false ;
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    if aDomain <> fCertCommonName then begin
        if NOT DBReadCNDomain(aDomain) then   { V9.5 }
//      if NOT CertReadDomain(aDomain) then
        Exit;
    end;
    RemoveOldChallgs(aDomain);
    Result := DBDeleteCNDomain(aDomain);
    if Result then
        LogEvent('Common Name domain removed from database: ' + aDomain);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// reset domain order in database, to start again
function TSslX509Certs.CertResetDomain(const aDomain: String): Boolean;      { V9.2 }
begin
    Result := false ;
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    if aDomain <> fCertCommonName then begin
       if NOT DBReadCNDomain(aDomain) then   { V9.5 }
   //  if NOT CertReadDomain(aDomain) then
        Exit;
    end;
    RemoveOldChallgs(aDomain);
    FIssueState := IssStateNone;
    FCertRenewNow := False;
    Result := DBWriteCNDomain;
    if Result then
        LogEvent('Reset domain order in database: ' + aDomain);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// V9.5 check if domain should be renewed now
{ sets CertRenewNow if new cert needed, returns true if dates found }
function TSslX509Certs.CertRenewalDomain(const aDomain: String): Boolean;  { V9.5 }
var
   TestCert: TX509Base;
   LocalRenewDT: TDateTime;
   FName, Errs: String;
begin
    Result := false ;
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;

 // very important we don't interrupt an order
    if { (FIssueState <> IssStateNone) or } (FActiveOrders.Count > 0) then begin
        LogEvent('Renewal Checking Skipped, Order in Progress');
        Exit;
    end;
    LogEvent('Acme Supplier: ' + AcmeSupplierLits[FAcmeSupplier] + ', Renewal Checking for ' + aDomain);
    if aDomain <> fCertCommonName then begin
        if NOT DBReadCNDomain(aDomain) then   { V9.5 }
            Exit;
    end;
    if SupplierProto <> SuppProtoAcmeV2 then
        Exit;

// within renew days already based on certificate dates, if we have them
    if (FCertRenewDays > 0) and (FNewCertEndDT > 10) then begin
        LocalRenewDT:= FNewCertEndDT - FCertRenewDays;
        if (LocalRenewDT < Now) then begin
            FCertRenewNow := True;
            LogEvent('Order Renew Date Reached: ' + IcsDateTimeToAStr(LocalRenewDT) + ': ' + aDomain);
            Exit;
        end;
    end;

// still have valid information, unless waiting to update Collected to Finished
    if (FIssueState <> IssStateCollected) and ((FCertRenewRetryDT >= 10) and (FCertRenewRetryDT > Now)) then begin
        Result := True;
        LogEvent('Next Renewal Retry Check ' + IcsDateTimeToAStr(FCertRenewRetryDT) + ': ' + aDomain);
        Exit;
    end;

// old order without renewal information or certificate dates due to ordering failure
    if (FCertRenewalId = '') or (FNewCertStartDT < 10) or  (FNewCertEndDT < 10)  then begin
        TestCert := TX509Base.Create(Nil);
        try
            try
                FName := FFileFinalCert;
                if NOT FileExists(FName) then
                    FName := FFileFinalBundle;
                if NOT FileExists(FName) then
                    FName := FPartFNameFinal + FileSuffBundPem;
                 if NOT FileExists(FName) then
                    FName := FPartFNameFinal + FileSuffBundP12;
               if FileExists(FName) then
                    TestCert.LoadFromFileEx(FName, croNo, croNo, FPrivKeyPassword, Errs)
                else
                    Errs := 'Can Not Find Any Old Files';
                if Errs = '' then begin
                    FCertRenewalId := GetAcmeRenewalInfo(TestCert);  { builds ID from certificate AuthorityKeyId and SerialNum }
                    FNewCertEndDT := TestCert.ValidNotAfter;
                    FNewCertStartDT := TestCert.ValidNotBefore;
                    FCertRenewDays := Trunc(FNewCertEndDT - FNewCertStartDT) div 3;  // V9.5 temporary in case renewal info fails
                    DBWriteCNDomDates;
                    LogEvent('Updated Lost Dates From Old Certificate: ' + FName);
               end
               else
                    LogEvent ('Failed to Open Old Certificate: ' + Errs + ' - ' + FName);
            except
                on E:Exception do begin
                    LogEvent ('Failed to Open Old Certificate: ' + E.Message + ' - ' + FName);
                    Exit;
                end;
            end;
        finally
            TestCert.Free;
        end;
    end;

// get and keep latest dates
    Result := AcmeV2RenewalInfo;
    if NOT Result then begin
        LogEvent ('Failed to Find Renewal Dates, CertRenewalDomain');
        Exit;
    end;
    if NOT DBWriteCNDomDates then
        LogEvent ('Failed to Save Renewal Dates to Database, CertRenewalDomain');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// get order result, as best we can for logging and emails
function TSslX509Certs.GetOrderResult: String;
begin
    if NOT IsAccountOpen then begin
        FLastError := 'Must open certificate account first';   { V9.5 }
        LogEvent(FLastError);
        Exit;
    end;
    Result := 'Account Database Supplier: ' + FSupplierTitle + IcsCRLF +
              'Supplier: Protocol: ' + SupplierProtoLits [FSupplierProto] + IcsCRLF +
              'Challenge Type: ' + ChallengeTypeLits[FSuppCertChallenge] + IcsCRLF +
              'Product: ' + FSuppCertProduct + IcsCRLF;
    if FIssueState < IssStateCollected then begin
       Result := Result + 'Order not yet completed, State: ' + IssueStateLits[FIssueState];
    end
    else begin
        Result := Result + 'SSl certificate order completed OK' + IcsCRLF +
            'Supplier Order Id: ' + FSuppOrderId + IcsCRLF +
            'Our Order Number: ' + IntToStr(FNewOrderNum) + IcsCRLF +
            'Certificate Saved at: ' + FFileFinalBundle + IcsCRLF +
            FNewSslCert.CertInfo(False);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// load own Certificate Authority cert and private key ready to sign requests
function TSslX509Certs.LoadOwnCA: Boolean;
begin
    Result := False;
    LogTimeStamp;
    FNewSslCert.DoClearCA;
    FNewInterLines := '';
    if (FCACertFile = '') or (NOT FileExists(FCACertFile)) then begin
        LogEvent ('Can not find Own Certificate Authority cert file: ' + FCACertFile);
    end;
    try
        FNewSslCert.LoadFromFile(FCACertFile, croTry, croNo, FCAPkeyPw);
        if NOT FNewSslCert.IsPKeyLoaded then begin
            if (FCAPkeyFile = '') or (NOT FileExists(FCAPkeyFile)) then begin
                LogEvent('Can not find Own Certificate Authority private key file: ' + FCAPkeyFile);
                Exit;
            end;
            FNewSslCert.PrivateKeyLoadFromPemFile(FCAPkeyFile, FCAPkeyPw);
        end;
        if NOT FNewSslCert.IsCertLoaded then begin
            LogEvent('No certificate loaded');
            exit;
        end;
        if Pos ('CA=TRUE', FNewSslCert.BasicConstraints) = 0 then begin
            LogEvent('Certificate is not a CA');
            exit;
        end;
        if NOT FNewSslCert.SelfSigned then  // need to add intermediate CA to bundle
            FNewInterLines := FNewSslCert.SaveCertToText(False);
        FNewSslCert.X509CA := FNewSslCert.X509;
        FNewSslCert.PrivKeyCA := FNewSslCert.PrivateKey;
        LogEvent('Loaded Own Certificate Authority OK' + IcsCRLF + FNewSslCert.CertInfo(False));
        Result := True;
    except
        on E:Exception do begin
            LogEvent('Failed to load Own CA: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// create new certificate signed by our own Certificate Authority
function TSslX509Certs.OwnCASign(IsCA: Boolean): Boolean;             { V9.1 allow to create intermediate }
var
    InterLines: String;
begin
    Result := False;
    LogTimeStamp;
    LogEvent ('Checking account directory: ' + FDirCertWork);
    if NOT ForceDirectories(FDirCertWork) then begin
        LogEvent ('Failed to create directory: ' + FDirCertWork);
        exit;
    end;
    if NOT DBOpenINI(FDirCertWork, True) then
        Exit;
    DBReadAccount(FDirCertWork);  // ignore errors
    if NOT FNewSslCert.IsCALoaded then begin
        LogEvent('Must load Own Certificate Authority first');
        exit;
    end;
    FSupplierProto := SuppProtoOwnCA;
    FSupplierTitle := 'Own CA';
    if NOT DBWriteAccount then
        Exit;

// see if using details from old CSR
    FNewSslCert.DoClearCerts;
    if FCertCsrOrigin = CsrOriginFile then begin
        if NOT CheckCSR(True) then
            Exit;  // sets CommonName and SANs
    end;

// initial set-up
    if (FCertCommonName = '') then begin
        LogEvent('Must specify domain Common Name for certificate');
        Exit;
    end;
    LogEvent('Creating certificate signed by Own Certificate Authority for' + FCertCommonName) ;

 // make sure common name is also in SANs, so we can ignore it subsequently
    FNewSslCert.AltDNSList.Clear;
    InterLines := '';
    if (NOT IsCA) then begin    { V9.1 not if creating intermediate certificate }
        BuildSANList;
        InterLines := FNewSslCert.CaCertLines + IcsCRLF;  { V9.1 get intermediate certificate for bundle }
    end;

// order info
    FNewOrderNum := DBNewOrderNum;
    FSuppOrderRef := 'ICS-' + IntToStr(FNewOrderNum);
    FSuppCertProduct := FSupplierTitle;
    FNewCertPrefix := 'OwnCA-' ;

 // create CSR and private key
// work file names, in account directory, with orderid (no work names)
// fails if can not create directories
    FNewSslCert.ClearAll;
    fSuppOrderId := '';
    if NOT SetPartFNames (False) then Exit ;
    SetFullFileNames (FPartFNameWork) ;

// create private key and certificate service request
    if NOT CreateKeyandReq(IsCA) then exit ;      { V9.1 }

// set extensions
    with fNewSslCert do begin
        BasicIsCA := IsCA;
        ExpireDays := FCertValidity;
        BasicPathLen := 0;
        KeyCertSign := True;
        KeyCRLSign := False;
        KeyDigiSign := True;
        KeyDataEnc := False;
        KeyKeyEnc  := True;
        KeyKeyAgree := False;
        KeyNonRepud  := False;
        KeyExtClient  := True;
        KeyExtServer  := True;
        KeyExtEmail  := False;
        KeyExtCode := False;
        if FCertSerNumType = SerNumRandom then
            SerialNum := 0   // random serial
        else
            SerialNum := FNewOrderNum;
    end;
    FSuppOrderId := IntToStr(FNewOrderNum);
    try
        FNewSslCert.DoSignCertReq(False);
        LogEvent('Created certificate signed by Own Certificate Authority OK');
    except
        on E:Exception do begin
            LogEvent('Failed to sign CSR with Own Certificate Authority: ' + E.Message);
            Exit;
        end;
    end;
    if NOT SetPartFNames (False) then Exit ;   // build file names with order id
    fNewCertLines := FNewSslCert.SaveCertToText(False) + IcsCRLF + IcsCRLF + InterLines;   { V9.1 get intermediate certificate for bundle }
    LogEvent ('Certificate(s):' + IcsCRLF + fNewCertLines);

// save lots of certificates in different formats and places
    Result := SaveCertificateFiles(fCertCommonName);

// add our new certificate to a database
    if Result then begin
        FNewSslCert.CADBFile := FDirCertWork + FileCADB;
        try
            FNewSslCert.SaveToCADatabase(FFileFinalCert);
            LogEvent('Updated Own Certificate Authority database: ' + FNewSslCert.CADBFile);
        except
            on E:Exception do begin
                LogEvent('Failed to write Own Certificate Authority database: ' + E.Message);
          end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// V8.64 create new self signed certificate
function TSslX509Certs.SelfSign(IsCA: Boolean): Boolean;
var
    I: Integer;
    CN: String;
begin
    Result := False;
    LogEvent ('Generating private and public key pair for self signed cert, please wait');
    if (fCertCommonName = '') then begin
        LogEvent ('No Common Name for certificate');
        Exit;
    end;

// order info
    FNewOrderNum := DBNewOrderNum;
    FSuppOrderRef := 'ICS-' + IntToStr(FNewOrderNum);
    FSuppCertProduct := FSupplierTitle;
    FNewCertPrefix := 'OwnCA-' ;

    try
        if FPrivKeyType < PrivKeyRsa2048 then FPrivKeyType := PrivKeyRsa2048; { V8.64 sanity check }
        FNewSslCert.PrivKeyType := FPrivKeyType;
        FNewSslCert.PrivateKey := Nil;
        FNewSslCert.DoKeyPair;
        if FNewSslCert.IsPKeyLoaded then  { V8.64 check actually created }
            LogEvent ('Generated private and public key pair OK:' + IcsCRLF + FNewSslCert.PrivateKeyInfo)
        else begin
            LogEvent ('Failed to generate private key - bad key parameters?');
            exit ;
        end;
    except
        on E:Exception do  begin
            LogEvent ('Failed to generate private key - ' + E.Message);
            exit ;
        end;
    end;

    LogEvent ('Generating self signed certificate');
    try
        with fNewSslCert do begin
            CommonName := fCertCommonName;
            AltDNSList.Clear;
            if (NOT IsCA) and (FCertSubAltNames.Count > 0) then begin
                for I := 0 to FCertSubAltNames.Count - 1 do begin
                    AltDNSList.Add(FCertSubAltNames[I].SADomain);
                end;
            end;
            CertDigest := fCertSignDigestType ;
            Country := FCertCountry;
            State := FCertState;
            Locality := FCertLocality;
            Organization := FCertOrganization;
            OrgUnit := FCertOrgUnit;
            Descr := FCertDescr;
            Email := FCertContactEmail;

            // set extensions
            BasicIsCA := IsCA;
            ExpireDays := FCertValidity;
            BasicPathLen := 0;
            KeyCertSign := True;
            KeyCRLSign := False;
            KeyDigiSign := True;
            KeyDataEnc := False;
            KeyKeyEnc  := True;
            KeyKeyAgree := False;
            KeyNonRepud  := False;
            KeyExtClient  := True;
            KeyExtServer  := True;
            KeyExtEmail  := False;
            KeyExtCode := False;
            if FCertSerNumType = SerNumRandom then
                SerialNum := 0   // random serial
            else
                SerialNum := FNewOrderNum;
            FSuppOrderId := IntToStr(FNewOrderNum);
        end;
        FNewSslCert.DoSelfSignCert(False);
        LogEvent('Created Self Signed Certificate OK');
    except
        on E:Exception do begin
            LogEvent ('Failed to create self signed certificate - ' + E.Message);
            exit ;
        end;
    end;
  // create certificate file name from domain common name, change . to _ and * to x
    CN := IcsBuildCertFName(FCertCommonName) ;
    FPartFNameOrder := IncludeTrailingPathDelimiter(FDirCertWork) + FNewCertPrefix + FSuppOrderId + '-' + CN ;
    FPartFNameWork := FPartFNameOrder;
    fPartFNameFinal := IncludeTrailingPathDelimiter(fDirCertWork) + CN ;
    fNewCertLines := FNewSslCert.SaveCertToText(False);  // need lines to save files
    LogEvent ('Certificate:' + IcsCRLF + fNewCertLines);

// save lots of certificates in different formats and places
    Result := SaveCertificateFiles(fCertCommonName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$ENDIF USE_SSL}

end.
