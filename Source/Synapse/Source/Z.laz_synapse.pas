{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Z.laz_synapse; 

interface

uses
    Z.asn1util, Z.blcksock, Z.clamsend, Z.dnssend, Z.ftpsend, Z.ftptsend, Z.httpsend, 
  Z.imapsend, Z.ldapsend, Z.mimeinln, Z.mimemess, Z.mimepart, Z.nntpsend, Z.pingsend, 
  Z.pop3send, Z.slogsend, Z.smtpsend, Z.snmpsend, Z.sntpsend, Z.synachar, Z.synacode, 
  Z.synacrypt, Z.synadbg, Z.synafpc, Z.synaicnv, Z.synaip, Z.synamisc, Z.synaser, Z.synautil, 
  Z.synsock, Z.tlntsend, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('laz_synapse', @Register); 
end.
